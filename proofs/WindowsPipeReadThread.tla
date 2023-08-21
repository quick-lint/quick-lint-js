---- MODULE WindowsPipeReadThread ----
(*
NOTE[Event_Loop_Windows-Registered_Pipe_Read-proof]:
This is a TLA+/PlusCal specification for how Event_Loop_Windows manages the
Event_Loop_Windows::Registered_Pipe_Read thread.

The specification has three threads (PlusCal processes):
* The main thread. In C++ it executes Event_Loop_Windows::run.
* The reader thread. In C++ it executes
  Event_Loop_Windows::Registered_Pipe_Read::thread_routine.
* An arbitrary other thread, which in C++ could be the main thread or the reader
  thread or some other thread. It requests a that the reader thread stop via a
  call to Event_Loop_Base::un_keep_alive.

NOTE[Event_Loop_Windows-Registered_Pipe_Read-terminate]: The specification
proves that calling the Win32 TerminateThread API can only interrupt a ReadFile
call, not a running user callback. See the
ReaderThreadCannotBeTerminatedDuringCallback constraint.

The specification also proves that the reader thread and the main thread
eventually terminate if a stop is requested (and if the OS scheduler is fair, of
course). See the ReaderThreadFinishesIfStopped and MainThreadFinishesIfStopped
liveness properties.

The specification also proves that neither the reader thread nor the main thread
deadlock.

NOTE[Event_Loop_Windows-Registered_Pipe_Read-data-loss]: The specification does
*not* prove no data loss. In fact, it proves data loss! A Win32 ReadFile call
might finish successfully but the reader thread might still be terminated via
Win32 TerminateThread before the ReadFile's data is given to a user callback.
See the ReadDataIsNotDropped liveness property.
*)

EXTENDS Naturals, Sequences

MainThreadID == 0
ReaderThreadID == 1
OtherThreadID == 2

InvalidReadTicket == 0
LegalReadTickets == {1, 2}

(*
--fair algorithm WindowsPipeReadThread {
  variables
    (* Whether Win32 TerminateThread is allowed to be called.
       std::atomic<bool>
       Writable by the reader thread and readable by the main thread. *)
    allowTerminate = FALSE,
    (* Whether the event loop should stop.
       std::atomic<bool> (actually std::atomic<int> in C++)
       Writable by the other thread and readable by the main thread and the reader thread. *)
    stopRequested = FALSE,

    (* Implementation of Win32 TerminateThread.
       Writable by any thread (M_TerminateThread) and readable by the reader thread (R_CheckTerminateThread). *)
    terminateRequested = FALSE,
    (* Implementation of Win32 CancelIoEx.
       Writable by any thread (M_CancelIoEx) and readable by the reader thread (R_CheckTerminateThread). *)
    cancelIORequested = FALSE,
    (* Implementation of Win32 _beginthreadex.
       Writable by the main thread and readable by the reader thread. *)
    readerThreadSpawned = FALSE,
    (* Counter representing the result of a Win32 ReadFile call.
       This variable is used by ReadDataIsNotDropped to correlate calls to
       ReadFile with calls to the user callback.

       Possible values:
       * InvalidReadTicket (ReadFile never returned yet)
       * 1 (every odd ReadFile call)
       * 2 (every even ReadFile call)
       Sequence over time: <<InvalidReadTicket, 1, 2, 1, 2, 1, 2, ...>> *)
    readTicket = InvalidReadTicket;
  
  (* Win32 _beginthreadex *)
  macro M_BeginThread() {
    readerThreadSpawned := TRUE;
  }
  
  (* Win32 CancelIoEx *)
  (* NOTE(strager): CancelIoEx isn't called in this model. It's here anyway in
     case you want to play around with an algorithm that uses it. *)
  macro M_CancelIoEx() {
    (* NOTE(strager): Win32 CancelIoEx only has an effect if the thread is
       blocked in ReadFile. *)
    if (pc[ReaderThreadID] = "R_InBlockingRead") {
      cancelIORequested := TRUE;
    }
  }
  
  (* Win32 TerminateThread *)
  macro M_TerminateThread() {
    terminateRequested := TRUE;
  }
  
  (* Win32 WaitForSingleObject *)
  macro M_JoinReaderThread() {
    await pc[ReaderThreadID] = "Done";
  }
  
  (* Win32 ReadFile *)
  macro R_ReadFile() {
    readTicket := CASE readTicket = InvalidReadTicket -> 1
                    [] readTicket = 1 -> 2
                    [] readTicket = 2 -> 1;
  }

  process (MainThread = MainThreadID) {
    M_Idle:
    if (~stopRequested) {
      M_BeginThread();
      M_StartedReader:
      await stopRequested;
      M_MaybeTerminate:
      if (allowTerminate) {
        M_TerminateThread();
      };
      M_StartedHalt:
      M_JoinReaderThread();
    };
  }
  
  (* Win32 TerminateThread could be called at any time. We model this by calling
     R_CheckTerminateThread between every label. 'CT' stands for 'Check for
     Termination'. *)
  process (ReaderThread = ReaderThreadID) {
    R_NotStarted:
    either {
      await readerThreadSpawned;
    } or {
      await pc[MainThreadID] = "Done";
      goto Done;
    };
    R_LoopCT: if (terminateRequested) { goto Done; };
    R_Loop:
    while (~stopRequested) {
      R_AllowTerminateCT: if (terminateRequested) { goto Done; };
      R_AllowTerminate:
      allowTerminate := TRUE;
      R_CheckStopRequestedBeforeReadCT: if (terminateRequested) { goto Done; };
      R_CheckStopRequestedBeforeRead:
      if (stopRequested) {
        goto Done;
      };
      R_InReadCT: if (terminateRequested) { goto Done; };
      R_InRead:
      either {
        R_InFinishingRead:
        R_ReadFile();
        R_FinishedRead: skip;
      } or {
        (* ReadFile is blocked because the write end of the pipe is inactive. *)
        (* NOTE(strager): We need an extra label here. Otherwise, we would not
           catch the deadlock which would occur if Win32 CancelIoEx was not
           called. *)
        R_InBlockingRead:
        await terminateRequested \/ cancelIORequested;
        goto Done;
      };
      R_DisallowTerminateCT: if (terminateRequested) { goto Done; };
      R_DisallowTerminate:
      allowTerminate := FALSE;
      R_BeforeCallbackCT: if (terminateRequested) { goto Done; };
      R_BeforeCallback:
      (* This check is unnecessary but is an optimization. *)
      if (stopRequested) {
        goto Done;
      };
      R_InCallbackCT: if (terminateRequested) { goto Done; };
      R_InCallback:
      (* readTicket can be used by the user callback here. *)
      skip;
      R_CheckReadEOFCT: if (terminateRequested) { goto Done; };
      R_CheckReadEOF:
      either {
        skip;
      } or {
        (* ReadFile returned EOF. *)
        goto Done;
      };
    };
  }
  
  process (OtherThread = OtherThreadID) {
    O_RequestingStop:
    stopRequested := TRUE;
  }
}
*)

\* BEGIN TRANSLATION (chksum(pcal) = "33218624" /\ chksum(tla) = "7893d0fd")
VARIABLES allowTerminate, stopRequested, terminateRequested, 
          cancelIORequested, readerThreadSpawned, readTicket, pc

vars == << allowTerminate, stopRequested, terminateRequested, 
           cancelIORequested, readerThreadSpawned, readTicket, pc >>

ProcSet == {MainThreadID} \cup {ReaderThreadID} \cup {OtherThreadID}

Init == (* Global variables *)
        /\ allowTerminate = FALSE
        /\ stopRequested = FALSE
        /\ terminateRequested = FALSE
        /\ cancelIORequested = FALSE
        /\ readerThreadSpawned = FALSE
        /\ readTicket = InvalidReadTicket
        /\ pc = [self \in ProcSet |-> CASE self = MainThreadID -> "M_Idle"
                                        [] self = ReaderThreadID -> "R_NotStarted"
                                        [] self = OtherThreadID -> "O_RequestingStop"]

M_Idle == /\ pc[MainThreadID] = "M_Idle"
          /\ IF ~stopRequested
                THEN /\ readerThreadSpawned' = TRUE
                     /\ pc' = [pc EXCEPT ![MainThreadID] = "M_StartedReader"]
                ELSE /\ pc' = [pc EXCEPT ![MainThreadID] = "Done"]
                     /\ UNCHANGED readerThreadSpawned
          /\ UNCHANGED << allowTerminate, stopRequested, terminateRequested, 
                          cancelIORequested, readTicket >>

M_StartedReader == /\ pc[MainThreadID] = "M_StartedReader"
                   /\ stopRequested
                   /\ pc' = [pc EXCEPT ![MainThreadID] = "M_MaybeTerminate"]
                   /\ UNCHANGED << allowTerminate, stopRequested, 
                                   terminateRequested, cancelIORequested, 
                                   readerThreadSpawned, readTicket >>

M_MaybeTerminate == /\ pc[MainThreadID] = "M_MaybeTerminate"
                    /\ IF allowTerminate
                          THEN /\ terminateRequested' = TRUE
                          ELSE /\ TRUE
                               /\ UNCHANGED terminateRequested
                    /\ pc' = [pc EXCEPT ![MainThreadID] = "M_StartedHalt"]
                    /\ UNCHANGED << allowTerminate, stopRequested, 
                                    cancelIORequested, readerThreadSpawned, 
                                    readTicket >>

M_StartedHalt == /\ pc[MainThreadID] = "M_StartedHalt"
                 /\ pc[ReaderThreadID] = "Done"
                 /\ pc' = [pc EXCEPT ![MainThreadID] = "Done"]
                 /\ UNCHANGED << allowTerminate, stopRequested, 
                                 terminateRequested, cancelIORequested, 
                                 readerThreadSpawned, readTicket >>

MainThread == M_Idle \/ M_StartedReader \/ M_MaybeTerminate
                 \/ M_StartedHalt

R_NotStarted == /\ pc[ReaderThreadID] = "R_NotStarted"
                /\ \/ /\ readerThreadSpawned
                      /\ pc' = [pc EXCEPT ![ReaderThreadID] = "R_LoopCT"]
                   \/ /\ pc[MainThreadID] = "Done"
                      /\ pc' = [pc EXCEPT ![ReaderThreadID] = "Done"]
                /\ UNCHANGED << allowTerminate, stopRequested, 
                                terminateRequested, cancelIORequested, 
                                readerThreadSpawned, readTicket >>

R_LoopCT == /\ pc[ReaderThreadID] = "R_LoopCT"
            /\ IF terminateRequested
                  THEN /\ pc' = [pc EXCEPT ![ReaderThreadID] = "Done"]
                  ELSE /\ pc' = [pc EXCEPT ![ReaderThreadID] = "R_Loop"]
            /\ UNCHANGED << allowTerminate, stopRequested, terminateRequested, 
                            cancelIORequested, readerThreadSpawned, readTicket >>

R_Loop == /\ pc[ReaderThreadID] = "R_Loop"
          /\ IF ~stopRequested
                THEN /\ pc' = [pc EXCEPT ![ReaderThreadID] = "R_AllowTerminateCT"]
                ELSE /\ pc' = [pc EXCEPT ![ReaderThreadID] = "Done"]
          /\ UNCHANGED << allowTerminate, stopRequested, terminateRequested, 
                          cancelIORequested, readerThreadSpawned, readTicket >>

R_AllowTerminateCT == /\ pc[ReaderThreadID] = "R_AllowTerminateCT"
                      /\ IF terminateRequested
                            THEN /\ pc' = [pc EXCEPT ![ReaderThreadID] = "Done"]
                            ELSE /\ pc' = [pc EXCEPT ![ReaderThreadID] = "R_AllowTerminate"]
                      /\ UNCHANGED << allowTerminate, stopRequested, 
                                      terminateRequested, cancelIORequested, 
                                      readerThreadSpawned, readTicket >>

R_AllowTerminate == /\ pc[ReaderThreadID] = "R_AllowTerminate"
                    /\ allowTerminate' = TRUE
                    /\ pc' = [pc EXCEPT ![ReaderThreadID] = "R_CheckStopRequestedBeforeReadCT"]
                    /\ UNCHANGED << stopRequested, terminateRequested, 
                                    cancelIORequested, readerThreadSpawned, 
                                    readTicket >>

R_CheckStopRequestedBeforeReadCT == /\ pc[ReaderThreadID] = "R_CheckStopRequestedBeforeReadCT"
                                    /\ IF terminateRequested
                                          THEN /\ pc' = [pc EXCEPT ![ReaderThreadID] = "Done"]
                                          ELSE /\ pc' = [pc EXCEPT ![ReaderThreadID] = "R_CheckStopRequestedBeforeRead"]
                                    /\ UNCHANGED << allowTerminate, 
                                                    stopRequested, 
                                                    terminateRequested, 
                                                    cancelIORequested, 
                                                    readerThreadSpawned, 
                                                    readTicket >>

R_CheckStopRequestedBeforeRead == /\ pc[ReaderThreadID] = "R_CheckStopRequestedBeforeRead"
                                  /\ IF stopRequested
                                        THEN /\ pc' = [pc EXCEPT ![ReaderThreadID] = "Done"]
                                        ELSE /\ pc' = [pc EXCEPT ![ReaderThreadID] = "R_InReadCT"]
                                  /\ UNCHANGED << allowTerminate, 
                                                  stopRequested, 
                                                  terminateRequested, 
                                                  cancelIORequested, 
                                                  readerThreadSpawned, 
                                                  readTicket >>

R_InReadCT == /\ pc[ReaderThreadID] = "R_InReadCT"
              /\ IF terminateRequested
                    THEN /\ pc' = [pc EXCEPT ![ReaderThreadID] = "Done"]
                    ELSE /\ pc' = [pc EXCEPT ![ReaderThreadID] = "R_InRead"]
              /\ UNCHANGED << allowTerminate, stopRequested, 
                              terminateRequested, cancelIORequested, 
                              readerThreadSpawned, readTicket >>

R_InRead == /\ pc[ReaderThreadID] = "R_InRead"
            /\ \/ /\ pc' = [pc EXCEPT ![ReaderThreadID] = "R_InFinishingRead"]
               \/ /\ pc' = [pc EXCEPT ![ReaderThreadID] = "R_InBlockingRead"]
            /\ UNCHANGED << allowTerminate, stopRequested, terminateRequested, 
                            cancelIORequested, readerThreadSpawned, readTicket >>

R_InFinishingRead == /\ pc[ReaderThreadID] = "R_InFinishingRead"
                     /\ readTicket' = (CASE readTicket = InvalidReadTicket -> 1
                                         [] readTicket = 1 -> 2
                                         [] readTicket = 2 -> 1)
                     /\ pc' = [pc EXCEPT ![ReaderThreadID] = "R_FinishedRead"]
                     /\ UNCHANGED << allowTerminate, stopRequested, 
                                     terminateRequested, cancelIORequested, 
                                     readerThreadSpawned >>

R_FinishedRead == /\ pc[ReaderThreadID] = "R_FinishedRead"
                  /\ TRUE
                  /\ pc' = [pc EXCEPT ![ReaderThreadID] = "R_DisallowTerminateCT"]
                  /\ UNCHANGED << allowTerminate, stopRequested, 
                                  terminateRequested, cancelIORequested, 
                                  readerThreadSpawned, readTicket >>

R_InBlockingRead == /\ pc[ReaderThreadID] = "R_InBlockingRead"
                    /\ terminateRequested \/ cancelIORequested
                    /\ pc' = [pc EXCEPT ![ReaderThreadID] = "Done"]
                    /\ UNCHANGED << allowTerminate, stopRequested, 
                                    terminateRequested, cancelIORequested, 
                                    readerThreadSpawned, readTicket >>

R_DisallowTerminateCT == /\ pc[ReaderThreadID] = "R_DisallowTerminateCT"
                         /\ IF terminateRequested
                               THEN /\ pc' = [pc EXCEPT ![ReaderThreadID] = "Done"]
                               ELSE /\ pc' = [pc EXCEPT ![ReaderThreadID] = "R_DisallowTerminate"]
                         /\ UNCHANGED << allowTerminate, stopRequested, 
                                         terminateRequested, cancelIORequested, 
                                         readerThreadSpawned, readTicket >>

R_DisallowTerminate == /\ pc[ReaderThreadID] = "R_DisallowTerminate"
                       /\ allowTerminate' = FALSE
                       /\ pc' = [pc EXCEPT ![ReaderThreadID] = "R_BeforeCallbackCT"]
                       /\ UNCHANGED << stopRequested, terminateRequested, 
                                       cancelIORequested, readerThreadSpawned, 
                                       readTicket >>

R_BeforeCallbackCT == /\ pc[ReaderThreadID] = "R_BeforeCallbackCT"
                      /\ IF terminateRequested
                            THEN /\ pc' = [pc EXCEPT ![ReaderThreadID] = "Done"]
                            ELSE /\ pc' = [pc EXCEPT ![ReaderThreadID] = "R_BeforeCallback"]
                      /\ UNCHANGED << allowTerminate, stopRequested, 
                                      terminateRequested, cancelIORequested, 
                                      readerThreadSpawned, readTicket >>

R_BeforeCallback == /\ pc[ReaderThreadID] = "R_BeforeCallback"
                    /\ IF stopRequested
                          THEN /\ pc' = [pc EXCEPT ![ReaderThreadID] = "Done"]
                          ELSE /\ pc' = [pc EXCEPT ![ReaderThreadID] = "R_InCallbackCT"]
                    /\ UNCHANGED << allowTerminate, stopRequested, 
                                    terminateRequested, cancelIORequested, 
                                    readerThreadSpawned, readTicket >>

R_InCallbackCT == /\ pc[ReaderThreadID] = "R_InCallbackCT"
                  /\ IF terminateRequested
                        THEN /\ pc' = [pc EXCEPT ![ReaderThreadID] = "Done"]
                        ELSE /\ pc' = [pc EXCEPT ![ReaderThreadID] = "R_InCallback"]
                  /\ UNCHANGED << allowTerminate, stopRequested, 
                                  terminateRequested, cancelIORequested, 
                                  readerThreadSpawned, readTicket >>

R_InCallback == /\ pc[ReaderThreadID] = "R_InCallback"
                /\ TRUE
                /\ pc' = [pc EXCEPT ![ReaderThreadID] = "R_CheckReadEOFCT"]
                /\ UNCHANGED << allowTerminate, stopRequested, 
                                terminateRequested, cancelIORequested, 
                                readerThreadSpawned, readTicket >>

R_CheckReadEOFCT == /\ pc[ReaderThreadID] = "R_CheckReadEOFCT"
                    /\ IF terminateRequested
                          THEN /\ pc' = [pc EXCEPT ![ReaderThreadID] = "Done"]
                          ELSE /\ pc' = [pc EXCEPT ![ReaderThreadID] = "R_CheckReadEOF"]
                    /\ UNCHANGED << allowTerminate, stopRequested, 
                                    terminateRequested, cancelIORequested, 
                                    readerThreadSpawned, readTicket >>

R_CheckReadEOF == /\ pc[ReaderThreadID] = "R_CheckReadEOF"
                  /\ \/ /\ TRUE
                        /\ pc' = [pc EXCEPT ![ReaderThreadID] = "R_Loop"]
                     \/ /\ pc' = [pc EXCEPT ![ReaderThreadID] = "Done"]
                  /\ UNCHANGED << allowTerminate, stopRequested, 
                                  terminateRequested, cancelIORequested, 
                                  readerThreadSpawned, readTicket >>

ReaderThread == R_NotStarted \/ R_LoopCT \/ R_Loop \/ R_AllowTerminateCT
                   \/ R_AllowTerminate \/ R_CheckStopRequestedBeforeReadCT
                   \/ R_CheckStopRequestedBeforeRead \/ R_InReadCT
                   \/ R_InRead \/ R_InFinishingRead \/ R_FinishedRead
                   \/ R_InBlockingRead \/ R_DisallowTerminateCT
                   \/ R_DisallowTerminate \/ R_BeforeCallbackCT
                   \/ R_BeforeCallback \/ R_InCallbackCT \/ R_InCallback
                   \/ R_CheckReadEOFCT \/ R_CheckReadEOF

O_RequestingStop == /\ pc[OtherThreadID] = "O_RequestingStop"
                    /\ stopRequested' = TRUE
                    /\ pc' = [pc EXCEPT ![OtherThreadID] = "Done"]
                    /\ UNCHANGED << allowTerminate, terminateRequested, 
                                    cancelIORequested, readerThreadSpawned, 
                                    readTicket >>

OtherThread == O_RequestingStop

(* Allow infinite stuttering to prevent deadlock on termination. *)
Terminating == /\ \A self \in ProcSet: pc[self] = "Done"
               /\ UNCHANGED vars

Next == MainThread \/ ReaderThread \/ OtherThread
           \/ Terminating

Spec == /\ Init /\ [][Next]_vars
        /\ WF_vars(Next)

Termination == <>(\A self \in ProcSet: pc[self] = "Done")

\* END TRANSLATION 

(* Type check for all variables. *)
TypeOK ==
  /\ allowTerminate \in {TRUE, FALSE}
  /\ cancelIORequested \in {TRUE, FALSE}
  /\ terminateRequested \in {TRUE, FALSE}
  /\ readerThreadSpawned \in {TRUE, FALSE}
  /\ stopRequested \in {TRUE, FALSE}
  /\ readTicket \in (LegalReadTickets \union {InvalidReadTicket})

ReaderThreadIsDoneIfMainThreadStopped ==
  pc[MainThreadID] = "Done" => pc[ReaderThreadID] \in {"R_NotStarted", "Done"}

ReaderThreadCanOnlyRunIfMainThreadRequests ==
  pc[MainThreadID] = "M_Idle" => pc[ReaderThreadID] = "R_NotStarted"

(* See NOTE[Event_Loop_Windows-Registered_Pipe_Read-terminate]. *)
ReaderThreadCannotBeTerminatedDuringCallback ==
  terminateRequested => pc[ReaderThreadID] /= "R_InCallback"
  
ReaderThreadCannotBeTerminatedBeforeStarting ==
  terminateRequested => pc[ReaderThreadID] /= "R_NotStarted"

(* Invariant check. *)
Consistent ==
  /\ ReaderThreadIsDoneIfMainThreadStopped
  /\ ReaderThreadCanOnlyRunIfMainThreadRequests
  /\ ReaderThreadCannotBeTerminatedDuringCallback
  /\ ReaderThreadCannotBeTerminatedBeforeStarting
----
(* Liveness property. *)
ReaderThreadFinishesIfStopped ==
  stopRequested
    ~> (pc[ReaderThreadID] \in {"R_NotStarted", "Done"})

(* Liveness property. *)
MainThreadFinishesIfStopped ==
  (pc[MainThreadID] = "M_MaybeTerminate")
    ~> (pc[MainThreadID] = "Done")

(* Liveness property. *)
(* FIXME(strager): I think this property is incorrectly expressed. It does not
   guarantee that some sequence of steps leads to the callback being called. It
   may as well say that stopRequested will eventually be true, but that's not
   useful to say. *)
CallbackIsCalledForReadIfNotStopped ==
  \A ticket \in LegalReadTickets
    : (pc[ReaderThreadID] = "R_FinishedRead" /\ readTicket = ticket)
      ~> \/ pc[ReaderThreadID] = "R_InCallback" /\ readTicket = ticket
         \/ stopRequested

(* Liveness property. *)
(* FIXME(strager): I think this property holds, but TLC disagrees. TLC says that
   the property does not hold, but it shows me a state where
   (pc[MainThreadID] = "M_StartedReader") and a later state where
   (pc[ReaderThreadID] = "R_Loop"). I think that perhaps TLC has a bug. *)
ReaderThreadRunsIfMainThreadSpawnsReaderThread ==
  (pc[MainThreadID] = "M_StartedReader")
    ~> (pc[ReaderThreadID] = "R_Loop")

(* Liveness property. This property does *not* hold. See
   NOTE[Event_Loop_Windows-Registered_Pipe_Read-data-loss]. *)
ReadDataIsNotDropped ==
  \A ticket \in LegalReadTickets
    : (pc[ReaderThreadID] = "R_FinishedRead" /\ readTicket = ticket)
      ~> (pc[ReaderThreadID] = "R_InCallback" /\ readTicket = ticket)
=============================================================================
