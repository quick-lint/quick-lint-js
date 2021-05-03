/*
 * C Reusables
 * <http://github.com/mity/c-reusables>
 *
 * Copyright (c) 2017 Martin Mitas
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
 * OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
 * IN THE SOFTWARE.
 */

#ifndef CRE_CMDLINE_H
#define CRE_CMDLINE_H

#ifdef __cplusplus
extern "C" {
#endif


/* The option may have an argument. (Affects only long option.) */
#define CMDLINE_OPTFLAG_OPTIONALARG     0x0001

/* The option must have an argument.
 * Such short option cannot be grouped within single '-abc'. */
#define CMDLINE_OPTFLAG_REQUIREDARG     0x0002

/* Enable special compiler-like mode for the long option.
 *
 * Note ::shortname is not supported with this flag. CMDLINE_OPTION::shortname
 * is silently ignored if the flag is used.
 *
 * With this flag, CMDLINE_OPTION::longname is treated differently as follows:
 *
 * 1. The option matches if the CMDLINE_OPTION::longname is the exact prefix
 *   of the argv[i] from commandline.
 *
 * 2. Double dash ("--") is not automatically prepended to
 *    CMDLINE_OPTION::longname. (If you desire any leading dash, include it
 *    explicitly in CMDLINE_OPTION initialization.)
 *
 * 3. An argument (optionally after a whitespace) is required (the flag
 *    CMDLINE_OPTFLAG_COMPILERLIKE implicitly implies also the flag
 *    CMDLINE_OPTFLAG_REQUIREDARG).
 *
 *    But there is no delimiter expected (no "=" between the option and its
 *    argument). Whitespace is optional between the option and its argument.
 *
 *    Intended use is for options similar to what many compilers accept.
 *    For example:
 *      -DDEBUG=0               (-D is the option, DEBUG=0 is the argument).
 *      -Isrc/include           (-I is the option, src/include is the argument).
 *      -isystem /usr/include   (-isystem is the option, /usr/include is the argument).
 *      -lmath                  (-l is the option, math is the argument).
 */
#define CMDLINE_OPTFLAG_COMPILERLIKE    0x0004


/* Special (reserved) option IDs. Do not use these for any CMDLINE_OPTION::id.
 * See documentation of cmdline_read() to get info about their meaning.
 */
#define CMDLINE_OPTID_NONE              0
#define CMDLINE_OPTID_UNKNOWN           (-0x7fffffff + 0)
#define CMDLINE_OPTID_MISSINGARG        (-0x7fffffff + 1)
#define CMDLINE_OPTID_BOGUSARG          (-0x7fffffff + 2)


typedef struct CMDLINE_OPTION {
    char shortname;         /* Short (single char) option or 0. */
    const char* longname;   /* Long name (after "--") or NULL. */
    int id;                 /* Non-zero ID to identify the option in the callback; or zero to denote end of options list. */
    unsigned flags;         /* Bitmask of CMDLINE_OPTFLAG_xxxx flags. */
} CMDLINE_OPTION;


/* Parses all options and their arguments as specified by argc, argv accordingly
 * with the given options (except argv[0] which is ignored).
 *
 * The caller must specify the list of supported options in the 1st parameter
 * of the function. The array must end with a record whose CMDLINE_OPTION::id
 * is zero to zero.
 *
 * The provided callback function is called for each option on the command
 * line so that:
 *
 *   -- the "id" refers to the id of the option as specified  in options[].
 *
 *   -- the "arg" specifies an argument of the option or NULL if none is
 *      provided.
 *
 *   -- the "userdata" just allows to pass in some caller's context into
 *      the callback.
 *
 * Special cases (recognized via special "id" value) are reported to the
 * callback as follows:
 *
 *   -- If id is CMDLINE_OPTID_NONE, the callback informs about a non-option
 *      also known as a positional argument.
 *
 *      All argv[] tokens which are not interpreted as an options or an argument
 *      of any option fall into this category.
 *
 *      Usually, programs interpret these as paths to file to process.
 *
 *   -- If id is CMDLINE_OPTID_UNKNOWN, the corresponding argv[] looks like an
 *      option but it is not found in the options[] passed to cmdline_read().
 *
 *      The callback's parameter arg specifies the guilty command line token.
 *      Usually, program writes down an error message and exits.
 *
 *   -- If id is CMDLINE_OPTID_MISSINGARG, the given option is valid but its
 *      flag in options[] requires an argument; yet there is none on the
 *      command line.
 *
 *      The callback's parameter arg specifies the guilty option name.
 *      Usually, program writes down an error message and exits.
 *
 *   -- If id is CMDLINE_OPTID_BOGUSARG, the given option is valid but its
 *      flag in options[] does not expect an argument; yet the command line
 *      does provide one.
 *
 *      The callback's parameter arg specifies the guilty option name.
 *      Usually, program writes down an error message and exits.
 *
 * On success, zero is returned.
 *
 * If the callback returns a non-zero, cmdline_read() aborts immediately and
 * cmdline_read() propagates the same return value to the caller.
 */

int cmdline_read(const CMDLINE_OPTION* options, int argc, char** argv,
        int (*callback)(int /*id*/, const char* /*arg*/, void* /*userdata*/),
        void* userdata);


#ifdef __cplusplus
}  /* extern "C" { */
#endif

#endif  /* CRE_CMDLINE_H */
