/*
 * C Reusables
 * <http://github.com/mity/c-reusables>
 *
 * Copyright (c) 2017-2020 Martin Mitas
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

#include "cmdline.h"

#include <stdio.h>
#include <string.h>


#ifdef _WIN32
    #define snprintf    _snprintf
#endif


#define CMDLINE_AUXBUF_SIZE     32



static int
cmdline_handle_short_opt_group(const CMDLINE_OPTION* options, const char* arggroup,
        int (*callback)(int /*optval*/, const char* /*arg*/, void* /*userdata*/),
        void* userdata)
{
    const CMDLINE_OPTION* opt;
    int i;
    int ret = 0;

    for(i = 0; arggroup[i] != '\0'; i++) {
        for(opt = options; opt->id != 0; opt++) {
            if(arggroup[i] == opt->shortname)
                break;
        }

        if(opt->id != 0  &&  !(opt->flags & CMDLINE_OPTFLAG_REQUIREDARG)) {
            ret = callback(opt->id, NULL, userdata);
        } else {
            /* Unknown option. */
            char badoptname[3];
            badoptname[0] = '-';
            badoptname[1] = arggroup[i];
            badoptname[2] = '\0';
            ret = callback((opt->id != 0 ? CMDLINE_OPTID_MISSINGARG : CMDLINE_OPTID_UNKNOWN),
                            badoptname, userdata);
        }

        if(ret != 0)
            break;
    }

    return ret;
}

int
cmdline_read(const CMDLINE_OPTION* options, int argc, char** argv,
        int (*callback)(int /*optval*/, const char* /*arg*/, void* /*userdata*/),
        void* userdata)
{
    const CMDLINE_OPTION* opt;
    char auxbuf[CMDLINE_AUXBUF_SIZE+1];
    int fast_optarg_decision = 1;
    int after_doubledash = 0;
    int i = 1;
    int ret = 0;

    auxbuf[CMDLINE_AUXBUF_SIZE] = '\0';

    /* Check whether there is any CMDLINE_OPTFLAG_COMPILERLIKE option with
     * a name not starting with '-'. That would imply we can to check for
     * non-option arguments only after refusing all such options. */
    for(opt = options; opt->id != 0; opt++) {
        if((opt->flags & CMDLINE_OPTFLAG_COMPILERLIKE)  &&  opt->longname[0] != '-')
            fast_optarg_decision = 0;
    }

    while(i < argc) {
        if(after_doubledash  ||  strcmp(argv[i], "-") == 0) {
            /* Non-option argument.
             * Standalone "-" usually means "read from stdin" or "write to
             * stdout" so treat it always as a non-option. */
            ret = callback(CMDLINE_OPTID_NONE, argv[i], userdata);
        } else if(strcmp(argv[i], "--") == 0) {
            /* End of options. All the remaining tokens are non-options
             * even if they start with a dash. */
            after_doubledash = 1;
        } else if(fast_optarg_decision  &&  argv[i][0] != '-') {
            /* Non-option argument. */
            ret = callback(CMDLINE_OPTID_NONE, argv[i], userdata);
        } else {
            for(opt = options; opt->id != 0; opt++) {
                if(opt->flags & CMDLINE_OPTFLAG_COMPILERLIKE) {
                    size_t len = strlen(opt->longname);
                    if(strncmp(argv[i], opt->longname, len) == 0) {
                        /* Compiler-like option. */
                        if(argv[i][len] != '\0')
                            ret = callback(opt->id, argv[i] + len, userdata);
                        else if(i+1 < argc)
                            ret = callback(opt->id, argv[++i], userdata);
                        else
                            ret = callback(CMDLINE_OPTID_MISSINGARG, opt->longname, userdata);
                        break;
                    }
                } else if(opt->longname != NULL  &&  strncmp(argv[i], "--", 2) == 0) {
                    size_t len = strlen(opt->longname);
                    if(strncmp(argv[i]+2, opt->longname, len) == 0) {
                        /* Regular long option. */
                        if(argv[i][2+len] == '\0') {
                            /* with no argument provided. */
                            if(!(opt->flags & CMDLINE_OPTFLAG_REQUIREDARG))
                                ret = callback(opt->id, NULL, userdata);
                            else
                                ret = callback(CMDLINE_OPTID_MISSINGARG, argv[i], userdata);
                            break;
                        } else if(argv[i][2+len] == '=') {
                            /* with an argument provided. */
                            if(opt->flags & (CMDLINE_OPTFLAG_OPTIONALARG | CMDLINE_OPTFLAG_REQUIREDARG)) {
                                ret = callback(opt->id, argv[i]+2+len+1, userdata);
                            } else {
                                snprintf(auxbuf, CMDLINE_AUXBUF_SIZE, "--%s", opt->longname);
                                ret = callback(CMDLINE_OPTID_BOGUSARG, auxbuf, userdata);
                            }
                            break;
                        } else {
                            continue;
                        }
                    }
                } else if(opt->shortname != '\0'  &&  argv[i][0] == '-') {
                    if(argv[i][1] == opt->shortname) {
                        /* Regular short option. */
                        if(opt->flags & CMDLINE_OPTFLAG_REQUIREDARG) {
                            if(argv[i][2] != '\0')
                                ret = callback(opt->id, argv[i]+2, userdata);
                            else if(i+1 < argc)
                                ret = callback(opt->id, argv[++i], userdata);
                            else
                                ret = callback(CMDLINE_OPTID_MISSINGARG, argv[i], userdata);
                            break;
                        } else {
                            ret = callback(opt->id, NULL, userdata);

                            /* There might be more (argument-less) short options
                             * grouped together. */
                            if(ret == 0  &&  argv[i][2] != '\0')
                                ret = cmdline_handle_short_opt_group(options, argv[i]+2, callback, userdata);
                            break;
                        }
                    }
                }
            }

            if(opt->id == 0) {  /* still not handled? */
                if(argv[i][0] != '-') {
                    /* Non-option argument. */
                    ret = callback(CMDLINE_OPTID_NONE, argv[i], userdata);
                } else {
                    /* Unknown option. */
                    char* badoptname = argv[i];

                    if(strncmp(badoptname, "--", 2) == 0) {
                        /* Strip any argument from the long option. */
                        char* assignment = strchr(badoptname, '=');
                        if(assignment != NULL) {
                            size_t len = assignment - badoptname;
                            if(len > CMDLINE_AUXBUF_SIZE)
                                len = CMDLINE_AUXBUF_SIZE;
                            strncpy(auxbuf, badoptname, len);
                            auxbuf[len] = '\0';
                            badoptname = auxbuf;
                        }
                    }

                    ret = callback(CMDLINE_OPTID_UNKNOWN, badoptname, userdata);
                }
            }
        }

        if(ret != 0)
            return ret;
        i++;
    }

    return ret;
}

