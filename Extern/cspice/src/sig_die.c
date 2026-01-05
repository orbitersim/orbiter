/*

-Disclaimer

   THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
   CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
   GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
   ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
   PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
   TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
   WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
   PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
   SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
   SOFTWARE AND RELATED MATERIALS, HOWEVER USED.

   IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
   BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
   LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
   INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
   REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
   REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.

   RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
   THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
   CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
   ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.

*/

/*

-Description   

   This is a slightly modified version of the f2c library 
   file sig_die.c, which was included in the 1998-09-13 f2c 
   distribution.

   This file has been modified as follows:

     1) This "header" text has been added.

     2) The file optionally invokes macros that mangle the 
        external symbols in f2c's F77 and I77 libraries.  The
        purpose of this is to allow programs to link to 
        CSPICE and also link to Fortran objects that do 
        Fortran I/O.

        The mangling is invoked by defining the preprocessor
        flag

           MIX_C_AND_FORTRAN


   The name mangling capability used by this routine should only be
   used as a last resort.
   
-Version

   -CSPICE Version 1.0.0, 19-DEC-2001 (NJB)
   
-&
*/

   /*
   Mangle external symbols if we're mixing C and Fortran.  This
   code was not in the original version of sig_die.c obtained with
   the f2c distribution.
   */
   #ifdef MIX_C_AND_FORTRAN
      #include "f2cMang.h"
   #endif
   /*
   End of modification.  
   */

#include "stdio.h"
#include "signal.h"

#ifndef SIGIOT
#ifdef SIGABRT
#define SIGIOT SIGABRT
#endif
#endif

#ifdef KR_headers
void sig_die(s, kill) register char *s; int kill;
#else
#include "stdlib.h"
#ifdef __cplusplus
extern "C" {
#endif
 extern void f_exit(void);

void sig_die(register char *s, int kill)
#endif
{
   /* print error message, then clear buffers */
   fprintf(stderr, "%s\n", s);

   if(kill)
      {
      fflush(stderr);
      f_exit();
      fflush(stderr);
      /* now get a core */
#ifdef SIGIOT
      signal(SIGIOT, SIG_DFL);
#endif
      abort();
      }
   else {
#ifdef NO_ONEXIT
      f_exit();
#endif
      exit(1);
      }
   }
#ifdef __cplusplus
}
#endif
