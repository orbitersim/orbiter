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
   file dtime_.c, which was included in the 1998-09-13 f2c 
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
   code was not in the original version of dtime_.c obtained with
   the f2c distribution.
   */
   #ifdef MIX_C_AND_FORTRAN
      #include "f2cMang.h"
   #endif
   /*
   End of modification.  
   */


#include "time.h"

#ifdef MSDOS
#define USE_CLOCK
#endif

#ifndef USE_CLOCK
#define _INCLUDE_POSIX_SOURCE   /* for HP-UX */
#define _INCLUDE_XOPEN_SOURCE   /* for HP-UX */
#include "sys/types.h"
#include "sys/times.h"
#endif

#undef Hz
#ifdef CLK_TCK
#define Hz CLK_TCK
#else
#ifdef HZ
#define Hz HZ
#else
#define Hz 60
#endif
#endif

 double
#ifdef KR_headers
dtime_(tarray) float *tarray;
#else
dtime_(float *tarray)
#endif
{
#ifdef USE_CLOCK
#ifndef CLOCKS_PER_SECOND
#define CLOCKS_PER_SECOND Hz
#endif
   static double t0;
   double t = clock();
   tarray[1] = 0;
   tarray[0] = (t - t0) / CLOCKS_PER_SECOND;
   t0 = t;
   return tarray[0];
#else
   struct tms t;
   static struct tms t0;

   times(&t);
   tarray[0] = (t.tms_utime - t0.tms_utime) / Hz;
   tarray[1] = (t.tms_stime - t0.tms_stime) / Hz;
   t0 = t;
   return tarray[0] + tarray[1];
#endif
   }
