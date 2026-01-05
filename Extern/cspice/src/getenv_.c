/*

-Procedure getenv_ ( CSPICE version of the getenv_.c routine )

-Abstract

   This file replaces the standard f2c getenv_.c library file. The getenv_
   code now branches to Mac classic and non Mac classic code. The non Mac
   code matches the standard f2c library version, the Mac classic code
   returns a 0 as Mac classic has no environmental variable facility.
      
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

-Required_Reading

   None.
   
-Keywords

   TBD.

-Brief_I/O

   TBD.

-Detailed_Input

   TBD.

-Detailed_Output

   TBD.

-Parameters

   TBD.

-Exceptions

   TBD.

-Files

   TBD.

-Particulars 

   getenv - f77 subroutine to return environment variables

   called by:
      call getenv (ENV_NAME, char_var)
   where:
      ENV_NAME is the name of an environment variable
      char_var is a character variable which will receive
                  the current value of ENV_NAME, or all blanks
                  if ENV_NAME is not defined

-Examples

   TBD.

-Restrictions

  1) Requires CSPICE f2c.h header file.
     
   
-Literature_References

   None.

-Author_and_Institution

   E.D. Wright       (JPL)
   
-Version

   -CSPICE Version 1.0.2, 03-APR-2009 (EDW)

      Undefined the "min" and "max" macros prior to the #include "f2c.h"
      directive. f2c.h defines "min" and "max" as part of f2c.

   -CSPICE Version 1.0.1, 31-MAY-2007 (EDW)  

      Added include for stdlib.h. Corrected typo in header description.
      Removed CSPICE_MACPPC ifdef.

   -CSPICE Version 1.0.0, 02-JAN-2002 (EDW)  

-Index_Entries

   None.

-&
*/

#include <stdlib.h>

/* 
Undefine min and max macros to prevent a macro redefine warning
from the min and max defintions in f2c.h. 
*/
#undef min
#undef max

#include "f2c.h"

#ifdef KR_headers

void getenv_(fname, value, flen, vlen) char  *value, *fname;
                                          ftnlen vlen , flen;

#else

void getenv_(char *fname, char *value, ftnlen flen, ftnlen vlen)

#endif
   {

   extern   char **environ;
   register char *ep, *fp, *flast;
   register char **env = environ;

   flast = fname + flen;

   for(fp = fname ; fp < flast ; ++fp)
      {
      
      if(*fp == ' ')
         {
         flast = fp;
         break;
         }

      }

   while (ep = *env++)
      {

      for(fp = fname; fp<flast ; )
         {
         if(*fp++ != *ep++)
            {
            goto endloop;
            }
         }

      if(*ep++ == '=') 
         { 

         /* copy right hand side */

         while( *ep && --vlen>=0 )
            {
            *value++ = *ep++;
            }

         goto blank;
         }

      endloop: 
         ;
      }

   blank:
      while( --vlen >= 0 )
         {
         *value++ = ' ';
         }

   }

