/*
-Procedure  zzerrorinit ( Initialize the SPICE error subsytem to RETURN/NULL )

-Abstract

   Set the CSPICE error subsystem to RETURN mode, and the error
   device to NULL.

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

   error

*/

#include "SpiceUsr.h"
#include "zzerror.h"

void zzerrorinit(void)

/*

-Brief_I/O

   None.

-Detailed_Input

   None.

-Detailed_Output

   None.

-Parameters

   None.

-Exceptions

   None.

-Files

   None.

-Particulars

   Initializes the error subsystem to the "RETURN" state, sets
   the error output device to NULL (no output).

   Call this routine prior to calling zzerror.

-Examples

   Expected use, check failed, return the error string:
   
      /.
      Initialize the error system to RETURN/NULL
      ./
      zzerrorinit();

         ... CSPICE calls ...
   
      /.
      Check for a failure, return the error string if
      failed_c returns true.
      ./ 
      if( failed_c() )
         {
         error_str = zzerror( index );
         
         /.
         Return the error string traceback to
         the calling program.
         ./
         error_return( error_str );
         }

-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   E. D. Wright    (JPL)

-Version

   CSPICE 1.0.0 17-OCT-2005 (EDW)

      Initial release to CSPICE

-Index_Entries

   set error system return mode

-&
*/
   {
    
   static SpiceBoolean     first = SPICETRUE;

   /*
   Prevent repeated executions of code with the 'first' flag.
   */
   if ( first )
      {

      /*
      Explicitly set the error subsystem to return mode, the 
      error output device to NULL.
      */

      erract_c("SET", (SpiceInt) sizeof("RETURN"), "RETURN");
      errdev_c("SET", (SpiceInt) sizeof("NULL")  , "NULL"  );

      }

   }

