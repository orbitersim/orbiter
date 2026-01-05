/*

-Procedure zzerror ( Cat and return the long, short, and traceback
                     error strings)

-Abstract

   The default CSPICE behavior signals an exit on a CSPICE error.
   This action often conflicts with the error model used by other
   programming languages: IDL, Perl, MATLAB, etc. zzerrorinit
   and zzerror implement logic to permit easy use of another
   error model.

   zzerror retrieves the long error message, the short error,
   message and the call trace back, assembling those components
   into a single string, which also contains the CSPICE toolkit
   version ID, for return to the caller. This call also resets the
   failed_c() state.

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

#include <string.h>
#include <stdio.h>

#include "SpiceUsr.h"
#include "SpiceZfc.h"
#include "SpiceZst.h"
#include "zzerror.h"

#define     MSG_LEN             2024
#define     TRC_LEN             32
#define     MAXMOD              100
#define     OUT_LEN             2*MSG_LEN

const char * zzerror( long cnt )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   cnt        I   Either a flag (less than 0) indicating a scalar or an
                  array index.

   The function returns a string version of the SPICE error output.

-Detailed_Input

   cnt        A long integer defining the index of a vector at which
              the error signaled or a negative value indicating the
              error occurred during a scalar operation.

-Detailed_Output

   The function returns a pointer to a string (char *), the string
   containing the SPICE short and long error messages, plus
   the full trace back and the CSPICE toolkit version ID. If the error
   is signaled during a vectorized operation, the error string includes
   the vector index at failure.

-Parameters

   MSG_LEN    one half the max length of the return string. The return
              string has dimension 2*MSG_LEN.

   TRC_LEN    the max length of a string returned from trcnam_.

   MAXMOD     is the maximum storage depth for names in the
              traceback stack. Value copied from trcpkg.f.

-Exceptions

   1) If trcdep_ returns a 'depth' value larger than the maximum depth
      as assigned to MAXMOD, the routine returns a SPICE(BUG) error
      and error message to the caller.

-Files

   None.

-Particulars

   All interface functions immediately check failed_c() after
   calling CSPICE. When failed_c() returns SPICETRUE,
   the interface performs the appropriate action to return the
   error state to the interpreter.

   Call after detecting a failed_c() event.

   The user should call zzerrorinit prior to a zzerror call.
   zzerrorinit places the error subsystem in the RETURN/NULL
   state.

   This routine makes a call to reset_c to reset the error
   system to an non-error state. The call causes the following:

      failed_c returns SPICEFALSE value until another error signal.

      return_c returns SPICEFALSE value until another error signal.

      getsms_ and getlms_ return blank strings.

      The traceback routines return a traceback of the current
      active call chain, not the active call chain at the time
      of the last error.

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

   Example of a string returned by zzerror:

      In scalar context-

      cspice_str2et, 'Jan 1, 2049', et

      SPICE(NOLEAPSECONDS): [str2et_c->STR2ET->TTRANS] The variable
      that points to the leapseconds (DELTET/DELTA_AT) could not be
      located in the kernel pool.  It is likely that the leapseconds
      kernel has not been loaded via the routine FURNSH. (CSPICE_N0066)


      In a vector context-

      cspice_furnsh, 'naif0012.tls'
      cspice_furnsh, 'de405.bsp'

      cspice_str2et, 'Jan 1, 2049', et
      et_vec = dindgen(5000)*10000d + et

      cspice_spkezr, 'MOON', et_vec, 'J2000', 'NONE', 'EARTH', starg, ltime

      Creates the string

      SPICE(SPKINSUFFDATA): [spkezr_c->SPKEZR->SPKEZ->SPKGEO]
      Insufficient ephemeris data has been loaded to compute the
      state of 301 (MOON) relative to 399 (EARTH) at the
      ephemeris epoch 2050 JAN 01 01:07:44.183. Failure at input
      vector index 3154. (CSPICE_N0066)

-Restrictions

   Use with the SPICE error system in RETURN mode and the error
   device set to NULL.

-Literature_References

   None.

-Author_and_Institution

   J. Diaz del Rio     (ODC Space)
   E.D. Wright         (JPL)

-Version

   -CSPICE Version 2.0.0, 15-JAN-2021 (EDW) (JDR)

       Return string now includes the Toolkit version ID as returned
       by tkvrsn().

   -CSPICE Version 1.1.1, 08-MAR-2007 (EDW)

       Corrected spelling mistake in error message string.

   -CSPICE Version 1.1.0, 24-APR-2006 (EDW)

       Version 1.0.0 contained an extraneous chkin_c call which caused
       a cascade of 'zzerror_c' strings prefixed to error strings. This
       call bug was removed.

       Replaced LDPOOL reference in header docs with FURNSH.

   -CSPICE Version 1.0.0, 17-OCT-2005 (EDW)

       Initial release to CSPICE

-Index_Entries

   error message

-&

*/
   {

   /*
   Local variables. Tag the 'msg_short' as static so the memory
   remains after return.

   We append to 'msg_short' hence the reason for it having the
   largest size.

   */
   static char msg_short [OUT_LEN];
   char        msg_long  [MSG_LEN];
   char        trname    [TRC_LEN];


   /*
   Define an error message string for the case if the trcdep_
   call returns a value larger than MAXMOD.
   */
   char      * depth_err = "SPICE(BUG): [zzerror]. An error "
                           "occurred during the processing of a SPICE "
                           "error signal. The trcdep_ routine "
                           "returned a depth, %i, larger than the "
                           "maximum allowed depth, %i. Please "
                           "contact NAIF.";

   SpiceInt    i;

   SpiceInt    depth;
   SpiceChar   trlist[MAXMOD*TRC_LEN];


   /*
   Zero out the char arrays, just-in-case.
   */
   memset( msg_short, 0, 2 *MSG_LEN        );
   memset( msg_long,  0,    MSG_LEN        );
   memset( trlist,    0,    MAXMOD*TRC_LEN );

   /*
   Retrieve the depth of the call traceback stack.
   */
   (void) trcdep_( &depth );

   /*
   Check 'depth' as less-than or equal-to MAXMOD. Signal a
   SPICE error if not confirmed.
   */
   if ( depth > MAXMOD )
      {
      reset_c();

      sprintf( msg_short, depth_err, depth, MAXMOD );

      sprintf( msg_short + strlen(msg_short),
             " (%s)", tkvrsn_c("TOOLKIT") );

      return(msg_short);
      }


   /*
   Loop over the number of items in the trace list.
   Index starts at 1 as trcnam_ is an f2c'd routine.
   */
   for ( i=1; i<= depth; i++)
      {

      /*
      Retrieve the name (as a FORTRAN string) of the ith routine's name
      from the trace stack. No SPICE call name has a string length longer
      than TRC_LEN characters.
      */
      (void) trcnam_( (integer *) &i, trname, (ftnlen) TRC_LEN );

      /*
      The f2c code returns a FORTRAN type string, so null terminate
      the string for C.
      */
      F2C_ConvertStr( TRC_LEN, trname);

      /*
      Create the trace list string by concatenation. Add '->' as a
      marker between the routine names except on the first pass through
      the loop.
      */
      if ( i != 1 )
         {
         strcat( trlist, "->" );
         }
      strcat( trlist, trname );

      }

   /*
   Retrieve the short message from the error subsystem. The string has
   form "SPICE(MSGNAME)".
   */
   (void) getsms_(msg_short, (SpiceInt) sizeof msg_short);

   /*
   Null terminate the FORTRAN 'msg_short' string for use in C routines.
   */
   F2C_ConvertStr( 2*MSG_LEN, msg_short);

   /*
   Obtain the long message string, a brief description of the error.
   */
   (void) getlms_(msg_long, (ftnlen) sizeof(msg_long));

   /*
   Null terminate the FORTRAN 'msg_long' string for use in C routines.
   */
   F2C_ConvertStr( MSG_LEN, msg_long);

   /*
   Remember to reset the error system, so subsequent calls work.
   */
   reset_c();

   /*
   Combine the short, long and trace strings into a single string, then
   return the string.
   */
   sprintf( msg_short + strlen(msg_short),
            ": [%s] %s", trlist, msg_long );

   /*
   Add the index value for errors from vectorized functions. Scalar
   functions set 'cnt' to anything less than zero (normally -1 or -2).
   */
   if ( cnt >= 0 )
      {
      sprintf( msg_short + strlen(msg_short),
               " Failure occurred at input vector index %ld.", cnt);
      }

   sprintf( msg_short + strlen(msg_short),
            " (%s)", tkvrsn_c("TOOLKIT") );

   return(msg_short);
   }

