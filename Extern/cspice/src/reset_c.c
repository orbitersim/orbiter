/*

-Procedure reset_c ( Reset Error Status )

-Abstract

   Reset the CSPICE error status to a value of "no error."
   As a result, the status routine, failed_c, will return a value
   of SPICEFALSE

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

   ERROR

-Keywords

   ERROR

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"


   void reset_c ( void )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   None.

-Detailed_Input

   None.

-Detailed_Output

   None.

-Parameters

   None.

-Exceptions

   Error free.

   1)  This routine does not detect any errors.

       However, this routine is part of the CSPICE error
       handling mechanism.

-Files

   None.

-Particulars

   Please read the "required reading" first!

   The effects of this routine are:

   1. The CSPICE status is set to a value of "no error."

   2. The long and short error messages are set to blank.

   3. Setting of the long error message is re-enabled.


   Subsequent to a call to reset_c, references to the status
   indicator function, failed_c, will return a value of SPICEFALSE,
   until an error is detected.

   This routine should be called in cases where one wishes
   to attempt to continue processing after detection of an
   error, and the "RETURN" error action is being used. When
   the error response action is set to "RETURN", routines
   that have external references, or that can
   detect errors, return immediately upon entry when an
   error condition exists. This prevents a program from
   crashing, but does not allow for a recovery attempt.

   If one does wish to attempt to recover,
   in general the procedure is to test for an error
   condition, and if one exists, respond to the error
   (by outputting diagnostic messages, for example).  Next,
   a call to reset_c can be made. After resetting the
   error status, the normal execution thread can be resumed.

   It is also appropriate to call this routine when the error
   response action is "REPORT", if one wishes to recover
   from errors.

-Examples

   1. In this example, we try to read a line from the file,
       SPUD.DAT, using the toolkit routine, rdtext_c.
       When failed_c indicates an error, we grab the short
       error message and its explanation, using getmsg_c (see),
       log the messages using our user-defined routine,
       USER_LOG (NOT a CSPICE routine), reset the
       status, and keep going.

          /.
          We read a line from SPUD.DAT:
          ./

          rdtext_c ( "SPUD.DAT", line, LENOUT, &eof );

          if ( failed_c() )
             {

             /.
             Oops! an error occurred during the read.
             Recover the short error message and its
             explanation, reset the error status,
             log the messages, and continue...
             ./

             getmsg_c   ( "SHORT"  , LENOUT, short_mess );
             getmsg_c   ( "EXPLAIN", LENOUT, explain_mess );

             USER_LOG (  SMSG );
             USER_LOG (  EXPL );

             reset_c();
             }

-Restrictions

   1)  It can be dangerous to call this routine without
       RESPONDING to the error condition first; by calling
       reset_c, you are wiping out the CSPICE's knowledge of
       the error.

-Literature_References

   None.

-Author_and_Institution

   J. Diaz del Rio     (ODC Space)
   K.R. Gehringer      (JPL)
   E.D. Wright         (JPL)

-Version

   -CSPICE Version 1.0.2, 02-JUN-2021 (JDR)

       Edited the header to comply with NAIF standard.

   -CSPICE Version 1.0.1, 25-MAR-1998 (EDW)

       Minor corrections to header.

   -CSPICE Version 1.0.0, 08-FEB-1998 (EDW) (KRG)

-Index_Entries

   reset error status

-&
*/

{ /* Begin rest_c */

   /*
   Call the f2c'd Fortran routine.
   */
   reset_();


} /* End reset_c */
