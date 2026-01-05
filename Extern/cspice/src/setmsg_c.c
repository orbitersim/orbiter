/*

-Procedure setmsg_c  ( Set Long Error Message )

-Abstract

   Set the value of the current long error message.

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
   #include "SpiceZst.h"
   #include "SpiceZmc.h"


   void setmsg_c ( ConstSpiceChar * msg )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   msg        I   A long error message.

-Detailed_Input

   msg         is a ``long'' error message.
               `msg' is a detailed description of the error.
               `msg' is supposed to start with the name of the
               module which detected the error, followed by a
               colon. Example:

                  "rdtext_c:  There are no more free logical units"

               Only the first LMSGLN (see setmsg.c) characters of
               `msg' are stored; any further characters are
               truncated.

               Generally, `msg' will be stored internally by the
               CSPICE error handling mechanism. The only exception
               is the case in which the user has commanded the
               toolkit to ``ignore'' the error indicated by `msg'.

               As a default, `msg' will be output to the screen.
               See the required reading file for a discussion of how
               to customize toolkit error handling behavior, and
               in particular, the disposition of `msg'.

-Detailed_Output

   None.

-Parameters

   None.

-Exceptions

   1)  This routine is part of the interface to the CSPICE error
       handling mechanism. For this reason, this routine does not
       participate in the trace scheme, even though it has external
       references, except in the following two cases.

   2)  If the `msg' input string pointer is null, the error
       SPICE(NULLPOINTER) is signaled.

   3)  If the `msg' input string has zero length, the error
       SPICE(EMPTYSTRING) is signaled.

-Files

   None.

-Particulars

   The CSPICE routine sigerr_c should always be called
   AFTER this routine is called, when an error is detected.

   The effects of this routine are:

      1. If acceptance of a new long error message is
          allowed:

          `msg' will be stored internally. As a result,
          The CSPICE routine, getmsg_ , will be able to
          retrieve `msg', until `msg' has been ``erased''
          by a call to reset_c, or overwritten by another
          call to setmsg_c.


      2. If acceptance of a new long error message is not allowed,
          a call to this routine has no effect.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Create a user-defined error message, including both the
      short and long messages, providing the value of an integer
      and a double precision variables within the long message,
      and signal the error.


      Example code begins here.


      /.
         Program setmsg_ex1
      ./
      #include "SpiceUsr.h"

      int main()
      {

         /.
         Set long error message, with two different `marker'
         strings where the value of the variables will go.
         Our markers are "#" and "XX".
         ./
         setmsg_c ( "LONG MESSAGE.  Invalid operation value. "
                    "  The value was #.  Left endpoint "
                    "exceeded right endpoint.  The left "
                    "endpoint was:  XX."                      );

         /.
         Insert the integer number where the # is now.
         ./
         errint_c ( "#", 5 );

         /.
         Insert a double precision number where the XX is now.
         ./
         errdp_c ( "XX", 910.26111991 );

         /.
         Signal the error.
         ./
         sigerr_c ( "SPICE(USERDEFINED)" );

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      =====================================================================

      Toolkit version: N0066

      SPICE(USERDEFINED) --

      LONG MESSAGE. Invalid operation value. The value was 5. Left endpoint
      exceeded right endpoint. The left endpoint was: 9.1026111991000E+02.

      Oh, by the way:  The SPICELIB error handling actions are USER-
      TAILORABLE.  You can choose whether the Toolkit aborts or continues
      when errors occur, which error messages to output, and where to send
      the output.  Please read the ERROR "Required Reading" file, or see
      the routines ERRACT, ERRDEV, and ERRPRT.

      =====================================================================


      Note that the execution of this program produces the error
      SPICE(USERDEFINED), which follows the NAIF standard as
      described in the ERROR required reading.

-Restrictions

   1)  sigerr_c must be called once after each call to this routine.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   E.D. Wright         (JPL)

-Version

   -CSPICE Version 1.3.0, 05-AUG-2021 (JDR)

       Changed input argument "message" to "msg" for consistency with
       other routines.

       Edited the header to comply with NAIF standard. Added
       complete code example.

       Improved entry #1 and added entries #2 and #3 in -Exceptions
       section.

   -CSPICE Version 1.2.1, 25-MAR-1998 (EDW)

       Corrected errors in header.

   -CSPICE Version 1.2.0, 08-FEB-1998 (NJB)

       Re-implemented routine without dynamically allocated, temporary
       strings. Made various header fixes.

   -CSPICE Version 1.0.0, 25-OCT-1997 (EDW)

-Index_Entries

   set long error message

-&
*/

{ /* Begin setmsg_c */

   /* Local Variables */

   /*
   Check the input string to make sure the pointer is non-null
   and the string length is non-zero.
   */
   CHKFSTR ( CHK_DISCOVER, "setmsg_c", msg );


   /*
   Call the f2c'd Fortran routine.
   */
   setmsg_ ( ( char  * ) msg,
             ( ftnlen  ) strlen(msg) );


} /* End setmsg_c */
