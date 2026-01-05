/*

-Procedure return_c ( Immediate Return Indicator )

-Abstract

   Return SPICETRUE if CSPICE routines should return immediately upon
   entry.

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

   SpiceBoolean return_c ( void )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------

   The function returns the value, SPICETRUE, if and only if CSPICE
   routines should return immediately upon entry.

-Detailed_Input

   None.

-Detailed_Output

   The function returns the value, SPICETRUE, if and only if CSPICE
   routines should return immediately upon entry. The criterion
   for this is that the error response action is set to
   "RETURN", and an error condition exists.

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

   This routine can be referenced in non-toolkit code; in
   fact, its use is encouraged. Its purpose is to signal
   to the routine calling it that the caller should
   return immediately. The reference to return_c should
   be the first executable line of the calling program.

   In "RETURN" mode, CSPICE routines
   that have external references, or that can
   detect errors, return immediately upon entry when an
   error condition exists. They use return_c to determine
   when these conditions are met. Non--toolkit routines
   can do the same.

   Additionally, when an error is signaled in "RETURN" mode,
   no further errors can be signaled until the error condition
   is reset by a call to reset_c. Calls to SIGERR simply have
   no effect. Therefore, the error messages set in response
   to the FIRST error that was detected will be saved until
   reset_c is called. These messages can be retrieved by
   calls to getmsg_c.

   There are a number of advantages to using this mechanism.
   First, the likelihood of an error resulting in crash
   in a different routine is greatly reduced. Second,
   a program does not have to test the error status
   (using a reference to failed_c) after each call to a toolkit
   routine, but rather can make one test of status at the end
   of a series of calls. See "Examples" below.

   See the subroutine erract_c for definitions of the error action
   codes.

-Examples

   1. In this example, we show how to place a reference
       to return_c in your code:

      /.
             No executable lines precede this one.

             Test whether to return before doing
             anything else.
      ./

             if ( return_c() )
                {
                return;
                }


             [ rest of code goes here]

                       .
                       .
                       .


   2. Here's how one might code a sequence of calls
       to routines with code that follows the pattern
       given in example #1 above:

                      .
                      .
                      .

             [ code may go here ]

      /.
             We call routines A, B, and C;  then we
             test for errors, using the CSPICE error
             status indicator, failed_c:
      ./

             A();
             B();
             C();

             if ( failed_c() )
                {

      /.
                If we're here, an error occurred. The
                error might have been detected by A, B, C,
                or by a routine called by one of them.
                Get the explanation of the short error message
                and output it using the routine, user_out_c
                [user_out_c is a fictitious routine]:
      ./

                getmsg_c ( "EXPLAIN", MSG );

                user_out_c ( MSG );

                }

             [ rest of code goes here ]

                        .
                        .
                        .

-Restrictions

   1)  This routine has no effect unless the error action is "RETURN"!

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   E.D. Wright         (JPL)

-Version

   -CSPICE Version 1.1.1, 01-NOV-2021 (JDR)

       Edited the header to comply with NAIF standard.

   -CSPICE Version 1.1.0, 23-JUL-2001 (NJB)

       Removed tab characters from source file.

   -CSPICE Version 1.0.0, 08-FEB-1998 (EDW)

-Index_Entries

   immediate return indicator

-&
*/

{ /* Begin return_c */

   /*
   Call the f2c'd Fortran routine and set the status.
   */

   if ( return_() )
      {
      return SPICETRUE;
      }

   else
      {
      return SPICEFALSE;
      }


} /* End return_c */
