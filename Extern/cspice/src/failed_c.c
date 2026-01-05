/*

-Procedure failed_c ( Error Status Indicator )

-Abstract

   Return SPICETRUE if an error condition has been signaled via sigerr_c.
   failed_c is the CSPICE status indicator.

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

   SpiceBoolean failed_c ()

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------

   The function takes the value SPICETRUE if an error condition
   was detected; it is SPICEFALSE otherwise.

-Detailed_Input

   None.

-Detailed_Output

   Please read the required reading file before reading this!

   The value taken by failed_c indicates status.

   The status value applies to the CSPICE routines,
   and to any other routines which call the status-setting
   routine, sigerr_c.

   When failed_c has the value, SPICETRUE, an error condition
   exists. SPICEFALSE means "no error."

   More specifically, when failed_c has the value SPICETRUE,
   some routine has indicated an error by calling the
   CSPICE routine, sigerr_c. All CSPICE routines
   which can detect errors do this. Non-CSPICE
   routines may also reference sigerr_c if desired.

   When failed_c has the value SPICEFALSE, either no routine
   has yet signaled an error via sigerr_c, or the status
   has been reset using, what else, reset_c.

   failed_c is initialized to have the value, SPICEFALSE
   This indicates a  "no error" status.

   See "particulars" below for (slightly) more information.

-Parameters

   None.

-Exceptions

   Error free.

   1)  However, this routine is part of the CSPICE error
       handling mechanism.

-Files

   None.

-Particulars

   See the required reading file for details of error
   processing. However, here are some notes:

   When any CSPICE routine detects an error, the
   status is set to indicate an error condition via
   a call to sigerr_c. After sigerr_c
   returns, further calls to failed_c will return the
   value, SPICETRUE, indicating an error condition.

   Non-CSPICE routines may also call sigerr_c to indicate
   an error condition; failed_c will reflect such calls
   as well.

   It is possible to re-set the error status to indicate
   "no error" using the CSPICE routine, reset_c (see).

   The effect on failed_c of resetting the status is
   that failed_c will again return the value SPICEFALSE,
   indicating "no error."

   One of the main virtues of the CSPICE error
   handling mechanism is that you don't HAVE to test the
   error status after every call to a CSPICE routine.
   If you set the error handling mode to "RETURN", using
   the routine, erract_c, CSPICE routines won't crash
   when an error occurs; following the detection of the
   error, each routine will return immediately upon entry.
   Therefore, you call several CSPICE routines in a
   row, and just test status at the end of the sequence
   of calls, if you wish. See "examples" below.

-Examples

   1. Here's an example of a simple call to rdtext_c, followed
       by a test of the status.


           /.
           We read a line of text from file SPUD.DAT:
           ./

          rdtext_c ( "SPUD.DAT", line, LENOUT, &eof );

          if ( failed_c() )
             {

             /. An error occurred during the read. ./

             [respond to error here]

             }


   2. Here's an example in which we don't want to
         put the error test inside our loop. We just
         test the error status after the loop terminates.
         We can do this because we (that is, you, the user)
         have made the call,

                erract_c ( "RETURN", LENOUT, msg );

         prior to execution of the following code. If an
         error does occur, the remaining calls to rdtext_c
         will have no effect. Here's the example:

         /.
         We read the first 5000 lines of a file, or until
         EOF is reached, whichever comes first:
         ./

         lcount = 0;

         do {

            rdtext_c ( "SPUD.DAT", line[lcount], LENOUT, &eof );

            lcount++;

            }
         while (  !( eof ) && ( lcount <= 5000 )  );


         if ( failed_c() )
             {

             /. An error occurred during the read. ./

             [respond to error here]

             }

-Restrictions

   1)  This routine automatically detects errors occurring in
       the CSPICE code. To make this routine work for your own
       routines, your routines must call sigerr_c to report errors.

-Literature_References

   None.

-Author_and_Institution

   J. Diaz del Rio     (ODC Space)
   E.D. Wright         (JPL)

-Version

   -CSPICE Version 1.0.2, 01-NOV-2021 (JDR)

       Edited the header to comply with NAIF standard.

   -CSPICE Version 1.0.1, 08-FEB-1998 (EDW)

       Minor corrections to header information.

   -CSPICE Version 1.0.0, 25-OCT-1997 (EDW)

-Index_Entries

   error status indicator

-&
*/

{ /* Begin failed_c */

   /*
   Call the f2c'd Fortran routine and return the status.  Not much else
   to say.
   */

   if ( (SpiceBoolean) failed_() )
     {
     return SPICETRUE;
     }
   else
     {
     return SPICEFALSE;
     }


} /* End failed_c */
