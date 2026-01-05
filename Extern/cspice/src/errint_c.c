/*

-Procedure errint_c ( Insert Integer into Error Message Text )

-Abstract

   Substitute an integer for the first occurrence of a marker found
   in the current long error message.

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

   CONVERSION
   ERROR

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZst.h"
   #include "SpiceZmc.h"

   void errint_c ( ConstSpiceChar  * marker,
                   SpiceInt          intnum  )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   marker     I   A substring of the error message to be replaced.
   intnum     I   The integer to substitute for `marker'.

-Detailed_Input

   marker      is a character string which marks a position in
               the long error message where a character string
               representing an integer is to be substituted.
               Leading and trailing blanks in `marker' are not
               significant.

               Case IS significant;  "XX" is considered to be
               a different marker from "xx".

   intnum      is an integer whose character representation will
               be substituted for the first occurrence of `marker'
               in the long error message. This occurrence of the
               substring indicated by `marker' will be removed, and
               replaced by a character string, with no leading or
               trailing blanks, representing `intnum'.

-Detailed_Output

   None.

-Parameters

   None.

-Exceptions

   1)  If the `marker' input string pointer is null, the error
       SPICE(NULLPOINTER) is signaled.

   2)  If the `marker' input string has zero length, the error
       SPICE(EMPTYSTRING) is signaled.

-Files

   None.

-Particulars

   This routine updates the current long error message. If no marker
   is found, (e.g., in the case that the long error message is
   blank), the routine has no effect. If multiple instances of the
   marker designated by `marker' are found, only the first one is
   replaced.

   If the character string resulting from the substitution
   exceeds the maximum length of the long error message, the
   characters on the right are lost. No error is signaled.

   This routine has no effect if changes to the long message
   are not allowed.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Create a user-defined error message, including both the
      short and long messages, providing the value of two integer
      variables within the long message, and signal the error.


      Example code begins here.


      /.
         Program errint_ex1
      ./
      #include "SpiceUsr.h"

      int main()
      {

         /.
         Set long error message, with two different `marker'
         strings where the value of the integer variables
         will go.  Our markers are "#" and "XX".
         ./
         setmsg_c ( "LONG MESSAGE.  Invalid operation value. "
                    "  The value was #.  Left endpoint "
                    "exceeded right endpoint.  The left "
                    "endpoint was:  XX.  The right endpoint "
                    "was:  XX." );

         /.
         Insert the integer number where the # is now.
         ./
         errint_c ( "#", 5 );

         /.
         Insert another integer number in the long message
         where the first XX is now.
         ./
         errint_c ( "XX", 910 );

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
      exceeded right endpoint. The left endpoint was: 910. The right
      endpoint was: XX.

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

   1)  The caller must ensure that the message length, after sub-
       stitution is performed, doesn't exceed LMSGLN characters.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   E.D. Wright         (JPL)

-Version

   -CSPICE Version 1.2.0, 13-AUG-2021 (JDR)

       Changed the argument name "number" to "intnum" for consistency
       with other routines.

       Edited the header to comply with NAIF standard. Added
       complete code example based on existing code fragments.

   -CSPICE Version 1.1.0, 08-FEB-1998 (NJB)

       Re-implemented routine without dynamically allocated, temporary
       strings. Made various header fixes.

   -CSPICE Version 1.0.0, 25-OCT-1997 (EDW)

-Index_Entries

   insert integer into error message text

-&
*/

{ /* Begin errint_c */

   /*
   Check the input string marker to make sure the pointer is non-null
   and the string length is non-zero.  Since we don't check in
   prior to this, use the discovery check-in option.
   */
   CHKFSTR ( CHK_DISCOVER, "errint_c", marker );


   /*
   Call the f2c'd Fortran routine.
   */
   errint_ ( ( char    * ) marker,
             ( integer * ) &intnum,
             ( ftnlen    ) strlen(marker)  );


} /* End errint_c */
