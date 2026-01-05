/*

-Procedure frmnam_c (Frame to Name)

-Abstract

   Retrieve the name of a reference frame associated with a SPICE ID
   code.

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

   FRAMES

-Keywords

   FRAMES

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZmc.h"
   #include "SpiceZst.h"


   void frmnam_c ( SpiceInt      frcode,
                   SpiceInt      frnlen,
                   SpiceChar *   frname  )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   frcode     I   an integer code for a reference frame
   frnlen     I   Maximum length of output string.
   frname     O   the name associated with the reference frame.

-Detailed_Input

   frcode      is an integer code for a reference frame.

   frnlen      is the maximum number of characters that can be accommodated in
               the output string. This count includes room for the terminating
               null character. For example, if the maximum allowed length of
               the output string `frnnm', including the terminating null, is 33
               characters, then `frnlen' should be set to 33.

-Detailed_Output

   frname      is the name associated with the reference frame. It will
               be returned left justified.

               If `frcode' is not recognized as the name of a known
               reference frame, `frname' will be returned as an empty
               string.

               If `frname' is not sufficiently long to hold the name, it
               will be truncated on the right.

               All reference frame names are 32 or fewer characters in
               length. Thus declaring `frname' to be SpiceChar[33] will
               ensure that the returned name will not be truncated.

-Parameters

   None.

-Exceptions

   1)  If `frcode' is not recognized as the name of a known reference
       frame, `frname' will be returned as a blank.

   2)  If `frname' is not sufficiently long to hold the name, it will
       be truncated on the right.

   3)  If the `frname' output string pointer is null, the error
       SPICE(NULLPOINTER) is signaled.

   4)  If the `frname' output string has length less than two
       characters, the error SPICE(STRINGTOOSHORT) is signaled, since
       the output string is too short to contain one character of
       output data plus a null terminator.

-Files

   None.

-Particulars

   This routine retrieves the name of a reference frame associated
   with a SPICE frame ID code.

   The ID codes stored locally are scanned for a match with `frcode'.
   If a match is found, the name stored locally will be returned
   as the name for the frame.

   If `frcode' is not a member of the list of internally stored
   ID codes, the kernel pool will be examined to see if the
   variable

      FRAME_idcode_NAME

   is present (where idcode is the decimal character equivalent
   of `frcode'). If the variable is located and it has both
   character type and dimension 1, the string value of the
   kernel pool variable is returned as the name of the reference
   frame.

   Note that because the local information is always examined
   first and searches of the kernel pool are performed only
   after exhausting local information, it is not possible to
   override the local name for any reference frame that is
   known by this routine.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Given a SPICE frame ID, retrieve its associated frame name.

      Example code begins here.


      /.
         Program frmnam_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main( )
      {
         /.
         Local constants
         ./
         #define NAMELEN         33

         SpiceInt                frcode = 13000;

         /.
         Local variables
         ./
         SpiceChar               frname [NAMELEN];

         /.
         What frame name maps to ID code 13000?
         ./
         frmnam_c ( frcode, NAMELEN, frname );

         if ( !iswhsp_c(frname) )
         {
            printf ( "Frame ID  : %d\n", (int)frcode );
            printf ( "Frame name: %s\n", frname      );
         }
         else
         {
            printf ( "ID code %d not recognized as a SPICE frame ID\n",
                     (int)frcode                                      );
         }

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Frame ID  : 13000
      Frame name: ITRF93


-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   B.V. Semenov        (JPL)
   W.L. Taber          (JPL)
   E.D. Wright         (JPL)

-Version

   -CSPICE Version 1.1.0, 10-AUG-2021 (JDR)

       Changed the input argument name "lenout" to "frnlen" for consistency
       with other routines.

       Edited the header to comply with NAIF standard. Added complete code
       example.

   -CSPICE Version 1.0.3, 12-JUL-2016 (EDW)

       Edit to example program to use "%d" with explicit casts
       to int for printing SpiceInts with printf.

   -CSPICE Version 1.0.2, 08-JAN-2014 (BVS)

       Fixed typo in -Examples (frname_c -> frmnam_c). Reordered
       header sections.

   -CSPICE Version 1.0.1, 26-MAR-2003 (NJB)

       Fixed description of exception (4):  replaced "frnlen-1"
       with "frnlen." Removed spurious word "clock" from string
       description.

   -CSPICE Version 1.0.0, 13-AUG-2001 (NJB) (WLT)

-Index_Entries

   Frame idcode to frame name translation

-&
*/

{ /* Begin frmnam_c */


   /*
   Participate in error tracing.
   */
   chkin_c ( "frmnam_c" );

   /*
   Make sure the output frmnam has at least enough room for one output
   character and a null terminator.  Also check for a null pointer.
   */
   CHKOSTR ( CHK_STANDARD, "frmnam_c", frname, frnlen );


   /*
   Do the conversion.
   */
   frmnam_ ( ( integer * ) &frcode,
             ( char    * ) frname,
             ( ftnlen    ) frnlen-1 );

   /*
   Convert the Fortran string to a C string by placing a null
   after the last non-blank character.  This operation is valid
   whether or not the CSPICE routine signaled an error.
   */
   F2C_ConvertStr ( frnlen, frname );


   chkout_c ( "frmnam_c" );

} /* End frmnam_c */
