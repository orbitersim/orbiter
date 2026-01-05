/*

-Procedure rdtext_c ( Read a line from a text file )

-Abstract

   Read the next line of text from a text file.

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

   FILES
   TEXT

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZst.h"
   #include "SpiceZmc.h"

   void rdtext_c ( ConstSpiceChar * file,
                   SpiceInt         lineln,
                   SpiceChar      * line,
                   SpiceBoolean   * eof    )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   file       I   Name of text file.
   lineln     I   Available room in output line.
   line       O   Next line from the text file.
   eof        O   End-of-file indicator.

-Detailed_Input

   file        is the name of the text file from which the next
               line is to be read. If the file is not currently
               open, it is opened with a logical unit determined
               at run time, and the first line of the file is
               returned. Otherwise, the next line not yet read
               from the file is read and returned.

   lineln      is the available room in the output line, including
               the terminating null. If the maximum expected length
               of an output line is N, `lineln' should be at least N+1.

-Detailed_Output

   line        is next line of text in the specified file.
               If the end of the file is reached, `line' is blank.

   eof         is SPICETRUE when the end of the file is reached, and is
               otherwise SPICEFALSE.

-Parameters

   None.

-Exceptions

   1)  If too many files are open already, the error
       SPICE(TOOMANYFILESOPEN) is signaled by a routine in the call
       tree of this routine.

   2)  If the attempt to open the file fails, the error
       SPICE(FILEOPENFAILED) is signaled by a routine in the call
       tree of this routine.

   3)  If the attempt to read from the file fails, the error
       SPICE(FILEREADFAILED) is signaled by a routine in the call
       tree of this routine.

   4)  If the attempt to "inquire" the status of the file fails, the
       error SPICE(INQUIREFAILED) is signaled by a routine in the
       call tree of this routine.

   5)  If the `file' input string pointer is null, the error
       SPICE(NULLPOINTER) is signaled.

   6)  If the `file' input string has zero length, the error
       SPICE(EMPTYSTRING) is signaled.

   7)  If the `line' output string pointer is null, the error
       SPICE(NULLPOINTER) is signaled.

   8)  If the `line' output string has length less than two
       characters, the error SPICE(STRINGTOOSHORT) is signaled, since
       the output string is too short to contain one character of
       output data plus a null terminator.

-Files

   See input `file'.

-Particulars

   rdtext_c reads the next line from a text file. If the file is
   not currently open, it is opened with a logical unit determined
   at run time, and the first line of the file is returned.
   Otherwise, the next line not yet read from the file is returned.

   If the end of the file is reached, an empty line is returned,
   the end-of-file indicator is SPICETRUE, and the file is closed.

   Several files may be opened and read simultaneously. Thus,
   you may begin reading from one file before the end of another
   file has been reached. rdtext_c maintains a separate file pointer
   for each file.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) The following code example demonstrates how to read single
      lines from a text file and print them to the screen.

      Use the PCK kernel below as an example of source text file
      whose contents are read into the program.

         de-403-masses.tpc


      Example code begins here.


      /.
         Program rdtext_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"


      int main()
      {
         #define LENOUT 80

         SpiceBoolean eof;
         SpiceChar    line[LENOUT];

         eof = SPICEFALSE;

         while ( !eof )
         {
            rdtext_c ( "de-403-masses.tpc", LENOUT, line, &eof );
            printf ( "%s \n", line );

         }

         return ( 0 );

      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      KPL/PCK

      This file was produced by Kevin Gehringer on June 15, 1995.
      The file was updated by Bill Taber on March 26, 1996 to add
      the value of the AU to the list of items input to the kernel pool.

      Use this at your own risk, for now.

      The masses for the sun and barycenter given in this file are
      derived from the masses used for the integration of the planetary
      ephemerides DE-403. The value of the AU given in this ephemeris is.

         AU = 149597870.693 km

      In the ephemeris the values of the masses are given as ratios to
      of Solar GM to barycenter GM.  These values are given here.

         BODY1_GMSUN/GM  =   6023600.0
         BODY2_GMSUN/GM  =    408523.71
         BODY3_GMSUN/GM  =    332946.048134
         BODY4_GMSUN/GM  =   3098708.0
         BODY5_GMSUN/GM  =      1047.3486
         BODY6_GMSUN/GM  =      3497.898
         BODY7_GMSUN/GM  =     22902.94
         BODY8_GMSUN/GM  =     19412.24
         BODY9_GMSUN/GM  = 135000000.0

      These values are the ones that are used in the integration
      of DE-403 and are used by other products that use DE-403 to
      provide gravitational force models for integration of
      trajectories.


      \begindata

      AU         =      149597870.693

      BODY1_GM   =          22032.080
      BODY2_GM   =         324858.599
      BODY3_GM   =         403503.235
      BODY4_GM   =          42828.314
      BODY5_GM   =      126712767.863
      BODY6_GM   =       37940626.063
      BODY7_GM   =        5794559.128
      BODY8_GM   =        6836534.064
      BODY9_GM   =            983.055
      BODY10_GM  =   132712440023.310

      \begintext

      The masses of bodies other than the earth and moon are
      simply taken to be the masses of the barycenters given above.
      The masses of the earth and moon are taken from DE-403.

      \begindata

      BODY199_GM   =         22032.080
      BODY299_GM   =         324858.599
      BODY301_GM   =           4902.799
      BODY399_GM   =         398600.436
      BODY499_GM   =          42828.314
      BODY599_GM   =      126712767.881
      BODY699_GM   =       37940626.068
      BODY799_GM   =        5794559.128
      BODY899_GM   =        6836534.065
      BODY999_GM   =            983.055


-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   E.D. Wright         (JPL)

-Version

   -CSPICE Version 2.1.0, 04-AUG-2021 (JDR)

       Changed the input argument name "lenout" to "lineln" for consistency
       with other routines.

       Edited the header to comply with NAIF standard. Converted the existing
       code fragments into a complete example and added reference to a text
       kernel as input.

   -CSPICE Version 2.0.0, 07-OCT-1999 (NJB)

       Changed argument list to conform to SPICE convention: LENOUT
       now precedes the output string.

       Added description of lineln to the header.

       Added local logical variable for EOF flag.

   -CSPICE Version 1.0.0, 25-MAY-1999 (EDW)

-Index_Entries

   read a line from a text file

-&
*/

{ /* Begin rdtext_c */

   /*
   Local variables
   */
   logical                 endfil;


   /*
   Participate in error tracing.
   */
   chkin_c ( "rdtext_c" );


   /*
   Check the strings: file, line to insure the pointer is
   non-null and the string length is non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "rdtext_c", file );
   CHKOSTR ( CHK_STANDARD, "rdtext_c", line, lineln );


   /* Call the f2c'd routine. */
   rdtext_ ( ( char    * ) file,
             ( char    * ) line,
             ( logical * ) &endfil,
             ( ftnlen    ) strlen(file),
             ( ftnlen    ) lineln - 1 );

   /*
   Assign the SpiceBoolean EOF flag the logical value obtained
   from the f2c'd routine.
   */

   *eof  =  endfil;


   /* The string, line, is a Fortranish type string. Convert to C. */
   F2C_ConvertStr ( lineln, line );


   /* Checkout. */
   chkout_c ( "rdtext_c" );


} /* End rdtext_c */
