/*

-Procedure exists_c ( Does the file exist? )

-Abstract

   Determine whether a file exists.

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

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZmc.h"

   SpiceBoolean exists_c ( ConstSpiceChar  * fname )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   fname      I   Name of the file in question.

   The function returns the value SPICETRUE if the file exists,
   SPICEFALSE otherwise.

-Detailed_Input

   fname       is the name of the file in question.This may be
               any unambiguous file name valid on the user's
               computer, for example

                  '/usr/dir1/dir2/DATA.DAT'
                  './DATA.DAT'
                  'c:\usr\dir1\dir2\data.dat'

               Environment or shell variables may not be used.

-Detailed_Output

   The function returns the value SPICETRUE if the file exists,
   SPICEFALSE otherwise.

-Parameters

   None.

-Exceptions

   1)  If the filename is blank, the error SPICE(BLANKFILENAME) is
       signaled by a routine in the call tree of this routine.

   2)  If an I/O error occurs while checking the existence of the
       indicated file, the error SPICE(INQUIREFAILED) is signaled by
       a routine in the call tree of this routine.

   3)  If the `fname' input string pointer is null, the error
       SPICE(NULLPOINTER) is signaled. The function returns the value
       SPICEFALSE.

   4)  If the `fname' input string has zero length, the error
       SPICE(EMPTYSTRING) is signaled. The function returns the value
       SPICEFALSE.

-Files

   None.

-Particulars

   Uses the f2c I/O libraries to implement the existence test.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Given two arbitrary files (one of them the actual code example
      source file), determine if they exists.

      Example code begins here.


      /.
         Program exists_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Local constants.
         ./
         #define FILEN        15

         /.
         Local variables.
         ./

         SpiceInt             i;

         /.
         Define an array of file names.
         ./
         SpiceChar            fname  [2][FILEN] = { "exists_ex1.txt",
                                                    "exists_ex1.prg" };

         for ( i = 0; i < 2; i++ )
         {

            if ( exists_c ( fname[i] ) )
            {

               printf( "The file %s exists.\n", fname[i] );

            }
            else
            {

               printf( "Cannot find the file %s\n", fname[i] );

            }

         }

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Cannot find the file exists_ex1.txt
      The file exists_ex1.prg exists.


-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   K.R. Gehringer      (JPL)

-Version

   -CSPICE Version 1.1.2, 13-AUG-2021 (JDR)

       Edited the header to comply with NAIF standard.
       Added complete code example.

       Added entries #3 and #4 to -Exceptions section.

   -CSPICE Version 1.1.1, 01-JUL-2014 (NJB)

       VAX examples were deleted from the header.

   -CSPICE Version 1.1.0, 08-FEB-1998 (NJB)

       References to C2F_CreateStr_Sig were removed; code was
       cleaned up accordingly. String checks are now done using
       the macro CHKFSTR_VAL.

   -CSPICE Version 1.0.0, 25-OCT-1997 (NJB) (KRG)

       Based on SPICELIB Version 2.1.0, 04-MAR-1996 (KRG)

-Index_Entries

   does the file exist

-&
*/

{ /* Begin exists_c */


   /*
   Local variables
   */
   SpiceBoolean            fileExists;


   /*
   Participate in error tracing.
   */
   chkin_c ( "exists_c");


   /*
   Check the input string to make sure the pointer
   is non-null and the string length is non-zero.
   */
   CHKFSTR_VAL ( CHK_STANDARD, "exists_c", fname, SPICEFALSE );


   /*
   Do the existence test.
   */
   fileExists = (SpiceBoolean) exists_( ( char * ) fname,
                                        ( ftnlen ) strlen(fname) );

   chkout_c ( "exists_c" );
   return   ( fileExists );


} /* End exists_c */
