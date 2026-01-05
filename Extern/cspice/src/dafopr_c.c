/*

-Procedure dafopr_c ( DAF, open for read )

-Abstract

   Open a DAF for subsequent read requests.

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

   DAF

-Keywords

   DAF
   FILES

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZmc.h"


   void dafopr_c ( ConstSpiceChar    * fname,
                   SpiceInt          * handle  )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   fname      I   Name of DAF to be opened.
   handle     O   Handle assigned to DAF.

-Detailed_Input

   fname       is the file name of a DAF to be opened for read
               access.

-Detailed_Output

   handle      is the file handle associated with the file. This
               handle is used to identify the file in subsequent
               calls to other DAF routines.

-Parameters

   None.

-Exceptions

   1)  If the specified file has already been opened for read
       access, the handle already associated with the file is
       returned.

   2)  If the specified file has already been opened for write
       access, an error is signaled by a routine in the call
       tree of this routine.

   3)  If the specified file has already been opened by a non-DAF
       routine, an error is signaled by a routine in the call
       tree of this routine.

   4)  If the specified file cannot be opened without exceeding
       the maximum number of files, the error SPICE(DAFFTFULL)
       is signaled by a routine in the call tree of this routine.

   5)  If the attempt to read the file's file record fails, the error
       SPICE(FILEREADFAILED) is signaled by a routine in the call
       tree of this routine.

   6)  If the specified file is not a DAF file, an error is
       signaled by a routine in the call tree of this routine.

   7)  If no logical units are available, an error is
       signaled by a routine in the call tree of this routine.

   8)  If the file does not exist, an error is signaled by a routine
       in the call tree of this routine.

   9)  If an I/O error occurs in the process of opening the file,
       the error is signaled by a routine in the call tree of this
       routine.

   10) If the file name is blank or otherwise inappropriate,
       an error is signaled by a routine in the call tree of this
       routine.

   11) If the file was transferred improperly via FTP, an error is
       signaled by a routine in the call tree of this routine.

   12) If the file utilizes a binary file format that is not
       currently supported on this platform, an error is signaled by
       a routine in the call tree of this routine.

   13) If the `fname' input string pointer is null, the error
       SPICE(NULLPOINTER) is signaled.

   14) If the `fname' input string has zero length, the error
       SPICE(EMPTYSTRING) is signaled.

-Files

   See argument `fname'.

-Particulars

   Most DAFs require only read access. If you do not need to
   change the contents of a file, you should open it with dafopr_c.

-Examples

   The numerical results shown for these examples may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) In the following code fragment, dafopr_c is used to open a file,
      which is then searched for DAFs containing data for a
      particular object.

      #include "SpiceUsr.h"
           .
           .
           .
      dafopr_c ( fname, &handle );
      dafbfs_c ( handle );

      daffna_c ( &found );

      while ( found )
      {
         dafgs_c ( sum );
         dafus_c ( sum, ND, NI, dc, ic );

         if ( ic[0] == target_object )
         {
            .
            .
            .
         }

         daffna_c ( &found );
      }

   2) Use a simple routine to output the double precision and integer
      values stored in an SPK's segments descriptors. This function
      opens a DAF for read, performs a forwards search for the DAF
      arrays, prints segments description for each array found, then
      closes the DAF.

      Use the SPK kernel below as input DAF file for the program.

         de421.bsp


      Example code begins here.


      /.
         Program dafopr_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main()
         {

         /.
         Local constants
         ./

         /.
         Define the summary parameters appropriate
         for an SPK file.
         ./

         #define ND              2
         #define NI              6
         #define MAXSUM          125

         SpiceInt                ic  [ NI ];
         SpiceInt                handle;

         SpiceDouble             dc  [ ND ];
         SpiceDouble             sum [ MAXSUM ];

         SpiceChar             * kernel = "de421.bsp";

         SpiceBoolean            found;


         /.
         Open a DAF for read. Return a `handle' referring to the file.
         ./
         dafopr_c ( kernel, &handle );

         /.
         Begin a forward search on the file.
         ./
         dafbfs_c ( handle );

         /.
         Search until a DAF array is found.
         ./
         daffna_c ( &found );

         /.
         Loop while the search finds subsequent DAF arrays.
         ./
         while ( found )
            {

            dafgs_c ( sum );
            dafus_c ( sum, ND, NI, dc, ic );

            printf( " Doubles: %f %f \n", dc[0], dc[1] );
            printf( "Integers: %d %d %d %d %d %d\n\n",
                       (int)ic[0], (int)ic[1], (int)ic[2],
                       (int)ic[3], (int)ic[4], (int)ic[5] );


            /.
            Check for another segment.
            ./
            daffna_c ( &found );
            }

         /.
         Safely close the DAF.
         ./
         dafcls_c ( handle  );

         return ( 0 );
         }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


       Doubles: -3169195200.000000 1696852800.000000
      Integers: 1 0 1 2 641 310404

       Doubles: -3169195200.000000 1696852800.000000
      Integers: 2 0 1 2 310405 423048

       Doubles: -3169195200.000000 1696852800.000000
      Integers: 3 0 1 2 423049 567372

       Doubles: -3169195200.000000 1696852800.000000
      Integers: 4 0 1 2 567373 628976

       Doubles: -3169195200.000000 1696852800.000000
      Integers: 5 0 1 2 628977 674740

       Doubles: -3169195200.000000 1696852800.000000
      Integers: 6 0 1 2 674741 715224

       Doubles: -3169195200.000000 1696852800.000000
      Integers: 7 0 1 2 715225 750428

       Doubles: -3169195200.000000 1696852800.000000
      Integers: 8 0 1 2 750429 785632

       Doubles: -3169195200.000000 1696852800.000000
      Integers: 9 0 1 2 785633 820836

       Doubles: -3169195200.000000 1696852800.000000
      Integers: 10 0 1 2 820837 944040

       Doubles: -3169195200.000000 1696852800.000000
      Integers: 301 3 1 2 944041 1521324

       Doubles: -3169195200.000000 1696852800.000000
      Integers: 399 3 1 2 1521325 2098608

       Doubles: -3169195200.000000 1696852800.000000
      Integers: 199 1 1 2 2098609 2098620

       Doubles: -3169195200.000000 1696852800.000000
      Integers: 299 2 1 2 2098621 2098632

       Doubles: -3169195200.000000 1696852800.000000
      Integers: 499 4 1 2 2098633 2098644


      Note, the final entries in the integer array contain the
      segment start/end indexes. The output indicates the search
      proceeded from the start of the file (low value index) towards
      the end (high value index).

-Restrictions

   1)  Files opened using this routine must be closed with dafcls_c.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   K.R. Gehringer      (JPL)
   J.M. Lynch          (JPL)
   W.L. Taber          (JPL)
   I.M. Underwood      (JPL)
   E.D. Wright         (JPL)

-Version

   -CSPICE Version 1.0.3, 10-AUG-2021 (JDR)

       Edited the header to comply with NAIF standard. Added -Restrictions
       section.

       Improved -Exceptions sections.

   -CSPICE Version 1.0.2, 28-JUN-2016 (EDW)

       Edit to Example code, SpiceInts output as ints using
       explicit casting.

   -CSPICE Version 1.0.1, 10-OCT-2012 (EDW)

       Added a functional code example to the -Examples section.

       Removed the obsolete Reference citation to "NAIF
       Document 167.0."

   -CSPICE Version 1.0.0, 01-AUG-1999 (NJB) (KRG) (JML) (WLT) (IMU)

-Index_Entries

   open DAF for read

-&
*/

{ /* Begin dafopr_c */

   /*
   Participate in error tracing.
   */
   chkin_c ( "dafopr_c" );

   /*
   Check the file name to make sure the pointer is non-null
   and the string length is non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "dafopr_c", fname );


   dafopr_ ( ( char    * ) fname,
             ( integer * ) handle,
             ( ftnlen    ) strlen(fname) );


   chkout_c ( "dafopr_c" );

} /* End dafopr_c */
