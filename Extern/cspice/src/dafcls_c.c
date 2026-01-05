/*

-Procedure dafcls_c ( DAF, close )

-Abstract

   Close the DAF associated with a given handle.

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

   void dafcls_c ( SpiceInt handle )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   handle     I   Handle of DAF to be closed.

-Detailed_Input

   handle      is the file handle of a previously opened DAF file.

-Detailed_Output

   None.

-Parameters

   None.

-Exceptions

   1)  If the specified handle is not known to the DAF subsystem
       (because it does not belong to a file opened via the DAF
       API), nothing happens.

   2)  If this routine is used to close a file whose handle is
       known to the DAF subsystem, and if the file handle is
       attached to a non-DAF file, an error is signaled by a routine
       in the call tree of this routine.

-Files

   None.

-Particulars

   Because the DAF subsystem must keep track of what files are open at
   any given time, it is important that DAF files be closed only with
   dafcls_c, to prevent the remaining DAF routines from failing,
   sometimes mysteriously.

   Note that when a file is opened more than once for read access,
   dafopr_c returns the same handle each time it is re-opened.
   Each time the file is closed, dafcls_c checks to see if any other
   claims on the file are still active before physically closing
   the file.

-Examples

   The numerical results shown for these examples may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) In the following code fragment, the arrays in a file are
      examined in order to determine whether the file contains
      any arrays whose names begin with the word TEST.
      The complete names for these arrays are printed to
      the screen. The file is closed at the end of the search.

      #include "SpiceUsr.h"
          .
          .
          .
      dafopr_c ( fname, &handle );
      dafbfs_c ( handle );
      daffna_c ( &found );

      while ( found )
      {
         dafgn_c ( name );

         if (  strncmp( name, "TEST", 4 ) == 0  )
         {
            printf ( "%s\n", name );
         }
         daffna_c ( &found );
      }

      dafcls_c ( handle );


      Note that if the file has been opened already by a DAF routine
      at some other place in the calling program, it remains open.
      This makes it possible to examine files that have been opened
      for use by other modules without interfering with the operation
      of those routines.


   2) Use a simple routine to output the double precision and integer
      values stored in an SPK's segments descriptors. This function
      opens a DAF for read, performs a forwards search for the DAF
      arrays, prints segments description for each array found, then
      closes the DAF.

      Use the SPK kernel below as input DAF file for the program.

         de421.bsp


      Example code begins here.


      /.
         Program dafcls_ex1
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


      Note, the final entries in the integer array contains the
      segment start/end indexes. The output indicates the search
      proceeded from the start of the file (low value index) towards
      the end (high value index).

-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   K.R. Gehringer      (JPL)
   W.L. Taber          (JPL)
   I.M. Underwood      (JPL)
   E.D. Wright         (JPL)

-Version

   -CSPICE Version 1.0.4, 10-AUG-2021 (JDR)

       Edited the header to comply with NAIF standard.

   -CSPICE Version 1.0.3, 28-JUN-2016 (EDW)

       Edit to Example code, SpiceInts output as ints using
       explicit casting.

   -CSPICE Version 1.0.2, 10-OCT-2012 (EDW)

       Added a functional code example to the -Examples section.

       Removed the obsolete Reference citation to "NAIF
       Document 167.0."

   -CSPICE Version 1.0.1, 28-JAN-2004 (NJB)

       Header update: the -Exceptions section now lists the
       case of attempting to close a non-DAF file using this
       routine.

   -CSPICE Version 1.0.0, 01-AUG-1999 (NJB) (KRG) (WLT) (IMU)

-Index_Entries

   close DAF

-&
*/

{ /* Begin dafcls_c */


   /*
   Participate in error tracing.
   */
   chkin_c ( "dafcls_c" );


   dafcls_ ( ( integer * ) &handle  );


   chkout_c ( "dafcls_c" );

} /* End dafcls_c */
