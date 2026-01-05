/*

-Procedure cknr03_c ( C-kernel, number of records, type 03 )

-Abstract

   Return the number of pointing instances in a CK type 03 segment.
   The segment is identified by a CK file handle and segment
   descriptor.

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

   CK
   DAF

-Keywords

   POINTING

*/
   #include "SpiceUsr.h"
   #include "SpiceZfc.h"

   void cknr03_c ( SpiceInt            handle,
                   ConstSpiceDouble    descr  [],
                   SpiceInt          * nrec      )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   handle     I   The handle of the CK file containing the segment.
   descr      I   The descriptor of the type 3 segment.
   nrec       O   The number of pointing instances in the segment.

-Detailed_Input

   handle      is the handle of the binary CK file containing the
               segment. The file should have been opened for read
               or write access, by cklpf_c, dafopr_c or dafopw_c.

   descr       is the packed descriptor of a data type 3 CK segment.

-Detailed_Output

   nrec        is the number of pointing instances in the type 3
               segment.

-Parameters

   None.

-Exceptions

   1)  If the segment indicated by `descr' is not a type 3 segment, the
       error SPICE(CKWRONGDATATYPE) is signaled by a routine in the call
       tree of this routine.

   2)  If the specified handle does not belong to any DAF file that
       is currently known to be open, an error is signaled by a
       routine in the call tree of this routine.

   3)  If `descr' is not a valid descriptor of a segment in the CK
       file specified by `handle', the results of this routine are
       unpredictable.

-Files

   The CK file specified by `handle' should be open for read or
   write access.

-Particulars

   For a complete description of the internal structure of a type 3
   segment, see the CK required reading.

   This routine returns the number of discrete pointing instances
   contained in the specified segment. It is normally used in
   conjunction with ckgr03_c which returns the Ith pointing instance
   in the segment.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) The following code example extracts the SCLK time, boresight
      vector, and angular velocity vector for each pointing instance
      in the first segment in a CK file that contains segments of
      data type 3.

      Use the CK kernel below, available in the Venus Express PDS
      archives, as input for the code example.

         VEX_BOOM_V01.BC

      Example code begins here.


      /.
         Program cknr03_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Local variables.
         ./
         SpiceDouble          quat   [4];
         SpiceDouble          av     [3];
         SpiceDouble          bore   [3];
         SpiceDouble          cmat   [3][3];
         SpiceDouble          dcd    [2];
         SpiceDouble          descr  [5];
         SpiceDouble          record [8];
         SpiceDouble          sclkdp;

         SpiceInt             i;
         SpiceInt             icd    [6];
         SpiceInt             handle;
         SpiceInt             nrec;

         SpiceBoolean         avseg;
         SpiceBoolean         found;

         /.
         First load the file (it may also be opened by
         using cklpf_c).
         ./
         dafopr_c ( "VEX_BOOM_V01.BC", &handle );

         /.
         Begin forward search.  Find the first array.
         ./
         dafbfs_c ( handle );
         daffna_c ( &found );

         /.
         Get segment descriptor.
         ./
         dafgs_c ( descr );

         /.
         Unpack the segment descriptor into its double precision
         and integer components.
         ./
         dafus_c ( descr, 2, 6, dcd, icd );

         /.
         The data type for a segment is located in the third
         integer component of the descriptor.
         ./
         if ( icd[2] == 3 )
         {

            /.
            Does the segment contain `av' data?
            ./
            avseg =  ( icd[3] == 1 );

            /.
            How many records does this segment contain?
            ./
            cknr03_c ( handle, descr, &nrec );

            for ( i = 1; i <= nrec; i++ )
            {

               /.
               Get the ith pointing instance in the segment.
               ./
               ckgr03_c ( handle, descr, i, record );

               /.
               Unpack `record' into the time, quaternion, and av.
               ./
               sclkdp = record[0];

               moved_c ( record+1, 4, quat );

               if ( avseg )
               {
                  moved_c ( record+5, 3, av );
               }

               /.
               The boresight vector is the third row of the
               C-matrix.
               ./
               q2m_c ( quat, cmat );

               bore[0] = cmat[2][0];
               bore[1] = cmat[2][1];
               bore[2] = cmat[2][2];

               /.
               Write out the results.
               ./
               printf( "Record: %2d\n", (int)i );
               printf( "   SCLK time       : %24.6f\n", sclkdp );
               printf( "   Boresight       : %13.9f %13.9f %13.9f\n",
                                            bore[0], bore[1], bore[2] );

               if ( avseg )
               {
                  printf( "   Angular velocity: %13.9f %13.9f %13.9f\n",
                                                     av[0], av[1], av[2] );
               }
               printf( "\n" );
            }
         }

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Record:  1
         SCLK time       :           2162686.710986
         Boresight       :  -0.999122830   0.000000000   0.041875654
         Angular velocity:   0.000000000   0.000000000   0.000000000

      Record:  2
         SCLK time       :       54160369751.715164
         Boresight       :  -0.999122830   0.000000000   0.041875654
         Angular velocity:   0.000000000   1.176083393   0.000000000

      Record:  3
         SCLK time       :       54160454948.487686
         Boresight       :   0.000000000   0.000000000   1.000000000
         Angular velocity:   0.000000000   0.000000000   0.000000000

      Record:  4
         SCLK time       :      299264885854.937805
         Boresight       :   0.000000000   0.000000000   1.000000000
         Angular velocity:   0.000000000   0.000000000   0.000000000

      Record:  5
         SCLK time       :     2366007685832.532227
         Boresight       :   0.000000000   0.000000000   1.000000000
         Angular velocity:   0.000000000   0.000000000   0.000000000

      Record:  6
         SCLK time       :     4432750485810.126953
         Boresight       :   0.000000000   0.000000000   1.000000000
         Angular velocity:   0.000000000   0.000000000   0.000000000

      Record:  7
         SCLK time       :     6505155594828.757812
         Boresight       :   0.000000000   0.000000000   1.000000000
         Angular velocity:   0.000000000   0.000000000   0.000000000

      Record:  8
         SCLK time       :     8571898394806.352539
         Boresight       :   0.000000000   0.000000000   1.000000000
         Angular velocity:   0.000000000   0.000000000   0.000000000

      Record:  9
         SCLK time       :    10638641194783.947266
         Boresight       :   0.000000000   0.000000000   1.000000000
         Angular velocity:   0.000000000   0.000000000   0.000000000

      Record: 10
         SCLK time       :    12705383994761.541016
         Boresight       :   0.000000000   0.000000000   1.000000000
         Angular velocity:   0.000000000   0.000000000   0.000000000

      Record: 11
         SCLK time       :    14777789103780.169922
         Boresight       :   0.000000000   0.000000000   1.000000000
         Angular velocity:   0.000000000   0.000000000   0.000000000

      Record: 12
         SCLK time       :    16844531903757.763672
         Boresight       :   0.000000000   0.000000000   1.000000000
         Angular velocity:   0.000000000   0.000000000   0.000000000

      Record: 13
         SCLK time       :    18911274703735.359375
         Boresight       :   0.000000000   0.000000000   1.000000000
         Angular velocity:   0.000000000   0.000000000   0.000000000


-Restrictions

   1)  The binary CK file containing the segment whose descriptor was
       passed to this routine must be opened for read or write access
       by cklpf_c, dafopr_c or dafopw_c.

-Literature_References

   None.

-Author_and_Institution

   J. Diaz del Rio     (ODC Space)

-Version

   -CSPICE Version 1.0.0, 01-NOV-2021 (JDR)

-Index_Entries

   number of CK type_3 records

-&
*/

{ /* Begin cknr03_c */

   /*
   Participate in error tracing.
   */
   chkin_c ( "cknr03_c" );

   /*
   Call the f2c'd Fortran routine.
   */
   cknr03_ (  ( integer    * ) &handle,
              ( doublereal * )  descr,
              ( integer    * )  nrec   );

   chkout_c ( "cknr03_c" );

} /* End cknr03_c */
