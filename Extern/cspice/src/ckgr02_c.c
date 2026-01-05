/*

-Procedure ckgr02_c ( C-kernel, get record, type 02 )

-Abstract

   Return a specified pointing instance from a CK type 02 segment.
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

   void ckgr02_c ( SpiceInt            handle,
                   ConstSpiceDouble    descr  [],
                   SpiceInt            recno,
                   SpiceDouble         record [] )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   handle     I   The handle of the CK file containing the segment.
   descr      I   The segment descriptor.
   recno      I   The number of the pointing record to be returned.
   record     O   The pointing record.

-Detailed_Input

   handle      is the handle of the binary CK file containing the
               desired segment. The file should have been opened
               for read or write access, by cklpf_c, dafopr_c or dafopw_c.

   descr       is the packed descriptor of the data type 2 CK segment.

   recno       is the number of the individual pointing record to be
               returned from the data type 2 segment.

-Detailed_Output

   record      is the pointing record indexed by `recno' in the segment.
               The contents are as follows:

                  record[ 0 ] = start SCLK time of interval
                  record[ 1 ] = end SCLK time of interval
                  record[ 2 ] = TDB seconds per tick rate

                  record[ 3 ] = q0
                  record[ 4 ] = q1
                  record[ 5 ] = q2
                  record[ 6 ] = q3

                  record[ 7 ] = av0
                  record[ 8 ] = av1
                  record[ 9 ] = av2


               See the section on data type 2 in the CK Required
               Reading for a complete description on how pointing
               is obtained from a type 2 record.

               Note that the `record' returned by this routine is slightly
               different from that returned by the SPICELIB routine
               CKR02. The second element of the record returned by that
               routine contains the SCLK time at which pointing was
               requested, whereas this routine returns the SCLK time of
               the right endpoint of the interval for which the constant
               angular velocity model is valid.

-Parameters

   None.

-Exceptions

   1)  If the segment is not of data type 2, the error
       SPICE(CKWRONGDATATYPE) is signaled by a routine in the call tree of
       this routine.

   2)  If `recno' is less than one or greater than the number of records in
       the specified segment, the error SPICE(CKNONEXISTREC) is signaled by
       a routine in the call tree of this routine.

   3)  If the specified handle does not belong to any file that is
       currently known to be open, an error is signaled by a routine
       in the call tree of this routine.

   4)  If `descr' is not a valid descriptor of a segment in the CK
       file specified by `handle', the results of this routine are
       unpredictable.

-Files

   The CK file specified by `handle' should be open for read or write
   access.

-Particulars

   For a detailed description of the structure of a type 2 segment,
   see the CK Required Reading.

   This is a utility routine that may be used to read the individual
   pointing records that make up a data type 2 segment. It is
   normally used in combination with cknr02_c, which gives the number
   of pointing instances stored in a segment.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) The following code example extracts the start and end SCLK
      time, seconds per tick rate, platform's +Z axis direction,
      and angular velocity vector for each pointing instance in
      the first segment in a CK file that contains segments of data
      type 2.

      Use the CK kernel below, available in the Viking Orbiter PDS
      archives, as input for the code example.

         vo2_swu_ck2.bc

      Example code begins here.


      /.
         Program ckgr02_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main( )
      {

         SpiceDouble          av     [3];
         SpiceDouble          cmat   [3][3];
         SpiceDouble          dcd    [2];
         SpiceDouble          descr  [5];
         SpiceDouble          quat   [4];
         SpiceDouble          record [10];
         SpiceDouble          sclke;
         SpiceDouble          sclkr;
         SpiceDouble          sclks;
         SpiceDouble          z      [3];

         SpiceInt             icd    [6];
         SpiceInt             handle;
         SpiceInt             nrec;
         SpiceInt             i;

         SpiceBoolean         found;

         /.
         First load the file. (The file may also be opened by
         using cklpf_c).
         ./
         dafopr_c ( "vo2_swu_ck2.bc", &handle );

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
         if ( icd[2] == 2 )
         {

            /.
            How many records does this segment contain?
            ./
            cknr02_c ( handle, descr, &nrec );

            for ( i = 1; i <= nrec; i++ )
            {

               /.
               Get the ith record in the segment.
               ./
               ckgr02_c ( handle, descr, i, record );

               /.
               Unpack `record' into the start and end time, rate in
               TDB seconds/tick, quaternion, and av.
               ./
               sclks = record[0];
               sclke = record[1];
               sclkr = record[2];

               moved_c ( record+3, 4, quat );
               moved_c ( record+7, 3, av );

               /.
               The +Z axis direction is the third row of the
               C-matrix.
               ./
               q2m_c ( quat, cmat );

               z[0] = cmat[2][0];
               z[1] = cmat[2][1];
               z[2] = cmat[2][2];

               /.
               Write out the results.
               ./
               printf( "Record: %2d\n", (int)i );
               printf( "   Start encoded SCLK: %20.6f\n", sclks );
               printf( "   End encoded SCLK  : %20.6f\n", sclke );
               printf( "   TDB Seconds/tick  : %20.6f\n", sclkr );
               printf( "   +Z axis           : %12.8f %12.8f %12.8f\n",
                                                      z[0], z[1], z[2] );
               printf( "   Angular velocity  : %12.8f %12.8f %12.8f\n",
                                                   av[0], av[1], av[2] );
               printf( "\n" );
            }
         }

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Record:  1
         Start encoded SCLK:   32380393707.000015
         End encoded SCLK  :   32380395707.000015
         TDB Seconds/tick  :             0.001000
         +Z axis           :   0.62277152  -0.29420673   0.72498141
         Angular velocity  :   0.00000000   0.00000000   0.00000000

      Record:  2
         Start encoded SCLK:   32380402605.999947
         End encoded SCLK  :   32380404605.999947
         TDB Seconds/tick  :             0.001000
         +Z axis           :   0.62172600  -0.27894910   0.73187716
         Angular velocity  :   0.00000000   0.00000000   0.00000000

      Record:  3
         Start encoded SCLK:   32380412542.000053
         End encoded SCLK  :   32380414542.000053
         TDB Seconds/tick  :             0.001000
         +Z axis           :   0.62183610  -0.26307233   0.73764003
         Angular velocity  :   0.00000000   0.00000000   0.00000000

      Record:  4
         Start encoded SCLK:   32827264875.000000
         End encoded SCLK  :   32827266875.000000
         TDB Seconds/tick  :             0.001000
         +Z axis           :   0.82984105  -0.44796078   0.33270853
         Angular velocity  :   0.00000000   0.00000000   0.00000000

      Record:  5
         Start encoded SCLK:   32827403805.999992
         End encoded SCLK  :   32827405805.999992
         TDB Seconds/tick  :             0.001000
         +Z axis           :   0.80812500  -0.44911178   0.38109395
         Angular velocity  :   0.00000000   0.00000000   0.00000000

      Record:  6
         Start encoded SCLK:   32827412705.000042
         End encoded SCLK  :   32827414705.000042
         TDB Seconds/tick  :             0.001000
         +Z axis           :   0.81120505  -0.43593555   0.38975193
         Angular velocity  :   0.00000000   0.00000000   0.00000000

      Record:  7
         Start encoded SCLK:   32827417284.000038
         End encoded SCLK  :   32827419284.000038
         TDB Seconds/tick  :             0.001000
         +Z axis           :   0.81313834  -0.42861613   0.39382008
         Angular velocity  :   0.00000000   0.00000000   0.00000000

      Record:  8
         Start encoded SCLK:   33793314593.000053
         End encoded SCLK  :   33793316593.000053
         TDB Seconds/tick  :             0.001000
         +Z axis           :   0.79860617  -0.37840751   0.46801275
         Angular velocity  :   0.00000000   0.00000000   0.00000000

      Record:  9
         Start encoded SCLK:   33793332478.000046
         End encoded SCLK  :   33793334478.000046
         TDB Seconds/tick  :             0.001000
         +Z axis           :   0.77861783  -0.39171670   0.49021658
         Angular velocity  :   0.00000000   0.00000000   0.00000000

      Record: 10
         Start encoded SCLK:   33793341463.000061
         End encoded SCLK  :   33793343463.000061
         TDB Seconds/tick  :             0.001000
         +Z axis           :   0.76852381  -0.39797437   0.50098659
         Angular velocity  :   0.00000000   0.00000000   0.00000000

      Record: 11
         Start encoded SCLK:   33793350363.000034
         End encoded SCLK  :   33793352363.000034
         TDB Seconds/tick  :             0.001000
         +Z axis           :   0.75779934  -0.40484364   0.51170478
         Angular velocity  :   0.00000000   0.00000000   0.00000000

      Record: 12
         Start encoded SCLK:   33984028250.000000
         End encoded SCLK  :   33984030250.000000
         TDB Seconds/tick  :             0.001000
         +Z axis           :   0.77099184  -0.39926339   0.49614546
         Angular velocity  :   0.00000000   0.00000000   0.00000000

      Record: 13
         Start encoded SCLK:   33984046134.999992
         End encoded SCLK  :   33984048134.999992
         TDB Seconds/tick  :             0.001000
         +Z axis           :   0.75024440  -0.41218993   0.51694564
         Angular velocity  :   0.00000000   0.00000000   0.00000000

      Record: 14
         Start encoded SCLK:   33984055121.000053
         End encoded SCLK  :   33984057121.000053
         TDB Seconds/tick  :             0.001000
         +Z axis           :   0.73947359  -0.41886863   0.52699894
         Angular velocity  :   0.00000000   0.00000000   0.00000000

      Record: 15
         Start encoded SCLK:   33984220835.999966

      [...]


      Warning: incomplete output. Only 100 out of 875 lines have been
      provided.


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

   get CK type_2 record

-&
*/

{ /* Begin ckgr02_c */

   /*
   Participate in error tracing.
   */
   chkin_c ( "ckgr02_c" );

   /*
   Call the f2c'd Fortran routine.
   */
   ckgr02_ (  ( integer    * ) &handle,
              ( doublereal * )  descr,
              ( integer    * ) &recno,
              ( doublereal * )  record  );

   chkout_c ( "ckgr02_c" );

} /* End ckgr02_c */
