/*

-Procedure dafgda_c ( DAF, read data from address )

-Abstract

   Read the double precision data bounded by two addresses within
   a DAF.

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

   FILES

*/
   #include "SpiceUsr.h"
   #include "SpiceZfc.h"


   void dafgda_c ( SpiceInt       handle,
                   SpiceInt       baddr,
                   SpiceInt       eaddr,
                   SpiceDouble  * data )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   handle     I   Handle of a DAF.
   baddr,
   eaddr      I   Initial, final address within file.
   data       O   Data contained between `baddr' and `eaddr'.

-Detailed_Input

   handle      is the handle of a DAF.

   baddr,
   eaddr       are the initial and final addresses of a contiguous
               set of double precision numbers within a DAF.
               Presumably, these make up all or part of a particular
               array.

               Note that CSPICE DAF addresses begin at 1 as in the
               FORTRAN version of the SPICE Toolkit.

-Detailed_Output

   data        are the double precision data contained between
               the specified addresses within the specified file.

-Parameters

   None.

-Exceptions

   1)  If `baddr' is zero or negative, the error SPICE(DAFNEGADDR)
       is signaled by a routine in the call tree of this routine.

   2)  If baddr > eaddr, the error SPICE(DAFBEGGTEND) is signaled by
       a routine in the call tree of this routine.

   3)  If `handle' is invalid, an error is signaled by a routine in the
       call tree of this routine.

   4)  If the range of addresses covered between `baddr' and `eaddr'
       includes records that do not contain strictly double
       precision data, then the values returned in `data' are
       undefined. See the -Restrictions section below for details.

-Files

   None.

-Particulars

   The principal reason that DAFs are so easy to use is that
   the data in each DAF are considered to be one long contiguous
   set of double precision numbers. You can grab data from anywhere
   within a DAF without knowing (or caring) about the physical
   records in which they are stored.

   This routine replaces dafrda_c as the principal mechanism for
   reading the contents of DAF arrays.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Open a type 8 SPK for read access, retrieve the data for
      the first segment and identify the beginning and end addresses,
      the number of data elements within, the size of the data array,
      and print the first two records.

      Use the SPK kernel below as input type 8 SPK file for the example.

         mer1_ls_040128_iau2000_v1.bsp

      Each segment contains only two records which provide the start
      and end position for the MER-1 rover landing site in the IAU_MARS
      frame. Since the landing site does not change over time, it is
      expected that both records are equal.


      Example code begins here.


      /.
         Program dafgda_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main( )
      {
         /.
         Local constants.
         ./
         #define MAXDAT          1000
         #define MAXSUM          125
         #define ND              2
         #define NI              6

         /.
         Local variables.
         ./
         SpiceBoolean            found;

         SpiceDouble             dafsum [ MAXSUM ];
         SpiceDouble             data   [ MAXDAT ];
         SpiceDouble             dc     [ ND ];

         SpiceInt                baddr;
         SpiceInt                eaddr;
         SpiceInt                handle;
         SpiceInt                ic     [ NI ];

         /.
         Open the type 8 SPK for read access then read the
         data from the first segment.
         ./
         dafopr_c ( "mer1_ls_040128_iau2000_v1.bsp", &handle );

         /.
         Begin a forward search; find the first segment; read the
         segment summary.
         ./
         dafbfs_c ( handle );
         daffna_c ( &found );
         dafgs_c  ( dafsum );
         dafus_c  ( dafsum, ND, NI, dc, ic );

         /.
         Retrieve the data begin and end addresses.
         ./
         baddr = ic[4];
         eaddr = ic[5];

         printf( "Beginning address       : %d\n", baddr             );
         printf( "Ending address          : %d\n", eaddr             );
         printf( "Number of data elements : %d\n", eaddr - baddr + 1 );

         /.
         Extract all data bounded by the begin and end addresses.
         ./
         dafgda_c ( handle, baddr, eaddr, data );

         /.
         Check the data.
         ./
         printf ( "The first and second states stored in the segment:\n" );
         printf ( " %9.3f %9.3f %9.3f %9.3f %9.3f %9.3f\n",
                 data[0], data[1], data[2], data[3], data[4],  data[5]  );
         printf ( " %9.3f %9.3f %9.3f %9.3f %9.3f %9.3f\n",
                 data[6], data[7], data[8], data[9], data[10], data[11] );

         /.
         Safely close the file
         ./
         dafcls_c ( handle );

         return( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Beginning address       : 897
      Ending address          : 912
      Number of data elements : 16
      The first and second states stored in the segment:
        3376.422  -326.649  -115.392     0.000     0.000     0.000
        3376.422  -326.649  -115.392     0.000     0.000     0.000


-Restrictions

   1)  There are several types of records in a DAF. This routine
       is only to be used to read double precision data bounded
       between two DAF addresses. The range of addresses input
       may not cross data and summary record boundaries.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   E.D. Wright         (JPL)

-Version

   -CSPICE Version 1.1.0, 13-AUG-2021 (JDR)

       Changed input argument names "begin" and "end" to "baddr" and
       "eaddr" for consistency with other routines.

       Edited the header comply with NAIF standard. Added complete code
       example based on existing example.

   -CSPICE Version 1.0.1, 23-JAN-2008 (EDW)

       Removed a spurious and unneeded "-Declarations"
       tag. The tag's presence prevented the HTML API doc
       script from parsing the function description.

   -CSPICE Version 1.0.0, 14-SEP-2006 (NJB)

-Index_Entries

   read data from DAF address

-&
*/

{ /* Begin dafgda_c */

   /*
   Participate in error tracing.
   */
   chkin_c ( "dafgda_c" );

   dafgda_ ( ( integer    * ) &handle,
             ( integer    * ) &baddr,
             ( integer    * ) &eaddr,
             ( doublereal * ) data );

   chkout_c ( "dafgda_c" );

} /* End of dafgda_c */
