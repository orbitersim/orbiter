/*

-Procedure dascls_c ( DAS, close file )

-Abstract

   Close a DAS file.

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

   DAS

-Keywords

   DAS
   FILES

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZst.h"

   void dascls_c ( SpiceInt handle )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   handle     I   Handle of an open DAS file.
   FTSIZE     P   Maximum number of simultaneously open DAS files.

-Detailed_Input

   handle      is the file handle of an open DAS file.

-Detailed_Output

   None.

   See -Particulars for a description of the effect of this routine.

-Parameters

   All parameters described here are declared in the CSPICE
   header file SpiceDAS.h. See that file for parameter values.

   SPICE_DAS_FTSIZE

            is the maximum number of DAS files that can be
            open at any one time.

-Exceptions

   Error free.

   1)  If `handle' is not the handle of an open DAS file, no error
       is signaled.

-Files

   See the description of input argument `handle' in -Detailed_Input.

-Particulars

   This routine provides the primary recommended method of closing an
   open DAS file. It is also possible to close a DAS file without
   segregating it by calling daswbr_ and dasllc_. Closing a DAS file by
   any other means may cause the DAS mechanism for keeping track of
   which files are open to fail. Closing a DAS file that has been
   opened for writing by any other means may result in the production
   of something other than a DAS file.

-Examples

   The numerical results shown for these examples may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Open a new DAS file, add 100 double precision numbers
      to it, and then close the file.

      Example code begins here.


      /.
         Program dascls_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main()
      {
         #define  NMAX           100

         SpiceChar             * fname;
         SpiceChar             * ftype;
         SpiceChar             * ifname;

         SpiceDouble             ddata  [ NMAX ];

         SpiceInt                handle;
         SpiceInt                i;
         SpiceInt                n;
         SpiceInt                ncomch;


         /.
         We'll give the file the same internal file name
         as the file's actual name.  We don't require any
         comment records.
         ./
         fname  = "dascls_ex1.das";
         ftype  = "TEST";
         ifname = fname;
         ncomch = 0;

         printf ( "Opening the DAS file for writing...\n" );
         dasonw_c ( fname, ftype, fname, ncomch, &handle );


         for ( i = 0;  i < NMAX;  i++ )
         {
             ddata[i] = (SpiceDouble)i;
         }

         n = NMAX;

         printf ( "Adding the double precision numbers...\n" );
         dasadd_c ( handle, n, ddata );

         printf ( "Closing the DAS file...\n" );
         dascls_c ( handle );

         printf( "All ok.\n" );

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Opening the DAS file for writing...
      Adding the double precision numbers...
      Closing the DAS file...
      All ok.


      Note that after run completion, a new DAS file exists in
      the output directory.

   2) Dump several parameters from the first DLA segment of a DSK
      file. Note that DSK files are based on DAS. The segment is
      assumed to be of type 2.


      Example code begins here.


      /.
         Program dascls_ex2
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main()
      {
         /.
         Constants
         ./
         #define FILSIZ          256

         /.
         Local variables
         ./
         SpiceBoolean            found;

         SpiceChar               dsk  [ FILSIZ ];

         SpiceDLADescr           dladsc;

         SpiceDouble             voxori [3];
         SpiceDouble             voxsiz;
         SpiceDouble             vtxbds [3][2];

         SpiceInt                cgscal;
         SpiceInt                handle;
         SpiceInt                np;
         SpiceInt                nv;
         SpiceInt                nvxtot;
         SpiceInt                vgrext [3];
         SpiceInt                voxnpl;
         SpiceInt                voxnpt;
         SpiceInt                vtxnpl;

         /.
         Prompt for the name of DSK to read.
         ./
         prompt_c ( "Enter DSK name > ", FILSIZ, dsk );

         /.
         Open the DSK file for read access. We use the
         DAS-level interface for this function.
         ./
         dasopr_c ( dsk, &handle );

         /.
         Begin a forward search through the kernel. In
         this example, it's a very short search.
         ./
         dlabfs_c ( handle, &dladsc, &found );

         if ( !found )
         {
            setmsg_c ( "No segment found in file #." );
            errch_c  ( "#",  dsk                     );
            sigerr_c ( "SPICE(NOSEGMENT)"            );
         }

         /.
         If we made it this far, DLADSC is the
         DLA descriptor of the first segment.
         Read and display type 2 bookkeeping data.
         ./
         dskb02_c ( handle,  &dladsc, &nv,    &np,    &nvxtot,
                    vtxbds,  &voxsiz, voxori, vgrext, &cgscal,
                    &vtxnpl, &voxnpt, &voxnpl                 );

         printf ( "\n"
                  "Number of vertices:                 %d\n"
                  "Number of plates:                   %d\n"
                  "Number of voxels:                   %d\n",
                  (int)nv,
                  (int)np,
                  (int)nvxtot                                       );

         printf ( "Vertex bounds in X direction (km):  %f : %f\n"
                  "Vertex bounds in Y direction (km):  %f : %f\n"
                  "Vertex bounds in Z direction (km):  %f : %f\n",
                  vtxbds[0][0], vtxbds[0][1],
                  vtxbds[1][0], vtxbds[1][1],
                  vtxbds[2][0], vtxbds[2][1]                        );

         printf ( "Voxel edge length (km):             %f\n"
                  "Voxel grid origin (km):           ( %f %f %f )\n"
                  "Voxel grid extents:                 %d %d %d\n"
                  "Coarse voxel grid scale:            %d\n"
                  "Size of vertex-plate list:          %d\n"
                  "Size of voxel-plate pointer array:  %d\n"
                  "Size of voxel-plate list:           %d\n",
                  voxsiz,
                  voxori[0], voxori[1], voxori[2],
                  (int)vgrext[0], (int)vgrext[1], (int)vgrext[2],
                  (int)cgscal,
                  (int)vtxnpl,
                  (int)voxnpt,
                  (int)voxnpl                                       );

         /.
         Close the kernel. This isn't necessary in a stand-
         alone program, but it's good practice in subroutines
         because it frees program and system resources.
         ./
         dascls_c ( handle );

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, using the DSK file named phobos512.bds,
      the output was:


      Enter DSK name > phobos512.bds

      Number of vertices:                 1579014
      Number of plates:                   3145728
      Number of voxels:                   11914500
      Vertex bounds in X direction (km):  -13.440030 : 12.762800
      Vertex bounds in Y direction (km):  -11.520650 : 12.061140
      Vertex bounds in Z direction (km):  -9.570780 : 10.055000
      Voxel edge length (km):             0.104248
      Voxel grid origin (km):           ( -14.073520 -11.988554 -9.903588 )
      Voxel grid extents:                 260 235 195
      Coarse voxel grid scale:            5
      Size of vertex-plate list:          11010050
      Size of voxel-plate pointer array:  1151500
      Size of voxel-plate list:           6419540


-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   K.R. Gehringer      (JPL)
   W.L. Taber          (JPL)

-Version

   -CSPICE Version 1.0.1, 10-AUG-2021 (JDR)

       Edited the header to comply with NAIF standard.

       Updated code example #1 to produce logging information and replace
       f2c'ed by CSPICE APIs. Added code example #2 using example from
       dskb02_c.

   -CSPICE Version 1.0.0, 05-OCT-2006 (NJB) (KRG) (WLT)

-Index_Entries

   close an open DAS file

-&
*/

{ /* Begin dascls_c */



   /*
   Participate in error tracing.
   */

   chkin_c ( "dascls_c" );

   /*
   Call the f2c'd Fortran routine.  Use explicit type casts for every
   type defined by f2c.
   */
   dascls_ (  ( integer * ) &handle );


   chkout_c ( "dascls_c" );

} /* End dascls_c */
