/*

-Procedure dasopr_c ( DAS, open for read )

-Abstract

   Open a DAS file for reading.

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
   #include "SpiceZmc.h"


   void dasopr_c ( ConstSpiceChar  * fname,
                   SpiceInt        * handle )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   fname      I   Name of a DAS file to be opened.
   handle     O   Handle assigned to the opened DAS file.

-Detailed_Input

   fname       is the name of a DAS file to be opened with read
               access.

-Detailed_Output

   handle      is the handle that is  associated with the file. This
               handle is used to identify the file in subsequent
               calls to other DAS routines.

-Parameters

   None.

-Exceptions

   1)  If the input filename is blank, the error SPICE(BLANKFILENAME)
       is signaled by a routine in the call tree of this routine.

   2)  If the specified file does not exist, the error
       SPICE(FILENOTFOUND) is signaled by a routine in the call tree
       of this routine.

   3)  If the specified file has already been opened for read
       access, the handle already associated with the file is
       returned.

   4)  If the specified file has already been opened for write
       access, the error SPICE(DASRWCONFLICT) is signaled by a
       routine in the call tree of this routine.

   5)  If the specified file has already been opened by a non-DAS
       routine, the error SPICE(DASIMPROPOPEN) is signaled by a
       routine in the call tree of this routine.

   6)  If the specified file cannot be opened without exceeding the
       maximum allowed number of open DAS files, the error
       SPICE(DASFTFULL) is signaled by a routine in the call tree of
       this routine.

   7)  If the named file cannot be opened properly, an error is
       signaled by a routine in the call tree of this routine.

   8)  If the file record cannot be read, the error
       SPICE(FILEREADFAILED) is signaled by a routine in the call
       tree of this routine.

   9)  If the specified file is not a DAS file, as indicated by the
       file's ID word, an error is signaled by a routine in the call
       tree of this routine.

   10) If no logical units are available, an error is signaled
       by a routine in the call tree of this routine.

   11) If the `fname' input string pointer is null, the error
       SPICE(NULLPOINTER) is signaled.

   12) If the `fname' input string has zero length, the error
       SPICE(EMPTYSTRING) is signaled.

-Files

   See argument `fname'.

-Particulars

   Most DAS files require only read access. If you do not need to
   change the contents of a file, you should open it using dasopr_c.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Dump several parameters from the first DLA segment of a DSK
      file. Note that DSK files are based on DAS. The segment is
      assumed to be of type 2.

      Example code begins here.


      /.
         Program dasopr_ex1
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
   F.S. Turner         (JPL)
   I.M. Underwood      (JPL)

-Version

   -CSPICE Version 1.0.1, 10-AUG-2021 (JDR)

       Improved the -Exceptions section.

       Edited the header to comply with NAIF standard. Added
       complete code example using example from dskb02_c.

   -CSPICE Version 1.0.0, 05-OCT-2006 (NJB) (KRG) (WLT) (FST) (IMU)

-Index_Entries

   open a DAS file for reading
   open a DAS file for read access

-&
*/

{ /* Begin dasopr_c */


   /*
   Participate in error tracing.
   */
   chkin_c ( "dasopr_c" );

   /*
   Check the input string to make sure the pointer is non-null
   and the string length is non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "dasopr_c", fname );


   /*
   Call the f2c'd Fortran routine.  Use explicit type casts for every
   type defined by f2c.
   */
   dasopr_ ( ( char       * )  fname,
             ( integer    * )  handle,
             ( ftnlen       )  strlen(fname) );


   chkout_c ( "dasopr_c" );

} /* End dasopr_c */
