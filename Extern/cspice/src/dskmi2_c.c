/*

-Procedure dskmi2_c ( DSK, make spatial index for type 2 segment )

-Abstract

   Make spatial index for a DSK type 2 segment. The index is returned
   as a pair of arrays, one of type SpiceInt and one of type
   SpiceDouble. These arrays are suitable for use with the DSK type 2
   writer dskw02_c.

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
   DSK

-Keywords

   DAS
   DSK
   FILES
   PLATE
   TOPOGRAPHY

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #undef dskmi2_c


   void dskmi2_c  ( SpiceInt            nv,
                    ConstSpiceDouble    vrtces[][3],
                    SpiceInt            np,
                    ConstSpiceInt       plates[][3],
                    SpiceDouble         finscl,
                    SpiceInt            corscl,
                    SpiceInt            worksz,
                    SpiceInt            voxpsz,
                    SpiceInt            voxlsz,
                    SpiceBoolean        makvtl,
                    SpiceInt            spxisz,
                    SpiceInt            work   [][2],
                    SpiceDouble         spaixd [],
                    SpiceInt            spaixi []    )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   nv         I   Number of vertices.
   vrtces     I   Vertices.
   np         I   Number of plates.
   plates     I   Plates.
   finscl     I   Fine voxel scale.
   corscl     I   Coarse voxel scale.
   worksz     I   Workspace size.
   voxpsz     I   Voxel-plate pointer array size.
   voxlsz     I   Voxel-plate list array size.
   makvtl     I   Vertex-plate list flag.
   spxisz     I   Spatial index integer component size.
   work      I-O  Workspace.
   spaixd     O   Double precision component of spatial index.
   spaixi     O   Integer component of spatial index.

-Detailed_Input

   nv          is the number of vertices belonging to the input
               set of plates.

   vrtces      is an array of coordinates of the vertices. Vertex
               indices, also called "vertex numbers," are 1-based in
               all languages supported by SPICE, and range from 1 to
               `nv'. The ith vertex occupies elements [i-1][0:2] of
               this array.

   np          is the number of plates in the input plate set.

   plates      is an array representing the triangular plates of a
               shape model. The elements of `plates' are vertex
               indices; vertex indices, as well as plate indices, are
               1-based in all languages supported by SPICE. The vertex
               indices of the ith plate occupy elements [i-1][0:2] of
               this array.

   finscl      is the fine voxel scale. This scale determines the
               edge length of the cubical voxels comprising the fine
               voxel grid: the edge length `voxsiz' is approximately

                   finscl * {average plate extent}

               where the extents of a plate are the respective
               differences between the maximum and minimum
               coordinate values of the plate's vertices.

               The relationship between `voxsiz' and the average plate
               extent is approximate because the `voxsiz' is adjusted
               so that each dimension of the fine voxel grid is an
               integer multiple of the coarse voxel scale.

               See the -Particulars section below for further
               information on voxel scales.

   corscl      is the coarse voxel scale. This integer scale is the
               ratio of the edge length of coarse voxels to that of
               fine voxels. The coarse scale must be large enough so
               that the total number of coarse voxels does not exceed
               SPICE_DSK02_MAXCGR (see the -Parameters section below).

   worksz      is the second dimension of the workspace array `work'.
               `worksz' must be at least as large as the greater of

                  - the number of fine voxel-plate associations

                    This number is equal to

                       np * {average number of fine voxels
                             intersected by each plate}

                  - the number of vertex-plate associations, if
                    the vertex-plate mapping is constructed.

                    This number is equal to

                       nv + ( 3 * np )

   voxpsz      is the size of the fine voxel-plate pointer array.
               This array maps fine voxels to lists of plates that
               intersect those voxels. `voxpsz' must be at least as
               large as

                        3
                  corscl  * {number of non-empty coarse voxels}

   voxlsz      is the size of the fine voxel-plate list array. This
               array contains, for each non-empty fine voxel, the
               count of plates that intersect that voxel and the
               IDs of those plates. `voxlsz' must be at least as large
               as

                       `np' * {average number of fine voxels
                             intersected by each plate}

                   +   {number of non-empty fine voxels}

   makvtl      is a logical flag that, when set to SPICETRUE, indicates
               that a  vertex-plate association list is to be
               constructed.

               The amount of workspace that is needed may depend on
               whether a vertex-plate association list is
               constructed. When this list is constructed, the size
               of the integer component of the spatial index is
               increased by the size of the list and the size of a
               vertex-plate pointer array; the total of these sizes
               is

                  ( 2 * nv ) + ( 3 * np )

   spxisz      is the declared size of the output array `spaixi'. This
               size must be at least as large as the sum of

                  - the fixed-size part of the integer component of
                    the index, which includes the coarse voxel grid;
                    this value is

                       SPICE_DSK02_IXIFIX

                  - the size `voxpsz' of the voxel-plate pointer array

                  - the size `voxlsz' of the voxel-plate association
                    list

               plus, if the vertex-plate association list is
               constructed,

                  - the size `nv' of the vertex-plate pointer array

                  - the size of the vertex-plate association list;
                    this size is

                       nv + ( 3 * np )

   work        is the workspace array. The array should be declared
               with dimensions

                  (2, worksz)

               See the description of `worksz' above.

-Detailed_Output

   work        is the workspace array, modified by the operations
               performed by this routine.

   spaixd,
   spaixi      are, respectively, the double precision and integer
               components of the spatial index of the segment.

               `spaixd' must be declared with size at least
               SPICE_DSK02_IXDFIX.

               `spaixi' must be declared with size at least `spxisz'.

-Parameters

   See the header file

      SpiceDSK.h

   for declarations of DSK data type 2 (plate model) parameters.

   See the header file

      SpiceDLA.h

   for declarations of DLA descriptor sizes and documentation of the
   contents of DLA descriptors.

   See the header file

      SpiceDSK.h

   for declarations of DSK descriptor sizes and documentation of the
   contents of DSK descriptors.

-Exceptions

   1)  If the fine voxel scale is non-positive, the error
       SPICE(BADFINEVOXELSCALE) is signaled by a routine in the call tree of
       this routine.

   2)  If the coarse voxel scale is less than 1, the error
       SPICE(BADCOARSEVOXSCALE) is signaled by a routine in the call tree of
       this routine.

   3)  If `nv' is less than 3 or greater than SPICE_DSK02_MAXVRT, the error
       SPICE(BADVERTEXCOUNT) is signaled by a routine in the call tree of
       this routine.

   4)  If `np' is less than 1 or greater than SPICE_DSK02_MAXPLT, the error
       SPICE(BADPLATECOUNT) is signaled by a routine in the call tree of
       this routine.

   5)  If the workspace size `worksz' is less than np+1, the error
       SPICE(WORKSPACETOOSMALL) is signaled by a routine in the call tree of
       this routine. This is merely a sanity check; normally the workspace
       will need to be substantially larger than this reference value. See
       the description of `worksz' in the header section -Detailed_Input
       above.

   6)  If the voxel-plate pointer array size `voxpsz' is less than 1, the
       error SPICE(PTRARRAYTOOSMALL) is signaled by a routine in the call
       tree of this routine. This is merely a sanity check; normally this
       pointer array will need to be substantially larger than this
       reference value. See the description of `voxpsz' in the header
       section -Detailed_Input above.

   7)  If the voxel-plate list array size `voxlsz' is less than np+1, the
       error SPICE(PLATELISTTOOSMALL) is signaled by a routine in the call
       tree of this routine. This is merely a sanity check; normally this
       array will need to be substantially larger than this reference value.
       See the description of `voxlsz' in the header section -Detailed_Input
       above.

   8)  If the size `spxisz' of the integer array `spaixi' is too small
       to contain its constituent structures, where the sizes
       of these structures are derived from the inputs

           `nv', `np', `voxpsz', `voxlsz'

       the error SPICE(INTINDEXTOOSMALL) is signaled by a routine in the
       call tree of this routine.

   9)  If there is insufficient room to create any of the data
       structures contained in the spatial index, an error is
       signaled by a routine in the call tree of this routine.

-Files

   None.

-Particulars

   Users planning to create DSK files should consider whether the
   SPICE DSK creation utility MKDSK may be suitable for their needs.

   This routine supports use of the DSK type 2 segment writer dskw02_c
   by creating the "spatial index" arrays required as inputs to that
   routine.

   A spatial index is a group of data structures that facilitates
   rapid high-level computations involving sets of plates. The data
   structures created by this routine are aggregated into arrays
   of type SpiceInt and type SpiceDouble.


   Voxel grids
   ===========

   A key geometric computation---probably the most important, as it
   serves as a foundation for other high-level computations---is
   finding the intersection of a ray with the plate set. DSK type 2
   segments use data structures called "voxel grids" as part of
   their indexing mechanism. There is a "coarse grid": a box that
   completely encloses a DSK type 2 segment's plate set, and which
   is composed of identically-sized cubes called "coarse voxels."
   Each coarse voxel in composed of smaller cubes called "fine
   voxels." When the term "voxel" is used without qualification, it
   refers to fine voxels.

   Type 2 DSK segments contain data structures that associate plates
   with the fine voxels intersected by those plates. These
   structures enable the type 2 DSK software to rapidly find plates
   in a given region of space.

   Voxel scales
   ============

   There are two voxel scales:

   -  The coarse voxel scale is the integer ratio of the
      edge length of a coarse voxel to the edge length of
      a fine voxel

   -  The fine voxel scale is the double precision ratio
      of the edge length of a fine voxel to the average
      extent of the plates in the input plate set. "Extents"
      of a plate are the absolute values of the differences
      between the respective maximum and minimum X, Y, and Z
      coordinates of the plate's vertices.

   Voxel scales determine the resolution of the voxel grid.
   Voxel scales must be chosen to satisfy size constraints and
   provide reasonable plate lookup performance.

   The following considerations apply to spatial indexes of
   type 2 DSK segments:

      1)  The maximum number of coarse voxels is fixed at
          SPICE_DSK02_MAXCGR (declared in SpiceDSK.h).

      2)  If there are too few fine voxels, the average number of
          plates per fine voxel will be very large. This largely
          negates the performance improvement afforded by having an
          index. Also, the number of plates per voxel may exceed
          limits imposed by DSK functions that use static arrays.

      3)  If there are too many fine voxels, the average number of
          voxels intersected by a given plate may be too large for
          all the plate-voxel associations to be stored. In
          addition, the time needed to examine the plate lists for
          each voxel (including the empty ones) may become quite
          large, again negating the value of the index.

   In many cases, voxel scales yielding optimum performance must be
   determined by experiment. However, the following heuristics can
   provide reasonable starting values:

      Let `np' be the number of plates. Let `fs' be the fine voxel
      scale. Then a reasonable value of `fs' may be

                 (0.25)
         fs =  np       / 8.

      In general, `fs' should not smaller than 1.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Create a three-segment DSK file using plate model data for
      Phobos. Use latitudinal, rectangular, and planetodetic
      coordinates in the respective segments. This is not a
      realistic example, but it serves to demonstrate use of
      the supported coordinate systems.

      Use the DSK kernel below to provide, for simplicity, the
      input plate and vertex data. The selected input file has one
      segment.

         phobos_3_3.bds


      Example code begins here.


      /.
         Program dskmi2_ex1

         Example program for dskw02_c, dskmi2_c, and dskrb2_c

         Create a three-segment DSK file using plate model data for
         Phobos. Use latitudinal, rectangular, and planetodetic
         coordinates in the respective segments.

         For simplicity, use an existing DSK file to provide the
         input plate and vertex data. The selected input file has one
         segment.

            Version 1.0.0 22-JAN-2016 (NJB)

      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main()
      {

         /.
         Local constants
         ./
         #define FILSIZ          256
         #define LNSIZE           81
         #define NCOR              4
         #define NSEG              3
         #define NAMLEN           21

         /.
         Local variables
         ./

         /.
         Below, we declare large arrays static to avoid stack
         overflow problems.
         ./

         SpiceBoolean            found;

         SpiceChar               cornam [ NCOR ][ NAMLEN ] =
                                 { "radius",
                                   "Z-coordinate",
                                   "Z-coordinate",
                                   "altitude" };

         SpiceChar             * dsk;
         SpiceChar             * frame;
         SpiceChar             * indsk;

         SpiceDLADescr           dladsc;

         SpiceDouble             corpar [ SPICE_DSK_NSYPAR ];
         SpiceDouble             f;
         SpiceDouble             finscl;
         SpiceDouble             first;
         SpiceDouble             last;
         SpiceDouble             mncor1;
         SpiceDouble             mncor2;
         SpiceDouble             mncor3;
         SpiceDouble             mxcor1;
         SpiceDouble             mxcor2;
         SpiceDouble             mxcor3;
         SpiceDouble             re;
         SpiceDouble             rp;

         /.
         Note: the values of SPICE_DSK02_MAXVRT and
         SPICE_DSK02_MAXPLT declared in SpiceDSK.h,
         integer spatial index dimension SPICE_DSK02_SPAISZ,
         and the workspace dimension SPICE_DSK02_MAXCEL
         are very large. Smaller buffers can be used for most
         applications.
         ./
         static SpiceDouble      spaixd [ SPICE_DSK02_SPADSZ ];
         static SpiceDouble      vrtces [ SPICE_DSK02_MAXVRT ][3];

         SpiceInt                center;
         SpiceInt                corscl;
         SpiceInt                corsys;
         SpiceInt                dclass;
         SpiceInt                handle;
         SpiceInt                i;
         SpiceInt                inhan;
         SpiceInt                np;
         SpiceInt                nv;
         static SpiceInt         plates [ SPICE_DSK02_MAXPLT ][3];
         SpiceInt                segno;
         static SpiceInt         spaixi [ SPICE_DSK02_SPAISZ ];
         SpiceInt                spaisz;
         SpiceInt                surfid;
         SpiceInt                voxpsz;
         SpiceInt                voxlsz;
         static SpiceInt         work   [ SPICE_DSK02_MAXCEL ][2];
         SpiceInt                worksz;


         /.
         Assign names of input and output DSK files.
         ./
         indsk = "phobos_3_3.bds";
         dsk   = "phobos_3_3_3seg.bds";

         /.
         Open input DSK for read access; find first segment.
         ./
         dasopr_c ( indsk, &inhan );
         dlabfs_c ( inhan, &dladsc, &found );

         /.
         Fetch vertices and plates from input DSK file.

         Note that vertex and plate indices are 1-based.
         ./
         printf ( "Reading input data...\n" );

         dskv02_c ( inhan, &dladsc, 1, SPICE_DSK02_MAXVRT,
                    &nv,   vrtces                           );
         dskp02_c ( inhan, &dladsc, 1, SPICE_DSK02_MAXPLT,
                    &np,   plates                           );

         printf ( "Done.\n" );


         /.
         Set input array sizes required by dskmi2_c.
         ./
         voxpsz = SPICE_DSK02_MAXVXP;
         voxlsz = SPICE_DSK02_MXNVLS;
         worksz = SPICE_DSK02_MAXCEL;
         spaisz = SPICE_DSK02_SPAISZ;

         /.
         Set fine and coarse voxel scales. (These usually
         need to determined by experimentation.)
         ./
         finscl = 5.0;
         corscl = 4;

         /.
         Open a new DSK file.
         ./
         dskopn_c ( dsk, dsk, 0, &handle );

         /.
         Create three segments and add them to the file.
         ./
         for ( segno = 1;  segno <= NSEG;  segno++ )
         {
            /.
            Create spatial index. We won't generate a
            vertex-plate mapping, so we set the flag
            for creating this map to "false."
            ./
            printf ( "Creating segment %d\n", (int)segno );
            printf ( "Creating spatial index...\n" );

            dskmi2_c ( nv,     vrtces,     np,     plates,
                       finscl, corscl,     worksz, voxpsz,
                       voxlsz, SPICEFALSE, spaisz, work,
                       spaixd, spaixi                    );

            printf ( "Done.\n" );

            /.
            Set up inputs describing segment attributes:

            - Central body: Phobos
            - Surface ID code: user's choice.
              We use the segment number here.
            - Data class: general (arbitrary) shape
            - Body-fixed reference frame
            - Time coverage bounds (TBD)
            ./
            center = 401;
            surfid = segno;
            dclass = SPICE_DSK_GENCLS;
            frame  = "IAU_PHOBOS";

            first = -50 * jyear_c();
            last  =  50 * jyear_c();

            /.
            Set the coordinate system and coordinate system
            bounds based on the segment index.

            Zero out the coordinate parameters to start.
            ./
            for ( i = 0;  i < SPICE_DSK_NSYPAR;  i++ )
            {
               corpar[i] = 0.0;
            }

            if ( segno == 1 )
            {
               /.
               Use planetocentric latitudinal coordinates. Set
               the longitude and latitude bounds.
               ./
               corsys = SPICE_DSK_LATSYS;

               mncor1 = -pi_c();
               mxcor1 =  pi_c();
               mncor2 = -pi_c()/2;
               mxcor2 =  pi_c()/2;
            }
            else if ( segno == 2 )
            {

               /.
               Use rectangular coordinates. Set the
               X and Y bounds.

               The bounds shown here were derived from
               the plate data. They lie slightly outside
               of the range spanned by the plates.
               ./
               corsys = SPICE_DSK_RECSYS;

               mncor1 = -1.3;
               mxcor1 =  1.31;
               mncor2 = -1.21;
               mxcor2 =  1.2;
            }
            else
            {
               /.
               Set the coordinate system to planetodetic.
               ./
               corsys    = SPICE_DSK_PDTSYS;

               mncor1    = -pi_c();
               mxcor1    =  pi_c();
               mncor2    = -pi_c()/2;
               mxcor2    =  pi_c()/2;

               /.
               We'll use equatorial and polar radii from
               pck00010.tpc. These normally would be fetched
               at run time, but for simplicity, we'll use
               hard-coded values.
               ./
               re        = 13.0;
               rp        =  9.1;
               f         = ( re - rp ) / re;

               corpar[0] = re;
               corpar[1] = f;
            }
            /.
            Compute plate model radius bounds.
            ./
            printf ( "Computing %s bounds of plate set...\n",
                     cornam[corsys-1]                        );

            dskrb2_c ( nv,     vrtces, np,      plates,
                       corsys, corpar, &mncor3, &mxcor3 );

            printf ( "Done.\n" );

            /.
            Write the segment to the file.
            ./
            printf ( "Writing segment...\n" );

            dskw02_c ( handle,
                       center, surfid, dclass, frame,  corsys,
                       corpar, mncor1, mxcor1, mncor2, mxcor2,
                       mncor3, mxcor3, first,  last,   nv,
                       vrtces, np,     plates, spaixd, spaixi );

            printf ( "Done.\n" );
         }

         /.
         Segregate the data records in the DSK file and
         close the file.
         ./
         printf ( "Segregating and closing DSK file...\n" );

         dskcls_c ( handle, SPICETRUE );

         printf ( "Done.\n" );

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Reading input data...
      Done.
      Creating segment 1
      Creating spatial index...
      Done.
      Computing radius bounds of plate set...
      Done.
      Writing segment...
      Done.
      Creating segment 2
      Creating spatial index...
      Done.
      Computing Z-coordinate bounds of plate set...
      Done.
      Writing segment...
      Done.
      Creating segment 3
      Creating spatial index...
      Done.
      Computing altitude bounds of plate set...
      Done.
      Writing segment...
      Done.
      Segregating and closing DSK file...
      Done.


      Note that after run completion, a new DSK exists in the output
      directory.

-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)

-Version

   -CSPICE Version 1.0.1, 10-AUG-2021 (JDR)

       Edited the header to comply with NAIF standard. Fixed I/O type
       of arguments "work", "spaixd" and "spaixi" in -Brief_I/O table.

       Added example's output.

   -CSPICE Version 1.0.0, 13-DEC-2016 (NJB)

-Index_Entries

   make spatial index for type 2 DSK segment

-&
*/

{ /* Begin dskmi2_c */



   /*
   Local variables
   */
   logical                 vtlflg;

   /*
   Participate in error tracing.
   */
   chkin_c ( "dskmi2_c" );

   /*
   Cast input "make vertex list" flag to type logical.
   */
   vtlflg = (logical) makvtl;


   dskmi2_ ( ( integer     * ) &nv,
             ( doublereal  * ) vrtces,
             ( integer     * ) &np,
             ( integer     * ) plates,
             ( doublereal  * ) &finscl,
             ( integer     * ) &corscl,
             ( integer     * ) &worksz,
             ( integer     * ) &voxpsz,
             ( integer     * ) &voxlsz,
             ( logical     * ) &vtlflg,
             ( integer     * ) &spxisz,
             ( integer     * ) work,
             ( doublereal  * ) spaixd,
             ( integer     * ) spaixi   );


   chkout_c ( "dskmi2_c" );

} /* End dskmi2_c */
