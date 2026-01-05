/*

-Procedure dskw02_c ( DSK, write type 2 segment )

-Abstract

   Write a type 2 segment to a DSK file.

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
   NAIF_IDS

-Keywords

   DAS
   DSK
   FILES
   PLATE
   TOPOGRAPHY

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZmc.h"
   #undef dskw02_c


   void dskw02_c ( SpiceInt             handle,
                   SpiceInt             center,
                   SpiceInt             surfid,
                   SpiceInt             dclass,
                   ConstSpiceChar     * frame,
                   SpiceInt             corsys,
                   ConstSpiceDouble     corpar[],
                   SpiceDouble          mncor1,
                   SpiceDouble          mxcor1,
                   SpiceDouble          mncor2,
                   SpiceDouble          mxcor2,
                   SpiceDouble          mncor3,
                   SpiceDouble          mxcor3,
                   SpiceDouble          first,
                   SpiceDouble          last,
                   SpiceInt             nv,
                   ConstSpiceDouble     vrtces[][3],
                   SpiceInt             np,
                   ConstSpiceInt        plates[][3],
                   ConstSpiceDouble     spaixd[],
                   ConstSpiceInt        spaixi[]      )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   handle     I   Handle assigned to the opened DSK file.
   center     I   Central body ID code.
   surfid     I   Surface ID code.
   dclass     I   Data class.
   frame      I   Reference frame.
   corsys     I   Coordinate system code.
   corpar     I   Coordinate system parameters.
   mncor1     I   Minimum value of first coordinate.
   mxcor1     I   Maximum value of first coordinate.
   mncor2     I   Minimum value of second coordinate.
   mxcor2     I   Maximum value of second coordinate.
   mncor3     I   Minimum value of third coordinate.
   mxcor3     I   Maximum value of third coordinate.
   first      I   Coverage start time.
   last       I   Coverage stop time.
   nv         I   Number of vertices.
   vrtces     I   Vertices.
   np         I   Number of plates.
   plates     I   Plates.
   spaixd     I   Double precision component of spatial index.
   spaixi     I   Integer component of spatial index.
   SPICE_DSK_ANGMRG
              P   Angular round-off margin.
   SPICE_DSK_GENCLS
              P   General surface DSK class.
   SPICE_DSK_SVFCLS
              P   Single-valued function DSK class.
   SPICE_DSK_NSYPAR
              P   Maximum number of coordinate system parameters in
                  a DSK descriptor.
   SPICE_DSK02_MAXCGR
              P   Maximum DSK type 2 coarse voxel count.
   SPICE_DSK02_MAXPLT
              P   Maximum DSK type 2 plate count.
   SPICE_DSK02_MAXVOX
              P   Maximum DSK type 2 voxel count.
   SPICE_DSK02_MAXVRT
              P   Maximum DSK type 2 vertex count.

-Detailed_Input

   handle      is the DAS file handle associated with a DSK file.
               The file must be open for write access.

   center      is the ID code of the body whose surface is described
               by the input plate model. `center' refers to an
               ephemeris object.

   surfid      is the ID code of the surface described by the input
               plate model. Multiple surfaces (for example, surfaces
               having different resolutions) may be associated with a
               given body.

   dclass      is the data class of the input data set. See the
               header file SpiceDSK.h for values and meanings.

   frame       is the name of the reference frame with respect to
               which the input data are expressed.

   corsys      is the coordinate system in which the spatial coverage
               of the input data is expressed. `corsys' is an integer
               code. See the header file SpiceDSK.h for values and
               meanings.

   corpar      is an array of parameters associated with the input
               coordinate system.

               For latitudinal and rectangular coordinates, `corpar'
               is ignored.

               For planetodetic coordinates, the contents of `corpar'
               are:

                  Element         Contents
                  ---------       -----------------------------------
                  corpar[0]       Equatorial radius of reference
                                  spheroid.

                  corpar[1]       Flattening coefficient. The polar
                                  radius of the reference spheroid
                                  is given by

                                     corpar[0] * ( 1 - corpar[1] )

                  corpar[2]...
                  corpar[SPICE_DSK_NSYPAR-1]  Unused.


   mncor1,
   mxcor1,
   mncor2,
   mxcor2,
   mncor3,
   mxcor3      are, respectively, lower and upper bounds of
               each of the coordinates of the input data, where the
               coordinate system is defined by `corsys' and `corpar'.
               These bounds define the region for which the segment
               provides data.

               Distance units are km. Angular units are radians.

               The interpretation of these bounds depends on the data
               class; see `dclass' above.

                  Single-valued surfaces
                  ----------------------

                  If the segment has data class SPICE_DSK_SVFCLS (see
                  SpiceDSK.h), the segment defines a surface as a
                  single-valued function of its domain coordinates:
                  for example, it may define the radius of the
                  surface as a function of planetocentric longitude
                  and latitude.

                  In this case, the input data must cover a
                  rectangle in dimensions 1 and 2 of the input
                  coordinate system: the set of points

                     R = { (x,y): mncor1 < x < mxcor1;
                                  mncor2 < y < mxcor2  }

                  must be completely covered by the input data. In
                  other words, for each point (x,y) of R, there must
                  be some plate containing a point whose first two
                  coordinates are (x,y).

                  The plate set may extend beyond the coordinate
                  range defined by the bounds on the domain.

                  Normally for single-valued surfaces, `mncor3' and
                  `mxcor3' are the minimum and maximum values of the
                  function attained on the domain.


                  General surfaces
                  ----------------

                  If the segment has data class SPICE_DSK_GENCLS (see
                  SpiceDSK.h), the segment simply contains a collection
                  of plates: no guarantees are made about the topology
                  of the surface. The coordinate bounds simply indicate
                  the spatial region for which the segment provides
                  data.

                  Note that shapes of small bodies such as asteroids
                  and comet nuclei may fall into the "general
                  surface" category. Surface features such as cliffs,
                  caves, and arches can prevent a surface from being
                  represented as a single-valued function of latitude
                  and longitude, for example.


               Longitude interpretation and restrictions
               -----------------------------------------

               The following rules apply to longitudes provided in
               the arguments

                  mncor1
                  mxcor1

               All angles have units of radians. The tolerance
               SPICE_DSK_ANGMRG is used for the comparisons shown
               below.

                  1) Longitudes must be in the range

                        -2*pi  :  2*pi

                     Values that are out of range by no more than
                     SPICE_DSK_ANGMRG are bracketed to be in range.


                  2) It is acceptable for the longitude bounds to be
                     equal or out of order. If

                        mxcor1 < mncor1
                               -

                     then either `mxcor1' is treated by the DSK
                     subsystem as though it were mxcor1 + 2*pi, or
                     `mncor1' is treated as mncor1 - 2*pi: whichever
                     shift puts the bounds in the allowed range is
                     made.

                     The input longitude bounds must not be equal.
                     If the lower bound is greater than the upper
                     bound, the difference between the bounds must
                     not be an integer multiple of 2*pi.

                     Aside from any small changes made to move the
                     input values of `mncor1' or `mxcor1' into range,
                     the values are stored in the DSK segment as is.


                  3) `mxcor1' must not exceed `mncor1' by more than 2*pi.
                     Values that are out of range by no more than
                     SPICE_DSK_ANGMRG are bracketed to be in range.

   first,
   last        are the endpoints of the time interval over which this
               data set is applicable. These times are expressed as
               seconds past J2000 TDB.

   nv          is the number of vertices belonging to the plate
               model.

   vrtces      is an array of coordinates of the vertices.
               The ith vertex occupies elements [i-1][0:2] of
               this array.

   np          is the number of plates in the plate model.

   plates      is an array representing the plates of the model.
               The elements of `plates' are vertex indices. The vertex
               indices of the ith plate occupy elements [i-1][0:2] of
               this array.

   spaixd,
   spaixi      are, respectively, the double precision and integer
               components of the spatial index of the segment.

               It is strongly recommended that the helper routine
               dskmi2_c be used to create these arrays. See the
               -Examples section below.

-Detailed_Output

   None. This routine operates by side effects.

-Parameters

   See the header files

      SpiceDSK.h
      SpiceDtl.h

   for declarations of parameters that may be used as inputs to this
   routine, or that may be used to declare bounds of arrays which are
   arguments of this routine.

-Exceptions

   1)  If the reference frame name `frame' could not be mapped to
       an ID code, the error SPICE(FRAMEIDNOTFOUND) is signaled by
       a routine in the call tree of this routine.

   2)  If the segment stop time precedes the start time, the
       error SPICE(TIMESOUTOFORDER) is signaled by a routine in the
       call tree of this routine.

   3)  If an input longitude value is outside the range

          [ -2*pi - SPICE_DSK_ANGMRG,   2*pi + SPICE_DSK_ANGMRG ]

       the error SPICE(VALUEOUTOFRANGE) is signaled by a routine in
       the call tree of this routine. Longitudes outside of the range
       by a smaller amount than SPICE_DSK_ANGMRG will be truncated to
       lie in the interval [-2*pi, 2*pi].

   4)  If the absolute value of the difference between the input
       maximum longitude and the minimum longitude is more than 2*pi +
       SPICE_DSK_ANGMRG, the error SPICE(INVALIDLONEXTENT) will be
       signaled by a routine in the call tree of this routine. If
       either longitude bound exceeds the other by an amount between
       2*pi and 2*pi+SPICE_DSK_ANGMRG, the larger value will be
       truncated to the smaller value plus 2*pi.

   5)  If an input latitude value is outside the range

          [ -pi/2 - SPICE_DSK_ANGMRG,   pi/2 + SPICE_DSK_ANGMRG ]

       the error SPICE(VALUEOUTOFRANGE) is signaled by a routine in
       the call tree of this routine. Latitudes outside of the range
       by a smaller amount than SPICE_DSK_ANGMRG will be truncated to
       lie in the interval [-pi/2, pi/2].

   6)  If the coordinate system is latitudinal and the lower radius
       bound is negative, or if the upper radius bound is
       non-positive, the error SPICE(VALUEOUTOFRANGE) is signaled
       by a routine in the call tree of this routine.

   7)  If the coordinate system is latitudinal or planetodetic
       and the bounds of the radius or altitude coordinate are
       out of order, the error SPICE(BOUNDSOUTOFORDER) is signaled
       by a routine in the call tree of this routine.

   8)  If the coordinate system is latitudinal or planetodetic and
       the lower and upper bounds of the longitude, latitude, radius
       or altitude coordinate, respectively, are equal, the error
       SPICE(ZEROBOUNDSEXTENT) is signaled by a routine in the call
       tree of this routine. If the lower longitude bound is greater
       than the upper bound, and if the difference between the
       bounds is an integer multiple of 2*pi, the same error is
       signaled.

   9)  If the coordinate system is planetodetic and the input
       equatorial radius is non-positive, the error
       SPICE(VALUEOUTOFRANGE) is signaled by a routine in the call
       tree of this routine.

   10) If the coordinate system is planetodetic and the input
       flattening coefficient is greater than or equal to 1, the
       error SPICE(VALUEOUTOFRANGE) is signaled by a routine in the
       call tree of this routine.

   11) If the coordinate system is planetodetic, and if the minimum
       altitude is less than the maximum of

                 2           2
           {  -(B / A),   -(A / B)  }

       where A and B are the semi-major and semi-minor axis lengths
       of the reference ellipsoid, the error SPICE(DEGENERATESURFACE)
       is signaled by a routine in the call tree of this routine.

   12) If the coordinate system is rectangular and any coordinate
       lower bound is greater than or equal to the corresponding
       upper bound, the error SPICE(BOUNDSOUTOFORDER) is signaled
       by a routine in the call tree of this routine.

   13) If the coordinate system code is not recognized, the error
       SPICE(NOTSUPPORTED) is signaled by a routine in the call tree
       of this routine.

   14) If any vertex index belonging to an input plate is outside of
       the range 1:nv, the error SPICE(BADVERTEXINDEX) is signaled
       by a routine in the call tree of this routine.

   15) If `nv' is less than 1 or greater than SPICE_DSK02_MAXVRT, the
       error SPICE(VALUEOUTOFRANGE) is signaled by a routine in the
       call tree of this routine.

   16) If `np' is less than 1 or greater than SPICE_DSK02_MAXPLT, the
       error SPICE(VALUEOUTOFRANGE) is signaled by a routine in the
       call tree of this routine.

   17) If any voxel grid extent is less than 1 or greater than
       SPICE_DSK02_MAXVOX, the error SPICE(VALUEOUTOFRANGE) is
       signaled by a routine in the call tree of this routine.

   18) If the voxel count is greater than SPICE_DSK02_MAXVOX, the error
       SPICE(VALUEOUTOFRANGE) is signaled by a routine in the call
       tree of this routine.

   19) If the coarse voxel count is less than 1 or greater than
       SPICE_DSK02_MAXCGR, the error SPICE(VALUEOUTOFRANGE) is
       signaled by a routine in the call tree of this routine.

   20) If the coarse voxel scale is less than 1 or more than
       the cube root of the fine voxel count, the error
       SPICE(VALUEOUTOFRANGE) is signaled by a routine in the call
       tree of this routine.

   21) If the cube of the coarse voxel scale does not divide the
       fine voxel count evenly, the error SPICE(INCOMPATIBLESCALE)
       is signaled by a routine in the call tree of this routine.

   22) If the input data class is not recognized, the error
       SPICE(NOTSUPPORTED) is signaled by a routine in the call
       tree of this routine.

   23) If an error occurs while writing the segment to the output
       DSK file, the error is signaled by a routine in the call
       tree of this routine.

   24) If the `frame' input string pointer is null, the error
       SPICE(NULLPOINTER) is signaled.

   25) If the `frame' input string has zero length, the error
       SPICE(EMPTYSTRING) is signaled.

-Files

   See argument `handle'.

-Particulars

   This routine writes a type 2 segment to a DSK file that has been
   opened for write access.

   Users planning to create DSK files should consider whether the
   SPICE DSK creation utility MKDSK may be suitable for their needs.

   This routine is supported by the routines dskmi2_c and dskrb2_c
   dskmi2_c simplifies use of this routine by creating the "spatial
   index" arrays required as inputs by this routine. dskrb2_c computes
   bounds on the third coordinate of the input plate set.

   Spatial Indexes
   ===============

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
          index. Also, the number of plates per voxel may exceed limits
          imposed by DSK subroutines that use static arrays.

      3)  If there are too many fine voxels, the average number of
          voxels intersected by a given plate may be too large for all
          the plate-voxel associations to be stored. In addition, the
          time needed to examine the plate lists for each voxel
          (including the empty ones) may become quite large, again
          negating the value of the index.

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
         Program dskw02_ex1

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

       Edited the header to comply with NAIF standard.
       Added example's output.

   -CSPICE Version 1.0.0, 04-APR-2017 (NJB)

       Based on Alpha DSK version 2.0.0, 13-MAY-2010 (NJB).

       Caution: this routine's argument list has been changed.

-Index_Entries

   write a type 2 DSK segment

-&
*/

{ /* Begin dskw02_c */


   /*
   Participate in error tracing.
   */
   chkin_c ( "dskw02_c" );

   /*
   Check the input frame string to make sure the pointer is non-null
   and the string length is non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "dskw02_c", frame );


   dskw02_ ( (integer     * ) &handle,
             (integer     * ) &center,
             (integer     * ) &surfid,
             (integer     * ) &dclass,
             (char        * ) frame,
             (integer     * ) &corsys,
             (doublereal  * ) corpar,
             (doublereal  * ) &mncor1,
             (doublereal  * ) &mxcor1,
             (doublereal  * ) &mncor2,
             (doublereal  * ) &mxcor2,
             (doublereal  * ) &mncor3,
             (doublereal  * ) &mxcor3,
             (doublereal  * ) &first,
             (doublereal  * ) &last,
             (integer     * ) &nv,
             (doublereal  * ) vrtces,
             (integer     * ) &np,
             (integer     * ) plates,
             (doublereal  * ) spaixd,
             (integer     * ) spaixi,
             (ftnlen        ) strlen(frame)  );


   chkout_c ( "dskw02_c" );

} /* End dskw02_c */
