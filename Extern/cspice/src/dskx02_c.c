/*

-Procedure dskx02_c ( DSK, ray-surface intercept, type 2 )

-Abstract

   Determine the plate ID and body-fixed coordinates of the
   intersection of a specified ray with the surface defined by a
   type 2 DSK plate model.

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

   GEOMETRY
   SHAPES

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZim.h"
   #include "SpiceDLA.h"
   #undef   dskx02_c

   void dskx02_c ( SpiceInt               handle,
                   ConstSpiceDLADescr   * dladsc,
                   ConstSpiceDouble       vertex  [3],
                   ConstSpiceDouble       raydir  [3],
                   SpiceInt             * plid,
                   SpiceDouble            xpt     [3],
                   SpiceBoolean         * found        )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   handle     I   Handle of DSK kernel containing plate model.
   dladsc     I   DLA descriptor of plate model segment.
   vertex     I   Ray's vertex in the  body fixed frame.
   raydir     I   Ray direction in the body fixed frame.
   plid       O   ID code of the plate intersected by the ray.
   xpt        O   Intercept.
   found      O   Flag indicating whether intercept exists.

-Detailed_Input

   handle      is the file handle of a DSK file containing a shape
               model for a target body. The shape model is stored
               in a type 2 DSK segment.

   dladsc      is the DLA descriptor of a type 2 DSK segment containing
               plate model data representing the surface of the target
               body. Normally this descriptor will be obtained by a
               search through a DSK file using the DLA search routines;
               see the -Examples header section below for a working code
               example illustrating a simple search.

   vertex      is the vertex of a ray. `vertex' is expressed relative
               to the body fixed reference frame associated with the
               target body. This reference frame is the same frame
               relative to which the vertices of the plate model
               are expressed. Units are km.

               The vertex is required to be outside the target
               body.

   raydir      is the ray's direction vector. `raydir' is expressed
               relative to the body fixed reference frame associated
               with the target body.

-Detailed_Output

   plid        is the ID of the plate closest to the input ray's
               vertex at which a ray-surface intercept exists.
               If no intercept exists, `plid' is undefined.

   xpt         is the ray-target intercept closest to the ray's
               vertex, if an intercept exists. `xpt' is expressed
               relative to the body-fixed reference frame associated
               with the target body. Units are km.

               If no intercept exists, `xpt' is undefined.

   found       is a logical flag that indicates whether or not the ray
               does indeed intersect the target. If the ray intersects a
               plate `found' is SPICETRUE. Otherwise `found' is
               SPICEFALSE.

-Parameters

   See the header file

      SpiceDtl.h

   for the values of tolerance parameters used by default by the
   ray-surface intercept algorithm.

   See the header file

      SpiceDLA.h

   for declarations of DLA descriptor sizes and documentation of the
   contents of DLA descriptors.

   See the header file

      SpiceDSK.h

   for declarations of DSK descriptor sizes and documentation of the
   contents of DSK descriptors.

   See the header file

      SpiceDSK.h

   for declarations of DSK data type 2 (plate model) parameters.

-Exceptions

   1)  If the input handle is invalid, an error is signaled by a
       routine in the call tree of this routine.

   2)  If a file read error occurs, the error is signaled by a
       routine in the call tree of this routine.

   3)  If the input DLA descriptor is invalid, the effect of this
       routine is undefined. The error *may* be diagnosed by
       routines in the call tree of this routine, but there are no
       guarantees.

   4)  If an error occurs while trying to look up any component
       of the shape model, the error is signaled by a routine in the
       call tree of this routine.

   5)  If the input ray direction is the zero vector, the error
       SPICE(ZEROVECTOR) is signaled by a routine in the call tree of
       this routine.

   6)  If the coarse voxel grid scale of the shape model is less than
       1, the error SPICE(VALUEOUTOFRANGE) is signaled by a routine
       in the call tree of this routine.

   7)  If the coarse voxel grid of the shape model contains more
       than SPICE_DSK_MAXCGR (see SpiceDSK.h) voxels, the error
       SPICE(GRIDTOOLARGE) is signaled by a routine in the call tree of
       this routine.

   8)  If the plate list for any intersected voxel is too large
       for this routine to buffer, the error SPICE(ARRAYTOOSMALL)
       is signaled by a routine in the call tree of this routine.

   9)  Due to round-off errors, results from this routine may
       differ across platforms. Results also may differ from
       those expected---and not necessarily by a small amount.
       For example, a ray may miss a plate it was expected to
       hit and instead hit another plate considerably farther
       from the ray's vertex, or miss the target entirely.

   10) In the event that an intercept point lies on multiple
       plates (that is, the point is on an edge or vertex),
       a plate will be selected. Due to round-off error, the
       selection may vary across platforms.

-Files

   See the description of the input argument `handle'.

-Particulars

   This routine solves the ray-surface intercept problem for
   a specified ray and a surface represented by triangular plate
   model. The surface representation is provided by data in a
   type 2 segment of a DSK file.

   This routine does not assume that the segment from which the
   surface model data are read represents the entire surface of
   the target body. A program could call this routine repeatedly
   to find the surface intercept of a ray and a shape model
   partitioned into multiple segments.

   In general, this routine should be expected to run faster
   when used with smaller shape models.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as input,
   the compiler and supporting libraries, and the machine specific
   arithmetic implementation.

   1) Find the surface intercept points corresponding to a latitude/
      longitude grid of a specified resolution, for a specified
      target body.

      This simple program assumes the shape model for the target
      body is stored in a single type 2 DSK segment, and that this
      segment is the first one in the DSK file to which it belongs.


      Example code begins here.


      /.
         Program dskx02_ex1
      ./
      #include <stdio.h>
      #include <math.h>
      #include "SpiceUsr.h"

      int main()
      {
         /.
         Local parameters
         ./
         #define  FILSIZ             256
         #define  NLAT               9
         #define  NLON               9
         #define  TOL                ( 1.e-12 )
         #define  SPICE_MXCOR3_IDX   21

         /.
         Local variables
         ./
         SpiceBoolean            found;

         SpiceChar               dsk [ FILSIZ ];

         SpiceDLADescr           dladsc;
         SpiceDSKDescr           dskdsc;

         SpiceDouble             lat;
         SpiceDouble             lon;
         SpiceDouble             maxr;
         SpiceDouble             r;
         SpiceDouble             raydir [3];
         SpiceDouble             vertex [3];
         SpiceDouble             xlat;
         SpiceDouble             xlon;
         SpiceDouble             xpt    [3];
         SpiceDouble             xr;

         SpiceInt                handle;
         SpiceInt                i;
         SpiceInt                j;
         SpiceInt                plid;



         /.
         Prompt for the name of the DSK to read.
         ./
         prompt_c ( "Enter DSK name > ", FILSIZ, dsk );

         /.
         Open the DSK file for read access.
         We use the DAS-level interface for
         this function.
         ./
         dasopr_c ( dsk, &handle );

         /.
         Begin a forward search through the
         kernel, treating the file as a DLA.
         In this example, it's a very short
         search.
         ./
         dlabfs_c ( handle, &dladsc, &found );

         if ( !found  )
         {
            /.
            We arrive here only if the kernel
            contains no segments.  This is
            unexpected, but we're prepared for it.
            ./
            setmsg_c ( "No segments found in DSK file #.");
            errch_c  ( "#",  dsk                         );
            sigerr_c ( "SPICE(NODATA)"                   );
         }

         /.
         If we made it this far, DLADSC is the
         DLA descriptor of the first segment.

         We're going to generate the intercept points
         using a set of rays which point toward the
         origin and whose vertices are on a specified
         specified lat/lon grid.  To start out we
         must pick a reasonable range from the origin
         for the vertices:  the range must be large
         enough so that the vertices are guaranteed
         to be outside the target body but small
         enough that we don't lose too much precision
         in the surface intercept computation.

         We'll look up the upper bound for the target
         radius, then use 2 times this value as the
         vertex magnitude.
         ./

         dskgd_c ( handle, &dladsc, &dskdsc );

         maxr =  dskdsc.co3max;
         r    = 2.0 * maxr;

         /.
         Now generate the intercept points. We generate
         intercepts along latitude bounds, working from
         north to south. Latitude ranges from +80 to -80
         degrees. Longitude ranges from 0 to 320 degrees.
         The increment is 20 degrees for latitude and 40
         degrees for longitude.
         ./
         for ( i = 0;  i < NLAT;  i++  )
         {
            lat = rpd_c() * ( 80.0 - 20.0*i );

            for ( j = 0;  j < NLON;  j++  )
            {
               lon = rpd_c() * 40.0*j;

               /.
               Produce a ray vertex for the current
               lat/lon value.  Negate the vertex to
               produce the ray's direction vector.
               ./
               latrec_c ( r, lon, lat, vertex );
               vminus_c ( vertex,      raydir );

               /.
               Find the surface intercept for this
               ray.
               ./
               dskx02_c ( handle, &dladsc, vertex, raydir,
                          &plid,  xpt,     &found          );

               /.
               Since the ray passes through the origin on
               the body-fixed frame associated with the
               target body, we'd rarely expect to find that
               the ray failed to intersect the surface.
               For safety, we check the FOUND flag.  (A
               "not found" condition could be a sign of
               a bug.)
               ./
               if ( ! found  )
               {
                  printf ( "\n"
                           "Intercept not found!\n"
                           "   Ray vertex:\n"
                           "   Longitude (deg): %f\n"
                           "   Latitude  (deg): %f\n"
                           "   Radius     (km): %e\n"
                           "\n",
                           lon * dpr_c(),
                           lat * dpr_c(),
                           r                           );
               }
               else
               {
                  /.
                  This is the normal case.  Display the
                  intercept plate ID and the intercept
                  point in both cartesian and latitudinal
                  coordinates.  Show the corresponding ray
                  vertex to facilitate validation of results.

                  Use recrad_c rather than reclat_c to produce
                  non-negative longitudes.
                  ./
                  recrad_c ( xpt, &xr, &xlon, &xlat );

                  printf ( "\n"
                           "Intercept found:\n"
                           "   Plate ID:                 %d\n"
                           "   Cartesian Coordinates: %12.8f %12.8f %12.8f\n"
                           "   Latitudinal Coordinates:\n"
                           "   Longitude (deg): %f\n"
                           "   Latitude  (deg): %f\n"
                           "   Radius     (km): %e\n"
                           "\n"
                           "   Ray vertex:\n"
                           "   Longitude (deg): %f\n"
                           "   Latitude  (deg): %f\n"
                           "   Radius     (km): %e\n"
                           "\n",
                           (int)plid,
                           xpt[0], xpt[1], xpt[2],
                           xlon * dpr_c(),
                           xlat * dpr_c(),
                           xr,
                           lon  * dpr_c(),
                           lat  * dpr_c(),
                           r                                           );

                  /.
                  Perform sanity checks on the intercept
                  coordinates.  Stop the program if any error
                  is larger than our tolerance value.
                  ./
                  if ( fabs(xlat-lat) > TOL )
                  {
                     sigerr_c ( "Latitude error!" );
                  }

                  if (  (xlon - lon)  > pi_c()  )
                  {
                     xlon -= twopi_c();
                  }

                  if (  (xlon - lon)  > TOL  )
                  {
                     sigerr_c ( "Longitude error!" );
                  }

                  if ( xr  > (1.0+TOL)*maxr  )
                  {
                     sigerr_c ( "Radius error!" );
                  }
               }
               /.
               End of longitude loop.
               ./
            }
            /.
            End of latitude loop.
            ./
         }

         /.
         Close the kernel.  This isn't necessary in a stand-
         alone program, but it's good practice in subroutines
         because it frees program and system resources.
         ./
         dascls_c ( handle );

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, using the DSK file named phobos_3_3.bds, the output
      was:


      Enter DSK name > phobos_3_3.bds

      Intercept found:
         Plate ID:                 306238
         Cartesian Coordinates:   1.52087789   0.00000000   8.62532711
         Latitudinal Coordinates:
         Longitude (deg): 0.000000
         Latitude  (deg): 80.000000
         Radius     (km): 8.758387e+00

         Ray vertex:
         Longitude (deg): 0.000000
         Latitude  (deg): 80.000000
         Radius     (km): 2.802354e+01


      Intercept found:
         Plate ID:                 317112
         Cartesian Coordinates:   1.18970365   0.99827989   8.80777185
         Latitudinal Coordinates:
         Longitude (deg): 40.000000
         Latitude  (deg): 80.000000
         Radius     (km): 8.943646e+00

         Ray vertex:
         Longitude (deg): 40.000000
         Latitude  (deg): 80.000000
         Radius     (km): 2.802354e+01


      Intercept found:
         Plate ID:                 324141
         Cartesian Coordinates:   0.27777518   1.57534131   9.07202903
         Latitudinal Coordinates:
         Longitude (deg): 80.000000
         Latitude  (deg): 80.000000
         Radius     (km): 9.211980e+00

         Ray vertex:
         Longitude (deg): 80.000000
         Latitude  (deg): 80.000000
         Radius     (km): 2.802354e+01


      Intercept found:
         Plate ID:                 327994
         Cartesian Coordinates:  -0.81082405   1.40438846   9.19682344
         Latitudinal Coordinates:
         Longitude (deg): 120.000000
         Latitude  (deg): 80.000000
         Radius     (km): 9.338699e+00

         Ray vertex:
         Longitude (deg): 120.000000
         Latitude  (deg): 80.000000
         Radius     (km): 2.802354e+01


      Intercept found:
         Plate ID:                 329431
         Cartesian Coordinates:  -1.47820193   0.53802150   8.92132122
         Latitudinal Coordinates:
         Longitude (deg): 160.000000
         Latitude  (deg): 80.000000
         Radius     (km): 9.058947e+00

         Ray vertex:
         Longitude (deg): 160.000000
         Latitude  (deg): 80.000000
         Radius     (km): 2.802354e+01


      Intercept found:
         Plate ID:                 196042
         Cartesian Coordinates:  -1.49854761  -0.54542673   9.04411256
         Latitudinal Coordinates:
         Longitude (deg): 200.000000
         Latitude  (deg): 80.000000
         Radius     (km): 9.183633e+00

         Ray vertex:
         Longitude (deg): 200.000000
         Latitude  (deg): 80.000000
         Radius     (km): 2.802354e+01


      Intercept found:
         Plate ID:                 235899
         Cartesian Coordinates:  -0.78240454  -1.35516441   8.87447325
         Latitudinal Coordinates:
         Longitude (deg): 240.000000
         Latitude  (deg): 80.000000
         Radius     (km): 9.011376e+00

         Ray vertex:
         Longitude (deg): 240.000000
         Latitude  (deg): 80.000000
         Radius     (km): 2.802354e+01



      [...]


      Warning: incomplete output. Only 100 out of 1135 lines have
      been provided.


-Restrictions

   None.

-Literature_References

   [1]  A. Woo, "Fast Ray-Box Intersection", Graphic Gems I,
        395-396, Aug. 1990

-Author_and_Institution

   N.J. Bachman        (JPL)
   J.A. Bytof          (JPL)
   J. Diaz del Rio     (ODC Space)
   W.L. Taber          (JPL)
   E.D. Wright         (JPL)

-Version

   -CSPICE Version 1.0.3, 07-AUG-2021 (JDR)

       Edited the header to comply with NAIF standard. Updated
       code example to remove unnecessary include files.

   -CSPICE Version 1.0.2, 04-APR-2017 (EDW)

       Edit to example program to use "%d" with explicit casts
       to int for printing SpiceInts with printf.

   -CSPICE Version 1.0.1, 20-JUL-2011 (NJB)

       Header correction: the detailed input section
       now says that the ray's vertex *is* required to
       be outside the target body.

   -CSPICE Version 1.0.0, 05-JUN-2010 (NJB) (EDW) (JAB) (WLT)

-Index_Entries

   plate and plate model point intersected by ray
   intersection of ray and surface

-&
*/

{ /* Begin dskx02_c */

   /*
   Local variables
   */
   logical                 fnd;

   SpiceInt                fDLADescr  [ SPICE_DLA_DSCSIZ ];

   /*
   Participate in error tracing.
   */
   chkin_c ( "dskx02_c" );


   /*
   Populate the Fortran DLA descriptor array fCurrent with the contents
   of the input descriptor.
   */
   fDLADescr[SPICE_DLA_BWDIDX] = dladsc->bwdptr;
   fDLADescr[SPICE_DLA_FWDIDX] = dladsc->fwdptr;
   fDLADescr[SPICE_DLA_IBSIDX] = dladsc->ibase;
   fDLADescr[SPICE_DLA_ISZIDX] = dladsc->isize;
   fDLADescr[SPICE_DLA_DBSIDX] = dladsc->dbase;
   fDLADescr[SPICE_DLA_DSZIDX] = dladsc->dsize;
   fDLADescr[SPICE_DLA_CBSIDX] = dladsc->cbase;
   fDLADescr[SPICE_DLA_CSZIDX] = dladsc->csize;


   dskx02_ (  ( integer     * ) &handle,
              ( integer     * ) fDLADescr,
              ( doublereal  * ) vertex,
              ( doublereal  * ) raydir,
              ( integer     * ) plid,
              ( doublereal  * ) xpt,
              ( logical     * ) &fnd    );

   /*
   Set the output SpiceBoolean found flag.
   */

   *found = (SpiceBoolean) fnd;


   chkout_c ( "dskx02_c" );

} /* End dskx02_c */
