/*

-Procedure term_pl02 ( Terminator using DSK type 2 plate model )

-Abstract

   Deprecated: This routine has been superseded by the CSPICE routine
   termpt_c. This routine is supported for purposes of backward
   compatibility only.

   Compute a set of points on the umbral or penumbral terminator of a
   specified target body, where the target body's surface is
   represented by a triangular plate model contained in a type 2 DSK
   segment.

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

   NAIF_IDS
   PCK
   SPK
   TIME

-Keywords

   BODY
   GEOMETRY
   MATH

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZst.h"
   #include "SpiceZmc.h"


   void term_pl02 ( SpiceInt              handle,
                    ConstSpiceDLADescr  * dladsc,
                    ConstSpiceChar      * trmtyp,
                    ConstSpiceChar      * source,
                    ConstSpiceChar      * target,
                    SpiceDouble           et,
                    ConstSpiceChar      * fixref,
                    ConstSpiceChar      * abcorr,
                    ConstSpiceChar      * obsrvr,
                    SpiceInt              npts,
                    SpiceDouble         * trgepc,
                    SpiceDouble           obspos   [3],
                    SpiceDouble           trmpts   [][3],
                    SpiceInt              pltids   []     )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   handle     I   DSK handle.
   dladsc     I   DLA descriptor of target body segment.
   trmtyp     I   Terminator type.
   source     I   Light source.
   target     I   Target body.
   et         I   Observation epoch.
   fixref     I   Body-fixed frame associated with target.
   abcorr     I   Aberration correction.
   obsrvr     I   Observer.
   npts       I   Number of points in terminator set.
   trgepc     O   Epoch associated with target center.
   obspos     O   Position of observer in body-fixed frame.
   trmpts     O   Terminator point set.
   pltids     O   DSK plate IDs of surface points.

-Detailed_Input

   handle      is the DAS file handle of a DSK file open for read
               access. This kernel must contain a type 2 segment
               that provides a plate model representing the entire
               surface of the target body.

   dladsc      is the DLA descriptor of a DSK segment representing
               the surface of a target body.

   trmtyp      is a string indicating the type of terminator to
               compute: umbral or penumbral. The umbral terminator is
               the boundary of the portion of the target surface in
               total shadow. The penumbral terminator is the boundary
               of the portion of the surface that is completely
               illuminated. Note that in astronomy references, the
               unqualified word "terminator" refers to the umbral
               terminator. Here, the unqualified word refers to either
               type of terminator.

               To compute the terminator points, this routine first
               computes a set of points on the terminator of the
               indicated type on the surface of a reference ellipsoid
               for the target body. Each such point defines the
               direction of a ray emanating from the target center and
               associated with a terminator point on the actual surface
               defined by the plate model. The outermost surface
               intercept of each such ray is a considered to be a
               terminator point of the surface defined by the plate
               model.

               Possible values of `trmtyp' are

                  "UMBRAL"
                  "PENUMBRAL"

               Case and leading or trailing blanks in `trmtyp' are
               not significant.

   source      is the name of the body acting as a light source.
               `source' is case-insensitive, and leading and trailing
               blanks in `target' are not significant. Optionally, you
               may supply a string containing the integer ID code
               for the object. For example both "SUN" and "10" are
               legitimate strings that indicate the Sun is the light
               source.

               This routine assumes that a kernel variable
               representing the light source's radii is present in
               the kernel pool. Normally the kernel variable would
               be defined by loading a PCK file.

               The shape of the light source is always modeled as a
               sphere, regardless of whether radii defining a
               triaxial ellipsoidal shape model are available in the
               kernel pool. The maximum radius of the body is used
               as the radius of the sphere.

   target      is the name of the target body. `target' is
               case-insensitive, and leading and trailing blanks in
               `target' are not significant. Optionally, you may supply
               a string containing the integer ID code for the object.
               For example both "MOON" and "301" are legitimate strings
               that indicate the moon is the target body.

               This routine assumes that a kernel variable representing
               the target's radii is present in the kernel pool.
               Normally the kernel variable would be defined by loading
               a PCK file.

   et          is the epoch of participation of the observer,
               expressed as ephemeris seconds past J2000 TDB: `et' is
               the epoch at which the observer's position is
               computed.

               When aberration corrections are not used, `et' is also
               the epoch at which the position and orientation of the
               target body and position of the light source are
               computed.

               When aberration corrections are used, `et' is the epoch
               at which the observer's position relative to the solar
               system barycenter is computed; in this case the
               position and orientation of the target body are
               computed at et-lt or et+lt, where `lt' is the one-way
               light time between the target body's center and the
               observer, and the sign applied to `lt' depends on the
               selected correction. See the description of `abcorr'
               below for details.

   fixref      is the name of the reference frame relative to which
               the output terminator points are expressed. This must
               a body-centered, body-fixed frame associated with the
               target. The frame's axes must be compatible with the
               triaxial ellipsoidal shape model associated with the
               target body (normally provide via a PCK): this
               routine assumes that the first, second, and third
               axis lengths correspond, respectively, to the x, y,
               and z-axes of the frame designated by `fixref'.

               `fixref' may refer to a built-in frame (documented in
               the Frames Required Reading) or a frame defined by a
               loaded frame kernel (FK).

               The orientation of the frame designated by `fixref' is
               evaluated at epoch of participation of the target
               body. See the descriptions of `et' and `abcorr' for
               details.

   abcorr      indicates the aberration correction to be applied
               when computing the observer-target position, the
               orientation of the target body, and the target-
               source position vector.  `abcorr' may be any of
               the following.

                  "NONE"     Apply no correction. Compute the
                             terminator points using the position
                             of the light source and target, and
                             the orientation of the target, at `et'.

               Let `lt' represent the one-way light time between the
               observer and the target body's center. The following
               values of `abcorr' apply to the "reception" case in
               which photons depart from the target body's center at
               the light-time corrected epoch et-lt and *arrive* at
               the observer's location at `et':


                  "LT"       Correct for one-way light time (also
                             called "planetary aberration") using a
                             Newtonian formulation. This correction
                             yields the location of the terminator
                             points at the approximate time they
                             emitted photons arriving at the
                             observer at `et' (the difference between
                             light time to the target center and
                             light time to the terminator points
                             is ignored).

                             The light time correction uses an
                             iterative solution of the light time
                             equation. The solution invoked by the
                             "LT" option uses one iteration.

                             The target position as seen by the
                             observer, the position of the light
                             source as seen from the target at
                             et-lt, and the rotation of the target
                             body, are corrected for light time.

                  'LT+S'     Correct for one-way light time and
                             stellar aberration using a Newtonian
                             formulation. This option modifies the
                             positions obtained with the "LT" option
                             to account for the observer's velocity
                             relative to the solar system
                             barycenter. This correction also
                             applies to the position of the light
                             source relative to the target. The
                             result is the apparent terminator as
                             seen by the observer.

                  "CN"       Converged Newtonian light time
                             correction. In solving the light time
                             equation, the "CN" correction iterates
                             until the solution converges. The
                             position and rotation of the target
                             body and the position of the light
                             source relative to the target are
                             corrected for light time.

                  'CN+S'     Converged Newtonian light time
                             and stellar aberration corrections.

   obsrvr      is the name of the observing body. This is typically
               a spacecraft, the Earth, or a surface point on the
               Earth.  `obsrvr' is case-insensitive, and leading and
               trailing blanks in `obsrvr' are not significant.
               Optionally, you may supply a string containing the
               integer ID code for the object. For example both
               "EARTH" and "399" are legitimate strings that indicate
               the Earth is the observer.

   npts        is the number of terminator points to compute.

-Detailed_Output

   trgepc      is the "target epoch."  `trgepc' is defined as follows:
               letting `lt' be the one-way light time between the
               target center and observer, `trgepc' is either the
               epoch et-lt or `et' depending on whether the requested
               aberration correction is, respectively, for received
               radiation or omitted. `lt' is computed using the
               method indicated by `abcorr'.

               `trgepc' is expressed as seconds past J2000 TDB.


   obspos      is the vector from the center of the target body at
               epoch `trgepc' to the observer at epoch `et'.  `obspos' is
               expressed in the target body-fixed reference frame
               `fixref', which is evaluated at `trgepc'.

               `obspos' is returned to simplify various related
               computations that would otherwise be cumbersome. For
               example, the vector `xvec' from the observer to the
               Ith terminator point can be calculated via the call

                  vminus_c ( trmpts[i], obspos, xvec );

               The components of `obspos' are given in units of km.


   trmpts      is an array of points on the umbral or penumbral
               terminator of the target, as specified by the input
               argument `trmtyp'. The ith point is contained in the
               array elements

                   trmpts[i][j],  j = 0, 1, 2

               As described above, each terminator point lies on a ray
               emanating from the center of the target and passing
               through a terminator point on the target's reference
               ellipsoid. Each terminator point *on the reference
               ellipsoid* is the point of tangency of a plane that is
               also tangent to the light source. These associated
               points of tangency on the light source have uniform
               distribution in longitude when expressed in a
               cylindrical coordinate system whose Z-axis is the
               target-source vector. The magnitude of the separation in
               longitude between the tangency points on the light
               source is

                  2*Pi / npts

               If the reference ellipsoid for the target is spherical,
               the terminator points also are uniformly distributed in
               longitude in the cylindrical system described above. If
               the reference ellipsoid of the target is non-spherical,
               the longitude distribution of the points generally is
               not uniform.

               The terminator points are expressed in the body-fixed
               reference frame designated by `fixref'. Units are km.


   pltids      is an array of integer ID codes of the plates on which
               the terminator points are located. The ith plate ID
               corresponds to the ith terminator point. These ID codes can
               be use to look up data associated with the plate, such
               as the plate's vertices or outward normal vector.

               `pltids' should be declared by the caller

                  SpiceInt            pltids [npts];

-Parameters

   None.

-Exceptions

   1)  If the input frame name `fixref' cannot be mapped
       to a frame ID code, the error SPICE(NOTRANSLATION) is
       signaled.

   2)  If the target name `target' cannot be mapped to a body ID code,
       the error SPICE(IDCODENOTFOUND) is signaled.

   3)  If the source name `source' cannot be mapped to a body ID
       code, an error is signaled by a routine in the call tree of
       this routine.

   4)  If the frame designated by `fixref' is not centered
       on the target, the error SPICE(INVALIDFIXREF) is
       signaled.

   5)  If the terminator type is not recognized, an error
       is signaled by a routine in the call tree of
       this routine.

   6)  If the set size `npts' is not at least 1, an error
       is signaled by a routine in the call tree of
       this routine.

   7)  If any of the reference ellipsoid's semi-axis lengths is
       non-positive, an error is signaled by a routine in the
       call tree of this routine.

   8)  If the light source has non-positive radius, an error
       is signaled by a routine in the call tree of
       this routine.

   9)  If the light source intersects the smallest sphere centered at
       the origin and containing the ellipsoid, an error is signaled
       by a routine in the call tree of this routine.

   10) If radii for the target body or light source are not
       available in the kernel pool, an error is signaled by
       a routine in the call tree of this routine.

   11) If radii are available but either body does not have three
       radii, the error SPICE(INVALIDCOUNT) is signaled.

   12) If any SPK look-up fails, an error is signaled by
       a routine in the call tree of this routine.

   13) If a DSK providing a DSK type 2 plate model has not been
       loaded prior to calling term_pl02, an error is signaled by a
       routine in the call tree of this routine.

   14) If the segment associated with the input DLA descriptor is not
       of data type 2, the error SPICE(WRONGDATATYPE) is signaled.

   15) If a surface point cannot be computed because the ray
       corresponding to a longitude/latitude pair fails to intersect
       the target surface as defined by the plate model, an error is
       signaled by a routine in the call tree of this routine.

   16) If the DSK segment identified by `dladsc' is not for the
       body identified by `target', the error SPICE(DSKTARGETMISMATCH)
       is signaled.

   17) If any input string pointer is null, the error
       SPICE(NULLPOINTER) is signaled.

   18) If any input string has length zero, the error
       SPICE(EMPTYSTRING) is signaled.

-Files

   Appropriate DSK, SPK, PCK, and frame kernels must be loaded by the
   calling program before this routine is called.

   The following data are required:

   -  DSK data:  a DSK file containing a plate model representing the
      target body's surface must be loaded. This kernel must contain
      a type 2 segment that contains data for the entire surface of
      the target body.

   -  SPK data: ephemeris data for target, observer, and light
      source must be loaded. If aberration corrections are used,
      the states of all three objects relative to the solar system
      barycenter must be calculable from the available ephemeris
      data. Typically ephemeris data are made available by loading
      one or more SPK files via furnsh_c.

   -  PCK data: triaxial radii for the target body and
      the light source must be loaded into the kernel pool.
      Typically this is done by loading a text PCK file via
      furnsh_c.

   -  Further PCK data: rotation data for the target body must
      be loaded. These may be provided in a text or binary PCK
      file.

   -  Frame data: if a frame definition is required to convert
      the observer and target states to the target body-fixed
      frame designated by `fixref', that definition must be
      available in the kernel pool. Typically the definitions of
      frames not already built-in to SPICE are supplied by loading
      a frame kernel.

   In all cases, kernel data are normally loaded once per program
   run, NOT every time this routine is called.

-Particulars

   In this routine, we use the term "umbral terminator" to denote
   the curve usually called the "terminator":  this curve is the
   boundary of the portion of the target body's surface that lies in
   total shadow. We use the term "penumbral terminator" to denote
   the boundary of the completely illuminated portion of the
   surface.

   Boundaries of illuminated regions on an arbitrary surface are often
   complicated point sets: boundaries of shadows of mountains and
   craters, if present, all contribute to the overall set. To make the
   terminator computation tractable, we simplify the problem by using a
   reference ellipsoid for guidance. We compute a set of terminator
   points on the reference ellipsoid for the target body, then use
   those points to define the latitudes and longitudes of terminator
   points on the surface defined by the specified triangular shape
   model. As such, the set of terminator points found by this routine
   is just an approximation.

   Below we discuss the computation of terminator points on the target
   body's reference ellipsoid.

   This routine assumes a spherical light source. Light rays are
   assumed to travel along straight lines; refraction is not modeled.

   Points on the reference ellipsoid at which the entire cap of
   the light source is visible are considered to be completely
   illuminated. Points on the ellipsoid at which some portion
   (or all) of the cap of the light source are blocked are
   considered to be in partial (or total) shadow.

   In general, the terminator on an ellipsoid is a more complicated
   curve than the limb (which is always an ellipse).  Aside from
   various special cases, the terminator does not lie in a plane.

   However, the condition for a point X on the ellipsoid to lie on
   the terminator is simple:  a plane tangent to the ellipsoid at X
   must also be tangent to the light source. If this tangent plane
   does not intersect the vector from the center of the ellipsoid to
   the center of the light source, then X lies on the umbral
   terminator; otherwise X lies on the penumbral terminator.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as input,
   the compiler and supporting libraries, and the machine specific
   arithmetic implementation.

   1) Compute sets of umbral and penumbral terminator points on Phobos
      as seen from Mars. Perform a consistency check using the solar
      incidence angle at each point, where the solar incidence angle
      is computed using both a reference ellipsoid and the actual
      plate model surface and surface normal. We expect to see a
      solar incidence angle of approximately 90 degrees. Since the
      solar incidence angle is measured between the local outward
      normal and the direction to the Sun, the solar incidence angle
      at an umbral or penumbral terminator point should be,
      respectively, greater than or less than 90 degrees by
      approximately the angular radius of the Sun as seen from each
      terminator point.


      Use the meta-kernel shown below to load the required SPICE
      kernels.


         KPL/MK

         File: term_pl02_ex1.tm

         This meta-kernel is intended to support operation of SPICE
         example programs. The kernels shown here should not be
         assumed to contain adequate or correct versions of data
         required by SPICE-based user applications.

         In order for an application to use this meta-kernel, the
         kernels referenced here must be present in the user's
         current working directory.

         The names and contents of the kernels referenced
         by this meta-kernel are as follows:

            File name                        Contents
            ---------                        --------
            mar097.bsp                       Mars satellite ephemeris
            pck00010.tpc                     Planet orientation and
                                             radii
            naif0010.tls                     Leapseconds


         \begindata

            KERNELS_TO_LOAD = ( 'mar097.bsp',
                                'pck00010.tpc',
                                'naif0010.tls' )
         \begintext

         End of meta-kernel


      Use the DSK kernel below to provide the plate model representation
      of the surface of Phobos.

         phobos_3_3.bds



      Example code begins here.


      /.
         Program term_pl02_ex1
      ./
      #include <stdio.h>
      #include <math.h>
      #include "SpiceUsr.h"

      int main()
      {

         /.
         Local parameters
         ./
         #define  FILSIZ         256
         #define  NPOINTS        3
         #define  NTYPES         2
         #define  TOL            ( 1.e-12 )
         #define  CORLEN         15
         #define  TYPLEN         81
         #define  TIMLEN         41

         /.
         Local variables
         ./
         SpiceBoolean            found;

         SpiceChar             * abcorr = "LT+S";
         SpiceChar               dsk     [ FILSIZ  ];
         SpiceChar             * fixref = "IAU_PHOBOS";
         SpiceChar               meta    [ FILSIZ  ];

         SpiceChar             * trmtyp;
         SpiceChar             * trmtypes [ NTYPES ] =
                                 {
                                    "Umbral",
                                    "Penumbral"
                                 };

         SpiceChar             * obsrvr = "Mars";
         SpiceChar             * target = "Phobos";
         SpiceChar             * utcstr = "2007 FEB 9 00:00:00 UTC";
         SpiceChar               timstr  [ TIMLEN];

         SpiceDLADescr           dladsc;

         SpiceDouble             delta;
         SpiceDouble             emissn;
         SpiceDouble             lt;
         SpiceDouble             obspos    [3];
         SpiceDouble             phase;
         SpiceDouble             radius;
         SpiceDouble             solar;
         SpiceDouble             sunAngRad;
         SpiceDouble             sunPos    [3];
         SpiceDouble             sunRadii  [3];
         SpiceDouble             sunVec    [3];
         SpiceDouble             trgepc;
         SpiceDouble             et;
         SpiceDouble             lat;
         SpiceDouble             lon;
         SpiceDouble             trmpts    [NPOINTS][3];

         SpiceInt                handle;
         SpiceInt                i;
         SpiceInt                n;
         SpiceInt                typidx;
         SpiceInt                pltids  [NPOINTS];

         /.
         Prompt for the name of a meta-kernel specifying
         all of the other kernels we need.  Load the
         metakernel.
         ./
         prompt_c ( "Enter meta-kernel name > ", FILSIZ, meta );
         furnsh_c ( meta );

         /.
         Prompt for the name of the DSK to read.
         ./
         prompt_c ( "Enter DSK name         > ", FILSIZ, dsk );

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
         If we made it this far, `dladsc' is the
         DLA descriptor of the first segment.

         Convert the observation time to seconds past J2000 TDB.
         ./
         str2et_c ( utcstr, &et );

         timout_c ( et,
                    "YYYY-MON-DD "
                    "HR:MN:SC.### ::TDB(TDB)",
                    TIMLEN,
                    timstr                    );

         printf ( "\n\n"
                  "   Observer:               %s\n"
                  "   Target:                 %s\n"
                  "   Observation epoch:      %s\n"
                  "   Aberration correction:  %s\n"
                  "   Body-fixed frame:       %s\n",
                  obsrvr,
                  target,
                  timstr,
                  abcorr,
                  fixref                            );

         /.
         Look up the radii of the Sun.  We'll use these as
         part of a computation to check the solar incidence
         angles at the terminator points.
         ./
         bodvrd_c ( "SUN", "RADII", 3, &n, sunRadii );

         /.
         Now compute grids of terminator points using both
         terminator types.
         ./
         for ( typidx = 0;  typidx < NTYPES;  typidx++  )
         {
            /.
            Select the terminator type.
            ./
            trmtyp  = trmtypes [typidx];

            printf (  "\n"
                      "   Terminator type: %s\n", trmtyp  );

            /.
            Compute the terminator point set.
            ./
            term_pl02 ( handle,   &dladsc,  trmtyp,   "Sun",
                        target,   et,       fixref,   abcorr,
                        obsrvr,   NPOINTS,  &trgepc,  obspos,
                        trmpts,   pltids                    );

            /.
            Display the terminator points.
            ./
            for ( i = 0;  i < NPOINTS;  i++  )
            {
               printf ( "\n" );

               reclat_c ( trmpts[i], &radius, &lon, &lat );

               printf (
               "      Terminator point %d:\n"
               "         Radius                     (km):  %f\n"
               "         Planetocentric longitude   (deg): %f\n"
               "         Planetocentric latitude    (deg): %f\n"
               "         Plate ID:                         %d\n",
               (int)i,
               radius,
               lon * dpr_c(),
               lat * dpr_c(),
               (int)pltids[i]                                  );

               /.
               Compute the angular radius of the Sun as seen from
               the current terminator point.  Subtracting (adding)
               this value from (to) the solar incidence angle for
               umbral (penumbral) terminator points should yield a
               value close to 90 degrees.  This provides a sanity
               check on the locations of the terminator points.

               First find the position of the Sun relative to the
               target's center at the light time corrected epoch
               trgepc.
               ./
               spkpos_c ( "Sun",  trgepc, fixref,
                          abcorr, target, sunPos, &lt );

               vsub_c ( sunPos, trmpts[i], sunVec );

               sunAngRad = asin ( sunRadii[0] / vnorm_c(sunVec) );

               /.
               Compute the delta by which we adjust the solar
               incidence angles.
               ./
               if ( eqstr_c( trmtyp, "umbral" ) )
               {
                  delta = -sunAngRad;
               }
               else
               {
                  delta =  sunAngRad;
               }

               /.
               Compute the illumination angles using an ellipsoidal
               representation of the target's surface. The role of
               this representation is to provide an outward surface
               normal.
               ./
               illum_c ( target,  et,         abcorr,
                         obsrvr,  trmpts[i],  &phase,
                         &solar,  &emissn             );

               printf (
               "            "
               "Solar incidence angle derived using\n"
               "            "
               "   - an ellipsoidal reference surface        (deg):"
               " %f\n",
               solar  * dpr_c()                                      );

               printf (
               "            "
               "        > adjusted for Solar angular radius  (deg):"
               " %f\n",
               (solar+delta)  * dpr_c()                              );

               /.
               Compute the illumination angles at the terminator point
               using the actual plate model surface normal.
               ./
               illum_pl02 ( handle, &dladsc, target,   et,
                            abcorr, obsrvr,  trmpts[i],
                            &phase, &solar,  &emissn      );

               printf (
               "            "
               "   - plate model's surface and normal vector (deg):"
               " %f\n",
               solar  * dpr_c()                                      );
            }
         }

         printf ( "\n" );

         /.
         Close the kernel.  This isn't necessary in a stand-
         alone program, but it's good practice in subroutines
         because it frees program and system resources.
         ./
         dascls_c ( handle );

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, using the meta-kernel file named term_pl02_ex1.tm and
      the DSK file named phobos_3_3.bds, the output was:


      Enter meta-kernel name > term_pl02_ex1.tm
      Enter DSK name         > phobos_3_3.bds


         Observer:               Mars
         Target:                 Phobos
         Observation epoch:      2007-FEB-09 00:01:05.184 (TDB)
         Aberration correction:  LT+S
         Body-fixed frame:       IAU_PHOBOS

         Terminator type: Umbral

            Terminator point 0:
               Radius                     (km):  12.111257
               Planetocentric longitude   (deg): 34.584501
               Planetocentric latitude    (deg): -0.001298
               Plate ID:                         200400
                  Solar incidence angle derived using
                     - an ellipsoidal reference surface        (deg): 90.182028
                          > adjusted for Solar angular radius  (deg): 90.000000
                     - plate model's surface and normal vector (deg): 90.240660

            Terminator point 1:
               Radius                     (km):  9.774665
               Planetocentric longitude   (deg): -143.659941
               Planetocentric latitude    (deg): 43.397190
               Plate ID:                         156958
                  Solar incidence angle derived using
                     - an ellipsoidal reference surface        (deg): 90.182028
                          > adjusted for Solar angular radius  (deg): 90.000000
                     - plate model's surface and normal vector (deg): 87.138686

            Terminator point 2:
               Radius                     (km):  11.500619
               Planetocentric longitude   (deg): -146.128151
               Planetocentric latitude    (deg): -43.082379
               Plate ID:                         25552
                  Solar incidence angle derived using
                     - an ellipsoidal reference surface        (deg): 90.182029
                          > adjusted for Solar angular radius  (deg): 90.000000
                     - plate model's surface and normal vector (deg): 91.404206

         Terminator type: Penumbral

            Terminator point 0:
               Radius                     (km):  12.859785
               Planetocentric longitude   (deg): -145.415505
               Planetocentric latitude    (deg): 0.001299
               Plate ID:                         86763
                  Solar incidence angle derived using
                     - an ellipsoidal reference surface        (deg): 89.817972
                          > adjusted for Solar angular radius  (deg): 90.000000
                     - plate model's surface and normal vector (deg): 89.055489

            Terminator point 1:
               Radius                     (km):  10.327413
               Planetocentric longitude   (deg): 36.340069
               Planetocentric latitude    (deg): -43.397192
               Plate ID:                         76977
                  Solar incidence angle derived using
                     - an ellipsoidal reference surface        (deg): 89.817972
                          > adjusted for Solar angular radius  (deg): 90.000000
                     - plate model's surface and normal vector (deg): 77.351956

            Terminator point 2:
               Radius                     (km):  10.086025
               Planetocentric longitude   (deg): 33.871859
               Planetocentric latitude    (deg): 43.082380
               Plate ID:                         282136
                  Solar incidence angle derived using
                     - an ellipsoidal reference surface        (deg): 89.817972
                          > adjusted for Solar angular radius  (deg): 90.000000
                     - plate model's surface and normal vector (deg): 88.997322


-Restrictions

   1)  This routine models light paths as straight lines.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)

-Version

   -CSPICE Version 1.1.0, 26-OCT-2021 (JDR)

       Changed the argument names "npoints" and "plateIDs" to "npts" and
       "pltids" for consistency with other routines.

       Edited the header to comply with NAIF standard.

       Index lines now state that this routine is deprecated.

   -CSPICE Version 1.0.0, 22-FEB-2017 (NJB)

       Include file references have been updated. Integer output
       format in the example program has been updated.

       An error in the Detailed_Output header section was corrected.
       The text now states that the central axis of the cylindrical
       system is the target-source vector.

       29-APR-2014 (NJB)

          Bug fix: added chkout_c call after call to edterm_.

          Bug fix: corrected module name used in check-in and
          check-out calls.

          Bug fix: added error handling for null input string
          pointers and empty strings.

          Changed name of argument `fixfrm' to `fixref'.

       14-MAY-2010 (NJB)

          Updated kernels in example program.

       12-FEB-2010 (NJB)

          This routine now calls the CSPICE routine

             edterm_

          instead of

             edterm_pl02__

          This routine was updated to include

             SpiceDSK.h
             pl02.h

          The header example has been updated to use current
          planetary and satellite SPK files.

       09-FEB-2007 (NJB)

-Index_Entries

   DEPRECATED find terminator on plate model
   DEPRECATED find terminator on triangular shape model
   DEPRECATED find umbral terminator on plate model
   DEPRECATED find umbral terminator on plate model
   DEPRECATED find penumbral terminator on plate model
   DEPRECATED find penumbral terminator on shape model

-&
*/

{ /* Begin term_pl02_c */


   /*
   Local variables
   */
   SpiceBoolean            found;

   SpiceDSKDescr           dskdsc;

   /*
   Pointer for a dynamically allocated lon/lat grid
   (the type of this pointer is "pointer to an
   array of two SpiceDoubles").
   */
   SpiceDouble         ( * lonLatGridPtr )[2];
   SpiceDouble             radius;

   SpiceInt                i;
   SpiceInt                nBytes;
   SpiceInt                trgcde;


   /*
   Participate in error tracing.
   */
   chkin_c ( "term_pl02" );


   /*
   Check the input strings. Make sure none of the pointers are null and
   that each string contains at least one non-null character.
   */
   CHKFSTR ( CHK_STANDARD, "term_pl02", trmtyp );
   CHKFSTR ( CHK_STANDARD, "term_pl02", source );
   CHKFSTR ( CHK_STANDARD, "term_pl02", target );
   CHKFSTR ( CHK_STANDARD, "term_pl02", fixref );
   CHKFSTR ( CHK_STANDARD, "term_pl02", abcorr );
   CHKFSTR ( CHK_STANDARD, "term_pl02", obsrvr );

   /*
   Map the target name to an ID code.
   */
   bods2c_c ( target, &trgcde, &found );

   if ( failed_c() )
   {
      chkout_c ( "term_pl02" );
      return;
   }

   if ( !found )
   {
      setmsg_c ( "The target name # could not be mapped "
                 "to an ID code."                        );
      errch_c  ( "#", target                             );
      sigerr_c ( "SPICE(IDCODENOTFOUND)"                 );
      chkout_c ( "term_pl02"                             );
      return;
   }

   /*
   Check the DSK descriptor of the DSK segment; make sure it's
   for the correct body.
   */
   dskgd_c ( handle, dladsc, &dskdsc );

   if ( failed_c() )
   {
      chkout_c ( "term_pl02" );
      return;
   }

   if ( dskdsc.center != trgcde )
   {
      setmsg_c ( "The target is # but the input DSK "
                 "segment is for body #."            );
      errint_c ( "#", dskdsc.center                  );
      sigerr_c ( "SPICE(DSKTARGETMISMATCH)"          );
      chkout_c ( "term_pl02"                         );
      return;
   }


   /*
   First step:  find terminator points on the reference ellipsoid.  We'll
   let the f2c'd routine edterm_pl02__ handle checking of the inputs; if
   this routine signals an error, we'll exit.

   Note in particular that if a PCK file providing radii for the target
   reference ellipsoid hasn't been loaded, we'll find out here.
   */
   edterm_ (  ( char       * ) trmtyp,
              ( char       * ) source,
              ( char       * ) target,
              ( doublereal * ) &et,
              ( char       * ) fixref,
              ( char       * ) abcorr,
              ( char       * ) obsrvr,
              ( integer    * ) &npts,
              ( doublereal * ) trgepc,
              ( doublereal * ) obspos,
              ( doublereal * ) trmpts,
              ( ftnlen       ) strlen(trmtyp),
              ( ftnlen       ) strlen(source),
              ( ftnlen       ) strlen(target),
              ( ftnlen       ) strlen(fixref),
              ( ftnlen       ) strlen(abcorr),
              ( ftnlen       ) strlen(obsrvr)   );

   if ( failed_c() )
   {
      chkout_c ( "term_pl02" );
      return;
   }

   /*
   At this point, the outputs

      trgepc
      obspos

   have been set.

   Allocate an array to hold longitude/latitude coordinates
   corresponding to the terminator points on the reference
   ellipsoid.
   */
   nBytes        =   npts * 2 * sizeof( SpiceDouble );

   lonLatGridPtr = ( SpiceDouble(*)[2] ) malloc( nBytes );

   if ( !lonLatGridPtr )
   {
      setmsg_c ( "Call to malloc to allocate # bytes of memory "
                 "for the lon/lat array failed."                 );
      errint_c ( "#",  nBytes                                    );
      chkout_c ( "term_pl02"                                     );
      return;
   }

   /*
   Fill in the longitude/latitude grid.
   */
   for ( i = 0;  i < npts;  i++ )
   {
      /*
      Convert the ith terminator point on the reference ellipsoid
      from rectangular to latitudinal coordinates.
      */
      reclat_c (  trmpts[i],
                  &radius,
                  &( lonLatGridPtr[i][0] ),
                  &( lonLatGridPtr[i][1] )   );
   }

   /*
   Find the plate model's surface points at the lon/lat locations
   we've computed.  Find the plate ID corresponding to each surface
   point as well.

   We cast the lon/lat grid pointer to "pointer to const array of two
   SpiceDoubles" for compatibility with the prototype of llgrid_pl02.
   Only the const qualifier is new; the size is unchanged.
   */
   llgrid_pl02 ( handle,
                 dladsc,
                 npts,
                 (ConstSpiceDouble (*)[2]) lonLatGridPtr,
                 trmpts,
                 pltids                                 );

   /*
   If no error occurred, the outputs

      trmpts
      pltids

   are set.

   Free the dynamically allocated lon/lat array.
   */
   free ( lonLatGridPtr );

   chkout_c ( "term_pl02" );

} /* End term_pl02_c */
