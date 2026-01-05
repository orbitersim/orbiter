/*

-Procedure edterm_c ( Ellipsoid terminator )

-Abstract

   Compute a set of points on the umbral or penumbral terminator of
   a specified target body, where the target shape is modeled as an
   ellipsoid.

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

   FRAMES
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
   #include "SpiceZmc.h"
   #include "SpiceZst.h"
   #undef   edterm_c

   void edterm_c ( ConstSpiceChar     * trmtyp,
                   ConstSpiceChar     * source,
                   ConstSpiceChar     * target,
                   SpiceDouble          et,
                   ConstSpiceChar     * fixref,
                   ConstSpiceChar     * abcorr,
                   ConstSpiceChar     * obsrvr,
                   SpiceInt             npts,
                   SpiceDouble        * trgepc,
                   SpiceDouble          obspos  [3],
                   SpiceDouble          trmpts  [ ][3] )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
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

-Detailed_Input

   trmtyp      is a string indicating the type of terminator to
               compute: umbral or penumbral. The umbral terminator is
               the boundary of the portion of the ellipsoid surface in
               total shadow. The penumbral terminator is the boundary
               of the portion of the surface that is completely
               illuminated. Note that in astronomy references, the
               unqualified word "terminator" refers to the umbral
               terminator. Here, the unqualified word refers to either
               type of terminator.

               Possible values of `trmtyp' are

                  "UMBRAL"
                  "PENUMBRAL"

               Case and leading or trailing blanks in `trmtyp' are
               not significant.


   source      is the name of the body acting as a light source.
               `source' is case-insensitive, and leading and trailing
               blanks in `target' are not significant. Optionally, you
               may supply a string containing the integer ID code for
               the object. For example both "SUN" and "10" are
               legitimate strings that indicate the Sun is the light
               source.

               This routine assumes that a kernel variable representing
               the light source's radii is present in the kernel pool.
               Normally the kernel variable would be defined by loading
               a PCK file.

               The shape of the light source is always modeled as a
               sphere, regardless of whether radii defining a triaxial
               ellipsoidal shape model are available in the kernel
               pool. The maximum radius of the body is used as the
               radius of the sphere.


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


   et          is the epoch of participation of the observer, expressed
               as ephemeris seconds past J2000 TDB: `et' is the epoch
               at which the observer's position is computed.

               When aberration corrections are not used, `et' is also
               the epoch at which the position and orientation of the
               target body and position of the light source are
               computed.

               When aberration corrections are used, `et' is the epoch
               at which the observer's position relative to the solar
               system barycenter is computed; in this case the position
               and orientation of the target body are computed at
               et-lt, where lt is the one-way light time between the
               target body's center and the observer. See the
               description of `abcorr' below for details.


   fixref      is the name of the reference frame relative to which the
               output terminator points are expressed. This must be a
               body-centered, body-fixed frame associated with the
               target. The frame's axes must be compatible with the
               triaxial ellipsoidal shape model associated with the
               target body (normally provide via a PCK): this routine
               assumes that the first, second, and third axis lengths
               correspond, respectively, to the x, y, and z-axes of the
               frame designated by `fixref'.

               `fixref' may refer to a built-in frame (documented in
               the Frames Required Reading) or a frame defined by a
               loaded frame kernel (FK).

               The orientation of the frame designated by `fixref' is
               evaluated at epoch of participation of the target body.
               See the descriptions of `et' and `abcorr' for details.


   abcorr      indicates the aberration correction to be applied
               when computing the observer-target position, the
               orientation of the target body, and the target-
               source position vector. `abcorr' may be any of
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
               Earth. `obsrvr' is case-insensitive, and leading and
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
               epoch `trgepc' to the observer at epoch `et'. `obspos' is
               expressed in the target body-fixed reference frame
               `fixref', which is evaluated at `trgepc'.

               `obspos' is returned to simplify various related
               computations that would otherwise be cumbersome. For
               example, the vector `xvec' from the observer to the
               ith terminator point can be calculated via the call

                  vsub_c ( trmpts[i], obspos, xvec );

               To transform the vector `obspos' from a reference frame
               `fixref' at time `trgepc' to a time-dependent reference
               frame `ref' at time `et', the routine pxfrm2_c should be
               called. Let `xform' be the 3x3 matrix representing the
               rotation from the reference frame `fixref' at time
               `trgepc' to the reference frame `ref' at time `et'. Then
               `obspos' can be transformed to the result `refvec' as
               follows:

                   pxfrm2_c ( fixref, ref,    trgepc, et, xform );
                   mxv_c    ( xform,  obspos, refvec );


   trmpts      is an array of points on the umbral or penumbral
               terminator of the ellipsoid, as specified by the
               input argument `trmtyp'. The ith point is contained in
               the array elements

                   trmpts[i][j],  j = 0, 1, 2

               Each terminator point is the point of tangency of a
               plane that is also tangent to the light source. These
               associated points of tangency on the light source have
               uniform distribution in longitude when expressed in a
               cylindrical coordinate system whose Z-axis is the target
               center to source center vector. The magnitude of the
               separation in longitude between the tangency points on
               the light source is

                  2*pi / npts

               If the target is spherical, the terminator points
               also are uniformly distributed in longitude in the
               cylindrical system described above. If the target is
               non-spherical, the longitude distribution of the
               points generally is not uniform.

               The terminator points are expressed in the body-fixed
               reference frame designated by `fixref'. Units are km.

-Parameters

   None.

-Exceptions

   1)  If the input frame name `fixref' cannot be mapped
       to a frame ID code, the error SPICE(NOTRANSLATION) is
       signaled by a routine in the call tree of this routine.

   2)  If the target name `target' cannot be mapped
       to a body ID code, the error SPICE(NOTRANSLATION) is
       signaled by a routine in the call tree of this routine.

   3)  If the frame designated by `fixref' is not centered
       on the target, the error SPICE(INVALIDFIXREF) is
       signaled by a routine in the call tree of this routine.

   4)  If the terminator type is not recognized, an error
       is signaled by a routine in the call tree of
       this routine.

   5)  If the terminator point count `npts' is not at least 1, an error
       is signaled by a routine in the call tree of this routine.

   6)  If the light source has non-positive radius, an error
       is signaled by a routine in the call tree of
       this routine.

   7)  If the light source intersects the smallest sphere centered at
       the origin and containing the ellipsoid, an error is signaled
       by a routine in the call tree of this routine.

   8)  If radii for the target body or light source are not
       available in the kernel pool, an error is signaled by
       a routine in the call tree of this routine.

   9)  If radii are available but either body does not have three
       radii, an error is signaled by a routine in the call tree of
       this routine.

   10) If any of the radii is less-than or equal to zero, an error is
       signaled by a routine in the call tree of this routine.

   11) If any SPK look-up fails, an error is signaled by
       a routine in the call tree of this routine.

   12) If any of the `trmtyp', `source', `target', `fixref', `abcorr'
       or `obsrvr' input string pointers is null, the error
       SPICE(NULLPOINTER) is signaled.

   13) If any of the `trmtyp', `source', `target', `fixref', `abcorr'
       or `obsrvr' input strings has zero length, the error
       SPICE(EMPTYSTRING) is signaled.

   14) If any of the `obspos' or `trmpts' output array pointers is
       null, the error SPICE(NULLPOINTER) is signaled.

-Files

   Appropriate SPK, PCK, and frame kernels must be loaded by the
   calling program before this routine is called.

   The following data are required:

   -  SPK data: ephemeris data for the target, observer, and light
      source must be loaded. If aberration corrections are used, the
      states of all three objects relative to the solar system
      barycenter must be calculable from the available ephemeris
      data. Typically ephemeris data are made available by loading
      one or more SPK files via furnsh_c.

   -  PCK data: triaxial radii for the target body and the light
      source must be loaded into the kernel pool. Typically this is
      done by loading a text PCK file via furnsh_c.

   -  Further PCK data: rotation data for the target body must be
      loaded. These may be provided in a text or binary PCK file.

   -  Frame data: if a frame definition is required to convert the
      observer and target states to the target body-fixed frame
      designated by `fixref', that definition must be available in
      the kernel pool. Typically the definitions of frames not
      already built-in to SPICE are supplied by loading a frame
      kernel.

   In all cases, kernel data are normally loaded once per program
   run, NOT every time this routine is called.

-Particulars

   This routine models the boundaries of shadow regions on an
   ellipsoidal target body "illuminated" by a spherical light
   source. Light rays are assumed to travel along straight lines;
   refraction is not modeled.

   Points on the target body's surface are classified according to
   their illumination as follows:

   -  A target surface point X for which no vector from X to any
      point in the light source intersects the target, except at
      X, is considered to be "completely illuminated."

   -  A target surface point X for which each vector from X to a
      point in the light source intersects the target at points
      other than X is considered to be "in total shadow."

   -  All other target points are considered to be in partial
      shadow.

   In this routine, we use the term "umbral terminator" to denote
   the curve usually called the "terminator": this curve is the
   boundary of the portion of the target body's surface that lies in
   total shadow. We use the term "penumbral terminator" to denote
   the boundary of the completely illuminated portion of the
   surface.

   In general, the terminator on an ellipsoid is a more complicated
   curve than the limb (which is always an ellipse). Aside from
   various special cases, the terminator does not lie in a plane.

   However, the condition for a point X on the ellipsoid to lie on
   the terminator is simple: a plane tangent to the ellipsoid at X
   must also be tangent to the light source. If this tangent plane
   does not intersect the vector from the center of the ellipsoid to
   the center of the light source, then X lies on the umbral
   terminator; otherwise X lies on the penumbral terminator.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Compute sets of umbral and penumbral terminator points on the
      Moon. Perform a consistency check using the solar incidence
      angle at each point. We expect to see a solar incidence angle of
      approximately 90 degrees. Since the solar incidence angle is
      measured between the local outward normal and the direction to
      the center of the Sun, the solar incidence angle at an umbral
      terminator point should exceed 90 degrees by approximately the
      angular radius of the Sun, while the angle at a penumbral
      terminator points should be less than 90 degrees by that amount.

      Use the meta-kernel shown below to load the required SPICE
      kernels.


         KPL/MK

         File name: edterm_ex1.tm

         This meta-kernel is intended to support operation of SPICE
         example programs. The kernels shown here should not be
         assumed to contain adequate or correct versions of data
         required by SPICE-based user applications.

         In order for an application to use this meta-kernel, the
         kernels referenced here must be present in the user's
         current working directory.

         The names and contents of the kernels referenced
         by this meta-kernel are as follows:

            File name                     Contents
            ---------                     --------
            de421.bsp                     Planetary ephemeris
            pck00010.tpc                  Planet orientation and
                                          radii
            naif0010.tls                  Leapseconds


         \begindata

            KERNELS_TO_LOAD = ( 'de421.bsp',
                                'pck00010.tpc',
                                'naif0010.tls'  )

         \begintext

         End of meta-kernel


      Example code begins here.


      /.
         Program edterm_ex1
      ./

      #include <stdio.h>
      #include <math.h>
      #include "SpiceUsr.h"


      int main()
      {
         /.
         Local constants
         ./

         #define META            "edterm_ex1.tm"
         #define NPTS            3


         /.
         Local variables
         ./
         SpiceBoolean            first = SPICETRUE;

         SpiceChar             * abcorr;
         SpiceChar             * fixref;
         SpiceChar             * obsrvr;
         SpiceChar             * source;
         SpiceChar             * target;
         SpiceChar             * trmtyps [2] = { "UMBRAL",
                                                 "PENUMBRAL" };
         SpiceChar             * utc;

         SpiceDouble             angrad;
         SpiceDouble             emissn;
         SpiceDouble             et;
         SpiceDouble             lat;
         SpiceDouble             lon;
         SpiceDouble             lt;
         SpiceDouble             obspos  [3];
         SpiceDouble             phase;
         SpiceDouble             radius;
         SpiceDouble             s       [2] = { -1.0,  1.0};

         SpiceDouble             solar;
         SpiceDouble             srcpos  [3];
         SpiceDouble             srcrad  [3];
         SpiceDouble             srfvec  [3];
         SpiceDouble             trgepc;
         SpiceDouble             trmpts  [NPTS][3];

         SpiceInt                i;
         SpiceInt                n;
         SpiceInt                trmidx;


         /.
         Load meta-kernel.
         ./
         furnsh_c ( META );

         /.
         Set observation time.
         ./
         utc    = "2007 FEB 3 00:00:00.000";

         str2et_c ( utc, &et );

         /.
         Set participating objects, frame, and aberration
         corrections.
         ./
         obsrvr = "EARTH";
         target = "MOON";
         source = "SUN";
         fixref = "IAU_MOON";
         abcorr = "LT+S";

         /.
         Look up the radii of the sun.
         ./
         bodvrd_c ( source, "RADII", 3, &n, srcrad );

         /.
         Compute terminator points.
         ./
         for ( trmidx = 0;  trmidx < 2;  trmidx++  )
         {
            edterm_c ( trmtyps[trmidx],  source,  target,
                       et,               fixref,  abcorr,
                       obsrvr,           NPTS,    &trgepc,
                       obspos,           trmpts            );
            /.
            Validate terminator points.

            Look up the target-sun vector at the light-time
            corrected target epoch.
            ./
            if ( first )
            {
               spkpos_c ( source, trgepc, fixref,
                          abcorr, target, srcpos, &lt );

               first = SPICEFALSE;
            }


            printf ( "\n"
                     " Terminator type: %s\n", trmtyps[trmidx] );

            for ( i = 0;  i < NPTS;  i++ )
            {
               /.
               Convert the ith terminator point to latitudinal
               coordinates. Display the point.
               ./
               reclat_c ( trmpts[i], &radius, &lon, &lat );

               printf ( "\n"
                        "   Terminator point %d:\n"
                        "     Radius                     (km):  %18.9f\n"
                        "     Planetocentric longitude   (deg): %18.9f\n"
                        "     Planetocentric latitude    (deg): %18.9f\n",
                        (int)i,
                        radius,
                        lon * dpr_c(),
                        lat * dpr_c()                            );

               /.
               Find the illumination angles at the
               ith terminator point.
               ./
               ilumin_c  ( "Ellipsoid", target,  et,         fixref,
                           abcorr,      obsrvr,  trmpts[i],  &trgepc,
                           srfvec,      &phase,  &solar,     &emissn  );

               printf ( "     Solar incidence angle      (deg): %18.9f\n",
                        solar * dpr_c()                                   );

               /.
               Find the angular radius of the Sun as seen from
               the terminator point.
               ./
               angrad = asin (   srcrad[0]
                               / vdist_c ( srcpos, trmpts[i] )  );

               /.
               Display the solar incidence angle after
               adjusting the angular radius of the Sun
               as seen from the terminator point.The
               result should be approximately 90 degrees.
               ./
               printf ( "     Solar incidence angle adjusted for\n"
                        "     sun's angular radius (deg):       %18.9f\n",
                        ( solar + ( s[trmidx]*angrad ) ) * dpr_c()        );
            }
         }

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


       Terminator type: UMBRAL

         Terminator point 0:
           Radius                     (km):      1737.400000000
           Planetocentric longitude   (deg):      -95.084552819
           Planetocentric latitude    (deg):        0.004052763
           Solar incidence angle      (deg):       90.269765815
           Solar incidence angle adjusted for
           sun's angular radius (deg):             90.000000125

         Terminator point 1:
           Radius                     (km):      1737.400000000
           Planetocentric longitude   (deg):       84.228091534
           Planetocentric latitude    (deg):       59.995755519
           Solar incidence angle      (deg):       90.269765709
           Solar incidence angle adjusted for
           sun's angular radius (deg):             90.000000019

         Terminator point 2:
           Radius                     (km):      1737.400000000
           Planetocentric longitude   (deg):       87.216417974
           Planetocentric latitude    (deg):      -59.979550515
           Solar incidence angle      (deg):       90.269765733
           Solar incidence angle adjusted for
           sun's angular radius (deg):             90.000000043

       Terminator type: PENUMBRAL

         Terminator point 0:
           Radius                     (km):      1737.400000000
           Planetocentric longitude   (deg):       84.914100511
           Planetocentric latitude    (deg):       -0.004073047
           Solar incidence angle      (deg):       89.730234402
           Solar incidence angle adjusted for
           sun's angular radius (deg):             90.000000122

         Terminator point 1:
           Radius                     (km):      1737.400000000
           Planetocentric longitude   (deg):      -95.769215814
           Planetocentric latitude    (deg):      -59.995785101
           Solar incidence angle      (deg):       89.730234301
           Solar incidence angle adjusted for
           sun's angular radius (deg):             90.000000021

         Terminator point 2:
           Radius                     (km):      1737.400000000
           Planetocentric longitude   (deg):      -92.780892017
           Planetocentric latitude    (deg):       59.979498997
           Solar incidence angle      (deg):       89.730234325
           Solar incidence angle adjusted for
           sun's angular radius (deg):             90.000000044


-Restrictions

   1)  This routine models light paths as straight lines.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   E.D. Wright         (JPL)

-Version

   -CSPICE Version 1.0.2, 01-NOV-2021 (JDR)

       Edited the header to comply with NAIF standard.

   -CSPICE Version 1.0.1, 12-JUL-2016 (EDW)

       Edit to example program to use "%d" with explicit casts
       to int for printing SpiceInts with printf.

   -CSPICE Version 1.0.0, 13-JUN-2012 (NJB) (EDW)

-Index_Entries

   find terminator on ellipsoid
   find umbral terminator on ellipsoid
   find penumbral terminator on ellipsoid

-&
*/

{ /* Begin edterm_c */

   /*
   Participate in error tracing.
   */
   chkin_c ( "edterm_c" );

   /*
   Check input string pointers and lengths.
   */
   CHKFSTR ( CHK_STANDARD, "edterm_c", trmtyp );
   CHKFSTR ( CHK_STANDARD, "edterm_c", source );
   CHKFSTR ( CHK_STANDARD, "edterm_c", target );
   CHKFSTR ( CHK_STANDARD, "edterm_c", fixref );
   CHKFSTR ( CHK_STANDARD, "edterm_c", abcorr );
   CHKFSTR ( CHK_STANDARD, "edterm_c", obsrvr );

   /*
   Check output array pointers.
   */
   CHKPTR  ( CHK_STANDARD, "edterm_c", obspos );
   CHKPTR  ( CHK_STANDARD, "edterm_c", trmpts );


   edterm_ ( ( char       * ) trmtyp,
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
             ( ftnlen       ) strlen(obsrvr)  );

   chkout_c ( "edterm_c" );

} /* End edterm_c */
