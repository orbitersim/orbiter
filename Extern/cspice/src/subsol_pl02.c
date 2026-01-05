/*

-Procedure subsol_pl02 ( Sub-solar point using DSK type 2 plate model )

-Abstract

   Deprecated: This routine has been superseded by the CSPICE routine
   subslr_c. This routine is supported for purposes of backward
   compatibility only.

   Compute the rectangular coordinates of the sub-solar point on a
   target body at a particular epoch, optionally corrected for light
   time and stellar aberration.  The target body's surface is
   represented by a triangular plate model contained in a type 2 DSK
   segment. Return the sub-solar point's coordinates expressed in the
   body-fixed frame associated with the target body.  Also, return the
   observer's distance from the sub-solar point.

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

   GEOMETRY

*/
   #include <string.h>
   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZmc.h"
   #include "SpiceZst.h"


   void subsol_pl02 ( SpiceInt               handle,
                      ConstSpiceDLADescr   * dladsc,
                      ConstSpiceChar       * method,
                      ConstSpiceChar       * target,
                      SpiceDouble            et,
                      ConstSpiceChar       * abcorr,
                      ConstSpiceChar       * obsrvr,
                      SpiceDouble            spoint [3],
                      SpiceDouble          * dist,
                      SpiceInt             * plateID     )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   handle     I   DSK handle.
   dladsc     I   DLA descriptor of target body segment.
   method     I   Computation method.
   target     I   Name of target body.
   et         I   Epoch in ephemeris seconds past J2000 TDB.
   abcorr     I   Aberration correction.
   obsrvr     I   Name of observing body.
   spoint     O   Sub-solar point on the target body.
   dist       O   Distance of observer from sub-solar point.
   plateID    O   DSK plate ID of sub-solar point.

-Detailed_Input

   handle      is the DAS file handle of a DSK file open for read
               access.  This kernel must contain a type 2 segment that
               provides a plate model representing the entire surface
               of the target body.


   dladsc      is the DLA descriptor of a DSK segment representing the
               surface of the target body.


   method      is a short string specifying the computation method
               to be used.  The choices are:

                  "Intercept"        The sub-solar point is defined as
                                     the plate model surface intercept
                                     of the ray starting at the Sun and
                                     passing through the target's
                                     center.

                  "Ellipsoid
                   near point"       The sub-solar point is defined as
                                     the plate model surface intercept
                                     of the ray starting at the Sun and
                                     passing through the nearest point
                                     to the observer on a reference
                                     ellipsoid associated with the
                                     target body.

                                     This option requires that the
                                     reference ellipsoid's radii be
                                     available in the kernel pool.

               Neither case nor white space are significant in the
               string "method".  For example, the string

                  "  ellipsoidNEARPOINT"

               is valid.


   target      is the name of the target body.  `target' is
               case-insensitive, and leading and trailing blanks in
               `target' are not significant. Optionally, you may supply
               a string containing the integer ID code for the object.
               For example both "MOON" and "301" are legitimate strings
               that indicate the moon is the target body.

               This routine assumes that the target body's surface is
               represented by a plate model, and that a DSK file
               containing the plate model has been loaded via dasopr_c.


   et          is the epoch, represented  as seconds past J2000 TDB, at
               which the sub-solar point on the target body is to be
               computed.  When aberration corrections are used, `et'
               refers to the epoch at which radiation is received at
               the observer.


   abcorr      indicates the aberration corrections to be applied to
               the position and orientation of the target body and the
               position of the Sun to account for one-way light time
               and stellar aberration.  See the discussion in the
               Particulars section for recommendations on how to choose
               aberration corrections.

               `abcorr' may be any of the following:

                  "NONE"     Apply no correction.  Use the geometric
                             positions of the Sun and target body
                             relative to the observer; evaluate the
                             target body's orientation at `et'.

               The following values of `abcorr' apply to the
               "reception" case in which photons depart from the
               target's location at the light-time corrected epoch
               et-lt and *arrive* at the observer's location at
               `et':

                  "LT"       Correct for one-way light time (also
                             called "planetary aberration") using a
                             Newtonian formulation. This correction
                             uses the position and orientation of the
                             target at the moment it emitted photons
                             arriving at the observer at `et'.  The
                             position of the Sun relative to the
                             target is corrected for the one-way light
                             time from the Sun to the target.

                             The light time correction uses an
                             iterative solution of the light time
                             equation (see Particulars for details).
                             The solution invoked by the "LT" option
                             uses one iteration.

                  "LT+S"     Correct for one-way light time and stellar
                             aberration using a Newtonian formulation.
                             This option modifies the positions
                             obtained with the "LT" option to account
                             for the observer's velocity relative to
                             the solar system barycenter (note the
                             target plays the role of "observer" in the
                             computation of the aberration-corrected
                             target-Sun vector). The result is the
                             sub-solar point computed using apparent
                             position and orientation of the target as
                             seen by the observer and the apparent
                             position of the Sun as seen by the target.

                  "CN"       Converged Newtonian light time correction.
                             In solving the light time equation, the
                             "CN" correction iterates until the
                             solution converges (three iterations on
                             all supported platforms).

                             The "CN" correction typically does not
                             substantially improve accuracy because the
                             errors made by ignoring relativistic
                             effects may be larger than the improvement
                             afforded by obtaining convergence of the
                             light time solution. The "CN" correction
                             computation also requires a significantly
                             greater number of CPU cycles than does the
                             one-iteration light time correction.

                  "CN+S"     Converged Newtonian light time
                             and stellar aberration corrections.


   obsrvr      is the name of the observing body.  This is typically a
               spacecraft, the earth, or a surface point on the earth.
               `obsrvr' is case-insensitive, and leading and trailing
               blanks in `obsrvr' are not significant. Optionally, you
               may supply a string containing the integer ID code for
               the object.  For example both "EARTH" and "399" are
               legitimate strings that indicate the earth is the
               observer.

-Detailed_Output

   spoint      is the sub-solar point on the target body expressed
               relative to the body-fixed reference frame of the target
               body.

               The definition of sub-solar point depends on the
               selected computation method.  See the description of the
               input argument `method' for details.

               The target body-fixed frame, which is time-dependent, is
               evaluated at `et' if `abcorr' is "NONE"; otherwise the
               frame is evaluated at et-lt, where `lt' is the one-way
               light time from target to observer.

               The position and orientation of the target body and the
               position of the Sun are corrected for aberration as
               specified by `abcorr'; the corrected positions and
               orientation are used in the computation of `spoint'.


   dist        is the distance between the observer and the sub-solar
               point.  The observer is presumed to be outside the
               target body, so `dist' is always non-negative.


   plateID     is the integer ID code of the plate on which the
               sub-solar point is located.  This ID code can be
               use to look up data associated with the plate, such
               as the plate's vertices or outward normal vector.

-Parameters

   None.

-Exceptions

   If any of the listed errors occur, the output arguments are
   left unchanged.

   1)  If the input argument `method' is not recognized, the error
       SPICE(DUBIOUSMETHOD) is signaled.

   2)  If either of the input body names `target' or `obsrvr' cannot be
       mapped to NAIF integer codes, the error SPICE(IDCODENOTFOUND)
       is signaled.

   3)  If `obsrvr' and `target' map to the same NAIF integer ID codes, the
       error SPICE(BODIESNOTDISTINCT) is signaled.

   4)  If frame definition data enabling the evaluation of the state
       of the target relative to the observer in target body-fixed
       coordinates have not been loaded prior to calling subsol_pl02,
       an error is signaled by a routine in the call tree of this
       routine.

   5)  If the specified aberration correction is not recognized, an
       error is signaled by a routine in the call tree of this
       routine.

   6)  If insufficient ephemeris data have been loaded prior to
       calling subsol_pl02, an error is signaled by a
       routine in the call tree of this routine.

   7)  If a DSK providing a DSK type 2 plate model has not been
       loaded prior to calling subsol_pl02, an error is signaled by a
       routine in the call tree of this routine.

   8)  If the computation method is "near point" and radii of the
       target body have not been loaded into the kernel pool, an
       error is signaled by a routine in the call tree of this
       routine.

   9)  If PCK data supplying a rotation model for the target body
       have not been loaded prior to calling subsol_pl02, an error is
       signaled by a routine in the call tree of this routine.

   10) If the segment associated with the input DLA descriptor does not
       contain data for the designated target, the error
       SPICE(TARGETMISMATCH) is signaled.

   11) If the segment associated with the input DLA descriptor is not
       of data type 2, the error SPICE(WRONGDATATYPE) is signaled.

   12) If the sub-solar point cannot be computed because the ray from
       the observer to the aim point designated by `method' fails to
       intersect the target surface as defined by the plate model, the
       error SPICE(NOINTERCEPT) is signaled.

   13) Use of transmission-style aberration corrections is not
       permitted.  If abcorr specified such a correction, the
       error SPICE(NOTSUPPORTED) is signaled.

   14) The observer is presumed to be outside the target body; no
       checks are made to verify this.

   15) If any input string pointer is null, the error SPICE(NULLPOINTER)
       is signaled.

   16) If any input string has length zero, the error SPICE(EMPTYSTRING)
       is signaled.

-Files

   Appropriate DSK, SPK, PCK, and frame data must be available to
   the calling program before this routine is called.  Typically
   the data are made available by loading kernels; however the
   data may be supplied via subroutine interfaces if applicable.

   The following data are required:

   -  DSK data:  a DSK file containing a plate model representing the
      target body's surface must be loaded. This kernel must contain
      a type 2 segment that contains data for the entire surface of
      the target body.

   -  SPK data:  ephemeris data for target, observer, and Sun must be
      loaded.  If aberration corrections are used, the states of
      target and observer relative to the solar system barycenter
      must be calculable from the available ephemeris data. Typically
      ephemeris data are made available by loading one or more SPK
      files via furnsh_c.

   -  PCK data:  triaxial radii for the target body must be loaded
      into the kernel pool if the "Near Point" method is selected.
      Typically these data are made available by loading a text PCK
      file via furnsh_c.

   -  Further PCK data:  rotation data for the target body must
      be loaded.  These may be provided in a text or binary PCK file.
      Either type of file may be loaded via furnsh_c.

   -  Frame data:  if a frame definition is required to convert
      the observer and target states to the body-fixed frame of
      the target, that definition must be available in the kernel
      pool.  Typically the definition is supplied by loading a
      frame kernel via furnsh_c.

   In all cases, kernel data are normally loaded once per program
   run, NOT every time this routine is called.

-Particulars

   subsol_pl02 computes the sub-solar point on a target body.
   subsol_pl02 also determines the distance of the observer from the
   sub-solar point.

   Sub-point Definitions
   =====================

   This routine offers two ways of defining the sub-solar point:

      - The "intercept" method. In general, this definition
        calls for defining a ray emanating from the Sun and
        passing through the center of the target body.  The intercept
        on the first plate (the one closest to the observer) hit by this
        ray is the sub-point.

      - The "ellipsoid near point" method.  When a target's surface is
        modeled by a set of triangular plates, the notion of "dropping
        a perpendicular segment to the surface," which makes sense for
        convex surfaces, becomes problematic:  there need not be any
        plate whose normal vector is parallel to a segment from the Sun
        to some point on that plate, or there could be more than one
        such plate.  If such a plate exists, it might be located
        anywhere on the visible surface---not necessarily "below" the
        Sun.

        To work around these problems, the ellipsoid near point method
        uses a reference ellipsoid to define a preliminary sub-solar
        point: this is the unique point on the ellipsoid's surface at
        which the outward surface normal points toward the Sun.  Then
        the plate model sub-solar point is defined as the plate
        intercept closest to the Sun of a ray emanating from the Sun
        and passing through the preliminary sub-solar point on the
        ellipsoid.

   For a large target such as Mars, or for any target whose reference
   ellipsoid deviates significantly from spherical, the results
   obtained using the two sub-point definitions can be quite different.
   The example program provided below demonstrates this fact; Phobos is
   the target body in this case.  Some analysis on the user's part will
   be needed to select the "best" definition for a given application.

   When comparing sub-solar point computations with results from
   sources other than SPICE, it's essential to make sure the same
   geometric definitions are used.


   Aberration Corrections
   ======================

   Below, we indicate the aberration corrections to use for some
   common applications:

      1) Compute the sub-solar point using the apparent direction
         and orientation of a target. This is the most common case for
         a remote-sensing observation.  When the observer's altitude
         is more than one target radius above the surface:

            Use "LT+S":  apply both light time and stellar
            aberration corrections.

         Note that when the observer is close to the target surface,
         this choice may yield inaccurate results, since light time is
         measured between the observer and the target center.  When the
         observer has altitude of less than one target radius above the
         surface, aberration corrections should be omitted, so in this
         case abcorr should be set to:

            "NONE"

         Note that this selection calls for using the geometric position
         of the Sun.


      2) Use a geometric position vector and uncorrected target
         orientation as low-accuracy estimates for an application where
         execution speed is critical.

            Use "NONE".

   See the header of the CSPICE routine spkezr_c for a detailed
   discussion of aberration corrections.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as input,
   the compiler and supporting libraries, and the machine specific
   arithmetic implementation.

   1) Find the sub-solar point on Phobos as seen from Earth for a
      specified sequence of times. Perform the computation twice,
      using both the "intercept" and "ellipsoid near point"
      options. Compute the corresponding sub-solar point values
      using an ellipsoidal surface for comparison.


      Use the meta-kernel shown below to load the required SPICE
      kernels.


         KPL/MK

         File: subsol_pl02_ex1.tm

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
         Program subsol_pl02_ex1
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
         #define  NCORR          2
         #define  NSAMP          3
         #define  NMETHOD        2
         #define  TOL            ( 1.e-12 )
         #define  CORLEN         15
         #define  METHLEN        81
         #define  TIMLEN         41

         /.
         Local variables
         ./
         SpiceBoolean            found;

         SpiceChar             * abcorr;
         SpiceChar             * abcorrs[ NCORR ]   =
                                 {
                                    "NONE",
                                    "LT+S"
                                 };

         SpiceChar               dsk     [ FILSIZ  ];

         SpiceChar             * emethod;
         SpiceChar             * emethods[ NMETHOD ] =
                                 {
                                    "Intercept",
                                    "Near point"
                                 };

         SpiceChar               meta    [ FILSIZ  ];

         SpiceChar             * method;
         SpiceChar             * methods [ NMETHOD ] =
                                 {
                                    "Intercept",
                                    "Ellipsoid near point"
                                 };

         SpiceChar             * obsrvr = "Earth";
         SpiceChar             * target = "Phobos";
         SpiceChar               timstr [ TIMLEN ];

         SpiceDLADescr           dladsc;

         SpiceDouble             dist;
         SpiceDouble             elat;
         SpiceDouble             elon;
         SpiceDouble             erad;
         SpiceDouble             et0;
         SpiceDouble             et;
         SpiceDouble             f;
         SpiceDouble             radii  [3];
         SpiceDouble             re;
         SpiceDouble             rp;
         SpiceDouble             stepsize ;
         SpiceDouble             xlat;
         SpiceDouble             xlon;
         SpiceDouble             xpt    [3];
         SpiceDouble             xr;

         SpiceInt                coridx;
         SpiceInt                handle;
         SpiceInt                i;
         SpiceInt                midx;
         SpiceInt                n;
         SpiceInt                plid;

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
         Look up radii of the target; compute flattening
         coefficient.
         ./
         bodvrd_c ( target, "RADII", 3, &n, radii );

         re = radii[0];
         rp = radii[2];
         f  = ( re - rp ) / re;

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

         Now compute sub-solar points using both computation
         methods.  We'll vary the aberration corrections
         and the epochs.
         ./
         et0      = 0.0;
         stepsize = 1.e6;

         for ( i = 0;  i < NSAMP;  i++  )
         {
            /.
            Set the computation time for the ith
            sample.
            ./
            et = et0 + i*stepsize;

            timout_c ( et,
                       "YYYY-MON-DD "
                       "HR:MN:SC.### ::TDB(TDB)",
                       TIMLEN,
                       timstr                    );

            printf ( "\n\nObservation epoch:  %s\n",
                     timstr                      );

            for ( coridx = 0;  coridx < NCORR;  coridx++  )
            {
               /.
               Select the aberration correction.
               ./
               abcorr = abcorrs[coridx];
               printf ( "\n"
                        "   abcorr = %s\n", abcorr  );

               for ( midx = 0;  midx < NMETHOD;  midx++  )
               {
                  /.
                  Select the computation method.
                  ./
                  method  = methods [midx];
                  emethod = emethods[midx];

                  printf (  "\n"
                            "       Method = %s\n", method  );

                  /.
                  Compute the sub-solar point using a plate model
                  representation of the target's surface.
                  ./
                  subsol_pl02 ( handle, &dladsc, method,
                                target, et,      abcorr,
                                obsrvr, xpt,     &dist,   &plid );

                  /.
                  Represent the intercept in latitudinal
                  coordinates.
                  ./
                  reclat_c ( xpt, &xr, &xlon, &xlat );

                  printf (
                  "         Sub-solar point on plate model surface:\n"
                  "             Planetocentric Longitude (deg):  %f\n"
                  "             Planetocentric Latitude  (deg):  %f\n"
                  "             Radius                    (km):  %f\n"
                  "             Observer distance         (km):  %f\n",
                  xlon * dpr_c(),
                  xlat * dpr_c(),
                  xr,
                  dist                                                 );

                  /.
                  Compute the sub-solar point using an ellipsoidal
                  representation of the target's surface.  (The
                  routine subsol_c doesn't return distance.)
                  ./
                  subsol_c ( emethod, target, et,  abcorr,
                             obsrvr,  xpt                 );

                  /.
                  Represent the intercept in latitudinal
                  coordinates.
                  ./
                  reclat_c ( xpt, &erad, &elon, &elat );

                  printf (
                  "         Sub-solar point on ellipsoid surface:\n"
                  "             Planetocentric Longitude (deg):  %f\n"
                  "             Planetocentric Latitude  (deg):  %f\n"
                  "             Radius                    (km):  %f\n",
                  elon * dpr_c(),
                  elat * dpr_c(),
                  erad                                                 );
               }
            }
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
      platform, using the meta-kernel file named subsol_pl02_ex1.tm
      and the DSK file named phobos_3_3.bds, the output was:


      Enter meta-kernel name > subsol_pl02_ex1.tm
      Enter DSK name         > phobos_3_3.bds


      Observation epoch:  2000-JAN-01 12:00:00.000 (TDB)

         abcorr = NONE

             Method = Intercept
               Sub-solar point on plate model surface:
                   Planetocentric Longitude (deg):  102.413905
                   Planetocentric Latitude  (deg):  -24.533127
                   Radius                    (km):  11.612325
                   Observer distance         (km):  276700026.580116
               Sub-solar point on ellipsoid surface:
                   Planetocentric Longitude (deg):  102.413905
                   Planetocentric Latitude  (deg):  -24.533127
                   Radius                    (km):  10.922580

             Method = Ellipsoid near point
               Sub-solar point on plate model surface:
                   Planetocentric Longitude (deg):  105.857346
                   Planetocentric Latitude  (deg):  -16.270558
                   Radius                    (km):  11.645162
                   Observer distance         (km):  276700027.058857
               Sub-solar point on ellipsoid surface:
                   Planetocentric Longitude (deg):  105.973365
                   Planetocentric Latitude  (deg):  -15.976232
                   Radius                    (km):  11.249340

         abcorr = LT+S

             Method = Intercept
               Sub-solar point on plate model surface:
                   Planetocentric Longitude (deg):  114.623420
                   Planetocentric Latitude  (deg):  -24.533628
                   Radius                    (km):  11.411417
                   Observer distance         (km):  276710249.413113
               Sub-solar point on ellipsoid surface:
                   Planetocentric Longitude (deg):  114.623420
                   Planetocentric Latitude  (deg):  -24.533628
                   Radius                    (km):  11.046740

             Method = Ellipsoid near point
               Sub-solar point on plate model surface:
                   Planetocentric Longitude (deg):  120.870428
                   Planetocentric Latitude  (deg):  -15.247903
                   Radius                    (km):  11.350346
                   Observer distance         (km):  276710250.304809
               Sub-solar point on ellipsoid surface:
                   Planetocentric Longitude (deg):  120.795481
                   Planetocentric Latitude  (deg):  -15.366726
                   Radius                    (km):  11.494153


      Observation epoch:  2000-JAN-13 01:46:40.000 (TDB)

         abcorr = NONE

             Method = Intercept
               Sub-solar point on plate model surface:
                   Planetocentric Longitude (deg):  4.432684
                   Planetocentric Latitude  (deg):  -24.281966
                   Radius                    (km):  12.888491
                   Observer distance         (km):  286106845.772886
               Sub-solar point on ellipsoid surface:
                   Planetocentric Longitude (deg):  4.432684
                   Planetocentric Latitude  (deg):  -24.281966
                   Radius                    (km):  11.980161

             Method = Ellipsoid near point
               Sub-solar point on plate model surface:
                   Planetocentric Longitude (deg):  3.418663
                   Planetocentric Latitude  (deg):  -12.568166
                   Radius                    (km):  12.783152
                   Observer distance         (km):  286106846.122051
               Sub-solar point on ellipsoid surface:
                   Planetocentric Longitude (deg):  3.411488
                   Planetocentric Latitude  (deg):  -12.479982
                   Radius                    (km):  12.689005

         abcorr = LT+S

             Method = Intercept
               Sub-solar point on plate model surface:
                   Planetocentric Longitude (deg):  16.838631
                   Planetocentric Latitude  (deg):  -24.282864
                   Radius                    (km):  12.469595
                   Observer distance         (km):  286118910.431431
               Sub-solar point on ellipsoid surface:
                   Planetocentric Longitude (deg):  16.838631
                   Planetocentric Latitude  (deg):  -24.282864
                   Radius                    (km):  11.882348

             Method = Ellipsoid near point
               Sub-solar point on plate model surface:
                   Planetocentric Longitude (deg):  13.101289
                   Planetocentric Latitude  (deg):  -12.676662
                   Radius                    (km):  12.596556
                   Observer distance         (km):  286118910.300721

      [...]


      Warning: incomplete output. Only 100 out of 155 lines have been
      provided.


-Restrictions

   1)  This routine assumes that the origin of the body-fixed reference
       frame associated with the target body is located in the interior
       of that body.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)

-Version

   -CSPICE Version 2.1.0, 26-OCT-2021 (JDR) (NJB)

       Bug fix: call to ljust_ is now followed by call to F2C_ConvertStr.

       Edited the Examples section to comply with NAIF standard.

       Index lines now state that this routine is deprecated.

   -CSPICE Version 2.0.0, 23-JUL-2016 (NJB)

       Bug fix: the DSK segment's surface ID code is no longer
       required to match that of the target. The segment's
       center ID must match.

       Added failed_c calls.

       Include file references have been updated.

   -Beta Version 1.3.0, 30-APR-2014 (NJB) (BVS)

       Adding missing "return" statement after chkout_c call
       in branch for bad input method string.

       Now includes dsk_proto.h.

       Last update was 07-APR-2014 (BVS)

       Changed FRAME to FRAMES in the Required_Reading section.

   -Beta Version 1.2.0, 14-MAY-2010 (NJB)

       Updated for compatibility with new DSK design.

   -Beta Version 1.1.0, 09-FEB-2007 (NJB)

       Bug fix:  type of local variable fDLADescr was changed to SpiceInt.

   -Beta Version 1.0.0, 06-NOV-2006 (NJB)

-Index_Entries

   DEPRECATED sub-solar point

-&
*/

{ /* Begin subsol_pl02 */


   /*
   Prototypes
   */
   int dskgd_(integer *handle, integer *dladsc, doublereal *dskdsc);


   /*
   Local parameters
   */
   #define FRNMLN          33
   #define CTRIDX          1
   #define TYPIDX          3
   #define NEARPT          "Near Point"
   #define CORLEN          16

   /*
   Local variables
   */
   SpiceBoolean            found;

   SpiceChar               frname     [ FRNMLN ];
   SpiceChar               loccor     [ CORLEN ];

   SpiceDouble             ettarg;
   SpiceDouble             fDSKDescr  [ SPICE_DSK_DSCSIZ ];
   SpiceDouble             ltsun;
   SpiceDouble             lttarg;
   SpiceDouble             npt        [3];
   SpiceDouble             npalt;
   SpiceDouble             obspos     [3];
   SpiceDouble             radii      [3];
   SpiceDouble             raydir     [3];
   SpiceDouble             sunpos     [3];
   SpiceDouble             trgpos     [3];

   SpiceInt                centerID;
   SpiceInt                dataType;
   SpiceInt                fDLADescr  [ SPICE_DLA_DSCSIZ ];
   SpiceInt                frcode;
   SpiceInt                n;
   SpiceInt                obscde;
   SpiceInt                trgcde;



   /*
   Participate in error tracing.
   */
   if ( return_c()  )
   {
      return;
   }
   chkin_c ( "subsol_pl02" );

   /*
   Check the input strings: method, target, abcorr, and obsrvr.  Make
   sure none of the pointers are null and that each string contains at
   least one non-null character.
   */
   CHKFSTR ( CHK_STANDARD, "subsol_pl02", method );
   CHKFSTR ( CHK_STANDARD, "subsol_pl02", target );
   CHKFSTR ( CHK_STANDARD, "subsol_pl02", abcorr );
   CHKFSTR ( CHK_STANDARD, "subsol_pl02", obsrvr );

   /*
   Check the aberration correction string:  reject transmission
   corrections.
   */
   ljust_ ( ( char * ) abcorr,
            ( char * ) loccor,
            ( ftnlen ) strlen(abcorr),
            ( ftnlen ) CORLEN-1         );

   F2C_ConvertStr( CORLEN, loccor );

   if (  matchi_c( loccor, "X*", '*', '?' )  )
   {
      setmsg_c ( "Input aberration correction specification # "
                 "calls for transmission-style corrections."    );
      errch_c  ( "#",  abcorr                                   );
      sigerr_c ( "SPICE(NOTSUPPORTED)"                          );
      chkout_c ( "subsol_pl02"                                  );
      return;
   }

   /*
   Obtain integer codes for the target and observer.
   */
   bods2c_c ( target, &trgcde, &found );

   if ( failed_c() )
   {
      chkout_c ( "subsol_pl02" );
      return;
   }

   if ( !found  )
   {
      setmsg_c ( "The target, '#', is not a recognized name for an "
                 "ephemeris object. The cause of this problem may be "
                 "that you need an updated version of the SPICE "
                 "Toolkit."                                            );
      errch_c  ( "#", target                                           );
      sigerr_c ( "SPICE(IDCODENOTFOUND)"                               );
      chkout_c ( "subsol_pl02"                                         );
      return;
   }

   bods2c_c ( obsrvr, &obscde, &found );

   if ( failed_c() )
   {
      chkout_c ( "subsol_pl02" );
      return;
   }

   if ( !found  )
   {
      setmsg_c ( "The observer, '#', is not a recognized name for an "
                 "ephemeris object. The cause of this problem may be "
                 "that you need an updated version of the SPICE "
                 "Toolkit."                                            );
      errch_c  ( "#", obsrvr                                           );
      sigerr_c ( "SPICE(IDCODENOTFOUND)"                               );
      chkout_c ( "subsol_pl02"                                         );
      return;
   }

   /*
   Make sure target and observer don't coincide.
   */
   if ( trgcde == obscde  )
   {
      setmsg_c ( "Both target and observer have the same integer "
                 "ID code #.  These objects must be distinct."     );
      errint_c ( "#", obscde                                       );
      sigerr_c ( "SPICE(BODIESNOTDISTINCT)"                        );
      chkout_c ( "subsol_pl02"                                     );
      return;
   }

   /*
   Get the name of the default body-fixed reference frame associated with
   the target body.
   */
   cidfrm_c ( trgcde, FRNMLN, &frcode, frname, &found );

   if ( failed_c() )
   {
      chkout_c ( "subsol_pl02" );
      return;
   }

   if ( !found  )
   {
      setmsg_c ( "No body-fixed frame is associated with "
                 "target body #; a frame kernel must be "
                 "loaded to make this association.  Consult "
                 "the FRAMES Required Reading for details."   );
      errch_c  ( "#", target                                  );
      sigerr_c ( "SPICE(IDCODENOTFOUND)"                      );
      chkout_c ( "subsol_pl02"                                 );
      return;
   }

   /*
   Fetch and examine the DSK descriptor of the segment from which we're
   getting the shape data.  Make sure this is a type 2 segment for the
   specified target.  Since we don't know a priori that the segment
   has type 2, we can't use the type 2 fetch routine for this operation.
   Instead, we use the f2c'd version of the Fortran routine dskgd.
   We must prepare a Fortran-style DLA descriptor in order to call
   this routine.

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

   /*
   Call the f2c'd "get DSK descriptor" routine.
   */
   dskgd_ ( ( integer    * ) &handle,
            ( integer    * ) fDLADescr,
            ( doublereal * ) fDSKDescr  );

   if ( failed_c() )
   {
      chkout_c ( "subsol_pl02" );
      return;
   }

   /*
   Extract object ID and DSK data type from the descriptor.
   */
   centerID = (SpiceInt)fDSKDescr[CTRIDX];
   dataType = (SpiceInt)fDSKDescr[TYPIDX];

   if ( centerID != trgcde )
   {
      setmsg_c ( "Input segment is for object with integer ID "
                 "code #, which does not match target ID code #."  );
      errint_c ( "#", centerID                                     );
      errint_c ( "#", trgcde                                       );
      sigerr_c ( "SPICE(TARGETMISMATCH)"                           );
      chkout_c ( "subsol_pl02"                                     );
      return;
   }

   if ( dataType != 2 )
   {
      setmsg_c ( "Input segment has DSK data type #.  A segment of "
                 "type 2 is required."                               );
      errint_c ( "#", dataType                                       );
      sigerr_c ( "SPICE(WRONGDATATYPE)"                              );
      chkout_c ( "subsol_pl02"                                       );
      return;
   }


   /*
   We're done with error checks on our inputs.

   Look up the aberration-corrected position of the target's center as
   seen from the observer.  Use the aberration-corrected target
   body-fixed reference frame.  Let `ettarg' the epoch associated with
   the target.
   */
   spkezp_c ( trgcde, et, frname, abcorr, obscde, trgpos, &lttarg );

   if ( failed_c() )
   {
      chkout_c ( "subsol_pl02" );
      return;
   }

   zzcorepc_ ( ( char        * ) abcorr,
               ( doublereal  * ) &et,
               ( doublereal  * ) &lttarg,
               ( doublereal  * ) &ettarg,
               ( ftnlen        ) strlen(abcorr)  );

   /*
   Now find the apparent position of the Sun as seen from the
   target center at `ettarg'.
   */
   spkpos_c ( "Sun", ettarg, frname, abcorr, target, sunpos, &ltsun );

   if ( failed_c() )
   {
      chkout_c ( "subsol_pl02" );
      return;
   }

   /*
   Now we must define a ray from the sun to the preliminary sub-solar
   point, which is either the target's center or the near point to the
   sun on the target's reference ellipsoid.

   In both cases the sun's position is the ray's vertex.

   Determine the ray's vertex and direction vector. The way we define
   this vector depends on our computation method.
   */
   if (  eqstr_c( method, "intercept" )  )
   {
      /*
      The ray points from the Sun towards the target's center.
      */
      vminus_c ( sunpos, raydir );
      vhat_c   ( raydir, raydir );
   }

   else if (  eqstr_c( method, "ellipsoid near point" )  )
   {
      /*
      The ray we wish to use points towards the closest point to the
      observer on the reference ellipsoid.  We must first compute
      this near point.

      Fetch the radii of the reference ellipsoid.
      */
      bodvrd_c ( target, "RADII", 3, &n, radii );

      nearpt_c ( sunpos, radii[0], radii[1], radii[2], npt, &npalt );

      /*
      Check for failure here because unavailability of ellipsoid
      radii is a possibility.
      */
      if ( failed_c() )
      {
         chkout_c ( "subsol_pl02" );
         return;
      }

      /*
      Now we can compute the ray's direction vector.
      */
      vsub_c ( npt,     sunpos, raydir );
      vhat_c ( raydir,          raydir );

   }
   else
   {
      setmsg_c ( "The computation method # was not recognized. "
                 "Allowed values are 'Ellipsoid near point' "
                 "and 'Intercept'."                              );
      errch_c  ( "#",  method                                    );
      sigerr_c ( "SPICE(DUBIOUSMETHOD)"                          );
      chkout_c ( "subsol_pl02"                                   );
      return;
   }

   /*
   Find the surface intercept defined by the vertex, ray direction,
   and surface plate model.  This is the sub-solar point `spoint'.
   */
   dskx02_c ( handle, dladsc, sunpos, raydir, plateID, spoint, &found );

   if ( failed_c() )
   {
      chkout_c ( "subsol_pl02" );
      return;
   }

   if ( !found  )
   {
      setmsg_c ( "Ray from the Sun to the center of target # "
                 "did not intersect the surface defined by "
                 "the input handle and descriptor."           );
      errch_c  ( "#", target                                  );
      sigerr_c ( "SPICE(NOINTERCEPT)"                         );
      chkout_c ( "subsol_pl02"                                );
      return;
   }


   /*
   Find the range from observer to sub-solar point.
   */
   vminus_c ( trgpos, obspos );

   *dist = vdist_c ( spoint, obspos );


   /*
   At this point, `spoint', `dist', and `plateID' are set.
   */


   chkout_c ( "subsol_pl02" );

} /* End subsol_pl02 */
