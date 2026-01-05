/*

-Procedure subpt_pl02 ( Sub-observer point using DSK type 2 plate model )

-Abstract

   Deprecated: This routine has been superseded by the CSPICE routine
   subpnt_c. This routine is supported for purposes of backward
   compatibility only.

   Compute the rectangular coordinates of the sub-observer point on a
   target body at a particular epoch, optionally corrected for light
   time and stellar aberration.  The target body's surface is
   represented by a triangular plate model contained in a type 2 DSK
   segment. Return the sub-observer point's coordinates expressed in the
   body-fixed frame associated with the target body.  Also, return the
   observer's altitude above the target body.

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

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZmc.h"
   #include "SpiceZst.h"


   void subpt_pl02 ( SpiceInt               handle,
                     ConstSpiceDLADescr   * dladsc,
                     ConstSpiceChar       * method,
                     ConstSpiceChar       * target,
                     SpiceDouble            et,
                     ConstSpiceChar       * abcorr,
                     ConstSpiceChar       * obsrvr,
                     SpiceDouble            spoint [3],
                     SpiceDouble          * alt,
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
   spoint     O   Sub-observer point on the target body.
   alt        O   Altitude of the observer above the target body.
   plateID    O   DSK plate ID of sub-point.

-Detailed_Input

   handle      is the DAS file handle of a DSK file open for read
               access.  This kernel must contain a type 2 segment
               that provides a plate model representing the entire
               surface of the target body.


   dladsc      is the DLA descriptor of a DSK segment representing
               the surface of the target body.


   method      is a short string specifying the computation method
               to be used.  The choices are:

                  "Intercept"        The sub-observer point is defined
                                     as the plate model surface
                                     intercept of the ray starting at
                                     the observer and passing through
                                     the target's center.

                  "Ellipsoid
                   near point"       The sub-observer point is defined
                                     as the plate model surface
                                     intercept of the ray starting at
                                     the observer and passing through
                                     the nearest point to the observer
                                     on a reference ellipsoid
                                     associated with the target body.

                                     This option requires that the
                                     reference ellipsoid's radii be
                                     available in the kernel pool.

               For both computation methods, this routine finds a
               sub-point on the same side of the target body as the
               observer.  If the observer is inside the target body,
               the "sub-point" will actually be above the observer.
               In the case of multiple intercepts, the outermost one
               (that is, the one farthest from the target center) is
               selected.

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
               which the sub-observer point on the target body is to be
               computed.  When aberration corrections are used, `et'
               refers to the epoch at which radiation is received at
               the observer.


   abcorr      indicates the aberration corrections to be applied to
               the position and orientation of the target body to
               account for one-way light time and stellar aberration.
               See the discussion in the Particulars section for
               recommendations on how to choose aberration corrections.

               `abcorr' may be any of the following:

                  "NONE"     Apply no correction. Use the geometric
                             position of the target body relative to
                             the observer; evaluate the target body's
                             orientation at `et'.

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
                             arriving at the observer at `et'.

                             The light time correction uses an
                             iterative solution of the light time
                             equation (see Particulars for details).
                             The solution invoked by the "LT" option
                             uses one iteration.

                  "LT+S"     Correct for one-way light time and stellar
                             aberration using a Newtonian formulation.
                             This option modifies the position obtained
                             with the "LT" option to account for the
                             observer's velocity relative to the solar
                             system barycenter. The result is the
                             sub-observer point computed using the
                             apparent position and orientation of the
                             target as seen by the observer.

                  "CN"       Converged Newtonian light time
                             correction.  In solving the light time
                             equation, the "CN" correction iterates
                             until the solution converges (three
                             iterations on all supported platforms).

                             The "CN" correction typically does not
                             substantially improve accuracy because
                             the errors made by ignoring
                             relativistic effects may be larger than
                             the improvement afforded by obtaining
                             convergence of the light time solution.
                             The "CN" correction computation also
                             requires a significantly greater number
                             of CPU cycles than does the
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

   spoint      is the sub-observer point on the target body expressed
               relative to the body-fixed reference frame of the target
               body.

               The definition of sub-observer point depends on the
               selected computation method.  See the description of the
               input argument `method' for details.

               The target body-fixed frame, which is time-dependent, is
               evaluated at `et' if `abcorr' is "NONE"; otherwise the
               frame is evaluated at et-lt, where `lt' is the one-way
               light time from target to observer.

               The position and orientation of the target body are
               corrected for aberration as specified by `abcorr'; the
               corrected position and orientation are used in the
               computation of `spoint'.


   alt         is the signed distance between the observer and the
               sub-point.  When the observer is outside the body
               `alt' is positive; when the observer is inside, `alt'
               is negative.

               Note that `alt' is not truly an `altitude' unless the
               observer-to-sub-point vector happens to be perpendicular
               to the target body's surface at the sub-point.  In
               general this condition should not be expected to hold,
               unless the plate model representation of the target
               body's surface very nearly matches the target body's
               reference ellipsoid and the "ellipsoid near point"
               computation method is selected.



   plateID     is the integer ID code of the plate on which the
               sub-observer point is located.  This ID code can be
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
       of the target relative to the observer in the target
       body-fixed frame have not been loaded prior to calling
       subpt_pl02, an error is signaled by a routine in the call tree
       of this routine.

   5)  If the specified aberration correction is not recognized, an
       error is signaled by a routine in the call tree of this
       routine.

   6)  If insufficient ephemeris data have been loaded prior to
       calling subpt_pl02, an error is signaled by a
       routine in the call tree of this routine.

   7)  If a DSK providing a DSK type 2 plate model has not been
       loaded prior to calling subpt_pl02, an error is signaled by a
       routine in the call tree of this routine.

   8)  If the computation method is "ellipsoid near point" and radii
       of the target body have not been loaded into the kernel pool,
       an error is signaled by a routine in the call tree of this
       routine.

   9)  If PCK data supplying a rotation model for the target body
       have not been loaded prior to calling subpt_pl02, an error is
       signaled by a routine in the call tree of this routine.

   10) If the segment associated with the input DLA descriptor does not
       contain data for the designated target, the error
       SPICE(TARGETMISMATCH) is signaled.

   11) If the segment associated with the input DLA descriptor is not
       of data type 2, the error SPICE(WRONGDATATYPE) is signaled.

   12) If the sub-point cannot be computed because the ray from the
       observer to the aim point designated by `method' fails to
       intersect the target surface as defined by the plate model,
       the error SPICE(NOINTERCEPT) is signaled.

   13) Use of transmission-style aberration corrections is not
       permitted.  If abcorr specified such a correction, the
       error SPICE(NOTSUPPORTED) is signaled.

   14) If any input string pointer is null, the error SPICE(NULLPOINTER)
       is signaled.

   15) If any input string has length zero, the error SPICE(EMPTYSTRING)
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

   -  SPK data:  ephemeris data for target and observer must be
      loaded.  If aberration corrections are used, the states of
      target and observer relative to the solar system barycenter
      must be calculable from the available ephemeris data.
      Typically ephemeris data are made available by loading one
      or more SPK files via furnsh_c.

   -  PCK data:  triaxial radii for the target body must be loaded
      into the kernel pool if the "Ellipsoid Near Point" method is
      selected. Typically these data are made available by loading a
      text PCK file via furnsh_c.

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

   subpt_pl02 computes the sub-observer point (abbreviated as
   "sub-point") on a target body. subpt_pl02 also determines the
   distance from the observer to the sub-observer point.

   Sub-point Definitions
   =====================

   This routine offers two ways of defining the sub-point:

      - The "intercept" method. In general, this definition
        calls for defining a ray emanating from the observer and
        passing through the center of the target body.  The intercept
        on the first plate (the one closest to the observer) hit by this
        ray is the sub-point.

        The details of this definition are a bit more complex, because
        this routine handles the case where the observer is inside the
        target.  In such cases, the sub-point is actually the point
        that would be obtained by scaling up the target center-
        observer vector so as to place the observer outside the target,
        then computing the sub-point in the usual way.  This handling
        of the interior observer case prevents an observer location
        that is slightly below the surface from accidentally being
        associated with a sub-point on the opposite side of the target.
        However, the possibility that the "sub-point" may be "above"
        the observer may seem counterintuitive.

      - The "ellipsoid near point" method.  When a target's surface is
        modeled by a set of triangular plates, the notion of "dropping
        a perpendicular segment to the surface," which makes sense
        for convex surfaces, becomes problematic:  there need not be
        any plate whose normal vector is parallel to a segment from
        the observer to some point on that plate, or there could be
        more than one such plate.  If such a plate exists, it might
        be located anywhere on the visible surface---not necessarily
        "below" the observer.

        To work around these problems, the ellipsoid near point method
        uses a reference ellipsoid to define a preliminary sub-point:
        for an exterior observer, this is the unique point on the
        ellipsoid's surface at which the outward surface normal points
        toward the observer.  Then the plate model sub-point is defined
        as the plate intercept closest to the observer of a ray
        emanating from the observer and passing through the preliminary
        sub-point on the ellipsoid.

   For a large target such as Mars, or for any target whose reference
   ellipsoid deviates significantly from spherical, the results
   obtained using the two sub-point definitions can be quite different.
   The example program provided below demonstrates this fact; Phobos is
   the target body in this case.  Some analysis on the user's part will
   be needed to select the "best" definition for a given application.

   When comparing sub-point computations with results from sources
   other than SPICE, it's essential to make sure the same geometric
   definitions are used.


   Aberration Corrections
   ======================

   Below, we indicate the aberration corrections to use for some
   common applications:

      1) Compute the sub-observer point using the apparent direction
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

   1) Find the sub-observer point on Phobos as seen from Earth for a
      specified sequence of times, using a DSK file to provide
      topographic data. Perform the computation twice,
      using both the "intercept" and "ellipsoid near point"
      options. Compute the corresponding sub-observer point values
      using an ellipsoidal surface for comparison.


      Use the meta-kernel shown below to load the required SPICE
      kernels.


         KPL/MK

         File: subpt_pl02_ex1.tm

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
         Program subpt_pl02_ex1
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

         SpiceDouble             alt;
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
         If we made it this far, `dladsc' is the
         DLA descriptor of the first segment.

         Now compute sub-points using both computation
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
                  Compute the sub-observer point using a plate model
                  representation of the target's surface.
                  ./
                  subpt_pl02 ( handle, &dladsc, method,
                               target, et,      abcorr,
                               obsrvr, xpt,     &alt,   &plid );

                  /.
                  Represent the intercept in latitudinal
                  coordinates.
                  ./
                  reclat_c ( xpt, &xr, &xlon, &xlat );

                  printf (
                  "         Sub-point on plate model surface:\n"
                  "             Planetocentric Longitude (deg):  %f\n"
                  "             Planetocentric Latitude  (deg):  %f\n"
                  "             Radius                    (km):  %f\n"
                  "             Observer altitude         (km):  %f\n"
                  "             ID of intercept plate         :  %d\n",
                  xlon * dpr_c(),
                  xlat * dpr_c(),
                  xr,
                  alt,
                  (int)plid                                          );

                  /.
                  Compute the sub-observer point using an ellipsoidal
                  representation of the target's surface.
                  ./
                  subpt_c ( emethod, target, et,  abcorr,
                            obsrvr,  xpt,    &alt        );

                  /.
                  Represent the intercept in latitudinal
                  coordinates.
                  ./
                  reclat_c ( xpt, &erad, &elon, &elat );

                  printf (
                  "         Sub-point on ellipsoid surface:\n"
                  "             Planetocentric Longitude (deg):  %f\n"
                  "             Planetocentric Latitude  (deg):  %f\n"
                  "             Radius                    (km):  %f\n"
                  "             Observer altitude         (km):  %f\n",
                  elon * dpr_c(),
                  elat * dpr_c(),
                  erad,
                  alt                                                 );
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
      platform, using the meta-kernel file named subpt_pl02_ex1.tm
      and the DSK file named phobos_3_3.bds, the output was:


      Enter meta-kernel name > subpt_pl02_ex1.tm
      Enter DSK name         > phobos_3_3.bds


      Observation epoch:  2000-JAN-01 12:00:00.000 (TDB)

         abcorr = NONE

             Method = Intercept
               Sub-point on plate model surface:
                   Planetocentric Longitude (deg):  68.184433
                   Planetocentric Latitude  (deg):  -22.013938
                   Radius                    (km):  10.904558
                   Observer altitude         (km):  276700025.580291
                   ID of intercept plate         :  154969
               Sub-point on ellipsoid surface:
                   Planetocentric Longitude (deg):  68.184433
                   Planetocentric Latitude  (deg):  -22.013938
                   Radius                    (km):  11.111635
                   Observer altitude         (km):  276700025.373214

             Method = Ellipsoid near point
               Sub-point on plate model surface:
                   Planetocentric Longitude (deg):  62.358996
                   Planetocentric Latitude  (deg):  -13.611694
                   Radius                    (km):  11.189797
                   Observer altitude         (km):  276700025.467226
                   ID of intercept plate         :  182987
               Sub-point on ellipsoid surface:
                   Planetocentric Longitude (deg):  62.501544
                   Planetocentric Latitude  (deg):  -13.828255
                   Radius                    (km):  11.480112
                   Observer altitude         (km):  276700025.172491

         abcorr = LT+S

             Method = Intercept
               Sub-point on plate model surface:
                   Planetocentric Longitude (deg):  80.397650
                   Planetocentric Latitude  (deg):  -22.012145
                   Radius                    (km):  11.102641
                   Observer altitude         (km):  276710248.044155
                   ID of intercept plate         :  161027
               Sub-point on ellipsoid surface:
                   Planetocentric Longitude (deg):  80.397650
                   Planetocentric Latitude  (deg):  -22.012145
                   Radius                    (km):  10.997901
                   Observer altitude         (km):  276710248.148895

             Method = Ellipsoid near point
               Sub-point on plate model surface:
                   Planetocentric Longitude (deg):  77.596717
                   Planetocentric Latitude  (deg):  -14.326159
                   Radius                    (km):  11.278719
                   Observer altitude         (km):  276710247.981509
                   ID of intercept plate         :  184540
               Sub-point on ellipsoid surface:
                   Planetocentric Longitude (deg):  77.592522
                   Planetocentric Latitude  (deg):  -14.314136
                   Radius                    (km):  11.261264
                   Observer altitude         (km):  276710247.999143


      Observation epoch:  2000-JAN-13 01:46:40.000 (TDB)

         abcorr = NONE

             Method = Intercept
               Sub-point on plate model surface:
                   Planetocentric Longitude (deg):  -28.168808
                   Planetocentric Latitude  (deg):  -23.838824
                   Radius                    (km):  12.939782
                   Observer altitude         (km):  286106844.028053
                   ID of intercept plate         :  105656
               Sub-point on ellipsoid surface:
                   Planetocentric Longitude (deg):  -28.168808
                   Planetocentric Latitude  (deg):  -23.838824
                   Radius                    (km):  11.740687
                   Observer altitude         (km):  286106845.227148

             Method = Ellipsoid near point
               Sub-point on plate model surface:
                   Planetocentric Longitude (deg):  -22.941997
                   Planetocentric Latitude  (deg):  -13.930327
                   Radius                    (km):  13.832136
                   Observer altitude         (km):  286106843.393083
                   ID of intercept plate         :  135034
               Sub-point on ellipsoid surface:
                   Planetocentric Longitude (deg):  -22.381480
                   Planetocentric Latitude  (deg):  -12.794856
                   Radius                    (km):  12.437687
                   Observer altitude         (km):  286106844.817034

         abcorr = LT+S

             Method = Intercept
               Sub-point on plate model surface:
                   Planetocentric Longitude (deg):  -15.758498
                   Planetocentric Latitude  (deg):  -23.837297
                   Radius                    (km):  13.159728

      [...]


      Warning: incomplete output. Only 100 out of 179 lines have been
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

       Include file references have been updated. Now calls zzdsksgr_
       instead of dsksgr_. Updated integer output format in example
       program.

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

   DEPRECATED sub-observer point

-&
*/

{ /* Begin subpt_pl02 */


   /*
   Prototypes
   */


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

   SpiceChar               loccor     [ CORLEN ];
   SpiceChar               frname     [ FRNMLN ];

   SpiceDouble             fDSKDescr  [ SPICE_DSK_DSCSIZ ];
   SpiceDouble             lt;
   SpiceDouble             mag;
   SpiceDouble             maxrad;
   SpiceDouble             normal     [3];
   SpiceDouble             npt        [3];
   SpiceDouble             nptalt;
   SpiceDouble             obspos     [3];
   SpiceDouble             offset     [3];
   SpiceDouble             radii      [3];
   SpiceDouble             raydir     [3];
   SpiceDouble             scale;
   SpiceDouble             trgpos     [3];
   SpiceDouble             vertex     [3];

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
   chkin_c ( "subpt_pl02" );


   /*
   Check the input strings: method, target, abcorr, and obsrvr.  Make
   sure none of the pointers are null and that each string contains at
   least one non-null character.
   */
   CHKFSTR ( CHK_STANDARD, "subpt_pl02", method );
   CHKFSTR ( CHK_STANDARD, "subpt_pl02", target );
   CHKFSTR ( CHK_STANDARD, "subpt_pl02", abcorr );
   CHKFSTR ( CHK_STANDARD, "subpt_pl02", obsrvr );

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
      chkout_c ( "subpt_pl02"                                   );
      return;
   }

   /*
   Obtain integer codes for the target and observer.
   */
   bods2c_c ( target, &trgcde, &found );

   if ( failed_c() )
   {
      chkout_c ( "subpt_pl02" );
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
      chkout_c ( "subpt_pl02"                                          );
      return;
   }

   bods2c_c ( obsrvr, &obscde, &found );

   if ( failed_c() )
   {
      chkout_c ( "subpt_pl02" );
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
      chkout_c ( "subpt_pl02"                                          );
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
      chkout_c ( "subpt_pl02"                                      );
      return;
   }

   /*
   Get the name of the default body-fixed reference frame associated with
   the target body.
   */
   cidfrm_c ( trgcde, FRNMLN, &frcode, frname, &found );

   if ( failed_c() )
   {
      chkout_c ( "subpt_pl02" );
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
      chkout_c ( "subpt_pl02"                                 );
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
      chkout_c ( "subpt_pl02" );
      return;
   }

   /*
   Extract center ID and DSK data type from the descriptor.
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
      chkout_c ( "subpt_pl02"                                      );
      return;
   }

   if ( dataType != 2 )
   {
      setmsg_c ( "Input segment has DSK data type #.  A segment of "
                 "type 2 is required."                               );
      errint_c ( "#", dataType                                       );
      sigerr_c ( "SPICE(WRONGDATATYPE)"                              );
      chkout_c ( "subpt_pl02"                                        );
      return;
   }


   /*
   We're done with error checks on our inputs.

   Get the maximum radius value associated with the target body.
   We'll use this later to compute a numerically safe ray vertex.
   */
   maxrad = zzdsksgr_ ( fDSKDescr );

   if ( failed_c() )
   {
      chkout_c ( "subpt_pl02" );
      return;
   }

   /*
   Look up the aberration-corrected position of the target's center as
   seen from the observer.  Use the aberration-corrected target
   body-fixed reference frame.
   */
   spkezp_c ( trgcde, et, frname, abcorr, obscde, trgpos, &lt );

   if ( failed_c() )
   {
      chkout_c ( "subpt_pl02" );
      return;
   }

   /*
   Determine the ray's vertex and direction vector. The way we define
   this vector depends on our computation method.
   */
   if (  eqstr_c( method, "intercept" )  )
   {
      /*
      The ray points from the observer towards the target's center.
      */
      vhat_c ( trgpos, raydir );

      /*
      To avoid numerical problems, we pick a vertex that is guaranteed
      to be a reasonable distance away from the target's surface.
      */
      scale = maxd_c ( 2,  1.0,  2.0*maxrad );

      vscl_c ( -scale, raydir, vertex );
   }

   else if (  eqstr_c( method, "ellipsoid near point" )  )
   {
      /*
      The ray we wish to use points towards the closest point to the
      observer on the reference ellipsoid.  We can get this point from
      the CSPICE sub-point routine for ellipsoids.
      */
      subpt_c ( NEARPT, target, et, abcorr, obsrvr, npt, &nptalt );

      if ( failed_c() )
      {
         chkout_c ( "subpt_pl02" );
         return;
      }

      /*
      Fetch the radii of the reference ellipsoid.
      */
      bodvrd_c ( target, "RADII", 3, &n, radii );

      if ( failed_c() )
      {
         chkout_c ( "subpt_pl02" );
         return;
      }

      /*
      Check for failure here because unavailability of ellipsoid
      radii is a possibility.
      */
      if ( failed_c() )
      {
         chkout_c ( "subpt_pl02" );
         return;
      }

      /*
      To avoid numerical problems, we pick a vertex that is guaranteed
      to be a reasonable distance away from the target's surface.  The
      vertex will be placed above the near point we just found; we'll
      compute the vertex by adding a scaled outward normal vector to
      `npt'.  Compute the scale, look up the outward normal, and
      compute the vertex itself.
      */
      scale = maxd_c ( 2,  1.0,  2.0*maxrad );

      surfnm_c ( radii[0], radii[1], radii[2], npt, normal );

      if ( failed_c() )
      {
         chkout_c ( "subpt_pl02" );
         return;
      }

      vlcom_c  ( scale, normal, 1.0, npt, vertex );

      /*
      The ray direction is just the negative of the outward normal.
      */
      vminus_c ( normal, raydir );

   }
   else
   {
      setmsg_c ( "The computation method # was not recognized. "
                 "Allowed values are 'Ellipsoid near point' "
                 "and 'Intercept'."                              );
      errch_c  ( "#",  method                                    );
      sigerr_c ( "SPICE(DUBIOUSMETHOD)"                          );
      chkout_c ( "subpt_pl02"                                    );
      return;
   }

   /*
   Find the surface intercept defined by the vertex, ray direction,
   and surface plate model.
   */
   dskx02_c ( handle, dladsc, vertex, raydir, plateID, spoint, &found );

   if ( failed_c() )
   {
      chkout_c ( "subpt_pl02" );
      return;
   }

   if ( !found  )
   {
      setmsg_c ( "Ray from observer # to center of target # "
                 "did not intersect the surface defined by "
                 "the input handle and descriptor."           );
      errch_c  ( "#", obsrvr                                  );
      errch_c  ( "#", target                                  );
      sigerr_c ( "SPICE(NOINTERCEPT)"                         );
      chkout_c ( "subpt_pl02"                                 );
      return;
   }

   /*
   Use the observer-intercept distance as magnitude of the altitude.
   Note that the "altitude" we compute here is not a true altitude
   in the sense of being the height above the nearest point to the
   observer.

   We must find the sign of the altitude.  Let `obspos' be the
   position of the observer relative to the target body's center.
   Let `offset' be the vector from the observer to `spoint'.  If
   `offset' has positive inner product with raydir, then the
   observer is considered to be "above" the sub-point.
   */
   vminus_c ( trgpos, obspos );
   vsub_c   ( spoint, obspos, offset );

   mag = vnorm_c ( offset );

   if ( vdot_c( offset, raydir ) >= 0.0 )
   {
      *alt =  mag;
   }
   else
   {
      *alt = -mag;
   }

   /*
   At this point, `spoint', `alt', and `plateID' are set.
   */


   chkout_c ( "subpt_pl02" );

} /* End subpt_pl02 */
