/*

-Procedure illum_plid_pl02 (illumination angles using type 2 DSK)

-Abstract

   Deprecated: This routine has been superseded by the CSPICE routine
   illumf_c. This routine is supported for purposes of backward
   compatibility only.

   Compute the illumination angles---phase, solar incidence, and
   emission---at a specified point on a target body at a particular
   epoch, optionally corrected for light time and stellar aberration.
   Return logical flags indicating whether the surface point is
   shadowed or occulted by the target body.

   The target body's surface is represented by a triangular plate model
   contained in a type 2 DSK segment. The ID of the plate on which the
   point is located must be provided by the caller.

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


   void illum_plid_pl02 ( SpiceInt               handle,
                          ConstSpiceDLADescr   * dladsc,
                          ConstSpiceChar       * target,
                          SpiceDouble            et,
                          ConstSpiceChar       * abcorr,
                          ConstSpiceChar       * obsrvr,
                          SpiceDouble            spoint [3],
                          SpiceInt               plid,
                          SpiceDouble          * trgepc,
                          SpiceDouble            srfvec [3],
                          SpiceDouble          * phase,
                          SpiceDouble          * solar,
                          SpiceDouble          * emissn,
                          SpiceBoolean         * visible,
                          SpiceBoolean         * lit       )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   handle     I   DSK handle.
   dladsc     I   DLA descriptor of target body segment.
   target     I   Name of target body.
   et         I   Epoch in ephemeris seconds past J2000 TDB.
   abcorr     I   Aberration correction.
   obsrvr     I   Name of observing body.
   spoint     I   Body-fixed coordinates of a target surface point.
   plid       I   ID of plate on which surface point is located.
   trgepc     O   Target surface point epoch.
   srfvec     O   Vector from observer to target surface point.
   phase      O   Phase angle at the surface point.
   solar      O   Solar incidence angle at the surface point.
   emissn     O   Emission angle at the surface point.
   visible    O   Visibility flag (SPICETRUE = visible).
   lit        O   Illumination flag (SPICETRUE = illuminated).

-Detailed_Input

   handle      is the DAS file handle of a DSK file open for read
               access. This kernel must contain a type 2 segment
               that provides a plate model representing the entire
               surface of the target body.


   dladsc      is the DLA descriptor of a DSK segment representing
               the surface of the target body.


   target      is the name of the target body.  `target' is
               case-insensitive, and leading and trailing blanks in
               `target' are not significant. Optionally, you may supply
               a string containing the integer ID code for the object.
               For example both "MOON" and "301" are legitimate strings
               that indicate the moon is the target body.

               This routine assumes that the target body's surface is
               represented using a plate model, and that a DSK file
               containing the plate model has been loaded via dasopr_c.


   et          is the epoch, represented  as seconds past J2000 TDB, at
               which the illumination angles are to be computed. When
               aberration corrections are used, `et' refers to the
               epoch at which radiation is received at the observer.

   abcorr      indicates the aberration corrections to be applied to
               the position and orientation of the target body and the
               position of the Sun to account for one-way light time
               and stellar aberration. See the discussion in the
               Particulars section for recommendations on how to choose
               aberration corrections.

               `abcorr' may be any of the following:

                  "NONE"     Apply no correction. Use the geometric
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
                             arriving at the observer at `et'. The
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
                             target-Sun vector). The result is that the
                             illumination angles are computed using
                             apparent position and orientation of the
                             target as seen by the observer and the
                             apparent position of the Sun as seen by
                             the target.

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

   obsrvr      is the name of the observing body. This is typically a
               spacecraft, the earth, or a surface point on the earth.
               `obsrvr' is case-insensitive, and leading and trailing
               blanks in `obsrvr' are not significant. Optionally, you
               may supply a string containing the integer ID code for
               the object. For example both "EARTH" and "399" are
               legitimate strings that indicate the earth is the
               observer.

   spoint      is a surface point on the target body, expressed in
               rectangular body-fixed (body equator and prime meridian)
               coordinates.  `spoint' need not be visible from the
               observer's location at time `et'.


   plid        is the integer ID of the plate on which `spoint' is
               located. If `spoint' was found by calling any of the
               routines

                  dskx02_c
                  subpt_pl02
                  subsol_pl02

               `plid' is the plate ID returned by the called routine.

-Detailed_Output

   All outputs are computed using the body-fixed, body-centered
   reference frame of the DSK segment identified by `handle' and
   `dladsc'. This frame is referred to below as `fixref'. The
   frame ID of `fixref' may be obtained by calling dskgd_c, as
   is shown in the Examples section below.

   The orientation of the frame `fixref' is evaluated at the
   epoch `trgepc'.


   trgepc      is the "surface point epoch." `trgepc' is defined as
               follows: letting `lt' be the one-way light time between
               the observer and the input surface point `spoint',
               `trgepc' is either the epoch et-lt or `et' depending on
               whether the requested aberration correction is,
               respectively, for received radiation or omitted. `lt' is
               computed using the method indicated by `abcorr'.

               `trgepc' is expressed as seconds past J2000 TDB.


   srfvec      is the vector from the observer's position at `et' to
               the aberration-corrected (or optionally, geometric)
               position of `spoint', where the aberration corrections
               are specified by `abcorr'. `srfvec' is expressed in the
               target body-fixed reference frame designated by
               `fixref', evaluated at `trgepc'.

               The components of `srfvec' are given in units of km.

               One can use the CSPICE function vnorm_c to obtain the
               distance between the observer and `spoint':

                  dist = vnorm_c ( srfvec );

               The observer's position `obspos', relative to the
               target body's center, where the center's position is
               corrected for aberration effects as indicated by
               `abcorr', can be computed via the call:

                  vsub_c ( spoint, srfvec, obspos );

               To transform the vector `srfvec' from a reference frame
               `fixref' at time `trgepc' to a time-dependent reference
               frame `ref' at time `et', the routine pxfrm2_c should be
               called. Let `xform' be the 3x3 matrix representing the
               rotation from the reference frame `fixref' at time
               `trgepc' to the reference frame `ref' at time `et'. Then
               `srfvec' can be transformed to the result `refvec' as
               follows:

                  pxfrm2_c ( fixref, ref,    trgepc, et, xform );
                  mxv_c    ( xform,  srfvec, refvec            );


   phase       is the phase angle at `spoint', as seen from `obsrvr' at
               time `et'. This is the angle between the spoint-obsrvr
               vector and the spoint-sun vector. Units are radians. The
               range of `phase' is [0, pi].


   solar       is the solar incidence angle at `spoint', as seen from
               `obsrvr' at time `et'. This is the angle between the
               surface normal vector at `spoint' and the spoint-sun
               vector. Units are radians. The range of `solar' is [0,
               pi].

               Note that if the target surface is non-convex, a solar
               incidence angle less than pi/2 radians does not imply
               the surface point is illuminated. See the description of
               `lit' below.


   emissn      is the emission angle at `spoint', as seen from `obsrvr'
               at time `et'. This is the angle between the surface
               normal vector at `spoint' and the spoint-observer
               vector. Units are radians. The range of `emissn' is
               is [0, pi].

               See Particulars below for a detailed discussion of the
               definitions of these angles.

               Note that if the target surface is non-convex, an emission
               angle less than pi/2 radians does not imply the surface
               point is visible to the observer. See the description of
               `visible' below.


   visible     is a logical flag indicating whether the surface point
               is visible to the observer. `visible' takes into account
               whether the target surface occults `spoint', regardless
               of the emission angle at `spoint'. `visible' is returned
               with the value SPICETRUE if `spoint' is visible;
               otherwise it is SPICEFALSE.


   lit         is a logical flag indicating whether the surface point
               is illuminated; the point is considered to be
               illuminated if the vector from the point to the center
               of the sun doesn't intersect the target surface. `lit'
               takes into account whether the target surface casts a
               shadow on `spoint', regardless of the solar incidence
               angle at `spoint'. `lit' is returned with the value
               SPICETRUE if `spoint' is illuminated; otherwise it is
               SPICEFALSE.

-Parameters

   None.

-Exceptions

   If any of the listed errors occur, the output arguments are
   left unchanged.

   1)  If `plid' is not a valid plate ID, an error is signaled
       by a routine in the call tree of this routine.

   2)  If either of the input body names `target' or `obsrvr' cannot be
       mapped to NAIF integer codes, the error SPICE(IDCODENOTFOUND)
       is signaled.

   3)  If `obsrvr' and `target' map to the same NAIF integer ID codes, the
       error SPICE(BODIESNOTDISTINCT) is signaled.

   4)  If frame definition data enabling the evaluation of the state
       of the target relative to the observer in the target
       body-fixed frame have not been loaded prior to calling
       illum_plid_pl02, an error is signaled by a routine in the call
       tree of this routine.

   5)  If the specified aberration correction is not recognized, an
       error is signaled by a routine in the call tree of this
       routine.

   6)  If insufficient ephemeris data have been loaded prior to
       calling illum_plid_pl02, an error is signaled by a
       routine in the call tree of this routine.

   7)  If a DSK providing a DSK type 2 plate model has not been
       loaded prior to calling illum_plid_pl02, an error is signaled
       by a routine in the call tree of this routine.

   8)  If PCK data supplying a rotation model for the target body
       have not been loaded prior to calling illum_plid_pl02, an
       error is signaled by a routine in the call tree of this
       routine.

   9)  If the segment associated with the input DLA descriptor does not
       contain data for the designated target, the error
       SPICE(TARGETMISMATCH) is signaled. The target body of the DSK
       segment is determined from the `center' member of the segment's
       DSK descriptor.

   10) If the segment associated with the input DLA descriptor is not
       of data type 2, the error SPICE(WRONGDATATYPE) is signaled.

   11) Use of transmission-style aberration corrections is not
       permitted. If abcorr specified such a correction, the
       error SPICE(NOTSUPPORTED) is signaled.

   12) The observer is presumed to be outside the target body; no
       checks are made to verify this.

   13) If the DSK segment's coordinate system is not latitudinal
       (aka planetocentric), the error SPICE(BADCOORDSYSTEM) is signaled.

   14) If any input string pointer is null, the error
       SPICE(NULLPOINTER) is signaled.

   15) If any input string has length zero, the error
       SPICE(EMPTYSTRING) is signaled.

-Files

   Appropriate DSK, SPK, PCK, and frame data must be available to
   the calling program before this routine is called. Typically
   the data are made available by loading kernels; however the
   data may be supplied via subroutine interfaces if applicable.

   The following data are required:

   -  DSK data:  a DSK file containing a plate model representing the
      target body's surface must be loaded. This kernel must contain
      a type 2 segment that contains data for the entire surface of
      the target body.

   -  SPK data: ephemeris data for target, observer, and Sun must be
      loaded. If aberration corrections are used, the states of
      target and observer relative to the solar system barycenter
      must be calculable from the available ephemeris data. Typically
      ephemeris data are made available by loading one or more SPK
      files via furnsh_c.

   -  PCK data: rotation data for the target body must
      be loaded. These may be provided in a text or binary PCK file.
      Either type of file may be loaded via furnsh_c.

   -  Frame data: if a frame definition is required to convert
      the observer and target states to the body-fixed frame of
      the target, that definition must be available in the kernel
      pool. Typically the definition is supplied by loading a
      frame kernel via furnsh_c.

   In all cases, kernel data are normally loaded once per program
   run, NOT every time this routine is called.

-Particulars

   The term "illumination angles" refers to following set of
   angles:


      solar incidence angle    Angle between the surface normal at the
                               specified surface point and the vector
                               from the surface point to the Sun.

      emission angle           Angle between the surface normal at the
                               specified surface point and the vector
                               from the surface point to the observer.

      phase angle              Angle between the vectors from the
                               surface point to the observing body and
                               from the surface point to the Sun.


   The diagram below illustrates the geometric relationships defining
   these angles. The labels for the solar incidence, emission, and
   phase angles are "s.i.", "e.", and "phase".


                                                    *
                                                   Sun

                  surface normal vector
                            ._                 _.
                            |\                 /|  Sun vector
                              \    phase      /
                               \   .    .    /
                               .            .
                                 \   ___   /
                            .     \/     \/
                                  _\ s.i./
                           .    /   \   /
                           .   |  e. \ /
       *             <--------------- *  surface point on
    viewing            vector            target body
    location           to viewing
    (observer)         location


   Note that if the target-observer vector, the target normal vector
   at the surface point, and the target-sun vector are coplanar, then
   phase is the sum of incidence and emission. This is rarely true;
   usually

      phase angle  <  solar incidence angle + emission angle


   All of the above angles can be computed using light time
   corrections, light time and stellar aberration corrections, or
   no aberration corrections. The way aberration corrections
   are used is described below.

   Care must be used in computing light time corrections. The
   guiding principle used here is "describe what appears in
   an image."


      Observer-target body surface point vector
      -----------------------------------------

      Let `et' be the epoch at which an observation or remote
      sensing measurement is made, and let et - lt ("lt" stands
      for "light time") be the epoch at which the photons received
      at `et' were emitted from the body (we use the term "emitted"
      loosely here).

      The correct observer-target vector points from the observer's
      location at `et' to the surface point location at et - lt.
      The target-observer vector points in the opposite direction.

      Since light time corrections are not anti-symmetric, the correct
      target-observer vector CANNOT be found by negating the light
      time corrected position of the observer as seen from the
      target body.


      Target body's orientation
      -------------------------

      Using the definitions of `et' and `lt' above, the target
      body's orientation at et-lt is used. The surface
      normal is dependent on the target body's orientation, so
      the body's orientation model must be evaluated for the correct
      epoch.


      Target body -- Sun vector
      -------------------------

      All surface features on the target body will appear in a
      measurement made at `et' as they were at the target at epoch
      et-lt. In particular, lighting on the target body is dependent
      on the apparent location of the Sun as seen from the target body
      at et-lt. So, a second light time correction is used in finding
      the apparent location of the Sun.


   Stellar aberration corrections, when used, are applied as follows:


      Observer-target body vector
      ---------------------------

      In addition to light time correction, stellar aberration is used
      in computing the apparent target surface point position as seen
      from the observer's location at time `et'. This apparent position
      defines the observer-target surface point vector.


      Target body-Sun vector
      ----------------------

      The target body-Sun vector is the apparent position of the Sun,
      corrected for light time and stellar aberration, as seen from
      the target body at time et-lt. Note that the target body's
      position is not affected by the stellar aberration correction
      applied in finding its apparent position as seen by the
      observer.

   Once all of the vectors, as well as the target body's orientation,
   have been computed with the proper aberration corrections, the
   element of time is eliminated from the computation. The problem
   becomes a purely geometric one and is described by the diagram above.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as input,
   the compiler and supporting libraries, and the machine specific
   arithmetic implementation.

   1) Find the illumination angles at both the sub-observer point and
      sub-solar point on Phobos as seen from Mars for a specified
      sequence of times. Perform each computation twice, using both the
      "intercept" and "ellipsoid near point" options for the sub-observer
      point and sub-solar point computations. Compute the corresponding
      illumination angles using an ellipsoidal surface for comparison.
      (Note that the surface points on the plate model generally will
      not lie on the ellipsoid's surface, so the emission and solar
      incidence angles won't generally be zero at the sub-observer
      and sub-solar points, respectively.)


      Use the meta-kernel shown below to load the required SPICE
      kernels.


         KPL/MK

         File: illum_plid_pl02_ex1.tm

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
         Program illum_plid_pl02_ex1

      Find the illumination angles at both the sub-observer point and
      sub-solar point on Phobos as seen from Mars for a specified
      sequence of times. Perform each computation twice, using both the
      "intercept" and "ellipsoid near point" options for the sub-observer
      point and sub-solar point computations. Compute the corresponding
      illumination angles using an ellipsoidal surface for comparison.
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
         #define  FRNMLN         33

         /.
         Local variables
         ./
         SpiceBoolean            found;
         SpiceBoolean            lit;
         SpiceBoolean            visible;

         SpiceChar             * abcorr;
         SpiceChar             * abcorrs[ NCORR ]   =
                                 {
                                    "NONE",
                                    "CN+S"
                                 };

         SpiceChar               dsk     [ FILSIZ  ];

         SpiceChar             * emethod;
         SpiceChar             * emethods[ NMETHOD ] =
                                 {
                                    "Intercept",
                                    "Near point"
                                 };

         SpiceChar               fixref  [ FRNMLN  ];
         SpiceChar               meta    [ FILSIZ  ];
         SpiceChar             * method;
         SpiceChar             * methods [ NMETHOD ] =
                                 {
                                    "Intercept",
                                    "Ellipsoid near point"
                                 };

         SpiceChar             * obsrvr = "Mars";
         SpiceChar             * target = "Phobos";
         SpiceChar               timstr [ TIMLEN ];

         SpiceDLADescr           dladsc;
         SpiceDSKDescr           dskdsc;

         SpiceDouble             alt;
         SpiceDouble             dist;
         SpiceDouble             emissn;
         SpiceDouble             esrfvc   [3];
         SpiceDouble             et0;
         SpiceDouble             etrgep;
         SpiceDouble             et;
         SpiceDouble             phase;
         SpiceDouble             solar;
         SpiceDouble             srfvec   [3];
         SpiceDouble             stepsize;
         SpiceDouble             trgepc;
         SpiceDouble             xlat;
         SpiceDouble             xlon;
         SpiceDouble             xpt      [3];
         SpiceDouble             xr;

         SpiceInt                coridx;
         SpiceInt                handle;
         SpiceInt                i;
         SpiceInt                midx;
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

         Get the DSK descriptor of the segment; from this
         descriptor we can obtain the ID of body-fixed frame
         associated with the segment. We'll need this frame
         later to compute illumination angles on the target
         body's reference ellipsoid.
         ./
         dskgd_c ( handle, &dladsc, &dskdsc );

         frmnam_c ( dskdsc.frmcde, FRNMLN, fixref );

         if ( eqstr_c(fixref, " ") )
         {
            setmsg_c ( "Frame ID code # could not be mapped to "
                       "a frame name."                          );
            errint_c ( "#", dskdsc.frmcde                       );
            sigerr_c ( "SPICE(UNKNOWNFRAME)"                    );
         }

         /.
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
                            "      Method = %s\n", method  );

                  /.
                  Compute the sub-observer point using a plate model
                  representation of the target's surface.
                  ./
                  subpt_pl02 ( handle, &dladsc, method,
                               target, et,      abcorr,
                               obsrvr, xpt,     &alt,   &plid );

                  /.
                  Compute the illumination angles at the sub-observer
                  point. Also compute the light-of-sight visibility and
                  shadowing flags.
                  ./
                  illum_plid_pl02 ( handle,  &dladsc,  target, et,
                                    abcorr,  obsrvr,   xpt,    plid,
                                    &trgepc, srfvec,   &phase, &solar,
                                    &emissn, &visible, &lit            );

                  /.
                  Represent the intercept in latitudinal
                  coordinates.
                  ./
                  reclat_c ( xpt, &xr, &xlon, &xlat );

                  printf ( "\n"
                  "        Sub-observer point on plate model surface:\n"
                  "          Planetocentric Longitude (deg):  %f\n"
                  "          Planetocentric Latitude  (deg):  %f\n\n",
                  xlon   * dpr_c(),
                  xlat   * dpr_c() );
                  printf (
                  "            Illumination angles derived using a\n"
                  "            plate model surface:\n"
                  "                Phase angle              (deg):  %f\n"
                  "                Solar incidence angle    (deg):  %f\n"
                  "                Illumination flag             :  %ld\n"
                  "                Emission angle           (deg):  %f\n"
                  "                Visibility flag               :  %ld\n"
                  "                Range to surface point    (km):  %15.9e\n",
                  phase  * dpr_c(),
                  solar  * dpr_c(),
                  (long) lit,
                  emissn * dpr_c(),
                  (long) visible,
                  vnorm_c( srfvec )     );

                  /.
                  Compute the illumination angles using an ellipsoidal
                  representation of the target's surface. The role of
                  this representation is to provide an outward surface
                  normal.
                  ./
                  ilumin_c ( "Ellipsoid", target, et,     fixref,
                             abcorr,      obsrvr, xpt,    &etrgep,
                             esrfvc,      &phase, &solar, &emissn );

                  printf (
                  "            Illumination angles derived using an\n"
                  "            ellipsoidal reference surface:\n"
                  "                Phase angle              (deg):  %f\n"
                  "                Solar incidence angle    (deg):  %f\n"
                  "                Emission angle           (deg):  %f\n",
                  phase  * dpr_c(),
                  solar  * dpr_c(),
                  emissn * dpr_c()                                     );

                  /.
                  Now repeat our computations using the sub-solar point.

                  Compute the sub-solar point using a plate model
                  representation of the target's surface.
                  ./
                  subsol_pl02 ( handle, &dladsc, method,
                                target, et,      abcorr,
                                obsrvr, xpt,     &dist,   &plid );

                  /.
                  Compute the illumination angles at the sub-solar point.
                  Also compute the light-of-sight visibility and
                  shadowing flags.
                  ./
                  illum_plid_pl02 ( handle,  &dladsc,  target, et,
                                    abcorr,  obsrvr,   xpt,    plid,
                                    &trgepc, srfvec,   &phase, &solar,
                                    &emissn, &visible, &lit           );

                  /.
                  Represent the intercept in latitudinal coordinates.
                  ./
                  reclat_c ( xpt, &xr, &xlon, &xlat );

                  printf (  "\n"
                  "        Sub-solar point on plate model surface:\n"
                  "          Planetocentric Longitude (deg):  %f\n"
                  "          Planetocentric Latitude  (deg):  %f\n\n",
                  xlon   * dpr_c(),
                  xlat   * dpr_c() );
                  printf (
                  "            Illumination angles derived using a\n"
                  "            plate model surface:\n"
                  "                Phase angle              (deg):  %f\n"
                  "                Solar incidence angle    (deg):  %f\n"
                  "                Illumination flag             :  %ld\n"
                  "                Emission angle           (deg):  %f\n"
                  "                Visibility flag               :  %ld\n"
                  "                Range to surface point    (km):  %15.9e\n",
                  phase  * dpr_c(),
                  solar  * dpr_c(),
                  (long) lit,
                  emissn * dpr_c(),
                  (long) visible,
                  vnorm_c( srfvec )     );

                  /.
                  Compute the illumination angles using an ellipsoidal
                  representation of the target's surface. The role of
                  this representation is to provide an outward surface
                  normal.
                  ./
                  ilumin_c ( "Ellipsoid", target, et,     fixref,
                             abcorr,      obsrvr, xpt,    &etrgep,
                             esrfvc,      &phase, &solar, &emissn );

                  printf (
                  "            Illumination angles derived using an\n"
                  "            ellipsoidal reference surface:\n"
                  "                Phase angle              (deg):  %f\n"
                  "                Solar incidence angle    (deg):  %f\n"
                  "                Emission angle           (deg):  %f\n",
                  phase  * dpr_c(),
                  solar  * dpr_c(),
                  emissn * dpr_c()                                     );
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
      platform, using the meta-kernel file named illum_plid_pl02_ex1.tm
      and the DSK file named phobos_3_3.bds, the output was:


      Enter meta-kernel name > illum_plid_pl02_ex1.tm
      Enter DSK name         > phobos_3_3.bds


      Observation epoch:  2000-JAN-01 12:00:00.000 (TDB)

         abcorr = NONE

            Method = Intercept

              Sub-observer point on plate model surface:
                Planetocentric Longitude (deg):  -0.348118
                Planetocentric Latitude  (deg):  0.008861

                  Illumination angles derived using a
                  plate model surface:
                      Phase angle              (deg):  101.596824
                      Solar incidence angle    (deg):  98.376877
                      Illumination flag             :  0
                      Emission angle           (deg):  9.812914
                      Visibility flag               :  1
                      Range to surface point    (km):  9.501835727e+03
                  Illumination angles derived using an
                  ellipsoidal reference surface:
                      Phase angle              (deg):  101.596824
                      Solar incidence angle    (deg):  101.695444
                      Emission angle           (deg):  0.104977

              Sub-solar point on plate model surface:
                Planetocentric Longitude (deg):  102.413905
                Planetocentric Latitude  (deg):  -24.533127

                  Illumination angles derived using a
                  plate model surface:
                      Phase angle              (deg):  101.665306
                      Solar incidence angle    (deg):  13.068798
                      Illumination flag             :  1
                      Emission angle           (deg):  98.408735
                      Visibility flag               :  0
                      Range to surface point    (km):  9.516720964e+03
                  Illumination angles derived using an
                  ellipsoidal reference surface:
                      Phase angle              (deg):  101.665306
                      Solar incidence angle    (deg):  11.594741
                      Emission angle           (deg):  98.125499

            Method = Ellipsoid near point

              Sub-observer point on plate model surface:
                Planetocentric Longitude (deg):  -0.264850
                Planetocentric Latitude  (deg):  0.004180

                  Illumination angles derived using a
                  plate model surface:
                      Phase angle              (deg):  101.596926
                      Solar incidence angle    (deg):  98.376877
                      Illumination flag             :  0
                      Emission angle           (deg):  9.812985
                      Visibility flag               :  1
                      Range to surface point    (km):  9.501837763e+03
                  Illumination angles derived using an
                  ellipsoidal reference surface:
                      Phase angle              (deg):  101.596926
                      Solar incidence angle    (deg):  101.593324
                      Emission angle           (deg):  0.003834

              Sub-solar point on plate model surface:
                Planetocentric Longitude (deg):  105.857346
                Planetocentric Latitude  (deg):  -16.270558

                  Illumination angles derived using a
                  plate model surface:
                      Phase angle              (deg):  101.663675
                      Solar incidence angle    (deg):  16.476730
                      Illumination flag             :  1
                      Emission angle           (deg):  118.124981
                      Visibility flag               :  0
                      Range to surface point    (km):  9.517506732e+03
                  Illumination angles derived using an
                  ellipsoidal reference surface:
                      Phase angle              (deg):  101.663675
                      Solar incidence angle    (deg):  0.422781
                      Emission angle           (deg):  101.541470

         abcorr = CN+S

            Method = Intercept

              Sub-observer point on plate model surface:
                Planetocentric Longitude (deg):  -0.348101
                Planetocentric Latitude  (deg):  0.008861

                  Illumination angles derived using a
                  plate model surface:
                      Phase angle              (deg):  101.592246
                      Solar incidence angle    (deg):  98.372348
                      Illumination flag             :  0
                      Emission angle           (deg):  9.812902
                      Visibility flag               :  1
                      Range to surface point    (km):  9.502655917e+03

      [...]


      Warning: incomplete output. Only 100 out of 479 lines have been
      provided.


-Restrictions

   1)  The solar illumination state indicated by the output argument `lit'
       is computed treating the sun as a point light source. Surface
       points that are illuminated by part of the sun's disc are
       classified as "lit" or not depending on whether the center of the
       sun is visible from those points.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)

-Version

   -CSPICE Version 1.1.0, 26-OCT-2021 (JDR) (NJB)

       Bug fix: call to ljust_ is now followed by call to F2C_ConvertStr.

       Edited the Examples section to comply with NAIF standard. Fixed
       printf statements on example code as they violated ANSI-C 89
       standard maximum string literal length of 509 characters.

       Index lines now state that this routine is deprecated.

   -CSPICE Version 1.0.0, 22-FEB-2017 (NJB)

       Bug fix: the DSK segment's surface ID code is no longer
       required to match that of the target. The segment's
       center ID must match.

       Include file references have been updated.

       Added failed_c calls.

       02-SEP-2014 (NJB)

          Based on illum_pl02 Beta Version 1.3.0, 30-APR-2014 (NJB) (BVS)

-Index_Entries

   DEPRECATED plate model point visibility and shadowing
   DEPRECATED illumination angles using DSK type 2 plate model
   DEPRECATED lighting angles using DSK type 2 plate model
   DEPRECATED phase angle using DSK type 2 plate model
   DEPRECATED emission angle using DSK type 2 plate model
   DEPRECATED solar incidence angle using DSK type 2

-&
*/

{ /* Begin illum_plid_pl02 */


   /*
   Prototypes
   */
   int dskgd_(integer *handle, integer *dladsc, doublereal *dskdsc);


   /*
   Local parameters
   */
   #define FRNMLN          33
   #define CORLEN          16
   #define TOLSCL          1.e-10

   /*
   Local variables
   */
   SpiceBoolean            found;

   SpiceChar               loccor     [ CORLEN ];
   SpiceChar               fixref     [ FRNMLN ];

   SpiceDSKDescr           dskdsc;

   SpiceDouble             ltsun;
   SpiceDouble             lttarg;
   SpiceDouble             maxrad;
   SpiceDouble             normal     [ 3 ];
   SpiceDouble             obsvec     [ 3 ];
   SpiceDouble             shiftpt    [ 3 ] ;
   SpiceDouble             sunsta     [ 6 ];
   SpiceDouble             tol;
   SpiceDouble             trgsta     [ 6 ];
   SpiceDouble             xpt        [ 3 ];

   SpiceInt                centerID;
   SpiceInt                dataType;
   SpiceInt                frcode;
   SpiceInt                obscde;
   SpiceInt                trgcde;
   SpiceInt                xplid;


   /*
   Participate in error tracing.
   */
   if ( return_c()  )
   {
      return;
   }
   chkin_c ( "illum_plid_pl02" );


   /*
   Check the input strings: method, target, abcorr, and obsrvr.  Make
   sure none of the pointers are null and that each string contains at
   least one non-null character.
   */
   CHKFSTR ( CHK_STANDARD, "illum_plid_pl02", target );
   CHKFSTR ( CHK_STANDARD, "illum_plid_pl02", abcorr );
   CHKFSTR ( CHK_STANDARD, "illum_plid_pl02", obsrvr );

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
      chkout_c ( "illum_plid_pl02"                              );
      return;
   }



   /*
   Obtain integer codes for the target and observer.
   */
   bods2c_c ( target, &trgcde, &found );

   if ( failed_c() )
   {
      chkout_c ( "illum_plid_pl02" );
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
      chkout_c ( "illum_plid_pl02"                                     );
      return;
   }

   bods2c_c ( obsrvr, &obscde, &found );

   if ( failed_c() )
   {
      chkout_c ( "illum_plid_pl02" );
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
      chkout_c ( "illum_plid_pl02"                                     );
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
      chkout_c ( "illum_plid_pl02"                                 );
      return;
   }


   /*
   Fetch and examine the DSK descriptor of the segment from which we're
   getting the shape data. Make sure this is a type 2 segment for the
   specified target.  Since we don't know a priori that the segment
   has type 2, we can't use the type 2 fetch routine for this operation.
   */

   dskgd_c ( handle, dladsc, &dskdsc );

   if ( failed_c() )
   {
      chkout_c ( "illum_plid_pl02" );
      return;
   }

   /*
   Extract object ID and DSK data type from the DSK descriptor.
   */
   centerID = dskdsc.center;
   dataType = dskdsc.dtype;

   if ( centerID != trgcde )
   {
      setmsg_c ( "Input segment is for object with integer ID "
                 "code #, which does not match target ID code #."  );
      errint_c ( "#", centerID                                     );
      errint_c ( "#", trgcde                                       );
      sigerr_c ( "SPICE(TARGETMISMATCH)"                           );
      chkout_c ( "illum_plid_pl02"                                 );
      return;
   }

   if ( dataType != 2 )
   {
      setmsg_c ( "Input segment has DSK data type #.  A segment of "
                 "type 2 is required."                               );
      errint_c ( "#", dataType                                       );
      sigerr_c ( "SPICE(WRONGDATATYPE)"                              );
      chkout_c ( "illum_plid_pl02"                                   );
      return;
   }


   /*
   Extract frame ID from the DSK descriptor; get the name of the
   default body-fixed reference frame associated with the target body.
   */
   frcode = dskdsc.frmcde;

   frmnam_c ( frcode, FRNMLN, fixref );

   if ( eqstr_c( fixref, " " ) )
   {
      setmsg_c ( "No body-fixed frame name is associated with "
                 "frame ID code #; a frame kernel must be "
                 "loaded to make this association.  Consult "
                 "the FRAMES Required Reading for details."   );
      errint_c ( "#", frcode                                  );
      sigerr_c ( "SPICE(FRAMENAMENOTFOUND)"                   );
      chkout_c ( "illum_plid_pl02"                            );
      return;
   }


   /*
   Check the segment's coordinate system. This routine handles
   only latitudinal coordinates: they're used to provide the
   segment's maximum radius value.
   */
   if ( dskdsc.corsys  !=  SPICE_DSK_LATSYS )
   {
      setmsg_c ( "The input DSK segment's coordinate system code # "
                 "designates a system not currently supported by "
                 "this routine. The system must be latitudinal. "
                 "See the header file SpiceDSK.h for coordinate "
                 "system codes."                                    );
      errint_c ( "#", dskdsc.corsys                                 );
      sigerr_c ( "SPICE(FRAMENAMENOTFOUND)"                         );
      chkout_c ( "illum_plid_pl02"                                  );
      return;
   }


   /*
   We're done with error checks on our inputs.

   Get the outward normal vector at the input plate.
   */
   dskn02_c ( handle, dladsc, plid, normal );

   if ( failed_c() )
   {
      chkout_c ( "illum_plid_pl02" );
      return;
   }

   /*
   Compute the aberration-corrected position of the surface point
   `spoint' as seen from the observer. Use the aberration-corrected
   target body-fixed reference frame. Negate the target position to obtain
   the target-observer vector. Let `trgepc' be the epoch associated with
   the target.
   */
   spkcpt_c ( spoint,   target, fixref, et,     fixref,
              "TARGET", abcorr, obsrvr, trgsta, &lttarg );

   if ( failed_c() )
   {
      chkout_c ( "illum_plid_pl02" );
      return;
   }

   vequ_c   ( trgsta, srfvec );
   vminus_c ( srfvec, obsvec );

   zzcorepc_ ( ( char        * ) abcorr,
               ( doublereal  * ) &et,
               ( doublereal  * ) &lttarg,
               ( doublereal  * ) trgepc,
               ( ftnlen        ) strlen(abcorr)  );

   /*
   Now find the apparent position of the Sun as seen from the
   target center at `trgepc'.
   */
   spkcpo_c ( "Sun",  *trgepc, fixref, "OBSERVER", abcorr,
              spoint, target,  fixref, sunsta,     &ltsun  );

   if ( failed_c() )
   {
      chkout_c ( "illum_plid_pl02" );
      return;
   }

   /*
   Find the illumination angles. vsep_c will give us angular
   separation in radians.
   */

   *phase   =  vsep_c ( sunsta, obsvec );
   *solar   =  vsep_c ( normal, sunsta );
   *emissn  =  vsep_c ( normal, obsvec );


   /*
   Determine whether the observer-target vector intercepts the surface
   before reaching the surface point. We'll use the negative of this
   vector for the computation, and we'll shift the surface point "up"
   (in the outward normal direction) in order to be able do detect
   an intersection with the plate on which `spoint' lies.
   */
   maxrad = dskdsc.co3max;
   tol    = TOLSCL * maxrad;

   vlcom_c ( 1.0, spoint, tol, normal, shiftpt );

   dskx02_c ( handle, dladsc, shiftpt, obsvec, &xplid, xpt, &found );

   if ( failed_c() )
   {
      chkout_c ( "illum_plid_pl02" );
      return;
   }

   /*
   If this ray hits the surface, the line of sight between observer
   and target is blocked. To avoid contradictions in outputs due to
   numerical errors, take the emission angle into account.
   */

   *visible = ( !found )  &&  ( *emissn < halfpi_c() );


   /*
   Perform the analogous computation for the surface point to sun
   vector.
   */
   dskx02_c ( handle, dladsc, shiftpt, sunsta, &xplid, xpt, &found );

   if ( failed_c() )
   {
      chkout_c ( "illum_plid_pl02" );
      return;
   }


   *lit = ( !found )  &&  ( *solar < halfpi_c() );


   chkout_c ( "illum_plid_pl02" );

 } /* End illum_plid_pl02 */
