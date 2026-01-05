/*

-Procedure subpt_c ( Sub-observer point )

-Abstract

   Deprecated: This routine has been superseded by the CSPICE
   routine subpnt_c. This routine is supported for purposes of
   backward compatibility only.

   Compute the rectangular coordinates of the sub-observer point on
   a target body at a particular epoch, optionally corrected for
   planetary (light time) and stellar aberration. Return these
   coordinates expressed in the body-fixed frame associated with the
   target body. Also, return the observer's altitude above the
   target body.

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


   void subpt_c ( ConstSpiceChar       * method,
                  ConstSpiceChar       * target,
                  SpiceDouble            et,
                  ConstSpiceChar       * abcorr,
                  ConstSpiceChar       * obsrvr,
                  SpiceDouble            spoint [3],
                  SpiceDouble          * alt         )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   method     I   Computation method.
   target     I   Name of target body.
   et         I   Epoch in ephemeris seconds past J2000 TDB.
   abcorr     I   Aberration correction.
   obsrvr     I   Name of observing body.
   spoint     O   Sub-observer point on the target body.
   alt        O   Altitude of the observer above the target body.

-Detailed_Input

   method      is a short string specifying the computation method
               to be used. The choices are:

                  "Near point"       The sub-observer point is
                                     defined as the nearest point on
                                     the target relative to the
                                     observer.

                  "Intercept"        The sub-observer point is
                                     defined as the target surface
                                     intercept of the line
                                     containing the observer and the
                                     target's center.

               In both cases, the intercept computation treats the
               surface of the target body as a triaxial ellipsoid.
               The ellipsoid's radii must be available in the kernel
               pool.

               Neither case nor white space are significant in
               `method'. For example, the string " NEARPOINT" is
               valid.


   target      is the name of the target body.  `target' is
               case-insensitive, and leading and trailing blanks in
               `target' are not significant. Optionally, you may supply
               a string containing the integer ID code for the object.
               For example both "MOON" and "301" are legitimate strings
               that indicate the moon is the target body.

               This routine assumes that the target body is modeled by
               a tri-axial ellipsoid, and that a PCK file containing
               its radii has been loaded into the kernel pool via
               furnsh_c.


   et          is the epoch in ephemeris seconds past J2000 at which
               the sub-observer point on the target body is to be
               computed.


   abcorr      indicates the aberration corrections to be applied
               when computing the observer-target state.  `abcorr'
               may be any of the following.

                  "NONE"     Apply no correction. Return the
                             geometric sub-observer point on the
                             target body.

                  "LT"       Correct for planetary (light time)
                             aberration. Both the state and rotation
                             of the target body are corrected for
                             light time.

                  "LT+S"     Correct for planetary (light time) and
                             stellar aberrations. Both the state and
                             rotation of the target body are
                             corrected for light time.


                  "CN"       Converged Newtonian light time correction.
                             In solving the light time equation, the
                             "CN" correction iterates until the
                             solution converges (three iterations on
                             all supported platforms). Whether the
                             "CN+S" solution is substantially more
                             accurate than the "LT" solution depends on
                             the geometry of the participating objects
                             and on the accuracy of the input data. In
                             all cases this routine will execute more
                             slowly when a converged solution is
                             computed. See the -Particulars section of
                             spkezr_c for a discussion of precision of
                             light time corrections.

                             Both the state and rotation of the target
                             body are corrected for light time.

                  "CN+S"     Converged Newtonian light time correction
                             and stellar aberration correction.

                             Both the state and rotation of the target
                             body are corrected for light time.


   obsrvr      is the name of the observing body. This is typically a
               spacecraft, the earth, or a surface point on the earth.
               `obsrvr' is case-insensitive, and leading and trailing
               blanks in `obsrvr' are not significant. Optionally, you
               may supply a string containing the integer ID code for
               the object. For example both "EARTH" and "399" are
               legitimate strings that indicate the earth is the
               observer.

-Detailed_Output

   spoint      is the sub-observer point on the target body at `et'
               expressed relative to the body-fixed frame of the
               target body.

               The sub-observer point is defined either as the point
               on the target body that is closest to the observer,
               or the target surface intercept of the line from the
               observer to the target's center; the input argument
               `method' selects the definition to be used.

               The body-fixed frame, which is time-dependent, is
               evaluated at `et' if `abcorr' is "NONE"; otherwise the
               frame is evaluated at et-lt, where `lt' is the one-way
               light time from target to observer.

               The state of the target body is corrected for
               aberration as specified by `abcorr'; the corrected
               state is used in the geometric computation. As
               indicated above, the rotation of the target is
               retarded by one-way light time if `abcorr' specifies
               that light time correction is to be done.

   alt         is the "altitude" of the observer above the target
               body. When `method' specifies a "near point"
               computation, `alt' is truly altitude in the standard
               geometric sense: the length of a segment dropped from
               the observer to the target's surface, such that the
               segment is perpendicular to the surface at the
               contact point `spoint'.

               When `method' specifies an "intercept" computation, `alt'
               is still the length of the segment from the observer
               to the surface point `spoint', but this segment in
               general is not perpendicular to the surface.

-Parameters

   None.

-Exceptions

   If any of the listed errors occur, the output arguments are
   left unchanged.

   1)  If the input argument `method' is not recognized, the error
       SPICE(DUBIOUSMETHOD) is signaled by a routine in the call tree
       of this routine.

   2)  If either of the input body names `target' or `obsrvr' cannot be
       mapped to NAIF integer codes, the error SPICE(IDCODENOTFOUND)
       is signaled by a routine in the call tree of this routine.

   3)  If `obsrvr' and `target' map to the same NAIF integer ID codes,
       the error SPICE(BODIESNOTDISTINCT) is signaled by a routine in
       the call tree of this routine.

   4)  If frame definition data enabling the evaluation of the state
       of the target relative to the observer in target body-fixed
       coordinates have not been loaded prior to calling subpt_c, an
       error is signaled by a routine in the call tree of this
       routine.

   5)  If the specified aberration correction is not recognized, an
       error is signaled by a routine in the call tree of this
       routine.

   6)  If insufficient ephemeris data have been loaded prior to
       calling subpt_c, an error is signaled by a
       routine in the call tree of this routine.

   7)  If the triaxial radii of the target body have not been loaded
       into the kernel pool prior to calling subpt_c, an error is
       signaled by a routine in the call tree of this routine.

   8)  If the size of the `target' body radii kernel variable is not
       three, an error is signaled by a routine in the call tree of
       this routine.

   9)  If any of the three `target' body radii is less-than or equal to
       zero, an error is signaled by a routine in the call tree of
       this routine.

   10) If PCK data supplying a rotation model for the target body
       have not been loaded prior to calling subpt_c, an error is
       signaled by a routine in the call tree of this routine.

   11) If any of the `method', `target', `abcorr' or `obsrvr' input
       string pointers is null, the error SPICE(NULLPOINTER) is
       signaled.

   12) If any of the `method', `target', `abcorr' or `obsrvr' input
       strings has zero length, the error SPICE(EMPTYSTRING) is
       signaled.

-Files

   Appropriate SPK, PCK, and frame data must be available to
   the calling program before this routine is called. Typically
   the data are made available by loading kernels; however the
   data may be supplied via subroutine interfaces if applicable.

   The following data are required:

   -  SPK data: ephemeris data for target and observer must be
      loaded. If aberration corrections are used, the states of
      target and observer relative to the solar system barycenter
      must be calculable from the available ephemeris data.
      Typically ephemeris data are made available by loading one
      or more SPK files via furnsh_c.

   -  PCK data: triaxial radii for the target body must be loaded
      into the kernel pool. Typically this is done by loading a
      text PCK file via furnsh_c.

   -  Further PCK data: rotation data for the target body must
      be loaded. These may be provided in a text or binary PCK file.
      Either type of file may be loaded via furnsh_c

   -  Frame data: if a frame definition is required to convert
      the observer and target states to the body-fixed frame of
      the target, that definition must be available in the kernel
      pool. Typically the definition is supplied by loading a
      frame kernel via furnsh_c.

   In all cases, kernel data are normally loaded once per program
   run, NOT every time this routine is called.

-Particulars

   subpt_c computes the sub-observer point on a target body.
   (The sub-observer point is commonly called the sub-spacecraft
   point when the observer is a spacecraft.) subpt_c also
   determines the altitude of the observer above the target body.

   There are two different popular ways to define the sub-observer
   point:  "nearest point on target to observer" or "target surface
   intercept of line containing observer and target." These
   coincide when the target is spherical and generally are distinct
   otherwise.

   When comparing sub-point computations with results from sources
   other than SPICE, it's essential to make sure the same geometric
   definitions are used.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Find the sub-observer point of the Mars Global Surveyor (MGS)
      spacecraft on Mars for a specified time. Perform the computation
      twice, using both the "intercept" and "near point" options.

      Use the meta-kernel shown below to load the required SPICE
      kernels.


         KPL/MK

         File: subpt_ex1.tm

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
            de430.bsp                        Planetary ephemeris
            mar097.bsp                       Mars satellite ephemeris
            pck00010.tpc                     Planet orientation and
                                             radii
            naif0011.tls                     Leapseconds
            mgs_ext12_ipng_mgs95j.bsp        MGS ephemeris

         \begindata

            KERNELS_TO_LOAD = ( 'de430.bsp',
                                'mar097.bsp',
                                'pck00010.tpc',
                                'naif0011.tls',
                                'mgs_ext12_ipng_mgs95j.bsp')
         \begintext

         End of meta-kernel


      Example code begins here.


      /.
         Program subpt_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main ()
      {
         #define  METHODLEN      25

         SpiceChar               method [2][METHODLEN] =
                                 {
                                    "Intercept", "Near point"
                                 };

         SpiceDouble             alt;
         SpiceDouble             et;
         SpiceDouble             lat;
         SpiceDouble             lon;
         SpiceDouble             radius;
         SpiceDouble             spoint [3];

         SpiceInt                i;


         /.
         Load kernel files.
         ./
         furnsh_c ( "subpt_ex1.tm" );

         /.
         Convert the UTC request time to ET (seconds past J2000 TDB).
         ./
         str2et_c ( "2003 OCT 13 06:00:00 UTC", &et );

         /.
         Compute sub-spacecraft point using light time and stellar
         aberration corrections.  Use the "target surface intercept"
         definition of sub-spacecraft point on the first loop
         iteration, and use the "near point" definition on the
         second.
         ./

         for ( i = 0;  i < 2;  i++ )
         {
            subpt_c ( method[i],
                      "MARS",     et,      "LT+S",
                      "MGS",      spoint,  &alt    );

            /.
            Convert rectangular coordinates to planetocentric
            latitude and longitude.  Convert radians to degrees.
            ./
            reclat_c ( spoint, &radius, &lon, &lat );

            lon *= dpr_c ();
            lat *= dpr_c ();

            /.
            Write the results.
            ./

            printf ( "\n"
                     "Computation method:  %s\n"
                     "\n"
                     "   Radius                   (km)  =  %25.15e\n"
                     "   Planetocentric Latitude  (deg) =  %25.15e\n"
                     "   Planetocentric Longitude (deg) =  %25.15e\n"
                     "   Altitude                 (km)  =  %25.15e\n"
                     "\n",
                     method[i],
                     radius,
                     lat,
                     lon,
                     alt                                             );
         }

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Computation method:  Intercept

         Radius                   (km)  =      3.384312533397138e+03
         Planetocentric Latitude  (deg) =     -5.030337633029706e+01
         Planetocentric Longitude (deg) =     -1.236454101932696e+02
         Altitude                 (km)  =      3.726693126686101e+02


      Computation method:  Near point

         Radius                   (km)  =      3.384323814918221e+03
         Planetocentric Latitude  (deg) =     -5.027042824194621e+01
         Planetocentric Longitude (deg) =     -1.236454101932696e+02
         Altitude                 (km)  =      3.726636724892205e+02


-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   C.H. Acton          (JPL)
   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   J.E. McLean         (JPL)
   B.V. Semenov        (JPL)

-Version

   -CSPICE Version 1.0.6, 01-NOV-2021 (JDR)

       Edited the header to comply with NAIF standard.
       Updated example to use a meta-kernel to load the required
       kernels.

   -CSPICE Version 1.0.5, 10-JUL-2014 (NJB)

       Discussion of light time corrections was updated. Assertions
       that converged light time corrections are unlikely to be
       useful were removed.

   -CSPICE Version 1.0.4, 19-MAY-2010 (BVS)

       Index line now states that this routine is deprecated.

   -CSPICE Version 1.0.3, 07-FEB-2008 (NJB)

       -Abstract now states that this routine is deprecated.

   -CSPICE Version 1.0.2, 22-JUL-2004 (NJB)

       Updated header to indicate that the `target' and `observer'
       input arguments can now contain string representations of
       integers.

   -CSPICE Version 1.0.1, 27-JUL-2003 (NJB) (CHA)

       Various header corrections were made. The example program
       was upgraded to use real kernels, and the program's output is
       shown.

   -CSPICE Version 1.0.0, 31-MAY-1999 (NJB) (JEM)

-Index_Entries

   DEPRECATED sub-observer point

-&
*/

{ /* Begin subpt_c */


   /*
   Participate in error tracing.
   */
   chkin_c ( "subpt_c" );


   /*
   Check the input strings: method, target, abcorr, and obsrvr.  Make
   sure none of the pointers are null and that each string contains at
   least one non-null character.
   */
   CHKFSTR ( CHK_STANDARD, "subpt_c", method );
   CHKFSTR ( CHK_STANDARD, "subpt_c", target );
   CHKFSTR ( CHK_STANDARD, "subpt_c", abcorr );
   CHKFSTR ( CHK_STANDARD, "subpt_c", obsrvr );


   /*
   Call the f2c'd routine.
   */
   subpt_ (  ( char         * ) method,
             ( char         * ) target,
             ( doublereal   * ) &et,
             ( char         * ) abcorr,
             ( char         * ) obsrvr,
             ( doublereal   * ) spoint,
             ( doublereal   * ) alt,
             ( ftnlen         ) strlen(method),
             ( ftnlen         ) strlen(target),
             ( ftnlen         ) strlen(abcorr),
             ( ftnlen         ) strlen(obsrvr)  );


   chkout_c ( "subpt_c" );

} /* End subpt_c */
