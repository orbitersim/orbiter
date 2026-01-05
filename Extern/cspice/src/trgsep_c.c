/*

-Procedure trgsep_c ( Separation quantity from observer )

-Abstract

   Compute the angular separation in radians between two spherical
   or point objects.

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

   ABCORR

-Keywords

   ANGLE
   GEOMETRY

*/
   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZmc.h"
   #undef    trgsep_c

   SpiceDouble trgsep_c ( SpiceDouble         et,
                          ConstSpiceChar    * targ1,
                          ConstSpiceChar    * shape1,
                          ConstSpiceChar    * frame1,
                          ConstSpiceChar    * targ2,
                          ConstSpiceChar    * shape2,
                          ConstSpiceChar    * frame2,
                          ConstSpiceChar    * obsrvr,
                          ConstSpiceChar    * abcorr )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   et         I   Ephemeris seconds past J2000 TDB.
   targ1      I   First target body name.
   shape1     I   First target body shape.
   frame1     I   Reference frame of first target.
   targ2      I   Second target body name.
   shape2     I   First target body shape.
   frame2     I   Reference frame of second target.
   obsrvr     I   Observing body name.
   abcorr     I   Aberration corrections flag.

   The function returns the angular separation between two targets,
   `targ1' and `targ2', as seen from an observer `obsrvr', possibly
   corrected for aberration corrections.

-Detailed_Input

   et          is the time in ephemeris seconds past J2000 TDB at
               which the separation is to be measured.

   targ1       is the string naming the first body of interest. You can
               also supply the integer ID code for the object as an
               integer string. For example both "MOON" and "301"
               are legitimate strings that indicate the moon is the
               target body.

   shape1      is the string naming the geometric model used to
               represent the shape of the `targ1' body. Models
               supported by this routine:

                  "SPHERE"        Treat the body as a sphere with
                                  radius equal to the maximum value of
                                  BODYnnn_RADII.

                  "POINT"         Treat the body as a point;
                                  radius has value zero.

               The `shape1' string lacks sensitivity to case, leading
               and trailing blanks.

   frame1      is the string naming the body-fixed reference frame
               corresponding to `targ1'. trgsep_c does not currently use
               this argument's value, its use is reserved for future
               shape models. The value "NULL" will suffice for
               "POINT" and "SPHERE" shaped bodies.

   targ2       is the string naming the second body of interest. You can
               also supply the integer ID code for the object as an
               integer string. For example both "MOON" and "301"
               are legitimate strings that indicate the moon is the
               target body.

   shape2      is the string naming the geometric model used to
               represent the shape of the `targ2'. Models supported by
               this routine:

                  "SPHERE"        Treat the body as a sphere with
                                  radius equal to the maximum value of
                                  BODYnnn_RADII.

                  "POINT"         Treat the body as a single point;
                                  radius has value zero.

               The `shape2' string lacks sensitivity to case, leading
               and trailing blanks.

   frame2      is the string naming the body-fixed reference frame
               corresponding to `targ2'. trgsep_c does not currently use
               this argument's value, its use is reserved for future
               shape models. The value "NULL" will suffice for
               "POINT" and "SPHERE" shaped bodies.

   obsrvr      is the string naming the observing body. Optionally, you
               may supply the ID code of the object as an integer
               string. For example, both "EARTH" and "399" are
               legitimate strings to supply to indicate the
               observer is Earth.

   abcorr      is the string description of the aberration corrections
               to apply to the state evaluations to account for
               one-way light time and stellar aberration.

               This routine accepts the same aberration corrections
               as does the SPICE routine spkezr_c. See the header of
               spkezr_c for a detailed description of the aberration
               correction options. For convenience, the options are
               listed below:

                  "NONE"     Apply no correction.

                  "LT"       "Reception" case: correct for
                             one-way light time using a Newtonian
                             formulation.

                  "LT+S"     "Reception" case: correct for
                             one-way light time and stellar
                             aberration using a Newtonian
                             formulation.

                  "CN"       "Reception" case: converged
                             Newtonian light time correction.

                  "CN+S"     "Reception" case: converged
                             Newtonian light time and stellar
                             aberration corrections.

                  "XLT"      "Transmission" case: correct for
                             one-way light time using a Newtonian
                             formulation.

                  "XLT+S"    "Transmission" case: correct for
                             one-way light time and stellar
                             aberration using a Newtonian
                             formulation.

                  "XCN"      "Transmission" case: converged
                             Newtonian light time correction.

                  "XCN+S"    "Transmission" case: converged
                             Newtonian light time and stellar
                             aberration corrections.

               The `abcorr' string lacks sensitivity to case, leading
               and trailing blanks.

-Detailed_Output

   The function returns the angular separation between two targets,
   `targ1' and `targ2', as seen from an observer `obsrvr' expressed in
   radians.

   The observer is the angle's vertex. The angular separation between
   the targets may be measured between the centers or figures (limbs)
   of the targets, depending on whether the target shapes are modeled
   as spheres or points.

   If the target shape is either a spheroid or an ellipsoid, the
   radius used to compute the limb will be the largest of the radii
   of the target's tri-axial ellipsoid model.

   If the targets are modeled as points the result ranges from 0
   to Pi radians or 180 degrees.

   If the target shapes are modeled as spheres or ellipsoids, the
   function returns a negative value when the bodies overlap
   (occult). Note that in this situation the function returns 0 when
   the limbs of the bodies start or finish the overlap.

   The positions of the targets may optionally be corrected for light
   time and stellar aberration.

-Parameters

   None.

-Exceptions

   1)  If the three objects `targ1', `targ2' and `obsrvr' are not
       distinct, an error is signaled by a routine in the call tree
       of this routine.

   2)  If the object names for `targ1', `targ2' or `obsrvr' cannot resolve
       to a NAIF body ID, an error is signaled by a routine in the
       call tree of this routine.

   3)  If the reference frame associated with `targ1', `frame1', is not
       centered on `targ1', or if the reference frame associated with
       `targ2', `frame2', is not centered on `targ2', an error is signaled
       by a routine in the call tree of this routine. This
       restriction does not apply to shapes "SPHERE" and "POINT", for
       which the frame input is ignored.

   4)  If the frame name for `frame1' or `frame2' cannot resolve to a
       NAIF frame ID, an error is signaled by a routine in the call
       tree of this routine.

   5)  If the body shape for `targ1', `shape1', or the body shape for
       `targ2', `shape2', is not recognized, an error is signaled by a
       routine in the call tree of this routine.

   6)  If the requested aberration correction `abcorr' is not
       recognized, an error is signaled by a routine in the call tree
       of this routine.

   7)  If either one or both targets' shape is modeled as sphere, and
       the required PCK data has not been loaded, an error is
       signaled by a routine in the call tree of this routine.

   8)  If the ephemeris data required to perform the needed state
       look-ups are not loaded, an error is signaled by a routine in
       the call tree of this routine.

   9)  If the observer `obsrvr' is located within either one of the
       targets, an error is signaled by a routine in the call tree of
       this routine.

   10) If an error is signaled, the function returns a meaningless
       result.

   11) If any of the `targ1', `shape1', `frame1', `targ2', `shape2',
       `frame2', `obsrvr' or `abcorr' input string pointers is null,
       the error SPICE(NULLPOINTER) is signaled. The function returns
       the value 0.0.

   12) If any of the `targ1', `shape1', `frame1', `targ2', `shape2',
       `frame2', `obsrvr' or `abcorr' input strings has zero length,
       the error SPICE(EMPTYSTRING) is signaled. The function returns
       the value 0.0.

-Files

   Appropriate SPICE kernels must be loaded by the calling program
   before this routine is called.

   The following data are required:

   -  An SPK file (or files) containing ephemeris data sufficient to
      compute the position of each of the targets with respect to the
      observer. If aberration corrections are used, the states of
      target and observer relative to the solar system barycenter
      must be calculable from the available ephemeris data.

   -  A PCK file containing the targets' tri-axial ellipsoid model,
      if the targets are modeled as spheres.

   -  If non-inertial reference frames are used, then PCK files,
      frame kernels, C-kernels, and SCLK kernels may be needed.

-Particulars

   This routine determines the apparent separation between the
   two objects as observed from a third. The value reported is
   corrected for light time. Moreover, if at the time this routine
   is called, stellar aberration corrections are enabled, this
   correction will also be applied to the apparent positions of the
   centers of the two objects.

   Please refer to the Aberration Corrections Required Reading
   (abcorr.req) for detailed information describing the nature and
   calculation of the applied corrections.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Calculate the apparent angular separation of the Earth and
      Moon as observed from the Sun at a TDB time known as a time
      of maximum separation. Calculate and output the separation
      modeling the Earth and Moon as point bodies and as spheres.
      Provide the result in degrees.

      Use the meta-kernel shown below to load the required SPICE
      kernels.


         KPL/MK

         File name: trgsep_ex1.tm

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
            pck00009.tpc                  Planet orientation and
                                          radii
            naif0009.tls                  Leapseconds

         \begindata

            KERNELS_TO_LOAD = ( 'de421.bsp',
                                'pck00009.tpc',
                                'naif0009.tls'  )

         \begintext

         End of meta-kernel


      Example code begins here.


      /.
         Program trgsep_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Local variables.
         ./
         SpiceChar          * tdbstr;
         SpiceChar          * obsrvr;
         SpiceChar          * abcorr;

         SpiceDouble          et;
         SpiceDouble          value;

         SpiceChar            frame  [2][33] = { "IAU_MOON","IAU_EARTH" };

         SpiceChar            targ   [2][33] = { "MOON","EARTH" };

         SpiceChar            shape  [2][33] = { "POINT","SPHERE" };

         /.
         Load the kernels.
         ./
         furnsh_c ( "trgsep_ex1.tm" );

         tdbstr = "2007-JAN-11 11:21:20.213872 (TDB)";
         obsrvr = "SUN";
         abcorr = "LT+S";

         str2et_c ( tdbstr, &et );

         value = trgsep_c ( et,       targ[0], shape[0],
                            frame[0], targ[1], shape[0],
                            frame[1], obsrvr,  abcorr );

         printf( "Bodies:          %-6s%-6s\n", targ[0], targ[1] );
         printf( "as seen from:    %-6s\n", obsrvr );
         printf( "at TDB time:     %-36s\n", tdbstr );
         printf( "with correction: %s\n", abcorr );
         printf( "\n" );

         printf( "Apparent angular separation:\n" );
         printf( "   point body models  (deg.):  %11.8f\n", value * dpr_c() );

         value = trgsep_c ( et,       targ[0], shape[1],
                            frame[0], targ[1], shape[1],
                            frame[1], obsrvr,  abcorr );

         printf( "   sphere body models (deg.):  %11.8f\n", value * dpr_c() );

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Bodies:          MOON  EARTH
      as seen from:    SUN
      at TDB time:     2007-JAN-11 11:21:20.213872 (TDB)
      with correction: LT+S

      Apparent angular separation:
         point body models  (deg.):   0.15729276
         sphere body models (deg.):   0.15413221


-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   M. Costa Sitja      (JPL)
   J. Diaz del Rio     (ODC Space)

-Version

   -CSPICE Version 1.0.0, 07-AUG-2021 (MCS) (JDR)

-Index_Entries

   compute the angular separation between two target bodies

-&
*/

{ /* Begin trgsep_c */

   /*
   Local variables.
   */
   SpiceDouble        result;

   /*
   Participate in error tracing.
   */
   chkin_c ( "trgsep_c" );

   /*
   Check the input string arguments:

      targ1
      shape1
      frame1
      targ2
      shape2
      frame2
      obsrvr
      abcorr

   Make sure each pointer is non-null and each string contains
   at least one data character: that is, one character
   preceding the null terminator.
   */
   CHKFSTR_VAL ( CHK_STANDARD, "trgsep_c", targ1, 0.0 );
   CHKFSTR_VAL ( CHK_STANDARD, "trgsep_c", shape1, 0.0 );
   CHKFSTR_VAL ( CHK_STANDARD, "trgsep_c", frame1, 0.0 );
   CHKFSTR_VAL ( CHK_STANDARD, "trgsep_c", targ2, 0.0 );
   CHKFSTR_VAL ( CHK_STANDARD, "trgsep_c", shape2, 0.0 );
   CHKFSTR_VAL ( CHK_STANDARD, "trgsep_c", frame2, 0.0 );
   CHKFSTR_VAL ( CHK_STANDARD, "trgsep_c", obsrvr, 0.0 );
   CHKFSTR_VAL ( CHK_STANDARD, "trgsep_c", abcorr, 0.0 );

   /*
   Call the f2c'd Fortran routine.
   */
   result = (SpiceDouble) trgsep_ (  ( doublereal * ) &et,
                                     ( char       * )  targ1,
                                     ( char       * )  shape1,
                                     ( char       * )  frame1,
                                     ( char       * )  targ2,
                                     ( char       * )  shape2,
                                     ( char       * )  frame2,
                                     ( char       * )  obsrvr,
                                     ( char       * )  abcorr,
                                     ( ftnlen       )  strlen(targ1),
                                     ( ftnlen       )  strlen(shape1),
                                     ( ftnlen       )  strlen(frame1),
                                     ( ftnlen       )  strlen(targ2),
                                     ( ftnlen       )  strlen(shape2),
                                     ( ftnlen       )  strlen(frame2),
                                     ( ftnlen       )  strlen(obsrvr),
                                     ( ftnlen       )  strlen(abcorr)  );

   chkout_c ( "trgsep_c" );

   /*
   Tell the caller the result.
   */
   return ( result );

} /* End trgsep_c */
