/*

-Procedure oscltx_c ( Extended osculating elements from state )

-Abstract

   Determine the set of osculating conic orbital elements that
   corresponds to the state (position, velocity) of a body at some
   epoch. In additional to the classical elements, return the true
   anomaly, semi-major axis, and period, if applicable.

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

   CONIC
   ELEMENTS
   EPHEMERIS

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #undef   oscltx_c

   void oscltx_c ( ConstSpiceDouble state [6],
                   SpiceDouble      et,
                   SpiceDouble      mu,
                   SpiceDouble      elts  [SPICE_OSCLTX_NELTS] )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   state      I   State of body at epoch of elements.
   et         I   Epoch of elements.
   mu         I   Gravitational parameter (GM) of primary body.
   elts       O   Extended set of classical conic elements.

-Detailed_Input

   state       is the state (position and velocity) of the body
               at some epoch. Components are x, y, z, dx/dt, dy/dt,
               dz/dt. `state' must be expressed relative to an
               inertial reference frame. Units are km and km/sec.

   et          is the epoch of the input state, in ephemeris seconds
               past J2000.

   mu          is the gravitational parameter (GM, km^3/sec^2) of
               the primary body.

-Detailed_Output

   elts        are equivalent conic elements describing the orbit
               of the body around its primary. The elements are,
               in order:

                  RP      Perifocal distance.
                  ECC     Eccentricity.
                  INC     Inclination.
                  LNODE   Longitude of the ascending node.
                  ARGP    Argument of periapsis.
                  M0      Mean anomaly at epoch.
                  T0      Epoch.
                  MU      Gravitational parameter.
                  NU      True anomaly at epoch.
                  A       Semi-major axis. A is set to zero if
                          it is not computable.
                  TAU     Orbital period. Applicable only for
                          elliptical orbits. Set to zero otherwise.

               The epoch of the elements is the epoch of the input
               state. Units are km, rad, rad/sec. The same elements
               are used to describe all three types (elliptic,
               hyperbolic, and parabolic) of conic orbits.

               See the -Parameters section for information on the
               declaration of `elts'.

-Parameters

   SPICE_OSCLTX_NELTS

               is the length of the output array `elts'.

               `elts' is intended to contain unused space to
               hold additional elements that may be added in
               a later version of this routine. In order to
               maintain forward compatibility, user
               applications should declare `elts' as follows:

                  SpiceDouble    elts[SPICE_OSCLTX_NELTS];

-Exceptions

   1)  If `mu' is not positive, the error SPICE(NONPOSITIVEMASS)
       is signaled by a routine in the call tree of this routine.

   2)  If the specific angular momentum vector derived from `state'
       is the zero vector, the error SPICE(DEGENERATECASE)
       is signaled by a routine in the call tree of this routine.

   3)  If the position or velocity vectors derived from `state'
       is the zero vector, the error SPICE(DEGENERATECASE)
       is signaled by a routine in the call tree of this routine.

   4)  If the inclination is determined to be zero or 180 degrees,
       the longitude of the ascending node is set to zero.

   5)  If the eccentricity is determined to be zero, the argument of
       periapse is set to zero.

   6)  If the eccentricity of the orbit is very close to but not
       equal to zero, the argument of periapse may not be accurately
       determined.

   7)  For inclinations near but not equal to 0 or 180 degrees,
       the longitude of the ascending node may not be determined
       accurately. The argument of periapse and mean anomaly may
       also be inaccurate.

   8)  For eccentricities very close to but not equal to 1, the
       results of this routine are unreliable.

   9)  If the specific angular momentum vector is non-zero but
       "close" to zero, the results of this routine are unreliable.

   10) If `state' is expressed relative to a non-inertial reference
       frame, the resulting elements are invalid. No error checking
       is done to detect this problem.

   11) The semi-major axis and period may not be computable for
       orbits having eccentricity too close to 1. If the semi-major
       axis is not computable, both it and the period are set to
       zero. If the period is not computable, it is set to zero.

-Files

   None.

-Particulars

   This routine returns in the first 8 elements of the array `elts'
   the outputs computed by oscelt_c, and in addition returns in
   elements 9-11 the quantities:

      elts[8]   true anomaly at `et', in radians.

      elts[9]   orbital semi-major axis at `et', in km. Valid
                if and only if this value is non-zero.

                The semi-major axis won't be computable if the
                eccentricity of the orbit is too close to 1.
                In this case A is set to zero.

      elts[10]  orbital period. If the period is not computable,
                TAU is set to zero.

   The CSPICE routine conics_c is an approximate inverse of this
   routine: conics_c maps a set of osculating elements and a time to a
   state vector.

-Examples

   The numerical results shown for these examples may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Determine the osculating conic orbital elements of Phobos
      with respect to Mars at some arbitrary time in the J2000
      inertial reference frame, including true anomaly, semi-major
      axis and period.

      Use the meta-kernel shown below to load the required SPICE
      kernels.


         KPL/MK

         File name: oscltx_ex1.tm

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
            mar097.bsp                    Mars satellite ephemeris
            gm_de431.tpc                  Gravitational constants
            naif0012.tls                  Leapseconds


         \begindata

            KERNELS_TO_LOAD = ( 'mar097.bsp',
                                'gm_de431.tpc',
                                'naif0012.tls'  )

         \begintext

         End of meta-kernel


      Example code begins here.


      /.
         Program oscltx_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Local variables.
         ./
         SpiceDouble          elts   [SPICE_OSCLTX_NELTS];
         SpiceDouble          et;
         SpiceDouble          lt;
         SpiceDouble          mu     [1];
         SpiceDouble          state  [6];

         SpiceInt             dim;

         /.
         Load the meta kernel listing the needed SPK, LSK and
         PCK with gravitational parameters kernels.
         ./
         furnsh_c ( "oscltx_ex1.tm" );

         /.
         Convert the time string to ephemeris time
         ./
         str2et_c ( "Dec 25, 2007", &et );

         /.
         Retrieve the state of Phobos with respect to Mars in
         J2000.
         ./
         spkezr_c ( "PHOBOS", et, "J2000", "NONE", "MARS",
                     state,   &lt                         );

         /.
         Read the gravitational parameter for Mars.
         ./
         bodvrd_c ( "MARS", "GM", 1, &dim, mu );

         /.
         Convert the state 6-vector to the elts 8-vector. Note:
         bodvrd_c returns data as arrays, so to access the
         gravitational parameter (the only value in the array),
         we use mu[0]).
         ./
         oscltx_c ( state, et, mu[0], elts );

         /.
         Output the elts vector.
         ./
         printf( "Perifocal distance          (km):  %20.9f\n",
                  elts[0]                                      );
         printf( "Eccentricity                    :  %20.9f\n",
                  elts[1]                                      );
         printf( "Inclination                (deg):  %20.9f\n",
                  elts[2] * dpr_c ( )                          );
         printf( "Lon of ascending node      (deg):  %20.9f\n",
                  elts[3] * dpr_c ( )                          );
         printf( "Argument of periapsis      (deg):  %20.9f\n",
                  elts[4] * dpr_c ( )                          );
         printf( "Mean anomaly at epoch      (deg):  %20.9f\n",
                  elts[5] * dpr_c ( )                          );
         printf( "Epoch                        (s):  %20.9f\n",
                  elts[6]                                      );
         printf( "Gravitational parameter (km3/s2):  %20.9f\n",
                  elts[7]                                      );
         printf( "True anomaly at epoch      (deg):  %20.9f\n",
                  elts[8] * dpr_c ( )                          );
         printf( "Orbital semi-major axis     (km):  %20.9f\n",
                  elts[9]                                      );
         printf( "Orbital period               (s):  %20.9f\n",
                  elts[10]                                     );

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Perifocal distance          (km):        9232.574671621
      Eccentricity                    :           0.015611390
      Inclination                (deg):          38.122523166
      Lon of ascending node      (deg):          47.038405590
      Argument of periapsis      (deg):         214.154643002
      Mean anomaly at epoch      (deg):         340.504846607
      Epoch                        (s):   251812865.183709204
      Gravitational parameter (km3/s2):       42828.373620699
      True anomaly at epoch      (deg):         339.896662808
      Orbital semi-major axis     (km):        9378.993805149
      Orbital period               (s):       27577.090893061


   2) Calculate the history of Phobos's orbital period at intervals
      of six months for a time interval of 10 years.

      Use the meta-kernel from the first example.


      Example code begins here.


      /.
         Program oscltx_ex2
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Local parameters.
         ./
         #define TIMLEN       24

         /.
         Local variables.
         ./
         SpiceChar            utcstr [TIMLEN];

         SpiceDouble          elts   [SPICE_OSCLTX_NELTS];
         SpiceDouble          et;
         SpiceDouble          lt;
         SpiceDouble          mu     [1];
         SpiceDouble          state  [6];
         SpiceDouble          step;

         SpiceInt             dim;
         SpiceInt             i;

         /.
         Load the meta kernel listing the needed SPK, LSK and
         PCK with gravitational parameters kernels.
         ./
         furnsh_c ( "oscltx_ex1.tm" );

         /.
         Read the gravitational parameter for Mars.
         ./
         bodvrd_c ( "MARS", "GM", 1, &dim, mu );

         /.
         Convert the time string to ephemeris time
         ./
         str2et_c ( "Jan 1, 2000 12:00:00", &et );

         /.
         A step of six months - in seconds.
         ./
         step = 180.0 * spd_c ( );

         /.
         10 years in steps of six months starting
         approximately Jan 1, 2000.
         ./
         printf( "        UCT Time             Period\n" );
         printf( "------------------------  ------------\n" );

         for ( i = 0; i < 20; i++ )
         {

            /.
            Retrieve the state; convert to osculating elements.
            ./
            spkezr_c ( "PHOBOS", et, "J2000", "NONE", "MARS",
                        state,  &lt                          );
            oscltx_c ( state, et, mu[0], elts );

            /.
            Convert the ephemeris time to calendar UTC.
            ./
            et2utc_c ( et, "C", 3, TIMLEN, utcstr );

            printf( "%s  %11.5f\n", utcstr, elts[10] );

            et = et + step;

         }

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


              UCT Time             Period
      ------------------------  ------------
      2000 JAN 01 12:00:00.00  27575.41925
      2000 JUN 29 12:00:00.00  27575.12405
      2000 DEC 26 12:00:00.00  27574.98775
      2001 JUN 24 12:00:00.00  27574.27316
      2001 DEC 21 12:00:00.00  27573.09614
      2002 JUN 19 11:59:59.99  27572.26206
      2002 DEC 16 12:00:00.00  27572.33639
      2003 JUN 14 11:59:59.99  27572.57699
      2003 DEC 11 12:00:00.00  27572.44191
      2004 JUN 08 11:59:59.99  27572.33853
      2004 DEC 05 12:00:00.00  27572.96474
      2005 JUN 03 11:59:59.99  27574.45044
      2005 NOV 30 12:00:00.00  27575.62760
      2006 MAY 29 11:59:58.99  27576.17410
      2006 NOV 25 11:59:59.00  27576.70212
      2007 MAY 24 11:59:58.99  27577.62501
      2007 NOV 20 11:59:59.00  27578.95916
      2008 MAY 18 11:59:58.99  27579.54508
      2008 NOV 14 11:59:59.00  27578.92061
      2009 MAY 13 11:59:57.99  27577.80062


-Restrictions

   1)  The input state vector must be expressed relative to an
       inertial reference frame.

   2)  Osculating elements are generally not useful for
       high-accuracy work.

   3)  Accurate osculating elements may be difficult to derive for
       near-circular or near-equatorial orbits. Osculating elements
       for such orbits should be used with caution.

   4)  Extracting osculating elements from a state vector is a
       mathematically simple but numerically challenging task. The
       mapping from a state vector to equivalent elements is
       undefined for certain state vectors, and the mapping is
       difficult to implement with finite precision arithmetic for
       states near the subsets of R6 where singularities occur.

       In general, the elements found by this routine can have
       two kinds of problems:

       -  The elements are not accurate but still represent
          the input state accurately. The can happen in
          cases where the inclination is near zero or 180
          degrees, or for near-circular orbits.

       -  The elements are garbage. This can occur when
          the eccentricity of the orbit is close to but
          not equal to 1. In general, any inputs that cause
          great loss of precision in the computation of the
          specific angular momentum vector or the eccentricity
          vector will result in invalid outputs.

       For further details, see the -Exceptions section.

       Users of this routine should carefully consider whether
       it is suitable for their applications. One recommended
       "sanity check" on the outputs is to supply them to the
       CSPICE routine conics_c and compare the resulting state
       vector with the one supplied to this routine.

-Literature_References

   [1]  R. Bate, D. Mueller, and J. White, "Fundamentals of
        Astrodynamics," Dover Publications Inc., 1971.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   K.R. Gehringer      (JPL)
   I.M. Underwood      (JPL)
   E.D. Wright         (JPL)

-Version

   -CSPICE Version 1.0.1, 10-AUG-2021 (JDR)

       Edited the header to comply with NAIF standard. Reformatted
       -Detailed_Input and -Parameters sections.

       Added complete code examples to -Examples section.

   -CSPICE Version 1.0.0, 25-JAN-2017 (NJB) (KRG) (IMU) (EDW)

       Original version 11-NOV-2014 (NJB) (KRG) (IMU) (EDW)

-Index_Entries

   extended conic elements from state
   extended osculating elements from state
   convert state to extended osculating elements

-&
*/

{ /* Begin oscltx_c */


   /*
   Participate in error tracing.
   */
   chkin_c ( "oscltx_c" );


   /*
   Let the f2c'd routine do the work.
   */
   oscltx_ (  (doublereal *) state,
              (doublereal *) &et,
              (doublereal *) &mu,
              (doublereal *) elts  );


   chkout_c ( "oscltx_c" );

} /* End oscltx_c */
