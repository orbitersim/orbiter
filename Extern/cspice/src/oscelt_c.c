/*

-Procedure oscelt_c ( Determine conic elements from state )

-Abstract

   Determine the set of osculating conic orbital elements that
   corresponds to the state (position, velocity) of a body at
   some epoch.

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
   EPHEMERIS

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZst.h"
   #undef    oscelt_c


   void oscelt_c ( ConstSpiceDouble   state[6],
                   SpiceDouble        et,
                   SpiceDouble        mu,
                   SpiceDouble        elts[8]   )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   state      I   State of body at epoch of elements.
   et         I   Epoch of elements.
   mu         I   Gravitational parameter (GM) of primary body.
   elts       O   Equivalent conic elements.

-Detailed_Input

   state       is the state (position and velocity) of the body
               at some epoch. Components are x, y, z, dx/dt, dy/dt,
               dz/dt. `state' must be expressed relative to an
               inertial reference frame. Units are km and km/sec.


   et          is the epoch of the input state, in ephemeris seconds
               past J2000.

                                                    3    2
   mu          is the gravitational parameter (GM, km /sec ) of
               the primary body.

-Detailed_Output

   elts        are equivalent conic elements describing the orbit
               of the body around its primary. The elements are,
               in order:

                  rp      Perifocal distance.
                  ecc     Eccentricity.
                  inc     Inclination.
                  lnode   Longitude of the ascending node.
                  argp    Argument of periapsis.
                  m0      Mean anomaly at epoch.
                  t0      Epoch.
                  mu      Gravitational parameter.

               The epoch of the elements is the epoch of the input
               state. Units are km, rad, rad/sec. The same elements
               are used to describe all three types (elliptic,
               hyperbolic, and parabolic) of conic orbit.

-Parameters

   None.

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

-Files

   None.

-Particulars

   The CSPICE routine conics_c is the inverse of this routine:
   conics_c maps a set of osculating elements and a time to a state
   vector.

-Examples

   The numerical results shown for these examples may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Determine the osculating elements of Phobos with respect to
      Mars at some arbitrary time in the J2000 inertial reference
      frame.

      Use the meta-kernel shown below to load the required SPICE
      kernels.


         KPL/MK

         File name: oscelt_ex1.tm

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
         Program oscelt_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main( )
      {
         /.
         Local variables.
         ./
         SpiceDouble          elts   [8];
         SpiceDouble          et;
         SpiceDouble          lt;
         SpiceDouble          mu     [1];
         SpiceDouble          state  [6];

         SpiceInt             dim;

         /.
         Load the meta kernel listing the needed SPK, LSK and
         PCK with gravitational parameters kernels.
         ./
         furnsh_c ( "oscelt_ex1.tm" );

         /.
         Convert the time string to ephemeris time
         ./
         str2et_c( "Dec 25, 2007", &et );

         /.
         Retrieve the state of Phobos with respect to Mars in J2000.
         ./
         spkezr_c ( "PHOBOS", et, "J2000", "NONE", "MARS",
                    state,    &lt                         );

         /.
         Read the gravitational parameter for Mars.
         ./
         bodvrd_c ( "MARS", "GM", 1, &dim, mu );

         /.
         Convert the state 6-vector to the elts 8-vector. Note: the
         bodvrd_c returns data as arrays, so to access the gravitational
         parameter (the only value in the array), we use mu[0].
         ./
         oscelt_c ( state, et, mu[0], elts );

         /.
         Output the elts vector.
         ./
         printf( "Perifocal distance          (km): %21.10f\n", elts[0] );
         printf( "Eccentricity                    : %21.10f\n", elts[1] );
         printf( "Inclination                (deg): %21.10f\n",
                                                      elts[2] * dpr_c() );
         printf( "Lon of ascending node      (deg): %21.10f\n",
                                                      elts[3] * dpr_c() );
         printf( "Argument of periapsis      (deg): %21.10f\n",
                                                      elts[4] * dpr_c() );
         printf( "Mean anomaly at epoch      (deg): %21.10f\n",
                                                      elts[5] * dpr_c() );
         printf( "Epoch                        (s): %21.10f\n", elts[6] );
         printf( "Gravitational parameter (km3/s2): %21.10f\n", elts[7] );

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Perifocal distance          (km):       9232.5746716211
      Eccentricity                    :          0.0156113904
      Inclination                (deg):         38.1225231660
      Lon of ascending node      (deg):         47.0384055902
      Argument of periapsis      (deg):        214.1546430017
      Mean anomaly at epoch      (deg):        340.5048466068
      Epoch                        (s):  251812865.1837092042
      Gravitational parameter (km3/s2):      42828.3736206991


   2) Calculate the history of Phobos's orbit plane inclination
      with respect to Mars in the J2000 frame at intervals of six
      months for a time interval of 10 years.

      Use the meta-kernel from the first example.


      Example code begins here.


      /.
         Program oscelt_ex2
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main( )
      {
         /.
         Local parameters.
         ./
         #define TIMLEN       41
         /.
         Local variables.
         ./
         SpiceChar            utcstr [TIMLEN];

         SpiceDouble          elts   [8];
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
         furnsh_c ( "oscelt_ex1.tm" );

         /.
         Read the gravitational parameter for Mars.
         ./
         bodvrd_c ( "MARS", "GM", 1, &dim, mu );

         /.
         Convert the time string to ephemeris time
         ./
         str2et_c( "Jan 1, 2000 12:00:00", &et );

         /.
         A step of six months - in seconds.
         ./
         step = 180.0 * spd_c( );

         /.
         10 years in steps of six months starting
         approximately Jan 1, 2000.
         ./
         printf( "        UCT Time          Inclination\n" );
         printf( "------------------------  -----------\n" );

         for ( i = 0; i < 20; i++ )
         {

            /.
            Retrieve the state; convert to osculating elements.
            ./
            spkezr_c ( "PHOBOS", et, "J2000", "NONE", "MARS",
                       state,    &lt                         );
            oscelt_c ( state, et, mu[0], elts );

            /.
            Convert the ephemeris time to calendar UTC.
            ./
            et2utc_c( et, "C", 3, TIMLEN, utcstr );

            printf( "%s %12.6f\n", utcstr, elts[2] * dpr_c( ) );

            et += step;

         }

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


              UCT Time          Inclination
      ------------------------  -----------
      2000 JAN 01 12:00:00.000    36.055248
      2000 JUN 29 12:00:00.000    37.112144
      2000 DEC 26 12:00:00.000    38.152129
      2001 JUN 24 12:00:00.000    37.552071
      2001 DEC 21 12:00:00.000    36.242049
      2002 JUN 19 11:59:59.999    36.330470
      2002 DEC 16 12:00:00.000    37.674595
      2003 JUN 14 11:59:59.999    38.121191
      2003 DEC 11 12:00:00.001    36.973204
      2004 JUN 08 11:59:59.999    36.033732
      2004 DEC 05 12:00:00.001    36.844542
      2005 JUN 03 11:59:59.999    38.077365
      2005 NOV 30 12:00:00.001    37.786106
      2006 MAY 29 11:59:58.999    36.413540
      2006 NOV 25 11:59:59.001    36.171050
      2007 MAY 24 11:59:58.999    37.448015
      2007 NOV 20 11:59:59.001    38.189118
      2008 MAY 18 11:59:58.999    37.223573
      2008 NOV 14 11:59:59.001    36.084745
      2009 MAY 13 11:59:57.999    36.608971


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
   E.D. Wright         (JPL)

-Version

   -CSPICE Version 1.0.3, 10-AUG-2021 (JDR)

       Edited the header to comply with NAIF standard. Added complete code
       examples to -Examples section.

   -CSPICE Version 1.0.2, 27-DEC-2007 (NJB)

      Updated -Index_Entries header section to use keywords
      "osculating" and "convert." Updated -Particulars header
      section to refer to conics_c.

   -CSPICE Version 1.0.1, 17-NOV-2005 (NJB)

      The -Exceptions and -Restrictions header sections were filled in.
      Some corrections were made to the code example.

   -CSPICE Version 1.0.0, 16-APR-1999 (EDW)

-Index_Entries

   conic elements from state
   osculating elements from state
   convert state to osculating elements

-&
*/

{ /* Begin oscelt_c */


   /*
   Participate in error tracing.
   */
   chkin_c ( "oscelt_c" );


   /*
   Call the f2c'd Fortran routine.
   */
   oscelt_( ( doublereal * ) state,
            ( doublereal * ) &et   ,
            ( doublereal * ) &mu   ,
            ( doublereal * ) elts  );


   chkout_c ( "oscelt_c" );

} /* End oscelt_c */
