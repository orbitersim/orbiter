/*

-Procedure evsgp4_c ( Evaluate "two-line" element data )

-Abstract

   Evaluate NORAD two-line element data for earth orbiting
   spacecraft. This evaluator uses algorithms as described
   in Vallado 2006 [4].

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

   EPHEMERIS

*/
   #include "SpiceUsr.h"
   #include "SpiceZfc.h"

   void evsgp4_c ( SpiceDouble         et,
                   ConstSpiceDouble    geophs [8],
                   ConstSpiceDouble    elems  [10],
                   SpiceDouble         state  [6] )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   et         I   Epoch in seconds past ephemeris epoch J2000.
   geophs     I   Geophysical constants
   elems      I   Two-line element data
   state      O   Evaluated state

-Detailed_Input

   et          is the epoch in seconds past ephemeris epoch J2000
               at which a state should be produced from the
               input elements.

   geophs      is a collection of 8 geophysical constants needed
               for computing a state. The order of these
               constants must be:

                  geophs[0] = J2 gravitational harmonic for Earth.
                  geophs[1] = J3 gravitational harmonic for Earth.
                  geophs[2] = J4 gravitational harmonic for Earth.

               These first three constants are dimensionless.

                  geophs[3] = KE: Square root of the GM for Earth where
                              GM is expressed in Earth radii cubed per
                              minutes squared.

                  geophs[4] = QO: High altitude bound for atmospheric
                              model in km.

                  geophs[5] = SO: Low altitude bound for atmospheric
                              model in km.

                  geophs[6] = RE: Equatorial radius of the earth in km.

                  geophs[7] = AE: Distance units/earth radius
                              (normally 1).

               Below are currently recommended values for these
               items:

                  J2 =    1.082616e-3
                  J3 =   -2.53881e-6
                  J4 =   -1.65597e-6

               The next item is the square root of GM for the Earth
               given in units of earth-radii**1.5/Minute

                  KE =    7.43669161e-2

               The next two items define the top and bottom of the
               atmospheric drag model used by the type 10 ephemeris
               type. Don't adjust these unless you understand the full
               implications of such changes.

                  QO =  120.0e0
                  SO =   78.0e0

               The ER value is the equatorial radius in km of the Earth
               as used by NORAD.

                  ER = 6378.135e0

               The value of AE is the number of distance units per
               Earth radii used by the NORAD state propagation
               software. The value should be 1 unless you've got a very
               good understanding of the NORAD routine SGP4 and the
               affect of changing this value.

                  AE =    1.0e0

   elems       is an array containing two-line element data
               as prescribed below. The elements NDD6O and BSTAR
               must already be scaled by the proper exponent stored
               in the two line elements set. Moreover, the
               various items must be converted to the units shown
               here.

                  elems[ 0 ] = NDT20 in radians/minute**2
                  elems[ 1 ] = NDD60 in radians/minute**3
                  elems[ 2 ] = BSTAR
                  elems[ 3 ] = INCL  in radians
                  elems[ 4 ] = NODE0 in radians
                  elems[ 5 ] = ECC
                  elems[ 6 ] = OMEGA in radians
                  elems[ 7 ] = M0    in radians
                  elems[ 8 ] = N0    in radians/minute
                  elems[ 9 ] = EPOCH of the elements in seconds
                               past ephemeris epoch J2000.

-Detailed_Output

   state       is the state produced by evaluating the input elements
               at the input epoch `et'. Units are km and km/sec relative
               to the TEME reference frame.

-Parameters

   None.

-Exceptions

   1)  No checks are made on the reasonableness of the inputs.

   2)  If a problem occurs when evaluating the elements, an
       error is signaled by a routine in the call tree of this
       routine.

-Files

   None.

-Particulars

   This routine evaluates any NORAD two-line element sets for
   near-earth orbiting satellites using the algorithms described in
   Vallado 2006 [4].

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Suppose you have a set of two-line elements for the LUME-1
      cubesat. This example shows how you can use this routine
      together with the routine getelm_c to propagate a state to an
      epoch of interest.

      Use the meta-kernel shown below to load the required SPICE
      kernels.


         KPL/MK

         File name: evsgp4_ex1.tm

         This meta-kernel is intended to support operation of SPICE
         example programs. The kernels shown here should not be
         assumed to contain adequate or correct versions of data
         required by SPICE-based user applications.

         In order for an application to use this meta-kernel, the
         kernels referenced here must be present in the user's
         current working directory.

         The names and contents of the kernels referenced
         by this meta-kernel are as follows:

            File name           Contents
            ---------           ------------------------------------
            naif0012.tls        Leapseconds
            geophysical.ker     geophysical constants for evaluation
                                of two-line element sets.

         The geophysical.ker is a PCK file that is provided with the
         CSPICE toolkit under the "/data" directory.

         \begindata

            KERNELS_TO_LOAD = ( 'naif0012.tls',
                                'geophysical.ker'  )

         \begintext

         End of meta-kernel


      Example code begins here.


      /.
         Program evsgp4_ex1
      ./
      #include <stdio.h>
      #include <string.h>
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Local parameters.
         ./
         #define TIMSTR       "2020-05-26 02:25:00"
         #define PNAMLN       3
         #define TLELLN       70

         /.
         The LUME-1 cubesat is an Earth orbiting object; set
         the center ID to the Earth ID.
         ./
         #define CENTER       399

         /.
         Local variables.
         ./
         SpiceDouble          elems  [10];
         SpiceDouble          epoch;
         SpiceDouble          et;
         SpiceDouble          geophs [8];
         SpiceDouble          state  [6];

         SpiceInt             i;
         SpiceInt             n;

         /.
         These are the variables that will hold the constants
         required by evspg4_c. These constants are available from
         the loaded PCK file, which provides the actual values and
         units as used by NORAD propagation model.

            Constant   Meaning
            --------   ------------------------------------------
            J2         J2 gravitational harmonic for Earth.
            J3         J3 gravitational harmonic for Earth.
            J4         J4 gravitational harmonic for Earth.
            KE         Square root of the GM for Earth.
            QO         High altitude bound for atmospheric model.
            SO         Low altitude bound for atmospheric model.
            ER         Equatorial radius of the Earth.
            AE         Distance units/earth radius.
         ./
         SpiceChar            noadpn [8][PNAMLN] = { "J2", "J3", "J4", "KE",
                                                     "QO", "SO", "ER", "AE" };

         /.
         Define the Two-Line Element set for LUME-1.
         ./
         SpiceChar            tle    [2][TLELLN] = {
                    "1 43908U 18111AJ  20146.60805006  .00000806"
                                     "  00000-0  34965-4 0  9999",
                    "2 43908  97.2676  47.2136 0020001 220.6050 "
                                     "139.3698 15.24999521 78544" };

         /.
         Load the MK file that includes the PCK file that provides
         the geophysical constants required for the evaluation of
         the two-line elements sets and the LSK, as it is required
         by getelm_c to perform time conversions.
         ./
         furnsh_c ( "evsgp4_ex1.tm" );

         /.
         Retrieve the data from the kernel, and place it on
         the `geophs' array.
         ./
         for ( i = 0; i < 8; i++ )
         {
            bodvcd_c ( CENTER, noadpn[i], 1, &n, geophs+i );
         }

         /.
         Convert the Two Line Elements lines to the element sets.
         Set the lower bound for the years to be the beginning
         of the space age.
         ./
         getelm_c ( 1957, TLELLN, tle, &epoch, elems );

         /.
         Now propagate the state using evsgp4_c to the epoch
         of interest.
         ./
         str2et_c ( TIMSTR, &et );
         evsgp4_c ( et, geophs, elems, state );

         /.
         Display the results.
         ./
         printf( "Epoch   : %s\n", TIMSTR );
         printf( "Position: %15.8f %15.8f %15.8f\n",
                        state[0], state[1], state[2] );
         printf( "Velocity: %15.8f %15.8f %15.8f\n",
                        state[3], state[4], state[5] );

         return ( 0 );
      }


      When this program was executed on a PC/Linux/gcc/64-bit
      platform, the output was:


      Epoch   : 2020-05-26 02:25:00
      Position:  -4644.60403398  -5038.95025539   -337.27141116
      Velocity:     -0.45719025      0.92884817     -7.55917355


-Restrictions

   None.

-Literature_References

   [1]  F. Hoots and R. Roehrich, "Spacetrack Report #3: Models for
        Propagation of the NORAD Element Sets," U.S. Air Force
        Aerospace Defense Command, Colorado Springs, CO, 1980.

   [2]  F. Hoots, "Spacetrack Report #6: Models for Propagation of
        Space Command Element Sets,"  U.S. Air Force Aerospace
        Defense Command, Colorado Springs, CO, 1986.

   [3]  F. Hoots, P. Schumacher and R. Glover, "History of Analytical
        Orbit Modeling in the U. S. Space Surveillance System,"
        Journal of Guidance, Control, and Dynamics. 27(2):174-185,
        2004.

   [4]  D. Vallado, P. Crawford, R. Hujsak and T. Kelso, "Revisiting
        Spacetrack Report #3," paper AIAA 2006-6753 presented at the
        AIAA/AAS Astrodynamics Specialist Conference, Keystone, CO.,
        August 21-24, 2006.

-Author_and_Institution

   M. Costa Sitja      (JPL)

-Version

   -CSPICE Version 1.0.0, 05-NOV-2021 (MCS)

-Index_Entries

   Evaluate NORAD two-line element data using SGP4.

-&
*/

{ /* Begin evsgp4_c */

   /*
   Participate in error tracing.
   */
   chkin_c ( "evsgp4_c" );

   /*
   Call the f2c'd Fortran routine.
   */
   evsgp4_ (  ( doublereal * ) &et,
              ( doublereal * )  geophs,
              ( doublereal * )  elems,
              ( doublereal * )  state  );

   chkout_c ( "evsgp4_c" );

} /* End evsgp4_c */
