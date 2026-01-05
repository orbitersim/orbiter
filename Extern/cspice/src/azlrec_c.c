/*

-Procedure azlrec_c ( AZ/EL to rectangular coordinates )

-Abstract

   Convert from range, azimuth and elevation of a point to
   rectangular coordinates.

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

   CONVERSION
   COORDINATES

*/
   #include "SpiceUsr.h"
   #include "SpiceZfc.h"

   void azlrec_c ( SpiceDouble         range,
                   SpiceDouble         az,
                   SpiceDouble         el,
                   SpiceBoolean        azccw,
                   SpiceBoolean        elplsz,
                   SpiceDouble         rectan [3] )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   range      I   Distance of the point from the origin.
   az         I   Azimuth in radians.
   el         I   Elevation in radians.
   azccw      I   Flag indicating how azimuth is measured.
   elplsz     I   Flag indicating how elevation is measured.
   rectan     O   Rectangular coordinates of a point.

-Detailed_Input

   range       is the distance of the point from the origin. The
               input should be in terms of the same units in which
               the output is desired.

               Although negative values for `range' are allowed, its
               use may lead to undesired results. See the -Exceptions
               section for a discussion on this topic.

   az          is the azimuth of the point. This is the angle between
               the projection onto the XY plane of the vector from
               the origin to the point and the +X axis of the
               reference frame. `az' is zero at the +X axis.

               The way azimuth is measured depends on the value of
               the logical flag `azccw'. See the descriptions of the
               argument `azccw' for details.

               The range (i.e., the set of allowed values) of `az' is
               unrestricted. See the -Exceptions section for a
               discussion on the `az' range.

               Units are radians.

   el          is the elevation of the point. This is the angle
               between the vector from the origin to the point and
               the XY plane. `el' is zero at the XY plane.

               The way elevation is measured depends on the value of
               the logical flag `elplsz'. See the descriptions of the
               argument `elplsz' for details.

               The range (i.e., the set of allowed values) of `el' is
               [-pi/2, pi/2], but no error checking is done to ensure
               that `el' is within this range. See the -Exceptions
               section for a discussion on the `el' range.

               Units are radians.

   azccw       is a flag indicating how the azimuth is measured.

               If `azccw' is SPICETRUE, the azimuth increases in the
               counterclockwise direction; otherwise it increases
               in the clockwise direction.

   elplsz      is a flag indicating how the elevation is measured.

               If `elplsz' is SPICETRUE, the elevation increases from
               the XY plane toward +Z; otherwise toward -Z.

-Detailed_Output

   rectan      is an array containing the rectangular coordinates of
               the point.

               The units associated with the point are those
               associated with the input `range'.

-Parameters

   None.

-Exceptions

   Error free.

   1)  If the value of the input argument `range' is negative
       the output rectangular coordinates will be negated, i.e.
       the resulting array will be of the same length
       but opposite direction to the one that would be obtained
       with a positive input argument `range' of value ||RANGE||.

   2)  If the value of the input argument `el' is outside the
       range [-pi/2, pi/2], the results may not be as
       expected.

   3)  If the value of the input argument `az' is outside the
       range [0, 2*pi], the value will be mapped to a value
       inside the range that differs from the input value by an
       integer multiple of 2*pi.

-Files

   None.

-Particulars

   This routine converts the azimuth, elevation, and range
   of a point into the associated rectangular coordinates.

   The input is defined by the distance from the center of
   the reference frame (range), the angle from a reference
   vector (azimuth), and the angle above the XY plane of the
   reference frame (elevation).

   The way azimuth and elevation are measured depends on the
   values given by the user to the `azccw' and `elplsz' logical
   flags. See the descriptions of these input arguments
   for details.

-Examples

   The numerical results shown for these examples may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Create four tables showing a variety of azimuth/elevation
      coordinates and the corresponding rectangular coordinates,
      resulting from the different choices of the `azccw' and `elplsz'
      flags.

      Corresponding azimuth/elevation and rectangular coordinates
      are listed to three decimal places. Input angles are in
      degrees.


      Example code begins here.


      /.
         Program azlrec_ex1
      ./
      #include <stdio.h>
      #include <string.h>
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Local parameters.
         ./
         #define NREC         11

         /.
         Local variables.
         ./
         SpiceChar            msg    [31];

         SpiceDouble          raz;
         SpiceDouble          rel;
         SpiceDouble          rectan [3];

         SpiceInt             i;
         SpiceInt             j;
         SpiceInt             n;

         /.
         Define the input azimuth/elevation coordinates and the
         different choices of the `azccw' and `elplsz' flags.
         ./
         SpiceDouble          range  [NREC] = { 0.0,   1.0,   1.0,   1.0,
                                                1.0,   1.0,   1.0,   1.414,
                                                1.414, 1.414, 1.732 };

         SpiceDouble          az     [NREC] = {   0.0,   0.0, 270.0,   0.0,
                                                180.0,  90.0,   0.0, 315.0,
                                                  0.0, 270.0, 315.0 };

         SpiceDouble          el     [NREC] = {   0.0,   0.0,     0.0,
                                                -90.0,   0.0,     0.0,
                                                 90.0,   0.0,   -45.0,
                                                -45.0, -35.264,
                                                 };

         SpiceBoolean         azccw  [2] = { SPICEFALSE,  SPICETRUE };
         SpiceBoolean         elplsz [2] = { SPICEFALSE,  SPICETRUE };

         /.
         Create a table for each combination of `azccw' and `elplsz'.
         ./
         for ( i = 0; i < 2; i++ )
         {
            for ( j = 0; j < 2; j++ )
            {

               /.
               Display the flag settings.
               ./
               strncpy( msg, "AZCCW = #; ELPLSZ = #", 22 );
               repml_c ( msg, "#", azccw[i], 'C', 31, msg );
               repml_c ( msg, "#", elplsz[j], 'C', 31, msg );

               printf( "\n" );
               printf( "%s\n", msg );

               /.
               Print the banner.
               ./
               printf( "\n" );
               printf( "   range      az       el    rect[0]  rect[1]  "
                       "rect[2]\n" );
               printf( "  -------  -------  -------  -------  -------  "
                       "-------\n" );

               /.
               Do the conversion. Input angles in degrees.
               ./
               for ( n = 0; n < NREC; n++ )
               {
                  raz = az[n] * rpd_c();
                  rel = el[n] * rpd_c();

                  azlrec_c ( range[n], raz,       rel,
                             azccw[i], elplsz[j], rectan );

                  printf( "%9.3f %8.3f %8.3f %8.3f %8.3f %8.3f\n",
                          range[n],  az[n],     el[n],
                          rectan[0], rectan[1], rectan[2] );
               }
            }
         }

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      AZCCW = False; ELPLSZ = False

         range      az       el    rect[0]  rect[1]  rect[2]
        -------  -------  -------  -------  -------  -------
          0.000    0.000    0.000    0.000    0.000    0.000
          1.000    0.000    0.000    1.000    0.000    0.000
          1.000  270.000    0.000   -0.000    1.000    0.000
          1.000    0.000  -90.000    0.000    0.000    1.000
          1.000  180.000    0.000   -1.000   -0.000    0.000
          1.000   90.000    0.000    0.000   -1.000    0.000
          1.000    0.000   90.000    0.000    0.000   -1.000
          1.414  315.000    0.000    1.000    1.000    0.000
          1.414    0.000  -45.000    1.000    0.000    1.000
          1.414  270.000  -45.000   -0.000    1.000    1.000
          1.732  315.000  -35.264    1.000    1.000    1.000

      AZCCW = False; ELPLSZ = True

         range      az       el    rect[0]  rect[1]  rect[2]
        -------  -------  -------  -------  -------  -------
          0.000    0.000    0.000    0.000    0.000    0.000
          1.000    0.000    0.000    1.000    0.000    0.000
          1.000  270.000    0.000   -0.000    1.000    0.000
          1.000    0.000  -90.000    0.000    0.000   -1.000
          1.000  180.000    0.000   -1.000   -0.000    0.000
          1.000   90.000    0.000    0.000   -1.000    0.000
          1.000    0.000   90.000    0.000    0.000    1.000
          1.414  315.000    0.000    1.000    1.000    0.000
          1.414    0.000  -45.000    1.000    0.000   -1.000
          1.414  270.000  -45.000   -0.000    1.000   -1.000
          1.732  315.000  -35.264    1.000    1.000   -1.000

      AZCCW = True; ELPLSZ = False

         range      az       el    rect[0]  rect[1]  rect[2]
        -------  -------  -------  -------  -------  -------
          0.000    0.000    0.000    0.000    0.000    0.000
          1.000    0.000    0.000    1.000    0.000    0.000
          1.000  270.000    0.000   -0.000   -1.000    0.000
          1.000    0.000  -90.000    0.000    0.000    1.000
          1.000  180.000    0.000   -1.000    0.000    0.000
          1.000   90.000    0.000    0.000    1.000    0.000
          1.000    0.000   90.000    0.000    0.000   -1.000
          1.414  315.000    0.000    1.000   -1.000    0.000
          1.414    0.000  -45.000    1.000    0.000    1.000
          1.414  270.000  -45.000   -0.000   -1.000    1.000
          1.732  315.000  -35.264    1.000   -1.000    1.000

      AZCCW = True; ELPLSZ = True

         range      az       el    rect[0]  rect[1]  rect[2]
        -------  -------  -------  -------  -------  -------
          0.000    0.000    0.000    0.000    0.000    0.000
          1.000    0.000    0.000    1.000    0.000    0.000
          1.000  270.000    0.000   -0.000   -1.000    0.000
          1.000    0.000  -90.000    0.000    0.000   -1.000
          1.000  180.000    0.000   -1.000    0.000    0.000
          1.000   90.000    0.000    0.000    1.000    0.000
          1.000    0.000   90.000    0.000    0.000    1.000
          1.414  315.000    0.000    1.000   -1.000    0.000
          1.414    0.000  -45.000    1.000    0.000   -1.000
          1.414  270.000  -45.000   -0.000   -1.000   -1.000
          1.732  315.000  -35.264    1.000   -1.000   -1.000


   2) Compute the right ascension and declination of the pointing
      direction of DSS-14 station at a given epoch.

      Task Description
      ================

      In this example, we will obtain the right ascension and
      declination of the pointing direction of the DSS-14 station at
      a given epoch, by converting the station's pointing direction
      given in azimuth and elevation to rectangular coordinates
      in the station topocentric reference frame and applying a
      frame transformation from DSS-14_TOPO to J2000, in order to
      finally obtain the corresponding right ascension and
      declination of the pointing vector.

      In order to introduce the usage of the logical flags `azccw'
      and `elplsz', we will assume that the azimuth is measured
      counterclockwise and the elevation negative towards +Z
      axis of the DSS-14_TOPO reference frame.

      Kernels
      =======

      Use the meta-kernel shown below to load the required SPICE
      kernels.


         KPL/MK

         File name: azlrec_ex2.tm

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
           naif0011.tls                     Leapseconds
           earth_720101_070426.bpc          Earth historical
                                            binary PCK
           earth_topo_050714.tf             DSN station FK

         \begindata

         KERNELS_TO_LOAD = ( 'naif0011.tls',
                             'earth_720101_070426.bpc',
                             'earth_topo_050714.tf'     )

         \begintext

         End of meta-kernel.


      Example code begins here.


      /.
         Program azlrec_ex2
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Local parameters
         ./
         #define META         "azlrec_ex2.tm"

         /.
         Local variables
         ./
         SpiceChar            msg    [41];
         SpiceChar          * obstim;
         SpiceChar          * ref;

         SpiceDouble          az;
         SpiceDouble          azr;
         SpiceDouble          dec;
         SpiceDouble          el;
         SpiceDouble          elr;
         SpiceDouble          et;
         SpiceDouble          jpos   [3];
         SpiceDouble          ptarg  [3];
         SpiceDouble          r;
         SpiceDouble          ra;
         SpiceDouble          range;
         SpiceDouble          rotate [3][3];

         SpiceBoolean         azccw;
         SpiceBoolean         elplsz;

         /.
         Load SPICE kernels.
         ./
         furnsh_c ( META );

         /.
         Convert the observation time to seconds past J2000 TDB.
         ./
         obstim = "2003 OCT 13 06:00:00.000000 UTC";

         str2et_c ( obstim, &et );

         /.
         Set the local topocentric frame
         ./
         ref    = "DSS-14_TOPO";

         /.
         Set the station's pointing direction in azimuth and
         elevation. Set arbitrarily the range to 1.0. Azimuth
         and elevation shall be given in radians. Azimuth
         increases counterclockwise and elevation is negative
         towards +Z (above the local horizon)
         ./
         az     =   75.00;
         el     =  -27.25;
         azr    =   az * rpd_c();
         elr    =   el * rpd_c();
         r      =    1.00;
         azccw  = SPICETRUE;
         elplsz = SPICEFALSE;

         /.
         Obtain the rectangular coordinates of the station's
         pointing direction.
         ./
         azlrec_c ( r, azr, elr, azccw, elplsz, ptarg );

         /.
         Transform the station's pointing vector from the
         local topocentric frame to J2000.
         ./
         pxform_c ( ref, "J2000", et, rotate );
         mxv_c ( rotate, ptarg, jpos );

         /.
         Compute the right ascension and declination.
         Express both angles in degrees.
         ./
         recrad_c ( jpos, &range, &ra, &dec );
         ra =   ra * dpr_c();
         dec =   dec * dpr_c();

         /.
         Display the computed pointing vector, the input
         data and resulting the angles.
         ./
         printf( "\n" );
         printf( "Pointing azimuth    (deg):  %14.8f\n", az );
         printf( "Pointing elevation  (deg):  %14.8f\n", el );

         repml_c ( "Azimuth counterclockwise?: #", "#", azccw, 'C', 41, msg );
         printf( "%s\n", msg );

         repml_c ( "Elevation positive +Z?   : #", "#", elplsz, 'C', 41,
                   msg );
         printf( "%s\n", msg );

         printf( "Observation epoch        : %s\n", obstim );
         printf( "\n" );
         printf( "Pointing direction (normalized):  \n" );
         printf( "   %14.8f %14.8f %14.8f\n", ptarg[0], ptarg[1], ptarg[2] );
         printf( "\n" );
         printf( "Pointing right ascension (deg):  %14.8f\n", ra );
         printf( "Pointing declination (deg):      %14.8f\n", dec );
         printf( "\n" );

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Pointing azimuth    (deg):     75.00000000
      Pointing elevation  (deg):    -27.25000000
      Azimuth counterclockwise?: True
      Elevation positive +Z?   : False
      Observation epoch        : 2003 OCT 13 06:00:00.000000 UTC

      Pointing direction (normalized):
             0.23009457     0.85872462     0.45787392

      Pointing right ascension (deg):    280.06179939
      Pointing declination (deg):         26.92826084


-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   J. Diaz del Rio     (ODC Space)

-Version

   -CSPICE Version 1.0.0, 08-FEB-2021 (JDR)

-Index_Entries

   range, az and el to rectangular coordinates
   range, azimuth and elevation to rectangular
   convert range, az and el to rectangular coordinates
   convert range, azimuth and elevation to rectangular

-&
*/

{ /* Begin azlrec_c */

   /*
   Local variables.
   */
   logical            logCazccw;
   logical            logCelplsz;

   /*
   Error free: no error tracing required.
   */

   /*
   Get a type logical copy of the `azccw' flag.
   */
   logCazccw = azccw;

   /*
   Get a type logical copy of the `elplsz' flag.
   */
   logCelplsz = elplsz;

   /*
   Call the f2c'd Fortran routine.
   */
   azlrec_ (  ( doublereal * ) &range,
              ( doublereal * ) &az,
              ( doublereal * ) &el,
              ( logical    * ) &logCazccw,
              ( logical    * ) &logCelplsz,
              ( doublereal * )  rectan     );

} /* End azlrec_c */
