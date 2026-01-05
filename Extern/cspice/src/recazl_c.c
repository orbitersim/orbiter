/*

-Procedure recazl_c ( Rectangular coordinates to AZ/EL )

-Abstract

   Convert rectangular coordinates of a point to range, azimuth and
   elevation.

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

   void recazl_c ( ConstSpiceDouble    rectan [3],
                   SpiceBoolean        azccw,
                   SpiceBoolean        elplsz,
                   SpiceDouble       * range,
                   SpiceDouble       * az,
                   SpiceDouble       * el         )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   rectan     I   Rectangular coordinates of a point.
   azccw      I   Flag indicating how Azimuth is measured.
   elplsz     I   Flag indicating how Elevation is measured.
   range      O   Distance of the point from the origin.
   az         O   Azimuth in radians.
   el         O   Elevation in radians.

-Detailed_Input

   rectan      are the rectangular coordinates of a point.

   azccw       is a flag indicating how azimuth is measured.

               If `azccw' is SPICETRUE, azimuth increases in the
               counterclockwise direction; otherwise it increases in
               the clockwise direction.

   elplsz      is a flag indicating how elevation is measured.

               If `elplsz' is SPICETRUE, elevation increases from
               the XY plane toward +Z; otherwise toward -Z.

-Detailed_Output

   range       is the distance of the point from the origin.

               The units associated with `range' are those associated
               with the input point.

   az          is the azimuth of the point. This is the angle between
               the projection onto the XY plane of the vector from the
               origin to the point and the +X axis of the reference
               frame. `az' is zero at the +X axis.

               The way azimuth is measured depends on the value of the
               logical flag `azccw'. See the description of the argument
               `azccw' for details.

               `az' is output in radians. The range of `az' is [0, 2*pi].

   el          is the elevation of the point. This is the angle between
               the vector from the origin to the point and the XY
               plane. `el' is zero at the XY plane.

               The way elevation is measured depends on the value of
               the logical flag `elplsz'. See the description of the
               argument `elplsz' for details.

               `el' is output in radians. The range of `el' is [-pi/2,
               pi/2].

-Parameters

   None.

-Exceptions

   Error free.

   1)  If the X and Y components of `rectan' are both zero, the
       azimuth is set to zero.

   2)  If `rectan' is the zero vector, azimuth and elevation
       are both set to zero.

-Files

   None.

-Particulars

   This routine returns the range, azimuth, and elevation of a point
   specified in rectangular coordinates.

   The output is defined by the distance from the center of the
   reference frame (range), the angle from a reference vector
   (azimuth), and the angle above the XY plane of the reference
   frame (elevation).

   The way azimuth and elevation are measured depends on the values
   given by the user to the `azccw' and `elplsz' logical flags. See the
   descriptions of these input arguments for details.

-Examples

   The numerical results shown for these examples may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Create four tables showing a variety of rectangular
      coordinates and the corresponding range, azimuth and
      elevation, resulting from the different choices of the `azccw'
      and `elplsz' flags.

      Corresponding rectangular coordinates and azimuth, elevation
      and range are listed to three decimal places. Output angles
      are in degrees.


      Example code begins here.


      /.
         Program recazl_ex1
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

         SpiceDouble          az;
         SpiceDouble          el;
         SpiceDouble          range;
         SpiceInt             i;
         SpiceInt             j;
         SpiceInt             n;

         /.
         Define the input rectangular coordinates and the
         different choices of the `azccw' and `elplsz' flags.
         ./
         SpiceDouble          rectan [NREC][3] = { {0.0,0.0,0.0},
                                                   {1.0,0.0,0.0},
                                                   {0.0,1.0,0.0},
                                                   {0.0,0.0,1.0},
                                                   {-1.0,0.0,0.0},
                                                   {0.0,-1.0,0.0},
                                                   {0.0,0.0,-1.0},
                                                   {1.0,1.0,0.0},
                                                   {1.0,0.0,1.0},
                                                   {0.0,1.0,1.0},
                                                   {1.0,1.0,1.0} };

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
               printf( "  rect[0]  rect[1]  rect[2]   range      az   "
                       "    el\n" );
               printf( "  -------  -------  -------  -------  ------- "
                       " -------\n" );

               /.
               Do the conversion. Output angles in degrees.
               ./
               for ( n = 0; n < NREC; n++ )
               {
                  recazl_c ( rectan[n], azccw[i], elplsz[j],
                             &range,    &az,      &el );

                  printf( "%9.3f %8.3f %8.3f %8.3f %8.3f %8.3f\n",
                          rectan[n][0], rectan[n][1], rectan[n][2],
                          range,        az * dpr_c(), el * dpr_c() );
               }
            }
         }

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      AZCCW = False; ELPLSZ = False

        rect[0]  rect[1]  rect[2]   range      az       el
        -------  -------  -------  -------  -------  -------
          0.000    0.000    0.000    0.000    0.000    0.000
          1.000    0.000    0.000    1.000    0.000    0.000
          0.000    1.000    0.000    1.000  270.000    0.000
          0.000    0.000    1.000    1.000    0.000  -90.000
         -1.000    0.000    0.000    1.000  180.000    0.000
          0.000   -1.000    0.000    1.000   90.000    0.000
          0.000    0.000   -1.000    1.000    0.000   90.000
          1.000    1.000    0.000    1.414  315.000    0.000
          1.000    0.000    1.000    1.414    0.000  -45.000
          0.000    1.000    1.000    1.414  270.000  -45.000
          1.000    1.000    1.000    1.732  315.000  -35.264

      AZCCW = False; ELPLSZ = True

        rect[0]  rect[1]  rect[2]   range      az       el
        -------  -------  -------  -------  -------  -------
          0.000    0.000    0.000    0.000    0.000    0.000
          1.000    0.000    0.000    1.000    0.000    0.000
          0.000    1.000    0.000    1.000  270.000    0.000
          0.000    0.000    1.000    1.000    0.000   90.000
         -1.000    0.000    0.000    1.000  180.000    0.000
          0.000   -1.000    0.000    1.000   90.000    0.000
          0.000    0.000   -1.000    1.000    0.000  -90.000
          1.000    1.000    0.000    1.414  315.000    0.000
          1.000    0.000    1.000    1.414    0.000   45.000
          0.000    1.000    1.000    1.414  270.000   45.000
          1.000    1.000    1.000    1.732  315.000   35.264

      AZCCW = True; ELPLSZ = False

        rect[0]  rect[1]  rect[2]   range      az       el
        -------  -------  -------  -------  -------  -------
          0.000    0.000    0.000    0.000    0.000    0.000
          1.000    0.000    0.000    1.000    0.000    0.000
          0.000    1.000    0.000    1.000   90.000    0.000
          0.000    0.000    1.000    1.000    0.000  -90.000
         -1.000    0.000    0.000    1.000  180.000    0.000
          0.000   -1.000    0.000    1.000  270.000    0.000
          0.000    0.000   -1.000    1.000    0.000   90.000
          1.000    1.000    0.000    1.414   45.000    0.000
          1.000    0.000    1.000    1.414    0.000  -45.000
          0.000    1.000    1.000    1.414   90.000  -45.000
          1.000    1.000    1.000    1.732   45.000  -35.264

      AZCCW = True; ELPLSZ = True

        rect[0]  rect[1]  rect[2]   range      az       el
        -------  -------  -------  -------  -------  -------
          0.000    0.000    0.000    0.000    0.000    0.000
          1.000    0.000    0.000    1.000    0.000    0.000
          0.000    1.000    0.000    1.000   90.000    0.000
          0.000    0.000    1.000    1.000    0.000   90.000
         -1.000    0.000    0.000    1.000  180.000    0.000
          0.000   -1.000    0.000    1.000  270.000    0.000
          0.000    0.000   -1.000    1.000    0.000  -90.000
          1.000    1.000    0.000    1.414   45.000    0.000
          1.000    0.000    1.000    1.414    0.000   45.000
          0.000    1.000    1.000    1.414   90.000   45.000
          1.000    1.000    1.000    1.732   45.000   35.264


   2) Compute the apparent azimuth and elevation of Venus as seen
      from the DSS-14 station.

      Task Description
      ================

      In this example, we will obtain the apparent position of
      Venus as seen from the DSS-14 station in the DSS-14 topocentric
      reference frame. We will use a station frames kernel and
      transform the resulting rectangular coordinates to azimuth,
      elevation and range using azlrec_c.

      In order to introduce the usage of the logical flags `azccw'
      and `elplsz', we will request the azimuth to be measured
      clockwise and the elevation positive towards the +Z
      axis of the DSS-14_TOPO reference frame.


      Kernels
      =======

      Use the meta-kernel shown below to load the required SPICE
      kernels.


         KPL/MK

         File name: recazl_ex2.tm

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
            naif0011.tls                     Leapseconds
            earth_720101_070426.bpc          Earth historical
                                             binary PCK
            earthstns_itrf93_050714.bsp      DSN station SPK
            earth_topo_050714.tf             DSN station FK

         \begindata

         KERNELS_TO_LOAD = ( 'de430.bsp',
                             'naif0011.tls',
                             'earth_720101_070426.bpc',
                             'earthstns_itrf93_050714.bsp',
                             'earth_topo_050714.tf'         )

         \begintext

         End of meta-kernel.


      Example code begins here.


      /.
         Program recazl_ex2
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Local parameters
         ./
         #define META         "recazl_ex2.tm"

         /.
         Local variables
         ./
         SpiceChar          * abcorr;
         SpiceChar          * obs;
         SpiceChar          * obstim;
         SpiceChar          * ref;
         SpiceChar          * target;

         SpiceDouble          az;
         SpiceDouble          el;
         SpiceDouble          et;
         SpiceDouble          lt;
         SpiceDouble          ptarg  [3];
         SpiceDouble          r;

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
         Set the target, observer, observer frame, and
         aberration corrections.
         ./
         target = "VENUS";
         obs    = "DSS-14";
         ref    = "DSS-14_TOPO";
         abcorr = "CN+S";

         /.
         Compute the observer-target position.
         ./
         spkpos_c ( target, et, ref, abcorr, obs, ptarg, &lt );

         /.
         Compute azimuth, elevation and range of Venus
         as seen from DSS-14, with azimuth increasing
         clockwise and elevation positive towards +Z
         axis of the DSS-14_TOPO reference frame
         ./
         azccw  = SPICEFALSE;
         elplsz = SPICETRUE;

         recazl_c ( ptarg, azccw, elplsz, &r, &az, &el );

         /.
         Express both angles in degrees.
         ./
         el =   el * dpr_c();
         az =   az * dpr_c();

         /.
         Display the computed position, the range and
         the angles.
         ./
         printf( "\n" );
         printf( "Target:                %s\n", target );
         printf( "Observation time:      %s\n", obstim );
         printf( "Observer center:       %s\n", obs );
         printf( "Observer frame:        %s\n", ref );
         printf( "Aberration correction: %s\n", abcorr );
         printf( "\n" );
         printf( "Observer-target position (km):\n" );
         printf( "%21.8f %20.8f %20.8f\n", ptarg[0], ptarg[1], ptarg[2] );
         printf( "Light time (s):        %19.8f\n", lt );
         printf( "\n" );
         printf( "Target azimuth          (deg):  %19.8f\n", az );
         printf( "Target elevation        (deg):  %19.8f\n", el );
         printf( "Observer-target distance (km):  %19.8f\n", r );
         printf( "\n" );

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Target:                VENUS
      Observation time:      2003 OCT 13 06:00:00.000000 UTC
      Observer center:       DSS-14
      Observer frame:        DSS-14_TOPO
      Aberration correction: CN+S

      Observer-target position (km):
          66886767.37916669   146868551.77222887  -185296611.10841593
      Light time (s):               819.63862811

      Target azimuth          (deg):         294.48543372
      Target elevation        (deg):         -48.94609726
      Observer-target distance (km):   245721478.99272084


-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   J. Diaz del Rio     (ODC Space)

-Version

   -CSPICE Version 1.0.0, 01-NOV-2021 (JDR)

-Index_Entries

   rectangular coordinates to range, az and el
   rectangular to range, azimuth and elevation
   convert rectangular coordinates to range, az and el
   convert rectangular to range, azimuth and elevation

-&
*/

{ /* Begin recazl_c */

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
   recazl_ (  ( doublereal * )  rectan,
              ( logical    * ) &logCazccw,
              ( logical    * ) &logCelplsz,
              ( doublereal * )  range,
              ( doublereal * )  az,
              ( doublereal * )  el         );

} /* End recazl_c */
