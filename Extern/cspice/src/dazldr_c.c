/*

-Procedure dazldr_c ( Derivative of AZ/EL w.r.t. rectangular )

-Abstract

   Compute the Jacobian matrix of the transformation from
   rectangular to azimuth/elevation coordinates.

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

   COORDINATES
   DERIVATIVES
   MATRIX

*/
   #include "SpiceUsr.h"
   #include "SpiceZfc.h"

   void dazldr_c ( SpiceDouble         x,
                   SpiceDouble         y,
                   SpiceDouble         z,
                   SpiceBoolean        azccw,
                   SpiceBoolean        elplsz,
                   SpiceDouble         jacobi [3][3] )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   x          I   x-coordinate of point.
   y          I   y-coordinate of point.
   z          I   z-coordinate of point.
   azccw      I   Flag indicating how azimuth is measured.
   elplsz     I   Flag indicating how elevation is measured.
   jacobi     O   Matrix of partial derivatives.

-Detailed_Input

   x,
   y,
   z           are the rectangular coordinates of the point at
               which the Jacobian matrix of the map from rectangular
               to azimuth/elevation coordinates is desired.

   azccw       is a flag indicating how the azimuth is measured.

               If `azccw' is SPICETRUE, the azimuth increases in the
               counterclockwise direction; otherwise it increases
               in the clockwise direction.

   elplsz      is a flag indicating how the elevation is measured.

               If `elplsz' is SPICETRUE, the elevation increases from the
               XY plane toward +Z; otherwise toward -Z.

-Detailed_Output

   jacobi      is the matrix of partial derivatives of the
               transformation from rectangular to azimuth/elevation
               coordinates. It has the form

                  .-                            -.
                  |  dr/dx     dr/dy     dr/dz   |
                  |  daz/dx    daz/dy    daz/dz  |
                  |  del/dx    del/dy    del/dz  |
                  `-                            -'

                evaluated at the input values of `x', `y', and `z'.

-Parameters

   None.

-Exceptions

   1)  If the input point is on the Z-axis ( x = 0 and y = 0 ), the
       Jacobian matrix is undefined and therefore, the error
       SPICE(POINTONZAXIS) is signaled by a routine in the call tree
       of this routine.

-Files

   None.

-Particulars

   When performing vector calculations with velocities it is
   usually most convenient to work in rectangular coordinates.
   However, once the vector manipulations have been performed
   it is often desirable to convert the rectangular representations
   into azimuth/elevation coordinates to gain insights about
   phenomena in this coordinate system.

   To transform rectangular velocities to derivatives of coordinates
   in an azimuth/elevation coordinate system, one uses the Jacobian
   matrix of the transformation between the two systems.

   Given a state in rectangular coordinates

      ( x, y, z, dx, dy, dz )

   the corresponding azimuth/elevation coordinate derivatives are
   given by the matrix equation:

                    t          |                      t
      (dr, daz, del)   = jacobi|        * (dx, dy, dz)
                               |(x,y,z)

   This routine computes the matrix

            |
      jacobi|
            |(x, y, z)

   In the azimuth/elevation coordinate system, several conventions
   exist on how azimuth and elevation are measured. Using the `azccw'
   and `elplsz' flags, users indicate which conventions shall be used.
   See the descriptions of these input arguments for details.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Find the azimuth/elevation state of Venus as seen from the
      DSS-14 station at a given epoch. Map this state back to
      rectangular coordinates as a check.

      Task description
      ================

      In this example, we will obtain the apparent state of Venus as
      seen from the DSS-14 station in the DSS-14 topocentric
      reference frame. We will use a station frames kernel and
      transform the resulting rectangular coordinates to azimuth,
      elevation and range and its derivatives using recazl_c and
      dazldr_c.

      We will map this state back to rectangular coordinates using
      azlrec_c and drdazl_c.

      In order to introduce the usage of the logical flags `azccw'
      and `elplsz', we will request the azimuth to be measured
      clockwise and the elevation positive towards +Z
      axis of the DSS-14_TOPO reference frame.

      Kernels
      =======

      Use the meta-kernel shown below to load the required SPICE
      kernels.


         KPL/MK

         File name: dazldr_ex1.tm

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
         Program dazldr_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Local parameters
         ./
         #define META         "dazldr_ex1.tm"

         /.
         Local variables
         ./
         SpiceChar          * abcorr;
         SpiceChar          * obs;
         SpiceChar          * obstim;
         SpiceChar          * ref;
         SpiceChar          * target;

         SpiceDouble          az;
         SpiceDouble          azlvel [3];
         SpiceDouble          drectn [3];
         SpiceDouble          el;
         SpiceDouble          et;
         SpiceDouble          jacobi [3][3];
         SpiceDouble          lt;
         SpiceDouble          state  [6];
         SpiceDouble          r;
         SpiceDouble          rectan [3];

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
         Compute the observer-target state.
         ./
         spkezr_c ( target, et, ref, abcorr, obs, state, &lt );

         /.
         Convert position to azimuth/elevation coordinates,
         with azimuth increasing clockwise and elevation
         positive towards +Z axis of the DSS-14_TOPO
         reference frame
         ./
         azccw  = SPICEFALSE;
         elplsz = SPICETRUE;

         recazl_c ( state, azccw, elplsz, &r, &az, &el );

         /.
         Convert velocity to azimuth/elevation coordinates.
         ./
         dazldr_c ( state[0], state[1], state[2], azccw, elplsz, jacobi );

         mxv_c ( jacobi, state+3, azlvel );

         /.
         As a check, convert the azimuth/elevation state back to
         rectangular coordinates.
         ./
         azlrec_c ( r, az, el, azccw, elplsz, rectan );

         drdazl_c ( r, az, el, azccw, elplsz, jacobi );

         mxv_c ( jacobi, azlvel, drectn );

         printf( "\n" );
         printf( "AZ/EL coordinates:\n" );
         printf( "\n" );
         printf( "   Range      (km)        =  %19.8f\n", r );
         printf( "   Azimuth    (deg)       =  %19.8f\n", az * dpr_c() );
         printf( "   Elevation  (deg)       =  %19.8f\n", el * dpr_c() );
         printf( "\n" );
         printf( "AZ/EL velocity:\n" );
         printf( "\n" );
         printf( "   d Range/dt     (km/s)  =  %19.8f\n", azlvel[0] );
         printf( "   d Azimuth/dt   (deg/s) =  %19.8f\n",
                                      azlvel[1] * dpr_c() );
         printf( "   d Elevation/dt (deg/s) =  %19.8f\n",
                                      azlvel[2] * dpr_c() );
         printf( "\n" );
         printf( "Rectangular coordinates:\n" );
         printf( "\n" );
         printf( "   X (km)                 =  %19.8f\n", state[0] );
         printf( "   Y (km)                 =  %19.8f\n", state[1] );
         printf( "   Z (km)                 =  %19.8f\n", state[2] );
         printf( "\n" );
         printf( "Rectangular velocity:\n" );
         printf( "\n" );
         printf( "   dX/dt (km/s)           =  %19.8f\n", state[3] );
         printf( "   dY/dt (km/s)           =  %19.8f\n", state[4] );
         printf( "   dZ/dt (km/s)           =  %19.8f\n", state[5] );
         printf( "\n" );
         printf( "Rectangular coordinates from inverse mapping:\n" );
         printf( "\n" );
         printf( "   X (km)                 =  %19.8f\n", rectan[0] );
         printf( "   Y (km)                 =  %19.8f\n", rectan[1] );
         printf( "   Z (km)                 =  %19.8f\n", rectan[2] );
         printf( "\n" );
         printf( "Rectangular velocity from inverse mapping:\n" );
         printf( "\n" );
         printf( "   dX/dt (km/s)           =  %19.8f\n", drectn[0] );
         printf( "   dY/dt (km/s)           =  %19.8f\n", drectn[1] );
         printf( "   dZ/dt (km/s)           =  %19.8f\n", drectn[2] );
         printf( "\n" );

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      AZ/EL coordinates:

         Range      (km)        =   245721478.99272084
         Azimuth    (deg)       =         294.48543372
         Elevation  (deg)       =         -48.94609726

      AZ/EL velocity:

         d Range/dt     (km/s)  =          -4.68189834
         d Azimuth/dt   (deg/s) =           0.00402256
         d Elevation/dt (deg/s) =          -0.00309156

      Rectangular coordinates:

         X (km)                 =    66886767.37916667
         Y (km)                 =   146868551.77222887
         Z (km)                 =  -185296611.10841590

      Rectangular velocity:

         dX/dt (km/s)           =        6166.04150307
         dY/dt (km/s)           =      -13797.77164550
         dZ/dt (km/s)           =       -8704.32385654

      Rectangular coordinates from inverse mapping:

         X (km)                 =    66886767.37916658
         Y (km)                 =   146868551.77222890
         Z (km)                 =  -185296611.10841590

      Rectangular velocity from inverse mapping:

         dX/dt (km/s)           =        6166.04150307
         dY/dt (km/s)           =      -13797.77164550
         dZ/dt (km/s)           =       -8704.32385654


-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   J. Diaz del Rio     (ODC Space)

-Version

   -CSPICE Version 1.0.0, 07-AUG-2021 (JDR)

-Index_Entries

   Jacobian matrix of AZ/EL w.r.t. rectangular coordinates
   Rectangular to range, azimuth and elevation derivative
   Rectangular to range, AZ and EL velocity conversion

-&
*/

{ /* Begin dazldr_c */

   /*
   Local variables.
   */
   logical            logCazccw;
   logical            logCelplsz;

   /*
   Participate in error tracing.
   */
   chkin_c ( "dazldr_c" );

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
   dazldr_ (  ( doublereal * ) &x,
              ( doublereal * ) &y,
              ( doublereal * ) &z,
              ( logical    * ) &logCazccw,
              ( logical    * ) &logCelplsz,
              ( doublereal * )  jacobi     );

   /*
   Transpose the output matrix to put it in row-major order.
   */
   xpose_c ( jacobi, jacobi );

   chkout_c ( "dazldr_c" );

} /* End dazldr_c */
