/*

-Procedure stlabx_c ( Stellar aberration, transmission case )

-Abstract

   Correct the position of a target for the stellar aberration
   effect on radiation transmitted from a specified observer to
   the target.

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

   void stlabx_c ( ConstSpiceDouble    pobj   [3],
                   ConstSpiceDouble    vobs   [3],
                   SpiceDouble         corpos [3] )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   pobj       I   Position of an object with respect to the
                  observer.
   vobs       I   Velocity of the observer with respect to the
                  Solar System barycenter.
   corpos     O   Corrected position of the object.

-Detailed_Input

   pobj        is the cartesian position vector of an object with
               respect to the observer, possibly corrected for
               light time. Units are km.

   vobs        is the cartesian velocity vector of the observer
               with respect to the Solar System barycenter. Units
               are km/s.

-Detailed_Output

   corpos      is the  position of the object relative to the
               observer, corrected for the stellar aberration
               effect on radiation directed toward the target. This
               correction is the inverse of the usual stellar
               aberration correction: the corrected vector
               indicates the direction in which radiation must be
               emitted from the observer, as seen in an inertial
               reference frame having velocity equal to that of the
               observer, in order to reach the position indicated by
               the input vector `pobj'.

-Parameters

   None.

-Exceptions

   1)  If the velocity of the observer is greater than or equal to
       the speed of light, an error is signaled by a routine in the
       call tree of this routine. The outputs are undefined.

-Files

   None.

-Particulars

   In order to transmit radiation from an observer to a specified
   target, the emission direction must be corrected for one way
   light time and for the motion of the observer relative to the
   solar system barycenter. The correction for the observer's
   motion when transmitting to a target is the inverse of the
   usual stellar aberration correction applied to the light-time
   corrected position of the target as seen by the observer.

   Below is the description of the stellar aberration correction
   used in the CSPICE routine stelab_c (with the notation changed
   slightly):

      Let `r' be the vector from the observer to the object, and `v' be
      the velocity of the observer with respect to the Solar System
      barycenter. Let `w' be the angle between them. The aberration
      angle `phi' is given by

         sin(phi) = v * sin(w) / C

      Let `h' be the vector given by the cross product

         h = r x v

      Rotate `r' by `phi' radians about `h' to obtain the apparent position
      of the object.

   This routine applies the inverse correction, so here the rotation
   about `h' is by -phi radians.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Compute the apparent position of the Moon relative to the
      Earth, corrected for one way light-time and stellar aberration
      effect on radiation transmitted from the Earth to the Moon,
      given the geometric state of the Earth relative to the Solar
      System Barycenter, and the difference between the stelar
      aberration corrected and uncorrected position vectors, taking
      several steps.

      First, compute the light-time corrected state of the Moon body
      as seen by the Earth, using its geometric state. Then apply
      the correction for stellar aberration to the light-time
      corrected state of the target body, both for the transmission
      case.

      The code in this example could be replaced by a single call
      to spkpos_c:

          spkpos_c ( "MOON", et, "J2000", "XLT+S", "EARTH", pos, &lt );


      Use the meta-kernel shown below to load the required SPICE
      kernels.


         KPL/MK

         File name: stlabx_ex1.tm

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
            de418.bsp                     Planetary ephemeris
            naif0009.tls                  Leapseconds

         \begindata

            KERNELS_TO_LOAD = ( 'de418.bsp',
                                'naif0009.tls'  )

         \begintext

         End of meta-kernel


      Example code begins here.


      /.
         Program stlabx_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Local variables.
         ./
         SpiceChar          * reffrm;
         SpiceChar          * utcstr;

         SpiceDouble          appdif [3];
         SpiceDouble          et;
         SpiceDouble          lt;
         SpiceDouble          pcorr  [3];
         SpiceDouble          pos    [3];
         SpiceDouble          sobs   [6];

         SpiceInt             idobs;
         SpiceInt             idtarg;

         /.
         Assign an observer, Earth, target, Moon, time of interest
         and reference frame for returned vectors.
         ./
         idobs  = 399;
         idtarg = 301;
         utcstr = "July 4 2004";
         reffrm = "J2000";

         /.
         Load the needed kernels.
         ./
         furnsh_c ( "stlabx_ex1.tm" );

         /.
         Convert the time string to ephemeris time.
         ./
         str2et_c ( utcstr, &et );

         /.
         Get the state of the observer with respect to the solar
         system barycenter.
         ./
         spkssb_c ( idobs, et, reffrm, sobs );

         /.
         Get the light-time corrected position `pos' of the target
         body `idtarg' as seen by the observer. Normally we would
         call spkpos_c to obtain this vector, but we already have
         the state of the observer relative to the solar system
         barycenter, so we can avoid looking up that state twice
         by calling spkapo_c.
         ./
         spkapo_c ( idtarg, et, reffrm, sobs, "XLT", pos, &lt );

         /.
         Output the uncorrected vector.
         ./
         printf( "Uncorrected position vector\n" );
         printf( "    %18.6f %18.6f %18.6f\n", pos[0], pos[1], pos[2] );

         /.
         Apply the correction for stellar aberration to the
         light-time corrected position of the target body.
         ./
         stlabx_c ( pos, sobs+3, pcorr );

         /.
         Output the corrected position vector and the apparent
         difference from the uncorrected vector.
         ./
         printf( "\n" );
         printf( "Corrected position vector\n" );
         printf( "    %18.6f %18.6f %18.6f\n", pcorr[0], pcorr[1], pcorr[2] );

         /.
         Apparent difference.
         ./
         vsub_c ( pos, pcorr, appdif );
         printf( "\n" );
         printf( "Apparent difference\n" );
         printf( "    %18.6f %18.6f %18.6f\n",
                 appdif[0], appdif[1], appdif[2] );

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Uncorrected position vector
               201809.933536     -260878.049826     -147716.077987

      Corrected position vector
               201782.730972     -260894.375627     -147724.405897

      Apparent difference
                   27.202563          16.325802           8.327911


-Restrictions

   None.

-Literature_References

   [1]  W. Owen, "The Treatment of Aberration in Optical Navigation",
        JPL IOM #314.8-524, 8 February 1985.

-Author_and_Institution

   J. Diaz del Rio     (ODC Space)

-Version

   -CSPICE Version 1.0.0, 13-AUG-2021 (JDR)

-Index_Entries

   stellar aberration for transmission case

-&
*/

{ /* Begin stlabx_c */

   /*
   Participate in error tracing.
   */
   chkin_c ( "stlabx_c" );

   /*
   Call the f2c'd Fortran routine.
   */
   stlabx_ (  ( doublereal * )  pobj,
              ( doublereal * )  vobs,
              ( doublereal * )  corpos  );

   chkout_c ( "stlabx_c" );

} /* End stlabx_c */
