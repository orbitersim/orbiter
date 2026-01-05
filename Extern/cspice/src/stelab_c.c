/*

-Procedure stelab_c     ( Stellar Aberration )

-Abstract

   Correct the apparent position of an object for stellar
   aberration.

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
   #undef    stelab_c


   void stelab_c ( ConstSpiceDouble   pobj[3],
                   ConstSpiceDouble   vobs[3],
                   SpiceDouble        appobj[3] )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   pobj       I   Position of an object with respect to the
                  observer.
   vobs       I   Velocity of the observer with respect to the
                  Solar System barycenter.
   appobj     O   Apparent position of the object with respect to
                  the observer, corrected for stellar aberration.

-Detailed_Input

   pobj        is the position (x, y, z, km) of an object with
               respect to the observer, possibly corrected for
               light time.

   vobs        is the velocity (dx/dt, dy/dt, dz/dt, km/sec)
               of the observer with respect to the Solar System
               barycenter.

-Detailed_Output

   appobj      is the apparent position of the object relative
               to the observer, corrected for stellar aberration.

-Parameters

   None.

-Exceptions

   1)  If the velocity of the observer is greater than or equal
       to the speed of light, the error SPICE(VALUEOUTOFRANGE)
       is signaled by a routine in the call tree of this routine.

-Files

   None.

-Particulars

   Let r be the vector from the observer to the object, and v be
       -                                                    -
   the velocity of the observer with respect to the Solar System
   barycenter. Let w be the angle between them. The aberration
   angle phi is given by

        sin(phi) = v sin(w) / c

   Let h be the vector given by the cross product
       -

         h = r X v
         -   -   -

   Rotate r by phi radians about h to obtain the apparent position
          -                      -
   of the object.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Compute the apparent position of the Moon relative to the
      Earth, corrected for one light-time and stellar aberration,
      given the geometric state of the Earth relative to the Solar
      System Barycenter, and the difference between the stellar
      aberration corrected and uncorrected position vectors, taking
      several steps.

      First, compute the light-time corrected state of the Moon body
      as seen by the Earth, using its geometric state. Then apply
      the correction for stellar aberration to the light-time
      corrected state of the target body.

      The code in this example could be replaced by a single call
      to spkpos_c:

         spkpos_c ( "MOON", et, "J2000", "LT+S", "EARTH", pos, &lt );


      Use the meta-kernel shown below to load the required SPICE
      kernels.


         KPL/MK

         File name: stelab_ex1.tm

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
         Program stelab_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main ()
      {

         /.
         Local variables.
         ./
         SpiceChar             * reffrm;
         SpiceChar             * utcstr;

         SpiceDouble             appdif [ 3 ];
         SpiceDouble             et;
         SpiceDouble             lt;
         SpiceDouble             pcorr  [ 3 ];
         SpiceDouble             pos    [ 3 ];
         SpiceDouble             sobs   [ 6 ];

         SpiceInt                idobs;
         SpiceInt                idtarg;

         /.
         Assign an observer, Earth, target, Moon, time of interest and
         reference frame for returned vectors.
         ./
         idobs  = 399;
         idtarg = 301;
         utcstr = "July 4 2004";
         reffrm = "J2000";

         /.
         Load the needed kernels.
         ./
         furnsh_c ( "stelab_ex1.tm" );

         /.
         Convert the time string to ephemeris time, J2000.
         ./
         str2et_c ( utcstr, &et );

         /.
         Get the state of the observer with respect to the solar
         system barycenter.
         ./
         spkssb_c ( idobs,  et, reffrm, sobs );

         /.
         Get the light-time corrected position `pos' of the target
         body `idtarg' as seen by the observer.
         ./
         spkapo_c ( idtarg, et, reffrm, sobs, "LT", pos, &lt );

         /.
         Output the uncorrected vector.
         ./
         printf ( "Uncorrected position vector\n" );
         printf ( "   %18.6f %18.6f %18.6f\n", pos[0], pos[1], pos[2] );

         /.
         Apply the correction for stellar aberration to the
         light-time corrected position of the target body.
         ./
         stelab_c ( pos, sobs+3, pcorr );

         /.
         Output the corrected position vector and the apparent
         difference from the uncorrected vector.
         ./
         printf ( "\n" );
         printf ( "Corrected position vector\n" );
         printf ( "   %18.6f %18.6f %18.6f\n",
                   pcorr[0], pcorr[1], pcorr[2] );

         /.
         Apparent difference.
         ./
         vsub_c ( pos, pcorr, appdif );
         printf ( "\n" );
         printf ( "Apparent difference\n" );
         printf ( "   %18.6f %18.6f %18.6f\n",
                   appdif[0], appdif[1], appdif[2] );


         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Uncorrected position vector
              201738.725087     -260893.141602     -147722.589056

      Corrected position vector
              201765.929516     -260876.818077     -147714.262441

      Apparent difference
                 -27.204429         -16.323525          -8.326615


-Restrictions

   None.

-Literature_References

   [1]  W. Owen, "The Treatment of Aberration in Optical Navigation",
        JPL IOM #314.8-524, 8 February 1985.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   W.L. Taber          (JPL)
   I.M. Underwood      (JPL)

-Version

   -CSPICE Version 1.0.2, 05-JUL-2021 (JDR)

       Edited the header to comply with NAIF standard.

       Added example's meta-kernel and problem statement. Created complete
       code example from existing code fragments.

   -CSPICE Version 1.0.1, 08-JAN-2008 (NJB)

       The header example was updated to remove references
       to spkapp_c.

   -CSPICE Version 1.0.0, 22-OCT-1998 (NJB) (IMU) (WLT)

       Based on SPICELIB Version 1.0.2, 10-MAR-1992 (WLT)

-Index_Entries

   stellar aberration

-&
*/

{ /* Begin stelab_c */

   /*
   Participate in error tracing.
   */
   chkin_c ( "stelab_c" );


   /*
   Call the f2c'd routine.
   */
   stelab_ ( ( doublereal * ) pobj,
             ( doublereal * ) vobs,
             ( doublereal * ) appobj );


   chkout_c ( "stelab_c" );

} /* End stelab_c */
