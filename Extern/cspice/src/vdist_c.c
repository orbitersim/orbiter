/*

-Procedure vdist_c ( Vector distance )

-Abstract

   Return the distance between two three-dimensional vectors.

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

   VECTOR

*/

   #include "SpiceUsr.h"
   #undef    vdist_c


   SpiceDouble vdist_c ( ConstSpiceDouble v1[3],
                         ConstSpiceDouble v2[3] )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------

   v1,
   v2         I   Two 3-vectors.

   The function returns the distance between v1 and v2.

-Detailed_Input

   v1,
   v2          are two vectors in three-dimensional space, the
               distance between which is desired.

-Detailed_Output

   The function returns the distance between v1 and v2. This is
   defined as

            ||  v1 - v2  ||,

   where || x || indicates the Euclidean norm of the vector x.

-Parameters

   None.

-Exceptions

   Error free.

-Files

   None.

-Particulars

   This function is simply shorthand for the code

      vsub_c ( v1, v2, diff );

      dist = vnorm_c ( diff );

   Using this function saves you the annoyance of declaring local
   storage for the difference vector `diff'.


   The Euclidean norm of a three-dimensional vector (x, y, z) is
   defined as

                                   1/2
           2        2        2
      (   x    +   y    +   z    ).


   This number is the distance of the point (x, y, z) from the
   origin. If `a' and `b' are two vectors whose components are

      ( a[0], a[1], a[2] )    and    ( b[0], b[1], b[2] ),

   then the distance between `a' and `b' is the norm of the difference
   a - b, which has components


      (  a[0] - b[0],  a[1] - b[1],  a[2] - b[2]  ).


   A related routine is vdistg_c, which computes the distance between
   two vectors of general dimension.

-Examples

   The numerical results shown for these examples may differ across
   platforms. The results depend on the SPICE kernels used as input,
   the compiler and supporting libraries, and the machine specific
   arithmetic implementation.

   1) Define two three-dimensional vectors and calculate the distance
      between them.

      Example code begins here.


      /.
         Program vdist_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main( )
      {
         /.
         Local variables.
         ./
         SpiceDouble             dist;

         /.
         Define the vectors, and calculate the distance between the
         coordinates.
         ./
         SpiceDouble             v1  [ 3 ]  = { 1.0, 0.0, 0.0 };
         SpiceDouble             v2  [ 3 ]  = { 0.0, 1.0, 0.0 };

         dist = vdist_c ( v1, v2 );
         printf( " Distance between v1 and v2: %12.6f\n", dist );

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


       Distance between v1 and v2:     1.414214


   2) Given the planetocentric coordinates of a point on the surface of
      Mars, compute the distance between that point and Phobos.

      Use the meta-kernel shown below to load the required SPICE
      kernels.


         KPL/MK

         File: vdist_ex2.tm

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


         \begindata

            KERNELS_TO_LOAD = ( 'de430.bsp',
                                'mar097.bsp',
                                'pck00010.tpc',
                                'naif0011.tls'  )

         \begintext

         End of meta-kernel


      Example code begins here.


      /.
         Program vdist_ex2
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main( )
      {
         /.
         Local variables
         ./
         SpiceChar             * epoch;

         SpiceDouble             dist;
         SpiceDouble             et;
         SpiceDouble             lat;
         SpiceDouble             lon;
         SpiceDouble             lt;
         SpiceDouble             pos    [ 3 ];
         SpiceDouble             radius;
         SpiceDouble             rover  [ 3 ];

         /.
         Load the kernels.
         ./
         furnsh_c( "vdist_ex2.tm" );

         /.
         Define the point on the surface of Mars by its planetocentric
         coordinates, and the epoch.
         ./
         epoch  = "2018-07-25 17:14";
         lon    =    8.544377 * rpd_c( );
         lat    =   42.880602 * rpd_c( );
         radius = 3380.0;

         /.
         Convert that point coordinates to rectangular.
         ./
         latrec_c ( radius, lon, lat, rover );

         /.
         Convert the UTC epoch to ephemeris time.
         ./
         str2et_c ( epoch, &et );

         /.
         Compute the position of Phobos with respect to Mars in IAU_MARS
         body-fixed reference frame.
         ./
         spkpos_c ( "PHOBOS", et,  "IAU_MARS", "NONE", "MARS",
                     pos,    &lt                              );

         /.
         Compute the distance between Phobos and the point on the surface
         of Mars.
         ./
         dist = vdist_c ( rover, pos );
         printf( " Epoch: %s\n", epoch );
         printf( " Distance between location and Phobos (km): %12.6f\n",
                                                                  dist );

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


       Epoch: 2018-07-25 17:14
       Distance between location and Phobos (km):  7174.781393


-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   E.D. Wright         (JPL)

-Version

   -CSPICE Version 1.2.1, 25-AUG-2021 (JDR)

       Edited the header to comply with NAIF standard.

       Added two complete examples to the -Examples section.

   -CSPICE Version 1.2.0, 22-OCT-1998 (NJB)

       Made input vectors const. Removed #include of SpiceZfc.h.

   -CSPICE Version 1.1.0, 06-MAR-1998 (EDW)

       Removed non printing character.

   -CSPICE Version 1.0.0, 08-FEB-1998 (EDW)

-Index_Entries

   distance between 3-dimensional vectors

-&
*/

{ /* Begin vdist_c */

   /*
   Local constants
   */

   SpiceDouble    diff[3];


   /* Function Body */

   vsub_c ( v1, v2, diff);


   return  vnorm_c (diff);

} /* End vdist_c */
