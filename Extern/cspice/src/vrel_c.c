/*

-Procedure vrel_c ( Vector relative difference, 3 dimensions )

-Abstract

   Return the relative difference between two 3-dimensional vectors.

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

   MATH
   VECTOR

*/

   #include "SpiceUsr.h"
   #include "SpiceZmc.h"
   #include "SpiceZim.h"
   #undef   vrel_c

   SpiceDouble vrel_c ( ConstSpiceDouble v1[3],
                        ConstSpiceDouble v2[3]  )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   v1,
   v2         I   Input vectors.

   The function returns the relative difference between two
   3-dimensional vectors.

-Detailed_Input

   v1,
   v2          are two 3-dimensional vectors for which the relative
               difference is to be computed.

-Detailed_Output

   The function returns the relative difference between the two input
   3-dimensional vectors `v1' and `v2'.

   It is defined as:

                        || v1 - v2 ||
      vrel_c   =   ------------------------
                    max ( ||v1||, ||v2|| )

   where ||x|| indicates the Euclidean norm of the vector `x'.

   vrel_c assumes values in the range [0,2]. If both `v1' and `v2' are zero
   vectors then vrel_c is defined to be zero.

-Parameters

   None.

-Exceptions

   Error free.

   1)  If both `v1' and `v2' are zero vectors, then vrel_c is defined
       to be zero.

-Files

   None.

-Particulars

   This function computes the relative difference between two
   3-dimensional vectors as defined above.

   The function vrelg_c may be used to find the relative difference
   for two vectors of general dimension.

-Examples

   This example code fragment computes the relative difference
   between the geometric and light time corrected state of Io
   with respect to Voyager 2 at a given UTC time.

         #include "SpiceUsr.h"
               .
               .
               .
         /.
         The NAIF integer code for Io is 501 and the code for
         Voyager 2 is -32.
         ./

         #define           IO    501
         #define           VG2   -32


         /.
         Local variables
         ./
         SpiceDouble       state [ 6 ];
         SpiceDouble       pos1  [ 3 ];
         SpiceDouble       pos2  [ 3 ];
         SpiceDouble       diff;
         SpiceDouble       lt;
         SpiceDouble       et;

         SpiceChar       * utc = "1979 JUN 25 12:00:00";

         /.
         Load the sample SPK ephemeris file.
        ./
         furnsh_c ( "VG2_JUP.BSP" );


         /.
         Convert the UTC time string to ephemeris time.
         ./
         utc2et_c ( utc, &et );


         /.
         First calculate the geometric state and then the light
         time corrected state.
         ./
         spkez_c ( IO, et, "J2000", "none", VG2, state, &lt );

         vequ_c  ( state, pos1 );

         spkez_c ( IO, et, "J2000", "lt", VG2, state, &lt );

         vequ_c  ( state, pos2 );

         /.
         Call vrel_c to find the relative difference between the
         two states.
         ./
         diff = vrel_c ( pos1, pos2 );

         .
         .
         .

-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   J.M. Lynch          (JPL)
   E.D. Wright         (JPL)

-Version

   -CSPICE Version 1.1.1, 09-APR-2021 (JDR)

       Edited the header to comply with NAIF standard.

   -CSPICE Version 1.1.0, 28-AUG-2001 (NJB)

       Include interface macro definition file SpiceZim.h.

   -CSPICE Version 1.0.1, 13-APR-2000 (NJB)

       Made some minor updates and corrections in the code example.

   -CSPICE Version 1.0.0, 06-JUL-1999 (EDW) (JML)

-Index_Entries

   relative difference of 3-dimensional vectors

-&
*/

{ /* Begin vrel_c */


   /*
   Local variables
   */
   SpiceDouble       nunorm;
   SpiceDouble       denorm;


   /* If the vectors are both zero or equivalent, return 0. */

   nunorm = vdist_c ( v1, v2 );

   if ( nunorm == 0. )
      {
      return 0.;
      }
   else
      {
      denorm = MaxVal( vnorm_c( v1 ), vnorm_c( v2 ) );
      return ( nunorm/denorm );
      }


} /* End vrel_c */
