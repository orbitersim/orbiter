/*

-Procedure vrelg_c ( Vector relative difference, general dimension )

-Abstract

   Return the relative difference between two vectors of general
   dimension.

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
   #undef   vrelg_c

   SpiceDouble vrelg_c ( ConstSpiceDouble * v1,
                         ConstSpiceDouble * v2,
                         SpiceInt           ndim  )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   v1,
   v2         I   Input vectors.
   ndim       I   Dimension of `v1' and `v2'.

   The function returns the relative difference between two vectors
   of general dimension.

-Detailed_Input

   v1,
   v2          are two vectors for which the relative difference is to
               be computed.

   ndim        is the dimension of `v1' and `v2'.

-Detailed_Output

   The function returns the relative difference between the two input
   n-dimensional vectors `v1' and `v2'.

   It is defined as:

                        || v1 - v2 ||
      vrelg_c   =   -------------------------
                     maxd ( ||v1||, ||v2|| )

   where ||x|| indicates the Euclidean norm of the vector `x'.

   vrelg_c assumes values in the range [0,2]. If both `v1' and `v2' are
   zero vectors then vrelg_c is defined to be zero.

-Parameters

   None.

-Exceptions

   Error free.

   1)  If both `v1' and `v2' are zero vectors, then vrelg_c is defined to
       be zero.

-Files

   None.

-Particulars

   This function computes the relative difference between two vectors
   of general dimension as defined above.

   The function vrel_c may be used to find the relative difference
   for two 3-dimensional vectors.

-Examples

   This example determines if the state of Jupiter, with respect
   to Voyager 2, for a set of times is the same for two different
   ephemeris files. Instead of insisting on absolute equality
   between the state vectors, the program will check if the relative
   difference between the vectors is greater than a fixed tolerance.

         #include "SpiceUsr.h"
               .
               .
               .
         /.
         The NAIF code for Jupiter is 599 and for Voyager 2 is -32.
         Set the tolerance to be 0.0005.
         ./

         #define            NUM   500
         #define            JUP   599
         #define            VG2   -32
         #define            TOL   .0005

         /.
         Local variables
         ./
         SpiceDouble        state1 [6][NUM];
         SpiceDouble        state2 [6][NUM];
         SpiceDouble        et     [NUM];
         SpiceDouble        lt;
         SpiceDouble        diff;

         SpiceInt           i;

         .
         .
         .

         /.
         Load  the first SPK file.
         ./
         furnsh_c ( "VG2_SOURCE_1.BSP" );


         /.
         Find the states for each time in the array ET.
         This example assumes that the SPK file can
         provide states for all of the times in the array.
         ./
         for ( i = 0; i < NUM; i++ )
            {
            spkez_c ( JUP, et[i], "J2000", "lt", VG2,
                                   state1[1][i], &lt  );
            }


         /.
         Unload the first file and load the second one.
         ./
         unload_c ( "VG2_SOURCE_1.BSP" );

         furnsh_c ( "VG2_SOURCE_2.BSP" );


         /.
         Find the states from the new file.
         ./
         for ( i = 0; i < NUM; i++ )
            {
            spkez_c ( JUP, et[i], "J2000", "lt",
                      VG2, state2[1][i], &lt  );
            }


         /.
         Now compare the two state vectors for each time.
         ./
         for ( i = 0; i < NUM; i++ )
            {
            diff = vrelg_c ( state1[1][i], state2[1][i], 6 );

            if ( diff > TOL )
               {
                 ...
               }

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

   -CSPICE Version 1.0.1, 09-AUG-2021 (JDR)

       Edited the header to comply with NAIF standard.

   -CSPICE Version 1.0.0, 28-AUG-2001 (NJB)

       Include interface macro definition file SpiceZim.h.
       Made some minor updates and corrections in the code example.

   -CSPICE Version 1.0.0, 06-JUL-1999 (EDW) (JML)

-Index_Entries

   relative difference of n-dimensional vectors

-&
*/

{ /* Begin vrelg_c */

   /*
   Local variables
   */
   SpiceDouble       nunorm;
   SpiceDouble       denorm;



   /* If the vectors are both zero or equivalent, return 0. */

   nunorm = vdistg_c ( v1, v2, ndim );

   if ( nunorm == 0. )
      {
      return 0.;
      }
   else
      {
      denorm = MaxVal( vnormg_c( v1, ndim ), vnormg_c( v2, ndim ) );
      return ( nunorm/denorm );
      }


} /* End vrelg_c */
