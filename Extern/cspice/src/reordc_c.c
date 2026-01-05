/*

-Procedure reordc_c ( Reorder a character array )

-Abstract

   Reorder the elements of an array of character strings according to
   a given order vector.

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

   ARRAY
   SORT

*/

   #include <stdlib.h>
   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZst.h"
   #include "SpiceZmc.h"
   #include "SpiceZim.h"
   #include "zzalloc.h"
   #undef    reordc_c


   void reordc_c ( ConstSpiceInt  * iorder,
                   SpiceInt         ndim,
                   SpiceInt         arrlen,
                   void           * array    )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   iorder     I   Order vector to be used to re-order array.
   ndim       I   Dimension of array.
   arrlen     I   String length.
   array     I-O  Array to be re-ordered.

-Detailed_Input

   iorder      is the order vector to be used to re-order the input
               array. The first element of iorder is the index of
               the first item of the re-ordered array, and so on.

               Note that the order imposed by reordc_c is not the
               same order that would be imposed by a sorting
               routine. In general, the order vector will have
               been created (by one of the order routines) for
               a related array, as illustrated in the example below.

               The elements of iorder range from zero to ndim-1.

   ndim        is the number of elements in the input array.

   arrlen      is the declared length of the strings in the input
               string array, including null terminators. The input
               array should be declared with dimension

                  [ndim][arrlen]

   array       on input, is an array containing some number of
               elements in unspecified order.

-Detailed_Output

   array       on output, is the same array, with the elements
               re-ordered as specified by iorder.

-Parameters

   None.

-Exceptions

   1)  If ndim < 2, this routine executes a no-op. This case is
       not an error.

   2)  If the `array' input array pointer is null, the error
       SPICE(NULLPOINTER) is signaled.

   3)  If the `array' input array strings have length less than two
       characters, the error SPICE(STRINGTOOSHORT) is signaled.

   4)  If memory cannot be allocated to create the temporary variable
       required for the execution of the underlying Fortran routine,
       the error SPICE(MALLOCFAILED) is signaled.

-Files

   None.

-Particulars

   After re-ordering, the element at index iorder[0] of the input array is
   the first element of the output array, the element at index iorder[1] of
   the input array is the second element of the output array, and so on.

   The order vector used by reordc_c is typically created for
   a related array by one of the order*_c routines, as shown in
   the example below.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Sort four related arrays containing the names, masses,
      integer ID codes, and flags indicating whether they have
      a ring system, for a group of planets.


      Example code begins here.


      /.
         Program reordc_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Local constants.
         ./
         #define NDIM         8
         #define STRLEN       8

         /.
         Local variables.
         ./

         SpiceInt             i;
         SpiceInt             iorder [NDIM];

         /.
         Set the arrays containing the names, masses (given as
         ratios to of Solar GM to barycenter GM), integer ID
         codes, and flags indicating whether they have a ring
         system.
         ./
         SpiceChar            names  [NDIM][STRLEN] = {
                                 "MERCURY", "VENUS",  "EARTH",  "MARS",
                                 "JUPITER", "SATURN", "URANUS", "NEPTUNE" };

         SpiceDouble          masses [NDIM] = {     22032.080,   324858.599,
                                                   398600.436,    42828.314,
                                                126712767.881, 37940626.068,
                                                  5794559.128,  6836534.065 };

         SpiceInt             codes  [NDIM] = { 199, 299, 399, 499,
                                                599, 699, 799, 899 };

         SpiceBoolean         rings  [NDIM] = { SPICEFALSE, SPICEFALSE,
                                                SPICEFALSE, SPICEFALSE,
                                                SPICETRUE,  SPICETRUE,
                                                SPICETRUE,  SPICETRUE  };

         /.
         Sort the object arrays by name.
         ./
         orderc_c ( STRLEN, names, NDIM,   iorder );

         reordc_c ( iorder, NDIM,  STRLEN, names  );
         reordd_c ( iorder, NDIM,  masses         );
         reordi_c ( iorder, NDIM,  codes          );
         reordl_c ( iorder, NDIM,  rings          );

         /.
         Output the resulting table.
         ./
         printf( " Planet   Mass(GMS/GM)  ID Code  Rings?\n" );
         printf( "-------  -------------  -------  ------\n" );

         for ( i = 0; i < NDIM; i++ )
         {

            printf( "%-7s %14.3f %8d %4d\n",
                    names[i], masses[i], codes[i], rings[i] );

         }

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


       Planet   Mass(GMS/GM)  ID Code  Rings?
      -------  -------------  -------  ------
      EARTH       398600.436      399    0
      JUPITER  126712767.881      599    1
      MARS         42828.314      499    0
      MERCURY      22032.080      199    0
      NEPTUNE    6836534.065      899    1
      SATURN    37940626.068      699    1
      URANUS     5794559.128      799    1
      VENUS       324858.599      299    0


-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   W.L. Taber          (JPL)
   I.M. Underwood      (JPL)

-Version

   -CSPICE Version 1.1.0, 27-AUG-2021 (NJB) (JDR)

       Bug fix: complete re-write. Previous version did not perform stated
       function.

       Changed input argument name "lenvals" to "arrlen" for consistency
       with other routines.

       Edited the header to comply with NAIF standard.
       Added complete code example.

   -CSPICE Version 1.0.0, 10-JUL-2002 (NJB) (WLT) (IMU)

-Index_Entries

   reorder a character array

-&
*/

{ /* Begin reordc_c */

   /*
   Local variables
   */
   SpiceChar             * CValsArr;
   SpiceChar             * from;
   SpiceChar             * to;

   SpiceInt                CValsArrSize;
   SpiceInt                i;


   /*
   This implementation doesn't call the f2c'd function reordc_, but rather
   performs the re-ordering in-line. This approach was chosen for
   efficiency, because the usual approach would require allocation of a
   Fortran-style string array for transmission of the input/output string
   array to the f2c'd function, and would incur the time penalty of having
   to traverse the cycles of the array to perform re-ordering.

   Since a dynamically allocated array is needed anyway, using it enables
   this function to quickly create a re-ordered array without the help of
   the f2c'd function.
   */

   /*
   If the input array doesn't have at least two elements, return
   immediately.
   */
   if ( ndim < 2 )
   {
      return;
   }

   /*
   Use discovery check-in.

   Make sure the input pointer for the string array is non-null
   and that the length `arrlen' is sufficient.
   */
   CHKOSTR ( CHK_DISCOVER, "reordc_c", array, arrlen );

   /*
   Allocate an array to hold the re-ordered strings.
   */
   CValsArrSize = ndim * arrlen;

   CValsArr = (SpiceChar *) alloc_SpiceMemory ( CValsArrSize );

   if ( !CValsArr )
   {
      chkin_c( "reordc_c" );

      setmsg_c ( "Attempt to dynamically allocate # bytes failed." );
      errint_c ( "#", CValsArrSize     );
      sigerr_c ( "SPICE(MALLOCFAILED)" );

      chkout_c( "reordc_c" );
      return;
   }

   /*
   Write the input strings to the dynamic array in the order given by
   the input order vector `iorder'.
   */
   for ( i = 0;  i < ndim;  ++i )
   {
      from = (SpiceChar*)array  + ( iorder[i] * arrlen );
      to   = CValsArr           + ( i         * arrlen );

      strncpy( to, from, arrlen );
   }

   /*
   Now overwrite the input array with the re-ordered string array.
   */
   for ( i = 0;  i < ndim;  ++i )
   {
      from = CValsArr           + ( i * arrlen );
      to   = (SpiceChar*)array  + ( i * arrlen );

      strncpy( to, from, arrlen );
   }

   /*
   Free the dynamically allocated array.
   */
   free_SpiceMemory( CValsArr );

   if ( alloc_count() != 0 )
   {
      chkin_c( "reordc_c" );

      ALLOC_CHECK;

      chkout_c( "reordc_c" );
      return;
   }

} /* End reordc_c */
