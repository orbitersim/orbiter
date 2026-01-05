/*

-Procedure orderc_c ( Order of a character array )

-Abstract

   Determine the order of elements in an array of character strings.

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

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZst.h"
   #include "SpiceZmc.h"
   #undef    orderc_c


   void orderc_c ( SpiceInt      arrlen,
                   const void  * array,
                   SpiceInt      ndim,
                   SpiceInt    * iorder  )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   arrlen     I   String length.
   array      I   Input array.
   ndim       I   Dimension of array.
   iorder     O   Order vector for array.

-Detailed_Input

   arrlen      is the declared length of the strings in the input
               string array, including null terminators. The input
               array should be declared with dimension

                  [ndim][arrlen]

   array       is the input array.

   ndim        is the number of elements in the input array.

-Detailed_Output

   iorder      is the order vector for the input array.
               iorder[0] is the index of the smallest element
               of array; iorder[1] is the index of the next
               smallest; and so on. Strings are ordered according
               to the ASCII collating sequence. Trailing white space
               is ignored when comparing strings.

               The elements of iorder range from zero to ndim-1.

-Parameters

   None.

-Exceptions

   1)  If ndim < 1, this routine returns immediately. This case is not
       considered an error.

   2)  If the `array' input array pointer is null, the error
       SPICE(NULLPOINTER) is signaled.

   3)  If the `array' input array strings have length less than two
       characters, the error SPICE(STRINGTOOSHORT) is signaled.

-Files

   None.

-Particulars

   orderc_c finds the index of the smallest element of the input
   array. This becomes the first element of the order vector.
   The process is repeated for the rest of the elements.

   The order vector returned by orderc_c may be used by any of
   the reord* routines to sort sets of related arrays, as shown
   in the example below.

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
         Program orderc_ex1
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

   1)  String comparisons performed by this routine are Fortran-style:
       trailing blanks in the input array or key value are ignored.
       This gives consistent behavior with CSPICE code generated by
       the f2c translator, as well as with the Fortran SPICE Toolkit.

       Note that this behavior is not identical to that of the ANSI
       C library functions strcmp and strncmp.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   I.M. Underwood      (JPL)

-Version

   -CSPICE Version 1.1.0, 04-AUG-2021 (JDR)

       Changed input argument name "lenvals" to "arrlen" for consistency
       with other routines.

       Edited the header to comply with NAIF standard.
       Added complete code example.

   -CSPICE Version 1.0.0, 18-JUL-2002 (NJB) (IMU)

-Index_Entries

   order of a character array

-&
*/

{ /* Begin orderc_c */


   /*
   Local variables
   */
   SpiceChar             * fCvalsArr;

   SpiceInt                fCvalsLen;
   SpiceInt                i;


   /*
   Participate in error tracing.
   */
   chkin_c ( "orderc_c" );

   /*
   Return immediately if the array dimension is non-positive.
   */
   if ( ndim < 1 )
   {
      chkout_c ( "orderc_c" );
      return;
   }

   /*
   Make sure the input pointer for the string array is non-null
   and that the length arrlen is sufficient.
   */
   CHKOSTR ( CHK_STANDARD, "orderc_c", array, arrlen );


   /*
   Create a Fortran-style string array.
   */
   C2F_MapStrArr ( "orderc_c",
                   ndim, arrlen, array, &fCvalsLen, &fCvalsArr );

   if ( failed_c() )
   {
      chkout_c ( "orderc_c" );
      return;
   }


   /*
   Call the f2c'd routine.
   */
   orderc_ (  ( char       * ) fCvalsArr,
              ( integer    * ) &ndim,
              ( integer    * ) iorder,
              ( ftnlen       ) fCvalsLen     );

   /*
   Free the dynamically allocated array.
   */
   free ( fCvalsArr );


   /*
   Map the order vector elements to the range 0 : ndim-1.
   */
   for ( i = 0;  i < ndim;  i++ )
   {
      --iorder[i];
   }


   chkout_c ( "orderc_c" );

} /* End orderc_c */
