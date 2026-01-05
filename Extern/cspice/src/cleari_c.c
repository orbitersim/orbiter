/*

-Procedure cleari_c ( Clear an integer array )

-Abstract

   Fill an integer array with zeros.

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
   ASSIGNMENT

*/
   #include "SpiceUsr.h"


   void cleari_c ( SpiceInt            ndim,
                   SpiceInt            array  [] )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   ndim       I   The number of elements of `array' which are to be
                  set to zero.
   array      O   Integer array to be filled.

-Detailed_Input

   ndim        is the number of elements in `array' which are to be
               set to zero.

-Detailed_Output

   array       is the integer array which is to be filled with
               zeros.

-Parameters

   None.

-Exceptions

   Error free.

   1)  If ndim < 1, the array is not modified.

-Files

   None.

-Particulars

   None.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Initialize all members of an integer array to the same value
      and clear it afterwards.


      Example code begins here.


      /.
         Program cleari_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Local parameters.
         ./
         #define NDIM         4

         /.
         Local variables.
         ./
         SpiceInt             array  [NDIM];

         /.
         Initialize all member of the array `array' to 11, and
         print out its contents.
         ./
         filli_c ( 11, NDIM, array );

         printf( "Contents of ARRAY before cleari_c:\n" );
         printf( "%4d %3d %3d %3d\n",
                 (int)array[0], (int)array[1], (int)array[2], (int)array[3] );

         /.
         Clear the contents of `array' and print it.
         ./
         cleari_c ( NDIM, array );

         printf( "\n" );
         printf( "Contents of ARRAY after cleari_c:\n" );
         printf( "%4d %3d %3d %3d\n",
                (int)array[0], (int)array[1], (int)array[2], (int)array[3] );

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Contents of ARRAY before cleari_c:
        11  11  11  11

      Contents of ARRAY after cleari_c:
         0   0   0   0


-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   J. Diaz del Rio     (ODC Space)

-Version

   -CSPICE Version 1.0.0, 19-MAY-2021 (JDR)

-Index_Entries

   clear an integer array

-&
*/

{ /* Begin cleari_c */

   /*
   Local variables
   */
   SpiceInt                i;

   /*
   Error free:  no error tracing required.
   */

   for ( i = 0;  i < ndim;  i++ )
   {
      array[i] = 0;
   }

} /* End cleari_c */
