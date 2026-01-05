/*

-Procedure dasrdd_c ( DAS, read data, double precision )

-Abstract

   Read double precision data from a range of DAS logical addresses.

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

   DAS

-Keywords

   ARRAY
   ASSIGNMENT
   DAS
   FILES

*/
   #include "SpiceUsr.h"
   #include "SpiceZfc.h"

   void dasrdd_c ( SpiceInt            handle,
                   SpiceInt            first,
                   SpiceInt            last,
                   SpiceDouble         data   [] )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   handle     I   DAS file handle.
   first,
   last       I   Bounds of range of DAS double precision logical
                  addresses.
   data       O   Data having addresses `first' through `last'.

-Detailed_Input

   handle      is a file handle for an open DAS file.

   first,
   last        are the lower and upper bounds of a range of DAS double
               precision logical addresses. The range includes these
               bounds. `first' and `last' must be greater than or equal to 1
               and less than or equal to the highest double precision
               DAS address in the DAS file designated by `handle'.

-Detailed_Output

   data        is an array of double precision numbers. `data'
               should have length at least last - first + 1.

-Parameters

   None.

-Exceptions

   1)  If the input file handle is invalid, an error is signaled
       by a routine in the call tree of this routine. `data' will
       not be modified.

   2)  If `first' or `last' are out of range, an error is signaled
       by a routine in the call tree of this routine.

   3)  If `first' is greater than `last', `data' is left unchanged.

   4)  If `data' is declared with length less than first - last + 1,
       the error cannot be diagnosed by this routine.

   5)  If a file read error occurs, the error is signaled by a
       routine in the call tree of this routine.

-Files

   See the description of the argument `handle' in -Detailed_Input.

-Particulars

   This routine provides random read access to the double precision
   data in a DAS file. This data are logically structured as a
   one-dimensional array of double precision numbers.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Create a new DAS file TEST.DAS and add 200 double
      precision numbers to it. Close the file, then re-open
      it and read the data back out.


      Example code begins here.


      /.
         Program dasrdd_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Local parameters.
         ./
         #define FNAME        "dasrdd_ex1.das"
         #define TYPE         "TEST"

         /.
         Local variables.
         ./
         SpiceDouble          data   [200];

         SpiceInt             handle;
         SpiceInt             i;
         SpiceInt             j;

         /.
         Open a new DAS file. Use the file name as the internal
         file name, and reserve no records for comments.
         ./
         dasonw_c ( FNAME, TYPE, FNAME, 0, &handle );

         /.
         Fill the array `data' with the double precision
         numbers 1.0 through 100.0, and add this array
         to the file.
         ./
         for ( i = 1; i <= 100; i++ )
         {
            data[i-1] = (double)( i );
         }

         dasadd_c ( handle, 100, data );

         /.
         Now append the array `data' to the file again.
         ./
         dasadd_c ( handle, 100, data );

         /.
         Close the file.
         ./
         dascls_c ( handle );

         /.
         Now verify the addition of data by opening the
         file for read access and retrieving the data.
         ./
         dasopr_c ( FNAME, &handle );
         dasrdd_c ( handle, 1, 200, data );

         /.
         Dump the data to the screen.  We should see the
         sequence 1.0, 2.0, ..., 100.0, 1.0, 2.0, ..., 100.0.
         The numbers will be represented as double precision
         numbers in the output.
         ./
         printf( "\n" );
         printf( "Data from \"%s\":\n", FNAME );
         printf( "\n" );
         for ( i = 0; i < 25; i++ )
         {
            for ( j = 0; j < 8; j++ )
            {
               printf( "%7.1f", data[i*8+j] );
            }
            printf( "\n" );
         }

         /.
         Close the file.
         ./
         dascls_c ( handle );

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Data from "dasrdd_ex1.das":

          1.0    2.0    3.0    4.0    5.0    6.0    7.0    8.0
          9.0   10.0   11.0   12.0   13.0   14.0   15.0   16.0
         17.0   18.0   19.0   20.0   21.0   22.0   23.0   24.0
         25.0   26.0   27.0   28.0   29.0   30.0   31.0   32.0
         33.0   34.0   35.0   36.0   37.0   38.0   39.0   40.0
         41.0   42.0   43.0   44.0   45.0   46.0   47.0   48.0
         49.0   50.0   51.0   52.0   53.0   54.0   55.0   56.0
         57.0   58.0   59.0   60.0   61.0   62.0   63.0   64.0
         65.0   66.0   67.0   68.0   69.0   70.0   71.0   72.0
         73.0   74.0   75.0   76.0   77.0   78.0   79.0   80.0
         81.0   82.0   83.0   84.0   85.0   86.0   87.0   88.0
         89.0   90.0   91.0   92.0   93.0   94.0   95.0   96.0
         97.0   98.0   99.0  100.0    1.0    2.0    3.0    4.0
          5.0    6.0    7.0    8.0    9.0   10.0   11.0   12.0
         13.0   14.0   15.0   16.0   17.0   18.0   19.0   20.0
         21.0   22.0   23.0   24.0   25.0   26.0   27.0   28.0
         29.0   30.0   31.0   32.0   33.0   34.0   35.0   36.0
         37.0   38.0   39.0   40.0   41.0   42.0   43.0   44.0
         45.0   46.0   47.0   48.0   49.0   50.0   51.0   52.0
         53.0   54.0   55.0   56.0   57.0   58.0   59.0   60.0
         61.0   62.0   63.0   64.0   65.0   66.0   67.0   68.0
         69.0   70.0   71.0   72.0   73.0   74.0   75.0   76.0
         77.0   78.0   79.0   80.0   81.0   82.0   83.0   84.0
         85.0   86.0   87.0   88.0   89.0   90.0   91.0   92.0
         93.0   94.0   95.0   96.0   97.0   98.0   99.0  100.0


      Note that after run completion, a new DAS file exists in the
      output directory.

-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   J. Diaz del Rio     (ODC Space)

-Version

   -CSPICE Version 1.0.0, 09-MAR-2021 (JDR)

-Index_Entries

   read double precision data from a DAS file

-&
*/

{ /* Begin dasrdd_c */

   /*
   Participate in error tracing.
   */
   chkin_c ( "dasrdd_c" );

   /*
   Call the f2c'd Fortran routine.
   */
   dasrdd_ (  ( integer    * ) &handle,
              ( integer    * ) &first,
              ( integer    * ) &last,
              ( doublereal * )  data   );

   chkout_c ( "dasrdd_c" );

} /* End dasrdd_c */
