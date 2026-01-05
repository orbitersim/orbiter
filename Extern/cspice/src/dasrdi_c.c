/*

-Procedure dasrdi_c ( DAS, read data, integer )

-Abstract

   Read integer data from a range of DAS logical addresses.

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

   void dasrdi_c ( SpiceInt            handle,
                   SpiceInt            first,
                   SpiceInt            last,
                   SpiceInt            data   [] )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   handle     I   DAS file handle.
   first,
   last       I   Bounds of range of DAS integer logical addresses.
   data       O   Data having addresses `first' through `last'.

-Detailed_Input

   handle      is a file handle for an open DAS file.

   first,
   last        are the lower and upper bounds of a range of DAS integer
               logical addresses. The range includes these bounds. `first'
               and `last' must be greater than or equal to 1 and less than
               or equal to the highest integer DAS address in the DAS
               file designated by `handle'.

-Detailed_Output

   data        is an array of integers. `data' should have length
               at least last - first + 1.

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

   This routine provides random read access to the integer data in
   a DAS file. This data are logically structured as a
   one-dimensional array of integers.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Create a new DAS file and add 200 integers to it. Close the
      file, then re-open it and read the data back out.


      Example code begins here.


      /.
         Program dasrdi_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Local parameters.
         ./
         #define FNAME        "dasrdi_ex1.das"
         #define TYPE         "TEST"

         /.
         Local variables.
         ./
         SpiceInt             data   [200];

         SpiceInt             handle;
         SpiceInt             i;
         SpiceInt             j;

         /.
         Open a new DAS file. Use the file name as the internal
         file name, and reserve no records for comments.
         ./
         dasonw_c ( FNAME, TYPE, FNAME, 0, &handle );

         /.
         Fill the array `data' with the integers 1 through
         100, and add this array to the file.
         ./
         for ( i = 1; i <= 100; i++ )
         {
            data[i-1] = i;
         }

         dasadi_c ( handle, 100, data );

         /.
         Now append the array `data' to the file again.
         ./
         dasadi_c ( handle, 100, data );

         /.
         Close the file.
         ./
         dascls_c ( handle );

         /.
         Now verify the addition of data by opening the
         file for read access and retrieving the data.
         ./
         dasopr_c ( FNAME, &handle );
         dasrdi_c ( handle, 1, 200, data );

         /.
         Dump the data to the screen.  We should see the
         sequence  1, 2, ..., 100, 1, 2, ... , 100.
         ./
         printf( "\n" );
         printf( "Data from \"%s\":\n", FNAME );
         printf( "\n" );
         for ( i = 0; i < 20; i++ )
         {
            for ( j = 0; j < 10; j++ )
            {
               printf( "%5d", data[i*10+j] );
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


      Data from "dasrdi_ex1.das":

          1    2    3    4    5    6    7    8    9   10
         11   12   13   14   15   16   17   18   19   20
         21   22   23   24   25   26   27   28   29   30
         31   32   33   34   35   36   37   38   39   40
         41   42   43   44   45   46   47   48   49   50
         51   52   53   54   55   56   57   58   59   60
         61   62   63   64   65   66   67   68   69   70
         71   72   73   74   75   76   77   78   79   80
         81   82   83   84   85   86   87   88   89   90
         91   92   93   94   95   96   97   98   99  100
          1    2    3    4    5    6    7    8    9   10
         11   12   13   14   15   16   17   18   19   20
         21   22   23   24   25   26   27   28   29   30
         31   32   33   34   35   36   37   38   39   40
         41   42   43   44   45   46   47   48   49   50
         51   52   53   54   55   56   57   58   59   60
         61   62   63   64   65   66   67   68   69   70
         71   72   73   74   75   76   77   78   79   80
         81   82   83   84   85   86   87   88   89   90
         91   92   93   94   95   96   97   98   99  100


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

   read integer data from a DAS file

-&
*/

{ /* Begin dasrdi_c */

   /*
   Participate in error tracing.
   */
   chkin_c ( "dasrdi_c" );

   /*
   Call the f2c'd Fortran routine.
   */
   dasrdi_ (  ( integer    * ) &handle,
              ( integer    * ) &first,
              ( integer    * ) &last,
              ( integer    * )  data   );

   chkout_c ( "dasrdi_c" );

} /* End dasrdi_c */
