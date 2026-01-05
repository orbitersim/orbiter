/*

-Procedure dasadi_c ( DAS, add data, integer )

-Abstract

   Add an array of integers to a DAS file.

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

   void dasadi_c ( SpiceInt            handle,
                   SpiceInt            n,
                   ConstSpiceInt       data   [] )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   handle     I   DAS file handle.
   n          I   Number of integers to add to DAS file.
   data       I   Array of integers to add.

-Detailed_Input

   handle      is a file handle of a DAS file opened for writing.

   n           is the number of integer "words" to add to the DAS file
               specified by `handle'.

   data        is an array of integers to be added to the specified DAS
               file. Elements 0 through n-1 are appended to the integer
               data in the file.

-Detailed_Output

   None.

   See -Particulars for a description of the effect of this routine.

-Parameters

   None.

-Exceptions

   1)  If the input file handle is invalid, an error is signaled by a
       routine in the call tree of this routine.

   2)  If an I/O error occurs during the data addition attempted by
       this routine, the error is signaled by a routine in the call
       tree of this routine.

   3)  If the input count `n' is less than 1, no data will be added to
       the specified DAS file. No error will be signaled.

-Files

   See the description of the argument `handle' in -Detailed_Input.

-Particulars

   This routine adds integer data to a DAS file by "appending" them
   after any integer data already in the file. The sense in which
   the data are "appended" is that the data will occupy a range of
   logical addresses for integer data that immediately follow the
   last logical address of an integer that is occupied at the time
   this routine is called. The diagram below illustrates this
   addition:

      +-------------------------+
      |    (already in use)     |  Integer logical address 1
      +-------------------------+
                  .
                  .
                  .
      +-------------------------+
      |    (already in use)     |  last integer logical address
      +-------------------------+  in use before call to dasadi_c
      |        data[0]          |
      +-------------------------+
                  .
                  .
                  .
      +-------------------------+
      |        data[n-1]        |
      +-------------------------+


   The logical organization of the integers in the DAS file is
   independent of the location in the file of any data of double
   precision or character type.

   The actual physical write operations that add the input array
   `data' to the indicated DAS file might not take place before this
   routine returns, since the DAS system buffers data that are
   written as well as data that are read. In any case, the data
   will be flushed to the file at the time the file is closed, if
   not earlier. A physical write of all buffered records can be
   forced by calling the CSPICE routine daswbr_c (DAS, write
   buffered records).

   In order to update integer logical addresses that already contain
   data, the CSPICE routine dasudi_c (DAS update data, integer)
   should be used.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Create a new DAS file and add 200 integers to it. Close the
      file, then re-open it and read the data back out.


      Example code begins here.


      /.
         Program dasadi_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Local parameters.
         ./
         #define FNAME        "dasadi_ex1.das"
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


      Data from "dasadi_ex1.das":

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

   add integer data to a DAS file

-&
*/

{ /* Begin dasadi_c */

   /*
   Participate in error tracing.
   */
   chkin_c ( "dasadi_c" );

   /*
   Call the f2c'd Fortran routine.
   */
   dasadi_ (  ( integer    * ) &handle,
              ( integer    * ) &n,
              ( integer    * )  data   );

   chkout_c ( "dasadi_c" );

} /* End dasadi_c */
