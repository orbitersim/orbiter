/*

-Procedure dasopw_c ( DAS, open for write )

-Abstract

   Open a DAS file for writing.

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

   DAS
   FILES

*/
   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZmc.h"
   #undef    dasopw_c

   void dasopw_c ( ConstSpiceChar    * fname,
                   SpiceInt          * handle )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   fname      I   Name of a DAS file to be opened.
   handle     O   Handle assigned to the opened DAS file.

-Detailed_Input

   fname       is the name of a DAS file to be opened with write
               access.

-Detailed_Output

   handle      is the handle that is associated with the file. This
               handle is used to identify the file in subsequent
               calls to other DAS routines.

-Parameters

   None.

-Exceptions

   1)  If the input filename is blank, an error is signaled by a
       routine in the call tree of this routine.

   2)  If the specified file does not exist, an error is signaled by
       a routine in the call tree of this routine.

   3)  If the specified file has already been opened, either by the
       DAS file routines or by other code, an error is signaled by a
       routine in the call tree of this routine. Note that this
       response is not paralleled by dasopr_c, which allows you to open
       a DAS file for reading even if it is already open for reading.

   4)  If the specified file cannot be opened without exceeding the
       maximum allowed number of open DAS files, the error
       SPICE(DASFTFULL) is signaled by a routine in the call tree of
       this routine.

   5)  If the specified file cannot be opened properly, an error
       is signaled by a routine in the call tree of this routine.

   6)  If the file record cannot be read, an error is signaled by a
       routine in the call tree of this routine.

   7)  If the specified file is not a DAS file, as indicated by the
       file's ID word, an error is signaled by a routine in the call
       tree of this routine.

   8)  If no logical units are available, an error is signaled
       by a routine in the call tree of this routine.

   9)  If the `fname' input string pointer is null, the error
       SPICE(NULLPOINTER) is signaled.

   10) If the `fname' input string has zero length, the error
       SPICE(EMPTYSTRING) is signaled.

-Files

   See argument `fname'.

-Particulars

   Most DAS files require only read access. If you do not need to
   change the contents of a file, you should open it with dasopr_c.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Create a new DAS file containing 200 integer addresses set
      to zero. Re-open the file for write access again, and write
      to its addresses 1 through 200 in random-access fashion by
      updating the file.


      Example code begins here.


      /.
         Program dasopw_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Local Parameters
         ./
         #define DASNAM       "dasopw_ex1.das"
         #define IDATLN       200

         /.
         Local Variables
         ./
         SpiceChar          * type;

         SpiceInt             handle;
         SpiceInt             i;
         SpiceInt             idata  [IDATLN];
         SpiceInt             j;

         /.
         Open a new DAS file. Reserve no comment records.
         ./
         type = "TEST";
         dasonw_c ( DASNAM, type, "TEST/DASOPW_EX1", 0, &handle );

         /.
         Append 200 integers to the file; after the data are
         present, we're free to update it in any order we
         please. (cleari_c zeros out an integer array.)
         ./
         cleari_c ( IDATLN, idata );
         dasadi_c ( handle, IDATLN, idata );

         /.
         Close the file.
         ./
         dascls_c ( handle );

         /.
         Open the file again for writing.
         ./
         dasopw_c ( DASNAM, &handle );

         /.
         Reset the data array, and read the data into it.
         ./
         filli_c ( -1, IDATLN, idata );
         dasrdi_c ( handle, 1, IDATLN, idata );

         /.
         Print the contents of the file before updating it.
         ./
         printf( "Contents of %s before update:\n", DASNAM );
         printf( "\n" );
         for ( i = 0; i < 20; i++ )
         {
            for ( j = 0; j < 10; j++ )
            {
               printf( "%5d", idata[i*10+j] );
            }
            printf( "\n" );
         }

         /.
         Now the integer logical addresses 1:200 can be
         written to in random-access fashion. We'll fill them
         in reverse order.
         ./
         for ( i = IDATLN; i >= 1; i -= 1 )
         {
            dasudi_c ( handle, i, i, &i );
         }

         /.
         Close the file.
         ./
         dascls_c ( handle );

         /.
         Now make sure that we updated the file properly.
         Open the file for reading and dump the contents
         of the integer logical addresses 1:200.
         ./
         dasopr_c ( DASNAM, &handle );

         cleari_c ( IDATLN, idata );
         dasrdi_c ( handle, 1, IDATLN, idata );

         printf( "\n" );
         printf( "Contents of %s after update:\n", DASNAM );
         printf( "\n" );
         for ( i = 0; i < 20; i++ )
         {
            for ( j = 0; j < 10; j++ )
            {
               printf( "%5d", idata[i*10+j] );
            }
            printf( "\n" );
         }

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Contents of dasopw_ex1.das before update:

          0    0    0    0    0    0    0    0    0    0
          0    0    0    0    0    0    0    0    0    0
          0    0    0    0    0    0    0    0    0    0
          0    0    0    0    0    0    0    0    0    0
          0    0    0    0    0    0    0    0    0    0
          0    0    0    0    0    0    0    0    0    0
          0    0    0    0    0    0    0    0    0    0
          0    0    0    0    0    0    0    0    0    0
          0    0    0    0    0    0    0    0    0    0
          0    0    0    0    0    0    0    0    0    0
          0    0    0    0    0    0    0    0    0    0
          0    0    0    0    0    0    0    0    0    0
          0    0    0    0    0    0    0    0    0    0
          0    0    0    0    0    0    0    0    0    0
          0    0    0    0    0    0    0    0    0    0
          0    0    0    0    0    0    0    0    0    0
          0    0    0    0    0    0    0    0    0    0
          0    0    0    0    0    0    0    0    0    0
          0    0    0    0    0    0    0    0    0    0
          0    0    0    0    0    0    0    0    0    0

      Contents of dasopw_ex1.das after update:

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
        101  102  103  104  105  106  107  108  109  110
        111  112  113  114  115  116  117  118  119  120
        121  122  123  124  125  126  127  128  129  130
        131  132  133  134  135  136  137  138  139  140
        141  142  143  144  145  146  147  148  149  150
        151  152  153  154  155  156  157  158  159  160
        161  162  163  164  165  166  167  168  169  170
        171  172  173  174  175  176  177  178  179  180
        181  182  183  184  185  186  187  188  189  190
        191  192  193  194  195  196  197  198  199  200


      Note that after run completion, a new DAS file exists in the
      output directory.

-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   K.R. Gehringer      (JPL)
   W.L. Taber          (JPL)
   F.S. Turner         (JPL)
   I.M. Underwood      (JPL)

-Version

   -CSPICE Version 1.0.1, 10-AUG-2021 (JDR)

       Improved the -Exceptions section. Updated the header to comply with
       NAIF standard and added complete code example.

   -CSPICE Version 1.0.0, 11-NOV-2016 (NJB) (KRG) (WLT) (FST) (IMU)

-Index_Entries

   open a DAS file for writing
   open a DAS file for write access

-&
*/

{ /* Begin dasopw_c */

   /*
   Participate in error tracing.
   */
   chkin_c ( "dasopw_c" );

   /*
   Check the input string `fname' to make sure the pointer is non-null
   and the string length is non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "dasopw_c", fname  );

   /*
   Call the f2c'd Fortran routine.
   */
   dasopw_ (  ( char       * )  fname,
              ( integer    * )  handle,
              ( ftnlen       )  strlen(fname)  );

   chkout_c ( "dasopw_c" );

} /* End dasopw_c */
