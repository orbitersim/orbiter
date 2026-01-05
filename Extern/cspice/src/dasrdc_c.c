/*

-Procedure dasrdc_c ( DAS, read data, character )

-Abstract

   Read character data from a range of DAS logical addresses.

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
   #include "SpiceZmc.h"

   void dasrdc_c ( SpiceInt            handle,
                   SpiceInt            first,
                   SpiceInt            last,
                   SpiceInt            bpos,
                   SpiceInt            epos,
                   SpiceInt            datlen,
                   void              * data   )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   handle     I   DAS file handle.
   first,
   last       I   Range of DAS character logical addresses.
   bpos,
   epos       I   Begin and end positions of substrings.
   datlen     I   Common length of the character arrays in `data'.
   data       O   Data having addresses `first' through `last'.

-Detailed_Input

   handle      is a file handle for an open DAS file.

   first,
   last        are a range of DAS character logical addresses.
               `first' and `last' must be greater than or equal to
               1 and less than or equal to the highest character
               logical address in the DAS file designated by
               `handle'.

   bpos,
   epos        are begin and end character positions that define
               the substrings of the elements of the output array
               `data' into which character data is to be read.

   datlen      is the common length of the character arrays in `data'.

-Detailed_Output

   data        is two-dimensional character array. On output, the character
               words in the logical address range `first' through `last' are
               copied into the characters

                  data[0][bpos],
                  data[0][bpos+1],
                              .
                              .
                              .
                  data[0][epos],
                  data[1][bpos],
                  data[1][bpos+1],
                              .
                              .
                              .
                  data[r-1][bpos]
                  data[r-1][bpos+1]
                              .
                              .
                              .

               in that order. Note that the character positions of `data'
               **other** than the ones shown in the diagram remain
               unmodified.

               `data' must be declared at least as

                  SpiceChar            data   [r][epos+1]

               with the dimension `r' being at least

                  r = int( ( last - first + sublen ) / sublen )

               and `sublen', the length of each of the substrings read
               into the array elements from the DAS file, being

                  sublen  =  epos - bpos + 1

-Parameters

   None.

-Exceptions

   1)  If the input file handle is invalid, an error is signaled
       by a routine in the call tree of this routine. `data' will
       not be modified.

   2)  If `epos' or `bpos' are outside of the range

          [  0,  datlen-1 ]

       or if epos < bpos, the error SPICE(BADSUBSTRINGBOUNDS) is
       signaled by a routine in the call tree of this routine.

   3)  If `first' or `last' are out of range, an error is signaled by a
       routine in the call tree of this routine. `data' will not be
       modified.

   4)  If `first' is greater than `last', `data' is left unchanged.

   5)  If `data' is declared with an `r' dimension of less than

          ( last - first + ( epos-bpos+1 )  ) / ( epos-bpos+1 )

       the error cannot be diagnosed by this routine.

   6)  If the `data' output string pointer is null, the error
       SPICE(NULLPOINTER) is signaled.

   7)  If the `data' output string has length less than one character,
       the error SPICE(NOROOMFORDATA) is signaled.

-Files

   See the description of the argument `handle' in -Detailed_Input.

-Particulars

   DAS is a low-level format meant to store and transmit data. As
   such, character data in DAS files are not interpreted by CSPICE
   DAS input or output routines. There are no limits on which
   character values may be placed in the virtual character array of a
   DAS file.

   This routine provides random read access to the character data in
   a DAS file. These data are logically structured as a
   one-dimensional array of characters.

   However, the interface of this routine provides for extraction of data
   from a DAS file into a two-dimensional array of characters.

   dasrdc_c allows the caller to control the amount of character data
   read into each array element. This feature allows a program to
   read character data into an array that has a different string
   length from the one used to write the character data, without
   losing the correspondence between input and output array elements.
   For example, an array of strings of 32 characters can be written
   to a DAS file and read back by dasrdc_c into a buffer of strings
   having length 80 characters, mapping each 32-character string to
   characters 1--32 of the output buffer.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) The following example demonstrates the capabilities of the
      DAS character data routines. The reader should notice that
      in these interfaces, the character data are treated not as
      strings (or arrays of strings) but as a stream of single
      characters: DAS character data are not limited to
      human-readable text. For example, one can store images or
      DEM data as DAS character data.

      The example shows how to add a variable amount of character
      data to a new DAS file, how to update some of the character
      logical addresses within that file, and how to read that
      data out to a different array.


      Example code begins here.


      /.
         Program dasrdc_ex1
      ./
      #include <stdio.h>
      #include <string.h>
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Local parameters.
         ./
         #define FNAME        "dasrdc_ex1.das"
         #define TYPE         "TEST"

         /.
         Local variables.
         ./
         SpiceChar            cdatin [3][22];
         SpiceInt             handle;
         SpiceInt             i;

         SpiceChar            cdastr [31];
         SpiceChar            cdatou [10][30] = {
                                           "..............................",
                                           "..............................",
                                           "..............................",
                                           "..............................",
                                           "..............................",
                                           "..............................",
                                           "..............................",
                                           "..............................",
                                           "         1         2         3",
                                           "123456789012345678901234567890" };

         /.
         Open a new DAS file. Use the file name as the internal
         file name, and reserve no records for comments.
         ./
         dasonw_c ( FNAME, TYPE, FNAME, 0, &handle );

         /.
         Set the input data. Note that these data will be
         considered as a binary data stream: DAS character data
         are not limited to human-readable text. For example,
         one can store images or DEM data as DAS character data.
         ./
         strncpy( cdatin[0], "--F-345678901234567890", 22 );
         strncpy( cdatin[1], "--S-345678901234567890", 22 );
         strncpy( cdatin[2], "--T-IRDxxxxxxxxxxxxxxx", 22 );

         /.
         Add the last 20 characters of the first two elements
         of `cdatin', and the 3rd character from the third one.
         ./
         dasadc_c ( handle, 41, 2, 21, 22, cdatin );

         /.
         Update the 10th, 20th and 30th character in the DAS
         file with a vertical bar.
         ./
         for ( i = 1; i < 4; i++ )
         {
            dasudc_c ( handle, i*10, i*10, 0, 0, 1, "|" );
         }

         /.
         Close the file.
         ./
         dascls_c ( handle );

         /.
         Now verify the addition of data by opening the
         file for read access and retrieving the data.
         ./
         dasopr_c ( FNAME, &handle );

         /.
         Read the 41 characters that we stored on the DAS
         file. Update the data on the `cdatou' array, placing
         6 characters on each element, starting from the
         10th position.
         ./
         dasrdc_c ( handle, 1, 41, 9, 14, 30, cdatou );

         /.
         Dump the data to the screen. Note that the last
         three lines should remain unmodified, and that
         only 5 characters will be written on the 7th line.
         ./
         printf( "\n" );
         printf( "Data from \"%s\":\n", FNAME );
         printf( "\n" );

         for ( i = 0; i < 10; i++ )
         {

            /.
            Add null-terminating character to `cdatou[i]' in order to
            print it to the screen.
            ./
            strncpy( cdastr, cdatou[i], 30 );
            cdastr[30] = '\0';

            printf( "%s\n", cdastr );
         }

         /.
         Close the file.
         ./
         dascls_c ( handle );

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Data from "dasrdc_ex1.das":

      .........F-3456...............
      .........789|12...............
      .........345678...............
      .........9|S-34...............
      .........56789|...............
      .........123456...............
      .........7890T................
      ..............................
               1         2         3
      123456789012345678901234567890


      Note that after run completion, a new DAS file exists in the
      output directory.

-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   J. Diaz del Rio     (ODC Space)

-Version

   -CSPICE Version 1.0.0, 19-MAY-2021 (JDR)

-Index_Entries

   read character data from a DAS file

-&
*/

{ /* Begin dasrdc_c */

   /*
   Local variables.
   */
   SpiceInt           bposFidx;
   SpiceInt           eposFidx;

   /*
   Participate in error tracing.
   */
   chkin_c ( "dasrdc_c" );

   /*
   Convert

      bpos
      epos

   to Fortran-style indices.
   */
   bposFidx = bpos + 1;
   eposFidx = epos + 1;

   /*
   Special case: this routine assumes that `data' is a two-dimensional
   character data stream that shall be treated as binary character data
   and not as an array of strings. Null characters shall not be treated
   as string termination characters.

   Make sure that the `data' output array pointer is non-null and that the
   length-dimension of each character array is at least one.
   */
   CHKPTR ( CHK_STANDARD, "dasrdc_c", data );

   if ( datlen < 1 )
   {
      setmsg_c ( "The common length of character arrays in `data', "
                 "datlen, must be at least 1. Actual value = #."      );
      errint_c ( "#", datlen                                          );
      sigerr_c ( "SPICE(NOROOMFORDATA)"                               );

      chkout_c ( "dasrdc_c" );
      return;
   }

   /*
   Call the f2c'd Fortran routine.
   */
   dasrdc_ (  ( integer    * ) &handle,
              ( integer    * ) &first,
              ( integer    * ) &last,
              ( integer    * ) &bposFidx,
              ( integer    * ) &eposFidx,
              ( char       * )  data,
              ( ftnlen       )  datlen   );

   chkout_c ( "dasrdc_c" );

} /* End dasrdc_c */
