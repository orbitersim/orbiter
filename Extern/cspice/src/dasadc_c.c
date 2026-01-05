/*

-Procedure dasadc_c ( DAS, add data, character )

-Abstract

   Add character data to a DAS file.

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

   void dasadc_c ( SpiceInt            handle,
                   SpiceInt            n,
                   SpiceInt            bpos,
                   SpiceInt            epos,
                   SpiceInt            datlen,
                   const void        * data   )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   handle     I   DAS file handle.
   n          I   Number of characters to add to file.
   bpos,
   epos       I   Begin and end positions of substrings.
   datlen     I   Common length of the character arrays in `data'.
   data       I   Array providing the set of substrings to be added
                  to the character data in the DAS file.

-Detailed_Input

   handle      is a file handle of a DAS file opened for writing.

   n           is the total number of characters to add to the specified
               DAS file.

   bpos,
   epos        are the begin and end character positions that define the
               substrings in each of the elements of the input array.
               This routine writes the first `n' characters from the
               specified set of substrings to the specified DAS file.

   data        is a two-dimensional character array, some portion of whose
               contents are to be added to the specified DAS file.
               Specifically, the first `n' characters of the substrings

                  data[i][bpos:epos],    i = 0, ...

               are appended to the character data in the file.

               `data' must be declared at least as

                  SpiceChar            data   [r][epos+1]

               with the dimension `r' being at least

                  r = int( ( n + sublen - 1 ) / sublen )

               and `sublen', the length of each of the substrings in
               the array to be added to the DAS file, being

                  sublen  =  epos - bpos + 1

               The order of characters in the input substrings is
               considered to increase from left to right within each
               element of `data', and to increase with the indices of the
               elements of `data'.

   datlen      is the common length of the character arrays in `data'.

-Detailed_Output

   None.

   See -Particulars for a description of the effect of this routine.

-Parameters

   None.

-Exceptions

   1)  If the input file handle is invalid, an error is signaled
       by a routine in the call tree of this routine.

   2)  If `epos' or `bpos' are outside of the range

          [  0,  datlen - 1  ]

       or if epos < bpos, the error SPICE(BADSUBSTRINGBOUNDS) is
       signaled by a routine in the call tree of this routine.

   3)  If the input count `n' is less than 1, no data will be
       added to the specified DAS file.

   4)  If an I/O error occurs during the data addition attempted
       by this routine, the error is signaled by a routine in the
       call tree of this routine.

   5)  If `n' is greater than the number of characters in the
       specified set of input substrings, the results of calling
       this routine are unpredictable. This routine cannot
       detect this error.

   6)  If the `data' input string pointer is null, the error
       SPICE(NULLPOINTER) is signaled.

   7)  If the `data' input string has length less than one character,
       the error SPICE(NODATA) is signaled.

-Files

   See the description of the argument `handle' in -Detailed_Input.

-Particulars

   DAS is a low-level format meant to store and transmit data. As
   such, character data in DAS files are not interpreted by SPICELIB
   DAS input or output routines. There are no limits on which
   character values may be placed in the virtual character array of a
   DAS file.

   This routine adds character data to a DAS file by "appending" them
   after any character data already in the file. The sense in which
   the data are "appended" is that the data will occupy a range of
   logical addresses for character data that immediately follow the
   last logical address of a character that is occupied at the time
   this routine is called. The diagram below illustrates this
   addition:

      +-------------------------+
      |    (already in use)     |  Character logical address 1
      +-------------------------+
                  .
                  .
                  .
      +-------------------------+  last character logical address
      |   (already in use)      |  in use before call to dasadc_c
      +-------------------------+
      |  data[0][bpos]          |  First added character
      +-------------------------+
      |  data[0][bpos+1]        |
      +-------------------------+
                   .
                   .
                   .
      +-------------------------+
      |  data[0][epos]          |
      +-------------------------+
      |  data[1][bpos]          |
      +-------------------------+
                   .
                   .
                   .
      +-------------------------+
      |  data[r-1][c]           |  n'th added character---here `r' is
      +-------------------------+
                                      (SpiceInt)( (n+l-1)/l )

                                   where l = epos - bpos + 1, and
                                   `c' is

                                      bpos + ( n - (r-1)*l ) - 1

   The logical organization of the characters in the DAS file is
   independent of the order of addition to the file or physical
   location of any data of integer or double precision type.

   The actual physical write operations that add the input array
   `data' to the indicated DAS file may not take place before this
   routine returns, since the DAS system buffers data that are
   written as well as data that are read. In any case, the data
   will be flushed to the file at the time the file is closed, if
   not earlier. A physical write of all buffered records can be
   forced by calling the CSPICE routine daswbr_c (DAS, write
   buffered records).

   In order to update character logical addresses that already
   contain data, the CSPICE routine dasudc_c (DAS, update data,
   character) should be used.

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
         Program dasadc_ex1
      ./
      #include <stdio.h>
      #include <string.h>
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Local parameters.
         ./
         #define FNAME        "dasadc_ex1.das"
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


      Data from "dasadc_ex1.das":

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

   add character data to a DAS file

-&
*/

{ /* Begin dasadc_c */

   /*
   Local variables.
   */
   SpiceInt           bposFidx;
   SpiceInt           eposFidx;

   /*
   Participate in error tracing.
   */
   chkin_c ( "dasadc_c" );

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

   Make sure that the `data' input array pointer is non-null and that the
   length-dimension of each character array is at least one.
   */
   CHKPTR ( CHK_STANDARD, "dasadc_c", data );

   if ( datlen < 1 )
   {
      setmsg_c ( "The common length of character arrays in `data', "
                 "datlen, must be at least 1. Actual value = #."      );
      errint_c ( "#", datlen                                          );
      sigerr_c ( "SPICE(NODATA)"                                      );

      chkout_c ( "dasadc_c" );
      return;
   }

   /*
   Call the f2c'd Fortran routine.
   */
   dasadc_ (  ( integer    * ) &handle,
              ( integer    * ) &n,
              ( integer    * ) &bposFidx,
              ( integer    * ) &eposFidx,
              ( char       * )  data,
              ( ftnlen       )  datlen   );

   chkout_c ( "dasadc_c" );

} /* End dasadc_c */
