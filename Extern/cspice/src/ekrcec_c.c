/*

-Procedure ekrcec_c ( EK, read column entry element, character )

-Abstract

   Read data from a character column in a specified EK record.

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

   EK

-Keywords

   EK
   FILES
   UTILITY

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZst.h"
   #include "SpiceZmc.h"

   void ekrcec_c ( SpiceInt           handle,
                   SpiceInt           segno,
                   SpiceInt           recno,
                   ConstSpiceChar   * column,
                   SpiceInt           cvalen,
                   SpiceInt         * nvals,
                   void             * cvals,
                   SpiceBoolean     * isnull )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   handle     I   Handle attached to EK file.
   segno      I   Index of segment containing record.
   recno      I   Record from which data is to be read.
   column     I   Column name.
   cvalen     I   Maximum length of output strings.
   nvals      O   Number of values in column entry.
   cvals      O   Character values in column entry.
   isnull     O   Flag indicating whether column entry is null.

-Detailed_Input

   handle      is an EK file handle. The file may be open for
               read or write access.

   segno       is the index of the segment from which data is to
               be read. The first segment in the file has index 0.

   recno       is the index of the record from which data is to be
               read. This record number is relative to the start
               of the segment indicated by segno; the first
               record in the segment has index 0.

   column      is the name of the column from which data is to be
               read.

   cvalen      is the maximum string length that can be accommodated in
               the output array cvals. This length must large enough to
               hold the longest element of the specified column entry,
               including a null terminator. If the column element contains
               strings of length up to n characters, `cvalen' should be set
               to n + 1.

-Detailed_Output

   nvals,
   cvals       are, respectively, the number of values found in
               the specified column entry and the set of values
               themselves. The array cvals must have sufficient
               string length to accommodate the longest string
               in the returned column entry. The calling application
               should declare `cvals' with dimension

                  [nelts][cvalen]

               where `nelts' is the maximum number of elements that
               occur in any entry of the specified column.

               For columns having fixed-size entries, when a
               a column entry is null, `nvals' is still set to the
               column entry size. For columns having variable-
               size entries, `nvals' is set to 1 for null entries.

   isnull      is a logical flag indicating whether the returned
               column entry is null.

-Parameters

   None.

-Exceptions

   1)  If `handle' is invalid, an error is signaled by a routine in the
       call tree of this routine.

   2)  If `segno' is out of range, an error is signaled by a routine in
       the call tree of this routine.

   3)  If `recno' is out of range, an error is signaled by a routine in
       the call tree of this routine.

   4)  If `column' is not the name of a declared column, an error
       is signaled by a routine in the call tree of this routine.

   5)  If `column' specifies a column of whose data type is not
       character, the error SPICE(WRONGDATATYPE) is signaled by a
       routine in the call tree of this routine.

   6)  If `column' specifies a column of whose class is not a character
       class known to this routine, the error SPICE(NOCLASS) is
       signaled by a routine in the call tree of this routine.

   7)  If an attempt is made to read an uninitialized column entry,
       an error is signaled by a routine in the call tree of this
       routine. A null entry is considered to be initialized, but
       entries do not contain null values by default.

   8)  If an I/O error occurs while reading or writing the indicated
       file, the error is signaled by a routine in the call tree of
       this routine.

   9)  If any element of the column entry would be truncated when
       assigned to an element of `cvals', an error is signaled by a
       routine in the call tree of this routine.

   10) If the `column' input string pointer is null, the error
       SPICE(NULLPOINTER) is signaled.

   11) If the `column' input string has zero length, the error
       SPICE(EMPTYSTRING) is signaled.

   12) If the `cvals' output string pointer is null, the error
       SPICE(NULLPOINTER) is signaled.

   13) If the `cvals' output string has length less than two
       characters, the error SPICE(STRINGTOOSHORT) is signaled, since
       the output string is too short to contain one character of
       output data plus a null terminator.

-Files

   See the EK Required Reading for a discussion of the EK file
   format.

-Particulars

   This routine is a utility that allows an EK file to be read
   directly without using the high-level query interface.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) The following program demonstrates how to create a new EK and
      add data to a character column in a given record within the
      file, and how to read the data from it.


      Example code begins here.


      /.
         Program ekrcec_ex1
      ./
      #include <stdio.h>
      #include <string.h>
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Local constants.
         ./
         #define EKNAME       "ekrcec_ex1.bdb"
         #define IFNAME       "Test EK"
         #define TABLE        "CHR_DATA"
         #define CVLEN        10
         #define DECLEN       201
         #define MAXVAL       4
         #define NCOLS        2
         #define NROWS        6
         #define NRESVC       0

         /.
         Local variables
         ./
         SpiceChar            cdecls [NCOLS] [DECLEN];
         SpiceChar            cnames [NCOLS] [SPICE_EK_CSTRLN];
         SpiceChar            cvals  [MAXVAL][CVLEN];

         SpiceInt             handle;
         SpiceInt             i;
         SpiceInt             nvals;
         SpiceInt             recno;
         SpiceInt             segno;

         SpiceBoolean         isnull;

         /.
         Open a new EK file.  For simplicity, we won't
         reserve space for the comment area, so the
         number of reserved comment characters is zero.
         The constant IFNAME is the internal file name.
         ./
         ekopn_c ( EKNAME, IFNAME, NRESVC, &handle );

         /.
         Set up the table and column names and declarations
         for the CHR_DATA segment.  We'll index all of
         the columns.
         ./
         strcpy( cnames[0], "CHR_COL_1" );
         strcpy( cdecls[0], "DATATYPE = CHARACTER*(*), "
                            "INDEXED = TRUE, NULLS_OK = TRUE" );

         strcpy( cnames[1], "CHR_COL_2" );
         strcpy( cdecls[1], "DATATYPE = CHARACTER*(9), "
                            "SIZE = VARIABLE, NULLS_OK = TRUE" );

         /.
         Start the segment.
         ./
         ekbseg_c ( handle, TABLE,  NCOLS,   SPICE_EK_CSTRLN,
                    cnames, DECLEN, cdecls, &segno           );

         for ( i = 0; i < NROWS; i++ )
         {

            ekappr_c ( handle, segno, &recno );

            isnull = ( i == 1 );

            sprintf  ( cvals[0], "%d", i );
            ekacec_c ( handle, segno, recno, cnames[0],
                       1,      CVLEN, cvals, isnull    );

            /.
            Array-valued columns follow.
            ./
            sprintf  ( cvals[0], "%d", 10*i     );
            sprintf  ( cvals[1], "%d", 10*i + 1 );
            sprintf  ( cvals[2], "%d", 10*i + 2 );
            sprintf  ( cvals[3], "%d", 10*i + 3 );
            ekacec_c ( handle, segno, recno, cnames[1],
                       4,      CVLEN, cvals, isnull    );

         }

         /.
         End the file.
         ./
         ekcls_c ( handle );

         /.
         Open the created file. Show the values added.
         ./
         ekopr_c ( EKNAME, &handle );

         for ( i = 0; i < NROWS; i++ )
         {

            ekrcec_c ( handle, segno, i,      cnames[0],
                       CVLEN, &nvals, cvals, &isnull    );

            if ( ! isnull )
            {

               printf( "Data from column:  %s\n", cnames[0] );
               printf( "   record number:  %d\n", i );
               printf( "   values       :  %s\n", cvals[0] );
               printf( " \n" );

            }
            else
            {

               printf( "Record  %d flag is NULL.\n", i );
               printf( " \n" );

            }

            /.
            Array-valued columns follow.
            ./
            ekrcec_c ( handle, segno, i,      cnames[1],
                       CVLEN, &nvals, cvals, &isnull    );

            if ( ! isnull )
            {

               printf( "Data from column:  %s\n", cnames[1] );
               printf( "   record number:  %d\n", i );
               printf( "   values       :  %s  %s  %s  %s\n",
                                     cvals[0], cvals[1], cvals[2], cvals[3] );
               printf( " \n" );

            }

         }

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Data from column:  CHR_COL_1
         record number:  0
         values       :  0

      Data from column:  CHR_COL_2
         record number:  0
         values       :  0          1          2          3

      Record  1 flag is NULL.

      Data from column:  CHR_COL_1
         record number:  2
         values       :  2

      Data from column:  CHR_COL_2
         record number:  2
         values       :  20         21         22         23

      Data from column:  CHR_COL_1
         record number:  3
         values       :  3

      Data from column:  CHR_COL_2
         record number:  3
         values       :  30         31         32         33

      Data from column:  CHR_COL_1
         record number:  4
         values       :  4

      Data from column:  CHR_COL_2
         record number:  4
         values       :  40         41         42         43

      Data from column:  CHR_COL_1
         record number:  5
         values       :  5

      Data from column:  CHR_COL_2
         record number:  5
         values       :  50         51         52         53


      Note that the second record does not appear due to setting the
      `isnull' flag to true for that record.

      After run completion, a new EK exists in the output directory.

-Restrictions

   1)  EK files open for write access are not necessarily readable.
       In particular, a column entry can be read only if it has been
       initialized. The caller is responsible for determining
       when it is safe to read from files open for write access.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   W.L. Taber          (JPL)

-Version

   -CSPICE Version 1.2.0, 02-AUG-2021 (JDR)

       Changed input argument name "lenout" to "cvalen" for consistency
       with other routines.

       Edited the header to comply with NAIF standard. Added
       complete code example.

   -CSPICE Version 1.1.0, 21-MAY-2001 (WLT)

       Added a cast to (char *) in the call to  F2C_ConvertStrArr to
       support compilation under C++.

   -CSPICE Version 1.0.0, 04-JUL-2000 (NJB)

-Index_Entries

   read character data from EK column

-&
*/

{ /* Begin ekrcec_c */


   /*
   Local variables
   */
   logical                 null;


   /*
   Participate in error tracing.
   */
   chkin_c ( "ekrcec_c" );


   /*
   Check the column name to make sure the pointer is non-null
   and the string length is non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "ekrcec_c", column );


   /*
   Make sure the output array has at least enough room for one output
   character and a null terminator.  Also check for a null pointer.
   */
   CHKOSTR ( CHK_STANDARD, "ekrcec_c", cvals, cvalen );

   /*
   Map the segment and record numbers to their Fortran-style
   values.  Pass a flag of type logical to ekrced_.
   */

   segno++;
   recno++;


   ekrcec_ ( ( integer * ) &handle,
             ( integer * ) &segno,
             ( integer * ) &recno,
             ( char    * ) column,
             ( integer * ) nvals,
             ( char    * ) cvals,
             ( logical * ) &null,
             ( ftnlen    ) strlen(column),
             ( ftnlen    ) cvalen-1        );

   /*
   Convert the output array from Fortran to C style.
   */
   F2C_ConvertStrArr ( *nvals, cvalen, (char *) cvals );


   /*
   Cast the null flag back to a SpiceBoolean.
   */
   *isnull = null;


   chkout_c ( "ekrcec_c" );

} /* End ekrcec_c */
