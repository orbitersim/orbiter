/*

-Procedure ekucec_c ( EK, update character column entry )

-Abstract

   Update a character column entry in a specified EK record.

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
   #include "SpiceZim.h"
   #undef    ekucec_c


   void ekucec_c  ( SpiceInt          handle,
                    SpiceInt          segno,
                    SpiceInt          recno,
                    ConstSpiceChar  * column,
                    SpiceInt          nvals,
                    SpiceInt          cvalen,
                    const void      * cvals,
                    SpiceBoolean      isnull )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   handle     I   EK file handle.
   segno      I   Index of segment containing record.
   recno      I   Record to which data is to be updated.
   column     I   Column name.
   nvals      I   Number of values in new column entry.
   cvalen     I   Declared length of character values.
   cvals      I   Character values comprising new column entry.
   isnull     I   Flag indicating whether column entry is null.

-Detailed_Input

   handle      is the handle of an EK file open for write access.

   segno       is the index of the segment containing the column entry
               to be updated. EK segment numbers range from
               0 to N-1, where N is the number of segments
               in the kernel.

   recno       is the index of the record containing the column entry
               to be updated. This record number is relative to the start
               of the segment indicated by segno; the first
               record in the segment has index 0.

   column      is the name of the column containing the entry to
               be updated.

   nvals       is the number of elements in the new value to be inserted
               into the specified column.

   cvalen      is the length of the strings in the `cvals' array, where
               the length includes space for null terminators.

               If the  column has fixed-size entries, then `nvals'
               must equal the entry size for the specified column.


   cvals       is the set of replacement values themselves. The values
               are written into the specified column and record.

               The array `cvals' should be declared with dimensions

                  [nelts][cvalen]

               where `nelts' is greater than or equal to `nvals'.

   isnull      is a logical flag indicating whether the entry is
               null. If `isnull' is SPICEFALSE, the column entry
               defined by `nvals' and `cvals' is added to the
               specified kernel file.

               If `isnull' is SPICETRUE, `nvals' and `cvals' are
               ignored: no data are written into the specified column
               entry. The column entry is marked as a null value.

               If the column has fixed-length, variable-size
               entries, the number of entries is considered to
               be 1.

-Detailed_Output

   None.

   See -Particulars for a description of the effect of this routine.

-Parameters

   None.

-Exceptions

   1)  If `handle' is invalid, an error is signaled by a routine in the
       call tree of this routine.

   2)  If `segno' is out of range, an error is signaled by a routine in
       the call tree of this routine.

   3)  If `column' is not the name of a declared column, an error
       is signaled by a routine in the call tree of this routine.

   4)  If `column' specifies a column of whose data type is not
       CHARACTER, the error SPICE(WRONGDATATYPE) is signaled by a
       routine in the call tree of this routine.

   5)  If `recno' is out of range, an error is signaled by a routine in
       the call tree of this routine.

   6)  If the specified column has fixed-size entries and `nvals' does
       not match this size, an error is signaled by a routine in the
       call tree of this routine.

   7)  If the specified column has variable-size entries and `nvals' is
       non-positive, an error is signaled by a routine in the call
       tree of this routine.

   8)  If an attempt is made to add a null value to a column that
       doesn't take null values, an error is signaled by a routine in
       the call tree of this routine.

   9)  If `column' specifies a column of whose class is not a character
       class known to this routine, the error SPICE(NOCLASS) is
       signaled by a routine in the call tree of this routine.

   10) If an I/O error occurs while reading or writing the indicated
       file, the error is signaled by a routine in the call tree of
       this routine.

   11) If the `column' input string pointer is null, the error
       SPICE(NULLPOINTER) is signaled.

   12) If the `column' input string has zero length, the error
       SPICE(EMPTYSTRING) is signaled.

   13) If the `cvals' input array pointer is null, the error
       SPICE(NULLPOINTER) is signaled.

   14) If the `cvals' input array strings have length less than two
       characters, the error SPICE(STRINGTOOSHORT) is signaled.

   15) If memory cannot be allocated to create the temporary variable
       required for the execution of the underlying Fortran routine,
       the error SPICE(MALLOCFAILED) is signaled.

-Files

   See the EK Required Reading for a discussion of the EK file
   format.

-Particulars

   This routine operates by side effects: it modifies the named
   EK file by replacing a specified character column entry.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) The following program demonstrates how to create a new EK and
      add data to a character column in a given record within the
      file, how to update the data in this record, and how to read
      the data from it.


      Example code begins here.


      /.
         Program ekucec_ex1
      ./
      #include <stdio.h>
      #include <string.h>
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Local constants.
         ./
         #define EKNAME       "ekucec_ex1.bdb"
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
         Open the EK for write access.
         ./
         ekopw_c ( EKNAME, &handle );

         /.
         Negate the values in the odd-numbered records
         using the update routines.
         ./
         for ( i = 1; i < NROWS; i = i+2 )
         {
            recno = i;

            isnull = ( i == 1 );

            sprintf  ( cvals[0], "%d", -i );
            ekucec_c ( handle, segno, recno, cnames[0],
                       1,      CVLEN, cvals, isnull    );

            /.
            Array-valued columns follow.
            ./
            sprintf  ( cvals[0], "%d", -10*i       );
            sprintf  ( cvals[1], "%d", -(10*i + 1) );
            sprintf  ( cvals[2], "%d", -(10*i + 2) );
            sprintf  ( cvals[3], "%d", -(10*i + 3) );
            ekucec_c ( handle, segno, recno, cnames[1],
                       4,      CVLEN, cvals, isnull    );

         }

         /.
         Close the file.
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
         values       :  -3

      Data from column:  CHR_COL_2
         record number:  3
         values       :  -30        -31        -32        -33

      Data from column:  CHR_COL_1
         record number:  4
         values       :  4

      Data from column:  CHR_COL_2
         record number:  4
         values       :  40         41         42         43

      Data from column:  CHR_COL_1
         record number:  5
         values       :  -5

      Data from column:  CHR_COL_2
         record number:  5
         values       :  -50        -51        -52        -53


      Note that the second record does not appear due to setting the
      `isnull' flag to true for that record. The odd value record
      numbers have negative values as a result of the update calls.

      After run completion, a new EK exists in the output directory.

-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)

-Version

   -CSPICE Version 1.1.0, 10-AUG-2021 (JDR)

       Changed input argument name "vallen" to "cvalen" for consistency
       with other routines.

       Edited the header to comply with NAIF standard. Added complete
       code example.

   -CSPICE Version 1.0.0, 28-AUG-2001 (NJB)

-Index_Entries

   replace character entry in an EK column

-&
*/

{ /* Begin ekucec_c */


   /*
   Local variables
   */
   logical                 null;

   SpiceChar            ** cvalsPtr;
   SpiceChar             * fCvalsArr;

   SpiceInt                i;
   SpiceInt                fCvalsLen;


   /*
   Participate in error tracing.
   */
   chkin_c ( "ekucec_c" );

   /*
   Check the column name to make sure the pointer is non-null
   and the string length is non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "ekucec_c", column );

   /*
   Check the value array to make sure the pointer is non-null
   and the string length is non-zero.  Note:  this check is normally
   done for output strings:  CHKOSTR is the macro that does the job.
   */
   CHKOSTR ( CHK_STANDARD, "ekucec_c", cvals, cvalen );

   /*
   We need to make a blank-padded version of the cvals array.
   We'll first allocate an array of character pointers to index
   the values, initialize this array, and use it to produce
   a dynamically allocated array of Fortran-style strings.
   */
   cvalsPtr = ( SpiceChar ** ) malloc ( nvals * sizeof(SpiceChar *) );

   if ( cvalsPtr == 0 )
   {
      setmsg_c ( "Failure on malloc call to create pointer array "
                 "for column values."                              );
      sigerr_c ( "SPICE(MALLOCFAILED)"                             );
      chkout_c ( "ekucec_c"                                        );
      return;
   }

   for ( i = 0;  i < nvals;  i++  )
   {
      cvalsPtr[i] =  (SpiceChar *)cvals  +  ( i * cvalen );
   }

   C2F_CreateFixStrArr (  nvals,
                          cvalen,
                          ( ConstSpiceChar ** ) cvalsPtr,
                          &fCvalsLen,
                          &fCvalsArr                      );

   if ( failed_c() )
   {
      free ( cvalsPtr );

      chkout_c ( "ekucec_c" );
      return;
   }

   /*
   Map the segment and record numbers to the Fortran range.  Get a
   local logical variable to represent the null flag.
   */
   segno++;
   recno++;

   null = isnull;

   ekucec_ ( ( integer    * ) &handle,
             ( integer    * ) &segno,
             ( integer    * ) &recno,
             ( char       * ) column,
             ( integer    * ) &nvals,
             ( char       * ) fCvalsArr,
             ( logical    * ) &null,
             ( ftnlen       ) strlen(column),
             ( ftnlen       ) fCvalsLen        );


   /*
   Clean up our dynamically allocated arrays.
   */
   free ( cvalsPtr     );
   free ( fCvalsArr    );


   chkout_c ( "ekucec_c" );

} /* End ekucec_c */
