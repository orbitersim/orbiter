/*

-Procedure ekacei_c ( EK, add integer data to column )

-Abstract

   Add data to an integer column in a specified EK record.

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
   #include "SpiceZmc.h"
   #include "SpiceZim.h"
   #undef    ekacei_c


   void ekacei_c  ( SpiceInt          handle,
                    SpiceInt          segno,
                    SpiceInt          recno,
                    ConstSpiceChar  * column,
                    SpiceInt          nvals,
                    ConstSpiceInt   * ivals,
                    SpiceBoolean      isnull )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   handle     I   EK file handle.
   segno      I   Index of segment containing record.
   recno      I   Record to which data is to be added.
   column     I   Column name.
   nvals      I   Number of values to add to column.
   ivals      I   Integer values to add to column.
   isnull     I   Flag indicating whether column entry is null.

-Detailed_Input

   handle      is the handle of an EK file open for write access.

   segno       is the number of the segment to which the record
               is to be added. EK segment numbers range from
               0 to N-1, where N is the number of segments
               in the kernel.

   recno       is the index of the record to which data is to be
               added. This record number is relative to the start
               of the segment indicated by segno; the first
               record in the segment has index 0.

   column      is the name of the column to which data is to be
               added.

   nvals,
   ivals       are, respectively, the number of values to add to
               the specified column and the set of values
               themselves. The data values are written into the
               specified column and record.

               If the column has fixed-size entries, then NVALS
               must equal the entry size for the specified column.


   isnull      is a logical flag indicating whether the entry is
               null. If isnull is SPICEFALSE, the column entry
               defined by nvals and ivals is added to the
               specified kernel file.

               If isnull is SPICETRUE, nvals and cvals are ignored:
               no data are written into the specified column entry.
               The column entry is marked as a null value.

               If the column has fixed-length, variable-size
               entries, the number of entries is considered to
               be 1.

-Detailed_Output

   None. See -Particulars for a description of the effect of this
   routine.

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
       integer, the error SPICE(WRONGDATATYPE) is signaled by a
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

   9)  If `column' specifies a column of whose class is not a
       character class known to this routine, the error
       SPICE(NOCLASS) is signaled by a routine in the call tree of
       this routine.

   10) If an I/O error occurs while reading or writing the indicated
       file, the error is signaled by a routine in the call tree of
       this routine.

   11) If the `column' input string pointer is null, the error
       SPICE(NULLPOINTER) is signaled.

   12) If the `column' input string has zero length, the error
       SPICE(EMPTYSTRING) is signaled.

-Files

   See the EK Required Reading for a discussion of the EK file
   format.

-Particulars

   This routine operates by side effects: it modifies the named
   EK file by adding data to the specified record in the specified
   column. Data may be added to a segment in random order; it is not
   necessary to fill in columns or rows sequentially. Data may only
   be added one column entry at a time.

-Examples

   The numerical results shown for these examples may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) This example demonstrates how to add integer values
      to a column in three different cases: single values,
      variable-size arrays and static-size arrays.

      Create an EK that contains a table TAB that has the following
      columns:

         Column name   Data Type   Size
         -----------   ---------   ----
         INT_COL_1     INT         1
         INT_COL_2     INT         VARIABLE
         INT_COL_3     INT         3


      Issue the following query

          query = "SELECT INT_COL_1, INT_COL2, INT_COL3 FROM TAB"

      to fetch and dump column values from the rows that satisfy the
      query.


      Example code begins here.


      /.
         Program ekacei_ex1
      ./
      #include <stdio.h>
      #include <string.h>
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Local parameters
         ./
         #define EKNAME       "ekacei_ex1.bdb"
         #define TABLE        "TAB"
         #define COL3SZ       3
         #define DECLEN       200
         #define ERRLEN       1840
         #define MXC2SZ       4
         #define NCOLS        3
         #define NROWS        4

         /.
         Local variables
         ./
         SpiceChar            cdecls [NCOLS][DECLEN];
         SpiceChar            cnames [NCOLS][SPICE_EK_CSTRLN];
         SpiceChar            errmsg [ERRLEN];
         SpiceChar          * ifname;
         SpiceChar          * query;

         SpiceInt             col1;
         SpiceInt             col2   [MXC2SZ];
         SpiceInt             col3   [COL3SZ];
         SpiceInt             eltidx;
         SpiceInt             handle;
         SpiceInt             i;
         SpiceInt             ivals  [MXC2SZ];
         SpiceInt             j;
         SpiceInt             nelt;
         SpiceInt             nmrows;
         SpiceInt             nresvc;
         SpiceInt             recno;
         SpiceInt             row;
         SpiceInt             segno;
         SpiceInt             selidx;

         SpiceBoolean         error;
         SpiceBoolean         found;
         SpiceBoolean         isnull;

         /.
         Open a new EK file.  For simplicity, we will not
         reserve any space for the comment area, so the
         number of reserved comment characters is zero.
         The variable `ifname' is the internal file name.
         ./
         nresvc  =  0;
         ifname  =  "Test ek/created 13-JUN-2019";

         ekopn_c ( EKNAME, ifname, nresvc, &handle );

         /.
         Set up the column names and declarations
         for the TAB segment.  We'll index all of
         the columns.
         ./
         strcpy( cnames[0], "INT_COL_1" );
         strcpy( cdecls[0], "DATATYPE = INTEGER, INDEXED  = TRUE" );

         strcpy( cnames[1], "INT_COL_2" );
         strcpy( cdecls[1], "DATATYPE = INTEGER, SIZE = VARIABLE, "
                            "NULLS_OK = TRUE" );

         strcpy( cnames[2], "INT_COL_3" );
         strcpy( cdecls[2], "DATATYPE = INTEGER, SIZE = 3" );

         /.
         Start the segment.
         ./
         ekbseg_c ( handle, TABLE,  NCOLS,   SPICE_EK_CSTRLN,
                    cnames, DECLEN, cdecls, &segno           );

         /.
         At the records to the table.
         ./
         for ( i = 1; i <= NROWS; i++ )
         {

            /.
            Append a new record to the EK.
            ./
            ekappr_c ( handle, segno, &recno );

            /.
            Add INT_COL_1
            ./
            col1 = i * 100;

            ekacei_c ( handle,  segno, recno,     cnames[0],
                       1,      &col1,  SPICEFALSE           );

            /.
            Add `i' items to INT_COL_2
            ./
            for ( j = 0; j < i; j++ )
            {
               col2[j] = j + 1 + i*200;
            }
            isnull = ( i == 2 );

            ekacei_c ( handle, segno, recno, cnames[1], i, col2, isnull );

            /.
            Add 3 items to INT_COL_3
            ./
            for ( j = 0; j < 3; j++ )
            {
               col3[j] =  i + (j+1)*100.0;
            }

            ekacei_c ( handle, segno, recno, cnames[2], 3, col3, SPICEFALSE );

         }

         /.
         Close the file.
         ./
         ekcls_c ( handle );

         /.
         Open the created file. Perform the query and show the
         results.
         ./
         furnsh_c ( EKNAME );

         query = "SELECT INT_COL_1, INT_COL_2, INT_COL_3 FROM TAB";

         /.
         Query the EK system for data rows matching the
         SELECT constraints.
         ./
         ekfind_c ( query, ERRLEN, &nmrows, &error, errmsg );

         /.
         Check whether an error occurred while processing the
         SELECT clause. If so, output the error message.
         ./
         if ( error )
         {
            printf( "SELECT clause error:  %s\n", errmsg );
         }
         else
         {

            for ( row = 0; row < nmrows; row++ )
            {

               printf( " \n" );
               printf( "ROW  =  %2d\n", row );

               /.
               Fetch values from column INT_COL_1.  Since
               INT_COL_1 was the first column selected, the
               selection index `selidx' is set to 0.
               ./
               selidx = 0;
               eltidx = 0;
               ekgi_c ( selidx, row, eltidx, ivals, &isnull, &found );

               printf( "  COLUMN = INT_COL_1:" );

               if ( isnull )
               {
                  printf( " <Null>\n" );
               }
               else
               {
                  printf( " %6d\n", ivals[0] );
               }

               /.
               Fetch values from column INT_COL_2 in the current
               row.  Since INT_COL_2 contains variable-size array
               elements, we call eknelt_c to determine how many
               elements to fetch.
               ./
               selidx = 1;
               nelt = eknelt_c ( selidx, row );

               eltidx = 0;
               isnull = SPICEFALSE;

               while ( ( eltidx < nelt ) && ( !isnull ) )
               {

                  ekgi_c ( selidx,        row,     eltidx,
                           ivals+eltidx, &isnull, &found  );

                  eltidx = eltidx + 1;

                  /.
                  If the column entry is null, we'll be kicked
                  out of this loop after the first iteration.
                  ./
               }

               printf( "  COLUMN = INT_COL_2:" );

               if ( isnull )
               {
                  printf( " <Null>\n" );
               }
               else
               {

                  for ( i = 0; i < nelt; i++ )
                  {
                     printf( " %6d", ivals[i] );
                  }

                  printf( " \n" );

               }

               /.
               Fetch values from column INT_COL_3 in the current
               row.  We need not call eknelt_c since we know how
               many elements are in each column entry.
               ./
               selidx = 2;
               eltidx = 0;
               isnull = SPICEFALSE;

               while ( ( eltidx < COL3SZ ) && ( !isnull ) )
               {

                  ekgi_c ( selidx,        row,     eltidx,
                           ivals+eltidx, &isnull, &found  );

                  eltidx = eltidx + 1;

               }

               printf( "  COLUMN = INT_COL_3:" );

               if ( isnull )
               {
                  printf( " <Null>\n" );
               }
               else
               {

                  for ( i = 0; i < COL3SZ; i++ )
                  {
                     printf( " %6d", ivals[i] );
                  }

                  printf( " \n" );

               }

            }

            /.
            We either parsed the SELECT clause or had an error.
            ./
         }

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      ROW  =   0
        COLUMN = INT_COL_1:    100
        COLUMN = INT_COL_2:    201
        COLUMN = INT_COL_3:    101    201    301

      ROW  =   1
        COLUMN = INT_COL_1:    200
        COLUMN = INT_COL_2: <Null>
        COLUMN = INT_COL_3:    102    202    302

      ROW  =   2
        COLUMN = INT_COL_1:    300
        COLUMN = INT_COL_2:    601    602    603
        COLUMN = INT_COL_3:    103    203    303

      ROW  =   3
        COLUMN = INT_COL_1:    400
        COLUMN = INT_COL_2:    801    802    803    804
        COLUMN = INT_COL_3:    104    204    304


      Note that after run completion, a new EK file exists in the
      output directory.


   2) Suppose we want to create an E-kernel which contains a table
      of items that have been ordered. The columns of this table
      are shown below:

         DATAITEMS

            Column Name     Data Type
            -----------     ---------
            ITEM_ID         INTEGER
            ORDER_ID        INTEGER
            ITEM_NAME       CHARACTER*(*)
            DESCRIPTION     CHARACTER*(*)
            PRICE           DOUBLE PRECISION


      This EK file will have one segment containing the DATAITEMS
      table.

      This examples demonstrates how to open a new EK file; create
      the segment described above and how to insert a new record
      into it.


      Example code begins here.


      /.
         Program ekacei_ex2
      ./
      #include <string.h>
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Local parameters
         ./
         #define EKNAME       "ekacei_ex2.bdb"
         #define TABLE        "DATAITEMS"
         #define DECLEN       201
         #define DESCLN       81
         #define NAMLEN       41
         #define NCOLS        5

         /.
         Local variables
         ./
         SpiceChar            cdecls [NCOLS][DECLEN];
         SpiceChar            cnames [NCOLS][SPICE_EK_CSTRLN];
         SpiceChar          * descrp;
         SpiceChar          * ifname;
         SpiceChar          * itemnm;

         SpiceDouble          price;

         SpiceInt             esize;
         SpiceInt             handle;
         SpiceInt             itemid;
         SpiceInt             nresvc;
         SpiceInt             ordid;
         SpiceInt             recno;
         SpiceInt             segno;

         SpiceBoolean         isnull;

         /.
         Open a new EK file.  For simplicity, we will not
         reserve any space for the comment area, so the
         number of reserved comment characters is zero.
         The variable `ifname' is the internal file name.
         ./
         nresvc  =  0;
         ifname  =  "Test EK;Created 21-JUN-2019";

         ekopn_c ( EKNAME, ifname, nresvc, &handle );

         /.
         Set up the table and column names and declarations
         for the DATAITEMS segment.  We'll index all of
         the columns.  All columns are scalar, so we omit
         the size declaration.
         ./
         strcpy( cnames[0], "ITEM_ID" );
         strcpy( cdecls[0], "DATATYPE = INTEGER, INDEXED = TRUE" );

         strcpy( cnames[1], "ORDER_ID" );
         strcpy( cdecls[1], "DATATYPE = INTEGER, INDEXED = TRUE" );

         strcpy( cnames[2], "ITEM_NAME" );
         strcpy( cdecls[2], "DATATYPE = CHARACTER*(*), INDEXED  = TRUE" );

         strcpy( cnames[3], "DESCRIPTION" );
         strcpy( cdecls[3], "DATATYPE = CHARACTER*(*), INDEXED  = TRUE" );

         strcpy( cnames[4], "PRICE" );
         strcpy( cdecls[4], "DATATYPE = DOUBLE PRECISION, INDEXED  = TRUE" );

         /.
         Start the segment. Since we have no data for this
         segment, start the segment by just defining the new
         segment's schema.
         ./
         ekbseg_c ( handle, TABLE,  NCOLS,   SPICE_EK_CSTRLN,
                    cnames, DECLEN, cdecls, &segno           );

         /.
         Append a new, empty record to the DATAITEMS
         table. Recall that the DATAITEMS table
         is in segment number 0.  The call will return
         the number of the new, empty record.
         ./
         segno = 0;
         ekappr_c ( handle, segno, &recno );

         /.
         At this point, the new record is empty.  A valid EK
         cannot contain empty records.  We fill in the data
         here.  Data items are filled in one column at a time.
         The order in which the columns are filled in is not
         important.  We use the ekaceX_c (add column entry)
         routines to fill in column entries.  We'll assume
         that no entries are null.  All entries are scalar,
         so the entry size is 0.
         ./
         isnull   =  SPICEFALSE;
         esize    =  0;

         /.
         The following variables will contain the data for
         the new record.
         ./
         ordid    =   10011;
         itemid   =   531;
         itemnm   =  "Sample item";
         descrp   =  "This sample item is used only in tests.";
         price    =   1345.678;

         /.
         Note that the names of the routines called
         correspond to the data types of the columns:  the
         last letter of the routine name is C, I, or D,
         depending on the data type.
         ./
         ekacei_c ( handle, segno, recno, "ORDER_ID", esize, &ordid, isnull );

         ekacei_c ( handle, segno, recno, "ITEM_ID", esize, &itemid, isnull );

         ekacec_c ( handle, segno,  recno,  "ITEM_NAME",
                    esize,  NAMLEN, itemnm,  isnull     );

         ekacec_c ( handle, segno,  recno,  "DESCRIPTION",
                    esize,  DESCLN, descrp,  isnull       );

         ekaced_c ( handle, segno, recno, "PRICE", esize, &price, isnull );

         /.
         Close the file to make the update permanent.
         ./
         ekcls_c ( handle );

         return ( 0 );
      }


      When this program is executed, no output is presented on
      screen. After run completion, a new EK file exists in the
      output directory.

-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)

-Version

   -CSPICE Version 1.0.1, 10-AUG-2021 (JDR)

       Edited the header to comply with NAIF standard. Added
       complete code examples.

   -CSPICE Version 1.0.0, 28-AUG-2001 (NJB)

-Index_Entries

   add integer data to EK column
   add data to EK
   write integer data to EK column

-&
*/

{ /* Begin ekacei_c */


   /*
   Local variables
   */
   logical                 null;

   /*
   Participate in error tracing.
   */
   chkin_c ( "ekacei_c" );

   /*
   Check the column name to make sure the pointer is non-null
   and the string length is non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "ekacei_c", column );

   /*
   Convert the null flag to type logical before passing it to
   ekacei_.  Also map the segment and record numbers to their
   Fortran-style counterparts.
   */

   null = isnull;

   segno++;
   recno++;

   ekacei_  (  ( integer * ) &handle,
               ( integer * ) &segno,
               ( integer * ) &recno,
               ( char    * )  column,
               ( integer * ) &nvals,
               ( integer * )  ivals,
               ( logical * ) &null,
               ( ftnlen    ) strlen(column) );


   chkout_c ( "ekacei_c" );

} /* End ekacei_c */
