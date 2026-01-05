/*

-Procedure ekgd_c  ( EK, get event data, double precision )

-Abstract

   Return an element of an entry in a column of double precision
   type in a specified row.

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

   ASSIGNMENT
   EK

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"

   void ekgd_c ( SpiceInt          selidx,
                 SpiceInt          row,
                 SpiceInt          elment,
                 SpiceDouble     * ddata,
                 SpiceBoolean    * null,
                 SpiceBoolean    * found  )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   selidx     I   Index of parent column in SELECT clause.
   row        I   Row to fetch from.
   elment     I   Index of element, within column entry, to fetch.
   ddata      O   Double precision element of column entry.
   null       O   Flag indicating whether column entry was null.
   found      O   Flag indicating whether column was present in row.

-Detailed_Input

   selidx      is the SELECT clause index of the column to fetch
               from. The range of selidx is from 0 to one less than
               the number of columns in the SELECT clause.

   row         is the output row containing the entry to fetch
               from. The range of row is from 0 to one less than
               the number of rows satisfying the previous query.

   elment      is the index of the element of the column entry
               to fetch. The normal range of elment is from 0 to
               one less than the size of the column's entry, but
               elment is allowed to exceed the number of elements in
               the column entry; if it does, found is returned
               as SPICEFALSE. This allows the caller to read data
               from the column entry in a loop without checking the
               number of available elements first.

               Null values in variable-sized columns are
               considered to have size 1.

-Detailed_Output

   ddata       is the requested element of the specified column
               entry. If the entry is null, ddata is undefined.

   null        is a logical flag indicating whether the entry
               belonging to the specified column in the specified
               row is null.

   found       is a logical flag indicating whether the specified
               element was found. If the element does not exist,
               found is returned as SPICEFALSE.

-Parameters

   None.

-Exceptions

   1)  If the input argument `elment' is less than 0, the error
       SPICE(INVALIDINDEX) is signaled by a routine in the call tree of this
       routine and `found' is returned SPICEFALSE. However, `elment' is allowed
       to be greater than or equal to the number of elements in the specified
       column entry; this allows the caller to read data from the column entry
       in a loop without checking the number of available elements first. If
       `elment' is greater than or equal to the number of available elements,
       `found' is returned SPICEFALSE.

   2)  If `selidx' is outside of the range established by the last query
       passed to the EK search engine, the error SPICE(INVALIDINDEX) is
       signaled by a routine in the call tree of this routine and `found' is
       returned SPICEFALSE.

   3)  If the input argument `row' is less than 0 or greater than or
       equal to the number of rows matching the query, the error
       SPICE(INVALIDINDEX) is signaled by a routine in the call tree
       of this routine and `found' is returned SPICEFALSE.

   4)  If the specified column does not have DP or TIME type, the
       error SPICE(INVALIDTYPE) is signaled by a routine in the call
       tree of this routine.

   5)  If this routine is called when no E-kernels have been loaded,
       the error SPICE(NOLOADEDFILES) is signaled by a routine in the
       call tree of this routine.

-Files

   The EK "query and fetch" suite of functions reads binary `sequence
   component' EK files. In order for a binary EK file to be
   accessible to this routine, the file must be `loaded' via a call
   to the function eklef_c.

   Text format EK files cannot be used by this routine; they must
   first be converted by binary format by the NAIF Toolkit utility
   SPACIT.

-Particulars

   This routine allows retrieval of data from double precision columns.

   This routine returns one element at a time in order to save the
   caller from imposing a limit on the size of the column entries
   that can be handled.

-Examples

   The numerical results shown for these examples may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Perform a query on an EK file that contains a database with
      the Supplementary Engineering Data Records of the Viking
      Project in order to retrieve the PLATFORM_CLOCK values (double
      precision) that correspond to the images with IMAGE_NUMBER
      smaller than a given value, ordered by IMAGE_NUMBER.


      Use the EK kernel below to load the information from the
      original Supplementary Engineering Data Record (SEDR) data
      set generated by the Viking Project.

         vo_sedr.bdb


      Example code begins here.


      /.
         Program ekgd_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Local parameters
         ./
         #define EKNAME       "vo_sedr.bdb"
         #define ERRLEN       1840

         /.
         Local variables
         ./
         SpiceChar            errmsg [ERRLEN];
         SpiceChar          * query;

         SpiceDouble          ddata;

         SpiceInt             eltidx;
         SpiceInt             nmrows;
         SpiceInt             rowno;
         SpiceInt             selidx;

         SpiceBoolean         error;
         SpiceBoolean         found;
         SpiceBoolean         isnull;

         /.
         Open an EK file.
         ./
         furnsh_c ( EKNAME );

         /.
         The table "VIKING_SEDR_DATA" has a column
         "PLATFORM_CLOCK" of double precision values.

         Define a set of constraints to perform a query on
         all loaded EK files (the SELECT clause). In this
         case select the column "PLATFORM_CLOCK" from table
         "VIKING_SEDR_DATA" sorted by "IMAGE_NUMBER".
         ./
         query = "Select PLATFORM_CLOCK from VIKING_SEDR_DATA "
                 "where IMAGE_NUMBER < 25860000 "
                 "order by IMAGE_NUMBER";

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

            /.
            Fetch the character data. We know the query returned
            one column and the column contains only scalar data,
            so the index of all elements is 0.
            ./
            selidx = 0;
            eltidx = 0;

            /.
            Loop over each row found matching the query.
            ./
            for ( rowno = 0; rowno < nmrows; rowno++ )
            {

               printf( "Row  %2d: Double precision data: ", rowno );

               /.
               Use ekgd_c to retrieve the string from
               ./
               ekgd_c ( selidx, rowno, eltidx, &ddata, &isnull, &found );

               if ( isnull )
               {
                  printf( "<Null>\n" );
               }
               else
               {
                  printf( "%10.6f\n", ddata );
               }

            }

         }

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Row   0: Double precision data: 119.880000
      Row   1: Double precision data: 119.270000
      Row   2: Double precision data: 119.880000
      Row   3: Double precision data: 119.270000
      Row   4: Double precision data: 119.880000
      Row   5: Double precision data: 119.270000
      Row   6: Double precision data: 120.140000
      Row   7: Double precision data: 119.520000
      Row   8: Double precision data: 120.140000
      Row   9: Double precision data: 120.140000
      Row  10: Double precision data: 120.140000
      Row  11: Double precision data: 221.920000
      Row  12: Double precision data: 221.920000
      Row  13: Double precision data: 221.920000
      Row  14: Double precision data: 120.140000
      Row  15: Double precision data: 120.140000
      Row  16: Double precision data: 120.140000
      Row  17: Double precision data: 120.220000
      Row  18: Double precision data: 120.220000
      Row  19: Double precision data: 120.220000
      Row  20: Double precision data: 120.370000
      Row  21: Double precision data: 120.370000
      Row  22: Double precision data: 120.370000
      Row  23: Double precision data: 120.290000
      Row  24: Double precision data: 120.290000
      Row  25: Double precision data: 120.290000


   2) Perform a query on an EK file that contains a database with
      the Supplementary Engineering Data Records of the Viking
      Project in order to retrieve the IMAGE_TIME values (double
      precision time) that correspond to the images with
      IMAGE_NUMBER smaller than a given value, ordered by
      IMAGE_NUMBER.


      Use the EK kernel below to load the information from the
      original Supplementary Engineering Data Record (SEDR) data
      set generated by the Viking Project.

         vo_sedr.bdb

      Use the LSK kernel below to load the leap seconds and time
      constants required for the conversions.

         naif0012.tls


      Example code begins here.


      /.
         Program ekgd_ex2
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Local parameters
         ./
         #define EKNAME       "vo_sedr.bdb"
         #define LSKNAM       "naif0012.tls"
         #define ERRLEN       1840
         #define TIMLEN       24

         /.
         Local variables
         ./
         SpiceChar            errmsg [ERRLEN];
         SpiceChar          * query;
         SpiceChar            utcstr [TIMLEN];

         SpiceDouble          ddata;

         SpiceInt             eltidx;
         SpiceInt             nmrows;
         SpiceInt             rowno;
         SpiceInt             selidx;

         SpiceBoolean         error;
         SpiceBoolean         found;
         SpiceBoolean         isnull;

         /.
         Load leapseconds file for time conversion.
         ./
         furnsh_c ( LSKNAM );

         /.
         Open an EK file.
         ./
         furnsh_c ( EKNAME );

         /.
         The table "VIKING_SEDR_DATA" has a column
         "IMAGE_TIME" of time values (stored as double
         precision items).

         Define a set of constraints to perform a query on
         all loaded EK files (the SELECT clause). In this
         case select the column "IMAGE_TIME" from table
         "VIKING_SEDR_DATA" sorted by "IMAGE_NUMBER".
         ./
         query = "Select IMAGE_TIME from VIKING_SEDR_DATA "
                 "where IMAGE_NUMBER < 25860000 "
                 "order by IMAGE_NUMBER";

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

            /.
            Fetch the character data. We know the query returned
            one column and the column contains only scalar data,
            so the index of all elements is 0.
            ./
            selidx = 0;
            eltidx = 0;

            /.
            Loop over each row found matching the query.
            ./
            for ( rowno = 0; rowno < nmrows; rowno++ )
            {

               printf( "Row  %2d: Time data: ", rowno );

               /.
               Use ekgd_c to retrieve the string from
               ./
               ekgd_c ( selidx, rowno, eltidx, &ddata, &isnull, &found );

               if ( isnull )
               {
                  printf( "<Null>\n" );
               }
               else
               {
                  et2utc_c ( ddata, "C", 3, TIMLEN, utcstr );
                  printf( " %s\n", utcstr );
               }

            }

         }

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Row   0: Time data:  1976 JUN 16 16:50:55.92
      Row   1: Time data:  1976 JUN 16 16:51:00.26
      Row   2: Time data:  1976 JUN 16 20:56:53.05
      Row   3: Time data:  1976 JUN 16 20:56:57.39
      Row   4: Time data:  1976 JUN 17 01:02:50.17
      Row   5: Time data:  1976 JUN 17 01:02:54.52
      Row   6: Time data:  1976 JUN 17 05:08:56.26
      Row   7: Time data:  1976 JUN 17 05:09:00.60
      Row   8: Time data:  1976 JUN 17 06:30:28.42
      Row   9: Time data:  1976 JUN 17 06:30:46.17
      Row  10: Time data:  1976 JUN 17 06:30:55.16
      Row  11: Time data:  1976 JUN 17 11:17:47.47
      Row  12: Time data:  1976 JUN 17 11:18:05.22
      Row  13: Time data:  1976 JUN 17 11:18:14.21
      Row  14: Time data:  1976 JUN 17 13:20:23.63
      Row  15: Time data:  1976 JUN 17 13:20:41.38
      Row  16: Time data:  1976 JUN 17 13:20:50.37
      Row  17: Time data:  1976 JUN 17 15:23:17.71
      Row  18: Time data:  1976 JUN 17 15:23:35.46
      Row  19: Time data:  1976 JUN 17 15:23:44.46
      Row  20: Time data:  1976 JUN 17 17:26:20.76
      Row  21: Time data:  1976 JUN 17 17:26:38.51
      Row  22: Time data:  1976 JUN 17 17:26:47.50
      Row  23: Time data:  1976 JUN 17 19:29:23.80
      Row  24: Time data:  1976 JUN 17 19:29:41.55
      Row  25: Time data:  1976 JUN 17 19:29:50.54


   3) This example demonstrates how to fetch double precision values
      from a column in three different cases: single values,
      variable-size arrays and static-size arrays.

      Create an EK that contains a table TAB that has the following
      columns:

         Column name   Data Type   Size
         -----------   ---------   ----
         DP_COL_1      DP          1
         DP_COL_2      DP          VARIABLE
         DP_COL_3      DP          3

      Issue the following query

          query = "SELECT DP_COL_1, DP_COL_2, DP_COL_3 FROM TAB"

      to fetch and dump column values from the rows that satisfy the
      query.


      Example code begins here.


      /.
         Program ekgd_ex3
      ./
      #include <stdio.h>
      #include <string.h>
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Local parameters
         ./
         #define EKNAME       "ekgd_ex3.bdb"
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

         SpiceDouble          col1;
         SpiceDouble          col2   [MXC2SZ];
         SpiceDouble          col3   [COL3SZ];
         SpiceDouble          dvals  [MXC2SZ];

         SpiceInt             eltidx;
         SpiceInt             handle;
         SpiceInt             i;
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
         ifname  =  "Test EK/Created 13-JUN-2019";

         ekopn_c ( EKNAME, ifname, nresvc, &handle );

         /.
         Set up the column names and declarations
         for the TAB segment.  We'll index all of
         the columns.
         ./
         strcpy( cnames[0], "DP_COL_1" );
         strcpy( cdecls[0], "DATATYPE = DOUBLE PRECISION, INDEXED  = TRUE" );

         strcpy( cnames[1], "DP_COL_2" );
         strcpy( cdecls[1], "DATATYPE = DOUBLE PRECISION, SIZE = VARIABLE" );

         strcpy( cnames[2], "DP_COL_3" );
         strcpy( cdecls[2], "DATATYPE = DOUBLE PRECISION, SIZE = 3" );

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
            Add DP_COL_1
            ./
            col1 = i * 100.0;

            ekaced_c ( handle,  segno, recno, cnames[0],
                       1,      &col1,  SPICEFALSE );

            /.
            Add `i' items to DP_COL_2
            ./
            for ( j = 0; j < i; j++ )
            {
               col2[j] = j + 1 + i*200.0;
            }

            ekaced_c ( handle, segno, recno, cnames[1], i, col2, SPICEFALSE );

            /.
            Add 3 items to DP_COL_3
            ./
            for ( j = 0; j < 3; j++ )
            {
               col3[j] =  i + (j+1)*100.0;
            }

            ekaced_c ( handle, segno, recno, cnames[2], 3, col3, SPICEFALSE );

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

         query = "SELECT DP_COL_1, DP_COL_2, DP_COL_3 FROM TAB";

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
               Fetch values from column DP_COL_1.  Since
               DP_COL_1 was the first column selected, the
               selection index `selidx' is set to 0.
               ./
               selidx = 0;
               eltidx = 0;
               ekgd_c ( selidx, row, eltidx, dvals, &isnull, &found );

               printf( "  COLUMN = DP_COL_1: " );

               if ( isnull )
               {
                  printf( "<Null>\n" );
               }
               else
               {
                  printf( " %f\n", dvals[0] );
               }

               /.
               Fetch values from column DP_COL_2 in the current
               row.  Since DP_COL_2 contains variable-size array
               elements, we call eknelt_c to determine how many
               elements to fetch.
               ./
               selidx = 1;
               nelt = eknelt_c ( selidx, row );

               eltidx = 0;
               isnull = SPICEFALSE;

               while ( ( eltidx < nelt ) && ( !isnull ) )
               {

                  ekgd_c ( selidx,        row,     eltidx,
                           dvals+eltidx, &isnull, &found  );

                  eltidx = eltidx + 1;

                  /.
                  If the column entry is null, we'll be kicked
                  out of this loop after the first iteration.
                  ./
               }

               printf( "  COLUMN = DP_COL_2:" );

               if ( isnull )
               {
                  printf( "<Null>\n" );
               }
               else
               {

                  for ( i = 0; i < nelt; i++ )
                  {
                     printf( " %6.1f", dvals[i] );
                  }

                  printf( " \n" );

               }

               /.
               Fetch values from column DP_COL_3 in the current
               row.  We need not call eknelt_c since we know how
               many elements are in each column entry.
               ./
               selidx = 2;
               eltidx = 0;
               isnull = SPICEFALSE;

               while ( ( eltidx < COL3SZ ) && ( !isnull ) )
               {

                  ekgd_c ( selidx,        row,     eltidx,
                           dvals+eltidx, &isnull, &found  );

                  eltidx = eltidx + 1;

               }

               printf( "  COLUMN = DP_COL_3:" );

               if ( isnull )
               {
                  printf( "<Null>\n" );
               }
               else
               {

                  for ( i = 0; i < COL3SZ; i++ )
                  {
                     printf( " %6.1f", dvals[i] );
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
        COLUMN = DP_COL_1:  100.000000
        COLUMN = DP_COL_2:  201.0
        COLUMN = DP_COL_3:  101.0  201.0  301.0

      ROW  =   1
        COLUMN = DP_COL_1:  200.000000
        COLUMN = DP_COL_2:  401.0  402.0
        COLUMN = DP_COL_3:  102.0  202.0  302.0

      ROW  =   2
        COLUMN = DP_COL_1:  300.000000
        COLUMN = DP_COL_2:  601.0  602.0  603.0
        COLUMN = DP_COL_3:  103.0  203.0  303.0

      ROW  =   3
        COLUMN = DP_COL_1:  400.000000
        COLUMN = DP_COL_2:  801.0  802.0  803.0  804.0
        COLUMN = DP_COL_3:  104.0  204.0  304.0


      Note that after run completion, a new EK file exists in the
      output directory.


   4) Query the EK system and fetch data matching that query.

      The program shown here does not rely on advance
      knowledge of the input query or the contents of any loaded EK
      files.

      To simplify the example, we assume that all data are scalar.
      This assumption relieves us of the need to test the size of
      column entries before fetching them. In the event that a
      column contains variable-size array entries, the entry point
      eknelt_c may be called to obtain the size of column entries to
      be fetched. See eknelt_c for an example.


      Use the EK kernel below to load the information from the
      original Supplementary Engineering Data Record (SEDR) data
      set generated by the Viking Project.

         vo_sedr.bdb

      Use the LSK kernel below to load the leap seconds and time
      constants required for the conversions.

         naif0012.tls


      Example code begins here.


      /.
         Program ekgd_ex4
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Local parameters
         ./
         #define EKNAME       "vo_sedr.bdb"
         #define LSKNAM       "naif0012.tls"
         #define ERRLEN       1840
         #define TIMLEN       27
         #define TYPLEN       4
         #define XCLSLN       4

         /.
         Local variables
         ./
         SpiceChar            cdata  [SPICE_EK_MAXQSTR];
         SpiceChar            cols   [SPICE_EK_MAXQSEL][SPICE_EK_MAXQRY];
         SpiceChar            errmsg [ERRLEN];
         SpiceChar          * query;
         SpiceChar            utcstr [TIMLEN];
         SpiceChar            tabs   [SPICE_EK_MAXQTAB][SPICE_EK_MAXQRY];
         SpiceEKExprClass     xclass [SPICE_EK_MAXQSEL];
         SpiceEKDataType      xtypes [SPICE_EK_MAXQSEL];

         SpiceDouble          ddata;
         SpiceDouble          tdata;

         SpiceInt             b;
         SpiceInt             colno;
         SpiceInt             e;
         SpiceInt             handle;
         SpiceInt             idata;
         SpiceInt             n;
         SpiceInt             nmrows;
         SpiceInt             row;
         SpiceInt             xbegs  [SPICE_EK_MAXQSEL];
         SpiceInt             xends  [SPICE_EK_MAXQSEL];

         SpiceBoolean         error;
         SpiceBoolean         found;
         SpiceBoolean         null;

         /.
         Load leapseconds file for time conversion.
         ./
         furnsh_c ( LSKNAM );

         /.
         Load EK.
         ./
         eklef_c ( EKNAME, &handle );

         /.
         Setup the query.  Parse the SELECT clause using
         ekpsel_c.
         ./
         query = "Select IMAGE_NUMBER, IMAGE_ID, PLATFORM_CLOCK, IMAGE_TIME "
                 "from VIKING_SEDR_DATA where IMAGE_NUMBER < 25850000 "
                 "order by IMAGE_NUMBER";

         ekpsel_c ( query,  ERRLEN, SPICE_EK_MAXQRY, SPICE_EK_MAXQRY,
                    &n,     xbegs,  xends,           xtypes,
                    xclass, tabs,   cols,           &error,          errmsg );

         if ( error )
         {
            printf( "%s\n", errmsg );
         }
         else
         {

            /.
            Submit query to the EK query system.
            ./
            ekfind_c ( query, ERRLEN, &nmrows, &error, errmsg );

            if ( error )
            {

               printf( "%s\n", errmsg );

            }
            else
            {

               /.
               Fetch the rows that matched the query.
               ./
               for ( row = 0; row < nmrows; row++ )
               {

                  /.
                  Fetch data from the ith row.
                  ./
                  printf( " \n" );
                  printf( "ROW =  %d\n", row );

                  for ( colno = 0; colno < n; colno++ )
                  {

                     /.
                     Fetch the data from the jth selected
                     column.
                     ./
                     if ( xclass[colno] == SPICE_EK_EXP_COL )
                     {
                        printf( "  %s.%-14s: ", tabs[colno], cols[colno] );
                     }
                     else
                     {
                        b  =  xbegs[colno];
                        e  =  xends[colno];
                        printf( "ITEM =  (%.*s)", e-b, query+b );
                     }

                     if ( xtypes[colno] == SPICE_CHR )
                     {
                        ekgc_c ( colno,  row,   1, SPICE_EK_MAXQSTR,
                                 cdata, &null, &found               );

                        if ( null )
                        {
                           printf( "<Null>\n" );
                        }
                        else
                        {
                           printf( "%s\n", cdata );
                        }

                     }
                     else if ( xtypes[colno] == SPICE_DP )
                     {
                        ekgd_c ( colno, row, 1, &ddata, &null, &found );

                        if ( null )
                        {
                           printf( "<Null>\n" );
                        }
                        else
                        {
                           printf( "%f\n", ddata );
                        }

                     }
                     else if ( xtypes[colno] == SPICE_INT )
                     {
                        ekgi_c ( colno, row, 1, &idata, &null, &found );

                        if ( null )
                        {
                           printf( "<Null>\n" );
                        }
                        else
                        {
                           printf( "%d\n", idata );
                        }

                     }
                     else
                     {

                        /.
                        The item is a time value.  Convert it
                        to UTC for output.
                        ./
                        ekgd_c ( colno, row, 1, &tdata, &null, &found );

                        if ( null )
                        {
                           printf( "<Null>\n" );
                        }
                        else
                        {
                           et2utc_c ( tdata, "C", 3, TIMLEN, utcstr );
                           printf( "%s\n", utcstr );
                        }

                     }

                     /.
                     We're done with the column having index `colno'.
                     ./
                  }

                  /.
                  We're done with the row having index `row'.
                  ./
               }

               /.
               We either processed the query or had an error.
               ./
            }

            /.
            We either parsed the SELECT clause or had an error.
            ./
         }

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      ROW =  0
        VIKING_SEDR_DATA.IMAGE_NUMBER  : 25837050
        VIKING_SEDR_DATA.IMAGE_ID      : 168C09
        VIKING_SEDR_DATA.PLATFORM_CLOCK: 119.880000
        VIKING_SEDR_DATA.IMAGE_TIME    : 1976 JUN 16 16:50:55.925

      ROW =  1
        VIKING_SEDR_DATA.IMAGE_NUMBER  : 25837051
        VIKING_SEDR_DATA.IMAGE_ID      : 168C10
        VIKING_SEDR_DATA.PLATFORM_CLOCK: 119.270000
        VIKING_SEDR_DATA.IMAGE_TIME    : 1976 JUN 16 16:51:00.269

      ROW =  2
        VIKING_SEDR_DATA.IMAGE_NUMBER  : 25840344
        VIKING_SEDR_DATA.IMAGE_ID      : 168C11
        VIKING_SEDR_DATA.PLATFORM_CLOCK: 119.880000
        VIKING_SEDR_DATA.IMAGE_TIME    : 1976 JUN 16 20:56:53.051

      ROW =  3
        VIKING_SEDR_DATA.IMAGE_NUMBER  : 25840345
        VIKING_SEDR_DATA.IMAGE_ID      : 168C12
        VIKING_SEDR_DATA.PLATFORM_CLOCK: 119.270000
        VIKING_SEDR_DATA.IMAGE_TIME    : 1976 JUN 16 20:56:57.395

      ROW =  4
        VIKING_SEDR_DATA.IMAGE_NUMBER  : 25843638
        VIKING_SEDR_DATA.IMAGE_ID      : 169C01
        VIKING_SEDR_DATA.PLATFORM_CLOCK: 119.880000
        VIKING_SEDR_DATA.IMAGE_TIME    : 1976 JUN 17 01:02:50.177

      ROW =  5
        VIKING_SEDR_DATA.IMAGE_NUMBER  : 25843639
        VIKING_SEDR_DATA.IMAGE_ID      : 169C02
        VIKING_SEDR_DATA.PLATFORM_CLOCK: 119.270000
        VIKING_SEDR_DATA.IMAGE_TIME    : 1976 JUN 17 01:02:54.521

      ROW =  6
        VIKING_SEDR_DATA.IMAGE_NUMBER  : 25846934
        VIKING_SEDR_DATA.IMAGE_ID      : 169C03
        VIKING_SEDR_DATA.PLATFORM_CLOCK: 120.140000
        VIKING_SEDR_DATA.IMAGE_TIME    : 1976 JUN 17 05:08:56.263

      ROW =  7
        VIKING_SEDR_DATA.IMAGE_NUMBER  : 25846935
        VIKING_SEDR_DATA.IMAGE_ID      : 169C04
        VIKING_SEDR_DATA.PLATFORM_CLOCK: 119.520000
        VIKING_SEDR_DATA.IMAGE_TIME    : 1976 JUN 17 05:09:00.607

      ROW =  8
        VIKING_SEDR_DATA.IMAGE_NUMBER  : 25848026
        VIKING_SEDR_DATA.IMAGE_ID      : 169C05
        VIKING_SEDR_DATA.PLATFORM_CLOCK: 120.140000
        VIKING_SEDR_DATA.IMAGE_TIME    : 1976 JUN 17 06:30:28.424

      ROW =  9
        VIKING_SEDR_DATA.IMAGE_NUMBER  : 25848030
        VIKING_SEDR_DATA.IMAGE_ID      : 169C09
        VIKING_SEDR_DATA.PLATFORM_CLOCK: 120.140000
        VIKING_SEDR_DATA.IMAGE_TIME    : 1976 JUN 17 06:30:46.174

      ROW =  10
        VIKING_SEDR_DATA.IMAGE_NUMBER  : 25848032
        VIKING_SEDR_DATA.IMAGE_ID      : 169C11
        VIKING_SEDR_DATA.PLATFORM_CLOCK: 120.140000
        VIKING_SEDR_DATA.IMAGE_TIME    : 1976 JUN 17 06:30:55.168


-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   E.D. Wright         (JPL)

-Version

   -CSPICE Version 1.2.0, 02-AUG-2021 (JDR)

       Edited the header to comply with NAIF standard. Added
       complete code examples.

   -CSPICE Version 1.1.1, 09-FEB-2003 (EDW)

       Minor edit to correct typo in header.

   -CSPICE Version 1.1.0, 09-JUL-1998 (NJB)

       Bug fix: now uses local logical variable to capture the
       error flag value returned by the underlying f2c'd routine.

   -CSPICE Version 1.0.0, 27-MAR-1998 (NJB)

       Based on SPICELIB Version 1.1.0, 07-JUL-1996 (NJB)

-Index_Entries

   fetch element from double precision column entry

-&
*/

{ /* Begin ekgd_c */

   /*
   Local variables
   */
   logical                 fnd;


   /*
   Participate in error tracing.
   */
   chkin_c ( "ekgd_c" );


   /*
   Convert indices to Fortran-style; increment each index.
   */
   selidx ++;
   row    ++;
   elment ++;


   /*
   Call the f2c'd routine.
   */
   ekgd_  ( ( integer    * ) &selidx,
            ( integer    * ) &row,
            ( integer    * ) &elment,
            ( doublereal * ) ddata,
            ( logical    * ) null,
            ( logical    * ) &fnd  );


   /*
   Set the SpiceBoolean output found flag.
   */

   *found  =  fnd;


   chkout_c ( "ekgd_c" );

} /* End ekgd_c */
