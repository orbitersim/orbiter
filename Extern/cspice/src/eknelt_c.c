/*

-Procedure eknelt_c  ( EK, get number of elements in column entry )

-Abstract

   Return the number of elements in a specified column entry in
   the current row.

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

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"


   SpiceInt eknelt_c ( SpiceInt  selidx,
                       SpiceInt  row     )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   selidx     I   Index of parent column in SELECT clause.
   row        I   Row containing element.

   The function returns the number of elements in entry in current row.

-Detailed_Input

   selidx      is the SELECT clause index of the column to
               fetch from. The range of selidx is 0 : (nsel-1)
               inclusive, where nsel is the number of items in
               the SELECT clause of the current query.

   row         is the index of the row containing the element.
               This number refers to a member of the set of rows
               matching a query. row must be in the range

                 0 : nmrows-1

               where nmrows is the matching row count returned
               by ekfind_c.

-Detailed_Output

   The function returns the number of elements in the column entry
   belonging to the specified column in the current row.

   Null entries in variable-size columns are considered to have size 1.

-Parameters

   None.

-Exceptions

   1)  If this routine is called when no E-kernels have been loaded,
       the error SPICE(NOLOADEDFILES) is signaled by a routine in the
       call tree of this routine.

   2)  If `selidx' is outside of the range established by the last query
       passed to the EK search engine, the error SPICE(INVALIDINDEX) is
       signaled by a routine in the call tree of this routine.

   3)  If `row' is outside of the range established by the last query passed
       to the EK search engine, the error SPICE(INVALIDINDEX) is signaled by
       a routine in the call tree of this routine.

-Files

   At least one E-kernel must be loaded before queries may be passed to
   the EK system via ekfind_c.

-Particulars

   This routine is meant to be used in conjunction with the EK
   fetch entry points ekgc_c, ekgd_c, and ekgi_c. This routine
   allows the caller of those routines to determine appropriate
   loop bounds to use to fetch each column entry in the current row.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) This example demonstrates how to fetch integer, double
      precision and character string values from a column when such
      column corresponds to either a variable-size array or to a
      static-size array.

      Create an EK that contains a table TAB that has the following
      columns:

          Column name   Data Type   Size
          -----------   ---------   ----
          IARRAY        INT         3
          DARRAY        DP          VARIABLE
          CARRAY        CHR         VARIABLE

      Issue the following query

          query = "SELECT IARRAY, DARRAY, CARRAY FROM TAB"

      to fetch and dump column values from the rows that satisfy the
      query.


      Example code begins here.


      /.
         Program eknelt_ex1
      ./
      #include <stdio.h>
      #include <string.h>
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Local parameters
         ./
         #define EKNAME       "eknelt_ex1.bdb"
         #define TABLE        "TAB"
         #define CHRSLN       6
         #define COL1SZ       3
         #define DECLEN       201
         #define ERRLEN       1841
         #define MXC2SZ       4
         #define MXC3SZ       7
         #define NCOLS        3
         #define NROWS        4
         #define STRSIZ       31

         /.
         Local variables
         ./
         SpiceChar            cdecls [NCOLS] [DECLEN];
         SpiceChar            cnames [NCOLS] [SPICE_EK_CSTRLN];
         SpiceChar            col3   [MXC3SZ][CHRSLN];
         SpiceChar            cvals  [MXC3SZ][STRSIZ];
         SpiceChar            errmsg [ERRLEN];
         SpiceChar          * ifname;
         SpiceChar          * query;

         SpiceDouble          col2   [MXC2SZ];
         SpiceDouble          dvals  [MXC2SZ];

         SpiceInt             col1   [COL1SZ];
         SpiceInt             eltidx;
         SpiceInt             handle;
         SpiceInt             i;
         SpiceInt             ivals  [COL1SZ];
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
         strcpy( cnames[0], "IARRAY" );
         strcpy( cdecls[0], "DATATYPE = INTEGER, SIZE = 3" );

         strcpy( cnames[1], "DARRAY" );
         strcpy( cdecls[1], "DATATYPE = DOUBLE PRECISION, SIZE = VARIABLE" );

         strcpy( cnames[2], "CARRAY" );
         strcpy( cdecls[2], "DATATYPE = CHARACTER*(5), SIZE = VARIABLE" );

         /.
         Start the segment.
         ./
         ekbseg_c ( handle, TABLE,  NCOLS,   SPICE_EK_CSTRLN,
                    cnames, DECLEN, cdecls, &segno           );

         /.
         At the records to the table.
         ./
         for ( i = 0; i < NROWS; i++ )
         {

            /.
            Append a new record to the EK.
            ./
            ekappr_c ( handle, segno, &recno );

            /.
            Add 3 items to IARRAY
            ./
            for ( j = 0; j < COL1SZ; j++ )
            {
               col1[j] =  i+1 + (j+1)*100;
            }

            ekacei_c ( handle,    segno,  recno,
                       cnames[0], COL1SZ, col1,  SPICEFALSE );

            /.
            Add i+1 items to DARRAY
            ./
            for ( j = 0; j <= i; j++ )
            {
               col2[j] = j+1 + (i+1)*200.0;
            }

            ekaced_c ( handle,    segno, recno,
                       cnames[1], i+1,   col2,  SPICEFALSE );

            /.
            Add 4+i items to CARRAY
            ./
            for ( j = 0; j < 4+i; j++ )
            {
               repmi_c ( "ST#", "#", j+1 + (i+1)*100, CHRSLN, col3[j] );
            }

            ekacec_c ( handle, segno,  recno, cnames[2],
                       i+4,    CHRSLN, col3,  SPICEFALSE );

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

         query = "SELECT IARRAY, DARRAY, CARRAY FROM TAB";

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
               Fetch values from column IARRAY in the current
               row.  Since IARRAY was the first column selected,
               the selection index `selidx' is set to 0.
               ./
               selidx = 0;
               eltidx = 0;
               isnull = SPICEFALSE;

               while ( ( eltidx < COL1SZ ) && ( !isnull ) )
               {
                  /.
                  If the column entry is null, we'll be kicked
                  out of this loop after the first iteration.
                  ./
                  ekgi_c ( selidx,        row,     eltidx,
                           ivals+eltidx, &isnull, &found  );

                  eltidx = eltidx + 1;

               }

               printf( "  COLUMN = IARRAY:" );

               if ( isnull )
               {
                  printf( "<Null>\n" );
               }
               else
               {

                  for ( i = 0; i < COL1SZ; i++ )
                  {
                     printf( "%6d", ivals[i] );
                  }

                  printf( " \n" );

               }

               /.
               Fetch values from column DARRAY in the current
               row.  Since DARRAY contains variable-size array
               elements, we call eknelt_c to determine how many
               elements to fetch.
               ./
               selidx = 1;
               nelt   = eknelt_c ( selidx, row );

               eltidx = 0;
               isnull = SPICEFALSE;

               while ( ( eltidx < nelt ) && ( !isnull ) )
               {

                  ekgd_c ( selidx,        row,     eltidx,
                           dvals+eltidx, &isnull, &found  );

                  eltidx = eltidx + 1;

               }

               printf( "  COLUMN = DARRAY:" );

               if ( isnull )
               {
                  printf( "<Null>\n" );
               }
               else
               {

                  for ( i = 0; i < nelt; i++ )
                  {
                     printf( "%6.1f", dvals[i] );
                  }

                  printf( " \n" );

               }

               /.
               Fetch values from column CARRAY in the current
               row.
               ./
               selidx = 2;
               nelt   = eknelt_c ( selidx, row );

               eltidx = 0;
               isnull = SPICEFALSE;

               while ( ( eltidx < nelt ) && ( !isnull ) )
               {

                  ekgc_c ( selidx,         row,     eltidx, STRSIZ,
                           cvals[eltidx], &isnull, &found          );

                  eltidx = eltidx + 1;

               }

               printf( "  COLUMN = CARRAY:" );

               if ( isnull )
               {
                  printf( "<Null>\n" );
               }
               else
               {

                  for ( i = 0; i < nelt; i++ )
                  {
                     printf( " %s", cvals[i] );
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
        COLUMN = IARRAY:   101   201   301
        COLUMN = DARRAY: 201.0
        COLUMN = CARRAY: ST101 ST102 ST103 ST104

      ROW  =   1
        COLUMN = IARRAY:   102   202   302
        COLUMN = DARRAY: 401.0 402.0
        COLUMN = CARRAY: ST201 ST202 ST203 ST204 ST205

      ROW  =   2
        COLUMN = IARRAY:   103   203   303
        COLUMN = DARRAY: 601.0 602.0 603.0
        COLUMN = CARRAY: ST301 ST302 ST303 ST304 ST305 ST306

      ROW  =   3
        COLUMN = IARRAY:   104   204   304
        COLUMN = DARRAY: 801.0 802.0 803.0 804.0
        COLUMN = CARRAY: ST401 ST402 ST403 ST404 ST405 ST406 ST407


      Note that after run completion, a new EK file exists in the
      output directory.

-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)

-Version

   -CSPICE Version 1.1.1, 02-AUG-2021 (JDR)

       Edited the header to comply with NAIF standard. Added
       complete code example based on existing fragment.

   -CSPICE Version 1.1.0, 23-JUL-2001 (NJB)

      Removed tab characters from source file.

   -CSPICE Version 1.0.0, 24-FEB-1999 (NJB)

-Index_Entries

   return the number of elements in a column entry

-&
*/

{ /* Begin eknelt_c */



   /*
   Local variables
   */
   SpiceInt                fIndex;
   SpiceInt                fRow;

   SpiceInt                n;



   /*
   Participate in error tracing.
   */

   chkin_c ( "eknelt_c" );

   /*
   Convert the SELECT clause index and row number to Fortran-style.
   */

   fIndex = selidx + 1;
   fRow   = row    + 1;


   /*
   Get the number of elements from the f2c'd routine.
   */

   eknelt_ ( ( integer * ) &fIndex,
             ( integer * ) &fRow,
             ( integer * ) &n      );


   /*
   Check out before returning the output value.
   */
   chkout_c ( "eknelt_c" );


   return ( n );


} /* End eknelt_c */
