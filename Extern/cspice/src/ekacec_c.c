/*

-Procedure ekacec_c ( EK, add character data to column )

-Abstract

   Add data to a character column in a specified EK record.

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
   UTILITY

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZst.h"
   #include "SpiceZmc.h"
   #include "SpiceZim.h"
   #undef    ekacec_c

   void ekacec_c  ( SpiceInt          handle,
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
   recno      I   Record to which data is to be added.
   column     I   Column name.
   nvals      I   Number of values to add to column.
   cvalen     I   Declared length of character values.
   cvals      I   Character values to add to column.
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


   nvals       is the number of entries in the value to be added to the
               specified column.

   cvalen      is the length of the strings in the cvals array, where
               the length includes space for null terminators.

               If the  column has fixed-size entries, then nvals
               must equal the entry size for the specified column.


   cvals       is the set of values themselves. The data values are
               written into the specified column and record.

               The array cvals should be declared with dimensions

                  [nelts][cvalen]

               where nelts is greater than or equal to nvals.

   isnull      is a logical flag indicating whether the entry is
               null. If `isnull' is SPICEFALSE, the column entry
               defined by `nvals' and `cvals' is added to the
               specified kernel file.

               If `isnull' is SPICETRUE, `nvals' and `cvals' are
               ignored: no data are written into the specified
               column entry. The column entry is marked as a null
               value.

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
       character, the error SPICE(WRONGDATATYPE) is signaled by a
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
   EK file by adding data to the specified record in the specified
   column. Data may be added to a segment in random order; it is not
   necessary to fill in columns or rows sequentially. Data may only
   be added one column entry at a time.

-Examples

   The numerical results shown for these examples may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) The following program demonstrates how to create a new EK and
      add data to a character column in a given record within the
      file, and how to read the data from it.

      Example code begins here.


      /.
         Program ekacec_ex1
      ./
      #include <stdio.h>
      #include <string.h>
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Local parameters
         ./
         #define EKNAME       "ekacec_ex1.bdb"
         #define TABLE        "TABLENAME"
         #define CBUFSZ       4
         #define DECLEN       201
         #define LINESZ       7
         #define NCOLS        2
         #define STRLEN       7

         /.
         Local variables
         ./
         SpiceChar            cdecls [NCOLS] [DECLEN];
         SpiceChar            cnames [NCOLS] [SPICE_EK_CSTRLN];
         SpiceChar            cvals  [CBUFSZ][LINESZ];
         SpiceChar          * ifname;

         SpiceInt             handle;
         SpiceInt             i;
         SpiceInt             j;
         SpiceInt             k;
         SpiceInt             nresvc;
         SpiceInt             nvals;
         SpiceInt             recno;
         SpiceInt             segno;

         SpiceBoolean         isnull;

         /.
         Create a list of character strings.
         ./
         SpiceChar            cbuf   [CBUFSZ][STRLEN] = {
                                    "CHSTR1", "CHSTR2", "CHSTR3", "CHSTR4"  };

         /.
         Open a new EK file.  For simplicity, we will not
         reserve any space for the comment area, so the
         number of reserved comment characters is zero.
         The variable `ifname' is the internal file name.
         ./
         nresvc  =  0;
         ifname  =  "Test EK/created 31-MAY-2019";

         ekopn_c ( EKNAME, ifname, nresvc, &handle );

         /.
         Define the column names and formats.
         ./
         strcpy( cnames[0], "CCOL" );
         strcpy( cdecls[0], "DATATYPE = CHARACTER*(*), "
                            "INDEXED = TRUE, NULLS_OK = TRUE" );

         strcpy( cnames[1], "CARRAY" );
         strcpy( cdecls[1], "DATATYPE = CHARACTER*(6), "
                            "SIZE = VARIABLE, NULLS_OK = TRUE" );

         /.
         Start the segment.
         ./
         ekbseg_c ( handle, TABLE,  NCOLS,   SPICE_EK_CSTRLN,
                    cnames, DECLEN, cdecls, &segno           );

         /.
         Append a new record to the EK.
         ./
         ekappr_c ( handle, segno, &recno );

         /.
         Add the value "999" to the first record of the column
         CCOL in the `segno' segment of the EK file designated
         by `handle'.
         ./
         ekacec_c ( handle, segno,  recno, "CCOL",
                    1,      4,     "999",   SPICEFALSE );

         /.
         Add an array `cbuf' of 4 values to the first record of
         the column CARRAY in the `segno' segment of the EK file
         designated by `handle'.
         ./
         ekacec_c ( handle, segno,  recno, "CARRAY",
                    CBUFSZ, STRLEN, cbuf,   SPICEFALSE );

         /.
         Append a second record to the EK.
         ./
         ekappr_c ( handle, segno, &recno );

         /.
         Repeat the operation again for the second record, but
         this time, add only 2 values of `cbuf'.
         ./
         ekacec_c ( handle, segno,  recno, "CARRAY",
                    2,      STRLEN, cbuf,   SPICEFALSE );

         /.
         Add a null value to the CCOL in the second record.
         The argument 999 is ignored because the null flag is
         set to SPICETRUE.
         ./
         ekacec_c ( handle, segno, recno, "CCOL",
                    1,      4,     "999", SPICETRUE );

         /.
         Close the file.
         ./
         ekcls_c ( handle );

         /.
         Open the created file. Show the values added.
         ./
         ekopr_c ( EKNAME, &handle );

         printf( "Record   CCOL              CARRAY\n" );
         printf( "------  ------   ------------------------------\n" );

         /.
         The file we have created has only one segment and
         two records within. Each record has two columns.
         ./
         segno = 0;

         /.
         Go over each record...
         ./
         for ( i = 0; i < 2; i++ )
         {

            printf( "%4d     ", i );

            /.
            ... and each column.
            ./
            for ( j = 0; j < NCOLS; j++ )
            {

               /.
               Read the data from the first column.
               ./
               ekrcec_c ( handle,  segno, i,      cnames[j],
                          LINESZ, &nvals, cvals, &isnull   );

               if ( isnull )
               {
                  printf( "NULL    " );
               }
               else
               {

                  for ( k = 0; k < nvals; k++ )
                  {
                     printf( "%6s  ", cvals[k] );
                  }
               }
            }
            printf( " \n" );

         }

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Record   CCOL              CARRAY
      ------  ------   ------------------------------
         0     999     CHSTR1  CHSTR2  CHSTR3  CHSTR4
         1     NULL    CHSTR1  CHSTR2


      Note that after run completion, a new EK file exists in the
      output directory.


   2) A more detailed example.

      Suppose we have an E-kernel which contains records of orders
      for data products. The E-kernel has a table called DATAORDERS
      that consists of the set of columns listed below:

         DATAORDERS

            Column Name     Data Type
            -----------     ---------
            ORDER_ID        INTEGER
            CUSTOMER_ID     INTEGER
            LAST_NAME       CHARACTER*(*)
            FIRST_NAME      CHARACTER*(*)
            ORDER_DATE      TIME
            COST            DOUBLE PRECISION

      The order database also has a table of items that have been
      ordered. The columns of this table are shown below:

         DATAITEMS

            Column Name     Data Type
            -----------     ---------
            ITEM_ID         INTEGER
            ORDER_ID        INTEGER
            ITEM_NAME       CHARACTER*(*)
            DESCRIPTION     CHARACTER*(*)
            PRICE           DOUBLE PRECISION


      We'll suppose that the EK file contains two segments, the
      first containing the DATAORDERS table and the second
      containing the DATAITEMS table.

      This examples demonstrates how to open a new EK file; create
      the two segments described above, using fast writers; and
      how to insert a new record into one of the tables.


      Use the LSK kernel below to load the leap seconds and time
      constants required for the conversions.

         naif0012.tls


      Example code begins here.


      /.
         Program ekacec_ex2
      ./
      #include <string.h>
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Local parameters
         ./
         #define EKNAME       "ekacec_ex2.bes"
         #define TABLE        "DATAORDERS"
         #define DECLEN       200
         #define DESCLN       80
         #define FNMLEN       50
         #define LNMLEN       50
         #define NAMLEN       40
         #define NCOLS        6
         #define NROWS        9
         #define UTCLEN       30

         /.
         Local variables
         ./
         SpiceChar            cdecls [NCOLS] [DECLEN];
         SpiceChar            cnames [NCOLS] [SPICE_EK_CSTRLN];
         SpiceChar          * descrp;
         SpiceChar            fnames [NROWS] [FNMLEN];
         SpiceChar          * ifname;
         SpiceChar          * itemnm;
         SpiceChar            lnames [NROWS] [LNMLEN];
         SpiceChar            odate  [UTCLEN];

         SpiceDouble          costs  [NROWS];
         SpiceDouble          ets    [NROWS];
         SpiceDouble          price;

         SpiceInt             cstids [NROWS];
         SpiceInt             esize;
         SpiceInt             handle;
         SpiceInt             i;
         SpiceInt             id;
         SpiceInt             itemid;
         SpiceInt             nresvc;
         SpiceInt             ordid;
         SpiceInt             ordids [NROWS];
         SpiceInt             rcptrs [NROWS];
         SpiceInt             recno;
         SpiceInt             segno;
         SpiceInt             sizes  [NROWS];
         SpiceInt             wkindx [NROWS];

         SpiceBoolean         isnull;
         SpiceBoolean         nlflgs [NROWS];

         /.
         Load a leapseconds kernel for UTC/ET conversion.
         ./
         furnsh_c ( "naif0012.tls" );

         /.
         Open a new EK file.  For simplicity, we will not
         reserve any space for the comment area, so the
         number of reserved comment characters is zero.
         The variable `ifname' is the internal file name.
         ./
         nresvc  =  0;
         ifname  =  "Test EK/Created 01-JUN-2019";

         ekopn_c ( EKNAME, ifname, nresvc, &handle );

         /.
         Set up the table and column names and declarations
         for the DATAORDERS segment.  We'll index all of
         the columns.  All columns are scalar, so we omit
         the size declaration.  Only the COST column may take
         null values.
         ./
         strcpy( cnames[0], "ORDER_ID" );
         strcpy( cdecls[0], "DATATYPE = INTEGER, INDEXED = TRUE" );

         strcpy( cnames[1], "CUSTOMER_ID" );
         strcpy( cdecls[1], "DATATYPE = INTEGER, INDEXED = TRUE" );

         strcpy( cnames[2], "LAST_NAME" );
         strcpy( cdecls[2], "DATATYPE = CHARACTER*(*), INDEXED  = TRUE" );

         strcpy( cnames[3], "FIRST_NAME" );
         strcpy( cdecls[3], "DATATYPE = CHARACTER*(*), INDEXED  = TRUE" );

         strcpy( cnames[4], "ORDER_DATE" );
         strcpy( cdecls[4], "DATATYPE = TIME, INDEXED  = TRUE" );

         strcpy( cnames[5], "COST" );
         strcpy( cdecls[5], "DATATYPE = DOUBLE PRECISION, "
                            "INDEXED  = TRUE, NULLS_OK = TRUE" );

         /.
         Start the segment.  We presume the number of rows
         of data is known in advance.
         ./
         ekifld_c ( handle, TABLE,  NCOLS,   NROWS, SPICE_EK_CSTRLN,
                    cnames, DECLEN, cdecls, &segno, rcptrs          );

         /.
         At this point, arrays containing data for the
         segment's columns may be filled in.  The names
         of the data arrays are shown below.

            Column           Data array

            "ORDER_ID"       ordids
            "CUSTOMER_ID"    cstids
            "LAST_NAME"      lnames
            "FIRST_NAME"     fnames
            "ORDER_DATE"     ets
            "COST"           costs
         ./
         for ( i = 0; i < NROWS; i++ )
         {

            id = i + 1;

            ordids[i] = id;
            cstids[i] = id * 100;
            costs[i]  = id * 100.0;

            repmi_c ( "Order # Customer first name", "#",
                                     id, FNMLEN, fnames[i] );
            repmi_c ( "Order # Customer last name",  "#",
                                     id, LNMLEN, lnames[i] );
            repmi_c ( "1998 Mar #", "#", id, UTCLEN, odate );

            utc2et_c ( odate, ets+i );

            nlflgs[i] = SPICEFALSE;

         }

         nlflgs[1] = SPICETRUE;

         /.
         The `sizes' array shown below is ignored for scalar
         and fixed-size array columns, so we need not
         initialize it.  For variable-size arrays, the
         ith element of the `sizes' array must contain the size
         of the ith column entry in the column being written.
         Normally, the `sizes' array would be reset for each
         variable-size column.

         The `nlflgs' array indicates which entries are null.
         It is ignored for columns that don't allow null
         values.  In this case, only the COST column allows
         nulls.

         Add the columns of data to the segment.  All of the
         data for each column is written in one shot.
         ./
         ekacli_c ( handle, segno,  "ORDER_ID", ordids,
                    sizes,  nlflgs,  rcptrs,    wkindx );

         ekacli_c ( handle, segno,  "CUSTOMER_ID", cstids,
                    sizes,  nlflgs,  rcptrs,       wkindx );

         ekaclc_c ( handle, segno, "LAST_NAME", LNMLEN,
                    lnames, sizes,  nlflgs,     rcptrs, wkindx );

         ekaclc_c ( handle, segno, "FIRST_NAME", FNMLEN,
                    fnames, sizes,  nlflgs,      rcptrs, wkindx );

         ekacld_c ( handle, segno,  "ORDER_DATE", ets,
                    sizes,  nlflgs,  rcptrs,      wkindx );

         ekacld_c ( handle, segno,  "COST",  costs,
                    sizes,  nlflgs,  rcptrs, wkindx );

         /.
         Complete the segment.  The `rcptrs' array is that
         returned by ekifld_c.
         ./
         ekffld_c ( handle, segno, rcptrs );

         /.
         At this point, the second segment could be
         created by an analogous process.  In fact, the
         second segment could be created at any time; it is
         not necessary to populate the first segment with
         data before starting the second segment.

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
         Start the new segment. Since we have no data for this
         segment, start the segment by just defining the new
         segment's schema.
         ./
         ekbseg_c ( handle, "DATAITEMS", 5,       SPICE_EK_CSTRLN,
                    cnames,  DECLEN,     cdecls, &segno           );

         /.
         Close the file by a call to ekcls_c.
         ./
         ekcls_c ( handle );

         /.
         Now, we want to insert a new record into the DATAITEMS
         table.

         Open the database for write access.  This call is
         made when the file already exists.
         ./
         ekopw_c ( EKNAME, &handle );

         /.
         Append a new, empty record to the DATAITEMS
         table. Recall that the DATAITEMS table
         is in segment number 1.  The call will return
         the number of the new, empty record.
         ./
         segno = 1;
         ekappr_c ( handle, segno, &recno );

         /.
         At this point, the new record is empty.  A valid EK
         cannot contain empty records.  We fill in the data
         here.  Data items are filled in one column at a time.
         The order in which the columns are filled in is not
         important.  We use the ekaceX_c (add column entry)
         routines to fill in column entries.  We'll assume
         that no entries are null.  All entries are scalar,
         so the entry size is 1.
         ./
         isnull   =  SPICEFALSE;
         esize    =  1;

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
         last letter of the routine name is C, `i', or D,
         depending on the data type.
         ./
         ekacei_c ( handle,  segno, recno, "ORDER_ID",
                    esize,  &ordid, isnull            );

         ekacei_c ( handle,  segno,  recno, "ITEM_ID",
                    esize,  &itemid, isnull           );

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

   -CSPICE Version 1.1.0, 10-AUG-2021 (JDR)

       Changed input argument name "vallen" to "cvalen" for consistency
       with other routines.

       Edited the header to comply with NAIF standard. Added complete
       code examples.

       Added entry #15 to -Exceptions section.

   -CSPICE Version 1.0.0, 28-AUG-2001 (NJB)

-Index_Entries

   add character data to EK column
   add data to EK
   write character data to EK column

-&
*/

{ /* Begin ekacec_c */


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
   chkin_c ( "ekacec_c" );

   /*
   Check the column name to make sure the pointer is non-null
   and the string length is non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "ekacec_c", column );

   /*
   Check the value array to make sure the pointer is non-null
   and the string length is non-zero.  Note:  this check is normally
   done for output strings:  CHKOSTR is the macro that does the job.
   */
   CHKOSTR ( CHK_STANDARD, "ekacec_c", cvals, cvalen );

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
      chkout_c ( "ekacec_c"                                        );
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

      chkout_c ( "ekacec_c" );
      return;
   }

   /*
   Map the segment and record numbers to the Fortran range.  Get a
   local logical variable to represent the null flag.
   */
   segno++;
   recno++;

   null = isnull;

   ekacec_ ( ( integer    * ) &handle,
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


   chkout_c ( "ekacec_c" );

} /* End ekacec_c */
