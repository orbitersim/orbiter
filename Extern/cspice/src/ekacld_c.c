/*

-Procedure ekacld_c ( EK, add double precision column to segment )

-Abstract

   Add an entire double precision column to an EK segment.

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
   #include <stdlib.h>
   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZst.h"
   #include "SpiceZmc.h"
   #undef    ekacld_c


   void ekacld_c ( SpiceInt                handle,
                   SpiceInt                segno,
                   ConstSpiceChar        * column,
                   ConstSpiceDouble      * dvals,
                   ConstSpiceInt         * entszs,
                   ConstSpiceBoolean     * nlflgs,
                   ConstSpiceInt         * rcptrs,
                   SpiceInt              * wkindx  )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   handle     I   EK file handle.
   segno      I   Number of segment to add column to.
   column     I   Column name.
   dvals      I   Double precision values to add to column.
   entszs     I   Array of sizes of column entries.
   nlflgs     I   Array of null flags for column entries.
   rcptrs     I   Record pointers for segment.
   wkindx    I-O  Work space for column index.

-Detailed_Input

   handle      is the handle of an EK file that is open for writing.
               A "begin segment for fast write" operation must
               have already been performed for the designated
               segment.

   segno       is the number of the segment to which data is to be
               added. Segments are numbered from 0 to nseg-1, where
               nseg is the count of segments in the file.

   column      is the name of the column to be added. All of
               the data for the named column will be added in
               one shot.

   dvals       is an array containing the entire set of column
               entries for the specified column. The entries
               are listed in row-order:  the column entry for the
               first row of the segment is first, followed by the
               column entry for the second row, and so on. The
               number of column entries must match the declared
               number of rows in the segment. For columns having
               fixed-size entries, a null entry must be allocated
               the same amount of space occupied by a non-null
               entry in the array dvals. For columns having
               variable-size entries, null entries do not require
               any space in the dvals* array, but in any case must
               have their allocated space described correctly by
               the corresponding element of the entszs array
               (described below).

   entszs      is an array containing sizes of column entries.
               The Ith element of entszs gives the size of the
               Ith column entry. entszs is used only for columns
               having variable-size entries. For such columns,
               the dimension of entszs must be at least nrows.
               The size of null entries should be set to zero.

               For columns having fixed-size entries, the
               dimension of this array may be any positive value.

   nlflgs      is an array of logical flags indicating whether
               the corresponding entries are null. If the Ith
               element of nlflgs is SPICEFALSE, the Ith column entry
               defined by dvals and entszs is added to the
               current segment in the specified kernel file.

               If the Ith element of nlfgls is SPICETRUE, the
               contents of the Ith column entry are undefined.

               nlflgs is used only for columns that allow null
               values; it's ignored for other columns.

   rcptrs      is an array of record pointers for the input
               segment. This array is obtained as an output
               from ekifld_c, the routine called to initiate a
               fast write.

   wkindx      is a work space array used for building a column
               index. If the column is indexed, the dimension of
               wkindx_c must be at nrows, where nrows is the number
               of rows in the column. If the column is not
               indexed, this work space is not used, so the
               dimension may be any positive value.

-Detailed_Output

   None. See -Particulars for a description of the effect of this
   routine.

-Parameters

   None.

-Exceptions

   1)  If `handle' is invalid, an error is signaled by a routine in the
       call tree of this routine.

   2)  If `column' is not the name of a declared column, an error is
       signaled by a routine in the call tree of this routine.

   3)  If `column' specifies a column of whose data type is not
       integer, the error SPICE(WRONGDATATYPE) is signaled by a
       routine in the call tree of this routine.

   4)  If the specified column already contains ANY entries, an error
       is signaled by a routine in the call tree of this routine.

   5)  If an I/O error occurs while reading or writing the indicated
       file, the error is signaled by a routine in the call tree of
       this routine.

   6)  If the `column' input string pointer is null, the error
       SPICE(NULLPOINTER) is signaled.

   7)  If the `column' input string has zero length, the error
       SPICE(EMPTYSTRING) is signaled.

   8)  If memory cannot be allocated to create the temporary variable
       required for the execution of the underlying Fortran routine,
       the error SPICE(MALLOCFAILED) is signaled.

-Files

   See the EK Required Reading for a discussion of the EK file
   format.

-Particulars

   This routine operates by side effects: it modifies the named
   EK file by adding data to the specified column. This routine
   writes the entire contents of the specified column in one shot.
   This routine creates columns much more efficiently than can be
   done by sequential calls to ekaced_c, but has the drawback that
   the caller must use more memory for the routine's inputs. This
   routine cannot be used to add data to a partially completed
   column.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Suppose we want to create an Sequence Component E-kernel
      named "ekacld_ex1.bdb" which contains records of orders for
      data products. The E-kernel has a table called DATAORDERS
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


      The file "ekacld_ex1.bdb" will contain two segments, the first
      containing the DATAORDERS table and the second containing the
      DATAITEMS table.

      This example demonstrates how to open a new EK file and create
      the first of the segments described above.

      Use the LSK kernel below to load the leap seconds and time
      constants required for the conversions.

         naif0012.tls


      Example code begins here.


      /.
         Program ekacld_ex1
      ./
      #include <stdio.h>
      #include <string.h>
      #include "SpiceUsr.h"

      int main()
      {
         /.
         Constants
         ./
         #define  CNMLEN      ( SPICE_EK_CNAMSZ + 1 )
         #define  DECLEN        201
         #define  EKNAME        "ekacld_ex1.bdb"
         #define  FNMLEN        50
         #define  IFNAME        "Test EK/Created 03-JUL-2018"
         #define  LNMLEN        50
         #define  LSK           "naif0012.tls"
         #define  NCOLS         6
         #define  NRESVC        0
         #define  NROWS         9
         #define  TABLE         "DATAORDERS"
         #define  TNMLEN        SPICE_EK_TNAMSZ
         #define  UTCLEN        30


         /.
         Local variables
         ./
         SpiceBoolean            nlflgs [ NROWS  ];

         SpiceChar               cdecls [ NCOLS ] [ DECLEN ];
         SpiceChar               cnames [ NCOLS ] [ CNMLEN ];
         SpiceChar               fnames [ NROWS ] [ FNMLEN ];
         SpiceChar               lnames [ NROWS ] [ LNMLEN ];
         SpiceChar               dateStr[ UTCLEN ];

         SpiceDouble             costs  [ NROWS ];
         SpiceDouble             ets    [ NROWS ];

         SpiceInt                cstids [ NROWS ];
         SpiceInt                ordids [ NROWS ];
         SpiceInt                handle;
         SpiceInt                i;
         SpiceInt                rcptrs [ NROWS ];
         SpiceInt                segno;
         SpiceInt                sizes  [ NROWS ];
         SpiceInt                wkindx [ NROWS ];


         /.
         Load a leapseconds kernel for UTC/ET conversion.
         ./
         furnsh_c ( LSK );

         /.
         Open a new EK file.  For simplicity, we will not
         reserve any space for the comment area, so the
         number of reserved comment characters is zero.
         The constant IFNAME is the internal file name.
         ./
         ekopn_c ( EKNAME, IFNAME, NRESVC, &handle );

         /.
         Set up the table and column names and declarations
         for the DATAORDERS segment.  We'll index all of
         the columns.  All columns are scalar, so we omit
         the size declaration.  Only the COST column may take
         null values.
         ./
         strcpy ( cnames[0], "ORDER_ID"                           );
         strcpy ( cdecls[0], "DATATYPE = INTEGER, INDEXED = TRUE" );

         strcpy ( cnames[1], "CUSTOMER_ID"                        );
         strcpy ( cdecls[1], "DATATYPE = INTEGER, INDEXED = TRUE" );

         strcpy ( cnames[2], "LAST_NAME"                          );
         strcpy ( cdecls[2], "DATATYPE = CHARACTER*(*),"
                             "INDEXED  = TRUE"                    );

         strcpy ( cnames[3], "FIRST_NAME"                         );
         strcpy ( cdecls[3], "DATATYPE = CHARACTER*(*),"
                             "INDEXED  = TRUE"                    );

         strcpy ( cnames[4], "ORDER_DATE"                         );
         strcpy ( cdecls[4], "DATATYPE = TIME, INDEXED  = TRUE"   );

         strcpy ( cnames[5], "COST"                               );
         strcpy ( cdecls[5], "DATATYPE = DOUBLE PRECISION,"
                             "INDEXED  = TRUE,"
                             "NULLS_OK = TRUE"                    );

         /.
         Start the segment.  We presume the number of  rows
         of data is known in advance.
         ./
         ekifld_c ( handle,  TABLE,   NCOLS,  NROWS,   CNMLEN,
                    cnames,  DECLEN,  cdecls, &segno,  rcptrs );

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


         The null flags array indicates which entries are null.
         It is ignored for columns that don't allow null
         values.  In this case, only the COST column allows
         nulls.

         Fill in data arrays and null flag arrays here.  This code
         section would normally be replaced by calls to user functions
         returning column values.
         ./

         for ( i = 0;  i < NROWS;  i++ )
         {
            ordids[i]  =  i;
            cstids[i]  =  i*100;
            costs [i]  =  (SpiceDouble) 100*i;

            sprintf  ( fnames[i], "Order %d Customer first name", i );
            sprintf  ( lnames[i], "Order %d Customer last name",  i );
            sprintf  ( dateStr,   "1998 Mar %d",                  i );

            utc2et_c ( dateStr, ets+i );

            nlflgs[i]  =  SPICEFALSE;
         }

         nlflgs[1] = SPICETRUE;


         /.
         The sizes array shown below is ignored for scalar
         and fixed-size array columns, so we need not
         initialize it.  For variable-size arrays, the
         Ith element of the sizes array must contain the size
         of the Ith column entry in the column being written.
         Normally, the sizes array would be reset for each
         variable-size column.

         Add the columns of data to the segment.  All of the
         data for each column is written in one shot.
         ./
         ekacli_c ( handle,  segno,   "order_id",    ordids,
                    sizes,   nlflgs,  rcptrs,        wkindx );

         ekacli_c ( handle,  segno,   "customer_id", cstids,
                    sizes,   nlflgs,  rcptrs,        wkindx );

         ekaclc_c ( handle,  segno,   "last_name",   LNMLEN,
                    lnames,  sizes,   nlflgs,        rcptrs,  wkindx );

         ekaclc_c ( handle,  segno,   "first_name",  FNMLEN,
                    fnames,  sizes,   nlflgs,        rcptrs,  wkindx );

         ekacld_c ( handle,  segno,   "order_date",  ets,
                    sizes,   nlflgs,  rcptrs,        wkindx );

         ekacld_c ( handle,  segno,   "cost",        costs,
                    sizes,   nlflgs,  rcptrs,        wkindx );

         /.
         Complete the segment. The `rcptrs' array is that
         returned by ekifld_c.
         ./
         ekffld_c ( handle, segno, rcptrs );

         /.
         At this point, the second segment could be
         created by an analogous process.  In fact, the
         second segment could be created at any time; it is
         not necessary to populate the first segment with
         data before starting the second segment.

         The file must be closed by a call to ekcls_c.
         ./
         ekcls_c ( handle );

         return ( 0 );
      }


      When this program is executed, no output is presented on
      screen. After run completion, a new EK file exists in the
      output directory.

-Restrictions

   1)  Only one segment can be created at a time using the fast
       write routines.

   2)  No other EK operation may interrupt a fast write. For
       example, it is not valid to issue a query while a fast write
       is in progress.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   E.D. Wright         (JPL)

-Version

   -CSPICE Version 1.1.3, 10-AUG-2021 (JDR)

       Edited the header to comply with NAIF standard.

       Updated code example to include the string.h file, and to
       update EK parameter names to "SPICE_EK_CNAMSZ" and
       "SPICE_EK_TNAMSZ".

       Added entry #8 to -Exceptions section.

   -CSPICE Version 1.1.2, 14-AUG-2006 (EDW)

       Replace mention of ldpool_c with furnsh_c.

   -CSPICE Version 1.1.1, 09-JAN-2002 (NJB)

       Documentation change: instances of the phrase "fast load"
       were replaced with "fast write."

   -CSPICE Version 1.1.0, 07-JUL-1998 (NJB)

       Bug fix: now uses dynamically allocated array of type logical
       to interface with underlying f2c'd function ekacld_.

       Now maps segno from C to Fortran range.

       Added "undef" of masking macro. Changed input pointer types
       to pointers to const objects.

   -CSPICE Version 1.0.0, 01-APR-1998 (NJB)

       Based on SPICELIB Version 1.0.0, 08-NOV-1995 (NJB)

-Index_Entries

   write entire double precision column to EK segment

-&
*/

{ /* Begin ekacld_c */

   /*
   Local variables
   */
   logical               * logicalFlags;

   SpiceEKSegSum           summary;

   SpiceInt                fSegno;
   SpiceInt                i;
   SpiceInt                n;



   /*
   Participate in error tracing.
   */
   chkin_c ( "ekacld_c" );


   /*
   Check the column name to make sure the pointer is non-null
   and the string length is non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "ekacld_c", column );


   /*
   Get the row count for this segment.
   */
   ekssum_c ( handle, segno, &summary );

   n = summary.nrows;


   /*
   Allocate an array of logicals and assign values from the input
   array of SpiceBooleans.
   */

   logicalFlags = ( logical * ) malloc ( n * sizeof(logical) );

   if ( !logicalFlags )
   {
      setmsg_c ( "Failure on malloc call to create null flag array "
                 "for column values."                              );
      sigerr_c ( "SPICE(MALLOCFAILED)"                             );
      chkout_c ( "ekacld_c"                                        );
      return;
   }


   /*
   Copy the input null flags to our array of type logical.
   */
   for ( i = 0;  i < n;  i++ )
   {
      logicalFlags[i] = nlflgs[i];
   }


   /*
   Map the segment number to the Fortran range.
   */
   fSegno = segno + 1;

   /*
   Call the f2c'd routine.
   */
   ekacld_ ( ( integer    * ) &handle,
             ( integer    * ) &fSegno,
             ( char       * ) column,
             ( doublereal * ) dvals,
             ( integer    * ) entszs,
             ( logical    * ) logicalFlags,
             ( integer    * ) rcptrs,
             ( integer    * ) wkindx,
             ( ftnlen       ) strlen(column)  );


   /*
   We're done with the local null flag array.
   */
   free ( logicalFlags );


   chkout_c ( "ekacld_c" );

} /* End ekacld_c */
