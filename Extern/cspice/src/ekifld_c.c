/*

-Procedure ekifld_c ( EK, initialize segment for fast write )

-Abstract

   Initialize a new E-kernel segment to allow fast writing.

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
   #include "SpiceZst.h"
   #include "SpiceZmc.h"
   #include "SpiceZim.h"
   #undef    ekifld_c


   void ekifld_c ( SpiceInt           handle,
                   ConstSpiceChar   * tabnam,
                   SpiceInt           ncols,
                   SpiceInt           nrows,
                   SpiceInt           cnamln,
                   const void       * cnames,
                   SpiceInt           declen,
                   const void       * decls,
                   SpiceInt         * segno,
                   SpiceInt         * rcptrs )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   handle     I   File handle.
   tabnam     I   Table name.
   ncols      I   Number of columns in the segment.
   nrows      I   Number of rows in the segment.
   cnamln     I   Length of names in column name array.
   cnames     I   Names of columns.
   declen     I   Length of declaration strings in declaration array.
   decls      I   Declarations of columns.
   segno      O   Segment number.
   rcptrs     O   Array of record pointers.

-Detailed_Input

   handle      is the handle of an EK file open for write access.
               A new segment is to be created in this file.

   tabnam      is the name of the EK table to which the current
               segment belongs. All segments in the EK file
               designated by handle must have identical column
               attributes. tabnam must not exceed SPICE_EK_TNAMSZ
               (see SpiceEK.h) characters in length. Case is not
               significant. Table names must start with a letter
               and contain only characters from the set
               {A-Z,a-z,0-9,$,_}.

   ncols       is the number of columns in a new segment.

   nrows       is the number of rows in a new segment. Each
               column to be added to the segment must contain
               the number of entries indicated by nrows.

   cnamln,
   cnames      are, respectively, the length of the column name
               strings in the column name array, and the base
               address of the array itself. The array should have
               dimensions

                  [ncols][cnamln]

   declen,
   decls       are, respectively, the length of the declaration
               strings in the declaration array, and the base
               address of the array itself. The array should have
               dimensions

                  [ncols][declen]

               The Ith element of cnames and the Ith element of decls
               apply to the Ith column in the segment.

               Column names must not exceed SPICE_EK_CNAMSZ (see
               SpiceEK.h) characters in length. Case is not
               significant. Column names must start with a letter
               and contain only characters from the set
               {A-Z,a-z,0-9,$,_}.

               The declarations are strings that contain
               `keyword=value' assignments that define the
               attributes of the columns to which they apply. The
               column attributes that are defined by a column
               declaration are:

                  DATATYPE
                  SIZE
                  <is the column indexed?>
                  <does the column allow null values?>

               The form of a declaration is

                  "DATATYPE  = <type>,
                   SIZE      = <size>,
                   INDEXED   = <boolean>,
                   NULLS_OK  = <boolean>"

               For example, an indexed, scalar, integer column
               that allows null values would have the declaration

                  "DATATYPE  = INTEGER,
                   SIZE      = 1,
                   INDEXED   = TRUE,
                   NULLS_OK  = TRUE"

               Commas are required to separate the assignments
               within declarations; white space is optional;
               case is not significant.

               The order in which the attribute keywords are
               listed in declaration is not significant.

               Every column in a segment must be declared.

               Each column entry is effectively an array, each
               element of which has the declared data type. The
               SIZE keyword indicates how many elements are in
               each entry of the column in whose declaration the
               keyword appears. Note that only scalar-valued
               columns (those for which SIZE = 1) may be
               referenced in query constraints. A size
               assignment has the syntax

                  SIZE = <integer>

               or
                  SIZE = VARIABLE

               The size value defaults to 1 if omitted.

               The DATATYPE keyword defines the data type of
               column entries. The DATATYPE assignment syntax
               has any of the forms

                  DATATYPE = CHARACTER*(<length>)
                  DATATYPE = CHARACTER*(*)
                  DATATYPE = DOUBLE PRECISION
                  DATATYPE = INTEGER
                  DATATYPE = TIME

               As the datatype declaration syntax suggests,
               character strings may have fixed or variable
               length. Variable-length strings are allowed only
               in columns of size 1.

               Optionally, scalar-valued columns may be indexed.
               To create an index for a column, use the assignment

                  INDEXED = TRUE

               By default, columns are not indexed.

               Optionally, any column can allow null values. To
               indicate that a column may allow null values, use
               the assignment

                  NULLS_OK = TRUE

               in the column declaration. By default, null
               values are not allowed in column entries.

-Detailed_Output

   segno       is the number of the segment to which data is to be
               added. Segments are numbered from 0 to nseg-1, where
               nseg is the count of segments in the file. Segment
               numbers are used as unique identifiers by other EK
               access routines.

   rcptrs      is an array of record pointers for the input
               segment. This array must not be modified by the
               caller.

               The array rcptrs must be passed as an input to
               each column addition routine called while
               writing the specified segment.

               rcptrs must be declared with dimension nrows.

-Parameters

   None.

-Exceptions

   1)  If `handle' is invalid, an error is signaled by a routine in the
       call tree of this routine.

   2)  If `tabnam' is more than SPICE_EK_TNAMSZ characters long, an error
       is signaled by a routine in the call tree of this routine.

   3)  If `tabnam' contains any nonprintable characters, an error
       is signaled by a routine in the call tree of this routine.

   4)  If `ncols' is non-positive or greater than the maximum allowed
       number SPICE_EK_MXCLSG, an error is signaled by a routine in the call
       tree of this routine.

   5)  If `nrows' is non-positive, the error SPICE(INVALIDCOUNT)
       is signaled by a routine in the call tree of this routine.

   6)  If any column name exceeds SPICE_EK_CNAMSZ characters in length, an
       error is signaled by a routine in the call tree of this
       routine.

   7)  If any column name contains non-printable characters, an error
       is signaled by a routine in the call tree of this routine.

   8)  If a declaration cannot be understood by this routine, an
       error is signaled by a routine in the call tree of this
       routine.

   9)  If an non-positive string length or element size is specified,
       an error is signaled by a routine in the call tree of this
       routine.

   10) If an I/O error occurs while reading or writing the indicated
       file, the error is signaled by a routine in the call tree of
       this routine.

   11) If the `tabnam' input string pointer is null, the error
       SPICE(NULLPOINTER) is signaled.

   12) If the `tabnam' input string has zero length, the error
       SPICE(EMPTYSTRING) is signaled.

   13) If any of the `cnames' or `decls' input array pointers is
       null, the error SPICE(NULLPOINTER) is signaled.

   14) If any of the `cnames' or `decls' input arrays' strings have
       length less than two characters, the error
       SPICE(STRINGTOOSHORT) is signaled.

-Files

   See the EK Required Reading for a discussion of the EK file
   format.

-Particulars

   This routine prepares an EK for the creation of a new segment via
   the fast column writer routines. After this routine is called,
   the columns of the segment are filled in by calls to the fast
   column writer routines of the appropriate data types. The fast
   column writer routines are:

      ekaclc_c {EK, add column, character}
      ekacld_c {EK, add column, double precision}
      ekacli_c {EK, add column, integer}

   When all of the columns have been added, the write operation is
   completed by a call to ekffld_c {EK, finish fast write}.

   The segment is not valid until ekffld_c has been called.

   The EK system supports only one fast write at a time. It is
   not possible use the fast write routines to simultaneously write
   multiple segments, either in the same EK file or in different
   files.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Suppose we want to create an Sequence Component E-kernel
      named "ekifld_ex1.bdb" which contains records of orders for
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


      The file "ekifld_ex1.bdb" will contain two segments, the first
      containing the DATAORDERS table and the second containing the
      DATAITEMS table.

      This example demonstrates how to open a new EK file and create
      the first of the segments described above.

      Use the LSK kernel below to load the leap seconds and time
      constants required for the conversions.

         naif0012.tls


      Example code begins here.


      /.
         Program ekifld_ex1
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
         #define  EKNAME        "ekifld_ex1.bdb"
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

   -CSPICE Version 2.4.0, 10-AUG-2021 (JDR)

       Edited the -Examples section to comply with NAIF standard.
       Updated code example to include the string.h file, and to
       update EK parameter names to "SPICE_EK_CNAMSZ" and
       "SPICE_EK_TNAMSZ".

       Changed the input argument name "cnmlen" to "cnamln" for
       consistency with other routines.

   -CSPICE Version 2.3.1, 14-AUG-2006 (EDW)

       Replace mention of ldpool_c with furnsh_c.

   -CSPICE Version 2.3.0, 12-JUL-2002 (NJB)

       Call to C2F_CreateStrArr_Sig replaced with call to C2F_MapStrArr.

   -CSPICE Version 2.2.0, 10-JAN-2002 (NJB)

       Const-qualified input arrays. Added casts to type (void *)
       to expressions passed to free(), in order to suppress compilation
       warnings under MS Visual C++/C.

       Documentation change: instances of the phrase "fast load"
       were replaced with "fast write."

       Corrected parameter names giving maximum table and column name
       lengths.

   -CSPICE Version 2.1.0, 14-FEB-2000 (NJB)

       Calls to C2F_CreateStrArr replaced with calls to error-signaling
       version of this routine:  C2F_CreateStrArr_Sig.

   -CSPICE Version 2.0.0, 07-JUL-1999 (NJB)

       Output segment number segno is now mapped to C range.

   -CSPICE Version 1.0.0, 08-MAR-1999 (NJB)

       Based on SPICELIB Version 1.0.0, 25-OCT-1995 (NJB)

-Index_Entries

   start new E-kernel segment for fast writing
   start new EK segment for fast writing

-&
*/

{ /* Begin ekifld_c */

   /*
   Local variables
   */
   SpiceChar             * fCnameArr;
   SpiceChar             * fCdeclArr;

   SpiceInt                fCnameLen;
   SpiceInt                fCdeclLen;



   /*
   Participate in error tracing.
   */
   chkin_c ( "ekifld_c" );


   /*
   Check the table name to make sure the pointer is non-null
   and the string length is non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "ekifld_c", tabnam );

   /*
   Check the column name array to make sure the pointer is non-null
   and the string length is non-zero.  Note:  this check is normally
   done for output strings:  CHKOSTR is the macro that does the job.
   */
   CHKOSTR ( CHK_STANDARD, "ekifld_c", cnames, cnamln );

   /*
   Check the declaration array to make sure the pointer is non-null
   and the string length is non-zero.
   */
   CHKOSTR ( CHK_STANDARD, "ekifld_c", decls, declen );

   /*
   Create a Fortran-style array of column names.
   */

   C2F_MapStrArr ( "ekifld_c",
                   ncols, cnamln, cnames, &fCnameLen, &fCnameArr );

   if ( failed_c() )
   {
      chkout_c ( "ekifld_c" );
      return;
   }


   /*
   Produce a Fortran-style array for the declarations, as we did for
   the column names.
   */
   C2F_MapStrArr ( "ekifld_c",
                   ncols, declen, decls,  &fCdeclLen, &fCdeclArr );

   if ( failed_c() )
   {
      free ( (void *) fCnameArr );

      chkout_c ( "ekifld_c" );
      return;
   }


   /*
   Call the f2c'd Fortran routine.  Use explicit type casts for every
   type defined by f2c.
   */
   ekifld_ ( ( integer  * ) &handle,
             ( char     * ) tabnam,
             ( integer  * ) &ncols,
             ( integer  * ) &nrows,
             ( char     * ) fCnameArr,
             ( char     * ) fCdeclArr,
             ( integer  * ) segno,
             ( integer  * ) rcptrs,
             ( ftnlen     ) strlen(tabnam),
             ( ftnlen     ) fCnameLen,
             ( ftnlen     ) fCdeclLen       );

   /*
   Clean up all of our dynamically allocated arrays.
   */
   free ( (void *) fCnameArr );
   free ( (void *) fCdeclArr );

   /*
   Map segno to C style range.
   */

   (*segno)--;


   chkout_c ( "ekifld_c" );

} /* End ekifld_c */
