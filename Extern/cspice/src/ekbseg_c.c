/*

-Procedure ekbseg_c ( EK, start new segment )

-Abstract

   Start a new segment in an E-kernel.

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
   #undef   ekbseg_c

   void ekbseg_c ( SpiceInt           handle,
                   ConstSpiceChar   * tabnam,
                   SpiceInt           ncols,
                   SpiceInt           cnamln,
                   const void       * cnames,
                   SpiceInt           declen,
                   const void       * decls,
                   SpiceInt         * segno  )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   handle     I   File handle.
   tabnam     I   Table name.
   ncols      I   Number of columns in the segment.
   cnamln     I   Length of names in column name array.
   cnames     I   Names of columns.
   declen     I   Length of declaration strings in declaration array.
   decls      I   Declarations of columns.
   segno      O   Segment number.

-Detailed_Input

   handle      is the handle of an EK file that is open for writing.

   tabnam      is the name of the EK table to which the current
               segment belongs. All segments in the EK file
               designated by handle must have identical column
               attributes. tabnam must not exceed SPICE_EK_TNAMSZ
               characters (see SpiceEK.h) in length. Case is not
               significant. Table names must start with a letter and
               contain only characters from the set
               {A-Z,a-z,0-9,$,_}.

   ncols       is the number of columns in a new segment.

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

               Column names must not exceed CSPICE_EK_CNAMSZ
               characters (see SpiceEK.h) in length. Case is not
               significant. Column names must start with a letter
               and contain only characters from the set
               {A-Z,a-z,0-9,$,_}.

               The declarations are strings that contain
               "keyword=value" assignments that define the
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
       number SPICE_EK_MXCLSG, the error SPICE(INVALIDCOUNT) is signaled by a
       routine in the call tree of this routine.

   5)  If any column name exceeds SPICE_EK_CNAMSZ characters in length, an
       error is signaled by a routine in the call tree of this
       routine.

   6)  If any column name contains non-printable characters, an error
       is signaled by a routine in the call tree of this routine.

   7)  If a declaration cannot be understood by this routine, an
       error is signaled by a routine in the call tree of this
       routine.

   8)  If an non-positive string length or element size is specified,
       an error is signaled by a routine in the call tree of this
       routine.

   9)  If an I/O error occurs while reading or writing the indicated
       file, the error is signaled by a routine in the call tree of
       this routine.

   10) If the `tabnam' input string pointer is null, the error
       SPICE(NULLPOINTER) is signaled.

   11) If the `tabnam' input string has zero length, the error
       SPICE(EMPTYSTRING) is signaled.

   12) If any of the `cnames' or `decls' input array pointers is
       null, the error SPICE(NULLPOINTER) is signaled.

   13) If any of the `cnames' or `decls' input arrays' strings have
       length less than two characters, the error
       SPICE(STRINGTOOSHORT) is signaled.

-Files

   See the EK Required Reading for a discussion of the EK file
   format.

-Particulars

   This routine operates by side effects: it prepares an EK for
   the addition of a new segment. It is not necessary to take
   any special action to "complete" a segment; segments are readable
   after the completion of any record insertion, deletion, write,
   or update operation.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Suppose we want to create an E-kernel which contains a table
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
         Program ekbseg_ex1
      ./
      #include <string.h>
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Local parameters
         ./
         #define EKNAME       "ekbseg_ex1.bdb"
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

   -CSPICE Version 1.2.0, 13-AUG-2021 (JDR)

       Changed input argument name "cnmlen" to "cnamln" for consistency
       with other functions.

       Edited the header to comply with NAIF standard. Added
       complete code example.

   -CSPICE Version 1.1.0, 12-JUL-2002 (NJB)

       Call to C2F_CreateStrArr_Sig replaced with call to C2F_MapStrArr.

   -CSPICE Version 1.0.0, 17-NOV-2001 (NJB)

-Index_Entries

   start new E-kernel segment
   start new EK segment

-&
*/

{ /* Begin ekbseg_c */



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
   chkin_c ( "ekbseg_c" );

   /*
   Check the table name to make sure the pointer is non-null
   and the string length is non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "ekbseg_c", tabnam );

   /*
   Check the column name array to make sure the pointer is non-null
   and the string length is non-zero.  Note:  this check is normally
   done for output strings:  CHKOSTR is the macro that does the job.
   */
   CHKOSTR ( CHK_STANDARD, "ekbseg_c", cnames, cnamln );

   /*
   Check the declaration array to make sure the pointer is non-null
   and the string length is non-zero.
   */
   CHKOSTR ( CHK_STANDARD, "ekbseg_c", decls, declen );

   C2F_MapStrArr ( "ekbseg_c",
                   ncols, cnamln, cnames, &fCnameLen, &fCnameArr );

   if ( failed_c() )
   {
      chkout_c ( "ekbseg_c" );
      return;
   }


   C2F_MapStrArr ( "ekbseg_c",
                   ncols, declen, decls, &fCdeclLen, &fCdeclArr );

   if ( failed_c() )
   {
      free ( fCnameArr );

      chkout_c ( "ekbseg_c" );
      return;
   }


   /*
   Call the f2c'd Fortran routine.  Use explicit type casts for every
   type defined by f2c.
   */
   ekbseg_ ( ( integer  * ) &handle,
             ( char     * ) tabnam,
             ( integer  * ) &ncols,
             ( char     * ) fCnameArr,
             ( char     * ) fCdeclArr,
             ( integer  * ) segno,
             ( ftnlen     ) strlen(tabnam),
             ( ftnlen     ) fCnameLen,
             ( ftnlen     ) fCdeclLen       );

   /*
   Clean up all of our dynamically allocated arrays.
   */
   free ( fCnameArr );
   free ( fCdeclArr );

   /*
   Map segno to C style range.
   */

   (*segno)--;


   chkout_c ( "ekbseg_c" );

} /* End ekbseg_c */
