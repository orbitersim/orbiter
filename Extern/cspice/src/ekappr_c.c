/*

-Procedure ekappr_c ( EK, append record onto segment )

-Abstract

   Append a new, empty record at the end of a specified E-kernel
   segment.

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

   PRIVATE
   UTILITY

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"

   void ekappr_c ( SpiceInt     handle,
                   SpiceInt     segno,
                   SpiceInt   * recno  )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   handle     I   File handle.
   segno      I   Segment number.
   recno      O   Number of appended record.

-Detailed_Input

   handle      is a file handle of an EK open for write access.

   segno       is the number of the segment to which the record
               is to be added. EK segment numbers range from
               zero to N-1, where N is the number of segments
               in the kernel.

-Detailed_Output

   recno       is the number of the record appended by this
               routine. recno is used to identify the record
               when writing column entries to it. EK record
               numbers range from 0 to N-1, where N is the
               number of records in the segment containing
               the record.

-Parameters

   None.

-Exceptions

   1)  If `handle' is invalid, an error is signaled by a routine in the
       call tree of this routine. The file will not be modified.

   2)  If `segno' is out of range, the error SPICE(INVALIDINDEX) is
       signaled by a routine in the call tree of this routine. The
       file will not be modified.

   3)  If an I/O error occurs while reading or writing the indicated
       file, the error is signaled by a routine in the call tree of
       this routine. The file may be corrupted.

-Files

   See the EK Required Reading for a discussion of the EK file
   format.

-Particulars

   This routine operates by side effects: It appends a new, empty
   record structure to an EK segment. The ordinal position of the
   new record is one greater than the previous number of records in
   in the segment.

   After a new record has been appended to a segment by this routine,
   the record must be populated with data using the ekaceX_c
   routines.  EKs are valid only when all of their column entries
   are initialized.

   To insert a record into a segment at a specified ordinal position,
   use the routine ekappr_c.

   This routine cannot be used with the "fast write" suite of
   routines. See the EK Required Reading for a discussion of the
   fast writers.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1)  Append a record to a specified segment.

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

      This examples creates such EK, with no records in either
      table, and after re-opening the file, inserts a new record
      into the DATAITEMS table.


      Example code begins here.


      /.
         Program ekappr_ex1
      ./
      #include <string.h>
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Local parameters
         ./
         #define EKNAME       "ekappr_ex1.bdb"
         #define DECLEN       201
         #define DESCLN       81
         #define NAMLEN       41
         #define NCOLS        6

         /.
         Local variables
         ./
         SpiceChar            cdecls [NCOLS] [DECLEN];
         SpiceChar            cnames [NCOLS] [SPICE_EK_CSTRLN];
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
         Start the first segment. Since we have no data for this
         segment, start the segment by just defining the new
         segment's schema.
         ./
         ekbseg_c ( handle, "DATAORDERS", NCOLS,   SPICE_EK_CSTRLN,
                    cnames,  DECLEN,      cdecls, &segno           );

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
         End the file by a call to ekcls_c.
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

   -CSPICE Version 1.0.1, 24-NOV-2021 (JDR)

       Edited the header to comply with NAIF standard. Added
       complete code example.

   -CSPICE Version 1.0.0, 09-JAN-2002 (NJB)

-Index_Entries

   append record to EK segment

-&
*/

{ /* Begin ekappr_c */



   /*
   Participate in error tracing.
   */
   chkin_c ( "ekappr_c" );

   /*
   Convert the segment number to a Fortran index.
   */
   segno++;


   ekappr_ (  ( integer * )  &handle,
              ( integer * )  &segno,
              ( integer * )  recno   );

   /*
   Convert the record number to a C style index.
   */

   ( *recno )--;


   chkout_c ( "ekappr_c" );

} /* End ekappr_c */
