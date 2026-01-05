/*

-Procedure ekinsr_c ( EK, insert record into segment )

-Abstract

   Add a new, empty record to a specified E-kernel segment at
   a specified index.

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

   void ekinsr_c ( SpiceInt  handle,
                   SpiceInt  segno,
                   SpiceInt  recno )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   handle     I   File handle.
   segno      I   Segment number.
   recno      I   Record number.

-Detailed_Input

   handle      is a file handle of an EK open for write access.

   segno       is the number of the segment to which the record
               is to be added.

   recno       is the index of the new record. recno must be
               in the range 0 : NREC, where NREC is the
               number of records in the segment prior to the
               insertion. If recno is equal to NREC, the
               new record is appended. Otherwise, the new
               record has the ordinal position specified by
               recno, and the records previously occupying
               positions recno : NREC-1 have their indices
               incremented by 1.

-Detailed_Output

   None. See the -Particulars section for a description of the
   effect of this routine.

-Parameters

   None.

-Exceptions

   1)  If `handle' is invalid, an error is signaled by a routine in the
       call tree of this routine. The file will not be modified.

   2)  If `segno' is out of range, the error SPICE(INVALIDINDEX) is
       signaled by a routine in the call tree of this routine. The
       file will not be modified.

   3)  If `recno' is out of range, the error SPICE(INVALIDINDEX) is
       signaled by a routine in the call tree of this routine. The
       file will not be modified.

   4)  If an I/O error occurs while reading or writing the indicated
       file, the error is signaled by a routine in the call tree of
       this routine. The file may be corrupted.

-Files

   See the EK Required Reading for a discussion of the EK file
   format.

-Particulars

   This routine operates by side effects: It adds a new, empty
   record structure to an EK segment at a specified ordinal position.

   After a record has been inserted into a segment by this routine,
   the record must be populated with data using the ekaceX_c
   routines. EKs are valid only when all of their column entries
   are initialized.

   To append a record to a segment, use the routine ekappr_c.

   This routine cannot be used with the "fast write" suite of
   routines. See the EK Required Reading for a discussion of the
   fast writers.

-Examples

   1)  Insert a record into a specified E-kernel segment at a
       specified ordinal position.

       Suppose we have an E-kernel named order_db.EK which contains
       records of orders for data products. The E-kernel has a
       table called DATAORDERS that consists of the set of columns
       listed below:

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


       We'll suppose that the file order_db.EK contains two segments,
       the first containing the DATAORDERS table and the second
       containing the DATAITEMS table.

       If we wanted to insert a new record into the DATAORDERS
       table in position 0, we'd make the following calls:




            EKCLS ( HANDLE )
          #include "SpiceUsr.h"
              .
              .
              .
          /.
          Open the database for write access. This call is
          made when the file already exists. See ekopn_c for
          an example of creating a new file.
          ./
          ekopw_c ( "order_db.ek", &handle );

          /.
          Insert a new, empty record into the DATAORDERS
          table at record number 0. This moves the existing
          records down, so the old record 0 becomes record 1,
          and so on. Recall that the DATAORDERS table
          is in segment number 0.
          ./
          recno = 0;
          segno = 0;

          ekinsr_c ( handle, segno, recno );

          /.
          At this point, the new record is empty. A valid EK
          cannot contain empty records. We fill in the data
          here. Data items are filled in one column at a time.
          The order in which the columns are filled in is not
          important. We use the ekaceX_c (add column entry)
          routines to fill in column entries. We'll assume
          that no entries are null. All entries are scalar,
          so the entry size is 1.
          ./
          isnull  =  SPICEFALSE;
          size    =  1;

          /.
          The following variables will contain the data for
          the new record.
          ./
          ordid    =   10011;
          custid   =   531;
          lname    =   "scientist";
          fname    =   "joe";
          odate    =   "1995-sep-20";
          cost     =   5000.;

          /.
          Note that the names of the routines called
          correspond to the data types of the columns: the
          last letter of the routine name is C, I, or D,
          depending on the data type. Time values are
          converted to ET for storage.
          ./
          ekacei_c ( handle,  segno,  recno, "order_id",
                     size,    ordid,  isnull             );

          ekacei_c ( handle,  segno,  recno, "customer_id",
                     size,    custid, isnull              );

          ekacec_c ( handle,  segno,  recno, "last_name",
                     size,    vallen, lname,  isnull      );

          ekacec_c ( handle,  segno,  recno, "first_name",
                     size,    vallen, fname,  isnull      );

          utc2et_c ( odate,   &et );


          ekaced_c ( handle, segno,  recno, "order_date",
                     size,   et,     isnull               );

          ekaced_c ( handle, segno,  recno, "cost",
                     size,   cost,   isnull               );

          /.
          Close the file to make the update permanent.
          ./
          ekcls_c ( handle );

-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)

-Version

   -CSPICE Version 1.0.1, 24-NOV-2021 (JDR)

       Edited the header to comply with NAIF standard.

   -CSPICE Version 1.0.0, 09-JAN-2002 (NJB)

-Index_Entries

   insert record into EK segment

-&
*/

{ /* Begin ekinsr_c */


   /*
   Participate in error tracing.
   */
   chkin_c ( "ekinsr_c" );

   /*
   Map segment and record numbers to their Fortran ranges.
   */

   segno++;
   recno++;

   ekinsr_ ( &handle, &segno, &recno );


   chkout_c ( "ekinsr_c" );

} /* End ekinsr_c */
