/*

-Procedure ekdelr_c ( EK, delete record from segment )

-Abstract

   Delete a specified record from a specified E-kernel segment.

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

   void ekdelr_c ( SpiceInt   handle,
                   SpiceInt   segno,
                   SpiceInt   recno )

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
               is to be added. EK segment numbers range from
               zero to N-1, where N is the number of segments
               in the kernel.

   recno       is the index of the record to delete. recno must
               be in the range 0 : N, where N is the
               number of records in the segment prior to the
               insertion.

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

   This routine operates by side effects: it deletes a record
   from an EK segment. Deleting a record implies:

      1) All column entries in the record are deleted.

      2) Link counts are decremented for data pages containing
         column entries in the record to be deleted. Pages whose
         link counts drop to zero are freed.

      3) All column indexes are updated for the parent segment.

      4) The link count is decremented for the page containing the
         record pointer structure of the record to be deleted. If
         the link count drops to zero, the page is freed.

      5) The pointer to the deleted record is deleted from the
         record tree for the parent segment.

      6) The segment's metadata is updated to reflect the new
         record count.

-Examples

   1)  Suppose the second segment of an EK file designated by
       handle contains 5 records:

          +-----------------+
          |     Record 0    |
          +-----------------+
          |     Record 1    |
          +-----------------+
          |     Record 2    |
          +-----------------+
          |     Record 3    |
          +-----------------+
          |     Record 4    |
          +-----------------+

       Then the call

          ekdelr_c ( handle, 1, 2 )

       deletes the third record from the segment, leaving the
       segment's contents as follows:

          +-----------------+
          |     Record 0    |
          +-----------------+
          |     Record 1    |
          +-----------------+
          |     Record 3    |
          +-----------------+
          |     Record 4    |
          +-----------------+

-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)

-Version

   -CSPICE Version 1.0.1, 10-AUG-2021 (JDR)

       Edited the header to comply with NAIF standard.

   -CSPICE Version 1.0.0, 16-JUN-2000 (NJB)

-Index_Entries

   delete record from an EK segment

-&
*/

{ /* Begin ekdelr_c */



   /*
   Participate in error tracing.
   */
   chkin_c ( "ekdelr_c" );

   /*
   Convert indices to Fortran style.
   */
   segno++;
   recno++;

   ekdelr_ (  ( integer * )  &handle,
              ( integer * )  &segno,
              ( integer * )  &recno  );


   chkout_c ( "ekdelr_c" );

} /* End ekdelr_c */
