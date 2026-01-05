/*

-Procedure ekucei_c ( EK, update integer column entry )

-Abstract

   Update an integer column entry in a specified EK record.

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
   #undef    ekucei_c


   void ekucei_c  ( SpiceInt          handle,
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
   handle     I   Handle attached to EK file.
   segno      I   Index of segment containing record.
   recno      I   Record in which entry is to be updated.
   column     I   Column name.
   nvals      I   Number of values in new column entry.
   ivals      I   Integer values comprising new column entry.
   isnull     I   Flag indicating whether column entry is null.

-Detailed_Input

   handle      is a file handle attached to an EK open for
               write access.

   segno       is the index of the segment containing the column
               entry to be updated. EK segment numbers range from
               0 to N-1, where N is the number of segments
               in the kernel.

   recno       is the index of the record containing the column
               entry to be updated. This record number is
               relative to the start of the segment indicated by
               segno; the first record in the segment has index 0.

   column      is the name of the column containing the entry to
               be updated.

   nvals,
   ivals       are, respectively, the number of values to insert into
               the specified column and the set of values
               themselves. The data values are written in to the
               specified column and record.

               If the  column has fixed-size entries, then nvals
               must equal the entry size for the specified column.

               For columns with variable-sized entries, the size
               of the new entry need not match the size of the
               entry it replaces. In particular, the new entry
               may be larger.

   isnull      is a logical flag indicating whether the entry is
               null. If isnull is SPICEFALSE, the column entry
               defined by nvals and ivals is added to the
               specified kernel file.

               If ISNULL is SPICETRUE, nvals and ivals are ignored.
               The column entry is marked as a null value.
               The contents of the column entry are undefined.
               If the column has fixed-length, variable-size
               entries, the number of entries is considered to
               be 1.

               The new entry may be null even though it replaces
               a non-null value, and vice versa.

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

   9)  If `column' specifies a column of whose class is not an integer
       class known to this routine, the error SPICE(NOCLASS) is
       signaled by a routine in the call tree of this routine.

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
   be added one logical element at a time. Partial assignments of
   logical elements are not supported.

-Examples

   1)  Replace the value in the third record of the column ICOL in
       the fifth segment of an EK file designated by handle. Set
       the new value to 999.

          #include <SpiceUsr.h>
              .
              .
              .
          ekucei_c ( handle, 4, 2, "ICOL", 1, 999, SPICEFALSE );


   2)  Same as (1), but this time add a null value. The argument
       999 is ignored because the null flag is set to SPICETRUE

          #include <SpiceUsr.h>
              .
              .
              .
          ekucei_c ( handle, 4, 2, "ICOL", 1, 999, SPICETRUE );


   3)  Replace the entry in the third record of the column IARRAY in
       the fifth segment of an EK file designated by handle. Set
       the new value using an array ibuff of 10 values.

          #include <SpiceUsr.h>
              .
              .
              .
          ekucei_c ( handle, 4, 2, "IARRAY", 10, ibuff, SPICEFALSE );

-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)

-Version

   -CSPICE Version 1.0.1, 13-AUG-2021 (JDR)

       Edited the header to comply with NAIF standard.

   -CSPICE Version 1.0.0, 28-AUG-2001 (NJB)

-Index_Entries

   replace integer entry in an EK column

-&
*/

{ /* Begin ekucei_c */


   /*
   Local variables
   */
   logical                 null;

   /*
   Participate in error tracing.
   */
   chkin_c ( "ekucei_c" );

   /*
   Check the column name to make sure the pointer is non-null
   and the string length is non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "ekucei_c", column );

   /*
   Convert the null flag to type logical before passing it to
   ekucei_.  Also map the segment and record numbers to their
   Fortran-style counterparts.
   */

   null = isnull;

   segno++;
   recno++;

   ekucei_  (  ( integer * ) &handle,
               ( integer * ) &segno,
               ( integer * ) &recno,
               ( char    * )  column,
               ( integer * ) &nvals,
               ( integer * )  ivals,
               ( logical * ) &null,
               ( ftnlen    ) strlen(column) );


   chkout_c ( "ekucei_c" );

} /* End ekucei_c */
