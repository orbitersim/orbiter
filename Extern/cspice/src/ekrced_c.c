/*

-Procedure ekrced_c ( EK, read column entry element, d.p. )

-Abstract

   Read data from a double precision column in a specified EK
   record.

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

   void ekrced_c ( SpiceInt           handle,
                   SpiceInt           segno,
                   SpiceInt           recno,
                   ConstSpiceChar   * column,
                   SpiceInt         * nvals,
                   SpiceDouble      * dvals,
                   SpiceBoolean     * isnull )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   handle     I   Handle attached to EK file.
   segno      I   Index of segment containing record.
   recno      I   Record from which data is to be read.
   column     I   Column name.
   nvals      O   Number of values in column entry.
   dvals      O   D.p. values in column entry.
   isnull     O   Flag indicating whether column entry is null.

-Detailed_Input

   handle      is an EK file handle. The file may be open for
               read or write access.

   segno       is the index of the segment from which data is to
               be read. The first segment in the file has index 0.

   recno       is the index of the record from which data is to be
               read. This record number is relative to the start
               of the segment indicated by segno; the first
               record in the segment has index 0.

   column      is the name of the column from which data is to be
               read.

-Detailed_Output

   nvals,
   ivals       are, respectively, the number of values found in
               the specified column entry and the set of values
               themselves.

               For columns having fixed-size entries, when a
               a column entry is null, nvals is still set to the
               column entry size. For columns having variable-
               size entries, nvals is set to 1 for null entries.

   isnull      is a logical flag indicating whether the returned
               column entry is null.

-Parameters

   None.

-Exceptions

   1)  If `handle' is invalid, an error is signaled by a routine in the
       call tree of this routine.

   2)  If `segno' is out of range, an error is signaled by a routine in
       the call tree of this routine.

   3)  If `recno' is out of range, an error is signaled by a routine in
       the call tree of this routine.

   4)  If `column' is not the name of a declared column, an error
       is signaled by a routine in the call tree of this routine.

   5)  If `column' specifies a column of whose data type is not double
       precision, the error SPICE(WRONGDATATYPE) is signaled by a
       routine in the call tree of this routine.

   6)  If `column' specifies a column of whose class is not a double
       precision class known to this routine, the error
       SPICE(NOCLASS) is signaled by a routine in the call tree of
       this routine.

   7)  If an attempt is made to read an uninitialized column entry,
       an error is signaled by a routine in the call tree of this
       routine. A null entry is considered to be initialized, but
       entries do not contain null values by default.

   8)  If an I/O error occurs while reading or writing the indicated
       file, the error is signaled by a routine in the call tree of
       this routine.

   9)  If the `column' input string pointer is null, the error
       SPICE(NULLPOINTER) is signaled.

   10) If the `column' input string has zero length, the error
       SPICE(EMPTYSTRING) is signaled.

-Files

   See the EK Required Reading for a discussion of the EK file
   format.

-Particulars

   This routine is a utility that allows an EK file to be read
   directly without using the high-level query interface.

-Examples

   1)  Read the value in the third record of the column DCOL in
       the fifth segment of an EK file designated by HANDLE.


          #include "SpiceUsr.h"
             .
             .
             .
          ekrced_c ( handle, 4, 2, "DCOL", &n, &dval, &isnull );

-Restrictions

   1)  EK files open for write access are not necessarily readable.
       In particular, a column entry can be read only if it has been
       initialized. The caller is responsible for determining
       when it is safe to read from files open for write access.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)

-Version

   -CSPICE Version 1.0.1, 02-AUG-2021 (JDR)

       Edited the header to comply with NAIF standard.

   -CSPICE Version 1.0.0, 04-JUL-2000 (NJB)

-Index_Entries

   read double precision data from EK column

-&
*/

{ /* Begin ekrced_c */

   /*
   Local variables
   */
   logical                 null;

   /*
   Participate in error tracing.
   */
   chkin_c ( "ekrced_c" );


   /*
   Check the column name to make sure the pointer is non-null
   and the string length is non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "ekrced_c", column );


   /*
   Map the segment and record numbers to their Fortran-style
   values.  Pass a flag of type logical to ekrced_.
   */

   segno++;
   recno++;

   ekrced_ ( ( integer    * ) &handle,
             ( integer    * ) &segno,
             ( integer    * ) &recno,
             ( char       * ) column,
             ( integer    * ) nvals,
             ( doublereal * ) dvals,
             ( logical    * ) &null,
             ( ftnlen       ) strlen(column) );

   /*
   Set the output null flag.
   */

   *isnull = null;


   chkout_c ( "ekrced_c" );

} /* End ekrced_c */
