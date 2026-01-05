/*

-Procedure ekopw_c ( EK, open file for writing )

-Abstract

   Open an existing E-kernel file for writing.

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


   void ekopw_c ( ConstSpiceChar  * fname,
                  SpiceInt        * handle )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   fname      I   Name of EK file.
   handle     O   Handle attached to EK file.

-Detailed_Input

   fname       is the name of an existing E-kernel file to be
               opened for write access.

-Detailed_Output

   handle      is the DAS file handle of the EK designate by
               `fname'. This handle is used to identify the file
               to other EK routines.

-Parameters

   SPICE_DAS_FTSIZE

               is the maximum number of DAS files that a user can
               have open simultaneously. This includes any files used
               by the DAS system.

               See the header file SpiceDAS.h for the actual value of
               this parameter.

-Exceptions

   1)  If the indicated file cannot be opened, an error is signaled
       by a routine in the call tree of this routine.

   2)  If the indicated file has the wrong architecture version, an
       error is signaled by a routine in the call tree of this
       routine.

   3)  If an I/O error occurs while reading or writing the indicated
       file, the error is signaled by a routine in the call tree of
       this routine.

   4)  If the `fname' input string pointer is null, the error
       SPICE(NULLPOINTER) is signaled.

   5)  If the `fname' input string has zero length, the error
       SPICE(EMPTYSTRING) is signaled.

-Files

   See the EK Required Reading for a discussion of the EK file
   format.

-Particulars

   This routine should be used to open an EK existing file for write
   access.

   Opening an EK file with this routine makes the EK accessible to
   the following CSPICE EK access routines, all of which modify
   the target EK file:

      Begin segment:

         ekbseg_c

      Append, insert, delete records:

         ekappr_c
         ekinsr_c
         ekdelr_c

      Add column entries:

         ekacec_c
         ekaced_c
         ekacei_c

      Update existing column entries:

         ekucec_c
         ekuced_c
         ekucei_c

      Execute fast write:

         ekifld_c
         ekffld_c
         ekaclc_c
         ekacld_c
         ekacli_c

   An EK opened for write access is also accessible for reading.
   The file may be accessed by the CSPICE EK readers

         ekrcec_c
         ekrced_c
         ekrcei_c

      and summary routines:

         eknseg_c
         ekssum_c


   An EK opened for write access cannot be queried. To make an EK
   available to the EK query system, the file must be loaded via
   furnsh_c or eklef_c, rather than by this routine. See the EK Required
   Reading for further information.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) The following program demonstrates how to create a new EK and
      add data to a character column in a given record within the
      file, how to re-open the file for write access and update the
      data, and how to read the data from it.


      Example code begins here.


      /.
         Program ekopw_ex1
      ./
      #include <stdio.h>
      #include <string.h>
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Local parameters.
         ./
         #define EKNAME       "ekopw_ex1.bdb"
         #define IFNAME       "Test EK"
         #define TABLE        "CHR_DATA"
         #define CVLEN        10
         #define DECLEN       201
         #define MAXVAL       4
         #define NCOLS        2
         #define NROWS        6
         #define NRESVC       0

         /.
         Local variables.
         ./
         SpiceChar            cdecls [NCOLS] [DECLEN];
         SpiceChar            cnames [NCOLS] [SPICE_EK_CSTRLN];
         SpiceChar            cvals  [MAXVAL][CVLEN];

         SpiceInt             handle;
         SpiceInt             i;
         SpiceInt             nvals;
         SpiceInt             recno;
         SpiceInt             segno;

         SpiceBoolean         isnull;

         /.
         Open a new EK file.  For simplicity, we won't
         reserve space for the comment area, so the
         number of reserved comment characters is zero.
         The constant IFNAME is the internal file name.
         ./
         ekopn_c ( EKNAME, IFNAME, NRESVC, &handle );

         /.
         Set up the table and column names and declarations
         for the CHR_DATA segment.  We'll index all of
         the columns.
         ./
         strcpy( cnames[0], "CHR_COL_1" );
         strcpy( cdecls[0], "DATATYPE = CHARACTER*(*), "
                            "INDEXED = TRUE, NULLS_OK = TRUE" );

         strcpy( cnames[1], "CHR_COL_2" );
         strcpy( cdecls[1], "DATATYPE = CHARACTER*(9), "
                            "SIZE = VARIABLE, NULLS_OK = TRUE" );

         /.
         Start the segment.
         ./
         ekbseg_c ( handle, TABLE,  NCOLS,   SPICE_EK_CSTRLN,
                    cnames, DECLEN, cdecls, &segno           );

         for ( i = 0; i < NROWS; i++ )
         {
            ekappr_c ( handle, segno, &recno );

            isnull = ( i == 1 );

            sprintf  ( cvals[0], "%d", (int)i );
            ekacec_c ( handle, segno, recno, cnames[0],
                       1,      CVLEN, cvals, isnull    );

            /.
            Array-valued columns follow.
            ./
            sprintf  ( cvals[0], "%d", (int)(10*i)     );
            sprintf  ( cvals[1], "%d", (int)(10*i + 1) );
            sprintf  ( cvals[2], "%d", (int)(10*i + 2) );
            sprintf  ( cvals[3], "%d", (int)(10*i + 3) );
            ekacec_c ( handle, segno, recno, cnames[1],
                       4,      CVLEN, cvals, isnull    );
         }

         /.
         End the file.
         ./
         ekcls_c ( handle );

         /.
         Open the EK for write access.
         ./
         ekopw_c ( EKNAME, &handle );

         /.
         Negate the values in the odd-numbered records
         using the update routines.
         ./
         for ( i = 1; i < NROWS; i = i+2 )
         {
            recno = i;

            isnull = ( i == 1 );

            sprintf  ( cvals[0], "%d", (int)(-i) );
            ekucec_c ( handle, segno, recno, cnames[0],
                       1,      CVLEN, cvals, isnull    );

            /.
            Array-valued columns follow.
            ./
            sprintf  ( cvals[0], "%d", (int)(-10*i)       );
            sprintf  ( cvals[1], "%d", (int)(-(10*i + 1)) );
            sprintf  ( cvals[2], "%d", (int)(-(10*i + 2)) );
            sprintf  ( cvals[3], "%d", (int)(-(10*i + 3)) );
            ekucec_c ( handle, segno, recno, cnames[1],
                       4,      CVLEN, cvals, isnull    );
         }

         /.
         Close the file.
         ./
         ekcls_c ( handle );

         /.
         Open the created file. Show the values added.
         ./
         ekopr_c ( EKNAME, &handle );

         for ( i = 0; i < NROWS; i++ )
         {

            ekrcec_c ( handle, segno, i,      cnames[0],
                       CVLEN, &nvals, cvals, &isnull    );

            if ( ! isnull )
            {
               printf( "Data from column:  %s\n", cnames[0] );
               printf( "   record number:  %d\n", (int)i    );
               printf( "   values       :  %s\n", cvals[0]  );
               printf( " \n" );
            }
            else
            {
               printf( "Record  %d flag is NULL.\n", (int)i );
               printf( " \n" );
            }

            /.
            Array-valued columns follow.
            ./
            ekrcec_c ( handle, segno, i,      cnames[1],
                       CVLEN, &nvals, cvals, &isnull    );

            if ( ! isnull )
            {
               printf( "Data from column:  %s\n", cnames[1]   );
               printf( "   record number:  %d\n", (int)i      );
               printf( "   values       :  %s  %s  %s  %s\n",
                       cvals[0], cvals[1], cvals[2], cvals[3] );
               printf( " \n" );
            }

         }

         /.
         Close the file.
         ./
         ekcls_c ( handle );

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Data from column:  CHR_COL_1
         record number:  0
         values       :  0

      Data from column:  CHR_COL_2
         record number:  0
         values       :  0          1          2          3

      Record  1 flag is NULL.

      Data from column:  CHR_COL_1
         record number:  2
         values       :  2

      Data from column:  CHR_COL_2
         record number:  2
         values       :  20         21         22         23

      Data from column:  CHR_COL_1
         record number:  3
         values       :  -3

      Data from column:  CHR_COL_2
         record number:  3
         values       :  -30        -31        -32        -33

      Data from column:  CHR_COL_1
         record number:  4
         values       :  4

      Data from column:  CHR_COL_2
         record number:  4
         values       :  40         41         42         43

      Data from column:  CHR_COL_1
         record number:  5
         values       :  -5

      Data from column:  CHR_COL_2
         record number:  5
         values       :  -50        -51        -52        -53


      Note that the second record does not appear due to setting the
      `isnull' flag to SPICETRUE for that record. The odd value record
      numbers have negative values as a result of the update calls.

      After run completion, a new EK exists in the output directory.

-Restrictions

   1)  No more than SPICE_DAS_FTSIZE DAS files may be opened simultaneously.
       See the header file SpiceDAS.h for the value of SPICE_DAS_FTSIZE.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)

-Version

   -CSPICE Version 1.0.2, 02-AUG-2021 (JDR)

       Edited the header to comply with NAIF standard. Added complete
       code example.

       Added SPICE_DAS_FTSIZE parameter description. Updated index entry.

   -CSPICE Version 1.0.1, 09-JAN-2002 (NJB)

       Documentation change: instances of the phrase "fast load"
       were replaced with "fast write."

   -CSPICE Version 1.0.0, 25-MAY-1999 (NJB)

-Index_Entries

   open existing EK for writing

-&
*/

{ /* Begin ekopw_c */


   /*
   Participate in error tracing.
   */
   chkin_c ( "ekopw_c" );


   /*
   Check the file name string.  The pointer must be non-null
   and the string length must be at least 1.
   */
   CHKFSTR ( CHK_STANDARD, "ekopw_c", fname );


   /*
   Call the f2c'd routine.
   */
   ekopw_ (  ( char      * ) fname,
             ( integer   * ) handle,
             ( ftnlen      ) strlen(fname) );


   chkout_c ( "ekopw_c" );

} /* End ekopw_c */
