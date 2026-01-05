/*

-Procedure ekopn_c ( EK, open new file )

-Abstract

   Open a new E-kernel file and prepare the file for writing.

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
   NAIF_IDS
   TIME

-Keywords

   EK
   FILES
   UTILITY

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZst.h"
   #include "SpiceZmc.h"

   void ekopn_c ( ConstSpiceChar    * fname,
                  ConstSpiceChar    * ifname,
                  SpiceInt            ncomch,
                  SpiceInt          * handle )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   fname      I   Name of EK file.
   ifname     I   Internal file name.
   ncomch     I   The number of characters to reserve for comments.
   handle     O   Handle attached to new EK file.

-Detailed_Input

   fname       is the name of a new E-kernel file to be created.

   ifname      is the internal file name of a new E-kernel. The
               internal file name may be up to 60 characters in
               length, not including the null terminator.

   ncomch      is the amount of space, measured in characters, to
               be allocated in the comment area when the new EK
               file is created. It is not necessary to allocate
               space in advance in order to add comments, but
               doing so may greatly increase the efficiency with
               which comments may be added. Making room for
               comments after data has already been added to the
               file involves moving the data, and thus is slower.

               `ncomch' must be greater than or equal to zero.

-Detailed_Output

   handle      is the EK handle of the file designated by `fname'.
               This handle is used to identify the file to other
               EK routines.

-Parameters

   SPICE_DAS_FTSIZE

               is the maximum number of DAS files that a user can
               have open simultaneously. This includes any files used
               by the DAS system.

               See the header file SpiceDAS.h for the actual value of
               this parameter.

-Exceptions

   1)  If `ncomch' is less than zero, the error SPICE(INVALIDCOUNT) is
       signaled by a routine in the call tree of this routine. No
       file will be created.

   2)  If `ifname' is invalid, an error is signaled by a routine in the
       call tree of this routine.

   3)  If the indicated file cannot be opened, an error is signaled
       by a routine in the call tree of this routine. The new file
       will be deleted.

   4)  If an I/O error occurs while reading or writing the indicated
       file, the error is signaled by a routine in the call tree of
       this routine.

   5)  If any of the `fname' or `ifname' input string pointers is
       null, the error SPICE(NULLPOINTER) is signaled.

   6)  If any of the `fname' or `ifname' input strings has zero
       length, the error SPICE(EMPTYSTRING) is signaled.

-Files

   See the EK Required Reading for a discussion of the EK file
   format.

-Particulars

   This routine operates by side effects: it opens and prepares
   an EK for addition of data.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) This example demonstrates how to open a new EK file, creating
      the two segments described below, without any records.

      The EK file will contain two segments, the first containing
      the DATAORDERS table and the second containing the DATAITEMS
      table.

      The E-kernel DATAORDERS table called consists of the set of
      columns listed below:

         DATAORDERS

            Column Name     Data Type
            -----------     ---------
            ORDER_ID        INTEGER
            CUSTOMER_ID     INTEGER
            LAST_NAME       CHARACTER*(*)
            FIRST_NAME      CHARACTER*(*)
            ORDER_DATE      TIME
            COST            DOUBLE PRECISION

      The columns of the DATAITEMS table are shown below:

         DATAITEMS

            Column Name     Data Type
            -----------     ---------
            ITEM_ID         INTEGER
            ORDER_ID        INTEGER
            ITEM_NAME       CHARACTER*(*)
            DESCRIPTION     CHARACTER*(*)
            PRICE           DOUBLE PRECISION

      Note that it is not necessary to populate the first segment
      with data before starting the second segment, or before
      closing the EK.


      Example code begins here.


      /.
         Program ekopn_ex1
      ./
      #include <string.h>
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Local parameters
         ./
         #define EKNAME       "ekopn_ex1.bdb"
         #define DECLEN       201
         #define NAMLEN       41
         #define NCOLS        6

         /.
         Local variables
         ./
         SpiceChar            cdecls [NCOLS] [DECLEN];
         SpiceChar            cnames [NCOLS] [SPICE_EK_CSTRLN];
         SpiceChar          * ifname;

         SpiceInt             handle;
         SpiceInt             nresvc;
         SpiceInt             segno;

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
         Close the file by a call to ekcls_c.
         ./
         ekcls_c ( handle );

         return ( 0 );
      }


      When this program is executed, no output is presented on
      screen. After run completion, a new EK file exists in the
      output directory.

-Restrictions

   1)  No more than SPICE_DAS_FTSIZE DAS files may be opened simultaneously.
       See the header file SpiceDAS.h for the value of SPICE_DAS_FTSIZE.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)

-Version

   -CSPICE Version 1.0.1, 02-AUG-2021 (JDR)

       Edited the header to comply with NAIF standard. Added complete
       code example.

       Added SPICE_DAS_FTSIZE parameter description and the corresponding
       restriction in -Restrictions.

       Added entries #5 and #6 to -Exceptions section.

   -CSPICE Version 1.0.0, 31-MAR-1998 (NJB)

-Index_Entries

   open new E-kernel
   open new EK

-&
*/

{ /* Begin ekopn_c */


   /*
   Participate in error tracing.
   */
   chkin_c ( "ekopn_c" );

   /*
   Check the file name to make sure the pointer is non-null
   and the string length is non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "ekopn_c", fname );


   /*
   Check the internal file name to make sure the pointer is non-null
   and the string length is non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "ekopn_c", ifname );

   /*
   Call the f2c'd Fortran routine.  Use explicit type casts for every
   type defined by f2c.
   */
   ekopn_ ( ( char     * ) fname,
            ( char     * ) ifname,
            ( integer  * ) &ncomch,
            ( integer  * ) handle,
            ( ftnlen     ) strlen(fname),
            ( ftnlen     ) strlen(ifname) );

   chkout_c ( "ekopn_c" );

} /* End ekopn_c */
