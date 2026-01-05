/*

-Procedure ekops_c ( EK, open scratch file )

-Abstract

   Open a scratch (temporary) E-kernel file and prepare the file
   for writing.

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
   #include "SpiceZst.h"

   void ekops_c ( SpiceInt   * handle )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   handle     O   File handle attached to new EK file.

-Detailed_Input

   None.

-Detailed_Output

   handle      is the EK file handle of the file opened by this
               routine. This handle is used to identify the file
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
       by a routine in the call tree of this routine. The new file
       will be deleted.

   2)  If an I/O error occurs while reading or writing the indicated
       file, the error is signaled by a routine in the call tree of
       this routine.

-Files

   This routine creates a temporary EK file; the file is deleted
   when the calling program terminates or when the file is closed
   using the CSPICE routine ekcls_c.

   See the EK Required Reading for a discussion of the EK file
   format.

-Particulars

   This routine operates by side effects: it opens and prepares
   an EK for addition of data. "Scratch" files are automatically
   deleted when the calling program terminates normally or when
   closed using the CSPICE routine ekcls_c.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Suppose we want to create an E-kernel which contains a table
      of items that have been ordered but we do not want to keep
      the file. The columns of this table are shown below:

         DATAITEMS

            Column Name     Data Type
            -----------     ---------
            ITEM_ID         INTEGER
            ORDER_ID        INTEGER
            ITEM_NAME       CHARACTER*(*)
            DESCRIPTION     CHARACTER*(*)
            PRICE           DOUBLE PRECISION


      This examples demonstrates how to open a scratch EK file;
      create the segment described above, how to insert a new record
      into it, and how to summarize its contents.


      Example code begins here.


      /.
         Program ekops_ex1
      ./
      #include <stdio.h>
      #include <string.h>
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Local parameters
         ./
         #define TABLE        "DATAITEMS"
         #define DECLEN       201
         #define DESCLN       81

         /.
         One value per row/column element.
         ./
         #define MAXVAL       1
         #define NAMLEN       41
         #define COLSLN       5

         static SpiceChar     EKDTYP [4][5] = { "CHR", "DP", "INT", "TIME" };

         /.
         Local variables
         ./
         SpiceChar            cdecls [SPICE_EK_MXCLSG][DECLEN];
         SpiceChar            cnames [SPICE_EK_MXCLSG][SPICE_EK_CSTRLN];
         SpiceChar            cvals  [MAXVAL][NAMLEN];
         SpiceChar            descrp [DESCLN];
         SpiceChar            itemnm [NAMLEN];

         SpiceDouble          dvals  [MAXVAL];
         SpiceDouble          price;


         SpiceEKSegSum        segsum;

         SpiceInt             esize;
         SpiceInt             handle;
         SpiceInt             i;
         SpiceInt             itemid;
         SpiceInt             ivals  [MAXVAL];
         SpiceInt             nseg;
         SpiceInt             nvals;
         SpiceInt             ordid;
         SpiceInt             recno;
         SpiceInt             segno;

         SpiceBoolean         isnull;

         /.
         Open a scratch EK file to use for temporary
         storage.
         ./
         ekops_c ( &handle );

         /.
         Set up the table and column names and declarations
         for the DATAITEMS segment.  We'll index all of
         the columns.  All columns are scalar, so we omit
         the size declaration.
         ./
         strncpy( cnames[0], "ITEM_ID", 8 );
         strncpy( cdecls[0], "DATATYPE = INTEGER, INDEXED = TRUE", 35 );

         strncpy( cnames[1], "ORDER_ID", 9 );
         strncpy( cdecls[1], "DATATYPE = INTEGER, INDEXED = TRUE", 35 );

         strncpy( cnames[2], "ITEM_NAME", 10 );
         strncpy( cdecls[2], "DATATYPE = CHARACTER*(*),INDEXED  = TRUE", 41 );

         strncpy( cnames[3], "DESCRIPTION", 12 );
         strncpy( cdecls[3], "DATATYPE = CHARACTER*(*),INDEXED  = TRUE", 41 );

         strncpy( cnames[4], "PRICE", 6 );
         strncpy( cdecls[4], "DATATYPE = DOUBLE PRECISION,INDEXED  = TRUE",
                  44 );

         /.
         Start the segment. Since we have no data for this
         segment, start the segment by just defining the new
         segment's schema.
         ./
         ekbseg_c ( handle, TABLE,  COLSLN, SPICE_EK_CSTRLN,
                    cnames, DECLEN, cdecls, &segno );

         /.
         Append a new, empty record to the DATAITEMS
         table. Recall that the DATAITEMS table
         is the first segment.  The call will return
         the number of the new, empty record.
         ./
         segno = 0;
         ekappr_c ( handle, segno, &recno );

         /.
         At this point, the new record is empty. We fill in the
         data here.  Data items are filled in one column at a
         time. The order in which the columns are filled in is
         not important.  We use the different add column entry
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
         strncpy( itemnm, "Sample item", 12 );
         strncpy( descrp, "This sample item is used only in tests.", 40 );
         price    =   1345.67;

         /.
         Note that the names of the routines called
         correspond to the data types of the columns.
         ./
         ekacei_c ( handle, segno, recno, "ORDER_ID", esize, &ordid, isnull );

         ekacei_c ( handle, segno, recno, "ITEM_ID", esize, &itemid, isnull );

         ekacec_c ( handle, segno,  recno,  "ITEM_NAME",
                    esize,  NAMLEN, itemnm, isnull );

         ekacec_c ( handle, segno,  recno,  "DESCRIPTION",
                    esize,  DESCLN, descrp, isnull );

         ekaced_c ( handle, segno, recno, "price", esize, &price, isnull );

         /.
         At this point, we could perform read operations
         on the EK.

         Return the number of segments in the EK. Dump the
         desired summary information for each one.
         ./
         nseg = eknseg_c ( handle );
         printf( "Number of segments = %2d\n", (int)nseg );
         printf( "\n" );

         for ( segno = 0; segno < nseg; segno++ )
         {

            ekssum_c ( handle, segno, &segsum );

            printf( "Table containing segment: %s\n", segsum.tabnam );
            printf( "Number of rows          : %2d\n", (int)segsum.nrows  );
            printf( "Number of columns       : %2d\n", (int)segsum.ncols  );
            printf( "Table data              :\n" );

            for ( i = 0; i < segsum.ncols; i++ )
            {
               printf( "  Column: %s\n", segsum.cnames[i] );
               printf( "  Type  : %s\n", EKDTYP[ segsum.cdescrs[i].dtype ] );

               for ( recno = 0; recno < segsum.nrows; recno++ )
               {
                  if ( segsum.cdescrs[i].dtype == SPICE_CHR )
                  {
                     ekrcec_c ( handle,  segno, recno,  segsum.cnames[i],
                                NAMLEN, &nvals, cvals, &isnull           );

                     if ( isnull )
                     {
                        printf( "  Data  : <null>\n" );
                     }
                     else
                     {
                        printf( "  Data  : %s\n", cvals[0] );
                     }
                  }
                  else if ( segsum.cdescrs[i].dtype == SPICE_DP )
                  {
                     ekrced_c (  handle,  segno,  recno,  segsum.cnames[i],
                                &nvals,   dvals, &isnull                   );

                     if ( isnull )
                     {
                        printf( "  Data  : <null>\n" );
                     }
                     else
                     {
                        printf( "  Data  : %9.2f\n", dvals[0] );
                     }
                  }
                  else if ( segsum.cdescrs[i].dtype == SPICE_INT )
                  {
                     ekrcei_c (  handle,  segno,  recno,  segsum.cnames[i],
                                &nvals,   ivals, &isnull                   );

                     if ( isnull )
                     {
                        printf( "  Data  : <null>\n" );
                     }
                     else
                     {
                        printf( "  Data  : %6d\n", ivals[0] );
                     }
                  }

                  /.
                  There is no time data. Otherwise, we would need
                  to use an LSK and ekrced_c to read it
                  (internally, it is stored as double precision).
                  ./
                  printf( "\n" );
               }
            }

            printf( "----------------------------------------\n" );
         }

         /.
         Close the file. This will delete the scratch file
         and all the data will be lost.
         ./
         ekcls_c ( handle );

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Number of segments =  1

      Table containing segment: DATAITEMS
      Number of rows          :  1
      Number of columns       :  5
      Table data              :
        Column: ITEM_ID
        Type  : INT
        Data  :    531

        Column: ORDER_ID
        Type  : INT
        Data  :  10011

        Column: ITEM_NAME
        Type  : CHR
        Data  : Sample item

        Column: DESCRIPTION
        Type  : CHR
        Data  : This sample item is used only in tests.

        Column: PRICE
        Type  : DP
        Data  :   1345.67

      ----------------------------------------


      Note that after run completion, there is no EK file in the
      output directory as scratch files are deleted when they are
      closed or when the calling program terminates.

-Restrictions

   1)  No more than SPICE_DAS_FTSIZE DAS files may be opened simultaneously.
       See the header file SpiceDAS.h for the value of SPICE_DAS_FTSIZE.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)

-Version

   -CSPICE Version 1.0.1, 31-JUL-2020 (JDR)

       Edited the header to comply with NAIF standard and improved
       the API documentation. Added complete code example.

       Added SPICE_DAS_FTSIZE parameter description.

   -CSPICE Version 1.0.0, 02-APR-1998 (NJB)

-Index_Entries

   open scratch E-kernel
   open scratch EK

-&
*/

{ /* Begin ekops_c */


   /*
   Participate in error tracing.
   */
   chkin_c ( "ekops_c" );


   ekops_ ( ( integer * ) handle );


   chkout_c ( "ekops_c" );

} /* End ekops_c */
