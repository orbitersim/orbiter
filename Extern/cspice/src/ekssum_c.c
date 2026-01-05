/*

-Procedure ekssum_c ( EK, return segment summary )

-Abstract

   Return summary information for a specified segment in a
   specified EK.

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
   #include "SpiceZmc.h"

   void ekssum_c ( SpiceInt           handle,
                   SpiceInt           segno,
                   SpiceEKSegSum    * segsum )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   handle     I   Handle of EK.
   segno      I   Number of segment to be summarized.
   segsum     O   EK segment summary.

-Detailed_Input

   handle      is an EK file handle specifying the EK containing
               the segment to be summarized.

   segno       is the number of the segment whose summary is
               desired. Segments are numbered from 0 to nseg-1,
               where nseg is the count of segments in the file.

-Detailed_Output

   segsum      is a pointer to an EK segment summary. The summary is
               of type EKSegSum. The structure contains the
               following members:

                  tabnam      The name of the table to which the
                              segment belongs.

                  nrows       The number of rows in the segment.

                  ncols       The number of columns in the segment.

                  cnames      An array of names of columns in the
                              segment. Column names may contain
                              as many as SPICE_EK_CNAMSZ characters.
                              The array contains room for
                              SPICE_EK_MXCLSG column names.

                  cdescrs     An array of column attribute
                              descriptors of type SpiceEKAttDsc.
                              The array contains room for
                              SPICE_EK_MXCLSG descriptors. The Ith
                              descriptor corresponds to the column
                              whose name is the Ith element of the
                              array cnames.


               The column attribute descriptors have the following
               members:

                  cclass: Column class code.

                  dtype: Data type code: has type
                              SpiceEKDataType.

                  strlen: String length. Applies to SPICE_CHR
                              type. Value is SPICE_EK_VARSIZ for
                              variable-length strings.

                  size: Column entry size; this is the number
                              of array elements in a column entry.
                              The value is SPICE_EK_VARSIZ for
                              variable-size columns.

                  indexd: Index flag; value is SPICETRUE if the
                              column is indexed, SPICEFALSE
                              otherwise.

                  nullok: Null flag; value is SPICETRUE if the
                              column may contain null values,
                              SPICEFALSE otherwise.

-Parameters

   See the -Restrictions section.

-Exceptions

   1)  If `handle' is invalid, an error is signaled by a routine in the
       call tree of this routine. The output arguments will not be
       modified.

   2)  If `segno' is not the index of an existing segment in the
       specified file, the error SPICE(INDEXOUTOFRANGE) is signaled by
       a routine in the call tree of this routine. The output arguments
       will not be modified.

   3)  If an I/O error occurs while attempting to obtain summary
       information for the specified segment, the error is signaled
       by a routine in the call tree of this routine. The output
       arguments may be modified in this case.

-Files

   This routine provides summary information for segments belonging
   to a binary EK file.

-Particulars

   This routine supports the function of summarizing a binary
   EK file, allowing NAIF Toolkit users to determine whether it
   contains data of interest.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Dump the attributes of the segments in a specified EK.


      Example code begins here.


      /.
         ekssum_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main()
      {
         /.
         Constants
         ./
         #define FILSIZ          256

         SpiceChar               ek       [FILSIZ];
         static SpiceChar        chrTypes [4][5] = { "CHR",
                                                     "DP",
                                                     "INT",
                                                     "TIME" };
         SpiceEKSegSum           segsum;

         SpiceInt                handle;
         SpiceInt                i;
         SpiceInt                nseg;
         SpiceInt                segno;



         prompt_c ( "Enter name of EK file > ", FILSIZ, ek );

         /.
         Open the EK for read access and get the number of
         segments it contains.
         ./
         ekopr_c ( ek, &handle );

         nseg = eknseg_c ( handle );

         /.
         Loop through the segments, dumping the desired
         summary information for each one.
         ./
         printf ( "\n"
                  "Segment summary for file %s\n\n",
                  ek                                );

         for ( segno = 0;  segno < nseg;  segno++ )
         {

            ekssum_c (  handle,  segno,  &segsum );


            printf ( "=============================="
                     "=============================="
                     "\n"
                     "Table containing segment:  %s\n"
                     "\n"
                     "Number of rows:     %d\n"
                     "Number of columns:  %d\n"
                     "\n"
                     "Column names and attributes: \n"
                     "\n",
                     segsum.tabnam,
                     segsum.nrows,
                     segsum.ncols                            );


            for ( i = 0;  i < segsum.ncols;  i++ )
            {

               printf ( "\n"
                        "Column:   %s\n"
                        "\n"
                        "Data type:      %s\n",
                        segsum.cnames[i],
                        chrTypes[ segsum.cdescrs[i].dtype ]     );


               if ( segsum.cdescrs[i].size >= 0 )
               {
                  printf ( "Dimension:      %d\n",
                            segsum.cdescrs[i].size );
               }
               else
               {
                  printf ( "Dimension:      Variable\n" );
               }


               if ( segsum.cdescrs[i].dtype == SPICE_CHR )
               {
                  if ( segsum.cdescrs[i].strlen >= 0 )
                  {
                     printf ( "String length:  %d\n",
                               segsum.cdescrs[i].strlen         );
                  }
                  else
                  {
                     printf ( "String length:  Variable\n" );
                  }
               }


               if ( segsum.cdescrs[i].indexd )
               {
                   printf ( "Indexed\n" );
               }


               if ( segsum.cdescrs[i].nullok )
               {
                   printf ( "Nulls allowed\n" );
               }

               printf ( "\n" );

            }

            printf ( "\n"
                     "=============================="
                     "=============================="
                     "\n"                            );
         }

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, using the EK file named dif_cmdict_128_20050620.bdb
      to load the Deep Impact spacecraft subsystem commands dictionary,
      the output was:


      Enter name of EK file > dif_cmdict_128_20050620.bdb

      Segment summary for file dif_cmdict_128_20050620.bdb

      ============================================================
      Table containing segment:  DIF_COMMANDS

      Number of rows:     5798
      Number of columns:  7

      Column names and attributes:


      Column:   SUBSYSTEM

      Data type:      CHR
      Dimension:      1
      String length:  32
      Indexed


      Column:   COMMAND

      Data type:      CHR
      Dimension:      1
      String length:  32
      Indexed


      Column:   PARAMETER_NAME

      Data type:      CHR
      Dimension:      1
      String length:  32
      Indexed


      Column:   PARAMETER_INDEX

      Data type:      INT
      Dimension:      1
      Indexed


      Column:   PARAMETER_TYPE

      Data type:      CHR
      Dimension:      1
      String length:  32
      Nulls allowed


      Column:   PARAMETER_RANGE

      Data type:      CHR
      Dimension:      Variable
      String length:  80
      Nulls allowed


      Column:   DESCRIPTION

      Data type:      CHR
      Dimension:      Variable
      String length:  80


      ============================================================


-Restrictions

   1)  Many parameters used internally in this routine are from the
       Fortran SPICELIB include files ekcoldsc.inc and eksegdsc.inc.
       The parameters used in this routine must be kept in sync with
       those used in SPICELIB.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)

-Version

   -CSPICE Version 1.1.1, 10-AUG-2021 (JDR)

       Edited the header to comply with NAIF standard.

       Added the solution to the example when using a Deep Impact EK kernel.
       Reformatted example's output.

   -CSPICE Version 1.1.0, 12-JUL-1999 (NJB)

       Now calls zzeksinf_ instead of ekssum_ to get summary
       information. This enables retrieval of column classes and
       simplifies the code as well.

       Changed arrays of SpiceBoolean flags passed to ekssum_ to
       data type logical. Changed name of "class" member of structure
       SpiceEKSegSum to "cclass." The name "class" is a C++ keyword
       and prevented clean integration into C++ code.

   -CSPICE Version 1.0.0, 17-FEB-1999 (NJB)

-Index_Entries

   return EK segment summary

-&
*/

{ /* Begin ekssum_c */

   /*
   Local constants
   */
   #define   NTYPES        4
   #define   CTYPELEN      5
   #define   CDSCSZ        11
   #define   SDSCSZ        24
   #define   NCIDX         4
   #define   NRIDX         ( NCIDX  + 1 )
   #define   CLSIDX        0
   #define   TYPIDX        ( CLSIDX + 1 )
   #define   LENIDX        ( TYPIDX + 1 )
   #define   SIZIDX        ( LENIDX + 1 )
   #define   IXTIDX        5
   #define   NULIDX        ( IXTIDX + 2 )


   /*
   Local variables
   */
   SpiceInt                cdescrs [SPICE_EK_MXCLSG][CDSCSZ];
   SpiceInt                segdsc  [SDSCSZ];
   SpiceInt                i;



   /*
   Participate in error tracing.
   */
   chkin_c ( "ekssum_c" );


   /*
   Convert the segment number to a Fortran-style index.
   */
   segno ++;


   /*
   Call the f2c'd Fortran routine.  Use explicit type casts for every
   type defined by f2c.

   We have a special case here:  the Fortran routine has an output
   string array for the column data types.  Since the corresponding
   member of the segment summary uses an array of the enumerated type
   SpiceEKDataType, we must capture the output array, convert it to
   a C array, and map the elements to values of this type.
   */

   zzeksinf_ (  ( integer   * ) &handle,
                ( integer   * ) &segno,
                ( char      * ) segsum->tabnam,
                ( integer   * ) segdsc,
                ( char      * ) segsum->cnames,
                ( integer   * ) cdescrs,
                ( ftnlen      ) SPICE_EK_TNAMSZ,
                ( ftnlen      ) SPICE_EK_CNAMSZ  );


   if ( failed_c() )
   {
      chkout_c ( "ekssum_c" );
      return;
   }

   /*
   Fill in the segment summary values that are not contained in column
   descriptors.  These are:

     - table name
     - number of rows
     - number of columns
     - array of column names

   */

   F2C_ConvertStr ( SPICE_EK_TSTRLN, segsum->tabnam );

   segsum->nrows   =   segdsc[NRIDX];
   segsum->ncols   =   segdsc[NCIDX];

   F2C_ConvertTrStrArr (  segsum -> ncols,
                          SPICE_EK_CSTRLN,
                          ( SpiceChar * ) (segsum->cnames) );


   /*
   Fill the column attribute descriptors.
   */
   for ( i = 0;  i < segsum->ncols;  i++ )
   {
      segsum -> cdescrs[i].cclass  =  cdescrs[i][CLSIDX];
      segsum -> cdescrs[i].size    =  cdescrs[i][SIZIDX];
      segsum -> cdescrs[i].strlen  =  cdescrs[i][LENIDX];
      segsum -> cdescrs[i].indexd  =  cdescrs[i][IXTIDX] >= 0;
      segsum -> cdescrs[i].nullok  =  cdescrs[i][NULIDX] >= 0;
   }


   /*
   Convert the Fortran-style string tabnam to a C-style string.
   */
   F2C_ConvertStr ( SPICE_EK_TSTRLN,  segsum->tabnam );


   /*
   Convert the Fortran-style string array cnames to a C-style string
   array.
   */


   /*
   Assign the segsum->cdescrs member dtype using the data type codes
   from the cdescrs array.
   */
   for ( i = 0;  i < segsum->ncols;  i++ )
   {
      segsum->cdescrs[i].dtype  =  (SpiceEKDataType)
                                   ( cdescrs[i][TYPIDX] - 1 );
   }


   chkout_c ( "ekssum_c" );

} /* End ekssum_c */
