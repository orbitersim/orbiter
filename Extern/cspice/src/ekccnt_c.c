/*

-Procedure ekccnt_c ( EK, column count )

-Abstract

   Return the number of distinct columns in a specified, currently
   loaded table.

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

   void ekccnt_c ( ConstSpiceChar  * table,
                   SpiceInt        * ccount )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   table      I   Name of table.
   ccount     O   Count of distinct, currently loaded columns.

-Detailed_Input

   table       is the name of a currently loaded table. Case
               is not significant in the table name.

-Detailed_Output

   ccount      is the number of distinct columns in `table'.
               Columns that have the same name but belong to
               different segments that are considered to be
               portions of the same column, if the segments
               containing those columns belong to `table'.

-Parameters

   None.

-Exceptions

   1)  If the specified table is not loaded, the error
       SPICE(TABLENOTLOADED) is signaled by a routine in the call
       tree of this routine.

   2)  If the `table' input string pointer is null, the error
       SPICE(NULLPOINTER) is signaled.

   3)  If the `table' input string has zero length, the error
       SPICE(EMPTYSTRING) is signaled.

-Files

   This routine reads binary "sequence component" EK files.
   In order for a binary EK file to be accessible to this routine,
   the file must be loaded via a call to furnsh_c or the low-level
   EK loader eklef_c.

-Particulars

   This routine is a utility intended for use in conjunction with
   the function ekcii_c. These routines can be used to find the
   names and attributes of the columns that are currently loaded.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Examine an EK. Dump the names and attributes of the columns in
      each loaded table. ekccnt_c is used to obtain column counts.


      Example code begins here.


      /.
         Program ekccnt_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"


      int main( )
      {
         /.
         Local constants.
         ./
         #define FILEN           256

         /.
         Local variables.
         ./
         SpiceChar               colnam  [ SPICE_EK_CSTRLN ];
         SpiceChar               ek      [ FILEN ];
         SpiceChar               tabnam  [ SPICE_EK_TSTRLN ];

         SpiceChar             * typstrs [ 4 ] =
                                 {
                                    "CHR", "DP", "INT", "TIME"
                                 };

         SpiceEKAttDsc           attdsc;

         SpiceInt                i;
         SpiceInt                ncols;
         SpiceInt                ntab;
         SpiceInt                tab;


         prompt_c ( "Enter name of EK to examine > ", FILEN, ek );

         furnsh_c ( ek );

         /.
         Get the number of loaded tables.
         ./
         ekntab_c ( &ntab );

         printf ( "Number of tables in EK: %2i\n", ntab );

         for ( tab = 0;  tab < ntab;  tab++ )
         {
            /.
            Get the name of the current table, and look up
            the column count for this table.
            ./
            ektnam_c ( tab, SPICE_EK_TSTRLN, tabnam );

            ekccnt_c ( tabnam, &ncols );

            printf ( "------------------------------"
                     "------------------------------\n");
            printf ( "Table = %s\n\n", tabnam );


            /.
            For each column in the current table, look up the
            column's attributes.  The attribute block
            index parameters are defined in the include file
            ekattdsc.inc.
            ./

            for ( i = 0;  i < ncols;  i++ )
            {
               ekcii_c ( tabnam, i, SPICE_EK_CSTRLN, colnam, &attdsc );

               printf ( "Column = %s\n", colnam );


               /.
               Write out the current column's data type.
               ./

               printf ( "   Type = %s\n", typstrs[(int)attdsc.dtype] );

               if ( attdsc.dtype == SPICE_CHR )
               {
                  if ( attdsc.strlen == SPICE_EK_VARSIZ )
                  {
                     printf ( "   String length = VARIABLE\n" );
                  }
                  else
                  {
                      printf ( "   String length = %d\n",
                              (int) attdsc.strlen );
                  }
               }

               /.
               Write out the current column's entry size.
               ./
               printf ( "   Size = %d\n", (int)attdsc.size );


               /.
               Indicate whether the current column is indexed.
               ./
               if ( attdsc.indexd == SPICETRUE )
               {
                  printf ( "   Indexed.\n" );
               }
               else
               {
                  printf ( "   Not indexed.\n" );
               }

               /.
               Indicate whether the current column allows
               null values.
               ./
               if ( attdsc.nullok == SPICETRUE )
               {
                  printf ( "   Null values allowed.\n" );
               }
               else
               {
                  printf ( "   Null values not allowed.\n" );
               }
            }
            /.
            We're done with the current column.
            ./
         }
         /.
         We're done with the current table.
         ./

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, using the EK S79_CIMSSSUPa.bep (the Cassini Science
      Plan SPICE E-Kernel File based upon integrated science plan)
      as input, the output was:


      Enter name of EK to examine > S79_CIMSSSUPa.bep
      Number of tables in EK:  3
      ------------------------------------------------------------
      Table = CASSINI_SP_REQUEST

      Column = SUBSYSTEM
         Type = CHR
         String length = 32
         Size = 1
         Indexed.
         Null values not allowed.
      Column = REQUEST_ID
         Type = CHR
         String length = VARIABLE
         Size = 1
         Indexed.
         Null values not allowed.
      Column = REQUEST_TITLE
         Type = CHR
         String length = VARIABLE
         Size = 1
         Indexed.
         Null values allowed.
      Column = BEGIN_TIME
         Type = TIME
         Size = 1
         Indexed.
         Null values not allowed.
      Column = END_TIME
         Type = TIME
         Size = 1
         Indexed.
         Null values not allowed.
      Column = SEQUENCE
         Type = CHR
         String length = 32
         Size = 1
         Indexed.
         Null values not allowed.
      Column = POINTING_AGREEMENT
         Type = CHR
         String length = 80
         Size = -1
         Not indexed.
         Null values allowed.
      Column = PRIMARY_POINTING
         Type = CHR
         String length = VARIABLE
         Size = 1
         Indexed.
         Null values allowed.
      Column = SECONDARY_POINTING
         Type = CHR
         String length = VARIABLE
         Size = 1
         Indexed.
         Null values allowed.
      Column = REQ_DESCRIPTION
         Type = CHR
         String length = 80
         Size = -1
         Not indexed.
         Null values allowed.
      ------------------------------------------------------------
      Table = CASSINI_SP_OBSERVATION

      Column = SUBSYSTEM
         Type = CHR
         String length = 32
         Size = 1
         Indexed.
         Null values not allowed.
      Column = OBSERVATION_ID
         Type = CHR
         String length = VARIABLE
         Size = 1
         Indexed.
         Null values not allowed.
      Column = OBSERVATION_TITLE
         Type = CHR
         String length = VARIABLE
         Size = 1
         Indexed.
         Null values not allowed.
      Column = SEQUENCE
         Type = CHR
         String length = 32
         Size = 1
         Indexed.
         Null values not allowed.
      Column = SCIENCE_OBJECTIVE
         Type = CHR
         String length = 80
         Size = -1
         Not indexed.
         Null values allowed.
      Column = OBS_DESCRIPTION
         Type = CHR
         String length = 80
         Size = -1

      [...]


      Warning: incomplete output. Only 100 out of 129 lines have been
      provided.


-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   E.D. Wright         (JPL)

-Version

   -CSPICE Version 1.0.2, 02-AUG-2021 (JDR)

       Edited the header to comply with NAIF standard. Added complete
       code example based on existing code fragment.

   -CSPICE Version 1.0.1, 12-JUL-2016 (EDW)

       Edit to example program to use "%d" with explicit casts
       to int for printing SpiceInts with printf.

   -CSPICE Version 1.0.0, 14-OCT-2001 (NJB)

-Index_Entries

   return the number of loaded EK columns
   return the count of loaded EK columns

-&
*/

{ /* Begin ekccnt_c */


   /*
   Participate in error tracing.
   */
   chkin_c ( "ekccnt_c" );


   /*
   Check the input string to make sure the pointer
   is non-null and the string length is non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "ekccnt_c", table );

   ekccnt_ (  ( char    * ) table,
              ( integer * ) ccount,
              ( ftnlen    ) strlen(table)  );


   chkout_c ( "ekccnt_c" );

} /* End ekccnt_c */
