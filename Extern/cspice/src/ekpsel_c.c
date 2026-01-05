/*

-Procedure ekpsel_c ( EK, parse SELECT clause )

-Abstract

   Parse the SELECT clause of an EK query, returning full particulars
   concerning each selected item.

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

   None.

-Keywords

   PRIVATE

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZst.h"
   #include "SpiceZmc.h"


   void ekpsel_c ( ConstSpiceChar        * query,
                   SpiceInt                msglen,
                   SpiceInt                tablen,
                   SpiceInt                collen,
                   SpiceInt              * n,
                   SpiceInt              * xbegs,
                   SpiceInt              * xends,
                   SpiceEKDataType       * xtypes,
                   SpiceEKExprClass      * xclass,
                   void                  * tabs,
                   void                  * cols,
                   SpiceBoolean          * error,
                   SpiceChar             * errmsg  )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   query      I   EK query.
   msglen     I   Available space in the output error message string.
   tablen     I   Length of strings in `tabs' output array.
   collen     I   Length of strings in `cols' output array.
   n          O   Number of items in SELECT clause of `query'.
   xbegs      O   Begin positions of expressions in SELECT clause.
   xends      O   End positions of expressions in SELECT clause.
   xtypes     O   Data types of expressions.
   xclass     O   Classes of expressions.
   tabs       O   Names of tables qualifying SELECT columns.
   cols       O   Names of columns in SELECT clause of `query'.
   error      O   Error flag.
   errmsg     O   Parse error message.

-Detailed_Input

   query       is a character string containing an EK query.
               EK queries have the general form

                  SELECT <select expr>, <select expr>, ...
                  FROM <table spec>, <table spec>, ...
                  [WHERE <constraint list>]
                  [ORDER BY <order-by column list>]

               Here the symbol <select expr> indicates any
               expression representing an entity that can be
               selected. Commonly, the selected items are
               columns, with or without qualifying table names,
               having the form

                  <column name>
                  <table name>.<column name>
                  <table alias>.<column name>

               but more general expressions may also be selected.
               Examples are functions, such as

                  COUNT(*)
                  COUNT( <table name>.<column name> )
                  MAX  ( <table name>.<column name> )

               or expressions involving constants, such as

                  2 * <column name>


   msglen      is the allowed length for the output message string.
               This length must large enough to hold the output
               string plus the terminator. If the output string is
               expected to have x characters, `msglen' needs to be
               x + 1.

   tablen      is the length of the strings in the output table array.
               This length must large enough to hold the output
               strings plus the terminator. If the output strings
               are expected to have x characters, `tablen' needs to be
               x + 1. The parameter SPICE_EK_TSTRLN defines a string
               length sufficient to hold any table name. This
               parameter is defined by SpiceEK.h.

   collen      The length of the strings in the output column array.
               This length must large enough to hold the output
               strings plus the terminator. If the output strings
               are expected to have x characters, `collen' needs to be
               x + 1. The parameter SPICE_EK_CSTRLN defines a string
               length sufficient to hold any table name. This
               parameter is defined by SpiceEK.h.

-Detailed_Output

   n           is the number of items specified in the
               SELECT clause of the input query.

   xbegs,
   xends       are, respectively, arrays of begin and end
               positions of expressions designating items in the
               SELECT clause of the input query. The ith
               expression is located in the substring

                  query[ xbegs[i] ]...query[ xends[i] ]

   xtypes      is an array of values of type SpiceEKDataType giving
               types of the expressions in the SELECT clause.
               Values and meanings of `xtypes' are:

                  SPICE_CHR     Character type
                  SPICE_DP      Double precision type
                  SPICE_INT     Integer type
                  SPICE_TIME    Time type

               The ith element of `xtypes' refers to the ith
               selected item.

               The data type of an expression indicates which
               fetch routine to use to obtain values of the
               selected expression. The mapping of data types
               to fetch routines is shown below:

                  SPICE_CHR      ekgc_c
                  SPICE_DP       ekgd_c
                  SPICE_INT      ekgi_c
                  SPICE_TIME     ekgd_c

               Note that time values are stored as d.p. numbers.

   xclass      is an array of values of type SpiceEKExprClass giving
               the classes of the expressions occurring in the SELECT
               clause of the input query. Values and meanings of
               `xclass' are:

                  SPICE_EK_EXP_COL     Selected item was a column.
                                       The column may qualified by a
                                       table name.

                  SPICE_EK_EXP_FUNC    Selected item was a simple
                                       function invocation of the
                                       form

                                          F ( <column> )

                                       or else was

                                          COUNT(*)

                  SPICE_EK_EXP_EXPR    Selected item was a more
                                       general expression than those
                                       shown above.

               The ith element of `xclass' refers to the ith
               selected item.

               When a selected item is a column, the values of
               the arguments `tabs' and `cols' (discussed below) are
               defined.

   tabs        is an array of names of tables corresponding to
               the columns in the SELECT clause. The ith element
               of `tabs' corresponds to the table containing the
               ith SELECT column. Table names returned in `tabs'
               are the actual names of tables in loaded EKs, not
               aliases supplied in the input query. Table names
               are supplied even if the corresponding column was
               unqualified in the input query, as long as the
               column name was unambiguous.

               The contents of tabs[i] are defined if and only if
               the returned value of xclass[i] is SPICE_EK_EXP_COL.

               The caller should declare `tabs' as an array of strings
               of length

                  SPICE_EK_TSTRLN

               for example

                  SpiceChar     tabs[SPICE_EK_MAXQSEL][SPICE_EK_TSTRLN];

   cols        is an array containing the columns of the SELECT
               clause. The contents of cols[i] are defined if and
               only if the returned value of xclass[i] is
               SPICE_EK_EXP_COL.

               The caller should declare `cols' as an array of strings
               of length

                  SPICE_EK_CSTRLN

               for example

                  SpiceChar     tabs[SPICE_EK_MAXQSEL][SPICE_EK_CSTRLN];

   error       is a logical flag indicating whether the input
               `query' parsed correctly. The other outputs of this
               routine, except for `errmsg', are undefined if a
               parse error occurred. `error' is returned SPICETRUE if
               a parse error occurred, SPICEFALSE otherwise.

   errmsg      is a character string describing the cause of a
               parse error, if such an error occurred. Otherwise,
               `errmsg' is returned empty.

-Parameters

   None.

-Exceptions

   1)  Parse failures do not cause this routine to signal errors;
       instead, the `error' and `errmsg' outputs indicate invalid
       `query'.

   2)  Queries cannot be parsed correctly unless at least one EK
       is loaded.

   3)  If the `query' input string pointer is null, the error
       SPICE(NULLPOINTER) is signaled.

   4)  If the `query' input string has zero length, the error
       SPICE(EMPTYSTRING) is signaled.

   5)  If the `errmsg' output string pointer is null, the error
       SPICE(NULLPOINTER) is signaled.

   6)  If the `errmsg' output string has length less than two
       characters, the error SPICE(STRINGTOOSHORT) is signaled, since
       the output string is too short to contain one character of
       output data plus a null terminator.

-Files

   None.

-Particulars

   This routine allows callers of the EK fetch routines to determine
   at run time the attributes of the columns from which data is to be
   fetched.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Query the EK system and fetch data matching that query.

      The program shown here does not rely on advance
      knowledge of the input query or the contents of any loaded EK
      files.

      To simplify the example, we assume that all data are scalar.
      This assumption relieves us of the need to test the size of
      column entries before fetching them. In the event that a
      column contains variable-size array entries, the entry point
      eknelt_c may be called to obtain the size of column entries to
      be fetched. See eknelt_c for an example.


      Use the EK kernel below to load the information from the
      original Supplementary Engineering Data Record (SEDR) data
      set generated by the Viking Project.

         vo_sedr.bdb

      Use the LSK kernel below to load the leap seconds and time
      constants required for the conversions.

         naif0012.tls


      Example code begins here.


      /.
         Program ekpsel_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Local parameters
         ./
         #define EKNAME       "vo_sedr.bdb"
         #define LSKNAM       "naif0012.tls"
         #define ERRLEN       1840
         #define TIMLEN       27
         #define TYPLEN       4
         #define XCLSLN       4

         /.
         Local variables
         ./
         SpiceChar            cdata  [SPICE_EK_MAXQSTR];
         SpiceChar            cols   [SPICE_EK_MAXQSEL][SPICE_EK_MAXQRY];
         SpiceChar            errmsg [ERRLEN];
         SpiceChar          * query;
         SpiceChar            utcstr [TIMLEN];
         SpiceChar            tabs   [SPICE_EK_MAXQTAB][SPICE_EK_MAXQRY];
         SpiceEKExprClass     xclass [SPICE_EK_MAXQSEL];
         SpiceEKDataType      xtypes [SPICE_EK_MAXQSEL];

         SpiceDouble          ddata;
         SpiceDouble          tdata;

         SpiceInt             b;
         SpiceInt             colno;
         SpiceInt             e;
         SpiceInt             handle;
         SpiceInt             idata;
         SpiceInt             n;
         SpiceInt             nmrows;
         SpiceInt             row;
         SpiceInt             xbegs  [SPICE_EK_MAXQSEL];
         SpiceInt             xends  [SPICE_EK_MAXQSEL];

         SpiceBoolean         error;
         SpiceBoolean         found;
         SpiceBoolean         null;

         /.
         Load leapseconds file for time conversion.
         ./
         furnsh_c ( LSKNAM );

         /.
         Load EK.
         ./
         eklef_c ( EKNAME, &handle );

         /.
         Setup the query.  Parse the SELECT clause using
         ekpsel_c.
         ./
         query = "Select IMAGE_NUMBER, IMAGE_ID, PLATFORM_CLOCK, IMAGE_TIME "
                 "from VIKING_SEDR_DATA where IMAGE_NUMBER < 25850000 "
                 "order by IMAGE_NUMBER";

         ekpsel_c ( query,  ERRLEN, SPICE_EK_MAXQRY, SPICE_EK_MAXQRY,
                    &n,     xbegs,  xends,           xtypes,
                    xclass, tabs,   cols,           &error,          errmsg );

         if ( error )
         {
            printf( "%s\n", errmsg );
         }
         else
         {

            /.
            Submit query to the EK query system.
            ./
            ekfind_c ( query, ERRLEN, &nmrows, &error, errmsg );

            if ( error )
            {
               printf( "%s\n", errmsg );
            }
            else
            {

               /.
               Fetch the rows that matched the query.
               ./
               for ( row = 0; row < nmrows; row++ )
               {

                  /.
                  Fetch data from the ith row.
                  ./
                  printf( " \n" );
                  printf( "ROW =  %d\n", (int)row );

                  for ( colno = 0; colno < n; colno++ )
                  {

                     /.
                     Fetch the data from the jth selected
                     column.
                     ./
                     if ( xclass[colno] == SPICE_EK_EXP_COL )
                     {
                        printf( "  %s.%-14s: ", tabs[colno], cols[colno] );
                     }
                     else
                     {
                        b  =  xbegs[colno];
                        e  =  xends[colno];
                        printf( "ITEM =  (%.*s)", e-b, query+b );
                     }

                     if ( xtypes[colno] == SPICE_CHR )
                     {
                        ekgc_c ( colno,  row,   1, SPICE_EK_MAXQSTR,
                                 cdata, &null, &found               );

                        if ( null )
                        {
                           printf( "<Null>\n" );
                        }
                        else
                        {
                           printf( "%s\n", cdata );
                        }

                     }
                     else if ( xtypes[colno] == SPICE_DP )
                     {
                        ekgd_c ( colno, row, 1, &ddata, &null, &found );

                        if ( null )
                        {
                           printf( "<Null>\n" );
                        }
                        else
                        {
                           printf( "%f\n", ddata );
                        }

                     }
                     else if ( xtypes[colno] == SPICE_INT )
                     {
                        ekgi_c ( colno, row, 1, &idata, &null, &found );

                        if ( null )
                        {
                           printf( "<Null>\n" );
                        }
                        else
                        {
                           printf( "%d\n", (int)idata );
                        }

                     }
                     else
                     {

                        /.
                        The item is a time value.  Convert it
                        to UTC for output.
                        ./
                        ekgd_c ( colno, row, 1, &tdata, &null, &found );

                        if ( null )
                        {
                           printf( "<Null>\n" );
                        }
                        else
                        {
                           et2utc_c ( tdata, "C", 3, TIMLEN, utcstr );
                           printf( "%s\n", utcstr );
                        }

                     }

                     /.
                     We're done with the column having index `colno'.
                     ./
                  }

                  /.
                  We're done with the row having index `row'.
                  ./
               }

               /.
               We either processed the query or had an error.
               ./
            }

            /.
            We either parsed the SELECT clause or had an error.
            ./
         }

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      ROW =  0
        VIKING_SEDR_DATA.IMAGE_NUMBER  : 25837050
        VIKING_SEDR_DATA.IMAGE_ID      : 168C09
        VIKING_SEDR_DATA.PLATFORM_CLOCK: 119.880000
        VIKING_SEDR_DATA.IMAGE_TIME    : 1976 JUN 16 16:50:55.925

      ROW =  1
        VIKING_SEDR_DATA.IMAGE_NUMBER  : 25837051
        VIKING_SEDR_DATA.IMAGE_ID      : 168C10
        VIKING_SEDR_DATA.PLATFORM_CLOCK: 119.270000
        VIKING_SEDR_DATA.IMAGE_TIME    : 1976 JUN 16 16:51:00.269

      ROW =  2
        VIKING_SEDR_DATA.IMAGE_NUMBER  : 25840344
        VIKING_SEDR_DATA.IMAGE_ID      : 168C11
        VIKING_SEDR_DATA.PLATFORM_CLOCK: 119.880000
        VIKING_SEDR_DATA.IMAGE_TIME    : 1976 JUN 16 20:56:53.051

      ROW =  3
        VIKING_SEDR_DATA.IMAGE_NUMBER  : 25840345
        VIKING_SEDR_DATA.IMAGE_ID      : 168C12
        VIKING_SEDR_DATA.PLATFORM_CLOCK: 119.270000
        VIKING_SEDR_DATA.IMAGE_TIME    : 1976 JUN 16 20:56:57.395

      ROW =  4
        VIKING_SEDR_DATA.IMAGE_NUMBER  : 25843638
        VIKING_SEDR_DATA.IMAGE_ID      : 169C01
        VIKING_SEDR_DATA.PLATFORM_CLOCK: 119.880000
        VIKING_SEDR_DATA.IMAGE_TIME    : 1976 JUN 17 01:02:50.177

      ROW =  5
        VIKING_SEDR_DATA.IMAGE_NUMBER  : 25843639
        VIKING_SEDR_DATA.IMAGE_ID      : 169C02
        VIKING_SEDR_DATA.PLATFORM_CLOCK: 119.270000
        VIKING_SEDR_DATA.IMAGE_TIME    : 1976 JUN 17 01:02:54.521

      ROW =  6
        VIKING_SEDR_DATA.IMAGE_NUMBER  : 25846934
        VIKING_SEDR_DATA.IMAGE_ID      : 169C03
        VIKING_SEDR_DATA.PLATFORM_CLOCK: 120.140000
        VIKING_SEDR_DATA.IMAGE_TIME    : 1976 JUN 17 05:08:56.263

      ROW =  7
        VIKING_SEDR_DATA.IMAGE_NUMBER  : 25846935
        VIKING_SEDR_DATA.IMAGE_ID      : 169C04
        VIKING_SEDR_DATA.PLATFORM_CLOCK: 119.520000
        VIKING_SEDR_DATA.IMAGE_TIME    : 1976 JUN 17 05:09:00.607

      ROW =  8
        VIKING_SEDR_DATA.IMAGE_NUMBER  : 25848026
        VIKING_SEDR_DATA.IMAGE_ID      : 169C05
        VIKING_SEDR_DATA.PLATFORM_CLOCK: 120.140000
        VIKING_SEDR_DATA.IMAGE_TIME    : 1976 JUN 17 06:30:28.424

      ROW =  9
        VIKING_SEDR_DATA.IMAGE_NUMBER  : 25848030
        VIKING_SEDR_DATA.IMAGE_ID      : 169C09
        VIKING_SEDR_DATA.PLATFORM_CLOCK: 120.140000
        VIKING_SEDR_DATA.IMAGE_TIME    : 1976 JUN 17 06:30:46.174

      ROW =  10
        VIKING_SEDR_DATA.IMAGE_NUMBER  : 25848032
        VIKING_SEDR_DATA.IMAGE_ID      : 169C11
        VIKING_SEDR_DATA.PLATFORM_CLOCK: 120.140000
        VIKING_SEDR_DATA.IMAGE_TIME    : 1976 JUN 17 06:30:55.168


-Restrictions

   1)  Currently, column names are the only supported expressions.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   E.D. Wright         (JPL)

-Version

   -CSPICE Version 2.1.3, 10-AUG-2021 (JDR)

       Edited the header to comply with NAIF standard. Added complete
       code example.

       Extended the -Exceptions section to include the errors detected
       by this wrapper.

   -CSPICE Version 2.1.2, 22-MAR-2016 (NJB)

       Updated brief I/O to describe inputs "tablen" and "collen".
       Also added to -Detailed_Output descriptions of declarations
       of the outputs "tabs" and "cols".

   -CSPICE Version 2.1.1, 14-AUG-2006 (EDW)

       Replace mention of ldpool_c with furnsh_c.

   -CSPICE Version 2.1.0, 02-SEP-1999 (NJB)

       Local type logical variable now used for error flag used in
       interface of ekpsel_.

   -CSPICE Version 2.0.0, 19-JUL-1999 (NJB)

       The data types of the tabs and cols arguments were changed
       to (void *), and associated string length arguments were added.
       This style of interface for string arrays is now standard within
       CSPICE.

       Some corrections of the header comments were made.

   -CSPICE Version 1.0.0, 21-FEB-1999 (NJB)

-Index_Entries

   parse select clause of EK query

-&
*/

{ /* Begin ekpsel_c */


   /*
   Local constants


   XCLASSLEN is the maximum length of a short string indicating the
   class of a SELECT clause item in a QUERY.  The set of expected
   strings is defined by the Fortran SPICELIB routine EKPSEL.  The
   current set of strings is {"COL", "FUNC", "EXPR"}.
   */
   #define XCLASSLEN       4


   /*
   TYPSIZ is the string length associated with the array locXtypes.
   */
   #define TYPSIZ          ( SPICE_EK_TYPLEN + 1 )


   /*
   EXPSIZ is the string length associated with the array locXclass.
   */
   #define EXPSIZ          ( XCLASSLEN + 1 )


   /*
   Local variables
   */
   logical                 err;

   SpiceChar               locXtypes[SPICE_EK_MXCLSG][TYPSIZ];
   SpiceChar               locXclass[SPICE_EK_MXCLSG][EXPSIZ];
   SpiceChar             * strptr;

   SpiceInt                i;
   SpiceInt                lastnb;



   /*
   Participate in error tracing.
   */

   chkin_c ( "ekpsel_c" );

   /*
   Check the input query string to make sure the pointer is non-null and
   the string length is non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "ekpsel_c", query );


   /*
   Make sure the output error message string has at least enough room
   for one output character and a null terminator.  Also check for a
   null pointer.
   */
   CHKOSTR ( CHK_STANDARD, "ekpsel_c", errmsg, msglen );


   /*
   Call the f2c'd function.
   */
   ekpsel_ ( ( char    * ) query,
             ( integer * ) n,
             ( integer * ) xbegs,
             ( integer * ) xends,
             ( char    * ) locXtypes,
             ( char    * ) locXclass,
             ( char    * ) tabs,
             ( char    * ) cols,
             ( logical * ) &err,
             ( char    * ) errmsg,
             ( ftnlen    ) strlen(query),
             ( ftnlen    ) SPICE_EK_TYPLEN,
             ( ftnlen    ) XCLASSLEN,
             ( ftnlen    ) tablen-1,
             ( ftnlen    ) collen-1,
             ( ftnlen    ) msglen-1          );


   /*
   Assign the SpiceBoolean error flag.
   */

   *error = err;


   if ( failed_c() )
   {
      chkout_c ( "ekpsel_c" );
      return;
   }


   /*
   Convert the error message to a C style string.
   */
   F2C_ConvertStr ( msglen, errmsg );


   /*
   If there was a parse error, the other outputs are undefined.
   */
   if ( *error )
   {
      chkout_c ( "ekpsel_c" );
      return;
   }


   /*
   Map the token begin and end indices from Fortran to C style.
   */
   for ( i = 0;  i < *n;  i++ )
   {
      xbegs[i]--;
      xends[i]--;
   }


   /*
   Map the expression data types from strings to SpiceEKDataType values.
   First, map the Fortran-style strings returned by ekpsel_ to C
   style strings.
   */
   F2C_ConvertStrArr ( *n, TYPSIZ, (SpiceChar *)locXtypes );


   for ( i = 0;  i < *n;  i++ )
   {
      if (  eqstr_c( locXtypes[i], "CHR" )  )
      {
         xtypes[i] = SPICE_CHR;
      }

      else if (  eqstr_c( locXtypes[i], "DP" )  )
      {
         xtypes[i] = SPICE_DP;
      }

      else if (  eqstr_c( locXtypes[i], "INT" )  )
      {
         xtypes[i] = SPICE_INT;
      }

      else if (  eqstr_c( locXtypes[i], "TIME" )  )
      {
         xtypes[i] = SPICE_TIME;
      }

      else
      {
         setmsg_c ( "Unrecognized data type string <#> returned "
                    "by ekpsel_ for item #."                     );
         errch_c  ( "#",  locXtypes[i]                           );
         errint_c ( "#",  i                                      );
         sigerr_c ( "SPICE(BUG)"                                 );
         chkout_c ( "ekpsel_c"                                   );
         return;
      }
   }

   /*
   Map the expression classes from strings to SpiceEKExprClass values.
   First, map the Fortran-style strings returned by ekpsel_ to C
   style strings.
   */
   F2C_ConvertStrArr ( *n, EXPSIZ, (SpiceChar *)locXclass );

   for ( i = 0;  i < *n;  i++ )
   {
      if (  eqstr_c( locXclass[i], "COL" )  )
      {
         xclass[i] = SPICE_EK_EXP_COL;
      }

      else if (  eqstr_c( locXclass[i], "FUNC" )  )
      {
         xclass[i] = SPICE_EK_EXP_FUNC;
      }

      else if (  eqstr_c( locXclass[i], "EXPR" )  )
      {
         xclass[i] = SPICE_EK_EXP_EXPR;
      }

      else
      {
         setmsg_c ( "Unrecognized item class string <#> returned "
                    "by ekpsel_ for item #."                     );
         errch_c  ( "#",  locXclass[i]                           );
         errint_c ( "#",  i                                      );
         sigerr_c ( "SPICE(BUG)"                                 );
         chkout_c ( "ekpsel_c"                                   );
         return;
      }
   }


   /*
   Convert the array of table names to a C style array of strings.
   Null-terminate each string so as to eliminate trailing blanks.
   */
   F2C_ConvertStrArr ( *n, tablen, (SpiceChar *)tabs );

   for ( i = 0;  i < *n;  i++ )
   {
      strptr = ((SpiceChar *)tabs) + i*tablen;

      lastnb = F_StrLen ( tablen-1, strptr );

      *( strptr + lastnb ) = (char)0;
   }

   /*
   Convert the array of column names to a C style array of strings.
   Null-terminate each string so as to eliminate trailing blanks.
   */
   F2C_ConvertStrArr ( *n, collen, (SpiceChar *)cols );

   for ( i = 0;  i < *n;  i++ )
   {
      strptr = ((SpiceChar *)cols) + i*collen;

      lastnb = F_StrLen ( collen-1, strptr );

      *( strptr + lastnb ) = (char)0;
   }


   chkout_c ( "ekpsel_c" );

} /* End ekpsel_c */
