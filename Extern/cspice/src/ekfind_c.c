/*

-Procedure ekfind_c ( EK, find data )

-Abstract

   Find E-kernel data that satisfy a set of constraints.

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
   PARSE
   SEARCH

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZst.h"
   #include "SpiceZmc.h"

   void ekfind_c ( ConstSpiceChar    * query,
                   SpiceInt            errmln,
                   SpiceInt          * nmrows,
                   SpiceBoolean      * error,
                   SpiceChar         * errmsg )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   query      I   Query specifying data to be found.
   errmln     I   Declared length of output error message string.
   nmrows     O   Number of matching rows.
   error      O   Flag indicating whether query parsed correctly.
   errmsg     O   Parse error description.

-Detailed_Input

   query       is a character string that specifies a set of EK
               data to select from those present in currently
               loaded EK files. The selected data will be
               retrievable via the EK fetch routines ekgc_c, ekgd_c,
               and ekgi_c.

               The query consists of four clauses, the third and
               fourth of which are optional. The general form
               of a query is

                  SELECT <column list>
                  FROM <table list>
                  [WHERE <constraint list>]
                  [ORDER BY <ORDER BY column list>]

               where brackets indicate optional items. The
               elements of the query shown above are called,
               respectively, the `SELECT clause', the
               `FROM clause', the `WHERE clause', and the
               `ORDER BY clause'. The result of a query may be
               thought of as a new table, whose columns are those
               specified in the SELECT clause, whose rows are
               those satisfying the constraints of the WHERE
               clause, and whose rows are ordered according to
               the ORDER BY clause.

               The SELECT clause specifies a list of columns
               from which data are to be selected. In a simple
               (non-join) query, these columns must belong to
               the single table specified in the FROM clause.

               The form of a SELECT clause is

                  SELECT <column name> [ ,<column name>...]

               In queries having multiple tables in the FROM
               clause, column names are ambiguous if they occur
               in more than one table in the FROM clause. Such
               column names must be qualified with table
               identifiers. These identifiers may be the names of
               the tables to which the columns belong, or table
               `aliases', names (usually short ones) associated
               with tables in the FROM clause. Table aliases have
               duration limited to the execution of the query to
               which they belong.

               The form of a qualified column name is

                  <table name>.<column name>

               or

                  <table alias>.<column name>


               The FROM clause specifies the tables from which
               data are to be selected. In simple queries, only
               one table is listed. In this case the form of
               the FROM clause is

                  FROM <table name>

               In queries involving multiple tables, the form of
               the FROM clause becomes

                  FROM <table name> [<table alias>]
                       [ , <table name> [<table alias>] ... ]

               The aliases associated with the table names must
               be distinct and must not be the actual names of
               loaded EK tables.

               Queries involving multiple tables are called
               `joins'.

               The meaning of a FROM clause containing multiple
               tables is that the output is to be a subset of
               the rows of the Cartesian product of the listed
               tables. Normally, WHERE clause constraints are
               supplied to reduce the selected rows to a set of
               interest.

               The most common example of a join is a query with
               two tables listed in the FROM clause, and a WHERE
               clause constraint enforcing equality of members
               of a column in the first table with members of
               column in the second table. Such a query is
               called an `equi-join'. A join in which columns
               of different tables are related by an inequality
               is called a `non-equi-join'. Any type of join
               other than an equi-join may be very slow to
               evaluate, due to the large number of elements that
               may be contained in the Cartesian
               product of the listed tables.

               The WHERE clause lists constraints that must
               be met by each row satisfying the query. The
               constraints are specified as a logical combination
               of relational expressions. The form of the
               constraint list is

                  WHERE <constraint expression>

               where each <constraint expression> consists of one
               or more simple relational expressions of the form

                  <column name> <operator> <RHS symbol>

               where

                  <RHS symbol>

               is a column name, a literal value, or the special
               symbol

                  NULL

               and

                  <operator>

               is any of

                  EQ, GE, GT, LE, LIKE, LT, NE, NOT LIKE, <, <=,
                  =, >, >=, !=, <>

               For comparison with null values, the special
               syntaxes

                  <column name> IS NULL
                  <column name> IS NOT NULL

               are allowed, in addition to the standard
               comparison syntaxes using the equality or
               inequality operators.

               The LIKE operator allows comparison of a string
               value against a template. The template syntax
               is that allowed by the CSPICE routine MATCHI.
               Templates may include literal characters, the
               wild string marker '*', and the wild character
               marker '%'.  Case is significant in templates.

               Templates are bracketed by quote characters, just
               as are literal strings.

               The query language also supports the BETWEEN and
               NOT BETWEEN constructs

                  <column> BETWEEN <symbol 1> AND <symbol 2>

                  <column> NOT BETWEEN <symbol 1> AND <symbol 2>

               The tokens

                  <symbol 1>
                  <symbol 2>

               may be literal values or column names.

               The BETWEEN operator considers values that match
               the bounds to satisfy the condition: the BETWEEN
               operator tests for inclusion in the closed interval
               defined by the bounds.

               In the WHERE clause, simple relational expressions
               may be combined using the logical operators AND,
               OR, and NOT, as in the Fortran programming
               language. Parentheses may be used to enforce a
               desired order of evaluation of logical expressions.

               The expression syntax is NOT symmetric: literal
               values must not appear on the left hand side of the
               operators that apply to them.

               The columns named in a constraint clause must
               belong to the tables listed in the FROM clause.
               If the query is a join, qualifying table names or
               aliases are required wherever their omission would
               result in ambiguity.

               Data types of the columns or constants used on the
               right-hand-sides of operators must match the data
               types of the corresponding columns on the
               left-hand-sides, except that comparison of integer
               and double precision quantities is permitted.

               Literal strings used in constraints are always
               bracketed by quotes. Either single  quotes (')
               or double quotes (") may be used, but the same
               quote character must be used to start and end any
               literal string. Within character string values,
               quote characters must be doubled in order to be
               recognized. Case is significant in character
               except in comparisons using the LIKE and NOT LIKE
               operators, which ignore case: the expression

                  ANIMAL LIKE "*A*"

               would be considered true when ANIMAL takes the
               value

                  "cat"

               Time values are considered to be strings and
               require bracketing quotes. Currently, the
               only time values allowed are UTC times in ISO
               format, UTC times represented in forms accepted by
               the CSPICE routine TPARSE, and SCLK strings in
               NAIF format.

               The ORDER BY clause indicates which columns to
               use to order the output generated by the query.
               The columns in the ORDER BY clause define a
               dictionary ordering, with the first listed column
               acting as a primary key, the second column acting
               as a secondary key, and so on.

               For each ORDER BY column, the keywords ASC or DESC
               may be supplied to indicate whether the items in
               that column are to be listed in ascending or
               descending order. Ascending order is the default.
               The direction in which data items increase is
               referred to as the `order sense'.

               The ORDER BY clause, if present, must appear
               last in the query.

               The form of the ORDER BY clause is

                  ORDER BY <column name> [<order sense>]
                           [ ,<column name> [<order sense>]...]

               Rows satisfying the query constraints will be
               returned so that the entries of the first column
               specified in the ORDER BY clause will be appear in
               the order specified by the order sense keyword,
               which is assumed to be ASC if absent. When entries
               in the first through Nth ORDER BY column are equal,
               the entries in the (N+1)st ORDER BY column
               determine the order of the rows, and so on.

               As in the WHERE clause, column names must be
               qualified by table names or table aliases where
               they would otherwise be ambiguous.

               The query language is word-oriented, and some
               indicate whether the words are reserved. Reserved
               words must be separated from other words by white
               space. It is not necessary to use white space
               to separate words and punctuation characters.
               The list of reserved words is

                  AND
                  BETWEEN
                  BY
                  COLUMN
                  EQ
                  FROM
                  GE
                  GT
                  IS
                  LE
                  LT
                  LIKE
                  NE
                  NOT
                  NULL
                  OR
                  ORDER
                  SELECT
                  WHERE

               The left and right parenthesis characters are also
               reserved; they may not be used in queries outside
               of quoted strings.

               Case is not significant in queries, except within
               literal strings.


   errmln      is the maximum number of characters that can be
               accommodated in the output string. This count
               includes room for the terminating null character.
               For example, if the maximum allowed length of the
               output string, including the terminating null, is 25
               characters, then errmln should be set to 25.

-Detailed_Output

   nmrows      is the number of rows that match the query
               criteria. nmrows is defined if and only if
               error is returned as SPICEFALSE.

   error       is a logical flag indicating whether the query
               failed to parse correctly.

   errmsg      is a character string that describes ekfind_c's
               diagnosis of a parse error, should one occur.
               Otherwise, errmsg will be returned blank.

-Parameters

   See the include files.

-Exceptions

   1)  Most of the exceptions that can occur on a call to
       ekfind_c are caused by errors in the input query. ekfind_c
       attempts to diagnose these via the output error flag and
       error message, instead of signaling errors. The following
       classes of errors are detected:

          Scanning errors---these result from badly formed query
          in which ekfind_c could not identify all of the tokens.
          When these errors occur, ekfind_c may be too confused to
          give a helpful diagnostic message.

          Parsing errors---these result from a badly formed
          query that ekfind_c was able to separate into tokens
          but that ekfind_c determined to be syntactically invalid:

          Name resolution errors---these result from referencing
          invalid or ambiguous column or table names in a query.

          Time resolution errors---these result from use of time
          strings that cannot be parsed.

          Semantic errors---these result from a syntactically
          valid query that violates a limit or a restriction on
          values used in a query.


   Some problems with queries are not trapped by ekfind_c but
   instead cause errors to be signaled. These are listed below.

   2)  If no E-kernels are loaded at the time this routine is called,
       an error is signaled by a routine in the call tree of this
       routine.

   3)  If a leapseconds kernel is is not loaded before this routine
       is called, UTC time values may not be used in queries. If they
       are, an error is signaled by a routine in the call tree of
       this routine.

   4)  If an SCLK kernel for the appropriate spacecraft clock has not
       been loaded before this routine is called, SCLK values for
       that clock may not be used in queries. If they are, an error
       is signaled by a routine in the call tree of this routine.

   5)  If the `query' input string pointer is null, the error
       SPICE(NULLPOINTER) is signaled.

   6)  If the `query' input string has zero length, the error
       SPICE(EMPTYSTRING) is signaled.

   7)  If the `errmsg' output string pointer is null, the error
       SPICE(NULLPOINTER) is signaled.

   8)  If the `errmsg' output string has length less than two
       characters, the error SPICE(STRINGTOOSHORT) is signaled, since
       the output string is too short to contain one character of
       output data plus a null terminator.

-Files

   This routine issues queries against one or more binary EKs that
   have been loaded into the CSPICE query system.

-Particulars

   This routine operates almost entirely by side effects: it
   prepares the EK fetch routines to return event data that
   satisfy the input query. See the EK Required Reading for examples
   of use of this routine in conjunction with the EK fetch routines.

-Examples

   The numerical results shown for these examples may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Perform a query on an EK file that contains a database with
      the different commands of the Deep Impact spacecraft subsystem,
      and a table with the subsystem id, the parameter name, and the
      description of that parameters, ordered by subsystem name. Print
      the number of records of that table.

      Use the EK kernel below to load the Deep Impact spacecraft
      subsystem commands dictionary.

         dif_cmdict_128_20050620.bdb


      Example code begins here.


      /.
         Program ekfind_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Local parameters
         ./
         #define EKNAME       "dif_cmdict_128_20050620.bdb"
         #define ERRLEN       1841

         /.
         Local variables
         ./
         SpiceChar            errmsg [ERRLEN];
         SpiceChar          * query;

         SpiceInt             nmrows;

         SpiceBoolean         error;

         /.
         Open an EK file.
         ./
         furnsh_c ( EKNAME );

         /.
         The EK file contains the table "DIF_COMMANDS",
         and that "DIF_COMMANDS" contains columns named:

           SUBSYSTEM, COMMAND, PARAMETER_NAME, DESCRIPTION

         Define a set of constraints to perform a query on all
         loaded EK files (the SELECT clause).
         ./
         query = "Select SUBSYSTEM, COMMAND, PARAMETER_NAME, DESCRIPTION "
                 "from DIF_COMMANDS order by SUBSYSTEM";

         /.
         Query the EK system for data rows matching the
         SELECT constraints.
         ./
         ekfind_c ( query, ERRLEN, &nmrows, &error, errmsg );

         /.
         Check whether an error occurred while processing the
         SELECT clause. If so, output the error message.
         ./
         if ( error )
         {
            printf( "SELECT clause error:  %s\n", errmsg );
         }
         else
         {

            /.
            If no error, `nmrows' contains the number of rows matching
            the constraints specified in the query string
            ./
            printf( "Number of matching rows:  %d\n", nmrows );

         }

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Number of matching rows:  5798


   2) Examples of strings containing syntactically valid queries:

          SELECT COL1 FROM TAB1

          select col1 from tab1 where col1 gt 5

          SELECT COL2 FROM TAB1 WHERE COL2 > 5.7 ORDER BY COL2

          SELECT COL2 FROM TAB1 WHERE COL1 != 5

          SELECT COL2 FROM TAB1 WHERE COL1 GE COL2

          SELECT COL1, COL2, COL3 FROM TAB1 ORDER BY COL1

          SELECT COL3 FROM TAB1 WHERE COL5 EQ "ABC"

          SELECT COL3 FROM TAB1 WHERE COL5 = "ABC"

          SELECT COL3 FROM TAB1 WHERE COL5 LIKE 'A*'

          SELECT COL3 FROM TAB1 WHERE COL5 LIKE 'A%%'

          SELECT COL4 FROM TAB1 WHERE COL4 = '1995 JAN 1 12:38:09.7'

          SELECT COL4 FROM TAB1 WHERE COL4 = "1995 JAN 1 12:38:09.7"

          SELECT COL4 FROM TAB1 WHERE
          COL4 NE 'GLL SCLK 02724646:67:7:2'

          SELECT COL1 FROM TAB1 WHERE COL1 != NULL

          SELECT COL1 FROM TAB1 WHERE COL1 IS NULL

          SELECT COL1 FROM TAB1 WHERE COL1 IS NOT NULL

          SELECT COL1, COL2, COL3 FROM TAB1
          WHERE (COL1 BETWEEN 4 AND 6) AND (COL3 NOT LIKE "A%%")
          ORDER BY COL1, COL3

          SELECT COL4 FROM TAB1
          WHERE COL4 BETWEEN "1995 JAN 1 12:38" AND
          "October 23, 1995"

          SELECT COL1, COL2 FROM TAB1 WHERE
          NOT (    ( ( COL1 <  COL2 ) AND ( COL1 > 5   ) )  OR
                   ( ( COL1 >= COL2 ) AND ( COL2 <= 10 ) )      )


          SELECT T1.COL1, T1.COL2, T2.COL2, T2.COL3
          FROM TABLE1 T1, TABLE2 T2
          WHERE T1.COL1 = T2.COL1
          AND T1.COL2 > 5
          ORDER BY T1.COL1, T2.COL2


   3) Examples of syntactically invalid queries:

          SELECT TIME WHERE TIME
          LT 1991 JAN 1                      {FROM clause is absent}

          select time from table1 where
          time lt 1991 jan 1                 {time string is not
                                              quoted}

          select time from table1
          where time .lt. '1991 jan 1'       {operator should be lt}

          select cmd from table1
          where "cmd,6tmchg" != cmd          {value is on left side
                                              of operator}

          select event_type from table1
          where event_type eq ""             {quoted string is empty
                                              ---use " " to indicate
                                              a blank string}

          select event_type from table1
          where event_type = "COMMENT"
          order TIME                         {ORDER BY phrase is
                                              lacking BY keyword}

          select COL1 from table where
          where COL1 eq MOC_EVENT            {literal string on
                                              right-hand-side of
                                              operator is not quoted}



       In the following examples, we'll assume that the program
       calling ekfind_c has loaded an EK containing two segments
       having columns having the following names and attributes:


        TABLE1:
        ==========

          Column name        Data type         Size       Indexed?
          -----------        ---------         ----       --------
          EVENT_TYPE         CHARACTER*32      1          YES
          EVENT_PARAMETERS   CHARACTER*(*)     1          NO
          COMMENT            CHARACTER*80      VARIABLE   NO


        TABLE2:
        ==========

          Column name        Data type         Size       Indexed?
          -----------        ---------         ----       --------
          EVENT_TYPE         CHARACTER*32      1          YES
          EVENT_PARAMETERS   CHARACTER*80      1          NO
          COMMENT            CHARACTER*80      VARIABLE   NO
          COMMAND            CHARACTER*80      1          YES


       Then the following queries are semantically invalid:

          SELECT EVENT_PARAMETERS
          FROM TABLE1
          WHERE EVENT_DURATION = 7.0         {No column called
                                              EVENT_DURATION
                                              is present in a loaded
                                              EK}

          SELECT COMMENT FROM TABLE2
          WHERE COMMENT EQ "N/A"             {The COMMENT column does
                                              not have size 1 and
                                              therefore cannot be
                                              referenced in a query}

-Restrictions

   1)  A leapseconds kernel must be loaded before this routine may
       be called, if UTC time values are used in input queries.

   2)  An appropriate SCLK kernel must be loaded before this routine
       may be called, if SCLK values are used in input queries.

   3)  Data found in response to a query become unavailable
       when a fast load is initiated via ekifld_c. Any desired
       fetches of the data must be performed before a fast
       load or any other operation that modifies the EK scratch
       area is initiated.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)

-Version

   -CSPICE Version 1.2.0, 13-AUG-2021 (JDR)

       Changed input argument name "lenout" to "errmln" for consistency
       with other routines.

       Edited the header to comply with NAIF standard. Added
       complete code examples.

   -CSPICE Version 1.1.2, 19-DEC-2001 (NJB)

       -Restrictions section was updated.

   -CSPICE Version 1.1.0, 12-JUL-1998 (NJB)

       Bug fix: now uses local logical variable to capture the
       error flag value returned by the underlying f2c'd routine.

   -CSPICE Version 1.0.0, 01-APR-1998 (NJB)

       Based on SPICELIB Version 1.0.0, 25-MAR-1998

-Index_Entries

   find EK data
   issue EK query

-&
*/

{ /* Begin ekfind_c */

   /*
   Local variables
   */
   logical                 fError;


   /*
   Participate in error tracing.
   */
   chkin_c ( "ekfind_c" );

   /*
   Check the query string to make sure the pointer is non-null
   and the string length is non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "ekfind_c", query );

   /*
   Make sure the output string has at least enough room for one output
   character and a null terminator.  Also check for a null pointer.
   */
   CHKOSTR ( CHK_STANDARD, "ekfind_c", errmsg, errmln );

   /*
   Call the f2c'd Fortran routine.  Use explicit type casts for every
   type defined by f2c.
   */
   ekfind_ (  ( char     * )  query,
              ( integer  * )  nmrows,
              ( logical  * )  &fError,
              ( char     * )  errmsg,
              ( ftnlen     )  strlen(query),
              ( ftnlen     )  errmln-1       );

   if ( fError )
   {
      /*
      If a parse error was detected, the output string errmsg will
      be set.  Convert the Fortran string to a C string by placing a
      null after the last non-blank character.  This operation is valid
      whether or not the SPICELIB routine signaled an error.
      */

      F2C_ConvertStr ( errmln, errmsg );
   }

   else
   {
      /*
      The error message may be uninitialized.  Null-terminate
      the message string.
      */
      errmsg[0] = NULLCHAR;
   }


   *error = fError;


   chkout_c ( "ekfind_c" );

} /* End ekfind_c */
