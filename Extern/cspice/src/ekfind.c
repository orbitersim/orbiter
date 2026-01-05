/* ekfind.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__27869 = 27869;
static integer c__100 = 100;
static integer c__500 = 500;

/* $Procedure EKFIND ( EK, find data ) */
/* Subroutine */ int ekfind_(char *query, integer *nmrows, logical *error, 
	char *errmsg, ftnlen query_len, ftnlen errmsg_len)
{
    extern /* Subroutine */ int zzekscan_(char *, integer *, integer *, 
	    integer *, integer *, integer *, integer *, integer *, doublereal 
	    *, char *, integer *, integer *, logical *, char *, ftnlen, 
	    ftnlen, ftnlen), zzeksemc_(char *, integer *, char *, logical *, 
	    char *, integer *, ftnlen, ftnlen, ftnlen), zzekqini_(integer *, 
	    integer *, integer *, char *, doublereal *, ftnlen), zzekpars_(
	    char *, integer *, integer *, integer *, integer *, integer *, 
	    doublereal *, char *, integer *, integer *, integer *, char *, 
	    doublereal *, logical *, char *, ftnlen, ftnlen, ftnlen, ftnlen), 
	    zzeknres_(char *, integer *, char *, logical *, char *, integer *,
	     ftnlen, ftnlen, ftnlen), zzektres_(char *, integer *, char *, 
	    doublereal *, logical *, char *, integer *, ftnlen, ftnlen, 
	    ftnlen), chkin_(char *, ftnlen);
    char eqryc[2000];
    doublereal eqryd[100];
    integer eqryi[27875], chbegs[500], chends[500];
    char chrbuf[2000];
    extern logical return_(void);
    doublereal numvls[100];
    integer errptr, lxbegs[500], lxends[500], ntoken, tokens[500], values[500]
	    ;
    extern /* Subroutine */ int chkout_(char *, ftnlen), eksrch_(integer *, 
	    char *, doublereal *, integer *, logical *, char *, ftnlen, 
	    ftnlen);

/* $ Abstract */

/*     Find E-kernel data that satisfy a set of constraints. */

/* $ Disclaimer */

/*     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE */
/*     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S. */
/*     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE */
/*     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE */
/*     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS" */
/*     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY */
/*     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A */
/*     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC */
/*     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE */
/*     SOFTWARE AND RELATED MATERIALS, HOWEVER USED. */

/*     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA */
/*     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT */
/*     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND, */
/*     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS, */
/*     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE */
/*     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY. */

/*     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF */
/*     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY */
/*     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE */
/*     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE. */

/* $ Required_Reading */

/*     EK */

/* $ Keywords */

/*     EK */
/*     PARSE */
/*     SEARCH */

/* $ Declarations */
/* $ Disclaimer */

/*     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE */
/*     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S. */
/*     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE */
/*     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE */
/*     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS" */
/*     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY */
/*     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A */
/*     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC */
/*     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE */
/*     SOFTWARE AND RELATED MATERIALS, HOWEVER USED. */

/*     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA */
/*     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT */
/*     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND, */
/*     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS, */
/*     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE */
/*     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY. */

/*     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF */
/*     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY */
/*     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE */
/*     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE. */


/*     Include Section:  EK Encoded Query Internal Parameters */

/*        ekquery.inc  Version 3    16-NOV-1995 (NJB) */

/*           Updated to reflect increased value of MAXCON in */
/*           ekqlimit.inc. */

/*        ekquery.inc  Version 2    03-AUG-1995 (NJB) */

/*           Updated to support representation of the SELECT clause. */


/*        ekquery.inc  Version 1    12-JAN-1995 (NJB) */


/*     An encoded EK query is an abstract data type implemented */
/*     as an integer cell, along with a double precision cell and */
/*     a character string.  The d.p. cell and string contain numeric */
/*     and string values from the query string represented by the */
/*     encoded query. */

/*     The parameters in this file are intended for use only by the */
/*     EK encoded query access routines.  Callers of EK routines should */
/*     not use these parameters. */

/*     The following parameters are indices of specified elements */
/*     in the integer portion of the encoded query. */

/*     Encoded query architecture type: */


/*     `Name resolution' consists of: */

/*        - Verifying existence of tables:  any table names listed */
/*          in the FROM clause of a query must be loaded. */

/*        - Validating table aliases used to qualify column names. */

/*        - Verifying existence of columns and obtaining data types */
/*          for columns. */

/*        - Setting data type codes for literal values in the encoded */
/*          query. */

/*        - Checking consistency of operators and operand data types. */

/*        - Making sure unqualified column names are unambiguous. */

/*        - For constraints, mapping the table names used to qualify */
/*          column names to the ordinal position in the FROM clause */
/*          of the corresponding table. */


/*     Initialization status---this flag indicates whether the encoded */
/*     query has been initialized.  Values are ITRUE or IFALSE.  See the */
/*     include file ekbool.inc for parameter values. */


/*     Parse status---this flag indicates whether the parsing operation */
/*     that produced an encoded query has been completed. Values are */
/*     ITRUE or IFALSE. */


/*     Name resolution status---this flag indicates whether names */
/*     have been resolved in an encoded query.  Values are ITRUE or */
/*     IFALSE. */


/*     Time resolution status---this flag indicates whether time values */
/*     have been resolved in an encoded query.  Time resolution */
/*     consists of converting strings representing time values to ET. */
/*     Values of the status are ITRUE or IFALSE. */


/*     Semantic check status---this flag indicates whether semantic */
/*     checking of constraints has been performed. */


/*     Number of tables specified in FROM clause: */


/*     Number of constraints in query: */


/*     A special value is used to indicate the `maximal' constraint--- */
/*     one that logically cannot be satisfied.  If the constraints */
/*     are equivalent to the maximal constraint, the location EQNCNS */
/*     is assigned the value EQMXML */


/*     Number of constraint conjunctions: */


/*     Number of order-by columns: */


/*     Number of SELECT columns: */


/*     Size of double precision buffer: */


/*     `Free' pointer into double precision buffer: */


/*     Size of character string buffer: */


/*     `Free' pointer into character string buffer: */


/*     The following four base pointers will be valid after a query */
/*     has been parsed: */

/*     Base pointer for SELECT column descriptors: */


/*     Base pointer for constraint descriptors: */


/*     Base pointer for conjunction sizes: */


/*     Base pointer for order-by column descriptors: */


/*     After the quantities named above, the integer array contains */
/*     series of descriptors for tables, constraints, and order-by */
/*     columns, as well as a list of `conjunction sizes'---that is, */
/*     the sizes of the groups of constraints that form conjunctions, */
/*     after the input query has been re-arranged as a disjunction of */
/*     conjunctions of constraints. */


/*     The offsets of specific elements within descriptors are */
/*     parameterized. The base addresses of the descriptors themselves */
/*     must be  calculated using the counts and sizes of the items */
/*     preceding them. */

/*     A diagram of the structure of the variable-size portion of the */
/*     integer array is shown below: */


/*        +-------------------------------------+ */
/*        | Fixed-size portion of encoded query | */
/*        +-------------------------------------+ */
/*        |         Encoded FROM clause         | */
/*        +-------------------------------------+ */
/*        |      Encoded constraint clause      | */
/*        +-------------------------------------+ */
/*        |          Conjunction sizes          | */
/*        +-------------------------------------+ */
/*        |       Encoded ORDER BY clause       | */
/*        +-------------------------------------+ */
/*        |        Encoded SELECT clause        | */
/*        +-------------------------------------+ */


/*     Value Descriptors */
/*     ---------------- */

/*     In order to discuss the various descriptors below, we'll make use */
/*     of sub-structures called `value descriptors'.  These descriptors */
/*     come in two flavors:  character and double precision.  For */
/*     strings, a descriptor is a set of begin and end pointers that */
/*     indicate the location of the string in the character portion of an */
/*     encoded query, along with the begin and end pointers for the */
/*     corresponding lexeme in the original query.  The pointers are set */
/*     to zero when they are not in use, for example if they refer to an */
/*     optional lexeme that did not appear in the input query. */

/*     All value descriptors start with a data type indicator; values */
/*     are from ektype.inc.  Integer and time values are referred to */
/*     by double precision descriptors. */

/*     Parameters for string value descriptor elements: */


/*     Numeric value descriptors are similar to those for string values, */
/*     the difference being that they have only one pointer to the value */
/*     they represent.  This pointer is the index of the value in the */
/*     encoded query's numeric buffer. */


/*     All value descriptors have the same size.  In order to allow */
/*     table descriptors to have the same size as value descriptors, */
/*     we include an extra element in the descriptor. */


/*     Column Descriptors */
/*     ----------------- */

/*     Each column descriptor consists of a character descriptor for the */
/*     name of the column, followed by an index, which gives the ordinal */
/*     position of the column in the logical table to which the column */
/*     belongs.  The index element is filled in during name resolution. */


/*     Table Descriptors */
/*     ----------------- */

/*     Each table descriptor consists of a character descriptor for the */
/*     name of the table, followed by an index, which gives the ordinal */
/*     position of the table in the FROM clause in the original query. */
/*     The index element is filled in during name resolution.  Aliases */
/*     and table names have identical descriptor structures. */


/*     Constraint descriptors */
/*     ------------------ */

/*     Each constraint is characterized by: */

/*        - A code indicating whether the constraint compares values */
/*          in two columns or the value in a column and a literal */
/*          value.  The values of this element are EQCOL and EQVAL. */



/*        - A descriptor for the table used to qualify the */
/*          column name on the left side of the constraint. */


/*        - A character value descriptor for the column name on the left */
/*          side of the query. */


/*        - An operator code indicating the relational operator used */
/*          in the constraint. */


/*        If the constraint compares values from two columns, the */
/*        next items are table and column name descriptors that apply to */
/*        the column named on the right side of the relational operator. */


/*        If the constraint has a literal value on the right side, the */
/*        operator code is followed by... */

/*        - a value descriptor. */


/*        - Size of a constraint descriptor: */


/*     Conjunction sizes */
/*     ----------------- */

/*     The size of each conjunction of constraints occupies a single */
/*     integer. */




/*     Order-by Column Descriptors */
/*     --------------------------- */

/*     Each order-by column descriptor contains descriptors for */
/*     the table containing the column and for the name of the column */
/*     itself; one additional element is used to indicate the direction */
/*     of the ordering (ascending vs descending). */


/*        - The last integer in the descriptor indicates whether the */
/*          order direction is ascending or descending. */


/*        - Size of an order-by column descriptor: */


/*     Codes indicating sense of ordering (ascending vs descending): */


/*     SELECT Column Descriptors */
/*     --------------------------- */

/*     Each SELECT column descriptor contains descriptors for */
/*     the table containing the column and for the name of the column */
/*     itself. */


/*        - Size of a SELECT column descriptor: */


/*     Miscellaneous parameters: */


/*     EQIMIN is the minimum size of the integer portion of */
/*     an encoded query.  EQIMIN depends on the parameters */

/*        MAXTAB */
/*        MAXCON */
/*        MAXORD */
/*        MAXSEL */

/*     all of which are declared in the include file ekqlimit.inc. */
/*     The functional definition of EQIMIN is: */

/*     INTEGER               EQIMIN */
/*     PARAMETER           ( EQIMIN =   EQVBAS */
/*    .                              +  MAXTAB * EQVDSZ * 2 */
/*    .                              +  MAXCON * EQCDSZ */
/*    .                              +  MAXCON */
/*    .                              +  MAXORD * EQODSZ */
/*    .                              +  MAXSEL * EQSDSZ     ) */


/*     End Include Section:  EK Encoded Query Internal Parameters */

/* $ Disclaimer */

/*     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE */
/*     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S. */
/*     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE */
/*     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE */
/*     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS" */
/*     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY */
/*     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A */
/*     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC */
/*     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE */
/*     SOFTWARE AND RELATED MATERIALS, HOWEVER USED. */

/*     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA */
/*     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT */
/*     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND, */
/*     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS, */
/*     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE */
/*     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY. */

/*     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF */
/*     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY */
/*     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE */
/*     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE. */


/*     Include Section:  EK Query Limit Parameters */

/*        ekqlimit.inc  Version 3    16-NOV-1995 (NJB) */

/*           Parameter MAXCON increased to 1000. */

/*        ekqlimit.inc  Version 2    01-AUG-1995 (NJB) */

/*           Updated to support SELECT clause. */


/*        ekqlimit.inc  Version 1    07-FEB-1995 (NJB) */


/*     These limits apply to character string queries input to the */
/*     EK scanner.  This limits are part of the EK system's user */
/*     interface:  the values should be advertised in the EK required */
/*     reading document. */


/*     Maximum length of an input query:  MAXQRY.  This value is */
/*     currently set to twenty-five 80-character lines. */


/*     Maximum number of columns that may be listed in the */
/*     `order-by clause' of a query:  MAXSEL.  MAXSEL = 50. */


/*     Maximum number of tables that may be listed in the `FROM */
/*     clause' of a query: MAXTAB. */


/*     Maximum number of relational expressions that may be listed */
/*     in the `constraint clause' of a query: MAXCON. */

/*     This limit applies to a query when it is represented in */
/*     `normalized form': that is, the constraints have been */
/*     expressed as a disjunction of conjunctions of relational */
/*     expressions. The number of relational expressions in a query */
/*     that has been expanded in this fashion may be greater than */
/*     the number of relations in the query as orginally written. */
/*     For example, the expression */

/*             ( ( A LT 1 ) OR ( B GT 2 ) ) */
/*        AND */
/*             ( ( C NE 3 ) OR ( D EQ 4 ) ) */

/*     which contains 4 relational expressions, expands to the */
/*     equivalent normalized constraint */

/*             (  ( A LT 1 ) AND ( C NE 3 )  ) */
/*        OR */
/*             (  ( A LT 1 ) AND ( D EQ 4 )  ) */
/*        OR */
/*             (  ( B GT 2 ) AND ( C NE 3 )  ) */
/*        OR */
/*             (  ( B GT 2 ) AND ( D EQ 4 )  ) */

/*     which contains eight relational expressions. */



/*     MXJOIN is the maximum number of tables that can be joined. */


/*     MXJCON is the maximum number of join constraints allowed. */


/*     Maximum number of order-by columns that may be used in the */
/*     `order-by clause' of a query: MAXORD. MAXORD = 10. */


/*     Maximum number of tokens in a query: 500. Tokens are reserved */
/*     words, column names, parentheses, and values. Literal strings */
/*     and time values count as single tokens. */


/*     Maximum number of numeric tokens in a query: */


/*     Maximum total length of character tokens in a query: */


/*     Maximum length of literal string values allowed in queries: */
/*     MAXSTR. */


/*     End Include Section:  EK Query Limit Parameters */

/* $ Disclaimer */

/*     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE */
/*     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S. */
/*     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE */
/*     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE */
/*     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS" */
/*     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY */
/*     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A */
/*     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC */
/*     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE */
/*     SOFTWARE AND RELATED MATERIALS, HOWEVER USED. */

/*     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA */
/*     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT */
/*     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND, */
/*     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS, */
/*     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE */
/*     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY. */

/*     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF */
/*     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY */
/*     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE */
/*     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE. */


/*     Include Section:  EK Data Types */

/*        ektype.inc Version 1  27-DEC-1994 (NJB) */


/*     Within the EK system, data types of EK column contents are */
/*     represented by integer codes.  The codes and their meanings */
/*     are listed below. */

/*     Integer codes are also used within the DAS system to indicate */
/*     data types; the EK system makes no assumptions about compatibility */
/*     between the codes used here and those used in the DAS system. */


/*     Character type: */


/*     Double precision type: */


/*     Integer type: */


/*     `Time' type: */

/*     Within the EK system, time values are represented as ephemeris */
/*     seconds past J2000 (TDB), and double precision numbers are used */
/*     to store these values.  However, since time values require special */
/*     treatment both on input and output, and since the `TIME' column */
/*     has a special role in the EK specification and code, time values */
/*     are identified as a type distinct from double precision numbers. */


/*     End Include Section:  EK Data Types */

/* $ Disclaimer */

/*     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE */
/*     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S. */
/*     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE */
/*     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE */
/*     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS" */
/*     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY */
/*     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A */
/*     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC */
/*     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE */
/*     SOFTWARE AND RELATED MATERIALS, HOWEVER USED. */

/*     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA */
/*     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT */
/*     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND, */
/*     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS, */
/*     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE */
/*     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY. */

/*     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF */
/*     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY */
/*     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE */
/*     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE. */


/*     Include Section:  EK Operator Codes */

/*        ekopcd.inc  Version 1  30-DEC-1994 (NJB) */


/*     Within the EK system, operators used in EK queries are */
/*     represented by integer codes.  The codes and their meanings are */
/*     listed below. */

/*     Relational expressions in EK queries have the form */

/*        <column name> <operator> <value> */

/*     For columns containing numeric values, the operators */

/*        EQ,  GE,  GT,  LE,  LT,  NE */

/*     may be used; these operators have the same meanings as their */
/*     Fortran counterparts.  For columns containing character values, */
/*     the list of allowed operators includes those in the above list, */
/*     and in addition includes the operators */

/*        LIKE,  UNLIKE */

/*     which are used to compare strings to a template.  In the character */
/*     case, the meanings of the parameters */

/*        GE,  GT,  LE,  LT */

/*     match those of the Fortran lexical functions */

/*        LGE, LGT, LLE, LLT */


/*     The additional unary operators */

/*        ISNULL, NOTNUL */

/*     are used to test whether a value of any type is null. */



/*     End Include Section:  EK Operator Codes */

/* $ Disclaimer */

/*     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE */
/*     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S. */
/*     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE */
/*     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE */
/*     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS" */
/*     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY */
/*     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A */
/*     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC */
/*     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE */
/*     SOFTWARE AND RELATED MATERIALS, HOWEVER USED. */

/*     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA */
/*     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT */
/*     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND, */
/*     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS, */
/*     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE */
/*     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY. */

/*     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF */
/*     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY */
/*     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE */
/*     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE. */


/*     Include Section:  EK Column Name Size */

/*        ekcnamsz.inc Version 1    17-JAN-1995 (NJB) */


/*     Size of column name, in characters. */


/*     End Include Section:  EK Column Name Size */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     QUERY      I   Query specifying data to be found. */
/*     NMROWS     O   Number of matching rows. */
/*     ERROR      O   Flag indicating whether query parsed correctly. */
/*     ERRMSG     O   Parse error description. */

/* $ Detailed_Input */

/*     QUERY    is a character string that specifies a set of EK */
/*              data to select from those present in currently */
/*              loaded EK files. The selected data will be */
/*              retrievable via the EK fetch routines EKGC, EKGD, */
/*              and EKGI. */

/*              The query consists of four clauses, the third and */
/*              fourth of which are optional. The general form */
/*              of a query is */

/*                 SELECT <column list> */
/*                 FROM <table list> */
/*                 [WHERE <constraint list>] */
/*                 [ORDER BY <ORDER BY column list>] */

/*              where brackets indicate optional items. The */
/*              elements of the query shown above are called, */
/*              respectively, the `SELECT clause', the */
/*              `FROM clause', the `WHERE clause', and the */
/*              `ORDER BY clause'. The result of a query may be */
/*              thought of as a new table, whose columns are those */
/*              specified in the SELECT clause, whose rows are */
/*              those satisfying the constraints of the WHERE */
/*              clause, and whose rows are ordered according to */
/*              the ORDER BY clause. */

/*              The SELECT clause specifies a list of columns */
/*              from which data are to be selected. In a simple */
/*              (non-join) query, these columns must belong to */
/*              the single table specified in the FROM clause. */

/*              The form of a SELECT clause is */

/*                 SELECT <column name> [ ,<column name>...] */

/*              In queries having multiple tables in the FROM */
/*              clause, column names are ambiguous if they occur */
/*              in more than one table in the FROM clause. Such */
/*              column names must be qualified with table */
/*              identifiers. These identifiers may be the names of */
/*              the tables to which the columns belong, or table */
/*              `aliases', names (usually short ones) associated */
/*              with tables in the FROM clause. Table aliases have */
/*              duration limited to the execution of the query to */
/*              which they belong. */

/*              The form of a qualified column name is */

/*                 <table name>.<column name> */

/*              or */

/*                 <table alias>.<column name> */


/*              The FROM clause specifies the tables from which */
/*              data are to be selected. In simple queries, only */
/*              one table is listed. In this case the form of */
/*              the FROM clause is */

/*                 FROM <table name> */

/*              In queries involving multiple tables, the form of */
/*              the FROM clause becomes */

/*                 FROM <table name> [<table alias>] */
/*                      [ , <table name> [<table alias>] ... ] */

/*              The aliases associated with the table names must */
/*              be distinct and must not be the actual names of */
/*              loaded EK tables. */

/*              Queries involving multiple tables are called */
/*              `joins'. */

/*              The meaning of a FROM clause containing multiple */
/*              tables is that the output is to be a subset of */
/*              the rows of the Cartesian product of the listed */
/*              tables. Normally, WHERE clause constraints are */
/*              supplied to reduce the selected rows to a set of */
/*              interest. */

/*              The most common example of a join is a query with */
/*              two tables listed in the FROM clause, and a WHERE */
/*              clause constraint enforcing equality of members */
/*              of a column in the first table with members of */
/*              column in the second table. Such a query is */
/*              called an `equi-join'. A join in which columns */
/*              of different tables are related by an inequality */
/*              is called a `non-equi-join'. Any type of join */
/*              other than an equi-join may be very slow to */
/*              evaluate, due to the large number of elements that */
/*              may be contained in the Cartesian */
/*              product of the listed tables. */

/*              The WHERE clause lists constraints that must */
/*              be met by each row satisfying the query. The */
/*              constraints are specified as a logical combination */
/*              of relational expressions. The form of the */
/*              constraint list is */

/*                 WHERE <constraint expression> */

/*              where each <constraint expression> consists of one */
/*              or more simple relational expressions of the form */

/*                 <column name> <operator> <RHS symbol> */

/*              where */

/*                 <RHS symbol> */

/*              is a column name, a literal value, or the special */
/*              symbol */

/*                 NULL */

/*              and */

/*                 <operator> */

/*              is any of */

/*                 EQ, GE, GT, LE, LIKE, LT, NE, NOT LIKE, <, <=, */
/*                 =, >, >=, !=, <> */

/*              For comparison with null values, the special */
/*              syntaxes */

/*                 <column name> IS NULL */
/*                 <column name> IS NOT NULL */

/*              are allowed, in addition to the standard */
/*              comparison syntaxes using the equality or */
/*              inequality operators. */

/*              The LIKE operator allows comparison of a string */
/*              value against a template. The template syntax */
/*              is that allowed by the SPICELIB routine MATCHI. */
/*              Templates may include literal characters, the */
/*              wild string marker '*', and the wild character */
/*              marker '%'. Case is significant in templates. */

/*              Templates are bracketed by quote characters, just */
/*              as are literal strings. */

/*              The query language also supports the BETWEEN and */
/*              NOT BETWEEN constructs */

/*                 <column> BETWEEN <symbol 1> AND <symbol 2> */

/*                 <column> NOT BETWEEN <symbol 1> AND <symbol 2> */

/*              The tokens */

/*                 <symbol 1> */
/*                 <symbol 2> */

/*              may be literal values or column names. */

/*              The BETWEEN operator considers values that match */
/*              the bounds to satisfy the condition: the BETWEEN */
/*              operator tests for inclusion in the closed interval */
/*              defined by the bounds. */

/*              In the WHERE clause, simple relational expressions */
/*              may be combined using the logical operators AND, */
/*              OR, and NOT, as in the Fortran programming */
/*              language. Parentheses may be used to enforce a */
/*              desired order of evaluation of logical expressions. */

/*              The expression syntax is NOT symmetric: literal */
/*              values must not appear on the left hand side of the */
/*              operators that apply to them. */

/*              The columns named in a constraint clause must */
/*              belong to the tables listed in the FROM clause. */
/*              If the query is a join, qualifying table names or */
/*              aliases are required wherever their omission would */
/*              result in ambiguity. */

/*              Data types of the columns or constants used on the */
/*              right-hand-sides of operators must match the data */
/*              types of the corresponding columns on the */
/*              left-hand-sides, except that comparison of integer */
/*              and double precision quantities is permitted. */

/*              Literal strings used in constraints are always */
/*              bracketed by quotes. Either single  quotes (') */
/*              or double quotes (") may be used, but the same */
/*              quote character must be used to start and end any */
/*              literal string. Within character string values, */
/*              quote characters must be doubled in order to be */
/*              recognized. Case is significant in character */
/*              except in comparisons using the LIKE and NOT LIKE */
/*              operators, which ignore case: the expression */

/*                 ANIMAL LIKE "*A*" */

/*              would be considered true when ANIMAL takes the */
/*              value */

/*                 "cat" */

/*              Time values are considered to be strings and */
/*              require bracketing quotes. Currently, the */
/*              only time values allowed are UTC times in ISO */
/*              format, UTC times represented in forms accepted by */
/*              the SPICELIB routine TPARSE, and SCLK strings in */
/*              NAIF format. */

/*              The ORDER BY clause indicates which columns to */
/*              use to order the output generated by the query. */
/*              The columns in the ORDER BY clause define a */
/*              dictionary ordering, with the first listed column */
/*              acting as a primary key, the second column acting */
/*              as a secondary key, and so on. */

/*              For each ORDER BY column, the keywords ASC or DESC */
/*              may be supplied to indicate whether the items in */
/*              that column are to be listed in ascending or */
/*              descending order. Ascending order is the default. */
/*              The direction in which data items increase is */
/*              referred to as the `order sense'. */

/*              The ORDER BY clause, if present, must appear */
/*              last in the query. */

/*              The form of the ORDER BY clause is */

/*                 ORDER BY <column name> [<order sense>] */
/*                          [ ,<column name> [<order sense>]...] */

/*              Rows satisfying the query constraints will be */
/*              returned so that the entries of the first column */
/*              specified in the ORDER BY clause will be appear in */
/*              the order specified by the order sense keyword, */
/*              which is assumed to be ASC if absent. When entries */
/*              in the first through Nth ORDER BY column are equal, */
/*              the entries in the (N+1)st ORDER BY column */
/*              determine the order of the rows, and so on. */

/*              As in the WHERE clause, column names must be */
/*              qualified by table names or table aliases where */
/*              they would otherwise be ambiguous. */

/*              The query language is word-oriented, and some */
/*              indicate whether the words are reserved. Reserved */
/*              words must be separated from other words by white */
/*              space. It is not necessary to use white space */
/*              to separate words and punctuation characters. */
/*              The list of reserved words is */

/*                 AND */
/*                 BETWEEN */
/*                 BY */
/*                 COLUMN */
/*                 EQ */
/*                 FROM */
/*                 GE */
/*                 GT */
/*                 IS */
/*                 LE */
/*                 LT */
/*                 LIKE */
/*                 NE */
/*                 NOT */
/*                 NULL */
/*                 OR */
/*                 ORDER */
/*                 SELECT */
/*                 WHERE */

/*              The left and right parenthesis characters are also */
/*              reserved; they may not be used in queries outside */
/*              of quoted strings. */

/*              Case is not significant in queries, except within */
/*              literal strings. */

/* $ Detailed_Output */

/*     NMROWS   is the number of rows that match the query */
/*              criteria. NMROWS is defined if and only if */
/*              ERROR is returned .FALSE. */

/*     ERROR    is a logical flag indicating whether the query */
/*              failed to parse correctly. */

/*     ERRMSG   is a character string that describes EKFIND's */
/*              diagnosis of a parse error, should one occur. */
/*              Otherwise, ERRMSG will be returned blank. */

/* $ Parameters */

/*     See the include files. */

/* $ Exceptions */

/*     1)  Most of the exceptions that can occur on a call to */
/*         EKFIND are caused by errors in the input query. EKFIND */
/*         attempts to diagnose these via the output error flag and */
/*         error message, instead of signaling errors. The following */
/*         classes of errors are detected: */

/*            Scanning errors---these result from badly formed query */
/*            in which EKFIND could not identify all of the tokens. */
/*            When these errors occur, EKFIND may be too confused to */
/*            give a helpful diagnostic message. */

/*            Parsing errors---these result from a badly formed */
/*            query that EKFIND was able to separate into tokens */
/*            but that EKFIND determined to be syntactically invalid: */

/*            Name resolution errors---these result from referencing */
/*            invalid or ambiguous column or table names in a query. */

/*            Time resolution errors---these result from use of time */
/*            strings that cannot be parsed. */

/*            Semantic errors---these result from a syntactically */
/*            valid query that violates a limit or a restriction on */
/*            values used in a query. */


/*     Some problems with queries are not trapped by EKFIND but */
/*     instead cause errors to be signaled. These are listed below. */

/*     2)  If no E-kernels are loaded at the time this routine is called, */
/*         an error is signaled by a routine in the call tree of this */
/*         routine. */

/*     3)  If a leapseconds kernel is is not loaded before this routine */
/*         is called, UTC time values may not be used in queries. If they */
/*         are, an error is signaled by a routine in the call tree of */
/*         this routine. */

/*     4)  If an SCLK kernel for the appropriate spacecraft clock has not */
/*         been loaded before this routine is called, SCLK values for */
/*         that clock may not be used in queries. If they are, an error */
/*         is signaled by a routine in the call tree of this routine. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine operates almost entirely by side effects: it */
/*     prepares the EK fetch routines to return event data that */
/*     satisfy the input query. See the EK Required Reading for */
/*     examples of use of this routine in conjunction with the EK */
/*     fetch routines. */

/* $ Examples */

/*     The numerical results shown for these examples may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */

/*     1) Perform a query on an EK file that contains a database with */
/*        the different commands of the Deep Impact spacecraft */
/*        subsystem, and a table with the subsystem id, the parameter */
/*        name, and the description of that parameters, ordered by */
/*        subsystem name. Print the number of records of that table. */

/*        Use the EK kernel below to load the Deep Impact spacecraft */
/*        subsystem commands dictionary. */

/*           dif_cmdict_128_20050620.bdb */


/*        Example code begins here. */


/*              PROGRAM EKFIND_EX1 */
/*              IMPLICIT NONE */

/*        C */
/*        C     Include the EK Maximum length of an input query, */
/*        C     MAXQRY, and the maximum length of literal string */
/*        C     values, MAXSTR, from eklimit.inc. */
/*        C */
/*              INCLUDE 'ekqlimit.inc' */

/*        C */
/*        C     Local parameters */
/*        C */
/*              CHARACTER*(*)         EKNAME */
/*              PARAMETER           ( EKNAME = */
/*             .                         'dif_cmdict_128_20050620.bdb' ) */

/*              INTEGER               ERRLEN */
/*              PARAMETER           ( ERRLEN = 1840 ) */

/*        C */
/*        C     Local variables */
/*        C */
/*              CHARACTER*(ERRLEN)    ERRMSG */
/*              CHARACTER*(MAXQRY)    QUERY */

/*              INTEGER               NMROWS */

/*              LOGICAL               ERROR */

/*        C */
/*        C     Open an EK file. */
/*        C */
/*              CALL FURNSH ( EKNAME ) */

/*        C */
/*        C     The EK file contains the table 'DIF_COMMANDS', */
/*        C     and that 'DIF_COMMANDS' contains columns named: */
/*        C */
/*        C       SUBSYSTEM, COMMAND, PARAMETER_NAME, DESCRIPTION */
/*        C */
/*        C     Define a set of constraints to perform a query on all */
/*        C     loaded EK files (the SELECT clause). */
/*        C */
/*              QUERY = 'Select SUBSYSTEM, COMMAND, PARAMETER_NAME, ' */
/*             .   //   'DESCRIPTION from DIF_COMMANDS ' */
/*             .   //   'order by SUBSYSTEM' */

/*        C */
/*        C     Query the EK system for data rows matching the */
/*        C     SELECT constraints. */
/*        C */
/*              CALL EKFIND ( QUERY, NMROWS, ERROR, ERRMSG ) */

/*        C */
/*        C     Check whether an error occurred while processing the */
/*        C     SELECT clause. If so, output the error message. */
/*        C */
/*              IF ( ERROR ) THEN */

/*                 WRITE(*,*) 'SELECT clause error: ', ERRMSG */

/*              ELSE */

/*        C */
/*        C        If no error, NMROWS contains the number of rows */
/*        C        matching the constraints specified in the query */
/*        C        string. */
/*        C */
/*                 WRITE(*,*) 'Number of matching rows: ', NMROWS */

/*              END IF */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*         Number of matching rows:         5798 */


/*     2) Examples of strings containing syntactically valid queries: */

/*            SELECT COL1 FROM TAB1 */

/*            select col1 from tab1 where col1 gt 5 */

/*            SELECT COL2 FROM TAB1 WHERE COL2 > 5.7D0 ORDER BY COL2 */

/*            SELECT COL2 FROM TAB1 WHERE COL1 != 5 */

/*            SELECT COL2 FROM TAB1 WHERE COL1 GE COL2 */

/*            SELECT COL1, COL2, COL3 FROM TAB1 ORDER BY COL1 */

/*            SELECT COL3 FROM TAB1 WHERE COL5 EQ "ABC" */

/*            SELECT COL3 FROM TAB1 WHERE COL5 = 'ABC' */

/*            SELECT COL3 FROM TAB1 WHERE COL5 LIKE 'A*' */

/*            SELECT COL3 FROM TAB1 WHERE COL5 LIKE 'A%%' */

/*            SELECT COL4 FROM TAB1 WHERE COL4 = '1995 JAN 1 12:38:09.7' */

/*            SELECT COL4 FROM TAB1 WHERE COL4 = "1995 JAN 1 12:38:09.7" */

/*            SELECT COL4 FROM TAB1 WHERE */
/*            COL4 NE 'GLL SCLK 02724646:67:7:2' */

/*            SELECT COL1 FROM TAB1 WHERE COL1 != NULL */

/*            SELECT COL1 FROM TAB1 WHERE COL1 IS NULL */

/*            SELECT COL1 FROM TAB1 WHERE COL1 IS NOT NULL */

/*            SELECT COL1, COL2, COL3 FROM TAB1 */
/*            WHERE (COL1 BETWEEN 4 AND 6) AND (COL3 NOT LIKE "A%%") */
/*            ORDER BY COL1, COL3 */

/*            SELECT COL4 FROM TAB1 */
/*            WHERE COL4 BETWEEN "1995 JAN 1 12:38" AND */
/*            "October 23, 1995" */

/*            SELECT COL1, COL2 FROM TAB1 WHERE */
/*            NOT (    ( ( COL1 <  COL2 ) AND ( COL1 > 5   ) )  OR */
/*                     ( ( COL1 >= COL2 ) AND ( COL2 <= 10 ) )      ) */


/*            SELECT T1.COL1, T1.COL2, T2.COL2, T2.COL3 */
/*            FROM TABLE1 T1, TABLE2 T2 */
/*            WHERE T1.COL1 = T2.COL1 */
/*            AND T1.COL2 > 5 */
/*            ORDER BY T1.COL1, T2.COL2 */


/*     3) Examples of syntactically invalid queries: */

/*            SELECT TIME WHERE TIME */
/*            LT 1991 JAN 1                      {FROM clause is absent} */

/*            select time from table1 where */
/*            time lt 1991 jan 1                 {time string is not */
/*                                                quoted} */

/*            select time from table1 */
/*            where time .lt. '1991 jan 1'       {operator should be lt} */

/*            select cmd from table1 */
/*            where "cmd,6tmchg" != cmd          {value is on left side */
/*                                                of operator} */

/*            select event_type from table1 */
/*            where event_type eq ""             {quoted string is empty */
/*                                                ---use " " to indicate */
/*                                                a blank string} */

/*            select event_type from table1 */
/*            where event_type = "COMMENT" */
/*            order TIME                         {ORDER BY phrase is */
/*                                                lacking BY keyword} */

/*            select COL1 from table where */
/*            where COL1 eq MOC_EVENT            {literal string on */
/*                                                right-hand-side of */
/*                                                operator is not quoted} */



/*         In the following examples, we'll assume that the program */
/*         calling EKFIND has loaded an EK containing two segments */
/*         having columns having the following names and attributes: */


/*          TABLE1: */
/*          ========== */

/*            Column name        Data type         Size       Indexed? */
/*            -----------        ---------         ----       -------- */
/*            EVENT_TYPE         CHARACTER*32      1          YES */
/*            EVENT_PARAMETERS   CHARACTER*(*)     1          NO */
/*            COMMENT            CHARACTER*80      VARIABLE   NO */


/*          TABLE2: */
/*          ========== */

/*            Column name        Data type         Size       Indexed? */
/*            -----------        ---------         ----       -------- */
/*            EVENT_TYPE         CHARACTER*32      1          YES */
/*            EVENT_PARAMETERS   CHARACTER*80      1          NO */
/*            COMMENT            CHARACTER*80      VARIABLE   NO */
/*            COMMAND            CHARACTER*80      1          YES */


/*         Then the following queries are semantically invalid: */

/*            SELECT EVENT_PARAMETERS */
/*            FROM TABLE1 */
/*            WHERE EVENT_DURATION = 7.0         {No column called */
/*                                                EVENT_DURATION */
/*                                                is present in a loaded */
/*                                                EK} */

/*            SELECT COMMENT FROM TABLE2 */
/*            WHERE COMMENT EQ "N/A"             {The COMMENT column does */
/*                                                not have size 1 and */
/*                                                therefore cannot be */
/*                                                referenced in a query} */

/* $ Restrictions */

/*     1)  A leapseconds kernel must be loaded before this routine may */
/*         be called, if UTC time values are used in input queries. */

/*     2)  An appropriate SCLK kernel must be loaded before this routine */
/*         may be called, if SCLK values are used in input queries. */

/*     3)  Data found in response to a query become unavailable */
/*         when a fast load is initiated via EKIFLD. Any desired */
/*         fetches of the data must be performed before a fast */
/*         load or any other operation that modifies the EK scratch */
/*         area is initiated. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     B.V. Semenov       (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 13-AUG-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. */
/*        Added complete code example. */

/* -    SPICELIB Version 1.0.4, 18-MAY-2010 (BVS) */

/*        Removed "C$" marker from text in the header. */

/* -    SPICELIB Version 1.0.3, 19-DEC-2001 (NJB) */

/*        $Restrictions section was updated. */

/* -    SPICELIB Version 1.0.2, 14-JAN-1997 (NJB) */

/*        Syntax descriptions for comparisons using null values have been */
/*        added. The $Examples section was augmented with sample queries */
/*        demonstrating use of the IS NULL and IS NOT NULL comparison */
/*        operators. */

/* -    SPICELIB Version 1.0.1, 16-AUG-1996 (NJB) */

/*        $Exceptions section of header was updated to indicate that */
/*        calling this routine while no E-kernels are loaded will cause */
/*        an error to be signaled. Previous version line was changed */
/*        from "Beta" to "SPICELIB," and the previous version was */
/*        corrected to 1.0.0. */

/* -    SPICELIB Version 1.0.0, 24-OCT-1995 (NJB) */

/* -& */
/* $ Index_Entries */

/*     find EK data */
/*     issue EK query */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     Storage limits: */


/*     Local variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("EKFIND", (ftnlen)6);
    }

/*     Initialize the encoded query each time, for safety. */

    zzekqini_(&c__27869, &c__100, eqryi, eqryc, eqryd, (ftnlen)2000);

/*     Find the tokens in the input query. */

    zzekscan_(query, &c__500, &c__100, &ntoken, tokens, lxbegs, lxends, 
	    values, numvls, chrbuf, chbegs, chends, error, errmsg, query_len, 
	    (ftnlen)2000, errmsg_len);
    if (*error) {
	chkout_("EKFIND", (ftnlen)6);
	return 0;
    }

/*     Now parse the query. */

    zzekpars_(query, &ntoken, lxbegs, lxends, tokens, values, numvls, chrbuf, 
	    chbegs, chends, eqryi, eqryc, eqryd, error, errmsg, query_len, (
	    ftnlen)2000, (ftnlen)2000, errmsg_len);
    if (*error) {
	chkout_("EKFIND", (ftnlen)6);
	return 0;
    }

/*     Resolve names. */

    zzeknres_(query, eqryi, eqryc, error, errmsg, &errptr, query_len, (ftnlen)
	    2000, errmsg_len);
    if (*error) {
	chkout_("EKFIND", (ftnlen)6);
	return 0;
    }

/*     Resolve time values, if necessary. */

    zzektres_(query, eqryi, eqryc, eqryd, error, errmsg, &errptr, query_len, (
	    ftnlen)2000, errmsg_len);
    if (*error) {
	chkout_("EKFIND", (ftnlen)6);
	return 0;
    }

/*     Perform semantic checks. */

    zzeksemc_(query, eqryi, eqryc, error, errmsg, &errptr, query_len, (ftnlen)
	    2000, errmsg_len);
    if (*error) {
	chkout_("EKFIND", (ftnlen)6);
	return 0;
    }

/*     If we arrived here, the encoded query is ready for execution. */
/*     Find the data satisfying the constraints. */

    eksrch_(eqryi, eqryc, eqryd, nmrows, error, errmsg, (ftnlen)2000, 
	    errmsg_len);
    chkout_("EKFIND", (ftnlen)6);
    return 0;
} /* ekfind_ */

