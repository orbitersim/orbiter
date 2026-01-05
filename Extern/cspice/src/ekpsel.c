/* ekpsel.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__27869 = 27869;
static integer c__100 = 100;

/* $Procedure EKPSEL ( EK, parse SELECT clause ) */
/* Subroutine */ int ekpsel_(char *query, integer *n, integer *xbegs, integer 
	*xends, char *xtypes, char *xclass, char *tabs, char *cols, logical *
	error, char *errmsg, ftnlen query_len, ftnlen xtypes_len, ftnlen 
	xclass_len, ftnlen tabs_len, ftnlen cols_len, ftnlen errmsg_len)
{
    /* Initialized data */

    static char chrtyp[4*4] = "CHR " "DP  " "INT " "TIME";

    /* System generated locals */
    integer i__1, i__2;

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_rnge(char *, integer, char *, integer);

    /* Local variables */
    char qtab[64];
    extern /* Subroutine */ int zzekencd_(char *, integer *, char *, 
	    doublereal *, logical *, char *, integer *, ftnlen, ftnlen, 
	    ftnlen), zzekqtab_(integer *, char *, integer *, char *, char *, 
	    ftnlen, ftnlen, ftnlen), zzekqini_(integer *, integer *, integer *
	    , char *, doublereal *, ftnlen), zzekreqi_(integer *, char *, 
	    integer *, ftnlen), zzekqsel_(integer *, char *, integer *, 
	    integer *, integer *, char *, integer *, char *, integer *, 
	    ftnlen, ftnlen, ftnlen);
    integer i__;
    extern /* Subroutine */ int ekcii_(char *, integer *, char *, integer *, 
	    ftnlen, ftnlen), chkin_(char *, ftnlen);
    char eqryc[2000];
    doublereal eqryd[100];
    integer eqryi[27875];
    extern logical return_(void);
    char aka[64], column[32];
    integer attdsc[6], colidx, errptr, tabidx;
    extern /* Subroutine */ int chkout_(char *, ftnlen);

/* $ Abstract */

/*     Parse the SELECT clause of an EK query, returning full particulars */
/*     concerning each selected item. */

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

/*     None. */

/* $ Keywords */

/*     PRIVATE */

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


/*     Include Section:  EK Column Attribute Descriptor Parameters */

/*        ekattdsc.inc Version 1    23-AUG-1995 (NJB) */


/*     This include file declares parameters used in EK column */
/*     attribute descriptors.  Column attribute descriptors are */
/*     a simplified version of column descriptors:  attribute */
/*     descriptors describe attributes of a column but do not contain */
/*     addresses or pointers. */


/*     Size of column attribute descriptor */


/*     Indices of various pieces of attribute descriptors: */


/*     ATTSIZ is the index of the column's class code.  (We use the */
/*     word `class' to distinguish this item from the column's data */
/*     type.) */


/*     ATTTYP is the index of the column's data type code (CHR, INT, DP, */
/*     or TIME).  The type is actually implied by the class, but it */
/*     will frequently be convenient to look up the type directly. */



/*     ATTLEN is the index of the column's string length value, if the */
/*     column has character type.  A value of IFALSE in this element of */
/*     the descriptor indicates that the strings have variable length. */


/*     ATTSIZ is the index of the column's element size value.  This */
/*     descriptor element is meaningful for columns with fixed-size */
/*     entries.  For variable-sized columns, this value is IFALSE. */


/*     ATTIDX is the location of a flag that indicates whether the column */
/*     is indexed.  The flag takes the value ITRUE if the column is */
/*     indexed and otherwise takes the value IFALSE. */


/*     ATTNFL is the index of a flag indicating whether nulls are */
/*     permitted in the column.  The value at location NFLIDX is */
/*     ITRUE if nulls are permitted and IFALSE otherwise. */


/*     End Include Section:  EK Column Attribute Descriptor Parameters */

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


/*     Include Section:  EK Table Name Size */

/*        ektnamsz.inc Version 1    17-JAN-1995 (NJB) */


/*     Size of table name, in characters. */


/*     End Include Section:  EK Table Name Size */

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

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     QUERY      I   EK query. */
/*     N          O   Number of items in SELECT clause of QUERY. */
/*     XBEGS      O   Begin positions of expressions in SELECT clause. */
/*     XENDS      O   End positions of expressions in SELECT clause. */
/*     XTYPES     O   Data types of expressions. */
/*     XCLASS     O   Classes of expressions. */
/*     TABS       O   Names of tables qualifying SELECT columns. */
/*     COLS       O   Names of columns in SELECT clause of QUERY. */
/*     ERROR      O   Error flag. */
/*     ERRMSG     O   Parse error message. */

/* $ Detailed_Input */

/*     QUERY    is a character string containing an EK query. */
/*              EK queries have the general form */

/*                 SELECT <select expr>, <select expr>, ... */
/*                 FROM <table spec>, <table spec>, ... */
/*                 [WHERE <constraint list>] */
/*                 [ORDER BY <order-by column list>] */

/*              Here the symbol <select expr> indicates any */
/*              expression representing an entity that can be */
/*              selected. Commonly, the selected items are */
/*              columns, with or without qualifying table names, */
/*              having the form */

/*                 <column name> */
/*                 <table name>.<column name> */
/*                 <table alias>.<column name> */

/*              but more general expressions may also be selected. */
/*              Examples are functions, such as */

/*                 COUNT(*) */
/*                 COUNT( <table name>.<column name> ) */
/*                 MAX  ( <table name>.<column name> ) */

/*              or expressions involving constants, such as */

/*                 2 * <column name> */

/* $ Detailed_Output */

/*     N        is the number of items specified in the */
/*              SELECT clause of the input query. */

/*     XBEGS, */
/*     XENDS    are, respectively, arrays of begin and end */
/*              positions of expressions designating items in the */
/*              SELECT clause of the input query. The Ith */
/*              expression is located in the substring */

/*                 QUERY ( XBEGS(I) : XENDS(I) ) */

/*     XTYPES   is an array of short strings indicating the data */
/*              types of the expressions in the SELECT clause. */
/*              Values and meanings of XTYPES are: */

/*                 'CHR'        Character type */
/*                 'DP'         Double precision type */
/*                 'INT'        Integer type */
/*                 'TIME'       Time type */

/*              The Ith element of XTYPES refers to the Ith */
/*              selected item. */

/*              The data type of an expression indicates which */
/*              fetch routine to use to obtain values of the */
/*              selected expression. The mapping of data types */
/*              to fetch routines is shown below: */

/*                 'CHR'        EKGC */
/*                 'DP'         EKGD */
/*                 'INT'        EKGI */
/*                 'TIME'       EKGD */

/*              Note that time values are stored as d.p. numbers. */

/*     XCLASS   is an array of short strings giving the classes */
/*              of the expressions occurring in the SELECT clause */
/*              of the input query. Values and meanings of */
/*              XCLASS are: */

/*                 'COL'        Selected item was a column. The */
/*                              column may qualified. */

/*                 'FUNC'       Selected item was a simple */
/*                              function invocation of the form */

/*                                 F ( <column> ) */

/*                              or else was */

/*                                 COUNT(*) */

/*                 'EXPR'       Selected item was a more general */
/*                              expression than those shown above. */

/*              The Ith element of XCLASS refers to the Ith */
/*              selected item. */

/*              When a selected item is a column, the values of */
/*              the arguments TABS and COLS (discussed below) are */
/*              defined. */

/*     TABS     is an array of names of tables corresponding to */
/*              the columns in the SELECT clause. The Ith element */
/*              of TABS corresponds to the table containing the */
/*              Ith SELECT column. Table names returned in TABS */
/*              are the actual names of tables in loaded EK, not */
/*              aliases supplied in the input query. Table names */
/*              are supplied even if the corresponding column was */
/*              unqualified in the input query, as long as the */
/*              column name was unambiguous. */

/*              The contents of TABS(I) are defined if and only if */
/*              the returned value of XCLASS(I) is 'COL'. */

/*     COLS     is an array containing the columns of the SELECT */
/*              clause. The contents of COLS(I) are defined if and */
/*              only if the returned value of XCLASS(I) is 'COL'. */

/*     ERROR    is a logical flag indicating whether the input */
/*              QUERY parsed correctly. The other outputs of this */
/*              routine, except for ERRMSG, are undefined if a */
/*              parse error occurred. ERROR is returned .TRUE. if */
/*              a parse error occurred, .FALSE. otherwise. */

/*     ERRMSG   is a character string describing the cause of a */
/*              parse error, if such an error occurred. Otherwise, */
/*              ERRMSG is returned blank. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  Parse failures do not cause this routine to signal errors; */
/*         instead, the ERROR and ERRMSG outputs indicate invalid */
/*         QUERY. */

/*     2)  Queries cannot be parsed correctly unless at least one EK */
/*         is loaded. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine allows callers of the EK fetch routines to determine */
/*     at run time the attributes of the columns from which data is to be */
/*     fetched. */

/* $ Examples */

/*     The numerical results shown for this example may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */

/*     1) Query the EK system and fetch data matching that query. */

/*        The program shown here does not rely on advance */
/*        knowledge of the input query or the contents of any loaded EK */
/*        files. */

/*        To simplify the example, we assume that all data are scalar. */
/*        This assumption relieves us of the need to test the size of */
/*        column entries before fetching them. In the event that a */
/*        column contains variable-size array entries, the entry point */
/*        EKNELT may be called to obtain the size of column entries to */
/*        be fetched. See EKNELT for an example. */


/*        Use the EK kernel below to load the information from the */
/*        original Supplementary Engineering Data Record (SEDR) data */
/*        set generated by the Viking Project. */

/*           vo_sedr.bdb */

/*        Use the LSK kernel below to load the leap seconds and time */
/*        constants required for the conversions. */

/*           naif0012.tls */


/*        Example code begins here. */


/*              PROGRAM EKPSEL_EX1 */
/*              IMPLICIT NONE */

/*        C */
/*        C     Include EK Query Limit Parameters */
/*        C */
/*              INCLUDE 'ekqlimit.inc' */

/*        C */
/*        C     SPICELIB functions */
/*        C */
/*              INTEGER               RTRIM */

/*        C */
/*        C     Local parameters */
/*        C */
/*              CHARACTER*(*)         EKNAME */
/*              PARAMETER           ( EKNAME = 'vo_sedr.bdb' ) */

/*              CHARACTER*(*)         LSKNAM */
/*              PARAMETER           ( LSKNAM = 'naif0012.tls' ) */

/*              INTEGER               DESCSZ */
/*              PARAMETER           ( DESCSZ = 31   ) */

/*              INTEGER               ERRLEN */
/*              PARAMETER           ( ERRLEN = 1840 ) */

/*              INTEGER               ITEMSZ */
/*              PARAMETER           ( ITEMSZ = DESCSZ + 4 ) */

/*              INTEGER               TIMLEN */
/*              PARAMETER           ( TIMLEN = 27   ) */

/*              INTEGER               TYPLEN */
/*              PARAMETER           ( TYPLEN = 4    ) */

/*              INTEGER               XCLSLN */
/*              PARAMETER           ( XCLSLN = 4    ) */

/*        C */
/*        C     Local variables */
/*        C */
/*              CHARACTER*(MAXSTR)    CDATA */
/*              CHARACTER*(MAXCLN)    COLS   ( MAXSEL ) */
/*              CHARACTER*(ERRLEN)    ERRMSG */
/*              CHARACTER*(ITEMSZ)    ITEM */
/*              CHARACTER*(DESCSZ)    OUTSTR */
/*              CHARACTER*(MAXQRY)    QUERY */
/*              CHARACTER*(TIMLEN)    UTCSTR */
/*              CHARACTER*(MAXCLN)    TABS   ( MAXTAB ) */
/*              CHARACTER*(XCLSLN)    XCLASS ( MAXSEL ) */
/*              CHARACTER*(TYPLEN)    XTYPES ( MAXSEL ) */

/*              DOUBLE PRECISION      DDATA */
/*              DOUBLE PRECISION      TDATA */

/*              INTEGER               B */
/*              INTEGER               COLNO */
/*              INTEGER               E */
/*              INTEGER               HANDLE */
/*              INTEGER               IDATA */
/*              INTEGER               N */
/*              INTEGER               NMROWS */
/*              INTEGER               ROW */
/*              INTEGER               XBEGS  ( MAXSEL ) */
/*              INTEGER               XENDS  ( MAXSEL ) */

/*              LOGICAL               ERROR */
/*              LOGICAL               FOUND */
/*              LOGICAL               NULL */

/*        C */
/*        C     Load leapseconds file for time conversion. */
/*        C */
/*              CALL FURNSH ( LSKNAM ) */

/*        C */
/*        C     Load EK. */
/*        C */
/*              CALL EKLEF  ( EKNAME, HANDLE ) */

/*        C */
/*        C     Setup the query.  Parse the SELECT clause using */
/*        C     EKPSEL. */
/*        C */
/*              QUERY = 'Select IMAGE_NUMBER, IMAGE_ID, ' */
/*             .   //          'PLATFORM_CLOCK, IMAGE_TIME ' */
/*             .   //   'from VIKING_SEDR_DATA ' */
/*             .   //   'where IMAGE_NUMBER < 25850000 ' */
/*             .   //   'order by IMAGE_NUMBER' */

/*              CALL EKPSEL ( QUERY,  N,    XBEGS, XENDS, XTYPES, */
/*             .              XCLASS, TABS, COLS,  ERROR, ERRMSG ) */

/*              IF ( ERROR ) THEN */

/*                 WRITE(*,*) ERRMSG */

/*              ELSE */

/*        C */
/*        C        Submit query to the EK query system. */
/*        C */
/*                 CALL EKFIND ( QUERY, NMROWS, ERROR, ERRMSG ) */

/*                 IF ( ERROR ) THEN */

/*                    WRITE(*,*) ERRMSG */

/*                 ELSE */

/*        C */
/*        C           Fetch the rows that matched the query. */
/*        C */
/*                    DO ROW = 1, NMROWS */

/*        C */
/*        C              Fetch data from the Ith row. */
/*        C */
/*                       WRITE (*,*) ' ' */
/*                       WRITE (*,*) 'ROW = ', ROW */

/*                       DO COLNO = 1, N */

/*        C */
/*        C                 Fetch the data from the Jth selected */
/*        C                 column. */
/*        C */
/*                          IF ( XCLASS(COLNO) .EQ. 'COL' ) THEN */

/*                             OUTSTR  =  COLS(COLNO) */
/*                             CALL PREFIX ( '.',         0, OUTSTR ) */
/*                             CALL PREFIX ( TABS(COLNO), 0, OUTSTR ) */
/*                             ITEM = '  ' // OUTSTR // ':' */

/*                          ELSE */

/*                             B  =  XBEGS(COLNO) */
/*                             E  =  XENDS(COLNO) */
/*                             ITEM = '  ITEM = ' // QUERY(B:E) */

/*                          END IF */

/*                          IF ( XTYPES(COLNO) .EQ. 'CHR' ) THEN */

/*                             CALL EKGC ( COLNO,  ROW,  1, */
/*             .                           CDATA, NULL, FOUND ) */

/*                             IF ( NULL ) THEN */
/*                                WRITE(*,*) ITEM, '<Null>' */
/*                             ELSE */
/*                                WRITE(*,*) ITEM, CDATA(:RTRIM(CDATA)) */
/*                             END IF */


/*                          ELSE IF ( XTYPES(COLNO) .EQ. 'DP' ) THEN */

/*                             CALL EKGD ( COLNO,  ROW,  1, */
/*             .                           DDATA, NULL, FOUND ) */

/*                             IF ( NULL ) THEN */
/*                                WRITE(*,*) ITEM, '<Null>' */
/*                             ELSE */
/*                                WRITE(*,*) ITEM, DDATA */
/*                             END IF */


/*                          ELSE IF ( XTYPES(COLNO) .EQ. 'INT' ) THEN */

/*                             CALL EKGI ( COLNO,  ROW,  1, */
/*             .                           IDATA, NULL, FOUND ) */

/*                             IF ( NULL ) THEN */
/*                                WRITE(*,*) ITEM, '<Null>' */
/*                             ELSE */
/*                                WRITE(*,*) ITEM, IDATA */
/*                             END IF */


/*                          ELSE */
/*        C */
/*        C                    The item is a time value.  Convert it */
/*        C                    to UTC for output. */
/*        C */
/*                             CALL EKGD ( COLNO,  ROW,  1, */
/*             .                           TDATA, NULL, FOUND ) */

/*                             IF ( NULL ) THEN */
/*                                WRITE(*,*) ITEM, '<Null>' */
/*                             ELSE */
/*                                CALL ET2UTC ( TDATA, 'C', 3, UTCSTR ) */
/*                                WRITE(*,*) ITEM, UTCSTR */
/*                             END IF */

/*                          END IF */

/*        C */
/*        C              We're done with the column having index COLNO. */
/*        C */
/*                       END DO */

/*        C */
/*        C           We're done with the row having index ROW. */
/*        C */
/*                    END DO */

/*        C */
/*        C        We either processed the query or had an error. */
/*        C */
/*                 END IF */

/*        C */
/*        C     We either parsed the SELECT clause or had an error. */
/*        C */
/*              END IF */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*         ROW =            1 */
/*           VIKING_SEDR_DATA.IMAGE_NUMBER  :     25837050 */
/*           VIKING_SEDR_DATA.IMAGE_ID      : 168C09 */
/*           VIKING_SEDR_DATA.PLATFORM_CLOCK:    119.88000000000000 */
/*           VIKING_SEDR_DATA.IMAGE_TIME    : 1976 JUN 16 16:50:55.925 */

/*         ROW =            2 */
/*           VIKING_SEDR_DATA.IMAGE_NUMBER  :     25837051 */
/*           VIKING_SEDR_DATA.IMAGE_ID      : 168C10 */
/*           VIKING_SEDR_DATA.PLATFORM_CLOCK:    119.27000000000000 */
/*           VIKING_SEDR_DATA.IMAGE_TIME    : 1976 JUN 16 16:51:00.269 */

/*         ROW =            3 */
/*           VIKING_SEDR_DATA.IMAGE_NUMBER  :     25840344 */
/*           VIKING_SEDR_DATA.IMAGE_ID      : 168C11 */
/*           VIKING_SEDR_DATA.PLATFORM_CLOCK:    119.88000000000000 */
/*           VIKING_SEDR_DATA.IMAGE_TIME    : 1976 JUN 16 20:56:53.051 */

/*         ROW =            4 */
/*           VIKING_SEDR_DATA.IMAGE_NUMBER  :     25840345 */
/*           VIKING_SEDR_DATA.IMAGE_ID      : 168C12 */
/*           VIKING_SEDR_DATA.PLATFORM_CLOCK:    119.27000000000000 */
/*           VIKING_SEDR_DATA.IMAGE_TIME    : 1976 JUN 16 20:56:57.395 */

/*         ROW =            5 */
/*           VIKING_SEDR_DATA.IMAGE_NUMBER  :     25843638 */
/*           VIKING_SEDR_DATA.IMAGE_ID      : 169C01 */
/*           VIKING_SEDR_DATA.PLATFORM_CLOCK:    119.88000000000000 */
/*           VIKING_SEDR_DATA.IMAGE_TIME    : 1976 JUN 17 01:02:50.177 */

/*         ROW =            6 */
/*           VIKING_SEDR_DATA.IMAGE_NUMBER  :     25843639 */
/*           VIKING_SEDR_DATA.IMAGE_ID      : 169C02 */
/*           VIKING_SEDR_DATA.PLATFORM_CLOCK:    119.27000000000000 */
/*           VIKING_SEDR_DATA.IMAGE_TIME    : 1976 JUN 17 01:02:54.521 */

/*         ROW =            7 */
/*           VIKING_SEDR_DATA.IMAGE_NUMBER  :     25846934 */
/*           VIKING_SEDR_DATA.IMAGE_ID      : 169C03 */
/*           VIKING_SEDR_DATA.PLATFORM_CLOCK:    120.14000000000000 */
/*           VIKING_SEDR_DATA.IMAGE_TIME    : 1976 JUN 17 05:08:56.263 */

/*         ROW =            8 */
/*           VIKING_SEDR_DATA.IMAGE_NUMBER  :     25846935 */
/*           VIKING_SEDR_DATA.IMAGE_ID      : 169C04 */
/*           VIKING_SEDR_DATA.PLATFORM_CLOCK:    119.52000000000000 */
/*           VIKING_SEDR_DATA.IMAGE_TIME    : 1976 JUN 17 05:09:00.607 */

/*         ROW =            9 */
/*           VIKING_SEDR_DATA.IMAGE_NUMBER  :     25848026 */
/*           VIKING_SEDR_DATA.IMAGE_ID      : 169C05 */
/*           VIKING_SEDR_DATA.PLATFORM_CLOCK:    120.14000000000000 */
/*           VIKING_SEDR_DATA.IMAGE_TIME    : 1976 JUN 17 06:30:28.424 */

/*         ROW =           10 */
/*           VIKING_SEDR_DATA.IMAGE_NUMBER  :     25848030 */
/*           VIKING_SEDR_DATA.IMAGE_ID      : 169C09 */
/*           VIKING_SEDR_DATA.PLATFORM_CLOCK:    120.14000000000000 */
/*           VIKING_SEDR_DATA.IMAGE_TIME    : 1976 JUN 17 06:30:46.174 */

/*         ROW =           11 */
/*           VIKING_SEDR_DATA.IMAGE_NUMBER  :     25848032 */
/*           VIKING_SEDR_DATA.IMAGE_ID      : 169C11 */
/*           VIKING_SEDR_DATA.PLATFORM_CLOCK:    120.14000000000000 */
/*           VIKING_SEDR_DATA.IMAGE_TIME    : 1976 JUN 17 06:30:55.168 */


/* $ Restrictions */

/*     1)  Currently, column names are the only supported expressions. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 06-JUL-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. Added complete */
/*        code example based on existing fragment. */

/* -    SPICELIB Version 1.0.0, 19-DEC-1995 (NJB) */

/* -& */
/* $ Index_Entries */

/*     parse select clause of EK query */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     Local variables */


/*     Saved values */


/*     Initial values */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("EKPSEL", (ftnlen)6);
    }

/*     Initialize the encoded query each time, for safety. */

    zzekqini_(&c__27869, &c__100, eqryi, eqryc, eqryd, (ftnlen)2000);

/*     Encode the input query. */

    zzekencd_(query, eqryi, eqryc, eqryd, error, errmsg, &errptr, query_len, (
	    ftnlen)2000, errmsg_len);
    if (*error) {
	chkout_("EKPSEL", (ftnlen)6);
	return 0;
    }

/*     Look up the number of SELECT columns.  For each column, look up */
/*     the parent table, the alias, and the column's name. */

    zzekreqi_(eqryi, "NUM_SELECT_COLS", n, (ftnlen)15);
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	zzekqsel_(eqryi, eqryc, &i__, &xbegs[i__ - 1], &xends[i__ - 1], qtab, 
		&tabidx, cols + (i__ - 1) * cols_len, &colidx, (ftnlen)2000, (
		ftnlen)64, cols_len);

/*        Make the table index to the table name. */

	zzekqtab_(eqryi, eqryc, &tabidx, tabs + (i__ - 1) * tabs_len, aka, (
		ftnlen)2000, tabs_len, (ftnlen)64);

/*        Currently, every expression is a column. */

	s_copy(xclass + (i__ - 1) * xclass_len, "COL", xclass_len, (ftnlen)3);

/*        Look up the data type of the column. */

	ekcii_(tabs + (i__ - 1) * tabs_len, &colidx, column, attdsc, tabs_len,
		 (ftnlen)32);
	s_copy(xtypes + (i__ - 1) * xtypes_len, chrtyp + (((i__2 = attdsc[1] 
		- 1) < 4 && 0 <= i__2 ? i__2 : s_rnge("chrtyp", i__2, "ekpse"
		"l_", (ftnlen)685)) << 2), xtypes_len, (ftnlen)4);
    }
    chkout_("EKPSEL", (ftnlen)6);
    return 0;
} /* ekpsel_ */

