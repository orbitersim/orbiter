/* zzekpars.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__27869 = 27869;
static integer c__100 = 100;
static integer c__1 = 1;
static integer c__11 = 11;
static integer c__6 = 6;
static integer c__27 = 27;
static integer c__29 = 29;
static integer c__26 = 26;
static integer c__0 = 0;
static integer c__10 = 10;
static integer c__50 = 50;

/* $Procedure      ZZEKPARS ( EK, parse tokenized EK query ) */
/* Subroutine */ int zzekpars_(char *query, integer *ntoken, integer *lxbegs, 
	integer *lxends, integer *tokens, integer *values, doublereal *numvls,
	 char *chrbuf, integer *chbegs, integer *chends, integer *eqryi, char 
	*eqryc, doublereal *eqryd, logical *error, char *prserr, ftnlen 
	query_len, ftnlen chrbuf_len, ftnlen eqryc_len, ftnlen prserr_len)
{
    /* System generated locals */
    integer i__1;

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_rnge(char *, integer, char *, integer), s_cmp(char *, char *, 
	    ftnlen, ftnlen);

    /* Local variables */
    integer nsel;
    extern /* Subroutine */ int zzekinqc_(char *, integer *, integer *, 
	    integer *, integer *, char *, integer *, ftnlen, ftnlen), 
	    zzekqini_(integer *, integer *, integer *, char *, doublereal *, 
	    ftnlen), zzektloc_(integer *, integer *, integer *, integer *, 
	    integer *, integer *, logical *), zzekweqi_(char *, integer *, 
	    integer *, ftnlen), zzeknrml_(char *, integer *, integer *, 
	    integer *, integer *, integer *, doublereal *, char *, integer *, 
	    integer *, integer *, char *, doublereal *, logical *, char *, 
	    ftnlen, ftnlen, ftnlen, ftnlen);
    integer b, e, i__, j, l;
    extern integer cardi_(integer *);
    extern /* Subroutine */ int chkin_(char *, ftnlen), repmc_(char *, char *,
	     char *, char *, ftnlen, ftnlen, ftnlen, ftnlen);
    integer ntabs;
    extern /* Subroutine */ int repmi_(char *, char *, integer *, char *, 
	    ftnlen, ftnlen, ftnlen);
    integer state, token;
    extern /* Subroutine */ int movei_(integer *, integer *, integer *);
    extern logical failed_(void);
    integer tabdsc[6];
    extern logical return_(void);
    char errtyp[32], expkey[32];
    integer alsdsc[6], coldsc[6], lxb, lxe, namdsc[6], ncnstr, norder, toknum,
	     valdsc[6];
    logical fnd;
    extern /* Subroutine */ int chkout_(char *, ftnlen), appndi_(integer *, 
	    integer *), cleari_(integer *, integer *);

/* $ Abstract */

/*     Parse an EK query that has been scanned and tokenized. */
/*     Represent the result as an encoded EK query. */

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


/*     Include Section:  EK Token Code Parameters */

/*        ektokn.inc  Version 2    25-JAN-1995 (NJB) */

/*           Updated to distinguish between special characters. */


/*        ektokn.inc  Version 1    05-DEC-1994 (NJB) */


/*     The EK query language tokens and codes are: */

/*        <keyword> */
/*        <identifier> */
/*        <integer> */
/*        <d.p. number> */
/*        <quoted string> */
/*        <left parenthesis> */
/*        <right parenthesis> */
/*        <comma> */
/*        <period> */
/*        <end of query> */



/*     End Include Section:  EK Token Code Parameters */

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


/*     Include Section:  EK Keyword Code Parameters */

/*        ekkeyw.inc  Version 4    24-JAN-1995 (NJB) */



/*     The EK query language keywords and codes are: */

/*        ALL */
/*        AND */
/*        ASC */
/*        AVG */
/*        BETWEEN */
/*        BY */
/*        COUNT */
/*        DESC */
/*        DISTINCT */
/*        EQ */
/*        FROM */
/*        GE */
/*        GROUP */
/*        GT */
/*        HAVING */
/*        IS */
/*        LE */
/*        LT */
/*        LIKE */
/*        MAX */
/*        MIN */
/*        NE */
/*        NOT */
/*        NULL */
/*        OR */
/*        ORDER */
/*        SELECT */
/*        SUM */
/*        WHERE */


/*     End Include Section:  EK Keyword Code Parameters */

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


/*     Include Section:  EK Boolean Enumerated Type */


/*        ekbool.inc Version 1   21-DEC-1994 (NJB) */


/*     Within the EK system, boolean values sometimes must be */
/*     represented by integer or character codes.  The codes and their */
/*     meanings are listed below. */

/*     Integer code indicating `true': */


/*     Integer code indicating `false': */


/*     Character code indicating `true': */


/*     Character code indicating `false': */


/*     End Include Section:  EK Boolean Enumerated Type */

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

/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     QUERY      I   Query in string form. */
/*     NTOKEN     I   Number of tokens in query. */
/*     LXBEGS, */
/*     LXENDS     I   Lexeme begin and end positions in QUERY. */
/*     TOKENS     I   Token codes. */
/*     VALUES     I   Values associated with tokens. */
/*     NUMVLS     I   Buffer containing numeric token values. */
/*     CHRBUF     I   Buffer containing string token values. */
/*     CHBEGS, */
/*     CHENDS     I   String token begin and end character positions. */
/*     EQRYI, */
/*     EQRYC, */
/*     EQRYD      O   Parsed query and string and number value buffers. */
/*     ERROR      O   Flag indicating whether query parsed correctly. */
/*     PRSERR     O   Parse error description. */

/* $ Detailed_Input */

/*     QUERY          is a string containing the original input query. */
/*                    QUERY is used only for creating error messages. */

/*     NTOKEN         is the number of tokens in the input query. */

/*     LXBEGS, */
/*     LXENDS         are lexeme begin and end pointers; the Ith */
/*                    lexeme in the query is */

/*                       QUERY ( LXBEGS(I) : LXENDS(I) ) */

/*                    (Lexemes are strings that correspond to tokens */
/*                     in the language.) */

/*     TOKENS         is an array of token codes.  The Ith element of */
/*                    TOKENS represents the Ith token in the scanned */
/*                    query. */

/*     VALUES         is an array of values associated with tokens; the */
/*                    Ith element of VALUES corresponds to the Ith */
/*                    token.  Keywords, for example, are distinguished */
/*                    by codes in the VALUES array.  Literal numeric */
/*                    and string tokens use the VALUES array to point */
/*                    to elements of NUMVLS or CHBEGS and CHENDS, */
/*                    respectively.  Some tokens don't need to use */
/*                    VALUES, but to simplify indexing, each token gets */
/*                    an element of this array. */

/*     NUMVLS         is an array of double precision numbers used to */
/*                    store the values corresponding to literal numeric */
/*                    tokens. */

/*     CHRBUF         is a string used to store the values of literal */
/*                    string tokens. */

/*     CHBEGS, */
/*     CHENDS         are pairs of begin and end pointers into CHRBUF. */
/*                    These pointers delimit character values */
/*                    associated with literal string tokens. */


/* $ Detailed_Output */

/*     EQRYI, */
/*     EQRYC, */
/*     EQRYD          are the integer, character, and numeric portions */
/*                    of an encoded form of the input query.  The */
/*                    SELECT, FROM, WHERE, and ORDER BY clauses of the */
/*                    input query are all represented in this encoding. */
/*                    WHERE clause constraints have been normalized. */

/*                    Normalized queries have their constraints grouped */
/*                    into a disjunction of conjunctions of relational */
/*                    expressions, as symbolized below: */

/*                          ( <rel_exp_1_1> and <rel_exp_1_2> and ... ) */
/*                       or ( <rel_exp_2_1> and <rel_exp_2_2> and ... ) */
/*                                     . */
/*                                     . */
/*                                     . */
/*                       or ( <rel_exp_N_1> and <rel_exp_N_2> and ... ) */

/*     ERROR, */
/*     PRSERR         are, respectively, a flag indicating whether the */
/*                    input query parsed correctly, and a message */
/*                    describing the parse error, if one occurred.  If */
/*                    no error occurred, ERROR is .FALSE. and PRSERR */
/*                    is blank. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  Most of the exceptions that can occur on a call to */
/*         ZZEKPARS are caused by errors in the input query.  ZZEKPARS */
/*         attempts to diagnose these via the output error flag and */
/*         error message, instead of signaling errors.  The */
/*         error messages that ZZEKPARS can return are listed below. */
/*         In the messages shown, the symbol # is used to designate */
/*         a marker for which a value can be substituted in an actual */
/*         message. */


/*            The BY keyword was not found following the */
/*            ORDER keyword. */

/*            Invalid keyword at location #. */
/*            Actual token was: # */

/*            Table or column name expected at location */
/*            #. Actual token was: # */

/*            Table name expected at location #. */
/*            Actual token was: # */

/*            Column name expected at location #. */
/*            Actual token was: # */

/*            Table alias, comma, or keyword expected at */
/*            location #. Actual token was: # */

/*            Comma or keyword expected at */
/*            location #. Actual token was: # */

/*            Comma expected at location #. Actual token was: # */

/*            PRSERR  =  More tokens were expected in query. */

/*            The keyword # was expected at location */
/*            #. Actual token was: # */

/*            Invalid token at location #. Token was: # */

/*            PRSERR  =  Number of tables in "FROM" clause exceeds */
/*            allowed maximum of #. */

/*            PRSERR  =  Number of order-by columns exceeds allowed */
/*            maximum of #. */

/*            PRSERR  =  Number of SELECT columns exceeds allowed */
/*            maximum of #. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine determines whether a query is syntactically correct; */
/*     it transforms correct queries into the EK system's encoded query */
/*     representation. */

/*     The encoded queries output by this routine are not ready for */
/*     execution; they still must undergo name resolution, time value */
/*     conversion, and semantic checking.  See EKFIND for an example of */
/*     the normal sequence of query processing. */

/* $ Examples */

/*     See the header of EKFIND for examples of valid and invalid */
/*     queries. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman   (JPL) */

/* $ Version */

/* -    SPICELIB Version 4.1.1, 03-OCT-2021 (NJB) */

/*        Corrected typos in comments. */

/* -    SPICELIB Version 4.1.0, 15-OCT-1996 (NJB) */

/*        Bug fix: default order sense was not encoded when ORDER-BY */
/*        clause was not the last clause of the query. */

/* -    SPICELIB Version 4.0.0, 17-NOV-1995 (NJB) */

/*        Complete re-write for architecture 3. */

/* -& */
/* $ Index_Entries */

/*     parse EK query */

/* -& */
/* $ Revisions */

/* -    SPICELIB Version 4.1.0, 15-OCT-1996 (NJB) */

/*        Bug fix: default order sense was not encoded when ORDER-BY */
/*        clause was not the last clause of the query.  The old algorithm */
/*        assumed that no clauses followed the ORDER-BY clause, which */
/*        at one time was a limitation of the EK query language. */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     State parameters */


/*     Other local parameters */


/*     Local variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("ZZEKPARS", (ftnlen)8);
    }

/*     Initialize the encoded query each time, for safety. */

    zzekqini_(&c__27869, &c__100, eqryi, eqryc, eqryd, eqryc_len);
    if (failed_()) {
	*error = TRUE_;
	s_copy(prserr, "SPICE(BUG):  encoded query init failed.", prserr_len, 
		(ftnlen)39);
	chkout_("ZZEKPARS", (ftnlen)8);
	return 0;
    }

/*     The structure of a query is */

/*        <QUERY>                 =>    <SELECT clause> <FROM clause> */
/*                                      <WHERE clause> <ORDER BY clause> */

/*        <SELECT clause>         =>    SELECT <select list> */

/*        <select list>           =>    <column entry> */
/*                                    | <select list>, <column entry> */

/*        <column entry>          =>    <table name>.<column name> */
/*                                    | <column name> */

/*        <FROM clause>           =>    <table name list> */

/*        <table name list>       =>    <table entry> */
/*                                    | <table name list>, <table entry> */

/*        <table entry>           =>    <table name> */
/*                                    | <table name> <table alias> */

/*        <WHERE clause>          =>    WHERE <constraint expression> */
/*                                    | <NIL> */

/*        <ORDER BY clause>       =>    ORDER BY <order-by list> */
/*                                    | <NIL> */

/*        <order-by list>         =>    <order-by column entry> */
/*                                    | <order-by list>, */
/*                                      <order-by column entry> */

/*        <order-by column entry> =>    <column entry> <order> */

/*        <order>                 =>    ASC */
/*                                    | DESC */
/*                                    | <NIL> */



/*     We'll parse the clauses of the query in the following order: */

/*        FROM */
/*        WHERE     (if present) */
/*        ORDER BY  (if present) */
/*        SELECT */


    zzektloc_(&c__1, &c__11, ntoken, tokens, values, &toknum, &fnd);
    if (! fnd) {
	*error = TRUE_;
	s_copy(errtyp, "FROM_NOT_FOUND", (ftnlen)32, (ftnlen)14);
	state = 16;
    } else {
	state = 0;
	ntabs = 0;
	nsel = 0;
	ncnstr = 0;
	norder = 0;
	*error = FALSE_;
	s_copy(prserr, " ", prserr_len, (ftnlen)1);
	s_copy(errtyp, " ", (ftnlen)32, (ftnlen)1);
    }
    while(state != 16) {

/*        Advance to the next token, if there is one. */

	++toknum;
	if (toknum > *ntoken) {

/*           We're out of tokens.  Set the token value to indicate */
/*           `end of query'. */

	    token = 11;
	} else {
	    token = tokens[toknum - 1];
	}

/*        Perform semantic actions based on the state and current token. */

	if (state == 0) {

/*           We expect to see an identifier representing a table name. */
/*           No other tokens are allowed. */

	    if (token == 2) {

/*              We've found a table name (as far as we can tell at */
/*              this point).  Make sure we haven't exceeded the limit */
/*              for table names; if not, add the appropriate information */
/*              to the encoded query. */

		++ntabs;
		if (ntabs > 10) {
		    *error = TRUE_;
		    s_copy(errtyp, "TOO_MANY_TABLES", (ftnlen)32, (ftnlen)15);
		    state = 16;
		} else {
		    i__ = values[toknum - 1];
		    b = chbegs[i__ - 1];
		    e = chends[i__ - 1];
		    l = e - b + 1;
		    lxb = lxbegs[toknum - 1];
		    lxe = lxends[toknum - 1];
		    zzekinqc_(chrbuf + (b - 1), &l, &lxb, &lxe, eqryi, eqryc, 
			    tabdsc, e - (b - 1), eqryc_len);

/*                 Append the table descriptor to the integer part of the */
/*                 query. */

		    for (j = 1; j <= 6; ++j) {
			appndi_(&tabdsc[(i__1 = j - 1) < 6 && 0 <= i__1 ? 
				i__1 : s_rnge("tabdsc", i__1, "zzekpars_", (
				ftnlen)558)], eqryi);
		    }

/*                 Add a place-holder value descriptor to reserve */
/*                 space for an alias descriptor for this table.  If an */
/*                 actual alias is supplied, we'll update this */
/*                 descriptor. */

		    cleari_(&c__6, alsdsc);
		    alsdsc[0] = 1;
		    for (j = 1; j <= 6; ++j) {
			appndi_(&alsdsc[(i__1 = j - 1) < 6 && 0 <= i__1 ? 
				i__1 : s_rnge("alsdsc", i__1, "zzekpars_", (
				ftnlen)571)], eqryi);
		    }

/*                 Update the table count in the encoded query. */

		    zzekweqi_("NUM_TABLES", &ntabs, eqryi, (ftnlen)10);
		    state = 1;
		}
	    } else if (token == 11) {
		*error = TRUE_;
		s_copy(errtyp, "MORE_TOKENS_EXP", (ftnlen)32, (ftnlen)15);
		state = 16;
	    } else {

/*              We've got the wrong kind of token here. */

		*error = TRUE_;
		s_copy(errtyp, "TABLE_EXP", (ftnlen)32, (ftnlen)9);
		state = 16;
	    }

/*           State is a member of {FRMTAB, TERM}. */

	} else if (state == 1) {

/*           We should see a comma, an alias, one of the SELECT, */
/*           WHERE or ORDER keywords, or the end of the query. */

	    if (token == 11) {

/*              We're out of tokens.  It's time to parse the */
/*              WHERE clause. */

		state = 4;
	    } else if (token == 8) {

/*              It's time to look for another table name. */

		state = 0;
	    } else if (token == 2) {

/*              We've got an alias.  Add this string to the encoded */
/*              query. */

		i__ = values[toknum - 1];
		b = chbegs[i__ - 1];
		e = chends[i__ - 1];
		l = e - b + 1;
		lxb = lxbegs[toknum - 1];
		lxe = lxends[toknum - 1];
		zzekinqc_(chrbuf + (b - 1), &l, &lxb, &lxe, eqryi, eqryc, 
			alsdsc, e - (b - 1), eqryc_len);

/*              Update the place-holder alias descriptor in the integer */
/*              part of the query. */

		movei_(alsdsc, &c__6, &eqryi[cardi_(eqryi)]);
		state = 3;
	    } else if (token == 1) {

/*              The last table name in the FROM clause is followed by */
/*              a keyword.  SELECT, WHERE and ORDER are the only valid */
/*              possibilities. */

		if (values[toknum - 1] != 29 && values[toknum - 1] != 27 && 
			values[toknum - 1] != 26) {

/*                 We've got a keyword we don't want here. */

		    *error = TRUE_;
		    s_copy(errtyp, "BAD_KEYWORD", (ftnlen)32, (ftnlen)11);
		    state = 16;
		} else {

/*                 Parse the WHERE clause. */

		    state = 4;
		}
	    } else {

/*              We've got the wrong kind of token altogether. */

		*error = TRUE_;
		s_copy(errtyp, "ALIAS_EXP", (ftnlen)32, (ftnlen)9);
		state = 16;
	    }

/*           STATE is a member of {FROM, FRMALS, WHERE, TERM}. */

	} else if (state == 3) {

/*           We should see a comma, the SELECT, WHERE or ORDER */
/*           keywords, or the end of the query. */

	    if (token == 11) {

/*              We're out of tokens.  It's time to parse the */
/*              WHERE clause. */

		state = 4;
	    } else if (token == 8) {

/*              It's time to look for another table name. */

		state = 0;
	    } else if (token == 1) {

/*              The last table name in the FROM clause is followed by */
/*              a keyword.  SELECT, WHERE and ORDER are the only valid */
/*              possibilities. */

		if (values[toknum - 1] != 29 && values[toknum - 1] != 27 && 
			values[toknum - 1] != 26) {

/*                 We've got a keyword we don't want here. */

		    *error = TRUE_;
		    s_copy(errtyp, "BAD_KEYWORD", (ftnlen)32, (ftnlen)11);
		    state = 16;
		} else {

/*                 Parse the WHERE clause. */

		    state = 4;
		}
	    } else {

/*              We've got the wrong kind of token altogether. */

		*error = TRUE_;
		s_copy(errtyp, "COMMA_OR_KEY_EXP", (ftnlen)32, (ftnlen)16);
		state = 16;
	    }

/*           STATE is a member of {FROM, WHERE, TERM}. */

	} else if (state == 11) {

/*           It's time to parse the SELECT clause.  We'll need to */
/*           locate the SELECT keyword. */

	    zzektloc_(&c__1, &c__27, ntoken, tokens, values, &toknum, &fnd);
	    if (! fnd) {
		*error = TRUE_;
		s_copy(errtyp, "SELECT_NOT_FOUND", (ftnlen)32, (ftnlen)16);
		state = 16;
	    } else {
		state = 12;
	    }
	} else if (state == 12) {

/*           We must see either the * token, the ALL keyword, */
/*           or an identifier here.  The identifier may be a lone */
/*           column name, or it may be a column name qualified by a */
/*           table name or alias. */

/*           For the moment, we don't support the * or ALL options. */

	    if (token == 2) {

/*              We've found a name (as far as we can tell at this point). */
/*              Make sure we haven't exceeded the limit for SELECT */
/*              column names; if not, store the name string in the */
/*              encoded query, and save the descriptor until we've */
/*              figured out whether we're looking at a column name or */
/*              table name. */

		++nsel;
		if (nsel > 50) {
		    *error = TRUE_;
		    s_copy(errtyp, "TOO_MANY_SEL_COLS", (ftnlen)32, (ftnlen)
			    17);
		    state = 16;
		} else {
		    i__ = values[toknum - 1];
		    b = chbegs[i__ - 1];
		    e = chends[i__ - 1];
		    l = e - b + 1;
		    lxb = lxbegs[toknum - 1];
		    lxe = lxends[toknum - 1];
		    zzekinqc_(chrbuf + (b - 1), &l, &lxb, &lxe, eqryi, eqryc, 
			    namdsc, e - (b - 1), eqryc_len);

/*                 Add a place-holder value descriptor to reserve */
/*                 space for a table descriptor for this name.  If it */
/*                 turns out that the current name is a table name, we'll */
/*                 update this descriptor. */

		    cleari_(&c__6, valdsc);
		    valdsc[0] = 1;
		    for (j = 1; j <= 6; ++j) {
			appndi_(&valdsc[(i__1 = j - 1) < 6 && 0 <= i__1 ? 
				i__1 : s_rnge("valdsc", i__1, "zzekpars_", (
				ftnlen)832)], eqryi);
		    }

/*                 Update the SELECT column count in the encoded query. */

		    zzekweqi_("NUM_SELECT_COLS", &nsel, eqryi, (ftnlen)15);
		    state = 14;
		}
	    } else if (token == 11) {
		*error = TRUE_;
		s_copy(errtyp, "MORE_TOKENS_EXP", (ftnlen)32, (ftnlen)15);
		state = 16;
	    } else {

/*              We've got the wrong kind of token here. */

		*error = TRUE_;
		s_copy(errtyp, "TABLE_OR_COLUMN_EXP", (ftnlen)32, (ftnlen)19);
		state = 16;
	    }

/*           State is a member of {SELNAM, TERM}. */

	} else if (state == 14) {

/*           We've seen a SELECT column name, or else the name */
/*           of a table qualifying a SELECT column name. */

	    if (token == 11) {

/*              The name we picked up was an unqualified column */
/*              name.  Append the saved name descriptor to the encoded */
/*              query. */

		for (j = 1; j <= 6; ++j) {
		    appndi_(&namdsc[(i__1 = j - 1) < 6 && 0 <= i__1 ? i__1 : 
			    s_rnge("namdsc", i__1, "zzekpars_", (ftnlen)879)],
			     eqryi);
		}
		state = 16;
	    } else if (token == 8) {

/*              The name we picked up was an unqualified column */
/*              name.  Append the saved name descriptor to the encoded */
/*              query.  Another name should follow. */

		for (j = 1; j <= 6; ++j) {
		    appndi_(&namdsc[(i__1 = j - 1) < 6 && 0 <= i__1 ? i__1 : 
			    s_rnge("namdsc", i__1, "zzekpars_", (ftnlen)892)],
			     eqryi);
		}
		state = 12;
	    } else if (token == 9) {

/*              The name we picked up was a table name or alias.  A */
/*              column name should follow. */

		state = 13;
	    } else if (token == 1) {

/*              We have the last column name in the SELECT clause. */

/*              Append the saved name descriptor to the encoded */
/*              query. */

		for (j = 1; j <= 6; ++j) {
		    appndi_(&namdsc[(i__1 = j - 1) < 6 && 0 <= i__1 ? i__1 : 
			    s_rnge("namdsc", i__1, "zzekpars_", (ftnlen)914)],
			     eqryi);
		}

/*              The last column name in the SELECT clause is followed by */
/*              a keyword.  FROM, WHERE and ORDER are the only valid */
/*              possibilities. */

		if (values[toknum - 1] == 29 || values[toknum - 1] == 11 || 
			values[toknum - 1] == 26) {

/*                 We're done with the SELECT clause. */

		    state = 16;
		} else {

/*                 We've got a keyword we don't want here. */

		    *error = TRUE_;
		    s_copy(errtyp, "BAD_KEYWORD", (ftnlen)32, (ftnlen)11);
		    state = 16;
		}
	    } else {

/*              We've got the wrong kind of token here. */

		*error = TRUE_;
		s_copy(errtyp, "BAD_TOKEN", (ftnlen)32, (ftnlen)9);
		state = 16;
	    }

/*           STATE is a member of {SELECT, SELTAB, TERM}. */

	} else if (state == 13) {

/*           We've picked up a qualifying table name for a SELECT */
/*           column.  We must see a column name here. */

	    if (token == 2) {

/*              Update the place-holder table name descriptor in the */
/*              encoded query. */

		movei_(namdsc, &c__6, &eqryi[cardi_(eqryi)]);

/*              Add the column name to the character part of the */
/*              encoded query. */

		i__ = values[toknum - 1];
		b = chbegs[i__ - 1];
		e = chends[i__ - 1];
		l = e - b + 1;
		lxb = lxbegs[toknum - 1];
		lxe = lxends[toknum - 1];
		zzekinqc_(chrbuf + (b - 1), &l, &lxb, &lxe, eqryi, eqryc, 
			coldsc, e - (b - 1), eqryc_len);

/*              Add the descriptor for the column name to the encoded */
/*              query. */

		for (j = 1; j <= 6; ++j) {
		    appndi_(&coldsc[(i__1 = j - 1) < 6 && 0 <= i__1 ? i__1 : 
			    s_rnge("coldsc", i__1, "zzekpars_", (ftnlen)992)],
			     eqryi);
		}
		state = 15;
	    } else if (token == 11) {
		*error = TRUE_;
		s_copy(errtyp, "MORE_TOKENS_EXP", (ftnlen)32, (ftnlen)15);
		state = 16;
	    } else {
		*error = TRUE_;
		s_copy(errtyp, "COLUMN_EXP", (ftnlen)32, (ftnlen)10);
		state = 16;
	    }

/*           STATE is a member of {SELCOL, TERM}. */

	} else if (state == 15) {

/*           We've picked up a qualified column name.  At this point, */
/*           we should see a keyword, a comma, or the end of the */
/*           query. */

	    if (token == 1) {

/*              The last column name in the SELECT clause is followed by */
/*              a keyword.  FROM, WHERE and ORDER are the only valid */
/*              possibilities. */

		if (values[toknum - 1] == 29 || values[toknum - 1] == 11 || 
			values[toknum - 1] == 26) {

/*                 We're done with the SELECT clause. */

		    state = 16;
		} else {

/*                 We've got a keyword we don't want here. */

		    *error = TRUE_;
		    s_copy(errtyp, "BAD_KEYWORD", (ftnlen)32, (ftnlen)11);
		    state = 16;
		}
	    } else if (token == 8) {

/*              We expect another SELECT column. */

		state = 12;
	    } else if (token == 11) {

/*              We're done with the SELECT clause. */

		state = 16;
	    } else {
		*error = TRUE_;
		s_copy(errtyp, "COMMA_OR_KEY_EXP", (ftnlen)32, (ftnlen)16);
		state = 16;
	    }

/*           STATE is a member of {SELECT, TERM}. */

	} else if (state == 4) {

/*           The WHERE clause is optional.  See whether we have one.  The */
/*           clause is started by a WHERE keyword. */

	    zzektloc_(&c__1, &c__29, ntoken, tokens, values, &toknum, &fnd);
	    if (fnd) {

/*              We're going to hand off the list of tokens that comprise */
/*              the WHERE clause of the query to a routine that will */
/*              parse the tokens and form a list of relational */
/*              constraints.  Once this is done, all we have to do here */
/*              is check the validity of the column names and the values */
/*              used in the constraints. */

		zzeknrml_(query, ntoken, lxbegs, lxends, tokens, values, 
			numvls, chrbuf, chbegs, chends, eqryi, eqryc, eqryd, 
			error, prserr, query_len, chrbuf_len, eqryc_len, 
			prserr_len);
		if (*error) {
		    s_copy(errtyp, "WHERE_ERROR", (ftnlen)32, (ftnlen)11);
		    state = 16;
		} else {

/*                 Parse the ORDER BY clause, if one is present. */

		    state = 5;
		}
	    } else {

/*              Parse the ORDER BY clause, if one is present. */

		state = 5;
	    }

/*           STATE is a member of {ORDER, TERM}. */

	} else if (state == 5) {

/*           The ORDER BY clause is optional.  See whether we have one. */
/*           The clause is started by an ORDER keyword. */

	    zzektloc_(&c__1, &c__26, ntoken, tokens, values, &toknum, &fnd);
	    if (fnd) {

/*              The BY keyword should follow the ORDER keyword. */

		if (toknum < *ntoken) {
		    ++toknum;
		    if (tokens[toknum - 1] == 1 && values[toknum - 1] == 6) {

/*                    We're ready to parse the ORDER BY clause. */

			state = 6;
		    } else {

/*                    No BY keyword followed the ORDER keyword. */

			*error = TRUE_;
			s_copy(errtyp, "BY_EXPECTED", (ftnlen)32, (ftnlen)11);
			state = 16;
		    }
		} else {

/*                 We're out of tokens where we shouldn't be. */

		    *error = TRUE_;
		    s_copy(errtyp, "BY_EXPECTED", (ftnlen)32, (ftnlen)11);
		    state = 16;
		}
	    } else {

/*              We're ready to go on to the SELECT clause. */

		state = 11;
	    }

/*           STATE is a member of {ORDRBY, SELKEY, TERM}. */

	} else if (state == 6) {

/*           We must see a name in the order column list here. */
/*           The name may be a lone column name, or it may be a column */
/*           name qualified by a table name or alias. */

	    if (token == 2) {

/*              We've found a name (as far as we can tell at this point). */
/*              Make sure we haven't exceeded the limit for order-by */
/*              column names; if not, store the name string in the */
/*              encoded query, and save the descriptor until we've */
/*              figured out whether we're looking at a column name or */
/*              table name. */

		++norder;
		if (norder > 10) {
		    *error = TRUE_;
		    s_copy(errtyp, "TOO_MANY_ORD_COLS", (ftnlen)32, (ftnlen)
			    17);
		    state = 16;
		} else {
		    i__ = values[toknum - 1];
		    b = chbegs[i__ - 1];
		    e = chends[i__ - 1];
		    l = e - b + 1;
		    lxb = lxbegs[toknum - 1];
		    lxe = lxends[toknum - 1];
		    zzekinqc_(chrbuf + (b - 1), &l, &lxb, &lxe, eqryi, eqryc, 
			    namdsc, e - (b - 1), eqryc_len);

/*                 Add a place-holder value descriptor to reserve */
/*                 space for a table descriptor for this name.  If it */
/*                 turns out that the current name is a table name, we'll */
/*                 update this descriptor. */

		    cleari_(&c__6, valdsc);
		    valdsc[0] = 1;
		    for (j = 1; j <= 6; ++j) {
			appndi_(&valdsc[(i__1 = j - 1) < 6 && 0 <= i__1 ? 
				i__1 : s_rnge("valdsc", i__1, "zzekpars_", (
				ftnlen)1244)], eqryi);
		    }

/*                 Update the order-by column count in the encoded query. */

		    zzekweqi_("NUM_ORDERBY_COLS", &norder, eqryi, (ftnlen)16);
		    state = 8;
		}
	    } else if (token == 11) {
		*error = TRUE_;
		s_copy(errtyp, "MORE_TOKENS_EXP", (ftnlen)32, (ftnlen)15);
		state = 16;
	    } else {

/*              We've got the wrong kind of token here. */

		*error = TRUE_;
		s_copy(errtyp, "TABLE_OR_COLUMN_EXP", (ftnlen)32, (ftnlen)19);
		state = 16;
	    }

/*           State is a member of {ORDNAM, TERM}. */

	} else if (state == 8) {

/*           We've seen an order-by column name, or else the name */
/*           of a table qualifying an order-by column name. */

	    if (token == 11) {

/*              The name we picked up was an unqualified column */
/*              name.  Append the saved name descriptor to the encoded */
/*              query. */

		for (j = 1; j <= 6; ++j) {
		    appndi_(&namdsc[(i__1 = j - 1) < 6 && 0 <= i__1 ? i__1 : 
			    s_rnge("namdsc", i__1, "zzekpars_", (ftnlen)1291)]
			    , eqryi);
		}

/*              Since no ASCENDING or DESCENDING sense keyword was */
/*              supplied, append the default value ASCENDING to the */
/*              order-by column descriptor in the encoded query. */

		appndi_(&c__0, eqryi);

/*              We're done with the ORDER BY clause; go on to parse the */
/*              SELECT clause. */

		state = 11;
	    } else if (token == 8) {

/*              The name we picked up was an unqualified column */
/*              name.  Append the saved name descriptor to the encoded */
/*              query.  Another name should follow. */

		for (j = 1; j <= 6; ++j) {
		    appndi_(&namdsc[(i__1 = j - 1) < 6 && 0 <= i__1 ? i__1 : 
			    s_rnge("namdsc", i__1, "zzekpars_", (ftnlen)1315)]
			    , eqryi);
		}

/*              Since no ASCENDING or DESCENDING sense keyword was */
/*              supplied, append the default value ASCENDING to the */
/*              order-by column descriptor in the encoded query. */

		appndi_(&c__0, eqryi);
		state = 6;
	    } else if (token == 9) {

/*              The name we picked up was a table name or alias.  A */
/*              column name should follow. */

		state = 7;
	    } else if (token == 1) {

/*              We have a column name, which may be followed by a */
/*              keyword indicating the sense of the ordering, or may */
/*              be followed by a keyword starting a new clause. */

/*              Append the saved name descriptor to the encoded */
/*              query. */

		for (j = 1; j <= 6; ++j) {
		    appndi_(&namdsc[(i__1 = j - 1) < 6 && 0 <= i__1 ? i__1 : 
			    s_rnge("namdsc", i__1, "zzekpars_", (ftnlen)1346)]
			    , eqryi);
		}

/*              Set the sense descriptor according to the keyword we've */
/*              picked up.  After this, we're ready to look for another */
/*              order-by column. */

		if (values[toknum - 1] == 3) {
		    appndi_(&c__0, eqryi);
		    state = 10;
		} else if (values[toknum - 1] == 8) {
		    appndi_(&c__1, eqryi);
		    state = 10;
		} else if (values[toknum - 1] == 29 || values[toknum - 1] == 
			11 || values[toknum - 1] == 27) {

/*                 Since no ASCENDING or DESCENDING sense keyword was */
/*                 supplied, append the default value ASCENDING to the */
/*                 order-by column descriptor in the encoded query. */

		    appndi_(&c__0, eqryi);

/*                 We're done with the ORDER BY clause.  Go on to */
/*                 parse the SELECT clause. */

		    state = 11;
		} else {

/*                 We've got a keyword we don't want here. */

		    *error = TRUE_;
		    s_copy(errtyp, "BAD_KEYWORD", (ftnlen)32, (ftnlen)11);
		    state = 16;
		}
	    } else {

/*              We've got the wrong kind of token here. */

		*error = TRUE_;
		s_copy(errtyp, "BAD_TOKEN", (ftnlen)32, (ftnlen)9);
		state = 16;
	    }

/*           STATE is a member of {ORDRBY, ORDTAB, ORDSNS, SELKEY, TERM}. */

	} else if (state == 7) {

/*           We've picked up a qualifying table name for an order-by */
/*           column.  We must see a column name here. */

	    if (token == 2) {

/*              Update the place-holder table name descriptor in the */
/*              encoded query. */

		movei_(namdsc, &c__6, &eqryi[cardi_(eqryi)]);

/*              Add the column name to the character part of the */
/*              encoded query. */

		i__ = values[toknum - 1];
		b = chbegs[i__ - 1];
		e = chends[i__ - 1];
		l = e - b + 1;
		lxb = lxbegs[toknum - 1];
		lxe = lxends[toknum - 1];
		zzekinqc_(chrbuf + (b - 1), &l, &lxb, &lxe, eqryi, eqryc, 
			coldsc, e - (b - 1), eqryc_len);

/*              Add the descriptor for the column name to the encoded */
/*              query. */

		for (j = 1; j <= 6; ++j) {
		    appndi_(&coldsc[(i__1 = j - 1) < 6 && 0 <= i__1 ? i__1 : 
			    s_rnge("coldsc", i__1, "zzekpars_", (ftnlen)1448)]
			    , eqryi);
		}
		state = 9;
	    } else if (token == 11) {
		*error = TRUE_;
		s_copy(errtyp, "MORE_TOKENS_EXP", (ftnlen)32, (ftnlen)15);
		state = 16;
	    } else {
		*error = TRUE_;
		s_copy(errtyp, "COLUMN_EXP", (ftnlen)32, (ftnlen)10);
		state = 16;
	    }

/*           STATE is a member of {ORDCOL, TERM}. */

	} else if (state == 9) {

/*           We've picked up a qualified column name.  At this point, */
/*           we should see a sense keyword, a comma, the end of the */
/*           query, or one of the FROM, SELECT, or WHERE keywords. */

	    if (token == 1) {
		if (values[toknum - 1] == 3) {

/*                 The ASCENDING keyword has been supplied.  After this, */
/*                 look for another column. */

		    appndi_(&c__0, eqryi);
		    state = 10;
		} else if (values[toknum - 1] == 8) {

/*                 The DESCENDING keyword has been supplied.  After this, */
/*                 look for another column. */

		    appndi_(&c__1, eqryi);
		    state = 10;
		} else if (values[toknum - 1] == 29 || values[toknum - 1] == 
			11 || values[toknum - 1] == 27) {

/*                 We're done with the ORDER BY clause.  Go on to */
/*                 parse the SELECT clause. */

		    state = 11;
		} else {
		    *error = TRUE_;
		    s_copy(errtyp, "BAD_KEYWORD", (ftnlen)32, (ftnlen)11);
		    state = 16;
		}
	    } else if (token == 8) {

/*              The ASCENDING keyword is implied. */

		appndi_(&c__0, eqryi);
		state = 6;
	    } else if (token == 11) {

/*              The ASCENDING keyword is implied. */

		appndi_(&c__0, eqryi);

/*              We're done with the ORDER BY clause.  Parse the SELECT */
/*              clause. */

		state = 11;
	    } else {
		*error = TRUE_;
		s_copy(errtyp, "COMMA_OR_KEY_EXP", (ftnlen)32, (ftnlen)16);
		state = 16;
	    }

/*           STATE is a member of {ORDRBY, ORDSNS, SELKEY, TERM}. */

	} else if (state == 10) {

/*           We've picked up an order sense keyword.  At this point, */
/*           we should see comma or the end of the query, or one of the */
/*           FROM, SELECT, or WHERE keywords. */

	    if (token == 8) {

/*              We're ready to look for another column. */

		state = 6;
	    } else if (token == 11) {

/*              We're done with the ORDER BY clause.  Parse the SELECT */
/*              clause. */

		state = 11;
	    } else if (token == 1) {
		if (values[toknum - 1] == 29 || values[toknum - 1] == 11 || 
			values[toknum - 1] == 27) {

/*                 We're done with the ORDER BY clause.  Go on to */
/*                 parse the SELECT clause. */

		    state = 11;
		} else {
		    *error = TRUE_;
		    s_copy(errtyp, "BAD_KEYWORD", (ftnlen)32, (ftnlen)11);
		    state = 16;
		}
	    } else {
		*error = TRUE_;
		s_copy(errtyp, "COMMA_EXP", (ftnlen)32, (ftnlen)9);
		state = 16;
	    }

/*           STATE is a member of {ORDRBY, SELKEY, TERM}. */

	} else {

/*           Somehow, we've reached an invalid state. */

	    *error = TRUE_;
	    s_copy(prserr, "SPICE(BUG) -- Invalid state reached in EK parser."
		    , prserr_len, (ftnlen)49);
	    state = 16;
	}

/*           STATE is a member of {ORDRBY, TERM}. */

    }

/*     At this point, either an error has been detected, or the query */
/*     has been parsed, and the query is represented in encoded form */
/*     in the outputs EQRYI, EQRYC, and EQRYD. */


/*     We centralize construction of error messages in the following */
/*     section. */

    if (*error) {
	if (s_cmp(errtyp, "FROM_NOT_FOUND", (ftnlen)32, (ftnlen)14) == 0) {
	    s_copy(prserr, "Every query must contain a FROM clause. The FROM"
		    " keyword was not found.", prserr_len, (ftnlen)71);
	} else if (s_cmp(errtyp, "SELECT_NOT_FOUND", (ftnlen)32, (ftnlen)16) 
		== 0) {
	    s_copy(prserr, "Every query must contain a SELECT clause. The SE"
		    "LECT keyword was not found.", prserr_len, (ftnlen)75);
	} else if (s_cmp(errtyp, "BY_EXPECTED", (ftnlen)32, (ftnlen)11) == 0) 
		{
	    s_copy(prserr, "The BY keyword was not found following the ORDER"
		    " keyword.", prserr_len, (ftnlen)57);
	} else if (s_cmp(errtyp, "BAD_KEYWORD", (ftnlen)32, (ftnlen)11) == 0) 
		{
	    lxb = lxbegs[toknum - 1];
	    lxe = lxends[toknum - 1];
	    s_copy(prserr, "Invalid keyword at location #. Actual token was:"
		    " #", prserr_len, (ftnlen)50);
	    repmi_(prserr, "#", &lxb, prserr, prserr_len, (ftnlen)1, 
		    prserr_len);
	    repmc_(prserr, "#", query + (lxb - 1), prserr, prserr_len, (
		    ftnlen)1, lxe - (lxb - 1), prserr_len);
	} else if (s_cmp(errtyp, "TABLE_OR_COLUMN_EXP", (ftnlen)32, (ftnlen)
		19) == 0) {
	    lxb = lxbegs[toknum - 1];
	    lxe = lxends[toknum - 1];
	    s_copy(prserr, "Table or column name expected at location #. Act"
		    "ual token was: #", prserr_len, (ftnlen)64);
	    repmi_(prserr, "#", &lxb, prserr, prserr_len, (ftnlen)1, 
		    prserr_len);
	    repmc_(prserr, "#", query + (lxb - 1), prserr, prserr_len, (
		    ftnlen)1, lxe - (lxb - 1), prserr_len);
	} else if (s_cmp(errtyp, "TABLE_EXP", (ftnlen)32, (ftnlen)9) == 0) {
	    lxb = lxbegs[toknum - 1];
	    lxe = lxends[toknum - 1];
	    s_copy(prserr, "Table name expected at location #. Actual token "
		    "was: #", prserr_len, (ftnlen)54);
	    repmi_(prserr, "#", &lxb, prserr, prserr_len, (ftnlen)1, 
		    prserr_len);
	    repmc_(prserr, "#", query + (lxb - 1), prserr, prserr_len, (
		    ftnlen)1, lxe - (lxb - 1), prserr_len);
	} else if (s_cmp(errtyp, "COLUMN_EXP", (ftnlen)32, (ftnlen)10) == 0) {
	    lxb = lxbegs[toknum - 1];
	    lxe = lxends[toknum - 1];
	    s_copy(prserr, "Column name expected at location #. Actual token"
		    " was: #", prserr_len, (ftnlen)55);
	    repmi_(prserr, "#", &lxb, prserr, prserr_len, (ftnlen)1, 
		    prserr_len);
	    repmc_(prserr, "#", query + (lxb - 1), prserr, prserr_len, (
		    ftnlen)1, lxe - (lxb - 1), prserr_len);
	} else if (s_cmp(errtyp, "ALIAS_EXP", (ftnlen)32, (ftnlen)9) == 0) {
	    lxb = lxbegs[toknum - 1];
	    lxe = lxends[toknum - 1];
	    s_copy(prserr, "Table alias, comma, or keyword expected at locat"
		    "ion #. Actual token was: #", prserr_len, (ftnlen)74);
	    repmi_(prserr, "#", &lxb, prserr, prserr_len, (ftnlen)1, 
		    prserr_len);
	    repmc_(prserr, "#", query + (lxb - 1), prserr, prserr_len, (
		    ftnlen)1, lxe - (lxb - 1), prserr_len);
	} else if (s_cmp(errtyp, "COMMA_OR_KEY_EXP", (ftnlen)32, (ftnlen)16) 
		== 0) {
	    lxb = lxbegs[toknum - 1];
	    lxe = lxends[toknum - 1];
	    s_copy(prserr, "Comma or keyword expected at location #. Actual "
		    "token was: #", prserr_len, (ftnlen)60);
	    repmi_(prserr, "#", &lxb, prserr, prserr_len, (ftnlen)1, 
		    prserr_len);
	    repmc_(prserr, "#", query + (lxb - 1), prserr, prserr_len, (
		    ftnlen)1, lxe - (lxb - 1), prserr_len);
	} else if (s_cmp(errtyp, "COMMA_EXP", (ftnlen)32, (ftnlen)9) == 0) {
	    lxb = lxbegs[toknum - 1];
	    lxe = lxends[toknum - 1];
	    s_copy(prserr, "Comma expected at location #. Actual token was: #"
		    , prserr_len, (ftnlen)49);
	    repmi_(prserr, "#", &lxb, prserr, prserr_len, (ftnlen)1, 
		    prserr_len);
	    repmc_(prserr, "#", query + (lxb - 1), prserr, prserr_len, (
		    ftnlen)1, lxe - (lxb - 1), prserr_len);
	} else if (s_cmp(errtyp, "MORE_TOKENS_EXP", (ftnlen)32, (ftnlen)15) ==
		 0) {
	    s_copy(prserr, "More tokens were expected in query.", prserr_len, 
		    (ftnlen)35);
	} else if (s_cmp(errtyp, "KEYWORD_EXP", (ftnlen)32, (ftnlen)11) == 0) 
		{
	    s_copy(prserr, "The keyword # was expected at location #. Actual"
		    " token was: #", prserr_len, (ftnlen)61);
	    repmc_(prserr, "#", expkey, prserr, prserr_len, (ftnlen)1, (
		    ftnlen)32, prserr_len);
	    repmi_(prserr, "#", &lxb, prserr, prserr_len, (ftnlen)1, 
		    prserr_len);
	    repmc_(prserr, "#", query + (lxb - 1), prserr, prserr_len, (
		    ftnlen)1, lxe - (lxb - 1), prserr_len);
	} else if (s_cmp(errtyp, "BAD_TOKEN", (ftnlen)32, (ftnlen)9) == 0) {
	    lxb = lxbegs[toknum - 1];
	    lxe = lxends[toknum - 1];
	    s_copy(prserr, "Invalid token at location #. Token was: #", 
		    prserr_len, (ftnlen)41);
	    repmi_(prserr, "#", &lxb, prserr, prserr_len, (ftnlen)1, 
		    prserr_len);
	    repmc_(prserr, "#", query + (lxb - 1), prserr, prserr_len, (
		    ftnlen)1, lxe - (lxb - 1), prserr_len);
	} else if (s_cmp(errtyp, "TOO_MANY_TABLES", (ftnlen)32, (ftnlen)15) ==
		 0) {
	    s_copy(prserr, "Number of tables in \"FROM\" clause exceeds allo"
		    "wed maximum of #.", prserr_len, (ftnlen)63);
	    repmi_(prserr, "#", &c__10, prserr, prserr_len, (ftnlen)1, 
		    prserr_len);
	} else if (s_cmp(errtyp, "TOO_MANY_ORD_COLS", (ftnlen)32, (ftnlen)17) 
		== 0) {
	    s_copy(prserr, "Number of order-by columns exceeds allowed maxim"
		    "um of #.", prserr_len, (ftnlen)56);
	    repmi_(prserr, "#", &c__10, prserr, prserr_len, (ftnlen)1, 
		    prserr_len);
	} else if (s_cmp(errtyp, "TOO_MANY_SEL_COLS", (ftnlen)32, (ftnlen)17) 
		== 0) {
	    s_copy(prserr, "Number of SELECT columns exceeds allowed maximum"
		    " of #.", prserr_len, (ftnlen)54);
	    repmi_(prserr, "#", &c__50, prserr, prserr_len, (ftnlen)1, 
		    prserr_len);
	} else if (s_cmp(errtyp, "WHERE_ERROR", (ftnlen)32, (ftnlen)11) != 0) 
		{
	    s_copy(prserr, "SPICE(BUG)--Unrecognized error type.  Type was #."
		    , prserr_len, (ftnlen)49);
	    repmc_(prserr, "#", errtyp, prserr, prserr_len, (ftnlen)1, (
		    ftnlen)32, prserr_len);
	}
    } else {

/*        Indicate that parsing is complete. */

	zzekweqi_("PARSED", &c__1, eqryi, (ftnlen)6);
    }
    chkout_("ZZEKPARS", (ftnlen)8);
    return 0;
} /* zzekpars_ */

