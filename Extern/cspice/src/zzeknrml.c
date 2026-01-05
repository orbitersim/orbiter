/* zzeknrml.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__500 = 500;
static integer c__1 = 1;
static integer c__29 = 29;
static integer c__5000 = 5000;
static integer c__3 = 3;
static integer c__7 = 7;
static integer c__8 = 8;
static integer c__0 = 0;

/* $Procedure      ZZEKNRML ( EK, normalize WHERE clause ) */
/* Subroutine */ int zzeknrml_(char *query, integer *ntoken, integer *lxbegs, 
	integer *lxends, integer *tokens, integer *values, doublereal *numvls,
	 char *chrbuf, integer *chbegs, integer *chends, integer *eqryi, char 
	*eqryc, doublereal *eqryd, logical *error, char *prserr, ftnlen 
	query_len, ftnlen chrbuf_len, ftnlen eqryc_len, ftnlen prserr_len)
{
    /* Initialized data */

    static integer logops[3] = { 2,25,23 };
    static integer logcde[3] = { -10,-11,-12 };
    static integer cmpops[7] = { 10,12,14,17,19,22,18 };
    static integer cmpcde[8] = { 1,2,3,4,5,6,7,8 };
    static integer cmpneg[8] = { 6,5,4,3,2,1,8,7 };
    static integer endkw[3] = { 11,26,27 };

    /* System generated locals */
    integer i__1, i__2, i__3;

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_rnge(char *, integer, char *, integer);

    /* Local variables */
    static integer node, tail, rels[15000]	/* was [3][5000] */, skip;
    static logical qual;
    static integer head1, head2, next, prev, type__;
    extern /* Subroutine */ int zzekinqc_(char *, integer *, integer *, 
	    integer *, integer *, char *, integer *, ftnlen, ftnlen), 
	    zzektloc_(integer *, integer *, integer *, integer *, integer *, 
	    integer *, logical *), zzekinqn_(doublereal *, integer *, integer 
	    *, integer *, integer *, doublereal *, integer *), zzekweqi_(char 
	    *, integer *, integer *, ftnlen);
    static integer b, e, i__, j, k;
    extern integer cardi_(integer *);
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    static integer nmeta, level;
    extern integer lnkhl_(integer *, integer *);
    static integer nconj, newcj;
    extern integer lnktl_(integer *, integer *);
    static integer first, newdj, nrels, sizes[1000], start, state, third;
    static logical donow;
    extern /* Subroutine */ int repmi_(char *, char *, integer *, char *, 
	    ftnlen, ftnlen, ftnlen), lnkan_(integer *, integer *), repmc_(
	    char *, char *, char *, char *, ftnlen, ftnlen, ftnlen, ftnlen);
    static integer cj[4], dj[2], op;
    extern integer isrchi_(integer *, integer *, integer *), lnknfn_(integer *
	    ), lnknxt_(integer *, integer *), lnkprv_(integer *, integer *);
    extern logical return_(void);
    static integer rlpool[10012]	/* was [2][5006] */, cjpool[10012]	
	    /* was [2][5006] */, cjptrs[5000], djpool[10012]	/* was [2][
	    5006] */, djptrs[5000], mtpool[1012]	/* was [2][506] */, 
	    mtcode[500], mtexpr[500], mstart[500], popcnd[500], cjnode, 
	    colptr, djnode, djtail, dspool[10012]	/* was [2][5006] */, 
	    dscbuf[35000]	/* was [7][5000] */, endloc, exprhd, fourth, 
	    lxb, lxe, metahd, newrel, rel[4], relptr, relset[5006], retcnd, 
	    rhsptr, second, tabptr, whrbeg, whrend, whrsiz;
    static logical fnd;
    extern /* Subroutine */ int chkout_(char *, ftnlen), lnkini_(integer *, 
	    integer *), lnkila_(integer *, integer *, integer *), lnkfsl_(
	    integer *, integer *, integer *), cleari_(integer *, integer *), 
	    lnkilb_(integer *, integer *, integer *), ssizei_(integer *, 
	    integer *), insrti_(integer *, integer *), appndi_(integer *, 
	    integer *);

/* $ Abstract */

/*     Convert the WHERE clause of an EK query to a normalized form. */

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

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     QUERY      I   Input EK query. */
/*     NTOKEN     I   Number of tokens in query. */
/*     LXBEGS     I   Start positions of lexemes comprising WHERE clause. */
/*     LXENDS     I   End positions of lexemes comprising WHERE clause. */
/*     TOKENS     I   Tokens comprising query. */
/*     VALUES     I   Values associated with tokens. */
/*     NUMVLS     I   Buffer containing numeric token values. */
/*     CHRBUF     I   Buffer containing string token values. */
/*     CHBEGS, */
/*     CHENDS     I   String token begin and end character positions. */
/*     EQRYI, */
/*     EQRYC, */
/*     EQRYD      O   Parsed query and string and number value buffers. */
/*     ERROR      O   Parse error flag. */
/*     PRSERR     O   Parse error message. */

/* $ Detailed_Input */

/*     QUERY          is an EK query to be parsed.  The tokens of the */
/*                    query have been found already.  See the header */
/*                    of the subroutine EKFIND for a detailed */
/*                    description of the EK query language. */

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
/*                    of an encoded form of the input query.  The WHERE */
/*                    clause of the input query is represented in this */
/*                    encoding.  The WHERE clause constraints have been */
/*                    normalized. */

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

/*     No matter how ridiculous the query passed to ZZEKNRML, the */
/*     routine diagnoses errors via the output arguments ERROR and */
/*     PRSERR.  No errors are signaled.  The possible error messages */
/*     returned by this routine are: */


/*        Conjunction table is full. */

/*        Disjunction table is full. */

/*        Empty WHERE clause. */

/*        Missing WHERE keyword. */

/*        More tokens expected. */

/*        NULL values are not allowed in BETWEEN or NOT BETWEEN clauses. */

/*        NULL values can only be used with the operators */
/*        "IS NULL", "NOT NULL" or equivalents. */

/*        Relation table is full. */

/*        Stack is full. */

/*        Syntax error:  badly formed WHERE clause. */

/*        Token following BETWEEN operator is invalid. */

/*        Token following NOT operator was invalid. */

/*        Token must be followed by a comparison operator. */

/*        Token must be followed by the AND operator. */

/*        Token sequence must be followed by a value. */

/*        Tokens were missing from comparison relation. */

/*        Tokens were missing from logical expression. */

/*        Too few tokens in WHERE clause. */

/*        Too many tokens in query; max allowed is #. */

/*        Unexpected keyword # found at location #. */

/*        Unexpected right parenthesis found. */

/*        Unexpected token # found at location #. */

/*        Unexpected token found following valid expression. */

/*        Unexpected token found. */

/*        WHERE clause ran out of tokens unexpectedly. */
/*        This may be due to an extra left parenthesis. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     Here is the grammar for the EK WHERE clause: */

/*        <WHERE clause>           =>   WHERE <relational expression> */


/*        <relational expression>  =>   <simple expression> */

/*                                      <NULL value expression> */

/*                                    | NOT <relational expression> */

/*                                    |   ( <relational expression> ) */

/*                                    |     <relational expression> */
/*                                      AND <relational expression> */

/*                                    |     <relational expression> */
/*                                      OR  <relational expression> */


/*        <simple expression>      =>   <LHS> <operator> <RHS> */

/*                                    | <LHS> BETWEEN     <RHS> AND <RHS> */

/*                                    | <LHS> NOT BETWEEN <RHS> AND <RHS> */


/*        <NULL value expression>  =>   <LHS> <Null operator> NULL */


/*        <LHS>                    =>   <name> */


/*        <RHS>                    =>   <name> */
/*                                    | <value> */


/*        <name>                   =>   <identifier> . <identifier> */
/*                                    | <identifier> */


/*        <operator>               =>   EQ */
/*                                    | GE */
/*                                    | GT */
/*                                    | LE */
/*                                    | LT */
/*                                    | NE */
/*                                    | LIKE */
/*                                    | NOT LIKE */
/*                                    | = */
/*                                    | >= */
/*                                    | > */
/*                                    | <= */
/*                                    | < */
/*                                    | != */
/*                                    | <> */


/*        <NULL operator>         =>    IS */
/*                                    | IS NOT */
/*                                    | EQ */
/*                                    | NE */
/*                                    | = */
/*                                    | != */
/*                                    | <> */


/*        <value>                 =>    <character value> */
/*                                    | <d.p. value> */
/*                                    | <integer value> */

/* $ Examples */

/*     1)  This routine breaks down the constraints of the WHERE clause */

/*             WHERE        ( ( COL1 EQ VAL1 ) OR ( COL2 NE VAL2 ) ) */
/*                     AND  ( ( COL3 LE VAL3 ) OR ( COL4 GT VAL4 ) ) */

/*         as */

/*                          (  ( COL1 EQ VAL1 ) AND ( COL3 LE VAL3 ) ) */
/*                     OR   (  ( COL1 EQ VAL1 ) AND ( COL4 GT VAL4 ) ) */
/*                     OR   (  ( COL2 NE VAL2 ) AND ( COL3 LE VAL3 ) ) */
/*                     OR   (  ( COL2 NE VAL2 ) AND ( COL4 GT VAL4 ) ) */



/*     2)  This routine breaks down the constraints of the WHERE clause */

/*             WHERE  NOT ( ( COL1 EQ VAL1 ) OR ( COL2 NE VAL2 ) ) */

/*          as */
/*                          ( COL1 NE VAL1 ) AND ( COL3 EQ VAL3 ) */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman   (JPL) */

/* $ Version */

/* -    SPICELIB Version 3.0.1, 02-OCT-2021 (NJB) */

/*        Corrected typos in comments. */

/* -    Beta Version 3.0.0, 17-NOV-1995 (NJB) */

/*        Significantly re-written for architecture 3. */

/* -& */


/*     SPICELIB functions */



/*     Local parameters */


/*     Data structure bounds: */


/*     MAXREL is the maximum number of relations that can be handled */
/*     by this routine. */


/*     MAXMET is the maximum number of meta-tokens making up any */
/*     expression. */


/*     LBPOOL is the lower bound of the second index of a linked list */
/*     pool array. */



/*     Stack parameters: */



/*     Operator parameters: */


/*     NLOGOP is the number of recognized logical operators.  These */
/*     are AND, OR, and NOT. */


/*     NRELOP is the number of arithmetic and character comparison */
/*     operators. */



/*     Meta-token codes, excluding codes for relational operators: */


/*     Number of keywords that can terminate a WHERE clause. */



/*     State parameters: */



/*     'Pop condition' codes: */


/*     Token descriptor size: */


/*     Local variables */


/*     Each comparison relation is expressed by three tokens, so the */
/*     comparison relations are represented by a 3 x MAXREL array.  The */
/*     first and third elements of each row of RELS are array indices */
/*     that point into the input array TOKENS; the middle element */
/*     of each row is an operator code.  The set of triples representing */
/*     comparison relations is indexed by a doubly linked list pool. */
/*     Each conjunction of comparison relations is represented by a */
/*     linked list of pointers to entries in the RELS array.  These */
/*     pointers are contained in the CJPTRS array.  The pointers are */
/*     linked via entries in the double linked list pool CJPOOL. */


/*     Each normalized expression is a disjunction of conjunctions.  Each */
/*     such disjunction is represented by a linked list of nodes */
/*     associated with pointers to entries in the CJPOOL array.  DJPTRS */
/*     is the parallel array used to associate each node of a disjunction */
/*     with the head node of a conjunction list in CJPOOL. */


/*     Meta-tokens are groups of tokens that comprise syntactic units */
/*     in a query.  Each symbol that appears on the left hand side of */
/*     a production rule in the grammar corresponds to a type of */
/*     meta-token. */

/*     Throughout the parsing process, the meta-tokens representing the */
/*     query are organized as a linked list.  Each meta-token is also */
/*     associated with a more detailed classification MTCODE. */

/*     For each meta-token that represents an identifier, a value, */
/*     a name, or an expression, there is a corresponding element of */
/*     MTEXPR.  This element contains a pointer to a token or to a */
/*     normalized expression.  In the latter case, the pointer is the */
/*     head node of a list in the disjunction table. */


/*     Stack variables */

/*     These variables have the following meanings: */

/*        MSTART is the node number of the first meta-token of */
/*        the current expression being parsed. */

/*        NMETA is the number of meta-tokens in the query. */

/*        POPCND is the `pop condition'.  This is a code indicating */
/*        what event must occur to trigger popping the current state. */
/*        The two events that can cause the state to be popped are */
/*        the execution of a reduction and encountering a right grouper. */



/*     Other local variables */


/*     Saved variables */



/*     Initial values */


/*     Note:  there is no "UNLIKE" keyword, but there is an UNLIKE */
/*     operator, which is the complement of the LIKE operator. */



/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("ZZEKNRML", (ftnlen)8);
    }

/*     No error at this point. */

    *error = FALSE_;
    s_copy(prserr, " ", prserr_len, (ftnlen)1);
    if (*ntoken > 500) {
	*error = TRUE_;
	s_copy(prserr, "Too many tokens in query; max allowed is #.", 
		prserr_len, (ftnlen)43);
	repmi_(prserr, "#", &c__500, prserr, prserr_len, (ftnlen)1, 
		prserr_len);
	chkout_("ZZEKNRML", (ftnlen)8);
	return 0;
    }

/*     Find out the start and end indices of the tokens comprising the */
/*     WHERE clause.  If there are no tokens in the WHERE clause, we may */
/*     as well go home. */

    zzektloc_(&c__1, &c__29, ntoken, tokens, values, &whrbeg, &fnd);
    ++whrbeg;
    if (! fnd) {
	*error = TRUE_;
	s_copy(prserr, "Missing WHERE keyword.", prserr_len, (ftnlen)22);
	chkout_("ZZEKNRML", (ftnlen)8);
	return 0;
    }

/*     The WHERE clause is terminated by the end of the query or by */
/*     the first keyword of the set {SELECT, FROM, ORDER} that follows */
/*     the WHERE keyword. */

    whrend = *ntoken;
    for (i__ = 1; i__ <= 3; ++i__) {
	zzektloc_(&c__1, &endkw[(i__1 = i__ - 1) < 3 && 0 <= i__1 ? i__1 : 
		s_rnge("endkw", i__1, "zzeknrml_", (ftnlen)732)], ntoken, 
		tokens, values, &endloc, &fnd);
	if (fnd) {
	    if (endloc < whrend && endloc > whrbeg) {
		whrend = endloc - 1;
	    }
	}
    }
    whrsiz = whrend - whrbeg + 1;
    if (whrsiz == 0) {
	*error = TRUE_;
	s_copy(prserr, "Empty WHERE clause.", prserr_len, (ftnlen)19);
	chkout_("ZZEKNRML", (ftnlen)8);
	return 0;
    }

/*     Initialize the pools. */

    lnkini_(&c__5000, rlpool);
    lnkini_(&c__5000, cjpool);
    lnkini_(&c__5000, djpool);
    lnkini_(&c__500, mtpool);
    lnkini_(&c__5000, dspool);

/*     Loop through our token list and classify the tokens.  Initialize */
/*     the meta-token list. */

    nmeta = 0;
    tail = 0;
    i__ = whrbeg;
    while(i__ <= whrend) {

/*        Allocate a node and link it in at the tail of the meta-token */
/*        list. */

	lnkan_(mtpool, &node);
	lnkila_(&tail, &node, mtpool);
	tail = node;

/*        Each meta-token's expression pointer points to its original */
/*        token index, by default. */

	mtexpr[(i__1 = node - 1) < 500 && 0 <= i__1 ? i__1 : s_rnge("mtexpr", 
		i__1, "zzeknrml_", (ftnlen)788)] = i__;
	if (tokens[i__ - 1] == 6) {
	    mtcode[(i__1 = node - 1) < 500 && 0 <= i__1 ? i__1 : s_rnge("mtc"
		    "ode", i__1, "zzeknrml_", (ftnlen)793)] = -2;
	} else if (tokens[i__ - 1] == 7) {
	    mtcode[(i__1 = node - 1) < 500 && 0 <= i__1 ? i__1 : s_rnge("mtc"
		    "ode", i__1, "zzeknrml_", (ftnlen)798)] = -1;
	} else if (tokens[i__ - 1] == 3 || tokens[i__ - 1] == 4) {

/*           Numeric values must be added to the encoded query.  We */
/*           allocate a descriptor from the descriptor pool for */
/*           each identifier.  The expression pointer for the */
/*           identifier points to the descriptor.  Note:  the */
/*           allocation should be safe, since we've checked the total */
/*           number of tokens in the query. */

	    mtcode[(i__1 = node - 1) < 500 && 0 <= i__1 ? i__1 : s_rnge("mtc"
		    "ode", i__1, "zzeknrml_", (ftnlen)811)] = -8;
	    lnkan_(dspool, &mtexpr[(i__1 = node - 1) < 500 && 0 <= i__1 ? 
		    i__1 : s_rnge("mtexpr", i__1, "zzeknrml_", (ftnlen)813)]);
	    if (tokens[i__ - 1] == 3) {
		type__ = 3;
	    } else {
		type__ = 2;
	    }
	    zzekinqn_(&numvls[values[i__ - 1] - 1], &type__, &lxbegs[i__ - 1],
		     &lxends[i__ - 1], eqryi, eqryd, &dscbuf[(i__2 = mtexpr[(
		    i__1 = node - 1) < 500 && 0 <= i__1 ? i__1 : s_rnge("mte"
		    "xpr", i__1, "zzeknrml_", (ftnlen)821)] * 7 - 7) < 35000 &&
		     0 <= i__2 ? i__2 : s_rnge("dscbuf", i__2, "zzeknrml_", (
		    ftnlen)821)]);

/*           Set the descriptor to indicate that it represents a value. */

	    dscbuf[(i__2 = mtexpr[(i__1 = node - 1) < 500 && 0 <= i__1 ? i__1 
		    : s_rnge("mtexpr", i__1, "zzeknrml_", (ftnlen)832)] * 7 - 
		    1) < 35000 && 0 <= i__2 ? i__2 : s_rnge("dscbuf", i__2, 
		    "zzeknrml_", (ftnlen)832)] = -8;
	} else if (tokens[i__ - 1] == 5) {

/*           The treatment of strings is analogous to that of numbers. */

	    mtcode[(i__1 = node - 1) < 500 && 0 <= i__1 ? i__1 : s_rnge("mtc"
		    "ode", i__1, "zzeknrml_", (ftnlen)839)] = -8;
	    lnkan_(dspool, &mtexpr[(i__1 = node - 1) < 500 && 0 <= i__1 ? 
		    i__1 : s_rnge("mtexpr", i__1, "zzeknrml_", (ftnlen)841)]);
	    b = chbegs[values[i__ - 1] - 1];
	    e = chends[values[i__ - 1] - 1];
	    i__3 = e - b + 1;
	    zzekinqc_(chrbuf + (b - 1), &i__3, &lxbegs[i__ - 1], &lxends[i__ 
		    - 1], eqryi, eqryc, &dscbuf[(i__2 = mtexpr[(i__1 = node - 
		    1) < 500 && 0 <= i__1 ? i__1 : s_rnge("mtexpr", i__1, 
		    "zzeknrml_", (ftnlen)846)] * 7 - 7) < 35000 && 0 <= i__2 ?
		     i__2 : s_rnge("dscbuf", i__2, "zzeknrml_", (ftnlen)846)],
		     e - (b - 1), eqryc_len);

/*           Set the descriptor to indicate that it represents a value. */

	    dscbuf[(i__2 = mtexpr[(i__1 = node - 1) < 500 && 0 <= i__1 ? i__1 
		    : s_rnge("mtexpr", i__1, "zzeknrml_", (ftnlen)857)] * 7 - 
		    1) < 35000 && 0 <= i__2 ? i__2 : s_rnge("dscbuf", i__2, 
		    "zzeknrml_", (ftnlen)857)] = -8;
	} else if (tokens[i__ - 1] == 2) {

/*           Identifiers must be added to the encoded query.  We */
/*           allocate a descriptor from the descriptor pool for */
/*           each identifier.  The expression pointer for the */
/*           identifier points to the descriptor. */

	    mtcode[(i__1 = node - 1) < 500 && 0 <= i__1 ? i__1 : s_rnge("mtc"
		    "ode", i__1, "zzeknrml_", (ftnlen)867)] = -7;
	    lnkan_(dspool, &mtexpr[(i__1 = node - 1) < 500 && 0 <= i__1 ? 
		    i__1 : s_rnge("mtexpr", i__1, "zzeknrml_", (ftnlen)869)]);
	    b = chbegs[values[i__ - 1] - 1];
	    e = chends[values[i__ - 1] - 1];
	    i__3 = e - b + 1;
	    zzekinqc_(chrbuf + (b - 1), &i__3, &lxbegs[i__ - 1], &lxends[i__ 
		    - 1], eqryi, eqryc, &dscbuf[(i__2 = mtexpr[(i__1 = node - 
		    1) < 500 && 0 <= i__1 ? i__1 : s_rnge("mtexpr", i__1, 
		    "zzeknrml_", (ftnlen)874)] * 7 - 7) < 35000 && 0 <= i__2 ?
		     i__2 : s_rnge("dscbuf", i__2, "zzeknrml_", (ftnlen)874)],
		     e - (b - 1), eqryc_len);

/*           Set the descriptor to indicate that it represents an */
/*           identifier. */

	    dscbuf[(i__2 = mtexpr[(i__1 = node - 1) < 500 && 0 <= i__1 ? i__1 
		    : s_rnge("mtexpr", i__1, "zzeknrml_", (ftnlen)886)] * 7 - 
		    1) < 35000 && 0 <= i__2 ? i__2 : s_rnge("dscbuf", i__2, 
		    "zzeknrml_", (ftnlen)886)] = -7;
	} else if (tokens[i__ - 1] == 9) {
	    mtcode[(i__1 = node - 1) < 500 && 0 <= i__1 ? i__1 : s_rnge("mtc"
		    "ode", i__1, "zzeknrml_", (ftnlen)891)] = -9;
	} else if (tokens[i__ - 1] == 1) {

/*           We have a keyword.  Identify it and locate the corresponding */
/*           code. */

	    j = isrchi_(&values[i__ - 1], &c__3, logops);
	    k = isrchi_(&values[i__ - 1], &c__7, cmpops);
	    if (j > 0) {

/*              We have a logical operator, unless we have the NOT LIKE */
/*               or NOT BETWEEN sequence. */

		if (logcde[(i__1 = j - 1) < 3 && 0 <= i__1 ? i__1 : s_rnge(
			"logcde", i__1, "zzeknrml_", (ftnlen)908)] != -12) {
		    mtcode[(i__1 = node - 1) < 500 && 0 <= i__1 ? i__1 : 
			    s_rnge("mtcode", i__1, "zzeknrml_", (ftnlen)910)] 
			    = logcde[(i__2 = j - 1) < 3 && 0 <= i__2 ? i__2 : 
			    s_rnge("logcde", i__2, "zzeknrml_", (ftnlen)910)];
		} else {
		    if (i__ <= whrend) {
			if (tokens[i__] == 1 && values[i__] == 18) {

/*                       Replace the NOT LIKE sequence with the */
/*                       UNLIKE operator.  Skip over the LIKE token. */

			    mtcode[(i__1 = node - 1) < 500 && 0 <= i__1 ? 
				    i__1 : s_rnge("mtcode", i__1, "zzeknrml_",
				     (ftnlen)922)] = 8;
			    ++i__;
			} else if (tokens[i__] == 1 && values[i__] == 5) {

/*                       Replace the NOT BETWEEN sequence with the */
/*                       NOTBTW operator.  Skip over the BETWEEN token. */

			    mtcode[(i__1 = node - 1) < 500 && 0 <= i__1 ? 
				    i__1 : s_rnge("mtcode", i__1, "zzeknrml_",
				     (ftnlen)931)] = -4;
			    ++i__;
			} else {
			    mtcode[(i__1 = node - 1) < 500 && 0 <= i__1 ? 
				    i__1 : s_rnge("mtcode", i__1, "zzeknrml_",
				     (ftnlen)935)] = -12;
			}
		    } else {
			mtcode[(i__1 = node - 1) < 500 && 0 <= i__1 ? i__1 : 
				s_rnge("mtcode", i__1, "zzeknrml_", (ftnlen)
				939)] = -12;
		    }
		}
	    } else if (k > 0) {
		mtcode[(i__1 = node - 1) < 500 && 0 <= i__1 ? i__1 : s_rnge(
			"mtcode", i__1, "zzeknrml_", (ftnlen)946)] = cmpcde[(
			i__2 = k - 1) < 8 && 0 <= i__2 ? i__2 : s_rnge("cmpc"
			"de", i__2, "zzeknrml_", (ftnlen)946)];
	    } else if (values[i__ - 1] == 5) {
		mtcode[(i__1 = node - 1) < 500 && 0 <= i__1 ? i__1 : s_rnge(
			"mtcode", i__1, "zzeknrml_", (ftnlen)950)] = -3;
	    } else if (values[i__ - 1] == 16) {

/*              The token IS translates to EQ; the token sequence */
/*              IS NOT translates to NE. */

		if (i__ < whrend) {
		    if (tokens[i__] == 1 && values[i__] == 23) {

/*                    We have an IS NOT sequence.  Skip over the NOT */
/*                    token; indicate the sequence with a single NE */
/*                    meta-token. */

			mtcode[(i__1 = node - 1) < 500 && 0 <= i__1 ? i__1 : 
				s_rnge("mtcode", i__1, "zzeknrml_", (ftnlen)
				967)] = 6;
			++i__;
		    } else {
			mtcode[(i__1 = node - 1) < 500 && 0 <= i__1 ? i__1 : 
				s_rnge("mtcode", i__1, "zzeknrml_", (ftnlen)
				970)] = 1;
		    }
		} else {
		    mtcode[(i__1 = node - 1) < 500 && 0 <= i__1 ? i__1 : 
			    s_rnge("mtcode", i__1, "zzeknrml_", (ftnlen)974)] 
			    = 1;
		}
	    } else if (values[i__ - 1] == 24) {

/*              The expression pointer for null values is NIL. */

		mtcode[(i__1 = node - 1) < 500 && 0 <= i__1 ? i__1 : s_rnge(
			"mtcode", i__1, "zzeknrml_", (ftnlen)982)] = -8;
		mtexpr[(i__1 = node - 1) < 500 && 0 <= i__1 ? i__1 : s_rnge(
			"mtexpr", i__1, "zzeknrml_", (ftnlen)983)] = 0;
	    } else {

/*              Sorry, that was the last chance for valid keywords. */

		lxb = lxbegs[i__ - 1];
		lxe = lxends[i__ - 1];
		*error = TRUE_;
		s_copy(prserr, "Unexpected keyword # found at location #.", 
			prserr_len, (ftnlen)41);
		repmc_(prserr, "#", query + (lxb - 1), prserr, prserr_len, (
			ftnlen)1, lxe - (lxb - 1), prserr_len);
		repmi_(prserr, "#", &lxb, prserr, prserr_len, (ftnlen)1, 
			prserr_len);
		chkout_("ZZEKNRML", (ftnlen)8);
		return 0;
	    }
	} else {

/*           Sorry, that was the last chance, period. */

	    lxb = lxbegs[i__ - 1];
	    lxe = lxends[i__ - 1];
	    *error = TRUE_;
	    s_copy(prserr, "Unexpected token # found at location #.", 
		    prserr_len, (ftnlen)39);
	    repmc_(prserr, "#", query + (lxb - 1), prserr, prserr_len, (
		    ftnlen)1, lxe - (lxb - 1), prserr_len);
	    repmi_(prserr, "#", &lxb, prserr, prserr_len, (ftnlen)1, 
		    prserr_len);
	    chkout_("ZZEKNRML", (ftnlen)8);
	    return 0;
	}

/*        At this point, we've classified the Ith token.  MTCODE(NODE) */
/*        is the meta-token code for this token. */

	++i__;
	++nmeta;
    }

/*     Initialize the head of the meta-token list. */

    metahd = lnkhl_(&tail, mtpool);

/*     Filter out extraneous parentheses around column names or */
/*     values. */

    node = metahd;
    while(node > 0) {
	if (mtcode[(i__1 = node - 1) < 500 && 0 <= i__1 ? i__1 : s_rnge("mtc"
		"ode", i__1, "zzeknrml_", (ftnlen)1036)] == -6 || mtcode[(i__2 
		= node - 1) < 500 && 0 <= i__2 ? i__2 : s_rnge("mtcode", i__2,
		 "zzeknrml_", (ftnlen)1036)] == -8) {

/*           If the current metatoken is bracketed by parentheses, */
/*           remove them and update the metatoken count accordingly. */

	    prev = lnkprv_(&node, mtpool);
	    next = lnknxt_(&node, mtpool);
	    if (prev > 0 && next > 0) {
		if (mtcode[(i__1 = prev - 1) < 500 && 0 <= i__1 ? i__1 : 
			s_rnge("mtcode", i__1, "zzeknrml_", (ftnlen)1047)] == 
			-2 && mtcode[(i__2 = next - 1) < 500 && 0 <= i__2 ? 
			i__2 : s_rnge("mtcode", i__2, "zzeknrml_", (ftnlen)
			1047)] == -1) {
		    lnkfsl_(&prev, &prev, mtpool);
		    lnkfsl_(&next, &next, mtpool);
		    metahd = lnkhl_(&node, mtpool);
		    nmeta += -2;

/*                 We don't advance the current token in this case */
/*                 because there may be more parentheses to remove. */

		} else {

/*                 This token is not bracketed by parentheses; look at */
/*                 the next metatoken. */

		    node = next;
		}
	    } else {

/*              This token is not bracketed by tokens on both sides; look */
/*              at the next metatoken.  It's ok for the next token to be */
/*              NIL. */

		node = next;
	    }
	} else {

/*           The current token is not a name or value; look at the next */
/*           token. */

	    node = lnknxt_(&node, mtpool);
	}
    }


/*     Now it's time to parse our expression.  We will validate the */
/*     expression by using our grammar rules to condense groups of */
/*     meta-tokens that correspond to the right-hand sides of grammatical */
/*     rules into meta-tokens that correspond to the left-hand sides */
/*     of those same rules.  Each such application of a grammar rule */
/*     is called a `reduction.'  When we're left with a single */
/*     meta-token of type <relational expression>, we're done. */

/*     If, before reaching the desired final state, we get to a point */
/*     where no reductions can be performed, we have a syntax error. */

/*     As parsing advances, we'll start to get meta-tokens that are */
/*     logical expressions.  Each logical expression will be represented */
/*     by a data structure that organizes the expression in a way that */
/*     we'll refer to as `normalized':  the expression will be */
/*     represented as a disjunction of conjunctions, for example */

/*        ( A AND B AND C ) OR ( D AND E ) OR ( F ) OR ( G AND H AND I ) */

/*     Each metatoken that represents a logical expression will */
/*     refer to it through a pointer which is a member of the MTEXPR */
/*     array. */

    if (whrsiz < 3) {
	*error = TRUE_;
	s_copy(prserr, "Too few tokens in WHERE clause.", prserr_len, (ftnlen)
		31);
	chkout_("ZZEKNRML", (ftnlen)8);
	return 0;
    } else {
	level = 1;
	mstart[(i__1 = level - 1) < 500 && 0 <= i__1 ? i__1 : s_rnge("mstart",
		 i__1, "zzeknrml_", (ftnlen)1125)] = metahd;
	popcnd[(i__1 = level - 1) < 500 && 0 <= i__1 ? i__1 : s_rnge("popcnd",
		 i__1, "zzeknrml_", (ftnlen)1126)] = 0;
	state = 2;
    }
    while(state != 4) {
	if (state == 2) {

/*           If the input query is valid, we're looking at the leftmost */
/*           meta-token of an expression that matches the right-hand */
/*           side of one of the grammar rules.  Referring back to the */
/*           rules, we see that there are only a few meta-tokens that are */
/*           valid as the first token of such an expression: */

/*              - A left grouper */
/*              - An identifier */
/*              - A name */
/*              - An expression */
/*              - A unary operator (`NOT' ) */

/*           We'll see if we can perform a reduction.  The reductions */
/*           that are possible depend on how many meta-tokens are */
/*           present in the expression we're looking at. */

/*           FIRST is the node number of the first meta-token to look */
/*           at, in an attempt to perform a reduction.  SECOND, THIRD, */
/*           and FOURTH have the obvious meanings; some of these may */
/*           be 0. */

	    first = mstart[(i__1 = level - 1) < 500 && 0 <= i__1 ? i__1 : 
		    s_rnge("mstart", i__1, "zzeknrml_", (ftnlen)1159)];
	    if (first > 0) {
		second = lnknxt_(&first, mtpool);
	    } else {
		second = 0;
	    }
	    if (second > 0) {
		third = lnknxt_(&second, mtpool);
	    } else {
		third = 0;
	    }
	    if (third > 0) {
		fourth = lnknxt_(&third, mtpool);
	    } else {
		fourth = 0;
	    }
	    if (first <= 0) {

/*              This never happens to good commands. */

		*error = TRUE_;
		s_copy(prserr, "WHERE clause ran out of tokens unexpectedly."
			"  This may be due to an extra left parenthesis.", 
			prserr_len, (ftnlen)91);
		chkout_("ZZEKNRML", (ftnlen)8);
		return 0;
	    }

/*           We have at least one meta-token to work with.  We'll */
/*           take different actions depending on its type. */

	    if (mtcode[(i__1 = first - 1) < 500 && 0 <= i__1 ? i__1 : s_rnge(
		    "mtcode", i__1, "zzeknrml_", (ftnlen)1199)] == -7) {

/*              This is a simple case to deal with:  in valid queries, */
/*              we have either the sequence */

/*                 <identifier> . <identifier> */

/*              or */

/*                 <identifier> */

/*              Both of these token sequences represent a column name; */
/*              in the former case, the name is qualified by a table */
/*              name, in the latter, the column name is unqualified. */
/*              If the table name is absent, we'll simply save a null */
/*              descriptor for it.  The descriptors will be linked, with */
/*              the table descriptor coming first, and the NAME token */
/*              resulting from reducing this token sequence will point to */
/*              the list of descriptors via the MTEXPR pointer. */


		if (third > 0) {

/*                 We can look at the following two tokens. */

		    if (mtcode[(i__1 = second - 1) < 500 && 0 <= i__1 ? i__1 :
			     s_rnge("mtcode", i__1, "zzeknrml_", (ftnlen)1224)
			    ] == -9 && mtcode[(i__2 = third - 1) < 500 && 0 <=
			     i__2 ? i__2 : s_rnge("mtcode", i__2, "zzeknrml_",
			     (ftnlen)1224)] == -7) {
			qual = TRUE_;
		    } else {
			qual = FALSE_;
		    }
		} else {

/*                 There aren't enough tokens for this name to be */
/*                 qualified. */

		    qual = FALSE_;
		}
		if (qual) {

/*                 We have a fully qualified column name.  Hook up the */
/*                 table and column name descriptors. */

		    tabptr = mtexpr[(i__1 = first - 1) < 500 && 0 <= i__1 ? 
			    i__1 : s_rnge("mtexpr", i__1, "zzeknrml_", (
			    ftnlen)1247)];
		    colptr = mtexpr[(i__1 = third - 1) < 500 && 0 <= i__1 ? 
			    i__1 : s_rnge("mtexpr", i__1, "zzeknrml_", (
			    ftnlen)1248)];
		    lnkila_(&tabptr, &colptr, dspool);

/*                 Reduce the expression to a <name> metatoken. */

		    mtcode[(i__1 = first - 1) < 500 && 0 <= i__1 ? i__1 : 
			    s_rnge("mtcode", i__1, "zzeknrml_", (ftnlen)1255)]
			     = -6;
		    lnkfsl_(&second, &third, mtpool);
		    nmeta += -2;
		} else {

/*                 We have an unqualified column name.  Allocate a table */
/*                 descriptor.  Set the table descriptor to indicate a */
/*                 null character descriptor.  Link this descriptor in */
/*                 before the column descriptor. */

		    lnkan_(dspool, &tabptr);
		    cleari_(&c__7, &dscbuf[(i__1 = tabptr * 7 - 7) < 35000 && 
			    0 <= i__1 ? i__1 : s_rnge("dscbuf", i__1, "zzekn"
			    "rml_", (ftnlen)1270)]);
		    dscbuf[(i__1 = tabptr * 7 - 7) < 35000 && 0 <= i__1 ? 
			    i__1 : s_rnge("dscbuf", i__1, "zzeknrml_", (
			    ftnlen)1271)] = 1;
		    dscbuf[(i__1 = tabptr * 7 - 1) < 35000 && 0 <= i__1 ? 
			    i__1 : s_rnge("dscbuf", i__1, "zzeknrml_", (
			    ftnlen)1272)] = -7;
		    colptr = mtexpr[(i__1 = first - 1) < 500 && 0 <= i__1 ? 
			    i__1 : s_rnge("mtexpr", i__1, "zzeknrml_", (
			    ftnlen)1274)];
		    lnkila_(&tabptr, &colptr, dspool);

/*                 Reduce the expression to a <name> metatoken. */
/*                 The reduction doesn't change the number of metatokens. */

		    mtexpr[(i__1 = first - 1) < 500 && 0 <= i__1 ? i__1 : 
			    s_rnge("mtexpr", i__1, "zzeknrml_", (ftnlen)1282)]
			     = tabptr;
		    mtcode[(i__1 = first - 1) < 500 && 0 <= i__1 ? i__1 : 
			    s_rnge("mtcode", i__1, "zzeknrml_", (ftnlen)1283)]
			     = -6;
		}

/*              Decide the next state. */

		state = 3;
	    } else if (mtcode[(i__1 = first - 1) < 500 && 0 <= i__1 ? i__1 : 
		    s_rnge("mtcode", i__1, "zzeknrml_", (ftnlen)1294)] == -8) 
		    {

/*              If the query is valid, the sequence of meta-tokens */
/*              should be one of */

/*                 <value>  AND  <name> */
/*                 <value>  AND  <value> */

/*              Both of these reduce to the symbol <BETWEEN expr>. */


		if (third <= 0) {
		    *error = TRUE_;
		    s_copy(prserr, "Tokens were missing from comparison rela"
			    "tion.", prserr_len, (ftnlen)45);
		    chkout_("ZZEKNRML", (ftnlen)8);
		    return 0;
		}

/*              Null values are not allowed in BETWEEN expressions. */

		if (mtexpr[(i__1 = first - 1) < 500 && 0 <= i__1 ? i__1 : 
			s_rnge("mtexpr", i__1, "zzeknrml_", (ftnlen)1318)] == 
			0 || mtexpr[(i__2 = third - 1) < 500 && 0 <= i__2 ? 
			i__2 : s_rnge("mtexpr", i__2, "zzeknrml_", (ftnlen)
			1318)] == 0) {
		    *error = TRUE_;
		    s_copy(prserr, "NULL values are not allowed in BETWEEN o"
			    "r NOT BETWEEN clauses.", prserr_len, (ftnlen)62);
		    chkout_("ZZEKNRML", (ftnlen)8);
		    return 0;
		}
		if (mtcode[(i__1 = third - 1) < 500 && 0 <= i__1 ? i__1 : 
			s_rnge("mtcode", i__1, "zzeknrml_", (ftnlen)1330)] == 
			-7) {

/*                 We'll need to reduce the IDENT before proceeding. */

		    start = third;
		    retcnd = 1;
		    state = 0;
		} else if (mtcode[(i__1 = second - 1) < 500 && 0 <= i__1 ? 
			i__1 : s_rnge("mtcode", i__1, "zzeknrml_", (ftnlen)
			1339)] == -10 && (mtcode[(i__2 = third - 1) < 500 && 
			0 <= i__2 ? i__2 : s_rnge("mtcode", i__2, "zzeknrml_",
			 (ftnlen)1339)] == -6 || mtcode[(i__3 = third - 1) < 
			500 && 0 <= i__3 ? i__3 : s_rnge("mtcode", i__3, 
			"zzeknrml_", (ftnlen)1339)] == -8)) {

/*                 This sequence of tokens, when seen in the PARSE */
/*                 state, is a set of value bounds for a BETWEEN or */
/*                 NOT BETWEEN expression.  Note that this token sequence */
/*                 can occur elsewhere, but not in the PARSE state. */
/*                 This is because the meta-token sequences */

/*                    <value>  AND  <name> */
/*                    <value>  AND  <value> */

/*                 occur at the start of the RHS of only two */
/*                 productions, namely */

/*                    <BETWEEN expr>  =>  <value>  AND  <name> */
/*                    <BETWEEN expr>  =>  <value>  AND  <value> */


/*                 Hook up the name or value descriptors. */

		    lnkilb_(&mtexpr[(i__1 = first - 1) < 500 && 0 <= i__1 ? 
			    i__1 : s_rnge("mtexpr", i__1, "zzeknrml_", (
			    ftnlen)1362)], &mtexpr[(i__2 = third - 1) < 500 &&
			     0 <= i__2 ? i__2 : s_rnge("mtexpr", i__2, "zzek"
			    "nrml_", (ftnlen)1362)], dspool);
		    mtcode[(i__1 = first - 1) < 500 && 0 <= i__1 ? i__1 : 
			    s_rnge("mtcode", i__1, "zzeknrml_", (ftnlen)1364)]
			     = -5;
		    lnkfsl_(&second, &third, mtpool);
		    nmeta += -2;

/*                 Decide the next state. */

		    state = 3;
		} else if (mtcode[(i__1 = second - 1) < 500 && 0 <= i__1 ? 
			i__1 : s_rnge("mtcode", i__1, "zzeknrml_", (ftnlen)
			1376)] > 0) {

/*                 The third meta-token is in the wrong place at the */
/*                 wrong time. */

		    *error = TRUE_;
		    s_copy(prserr, "Token sequence must be followed by a val"
			    "ue.", prserr_len, (ftnlen)43);
		    chkout_("ZZEKNRML", (ftnlen)8);
		    return 0;
		} else {

/*                 The second meta-token is supposed to be the AND token, */
/*                 but it's actually something else. */

		    *error = TRUE_;
		    s_copy(prserr, "Token must be followed by the AND operat"
			    "or.", prserr_len, (ftnlen)43);
		    chkout_("ZZEKNRML", (ftnlen)8);
		    return 0;
		}
	    } else if (mtcode[(i__1 = first - 1) < 500 && 0 <= i__1 ? i__1 : 
		    s_rnge("mtcode", i__1, "zzeknrml_", (ftnlen)1406)] == -6) 
		    {

/*              If the query is valid, the sequence of meta-tokens */
/*              should be any of */

/*                 <name>  <comparison operator> <value> */
/*                 <name>  <comparison operator> <name> */
/*                 <name>  <comparison operator> <ident> */

/*              or */

/*                 <name>  AND  <name> */
/*                 <name>  AND  <value> */
/*                 <name>  AND  <ident> */

/*              or */

/*                 <name>  BETWEEN  <BETWEEN expr> */
/*                 <name>  BETWEEN  <name> */
/*                 <name>  BETWEEN  <value> */
/*                 <name>  BETWEEN  <ident> */

/*              or */

/*                 <name>  <NOT BETWEEN>  <BETWEEN expr> */
/*                 <name>  <NOT BETWEEN>  <name> */
/*                 <name>  <NOT BETWEEN>  <value> */
/*                 <name>  <NOT BETWEEN>  <ident> */

/*              There must be at least three meta-tokens here. */


		if (third <= 0) {
		    *error = TRUE_;
		    s_copy(prserr, "Tokens were missing from comparison rela"
			    "tion.", prserr_len, (ftnlen)45);
		    chkout_("ZZEKNRML", (ftnlen)8);
		    return 0;
		}
		if (mtcode[(i__1 = third - 1) < 500 && 0 <= i__1 ? i__1 : 
			s_rnge("mtcode", i__1, "zzeknrml_", (ftnlen)1449)] == 
			-7) {

/*                 We'll need to reduce the IDENT before proceeding. */

		    start = third;
		    retcnd = 1;
		    state = 0;
		} else if (mtcode[(i__1 = second - 1) < 500 && 0 <= i__1 ? 
			i__1 : s_rnge("mtcode", i__1, "zzeknrml_", (ftnlen)
			1458)] == -10 && (mtcode[(i__2 = third - 1) < 500 && 
			0 <= i__2 ? i__2 : s_rnge("mtcode", i__2, "zzeknrml_",
			 (ftnlen)1458)] == -6 || mtcode[(i__3 = third - 1) < 
			500 && 0 <= i__3 ? i__3 : s_rnge("mtcode", i__3, 
			"zzeknrml_", (ftnlen)1458)] == -8)) {

/*                 This sequence of tokens, when seen in the PARSE */
/*                 state, is a set of value bounds for a BETWEEN or */
/*                 NOT BETWEEN expression.  Note that this token sequence */
/*                 can occur elsewhere, but not in the PARSE state. */
/*                 This is because the meta-token sequences */

/*                    <name>  AND  <name> */
/*                    <name>  AND  <value> */

/*                 occur at the start of the RHS of only two */
/*                 productions, namely */

/*                    <BETWEEN expr>  =>  <name>   AND  <name> */
/*                    <BETWEEN expr>  =>  <name>   AND  <value> */


/*                 Null values are not allowed in BETWEEN expressions. */

		    if (mtexpr[(i__1 = first - 1) < 500 && 0 <= i__1 ? i__1 : 
			    s_rnge("mtexpr", i__1, "zzeknrml_", (ftnlen)1481)]
			     == 0 || mtexpr[(i__2 = third - 1) < 500 && 0 <= 
			    i__2 ? i__2 : s_rnge("mtexpr", i__2, "zzeknrml_", 
			    (ftnlen)1481)] == 0) {
			*error = TRUE_;
			s_copy(prserr, "NULL values are not allowed in BETWE"
				"EN or NOT BETWEEN clauses.", prserr_len, (
				ftnlen)62);
			chkout_("ZZEKNRML", (ftnlen)8);
			return 0;
		    }

/*                 Hook up the name or value descriptors. */

		    lnkilb_(&mtexpr[(i__1 = first - 1) < 500 && 0 <= i__1 ? 
			    i__1 : s_rnge("mtexpr", i__1, "zzeknrml_", (
			    ftnlen)1495)], &mtexpr[(i__2 = third - 1) < 500 &&
			     0 <= i__2 ? i__2 : s_rnge("mtexpr", i__2, "zzek"
			    "nrml_", (ftnlen)1495)], dspool);
		    mtcode[(i__1 = first - 1) < 500 && 0 <= i__1 ? i__1 : 
			    s_rnge("mtcode", i__1, "zzeknrml_", (ftnlen)1497)]
			     = -5;
		    lnkfsl_(&second, &third, mtpool);
		    nmeta += -2;

/*                 Decide the next state. */

		    state = 3;
		} else if (mtcode[(i__1 = second - 1) < 500 && 0 <= i__1 ? 
			i__1 : s_rnge("mtcode", i__1, "zzeknrml_", (ftnlen)
			1510)] > 0 && (mtcode[(i__2 = third - 1) < 500 && 0 <=
			 i__2 ? i__2 : s_rnge("mtcode", i__2, "zzeknrml_", (
			ftnlen)1510)] == -6 || mtcode[(i__3 = third - 1) < 
			500 && 0 <= i__3 ? i__3 : s_rnge("mtcode", i__3, 
			"zzeknrml_", (ftnlen)1510)] == -8)) {

/*                 Positive meta-token codes denote comparison */
/*                 operators. */

/*                 We have an arithmetic, string, or column comparison */
/*                 expression.  This is a trivial normalized */
/*                 relational expression.  All we have to do */
/*                 is store the expression in the relation table, */
/*                 and free the second and third meta-tokens. */

		    if (lnknfn_(rlpool) < 1) {
			*error = TRUE_;
			s_copy(prserr, "Relation table is full.", prserr_len, 
				(ftnlen)23);
			chkout_("ZZEKNRML", (ftnlen)8);
			return 0;
		    }
		    lnkan_(rlpool, &newrel);
		    rels[(i__1 = newrel * 3 - 3) < 15000 && 0 <= i__1 ? i__1 :
			     s_rnge("rels", i__1, "zzeknrml_", (ftnlen)1533)] 
			    = mtexpr[(i__2 = first - 1) < 500 && 0 <= i__2 ? 
			    i__2 : s_rnge("mtexpr", i__2, "zzeknrml_", (
			    ftnlen)1533)];
		    rels[(i__1 = newrel * 3 - 2) < 15000 && 0 <= i__1 ? i__1 :
			     s_rnge("rels", i__1, "zzeknrml_", (ftnlen)1534)] 
			    = mtcode[(i__2 = second - 1) < 500 && 0 <= i__2 ? 
			    i__2 : s_rnge("mtcode", i__2, "zzeknrml_", (
			    ftnlen)1534)];
		    rels[(i__1 = newrel * 3 - 1) < 15000 && 0 <= i__1 ? i__1 :
			     s_rnge("rels", i__1, "zzeknrml_", (ftnlen)1535)] 
			    = mtexpr[(i__2 = third - 1) < 500 && 0 <= i__2 ? 
			    i__2 : s_rnge("mtexpr", i__2, "zzeknrml_", (
			    ftnlen)1535)];
		    lnkfsl_(&second, &third, mtpool);
		    nmeta += -2;

/*                 Now allocate an entry in the conjunction pool */
/*                 and make this entry point to the relation table */
/*                 entry. */

		    if (lnknfn_(cjpool) < 1) {
			*error = TRUE_;
			s_copy(prserr, "Conjunction table is full.", 
				prserr_len, (ftnlen)26);
			chkout_("ZZEKNRML", (ftnlen)8);
			return 0;
		    }
		    lnkan_(cjpool, &newcj);
		    cjptrs[(i__1 = newcj - 1) < 5000 && 0 <= i__1 ? i__1 : 
			    s_rnge("cjptrs", i__1, "zzeknrml_", (ftnlen)1556)]
			     = newrel;

/*                 Now allocate an entry in the disjunction pool */
/*                 and make this entry point to the conjunction pool */
/*                 entry. */

		    if (lnknfn_(djpool) < 1) {
			*error = TRUE_;
			s_copy(prserr, "Disjunction table is full.", 
				prserr_len, (ftnlen)26);
			chkout_("ZZEKNRML", (ftnlen)8);
			return 0;
		    }
		    lnkan_(djpool, &newdj);
		    djptrs[(i__1 = newdj - 1) < 5000 && 0 <= i__1 ? i__1 : 
			    s_rnge("djptrs", i__1, "zzeknrml_", (ftnlen)1572)]
			     = newcj;

/*                 Change the type of the first meta-token to EXPR and */
/*                 have that meta-token point to this table entry.  Bag */
/*                 the other two meta-tokens. */

		    mtcode[(i__1 = first - 1) < 500 && 0 <= i__1 ? i__1 : 
			    s_rnge("mtcode", i__1, "zzeknrml_", (ftnlen)1579)]
			     = -13;
		    mtexpr[(i__1 = first - 1) < 500 && 0 <= i__1 ? i__1 : 
			    s_rnge("mtexpr", i__1, "zzeknrml_", (ftnlen)1580)]
			     = newdj;

/*                 Decide the next state. */

		    state = 3;
		} else if (mtcode[(i__1 = second - 1) < 500 && 0 <= i__1 ? 
			i__1 : s_rnge("mtcode", i__1, "zzeknrml_", (ftnlen)
			1588)] == -3 || mtcode[(i__2 = second - 1) < 500 && 0 
			<= i__2 ? i__2 : s_rnge("mtcode", i__2, "zzeknrml_", (
			ftnlen)1588)] == -4) {

/*                 If the command is syntactically correct, the */
/*                 meta-token sequence should be one of: */

/*                    <name>  <BETWEEN>      <BETWEEN expr> */
/*                    <name>  <BETWEEN>      <value> */
/*                    <name>  <BETWEEN>      <name> */
/*                    <name>  <NOT BETWEEN>  <BETWEEN expr> */
/*                    <name>  <NOT BETWEEN>  <value> */
/*                    <name>  <NOT BETWEEN>  <name> */


		    if (mtcode[(i__1 = second - 1) < 500 && 0 <= i__1 ? i__1 :
			     s_rnge("mtcode", i__1, "zzeknrml_", (ftnlen)1602)
			    ] == -3 && mtcode[(i__2 = third - 1) < 500 && 0 <=
			     i__2 ? i__2 : s_rnge("mtcode", i__2, "zzeknrml_",
			     (ftnlen)1602)] == -5) {

/*                    It's a BETWEEN comparison.  We treat this as a */
/*                    disjunction of conjunctions of comparison */
/*                    relations: */
/*                                      <name>   >=   <item1> */
/*                              AND     <name>   <=   <item2> */

/*                         OR */
/*                                      <name>   <=   <item1> */
/*                              AND     <name>   >=   <item2> */

/*                    where item1 and item2 are specified by the */
/*                    descriptors belonging to the third meta-token. */

			for (i__ = 1; i__ <= 4; ++i__) {
			    if (i__ == 1 || i__ == 3) {
				k = mtexpr[(i__1 = third - 1) < 500 && 0 <= 
					i__1 ? i__1 : s_rnge("mtexpr", i__1, 
					"zzeknrml_", (ftnlen)1622)];
			    } else {

/*                          We need the descriptor pointer for the RHS */
/*                          item.  This descriptor is linked to the tail */
/*                          of the descriptor for the LHS item.  The */
/*                          number of nodes to skip over depends on */
/*                          whether the LHS item is a name or value. */

				k = mtexpr[(i__1 = third - 1) < 500 && 0 <= 
					i__1 ? i__1 : s_rnge("mtexpr", i__1, 
					"zzeknrml_", (ftnlen)1632)];
				if (dscbuf[(i__1 = k * 7 - 1) < 35000 && 0 <= 
					i__1 ? i__1 : s_rnge("dscbuf", i__1, 
					"zzeknrml_", (ftnlen)1634)] == -7) {
				    skip = 1;
				} else {
				    skip = 0;
				}
				i__1 = skip + 1;
				for (j = 1; j <= i__1; ++j) {
				    k = lnknxt_(&k, dspool);
				}
			    }
			    if (lnknfn_(rlpool) < 1) {
				*error = TRUE_;
				s_copy(prserr, "Relation table is full.", 
					prserr_len, (ftnlen)23);
				chkout_("ZZEKNRML", (ftnlen)8);
				return 0;
			    }
			    lnkan_(rlpool, &rel[(i__1 = i__ - 1) < 4 && 0 <= 
				    i__1 ? i__1 : s_rnge("rel", i__1, "zzekn"
				    "rml_", (ftnlen)1654)]);
			    rels[(i__2 = rel[(i__1 = i__ - 1) < 4 && 0 <= 
				    i__1 ? i__1 : s_rnge("rel", i__1, "zzekn"
				    "rml_", (ftnlen)1657)] * 3 - 3) < 15000 && 
				    0 <= i__2 ? i__2 : s_rnge("rels", i__2, 
				    "zzeknrml_", (ftnlen)1657)] = mtexpr[(
				    i__3 = first - 1) < 500 && 0 <= i__3 ? 
				    i__3 : s_rnge("mtexpr", i__3, "zzeknrml_",
				     (ftnlen)1657)];
			    if (i__ == 1 || i__ == 4) {
				rels[(i__2 = rel[(i__1 = i__ - 1) < 4 && 0 <= 
					i__1 ? i__1 : s_rnge("rel", i__1, 
					"zzeknrml_", (ftnlen)1660)] * 3 - 2) <
					 15000 && 0 <= i__2 ? i__2 : s_rnge(
					"rels", i__2, "zzeknrml_", (ftnlen)
					1660)] = 2;
			    } else {
				rels[(i__2 = rel[(i__1 = i__ - 1) < 4 && 0 <= 
					i__1 ? i__1 : s_rnge("rel", i__1, 
					"zzeknrml_", (ftnlen)1662)] * 3 - 2) <
					 15000 && 0 <= i__2 ? i__2 : s_rnge(
					"rels", i__2, "zzeknrml_", (ftnlen)
					1662)] = 4;
			    }
			    rels[(i__2 = rel[(i__1 = i__ - 1) < 4 && 0 <= 
				    i__1 ? i__1 : s_rnge("rel", i__1, "zzekn"
				    "rml_", (ftnlen)1665)] * 3 - 1) < 15000 && 
				    0 <= i__2 ? i__2 : s_rnge("rels", i__2, 
				    "zzeknrml_", (ftnlen)1665)] = k;
			    if (lnknfn_(cjpool) < 1) {
				*error = TRUE_;
				s_copy(prserr, "Conjunction table is full.", 
					prserr_len, (ftnlen)26);
				chkout_("ZZEKNRML", (ftnlen)8);
				return 0;
			    }
			    lnkan_(cjpool, &cj[(i__1 = i__ - 1) < 4 && 0 <= 
				    i__1 ? i__1 : s_rnge("cj", i__1, "zzeknr"
				    "ml_", (ftnlen)1675)]);
			    cjptrs[(i__2 = cj[(i__1 = i__ - 1) < 4 && 0 <= 
				    i__1 ? i__1 : s_rnge("cj", i__1, "zzeknr"
				    "ml_", (ftnlen)1677)] - 1) < 5000 && 0 <= 
				    i__2 ? i__2 : s_rnge("cjptrs", i__2, 
				    "zzeknrml_", (ftnlen)1677)] = rel[(i__3 = 
				    i__ - 1) < 4 && 0 <= i__3 ? i__3 : s_rnge(
				    "rel", i__3, "zzeknrml_", (ftnlen)1677)];
			}

/*                    Link the conjunction nodes to form the two */
/*                    conjunctions shown above. */

			lnkila_(cj, &cj[1], cjpool);
			lnkila_(&cj[2], &cj[3], cjpool);

/*                    Allocate disjunction pool entries and make them */
/*                    point to the two respective conjunctions. */

			for (i__ = 1; i__ <= 2; ++i__) {
			    if (lnknfn_(djpool) < 1) {
				*error = TRUE_;
				s_copy(prserr, "Disjunction table is full.", 
					prserr_len, (ftnlen)26);
				chkout_("ZZEKNRML", (ftnlen)8);
				return 0;
			    }
			    lnkan_(djpool, &dj[(i__1 = i__ - 1) < 2 && 0 <= 
				    i__1 ? i__1 : s_rnge("dj", i__1, "zzeknr"
				    "ml_", (ftnlen)1701)]);
			    djptrs[(i__2 = dj[(i__1 = i__ - 1) < 2 && 0 <= 
				    i__1 ? i__1 : s_rnge("dj", i__1, "zzeknr"
				    "ml_", (ftnlen)1702)] - 1) < 5000 && 0 <= 
				    i__2 ? i__2 : s_rnge("djptrs", i__2, 
				    "zzeknrml_", (ftnlen)1702)] = cj[(i__3 = (
				    i__ << 1) - 2) < 4 && 0 <= i__3 ? i__3 : 
				    s_rnge("cj", i__3, "zzeknrml_", (ftnlen)
				    1702)];
			}

/*                    Finally, link the disjunction pool entries, and */
/*                    create an <expression> meta-token.  Free the unused */
/*                    meta-tokens. */

			lnkila_(dj, &dj[1], djpool);
			mtcode[(i__1 = first - 1) < 500 && 0 <= i__1 ? i__1 : 
				s_rnge("mtcode", i__1, "zzeknrml_", (ftnlen)
				1713)] = -13;
			mtexpr[(i__1 = first - 1) < 500 && 0 <= i__1 ? i__1 : 
				s_rnge("mtexpr", i__1, "zzeknrml_", (ftnlen)
				1714)] = dj[0];
			lnkfsl_(&second, &third, mtpool);
			nmeta += -2;

/*                    Decide the next state. */

			state = 3;
		    } else if (mtcode[(i__1 = second - 1) < 500 && 0 <= i__1 ?
			     i__1 : s_rnge("mtcode", i__1, "zzeknrml_", (
			    ftnlen)1725)] == -4 && mtcode[(i__2 = third - 1) <
			     500 && 0 <= i__2 ? i__2 : s_rnge("mtcode", i__2, 
			    "zzeknrml_", (ftnlen)1725)] == -5) {

/*                    It's a NOT BETWEEN comparison.  We treat */
/*                    this as a disjunction of conjunctions of comparison */
/*                    relations: */

/*                                      <name>   <   <item1> */
/*                              AND     <name>   <   <item2> */

/*                         OR */
/*                                      <name>   >   <item1> */
/*                              AND     <name>   >   <item2> */

/*                    where item1 and item2 are specified by the */
/*                    descriptors belonging to the third meta-token. */

/*                    The actions here are closely analogous to those */
/*                    for the BETWEEN case. */

			for (i__ = 1; i__ <= 4; ++i__) {
			    if (i__ == 1 || i__ == 3) {
				k = mtexpr[(i__1 = third - 1) < 500 && 0 <= 
					i__1 ? i__1 : s_rnge("mtexpr", i__1, 
					"zzeknrml_", (ftnlen)1749)];
			    } else {

/*                          We need the descriptor pointer for the RHS */
/*                          item.  This descriptor is linked to the tail */
/*                          of the descriptor for the LHS item.  The */
/*                          number of nodes to skip over depends on */
/*                          whether the LHS item is a name or value. */

				k = mtexpr[(i__1 = third - 1) < 500 && 0 <= 
					i__1 ? i__1 : s_rnge("mtexpr", i__1, 
					"zzeknrml_", (ftnlen)1759)];
				if (dscbuf[(i__1 = k * 7 - 1) < 35000 && 0 <= 
					i__1 ? i__1 : s_rnge("dscbuf", i__1, 
					"zzeknrml_", (ftnlen)1761)] == -7) {
				    skip = 1;
				} else {
				    skip = 0;
				}
				i__1 = skip + 1;
				for (j = 1; j <= i__1; ++j) {
				    k = lnknxt_(&k, dspool);
				}
			    }
			    if (lnknfn_(rlpool) < 1) {
				*error = TRUE_;
				s_copy(prserr, "Relation table is full.", 
					prserr_len, (ftnlen)23);
				chkout_("ZZEKNRML", (ftnlen)8);
				return 0;
			    }
			    lnkan_(rlpool, &rel[(i__1 = i__ - 1) < 4 && 0 <= 
				    i__1 ? i__1 : s_rnge("rel", i__1, "zzekn"
				    "rml_", (ftnlen)1781)]);
			    rels[(i__2 = rel[(i__1 = i__ - 1) < 4 && 0 <= 
				    i__1 ? i__1 : s_rnge("rel", i__1, "zzekn"
				    "rml_", (ftnlen)1784)] * 3 - 3) < 15000 && 
				    0 <= i__2 ? i__2 : s_rnge("rels", i__2, 
				    "zzeknrml_", (ftnlen)1784)] = mtexpr[(
				    i__3 = first - 1) < 500 && 0 <= i__3 ? 
				    i__3 : s_rnge("mtexpr", i__3, "zzeknrml_",
				     (ftnlen)1784)];
			    if (i__ <= 2) {
				rels[(i__2 = rel[(i__1 = i__ - 1) < 4 && 0 <= 
					i__1 ? i__1 : s_rnge("rel", i__1, 
					"zzeknrml_", (ftnlen)1787)] * 3 - 2) <
					 15000 && 0 <= i__2 ? i__2 : s_rnge(
					"rels", i__2, "zzeknrml_", (ftnlen)
					1787)] = 5;
			    } else {
				rels[(i__2 = rel[(i__1 = i__ - 1) < 4 && 0 <= 
					i__1 ? i__1 : s_rnge("rel", i__1, 
					"zzeknrml_", (ftnlen)1789)] * 3 - 2) <
					 15000 && 0 <= i__2 ? i__2 : s_rnge(
					"rels", i__2, "zzeknrml_", (ftnlen)
					1789)] = 3;
			    }
			    rels[(i__2 = rel[(i__1 = i__ - 1) < 4 && 0 <= 
				    i__1 ? i__1 : s_rnge("rel", i__1, "zzekn"
				    "rml_", (ftnlen)1792)] * 3 - 1) < 15000 && 
				    0 <= i__2 ? i__2 : s_rnge("rels", i__2, 
				    "zzeknrml_", (ftnlen)1792)] = k;
			    if (lnknfn_(cjpool) < 1) {
				*error = TRUE_;
				s_copy(prserr, "Conjunction table is full.", 
					prserr_len, (ftnlen)26);
				chkout_("ZZEKNRML", (ftnlen)8);
				return 0;
			    }
			    lnkan_(cjpool, &cj[(i__1 = i__ - 1) < 4 && 0 <= 
				    i__1 ? i__1 : s_rnge("cj", i__1, "zzeknr"
				    "ml_", (ftnlen)1802)]);
			    cjptrs[(i__2 = cj[(i__1 = i__ - 1) < 4 && 0 <= 
				    i__1 ? i__1 : s_rnge("cj", i__1, "zzeknr"
				    "ml_", (ftnlen)1804)] - 1) < 5000 && 0 <= 
				    i__2 ? i__2 : s_rnge("cjptrs", i__2, 
				    "zzeknrml_", (ftnlen)1804)] = rel[(i__3 = 
				    i__ - 1) < 4 && 0 <= i__3 ? i__3 : s_rnge(
				    "rel", i__3, "zzeknrml_", (ftnlen)1804)];
			}

/*                    Link the conjunction nodes to form the two */
/*                    conjunctions shown above. */

			lnkila_(cj, &cj[1], cjpool);
			lnkila_(&cj[2], &cj[3], cjpool);

/*                    Allocate disjunction pool entries and make them */
/*                    point to the two respective conjunctions. */

			for (i__ = 1; i__ <= 2; ++i__) {
			    if (lnknfn_(djpool) < 1) {
				*error = TRUE_;
				s_copy(prserr, "Disjunction table is full.", 
					prserr_len, (ftnlen)26);
				chkout_("ZZEKNRML", (ftnlen)8);
				return 0;
			    }
			    lnkan_(djpool, &dj[(i__1 = i__ - 1) < 2 && 0 <= 
				    i__1 ? i__1 : s_rnge("dj", i__1, "zzeknr"
				    "ml_", (ftnlen)1828)]);
			    djptrs[(i__2 = dj[(i__1 = i__ - 1) < 2 && 0 <= 
				    i__1 ? i__1 : s_rnge("dj", i__1, "zzeknr"
				    "ml_", (ftnlen)1829)] - 1) < 5000 && 0 <= 
				    i__2 ? i__2 : s_rnge("djptrs", i__2, 
				    "zzeknrml_", (ftnlen)1829)] = cj[(i__3 = (
				    i__ << 1) - 2) < 4 && 0 <= i__3 ? i__3 : 
				    s_rnge("cj", i__3, "zzeknrml_", (ftnlen)
				    1829)];
			}

/*                    Finally, link the disjunction pool entries, and */
/*                    create an <expression> meta-token.  Free the unused */
/*                    meta-tokens. */

			lnkila_(dj, &dj[1], djpool);
			mtcode[(i__1 = first - 1) < 500 && 0 <= i__1 ? i__1 : 
				s_rnge("mtcode", i__1, "zzeknrml_", (ftnlen)
				1840)] = -13;
			mtexpr[(i__1 = first - 1) < 500 && 0 <= i__1 ? i__1 : 
				s_rnge("mtexpr", i__1, "zzeknrml_", (ftnlen)
				1841)] = dj[0];
			lnkfsl_(&second, &third, mtpool);
			nmeta += -2;

/*                    Decide the next state. */

			state = 3;
		    } else if (mtcode[(i__1 = third - 1) < 500 && 0 <= i__1 ? 
			    i__1 : s_rnge("mtcode", i__1, "zzeknrml_", (
			    ftnlen)1851)] == -6 || mtcode[(i__2 = third - 1) <
			     500 && 0 <= i__2 ? i__2 : s_rnge("mtcode", i__2, 
			    "zzeknrml_", (ftnlen)1851)] == -8) {

/*                    If the third meta-token is anything other than */
/*                    <BETWEEN expr>, we'll have to parse the portion of */
/*                    the query following the BETWEEN keyword before */
/*                    reducing the <BETWEEN> or <NOT BETWEEN> expression. */

			start = third;
			retcnd = 1;
			state = 0;
		    } else {
			*error = TRUE_;
			s_copy(prserr, "Token following BETWEEN operator is "
				"invalid.", prserr_len, (ftnlen)44);
			chkout_("ZZEKNRML", (ftnlen)8);
			return 0;
		    }
		} else if (mtcode[(i__1 = second - 1) < 500 && 0 <= i__1 ? 
			i__1 : s_rnge("mtcode", i__1, "zzeknrml_", (ftnlen)
			1874)] > 0) {

/*                 The third meta-token is in the wrong place at the */
/*                 wrong time. */

		    *error = TRUE_;
		    s_copy(prserr, "Token sequence must be followed by a val"
			    "ue.", prserr_len, (ftnlen)43);
		    chkout_("ZZEKNRML", (ftnlen)8);
		    return 0;
		} else {

/*                 The second meta-token is supposed to be a comparison */
/*                 operator, but it's actually something else. */

		    *error = TRUE_;
		    s_copy(prserr, "Token must be followed by a comparison o"
			    "perator.", prserr_len, (ftnlen)48);
		    chkout_("ZZEKNRML", (ftnlen)8);
		    return 0;
		}
	    } else if (mtcode[(i__1 = first - 1) < 500 && 0 <= i__1 ? i__1 : 
		    s_rnge("mtcode", i__1, "zzeknrml_", (ftnlen)1903)] == -13)
		     {

/*              If the query is valid, the sequence of meta-tokens */
/*              should be one of */

/*                 <expression> */
/*                 <expression>   ) */
/*                 <expression>   OR   <expression> */
/*                 <expression>   OR   NAME */
/*                 <expression>   OR   IDENT */
/*                 <expression>   OR   NOT */
/*                 <expression>   OR   ( */
/*                 <expression>   AND  <expression> */
/*                 <expression>   AND  NAME */
/*                 <expression>   AND  IDENT */
/*                 <expression>   AND  NOT */
/*                 <expression>   AND  ( */

		if (second <= 0) {

/*                 This is the last state we pass through */
/*                 before exiting the loop.  However, some syntax errors */
/*                 can get us here as well. */

		    if (level > 1 || nmeta > 1) {
			*error = TRUE_;
			s_copy(prserr, "More tokens expected.", prserr_len, (
				ftnlen)21);
			chkout_("ZZEKNRML", (ftnlen)8);
			return 0;
		    }
		    state = 4;
		} else if (mtcode[(i__1 = second - 1) < 500 && 0 <= i__1 ? 
			i__1 : s_rnge("mtcode", i__1, "zzeknrml_", (ftnlen)
			1939)] == -1) {

/*                 We've reached the end of a `parenthesized' */
/*                 expression. */

		    if (level > 1 && popcnd[(i__1 = level - 1) < 500 && 0 <= 
			    i__1 ? i__1 : s_rnge("popcnd", i__1, "zzeknrml_", 
			    (ftnlen)1944)] == 2) {

/*                    Time to pop the state. */

			state = 1;
		    } else {

/*                    There should not be a right grouper here. */

			*error = TRUE_;
			s_copy(prserr, "Unexpected right parenthesis found.", 
				prserr_len, (ftnlen)35);
			chkout_("ZZEKNRML", (ftnlen)8);
			return 0;
		    }

/*              In all other cases, there must be at least three */
/*              meta-tokens here.  Make sure there are. */

		} else if (third <= 0) {
		    *error = TRUE_;
		    s_copy(prserr, "More tokens expected.", prserr_len, (
			    ftnlen)21);
		    chkout_("ZZEKNRML", (ftnlen)8);
		    return 0;

/*              Take care of the cases that will require reducing a sub- */
/*              expression before reducing the current expression. */

		} else if (mtcode[(i__1 = third - 1) < 500 && 0 <= i__1 ? 
			i__1 : s_rnge("mtcode", i__1, "zzeknrml_", (ftnlen)
			1979)] == -7 || mtcode[(i__2 = third - 1) < 500 && 0 
			<= i__2 ? i__2 : s_rnge("mtcode", i__2, "zzeknrml_", (
			ftnlen)1979)] == -6 || mtcode[(i__3 = third - 1) < 
			500 && 0 <= i__3 ? i__3 : s_rnge("mtcode", i__3, 
			"zzeknrml_", (ftnlen)1979)] == -12) {
		    start = third;
		    retcnd = 1;
		    state = 0;
		} else if (mtcode[(i__1 = third - 1) < 500 && 0 <= i__1 ? 
			i__1 : s_rnge("mtcode", i__1, "zzeknrml_", (ftnlen)
			1988)] == -2) {

/*                 We'll have to push our state before continuing. */

		    start = fourth;
		    retcnd = 2;
		    state = 0;

/*              Now continue with the interesting cases. */

		} else if (mtcode[(i__1 = first - 1) < 500 && 0 <= i__1 ? 
			i__1 : s_rnge("mtcode", i__1, "zzeknrml_", (ftnlen)
			1999)] == -13 && mtcode[(i__2 = second - 1) < 500 && 
			0 <= i__2 ? i__2 : s_rnge("mtcode", i__2, "zzeknrml_",
			 (ftnlen)1999)] == -11 && mtcode[(i__3 = third - 1) < 
			500 && 0 <= i__3 ? i__3 : s_rnge("mtcode", i__3, 
			"zzeknrml_", (ftnlen)1999)] == -13) {

/*                 We have a disjunction of two normalized */
/*                 expressions.  We're not ready to perform a */
/*                 reduction yet; we need to see whether there's */
/*                 a higher priority operator, namely AND, on the */
/*                 right of the second expression. */

		    donow = TRUE_;
		    if (fourth > 0) {
			if (mtcode[(i__1 = fourth - 1) < 500 && 0 <= i__1 ? 
				i__1 : s_rnge("mtcode", i__1, "zzeknrml_", (
				ftnlen)2013)] == -10) {

/*                       The third token is already spoken for: */
/*                       the expression involving the operator */
/*                       to its right must be processed first. */

			    donow = FALSE_;
			}
		    }
		    if (donow) {

/*                    This is an easy case to handle: */
/*                    we can form the resulting normalized */
/*                    expression by just linking together the two */
/*                    lists in the disjunction table. */

			dj[0] = mtexpr[(i__1 = first - 1) < 500 && 0 <= i__1 ?
				 i__1 : s_rnge("mtexpr", i__1, "zzeknrml_", (
				ftnlen)2032)];
			dj[1] = mtexpr[(i__1 = third - 1) < 500 && 0 <= i__1 ?
				 i__1 : s_rnge("mtexpr", i__1, "zzeknrml_", (
				ftnlen)2033)];
			lnkilb_(dj, &dj[1], djpool);

/*                    The first meta-token will point to the resulting */
/*                    normalized expression; we'll discard the other */
/*                    two meta-tokens. */

			lnkfsl_(&second, &third, mtpool);
			nmeta += -2;

/*                    MTEXPR(FIRST) and MTCODE(FIRST) are already */
/*                    set correctly.  All we need to do is determine */
/*                    our next state.  The next state defaults to */
/*                    PARSE; the other possibility is POP. */

			state = 3;
		    } else {

/*                    We'll have to reduce the expression on the right */
/*                    of the third meta-token before coming back to */
/*                    this expression.  Get ready to push our state. */

/*                    The condition that must be met in order to pop our */
/*                    state will be that we've performed a reduction. */

			retcnd = 1;
			start = third;
			state = 0;
		    }

/*                 Either we've reduced an OR expression, in which case */
/*                 the state has been set to PARSE or POP, or we've */
/*                 found a sub-expression that must be reduced before */
/*                 we attack the current expression, in which case the */
/*                 state has been set to PUSH. */

		} else if (mtcode[(i__1 = first - 1) < 500 && 0 <= i__1 ? 
			i__1 : s_rnge("mtcode", i__1, "zzeknrml_", (ftnlen)
			2074)] == -13 && mtcode[(i__2 = second - 1) < 500 && 
			0 <= i__2 ? i__2 : s_rnge("mtcode", i__2, "zzeknrml_",
			 (ftnlen)2074)] == -10 && mtcode[(i__3 = third - 1) < 
			500 && 0 <= i__3 ? i__3 : s_rnge("mtcode", i__3, 
			"zzeknrml_", (ftnlen)2074)] == -13) {

/*                 We have the conjunction of two normalized */
/*                 expressions.  This case requires application of */
/*                 DeMorgan's laws to convert the expression to a */
/*                 normalized form. */

/*                 If we have two normalized expressions, say */

/*                    EXPR1 =        ( A11 and A12 and ... ) */
/*                                or ( A21 and A22 and ... ) */
/*                                              . */
/*                                              . */
/*                                              . */
/*                                or ( AM1 and AM2 and ... ) */


/*                    EXPR2 =        ( B11 and B12 and ... ) */
/*                                or ( B21 and B22 and ... ) */
/*                                              . */
/*                                              . */
/*                                              . */
/*                                or ( BN1 and BN2 and ... ) */



/*                 Then ( EXPR1 and EXPR2 ) = */


/*                        or       {  (     ( AI1 and AI2 and ... ) */
/*                    I = 1,...,M       and ( BJ1 and BJ2 and ... ) )  } */
/*                    J = 1,...,N */


/*                 We have the conjunction of two normalized */
/*                 So, to represent the normalized expression resulting */
/*                 from the conjuction of the expressions represented by */
/*                 the meta-tokens FIRST and THIRD, we will loop through */
/*                 each disjunction list and form the disjunction of all */
/*                 conjunctions of pairs of conjunctions, one of which is */
/*                 from the first expression and one of which is from the */
/*                 second.  After doing this, we'll clean up the */
/*                 conjunction and disjunction pools by freeing the */
/*                 elements in those pools used by the original two */
/*                 meta-tokens FIRST and THIRD. */


		    dj[0] = mtexpr[(i__1 = first - 1) < 500 && 0 <= i__1 ? 
			    i__1 : s_rnge("mtexpr", i__1, "zzeknrml_", (
			    ftnlen)2123)];
		    djtail = 0;
		    while(dj[0] > 0) {
			dj[1] = mtexpr[(i__1 = third - 1) < 500 && 0 <= i__1 ?
				 i__1 : s_rnge("mtexpr", i__1, "zzeknrml_", (
				ftnlen)2128)];
			while(dj[1] > 0) {

/*                       Allocate a new disjunction table entry, */
/*                       and create a new conjunction that represents */
/*                       the conjunction of the conjunction lists */
/*                       pointed to by DJ(1) and DJ(2). */

			    if (lnknfn_(djpool) < 1) {
				*error = TRUE_;
				s_copy(prserr, "Disjunction table is full.", 
					prserr_len, (ftnlen)26);
				chkout_("ZZEKNRML", (ftnlen)8);
				return 0;
			    }
			    lnkan_(djpool, &newdj);

/*                       Make copies of the conjunction lists pointed */
/*                       to by DJ(1) and DJ(2). */

			    cj[0] = djptrs[(i__1 = dj[0] - 1) < 5000 && 0 <= 
				    i__1 ? i__1 : s_rnge("djptrs", i__1, 
				    "zzeknrml_", (ftnlen)2150)];
			    tail = 0;
			    while(cj[0] > 0) {
				if (lnknfn_(cjpool) < 1) {
				    *error = TRUE_;
				    s_copy(prserr, "Conjunction table is ful"
					    "l.", prserr_len, (ftnlen)26);
				    chkout_("ZZEKNRML", (ftnlen)8);
				    return 0;
				}
				lnkan_(cjpool, &newcj);
				lnkila_(&tail, &newcj, cjpool);
				tail = newcj;
				cjptrs[(i__1 = tail - 1) < 5000 && 0 <= i__1 ?
					 i__1 : s_rnge("cjptrs", i__1, "zzek"
					"nrml_", (ftnlen)2165)] = cjptrs[(i__2 
					= cj[0] - 1) < 5000 && 0 <= i__2 ? 
					i__2 : s_rnge("cjptrs", i__2, "zzekn"
					"rml_", (ftnlen)2165)];
				cj[0] = lnknxt_(cj, cjpool);
			    }
			    head1 = lnkhl_(&tail, cjpool);
			    cj[1] = djptrs[(i__1 = dj[1] - 1) < 5000 && 0 <= 
				    i__1 ? i__1 : s_rnge("djptrs", i__1, 
				    "zzeknrml_", (ftnlen)2174)];
			    tail = 0;
			    while(cj[1] > 0) {
				if (lnknfn_(cjpool) < 1) {
				    *error = TRUE_;
				    s_copy(prserr, "Conjunction table is ful"
					    "l.", prserr_len, (ftnlen)26);
				    chkout_("ZZEKNRML", (ftnlen)8);
				    return 0;
				}
				lnkan_(cjpool, &newcj);
				lnkila_(&tail, &newcj, cjpool);
				tail = newcj;
				cjptrs[(i__1 = tail - 1) < 5000 && 0 <= i__1 ?
					 i__1 : s_rnge("cjptrs", i__1, "zzek"
					"nrml_", (ftnlen)2189)] = cjptrs[(i__2 
					= cj[1] - 1) < 5000 && 0 <= i__2 ? 
					i__2 : s_rnge("cjptrs", i__2, "zzekn"
					"rml_", (ftnlen)2189)];
				cj[1] = lnknxt_(&cj[1], cjpool);
			    }
			    head2 = lnkhl_(&tail, cjpool);

/*                       Now link these copies and make NEWDJ point to */
/*                       the resulting list. */

			    lnkilb_(&head1, &head2, cjpool);
			    djptrs[(i__1 = newdj - 1) < 5000 && 0 <= i__1 ? 
				    i__1 : s_rnge("djptrs", i__1, "zzeknrml_",
				     (ftnlen)2203)] = head1;

/*                       Link NEWDJ in at the tail of the disjunction */
/*                       list. */

			    lnkila_(&djtail, &newdj, djpool);
			    djtail = newdj;
			    dj[1] = lnknxt_(&dj[1], djpool);
			}
			dj[0] = lnknxt_(dj, djpool);
		    }

/*                 We've now created the new normalized expression that */
/*                 represents the conjunction of our original two */
/*                 expressions. */

/*                 Before continuing, we should clean up the entries in */
/*                 the disjunction and conjunction pools used by the */
/*                 original expressions.  We can save a little work */
/*                 by linking those entries before freeing them. */

		    lnkilb_(&mtexpr[(i__1 = first - 1) < 500 && 0 <= i__1 ? 
			    i__1 : s_rnge("mtexpr", i__1, "zzeknrml_", (
			    ftnlen)2230)], &mtexpr[(i__2 = third - 1) < 500 &&
			     0 <= i__2 ? i__2 : s_rnge("mtexpr", i__2, "zzek"
			    "nrml_", (ftnlen)2230)], djpool);
		    djnode = mtexpr[(i__1 = first - 1) < 500 && 0 <= i__1 ? 
			    i__1 : s_rnge("mtexpr", i__1, "zzeknrml_", (
			    ftnlen)2232)];
		    while(djnode > 0) {

/*                    Free the conjunction list pointed to by DJNODE. */

			cjnode = djptrs[(i__1 = djnode - 1) < 5000 && 0 <= 
				i__1 ? i__1 : s_rnge("djptrs", i__1, "zzeknr"
				"ml_", (ftnlen)2238)];
			i__1 = lnktl_(&cjnode, cjpool);
			lnkfsl_(&cjnode, &i__1, cjpool);
			djnode = lnknxt_(&djnode, djpool);
		    }

/*                 Free the disjunction list that starts with */
/*                 MTEXPR(FIRST). */

		    i__3 = lnktl_(&mtexpr[(i__2 = first - 1) < 500 && 0 <= 
			    i__2 ? i__2 : s_rnge("mtexpr", i__2, "zzeknrml_", 
			    (ftnlen)2251)], djpool);
		    lnkfsl_(&mtexpr[(i__1 = first - 1) < 500 && 0 <= i__1 ? 
			    i__1 : s_rnge("mtexpr", i__1, "zzeknrml_", (
			    ftnlen)2251)], &i__3, djpool);

/*                 NEWDJ is the tail node of the list of disjunctions */
/*                 we've just finished.  The first meta-token should */
/*                 point to the head of this disjunction list. */

		    mtexpr[(i__1 = first - 1) < 500 && 0 <= i__1 ? i__1 : 
			    s_rnge("mtexpr", i__1, "zzeknrml_", (ftnlen)2260)]
			     = lnkhl_(&newdj, djpool);

/*                 We no longer need the other two meta-tokens. */

		    lnkfsl_(&second, &third, mtpool);
		    nmeta += -2;

/*                 Decide the next state. */

		    state = 3;
		} else {

/*                 There are no other valid cases in which the first */
/*                 meta-token is an expression. */

		    *error = TRUE_;
		    s_copy(prserr, "Unexpected token found following valid e"
			    "xpression.", prserr_len, (ftnlen)50);
		    chkout_("ZZEKNRML", (ftnlen)8);
		    return 0;
		}
	    } else if (mtcode[(i__1 = first - 1) < 500 && 0 <= i__1 ? i__1 : 
		    s_rnge("mtcode", i__1, "zzeknrml_", (ftnlen)2288)] == -12)
		     {

/*              There are four valid token sequences that we could */
/*              see here: */

/*                 NOT  <expression> */
/*                 NOT  IDENT */
/*                 NOT  NAME */
/*                 NOT  NOT */
/*                 NOT  ( */

		if (second <= 0) {
		    *error = TRUE_;
		    s_copy(prserr, "Tokens were missing from logical express"
			    "ion.", prserr_len, (ftnlen)44);
		    chkout_("ZZEKNRML", (ftnlen)8);
		    return 0;
		} else if (mtcode[(i__1 = second - 1) < 500 && 0 <= i__1 ? 
			i__1 : s_rnge("mtcode", i__1, "zzeknrml_", (ftnlen)
			2307)] == -2) {

/*                 We'll have to push our state before continuing. */

		    start = third;
		    retcnd = 2;
		    state = 0;
		} else if (mtcode[(i__1 = second - 1) < 500 && 0 <= i__1 ? 
			i__1 : s_rnge("mtcode", i__1, "zzeknrml_", (ftnlen)
			2316)] == -12 || mtcode[(i__2 = second - 1) < 500 && 
			0 <= i__2 ? i__2 : s_rnge("mtcode", i__2, "zzeknrml_",
			 (ftnlen)2316)] == -7 || mtcode[(i__3 = second - 1) < 
			500 && 0 <= i__3 ? i__3 : s_rnge("mtcode", i__3, 
			"zzeknrml_", (ftnlen)2316)] == -6) {
		    start = second;
		    retcnd = 1;
		    state = 0;
		} else if (mtcode[(i__1 = second - 1) < 500 && 0 <= i__1 ? 
			i__1 : s_rnge("mtcode", i__1, "zzeknrml_", (ftnlen)
			2325)] == -13) {

/*                 We have the negation of a normalized expression. Since */
/*                 the NOT operator has higher precedence than any other, */
/*                 we need not concern ourselves with the token on the */
/*                 right of the expression. */

/*                 This case requires application of DeMorgan's laws to */
/*                 convert the expression to a normalized form. */


/*                 If we have a normalized expression, say */

/*                    EXPR  =         ( A11 and A12 and ... ) */
/*                                 or ( A21 and A22 and ... ) */
/*                                              . */
/*                                              . */
/*                                              . */
/*                                 or ( AM1 and AM2 and ... ) */

/*                 Then (using the tilde to express negation): */

/*                    ~EXPR =          ( ~A11 or ~A12 or ... ) */
/*                                 and ( ~A21 or ~A22 or ... ) */
/*                                              . */
/*                                              . */
/*                                              . */
/*                                 and ( ~AM1 or ~AM2 or ... ) */

/*                 Since each parenthesized expression above is a */
/*                 normalized expression, we can convert the conjunction */
/*                 of any of these expressions and a second normalized */
/*                 expression to normalized form using the method of the */
/*                 AND case above. */

/*                 We'll first build the expression */

/*                    ( ~A11 or ~A12 or ... ) */

/*                 and then combine the others with it, one by one. */
/*                 When we're all done, we'll negate the operators used */
/*                 in the comparison relations. */

/*                 The pointer EXPRHD will denote the head of the */
/*                 combined normalized expression. */

		    djnode = mtexpr[(i__1 = second - 1) < 500 && 0 <= i__1 ? 
			    i__1 : s_rnge("mtexpr", i__1, "zzeknrml_", (
			    ftnlen)2371)];
		    cjnode = djptrs[(i__1 = djnode - 1) < 5000 && 0 <= i__1 ? 
			    i__1 : s_rnge("djptrs", i__1, "zzeknrml_", (
			    ftnlen)2373)];
		    tail = 0;
		    while(cjnode > 0) {

/*                    Create a new singleton disjunction list */
/*                    that points to the relation pointed to by */
/*                    CJNODE. */

			if (lnknfn_(djpool) < 1) {
			    *error = TRUE_;
			    s_copy(prserr, "Disjunction table is full.", 
				    prserr_len, (ftnlen)26);
			    chkout_("ZZEKNRML", (ftnlen)8);
			    return 0;
			}
			lnkan_(djpool, &newdj);
			if (lnknfn_(cjpool) < 1) {
			    *error = TRUE_;
			    s_copy(prserr, "Conjunction table is full.", 
				    prserr_len, (ftnlen)26);
			    chkout_("ZZEKNRML", (ftnlen)8);
			    return 0;
			}
			lnkan_(cjpool, &newcj);
			cjptrs[(i__1 = newcj - 1) < 5000 && 0 <= i__1 ? i__1 :
				 s_rnge("cjptrs", i__1, "zzeknrml_", (ftnlen)
				2400)] = cjptrs[(i__2 = cjnode - 1) < 5000 && 
				0 <= i__2 ? i__2 : s_rnge("cjptrs", i__2, 
				"zzeknrml_", (ftnlen)2400)];
			djptrs[(i__1 = newdj - 1) < 5000 && 0 <= i__1 ? i__1 :
				 s_rnge("djptrs", i__1, "zzeknrml_", (ftnlen)
				2401)] = newcj;

/*                    Now link the new singleton disjunction list in */
/*                    at the tail of the disjunction list that */
/*                    parallels the conjunction list we're currently */
/*                    traversing. */

			lnkila_(&tail, &newdj, djpool);
			tail = newdj;
			cjnode = lnknxt_(&cjnode, cjpool);
		    }

/*                 Keep track of the head of the new normalized */
/*                 expression. */

		    exprhd = lnkhl_(&tail, djpool);

/*                 Now, for every remaining conjunction in the original */
/*                 expression, we'll form the normalized expression */
/*                 resulting from the conjunction of its negation and */
/*                 of our cumulative normalized expression.  As mentioned */
/*                 before, we won't negate the comparison operators */
/*                 just yet. */


		    djnode = lnknxt_(&djnode, djpool);
		    while(djnode > 0) {

/*                    Loop through our existing cumulative */
/*                    expression and the latest conjunction, forming */
/*                    all pairwise conjunctions. */

			dj[0] = exprhd;
			djtail = 0;
			while(dj[0] > 0) {
			    cj[1] = djptrs[(i__1 = djnode - 1) < 5000 && 0 <= 
				    i__1 ? i__1 : s_rnge("djptrs", i__1, 
				    "zzeknrml_", (ftnlen)2443)];
			    while(cj[1] > 0) {

/*                          Make a copy of the conjunction list pointed */
/*                          to by DJPTRS(DJ(1)). */

				cjnode = djptrs[(i__1 = dj[0] - 1) < 5000 && 
					0 <= i__1 ? i__1 : s_rnge("djptrs", 
					i__1, "zzeknrml_", (ftnlen)2450)];
				tail = 0;
				while(cjnode > 0) {
				    if (lnknfn_(cjpool) < 1) {
					*error = TRUE_;
					s_copy(prserr, "Conjunction table is"
						" full.", prserr_len, (ftnlen)
						26);
					chkout_("ZZEKNRML", (ftnlen)8);
					return 0;
				    }
				    lnkan_(cjpool, &newcj);
				    lnkila_(&tail, &newcj, cjpool);
				    cjptrs[(i__1 = newcj - 1) < 5000 && 0 <= 
					    i__1 ? i__1 : s_rnge("cjptrs", 
					    i__1, "zzeknrml_", (ftnlen)2465)] 
					    = cjptrs[(i__2 = cjnode - 1) < 
					    5000 && 0 <= i__2 ? i__2 : s_rnge(
					    "cjptrs", i__2, "zzeknrml_", (
					    ftnlen)2465)];
				    tail = newcj;
				    cjnode = lnknxt_(&cjnode, cjpool);
				}
				cj[0] = lnkhl_(&tail, cjpool);

/*                          Allocate a new conjunction table entry for */
/*                          the conjunction of the expressions */
/*                          pointed to by CJ(1) and CJ(2).  Allocate a */
/*                          new disjunction table entry to point to this */
/*                          new conjunction. */

				if (lnknfn_(cjpool) < 1) {
				    *error = TRUE_;
				    s_copy(prserr, "Conjunction table is ful"
					    "l.", prserr_len, (ftnlen)26);
				    chkout_("ZZEKNRML", (ftnlen)8);
				    return 0;
				}
				lnkan_(cjpool, &newcj);
				cjptrs[(i__1 = newcj - 1) < 5000 && 0 <= i__1 
					? i__1 : s_rnge("cjptrs", i__1, "zze"
					"knrml_", (ftnlen)2488)] = cjptrs[(
					i__2 = cj[1] - 1) < 5000 && 0 <= i__2 
					? i__2 : s_rnge("cjptrs", i__2, "zze"
					"knrml_", (ftnlen)2488)];
				if (lnknfn_(djpool) < 1) {
				    *error = TRUE_;
				    s_copy(prserr, "Disjunction table is ful"
					    "l.", prserr_len, (ftnlen)26);
				    chkout_("ZZEKNRML", (ftnlen)8);
				    return 0;
				}
				lnkan_(djpool, &newdj);

/*                          Hook everything up. */

				lnkilb_(cj, &newcj, cjpool);
				djptrs[(i__1 = newdj - 1) < 5000 && 0 <= i__1 
					? i__1 : s_rnge("djptrs", i__1, "zze"
					"knrml_", (ftnlen)2504)] = cj[0];
				lnkila_(&djtail, &newdj, djpool);
				djtail = newdj;
				cj[1] = lnknxt_(&cj[1], cjpool);
			    }
			    dj[0] = lnknxt_(dj, djpool);
			}

/*                    Before going on, clean up the conjunction and */
/*                    disjunction pool entries used by our last */
/*                    version of the cumulative expression. */

			dj[0] = exprhd;
			while(dj[0] > 0) {
			    cj[0] = djptrs[(i__1 = dj[0] - 1) < 5000 && 0 <= 
				    i__1 ? i__1 : s_rnge("djptrs", i__1, 
				    "zzeknrml_", (ftnlen)2526)];
			    cj[1] = lnktl_(cj, cjpool);
			    lnkfsl_(cj, &cj[1], cjpool);
			    dj[0] = lnknxt_(dj, djpool);
			}
			i__1 = lnktl_(&exprhd, djpool);
			lnkfsl_(&exprhd, &i__1, djpool);

/*                    Set EXPRHD to be the head of our updated, */
/*                    cumulative expression.  Start to work on the */
/*                    next conjunction. */

			exprhd = lnkhl_(&djtail, djpool);
			djnode = lnknxt_(&djnode, djpool);
		    }

/*                 EXPRHD now points to a new expression that will */
/*                 represent the negation of the expression pointed */
/*                 to by MTEXPR(SECOND), as soon as we negate the */
/*                 comparison operators referenced in the expression. */
/*                 Take care of this last step now.  To make sure that */
/*                 we negate each operator exactly once, we build a set */
/*                 of relations to be negated, then negate each relation */
/*                 in the set. */

		    ssizei_(&c__5000, relset);
		    djnode = mtexpr[(i__1 = second - 1) < 500 && 0 <= i__1 ? 
			    i__1 : s_rnge("mtexpr", i__1, "zzeknrml_", (
			    ftnlen)2561)];
		    while(djnode > 0) {
			cjnode = djptrs[(i__1 = djnode - 1) < 5000 && 0 <= 
				i__1 ? i__1 : s_rnge("djptrs", i__1, "zzeknr"
				"ml_", (ftnlen)2566)];
			while(cjnode > 0) {
			    relptr = cjptrs[(i__1 = cjnode - 1) < 5000 && 0 <=
				     i__1 ? i__1 : s_rnge("cjptrs", i__1, 
				    "zzeknrml_", (ftnlen)2570)];
			    insrti_(&relptr, relset);
			    cjnode = lnknxt_(&cjnode, cjpool);
			}
			djnode = lnknxt_(&djnode, djpool);
		    }
		    i__1 = cardi_(relset);
		    for (i__ = 1; i__ <= i__1; ++i__) {
			relptr = relset[(i__2 = i__ + 5) < 5006 && 0 <= i__2 ?
				 i__2 : s_rnge("relset", i__2, "zzeknrml_", (
				ftnlen)2584)];
			j = isrchi_(&rels[(i__2 = relptr * 3 - 2) < 15000 && 
				0 <= i__2 ? i__2 : s_rnge("rels", i__2, "zze"
				"knrml_", (ftnlen)2585)], &c__8, cmpcde);
			rels[(i__2 = relptr * 3 - 2) < 15000 && 0 <= i__2 ? 
				i__2 : s_rnge("rels", i__2, "zzeknrml_", (
				ftnlen)2588)] = cmpneg[(i__3 = j - 1) < 8 && 
				0 <= i__3 ? i__3 : s_rnge("cmpneg", i__3, 
				"zzeknrml_", (ftnlen)2588)];
		    }

/*                 Set the pointer of the first meta-token to point */
/*                 to our normalized expression, and change the */
/*                 meta-token's code to <expr>. */

		    mtexpr[(i__1 = first - 1) < 500 && 0 <= i__1 ? i__1 : 
			    s_rnge("mtexpr", i__1, "zzeknrml_", (ftnlen)2597)]
			     = exprhd;
		    mtcode[(i__1 = first - 1) < 500 && 0 <= i__1 ? i__1 : 
			    s_rnge("mtcode", i__1, "zzeknrml_", (ftnlen)2598)]
			     = -13;

/*                 Get rid of the second meta-token, and determine the */
/*                 next state. */

		    lnkfsl_(&second, &second, mtpool);
		    --nmeta;
		    state = 3;
		} else {

/*                 The second token is invalid in this context. */

		    *error = TRUE_;
		    s_copy(prserr, "Token following NOT operator was invalid."
			    , prserr_len, (ftnlen)41);
		    chkout_("ZZEKNRML", (ftnlen)8);
		    return 0;
		}

/*              This is the end of the NOT case. */

	    } else if (mtcode[(i__1 = first - 1) < 500 && 0 <= i__1 ? i__1 : 
		    s_rnge("mtcode", i__1, "zzeknrml_", (ftnlen)2625)] == -2) 
		    {

/*              We're looking at the start of a `parenthesized' */
/*              sub-expression. */

/*              Push our state, and start parsing at meta-token */
/*              SECOND.  The condition for popping our state will be */
/*              that we encounter a right grouper. */

		retcnd = 2;
		start = second;
		state = 0;
	    } else {

/*              Only a syntax error could get us here. */

		*error = TRUE_;
		s_copy(prserr, "Unexpected token found.", prserr_len, (ftnlen)
			23);
		chkout_("ZZEKNRML", (ftnlen)8);
		return 0;
	    }

/*           This is the end of the code for the PARSE state.  We've */
/*           determined the next parsing state at this point. */

	} else if (state == 3) {

/*           A reduction has been done.  Decide the next state. */

	    state = 3;
	    if (popcnd[(i__1 = level - 1) < 500 && 0 <= i__1 ? i__1 : s_rnge(
		    "popcnd", i__1, "zzeknrml_", (ftnlen)2661)] == 1) {
		state = 1;
	    } else {
		mstart[(i__1 = level - 1) < 500 && 0 <= i__1 ? i__1 : s_rnge(
			"mstart", i__1, "zzeknrml_", (ftnlen)2664)] = first;
		state = 2;
	    }
	} else if (state == 0) {

/*           Increment the stack level, and save the current */
/*           starting point and pop condition. */

	    ++level;
	    if (level > 500) {
		*error = TRUE_;
		s_copy(prserr, "Stack is full", prserr_len, (ftnlen)13);
		state = 4;
	    } else {
		mstart[(i__1 = level - 1) < 500 && 0 <= i__1 ? i__1 : s_rnge(
			"mstart", i__1, "zzeknrml_", (ftnlen)2682)] = start;
		popcnd[(i__1 = level - 1) < 500 && 0 <= i__1 ? i__1 : s_rnge(
			"popcnd", i__1, "zzeknrml_", (ftnlen)2683)] = retcnd;
		state = 2;
	    }
	} else if (state == 1) {

/*           If we can, pop the state. */

	    if (level > 1) {
		if (popcnd[(i__1 = level - 1) < 500 && 0 <= i__1 ? i__1 : 
			s_rnge("popcnd", i__1, "zzeknrml_", (ftnlen)2694)] == 
			2) {

/*                 If we're popping the state because we encountered a */
/*                 right grouper, we have a meta-token sequence that */
/*                 looks like this: */

/*                    (  EXPR  ) */

/*                        ^    ^ */
/*                     FIRST  SECOND */

/*                 We need to remove the grouping tokens, taking care to */
/*                 update the starting token at the next lower level, if */
/*                 the left grouper was the starting token. */

		    prev = lnkprv_(&first, mtpool);
		    if (mstart[(i__1 = level - 2) < 500 && 0 <= i__1 ? i__1 : 
			    s_rnge("mstart", i__1, "zzeknrml_", (ftnlen)2711)]
			     == prev) {
			mstart[(i__1 = level - 2) < 500 && 0 <= i__1 ? i__1 : 
				s_rnge("mstart", i__1, "zzeknrml_", (ftnlen)
				2712)] = first;
		    }
		    if (metahd == prev) {
			metahd = first;
		    }
		    lnkfsl_(&prev, &prev, mtpool);
		    lnkfsl_(&second, &second, mtpool);
		    nmeta += -2;
		}
		--level;
		state = 2;
	    } else {
		*error = TRUE_;
		s_copy(prserr, "Syntax error:  badly formed WHERE clause.", 
			prserr_len, (ftnlen)41);
		chkout_("ZZEKNRML", (ftnlen)8);
		return 0;
	    }
	}

/*        We've considered all states. */

    }

/*     At this point, there should be a single meta-token of type EXPR. */
/*     This meta-token should point to a normalized expression.  We'll */
/*     set the encoded query to represent this expression.  For each */
/*     constraint, we'll add a constraint descriptor to the encoded */
/*     query.  We'll also update the count of constraints, the count of */
/*     conjunctions, and we'll add a list of conjunction sizes. */

    djnode = mtexpr[(i__1 = first - 1) < 500 && 0 <= i__1 ? i__1 : s_rnge(
	    "mtexpr", i__1, "zzeknrml_", (ftnlen)2754)];
    nconj = 0;
    nrels = 0;
    while(djnode > 0) {
	++nconj;
	sizes[(i__1 = nconj - 1) < 1000 && 0 <= i__1 ? i__1 : s_rnge("sizes", 
		i__1, "zzeknrml_", (ftnlen)2761)] = 0;
	cjnode = djptrs[(i__1 = djnode - 1) < 5000 && 0 <= i__1 ? i__1 : 
		s_rnge("djptrs", i__1, "zzeknrml_", (ftnlen)2762)];
	while(cjnode > 0) {
	    ++nrels;
	    sizes[(i__1 = nconj - 1) < 1000 && 0 <= i__1 ? i__1 : s_rnge(
		    "sizes", i__1, "zzeknrml_", (ftnlen)2767)] = sizes[(i__2 =
		     nconj - 1) < 1000 && 0 <= i__2 ? i__2 : s_rnge("sizes", 
		    i__2, "zzeknrml_", (ftnlen)2767)] + 1;
	    relptr = cjptrs[(i__1 = cjnode - 1) < 5000 && 0 <= i__1 ? i__1 : 
		    s_rnge("cjptrs", i__1, "zzeknrml_", (ftnlen)2768)];
	    tabptr = rels[(i__1 = relptr * 3 - 3) < 15000 && 0 <= i__1 ? i__1 
		    : s_rnge("rels", i__1, "zzeknrml_", (ftnlen)2770)];
	    op = rels[(i__1 = relptr * 3 - 2) < 15000 && 0 <= i__1 ? i__1 : 
		    s_rnge("rels", i__1, "zzeknrml_", (ftnlen)2771)];
	    rhsptr = rels[(i__1 = relptr * 3 - 1) < 15000 && 0 <= i__1 ? i__1 
		    : s_rnge("rels", i__1, "zzeknrml_", (ftnlen)2772)];

/*           Add a constraint descriptor to the encoded query.  The */
/*           structure of these descriptors is documented in the include */
/*           file for encoded query parameters. */

/*           First, save space for the constraint type.  We'll fill this */
/*           in after finding out what's on the right hand side. */

	    appndi_(&c__0, eqryi);
	    k = cardi_(eqryi);

/*           Next, add name descriptors for the table and column on */
/*           the left-hand side.  These descriptors are linked and */
/*           pointed to by NAMPTR. */

	    for (i__ = 1; i__ <= 6; ++i__) {
		appndi_(&dscbuf[(i__1 = i__ + tabptr * 7 - 8) < 35000 && 0 <= 
			i__1 ? i__1 : s_rnge("dscbuf", i__1, "zzeknrml_", (
			ftnlen)2791)], eqryi);
	    }
	    colptr = lnknxt_(&tabptr, dspool);
	    for (i__ = 1; i__ <= 6; ++i__) {
		appndi_(&dscbuf[(i__1 = i__ + colptr * 7 - 8) < 35000 && 0 <= 
			i__1 ? i__1 : s_rnge("dscbuf", i__1, "zzeknrml_", (
			ftnlen)2797)], eqryi);
	    }

/*           What happens next depends on whether the query has a null */
/*           value on the right hand side.  This is indicated by the */
/*           relation's value pointer being NIL. */

	    if (rhsptr == 0) {

/*              For constraints involving null values, we change the */
/*              operator to ISNULL or NOTNUL as appropriate. */

		if (op == 1) {
		    op = 9;
		} else if (op == 6) {
		    op = 10;
		} else {
		    *error = TRUE_;
		    s_copy(prserr, "NULL values can only be used with the op"
			    "erators \"IS NULL\", \"NOT NULL\", or equivalent"
			    "s.", prserr_len, (ftnlen)86);
		    chkout_("ZZEKNRML", (ftnlen)8);
		    return 0;
		}

/*              Set the operator code. */

		appndi_(&op, eqryi);

/*              Pad the constraint descriptor up to the full length. */

		for (i__ = 1; i__ <= 12; ++i__) {
		    appndi_(&c__0, eqryi);
		}

/*              Set the descriptor's type by updating the reserved */
/*              location. */

		eqryi[k + 5] = 2;
	    } else {

/*              For `normal' constraints, that is, constraints that don't */
/*              involve null values, we set the operator code, then */
/*              fill in the information describing the RHS of the */
/*              constraint. */

		appndi_(&op, eqryi);
		if (dscbuf[(i__1 = rhsptr * 7 - 1) < 35000 && 0 <= i__1 ? 
			i__1 : s_rnge("dscbuf", i__1, "zzeknrml_", (ftnlen)
			2855)] == -8) {

/*                 The RHS contains a value.  Append the descriptor */
/*                 for the value, then pad the constraint descriptor. */

		    for (i__ = 1; i__ <= 6; ++i__) {
			appndi_(&dscbuf[(i__1 = i__ + rhsptr * 7 - 8) < 35000 
				&& 0 <= i__1 ? i__1 : s_rnge("dscbuf", i__1, 
				"zzeknrml_", (ftnlen)2861)], eqryi);
		    }
		    for (i__ = 1; i__ <= 6; ++i__) {
			appndi_(&c__0, eqryi);
		    }

/*                 Set the descriptor's type by updating the reserved */
/*                 location. */

		    eqryi[k + 5] = 2;
		} else {

/*                 The RHS contains a column name.  Append the */
/*                 descriptors for the table and column. */

		    for (i__ = 1; i__ <= 6; ++i__) {
			appndi_(&dscbuf[(i__1 = i__ + rhsptr * 7 - 8) < 35000 
				&& 0 <= i__1 ? i__1 : s_rnge("dscbuf", i__1, 
				"zzeknrml_", (ftnlen)2881)], eqryi);
		    }
		    colptr = lnknxt_(&rhsptr, dspool);
		    for (i__ = 1; i__ <= 6; ++i__) {
			appndi_(&dscbuf[(i__1 = i__ + colptr * 7 - 8) < 35000 
				&& 0 <= i__1 ? i__1 : s_rnge("dscbuf", i__1, 
				"zzeknrml_", (ftnlen)2887)], eqryi);
		    }

/*                 Set the descriptor's type by updating the reserved */
/*                 location. */

		    eqryi[k + 5] = 1;
		}
	    }

/*           We've updated the encoded query to reflect the current */
/*           constraint relation. */

	    cjnode = lnknxt_(&cjnode, cjpool);
	}

/*        We've set the array element SIZES(NCONJ). */

	djnode = lnknxt_(&djnode, djpool);
    }

/*     Set the counts of constraints and conjunctions in the encoded */
/*     query. */

    zzekweqi_("NUM_CONSTRAINTS", &nrels, eqryi, (ftnlen)15);
    zzekweqi_("NUM_CONJUNCTIONS", &nconj, eqryi, (ftnlen)16);

/*     Add the conjunction size list to the encoded query. */

    i__1 = nconj;
    for (i__ = 1; i__ <= i__1; ++i__) {
	appndi_(&sizes[(i__2 = i__ - 1) < 1000 && 0 <= i__2 ? i__2 : s_rnge(
		"sizes", i__2, "zzeknrml_", (ftnlen)2924)], eqryi);
    }
    chkout_("ZZEKNRML", (ftnlen)8);
    return 0;
} /* zzeknrml_ */

