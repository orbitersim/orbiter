/* zzeknres.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;

/* $Procedure  ZZEKNRES ( Private: EK, resolve names in encoded query ) */
/* Subroutine */ int zzeknres_(char *query, integer *eqryi, char *eqryc, 
	logical *error, char *errmsg, integer *errptr, ftnlen query_len, 
	ftnlen eqryc_len, ftnlen errmsg_len)
{
    /* System generated locals */
    integer i__1, i__2, i__3, i__4, i__5;

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_rnge(char *, integer, char *, integer), s_cmp(char *, char *, 
	    ftnlen, ftnlen);

    /* Local variables */
    integer base, ntab, ncnj, ncns, nord, nsel;
    extern /* Subroutine */ int zzekcchk_(char *, integer *, char *, integer *
	    , char *, char *, integer *, logical *, char *, integer *, ftnlen,
	     ftnlen, ftnlen, ftnlen, ftnlen), zzekqtab_(integer *, char *, 
	    integer *, char *, char *, ftnlen, ftnlen, ftnlen), zzekreqi_(
	    integer *, char *, integer *, ftnlen), zzekweqi_(char *, integer *
	    , integer *, ftnlen);
    integer i__, j;
    char table[64*10], alias[64*10];
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    integer nload;
    extern /* Subroutine */ int repmc_(char *, char *, char *, char *, ftnlen,
	     ftnlen, ftnlen, ftnlen);
    integer cc[10];
    extern logical failed_(void);
    char ltable[64];
    extern /* Subroutine */ int ekntab_(integer *);
    extern integer isrchc_(char *, integer *, char *, ftnlen, ftnlen);
    integer cnstyp, iparse;
    extern /* Subroutine */ int setmsg_(char *, ftnlen), sigerr_(char *, 
	    ftnlen), chkout_(char *, ftnlen), ektnam_(integer *, char *, 
	    ftnlen), ekccnt_(char *, integer *, ftnlen);
    logical fnd;
    integer lxb, lxe;

/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines.  Users should not call this routine directly due */
/*     to the volatile nature of this routine. */

/*     Resolve and semantically check table names, aliases, and column */
/*     names in an encoded EK query. */

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

/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     EQRYI     I-O  Integer component of query. */
/*     EQRYC     I-O  Character component of query. */
/*     ERROR      O   Error flag. */
/*     ERRMSG     O   Error message. */
/*     ERRPTR     O   Position in query where error was detected. */

/* $ Detailed_Input */

/*     QUERY          is the original query from which EQRYI and EQRYC */
/*                    were obtained.  QUERY is used only for */
/*                    construction of error messages. */

/*     EQRYI          is the integer portion of an encoded EK query. */
/*                    The query must have been parsed. */

/*     EQRYC          is the character portion of an encoded EK query. */

/* $ Detailed_Output */

/*     EQRYI          is the integer portion of an encoded EK query. */
/*                    On output, all names have been resolved, and */
/*                    table names, aliases, and column names have */
/*                    been semantically checked. */

/*     EQRYC          is the character portion of an encoded EK query. */

/*     ERROR          is a logical flag indicating whether an error was */
/*                    detected.  The error could be a name resolution */
/*                    error or a semantic error. */

/*     ERRMSG         is an error message describing an error in the */
/*                    input query, if one was detected.  If ERROR is */
/*                    returned .FALSE., then ERRPTR is undefined. */

/*     ERRPTR         is the character position in the original query */
/*                    at which an error was detected, if an error was */
/*                    found.  This index refers to the offending lexeme's */
/*                    position in the original query represented by the */
/*                    input encoded query.  If ERROR is returned .FALSE., */
/*                    ERRPTR is undefined. */
/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the input query is not initialized, the error will be */
/*         diagnosed by routines called by this routine.  The outputs */
/*         will not be modified. */

/*     2)  If the input query has not been parsed, the error */
/*         SPICE(QUERYNOTPARSED) will be signaled.  The outputs */
/*         will not be modified. */

/*     3)  If any sort of name resolution error or semantic error is */
/*         detected in the input query, the output flag ERROR is set, */
/*         and an error message is returned.  The checks performed by */
/*         this routine are listed below: */

/*           - All tables named in the FROM clause must be loaded */
/*             in the EK system. */

/*           - All aliases in the FROM clause must be distinct. */

/*           - No alias may be the name of a table in the FROM clause, */
/*             unless it is identical to the name of the table it is */
/*             associated with. */

/*           - No column name may be qualified with a name that is not */
/*             the name or alias of a table in the FROM clause. */

/*           - Each qualified column must be present in the table */
/*             indicated by its qualifying name. */

/*           - Each unqualified column name must be the name of a */
/*             column present in exactly one of the tables listed in the */
/*             FROM clause. */
/* $ Files */

/*     None. */

/* $ Particulars */

/*     Resolution of table names involves finding each table's ordinal */
/*     position in the FROM clause, and setting the table's descriptor */
/*     to record that position.  The same is done for column descriptors. */

/* $ Examples */

/*     See EKFIND. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.1, 03-OCT-2021 (NJB) */

/*        Corrected typos in comments. */

/* -    Beta Version 1.0.0, 17-OCT-1995 (NJB) */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     Local variables */


/*     No error to start with. */

    *error = FALSE_;
    s_copy(errmsg, " ", errmsg_len, (ftnlen)1);
    *errptr = 0;

/*     The query must have been parsed at this point, or it's no go. */

    zzekreqi_(eqryi, "PARSED", &iparse, (ftnlen)6);
    if (failed_()) {
	return 0;
    }
    if (iparse == -1) {
	chkin_("ZZEKNRES", (ftnlen)8);
	setmsg_("Encoded query has not been parsed.", (ftnlen)34);
	sigerr_("SPICE(QUERYNOTPARSED)", (ftnlen)21);
	chkout_("ZZEKNRES", (ftnlen)8);
	return 0;
    }

/*     Get the important counts from the query. */

    zzekreqi_(eqryi, "NUM_TABLES", &ntab, (ftnlen)10);
    zzekreqi_(eqryi, "NUM_CONSTRAINTS", &ncns, (ftnlen)15);
    zzekreqi_(eqryi, "NUM_CONJUNCTIONS", &ncnj, (ftnlen)16);
    zzekreqi_(eqryi, "NUM_ORDERBY_COLS", &nord, (ftnlen)16);
    zzekreqi_(eqryi, "NUM_SELECT_COLS", &nsel, (ftnlen)15);

/*     Start out by fetching the table names and their aliases. */

    i__1 = ntab;
    for (i__ = 1; i__ <= i__1; ++i__) {
	zzekqtab_(eqryi, eqryc, &i__, table + (((i__2 = i__ - 1) < 10 && 0 <= 
		i__2 ? i__2 : s_rnge("table", i__2, "zzeknres_", (ftnlen)258))
		 << 6), alias + (((i__3 = i__ - 1) < 10 && 0 <= i__3 ? i__3 : 
		s_rnge("alias", i__3, "zzeknres_", (ftnlen)258)) << 6), 
		eqryc_len, (ftnlen)64, (ftnlen)64);
    }

/*     Make sure that the aliases are distinct.  Rather than sorting */
/*     them, we'll check them in left-to-right order. */

    i__1 = ntab - 1;
    for (i__ = 1; i__ <= i__1; ++i__) {
	i__2 = ntab;
	for (j = i__ + 1; j <= i__2; ++j) {
	    if (s_cmp(alias + (((i__3 = i__ - 1) < 10 && 0 <= i__3 ? i__3 : 
		    s_rnge("alias", i__3, "zzeknres_", (ftnlen)269)) << 6), 
		    alias + (((i__4 = j - 1) < 10 && 0 <= i__4 ? i__4 : 
		    s_rnge("alias", i__4, "zzeknres_", (ftnlen)269)) << 6), (
		    ftnlen)64, (ftnlen)64) == 0 && s_cmp(alias + (((i__5 = 
		    i__ - 1) < 10 && 0 <= i__5 ? i__5 : s_rnge("alias", i__5, 
		    "zzeknres_", (ftnlen)269)) << 6), " ", (ftnlen)64, (
		    ftnlen)1) != 0) {
		*error = TRUE_;
		s_copy(errmsg, "Non-distinct alias <#> was found.", 
			errmsg_len, (ftnlen)33);
		base = ((j - 1 << 1) + 1) * 6 + 19;
		lxb = eqryi[base + 7];
		lxe = eqryi[base + 8];
		repmc_(errmsg, "#", query + (lxb - 1), errmsg, errmsg_len, (
			ftnlen)1, lxe - (lxb - 1), errmsg_len);
		*errptr = lxb;
		return 0;
	    }

/*           We've checked the Jth alias for a match. */

	}
    }

/*     Make sure that no alias matches a table name other than that of */
/*     the table it corresponds to. */

    i__1 = ntab;
    for (i__ = 1; i__ <= i__1; ++i__) {
	j = isrchc_(alias + (((i__2 = i__ - 1) < 10 && 0 <= i__2 ? i__2 : 
		s_rnge("alias", i__2, "zzeknres_", (ftnlen)299)) << 6), &ntab,
		 table, (ftnlen)64, (ftnlen)64);
	if (j != 0) {
	    if (j != i__) {
		*error = TRUE_;
		s_copy(errmsg, "Alias <#> conflicts with table name.", 
			errmsg_len, (ftnlen)36);
		base = ((i__ - 1 << 1) + 1) * 6 + 19;
		lxb = eqryi[base + 7];
		lxe = eqryi[base + 8];
		repmc_(errmsg, "#", query + (lxb - 1), errmsg, errmsg_len, (
			ftnlen)1, lxe - (lxb - 1), errmsg_len);
		*errptr = lxb;
		return 0;
	    }
	}
    }

/*     Make sure that all of the tables are loaded in the EK system. */

    ekntab_(&nload);
    i__1 = ntab;
    for (i__ = 1; i__ <= i__1; ++i__) {
	fnd = FALSE_;
	j = 1;
	while(j <= nload && ! fnd) {
	    ektnam_(&j, ltable, (ftnlen)64);
	    if (s_cmp(table + (((i__2 = i__ - 1) < 10 && 0 <= i__2 ? i__2 : 
		    s_rnge("table", i__2, "zzeknres_", (ftnlen)340)) << 6), 
		    ltable, (ftnlen)64, (ftnlen)64) == 0) {

/*              When we find a loaded table, save the column count for */
/*              that table. */

		fnd = TRUE_;
		ekccnt_(table, &cc[(i__2 = i__ - 1) < 10 && 0 <= i__2 ? i__2 :
			 s_rnge("cc", i__2, "zzeknres_", (ftnlen)346)], (
			ftnlen)64);
	    } else {
		++j;
	    }
	}
	if (! fnd) {
	    *error = TRUE_;
	    s_copy(errmsg, "Table <#> is not currently loaded.", errmsg_len, (
		    ftnlen)34);

/*           In order to set the error pointer, we'll need the */
/*           lexeme begin value for the offending table. */

	    base = (i__ - 1) * 12 + 19;
	    lxb = eqryi[base + 7];
	    lxe = eqryi[base + 8];
	    repmc_(errmsg, "#", query + (lxb - 1), errmsg, errmsg_len, (
		    ftnlen)1, lxe - (lxb - 1), errmsg_len);
	    *errptr = lxb;
	    return 0;
	}
    }

/*     At this point, the tables and aliases are deemed correct.  For */
/*     safety, fill in each table and alias descriptor with its */
/*     ordinal position. */

    i__1 = ntab;
    for (i__ = 1; i__ <= i__1; ++i__) {
	base = (i__ - 1) * 12 + 19;
	eqryi[base + 11] = i__;
	eqryi[base + 17] = i__;
    }

/*     Check the column names used in the constraints. */

    i__1 = ncns;
    for (i__ = 1; i__ <= i__1; ++i__) {

/*        Calculate the base address of the constraint. */

	base = ntab * 12 + 19 + (i__ - 1) * 26;

/*        Obtain the constraint type. */

	cnstyp = eqryi[base + 6];

/*        Check the column and table on the LHS of the constraint. */

	i__2 = base + 1;
	zzekcchk_(query, eqryi, eqryc, &ntab, table, alias, &i__2, error, 
		errmsg, errptr, query_len, eqryc_len, (ftnlen)64, (ftnlen)64, 
		errmsg_len);
	if (*error) {
	    return 0;
	}
	if (cnstyp == 1) {

/*           Check the column and table on the RHS of the constraint. */

	    i__2 = base + 14;
	    zzekcchk_(query, eqryi, eqryc, &ntab, table, alias, &i__2, error, 
		    errmsg, errptr, query_len, eqryc_len, (ftnlen)64, (ftnlen)
		    64, errmsg_len);
	    if (*error) {
		return 0;
	    }
	}
    }

/*     Do the same checks and assignments for the SELECT columns. */

    i__1 = nsel;
    for (i__ = 1; i__ <= i__1; ++i__) {

/*        Calculate the base address of the SELECT column descriptor. */

	base = ntab * 12 + 19 + ncnj + ncns * 26 + nord * 13 + (i__ - 1) * 12;
	zzekcchk_(query, eqryi, eqryc, &ntab, table, alias, &base, error, 
		errmsg, errptr, query_len, eqryc_len, (ftnlen)64, (ftnlen)64, 
		errmsg_len);
	if (*error) {
	    return 0;
	}
    }

/*     Do the same checks and assignments for the order-by columns. */

    i__1 = nord;
    for (i__ = 1; i__ <= i__1; ++i__) {

/*        Calculate the base address of the order-by column descriptor. */

	base = ntab * 12 + 19 + ncnj + ncns * 26 + (i__ - 1) * 13;
	zzekcchk_(query, eqryi, eqryc, &ntab, table, alias, &base, error, 
		errmsg, errptr, query_len, eqryc_len, (ftnlen)64, (ftnlen)64, 
		errmsg_len);
	if (*error) {
	    return 0;
	}
    }

/*     Indicate completion of name resolution. */

    zzekweqi_("NAMES_RESOLVED", &c__1, eqryi, (ftnlen)14);
    return 0;
} /* zzeknres_ */

