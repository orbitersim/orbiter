/* zzekpcol.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__3 = 3;
static integer c__0 = 0;

/* $Procedure  ZZEKPCOL ( Private: EK, parse column name ) */
/* Subroutine */ int zzekpcol_(char *qcol, integer *eqryi, char *eqryc, char *
	table, char *alias, integer *tabidx, char *column, integer *colidx, 
	logical *error, char *errmsg, ftnlen qcol_len, ftnlen eqryc_len, 
	ftnlen table_len, ftnlen alias_len, ftnlen column_len, ftnlen 
	errmsg_len)
{
    /* System generated locals */
    integer i__1, i__2, i__3;

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_rnge(char *, integer, char *, integer), s_cmp(char *, char *, 
	    ftnlen, ftnlen);

    /* Local variables */
    integer ntab;
    logical qual;
    extern /* Subroutine */ int zzekscan_(char *, integer *, integer *, 
	    integer *, integer *, integer *, integer *, integer *, doublereal 
	    *, char *, integer *, integer *, logical *, char *, ftnlen, 
	    ftnlen, ftnlen), zzekqtab_(integer *, char *, integer *, char *, 
	    char *, ftnlen, ftnlen, ftnlen), zzekreqi_(integer *, char *, 
	    integer *, ftnlen);
    integer i__, j;
    extern /* Subroutine */ int ekcii_(char *, integer *, char *, integer *, 
	    ftnlen, ftnlen), chkin_(char *, ftnlen), ucase_(char *, char *, 
	    ftnlen, ftnlen), repmc_(char *, char *, char *, char *, ftnlen, 
	    ftnlen, ftnlen, ftnlen);
    integer cc;
    extern logical failed_(void);
    integer icheck, chbegs[3], chends[3];
    char chrbuf[160];
    extern integer isrchc_(char *, integer *, char *, ftnlen, ftnlen);
    char alslst[64*10], tablst[64*10], tmpcol[32], tmptab[64];
    doublereal numvls[3];
    integer attdsc[6], lxbegs[3], lxends[3], nmatch, ntoken, tokens[3], 
	    values[3];
    logical fnd;
    extern /* Subroutine */ int setmsg_(char *, ftnlen), sigerr_(char *, 
	    ftnlen), chkout_(char *, ftnlen), ekccnt_(char *, integer *, 
	    ftnlen);

/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines.  Users should not call this routine directly due */
/*     to the volatile nature of this routine. */

/*     Given an encoded query, parse the name of a column appearing in */
/*     that query, returning full particulars concerning the column and */
/*     its parent table. */

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

/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     QCOL       I   Column name, possibly qualified. */
/*     EQRYI      I   Integer component of query. */
/*     EQRYC      I   Character component of query. */
/*     TABLE      O   Name of table qualifying column. */
/*     ALIAS      O   Alias of table, if present. */
/*     TABIDX     O   Index of TABLE in FROM clause, if known. */
/*     COLUMN     O   Name of QCOL, unqualified. */
/*     COLIDX     O   Index of QCOL within its parent virtual table. */
/*     ERROR      O   Error flag. */
/*     ERRMSG     O   Parse error message. */

/* $ Detailed_Input */

/*     QCOL           is a column name from an EK query.  QCOL may be */
/*                    qualified by a table name, in which case it */
/*                    conforms to the sytax */

/*                       <identifier> . <identifier> */

/*                    or QCOL may be unqualified, in which case it */
/*                    is simply an <identifier> in the EK query language. */


/*     EQRYI          is the integer portion of an encoded EK query. */
/*                    The query must have been parsed. */

/*     EQRYC          is the character portion of an encoded EK query. */

/* $ Detailed_Output */

/*     TABLE          is the name of the table containing the column */
/*                    identified by QCOL.  If QCOL contains a table name */
/*                    to begin with, TABLE is that name, converted to */
/*                    upper case. */

/*     ALIAS          is the alias of the table containing the column */
/*                    identified by QCOL, if an alias for that table is */
/*                    present in the input query.  If QCOL contains a */
/*                    table alias to begin with, TABLE is that alias, */
/*                    converted to upper case. */

/*     TABIDX         is the ordinal position in the FROM clause of the */
/*                    input query of the table containing the column */
/*                    designated by QCOL. */

/*     COLUMN         is the name of the column designated by QCOL, */
/*                    converted to upper case. */

/*     COLIDX         is the ordinal position column designated by QCOL */
/*                    with respect to the virtual table containing that */
/*                    column. */

/*     ERROR          is a logical flag indicating whether QCOL was */
/*                    parsed correctly.  The previous list of outputs */
/*                    are undefined if a parse error occurred.  ERROR */
/*                    is returned .TRUE. if a parse error occurred, */
/*                    .FALSE. otherwise. */

/*     ERRMSG         is a character string describing the cause of a */
/*                    parse error, if such an error occurred.  Otherwise, */
/*                    ERRMSG is returned blank. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the input query is not initialized, the error will be */
/*         diagnosed by routines called by this routine.  The outputs */
/*         will not be modified. */

/*     2)  If the input query has not been semantically checked, the */
/*         error SPICE(NOTSEMCHECKED) will be signaled.  The outputs */
/*         will not be modified. */

/*     3)  If the input QCOL does not parse as a qualified or */
/*         unqualified column name, the error flag and message will */
/*         indicate that a parse error occurred.  No error will be */
/*         signaled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine supports parsing of the SELECT clause of EK */
/*     queries by higher-level routines.  This routine is */
/*     superseded by the SPICELIB routine EKPSEL. */

/* $ Examples */

/*     1)  Suppose that EQRYI and EQRYC have been obtained by */
/*         encoding the query */

/*            'SELECT T1.COL1, T2.COL2 FROM TABLE1 T1, TABLE2 T2' */

/*         Suppose also that the table TABLE1 contains two columns */
/*         named COL1 and COL2, and that the columns occur in that */
/*         order in the table. */

/*         Then the call */

/*            CALL ZZEKPCOL ( 'T1.COL', EQRYI,  EQRYC,  TABLE, ALIAS, */
/*           .                TABIDX,   COLUMN, COLIDX, ERROR, ERRMSG ) */

/*         will return */

/*            TABLE  =  'TABLE1' */
/*            ALIAS  =  'T1' */
/*            TABIDX =  1 */
/*            COLUMN =  'COL1' */
/*            COLIDX =  1 */
/*            ERROR  =  .FALSE. */
/*            ERRMSG =  ' ' */


/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.1, 02-OCT-2021 (NJB) */

/*        Corrected typos in comments. */

/* -    Beta Version 1.0.0, 11-OCT-1995 (NJB) */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     Local variables */


/*     Use discovery check-in. */

    *error = FALSE_;
    s_copy(errmsg, " ", errmsg_len, (ftnlen)1);
    zzekreqi_(eqryi, "SEM_CHECKED", &icheck, (ftnlen)11);
    if (failed_()) {
	return 0;
    }

/*     Make sure the encoded query is in order before proceeding. */

    if (icheck == -1) {
	chkin_("ZZEKPCOL", (ftnlen)8);
	setmsg_("Encoded query has not yet been semantically checked.", (
		ftnlen)52);
	sigerr_("SPICE(NOTSEMCHECKED)", (ftnlen)20);
	chkout_("ZZEKPCOL", (ftnlen)8);
	return 0;
    }

/*     Scan the input column name.  There are only two valid token */
/*     sequences possible: */

/*        <identifier> */

/*        <identifier> . <identifier> */

/*     ZZEKSCAN should therefore return 1 or 3 tokens. */

    zzekscan_(qcol, &c__3, &c__0, &ntoken, tokens, lxbegs, lxends, values, 
	    numvls, chrbuf, chbegs, chends, error, errmsg, qcol_len, (ftnlen)
	    160, errmsg_len);
    if (*error) {
	return 0;
    }
    if (ntoken == 1) {
	if (tokens[0] != 2) {
	    *error = TRUE_;
	    s_copy(errmsg, "Invalid column name; name should consist of an i"
		    "dentifier.", errmsg_len, (ftnlen)58);
	    return 0;
	}
	ucase_(qcol, column, qcol_len, column_len);
	qual = FALSE_;
    } else if (ntoken == 3) {
	if (tokens[0] != 2) {
	    *error = TRUE_;
	    s_copy(errmsg, "Invalid table name; name should consist of an id"
		    "entifier.", errmsg_len, (ftnlen)57);
	    return 0;
	} else if (tokens[1] != 9) {
	    *error = TRUE_;
	    s_copy(errmsg, "Invalid qualified column name; table name should"
		    " be followed by a period.", errmsg_len, (ftnlen)73);
	    return 0;
	} else if (tokens[2] != 2) {
	    *error = TRUE_;
	    s_copy(errmsg, "Invalid column name; name should consist of an i"
		    "dentifier.", errmsg_len, (ftnlen)58);
	    return 0;
	}
	i__ = values[0];
	j = values[2];
	i__1 = chbegs[(i__2 = i__ - 1) < 3 && 0 <= i__2 ? i__2 : s_rnge("chb"
		"egs", i__2, "zzekpcol_", (ftnlen)350)] - 1;
	s_copy(tmptab, chrbuf + i__1, (ftnlen)64, chends[(i__3 = i__ - 1) < 3 
		&& 0 <= i__3 ? i__3 : s_rnge("chends", i__3, "zzekpcol_", (
		ftnlen)350)] - i__1);
	i__1 = chbegs[(i__2 = j - 1) < 3 && 0 <= i__2 ? i__2 : s_rnge("chbegs"
		, i__2, "zzekpcol_", (ftnlen)351)] - 1;
	s_copy(column, chrbuf + i__1, column_len, chends[(i__3 = j - 1) < 3 &&
		 0 <= i__3 ? i__3 : s_rnge("chends", i__3, "zzekpcol_", (
		ftnlen)351)] - i__1);
	qual = TRUE_;
    } else {
	*error = TRUE_;
	s_copy(errmsg, "Invalid tokens present in qualified column name. Val"
		"id syntax is <column> or <table>.<column>", errmsg_len, (
		ftnlen)93);
	return 0;
    }

/*     At this point, COLUMN and QUAL are set.  If a qualifying table */
/*     or alias was supplied, that string is stored in TMPTAB.  Both */
/*     COLUMN and TMPTAB are in upper case. */

/*     If we got this far, we'll need to look up the table names and */
/*     aliases from the query. */

    zzekreqi_(eqryi, "NUM_TABLES", &ntab, (ftnlen)10);
    i__1 = ntab;
    for (i__ = 1; i__ <= i__1; ++i__) {
	zzekqtab_(eqryi, eqryc, &i__, tablst + (((i__2 = i__ - 1) < 10 && 0 <=
		 i__2 ? i__2 : s_rnge("tablst", i__2, "zzekpcol_", (ftnlen)
		375)) << 6), alslst + (((i__3 = i__ - 1) < 10 && 0 <= i__3 ? 
		i__3 : s_rnge("alslst", i__3, "zzekpcol_", (ftnlen)375)) << 6)
		, eqryc_len, (ftnlen)64, (ftnlen)64);
    }

/*     If QCOL contains a table name, look for that name in the */
/*     table list, and if necessary, in the alias list. */

    if (qual) {
	*tabidx = isrchc_(tmptab, &ntab, tablst, (ftnlen)64, (ftnlen)64);
	if (*tabidx == 0) {
	    *tabidx = isrchc_(tmptab, &ntab, alslst, (ftnlen)64, (ftnlen)64);
	}

/*        If we didn't find the table name in either list, it's just */
/*        plain wrong. */

	if (*tabidx == 0) {
	    *error = TRUE_;
	    s_copy(errmsg, "Table name <#> does not match table or alias fro"
		    "m query.", errmsg_len, (ftnlen)56);
	    repmc_(errmsg, "#", tmptab, errmsg, errmsg_len, (ftnlen)1, (
		    ftnlen)64, errmsg_len);
	    return 0;
	}

/*        At this point, TABIDX is valid.  Locate the column within */
/*        the table. */

	ekccnt_(tablst + (((i__1 = *tabidx - 1) < 10 && 0 <= i__1 ? i__1 : 
		s_rnge("tablst", i__1, "zzekpcol_", (ftnlen)406)) << 6), &cc, 
		(ftnlen)64);
	if (failed_()) {
	    return 0;
	}
	fnd = FALSE_;
	j = 1;
	while(j <= cc && ! fnd) {
	    ekcii_(tablst + (((i__1 = *tabidx - 1) < 10 && 0 <= i__1 ? i__1 : 
		    s_rnge("tablst", i__1, "zzekpcol_", (ftnlen)418)) << 6), &
		    j, tmpcol, attdsc, (ftnlen)64, (ftnlen)32);
	    if (s_cmp(tmpcol, column, (ftnlen)32, column_len) == 0) {
		*colidx = j;
		fnd = TRUE_;
	    } else {
		++j;
	    }
	}
	if (! fnd) {
	    *error = TRUE_;
	    s_copy(errmsg, "Column name <#> does not appear in the qualifyin"
		    "g table <#>.", errmsg_len, (ftnlen)60);
	    repmc_(errmsg, "#", column, errmsg, errmsg_len, (ftnlen)1, 
		    column_len, errmsg_len);
	    repmc_(errmsg, "#", tmptab, errmsg, errmsg_len, (ftnlen)1, (
		    ftnlen)64, errmsg_len);
	    return 0;
	}

/*        At this point, TABIDX and COLIDX are set correctly. */

    } else {

/*        No qualifying table name was supplied.  COLUMN had better */
/*        be a unique column name among the set of columns belong to */
/*        tables in the FROM clause of the input query.  Check the */
/*        columns for each table in the FROM clause, looking for */
/*        matches. */

	nmatch = 0;
	i__1 = ntab;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    ekccnt_(tablst + (((i__2 = i__ - 1) < 10 && 0 <= i__2 ? i__2 : 
		    s_rnge("tablst", i__2, "zzekpcol_", (ftnlen)456)) << 6), &
		    cc, (ftnlen)64);
	    if (failed_()) {
		return 0;
	    }
	    i__2 = cc;
	    for (j = 1; j <= i__2; ++j) {
		ekcii_(tablst + (((i__3 = i__ - 1) < 10 && 0 <= i__3 ? i__3 : 
			s_rnge("tablst", i__3, "zzekpcol_", (ftnlen)464)) << 
			6), &j, tmpcol, attdsc, (ftnlen)64, (ftnlen)32);
		if (s_cmp(tmpcol, column, (ftnlen)32, column_len) == 0) {
		    ++nmatch;
		    s_copy(column, tmpcol, column_len, (ftnlen)32);
		    *colidx = j;
		    *tabidx = i__;
		}
	    }
	}

/*        Check to see whether we have the unique identification we're */
/*        hoping for. */

	if (nmatch == 0) {
	    *error = TRUE_;
	    s_copy(errmsg, "Column name <#> does not appear in any table in "
		    "FROM clause of query.", errmsg_len, (ftnlen)69);
	    repmc_(errmsg, "#", column, errmsg, errmsg_len, (ftnlen)1, 
		    column_len, errmsg_len);
	    return 0;
	} else if (nmatch > 1) {
	    *error = TRUE_;
	    s_copy(errmsg, "Column name <#> is ambiguous without a qualifyin"
		    "g table name.", errmsg_len, (ftnlen)61);
	    repmc_(errmsg, "#", column, errmsg, errmsg_len, (ftnlen)1, 
		    column_len, errmsg_len);
	    return 0;
	}

/*        At this point, COLUMN, TABIDX and COLIDX are set correctly. */

    }

/*     At this point, COLUMN, TABIDX and COLIDX are set correctly, */
/*     regardless of whether the input name was qualified.  Fill the rest */
/*     of our output variables. */

    s_copy(table, tablst + (((i__1 = *tabidx - 1) < 10 && 0 <= i__1 ? i__1 : 
	    s_rnge("tablst", i__1, "zzekpcol_", (ftnlen)508)) << 6), 
	    table_len, (ftnlen)64);
    s_copy(alias, alslst + (((i__1 = *tabidx - 1) < 10 && 0 <= i__1 ? i__1 : 
	    s_rnge("alslst", i__1, "zzekpcol_", (ftnlen)509)) << 6), 
	    alias_len, (ftnlen)64);
    return 0;
} /* zzekpcol_ */

