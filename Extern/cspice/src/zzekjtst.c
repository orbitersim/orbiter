/* zzekjtst.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__24 = 24;
static integer c__11 = 11;
static integer c__1 = 1;
static integer c__0 = 0;
static integer c__7 = 7;
static integer c__2 = 2;
static integer c__5 = 5;

/* $Procedure  ZZEKJTST  ( Test join candidates ) */
/* Subroutine */ int zzekjtst_0_(int n__, integer *segvec, integer *jbase1, 
	integer *nt1, integer *rb1, integer *nr1, integer *jbase2, integer *
	nt2, integer *rb2, integer *nr2, integer *njcnst, logical *active, 
	integer *cpidx1, integer *clidx1, integer *elts1, integer *ops, 
	integer *cpidx2, integer *clidx2, integer *elts2, integer *sthan, 
	integer *stsdsc, integer *stdtpt, integer *dtpool, integer *dtdscs, 
	logical *found, integer *rowvec)
{
    /* System generated locals */
    integer i__1, i__2, i__3, i__4, i__5, i__6, i__7;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer);

    /* Local variables */
    static integer base, case__, ltab;
    static logical done;
    static integer rtab, lcol, lseg, rcol, lelt, rseg, lcur, relt, lptr, lrow,
	     rptr, rrow;
    extern logical zzekvmch_(integer *, logical *, integer *, integer *, 
	    integer *, integer *, integer *, integer *, integer *, integer *, 
	    integer *, integer *, integer *), zzekrcmp_(integer *, integer *, 
	    integer *, integer *, integer *, integer *, integer *, integer *, 
	    integer *, integer *, integer *, integer *);
    static integer svcp1[100], svcp2[100], svrb1, svrb2;
    extern /* Subroutine */ int zzekspsh_(integer *, integer *), zzeksupd_(
	    integer *, integer *, integer *), zzekjsrt_(integer *, integer *, 
	    integer *, integer *, integer *, integer *, integer *, integer *, 
	    integer *, integer *, integer *, integer *, integer *), zzekstop_(
	    integer *);
    static integer i__, j, k, svnr1, svnr2, svnt1, svnt2, jbase, lbase, rbase;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    static integer lhans[100], lsdsc[2400]	/* was [24][100] */, rhans[
	    100], rsdsc[2400]	/* was [24][100] */;
    extern /* Subroutine */ int movei_(integer *, integer *, integer *);
    static integer lelts[100], cnstr, relts[100], dtptr, lrows[100], svops[
	    100], rrows[100], svbas1, svbas2, rb, nr, nt;
    static logical locact[100];
    extern integer lnknxt_(integer *, integer *);
    extern logical return_(void);
    static integer addrss, ldscrs[1100]	/* was [11][100] */, lovbas, lrvidx, 
	    minirv[2], offset, nt3, rdscrs[1100]	/* was [11][100] */, 
	    rovbas, rrvidx, svncon, tab, top;
    static logical fnd, lsmall;
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), zzeksrd_(integer *, integer *, integer *);

/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines.  Users should not call this routine directly due */
/*     to the volatile nature of this routine. */

/*     Test a set of candidate row vectors, all corresponding to the same */
/*     segment vector, against join constraints. */

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


/*     Include Section:  EK Column Descriptor Parameters */

/*        ekcoldsc.inc Version 6    23-AUG-1995 (NJB) */


/*     Note:  The column descriptor size parameter CDSCSZ  is */
/*     declared separately in the include section CDSIZE$INC.FOR. */

/*     Offset of column descriptors, relative to start of segment */
/*     integer address range.  This number, when added to the last */
/*     integer address preceding the segment, yields the DAS integer */
/*     base address of the first column descriptor.  Currently, this */
/*     offset is exactly the size of a segment descriptor.  The */
/*     parameter SDSCSZ, which defines the size of a segment descriptor, */
/*     is declared in the include file eksegdsc.inc. */


/*     Size of column descriptor */


/*     Indices of various pieces of column descriptors: */


/*     CLSIDX is the index of the column's class code.  (We use the */
/*     word `class' to distinguish this item from the column's data */
/*     type.) */


/*     TYPIDX is the index of the column's data type code (CHR, INT, DP, */
/*     or TIME).  The type is actually implied by the class, but it */
/*     will frequently be convenient to look up the type directly. */



/*     LENIDX is the index of the column's string length value, if the */
/*     column has character type.  A value of IFALSE in this element of */
/*     the descriptor indicates that the strings have variable length. */


/*     SIZIDX is the index of the column's element size value.  This */
/*     descriptor element is meaningful for columns with fixed-size */
/*     entries.  For variable-sized columns, this value is IFALSE. */


/*     NAMIDX is the index of the base address of the column's name. */


/*     IXTIDX is the data type of the column's index.  IXTIDX */
/*     contains a type value only if the column is indexed. For columns */
/*     that are not indexed, the location IXTIDX contains the boolean */
/*     value IFALSE. */


/*     IXPIDX is a pointer to the column's index.  IXTPDX contains a */
/*     meaningful value only if the column is indexed.  The */
/*     interpretation of the pointer depends on the data type of the */
/*     index. */


/*     NFLIDX is the index of a flag indicating whether nulls are */
/*     permitted in the column.  The value at location NFLIDX is */
/*     ITRUE if nulls are permitted and IFALSE otherwise. */


/*     ORDIDX is the index of the column's ordinal position in the */
/*     list of columns belonging to the column's parent segment. */


/*     METIDX is the index of the column's integer metadata pointer. */
/*     This pointer is a DAS integer address. */


/*     The last position in the column descriptor is reserved.  No */
/*     parameter is defined to point to this location. */


/*     End Include Section:  EK Column Descriptor Parameters */

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


/*     Include Section:  EK Join Row Set Parameters */

/*        ekjrs.inc  Version 1    07-FEB-1995 (NJB) */


/*     Maximum number of join row sets in a join row set union: */


/*     The layout of a join row set in the EK scratch area is shown */
/*     below: */

/*        +--------------------------------------------+ */
/*        |              join row set size             |  1 element */
/*        +--------------------------------------------+ */
/*        |    number of row vectors in join row set   |  1 element */
/*        +--------------------------------------------+ */
/*        |               table count (TC)             |  1 element */
/*        +--------------------------------------------+ */
/*        |          segment vector count (SVC)        |  1 element */
/*        +--------------------------------------------+ */
/*        |               segment vector 1             |  TC elements */
/*        +--------------------------------------------+ */
/*                              . */
/*                              . */
/*                              . */
/*        +--------------------------------------------+ */
/*        |               segment vector SVC           |  TC elements */
/*        +--------------------------------------------+ */
/*        |   segment vector 1 row set base address    |  1 element */
/*        +--------------------------------------------+ */
/*        |      segment vector 1 row count (RC_1)     |  1 element */
/*        +--------------------------------------------+ */
/*                              . */
/*                              . */
/*                              . */
/*        +--------------------------------------------+ */
/*        |  segment vector SVC row set base address   |  1 element */
/*        +--------------------------------------------+ */
/*        |   segment vector SVC row count (RC_SVC)    |  1 element */
/*        +--------------------------------------------+ */
/*        | Augmented row vectors for segment vector 1 |  (TC+1)*RC_1 */
/*        +--------------------------------------------+  elements */
/*                              . */
/*                              . */
/*                              . */
/*        +--------------------------------------------+ */
/*        |Augmented row vectors for segment vector SVC|  (TC+1)*RC_SVC1 */
/*        +--------------------------------------------+  elements */


/*     The following parameters indicate positions of elements in the */
/*     join row set structure: */


/*     Base-relative index of join row set size */


/*     Index of row vector count */


/*     Index of table count */


/*     Index of segment vector count */


/*     Base address of first segment vector */



/*     End Include Section:  EK Join Row Set Parameters */

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


/*     Include Section:  EK Segment Descriptor Parameters */

/*        eksegdsc.inc  Version 8  06-NOV-1995 (NJB) */


/*     All `base addresses' referred to below are the addresses */
/*     *preceding* the item the base applies to.  This convention */
/*     enables simplied address calculations in many cases. */

/*     Size of segment descriptor.  Note:  the include file ekcoldsc.inc */
/*     must be updated if this parameter is changed.  The parameter */
/*     CDOFF in that file should be kept equal to SDSCSZ. */


/*     Index of the segment type code: */


/*     Index of the segment's number.  This number is the segment's */
/*     index in the list of segments contained in the EK to which */
/*     the segment belongs. */


/*     Index of the DAS integer base address of the segment's integer */
/*     meta-data: */


/*     Index of the DAS character base address of the table name: */


/*     Index of the segment's column count: */


/*     Index of the segment's record count: */


/*     Index of the root page number of the record tree: */


/*     Index of the root page number of the character data page tree: */


/*     Index of the root page number of the double precision data page */
/*     tree: */


/*     Index of the root page number of the integer data page tree: */


/*     Index of the `modified' flag: */


/*     Index of the `initialized' flag: */


/*     Index of the shadowing flag: */


/*     Index of the companion file handle: */


/*     Index of the companion segment number: */


/*     The next three items are, respectively, the page numbers of the */
/*     last character, d.p., and integer data pages allocated by the */
/*     segment: */


/*     The next three items are, respectively, the page-relative */
/*     indices of the last DAS word in use in the segment's */
/*     last character, d.p., and integer data pages: */


/*     Index of the DAS character base address of the column name list: */


/*     The last descriptor element is reserved for future use.  No */
/*     parameter is defined to point to this location. */


/*     End Include Section:  EK Segment Descriptor Parameters */

/* $ Brief_I/O */

/*     Variable  I/O  Entry points */
/*     --------  ---  -------------------------------------------------- */
/*     SEGVEC     I   ZZEKJPRP */
/*     JBASE1     I   ZZEKJPRP */
/*     NT1        I   ZZEKJPRP */
/*     RB1        I   ZZEKJPRP */
/*     NR1        I   ZZEKJPRP */
/*     JBASE2     I   ZZEKJPRP */
/*     NT2        I   ZZEKJPRP */
/*     RB2        I   ZZEKJPRP */
/*     NR2        I   ZZEKJPRP */
/*     NJCNST     I   ZZEKJPRP */
/*     ACTIVE     I   ZZEKJPRP */
/*     CPIDX1     I   ZZEKJPRP */
/*     CLIDX1     I   ZZEKJPRP */
/*     ELTS1      I   ZZEKJPRP */
/*     OPS        I   ZZEKJPRP */
/*     CPIDX2     I   ZZEKJPRP */
/*     CLIDX2     I   ZZEKJPRP */
/*     ELTS2      I   ZZEKJPRP */
/*     STHAN      I   ZZEKJPRP */
/*     STSDSC     I   ZZEKJPRP */
/*     STDTPT     I   ZZEKJPRP */
/*     DTPOOL     I   ZZEKJPRP */
/*     DTDSCS     I   ZZEKJPRP */
/*     FOUND      O   ZZEKJNXT */
/*     ROWVEC     O   ZZEKJNXT */

/* $ Detailed_Input */

/*     See the entry points for a discussion of their inputs. */

/* $ Detailed_Output */

/*     See the entry points for a discussion of their inputs. */

/* $ Parameters */

/*     See the include files. */

/* $ Exceptions */

/*     1)  If this routine is called directly, the error */
/*         SPICE(BOGUSENTRY) is signaled. */

/*     See the entry points for discussions of exceptions pertaining to */
/*     those routines. */

/* $ Files */

/*     1)  This routine uses the EK scratch area, which employs a scratch */
/*         DAS file. */

/* $ Particulars */

/*     This suite of routines enables the EK system to execute table */
/*     joins with reasonable efficiency.  These routines make use of */
/*     join constraints to limit the number of joined row vectors that */
/*     must be considered in computing a join. */

/*     These routines deal with a limited case of the join problem: */
/*     the inputs define, for both join row sets participating in the */
/*     join, row vectors that are qualified by a single segment vector. */
/*     Thus this routine is meant to be called once for every pair of */
/*     segment vectors to be considered in executing the join. */

/*     The layout of a join row set in the EK scratch area is shown */
/*     in the include file for the join row set parameters. */

/* $ Examples */

/*     To use these routines, the normal sequence of actions is to */
/*     call ZZEKJPRP once to initialize them, and then to call */
/*     ZZEKJNXT in a loop to retrieve the row vectors satisfying */
/*     the join constraints.  See ZZEKJOIN for an example application. */

/* $ Restrictions */

/*     1)  This routine should not be called by routines outside of the */
/*         EK system. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman   (JPL) */

/* $ Version */

/* -    SPICELIB Version 2.0.1, 03-OCT-2021 (NJB) */

/*        Corrected typos in comments. */

/* -    SPICELIB Version 2.0.0, 20-JUL-1998 (NJB) */

/*        Modified entry point ZZEKJPRP to set CASE to EMPTY when either */
/*        input row count is zero.  Modified entry point ZZEKJNXT to */
/*        set FOUND to .FALSE. on the first pass when CASE is EMPTY. */

/* -    Beta Version 1.0.0, 11-OCT-1995 (NJB) */

/* -& */

/*     SPICELIB functions */


/*     Other functions */


/*     Local parameters */


/*     Local variables */


/*     Saved variables */

    /* Parameter adjustments */
    if (segvec) {
	}
    if (active) {
	}
    if (cpidx1) {
	}
    if (clidx1) {
	}
    if (elts1) {
	}
    if (ops) {
	}
    if (cpidx2) {
	}
    if (clidx2) {
	}
    if (elts2) {
	}
    if (sthan) {
	}
    if (stsdsc) {
	}
    if (stdtpt) {
	}
    if (dtpool) {
	}
    if (dtdscs) {
	}
    if (rowvec) {
	}

    /* Function Body */
    switch(n__) {
	case 1: goto L_zzekjprp;
	case 2: goto L_zzekjnxt;
	}

    chkin_("ZZEKJTST", (ftnlen)8);
    sigerr_("SPICE(BOGUSENTRY)", (ftnlen)17);
    chkout_("ZZEKJTST", (ftnlen)8);
    return 0;
/* $Procedure  ZZEKJPRP  ( Prepare join condition test ) */

L_zzekjprp:
/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines.  Users should not call this routine directly due */
/*     to the volatile nature of this routine. */

/*     Prepare to test a set of candidate row vectors, all corresponding */
/*     to the same segment vector, against join constraints. */

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
/*     UTILITY */

/* $ Declarations */

/*     INTEGER               LBPOOL */
/*     PARAMETER           ( LBPOOL = -5 ) */

/*     INTEGER               SEGVEC ( * ) */
/*     INTEGER               JBASE1 */
/*     INTEGER               NT1 */
/*     INTEGER               RB1 */
/*     INTEGER               NR1 */
/*     INTEGER               JBASE2 */
/*     INTEGER               NT2 */
/*     INTEGER               RB2 */
/*     INTEGER               NR2 */
/*     INTEGER               NJCNST */
/*     LOGICAL               ACTIVE ( * ) */
/*     INTEGER               CPIDX1 ( * ) */
/*     INTEGER               CLIDX1 ( * ) */
/*     INTEGER               OPS    ( * ) */
/*     INTEGER               CPIDX2 ( * ) */
/*     INTEGER               CLIDX2 ( * ) */
/*     INTEGER               STHAN  ( * ) */
/*     INTEGER               STSDSC ( 3, * ) */
/*     INTEGER               STDTPT ( * ) */
/*     INTEGER               DTPOOL ( 2,      LBPOOL : * ) */
/*     INTEGER               DTDSCS ( CDSCSZ,          * ) */

/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     SEGVEC     I   Composite segment vector for joined table. */
/*     JBASE1     I   Scratch area base address for first join row set. */
/*     NT1        I   Width of first table. */
/*     RB1        I   Row vector base address from first join row set. */
/*     NR1        I   Number of row vectors from first join row set. */
/*     JBASE2     I   Scratch area base address for second join row set. */
/*     NT2        I   Width of second table. */
/*     RB2        I   Row vector base address from second join row set. */
/*     NR2        I   Number of row vectors from second join row set. */
/*     JBASE1     I   Scratch area base address of first join row set. */
/*     JBASE2     I   Scratch area base address of second join row set. */
/*     NJCNST     I   Number of join constraints. */
/*     ACTIVE     I   Array of flags indicating applicable constraints. */
/*     CPIDX1     I   Cross product indices for LHS's of constraints. */
/*     CLIDX1     I   Column indices for LHS's of constraints. */
/*     OPS        I   Operator codes for constraints. */
/*     CPIDX2     I   Cross product indices for RHS's of constraints. */
/*     CLIDX2     I   Column indices for RHS's of constraints. */
/*     STHAN      I   Array of EK handles corresponding to segments. */
/*     STSDSC     I   Array of segment descriptors. */
/*     STDTPT     I   Array of set table column descriptor pointers. */
/*     DTPOOL     I   Linked list pool for column descriptors. */
/*     DTDSCS     I   Array of column descriptors. */

/* $ Detailed_Input */

/*     SEGVEC         is a composite segment vector for the output row */
/*                    vectors resulting from the join done by these */
/*                    routines.  SEGVEC has been created by suffixing */
/*                    a segment vector from the second input join row */
/*                    set onto a segment vector from the first join row */
/*                    set. */

/*     JBASE1         is the EK scratch area base address of the first */
/*                    input join row set.  This address is one less than */
/*                    the first address occupied by the join row set. */
/*                    See the $Particulars section for a description of */
/*                    join row sets. */

/*     NT1            is the number of tables in the first join row set. */

/*     RB1            is the scratch area base address of the considered */
/*                    row vectors from the first join row set.  This */
/*                    address is base-relative:  JBASE1+RB1 is the actual */
/*                    base address of the row vectors. */

/*     NR1            is the number of rows in the considered portion of */
/*                    the first join row set.  The portion in question */
/*                    is the set of row vectors corresponding to a */
/*                    single segment vector, namely, the one occupying */
/*                    the first NT1 elements of SEGVEC. */

/*     JBASE2, */
/*     NT2, */
/*     RB2, */
/*     NR2            are analogous quantities to JBASE1, NT2, RB1, and */
/*                    NR1; the quantities here apply to the second input */
/*                    join row set.  The segment vector qualifying the */
/*                    input row vectors from the second join row set */
/*                    occupies elements NT1+1 through NT1+NT2 of SEGVEC. */


/*     NJCNST         is the number of join constraints that must be */
/*                    satisfied by the output join row set.  Each of the */
/*                    input arrays CPIDX1, CLIDX1, OPS, CPIDX2, and */
/*                    CLIDX2 contains NJCNST elements. */

/*     ACTIVE         is an array of logical flags indicating which */
/*                    constraints are currently applicable.  The Nth */
/*                    element of ACTIVE indicates whether or not to apply */
/*                    the Nth constraint:  if ACTIVE(N) is .TRUE., the */
/*                    constraint is applicable, otherwise it isn't. */

/*                    In order for a join constraint to be active, it */
/*                    must relate a column in the first join row set */
/*                    to a column in the second join row set.  The LHS */
/*                    and RHS of the constraint need not refer */
/*                    to the first and second join row sets respectively. */

/*                    The elements of the other input arguments that */
/*                    define constraints are defined when the */
/*                    corresponding element of ACTIVE is .TRUE.  For */
/*                    example, when the second constraint is not active, */
/*                    the second column descriptor in DTDSCS may not be */
/*                    defined. */

/*     CPIDX1, */
/*     CLIDX1         are, respectively, a set of cross product indices */
/*                    and column indices that define the columns on the */
/*                    left-hand sides of the input constraints.  If the */
/*                    first input join row set contains rows from NT1 */
/*                    tables and the second input join row set contains */
/*                    rows from NT2 tables, then there are (NT1+NT2) */
/*                    components in the cross product of the tables */
/*                    specified by the input join row sets.  We'll index */
/*                    these from 1 to (NT1+NT2), with table 1 being the */
/*                    first table of the first input join row set, table */
/*                    2 being the second table of the first input join */
/*                    row set, table (NT1+1) being the first table of the */
/*                    second input join row set, and so on.  Each element */
/*                    of the argument CPIDX1 designates a table by this */
/*                    counting scheme.  The corresponding element of the */
/*                    argument CLIDX1 is the index of a column in the */
/*                    specified table.  The index is the ordinal position */
/*                    of the column's attributes in the column attribute */
/*                    list for the table containing the column. */


/*     ELTS1          is an array of element indices that apply to the */
/*                    columns on the left-hand-sides of constraints.  The */
/*                    Ith element of ELTS1 is the column entry index */
/*                    that applies to the Ith constraint. */

/*     OPS            is an array of relational operator codes.  The */
/*                    Ith code applies to the Ith join constraint. */

/*     CPIDX2, */
/*     CLIDX2         are, respectively, a set of cross product indices */
/*                    and column indices that define the columns on the */
/*                    right-hand sides of the input constraints.  The */
/*                    meanings of these arrays are analogous to those */
/*                    of CPIDX1 and CLIDX1.  Note that the indices are */
/*                    relative to the combined table of width NT1+NT2, */
/*                    *not* to the second table. */

/*     ELTS2          is an array of element indices that apply to the */
/*                    columns on the right-hand-sides of constraints. */
/*                    The Ith element of ELTS2 is the column entry index */
/*                    that applies to the Ith constraint. */

/*     STHAN          is an array of EK file handles.  The Ith element */
/*                    of STHAN is the handle of the EK containing the */
/*                    Ith loaded segment. */

/*     STSDSC         is an array of segment descriptors for all */
/*                    loaded segments. */

/*     STDTPT         is an array of descriptor table pointers all of */
/*                    the loaded segments.  For the Ith loaded segment, */

/*                       STDTPT(I) */

/*                    contains the node number of the descriptor entry */
/*                    of the first column in the Ith segment, where the */
/*                    order of columns is determined by the order in */
/*                    which the columns appear in the parent table's */
/*                    column attribute list. */

/*     DTPOOL, */
/*     DTDSCS         are, respectively, the linked list pool for */
/*                    the column descriptor array and the column */
/*                    descriptor array itself.  The latter contains */
/*                    a descriptor for each loaded column. */

/* $ Detailed_Output */

/*     None.  This routine operates entirely by side effects. */

/* $ Parameters */

/*     See the include files. */

/* $ Exceptions */

/*     1) This routine */

/*     All other error checking must be performed by the caller of this */
/*     routine.  Presently, that caller is ZZEKJOIN. */

/* $ Files */

/*     1)  This routine uses the EK scratch area, which employs a scratch */
/*         DAS file. */

/* $ Particulars */

/*     This routine prepares ZZEKJNXT to return row vectors satisfying */
/*     a specified set of join constraints.  The principal job of this */
/*     routine is to determine key columns to guide the order in which */
/*     candidate row vectors are tested.  When key columns are */
/*     available, this routine produces order vectors for those columns. */

/*     This routine writes to the EK scratch area.  The caller of this */
/*     routine must take this fact into account, because this routine */
/*     will normally be called during the construction of a join row set, */
/*     and scratch area addresses claimed by this routine will be */
/*     interspersed with those owned by the caller. */

/*     The territory occupied by this routine may be reclaimed later by */
/*     `squeezing' unused addresses out of the final join row set.  This */
/*     operation can be performed by ZZEKJSQZ. */

/* $ Examples */

/*     See ZZEKJOIN. */

/* $ Restrictions */

/*     1)  This routine should not be called by routines outside of the */
/*         EK system. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman   (JPL) */

/* $ Version */

/* -    SPICELIB Version 2.0.0, 20-JUL-1998 (NJB) */

/*        Modified entry point to set CASE to EMPTY when either */
/*        input row count is zero. */

/* -    Beta Version 1.0.0, 11-OCT-1995 (NJB) */

/* -& */
    if (return_()) {
	return 0;
    }
    chkin_("ZZEKJPRP", (ftnlen)8);

/*     We don't validate the inputs; these must be checked by ZZEKJOIN, */
/*     the only routine that should call this one. */

/*     Not much preparation is required if either input row count is */
/*     zero, since the cartesian product will be zero. */

    if (*nr1 == 0 || *nr2 == 0) {
	case__ = 4;
	chkout_("ZZEKJPRP", (ftnlen)8);
	return 0;
    }

/*     Set the table count and segment vector count for the output join */
/*     row set. */

    nt3 = *nt1 + *nt2;

/*     Create handle, segment base, and column descriptor */
/*     arrays for both sides of each active relational constraint. */

    i__1 = *njcnst;
    for (j = 1; j <= i__1; ++j) {
	if (active[j - 1]) {
	    ltab = cpidx1[j - 1];
	    rtab = cpidx2[j - 1];
	    lseg = segvec[ltab - 1];
	    rseg = segvec[rtab - 1];
	    lhans[(i__2 = j - 1) < 100 && 0 <= i__2 ? i__2 : s_rnge("lhans", 
		    i__2, "zzekjtst_", (ftnlen)654)] = sthan[lseg - 1];
	    rhans[(i__2 = j - 1) < 100 && 0 <= i__2 ? i__2 : s_rnge("rhans", 
		    i__2, "zzekjtst_", (ftnlen)655)] = sthan[rseg - 1];
	    movei_(&stsdsc[lseg * 24 - 24], &c__24, &lsdsc[(i__2 = j * 24 - 
		    24) < 2400 && 0 <= i__2 ? i__2 : s_rnge("lsdsc", i__2, 
		    "zzekjtst_", (ftnlen)657)]);
	    movei_(&stsdsc[rseg * 24 - 24], &c__24, &rsdsc[(i__2 = j * 24 - 
		    24) < 2400 && 0 <= i__2 ? i__2 : s_rnge("rsdsc", i__2, 
		    "zzekjtst_", (ftnlen)658)]);
	    dtptr = stdtpt[lseg - 1];
	    i__2 = clidx1[j - 1];
	    for (k = 2; k <= i__2; ++k) {
		dtptr = lnknxt_(&dtptr, dtpool);
	    }
	    movei_(&dtdscs[dtptr * 11 - 11], &c__11, &ldscrs[(i__2 = j * 11 - 
		    11) < 1100 && 0 <= i__2 ? i__2 : s_rnge("ldscrs", i__2, 
		    "zzekjtst_", (ftnlen)666)]);
	    dtptr = stdtpt[rseg - 1];
	    i__2 = clidx2[j - 1];
	    for (k = 2; k <= i__2; ++k) {
		dtptr = lnknxt_(&dtptr, dtpool);
	    }
	    movei_(&dtdscs[dtptr * 11 - 11], &c__11, &rdscrs[(i__2 = j * 11 - 
		    11) < 1100 && 0 <= i__2 ? i__2 : s_rnge("rdscrs", i__2, 
		    "zzekjtst_", (ftnlen)676)]);
	}
    }

/*     Our objective is to limit as far as possible the number of */
/*     row vectors that have to be tested against the join constraints. */

/*     We break the problem down into cases as follows: */

/*        1)  Try to find a pair of columns related by an equi-join */
/*            constraint.  If such a pair is found, sort each input */
/*            join row set using the appropriate column as a key. */
/*            We then can fairly rapidly compare row vectors for */
/*            equality in the columns to which the equi-join constraint */
/*            applies, and limit the application of the remaining tests */
/*            to row vectors that satisfy the first test. */

/*        2)  If no equi-join constraints are available, look for */
/*            join constraints using the operators LE, LT, GE, or GT. */
/*            Sort as in (1); then apply the rest of the constraints. */

/*        3)  Hard luck:  the only constraints we have (if any) involve */
/*            the operators NE, LIKE, or UNLIKE, none of which are */
/*            helpful.  Test every row vector. */


/*     First step:  We try to find a pair of columns related by an */
/*     equi-join constraint. */

    case__ = 3;
    j = 1;
    fnd = FALSE_;
    while(j <= *njcnst && ! fnd) {
	if (active[j - 1] && ops[j - 1] == 1) {

/*           Good deal, we've got an equi-join constraint.  Save the */
/*           index of this constraint. */

	    case__ = 1;
	    cnstr = j;
	    fnd = TRUE_;
	} else {
	    ++j;
	}
    }
    if (case__ == 3) {
	j = 1;
	fnd = FALSE_;
	while(j <= *njcnst && ! fnd) {
	    if (active[j - 1]) {
		if (ops[j - 1] == 5 || ops[j - 1] == 4 || ops[j - 1] == 2 || 
			ops[j - 1] == 3) {

/*                 We've got a non-equi-join constraint.  Save the */
/*                 index of this constraint. */

		    case__ = 2;
		    cnstr = j;
		    fnd = TRUE_;
		}
	    }
	    if (! fnd) {
		++j;
	    }
	}
    }

/*     At this point, we know which case we've got.  If we've picked */
/*     a distinguished constraint, produce order vectors for each */
/*     set of input rows vectors, using the keys defined by the */
/*     join constraint. */

    if (case__ != 3) {

/*        Produce an order vector for the column on the left side of */
/*        the CNSTR constraint.  We'll do this by turning the set of */
/*        row vectors we want to sort into a join row set.  We'll */
/*        create the join row set metadata and just make it point to */
/*        the collection of row vectors we wish to sort.  Consult the */
/*        join row set include file for a picture of the data structure */
/*        we're creating. */

	zzekstop_(&lbase);
	ltab = cpidx1[cnstr - 1];
	lcol = clidx1[cnstr - 1];
	lelt = elts1[cnstr - 1];

/*        Set JBASE to the base address of the join row set containing */
/*        the table indicated by LTAB.  Set NT, NR and RB to indicate, */
/*        respectively, the number of tables in this join row set, the */
/*        number of rows in the join row set, and the base address of the */
/*        relevant row vector set.  If LTAB is in the second join row */
/*        set, we'll adjust TAB to indicate position relative to the set */
/*        of tables defining the second join row set. */

	if (ltab <= *nt1) {
	    jbase = *jbase1;
	    nt = *nt1;
	    nr = *nr1;
	    rb = *rb1;
	    tab = ltab;
	} else {
	    jbase = *jbase2;
	    nt = *nt2;
	    nr = *nr2;
	    rb = *rb2;
	    tab = ltab - *nt1;
	}

/*        Save the dimensions and base addresses we'll need later. */

	svbas1 = jbase;
	svnt1 = nt;
	svrb1 = rb;
	svnr1 = nr;
	zzekspsh_(&c__1, &c__0);
	zzekspsh_(&c__1, &nr);
	zzekspsh_(&c__1, &c__1);
	zzekspsh_(&c__1, &c__1);
	zzekspsh_(&c__1, &segvec[ltab - 1]);
	zzekspsh_(&c__1, &c__7);
	zzekspsh_(&c__1, &nr);
	i__1 = nr;
	for (i__ = 1; i__ <= i__1; ++i__) {

/*           Grab the row pointer in position TAB from the Ith row */
/*           vector from the join row set containing the parent table */
/*           of the LHS constraint column. */

	    base = jbase + rb + (i__ - 1) * (nt + 1);
	    i__2 = base + tab;
	    i__3 = base + tab;
	    zzeksrd_(&i__2, &i__3, minirv);

/*           Fill in the segment vector pointer for the new very */
/*           narrow row vector. */

	    minirv[1] = 4;

/*           Append to the join row set under construction. */

	    zzekspsh_(&c__2, minirv);
	}
	zzekstop_(&top);
	i__1 = lbase + 1;
	i__2 = lbase + 1;
	i__3 = top - lbase;
	zzeksupd_(&i__1, &i__2, &i__3);
	zzekjsrt_(&c__1, &lbase, &c__1, &c__1, &lcol, &lelt, &c__0, sthan, 
		stsdsc, stdtpt, dtpool, dtdscs, &lovbas);

/*        Produce an order vector for the column on the right side of */
/*        the CNSTR constraint. */

	zzekstop_(&rbase);
	rtab = cpidx2[cnstr - 1];
	rcol = clidx2[cnstr - 1];
	relt = elts2[cnstr - 1];

/*        Set JBASE to the base address of the join row set containing */
/*        the table indicated by RTAB.  Set NT, NR and RB to indicate, */
/*        respectively, the number of tables in this join row set, the */
/*        number of rows in the join row set, and the base address of the */
/*        relevant row vector set.  If RTAB is in the second join row */
/*        set, we'll adjust TAB to indicate position relative to the set */
/*        of tables defining the second join row set. */

	if (rtab <= *nt1) {
	    jbase = *jbase1;
	    nt = *nt1;
	    nr = *nr1;
	    rb = *rb1;
	    tab = rtab;
	} else {
	    jbase = *jbase2;
	    nt = *nt2;
	    nr = *nr2;
	    rb = *rb2;
	    tab = rtab - *nt1;
	}

/*        Save the dimensions and base addresses we'll need later. */

	svbas2 = jbase;
	svnt2 = nt;
	svrb2 = rb;
	svnr2 = nr;
	zzekspsh_(&c__1, &c__0);
	zzekspsh_(&c__1, &nr);
	zzekspsh_(&c__1, &c__1);
	zzekspsh_(&c__1, &c__1);
	zzekspsh_(&c__1, &segvec[rtab - 1]);
	zzekspsh_(&c__1, &c__7);
	zzekspsh_(&c__1, &nr);
	i__1 = nr;
	for (i__ = 1; i__ <= i__1; ++i__) {

/*           Grab the row pointer in position TAB from the Ith row */
/*           vector from the join row set containing the parent table */
/*           of the RHS constraint column. */

	    base = jbase + rb + (i__ - 1) * (nt + 1);
	    i__2 = base + tab;
	    i__3 = base + tab;
	    zzeksrd_(&i__2, &i__3, minirv);

/*           Fill in the segment vector pointer for the new very */
/*           narrow row vector. */

	    minirv[1] = 4;

/*           Append to the join row set under construction. */

	    zzekspsh_(&c__2, minirv);
	}
	zzekstop_(&top);
	i__1 = rbase + 1;
	i__2 = rbase + 1;
	i__3 = top - rbase;
	zzeksupd_(&i__1, &i__2, &i__3);
	zzekjsrt_(&c__1, &rbase, &c__1, &c__1, &rcol, &relt, &c__0, sthan, 
		stsdsc, stdtpt, dtpool, dtdscs, &rovbas);

/*        Keep a local copy of the active constraint flags, deactivating */
/*        the distinguished one. */

	i__1 = *njcnst;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    locact[(i__2 = i__ - 1) < 100 && 0 <= i__2 ? i__2 : s_rnge("loca"
		    "ct", i__2, "zzekjtst_", (ftnlen)939)] = active[i__ - 1];
	}
	locact[(i__1 = cnstr - 1) < 100 && 0 <= i__1 ? i__1 : s_rnge("locact",
		 i__1, "zzekjtst_", (ftnlen)942)] = FALSE_;
    } else {

/*        This is the `no luck' case.  Save all of the constraints. */

	i__1 = *njcnst;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    locact[(i__2 = i__ - 1) < 100 && 0 <= i__2 ? i__2 : s_rnge("loca"
		    "ct", i__2, "zzekjtst_", (ftnlen)950)] = active[i__ - 1];
	}

/*        Save the counts pertaining to the input join row sets. */

	svnt1 = *nt1;
	svnt2 = *nt2;
	svnr1 = *nr1;
	svnr2 = *nr2;
	svrb1 = *rb1;
	svrb2 = *rb2;
	svbas1 = *jbase1;
	svbas2 = *jbase2;
    }

/*     In the non-equi-join case, record whether the join constraint */
/*     requires the left side to be less than, or less than or equal to, */
/*     the right side. */

    if (case__ == 2) {
	lsmall = ops[cnstr - 1] == 5 || ops[cnstr - 1] == 4;
    }

/*     Keep our own copy of the relational constraints, except for the */
/*     column indices, which are used only in this routine. */

    svncon = *njcnst;
    i__1 = svncon;
    for (i__ = 1; i__ <= i__1; ++i__) {
	svcp1[(i__2 = i__ - 1) < 100 && 0 <= i__2 ? i__2 : s_rnge("svcp1", 
		i__2, "zzekjtst_", (ftnlen)984)] = cpidx1[i__ - 1];
	svops[(i__2 = i__ - 1) < 100 && 0 <= i__2 ? i__2 : s_rnge("svops", 
		i__2, "zzekjtst_", (ftnlen)985)] = ops[i__ - 1];
	svcp2[(i__2 = i__ - 1) < 100 && 0 <= i__2 ? i__2 : s_rnge("svcp2", 
		i__2, "zzekjtst_", (ftnlen)986)] = cpidx2[i__ - 1];
    }

/*     Initialize the pointers we'll use to keep track of the */
/*     row vectors we'll be comparing.  Initialize the DONE flag */
/*     as well. */

    lptr = 1;
    lcur = 1;
    rptr = 1;
    done = FALSE_;
    chkout_("ZZEKJPRP", (ftnlen)8);
    return 0;
/* $Procedure  ZZEKJNXT  ( Return next join row vector ) */

L_zzekjnxt:
/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines.  Users should not call this routine directly due */
/*     to the volatile nature of this routine. */

/*     Return row vectors resulting from the join of two collections */
/*     of row vectors from two join row sets. */

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

/*     LOGICAL               FOUND */
/*     INTEGER               ROWVEC ( * ) */

/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     FOUND      O   Flag indicating whether a row vector was found. */
/*     ROWVEC     O   Row vector matching join constraints. */

/* $ Detailed_Input */

/*     None.  Inputs are set up by calling ZZEKJPRP. */

/* $ Detailed_Output */

/*     FOUND          is a logical flag indicating whether a row vector */
/*                    was found on the current call to this routine. */

/*     ROWVEC         is a row vector that satisfies the join */
/*                    constraints specified by the last set-up call to */
/*                    ZZEKJPRP.  ROWVEC is a composite of two row */
/*                    vectors from the join row sets specified by inputs */
/*                    to ZZEKJPRP.  This row vector does not have the */
/*                    segment vector pointer filled in.  ROWVEC is */
/*                    valid only when FOUND is TRUE. */

/* $ Parameters */

/*     See the include files. */

/* $ Exceptions */

/*     All error checking must be performed by the caller of this */
/*     routine.  Presently, that caller is ZZEKJOIN. */

/* $ Files */

/*     1)  This routine uses the EK scratch area, which employs a scratch */
/*         DAS file. */

/* $ Particulars */

/*     This routine takes advantage of the preparation performed by */
/*     ZZEKJPRP to find with reasonable efficiency row vectors satisfying */
/*     a specified set of join constraints. */

/* $ Examples */

/*     The normal usage of this routine is to call it repeatedly to */
/*     retrieve one row vector at a time, after setting up the */
/*     operation by calling ZZEKJPRP: */

/*         CALL ZZEKJPRP ( ... ) */

/*         CALL ZZEKJNXT ( FOUND, ROWVEC ) */

/*         DO WHILE ( FOUND ) */

/*            . */
/*            . */
/*            . */

/*            CALL ZZEKJNXT ( FOUND, ROWVEC ) */

/*         END DO */


/* $ Restrictions */

/*     1)  This routine should not be called by routines outside of the */
/*         EK system. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman   (JPL) */

/* $ Version */

/* -    SPICELIB Version 2.0.0, 20-JUL-1998 (NJB) */

/*        Modified entry point ZZEKJNXT to set FOUND to .FALSE. on the */
/*        first pass when CASE is EMPTY. */

/* -    Beta Version 1.0.0, 08-AUG-1995 (NJB) */

/* -& */

/*     No row vector found to start with. */

    *found = FALSE_;

/*     The action we take depends on the join constraint situation. */
/*     Handle the "empty" case first. */

    if (case__ == 4) {
	return 0;
    } else if (case__ == 1) {
	while(! done && ! (*found)) {

/*           At this point, LCUR and RPTR should point to the current */
/*           pair of order vector entries to use.  We should always have */

/*              1     <  LPTR  <  SVNR1 */
/*                    -        - */

/*              LPTR  <  LCUR  <  SVNR1 */
/*                    -        - */

/*              1     <  RPTR  <  SVNR2 */
/*                    -        - */

/*           here. */

/*           Look up the next set of row vector indices.  Get the row */
/*           numbers in the join columns for each order vector in our */
/*           mini-join row sets that we created for sorting. */

	    i__1 = lovbas + lcur;
	    i__2 = lovbas + lcur;
	    zzeksrd_(&i__1, &i__2, &lrvidx);
	    i__1 = rovbas + rptr;
	    i__2 = rovbas + rptr;
	    zzeksrd_(&i__1, &i__2, &rrvidx);
	    addrss = lbase + 7 + (lrvidx - 1 << 1) + 1;
	    zzeksrd_(&addrss, &addrss, &lrow);
	    addrss = rbase + 7 + (rrvidx - 1 << 1) + 1;
	    zzeksrd_(&addrss, &addrss, &rrow);

/*           Compare column entries, and advance the pointers as */
/*           required. */

	    if (zzekrcmp_(&c__5, &c__1, &lhans[(i__1 = cnstr - 1) < 100 && 0 
		    <= i__1 ? i__1 : s_rnge("lhans", i__1, "zzekjtst_", (
		    ftnlen)1201)], &lsdsc[(i__2 = cnstr * 24 - 24) < 2400 && 
		    0 <= i__2 ? i__2 : s_rnge("lsdsc", i__2, "zzekjtst_", (
		    ftnlen)1201)], &ldscrs[(i__3 = cnstr * 11 - 11) < 1100 && 
		    0 <= i__3 ? i__3 : s_rnge("ldscrs", i__3, "zzekjtst_", (
		    ftnlen)1201)], &lrow, &lelt, &rhans[(i__4 = cnstr - 1) < 
		    100 && 0 <= i__4 ? i__4 : s_rnge("rhans", i__4, "zzekjts"
		    "t_", (ftnlen)1201)], &rsdsc[(i__5 = cnstr * 24 - 24) < 
		    2400 && 0 <= i__5 ? i__5 : s_rnge("rsdsc", i__5, "zzekjt"
		    "st_", (ftnlen)1201)], &rdscrs[(i__6 = cnstr * 11 - 11) < 
		    1100 && 0 <= i__6 ? i__6 : s_rnge("rdscrs", i__6, "zzekj"
		    "tst_", (ftnlen)1201)], &rrow, &relt)) {


/*              The `left' key entry is smaller.  Advance the bottom */
/*              pointer on the left side. */

		if (lptr < svnr1) {
		    ++lptr;
		    lcur = lptr;
		} else {
		    done = TRUE_;
		}
	    } else if (zzekrcmp_(&c__1, &c__1, &lhans[(i__1 = cnstr - 1) < 
		    100 && 0 <= i__1 ? i__1 : s_rnge("lhans", i__1, "zzekjts"
		    "t_", (ftnlen)1227)], &lsdsc[(i__2 = cnstr * 24 - 24) < 
		    2400 && 0 <= i__2 ? i__2 : s_rnge("lsdsc", i__2, "zzekjt"
		    "st_", (ftnlen)1227)], &ldscrs[(i__3 = cnstr * 11 - 11) < 
		    1100 && 0 <= i__3 ? i__3 : s_rnge("ldscrs", i__3, "zzekj"
		    "tst_", (ftnlen)1227)], &lrow, &lelt, &rhans[(i__4 = cnstr 
		    - 1) < 100 && 0 <= i__4 ? i__4 : s_rnge("rhans", i__4, 
		    "zzekjtst_", (ftnlen)1227)], &rsdsc[(i__5 = cnstr * 24 - 
		    24) < 2400 && 0 <= i__5 ? i__5 : s_rnge("rsdsc", i__5, 
		    "zzekjtst_", (ftnlen)1227)], &rdscrs[(i__6 = cnstr * 11 - 
		    11) < 1100 && 0 <= i__6 ? i__6 : s_rnge("rdscrs", i__6, 
		    "zzekjtst_", (ftnlen)1227)], &rrow, &relt)) {


/*              The `left' key entry is equal.  Form a composite */
/*              row vector and test it against the full set of active */
/*              constraints. */

		if (svcp1[(i__1 = cnstr - 1) < 100 && 0 <= i__1 ? i__1 : 
			s_rnge("svcp1", i__1, "zzekjtst_", (ftnlen)1245)] <= 
			svnt1) {

/*                 The parent table of the column on the LHS of our */
/*                 equi-join constraint belongs to the first join */
/*                 row set. */

		    j = 1;
		    k = svnt1 + 1;
		} else {
		    j = svnt2 + 1;
		    k = 1;
		}
		offset = svrb1 + (lrvidx - 1) * (svnt1 + 1);
		i__1 = svbas1 + offset + 1;
		i__2 = svbas1 + offset + svnt1;
		zzeksrd_(&i__1, &i__2, &rowvec[j - 1]);
		offset = svrb2 + (rrvidx - 1) * (svnt2 + 1);
		i__1 = svbas2 + offset + 1;
		i__2 = svbas2 + offset + svnt2;
		zzeksrd_(&i__1, &i__2, &rowvec[k - 1]);

/*              Create row arrays for both sides of each active */
/*              relational constraint. */

		i__1 = svncon;
		for (j = 1; j <= i__1; ++j) {
		    if (locact[(i__2 = j - 1) < 100 && 0 <= i__2 ? i__2 : 
			    s_rnge("locact", i__2, "zzekjtst_", (ftnlen)1278)]
			    ) {
			ltab = svcp1[(i__2 = j - 1) < 100 && 0 <= i__2 ? i__2 
				: s_rnge("svcp1", i__2, "zzekjtst_", (ftnlen)
				1279)];
			rtab = svcp2[(i__2 = j - 1) < 100 && 0 <= i__2 ? i__2 
				: s_rnge("svcp2", i__2, "zzekjtst_", (ftnlen)
				1280)];
			lrows[(i__2 = j - 1) < 100 && 0 <= i__2 ? i__2 : 
				s_rnge("lrows", i__2, "zzekjtst_", (ftnlen)
				1281)] = rowvec[ltab - 1];
			rrows[(i__2 = j - 1) < 100 && 0 <= i__2 ? i__2 : 
				s_rnge("rrows", i__2, "zzekjtst_", (ftnlen)
				1282)] = rowvec[rtab - 1];
		    }
		}
		*found = zzekvmch_(&svncon, locact, lhans, lsdsc, ldscrs, 
			lrows, lelts, svops, rhans, rsdsc, rdscrs, rrows, 
			relts);

/*              Update the pointers. */

		if (lcur < svnr1) {
		    ++lcur;
		} else if (lcur == svnr1 && rptr < svnr2) {

/*                 We've compared every left hand entry from RPTR */
/*                 upwards to the right hand entry.  Time to work on */
/*                 the next right hand entry. */

		    ++rptr;
		    lcur = lptr;
		} else {

/*                 LCUR and RPTR point to the last entries in their */
/*                 respective row sets. */

		    done = TRUE_;
		}
	    } else {

/*              The current left key entry is greater than that */
/*              on the right.  It's time to look at the next entry */
/*              on the right, if possible. */

		if (rptr < svnr2) {
		    ++rptr;
		    lcur = lptr;
		} else {
		    done = TRUE_;
		}
	    }

/*           At this point, we've advanced at least one of LPTR, RPTR, */
/*           or LCUR, or else we've set DONE to .TRUE. */

	}
    } else if (case__ == 2) {

/*        This is the non-equi-join case. */

	while(! done && ! (*found)) {

/*           At this point, LPTR and RPTR should point to the current */
/*           pair of order vector entries to use.  We should always have */

/*              1     <  LPTR  <  SVNR1 */
/*                    -        - */

/*              1     <  RPTR  <  SVNR2 */
/*                    -        - */

/*           here. */

/*           Look up the next set of row vector indices.  Get the row */
/*           numbers in the join columns for each order vector in our */
/*           mini-join row sets that we created for sorting. */

	    i__1 = lovbas + lptr;
	    i__2 = lovbas + lptr;
	    zzeksrd_(&i__1, &i__2, &lrvidx);
	    i__1 = rovbas + rptr;
	    i__2 = rovbas + rptr;
	    zzeksrd_(&i__1, &i__2, &rrvidx);
	    addrss = lbase + 7 + (lrvidx - 1 << 1) + 1;
	    zzeksrd_(&addrss, &addrss, &lrow);
	    addrss = rbase + 7 + (rrvidx - 1 << 1) + 1;
	    zzeksrd_(&addrss, &addrss, &rrow);

/*           Compare column entries, and advance the pointers as */
/*           required. */

	    if (zzekrcmp_(&svops[(i__1 = cnstr - 1) < 100 && 0 <= i__1 ? i__1 
		    : s_rnge("svops", i__1, "zzekjtst_", (ftnlen)1378)], &
		    c__1, &lhans[(i__2 = cnstr - 1) < 100 && 0 <= i__2 ? i__2 
		    : s_rnge("lhans", i__2, "zzekjtst_", (ftnlen)1378)], &
		    lsdsc[(i__3 = cnstr * 24 - 24) < 2400 && 0 <= i__3 ? i__3 
		    : s_rnge("lsdsc", i__3, "zzekjtst_", (ftnlen)1378)], &
		    ldscrs[(i__4 = cnstr * 11 - 11) < 1100 && 0 <= i__4 ? 
		    i__4 : s_rnge("ldscrs", i__4, "zzekjtst_", (ftnlen)1378)],
		     &lrow, &lelt, &rhans[(i__5 = cnstr - 1) < 100 && 0 <= 
		    i__5 ? i__5 : s_rnge("rhans", i__5, "zzekjtst_", (ftnlen)
		    1378)], &rsdsc[(i__6 = cnstr * 24 - 24) < 2400 && 0 <= 
		    i__6 ? i__6 : s_rnge("rsdsc", i__6, "zzekjtst_", (ftnlen)
		    1378)], &rdscrs[(i__7 = cnstr * 11 - 11) < 1100 && 0 <= 
		    i__7 ? i__7 : s_rnge("rdscrs", i__7, "zzekjtst_", (ftnlen)
		    1378)], &rrow, &relt)) {


/*              This pair of row vectors satisfies the join constraint. */
/*              Form a composite row vector and test it against the full */
/*              set of active constraints. */

		if (svcp1[(i__1 = cnstr - 1) < 100 && 0 <= i__1 ? i__1 : 
			s_rnge("svcp1", i__1, "zzekjtst_", (ftnlen)1396)] <= 
			svnt1) {

/*                 The parent table of the column on the LHS of our */
/*                 equi-join constraint belongs to the first join */
/*                 row set. */

		    j = 1;
		    k = svnt1 + 1;
		} else {
		    j = svnt2 + 1;
		    k = 1;
		}
		offset = svrb1 + (lrvidx - 1) * (svnt1 + 1);
		i__1 = svbas1 + offset + 1;
		i__2 = svbas1 + offset + svnt1;
		zzeksrd_(&i__1, &i__2, &rowvec[j - 1]);
		offset = svrb2 + (rrvidx - 1) * (svnt2 + 1);
		i__1 = svbas2 + offset + 1;
		i__2 = svbas2 + offset + svnt2;
		zzeksrd_(&i__1, &i__2, &rowvec[k - 1]);

/*              Create row arrays for both sides of each active */
/*              relational constraint. */

		i__1 = svncon;
		for (j = 1; j <= i__1; ++j) {
		    if (locact[(i__2 = j - 1) < 100 && 0 <= i__2 ? i__2 : 
			    s_rnge("locact", i__2, "zzekjtst_", (ftnlen)1430)]
			    ) {
			ltab = svcp1[(i__2 = j - 1) < 100 && 0 <= i__2 ? i__2 
				: s_rnge("svcp1", i__2, "zzekjtst_", (ftnlen)
				1431)];
			rtab = svcp2[(i__2 = j - 1) < 100 && 0 <= i__2 ? i__2 
				: s_rnge("svcp2", i__2, "zzekjtst_", (ftnlen)
				1432)];
			lrows[(i__2 = j - 1) < 100 && 0 <= i__2 ? i__2 : 
				s_rnge("lrows", i__2, "zzekjtst_", (ftnlen)
				1433)] = rowvec[ltab - 1];
			rrows[(i__2 = j - 1) < 100 && 0 <= i__2 ? i__2 : 
				s_rnge("rrows", i__2, "zzekjtst_", (ftnlen)
				1434)] = rowvec[rtab - 1];
		    }
		}
		*found = zzekvmch_(&svncon, locact, lhans, lsdsc, ldscrs, 
			lrows, lelts, svops, rhans, rsdsc, rdscrs, rrows, 
			relts);
		if (lsmall) {

/*                 The `left' key entry is smaller.  All higher-indexed */
/*                 rows on the right side also satisfy the join */
/*                 constraint, combined with the current left hand side. */

		    if (rptr < svnr2) {
			++rptr;
		    } else if (lptr < svnr1) {
			++lptr;
			rptr = 1;
		    } else {
			done = TRUE_;
		    }
		} else {

/*                 The `right' key entry is smaller.  All higher-indexed */
/*                 rows on the left side also satisfy the join */
/*                 constraint, combined with the current right hand side. */

		    if (lptr < svnr1) {
			++lptr;
		    } else if (rptr < svnr2) {
			++rptr;
			lptr = 1;
		    } else {
			done = TRUE_;
		    }
		}

/*              We incremented LPTR or RPTR, or else we set DONE to */
/*              .TRUE. */

	    } else {

/*              The constraint was not met by the rows under */
/*              consideration. */

		if (lsmall) {

/*                 If the right side can be incremented, there's a */
/*                 chance of a match. */

		    if (rptr < svnr2) {
			++rptr;
		    } else {
			done = TRUE_;
		    }
		} else {

/*                 If the left side can be incremented, there's a */
/*                 chance of a match. */

		    if (lptr < svnr1) {
			++lptr;
		    } else {
			done = TRUE_;
		    }
		}

/*              We incremented LPTR or RPTR, or else we set DONE to */
/*              .TRUE. */

	    }
	}
    } else {

/*        We have no order vectors to help us out, so we just loop */
/*        through every possible combination.  When we find a match, */
/*        we return immediately, leaving the pointers set to enable */
/*        continuation of our search when we drop back into the loop */
/*        on a subsequent call. */

	while(lptr <= svnr1) {
	    while(rptr <= svnr2) {

/*              Form a composite row vector and test it against the full */
/*              set of active constraints. */

		offset = svrb1 + (lptr - 1) * (svnt1 + 1);
		i__1 = svbas1 + offset + 1;
		i__2 = svbas1 + offset + svnt1;
		zzeksrd_(&i__1, &i__2, rowvec);
		offset = svrb2 + (rptr - 1) * (svnt2 + 1);
		i__1 = svbas2 + offset + 1;
		i__2 = svbas2 + offset + svnt2;
		zzeksrd_(&i__1, &i__2, &rowvec[svnt1]);

/*              Create row arrays for both sides of each active */
/*              relational constraint. */

		i__1 = svncon;
		for (j = 1; j <= i__1; ++j) {
		    if (locact[(i__2 = j - 1) < 100 && 0 <= i__2 ? i__2 : 
			    s_rnge("locact", i__2, "zzekjtst_", (ftnlen)1571)]
			    ) {
			ltab = svcp1[(i__2 = j - 1) < 100 && 0 <= i__2 ? i__2 
				: s_rnge("svcp1", i__2, "zzekjtst_", (ftnlen)
				1572)];
			rtab = svcp2[(i__2 = j - 1) < 100 && 0 <= i__2 ? i__2 
				: s_rnge("svcp2", i__2, "zzekjtst_", (ftnlen)
				1573)];
			lrows[(i__2 = j - 1) < 100 && 0 <= i__2 ? i__2 : 
				s_rnge("lrows", i__2, "zzekjtst_", (ftnlen)
				1574)] = rowvec[ltab - 1];
			rrows[(i__2 = j - 1) < 100 && 0 <= i__2 ? i__2 : 
				s_rnge("rrows", i__2, "zzekjtst_", (ftnlen)
				1575)] = rowvec[rtab - 1];
		    }
		}
		*found = zzekvmch_(&svncon, locact, lhans, lsdsc, ldscrs, 
			lrows, lelts, svops, rhans, rsdsc, rdscrs, rrows, 
			relts);
		++rptr;
		if (*found) {
		    return 0;
		}
	    }
	    ++lptr;
	    rptr = 1;
	}
    }
    return 0;
} /* zzekjtst_ */

/* Subroutine */ int zzekjtst_(integer *segvec, integer *jbase1, integer *nt1,
	 integer *rb1, integer *nr1, integer *jbase2, integer *nt2, integer *
	rb2, integer *nr2, integer *njcnst, logical *active, integer *cpidx1, 
	integer *clidx1, integer *elts1, integer *ops, integer *cpidx2, 
	integer *clidx2, integer *elts2, integer *sthan, integer *stsdsc, 
	integer *stdtpt, integer *dtpool, integer *dtdscs, logical *found, 
	integer *rowvec)
{
    return zzekjtst_0_(0, segvec, jbase1, nt1, rb1, nr1, jbase2, nt2, rb2, 
	    nr2, njcnst, active, cpidx1, clidx1, elts1, ops, cpidx2, clidx2, 
	    elts2, sthan, stsdsc, stdtpt, dtpool, dtdscs, found, rowvec);
    }

/* Subroutine */ int zzekjprp_(integer *segvec, integer *jbase1, integer *nt1,
	 integer *rb1, integer *nr1, integer *jbase2, integer *nt2, integer *
	rb2, integer *nr2, integer *njcnst, logical *active, integer *cpidx1, 
	integer *clidx1, integer *elts1, integer *ops, integer *cpidx2, 
	integer *clidx2, integer *elts2, integer *sthan, integer *stsdsc, 
	integer *stdtpt, integer *dtpool, integer *dtdscs)
{
    return zzekjtst_0_(1, segvec, jbase1, nt1, rb1, nr1, jbase2, nt2, rb2, 
	    nr2, njcnst, active, cpidx1, clidx1, elts1, ops, cpidx2, clidx2, 
	    elts2, sthan, stsdsc, stdtpt, dtpool, dtdscs, (logical *)0, (
	    integer *)0);
    }

/* Subroutine */ int zzekjnxt_(logical *found, integer *rowvec)
{
    return zzekjtst_0_(2, (integer *)0, (integer *)0, (integer *)0, (integer *
	    )0, (integer *)0, (integer *)0, (integer *)0, (integer *)0, (
	    integer *)0, (integer *)0, (logical *)0, (integer *)0, (integer *)
	    0, (integer *)0, (integer *)0, (integer *)0, (integer *)0, (
	    integer *)0, (integer *)0, (integer *)0, (integer *)0, (integer *)
	    0, (integer *)0, found, rowvec);
    }

