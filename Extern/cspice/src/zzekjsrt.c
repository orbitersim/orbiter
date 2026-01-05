/* zzekjsrt.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;
static integer c__4 = 4;

/* $Procedure      ZZEKJSRT ( EK, join row set union sort ) */
/* Subroutine */ int zzekjsrt_(integer *njrs, integer *ubases, integer *
	norder, integer *otabs, integer *ocols, integer *oelts, integer *
	senses, integer *sthan, integer *stsdsc, integer *stdtpt, integer *
	dtpool, integer *dtdscs, integer *ordbas)
{
    /* System generated locals */
    integer i__1, i__2, i__3;
    char ch__1[32], ch__2[32];

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_cmp(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    static char cdat[32*250000];
    static doublereal ddat[250000];
    static integer idat[250000];
    integer ntab;
    logical nfjg, null;
    extern /* Subroutine */ int zzekvcal_(integer *, integer *, integer *);
    extern logical zzekvcmp_(integer *, integer *, integer *, integer *, 
	    integer *, integer *, integer *, integer *, integer *, integer *, 
	    integer *, integer *, integer *, integer *, integer *);
    extern /* Subroutine */ int zzeksupd_(integer *, integer *, integer *), 
	    zzekspsh_(integer *, integer *), zzekvset_(integer *, integer *), 
	    zzekstop_(integer *);
    integer i__, j, addrj;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    integer cvlen, rvecj[11], svecj[10];
    logical found;
    integer nrloc;
    logical brute;
    integer dtype;
    logical trunc;
    extern /* Subroutine */ int swapi_(integer *, integer *);
    integer nrows, jg;
    static char nf[1*250000];
    integer addrjg, handle, nr, rj;
    extern integer lnknxt_(integer *, integer *);
    extern logical return_(void);
    integer cprime, colptr, eltidx, gap;
    static integer ordvec[250000];
    integer prvbas, row, rjg, rowvec[11], rvecjg[11], rvsize, rwvbas, seg, 
	    segvec[10], sgvbas, svecjg[10], svsize, tabloc, tprime;
    logical jle, nfj;
    extern /* Subroutine */ int setmsg_(char *, ftnlen), errint_(char *, 
	    integer *, ftnlen), sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), errhan_(char *, integer *, ftnlen), zzekrsc_(integer *, 
	    integer *, integer *, integer *, integer *, integer *, char *, 
	    logical *, logical *, ftnlen), zzeksrd_(integer *, integer *, 
	    integer *), zzekrsd_(integer *, integer *, integer *, integer *, 
	    integer *, doublereal *, logical *, logical *), zzekrsi_(integer *
	    , integer *, integer *, integer *, integer *, integer *, logical *
	    , logical *);

/* $ Abstract */

/*     Sort the row vectors of a join row set union, given an order */
/*     relation defined by a set of qualified order-by columns. */

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
/*     NJRS       I   Number of join row sets in union. */
/*     UBASES     I   Base addresses of join row sets of union. */
/*     NORDER     I   Number of order-by columns. */
/*     OTABS      I   Order-by table indices relative to FROM clause. */
/*     OCOLS      I   Order-by column indices. */
/*     OELTS      I   Order-by element indices. */
/*     SENSES     I   Order directions. */
/*     STHAN      I   Handles of loaded files from segment table. */
/*     STSDSC     I   Array of descriptors of loaded segments. */
/*     STDTPT     I   Array of pointers to column descriptors. */
/*     DTPOOL     I   Column descriptor table pool. */
/*     DTDSCS     I   Column descriptor table. */
/*     ORDBAS     O   Scratch area base address for order vector. */

/* $ Detailed_Input */

/*     NJRS, */
/*     UBASES         are, respectively, the number of join row sets in */
/*                    the input join row set union, and the base */
/*                    addresses of those join row sets. */

/*     NORDER         is the number of order-by columns used to define */
/*                    the order relation used for sorting. */

/*     OTABS          is an array of indices identifying the parent */
/*                    tables of the order-by columns.  These indices */
/*                    are the ordinal positions of the parent tables */
/*                    in the FROM clause of the query to which the */
/*                    input joint row set corresponds. */

/*     OCOLS          is an array of indices identifying the order-by */
/*                    columns.  These indices are the ordinal positions */
/*                    of the columns in their virtual parent tables. */
/*                    The order of columns in virtual tables is set */
/*                    when EKs are loaded by the routine EKLEF.  The */
/*                    Nth element of OCOLS applies to the Nth order-by */
/*                    column. */

/*     OELTS          is an array of element indices identifying the */
/*                    order-by column entry elements to use when making */
/*                    order comparisons.  These indices are ignored for */
/*                    scalar order-by columns, but must be set properly */
/*                    for vector-valued order-by columns.  For example, */
/*                    if an order-by column has size 5, one could make */
/*                    order comparisons using the third elements of */
/*                    entries in this column.  The Nth element of OELTS */
/*                    applies to the Nth order-by column. */

/*     SENSES         is an array of parameters indicating the ordering */
/*                    sense for each order-by column.  An ordering sense */
/*                    can be ascending (the default) or descending.  The */
/*                    values indicating these senses are EQASND and */
/*                    EQDSND respectively.  These parameters are defined */
/*                    in the include file ekquery.inc.  The Nth element */
/*                    of SENSES applies to the Nth order-by column. */

/*     STHAN          is an array of EK handles corresponding to loaded */
/*                    segments.  STHAN is expected to be the array of */
/*                    the same name maintained by EKQMGR. */

/*     STSDSC         is an array of descriptors of loaded segments. */
/*                    STSDSC is expected to be the array of the same name */
/*                    maintained by EKQMGR. */

/*     STDTPT         is an array of pointers that map segments to lists */
/*                    of column descriptors in the column descriptor */
/*                    pool.  The Nth element of STDTPT is the head node */
/*                    number for the column descriptor list of the Nth */
/*                    loaded segment.  The column descriptor list is */
/*                    indexed by the linked list pool DTPOOL.  STDTPT is */
/*                    expected to be the array of the same name */
/*                    maintained by EKQMGR. */

/*     DTPOOL         is a linked list pool used to index the column */
/*                    descriptor array DTDSCS.  DTPOOL is expected to be */
/*                    the array of the same name maintained by EKQMGR. */

/*     DTDSCS         is an array of column descriptors for each loaded */
/*                    column.  There is a separate descriptor for each */
/*                    column in each segment.  The Nth node of DTPOOL */
/*                    is considered to point to the Nth element of */
/*                    DTDSCS.  DTDSCS is expected to be the array of the */
/*                    same name maintained by EKQMGR. */

/* $ Detailed_Output */

/*     ORDBAS         is the scratch area base address of the order */
/*                    vector created by this routine.  This address is */
/*                    the predecessor of the first scratch area address */
/*                    occupied by the order vector. */

/*                    The order vector indicates the order of the row */
/*                    vectors of the input join row set union, where the */
/*                    order relation is defined by the order-by columns, */
/*                    column entry element indices, and order senses. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the number of order-by columns NORDER is non-positive, */
/*         the error SPICE(INVALIDCOUNT) is signaled. */

/*     2)  If an I/O error occurs while attempting to create an order */
/*         vector for the specified row set, the error will be diagnosed */
/*         by routines called by this routine. */

/*     3)  If the first order-by column descriptor in the list has */
/*         an invalid data type code, the error SPICE(INVALIDTYPE) */
/*         is signaled. */
/* $ Files */

/*     The input join row set is presumed to refer to EK files currently */
/*     loaded via EKLEF. */

/* $ Particulars */

/*     This routine writes to the EK scratch area an order vector for the */
/*     specified join row set union.  The order vector is written in */
/*     ascending order starting at the location following ORDBAS.  The */
/*     order relation is defined by the order-by columns, column entry */
/*     element indices, and order senses. */

/* $ Examples */

/*     See EKGC. */

/* $ Restrictions */

/*     1)  This routine modifies the EK scratch area, and therefore */
/*         should not be used by routines outside of the EK system. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman   (JPL) */

/* $ Version */

/* -    SPICELIB Version 2.2.1, 01-OCT-2021 (NJB) */

/*        Corrected typo in comments. */

/* -    SPICELIB Version 2.2.0, 07-FEB-2015 (NJB) */

/*        Now uses ERRHAN to insert DAS file name into */
/*        long error messages. */

/* -    SPICELIB Version 2.1.0, 07-AUG-2006 (NJB) */

/*        Bug fix:  added initialization of variable PRVBAS to support */
/*                  operation under the Macintosh Intel Fortran */
/*                  compiler. Note that this bug did not affect */
/*                  operation of this routine on other platforms. */

/* -    SPICELIB Version 2.0.0, 09-SEP-2005 (NJB) */

/*        Increased buffer size parameter LIMIT1 from 25K to 250K. */
/*        Declared large buffers SAVED to prevent memory errors */
/*        under CYGWIN. */

/* -    SPICELIB Version 1.1.0, 18-JUN-1999 (WLT) */

/*        Removed several redundant calls to CHKIN */

/* -    Beta Version 1.0.0, 19-OCT-1995 (NJB) */

/* -& */
/* $ Revisions */

/* -    SPICELIB Version 2.1.0, 07-AUG-2006 (NJB) */

/*        Bug fix:  added initialization of variable PRVBAS to support */
/*                  operation under the Macintosh Intel Fortran */
/*                  compiler. Note that this bug did not affect */
/*                  operation of this routine on other platforms. The */
/*                  statement referencing the uninitialized variable */
/*                  was: */

/*           IF (  ( I .EQ. 1 ) .OR. ( SGVBAS .NE. PRVBAS )  ) THEN */

/*        In the previous version of the code, PRVBAS is uninitialized */
/*        when the loop counter I is 1.  PRVBAS *is* initialized when I */
/*        is greater than 1, so the logical value of the IF expression */
/*        is not affected by the lack of proper initialization. */

/*        However, the Intel Fortran compiler for the Mac flags a runtime */
/*        error when the above code is exercised.  So PRVBAS is now */
/*        initialized prior to the above IF statement. */


/* -    SPICELIB Version 2.0.0, 08-SEP-2005 (NJB) */

/*        Increased buffer size parameter LIMIT1 from 25K to 250K. */
/*        Declared large buffers SAVED to prevent memory errors */
/*        under CYGWIN.  The saved buffers are */

/*          CDAT */
/*          DDAT */
/*          IDAT */
/*          NF */
/*          ORDVEC */

/* -& */

/*     SPICELIB functions */


/*     Other functions */


/*     Other local parameters */


/*     Local variables */


/*     Saved variables */

/*     The following variables are saved in order to prevent */
/*     memory errors under Cygwin and in shared object libraries */
/*     under various Unix systems. */


/*     Statement functions */



/*     The following functions test whether two column entries */
/*     are equal.  In the integer and d.p. cases, the test is conclusive. */
/*     In the character case, the test indicates whether the initial */
/*     substrings consisting of the first INISUB characters of each of */
/*     the two entries are equal. */


/*     The following functions indicate whether the first of two column */
/*     entries is less than or equal to the second.  In the integer and */
/*     d.p. cases, the test is conclusive.  In the character case, the */
/*     test indicates whether the initial substring consisting of the */
/*     first INISUB characters of the first entry is less than or equal */
/*     to the corresponding initial substring of length INISUB of the */
/*     second entry. */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("ZZEKJSRT", (ftnlen)8);
    }

/*     If there are no order-by columns, that's an error. */

    if (*norder < 1) {
	setmsg_("Number of order-by columns must be positive but was #.", (
		ftnlen)54);
	errint_("#", norder, (ftnlen)1);
	sigerr_("SPICE(INVALIDCOUNT)", (ftnlen)19);
	chkout_("ZZEKJSRT", (ftnlen)8);
	return 0;
    }

/*     We split the sorting job up into two cases: */

/*        1)  If the number of rows to be sorted is not too large, */
/*            we can gain speed by reading data from the primary */
/*            order-by column into memory and sorting the row number */
/*            array in memory. */

/*        2)  If there's too much data for option (1) to handle, */
/*            we just read data from the order-by columns as needed. */
/*            This algorithm is simple, but very slow, since many */
/*            DAS reads of individual column entries are required. */


/*     Find out how many rows are in the join row set union. */

    nrows = 0;
    i__1 = *njrs;
    for (i__ = 1; i__ <= i__1; ++i__) {
	nrloc = ubases[i__ - 1] + 2;
	zzeksrd_(&nrloc, &nrloc, &nr);
	nrows += nr;
    }

/*     Get the number of tables in the cartesian product represented */
/*     by the join row set union.  The number of tables in the first */
/*     join row set suffices. */

    tabloc = ubases[0] + 3;
    zzeksrd_(&tabloc, &tabloc, &ntab);
    svsize = ntab;
    rvsize = ntab + 1;

/*     We can get the data types of the order-by columns from the */
/*     segment vector of the first row vector in the first join row set. */
/*     Initialize addressing in the join row set union so we can look up */
/*     the locations of these vectors. */

    zzekvset_(njrs, ubases);
    zzekvcal_(&c__1, &rwvbas, &sgvbas);
    i__1 = sgvbas + 1;
    i__2 = sgvbas + svsize;
    zzeksrd_(&i__1, &i__2, segvec);
    tprime = otabs[0];
    cprime = ocols[0];
    seg = segvec[(i__1 = tprime - 1) < 10 && 0 <= i__1 ? i__1 : s_rnge("segv"
	    "ec", i__1, "zzekjsrt_", (ftnlen)538)];
    colptr = stdtpt[seg - 1];
    i__1 = cprime;
    for (i__ = 2; i__ <= i__1; ++i__) {
	colptr = lnknxt_(&colptr, dtpool);
    }
    dtype = dtdscs[colptr * 11 - 10];
    if (nrows <= 250000) {

/*        Case 1. */

/*        We have a small enough quantity of data that we may be able */
/*        to speed up sorting by using memory.  Here's the plan: */

/*        We'll read data for the primary order-by column into memory. */
/*        The `primary' column is the one whose index appears first */
/*        in the input list of column indices.  We'll also maintain a */
/*        null flag array for the primary column.  If we can figure out */
/*        the order relation between two rows by looking at entries in */
/*        the primary order-by column, fine.  Otherwise, we let ZZEKVCMP */
/*        perform the comparison. */

/*        We'll sort the set of row vector numbers of the matching rows */
/*        in parallel with our data sort. */

/*        Character columns present a special case:  their string length */
/*        can get pretty big, and it could take a lot of memory to store */
/*        their column entries.  We compromise here:  we store only the */
/*        first INISUB characters of each character column entry.  If */
/*        we can't decide the order of two strings based on these initial */
/*        substrings, we let ZZEKVCMP handle the matter. */

/*        Read the primary column data.  Keep track of whether we've */
/*        truncated any strings. */

	trunc = FALSE_;
	prvbas = -1;
	i__1 = nrows;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    zzekvcal_(&i__, &rwvbas, &sgvbas);
	    if (i__ == 1 || sgvbas != prvbas) {
		i__2 = sgvbas + 1;
		i__3 = sgvbas + svsize;
		zzeksrd_(&i__2, &i__3, segvec);
		seg = segvec[(i__2 = tprime - 1) < 10 && 0 <= i__2 ? i__2 : 
			s_rnge("segvec", i__2, "zzekjsrt_", (ftnlen)589)];
		handle = sthan[seg - 1];
		colptr = stdtpt[seg - 1];
		i__2 = cprime;
		for (j = 2; j <= i__2; ++j) {
		    colptr = lnknxt_(&colptr, dtpool);
		}
	    }
	    i__2 = rwvbas + 1;
	    i__3 = rwvbas + rvsize;
	    zzeksrd_(&i__2, &i__3, rowvec);
	    row = rowvec[(i__2 = tprime - 1) < 11 && 0 <= i__2 ? i__2 : 
		    s_rnge("rowvec", i__2, "zzekjsrt_", (ftnlen)602)];
	    eltidx = oelts[cprime - 1];
	    if (dtype == 1) {
		zzekrsc_(&handle, &stsdsc[seg * 24 - 24], &dtdscs[colptr * 11 
			- 11], &row, &eltidx, &cvlen, cdat + (((i__2 = i__ - 
			1) < 250000 && 0 <= i__2 ? i__2 : s_rnge("cdat", i__2,
			 "zzekjsrt_", (ftnlen)608)) << 5), &null, &found, (
			ftnlen)32);
		if (! found) {
		    setmsg_("EK = #; SEG = #; ROW = #; COLIDX = #; ELT = #; "
			    "column entry elt was not found.", (ftnlen)78);
		    errhan_("#", &handle, (ftnlen)1);
		    errint_("#", &seg, (ftnlen)1);
		    errint_("#", &row, (ftnlen)1);
		    errint_("#", &dtdscs[colptr * 11 - 3], (ftnlen)1);
		    errint_("#", &eltidx, (ftnlen)1);
		    sigerr_("SPICE(BUG)", (ftnlen)10);
		    chkout_("ZZEKJSRT", (ftnlen)8);
		    return 0;
		}
		trunc = trunc || cvlen > 32;
	    } else if (dtype == 2 || dtype == 4) {
		zzekrsd_(&handle, &stsdsc[seg * 24 - 24], &dtdscs[colptr * 11 
			- 11], &row, &eltidx, &ddat[(i__2 = i__ - 1) < 250000 
			&& 0 <= i__2 ? i__2 : s_rnge("ddat", i__2, "zzekjsrt_"
			, (ftnlen)640)], &null, &found);
		if (! found) {
		    setmsg_("EK = #; SEG = #; ROW = #; COLIDX = #; ELT = #; "
			    "column entry elt was not found.", (ftnlen)78);
		    errhan_("#", &handle, (ftnlen)1);
		    errint_("#", &seg, (ftnlen)1);
		    errint_("#", &row, (ftnlen)1);
		    errint_("#", &dtdscs[colptr * 11 - 3], (ftnlen)1);
		    errint_("#", &eltidx, (ftnlen)1);
		    sigerr_("SPICE(BUG)", (ftnlen)10);
		    chkout_("ZZEKJSRT", (ftnlen)8);
		    return 0;
		}
	    } else if (dtype == 3) {
		zzekrsi_(&handle, &stsdsc[seg * 24 - 24], &dtdscs[colptr * 11 
			- 11], &row, &eltidx, &idat[(i__2 = i__ - 1) < 250000 
			&& 0 <= i__2 ? i__2 : s_rnge("idat", i__2, "zzekjsrt_"
			, (ftnlen)670)], &null, &found);
		if (! found) {
		    setmsg_("EK = #; SEG = #; ROW = #; COLIDX = #; ELT = #; "
			    "column entry elt was not found.", (ftnlen)78);
		    errhan_("#", &handle, (ftnlen)1);
		    errint_("#", &seg, (ftnlen)1);
		    errint_("#", &row, (ftnlen)1);
		    errint_("#", &dtdscs[colptr * 11 - 3], (ftnlen)1);
		    errint_("#", &eltidx, (ftnlen)1);
		    sigerr_("SPICE(BUG)", (ftnlen)10);
		    chkout_("ZZEKJSRT", (ftnlen)8);
		    return 0;
		}
	    } else {

/*              We must have a bogus column descriptor. */

		setmsg_("Unrecognized data type # for first column.", (ftnlen)
			42);
		errint_("#", &dtype, (ftnlen)1);
		sigerr_("SPICE(INVALIDTYPE)", (ftnlen)18);
		chkout_("ZZEKJSRT", (ftnlen)8);
		return 0;
	    }

/*           Set the character null flag for the current column entry. */

	    if (null) {
		*(unsigned char *)&nf[(i__2 = i__ - 1) < 250000 && 0 <= i__2 ?
			 i__2 : s_rnge("nf", i__2, "zzekjsrt_", (ftnlen)714)] 
			= 'T';
	    } else {
		*(unsigned char *)&nf[(i__2 = i__ - 1) < 250000 && 0 <= i__2 ?
			 i__2 : s_rnge("nf", i__2, "zzekjsrt_", (ftnlen)716)] 
			= 'F';
	    }
	    prvbas = sgvbas;
	}

/*        Initialize the order vector. */

	i__1 = nrows;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    ordvec[(i__2 = i__ - 1) < 250000 && 0 <= i__2 ? i__2 : s_rnge(
		    "ordvec", i__2, "zzekjsrt_", (ftnlen)728)] = i__;
	}

/*        At this point, we've read in the data for the primary order-by */
/*        column, and also have set the null flag array for the column. */
/*        We're ready to proceed with our sort. */

	gap = nrows / 2;
	while(gap > 0) {
	    i__1 = nrows;
	    for (i__ = gap + 1; i__ <= i__1; ++i__) {
		j = i__ - gap;
		while(j > 0) {
		    jg = j + gap;

/*                 Compare the Jth and JGth rows of the row set.  The */
/*                 logical JLE is TRUE when the Jth element is less than */
/*                 or equal to the JGth.  If the Jth and JGth elements */
/*                 compare equal, and there is more than one order-by */
/*                 column or if we've truncated string data, we'll have */
/*                 to go on and make a conclusive test.  Otherwise, we */
/*                 can set JLE based on the data we've read. */

/*                 Set the data array indices of the Jth and JGth */
/*                 elements, as indicated by the order vector. */

		    rj = ordvec[(i__2 = j - 1) < 250000 && 0 <= i__2 ? i__2 : 
			    s_rnge("ordvec", i__2, "zzekjsrt_", (ftnlen)759)];
		    rjg = ordvec[(i__2 = jg - 1) < 250000 && 0 <= i__2 ? i__2 
			    : s_rnge("ordvec", i__2, "zzekjsrt_", (ftnlen)760)
			    ];
		    nfj = *(unsigned char *)&nf[(i__2 = rj - 1) < 250000 && 0 
			    <= i__2 ? i__2 : s_rnge("nf", i__2, "zzekjsrt_", (
			    ftnlen)762)] == 'T';
		    nfjg = *(unsigned char *)&nf[(i__2 = rjg - 1) < 250000 && 
			    0 <= i__2 ? i__2 : s_rnge("nf", i__2, "zzekjsrt_",
			     (ftnlen)763)] == 'T';

/*                 Start out hoping for the best:  that we won't have */
/*                 to do a brute-force comparison. */

		    brute = FALSE_;
		    if (dtype == 3) {
			if (*norder == 1) {

/*                       We can make a decision based on the data in */
/*                       memory. */

			    if (senses[0] == 0) {
				jle = nfj || ! (nfj || nfjg) && idat[(i__2 = 
					rj - 1) < 250000 && 0 <= i__2 ? i__2 :
					 s_rnge("idat", i__2, "zzekjsrt_", (
					ftnlen)781)] <= idat[(i__3 = rjg - 1) 
					< 250000 && 0 <= i__3 ? i__3 : s_rnge(
					"idat", i__3, "zzekjsrt_", (ftnlen)
					781)];
			    } else {
				jle = nfjg || ! (nfj || nfjg) && idat[(i__2 = 
					rj - 1) < 250000 && 0 <= i__2 ? i__2 :
					 s_rnge("idat", i__2, "zzekjsrt_", (
					ftnlen)783)] >= idat[(i__3 = rjg - 1) 
					< 250000 && 0 <= i__3 ? i__3 : s_rnge(
					"idat", i__3, "zzekjsrt_", (ftnlen)
					783)];
			    }
			} else if (! (nfj && nfjg || ! (nfj || nfjg) && idat[(
				i__2 = rj - 1) < 250000 && 0 <= i__2 ? i__2 : 
				s_rnge("idat", i__2, "zzekjsrt_", (ftnlen)787)
				] == idat[(i__3 = rjg - 1) < 250000 && 0 <= 
				i__3 ? i__3 : s_rnge("idat", i__3, "zzekjsrt_"
				, (ftnlen)787)])) {

/*                       If the items we're comparing are unequal, we can */
/*                       still make a decision. */

			    if (senses[0] == 0) {
				jle = nfj || ! (nfj || nfjg) && idat[(i__2 = 
					rj - 1) < 250000 && 0 <= i__2 ? i__2 :
					 s_rnge("idat", i__2, "zzekjsrt_", (
					ftnlen)795)] <= idat[(i__3 = rjg - 1) 
					< 250000 && 0 <= i__3 ? i__3 : s_rnge(
					"idat", i__3, "zzekjsrt_", (ftnlen)
					795)];
			    } else {
				jle = nfjg || ! (nfj || nfjg) && idat[(i__2 = 
					rj - 1) < 250000 && 0 <= i__2 ? i__2 :
					 s_rnge("idat", i__2, "zzekjsrt_", (
					ftnlen)797)] >= idat[(i__3 = rjg - 1) 
					< 250000 && 0 <= i__3 ? i__3 : s_rnge(
					"idat", i__3, "zzekjsrt_", (ftnlen)
					797)];
			    }
			} else {

/*                       Otherwise, we'll have to look at values in the */
/*                       other order-by columns.  Get the segment and */
/*                       row vectors to be compared. */

			    brute = TRUE_;
			}
		    } else if (dtype == 2 || dtype == 4) {

/*                    The D.P. case parallels the integer case. */

			if (*norder == 1) {
			    if (senses[0] == 0) {
				jle = nfj || ! (nfj || nfjg) && ddat[(i__2 = 
					rj - 1) < 250000 && 0 <= i__2 ? i__2 :
					 s_rnge("ddat", i__2, "zzekjsrt_", (
					ftnlen)823)] <= ddat[(i__3 = rjg - 1) 
					< 250000 && 0 <= i__3 ? i__3 : s_rnge(
					"ddat", i__3, "zzekjsrt_", (ftnlen)
					823)];
			    } else {
				jle = nfjg || ! (nfj || nfjg) && ddat[(i__2 = 
					rj - 1) < 250000 && 0 <= i__2 ? i__2 :
					 s_rnge("ddat", i__2, "zzekjsrt_", (
					ftnlen)825)] >= ddat[(i__3 = rjg - 1) 
					< 250000 && 0 <= i__3 ? i__3 : s_rnge(
					"ddat", i__3, "zzekjsrt_", (ftnlen)
					825)];
			    }
			} else if (! (nfj && nfjg || ! (nfj || nfjg) && ddat[(
				i__2 = rj - 1) < 250000 && 0 <= i__2 ? i__2 : 
				s_rnge("ddat", i__2, "zzekjsrt_", (ftnlen)829)
				] == ddat[(i__3 = rjg - 1) < 250000 && 0 <= 
				i__3 ? i__3 : s_rnge("ddat", i__3, "zzekjsrt_"
				, (ftnlen)829)])) {
			    if (senses[0] == 0) {
				jle = nfj || ! (nfj || nfjg) && ddat[(i__2 = 
					rj - 1) < 250000 && 0 <= i__2 ? i__2 :
					 s_rnge("ddat", i__2, "zzekjsrt_", (
					ftnlen)834)] <= ddat[(i__3 = rjg - 1) 
					< 250000 && 0 <= i__3 ? i__3 : s_rnge(
					"ddat", i__3, "zzekjsrt_", (ftnlen)
					834)];
			    } else {
				jle = nfjg || ! (nfj || nfjg) && ddat[(i__2 = 
					rj - 1) < 250000 && 0 <= i__2 ? i__2 :
					 s_rnge("ddat", i__2, "zzekjsrt_", (
					ftnlen)836)] >= ddat[(i__3 = rjg - 1) 
					< 250000 && 0 <= i__3 ? i__3 : s_rnge(
					"ddat", i__3, "zzekjsrt_", (ftnlen)
					836)];
			    }
			} else {

/*                       Otherwise, we'll have to look at values in the */
/*                       other order-by columns.  Get the segment and */
/*                       row vectors to be compared. */

			    brute = TRUE_;
			}
		    } else {

/*                    In the character case where there is one order-by */
/*                    column, equality is a problem unless no truncation */
/*                    occurred. */

			if (*norder == 1 && ! trunc) {
			    if (senses[0] == 0) {
				s_copy(ch__1, cdat + (((i__2 = rj - 1) < 
					250000 && 0 <= i__2 ? i__2 : s_rnge(
					"cdat", i__2, "zzekjsrt_", (ftnlen)
					862)) << 5), (ftnlen)32, (ftnlen)32);
				s_copy(ch__2, cdat + (((i__3 = rjg - 1) < 
					250000 && 0 <= i__3 ? i__3 : s_rnge(
					"cdat", i__3, "zzekjsrt_", (ftnlen)
					862)) << 5), (ftnlen)32, (ftnlen)32);
				jle = nfj || ! (nfj || nfjg) && s_cmp(ch__1, 
					ch__2, (ftnlen)32, (ftnlen)32) <= 0;
			    } else {
				s_copy(ch__1, cdat + (((i__2 = rj - 1) < 
					250000 && 0 <= i__2 ? i__2 : s_rnge(
					"cdat", i__2, "zzekjsrt_", (ftnlen)
					864)) << 5), (ftnlen)32, (ftnlen)32);
				s_copy(ch__2, cdat + (((i__3 = rjg - 1) < 
					250000 && 0 <= i__3 ? i__3 : s_rnge(
					"cdat", i__3, "zzekjsrt_", (ftnlen)
					864)) << 5), (ftnlen)32, (ftnlen)32);
				jle = nfjg || ! (nfj || nfjg) && s_cmp(ch__1, 
					ch__2, (ftnlen)32, (ftnlen)32) >= 0;
			    }
			} else /* if(complicated condition) */ {
			    s_copy(ch__1, cdat + (((i__2 = rj - 1) < 250000 &&
				     0 <= i__2 ? i__2 : s_rnge("cdat", i__2, 
				    "zzekjsrt_", (ftnlen)868)) << 5), (ftnlen)
				    32, (ftnlen)32);
			    s_copy(ch__2, cdat + (((i__3 = rjg - 1) < 250000 
				    && 0 <= i__3 ? i__3 : s_rnge("cdat", i__3,
				     "zzekjsrt_", (ftnlen)868)) << 5), (
				    ftnlen)32, (ftnlen)32);
			    if (! (nfj && nfjg || ! (nfj || nfjg) && s_cmp(
				    ch__1, ch__2, (ftnlen)32, (ftnlen)32) == 
				    0)) {

/*                       If the items we're comparing are unequal, we can */
/*                       still make a decision. */

				if (senses[0] == 0) {
				    s_copy(ch__1, cdat + (((i__2 = rj - 1) < 
					    250000 && 0 <= i__2 ? i__2 : 
					    s_rnge("cdat", i__2, "zzekjsrt_", 
					    (ftnlen)876)) << 5), (ftnlen)32, (
					    ftnlen)32);
				    s_copy(ch__2, cdat + (((i__3 = rjg - 1) < 
					    250000 && 0 <= i__3 ? i__3 : 
					    s_rnge("cdat", i__3, "zzekjsrt_", 
					    (ftnlen)876)) << 5), (ftnlen)32, (
					    ftnlen)32);
				    jle = nfj || ! (nfj || nfjg) && s_cmp(
					    ch__1, ch__2, (ftnlen)32, (ftnlen)
					    32) <= 0;
				} else {
				    s_copy(ch__1, cdat + (((i__2 = rj - 1) < 
					    250000 && 0 <= i__2 ? i__2 : 
					    s_rnge("cdat", i__2, "zzekjsrt_", 
					    (ftnlen)878)) << 5), (ftnlen)32, (
					    ftnlen)32);
				    s_copy(ch__2, cdat + (((i__3 = rjg - 1) < 
					    250000 && 0 <= i__3 ? i__3 : 
					    s_rnge("cdat", i__3, "zzekjsrt_", 
					    (ftnlen)878)) << 5), (ftnlen)32, (
					    ftnlen)32);
				    jle = nfjg || ! (nfj || nfjg) && s_cmp(
					    ch__1, ch__2, (ftnlen)32, (ftnlen)
					    32) >= 0;
				}
			    } else {

/*                       Otherwise, we'll have to look at values in the */
/*                       other order-by columns.  Get the segment and */
/*                       row vectors to be compared. */

				brute = TRUE_;
			    }
			}
		    }
		    if (brute) {
			zzekvcal_(&rj, &rwvbas, &sgvbas);
			i__2 = sgvbas + 1;
			i__3 = sgvbas + svsize;
			zzeksrd_(&i__2, &i__3, svecj);
			i__2 = rwvbas + 1;
			i__3 = rwvbas + rvsize;
			zzeksrd_(&i__2, &i__3, rvecj);
			zzekvcal_(&rjg, &rwvbas, &sgvbas);
			i__2 = sgvbas + 1;
			i__3 = sgvbas + svsize;
			zzeksrd_(&i__2, &i__3, svecjg);
			i__2 = rwvbas + 1;
			i__3 = rwvbas + rvsize;
			zzeksrd_(&i__2, &i__3, rvecjg);
			jle = zzekvcmp_(&c__4, norder, otabs, ocols, oelts, 
				senses, sthan, stsdsc, stdtpt, dtpool, dtdscs,
				 svecj, rvecj, svecjg, rvecjg);
		    }

/*                 At this point, JLE is set. */

		    if (jle) {
			j = 0;
		    } else {

/*                    Swap the Jth and JGth elements of the order vector. */

			swapi_(&ordvec[(i__2 = j - 1) < 250000 && 0 <= i__2 ? 
				i__2 : s_rnge("ordvec", i__2, "zzekjsrt_", (
				ftnlen)924)], &ordvec[(i__3 = jg - 1) < 
				250000 && 0 <= i__3 ? i__3 : s_rnge("ordvec", 
				i__3, "zzekjsrt_", (ftnlen)924)]);
		    }
		    j -= gap;
		}
	    }

/*           The following division guarantees loop termination, even */
/*           if a DAS error occurs. */

	    gap /= 2;
	}

/*        We've sorted the row numbers in Case 1.  Push the order vector */
/*        onto the scratch area stack. */

	zzekstop_(ordbas);
	zzekspsh_(&nrows, ordvec);
    } else {

/*        Case 2. */

/*        Well, we really have a lot of data.  Don't try to read it into */
/*        memory.  Build the order vector in the scratch area. */

	zzekstop_(ordbas);
	i__1 = nrows;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    zzekspsh_(&c__1, &i__);
	}

/*        Re-order the order vector elements to reflect the order of the */
/*        corresponding rows. This uses the Shell Sort algorithm, but */
/*        swaps the elements of the order vector instead of the rows */
/*        themselves. */

	gap = nrows / 2;
	while(gap > 0) {
	    i__1 = nrows;
	    for (i__ = gap + 1; i__ <= i__1; ++i__) {
		j = i__ - gap;
		while(j > 0) {
		    jg = j + gap;

/*                 Set the indices of the Jth and JGth */
/*                 row vectors, as indicated by the order vector. */

		    i__2 = *ordbas + j;
		    i__3 = *ordbas + j;
		    zzeksrd_(&i__2, &i__3, &rj);
		    i__2 = *ordbas + jg;
		    i__3 = *ordbas + jg;
		    zzeksrd_(&i__2, &i__3, &rjg);

/*                 Compare the two row vectors. */

		    zzekvcal_(&rj, &rwvbas, &sgvbas);
		    i__2 = sgvbas + 1;
		    i__3 = sgvbas + svsize;
		    zzeksrd_(&i__2, &i__3, svecj);
		    i__2 = rwvbas + 1;
		    i__3 = rwvbas + rvsize;
		    zzeksrd_(&i__2, &i__3, rvecj);
		    zzekvcal_(&rjg, &rwvbas, &sgvbas);
		    i__2 = sgvbas + 1;
		    i__3 = sgvbas + svsize;
		    zzeksrd_(&i__2, &i__3, svecjg);
		    i__2 = rwvbas + 1;
		    i__3 = rwvbas + rvsize;
		    zzeksrd_(&i__2, &i__3, rvecjg);
		    if (zzekvcmp_(&c__4, norder, otabs, ocols, oelts, senses, 
			    sthan, stsdsc, stdtpt, dtpool, dtdscs, svecj, 
			    rvecj, svecjg, rvecjg)) {
			j = 0;
		    } else {

/*                    Swap the order vector's Jth and JGth elements. */

			addrj = *ordbas + j;
			addrjg = *ordbas + jg;
			zzeksupd_(&addrj, &addrj, &rjg);
			zzeksupd_(&addrjg, &addrjg, &rj);
		    }
		    j -= gap;
		}
	    }

/*           The following division guarantees loop termination, even */
/*           if a DAS error occurs. */

	    gap /= 2;
	}

/*        We've sorted the row numbers for case (2). */

    }

/*     We've sorted the row numbers, no matter how many there were. */

    chkout_("ZZEKJSRT", (ftnlen)8);
    return 0;
} /* zzekjsrt_ */

