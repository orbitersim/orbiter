/* zzekvmch.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__24 = 24;
static integer c__11 = 11;

/* $Procedure      ZZEKVMCH ( EK, vector match ) */
logical zzekvmch_(integer *ncnstr, logical *active, integer *lhans, integer *
	lsdscs, integer *lcdscs, integer *lrows, integer *lelts, integer *ops,
	 integer *rhans, integer *rsdscs, integer *rcdscs, integer *rrows, 
	integer *relts)
{
    /* System generated locals */
    integer i__1, i__2, i__3, i__4, i__5, i__6, i__7, i__8;
    logical ret_val;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer);

    /* Local variables */
    char cval[1024*2];
    integer hans[2], elts[2];
    logical null[2];
    integer rows[2];
    extern integer zzekecmp_(integer *, integer *, integer *, integer *, 
	    integer *);
    integer i__, n;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    integer cvlen[2];
    logical found;
    extern /* Subroutine */ int movei_(integer *, integer *, integer *);
    extern logical matchi_(char *, char *, char *, char *, ftnlen, ftnlen, 
	    ftnlen, ftnlen);
    integer cldscs[22]	/* was [11][2] */, cmplen[2], sgdscs[48]	/* 
	    was [24][2] */;
    extern /* Subroutine */ int setmsg_(char *, ftnlen), errhan_(char *, 
	    integer *, ftnlen), errint_(char *, integer *, ftnlen), sigerr_(
	    char *, ftnlen), chkout_(char *, ftnlen);
    integer rel;
    extern /* Subroutine */ int zzekrsc_(integer *, integer *, integer *, 
	    integer *, integer *, integer *, char *, logical *, logical *, 
	    ftnlen);

/* $ Abstract */

/*     Determine whether a vector of constraints involving comparisons of */
/*     specified EK column elements is satisfied. */

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


/*     Include Section:  EK Template Matching Wild Characters */


/*        ekwild.inc  Version 1   16-JAN-1995 (NJB) */


/*     Within the EK system, templates used for pattern matching */
/*     are those accepted by the SPICELIB routine MATCHW.  MATCHW */
/*     accepts two special characters:  one representing wild */
/*     strings and one representing wild characters.  This include */
/*     file defines those special characters for use within the EK */
/*     system. */


/*     Wild string symbol:  this character matches any string. */


/*     Wild character symbol:  this character matches any character. */


/*     End Include Section:  EK Template Matching Wild Characters */

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
/*     NCNSTR     I   Number of join constraints. */
/*     ACTIVE     I   Array of flags indicating applicable constraints. */
/*     LHANS      I   Handles of EKs for columns on LHS's of constraints. */
/*     LSDSCS     I   Descriptors of segments on LHS's of constraints. */
/*     LCDSCS     I   Column descriptors for LHS's of constraints. */
/*     LROWS      I   Row numbers for LHS's of constraints. */
/*     LCOLS      I   Column names for LHS's of constraints. */
/*     LELTS      I   Column element indices for LHS's of constraints. */
/*     OPS        I   Code for relational operator in constraints. */
/*     RHAN       I   Handles of EKs for columns on RHS's of constraints. */
/*     RSDSCS     I   Descriptors of segments on RHS's of constraints. */
/*     RCDSCS     I   Column descriptors for RHS's of constraints. */
/*     RROWS      I   Row numbers for RHS's of constraints. */
/*     RCOLS      I   Column names for RHS's of constraints. */
/*     RELTS      I   Column element indices for RHS's of constraints. */

/*     The function returns .TRUE. if and only if all of the relational */
/*     constraints specified by the input arguments are satisfied. */

/* $ Detailed_Input */

/*     NCNSTR         is the number of input join constraints.   Each */
/*                    input constraint relates two EK column entries; */
/*                    abstractly, the form of the constraints is: */

/*                       <col entry 1> <relational op> <col entry 2> */

/*                    The compared entries are defined by handles, */
/*                    segment base addresses, column descriptors, and row */
/*                    numbers. */

/*     ACTIVE         is an array of logical flags indicating which */
/*                    constraints are currently applicable.  The Nth */
/*                    element of ACTIVE indicates whether or not to apply */
/*                    the Nth constraint:  if ACTIVE(N) is .TRUE., the */
/*                    constraint is applicable, otherwise it isn't. */

/*                    The elements of the other input arguments that */
/*                    define constraints are defined when the */
/*                    corresponding element of ACTIVE is .TRUE.  For */
/*                    example, when the second constraint is not active, */
/*                    the second column descriptor in LDSCRS may not be */
/*                    defined. */

/*     LHANS          is an array of EK file handles for the left-hand- */
/*                    sides of the constraints. */

/*     LSDSCS         is an array of segment descriptors for the */
/*                    left-hand-sides of the constraints. */

/*     LDSCRS         is an array of column descriptors for the */
/*                    left-hand-sides of the constraints. */

/*     LROWS          is an array of row numbers for the left-hand-sides */
/*                    of the constraints. */

/*     LELTS          is an array of column entry element indices for the */
/*                    left-hand-sides of the constraints.  These */
/*                    indices are ignored unless the columns they apply */
/*                    to are array-valued. */

/*     OPS            is an array of relational operators used in the */
/*                    input constraints.  The elements of OPS are any of */
/*                    the integer parameters */

/*                       EQ, GE, GT, LE, LT, NE, LIKE, ISNULL, NOTNUL */

/*                    The Ith element of OPS corresponds to the Ith */
/*                    constraint. */

/*     RHANS          is an array of EK file handles for the right-hand- */
/*                    sides of the constraints. */

/*     RSDSCS         is an array of segment descriptors for the */
/*                    right-hand-sides of the constraints. */

/*     RDSCRS         is an array of column descriptors for the */
/*                    right-hand-sides of the constraints. */

/*     RROWS          is an array of row numbers for the right-hand-sides */
/*                    of the constraints. */

/*     RELTS          is an array of column entry element indices for the */
/*                    right-hand-sides of the constraints.  These */
/*                    indices are ignored unless the columns they apply */
/*                    to are array-valued. */


/* $ Detailed_Output */

/*     The function returns .TRUE. if and only if all of the relational */
/*     constraints specified by the input arguments are satisfied. */

/* $ Parameters */

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
/*     and in addition includes the operator */

/*        LIKE */

/*     which is used to compare strings to a template.  In the character */
/*     case, the meanings of the parameters */

/*        GE,  GT,  LE,  LT */

/*     match those of the Fortran lexical functions */

/*        LGE, LGT, LLE, LLT */


/*     The additional unary operators */

/*        ISNULL, NOTNUL */

/*     are used to test whether a value of any type is null. */


/* $ Exceptions */

/*     1)  If any of the input file handles is invalid, the error */
/*         will be diagnosed by routines called by this routine. */
/*         The function value is .FALSE. in this case. */

/*     2)  If an I/O error occurs while attempting to find the address */
/*         range of a column entry element, the error will */
/*         be diagnosed by routines called by this routine.  The */
/*         function value is .FALSE. in this case. */

/*     3)  If any of the input segment descriptors, column descriptors, */
/*         or row numbers are invalid, this routine may fail in */
/*         unpredictable, but possibly spectacular, ways.  Except */
/*         as described in this header section, no attempt is made to */
/*         handle these errors. */

/*     4)  If the data type code in an input column descriptor is not */
/*         recognized, the error SPICE(INVALIDDATATYPE) is signaled. */
/*         The function value is .FALSE. in this case. */

/*     5)  If a relational operator code is not recognized, the */
/*         error SPICE(UNNATURALRELATION) is signaled. */
/*         The function value is .FALSE. in this case. */

/* $ Files */

/*     See the descriptions of the arguments LHAN and RHAN in */
/*     $Detailed_Input. */

/* $ Particulars */

/*     This routine is an EK utility intended to centralize a frequently */
/*     performed comparison operation. */

/* $ Examples */

/*     See EKSRCH. */

/* $ Restrictions */

/*     1)  This routine must execute quickly.  Therefore, it checks in */
/*         only if it detects an error.  If an error is signaled by a */
/*         routine called by this routine, this routine will not appear */
/*         in the SPICELIB traceback display.  Also, in the interest */
/*         of speed, this routine does not test the value of the SPICELIB */
/*         function RETURN upon entry. */

/*     2)  This routine depends on the requested comparison to have */
/*         been semantically checked. Semantically invalid comparisons */
/*         are treated as bugs. */

/*     3)  Only the first MAXSTR characters of character strings are */
/*         used in comparisons. */
/* C */
/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman   (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.2.0, 05-FEB-2015 (NJB) */

/*        Updated to use ERRHAN. */

/* -    SPICELIB Version 1.1.0, 01-JUN-2010 (NJB) */

/*        Bug fix: subscript out of range error caused by */
/*        column entry strings longer than MAXLEN has been */
/*        corrected. Also updated Restrictions header section. */

/* -    Beta Version 1.0.0, 11-OCT-1995 (NJB) */

/* -& */

/*     SPICELIB functions */


/*     Non-SPICELIB functions */


/*     Local variables */


/*     Use discovery check-in for speed.  Don't check RETURN. */

/*     The function value defaults to .TRUE.  As we test the constraints, */
/*     we may find one that the input row vector doesn't satisfy, at */
/*     which point we can terminate the comparison. */

    ret_val = TRUE_;
    n = 1;
    while(n <= *ncnstr && ret_val) {
	if (active[n - 1]) {

/*           Apply the Nth join constraint to the input row vector. */

/*           Compare the entries in the two rows in the columns indicated */
/*           by the Nth column descriptor pair.  To do this, find the */
/*           address ranges for each column entry.  We don't check the */
/*           found flag because every column entry has at least one */
/*           element. */


/*           We'll start out setting REL to EQ.  If we find out */
/*           otherwise, we'll change it. */

	    hans[0] = lhans[n - 1];
	    hans[1] = rhans[n - 1];
	    movei_(&lsdscs[n * 24 - 24], &c__24, sgdscs);
	    movei_(&rsdscs[n * 24 - 24], &c__24, &sgdscs[24]);
	    rows[0] = lrows[n - 1];
	    rows[1] = rrows[n - 1];
	    elts[0] = lelts[n - 1];
	    elts[1] = relts[n - 1];
	    movei_(&lcdscs[n * 11 - 11], &c__11, cldscs);
	    movei_(&rcdscs[n * 11 - 11], &c__11, &cldscs[11]);
	    rel = zzekecmp_(hans, sgdscs, cldscs, rows, elts);

/*           Determine the truth of the Nth input relational expression, */
/*           and set ZZEKVMCH accordingly. */

	    if (ops[n - 1] == 1) {
		ret_val = rel == 1;
	    } else if (ops[n - 1] == 5) {
		ret_val = rel == 5;
	    } else if (ops[n - 1] == 4) {
		ret_val = rel != 3;
	    } else if (ops[n - 1] == 3) {
		ret_val = rel == 3;
	    } else if (ops[n - 1] == 2) {
		ret_val = rel != 5;
	    } else if (ops[n - 1] == 6) {
		ret_val = rel != 1;
	    } else if (ops[n - 1] == 7 && cldscs[1] == 1) {
		for (i__ = 1; i__ <= 2; ++i__) {
		    zzekrsc_(&hans[(i__1 = i__ - 1) < 2 && 0 <= i__1 ? i__1 : 
			    s_rnge("hans", i__1, "zzekvmch_", (ftnlen)402)], &
			    sgdscs[(i__2 = i__ * 24 - 24) < 48 && 0 <= i__2 ? 
			    i__2 : s_rnge("sgdscs", i__2, "zzekvmch_", (
			    ftnlen)402)], &cldscs[(i__3 = i__ * 11 - 11) < 22 
			    && 0 <= i__3 ? i__3 : s_rnge("cldscs", i__3, 
			    "zzekvmch_", (ftnlen)402)], &rows[(i__4 = i__ - 1)
			     < 2 && 0 <= i__4 ? i__4 : s_rnge("rows", i__4, 
			    "zzekvmch_", (ftnlen)402)], &elts[(i__5 = i__ - 1)
			     < 2 && 0 <= i__5 ? i__5 : s_rnge("elts", i__5, 
			    "zzekvmch_", (ftnlen)402)], &cvlen[(i__6 = i__ - 
			    1) < 2 && 0 <= i__6 ? i__6 : s_rnge("cvlen", i__6,
			     "zzekvmch_", (ftnlen)402)], cval + (((i__7 = i__ 
			    - 1) < 2 && 0 <= i__7 ? i__7 : s_rnge("cval", 
			    i__7, "zzekvmch_", (ftnlen)402)) << 10), &null[(
			    i__8 = i__ - 1) < 2 && 0 <= i__8 ? i__8 : s_rnge(
			    "null", i__8, "zzekvmch_", (ftnlen)402)], &found, 
			    (ftnlen)1024);
		    if (! found) {
			chkin_("ZZEKVMCH", (ftnlen)8);
			setmsg_("EK = #; COLIDX = #; ROW = #; ELTIDX  = #.  "
				"Column entry  element was not found.", (
				ftnlen)79);
			errhan_("#", &hans[(i__1 = i__ - 1) < 2 && 0 <= i__1 ?
				 i__1 : s_rnge("hans", i__1, "zzekvmch_", (
				ftnlen)419)], (ftnlen)1);
			errint_("#", &cldscs[(i__1 = i__ * 11 - 3) < 22 && 0 
				<= i__1 ? i__1 : s_rnge("cldscs", i__1, "zze"
				"kvmch_", (ftnlen)420)], (ftnlen)1);
			errint_("#", &rows[(i__1 = i__ - 1) < 2 && 0 <= i__1 ?
				 i__1 : s_rnge("rows", i__1, "zzekvmch_", (
				ftnlen)421)], (ftnlen)1);
			errint_("#", &elts[(i__1 = i__ - 1) < 2 && 0 <= i__1 ?
				 i__1 : s_rnge("elts", i__1, "zzekvmch_", (
				ftnlen)422)], (ftnlen)1);
			sigerr_("SPICE(INVALIDINDEX)", (ftnlen)19);
			chkout_("ZZEKVMCH", (ftnlen)8);
			return ret_val;
		    }
		    if (found && ! null[(i__1 = i__ - 1) < 2 && 0 <= i__1 ? 
			    i__1 : s_rnge("null", i__1, "zzekvmch_", (ftnlen)
			    429)]) {
/* Computing MIN */
			i__3 = cvlen[(i__2 = i__ - 1) < 2 && 0 <= i__2 ? i__2 
				: s_rnge("cvlen", i__2, "zzekvmch_", (ftnlen)
				431)];
			cmplen[(i__1 = i__ - 1) < 2 && 0 <= i__1 ? i__1 : 
				s_rnge("cmplen", i__1, "zzekvmch_", (ftnlen)
				431)] = min(i__3,1024);
		    } else {
			cmplen[(i__1 = i__ - 1) < 2 && 0 <= i__1 ? i__1 : 
				s_rnge("cmplen", i__1, "zzekvmch_", (ftnlen)
				433)] = 0;
		    }
		}
		ret_val = matchi_(cval, cval + 1024, "*", "%", cmplen[0], 
			cmplen[1], (ftnlen)1, (ftnlen)1);
	    } else if (ops[n - 1] == 8 && cldscs[1] == 1) {
		for (i__ = 1; i__ <= 2; ++i__) {
		    zzekrsc_(&hans[(i__1 = i__ - 1) < 2 && 0 <= i__1 ? i__1 : 
			    s_rnge("hans", i__1, "zzekvmch_", (ftnlen)451)], &
			    sgdscs[(i__2 = i__ * 24 - 24) < 48 && 0 <= i__2 ? 
			    i__2 : s_rnge("sgdscs", i__2, "zzekvmch_", (
			    ftnlen)451)], &cldscs[(i__3 = i__ * 11 - 11) < 22 
			    && 0 <= i__3 ? i__3 : s_rnge("cldscs", i__3, 
			    "zzekvmch_", (ftnlen)451)], &rows[(i__4 = i__ - 1)
			     < 2 && 0 <= i__4 ? i__4 : s_rnge("rows", i__4, 
			    "zzekvmch_", (ftnlen)451)], &elts[(i__5 = i__ - 1)
			     < 2 && 0 <= i__5 ? i__5 : s_rnge("elts", i__5, 
			    "zzekvmch_", (ftnlen)451)], &cvlen[(i__6 = i__ - 
			    1) < 2 && 0 <= i__6 ? i__6 : s_rnge("cvlen", i__6,
			     "zzekvmch_", (ftnlen)451)], cval + (((i__7 = i__ 
			    - 1) < 2 && 0 <= i__7 ? i__7 : s_rnge("cval", 
			    i__7, "zzekvmch_", (ftnlen)451)) << 10), &null[(
			    i__8 = i__ - 1) < 2 && 0 <= i__8 ? i__8 : s_rnge(
			    "null", i__8, "zzekvmch_", (ftnlen)451)], &found, 
			    (ftnlen)1024);
		    if (! found) {
			chkin_("ZZEKVMCH", (ftnlen)8);
			setmsg_("EK = #; COLIDX = #; ROW = #; ELTIDX  = #.  "
				"Column entry  element was not found.", (
				ftnlen)79);
			errhan_("#", &hans[(i__1 = i__ - 1) < 2 && 0 <= i__1 ?
				 i__1 : s_rnge("hans", i__1, "zzekvmch_", (
				ftnlen)468)], (ftnlen)1);
			errint_("#", &cldscs[(i__1 = i__ * 11 - 3) < 22 && 0 
				<= i__1 ? i__1 : s_rnge("cldscs", i__1, "zze"
				"kvmch_", (ftnlen)469)], (ftnlen)1);
			errint_("#", &rows[(i__1 = i__ - 1) < 2 && 0 <= i__1 ?
				 i__1 : s_rnge("rows", i__1, "zzekvmch_", (
				ftnlen)470)], (ftnlen)1);
			errint_("#", &elts[(i__1 = i__ - 1) < 2 && 0 <= i__1 ?
				 i__1 : s_rnge("elts", i__1, "zzekvmch_", (
				ftnlen)471)], (ftnlen)1);
			sigerr_("SPICE(INVALIDINDEX)", (ftnlen)19);
			chkout_("ZZEKVMCH", (ftnlen)8);
			return ret_val;
		    }
		    if (found && ! null[(i__1 = i__ - 1) < 2 && 0 <= i__1 ? 
			    i__1 : s_rnge("null", i__1, "zzekvmch_", (ftnlen)
			    479)]) {
/* Computing MIN */
			i__3 = cvlen[(i__2 = i__ - 1) < 2 && 0 <= i__2 ? i__2 
				: s_rnge("cvlen", i__2, "zzekvmch_", (ftnlen)
				481)];
			cmplen[(i__1 = i__ - 1) < 2 && 0 <= i__1 ? i__1 : 
				s_rnge("cmplen", i__1, "zzekvmch_", (ftnlen)
				481)] = min(i__3,1024);
		    } else {
			cmplen[(i__1 = i__ - 1) < 2 && 0 <= i__1 ? i__1 : 
				s_rnge("cmplen", i__1, "zzekvmch_", (ftnlen)
				483)] = 0;
		    }
		}
		ret_val = ! matchi_(cval, cval + 1024, "*", "%", cmplen[0], 
			cmplen[1], (ftnlen)1, (ftnlen)1);
	    } else {

/*              Sorry, we couldn't resist. */

		ret_val = FALSE_;
		chkin_("ZZEKVMCH", (ftnlen)8);
		setmsg_("The relational operator # was not recognized.", (
			ftnlen)45);
		errint_("#", &ops[n - 1], (ftnlen)1);
		sigerr_("SPICE(UNNATURALRELATION)", (ftnlen)24);
		chkout_("ZZEKVMCH", (ftnlen)8);
		return ret_val;
	    }
	}

/*        We've completed the test for the Nth constraint, if that */
/*        constraint was active. */

	++n;
    }
    return ret_val;
} /* zzekvmch_ */

