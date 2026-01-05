/* zzekkey.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1000 = 1000;
static integer c__11 = 11;

/* $Procedure  ZZEKKEY  ( EK, determine key column ) */
/* Subroutine */ int zzekkey_(integer *handle, integer *segdsc, integer *
	nrows, integer *ncnstr, integer *clidxs, integer *dsclst, integer *
	ops, integer *dtypes, char *chrbuf, integer *cbegs, integer *cends, 
	doublereal *dvals, integer *ivals, logical *active, integer *key, 
	integer *keydsc, integer *begidx, integer *endidx, logical *found, 
	ftnlen chrbuf_len)
{
    /* System generated locals */
    integer i__1, i__2;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer);

    /* Local variables */
    logical elim;
    extern integer ordi_(integer *, integer *);
    integer best;
    extern integer zzekille_(integer *, integer *, integer *, integer *, 
	    integer *, char *, doublereal *, integer *, ftnlen), zzekillt_(
	    integer *, integer *, integer *, integer *, integer *, char *, 
	    doublereal *, integer *, ftnlen);
    integer b, e, i__, j;
    extern integer cardi_(integer *);
    extern /* Subroutine */ int chkin_(char *, ftnlen), movei_(integer *, 
	    integer *, integer *);
    integer dtype;
    extern logical failed_(void);
    integer nmatch, conmap[1000];
    extern logical return_(void);
    integer eltidx, idxset[1006], lastle, lastlt, maxptr, minptr;
    logical indexd;
    extern /* Subroutine */ int setmsg_(char *, ftnlen), errint_(char *, 
	    integer *, ftnlen), sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), ssizei_(integer *, integer *), insrti_(integer *, 
	    integer *);
    logical fnd;
    integer col;

/* $ Abstract */

/*     Determine the key column to use when searching an EK segment */
/*     for rows matching query constraints. */

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
/*     HANDLE     I   Handle of EK file containing segment. */
/*     SEGDSC     I   Segment descriptor. */
/*     NROWS      I   Number of rows in segment. */
/*     NCNSTR     I   Number of relational constraints in query. */
/*     CLIDXS     I   Column attribute table indices for columns. */
/*     DSCLST     I   Array of column descriptors for constraints. */
/*     OPS        I   Operations used in query constraints. */
/*     DTYPES     I   Data types of scalar values used in constraints. */
/*     CHRBUF     I   Buffer containing query tokens. */
/*     CBEGS      I   Begin indices of character query tokens. */
/*     CENDS      I   End indices of character query tokens. */
/*     DVALS      I   D.p. values used in query constraints. */
/*     IVALS      I   Integer values used in query constraints. */
/*     ACTIVE    I-O  Array of flags indicating applicable constraints. */
/*     KEY        O   Index of key column. */
/*     KEYDSC     O   Descriptor of key column. */
/*     BEGIDX     O   Begin index of candidate row set. */
/*     ENDIDX     O   End index of candidate row set. */
/*     FOUND      O   Flag indicating whether a key column was found. */
/*     MAXCON     P   Maximum number of constraints allowed in query. */
/*     CDSCSZ     P   Column descriptor size. */

/* $ Detailed_Input */

/*     HANDLE         is the handle of the EK file containing the */
/*                    segment currently being searched for matching rows. */

/*     SEGDSC         the descriptor of the segment. */

/*     NROWS          is the number of rows in the segment designated by */
/*                    HANDLE and SEGDSC. */

/*     NCNSTR         is the number of input relational constraints. */
/*                    The input arrays CLIDXS, DSCLST, OPS, CHRBUF, */
/*                    CBEGS, CENDS, DVALS, and IVALS define these */
/*                    constraints.  Not all of the constraints may */
/*                    be applicable; the applicable constraints are */
/*                    identified by the input argument ACTIVE, which */
/*                    is described below.  Each *applicable* constraint */
/*                    has the form */

/*                       <table1.column1> <op> <value> */

/*     CLIDXS         is an array of column indices; the Nth index */
/*                    identifies the column on the left hand side of the */
/*                    Nth constraint.  Each index indicates the ordinal */
/*                    position of the attribute information for the */
/*                    corresponding column within the column attribute */
/*                    list for the column's parent table.  See the */
/*                    local variable declarations in EKBSR for further */
/*                    information on the column attribute list. */

/*     DSCLST         is an array of column descriptors for the columns */
/*                    referenced in the input constraints.  The Ith */
/*                    descriptor corresponds to the Ith constraint. */


/*     OPS            are relational operators used in the input */
/*                    constraints.  The elements of OPS are any of the */
/*                    integer parameters */

/*                       EQ, GE, GT, LE, LT, NE, LIKE, UNLIKE */

/*                    The Nth element of OPS corresponds to the Nth */
/*                    constraint. */

/*     DTYPES         is an array of data type codes for the values on */
/*                    the right hand sides of the input constraints. */
/*                    The Ith element of DTYPES applies to the Ith */
/*                    constraint. */

/*     CHRBUF, */
/*     CBEGS, */
/*     CENDS          are, respectively, a string containing character */
/*                    tokens representing values on the right hand sides */
/*                    of query constraints, and arrays of begin and end */
/*                    indices of these tokens within CHRBUF.  If the Nth */
/*                    constraint has a character value on the right hand */
/*                    side, that value is CHRBUF( CBEGS(N) : CENDS(N) ). */
/*                    For constraints whose right hand sides do not */
/*                    specify character values, the corresponding */
/*                    elements of CBEGS and CENDS are not used. */

/*     DVALS, */
/*     IVALS          are, respectively, arrays of double precision and */
/*                    integer values appearing on the right hand sides of */
/*                    input constraints.  The contents of DVALS and IVALS */
/*                    are meaningful only for those constraints whose */
/*                    right hand sides specify values having these data */
/*                    types. */

/*     ACTIVE         is an array of logical flags indicating which */
/*                    constraints are currently applicable.  The Nth */
/*                    element of ACTIVE indicates whether or not to apply */
/*                    the Nth constraint:  if ACTIVE(N) is .TRUE., the */
/*                    constraint is applicable, otherwise it isn't. */

/*                    The elements of the other input arguments that */
/*                    define constraints are defined when the */
/*                    corresponding element of ACTIVE is .TRUE.  For */
/*                    example, when the second constraint is not active, */
/*                    the second column descriptor in DSCLST may not be */
/*                    defined. */

/*                    Only constraints relating column entries to literal */
/*                    values may be active. */

/* $ Detailed_Output */

/*     ACTIVE         indicates, on output, which constraints are still */
/*                    active.  All constraints satisfied by the candidate */
/*                    row set are turned off on output. */

/*     KEY            is the index of the key column.  This index is */
/*                    taken from the input argument CLIDXS. */

/*     KEYDSC         is the column descriptor for the key column.  Note */
/*                    that this descriptor indicates whether the key */
/*                    column is indexed. */

/*     BEGIDX, */
/*     ENDIDX         are, respectively, begin and end indices for the */
/*                    candidate matching rows in the segment being */
/*                    searched.  These indices refer to positions in the */
/*                    key column's index:  the candidate rows are pointed */
/*                    to index elements having indices ranging from */
/*                    BEGIDX to ENDIDX, inclusively.  The actual */
/*                    candidate rows are referred to with one level of */
/*                    indirection. */

/*                    If the constraints on the key column entirely */
/*                    eliminate all rows in the segment, the returned */
/*                    values of BEGIDX and ENDIDX are, respectively, 1 */
/*                    and 0. */

/*     FOUND          is a logical flag indicating whether a key column */
/*                    was determined.  The other outputs of this routine */
/*                    are valid only if a key column was found.  This */
/*                    routine will fail to find a key column if there are */
/*                    no active constraints on indexed columns. */

/* $ Parameters */

/*     MAXCON         is the maximum number of constraints that may */
/*                    be used in a query. */

/*     CDSCSZ         is the size of a column descriptor. */

/* $ Exceptions */

/*     1)  If the segment contains no indexed columns on which there are */
/*         active constraints, the output argument FOUND is set to */
/*         .FALSE.  The other output arguments are undefined in this */
/*         case. */

/*     2)  If the constraints on the key column entirely eliminate all */
/*         rows in the segment, the returned values of BEGIDX and ENDIDX */
/*         are, respectively, 1 and 0. */

/*     3)  If the number of input constraints is out of range, the error */
/*         SPICE(INVALIDCOUNT) is signaled. */

/* $ Files */

/*     See the description of the input argument HANDLE. */

/* $ Particulars */

/*     The EKSRCH algorithm for finding rows matching a given set */
/*     of constraints attempts to use constraints on indexed columns */
/*     to enable the matching process to be performed efficiently. */
/*     The idea is to find the indexed column whose constraints limit */
/*     the possible set of matching rows to the smallest number; then */
/*     to linearly search through this set of candidate rows to see */
/*     which ones satisfy the remaining applicable constraints.  The */
/*     column used to initially limit the set of candidate rows is */
/*     called the `key column'.  The constraints on the key column that */
/*     are of interest are ones involving order relations or equality: */
/*     these constraints use the operators */

/*        EQ  GE  GT  LE  LT */

/*     Note that the NE operator is not of much use here. */

/*     The set of candidate rows simultaneously satisfies all such */
/*     constraints on the key column, and therefore is the intersection */
/*     of the set of rows satisfying each such constraint.  This method */
/*     of selecting candidate rows can rapidly eliminate large numbers of */
/*     rows from consideration, because the index on the key column can */
/*     be employed in finding rows that match constraints involving order */
/*     relations:  the start and end indices of such rows can be found */
/*     by a binary, rather than linear, search. */

/*     A segment may have multiple indexed columns on which there are */
/*     constraints involving order or equality relations; in this case */
/*     the column whose constraints are most restrictive is selected as */
/*     the key column. */

/*     It may also happen that a segment contains no indexed columns. */
/*     In such a case, the key column is not useful for narrowing the */
/*     set of candidate rows.  The first column of the segment is */
/*     arbitrarily selected as the key column in this case. */

/* $ Examples */

/*     See EKSRCH. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman   (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.2.1, 02-OCT-2021 (NJB) */

/*        Corrected typos in comments. */

/* -    SPICELIB Version 1.2.0, 26-JUL-1996 (NJB) */

/*        Added check of FAILED after calls to the EK search functions. */

/* -    SPICELIB Version 1.1.0, 17-APR-1996 (WLT) */

/*        Removed spurious periods that appeared at the */
/*        end of lines 524 and 577 in previous edition. */

/* -    Beta Version 1.0.0, 23-OCT-1995 (NJB) */


/* -& */
/* $ Revisions */

/* -    SPICELIB Version 1.2.0, 26-JUL-1996 (NJB) */

/*        Added check of FAILED after calls to the EK search functions. */
/* -& */

/*     SPICELIB functions */


/*     Other functions */


/*     Local parameters */


/*     Local variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("ZZEKKEY", (ftnlen)7);
    }

/*     There's no key column to begin with. */

    *found = FALSE_;
    if (*ncnstr < 0 || *ncnstr > 1000) {
	setmsg_("The number of constraints was #; valid range is 0:#", (
		ftnlen)51);
	errint_("#", ncnstr, (ftnlen)1);
	errint_("#", &c__1000, (ftnlen)1);
	sigerr_("SPICE(INVALIDCOUNT)", (ftnlen)19);
	chkout_("ZZEKKEY", (ftnlen)7);
	return 0;
    }

/*     Make a set out of the indices of indexed columns referenced */
/*     in active constraints.  Maintain a mapping from each column */
/*     to the index of some constraint that references that column. */

    ssizei_(&c__1000, idxset);
    i__1 = *ncnstr;
    for (i__ = 1; i__ <= i__1; ++i__) {
	if (active[i__ - 1]) {
	    indexd = dsclst[i__ * 11 - 6] != -1;
	    if (indexd) {
		insrti_(&clidxs[i__ - 1], idxset);
	    }
	}
    }
    i__1 = cardi_(idxset);
    for (i__ = 1; i__ <= i__1; ++i__) {
	fnd = FALSE_;
	j = 1;
	while(j <= *ncnstr && ! fnd) {
	    if (active[j - 1] && clidxs[j - 1] == idxset[(i__2 = i__ + 5) < 
		    1006 && 0 <= i__2 ? i__2 : s_rnge("idxset", i__2, "zzekk"
		    "ey_", (ftnlen)431)]) {
		fnd = TRUE_;
		conmap[(i__2 = i__ - 1) < 1000 && 0 <= i__2 ? i__2 : s_rnge(
			"conmap", i__2, "zzekkey_", (ftnlen)433)] = j;
	    } else {
		++j;
	    }
	}
    }

/*     We finish up now if there are no indexed columns */
/*     on which there are active constraints. */

    if (cardi_(idxset) == 0) {
	chkout_("ZZEKKEY", (ftnlen)7);
	return 0;
    }

/*     For each column in the `indexed' set, find out how many */
/*     candidate rows we'd have if we picked that column as the key */
/*     column.  If we find that the constraints on some column eliminate */
/*     all matching rows, we can stop. */

    *begidx = 1;
    *endidx = *nrows;
    best = idxset[6];
    nmatch = *nrows;
    elim = FALSE_;
    eltidx = 1;
    while(eltidx <= cardi_(idxset) && ! elim) {

/*        Get the attribute list pointer for the current column. */

	col = idxset[(i__1 = eltidx + 5) < 1006 && 0 <= i__1 ? i__1 : s_rnge(
		"idxset", i__1, "zzekkey_", (ftnlen)472)];

/*        Set the initial values of MINPTR, MAXPTR, and NMATCH */

	minptr = 1;
	maxptr = *nrows;
	i__ = 1;
	while(i__ <= *ncnstr && ! elim) {

/*           For each constraint, increase MINPTR or decrease MAXPTR */
/*           if the constraint allows us to do so. */

	    if (clidxs[i__ - 1] == col && active[i__ - 1]) {

/*              The Ith constraint is active and applies to this column. */

/*              If the column has character type, set the bounds of the */
/*              token on the right hand side of the constraint. */
/*              Otherwise, set the bounds to default valid values to */
/*              avoid subscript bounds errors. */

		dtype = dsclst[i__ * 11 - 10];
		if (dtype == 1) {
		    b = cbegs[i__ - 1];
		    e = cends[i__ - 1];
		} else {
		    b = 1;
		    e = 1;
		}

/*              At this point, MINPTR and MAXPTR are in the range */
/*              1:NROWS, and MINPTR is less than or equal to MAXPTR. */

		if (ops[i__ - 1] == 5) {

/*                 Find the index of the pointer to the last row */
/*                 whose value in this column is less than the */
/*                 value cited in the Ith constraint. */

		    lastlt = zzekillt_(handle, segdsc, &dsclst[i__ * 11 - 11],
			     nrows, &dtypes[i__ - 1], chrbuf + (b - 1), &
			    dvals[i__ - 1], &ivals[i__ - 1], e - (b - 1));

/*                 If all column elements were greater than or equal */
/*                 to the specified value, MAXPTR will be set to zero. */

		    maxptr = min(lastlt,maxptr);
		    elim = maxptr == 0;
		} else if (ops[i__ - 1] == 4) {

/*                 Find the index of the pointer to the last row */
/*                 whose value in this column is less or equal to */
/*                 the value cited in the Ith constraint. */

		    lastle = zzekille_(handle, segdsc, &dsclst[i__ * 11 - 11],
			     nrows, &dtypes[i__ - 1], chrbuf + (b - 1), &
			    dvals[i__ - 1], &ivals[i__ - 1], e - (b - 1));
		    maxptr = min(lastle,maxptr);
		    elim = maxptr == 0;
		} else if (ops[i__ - 1] == 1) {

/*                 Find both the pointer to the last row whose */
/*                 value in this column is less than the value cited in */
/*                 the Ith constraint, and the pointer to the last row */
/*                 whose value in this column is less than or equal to */
/*                 the value cited in the Ith constraint.  The */
/*                 successor of the former pointer, together with */
/*                 the latter pointer, bound the range of pointers */
/*                 to possible matching rows. */

		    lastlt = zzekillt_(handle, segdsc, &dsclst[i__ * 11 - 11],
			     nrows, &dtypes[i__ - 1], chrbuf + (b - 1), &
			    dvals[i__ - 1], &ivals[i__ - 1], e - (b - 1));
		    lastle = zzekille_(handle, segdsc, &dsclst[i__ * 11 - 11],
			     nrows, &dtypes[i__ - 1], chrbuf + (b - 1), &
			    dvals[i__ - 1], &ivals[i__ - 1], e - (b - 1));
		    if (lastlt < lastle) {

/*                    There is at least one row whose value in the */
/*                    current column matches the value cited in the Ith */
/*                    constraint, and LASTLE is the index of the pointer */
/*                    to the last such row.  The successor of LASTLT is */
/*                    the first pointer to such a row (even if LASTLT is */
/*                    zero). */

/* Computing MAX */
			i__1 = lastlt + 1;
			minptr = max(i__1,minptr);
			maxptr = min(lastle,maxptr);
		    } else {

/*                    No rows match this constraint. */

			elim = TRUE_;
		    }
		} else if (ops[i__ - 1] == 3) {

/*                 Find the index of the pointer to the last row */
/*                 whose value in this column is less or equal to */
/*                 the value cited in the Ith constraint.  The index of */
/*                 the pointer to the first row satisfying all of the */
/*                 constraints on this column is the successor of */
/*                 this pointer or a greater pointer. */

		    lastle = zzekille_(handle, segdsc, &dsclst[i__ * 11 - 11],
			     nrows, &dtypes[i__ - 1], chrbuf + (b - 1), &
			    dvals[i__ - 1], &ivals[i__ - 1], e - (b - 1));
/* Computing MAX */
		    i__1 = lastle + 1;
		    minptr = max(i__1,minptr);
		    elim = lastle == *nrows;
		} else if (ops[i__ - 1] == 2) {

/*                 Find the index of the pointer to the last row */
/*                 whose value in this column is less than the */
/*                 value cited in the Ith constraint.  The index of the */
/*                 pointer to the first row satisfying all of the */
/*                 constraints on this column is the successor of */
/*                 this pointer or a greater pointer. */

		    lastlt = zzekillt_(handle, segdsc, &dsclst[i__ * 11 - 11],
			     nrows, &dtypes[i__ - 1], chrbuf + (b - 1), &
			    dvals[i__ - 1], &ivals[i__ - 1], e - (b - 1));
/* Computing MAX */
		    i__1 = lastlt + 1;
		    minptr = max(i__1,minptr);
		    elim = lastlt == *nrows;
		}

/*              We've checked the Ith constraint to see whether */
/*              it applied to the current column, and if it did, */
/*              we adjusted MINPTR and MAXPTR to reflect the */
/*              constraint. */

	    }

/*           We've applied the Ith constraint, if it was active. */

	    if (minptr > maxptr) {
		elim = TRUE_;
	    }
	    if (! elim) {
		++i__;
	    }
	    if (failed_()) {
		chkout_("ZZEKKEY", (ftnlen)7);
		return 0;
	    }
	}

/*        We've applied all of active, applicable constraints to column */
/*        COL.  If these constraints did not eliminate the current */
/*        segment entirely, save the number of candidate rows we'd have */
/*        if we kept this column as the key column. */

	if (! elim) {
	    nmatch = maxptr - minptr + 1;
	    if (nmatch < *endidx - *begidx + 1) {

/*              This is our new key column, until a better one comes */
/*              along. */

		best = col;
		*begidx = minptr;
		*endidx = maxptr;
	    }
	    ++eltidx;
	}
    }
    if (elim) {

/*        If the segment was eliminated by constraints on the last column */
/*        we looked at, set BEGIDX and ENDIDX to indicate the absence of */
/*        matching rows. */

	*key = col;
	*begidx = 1;
	*endidx = 0;
    } else {

/*        BEST, BEGIDX, and ENDIDX are set to reflect the key column. */
/*        Set KEY and grab the descriptor of the key column. */

	*key = best;
    }
    i__ = conmap[(i__1 = ordi_(key, idxset) - 1) < 1000 && 0 <= i__1 ? i__1 : 
	    s_rnge("conmap", i__1, "zzekkey_", (ftnlen)698)];
    movei_(&dsclst[i__ * 11 - 11], &c__11, keydsc);

/*     De-activate constraints on the key column that we've already */
/*     applied. */

    i__1 = *ncnstr;
    for (i__ = 1; i__ <= i__1; ++i__) {
	if (active[i__ - 1] && clidxs[i__ - 1] == *key) {
	    if (ops[i__ - 1] == 5 || ops[i__ - 1] == 4 || ops[i__ - 1] == 1 ||
		     ops[i__ - 1] == 2 || ops[i__ - 1] == 3) {

/*              This constraint is met by the candidate rows; we can */
/*              turn it off. */

		active[i__ - 1] = FALSE_;
	    }
	}
    }

/*     At this point, we've found a key column. */

    *found = TRUE_;
    chkout_("ZZEKKEY", (ftnlen)7);
    return 0;
} /* zzekkey_ */

