/* zzekjoin.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__100 = 100;
static integer c__9 = 9;
static integer c__10 = 10;
static integer c__1 = 1;
static integer c__0 = 0;

/* $Procedure  ZZEKJOIN  ( Perform join on two join row sets ) */
/* Subroutine */ int zzekjoin_(integer *jbase1, integer *jbase2, integer *
	njcnst, logical *active, integer *cpidx1, integer *clidx1, integer *
	elts1, integer *ops, integer *cpidx2, integer *clidx2, integer *elts2,
	 integer *sthan, integer *stsdsc, integer *stdtpt, integer *dtpool, 
	integer *dtdscs, integer *jbase3, integer *nrows)
{
    /* System generated locals */
    integer i__1, i__2, i__3, i__4, i__5;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer);

    /* Local variables */
    extern /* Subroutine */ int zzeksupd_(integer *, integer *, integer *), 
	    zzekjprp_(integer *, integer *, integer *, integer *, integer *, 
	    integer *, integer *, integer *, integer *, integer *, logical *, 
	    integer *, integer *, integer *, integer *, integer *, integer *, 
	    integer *, integer *, integer *, integer *, integer *, integer *),
	     zzekspsh_(integer *, integer *), zzekjnxt_(logical *, integer *),
	     zzekstop_(integer *);
    integer i__;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    logical found;
    integer nresv, s1, s2, s3, segvec[10], offset, nr1, nr2, nr3, nt1, nt2, 
	    nt3, rb1, rb2, rb3, rowvec[11], sgvbas;
    extern /* Subroutine */ int setmsg_(char *, ftnlen), errint_(char *, 
	    integer *, ftnlen), sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen);
    integer top;
    extern /* Subroutine */ int zzeksrd_(integer *, integer *, integer *);
    integer nsv1, nsv2, nsv3;

/* $ Abstract */

/*     Perform join of two EK join row sets, subject to a specified set */
/*     of EK join constraints, yielding an EK join row set. */

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

/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     JBASE1     I   Scratch area base address of first join row set. */
/*     JBASE2     I   Scratch area base address of second join row set. */
/*     NJCNST     I   Number of join constraints. */
/*     ACTIVE     I   Array of flags indicating applicable constraints. */
/*     CPIDX1     I   Cross product indices for LHS's of constraints. */
/*     CLIDX1     I   Column indices for LHS's of constraints. */
/*     ELTS1      I   Column entry elt. indices for LHS'of constraints. */
/*     OPS        I   Operator codes for constraints. */
/*     CPIDX2     I   Cross product indices for RHS's of constraints. */
/*     CLIDX2     I   Column indices for RHS's of constraints. */
/*     ELTS2      I   Column entry elt. indices for RHS'of constraints. */
/*     STHAN      I   Array of EK handles corresponding to segments. */
/*     STSDSC     I   Array of segment descriptors. */
/*     STDTPT     I   Array of set table column descriptor pointers. */
/*     DTPOOL     I   Linked list pool for column descriptors. */
/*     DTDSCS     I   Array of column descriptors. */
/*     JBASE3     O   Scratch area base address of output join row set. */
/*     NROWS      O   Number of rows in output join row set. */
/*     CDSCSZ     P   Size of column descriptor. */

/* $ Detailed_Input */

/*     JBASE1         is the EK scratch area base address of the first */
/*                    input join row set.  This address is one less than */
/*                    the first address occupied by the join row set. */
/*                    See the $Particulars section for a description of */
/*                    join row sets. */

/*     JBASE2         is the EK scratch area base address of the second */
/*                    input join row set.  This address is one less than */
/*                    the first address occupied by the join row set. */

/*     NJCNST         is the number of join constraints that must be */
/*                    satisfied by the output join row set.  Each of the */
/*                    input arrays CPIDX1, CLIDX1, OPS, CPIDX2, and */
/*                    CLIDX2 contains NJCNST elements. */

/*     ACTIVE         is an array of logical flags indicating which */
/*                    constraints are currently applicable.  The Nth */
/*                    element of ACTIVE indicates whether or not to apply */
/*                    the Nth constraint:  if ACTIVE(N) is .TRUE., the */
/*                    constraint is applicable, otherwise it isn't. */

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

/*     ELTS1          is an array of column entry element indices.  These */
/*                    indices specify the elements of the LHS column */
/*                    entries to be used in testing the join constraints. */
/*                    For scalar columns, the corresponding values of */
/*                    ELTS1 are ignored. */

/*     OPS            is an array of relational operator codes.  The */
/*                    Ith code applies to the Ith join constraint. */

/*     CPIDX2, */
/*     CLIDX2         are, respectively, a set of cross product indices */
/*                    and column indices that define the columns on the */
/*                    right-hand sides of the input constraints.  The */
/*                    meanings of these arrays are analogous to those */
/*                    of CPIDX1 and CLIDX1. */

/*     ELTS2          is an array of column entry element indices.  These */
/*                    indices specify the elements of the LHS column */
/*                    entries to be used in testing the join constraints. */
/*                    For scalar columns, the corresponding values of */
/*                    ELTS2 are ignored. */

/*     STHAN          is an array of EK file handles.  The Ith element */
/*                    of STHAN is the handle of the EK containing the */
/*                    Ith loaded segment. */

/*     STSDSC         is an array of segment descriptors for all of the */
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

/*     JBASE3         is the EK scratch area base address of the output */
/*                    join row set.  This join row set represents that */
/*                    subset of the Cartesian product of the input */
/*                    join row sets which satisfies all of the input */
/*                    join constraints. */

/*     NROWS          is the number of `rows' in the output join row set. */
/*                    Each such row is actually a vector of rows, one */
/*                    belonging to each table in the Cartesian product */
/*                    of tables specified by the join operation. */

/* $ Parameters */

/*     See the include files. */

/* $ Exceptions */

/*     1)  If the number of constraints NCNSTR is out of range, the */
/*         error SPICE(INVALIDCOUNT) is signaled. */

/*     2)  If the table count in either input join row set is out of */
/*         range, the error SPICE(INVALIDCOUNT) is signaled. */

/*     3)  If the sum of the table counts of the input join row sets is */
/*         too large, the error SPICE(INVALIDCOUNT) is signaled. */

/*     4)  If either of cross product table indices for the input */
/*         constraints is out of range, the error SPICE(INVALIDINDEX) is */
/*         signaled. */

/* $ Files */

/*     1)  This routine uses the EK scratch area, which employs a scratch */
/*         DAS file. */

/* $ Particulars */

/*     The purpose of this routine is to compute the set of rows */
/*     resulting from joining two `join row sets'.  A join row set */
/*     is a structure in the EK scratch area that represents the */
/*     result of a table join, subject to constraints.  A join of */
/*     n tables, subject to constraints, may be computed by joining */
/*     the join of the first n-1 tables with the nth table; such a */
/*     procedure is the typical application envisioned for this routine. */

/*     Since all EK rows belong to segments, the set of rows formed by */
/*     taking the Cartesian product of two tables is actually the union */
/*     of the sets of rows belonging to the Cartesian products of the */
/*     possible pairs of segments, where the segments are taken from */
/*     the two tables being crossed.  Therefore, each join row set is */
/*     characterized by a list of n-tuples of segments, and by a list of */
/*     sets of n-tuples of row numbers, one row number set per segment */
/*     n-tuple.  The segments are identified by a vector of segment */
/*     list indices, which is called a `segment vector'.  The n-tuples */
/*     of rows are called `row vectors'.  Each segment vector has a */
/*     pointer and count that allow addressing the corresponding row */
/*     vectors. */

/*     Each join row set consists of: */

/*         - a base address in the scratch area */
/*         - a table count */
/*         - a segment vector count */
/*         - a set of segment vectors */
/*         - a set of segment vector row vector base addresses */
/*           (these are relative to the base of the join row set) */
/*         - a set of segment vector row vector counts */
/*         - a set of row vectors, augmented by offsets of their */
/*           parent segment vectors (these offsets are at the */
/*           end of each row vector) */


/*     The layout of a join row set in the EK scratch area is shown */
/*     in the include file for the join row set parameters. */

/* $ Examples */

/*     See EKSRCH. */

/* $ Restrictions */

/*     1)  Relies on the EK scratch area. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman   (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.2, 02-OCT-2021 (NJB) */

/*        Corrected typos in comments. */

/* -    SPICELIB Version 1.0.1, 20-JUL-1998 (NJB) */

/*        Deleted comment about squeezing out segment vectors without */
/*        corresponding row vectors; also deleted comment containing */
/*        a call to ZZEKJSQZ. */

/* -    Beta Version 1.0.0, 10-OCT-1995 (NJB) */

/* -& */

/*     Local variables */


/*     For speed, we use discovery check-in.  We don't check */
/*     RETURN at all. */


/*     Validate constraint count. */

    if (*njcnst < 0 || *njcnst > 100) {
	chkin_("ZZEKJOIN", (ftnlen)8);
	setmsg_("Number of join constraints was #; valid range is 0:#", (
		ftnlen)52);
	errint_("#", njcnst, (ftnlen)1);
	errint_("#", &c__100, (ftnlen)1);
	sigerr_("SPICE(INVALIDCOUNT)", (ftnlen)19);
	chkout_("ZZEKJOIN", (ftnlen)8);
	return 0;
    }

/*     Get the table count and segment vector count for each input join */
/*     row set. */

    i__1 = *jbase1 + 3;
    i__2 = *jbase1 + 3;
    zzeksrd_(&i__1, &i__2, &nt1);
    i__1 = *jbase1 + 4;
    i__2 = *jbase1 + 4;
    zzeksrd_(&i__1, &i__2, &nsv1);
    i__1 = *jbase2 + 3;
    i__2 = *jbase2 + 3;
    zzeksrd_(&i__1, &i__2, &nt2);
    i__1 = *jbase2 + 4;
    i__2 = *jbase2 + 4;
    zzeksrd_(&i__1, &i__2, &nsv2);

/*     Set the table count and segment vector count for the output join */
/*     row set. */

    nt3 = nt1 + nt2;
    nsv3 = nsv1 * nsv2;
    if (nt1 < 1 || nt2 > 9) {
	chkin_("ZZEKJOIN", (ftnlen)8);
	setmsg_("Number tables in first join row set was #; valid range is 1"
		":#", (ftnlen)61);
	errint_("#", &nt1, (ftnlen)1);
	errint_("#", &c__9, (ftnlen)1);
	sigerr_("SPICE(INVALIDCOUNT)", (ftnlen)19);
	chkout_("ZZEKJOIN", (ftnlen)8);
	return 0;
    } else if (nt2 < 1 || nt2 > 9) {
	chkin_("ZZEKJOIN", (ftnlen)8);
	setmsg_("Number tables in second join row set was #; valid range is "
		"1:#", (ftnlen)62);
	errint_("#", &nt2, (ftnlen)1);
	errint_("#", &c__9, (ftnlen)1);
	sigerr_("SPICE(INVALIDCOUNT)", (ftnlen)19);
	chkout_("ZZEKJOIN", (ftnlen)8);
	return 0;
    } else if (nt3 > 10) {
	chkin_("ZZEKJOIN", (ftnlen)8);
	setmsg_("Number of crossed tables was #; valid range is 0:#", (ftnlen)
		50);
	errint_("#", &nt3, (ftnlen)1);
	errint_("#", &c__10, (ftnlen)1);
	sigerr_("SPICE(INVALIDCOUNT)", (ftnlen)19);
	chkout_("ZZEKJOIN", (ftnlen)8);
	return 0;
    }

/*     Validate cross product indices.  The column indices don't lend */
/*     themselves to such a convenient check; we'll check those as we */
/*     use them. */

    i__1 = *njcnst;
    for (i__ = 1; i__ <= i__1; ++i__) {
	if (active[i__ - 1]) {
	    if (cpidx1[i__ - 1] < 1 || cpidx1[i__ - 1] > nt3) {
		chkin_("ZZEKJOIN", (ftnlen)8);
		setmsg_("Cross product table index for left hand side of con"
			"straint # was #; valid range is 1:#", (ftnlen)86);
		errint_("#", &i__, (ftnlen)1);
		errint_("#", &cpidx1[i__ - 1], (ftnlen)1);
		errint_("#", &nt3, (ftnlen)1);
		sigerr_("SPICE(INVALIDINDEX)", (ftnlen)19);
		chkout_("ZZEKJOIN", (ftnlen)8);
		return 0;
	    } else if (cpidx2[i__ - 1] < 1 || cpidx2[i__ - 1] > nt3) {
		chkin_("ZZEKJOIN", (ftnlen)8);
		setmsg_("Cross product table index for right hand side of co"
			"nstraint # was #; valid range is 1:#", (ftnlen)87);
		errint_("#", &i__, (ftnlen)1);
		errint_("#", &cpidx2[i__ - 1], (ftnlen)1);
		errint_("#", &nt3, (ftnlen)1);
		sigerr_("SPICE(INVALIDINDEX)", (ftnlen)19);
		chkout_("ZZEKJOIN", (ftnlen)8);
		return 0;
	    }
	}
    }

/*     Form the joint row set control area for output join row set. */

/*     The current stack top is the base address of the output join row */
/*     set. */

    zzekstop_(jbase3);

/*     Save room for the size and row vector count */

    for (i__ = 1; i__ <= 2; ++i__) {
	zzekspsh_(&c__1, &c__0);
    }

/*     The table count and segment vector count come next. */

    zzekspsh_(&c__1, &nt3);
    zzekspsh_(&c__1, &nsv3);

/*     Just reserve room for the segment vectors and the segment vector */
/*     row set base addresses and counts. */

    nresv = nsv3 * (nt3 + 2);
    i__1 = nresv;
    for (i__ = 1; i__ <= i__1; ++i__) {
	zzekspsh_(&c__1, &c__0);
    }

/*     Initialize the output segment vector count and the total row */
/*     count. */

    s3 = 0;
    *nrows = 0;

/*     For every segment vector in the first join row set, */

    i__1 = nsv1;
    for (s1 = 1; s1 <= i__1; ++s1) {

/*        Fill in the first NT1 elements of our composite segment vector */
/*        with the current segment vector from the first join row set. */

	offset = (s1 - 1) * nt1 + 4;
	i__2 = *jbase1 + offset + 1;
	i__3 = *jbase1 + offset + nt1;
	zzeksrd_(&i__2, &i__3, segvec);

/*        Get the row set base address and count for this segment vector. */

	offset = nsv1 * nt1 + 4 + (s1 - 1 << 1) + 1;
	i__2 = *jbase1 + offset;
	i__3 = *jbase1 + offset;
	zzeksrd_(&i__2, &i__3, &rb1);
	i__2 = *jbase1 + offset + 1;
	i__3 = *jbase1 + offset + 1;
	zzeksrd_(&i__2, &i__3, &nr1);

/*        For every segment vector in the second join row set, */

	i__2 = nsv2;
	for (s2 = 1; s2 <= i__2; ++s2) {

/*           Fill in the last NT2 elements of our composite segment */
/*           vector with the current segment vector from the second join */
/*           row set. */

	    offset = (s2 - 1) * nt2 + 4;
	    i__4 = *jbase2 + offset + 1;
	    i__5 = *jbase2 + offset + nt2;
	    zzeksrd_(&i__4, &i__5, &segvec[(i__3 = nt1) < 10 && 0 <= i__3 ? 
		    i__3 : s_rnge("segvec", i__3, "zzekjoin_", (ftnlen)520)]);

/*           Write this segment vector to the output join row set. */

	    ++s3;
	    sgvbas = (s3 - 1) * nt3 + 4;
	    i__3 = *jbase3 + sgvbas + 1;
	    i__4 = *jbase3 + sgvbas + nt3;
	    zzeksupd_(&i__3, &i__4, segvec);

/*           Get the row set base address and count for this segment */
/*           vector. */

	    offset = nsv2 * nt2 + 4 + (s2 - 1 << 1) + 1;
	    i__3 = *jbase2 + offset;
	    i__4 = *jbase2 + offset;
	    zzeksrd_(&i__3, &i__4, &rb2);
	    i__3 = *jbase2 + offset + 1;
	    i__4 = *jbase2 + offset + 1;
	    zzeksrd_(&i__3, &i__4, &nr2);

/*           It's time to decide which row vectors corresponding to */
/*           our two segment vectors satisfy the join constraints. */
/*           We pass off the job of determining which row vectors to */
/*           consider to the subroutine pair ZZEKJPRP (join preparation) */
/*           and ZZEKJNXT (get next joined row vector). */

/*           We defer establishing the base address of the output */
/*           row vector set until the join reduction is done, since */
/*           the join operation will use the scratch area. */

	    zzekjprp_(segvec, jbase1, &nt1, &rb1, &nr1, jbase2, &nt2, &rb2, &
		    nr2, njcnst, active, cpidx1, clidx1, elts1, ops, cpidx2, 
		    clidx2, elts2, sthan, stsdsc, stdtpt, dtpool, dtdscs);

/*           Initialize the row count for the current output segment */
/*           vector.  Also set the segment vector row set base address. */

	    nr3 = 0;
	    zzekstop_(&top);
	    rb3 = top - *jbase3;
	    offset = nsv3 * nt3 + 4 + (s3 - 1 << 1) + 1;
	    i__3 = *jbase3 + offset;
	    i__4 = *jbase3 + offset;
	    zzeksupd_(&i__3, &i__4, &rb3);

/*           Fetch the row vectors that satisfy the join constraints. */

	    nr3 = 0;
	    zzekjnxt_(&found, rowvec);
	    while(found) {

/*              Append the base offset of the parent segment vector */
/*              to the row vector.  The base offset is one less than */
/*              the base-relative address of the segment vector. */

		++nr3;
		rowvec[(i__3 = nt3) < 11 && 0 <= i__3 ? i__3 : s_rnge("rowvec"
			, i__3, "zzekjoin_", (ftnlen)588)] = sgvbas;

/*              Add this vector to the output join row set.  Get the */
/*              next row vector. */

		i__3 = nt3 + 1;
		zzekspsh_(&i__3, rowvec);
		zzekjnxt_(&found, rowvec);
	    }

/*           At this point, we've tested every row corresponding to the */
/*           current segment vector.  Update the row count for this */
/*           segment vector. */

	    offset = nsv3 * nt3 + 4 + (s3 - 1 << 1) + 2;
	    i__3 = *jbase3 + offset;
	    i__4 = *jbase3 + offset;
	    zzeksupd_(&i__3, &i__4, &nr3);

/*           Keep the overall row total up to date. */

	    *nrows += nr3;
	}
    }

/*     Fill in the row count and size values in the output join row */
/*     set. */

    zzekstop_(&top);
    i__1 = *jbase3 + 1;
    i__2 = *jbase3 + 1;
    i__3 = top - *jbase3;
    zzeksupd_(&i__1, &i__2, &i__3);
    i__1 = *jbase3 + 2;
    i__2 = *jbase3 + 2;
    zzeksupd_(&i__1, &i__2, nrows);

/*     We've constructed the output join row set resulting from */
/*     joining the input row sets. */

    return 0;
} /* zzekjoin_ */

