/* zztrvlnk.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure ZZTRVLNK ( Traverse AB cell linked-list ) */
/* Subroutine */ int zztrvlnk_(integer *aval, integer *maxa, integer *pntrs, 
	integer *cellsz, integer *cells, integer *maxb, integer *nb, integer *
	blist)
{
    integer i__;
    extern /* Subroutine */ int chkin_(char *, ftnlen), sigerr_(char *, 
	    ftnlen), chkout_(char *, ftnlen), setmsg_(char *, ftnlen), 
	    errint_(char *, integer *, ftnlen);
    extern logical return_(void);
    integer ptr;

/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines. Users should not call this routine directly due */
/*     to the volatile nature of this routine. */

/*     Traverse an AB pool linked list, searching for all cells that are */
/*     associated with a specified A-value. Store the B-values of these */
/*     cells in an array. */

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

/*     linked list */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     AVAL       I   A-value index associated with pool entry. */
/*     MAXA       I   Size of PNTRS array. */
/*     PNTRS      I   A-value pointer array. */
/*     CELLSZ     I   Number of cells in cell array. */
/*     CELLS      I   Cell array. */
/*     MAXB       I   Size of BLIST array. */
/*     NB         O   Number of B values found. */
/*     BLIST      O   List of B values. */

/* $ Detailed_Input */

/*     AVAL       is an index associated with a pool entry. AVAL */
/*                is the index of the Ith value of a finite set "A." */
/*                AVAL is an index into the PNTRS array. */

/*     MAXA       Size of PNTRS array. */

/*     PNTRS      is an array of integers acting as pointers into the */
/*                cell array. The element at index I of PNTRS points to */
/*                the head of a linked list in CELLS that associates */
/*                B-values with the index I. Here I is the "A value." */

/*                If there are no values associated with the Ith A value, */
/*                PNTRS(I) is null (-1). */


/*     CELLSZ     is the number of cells in the cell array. The array */
/*                has dimensions */

/*                   (2, CELLSZ) */

/*     CELLS      is the input cell array. The element */

/*                   CELLS ( 1, PNTRS(AVAL) ) */

/*                contains a B-value associated with AVAL. The element */

/*                   CELLS ( 2, PNTRS(AVAL) ) */

/*                is a pointer to the next node of the list associated */
/*                with AVAL, or is null (-1) if the list contains only */
/*                one element. The pointer element of the last list node */
/*                for AVAL is null. */

/*     MAXB       is the size of the output B-value list. */

/* $ Detailed_Output */

/*     NB         is the number of nodes in the list associated with */
/*                AVAL. */

/*     BLIST      is an array containing the B-values of the nodes */
/*                associated with AVAL. There is one entry in BLIST for */
/*                each node in the list associated with AVAL; the list */
/*                may contain duplicates. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the array index AVAL is less than 1 or exceeds the */
/*         declared size of the PNTR array, the error */
/*         SPICE(INDEXOUTOFRANGE) is signaled. Occurrence of this error */
/*         may result from the fine voxel scale being too small for the */
/*         data. */

/*     2)  If the size of the output BLIST array is non-positive, the */
/*         error SPICE(INVALIDSIZE) is signaled. */

/*     3)  If an element of the PNTRS array is larger than the length */
/*         CELLSZ of the cell array, the error SPICE(POINTEROUTOFRANGE) */
/*         is signaled. */

/*     4)  If the number of output list entries exceeds the size MAXB of */
/*         the output list array, the error SPICE(BARRAYTOOSMALL) is */
/*         signaled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     The AB list routines support manipulation of a data structure that */
/*     represents a one-to-many mapping from a set A to a set B. Each */
/*     element of set A is mapped to a set of values from B, where the */
/*     range values are represented by a linked list of entries in the */
/*     cell array. */

/*     The elements of set B associated with the Ith element of A are */
/*     stored in a linked list of cells starting with the cell */
/*     consisting of elements */

/*        CELLS(*,PNTRS(I)) */

/*     The set A is really an abstraction: it's just some finite set */
/*     with size in the range 1:MAXA. The input AVAL is not a member of */
/*     A but rather just an index into A. For consistency with existing */
/*     code, the name AVAL has been retained. */

/*     The fact that B values are stored in linked lists enables a */
/*     program to store entries for A-B associations in random order. */
/*     For example, in the process of constructing DSK type 2 segments, */
/*     the of non-empty fine voxels intersected by a given plate can be */
/*     computed; then an entry representing a voxel-plate association */
/*     can be made for each fine voxel in the set. In this case, the */
/*     set "A" is the set of fine voxels belonging to non-empty coarse */
/*     voxels. */

/*     This routine supports creation of DSK type 2 segments. It is used */
/*     for creation of both voxel-plate association data structures and */
/*     of vertex-plate association data structures. */

/* $ Examples */

/*     See usage in the routine ZZUNTNGL. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman    (JPL) */
/*     J.A. Bytof      (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 23-AUG-2016 (NJB) */

/*      07-JAN-2016 (NJB) */

/*        Strengthened check on invalid pointer. Renamed argument MAXCEL */
/*        to CELLSZ. Updated header comments. */

/*      26-MAR-2015 (NJB) */

/*        Updated long error messages to improve accuracy. */

/*        Header update: added description of MAXB input */
/*        argument. Updated description of MAXA input */
/*        argument. Filled out Exceptions section. */

/*      30-APR-2014 (NJB) */

/*        Changed argument list: each array argument now */
/*        has an associated argument giving its size. */
/*        Updated error checking to check array indices */
/*        against array sizes. */

/*      08-OCT-2009 (NJB) */

/*        Re-ordered header sections. */

/*      11-JUN-2004 (EDW) */

/*        Added check on AVAL to ensure it's smaller */
/*        than the size of MAXA. */

/*      26-AUG-2002 (BVS) */

/*        Replaced WRITE with normal error reporting calls. */

/*      03-FEB-1999 (JAB) */


/* -& */
/* $ Index_Entries */

/*     AB cells */

/* -& */

/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }
    chkin_("ZZTRVLNK", (ftnlen)8);
    if (*aval < 1 || *aval > *maxa) {
	setmsg_("Index AVAL is out of range. Index = #1. Valid range = 1:#2.",
		 (ftnlen)59);
	errint_("#1", aval, (ftnlen)2);
	errint_("#2", maxa, (ftnlen)2);
	sigerr_("SPICE(INDEXOUTOFRANGE)", (ftnlen)22);
	chkout_("ZZTRVLNK", (ftnlen)8);
	return 0;
    }
    if (*maxb < 1) {
	setmsg_("Maximum output list size MAXB is invalid. MAXB = #1.", (
		ftnlen)52);
	errint_("#1", maxb, (ftnlen)2);
	sigerr_("SPICE(INVALIDSIZE)", (ftnlen)18);
	chkout_("ZZTRVLNK", (ftnlen)8);
	return 0;
    }
    *nb = 0;
    blist[0] = 0;
    ptr = pntrs[*aval - 1];
    while(ptr != -1) {
	if (ptr < -1 || ptr == 0 || ptr > *cellsz) {
	    setmsg_("Value in PNTRS array is not a valid index in the cell a"
		    "rray.Value = #1. Array size = #2.", (ftnlen)88);
	    errint_("#1", &ptr, (ftnlen)2);
	    errint_("#2", cellsz, (ftnlen)2);
	    sigerr_("SPICE(POINTEROUTOFRANGE)", (ftnlen)24);
	    chkout_("ZZTRVLNK", (ftnlen)8);
	    return 0;
	}
	++(*nb);
	if (*nb > *maxb) {
	    setmsg_("Output value count is larger than B-list array room. Co"
		    "unt = #1. Output array room = #2. Input pointer index wa"
		    "s #3. Input pointer list size was #4. Last pointer was #"
		    "5. Cell size was #6.", (ftnlen)187);
	    errint_("#1", nb, (ftnlen)2);
	    errint_("#2", maxb, (ftnlen)2);
	    errint_("#3", aval, (ftnlen)2);
	    errint_("#4", maxa, (ftnlen)2);
	    errint_("#5", &ptr, (ftnlen)2);
	    errint_("#6", cellsz, (ftnlen)2);
	    sigerr_("SPICE(BARRAYTOOSMALL)", (ftnlen)21);
	    chkout_("ZZTRVLNK", (ftnlen)8);
	    return 0;
	}
	blist[*nb - 1] = cells[(ptr << 1) - 2];
	i__ = cells[(ptr << 1) - 1];
	ptr = i__;
    }

/*     Standard SPICE error handling. */

    chkout_("ZZTRVLNK", (ftnlen)8);
    return 0;
} /* zztrvlnk_ */

