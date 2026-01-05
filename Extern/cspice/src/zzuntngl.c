/* zzuntngl.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure ZZUNTNGL ( Untangle an AB linked-cell list  ) */
/* Subroutine */ int zzuntngl_(integer *nptr, integer *maxcel, integer *cells,
	 integer *maxb, integer *pntrs, integer *nout, integer *outlst)
{
    /* System generated locals */
    integer i__1, i__2;

    /* Local variables */
    integer aval, room;
    extern /* Subroutine */ int zztrvlnk_(integer *, integer *, integer *, 
	    integer *, integer *, integer *, integer *, integer *), chkin_(
	    char *, ftnlen);
    extern logical failed_(void);
    integer nfound;
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), setmsg_(char *, ftnlen), errint_(char *, integer *, 
	    ftnlen);
    integer ptrdex;
    extern logical return_(void);

/* $ Abstract */

/*     Untangle an unsorted AB linked-cell list into an A-index and */
/*     associated B-list. */

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

/*     AB linked list */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     NPTR       I   Length of A-list. */
/*     MAXCEL     I   Size of cell list. */
/*     CELLS      I   AB cell linked list. */
/*     MAXB       I   Maximum allowed dimension of B list. */
/*     PNTRS     I-O  Pointer array. */
/*     NOUT       O   Length of B-list. */
/*     OUTLST     O   B-list. */

/* $ Detailed_Input */

/*     NPTR       is the size of the input pointer array INPTR. */

/*     MAXCEL     is the number of cells in the CELLS array. That */
/*                array has dimensions */

/*                   (2, MAXCEL) */

/*     CELLS      is an array containing linked lists of "B" values. */
/*                The elements at indices */

/*                   (1,K) */
/*                   (2,K) */

/*                are, respectively, a "B" value and a pointer to */
/*                another element of the array. Null pointers are */
/*                indicated by the value -1. */

/*                Each linked list in CELLS contains a set of "B" */
/*                values associated with a particular "A" value. */
/*                The input pointer list maps "A" values to the head */
/*                of the associated list in CELLS. */

/*     MAXB       is the maximum number of elements in the output */
/*                array OUTLST. */

/*     PNTRS      is an array of pointers from A values to */
/*                linked lists of "B" entries in the CELLS array. */

/* $ Detailed_Output */

/*     PNTRS      is an array of pointers that map "A" values to */
/*                their associated "B" values in the array OUTLST. */
/*                Null pointers are indicated by the value -1. */

/*                PNTRS must be declared with size at least NPTR. */

/*     NOUT       is the number of elements in the output list OUTLST. */

/*     OUTLST     is an array containing lists of "B" values. The Kth */
/*                element of PNTRS is the index of the start of a list */
/*                in OUTLST of "B" values associated with the "A" value */
/*                having index K. The list of values associated with an */
/*                "A" value starts with a count and is followed by */
/*                the values themselves. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  The routine signals the error SPICE(BARRAYTOOSMALL) if the */
/*         entire set of B-value lists from the input cell array, */
/*         including the counts for each list, array cannot be stored in */
/*         the OUTLST array. */

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

/*     See usage in ZZMKSPIN. */

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

/*        Argument list change: removed INPTR; renamed */
/*        OUTPTR to PNTRS. PNTRS is an in-out argument. */
/*        Updated header. */

/*      30-APR-2014 (NJB) */

/*        Changed argument list to accommodate */
/*        better error checking. Added error checks */
/*        for array overflows. Changed calls to */
/*        ADDLNK and TRVLNK. Updated header I/O */
/*        descriptions. */

/*      08-OCT-2009 (NJB) */

/*        Re-ordered header sections. */

/*      16-JUL-2004 (EDW) */

/*        Added check on NPTR to ensure it's smaller */
/*        than the size of the pointer arrays. */

/*        Removed use of BVAL array, TRVLNK now directly */
/*        writes to OUTLST array. */

/*      03-FEB-1999 (JAB) */


/* -& */
/* $ Index_Entries */

/*     AB linked list */

/* -& */

/*     SPICELIB functions */


/*     Local Variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }
    chkin_("ZZUNTNGL", (ftnlen)8);
    if (*nptr > *maxcel) {
	setmsg_("Input pointer array is larger than cell array. Pointer arra"
		"y size = #1. Cell array size = #2.", (ftnlen)93);
	errint_("#1", nptr, (ftnlen)2);
	errint_("#2", maxcel, (ftnlen)2);
	sigerr_("SPICE(BARRAYTOOSMALL)", (ftnlen)21);
	chkout_("ZZUNTNGL", (ftnlen)8);
	return 0;
    }

/*     ROOM is the remaining room in the output list. */

    room = *maxb;

/*     Initialize pointer index. */

    ptrdex = 0;

/*     Loop over all A-values. */

    i__1 = *nptr;
    for (aval = 1; aval <= i__1; ++aval) {

/*        Traverse the chained list for a particular A-value */
/*        and collect associated B-values. If B-values exists, */
/*        return the number of B-vals to the OUTLST array */
/*        at element PTRDEX + 1; return the list of B-vals */
/*        starting at element PTRDEX + 2. */

/*        Make sure the output pointers below are in range. */

	if (ptrdex + 2 > *maxb) {
	    setmsg_("Index larger than output array. Index = #1. Array size "
		    "= #2.", (ftnlen)60);
	    i__2 = ptrdex + 2;
	    errint_("#1", &i__2, (ftnlen)2);
	    errint_("#2", maxb, (ftnlen)2);
	    sigerr_("SPICE(BARRAYTOOSMALL)", (ftnlen)21);
	    chkout_("ZZUNTNGL", (ftnlen)8);
	    return 0;
	}
	if (room <= 0) {
	    setmsg_("Remaining room in output array is #1. Current input poi"
		    "nter index = #2. Output array size = #3. Output pointer "
		    "index is #4.", (ftnlen)123);
	    errint_("#1", &room, (ftnlen)2);
	    errint_("#2", &aval, (ftnlen)2);
	    errint_("#3", maxb, (ftnlen)2);
	    errint_("#4", &ptrdex, (ftnlen)2);
	    sigerr_("SPICE(BARRAYTOOSMALL)", (ftnlen)21);
	    chkout_("ZZUNTNGL", (ftnlen)8);
	    return 0;
	}
	zztrvlnk_(&aval, nptr, pntrs, maxcel, cells, &room, &outlst[ptrdex], &
		outlst[ptrdex + 1]);
	if (failed_()) {
	    chkout_("ZZUNTNGL", (ftnlen)8);
	    return 0;
	}

/*        Increment pointer PTRDEX if we found any B-vals. */

	nfound = outlst[ptrdex];
	if (nfound > 0) {

/*           Store in PNTRS the pointer to the returned list. */
/*           This assignment overwrites the input pointer from */
/*           AVAL to the head of the associated list in CELLS. */

	    pntrs[aval - 1] = ptrdex + 1;

/*           Update the count of available spaces in the */
/*           output list. Account for the list size stored */
/*           at the front of the list. */

	    room -= nfound + 1;

/*           Increment PTRDEX to mark the position of the final */
/*           B-val. */

	    ptrdex = ptrdex + 1 + nfound;
	} else {

/*           If no associated B-values exist, set the */
/*           PNTRS element to -1, indicating no B-value. */

	    pntrs[aval - 1] = -1;
	}
    }

/*     Return the current pointer value. */

    *nout = ptrdex;

/*     Standard SPICE error handling. */

    chkout_("ZZUNTNGL", (ftnlen)8);
    return 0;
} /* zzuntngl_ */

