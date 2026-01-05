/* dafra.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure DAFRA ( DAF, Re-order arrays ) */
/* Subroutine */ int dafra_(integer *handle, integer *iorder, integer *n)
{
    /* System generated locals */
    integer i__1, i__2;

    /* Local variables */
    integer hold, i__;
    extern /* Subroutine */ int dafgn_(char *, ftnlen), dafgs_(doublereal *), 
	    dafrn_(char *, ftnlen), chkin_(char *, ftnlen);
    char holdn[1000];
    extern /* Subroutine */ int dafws_(doublereal *);
    integer index;
    doublereal holds[128];
    logical found;
    char tempn[1000];
    integer total;
    doublereal temps[128];
    integer start;
    extern /* Subroutine */ int daffna_(logical *);
    extern logical failed_(void);
    extern /* Subroutine */ int dafbfs_(integer *), sigerr_(char *, ftnlen), 
	    chkout_(char *, ftnlen), setmsg_(char *, ftnlen), errint_(char *, 
	    integer *, ftnlen);
    extern logical isordv_(integer *, integer *), return_(void);

/* $ Abstract */

/*     Reorder the arrays in a DAF according to a given order vector. */

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

/*     DAF */

/* $ Keywords */

/*     FILES */
/*     SORT */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     HANDLE     I   Handle of DAF. */
/*     IORDER     I   Order vector. */
/*     N          I   Dimension of IORDER. */

/* $ Detailed_Input */

/*     HANDLE   is the handle of a DAF that has been opened for */
/*              write access. Use DAFOPW, for example, to open */
/*              an existing file and get its handle. */

/*     IORDER   is the order vector to be used to re-order the */
/*              arrays stored in the DAF specified by HANDLE. */

/*              An integer order vector is an array of length */
/*              N whose elements are the integers 1 through N. */

/*              The first element of IORDER is the index of the */
/*              first array in the re-ordered file, and so on. */

/*     N        is the number of elements in the order vector. */
/*              This may be less than the number of arrays in */
/*              the file. */

/* $ Detailed_Output */

/*     None. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If IORDER is not an order vector (that is, if it does */
/*         not contain every integer between 1 and N), the error */
/*         SPICE(DISORDER) is signaled. */

/*     2)  If N is greater than the number of arrays in the file, */
/*         the error SPICE(DISARRAY) is signaled. */

/* $ Files */

/*     See argument HANDLE. */

/* $ Particulars */

/*     DAFRA does not actually move the elements of the double */
/*     precision arrays; it works by rearranging the contents */
/*     of the summary and name records in the file. The result */
/*     is that the search routines (BFS, FNA, BBS, FPA) will */
/*     return the arrays in the indicated order. */

/*     After re-ordering, array IORDER(1) of the input file is the */
/*     first array of the output file, array IORDER(2) of the input */
/*     file is the second array of the output file, and so on. */

/*     The order vector used by DAFRA is typically created for */
/*     a related array by one of the ORDER routines, as shown in */
/*     the example below. */

/* $ Examples */

/*     The following code fragment sorts the arrays in a DAF by name. */

/*        C */
/*        C     Collect the names of the arrays in the file. */
/*        C */
/*              CALL DAFOPW ( FILE, HANDLE ) */

/*              N = 0 */
/*              CALL DAFBFS ( HANDLE ) */
/*              CALL DAFFNA ( FOUND  ) */

/*              DO WHILE ( FOUND ) */
/*                 N = N + 1 */
/*                 CALL DAFGN  ( NAMES(I) ) */
/*                 CALL DAFFNA ( FOUND    ) */
/*              END DO */

/*        C */
/*        C     Sort the names. */
/*        C */
/*              CALL ORDERC ( NAMES, N, IORDER ) */

/*        C */
/*        C     Re-order the arrays. */
/*        C */
/*              CALL DARFA  ( HANDLE, IORDER, N ) */
/*              CALL DAFCLS ( HANDLE            ) */

/*     Afterward, a forward search like the one shown below */

/*        CALL DAFBFS ( HANDLE ) */
/*        CALL DAFFNA ( FOUND  ) */

/*        DO WHILE ( FOUND ) */
/*           CALL DAFGN ( NAME ) */
/*           WRITE (*,*) NAME */

/*           CALL DAFFNA ( FOUND ) */
/*        END DO */

/*     produces an ordered list of the names in the sorted file. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     W.L. Taber         (JPL) */
/*     I.M. Underwood     (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 27-AUG-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 28-MAR-1991 (IMU) */

/* -& */
/* $ Index_Entries */

/*     reorder DAF arrays */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     Local variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("DAFRA", (ftnlen)5);
    }

/*     If the order vector has fewer than two elements, don't bother. */

    if (*n < 2) {
	chkout_("DAFRA", (ftnlen)5);
	return 0;
    }

/*     If IORDER is not an order vector, complain. */

    if (! isordv_(iorder, n)) {
	setmsg_("Sorry, IORDER is not an order vector.", (ftnlen)37);
	sigerr_("SPICE(DISORDER)", (ftnlen)15);
	chkout_("DAFRA", (ftnlen)5);
	return 0;
    }

/*     If the number of arrays to be moved exceeds the number of */
/*     arrays in the file, complain. */

    total = 0;
    dafbfs_(handle);
    daffna_(&found);
    while(found && ! failed_()) {
	++total;
	daffna_(&found);
    }
    if (failed_()) {
	chkout_("DAFRA", (ftnlen)5);
	return 0;
    } else if (total < *n) {
	setmsg_("N (#) exceeds number of arrays (#).", (ftnlen)35);
	errint_("#", n, (ftnlen)1);
	errint_("#", &total, (ftnlen)1);
	sigerr_("SPICE(DISARRAY)", (ftnlen)15);
	chkout_("DAFRA", (ftnlen)5);
	return 0;
    }

/*     Not surprisingly, this routine is patterned closely after the */
/*     (original) REORDx routines in SPICELIB. The only differences */
/*     are that */

/*        1) This routine is not error free---it checks to make */
/*           sure that IORDER is in fact an order vector, and that */
/*           every element in IORDER refers to an existing array. */

/*        2) Instead of moving elements of an array in and out of */
/*           a temporary location, it moves summaries and names. */
/*           This means that two sets of temporary storage locations */
/*           are needed: one to hold the summary and name of the */
/*           guy who began the current cycle; and one to hold the guy */
/*           being moved from location HOLD to location INDEX. */

    start = 1;
    while(start < *n && ! failed_()) {

/*        Start the cycle. One guy (pair of summary and name record) */
/*        has to sit out (in HOLDS and HOLDN) until the end of the cycle */
/*        is reached. */

	index = start;
	hold = iorder[index - 1];
	dafbfs_(handle);
	i__1 = index;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    daffna_(&found);
	}
	dafgs_(holds);
	dafgn_(holdn, (ftnlen)1000);

/*        Move guys from HOLD to INDEX; then update HOLD (to point */
/*        to the next guy to be moved) and INDEX (to point at the */
/*        space just vacated). */

/*        Keep going until HOLD points to the first guy moved during */
/*        the current cycle. This ends the cycle. */

	while(hold != start) {

/*           Get the guy in position HOLD. */

	    dafbfs_(handle);
	    i__1 = hold;
	    for (i__ = 1; i__ <= i__1; ++i__) {
		daffna_(&found);
	    }
	    dafgs_(temps);
	    dafgn_(tempn, (ftnlen)1000);

/*           Move him to position INDEX. (Note that DAFWS is used to */
/*           update the summary instead of DAFRS, because the addresses */
/*           are actually being changed.) */

	    dafbfs_(handle);
	    i__1 = index;
	    for (i__ = 1; i__ <= i__1; ++i__) {
		daffna_(&found);
	    }
	    dafws_(temps);
	    dafrn_(tempn, (ftnlen)1000);

/*           Update HOLD and INDEX. */

	    index = hold;
	    hold = iorder[hold - 1];
	    iorder[index - 1] = -iorder[index - 1];
	}

/*        The last element in the cycle is restored from TEMP. */

	dafbfs_(handle);
	i__1 = index;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    daffna_(&found);
	}
	dafws_(holds);
	dafrn_(holdn, (ftnlen)1000);
	iorder[hold - 1] = -iorder[hold - 1];

/*        Begin the next cycle at the next element in the order */
/*        vector with a positive sign. (That is, the next one */
/*        that hasn't been moved.) */

	while(iorder[start - 1] < 0 && start < *n) {
	    ++start;
	}
    }

/*     Restore the original signs of the elements of the order */
/*     vector, for the next go around. */

    i__1 = *n;
    for (index = 1; index <= i__1; ++index) {
	iorder[index - 1] = (i__2 = iorder[index - 1], abs(i__2));
    }
    chkout_("DAFRA", (ftnlen)5);
    return 0;
} /* dafra_ */

