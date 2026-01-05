/* swapad.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure SWAPAD ( Swap elements within a DP array ) */
/* Subroutine */ int swapad_(integer *n, integer *locn, integer *m, integer *
	locm, doublereal *array)
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    integer nsub, i__;
    extern /* Subroutine */ int chkin_(char *, ftnlen), swapd_(doublereal *, 
	    doublereal *);
    integer extra, lm, ln, nm, nn, begsub;
    extern /* Subroutine */ int cyadip_(integer *, char *, integer *, 
	    doublereal *, ftnlen);
    integer direct;
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), setmsg_(char *, ftnlen), errint_(char *, integer *, 
	    ftnlen);
    extern logical return_(void);
    char dir[1];

/* $ Abstract */

/*     Swap (exchange) two non-intersecting groups of contiguous */
/*     elements of a double precision array. */

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

/*     ARRAY */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     N          I   Number of elements in the first group. */
/*     LOCN       I   Location of the first group. */
/*     M          I   Number of elements in the second group. */
/*     LOCM       I   Location of the second group. */
/*     ARRAY     I-O  The array. */

/* $ Detailed_Input */

/*     N, */
/*     LOCN     define the first group of elements to be exchanged: */
/*              ARRAY(LOCN) through ARRAY(LOCN+N-1). */

/*     M, */
/*     LOCM     define the second group of elements to be exchanged: */
/*              ARRAY(LOCM) through ARRAY(LOCM+M-1). These must be */
/*              distinct from the first group. */

/*     ARRAY    on input contains both groups of elements in their */
/*              original locations. */

/* $ Detailed_Output */

/*     ARRAY    on output contains the input array with the indicated */
/*              groups of elements exchanged. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the elements to be swapped are not distinct, the error */
/*         SPICE(NOTDISTINCT) is signaled. */

/*     2)  If LOCN or LOCM is less than one, the error */
/*         SPICE(INVALIDINDEX) is signaled. */

/*     3)  If the number of elements to be swapped is less than zero, */
/*         the error SPICE(INVALIDARGUMENT) is signaled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     If N [M] is zero, the second [first] group is removed from */
/*     its current location and inserted in front of ARRAY(LOCN) */
/*     [ARRAY(LOCM)]. Thus, to move the second [first] group to the */
/*     front of the list, set N [M] and LOCN [LOCM] to zero and one */
/*     respectively. To move the group to the end of the list, set */
/*     N [M] and LOCN [LOCM] to zero and one more than the number of */
/*     elements in the array. */

/*     All of the elements to be swapped must be distinct. */

/* $ Examples */

/*     Let ARRAY contain the following elements. */

/*           Roosevelt */
/*           Truman */
/*           Eisenhower */
/*           Kennedy */
/*           Johnson */
/*           Nixon */
/*           Ford */
/*           Carter */
/*           Reagan */
/*           Cuomo */

/*     Then the following calls */

/*           CALL SWAPAC (  1,  2,  2,  7,  ARRAY ) */
/*           CALL SWAPAC (  3,  1,  3,  8,  ARRAY ) */
/*           CALL SWAPAC (  3,  4,  0,  1,  ARRAY ) */
/*           CALL SWAPAC (  2,  4,  0,  11, ARRAY ) */

/*     yield the following arrays respectively. */

/*           [1]          [2]          [3]          [4] */

/*           Roosevelt    Carter       Kennedy      Roosevelt */
/*           Ford         Reagan       Johnson      Truman */
/*           Carter       Cuomo        Nixon        Eisenhower */
/*           Eisenhower   Kennedy      Roosevelt    Nixon */
/*           Kennedy      Johnson      Truman       Ford */
/*           Johnson      Nixon        Eisenhower   Carter */
/*           Nixon        Ford         Ford         Reagan */
/*           Truman       Roosevelt    Carter       Cuomo */
/*           Reagan       Truman       Reagan       Kennedy */
/*           Cuomo        Eisenhower   Cuomo        Johnson */

/*     The following calls */

/*           CALL SWAPAC ( 3, 2, 4, 5, ARRAY ) */
/*           CALL SWAPAC ( 4, 5, 3, 2, ARRAY ) */

/*     yield the following arrays. Note that the resulting arrays */
/*     are equivalent. */

/*           [1]          [2] */

/*           Roosevelt    Roosevelt */
/*           Johnson      Johnson */
/*           Nixon        Nixon */
/*           Ford         Ford */
/*           Carter       Carter */
/*           Truman       Truman */
/*           Eisenhower   Eisenhower */
/*           Kennedy      Kennedy */
/*           Reagan       Reagan */
/*           Cuomo        Cuomo */


/*     The calls */

/*           CALL SWAPAC ( 3,  5, 4,  6, ARRAY ) */
/*           CALL SWAPAC ( 3, -3, 3, 10, ARRAY ) */

/*     signal the errors */

/*           SPICE(NOTDISTINCT) */
/*           SPICE(INVALIDINDEX) */

/*     respectively. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     H.A. Neilan        (JPL) */
/*     B.V. Semenov       (JPL) */
/*     W.L. Taber         (JPL) */
/*     I.M. Underwood     (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.2.0, 13-AUG-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 1.1.1, 18-MAY-2010 (BVS) */

/*        Removed "C$" markers from text in the header. */

/* -    SPICELIB Version 1.1.0, 09-SEP-2005 (NJB) */

/*        Updated to remove non-standard use of duplicate arguments */
/*        in CYCLAD call. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU) (HAN) */

/* -& */
/* $ Index_Entries */

/*     swap elements within a d.p. array */

/* -& */
/* $ Revisions */

/* -    SPICELIB Version 1.1.0, 09-SEP-2005 (NJB) */

/*        Updated to remove non-standard use of duplicate arguments */
/*        in CYCLAD call. */

/* -    Beta Version 2.0.0, 03-JAN-1989 (HAN) */

/*        The "Particulars" section stated that by setting N [M] */
/*        to zero, the second [first] group is removed from its current */
/*        location and inserted in front of ARRAY(LOCM) [ARRAY(LOCN)]. */
/*        That statement was incorrect. Insertion occurs in front of */
/*        ARRAY(LOCN) [ARRAY(LOCM)]. The section has been corrected. */

/*        New checks for locations were added. LOCN and LOCM must be */
/*        greater than one, not zero as specified before. If they are */
/*        not, and error is signaled. */

/*        More examples were added to the "Examples" section, and */
/*        the long error messages were revised. */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     We will assume that LOCN and N refer to the earlier group of */
/*     elements, LOCM and M to the later group. (We can always make */
/*     this true by exchanging their values.) We also assume that */
/*     all the elements to be swapped are distinct. (That is, LOCM */
/*     is greater than or equal to LOCN+N.) */

/*     It's easy enough to swap elements on a one-to-one basis, but */
/*     what about the ones left over? Without extra storage, they can */
/*     be moved one at a time; but each such move requires moving every */
/*     element between the origin and destination as well. For large */
/*     arrays, this is clearly unacceptable. */

/*     In the figure below, the array on the left contains two groups */
/*     of elements which are to be swapped. We can begin by swapping the */
/*     leading elements of each group one-for-one. */

/*        +--------------+        +--------------+ */
/*        |              |        |              | */
/*        +--------------+        +--------------+ */
/*        | Adam         |        | Barney       | */
/*        +--------------+        +--------------+ */
/*        | Alvin        |        | Betty        | */
/*        +--------------+        +--------------+ */
/*        |              |        |              |  <---+ */
/*        +--------------+        +--------------+      | */
/*        |              |        |              |      | */
/*        +--------------+        +--------------+      | */
/*        | Barney       |        | Adam         |      | */
/*        +--------------+        +--------------+      | */
/*        | Betty        |        | Alvin        |      | */
/*        +--------------+        +--------------+      | */
/*        | Bill         |        | Bill         |      | */
/*        +--------------+        +--------------+      | */
/*        | Bob          |        | Bob          |  <---+ */
/*        +--------------+        +--------------+ */
/*        |              |        |              | */
/*        +--------------+        +--------------+ */
/*        |              |        |              | */
/*        +--------------+        +--------------+ */

/*     Notice that cycling the indicated sub-array forward twice brings */
/*     the remaining elements to their proper locations. This is most */
/*     fortunate, because cycling the elements of an array is a linear */
/*     operation. (See CYCLAx for details.) */

/*     And what if the extra elements are in the first group? */

/*        +--------------+        +--------------+ */
/*        |              |        |              | */
/*        +--------------+        +--------------+ */
/*        | Barney       |        | Adam         | */
/*        +--------------+        +--------------+ */
/*        | Betty        |        | Alvin        | */
/*        +--------------+        +--------------+ */
/*        | Bill         |        | Bill         |  <---+ */
/*        +--------------+        +--------------+      | */
/*        | Bob          |        | Bob          |      | */
/*        +--------------+        +--------------+      | */
/*        |              |        |              |      | */
/*        +--------------+        +--------------+      | */
/*        |              |        |              |      | */
/*        +--------------+        +--------------+      | */
/*        | Adam         |        | Barney       |      | */
/*        +--------------+        +--------------+      | */
/*        | Alvin        |        | Betty        |  <---+ */
/*        +--------------+        +--------------+ */
/*        |              |        |              | */
/*        +--------------+        +--------------+ */
/*        |              |        |              | */
/*        +--------------+        +--------------+ */

/*     In this case, the indicated sub-array must be cycled backward */
/*     in order to bring the extra elements to their proper places. */

/*     The algorithm is: */

/*        1) Let DIRECT be the smaller of N and M, and let EXTRA */
/*           be the absolute value of the difference (N-M). */

/*        2) Exchange DIRECT elements directly. */

/*        3) Determine the direction of the cycle: forward when N < M, */
/*           backward when N > M. */

/*        4) Determine the sub-array to be cycled. It begins at element */
/*           (LOCN+DIRECT) and contains (LOCM-LOCN) + (M-DIRECT) elements */

/*        5) Cycle the sub-array EXTRA times in the indicated direction. */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("SWAPAD", (ftnlen)6);
    }

/*     Check to see if the inputs are valid. */

    if (*n < 0) {
	setmsg_("Number of elements in the first group is *.", (ftnlen)43);
	errint_("*", n, (ftnlen)1);
	sigerr_("SPICE(INVALIDARGUMENT)", (ftnlen)22);
	chkout_("SWAPAD", (ftnlen)6);
	return 0;
    } else if (*m < 0) {
	setmsg_("Number of elements in the second group is *.", (ftnlen)44);
	errint_("*", m, (ftnlen)1);
	sigerr_("SPICE(INVALIDARGUMENT)", (ftnlen)22);
	chkout_("SWAPAD", (ftnlen)6);
	return 0;
    } else if (*locn < 1) {
	setmsg_("Location of the first group is *.", (ftnlen)33);
	errint_("*", locn, (ftnlen)1);
	sigerr_("SPICE(INVALIDINDEX)", (ftnlen)19);
	chkout_("SWAPAD", (ftnlen)6);
	return 0;
    } else if (*locm < 1) {
	setmsg_("Location of the second group is *.", (ftnlen)34);
	errint_("*", locm, (ftnlen)1);
	sigerr_("SPICE(INVALIDINDEX)", (ftnlen)19);
	chkout_("SWAPAD", (ftnlen)6);
	return 0;
    }

/*     Make sure we have the groups in the right order. */

    if (*locn < *locm) {
	ln = *locn;
	lm = *locm;
	nn = *n;
	nm = *m;
    } else {
	ln = *locm;
	lm = *locn;
	nn = *m;
	nm = *n;
    }

/*     The elements must be distinct. */

    if (lm < ln + nn) {
	setmsg_("Elements to be swapped are not distinct.", (ftnlen)40);
	sigerr_("SPICE(NOTDISTINCT)", (ftnlen)18);
	chkout_("SWAPAD", (ftnlen)6);
	return 0;
    }

/*     Direct exchange. */

    direct = min(nn,nm);
    i__1 = direct - 1;
    for (i__ = 0; i__ <= i__1; ++i__) {
	swapd_(&array[ln + i__ - 1], &array[lm + i__ - 1]);
    }

/*     Cycle. */

    extra = (i__1 = nn - nm, abs(i__1));
    if (extra > 0) {
	if (nn < nm) {
	    *(unsigned char *)dir = 'F';
	} else {
	    *(unsigned char *)dir = 'B';
	}
	begsub = ln + direct;
	nsub = lm - ln + (nm - direct);
	cyadip_(&nsub, dir, &extra, &array[begsub - 1], (ftnlen)1);
    }
    chkout_("SWAPAD", (ftnlen)6);
    return 0;
} /* swapad_ */

