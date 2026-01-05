/* cyclad.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure CYCLAD ( Cycle the elements of a DP array ) */
/* Subroutine */ int cyclad_(doublereal *array, integer *nelt, char *dir, 
	integer *ncycle, doublereal *out, ftnlen dir_len)
{
    /* System generated locals */
    integer i__1, i__2;

    /* Local variables */
    doublereal last, temp;
    integer g, i__, j, k, l, m;
    extern /* Subroutine */ int chkin_(char *, ftnlen), errch_(char *, char *,
	     ftnlen, ftnlen), moved_(doublereal *, integer *, doublereal *), 
	    sigerr_(char *, ftnlen), chkout_(char *, ftnlen), setmsg_(char *, 
	    ftnlen);
    extern logical return_(void);
    extern integer gcd_(integer *, integer *);

/* $ Abstract */

/*     Cycle the elements of a double precision array forward */
/*     or backward. */

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
/*     ARRAY      I   Input array. */
/*     NELT       I   Number of elements. */
/*     DIR        I   Direction to cycle: 'F' or 'B'. */
/*     NCYCLE     I   Number of times to cycle. */
/*     OUT        O   Cycled array. */

/* $ Detailed_Input */

/*     ARRAY    is the array to be cycled. */

/*     NELT     is the number of elements in the input array. */

/*     DIR      is the direction in which the elements in the */
/*              array are to be cycled. */

/*                 'F' or 'f'  to cycle forward. */
/*                 'B' or 'b'  to cycle backward. */

/*     NCYCLE   is the number of times the elements in the array */
/*              are to be cycled. */

/* $ Detailed_Output */

/*     OUT      is the input array after it has been cycled. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the value of DIR is not recognized, the error */
/*         SPICE(INVALIDDIRECTION) is signaled. */

/*     2)  If NELT is less than 1, the output array is not modified. */

/*     3)  If NCYCLE is negative, the array is cycled NCYCLE times in */
/*         the opposite direction of DIR. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     An array is cycled when its contents are shifted forward or */
/*     backward by one place. An element pushed off one end of the */
/*     array is brought around to the other end of the array instead */
/*     of disappearing. */

/* $ Examples */

/*     Let the double precision A contain the following elements. */

/*        A(1) = 1.D0 */
/*        A(2) = 2.D0 */
/*        A(3) = 3.D0 */
/*        A(4) = 4.D0 */

/*     Cycling A forward once yields the array */

/*        A(1) = 4.D0 */
/*        A(2) = 1.D0 */
/*        A(3) = 2.D0 */
/*        A(4) = 3.D0 */

/*     Cycling A backward once yields the array */

/*        A(1) = 2.D0 */
/*        A(2) = 3.D0 */
/*        A(3) = 4.D0 */
/*        A(4) = 1.D0 */

/*     Cycling by any multiple of the number of elements in the array */
/*     yields the same array. */

/* $ Restrictions */

/*     1)  The memory used for the output array must be disjoint from the */
/*         memory used for the input array. */

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

/* -    SPICELIB Version 1.1.0, 20-AUG-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 1.0.3, 18-MAY-2010 (BVS) */

/*        Removed "C$" marker from text in the header. */

/* -    SPICELIB Version 1.0.2, 23-APR-2010 (NJB) */

/*        Header correction: assertions that the output */
/*        can overwrite the input have been removed. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU) (WLT) (HAN) */

/* -& */
/* $ Index_Entries */

/*     cycle the elements of a d.p. array */

/* -& */
/* $ Revisions */

/* -     Beta Version 2.0.0, 16-JAN-1989 (HAN) */

/*         Error handling was added to detect an invalid value for */
/*         the cycling direction. If the direction is not recognized */
/*         the error SPICE(INVALIDDIRECTION) is signaled and the */
/*         output array is not modified. (The routine used to copy the */
/*         input array into the output array if the direction was not */
/*         recognized.) */

/*         The "Exceptions" section was filled out in more detail. */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("CYCLAD", (ftnlen)6);
    }

/*     Don't even screw around if there are no elements in the array. */

    if (*nelt < 1) {
	chkout_("CYCLAD", (ftnlen)6);
	return 0;
    }

/*     A backward cycle is the same as a forward cycle by the opposite */
/*     of NCYCLE.  Moreover a cycle by K is the same as a cycle by */
/*     K + m*N for any integer m.  Thus we compute the value of the */
/*     minimum forward right cycle that is equivalent to the inputs. */

    if (*(unsigned char *)dir == 'B' || *(unsigned char *)dir == 'b') {
	k = -(*ncycle) % *nelt;
    } else if (*(unsigned char *)dir == 'F' || *(unsigned char *)dir == 'F') {
	k = *ncycle % *nelt;
    } else {
	setmsg_("Cycling direction was *.", (ftnlen)24);
	errch_("*", dir, (ftnlen)1, (ftnlen)1);
	sigerr_("SPICE(INVALIDDIRECTION)", (ftnlen)23);
	chkout_("CYCLAD", (ftnlen)6);
	return 0;
    }
    if (k < 0) {
	k += *nelt;
    } else if (k == 0) {
	moved_(array, nelt, out);
	chkout_("CYCLAD", (ftnlen)6);
	return 0;
    }

/*     The algorithm used to cycle arrays is identical to the one used */
/*     to cycle character strings in CYCLEC. We won't repeat the (rather */
/*     lengthy) description here. */

    g = gcd_(&k, nelt);
    m = *nelt / g;
    i__1 = g;
    for (i__ = 1; i__ <= i__1; ++i__) {
	l = i__;
	last = array[l - 1];
	i__2 = m;
	for (j = 1; j <= i__2; ++j) {
	    l += k;
	    if (l > *nelt) {
		l -= *nelt;
	    }
	    temp = array[l - 1];
	    out[l - 1] = last;
	    last = temp;
	}
    }
    chkout_("CYCLAD", (ftnlen)6);
    return 0;
} /* cyclad_ */

