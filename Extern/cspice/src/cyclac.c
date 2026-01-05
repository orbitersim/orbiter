/* cyclac.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure CYCLAC ( Cycle the elements of a character array ) */
/* Subroutine */ int cyclac_(char *array, integer *nelt, char *dir, integer *
	ncycle, char *out, ftnlen array_len, ftnlen dir_len, ftnlen out_len)
{
    /* System generated locals */
    integer i__1, i__2, i__3;

    /* Builtin functions */
    integer i_len(char *, ftnlen);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    char last[1], temp[1];
    integer c__, g, i__, j, k, l, m;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    extern integer nbwid_(char *, integer *, ftnlen);
    extern /* Subroutine */ int errch_(char *, char *, ftnlen, ftnlen), 
	    movec_(char *, integer *, char *, ftnlen, ftnlen);
    integer limit;
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen);
    integer widest;
    extern /* Subroutine */ int setmsg_(char *, ftnlen);
    integer outlen;
    extern logical return_(void);
    extern integer gcd_(integer *, integer *);

/* $ Abstract */

/*     Cycle the elements of a character array forward or backward. */

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

/*                    'F' or 'f'  to cycle forward. */
/*                    'B' or 'b'  to cycle backward. */

/*     NCYCLE   is the number of times the elements in the array */
/*              are to be cycled. */

/* $ Detailed_Output */

/*     OUT      is the input array after it has been cycled. */
/*              OUT may overwrite ARRAY. */

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

/*     Let the integer array A contain the following elements. */

/*           A(1) = 'apple' */
/*           A(2) = 'bear' */
/*           A(3) = 'cake' */
/*           A(4) = 'dragon' */

/*     Cycling A forward once yields the array */

/*           A(1) = 'dragon' */
/*           A(2) = 'apple' */
/*           A(3) = 'bear' */
/*           A(4) = 'cake' */

/*     Cycling A backward once yields the array */

/*           A(1) = 'bear' */
/*           A(2) = 'cake' */
/*           A(3) = 'dragon' */
/*           A(4) = 'apple' */

/*     Cycling by any multiple of the number of elements in the array */
/*     yields the same array. */

/* $ Restrictions */

/*     1)  The memory used for the output array must be identical to or */
/*         disjoint from the memory used for the input array. */

/*         That is: */

/*            CALL CYCLAC ( ARRAY, NELT, DIR, NCYCLE, ARRAY ) */

/*         will produce correct results, while */

/*            CALL CYCLAC ( ARRAY, NELT-3, DIR, NCYCLE, ARRAY(4) ) */

/*         will produce garbage. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     H.A. Neilan        (JPL) */
/*     B.V. Semenov       (JPL) */
/*     W.L. Taber         (JPL) */
/*     I.M. Underwood     (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 20-AUG-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 1.0.2, 18-MAY-2010 (BVS) */

/*        Removed "C$" marker from text in the header. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*         Comment section for permuted index source lines was added */
/*         following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU) (WLT) (HAN) */

/* -& */
/* $ Index_Entries */

/*     cycle the elements of a character array */

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
	chkin_("CYCLAC", (ftnlen)6);
    }

/*     Don't even screw around if there are no elements in the array. */

    if (*nelt < 1) {
	chkout_("CYCLAC", (ftnlen)6);
	return 0;
    }

/*     A backward cycle is the same as a forward cycle by the opposite */
/*     of NCYCLE.  Moreover a cycle by K is the same as a cycle by */
/*     K + m*N for any integer m.  Thus we compute the value of the */
/*     minimum forward right cycle that is equivalent to the inputs. */
/*     If the cycling direction is not recognized, signal an error. */

    if (*(unsigned char *)dir == 'B' || *(unsigned char *)dir == 'b') {
	k = -(*ncycle) % *nelt;
    } else if (*(unsigned char *)dir == 'F' || *(unsigned char *)dir == 'f') {
	k = *ncycle % *nelt;
    } else {
	setmsg_("Cycling direction was *.", (ftnlen)24);
	errch_("*", dir, (ftnlen)1, (ftnlen)1);
	sigerr_("SPICE(INVALIDDIRECTION)", (ftnlen)23);
	chkout_("CYCLAC", (ftnlen)6);
	return 0;
    }
    if (k < 0) {
	k += *nelt;
    } else if (k == 0) {
	movec_(array, nelt, out, array_len, out_len);
	chkout_("CYCLAC", (ftnlen)6);
	return 0;
    }

/*     The algorithm used to cycle arrays is identical to the one used */
/*     to cycle character strings in CYCLEC. We won't repeat the (rather */
/*     lengthy) description here. */

/*     The character version of CYCLAx differs from the other */
/*     versions in that a single character is cycled at a time. That */
/*     is, the first trip through the outermost loop cycles the first */
/*     characters of the array elements; the second trip cycles the */
/*     second characters; and so on. This allows the same algorithm to */
/*     be used for all the routines. The local storage required is just */
/*     a couple of characters. */


/*     Don't swap the ends of strings if they're just blank padded. */
/*     And don't overwrite the elements of the output array, if they */
/*     happen to be shorter thAn those in the input array. */

    outlen = i_len(out, out_len);
    widest = nbwid_(array, nelt, array_len);
    limit = min(outlen,widest);

/*     The greatest common divisor need only be computed once. */

    g = gcd_(&k, nelt);
    m = *nelt / g;

/*     To make this a non-character routine, remove all references to C. */

    i__1 = limit;
    for (c__ = 1; c__ <= i__1; ++c__) {
	i__2 = g;
	for (i__ = 1; i__ <= i__2; ++i__) {
	    l = i__;
	    *(unsigned char *)last = *(unsigned char *)&array[(l - 1) * 
		    array_len + (c__ - 1)];
	    i__3 = m;
	    for (j = 1; j <= i__3; ++j) {
		l += k;
		if (l > *nelt) {
		    l -= *nelt;
		}
		*(unsigned char *)temp = *(unsigned char *)&array[(l - 1) * 
			array_len + (c__ - 1)];
		*(unsigned char *)&out[(l - 1) * out_len + (c__ - 1)] = *(
			unsigned char *)last;
		*(unsigned char *)last = *(unsigned char *)temp;
	    }
	}
    }

/*     If needed, pad the output array with blanks. */

    if (outlen > limit) {
	i__1 = *nelt;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    i__2 = limit;
	    s_copy(out + ((i__ - 1) * out_len + i__2), " ", out_len - i__2, (
		    ftnlen)1);
	}
    }
    chkout_("CYCLAC", (ftnlen)6);
    return 0;
} /* cyclac_ */

