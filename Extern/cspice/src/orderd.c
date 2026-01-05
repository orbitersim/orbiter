/* orderd.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure ORDERD ( Order of a double precision array ) */
/* Subroutine */ int orderd_(doublereal *array, integer *ndim, integer *
	iorder)
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    integer i__, j;
    extern /* Subroutine */ int swapi_(integer *, integer *);
    integer jg, gap;

/* $ Abstract */

/*     Determine the order of elements in a double precision array. */

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
/*     SORT */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     ARRAY      I    Input array. */
/*     NDIM       I    Dimension of ARRAY. */
/*     IORDER     O    Order vector for ARRAY. */

/* $ Detailed_Input */

/*     ARRAY    is the input array. */

/*     NDIM     is the number of elements in the input array. */

/* $ Detailed_Output */

/*     IORDER   is the order vector for the input array. */
/*              IORDER(1) is the index of the smallest element */
/*              of ARRAY; IORDER(2) is the index of the next */
/*              smallest; and so on. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/*     1)  A negative input dimension causes this routine to leave the */
/*         output order vector unchanged. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     ORDERD finds the index of the smallest element of the input */
/*     array. This becomes the first element of the order vector. */
/*     The process is repeated for the rest of the elements. */

/*     The order vector returned by ORDERD may be used by any of */
/*     the REORD routines to sort sets of related arrays, as shown */
/*     in the example below. */

/* $ Examples */

/*     In the following example, the ORDER and REORD routines are */
/*     used to sort four related arrays (containing the names, */
/*     masses, integer ID codes, and visual magnitudes for a group */
/*     of satellites). This is representative of the typical use of */
/*     these routines. */

/*            C */
/*            C     Sort the object arrays by visual magnitude. */
/*            C */
/*                  CALL ORDERD ( VMAGS,  N, IORDER ) */

/*                  CALL REORDC ( IORDER, N, NAMES ) */
/*                  CALL REORDD ( IORDER, N, MASSES ) */
/*                  CALL REORDI ( IORDER, N, CODES ) */
/*                  CALL REORDR ( IORDER, N, VMAGS ) */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     W.L. Taber         (JPL) */
/*     I.M. Underwood     (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 04-JUL-2021 (JDR) */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 1.0.2, 23-MAR-2010 (NJB) */

/*        Header example was updated to show use of this routine. */
/*        $Exceptions section was updated. Header sections were */
/*        re-ordered. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU) */

/* -& */
/* $ Index_Entries */

/*     order of a d.p. array */

/* -& */

/*     Local variables */


/*     Begin with the initial ordering. */

    i__1 = *ndim;
    for (i__ = 1; i__ <= i__1; ++i__) {
	iorder[i__ - 1] = i__;
    }

/*     Find the smallest element, then the next smallest, and so on. */
/*     This uses the Shell Sort algorithm, but swaps the elements of */
/*     the order vector instead of the array itself. */

    gap = *ndim / 2;
    while(gap > 0) {
	i__1 = *ndim;
	for (i__ = gap + 1; i__ <= i__1; ++i__) {
	    j = i__ - gap;
	    while(j > 0) {
		jg = j + gap;
		if (array[iorder[j - 1] - 1] <= array[iorder[jg - 1] - 1]) {
		    j = 0;
		} else {
		    swapi_(&iorder[j - 1], &iorder[jg - 1]);
		}
		j -= gap;
	    }
	}
	gap /= 2;
    }
    return 0;
} /* orderd_ */

