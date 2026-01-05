/* zzrxr.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure      ZZRXR ( Multiply sequence of 3x3 matrices ) */
/* Subroutine */ int zzrxr_(doublereal *matrix, integer *n, doublereal *
	output)
{
    /* System generated locals */
    integer i__1, i__2, i__3, i__4, i__5;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer);

    /* Local variables */
    integer incr;
    doublereal temp[18]	/* was [3][3][2] */;
    integer i__, j, k;
    extern /* Subroutine */ int ident_(doublereal *);
    integer get, put;

/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines.  Users should not call this routine directly due */
/*     to the volatile nature of this routine. */

/*     This routine multiplies together a sequence of state */
/*     transformation matrices. */

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

/*     PRIVATE */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     MATRIX     I   A sequence of state transformation matrices */
/*     N          I   The number of 3x3  matrices */
/*     OUTPUT     O   The product of the 3x3 matrices. */

/* $ Detailed_Input */

/*     MATRIX      is an array of 3x3  matrices. */

/*     N           is an integer giving the number of matrices in the */
/*                 sequence. */


/* $ Detailed_Output */

/*     OUTPUT      is the product of the matrices stored in MATRIX. */
/*                 Specifically, it is the result of the product */

/*                       M_N * M_(N-1) * ... * M_2 * M_1 */

/*                 where the K'th matrix M_K is define by the */
/*                 relationship */

/*                    M_K( I, J )  = MATRIX ( I, J, K ) */


/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/*     1) If N is 0 or smaller OUTPUT will be returned as the */
/*        3x36 identity matrix. */

/*     2) IF N is 1 OUTPUT will be returned as M_1 where M_1 is */
/*        the matrix defined above in the description of OUTPUT. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This is a private SPICE routine that computes the product */
/*     of a sequence of 3x3  matrices. */

/* $ Examples */

/*     None. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     W.L. Taber      (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.1, 03-OCT-2021 (NJB) */

/*        Reordered header sections. Corrected typo in comments. */

/* -    SPICELIB Version 1.0.0, 03-MAR-1999 (WLT) */


/* -& */

/*    If we have more than 2 matrices to deal with we will need to */
/*    set up the PUT location */

    put = 1;

/*     We perform tests in the order they seem most likely to */
/*     occur. */

    if (*n == 2) {

/*        If there are exactly two inputs, then the output takes */
/*        only a single matrix multiply. */

	for (j = 1; j <= 3; ++j) {
	    for (k = 1; k <= 3; ++k) {
		output[(i__1 = j + k * 3 - 4) < 9 && 0 <= i__1 ? i__1 : 
			s_rnge("output", i__1, "zzrxr_", (ftnlen)162)] = 
			matrix[j + 8] * matrix[(k + 3) * 3 - 12] + matrix[j + 
			11] * matrix[(k + 3) * 3 - 11] + matrix[j + 14] * 
			matrix[(k + 3) * 3 - 10];
	    }
	}
    } else if (*n > 2) {

/*        We need to compute the product */

/*           MATRIX( , ,N) * MATRIX( , ,N-1) * ... * MATRIX( , , 1 ) */

/*        Compute the first product.  MATRIX( , ,2) * MATRIX( , ,1) */


/*        First compute the upper left hand 3x3 portion of the product... */

	for (j = 1; j <= 3; ++j) {
	    for (k = 1; k <= 3; ++k) {
		temp[(i__1 = j + (k + put * 3) * 3 - 13) < 18 && 0 <= i__1 ? 
			i__1 : s_rnge("temp", i__1, "zzrxr_", (ftnlen)184)] = 
			matrix[j + 8] * matrix[(k + 3) * 3 - 12] + matrix[j + 
			11] * matrix[(k + 3) * 3 - 11] + matrix[j + 14] * 
			matrix[(k + 3) * 3 - 10];
	    }
	}

/*        Now continue building the product.  Note we will toggle */
/*        back and forth from TEMP(,,1) to TEMP(,,2) for storing */
/*        (PUTting) the results of our computations.  This way we */
/*        don't have to spend time moving any of the our computation */
/*        results to get ready for the next product.  See the end */
/*        of the loop below (keeping mind the next three values) to */
/*        see the little trick that's used to toggle back and forth. */

	incr = -1;
	put = 2;
	get = 1;
	i__1 = *n - 1;
	for (i__ = 3; i__ <= i__1; ++i__) {

/*           First the upper left hand portion of the product. */

	    for (j = 1; j <= 3; ++j) {
		for (k = 1; k <= 3; ++k) {
		    temp[(i__2 = j + (k + put * 3) * 3 - 13) < 18 && 0 <= 
			    i__2 ? i__2 : s_rnge("temp", i__2, "zzrxr_", (
			    ftnlen)211)] = matrix[j + (i__ * 3 + 1) * 3 - 13] 
			    * temp[(i__3 = (k + get * 3) * 3 - 12) < 18 && 0 
			    <= i__3 ? i__3 : s_rnge("temp", i__3, "zzrxr_", (
			    ftnlen)211)] + matrix[j + (i__ * 3 + 2) * 3 - 13] 
			    * temp[(i__4 = (k + get * 3) * 3 - 11) < 18 && 0 
			    <= i__4 ? i__4 : s_rnge("temp", i__4, "zzrxr_", (
			    ftnlen)211)] + matrix[j + (i__ * 3 + 3) * 3 - 13] 
			    * temp[(i__5 = (k + get * 3) * 3 - 10) < 18 && 0 
			    <= i__5 ? i__5 : s_rnge("temp", i__5, "zzrxr_", (
			    ftnlen)211)];
		}
	    }

/*           And as before, we don't need to compute the upper right */
/*           or lower right hand 3x3 portions of the matrix. So */
/*           we just skip them.  Toggle GET and PUT so we will */
/*           be ready for the next pass. */

	    get = put;
	    put += incr;
	    incr = -incr;
	}

/*        Finally compute the last product.  First the upper */
/*        left hand portion of the product. */

	for (j = 1; j <= 3; ++j) {
	    for (k = 1; k <= 3; ++k) {
		output[(i__1 = j + k * 3 - 4) < 9 && 0 <= i__1 ? i__1 : 
			s_rnge("output", i__1, "zzrxr_", (ftnlen)238)] = 
			matrix[j + (*n * 3 + 1) * 3 - 13] * temp[(i__2 = (k + 
			get * 3) * 3 - 12) < 18 && 0 <= i__2 ? i__2 : s_rnge(
			"temp", i__2, "zzrxr_", (ftnlen)238)] + matrix[j + (*
			n * 3 + 2) * 3 - 13] * temp[(i__3 = (k + get * 3) * 3 
			- 11) < 18 && 0 <= i__3 ? i__3 : s_rnge("temp", i__3, 
			"zzrxr_", (ftnlen)238)] + matrix[j + (*n * 3 + 3) * 3 
			- 13] * temp[(i__4 = (k + get * 3) * 3 - 10) < 18 && 
			0 <= i__4 ? i__4 : s_rnge("temp", i__4, "zzrxr_", (
			ftnlen)238)];
	    }
	}
    } else if (*n == 1) {

/*        If there is only one matrix in the list the output is */
/*        simply the input. */

	for (i__ = 1; i__ <= 3; ++i__) {
	    for (j = 1; j <= 3; ++j) {
		output[(i__1 = j + i__ * 3 - 4) < 9 && 0 <= i__1 ? i__1 : 
			s_rnge("output", i__1, "zzrxr_", (ftnlen)252)] = 
			matrix[j + (i__ + 3) * 3 - 13];
	    }
	}
    } else if (*n <= 0) {
	ident_(output);
    }
    return 0;
} /* zzrxr_ */

