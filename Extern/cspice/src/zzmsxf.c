/* zzmsxf.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure      ZZMSXF ( Multiply sequence of state transformations ) */
/* Subroutine */ int zzmsxf_(doublereal *matrix, integer *n, doublereal *
	output)
{
    /* System generated locals */
    integer i__1, i__2, i__3, i__4, i__5, i__6, i__7, i__8;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer);

    /* Local variables */
    integer incr;
    doublereal temp[72]	/* was [6][6][2] */;
    integer i__, j, k, l, m, get, put;

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
/*     N          I   The number of state transformation matrices */
/*     OUTPUT     O   The product of the state transformations. */

/* $ Detailed_Input */

/*     MATRIX      is an array of 6x6 state transformation matrices. */
/*                 It is essential that all these matrices have the form */

/*                    -            - */
/*                   |      |       | */
/*                   |   R  |   0   | */
/*                   |      |       | */
/*                   | -----+------ | */
/*                   |      |       | */
/*                   |   D  |   R   | */
/*                   |      |       | */
/*                    -            - */

/*                 The routine does not compute the product of a sequence */
/*                 that does not satisfy this condition. */


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
/*        6x6 identity matrix. */

/*     2) IF N is 1 OUTPUT will be returned as M_1 where M_1 is */
/*        the matrix defined above in the description of OUTPUT. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This is a private SPICE routine that computes the product */
/*     of a sequence of state transformation matrices. */

/*     This routine takes special advantage of the structure of */
/*     state transformation matrices so that the number of */
/*     actual multiplies and additions is reduced to 3/8 of that */
/*     which would be needed by a general matrix multiplication */
/*     routine. */

/*     The key to this computation saving is the structure of the */
/*     state transformation matrix.  Suppose that M2 and M1 are */
/*     two such matrices.  Then the product */

/*         -            -    -            - */
/*        |      |       |  |      |       | */
/*        |   R2 |   0   |  |   R1 |   0   | */
/*        |      |       |  |      |       | */
/*        | -----+------ |  | -----+------ |  = */
/*        |      |       |  |      |       | */
/*        |   D2 |   R2  |  |   D1 |   R1  | */
/*        |      |       |  |      |       | */
/*         -            -    -            - */

/*         -                              - */
/*        |                  |             | */
/*        |   R2*R1          |     0       | */
/*        |                  |             | */
/*        | -----------------+------------ | */
/*        |                  |             | */
/*        |   D2*R1 + R2*D1  |   R2*R1     | */
/*        |                  |             | */
/*         -                              - */

/*     As can be seen this can be computed with 3 3x3 matrix multiplies */
/*     and one 3x3 matrix addition. */

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

/* -    SPICELIB Version 1.0.0, 19-SEP-1995 (WLT) */

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
		output[(i__1 = j + k * 6 - 7) < 36 && 0 <= i__1 ? i__1 : 
			s_rnge("output", i__1, "zzmsxf_", (ftnlen)211)] = 
			matrix[j + 35] * matrix[(k + 6) * 6 - 42] + matrix[j 
			+ 41] * matrix[(k + 6) * 6 - 41] + matrix[j + 47] * 
			matrix[(k + 6) * 6 - 40];
	    }
	}
	for (j = 4; j <= 6; ++j) {
	    for (k = 1; k <= 3; ++k) {
		output[(i__1 = j + k * 6 - 7) < 36 && 0 <= i__1 ? i__1 : 
			s_rnge("output", i__1, "zzmsxf_", (ftnlen)220)] = 
			matrix[j + 35] * matrix[(k + 6) * 6 - 42] + matrix[j 
			+ 41] * matrix[(k + 6) * 6 - 41] + matrix[j + 47] * 
			matrix[(k + 6) * 6 - 40] + matrix[j + 53] * matrix[(k 
			+ 6) * 6 - 39] + matrix[j + 59] * matrix[(k + 6) * 6 
			- 38] + matrix[j + 65] * matrix[(k + 6) * 6 - 37];
	    }
	}
	for (j = 1; j <= 3; ++j) {
	    for (k = 4; k <= 6; ++k) {
		output[(i__1 = j + k * 6 - 7) < 36 && 0 <= i__1 ? i__1 : 
			s_rnge("output", i__1, "zzmsxf_", (ftnlen)231)] = 0.;
	    }
	}
	for (j = 4; j <= 6; ++j) {
	    l = j - 3;
	    for (k = 4; k <= 6; ++k) {
		m = k - 3;
		output[(i__1 = j + k * 6 - 7) < 36 && 0 <= i__1 ? i__1 : 
			s_rnge("output", i__1, "zzmsxf_", (ftnlen)239)] = 
			output[(i__2 = l + m * 6 - 7) < 36 && 0 <= i__2 ? 
			i__2 : s_rnge("output", i__2, "zzmsxf_", (ftnlen)239)]
			;
	    }
	}
    } else if (*n > 2) {

/*        We need to compute the product */

/*           MATRIX( , ,N) * MATRIX( , ,N-1) * ... * MATRIX( , , 1 ) */

/*        Compute the first product.  MATRIX( , ,2) * MATRIX( , ,1) */


/*        First compute the upper left hand 3x3 portion of the product... */

	for (j = 1; j <= 3; ++j) {
	    for (k = 1; k <= 3; ++k) {
		temp[(i__1 = j + (k + put * 6) * 6 - 43) < 72 && 0 <= i__1 ? 
			i__1 : s_rnge("temp", i__1, "zzmsxf_", (ftnlen)260)] =
			 matrix[j + 35] * matrix[(k + 6) * 6 - 42] + matrix[j 
			+ 41] * matrix[(k + 6) * 6 - 41] + matrix[j + 47] * 
			matrix[(k + 6) * 6 - 40];
	    }
	}

/*        Next compute the lower left hand 3x3 portion of the product. */

	for (j = 4; j <= 6; ++j) {
	    for (k = 1; k <= 3; ++k) {
		temp[(i__1 = j + (k + put * 6) * 6 - 43) < 72 && 0 <= i__1 ? 
			i__1 : s_rnge("temp", i__1, "zzmsxf_", (ftnlen)271)] =
			 matrix[j + 35] * matrix[(k + 6) * 6 - 42] + matrix[j 
			+ 41] * matrix[(k + 6) * 6 - 41] + matrix[j + 47] * 
			matrix[(k + 6) * 6 - 40] + matrix[j + 53] * matrix[(k 
			+ 6) * 6 - 39] + matrix[j + 59] * matrix[(k + 6) * 6 
			- 38] + matrix[j + 65] * matrix[(k + 6) * 6 - 37];
	    }
	}

/*        We don't bother to compute the upper right hand 3x3 portion */
/*        of the matrix since it is always zero. */

/*        Finally we could copy the lower right hand 3x3 portion of the */
/*        product from the upper left hand portion.  But as you can */
/*        see below we never actually have to reference TEMP(I,K,GET) */
/*        for K = 4 to 6.  So we can just skip that part of the */
/*        computation. */


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
		    temp[(i__2 = j + (k + put * 6) * 6 - 43) < 72 && 0 <= 
			    i__2 ? i__2 : s_rnge("temp", i__2, "zzmsxf_", (
			    ftnlen)309)] = matrix[j + (i__ * 6 + 1) * 6 - 43] 
			    * temp[(i__3 = (k + get * 6) * 6 - 42) < 72 && 0 
			    <= i__3 ? i__3 : s_rnge("temp", i__3, "zzmsxf_", (
			    ftnlen)309)] + matrix[j + (i__ * 6 + 2) * 6 - 43] 
			    * temp[(i__4 = (k + get * 6) * 6 - 41) < 72 && 0 
			    <= i__4 ? i__4 : s_rnge("temp", i__4, "zzmsxf_", (
			    ftnlen)309)] + matrix[j + (i__ * 6 + 3) * 6 - 43] 
			    * temp[(i__5 = (k + get * 6) * 6 - 40) < 72 && 0 
			    <= i__5 ? i__5 : s_rnge("temp", i__5, "zzmsxf_", (
			    ftnlen)309)];
		}
	    }

/*           Next the lower left hand portion of the product. */

	    for (j = 4; j <= 6; ++j) {
		for (k = 1; k <= 3; ++k) {
		    temp[(i__2 = j + (k + put * 6) * 6 - 43) < 72 && 0 <= 
			    i__2 ? i__2 : s_rnge("temp", i__2, "zzmsxf_", (
			    ftnlen)321)] = matrix[j + (i__ * 6 + 1) * 6 - 43] 
			    * temp[(i__3 = (k + get * 6) * 6 - 42) < 72 && 0 
			    <= i__3 ? i__3 : s_rnge("temp", i__3, "zzmsxf_", (
			    ftnlen)321)] + matrix[j + (i__ * 6 + 2) * 6 - 43] 
			    * temp[(i__4 = (k + get * 6) * 6 - 41) < 72 && 0 
			    <= i__4 ? i__4 : s_rnge("temp", i__4, "zzmsxf_", (
			    ftnlen)321)] + matrix[j + (i__ * 6 + 3) * 6 - 43] 
			    * temp[(i__5 = (k + get * 6) * 6 - 40) < 72 && 0 
			    <= i__5 ? i__5 : s_rnge("temp", i__5, "zzmsxf_", (
			    ftnlen)321)] + matrix[j + (i__ * 6 + 4) * 6 - 43] 
			    * temp[(i__6 = (k + get * 6) * 6 - 39) < 72 && 0 
			    <= i__6 ? i__6 : s_rnge("temp", i__6, "zzmsxf_", (
			    ftnlen)321)] + matrix[j + (i__ * 6 + 5) * 6 - 43] 
			    * temp[(i__7 = (k + get * 6) * 6 - 38) < 72 && 0 
			    <= i__7 ? i__7 : s_rnge("temp", i__7, "zzmsxf_", (
			    ftnlen)321)] + matrix[j + (i__ * 6 + 6) * 6 - 43] 
			    * temp[(i__8 = (k + get * 6) * 6 - 37) < 72 && 0 
			    <= i__8 ? i__8 : s_rnge("temp", i__8, "zzmsxf_", (
			    ftnlen)321)];
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
		output[(i__1 = j + k * 6 - 7) < 36 && 0 <= i__1 ? i__1 : 
			s_rnge("output", i__1, "zzmsxf_", (ftnlen)350)] = 
			matrix[j + (*n * 6 + 1) * 6 - 43] * temp[(i__2 = (k + 
			get * 6) * 6 - 42) < 72 && 0 <= i__2 ? i__2 : s_rnge(
			"temp", i__2, "zzmsxf_", (ftnlen)350)] + matrix[j + (*
			n * 6 + 2) * 6 - 43] * temp[(i__3 = (k + get * 6) * 6 
			- 41) < 72 && 0 <= i__3 ? i__3 : s_rnge("temp", i__3, 
			"zzmsxf_", (ftnlen)350)] + matrix[j + (*n * 6 + 3) * 
			6 - 43] * temp[(i__4 = (k + get * 6) * 6 - 40) < 72 &&
			 0 <= i__4 ? i__4 : s_rnge("temp", i__4, "zzmsxf_", (
			ftnlen)350)];
	    }
	}

/*        The lower left hand portion of the product. */

	for (j = 4; j <= 6; ++j) {
	    for (k = 1; k <= 3; ++k) {
		output[(i__1 = j + k * 6 - 7) < 36 && 0 <= i__1 ? i__1 : 
			s_rnge("output", i__1, "zzmsxf_", (ftnlen)361)] = 
			matrix[j + (*n * 6 + 1) * 6 - 43] * temp[(i__2 = (k + 
			get * 6) * 6 - 42) < 72 && 0 <= i__2 ? i__2 : s_rnge(
			"temp", i__2, "zzmsxf_", (ftnlen)361)] + matrix[j + (*
			n * 6 + 2) * 6 - 43] * temp[(i__3 = (k + get * 6) * 6 
			- 41) < 72 && 0 <= i__3 ? i__3 : s_rnge("temp", i__3, 
			"zzmsxf_", (ftnlen)361)] + matrix[j + (*n * 6 + 3) * 
			6 - 43] * temp[(i__4 = (k + get * 6) * 6 - 40) < 72 &&
			 0 <= i__4 ? i__4 : s_rnge("temp", i__4, "zzmsxf_", (
			ftnlen)361)] + matrix[j + (*n * 6 + 4) * 6 - 43] * 
			temp[(i__5 = (k + get * 6) * 6 - 39) < 72 && 0 <= 
			i__5 ? i__5 : s_rnge("temp", i__5, "zzmsxf_", (ftnlen)
			361)] + matrix[j + (*n * 6 + 5) * 6 - 43] * temp[(
			i__6 = (k + get * 6) * 6 - 38) < 72 && 0 <= i__6 ? 
			i__6 : s_rnge("temp", i__6, "zzmsxf_", (ftnlen)361)] 
			+ matrix[j + (*n * 6 + 6) * 6 - 43] * temp[(i__7 = (k 
			+ get * 6) * 6 - 37) < 72 && 0 <= i__7 ? i__7 : 
			s_rnge("temp", i__7, "zzmsxf_", (ftnlen)361)];
	    }
	}

/*        The upper right hand portion of the product is zero. */

	for (j = 1; j <= 3; ++j) {
	    for (k = 4; k <= 6; ++k) {
		output[(i__1 = j + k * 6 - 7) < 36 && 0 <= i__1 ? i__1 : 
			s_rnge("output", i__1, "zzmsxf_", (ftnlen)374)] = 0.;
	    }
	}

/*        The lower right hand portion of the product is a copy of */
/*        the upper left hand portion of the product. */

	for (j = 4; j <= 6; ++j) {
	    l = j - 3;
	    for (k = 4; k <= 6; ++k) {
		m = k - 3;
		output[(i__1 = j + k * 6 - 7) < 36 && 0 <= i__1 ? i__1 : 
			s_rnge("output", i__1, "zzmsxf_", (ftnlen)387)] = 
			output[(i__2 = l + m * 6 - 7) < 36 && 0 <= i__2 ? 
			i__2 : s_rnge("output", i__2, "zzmsxf_", (ftnlen)387)]
			;
	    }
	}
    } else if (*n == 1) {

/*        If there is only one matrix in the list the output is */
/*        simply the input. */

	for (i__ = 1; i__ <= 6; ++i__) {
	    for (j = 1; j <= 6; ++j) {
		output[(i__1 = j + i__ * 6 - 7) < 36 && 0 <= i__1 ? i__1 : 
			s_rnge("output", i__1, "zzmsxf_", (ftnlen)401)] = 
			matrix[j + (i__ + 6) * 6 - 43];
	    }
	}
    } else if (*n <= 0) {
	for (j = 1; j <= 6; ++j) {
	    output[(i__1 = j + j * 6 - 7) < 36 && 0 <= i__1 ? i__1 : s_rnge(
		    "output", i__1, "zzmsxf_", (ftnlen)410)] = 1.;
	    for (k = j + 1; k <= 6; ++k) {
		output[(i__1 = j + k * 6 - 7) < 36 && 0 <= i__1 ? i__1 : 
			s_rnge("output", i__1, "zzmsxf_", (ftnlen)413)] = 0.;
		output[(i__1 = k + j * 6 - 7) < 36 && 0 <= i__1 ? i__1 : 
			s_rnge("output", i__1, "zzmsxf_", (ftnlen)414)] = 0.;
	    }
	}
    }
    return 0;
} /* zzmsxf_ */

