/* invort.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure INVORT ( Invert nearly orthogonal matrices ) */
/* Subroutine */ int invort_(doublereal *m, doublereal *mit)
{
    /* Initialized data */

    static logical first = TRUE_;

    /* System generated locals */
    integer i__1, i__2;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer);

    /* Local variables */
    doublereal temp[9]	/* was [3][3] */;
    integer i__;
    doublereal scale;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    static doublereal bound;
    extern doublereal dpmax_(void);
    extern /* Subroutine */ int errdp_(char *, doublereal *, ftnlen), xpose_(
	    doublereal *, doublereal *), unorm_(doublereal *, doublereal *, 
	    doublereal *);
    doublereal length;
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), vsclip_(doublereal *, doublereal *), setmsg_(char *, 
	    ftnlen), errint_(char *, integer *, ftnlen);

/* $ Abstract */

/*     Construct the inverse of a 3x3 matrix with orthogonal columns and */
/*     non-zero column norms using a numerically stable algorithm. The */
/*     rows of the output matrix are the columns of the input matrix */
/*     divided by the length squared of the corresponding columns. */

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

/*     MATRIX */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     M          I   A 3x3 matrix. */
/*     MIT        O   M after transposition and scaling of rows. */

/* $ Detailed_Input */

/*     M        is a 3x3 matrix. */

/* $ Detailed_Output */

/*     MIT      is the matrix obtained by transposing M and dividing */
/*              the rows by squares of their norms. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If any of the columns of M have zero length, the error */
/*         SPICE(ZEROLENGTHCOLUMN) is signaled. */

/*     2)  If any column is too short to allow computation of the */
/*         reciprocal of its length without causing a floating */
/*         point overflow, the error SPICE(COLUMNTOOSMALL) is signaled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     Suppose that M is the matrix */

/*            .-                      -. */
/*            |   A*u    B*v     C*w   | */
/*            |      1      1       1  | */
/*            |                        | */
/*            |   A*u    B*v     C*w   | */
/*            |      2      2       2  | */
/*            |                        | */
/*            |   A*u    B*v     C*w   | */
/*            |      3      3       3  | */
/*            `-                      -' */

/*     where the vectors (u , u , u ),  (v , v , v ),  and (w , w , w ) */
/*                         1   2   3      1   2   3          1   2   3 */
/*     are unit vectors. This routine produces the matrix: */


/*            .-                      -. */
/*            |   a*u    a*u     a*u   | */
/*            |      1      2       3  | */
/*            |                        | */
/*            |   b*v    b*v     b*v   | */
/*            |      1      2       3  | */
/*            |                        | */
/*            |   c*w    c*w     c*w   | */
/*            |      1      2       3  | */
/*            `-                      -' */

/*     where a = 1/A, b = 1/B, and c = 1/C. */

/* $ Examples */

/*     The numerical results shown for this example may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */

/*     1) Given a double precision 3x3 matrix with mutually orthogonal */
/*        rows of arbitrary length, compute its inverse. Check that the */
/*        original matrix times the computed inverse produces the */
/*        identity matrix. */

/*        Example code begins here. */


/*              PROGRAM INVORT_EX1 */
/*              IMPLICIT NONE */

/*        C */
/*        C     Local variables. */
/*        C */
/*              DOUBLE PRECISION      IMAT ( 3, 3 ) */
/*              DOUBLE PRECISION      M    ( 3, 3 ) */
/*              DOUBLE PRECISION      MOUT ( 3, 3 ) */

/*              INTEGER               I */
/*              INTEGER               J */

/*        C */
/*        C     Define a matrix to invert. */
/*        C */
/*              DATA                  M  /  0.D0,  0.5D0, 0.D0, */
/*             .                           -1.D0,  0.D0,  0.D0, */
/*             .                            0.D0,  0.D0,  1.D0 / */

/*              WRITE(*,*) 'Original Matrix:' */
/*              DO I=1, 3 */

/*                 WRITE(*,'(3F16.7)') ( M(I,J), J=1,3 ) */

/*              END DO */
/*        C */
/*        C     Invert the matrix, then output. */
/*        C */
/*              CALL INVORT ( M, MOUT ) */

/*              WRITE(*,*) ' ' */
/*              WRITE(*,*) 'Inverse Matrix:' */
/*              DO I=1, 3 */

/*                 WRITE(*,'(3F16.7)') ( MOUT(I,J), J=1,3 ) */

/*              END DO */

/*        C */
/*        C     Check the M times MOUT produces the identity matrix. */
/*        C */
/*              CALL MXM ( M, MOUT, IMAT ) */

/*              WRITE(*,*) ' ' */
/*              WRITE(*,*) 'Original times inverse:' */
/*              DO I=1, 3 */

/*                 WRITE(*,'(3F16.7)') ( IMAT(I,J), J=1,3 ) */

/*              END DO */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*         Original Matrix: */
/*               0.0000000      -1.0000000       0.0000000 */
/*               0.5000000       0.0000000       0.0000000 */
/*               0.0000000       0.0000000       1.0000000 */

/*         Inverse Matrix: */
/*               0.0000000       2.0000000       0.0000000 */
/*              -1.0000000       0.0000000       0.0000000 */
/*               0.0000000       0.0000000       1.0000000 */

/*         Original times inverse: */
/*               1.0000000       0.0000000       0.0000000 */
/*               0.0000000       1.0000000       0.0000000 */
/*               0.0000000       0.0000000       1.0000000 */


/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     W.L. Taber         (JPL) */
/*     E.D. Wright        (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.2.0, 26-OCT-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. Fixed I/O type */
/*        of argument MIT in $Brief_I/O table. Extended $Abstract */
/*        section. */

/*        Added complete code example to $Examples section. */

/* -    SPICELIB Version 1.1.1, 14-NOV-2013 (EDW) */

/*        Edit to $Abstract. Eliminated unneeded $Revisions section. */

/* -    SPICELIB Version 1.1.0, 02-SEP-2005 (NJB) */

/*        Updated to remove non-standard use of duplicate arguments */
/*        in VSCL call. */

/* -    SPICELIB Version 1.0.0, 02-JAN-2002 (WLT) */

/* -& */
/* $ Index_Entries */

/*     Transpose a matrix and invert the lengths of the rows */
/*     Invert a pseudo orthogonal matrix */

/* -& */

/*     SPICELIB functions */


/*     Local Variables */


/*     Saved variables */


/*     Initial values */


/*     Use discovery check-in. */


/*     The first time through, get a copy of DPMAX. */

    if (first) {
	bound = dpmax_();
	first = FALSE_;
    }

/*     For each column, construct a scaled copy. However, make sure */
/*     everything is do-able before trying something. */

    for (i__ = 1; i__ <= 3; ++i__) {
	unorm_(&m[(i__1 = i__ * 3 - 3) < 9 && 0 <= i__1 ? i__1 : s_rnge("m", 
		i__1, "invort_", (ftnlen)304)], &temp[(i__2 = i__ * 3 - 3) < 
		9 && 0 <= i__2 ? i__2 : s_rnge("temp", i__2, "invort_", (
		ftnlen)304)], &length);
	if (length == 0.) {
	    chkin_("INVORT", (ftnlen)6);
	    setmsg_("Column # of the input matrix has a norm of zero. ", (
		    ftnlen)49);
	    errint_("#", &i__, (ftnlen)1);
	    sigerr_("SPICE(ZEROLENGTHCOLUMN)", (ftnlen)23);
	    chkout_("INVORT", (ftnlen)6);
	    return 0;
	}

/*        Make sure we can actually rescale the rows. */

	if (length < 1.) {
	    if (length * bound < 1.) {
		chkin_("INVORT", (ftnlen)6);
		setmsg_("The length of column # is #. This number cannot be "
			"inverted.  For this reason, the scaled transpose of "
			"the input matrix cannot be formed. ", (ftnlen)138);
		errint_("#", &i__, (ftnlen)1);
		errdp_("#", &length, (ftnlen)1);
		sigerr_("SPICE(COLUMNTOOSMALL)", (ftnlen)21);
		chkout_("INVORT", (ftnlen)6);
		return 0;
	    }
	}
	scale = 1. / length;
	vsclip_(&scale, &temp[(i__1 = i__ * 3 - 3) < 9 && 0 <= i__1 ? i__1 : 
		s_rnge("temp", i__1, "invort_", (ftnlen)342)]);
    }

/*     If we make it this far, we just need to transpose TEMP into MIT. */

    xpose_(temp, mit);
    return 0;
} /* invort_ */

