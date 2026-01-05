/* invert.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static doublereal c_b2 = 0.;
static integer c__9 = 9;

/* $Procedure INVERT ( Invert a 3x3 matrix ) */
/* Subroutine */ int invert_(doublereal *m, doublereal *mout)
{
    doublereal mdet;
    extern /* Subroutine */ int filld_(doublereal *, integer *, doublereal *),
	     vsclg_(doublereal *, doublereal *, integer *, doublereal *);
    doublereal mtemp[9]	/* was [3][3] */, invdet;
    extern doublereal det_(doublereal *);

/* $ Abstract */

/*     Generate the inverse of a 3x3 matrix. */

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

/*     MATH */
/*     MATRIX */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     M          I   Matrix to be inverted. */
/*     MOUT       O   Inverted matrix (M)**-1. If M is singular, then */
/*                    MOUT will be the zero matrix. */

/* $ Detailed_Input */

/*     M        is an arbitrary 3x3 matrix. The limits on the size of */
/*              elements of M are determined by the process of */
/*              calculating the cofactors of each element of the matrix. */
/*              For a 3x3 matrix this amounts to the differencing of two */
/*              terms, each of which consists of the multiplication of */
/*              two matrix elements. This multiplication must not exceed */
/*              the range of double precision numbers or else an overflow */
/*              error will occur. */

/* $ Detailed_Output */

/*     MOUT     is the inverse of M and is calculated explicitly using */
/*              the matrix of cofactors. MOUT is set to be the zero */
/*              matrix if M is singular. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/*     1)  If M is singular, MOUT is set to be the zero matrix. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     First the determinant is explicitly calculated using the */
/*     fundamental definition of the determinant. If this value is less */
/*     that 10**-16 then the matrix is deemed to be singular and the */
/*     output value is filled with zeros. Otherwise, the output matrix */
/*     is calculated an element at a time by generating the cofactor of */
/*     each element. Finally, each element in the matrix of cofactors */
/*     is multiplied by the reciprocal of the determinant and the result */
/*     is the inverse of the original matrix. */

/*     NO INTERNAL CHECKING ON THE INPUT MATRIX M IS PERFORMED EXCEPT */
/*     ON THE SIZE OF ITS DETERMINANT.  THUS IT IS POSSIBLE TO GENERATE */
/*     A FLOATING POINT OVERFLOW OR UNDERFLOW IN THE PROCESS OF */
/*     CALCULATING THE MATRIX OF COFACTORS. */

/* $ Examples */

/*     The numerical results shown for this example may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */

/*     1) Given a double precision 3x3 matrix, compute its inverse. Check */
/*        that the original matrix times the computed inverse produces */
/*        the identity matrix. */

/*        Example code begins here. */


/*              PROGRAM INVERT_EX1 */
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
/*              CALL INVERT ( M, MOUT ) */

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
/*               0.0000000       2.0000000      -0.0000000 */
/*              -1.0000000       0.0000000      -0.0000000 */
/*               0.0000000      -0.0000000       1.0000000 */

/*         Original times inverse: */
/*               1.0000000       0.0000000       0.0000000 */
/*               0.0000000       1.0000000       0.0000000 */
/*               0.0000000       0.0000000       1.0000000 */


/* $ Restrictions */

/*     1)  The input matrix must be such that generating the cofactors */
/*         will not cause a floating point overflow or underflow. The */
/*         strictness of this condition depends, of course, on the */
/*         computer installation and the resultant maximum and minimum */
/*         values of double precision numbers. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     W.M. Owen          (JPL) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 06-JUL-2021 (JDR) */

/*        Changed input argument name M1 to M for consistency with other */
/*        routines. */

/*        Added IMPLICIT NONE statement. */

/*        Updated the header to comply with NAIF standard. Added */
/*        complete code example to $Examples section. */

/* -    SPICELIB Version 1.0.2, 22-APR-2010 (NJB) */

/*        Header correction: assertions that the output */
/*        can overwrite the input have been removed. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (WMO) */

/* -& */
/* $ Index_Entries */

/*     invert a 3x3_matrix */

/* -& */

/*     Local variables */


/*  Find the determinant of M and check for singularity */

    mdet = det_(m);
    if (abs(mdet) < 1e-16) {
	filld_(&c_b2, &c__9, mout);
	return 0;
    }

/*  Get the cofactors of each element of M */

    mtemp[0] = m[4] * m[8] - m[5] * m[7];
    mtemp[3] = -(m[3] * m[8] - m[5] * m[6]);
    mtemp[6] = m[3] * m[7] - m[4] * m[6];
    mtemp[1] = -(m[1] * m[8] - m[2] * m[7]);
    mtemp[4] = m[0] * m[8] - m[2] * m[6];
    mtemp[7] = -(m[0] * m[7] - m[1] * m[6]);
    mtemp[2] = m[1] * m[5] - m[2] * m[4];
    mtemp[5] = -(m[0] * m[5] - m[2] * m[3]);
    mtemp[8] = m[0] * m[4] - m[1] * m[3];

/*  Multiply the cofactor matrix by 1/MDET to obtain the inverse */

    invdet = 1. / mdet;
    vsclg_(&invdet, mtemp, &c__9, mout);

    return 0;
} /* invert_ */

