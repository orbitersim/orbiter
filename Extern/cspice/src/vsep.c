/* vsep.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure VSEP  ( Angular separation of vectors, 3 dimensions ) */
doublereal vsep_(doublereal *v1, doublereal *v2)
{
    /* System generated locals */
    doublereal ret_val;

    /* Builtin functions */
    double asin(doublereal);

    /* Local variables */
    extern doublereal vdot_(doublereal *, doublereal *);
    doublereal dmag1, dmag2, vtemp[3];
    extern /* Subroutine */ int unorm_(doublereal *, doublereal *, doublereal 
	    *);
    extern doublereal vnorm_(doublereal *);
    doublereal u1[3], u2[3];
    extern doublereal pi_(void);

/* $ Abstract */

/*     Find the separation angle in radians between two double */
/*     precision, 3-dimensional vectors. This angle is defined as zero */
/*     if either vector is zero. */

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

/*     ANGLE */
/*     VECTOR */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     V1         I   First vector. */
/*     V2         I   Second vector. */

/*     The function returns the angle between V1 and V2 expressed in */
/*     radians. */

/* $ Detailed_Input */

/*     V1, */
/*     V2       are two double precision 3-dimensional vectors. Either */
/*              V1 or V2, or both, may be the zero vector. */

/*              An implicit assumption exists that V1 and V2 are */
/*              specified in the same reference frame. If this is not */
/*              the case, the numerical result of this routine has no */
/*              meaning. */

/* $ Detailed_Output */

/*     The function returns the angle between V1 and V2 expressed in */
/*     radians. */

/*     VSEP is strictly non-negative. If either V1 or V2 is the zero */
/*     vector, then VSEP is defined to be 0 radians. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     In the plane, it is a simple matter to calculate the angle */
/*     between two vectors once the two vectors have been made to be */
/*     unit length. Then, since the two vectors form the two equal */
/*     sides of an isosceles triangle, the length of the third side */
/*     is given by the expression */

/*           LENGTH = 2.0 * SIN ( VSEP/2.0 ) */

/*     The length is given by the magnitude of the difference of the */
/*     two unit vectors */

/*           LENGTH = NORM ( U1 - U2 ) */

/*     Once the length is found, the value of VSEP may be calculated */
/*     by inverting the first expression given above as */

/*           VSEP = 2.0 * ARCSIN ( LENGTH/2.0 ) */

/*     This expression becomes increasingly unstable when VSEP gets */
/*     larger than PI/2 radians or 90 degrees. In this situation (which */
/*     is easily detected by determining the sign of the dot product of */
/*     V1 and V2) the supplementary angle is calculated first and */
/*     then VSEP is given by */

/*           VSEP = PI - SUPPLEMENTARY_ANGLE */

/* $ Examples */

/*     The numerical results shown for this example may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */

/*     1) Define two sets of 3-dimensional vectors and compute the */
/*        angular separation between each vector in first set and the */
/*        corresponding vector in the second set. */


/*        Example code begins here. */


/*              PROGRAM VSEP_EX1 */
/*              IMPLICIT NONE */

/*        C */
/*        C     SPICELIB functions. */
/*        C */
/*              DOUBLE PRECISION      VSEP */

/*        C */
/*        C     Local parameters. */
/*        C */
/*              INTEGER               SETSIZ */
/*              PARAMETER           ( SETSIZ = 3 ) */

/*        C */
/*        C     Local variables. */
/*        C */
/*              DOUBLE PRECISION      V1   ( 3, SETSIZ ) */
/*              DOUBLE PRECISION      V2   ( 3, SETSIZ ) */

/*              INTEGER               I */
/*              INTEGER               J */

/*        C */
/*        C     Define the two vector sets. */
/*        C */
/*              DATA                  V1 / 1.D0,  0.D0,  0.D0, */
/*             .                           1.D0,  0.D0,  0.D0, */
/*             .                           3.D0,  0.D0,  0.D0  / */

/*              DATA                  V2 / 1.D0,  0.D0,  0.D0, */
/*             .                           0.D0,  1.D0,  0.D0, */
/*             .                          -5.D0,  0.D0,  0.D0  / */

/*        C */
/*        C     Calculate the angular separation between each pair */
/*        C     of vectors. */
/*        C */
/*              DO I=1, SETSIZ */

/*                 WRITE(*,'(A,3F6.1)')  'First vector            : ', */
/*             .                        ( V1(J,I), J=1,3 ) */
/*                 WRITE(*,'(A,3F6.1)')  'Second vector           : ', */
/*             .                        ( V2(J,I), J=1,3 ) */
/*                 WRITE(*,'(A,F15.10)') 'Angular separation (rad): ', */
/*             .                             VSEP ( V1(1,I), V2(1,I) ) */
/*                 WRITE(*,*) */

/*              END DO */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*        First vector            :    1.0   0.0   0.0 */
/*        Second vector           :    1.0   0.0   0.0 */
/*        Angular separation (rad):    0.0000000000 */

/*        First vector            :    1.0   0.0   0.0 */
/*        Second vector           :    0.0   1.0   0.0 */
/*        Angular separation (rad):    1.5707963268 */

/*        First vector            :    3.0   0.0   0.0 */
/*        Second vector           :   -5.0   0.0   0.0 */
/*        Angular separation (rad):    3.1415926536 */


/* $ Restrictions */

/*     1)  The user is required to insure that the input vectors will not */
/*         cause floating point overflow upon calculation of the vector */
/*         dot product since no error detection or correction code is */
/*         implemented. In practice, this is not a significant */
/*         restriction. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     K.R. Gehringer     (JPL) */
/*     W.M. Owen          (JPL) */
/*     W.L. Taber         (JPL) */
/*     E.D. Wright        (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.2.0, 05-JUL-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. Removed */
/*        unnecessary $Revisions section. */

/*        Added complete code example based on existing example. */

/* -    SPICELIB Version 1.1.1, 17-APR-2006 (EDW) */

/*        Typo correction to the value of PI/2 in the $Examples */
/*        section, 1.571 instead of 1.71. */

/* -    SPICELIB Version 1.1.0, 29-FEB-1996 (KRG) */

/*        The declaration for the SPICELIB function PI is now */
/*        preceded by an EXTERNAL statement declaring PI to be an */
/*        external function. This removes a conflict with any */
/*        compilers that have a PI intrinsic function. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (WMO) (WLT) */

/* -& */
/* $ Index_Entries */

/*     angular separation of 3-dimensional vectors */

/* -& */

/*     SPICELIB functions */


/*     Local Variables */

/*     The following declarations represent, respectively: */
/*        Magnitudes of V1, V2 */
/*        Either of the difference vectors: V1-V2 or V1-(-V2) */
/*        Unit vectors parallel to V1 and V2 */


/*  Calculate the magnitudes of V1 and V2; if either is 0, VSEP = 0 */

    unorm_(v1, u1, &dmag1);
    if (dmag1 == 0.) {
	ret_val = 0.;
	return ret_val;
    }
    unorm_(v2, u2, &dmag2);
    if (dmag2 == 0.) {
	ret_val = 0.;
	return ret_val;
    }
    if (vdot_(u1, u2) > 0.) {
	vtemp[0] = u1[0] - u2[0];
	vtemp[1] = u1[1] - u2[1];
	vtemp[2] = u1[2] - u2[2];
	ret_val = asin(vnorm_(vtemp) * .5) * 2.;
    } else if (vdot_(u1, u2) < 0.) {
	vtemp[0] = u1[0] + u2[0];
	vtemp[1] = u1[1] + u2[1];
	vtemp[2] = u1[2] + u2[2];
	ret_val = pi_() - asin(vnorm_(vtemp) * .5) * 2.;
    } else {
	ret_val = pi_() / 2.;
    }
    return ret_val;
} /* vsep_ */

