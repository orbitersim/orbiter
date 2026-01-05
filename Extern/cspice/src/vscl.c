/* vscl.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure VSCL ( Vector scaling, 3 dimensions ) */
/* Subroutine */ int vscl_(doublereal *s, doublereal *v1, doublereal *vout)
{
/* $ Abstract */

/*     Multiply a scalar and a double precision 3-dimensional vector. */

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

/*     VECTOR */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     S          I   Scalar to multiply a vector. */
/*     V1         I   Vector to be multiplied. */
/*     VOUT       O   Product vector, S * V1. */

/* $ Detailed_Input */

/*     S        is a double precision scalar used to multiply the vector */
/*              V1. */

/*     V1       is a double precision 3-dimensional vector, which is to */
/*              be scaled by S. */

/* $ Detailed_Output */

/*     VOUT     is a double precision 3-dimensional vector containing */
/*              the product of the scalar with the vector V1. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     For each value of the index I from 1 to 3, this subroutine */
/*     performs the following multiplication */

/*        VOUT(I) = S * V1(I) */

/*     No error checking is performed to guard against numeric overflow */
/*     or underflow. */

/* $ Examples */

/*     The numerical results shown for this example may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */

/*     1) Define a sets of scalar double precision values and use them */
/*        to scale a given 3-dimensional vector. */


/*        Example code begins here. */


/*              PROGRAM VSCL_EX1 */
/*              IMPLICIT NONE */

/*        C */
/*        C     Local parameters. */
/*        C */
/*              INTEGER               SETSIZ */
/*              PARAMETER           ( SETSIZ = 3 ) */

/*        C */
/*        C     Local variables. */
/*        C */
/*              DOUBLE PRECISION      S    ( SETSIZ ) */
/*              DOUBLE PRECISION      V1   ( 3      ) */
/*              DOUBLE PRECISION      VOUT ( 3      ) */

/*              INTEGER               I */

/*        C */
/*        C     Define the set of scalars and the input vector. */
/*        C */
/*              DATA                  S    / 3.D0, 0.D0, -1.D0 / */

/*              DATA                  V1   / 1.D0, 2.D0, -3.D0 / */


/*              WRITE(*,'(A,3F6.1)') 'Input vector : ', V1 */
/*              WRITE(*,*) */

/*        C */
/*        C     Calculate product of each scalar and V1. */
/*        C */
/*              DO I=1, SETSIZ */

/*                 CALL VSCL ( S(I), V1, VOUT ) */

/*                 WRITE(*,'(A,F6.1)')  'Scale factor : ', S(I) */
/*                 WRITE(*,'(A,3F6.1)') 'Output vector: ', VOUT */
/*                 WRITE(*,*) */

/*              END DO */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*        Input vector :    1.0   2.0  -3.0 */

/*        Scale factor :    3.0 */
/*        Output vector:    3.0   6.0  -9.0 */

/*        Scale factor :    0.0 */
/*        Output vector:    0.0   0.0  -0.0 */

/*        Scale factor :   -1.0 */
/*        Output vector:   -1.0  -2.0   3.0 */


/* $ Restrictions */

/*     1)  The user is responsible for insuring that no floating point */
/*         overflow occurs from multiplying S by any component of V1. No */
/*         error recovery or reporting scheme is incorporated in this */
/*         subroutine. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     W.M. Owen          (JPL) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 06-JUL-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. Added complete */
/*        code example based on existing example. */

/* -    SPICELIB Version 1.0.2, 23-APR-2010 (NJB) */

/*        Header correction: assertions that the output */
/*        can overwrite the input have been removed. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (WMO) */

/* -& */
/* $ Index_Entries */

/*     3-dimensional vector scaling */

/* -& */
    vout[0] = *s * v1[0];
    vout[1] = *s * v1[1];
    vout[2] = *s * v1[2];
    return 0;
} /* vscl_ */

