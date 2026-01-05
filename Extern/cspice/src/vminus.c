/* vminus.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure VMINUS ( Negate vector, "-V", 3 dimensions ) */
/* Subroutine */ int vminus_(doublereal *v1, doublereal *vout)
{
/* $ Abstract */

/*     Negate a double precision 3-dimensional vector. */

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
/*     V1         I   Vector to be negated. */
/*     VOUT       O   Negated vector -V1. */

/* $ Detailed_Input */

/*     V1       is any double precision 3-dimensional vector. */

/* $ Detailed_Output */

/*     VOUT     is the negation (additive inverse) of V1. It is a */
/*              double precision 3-dimensional vector. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     For each value of the index I from 1 to 3, VMINUS negates V1 */
/*     by the expression: */

/*        VOUT(I) = - V1(I) */

/*     No error checking is performed since overflow can occur ONLY if */
/*     the dynamic range of positive floating point numbers is not the */
/*     same size as the dynamic range of negative floating point numbers */
/*     AND at least one component of V1 falls outside the common range. */
/*     The likelihood of this occurring is so small as to be of no */
/*     concern. */

/* $ Examples */

/*     The numerical results shown for this example may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */

/*     1) Define a set of 3-dimensional vectors and negate each of them. */


/*        Example code begins here. */


/*              PROGRAM VMINUS_EX1 */
/*              IMPLICIT NONE */

/*        C */
/*        C     Local parameters. */
/*        C */
/*              INTEGER               SETSIZ */
/*              PARAMETER           ( SETSIZ = 2 ) */

/*        C */
/*        C     Local variables. */
/*        C */
/*              DOUBLE PRECISION      V1   ( 3, SETSIZ ) */
/*              DOUBLE PRECISION      VOUT ( 3         ) */

/*              INTEGER               I */
/*              INTEGER               J */

/*        C */
/*        C     Define a set of 3-dimensional vectors. */
/*        C */
/*              DATA                  V1  /  1.D0, -2.D0, 0.D0, */
/*             .                             0.D0,  0.D0, 0.D0  / */

/*        C */
/*        C     Negate each vector */
/*        C */
/*              DO I=1, SETSIZ */

/*                 CALL VMINUS ( V1(1,I), VOUT ) */

/*                 WRITE(*,'(A,3F6.1)') 'Input vector  : ', */
/*             .                        ( V1(J,I), J=1,3 ) */
/*                 WRITE(*,'(A,3F6.1)') 'Negated vector: ', VOUT */
/*                 WRITE(*,*) */

/*              END DO */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*        Input vector  :    1.0  -2.0   0.0 */
/*        Negated vector:   -1.0   2.0  -0.0 */

/*        Input vector  :    0.0   0.0   0.0 */
/*        Negated vector:   -0.0  -0.0  -0.0 */


/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     W.M. Owen          (JPL) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.3, 02-OCT-2021 (JDR) */

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

/*     negate a 3-dimensional vector */

/* -& */
    vout[0] = -v1[0];
    vout[1] = -v1[1];
    vout[2] = -v1[2];

    return 0;
} /* vminus_ */

