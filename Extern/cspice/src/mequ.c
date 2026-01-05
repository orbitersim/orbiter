/* mequ.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__9 = 9;

/* $Procedure MEQU ( Matrix equal to another, 3x3 ) */
/* Subroutine */ int mequ_(doublereal *m1, doublereal *mout)
{
    extern /* Subroutine */ int moved_(doublereal *, integer *, doublereal *);

/* $ Abstract */

/*     Set one double precision 3x3 matrix equal to another. */

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

/*     ASSIGNMENT */
/*     MATRIX */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     M1         I   Input matrix. */
/*     MOUT       O   Output matrix equal to M1. */

/* $ Detailed_Input */

/*     M1       is an arbitrary input 3x3 matrix. There are no */
/*              restrictions on what it may contain. */

/* $ Detailed_Output */

/*     MOUT     is a 3x3 matrix set to be equal to M1. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     None. */

/* $ Examples */

/*     The numerical results shown for this example may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */

/*     1) This trivial example demonstrates how to use MEQU to assign */
/*        one matrix to another. */


/*        Example code begins here. */


/*              PROGRAM MEQU_EX1 */
/*              IMPLICIT NONE */

/*        C */
/*        C     Local variables. */
/*        C */
/*              DOUBLE PRECISION      M1   ( 3, 3 ) */
/*              DOUBLE PRECISION      MOUT ( 3, 3 ) */

/*              INTEGER               I */
/*              INTEGER               J */

/*        C */
/*        C     Define M1. */
/*        C */
/*              DATA                  M1   /  0.0D0,  1.0D0,  0.0D0, */
/*             .                             -1.0D0,  0.0D0,  0.0D0, */
/*             .                              0.0D0,  0.0D0,  1.0D0  / */

/*        C */
/*        C     Assign M1 to MOUT and print MOUT. */
/*        C */
/*              CALL MEQU ( M1, MOUT ) */

/*              WRITE(*,'(A)') 'MOUT:' */
/*              DO I=1, 3 */

/*                 WRITE(*,'(3F16.7)') ( MOUT(I,J), J=1,3 ) */

/*              END DO */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*        MOUT: */
/*               0.0000000      -1.0000000       0.0000000 */
/*               1.0000000       0.0000000       0.0000000 */
/*               0.0000000       0.0000000       1.0000000 */


/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     W.M. Owen          (JPL) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 04-JUL-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. Added complete */
/*        code example based on existing example. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (WMO) */

/* -& */
/* $ Index_Entries */

/*     equal to another 3x3_matrix */

/* -& */
    moved_(m1, &c__9, mout);
    return 0;
} /* mequ_ */

