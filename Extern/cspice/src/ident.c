/* ident.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure IDENT ( Return the 3x3 identity matrix ) */
/* Subroutine */ int ident_(doublereal *matrix)
{
/* $ Abstract */

/*     Return the 3x3 identity matrix. */

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
/*     MATRIX     O   The 3x3 identity matrix. */

/* $ Detailed_Input */

/*     None. */

/* $ Detailed_Output */

/*     MATRIX   is the 3x3 Identity matrix. That MATRIX is */
/*              the following */

/*                 .-                       -. */
/*                 |  1.0D0   0.0D0   0.0D0  | */
/*                 |  0.0D0   1.0D0   0.0D0  | */
/*                 |  0.0D0   0.0D0   1.0D0  | */
/*                 `-                       -' */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This is a utility routine for obtaining the 3x3 identity matrix */
/*     so that you may avoid having to write the loop or assignments */
/*     needed to get the matrix. */

/* $ Examples */

/*     The numerical results shown for this example may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */

/*     1) Define a 3x3 matrix and compute its inverse using the SPICELIB */
/*        routine INVERT. Verify the accuracy of the computed inverse */
/*        using the mathematical identity */

/*                -1 */
/*           M x M   - I = 0 */

/*        where I is the 3x3 identity matrix. */


/*        Example code begins here. */


/*              PROGRAM IDENT_EX1 */
/*              IMPLICIT NONE */

/*        C */
/*        C     Local variables. */
/*        C */
/*              DOUBLE PRECISION      IDMAT  ( 3, 3 ) */
/*              DOUBLE PRECISION      IMAT   ( 3, 3 ) */
/*              DOUBLE PRECISION      M      ( 3, 3 ) */
/*              DOUBLE PRECISION      MOUT   ( 3, 3 ) */
/*              DOUBLE PRECISION      MZERO  ( 3, 3 ) */

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
/*              CALL IDENT ( IDMAT ) */
/*              CALL MXM   ( M, MOUT, IMAT ) */

/*              CALL VSUBG ( IMAT, IDMAT, 9, MZERO ) */

/*              WRITE(*,*) ' ' */
/*              WRITE(*,*) 'Original times inverse minus identity:' */
/*              DO I=1, 3 */

/*                 WRITE(*,'(3F16.7)') ( MZERO(I,J), J=1,3 ) */

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

/*         Original times inverse minus identity: */
/*               0.0000000       0.0000000       0.0000000 */
/*               0.0000000       0.0000000       0.0000000 */
/*               0.0000000       0.0000000       0.0000000 */


/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.1, 03-JUN-2021 (JDR) */

/*        Edited the header to comply with NAIF standard. Added complete */
/*        code example. */

/* -    SPICELIB Version 1.0.0, 05-FEB-1996 (WLT) */

/* -& */
/* $ Index_Entries */

/*     Get the 3x3 identity matrix */

/* -& */
    matrix[0] = 1.;
    matrix[1] = 0.;
    matrix[2] = 0.;
    matrix[3] = 0.;
    matrix[4] = 1.;
    matrix[5] = 0.;
    matrix[6] = 0.;
    matrix[7] = 0.;
    matrix[8] = 1.;
    return 0;
} /* ident_ */

