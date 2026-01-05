/* trace.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure TRACE ( Trace of a 3x3 matrix ) */
doublereal trace_(doublereal *matrix)
{
    /* System generated locals */
    doublereal ret_val;

/* $ Abstract */

/*     Return the trace of a 3x3 matrix. */

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
/*     MATRIX     I   3x3 matrix of double precision numbers. */

/*     The function returns the trace of MATRIX. */

/* $ Detailed_Input */

/*     MATRIX   is a double precision 3x3 matrix. */

/* $ Detailed_Output */

/*     The function returns the trace of MATRIX, i.e. it is the sum of */
/*     the diagonal elements of MATRIX. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     The code reflects precisely the following mathematical */
/*     expression: */

/*                  3 */
/*                .---- */
/*                 \ */
/*        TRACE =   )  MATRIX(I,I) */
/*                 / */
/*                '---- */
/*                 I=1 */

/*     No error detection or correction is implemented within this */
/*     function. */

/* $ Examples */

/*     The numerical results shown for this example may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */

/*     1) Given a 3x3 double precision matrix, compute its trace. */


/*        Example code begins here. */


/*              PROGRAM TRACE_EX1 */
/*              IMPLICIT NONE */

/*        C */
/*        C     SPICELIB functions. */
/*        C */
/*              DOUBLE PRECISION      TRACE */

/*        C */
/*        C     Local variables. */
/*        C */
/*              DOUBLE PRECISION      MATRIX ( 3, 3 ) */

/*              INTEGER               I */
/*              INTEGER               J */

/*        C */
/*        C     Define MATRIX. */
/*        C */
/*              DATA                  MATRIX  /  3.D0,  0.D0,  4.D0, */
/*             .                                 5.D0, -2.D0,  0.D0, */
/*             .                                 7.D0,  8.D0, -1.D0  / */


/*              WRITE(*,'(A)') 'MATRIX:' */
/*              DO I=1, 3 */

/*                 WRITE(*,'(3F6.1)') ( MATRIX(I,J), J=1,3 ) */

/*              END DO */

/*        C */
/*        C     Compute the trace of MATRIX and display the result. */
/*        C */
/*              WRITE(*,*) */
/*              WRITE(*,'(A,F4.1)') 'Trace: ', TRACE ( MATRIX ) */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*        MATRIX: */
/*           3.0   5.0   7.0 */
/*           0.0  -2.0   8.0 */
/*           4.0   0.0  -1.0 */

/*        Trace:  0.0 */


/* $ Restrictions */

/*     1)  No checking is performed to guard against floating point */
/*         overflow or underflow. This routine should probably not be */
/*         used if the input matrix is expected to have large double */
/*         precision numbers along the diagonal. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.2, 03-JUL-2020 (JDR) */

/*        Edited the header to comply with NAIF standard. Added complete */
/*        code example based on existing example. */

/*        Added IMPLICIT NONE statement. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) */

/* -& */
/* $ Index_Entries */

/*     trace of a 3x3_matrix */

/* -& */
    ret_val = matrix[0] + matrix[4] + matrix[8];
    return ret_val;
} /* trace_ */

