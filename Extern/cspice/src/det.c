/* det.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure DET  ( Determinant of a double precision 3x3 matrix ) */
doublereal det_(doublereal *m1)
{
    /* System generated locals */
    doublereal ret_val;

/* $ Abstract */

/*     Compute the determinant of a double precision 3x3 matrix. */

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
/*     M1         I   Matrix whose determinant is to be found. */

/*     The function returns the value of the determinant found by direct */
/*     application of the definition of the determinant. */

/* $ Detailed_Input */

/*     M1       is any double precision, 3x3 matrix. */

/* $ Detailed_Output */

/*     The function returns the value of the determinant found by direct */
/*     application of the definition of the determinant. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     DET calculates the determinant of M1 in a single arithmetic */
/*     expression which is, effectively, the expansion of M1 about its */
/*     first row. Since the calculation of the determinant involves */
/*     the multiplication of numbers whose magnitudes are unrestricted, */
/*     there is the possibility of floating point overflow or underflow. */
/*     NO error checking or recovery is implemented in this routine. */

/* $ Examples */

/*     The numerical results shown for this example may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */

/*     1) Given a 3x3 double precision matrix, compute its determinant. */

/*        Example code begins here. */


/*              PROGRAM DET_EX1 */
/*              IMPLICIT NONE */

/*        C */
/*        C     SPICELIB functions. */
/*        C */
/*              DOUBLE PRECISION      DET */

/*        C */
/*        C     Local variables */
/*        C */
/*              DOUBLE PRECISION      M1     ( 3, 3 ) */
/*              DOUBLE PRECISION      M2     ( 3, 3 ) */

/*        C */
/*        C     Set M1 and M2. */
/*        C */
/*              DATA                  M1 /  1.D0,  2.D0,  3.D0, */
/*             .                            4.D0,  5.D0,  6.D0, */
/*             .                            7.D0,  8.D0,  9.D0  / */

/*              DATA                  M2 /  1.D0,  2.D0,  3.D0, */
/*             .                            0.D0,  5.D0,  6.D0, */
/*             .                            0.D0,  0.D0,  9.D0  / */

/*        C */
/*        C     Display the determinant of M1 and M2. */
/*        C */
/*              WRITE(*,'(A,F6.2)') 'Determinant of M1:', DET(M1) */
/*              WRITE(*,'(A,F6.2)') 'Determinant of M2:', DET(M2) */


/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*        Determinant of M1:  0.00 */
/*        Determinant of M2: 45.00 */


/* $ Restrictions */

/*     1)  No checking is implemented to determine whether M1 will cause */
/*         overflow or underflow in the process of calculating the */
/*         determinant. In most cases, this will not pose a problem. */
/*         The user is required to determine if M1 is suitable matrix */
/*         for DET to operate on. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     W.M. Owen          (JPL) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.2, 02-JUL-2021 (JDR) */

/*        Edited the header to comply with NAIF standard. Added complete */
/*        code example based on existing fragment. */

/*        Added missing IMPLICIT NONE statement. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (WMO) */

/* -& */
/* $ Index_Entries */

/*     determinant of a d.p. 3x3_matrix */

/* -& */
    ret_val = m1[0] * (m1[4] * m1[8] - m1[7] * m1[5]) - m1[3] * (m1[1] * m1[8]
	     - m1[7] * m1[2]) + m1[6] * (m1[1] * m1[5] - m1[4] * m1[2]);
    return ret_val;
} /* det_ */

