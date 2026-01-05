/* vaddg.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure VADDG ( Vector addition, general dimension ) */
/* Subroutine */ int vaddg_(doublereal *v1, doublereal *v2, integer *ndim, 
	doublereal *vout)
{
    /* System generated locals */
    integer v1_dim1, v2_dim1, vout_dim1, i__1, i__2, i__3, i__4;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer);

    /* Local variables */
    integer i__;

/* $ Abstract */

/*     Add two vectors of arbitrary dimension. */

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
/*     V1         I   First vector to be added. */
/*     V2         I   Second vector to be added. */
/*     NDIM       I   Dimension of V1, V2, and VOUT. */
/*     VOUT       O   Sum vector, V1 + V2. */

/* $ Detailed_Input */

/*     V1, */
/*     V2       are two arbitrary double precision n-dimensional */
/*              vectors. */

/*     NDIM     is the dimension of V1, V2 and VOUT. */

/* $ Detailed_Output */

/*     VOUT     is the double precision n-dimensional vector sum of V1 */
/*              and V2. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine simply performs addition between components of V1 */
/*     and V2. No checking is performed to determine whether floating */
/*     point overflow has occurred. */

/* $ Examples */

/*     The numerical results shown for this example may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */

/*     1) Define two sets of n-dimensional vectors and compute the sum */
/*        of each vector in first set with the corresponding vector in */
/*        the second set. */


/*        Example code begins here. */


/*              PROGRAM VADDG_EX1 */
/*              IMPLICIT NONE */

/*        C */
/*        C     Local parameters. */
/*        C */
/*              INTEGER               NDIM */
/*              PARAMETER           ( NDIM   = 4 ) */

/*              INTEGER               SETSIZ */
/*              PARAMETER           ( SETSIZ = 2 ) */

/*        C */
/*        C     Local variables. */
/*        C */
/*              DOUBLE PRECISION      SETA ( NDIM, SETSIZ ) */
/*              DOUBLE PRECISION      SETB ( NDIM, SETSIZ ) */
/*              DOUBLE PRECISION      VOUT ( NDIM ) */

/*              INTEGER               I */
/*              INTEGER               J */

/*        C */
/*        C     Define the two vector sets. */
/*        C */
/*              DATA                  SETA / */
/*             .                      1.D0,  2.D0,   3.D0,  4.D0, */
/*             .                      1.D-7, 1.D23, 1.D-9,  0.D0   / */

/*              DATA                  SETB / */
/*             .                      4.D0,  5.D0,   6.D0,  7.D0, */
/*             .                      1.D24, 1.D23,  0.D0,  3.D-23  / */

/*        C */
/*        C     Calculate the sum of each pair of vectors */
/*        C */
/*              DO I=1, SETSIZ */

/*                 CALL VADDG ( SETA(1,I), SETB(1,I), NDIM, VOUT ) */

/*                 WRITE(*,'(A,4E11.2)') 'Vector A  : ', */
/*             .                        ( SETA(J,I), J=1,NDIM ) */
/*                 WRITE(*,'(A,4E11.2)') 'Vector B  : ', */
/*             .                        ( SETB(J,I), J=1,NDIM ) */
/*                 WRITE(*,'(A,4E11.2)') 'Sum vector: ', VOUT */
/*                 WRITE(*,*) ' ' */

/*              END DO */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*        Vector A  :    0.10E+01   0.20E+01   0.30E+01   0.40E+01 */
/*        Vector B  :    0.40E+01   0.50E+01   0.60E+01   0.70E+01 */
/*        Sum vector:    0.50E+01   0.70E+01   0.90E+01   0.11E+02 */

/*        Vector A  :    0.10E-06   0.10E+24   0.10E-08   0.00E+00 */
/*        Vector B  :    0.10E+25   0.10E+24   0.00E+00   0.30E-22 */
/*        Sum vector:    0.10E+25   0.20E+24   0.10E-08   0.30E-22 */


/* $ Restrictions */

/*     1)  The user is required to determine that the magnitude each */
/*         component of the vectors is within the appropriate range so as */
/*         not to cause floating point overflow. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     W.M. Owen          (JPL) */
/*     W.L. Taber         (JPL) */
/*     E.D. Wright        (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 06-JUL-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. Added complete */
/*        code example based on existing example. */

/* -    SPICELIB Version 1.0.3, 23-APR-2010 (NJB) */

/*        Header correction: assertions that the output */
/*        can overwrite the input have been removed. */

/* -    SPICELIB Version 1.0.2, 07-NOV-2003 (EDW) */

/*        Corrected a mistake in the second example's value */
/*        for VOUT, i.e. replaced (1D24, 2D23, 0.0) with */
/*        (1D24, 2D23). */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (WMO) */

/* -& */
/* $ Index_Entries */

/*     n-dimensional vector addition */

/* -& */

/*     Local variables */

    /* Parameter adjustments */
    vout_dim1 = *ndim;
    v2_dim1 = *ndim;
    v1_dim1 = *ndim;

    /* Function Body */
    i__1 = *ndim;
    for (i__ = 1; i__ <= i__1; ++i__) {
	vout[(i__2 = i__ - 1) < vout_dim1 && 0 <= i__2 ? i__2 : s_rnge("vout",
		 i__2, "vaddg_", (ftnlen)232)] = v1[(i__3 = i__ - 1) < 
		v1_dim1 && 0 <= i__3 ? i__3 : s_rnge("v1", i__3, "vaddg_", (
		ftnlen)232)] + v2[(i__4 = i__ - 1) < v2_dim1 && 0 <= i__4 ? 
		i__4 : s_rnge("v2", i__4, "vaddg_", (ftnlen)232)];
    }
    return 0;
} /* vaddg_ */

