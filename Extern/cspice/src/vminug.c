/* vminug.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure VMINUG ( Negate vector, "-V", general dimension ) */
/* Subroutine */ int vminug_(doublereal *vin, integer *ndim, doublereal *vout)
{
    /* System generated locals */
    integer vin_dim1, vout_dim1, i__1, i__2, i__3;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer);

    /* Local variables */
    integer i__;

/* $ Abstract */

/*     Negate a double precision vector of arbitrary dimension. */

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
/*     VIN        I   n-dimensional vector to be negated. */
/*     NDIM       I   Dimension of VIN (and also VOUT). */
/*     VOUT       O   Negated vector -V1. */

/* $ Detailed_Input */

/*     VIN      is any double precision vector of arbitrary size. */

/*     NDIM     is the dimension of VIN and VOUT. */

/* $ Detailed_Output */

/*     VOUT     is a n-dimensional double precision vector which */
/*              contains the negation of VIN. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     For each value of the index I from 1 to NDIM, VMINUG negates VIN */
/*     by the expression: */

/*        VOUT(I) = - VIN(I) */

/*     No error checking is performed since overflow can occur ONLY if */
/*     the dynamic range of positive floating point numbers is not the */
/*     same size as the dynamic range of negative floating point numbers */
/*     AND at least one component of VIN falls outside the common range. */
/*     The likelihood of this occurring is so small as to be of no */
/*     concern. */

/* $ Examples */

/*     The numerical results shown for this example may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */

/*     1) Define a set of n-dimensional vectors and negate each of them. */


/*        Example code begins here. */


/*              PROGRAM VMINUG_EX1 */
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
/*              DOUBLE PRECISION      VIN  ( NDIM, SETSIZ ) */
/*              DOUBLE PRECISION      VOUT ( NDIM         ) */

/*              INTEGER               I */
/*              INTEGER               J */

/*        C */
/*        C     Define a set of n-dimensional vectors. */
/*        C */
/*              DATA                  VIN  / */
/*             .                    -10.D0, 15.D0, -5.D0, 20.D0, */
/*             .                      0.D0,  0.D0,  0.D0,  0.D0  / */

/*        C */
/*        C     Negate each vector */
/*        C */
/*              DO I=1, SETSIZ */

/*                 CALL VMINUG ( VIN(1,I), NDIM, VOUT ) */

/*                 WRITE(*,'(A,4F7.1)') 'Input vector  : ', */
/*             .                        ( VIN(J,I), J=1,NDIM ) */
/*                 WRITE(*,'(A,4F7.1)') 'Negated vector: ', VOUT */
/*                 WRITE(*,*) */

/*              END DO */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*        Input vector  :   -10.0   15.0   -5.0   20.0 */
/*        Negated vector:    10.0  -15.0    5.0  -20.0 */

/*        Input vector  :     0.0    0.0    0.0    0.0 */
/*        Negated vector:    -0.0   -0.0   -0.0   -0.0 */


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

/* -    SPICELIB Version 1.0.3, 02-OCT-2020 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. Added complete */
/*        code example based on existing example. */

/*        Extended $Particulars section. */

/* -    SPICELIB Version 1.0.2, 23-APR-2010 (NJB) */

/*        Header correction: assertions that the output */
/*        can overwrite the input have been removed. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (WMO) */

/* -& */
/* $ Index_Entries */

/*     negate an n-dimensional vector */

/* -& */

    /* Parameter adjustments */
    vout_dim1 = *ndim;
    vin_dim1 = *ndim;

    /* Function Body */
    i__1 = *ndim;
    for (i__ = 1; i__ <= i__1; ++i__) {
	vout[(i__2 = i__ - 1) < vout_dim1 && 0 <= i__2 ? i__2 : s_rnge("vout",
		 i__2, "vminug_", (ftnlen)215)] = -vin[(i__3 = i__ - 1) < 
		vin_dim1 && 0 <= i__3 ? i__3 : s_rnge("vin", i__3, "vminug_", 
		(ftnlen)215)];
    }
    return 0;
} /* vminug_ */

