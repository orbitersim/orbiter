/* vhatg.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure VHATG ( "V-Hat", unit vector along V, general dimension ) */
/* Subroutine */ int vhatg_(doublereal *v1, integer *ndim, doublereal *vout)
{
    /* System generated locals */
    integer v1_dim1, vout_dim1, i__1, i__2, i__3;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer);

    /* Local variables */
    doublereal vmag;
    integer i__;
    extern doublereal vnormg_(doublereal *, integer *);

/* $ Abstract */

/*     Find the unit vector along a double precision vector of */
/*     arbitrary dimension. */

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
/*     V1         I   Vector to be normalized. */
/*     NDIM       I   Dimension of V1 (and also VOUT). */
/*     VOUT       O   Unit vector along V1. */

/* $ Detailed_Input */

/*     V1       is any double precision vector of arbitrary dimension. */

/*     NDIM     is the dimension of V1 (and also VOUT). */

/* $ Detailed_Output */

/*     VOUT     is the unit vector in the direction of V1: */

/*                           V1 */
/*                 VOUT = -------- */
/*                         ||V1|| */

/*              If V1 represents the zero vector, then VOUT will also be */
/*              the zero vector. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     VHATG determines the magnitude of V1 and then divides each */
/*     component of V1 by the magnitude. This process is highly stable */
/*     over the whole range of multi-dimensional vectors. */

/*     This routine will detect if V1 the zero vector, and will not */
/*     attempt to divide by zero. */

/* $ Examples */

/*     The numerical results shown for this example may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */

/*     1) Define a set of n-dimensional vectors and find the unit vector */
/*        along each of them. */


/*        Example code begins here. */


/*              PROGRAM VHATG_EX1 */
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
/*              DOUBLE PRECISION      V1   ( NDIM, SETSIZ ) */
/*              DOUBLE PRECISION      VOUT ( NDIM         ) */

/*              INTEGER               I */
/*              INTEGER               J */

/*        C */
/*        C     Define the vector set. */
/*        C */
/*              DATA                  V1  / */
/*             .                          5.D0,  12.D0,  0.D0,  0.D0, */
/*             .                          1.D-7,  2.D-7, 2.D-7, 0.D0  / */

/*        C */
/*        C     Calculate the unit vectors. */
/*        C */
/*              DO I=1, SETSIZ */

/*                 CALL VHATG ( V1(1,I), NDIM, VOUT ) */

/*                 WRITE(*,'(A,4F12.7)') 'Input vector: ', */
/*             .                         ( V1(J,I), J=1,NDIM ) */
/*                 WRITE(*,'(A,4F12.7)') 'Unit vector : ', VOUT */
/*                 WRITE(*,*) */

/*              END DO */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*        Input vector:    5.0000000  12.0000000   0.0000000   0.0000000 */
/*        Unit vector :    0.3846154   0.9230769   0.0000000   0.0000000 */

/*        Input vector:    0.0000001   0.0000002   0.0000002   0.0000000 */
/*        Unit vector :    0.3333333   0.6666667   0.6666667   0.0000000 */


/* $ Restrictions */

/*     1)  The relative number of cases whereby floating point overflow */
/*         may occur is negligible. Thus, no error recovery or reporting */
/*         scheme is incorporated into this subroutine. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     W.M. Owen          (JPL) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 05-JUL-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. Added complete */
/*        code example based on existing example. */

/* -    SPICELIB Version 1.0.2, 22-APR-2010 (NJB) */

/*        Header correction: assertions that the output */
/*        can overwrite the input have been removed. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (WMO) */

/* -& */
/* $ Index_Entries */

/*     unitize a n-dimensional vector */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*  Obtain the magnitude of V1 */

    /* Parameter adjustments */
    vout_dim1 = *ndim;
    v1_dim1 = *ndim;

    /* Function Body */
    vmag = vnormg_(v1, ndim);

/*   If VMAG is nonzero, then normalize.  Note that this process is */
/*   numerically stable: overflow could only happen if VMAG were small, */
/*   but this could only happen if each component of V1 were small. */
/*   In fact, the magnitude of any vector is never less than the */
/*   magnitude of any component. */

    if (vmag > 0.) {
	i__1 = *ndim;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    vout[(i__2 = i__ - 1) < vout_dim1 && 0 <= i__2 ? i__2 : s_rnge(
		    "vout", i__2, "vhatg_", (ftnlen)241)] = v1[(i__3 = i__ - 
		    1) < v1_dim1 && 0 <= i__3 ? i__3 : s_rnge("v1", i__3, 
		    "vhatg_", (ftnlen)241)] / vmag;
	}
    } else {
	i__1 = *ndim;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    vout[(i__2 = i__ - 1) < vout_dim1 && 0 <= i__2 ? i__2 : s_rnge(
		    "vout", i__2, "vhatg_", (ftnlen)245)] = 0.;
	}
    }

    return 0;
} /* vhatg_ */

