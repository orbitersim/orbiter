/* ucrss.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure UCRSS ( Unitized cross product, 3x3 ) */
/* Subroutine */ int ucrss_(doublereal *v1, doublereal *v2, doublereal *vout)
{
    /* System generated locals */
    doublereal d__1, d__2;

    /* Local variables */
    doublereal vmag, maxv1, maxv2;
    extern doublereal vnorm_(doublereal *);
    doublereal vcross[3], tv1[3], tv2[3];

/* $ Abstract */

/*     Compute the normalized cross product of two 3-vectors. */

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
/*     V1         I   Left vector for cross product. */
/*     V2         I   Right vector for cross product. */
/*     VOUT       O   Normalized cross product of V1 and V2. */

/* $ Detailed_Input */

/*     V1, */
/*     V2       are two double precision 3-dimensional vectors. */
/*              Typically, these might represent the (possibly unit) */
/*              vector to a planet, Sun, or a star which defines the */
/*              orientation of axes of some reference frame. */

/* $ Detailed_Output */

/*     VOUT     is the double precision 3-dimensional normalized cross */
/*              product of V1 and V2. VOUT is the result of the */
/*              computation */

/*                     V1 x V2 */
/*                 --------------- */
/*                  || V1 x V2 || */

/*              where "x" denotes the cross product and ||X||| the norm */
/*              of a vector X. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/*     1)  If the cross product of V1 and V2 yields the zero-vector, */
/*         then the zero-vector is returned instead of a vector of */
/*         unit length. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     None. */

/* $ Examples */

/*     The numerical results shown for this example may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */

/*     1) Define two sets of vectors and compute the normalized cross */
/*        product of each vector in first set and the corresponding */
/*        vector in the second set. */


/*        Example code begins here. */


/*              PROGRAM UCRSS_EX1 */
/*              IMPLICIT NONE */

/*        C */
/*        C     Local parameters. */
/*        C */
/*              INTEGER               NDIM */
/*              PARAMETER           ( NDIM   = 3 ) */

/*              INTEGER               SETSIZ */
/*              PARAMETER           ( SETSIZ = 2 ) */

/*        C */
/*        C     Local variables. */
/*        C */
/*              DOUBLE PRECISION      V1   ( NDIM, SETSIZ ) */
/*              DOUBLE PRECISION      V2   ( NDIM, SETSIZ ) */
/*              DOUBLE PRECISION      VOUT ( NDIM ) */

/*              INTEGER               I */
/*              INTEGER               J */

/*        C */
/*        C     Define the two vector sets. */
/*        C */
/*              DATA                  V1   / 0.D0,  1.D0,  0.D0, */
/*             .                             5.D0,  5.D0,  5.D0  / */

/*              DATA                  V2   / 3.D0,  0.D0,  0.D0, */
/*             .                            -2.D0, -2.D0, -2.D0  / */

/*        C */
/*        C     Calculate the cross product of each pair of vectors */
/*        C */
/*              DO I=1, SETSIZ */

/*                 CALL UCRSS ( V1(1,I), V2(1,I), VOUT ) */

/*                 WRITE(*,'(A,3F5.1)') 'Vector A                : ', */
/*             .                                  ( V1(J,I), J=1,3 ) */
/*                 WRITE(*,'(A,3F5.1)') 'Vector B                : ', */
/*             .                                  ( V2(J,I), J=1,3 ) */
/*                 WRITE(*,'(A,3F5.1)') 'Normalized cross product: ', */
/*             .                                                VOUT */
/*                 WRITE(*,*) ' ' */

/*              END DO */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*        Vector A                :   0.0  1.0  0.0 */
/*        Vector B                :   3.0  0.0  0.0 */
/*        Normalized cross product:   0.0  0.0 -1.0 */

/*        Vector A                :   5.0  5.0  5.0 */
/*        Vector B                :  -2.0 -2.0 -2.0 */
/*        Normalized cross product:   0.0  0.0  0.0 */


/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     W.M. Owen          (JPL) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 06-JUL-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. Added complete */
/*        code example. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (WMO) (WLT) */

/* -& */
/* $ Index_Entries */

/*     unitized cross product */

/* -& */
/* $ Revisions */

/* -    Beta Version 1.1.0, 10-JAN-1989 (WLT) */

/*        Error free specification added. In addition the algorithm was */
/*        made more robust in the sense that floating point overflows */
/*        cannot occur. */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     Get the biggest component of each of the two vectors. */

/* Computing MAX */
    d__1 = abs(v1[0]), d__2 = abs(v1[1]), d__1 = max(d__1,d__2), d__2 = abs(
	    v1[2]);
    maxv1 = max(d__1,d__2);
/* Computing MAX */
    d__1 = abs(v2[0]), d__2 = abs(v2[1]), d__1 = max(d__1,d__2), d__2 = abs(
	    v2[2]);
    maxv2 = max(d__1,d__2);

/*     Scale V1 and V2 by 1/MAXV1 and 1/MAXV2 respectively */

    if (maxv1 != 0.) {
	tv1[0] = v1[0] / maxv1;
	tv1[1] = v1[1] / maxv1;
	tv1[2] = v1[2] / maxv1;
    } else {
	tv1[0] = 0.;
	tv1[1] = 0.;
	tv1[2] = 0.;
    }
    if (maxv2 != 0.) {
	tv2[0] = v2[0] / maxv2;
	tv2[1] = v2[1] / maxv2;
	tv2[2] = v2[2] / maxv2;
    } else {
	tv2[0] = 0.;
	tv2[1] = 0.;
	tv2[2] = 0.;
    }

/*  Calculate the cross product of V1 and V2 */

    vcross[0] = tv1[1] * tv2[2] - tv1[2] * tv2[1];
    vcross[1] = tv1[2] * tv2[0] - tv1[0] * tv2[2];
    vcross[2] = tv1[0] * tv2[1] - tv1[1] * tv2[0];

/*  Get the magnitude of VCROSS and normalize it */

    vmag = vnorm_(vcross);
    if (vmag > 0.) {
	vout[0] = vcross[0] / vmag;
	vout[1] = vcross[1] / vmag;
	vout[2] = vcross[2] / vmag;
    } else {
	vout[0] = 0.;
	vout[1] = 0.;
	vout[2] = 0.;
    }
    return 0;
} /* ucrss_ */

