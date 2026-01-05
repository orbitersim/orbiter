/* vproj.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure VPROJ ( Vector projection, 3 dimensions ) */
/* Subroutine */ int vproj_(doublereal *a, doublereal *b, doublereal *p)
{
    /* System generated locals */
    doublereal d__1, d__2;

    /* Local variables */
    doublereal biga, bigb;
    extern /* Subroutine */ int vscl_(doublereal *, doublereal *, doublereal *
	    );
    extern doublereal vdot_(doublereal *, doublereal *);
    doublereal r__[3], t[3], scale;

/* $ Abstract */

/*     Compute the projection of one 3-dimensional vector onto another */
/*     3-dimensional vector. */

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
/*     A          I   The vector to be projected. */
/*     B          I   The vector onto which A is to be projected. */
/*     P          O   The projection of A onto B. */

/* $ Detailed_Input */

/*     A        is a double precision, 3-dimensional vector. This */
/*              vector is to be projected onto the vector B. */

/*     B        is a double precision, 3-dimensional vector. This */
/*              vector is the vector which receives the projection. */

/* $ Detailed_Output */

/*     P        is a double precision, 3-dimensional vector containing */
/*              the projection of A onto B. (P is necessarily parallel */
/*              to B.) If B is the zero vector then P will be returned */
/*              as the zero vector. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     Given any vectors A and B, there is a unique decomposition of */
/*     A as a sum V + P such that V, the dot product of V and B, is zero, */
/*     and the dot product of P with B is equal the product of the */
/*     lengths of P and B. P is called the projection of A onto B. It */
/*     can be expressed mathematically as */

/*        DOT(A,B) */
/*        -------- * B */
/*        DOT(B,B) */

/*     (This is not necessarily the prescription used to compute the */
/*     projection. It is intended only for descriptive purposes.) */

/* $ Examples */

/*     The numerical results shown for this example may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */

/*     1) Define two sets of vectors and compute the projection of */
/*        each vector of the first set on the corresponding vector of */
/*        the second set. */

/*        Example code begins here. */


/*              PROGRAM VPROJ_EX1 */
/*              IMPLICIT NONE */

/*        C */
/*        C     Local parameters. */
/*        C */
/*              INTEGER               NDIM */
/*              PARAMETER           ( NDIM   = 3 ) */

/*              INTEGER               SETSIZ */
/*              PARAMETER           ( SETSIZ = 4 ) */

/*        C */
/*        C     Local variables. */
/*        C */
/*              DOUBLE PRECISION      SETA ( NDIM, SETSIZ ) */
/*              DOUBLE PRECISION      SETB ( NDIM, SETSIZ ) */
/*              DOUBLE PRECISION      PVEC ( NDIM ) */

/*              INTEGER               I */
/*              INTEGER               J */

/*        C */
/*        C     Define the two vector sets. */
/*        C */
/*              DATA                  SETA / 6.D0,  6.D0,  6.D0, */
/*             .                             6.D0,  6.D0,  6.D0, */
/*             .                             6.D0,  6.D0,  0.D0, */
/*             .                             6.D0,  0.D0,  0.D0  / */

/*              DATA                  SETB / 2.D0,  0.D0,  0.D0, */
/*             .                            -3.D0,  0.D0,  0.D0, */
/*             .                             0.D0,  7.D0,  0.D0, */
/*             .                             0.D0,  0.D0,  9.D0  / */

/*        C */
/*        C     Calculate the projection */
/*        C */
/*              DO I=1, SETSIZ */

/*                 CALL VPROJ ( SETA(1,I), SETB(1,I), PVEC ) */

/*                 WRITE(*,'(A,3F5.1)') 'Vector A  : ', */
/*             .                        ( SETA(J,I), J=1,3 ) */
/*                 WRITE(*,'(A,3F5.1)') 'Vector B  : ', */
/*             .                        ( SETB(J,I), J=1,3 ) */
/*                 WRITE(*,'(A,3F5.1)') 'Projection: ', PVEC */
/*                 WRITE(*,*) ' ' */

/*              END DO */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*        Vector A  :   6.0  6.0  6.0 */
/*        Vector B  :   2.0  0.0  0.0 */
/*        Projection:   6.0  0.0  0.0 */

/*        Vector A  :   6.0  6.0  6.0 */
/*        Vector B  :  -3.0  0.0  0.0 */
/*        Projection:   6.0 -0.0 -0.0 */

/*        Vector A  :   6.0  6.0  0.0 */
/*        Vector B  :   0.0  7.0  0.0 */
/*        Projection:   0.0  6.0  0.0 */

/*        Vector A  :   6.0  0.0  0.0 */
/*        Vector B  :   0.0  0.0  9.0 */
/*        Projection:   0.0  0.0  0.0 */


/* $ Restrictions */

/*     1)  An implicit assumption exists that A and B are specified in */
/*         the same reference frame. If this is not the case, the */
/*         numerical result has no meaning. */

/* $ Literature_References */

/*     [1]  G. Thomas and R. Finney, "Calculus and Analytic Geometry," */
/*          7th Edition, Addison Wesley, 1988. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 26-OCT-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. Added complete */
/*        code example. Added entry in $Restrictions section. */

/* -    SPICELIB Version 1.0.2, 23-APR-2010 (NJB) */

/*        Header correction: assertions that the output */
/*        can overwrite the input have been removed. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) */

/* -& */
/* $ Index_Entries */

/*     3-dimensional vector projection */

/* -& */
/* $ Revisions */

/* -    Beta Version 1.1.0, 4-JAN-1989 (WLT) */

/*        Upgrade the routine to work with negative axis indexes. Also */
/*        take care of the funky way the indices (other than the input) */
/*        were obtained via the MOD function. It works but isn't as */
/*        clear (or fast) as just reading the axes from data. */

/* -& */

/*     SPICELIB functions */


/*     Local variables */

/* Computing MAX */
    d__1 = abs(a[0]), d__2 = abs(a[1]), d__1 = max(d__1,d__2), d__2 = abs(a[2]
	    );
    biga = max(d__1,d__2);
/* Computing MAX */
    d__1 = abs(b[0]), d__2 = abs(b[1]), d__1 = max(d__1,d__2), d__2 = abs(b[2]
	    );
    bigb = max(d__1,d__2);
    if (biga == 0.) {
	p[0] = 0.;
	p[1] = 0.;
	p[2] = 0.;
	return 0;
    }
    if (bigb == 0.) {
	p[0] = 0.;
	p[1] = 0.;
	p[2] = 0.;
	return 0;
    }
    r__[0] = b[0] / bigb;
    r__[1] = b[1] / bigb;
    r__[2] = b[2] / bigb;
    t[0] = a[0] / biga;
    t[1] = a[1] / biga;
    t[2] = a[2] / biga;
    scale = vdot_(t, r__) * biga / vdot_(r__, r__);
    vscl_(&scale, r__, p);
    return 0;
} /* vproj_ */

