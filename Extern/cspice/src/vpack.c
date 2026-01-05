/* vpack.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure VPACK ( Pack three scalar components into a vector ) */
/* Subroutine */ int vpack_(doublereal *x, doublereal *y, doublereal *z__, 
	doublereal *v)
{
/* $ Abstract */

/*     Pack three scalar components into a vector. */

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
/*     X, */
/*     Y, */
/*     Z          I   Scalar components of a vector. */
/*     V          O   Equivalent vector. */

/* $ Detailed_Input */

/*     X, */
/*     Y, */
/*     Z        are the scalar components of a 3-dimensional vector. */

/* $ Detailed_Output */

/*     V        is the equivalent vector, such that */

/*                 V(1) = X */
/*                 V(2) = Y */
/*                 V(3) = Z */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     Basically, this is just shorthand notation for the common */
/*     sequence */

/*        V(1) = X */
/*        V(2) = Y */
/*        V(3) = Z */

/*     The routine is useful largely for two reasons. First, it */
/*     reduces the chance that the programmer will make a "cut and */
/*     paste" mistake, like */

/*        V(1) = X */
/*        V(1) = Y */
/*        V(1) = Z */

/*     Second, it makes conversions between equivalent units simpler, */
/*     and clearer. For instance, the sequence */

/*        V(1) = X * RPD */
/*        V(2) = Y * RPD */
/*        V(3) = Z * RPD */

/*     can be replaced by the (nearly) equivalent sequence */

/*        CALL VPACK  ( X,   Y, Z, V ) */
/*        CALL VSCLIP ( RPD, V       ) */

/* $ Examples */

/*     The numerical results shown for this example may differ across */
/*     platforms. The results depend on the SPICE kernels used as input */
/*     (if any), the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */

/*     1) Compute an upward normal of an equilateral triangle lying */
/*        in the X-Y plane and centered at the origin. */


/*        Example code begins here. */


/*              PROGRAM VPACK_EX1 */
/*              IMPLICIT NONE */

/*        C */
/*        C     Local variables */
/*        C */
/*              DOUBLE PRECISION      NORMAL ( 3 ) */
/*              DOUBLE PRECISION      S */
/*              DOUBLE PRECISION      V1     ( 3 ) */
/*              DOUBLE PRECISION      V2     ( 3 ) */
/*              DOUBLE PRECISION      V3     ( 3 ) */


/*              S = SQRT(3.D0)/2 */

/*        C */
/*        C     Define the three corners of the triangle. */
/*        C */
/*              CALL VPACK (    S,  -0.5D0,  0.D0, V1 ) */
/*              CALL VPACK ( 0.D0,    1.D0,  0.D0, V2 ) */
/*              CALL VPACK (   -S,  -0.5D0,  0.D0, V3 ) */

/*        C */
/*        C     Compute an upward normal of the triangle. */
/*        C */
/*              CALL PLTNRM ( V1, V2, V3, NORMAL ) */

/*              WRITE (*, '(A,3F17.13)' ) 'NORMAL = ', NORMAL */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*        NORMAL =   0.0000000000000  0.0000000000000  2.5980762113533 */


/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     W.L. Taber         (JPL) */
/*     I.M. Underwood     (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 16-JUL-2020 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. Added complete */
/*        code example. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU) */

/* -& */
/* $ Index_Entries */

/*     pack three scalar components into a vector */

/* -& */

/*     Just shorthand, like it says above. */

    v[0] = *x;
    v[1] = *y;
    v[2] = *z__;
    return 0;
} /* vpack_ */

