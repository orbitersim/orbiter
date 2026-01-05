/* dvnorm.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure DVNORM ( Derivative of vector norm ) */
doublereal dvnorm_(doublereal *state)
{
    /* System generated locals */
    doublereal ret_val;

    /* Local variables */
    extern /* Subroutine */ int vhat_(doublereal *, doublereal *);
    doublereal xhat[3];
    extern doublereal vdot_(doublereal *, doublereal *), vnorm_(doublereal *);

/* $ Abstract */

/*     Calculate the derivative of the norm of a 3-vector. */

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

/*     DERIVATIVE */
/*     MATH */
/*     VECTOR */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     STATE      I   A 6-vector composed of three coordinates and their */
/*                    derivatives. */

/*     The function returns the derivative of the norm of the position */
/*     component of the input STATE vector. */

/* $ Detailed_Input */

/*     STATE    is a double precision 6-vector, the second three */
/*              components being the derivatives of the first three */
/*              with respect to some scalar. */

/*                               dx */
/*                 STATE =  ( x, -- ) */
/*                               ds */

/*              A common form for STATE would contain position and */
/*              velocity. */

/* $ Detailed_Output */

/*     The function returns the derivative of the norm of the position */
/*     component of the input STATE vector: */

/*                  d ||x|| */
/*        DVNORM = -------- */
/*                    ds */

/*     where the norm of x is given by: */

/*                                     .---------------- */
/*                   .---------       /    2    2    2 */
/*        ||x|| =  \/ < x, x >  = \  / ( x1 + x2 + x3  ) */
/*                                 \/ */


/*     If the velocity component of STATE is: */

/*                  dx1   dx2   dx3 */
/*           v = ( ----, ----, ---- ) */
/*                  ds    ds    ds */

/*     then */

/*           d||x||      < x, v > */
/*           ------ =  ------------  =  < xhat, v > */
/*             ds        .--------- */
/*                     \/ < x, x > */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/*     1)  If the first three components of STATE ("x") describe the */
/*         origin (zero vector) the routine returns zero as the */
/*         derivative of the vector norm. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     A common use for this routine is to calculate the time derivative */
/*     of the radius corresponding to a state vector. */

/* $ Examples */

/*     The numerical results shown for this example may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */

/*     1) Compute the derivative of the norm of three vectors of */
/*        different magnitudes. Use the first two vectors to define */
/*        the derivatives as parallel and anti-parallel, and let */
/*        the third be the zero vector. */

/*        Example code begins here. */


/*              PROGRAM DVNORM_EX1 */
/*              IMPLICIT NONE */

/*              DOUBLE PRECISION      X     (3) */
/*              DOUBLE PRECISION      MAG   (3) */
/*              DOUBLE PRECISION      DVMAG (3) */
/*              DOUBLE PRECISION      Y     (6) */

/*              DOUBLE PRECISION      DVNORM */
/*        C */
/*        C     Create several 6-vectors (6x1 arrays) with the structure */
/*        C */
/*        C        s = |  x  | */
/*        C            |     | */
/*        C            |  dx | */
/*        C            |  -- | */
/*        C            |  ds | */
/*        C */
/*        C      where 'x' is a 3-vector (3x1 array). */
/*        C */

/*        C */
/*        C      Create 's' with 'x' of varying magnitudes. Use 'x' */
/*        C      and '-x' to define the derivative as parallel and */
/*        C      anti-parallel. */
/*        C */
/*              MAG(1) =  -4.D0 */
/*              MAG(2) =   4.D0 */
/*              MAG(3) =  12.D0 */

/*              X(1)   = 1.D0 */
/*              X(2)   = DSQRT( 2.D0 ) */
/*              X(3)   = DSQRT( 3.D0 ) */

/*        C */
/*        C     Parallel... */
/*        C */
/*              Y(1)   = X(1) * 10.D0**MAG(1) */
/*              Y(2)   = X(2) * 10.D0**MAG(1) */
/*              Y(3)   = X(3) * 10.D0**MAG(1) */
/*              Y(4)   = X(1) */
/*              Y(5)   = X(2) */
/*              Y(6)   = X(3) */

/*              WRITE(*,*) 'Parallel x, dx/ds         : ', DVNORM( Y ) */

/*        C */
/*        C     ... anti-parallel... */
/*        C */
/*              Y(1)   = X(1) * 10.D0**MAG(2) */
/*              Y(2)   = X(2) * 10.D0**MAG(2) */
/*              Y(3)   = X(3) * 10.D0**MAG(2) */
/*              Y(4)   = -X(1) */
/*              Y(5)   = -X(2) */
/*              Y(6)   = -X(3) */

/*              WRITE(*,*) 'Anti-parallel x, dx/ds    : ', DVNORM( Y ) */

/*        C */
/*        C     ... 'x' zero vector */
/*        C */
/*              Y(1)   = 0.D0 */
/*              Y(2)   = 0.D0 */
/*              Y(3)   = 0.D0 */
/*              Y(4)   = X(1) * 10.D0**MAG(3) */
/*              Y(5)   = X(2) * 10.D0**MAG(3) */
/*              Y(6)   = X(3) * 10.D0**MAG(3) */

/*              WRITE(*,*) 'Zero vector x, large dx/ds: ', DVNORM( Y ) */
/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*         Parallel x, dx/ds         :    2.4494897427831779 */
/*         Anti-parallel x, dx/ds    :   -2.4494897427831779 */
/*         Zero vector x, large dx/ds:    0.0000000000000000 */


/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     E.D. Wright        (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 06-JUL-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. Added problem */
/*        statement to the example. Moved the contents of the */
/*        $Restrictions section to $Exceptions. */

/* -    SPICELIB Version 1.0.0, 03-MAY-2010 (EDW) */

/* -& */
/* $ Index_Entries */

/*     derivative of 3-vector norm */

/* -& */

/*     SPICELIB functions. */


/*     Local Variables. */


/*     If "x" describes the zero vector, return zero as the derivative */
/*     of the vector norm. */

    if (vnorm_(state) == 0.) {
	ret_val = 0.;
	return ret_val;
    }

/*     Construct a unit vector from the x vector data */
/*     in STATE. */

    vhat_(state, xhat);

/*     Project the velocity components onto the XHAT vector. */

/*      d ||x||          x */
/*      -------  = v . ----- */
/*        ds           ||x|| */

    ret_val = vdot_(&state[3], xhat);
    return ret_val;
} /* dvnorm_ */

