/* kpsolv.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure KPSOLV ( Solve Kepler's Equation --- Vector Form ) */
doublereal kpsolv_(doublereal *evec)
{
    /* System generated locals */
    integer i__1, i__2, i__3, i__4;
    doublereal ret_val, d__1, d__2, d__3, d__4;

    /* Builtin functions */
    double sqrt(doublereal);
    integer i_dnnt(doublereal *);
    double cos(doublereal), sin(doublereal);

    /* Local variables */
    doublereal cosx, sinx, h__;
    integer i__;
    doublereal k, x;
    extern /* Subroutine */ int chkin_(char *, ftnlen), errdp_(char *, 
	    doublereal *, ftnlen);
    integer maxit;
    doublereal y0, xl, xm, xu, yx;
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), setmsg_(char *, ftnlen);
    doublereal ecc, ecc2, yxm, ypx;

/* $ Abstract */

/*     Solve the equation X = < EVEC, U(X) > where U(X) is the unit */
/*     vector [ COS(X), SIN(X) ] and  < , > denotes the two-dimensional */
/*     dot product. */

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

/*     ROOTS */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     EVEC       I   A 2-vector whose magnitude is less than 1. */

/*     The function returns the solution to X = < EVEC, U(X) > */

/* $ Detailed_Input */

/*     EVEC     is any two dimensional vector whose magnitude is */
/*              less than 1. */

/* $ Detailed_Output */

/*     The function returns the value X such that the equation */

/*        X = EVEC(1)COS(X) + EVEC(2)SIN(X). */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the magnitude of EVEC is greater than or equal to 1, */
/*         the error SPICE(EVECOUTOFRANGE) is signaled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine uses bisection and Newton's method to find */
/*     the root of the equation */

/*        X = EVEC(1)COS(X) + EVEC(2)SIN(X). */

/*     This equation is just a "vector form" of Kepler's equation. */

/* $ Examples */

/*     Suppose you need to solve the equation */

/*         M = E - e SIN(E)                           [ 1 ] */

/*     for E. If we let X = E - M the equation is transformed to */

/*        0 = X - e SIN( X + M ) */

/*          = X - e SIN( M ) COS(X) - e COS(M) SIN ( X ) */

/*     Thus if we solve the equation */

/*        X = e SIN(M) COS(X) + e COS(M) SIN(X) */

/*     we can find the value of X we can compute E. */

/*     The code fragment below illustrates how this routine can */
/*     be used to solve equation [1]. */

/*         EVEC(1) = ECC * DSIN(M) */
/*         EVEC(2) = ECC * DCOS(M) */
/*         E       = M   + KPSOLV( EVEC ) */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.1, 26-OCT-2021 (JDR) */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 1.1.0, 26-AUG-1997 (WLT) */

/*        KPSOLV is now given an initial value of zero so that */
/*        if an error condition is detected, KPSOLV will have */
/*        a return value. */

/* -    SPICELIB Version 1.0.0, 03-JAN-1997 (WLT) */

/* -& */
/* $ Index_Entries */

/*     Solve the vector form of the Kepler equation */

/* -& */

/*     MXNEWT is the number of iterations we will perform */
/*     in the Newtons method for finding the solution to */
/*     the vector form of Kepler's equation.  It has been */
/*     empirically determined that 5 iterations is always */
/*     sufficient on computers have 64 bit double precision */
/*     numbers. */


/*     We give the function an initial value, just in case */
/*     we exit without solving Kepler's equation. */

    ret_val = 0.;
    h__ = evec[0];
    k = evec[1];
    ecc2 = h__ * h__ + k * k;
    if (ecc2 >= 1.) {
	chkin_("KPSOLV", (ftnlen)6);
	setmsg_("The magnitude of the vector EVEC = ( #, # ) must be less th"
		"an 1.  However, the magnitude of this vector is #.", (ftnlen)
		109);
	errdp_("#", &h__, (ftnlen)1);
	errdp_("#", &k, (ftnlen)1);
	d__1 = sqrt(ecc2);
	errdp_("#", &d__1, (ftnlen)1);
	sigerr_("SPICE(EVECOUTOFRANGE)", (ftnlen)21);
	chkout_("KPSOLV", (ftnlen)6);
	return ret_val;
    }

/*     We first approximate the equation 0 = X - H * COS(X) - K * SIN(X) */
/*     using bisection.  If we let Y(X) = X - H * COS(X) - K * SIN(X) */

/*        Y( ECC) =  ECC - <EVEC,U(X)>  =   ECC - ECC*COS(ANGLE_X) > 0 */
/*        Y(-ECC) = -ECC - <EVEC,U(X)>  =  -ECC - ECC*COS(ANGLE_X) < 0 */

/*     where ANGLE_X is the angle between U(X) and EVEC. Thus -ECC */
/*     and ECC necessarily bracket the root of the equation Y(X) = 0. */

/*     Also note that Y'(X) = 1 - < EVEC, V(X) > where V(X) is the */
/*     unit vector given by U'(X).  Thus Y is an increasing function */
/*     over the interval from -ECC to ECC. */

/*     The mid point of ECC and -ECC is 0 and Y(0) = -H.  Thus */
/*     we can do the first bisection step without doing */
/*     much in the way of computations. */

    y0 = -h__;
    xm = 0.;
    ecc = sqrt(ecc2);
    if (y0 > 0.) {
	xu = 0.;
	xl = -ecc;
    } else if (y0 < 0.) {
	xu = ecc;
	xl = 0.;
    } else {
	ret_val = 0.;
	return ret_val;
    }

/*     Iterate until we are assured of being in a region where */
/*     Newton's method will converge quickly.  The formula */
/*     below was empirically determined to give good results. */

/* Computing MIN */
/* Computing MAX */
    d__1 = 1. / (1. - ecc);
    i__3 = 1, i__4 = i_dnnt(&d__1);
    i__1 = 32, i__2 = max(i__3,i__4);
    maxit = min(i__1,i__2);
    i__1 = maxit;
    for (i__ = 1; i__ <= i__1; ++i__) {

/*        Compute the next midpoint.  We bracket XM by XL and XU just in */
/*        case some kind of strange rounding occurs in the computation */
/*        of the midpoint. */

/* Computing MAX */
/* Computing MIN */
	d__3 = xu, d__4 = (xl + xu) * .5;
	d__1 = xl, d__2 = min(d__3,d__4);
	xm = max(d__1,d__2);

/*        Compute Y at the midpoint of XU and XL */

	yxm = xm - h__ * cos(xm) - k * sin(xm);

/*        Determine the new upper and lower bounds. */

	if (yxm > 0.) {
	    xu = xm;
	} else {
	    xl = xm;
	}
    }

/*     We've bisected into a region where we can now get rapid */
/*     convergence using Newton's method. */

    x = xm;
    for (i__ = 1; i__ <= 5; ++i__) {
	cosx = cos(x);
	sinx = sin(x);

/*        Compute Y and Y' at X.  Use these to get the next */
/*        iteration for X. */

/*        For those of you who might be wondering, "Why not put */
/*        in a check for YX .EQ. 0 and return early if we get */
/*        an exact solution?"  Here's why.  An empirical check */
/*        of those cases where you can actually escape from the */
/*        Do-loop  showed that the test YX .EQ. 0 is true */
/*        only about once in every 10000 case of random inputs */
/*        of EVEC.  Thus on average the check is a waste of */
/*        time and we don't bother with it. */

	yx = x - h__ * cosx - k * sinx;
	ypx = h__ * sinx + 1. - k * cosx;
	x -= yx / ypx;
    }
    ret_val = x;
    return ret_val;
} /* kpsolv_ */

