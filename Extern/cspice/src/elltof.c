/* elltof.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure ELLTOF ( Elliptic time of flight ) */
/* Subroutine */ int elltof_(doublereal *ma, doublereal *ecc, doublereal *e)
{
    /* System generated locals */
    doublereal d__1, d__2;

    /* Builtin functions */
    double sin(doublereal), sqrt(doublereal), cos(doublereal);

    /* Local variables */
    doublereal a, b, m;
    integer n;
    doublereal q, r__, y;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    extern doublereal dcbrt_(doublereal *);
    doublereal deriv, m0;
    extern doublereal twopi_(void);
    doublereal deriv2, fn, change;
    extern doublereal pi_(void), halfpi_(void);
    doublereal qr, mprime;
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen);
    extern logical return_(void);

/* $ Abstract */

/*     Solve the time of flight equation MA = E - e sin(E) for the */
/*     elliptic eccentric anomaly E, given mean anomaly the MA and */
/*     the eccentricity ECC. */

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

/*     CONIC */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     MA         I   Mean anomaly at epoch. */
/*     ECC        I   Eccentricity. */
/*     E          O   Elliptic eccentric anomaly. */

/* $ Detailed_Input */

/*     MA       is the elliptic mean anomaly of an orbiting body at */
/*              some epoch t, */

/*                                3 1/2 */
/*                 MA = (t-T)(mu/a ) */

/*              where T is the time of periapsis passage, a is */
/*              the semi-major axis of the orbit, and mu is the */
/*              gravitational parameter of the primary body. */

/*     ECC      is the eccentricity of the orbit. */

/* $ Detailed_Output */

/*     E        is the corresponding eccentric anomaly. This is the */
/*              solution to the time of flight equation */

/*                 MA = E - e sin(E) */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the eccentricity (ECC) is outside the range [0,1), */
/*         the error SPICE(WRONGCONIC) is signaled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     Iterate to solve */

/*        f(E,MA,e) = E - e sin(E) - MA = 0 */

/* $ Examples */

/*     ELLTOF, HYPTOF, and PARTOF are used by CONICS. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     [1]  R. Bate, D. Mueller, and J. White, "Fundamentals of */
/*          Astrodynamics," Dover Publications Inc., 1971. */

/*     [2]  E. W. Ng, "A General Algorithm for the Solution of Kepler's */
/*          Equation for Elliptic Orbits", Cel. Mech. 20, pp.243-249, */
/*          1979. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     K.R. Gehringer     (JPL) */
/*     W.L. Taber         (JPL) */
/*     I.M. Underwood     (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.2.0, 14-APR-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 1.1.0, 29-FEB-1996 (KRG) */

/*        The declaration for the SPICELIB function PI is now */
/*        preceded by an EXTERNAL statement declaring PI to be an */
/*        external function. This removes a conflict with any */
/*        compilers that have a PI intrinsic function. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU) */

/* -& */
/* $ Index_Entries */

/*     elliptic time of flight */

/* -& */
/* $ Revisions */

/* -    Beta Version 1.1.0, 8-JAN-1989 (IMU) */

/*        The routine now verifies that the eccentricity is in the */
/*        proper range---[0,1)---before proceeding. */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     Local variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("ELLTOF", (ftnlen)6);
    }
    if (*ecc < 0. || *ecc >= 1.) {
	sigerr_("SPICE(WRONGCONIC)", (ftnlen)17);
	chkout_("ELLTOF", (ftnlen)6);
	return 0;
    }

/*     For reasons of numerical stability, we would like to restrict */
/*     our solution to the interval [0,pi]. Because E, M, and sin E */
/*     are always positive or negative together, we can pretend that M */
/*     is positive and adjust the sign of the result. And for M, E > pi, */
/*     we can define */

/*           M = 2n pi + M'     and    E = 2n pi + E' */

/*     where M' and E' are in the interval [-pi,pi]. Solving for E' */
/*     gives us E. */

/*     So, we begin by reducing the input mean anomaly to [0,pi]. */

    m = abs(*ma);
    if (m > pi_()) {
	n = (integer) ((m - pi_()) / twopi_()) + 1;
	mprime = m - n * twopi_();
    } else {
	n = 0;
	mprime = m;
    }
    m = abs(mprime);

/*     The convergence of the iterative scheme below depends on a good */
/*     initial estimate for E. */

/*     For small eccentricity, the initial estimate E = M is sufficient. */
/*     However, as the eccentricity increases, so does the number of */
/*     iterations required for convergence. For sufficiently large */
/*     eccentricity, this estimate leads to divergence. */

/*     Ng [2] notes that the function y(M,e) */

/*           E - M */
/*          -------  =  sin(e y + M) */
/*             e */

/*     increases and decreases monotonically when M is in the ranges */
/*     [0,M0] and [m0,pi], respectively. */

/*     When M0 < M < pi, where M0 = (pi/2) - e, the cubic */
/*             -   - */

/*                              pi - M  2        pi - M    pi - M */
/*           B(M,e) = 1 - (1 -  -------)  (1 + 2 ------- - -------) */
/*                              pi - M0          pi - M0    1 + e */

/*     provides a good initial estimate of y for all values of e. */


    m0 = halfpi_() - *ecc;
    if (m >= m0) {
	a = pi_() - m;
	b = pi_() - m0;
/* Computing 2nd power */
	d__1 = 1. - a / b;
	y = 1. - d__1 * d__1 * (a * 2. / b + 1. - a / (*ecc + 1.));
	*e = *ecc * sin(*ecc * y + m) + m;

/*     The situation is a little more troublesome, however, when M < M0. */
/*     For small eccentricity, the cubic */

/*                                  2 */
/*           A(M,e) = 1 - (1 - M/M0)  (1 + 2M/M0 - M/(1-e) ) */

/*     gives a reasonable first estimate of y. However, as e -> 1, */
/*     successive approximations of the form */

/*                             k           k */
/*           C (M,e) = 1 - (-1)  (1 - M/M0) */
/*            k */

/*     are used, where k = 4 for e > 0.7, and k = 8 for e > 0.85. */

/*     For high eccentricity (e > 0.96) and low mean anomaly (M < 0.05), */
/*     these successive approximations eventually fail. Fortunately, in */
/*     just these cases, the cubic */

/*                           3    2  1/3           3    2  1/3 */
/*           D(M,e) = [r + (q  + r )]     + [r - (q  + r )] */

/*     where */

/*           r = 3M/e,   q = (2/e)(1 - e) */

/*     provides a reasonable estimate of E directly. */


    } else if (*ecc <= .7) {
/* Computing 2nd power */
	d__1 = 1. - m / m0;
	y = 1. - d__1 * d__1 * (m * 2. / m0 + 1. - m / (1. - *ecc));
	*e = *ecc * sin(*ecc * y + m) + m;
    } else if (*ecc <= .85) {
/* Computing 4th power */
	d__1 = 1. - m / m0, d__1 *= d__1;
	y = 1. - d__1 * d__1;
	*e = *ecc * sin(*ecc * y + m) + m;
    } else if (*ecc <= .96 || m > .05) {
/* Computing 8th power */
	d__1 = 1. - m / m0, d__1 *= d__1, d__1 *= d__1;
	y = 1. - d__1 * d__1;
	*e = *ecc * sin(*ecc * y + m) + m;
    } else {
	q = 2. / *ecc * (1. - *ecc);
	r__ = m / *ecc * 3.;
/* Computing 3rd power */
	d__1 = q;
/* Computing 2nd power */
	d__2 = r__;
	qr = sqrt(d__1 * (d__1 * d__1) + d__2 * d__2);
	d__1 = r__ + qr;
	d__2 = r__ - qr;
	*e = dcbrt_(&d__1) + dcbrt_(&d__2);
    }

/*     Use the Newton second-order method, */

/*                                           2 */
/*          E    = E  - (f/f')*(1 + f*f''/2f' ) */
/*           i+1    i */

/*     where */

/*          f   = E - e sin(E) - M */
/*          f'  = 1 - e cos(E) */
/*          f'' =     e sin(E) */

    change = 1.;
    while(abs(change) > 1e-15) {
	fn = *e - *ecc * sin(*e) - m;
	deriv = 1. - *ecc * cos(*e);
	deriv2 = *ecc * sin(*e);
/* Computing 2nd power */
	d__1 = deriv;
	change = fn / deriv * (fn * deriv2 / (d__1 * d__1 * 2.) + 1.);
	*e -= change;
    }

/*     "Unwrap" E' into the actual value of E. */

    if (mprime < 0.) {
	*e = -(*e);
    }
    if (n > 0) {
	*e += n * twopi_();
    }
    if (*ma < 0.) {
	*e = -(*e);
    }
    chkout_("ELLTOF", (ftnlen)6);
    return 0;
} /* elltof_ */

