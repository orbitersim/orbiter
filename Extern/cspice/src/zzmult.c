/* zzmult.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure ZZMULT ( Safer multiplication ) */
doublereal zzmult_(doublereal *a, doublereal *b)
{
    /* Initialized data */

    static logical first = TRUE_;

    /* System generated locals */
    doublereal ret_val, d__1;

    /* Builtin functions */
    double d_lg10(doublereal *);

    /* Local variables */
    static doublereal loga, logb;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    extern doublereal dpmax_(void);
    extern /* Subroutine */ int errdp_(char *, doublereal *, ftnlen);
    static doublereal expnt;
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), setmsg_(char *, ftnlen);
    extern logical return_(void);

/* $ Abstract */

/*     Safely calculate the value A*B, avoiding the possibility */
/*     of floating point exceptions (FPE) due to numeric underflow */
/*     or numeric overflow. */

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

/* $ Declarations */
/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     A          I   Multiplier. */
/*     B          I   Multiplicand. */

/* $ Detailed_Input */

/*     A          Multiplier for the multiplication operation. */

/*     B          Multiplicand for the multiplication operation. */

/* $ Detailed_Output */

/*     ZZMULT      The value A*B. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1) SPICE(NUMERICOVERFLOW) signals if the logarithm base 10 */
/*        of the multiplication is greater than EXPNT (defined */
/*        below). */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     We want to avoid a floating point exception signal from */
/*     the platform. This routine does not trap exceptions, */
/*     the intended purpose is to prevent exceptions. */

/*     Given, for the IEEE 754 double-precision binary */
/*     floating-point format, the order of magnitude of the minimum */
/*     normal positive double equals -307 and the order of magnitude of */
/*     the maximum double equals 308. */

/*       -307 <= LOG10(|A|) + LOG10(|B|) <= 308 */

/*     or */

/*         -307                   308 */
/*        10     <=  |A|*|B| <= 10 */

/*     Satisfying this condition should guarantee no floating */
/*     point exceptions. */

/*     Underflow returns zero without an error signal as per SPICE */
/*     convention. */

/*     Important, this routine does not calculate or enforce a */
/*     precision on the multiplication evaluation. A safe evaluation */
/*     may result in a result unusable due to precision loss. */

/*     The routine does not depend on platform-specific arithmetic */
/*     exception handling, even though the bound for the computed */
/*     ratio is platform-specific. */

/*     The range [-307,308] is valid for IEEE double precision. */
/*     It may occur this routine runs on a non compliant platform, */
/*     so calculate the range based on the DPMAX() value. */

/*     Assign a parameter EXPNT such that EXPNT equals the order of */
/*     DPMAX. The routine uses the range [-(EXPNT-1), EXPNT]. */

/*     This routine checks the sum of the base 10 logarithms */
/*     of A and B to ensure the magnitude of A*B is */
/*     within the range [-(EXPNT-1), EXPNT]. */

/* $ Examples */

/*     Demonstrate the use of ZZMULT with DPMAX and zero as the */
/*     multiplier and multiplicand. */

/*           PROGRAM ZZMULT_T */
/*           IMPLICIT NONE */

/*           DOUBLE PRECISION      A */
/*           DOUBLE PRECISION      B */
/*           DOUBLE PRECISION      MULT */

/*     C */
/*     C     SPICE functions. */
/*     C */
/*           DOUBLE PRECISION      ZZMULT */
/*           DOUBLE PRECISION      DPMAX */


/*     C */
/*     C     Set error reporting to REPORT. */
/*     C */
/*           CALL ERRACT( 'SET', 'REPORT' ) */


/*     C */
/*     C     Standard, safe evaluation. */
/*     C */
/*           A = 1.D0 */
/*           B = 10.D0 */

/*           MULT = ZZMULT( A, B ) */
/*           WRITE(*,*) 'MULT 1*10         = ', MULT */



/*     C */
/*     C     A numeric overflow event as defined in ZZMULT. */
/*     C */
/*           A = 1.D0 */
/*           B = DPMAX() */

/*           MULT = ZZMULT( A, B ) */
/*           WRITE(*,*) 'MULT 1*DPMAX()    = ', MULT */



/*     C */
/*     C     A numeric overflow event as defined in ZZMULT. */
/*     C */
/*           A = 1.D208 */
/*           B = 1.D307 */

/*           MULT = ZZMULT( A, B ) */
/*           WRITE(*,*) 'MULT 1.D515       = ', MULT */



/*     C */
/*     C     A multiply by zero event. */
/*     C */
/*           A = 1.D0 */
/*           B = 0.D0 */

/*           MULT = ZZMULT( A, B ) */
/*           WRITE(*,*) 'MULT 1*0          = ', MULT */



/*     C */
/*     C     A multiply by zero event. */
/*     C */
/*           A = 0.D0 */
/*           B = 0.D0 */

/*           MULT = ZZMULT( A, B ) */
/*           WRITE(*,*) 'MULT 0*0          = ', MULT */


/*           END */

/*   The program outputs: */

/*   -The function returns the evaluation value. No error. */

/*      MULT 1*10         =    10.000000000000000 */


/*   -The function signals a NUMERICOVERFLOW error, and sets the */
/*    return value to zero. */

/*     ================================================================= */

/*     Toolkit version: N0064 */

/*     SPICE(NUMERICOVERFLOW) -- */

/*     Numerical overflow event. Multiplier value, 1.0000000000000E+00, */
/*     multiplicand value, 1.7976931348623E+308. */

/*     A traceback follows.  The name of the highest level module is */
/*     first. */
/*     ZZMULT */

/*     ================================================================= */
/*      MULT 1*DPMAX()   =  0.0000000000000000 */


/*   -The function signals a NUMERICOVERFLOW error, and sets the */
/*    return value to zero. */

/*     ================================================================= */

/*     Toolkit version: N0064 */

/*     SPICE(NUMERICOVERFLOW) -- */

/*     Numerical overflow event. Multiplier value, 1.0000000000000E+208, */
/*     multiplicand value, 1.0000000000000E+307. */

/*     A traceback follows.  The name of the highest level module is */
/*     first. */
/*     ZZMULT */

/*     ================================================================= */
/*      MULT 1.D515       =    0.0000000000000000 */


/*   -The function returns the evaluation value. No error. */

/*      MULT 1*0          =    0.0000000000000000 */


/*   -The function returns the evaluation value. No error. */

/*      MULT 0*0          =    0.0000000000000000 */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     E.D. Wright    (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 31-JAN-2014 (EDW) */

/* -& */
/* $ Index_Entries */

/*     multiplication, avoid floating point exception */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     The bounds on the potential result of the calculation. */


/*     First entry flag. */


/*     Return on error. */

    if (return_()) {
	ret_val = 0.;
	return ret_val;
    }

/*     Participate in error tracing. */

    chkin_("ZZMULT", (ftnlen)6);

/*     Calculate the bounds parameter on first entry. */
/*     The double precision maximum value has the form */
/*     "d*(10**EXPNT)." The value of interest is "EXPNT." */

    if (first) {
	first = FALSE_;

/*        A "floor" evaluation. */

	d__1 = dpmax_();
	expnt = (doublereal) ((integer) d_lg10(&d__1));
    }

/*     If either A or B equals zero the multiplication is zero. */

    if (*a == 0. || *b == 0.) {
	ret_val = 0.;
	chkout_("ZZMULT", (ftnlen)6);
	return ret_val;
    }

/*     Calculate base 10 logarithms of the absolute value of the */
/*     numerator and denominator. Recall the base 10 log of a negative */
/*     real is a complex number (an irritating reality). Our interest */
/*     is the magnitude of the result, not the sign. */

/*     An earlier check returned if A or B equal zero. */

    d__1 = abs(*a);
    loga = d_lg10(&d__1);
    d__1 = abs(*b);
    logb = d_lg10(&d__1);

/*     Local possible overflow check. */

    if (loga + logb > expnt) {
	ret_val = 0.;
	setmsg_("Numerical overflow event. Multiplier value, #1, multiplican"
		"d value, #2.", (ftnlen)71);
	errdp_("#1", a, (ftnlen)2);
	errdp_("#2", b, (ftnlen)2);
	sigerr_("SPICE(NUMERICOVERFLOW)", (ftnlen)22);
	chkout_("ZZMULT", (ftnlen)6);
	return ret_val;
    }

/*     Local possible underflow check. Accept this may occur, */
/*     return a zero. */

    if (loga + logb < -(expnt - 1.)) {
	ret_val = 0.;
	chkout_("ZZMULT", (ftnlen)6);
	return ret_val;
    }

/*     This operation should be safe. Probably. */

    ret_val = *a * *b;
    chkout_("ZZMULT", (ftnlen)6);
    return ret_val;
} /* zzmult_ */

