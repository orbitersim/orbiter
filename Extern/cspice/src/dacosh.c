/* dacosh.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure DACOSH ( Double precision arc hyperbolic cosine ) */
doublereal dacosh_(doublereal *x)
{
    /* System generated locals */
    doublereal ret_val;

    /* Builtin functions */
    double sqrt(doublereal), log(doublereal);

    /* Local variables */
    extern /* Subroutine */ int chkin_(char *, ftnlen), sigerr_(char *, 
	    ftnlen), chkout_(char *, ftnlen), setmsg_(char *, ftnlen);
    extern logical return_(void);

/* $ Abstract */

/*     Return the inverse hyperbolic cosine of a double precision */
/*     argument. */

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

/*     HYPERBOLIC */
/*     MATH */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*      X         I   Number whose inverse hyperbolic cosine is desired. */

/*     The function returns the inverse hyperbolic cosine of a double */
/*     precision number. */

/* $ Detailed_Input */

/*     X        is any double precision number greater than or equal to */
/*              1. */

/* $ Detailed_Output */

/*     The function returns the inverse hyperbolic cosine of the double */
/*     precision number X. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If X is less than 1.0D0, the error SPICE(INVALIDARGUMENT) is */
/*         signaled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This function simply implements the definition of the inverse */
/*     hyperbolic cosine as follows: */

/*        DACOSH = DLOG (X + DSQRT (X*X-1.D0)) */

/*     If the input value is not valid, an error is signaled. */

/* $ Examples */

/*     The following table gives a few values for X and the resulting */
/*     value of DACOSH. */

/*     X                       DACOSH(X) */
/*     ---------------------------------------------- */
/*     1.000000000000000      0.0000000000000000E+00 */
/*     10.00000000000000       2.993222846126381 */
/*     100.0000000000000       5.298292365610485 */
/*     1000.000000000000       7.600902209541989 */

/* $ Restrictions */

/*     1)  The value of the input variable X must be greater than or */
/*         equal to 1.0d0. */

/* $ Literature_References */

/*     [1]  W.H. Beyer, "CRC Standard Mathematical Tables," CRC Press, */
/*          1987. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     H.A. Neilan        (JPL) */
/*     W.M. Owen          (JPL) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.2.0, 17-JUN-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 1.1.0, 17-MAY-1994 (HAN) */

/*        Set the default function value to either 0, 0.0D0, .FALSE., */
/*        or blank depending on the type of the function. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (WMO) */

/* -& */
/* $ Index_Entries */

/*     d.p. arc hyperbolic_cosine */

/* -& */

/*     SPICELIB functions */


/*   Set up the error processing. */

    if (return_()) {
	ret_val = 0.;
	return ret_val;
    } else {
	chkin_("DACOSH", (ftnlen)6);
	ret_val = 0.;
    }

/*  Check that X >= 1. */

    if (*x < 1.) {
	setmsg_("DACOSH: Invalid argument, X is less than one.", (ftnlen)45);
	sigerr_("SPICE(INVALIDARGUMENT)", (ftnlen)22);
	chkout_("DACOSH", (ftnlen)6);
	return ret_val;
    }

/*  Abiding by the order implied by the parentheses in the expression */
/*  (1.0D0/X)/X prevents floating point overflow that might occur for */
/*  large values of X if the equivalent expression, 1.0D0/(X*X), were */
/*  used. */

    ret_val = log(*x + *x * sqrt(1. - 1. / *x / *x));
    chkout_("DACOSH", (ftnlen)6);
    return ret_val;
} /* dacosh_ */

