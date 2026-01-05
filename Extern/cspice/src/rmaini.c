/* rmaini.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure RMAINI ( Remainder --- integer ) */
/* Subroutine */ int rmaini_(integer *num, integer *denom, integer *q, 
	integer *rem)
{
    extern /* Subroutine */ int chkin_(char *, ftnlen), sigerr_(char *, 
	    ftnlen), chkout_(char *, ftnlen), setmsg_(char *, ftnlen);

/* $ Abstract */

/*     Compute the integer quotient and non-negative remainder */
/*     of NUM and DENOM. */

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
/*     UTILITY */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     NUM        I   Numerator used to compute quotient and remainder. */
/*     DENOM      I   Denominator used to compute quotient and remainder. */
/*     Q          O   Integer portion of the quotient NUM/DENOM. */
/*     REM        O   Remainder of the quotient NUM/DENOM. */

/* $ Detailed_Input */

/*     NUM      is the numerator of a quotient */

/*     DENOM    is the denominator of a quotient */

/* $ Detailed_Output */

/*     Q        is the largest integer less than or equal to the */
/*              quotient NUM/DENOM */

/*     REM      is the remainder of the integer division NUM/DENOM */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If DENOM is zero, the error SPICE(DIVIDEBYZERO) is signaled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     Given the integer inputs NUM and DENOM, this routine */
/*     finds integers Q and REM that satisfy the following conditions: */

/*         1) NUM = DENOM * Q + REM */

/*         2) REM is a non negative integer less than the absolute */
/*            value of DENOM. */

/*     This routine serves as a macro. In this way the code to perform */
/*     this task can be written and maintained in a single location. */

/* $ Examples */

/*     One frequently needs to compute the  ``360 modulus'' of a */
/*     number. For positive numbers the FORTRAN intrinsic mod */
/*     function works well. However, for negative numbers the */
/*     intrinsic will return a negative modulus. This routine */
/*     can be used to compute the positive 360 pi modulus (MOD360) for */
/*     any integer I by the call: */

/*         CALL RMAINI ( I, 360, Q, MOD360 ) */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 12-AUG-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 1.0.0, 01-DEC-1995 (WLT) */

/* -& */
/* $ Index_Entries */

/*     Compute the remainder of an integer division */

/* -& */

/*     Take care of the zero-denominator case first */

    if ((doublereal) (*denom) == 0.) {
	chkin_("RMAINI", (ftnlen)6);
	setmsg_("Attempting to compute a quotient with a divide by zero.", (
		ftnlen)55);
	sigerr_("SPICE(DIVIDEBYZERO)", (ftnlen)19);
	chkout_("RMAINI", (ftnlen)6);
	return 0;
    }
    *q = *num / *denom;
    *rem = *num - *denom * *q;
    if (*rem < 0) {
	--(*q);
	*rem += *denom;
    }
    return 0;
} /* rmaini_ */

