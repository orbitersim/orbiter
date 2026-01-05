/* zzutcpm.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;

/* $Procedure      ZZUTCPM ( UTC Plus or Minus Parse ) */
/* Subroutine */ int zzutcpm_(char *string, integer *start, doublereal *hoff, 
	doublereal *moff, integer *last, logical *succes, ftnlen string_len)
{
    /* System generated locals */
    integer i__1;

    /* Builtin functions */
    integer i_len(char *, ftnlen);

    /* Local variables */
    integer need;
    doublereal sign, x;
    extern logical samch_(char *, integer *, char *, integer *, ftnlen, 
	    ftnlen);
    integer nchar;
    char error[80];
    integer unsat, unsto;
    extern /* Subroutine */ int lx4uns_(char *, integer *, integer *, integer 
	    *, ftnlen);
    integer length, signat;
    extern /* Subroutine */ int nparsd_(char *, doublereal *, char *, integer 
	    *, ftnlen, ftnlen);
    integer ptr;

/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines.  Users should not call this routine directly due */
/*     to the volatile nature of this routine. */

/*     Parse a substring of the form ::UTC[+/-]1-12:0-59 */

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

/*      None. */

/* $ Keywords */

/*      Time --- PRIVATE */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     STRING     I   is a string containing a substring ::UTC+HR:MN */
/*     START      I   is the location in the string to start parsing */
/*     HOFF       O   is the d.p. value associated with HR. */
/*     MOFF       O   is the d.p. value associated with MN */
/*     LAST       O   is the end of the time zone substring. */
/*     SUCCES     O   indicates that a time zone was parsed. */

/* $ Detailed_Input */

/*     STRING     is a string that has an embedded substring of the */
/*                form ::UTC+HR[:MN] ( or ::UTC-HR[:MN] starting at */
/*                character start. */

/*     START      is the location in STRING where a time zone */
/*                specification is believed to begin. */

/* $ Detailed_Output */

/*     HOFF       is the double precision value associated with */
/*                HR in the picture above.  This value will be */
/*                between -12 and 12 inclusive. */

/*     MOFF       is the double precision value associated with MN */
/*                in the picture above.  This value will be between */
/*                0 and 59 inclusive (or -59 and 0 inclusive) depending */
/*                on the sign present in the UTC+/- substring.  The */
/*                sign of MOFF is the same as the sign present in the */
/*                string. */

/*     LAST       is the last character of the time zone specification. */
/*                If the string doesn't have a correct format and */
/*                range of values, LAST is returns as START - 1. */

/*     SUCCES     is a logical which if true, indicates that a time */
/*                zone was successfully parsed. */

/* $ Parameters */

/*     None. */

/* $ Files */

/*     None. */

/* $ Exceptions */

/*     1) There are no exceptions.  Either the string matches */
/*        the template or it doesn't.  No case is regarded */
/*        as an error. */

/* $ Particulars */

/*     This is a private routine for parsing time zones specified */
/*     as UTC+/-HR:MN  where HR is an unsigned integer between 0 and */
/*     11.  HR must have no more than 2 digits.  MN is expected */
/*     to be an unsigned integer between 0 and 59 inclusive.  It must */
/*     have no more than 2 digits. */

/* $ Examples */

/*     See TIMOUT. */

/* $ Restrictions */

/*     None. */

/* $ Author_and_Institution */

/*     W.L. Taber      (JPL) */

/* $ Literature_References */

/*     None. */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 27-SEP-1996 (WLT) */


/* -& */

/*     Spicelib functions */


/*     Local Variables */


/*     This is a special purpose routine.  The input string must have */
/*     exactly the right format to be a time zone substring.  If anything */
/*     goes wrong, we just bail out and leave HOFF and MOFF right at */
/*     zero. */

    *hoff = 0.;
    *moff = 0.;
    *last = *start - 1;
    *succes = FALSE_;

/*     Note that NEED   = START + LEN('::UTC+x') - 1 */
/*               SIGNAT = START + LEN('::UTC+' ) - 1 */

    length = i_len(string, string_len);
    need = *start + 6;
    signat = *start + 5;
    unsat = need;
    if (length < need) {
	return 0;
    }
    if (*(unsigned char *)&string[signat - 1] == '+') {
	sign = 1.;
    } else if (*(unsigned char *)&string[signat - 1] == '-') {
	sign = -1.;
    } else {
	return 0;
    }

/*     So far everything looks fine, "lex" the string starting at */
/*     SIGNAT + 1 for an unsigned integer. */

    lx4uns_(string, &unsat, &unsto, &nchar, string_len);
    if (nchar > 0 && nchar < 3) {
	nparsd_(string + (unsat - 1), &x, error, &ptr, unsto - (unsat - 1), (
		ftnlen)80);
	if (x >= 13.) {
	    return 0;
	}
	*last = unsto;
	*hoff = sign * x;
    } else {
	return 0;
    }

/*     If we're still in the game at this point, we have at least */
/*     an hour offset, see if there is a minutes portion to the */
/*     time zone. */

    *succes = TRUE_;
    i__1 = unsto + 1;
    if (samch_(string, &i__1, ":", &c__1, string_len, (ftnlen)1)) {
	unsat = unsto + 2;
    } else {
	return 0;
    }
    lx4uns_(string, &unsat, &unsto, &nchar, string_len);
    if (nchar > 0 && nchar < 3) {
	nparsd_(string + (unsat - 1), &x, error, &ptr, unsto - (unsat - 1), (
		ftnlen)80);
	if (x > 59.) {
	    return 0;
	}
	*last = unsto;
	*moff = sign * x;
    }
    return 0;
} /* zzutcpm_ */

