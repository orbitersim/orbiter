/* zzhashi.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure ZZHASHI ( Private---integer hash function ) */
integer zzhashi_(integer *n, integer *m)
{
    /* System generated locals */
    integer ret_val;

    /* Local variables */
    extern /* Subroutine */ int chkin_(char *, ftnlen), sigerr_(char *, 
	    ftnlen), chkout_(char *, ftnlen), setmsg_(char *, ftnlen), 
	    errint_(char *, integer *, ftnlen);

/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines. Users should not call this routine directly due */
/*     to the volatile nature of this routine. */

/*     This function returns the hash value corresponding to an integer. */

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

/*     PRIVATE UTILITY */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     N          I   An integer number. */
/*     M          I   Divisor used for the hash function */

/*     The function returns the hash value associated with N. */

/* $ Detailed_Input */

/*     N           is an integer number. */

/*     M           is the divisor of the hashing function. This value */
/*                 defines the spread of the hash values, that spread */
/*                 covering the interval [1, M]. M must be a positive */
/*                 number. If it is not, the function signals an error */
/*                 and returns 0. */

/* $ Detailed_Output */

/*     The function returns the hash value of N as computed using M */
/*     as the hash function divisor. */

/*     This function is sign-insensitive, i.e. it computes the same */
/*     hash value for A as is it does for -A. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1) If the input divisor value is not in the allowed range, the */
/*        error 'SPICE(INVALIDDIVISOR)' will be signaled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine computes the hash value of an integer number using */
/*     the user specified hash divisor value. */

/* $ Examples */

/*     None. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     W.L. Taber      (JPL) */
/*     E.D. Wright     (JPL) */
/*     B.V. Semenov    (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 02-AUG-2013 (BVS) */

/* -& */

/*     Check divisor. */

    if (*m <= 0) {
	ret_val = 0;
	chkin_("ZZHASHI", (ftnlen)7);
	setmsg_("The input hash function divisor was not a positive number. "
		"It was #.", (ftnlen)68);
	errint_("#", m, (ftnlen)1);
	sigerr_("SPICE(INVALIDDIVISOR)", (ftnlen)21);
	chkout_("ZZHASHI", (ftnlen)7);
	return ret_val;
    }

/*     Use simple division method -- h(k) = k mod m. */

    ret_val = abs(*n) % *m + 1;
    return ret_val;
} /* zzhashi_ */

