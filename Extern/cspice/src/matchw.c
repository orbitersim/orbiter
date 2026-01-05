/* matchw.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;

/* $Procedure MATCHW ( Match string against wildcard template ) */
logical matchw_(char *string, char *templ, char *wstr, char *wchr, ftnlen 
	string_len, ftnlen templ_len, ftnlen wstr_len, ftnlen wchr_len)
{
    /* System generated locals */
    integer i__1;
    logical ret_val;

    /* Local variables */
    integer left, slen, tlen, scur, tcur, i__, j;
    extern logical samch_(char *, integer *, char *, integer *, ftnlen, 
	    ftnlen);
    integer right, slast, tlast;
    extern integer lastnb_(char *, ftnlen), frstnb_(char *, ftnlen);
    logical nosubm;
    integer sfirst, tfirst;

/* $ Abstract */

/*     Determine whether a string is matched by a template containing */
/*     wild cards. */

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

/*     CHARACTER */
/*     COMPARE */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     STRING     I   String to be tested. */
/*     TEMPL      I   Template (with wild cards) to test against STRING. */
/*     WSTR       I   Wild string token. */
/*     WCHR       I   Wild character token. */

/*     The function returns .TRUE. if STRING matches TEMPL and otherwise */
/*     returns .FALSE. */

/* $ Detailed_Input */

/*     STRING   is the input character string to be tested for a match */
/*              against the input template. Leading and trailing blanks */
/*              are ignored. */

/*     TEMPL    is the input template to be tested for a match against */
/*              the input string. TEMPL may contain wild cards. Leading */
/*              and trailing blanks are ignored. */

/*     WSTR     is the wild string token used in the input template. The */
/*              wild string token may represent from zero to any number */
/*              of characters. */

/*     WCHR     is the wild character token used in the input template. */
/*              The wild character token represents exactly one */
/*              character. */

/* $ Detailed_Output */

/*     The function returns .TRUE. when the input string matches the */
/*     input template, and .FALSE. otherwise. The string and template */
/*     match whenever the template can expand (through replacement of its */
/*     wild cards) to become the input string. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     MATCHW ignores leading and trailing blanks in both the string */
/*     and the template. All of the following are equivalent (they */
/*     all return .TRUE.). */

/*        MATCHW ( 'ALCATRAZ',     'A*Z',      '*', '%' ) */
/*        MATCHW ( '  ALCATRAZ  ', 'A*Z',      '*', '%' ) */
/*        MATCHW ( 'ALCATRAZ',     '  A*Z  ',  '*', '%' ) */
/*        MATCHW ( '  ALCATRAZ  ', '  A*Z  ',  '*', '%' ) */

/*     MATCHW is case-sensitive:  uppercase characters do not match */
/*     lowercase characters, and vice versa. Wild characters match */
/*     characters of both cases. */

/* $ Examples */

/*     Let */

/*        STRING  = '  ABCDEFGHIJKLMNOPQRSTUVWXYZ  ' */
/*        WSTR    = '*' */
/*        WCHR    = '%' */

/*     Then */

/*        if TEMPL is  '*A*'        MATCHW is   T */
/*                     'A%D*'                     F */
/*                     'A%C*'                   T */
/*                     '%A*'                      F */
/*                         '%%CD*Z'                 T */
/*                         '%%CD'                     F */
/*                         'A*MN*Y*Z'               T */
/*                         'A*MN*Y*%Z'                F */
/*                         '*BCD*Z*'                T */
/*                         '*bcd*z*'                  F */
/*                         ' *BCD*Z*  '             T */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     H.A. Neilan        (JPL) */
/*     W.L. Taber         (JPL) */
/*     I.M. Underwood     (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.4.0, 06-JUL-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. Removed */
/*        unnecessary entries from $Revisions section. */

/* -    SPICELIB Version 1.3.1, 11-NOV-2005 (NJB) */

/*        Corrected example calls in header; made other minor */
/*        edits to header. */

/* -    SPICELIB Version 1.3.0, 08-JUN-1999 (WLT) */

/*        Fixed comments in detailed output and example sections. */

/* -    SPICELIB Version 1.2.0, 15-MAY-1995 (WLT) */

/*        Direct substring comparisons were replaced with the logical */
/*        function SAMCH in several cases so as to avoid out of range */
/*        errors when examining substrings. */

/* -    SPICELIB Version 1.1.0, 17-MAY-1994 (HAN) */

/*        Set the default function value to either 0, 0.0D0, .FALSE., */
/*        or blank depending on the type of the function. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (IMU) */

/* -& */
/* $ Index_Entries */

/*     match string against wildcard template */
/*     test whether a string matches a wildcard template */

/* -& */
/* $ Revisions */

/* -    Beta Version 1.1.0, 06-OCT-1988 (WLT) */

/*        The old algorithm just did not work. Strings with wild */
/*        characters at the beginning or end of the string were not */
/*        matched correctly. For example, A% matched APPROX, if the */
/*        wild character token was % and the wild string token was *. */
/*        Needless to say, a new algorithm was developed. */

/* -& */

/*     SPICELIB functions */


/*     Local Variables */


/*     Set the default function value to be FALSE. */

    ret_val = FALSE_;

/*     First let's get everybody's measurements. */

    sfirst = frstnb_(string, string_len);
    slast = lastnb_(string, string_len);
    tfirst = frstnb_(templ, templ_len);
    tlast = lastnb_(templ, templ_len);
    tlen = tlast - tfirst + 1;
    slen = slast - sfirst + 1;
    scur = max(1,sfirst);
    tcur = tfirst;

/*     A blank template matches a blank string, and nothing else. */

    if (tlast == 0 && slast == 0) {
	ret_val = TRUE_;
	return ret_val;
    } else if (tlast == 0) {
	ret_val = FALSE_;
	return ret_val;
    }

/*     The beginning of the string and template must be identical */
/*     up to the first occurrence of a wild string. */

    while(tcur <= tlast && scur <= slast && ! samch_(templ, &tcur, wstr, &
	    c__1, templ_len, (ftnlen)1)) {
	if (*(unsigned char *)&templ[tcur - 1] != *(unsigned char *)&string[
		scur - 1] && *(unsigned char *)&templ[tcur - 1] != *(unsigned 
		char *)wchr) {
	    ret_val = FALSE_;
	    return ret_val;
	} else {
	    ++tcur;
	    ++scur;
	}
    }

/*     There are a three ways we could have finished the loop above */
/*     without hitting a wild string. */

/*     Case 1.  Both the string and template ran out of characters at */
/*     the same time without running into a wild string in the template. */

    if (tcur > tlast && scur > slast) {
	ret_val = TRUE_;
	return ret_val;
    }

/*     Case 2. The template ran out of characters while there were still */
/*     characters remaining in the in the string.  No match. */

    if (tcur > tlast && scur <= slast) {
	ret_val = FALSE_;
	return ret_val;
    }

/*     Case 3. The string ran out of characters while non-wild characters */
/*     remain in the template. */

/*     We have to check to see if any non-wild-string characters */
/*     remain.  If so, we DO NOT have a match.  On the other hand if */
/*     only wild string characters remain we DO have a match. */

    if (tcur <= tlast && scur > slast) {
	ret_val = TRUE_;
	i__1 = tlast;
	for (i__ = tcur; i__ <= i__1; ++i__) {
	    ret_val = ret_val && *(unsigned char *)&templ[i__ - 1] == *(
		    unsigned char *)wstr;
	}
	return ret_val;
    }

/*     OK. There is only one way that you can get to this point. */
/*     It must be the case that characters remain in both the template */
/*     (TCUR .LE. TLAST) and the string (SCUR .LE. SLAST).  Moreover, */
/*     to get out of the first loop you had to hit a wild string */
/*     character.  Find the first non-wild-string character in the */
/*     template. (If there isn't one, we have a match.) */

    while(tcur <= tlast && samch_(templ, &tcur, wstr, &c__1, templ_len, (
	    ftnlen)1)) {
	++tcur;
    }
    if (tcur > tlast) {
	ret_val = TRUE_;
	return ret_val;
    }

/*     Still here? Ok. We have a non-wild-string character at TCUR. Call */
/*     this position left and look for the right end of the maximum */
/*     length substring of TEMPL (starting at left) that does not have */
/*     a wild string character. */

    left = tcur;
    while(tcur <= tlast && ! samch_(templ, &tcur, wstr, &c__1, templ_len, (
	    ftnlen)1)) {
	++tcur;
    }
    right = tcur - 1;
    while(left <= tlast) {

/*        First see if there is enough room left in the string to */
/*        match TEMPL(LEFT:RIGHT) */

	if (slast - scur < right - left) {
	    ret_val = FALSE_;
	    return ret_val;
	}

/*        The substring TEMPL(LEFT:RIGHT) might be the end of the */
/*        string.  In such a case the ends of STRING must match */
/*        exactly with the end of TEMPL. */

	if (right == tlast) {
	    i__ = slast;
	    j = tlast;
	    while(j >= left) {
		if (samch_(templ, &j, wchr, &c__1, templ_len, (ftnlen)1) || 
			samch_(templ, &j, string, &i__, templ_len, string_len)
			) {
		    --j;
		    --i__;
		} else {
		    ret_val = FALSE_;
		    return ret_val;
		}
	    }

/*           If we made it through the loop, we've got a match. */

	    ret_val = TRUE_;
	    return ret_val;
	} else {

/*           In this case TEMPL(LEFT:RIGHT) is in between wild string */
/*           characters.  Try to find a substring at or to the right */
/*           of SCUR in STRING that matches TEMPL(LEFT:RIGHT) */

	    nosubm = TRUE_;
	    while(nosubm) {
		i__ = scur;
		j = left;
		while(j <= right && (samch_(string, &i__, templ, &j, 
			string_len, templ_len) || samch_(wchr, &c__1, templ, &
			j, (ftnlen)1, templ_len))) {
		    ++i__;
		    ++j;
		}

/*              If J made it past RIGHT, we have a substring match */

		if (j > right) {
		    scur = i__;
		    nosubm = FALSE_;

/*              Otherwise, try the substring starting 1 to the right */
/*              of where our last try began. */

		} else {
		    ++scur;

/*                 Make sure there's room to even attempt a match. */

		    if (slast - scur < right - left) {
			ret_val = FALSE_;
			return ret_val;
		    }
		}
	    }
	}

/*        If you have reached this point there must be something left */
/*        in the template and that something must begin with a wild */
/*        string character.  Hunt for the next substring that doesn't */
/*        contain a wild string character. */

	while(tcur <= tlast && samch_(templ, &tcur, wstr, &c__1, templ_len, (
		ftnlen)1)) {
	    ++tcur;
	}
	if (tcur > tlast) {

/*           All that was left was wild string characters.  We've */
/*           got a match. */

	    ret_val = TRUE_;
	    return ret_val;
	}

/*        Still here? Ok. We have a non-wild-string character at TCUR. */
/*        Call this position left and look for the right end of the */
/*        maximum length substring of TEMPL (starting at left) that */
/*        does not have a wild string character. */

	left = tcur;
	while(tcur <= tlast && ! samch_(templ, &tcur, wstr, &c__1, templ_len, 
		(ftnlen)1)) {
	    ++tcur;
	}
	right = tcur - 1;
    }
    return ret_val;
} /* matchw_ */

