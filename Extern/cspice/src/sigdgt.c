/* sigdgt.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;

/* $Procedure SIGDGT ( Retain significant digits ) */
/* Subroutine */ int sigdgt_(char *in, char *out, ftnlen in_len, ftnlen 
	out_len)
{
    /* System generated locals */
    integer i__1, i__2;

    /* Builtin functions */
    integer i_len(char *, ftnlen);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer i_indx(char *, char *, ftnlen, ftnlen), s_cmp(char *, char *, 
	    ftnlen, ftnlen);

    /* Local variables */
    extern integer cpos_(char *, char *, integer *, ftnlen, ftnlen);
    integer zero, i__, j, k, l, begin;
    char lchar[1];
    extern integer lastnb_(char *, ftnlen), frstnb_(char *, ftnlen);
    integer end;

/* $ Abstract */

/*     Retain only the significant digits in a numeric string. */

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
/*     PARSING */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     IN         I   Input numeric string. */
/*     OUT        O   Numeric string, with insignificant digits removed. */

/* $ Detailed_Input */

/*     IN       is a numeric string. */

/* $ Detailed_Output */

/*     OUT      is the same numeric string with insignificant */
/*              zeros and spaces removed. The special case '.000...' */
/*              becomes just '0'. OUT may overwrite IN. If the */
/*              output string is too long, it is truncated on the */
/*              right. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/*     1)  If IN is a non-numeric string, the contents of OUT are */
/*         unpredictable. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     There are only two interesting cases: */

/*        1) There is a decimal point and an exponent immediately */
/*           preceded by zero ('...0E', '...0D', '...0e', '...0d') */
/*           or by a space ('... E', '... D', '... e', '... d'). */

/*        2) There is a decimal point and no exponent, and the last non- */
/*           blank character is a zero ('...0'). */

/*     In each of these cases, go to the zero in question, and step */
/*     backwards until you find something other than a blank or a zero. */

/*     Finally, remove all leading spaces, and all occurrences of more */
/*     than one consecutive space within the string. */

/* $ Examples */

/*     The following examples illustrate the use of SIGDGT. */

/*     '0.123456000000D-04'        becomes     '0.123456D-04' */
/*     '  -9.2100000000000'                    '-9.21' */
/*     '       13'                             '13' */
/*     '    00013'                             '00013' */
/*     ' .314 159 265 300 000 e1'              '.314 159 265 3e1' */
/*     '   123    45     6'                    '123 45 6' */
/*     '  .000000000'                          '0' */

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

/* -    SPICELIB Version 1.1.0, 12-AUG-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU) (HAN) (NJB) (WLT) */

/* -& */
/* $ Index_Entries */

/*     retain significant digits */

/* -& */
/* $ Revisions */

/* -     Beta Version 1.3.0, 21-MAR-1989 (WLT) */

/*         Previous fix was unbelievably bad, very buggy. This */
/*         has been fixed along with other bugs and non-standard */
/*         code has been removed. */

/* -     Beta Version 1.2.0, 28-FEB-1989 (WLT) */

/*         Reference to INSSUB replaced by SUFFIX */

/* -     Beta Version 1.1.1, 17-FEB-1989 (HAN) (NJB) */

/*         Declaration of the unused function ISRCHC removed. */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     Find the first and last non-blank characters in the string. */

/* Computing MAX */
    i__1 = 1, i__2 = frstnb_(in, in_len);
    begin = max(i__1,i__2);
/* Computing MAX */
    i__1 = 1, i__2 = lastnb_(in, in_len);
    end = max(i__1,i__2);
    *(unsigned char *)lchar = ' ';

/*     Trivial case. */

    if (begin == end) {
	*(unsigned char *)out = *(unsigned char *)&in[begin - 1];
	if (i_len(out, out_len) > 1) {
	    s_copy(out + 1, " ", out_len - 1, (ftnlen)1);
	}

/*     If there is no decimal point, all zeros are significant. */

    } else if (i_indx(in, ".", in_len, (ftnlen)1) == 0) {
	l = 1;
	k = begin;
	while(l <= i_len(out, out_len) && k <= end) {
	    *(unsigned char *)&out[l - 1] = *(unsigned char *)&in[k - 1];

/*           Don't increment L if the last item copied was a space */
/*           (we don't want to copy extra spaces). */

	    if (*(unsigned char *)&in[k - 1] != ' ' || *(unsigned char *)
		    lchar != ' ') {
		++l;
	    }
	    *(unsigned char *)lchar = *(unsigned char *)&in[k - 1];
	    ++k;
	}
	if (l <= i_len(out, out_len)) {
	    s_copy(out + (l - 1), " ", out_len - (l - 1), (ftnlen)1);
	}
    } else {

/*        Is there is a decimal point and an exponent immediately */
/*        preceded by zero ('...0E', '...0D', '...0e', '...0d') or */
/*        by a space ('... E', '... D', '... e', '... d')? */

	zero = i_indx(in, "0E", in_len, (ftnlen)2);
	if (zero == 0) {
	    zero = i_indx(in, "0D", in_len, (ftnlen)2);
	}
	if (zero == 0) {
	    zero = i_indx(in, "0e", in_len, (ftnlen)2);
	}
	if (zero == 0) {
	    zero = i_indx(in, "0d", in_len, (ftnlen)2);
	}
	if (zero == 0) {
	    zero = i_indx(in, " E", in_len, (ftnlen)2);
	}
	if (zero == 0) {
	    zero = i_indx(in, " D", in_len, (ftnlen)2);
	}
	if (zero == 0) {
	    zero = i_indx(in, " e", in_len, (ftnlen)2);
	}
	if (zero == 0) {
	    zero = i_indx(in, " d", in_len, (ftnlen)2);
	}

/*        Begin there, and move toward the front of the string until */
/*        something other than a blank or a zero is encountered. Then */
/*        remove the superfluous characters. */

	if (zero > 0) {
	    j = zero + 1;
	    i__ = zero;
	    while(*(unsigned char *)&in[i__ - 1] == '0' || *(unsigned char *)&
		    in[i__ - 1] == ' ') {
		--i__;
	    }
	    l = 1;
	    k = begin;
	    while(l <= i_len(out, out_len) && k <= i__) {
		*(unsigned char *)&out[l - 1] = *(unsigned char *)&in[k - 1];

/*              Don't increment L if the last item copied was a space. */

		if (*(unsigned char *)&in[k - 1] != ' ' || *(unsigned char *)
			lchar != ' ') {
		    ++l;
		}
		*(unsigned char *)lchar = *(unsigned char *)&in[k - 1];
		++k;
	    }
	    k = j;
	    while(l <= i_len(out, out_len) && k <= end) {
		*(unsigned char *)&out[l - 1] = *(unsigned char *)&in[k - 1];

/*              Increment L only if we don't have two consecutive */
/*              spaces. */

		if (*(unsigned char *)&in[k - 1] != ' ' || *(unsigned char *)
			lchar != ' ') {
		    ++l;
		}
		*(unsigned char *)lchar = *(unsigned char *)&in[k - 1];
		++k;
	    }
	    if (l <= i_len(out, out_len)) {
		s_copy(out + (l - 1), " ", out_len - (l - 1), (ftnlen)1);
	    }


/*        Is there is a decimal point and no exponent, and is the last */
/*        non-blank character a zero ('...0')? Then truncate the string */
/*        after the last character that is neither a blank nor a zero. */

	} else if (*(unsigned char *)&in[end - 1] == '0' && cpos_(in, "EeDd", 
		&c__1, in_len, (ftnlen)4) == 0) {
	    i__ = end;
	    while(*(unsigned char *)&in[i__ - 1] == '0' || *(unsigned char *)&
		    in[i__ - 1] == ' ') {
		--i__;
	    }
	    l = 1;
	    k = begin;
	    while(l <= i_len(out, out_len) && k <= i__) {
		*(unsigned char *)&out[l - 1] = *(unsigned char *)&in[k - 1];

/*              Increment L only if we don't have two consecutive */
/*              spaces. */

		if (*(unsigned char *)&in[k - 1] != ' ' || *(unsigned char *)
			lchar != ' ') {
		    ++l;
		}
		*(unsigned char *)lchar = *(unsigned char *)&in[k - 1];
		++k;
	    }
	    if (l <= i_len(out, out_len)) {
		s_copy(out + (l - 1), " ", out_len - (l - 1), (ftnlen)1);
	    }
	} else {
	    l = 1;
	    k = begin;
	    while(l <= i_len(out, out_len) && k <= end) {
		*(unsigned char *)&out[l - 1] = *(unsigned char *)&in[k - 1];

/*              Increment L only if we don't have two consecutive spaces. */

		if (*(unsigned char *)&in[k - 1] != ' ' || *(unsigned char *)
			lchar != ' ') {
		    ++l;
		}
		*(unsigned char *)lchar = *(unsigned char *)&in[k - 1];
		++k;
	    }
	    if (l <= i_len(out, out_len)) {
		s_copy(out + (l - 1), " ", out_len - (l - 1), (ftnlen)1);
	    }
	}
    }

/*     Special case. The string '.0000....' reduces to '.' after the */
/*     zeros are removed. */

    if (s_cmp(out, ".", out_len, (ftnlen)1) == 0) {
	s_copy(out, "0", out_len, (ftnlen)1);
    }
    return 0;
} /* sigdgt_ */

