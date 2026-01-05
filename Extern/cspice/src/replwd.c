/* replwd.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure REPLWD ( Replace a word ) */
/* Subroutine */ int replwd_(char *instr, integer *nth, char *new__, char *
	outstr, ftnlen instr_len, ftnlen new_len, ftnlen outstr_len)
{
    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_cmp(char *, char *, ftnlen, ftnlen), i_len(char *, ftnlen);

    /* Local variables */
    integer f, i__, j, k, l, n, begin, shift;
    extern /* Subroutine */ int nthwd_(char *, integer *, char *, integer *, 
	    ftnlen, ftnlen);
    char short__[2];
    extern /* Subroutine */ int fndnwd_(char *, integer *, integer *, integer 
	    *, ftnlen);
    extern integer lastnb_(char *, ftnlen), frstnb_(char *, ftnlen);

/* $ Abstract */

/*     Replace the Nth word in a string with a new word. */

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

/*     ASSIGNMENT */
/*     WORD */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     INSTR      I   Input string. */
/*     NTH        I   Number of the word to be replaced. */
/*     NEW        I   Replacement word. */
/*     OUTSTR     O   Output string. */

/* $ Detailed_Input */

/*     INSTR    is the input character string, possibly containing */
/*              one or more words, where a word is any string of */
/*              consecutive non-blank characters delimited by a */
/*              blank or by either end of the string. */

/*     NTH      is the number of the word to be replaced. Words */
/*              are numbered from one. If NTH is less than one, */
/*              or greater than the number of words in the string, */
/*              no replacement is made. */

/*     NEW      is the word which is to replace the specified word */
/*              in the input string. Leading and trailing blanks */
/*              are ignored. If the replacement word is blank, */
/*              the original word is simply removed. */

/* $ Detailed_Output */

/*     OUTSTR   is the output string. This is the input string */
/*              with the N'th word replaced by the word NEW. */
/*              Any blanks originally surrounding the replaced */
/*              word are retained. */

/*              OUTSTR may overwrite INSTR. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/*     1)  If NEW is blank, then the Nth word is replaced by a single */
/*         space. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     The effect of this routine is to remove the old word with */
/*     REMSUB, and insert the replacement word with INSSUB. */

/* $ Examples */

/*     Let */
/*           INSTR  = '  Woodsy is the Anti-Pollution  Owl.' */

/*     and */
/*           NEW    = '   an   ' */

/*     then the following values of NTH yield the following strings. */

/*           NTH      OUTSTR */
/*           ---      ------------------------------------------ */
/*            -1      '  Woodsy is the Anti-Pollution  Owl.' */
/*             0      '  Woodsy is the Anti-Pollution  Owl.' */
/*             1      '  an is the Anti-Pollution  Owl.' */
/*             3      '  Woodsy is an Anti-Pollution  Owl.' */
/*             4      '  Woodsy is the an  Owl.' */
/*             5      '  Woodsy is the Anti-Pollution  an' */
/*             6      '  Woodsy is the Anti-Pollution  Owl.' */

/*     Note that in the first, second, and last cases, the string */
/*     was not changed. Note also that in the next to last case, */
/*     the final period was treated as part of the fifth word in the */
/*     string. */

/*     If NEW is ' ', and NTH is 3, then */

/*           OUTSTR = '  Woodsy is Anti-Pollution  Owl.' */

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

/*        Edited the header to comply with NAIF standard. Removed */
/*        unnecessary $Revisions section. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU) (WLT) (HAN) (NJB) */

/* -& */
/* $ Index_Entries */

/*     replace a word */

/* -& */

/*     SPICELIB functions */


/*   Local Variables */


/*     First just shift the input string into the output string, */
/*     then do everything in place (for the case when the new */
/*     word is longer than the old one.  When its shorter we'll */
/*     need to change this scheme slightly.) */

    s_copy(outstr, instr, outstr_len, instr_len);

/*     Where does the word to be replaced begin? If there is none, */
/*     just return the original string. */

    nthwd_(outstr, nth, short__, &begin, outstr_len, (ftnlen)2);
    if (begin == 0) {
	return 0;
    }

/*     Otherwise, find out where it ends as well. */

    fndnwd_(instr, &begin, &i__, &j, instr_len);

/*     Now insert only the non-blank part of the replacement string. */
/*     If the replacement string is blank, don't insert anything. */

    if (s_cmp(new__, " ", new_len, (ftnlen)1) != 0) {
	f = frstnb_(new__, new_len);
	l = lastnb_(new__, new_len);

/*        Except in the lucky case that the word to insert is the */
/*        same length as the word it's replacing, we will have */
/*        to shift right or left by some amount.  Compute the */
/*        appropriate amount to shift right. */

	shift = l - f - (j - i__);
    } else {
	f = 1;
	l = 1;
	shift = i__ - j;
    }
    if (shift > 0) {

/*        To shift right in place start at the right most character */
/*        of the string and copy the character SHIFT spaces to the */
/*        left. */

	k = i_len(outstr, outstr_len);
	n = k - shift;
	while(n > j) {
	    *(unsigned char *)&outstr[k - 1] = *(unsigned char *)&outstr[n - 
		    1];
	    --k;
	    --n;
	}

/*        Once the appropriate characters have been shifted out */
/*        of the way, replace the opened space with the new */
/*        word. */

	while(f <= l && i__ <= i_len(outstr, outstr_len)) {
	    *(unsigned char *)&outstr[i__ - 1] = *(unsigned char *)&new__[f - 
		    1];
	    ++f;
	    ++i__;
	}
    } else {

/*        We have a left shift. Fill in the first part of the word */
/*        we are replacing with the new one. */

	while(f <= l && i__ <= i_len(outstr, outstr_len)) {
	    *(unsigned char *)&outstr[i__ - 1] = *(unsigned char *)&new__[f - 
		    1];
	    ++f;
	    ++i__;
	}

/*        Now starting just past the end of the word we are replacing */
/*        shift the remainder of string left one character at a time. */

	if (shift < 0) {
	    ++j;
	    while(i__ <= i_len(outstr, outstr_len) && j <= i_len(instr, 
		    instr_len)) {
		*(unsigned char *)&outstr[i__ - 1] = *(unsigned char *)&instr[
			j - 1];
		++i__;
		++j;
	    }

/*           Finally pad the string with blanks. */

	    if (i__ <= i_len(outstr, outstr_len)) {
		s_copy(outstr + (i__ - 1), " ", outstr_len - (i__ - 1), (
			ftnlen)1);
	    }
	}
    }
    return 0;
} /* replwd_ */

