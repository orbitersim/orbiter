/* wdindx.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure WDINDX ( Index of a Word Within a String ) */
integer wdindx_(char *string, char *word, ftnlen string_len, ftnlen word_len)
{
    /* System generated locals */
    integer ret_val, i__1, i__2;

    /* Builtin functions */
    integer s_cmp(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    integer i__, j, begwd, endwd, wdlen, bgtond;
    extern integer lastnb_(char *, ftnlen);
    integer begstr;
    extern integer frstnb_(char *, ftnlen);
    integer endstr, strlen;

/* $ Abstract */

/*     Find the index of a word within a string. If the word does not */
/*     exist as a word within the string, the value zero is returned. */

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

/*     PARSING */
/*     SEARCH */
/*     WORD */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     STRING     I   String of characters, potentially containing words */
/*     WORD       I   A string of consecutive printable letters. */

/*     The function returns the location of WORD within STRING. */

/* $ Detailed_Input */

/*     STRING   is a string of characters, potentially containing the */
/*              word. */

/*              Leading and trailing blanks are not significant in */
/*              STRING. */

/*     WORD     is a string of consecutive printable characters. */

/*              Leading and trailing blanks are not significant in */
/*              WORD. */

/* $ Detailed_Output */

/*     The function returns the location of WORD within STRING, providing */
/*     the index of the first letter of WORD within the input STRING. If */
/*     WORD does not exist or WORD is blank, the function returns zero. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     A word within a string is a substring beginning and ending with a */
/*     non-blank characters that is delimited by blanks on each end. (A */
/*     blank is assumed to precede and follow the first and last */
/*     characters of a string.) */

/*     Given a word, this routine returns the index of the first letter */
/*     of the first word of STRING that matches the word. */

/* $ Examples */

/*     STRING: */
/*                      1         2         3         4 */
/*     WORD    1234567890123456789012345678901234567890123456    WDINDX */
/*     ------  ----------------------------------------------    ------ */
/*     'POT'   'PUT THE POTATOES IN THE POT'                     25 */
/*     'TOES'                                                    0 */
/*     'PUT'                                                     1 */
/*     'THE'                                                     5 */
/*     'IN THE'                                                  18 */
/*     'THE PO'                                                  0 */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 03-OCT-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) */

/* -& */
/* $ Index_Entries */

/*     index of a word within a string */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     Find the ends of the word and input string. */

    endstr = lastnb_(string, string_len);
    begstr = frstnb_(string, string_len);
    endwd = lastnb_(word, word_len);
    begwd = frstnb_(word, word_len);

/*     Get the offset from the beginning of the word to the end of the */
/*     word, the word length and the string length. */

    bgtond = endwd - begwd;
    wdlen = bgtond + 1;
    strlen = endstr + 1 - begstr;

/*     We deal with all of the pathologies first... */

    if (endwd < 1 || strlen < wdlen) {

/*        ... If we got a blank word or a string that is too short, then */
/*        the index of the word is zero. */

	ret_val = 0;
	return ret_val;
    } else if (strlen == wdlen) {

/*        ... the word and string have the same non-blank length. */
/*        Either they match up or they don't.  Find out and return. */

	if (s_cmp(string + (begstr - 1), word + (begwd - 1), endstr - (begstr 
		- 1), endwd - (begwd - 1)) == 0) {
	    ret_val = begstr;
	} else {
	    ret_val = 0;
	}
	return ret_val;
    }

/*     Ok.  Now we've got a realistic case to deal with.  The string */
/*     length is longer than the word length.  Check to see if we have a */
/*     match at the beginning of the string. */

    i__ = begstr;
    j = i__ + bgtond;
    i__1 = j;
    if (s_cmp(string + (i__ - 1), word + (begwd - 1), j - (i__ - 1), endwd - (
	    begwd - 1)) == 0 && s_cmp(string + i__1, " ", j + 1 - i__1, (
	    ftnlen)1) == 0) {
	ret_val = i__;
	return ret_val;
    }

/*     No luck yet?  Search the string until we find a word match or */
/*     we run out of string to check. */

    i__ = begstr + 1;
    j = i__ + bgtond;
    for(;;) { /* while(complicated condition) */
	i__1 = i__ - 2;
	i__2 = j;
	if (!(j < endstr && ! (s_cmp(string + (i__ - 1), word + (begwd - 1), 
		j - (i__ - 1), endwd - (begwd - 1)) == 0 && s_cmp(string + 
		i__1, " ", i__ - 1 - i__1, (ftnlen)1) == 0 && s_cmp(string + 
		i__2, " ", j + 1 - i__2, (ftnlen)1) == 0)))
		break;
	++i__;
	++j;
    }

/*     If J equals ENDSTR then no match was found in the interior of the */
/*     string.  We make a last check at the end. */

    if (j == endstr) {
	i__1 = i__ - 2;
	if (s_cmp(string + i__1, " ", i__ - 1 - i__1, (ftnlen)1) == 0 && 
		s_cmp(string + (i__ - 1), word + (begwd - 1), j - (i__ - 1), 
		endwd - (begwd - 1)) == 0) {
	    ret_val = i__;
	} else {
	    ret_val = 0;
	}
    } else {

/*        The only way to get here is if we exited the above loop before */
/*        running out of room --- that is we had a word match.  Set */
/*        the index to the value of "I" that got us out of the loop. */

	ret_val = i__;
    }
    return ret_val;
} /* wdindx_ */

