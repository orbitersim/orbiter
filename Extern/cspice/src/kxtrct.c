/* kxtrct.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure KXTRCT ( Extract a substring starting with a keyword ) */
/* Subroutine */ int kxtrct_(char *keywd, char *terms, integer *nterms, char *
	wordsq, logical *found, char *substr, ftnlen keywd_len, ftnlen 
	terms_len, ftnlen wordsq_len, ftnlen substr_len)
{
    /* System generated locals */
    integer i__1;

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    integer b, e;
    extern integer nblen_(char *, ftnlen);
    integer start, berase, eerase;
    extern integer isrchc_(char *, integer *, char *, ftnlen, ftnlen);
    integer delims;
    extern /* Subroutine */ int fndnwd_(char *, integer *, integer *, integer 
	    *, ftnlen);
    integer begstr;
    extern /* Subroutine */ int shiftl_(char *, integer *, char *, char *, 
	    ftnlen, ftnlen, ftnlen);
    extern integer wdindx_(char *, char *, ftnlen, ftnlen);
    integer endstr, positn;

/* $ Abstract */

/*     Locate a keyword in a string and extract the substring from */
/*     the beginning of the first word following the keyword to the */
/*     beginning of the first subsequent recognized terminator of a list. */

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
/*     PARSING */
/*     SEARCH */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     KEYWD      I   Word that marks the beginning of text of interest. */
/*     TERMS      I   Set of words, any of which marks the end of text. */
/*     NTERMS     I   Number of TERMS. */
/*     WORDSQ    I-O  String containing a sequence of words. */
/*     FOUND      O   .TRUE. if the keyword is found in the string. */
/*     SUBSTR     O   String from end of KEYWD to beginning of first */
/*                    TERMS item found. */

/* $ Detailed_Input */

/*     KEYWD    is a word used to mark the start of text of interest. */

/*     TERMS    is a set of words, any one of which may signal the end of */
/*              text of interest. */

/*     NTERMS   is the number of TERMS. */

/*     WORDSQ   is a character string made up of words, that may contain */
/*              the keyword in KEYWD. */

/* $ Detailed_Output */

/*     WORDSQ   is the input string stripped of all words from the */
/*              beginning of the keyword KEYWD to the end of the last */
/*              word preceding one of the words in TERMS (or the end of */
/*              the string if none of the TERMS follows KEYWD in the */
/*              string). */

/*     FOUND    is .TRUE. if KEYWD is present in the input WORDSQ. */

/*     SUBSTR   is the substring that begins with the first word */
/*              following KEYWD up to the beginning of any of the words */
/*              in TERM or the end of the string. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     Definitions: */

/*     A WORD        is a set of consecutive non-blank characters */
/*                   delimited by blanks or either end of the string */
/*                   that contains them. */

/*     Given a string and a keyword this routine locates the first */
/*     occurrence of the keyword in the string and returns the */
/*     substring between the end of the keyword and the first occurrence */
/*     of any of the words in a list of terminating words. If none */
/*     of the terminating words follows the keyword in the string, */
/*     the routine returns all of the string following the keyword. */

/*     If the next word following the keyword is a terminating word, */
/*     the substring returned will be a blank. */

/*     If the keyword can not be located in the string, the variable */
/*     FOUND will be returned as .FALSE. and the input string will be */
/*     unchanged. The substring will be returned as a blank. */

/*     In all other cases, the part of the input string from the */
/*     beginning of the keyword to the start of the first terminating */
/*     word will be removed. If no terminating word follows the keyword */
/*     the portion of the string from the keyword to the last non-blank */
/*     character of the string will be removed. */

/* $ Examples */

/*     Example 1. */
/*     ---------- */
/*       Input:  WORDSQ  'FROM 1 October 1984 12:00:00 TO 1 January 1987' */
/*               KEYWD   'TO' */
/*               TERMS   'FROM' */
/*                       'TO' */
/*                       'BEGINNING' */
/*                       'ENDING' */

/*       Output: WORDSQ  'FROM 1 October 1984 12:00:00 ' */
/*               FOUND   .TRUE. */
/*               SUBSTR  '1 January 1987' */


/*     Example 2. */
/*     ---------- */
/*       Input:  WORDSQ  'FROM 1 October 1984 12:00:00 TO 1 January 1987' */
/*               KEYWD   'FROM' */
/*               TERMS   'FROM' */
/*                       'TO' */
/*                       'BEGINNING' */
/*                       'ENDING' */

/*       Output: WORDSQ  ' TO 1 January 1987' */
/*               FOUND   .TRUE. */
/*               SUBSTR  '1 October 1984 12:00:00' */


/*     Example 3. */
/*     ---------- */
/*       Input:  WORDSQ  'ADDRESS: 4800 OAK GROVE DRIVE PHONE: 354-4321 ' */
/*               KEYWD   'ADDRESS:' */
/*               TERMS   'ADDRESS:' */
/*                       'PHONE:' */
/*                       'NAME:' */

/*       Output: WORDSQ  ' PHONE: 354-4321 ' */
/*               FOUND   .TRUE. */
/*               SUBSTR  '4800 OAK GROVE DRIVE' */


/*     Example 4. */
/*     ---------- */
/*       Input:  WORDSQ  'ADDRESS: 4800 OAK GROVE DRIVE PHONE: 354-4321 ' */
/*               KEYWD   'NAME:' */
/*               TERMS   'ADDRESS:' */
/*                       'PHONE:' */
/*                       'NAME:' */

/*       Output: WORDSQ  'ADDRESS: 4800 OAK GROVE DRIVE PHONE: 354-4321 ' */
/*               FOUND   .FALSE. */
/*               SUBSTR  ' ' */

/* $ Restrictions */

/*     1)  It is the user's responsibility to make sure there is adequate */
/*         room in SUBSTR to contain the substring. */

/*     2)  SUBSTR cannot overwrite WORDSQ. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     H.A. Neilan        (JPL) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 17-JUN-2021 (JDR) */

/*        Added IMPLICIT NONE statement. Changed the input argument */
/*        STRING to WORDSQ for consistency with other routines. */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (HAN) */

/* -& */
/* $ Index_Entries */

/*     extract a substring starting with a keyword */

/* -& */
/* $ Revisions */

/* -     Beta Version 1.1.0, 28-FEB-1989 (WLT) */

/*         Reference to REMSUB replaced by SHIFTL. */

/* -     Beta Version 1.0.1, 10-FEB-1989 (HAN) */

/*         Contents of the $Exceptions section was changed */
/*         to "error free" to reflect the decision that the */
/*         module will never participate in error handling. */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     Locate the keyword within the string. */

    positn = wdindx_(wordsq, keywd, wordsq_len, keywd_len);

/*     If the keyword wasn't found, set the outputs and head for home. */

    if (positn == 0) {
	*found = FALSE_;
	s_copy(substr, " ", substr_len, (ftnlen)1);
	return 0;
    } else {
	*found = TRUE_;
    }

/*     Set the begin erase marker to the start of the current word */
/*     Set the end   erase marker to the end   of the current word */

    berase = positn;
    eerase = positn + nblen_(keywd, keywd_len) - 1;
    start = eerase + 1;

/*     Find the begin and end of the next word. */

    fndnwd_(wordsq, &start, &b, &e, wordsq_len);

/*     If there is a next word ( E came back non-zero ) see if its a */
/*     terminator. */

    if (e != 0) {
	delims = isrchc_(wordsq + (b - 1), nterms, terms, e - (b - 1), 
		terms_len);
    }

/*     If we found a terminator, or were already at the end of the */
/*     string, we are done.  Remove the keyword and put a blank in */
/*     SUBSTR */

    if (e == 0 || delims != 0) {
	i__1 = eerase - berase + 1;
	shiftl_(wordsq + (berase - 1), &i__1, " ", wordsq + (berase - 1), 
		wordsq_len - (berase - 1), (ftnlen)1, wordsq_len - (berase - 
		1));
	s_copy(substr, " ", substr_len, (ftnlen)1);
	return 0;
    }

/*     Ok. If we made it this far,  we have at least one legitimate word */
/*     following the keyword,  set the pointer for the start of the */
/*     substring (to return) to the beginning of this word. */

    begstr = b;

/*     Now we just examine each word until we run out of string or we */
/*     run into a terminator. */

    while(e != 0 && delims == 0) {
	endstr = e;
	eerase = e;
	start = e + 1;
	fndnwd_(wordsq, &start, &b, &e, wordsq_len);
	if (e != 0) {
	    delims = isrchc_(wordsq + (b - 1), nterms, terms, e - (b - 1), 
		    terms_len);
	}
    }

/*     That's it, load the substring variable and remove the keyword */
/*     and words up to the terminator or end of the string --- whichever */
/*     came first. */

    s_copy(substr, wordsq + (begstr - 1), substr_len, endstr - (begstr - 1));
    i__1 = eerase - berase + 1;
    shiftl_(wordsq + (berase - 1), &i__1, " ", wordsq + (berase - 1), 
	    wordsq_len - (berase - 1), (ftnlen)1, wordsq_len - (berase - 1));
    return 0;
} /* kxtrct_ */

