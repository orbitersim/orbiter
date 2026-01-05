/* dxtrct.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure DXTRCT (Extract Double Precision Values From A String) */
/* Subroutine */ int dxtrct_(char *keywd, integer *maxwds, char *string, 
	integer *nfound, integer *parsed, doublereal *values, ftnlen 
	keywd_len, ftnlen string_len)
{
    /* Builtin functions */
    integer s_cmp(char *, char *, ftnlen, ftnlen);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    integer pntr, i__, j;
    doublereal x;
    extern integer nblen_(char *, ftnlen);
    char error[80];
    integer start, fallbk, berase, eerase;
    extern /* Subroutine */ int fndnwd_(char *, integer *, integer *, integer 
	    *, ftnlen);
    integer length;
    extern integer lastnb_(char *, ftnlen);
    extern /* Subroutine */ int nparsd_(char *, doublereal *, char *, integer 
	    *, ftnlen, ftnlen);
    extern integer wdindx_(char *, char *, ftnlen, ftnlen);
    integer positn;

/* $ Abstract */

/*     Locate a keyword and succeeding numeric words within a string. */
/*     Parse and store the numeric words. Remove the keyword and */
/*     numeric words from the input string. */

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
/*     WORD */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     KEYWD      I   Keyword used to mark start of a set of numbers. */
/*     MAXWDS     I   Maximum number of numeric words that can be parsed */
/*     STRING    I-O  String potentially containing KEYWD and numbers. */
/*     NFOUND     O   Number of numeric words found following the KEYWD. */
/*     PARSED     O   Number of numeric words translated and returned. */
/*     VALUES     O   The double precision values for the numbers. */

/* $ Detailed_Input */

/*     KEYWD    is a word used to mark the start of a set of numeric */
/*              words of interest. */

/*     MAXWDS   is the maximum number of numeric words that can be */
/*              parsed and returned. */

/*     STRING   is a string potentially containing KEYWD and numbers. */

/* $ Detailed_Output */

/*     STRING   is the input string stripped of all parsed */
/*              numeric words. If there was room available to parse */
/*              all of the numeric words associated with KEYWD, the */
/*              keyword that marked the beginning of the parsed */
/*              numbers in the original string will also be removed. */

/*     NFOUND   is the number of numeric words that were found */
/*              following KEYWD but preceding the next non-numeric */
/*              word of the string. If the KEYWD is not present in */
/*              the string, NFOUND is returned as -1. If the keyword */
/*              is located but the next word in the string is */
/*              non-numeric NFOUND will be returned as 0. */

/*     PARSED   is the number of numeric words that were actually */
/*              parsed and stored in the output array VALUES. If no */
/*              values are parsed PARSED is returned as 0. */

/*     VALUES   are the double precision values for the parsed */
/*              numeric words that follow the first occurrence of the */
/*              keyword but precede the next non-numeric word. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     Definitions: */

/*        WORD           is a set of consecutive non-blank characters */
/*                       delimited by blanks or the end of the string */
/*                       that contains them. */

/*        NUMERIC WORD   a word that can be parsed by the */
/*                       SPICELIB routine NPARSD without error. All */
/*                       FORTRAN numeric representations are numeric */
/*                       words. In addition 'PI', 'Pi', 'pI', and 'pi' */
/*                       are all recognized as having the value: */

/*                          3.1415926535897932384626D0 */

/*                       See NPARSD FOR A a full description of */
/*                       legitimate numeric words. */

/*     Given a string and a keyword this routine locates the first */
/*     occurrence of the keyword in the string and returns the double */
/*     precision representations of up to MAXWDS succeeding numeric */
/*     words. All parsed numeric words are removed from the string. */
/*     If every numeric word following KEYWD but preceding the next */
/*     non-numeric word is parsed,  KEYWD will also be removed from */
/*     the string. */

/*     If the keyword cannot be located in the string, the variable */
/*     NFOUND will be returned as -1 and the string will be unchanged. */

/*     In all other cases, some part of the string (possibly all of it) */
/*     will be removed. */

/* $ Examples */

/*     Input   STRING  'LONGITUDE 39.2829  LATITUDE 24.27682' */
/*             KEYWD   'LONGITUDE' */
/*             MAXWDS   4 */

/*     Output: STRING  '  LATITUDE 24.27682' */
/*             NFOUND  1 */
/*             PARSED  1 */
/*             VALUES  3.92829D+01 */



/*     Input   STRING  'THIS IS A BAD STRING FOR NUMBERS' */
/*             KEYWD   'RADIUS' */
/*             MAXWDS  2 */

/*     Output: STRING  'THIS IS A BAD STRING FOR NUMBERS' */
/*             NFOUND  -1 */
/*             PARSED   0 */
/*             VALUES   (unchanged) */



/*     Input   STRING  'PRIMES 11  13 17 19 23 NON-PRIMES 12 14 15' */
/*             KEYWD   'PRIMES' */
/*             MAXWDS  3 */

/*     Output: STRING  'PRIMES  19 23 NON-PRIMES 12 14 15' */
/*             NFOUND  5 */
/*             PARSED  3 */
/*             VALUES  1.1D+01 */
/*                     1.3D+01 */
/*                     1.7D+01 */

/*     Input   STRING  'PRIMES 11  13 17 19 23 NON-PRIMES 12 14 15' */
/*             KEYWD   'PRIMES' */
/*             MAXWDS  5 */

/*     Output: STRING  ' NON-PRIMES 12 14 15' */
/*             NFOUND  5 */
/*             PARSED  5 */
/*             VALUES  1.1D+01 */
/*                     1.3D+01 */
/*                     1.7D+01 */
/*                     1.9D+01 */
/*                     2.3D+01 */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     H.A. Neilan        (JPL) */
/*     W.L. Taber         (JPL) */
/*     I.M. Underwood     (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.2.0, 05-JUN-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 1.1.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.1.0, 23-MAY-1990 (HAN) */

/*        The variable FOUND was changed to NFOUND. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (IMU) */

/* -& */
/* $ Index_Entries */

/*     extract d.p. values from a string */

/* -& */
/* $ Revisions */

/* -     SPICELIB Version 1.1.0, 23-MAY-1990 (HAN) */

/*         The variable FOUND was changed to NFOUND. Other SPICELIB */
/*         routines that use the variable FOUND declare it as a logical. */
/*         In order to conform to this convention, FOUND was changed to */
/*         NFOUND to indicate that it has an integer value, not a logical */
/*         value. */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     No keywords or numbers have been located yet. */

    *nfound = 0;
    *parsed = 0;

/*     Locate the keyword within the string and get the length of the */
/*     string. */

    positn = wdindx_(string, keywd, string_len, keywd_len);
    length = lastnb_(string, string_len);
    if (positn == 0) {
	*nfound = -1;
	*parsed = 0;
	return 0;
    }

/*     Set the begin erase marker to the start of the current word */
/*     Set the end   erase marker to the end   of the current word */

    berase = positn;
    eerase = positn + nblen_(keywd, keywd_len) - 1;
    start = eerase + 1;
    if (start < length) {

/*        Locate the next word and try to parse it ... */

	fndnwd_(string, &start, &i__, &j, string_len);
	nparsd_(string + (i__ - 1), &x, error, &pntr, j - (i__ - 1), (ftnlen)
		80);
	if (s_cmp(error, " ", (ftnlen)80, (ftnlen)1) == 0) {

/*           ...  mark its starting position as a possible starting */
/*           point for deletion if we run out of room for parsed numbers. */

	    fallbk = i__;
	    eerase = j;
	    start = j + 1;
	    ++(*nfound);
	    ++(*parsed);
	    values[*parsed - 1] = x;
	}
    } else {
	s_copy(string + (berase - 1), " ", string_len - (berase - 1), (ftnlen)
		1);
	return 0;
    }

/*     Now find all of the succeeding numeric words until we run out of */
/*     numeric words or string to look at. */

    while(start < length && s_cmp(error, " ", (ftnlen)80, (ftnlen)1) == 0) {

/*        Find the next word and try to parse it as a number. */

	fndnwd_(string, &start, &i__, &j, string_len);
	nparsd_(string + (i__ - 1), &x, error, &pntr, j - (i__ - 1), (ftnlen)
		80);
	if (s_cmp(error, " ", (ftnlen)80, (ftnlen)1) == 0) {

/*           It's a number! Congratulations! */

	    ++(*nfound);

/*           If there is room ... */

	    if (*nfound <= *maxwds) {

/*              1.  Increment the counter PARSED. */
/*              2.  Load the DP value into the output array. */
/*              3.  Set the pointer for the end of the erase */
/*                   region to be the end of this word. */

		++(*parsed);
		values[*parsed - 1] = x;
		eerase = j;
	    } else {

/*              Set the pointer of the begin erase region to be the */
/*              the pointer set up just for this occasion. */

		berase = fallbk;
	    }

/*           Set the place to begin looking for the next word to be */
/*           at the first character following the end of the current */
/*           word. */

	    start = j + 1;
	}
    }

/*     Remove the parsed words from the string. */

    i__ = berase;
    j = eerase + 1;
    while(j <= length) {
	*(unsigned char *)&string[i__ - 1] = *(unsigned char *)&string[j - 1];
	++i__;
	++j;
    }
    s_copy(string + (i__ - 1), " ", string_len - (i__ - 1), (ftnlen)1);
    return 0;
} /* dxtrct_ */

