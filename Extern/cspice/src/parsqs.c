/* parsqs.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure PARSQS ( Parse quoted string token ) */
/* Subroutine */ int parsqs_(char *string, char *qchar, char *value, integer *
	length, logical *error, char *errmsg, integer *ptr, ftnlen string_len,
	 ftnlen qchar_len, ftnlen value_len, ftnlen errmsg_len)
{
    /* System generated locals */
    integer i__1;

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer i_len(char *, ftnlen), s_cmp(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    integer last, ipos, opos, inlen, first;
    extern integer lastnb_(char *, ftnlen), frstnb_(char *, ftnlen);
    integer outlen;
    char chr[1];

/* $ Abstract */

/*     Parse a quoted string token. */

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
/*     SCANNING */
/*     STRING */
/*     UTILITY */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     STRING     I   Quoted string to be parsed. */
/*     QCHAR      I   Quote delimiter character. */
/*     VALUE      O   Parsed string. */
/*     LENGTH     O   Number of significant characters in VALUE. */
/*     ERROR      O   Logical error flag. */
/*     ERRMSG     O   Message indicating whether errors have occurred. */
/*     PTR        O   Position in string where an error occurred. */

/* $ Detailed_Input */

/*     STRING   is a character string containing a `quoted string */
/*              token'. Quoted string tokens are sequences of */
/*              characters that represent literal strings. */
/*              Syntactically, a string token is a sequence of */
/*              characters that begins and ends with a designated */
/*              `quote character'. Within the token, any */
/*              occurrence of the quote character is indicated by */
/*              an adjacent pair of quote characters: for example, */
/*              if the quote character is */

/*                 " */

/*              then the token representing one instance of this */
/*              character is */

/*                 """" */

/*              Here the first quote indicates the beginning of the */
/*              token, the next two quotes together indicate a */
/*              single quote character that constitutes the */
/*              `contents' of the token, and the final quote */
/*              indicates the end of the token. */

/*              Leading and trailing blanks in STRING are ignored. */
/*              The input string may not contain any trailing, */
/*              non-blank characters after the final quote */
/*              character. */

/*              All blanks occurring between the bracketing */
/*              quote characters in STRING are significant. */


/*     QCHAR    is the quote character. This is always a single */
/*              character. The characters */

/*                 "  and ' */

/*              are common choices, but any non-blank character is */
/*              accepted. Case *is* significant in QCHAR. */

/* $ Detailed_Output */

/*     VALUE    is the string resulting from parsing STRING. */
/*              VALUE is obtained from STRING by removing the */
/*              bracketing quote characters and replacing each pair */
/*              of quote characters in the interior of STRING with */
/*              a singleton quote character. The value resulting */
/*              from parsing STRING will occupy the leftmost */
/*              characters of VALUE, but will not be */
/*              `left-justified', since leading blanks within */
/*              the quoted string token in STRING are significant. */

/*     LENGTH   is the number of significant characters in VALUE. */
/*              This is the number of characters in the string */
/*              resulting from parsing the input string. Because */
/*              parsed strings containing embedded quote */
/*              characters are shorter than the unparsed tokens */
/*              that represent them, LENGTH may be less than the */
/*              number of characters between the bracketing quote */
/*              characters of the input string. */

/*     ERROR    is a logical flag indicating whether a parse error */
/*              occurred; if so, ERROR is returned with the value */
/*              .TRUE. */

/*     ERRMSG   is a message indicating that STRING could not be */
/*              parsed due to an error in its structure. If the */
/*              input string token was successfully parsed, ERRMSG */
/*              will be returned as a blank string. */

/*     PTR      indicates the character position at which an */
/*              error in STRING was detected. If STRING is */
/*              correctly formed, PTR is returned as 0. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/*     1)  If the input argument QCHAR is blank, a parse error will be */
/*         indicated by ERROR; PTR will be set to 1. The contents of */
/*         VALUE and LENGTH are undefined in this case. */

/*     2)  If STRING is not a well-formed quoted string, a parse error */
/*         will be indicated by ERROR and PTR. The contents of VALUE */
/*          and LENGTH are undefined in this case. */

/*     3)  If the length of the output string VALUE is too short to */
/*         accommodate the parsed string token produced by this routine, */
/*         a parse error message to this effect is generated.  VALUE */
/*         will contain the as much as possible of the result, truncated */
/*         on the right. */

/*     4)  If STRING consists of a null string token, that is, two */
/*         adjacent quote characters with nothing but blanks on either */
/*         side, a parse error will be indicated. The contents of VALUE */
/*          and LENGTH are undefined in this case. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     Quote characters may be ANY non-blank character. For example, the */
/*     ampersand */

/*        & */

/*     is a perfectly valid quote character. If we were using the */
/*     ampersand as the quote character, then the term `doubled quote' */
/*     in the following discussion would refer to the sequence */

/*        && */

/*     not the character */

/*        " */

/*     The string tokens that are expected inputs to this routine are */
/*     Fortran-style quoted strings: they start and end with quote */
/*     characters. In the interior of any such token, any quote */
/*     characters are represented by doubled quote characters. These */
/*     rules imply that the number of quote characters in a valid quoted */
/*     string token is always even. The end of a quoted string token is */
/*     located at the first even-numbered quote character, counting from */
/*     the initial quote character, that is  not the first member of a */
/*     pair of quotes indicating an embedded quote character. */

/*     This routine is meant to be used together with the SPICELIB */
/*     routine LXQSTR (Lex quoted string):  LXQSTR is used to identify */
/*     quoted string tokens, and this routine converts the tokens to */
/*     string values. */

/* $ Examples */

/*     1)  The table below illustrates the action of this routine. */


/*     STRING               QCHAR   VALUE           LENGTH       ERROR */
/*     ================================================================= */
/*     "SPICE"              "       SPICE           5            .FALSE. */
/*     "SPICE"              '       <undefined>     <undefined>  .TRUE. */
/*     """SPICE"" system"   "       "SPICE" system  14           .FALSE. */
/*     " "                  "       <single blank>  1            .FALSE. */
/*     ''                   '       <undefined>     <undefined>  .TRUE. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.2.0, 03-OCT-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 1.1.0, 08-MAY-1996 (WLT) */

/*        Corrected the problem with an uninitialized variable */
/*        INLEN that was detected on the HP and reported by Steve */
/*        Schlaifer of MASL. */

/* -    SPICELIB Version 1.0.0, 21-NOV-1994 (NJB) */

/* -& */
/* $ Index_Entries */

/*     parse quoted string token */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     Error free, no check-in required.  No parse error to start with. */
/*     No characters in the parsed string to start with. */

    *error = FALSE_;
    s_copy(errmsg, " ", errmsg_len, (ftnlen)1);
    *ptr = 0;
    *length = 0;

/*     Reject invalid quote characters. */

    if (*(unsigned char *)qchar == ' ') {
	*error = TRUE_;
	s_copy(errmsg, "The quote character must be non-blank, but isn't", 
		errmsg_len, (ftnlen)48);
	*ptr = 1;
    }

/*     Grab the lengths of the string arguments. */

    inlen = i_len(string, string_len);
    outlen = i_len(value, value_len);

/*     The token to be parsed extends from the first non-blank */
/*     character to the last non-blank character of STRING. */

    first = frstnb_(string, string_len);
    last = lastnb_(string, string_len);
    if (first == 0) {
	*error = TRUE_;
	s_copy(errmsg, "Blank input string", errmsg_len, (ftnlen)18);
	*ptr = inlen;
	return 0;
    }

/*     The input token must be bracketed by quote characters. */

    if (*(unsigned char *)&string[first - 1] != *(unsigned char *)qchar) {
	*error = TRUE_;
	s_copy(errmsg, "String token does not start with quote character", 
		errmsg_len, (ftnlen)48);
	*ptr = first;
	return 0;
    } else if (*(unsigned char *)&string[last - 1] != *(unsigned char *)qchar)
	     {
	*error = TRUE_;
	s_copy(errmsg, "String token does not end with quote character", 
		errmsg_len, (ftnlen)46);
	*ptr = last;
	return 0;
    }

/*     Null strings are not accepted. */

    if (first == last - 1) {
	*error = TRUE_;
	s_copy(errmsg, "Null (zero length) string token", errmsg_len, (ftnlen)
		31);
	*ptr = last;
	return 0;
    }

/*     Transfer the interior characters of the input string to the output */
/*     string, replacing each doubled quote character with a single quote */
/*     character.  The interior of the string must not contain any */
/*     un-doubled quotes; we have a parse error if we find any such */
/*     stragglers. */

    opos = 1;
    ipos = first + 1;
    while(ipos <= last - 1 && opos <= outlen) {

/*        At this point, IPOS points to the current input character to */
/*        examine; OPOS points to the currently available position to */
/*        write to in the output string. */

	*(unsigned char *)chr = *(unsigned char *)&string[ipos - 1];
	if (*(unsigned char *)chr != *(unsigned char *)qchar) {

/*           This is the normal, non-quote case.  Transfer the */
/*           character to the output string and advance both the input */
/*           and output character positions. */

	    *(unsigned char *)&value[opos - 1] = *(unsigned char *)chr;
	    ++ipos;
	    ++opos;
	    ++(*length);
	} else {

/*           We've encountered a quote character.  By construction, the */
/*           parity of this quote character must be odd.  The quote must */
/*           be followed immediately by a second, interior quote. */

	    if (ipos == last - 1) {

/*              We're already looking at the last interior input */
/*              character. */

		*error = TRUE_;
		s_copy(errmsg, "Quote character is unmatched or else string "
			"ends without final quote; take your pick", errmsg_len,
			 (ftnlen)84);
		*ptr = ipos;
		return 0;
	    } else /* if(complicated condition) */ {
		i__1 = ipos;
		if (s_cmp(string + i__1, qchar, ipos + 1 - i__1, (ftnlen)1) !=
			 0) {
		    *error = TRUE_;
		    s_copy(errmsg, "Interior quote character is not doubled", 
			    errmsg_len, (ftnlen)39);
		    *ptr = ipos;
		    return 0;
		} else {

/*              This is the normal case; the quote character is doubled. */
/*              Transfer a single quote character to the output string, */
/*              and skip over the second quote in the input string. */

		    *(unsigned char *)&value[opos - 1] = *(unsigned char *)
			    chr;
		    ++opos;
		    ++(*length);
		    ipos += 2;
		}
	    }
	}
    }
    if (ipos < last - 1) {

/*        We must have stopped transferring characters to VALUE */
/*        because we ran out of room. */

	*error = TRUE_;
	s_copy(errmsg, "Output string too short, truncated on right", 
		errmsg_len, (ftnlen)43);
	*ptr = ipos;
	return 0;
    }
    if (opos < outlen) {

/*        Blank-pad the trailing portion of the output string. */

	s_copy(value + (opos - 1), " ", value_len - (opos - 1), (ftnlen)1);
    }
    return 0;
} /* parsqs_ */

