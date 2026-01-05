/* zzektloc.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure      ZZEKTLOC ( EK, locate token in tokenized EK query ) */
/* Subroutine */ int zzektloc_(integer *tokid, integer *kwcode, integer *
	ntoken, integer *tokens, integer *values, integer *loc, logical *
	found)
{
/* $ Abstract */

/*     Locate the first occurrence of a specified token in a tokenized */
/*     EK query.  The input may actually be any subset of token codes */
/*     and corresponding keyword codes from a tokenized query. */

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

/*     EK */

/* $ Keywords */

/*     EK */
/*     PRIVATE */

/* $ Declarations */
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


/*     Include Section:  EK Token Code Parameters */

/*        ektokn.inc  Version 2    25-JAN-1995 (NJB) */

/*           Updated to distinguish between special characters. */


/*        ektokn.inc  Version 1    05-DEC-1994 (NJB) */


/*     The EK query language tokens and codes are: */

/*        <keyword> */
/*        <identifier> */
/*        <integer> */
/*        <d.p. number> */
/*        <quoted string> */
/*        <left parenthesis> */
/*        <right parenthesis> */
/*        <comma> */
/*        <period> */
/*        <end of query> */



/*     End Include Section:  EK Token Code Parameters */

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


/*     Include Section:  EK Keyword Code Parameters */

/*        ekkeyw.inc  Version 4    24-JAN-1995 (NJB) */



/*     The EK query language keywords and codes are: */

/*        ALL */
/*        AND */
/*        ASC */
/*        AVG */
/*        BETWEEN */
/*        BY */
/*        COUNT */
/*        DESC */
/*        DISTINCT */
/*        EQ */
/*        FROM */
/*        GE */
/*        GROUP */
/*        GT */
/*        HAVING */
/*        IS */
/*        LE */
/*        LT */
/*        LIKE */
/*        MAX */
/*        MIN */
/*        NE */
/*        NOT */
/*        NULL */
/*        OR */
/*        ORDER */
/*        SELECT */
/*        SUM */
/*        WHERE */


/*     End Include Section:  EK Keyword Code Parameters */

/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     TOKID      I   Token ID. */
/*     KWCODE     I   Keyword code. */
/*     NTOKEN     I   Number of tokens in query. */
/*     TOKENS     I   Token codes. */
/*     VALUES     I   Pointers to numeric and string token values. */
/*     LOC        O   Location of first occurrence of token. */
/*     FOUND      O   Flag indicating whether token was found. */

/* $ Detailed_Input */

/*     TOKID          is a token code identifying the type of token */
/*                    sought. */

/*     KWCODE         is a code that specifies the desired keyword, */
/*                    if the desired token is a keyword.  KWCODE is */
/*                    ignored if the desired token is not a keyword. */

/*     NTOKEN         is the number of tokens in the input query. */

/*     TOKENS         is an array of token codes.  This array normally */
/*                    represents a tokenized EK query or a sublist of */
/*                    such a query. */

/*     VALUES         is a list of values associated with the codes */
/*                    contained in TOKENS.  When the Ith element of */
/*                    TOKENS indicates that the Ith token is a keyword, */
/*                    the Ith element of VALUES contains the code */
/*                    specifying which keyword is meant. */

/* $ Detailed_Output */

/*     LOC            is the index in the input token list at which */
/*                    the desired token was first encountered.  LOC */
/*                    is meaningful only if FOUND is .TRUE. */

/*     FOUND          is a logical flag indicating whether the desired */
/*                    token was found. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     None. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This is a utility that simplifies parsing of tokenized EK */
/*     queries. */

/* $ Examples */

/*     See ZZEKPARS. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman   (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.1, 02-OCT-2021 (NJB) */

/*        Corrected typo in comment. */

/* -    Beta Version 1.0.0, 11-OCT-1995 (NJB) */

/* -& */

/*     Error free. */

    *found = FALSE_;
    *loc = 1;
    while(*loc <= *ntoken) {
	if (tokens[*loc - 1] == *tokid) {
	    if (*tokid == 1) {

/*              To get a match, the keyword codes must match. */

		if (values[*loc - 1] == *kwcode) {
		    *found = TRUE_;
		    return 0;
		}
	    } else {

/*              For non-keyword tokens, we're done at this point. */

		*found = TRUE_;
		return 0;
	    }
	}
	++(*loc);
    }
    return 0;
} /* zzektloc_ */

