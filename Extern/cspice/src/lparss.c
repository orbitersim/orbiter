/* lparss.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__0 = 0;

/* $Procedure LPARSS ( Parse a list of items; return a set. ) */
/* Subroutine */ int lparss_(char *list, char *delims, char *set, ftnlen 
	list_len, ftnlen delims_len, ftnlen set_len)
{
    /* Builtin functions */
    integer s_cmp(char *, char *, ftnlen, ftnlen), i_indx(char *, char *, 
	    ftnlen, ftnlen);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    char bchr[1], echr[1];
    integer nmax, b, e, n;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    logical valid;
    extern integer sizec_(char *, ftnlen);
    extern logical failed_(void);
    extern /* Subroutine */ int scardc_(integer *, char *, ftnlen), validc_(
	    integer *, integer *, char *, ftnlen);
    extern integer lastnb_(char *, ftnlen);
    extern /* Subroutine */ int chkout_(char *, ftnlen), insrtc_(char *, char 
	    *, ftnlen, ftnlen);
    extern logical return_(void);
    integer eol;

/* $ Abstract */

/*     Parse a list of items delimited by multiple delimiters, */
/*     placing the resulting items into a set. */

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

/*     CELLS */
/*     SETS */

/* $ Keywords */

/*     CHARACTER */
/*     PARSING */
/*     SETS */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     LIST       I   List of items delimited by DELIMS on input. */
/*     DELIMS     I   Single characters which delimit items. */
/*     SET        O   Items in the list, validated, left justified. */

/* $ Detailed_Input */

/*     LIST     is a list of items delimited by any one of the characters */
/*              in the string DELIMS. Consecutive delimiters, and */
/*              delimiters at the beginning and end of the list, are */
/*              considered to delimit blank items. A blank list is */
/*              considered to contain a single, blank item. Leading and */
/*              trailing blanks in list are ignored. */

/*     DELIMS   contains the individual characters which delimit the */
/*              items in the list. These may be any ASCII characters, */
/*              including blanks. */

/*              However, by definition, consecutive blanks are NOT */
/*              considered to be consecutive delimiters. Nor are a blank */
/*              and any other delimiter considered to be consecutive */
/*              delimiters. In addition, leading and trailing blanks are */
/*              ignored. */

/* $ Detailed_Output */

/*     SET      is a SPICE set containing the items in the list, left */
/*              justified. Any item in the list too long to fit into an */
/*              element of SET is truncated on the right. */

/*              The strings in SET will be sorted in increasing order, */
/*              and duplicates will be removed. Trailing blanks are */
/*              ignored in string comparisons. */

/*              The size of the set must be initialized prior to calling */
/*              LPARSS. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the size of the set is not large enough to accommodate all */
/*         of the items in the set, an error is signaled by a routine in */
/*         the call tree of this routine. */

/*     2)  If the string length of SET is too short to accommodate an */
/*         item, the item will be truncated on the right. */

/*     3)  If the string length of SET is too short to permit encoding of */
/*         integers via the SPICELIB routine ENCHAR, an error is signaled */
/*         by a routine in the call tree of this routine. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     None. */

/* $ Examples */

/*     The following examples illustrate the operation of LPARSS. */

/*     1) Let */
/*              LIST        = 'A number of words   separated   by */
/*                              spaces.' */
/*              DELIMS      = ' ,.' */
/*              SIZE (SET)  = 20 */

/*        Then */

/*              CARDC (SET) = 8 */

/*              SET (1)     = ' ' */
/*              SET (2)     = 'A' */
/*              SET (3)     = 'by' */
/*              SET (4)     = 'number' */
/*              SET (5)     = 'of' */
/*              SET (6)     = 'separated' */
/*              SET (7)     = 'spaces' */
/*              SET (8)     = 'words' */


/*     2) Let */

/*              LIST        = '  1986-187// 13:15:12.184 ' */
/*              DELIMS      = ' ,/-:' */
/*              SIZE (SET)  = 20 */

/*        Then */

/*              CARDC (SET) = 6 */

/*              SET (1)     = ' ' */
/*              SET (2)     = '12.184' */
/*              SET (3)     = '13' */
/*              SET (4)     = '15' */
/*              SET (5)     = '187' */
/*              SET (6)     = '1986' */


/*     3) Let   LIST        = '  ,This,  is, ,an,, example, ' */
/*              DELIMS      = ' ,' */
/*              SIZE (SET)  = 20 */

/*        Then */
/*              CARDC (SET) = 5 */

/*              SET (1)     = ' ' */
/*              SET (2)     = 'This' */
/*              SET (3)     = 'an' */
/*              SET (4)     = 'example' */
/*              SET (5)     = 'is' */


/*     4) Let   LIST        = 'Mary had a little lamb, little lamb */
/*                             whose fleece was white      as snow.' */
/*              DELIMS      = ' ,.' */
/*              SIZE (SET)  = 6 */

/*        An error would be signaled because the set is not */
/*        large enough to accommodate all of the items in the */
/*        list. */


/*     5) Let   LIST        = '1 2 3 4 5 6 7 8 9 10.' */
/*              DELIMS      = ' .' */
/*              SIZE (SET)  = 10 */

/*        An error would be signaled because the set is not */
/*        large enough to accommodate all of the items in the */
/*        list. Note that delimiters at the end (or beginning) */
/*        of list are considered to delimit blank items. */


/*     6) Let   LIST        = '1 2 3 4 5 6 7 8 9 10.' */
/*              DELIMS      = '.' */
/*              SIZE (SET)  = 10 */

/*        Then */

/*              CARDC (SET) = 2 */

/*              SET (1)     = ' ' */
/*              SET (2)     = '1 2 3 4 5 6 7 8 9 10' */

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

/* -    SPICELIB Version 1.2.0, 24-AUG-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. Improved */
/*        documentation of arguments LIST, DELIM and SET. */

/*        Updated entries #2 and #3 in $Exceptions section: changed */
/*        wrong argument name, and indicated that the routine used */
/*        for encoding is part of SPICELIB. */

/* -    SPICELIB Version 1.1.0, 26-OCT-2005 (NJB) */

/*        Bug fix: code was modified to avoid out-of-range */
/*        substring bound conditions. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (HAN) (IMU) */

/* -& */
/* $ Index_Entries */

/*     parse a list of items and return a set */

/* -& */
/* $ Revisions */

/* -    SPICELIB Version 1.1.0, 26-OCT-2005 (NJB) */

/*        Bug fix: code was modified to avoid out-of-range */
/*        substring bound conditions. The previous version */
/*        of this routine used DO WHILE statements of the form */

/*                  DO WHILE (      ( B         .LE. EOL   ) */
/*           .                .AND. ( LIST(B:B) .EQ. BLANK ) ) */

/*        Such statements can cause index range violations when the */
/*        index B is greater than the length of the string LIST. */
/*        Whether or not such violations occur is platform-dependent. */


/* -    Beta Version 2.0.0, 10-JAN-1989 (HAN) */

/*        Error handling was added, and old error flags and their */
/*        checks were removed. An error is signaled if the set */
/*        is not large enough to accommodate all of the items in */
/*        the list. */

/*        The header documentation was updated to reflect the error */
/*        handling changes, and more examples were added. */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     Local variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("LPARSS", (ftnlen)6);
    }

/*     Because speed is essential in many list parsing applications, */
/*     LPARSS, like LPARSE, parses the input list in a single pass. */
/*     What follows is nearly identical to LPARSE, except the FORTRAN */
/*     INDEX function is used to test for delimiters, instead of testing */
/*     each character for simple equality. Also, the items are inserted */
/*     into a set instead of simply placed at the end of an array. */

/*     No items yet. */

    n = 0;

/*     What is the size of the set? */

    nmax = sizec_(set, set_len);

/*     The array has not been validated yet. */

    valid = FALSE_;

/*     Blank list contains a blank item.  No need to validate. */

    if (s_cmp(list, " ", list_len, (ftnlen)1) == 0) {
	scardc_(&c__0, set, set_len);
	insrtc_(" ", set, (ftnlen)1, set_len);
	valid = TRUE_;
    } else {

/*        Eliminate trailing blanks.  EOL is the last non-blank */
/*        character in the list. */

	eol = lastnb_(list, list_len);

/*        As the King said to Alice: 'Begin at the beginning. */
/*        Continue until you reach the end. Then stop.' */

/*        When searching for items, B is the beginning of the current */
/*        item; E is the end.  E points to the next non-blank delimiter, */
/*        if any; otherwise E points to either the last character */
/*        preceding the next item, or to the last character of the list. */

	b = 1;
	while(b <= eol) {

/*           Skip any blanks before the next item or delimiter. */

/*           At this point in the loop, we know */

/*              B <= EOL */

	    *(unsigned char *)bchr = *(unsigned char *)&list[b - 1];
	    while(b <= eol && *(unsigned char *)bchr == 32) {
		++b;
		if (b <= eol) {
		    *(unsigned char *)bchr = *(unsigned char *)&list[b - 1];
		}
	    }

/*           At this point B is the index of the next non-blank */
/*           character BCHR, or else */

/*              B == EOL + 1 */

/*           The item ends at the next delimiter. */

	    e = b;
	    if (e <= eol) {
		*(unsigned char *)echr = *(unsigned char *)&list[e - 1];
	    } else {
		*(unsigned char *)echr = ' ';
	    }
	    while(e <= eol && i_indx(delims, echr, delims_len, (ftnlen)1) == 
		    0) {
		++e;
		if (e <= eol) {
		    *(unsigned char *)echr = *(unsigned char *)&list[e - 1];
		}
	    }

/*           (This is different from LPARSE. If the delimiter was */
/*           a blank, find the next non-blank character. If it's not */
/*           a delimiter, back up. This prevents constructions */
/*           like 'a , b', where the delimiters are blank and comma, */
/*           from being interpreted as three items instead of two. */
/*           By definition, consecutive blanks, or a blank and any */
/*           other delimiter, do not count as consecutive delimiters.) */

	    if (e <= eol && *(unsigned char *)echr == 32) {

/*              Find the next non-blank character. */

		while(e <= eol && *(unsigned char *)echr == 32) {
		    ++e;
		    if (e <= eol) {
			*(unsigned char *)echr = *(unsigned char *)&list[e - 
				1];
		    }
		}
		if (e <= eol) {
		    if (i_indx(delims, echr, delims_len, (ftnlen)1) == 0) {

/*                    We're looking at a non-delimiter character. */

/*                    E is guaranteed to be > 1 if we're here, so the */
/*                    following subtraction is valid. */

			--e;
		    }
		}
	    }

/*           The item now lies between B and E. Unless, of course, B and */
/*           E are the same character; this can happen if the list */
/*           starts or ends with a non-blank delimiter, or if we have */
/*           stumbled upon consecutive delimiters. */

	    if (! valid) {

/*              If the array has not been validated, it's just an */
/*              array, and we can insert items directly into it. */
/*              Unless it's full, in which case we validate now and */
/*              insert later. */

		if (n < nmax) {
		    ++n;
		    if (e > b) {
			s_copy(set + (n + 5) * set_len, list + (b - 1), 
				set_len, e - 1 - (b - 1));
		    } else {
			s_copy(set + (n + 5) * set_len, " ", set_len, (ftnlen)
				1);
		    }
		} else {
		    validc_(&nmax, &nmax, set, set_len);
		    valid = TRUE_;
		}
	    }

/*           Once the set has been validated, the strings are inserted */
/*           into the set if there's room. If there is not enough room */
/*           in the set, let INSRTC signal the error. */

	    if (valid) {
		if (e > b) {
		    insrtc_(list + (b - 1), set, e - 1 - (b - 1), set_len);
		} else {
		    insrtc_(" ", set, (ftnlen)1, set_len);
		}
		if (failed_()) {
		    chkout_("LPARSS", (ftnlen)6);
		    return 0;
		}
	    }

/*           If there are more items to be found, continue with the */
/*           character following E (which is a delimiter). */

	    b = e + 1;
	}

/*        If the array has not yet been validated, validate it before */
/*        returning. */

	if (! valid) {
	    validc_(&nmax, &n, set, set_len);
	}

/*        If the list ended with a (non-blank) delimiter, insert a */
/*        blank item into the set. If there isn't any room, signal */
/*        an error. */

	if (i_indx(delims, list + (eol - 1), delims_len, (ftnlen)1) != 0) {
	    insrtc_(" ", set, (ftnlen)1, set_len);

/*           If INSRTC failed to insert the blank because the set */
/*           was already full, INSRTC will have signaled an error. */
/*           No action is necessary here. */

	}
    }
    chkout_("LPARSS", (ftnlen)6);
    return 0;
} /* lparss_ */

