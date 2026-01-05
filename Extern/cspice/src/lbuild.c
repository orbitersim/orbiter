/* lbuild.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__0 = 0;

/* $Procedure LBUILD ( Build a list in a character string ) */
/* Subroutine */ int lbuild_(char *items, integer *n, char *delim, char *list,
	 ftnlen items_len, ftnlen delim_len, ftnlen list_len)
{
    /* System generated locals */
    integer i__1;

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer i_len(char *, ftnlen), s_cmp(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    integer dlen, ilen, llen, last, lpos, i__, first;
    extern integer lastnb_(char *, ftnlen), frstnb_(char *, ftnlen);
    extern /* Subroutine */ int suffix_(char *, integer *, char *, ftnlen, 
	    ftnlen);

/* $ Abstract */

/*     Build a list of items delimited by a character. */

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
/*     LIST */
/*     STRING */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     ITEMS      I   Items in the list. */
/*     N          I   Number of items in the list. */
/*     DELIM      I   String used to delimit items. */
/*     LIST       O   List of items delimited by DELIM. */

/* $ Detailed_Input */

/*     ITEMS    are the items to be combined to make the output */
/*              list. Leading and trailing blanks are ignored. */
/*              (Only the non-blank parts of the items are used.) */

/*     N        is the number of items. */

/*     DELIM    is the string used to delimit the items in the */
/*              output list. DELIM may contain any number of */
/*              characters, including blanks. */

/* $ Detailed_Output */

/*     LIST     is the output list, containing the N elements of */
/*              ITEMS delimited by DELIM. If LIST is not long enough */
/*              to contain the output list, it is truncated on the */
/*              right. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     The non-blank parts of the elements of the ITEMS array are */
/*     appended to the list, one at a time, separated by DELIM. */

/* $ Examples */

/*     The following examples illustrate the operation of LBUILD. */

/*     1) Let */
/*              DELIM    = ' ' */

/*              ITEMS(1) = 'A' */
/*              ITEMS(2) = '  number' */
/*              ITEMS(3) = 'of' */
/*              ITEMS(4) = ' words' */
/*              ITEMS(5) = 'separated' */
/*              ITEMS(6) = '  by' */
/*              ITEMS(7) = 'spaces' */

/*        Then */
/*              LIST  = 'A number of words separated by spaces' */

/*     2) Let */
/*              DELIM    = '/' */

/*              ITEMS(1) = ' ' */
/*              ITEMS(2) = ' ' */
/*              ITEMS(3) = 'option1' */
/*              ITEMS(4) = ' ' */
/*              ITEMS(5) = 'option2' */
/*              ITEMS(6) = ' ' */
/*              ITEMS(7) = ' ' */
/*              ITEMS(8) = ' ' */

/*        Then */
/*              LIST  = '//option1//option2///' */

/*     3) Let */
/*              DELIM    = ' and ' */

/*              ITEMS(1) = 'Bob' */
/*              ITEMS(2) = 'Carol' */
/*              ITEMS(3) = 'Ted' */
/*              ITEMS(4) = 'Alice' */

/*        Then */
/*              LIST  = 'Bob and Carol and Ted and Alice' */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     W.L. Taber         (JPL) */
/*     I.M. Underwood     (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 12-AUG-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU) */

/* -& */
/* $ Index_Entries */

/*     build a list in a character_string */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     Find the non-blank part of each item. Move it to the */
/*     end of the list, followed by a delimiter. If the item is */
/*     blank, don't move anything but the delimiter. */

/*     LPOS is the next position in the output list to be filled. */
/*     LLEN is the length of the output list. */
/*     DLEN is the length of DELIM. */
/*     ILEN is the length of the next item in the list. */

    s_copy(list, " ", list_len, (ftnlen)1);
    lpos = 1;
    llen = i_len(list, list_len);
    dlen = i_len(delim, delim_len);
    if (*n > 0) {
	i__1 = *n;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    if (lpos <= llen) {
		if (s_cmp(items + (i__ - 1) * items_len, " ", items_len, (
			ftnlen)1) == 0) {
		    s_copy(list + (lpos - 1), delim, list_len - (lpos - 1), 
			    delim_len);
		    lpos += dlen;
		} else {
		    first = frstnb_(items + (i__ - 1) * items_len, items_len);
		    last = lastnb_(items + (i__ - 1) * items_len, items_len);
		    ilen = last - first + 1;
		    s_copy(list + (lpos - 1), items + ((i__ - 1) * items_len 
			    + (first - 1)), list_len - (lpos - 1), last - (
			    first - 1));
		    suffix_(delim, &c__0, list, delim_len, list_len);
		    lpos = lpos + ilen + dlen;
		}
	    }
	}

/*     We're at the end of the list. Right now, the list ends in */
/*     a delimiter. Drop it. */

	if (lpos - dlen <= llen) {
	    i__1 = lpos - dlen - 1;
	    s_copy(list + i__1, " ", list_len - i__1, (ftnlen)1);
	}
    }
    return 0;
} /* lbuild_ */

