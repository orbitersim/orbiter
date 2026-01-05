/* lparse.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure LPARSE ( Parse items from a list ) */
/* Subroutine */ int lparse_(char *list, char *delim, integer *nmax, integer *
	n, char *items, ftnlen list_len, ftnlen delim_len, ftnlen items_len)
{
    /* Builtin functions */
    integer s_cmp(char *, char *, ftnlen, ftnlen);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer i_len(char *, ftnlen);

    /* Local variables */
    char bchr[1], echr[1];
    integer b, e, eol;

/* $ Abstract */

/*     Parse a list of items delimited by a single character. */

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
/*     PARSING */
/*     STRING */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     LIST       I   List of items delimited by DELIM. */
/*     DELIM      I   Single character used to delimit items. */
/*     NMAX       I   Maximum number of items to return. */
/*     N          O   Number of items in the list. */
/*     ITEMS      O   Items in the list, left justified. */

/* $ Detailed_Input */

/*     LIST     is a list of items delimited by the single character */
/*              DELIM. Consecutive delimiters, and delimiters at the */
/*              beginning and end of the list, are considered to */
/*              delimit blank items. A blank list is considered to */
/*              contain a single (blank) item. */

/*     DELIM    is the character delimiting the items in the list. */
/*              This may be any ASCII character, including a blank. */
/*              However, by definition, consecutive blanks are NOT */
/*              considered to be consecutive delimiters. In addition, */
/*              leading and trailing blanks are ignored. */

/*     NMAX     is the maximum number of items to be returned from */
/*              the list. This allows the user to guard against */
/*              overflow from a list containing more items than */
/*              expected. */

/* $ Detailed_Output */

/*     N        is the number of items in the list. N may be */
/*              any number between one and NMAX. N is always the */
/*              number of delimiters plus one. */

/*     ITEMS    are the items in the list, left justified. Any item */
/*              in the list too long to fit into an element of ITEMS */
/*              is truncated on the right. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/*     1)  If the string length of ITEMS is too short to accommodate */
/*         an item, the item will be truncated on the right. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     None. */

/* $ Examples */

/*     The numerical results shown for these examples may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */

/*     1) Parse a character string to retrieve the words contained */
/*        within. */

/*        Example code begins here. */


/*              PROGRAM LPARSE_EX1 */
/*              IMPLICIT NONE */

/*        C */
/*        C     Local constants. */
/*        C */
/*              INTEGER                 NMAX */
/*              PARAMETER             ( NMAX   = 25  ) */

/*              INTEGER                 STRLEN */
/*              PARAMETER             ( STRLEN = 255 ) */

/*        C */
/*        C     Local variables. */
/*        C */
/*              CHARACTER*(1)           DELIM */
/*              CHARACTER*(STRLEN)      ITEMS  ( NMAX ) */
/*              CHARACTER*(STRLEN)      LIST */

/*              INTEGER                 I */
/*              INTEGER                 N */

/*        C */
/*        C     Define the list of delimited items. */
/*        C */
/*        C     Think of a sentence as a list delimited by a space. */
/*        C     DELIM is assigned to a space. */
/*        C */
/*              LIST  = 'Run and find out.' */
/*              DELIM = ' ' */

/*        C */
/*        C     Parse the items from LIST. */
/*        C */
/*              CALL LPARSE ( LIST, DELIM, NMAX, N, ITEMS ) */

/*        C */
/*        C     Output the ITEMS. */
/*        C */
/*              DO I = 1, N */

/*                 WRITE(*,'(A,I3,2A)') 'Item', I, ': ', ITEMS(I) */

/*              END DO */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*        Item  1: Run */
/*        Item  2: and */
/*        Item  3: find */
/*        Item  4: out. */


/*     2) Repeat the previous example with different character */
/*        delimiting the items in the list and different maximum number */
/*        of items to return. */

/*        Example code begins here. */


/*              PROGRAM LPARSE_EX2 */
/*              IMPLICIT NONE */

/*        C */
/*        C     SPICELIB functions. */
/*        C */
/*              INTEGER                 RTRIM */

/*        C */
/*        C     Local constants. */
/*        C */
/*              INTEGER                 NCASES */
/*              PARAMETER             ( NCASES = 2   ) */

/*              INTEGER                 NMAXT */
/*              PARAMETER             ( NMAXT  = 25  ) */

/*              INTEGER                 STRLEN */
/*              PARAMETER             ( STRLEN = 255 ) */

/*        C */
/*        C     Local variables. */
/*        C */
/*              CHARACTER*(1)           DELIM  ( NCASES ) */
/*              CHARACTER*(STRLEN)      ITEMS  ( NMAXT  ) */
/*              CHARACTER*(STRLEN)      LIST   ( NCASES ) */

/*              INTEGER                 I */
/*              INTEGER                 J */
/*              INTEGER                 N */
/*              INTEGER                 NMAX   ( NCASES ) */

/*        C */
/*        C     Define the lists of delimited items, the delimiting */
/*        C     character and the maximum number of items to return. */
/*        C */
/*              LIST(1)  = '//option1//option2/ //' */
/*              DELIM(1) = '/' */
/*              NMAX(1)  = 20 */

/*              LIST(2)  = ' ,bob,   carol,, ted,  alice' */
/*              DELIM(2) = ',' */
/*              NMAX(2)  = 4 */

/*              DO I = 1, NCASES */

/*                 WRITE(*,'(A,I2,A)') 'Case', I, ':' */
/*                 WRITE(*,'(3A)')   '   String: ''', */
/*             .                     LIST(I)(:RTRIM(LIST(I))), '''' */
/*                 WRITE(*,'(3A)')   '   DELIM : ''', DELIM(I), '''' */
/*                 WRITE(*,'(A,I3)') '   NMAX  :', NMAX(I) */
/*                 WRITE(*,'(A)')    '   Output items:' */

/*        C */
/*        C        Parse the items from LIST. */
/*        C */
/*                 CALL LPARSE ( LIST(I), DELIM(I), NMAX(I), N, ITEMS ) */

/*        C */
/*        C        Output the ITEMS. */
/*        C */
/*                 DO J = 1, N */

/*                    WRITE(*,'(A,I3,3A)') '      Item', J, ': ''', */
/*             .                  ITEMS(J)(:RTRIM(ITEMS(J))), '''' */

/*                 END DO */

/*              END DO */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*        Case 1: */
/*           String: '//option1//option2/ //' */
/*           DELIM : '/' */
/*           NMAX  : 20 */
/*           Output items: */
/*              Item  1: ' ' */
/*              Item  2: ' ' */
/*              Item  3: 'option1' */
/*              Item  4: ' ' */
/*              Item  5: 'option2' */
/*              Item  6: ' ' */
/*              Item  7: ' ' */
/*              Item  8: ' ' */
/*        Case 2: */
/*           String: ' ,bob,   carol,, ted,  alice' */
/*           DELIM : ',' */
/*           NMAX  :  4 */
/*           Output items: */
/*              Item  1: ' ' */
/*              Item  2: 'bob' */
/*              Item  3: 'carol' */
/*              Item  4: ' ' */


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

/* -    SPICELIB Version 1.2.0, 06-JUL-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. Removed */
/*        unnecessary entries from $Revisions section. */

/*        Added complete code example. */

/* -    SPICELIB Version 1.1.0, 26-OCT-2005 (NJB) */

/*        Bug fix: code was modified to avoid out-of-range */
/*        substring bound conditions. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU) (HAN) (NJB) */

/* -& */
/* $ Index_Entries */

/*     parse items from a list */

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

/* -& */

/*     Local parameters */


/*     Local variables */


/*     Because speed is essential in many list parsing applications, */
/*     LPARSE parses the input list in a single pass. */


/*     Nothing yet. */

    *n = 0;

/*     Blank list contains a blank item. */

    if (s_cmp(list, " ", list_len, (ftnlen)1) == 0) {
	*n = 1;
	s_copy(items, " ", items_len, (ftnlen)1);
    } else {

/*        Eliminate trailing blanks. EOL is the last non-blank */
/*        character in the list. */

	eol = i_len(list, list_len);
	while(*(unsigned char *)&list[eol - 1] == ' ') {
	    --eol;
	}

/*        As the king said to Alice: 'Begin at the beginning. */
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
	    while(e <= eol && *(unsigned char *)echr != *(unsigned char *)
		    delim) {
		++e;
		if (e <= eol) {
		    *(unsigned char *)echr = *(unsigned char *)&list[e - 1];
		}
	    }

/*           The item now lies between B and E. Unless, of course, B and */
/*           E are the same character; this can happen if the list */
/*           starts or ends with a non-blank delimiter, or if we have */
/*           stumbled upon consecutive delimiters. */

	    ++(*n);
	    if (e > b) {
		s_copy(items + (*n - 1) * items_len, list + (b - 1), 
			items_len, e - 1 - (b - 1));
	    } else {
		s_copy(items + (*n - 1) * items_len, " ", items_len, (ftnlen)
			1);
	    }

/*           If there are more items to be found, continue with */
/*           character following E (which is a delimiter). */

	    if (*n < *nmax) {
		b = e + 1;
	    } else {
		return 0;
	    }
	}

/*        If the list ended with a (non-blank) delimiter, add a blank */
/*        item to the end. */

	if (*(unsigned char *)&list[eol - 1] == *(unsigned char *)delim && *n 
		< *nmax) {
	    ++(*n);
	    s_copy(items + (*n - 1) * items_len, " ", items_len, (ftnlen)1);
	}
    }
    return 0;
} /* lparse_ */

