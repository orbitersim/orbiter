/* nextwd.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure NEXTWD ( Next word in a character string ) */
/* Subroutine */ int nextwd_(char *string, char *next, char *rest, ftnlen 
	string_len, ftnlen next_len, ftnlen rest_len)
{
    /* System generated locals */
    integer i__1;

    /* Builtin functions */
    integer s_cmp(char *, char *, ftnlen, ftnlen);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer i_len(char *, ftnlen);

    /* Local variables */
    integer i__, begin;
    extern /* Subroutine */ int ljust_(char *, char *, ftnlen, ftnlen);
    logical inword;
    integer end;

/* $ Abstract */

/*     Return the next word in a given character string, and */
/*     left justify the rest of the string. */

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
/*     WORD */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     STRING     I   Input character string. */
/*     NEXT       O   The next word in the string. */
/*     REST       O   The remaining part of STRING, left-justified. */

/* $ Detailed_Input */

/*     STRING   is the input string to be parsed. Each word of this */
/*              string is a maximal sequence of consecutive non-blank */
/*              characters. */

/* $ Detailed_Output */

/*     NEXT     is the first word in STRING. It is called the "next" word */
/*              because NEXTWD is typically called repeatedly to find the */
/*              words of the input string in left-to-right order. A word */
/*              is a maximal sequence of consecutive non-blank */
/*              characters. NEXT is always returned left-justified. */

/*              If STRING is blank, NEXT is blank. */

/*              NEXT may NOT overwrite STRING. */

/*     REST     is the remaining part of STRING, left-justified after the */
/*              removal of NEXT. */

/*              REST may overwrite STRING. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/*     1)  If the declared lengths of NEXT and REST are not large enough */
/*         to hold the output strings, they are truncated on the right. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     NEXTWD is used primarily for parsing input commands consisting */
/*     of one or more words, where a word is defined to be any sequence */
/*     of consecutive non-blank characters. Successive calls to NEXTWD, */
/*     each using the previous value of REST as the input string, allow */
/*     the calling routine to neatly parse and process one word at a */
/*     time. */

/*     NEXTWD cuts the input string into two pieces, and returns them */
/*     separately. The first piece is the first word in the string. */
/*     (Leading blanks are ignored. The first word, which is returned in */
/*     the output argument NEXT, runs from the first non-blank character */
/*     in the string up to the first blank that follows it.) The second */
/*     piece is whatever is left after the first word is removed. The */
/*     second piece is left justified, to simplify later calls to NEXTWD. */

/* $ Examples */

/*     The numerical results shown for this example may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */

/*     1) Given a character string, get the sequence of words within. */


/*        Example code begins here. */


/*              PROGRAM NEXTWD_EX1 */
/*              IMPLICIT NONE */

/*        C */
/*        C     SPICELIB functions. */
/*        C */
/*              LOGICAL               EQSTR */

/*        C */
/*        C     Local parameters. */
/*        C */
/*              INTEGER               LINZS */
/*              PARAMETER           ( LINZS = 47 ) */

/*              INTEGER               WRDSZ */
/*              PARAMETER           ( WRDSZ = 5  ) */

/*        C */
/*        C     Local variables. */
/*        C */
/*              CHARACTER*(WRDSZ)     NEXT */
/*              CHARACTER*(LINZS)     REST */
/*              CHARACTER*(LINZS)     STRING */

/*              REST = '  Now is the time,  for all good men to come.' */

/*              WRITE(*,'(A)') 'Next   Rest of the string' */
/*              WRITE(*,'(A)') '-----  ---' */
/*             .            // '---------------------------------------' */

/*              DO WHILE ( .NOT. EQSTR ( REST, ' ' ) ) */

/*                 STRING = REST */
/*                 CALL NEXTWD ( STRING, NEXT, REST ) */

/*                 WRITE(*,'(A5,2X,A)') NEXT, REST */

/*              END DO */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*        Next   Rest of the string */
/*        -----  ------------------------------------------ */
/*        Now    is the time,  for all good men   to come. */
/*        is     the time,  for all good men   to come. */
/*        the    time,  for all good men   to come. */
/*        time,  for all good men   to come. */
/*        for    all good men   to come. */
/*        all    good men   to come. */
/*        good   men   to come. */
/*        men    to come. */
/*        to     come. */
/*        come. */


/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     K.R. Gehringer     (JPL) */
/*     W.L. Taber         (JPL) */
/*     I.M. Underwood     (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.3.0, 19-MAY-2021 (JDR) (NJB) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. Added complete */
/*        code example based on the existing fragment. */

/*        Updated header documentation. Added entry #1 in $Exceptions */
/*        section. */

/* -    SPICELIB Version 1.2.0, 04-APR-1996 (KRG) */

/*        Fixed a problem that could occur when STRING and REST are */
/*        the same character string. Simplified the algorithm a bit */
/*        while I was at it. */

/*        Single character comparisons now make use of ICHAR to */
/*        perform the comparisons as integers for speed. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU) */

/* -& */
/* $ Index_Entries */

/*     next word in a character_string */

/* -& */

/*     Local Parameters */


/*     Local variables */


/*     The trivial case. */

    if (s_cmp(string, " ", string_len, (ftnlen)1) == 0) {
	s_copy(next, " ", next_len, (ftnlen)1);
	s_copy(rest, " ", rest_len, (ftnlen)1);

/*     The non-trivial case. */

    } else {

/*        Get the length of the string. */

	end = i_len(string, string_len);

/*        Skip leading blanks and set flags indicating that we are */
/*        not in a word and that we do not have a word. */

	begin = 1;
	inword = FALSE_;

/*        We know the string is not blank, so we will eventually */
/*        get to a word, thus no need to check against END here. */

	while(! inword) {
	    if (*(unsigned char *)&string[begin - 1] == 32) {
		++begin;
	    } else {
		inword = TRUE_;
	    }
	}

/*        We are now in a word. Step through the input string until the */
/*        next blank is encountered or until the end of the string is */
/*        found. We start at BEGIN even though we know from above that */
/*        STRING(BEGIN:BEGIN) is not blank; this allows us to deal */
/*        cleanly with the case where the string is a single character */
/*        long and not blank (because we're in that case). */

	i__ = begin;
	while(inword) {
	    if (*(unsigned char *)&string[i__ - 1] != 32) {
		++i__;
		if (i__ > end) {
		    --i__;
		    inword = FALSE_;
		}
	    } else {
		--i__;
		inword = FALSE_;
	    }
	}

/*        Our word is the substring between BEGIN and I. Note that I */
/*        might be equal to END, so we have to be careful about setting */
/*        the REST. We also left justify REST as we set it. LJUST does */
/*        the right thing if STRING and REST overlap. If we do not have */
/*        a word, the NEXT and REST are both blank. */

	s_copy(next, string + (begin - 1), next_len, i__ - (begin - 1));
	if (i__ < end) {
	    i__1 = i__;
	    ljust_(string + i__1, rest, string_len - i__1, rest_len);
	} else {
	    s_copy(rest, " ", rest_len, (ftnlen)1);
	}
    }
    return 0;
} /* nextwd_ */

