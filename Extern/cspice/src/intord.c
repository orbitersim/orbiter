/* intord.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__0 = 0;

/* $Procedure INTORD ( Convert an integer to ordinal text ) */
/* Subroutine */ int intord_(integer *n, char *string, ftnlen string_len)
{
    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_cmp(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    integer last, i__;
    char mystr[148];
    extern integer lastnb_(char *, ftnlen);
    extern /* Subroutine */ int suffix_(char *, integer *, char *, ftnlen, 
	    ftnlen), inttxt_(integer *, char *, ftnlen);

/* $ Abstract */

/*     Convert an integer to an equivalent written ordinal phrase. */
/*     For example, convert 121 to 'ONE HUNDRED TWENTY-FIRST'. */

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

/*     CONVERSION */
/*     PARSING */
/*     STRING */
/*     UNITS */
/*     UTILITY */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     N          I   An integer (less than 10**12 in absolute value). */
/*     STRING     O   An English string representing the ordinal of N. */

/* $ Detailed_Input */

/*     N        is an integer (less than 10**12 in absolute value). */
/*              Moreover, if N is less than zero, -N must be a */
/*              a legitimate number on the host machine. */

/*              In the context of this routine N represents the */
/*              ranking of some item within a group. */

/* $ Detailed_Output */

/*     STRING   is the English ordinal equivalent of N. STRING will */
/*              contain only upper case letters. */

/* $ Parameters */

/*     MAXORD   is one more than the length of the longest ordinal */
/*              string that can be produced by a call to this routine: */
/*              One string of maximum length is: */

/*                 'NEGATIVE '                                  // */
/*                 'SEVEN HUNDRED SEVENTY-SEVEN BILLION '       // */
/*                 'SEVEN HUNDRED SEVENTY-SEVEN MILLION '       // */
/*                 'SEVEN HUNDRED SEVENTY-SEVEN THOUSAND '      // */
/*                 'SEVEN HUNDRED SEVENTY-SEVENTH' */

/*              It has 147 characters. */

/*              The parameter MAXORD is used to declare a local string */
/*              of sufficient length to allow the construction of */
/*              any ordinal string. */

/* $ Exceptions */

/*     Error free. */

/*     1)  If the resulting ordinal is longer than the output string, */
/*         it will be truncated on the right, leaving only the most */
/*         significant portion of the ordinal. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This is used primarily for generating error messages. For example, */
/*     if the third letter or token in a string is in error, it might */
/*     be desirable to supply a message like the following: */

/*        'The third token of 31-JAN-198$ is not a valid year.' */

/* $ Examples */

/*     N           STRING */
/*     ------      ------------------------------------------- */
/*     -6          NEGATIVE SIXTH */
/*      1          FIRST */
/*      2          SECOND */
/*      3          THIRD */
/*      4          FOURTH */
/*      20         TWENTIETH */
/*      21         TWENTY-FIRST */
/*      99         NINETY-NINTH */
/*      82131      EIGHTY-TWO THOUSAND ONE HUNDRED THIRTY-FIRST */

/* $ Restrictions */

/*     1)  Whatever restrictions apply to INTTXT apply to this routine */
/*         as well. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 12-AUG-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 15-AUG-1990 (WLT) */

/* -& */
/* $ Index_Entries */

/*     convert an integer to ordinal text */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     First get the English equivalent of the cardinal N. */

    s_copy(mystr, " ", (ftnlen)148, (ftnlen)1);
    inttxt_(n, mystr, (ftnlen)148);
    last = lastnb_(mystr, (ftnlen)148);
    i__ = last;

/*     Find the beginning of the last number of MYSTR. */

    while(*(unsigned char *)&mystr[i__ - 1] != '-' && *(unsigned char *)&
	    mystr[i__ - 1] != ' ' && i__ > 1) {
	--i__;
    }
    if (*(unsigned char *)&mystr[i__ - 1] == ' ' || *(unsigned char *)&mystr[
	    i__ - 1] == '-') {
	++i__;
    }

/*     Now convert the last cardinal to an ordinal. */

    if (s_cmp(mystr + (i__ - 1), "ONE", last - (i__ - 1), (ftnlen)3) == 0) {
	s_copy(mystr + (i__ - 1), "FIRST", 148 - (i__ - 1), (ftnlen)5);
    } else if (s_cmp(mystr + (i__ - 1), "TWO", last - (i__ - 1), (ftnlen)3) ==
	     0) {
	s_copy(mystr + (i__ - 1), "SECOND", 148 - (i__ - 1), (ftnlen)6);
    } else if (s_cmp(mystr + (i__ - 1), "THREE", last - (i__ - 1), (ftnlen)5) 
	    == 0) {
	s_copy(mystr + (i__ - 1), "THIRD", 148 - (i__ - 1), (ftnlen)5);
    } else if (s_cmp(mystr + (i__ - 1), "FIVE", last - (i__ - 1), (ftnlen)4) 
	    == 0) {
	s_copy(mystr + (i__ - 1), "FIFTH", 148 - (i__ - 1), (ftnlen)5);
    } else if (s_cmp(mystr + (i__ - 1), "EIGHT", last - (i__ - 1), (ftnlen)5) 
	    == 0) {
	s_copy(mystr + (i__ - 1), "EIGHTH", 148 - (i__ - 1), (ftnlen)6);
    } else if (s_cmp(mystr + (i__ - 1), "NINE", last - (i__ - 1), (ftnlen)4) 
	    == 0) {
	s_copy(mystr + (i__ - 1), "NINTH", 148 - (i__ - 1), (ftnlen)5);
    } else if (s_cmp(mystr + (i__ - 1), "TWELVE", last - (i__ - 1), (ftnlen)6)
	     == 0) {
	s_copy(mystr + (i__ - 1), "TWELFTH", 148 - (i__ - 1), (ftnlen)7);
    } else if (*(unsigned char *)&mystr[last - 1] == 'Y') {
	s_copy(mystr + (last - 1), "IETH", 148 - (last - 1), (ftnlen)4);
    } else {
	suffix_("TH", &c__0, mystr, (ftnlen)2, (ftnlen)148);
    }

/*     Now simply put MYSTR into STRING and return. */

    s_copy(string, mystr, string_len, (ftnlen)148);
    return 0;
} /* intord_ */

