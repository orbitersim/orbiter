/* errch.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__3 = 3;
static integer c__2 = 2;

/* $Procedure ERRCH  ( Insert String into Error Message Text ) */
/* Subroutine */ int errch_(char *marker, char *string, ftnlen marker_len, 
	ftnlen string_len)
{
    /* System generated locals */
    address a__1[3], a__2[2];
    integer i__1, i__2, i__3[3], i__4[2];

    /* Builtin functions */
    integer i_indx(char *, char *, ftnlen, ftnlen), s_cmp(char *, char *, 
	    ftnlen, ftnlen);
    /* Subroutine */ int s_cat(char *, char **, integer *, integer *, ftnlen),
	     s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    extern integer nblen_(char *, ftnlen);
    extern logical allowd_(void);
    extern integer lastnb_(char *, ftnlen);
    char lngmsg[1840];
    extern /* Subroutine */ int getlms_(char *, ftnlen);
    extern integer frstnb_(char *, ftnlen);
    char tmpmsg[1840];
    integer mrkpos;
    extern /* Subroutine */ int putlms_(char *, ftnlen);

/* $ Abstract */

/*     Substitute a character string for the first occurrence of */
/*     a marker in the current long error message. */

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

/*     ERROR */

/* $ Keywords */

/*     CONVERSION */
/*     ERROR */

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


/*     Include File:  SPICELIB Error Handling Parameters */

/*        errhnd.inc  Version 2    18-JUN-1997 (WLT) */

/*           The size of the long error message was */
/*           reduced from 25*80 to 23*80 so that it */
/*           will be accepted by the Microsoft Power Station */
/*           FORTRAN compiler which has an upper bound */
/*           of 1900 for the length of a character string. */

/*        errhnd.inc  Version 1    29-JUL-1997 (NJB) */



/*     Maximum length of the long error message: */


/*     Maximum length of the short error message: */


/*     End Include File:  SPICELIB Error Handling Parameters */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  --------------------------------------------------- */
/*     MARKER     I   A substring of the error message to be replaced. */
/*     STRING     I   The character string to substitute for MARKER. */

/* $ Detailed_Input */

/*     MARKER   is a character string that marks a position in */
/*              the long error message where a character string */
/*              is to be substituted. Leading and trailing blanks */
/*              in MARKER are not significant. */

/*              Case IS significant:  'XX' is considered to be */
/*              a different marker from 'xx'. */

/*     STRING   is a character string that will be substituted for */
/*              the first occurrence of MARKER in the long error */
/*              message. This occurrence of the substring indicated */
/*              by MARKER will be removed and replaced by STRING. */
/*              Leading and trailing blanks in STRING are not */
/*              significant. However, if STRING is completely blank, */
/*              a single blank character will be substituted for */
/*              the marker. */

/* $ Detailed_Output */

/*     None. */

/* $ Parameters */

/*     LMSGLN   is the maximum length of the long error message. See */
/*              the include file errhnd.inc for the value of LMSGLN. */

/* $ Exceptions */

/*     1)  If the character string resulting from the substitution */
/*         exceeds the maximum length of the long error message, the */
/*         long error message is truncated on the right. No error is */
/*         signaled. */

/*     2)  If MARKER is blank, no substitution is performed. No error */
/*         is signaled. */

/*     3)  If STRING is blank, then the first occurrence of MARKER is */
/*         replaced by a single blank. */

/*     4)  If MARKER does not appear in the long error message, no */
/*         substitution is performed. No error is signaled. */

/*     5)  If changes to the long error message are disabled, this */
/*         routine has no effect. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     The purpose of this routine is to allow you to tailor the long */
/*     error message to include specific information that is available */
/*     only at run time. This capability is somewhat like being able to */
/*     put variables in your error messages. */

/* $ Examples */

/*     1)   In this example, the marker is  '#'.  We'll signal a file */
/*          open error, and we'll include in the error message the name */
/*          of the file we tried to open.  There are three steps: */

/*             -- Set the long message, using a marker for the location */
/*                where a value is to be substituted. */

/*             -- Substitute the file name into the error message. */

/*             -- Signal the error (causing output of error messages) */
/*                using the SPICELIB routine SIGERR. */

/*             C */
/*             C     Error on file open attempt.  Signal an error. */
/*             C     The character string variable FILE contains the */
/*             C     file name. */
/*             C */
/*             C     After the call to ERRCH, the long error message */
/*             C     will contain the file name held in the string */
/*             C     FILE.  For example, if FILE contains the name */
/*             C     'MYFILE.DAT', the long error message will be */
/*             C */
/*             C         'File open error.  File is MYFILE.DAT.' */
/*             C */

/*                   CALL SETMSG ( 'File open error.  File is #.' ) */
/*                   CALL ERRCH  ( '#',  FILE                     ) */
/*                   CALL SIGERR ( SPICE(FILEOPENFAILED)        ) */


/*     2)   Same example as (1), except this time we'll use a better- */
/*          looking and more descriptive marker than '#'.  Instead, */
/*          we'll use the marker 'FILENAME'. This does not affect the */
/*          long error message; it just makes the code more readable. */

/*             C */
/*             C     Error on file open attempt. Signal an error. */
/*             C     The character string variable FILE contains the */
/*             C     file name. */
/*             C */
/*                   CALL SETMSG ( 'File open error. File is FILENAME.') */
/*                   CALL ERRCH  ( 'FILENAME',  FILE                   ) */
/*                   CALL SIGERR ( SPICE(FILEOPENFAILED)             ) */


/*     3)   Same example as (2), except this time there's a problem with */
/*          the variable FILE: it's blank. This time, the code fragment */

/*             C */
/*             C     Error on file open attempt. Signal an error. */
/*             C     The character string variable FILE contains the */
/*             C     file name. */
/*             C */
/*                   CALL SETMSG ( 'File open error. File is FILENAME.') */
/*                   CALL ERRCH  ( 'FILENAME',  FILE                   ) */

/*          sets the long error message to */

/*             'File open error. File is  '. */

/* $ Restrictions */

/*     1)  The caller must ensure that the message length, after sub- */
/*         stitution is performed, doesn't exceed LMSGLN characters. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     J.E. McLean        (JPL) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 2.2.0, 13-AUG-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 2.1.0, 29-JUL-1997 (NJB) */

/*        Maximum length of the long error message is now represented */
/*        by the parameter LMSGLN. */

/* -    SPICELIB Version 2.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 2.0.0, 25-MAR-1991 (JEM) (NJB) */

/*        When the input value of STRING is blank, this version */
/*        replaces the first occurrence of MARKER with a */
/*        single blank character. Header was edited to improve */
/*        clarity. Cosmetic changes to code were made. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (NJB) */

/* -& */
/* $ Index_Entries */

/*     insert string into error message text */

/* -& */
/* $ Revisions */

/* -    SPICELIB Version 2.1.0, 29-JUL-1997 (NJB) */

/*        Maximum length of the long error message is now represented */
/*        by the parameter LMSGLN. */

/* -    SPICELIB Version 2.0.0, 25-MAR-1991 (JEM) (NJB) */

/*        When the input value of STRING is blank, this version */
/*        replaces the first occurrence of MARKER with a */
/*        single blank character. The previous version made */
/*        no substitution, leaving the marker in the long error */
/*        message. */

/*        The $Exceptions, $Examples, and $Particulars sections were */
/*        updated to improve accuracy and clarity. Some cosmetic */
/*        changes were made as well. */

/*        Also, some cosmetic changes to the code were made. */

/* -& */

/*     SPICELIB functions */


/*     Local Variables: */


/*     Changes to the long error message must be allowed, or we do */
/*     nothing. */

    if (! allowd_()) {
	return 0;
    }

/*     MARKER must have some non-blank characters, or we do nothing. */

    if (lastnb_(marker, marker_len) == 0) {
	return 0;
    }

/*     Get a copy of the current long error message. */

    getlms_(lngmsg, (ftnlen)1840);

/*     Locate the leftmost occurrence of MARKER, if there is one */
/*     (ignoring leading and trailing blanks): */

    i__1 = frstnb_(marker, marker_len) - 1;
    mrkpos = i_indx(lngmsg, marker + i__1, (ftnlen)1840, lastnb_(marker, 
	    marker_len) - i__1);
    if (mrkpos == 0) {

/*        MARKER does not occur in the long error message, so there's */
/*        no substitution to perform. */

	return 0;
    } else {

/*        We put together TMPMSG, a copy of LNGMSG with MARKER */
/*        replaced by STRING. */

	if (mrkpos > 1) {

/*           MARKER is not at the beginning of the long error message. */

	    if (mrkpos + nblen_(marker, marker_len) <= lastnb_(lngmsg, (
		    ftnlen)1840)) {

/*              There's more of the long message after the marker. */

		if (s_cmp(string, " ", string_len, (ftnlen)1) != 0) {
		    i__1 = frstnb_(string, string_len) - 1;
		    i__2 = mrkpos + nblen_(marker, marker_len) - 1;
/* Writing concatenation */
		    i__3[0] = mrkpos - 1, a__1[0] = lngmsg;
		    i__3[1] = lastnb_(string, string_len) - i__1, a__1[1] = 
			    string + i__1;
		    i__3[2] = 1840 - i__2, a__1[2] = lngmsg + i__2;
		    s_cat(tmpmsg, a__1, i__3, &c__3, (ftnlen)1840);
		} else {
		    i__1 = mrkpos + nblen_(marker, marker_len) - 1;
/* Writing concatenation */
		    i__3[0] = mrkpos - 1, a__1[0] = lngmsg;
		    i__3[1] = 1, a__1[1] = " ";
		    i__3[2] = 1840 - i__1, a__1[2] = lngmsg + i__1;
		    s_cat(tmpmsg, a__1, i__3, &c__3, (ftnlen)1840);
		}
	    } else {

/*              The long error message ends with MARKER. */

		if (s_cmp(string, " ", string_len, (ftnlen)1) != 0) {
		    i__1 = frstnb_(string, string_len) - 1;
/* Writing concatenation */
		    i__4[0] = mrkpos - 1, a__2[0] = lngmsg;
		    i__4[1] = lastnb_(string, string_len) - i__1, a__2[1] = 
			    string + i__1;
		    s_cat(tmpmsg, a__2, i__4, &c__2, (ftnlen)1840);
		} else {
/* Writing concatenation */
		    i__4[0] = mrkpos - 1, a__2[0] = lngmsg;
		    i__4[1] = 1, a__2[1] = " ";
		    s_cat(tmpmsg, a__2, i__4, &c__2, (ftnlen)1840);
		}
	    }
	} else {

/*           The long error message starts with MARKER (MRKPOS is 1). */

	    if (nblen_(marker, marker_len) < lastnb_(lngmsg, (ftnlen)1840)) {

/*              There's more of the long message after the marker... */

		if (s_cmp(string, " ", string_len, (ftnlen)1) != 0) {
		    i__1 = frstnb_(string, string_len) - 1;
		    i__2 = nblen_(marker, marker_len);
/* Writing concatenation */
		    i__4[0] = lastnb_(string, string_len) - i__1, a__2[0] = 
			    string + i__1;
		    i__4[1] = 1840 - i__2, a__2[1] = lngmsg + i__2;
		    s_cat(tmpmsg, a__2, i__4, &c__2, (ftnlen)1840);
		} else {
		    i__1 = nblen_(marker, marker_len);
/* Writing concatenation */
		    i__4[0] = 1, a__2[0] = " ";
		    i__4[1] = 1840 - i__1, a__2[1] = lngmsg + i__1;
		    s_cat(tmpmsg, a__2, i__4, &c__2, (ftnlen)1840);
		}
	    } else {

/*              The marker's the whole string: */

		if (s_cmp(string, " ", string_len, (ftnlen)1) != 0) {
		    i__1 = frstnb_(string, string_len) - 1;
		    s_copy(tmpmsg, string + i__1, (ftnlen)1840, lastnb_(
			    string, string_len) - i__1);
		} else {
		    s_copy(tmpmsg, " ", (ftnlen)1840, (ftnlen)1);
		}
	    }
	}

/*        Update the long error message: */

	putlms_(tmpmsg, (ftnlen)1840);
    }
    return 0;
} /* errch_ */

