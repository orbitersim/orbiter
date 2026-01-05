/* errdp.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__14 = 14;
static integer c__3 = 3;
static integer c__2 = 2;

/* $Procedure ERRDP  ( Insert D.P. Number into Error Message Text ) */
/* Subroutine */ int errdp_(char *marker, doublereal *dpnum, ftnlen 
	marker_len)
{
    /* System generated locals */
    address a__1[3], a__2[2];
    integer i__1, i__2[3], i__3[2];

    /* Builtin functions */
    integer i_indx(char *, char *, ftnlen, ftnlen);
    /* Subroutine */ int s_cat(char *, char **, integer *, integer *, ftnlen),
	     s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    extern /* Subroutine */ int dpstr_(doublereal *, integer *, char *, 
	    ftnlen), ljust_(char *, char *, ftnlen, ftnlen);
    extern logical allowd_(void);
    extern integer lastnb_(char *, ftnlen);
    char lngmsg[1840];
    extern /* Subroutine */ int getlms_(char *, ftnlen);
    extern integer frstnb_(char *, ftnlen);
    char dpstrg[21], tmpmsg[1840];
    extern /* Subroutine */ int putlms_(char *, ftnlen);
    integer strpos;

/* $ Abstract */

/*     Substitute a double precision number for the first occurrence of */
/*     a marker found in the current long error message. */

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
/*     --------  ---  -------------------------------------------------- */
/*     MARKER     I   A substring of the error message to be replaced. */
/*     DPNUM      I   The d.p. number to substitute for MARKER. */

/* $ Detailed_Input */

/*     MARKER   is a character string which marks a position in */
/*              the long error message where a character string */
/*              representing an double precision number is to be */
/*              substituted. Leading and trailing blanks in MARKER */
/*              are not significant. */

/*              Case IS significant;  'XX' is considered to be */
/*              a different marker from 'xx'. */

/*     DPNUM    is an double precision number whose character */
/*              representation will be substituted for the first */
/*              occurrence of MARKER in the long error message. */
/*              This occurrence of the substring indicated by MARKER */
/*              will be removed, and replaced by a character string, */
/*              with no leading or trailing blanks, representing */
/*              DPNUM. */

/* $ Detailed_Output */

/*     None. */

/* $ Parameters */

/*     LMSGLN   is the maximum length of the long error message. See */
/*              the include file errhnd.inc for the value of LMSGLN. */

/* $ Exceptions */

/*     1)  This routine does not detect any errors. */

/*         However, this routine is part of the SPICELIB error */
/*         handling mechanism. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     The effect of this routine is to update the current long */
/*     error message. If no marker is found, (e.g., in the */
/*     case that the long error message is blank), the routine */
/*     has no effect. If multiple instances of the marker */
/*     designated by MARKER are found, only the first one is */
/*     replaced. */

/*     If the character string resulting from the substitution */
/*     exceeds the maximum length of the long error message, the */
/*     characters on the right are lost. No error is signaled. */

/*     This routine has no effect if changes to the long message */
/*     are not allowed. */

/* $ Examples */

/*     The numerical results shown for this example may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */

/*     1) Create a user-defined error message, including both the */
/*        short and long messages, providing the value of two double */
/*        precision variables within the long message, and signal the */
/*        error. */


/*        Example code begins here. */


/*              PROGRAM ERRDP_EX1 */
/*              IMPLICIT NONE */

/*        C */
/*        C     Set long error message, with two different MARKER */
/*        C     strings where the value of the double precision */
/*        C     variables will go.  Our markers are '#' and 'XX'. */
/*        C */
/*              CALL SETMSG ( 'LONG MESSAGE.  Invalid operation value. ' */
/*             .         //   '  The value was #.  Left endpoint ' */
/*             .         //   'exceeded right endpoint.  The left ' */
/*             .         //   'endpoint was:  XX.  The right endpoint ' */
/*             .         //   'was:  XX.' ) */

/*        C */
/*        C     Insert a double precision where the # is now. */
/*        C */
/*              CALL ERRDP ( '#',  5.D0  ) */

/*        C */
/*        C     Insert another double precision number in the long */
/*        C     message where the first XX is now. */
/*        C */
/*              CALL ERRDP ( 'XX', 73.4567D0 ) */

/*        C */
/*        C     Signal the error. */
/*        C */
/*              CALL SIGERR ( 'SPICE(USERDEFINED)' ) */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*        ============================================================*** */

/*        Toolkit version: N0066 */

/*        SPICE(USERDEFINED) -- */

/*        LONG MESSAGE. Invalid operation value. The value was */
/*        5.0000000000000E+00. Left endpoint exceeded right endpoint. *** */
/*        endpoint was: 7.3456700000000E+01. The right endpoint was: XX. */

/*        Oh, by the way:  The SPICELIB error handling actions are USER- */
/*        TAILORABLE.  You can choose whether the Toolkit aborts or co*** */
/*        when errors occur, which error messages to output, and where*** */
/*        the output.  Please read the ERROR "Required Reading" file, *** */
/*        the routines ERRACT, ERRDEV, and ERRPRT. */

/*        ============================================================*** */


/*        Warning: incomplete output. 6 lines extended past the right */
/*        margin of the header and have been truncated. These lines are */
/*        marked by "***" at the end of each line. */


/*        Note that the execution of this program produces the error */
/*        SPICE(USERDEFINED), which follows the NAIF standard as */
/*        described in the ERROR required reading. */

/* $ Restrictions */

/*     1)  The caller must ensure that the message length, after sub- */
/*         stitution is performed, doesn't exceed LMSGLN characters. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     B.V. Semenov       (JPL) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 2.3.0, 13-AUG-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. Removed */
/*        unnecessary $Revisions section. Added complete code example */
/*        based on existing fragments. */

/* -    SPICELIB Version 2.2.1, 08-JAN-2014 (BVS) */

/*        Fixed header example (5.0 -> 5.D0). */

/* -    SPICELIB Version 2.2.0, 29-JUL-2005 (NJB) */

/*        Bug fix: increased length of internal string DPSTRG to */
/*        handle 3-digit exponents. */

/* -    SPICELIB Version 2.1.0, 29-JUL-1997 (NJB) */

/*        Bug fix: extraneous leading blank has been removed from */
/*        numeric string substituted for marker. */

/*        Maximum length of the long error message is now represented */
/*        by the parameter LMSGLN. Miscellaneous format changes to the */
/*        header, code and in-line comments were made. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (NJB) */

/* -& */
/* $ Index_Entries */

/*     insert d.p. number into error message text */

/* -& */

/*     SPICELIB functions */


/*     Local Variables: */


/*     Length of DPSTRG is number of significant digits plus 7 */
/*     (see DPSTR header) */


/*     Executable Code: */


/*     Changes to the long error message have to be allowed, or we */
/*     do nothing. */

    if (! allowd_()) {
	return 0;
    }

/*     MARKER has to have some non-blank characters, or we do nothing. */

    if (lastnb_(marker, marker_len) == 0) {
	return 0;
    }

/*     Get a copy of the current long error message.  Convert DPNUM */
/*     to a character string.  Ask for 14 significant digits in */
/*     string. */

    getlms_(lngmsg, (ftnlen)1840);
    dpstr_(dpnum, &c__14, dpstrg, (ftnlen)21);
    ljust_(dpstrg, dpstrg, (ftnlen)21, (ftnlen)21);

/*     Locate the leftmost occurrence of MARKER, if there is one */
/*     (ignoring leading and trailing blanks): */

    i__1 = frstnb_(marker, marker_len) - 1;
    strpos = i_indx(lngmsg, marker + i__1, (ftnlen)1840, lastnb_(marker, 
	    marker_len) - i__1);
    if (strpos == 0) {
	return 0;
    } else {

/*        We put together TMPMSG, a copy of LNGMSG with MARKER */
/*        replaced by the character representation of DPNUM: */

	if (strpos > 1) {
	    if (strpos + lastnb_(marker, marker_len) - frstnb_(marker, 
		    marker_len) < lastnb_(lngmsg, (ftnlen)1840)) {

/*              There's more of the long message after the marker... */

		i__1 = strpos + lastnb_(marker, marker_len) - frstnb_(marker, 
			marker_len);
/* Writing concatenation */
		i__2[0] = strpos - 1, a__1[0] = lngmsg;
		i__2[1] = lastnb_(dpstrg, (ftnlen)21), a__1[1] = dpstrg;
		i__2[2] = 1840 - i__1, a__1[2] = lngmsg + i__1;
		s_cat(tmpmsg, a__1, i__2, &c__3, (ftnlen)1840);
	    } else {
/* Writing concatenation */
		i__3[0] = strpos - 1, a__2[0] = lngmsg;
		i__3[1] = lastnb_(dpstrg, (ftnlen)21), a__2[1] = dpstrg;
		s_cat(tmpmsg, a__2, i__3, &c__2, (ftnlen)1840);
	    }
	} else {

/*           We're starting with the d.p. number, so we know it fits... */

	    if (lastnb_(marker, marker_len) - frstnb_(marker, marker_len) < 
		    lastnb_(lngmsg, (ftnlen)1840)) {

/*              There's more of the long message after the marker... */

		i__1 = strpos + lastnb_(marker, marker_len) - frstnb_(marker, 
			marker_len);
/* Writing concatenation */
		i__3[0] = lastnb_(dpstrg, (ftnlen)21), a__2[0] = dpstrg;
		i__3[1] = 1840 - i__1, a__2[1] = lngmsg + i__1;
		s_cat(tmpmsg, a__2, i__3, &c__2, (ftnlen)1840);
	    } else {

/*              The marker's the whole string: */

		s_copy(tmpmsg, dpstrg, (ftnlen)1840, (ftnlen)21);
	    }
	}

/*        Update the long message: */

	putlms_(tmpmsg, (ftnlen)1840);
    }
    return 0;
} /* errdp_ */

