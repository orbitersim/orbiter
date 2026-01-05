/* iso2utc.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;
static integer c__2 = 2;
static integer c__3 = 3;
static integer c__4 = 4;
static integer c__5 = 5;
static integer c__6 = 6;
static integer c__7 = 7;
static integer c__8 = 8;
static integer c__9 = 9;
static integer c__10 = 10;
static integer c__12 = 12;
static integer c__11 = 11;
static integer c__13 = 13;
static integer c__14 = 14;
static integer c__15 = 15;
static integer c__16 = 16;
static integer c__17 = 17;
static integer c__18 = 18;
static integer c__19 = 19;

/* $Procedure ISO2UTC ( Convert ISO time strings to UTC strings. ) */
/* Subroutine */ int iso2utc_(char *tstrng, char *utcstr, char *error, ftnlen 
	tstrng_len, ftnlen utcstr_len, ftnlen error_len)
{
    /* Initialized data */

    static char months[3*13] = "???" "JAN" "FEB" "MAR" "APR" "MAY" "JUN" 
	    "JUL" "AUG" "SEP" "OCT" "NOV" "DEC";
    static char imonth[2*12] = "01" "02" "03" "04" "05" "06" "07" "08" "09" 
	    "10" "11" "12";

    /* System generated locals */
    address a__1[3], a__2[5];
    integer i__1[3], i__2, i__3[5];

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen), s_cat(char *,
	     char **, integer *, integer *, ftnlen);
    integer s_rnge(char *, integer, char *, integer);
    logical l_lt(char *, char *, ftnlen, ftnlen), l_gt(char *, char *, ftnlen,
	     ftnlen);

    /* Local variables */
    integer l, m;
    char ascii[100];
    extern /* Subroutine */ int chkin_(char *, ftnlen), repmc_(char *, char *,
	     char *, char *, ftnlen, ftnlen, ftnlen, ftnlen);
    extern integer rtrim_(char *, ftnlen);
    extern /* Subroutine */ int ljust_(char *, char *, ftnlen, ftnlen);
    char mystr[128];
    logical change;
    extern integer bsrchc_(char *, integer *, char *, ftnlen, ftnlen);
    extern /* Subroutine */ int chkout_(char *, ftnlen);
    extern logical return_(void);

/* $ Abstract */

/*     Deprecated: This routine is deprecated because all high-level */
/*     time conversion routines (STR2ET, UTC2ET, TPARSE) were updated */
/*     to accept ISO formatted times on input.  This routine is */
/*     supported for purposes of backward compatibility only. */

/*     Convert date-time strings represented in the format adopted by the */
/*     International Standards Organization (ISO) to equivalent UTC time */
/*     strings recognized by the SPICELIB routine TPARSE. */

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

/*     TIME */

/* $ Keywords */

/*     TIME */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     TSTRNG     I   String representing a calendar or julian date epoch */
/*     UTCSTR     O   SPICELIB UTC string corresponding to TSTRNG */
/*     ERROR      O   Error message if something went wrong. */

/* $ Detailed_Input */

/*     TSTRNG   is an input time string, containing a time string */
/*              in ISO format. This routine is not sensitive to */
/*              the case of the characters that make up TSTRNG. */
/*              Thus 1992-192t12:29:28 and 1992-192T12:29:28 */
/*              are equivalent. */

/*              The ISO standard time formats are: */

/*                 Year Month Day    yyyy-mm-ddThh:mm:ss[.sss...] */
/*                                   yyyy-mm-dd */

/*                 Day of Year       yyyy-dddThh:mm:ss[.sss...] */
/*                                   yyyy-ddd */

/*              The letters y,m,d,h,m,s can stand for any digit. */
/*              All digits are required in these formats. Moreover */
/*              the year portion of these strings must be between */
/*              1000 and 2999 inclusive. */

/*              The length of TSTRNG should not exceed 80 characters. */

/*              We point out that the format yyyy-ddd may be */
/*              interpreted very differently by routine UTC2ET. */
/*              1992-003 is interpreted by UTC2ET as March 1, 1992 */
/*              whereas it is interpret as January 3, 1992 by ISO2ET. */

/*              User's should be aware of these differences in */
/*              interpretation and exercise adequate care in their */
/*              programs to avoid this possible confusion. */

/* $ Detailed_Output */

/*     UTCSTR   is the equivalent of TSTRNG, expressed in a UTC */
/*              time string that can be parsed by the SPICELIB */
/*              routine TPARSE. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the string is interpreted as an ISO format string and */
/*         the year portion is not within the range [1000, 2999], the */
/*         error SPICE(YEAROUTOFBOUNDS) is signaled. UTCSTR is */
/*         not changed. */

/*     2)  If the string does not clearly match the ISO format, */
/*         the error SPICE(NOTISOFORMAT) is signaled. UTCSTR is not */
/*         changed. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     The input string is converted to a UTC time string as defined */
/*     by the SPICELIB routine TPARSE. */

/* $ Examples */

/*     To convert the time string 1992-04-03T14:12:28 to the */
/*     corresponding ephemeris time, execute the following instructions: */

/*        TSTRNG = '1992-04-03T14:12:28' */

/*        CALL ISO2UTC ( TSTRNG, UTCSTR, ERROR ) */

/*        CALL TPARSE ( UTCSTR, UTCSEC, ERROR ) */

/*        CALL DELTET ( UTCSEC, 'UTC', DELTA ) */

/*        ET = DELTA + UTCSEC */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     [1]  J. Jespersen and J. Fitz-Randolph, "From Sundials to Atomic */
/*          Clocks, Understanding Time and Frequency," Dover */
/*          Publications, Inc. New York, 1877. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     K.R. Gehringer     (JPL) */
/*     J.M. Lynch         (JPL) */
/*     B.V. Semenov       (JPL) */
/*     E.D. Wright        (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.2.0, 23-DEC-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. Remove obsolete */
/*        reference to NAIF document 268 from $Literature_References. */

/* -    SPICELIB Version 1.1.2, 28-FEB-2008 (BVS) */

/*        Corrected the contents of the $Required_Reading section. */

/* -    SPICELIB Version 1.1.1, 19-SEP-2006 (EDW) */

/*        Added text to previously empty $Restrictions section. */

/* -    SPICELIB Version 1.0.0, 11-JUL-1995 (KRG) (JML) */

/*        Based on */

/*        EKLIB Version 1.1.0, 11-JUL-1995 (KRG) */

/*           Fixed a typo in the $Detailed_Output section of the header. */
/*           The output variable was listed as ET when it should have */
/*           been UTCSTR. */

/*           Changed the length of ASCII to be 100 rather than 128. This */
/*           removes possible compiler warning messages for truncating */
/*           character variables on assignments. The maximum nonblank */
/*           length for an input time ISO string is 80 characters, so */
/*           placing it into a temporary array of 100 characters should */
/*           pose no difficulties. */

/*        EKLIB Version 1.0.0, 25-FEB-1993 (JML) */

/* -& */
/* $ Index_Entries */

/*     DEPRECATED Transform ISO time strings to UTC strings */

/* -& */

/*     SPICELIB functions */


/*     Local Parameters */


/*     In-line functions. */


/*     Local Variables */


/*     Initial Values */


/*     In-line Function Definitions */


/*     Standard SPICELIB exception handling */

    if (return_()) {
	return 0;
    } else {
	chkin_("ISO2UTC", (ftnlen)7);
    }

/*     Left justify the input time string, and determine the location of */
/*     it's last non-blank character.  Finally make some local copies. */

    ljust_(tstrng, ascii, tstrng_len, (ftnlen)100);
    l = rtrim_(ascii, (ftnlen)100);
    s_copy(mystr, ascii, (ftnlen)128, (ftnlen)100);
    change = FALSE_;

/*     Next check for one of the ISO allowed formats. */

    if (l == 8) {

/*        The possible format is: yyyy-ddd.  If we get a */
/*        match construct the corresponding SPICE day of */
/*        year format using JAN  (e.g. 1991-JAN-261). */

	if (*(unsigned char *)&ascii[c__1 - 1] >= '0' && *(unsigned char *)&
		ascii[c__1 - 1] <= '9' && (*(unsigned char *)&ascii[c__2 - 1] 
		>= '0' && *(unsigned char *)&ascii[c__2 - 1] <= '9') && (*(
		unsigned char *)&ascii[c__3 - 1] >= '0' && *(unsigned char *)&
		ascii[c__3 - 1] <= '9') && (*(unsigned char *)&ascii[c__4 - 1]
		 >= '0' && *(unsigned char *)&ascii[c__4 - 1] <= '9') && *(
		unsigned char *)&ascii[c__5 - 1] == '-' && (*(unsigned char *)
		&ascii[c__6 - 1] >= '0' && *(unsigned char *)&ascii[c__6 - 1] 
		<= '9') && (*(unsigned char *)&ascii[c__7 - 1] >= '0' && *(
		unsigned char *)&ascii[c__7 - 1] <= '9') && (*(unsigned char *
		)&ascii[c__8 - 1] >= '0' && *(unsigned char *)&ascii[c__8 - 1]
		 <= '9')) {
/* Writing concatenation */
	    i__1[0] = 5, a__1[0] = ascii;
	    i__1[1] = 3, a__1[1] = "JAN";
	    i__1[2] = 96, a__1[2] = ascii + 4;
	    s_cat(mystr, a__1, i__1, &c__3, (ftnlen)128);
	    change = TRUE_;
	}
    } else if (l == 10) {

/*        The possible format is: yyyy-mm-dd. If we get a match */
/*        construct the corresponding SPICE yyyy-mm-dd format. */

	if (*(unsigned char *)&ascii[c__1 - 1] >= '0' && *(unsigned char *)&
		ascii[c__1 - 1] <= '9' && (*(unsigned char *)&ascii[c__2 - 1] 
		>= '0' && *(unsigned char *)&ascii[c__2 - 1] <= '9') && (*(
		unsigned char *)&ascii[c__3 - 1] >= '0' && *(unsigned char *)&
		ascii[c__3 - 1] <= '9') && (*(unsigned char *)&ascii[c__4 - 1]
		 >= '0' && *(unsigned char *)&ascii[c__4 - 1] <= '9') && *(
		unsigned char *)&ascii[c__5 - 1] == '-' && (*(unsigned char *)
		&ascii[c__6 - 1] >= '0' && *(unsigned char *)&ascii[c__6 - 1] 
		<= '9') && (*(unsigned char *)&ascii[c__7 - 1] >= '0' && *(
		unsigned char *)&ascii[c__7 - 1] <= '9') && *(unsigned char *)
		&ascii[c__8 - 1] == '-' && (*(unsigned char *)&ascii[c__9 - 1]
		 >= '0' && *(unsigned char *)&ascii[c__9 - 1] <= '9') && (*(
		unsigned char *)&ascii[c__10 - 1] >= '0' && *(unsigned char *)
		&ascii[c__10 - 1] <= '9')) {
	    m = bsrchc_(ascii + 5, &c__12, imonth, (ftnlen)2, (ftnlen)2);
/* Writing concatenation */
	    i__1[0] = 5, a__1[0] = ascii;
	    i__1[1] = 3, a__1[1] = months + ((i__2 = m) < 13 && 0 <= i__2 ? 
		    i__2 : s_rnge("months", i__2, "iso2utc_", (ftnlen)332)) * 
		    3;
	    i__1[2] = 93, a__1[2] = ascii + 7;
	    s_cat(mystr, a__1, i__1, &c__3, (ftnlen)128);
	    change = TRUE_;
	}
    } else if (l >= 17) {

/*        There are two possible formats yyyy-dddThh:mm:ss.ssssss */
/*                                       yyyy-mm-ddThh:mm:ss.ssssss */
/*        As above, if we get a match up to the first character following */
/*        a 'T', convert this to a standard SPICE time string. */

	if (*(unsigned char *)&ascii[c__1 - 1] >= '0' && *(unsigned char *)&
		ascii[c__1 - 1] <= '9' && (*(unsigned char *)&ascii[c__2 - 1] 
		>= '0' && *(unsigned char *)&ascii[c__2 - 1] <= '9') && (*(
		unsigned char *)&ascii[c__3 - 1] >= '0' && *(unsigned char *)&
		ascii[c__3 - 1] <= '9') && (*(unsigned char *)&ascii[c__4 - 1]
		 >= '0' && *(unsigned char *)&ascii[c__4 - 1] <= '9') && *(
		unsigned char *)&ascii[c__5 - 1] == '-' && (*(unsigned char *)
		&ascii[c__6 - 1] >= '0' && *(unsigned char *)&ascii[c__6 - 1] 
		<= '9') && (*(unsigned char *)&ascii[c__7 - 1] >= '0' && *(
		unsigned char *)&ascii[c__7 - 1] <= '9') && (*(unsigned char *
		)&ascii[c__8 - 1] >= '0' && *(unsigned char *)&ascii[c__8 - 1]
		 <= '9') && (*(unsigned char *)&ascii[c__9 - 1] == 'T' || *(
		unsigned char *)&ascii[c__9 - 1] == 't') && (*(unsigned char *
		)&ascii[c__10 - 1] >= '0' && *(unsigned char *)&ascii[c__10 - 
		1] <= '9') && (*(unsigned char *)&ascii[c__11 - 1] >= '0' && *
		(unsigned char *)&ascii[c__11 - 1] <= '9') && *(unsigned char 
		*)&ascii[c__12 - 1] == ':' && (*(unsigned char *)&ascii[c__13 
		- 1] >= '0' && *(unsigned char *)&ascii[c__13 - 1] <= '9') && 
		(*(unsigned char *)&ascii[c__14 - 1] >= '0' && *(unsigned 
		char *)&ascii[c__14 - 1] <= '9') && *(unsigned char *)&ascii[
		c__15 - 1] == ':' && (*(unsigned char *)&ascii[c__16 - 1] >= 
		'0' && *(unsigned char *)&ascii[c__16 - 1] <= '9') && (*(
		unsigned char *)&ascii[c__17 - 1] >= '0' && *(unsigned char *)
		&ascii[c__17 - 1] <= '9')) {
/* Writing concatenation */
	    i__3[0] = 5, a__2[0] = ascii;
	    i__3[1] = 3, a__2[1] = "JAN";
	    i__3[2] = 4, a__2[2] = ascii + 4;
	    i__3[3] = 1, a__2[3] = " ";
	    i__3[4] = 91, a__2[4] = ascii + 9;
	    s_cat(mystr, a__2, i__3, &c__5, (ftnlen)128);
	    change = TRUE_;
	} else if (*(unsigned char *)&ascii[c__1 - 1] >= '0' && *(unsigned 
		char *)&ascii[c__1 - 1] <= '9' && (*(unsigned char *)&ascii[
		c__2 - 1] >= '0' && *(unsigned char *)&ascii[c__2 - 1] <= '9')
		 && (*(unsigned char *)&ascii[c__3 - 1] >= '0' && *(unsigned 
		char *)&ascii[c__3 - 1] <= '9') && (*(unsigned char *)&ascii[
		c__4 - 1] >= '0' && *(unsigned char *)&ascii[c__4 - 1] <= '9')
		 && *(unsigned char *)&ascii[c__5 - 1] == '-' && (*(unsigned 
		char *)&ascii[c__6 - 1] >= '0' && *(unsigned char *)&ascii[
		c__6 - 1] <= '9') && (*(unsigned char *)&ascii[c__7 - 1] >= 
		'0' && *(unsigned char *)&ascii[c__7 - 1] <= '9') && *(
		unsigned char *)&ascii[c__8 - 1] == '-' && (*(unsigned char *)
		&ascii[c__9 - 1] >= '0' && *(unsigned char *)&ascii[c__9 - 1] 
		<= '9') && (*(unsigned char *)&ascii[c__10 - 1] >= '0' && *(
		unsigned char *)&ascii[c__10 - 1] <= '9') && (*(unsigned char 
		*)&ascii[c__11 - 1] == 'T' || *(unsigned char *)&ascii[c__11 
		- 1] == 't') && (*(unsigned char *)&ascii[c__12 - 1] >= '0' &&
		 *(unsigned char *)&ascii[c__12 - 1] <= '9') && (*(unsigned 
		char *)&ascii[c__13 - 1] >= '0' && *(unsigned char *)&ascii[
		c__13 - 1] <= '9') && *(unsigned char *)&ascii[c__14 - 1] == 
		':' && (*(unsigned char *)&ascii[c__15 - 1] >= '0' && *(
		unsigned char *)&ascii[c__15 - 1] <= '9') && (*(unsigned char 
		*)&ascii[c__16 - 1] >= '0' && *(unsigned char *)&ascii[c__16 
		- 1] <= '9') && *(unsigned char *)&ascii[c__17 - 1] == ':' && 
		(*(unsigned char *)&ascii[c__18 - 1] >= '0' && *(unsigned 
		char *)&ascii[c__18 - 1] <= '9') && (*(unsigned char *)&ascii[
		c__19 - 1] >= '0' && *(unsigned char *)&ascii[c__19 - 1] <= 
		'9')) {
	    m = bsrchc_(ascii + 5, &c__12, imonth, (ftnlen)2, (ftnlen)2);
/* Writing concatenation */
	    i__3[0] = 5, a__2[0] = ascii;
	    i__3[1] = 3, a__2[1] = months + ((i__2 = m) < 13 && 0 <= i__2 ? 
		    i__2 : s_rnge("months", i__2, "iso2utc_", (ftnlen)379)) * 
		    3;
	    i__3[2] = 3, a__2[2] = ascii + 7;
	    i__3[3] = 1, a__2[3] = " ";
	    i__3[4] = 89, a__2[4] = ascii + 11;
	    s_cat(mystr, a__2, i__3, &c__5, (ftnlen)128);
	    change = TRUE_;
	}
    }

/*     If we didn't make some change to the input string, it's NOT */
/*     an ISO format string. Say so in an error message and return. */

    if (! change) {
	s_copy(error, "The input string does not match the format expected o"
		"f ISO time strings. The acceptable formats are: yyyy-ddd, yy"
		"yy-mm-dd, yyyy-dddThh:mm:ss[.ss...], and yyyy-mm-ddThh:mm:ss"
		"[.ss...].  The input string was #. ", error_len, (ftnlen)208);
	repmc_(error, "#", mystr, error, error_len, (ftnlen)1, l, error_len);
	chkout_("ISO2UTC", (ftnlen)7);
	return 0;
    }

/*     Check for a year out of the range from 1000 to 2999 */

    if (change && (l_lt(ascii, "1000", (ftnlen)4, (ftnlen)4) || l_gt(ascii, 
	    "2999", (ftnlen)4, (ftnlen)4))) {
	s_copy(error, "Years outside the range from 1000 to 2999 are not sup"
		"ported in SPICE-ISO format. You've supplied a time string of"
		" the form # ... ", error_len, (ftnlen)129);
	repmc_(error, "#", ascii, error, error_len, (ftnlen)1, (ftnlen)7, 
		error_len);
	chkout_("ISO2UTC", (ftnlen)7);
	return 0;
    }

/*     That's it. */

    s_copy(error, " ", error_len, (ftnlen)1);
    s_copy(utcstr, mystr, utcstr_len, (ftnlen)128);
    chkout_("ISO2UTC", (ftnlen)7);
    return 0;
} /* iso2utc_ */

