/* zzektcnv.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;
static integer c__0 = 0;
static integer c__3 = 3;

/* $Procedure  ZZEKTCNV ( Private: EK, time conversion ) */
/* Subroutine */ int zzektcnv_(char *timstr, doublereal *et, logical *error, 
	char *errmsg, ftnlen timstr_len, ftnlen errmsg_len)
{
    /* System generated locals */
    integer i__1;

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    doublereal tvec[10];
    logical mods;
    char type__[32];
    extern integer posr_(char *, char *, integer *, ftnlen, ftnlen);
    extern /* Subroutine */ int sct2e_(integer *, doublereal *, doublereal *);
    integer clkid;
    extern /* Subroutine */ int chkin_(char *, ftnlen), ucase_(char *, char *,
	     ftnlen, ftnlen), repmc_(char *, char *, char *, char *, ftnlen, 
	    ftnlen, ftnlen, ftnlen);
    integer ntvec;
    extern integer rtrim_(char *, ftnlen);
    extern /* Subroutine */ int ljust_(char *, char *, ftnlen, ftnlen), 
	    scn2id_(char *, integer *, logical *, ftnlen), str2et_(char *, 
	    doublereal *, ftnlen);
    extern logical failed_(void);
    doublereal sclkdp;
    char modify[32*10], sclmsg[160];
    logical succes, yabbrv;
    extern /* Subroutine */ int scpars_(integer *, char *, logical *, char *, 
	    doublereal *, ftnlen, ftnlen), chkout_(char *, ftnlen), suffix_(
	    char *, integer *, char *, ftnlen, ftnlen);
    char locstr[80], pictur[80];
    extern /* Subroutine */ int cmprss_(char *, integer *, char *, char *, 
	    ftnlen, ftnlen, ftnlen);
    extern logical return_(void);
    extern /* Subroutine */ int tpartv_(char *, doublereal *, integer *, char 
	    *, char *, logical *, logical *, logical *, char *, char *, 
	    ftnlen, ftnlen, ftnlen, ftnlen, ftnlen);
    logical fnd;
    integer loc;

/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines.  Users should not call this routine directly due */
/*     to the volatile nature of this routine. */

/*     Convert time strings from EK query to ephemeris time. */

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


/*     Include Section:  EK Data Types */

/*        ektype.inc Version 1  27-DEC-1994 (NJB) */


/*     Within the EK system, data types of EK column contents are */
/*     represented by integer codes.  The codes and their meanings */
/*     are listed below. */

/*     Integer codes are also used within the DAS system to indicate */
/*     data types; the EK system makes no assumptions about compatibility */
/*     between the codes used here and those used in the DAS system. */


/*     Character type: */


/*     Double precision type: */


/*     Integer type: */


/*     `Time' type: */

/*     Within the EK system, time values are represented as ephemeris */
/*     seconds past J2000 (TDB), and double precision numbers are used */
/*     to store these values.  However, since time values require special */
/*     treatment both on input and output, and since the `TIME' column */
/*     has a special role in the EK specification and code, time values */
/*     are identified as a type distinct from double precision numbers. */


/*     End Include Section:  EK Data Types */

/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     TIMSTR     I   Time string. */
/*     ET         O   Ephemeris time in seconds past J2000, TDB. */
/*     ERROR      O   Error flag. */
/*     ERRMSG     O   Error message. */

/* $ Detailed_Input */

/*     TIMSTR         is a string representing a time value.  The value */
/*                    make be an SCLK string in the form */

/*                       <clock name> SCLK <clock string> */

/*                    or may be any string acceptable to ST2ET. */

/* $ Detailed_Output */

/*     ET             is the ephemeris time equivalent to the input */
/*                    time. */

/*     ERROR          is a logical flag indicating whether an error was */
/*                    detected.  Note that a time string might be */
/*                    syntactically valid, but incapable of being */
/*                    converted to ET if the appropriate time kernels */
/*                    (Leapseconds or SCLK) are not loaded. */

/*     ERRMSG         is an error message describing an error in the */
/*                    input query, if one was detected. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If any sort of time conversion error occurs, the output flag */
/*         ERROR is set, and an error message is returned. */

/*         The routine attempts to avoid causing errors that must */
/*         be trapped by SPICELIB error handling.  Time string syntax */
/*         errors or missing kernel files, for example, should not trip */
/*         SPICELIB error handling. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     Strings representing time values are interpreted as follows: */

/*        1)  The string is first examined to see whether it's an */
/*            SCLK string for a recognized clock; if it is, the */
/*            string is converted to the equivalent ET. */

/*        2)  If the string is not a SCLK string, it is expected */
/*            to be some sort of standard time representation. */
/*            The string is checked to see whether it's in a format */
/*            that TPARTV can handle.  If TPARTV can't deal with it, */
/*            the string is considered to be invalid. */

/* $ Examples */

/*     See ZZEKTRES. */

/* $ Restrictions */

/*     1) A leapseconds kernel must be loaded at the time this routine */
/*        is called. */

/*     2) In order to convert SCLK strings, an appropriate SCLK kernel */
/*        must be loaded at the time this routine is called. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */

/* $ Version */

/* -    SPICELIB Version 2.0.0, 12-AUG-2001 (NJB) */

/*        Now converts standard time strings to ET via STR2ET instead */
/*        of the less general routines ISO2UTC and UTC2ET. */

/*        Bug fix:  modified algorithm to handle case where string */
/*        "SCLK" appears in SCLK name. */

/*        Bug fix:  construction of error messages in case where */
/*        FAILED() returns .TRUE. has been changed so that REPMC is */
/*        not called.  Instead, the error-free routine SUFFIX is */
/*        used. */

/* -    SPICELIB Version 1.0.0, 11-OCT-1995 (NJB) */

/* -& */
/* $ Revisions */

/* -    SPICELIB Version 2.0.0, 12-AUG-2001 (NJB) */

/*        Now converts standard time strings to ET via STR2ET instead */
/*        of the less general routines ISO2UTC and UTC2ET. */

/*        Bug fix:  modified algorithm to handle case where string */
/*        "SCLK" appears in SCLK name. */

/*        Bug fix:  construction of error messages in case where */
/*        FAILED() returns .TRUE. has been changed so that REPMC is */
/*        not called.  Instead, the error-free routine SUFFIX is */
/*        used. */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     Local variables */

    if (return_()) {
	return 0;
    }
    chkin_("ZZEKTCNV", (ftnlen)8);

/*     No error to start with. */

    *error = FALSE_;
    s_copy(errmsg, " ", errmsg_len, (ftnlen)1);

/*     Get a left-justified, compressed, upper-case copy of */
/*     the string, so we can easily search it for substrings */
/*     that would identify it as SCLK.  If we do find a */
/*     match, remove the identifying substring (of the form */
/*     'MO SCLK', 'VGR1 SCLK', etc.). */

    cmprss_(" ", &c__1, timstr, locstr, (ftnlen)1, timstr_len, (ftnlen)80);
    ljust_(locstr, locstr, (ftnlen)80, (ftnlen)80);
    ucase_(locstr, locstr, (ftnlen)80, (ftnlen)80);
    i__1 = rtrim_(locstr, (ftnlen)80);
    loc = posr_(locstr, "SCLK", &i__1, (ftnlen)80, (ftnlen)4);
    if (loc > 0) {

/*        It's a SCLK string.  Find the ID code, if we can. */

	scn2id_(locstr, &clkid, &fnd, loc + 3);
	if (! fnd) {

/*           We don't recognize this SCLK type. */

	    *error = TRUE_;
	    if (loc > 1) {
		s_copy(errmsg, "Time conversion failed; SCLK type <#> was no"
			"t recognized.", errmsg_len, (ftnlen)57);
		repmc_(errmsg, "#", timstr, errmsg, errmsg_len, (ftnlen)1, 
			loc - 1, errmsg_len);
	    } else {
		s_copy(errmsg, "Time conversion failed; SCLK name was not su"
			"pplied.", errmsg_len, (ftnlen)51);
	    }
	    chkout_("ZZEKTCNV", (ftnlen)8);
	    return 0;
	}

/*        If we got this far, we recognized the SCLK type. */
/*        Convert the time to ET. */

	i__1 = loc + 3;
	scpars_(&clkid, locstr + i__1, error, sclmsg, &sclkdp, 80 - i__1, (
		ftnlen)160);
	if (failed_()) {

/*           We'll arrive here if the required SCLK kernel hasn't */
/*           been loaded. */

	    *error = TRUE_;
	    s_copy(errmsg, "Unexpected SPICELIB error encountered while atte"
		    "mpting to parse the string <", errmsg_len, (ftnlen)76);
	    suffix_(timstr, &c__0, errmsg, timstr_len, errmsg_len);
	    suffix_(">", &c__0, errmsg, (ftnlen)1, errmsg_len);
	    chkout_("ZZEKTCNV", (ftnlen)8);
	    return 0;
	} else if (*error) {
	    s_copy(errmsg, "The string <#> didn't parse as a spacecraft cloc"
		    "k string.", errmsg_len, (ftnlen)57);
	    repmc_(errmsg, "#", timstr, errmsg, errmsg_len, (ftnlen)1, 
		    timstr_len, errmsg_len);
	    suffix_(sclmsg, &c__3, errmsg, (ftnlen)160, errmsg_len);
	    chkout_("ZZEKTCNV", (ftnlen)8);
	    return 0;
	} else {
	    sct2e_(&clkid, &sclkdp, et);
	    if (failed_()) {
		*error = TRUE_;
		s_copy(errmsg, "Unexpected SPICELIB error encountered while "
			"attempting to parse the string <", errmsg_len, (
			ftnlen)76);
		suffix_(timstr, &c__0, errmsg, timstr_len, errmsg_len);
		suffix_(">", &c__0, errmsg, (ftnlen)1, errmsg_len);
		chkout_("ZZEKTCNV", (ftnlen)8);
		return 0;
	    }
	}
    } else {

/*        We could have a standard time string.  Make sure that the */
/*        time string is acceptable before actually calling STR2ET. */

	tpartv_(locstr, tvec, &ntvec, type__, modify, &mods, &yabbrv, &succes,
		 pictur, errmsg, (ftnlen)80, (ftnlen)32, (ftnlen)32, (ftnlen)
		80, errmsg_len);
	if (succes) {

/*           It's safe to pass the string to STR2ET. */

	    str2et_(locstr, et, (ftnlen)80);
	    if (failed_()) {
		*error = TRUE_;
		s_copy(errmsg, "Unexpected SPICELIB error encountered while "
			"attempting to parse the string <", errmsg_len, (
			ftnlen)76);
		suffix_(timstr, &c__0, errmsg, timstr_len, errmsg_len);
		suffix_(">", &c__0, errmsg, (ftnlen)1, errmsg_len);
		chkout_("ZZEKTCNV", (ftnlen)8);
		return 0;
	    }
	} else {

/*           The string cannot be parsed by STR2ET.  The error message */
/*           was set by TPARTV. */

	    *error = TRUE_;
	    chkout_("ZZEKTCNV", (ftnlen)8);
	    return 0;
	}

/*        We're done with the standard time string case. */

    }

/*     We've parsed a time string, if it was an SCLK or standard string. */

    chkout_("ZZEKTCNV", (ftnlen)8);
    return 0;
} /* zzektcnv_ */

