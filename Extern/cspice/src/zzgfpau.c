/* zzgfpau.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__0 = 0;

/* $Procedure ZZGFPAU ( Private --- GF, phase angle utility routine ) */
/* Subroutine */ int zzgfpau_0_(int n__, char *target, char *illmn, char *
	abcorr, char *obsrvr, U_fp udfunc, doublereal *et, logical *decres, 
	doublereal *rvl, integer *xtarg, integer *xillmn, char *xabcor, 
	integer *xobs, logical *xablk, ftnlen target_len, ftnlen illmn_len, 
	ftnlen abcorr_len, ftnlen obsrvr_len, ftnlen xabcor_len)
{
    /* System generated locals */
    integer i__1, i__2;
    doublereal d__1;

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_rnge(char *, integer, char *, integer);

    /* Local variables */
    doublereal dsep, uvec[3];
    extern /* Subroutine */ int vhat_(doublereal *, doublereal *);
    extern doublereal vdot_(doublereal *, doublereal *);
    extern /* Subroutine */ int zzvalcor_(char *, logical *, ftnlen);
    integer i__;
    extern /* Subroutine */ int chkin_(char *, ftnlen), ucase_(char *, char *,
	     ftnlen, ftnlen), errch_(char *, char *, ftnlen, ftnlen);
    logical found;
    extern doublereal dvsep_(doublereal *, doublereal *);
    static integer svobs;
    extern /* Subroutine */ int spkez_(integer *, doublereal *, char *, char *
	    , integer *, doublereal *, doublereal *, ftnlen, ftnlen);
    doublereal s1[6], s2[6];
    extern /* Subroutine */ int bods2c_(char *, integer *, logical *, ftnlen);
    extern logical failed_(void);
    doublereal lt;
    extern doublereal clight_(void);
    static char svabco[5];
    static logical svablk[15];
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), vsclip_(doublereal *, doublereal *), setmsg_(char *, 
	    ftnlen);
    static integer svillm, svtarg;
    extern /* Subroutine */ int cmprss_(char *, integer *, char *, char *, 
	    ftnlen, ftnlen, ftnlen);
    extern logical return_(void);
    doublereal dlt;
    extern /* Subroutine */ int zzgfpaq_(doublereal *, integer *, integer *, 
	    integer *, char *, doublereal *, ftnlen);

/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines.  Users should not call this routine directly due */
/*     to the volatile nature of this routine. */

/*     This is the umbrella routine for the three entry points needed by */
/*     GFEVNT in order to find body centered phase angle events. */

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

/*     EPHEMERIS */
/*     GEOMETRY */
/*     SEARCH */

/* $ Declarations */
/* $ Abstract */

/*     Include file zzabcorr.inc */

/*     SPICE private file intended solely for the support of SPICE */
/*     routines.  Users should not include this file directly due */
/*     to the volatile nature of this file */

/*     The parameters below define the structure of an aberration */
/*     correction attribute block. */

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

/* $ Parameters */

/*     An aberration correction attribute block is an array of logical */
/*     flags indicating the attributes of the aberration correction */
/*     specified by an aberration correction string.  The attributes */
/*     are: */

/*        - Is the correction "geometric"? */

/*        - Is light time correction indicated? */

/*        - Is stellar aberration correction indicated? */

/*        - Is the light time correction of the "converged */
/*          Newtonian" variety? */

/*        - Is the correction for the transmission case? */

/*        - Is the correction relativistic? */

/*    The parameters defining the structure of the block are as */
/*    follows: */

/*       NABCOR    Number of aberration correction choices. */

/*       ABATSZ    Number of elements in the aberration correction */
/*                 block. */

/*       GEOIDX    Index in block of geometric correction flag. */

/*       LTIDX     Index of light time flag. */

/*       STLIDX    Index of stellar aberration flag. */

/*       CNVIDX    Index of converged Newtonian flag. */

/*       XMTIDX    Index of transmission flag. */

/*       RELIDX    Index of relativistic flag. */

/*    The following parameter is not required to define the block */
/*    structure, but it is convenient to include it here: */

/*       CORLEN    The maximum string length required by any aberration */
/*                 correction string */

/* $ Author_and_Institution */

/*     N.J. Bachman    (JPL) */

/* $ Literature_References */

/*     None. */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 18-DEC-2004 (NJB) */

/* -& */
/*     Number of aberration correction choices: */


/*     Aberration correction attribute block size */
/*     (number of aberration correction attributes): */


/*     Indices of attributes within an aberration correction */
/*     attribute block: */


/*     Maximum length of an aberration correction string: */


/*     End of include file zzabcorr.inc */

/* $ Brief_I/O */

/*     VARIABLE  I/O  Entry points */
/*     --------  ---  -------------------------------------------------- */
/*     TARGET    I-O  ZZGFPAIN, ZZGFPAX */
/*     ILLMN     I-O  ZZGFPAIN, ZZGFPAX */
/*     ABCORR    I-O  ZZGFPAIN, ZZGFPAX */
/*     OBSRVR    I-O  ZZGFPAIN, ZZGFPAX */
/*     ET         I   ZZGFPADC, ZZGFPAGQ */
/*     UDFUNC     I   ZZGFPADC */
/*     DECRES     O   ZZGFPADC */
/*     RVL        O   ZZGFPAGQ */
/*     XTARG      O   ZZGFPAX */
/*     XILLMN     O   ZZGFPAX */
/*     XABCOR     O   ZZGFPAX */
/*     XOBS       O   ZZGFPAX */
/*     XABLK      O   ZZGFPAX */

/* $ Detailed_Input */

/*     TARGET   the string name of a target body.  Optionally, you may */
/*              supply the integer ID code for the object as an */
/*              integer string.  For example both 'MOON' and '301' */
/*              are legitimate strings that indicate the moon is the */
/*              target body. */

/*              The target and observer define a position vector */
/*              that points from the observer to the target. */

/*     ILLMN    the string name of the illuminating body. This will */
/*              normally be 'SUN' but the algorithm can use any */
/*              ephemeris object */

/*     ABCORR   the string description of the aberration corrections to */
/*              apply to the state evaluations to account for one-way */
/*              light time and stellar aberration. */

/*              Any signal receive aberration correction accepted by */
/*              the SPICE routine SPKEZR is accepted here. See the header */
/*              of SPKEZR for a detailed description of the */
/*              aberration correction options. For convenience, */
/*              the options are listed below: */

/*                 'NONE'     Apply no correction. Returns the "true" */
/*                            geometric state. */

/*                 'LT'       "Reception" case:  correct for */
/*                            one-way light time using a Newtonian */
/*                            formulation. */

/*                 'LT+S'     "Reception" case:  correct for */
/*                            one-way light time and stellar */
/*                            aberration using a Newtonian */
/*                            formulation. */

/*                 'CN'       "Reception" case:  converged */
/*                            Newtonian light time correction. */

/*                 'CN+S'     "Reception" case:  converged */
/*                            Newtonian light time and stellar */
/*                            aberration corrections. */

/*              The ABCORR string lacks sensitivity to case, leading */
/*              and trailing blanks. */

/*     OBSRVR   the string name of an observing body.  Optionally, you */
/*              may supply the ID code of the object as an integer */
/*              string. For example, both 'EARTH' and '399' are */
/*              legitimate strings to indicate the observer as Earth. */

/*     ET       the time in TDB seconds past J2000 at which to calculate */
/*              the value of or characteristic of the phase angle */
/*              between SVTARG and SVILLMN as seen from SVOBS. */

/*     For more information, see individual entry points. */

/* $ Detailed_Output */

/*     DECRES   is .TRUE. if the phase angle is decreasing at ET. */
/*              Otherwise it is .FALSE.. */

/*     RVL      the phase angle between SVTARG and SVILLMN as seen */
/*              from SVOBS at time ET. */

/*     XTARG    SPICE ID value for the target body initialized */
/*              via ZZGFPAIN. */

/*     XILLMN   SPICE ID value for the illuminator body initialized */
/*              via ZZGFPAIN. */

/*     XABCOR   String value for the aberration correction initialized */
/*              via ZZGFPAIN. */

/*     XOBS     SPICE ID value for the observing body initialized */
/*              via ZZGFPAIN. */

/*     XABLK    Aberration correction attribute block as initialized */
/*              by ABCORR. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1) SPICE(BOGUSENTRY) signals if a direct call to ZZGFPAU occurs. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine serves as the umbrella routine for three entry points */
/*     needed by GFEVNT in solving for phase angle conditions. */

/*     The three entry points are */

/*        ZZGFPAIN --- an initialization routine that must be called */
/*                     prior to attempting to solve for any range */
/*                     rate event. */

/*        ZZGFPADC --- determines whether or not phase angle is */
/*                     decreasing at some time. */

/*        ZZGFPAGQ --- returns the phase angle of the two objects */
/*                     of concern as a function of ET. */

/* $ Examples */

/*     None. */

/* $ Restrictions */

/*     ZZGFPAIN must be called prior to use of any of the other */
/*     entry points (think constructor). */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     E.D. Wright    (JPL) */
/*     N.J. Bachman   (JPL) */

/* $ Version */

/* -    SPICELIB version 1.0.0 16-SEP-2012 (EDW) (NJB) */

/* -& */
/* $ Index_Entries */

/*     find phase angle events */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     The phase angle calculation is invariant with respect to */
/*     reference frame. Use J2000 for convenience. */


/*     Local Variables */


/*     Saved Variables */

    /* Parameter adjustments */
    if (xablk) {
	}

    /* Function Body */
    switch(n__) {
	case 1: goto L_zzgfpain;
	case 2: goto L_zzgfpadc;
	case 3: goto L_zzgfpagq;
	case 4: goto L_zzgfpax;
	}

    chkin_("ZZGFPAU", (ftnlen)7);
    sigerr_("SPICE(BOGUSENTRY)", (ftnlen)17);
    chkout_("ZZGFPAU", (ftnlen)7);
    return 0;
/* $Procedure  ZZGFPAIN ( Private --- GF, phase angle initialization ) */

L_zzgfpain:
/* $ Abstract */

/*     This is the initialization entry point used for describing */
/*     the event that is to be solved for by ZZGFSOLV. */

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

/*     PHASE ANGLE */
/*     EPHEMERIS */
/*     GEOMETRY */
/*     SEARCH */

/* $ Declarations */

/*      CHARACTER*(*)         TARGET */
/*      CHARACTER*(*)         ABCORR */
/*      CHARACTER*(*)         OBSRVR */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     TARGET     I   Name of the target body. */
/*     ILLMN      I   Name of the illuminating body. */
/*     ABCORR     I   Aberration correction flag. */
/*     OBSRVR     I   Name of the observing body. */

/* $ Detailed_Input */

/*     TARGET   the string name of a target body.  Optionally, you may */
/*              supply the integer ID code for the object as an */
/*              integer string.  For example both 'MOON' and '301' */
/*              are legitimate strings that indicate the moon is the */
/*              target body. */

/*              Case and leading or trailing blanks are not significant */
/*              in the string TARGET. */

/*     ILLMN    the string name of the illuminating body. This will */
/*              normally be 'SUN' but the algorithm can use any */
/*              ephemeris object */

/*              Case and leading or trailing blanks are not significant */
/*              in the string ILLMN. */

/*     ABCORR   the string description of the aberration corrections to */
/*              apply to the state evaluations to account for one-way */
/*              light time and stellar aberration. */

/*              Any aberration correction accepted by the SPICE */
/*              routine SPKEZR is accepted here. See the header */
/*              of SPKEZR for a detailed description of the */
/*              aberration correction options. For convenience, */
/*              the options are listed below: */

/*                 'NONE'     Apply no correction. Returns the "true" */
/*                            geometric state. */

/*                 'LT'       "Reception" case:  correct for */
/*                            one-way light time using a Newtonian */
/*                            formulation. */

/*                 'LT+S'     "Reception" case:  correct for */
/*                            one-way light time and stellar */
/*                            aberration using a Newtonian */
/*                            formulation. */

/*                 'CN'       "Reception" case:  converged */
/*                            Newtonian light time correction. */

/*                'CN+S'     "Reception" case:  converged */
/*                            Newtonian light time and stellar */
/*                            aberration corrections. */

/*              Note that this routine accepts only reception mode */
/*              aberration corrections. */

/*              The ABCORR string lacks sensitivity to case, leading */
/*              and trailing blanks. */

/*     OBSRVR   the string name of an observing body.  Optionally, you */
/*              may supply the ID code of the object as an integer */
/*              string. For example, both 'EARTH' and '399' are */
/*              legitimate strings to indicate the observer as Earth. */

/*              Case and leading or trailing blanks are not significant */
/*              in the string OBSRVR. */

/* $ Detailed_Output */

/*     None. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1) SPICE(IDCODENOTFOUND) signals if the object name for the */
/*        target, TARGET, is not a recognized name. */

/*     2) SPICE(IDCODENOTFOUND) signals if the object name for the */
/*        illuminator, ILLMN, is not a recognized name. */

/*     3) SPICE(IDCODENOTFOUND) signals if the object name for the */
/*        observer, OBSRVR, is not a recognized name. */

/*     4) SPICE(BODIESNOTDISTINCT) signals if the three objects */
/*        associated with a PHASE ANGLE search are not distinct. */

/*     5) SPICE(INVALIDOPTION) signals for any transmit mode aberration */
/*        correction. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     None. */

/* $ Examples */

/*     None. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman   (JPL) */
/*     E.D. Wright    (JPL) */

/* $ Version */

/* -    SPICELIB version 1.0.0 19-OCT-2010 (EDW)(NJB) */

/* -& */
/* $ Index_Entries */

/*     phase angle initialization routine */

/* -& */

/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }
    chkin_("ZZGFPAIN", (ftnlen)8);

/*     Find NAIF IDs for TARGET, ILLMN, and OBSRVR. */

    bods2c_(target, &svtarg, &found, target_len);
    if (! found) {
	setmsg_("The target object, '#', is not a recognized name for an eph"
		"emeris object. The cause of this problem may be that you nee"
		"d an updated version of the SPICE Toolkit. ", (ftnlen)162);
	errch_("#", target, (ftnlen)1, target_len);
	sigerr_("SPICE(IDCODENOTFOUND)", (ftnlen)21);
	chkout_("ZZGFPAIN", (ftnlen)8);
	return 0;
    }
    bods2c_(illmn, &svillm, &found, illmn_len);
    if (! found) {
	setmsg_("The illuminator object, '#', is not a recognized name for a"
		"n ephemeris object. The cause of this problem may be that yo"
		"u need an updated version of the SPICE toolkit. ", (ftnlen)
		167);
	errch_("#", obsrvr, (ftnlen)1, obsrvr_len);
	sigerr_("SPICE(IDCODENOTFOUND)", (ftnlen)21);
	chkout_("ZZGFPAIN", (ftnlen)8);
	return 0;
    }
    bods2c_(obsrvr, &svobs, &found, obsrvr_len);
    if (! found) {
	setmsg_("The observer object, '#', is not a recognized name for an e"
		"phemeris object. The cause of this problem may be that you n"
		"eed an updated version of the SPICE toolkit. ", (ftnlen)164);
	errch_("#", obsrvr, (ftnlen)1, obsrvr_len);
	sigerr_("SPICE(IDCODENOTFOUND)", (ftnlen)21);
	chkout_("ZZGFPAIN", (ftnlen)8);
	return 0;
    }

/*     Make sure the observer, illuminator, and target are distinct. */

    if (svtarg == svobs || svtarg == svillm || svobs == svillm) {
	setmsg_("The observer, illuminator, and target must be distinct obje"
		"cts, but are not: OBSRVR = #, TARGET = #, are not: ILLMN= #.",
		 (ftnlen)119);
	errch_("#", obsrvr, (ftnlen)1, obsrvr_len);
	errch_("#", target, (ftnlen)1, target_len);
	errch_("#", illmn, (ftnlen)1, illmn_len);
	sigerr_("SPICE(BODIESNOTDISTINCT)", (ftnlen)24);
	chkout_("ZZGFPAIN", (ftnlen)8);
	return 0;
    }

/*     Squeeze all blanks out of the aberration correction */
/*     string; ensure the string is in upper case. */

    cmprss_(" ", &c__0, abcorr, svabco, (ftnlen)1, abcorr_len, (ftnlen)5);
    ucase_(svabco, svabco, (ftnlen)5, (ftnlen)5);

/*     Check the aberration correction. If SPKEZR can't handle it, */
/*     neither can we. */

    zzvalcor_(svabco, svablk, (ftnlen)5);
    if (failed_()) {
	chkout_("ZZGFPAIN", (ftnlen)8);
	return 0;
    }

/*     Restrict correction to reception cases. */

    if (svablk[4]) {
	setmsg_("Invalid aberration correction '#'. Phase angle geometry cal"
		"culations currently restricted to reception cases.", (ftnlen)
		109);
	errch_("#", abcorr, (ftnlen)1, abcorr_len);
	sigerr_("SPICE(INVALIDOPTION)", (ftnlen)20);
	chkout_("ZZGFPAIN", (ftnlen)8);
	return 0;
    }
    chkout_("ZZGFPAIN", (ftnlen)8);
    return 0;
/* $Procedure ZZGFPADC ( Private --- GF, when phase angle is decreasing ) */

L_zzgfpadc:
/* $ Abstract */

/*     Computes whether or not the phase angle between the observer */
/*     and the target is decreasing at time ET. */

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

/*     EPHEMERIS */
/*     GEOMETRY */
/*     SEARCH */

/* $ Declarations */

/*     DOUBLE PRECISION      ET */
/*     LOGICAL               DECRES */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     ET         I   Ephemeris seconds past J2000 TDB. */
/*     DECRES     O   .TRUE. if phase angle is decreasing, .FALSE. */
/*                    otherwise. */

/* $ Detailed_Input */

/*     ET         the time in ephemeris seconds past J2000 TDB at which */
/*                to determine whether the phase angle between SVTARG */
/*                and SVILLMN as seen from SVOBS is decreasing. */

/* $ Detailed_Output */

/*     DECRES     is .TRUE. if the phase angle is decreasing at ET. */
/*                Otherwise it is .FALSE.. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     None. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     A function f(x) is strictly decreasing at x0 if and only if there */
/*     exists some delta > 0 such that for all dx satisfying */

/*        0  <  dx  < delta */

/*     we have */

/*        f(x0)       <  f(x0 + dx) */

/*     and */

/*        f(x0 - dx)  <  f(x) */

/*     Note that a strictly decreasing function need not be */
/*     differentiable in a neighborhood of x0; it can have jump */
/*     discontinuities in any neighborhood of x0 and even at x0. */

/* $ Examples */

/*     None. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman   (JPL) */
/*     E.D. Wright    (JPL) */

/* $ Version */

/* -    SPICELIB version 1.0.0 10-JAN-2012 (EDW) */

/* -& */
/* $ Index_Entries */

/*     when phase angle is decreasing */

/* -& */

/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }
    chkin_("ZZGFPADC", (ftnlen)8);

/*     Get the state of the TARG object relative to OBS at ET. */

    spkez_(&svtarg, et, "J2000", svabco, &svobs, s1, &lt, (ftnlen)5, (ftnlen)
	    5);
    if (failed_()) {
	chkout_("ZZGFPADC", (ftnlen)8);
	return 0;
    }

/*     Get the state of the ILLMN object relative to TARG at ET */
/*     for no aberration correction, or ET - LT otherwise. */

    if (svablk[0]) {

/*        No correction, geometric. */

	spkez_(&svillm, et, "J2000", svabco, &svtarg, s2, &lt, (ftnlen)5, (
		ftnlen)5);
    } else {
	d__1 = *et - lt;
	spkez_(&svillm, &d__1, "J2000", svabco, &svtarg, s2, &lt, (ftnlen)5, (
		ftnlen)5);
	if (failed_()) {
	    chkout_("ZZGFPADC", (ftnlen)8);
	    return 0;
	}

/*        Correct velocity for time derivative of the observer target */
/*        light-time. We need to do this since the SPK evaluation occurs */
/*        at ET - LT. */

/*        d( ET - LT ) = (1 - d LT ) */
/*        ------------        ---- */
/*        dt                  dt */

/*        LT = ||R|| */
/*             ----- */
/*               C */
/*                                     ^ */
/*        d LT    = 1   d ||R||  = 1 < R, V > */
/*        ----      -   -------    - */
/*        dt        C   dt         C */


	vhat_(s1, uvec);
	dlt = vdot_(uvec, &s1[3]) / clight_();

/*        Apply the correction to the velocity vector components. */

	d__1 = 1. - dlt;
	vsclip_(&d__1, &s2[3]);
    }
    if (failed_()) {
	chkout_("ZZGFPADC", (ftnlen)8);
	return 0;
    }

/*                       ILLMN      OBS */
/*       ILLMN as seen      *       / */
/*       from TARG at       |      / */
/*       ET - LT.           |     / */
/*                         >|..../< phase angle */
/*                          |   / */
/*                        . |  / */
/*                      .   | / */
/*                     .     *     TARG as seen from OBS */
/*               SEP   .   TARG    at ET */
/*                      .  / */
/*                        / */
/*                       * */

/*     Calculate the derivative of the angle separating the vectors */
/*     relative to TARG. */

/*        PI = SEP + PHASE */

/*        dPHASE     dSEP */
/*        ------ = - ---- */
/*         dt         dt */

    dsep = dvsep_(s1, s2);
    *decres = -dsep < 0.;
    chkout_("ZZGFPADC", (ftnlen)8);
    return 0;
/* $Procedure ZZGFPAGQ ( Private --- GF, phase angle between two bodies ) */

L_zzgfpagq:
/* $ Abstract */

/*     Determine the phase angle between the centers of the two */
/*     bodies. */

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

/*     PHASE ANGLE */
/*     EPHEMERIS */
/*     GEOMETRY */
/*     SEARCH */

/* $ Declarations */

/*      DOUBLE PRECISION      ET */
/*      DOUBLE PRECISION      RVL */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     ET         I   Ephemeris seconds past J2000 TDB. */
/*     RVL        O   Phase angle at time ET. */

/* $ Detailed_Input */

/*     ET         the time in ephemeris seconds past J2000 TDB at which */
/*                to compute the phase angle. */

/* $ Detailed_Output */

/*     RVL        the phase angle between SVTARG and SVILLMN as seen */
/*                from SVOBS at time ET. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     None. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     None. */

/* $ Examples */

/*     None. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman   (JPL) */
/*     E.D. Wright    (JPL) */

/* $ Version */

/* -    SPICELIB version 1.0.0 19-OCT-2010 (EDW) */

/* -& */
/* $ Index_Entries */

/*     get phase angle between two bodies */

/* -& */
    zzgfpaq_(et, &svtarg, &svillm, &svobs, svabco, rvl, (ftnlen)5);
    return 0;
/* $Procedure ZZGFPAX ( Private -- GF, retrieve ZZGFPAIN values ) */

L_zzgfpax:
/* $ Abstract */

/*     Retrieve values set in ZZGFPAIN. */

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

/*     None. */

/* $ Declarations */

/*     None. */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     XTARG      O   Saved value for the target body. */
/*     XILLMN     O   Saved value for the illuminator body. */
/*     XABCOR     O   Saved value for the aberration correction. */
/*     XOBS       O   Saved value for the observing body. */
/*     XABLK      O   Saved value for the aberration correction */
/*                    attribute block. */

/* $ Detailed_Input */

/*     None. */

/* $ Detailed_Output */

/*     XTARG    SPICE ID value for the target body initialized */
/*              via ZZGFPAIN. */

/*     XILLMN   SPICE ID value for the illuminator body initialized */
/*              via ZZGFPAIN. */

/*     XABCOR   String value for the aberration correction initialized */
/*              via ZZGFPAIN. */

/*     XOBS     SPICE ID value for the observing body initialized */
/*              via ZZGFPAIN. */

/*     XABLK    Aberration correction attribute block as initialized */
/*              by ABCORR. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     None. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     None. */

/* $ Examples */

/*     None. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman   (JPL) */
/*     E.D. Wright    (JPL) */

/* $ Version */

/* -    SPICELIB version 1.0.0 16-SEP-2012 (EDW) */

/* -& */
/* $ Index_Entries */

/*     get saved phase angle parameters */

/* -& */
    *xtarg = svtarg;
    *xillmn = svillm;
    s_copy(xabcor, svabco, xabcor_len, (ftnlen)5);
    *xobs = svobs;
    for (i__ = 1; i__ <= 6; ++i__) {
	xablk[(i__1 = i__ - 1) < 15 && 0 <= i__1 ? i__1 : s_rnge("xablk", 
		i__1, "zzgfpau_", (ftnlen)1083)] = svablk[(i__2 = i__ - 1) < 
		15 && 0 <= i__2 ? i__2 : s_rnge("svablk", i__2, "zzgfpau_", (
		ftnlen)1083)];
    }
    return 0;
} /* zzgfpau_ */

/* Subroutine */ int zzgfpau_(char *target, char *illmn, char *abcorr, char *
	obsrvr, U_fp udfunc, doublereal *et, logical *decres, doublereal *rvl,
	 integer *xtarg, integer *xillmn, char *xabcor, integer *xobs, 
	logical *xablk, ftnlen target_len, ftnlen illmn_len, ftnlen 
	abcorr_len, ftnlen obsrvr_len, ftnlen xabcor_len)
{
    return zzgfpau_0_(0, target, illmn, abcorr, obsrvr, udfunc, et, decres, 
	    rvl, xtarg, xillmn, xabcor, xobs, xablk, target_len, illmn_len, 
	    abcorr_len, obsrvr_len, xabcor_len);
    }

/* Subroutine */ int zzgfpain_(char *target, char *illmn, char *abcorr, char *
	obsrvr, ftnlen target_len, ftnlen illmn_len, ftnlen abcorr_len, 
	ftnlen obsrvr_len)
{
    return zzgfpau_0_(1, target, illmn, abcorr, obsrvr, (U_fp)0, (doublereal *
	    )0, (logical *)0, (doublereal *)0, (integer *)0, (integer *)0, (
	    char *)0, (integer *)0, (logical *)0, target_len, illmn_len, 
	    abcorr_len, obsrvr_len, (ftnint)0);
    }

/* Subroutine */ int zzgfpadc_(U_fp udfunc, doublereal *et, logical *decres)
{
    return zzgfpau_0_(2, (char *)0, (char *)0, (char *)0, (char *)0, udfunc, 
	    et, decres, (doublereal *)0, (integer *)0, (integer *)0, (char *)
	    0, (integer *)0, (logical *)0, (ftnint)0, (ftnint)0, (ftnint)0, (
	    ftnint)0, (ftnint)0);
    }

/* Subroutine */ int zzgfpagq_(doublereal *et, doublereal *rvl)
{
    return zzgfpau_0_(3, (char *)0, (char *)0, (char *)0, (char *)0, (U_fp)0, 
	    et, (logical *)0, rvl, (integer *)0, (integer *)0, (char *)0, (
	    integer *)0, (logical *)0, (ftnint)0, (ftnint)0, (ftnint)0, (
	    ftnint)0, (ftnint)0);
    }

/* Subroutine */ int zzgfpax_(integer *xtarg, integer *xillmn, char *xabcor, 
	integer *xobs, logical *xablk, ftnlen xabcor_len)
{
    return zzgfpau_0_(4, (char *)0, (char *)0, (char *)0, (char *)0, (U_fp)0, 
	    (doublereal *)0, (logical *)0, (doublereal *)0, xtarg, xillmn, 
	    xabcor, xobs, xablk, (ftnint)0, (ftnint)0, (ftnint)0, (ftnint)0, 
	    xabcor_len);
    }

