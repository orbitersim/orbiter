/* zzgfrru.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__0 = 0;

/* $Procedure ZZGFRRU ( Private --- GF, range rate utility routine ) */
/* Subroutine */ int zzgfrru_0_(int n__, char *target, char *abcorr, char *
	obsrvr, doublereal *dt, U_fp udfunc, doublereal *et, logical *decres, 
	doublereal *rvl, integer *xtarg, char *xabcor, integer *xobs, 
	doublereal *xdt, ftnlen target_len, ftnlen abcorr_len, ftnlen 
	obsrvr_len, ftnlen xabcor_len)
{
    /* System generated locals */
    doublereal d__1;

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    doublereal dfdt[6];
    extern doublereal vdot_(doublereal *, doublereal *);
    static doublereal svdt;
    extern /* Subroutine */ int zzvalcor_(char *, logical *, ftnlen);
    integer n;
    extern /* Subroutine */ int chkin_(char *, ftnlen), ucase_(char *, char *,
	     ftnlen, ftnlen), errch_(char *, char *, ftnlen, ftnlen), dvhat_(
	    doublereal *, doublereal *);
    logical found;
    doublereal drvel, state[6], srhat[6];
    static char svref[32];
    static integer svobs;
    extern /* Subroutine */ int spkez_(integer *, doublereal *, char *, char *
	    , integer *, doublereal *, doublereal *, ftnlen, ftnlen), bods2c_(
	    char *, integer *, logical *, ftnlen);
    extern logical failed_(void);
    doublereal lt;
    static char svabco[5];
    logical attblk[15];
    extern /* Subroutine */ int qderiv_(integer *, doublereal *, doublereal *,
	     doublereal *, doublereal *), sigerr_(char *, ftnlen), chkout_(
	    char *, ftnlen), setmsg_(char *, ftnlen);
    doublereal states[12]	/* was [6][2] */;
    static integer svtarg;
    extern /* Subroutine */ int cmprss_(char *, integer *, char *, char *, 
	    ftnlen, ftnlen, ftnlen);
    extern logical return_(void);
    extern /* Subroutine */ int zzgfrrq_(doublereal *, integer *, integer *, 
	    char *, doublereal *, ftnlen);

/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines.  Users should not call this routine directly due */
/*     to the volatile nature of this routine. */

/*     This is the umbrella routine for the entry points needed by */
/*     GFEVNT in order to find range rate events. */

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

/*     RANGE RATE */
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
/*     TARGET     I   ZZGFRRIN */
/*     ABCORR     I   ZZGFRRIN */
/*     OBSRVR     I   ZZGFRRIN */
/*     ET         I   ZZGFRRDC, ZZGFRRGQ */
/*     DT         I   ZZGFRRIN */
/*     UDFUNC     I   ZZGFRRDC */
/*     DECRES     O   ZZGFRRDC */
/*     RVL        O   ZZGFRRGQ */
/*     XTARG      O   ZZGFRRX */
/*     XABCOR     O   ZZGFRRX */
/*     XOBS       O   ZZGFRRX */
/*     XDT        O   ZZGFRRX */

/* $ Detailed_Input */

/*     TARGET   the string name of a target body.  Optionally, you may */
/*              supply the integer ID code for the object as an */
/*              integer string.  For example both 'MOON' and '301' */
/*              are legitimate strings that indicate the moon is the */
/*              target body. */

/*              The target and observer define a position vector */
/*              that points from the observer to the target. */

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

/*                 'CN+S'     "Reception" case:  converged */
/*                            Newtonian light time and stellar */
/*                            aberration corrections. */

/*                 'XLT'      "Transmission" case:  correct for */
/*                            one-way light time using a Newtonian */
/*                            formulation. */

/*                 'XLT+S'    "Transmission" case:  correct for */
/*                            one-way light time and stellar */
/*                            aberration using a Newtonian */
/*                            formulation. */

/*                 'XCN'      "Transmission" case:  converged */
/*                            Newtonian light time correction. */

/*                 'XCN+S'    "Transmission" case:  converged */
/*                            Newtonian light time and stellar */
/*                            aberration corrections. */

/*              The ABCORR string lacks sensitivity to case, leading */
/*              and trailing blanks. */

/*     OBSRVR   the string name of an observing body.  Optionally, you */
/*              may supply the ID code of the object as an integer */
/*              string. For example, both 'EARTH' and '399' are */
/*              legitimate strings to indicate the observer as Earth. */

/*     ET       time in TDB seconds past J2000 at which to calculate */
/*              the value of or characteristic of the range rate of */
/*              the observer-target vector. */

/*     DT       a scalar double precision value representing half the */
/*              interval in TDB seconds separating the evaluation */
/*              epochs; the evaluations occur at epochs */
/*              (ET + DT) and (ET - DT). */

/*              DT may be negative but must be non-zero. */

/*     XTARG    SPICE ID value for the target body initialized */
/*              via ZZGFRRIN. */

/*     XABCOR   String value for the aberration correction initialized */
/*              via ZZGFRRIN. */

/*     XOBS     SPICE ID value for the observing body initialized */
/*              via ZZGFRRIN. */

/*     XDT      Saved value for DT. */

/*     For more information, see individual entry points. */

/* $ Detailed_Output */

/*     None. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     None. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine serves as the umbrella routine for three entry points */
/*     needed by GFEVNT in solving for range rate conditions. */

/*     The three entry points are */

/*        ZZGFRRIN --- an initialization routine that must be called */
/*                     prior to attempting to solve for any range */
/*                     rate event. */

/*        ZZGFRRDC --- determines whether or not range rate is */
/*                     decreasing at some time. */

/*        ZZGFRRGQ --- returns the range rate of the two objects */
/*                     of concern as a function of ET. */

/* $ Examples */

/*     None. */

/* $ Restrictions */

/*     ZZGFRRIN must be called prior to use of any of the other */
/*     entry points (think constructor). */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     W.L. Taber     (JPL) */
/*     I.M. Underwood (JPL) */
/*     L.S. Elson     (JPL) */

/* $ Version */

/* -    SPICELIB version 2.0.1 01-OCT-2021 (NJB) */

/*        Fixed typo in comments. */

/* -    SPICELIB version 2.0.0 21-APR-2014 (EDW) */

/*        Added BOGUSENTRY error check to call ZZGFRRU. */

/*        Added ZZGFRRX entry point for test family */
/*        retrieval of values saved by ZZGFRRIN. */

/*        Code edits to implement use of ZZGFRELX. */
/*        These edits include removal of unneeded routines: */

/*           ZZGFRRUR */
/*           ZZGFRRLT */

/*        and corresponding unused variables. */

/*        Update to header entries. */

/* -    SPICELIB version 1.0.1 08-JUL-2010 (EDW) */

/*        Minor typo corrections to headers. */

/* -    SPICELIB version 1.0.0 09-OCT-2009 (LSE)(EDW) */

/* -& */
/* $ Index_Entries */

/*     find range rate events */

/* -& */

/*     SPICELIB functions */


/*     Local Variables */


/*     Saved Variables */

    switch(n__) {
	case 1: goto L_zzgfrrin;
	case 2: goto L_zzgfrrdc;
	case 3: goto L_zzgfrrgq;
	case 4: goto L_zzgfrrx;
	}

    chkin_("ZZGFRRU", (ftnlen)7);
    sigerr_("SPICE(BOGUSENTRY)", (ftnlen)17);
    chkout_("ZZGFRRU", (ftnlen)7);
    return 0;
/* $Procedure ZZGFRRIN ( Private --- GF, range rate initialization ) */

L_zzgfrrin:
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

/*     RANGE RATE */
/*     EPHEMERIS */
/*     GEOMETRY */
/*     SEARCH */

/* $ Declarations */

/*      CHARACTER*(*)         TARGET */
/*      CHARACTER*(*)         ABCORR */
/*      CHARACTER*(*)         OBSRVR */
/*      DOUBLE PRECISION      DT */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     TARGET     I   Name of the target body */
/*     ABCORR     I   Aberration correction flag */
/*     OBSRVR     I   Name of the observing body */
/*     DT         I   Interval from ET for derivative calculation. */

/* $ Detailed_Input */

/*     TARGET   the string name of a target body.  Optionally, you may */
/*              supply the integer ID code for the object as an */
/*              integer string.  For example both 'MOON' and '301' */
/*              are legitimate strings that indicate the moon is the */
/*              target body. */

/*              The target and observer define a position vector */
/*              that points from the observer to the target. */

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

/*                 'XLT'      "Transmission" case:  correct for */
/*                            one-way light time using a Newtonian */
/*                            formulation. */

/*                 'XLT+S'    "Transmission" case:  correct for */
/*                            one-way light time and stellar */
/*                            aberration using a Newtonian */
/*                            formulation. */

/*                 'XCN'      "Transmission" case:  converged */
/*                            Newtonian light time correction. */

/*                 'XCN+S'    "Transmission" case:  converged */
/*                            Newtonian light time and stellar */
/*                            aberration corrections. */

/*              The ABCORR string lacks sensitivity to case, leading */
/*              and trailing blanks. */

/*     OBSRVR   the string name of an observing body.  Optionally, you */
/*              may supply the ID code of the object as an integer */
/*              string. For example, both 'EARTH' and '399' are */
/*              legitimate strings to indicate the observer as Earth. */

/*     DT         a scalar double precision value representing half the */
/*                interval in TDB seconds separating the evaluation */
/*                epochs; the evaluations occur at epochs */
/*                (ET + DT) and (ET - DT). */

/*                DT may be negative but must be non-zero. */

/* $ Detailed_Output */

/*     None. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1) SPICE(IDCODENOTFOUND) signals if the object name for the */
/*        target, TARGET, is not a recognized name. */

/*     2) SPICE(IDCODENOTFOUND) signals if the object name for the */
/*        observer, OBSRVR, is not a recognized name. */

/*     3) SPICE(BODIESNOTDISTINCT) signals if the two objects */
/*        associated with a range rate search are not distinct. */

/*     4) SPICE(INVALIDVALUE) signals for the delta, DT, equal to zero. */

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

/*     W.L. Taber     (JPL) */
/*     I.M. Underwood (JPL) */
/*     L.S. Elson     (JPL) */

/* $ Version */

/* -    SPICELIB version 2.0.0 18-SEP-2012 (EDW) */

/*        Added proper Exceptions section. */

/*        Added error check on value of DT to ensure non-zero value. */

/*        REFVAL removed from routine argument list due to the use */
/*        of ZZGFRELX to calculate the events. */

/*        Minor typo correction to header. */

/* -    SPICELIB version 1.0.0 09-OCT-2009 (LSE)(EDW) */

/* -& */
/* $ Index_Entries */

/*     range rate initialization routine. */

/* -& */
    if (return_()) {
	return 0;
    }
    chkin_("ZZGFRRIN", (ftnlen)8);

/*     Find NAIF IDs for TARGET and OBSRVR. */

    bods2c_(target, &svtarg, &found, target_len);
    if (! found) {
	setmsg_("The target object, '#', is not a recognized name for an eph"
		"emeris object. The cause of this problem may be that you nee"
		"d an updated version of the SPICE Toolkit. ", (ftnlen)162);
	errch_("#", target, (ftnlen)1, target_len);
	sigerr_("SPICE(IDCODENOTFOUND)", (ftnlen)21);
	chkout_("ZZGFRRIN", (ftnlen)8);
	return 0;
    }
    bods2c_(obsrvr, &svobs, &found, obsrvr_len);
    if (! found) {
	setmsg_("The observer, '#', is not a recognized name for an ephemeri"
		"s object. The cause of this problem may be that you need an "
		"updated version of the SPICE toolkit. ", (ftnlen)157);
	errch_("#", obsrvr, (ftnlen)1, obsrvr_len);
	sigerr_("SPICE(IDCODENOTFOUND)", (ftnlen)21);
	chkout_("ZZGFRRIN", (ftnlen)8);
	return 0;
    }

/*     Make sure the observer and target are distinct. */

    if (svtarg == svobs) {
	setmsg_("The observer and target must be distinct objects, but are n"
		"ot: OBSRVR = #; TARGET = #.", (ftnlen)86);
	errch_("#", obsrvr, (ftnlen)1, obsrvr_len);
	errch_("#", target, (ftnlen)1, target_len);
	sigerr_("SPICE(BODIESNOTDISTINCT)", (ftnlen)24);
	chkout_("ZZGFRRIN", (ftnlen)8);
	return 0;
    }

/*     Squeeze all blanks out of the aberration correction */
/*     string; ensure the string is in upper case. */

    cmprss_(" ", &c__0, abcorr, svabco, (ftnlen)1, abcorr_len, (ftnlen)5);
    ucase_(svabco, svabco, (ftnlen)5, (ftnlen)5);

/*     Check the aberration correction. If SPKEZR can't handle it, */
/*     neither can we. */

    zzvalcor_(svabco, attblk, (ftnlen)5);
    if (failed_()) {
	chkout_("ZZGFRRIN", (ftnlen)8);
	return 0;
    }

/*     The "delta" argument for QDERIV, DT, must have a non-zero value. */

    if (*dt == 0.) {
	setmsg_("Delta value for QDERIV is zero; a non-zero value is require"
		"d.", (ftnlen)61);
	sigerr_("SPICE(INVALIDVALUE)", (ftnlen)19);
	chkout_("ZZGFRRIN", (ftnlen)8);
	return 0;
    }

/*     Save the DT value. */

    s_copy(svref, "J2000", (ftnlen)32, (ftnlen)5);
    svdt = *dt;
    chkout_("ZZGFRRIN", (ftnlen)8);
    return 0;
/* $Procedure ZZGFRRDC (  Private --- GF, when range rate is decreasing ) */

L_zzgfrrdc:
/* $ Abstract */

/*     Computes whether or not the range rate between the observer */
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

/*     RANGE RATE */
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
/*     DECRES     O   true if range rate is decreasing, false */
/*                    otherwise. */

/* $ Detailed_Input */

/*     ET         time in seconds past J2000 at which to calculate */
/*                whether the range rate of the observer-target vector */
/*                is decreasing. */

/* $ Detailed_Output */

/*     DECRES     is true if the range rate between the objects */
/*                is decreasing, false otherwise. */


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

/*     W.L. Taber     (JPL) */
/*     I.M. Underwood (JPL) */
/*     L.S. Elson     (JPL) */

/* $ Version */

/* -    SPICELIB version 2.0.0 18-FEB-2011 (EDW) */

/*        Added UDFUNC to argument list for use of ZZGFRELX when */
/*        calculating the events. */

/*        Minor typo correction to comments. */

/* -    SPICELIB version 1.0.0 09-OCT-2009 (LSE)(EDW) */

/* -& */
/* $ Index_Entries */

/*     when range rate is decreasing */

/* -& */

/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }
    chkin_("ZZGFRRDC", (ftnlen)8);
    n = 6;

/*     The range rate of interest is of SVTARG relative to the SVOBS. */
/*     The function requires the acceleration of SVTARG relative */
/*     to SVOBS. */

    d__1 = *et - svdt;
    spkez_(&svtarg, &d__1, svref, svabco, &svobs, states, &lt, (ftnlen)32, (
	    ftnlen)5);
    d__1 = *et + svdt;
    spkez_(&svtarg, &d__1, svref, svabco, &svobs, &states[6], &lt, (ftnlen)32,
	     (ftnlen)5);

/*     Approximate the derivative of the position and velocity by */
/*     finding the derivative of a quadratic approximating function. */

/*        DFDT(1) = Vx */
/*        DFDT(2) = Vy */
/*        DFDT(3) = Vz */
/*        DFDT(4) = Ax */
/*        DFDT(5) = Ay */
/*        DFDT(6) = Az */

    qderiv_(&n, states, &states[6], &svdt, dfdt);
    spkez_(&svtarg, et, svref, svabco, &svobs, state, &lt, (ftnlen)32, (
	    ftnlen)5);
    if (failed_()) {
	chkout_("ZZGFRRDC", (ftnlen)8);
	return 0;
    }

/*        d ||r||     ^ */
/*        ------- = < r, v > */
/*        dt */

/*         2           ^          ^ */
/*        d ||r||   < dr, v > + < r, dv > */
/*        ------- =   --             -- */
/*          2 */
/*        dt          dt             dt */

    dvhat_(state, srhat);
    drvel = vdot_(&dfdt[3], srhat) + vdot_(&state[3], &srhat[3]);
    *decres = drvel < 0.;
    chkout_("ZZGFRRDC", (ftnlen)8);
    return 0;
/* $Procedure ZZGFRRGQ ( Private --- GF, get range rate between bodies ) */

L_zzgfrrgq:
/* $ Abstract */

/*     Determine the range rate between the centers of the two */
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

/*     RANGE RATE */
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
/*     RVL        O   Range rate at time ET. */

/* $ Detailed_Input */

/*     ET         time in ephemeris seconds past J2000 when the range */
/*                rate between the two bodies is to be computed. */

/* $ Detailed_Output */

/*     RVL        is the range rate of SVTARG as seen from SVOBS at */
/*                time ET. */

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

/*     W.L. Taber     (JPL) */
/*     I.M. Underwood (JPL) */
/*     L.S. Elson     (JPL) */

/* $ Version */

/* -    SPICELIB version 1.0.0 09-JUN-2009 (LSE)(EDW) */

/* -& */
/* $ Index_Entries */

/*     get range rate between two bodies */

/* -& */
    zzgfrrq_(et, &svtarg, &svobs, svabco, rvl, (ftnlen)5);
    return 0;
/* $Procedure ZZGFRRX ( Private -- GF, retrieve ZZGFRRIN values ) */

L_zzgfrrx:
/* $ Abstract */

/*     Retrieve values set in ZZGFRRIN. */

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
/*     XABCOR     O   Saved value for the aberration correction. */
/*     XOBS       O   Saved value for the observing body. */
/*     XDT        O   Saved value for DT. */

/* $ Detailed_Input */

/*     None. */

/* $ Detailed_Output */

/*     XTARG    SPICE ID value for the target body initialized */
/*              via ZZGFRRIN. */

/*     XABCOR   String value for the aberration correction initialized */
/*              via ZZGFRRIN. */

/*     XOBS     SPICE ID value for the observing body initialized */
/*              via ZZGFRRIN. */

/*     XDT      Saved value for DT. */

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

/*     get saved range rate parameters */

/* -& */
    *xtarg = svtarg;
    s_copy(xabcor, svabco, xabcor_len, (ftnlen)5);
    *xobs = svobs;
    *xdt = svdt;
    return 0;
} /* zzgfrru_ */

/* Subroutine */ int zzgfrru_(char *target, char *abcorr, char *obsrvr, 
	doublereal *dt, U_fp udfunc, doublereal *et, logical *decres, 
	doublereal *rvl, integer *xtarg, char *xabcor, integer *xobs, 
	doublereal *xdt, ftnlen target_len, ftnlen abcorr_len, ftnlen 
	obsrvr_len, ftnlen xabcor_len)
{
    return zzgfrru_0_(0, target, abcorr, obsrvr, dt, udfunc, et, decres, rvl, 
	    xtarg, xabcor, xobs, xdt, target_len, abcorr_len, obsrvr_len, 
	    xabcor_len);
    }

/* Subroutine */ int zzgfrrin_(char *target, char *abcorr, char *obsrvr, 
	doublereal *dt, ftnlen target_len, ftnlen abcorr_len, ftnlen 
	obsrvr_len)
{
    return zzgfrru_0_(1, target, abcorr, obsrvr, dt, (U_fp)0, (doublereal *)0,
	     (logical *)0, (doublereal *)0, (integer *)0, (char *)0, (integer 
	    *)0, (doublereal *)0, target_len, abcorr_len, obsrvr_len, (ftnint)
	    0);
    }

/* Subroutine */ int zzgfrrdc_(U_fp udfunc, doublereal *et, logical *decres)
{
    return zzgfrru_0_(2, (char *)0, (char *)0, (char *)0, (doublereal *)0, 
	    udfunc, et, decres, (doublereal *)0, (integer *)0, (char *)0, (
	    integer *)0, (doublereal *)0, (ftnint)0, (ftnint)0, (ftnint)0, (
	    ftnint)0);
    }

/* Subroutine */ int zzgfrrgq_(doublereal *et, doublereal *rvl)
{
    return zzgfrru_0_(3, (char *)0, (char *)0, (char *)0, (doublereal *)0, (
	    U_fp)0, et, (logical *)0, rvl, (integer *)0, (char *)0, (integer *
	    )0, (doublereal *)0, (ftnint)0, (ftnint)0, (ftnint)0, (ftnint)0);
    }

/* Subroutine */ int zzgfrrx_(integer *xtarg, char *xabcor, integer *xobs, 
	doublereal *xdt, ftnlen xabcor_len)
{
    return zzgfrru_0_(4, (char *)0, (char *)0, (char *)0, (doublereal *)0, (
	    U_fp)0, (doublereal *)0, (logical *)0, (doublereal *)0, xtarg, 
	    xabcor, xobs, xdt, (ftnint)0, (ftnint)0, (ftnint)0, xabcor_len);
    }

