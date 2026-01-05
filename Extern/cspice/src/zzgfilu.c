/* zzgfilu.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__0 = 0;
static integer c__3 = 3;

/* $Procedure ZZGFILU ( GF, illumination angle utilities ) */
/* Subroutine */ int zzgfilu_0_(int n__, char *method, char *angtyp, char *
	target, char *illum, char *fixref, char *abcorr, char *obsrvr, 
	doublereal *spoint, doublereal *et, U_fp udfunc, logical *decres, 
	doublereal *angle, ftnlen method_len, ftnlen angtyp_len, ftnlen 
	target_len, ftnlen illum_len, ftnlen fixref_len, ftnlen abcorr_len, 
	ftnlen obsrvr_len)
{
    /* Initialized data */

    static char angnms[50*3] = "PHASE                                       "
	    "      " "INCIDENCE                                         " 
	    "EMISSION                                          ";

    /* System generated locals */
    integer i__1;

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_cmp(char *, char *, ftnlen, ftnlen), s_rnge(char *, integer, 
	    char *, integer);

    /* Local variables */
    doublereal rate;
    extern /* Subroutine */ int vequ_(doublereal *, doublereal *), zzvalcor_(
	    char *, logical *, ftnlen), zzilusta_(char *, char *, char *, 
	    doublereal *, char *, char *, char *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, ftnlen, ftnlen, ftnlen, 
	    ftnlen, ftnlen, ftnlen);
    integer n;
    doublereal radii[3];
    extern /* Subroutine */ int chkin_(char *, ftnlen), ucase_(char *, char *,
	     ftnlen, ftnlen), errch_(char *, char *, ftnlen, ftnlen);
    logical found;
    static char svref[32];
    static integer svobs;
    extern /* Subroutine */ int ljust_(char *, char *, ftnlen, ftnlen), 
	    bods2c_(char *, integer *, logical *, ftnlen);
    extern logical failed_(void);
    integer fxfcde;
    extern integer esrchc_(char *, integer *, char *, ftnlen, ftnlen);
    doublereal angles[3], incsta[2];
    static char svinam[36];
    doublereal emista[2], ettarg, normal[3];
    static char svonam[36];
    doublereal phssta[2], srfvec[3];
    static char svmeth[200], svtnam[36];
    integer fxclss, fxtyid, fxcent;
    extern logical return_(void);
    static char svcorr[5];
    static doublereal svnrml[3];
    static integer svaidx, svilum, svtarg;
    static logical svablk[15];
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), setmsg_(char *, ftnlen), cmprss_(char *, integer *, char 
	    *, char *, ftnlen, ftnlen, ftnlen), bodvrd_(char *, char *, 
	    integer *, integer *, doublereal *, ftnlen, ftnlen), surfnm_(
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *), namfrm_(char *, integer *, ftnlen), frinfo_(
	    integer *, integer *, integer *, integer *, logical *), errint_(
	    char *, integer *, ftnlen), illumg_(char *, char *, char *, 
	    doublereal *, char *, char *, char *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, ftnlen, 
	    ftnlen, ftnlen, ftnlen, ftnlen, ftnlen);
    static doublereal svsspt[3];

/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines. Users should not call this routine directly due */
/*     to the volatile nature of this routine. */

/*     This is the umbrella routine for the entry points used by */
/*     GFEVNT in order to find illumination angle events. */

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

/*     GF */
/*     NAIF_IDS */
/*     SPK */
/*     TIME */

/* $ Keywords */

/*     ANGLE */
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

/*     VARIABLE  I/O  ENTRY POINTS */
/*     --------  ---  -------------------------------------------------- */
/*     METHOD     I   ZZGFILIN */
/*     ANGTYP     I   ZZGFILIN */
/*     TARGET     I   ZZGFILIN */
/*     ILLUM      I   ZZGFILIN */
/*     FIXREF     I   ZZGFILIN */
/*     ABCORR     I   ZZGFILIN */
/*     OBSRVR     I   ZZGFILIN */
/*     SPOINT     I   ZZGFILIN */
/*     ET         I   ZZGFILLT, ZZGFILGQ */
/*     UDFUNC     I   ZZGFILDC */
/*     DECRES     O   ZZGFILDC */
/*     ANGLE      O   ZZGFILGQ */

/* $ Detailed_Input */

/*     See individual entry points. */

/* $ Detailed_Output */

/*     See individual entry points. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  See individual entry points. */

/* $ Files */

/*     Appropriate kernels must be loaded by the calling program before */
/*     this routine is called. */

/*     The following data are required: */

/*     -  SPK data: ephemeris data for target, observer, and */
/*        illumination source, for the times at which state or */
/*        positions are computed, must be loaded. If aberration */
/*        corrections are used, the states of target and observer */
/*        relative to the solar system barycenter must be calculable */
/*        from the available ephemeris data. Typically ephemeris data */
/*        are made available by loading one or more SPK files via */
/*        FURNSH. */

/*     -  If non-inertial reference frames are used, then PCK */
/*        files, frame kernels, C-kernels, and SCLK kernels may be */
/*        needed. */

/*     In all cases, kernel data are normally loaded once per program */
/*     run, NOT every time this routine is called. */

/* $ Particulars */

/*     This is an umbrella for routines required by the GF scalar */
/*     quantity search algorithm to support searches involving */
/*     illumination angle constraints. */

/*     The entry points of this routine are: */

/*        ZZGFILIN   Saves the user-supplied inputs defining the */
/*                   illumination angle computation to be performed. */
/*                   Initializes the illumination angle search. */

/*        ZZGFILDC   Determines whether or not a specified illumination */
/*                   angle is decreasing at a specified epoch. */

/*        ZZGFILGQ   Returns the specified illumination angle */
/*                   at a specified epoch. */

/* $ Examples */

/*     See GFEVNT. */

/* $ Restrictions */

/*     1)  This is a SPICELIB private routine; it should not be called by */
/*         user applications. */

/*     2)  ZZGFILIN must be called prior to use of any of the other */
/*         entry points. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     E.D. Wright        (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.1, 30-MAY-2021 (NJB) (JDR) */

/*        Edited the umbrella routine and all its entry points' headers */
/*        to comply with NAIF standard. */

/*        Updated header of entry point ZZGFILIN: added missing */
/*        argument documentation. */

/* -    SPICELIB Version 1.1.0, 08-MAR-2017 (NJB) */

/*        Bug fixes: added FAILED checks in entry points ZZGFILIN, */
/*        ZZGFILDC, and ZZGFILGQ. */

/*        Removed unused declaration of parameter SUN. */


/* -    SPICELIB Version 1.0.0, 23-MAY-2012 (NJB) (EDW) */

/* -& */
/* $ Index_Entries */

/*     umbrella routine for finding illumination angle events. */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     Indices of illumination angles in the ANGLES */
/*     array: */


/*     Local Variables */


/*     Saved Variables */


/*     Initial values */

    /* Parameter adjustments */
    if (spoint) {
	}

    /* Function Body */
    switch(n__) {
	case 1: goto L_zzgfilin;
	case 2: goto L_zzgfildc;
	case 3: goto L_zzgfilgq;
	}


/*     This routine should never be called directly. */

    chkin_("ZZGFILU", (ftnlen)7);
    sigerr_("SPICE(BOGUSENTRY)", (ftnlen)17);
    chkout_("ZZGFILU", (ftnlen)7);
    return 0;
/* $Procedure ZZGFILIN ( GF, illumination angle utility initialization ) */

L_zzgfilin:
/* $ Abstract */

/*     Initialize the GF illumination angle constraint search utilities. */

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

/*     GF */
/*     NAIF_IDS */
/*     SPK */
/*     TIME */

/* $ Keywords */

/*     ANGLE */
/*     EPHEMERIS */
/*     GEOMETRY */
/*     SEARCH */

/* $ Declarations */

/*      CHARACTER*(*)         METHOD */
/*      CHARACTER*(*)         ANGTYP */
/*      CHARACTER*(*)         TARGET */
/*      CHARACTER*(*)         ILLUM */
/*      CHARACTER*(*)         FIXREF */
/*      CHARACTER*(*)         ABCORR */
/*      CHARACTER*(*)         OBSRVR */
/*      DOUBLE PRECISION      SPOINT ( 3 ) */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     METHOD     I   Computation method. */
/*     ANGTYP     I   Type of illumination angle. */
/*     TARGET     I   Target body name. */
/*     ILLUM      I   Illumination source name. */
/*     FIXREF     I   Body-fixed, body-centered target body frame. */
/*     ABCORR     I   Aberration correction specifier. */
/*     OBSRVR     I   Observer name. */
/*     SPOINT     I   Body-fixed coordinates of a target surface point. */

/* $ Detailed_Input */

/*     METHOD   is a short string providing parameters defining */
/*              the computation method to be used. Parameters */
/*              include, but are not limited to, the shape model */
/*              used to represent the surface of the target body. */

/*              The only choice currently supported is */

/*                 'Ellipsoid'        The illumination angle */
/*                                    computation uses a triaxial */
/*                                    ellipsoid to model the surface */
/*                                    of the target body. The */
/*                                    ellipsoid's radii must be */
/*                                    available in the kernel pool. */

/*              Neither case nor blanks are significant in METHOD. */
/*              For example, the string ' eLLipsoid ' is valid. */


/*     ANGTYP   is a string specifying the type of illumination */
/*              angle for which a search is to be performed. The */
/*              possible values of ANGTYP are */

/*                 'PHASE' */
/*                 'INCIDENCE' */
/*                 'EMISSION' */

/*              When the illumination source is the sun, the */
/*              incidence angle is commonly called the "solar */
/*              incidence angle." */

/*              See the $Particulars section below for a detailed */
/*              description of these angles. */

/*              Neither case nor white space are significant in */
/*              ANGTYP. For example, the string ' Incidence ' is */
/*              valid. */


/*     TARGET   is the name of a target body. Optionally, you may */
/*              supply the integer ID code for the object as */
/*              an integer string. For example both 'MOON' and */
/*              '301' are legitimate strings that indicate the */
/*              moon is the target body. */

/*              The target and observer define a position vector */
/*              which points from the observer to the target. */

/*              Case and leading or trailing blanks are not */
/*              significant in the string TARGET. */


/*     ILLUM    is the name of the illumination source. This source */
/*              may be any ephemeris object. Case, blanks, and */
/*              numeric values are treated in the same way as for the */
/*              input TARGET. */

/*     FIXREF   is the name of the body-fixed, body-centered */
/*              reference frame associated with the target body. The */
/*              input surface point SPOINT is expressed relative to */
/*              this reference frame, and this frame is used to */
/*              define the orientation of the target body as a */
/*              function of time. */

/*              The string FIXREF is case-insensitive, and leading */
/*              and trailing blanks in FIXREF are not significant. */

/*     ABCORR   indicates the aberration corrections to be applied */
/*              when computing the target's position and orientation. */
/*              Any value accepted by SPKEZR may be used. */

/*              See the header of the SPICE routine SPKEZR for a */
/*              detailed description of the aberration correction */
/*              options. */

/*              Case and embedded blanks are not significant in */
/*              ABCORR. */


/*     OBSRVR   is the name of the body from which the occultation is */
/*              observed. Optionally, you may supply the integer NAIF */
/*              ID code for the body as a string. */

/*              Case and leading or trailing blanks are not */
/*              significant in the string OBSRVR. */


/*     SPOINT   is a surface point on the target body, expressed in */
/*              Cartesian coordinates, relative to the body-fixed */
/*              target frame designated by FIXREF. */

/*              SPOINT need not be visible from the observer's */
/*              location in order for the constraint specified by */
/*              RELATE and REFVAL (see descriptions below) to be */
/*              satisfied. */

/*              The components of SPOINT have units of km. */

/* $ Detailed_Output */

/*     None. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If name of the target, observer, or illumination source */
/*         cannot be translated to a NAIF ID code, the error */
/*         SPICE(IDCODENOTFOUND) is signaled. */

/*     2)  If  target body coincides with the observer body OBSRVR or */
/*         the illumination source SOURCE, error, */
/*         the error SPICE(BODIESNOTDISTINCT) is signaled. */

/*     3)  If transmission-style aberration corrections are requested, */
/*         the error SPICE(NOTSUPPORTED) is signaled. */

/*     4)  If the aberration correction string is invalid, an error */
/*         is signaled by a routine in the call tree of this */
/*         routine. */

/*     5)  If the illumination angle type is not recognized, the error */
/*         SPICE(NOTSUPPORTED) is signaled. */

/*     6)  If the input target body-fixed frame FIXREF is not */
/*         recognized, the error SPICE(UNKNOWNFRAME) is signaled. A */
/*         frame name may fail to be recognized because a required frame */
/*         specification kernel has not been loaded; another cause is a */
/*         misspelling of the frame name. */

/*     7)  If the input frame FIXREF is not centered at the target body, */
/*         the error SPICE(INVALIDFRAME) is signaled. */

/*     8)  If the input argument METHOD is not recognized, the error */
/*         SPICE(INVALIDMETHOD) is signaled. */

/* $ Files */

/*     See the header of the umbrella routine ZZGFILU. */

/* $ Particulars */

/*     This routine must be called once before each GF search for */
/*     illumination angle events. */

/* $ Examples */

/*     See GFEVNT. */

/* $ Restrictions */

/*     1)  This is a SPICELIB private routine; it should not be called by */
/*         user applications. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     E.D. Wright        (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.1, 30-MAY-2021 (NJB) (JDR) */

/*        Edited the header to comply with NAIF standard. */

/*        Updated header: added missing argument documentation. */

/* -    SPICELIB Version 1.1.0, 08-MAR-2017 (NJB) */

/*        Bug fixes: now checks FAILED after each BODS2C call */
/*        and after the BODVRD call. */

/* -    SPICELIB Version 1.0.0, 23-MAY-2012 (NJB) (EDW) */

/* -& */
/* $ Index_Entries */

/*     illumination angle initialization routine */

/* -& */
    if (return_()) {
	return 0;
    }
    chkin_("ZZGFILIN", (ftnlen)8);

/*     Find NAIF IDs for TARGET, OBSRVR, and ILLUM. */

    bods2c_(target, &svtarg, &found, target_len);
    if (failed_()) {
	chkout_("ZZGFILIN", (ftnlen)8);
	return 0;
    }
    if (! found) {
	setmsg_("The target object, '#', is not a recognized name for an eph"
		"emeris object. The cause of this problem may be that you nee"
		"d an updated version of the SPICE Toolkit. ", (ftnlen)162);
	errch_("#", target, (ftnlen)1, target_len);
	sigerr_("SPICE(IDCODENOTFOUND)", (ftnlen)21);
	chkout_("ZZGFILIN", (ftnlen)8);
	return 0;
    }
    bods2c_(obsrvr, &svobs, &found, obsrvr_len);
    if (failed_()) {
	chkout_("ZZGFILIN", (ftnlen)8);
	return 0;
    }
    if (! found) {
	setmsg_("The observer, '#', is not a recognized name for an ephemeri"
		"s object. The cause of this problem may be that you need an "
		"updated version of the SPICE toolkit. ", (ftnlen)157);
	errch_("#", obsrvr, (ftnlen)1, obsrvr_len);
	sigerr_("SPICE(IDCODENOTFOUND)", (ftnlen)21);
	chkout_("ZZGFILIN", (ftnlen)8);
	return 0;
    }
    bods2c_(illum, &svilum, &found, illum_len);
    if (failed_()) {
	chkout_("ZZGFILIN", (ftnlen)8);
	return 0;
    }
    if (! found) {
	setmsg_("The illumination source, '#', is not a recognized name for "
		"an ephemeris object. The cause of this problem may be that y"
		"ou need an updated version of the SPICE toolkit. ", (ftnlen)
		168);
	errch_("#", illum, (ftnlen)1, illum_len);
	sigerr_("SPICE(IDCODENOTFOUND)", (ftnlen)21);
	chkout_("ZZGFILIN", (ftnlen)8);
	return 0;
    }

/*     Make sure the observer and target are distinct. */

    if (svtarg == svobs) {
	setmsg_("The observer and target must be distinct objects, but are n"
		"ot: OBSRVR = #; TARGET = #.", (ftnlen)86);
	errch_("#", obsrvr, (ftnlen)1, obsrvr_len);
	errch_("#", target, (ftnlen)1, target_len);
	sigerr_("SPICE(BODIESNOTDISTINCT)", (ftnlen)24);
	chkout_("ZZGFILIN", (ftnlen)8);
	return 0;
    }

/*     Make sure the target and illumination source are distinct. */

    if (svtarg == svilum) {
	setmsg_("The target and illumination source must be distinct objects"
		", but are not: TARGET = #; ILLUM = #.", (ftnlen)96);
	errch_("#", target, (ftnlen)1, target_len);
	errch_("#", illum, (ftnlen)1, illum_len);
	sigerr_("SPICE(BODIESNOTDISTINCT)", (ftnlen)24);
	chkout_("ZZGFILIN", (ftnlen)8);
	return 0;
    }

/*     Save the observer, target, and illumination source names. */

    s_copy(svonam, obsrvr, (ftnlen)36, obsrvr_len);
    s_copy(svtnam, target, (ftnlen)36, target_len);
    s_copy(svinam, illum, (ftnlen)36, illum_len);

/*     Squeeze all blanks out of the aberration correction */
/*     string; ensure the string is in upper case. */

    cmprss_(" ", &c__0, abcorr, svcorr, (ftnlen)1, abcorr_len, (ftnlen)5);
    ucase_(svcorr, svcorr, (ftnlen)5, (ftnlen)5);

/*     Check the aberration correction. If SPKEZR can't handle it, */
/*     neither can we. */

    zzvalcor_(svcorr, svablk, (ftnlen)5);
    if (failed_()) {
	chkout_("ZZGFILIN", (ftnlen)8);
	return 0;
    }

/*     Reject transmission corrections. */

    if (svablk[4]) {
	setmsg_("Aberration correction was #; transmission corrections are n"
		"ot allowed by this routine.", (ftnlen)86);
	errch_("#", abcorr, (ftnlen)1, abcorr_len);
	sigerr_("SPICE(NOTSUPPORTED)", (ftnlen)19);
	chkout_("ZZGFILIN", (ftnlen)8);
	return 0;
    }

/*     Look up the radii for the target body. */

    bodvrd_(target, "RADII", &c__3, &n, radii, target_len, (ftnlen)5);
    if (failed_()) {
	chkout_("ZZGFILIN", (ftnlen)8);
	return 0;
    }

/*     Find the surface normal at the surface point. Create a */
/*     body-fixed state vector for the normal. */

    surfnm_(radii, &radii[1], &radii[2], spoint, normal);
    if (failed_()) {
	chkout_("ZZGFILIN", (ftnlen)8);
	return 0;
    }
    vequ_(normal, svnrml);

/*     Save the surface point in the body-fixed reference frame. */

    vequ_(spoint, svsspt);

/*     Save a left-justified, upper case copy of the computation method */
/*     for the illumination angles. */

    ljust_(method, svmeth, method_len, (ftnlen)200);
    ucase_(svmeth, svmeth, (ftnlen)200, (ftnlen)200);
    if (s_cmp(svmeth, "ELLIPSOID", (ftnlen)200, (ftnlen)9) != 0) {
	setmsg_("The only supported computation method is ELLIPSOID; the inp"
		"ut method was #.", (ftnlen)75);
	errch_("#", method, (ftnlen)1, method_len);
	sigerr_("SPICE(INVALIDMETHOD)", (ftnlen)20);
	chkout_("ZZGFILIN", (ftnlen)8);
	return 0;
    }

/*     Save a left-justified, upper case copy of the reference frame */
/*     name. */

    ljust_(fixref, svref, fixref_len, (ftnlen)32);
    ucase_(svref, svref, (ftnlen)32, (ftnlen)32);

/*     Look up the frame attributes; make sure the frame is centered */
/*     on the target body. */


/*     Determine the attributes of the frame designated by FIXREF. */

    namfrm_(fixref, &fxfcde, fixref_len);
    frinfo_(&fxfcde, &fxcent, &fxclss, &fxtyid, &found);
    if (failed_()) {
	chkout_("ZZGFILIN", (ftnlen)8);
	return 0;
    }
    if (! found) {
	setmsg_("Reference frame # is not recognized by the SPICE frame subs"
		"ystem. Possibly a required frame definition kernel has not b"
		"een loaded.", (ftnlen)130);
	errch_("#", fixref, (ftnlen)1, fixref_len);
	sigerr_("SPICE(UNKNOWNFRAME)", (ftnlen)19);
	chkout_("ZZGFILIN", (ftnlen)8);
	return 0;
    }

/*     Make sure that FIXREF is centered at the target body's center. */

    if (fxcent != svtarg) {
	setmsg_("Reference frame # is not centered at the target body #. The"
		" ID code of the frame center is #.", (ftnlen)93);
	errch_("#", fixref, (ftnlen)1, fixref_len);
	errch_("#", target, (ftnlen)1, target_len);
	errint_("#", &fxcent, (ftnlen)1);
	sigerr_("SPICE(INVALIDFRAME)", (ftnlen)19);
	chkout_("ZZGFILIN", (ftnlen)8);
	return 0;
    }

/*     Save the index of the angle type. */

    svaidx = esrchc_(angtyp, &c__3, angnms, angtyp_len, (ftnlen)50);
    if (svaidx == 0) {
	setmsg_("Illumination angle type # is not recognized.", (ftnlen)44);
	errch_("#", angtyp, (ftnlen)1, angtyp_len);
	sigerr_("SPICE(NOTSUPPORTED)", (ftnlen)19);
	chkout_("ZZGFILIN", (ftnlen)8);
	return 0;
    }
    chkout_("ZZGFILIN", (ftnlen)8);
    return 0;
/* $Procedure ZZGFILDC ( GF, is illumination angle decreasing? ) */

L_zzgfildc:
/* $ Abstract */

/*     Indicate whether a specified illumination angle is decreasing at */
/*     a specified time. */

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

/*     GF */
/*     NAIF_IDS */
/*     SPK */
/*     TIME */

/* $ Keywords */

/*     ANGLE */
/*     EPHEMERIS */
/*     GEOMETRY */
/*     SEARCH */

/* $ Declarations */

/*     EXTERNAL              UDFUNC */
/*     DOUBLE PRECISION      ET */
/*     LOGICAL               DECRES */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     UDFUNC     I   Placeholder external routine argument. */
/*     ET         I   Ephemeris seconds past J2000 TDB. */
/*     DECRES     O   Flag indicating whether illumination angle is */
/*                    decreasing. */

/* $ Detailed_Input */

/*     UDFUNC   is a placeholder subroutine argument. This argument is */
/*              provided for compatibility with ZZGFSOLVX. It is not */
/*              used by the entry points of this package. */

/*     ET       is the time, expressed as seconds past J2000 TDB, at */
/*              which to determine whether or not the illumination */
/*              angle between the observer and target is decreasing. */

/* $ Detailed_Output */

/*     DECRES   is a logical flag that indicates whether the */
/*              observer-target illumination angle is decreasing at */
/*              ET. The observer, target, and aberration correction */
/*              used to compute the illumination angle are defined by */
/*              the latest call to the initialization entry point */
/*              ZZGFILIN. */

/*              DECRES is .TRUE. if and only if the observer-target */
/*              illumination angle is decreasing at ET. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the state of the target relative to the observer at ET can */
/*         not be found due to an SPK lookup failure, an error is */
/*         signaled by a routine in the call tree of this routine. */

/* $ Files */

/*     See the header of the umbrella routine ZZGFILU. */

/* $ Particulars */

/*     This routine is used by ZZGFRELX, and indirectly by GFILUM, to */
/*     determine the time intervals, within the confinement window, on */
/*     which the observer-target illumination angle is monotone */
/*     increasing or monotone decreasing. */

/* $ Examples */

/*     See GFILUM, GFEVNT, ZZGFRELX. */

/* $ Restrictions */

/*     1)  This is a SPICELIB private routine; it should not be called by */
/*         user applications. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     E.D. Wright        (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.1, 29-MAY-2021 (JDR) */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 1.1.0, 08-MAR-2017 (NJB) */

/*        Bug fix: now checks FAILED after ZZILUSTA call. */

/* -    SPICELIB Version 1.0.0, 23-MAY-2012 (NJB) (EDW) */

/* -& */
/* $ Index_Entries */

/*     indicate whether illumination angle is decreasing */

/* -& */

/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }
    chkin_("ZZGFILDC", (ftnlen)8);

/*     Compute the rates of change of all of the illumination angles. */

    zzilusta_(svmeth, svtnam, svinam, et, svref, svcorr, svonam, svsspt, 
	    svnrml, phssta, incsta, emista, (ftnlen)200, (ftnlen)36, (ftnlen)
	    36, (ftnlen)32, (ftnlen)5, (ftnlen)36);
    if (failed_()) {
	chkout_("ZZGFILDC", (ftnlen)8);
	return 0;
    }
    if (svaidx == 1) {
	rate = phssta[1];
    } else if (svaidx == 2) {
	rate = incsta[1];
    } else if (svaidx == 3) {
	rate = emista[1];
    } else {

/*        We should never get here. */

	setmsg_("Unexpected value of SVAIDX: #.", (ftnlen)30);
	errint_("#", &svaidx, (ftnlen)1);
	sigerr_("SPICE(BUG)", (ftnlen)10);
    }

/*     The observer-target illumination angle is decreasing if and only */
/*     the derivative of the angle with respect to time is negative. */

    *decres = rate < 0.;
    chkout_("ZZGFILDC", (ftnlen)8);
    return 0;
/* $Procedure ZZGFILGQ ( GF, get illumination angle ) */

L_zzgfilgq:
/* $ Abstract */

/*     Return the specified illumination angle at a specified epoch. */

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

/*     GF */
/*     NAIF_IDS */
/*     SPK */
/*     TIME */

/* $ Keywords */

/*     EPHEMERIS */
/*     GEOMETRY */
/*     ILLUMINATION ANGLE */
/*     SEARCH */

/* $ Declarations */

/*      DOUBLE PRECISION      ET */
/*      DOUBLE PRECISION      ANGLE */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     ET         I   Ephemeris seconds past J2000 TDB. */
/*     ANGLE      O   Illumination angle at time ET. */

/* $ Detailed_Input */

/*     ET       is the time, expressed as seconds past J2000 TDB, at */
/*              which the illumination angle is to be computed. */

/* $ Detailed_Output */

/*     ANGLE    is the illumination angle as seen by the observer at */
/*              time ET. The observer, target, and aberration */
/*              correction used to compute the illumination angle are */
/*              defined by the latest call to the initialization entry */
/*              point ZZGFILIN. */

/*              Units are radians. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the position of the target relative to the observer at ET */
/*         can not be found due to an SPK lookup failure, an error is */
/*         signaled by a routine in the call tree of this routine. */

/* $ Files */

/*     See the header of the umbrella routine ZZGFILU. */

/* $ Particulars */

/*     This routine determines the apparent illumination angle as seen */
/*     from the observer at time ET. This functionality supports GFREL's */
/*     comparisons of relative extrema in order to determine absolute */
/*     extrema. */

/* $ Examples */

/*     See GFREL. */

/* $ Restrictions */

/*     1)  This is a SPICELIB private routine; it should not be called by */
/*         user applications. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     E.D. Wright        (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.1, 29-MAY-2021 (JDR) */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 1.1.0, 08-MAR-2017 (NJB) */

/*        Bug fix: now checks FAILED after ILLUMG call. */

/* -    SPICELIB Version 1.0.0, 23-MAY-2012 (NJB) (EDW) */

/* -& */
/* $ Index_Entries */

/*     return illumination angle */

/* -& */
    if (return_()) {
	return 0;
    }
    chkin_("ZZGFILGQ", (ftnlen)8);
    illumg_(svmeth, svtnam, svinam, et, svref, svcorr, svonam, svsspt, &
	    ettarg, srfvec, angles, &angles[1], &angles[2], (ftnlen)200, (
	    ftnlen)36, (ftnlen)36, (ftnlen)32, (ftnlen)5, (ftnlen)36);
    if (failed_()) {
	chkout_("ZZGFILGQ", (ftnlen)8);
	return 0;
    }
    *angle = angles[(i__1 = svaidx - 1) < 3 && 0 <= i__1 ? i__1 : s_rnge(
	    "angles", i__1, "zzgfilu_", (ftnlen)1191)];
    chkout_("ZZGFILGQ", (ftnlen)8);
    return 0;
} /* zzgfilu_ */

/* Subroutine */ int zzgfilu_(char *method, char *angtyp, char *target, char *
	illum, char *fixref, char *abcorr, char *obsrvr, doublereal *spoint, 
	doublereal *et, U_fp udfunc, logical *decres, doublereal *angle, 
	ftnlen method_len, ftnlen angtyp_len, ftnlen target_len, ftnlen 
	illum_len, ftnlen fixref_len, ftnlen abcorr_len, ftnlen obsrvr_len)
{
    return zzgfilu_0_(0, method, angtyp, target, illum, fixref, abcorr, 
	    obsrvr, spoint, et, udfunc, decres, angle, method_len, angtyp_len,
	     target_len, illum_len, fixref_len, abcorr_len, obsrvr_len);
    }

/* Subroutine */ int zzgfilin_(char *method, char *angtyp, char *target, char 
	*illum, char *fixref, char *abcorr, char *obsrvr, doublereal *spoint, 
	ftnlen method_len, ftnlen angtyp_len, ftnlen target_len, ftnlen 
	illum_len, ftnlen fixref_len, ftnlen abcorr_len, ftnlen obsrvr_len)
{
    return zzgfilu_0_(1, method, angtyp, target, illum, fixref, abcorr, 
	    obsrvr, spoint, (doublereal *)0, (U_fp)0, (logical *)0, (
	    doublereal *)0, method_len, angtyp_len, target_len, illum_len, 
	    fixref_len, abcorr_len, obsrvr_len);
    }

/* Subroutine */ int zzgfildc_(U_fp udfunc, doublereal *et, logical *decres)
{
    return zzgfilu_0_(2, (char *)0, (char *)0, (char *)0, (char *)0, (char *)
	    0, (char *)0, (char *)0, (doublereal *)0, et, udfunc, decres, (
	    doublereal *)0, (ftnint)0, (ftnint)0, (ftnint)0, (ftnint)0, (
	    ftnint)0, (ftnint)0, (ftnint)0);
    }

/* Subroutine */ int zzgfilgq_(doublereal *et, doublereal *angle)
{
    return zzgfilu_0_(3, (char *)0, (char *)0, (char *)0, (char *)0, (char *)
	    0, (char *)0, (char *)0, (doublereal *)0, et, (U_fp)0, (logical *)
	    0, angle, (ftnint)0, (ftnint)0, (ftnint)0, (ftnint)0, (ftnint)0, (
	    ftnint)0, (ftnint)0);
    }

