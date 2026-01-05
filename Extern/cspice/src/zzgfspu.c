/* zzgfspu.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__0 = 0;
static integer c__2 = 2;

/* $Procedure ZZGFSPU ( Private - GF, angular separation routines ) */
/* Subroutine */ int zzgfspu_0_(int n__, char *of, char *from, char *shape, 
	char *frame, doublereal *et, U_fp udfunc, char *abcorr, logical *
	decres, doublereal *sep, char *xabcr, integer *xbod, char *yref, char 
	*xref, integer *xobs, doublereal *xrad, integer *xshp, ftnlen of_len, 
	ftnlen from_len, ftnlen shape_len, ftnlen frame_len, ftnlen 
	abcorr_len, ftnlen xabcr_len, ftnlen yref_len, ftnlen xref_len)
{
    /* Initialized data */

    static char svshap[32*2] = "POINT                           " "SPHERE   "
	    "                       ";
    static char ref[5] = "J2000";

    /* System generated locals */
    doublereal d__1;

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    extern doublereal dhfa_(doublereal *, doublereal *);
    extern /* Subroutine */ int zzgftreb_(integer *, doublereal *);
    doublereal axes1[3], axes2[3];
    extern /* Subroutine */ int zzvalcor_(char *, logical *, ftnlen), chkin_(
	    char *, ftnlen), ucase_(char *, char *, ftnlen, ftnlen), errch_(
	    char *, char *, ftnlen, ftnlen);
    integer class__;
    logical found;
    extern doublereal dvsep_(doublereal *, doublereal *);
    static char svref[32];
    static integer svobs;
    extern /* Subroutine */ int spkez_(integer *, doublereal *, char *, char *
	    , integer *, doublereal *, doublereal *, ftnlen, ftnlen);
    integer fcode1, fcode2;
    extern /* Subroutine */ int ljust_(char *, char *, ftnlen, ftnlen), 
	    bods2c_(char *, integer *, logical *, ftnlen);
    static integer svbod1, svbod2;
    static doublereal svrad1, svrad2;
    static char svref1[32], svref2[32];
    extern logical failed_(void);
    static integer svshp1, svshp2;
    doublereal lt, dtheta;
    extern integer isrchc_(char *, integer *, char *, ftnlen, ftnlen);
    static char svabcr[32];
    logical attblk[15];
    integer clssid;
    extern /* Subroutine */ int namfrm_(char *, integer *, ftnlen), frinfo_(
	    integer *, integer *, integer *, integer *, logical *), sigerr_(
	    char *, ftnlen), chkout_(char *, ftnlen), setmsg_(char *, ftnlen),
	     errint_(char *, integer *, ftnlen), cmprss_(char *, integer *, 
	    char *, char *, ftnlen, ftnlen, ftnlen);
    extern logical return_(void);
    doublereal pv1[6], pv2[6];
    extern doublereal zzsepq_(doublereal *, integer *, integer *, doublereal *
	    , doublereal *, integer *, char *, char *, ftnlen, ftnlen);
    integer ctr1, ctr2;

/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines. Users should not call this routine directly due */
/*     to the volatile nature of this routine. */

/*     This is the umbrella routine for the entry points needed by */
/*     GFEVNT in order to find angular separation events. */

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

/*     ANGLE */
/*     GEOMETRY */
/*     ROOT */

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
/*     OF         I   ZZGFSPIN */
/*     FROM       I   ZZGFSPIN */
/*     SHAPE      I   ZZGFSPIN */
/*     FRAME      I   ZZGFSPIN */
/*     ET         I   ZZGFSPDC, ZZGFSPGQ */
/*     UDFUNC     I   ZZGFSPDC */
/*     ABCORR     I   ZZGFSPIN */
/*     DECRES     O   ZZGFSPDC */
/*     SEP        O   ZZGFSPGQ */
/*     XABCR      O   ZZGFSPX */
/*     XBOD       O   ZZGFSPX */
/*     YREF       O   ZZGFSPX */
/*     XREF       O   ZZGFSPX */
/*     XOBS       O   ZZGFSPX */
/*     XRAD       O   ZZGFSPX */
/*     XSHP       O   ZZGFSPX */

/* $ Detailed_Input */

/*     OF       the string array naming the bodies whose angular */
/*              separation is of interest. */

/*     FROM     the string naming the observer. */

/*     SHAPE    the string array naming the geometric model used to */
/*              represent the shapes of OF. The relation between SHAPE */
/*              and OF is 1-to-1. */

/*              Models supported by this routine: */

/*                 'SPHERE'        Treat the body as a sphere with */
/*                                 radius equal to the maximum value of */
/*                                 BODYnnn_RADII */

/*                 'POINT'         Treat the body as a single point; */
/*                                 radius has value zero. */

/*              The SHAPE string lacks sensitivity to case and leading */
/*              or trailing blank. */

/*     FRAME    the string array naming the body-fixed reference frames */
/*              corresponding to OF. The relation between FRAME */
/*              and OF is 1-to-1. */

/*     ET       is the time in second past J2000 at which one wants */
/*              to determine an event condition. */

/*     ABCORR   the string description of the aberration corrections */
/*              to apply to the state evaluations to account for */
/*              one-way light time and stellar aberration. */

/*              This routine accepts the same aberration corrections */
/*              as does the SPICE routine SPKEZR. See the header of */
/*              SPKEZR for a detailed description of the aberration */
/*              correction options. For convenience, the options are */
/*              listed below: */

/*                 'NONE'     Apply no correction. */

/*                 'LT'       "Reception" case: correct for */
/*                            one-way light time using a Newtonian */
/*                            formulation. */

/*                 'LT+S'     "Reception" case: correct for */
/*                            one-way light time and stellar */
/*                            aberration using a Newtonian */
/*                            formulation. */

/*                 'CN'       "Reception" case: converged */
/*                            Newtonian light time correction. */

/*                 'CN+S'     "Reception" case: converged */
/*                            Newtonian light time and stellar */
/*                            aberration corrections. */

/*                 'XLT'      "Transmission" case: correct for */
/*                            one-way light time using a Newtonian */
/*                            formulation. */

/*                 'XLT+S'    "Transmission" case: correct for */
/*                            one-way light time and stellar */
/*                            aberration using a Newtonian */
/*                            formulation. */

/*                 'XCN'      "Transmission" case: converged */
/*                            Newtonian light time correction. */

/*                 'XCN+S'    "Transmission" case: converged */
/*                            Newtonian light time and stellar */
/*                            aberration corrections. */

/*                 The ABCORR string lacks sensitivity to case, leading */
/*                 and trailing blanks. */

/*     DECRES   is .TRUE. if the angular separation between the */
/*              objects is decreasing. Otherwise it is .FALSE. */

/*     SEP      is the angular separation between SVBOD1 and SVBOD2 as */
/*              seen from SVOBS at time ET. */

/*     For more information, see individual entry points. */

/* $ Detailed_Output */

/*     See individual entry points. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If a direct call to ZZGFSPU occurs, the error */
/*         SPICE(BOGUSENTRY) is signaled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine serves as the umbrella routine for 4 entry points */
/*     needed by GFEVNT in solving for angular separation conditions. */

/*     The five entry points are */

/*        ZZGFSPIN  --- an initialization routine that must be called */
/*                     prior to attempting to solve for any angular */
/*                     separation event. */

/*        ZZGFSPDC --- determines whether or not angular separation is */
/*                     decreasing at some time. */

/*        ZZGFSPGQ --- returns the angular separation of the two */
/*                     objects of interest as a function of ET. */

/* $ Examples */

/*     None. */

/* $ Restrictions */

/*     1)  ZZGFSPIN must be called prior to use of any of the */
/*         other entry points. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     E.D. Wright        (JPL) */

/* $ Version */

/* -    SPICELIB Version 3.0.0, 31-MAY-2021 (EDW) (JDR) */

/*        Edited umbrella routine and all its entry points' headers to */
/*        comply with NAIF standard. Extended $Brief_I/O to list all */
/*        arguments and refer to corresponding entry points. */

/*        Moved UDFUNC declaration to $Declarations section. */

/*        Routine ZZGFSPGQ now calls ZZSEPQ, a copy of ZZGFSPQ cast as */
/*        a function. */

/* -    SPICELIB Version 2.0.0, 27-JUN-2012 (EDW) */

/*        Code edits to implement use of ZZGFRELX. */
/*        These edits include removal of unneeded routines: */

/*           ZZGFSPUR */
/*           ZZGFSPLT */

/*        and corresponding unused variables. */

/*        Routine ZZGFGSEP renamed to ZZGFSPGQ to match geometry finder */
/*        naming convention. */

/*        Implemented a proper $Exceptions section. Update to header */
/*        entries. */

/* -    SPICELIB Version 1.1.0, 29-DEC-2009 (NJB) (EDW) */

/*        Edited argument descriptions. Removed mention of "ELLIPSOID" */
/*        shape from SHAPE as that option is not yet implemented. */

/*        Added an error check on body frame centers to enforce */
/*        a body frame center is the body. This check does not apply */
/*        to "POINT" or "SPHERE" shape targets, and so will not */
/*        execute for this version of the routine. */

/*        Rename of the ZZDHA call to DHFA. */

/* -    SPICELIB Version 1.0.0, 19-FEB-2009 (NJB) (EDW) */

/* -& */
/* $ Index_Entries */

/*     umbrella routine for finding angular separation events */

/* -& */

/*     SPICELIB functions */


/*     Local Variables */


/*     Saved Variables */


/*     Below we initialize the list of shape names. */


/*     Define integer ID parameters for the shape names in */
/*     SVSHAP. */

    /* Parameter adjustments */
    if (of) {
	}
    if (shape) {
	}
    if (frame) {
	}
    if (xbod) {
	}
    if (xref) {
	}
    if (xrad) {
	}
    if (xshp) {
	}

    /* Function Body */
    switch(n__) {
	case 1: goto L_zzgfspin;
	case 2: goto L_zzgfspdc;
	case 3: goto L_zzgfspgq;
	case 4: goto L_zzgfspx;
	}


/*     Never directly call this routine. */

    chkin_("ZZGFSPU", (ftnlen)7);
    sigerr_("SPICE(BOGUSENTRY)", (ftnlen)17);
    chkout_("ZZGFSPU", (ftnlen)7);
    return 0;
/* $Procedure ZZGFSPIN ( Private - GF, angular separation initialization ) */

L_zzgfspin:
/* $ Abstract */

/*     This routine initializes variables that describe an angular */
/*     separation event of interest for solution by ZZGFSOLV. */

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

/*     ANGLE */
/*     GEOMETRY */
/*     ROOT */

/* $ Declarations */

/*      CHARACTER*(*)         OF   ( 2 ) */
/*      CHARACTER*(*)         FROM */
/*      CHARACTER*(*)         SHAPE( 2 ) */
/*      CHARACTER*(*)         FRAME( 2 ) */
/*      CHARACTER*(*)         ABCORR */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     OF         I   Body id's of the angular separation objects */
/*     FROM       I   Observer name */
/*     SHAPE      I   Array of shape IDs corresponding to OF */
/*     FRAME      I   Array of frame names corresponding to OF */
/*     ABCORR     I   Aberration correction flag. */

/* $ Detailed_Input */

/*     OF       the string array naming the bodies whose angular */
/*              separation is of interest. */

/*     FROM     the string naming the observer. */

/*     SHAPE    the string array naming the geometric model used to */
/*              represent the shapes of OF. The relation between SHAPE */
/*              and OF is 1-to-1 and onto. */

/*              Models supported by this routine: */

/*                 'SPHERE'        Treat the body as a sphere with */
/*                                 radius equal to the maximum value of */
/*                                 BODYnnn_RADII */

/*                 'POINT'         Treat the body as a single point; */
/*                                 radius has value zero. */

/*              The SHAPE string lacks sensitivity to case and leading */
/*              or trailing blank. */

/*     FRAME    the string array naming the body-fixed reference frames */
/*              corresponding to OF. The relation between FRAME */
/*              and OF is 1-to-1. */

/*     ABCORR   the string description of the aberration corrections */
/*              to apply to the state evaluations to account for */
/*              one-way light time and stellar aberration. */

/*              This routine accepts the same aberration corrections */
/*              as does the SPICE routine SPKEZR. See the header of */
/*              SPKEZR for a detailed description of the aberration */
/*              correction options. For convenience, the options are */
/*              listed below: */

/*                 'NONE'     Apply no correction. */

/*                 'LT'       "Reception" case: correct for */
/*                            one-way light time using a Newtonian */
/*                            formulation. */

/*                 'LT+S'     "Reception" case: correct for */
/*                            one-way light time and stellar */
/*                            aberration using a Newtonian */
/*                            formulation. */

/*                 'CN'       "Reception" case: converged */
/*                            Newtonian light time correction. */

/*                 'CN+S'     "Reception" case: converged */
/*                            Newtonian light time and stellar */
/*                            aberration corrections. */

/*                 'XLT'      "Transmission" case: correct for */
/*                            one-way light time using a Newtonian */
/*                            formulation. */

/*                 'XLT+S'    "Transmission" case: correct for */
/*                            one-way light time and stellar */
/*                            aberration using a Newtonian */
/*                            formulation. */

/*                 'XCN'      "Transmission" case: converged */
/*                            Newtonian light time correction. */

/*                 'XCN+S'    "Transmission" case: converged */
/*                            Newtonian light time and stellar */
/*                            aberration corrections. */

/*                 The ABCORR string lacks sensitivity to case, leading */
/*                 and trailing blanks. */

/* $ Detailed_Output */

/*     None. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the object name for target 1, OF(1), is not a recognized */
/*         name, the error SPICE(IDCODENOTFOUND) is signaled. */

/*     2)  If the object name for target 2, OF(2), is not a recognized */
/*         name, the error SPICE(IDCODENOTFOUND) is signaled. */

/*     3)  If the object name for the observer, FROM, is not a recognized */
/*         name, the error SPICE(IDCODENOTFOUND) is signaled. */

/*     4)  If the three objects associated with an ANGULAR SEPARATION */
/*         search are not distinct, the error SPICE(BODIESNOTDISTINCT) is */
/*         signaled. */

/*     5)  If the body shape for target 1, SHAPE(1), is not recognized, */
/*         the error SPICE(NOTRECOGNIZED) is signaled. */

/*     6)  If the SHAPE(1) value lacks a corresponding case block, the */
/*         error SPICE(BUG) is signaled. This indicates a programming */
/*         error. */

/*     7)  If the body shape for target 2, SHAPE(2), is not recognized, */
/*         the error SPICE(NOTRECOGNIZED) is signaled. */

/*     8)  If the SHAPE(2) value lacks a corresponding case block, the */
/*         error SPICE(BUG) is signaled. This indicates a programming */
/*         error. */

/*     9)  If frame subsystem did not recognize frame name FRAME(1), the */
/*         error SPICE(NOFRAME) is signaled. */

/*     10) If the reference frame associated with target body 1, OF(1), */
/*         is not centered on target body 1, the error */
/*         SPICE(INVALIDFRAME) is signaled. */

/*     11) If frame subsystem did not recognize frame name FRAME(2), the */
/*         error SPICE(NOFRAME) is signaled. */

/*     12) If the reference frame associated with target body 2, OF(2), */
/*         is not centered on target body 2, the error */
/*         SPICE(INVALIDFRAME) is signaled. */

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

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     E.D. Wright        (JPL) */

/* $ Version */

/* -    SPICELIB Version 2.0.1, 31-MAY-2021 (JDR) */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 2.0.0, 27-JUN-2012 (EDW) */

/*        REFVAL removed from routine argument list due to the use */
/*        of ZZGFRELX to calculate the events. */

/*        Implemented a proper $Exceptions section. Update to */
/*        $Author_and_Institution section. */

/* -    SPICELIB Version 1.1.0, 29-DEC-2009 (NJB) (EDW) */

/*        Edited argument descriptions. Removed mention of "ELLIPSOID" */
/*        shape from SHAPE as that option is not yet implemented. */

/*        Added an error check on body frame centers to enforce */
/*        a body frame center is the body. This check does not apply */
/*        to "POINT" or "SPHERE" shape targets, and so will not */
/*        execute for this version of the routine. */

/* -    SPICELIB Version 1.0.0, 14-APR-2008 (NJB) (EDW) */

/* -& */
/* $ Index_Entries */

/*     angular separation initialization routine */

/* -& */
    if (return_()) {
	return 0;
    } else {
	chkin_("ZZGFSPIN", (ftnlen)8);
    }
    bods2c_(of, &svbod1, &found, of_len);
    if (! found) {
	setmsg_("The object name for target 1, '#', is not a recognized name"
		" for an ephemeris object. The cause of this problem may be t"
		"hat you need an updated version of the SPICE Toolkit.", (
		ftnlen)172);
	errch_("#", of, (ftnlen)1, of_len);
	sigerr_("SPICE(IDCODENOTFOUND)", (ftnlen)21);
	chkout_("ZZGFSPIN", (ftnlen)8);
	return 0;
    }
    bods2c_(of + of_len, &svbod2, &found, of_len);
    if (! found) {
	setmsg_("The object name for target 2, '#', is not a recognized name"
		" for an ephemeris object. The cause of this problem may be t"
		"hat you need an updated version of the SPICE Toolkit.", (
		ftnlen)172);
	errch_("#", of + of_len, (ftnlen)1, of_len);
	sigerr_("SPICE(IDCODENOTFOUND)", (ftnlen)21);
	chkout_("ZZGFSPIN", (ftnlen)8);
	return 0;
    }
    bods2c_(from, &svobs, &found, from_len);
    if (! found) {
	setmsg_("The object name for the observer, '#', is not a recognized "
		"name for an ephemeris object. The cause of this problem may "
		"be that you need an updated version of the SPICE Toolkit.", (
		ftnlen)176);
	errch_("#", from, (ftnlen)1, from_len);
	sigerr_("SPICE(IDCODENOTFOUND)", (ftnlen)21);
	chkout_("ZZGFSPIN", (ftnlen)8);
	return 0;
    }

/*     Confirm the three bodies have unique IDs. */

    if (svobs == svbod1 || svobs == svbod2 || svbod1 == svbod2) {
	setmsg_("All three objects associated with an ANGULAR SEPARATION sea"
		"rch must be distinct. The objects whose angular separation i"
		"s of interest were # and #. The observer was #.", (ftnlen)166)
		;
	errint_("#", &svbod1, (ftnlen)1);
	errint_("#", &svbod2, (ftnlen)1);
	errint_("#", &svobs, (ftnlen)1);
	sigerr_("SPICE(BODIESNOTDISTINCT)", (ftnlen)24);
	chkout_("ZZGFSPIN", (ftnlen)8);
	return 0;
    }

/*     Squeeze all blanks out of the aberration correction */
/*     string; ensure the string is in upper case. */

    cmprss_(" ", &c__0, abcorr, svabcr, (ftnlen)1, abcorr_len, (ftnlen)32);
    ucase_(svabcr, svabcr, (ftnlen)32, (ftnlen)32);

/*     Check the aberration correction. If SPKEZR can't handle it, */
/*     neither can we. */

    zzvalcor_(svabcr, attblk, (ftnlen)32);
    if (failed_()) {
	chkout_("ZZGFSPIN", (ftnlen)8);
	return 0;
    }
    s_copy(svref, ref, (ftnlen)32, (ftnlen)5);
    s_copy(svref1, frame, (ftnlen)32, frame_len);
    s_copy(svref2, frame + frame_len, (ftnlen)32, frame_len);

/*     Check shapes... */

    ljust_(shape, shape, shape_len, shape_len);
    ucase_(shape, shape, shape_len, shape_len);

/*     If we pass the error check, then SHAPE(1) exists in SVSHAP. */

    svshp1 = isrchc_(shape, &c__2, svshap, shape_len, (ftnlen)32);
    if (svshp1 == 0) {
	setmsg_("The body shape, # is not recognized.  Supported quantities "
		"are: POINT, SPHERE.", (ftnlen)78);
	errch_("#", shape, (ftnlen)1, shape_len);
	sigerr_("SPICE(NOTRECOGNIZED)", (ftnlen)20);
	chkout_("ZZGFSPIN", (ftnlen)8);
	return 0;
    } else if (svshp1 == 1) {
	svrad1 = 0.;
    } else if (svshp1 == 2) {
	zzgftreb_(&svbod1, axes1);
	if (failed_()) {
	    chkout_("ZZGFSPIN", (ftnlen)8);
	    return 0;
	}
/* Computing MAX */
	d__1 = max(axes1[0],axes1[1]);
	svrad1 = max(d__1,axes1[2]);
    } else {

/*        This code executes only if someone adds a new shape */
/*        name to SVSHAP then fails to update the SVSHP1 condition */
/*        block to respond to the name. Fortran needs SWITCH...CASE. */

	setmsg_("Encountered uncoded shape ID for #. This indicates a bug. P"
		"lease contact NAIF.", (ftnlen)78);
	errch_("#", shape, (ftnlen)1, shape_len);
	sigerr_("SPICE(BUG)", (ftnlen)10);
	chkout_("ZZGFSPIN", (ftnlen)8);
	return 0;
    }
    ljust_(shape + shape_len, shape + shape_len, shape_len, shape_len);
    ucase_(shape + shape_len, shape + shape_len, shape_len, shape_len);

/*     If we pass the error check, then SHAPE(2) exists in SVSHAP. */

    svshp2 = isrchc_(shape + shape_len, &c__2, svshap, shape_len, (ftnlen)32);
    if (svshp2 == 0) {
	setmsg_("The body shape, # is not recognized.  Supported quantities "
		"are: POINT, SPHERE.", (ftnlen)78);
	errch_("#", shape + shape_len, (ftnlen)1, shape_len);
	sigerr_("SPICE(NOTRECOGNIZED)", (ftnlen)20);
	chkout_("ZZGFSPIN", (ftnlen)8);
	return 0;
    } else if (svshp2 == 1) {
	svrad2 = 0.;
    } else if (svshp2 == 2) {
	zzgftreb_(&svbod2, axes2);
	if (failed_()) {
	    chkout_("ZZGFSPIN", (ftnlen)8);
	    return 0;
	}
/* Computing MAX */
	d__1 = max(axes2[0],axes2[1]);
	svrad2 = max(d__1,axes2[2]);
    } else {

/*        This code executes only if someone adds a new shape */
/*        name to SVSHAP then fails to update the SVSHP2 condition */
/*        block to respond to the name. Fortran needs SWITCH...CASE. */

	setmsg_("Encountered uncoded shape ID for #. This indicates a bug. P"
		"lease contact NAIF.", (ftnlen)78);
	errch_("#", shape + shape_len, (ftnlen)1, shape_len);
	sigerr_("SPICE(BUG)", (ftnlen)10);
	chkout_("ZZGFSPIN", (ftnlen)8);
	return 0;
    }

/*     Confirm the center of the input reference frames correspond */
/*     to the target bodies for non-point, non-spherical bodies. */

/*        FRAME1 centered on TARG1 */
/*        FRAME2 centered on TARG2 */

/*     This check does not apply to POINT or SPHERE shapes. */

    if (svshp1 != 1 && svshp1 != 2) {
	namfrm_(svref1, &fcode1, (ftnlen)32);
	frinfo_(&fcode1, &ctr1, &class__, &clssid, &found);
	if (! found) {
	    setmsg_("Frame system did not recognize frame #.", (ftnlen)39);
	    errch_("#", svref1, (ftnlen)1, (ftnlen)32);
	    sigerr_("SPICE(NOFRAME)", (ftnlen)14);
	    chkout_("ZZGFSPIN", (ftnlen)8);
	    return 0;
	}
	if (svbod1 != ctr1) {
	    setmsg_("The reference frame #1 associated with target body #2 i"
		    "s not centered on #2. The frame must be centered on the "
		    "target body.", (ftnlen)123);
	    errch_("#1", svref1, (ftnlen)2, (ftnlen)32);
	    errch_("#2", of, (ftnlen)2, of_len);
	    sigerr_("SPICE(INVALIDFRAME)", (ftnlen)19);
	    chkout_("ZZGFSPIN", (ftnlen)8);
	    return 0;
	}
    }
    if (svshp2 != 1 && svshp2 != 2) {
	namfrm_(svref2, &fcode2, (ftnlen)32);
	frinfo_(&fcode2, &ctr2, &class__, &clssid, &found);
	if (! found) {
	    setmsg_("Frame system did not recognize frame #.", (ftnlen)39);
	    errch_("#", svref2, (ftnlen)1, (ftnlen)32);
	    sigerr_("SPICE(NOFRAME)", (ftnlen)14);
	    chkout_("ZZGFSPIN", (ftnlen)8);
	    return 0;
	}
	if (svbod2 != ctr2) {
	    setmsg_("The reference frame #1 associated with target body #2 i"
		    "s not centered on #2. The frame must be centered on the "
		    "target body.", (ftnlen)123);
	    errch_("#1", svref2, (ftnlen)2, (ftnlen)32);
	    errch_("#2", of + of_len, (ftnlen)2, of_len);
	    sigerr_("SPICE(INVALIDFRAME)", (ftnlen)19);
	    chkout_("ZZGFSPIN", (ftnlen)8);
	    return 0;
	}
    }
    chkout_("ZZGFSPIN", (ftnlen)8);
    return 0;
/* $Procedure ZZGFSPDC ( Private - GF, angular separation decreasing) */

L_zzgfspdc:
/* $ Abstract */

/*     Computes whether or not the angular separation between SVBOD1 and */
/*     SVBOD2 is decreasing at time ET. */

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

/*     ANGLE */
/*     GEOMETRY */
/*     ROOT */

/* $ Declarations */

/*     DOUBLE PRECISION      ET */
/*     LOGICAL               DECRES */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     ET         I   Ephemeris seconds past J2000 TDB. */
/*     DECRES     O   .TRUE. if angular separation is decreasing .FALSE. */
/*                    otherwise. */

/* $ Detailed_Input */

/*     ET       time in seconds past J2000 at which one wishes to */
/*              determine whether or not the angular separation of the */
/*              two bodies is decreasing. */

/* $ Detailed_Output */

/*     DECRES   is .TRUE. if the angular separation between the objects */
/*              is decreasing. Otherwise it is .FALSE. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the observer is inside one of the objects, the object will */
/*         be regarded as having a 90 degree apparent radius. */

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

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     E.D. Wright        (JPL) */

/* $ Version */

/* -    SPICELIB Version 2.0.1, 30-MAY-2021 (JDR) */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 2.0.0, 18-FEB-2011 (EDW) */

/*        Added UDFUNC to argument list for use of ZZGFRELX when */
/*        calculating the events. */

/* -    SPICELIB Version 1.0.1, 06-JUL-2009 (NJB) (EDW) */

/*        Rename of the ZZDHA call to DHFA. */

/* -    SPICELIB Version 1.0.0, 29-APR-2008 (NJB) */

/* -& */
/* $ Index_Entries */

/*     angular separation is decreasing */

/* -& */

/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("ZZGFSPDC", (ftnlen)8);
    }
    spkez_(&svbod1, et, svref, svabcr, &svobs, pv1, &lt, (ftnlen)32, (ftnlen)
	    32);
    if (failed_()) {
	chkout_("ZZGFSPDC", (ftnlen)8);
	return 0;
    }
    spkez_(&svbod2, et, svref, svabcr, &svobs, pv2, &lt, (ftnlen)32, (ftnlen)
	    32);
    if (failed_()) {
	chkout_("ZZGFSPDC", (ftnlen)8);
	return 0;
    }

/*     The angular separation between the bodies has the value */

/*        theta = sep - alpha1 - alpha2 */

/*     With alpha1 the half angle of SVBOD1, alpha2 the half */
/*     angle of SVBOD2, half angle defined as (for spheres): */

/*        sin(alpha) = body_radius */
/*                     ----------- */
/*                     range_to_body */

/*     The corresponding time derivative of theta: */

/*        d(theta) = d(sep) - d(alpha1) - d(alpha2) */
/*        --------   ------   ---------   --------- */
/*        dt         dt       dt          dt */

/*     Note, alpha1, alpha2 and their derivatives have value zero */
/*     for point objects. */

    dtheta = dvsep_(pv1, pv2);

/*     Check for a failure caused by a numerical event. */

    if (failed_()) {
	*decres = TRUE_;
	chkout_("ZZGFSPDC", (ftnlen)8);
	return 0;
    }
    dtheta = dtheta - dhfa_(pv1, &svrad1) - dhfa_(pv2, &svrad2);
    if (dtheta < 0.) {
	*decres = TRUE_;
    } else {
	*decres = FALSE_;
    }
    chkout_("ZZGFSPDC", (ftnlen)8);
    return 0;
/* $Procedure ZZGFSPGQ ( Private - GF, calculate angular separation ) */

L_zzgfspgq:
/* $ Abstract */

/*     Determine the angular separation between the limbs of the two */
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

/*     ANGLE */
/*     GEOMETRY */
/*     ROOT */

/* $ Declarations */

/*      DOUBLE PRECISION      ET */
/*      DOUBLE PRECISION      SEP */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     ET         I   Ephemeris seconds past J2000 TDB. */
/*     SEP        O   Separation at time ET. */

/* $ Detailed_Input */

/*     ET       time in ephemeris seconds past J2000 when the */
/*              angular separation between the two bodies is */
/*              to be computed. */

/* $ Detailed_Output */

/*     SEP      is the angular separation between SVBOD1 and SVBOD2 as */
/*              seen from SVOBS at time ET. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     None. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine determines the apparent angular separation between */
/*     the limbs of bodies SVBOD1 and SVBOD2 as seen from SVOBS at */
/*     time ET. */

/* $ Examples */

/*     None. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     L.S. Elson         (JPL) */
/*     E.D. Wright        (JPL) */

/* $ Version */

/* -    SPICELIB Version 3.0.0, 30-MAY-2021 (EDW) (JDR) */

/*        Edited the header to comply with NAIF standard. */

/*        Routine now calls ZZSEPQ, a copy of ZZGFSPQ cast as */
/*        a function. */

/* -    SPICELIB Version 2.0.0, 17-FEB-2011 (EDW) */

/*        Routine renamed from ZZGFGSEP to ZZGFSPGQ to match geometry */
/*        finder naming convention. */

/* -    SPICELIB Version 1.0.0, 26-AUG-2003 (LSE) */

/* -& */
/* $ Index_Entries */

/*     angular separation between two bodies */

/* -& */
    *sep = zzsepq_(et, &svbod1, &svbod2, &svrad1, &svrad2, &svobs, svabcr, 
	    svref, (ftnlen)32, (ftnlen)32);
    return 0;
/* $Procedure ZZGFSPX ( Private -- GF, retrieve ZZGFSPIN values ) */

L_zzgfspx:
/* $ Abstract */

/*     Retrieve values set in ZZGFSPIN. */

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

/*     CHARACTER*(*)         XABCR */
/*     INTEGER               XBOD  (2) */
/*     CHARACTER*(*)         YREF */
/*     CHARACTER*(*)         XREF  (2) */
/*     INTEGER               XOBS */
/*     DOUBLE PRECISION      XRAD  (2) */
/*     INTEGER               XSHP  (2) */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     XABCR      O   Saved value SVABCR. */
/*     XBOD       O   Saved values SVBOD1 and SVBOD2. */
/*     YREF       O   Saved value SVREF. */
/*     XREF       O   Saved values SVREF1 and SVREF2. */
/*     XOBS       O   Saved value SVOBS. */
/*     XRAD       O   Saved values SVRAD1 and SVRAD2. */
/*     XSHP       O   Saved values SVSHP1 and SVSHP2. */

/* $ Detailed_Input */

/*     None. */

/* $ Detailed_Output */

/*     XABCR    initialized via ZZGFSPIN. */

/*     XBOD1    initialized via ZZGFSPIN. */

/*     XBOD2    initialized via ZZGFSPIN. */

/*     YREF     initialized via ZZGFSPIN. */

/*     XREF1    initialized via ZZGFSPIN. */

/*     XREF2    initialized via ZZGFSPIN. */

/*     XOBS     initialized via ZZGFSPIN. */

/*     XRAD1    initialized via ZZGFSPIN. */

/*     XRAD2    initialized via ZZGFSPIN. */

/*     XSHP1    initialized via ZZGFSPIN. */

/*     XSHP2    initialized via ZZGFSPIN. */

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

/*     J. Diaz del Rio    (ODC Space) */
/*     E.D. Wright        (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.1, 30-MAY-2021 (EDW) (JDR) */

/*        Edited the header to comply with NAIF standard. */

/*        Completed descriptions for $Brief_I/O. */

/* -    SPICELIB Version 1.0.0, 24-SEP-2012 (EDW) */

/* -& */
/* $ Index_Entries */

/*     get saved separation angle parameters */

/* -& */
    s_copy(xabcr, svabcr, xabcr_len, (ftnlen)32);
    xbod[0] = svbod1;
    xbod[1] = svbod2;
    s_copy(yref, svref, yref_len, (ftnlen)32);
    s_copy(xref, svref1, xref_len, (ftnlen)32);
    s_copy(xref + xref_len, svref2, xref_len, (ftnlen)32);
    *xobs = svobs;
    xrad[0] = svrad1;
    xrad[1] = svrad2;
    xshp[0] = svshp1;
    xshp[1] = svshp2;
    return 0;
} /* zzgfspu_ */

/* Subroutine */ int zzgfspu_(char *of, char *from, char *shape, char *frame, 
	doublereal *et, U_fp udfunc, char *abcorr, logical *decres, 
	doublereal *sep, char *xabcr, integer *xbod, char *yref, char *xref, 
	integer *xobs, doublereal *xrad, integer *xshp, ftnlen of_len, ftnlen 
	from_len, ftnlen shape_len, ftnlen frame_len, ftnlen abcorr_len, 
	ftnlen xabcr_len, ftnlen yref_len, ftnlen xref_len)
{
    return zzgfspu_0_(0, of, from, shape, frame, et, udfunc, abcorr, decres, 
	    sep, xabcr, xbod, yref, xref, xobs, xrad, xshp, of_len, from_len, 
	    shape_len, frame_len, abcorr_len, xabcr_len, yref_len, xref_len);
    }

/* Subroutine */ int zzgfspin_(char *of, char *from, char *shape, char *frame,
	 char *abcorr, ftnlen of_len, ftnlen from_len, ftnlen shape_len, 
	ftnlen frame_len, ftnlen abcorr_len)
{
    return zzgfspu_0_(1, of, from, shape, frame, (doublereal *)0, (U_fp)0, 
	    abcorr, (logical *)0, (doublereal *)0, (char *)0, (integer *)0, (
	    char *)0, (char *)0, (integer *)0, (doublereal *)0, (integer *)0, 
	    of_len, from_len, shape_len, frame_len, abcorr_len, (ftnint)0, (
	    ftnint)0, (ftnint)0);
    }

/* Subroutine */ int zzgfspdc_(U_fp udfunc, doublereal *et, logical *decres)
{
    return zzgfspu_0_(2, (char *)0, (char *)0, (char *)0, (char *)0, et, 
	    udfunc, (char *)0, decres, (doublereal *)0, (char *)0, (integer *)
	    0, (char *)0, (char *)0, (integer *)0, (doublereal *)0, (integer *
	    )0, (ftnint)0, (ftnint)0, (ftnint)0, (ftnint)0, (ftnint)0, (
	    ftnint)0, (ftnint)0, (ftnint)0);
    }

/* Subroutine */ int zzgfspgq_(doublereal *et, doublereal *sep)
{
    return zzgfspu_0_(3, (char *)0, (char *)0, (char *)0, (char *)0, et, (
	    U_fp)0, (char *)0, (logical *)0, sep, (char *)0, (integer *)0, (
	    char *)0, (char *)0, (integer *)0, (doublereal *)0, (integer *)0, 
	    (ftnint)0, (ftnint)0, (ftnint)0, (ftnint)0, (ftnint)0, (ftnint)0, 
	    (ftnint)0, (ftnint)0);
    }

/* Subroutine */ int zzgfspx_(char *xabcr, integer *xbod, char *yref, char *
	xref, integer *xobs, doublereal *xrad, integer *xshp, ftnlen 
	xabcr_len, ftnlen yref_len, ftnlen xref_len)
{
    return zzgfspu_0_(4, (char *)0, (char *)0, (char *)0, (char *)0, (
	    doublereal *)0, (U_fp)0, (char *)0, (logical *)0, (doublereal *)0,
	     xabcr, xbod, yref, xref, xobs, xrad, xshp, (ftnint)0, (ftnint)0, 
	    (ftnint)0, (ftnint)0, (ftnint)0, xabcr_len, yref_len, xref_len);
    }

