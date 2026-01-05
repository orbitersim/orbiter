/* zzspin.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;
static integer c__2 = 2;

/* $Procedure ZZSPIN ( Separation quantity initializer ) */
/* Subroutine */ int zzspin_(char *targ1, char *shape1, char *frame1, char *
	targ2, char *shape2, char *frame2, char *obsrvr, char *abcorr, 
	integer *bods, integer *frames, doublereal *mxrad, integer *obs, 
	ftnlen targ1_len, ftnlen shape1_len, ftnlen frame1_len, ftnlen 
	targ2_len, ftnlen shape2_len, ftnlen frame2_len, ftnlen obsrvr_len, 
	ftnlen abcorr_len)
{
    /* Initialized data */

    static char svshap[32*2] = "POINT                           " "SPHERE   "
	    "                       ";

    /* System generated locals */
    doublereal d__1;

    /* Local variables */
    char shap1[6], shap2[6];
    extern /* Subroutine */ int zzgftreb_(integer *, doublereal *);
    doublereal axes1[3], axes2[3];
    extern /* Subroutine */ int zzvalcor_(char *, logical *, ftnlen), chkin_(
	    char *, ftnlen), errch_(char *, char *, ftnlen, ftnlen);
    integer class__;
    logical found;
    extern /* Subroutine */ int bods2c_(char *, integer *, logical *, ftnlen);
    extern logical failed_(void);
    extern integer isrchc_(char *, integer *, char *, ftnlen, ftnlen);
    extern /* Subroutine */ int namfrm_(char *, integer *, ftnlen);
    integer clssid;
    logical attblk[15];
    extern /* Subroutine */ int frinfo_(integer *, integer *, integer *, 
	    integer *, logical *), sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), setmsg_(char *, ftnlen), errint_(char *, integer *, 
	    ftnlen), ljucrs_(integer *, char *, char *, ftnlen, ftnlen);
    extern logical return_(void);
    integer ctr1, ctr2, shp1, shp2;

/* $ Abstract */

/*     Check/initialize the quantities describing an angular separation */
/*     between two spherical or point objects. */

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

/*     ABCORR.REQ */

/* $ Keywords */

/*     ANGLE */
/*     GEOMETRY */

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

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     TARG1      I   Name for first target */
/*     SHAPE1     I   Shape corresponding to TARG1 */
/*     FRAME1     I   Body fixed frame for TARG1 */
/*     TARG2      I   Name for second target */
/*     SHAPE2     I   Shape corresponding to TARG2 */
/*     FRAME2     I   Body fixed frame for TARG2 */
/*     OBSRVR     I   Name for observer body */
/*     ABCORR     I   Aberration correction flag */
/*     BODS       O   Array(2) of body/SPK IDs for TARG1 and TARG2 */
/*     FRAMES     O   Array(2) of frame IDs for TARG1 and TARG2 */
/*     MXRAD      O   Array(2) of max radii values for TARG1 and TARG2 */
/*     OBS        O   Body/SPK IDs of OBSRVR */

/* $ Detailed_Input */

/*     TARG1    the string naming the first body of interest. You can */
/*              also supply the integer ID code for the object as an */
/*              integer string. For example both 'MOON' and '301' */
/*              are legitimate strings that indicate the moon is the */
/*              target body. */

/*     SHAPE1   the string naming the geometric model used to */
/*              represent the shape of the TARG1 body. Models */
/*              supported by this routine: */

/*                'SPHERE'        Treat the body as a sphere with */
/*                                radius equal to the maximum value of */
/*                                BODYnnn_AXES. */

/*                'POINT'         Treat the body as a point; */
/*                                radius has value zero. */

/*              The SHAPE1 string lacks sensitivity to case, leading */
/*              and trailing blanks. */

/*     FRAME1   the string naming the body-fixed reference frame */
/*              corresponding to TARG1. This routine does not currently */
/*              use this argument's value, its use is reserved for */
/*              future shape models. The value 'NULL' will suffice for */
/*              "POINT" and "SPHERE" shaped bodies. */

/*     TARG2    the string naming the second body of interest. You can */
/*              also supply the integer ID code for the object as an */
/*              integer string. For example both 'MOON' and '301' */
/*              are legitimate strings that indicate the moon is the */
/*              target body. */

/*     SHAPE2   the string naming the geometric model used to */
/*              represent the shape of the TARG2 body. Models */
/*              supported by this routine: */

/*                'SPHERE'        Treat the body as a sphere with */
/*                                radius equal to the maximum value of */
/*                                BODYnnn_AXES. */

/*                'POINT'         Treat the body as a point; */
/*                                radius has value zero. */

/*              The SHAPE2 string lacks sensitivity to case, leading */
/*              and trailing blanks. */

/*     FRAME2   the string naming the body-fixed reference frame */
/*              corresponding to TARG2. This routine does not currently */
/*              use this argument's value, its use is reserved for */
/*              future shape models. The value 'NULL' will suffice for */
/*              "POINT" and "SPHERE" shaped bodies. */

/*     OBSRVR   the string naming the observing body. Optionally, you */
/*              may supply the ID code of the object as an integer */
/*              string. For example, both 'EARTH' and '399' are */
/*              legitimate strings to supply to indicate the */
/*              observer is Earth. */

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

/*     BODS     the NAIF SPK/body IDs of the two objects for which to */
/*              determine the angular separation. */

/*     FRAMES   the NAIF frame IDs of the body fixed reference frames */
/*              of two objects for which to determine the angular */
/*              separation. */

/*     MXRAD    the maximum radial values of the two objects. */

/*     OBS      the NAIF SPK/body ID of the observer. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the three objects TARG1, TARG2, and OBSRVR are not */
/*         distinct, the error SPICE(BODIESNOTDISTINCT) is signaled. */

/*     2)  If the SHAPE1 or SHAPE2 values lack a corresponding case */
/*         block, the error SPICE(BUG) is signaled. This indicates a */
/*         programming error. */

/*     3)  If the object names for TARG1 TARG2, or OBSRVR cannot resolve */
/*         to a NAIF body ID, the error SPICE(IDCODENOTFOUND) is */
/*         signaled. */

/*     4)  If the reference frame associated with TARG1, FRAME1, is not */
/*         centered on TARG1, or if the reference frame associated with */
/*         TARG2, FRAME2, is not centered on TARG2, the error */
/*         SPICE(INVALIDFRAME) is signaled. */

/*     5)  If the frame name for FRAME1 or FRAME2 cannot resolve to a */
/*         NAIF frame ID, the error SPICE(NOFRAME) is signaled. */

/*     6)  If the body shape for TARG1, SHAPE1, or the body shape for */
/*         TARG2, SHAPE2, is not recognized, the error */
/*         SPICE(NOTRECOGNIZED) is signaled. */

/*     7)  If the requested aberration correction ABCORR is not */
/*         recognized, an error is signaled by a routine in the call tree */
/*         of this routine. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     Please refer to the Aberration Corrections Required Reading */
/*     (abcorr.req) for detailed information describing the nature and */
/*     calculation of the applied corrections. */

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

/* -    SPICELIB Version 1.0.0, 16-JAN-2021 (EDW) (JDR) */

/*        Based on code originally in zzgfspu.f. */

/* -& */
/* $ Index_Entries */

/*     check quantities for apparent relative angular separation */

/* -& */

/*     SPICELIB functions. */


/*     Local parameters */


/*     Local Variables */


/*     Below we initialize the list of shape names. */


/*     Define integer ID parameters for the shape names in */
/*     SVSHAP. */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }
    chkin_("ZZSPIN", (ftnlen)6);

/*     FRAMES argument not currently used. */

    frames[0] = -1;
    frames[1] = -1;
    bods2c_(targ1, bods, &found, targ1_len);
    if (! found) {
	setmsg_("The object name for target 1, '#', is not a recognized name"
		" for an ephemeris object. The cause of this problem may be t"
		"hat you need an updated version of the SPICE Toolkit.", (
		ftnlen)172);
	errch_("#", targ1, (ftnlen)1, targ1_len);
	sigerr_("SPICE(IDCODENOTFOUND)", (ftnlen)21);
	chkout_("ZZSPIN", (ftnlen)6);
	return 0;
    }
    bods2c_(targ2, &bods[1], &found, targ2_len);
    if (! found) {
	setmsg_("The object name for target 2, '#', is not a recognized name"
		" for an ephemeris object. The cause of this problem may be t"
		"hat you need an updated version of the SPICE Toolkit.", (
		ftnlen)172);
	errch_("#", targ2, (ftnlen)1, targ2_len);
	sigerr_("SPICE(IDCODENOTFOUND)", (ftnlen)21);
	chkout_("ZZSPIN", (ftnlen)6);
	return 0;
    }
    bods2c_(obsrvr, obs, &found, obsrvr_len);
    if (! found) {
	setmsg_("The object name for the observer, '#', is not a recognized "
		"name for an ephemeris object. The cause of this problem may "
		"be that you need an updated version of the SPICE Toolkit.", (
		ftnlen)176);
	errch_("#", obsrvr, (ftnlen)1, obsrvr_len);
	sigerr_("SPICE(IDCODENOTFOUND)", (ftnlen)21);
	chkout_("ZZSPIN", (ftnlen)6);
	return 0;
    }

/*     Confirm the three bodies have unique IDs. */

    if (*obs == bods[0] || *obs == bods[1] || bods[0] == bods[1]) {
	setmsg_("All three objects associated with an ANGULAR SEPARATION cal"
		"culation must be distinct. The objects whose angular separat"
		"ion is of interest were # and #. The observer was #.", (
		ftnlen)171);
	errint_("#", bods, (ftnlen)1);
	errint_("#", &bods[1], (ftnlen)1);
	errint_("#", obs, (ftnlen)1);
	sigerr_("SPICE(BODIESNOTDISTINCT)", (ftnlen)24);
	chkout_("ZZSPIN", (ftnlen)6);
	return 0;
    }

/*     Check the aberration correction. If SPKEZR can't handle it, */
/*     neither can we. */

    zzvalcor_(abcorr, attblk, abcorr_len);
    if (failed_()) {
	chkout_("ZZSPIN", (ftnlen)6);
	return 0;
    }

/*     Check first shape. */

    ljucrs_(&c__1, shape1, shap1, shape1_len, (ftnlen)6);

/*     If we pass the error check, then SHAPE1 exists in SVSHAP. */

    shp1 = isrchc_(shap1, &c__2, svshap, (ftnlen)6, (ftnlen)32);
    if (shp1 == 0) {
	setmsg_("The body shape, # is not recognized.  Supported quantities "
		"are: POINT, SPHERE.", (ftnlen)78);
	errch_("#", shap1, (ftnlen)1, (ftnlen)6);
	sigerr_("SPICE(NOTRECOGNIZED)", (ftnlen)20);
	chkout_("ZZSPIN", (ftnlen)6);
	return 0;
    } else if (shp1 == 1) {
	mxrad[0] = 0.;
    } else if (shp1 == 2) {
	zzgftreb_(bods, axes1);
	if (failed_()) {
	    chkout_("ZZSPIN", (ftnlen)6);
	    return 0;
	}
/* Computing MAX */
	d__1 = max(axes1[0],axes1[1]);
	mxrad[0] = max(d__1,axes1[2]);
    } else {

/*        This code executes only if someone adds a new shape */
/*        name to SVSHAP then fails to update the SHP1 condition */
/*        block to respond to the name. Fortran needs SWITCH...CASE. */

	setmsg_("Encountered uncoded shape ID for #. This indicates a bug. P"
		"lease contact NAIF.", (ftnlen)78);
	errch_("#", shap1, (ftnlen)1, (ftnlen)6);
	sigerr_("SPICE(BUG)", (ftnlen)10);
	chkout_("ZZSPIN", (ftnlen)6);
	return 0;
    }

/*     Check second shape. */

    ljucrs_(&c__1, shape2, shap2, shape2_len, (ftnlen)6);

/*     If we pass the error check, then SHAPE2 exists in SVSHAP. */

    shp2 = isrchc_(shap2, &c__2, svshap, (ftnlen)6, (ftnlen)32);
    if (shp2 == 0) {
	setmsg_("The body shape, # is not recognized.  Supported quantities "
		"are: POINT, SPHERE.", (ftnlen)78);
	errch_("#", shap2, (ftnlen)1, (ftnlen)6);
	sigerr_("SPICE(NOTRECOGNIZED)", (ftnlen)20);
	chkout_("ZZSPIN", (ftnlen)6);
	return 0;
    } else if (shp2 == 1) {
	mxrad[1] = 0.;
    } else if (shp2 == 2) {
	zzgftreb_(&bods[1], axes2);
	if (failed_()) {
	    chkout_("ZZSPIN", (ftnlen)6);
	    return 0;
	}
/* Computing MAX */
	d__1 = max(axes2[0],axes2[1]);
	mxrad[1] = max(d__1,axes2[2]);
    } else {

/*        This code executes only if someone adds a new shape */
/*        name to SVSHAP then fails to update the SHP2 condition */
/*        block to respond to the name. Fortran needs SWITCH...CASE. */

	setmsg_("Encountered uncoded shape ID for #. This indicates a bug. P"
		"lease contact NAIF.", (ftnlen)78);
	errch_("#", shap2, (ftnlen)1, (ftnlen)6);
	sigerr_("SPICE(BUG)", (ftnlen)10);
	chkout_("ZZSPIN", (ftnlen)6);
	return 0;
    }

/*     Confirm the center of the input reference frames correspond */
/*     to the target bodies for non-point, non-spherical bodies. */

/*        FRAME1 centered on TARG1 */
/*        FRAME2 centered on TARG2 */

/*     This check does not apply to POINT or SPHERE shapes. */

    if (shp1 != 1 && shp1 != 2) {
	namfrm_(frame1, frames, frame1_len);
	frinfo_(frames, &ctr1, &class__, &clssid, &found);
	if (! found) {
	    setmsg_("Frame system did not recognize frame #.", (ftnlen)39);
	    errch_("#", frame1, (ftnlen)1, frame1_len);
	    sigerr_("SPICE(NOFRAME)", (ftnlen)14);
	    chkout_("ZZSPIN", (ftnlen)6);
	    return 0;
	}
	if (bods[0] != ctr1) {
	    setmsg_("The reference frame #1 associated with target body #2 i"
		    "s not centered on #2. The frame must be centered on the "
		    "target body.", (ftnlen)123);
	    errch_("#1", frame1, (ftnlen)2, frame1_len);
	    errch_("#2", targ1, (ftnlen)2, targ1_len);
	    sigerr_("SPICE(INVALIDFRAME)", (ftnlen)19);
	    chkout_("ZZSPIN", (ftnlen)6);
	    return 0;
	}
    }
    if (shp2 != 1 && shp2 != 2) {
	namfrm_(frame2, &frames[1], frame2_len);
	frinfo_(&frames[1], &ctr2, &class__, &clssid, &found);
	if (! found) {
	    setmsg_("Frame system did not recognize frame #.", (ftnlen)39);
	    errch_("#", frame2, (ftnlen)1, frame2_len);
	    sigerr_("SPICE(NOFRAME)", (ftnlen)14);
	    chkout_("ZZSPIN", (ftnlen)6);
	    return 0;
	}
	if (bods[1] != ctr2) {
	    setmsg_("The reference frame #1 associated with target body #2 i"
		    "s not centered on #2. The frame must be centered on the "
		    "target body.", (ftnlen)123);
	    errch_("#1", frame2, (ftnlen)2, frame2_len);
	    errch_("#2", targ2, (ftnlen)2, targ2_len);
	    sigerr_("SPICE(INVALIDFRAME)", (ftnlen)19);
	    chkout_("ZZSPIN", (ftnlen)6);
	    return 0;
	}
    }

/*     Done. Check out. Return. */

    chkout_("ZZSPIN", (ftnlen)6);
    return 0;
} /* zzspin_ */

