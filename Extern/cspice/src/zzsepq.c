/* zzsepq.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static doublereal c_b12 = 1e-12;

/* $Procedure ZZSEPQ ( Separation quantity from observer ) */
doublereal zzsepq_(doublereal *et, integer *bod1, integer *bod2, doublereal *
	r1, doublereal *r2, integer *obs, char *abcorr, char *ref, ftnlen 
	abcorr_len, ftnlen ref_len)
{
    /* System generated locals */
    doublereal ret_val, d__1;

    /* Local variables */
    extern doublereal vsep_(doublereal *, doublereal *);
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    doublereal theta;
    extern /* Subroutine */ int errdp_(char *, doublereal *, ftnlen);
    extern doublereal vnorm_(doublereal *);
    doublereal range1, range2;
    extern logical failed_(void);
    doublereal lt;
    extern doublereal dasine_(doublereal *, doublereal *);
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), setmsg_(char *, ftnlen), errint_(char *, integer *, 
	    ftnlen), spkezp_(integer *, doublereal *, char *, char *, integer 
	    *, doublereal *, doublereal *, ftnlen, ftnlen);
    extern logical return_(void);
    doublereal pv1[3], pv2[3], ang1, ang2;

/* $ Abstract */

/*     Compute the angular separation between two spherical or */
/*     point objects. */

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

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     ET         I   Ephemeris seconds past J2000 TDB */
/*     BOD1       I   NAIF ID for first target */
/*     BOD2       I   NAIF ID for second target */
/*     R1         I   Radius of a spherical model for BOD1 */
/*     R2         I   Radius of a spherical model for BOD2 */
/*     OBS        I   NAIF ID of observer */
/*     ABCORR     I   Aberration correction flag */
/*     REF        I   Reference frame of the angular separation */

/*     The function returns the angular separation between two bodies, */
/*     BOD1 and BOD2, with radius R1 and R2 respectively, as seen from */
/*     an observer OBS, possibly corrected for aberration corrections. */

/* $ Detailed_Input */

/*     ET       is the time in ephemeris seconds past J2000 TDB at */
/*              which the separation is to be measured. */

/*     BOD1 */
/*     BOD2     the NAIF IDs of the two objects for which to */
/*              determine the angular separation. */

/*     R1 */
/*     R2       are the radii of the two objects BOD1 and BOD2 */
/*              respectively. */

/*     OBS      the NAIF ID identifying the body observing */
/*              BOD1 and BOD2. */

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

/*     REF      is the name of the reference frame relative to which */
/*              the angular separation should be expressed. This may */
/*              be any frame supported by the SPICE system, including */
/*              built-in frames (documented in the Frames Required */
/*              Reading) and frames defined by a loaded frame kernel. */

/*              When REF designates a non-inertial frame, the */
/*              orientation of the frame is evaluated at an epoch */
/*              dependent on the selected aberration correction. */

/* $ Detailed_Output */

/*     The function returns the light-time (and stellar aberration */
/*     corrected if this feature is enabled) separation of the two */
/*     objects BOD1 and BOD2 as observed from OBS. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  The kernel files to be used by this routine must be loaded */
/*         (normally using the SPICELIB routine FURNSH) before this */
/*         routine is called. */

/*     2)  If either R1 or R2 have a negative value, the error */
/*         SPICE(BADRADIUS) is signaled. */

/*     3)  If the ephemeris data required to perform the needed state */
/*         look-ups are not loaded, an error is signaled by a routine in */
/*         the call tree of this routine. */

/*     4)  If R1 < RANGE1 or R2 < RANGE2, the error SPICE(INSIDEBODY) */
/*         is signaled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine determines the apparent separation between the limbs */
/*     of two objects as observed from a third. The value reported is */
/*     corrected for light time. Moreover, if at the time this routine */
/*     is called, stellar aberration corrections are enabled, this */
/*     correction will also be applied to the apparent positions of the */
/*     centers of the two objects. */

/* $ Examples */

/*     None. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     L.S. Elson         (JPL) */
/*     W.L. Taber         (JPL) */
/*     E.D. Wright        (JPL) */

/* $ Version */

/* -    SPICELIB Version 2.0.0, 16-JAN-2021 (EDW) (JDR) */

/*        Updated the header to comply with NAIF standard. */

/*        Routine made public, implemented as a function, renamed */
/*        ZZSEPQ from ZZGFSPQ. */

/*        Added an error check for observer to target ranges */
/*        less-than target body radii. */

/* -    SPICELIB Version 1.0.1, 28-JUN-2012 (EDW) */

/*        Minor edits to header, correcting spelling error and improving */
/*        syntax of $Exceptions. */

/*        Corrected a typo in SETMSG error message where BOD1 was quoted */
/*        twice instead of quoting BOD1 and BOD2. This edit does change */
/*        the error message output. */

/* -    SPICELIB Version 1.0.0, 03-MAR-2009 (EDW) (LSE) (WLT) */

/* -& */
/* $ Index_Entries */

/*     compute the apparent relative angular separation */

/* -& */

/*     SPICELIB functions. */


/*     Local Variables. */


/*     ATOL is a tolerance value for computing arc sine. */


/*     Set an initial value to return in case of error. */

    ret_val = 0.;

/*     Standard SPICE error handling. */

    if (return_()) {
	return ret_val;
    }
    chkin_("ZZSEPQ", (ftnlen)6);

/*     First check for bad inputs. */

    if (*r1 < 0. || *r2 < 0.) {
	setmsg_("A negative radius for a body was encountered. The radius fo"
		"r body #1 was given as #2, the radius of body #3 was given a"
		"s #4.", (ftnlen)124);
	errint_("#1", bod1, (ftnlen)2);
	errdp_("#2", r1, (ftnlen)2);
	errint_("#3", bod2, (ftnlen)2);
	errdp_("#4", r2, (ftnlen)2);
	sigerr_("SPICE(BADRADIUS)", (ftnlen)16);
	chkout_("ZZSEPQ", (ftnlen)6);
	return ret_val;
    }

/*     Get the state of the BOD1, BOD2 objects relative to OBS. */

    spkezp_(bod1, et, ref, abcorr, obs, pv1, &lt, ref_len, abcorr_len);
    if (failed_()) {
	chkout_("ZZSEPQ", (ftnlen)6);
	return ret_val;
    }
    spkezp_(bod2, et, ref, abcorr, obs, pv2, &lt, ref_len, abcorr_len);
    if (failed_()) {
	chkout_("ZZSEPQ", (ftnlen)6);
	return ret_val;
    }

/*     Compute the range to the objects of interest. */

    range1 = vnorm_(pv1);
    range2 = vnorm_(pv2);

/*     Compute the apparent angular radii as seen from OBS. */

    if (range1 > *r1) {
	d__1 = *r1 / range1;
	ang1 = dasine_(&d__1, &c_b12);
	if (failed_()) {
	    chkout_("ZZSEPQ", (ftnlen)6);
	    return ret_val;
	}
    } else {
	setmsg_("Observer object #1 located within surface of target 1 objec"
		"t #2. Range to target 1 #3, radius of target 1 #4", (ftnlen)
		108);
	errint_("#1", obs, (ftnlen)2);
	errint_("#2", bod1, (ftnlen)2);
	errdp_("#3", &range1, (ftnlen)2);
	errdp_("#4", r1, (ftnlen)2);
	sigerr_("SPICE(INSIDEBODY)", (ftnlen)17);
	chkout_("ZZSEPQ", (ftnlen)6);
	return ret_val;
    }
    if (range2 > *r2) {
	d__1 = *r2 / range2;
	ang2 = dasine_(&d__1, &c_b12);
	if (failed_()) {
	    chkout_("ZZSEPQ", (ftnlen)6);
	    return ret_val;
	}
    } else {
	setmsg_("Observer object #1 located within surface of target 2 objec"
		"t #2. Range to target 2 #3, radius of target 2 #4", (ftnlen)
		108);
	errint_("#1", obs, (ftnlen)2);
	errint_("#2", bod2, (ftnlen)2);
	errdp_("#3", &range2, (ftnlen)2);
	errdp_("#4", r2, (ftnlen)2);
	sigerr_("SPICE(INSIDEBODY)", (ftnlen)17);
	chkout_("ZZSEPQ", (ftnlen)6);
	return ret_val;
    }

/*     Finally compute the apparent separation. */

    theta = vsep_(pv1, pv2);
    ret_val = theta - ang1 - ang2;
    chkout_("ZZSEPQ", (ftnlen)6);
    return ret_val;
} /* zzsepq_ */

