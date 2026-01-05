/* zzfrmgt0.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__36 = 36;
static integer c__1 = 1;
static integer c__6 = 6;

/* $Procedure      ZZFRMGT0 ( Frame get transformation ) */
/* Subroutine */ int zzfrmgt0_(integer *infrm, doublereal *et, doublereal *
	xform, integer *outfrm, logical *found)
{
    /* Initialized data */

    static logical pass1 = TRUE_;
    static char versn[6] = "4.0.0 ";

    /* System generated locals */
    integer i__1, i__2;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer);

    /* Local variables */
    extern /* Subroutine */ int zzdynfr0_(integer *, integer *, doublereal *, 
	    doublereal *, integer *);
    integer type__, i__, j;
    extern /* Subroutine */ int zzswfxfm_(integer *, doublereal *, integer *, 
	    doublereal *, integer *, logical *), chkin_(char *, ftnlen), 
	    errch_(char *, char *, ftnlen, ftnlen);
    static doublereal idnt66[36]	/* was [6][6] */;
    doublereal tsipm[36]	/* was [6][6] */;
    extern logical failed_(void);
    extern /* Subroutine */ int cleard_(integer *, doublereal *), ckfxfm_(
	    integer *, doublereal *, doublereal *, integer *, logical *);
    integer center;
    extern /* Subroutine */ int frinfo_(integer *, integer *, integer *, 
	    integer *, logical *), tisbod_(char *, integer *, doublereal *, 
	    doublereal *, ftnlen), tkfram_(integer *, doublereal *, integer *,
	     logical *), sigerr_(char *, ftnlen);
    integer typeid;
    doublereal rotate[9]	/* was [3][3] */;
    extern /* Subroutine */ int chkout_(char *, ftnlen), setmsg_(char *, 
	    ftnlen), errint_(char *, integer *, ftnlen), irfrot_(integer *, 
	    integer *, doublereal *);
    extern logical return_(void);
    extern /* Subroutine */ int invstm_(doublereal *, doublereal *);

/* $ Abstract */

/*     Find the transformation from a user specified frame to */
/*     another frame at a user specified epoch. */

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

/*     FRAMES */

/* $ Keywords */

/*     FRAMES */

/* $ Declarations */
/* $ Abstract */

/*     The parameters below form an enumerated list of the recognized */
/*     frame types. They are: INERTL, PCK, CK, TK, DYN, SWTCH, and ALL. */
/*     The meanings are outlined below. */

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

/*     INERTL      an inertial frame that is listed in the routine */
/*                 CHGIRF and that requires no external file to */
/*                 compute the transformation from or to any other */
/*                 inertial frame. */

/*     PCK         is a frame that is specified relative to some */
/*                 INERTL frame and that has an IAU model that */
/*                 may be retrieved from the PCK system via a call */
/*                 to the routine TISBOD. */

/*     CK          is a frame defined by a C-kernel. */

/*     TK          is a "text kernel" frame.  These frames are offset */
/*                 from their associated "relative" frames by a */
/*                 constant rotation. */

/*     DYN         is a "dynamic" frame.  These currently are */
/*                 parameterized, built-in frames where the full frame */
/*                 definition depends on parameters supplied via a */
/*                 frame kernel. */

/*     SWTCH       is a "switch" frame. These frames have orientation */
/*                 defined by their alignment with base frames selected */
/*                 from a prioritized list. The base frames optionally */
/*                 have associated time intervals of applicability. */

/*     ALL         indicates any of the above classes. This parameter */
/*                 is used in APIs that fetch information about frames */
/*                 of a specified class. */


/* $ Author_and_Institution */

/*     N.J. Bachman    (JPL) */
/*     B.V. Semenov    (JPL) */
/*     W.L. Taber      (JPL) */

/* $ Literature_References */

/*     None. */

/* $ Version */

/* -    SPICELIB Version 5.0.0, 08-OCT-2020 (NJB) (BVS) */

/*       The parameter SWTCH was added to support the switch */
/*       frame class. */

/* -    SPICELIB Version 4.0.0, 08-MAY-2012 (NJB) */

/*       The parameter ALL was added to support frame fetch APIs. */

/* -    SPICELIB Version 3.0.0, 28-MAY-2004 (NJB) */

/*       The parameter DYN was added to support the dynamic frame class. */

/* -    SPICELIB Version 2.0.0, 12-DEC-1996 (WLT) */

/*        Various unused frames types were removed and the */
/*        frame time TK was added. */

/* -    SPICELIB Version 1.0.0, 10-DEC-1995 (WLT) */

/* -& */

/*     End of INCLUDE file frmtyp.inc */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     INFRM      I   The integer code for a SPICE reference frame. */
/*     ET         I   An epoch in seconds past J2000. */
/*     XFORM      O   A state transformation matrix. */
/*     OUTFRM     O   The frame that XFORM transforms INFRM to. */
/*     FOUND      O   TRUE if a frame transformation can be found. */

/* $ Detailed_Input */

/*     INFRM       is the SPICE ID-code for some reference frame. */

/*     ET          is an epoch in ephemeris seconds past J2000 at */
/*                 which the user wishes to retrieve a state */
/*                 transformation matrix. */

/* $ Detailed_Output */

/*     XFORM       is a 6x6 matrix that transforms states relative to */
/*                 INFRM to states relative to OUTFRM. (Assuming such a */
/*                 transformation can be found.) */

/*     OUTFRM      is the SPICE ID-code of a reference frame. The 6x6 */
/*                 matrix XFORM transforms states relative to INFRM to */
/*                 states relative to OUTFRM. The state transformation */
/*                 is achieved by multiplying XFORM on the right by a */
/*                 state relative to INFRM.  This is easily accomplished */
/*                 via the subroutine call shown below. */

/*                    CALL MXVG ( XFORM, STATE, 6, 6, OSTATE ) */

/*     FOUND       is a logical flag indicating whether or not a */
/*                 transformation matrix could be found from INFRM to */
/*                 some other frame. If a transformation matrix cannot */
/*                 be found OUTFRM will be set to zero, FOUND will be */
/*                 set to FALSE and XFORM will be returned as the zero */
/*                 matrix. */

/* $ Parameters */

/*     See include file. */

/* $ Exceptions */

/*     1) If a transformation matrix cannot be located, then FOUND will */
/*        be set to FALSE, OUTFRM will be set to zero and XFORM will be */
/*        set to the zero 6x6 matrix. */

/*     2) If the class of the requested frame is not recognized the */
/*        exception 'SPICE(UNKNOWNFRAMETYPE)' will be signaled. */

/* $ Files */

/*     LSK, SCLK, PCK, FK, SPK, and/or CK kernels may need to be loaded */
/*     to provide the needed frame definition and transformation data. */

/* $ Particulars */

/*     This is a low level routine used for determining a chain of state */
/*     transformation matrices from one frame to another. */

/* $ Examples */

/*     See FRMCHG. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman    (JPL) */
/*     B.V. Semenov    (JPL) */
/*     W.L. Taber      (JPL) */

/* $ Version */

/* -    SPICELIB Version 5.0.0, 18-SEP-2020 (NJB) */

/*        Support for switch frames was added. VERSN is now */
/*        initialized via a DATA statement. Corrected long error */
/*        message to use the term "class" rather than "class id-code." */

/* -    SPICELIB Version 4.0.0, 05-JAN-2014 (BVS) */

/*        To prevent operations with un-initialized DP numbers, wrapped */
/*        IF ( .NOT. FAILED() ) ... END IF around output matrix */
/*        transposition/re-assignment operations in all branches where */
/*        the routine returning the matrix might fail. */

/*        Added zero-ing out the output matrix in cases of a failed or */
/*        .NOT. FOUND transformation look ups. */

/* -    SPICELIB Version 3.0.0, 18-DEC-2004 (NJB) */

/*        Added the new frame type 'DYN' to the list of frame */
/*        types recognized by FRMGET. */

/* -    SPICELIB Version 2.0.0, 03-APR-1997 (WLT) */

/*        Added the new frame type 'TK' to the list of frame */
/*        types recognized by FRMGET.  In addition the routine */
/*        now checks FAILED after "getting" the frame transformation. */

/* -    SPICELIB Version 1.0.0, 20-OCT-1994 (WLT) */


/* -& */
/* $ Index_Entries */

/*     Find a frame transformation matrix from a specified frame */

/* -& */

/*     SPICELIB Functions */


/*     Local Variables */


/*     Saved variables */


/*     Initial values */


/*     Set output flag. */

    *found = FALSE_;

/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }
    chkin_("ZZFRMGT0", (ftnlen)8);

/*     On the first pass, initialize the identity matrix. */

    if (pass1) {
	cleard_(&c__36, idnt66);
	for (i__ = 1; i__ <= 6; ++i__) {
	    idnt66[(i__1 = i__ + i__ * 6 - 7) < 36 && 0 <= i__1 ? i__1 : 
		    s_rnge("idnt66", i__1, "zzfrmgt0_", (ftnlen)233)] = 1.;
	}
	pass1 = FALSE_;
    }

/*     Get all the needed information about this frame. */

    frinfo_(infrm, &center, &type__, &typeid, found);
    if (! (*found)) {
	cleard_(&c__36, xform);
	*outfrm = 0;
	chkout_("ZZFRMGT0", (ftnlen)8);
	return 0;
    }

/*     FOUND was set to true by the FRINFO call. Compute transformation */
/*     based on the frame class. */

    if (type__ == 1) {
	irfrot_(infrm, &c__1, rotate);
	if (! failed_()) {
	    for (i__ = 1; i__ <= 3; ++i__) {
		for (j = 1; j <= 3; ++j) {
		    xform[(i__1 = i__ + j * 6 - 7) < 36 && 0 <= i__1 ? i__1 : 
			    s_rnge("xform", i__1, "zzfrmgt0_", (ftnlen)267)] =
			     rotate[(i__2 = i__ + j * 3 - 4) < 9 && 0 <= i__2 
			    ? i__2 : s_rnge("rotate", i__2, "zzfrmgt0_", (
			    ftnlen)267)];
		    xform[(i__1 = i__ + 3 + (j + 3) * 6 - 7) < 36 && 0 <= 
			    i__1 ? i__1 : s_rnge("xform", i__1, "zzfrmgt0_", (
			    ftnlen)268)] = rotate[(i__2 = i__ + j * 3 - 4) < 
			    9 && 0 <= i__2 ? i__2 : s_rnge("rotate", i__2, 
			    "zzfrmgt0_", (ftnlen)268)];
		    xform[(i__1 = i__ + 3 + j * 6 - 7) < 36 && 0 <= i__1 ? 
			    i__1 : s_rnge("xform", i__1, "zzfrmgt0_", (ftnlen)
			    269)] = 0.;
		    xform[(i__1 = i__ + (j + 3) * 6 - 7) < 36 && 0 <= i__1 ? 
			    i__1 : s_rnge("xform", i__1, "zzfrmgt0_", (ftnlen)
			    270)] = 0.;
		}
	    }
	    *outfrm = 1;
	}
    } else if (type__ == 2) {
	tisbod_("J2000", &typeid, et, tsipm, (ftnlen)5);
	if (! failed_()) {
	    invstm_(tsipm, xform);
	    *outfrm = 1;
	}
    } else if (type__ == 3) {
	ckfxfm_(&typeid, et, xform, outfrm, found);
    } else if (type__ == 4) {
	tkfram_(&typeid, rotate, outfrm, found);
	if (! failed_()) {
	    for (i__ = 1; i__ <= 3; ++i__) {
		for (j = 1; j <= 3; ++j) {
		    xform[(i__1 = i__ + j * 6 - 7) < 36 && 0 <= i__1 ? i__1 : 
			    s_rnge("xform", i__1, "zzfrmgt0_", (ftnlen)305)] =
			     rotate[(i__2 = i__ + j * 3 - 4) < 9 && 0 <= i__2 
			    ? i__2 : s_rnge("rotate", i__2, "zzfrmgt0_", (
			    ftnlen)305)];
		    xform[(i__1 = i__ + 3 + (j + 3) * 6 - 7) < 36 && 0 <= 
			    i__1 ? i__1 : s_rnge("xform", i__1, "zzfrmgt0_", (
			    ftnlen)306)] = rotate[(i__2 = i__ + j * 3 - 4) < 
			    9 && 0 <= i__2 ? i__2 : s_rnge("rotate", i__2, 
			    "zzfrmgt0_", (ftnlen)306)];
		    xform[(i__1 = i__ + 3 + j * 6 - 7) < 36 && 0 <= i__1 ? 
			    i__1 : s_rnge("xform", i__1, "zzfrmgt0_", (ftnlen)
			    307)] = 0.;
		    xform[(i__1 = i__ + (j + 3) * 6 - 7) < 36 && 0 <= i__1 ? 
			    i__1 : s_rnge("xform", i__1, "zzfrmgt0_", (ftnlen)
			    308)] = 0.;
		}
	    }
	}
    } else if (type__ == 5) {

/*        Unlike the other frame classes, the dynamic frame evaluation */
/*        routine ZZDYNFRM requires the input frame ID rather than the */
/*        dynamic frame class ID. ZZDYNFRM also requires the center ID */
/*        we found via the FRINFO call. */

	zzdynfr0_(infrm, &center, et, xform, outfrm);

/*        The FOUND flag was set by FRINFO earlier; we don't touch */
/*        it here. If ZZDYNFRM signaled an error, FOUND will be set */
/*        to .FALSE. at end of this routine. */

    } else if (type__ == 6) {
	zzswfxfm_(infrm, et, &c__6, xform, outfrm, found);
    } else {
	cleard_(&c__36, xform);
	*outfrm = 0;
	*found = FALSE_;
	setmsg_("The reference frame # has class #. This form of reference f"
		"rame is not supported in version # of ZZFRMGT0. You need to "
		"update your version of SPICELIB to the latest version in ord"
		"er to support this frame. ", (ftnlen)205);
	errint_("#", infrm, (ftnlen)1);
	errint_("#", &type__, (ftnlen)1);
	errch_("#", versn, (ftnlen)1, (ftnlen)6);
	sigerr_("SPICE(UNKNOWNFRAMETYPE)", (ftnlen)23);
	chkout_("ZZFRMGT0", (ftnlen)8);
	return 0;
    }

/*     Make sure to clear outputs in case of a failure as defined in */
/*     in the header. */

    if (failed_() || ! (*found)) {
	cleard_(&c__36, xform);
	*outfrm = 0;
	*found = FALSE_;
    }
    chkout_("ZZFRMGT0", (ftnlen)8);
    return 0;
} /* zzfrmgt0_ */

