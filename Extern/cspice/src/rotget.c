/* rotget.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__9 = 9;
static integer c__1 = 1;
static integer c__3 = 3;

/* $Procedure ROTGET ( Frame get rotation ) */
/* Subroutine */ int rotget_(integer *infrm, doublereal *et, doublereal *
	rotate, integer *outfrm, logical *found)
{
    /* Initialized data */

    static char versn[6] = "5.0.0 ";

    doublereal tipm[9]	/* was [3][3] */;
    integer type__;
    extern /* Subroutine */ int zzswfxfm_(integer *, doublereal *, integer *, 
	    doublereal *, integer *, logical *), zzdynrot_(integer *, integer 
	    *, doublereal *, doublereal *, integer *), chkin_(char *, ftnlen),
	     errch_(char *, char *, ftnlen, ftnlen), xpose_(doublereal *, 
	    doublereal *);
    extern logical failed_(void);
    extern /* Subroutine */ int cleard_(integer *, doublereal *);
    integer center;
    extern /* Subroutine */ int tipbod_(char *, integer *, doublereal *, 
	    doublereal *, ftnlen), frinfo_(integer *, integer *, integer *, 
	    integer *, logical *), tkfram_(integer *, doublereal *, integer *,
	     logical *), ckfrot_(integer *, doublereal *, doublereal *, 
	    integer *, logical *), sigerr_(char *, ftnlen);
    integer typeid;
    extern /* Subroutine */ int chkout_(char *, ftnlen), setmsg_(char *, 
	    ftnlen), errint_(char *, integer *, ftnlen), irfrot_(integer *, 
	    integer *, doublereal *);
    extern logical return_(void);

/* $ Abstract */

/*     Find the rotation from a user specified frame to another frame at */
/*     a user specified epoch. */

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
/*     ROTATE     O   A rotation matrix. */
/*     OUTFRM     O   The frame that ROTATE transforms INFRM to. */
/*     FOUND      O   .TRUE. if a rotation can be found. */

/* $ Detailed_Input */

/*     INFRM    is the SPICE ID-code for some reference frame. */

/*     ET       is an epoch in ephemeris seconds past J2000 at which */
/*              the user wishes to retrieve a rotation matrix. */

/* $ Detailed_Output */

/*     ROTATE   is a 3x3 matrix that rotates positions relative to */
/*              INFRM to positions relative to OUTFRM. (Assuming such */
/*              a rotation can be found.) */

/*     OUTFRM   is the SPICE ID-code of a reference frame. The 3x3 */
/*              matrix ROTATE rotates positions relative to INFRM to */
/*              positions relative to OUTFRM. The positions */
/*              transformation is achieved by multiplying ROTATE on */
/*              the right by a position relative to INFRM. This is */
/*              easily accomplished via the subroutine call shown */
/*              below. */

/*                 CALL MXV  ( ROTATE, INPOS,  OUTPOS ) */

/*     FOUND    is a logical flag indicating whether or not a */
/*              rotation matrix could be found from INFRM to some */
/*              other frame. If a rotation matrix cannot be found */
/*              OUTFRM will be set to zero, FOUND will be set to */
/*              .FALSE. and ROTATE will be returned as the zero matrix. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If a rotation matrix cannot be located, then FOUND will be set */
/*         to .FALSE., OUTFRM will be set to zero and ROTATE will be set */
/*         to the zero 3x3 matrix. */

/*     2)  If the class of the requested frame is not recognized the */
/*         exception, the error SPICE(UNKNOWNFRAMETYPE) is signaled. */

/* $ Files */

/*     LSK, SCLK, PCK, FK, SPK, and/or CK kernels may need to be loaded */
/*     to provide the needed frame definition and transformation data. */

/* $ Particulars */

/*     This is a low level routine used for determining a chain of */
/*     position transformation matrices from one frame to another. */

/* $ Examples */

/*     See REFCHG. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     B.V. Semenov       (JPL) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 5.0.0, 15-MAR-2021 (NJB) (JDR) */

/*        **Updated shadow routines ZZROTGT0 and ZZROTGT1, as must be */
/*          done each time this routine is updated.** */

/*        Support for switch frames was added. VERSN is now */
/*        initialized via a DATA statement. Corrected long error */
/*        message to use the term "class" rather than "class id-code." */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 4.0.0, 21-MAR-2014 (BVS) */

/*        To prevent operations with un-initialized DP numbers, wrapped */
/*        IF ( .NOT. FAILED() ) ... END IF around output matrix */
/*        transposition operation in the PCK frame branch where the */
/*        routine returning the matrix might fail. */

/*        Incremented major version token by 2 to sync up versions with */
/*        FRMGET. */

/* -    SPICELIB Version 2.1.0, 02-MAR-2010 (NJB) */

/*        Bug fix: frame ID rather than frame class ID */
/*        is now passed to dynamic frame evaluation */
/*        routine ZZDYNROT. Order of header sections was */
/*        corrected. */

/* -    SPICELIB Version 2.0.0, 18-DEC-2004 (NJB) */

/*        Added the new frame type 'DYN' to the list of frame */
/*        types recognized by ROTGET. */

/* -    SPICELIB Version 1.0.0, 03-MAR-1999 (WLT) */

/* -& */
/* $ Index_Entries */

/*     Find a rotation matrix from a specified frame */

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
    chkin_("ROTGET", (ftnlen)6);

/*     Get all the needed information about this frame. */

    frinfo_(infrm, &center, &type__, &typeid, found);
    if (! (*found)) {
	cleard_(&c__9, rotate);
	*outfrm = 0;
	chkout_("ROTGET", (ftnlen)6);
	return 0;
    }

/*     FOUND was set to true by the FRINFO call. Compute rotation based */
/*     on the frame class. */

    if (type__ == 1) {
	irfrot_(infrm, &c__1, rotate);
	if (! failed_()) {
	    *outfrm = 1;
	}
    } else if (type__ == 2) {
	tipbod_("J2000", &typeid, et, tipm, (ftnlen)5);
	if (! failed_()) {
	    xpose_(tipm, rotate);
	    *outfrm = 1;
	}
    } else if (type__ == 3) {
	ckfrot_(&typeid, et, rotate, outfrm, found);
    } else if (type__ == 4) {
	tkfram_(&typeid, rotate, outfrm, found);
    } else if (type__ == 5) {

/*        Unlike the other frame classes, the dynamic frame evaluation */
/*        routine ZZDYNROT requires the input frame ID rather than the */
/*        dynamic frame class ID. ZZDYNROT also requires the center ID */
/*        we found via the FRINFO call. */
	zzdynrot_(infrm, &center, et, rotate, outfrm);

/*        The FOUND flag was set by FRINFO earlier; we don't touch */
/*        it here. If ZZDYNROT signaled an error, FOUND will be set */
/*        to .FALSE. at end of this routine. */

    } else if (type__ == 6) {
	zzswfxfm_(infrm, et, &c__3, rotate, outfrm, found);
    } else {
	cleard_(&c__9, rotate);
	*outfrm = 0;
	*found = FALSE_;
	setmsg_("The reference frame # has class #. This form of reference f"
		"rame is not supported in version # of ROTGET. You need to up"
		"date your version of SPICELIB to the latest version in order"
		" to support this frame. ", (ftnlen)203);
	errint_("#", infrm, (ftnlen)1);
	errint_("#", &type__, (ftnlen)1);
	errch_("#", versn, (ftnlen)1, (ftnlen)6);
	sigerr_("SPICE(UNKNOWNFRAMETYPE)", (ftnlen)23);
	chkout_("ROTGET", (ftnlen)6);
	return 0;
    }

/*     Make sure to clear outputs in case of a failure as defined in */
/*     in the header. */

    if (failed_() || ! (*found)) {
	cleard_(&c__9, rotate);
	*outfrm = 0;
	*found = FALSE_;
    }
    chkout_("ROTGET", (ftnlen)6);
    return 0;
} /* rotget_ */

