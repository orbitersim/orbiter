/* refchg.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__2 = 2;

/* $Procedure REFCHG (Reference frame Change) */
/* Subroutine */ int refchg_(integer *frame1, integer *frame2, doublereal *et,
	 doublereal *rotate)
{
    /* System generated locals */
    integer i__1, i__2, i__3, i__4, i__5, i__6, i__7;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer);

    /* Local variables */
    integer node;
    logical done;
    integer cent, this__;
    extern /* Subroutine */ int zznofcon_(doublereal *, integer *, integer *, 
	    integer *, integer *, char *, ftnlen);
    integer i__, j, frame[10];
    extern /* Subroutine */ int chkin_(char *, ftnlen), ident_(doublereal *);
    integer class__;
    logical found;
    integer relto;
    extern /* Subroutine */ int xpose_(doublereal *, doublereal *), zzrxr_(
	    doublereal *, integer *, doublereal *);
    extern logical failed_(void);
    integer cmnode;
    extern integer isrchi_(integer *, integer *, integer *);
    integer clssid;
    extern /* Subroutine */ int frinfo_(integer *, integer *, integer *, 
	    integer *, logical *);
    logical gotone;
    char errmsg[1840];
    extern /* Subroutine */ int chkout_(char *, ftnlen), setmsg_(char *, 
	    ftnlen), errint_(char *, integer *, ftnlen), sigerr_(char *, 
	    ftnlen), rotget_(integer *, doublereal *, doublereal *, integer *,
	     logical *);
    extern logical return_(void);
    doublereal tmprot[9]	/* was [3][3] */;
    integer inc, get;
    doublereal rot[126]	/* was [3][3][14] */;
    integer put;
    doublereal rot2[18]	/* was [3][3][2] */;

/* $ Abstract */

/*     Return the transformation matrix from one */
/*     frame to another. */

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

/*     FRAMES */

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


/*     Include File:  SPICELIB Error Handling Parameters */

/*        errhnd.inc  Version 2    18-JUN-1997 (WLT) */

/*           The size of the long error message was */
/*           reduced from 25*80 to 23*80 so that it */
/*           will be accepted by the Microsoft Power Station */
/*           FORTRAN compiler which has an upper bound */
/*           of 1900 for the length of a character string. */

/*        errhnd.inc  Version 1    29-JUL-1997 (NJB) */



/*     Maximum length of the long error message: */


/*     Maximum length of the short error message: */


/*     End Include File:  SPICELIB Error Handling Parameters */

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
/*     FRAME1     I   the frame id-code for some reference frame */
/*     FRAME2     I   the frame id-code for some reference frame */
/*     ET         I   an epoch in TDB seconds past J2000. */
/*     ROTATE     O   a rotation matrix */

/* $ Detailed_Input */

/*     FRAME1   is the frame id-code in which some positions */
/*              are known. */

/*     FRAME2   is the frame id-code for some frame in which you */
/*              would like to represent positions. */

/*     ET       is the epoch at which to compute the transformation */
/*              matrix. This epoch should be in TDB seconds past */
/*              the ephemeris epoch of J2000. */

/* $ Detailed_Output */

/*     ROTATE   is a 3 x 3 rotation matrix that can be used to */
/*              transform positions relative to the frame */
/*              corresponding to frame FRAME2 to positions relative */
/*              to the frame FRAME2. More explicitly, if POS is */
/*              the position of some object relative to the */
/*              reference frame of FRAME1 then POS2 is the position */
/*              of the same object relative to FRAME2 where POS2 is */
/*              computed via the subroutine call below */

/*                 CALL MXV ( ROTATE, POS, POS2 ) */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If either of the reference frames is unrecognized, the error */
/*         SPICE(UNKNOWNFRAME) is signaled. */

/*     2)  If the auxiliary information needed to compute a non-inertial */
/*         frame is not available, an error is signaled */
/*         by a routine in the call tree of this routine. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine allows you to compute the rotation matrix */
/*     between two reference frames. */

/* $ Examples */

/*     Suppose that you have a position POS1 at epoch ET */
/*     relative to  FRAME1 and wish to determine its representation */
/*     POS2 relative to FRAME2. The following subroutine calls */
/*     would suffice to make this rotation. */

/*        CALL REFCHG ( FRAME1, FRAME2, ET,   ROTATE ) */
/*        CALL MXV    ( ROTATE, POS1,   POS2 ) */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 2.1.0, 08-OCT-2021 (JDR) (NJB) */

/*        Bug fix: added calls to FAILED after each call to */
/*        FRINFO and to ROTGET. */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 2.0.0, 14-DEC-2008 (NJB) */

/*        Upgraded long error message associated with frame */
/*        connection failure. */

/* -    SPICELIB Version 1.2.0, 26-APR-2004 (NJB) */

/*        Another typo was corrected in the long error message, and */
/*        in a comment. */

/* -    SPICELIB Version 1.1.0, 23-MAY-2000 (WLT) */

/*        A typo was corrected in the long error message. */

/* -    SPICELIB Version 1.0.0, 09-JUL-1998 (WLT) */

/* -& */
/* $ Index_Entries */

/*     Rotate positions from one frame to another */

/* -& */

/*     SPICE functions */


/*     Local Parameters */


/*     The root of all reference frames is J2000 (Frame ID = 1). */


/*     Local Variables */


/*     ROT contains the rotations from FRAME1 to FRAME2 */
/*     ROT(1...3,1...3,I) has the rotation from FRAME(I) */
/*     to FRAME(I+1).  We make extra room in ROT because we */
/*     plan to add rotations beyond the obvious chain from */
/*     FRAME1 to a root node. */


/*     ROT2 is used to store intermediate rotation from */
/*     FRAME2 to some node in the chain from FRAME1 to PCK or */
/*     INERTL frames. */


/*     FRAME contains the frames we transform from in going from */
/*     FRAME1 to FRAME2.  FRAME(1) = FRAME1 by  construction. */


/*     NODE counts the number of rotations needed to go */
/*     from FRAME1 to FRAME2. */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }
    chkin_("REFCHG", (ftnlen)6);

/*     Do the obvious thing first.  If FRAME1 and FRAME2 are the */
/*     same then we simply return the identity matrix. */

    if (*frame1 == *frame2) {
	ident_(rotate);
	chkout_("REFCHG", (ftnlen)6);
	return 0;
    }

/*     Now perform the obvious check to make sure that both */
/*     frames are recognized. */

    frinfo_(frame1, &cent, &class__, &clssid, &found);
    if (failed_()) {
	chkout_("REFCHG", (ftnlen)6);
	return 0;
    }
    if (! found) {
	setmsg_("The number # is not a recognized id-code for a reference fr"
		"ame. ", (ftnlen)64);
	errint_("#", frame1, (ftnlen)1);
	sigerr_("SPICE(UNKNOWNFRAME)", (ftnlen)19);
	chkout_("REFCHG", (ftnlen)6);
	return 0;
    }
    frinfo_(frame2, &cent, &class__, &clssid, &found);
    if (failed_()) {
	chkout_("REFCHG", (ftnlen)6);
	return 0;
    }
    if (! found) {
	setmsg_("The number # is not a recognized id-code for a reference fr"
		"ame. ", (ftnlen)64);
	errint_("#", frame2, (ftnlen)1);
	sigerr_("SPICE(UNKNOWNFRAME)", (ftnlen)19);
	chkout_("REFCHG", (ftnlen)6);
	return 0;
    }
    node = 1;
    frame[(i__1 = node - 1) < 10 && 0 <= i__1 ? i__1 : s_rnge("frame", i__1, 
	    "refchg_", (ftnlen)304)] = *frame1;
    found = TRUE_;

/*     Follow the chain of rotations until we run into */
/*     one that rotates to J2000 (frame id = 1) or we hit FRAME2. */

    while(frame[(i__1 = node - 1) < 10 && 0 <= i__1 ? i__1 : s_rnge("frame", 
	    i__1, "refchg_", (ftnlen)310)] != 1 && node < 10 && frame[(i__2 = 
	    node - 1) < 10 && 0 <= i__2 ? i__2 : s_rnge("frame", i__2, "refc"
	    "hg_", (ftnlen)310)] != *frame2 && found) {

/*        Find out what rotation is available for this */
/*        frame. */

	rotget_(&frame[(i__1 = node - 1) < 10 && 0 <= i__1 ? i__1 : s_rnge(
		"frame", i__1, "refchg_", (ftnlen)318)], et, &rot[(i__2 = (
		node * 3 + 1) * 3 - 12) < 126 && 0 <= i__2 ? i__2 : s_rnge(
		"rot", i__2, "refchg_", (ftnlen)318)], &frame[(i__3 = node) < 
		10 && 0 <= i__3 ? i__3 : s_rnge("frame", i__3, "refchg_", (
		ftnlen)318)], &found);
	if (failed_()) {
	    chkout_("REFCHG", (ftnlen)6);
	    return 0;
	}
	if (found) {

/*           We found a rotation matrix.  ROT(1,1,NODE) */
/*           now contains the rotation from FRAME(NODE) */
/*           to FRAME(NODE+1).  We need to look up the information */
/*           for the next NODE. */

	    ++node;
	}
    }
    done = frame[(i__1 = node - 1) < 10 && 0 <= i__1 ? i__1 : s_rnge("frame", 
	    i__1, "refchg_", (ftnlen)339)] == 1 || frame[(i__2 = node - 1) < 
	    10 && 0 <= i__2 ? i__2 : s_rnge("frame", i__2, "refchg_", (ftnlen)
	    339)] == *frame2 || ! found;
    while(! done) {

/*        The only way to get to this point is to have run out of */
/*        room in the array of reference frame rotation */
/*        buffers.  We will now build the rotation from */
/*        the previous NODE to whatever the next node in the */
/*        chain is.  We'll do this until we get to one of the */
/*        root classes or we run into FRAME2. */

	rotget_(&frame[(i__1 = node - 1) < 10 && 0 <= i__1 ? i__1 : s_rnge(
		"frame", i__1, "refchg_", (ftnlen)353)], et, &rot[(i__2 = (
		node * 3 + 1) * 3 - 12) < 126 && 0 <= i__2 ? i__2 : s_rnge(
		"rot", i__2, "refchg_", (ftnlen)353)], &relto, &found);
	if (failed_()) {
	    chkout_("REFCHG", (ftnlen)6);
	    return 0;
	}
	if (found) {

/*           Recall that ROT(1,1,NODE-1) contains the rotation */
/*           from FRAME(NODE-1) to FRAME(NODE).  We are going to replace */
/*           FRAME(NODE) with the frame indicated by RELTO.  This means */
/*           that ROT(1,1,NODE-1) should be replaced with the */
/*           rotation from FRAME(NODE) to RELTO. */

	    frame[(i__1 = node - 1) < 10 && 0 <= i__1 ? i__1 : s_rnge("frame",
		     i__1, "refchg_", (ftnlen)368)] = relto;
	    zzrxr_(&rot[(i__1 = ((node - 1) * 3 + 1) * 3 - 12) < 126 && 0 <= 
		    i__1 ? i__1 : s_rnge("rot", i__1, "refchg_", (ftnlen)369)]
		    , &c__2, tmprot);
	    for (i__ = 1; i__ <= 3; ++i__) {
		for (j = 1; j <= 3; ++j) {
		    rot[(i__1 = i__ + (j + (node - 1) * 3) * 3 - 13) < 126 && 
			    0 <= i__1 ? i__1 : s_rnge("rot", i__1, "refchg_", 
			    (ftnlen)373)] = tmprot[(i__2 = i__ + j * 3 - 4) < 
			    9 && 0 <= i__2 ? i__2 : s_rnge("tmprot", i__2, 
			    "refchg_", (ftnlen)373)];
		}
	    }
	}

/*        We are done if the class of the last frame is J2000 */
/*        or if the last frame is FRAME2 or if we simply couldn't get */
/*        another rotation. */

	done = frame[(i__1 = node - 1) < 10 && 0 <= i__1 ? i__1 : s_rnge(
		"frame", i__1, "refchg_", (ftnlen)383)] == 1 || frame[(i__2 = 
		node - 1) < 10 && 0 <= i__2 ? i__2 : s_rnge("frame", i__2, 
		"refchg_", (ftnlen)383)] == *frame2 || ! found;
    }

/*     Right now we have the following situation.  We have in hand */
/*     a collection of rotations between frames. (Assuming */
/*     that is that NODE .GT. 1.  If NODE .EQ. 1 then we have */
/*     no rotations computed yet. */


/*     ROT(1...3, 1...3, 1    )    rotates FRAME1   to FRAME(2) */
/*     ROT(1...3, 1...3, 2    )    rotates FRAME(2) to FRAME(3) */
/*     ROT(1...3, 1...3, 3    )    rotates FRAME(3) to FRAME(4) */
/*        . */
/*        . */
/*        . */
/*     ROT(1...3, 1...3, NODE-1 )  rotates FRAME(NODE-1) */
/*                                   to         FRAME(NODE) */


/*     One of the following situations is true. */

/*     1)  FRAME(NODE) is the root of all frames, J2000. */

/*     2)  FRAME(NODE) is the same as FRAME2 */

/*     3)  There is no rotation from FRAME(NODE) to another */
/*         more fundamental frame.  The chain of rotations */
/*         from FRAME1 stops at FRAME(NODE).  This means that the */
/*         "frame atlas" is incomplete because we can't get to the */
/*         root frame. */

/*     We now have to do essentially the same thing for FRAME2. */

    if (frame[(i__1 = node - 1) < 10 && 0 <= i__1 ? i__1 : s_rnge("frame", 
	    i__1, "refchg_", (ftnlen)421)] == *frame2) {

/*        We can handle this one immediately with the private routine */
/*        ZZRXR which multiplies a series of matrices. */

	i__1 = node - 1;
	zzrxr_(rot, &i__1, rotate);
	chkout_("REFCHG", (ftnlen)6);
	return 0;
    }

/*     We didn't luck out above.  So we follow the chain of */
/*     rotation for FRAME2.  Note that at the moment the */
/*     chain of rotations from FRAME2 to other frames */
/*     does not share a node in the chain for FRAME1. */
/*    ( GOTONE = .FALSE. ) . */

    this__ = *frame2;
    gotone = FALSE_;

/*     First see if there is any chain to follow. */

    done = this__ == 1;

/*     Set up the matrices ROT2(,,1) and ROT(,,2)  and set up */
/*     PUT and GET pointers so that we know where to GET the partial */
/*     rotation from and where to PUT partial results. */

    if (! done) {
	put = 1;
	get = 1;
	inc = 1;
    }

/*     Follow the chain of rotations until we run into */
/*     one that rotates to the root frame or we land in the */
/*     chain of nodes for FRAME1. */

/*     Note that this time we will simply keep track of the full */
/*     rotation from FRAME2 to the last node. */

    while(! done) {

/*        Find out what rotation is available for this */
/*        frame. */

	if (this__ == *frame2) {

/*           This is the first pass, just put the rotation */
/*           directly into ROT2(,,PUT). */

	    rotget_(&this__, et, &rot2[(i__1 = (put * 3 + 1) * 3 - 12) < 18 &&
		     0 <= i__1 ? i__1 : s_rnge("rot2", i__1, "refchg_", (
		    ftnlen)478)], &relto, &found);
	    if (failed_()) {
		chkout_("REFCHG", (ftnlen)6);
		return 0;
	    }
	    if (found) {
		this__ = relto;
		get = put;
		put += inc;
		inc = -inc;
		cmnode = isrchi_(&this__, &node, frame);
		gotone = cmnode > 0;
	    }
	} else {

/*           Fetch the rotation into a temporary spot TMPROT */

	    rotget_(&this__, et, tmprot, &relto, &found);
	    if (failed_()) {
		chkout_("REFCHG", (ftnlen)6);
		return 0;
	    }
	    if (found) {

/*              Next multiply TMPROT on the right by the last partial */
/*              product (in ROT2(,,GET) ).  We do this in line. */

		for (i__ = 1; i__ <= 3; ++i__) {
		    for (j = 1; j <= 3; ++j) {
			rot2[(i__1 = i__ + (j + put * 3) * 3 - 13) < 18 && 0 
				<= i__1 ? i__1 : s_rnge("rot2", i__1, "refch"
				"g_", (ftnlen)513)] = tmprot[(i__2 = i__ - 1) <
				 9 && 0 <= i__2 ? i__2 : s_rnge("tmprot", 
				i__2, "refchg_", (ftnlen)513)] * rot2[(i__3 = 
				(j + get * 3) * 3 - 12) < 18 && 0 <= i__3 ? 
				i__3 : s_rnge("rot2", i__3, "refchg_", (
				ftnlen)513)] + tmprot[(i__4 = i__ + 2) < 9 && 
				0 <= i__4 ? i__4 : s_rnge("tmprot", i__4, 
				"refchg_", (ftnlen)513)] * rot2[(i__5 = (j + 
				get * 3) * 3 - 11) < 18 && 0 <= i__5 ? i__5 : 
				s_rnge("rot2", i__5, "refchg_", (ftnlen)513)] 
				+ tmprot[(i__6 = i__ + 5) < 9 && 0 <= i__6 ? 
				i__6 : s_rnge("tmprot", i__6, "refchg_", (
				ftnlen)513)] * rot2[(i__7 = (j + get * 3) * 3 
				- 10) < 18 && 0 <= i__7 ? i__7 : s_rnge("rot2"
				, i__7, "refchg_", (ftnlen)513)];
		    }
		}

/*              Adjust GET and PUT so that GET points to the slots */
/*              where we just stored the result of our multiply and */
/*              so that PUT points to the next available storage */
/*              locations. */

		get = put;
		put += inc;
		inc = -inc;
		this__ = relto;
		cmnode = isrchi_(&this__, &node, frame);
		gotone = cmnode > 0;
	    }
	}

/*        See if we have a common node and determine whether or not */
/*        we are done with this loop. */

	done = this__ == 1 || gotone || ! found;
    }

/*     There are two possible scenarios.  Either the chain of */
/*     rotations from FRAME2 ran into a node in the chain for */
/*     FRAME1 or it didn't.  (The common node might very well be */
/*     the root node.)  If we didn't run into a common one, then */
/*     the two chains don't intersect and there is no way to */
/*     get from FRAME1 to FRAME2. */

    if (! gotone) {
	zznofcon_(et, frame1, &frame[(i__1 = node - 1) < 10 && 0 <= i__1 ? 
		i__1 : s_rnge("frame", i__1, "refchg_", (ftnlen)557)], frame2,
		 &this__, errmsg, (ftnlen)1840);
	if (failed_()) {

/*           We were unable to create the error message. This */
/*           unfortunate situation could arise if a frame kernel */
/*           is corrupted. */

	    chkout_("REFCHG", (ftnlen)6);
	    return 0;
	}

/*        The normal case: signal an error with a descriptive long */
/*        error message. */

	setmsg_(errmsg, (ftnlen)1840);
	sigerr_("SPICE(NOFRAMECONNECT)", (ftnlen)21);
	chkout_("REFCHG", (ftnlen)6);
	return 0;
    }

/*     Recall that we have the following. */

/*     ROT(1...3, 1...3, 1    )    rotates FRAME(1) to FRAME(2) */
/*     ROT(1...3, 1...3, 2    )    rotates FRAME(2) to FRAME(3) */
/*     ROT(1...3, 1...3, 3    )    rotates FRAME(3) to FRAME(4) */

/*     ROT(1...3, 1...3, CMNODE-1) rotates FRAME(CMNODE-1) */
/*                                   to         FRAME(CMNODE) */

/*     and that ROT2(1,1,GET) rotates from FRAME2 to CMNODE. */
/*     Hence the inverse of ROT2(1,1,GET) rotates from CMNODE */
/*     to FRAME2. */

/*     If we compute the inverse of ROT2 and store it in */
/*     the next available slot of ROT (.i.e. ROT(1,1,CMNODE) */
/*     we can simply apply our custom routine that multiplies a */
/*     sequence of rotation matrices together to get the */
/*     result from FRAME1 to FRAME2. */

    xpose_(&rot2[(i__1 = (get * 3 + 1) * 3 - 12) < 18 && 0 <= i__1 ? i__1 : 
	    s_rnge("rot2", i__1, "refchg_", (ftnlen)600)], &rot[(i__2 = (
	    cmnode * 3 + 1) * 3 - 12) < 126 && 0 <= i__2 ? i__2 : s_rnge(
	    "rot", i__2, "refchg_", (ftnlen)600)]);
    zzrxr_(rot, &cmnode, rotate);
    chkout_("REFCHG", (ftnlen)6);
    return 0;
} /* refchg_ */

