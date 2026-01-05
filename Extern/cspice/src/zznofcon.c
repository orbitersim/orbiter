/* zznofcon.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__14 = 14;
static integer c__1 = 1;

/* $Procedure  ZZNOFCON ( Create frame connection long error message ) */
/* Subroutine */ int zznofcon_(doublereal *et, integer *frame1, integer *
	endp1, integer *frame2, integer *endp2, char *errmsg, ftnlen 
	errmsg_len)
{
    /* System generated locals */
    integer i__1, i__2;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer), s_cmp(char *, char *, 
	    ftnlen, ftnlen);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    char name__[32*2];
    integer i__;
    char bname[32*2];
    extern /* Subroutine */ int etcal_(doublereal *, char *, ftnlen), chkin_(
	    char *, ftnlen);
    integer class__;
    extern /* Subroutine */ int repmc_(char *, char *, char *, char *, ftnlen,
	     ftnlen, ftnlen, ftnlen);
    integer endps[2];
    extern /* Subroutine */ int repmf_(char *, char *, doublereal *, integer *
	    , char *, char *, ftnlen, ftnlen, ftnlen, ftnlen);
    logical found;
    extern /* Subroutine */ int repmi_(char *, char *, integer *, char *, 
	    ftnlen, ftnlen, ftnlen);
    extern logical failed_(void);
    extern /* Subroutine */ int ckmeta_(integer *, char *, integer *, ftnlen);
    integer sclkid;
    logical havnam[2];
    integer frames[2], center, clssid;
    char phrase[600];
    extern /* Subroutine */ int frmnam_(integer *, char *, ftnlen), frinfo_(
	    integer *, integer *, integer *, integer *, logical *);
    logical ckmiss;
    extern /* Subroutine */ int chkout_(char *, ftnlen);
    logical scmiss;
    extern /* Subroutine */ int suffix_(char *, integer *, char *, ftnlen, 
	    ftnlen);
    extern logical return_(void), zzsclk_(integer *, integer *);
    char timstr[35];

/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines.  Users should not call this routine directly due */
/*     to the volatile nature of this routine. */

/*     Create an informative long error message for cases where the */
/*     frame system signals a SPICE(NOFRAMECONNECT) error. */

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
/*     TIME */

/* $ Keywords */

/*     FRAMES */
/*     PRIVATE */
/*     UTILITY */

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

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     ET         I   Epoch. */
/*     FRAME1     I   "From" frame ID code. */
/*     ENDP1      I   "From" path endpoint frame ID code. */
/*     FRAME2     I   "To" frame ID code. */
/*     ENDP2      I   "To" path endpoint frame ID code. */
/*     ERRMSG     O   Long error message. */

/* $ Detailed_Input */

/*     ET             Epoch of frame transformation, expressed as */
/*                    seconds past J2000 TDB. */

/*     FRAME1         Frame ID code of frame at start of first path. */

/*     ENDP1          Frame ID code of frame at end of first path; */
/*                    this frame is the last node that could be */
/*                    reached from the frame designated by FRAME1. */

/*     FRAME2         Frame ID code of frame at start of second path. */

/*     ENDP2          Frame ID code of frame at end of second path; */
/*                    this frame is the last node that could be */
/*                    reached from the frame designated by FRAME2. */

/* $ Detailed_Output */

/*     ERRMSG         Long error message specifying computable */
/*                    frame paths, indications of missing SCLK */
/*                    or CK data, and optionally, debugging hints. */

/*                    The rules for formation of this message are: */

/*                       1) State the epoch. */

/*                       2) State the names of the frames for which */
/*                          a connection was attempted, if these */
/*                          names are available. */

/*                       3) State the names of the frames at the */
/*                          endpoints of both paths, if these */
/*                          names are available. */

/*                          Omit this portion of the message for any */
/*                          path of length one: in other words, if a */
/*                          frame and path endpoint coincide, omit the */
/*                          clause stating the frame can be connected to */
/*                          itself. */

/*                       4) For any path endpoint frame, if that */
/*                          frame is of CK type, indicate that */
/*                          CK and SCLK data must be loaded for */
/*                          that frame. */

/*                       5) For any path endpoint frame, if that */
/*                          frame is of CK type and SCLK data for the */
/*                          SCLK associated with that frame are not */
/*                          available, indicate this problem, along with */
/*                          the CK and SCLK ID codes associated with */
/*                          this frame. */

/*                       6) If at least one path endpoint frame */
/*                          is of CK type, and all required SCLK data */
/*                          are present, include a closing message */
/*                          explaining how CK coverage may be inadequate */
/*                          and recommending use of CKBRIEF. */

/*                       7) If both path endpoint frames are of CK type, */
/*                          and required SCLK data are present for only */
/*                          one of these frames, include a closing */
/*                          message explaining how CK coverage may be */
/*                          inadequate for a frame for which SCLK data */
/*                          are available, and recommending use of */
/*                          CKBRIEF. */

/*                       8) Omit the closing message if no path */
/*                          endpoint CK frame has associated SCLK */
/*                          data. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1) If a call to FRINFO or NAMFRM signals an error, this routine */
/*        will not be able to create a long error message. The */
/*        caller will not be able to diagnose the frame connection */
/*        failure, since an error condition will already exist. */

/* $ Files */

/*     1) Each input frame ID argument will be mapped, if possible, */
/*        to a frame name. Any input frame ID that's not built in */
/*        must */

/* $ Particulars */

/*     This routine centralizes creation of a long error message for */
/*     frame connection failures. This routine should be called */
/*     from: */

/*        FRMCHG */
/*        REFCHG */
/*        ZZFRMCH0 */
/*        ZZFRMCH1 */
/*        ZZREFCH0 */
/*        ZZREFCH1 */

/* $ Examples */

/*     Below are some examples of messages created by this routine. */


/*    1)   At epoch 2.8149297000000E+08 TDB (2008 DEC 02 12:29:30.000 */
/*         TDB), there is insufficient information available to */
/*         transform from reference frame -82000 (CASSINI_SC_COORD) to */
/*         reference frame -41000 (MEX_SPACECRAFT). CASSINI_SC_COORD is */
/*         a CK frame; a CK file containing data for instrument or */
/*         structure -82000 at the epoch shown above, as well as a */
/*         corresponding SCLK kernel, must be loaded in order to use */
/*         this frame. Frame MEX_SPACECRAFT could be transformed to */
/*         frame -41001 (MEX_SC_REF). The latter is a CK frame; a CK */
/*         file containing data for instrument or structure -41001 at */
/*         the epoch shown above, as well as a corresponding SCLK */
/*         kernel, must be loaded in order to use this frame. Failure to */
/*         find required CK data could be due to one or more CK files */
/*         not having been loaded, or to the epoch shown above lying */
/*         within a coverage gap or beyond the coverage bounds of the */
/*         loaded CK files. It is also possible that no loaded CK file */
/*         has required angular velocity data for the input epoch, even */
/*         if a loaded CK does have attitude data for that epoch. You */
/*         can use CKBRIEF with the -dump option to display coverage */
/*         intervals of a CK file. */


/*    2)   At epoch 2.8149297000000E+08 TDB (2008 DEC 02 12:29:30.000 */
/*         TDB), there is insufficient information available to */
/*         transform from reference frame -82360 (CASSINI_ISS_NAC) to */
/*         reference frame 1 (J2000). Frame CASSINI_ISS_NAC could be */
/*         transformed to frame -82000 (CASSINI_SC_COORD). The latter is */
/*         a CK frame; a CK file containing data for instrument or */
/*         structure -82000 at the epoch shown above, as well as a */
/*         corresponding SCLK kernel, must be loaded in order to use */
/*         this frame. Failure to find required CK data could be due to */
/*         one or more CK files not having been loaded, or to the epoch */
/*         shown above lying within a coverage gap or beyond the */
/*         coverage bounds of the loaded CK files. It is also possible */
/*         that no loaded CK file has required angular velocity data for */
/*         the input epoch, even if a loaded CK does have attitude data */
/*         for that epoch. You can use CKBRIEF with the -dump option to */


/*     3)  At epoch 2.8149297000000E+08 TDB (2008 DEC 02 12:29:30.000 */
/*         TDB), there is insufficient information available to */
/*         transform from reference frame -82000 (CASSINI_SC_COORD) to */
/*         reference frame -41000 (MEX_SPACECRAFT). CASSINI_SC_COORD is */
/*         a CK frame; a CK file containing data for instrument or */
/*         structure -82000 at the epoch shown above, as well as a */
/*         corresponding SCLK kernel, must be loaded in order to use */
/*         this frame. No SCLK kernel for instrument or structure */
/*         -82000, with corresponding SCLK ID -82, is currently loaded. */
/*         Frame MEX_SPACECRAFT could be transformed to frame -41001 */
/*         (MEX_SC_REF). The latter is a CK frame; a CK file containing */
/*         data for instrument or structure -41001 at the epoch shown */
/*         above, as well as a corresponding SCLK kernel, must be loaded */
/*         in order to use this frame. No SCLK kernel for instrument or */
/*         structure -41001, with corresponding SCLK ID -41, is */
/*         currently loaded. */


/*     4)  At epoch 2.8149297000000E+08 TDB (2008 DEC 02 12:29:30.000 */
/*         TDB), there is insufficient information available to */
/*         transform from reference frame -82000 (CASSINI_SC_COORD) to */
/*         reference frame -41000 (MEX_SPACECRAFT). CASSINI_SC_COORD is */
/*         a CK frame; a CK file containing data for instrument or */
/*         structure -82000 at the epoch shown above, as well as a */
/*         corresponding SCLK kernel, must be loaded in order to use */
/*         this frame. Frame MEX_SPACECRAFT could be transformed to */
/*         frame -41001 (MEX_SC_REF). The latter is a CK frame; a CK */
/*         file containing data for instrument or structure -41001 at */
/*         the epoch shown above, as well as a corresponding SCLK */
/*         kernel, must be loaded in order to use this frame. No SCLK */
/*         kernel for instrument or structure -41001, with corresponding */
/*         SCLK ID -41, is currently loaded. For a CK frame for which */
/*         the corresponding SCLK kernel has been loaded, failure to */
/*         find required CK data could be due to one or more CK files */
/*         not having been loaded, or to the epoch shown above lying */
/*         within a coverage gap or beyond the coverage bounds of the */
/*         loaded CK files. It is also possible that no loaded CK file */
/*         has required angular velocity data for the input epoch, even */
/*         if a loaded CK does have attitude data for that epoch. You */
/*         can use CKBRIEF with the -dump option to display coverage */
/*         intervals of a CK file. */

/* $ Restrictions */

/*     1) This is a private routine. SPICE user applications should not */
/*        call this routine. */

/*     2) See exception (1) above. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman   (JPL) */

/* $ Version */

/* -    SPICELIB Version 2.0.0, 09-SEP-2013 (NJB) */

/*        Long error message for missing CK data now */
/*        mentions the possibility of missing angular */
/*        velocity data. */

/* -    SPICELIB Version 1.0.0, 14-DEC-2008 (NJB) */

/* -& */
/* $ Index_Entries */

/*     create error message for frame connection failure */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     Local variables */


/*     Because this routine might cause a SPICE error to be */
/*     signaled, we have to check in. */

    if (return_()) {
	return 0;
    }
    chkin_("ZZNOFCON", (ftnlen)8);

/*     Capture input IDs in arrays. */

    frames[0] = *frame1;
    frames[1] = *frame2;
    endps[0] = *endp1;
    endps[1] = *endp2;

/*     The flags CKMISS and SCMISS are used, respectively, to */
/*     record whether any CK lookup failed due to missing CK */
/*     data or missing SCLK data. Each of these flags is turned */
/*     on if at least one lookup failed due to the indicated */
/*     cause. */

    ckmiss = FALSE_;
    scmiss = FALSE_;

/*     Get a string representation of the transformation epoch. */

    etcal_(et, timstr, (ftnlen)35);

/*     Get the names of the participating frames, if available. */

    frmnam_(frames, name__, (ftnlen)32);
    frmnam_(&frames[1], name__ + 32, (ftnlen)32);
    frmnam_(endps, bname, (ftnlen)32);
    frmnam_(&endps[1], bname + 32, (ftnlen)32);
    if (failed_()) {
	chkout_("ZZNOFCON", (ftnlen)8);
	return 0;
    }
    for (i__ = 1; i__ <= 2; ++i__) {
	if (s_cmp(name__ + (((i__1 = i__ - 1) < 2 && 0 <= i__1 ? i__1 : 
		s_rnge("name", i__1, "zznofcon_", (ftnlen)383)) << 5), " ", (
		ftnlen)32, (ftnlen)1) == 0) {
	    s_copy(name__ + (((i__1 = i__ - 1) < 2 && 0 <= i__1 ? i__1 : 
		    s_rnge("name", i__1, "zznofcon_", (ftnlen)385)) << 5), 
		    "Name not available", (ftnlen)32, (ftnlen)18);
	    havnam[(i__1 = i__ - 1) < 2 && 0 <= i__1 ? i__1 : s_rnge("havnam",
		     i__1, "zznofcon_", (ftnlen)386)] = FALSE_;
	} else {
	    havnam[(i__1 = i__ - 1) < 2 && 0 <= i__1 ? i__1 : s_rnge("havnam",
		     i__1, "zznofcon_", (ftnlen)388)] = TRUE_;
	}
	if (s_cmp(bname + (((i__1 = i__ - 1) < 2 && 0 <= i__1 ? i__1 : s_rnge(
		"bname", i__1, "zznofcon_", (ftnlen)391)) << 5), " ", (ftnlen)
		32, (ftnlen)1) == 0) {
	    s_copy(bname + (((i__1 = i__ - 1) < 2 && 0 <= i__1 ? i__1 : 
		    s_rnge("bname", i__1, "zznofcon_", (ftnlen)392)) << 5), 
		    "Name not available", (ftnlen)32, (ftnlen)18);
	}
    }
    s_copy(errmsg, "At epoch # TDB (# TDB), there is insufficient informatio"
	    "n available to transform from reference frame # (@) to reference"
	    " frame # (@).", errmsg_len, (ftnlen)133);
    repmf_(errmsg, "#", et, &c__14, "E", errmsg, errmsg_len, (ftnlen)1, (
	    ftnlen)1, errmsg_len);
    repmc_(errmsg, "#", timstr, errmsg, errmsg_len, (ftnlen)1, (ftnlen)35, 
	    errmsg_len);
    for (i__ = 1; i__ <= 2; ++i__) {
	repmi_(errmsg, "#", &frames[(i__1 = i__ - 1) < 2 && 0 <= i__1 ? i__1 :
		 s_rnge("frames", i__1, "zznofcon_", (ftnlen)408)], errmsg, 
		errmsg_len, (ftnlen)1, errmsg_len);
	repmc_(errmsg, "@", name__ + (((i__1 = i__ - 1) < 2 && 0 <= i__1 ? 
		i__1 : s_rnge("name", i__1, "zznofcon_", (ftnlen)409)) << 5), 
		errmsg, errmsg_len, (ftnlen)1, (ftnlen)32, errmsg_len);
    }

/*     For any frame graph longer than a single point, tell the user */
/*     the endpoint of the frame connection graph originating */
/*     at that frame. */

    for (i__ = 1; i__ <= 2; ++i__) {
	if (frames[(i__1 = i__ - 1) < 2 && 0 <= i__1 ? i__1 : s_rnge("frames",
		 i__1, "zznofcon_", (ftnlen)420)] != endps[(i__2 = i__ - 1) < 
		2 && 0 <= i__2 ? i__2 : s_rnge("endps", i__2, "zznofcon_", (
		ftnlen)420)]) {
	    s_copy(phrase, "Frame # could be transformed to frame # (@).", (
		    ftnlen)600, (ftnlen)44);
	    if (havnam[(i__1 = i__ - 1) < 2 && 0 <= i__1 ? i__1 : s_rnge(
		    "havnam", i__1, "zznofcon_", (ftnlen)424)]) {
		repmc_(phrase, "#", name__ + (((i__1 = i__ - 1) < 2 && 0 <= 
			i__1 ? i__1 : s_rnge("name", i__1, "zznofcon_", (
			ftnlen)425)) << 5), phrase, (ftnlen)600, (ftnlen)1, (
			ftnlen)32, (ftnlen)600);
	    } else {
		repmi_(phrase, "#", &frames[(i__1 = i__ - 1) < 2 && 0 <= i__1 
			? i__1 : s_rnge("frames", i__1, "zznofcon_", (ftnlen)
			427)], phrase, (ftnlen)600, (ftnlen)1, (ftnlen)600);
	    }
	    repmi_(phrase, "#", &endps[(i__1 = i__ - 1) < 2 && 0 <= i__1 ? 
		    i__1 : s_rnge("endps", i__1, "zznofcon_", (ftnlen)430)], 
		    phrase, (ftnlen)600, (ftnlen)1, (ftnlen)600);
	    repmc_(phrase, "@", bname + (((i__1 = i__ - 1) < 2 && 0 <= i__1 ? 
		    i__1 : s_rnge("bname", i__1, "zznofcon_", (ftnlen)431)) <<
		     5), phrase, (ftnlen)600, (ftnlen)1, (ftnlen)32, (ftnlen)
		    600);
	    suffix_(phrase, &c__1, errmsg, (ftnlen)600, errmsg_len);

/*           The error messages below are appended only if they're not */
/*           redundant. */

	    if (i__ == 1 || endps[1] != endps[0]) {

/*              For each endpoint frame, if that frame is of CK type, */
/*              indicate the instrument ID for which CK data are needed. */

		frinfo_(&endps[(i__1 = i__ - 1) < 2 && 0 <= i__1 ? i__1 : 
			s_rnge("endps", i__1, "zznofcon_", (ftnlen)444)], &
			center, &class__, &clssid, &found);
		if (failed_()) {
		    chkout_("ZZNOFCON", (ftnlen)8);
		    return 0;
		}
		if (found) {
		    if (class__ == 3) {
			s_copy(phrase, "The latter is a CK frame; a CK file "
				"containing data for instrument or structure "
				"# at the epoch shown above, as well as a cor"
				"responding SCLK kernel, must be loaded in or"
				"der to use this frame.", (ftnlen)600, (ftnlen)
				190);
			repmi_(phrase, "#", &clssid, phrase, (ftnlen)600, (
				ftnlen)1, (ftnlen)600);
			suffix_(phrase, &c__1, errmsg, (ftnlen)600, 
				errmsg_len);

/*                    Find out whether we have SCLK data for this */
/*                    CK ID. */

			ckmeta_(&clssid, "SCLK", &sclkid, (ftnlen)4);
			if (! zzsclk_(&clssid, &sclkid)) {
			    scmiss = TRUE_;
			    s_copy(phrase, "No SCLK kernel for instrument or"
				    " structure #, with corresponding SCLK ID"
				    " #, is currently loaded.", (ftnlen)600, (
				    ftnlen)96);
			    repmi_(phrase, "#", &clssid, phrase, (ftnlen)600, 
				    (ftnlen)1, (ftnlen)600);
			    repmi_(phrase, "#", &sclkid, phrase, (ftnlen)600, 
				    (ftnlen)1, (ftnlen)600);
			    suffix_(phrase, &c__1, errmsg, (ftnlen)600, 
				    errmsg_len);
			} else {

/*                       If we got here and have the SCLK data, then */
/*                       we don't have CK data. */

			    ckmiss = TRUE_;
			}
		    }

/*                 End of CK frame case. */

		}

/*              End of "info found" case. */

	    }

/*           End of distinct frame case. */

	} else if (i__ == 1 || endps[1] != endps[0]) {

/*           The error messages below are appended only if they're not */
/*           redundant. */

/*           This graph has length one. If the frame comprising */
/*           this graph is a CK frame, generate a phrase */
/*           indicating the needed CK data. */

	    frinfo_(&frames[(i__1 = i__ - 1) < 2 && 0 <= i__1 ? i__1 : s_rnge(
		    "frames", i__1, "zznofcon_", (ftnlen)520)], &center, &
		    class__, &clssid, &found);
	    if (failed_()) {
		chkout_("ZZNOFCON", (ftnlen)8);
		return 0;
	    }
	    if (found) {
		if (class__ == 3) {
		    s_copy(phrase, "# is a CK frame; a CK file containing da"
			    "ta for instrument or structure # at the epoch sh"
			    "own above, as well as a corresponding SCLK kerne"
			    "l, must be loaded in order to use this frame.", (
			    ftnlen)600, (ftnlen)181);
		    if (havnam[(i__1 = i__ - 1) < 2 && 0 <= i__1 ? i__1 : 
			    s_rnge("havnam", i__1, "zznofcon_", (ftnlen)539)])
			     {
			repmc_(phrase, "#", name__ + (((i__1 = i__ - 1) < 2 &&
				 0 <= i__1 ? i__1 : s_rnge("name", i__1, 
				"zznofcon_", (ftnlen)540)) << 5), phrase, (
				ftnlen)600, (ftnlen)1, (ftnlen)32, (ftnlen)
				600);
		    } else {
			repmi_(phrase, "#", &frames[(i__1 = i__ - 1) < 2 && 0 
				<= i__1 ? i__1 : s_rnge("frames", i__1, "zzn"
				"ofcon_", (ftnlen)542)], phrase, (ftnlen)600, (
				ftnlen)1, (ftnlen)600);
		    }
		    repmi_(phrase, "#", &clssid, phrase, (ftnlen)600, (ftnlen)
			    1, (ftnlen)600);
		    suffix_(phrase, &c__1, errmsg, (ftnlen)600, errmsg_len);

/*                 Find out whether we have SCLK data for this */
/*                 CK ID. */

		    ckmeta_(&clssid, "SCLK", &sclkid, (ftnlen)4);
		    if (! zzsclk_(&clssid, &sclkid)) {
			scmiss = TRUE_;
			s_copy(phrase, "No SCLK kernel for instrument or str"
				"ucture #, with corresponding SCLK ID #, is c"
				"urrently loaded.", (ftnlen)600, (ftnlen)96);
			repmi_(phrase, "#", &clssid, phrase, (ftnlen)600, (
				ftnlen)1, (ftnlen)600);
			repmi_(phrase, "#", &sclkid, phrase, (ftnlen)600, (
				ftnlen)1, (ftnlen)600);
			suffix_(phrase, &c__1, errmsg, (ftnlen)600, 
				errmsg_len);
		    } else {

/*                    If we got here and have the SCLK data, then */
/*                    we don't have CK data. */

			ckmiss = TRUE_;
		    }
		}

/*              End of CK frame case. */

	    }

/*           End of "info found" case. */

	}

/*        End of path length case. */

    }

/*     End of path loop. */

    if (ckmiss) {

/*        At least one lookup failed due to missing CK data. */

/*        The informational message we include depends on whether we */
/*        also lack SCLK data. */

	if (scmiss) {

/*           We lack SCLK data for one frame and CK data for another. */

	    s_copy(phrase, "For a CK frame for which the corresponding SCLK "
		    "kernel has been loaded, failure to find required CK data"
		    " could be due to one or more CK files not having been lo"
		    "aded, or to the epoch shown above lying within a coverag"
		    "e gap or beyond the coverage bounds of the loaded CK fil"
		    "es. It is also possible that no loaded CK file has requi"
		    "red angular velocity data for the input epoch, even if a"
		    " loaded CK does have attitude data for that epoch. You c"
		    "an use CKBRIEF with the -dump option to display coverage"
		    " intervals of a CK file.", (ftnlen)600, (ftnlen)520);
	} else {

/*           We have SCLK data but lack CK data. */

	    s_copy(phrase, "Failure to find required CK data could be due to"
		    " one or more CK files not having been loaded, or to the "
		    "epoch shown above lying within a coverage gap or beyond "
		    "the coverage bounds of the loaded CK files. It is also p"
		    "ossible that no loaded CK file has required angular velo"
		    "city data for the input epoch, even if a loaded CK does "
		    "have attitude data for that epoch. You can use CKBRIEF w"
		    "ith the -dump option to display coverage intervals of a "
		    "CK file.", (ftnlen)600, (ftnlen)448);
	}
	suffix_(phrase, &c__1, errmsg, (ftnlen)600, errmsg_len);
    }
    chkout_("ZZNOFCON", (ftnlen)8);
    return 0;
} /* zznofcon_ */

