/* zzdsksph.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__3 = 3;

/* $Procedure ZZDSKSPH ( DSK, bounding spheres for target body ) */
/* Subroutine */ int zzdsksph_(integer *bodyid, integer *nsurf, integer *
	srflst, doublereal *minrad, doublereal *maxrad)
{
    /* Initialized data */

    static integer ctr[2] = { -1,-1 };
    static logical first = TRUE_;
    static integer prvfid = 0;
    static integer prvbod = 0;
    static integer prvlst[100] = { 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	    0,0,0,0,0,0,0,0,0,0,0,0,0 };
    static integer prvnls = -1;
    static doublereal svmaxr = -1.;
    static doublereal svminr = -1.;

    /* System generated locals */
    integer i__1;
    doublereal d__1;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer), i_dnnt(doublereal *), 
	    s_cmp(char *, char *, ftnlen, ftnlen);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    logical same;
    doublereal minr, maxr;
    extern /* Subroutine */ int zzdskbdc_();
    extern /* Subroutine */ int zzdskbbl_(integer *), zzdskchk_(integer *, 
	    logical *), zzdsksbd_(integer *), zzrecbox_(doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *), zzdskbss_(integer *);
    doublereal f;
    extern /* Subroutine */ int zzctruin_(integer *), zzdsksns_(U_fp, integer 
	    *, integer *, doublereal *, logical *);
    integer i__;
    extern /* Subroutine */ int chkin_(char *, ftnlen), repmc_(char *, char *,
	     char *, char *, ftnlen, ftnlen, ftnlen, ftnlen);
    extern doublereal dpmax_(void);
    logical found;
    extern /* Subroutine */ int repmi_(char *, char *, integer *, char *, 
	    ftnlen, ftnlen, ftnlen), movei_(integer *, integer *, integer *);
    extern doublereal vnorm_(doublereal *);
    extern logical failed_(void);
    doublereal re;
    integer dladsc[8], handle;
    extern /* Subroutine */ int cleard_(integer *, doublereal *);
    doublereal offmag, lt, rp, lx, ly, lz;
    integer framid;
    char frname[32];
    integer frclid;
    extern integer isrchi_(integer *, integer *, integer *);
    char errmsg[1840];
    doublereal boxctr[3];
    extern logical return_(void);
    doublereal boxrad, ctrmnr, dskdsc[24], midtim, offset[3], sgmaxr, sgminr;
    integer corsys, frcent, frclas, surfid;
    logical newlst, segfnd, update;
    extern /* Subroutine */ int setmsg_(char *, ftnlen), errint_(char *, 
	    integer *, ftnlen), sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), frinfo_(integer *, integer *, integer *, integer *, 
	    logical *), frmnam_(integer *, char *, ftnlen), spkgps_(integer *,
	     doublereal *, char *, integer *, doublereal *, doublereal *, 
	    ftnlen);

/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines. Users should not call this routine directly due to the */
/*     volatile nature of this routine. */

/*     Return radii of outer and inner bounding spheres for a given body */
/*     and surface list. The shape of the body is represented by DSK */
/*     data. The outputs of this routine are time-independent. */

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

/*     DSK */

/* $ Keywords */

/*     DSK */
/*     GEOMETRY */
/*     SURFACE */
/*     TOPOGRAPHY */

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


/*     Include file dla.inc */

/*     This include file declares parameters for DLA format */
/*     version zero. */

/*        Version 3.0.1 17-OCT-2016 (NJB) */

/*           Corrected comment: VERIDX is now described as a DAS */
/*           integer address rather than a d.p. address. */

/*        Version 3.0.0 20-JUN-2006 (NJB) */

/*           Changed name of parameter DSCSIZ to DLADSZ. */

/*        Version 2.0.0 09-FEB-2005 (NJB) */

/*           Changed descriptor layout to make backward pointer */
/*           first element.  Updated DLA format version code to 1. */

/*           Added parameters for format version and number of bytes per */
/*           DAS comment record. */

/*        Version 1.0.0 28-JAN-2004 (NJB) */


/*     DAS integer address of DLA version code. */


/*     Linked list parameters */

/*     Logical arrays (aka "segments") in a DAS linked array (DLA) file */
/*     are organized as a doubly linked list.  Each logical array may */
/*     actually consist of character, double precision, and integer */
/*     components.  A component of a given data type occupies a */
/*     contiguous range of DAS addresses of that type.  Any or all */
/*     array components may be empty. */

/*     The segment descriptors in a SPICE DLA (DAS linked array) file */
/*     are connected by a doubly linked list.  Each node of the list is */
/*     represented by a pair of integers acting as forward and backward */
/*     pointers.  Each pointer pair occupies the first two integers of a */
/*     segment descriptor in DAS integer address space.  The DLA file */
/*     contains pointers to the first integers of both the first and */
/*     last segment descriptors. */

/*     At the DLA level of a file format implementation, there is */
/*     no knowledge of the data contents.  Hence segment descriptors */
/*     provide information only about file layout (in contrast with */
/*     the DAF system).  Metadata giving specifics of segment contents */
/*     are stored within the segments themselves in DLA-based file */
/*     formats. */


/*     Parameter declarations follow. */

/*     DAS integer addresses of first and last segment linked list */
/*     pointer pairs.  The contents of these pointers */
/*     are the DAS addresses of the first integers belonging */
/*     to the first and last link pairs, respectively. */

/*     The acronyms "LLB" and "LLE" denote "linked list begin" */
/*     and "linked list end" respectively. */


/*     Null pointer parameter. */


/*     Segment descriptor parameters */

/*     Each segment descriptor occupies a contiguous */
/*     range of DAS integer addresses. */

/*        The segment descriptor layout is: */

/*           +---------------+ */
/*           | BACKWARD PTR  | Linked list backward pointer */
/*           +---------------+ */
/*           | FORWARD PTR   | Linked list forward pointer */
/*           +---------------+ */
/*           | BASE INT ADDR | Base DAS integer address */
/*           +---------------+ */
/*           | INT COMP SIZE | Size of integer segment component */
/*           +---------------+ */
/*           | BASE DP ADDR  | Base DAS d.p. address */
/*           +---------------+ */
/*           | DP COMP SIZE  | Size of d.p. segment component */
/*           +---------------+ */
/*           | BASE CHR ADDR | Base DAS character address */
/*           +---------------+ */
/*           | CHR COMP SIZE | Size of character segment component */
/*           +---------------+ */

/*     Parameters defining offsets for segment descriptor elements */
/*     follow. */


/*     Descriptor size: */


/*     Other DLA parameters: */


/*     DLA format version.  (This number is expected to occur very */
/*     rarely at integer address VERIDX in uninitialized DLA files.) */


/*     Characters per DAS comment record. */


/*     End of include file dla.inc */


/*     File: dsk.inc */


/*     Version 1.0.0 05-FEB-2016 (NJB) */

/*     Maximum size of surface ID list. */


/*     End of include file dsk.inc */


/*     Include file dskdsc.inc */

/*     This include file declares parameters for DSK segment descriptors. */

/* -       SPICELIB Version 1.0.0 08-FEB-2017 (NJB) */

/*           Updated version info. */

/*           22-JAN-2016 (NJB) */

/*              Added parameter for data class 2. Changed name of data */
/*              class 1 parameter. Corrected data class descriptions. */

/*           13-MAY-2010 (NJB) */

/*              Descriptor now contains two ID codes, one for the */
/*              surface, one for the associated ephemeris object. This */
/*              supports association of multiple surfaces with one */
/*              ephemeris object without creating file management */
/*              issues. */

/*              Room was added for coordinate system definition */
/*              parameters. */

/*               Flag arrays and model ID/component entries were deleted. */

/*            11-SEP-2008 (NJB) */


/*     DSK segment descriptors are implemented as an array of d.p. */
/*     numbers.  Note that each integer descriptor datum occupies one */
/*     d.p. value. */




/*     Segment descriptor parameters */

/*     Each segment descriptor occupies a contiguous */
/*     range of DAS d.p. addresses. */

/*        The DSK segment descriptor layout is: */

/*           +---------------------+ */
/*           | Surface ID code     | */
/*           +---------------------+ */
/*           | Center ID code      | */
/*           +---------------------+ */
/*           | Data class code     | */
/*           +---------------------+ */
/*           | Data type           | */
/*           +---------------------+ */
/*           | Ref frame code      | */
/*           +---------------------+ */
/*           | Coord sys code      | */
/*           +---------------------+ */
/*           | Coord sys parameters|  {10 elements} */
/*           +---------------------+ */
/*           | Min coord 1         | */
/*           +---------------------+ */
/*           | Max coord 1         | */
/*           +---------------------+ */
/*           | Min coord 2         | */
/*           +---------------------+ */
/*           | Max coord 2         | */
/*           +---------------------+ */
/*           | Min coord 3         | */
/*           +---------------------+ */
/*           | Max coord 3         | */
/*           +---------------------+ */
/*           | Start time          | */
/*           +---------------------+ */
/*           | Stop time           | */
/*           +---------------------+ */

/*     Parameters defining offsets for segment descriptor elements */
/*     follow. */


/*     Surface ID code: */


/*     Central ephemeris object NAIF ID: */


/*     Data class: */

/*     The "data class" is a code indicating the category of */
/*     data contained in the segment. */


/*     Data type: */


/*     Frame ID: */


/*     Coordinate system code: */


/*     Coordinate system parameter start index: */


/*     Number of coordinate system parameters: */


/*     Ranges for coordinate bounds: */


/*     Coverage time bounds: */


/*     Descriptor size (24): */


/*     Data class values: */

/*        Class 1 indicates a surface that can be represented as a */
/*                single-valued function of its domain coordinates. */

/*                An example is a surface defined by a function that */
/*                maps each planetodetic longitude and latitude pair to */
/*                a unique altitude. */


/*        Class 2 indicates a general surface. Surfaces that */
/*                have multiple points for a given pair of domain */
/*                coordinates---for example, multiple radii for a given */
/*                latitude and longitude---belong to class 2. */



/*     Coordinate system values: */

/*        The coordinate system code indicates the system to which the */
/*        tangential coordinate bounds belong. */

/*        Code 1 refers to the planetocentric latitudinal system. */

/*        In this system, the first tangential coordinate is longitude */
/*        and the second tangential coordinate is latitude. The third */
/*        coordinate is radius. */



/*        Code 2 refers to the cylindrical system. */

/*        In this system, the first tangential coordinate is radius and */
/*        the second tangential coordinate is longitude. The third, */
/*        orthogonal coordinate is Z. */



/*        Code 3 refers to the rectangular system. */

/*        In this system, the first tangential coordinate is X and */
/*        the second tangential coordinate is Y. The third, */
/*        orthogonal coordinate is Z. */



/*        Code 4 refers to the planetodetic/geodetic system. */

/*        In this system, the first tangential coordinate is longitude */
/*        and the second tangential coordinate is planetodetic */
/*        latitude. The third, orthogonal coordinate is altitude. */



/*     End of include file dskdsc.inc */

/* $ Abstract */

/*     This include file defines the dimension of the counter */
/*     array used by various SPICE subsystems to uniquely identify */
/*     changes in their states. */

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

/*     CTRSIZ      is the dimension of the counter array used by */
/*                 various SPICE subsystems to uniquely identify */
/*                 changes in their states. */

/* $ Author_and_Institution */

/*     B.V. Semenov    (JPL) */

/* $ Literature_References */

/*     None. */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 29-JUL-2013 (BVS) */

/* -& */

/*     End of include file. */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     BODYID     I   ID code of target body. */
/*     NSURF      I   Number of IDs in surface list. */
/*     SRFLST     I   List of surface IDs. */
/*     MINRAD     O   Radius of inner bounding sphere for body. */
/*     MAXRAD     O   Radius of outer bounding sphere for body. */

/* $ Detailed_Input */

/*     BODYID     is the body ID of the target for which radii */
/*                of bounding spheres are to be generated. */

/*     NSURF, */
/*     SRFLST     are, respectively, the surface list count and */
/*                an array containing a list of surface IDs. */

/*                If the count is zero, all surfaces for the body */
/*                are considered applicable. */

/* $ Detailed_Output */

/*     MINRAD     is the radius of an inner bounding sphere for the */
/*                surface of the body designated by BODYID, NSURF, */
/*                and SRFLST. The sphere is centered at the target */
/*                body's center. All points of the body's surface */
/*                are outside this sphere. */

/*                MINRAD is not necessarily a maximum lower bound. */

/*                Units are km. */


/*     MAXRAD     is the radius of an outer bounding sphere for the */
/*                surface of the body designated by BODYID, NSURF, */
/*                and SRFLST. The sphere is centered at the target */
/*                body's center. All points of the body's surface */
/*                are contained in this sphere. */

/*                MINRAD is not necessarily a minimum upper bound. */

/*                Units are km. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If, for a DSK segment's reference frame, frame information */
/*         cannot be looked up, the error SPICE(NOFRAMEDATA) is */
/*         signaled. */

/*     2)  If, for a DSK segment's reference frame, the frame's name */
/*         cannot be looked up, the error SPICE(FRAMENAMENOTFOUND) is */
/*         signaled. */

/*     3)  If a DSK segment descriptor has an unrecognized coordinate */
/*         system code, the error SPICE(NOTSUPPORTED) is signaled. */

/*     4)  If no DSK segments are found for the specified target */
/*         and surface set, the error SPICE(DSKDATANOTFOUND) is */
/*         signaled. */

/*     5)  If an error occurs while looking up DSK descriptors, */
/*         the error will be diagnosed by routines in the call */
/*         tree of this routine. */

/*     6)  If the surface list size is negative, the error */
/*         SPICE(VALUEOUTOFRANGE) is signaled. */

/* $ Files */

/*     This routine makes use of DSK files loaded by the ZZDSKBSR */
/*     subsystem. */

/*     If any loaded DSK segment has a reference frame that is not */
/*     centered at the segment's central (target) body, SPK data are */
/*     required to compute the offset between the frame's center and */
/*     the segment's center. The lookup epoch for a segment is the */
/*     midpoint of the segment's time coverage interval, so SPK data */
/*     must be available for that epoch. */

/*     Frame kernels may be required in order to look up a segment's */
/*     frame center offset. In some cases, additional kernels such */
/*     as CK kernels and SCLK kernels could be required to support */
/*     the offset vector lookup. */

/* $ Particulars */

/*     This routine is used as an initialization step by the ZZDSKSBF */
/*     entry point ZZSUDSKI. */

/*     The operation of this routine usually will result in physical */
/*     file reads, and so will be rather slow. */

/* $ Examples */

/*     See usage in ZZSUDSKI. */

/* $ Restrictions */

/*     1)  This routine does not take time into account when it */
/*         selects segments. All segments for the specified body */
/*         and surface list are selected. */

/*         This functionality might be inappropriate for some */
/*         future application. */

/*     2)  This is a private routine. It is meant to be used only by the */
/*         DSK subsystem. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman    (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 11-OCT-2021 (NJB) */

/*        Added and moved calls to FAILED() in order to prevent use */
/*        of uninitialized value of SEGFND. Added SAVE statement for */
/*        variable PRVFID. */

/* -    SPICELIB Version 1.0.0, 26-JUL-2016 (NJB) */

/*        Updated to use reference ellipsoid parameters and altitude */
/*        bounds to compute candidate values for bounding radii. This */
/*        applies to the planetodetic coordinate system. */

/*        30-JUN-2016 (NJB) */

/*        30-JAN-2015 (NJB) */

/*           Updated to provide inner radius in addition to outer */
/*           radius. Updated to support segment frame centers that don't */
/*           coincide with the target. */

/*        14-JAN-2015 (NJB) */

/* -& */
/* $ Index_Entries */

/*     find inner and outer bounding spheres for target body */

/* -& */

/*     SPICELIB functions */


/*     EXTERNAL routines */


/*     Local parameters */


/*     Local variables */


/*     Saved variables */


/*     Initial values */

    if (return_()) {
	return 0;
    }
    chkin_("ZZDSKSPH", (ftnlen)8);
    if (first) {
	zzctruin_(ctr);
    }

/*     Check NSURF. */

    if (*nsurf < 0) {
	setmsg_("NSURF must be non-negative but was #.", (ftnlen)37);
	errint_("#", nsurf, (ftnlen)1);
	sigerr_("SPICE(VALUEOUTOFRANGE)", (ftnlen)22);
	chkout_("ZZDSKSPH", (ftnlen)8);
	return 0;
    }

/*     Determine whether the input body surface list matches */
/*     the previous values. The following code applies whether */
/*     or not the surface list is non-empty. */

    newlst = TRUE_;
    if (! first) {
	if (*bodyid == prvbod) {
	    if (*nsurf == prvnls) {
		same = TRUE_;
		i__ = 1;
		while(i__ <= *nsurf && same) {
		    same = srflst[i__ - 1] == prvlst[(i__1 = i__ - 1) < 100 &&
			     0 <= i__1 ? i__1 : s_rnge("prvlst", i__1, "zzds"
			    "ksph_", (ftnlen)360)];
		    ++i__;
		}

/*              If SAME is true here, the body and surface list are the */
/*              same as on the previous call. */

		newlst = ! same;
	    }
	}
    }

/*     Set PRVNLS to a value that can't match a valid value, so */
/*     the surface list won't match after an error occurs. We'll */
/*     reset PRVNLS prior to exit if all goes well. */

    prvnls = -1;

/*     Check for DSK update in ZZDSKBSR. */

    zzdskchk_(ctr, &update);

/*     Initialize the temporary variables MINR, MAXR. */

    minr = svminr;
    maxr = svmaxr;
    if (first || update || newlst) {

/*        Initialize the saved radius data. */

	svmaxr = -1.;
	svminr = dpmax_();

/*        Prepare to fetch segment data. Initialize the ZZDSKBSR */
/*        segment list for the body of interest. */

	zzdskbbl_(bodyid);
	if (failed_()) {
	    chkout_("ZZDSKSPH", (ftnlen)8);
	    return 0;
	}

/*        Fetch segment DSK descriptors for the indicated body and */
/*        surface list. */

	prvfid = 0;
	cleard_(&c__3, offset);

/*        Re-initialize MINR and MAXR. */

	maxr = -1.;
	minr = dpmax_();

/*        Examine all segments for BODYID. */

	zzdskbss_(bodyid);
	zzdsksbd_(bodyid);
	zzdsksns_((U_fp)zzdskbdc_, &handle, dladsc, dskdsc, &segfnd);
	if (failed_()) {
	    chkout_("ZZDSKSPH", (ftnlen)8);
	    return 0;
	}
	while(segfnd) {
	    if (*nsurf > 0) {
		surfid = i_dnnt(dskdsc);
		i__ = isrchi_(&surfid, nsurf, srflst);
	    } else {
		i__ = 1;
	    }
	    if (i__ > 0) {

/*              If we're checking surface IDs, this segment qualifies. */
/*              Otherwise, we're not checking surface IDs, so the segment */
/*              qualifies by default. */

/*              Get the frame ID of this segment, and look up the frame's */
/*              center. */

		framid = i_dnnt(&dskdsc[4]);
		if (framid != prvfid) {

/*                 Get the frame center for the current segment. */

		    frinfo_(&framid, &frcent, &frclas, &frclid, &found);
		    if (! found) {
			setmsg_("No frame specification was found for frame "
				"ID #.", (ftnlen)48);
			errint_("#", &framid, (ftnlen)1);
			sigerr_("SPICE(NOFRAMEDATA)", (ftnlen)18);
			chkout_("ZZDSKSPH", (ftnlen)8);
			return 0;
		    }
		    if (frcent == *bodyid) {

/*                    The frame is centered at the target, so */
/*                    the frame center offset magnitude is zero. */

			offmag = 0.;
		    } else {
			frmnam_(&framid, frname, (ftnlen)32);
			if (failed_()) {
			    chkout_("ZZDSKSPH", (ftnlen)8);
			    return 0;
			}
			if (s_cmp(frname, " ", (ftnlen)32, (ftnlen)1) == 0) {
			    setmsg_("No frame name was found for frame ID #.",
				     (ftnlen)39);
			    errint_("#", &framid, (ftnlen)1);
			    sigerr_("SPICE(FRAMENAMENOTFOUND)", (ftnlen)24);
			    chkout_("ZZDSKSPH", (ftnlen)8);
			    return 0;
			}
			midtim = (dskdsc[22] + dskdsc[23]) / 2;
			spkgps_(&frcent, &midtim, frname, bodyid, offset, &lt,
				 (ftnlen)32);
			if (failed_()) {
			    chkout_("ZZDSKSPH", (ftnlen)8);
			    return 0;
			}
			offmag = vnorm_(offset);
		    }
		}

/*              Get the segment coordinate system and derive the maximum */
/*              radius of the segment. */

		corsys = i_dnnt(&dskdsc[5]);

/*              Get bounding radii for the segment relative to the */
/*              origin of the segment's coordinate system. We'll account */
/*              for the offset of the origin from the segment's central */
/*              body as a subsequent step. */

		if (corsys == 1) {
		    sgminr = dskdsc[20];
		    sgmaxr = dskdsc[21];
		} else if (corsys == 4) {

/*                 Use the reference spheroid and altitude bounds to */
/*                 generate initial bounding radii. */

		    re = dskdsc[6];
		    f = dskdsc[7];
		    rp = re * (1. - f);
		    if (f >= 0.) {

/*                    The spheroid is oblate. The maximum altitude over */
/*                    the equator is an upper bound for the distance of */
/*                    any surface point from the origin. The minimum */
/*                    altitude over either pole is a lower bound for */
/*                    the distance of any surface point from the origin. */

/*                    The DSK descriptor gives us the altitude bounds. */

			sgmaxr = re + dskdsc[21];
			sgminr = rp + dskdsc[20];
		    } else {
/*                    The spheroid is prolate. The maximum altitude over */
/*                    either pole is an upper bound for the distance of */
/*                    any surface point from the origin. */

			sgmaxr = rp + dskdsc[21];
			sgminr = re + dskdsc[20];
		    }
		} else if (corsys == 3) {
		    zzrecbox_(&dskdsc[16], boxctr, &lx, &ly, &lz, &boxrad);

/*                 SGMINR is a lower bound on the distance of the */
/*                 segment from the origin of the coordinate system. */

/* Computing MAX */
		    d__1 = vnorm_(boxctr) - boxrad;
		    sgminr = max(d__1,0.);
		    sgmaxr = vnorm_(boxctr) + boxrad;
		} else {
		    setmsg_("Coordinate system # is not currently supported.",
			     (ftnlen)47);
		    errint_("#", &corsys, (ftnlen)1);
		    sigerr_("SPICE(NOTSUPPORTED)", (ftnlen)19);
		    chkout_("ZZDSKSPH", (ftnlen)8);
		    return 0;
		}

/*              Apply the triangle inequality to derive minimum and */
/*              maximum values of the distance of the surface from the */
/*              body center, given the offset between the frame center */
/*              and the body center, and given bounds on the distance of */
/*              the surface from the frame's center. */

		if (offmag <= sgminr) {

/*                 The segment's central body is inside the inner */
/*                 bounding sphere of the segment. */

		    ctrmnr = sgminr - offmag;
		} else if (offmag >= sgmaxr) {

/*                 The segment's central body is outside the outer */
/*                 bounding sphere of the segment. */

		    ctrmnr = offmag - sgmaxr;
		} else {

/*                 The segment's central body is between the bounding */
/*                 spheres. No positive lower radius bound exists. */

		    ctrmnr = 0.;
		}

/*              Update the segment's outer bounding radius to */
/*              account for the frame center offset (which may */
/*              be zero). */

		sgmaxr += offmag;

/*              Update the global minimum and maximum radii. */

		minr = min(minr,ctrmnr);
		maxr = max(maxr,sgmaxr);
	    }

/*           Look at the next segment. */

	    zzdsksbd_(bodyid);
	    zzdsksns_((U_fp)zzdskbdc_, &handle, dladsc, dskdsc, &segfnd);
	    if (failed_()) {
		chkout_("ZZDSKSPH", (ftnlen)8);
		return 0;
	    }
	}
	if (maxr > 0. && ! failed_()) {

/*           Update the saved bounds. */

	    svminr = minr;
	    svmaxr = maxr;
	}
    }
    if (maxr < 0.) {

/*        We tried to update the radius bounds but didn't find any */
/*        segments for the specified body. */

/*        We have no radius data for the specified surface list. */

	if (*nsurf == 0) {
	    s_copy(errmsg, "No segments were found matching the body ID #.", (
		    ftnlen)1840, (ftnlen)46);
	} else {
	    s_copy(errmsg, "No segments were found matching the body ID # an"
		    "d the surface list <@>.", (ftnlen)1840, (ftnlen)71);
	    i__1 = *nsurf - 1;
	    for (i__ = 1; i__ <= i__1; ++i__) {
		repmc_(errmsg, "@", "*, @", errmsg, (ftnlen)1840, (ftnlen)1, (
			ftnlen)4, (ftnlen)1840);
		repmi_(errmsg, "*", &srflst[i__ - 1], errmsg, (ftnlen)1840, (
			ftnlen)1, (ftnlen)1840);
	    }
	    repmi_(errmsg, "@", &srflst[*nsurf - 1], errmsg, (ftnlen)1840, (
		    ftnlen)1, (ftnlen)1840);
	}
	setmsg_(errmsg, (ftnlen)1840);
	errint_("#", bodyid, (ftnlen)1);
	sigerr_("SPICE(DSKDATANOTFOUND)", (ftnlen)22);
	chkout_("ZZDSKSPH", (ftnlen)8);
	return 0;
    }
    if (! failed_()) {
	first = FALSE_;
	prvbod = *bodyid;
	prvnls = *nsurf;
	if (newlst) {
	    movei_(srflst, nsurf, prvlst);
	}
	*maxrad = svmaxr;
	*minrad = svminr;
    }
    chkout_("ZZDSKSPH", (ftnlen)8);
    return 0;
} /* zzdsksph_ */

