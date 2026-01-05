/* zzdskbun.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__2 = 2;
static integer c__0 = 0;
static integer c__1000 = 1000;
static integer c__9 = 9;

/* $Procedure ZZDSKBUN ( DSK, buffered unprioritized normal vector ) */
/* Subroutine */ int zzdskbun_(integer *bodyid, integer *nsurf, integer *
	srflst, doublereal *et, integer *fixfid, integer *nseg, integer *
	hanbuf, integer *dlabuf, doublereal *dskbuf, doublereal *offbuf, 
	doublereal *ctrbuf, doublereal *radbuf, doublereal *point, doublereal 
	*normal)
{
    /* System generated locals */
    integer i__1, i__2;

    /* Builtin functions */
    integer i_dnnt(doublereal *), s_rnge(char *, integer, char *, integer);

    /* Local variables */
    logical done;
    integer plid;
    logical xfnd;
    integer nhit;
    doublereal dist;
    extern /* Subroutine */ int vsub_(doublereal *, doublereal *, doublereal *
	    ), vequ_(doublereal *, doublereal *), mtxv_(doublereal *, 
	    doublereal *, doublereal *);
    integer i__, j;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    integer plate[3];
    extern /* Subroutine */ int moved_(doublereal *, integer *, doublereal *),
	     errdp_(char *, doublereal *, ftnlen);
    integer sghit[1000];
    doublereal segpt[3];
    integer dtype;
    extern doublereal vdist_(doublereal *, doublereal *);
    doublereal vtemp[3], xform[9]	/* was [3][3] */, verts[9]	/* 
	    was [3][3] */;
    extern logical failed_(void);
    extern /* Subroutine */ int refchg_(integer *, integer *, doublereal *, 
	    doublereal *);
    integer segfid;
    extern integer isrchi_(integer *, integer *, integer *);
    extern logical return_(void);
    doublereal sgmarg, sgxbuf[9000]	/* was [3][3][1000] */;
    integer corsys, prvfrm, surfce, winner;
    logical bodyok, inside, multfr, surfok, timeok;
    extern /* Subroutine */ int setmsg_(char *, ftnlen), errint_(char *, 
	    integer *, ftnlen), sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), dskgtl_(integer *, doublereal *), pltnrm_(doublereal *, 
	    doublereal *, doublereal *, doublereal *), vhatip_(doublereal *), 
	    mxv_(doublereal *, doublereal *, doublereal *), zzinrec_(
	    doublereal *, doublereal *, doublereal *, integer *, logical *), 
	    zzinlat_(doublereal *, doublereal *, doublereal *, integer *, 
	    logical *), zzinpdt_(doublereal *, doublereal *, doublereal *, 
	    doublereal *, integer *, logical *), zzptpl02_(integer *, integer 
	    *, doublereal *, doublereal *, integer *, integer *, doublereal *,
	     logical *);

/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines. Users should not call this routine directly due to the */
/*     volatile nature of this routine. */

/*     Find the outward unit normal vector at a specified point on the */
/*     surface of a body represented by one or more DSK segments. The */
/*     set of surface IDs to be considered is specified by the caller. */

/*     This routine uses DSK segments in an unprioritized manner. */
/*     All segments meeting the body, time, and surface constraints */
/*     are considered. */

/*     Segment descriptor and derived bounding data are passed in. */

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

/*     GEOMETRY */
/*     NORMAL */
/*     RAY */
/*     SURFACE */
/*     TOPOGRAPHY */
/*     VECTOR */

/* $ Declarations */

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


/*     File: dsktol.inc */


/*     This file contains declarations of tolerance and margin values */
/*     used by the DSK subsystem. */

/*     It is recommended that the default values defined in this file be */
/*     changed only by expert SPICE users. */

/*     The values declared in this file are accessible at run time */
/*     through the routines */

/*        DSKGTL  {DSK, get tolerance value} */
/*        DSKSTL  {DSK, set tolerance value} */

/*     These are entry points of the routine DSKTOL. */

/*        Version 1.0.0 27-FEB-2016 (NJB) */




/*     Parameter declarations */
/*     ====================== */

/*     DSK type 2 plate expansion factor */
/*     --------------------------------- */

/*     The factor XFRACT is used to slightly expand plates read from DSK */
/*     type 2 segments in order to perform ray-plate intercept */
/*     computations. */

/*     This expansion is performed to prevent rays from passing through */
/*     a target object without any intersection being detected. Such */
/*     "false miss" conditions can occur due to round-off errors. */

/*     Plate expansion is done by computing the difference vectors */
/*     between a plate's vertices and the plate's centroid, scaling */
/*     those differences by (1 + XFRACT), then producing new vertices by */
/*     adding the scaled differences to the centroid. This process */
/*     doesn't affect the stored DSK data. */

/*     Plate expansion is also performed when surface points are mapped */
/*     to plates on which they lie, as is done for illumination angle */
/*     computations. */

/*     This parameter is user-adjustable. */


/*     The keyword for setting or retrieving this factor is */


/*     Greedy segment selection factor */
/*     ------------------------------- */

/*     The factor SGREED is used to slightly expand DSK segment */
/*     boundaries in order to select segments to consider for */
/*     ray-surface intercept computations. The effect of this factor is */
/*     to make the multi-segment intercept algorithm consider all */
/*     segments that are sufficiently close to the ray of interest, even */
/*     if the ray misses those segments. */

/*     This expansion is performed to prevent rays from passing through */
/*     a target object without any intersection being detected. Such */
/*     "false miss" conditions can occur due to round-off errors. */

/*     The exact way this parameter is used is dependent on the */
/*     coordinate system of the segment to which it applies, and the DSK */
/*     software implementation. This parameter may be changed in a */
/*     future version of SPICE. */


/*     The keyword for setting or retrieving this factor is */


/*     Segment pad margin */
/*     ------------------ */

/*     The segment pad margin is a scale factor used to determine when a */
/*     point resulting from a ray-surface intercept computation, if */
/*     outside the segment's boundaries, is close enough to the segment */
/*     to be considered a valid result. */

/*     This margin is required in order to make DSK segment padding */
/*     (surface data extending slightly beyond the segment's coordinate */
/*     boundaries) usable: if a ray intersects the pad surface outside */
/*     the segment boundaries, the pad is useless if the intercept is */
/*     automatically rejected. */

/*     However, an excessively large value for this parameter is */
/*     detrimental, since a ray-surface intercept solution found "in" a */
/*     segment can supersede solutions in segments farther from the */
/*     ray's vertex. Solutions found outside of a segment thus can mask */
/*     solutions that are closer to the ray's vertex by as much as the */
/*     value of this margin, when applied to a segment's boundary */
/*     dimensions. */

/*     The keyword for setting or retrieving this factor is */


/*     Surface-point membership margin */
/*     ------------------------------- */

/*     The surface-point membership margin limits the distance */
/*     between a point and a surface to which the point is */
/*     considered to belong. The margin is a scale factor applied */
/*     to the size of the segment containing the surface. */

/*     This margin is used to map surface points to outward */
/*     normal vectors at those points. */

/*     If this margin is set to an excessively small value, */
/*     routines that make use of the surface-point mapping won't */
/*     work properly. */


/*     The keyword for setting or retrieving this factor is */


/*     Angular rounding margin */
/*     ----------------------- */

/*     This margin specifies an amount by which angular values */
/*     may deviate from their proper ranges without a SPICE error */
/*     condition being signaled. */

/*     For example, if an input latitude exceeds pi/2 radians by a */
/*     positive amount less than this margin, the value is treated as */
/*     though it were pi/2 radians. */

/*     Units are radians. */


/*     This parameter is not user-adjustable. */

/*     The keyword for retrieving this parameter is */


/*     Longitude alias margin */
/*     ---------------------- */

/*     This margin specifies an amount by which a longitude */
/*     value can be outside a given longitude range without */
/*     being considered eligible for transformation by */
/*     addition or subtraction of 2*pi radians. */

/*     A longitude value, when compared to the endpoints of */
/*     a longitude interval, will be considered to be equal */
/*     to an endpoint if the value is outside the interval */
/*     differs from that endpoint by a magnitude less than */
/*     the alias margin. */


/*     Units are radians. */


/*     This parameter is not user-adjustable. */

/*     The keyword for retrieving this parameter is */


/*     End of include file dsktol.inc */

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
/*     ET         I   Lookup epoch, expressed as seconds past J2000 TDB. */
/*     FIXFID     I   Frame ID of body-fixed frame for output vectors. */
/*     NSEG       I   Number of segments in buffers. */
/*     HANBUF     I   DSK handle buffer. */
/*     DLABUF     I   DLA segment descriptor buffer. */
/*     DSKBUF     I   DSK segment descriptor buffer. */
/*     OFFBUF     I   Frame center offset buffer. */
/*     CTRBUF     I   Bounding sphere center buffer. */
/*     RADBUF     I   Bounding sphere radius buffer. */
/*     POINT      I   Surface point. */
/*     NORMAL     O   Outward unit normal vector at surface point. */

/* $ Detailed_Input */

/*     BODYID     is the body ID of the target on which the input */
/*                point is located. */


/*     NSURF, */
/*     SRFLST     are, respectively, the surface list count and */
/*                an array containing a list of surface IDs. */

/*                If the count is zero, all surfaces for the body */
/*                are considered applicable. */


/*     ET         is the lookup epoch, specified as seconds past */
/*                J2000 TDB. Only DSK segments containing ET in */
/*                their time coverage intervals are considered */
/*                in the normal vector computation. */


/*     FIXFID     is the frame ID of a body-fixed reference frame */
/*                centered on the target body. */


/*     NSEG       is the number of DSK segments for which the input */
/*                buffers contain data. */


/*     HANBUF     is the DSK handle buffer. */


/*     DLABUF     is the DLA segment descriptor buffer. */


/*     DSKBUF     is the DSK segment descriptor buffer. */


/*     OFFBUF     is the frame offset buffer. For each DSK segment, the */
/*                entry in this buffer is the position of that segment's */
/*                reference frame center relative to the target body's */
/*                center. The vector is expressed in the segment's */
/*                reference frame. Units are km. */


/*     CTRBUF     is the bounding sphere center buffer. For each DSK */
/*                segment, the entry in this buffer is the position of */
/*                that segment's outer bounding sphere center relative */
/*                to the segment's reference frame center. The vector is */
/*                expressed in the segment's reference frame. Units are */
/*                km. */


/*     RADBUF     is the bounding sphere radius buffer. For each DSK */
/*                segment, the entry in this buffer is the radius of the */
/*                segment's outer bounding sphere. Units are km. */


/*     POINT      is a point on the surface of the target body. POINT is */
/*                expressed in the reference frame designated by FIXFID. */
/*                POINT represents an offset from the target body */
/*                center. */

/*                POINT must be located on the surface of the target */
/*                body, within a small tolerance. */


/* $ Detailed_Output */

/*     NORMAL     is the unit length, outward normal vector on the */
/*                surface of the target body at POINT. NORMAL is */
/*                expressed in the reference frame designated by FIXFID. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the input segment count is non-positive, the error */
/*         SPICE(NODSKSEGMENTS) is signaled. */

/*     2)  If an unrecognized coordinate system code is found */
/*         in a DSK descriptor, the error SPICE(BADCOORDSYS) is */
/*         signaled. */

/*     3)  If the input point is contained in more than MAXHIT */
/*         segments, the error SPICE(TOOMANYHITS) is signaled. */

/*     4)  If the input point is not contained in any DSK segment, */
/*         the error SPICE(POINTNOTINSEGMENT) is signaled. */

/*     5)  If an unrecognized segment data type is encountered, */
/*         the error SPICE(NOTSUPPORTED) is signaled. */

/*     6)  If the input point is not sufficiently close to the */
/*         surface, the error SPICE(POINTOFFSURFACE) will be */
/*         signaled. */

/*     7)  Any other errors that occur while looking up DSK data or */
/*         mapping the surface point to a normal vector will be signaled */
/*         by routines in the call tree of this routine. */

/* $ Files */

/*     This routine makes use of DSK files loaded by the ZZDSKBSR */
/*     subsystem. */

/*     Frame kernels may be required in order to perform transformations */
/*     from DSK segment frames to the output frame. In some cases, */
/*     additional kernels such as CK kernels and SCLK kernels could be */
/*     required to support the offset vector lookup. */

/* $ Particulars */

/*     This routine is meant to be called from the DSK API segment */
/*     buffering subsystem. All of the input buffers should contain */
/*     values computed after changes to the set of loaded DSKs. */

/*     This routine takes advantage of the input segment bound */
/*     information to efficiently determine which segments may contain */
/*     the input point. */

/* $ Examples */

/*     None. */

/* $ Restrictions */

/*     This is a private routine. It is meant to be used only by the DSK */
/*     subsystem. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman    (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 22-FEB-2017 (NJB) */

/*        Added FAILED calls. */

/*        07-APR-2016 (NJB) */

/*        Based on first version 17-DEC-2014 (NJB) */

/* -& */
/* $ Index_Entries */

/*     compute normals on dsk surface */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     Local variables */


/*     Saved variables */


/*     Initial values */

    if (return_()) {
	return 0;
    }
    chkin_("ZZDSKBUN", (ftnlen)8);

/*     Check the incoming segment count. */

    if (*nseg <= 0) {
	setmsg_("Input segment list was empty. This may be due to no DSKs co"
		"ntaining data for body # having been loaded.", (ftnlen)103);
	errint_("#", bodyid, (ftnlen)1);
	sigerr_("SPICE(NODSKSEGMENTS)", (ftnlen)20);
	chkout_("ZZDSKBUN", (ftnlen)8);
	return 0;
    }

/*     Get the segment margin from the tolerance database. */

    dskgtl_(&c__2, &sgmarg);

/*     Indicate we haven't yet seen a segment frame different */
/*     from the one designated by FIXFID. */

    multfr = FALSE_;

/*     Make a local copy of the point. We'll update this copy */
/*     later if need be. */

    vequ_(point, segpt);

/*     By default, each segment in the input list must be checked */
/*     for intersection. */

/*     We start out by trying to eliminate segments from consideration */
/*     by comparing the point's distance from their centers to the radii */
/*     of their bounding spheres. Only those segments whose bounding */
/*     spheres contain the point are examined further. */

    nhit = 0;
    prvfrm = 0;
    i__1 = *nseg;
    for (i__ = 1; i__ <= i__1; ++i__) {

/*        BODYOK indicates whether the input body ID matches that */
/*        of the current segment. */

	surfok = FALSE_;
	timeok = FALSE_;
	bodyok = *bodyid == i_dnnt(&dskbuf[i__ * 24 - 23]);
	if (bodyok) {

/*           See whether the current segment contains a surface we're */
/*           supposed to consider. If the surface list is empty, we */
/*           consider all surfaces. Otherwise, the surface of the */
/*           segment must be on the surface list in order to qualify. */

	    j = 0;
	    if (*nsurf > 0) {
		surfce = i_dnnt(&dskbuf[i__ * 24 - 24]);
		j = isrchi_(&surfce, nsurf, srflst);
	    }
	    surfok = *nsurf == 0 || j > 0;

/*           See whether the segment covers the input epoch. */

	    timeok = *et >= dskbuf[i__ * 24 - 2] && *et <= dskbuf[i__ * 24 - 
		    1];
	}
	if (bodyok && surfok && timeok) {

/*           This segment is to be considered. */

/*           In order to do any geometric comparison, the point must be */
/*           in the same frame as the segment we're checking. Transform */
/*           the point if need be. */

/*           Get the segment's frame ID. Get the transformation from the */
/*           input frame to the output frame if needed. */
	    segfid = i_dnnt(&dskbuf[i__ * 24 - 20]);
	    if (segfid != *fixfid) {

/*              We have a segment that uses a different frame */
/*              from that specified by FIXFID. */

		multfr = TRUE_;
		if (segfid != prvfrm) {

/*                 The frame of the current segment doesn't match */
/*                 that of the previous segment, so we'll need */
/*                 to look up the transformation from the input */
/*                 frame to the segment frame. */

/*                 Otherwise, XFORM already contains the correct */
/*                 transformation. */

		    refchg_(fixfid, &segfid, et, xform);
		    if (failed_()) {
			chkout_("ZZDSKBUN", (ftnlen)8);
			return 0;
		    }

/*                 Transform the local copy of the point to the segment's */
/*                 frame, and shift the local copy of the point so */
/*                 that it represents an offset relative to center of */
/*                 the segment's frame. */

		    mxv_(xform, point, segpt);

/*                 The direction of the buffered offset is from the body */
/*                 to the segment frame's center. The offset is */
/*                 expressed in the segment's frame. */

		    vsub_(segpt, &offbuf[i__ * 3 - 3], vtemp);
		    vequ_(vtemp, segpt);
		}
	    } else if (multfr) {

/*              The input and segment frames are the same for this */
/*              segment, but the current value of SEGPT needs to be */
/*              reset. */

		vequ_(point, segpt);
	    }

/*           Find the distance of the point from the "center" of the */
/*           segment's coverage volume. */

	    dist = vdist_(&ctrbuf[i__ * 3 - 3], segpt);
	    if (dist <= radbuf[i__ - 1]) {

/*              The point is inside or on the bounding surface. We'll */
/*              check the boundary of the segment for an intersection. */

		corsys = i_dnnt(&dskbuf[i__ * 24 - 19]);
		if (corsys == 1) {
		    zzinlat_(segpt, &dskbuf[i__ * 24 - 8], &sgmarg, &c__0, &
			    inside);
		} else if (corsys == 3) {
		    zzinrec_(segpt, &dskbuf[i__ * 24 - 8], &sgmarg, &c__0, &
			    inside);
		} else if (corsys == 4) {
		    zzinpdt_(segpt, &dskbuf[i__ * 24 - 8], &dskbuf[i__ * 24 - 
			    18], &sgmarg, &c__0, &inside);
		} else {
		    setmsg_("Coordinate system # is not supported.", (ftnlen)
			    37);
		    errint_("#", &corsys, (ftnlen)1);
		    sigerr_("SPICE(BADCOORDSYS)", (ftnlen)18);
		    chkout_("ZZDSKBUN", (ftnlen)8);
		    return 0;
		}
		if (failed_()) {
		    chkout_("ZZDSKBUN", (ftnlen)8);
		    return 0;
		}
		if (inside) {

/*                 The point in inside the region enclosed by the */
/*                 boundary of this segment. Save the index of the */
/*                 segment in the "hit list." */

		    if (nhit == 1000) {
			setmsg_("Too many segments contain the input point. "
				"Buffer size is #.", (ftnlen)60);
			errint_("#", &c__1000, (ftnlen)1);
			sigerr_("SPICE(TOOMANYHITS)", (ftnlen)18);
			chkout_("ZZDSKBUN", (ftnlen)8);
			return 0;
		    }
		    ++nhit;
		    sghit[(i__2 = nhit - 1) < 1000 && 0 <= i__2 ? i__2 : 
			    s_rnge("sghit", i__2, "zzdskbun_", (ftnlen)538)] =
			     i__;

/*                 Save the frame transformation for this segment. */

		    moved_(xform, &c__9, &sgxbuf[(i__2 = (nhit * 3 + 1) * 3 - 
			    12) < 9000 && 0 <= i__2 ? i__2 : s_rnge("sgxbuf", 
			    i__2, "zzdskbun_", (ftnlen)542)]);
		}
	    }

/*           The current segment matched the input criteria. */

/*           Update the saved segment frame ID to that of the segment */
/*           we just examined. */

	    prvfrm = segfid;
	}
    }

/*     We have an error if no segments were hit. None of that */
/*     "not found" nonsense here. */

    if (nhit == 0) {
	setmsg_("Input point (# # #) in frame # does not lie inside any segm"
		"ent for the specified body (#) and surfaces.", (ftnlen)103);
	errdp_("#", point, (ftnlen)1);
	errdp_("#", &point[1], (ftnlen)1);
	errdp_("#", &point[2], (ftnlen)1);
	errint_("#", fixfid, (ftnlen)1);
	errint_("#", bodyid, (ftnlen)1);
	sigerr_("SPICE(POINTNOTINSEGMENT)", (ftnlen)24);
	chkout_("ZZDSKBUN", (ftnlen)8);
	return 0;
    }

/*     Now process the segments on the hit list. If we find a segment */
/*     surface on which the input point is located, we compute the */
/*     normal vector and terminate the search. */

    i__ = 1;
    done = FALSE_;
    winner = 0;
    prvfrm = 0;
    while(! done) {

/*        I is the index in the hit list of the segment */
/*        we're considering. J is the index of that segment */
/*        in the parallel input arrays. */

	j = sghit[(i__1 = i__ - 1) < 1000 && 0 <= i__1 ? i__1 : s_rnge("sghit"
		, i__1, "zzdskbun_", (ftnlen)597)];
	segfid = i_dnnt(&dskbuf[j * 24 - 20]);
	if (segfid != *fixfid) {
	    if (segfid != prvfrm) {

/*              Transform and shift the input point. */

/*              Here I is an index in the hit list and J is */
/*              an index in the input arrays. */

		moved_(&sgxbuf[(i__1 = (i__ * 3 + 1) * 3 - 12) < 9000 && 0 <= 
			i__1 ? i__1 : s_rnge("sgxbuf", i__1, "zzdskbun_", (
			ftnlen)611)], &c__9, xform);
		mxv_(xform, point, segpt);
		vsub_(segpt, &offbuf[j * 3 - 3], vtemp);
		vequ_(vtemp, segpt);
	    }
	} else if (multfr) {
	    vequ_(point, segpt);
	}

/*        If the point lies on the surface described by the */
/*        current segment, find the outward unit normal */
/*        vector at the point. */

	dtype = i_dnnt(&dskbuf[j * 24 - 21]);
	xfnd = FALSE_;
	if (dtype == 2) {

/*           Find the plate on which the point lies, if any. */

	    zzptpl02_(&hanbuf[j - 1], &dlabuf[(j << 3) - 8], &dskbuf[j * 24 - 
		    24], segpt, &plid, plate, verts, &xfnd);
	    if (failed_()) {
		chkout_("ZZDSKBUN", (ftnlen)8);
		return 0;
	    }
	    if (xfnd) {

/*              Find the unit outward normal at SEGPT. We must */
/*              convert the output of PLTNRM to unit length. */

		pltnrm_(verts, &verts[3], &verts[6], normal);
		if (failed_()) {
		    chkout_("ZZDSKBUN", (ftnlen)8);
		    return 0;
		}
		vhatip_(normal);

/*              WINNER is the index in the hit list of the current */
/*              segment. */

		winner = i__;
		done = TRUE_;
	    }
	} else {
	    setmsg_("Segment type is #; this type is not currently supported."
		    , (ftnlen)56);
	    errint_("#", &dtype, (ftnlen)1);
	    sigerr_("SPICE(NOTSUPPORTED)", (ftnlen)19);
	    chkout_("ZZDSKBUN", (ftnlen)8);
	    return 0;
	}
	if (failed_()) {
	    chkout_("ZZDSKBUN", (ftnlen)8);
	    return 0;
	}
	if (! done) {
	    if (i__ == nhit) {

/*              We've looked at all of the segments. */

		done = TRUE_;
	    } else {

/*              Consider the next segment. */

		++i__;
	    }
	}
    }

/*     If we have an result, it may be represented in a frame */
/*     other than the input point frame. Transform it back to the */
/*     input frame, if need be. */

    if (xfnd) {

/*        J is the index in the input arrays of the "winning" segment. */

	j = sghit[(i__1 = winner - 1) < 1000 && 0 <= i__1 ? i__1 : s_rnge(
		"sghit", i__1, "zzdskbun_", (ftnlen)717)];
	segfid = i_dnnt(&dskbuf[j * 24 - 20]);
	if (segfid != *fixfid) {

/*           The segment frame and input frame differ. The normal */
/*           vector must be converted back to the input frame. */

	    moved_(&sgxbuf[(i__1 = (winner * 3 + 1) * 3 - 12) < 9000 && 0 <= 
		    i__1 ? i__1 : s_rnge("sgxbuf", i__1, "zzdskbun_", (ftnlen)
		    726)], &c__9, xform);
	    mtxv_(xform, normal, vtemp);
	    vequ_(vtemp, normal);
	}
    } else {

/*        The input point was not legitimate; otherwise we */
/*        would have found a solution. */

	setmsg_("Input point (# # #) in frame # does not lie on the surface "
		"contained in any segment for the specified body (#) and surf"
		"aces.", (ftnlen)124);
	errdp_("#", point, (ftnlen)1);
	errdp_("#", &point[1], (ftnlen)1);
	errdp_("#", &point[2], (ftnlen)1);
	errint_("#", fixfid, (ftnlen)1);
	errint_("#", bodyid, (ftnlen)1);
	sigerr_("SPICE(POINTOFFSURFACE)", (ftnlen)22);
	chkout_("ZZDSKBUN", (ftnlen)8);
	return 0;
    }
    chkout_("ZZDSKBUN", (ftnlen)8);
    return 0;
} /* zzdskbun_ */

