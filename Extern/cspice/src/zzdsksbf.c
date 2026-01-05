/* zzdsksbf.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__10 = 10;
static integer c__10000 = 10000;
static integer c__8 = 8;
static integer c__24 = 24;

/* $Procedure ZZDSKSBF ( DSK, manage the API segment buffer ) */
/* Subroutine */ int zzdsksbf_0_(int n__, integer *bodyid, integer *nsurf, 
	integer *srflst, doublereal *et, integer *fixfid, doublereal *vertex, 
	doublereal *raydir, doublereal *point, doublereal *xpt, integer *
	handle, integer *dladsc, doublereal *dskdsc, doublereal *dc, integer *
	ic, logical *found, doublereal *normal)
{
    /* Initialized data */

    static integer btnbod = 0;
    static integer btbody[10] = { 0,0,0,0,0,0,0,0,0,0 };
    static integer bsrctr[2] = { 0,0 };
    static logical first = TRUE_;
    static integer stfree = 1;

    /* System generated locals */
    integer i__1, i__2, i__3, i__4, i__5, i__6;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer);

    /* Local variables */
    integer nseg;
    extern /* Subroutine */ int zzdskbbl_(integer *), zzdskchk_(integer *, 
	    logical *), zzdsksba_(integer *, integer *, integer *, integer *, 
	    integer *, integer *, integer *, integer *, doublereal *, integer 
	    *, integer *, doublereal *, doublereal *, doublereal *), 
	    zzdsksbi_(integer *, integer *, integer *, integer *, integer *, 
	    integer *, integer *, doublereal *, integer *, integer *, 
	    doublereal *, doublereal *, doublereal *), zzdskbun_(integer *, 
	    integer *, integer *, doublereal *, integer *, integer *, integer 
	    *, integer *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *), zzdskbux_(integer *, 
	    integer *, integer *, doublereal *, integer *, integer *, integer 
	    *, integer *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, integer *,
	     doublereal *, integer *, logical *), zzctruin_(integer *);
    integer j;
    doublereal locdc[1];
    integer locic[1];
    extern /* Subroutine */ int chkin_(char *, ftnlen), moved_(doublereal *, 
	    integer *, doublereal *);
    static integer sthan[10000];
    static doublereal strad[10000];
    extern /* Subroutine */ int movei_(integer *, integer *, integer *);
    static doublereal stoff[30000]	/* was [3][10000] */, stctr[30000]	
	    /* was [3][10000] */;
    extern logical failed_(void);
    static integer stdlad[80000]	/* was [8][10000] */;
    extern integer isrchi_(integer *, integer *, integer *);
    static integer btsegp[10];
    static doublereal stdscr[240000]	/* was [24][10000] */;
    extern logical return_(void);
    static integer btstsz[10];
    integer segidx;
    logical update;
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen);
    integer bix;

/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines. Users should not call this routine directly due to the */
/*     volatile nature of this routine. */

/*     Manage the DSK API segment buffer data structure. */

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

/*     DLA */
/*     DSK */
/*     TOPOGRAPHY */
/*     UTILITY */

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

/*     Variable  I/O  Entry points */
/*     --------  ---  -------------------------------------------------- */
/*     BODYID     I   ZZSBFXR, ZZSBFNRM */
/*     NSURF      I   ZZSBFXR, ZZSBFNRM */
/*     SRFLST     I   ZZSBFXR, ZZSBFNRM */
/*     ET         I   ZZSBFXR, ZZSBFNRM */
/*     FIXFID     I   ZZSBFXR, ZZSBFNRM */
/*     VERTEX     I   ZZSBFXR */
/*     RAYDIR     I   ZZSBFXR */
/*     POINT      I   ZZSBFNRM */
/*     XPT        O   ZZSBFXR */
/*     FOUND      O   ZZSBFXR */
/*     HANDLE     O   ZZSBFXRI */
/*     DLADSC     O   ZZSBFXRI */
/*     DSKDSC     O   ZZSBFXRI */
/*     DC         O   ZZSBFXRI */
/*     IC         O   ZZSRFXRI */
/*     NORMAL     O   ZZSBFNRM */

/* $ Detailed_Input */

/*     See the entry points. */

/* $ Detailed_Output */

/*     See the entry points. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If this routine is called directly, it signals the error */
/*         SPICE(BOGUSENTRY). */

/*     See the entry points for descriptions of errors specific to */
/*     those routines. */

/* $ Files */

/*     This routine makes use of DSK files loaded by the ZZDSKBSR */
/*     subsystem. */

/* $ Particulars */

/*     This routine is meant to be used only by the DSK subsystem. */


/*     Data structure management */
/*     ========================= */

/*     This routine manages data structures used by SPICELIB geometry */
/*     APIs to perform efficient computations using DSK data. This is */
/*     the umbrella routine in which these structures are declared. */

/*     This routine also contains entry points that use these data */
/*     structures to perform geometric computations using DSK data. */
/*     See the section titled "Computations" below. */

/*     In a sense, ZZDSKSBF sits "above" the ZZDSKBSR subsystem. */
/*     High-level geometry routines access the data stored in the */
/*     ZZDSKSBF arrays directly; they don't call entry points of */
/*     ZZDSKBSR. */

/*     The ZZDSKSBF subsystem maintains two logical tables: a body */
/*     table and a segment table. Both tables are designed to store */
/*     a fixed maximum number of items; there is no "virtual storage" */
/*     concept that applies. */

/*     Items in both tables have priority assigned on a first-in, */
/*     first-out basis. When it becomes necessary to remove an */
/*     item to make room for another, the lowest-indexed item is */
/*     removed first, and the tables are compressed so that the */
/*     remaining items are contiguous. */

/*     When the state of the underlying ZZDSKBSR system changes due to */
/*     loading or unloading of DSK files, the ZZDSKSBF body and segment */
/*     tables must be re-initialized. Entry points of ZZDSKSBF */
/*     perform this action by calling ZZDSKSBI. */

/*     In addition to the tables maintained by this subsystem, a */
/*     counter is maintained. This counter is used to determine */
/*     whether the locally buffered data are in sync with the */
/*     information stored in the ZZDSKBSR subsystem. */


/*     Computations */
/*     ============ */

/*     This routine contains the following entry points that use */
/*     buffered segment data to perform computations: */

/*        ZZSBFXR:    prepare for and compute unprioritized */
/*                    ray-surface intercept using DSK data. */

/*        ZZSBFXRI:   prepare for and compute unprioritized */
/*                    ray-surface intercept using DSK data; return data */
/*                    source information such as file handle, DLA */
/*                    descriptor, and plate ID as well. */

/*        ZZSBFNRM:   prepare for and compute outward normal */
/*                    vector at a specified surface point, */
/*                    using unprioritized DSK data. */

/* $ Examples */

/*     See usage in ZZDSKSBF. */

/* $ Restrictions */

/*     1) This is a private routine. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman   (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 22-FEB-2017 (NJB) */

/*        Added FAILED calls in each entry point. */

/*        17-MAY-2016 (NJB) */

/* -& */
/* $ Index_Entries */

/*     manage DSK API segment buffer */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     Sizes of source info arrays returned by */
/*     ZZDSKBUX: */


/*     Body table variables */
/*     -------------------- */

/*        BTNBOD  is the number of bodies in the body table. */

/*        BTBODY  is an array of body ID codes. */

/*        BTSEGP  is an array of pointers (start indices) to entries in */
/*                the segment table. The Ith pointer indicates the start */
/*                index for entries for the Ith body. */

/*        BTSTSZ  is an array of segment table entry counts. The Ith */
/*                element of BTSTSZ is the number of entries in the */
/*                segment table for the Ith body. */



/*     Segment table variables */


/*     Local variables */


/*     Saved variables */


/*     Initial values */

    /* Parameter adjustments */
    if (vertex) {
	}
    if (raydir) {
	}
    if (point) {
	}
    if (xpt) {
	}
    if (dladsc) {
	}
    if (dskdsc) {
	}
    if (dc) {
	}
    if (ic) {
	}
    if (normal) {
	}

    /* Function Body */
    switch(n__) {
	case 1: goto L_zzsbfxr;
	case 2: goto L_zzsbfxri;
	case 3: goto L_zzsbfnrm;
	}

    if (return_()) {
	return 0;
    }
    chkin_("ZZDSKSBF", (ftnlen)8);
    sigerr_("SPICE(BOGUSENTRY)", (ftnlen)17);
    chkout_("ZZDSKSBF", (ftnlen)8);
    return 0;
/* $Procedure ZZSBFXR ( DSK, prepare and perform unprioritized intercept ) */

L_zzsbfxr:
/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines. Users should not call this routine directly due to the */
/*     volatile nature of this routine. */

/*     Prepare and execute unprioritized ray-surface intercept */
/*     computation using DSK API segment buffers. */

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

/*     DLA */
/*     DSK */
/*     TOPOGRAPHY */
/*     UTILITY */

/* $ Declarations */

/*     INTEGER               BODYID */
/*     INTEGER               NSURF */
/*     INTEGER               SRFLST ( * ) */
/*     DOUBLE PRECISION      ET */
/*     INTEGER               FIXFID */
/*     DOUBLE PRECISION      VERTEX ( 3 ) */
/*     DOUBLE PRECISION      RAYDIR ( 3 ) */
/*     DOUBLE PRECISION      XPT    ( 3 ) */
/*     LOGICAL               FOUND */

/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     BODYID     I   ID code of target body. */
/*     NSURF      I   Number of surface IDs in list. */
/*     SRFLST     I   Surface ID list. */
/*     ET         I   Evaluation epoch, seconds past J2000 TDB. */
/*     FIXFID     I   Frame ID of ray and intercept. */
/*     VERTEX     I   Ray's vertex. */
/*     RAYDIR     I   Ray's direction vector. */
/*     XPT        O   Surface intercept point. */
/*     FOUND      O   Found flag. True if intercept exists. */

/* $ Detailed_Input */

/*     BODYID     is the ID code of a target body. The ray-surface */
/*                intercept computation is performed using data */
/*                that represent the surface of this body. */

/*     NSURF, */
/*     SRFLST     are, respectively, a count of surface IDs and */
/*                a list of IDs. If the list is non-empty, only */
/*                the indicated surfaces will be used. If the */
/*                list is empty, all surfaces associated with the */
/*                input body ID will be considered. */

/*     ET         is the epoch for which the computation is to be */
/*                performed. This epoch is used for DSK segment */
/*                selection; only segments containing ET in their time */
/*                coverage interval will be used. ET is expressed as */
/*                seconds past J2000 TDB. */

/*     FIXFID     is the frame ID of a body-fixed frame associated */
/*                with the body designated by BODYID. This frame */
/*                is used to express the input ray's vertex and */
/*                direction vector. The output intercept will be */
/*                expressed in this frame as well. */

/*     VERTEX, */
/*     RAYDIR     are, respectively, the vertex and direction vector */
/*                of a ray. Both vectors are expressed in the frame */
/*                designated by FIXFID. */

/* $ Detailed_Output */

/*     XPT        is the surface intercept on the target body */
/*                nearest to the ray's vertex, if the intercept */
/*                exists. */

/*     FOUND      is a logical flag that is set to .TRUE. if and */
/*                only if an intercept exists. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If this routine is called directly, it signals the error */
/*         SPICE(BOGUSENTRY). */

/*     See the entry points for descriptions of errors specific to */
/*     those routines. */

/* $ Files */

/*     This routine makes use of DSK files loaded by the ZZDSKBSR */
/*     subsystem. */

/* $ Particulars */

/*     This routine is meant to be used only by the DSK subsystem. */

/*     This routine prepares the local buffers for a ray-surface */
/*     intercept computation using unprioritized DSK data. */
/*     It calls ZZDSKBUX to perform the computation. */

/* $ Examples */

/*     See usage in ZZRAYSFX. */

/* $ Restrictions */

/*     1) This is a private routine. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman   (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 22-FEB-2017 (NJB) */

/*        Added FAILED calls. */

/*        12-MAY-2016 (NJB) */

/* -& */
/* $ Index_Entries */

/*     prepare unprioritized ray surface intercept */

/* -& */
    if (return_()) {
	return 0;
    }
    chkin_("ZZSBFXR", (ftnlen)7);
    if (first) {

/*        Initialize BSR counter. */

	zzctruin_(bsrctr);
	first = FALSE_;
    }

/*     See whether the state of the loaded DSK set has changed */
/*     since the last call. */

    zzdskchk_(bsrctr, &update);
    if (update) {

/*        Make sure the ZZDSKBSR subsystem has completed the segment */
/*        list for the input body since the last time the BSR loaded */
/*        kernel state changed. */

	zzdskbbl_(bodyid);

/*        Initialize the local buffers. We restart from scratch */
/*        each time the BSR loaded kernel state changes. */

	zzdsksbi_(&c__10, &c__10000, btbody, &btnbod, btsegp, btstsz, sthan, 
		stdscr, stdlad, &stfree, stoff, stctr, strad);
    }
    if (failed_()) {
	chkout_("ZZSBFXR", (ftnlen)7);
	return 0;
    }

/*     Find the index of the input body ID in the body table. If */
/*     we re-initialized the tables, the index will be zero. */

    bix = isrchi_(bodyid, &btnbod, btbody);
    if (bix == 0) {

/*        We don't have buffered information for this body. Update */
/*        the body and segment tables to store data for it. */

	zzdsksba_(bodyid, &c__10, &c__10000, btbody, &btnbod, btsegp, btstsz, 
		sthan, stdscr, stdlad, &stfree, stoff, stctr, strad);
	if (failed_()) {
	    chkout_("ZZSBFXR", (ftnlen)7);
	    return 0;
	}

/*        The new body's position in the body table is at the end. */

	bix = btnbod;
    }

/*     Find the ray-surface intercept, using the buffered segment */
/*     data. */

    j = btsegp[(i__1 = bix - 1) < 10 && 0 <= i__1 ? i__1 : s_rnge("btsegp", 
	    i__1, "zzdsksbf_", (ftnlen)584)];
    nseg = btstsz[(i__1 = bix - 1) < 10 && 0 <= i__1 ? i__1 : s_rnge("btstsz",
	     i__1, "zzdsksbf_", (ftnlen)585)];
    zzdskbux_(bodyid, nsurf, srflst, et, fixfid, &nseg, &sthan[(i__1 = j - 1) 
	    < 10000 && 0 <= i__1 ? i__1 : s_rnge("sthan", i__1, "zzdsksbf_", (
	    ftnlen)587)], &stdlad[(i__2 = (j << 3) - 8) < 80000 && 0 <= i__2 ?
	     i__2 : s_rnge("stdlad", i__2, "zzdsksbf_", (ftnlen)587)], &
	    stdscr[(i__3 = j * 24 - 24) < 240000 && 0 <= i__3 ? i__3 : s_rnge(
	    "stdscr", i__3, "zzdsksbf_", (ftnlen)587)], &stoff[(i__4 = j * 3 
	    - 3) < 30000 && 0 <= i__4 ? i__4 : s_rnge("stoff", i__4, "zzdsks"
	    "bf_", (ftnlen)587)], &stctr[(i__5 = j * 3 - 3) < 30000 && 0 <= 
	    i__5 ? i__5 : s_rnge("stctr", i__5, "zzdsksbf_", (ftnlen)587)], &
	    strad[(i__6 = j - 1) < 10000 && 0 <= i__6 ? i__6 : s_rnge("strad",
	     i__6, "zzdsksbf_", (ftnlen)587)], vertex, raydir, xpt, &segidx, 
	    locdc, locic, found);
    chkout_("ZZSBFXR", (ftnlen)7);
    return 0;
/* $Procedure ZZSBFXRI ( DSK, unprioritized intercept with info ) */

L_zzsbfxri:
/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines. Users should not call this routine directly due to the */
/*     volatile nature of this routine. */

/*     Prepare and execute unprioritized ray-surface intercept */
/*     computation using DSK API segment buffers. Return source */
/*     information including handle, DLA descriptor, and */
/*     segment-specific information such as plate ID. */

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

/*     DLA */
/*     DSK */
/*     TOPOGRAPHY */
/*     UTILITY */

/* $ Declarations */

/*     INTEGER               BODYID */
/*     INTEGER               NSURF */
/*     INTEGER               SRFLST ( * ) */
/*     DOUBLE PRECISION      ET */
/*     INTEGER               FIXFID */
/*     DOUBLE PRECISION      VERTEX ( 3 ) */
/*     DOUBLE PRECISION      RAYDIR ( 3 ) */
/*     DOUBLE PRECISION      XPT    ( 3 ) */
/*     INTEGER               HANDLE */
/*     INTEGER               DLADSC ( * ) */
/*     DOUBLE PRECISION      DSKDSC ( * ) */
/*     DOUBLE PRECISION      DC     ( * ) */
/*     INTEGER               IC     ( * ) */
/*     LOGICAL               FOUND */

/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     BODYID     I   ID code of target body. */
/*     NSURF      I   Number of surface IDs in list. */
/*     SRFLST     I   Surface ID list. */
/*     ET         I   Evaluation epoch, seconds past J2000 TDB. */
/*     FIXFID     I   Frame ID of ray and intercept. */
/*     VERTEX     I   Ray's vertex. */
/*     RAYDIR     I   Ray's direction vector. */
/*     XPT        O   Surface intercept point. */
/*     HANDLE     O   Handle of DSK file. */
/*     DLADSC     O   DLA descriptor of segment. */
/*     DSKDSC     O   DSK descriptor of segment. */
/*     DC         O   Double precision component of source info. */
/*     IC         O   Integer component of source info. */
/*     FOUND      O   Found flag. True if intercept exists. */

/* $ Detailed_Input */

/*     BODYID     is the ID code of a target body. The ray-surface */
/*                intercept computation is performed using data */
/*                that represent the surface of this body. */

/*     NSURF, */
/*     SRFLST     are, respectively, a count of surface IDs and */
/*                a list of IDs. If the list is non-empty, only */
/*                the indicated surfaces will be used. If the */
/*                list is empty, all surfaces associated with the */
/*                input body ID will be considered. */

/*     ET         is the epoch for which the computation is to be */
/*                performed. This epoch is used for DSK segment */
/*                selection; only segments containing ET in their time */
/*                coverage interval will be used. ET is expressed as */
/*                seconds past J2000 TDB. */

/*     FIXFID     is the frame ID of a body-fixed frame associated */
/*                with the body designated by BODYID. This frame */
/*                is used to express the input ray's vertex and */
/*                direction vector. The output intercept will be */
/*                expressed in this frame as well. */

/*     VERTEX, */
/*     RAYDIR     are, respectively, the vertex and direction vector */
/*                of a ray. Both vectors are expressed in the frame */
/*                designated by FIXFID. */

/* $ Detailed_Output */

/*     XPT        is the surface intercept on the target body */
/*                nearest to the ray's vertex, if the intercept */
/*                exists. */

/*     HANDLE     is the handle of the DSK file that contributed */
/*                the segment in which the intercept was found. */

/*     DLADSC     is the DLA descriptor of the segment in which */
/*                the intercept was found. */

/*     DSKDSC     is the DSK descriptor of the segment in which */
/*                the intercept was found. */

/*     DC         is the double precision component of the source */
/*                data associated with the intercept. */

/*     IC         is the integer component of the source */
/*                data associated with the intercept. */

/*                   For type 2 segments, this component contains */
/*                   a plate ID in the first element. */

/*     FOUND      is a logical flag that is set to .TRUE. if and */
/*                only if an intercept exists. The other outputs */
/*                are valid if and only if FOUND is .TRUE. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If this routine is called directly, it signals the error */
/*         SPICE(BOGUSENTRY). */

/*     See the entry points for descriptions of errors specific to */
/*     those routines. */

/* $ Files */

/*     This routine makes use of DSK files loaded by the ZZDSKBSR */
/*     subsystem. */

/* $ Particulars */

/*     This routine is meant to be used only by the DSK subsystem. */

/*     This routine prepares the local buffers for a ray-surface */
/*     intercept computation using unprioritized DSK data. */
/*     It calls ZZDSKBUX to perform the computation. */

/* $ Examples */

/*     See usage in ZZRAYSFX. */

/* $ Restrictions */

/*     1) This is a private routine. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman   (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 22-FEB-2017 (NJB) */

/*        Added FAILED calls. */

/*        12-MAY-2016 (NJB) */

/* -& */
/* $ Index_Entries */

/*     unprioritized ray surface intercept with info */

/* -& */
    if (return_()) {
	return 0;
    }
    chkin_("ZZSBFXRI", (ftnlen)8);
    if (first) {

/*        Initialize BSR counter. */

	zzctruin_(bsrctr);
	first = FALSE_;
    }

/*     See whether the state of the loaded DSK set has changed */
/*     since the last call. */

    zzdskchk_(bsrctr, &update);
    if (update) {

/*        Make sure the ZZDSKBSR subsystem has completed the segment */
/*        list for the input body since the last time the BSR loaded */
/*        kernel state changed. */

	zzdskbbl_(bodyid);

/*        Initialize the local buffers. We restart from scratch */
/*        each time the BSR loaded kernel state changes. */

	zzdsksbi_(&c__10, &c__10000, btbody, &btnbod, btsegp, btstsz, sthan, 
		stdscr, stdlad, &stfree, stoff, stctr, strad);
    }
    if (failed_()) {
	chkout_("ZZSBFXRI", (ftnlen)8);
	return 0;
    }

/*     Find the index of the input body ID in the body table. If */
/*     we re-initialized the tables, the index will be zero. */

    bix = isrchi_(bodyid, &btnbod, btbody);
    if (bix == 0) {

/*        We don't have buffered information for this body. Update */
/*        the body and segment tables to store data for it. */

	zzdsksba_(bodyid, &c__10, &c__10000, btbody, &btnbod, btsegp, btstsz, 
		sthan, stdscr, stdlad, &stfree, stoff, stctr, strad);
	if (failed_()) {
	    chkout_("ZZSBFXRI", (ftnlen)8);
	    return 0;
	}

/*        The new body's position in the body table is at the end. */

	bix = btnbod;
    }

/*     Find the ray-surface intercept, using the buffered segment */
/*     data. */

    j = btsegp[(i__1 = bix - 1) < 10 && 0 <= i__1 ? i__1 : s_rnge("btsegp", 
	    i__1, "zzdsksbf_", (ftnlen)882)];
    nseg = btstsz[(i__1 = bix - 1) < 10 && 0 <= i__1 ? i__1 : s_rnge("btstsz",
	     i__1, "zzdsksbf_", (ftnlen)883)];
    zzdskbux_(bodyid, nsurf, srflst, et, fixfid, &nseg, &sthan[(i__1 = j - 1) 
	    < 10000 && 0 <= i__1 ? i__1 : s_rnge("sthan", i__1, "zzdsksbf_", (
	    ftnlen)885)], &stdlad[(i__2 = (j << 3) - 8) < 80000 && 0 <= i__2 ?
	     i__2 : s_rnge("stdlad", i__2, "zzdsksbf_", (ftnlen)885)], &
	    stdscr[(i__3 = j * 24 - 24) < 240000 && 0 <= i__3 ? i__3 : s_rnge(
	    "stdscr", i__3, "zzdsksbf_", (ftnlen)885)], &stoff[(i__4 = j * 3 
	    - 3) < 30000 && 0 <= i__4 ? i__4 : s_rnge("stoff", i__4, "zzdsks"
	    "bf_", (ftnlen)885)], &stctr[(i__5 = j * 3 - 3) < 30000 && 0 <= 
	    i__5 ? i__5 : s_rnge("stctr", i__5, "zzdsksbf_", (ftnlen)885)], &
	    strad[(i__6 = j - 1) < 10000 && 0 <= i__6 ? i__6 : s_rnge("strad",
	     i__6, "zzdsksbf_", (ftnlen)885)], vertex, raydir, xpt, &segidx, 
	    dc, ic, found);
    if (failed_()) {
	chkout_("ZZSBFXRI", (ftnlen)8);
	return 0;
    }
    if (*found) {

/*        Adjust the segment index to make it relative to the base value */
/*        1, instead of the current base J. */

	segidx = segidx + j - 1;
	*handle = sthan[(i__1 = segidx - 1) < 10000 && 0 <= i__1 ? i__1 : 
		s_rnge("sthan", i__1, "zzdsksbf_", (ftnlen)903)];
	movei_(&stdlad[(i__1 = (segidx << 3) - 8) < 80000 && 0 <= i__1 ? i__1 
		: s_rnge("stdlad", i__1, "zzdsksbf_", (ftnlen)905)], &c__8, 
		dladsc);
	moved_(&stdscr[(i__1 = segidx * 24 - 24) < 240000 && 0 <= i__1 ? i__1 
		: s_rnge("stdscr", i__1, "zzdsksbf_", (ftnlen)906)], &c__24, 
		dskdsc);
    }
    chkout_("ZZSBFXRI", (ftnlen)8);
    return 0;
/* $Procedure ZZSBFNRM ( DSK, prepare and compute unprioritized normal ) */

L_zzsbfnrm:
/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines. Users should not call this routine directly due to the */
/*     volatile nature of this routine. */

/*     Prepare and execute unprioritized outward surface normal */
/*     computation using DSK API segment buffers. */

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

/*     DLA */
/*     DSK */
/*     TOPOGRAPHY */
/*     UTILITY */

/* $ Declarations */

/*     INTEGER               BODYID */
/*     INTEGER               NSURF */
/*     INTEGER               SRFLST ( * ) */
/*     DOUBLE PRECISION      ET */
/*     INTEGER               FIXFID */
/*     DOUBLE PRECISION      POINT  ( 3 ) */
/*     DOUBLE PRECISION      NORMAL ( 3 ) */

/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     BODYID     I   ID code of target body. */
/*     NSURF      I   Number of surface IDs in list. */
/*     SRFLST     I   Surface ID list. */
/*     ET         I   Evaluation epoch, seconds past J2000 TDB. */
/*     FIXFID     I   Frame ID of surface point and normal vector. */
/*     POINT      I   Surface point. */
/*     NORMAL     O   Unit length outward normal at input point. */

/* $ Detailed_Input */

/*     BODYID     is the ID code of a target body. The outward */
/*                normal vector computation is performed using data */
/*                that represent the surface of this body. */

/*     NSURF, */
/*     SRFLST     are, respectively, a count of surface IDs and */
/*                a list of IDs. If the list is non-empty, only */
/*                the indicated surfaces will be used. If the */
/*                list is empty, all surfaces associated with the */
/*                input body ID will be considered. */

/*     ET         is the epoch for which the computation is to be */
/*                performed. This epoch is used for DSK segment */
/*                selection; only segments containing ET in their time */
/*                coverage interval will be used. ET is expressed as */
/*                seconds past J2000 TDB. */

/*     FIXFID     is the frame ID of a body-fixed frame associated with */
/*                the body designated by BODYID. This frame is used to */
/*                express the input surface point. The output normal */
/*                vector will be expressed in this frame as well. */

/*     POINT      is a surface point on the body designated by BODYID. */
/*                POINT is not arbitrary: it must be within a small */
/*                tolerance of the actual surface. Usually POINT is */
/*                obtained as the output of a SPICE computation. */

/*                POINT is expressed in the frame designated by FIXFID. */

/* $ Detailed_Output */

/*     NORMAL     is the unit-length outward surface normal vector at */
/*                the input POINT. */

/*                NORMAL is expressed in the frame designated by FIXFID. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If this routine is called directly, it signals the error */
/*         SPICE(BOGUSENTRY). */

/*     See the entry points for descriptions of errors specific to */
/*     those routines. */

/* $ Files */

/*     This routine makes use of DSK files loaded by the ZZDSKBSR */
/*     subsystem. */

/* $ Particulars */

/*     This routine is meant to be used only by the DSK subsystem. */

/*     This routine prepares the local buffers for a surface normal */
/*     vector computation using unprioritized DSK data. It calls */
/*     ZZDSKBUN to perform the computation. */

/* $ Examples */

/*     See usage in DSKNRM (provisional name). */

/* $ Restrictions */

/*     1) This is a private routine. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman   (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 22-FEB-2017 (NJB) */

/*        Added FAILED calls. */

/*        12-MAY-2016 (NJB) */

/* -& */
/* $ Index_Entries */

/*     prepare unprioritized normal vector computation */

/* -& */
    if (return_()) {
	return 0;
    }
    chkin_("ZZSBFNRM", (ftnlen)8);

/*     The following block of code is straight cut-and-paste from */
/*     ZZSBFXR. (We need to consider packaging this code in a */
/*     utility routine.) */

    if (first) {

/*        Initialize BSR counter. */

	zzctruin_(bsrctr);
	first = FALSE_;
    }

/*     See whether the state of the loaded DSK set has changed */
/*     since the last call. */

    zzdskchk_(bsrctr, &update);
    if (update) {

/*        Make sure the ZZDSKBSR subsystem has completed the segment */
/*        list for the input body since the last time the BSR loaded */
/*        kernel state changed. */

	zzdskbbl_(bodyid);

/*        Initialize the local buffers. We restart from scratch */
/*        each time the BSR loaded kernel state changes. */

	zzdsksbi_(&c__10, &c__10000, btbody, &btnbod, btsegp, btstsz, sthan, 
		stdscr, stdlad, &stfree, stoff, stctr, strad);
    }
    if (failed_()) {
	chkout_("ZZSBFNRM", (ftnlen)8);
	return 0;
    }

/*     Find the index of the input body ID in the body table. If */
/*     we re-initialized the tables, the index will be zero. */

    bix = isrchi_(bodyid, &btnbod, btbody);
    if (bix == 0) {

/*        We don't have buffered information for this body. Update */
/*        the body and segment tables to store data for it. */

	zzdsksba_(bodyid, &c__10, &c__10000, btbody, &btnbod, btsegp, btstsz, 
		sthan, stdscr, stdlad, &stfree, stoff, stctr, strad);
	if (failed_()) {
	    chkout_("ZZSBFNRM", (ftnlen)8);
	    return 0;
	}

/*        The new body's position in the body table is at the end. */

	bix = btnbod;
    }

/*     Find the outward unit normal vector, using the buffered segment */
/*     data. */

    j = btsegp[(i__1 = bix - 1) < 10 && 0 <= i__1 ? i__1 : s_rnge("btsegp", 
	    i__1, "zzdsksbf_", (ftnlen)1165)];
    nseg = btstsz[(i__1 = bix - 1) < 10 && 0 <= i__1 ? i__1 : s_rnge("btstsz",
	     i__1, "zzdsksbf_", (ftnlen)1166)];
    zzdskbun_(bodyid, nsurf, srflst, et, fixfid, &nseg, &sthan[(i__1 = j - 1) 
	    < 10000 && 0 <= i__1 ? i__1 : s_rnge("sthan", i__1, "zzdsksbf_", (
	    ftnlen)1168)], &stdlad[(i__2 = (j << 3) - 8) < 80000 && 0 <= i__2 
	    ? i__2 : s_rnge("stdlad", i__2, "zzdsksbf_", (ftnlen)1168)], &
	    stdscr[(i__3 = j * 24 - 24) < 240000 && 0 <= i__3 ? i__3 : s_rnge(
	    "stdscr", i__3, "zzdsksbf_", (ftnlen)1168)], &stoff[(i__4 = j * 3 
	    - 3) < 30000 && 0 <= i__4 ? i__4 : s_rnge("stoff", i__4, "zzdsks"
	    "bf_", (ftnlen)1168)], &stctr[(i__5 = j * 3 - 3) < 30000 && 0 <= 
	    i__5 ? i__5 : s_rnge("stctr", i__5, "zzdsksbf_", (ftnlen)1168)], &
	    strad[(i__6 = j - 1) < 10000 && 0 <= i__6 ? i__6 : s_rnge("strad",
	     i__6, "zzdsksbf_", (ftnlen)1168)], point, normal);
    chkout_("ZZSBFNRM", (ftnlen)8);
    return 0;
} /* zzdsksbf_ */

/* Subroutine */ int zzdsksbf_(integer *bodyid, integer *nsurf, integer *
	srflst, doublereal *et, integer *fixfid, doublereal *vertex, 
	doublereal *raydir, doublereal *point, doublereal *xpt, integer *
	handle, integer *dladsc, doublereal *dskdsc, doublereal *dc, integer *
	ic, logical *found, doublereal *normal)
{
    return zzdsksbf_0_(0, bodyid, nsurf, srflst, et, fixfid, vertex, raydir, 
	    point, xpt, handle, dladsc, dskdsc, dc, ic, found, normal);
    }

/* Subroutine */ int zzsbfxr_(integer *bodyid, integer *nsurf, integer *
	srflst, doublereal *et, integer *fixfid, doublereal *vertex, 
	doublereal *raydir, doublereal *xpt, logical *found)
{
    return zzdsksbf_0_(1, bodyid, nsurf, srflst, et, fixfid, vertex, raydir, (
	    doublereal *)0, xpt, (integer *)0, (integer *)0, (doublereal *)0, 
	    (doublereal *)0, (integer *)0, found, (doublereal *)0);
    }

/* Subroutine */ int zzsbfxri_(integer *bodyid, integer *nsurf, integer *
	srflst, doublereal *et, integer *fixfid, doublereal *vertex, 
	doublereal *raydir, doublereal *xpt, integer *handle, integer *dladsc,
	 doublereal *dskdsc, doublereal *dc, integer *ic, logical *found)
{
    return zzdsksbf_0_(2, bodyid, nsurf, srflst, et, fixfid, vertex, raydir, (
	    doublereal *)0, xpt, handle, dladsc, dskdsc, dc, ic, found, (
	    doublereal *)0);
    }

/* Subroutine */ int zzsbfnrm_(integer *bodyid, integer *nsurf, integer *
	srflst, doublereal *et, integer *fixfid, doublereal *point, 
	doublereal *normal)
{
    return zzdsksbf_0_(3, bodyid, nsurf, srflst, et, fixfid, (doublereal *)0, 
	    (doublereal *)0, point, (doublereal *)0, (integer *)0, (integer *)
	    0, (doublereal *)0, (doublereal *)0, (integer *)0, (logical *)0, 
	    normal);
    }

