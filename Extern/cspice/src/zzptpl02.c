/* zzptpl02.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__17 = 17;
static integer c__1 = 1;
static integer c__3 = 3;
static integer c__18 = 18;
static integer c__4 = 4;
static integer c__5 = 5;
static integer c__8 = 8;
static integer c__0 = 0;
static integer c__14 = 14;
static integer c__10 = 10;
static integer c__11 = 11;
static integer c__9 = 9;
static integer c__19 = 19;

/* $Procedure ZZPTPL02 ( DSK, map point to plate, type 2 ) */
/* Subroutine */ int zzptpl02_(integer *handle, integer *dladsc, doublereal *
	dskdsc, doublereal *point, integer *plid, integer *plate, doublereal *
	verts, logical *found)
{
    /* Initialized data */

    static doublereal limit = -1.;
    static logical pass1 = TRUE_;
    static integer prvdsc[8] = { 0,0,0,0,0,0,0,0 };
    static integer prvhan = 0;

    /* System generated locals */
    integer i__1, i__2;
    doublereal d__1;

    /* Builtin functions */
    integer i_dnnt(doublereal *), s_rnge(char *, integer, char *, integer);

    /* Local variables */
    doublereal dmin__, dist;
    static doublereal maxr;
    extern doublereal vdot_(doublereal *, doublereal *);
    extern integer zzvox2id_(integer *, integer *);
    extern /* Subroutine */ int vsub_(doublereal *, doublereal *, doublereal *
	    ), zzsegbox_(doublereal *, doublereal *, doublereal *), zzinvelt_(
	    doublereal *, integer *, doublereal *, doublereal *, doublereal *,
	     integer *, logical *);
    integer i__, j, k, n;
    extern /* Subroutine */ int dskd02_(integer *, integer *, integer *, 
	    integer *, integer *, integer *, doublereal *), zzvoxcvo_(integer 
	    *, integer *, integer *, integer *, integer *, integer *);
    integer nread;
    extern /* Subroutine */ int chkin_(char *, ftnlen), dski02_(integer *, 
	    integer *, integer *, integer *, integer *, integer *, integer *);
    doublereal pnear[3];
    extern doublereal dpmax_(void);
    extern /* Subroutine */ int moved_(doublereal *, integer *, doublereal *),
	     movei_(integer *, integer *, integer *);
    integer start;
    extern /* Subroutine */ int pltnp_(doublereal *, doublereal *, doublereal 
	    *, doublereal *, doublereal *, doublereal *);
    extern logical failed_(void);
    integer cgroff[3];
    extern integer brckti_(integer *, integer *, integer *);
    extern logical dlassg_(integer *, integer *, integer *, integer *), 
	    return_(void);
    doublereal boxctr[3], offset[3], normal[3], pntoff[3], ptsrfm;
    static doublereal voxori[3], voxsiz;
    doublereal vrttmp[9]	/* was [3][3] */, xpdfrc, xverts[9]	/* 
	    was [3][3] */;
    integer cgrcor[3], cgrext[3], cgrptr;
    static integer cgrscl, corsys;
    integer cgrvid, nplate, pidtmp, pltbuf[1000], pltptr, plttmp[3], ptrloc, 
	    ptroff, remain, vgrcor[3];
    static integer vgrext[3];
    integer vid;
    logical inside;
    extern /* Subroutine */ int chkout_(char *, ftnlen), setmsg_(char *, 
	    ftnlen), sigerr_(char *, ftnlen), dskgtl_(integer *, doublereal *)
	    , pltexp_(doublereal *, doublereal *, doublereal *), pltnrm_(
	    doublereal *, doublereal *, doublereal *, doublereal *), vhatip_(
	    doublereal *);

/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines. Users should not call this routine directly due to the */
/*     volatile nature of this routine. */

/*     Map a point to the nearest plate in a specified type 2 DSK */
/*     segment. */

/*     The point is expressed in the reference frame of the segment and */
/*     represents an offset from the center of the segment's reference */
/*     frame. */

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
/*     INTERCEPT */
/*     INTERSECTION */
/*     RAY */
/*     SURFACE */
/*     TOPOGRAPHY */

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


/*     Include file dsk02.inc */

/*     This include file declares parameters for DSK data type 2 */
/*     (plate model). */

/* -       SPICELIB Version 1.0.0 08-FEB-2017 (NJB) */

/*          Updated version info. */

/*           22-JAN-2016 (NJB) */

/*              Now includes spatial index parameters. */

/*           26-MAR-2015 (NJB) */

/*              Updated to increase MAXVRT to 16000002. MAXNPV */
/*              has been changed to (3/2)*MAXPLT. Set MAXVOX */
/*              to 100000000. */

/*           13-MAY-2010 (NJB) */

/*              Updated to reflect new no-record design. */

/*           04-MAY-2010 (NJB) */

/*              Updated for new type 2 segment design. Now uses */
/*              a local parameter to represent DSK descriptor */
/*              size (NB). */

/*           13-SEP-2008 (NJB) */

/*              Updated to remove albedo information. */
/*              Updated to use parameter for DSK descriptor size. */

/*           27-DEC-2006 (NJB) */

/*              Updated to remove minimum and maximum radius information */
/*              from segment layout.  These bounds are now included */
/*              in the segment descriptor. */

/*           26-OCT-2006 (NJB) */

/*              Updated to remove normal, center, longest side, albedo, */
/*              and area keyword parameters. */

/*           04-AUG-2006 (NJB) */

/*              Updated to support coarse voxel grid.  Area data */
/*              have now been removed. */

/*           10-JUL-2006 (NJB) */


/*     Each type 2 DSK segment has integer, d.p., and character */
/*     components.  The segment layout in DAS address space is as */
/*     follows: */


/*        Integer layout: */

/*           +-----------------+ */
/*           | NV              |  (# of vertices) */
/*           +-----------------+ */
/*           | NP              |  (# of plates ) */
/*           +-----------------+ */
/*           | NVXTOT          |  (total number of voxels) */
/*           +-----------------+ */
/*           | VGREXT          |  (voxel grid extents, 3 integers) */
/*           +-----------------+ */
/*           | CGRSCL          |  (coarse voxel grid scale, 1 integer) */
/*           +-----------------+ */
/*           | VOXNPT          |  (size of voxel-plate pointer list) */
/*           +-----------------+ */
/*           | VOXNPL          |  (size of voxel-plate list) */
/*           +-----------------+ */
/*           | VTXNPL          |  (size of vertex-plate list) */
/*           +-----------------+ */
/*           | PLATES          |  (NP 3-tuples of vertex IDs) */
/*           +-----------------+ */
/*           | VOXPTR          |  (voxel-plate pointer array) */
/*           +-----------------+ */
/*           | VOXPLT          |  (voxel-plate list) */
/*           +-----------------+ */
/*           | VTXPTR          |  (vertex-plate pointer array) */
/*           +-----------------+ */
/*           | VTXPLT          |  (vertex-plate list) */
/*           +-----------------+ */
/*           | CGRPTR          |  (coarse grid occupancy pointers) */
/*           +-----------------+ */



/*        D.p. layout: */

/*           +-----------------+ */
/*           | DSK descriptor  |  DSKDSZ elements */
/*           +-----------------+ */
/*           | Vertex bounds   |  6 values (min/max for each component) */
/*           +-----------------+ */
/*           | Voxel origin    |  3 elements */
/*           +-----------------+ */
/*           | Voxel size      |  1 element */
/*           +-----------------+ */
/*           | Vertices        |  3*NV elements */
/*           +-----------------+ */


/*     This local parameter MUST be kept consistent with */
/*     the parameter DSKDSZ which is declared in dskdsc.inc. */


/*     Integer item keyword parameters used by fetch routines: */


/*     Double precision item keyword parameters used by fetch routines: */


/*     The parameters below formerly were declared in pltmax.inc. */

/*     Limits on plate model capacity: */

/*     The maximum number of bodies, vertices and */
/*     plates in a plate model or collective thereof are */
/*     provided here. */

/*     These values can be used to dimension arrays, or to */
/*     use as limit checks. */

/*     The value of MAXPLT is determined from MAXVRT via */
/*     Euler's Formula for simple polyhedra having triangular */
/*     faces. */

/*     MAXVRT is the maximum number of vertices the triangular */
/*            plate model software will support. */


/*     MAXPLT is the maximum number of plates that the triangular */
/*            plate model software will support. */


/*     MAXNPV is the maximum allowed number of vertices, not taking into */
/*     account shared vertices. */

/*     Note that this value is not sufficient to create a vertex-plate */
/*     mapping for a model of maximum plate count. */


/*     MAXVOX is the maximum number of voxels. */


/*     MAXCGR is the maximum size of the coarse voxel grid. */


/*     MAXEDG is the maximum allowed number of vertex or plate */
/*     neighbors a vertex may have. */

/*     DSK type 2 spatial index parameters */
/*     =================================== */

/*        DSK type 2 spatial index integer component */
/*        ------------------------------------------ */

/*           +-----------------+ */
/*           | VGREXT          |  (voxel grid extents, 3 integers) */
/*           +-----------------+ */
/*           | CGRSCL          |  (coarse voxel grid scale, 1 integer) */
/*           +-----------------+ */
/*           | VOXNPT          |  (size of voxel-plate pointer list) */
/*           +-----------------+ */
/*           | VOXNPL          |  (size of voxel-plate list) */
/*           +-----------------+ */
/*           | VTXNPL          |  (size of vertex-plate list) */
/*           +-----------------+ */
/*           | CGRPTR          |  (coarse grid occupancy pointers) */
/*           +-----------------+ */
/*           | VOXPTR          |  (voxel-plate pointer array) */
/*           +-----------------+ */
/*           | VOXPLT          |  (voxel-plate list) */
/*           +-----------------+ */
/*           | VTXPTR          |  (vertex-plate pointer array) */
/*           +-----------------+ */
/*           | VTXPLT          |  (vertex-plate list) */
/*           +-----------------+ */


/*        Index parameters */


/*     Grid extent: */


/*     Coarse grid scale: */


/*     Voxel pointer count: */


/*     Voxel-plate list count: */


/*     Vertex-plate list count: */


/*     Coarse grid pointers: */


/*     Size of fixed-size portion of integer component: */


/*        DSK type 2 spatial index double precision component */
/*        --------------------------------------------------- */

/*           +-----------------+ */
/*           | Vertex bounds   |  6 values (min/max for each component) */
/*           +-----------------+ */
/*           | Voxel origin    |  3 elements */
/*           +-----------------+ */
/*           | Voxel size      |  1 element */
/*           +-----------------+ */



/*        Index parameters */

/*     Vertex bounds: */


/*     Voxel grid origin: */


/*     Voxel size: */


/*     Size of fixed-size portion of double precision component: */


/*     The limits below are used to define a suggested maximum */
/*     size for the integer component of the spatial index. */


/*     Maximum number of entries in voxel-plate pointer array: */


/*     Maximum cell size: */


/*     Maximum number of entries in voxel-plate list: */


/*     Spatial index integer component size: */


/*     End of include file dsk02.inc */


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

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     HANDLE     I   DSK file handle. */
/*     DLADSC     I   DLA descriptor of segment. */
/*     DSKDSC     I   DSK descriptor of segment. */
/*     POINT      I   Input point. */
/*     PLID       O   Plate ID. */
/*     PLATE      O   Plate, expressed as an array of three vertex IDs. */
/*     VERTS      O   Vertices of plate. */
/*     FOUND      O   Found flag. */
/*     PTMEMM     P   Default margin for point-plate distance. */
/*     XFRACT     P   Default plate expansion fraction. */

/* $ Detailed_Input */

/*     HANDLE     is the handle of a DSK file containing a type 2 DSK */
/*                segment. The input point is to be matched with the */
/*                closest plate in this segment. */

/*     DLADSC     is the DLA descriptor of the DSK segment to be used. */

/*     DSKDSC     is the DSK descriptor of the DSK segment to be used. */

/*     POINT      is a point that is on or very near the surface */
/*                described by the DSK segment. POINT is expressed in */
/*                the reference frame of the segment and represents an */
/*                offset from the center of that frame. The frame center */
/*                may be distinct from the central body of the segment. */

/* $ Detailed_Output */

/*     PLID       is the ID of the plate nearest to the input point, if */
/*                such a plate is found. A plate ID is the index of a */
/*                plate in the set of plates contained in the segment. */
/*                PLID is 1-based. */

/*                The distances between POINT and the respective plates */
/*                are computed using the plate expansion factor XFRACT. */
/*                See the Parameters section below and the routine */
/*                PLTEXP for details. */

/*                A plate will be found only if POINT is sufficiently */
/*                close to at least one plate in the segment, when plate */
/*                expansion is taken into account. A margin is used to */
/*                determine whether a plate is close enough to the point */
/*                to be considered as a solution. See the Parameters */
/*                section for details. */


/*     PLATE      is the plate designated by PLID. It is an array of */
/*                three integers. The integers are the IDs of the */
/*                plate's vertices. */

/*     VERTS      is an array containing the plate's vertices. These */
/*                a 3-dimensional, double precision vectors. The Ith */
/*                vertex is stored in elements */

/*                   VERTS(1:3,I) */

/*     FOUND      is a logical flag that is set to .TRUE. if and only */
/*                if plate nearest to the input point was found */

/*                The outputs PLID, PLATE, and VERTS are valid if and */
/*                only if FOUND is .TRUE. */

/* $ Parameters */

/*     See the include file */

/*        dsktol.inc */

/*     for declarations of these parameters. These defaults can be */
/*     overridden. See DSKSTL for details. */


/*     PTMEMM     is the default value of the point-plate membership */
/*                parameter, which is used to determine whether a point */
/*                is close enough to a plate to be considered as a */
/*                solution. Let S be the current value of this */
/*                parameter; the bounding radius of the segment is */
/*                multiplied by (1+S) to produce a distance used for */
/*                this comparison. */

/*     XFRACT     is the default value of an expansion factor applied to */
/*                each plate before the distance of POINT from the plate */
/*                is computed. Let S be the current value of this */
/*                parameter; each plate is expanded by a factor of (1+S) */
/*                to produce a plate used for the membership test. */

/*                This expansion is performed to keep results of this */
/*                routine consistent with those of DSKX02, which also */
/*                performs plate expansion. */

/* $ Exceptions */

/*     1)  If an invalid voxel edge length is detected, the error */
/*         SPICE(VALUEOUTOFRANGE) is signaled. */

/*     2)  If the coarse voxel scale is zero, the error */
/*         SPICE(VALUEOUTOFRANGE) is signaled. */

/*     3)  If an unrecognized coordinate system is encountered, the */
/*         error SPICE(NOTSUPPORTED) is signaled. */

/*     4)  If an error occurs while this routine attempts to read */
/*         DSK data, the error will be signaled by a routine in */
/*         the call tree of this routine. */

/* $ Files */

/*     Appropriate kernels must be loaded by the calling program before */
/*     this routine is called. */

/*     The following data are required: */

/*        - DSK data: the DSK file designated by HANDLE and containing */
/*          the segment having the DLA descriptor DLADSC must be loaded */
/*          at the time this routine is called. */

/*     Kernel data are normally loaded once per program run, NOT every */
/*     time this routine is called. */

/* $ Particulars */

/*     This routine supports geometry routines that must associate */
/*     a computed surface point with the plate to which that point */
/*     belongs. */

/* $ Examples */

/*     See usage in ZZDSKNRM. */

/* $ Restrictions */

/*     This is a private routine. It is meant to be used only by the DSK */
/*     subsystem. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman    (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.1, 03-OCT-2021 (NJB) */

/*        Corrected typo in comments. */

/* -    SPICELIB Version 1.0.0, 22-FEB-2017 (NJB) */

/*        Added FAILED calls. */

/*        23-AUG-2016 (NJB) */

/*        Bug fix: now saves previous handle and DLA descriptor. */

/*        17-JUN-2016 (NJB) */

/*           Added support for planetodetic coordinates. Updated */
/*           bounding radius computation for segments using latitudinal */
/*           coordinates; this routine now calls ZZSEGBOX. Changed */
/*           implementation to use ZZINVELT and ZZVOXCVO. Updated */
/*           parameter descriptions. */

/*        07-OCT-2015 (NJB) */

/*           Now uses plate expansion before testing point-plate */
/*           distance. */

/*        15-DEC-2014 (NJB) */

/* -& */
/* $ Index_Entries */

/*     map point to plate in type 2 dsk segment */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     Local variables */


/*     Saved variables */


/*     Initial values */

    if (return_()) {
	return 0;
    }
    chkin_("ZZPTPL02", (ftnlen)8);

/*     No plate has been found so far. */

    *found = FALSE_;

/*     Decide whether we're looking at the segment we saw */
/*     on the previous call. */

    if (pass1 || ! dlassg_(handle, &prvhan, dladsc, prvdsc)) {

/*        We'll need to look up the voxel grid parameters for this */
/*        segment. */

	dskd02_(handle, dladsc, &c__17, &c__1, &c__3, &n, voxori);
	dskd02_(handle, dladsc, &c__18, &c__1, &c__1, &n, &voxsiz);
	dski02_(handle, dladsc, &c__4, &c__1, &c__3, &n, vgrext);
	dski02_(handle, dladsc, &c__5, &c__1, &c__1, &n, &cgrscl);
	if (failed_()) {
	    chkout_("ZZPTPL02", (ftnlen)8);
	    return 0;
	}
	if (voxsiz == 0.) {
	    setmsg_("Voxel edge length is zero; length must be positive.", (
		    ftnlen)51);
	    sigerr_("SPICE(VALUEOUTOFRANGE)", (ftnlen)22);
	    chkout_("ZZPTPL02", (ftnlen)8);
	    return 0;
	}
	if (cgrscl == 0) {
	    setmsg_("Coarse voxel scale is zero; scale must be positive.", (
		    ftnlen)51);
	    sigerr_("SPICE(VALUEOUTOFRANGE)", (ftnlen)22);
	    chkout_("ZZPTPL02", (ftnlen)8);
	    return 0;
	}
	corsys = i_dnnt(&dskdsc[5]);
	zzsegbox_(dskdsc, boxctr, &maxr);
	if (failed_()) {
	    chkout_("ZZPTPL02", (ftnlen)8);
	    return 0;
	}

/*        We successfully obtained the desired segment parameters, so we */
/*        don't need to execute this code again until the segment */
/*        changes. Save the current handle and DLA descriptor. */

	prvhan = *handle;
	movei_(dladsc, &c__8, prvdsc);
	pass1 = FALSE_;
    }

/*     Look up the point-plate membership margin; compute */
/*     the distance limit. This call must be made on every */
/*     call to ZZPTPL02. */

    dskgtl_(&c__4, &ptsrfm);
    limit = ptsrfm * maxr;

/*     Look up the plate expansion fraction. This call must be made on */
/*     every call to ZZPTPL02. */

    dskgtl_(&c__1, &xpdfrc);

/*     Find out whether the point is within the volume element */
/*     bounding the segment. */

    zzinvelt_(point, &corsys, &dskdsc[6], &dskdsc[16], &ptsrfm, &c__0, &
	    inside);
    if (failed_()) {
	chkout_("ZZPTPL02", (ftnlen)8);
	return 0;
    }
    if (! inside) {

/*        The point is too far from the segment to be considered */
/*        to lie on a plate in that segment. */

	chkout_("ZZPTPL02", (ftnlen)8);
	return 0;
    }

/*     Map the point to the coordinates of a voxel containing it. If the */
/*     point is outside the voxel grid, map the point to the closest */
/*     voxel. */

    vsub_(point, voxori, offset);
    for (i__ = 1; i__ <= 3; ++i__) {
	j = (integer) (offset[(i__1 = i__ - 1) < 3 && 0 <= i__1 ? i__1 : 
		s_rnge("offset", i__1, "zzptpl02_", (ftnlen)471)] / voxsiz) + 
		1;
	vgrcor[(i__1 = i__ - 1) < 3 && 0 <= i__1 ? i__1 : s_rnge("vgrcor", 
		i__1, "zzptpl02_", (ftnlen)473)] = brckti_(&j, &c__1, &vgrext[
		(i__2 = i__ - 1) < 3 && 0 <= i__2 ? i__2 : s_rnge("vgrext", 
		i__2, "zzptpl02_", (ftnlen)473)]);
    }

/*     Compute the coordinates of the coarse voxel containing the fine */
/*     voxel we just identified. Get the 1-d offset of the fine voxel */
/*     relative the coarse voxel; this offset gives us the index of the */
/*     pointer associating the fine voxel with its plate list. The */
/*     1-d offset PTROFF is 1-based. */

    zzvoxcvo_(vgrcor, vgrext, &cgrscl, cgrcor, cgroff, &ptroff);
    if (failed_()) {
	chkout_("ZZPTPL02", (ftnlen)8);
	return 0;
    }

/*     Fetch the pointer from the coarse voxel to the first element of */
/*     its fine voxel pointer array. */

/*     We'll need the 1-D offset of the coarse voxel from the base of */
/*     the coarse voxel grid. */

    for (i__ = 1; i__ <= 3; ++i__) {
	cgrext[(i__1 = i__ - 1) < 3 && 0 <= i__1 ? i__1 : s_rnge("cgrext", 
		i__1, "zzptpl02_", (ftnlen)499)] = vgrext[(i__2 = i__ - 1) < 
		3 && 0 <= i__2 ? i__2 : s_rnge("vgrext", i__2, "zzptpl02_", (
		ftnlen)499)] / cgrscl;
    }
    cgrvid = zzvox2id_(cgrcor, cgrext);
    dski02_(handle, dladsc, &c__14, &cgrvid, &c__1, &n, &cgrptr);
    if (failed_()) {
	chkout_("ZZPTPL02", (ftnlen)8);
	return 0;
    }
    if (cgrptr < 1) {

/*        There are no non-empty fine voxels, hence no plates, in the */
/*        coarse voxel we're looking at. */

	chkout_("ZZPTPL02", (ftnlen)8);
	return 0;
    }

/*     Look up the pointer to the plate list for this voxel, and if */
/*     the pointer is non-null, look up the plate count. */

    ptrloc = cgrptr - 1 + ptroff;
    dski02_(handle, dladsc, &c__10, &ptrloc, &c__1, &n, &pltptr);
    if (failed_() || pltptr < 1) {
	chkout_("ZZPTPL02", (ftnlen)8);
	return 0;
    }
    dski02_(handle, dladsc, &c__11, &pltptr, &c__1, &n, &nplate);
    if (failed_() || nplate < 1) {
	chkout_("ZZPTPL02", (ftnlen)8);
	return 0;
    }

/*     Loop through the plates, keeping track of the minimum plate-point */
/*     distance. */

    dmin__ = dpmax_();
    remain = nplate;
    nread = min(remain,1000);
    i__ = 1;
    while(remain > 0) {

/*        Look up the current set of plate IDs. */

	i__1 = pltptr + i__;
	dski02_(handle, dladsc, &c__11, &i__1, &nread, &n, pltbuf);
	if (failed_()) {
	    chkout_("ZZPTPL02", (ftnlen)8);
	    return 0;
	}

/*        Look up the vertices of each plate in the buffer and find */
/*        the distance of the point from that plate. Quit if we */
/*        find a match. */

	i__1 = nread;
	for (j = 1; j <= i__1; ++j) {
	    pidtmp = pltbuf[(i__2 = j - 1) < 1000 && 0 <= i__2 ? i__2 : 
		    s_rnge("pltbuf", i__2, "zzptpl02_", (ftnlen)567)];
	    start = (pidtmp - 1) * 3 + 1;
	    dski02_(handle, dladsc, &c__9, &start, &c__3, &n, plttmp);
	    for (k = 1; k <= 3; ++k) {
		vid = plttmp[(i__2 = k - 1) < 3 && 0 <= i__2 ? i__2 : s_rnge(
			"plttmp", i__2, "zzptpl02_", (ftnlen)574)];
		start = (vid - 1) * 3 + 1;
		dskd02_(handle, dladsc, &c__19, &start, &c__3, &n, &vrttmp[(
			i__2 = k * 3 - 3) < 9 && 0 <= i__2 ? i__2 : s_rnge(
			"vrttmp", i__2, "zzptpl02_", (ftnlen)577)]);
	    }
	    if (failed_()) {
		chkout_("ZZPTPL02", (ftnlen)8);
		return 0;
	    }

/*           Work with an expanded version of the plate. */

	    pltexp_(vrttmp, &xpdfrc, xverts);
	    pltnrm_(xverts, &xverts[3], &xverts[6], normal);
	    if (failed_()) {
		chkout_("ZZPTPL02", (ftnlen)8);
		return 0;
	    }
	    vhatip_(normal);
	    vsub_(point, xverts, pntoff);
	    if ((d__1 = vdot_(pntoff, normal), abs(d__1)) <= limit) {

/*              The input point lies in a narrow region of space */
/*              bounded by two planes, both of which are parallel */
/*              to the plate. The plate lies between the planes. */

/*              This test does not rule out a comparison between POINT */
/*              and a distant plate, if POINT is close to the plane */
/*              containing that plate. However, the proportion of such */
/*              cases will normally be small. */

		pltnp_(point, xverts, &xverts[3], &xverts[6], pnear, &dist);
		if (failed_()) {
		    chkout_("ZZPTPL02", (ftnlen)8);
		    return 0;
		}
	    } else {
		dist = dpmax_();
	    }
	    if (dist <= limit) {

/*              We have a reasonable candidate for the closest plate. */

		*found = TRUE_;
		if (dist < dmin__) {
		    dmin__ = dist;
		    *plid = pidtmp;

/*                 Set the output vertices to the original version. */

		    movei_(plttmp, &c__3, plate);
		    moved_(vrttmp, &c__9, verts);

/*                 We'll return the above values if we don't find */
/*                 a better match. */

		}
	    }
	}

/*        Prepare to read the next set of plate IDs, if any. */

	remain -= nread;
	i__ += nread;
	nread = min(remain,1000);
    }
    chkout_("ZZPTPL02", (ftnlen)8);
    return 0;
} /* zzptpl02_ */

