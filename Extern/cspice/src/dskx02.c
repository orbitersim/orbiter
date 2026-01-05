/* dskx02.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__200 = 200;
static integer c_b27 = 100000;
static integer c__14 = 14;
static integer c__1 = 1;
static integer c__8 = 8;
static integer c__50000 = 50000;
static doublereal c_b40 = 1.;
static doublereal c_b41 = -1.;
static integer c__10 = 10;
static integer c__11 = 11;
static integer c_b89 = 256000;
static integer c__9 = 9;
static integer c__3 = 3;
static integer c__19 = 19;
static integer c__2 = 2;
static integer c__0 = 0;

/* $Procedure DSKX02 ( DSK, ray-surface intercept, type 2 ) */
/* Subroutine */ int dskx02_(integer *handle, integer *dladsc, doublereal *
	vertex, doublereal *raydir, integer *plid, doublereal *xpt, logical *
	found)
{
    /* Initialized data */

    static doublereal grdext[3] = { -1.,-1.,-1. };
    static integer prvhan = 0;
    static integer prvdsc[8] = { 0,0,0,0,0,0,0,0 };

    /* System generated locals */
    integer i__1, i__2, i__3, i__4, i__5, i__6;
    doublereal d__1, d__2;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer), i_dnnt(doublereal *);

    /* Local variables */
    doublereal vtx2[3];
    extern /* Subroutine */ int xdda_(doublereal *, doublereal *, integer *, 
	    integer *, integer *, integer *);
    logical have;
    doublereal near__;
    integer cvid;
    static integer ncgr;
    extern /* Subroutine */ int vhat_(doublereal *, doublereal *);
    doublereal udir[3];
    integer vloc, vids[3];
    logical hits;
    extern doublereal vdot_(doublereal *, doublereal *);
    extern /* Subroutine */ int vsub_(doublereal *, doublereal *, doublereal *
	    ), vequ_(doublereal *, doublereal *);
    integer pntr;
    static doublereal xtol;
    extern /* Subroutine */ int zztogrid_(doublereal *, doublereal *, 
	    doublereal *, doublereal *), zzinvelt_(doublereal *, integer *, 
	    doublereal *, doublereal *, doublereal *, integer *, logical *), 
	    zzraybox_(doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, logical *);
    integer i__, j, k;
    extern /* Subroutine */ int dskb02_(integer *, integer *, integer *, 
	    integer *, integer *, doublereal *, doublereal *, doublereal *, 
	    integer *, integer *, integer *, integer *, integer *);
    doublereal edges[9]	/* was [3][3] */, scale;
    integer final, w;
    extern /* Subroutine */ int dskd02_(integer *, integer *, integer *, 
	    integer *, integer *, integer *, doublereal *), chkin_(char *, 
	    ftnlen), dski02_(integer *, integer *, integer *, integer *, 
	    integer *, integer *, integer *), dskgd_(integer *, integer *, 
	    doublereal *), filli_(integer *, integer *, integer *);
    logical inseg;
    doublereal coord[3];
    static doublereal vbuff[600]	/* was [3][200] */;
    extern doublereal dpmax_(void);
    extern /* Subroutine */ int movei_(integer *, integer *, integer *);
    integer nvbuf;
    extern /* Subroutine */ int vlcom_(doublereal *, doublereal *, doublereal 
	    *, doublereal *, doublereal *);
    static integer vxlcg[150000]	/* was [3][50000] */;
    logical extra;
    extern /* Subroutine */ int vsubg_(doublereal *, doublereal *, integer *, 
	    doublereal *);
    integer group, start;
    static integer vidxs[200];
    logical invox;
    integer cgxyz[3];
    extern logical vzero_(doublereal *);
    doublereal xpnts[9]	/* was [3][3] */;
    static integer cgscl2;
    extern /* Subroutine */ int vlcom3_(doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *);
    extern logical failed_(void);
    static integer cgscal;
    integer fx, fy, fz;
    static integer np;
    doublereal greedm;
    static integer nv;
    integer grpbeg, to, vi;
    extern logical return_(void);
    extern integer isrchi_(integer *, integer *, integer *);
    static doublereal dskdsc[24], grdtol;
    doublereal hitcor[3], normal[3], obsmat[9]	/* was [3][3] */, points[9]	
	    /* was [3][3] */;
    static doublereal voxori[3], voxsiz, vtxbds[6]	/* was [2][3] */;
    doublereal vtxoff[3], xpdfrc;
    static integer cgrext[3], cgrptr[100000], corsys;
    integer dim, grpend, grpsiz, minidx, ngroup, nplate, nvxout;
    static integer nvxtot;
    integer offset;
    static integer ordvec[256000], platid[256000];
    integer plroom;
    static integer source[256000];
    integer totplt;
    static integer vgrext[3], voxlst[150000]	/* was [3][50000] */, voxnpl, 
	    voxnpt;
    integer voxptr;
    static integer vtxnpl, vxlout[50000], vxlstr[50000];
    logical boxhit, newseg;
    extern /* Subroutine */ int cleari_(integer *, integer *), setmsg_(char *,
	     ftnlen), sigerr_(char *, ftnlen), chkout_(char *, ftnlen), 
	    errint_(char *, integer *, ftnlen), dskgtl_(integer *, doublereal 
	    *), orderi_(integer *, integer *, integer *), pltexp_(doublereal *
	    , doublereal *, doublereal *);
    integer vxc1, vxc2, vxc3;
    extern /* Subroutine */ int insang_(doublereal *, doublereal *, 
	    doublereal *, doublereal *, logical *, doublereal *), pltnrm_(
	    doublereal *, doublereal *, doublereal *, doublereal *);
    doublereal xpt2[3];

/* $ Abstract */

/*     Determine the plate ID and body-fixed coordinates of the */
/*     intersection of a specified ray with the surface defined by a */
/*     type 2 DSK plate model. */

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

/*     GEOMETRY */
/*     SHAPES */

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
/*     HANDLE     I   Handle of DSK kernel containing plate model. */
/*     DLADSC     I   DLA descriptor of plate model segment. */
/*     VERTEX     I   Ray's vertex in the  body fixed frame. */
/*     RAYDIR     I   Ray direction in the body fixed frame. */
/*     PLID       O   ID code of the plate intersected by the ray. */
/*     XPT        O   Intercept. */
/*     FOUND      O   Flag indicating whether intercept exists. */
/*     XFRACT     P   Plate expansion fraction. */

/* $ Detailed_Input */

/*     HANDLE   is the file handle of a DSK file containing a shape */
/*              model for a target body. The shape model is stored */
/*              in a type 2 DSK segment. */

/*     DLADSC   is the DLA descriptor of a type 2 DSK segment */
/*              containing plate model data representing the surface of */
/*              the target body. The caller should declare DLADSC */
/*              with size DLADSZ; this size parameter is defined in */
/*              the INCLUDE file dla.inc. Normally this descriptor */
/*              will be obtained by a search through a DSK file */
/*              using the DLA search routines; see the $Examples */
/*              header section below for a working code example */
/*              illustrating a simple search. */

/*     VERTEX   is the vertex of a ray. VERTEX is expressed relative */
/*              to the body fixed reference frame associated with the */
/*              target body. This reference frame is the same frame */
/*              relative to which the vertices of the plate model */
/*              are expressed. Units are km. */

/*              The vertex is required to be outside the target */
/*              body. */

/*     RAYDIR   is the ray's direction vector. RAYDIR is expressed */
/*              relative to the body fixed reference frame associated */
/*              with the target body. */

/* $ Detailed_Output */

/*     PLID     is the ID of the plate closest to the input ray's */
/*              vertex at which a ray-surface intercept exists. */
/*              If no intercept exists, PLID is undefined. */

/*     XPT      is the ray-target intercept closest to the ray's */
/*              vertex, if an intercept exists. XPT is expressed */
/*              relative to the body-fixed reference frame associated */
/*              with the target body. Units are km. */

/*              If no intercept exists, XPT is undefined. */

/*     FOUND    is a logical flag that indicates whether or not the */
/*              ray does indeed intersect the target. If the ray */
/*              intersects a plate FOUND is .TRUE. Otherwise FOUND is */
/*              .FALSE. */

/* $ Parameters */

/*     XFRACT   is the default plate expansion fraction. This */
/*              parameter can be overridden. */

/*     See the include file */

/*        dsktol.inc */

/*     for the values of tolerance parameters used by default by the */
/*     ray-surface intercept algorithm. */

/*     See the include file */

/*        dla.inc */

/*     for declarations of DLA descriptor sizes and documentation of the */
/*     contents of DLA descriptors. */

/*     See the include file */

/*        dskdsc.inc */

/*     for declarations of DSK descriptor sizes and documentation of the */
/*     contents of DSK descriptors. */

/*     See the include file */

/*        dsk02.inc */

/*     for declarations of DSK data type 2 (plate model) parameters. */

/* $ Exceptions */

/*     1)  If the input handle is invalid, an error is signaled by a */
/*         routine in the call tree of this routine. */

/*     2)  If a file read error occurs, the error is signaled by a */
/*         routine in the call tree of this routine. */

/*     3)  If the input DLA descriptor is invalid, the effect of this */
/*         routine is undefined. The error *may* be diagnosed by */
/*         routines in the call tree of this routine, but there are no */
/*         guarantees. */

/*     4)  If an error occurs while trying to look up any component */
/*         of the shape model, the error is signaled by a routine in the */
/*         call tree of this routine. */

/*     5)  If the input ray direction is the zero vector, the error */
/*         SPICE(ZEROVECTOR) is signaled. */

/*     6)  If the coarse voxel grid scale of the shape model is less */
/*         than 1, the error SPICE(VALUEOUTOFRANGE) is signaled. */

/*     7)  If the coarse voxel grid of the shape model contains more */
/*         than MAXCGR (see dsk02.inc) voxels, the error */
/*         SPICE(GRIDTOOLARGE) is signaled. */

/*     8)  If the plate list for any intersected voxel is too large */
/*         for this routine to buffer, the error SPICE(ARRAYTOOSMALL) */
/*         is signaled. */

/*     9)  Due to round-off errors, results from this routine may */
/*         differ across platforms. Results also may differ from */
/*         those expected---and not necessarily by a small amount. */
/*         For example, a ray may miss a plate it was expected to */
/*         hit and instead hit another plate considerably farther */
/*         from the ray's vertex, or miss the target entirely. */

/*     10) In the event that an intercept point lies on multiple */
/*         plates (that is, the point is on an edge or vertex), */
/*         a plate will be selected. Due to round-off error, the */
/*         selection may vary across platforms. */

/* $ Files */

/*     See the description of the input argument HANDLE. */

/* $ Particulars */

/*     This routine solves the ray-surface intercept problem for */
/*     a specified ray and a surface represented by triangular plate */
/*     model. The surface representation is provided by data in a */
/*     type 2 segment of a DSK file. */

/*     This routine does not assume that the segment from which the */
/*     surface model data are read represents the entire surface of */
/*     the target body. A program could call this routine repeatedly */
/*     to find the surface intercept of a ray and a shape model */
/*     partitioned into multiple segments. */

/*     In general, this routine should be expected to run faster */
/*     when used with smaller shape models. */

/* $ Examples */

/*     The numerical results shown for this example may differ across */
/*     platforms. The results depend on the SPICE kernels used as input, */
/*     the compiler and supporting libraries, and the machine specific */
/*     arithmetic implementation. */

/*     1) Find the surface intercept points corresponding to a latitude/ */
/*        longitude grid of a specified resolution, for a specified */
/*        target body. */

/*        This simple program assumes the shape model for the target */
/*        body is stored in a single type 2 DSK segment, and that this */
/*        segment is the first one in the DSK file to which it belongs. */


/*        Example code begins here. */


/*              PROGRAM DSKX02_EX1 */
/*              IMPLICIT NONE */

/*              INCLUDE 'dla.inc' */
/*              INCLUDE 'dskdsc.inc' */
/*              INCLUDE 'dsk02.inc' */
/*        C */
/*        C */
/*        C     SPICELIB functions */
/*        C */
/*              DOUBLE PRECISION      DSKSGR */
/*              DOUBLE PRECISION      RPD */
/*        C */
/*        C */
/*        C     Local parameters */
/*        C */
/*              INTEGER               FILSIZ */
/*              PARAMETER           ( FILSIZ = 255 ) */

/*              INTEGER               NLAT */
/*              PARAMETER           ( NLAT   = 9 ) */

/*              INTEGER               NLON */
/*              PARAMETER           ( NLON   = 9 ) */

/*        C */
/*        C     Local parameters */
/*        C */
/*              DOUBLE PRECISION      TOL */
/*              PARAMETER           ( TOL   =  1.D-12 ) */

/*        C */
/*        C     Local variables */
/*        C */
/*              CHARACTER*(FILSIZ)    DSK */

/*              DOUBLE PRECISION      DSKDSC ( DSKDSZ ) */
/*              DOUBLE PRECISION      LAT */
/*              DOUBLE PRECISION      LON */
/*              DOUBLE PRECISION      MAXR */
/*              DOUBLE PRECISION      R */
/*              DOUBLE PRECISION      RAYDIR ( 3 ) */
/*              DOUBLE PRECISION      VERTEX ( 3 ) */
/*              DOUBLE PRECISION      XLAT */
/*              DOUBLE PRECISION      XLON */
/*              DOUBLE PRECISION      XPT    ( 3 ) */
/*              DOUBLE PRECISION      XR */

/*              INTEGER               DLADSC ( DLADSZ ) */
/*              INTEGER               HANDLE */
/*              INTEGER               I */
/*              INTEGER               J */
/*              INTEGER               PLID */

/*              LOGICAL               FOUND */

/*        C */
/*        C     Prompt for the name of the DSK to read. */
/*        C */
/*              CALL PROMPT ( 'Enter DSK name > ', DSK ) */
/*        C */
/*        C     Open the DSK file for read access. */
/*        C     We use the DAS-level interface for */
/*        C     this function. */
/*        C */
/*              CALL DASOPR ( DSK, HANDLE ) */
/*        C */
/*        C     Begin a forward search through the */
/*        C     kernel, treating the file as a DLA. */
/*        C     In this example, it's a very short */
/*        C     search. */
/*        C */
/*              CALL DLABFS ( HANDLE, DLADSC, FOUND ) */

/*              IF ( .NOT. FOUND ) THEN */
/*        C */
/*        C        We arrive here only if the kernel */
/*        C        contains no segments.  This is */
/*        C        unexpected, but we're prepared for it. */
/*        C */
/*                 CALL SETMSG ( 'No segments found ' */
/*             .   //            'in DSK file #.'    ) */
/*                 CALL ERRCH  ( '#',  DSK           ) */
/*                 CALL SIGERR ( 'SPICE(NODATA)'     ) */

/*              END IF */

/*        C */
/*        C     If we made it this far, DLADSC is the */
/*        C     DLA descriptor of the first segment. */
/*        C */
/*        C     We're going to generate the intercept points */
/*        C     using a set of rays which point toward the */
/*        C     origin and whose vertices are on a */
/*        C     specified lat/lon grid.  To start out we */
/*        C     must pick a reasonable range from the origin */
/*        C     for the vertices:  the range must be large */
/*        C     enough so that the vertices are guaranteed */
/*        C     to be outside the target body but small */
/*        C     enough that we don't lose too much precision */
/*        C     in the surface intercept computation. */
/*        C */
/*        C     We'll look up the upper bound for the target */
/*        C     radius, then use 2 times this value as the */
/*        C     vertex magnitude. */
/*        C */
/*              CALL DSKGD ( HANDLE, DLADSC, DSKDSC ) */

/*              MAXR = DSKDSC(MX3IDX) */
/*              R    = 2.D0 * MAXR */

/*        C */
/*        C     Now generate the intercept points.  We generate */
/*        C     intercepts along latitude bounds, working from */
/*        C     north to south. Latitude ranges from +80 to */
/*        C     -80 degrees. Longitude ranges from 0 to 320 */
/*        C     degrees. The increment is 20 degrees for */
/*        C     latitude and 40 degrees for longitude. */
/*        C */
/*              DO I = 1, NLAT */

/*                 LAT = RPD() * ( 100.D0 - 20.D0*I ) */

/*                 DO J = 1, NLON */

/*                    LON = RPD() * 40.D0 * (J-1) */
/*        C */
/*        C           Produce a ray vertex for the current */
/*        C           lat/lon value.  Negate the vertex to */
/*        C           produce the ray's direction vector. */
/*        C */
/*                    CALL LATREC ( R, LON, LAT, VERTEX ) */
/*                    CALL VMINUS ( VERTEX,      RAYDIR ) */
/*        C */
/*        C           Find the surface intercept for this */
/*        C           ray. */
/*        C */
/*                    CALL DSKX02 ( HANDLE, DLADSC, VERTEX, */
/*             .                    RAYDIR, PLID,   XPT,    FOUND  ) */
/*        C */
/*        C           Since the ray passes through the origin on */
/*        C           the body-fixed frame associated with the */
/*        C           target body, we'd rarely expect to find that */
/*        C           the ray failed to intersect the surface. */
/*        C           For safety, we check the FOUND flag.  (A */
/*        C           "not found" condition could be a sign of */
/*        C           a bug.) */
/*        C */
/*                    IF ( .NOT. FOUND ) THEN */

/*                       WRITE(*,*) ' ' */
/*                       WRITE(*,*) 'Intercept not found!' */
/*                       WRITE(*,*) '   Ray vertex:' */
/*                       WRITE(*,*) '   Longitude (deg): ', LON/RPD() */
/*                       WRITE(*,*) '   Latitude  (deg): ', LAT/RPD() */
/*                       WRITE(*,*) '   Range      (km): ', R */
/*                       WRITE(*,*) ' ' */

/*                    ELSE */
/*        C */
/*        C              This is the normal case.  Display the */
/*        C              intercept plate ID and the intercept */
/*        C              point in both Cartesian and latitudinal */
/*        C              coordinates.  Show the corresponding ray */
/*        C              vertex to facilitate validation of results. */
/*        C */
/*        C              Use RECRAD rather than RECLAT to produce */
/*        C              non-negative longitudes. */
/*        C */
/*                       CALL RECRAD ( XPT, XR, XLON, XLAT ) */

/*                       WRITE(*,*) ' ' */
/*                       WRITE(*,*) 'Intercept found:' */
/*                       WRITE(*,*) '   Plate ID:             ', PLID */
/*                       WRITE(*, '(1X,A,3F12.8)' ) */
/*             .         '   Cartesian coordinates:', XPT */
/*                       WRITE(*,*) '   Latitudinal coordinates:' */
/*                       WRITE(*,*) '   Longitude (deg): ', XLON/RPD() */
/*                       WRITE(*,*) '   Latitude  (deg): ', XLAT/RPD() */
/*                       WRITE(*,*) '   Range      (km): ', XR */
/*                       WRITE(*,*) */
/*                       WRITE(*,*) '   Ray vertex:' */
/*                       WRITE(*,*) '   Longitude (deg): ', LON/RPD() */
/*                       WRITE(*,*) '   Latitude  (deg): ', LAT/RPD() */
/*                       WRITE(*,*) '   Range      (km): ', R */
/*                       WRITE(*,*) ' ' */

/*                    END IF */

/*                 END DO */

/*              END DO */
/*        C */
/*        C     Close the kernel.  This isn't necessary in a stand- */
/*        C     alone program, but it's good practice in subroutines */
/*        C     because it frees program and system resources. */
/*        C */
/*              CALL DASCLS ( HANDLE ) */
/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, using the DSK file named phobos_3_3.bds, the output */
/*        was: */


/*        Enter DSK name > phobos_3_3.bds */

/*         Intercept found: */
/*            Plate ID:                   306238 */
/*            Cartesian coordinates:  1.52087789  0.00000000  8.62532711 */
/*            Latitudinal coordinates: */
/*            Longitude (deg):    0.0000000000000000 */
/*            Latitude  (deg):    80.000000000000014 */
/*            Range      (km):    8.7583866856211490 */

/*            Ray vertex: */
/*            Longitude (deg):    0.0000000000000000 */
/*            Latitude  (deg):    80.000000000000000 */
/*            Range      (km):    28.023536291251524 */


/*         Intercept found: */
/*            Plate ID:                   317112 */
/*            Cartesian coordinates:  1.18970365  0.99827989  8.80777185 */
/*            Latitudinal coordinates: */
/*            Longitude (deg):    40.000000000000000 */
/*            Latitude  (deg):    80.000000000000000 */
/*            Range      (km):    8.9436459265318629 */

/*            Ray vertex: */
/*            Longitude (deg):    40.000000000000000 */
/*            Latitude  (deg):    80.000000000000000 */
/*            Range      (km):    28.023536291251524 */


/*         Intercept found: */
/*            Plate ID:                   324141 */
/*            Cartesian coordinates:  0.27777518  1.57534131  9.07202903 */
/*            Latitudinal coordinates: */
/*            Longitude (deg):    80.000000000000028 */
/*            Latitude  (deg):    80.000000000000014 */
/*            Range      (km):    9.2119797003191017 */

/*            Ray vertex: */
/*            Longitude (deg):    80.000000000000000 */
/*            Latitude  (deg):    80.000000000000000 */
/*            Range      (km):    28.023536291251524 */


/*         Intercept found: */
/*            Plate ID:                   327994 */
/*            Cartesian coordinates: -0.81082405  1.40438846  9.19682344 */
/*            Latitudinal coordinates: */
/*            Longitude (deg):    120.00000000000001 */
/*            Latitude  (deg):    80.000000000000000 */
/*            Range      (km):    9.3386992651610452 */

/*            Ray vertex: */
/*            Longitude (deg):    119.99999999999999 */
/*            Latitude  (deg):    80.000000000000000 */
/*            Range      (km):    28.023536291251524 */


/*         Intercept found: */
/*            Plate ID:                   329431 */
/*            Cartesian coordinates: -1.47820193  0.53802150  8.92132122 */
/*            Latitudinal coordinates: */
/*            Longitude (deg):    160.00000000000006 */
/*            Latitude  (deg):    80.000000000000014 */
/*            Range      (km):    9.0589469760393797 */

/*            Ray vertex: */
/*            Longitude (deg):    160.00000000000000 */
/*            Latitude  (deg):    80.000000000000000 */
/*            Range      (km):    28.023536291251524 */


/*         Intercept found: */
/*            Plate ID:                   196042 */
/*            Cartesian coordinates: -1.49854761 -0.54542673  9.04411256 */
/*            Latitudinal coordinates: */
/*            Longitude (deg):    200.00000000000000 */
/*            Latitude  (deg):    80.000000000000000 */
/*            Range      (km):    9.1836325764960041 */

/*            Ray vertex: */
/*            Longitude (deg):    200.00000000000000 */
/*            Latitude  (deg):    80.000000000000000 */
/*            Range      (km):    28.023536291251524 */


/*         Intercept found: */
/*            Plate ID:                   235899 */
/*            Cartesian coordinates: -0.78240454 -1.35516441  8.87447325 */
/*            Latitudinal coordinates: */
/*            Longitude (deg):    239.99999999999991 */
/*            Latitude  (deg):    80.000000000000000 */
/*            Range      (km):    9.0113763066160804 */

/*            Ray vertex: */
/*            Longitude (deg):    239.99999999999997 */
/*            Latitude  (deg):    80.000000000000000 */
/*            Range      (km):    28.023536291251524 */



/*        [...] */


/*        Warning: incomplete output. Only 100 out of 1135 lines have */
/*        been provided. */


/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     [1]  A. Woo, "Fast Ray-Box Intersection", Graphic Gems I, */
/*          395-396, Aug. 1990 */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J.A. Bytof         (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     B.V. Semenov       (JPL) */
/*     E.D. Wright        (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 13-JAN-2021 (NJB) (JDR) (BVS) */

/*        Bug fix: in some cases the previous version of this routine */
/*        could still return an intercept outside of the segment */
/*        boundaries by more than the allowed margin. In those cases, */
/*        the returned plate ID was invalid. Both problems have been */
/*        corrected. */

/*        See $Revisions for details. */

/*        Edited the header to comply with NAIF standard. Updated the */
/*        example code to retrieve directly from the DSK descriptor the */
/*        upper bound for the target radius. Added record in */
/*        $Index_Entries. */

/* -    SPICELIB Version 1.0.0, 04-APR-2017 (NJB) (EDW) (JAB) */

/*        Added test for containment of intersection point */
/*        within segment boundaries. Updated logic for saving */
/*        segment attributes so that errors won't cause saved */
/*        values to be improperly re-used on subsequent calls. */

/*        24-FEB-2016 (NJB) */

/*           Replaced call to TOGRID with call to ZZTOGRID. */
/*           Replaced call to PLTREC with call to PLTNRM. */
/*           Now obtains plate expansion fraction from DSKGTL. */

/*        25-FEB-2015 (NJB) */

/*           Bug fix: now ray-voxel grid intercept is displaced toward */
/*           the input ray's vertex only when the vertex is outside */
/*           the target body's voxel grid. */

/*        10-SEP-2014 (NJB) */

/*           Bug fix: during an intercept search over the voxel list */
/*           returned by XDDA, if an intercept outside the current */
/*           voxel---by more than a small tolerance---is found, the */
/*           search rejects that intercept and continues until a */
/*           valid intercept is found and all plates in the voxel */
/*           containing that intercept have been checked for an */
/*           intersection. The rejected intercept may later be */
/*           determined to be a valid solution during a check of */
/*           plates associated with a voxel that contains that */
/*           intercept; in fact it is the correct solution if no */
/*           other plates contain a solution closer to the ray's */
/*           vertex. (Usually there is a unique voxel containing the */
/*           intercept, but this is not so if the intercept lies on */
/*           a voxel boundary not on an edge of the voxel grid.) */

/*           Note that there's no need to look for intersections in */
/*           voxels further out in the voxel list than the first one */
/*           that contains an intercept. */

/*           The previous version of the routine terminated the */
/*           search after checking all plates in the current voxel, */
/*           after an intercept was found on any plate associated */
/*           with that voxel. The intercept was not required to be */
/*           contained in the voxel. */

/*           See the $Revisions section for details. */

/*        30-JUN-2014 (NJB) */

/*           Bug fix: renamed "found" flag returned by ZZRAYBOX. */

/*           Added code to test for empty voxel list after */
/*           voxel list compression. */

/*        15-JUN-2014 (NJB) */

/*           Made some minor edits to in-line comments, and removed */
/*           comments that had become inapplicable due to code changes. */

/*        06-JUN-2014 (NJB) */

/*           Now expands plates slightly before performing ray-plate */
/*           intersection computations. */

/*           Bug fix: now calls ZZRAYBOX to find the ray-box intercept. */
/*           This reduces round-off error in the variable COORD. */

/*        02-MAY-2014 (NJB) */

/*           Bug fix: added FAILED checks after each DSKI02 and DSKD02 */
/*           call. */

/*           Some precautionary measures were added: a backstop */
/*           check for an empty voxel list was added after the XDDA */
/*           call. A backstop initialization of PNTR was added */
/*           before the plate collection loop. This initialization */
/*           is needed only if the voxel list returned by XDDA is */
/*           empty. The list should never be empty. */

/*        25-MAR-2014 (NJB) */

/*           Bug fix: duplicate plates are now marked so that the */
/*           unmarked instance is the one in the closest voxel to */
/*           the ray's origin. */

/*           Bug fix: corrected buffer overflow error detection for */
/*           insertion of plate IDs into plate ID array. */

/*        20-JUL-2011 (NJB) */

/*           Bug fix: this routine now tests FAILED after its */
/*           call to XDDA. */

/*           Header correction: the detailed input section */
/*           now says that the ray's vertex *is* required to */
/*           be outside the target body. */

/*        09-JUN-2011 (NJB) */

/*           All large local arrays are now saved in order to support */
/*           calling a C translation of this routine from Java. */

/*           The buffer VIDXS is now initialized prior to its */
/*           first use in an argument list. This was done to */
/*           to suppress compiler warnings. The original code was */
/*           correct, since along with the buffer, an array size */
/*           of zero was passed to the called function. */

/*           The example program was updated for compatibility with */
/*           the final DSK descriptor layout. The output format was */
/*           adjusted. Sample output from the program is now shown */
/*           in the header. */

/*        13-MAY-2010 (NJB) */

/*           No longer uses plate records to weed out */
/*           plates prior to ray-plate intercept tests. */
/*           Now uses local vertex buffer. Logic for choosing */
/*           plate when intercept is on edge or vertex has */
/*           been simplified. */

/*        06-MAY-2010 (NJB) */

/*           Now calls DSKB02 rather than DSKP02. */

/*        20-APR-2010 (NJB) */

/*           Updated header section order. */

/*        26-SEP-2008 (NJB) */

/*           Moved OBSMAT computation out of loop. */

/*        27-DEC-2006 (NJB) (EDW) */

/*           Header example was updated to show maximum radius */
/*           being obtained from DSK descriptor rather than via */
/*           all to DSKD02. */

/*        31-OCT-2006 (NJB) (EDW) */

/*           Modified to work with DLA-based kernels. Many */
/*           changes were made to the algorithm to improve */
/*           execution speed. */

/*        19-AUG-2004 (EDW) */

/*           Implemented "Fast Ray-Box Intersection" algorithm. */
/*           Renamed routine DSKX02 from PLBORE_3. */

/*        25-FEB-1999 (JAB) */

/*           Based on PLBORE and PLBORE_2. */

/* -& */
/* $ Index_Entries */

/*     plate and plate model point intersected by ray */
/*     intersection of ray and surface */

/* -& */
/* $ Revisions */

/* -    SPICELIB Version 1.1.0, 13-JAN-2021 (NJB) */

/*        Bug fix: in some cases the previous version of this */
/*        routine could return an intercept outside of the segment */
/*        boundaries (the "outside intercept") by more than the */
/*        allowed margin. In those cases, the returned plate ID was */
/*        invalid. */

/*        Both problems have been corrected. */

/*        Description of the bug */
/*        ---------------------- */

/*        In the case where all of the follow conditions hold: */

/*           - the input ray's intercepts exist both within and */
/*             outside the segment's boundaries */

/*           - the outside intercept is considered the nearest */
/*             solution to the vertex at the time the intercept */
/*             is found */

/*           - the intercept that should have been selected was */
/*             found before the outside intercept */

/*           - both intercepts lie on plates belonging to the same voxel */

/*        the outside intercept will overwrite the correct intercept. */

/*        In the situation described above, the plate ID returned */
/*        will not be that of the outside plate. */

/*        Solution */
/*        -------- */

/*        Each intercept that passes the test for being closest, of */
/*        all intercepts seen so far, to the ray's vertex is stored */
/*        in a temporary variable. The output XPT is updated only */
/*        when the intercept is found to lie within the segment's */
/*        coordinate bounds, or outside the bounds by no more than */
/*        the allowed margin. */

/* -    SPICELIB Version 1.0.0, 04-APR-2017 (NJB) */

/*        10-SEP-2014 (NJB) */

/*           Bug fix: during an intercept search over the voxel */
/*           list returned by XDDA, if an intercept outside the */
/*           current voxel---by more than a small tolerance---is */
/*           found, the search rejects that intercept and continues */
/*           until a valid intercept is found and all plates in the */
/*           voxel containing that intercept have been checked for */
/*           an intersection. The rejected intercept may later be */
/*           determined to be a valid solution during a check of */
/*           plates associated with a voxel that contains that */
/*           intercept; in fact it is the correct solution if no */
/*           other plates contain a solution closer to the ray's */
/*           vertex. (Usually there is a unique voxel containing */
/*           the intercept, but this is not so if the intercept */
/*           lies on a voxel boundary not on an edge of the voxel */
/*           grid.) */

/*           Note that there's no need to look for intersections in */
/*           voxels further out in the voxel list than the first */
/*           one that contains an intercept. */

/*           The previous version of the routine terminated the */
/*           search after checking all plates in the current voxel, */
/*           after an intercept was found on any plate associated */
/*           with that voxel. The intercept was not required to be */
/*           contained in the voxel. */

/*           In the previous version of the routine, an intercept */
/*           found outside of the current voxel could effectively */
/*           mask an intercept closer to the ray's vertex, as shown */
/*           in the diagram below. */

/*           In this diagram, "V" represents the vertex of the ray. */
/*           The letter sequences "Q*" and "P*" represent plates. */
/*           Here the ray hits voxel 1 and finds an intercept on */
/*           plate P* at the point marked by "@." No other */
/*           intercepts on plates in voxel 1 exist, so the search */
/*           terminates. The intercept marked by "$" is closer to */
/*           the vertex but is not seen. */

/*                                   V */
/*                                  / */
/*                                 / */
/*                                / */
/*              +--------------+-/------------+ */
/*              |    voxel 2   |/    voxel 1  | */
/*              |              /              | */
/*              |        QQQQQ$|              | */
/*              |            / |              | */
/*              |           /  |              | */
/*              |          /   |              | */
/*              |       PP@PPPPPPPPPPPPPPP    | */
/*              +--------------+--------------+ */


/*           The updated algorithm, when presented with the */
/*           situation shown above, will check all plates in voxel */
/*           2 before terminating. */

/*           Note that the problem could occur in cases where */
/*           voxels 1 and 2 are not adjacent. */

/* -& */

/*     SPICELIB Functions */


/*     Statement function type declarations */


/*     Local parameters */


/*     Tolerance used for vertex-voxel grid distance test: */


/*     Maximum number of voxels we can accept for */
/*     one XDDA call. */


/*     Maximum number of plates we work with */
/*     at any time. */


/*     Parameter indicating no coordinates are to be excluded */
/*     in the test for a point being within segment boundaries. */


/*     Local Variables */


/*     Saved variables */


/*     Initial values */


/*     Statement functions */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }
    chkin_("DSKX02", (ftnlen)6);

/*     Until we have better knowledge we assume there is no intersection. */

    *plid = 0;
    *found = FALSE_;
    have = FALSE_;
    near__ = dpmax_();

/*     Initialize the vertex buffer. */

    cleari_(&c__200, vidxs);

/*     Check whether the ray direction vector is the zero vector. */

    if (vzero_(raydir)) {
	setmsg_("Ray direction is the zero vector.", (ftnlen)33);
	sigerr_("SPICE(RAYISZEROVECTOR)", (ftnlen)22);
	chkout_("DSKX02", (ftnlen)6);
	return 0;
    }

/*     Obtain the unit vector of the ray from the observer. */

    vhat_(raydir, udir);

/*     Decide whether we're looking at a new segment. */

    newseg = TRUE_;
    if (*handle == prvhan) {

/*        The input handle matches the previous handle.  Note that the */
/*        initial value of PRVHAN is 0, which is never a valid handle, */
/*        so on the first pass, this test will fail. */

	if (dladsc[2] == prvdsc[2] && dladsc[4] == prvdsc[4] && dladsc[6] == 
		prvdsc[6]) {

/*           All of the DLA segment base addresses match. */

	    newseg = FALSE_;
	}
    }
    if (newseg) {

/*        Make sure we can't have a match with an earlier */
/*        segment on a subsequent call, if we exit this */
/*        block due to an error. */

	prvhan = 0;

/*        Retrieve the voxel grid origin in model */
/*        units and calculate the farthest extent of the */
/*        voxel grid in voxel space. */

	dskb02_(handle, dladsc, &nv, &np, &nvxtot, vtxbds, &voxsiz, voxori, 
		vgrext, &cgscal, &vtxnpl, &voxnpt, &voxnpl);
	if (failed_()) {
	    chkout_("DSKX02", (ftnlen)6);
	    return 0;
	}

/*        Compute the grid dimensions in units of km. First check */
/*        the voxel size. */

	if (voxsiz == 0.) {
	    setmsg_("Voxel size is zero. This is an error in the DSK file at"
		    "tached to handle #.", (ftnlen)74);
	    errint_("#", handle, (ftnlen)1);
	    sigerr_("SPICE(INVALIDVALUE)", (ftnlen)19);
	    chkout_("DSKX02", (ftnlen)6);
	    return 0;
	}
	for (i__ = 1; i__ <= 3; ++i__) {
	    grdext[(i__1 = i__ - 1) < 3 && 0 <= i__1 ? i__1 : s_rnge("grdext",
		     i__1, "dskx02_", (ftnlen)1163)] = vgrext[(i__2 = i__ - 1)
		     < 3 && 0 <= i__2 ? i__2 : s_rnge("vgrext", i__2, "dskx0"
		    "2_", (ftnlen)1163)] * voxsiz;
	}

/*        Set the margin used for checking whether the ray's vertex */
/*        is close to the voxel grid. */

/* Computing MAX */
	d__1 = max(grdext[0],grdext[1]);
	grdtol = max(d__1,grdext[2]) * 1e-12;

/*        Check the coarse grid voxel scale. */

	if (cgscal < 1) {
	    setmsg_("Coarse grid scale = #; should be >= 1.", (ftnlen)38);
	    errint_("#", &cgscal, (ftnlen)1);
	    sigerr_("SPICE(VALUEOUTOFRANGE)", (ftnlen)22);
	    chkout_("DSKX02", (ftnlen)6);
	    return 0;
	}

/*        Get the coarse voxel grid dimensions and the coarse voxel */
/*        occupancy flags. */

	for (i__ = 1; i__ <= 3; ++i__) {
	    cgrext[(i__1 = i__ - 1) < 3 && 0 <= i__1 ? i__1 : s_rnge("cgrext",
		     i__1, "dskx02_", (ftnlen)1190)] = vgrext[(i__2 = i__ - 1)
		     < 3 && 0 <= i__2 ? i__2 : s_rnge("vgrext", i__2, "dskx0"
		    "2_", (ftnlen)1190)] / cgscal;
	}
	cgscl2 = cgscal * cgscal;
/* Computing 3rd power */
	i__1 = cgscal;
	ncgr = nvxtot / (i__1 * (i__1 * i__1));
	if (ncgr > 100000) {
	    setmsg_("Coarse grid size NCGR = #. Buffer size = #", (ftnlen)42);
	    errint_("#", &ncgr, (ftnlen)1);
	    errint_("#", &c_b27, (ftnlen)1);
	    sigerr_("SPICE(GRIDTOOLARGE)", (ftnlen)19);
	    chkout_("DSKX02", (ftnlen)6);
	    return 0;
	}
	dski02_(handle, dladsc, &c__14, &c__1, &c_b27, &dim, cgrptr);
	dskgd_(handle, dladsc, dskdsc);
	if (failed_()) {
	    chkout_("DSKX02", (ftnlen)6);
	    return 0;
	}
	corsys = i_dnnt(&dskdsc[5]);
	prvhan = *handle;
	movei_(dladsc, &c__8, prvdsc);
    }

/*     Compute tolerance used for determining whether an intercept */
/*     is inside a voxel. The expansion fraction must be fetched */
/*     on every call to DSKX02. */

    dskgtl_(&c__1, &xpdfrc);
/* Computing MAX */
    d__1 = abs(grdext[0]), d__2 = abs(grdext[1]), d__1 = max(d__1,d__2), d__2 
	    = abs(grdext[2]);
    xtol = xpdfrc * max(d__1,d__2);

/*     Find the ray intercept on the surface of the fine voxel grid, */
/*     if the intercept exists. */

    zzraybox_(vertex, raydir, voxori, grdext, vtx2, &boxhit);
    if (! boxhit) {
	chkout_("DSKX02", (ftnlen)6);
	return 0;
    }

/*     Convert the grid intercept to voxel space coordinates. */
/*     The result COORD will be used as the ray's vertex in XDDA. */

    zztogrid_(vtx2, voxori, &voxsiz, coord);

/*     Determine the voxels hit by the ray. */

    xdda_(coord, udir, vgrext, &c__50000, &nvxout, voxlst);
    if (failed_()) {
	chkout_("DSKX02", (ftnlen)6);
	return 0;
    }

/*     We don't expect the voxel list to be empty, but leave now */
/*     if it is. */

    if (nvxout == 0) {
	chkout_("DSKX02", (ftnlen)6);
	return 0;
    }

/*     Rather than using the original observer's location, we use a */
/*     location derived from COORD, which is the intercept of the ray */
/*     and the surface of the voxel grid.  We start with COORD, convert */
/*     this location to the model coordinate system, and back outward a */
/*     bit to make sure we obtain a location outside the grid (we don't */
/*     want to miss any plates that might be located right on the grid's */
/*     surface). */

/*     This vertex change is not performed if the vertex is already */
/*     inside, or within a small margin away from, the voxel grid. */

    vsub_(vertex, voxori, vtxoff);
    if (vtxoff[0] < -grdtol || vtxoff[0] > grdtol + grdext[0] || vtxoff[1] < 
	    -grdtol || vtxoff[1] > grdtol + grdext[1] || vtxoff[2] < -grdtol 
	    || vtxoff[2] > grdtol + grdext[2]) {

/*        The vertex is outside of the voxel grid by more than the */
/*        margin. Move the ray-grid intercept away from the grid to */
/*        improve numeric performance. */

	vlcom3_(&c_b40, voxori, &voxsiz, coord, &c_b41, udir, vtx2);
    }

/*     We are going to need to subtract the location of the observer */
/*     from vertices of a plate. To speed things up a tiny bit, we'll */
/*     make 3 copies of the observer's location so that we make a single */
/*     subroutine call to handle the 3 subtractions. */

    vequ_(vtx2, obsmat);
    vequ_(vtx2, &obsmat[3]);
    vequ_(vtx2, &obsmat[6]);

/*     Use the coarse voxel grid to compress the voxel list. We */
/*     remove all voxels belonging to empty coarse voxels. */

    to = 0;
    i__1 = nvxout;
    for (i__ = 1; i__ <= i__1; ++i__) {

/*        Find the coordinates, then the ID, of the coarse voxel */
/*        containing this voxel. */

	for (j = 1; j <= 3; ++j) {
	    cgxyz[(i__2 = j - 1) < 3 && 0 <= i__2 ? i__2 : s_rnge("cgxyz", 
		    i__2, "dskx02_", (ftnlen)1326)] = (voxlst[(i__3 = j + i__ 
		    * 3 - 4) < 150000 && 0 <= i__3 ? i__3 : s_rnge("voxlst", 
		    i__3, "dskx02_", (ftnlen)1326)] - 1) / cgscal + 1;
	}
	cvid = cgxyz[0] + cgrext[0] * (cgxyz[1] + cgxyz[2] * cgrext[1] - (
		cgrext[1] + 1));
	if (cgrptr[(i__2 = cvid - 1) < 100000 && 0 <= i__2 ? i__2 : s_rnge(
		"cgrptr", i__2, "dskx02_", (ftnlen)1333)] > 0) {

/*           This coarse voxel is non-empty; add the index of the */
/*           current voxel to the output list.  Save the coordinates of */
/*           the parent coarse voxel as well. */

	    ++to;
	    vxlout[(i__2 = to - 1) < 50000 && 0 <= i__2 ? i__2 : s_rnge("vxl"
		    "out", i__2, "dskx02_", (ftnlen)1340)] = i__;
	    for (j = 1; j <= 3; ++j) {
		vxlcg[(i__2 = j + to * 3 - 4) < 150000 && 0 <= i__2 ? i__2 : 
			s_rnge("vxlcg", i__2, "dskx02_", (ftnlen)1343)] = 
			cgxyz[(i__3 = j - 1) < 3 && 0 <= i__3 ? i__3 : s_rnge(
			"cgxyz", i__3, "dskx02_", (ftnlen)1343)];
	    }

/*           Save the coarse voxel start pointer as well. */

	    vxlstr[(i__2 = to - 1) < 50000 && 0 <= i__2 ? i__2 : s_rnge("vxl"
		    "str", i__2, "dskx02_", (ftnlen)1348)] = cgrptr[(i__3 = 
		    cvid - 1) < 100000 && 0 <= i__3 ? i__3 : s_rnge("cgrptr", 
		    i__3, "dskx02_", (ftnlen)1348)];
	}
    }

/*     Update NVXOUT to be the number of voxels in the compressed list. */

    nvxout = to;

/*     If the voxel list became empty after compression, we're */
/*     done. */

    if (nvxout == 0) {
	chkout_("DSKX02", (ftnlen)6);
	return 0;
    }

/*     Initialize PNTR in case the voxel list is empty. */
/*     (This is a backstop precaution: the voxel list */
/*     should never be empty at this point.) PNTR will */
/*     be referenced after the end of the outer loop below. */

    pntr = 1;

/*     The vertex buffer is empty so far. */

    nvbuf = 0;

/*     Break up the list of voxels into groups; process each */
/*     group in turn until we find an intersection or run out */
/*     of voxels. */

/* Computing MAX */
    i__1 = 1, i__2 = (nvxout + 1) / 2;
    grpsiz = max(i__1,i__2);
    ngroup = (nvxout - 1) / grpsiz + 1;
    group = 1;
    while(group <= ngroup && ! have) {
	pntr = 1;
	grpbeg = (group - 1) * grpsiz + 1;
/* Computing MIN */
	i__1 = grpbeg + grpsiz - 1;
	grpend = min(i__1,nvxout);
	plroom = 256000;
	i__1 = grpend;
	for (vi = grpbeg; vi <= i__1; ++vi) {

/*           Look up the plate list pointer for this voxel. */


/*           We begin by finding the offset of the voxel from */
/*           the base of its parent coarse voxel. */

	    j = vxlout[(i__2 = vi - 1) < 50000 && 0 <= i__2 ? i__2 : s_rnge(
		    "vxlout", i__2, "dskx02_", (ftnlen)1411)];
	    fx = voxlst[(i__2 = j * 3 - 3) < 150000 && 0 <= i__2 ? i__2 : 
		    s_rnge("voxlst", i__2, "dskx02_", (ftnlen)1413)] - cgscal 
		    * (vxlcg[(i__3 = vi * 3 - 3) < 150000 && 0 <= i__3 ? i__3 
		    : s_rnge("vxlcg", i__3, "dskx02_", (ftnlen)1413)] - 1);
	    fy = voxlst[(i__2 = j * 3 - 2) < 150000 && 0 <= i__2 ? i__2 : 
		    s_rnge("voxlst", i__2, "dskx02_", (ftnlen)1414)] - cgscal 
		    * (vxlcg[(i__3 = vi * 3 - 2) < 150000 && 0 <= i__3 ? i__3 
		    : s_rnge("vxlcg", i__3, "dskx02_", (ftnlen)1414)] - 1);
	    fz = voxlst[(i__2 = j * 3 - 1) < 150000 && 0 <= i__2 ? i__2 : 
		    s_rnge("voxlst", i__2, "dskx02_", (ftnlen)1415)] - cgscal 
		    * (vxlcg[(i__3 = vi * 3 - 1) < 150000 && 0 <= i__3 ? i__3 
		    : s_rnge("vxlcg", i__3, "dskx02_", (ftnlen)1415)] - 1);
	    offset = fx + cgscal * (fy - 1) + cgscl2 * (fz - 1);

/*           Now compute the index of voxel-plate list pointer in */
/*           the pointer array, and look up the pointer. */

	    j = vxlstr[(i__2 = vi - 1) < 50000 && 0 <= i__2 ? i__2 : s_rnge(
		    "vxlstr", i__2, "dskx02_", (ftnlen)1423)] + offset - 1;
	    dski02_(handle, dladsc, &c__10, &j, &c__1, &dim, &voxptr);
	    if (failed_()) {
		chkout_("DSKX02", (ftnlen)6);
		return 0;
	    }
	    if (voxptr == -1) {
		nplate = 0;
	    } else {

/*              Get the plate count for this voxel. */

		dski02_(handle, dladsc, &c__11, &voxptr, &c__1, &dim, &nplate)
			;
		if (failed_()) {
		    chkout_("DSKX02", (ftnlen)6);
		    return 0;
		}
	    }
	    if (nplate > 0) {
		if (nplate <= plroom) {

/*                 Get the plate list for this voxel. */

		    i__3 = voxptr + 1;
		    dski02_(handle, dladsc, &c__11, &i__3, &nplate, &dim, &
			    platid[(i__2 = pntr - 1) < 256000 && 0 <= i__2 ? 
			    i__2 : s_rnge("platid", i__2, "dskx02_", (ftnlen)
			    1457)]);
		    if (failed_()) {
			chkout_("DSKX02", (ftnlen)6);
			return 0;
		    }
		    plroom -= nplate;
		} else {
		    setmsg_("NPLATE = #. Available room in PLATID array = #."
			    " Array size = #.", (ftnlen)63);
		    errint_("#", &nplate, (ftnlen)1);
		    errint_("#", &plroom, (ftnlen)1);
		    errint_("#", &c_b89, (ftnlen)1);
		    sigerr_("SPICE(ARRAYTOOSMALL)", (ftnlen)20);
		    chkout_("DSKX02", (ftnlen)6);
		    return 0;
		}

/*              Fill in the corresponding elements of the parallel */
/*              "source" array with the current voxel loop index. */
/*              XDDA lists these voxels in the order the ray hits */
/*              them, so the lowest indexed voxels are hit first. */

		filli_(&vi, &nplate, &source[(i__2 = pntr - 1) < 256000 && 0 
			<= i__2 ? i__2 : s_rnge("source", i__2, "dskx02_", (
			ftnlen)1486)]);
	    }

/*           NPLATE returns zero or greater. */

	    pntr += nplate;
	}

/*        We've collected all plate info for the current voxel group. */

	totplt = pntr - 1;

/*        We want to sort the plate ID array and remove duplicates. */
/*        However, we want to keep the plates ordered according to the */
/*        sequence in which their containing voxels were hit by the ray. */
/*        So we find the order vector for the plate ID array, then use */
/*        this vector to mark duplicates. */

	orderi_(platid, &totplt, ordvec);

/*        Negate the plate ID of every duplicate we find, leaving */
/*        the instance in the voxel closest to the ray's origin */
/*        unmarked. For every pair of plates with the same ID, */
/*        we'll mark the one with the greater index in the plate */
/*        ID array. */

/*        We use MINDIX to identify the index, in the plate ID array, */
/*        of the current unmarked plate ID. MINIDX is re-used for */
/*        each set of distinct plate IDs. */

/*        The following loop considers plate IDs in increasing order. */

	minidx = ordvec[0];
	i__1 = totplt;
	for (i__ = 2; i__ <= i__1; ++i__) {

/*           The condition below uses absolute value because the plate */
/*           ID at index I-1 may have been "marked" via negation. */

	    if (platid[(i__3 = ordvec[(i__2 = i__ - 1) < 256000 && 0 <= i__2 ?
		     i__2 : s_rnge("ordvec", i__2, "dskx02_", (ftnlen)1530)] 
		    - 1) < 256000 && 0 <= i__3 ? i__3 : s_rnge("platid", i__3,
		     "dskx02_", (ftnlen)1530)] == (i__6 = platid[(i__5 = 
		    ordvec[(i__4 = i__ - 2) < 256000 && 0 <= i__4 ? i__4 : 
		    s_rnge("ordvec", i__4, "dskx02_", (ftnlen)1530)] - 1) < 
		    256000 && 0 <= i__5 ? i__5 : s_rnge("platid", i__5, "dsk"
		    "x02_", (ftnlen)1530)], abs(i__6))) {

/*              The plates having indices ORDVEC(I-1) and ORDVEC(I) are */
/*              duplicates. */

/*              At this point MINIDX is the lowest index in the plate ID */
/*              array of any plate seen so far having an ID equal to */
/*              PLATID(ORDVEC(I)). */

/*              If ORDVEC(I) is the new "minimum," negate the plate ID */
/*              at the old "minimum"; otherwise negate the plate ID at */
/*              index ORDVEC(I). */

		if (ordvec[(i__2 = i__ - 1) < 256000 && 0 <= i__2 ? i__2 : 
			s_rnge("ordvec", i__2, "dskx02_", (ftnlen)1544)] < 
			minidx) {

/*                 The plate that was previously at the minimum index is */
/*                 now considered a duplicate. The new minimum index for */
/*                 the current plate ID value is ORDVEC(I). */

		    platid[(i__2 = minidx - 1) < 256000 && 0 <= i__2 ? i__2 : 
			    s_rnge("platid", i__2, "dskx02_", (ftnlen)1550)] =
			     -platid[(i__3 = minidx - 1) < 256000 && 0 <= 
			    i__3 ? i__3 : s_rnge("platid", i__3, "dskx02_", (
			    ftnlen)1550)];
		    minidx = ordvec[(i__2 = i__ - 1) < 256000 && 0 <= i__2 ? 
			    i__2 : s_rnge("ordvec", i__2, "dskx02_", (ftnlen)
			    1551)];
		} else {

/*                 The current plate is a duplicate; mark it. */

		    platid[(i__3 = ordvec[(i__2 = i__ - 1) < 256000 && 0 <= 
			    i__2 ? i__2 : s_rnge("ordvec", i__2, "dskx02_", (
			    ftnlen)1557)] - 1) < 256000 && 0 <= i__3 ? i__3 : 
			    s_rnge("platid", i__3, "dskx02_", (ftnlen)1557)] =
			     -platid[(i__5 = ordvec[(i__4 = i__ - 1) < 256000 
			    && 0 <= i__4 ? i__4 : s_rnge("ordvec", i__4, 
			    "dskx02_", (ftnlen)1557)] - 1) < 256000 && 0 <= 
			    i__5 ? i__5 : s_rnge("platid", i__5, "dskx02_", (
			    ftnlen)1557)];
		}
	    } else {

/*              We're looking at a new plate ID. For the moment, this */
/*              ID has no duplicates. */

		minidx = ordvec[(i__2 = i__ - 1) < 256000 && 0 <= i__2 ? i__2 
			: s_rnge("ordvec", i__2, "dskx02_", (ftnlen)1566)];
	    }
	}

/*        If something went wrong up above there is no point in */
/*        going on from here. */

	if (failed_()) {
	    chkout_("DSKX02", (ftnlen)6);
	    return 0;
	}

/*        Now examine each plate in the PLATID list for this voxel group. */
/*        PNTR has the value of the index available for data in */
/*        PLATID, so the final location of data is at index PNTR - 1. */

	extra = FALSE_;
	final = 0;
	near__ = 0.;
	i__ = 1;
	while(i__ <= totplt && ! extra) {

/*           Retrieve the current plate ID. */

	    j = platid[(i__1 = i__ - 1) < 256000 && 0 <= i__1 ? i__1 : s_rnge(
		    "platid", i__1, "dskx02_", (ftnlen)1596)];
	    if (j > 0) {

/*              This is not a duplicate plate; consider it. */

		if (have) {

/*                 We already have a hit. See whether this plate resides */
/*                 in the voxel in which the last hit was found, or in a */
/*                 later voxel. */

		    if (source[(i__1 = i__ - 1) < 256000 && 0 <= i__1 ? i__1 :
			     s_rnge("source", i__1, "dskx02_", (ftnlen)1609)] 
			    > final) {

/*                    This is a "late plate": it occurs in a voxel later */
/*                    than that in which the first valid hit was found. */

			extra = TRUE_;
		    }
		}
		if (! extra) {

/*                 Fetch the vertex IDs of this plate. */

		    start = (j - 1) * 3 + 1;
		    dski02_(handle, dladsc, &c__9, &start, &c__3, &dim, vids);
		    if (failed_()) {
			chkout_("DSKX02", (ftnlen)6);
			return 0;
		    }

/*                 Fetch the vertices themselves. */

		    for (k = 1; k <= 3; ++k) {

/*                    Any vertex may be buffered already. Look in */
/*                    the vertex buffer before reading the vertex. */

			vloc = isrchi_(&vids[(i__1 = k - 1) < 3 && 0 <= i__1 ?
				 i__1 : s_rnge("vids", i__1, "dskx02_", (
				ftnlen)1643)], &nvbuf, vidxs);
			if (vloc > 0) {

/*                       The vertex was buffered; just copy it. */

			    vequ_(&vbuff[(i__1 = vloc * 3 - 3) < 600 && 0 <= 
				    i__1 ? i__1 : s_rnge("vbuff", i__1, "dsk"
				    "x02_", (ftnlen)1649)], &points[(i__2 = k *
				     3 - 3) < 9 && 0 <= i__2 ? i__2 : s_rnge(
				    "points", i__2, "dskx02_", (ftnlen)1649)])
				    ;
			} else {

/*                       Read in the vertex. */

			    start = (vids[(i__1 = k - 1) < 3 && 0 <= i__1 ? 
				    i__1 : s_rnge("vids", i__1, "dskx02_", (
				    ftnlen)1655)] - 1) * 3 + 1;
			    dskd02_(handle, dladsc, &c__19, &start, &c__3, &
				    dim, &points[(i__1 = k * 3 - 3) < 9 && 0 
				    <= i__1 ? i__1 : s_rnge("points", i__1, 
				    "dskx02_", (ftnlen)1657)]);
			    if (failed_()) {
				chkout_("DSKX02", (ftnlen)6);
				return 0;
			    }

/*                       If there's room, buffer this vertex. */

			    if (nvbuf < 200) {
				++nvbuf;
				vequ_(&points[(i__1 = k * 3 - 3) < 9 && 0 <= 
					i__1 ? i__1 : s_rnge("points", i__1, 
					"dskx02_", (ftnlen)1673)], &vbuff[(
					i__2 = nvbuf * 3 - 3) < 600 && 0 <= 
					i__2 ? i__2 : s_rnge("vbuff", i__2, 
					"dskx02_", (ftnlen)1673)]);
				vidxs[(i__1 = nvbuf - 1) < 200 && 0 <= i__1 ? 
					i__1 : s_rnge("vidxs", i__1, "dskx02_"
					, (ftnlen)1675)] = vids[(i__2 = k - 1)
					 < 3 && 0 <= i__2 ? i__2 : s_rnge(
					"vids", i__2, "dskx02_", (ftnlen)1675)
					];
			    }
			}
		    }
		}
		if (! extra) {

/*                 The current plate qualifies for testing using INSANG. */

/*                 Retrieve the model coordinates of the J'th plate's */
/*                 three vertices. Expand the plate slightly to prevent */
/*                 round-off error from causing the ray to miss the */
/*                 plate. Compute the edges of the tetrahedral angle */
/*                 with the observer as the apex and the vertices as */
/*                 members of the edge rays. Finally see if the ray */
/*                 lies inside the tetrahedron. */

		    vsubg_(points, obsmat, &c__9, edges);
		    pltexp_(edges, &xpdfrc, xpnts);
		    insang_(udir, xpnts, &xpnts[3], &xpnts[6], &hits, &scale);
		    if (hits) {

/*                    Reject intersections with plates that face away */
/*                    from the ray. Accept intersections with plates */
/*                    that face toward the ray. */

			pltnrm_(points, &points[3], &points[6], normal);
			hits = vdot_(udir, normal) <= 0.;
		    }
		    if (hits) {

/*                    The ray intersects this plate. */

			if (! have || scale < near__) {

/*                       Either this is the first intersection we've */
/*                       found, or this is the closest intersection to */
/*                       the vertex we've found. Compute the intercept */
/*                       coordinates and see whether the intercept is */
/*                       within the current voxel. Use a small tolerance */
/*                       for the comparison. */
/*                       If this intersection point is closer to the */
/*                       ray's vertex than the last one, pick this point */
/*                       and the plate it's on. */

/*                       Note that we don't yet know that this solution */
/*                       is valid. */

/*                          ___    ____   __________ */
/*                          XPT2 = VTX2 + SCALE*UDIR */

			    vlcom_(&c_b40, vtx2, &scale, udir, xpt2);

/*                       Compute the voxel grid coordinates of the */
/*                       intercept. HITCOR is a double precision vector */
/*                       having units of voxels (voxel edge length, to */
/*                       be precise). Note that the components of HITCOR */
/*                       are zero-based. */

			    zztogrid_(xpt2, voxori, &voxsiz, hitcor);

/*                       Look up the voxel grid coordinates (integer, */
/*                       1-based) of the current voxel. */

			    k = vxlout[(i__2 = source[(i__1 = i__ - 1) < 
				    256000 && 0 <= i__1 ? i__1 : s_rnge("sou"
				    "rce", i__1, "dskx02_", (ftnlen)1756)] - 1)
				     < 50000 && 0 <= i__2 ? i__2 : s_rnge(
				    "vxlout", i__2, "dskx02_", (ftnlen)1756)];
			    vxc1 = voxlst[(i__1 = k * 3 - 3) < 150000 && 0 <= 
				    i__1 ? i__1 : s_rnge("voxlst", i__1, 
				    "dskx02_", (ftnlen)1758)];
			    vxc2 = voxlst[(i__1 = k * 3 - 2) < 150000 && 0 <= 
				    i__1 ? i__1 : s_rnge("voxlst", i__1, 
				    "dskx02_", (ftnlen)1759)];
			    vxc3 = voxlst[(i__1 = k * 3 - 1) < 150000 && 0 <= 
				    i__1 ? i__1 : s_rnge("voxlst", i__1, 
				    "dskx02_", (ftnlen)1760)];
			    invox = hitcor[0] > vxc1 - xtol - 1 && hitcor[0] <
				     vxc1 + xtol && hitcor[1] > vxc2 - xtol - 
				    1 && hitcor[1] < vxc2 + xtol && hitcor[2] 
				    > vxc3 - xtol - 1 && hitcor[2] < vxc3 + 
				    xtol;
			    if (invox) {

/*                          Reject solutions that are outside of the */
/*                          segment's boundaries, where the boundaries */
/*                          are extended using the "greedy" margin. */

				dskgtl_(&c__2, &greedm);
				zzinvelt_(xpt2, &corsys, &dskdsc[6], &dskdsc[
					16], &greedm, &c__0, &inseg);
				if (inseg) {

/*                             We have a viable intercept. Record the */
/*                             scale, plate ID, and source voxel index */
/*                             in the compressed voxel list pointer */
/*                             array VXLOUT. We won't look for */
/*                             intercepts beyond the voxel designated by */
/*                             FINAL. */

				    vequ_(xpt2, xpt);
				    have = TRUE_;
				    near__ = scale;
				    *plid = j;
				    final = source[(i__1 = i__ - 1) < 256000 
					    && 0 <= i__1 ? i__1 : s_rnge(
					    "source", i__1, "dskx02_", (
					    ftnlen)1800)];

/*                             Indicate that a solution was found. We'll */
/*                             keep looking for a better one if PLID is */
/*                             not the last plate of the current voxel. */

				    *found = TRUE_;
				}
			    } else {

/*                          We must re-consider this plate if we */
/*                          encounter it in a voxel later in the voxel */
/*                          list. Remove all duplication markings for */
/*                          this plate. */

				w = abs(j);
				i__1 = totplt;
				for (k = 1; k <= i__1; ++k) {
				    if ((i__3 = platid[(i__2 = k - 1) < 
					    256000 && 0 <= i__2 ? i__2 : 
					    s_rnge("platid", i__2, "dskx02_", 
					    (ftnlen)1821)], abs(i__3)) == w) {
					platid[(i__2 = k - 1) < 256000 && 0 <=
						 i__2 ? i__2 : s_rnge("platid"
						, i__2, "dskx02_", (ftnlen)
						1822)] = w;
				    }
				}
			    }
			}

/*                    End of case of checking possible solution */
/*                    intercept. */

		    }

/*                 We're done with processing the current hit. */

		}

/*              We're done with processing the current qualifying plate. */

	    }

/*           We're done with the current plate. */

/*           Fetch the next plate for this voxel group. */

	    ++i__;
	}

/*        We're done with the current voxel group. */

	++group;
    }

/*     We've either found an intercept or have run out of voxel groups */
/*     to check. */

/*     That's all folks. */

    chkout_("DSKX02", (ftnlen)6);
    return 0;
} /* dskx02_ */

