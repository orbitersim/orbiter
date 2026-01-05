/* zzmkspin.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c_b6 = 32000000;
static integer c_b36 = 100000000;
static doublereal c_b39 = .33333333333333331;
static integer c_b53 = 100000;

/* $Procedure ZZMKSPIN ( Make spatial index of plates ) */
/* Subroutine */ int zzmkspin_(integer *np, integer *plates, doublereal *
	vrtces, doublereal *voxscl, integer *cgscal, integer *maxptr, integer 
	*mxcell, integer *maxvxl, integer *cells, integer *nvox, doublereal *
	voxsiz, doublereal *voxori, integer *nvxtot, integer *nvxptr, integer 
	*vxptr, integer *nvxlst, integer *vxlist, doublereal *extent, integer 
	*cgrptr)
{
    /* System generated locals */
    integer i__1, i__2, i__3, i__4;
    doublereal d__1, d__2, d__3;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer);
    double d_nint(doublereal *);
    integer i_dnnt(doublereal *);
    double pow_dd(doublereal *, doublereal *);

    /* Local variables */
    static integer cvid, npcg;
    static doublereal vmod[3], xmin, ymin, xmax;
    extern integer zzvox2id_(integer *, integer *);
    static doublereal ymax, zmax, zmin;
    extern /* Subroutine */ int zzaddlnk_(integer *, integer *, integer *, 
	    integer *, integer *, integer *, integer *), zzinilnk_(integer *, 
	    integer *, integer *, integer *, integer *);
    static integer i__, j;
    extern /* Subroutine */ int zzuntngl_(integer *, integer *, integer *, 
	    integer *, integer *, integer *, integer *), zzgetvox_(doublereal 
	    *, doublereal *, integer *, doublereal *, logical *, integer *);
    static integer q, r__, cgoff[3];
    extern /* Subroutine */ int zzvoxcvo_(integer *, integer *, integer *, 
	    integer *, integer *, integer *), chkin_(char *, ftnlen);
    static integer ncell;
    extern /* Subroutine */ int vpack_(doublereal *, doublereal *, doublereal 
	    *, doublereal *);
    extern doublereal dpmin_(void), dpmax_(void);
    static doublereal bxmin, bymin, bxmax, bymax, bzmax, bzmin;
    static integer gxmin, gxmax, gymax, gymin;
    static doublereal avext;
    static integer gzmax, gzmin;
    static logical inbox;
    extern /* Subroutine */ int errdp_(char *, doublereal *, ftnlen);
    static doublereal xvmin, yvmin, xvmax, yvmax, zvmax, zvmin;
    static integer cgof1d, cgxyz[3], ixptr, vixyz[3];
    extern logical failed_(void);
    extern /* Subroutine */ int cleari_(integer *, integer *);
    static integer ncgflg, ix, iy, iz, to, cgrdim[3], nx;
    static doublereal xp[3];
    extern doublereal brcktd_(doublereal *, doublereal *, doublereal *);
    static doublereal yp[3], zp[3];
    static integer ny, nz;
    static doublereal mdltol;
    static integer vcoord[3];
    extern /* Subroutine */ int setmsg_(char *, ftnlen);
    extern logical return_(void);
    static doublereal cvxsiz, xextnt[6];
    extern /* Subroutine */ int errint_(char *, integer *, ftnlen), sigerr_(
	    char *, ftnlen), chkout_(char *, ftnlen);

/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines. Users should not call this routine directly due */
/*     to the volatile nature of this routine. */

/*     Create voxel grid data structure and voxel-plate mapping. */

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

/*     plate voxel index */

/* $ Declarations */

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

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     NP         I   Number of plates. */
/*     PLATES     I   Array of plates. */
/*     VRTCES     I   Array of vertices. */
/*     VOXSCL     I   Voxel scaling factor. */
/*     CGSCAL     I   Coarse voxel grid scaling factor. */
/*     MAXPTR     I   Maximum voxel pointer list size. */
/*     MXCELL     I   Cell array column dimension. */
/*     MAXVXL     I   Maximum voxel-plate list size. */
/*     CELLS     I-O  Workspace array for vertex list construction. */
/*     NVOX       O   Dimensions of voxel grid. */
/*     VOXSIZ     O   Size of a voxel in model units. */
/*     VOXORI     O   Origin of voxel grid in model units. */
/*     NVXTOT     O   Total number of voxels in grid. */
/*     NVXPTR     O   Number of pointers in VXPTR array. */
/*     VXPTR      O   Pointer index to the voxel to plates list. */
/*     NVXLST     O   Length of voxel to plates list. */
/*     VXLIST     O   Voxel to plates list. */
/*     EXTENT     O   Vertex coordinate extents in km. */
/*     CGRPTR     O   Coarse voxel grid pointer array. */

/* $ Detailed_Input */

/*     NP         Total number of plates in model. */

/*     PLATES     Array containing indices in the array VRTCES of */
/*                the vertices corresponding to each plate. The */
/*                elements (1:3,I) of PLATES are the vertex indices */
/*                of the Ith plate. */

/*     VRTCES     Array containing cartesian coordinates for all */
/*                vertices in the model reference frame. Elements */
/*                (1:3,I) of VRTCES are the coordinates of the Ith */
/*                vertex. */

/*     VOXSCL     The voxel size is determined by the average extent of */
/*                each plate in XYZ. VOXSCL is a scaling factor that */
/*                may be used to adjust final voxel size. */

/*     CGSCAL     The coarse voxel grid has voxel edge length */
/*                larger by factor CGSCAL than the fine voxel */
/*                edge length. */

/*     MAXPTR     is the maximum number of elements in the output */
/*                list of voxel pointers. */

/*     MXCELL     is the number of cells in the input cell array. */
/*                This is the second dimension of the array. */

/*     MAXVXL     is the maximum number of elements in the output */
/*                voxel-plate list. */

/*     CELLS      workspace array used to construct the voxel-plate */
/*                mapping. */

/* $ Detailed_Output */

/*     CELLS      workspace array used to construct the voxel-plate */
/*                mapping. */

/*     NVOX       Dimensions of the voxel grid in voxel units. */

/*     VOXSIZ     Size of each voxel in model units (km). */

/*     VOXORI     Origin of voxel grid in model units (km). */

/*     NVXTOT     Total number of voxels in grid. */

/*     NVXPTR     Number of pointers in VXPTR array. */

/*     VXPTR      Array of pointers that map voxels to their associated */
/*                plated IDs in the voxel to plates list. The Nth */
/*                element of this array contains a pointer to the Mth */
/*                element of VXLIST, or -1. If VXPTR(N) is -1, the Nth */
/*                voxel is empty. */

/*     NVXLST     The total number of elements in the plate list. */

/*     VXLIST     The plates list. For the Nth voxel, VXPTR(N) points to */
/*                VXLIST(M). VXLIST(M) contains the number plates that */
/*                intersect voxel N. The IDs of those plates are stored */
/*                in the following elements. */

/*     EXTENT     is an array of extents of vertex coordinates. Elements */
/*                indexed (2*I)-1 and 2*I are the minimum and maximum */
/*                values of the Ith coordinate, taken over all vertices. */
/*                Units are km. */

/*     CGRPTR     is an of array coarse voxel grid pointers. Null */
/*                pointers have the value -1. Non-null pointers indicate */
/*                locations in the voxel pointer array: each non-empty */
/*                coarse voxel points to a list of pointers for each of */
/*                the fine voxels it contains. */

/* $ Parameters */

/*     See the include file dsk02.inc. */

/* $ Exceptions */

/*     1)  If NP is less than 1 or greater than MAXPLT, the error */
/*         SPICE(VALUEOUTOFRANGE) is signaled. */

/*     2)  If the number of coarse voxels exceeds the grid size MAXCGR, */
/*         the error SPICE(COARSEGRIDOVERFLOW) is signaled. */

/*     3)  If the voxel count is greater than MAXVOX, the error */
/*         SPICE(VALUEOUTOFRANGE) is signaled. */

/*     4)  If the coarse voxel count is less than 1 or greater than */
/*         MAXCGR, the error SPICE(VALUEOUTOFRANGE) is signaled. */

/*     5)  If the coarse voxel scale is less than 1 or more than */
/*         the cube root of the fine voxel count, the error */
/*         SPICE(VALUEOUTOFRANGE) will be signaled. */

/*     6)  If the cube of the coarse voxel scale does not divide the */
/*         fine voxel count evenly, the error SPICE(INCOMPATIBLESCALE) */
/*         will be signaled. */

/*     7)  If the workspace overflows while this routine accumulates */
/*         voxel-plate associations, the error will be signaled by */
/*         a routine in the call tree of this routine. */

/*     8)  If the voxel-plate association list array overflows while */
/*         this routine accumulates voxel-plate associations, the error */
/*         will be signaled by a routine in the call tree of this */
/*         routine. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine supports creation of spatial indexes for DSK type 2 */
/*     segments. This routine determines the fine and coarse voxel */
/*     grid dimensions and parameters. It also builds the voxel-plate */
/*     association data structures. */

/* $ Examples */

/*     See usage in DSKMI2. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman  (JPL) */
/*     J.A. Bytof    (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 17-FEB-2017 (NJB) */

/*        Added new error checks. */

/*        29-JAN-2016 (NJB) */

/*           Removed reference to ZZVOXPAD. Renamed argument MAXCEL to */
/*           MXCELL to accommodate declaration of MAXCEL in dsk02.inc. */

/*           Updated long error messages and header comments. */

/*        26-MAR-2015 (NJB) */

/*           Functional change: now fills in the coarse voxel grid */
/*           pointer array with valid pointers; this is done in one */
/*           pass. The old version of the routine only marked the */
/*           elements of this array as null or non-null. */

/*           The algorithm for computing the set of voxels containing */
/*           the bounding box of each plate was re-written to improve */
/*           efficiency. */

/*        30-JUN-2014 (NJB) */

/*           Argument list change: removed work space array PNTRS. The */
/*           pointer array for the voxel-to-plate list mapping is now */
/*           constructed in place. */

/*           Changed argument list to include sizes of arrays. Changed */
/*           error handling to make use of array sizes. Changed call to */
/*           UNTNGL to accommodate argument list change in that routine. */
/*           Updated header I/O descriptions. */

/*        13-MAY-2010 (NJB) */

/*           Now accepts input workspace arrays PNTRS and CELLS. */
/*           Replaced argument VERT with arguments VRTCES and PLATES; */
/*           this was done to greatly reduce the memory needed by this */
/*           program. */

/*           Changed INCLUDE file to dsk02.inc. */

/*           Bug fix: determination of empty voxels is now based on */
/*           voxel pointers, not their target values. */


/*        08-OCT-2009 (NJB) */

/*           Re-ordered header sections. */

/*        12-SEP-2004 (EDW) */

/*           Improve/expand comments and descriptions. */

/*        03-FEB-1999 (JAB) */

/*           Original version. */

/* -& */
/* $ Index_Entries */

/*     spatial index plates voxels */

/* -& */
/* $ Revisions */

/*        20-MAR-2015 (NJB) */

/*        Functional change: now fills in the coarse voxel grid pointer */
/*        array with valid pointers; this is done in one pass. The old */
/*        version of the routine only marked the elements of this array */
/*        as null or non-null. */

/*        The maximum size of the voxel-plate pointer array has been */
/*        reduced to MAXVOX/2. */

/*        The array mapping voxels to the voxel-plate pointer array is */
/*        no longer used. Previously, voxel IDs were used as the */
/*        "A list" elements with which plate lists were associated. */
/*        Now, indices in the voxel-plate pointer array are used as */
/*        "A" values. */

/* -& */

/*     SPICELIB functions */


/*     Other functions */


/*     Local parameters */


/*     Fraction of voxel edge length used a tolerance for plate */
/*     inclusion in voxels: */


/*     Local Variables */


/*     Saved variables */


/*     Required for f2c use on Linux, all local variables */
/*     to static. */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }
    chkin_("ZZMKSPIN", (ftnlen)8);

/*     Check NP. */

    if (*np < 1 || *np > 32000000) {
	setmsg_("Plate count NP = #; count must be in the range 1:#.", (
		ftnlen)51);
	errint_("#", np, (ftnlen)1);
	errint_("#", &c_b6, (ftnlen)1);
	sigerr_("SPICE(VALUEOUTOFRANGE)", (ftnlen)22);
	chkout_("ZZMKSPIN", (ftnlen)8);
	return 0;
    }

/*     Make sure the coarse voxel scale is positive. We'll */
/*     perform additional checks later on. Those checks */
/*     require computations that can't be done if the coarse */
/*     scale is zero. */

    if (*cgscal < 1) {
	setmsg_("Coarse voxel scale = #; scale must be positive.", (ftnlen)47)
		;
	errint_("#", cgscal, (ftnlen)1);
	sigerr_("SPICE(VALUEOUTOFRANGE)", (ftnlen)22);
	chkout_("ZZMKSPIN", (ftnlen)8);
	return 0;
    }

/*     Get the average extents of all plates and the */
/*     overall model extent. The extents have model units; in */
/*     other words km. */

    avext = 0.;
    xmin = dpmax_();
    xmax = dpmin_();
    ymin = dpmax_();
    ymax = dpmin_();
    zmin = dpmax_();
    zmax = dpmin_();
    i__1 = *np;
    for (i__ = 1; i__ <= i__1; ++i__) {
	bxmin = dpmax_();
	bxmax = dpmin_();
	bymin = dpmax_();
	bymax = dpmin_();
	bzmin = dpmax_();
	bzmax = dpmin_();
	xp[0] = vrtces[plates[i__ * 3 - 3] * 3 - 3];
	xp[1] = vrtces[plates[i__ * 3 - 2] * 3 - 3];
	xp[2] = vrtces[plates[i__ * 3 - 1] * 3 - 3];
	yp[0] = vrtces[plates[i__ * 3 - 3] * 3 - 2];
	yp[1] = vrtces[plates[i__ * 3 - 2] * 3 - 2];
	yp[2] = vrtces[plates[i__ * 3 - 1] * 3 - 2];
	zp[0] = vrtces[plates[i__ * 3 - 3] * 3 - 1];
	zp[1] = vrtces[plates[i__ * 3 - 2] * 3 - 1];
	zp[2] = vrtces[plates[i__ * 3 - 1] * 3 - 1];
	for (j = 1; j <= 3; ++j) {

/*           Determine plate extents. */

/* Computing MIN */
	    d__1 = bxmin, d__2 = xp[(i__2 = j - 1) < 3 && 0 <= i__2 ? i__2 : 
		    s_rnge("xp", i__2, "zzmkspin_", (ftnlen)498)];
	    bxmin = min(d__1,d__2);
/* Computing MAX */
	    d__1 = bxmax, d__2 = xp[(i__2 = j - 1) < 3 && 0 <= i__2 ? i__2 : 
		    s_rnge("xp", i__2, "zzmkspin_", (ftnlen)499)];
	    bxmax = max(d__1,d__2);
/* Computing MIN */
	    d__1 = bymin, d__2 = yp[(i__2 = j - 1) < 3 && 0 <= i__2 ? i__2 : 
		    s_rnge("yp", i__2, "zzmkspin_", (ftnlen)501)];
	    bymin = min(d__1,d__2);
/* Computing MAX */
	    d__1 = bymax, d__2 = yp[(i__2 = j - 1) < 3 && 0 <= i__2 ? i__2 : 
		    s_rnge("yp", i__2, "zzmkspin_", (ftnlen)502)];
	    bymax = max(d__1,d__2);
/* Computing MIN */
	    d__1 = bzmin, d__2 = zp[(i__2 = j - 1) < 3 && 0 <= i__2 ? i__2 : 
		    s_rnge("zp", i__2, "zzmkspin_", (ftnlen)504)];
	    bzmin = min(d__1,d__2);
/* Computing MAX */
	    d__1 = bzmax, d__2 = zp[(i__2 = j - 1) < 3 && 0 <= i__2 ? i__2 : 
		    s_rnge("zp", i__2, "zzmkspin_", (ftnlen)505)];
	    bzmax = max(d__1,d__2);

/*           Determine model extent. */

	    xmin = min(xmin,bxmin);
	    xmax = max(xmax,bxmax);
	    ymin = min(ymin,bymin);
	    ymax = max(ymax,bymax);
	    zmin = min(zmin,bzmin);
	    zmax = max(zmax,bzmax);
	}
	extent[0] = xmin;
	extent[1] = xmax;
	extent[2] = ymin;
	extent[3] = ymax;
	extent[4] = zmin;
	extent[5] = zmax;

/*        Calculate the cumulative extent of all plates for */
/*        each degree of freedom. */

	avext = avext + (d__1 = bxmax - bxmin, abs(d__1)) + (d__2 = bymax - 
		bymin, abs(d__2)) + (d__3 = bzmax - bzmin, abs(d__3));
    }

/*     Calculate the average extent of all plates for */
/*     and the voxel size, i.e the length of one side */
/*     of a voxel cube. */

    avext /= (doublereal) (*np * 3);
    *voxsiz = *voxscl * avext;
    mdltol = *voxsiz * .001;

/*     Produce a set of vertex extents, extended by MDLTOL, */
/*     to be used later. */

    for (i__ = 1; i__ <= 5; i__ += 2) {
	xextnt[(i__1 = i__ - 1) < 6 && 0 <= i__1 ? i__1 : s_rnge("xextnt", 
		i__1, "zzmkspin_", (ftnlen)553)] = extent[(i__2 = i__ - 1) < 
		6 && 0 <= i__2 ? i__2 : s_rnge("extent", i__2, "zzmkspin_", (
		ftnlen)553)] - mdltol;
	xextnt[(i__1 = i__) < 6 && 0 <= i__1 ? i__1 : s_rnge("xextnt", i__1, 
		"zzmkspin_", (ftnlen)554)] = extent[(i__2 = i__) < 6 && 0 <= 
		i__2 ? i__2 : s_rnge("extent", i__2, "zzmkspin_", (ftnlen)554)
		] + mdltol;
    }

/*     Determine the size of the coarse voxels. */

    cvxsiz = *voxsiz * *cgscal;

/*     Determine the minima and maxima of the body centered */
/*     vertex coordinates expressed in coarse voxel units. Scale the */
/*     vertices coord values by CVXSIZ: this scales the */
/*     axis in the voxel model space producing cubic voxels */
/*     with length 1 along each edge in voxel space, */
/*     CVXSIZ along an edge in model space. */

    xvmin = xmin / cvxsiz;
    yvmin = ymin / cvxsiz;
    zvmin = zmin / cvxsiz;
    xvmax = xmax / cvxsiz;
    yvmax = ymax / cvxsiz;
    zvmax = zmax / cvxsiz;

/*     Extend the coarse voxel grid by at least 1/2 */
/*     coarse voxel length along each degree of freedom. */

    d__1 = xvmin - 1.;
    xvmin = d_nint(&d__1);
    d__1 = yvmin - 1.;
    yvmin = d_nint(&d__1);
    d__1 = zvmin - 1.;
    zvmin = d_nint(&d__1);
    d__1 = xvmax + 1.;
    xvmax = d_nint(&d__1);
    d__1 = yvmax + 1.;
    yvmax = d_nint(&d__1);
    d__1 = zvmax + 1.;
    zvmax = d_nint(&d__1);

/*     Calculate the coarse voxel grid origin in model units. */

    voxori[0] = xvmin * cvxsiz;
    voxori[1] = yvmin * cvxsiz;
    voxori[2] = zvmin * cvxsiz;

/*     Calculate the dimension of the voxel grid in */
/*     units of (regular) voxels. */

    d__1 = xvmax - xvmin;
    nx = i_dnnt(&d__1) * *cgscal;
    d__1 = yvmax - yvmin;
    ny = i_dnnt(&d__1) * *cgscal;
    d__1 = zvmax - zvmin;
    nz = i_dnnt(&d__1) * *cgscal;
    nvox[0] = nx;
    nvox[1] = ny;
    nvox[2] = nz;
    *nvxtot = nx * ny * nz;

/*     Make sure the number of voxels NVXTOT is within range. */

    if (*nvxtot > 100000000) {
	setmsg_("Fine voxel count NVXTOT = #; count must be in the range 1:#."
		, (ftnlen)60);
	errint_("#", nvxtot, (ftnlen)1);
	errint_("#", &c_b36, (ftnlen)1);
	sigerr_("SPICE(VALUEOUTOFRANGE)", (ftnlen)22);
	chkout_("ZZMKSPIN", (ftnlen)8);
	return 0;
    }

/*     Check the coarse voxel scale. It must be at least 1, and its */
/*     cube must not exceed the fine voxel count. */

    d__1 = (doublereal) (*nvxtot);
    if (*cgscal < 1 || (doublereal) (*cgscal) > pow_dd(&d__1, &c_b39)) {
	setmsg_("Coarse voxel scale = #; scale must be in the range 1:NVXTOT"
		"**3, where NVXTOT is the total fine voxel count. In this cas"
		"e, NVXTOT = #.", (ftnlen)133);
	errint_("#", cgscal, (ftnlen)1);
	errint_("#", nvxtot, (ftnlen)1);
	sigerr_("SPICE(VALUEOUTOFRANGE)", (ftnlen)22);
	chkout_("ZZMKSPIN", (ftnlen)8);
	return 0;
    }

/*     The cube of the coarse scale must divide the total voxel count */
/*     evenly. This is a consistency check: the code that derives the */
/*     voxel grid dimensions should ensure this condition is met. */

/* Computing 3rd power */
    i__1 = *cgscal;
    q = *nvxtot / (i__1 * (i__1 * i__1));
/* Computing 3rd power */
    i__1 = *cgscal;
    r__ = *nvxtot - q * (i__1 * (i__1 * i__1));
    if (r__ != 0) {
	setmsg_("Coarse voxel scale = #; the cube of the scale must divide N"
		"VXTOT evenly, where NVXTOT is the total  fine voxel count. I"
		"n this case, NVXTOT = #.", (ftnlen)143);
	errint_("#", cgscal, (ftnlen)1);
	errint_("#", nvxtot, (ftnlen)1);
	sigerr_("SPICE(INCOMPATIBLESCALE)", (ftnlen)24);
	chkout_("ZZMKSPIN", (ftnlen)8);
	return 0;
    }

/*     Check the number of coarse voxels. */

/* Computing 3rd power */
    i__1 = *cgscal;
    npcg = i__1 * (i__1 * i__1);
    ncgflg = *nvxtot / npcg;
    if (ncgflg > 100000) {
	setmsg_("Number of coarse voxels # exceeds limit #. Increase coarse "
		"voxel scale, fine voxel scale, or both.", (ftnlen)98);
	errint_("#", &ncgflg, (ftnlen)1);
	errint_("#", &c_b53, (ftnlen)1);
	sigerr_("SPICE(COARSEGRIDOVERFLOW)", (ftnlen)25);
	chkout_("ZZMKSPIN", (ftnlen)8);
	return 0;
    }

/*     Enumerate all voxels that each plate might intersect. */

    zzinilnk_(maxptr, mxcell, &ncell, vxptr, cells);

/*     Set the dimensions of the coarse grid. */

    cgrdim[0] = nx / *cgscal;
    cgrdim[1] = ny / *cgscal;
    cgrdim[2] = nz / *cgscal;
    cleari_(&ncgflg, cgrptr);

/*     TO points to the first free location in the VXPTR array. */

    to = 1;
    i__1 = *np;
    for (i__ = 1; i__ <= i__1; ++i__) {

/*        Find the extents of the Ith plate, where the extents */
/*        are expanded by TOL in each direction. We truncate */
/*        the expanded box at a distance of MDLTOL beyond the */
/*        extents of the vertex set, if necessary. */

	xp[0] = vrtces[plates[i__ * 3 - 3] * 3 - 3];
	xp[1] = vrtces[plates[i__ * 3 - 2] * 3 - 3];
	xp[2] = vrtces[plates[i__ * 3 - 1] * 3 - 3];
	yp[0] = vrtces[plates[i__ * 3 - 3] * 3 - 2];
	yp[1] = vrtces[plates[i__ * 3 - 2] * 3 - 2];
	yp[2] = vrtces[plates[i__ * 3 - 1] * 3 - 2];
	zp[0] = vrtces[plates[i__ * 3 - 3] * 3 - 1];
	zp[1] = vrtces[plates[i__ * 3 - 2] * 3 - 1];
	zp[2] = vrtces[plates[i__ * 3 - 1] * 3 - 1];
/* Computing MIN */
	d__2 = min(xp[0],xp[1]);
	d__1 = min(d__2,xp[2]) - mdltol;
	bxmin = brcktd_(&d__1, xextnt, &xextnt[1]);
/* Computing MAX */
	d__2 = max(xp[0],xp[1]);
	d__1 = max(d__2,xp[2]) + mdltol;
	bxmax = brcktd_(&d__1, xextnt, &xextnt[1]);
/* Computing MIN */
	d__2 = min(yp[0],yp[1]);
	d__1 = min(d__2,yp[2]) - mdltol;
	bymin = brcktd_(&d__1, &xextnt[2], &xextnt[3]);
/* Computing MAX */
	d__2 = max(yp[0],yp[1]);
	d__1 = max(d__2,yp[2]) + mdltol;
	bymax = brcktd_(&d__1, &xextnt[2], &xextnt[3]);
/* Computing MIN */
	d__2 = min(zp[0],zp[1]);
	d__1 = min(d__2,zp[2]) - mdltol;
	bzmin = brcktd_(&d__1, &xextnt[4], &xextnt[5]);
/* Computing MAX */
	d__2 = max(zp[0],zp[1]);
	d__1 = max(d__2,zp[2]) + mdltol;
	bzmax = brcktd_(&d__1, &xextnt[4], &xextnt[5]);

/*        Find the range of voxel coordinates that contain the bounding */
/*        box of the plate. All we need look at are the coordinates */
/*        of the two corners having minimum and maximum coordinates. */

/*        Start with the corner having minimum coordinates: */

	vpack_(&bxmin, &bymin, &bzmin, vmod);
	zzgetvox_(voxsiz, voxori, nvox, vmod, &inbox, vcoord);
	if (! inbox) {

/*           A corner of the bounding box lies outside the voxel grid. */
/*           This should never occur. */

	    setmsg_("BUG: bounding box of plate is outside of voxel grid. In"
		    "put coordinates were (#, #, #). Plate ID = #.", (ftnlen)
		    100);
	    errdp_("#", vmod, (ftnlen)1);
	    errdp_("#", &vmod[1], (ftnlen)1);
	    errdp_("#", &vmod[2], (ftnlen)1);
	    errint_("#", &i__, (ftnlen)1);
	    sigerr_("SPICE(BUG)", (ftnlen)10);
	    chkout_("ZZMKSPIN", (ftnlen)8);
	    return 0;
	}

/*        Unpack minimum voxel coordinates from VCOORD. */

	gxmin = vcoord[0];
	gymin = vcoord[1];
	gzmin = vcoord[2];

/*        Now handle the corner having maximum coordinates: */

	vpack_(&bxmax, &bymax, &bzmax, vmod);
	zzgetvox_(voxsiz, voxori, nvox, vmod, &inbox, vcoord);
	if (! inbox) {

/*           A corner of the bounding box lies outside the voxel grid. */
/*           This should never occur. */

	    setmsg_("BUG: bounding box of plate is outside of voxel grid. In"
		    "put coordinates were (#, #, #). Plate ID = #.", (ftnlen)
		    100);
	    errdp_("#", vmod, (ftnlen)1);
	    errdp_("#", &vmod[1], (ftnlen)1);
	    errdp_("#", &vmod[2], (ftnlen)1);
	    errint_("#", &i__, (ftnlen)1);
	    sigerr_("SPICE(BUG)", (ftnlen)10);
	    chkout_("ZZMKSPIN", (ftnlen)8);
	    return 0;
	}

/*        Unpack maximum voxel coordinates from VCOORD. */

	gxmax = vcoord[0];
	gymax = vcoord[1];
	gzmax = vcoord[2];

/*        Determine voxels that the bounding box of the plate */
/*        intersects. */

	i__2 = gzmax;
	for (iz = gzmin; iz <= i__2; ++iz) {
	    i__3 = gymax;
	    for (iy = gymin; iy <= i__3; ++iy) {
		i__4 = gxmax;
		for (ix = gxmin; ix <= i__4; ++ix) {
		    vixyz[0] = ix;
		    vixyz[1] = iy;
		    vixyz[2] = iz;

/*                   Find the coarse voxel containing this voxel, and */
/*                   compute the offset of this voxel within the coarse */
/*                   voxel. The output CGXYZ contains the 3-dimensional */
/*                   coordinates of the coarse voxel within the coarse */
/*                   grid. The output CGOF1D is the 1-based, */
/*                   1-dimensional offset of the current voxel (having */
/*                   coordinates VIXYZ) from the start of the coarse */
/*                   voxel. */

		    zzvoxcvo_(vixyz, nvox, cgscal, cgxyz, cgoff, &cgof1d);
		    if (failed_()) {
			chkout_("ZZMKSPIN", (ftnlen)8);
			return 0;
		    }
		    cvid = zzvox2id_(cgxyz, cgrdim);
		    if (cgrptr[cvid - 1] == 0) {

/*                      The coarse voxel at index CVID is empty so far. */
/*                      Allocate CGSCAL pointers for it in the VXPTR */
/*                      array; make the coarse voxel point to the first */
/*                      element of this sub-array. */

			cgrptr[cvid - 1] = to;
			to += npcg;
		    }

/*                   Let IXPTR be the index in the VXPTR array of the */
/*                   pointer for the current voxel. */

		    ixptr = cgrptr[cvid - 1] - 1 + cgof1d;
		    zzaddlnk_(&ixptr, &i__, maxptr, mxcell, vxptr, &ncell, 
			    cells);
		    if (failed_()) {
			chkout_("ZZMKSPIN", (ftnlen)8);
			return 0;
		    }
		}
	    }
	}
    }
    *nvxptr = to - 1;

/*     Generate two linked lists mapping voxel ID to the plates enclosed */
/*     within that voxel (if any). */

/*     VXPTR : An array, indexed by voxel ID. For an array element, */
/*             VXPTR(VOX_ID), greater than zero, the value identifies an */
/*             index in VXLIST, the value of that VXLIST array element */
/*             equaling the number of plates contained in the voxel */
/*             specified by the ID. The condition VXPTR(VOX_ID) = -1 */
/*             indicates the voxel contains no plates. */

/*     VXLIST: An array, indexed by the positive entries in VXPTR. The */
/*             element, N, identified by a VXPTR value describes the */
/*             number of plates contained in the corresponding voxel. */

/*                 N = VXLIST( VXPTR(VOX_ID) ) */

/*             The N elements following VXLIST( VXPTR(VOX_ID) ), */
/*             contain the IDs of those plates within the voxel. */

    zzuntngl_(nvxptr, mxcell, cells, maxvxl, vxptr, nvxlst, vxlist);
    chkout_("ZZMKSPIN", (ftnlen)8);
    return 0;
} /* zzmkspin_ */

