/* dskw02.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c_b116 = 16000002;
static integer c_b122 = 32000000;
static integer c__6 = 6;
static integer c__3 = 3;
static integer c_b145 = 100000000;
static doublereal c_b154 = .33333333333333331;
static integer c_b168 = 100000;
static integer c__24 = 24;
static integer c__10 = 10;
static integer c__1 = 1;

/* $Procedure DSKW02 ( DSK, write type 2 segment ) */
/* Subroutine */ int dskw02_(integer *handle, integer *center, integer *
	surfid, integer *dclass, char *frame, integer *corsys, doublereal *
	corpar, doublereal *mncor1, doublereal *mxcor1, doublereal *mncor2, 
	doublereal *mxcor2, doublereal *mncor3, doublereal *mxcor3, 
	doublereal *first, doublereal *last, integer *nv, doublereal *vrtces, 
	integer *np, integer *plates, doublereal *spaixd, integer *spaixi, 
	ftnlen frame_len)
{
    /* System generated locals */
    integer plates_dim2, i__1, i__2;
    doublereal d__1, d__2, d__3, d__4;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer);
    double pow_dd(doublereal *, doublereal *);

    /* Local variables */
    integer ncgr;
    extern /* Subroutine */ int vequ_(doublereal *, doublereal *);
    doublereal a, b;
    integer i__, j, k, q;
    doublereal r__;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    doublereal descr[24];
    extern /* Subroutine */ int errch_(char *, char *, ftnlen, ftnlen), 
	    errdp_(char *, doublereal *, ftnlen), moved_(doublereal *, 
	    integer *, doublereal *), movei_(integer *, integer *, integer *);
    extern doublereal twopi_(void);
    extern /* Subroutine */ int dasadd_(integer *, integer *, doublereal *);
    extern logical failed_(void);
    extern /* Subroutine */ int dasadi_(integer *, integer *, integer *), 
	    cleard_(integer *, doublereal *);
    integer frmcde;
    extern doublereal halfpi_(void);
    extern /* Subroutine */ int dlabns_(integer *);
    doublereal segbds[4]	/* was [2][2] */;
    extern /* Subroutine */ int dlaens_(integer *);
    extern logical return_(void);
    doublereal altlim, voxori[3], voxsiz, vtxbds[6]	/* was [2][3] */;
    integer cgrscl, nvxtot;
    extern doublereal dpr_(void);
    integer pvoxpl, pvoxpt, pvtxpl, pvtxpt, vgrext[3], voxnpt, voxnpl, vtxnpl;
    extern /* Subroutine */ int namfrm_(char *, integer *, ftnlen), setmsg_(
	    char *, ftnlen), sigerr_(char *, ftnlen), chkout_(char *, ftnlen),
	     errint_(char *, integer *, ftnlen);

/* $ Abstract */

/*     Write a type 2 segment to a DSK file. */

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

/*     DAS */
/*     DSK */

/* $ Keywords */

/*     DAS */
/*     DSK */
/*     FILES */
/*     PLATE */
/*     TOPOGRAPHY */

/* $ Declarations */

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
/*     HANDLE     I   Handle assigned to the opened DSK file. */
/*     CENTER     I   Central body ID code. */
/*     SURFID     I   Surface ID code. */
/*     DCLASS     I   Data class. */
/*     FRAME      I   Reference frame. */
/*     CORSYS     I   Coordinate system code. */
/*     CORPAR     I   Coordinate system parameters. */
/*     MNCOR1     I   Minimum value of first coordinate. */
/*     MXCOR1     I   Maximum value of first coordinate. */
/*     MNCOR2     I   Minimum value of second coordinate. */
/*     MXCOR2     I   Maximum value of second coordinate. */
/*     MNCOR3     I   Minimum value of third coordinate. */
/*     MXCOR3     I   Maximum value of third coordinate. */
/*     FIRST      I   Coverage start time. */
/*     LAST       I   Coverage stop time. */
/*     NV         I   Number of vertices. */
/*     VRTCES     I   Vertices. */
/*     NP         I   Number of plates. */
/*     PLATES     I   Plates. */
/*     SPAIXD     I   Double precision component of spatial index. */
/*     SPAIXI     I   Integer component of spatial index. */
/*     ANGMRG     P   Angular round-off margin. */
/*     GENCLS     P   General surface DSK class. */
/*     SVFCLS     P   Single-valued function DSK class. */
/*     NSYPAR     P   Maximum number of coordinate system parameters in */
/*                    a DSK descriptor. */
/*     MAXCGR     P   Maximum DSK type 2 coarse voxel count. */
/*     MAXPLT     P   Maximum DSK type 2 plate count. */
/*     MAXVOX     P   Maximum DSK type 2 voxel count. */
/*     MAXVRT     P   Maximum DSK type 2 vertex count. */

/* $ Detailed_Input */

/*     HANDLE   is the DAS file handle associated with a DSK file. */
/*              The file must be open for write access. */

/*     CENTER   is the ID code of the body whose surface is described */
/*              by the input plate model. CENTER refers to an */
/*              ephemeris object. */

/*     SURFID   is the ID code of the surface described by the input */
/*              plate model. Multiple surfaces (for example, surfaces */
/*              having different resolutions) may be associated with */
/*              a given body. */

/*     DCLASS   is the data class of the input data set. See the */
/*              INCLUDE file dskdsc.inc for values and meanings. */

/*     FRAME    is the name of the reference frame with respect to */
/*              which the input data are expressed. */

/*     CORSYS   is the coordinate system in which the spatial coverage */
/*              of the input data is expressed. CORSYS is an integer */
/*              code. The supported values of CORPAR are */

/*                 Parameter name      Coordinate system */
/*                 --------------      ----------------- */
/*                 LATSYS              Planetocentric latitudinal */
/*                 RECSYS              Rectangular (Cartesian) */
/*                 PDTSYS              Planetodetic */

/*              See the INCLUDE file dskdsc.inc for parameter */
/*              declarations. */


/*     CORPAR   is an array of parameters associated with the input */
/*              coordinate system. */

/*              For latitudinal and rectangular coordinates, CORPAR */
/*              is ignored. */

/*              For planetodetic coordinates, the contents of CORPAR */
/*              are: */

/*                 Element         Contents */
/*                 ---------       ----------------------------------- */
/*                 CORPAR(1)       Equatorial radius of reference */
/*                                 spheroid. */

/*                 CORPAR(2)       Flattening coefficient. The polar */
/*                                 radius of the reference spheroid */
/*                                 is given by */

/*                                    CORPAR(1) * ( 1 - CORPAR(2) ) */

/*                 CORPAR(3)... */
/*                 CORPAR(NSYPAR)  Unused. */


/*     MNCOR1, */
/*     MXCOR1, */
/*     MNCOR2, */
/*     MXCOR2, */
/*     MNCOR3, */
/*     MXCOR3   are, respectively, lower and upper bounds of */
/*              each of the coordinates of the input data, where the */
/*              coordinate system is defined by CORSYS and CORPAR. */
/*              These bounds define the region for which the segment */
/*              provides data. */

/*              Distance units are km. Angular units are radians. */

/*              The interpretation of these bounds depends on the data */
/*              class; see DCLASS above. */

/*                 Single-valued surfaces */
/*                 ---------------------- */

/*                 If the segment has data class SVFCLS (see */
/*                 dskdsc.inc), the segment defines a surface as a */
/*                 single-valued function of its domain coordinates: */
/*                 for example, it may define the radius of the */
/*                 surface as a function of planetocentric longitude */
/*                 and latitude, or Z as a function of X and Y. */

/*                 In this case, the input data must cover a */
/*                 rectangle in dimensions 1 and 2 of the input */
/*                 coordinate system: the set of points */

/*                    R = { (x,y): MNCOR1 <= x <= MXCOR1; */
/*                                 MNCOR2 <= y <= MXCOR2  } */

/*                 must be completely covered by the input data. In */
/*                 other words, for each point (x,y) of R, there must */
/*                 be some plate containing a point whose first two */
/*                 coordinates are (x,y). */

/*                 The plate set may extend beyond the coordinate */
/*                 range defined by the bounds on the domain. */

/*                 Normally for single-valued surfaces, MNCOR3 and */
/*                 MXCOR3 are the minimum and maximum values of the */
/*                 function attained on the domain. */


/*                 General surfaces */
/*                 ---------------- */

/*                 If the segment has data class GENCLS (see */
/*                 dskdsc.inc), the segment simply contains a */
/*                 collection of plates: no guarantees are made about */
/*                 the topology of the surface. The coordinate bounds */
/*                 simply indicate the spatial region for which the */
/*                 segment provides data. */

/*                 Note that shapes of small bodies such as asteroids */
/*                 and comet nuclei may fall into the "general */
/*                 surface" category. Surface features such as cliffs, */
/*                 caves, and arches can prevent a surface from being */
/*                 represented as a single-valued function of latitude */
/*                 and longitude, for example. */


/*              Longitude interpretation and restrictions */
/*              ----------------------------------------- */

/*              The following rules apply to longitudes provided in */
/*              the arguments */

/*                 MNCOR1 */
/*                 MXCOR1 */

/*              All angles have units of radians. The tolerance */
/*              ANGMRG is used for the comparisons shown below. */

/*                 1) Longitudes must be in the range */

/*                       -2*pi  :  2*pi */

/*                    Values that are out of range by no more than */
/*                    ANGMRG are bracketed to be in range. */


/*                 2) It is acceptable for the longitude bounds to be */
/*                    out of order. If */

/*                       MXCOR1 < MNCOR1 */

/*                    then either MXCOR1 is treated by the DSK */
/*                    subsystem as though it were MXCOR1 + 2*pi, or */
/*                    MNCOR1 is treated as MNCOR1 - 2*pi: whichever */
/*                    shift puts the bounds in the allowed range is */
/*                    made. */

/*                    The input longitude bounds must not be equal. */
/*                    If the lower bound is greater than the upper */
/*                    bound, the difference between the bounds must */
/*                    not be an integer multiple of 2*pi. */

/*                 3) MXCOR1 must not exceed MNCOR1 by more than 2*pi. */
/*                    Values that are out of range by no more than */
/*                    ANGMRG are bracketed to be in range. */


/*     FIRST, */
/*     LAST     are the endpoints of the time interval over which */
/*              this data set is applicable. These times are */
/*              expressed as seconds past J2000 TDB. */

/*     NV       is the number of vertices belonging to the plate */
/*              model. */

/*     VRTCES   is an array of coordinates of the vertices. */
/*              The Ith vertex occupies elements (1:3,I) of */
/*              this array. */

/*     NP       is the number of plates in the plate model. */

/*     PLATES   is an array representing the plates of the model. */
/*              The elements of PLATES are vertex indices. The vertex */
/*              indices of the Ith plate occupy elements (1:3,I) of */
/*              this array. */

/*     SPAIXD, */
/*     SPAIXI   are, respectively, the double precision and integer */
/*              components of the spatial index of the segment. */

/*              It is strongly recommended that the helper routine */
/*              DSKMI2 be used to create these arrays. See the */
/*              $Examples section below. */

/* $ Detailed_Output */

/*     None. This routine operates by side effects. */

/* $ Parameters */

/*     See the SPICELIB include files */

/*        dsk02.inc */
/*        dskdsc.inc */
/*        dsktol.inc */

/*     for declarations and detailed descriptions of the parameters */
/*     referenced in this header. */

/* $ Exceptions */

/*     1)  If the reference frame name FRAME could not be mapped to */
/*         an ID code, the error SPICE(FRAMEIDNOTFOUND) is signaled. */

/*     2)  If the segment stop time precedes the start time, the */
/*         error SPICE(TIMESOUTOFORDER) is signaled. */

/*     3)  If an input longitude value is outside the range */

/*            [ -2*pi - ANGMRG,   2*pi + ANGMRG ] */

/*         the error SPICE(VALUEOUTOFRANGE) is signaled. Longitudes */
/*         outside of the range by a smaller amount than ANGMRG will be */
/*         truncated to lie in the interval [-2*pi, 2*pi]. */

/*     4)  If the absolute value of the difference between the input */
/*         maximum longitude and the minimum longitude is more than 2*pi */
/*         + ANGMRG, the error SPICE(INVALIDLONEXTENT) is signaled. */
/*         If either longitude bound exceeds the other by an amount */
/*         between 2*pi and 2*pi+ANGMRG, the larger value will be */
/*         truncated to the smaller value plus 2*pi. */

/*     5)  If an input latitude value is outside the range */

/*            [ -pi/2 - ANGMRG,   pi/2 + ANGMRG ] */

/*         the error SPICE(VALUEOUTOFRANGE) is signaled. Latitudes */
/*         outside of the range by a smaller amount than ANGMRG will be */
/*         truncated to lie in the interval [-pi/2, pi/2]. */

/*     6)  If the coordinate system is latitudinal and the lower radius */
/*         bound is negative, or if the upper radius bound is */
/*         non-positive, the error SPICE(VALUEOUTOFRANGE) is signaled. */

/*     7)  If the coordinate system is latitudinal or planetodetic and */
/*         the bounds of the latitude, radius or altitude coordinate are */
/*         out of order, the error SPICE(BOUNDSOUTOFORDER) is signaled. */

/*     8)  If the coordinate system is latitudinal or planetodetic and */
/*         the lower and upper bounds of the longitude, latitude, radius */
/*         or altitude coordinate, respectively, are equal, the error */
/*         SPICE(ZEROBOUNDSEXTENT) is signaled. If the lower */
/*         longitude bound is greater than the upper bound, and if the */
/*         difference between the bounds is an integer multiple of 2*pi, */
/*         the same error is signaled. */

/*     9)  If the coordinate system is planetodetic and the input */
/*         equatorial radius is non-positive, the error */
/*         SPICE(VALUEOUTOFRANGE) is signaled. */

/*     10) If the coordinate system is planetodetic and the input */
/*         flattening coefficient is greater than or equal to 1, the */
/*         error SPICE(VALUEOUTOFRANGE) is signaled. */

/*     11) If the coordinate system is planetodetic, and if the minimum */
/*         altitude is less than the maximum of */

/*                    2           2 */
/*              {  -(B / A),   -(A / B)  } */

/*         where A and B are the semi-major and semi-minor axis lengths */
/*         of the reference ellipsoid, the error */
/*         SPICE(DEGENERATESURFACE) is signaled. */

/*     12) If the coordinate system is rectangular and any coordinate */
/*         lower bound is greater than or equal to the corresponding */
/*         upper bound, the error SPICE(BOUNDSOUTOFORDER) is signaled. */

/*     13) If the coordinate system code is not recognized, the error */
/*         SPICE(NOTSUPPORTED) is signaled. */

/*     14) If any vertex index belonging to an input plate is outside of */
/*         the range 1:NV, the error SPICE(BADVERTEXINDEX) is signaled. */

/*     15) If NV is less than 1 or greater than MAXVRT, the error */
/*         SPICE(VALUEOUTOFRANGE) is signaled. */

/*     16) If NP is less than 1 or greater than MAXPLT, the error */
/*         SPICE(VALUEOUTOFRANGE) is signaled. */

/*     17) If any voxel grid extent is less than 1 or greater than */
/*         MAXVOX, the error SPICE(VALUEOUTOFRANGE) is signaled. */

/*     18) If the voxel count is greater than MAXVOX, the error */
/*         SPICE(VALUEOUTOFRANGE) is signaled. */

/*     19) If the coarse voxel count is less than 1 or greater than */
/*         MAXCGR, the error SPICE(VALUEOUTOFRANGE) is signaled. */

/*     20) If the coarse voxel scale is less than 1 or more than */
/*         the cube root of the fine voxel count, the error */
/*         SPICE(VALUEOUTOFRANGE) is signaled. */

/*     21) If the cube of the coarse voxel scale does not divide the */
/*         fine voxel count evenly, the error SPICE(INCOMPATIBLESCALE) */
/*         is signaled. */

/*     22) If the input data class is not recognized, the error */
/*         SPICE(NOTSUPPORTED) is signaled. */

/*     23) If an error occurs while writing the segment to the output */
/*         DSK file, the error is signaled by a routine in the call */
/*         tree of this routine. */

/* $ Files */

/*     See argument HANDLE. */

/* $ Particulars */

/*     This routine writes a type 2 segment to a DSK file that */
/*     has been opened for write access. */

/*     Users planning to create DSK files should consider whether the */
/*     SPICE DSK creation utility MKDSK may be suitable for their needs. */

/*     This routine is supported by the routines DSKMI2 and DSKRB2 */
/*     DSKMI2 simplifies use of this routine by creating the "spatial */
/*     index" arrays required as inputs by this routine. DSKRB2 computes */
/*     bounds on the third coordinate of the input plate set. */

/*     Spatial Indexes */
/*     =============== */

/*     A spatial index is a group of data structures that facilitates */
/*     rapid high-level computations involving sets of plates. The data */
/*     structures created by this routine are aggregated into arrays */
/*     of type INTEGER and type DOUBLE PRECISION. */


/*     Voxel grids */
/*     =========== */

/*     A key geometric computation---probably the most important, as it */
/*     serves as a foundation for other high-level computations---is */
/*     finding the intersection of a ray with the plate set. DSK type 2 */
/*     segments use data structures called "voxel grids" as part of */
/*     their indexing mechanism. There is a "coarse grid": a box that */
/*     completely encloses a DSK type 2 segment's plate set, and which */
/*     is composed of identically-sized cubes called "coarse voxels." */
/*     Each coarse voxel in composed of smaller cubes called "fine */
/*     voxels." When the term "voxel" is used without qualification, it */
/*     refers to fine voxels. */

/*     Type 2 DSK segments contain data structures that associate plates */
/*     with the fine voxels intersected by those plates. These */
/*     structures enable the type 2 DSK software to rapidly find plates */
/*     in a given region of space. */

/*     Voxel scales */
/*     ============ */

/*     There are two voxel scales: */

/*     -  The coarse voxel scale is the integer ratio of the */
/*        edge length of a coarse voxel to the edge length of */
/*        a fine voxel */

/*     -  The fine voxel scale is the double precision ratio */
/*        of the edge length of a fine voxel to the average */
/*        extent of the plates in the input plate set. "Extents" */
/*        of a plate are the absolute values of the differences */
/*        between the respective maximum and minimum X, Y, and Z */
/*        coordinates of the plate's vertices. */

/*     Voxel scales determine the resolution of the voxel grid. */
/*     Voxel scales must be chosen to satisfy size constraints and */
/*     provide reasonable plate lookup performance. */

/*     The following considerations apply to spatial indexes of */
/*     type 2 DSK segments: */

/*        1)  The maximum number of coarse voxels is fixed at MAXCGR */
/*            (declared in dsk02.inc). */

/*        2)  If there are too few fine voxels, the average number of */
/*            plates per fine voxel will be very large. This largely */
/*            negates the performance improvement afforded by having an */
/*            index. Also, the number of plates per voxel may exceed */
/*            limits imposed by DSK subroutines that use static arrays. */

/*        3)  If there are too many fine voxels, the average number of */
/*            voxels intersected by a given plate may be too large for */
/*            all the plate-voxel associations to be stored. In */
/*            addition, the time needed to examine the plate lists for */
/*            each voxel (including the empty ones) may become quite */
/*            large, again negating the value of the index. */

/*     In many cases, voxel scales yielding optimum performance must be */
/*     determined by experiment. However, the following heuristics can */
/*     provide reasonable starting values: */

/*        Let NP be the number of plates. Let FS be the fine voxel */
/*        scale. Then a reasonable value of FS may be */

/*                   (0.25D0) */
/*           FS =  NP       / 8.D0 */

/*        In general, FS should not smaller than 1. */

/* $ Examples */

/*     The numerical results shown for this example may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */

/*     1) Create a three-segment DSK file using plate model data for */
/*        Phobos. Use latitudinal, rectangular, and planetodetic */
/*        coordinates in the respective segments. This is not a */
/*        realistic example, but it serves to demonstrate use of */
/*        the supported coordinate systems. */

/*        Use the DSK kernel below to provide, for simplicity, the */
/*        input plate and vertex data. The selected input file has one */
/*        segment. */

/*           phobos_3_3.bds */


/*        Example code begins here. */


/*        C */
/*        C     Example program for DSKW02, DSKMI2, and DSKRB2 */
/*        C */
/*        C        Create a three-segment DSK file using plate model */
/*        C        data for Phobos. Use latitudinal, rectangular, and */
/*        C        planetodetic coordinates in the respective segments. */
/*        C */
/*        C        For simplicity, use an existing DSK file to provide */
/*        C        the input plate and vertex data. The selected input */
/*        C        file has one segment. */
/*        C */
/*        C           Version 1.0.0 22-JAN-2016 (NJB) */
/*        C */
/*              PROGRAM DSKW02_EX1 */
/*              IMPLICIT NONE */

/*              INCLUDE 'dla.inc' */
/*              INCLUDE 'dskdsc.inc' */
/*              INCLUDE 'dsk02.inc' */

/*        C */
/*        C     SPICELIB functions */
/*        C */
/*              DOUBLE PRECISION      JYEAR */
/*              DOUBLE PRECISION      PI */
/*        C */
/*        C     Local parameters */
/*        C */
/*              INTEGER               FRNMLN */
/*              PARAMETER           ( FRNMLN = 32 ) */

/*              INTEGER               NSEG */
/*              PARAMETER           ( NSEG   = 3 ) */

/*              INTEGER               NAMLEN */
/*              PARAMETER           ( NAMLEN = 20 ) */

/*              INTEGER               FILSIZ */
/*              PARAMETER           ( FILSIZ = 255 ) */

/*              INTEGER               LNSIZE */
/*              PARAMETER           ( LNSIZE = 80 ) */

/*              INTEGER               NCOR */
/*              PARAMETER           ( NCOR   = 4 ) */

/*        C */
/*        C     Local variables */
/*        C */
/*              CHARACTER*(NAMLEN)    CORNAM ( NCOR ) */
/*              CHARACTER*(FILSIZ)    DSK */
/*              CHARACTER*(FRNMLN)    FRAME */
/*              CHARACTER*(FILSIZ)    INDSK */
/*              CHARACTER*(LNSIZE)    LINE */
/*        C */
/*        C     Note: the values of MAXVRT and MAXPLT declared */
/*        C     in dsk02.inc, and the integer spatial index */
/*        C     dimension SPAISZ are very large. Smaller buffers */
/*        C     can be used for most applications. */
/*        C */
/*              DOUBLE PRECISION      CORPAR ( NSYPAR ) */
/*              DOUBLE PRECISION      F */
/*              DOUBLE PRECISION      FINSCL */
/*              DOUBLE PRECISION      FIRST */
/*              DOUBLE PRECISION      LAST */
/*              DOUBLE PRECISION      MNCOR1 */
/*              DOUBLE PRECISION      MNCOR2 */
/*              DOUBLE PRECISION      MNCOR3 */
/*              DOUBLE PRECISION      MXCOR1 */
/*              DOUBLE PRECISION      MXCOR2 */
/*              DOUBLE PRECISION      MXCOR3 */
/*              DOUBLE PRECISION      RE */
/*              DOUBLE PRECISION      RP */
/*              DOUBLE PRECISION      SPAIXD ( IXDFIX ) */
/*              DOUBLE PRECISION      VRTCES ( 3, MAXVRT ) */

/*              INTEGER               CENTER */
/*              INTEGER               CORSCL */
/*              INTEGER               CORSYS */
/*              INTEGER               DCLASS */
/*              INTEGER               DLADSC ( DLADSZ ) */
/*              INTEGER               HANDLE */
/*              INTEGER               INHAN */
/*              INTEGER               NP */
/*              INTEGER               NV */
/*              INTEGER               PLATES ( 3, MAXPLT ) */
/*              INTEGER               SEGNO */
/*              INTEGER               SPAIXI ( SPAISZ ) */
/*              INTEGER               SURFID */
/*              INTEGER               VOXPSZ */
/*              INTEGER               VOXLSZ */
/*              INTEGER               WORK   ( 2, MAXCEL ) */
/*              INTEGER               WORKSZ */

/*              LOGICAL               FOUND */
/*        C */
/*        C     Saved variables */
/*        C */
/*        C     Save all large arrays to avoid stack problems. */
/*        C */
/*              SAVE */
/*        C */
/*        C     Initial values */
/*        C */
/*              DATA                  CORNAM / 'radius', */
/*             .                               'Z-coordinate', */
/*             .                               'Z-coordinate', */
/*             .                               'altitude'     / */

/*        C */
/*        C     Assign names of input and output DSK files. */
/*        C */
/*              INDSK = 'phobos_3_3.bds' */
/*              DSK   = 'phobos_3_3_3seg.bds' */
/*        C */
/*        C     Open input DSK for read access; find first segment. */
/*        C */
/*              CALL DASOPR ( INDSK, INHAN ) */
/*              CALL DLABFS ( INHAN, DLADSC, FOUND ) */
/*        C */
/*        C     Fetch vertices and plates from input DSK file. */
/*        C */
/*              WRITE (*,*) 'Reading input data...' */

/*              CALL DSKV02 ( INHAN, DLADSC, 1, MAXVRT, NV, VRTCES ) */
/*              CALL DSKP02 ( INHAN, DLADSC, 1, MAXPLT, NP, PLATES ) */

/*              WRITE (*,*) 'Done.' */
/*        C */
/*        C     Set input array sizes required by DSKMI2. */
/*        C */
/*              VOXPSZ = MAXVXP */
/*              VOXLSZ = MXNVLS */
/*              WORKSZ = MAXCEL */
/*        C */
/*        C     Set fine and coarse voxel scales. (These usually */
/*        C     need to determined by experimentation.) */
/*        C */
/*              FINSCL = 5.D0 */
/*              CORSCL = 4 */
/*        C */
/*        C     Open a new DSK file. */
/*        C */
/*              CALL DSKOPN ( DSK, DSK, 0, HANDLE ) */
/*        C */
/*        C     Create three segments and add them to the file. */
/*        C */
/*              DO SEGNO = 1, NSEG */
/*        C */
/*        C        Create spatial index. */
/*        C */
/*                 WRITE (*,*) 'Creating segment ', SEGNO */
/*                 WRITE (*,*) 'Creating spatial index...' */

/*                 CALL DSKMI2 ( NV,     VRTCES, NP,     PLATES, FINSCL, */
/*             .                 CORSCL, WORKSZ, VOXPSZ, VOXLSZ, .TRUE., */
/*             .                 SPAISZ, WORK,   SPAIXD, SPAIXI        ) */

/*                 WRITE (*,*) 'Done.' */
/*        C */
/*        C        Set up inputs describing segment attributes: */
/*        C */
/*        C        - Central body: Phobos */
/*        C        - Surface ID code: user's choice. */
/*        C          We use the segment number here. */
/*        C        - Data class: general (arbitrary) shape */
/*        C        - Body-fixed reference frame */
/*        C        - Time coverage bounds (TBD) */
/*        C */
/*                 CENTER = 401 */
/*                 SURFID = SEGNO */
/*                 DCLASS = GENCLS */
/*                 FRAME  = 'IAU_PHOBOS' */

/*                 FIRST = -50 * JYEAR() */
/*                 LAST  =  50 * JYEAR() */
/*        C */
/*        C        Set the coordinate system and coordinate system */
/*        C        bounds based on the segment index. */
/*        C */
/*        C        Zero out the coordinate parameters to start. */
/*        C */
/*                 CALL CLEARD ( NSYPAR, CORPAR ) */

/*                 IF ( SEGNO .EQ. 1 ) THEN */
/*        C */
/*        C           Use planetocentric latitudinal coordinates. Set */
/*        C           the longitude and latitude bounds. */
/*        C */
/*                    CORSYS = LATSYS */

/*                    MNCOR1 = -PI() */
/*                    MXCOR1 =  PI() */
/*                    MNCOR2 = -PI()/2 */
/*                    MXCOR2 =  PI()/2 */

/*                 ELSE IF ( SEGNO .EQ. 2 ) THEN */
/*        C */
/*        C           Use rectangular coordinates. Set the */
/*        C           X and Y bounds. */
/*        C */
/*        C           The bounds shown here were derived from */
/*        C           the plate data. They lie slightly outside */
/*        C           of the range spanned by the plates. */
/*        C */
/*                    CORSYS = RECSYS */

/*                    MNCOR1 = -1.3D0 */
/*                    MXCOR1 =  1.31D0 */
/*                    MNCOR2 = -1.21D0 */
/*                    MXCOR2 =  1.2D0 */

/*                 ELSE */
/*        C */
/*        C           Set the coordinate system to planetodetic. */
/*        C */
/*                    CORSYS    = PDTSYS */

/*                    MNCOR1    = -PI() */
/*                    MXCOR1    =  PI() */
/*                    MNCOR2    = -PI()/2 */
/*                    MXCOR2    =  PI()/2 */
/*        C */
/*        C           We'll use equatorial and polar radii from */
/*        C           pck00010.tpc. These normally would be fetched */
/*        C           at run time, but for simplicity, we'll use */
/*        C           hard-coded values. */

/*                    RE        = 13.0D0 */
/*                    RP        =  9.1D0 */
/*                    F         = ( RE - RP ) / RE */

/*                    CORPAR(1) = RE */
/*                    CORPAR(2) = F */

/*                 END IF */
/*        C */
/*        C        Compute plate model radius bounds. */
/*        C */
/*                 LINE = 'Computing # bounds of plate set...' */

/*                 CALL REPMC ( LINE, '#', CORNAM(CORSYS), LINE ) */
/*                 WRITE (*,*) LINE */

/*                 CALL DSKRB2 ( NV,     VRTCES, NP,     PLATES, */
/*             .                 CORSYS, CORPAR, MNCOR3, MXCOR3 ) */

/*                 WRITE (*,*) 'Done.' */
/*        C */
/*        C        Write the segment to the file. */
/*        C */
/*                 WRITE (*,*) 'Writing segment...' */

/*                 CALL DSKW02 ( HANDLE, */
/*             .                 CENTER, SURFID, DCLASS, FRAME,  CORSYS, */
/*             .                 CORPAR, MNCOR1, MXCOR1, MNCOR2, MXCOR2, */
/*             .                 MNCOR3, MXCOR3, FIRST,  LAST,   NV, */
/*             .                 VRTCES, NP,     PLATES, SPAIXD, SPAIXI ) */

/*                 WRITE (*,*) 'Done.' */

/*              END DO */
/*        C */
/*        C     Segregate the data records in the DSK file and */
/*        C     close the file. */
/*        C */
/*              WRITE (*,*) 'Segregating and closing DSK file...' */

/*              CALL DSKCLS ( HANDLE, .TRUE. ) */

/*              WRITE (*,*) 'Done.' */
/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*         Reading input data... */
/*         Done. */
/*         Creating segment            1 */
/*         Creating spatial index... */
/*         Done. */
/*         Computing radius bounds of plate set... */
/*         Done. */
/*         Writing segment... */
/*         Done. */
/*         Creating segment            2 */
/*         Creating spatial index... */
/*         Done. */
/*         Computing Z-coordinate bounds of plate set... */
/*         Done. */
/*         Writing segment... */
/*         Done. */
/*         Creating segment            3 */
/*         Creating spatial index... */
/*         Done. */
/*         Computing altitude bounds of plate set... */
/*         Done. */
/*         Writing segment... */
/*         Done. */
/*         Segregating and closing DSK file... */
/*         Done. */


/*        Note that after run completion, a new DSK exists in the output */
/*        directory. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */

/* $ Version */

/* -    SPICELIB Version 1.0.1, 28-AUG-2021 (JDR) (NJB) */

/*        Edited the header to comply with NAIF standard. */
/*        Added solution to code example. */

/*        Corrected description of coverage requirements for class 1 */
/*        segments. */

/*        Deleted paragraph saying that, except for changes made to */
/*        move longitude values into range, the values are stored in */
/*        segment "as is." */

/* -    SPICELIB Version 1.0.0, 04-MAR-2017 (NJB) */

/*        Fixed some comment typos. */

/*     10-OCT-2016 (NJB) */

/*        New error checks on inputs were added. */

/*     07-MAR-2016 (NJB) */

/*        New error checks on inputs were added. */

/*        Argument list change: spatial index is now passed in */
/*        as two arrays: SPAIXD and SPAIXI. */

/*        Argument CORPAR was added. */

/*        Double precision data are now written to the output */
/*        segment before integer data. */

/*        22-AUG-2012 (NJB) */

/*           Bug fix: corrected upper bound in test for */
/*           vertex count. */

/*        13-MAY-2010 (NJB) */

/*           Updated to reflect new type 2 segment design: normal */
/*           vectors, plate centers, and lengths of longest plate sides */
/*           are no longer stored in these segments. */

/*        03-APR-2010 (NJB) */

/*           New interface; general coordinates are supported. Time */
/*           bounds, surface ID, data class, and bounds of third */
/*           coordinate have been added. Albedo inputs have been */
/*           deleted. */

/*        09-OCT-2009 (NJB) */

/*           Header was added. */

/*        31-OCT-2006 (NJB) */

/*           Input arguments CGSCAL and VTXBDS were removed. */

/*        27-JUN-2006 (NJB) */

/*           Initial version. */

/* -& */
/* $ Index_Entries */

/*     write a type 2 DSK segment */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     Local variables */

    /* Parameter adjustments */
    plates_dim2 = *np;

    /* Function Body */
    if (return_()) {
	return 0;
    }
    chkin_("DSKW02", (ftnlen)6);

/*     Map the input reference frame name to an ID code. */

    namfrm_(frame, &frmcde, frame_len);
    if (frmcde == 0) {
	setmsg_("Input reference frame # could not be mapped to an ID code. "
		"The frame name might be misspelled, or possibly a required f"
		"rame kernel was not loaded. ", (ftnlen)147);
	errch_("#", frame, (ftnlen)1, frame_len);
	sigerr_("SPICE(FRAMEIDNOTFOUND)", (ftnlen)22);
	chkout_("DSKW02", (ftnlen)6);
	return 0;
    }

/*     Make sure the time bounds are in order. */

    if (*last <= *first) {
	setmsg_("Segment time bounds must be increasing; bounds were #:#.", (
		ftnlen)56);
	errdp_("#", first, (ftnlen)1);
	errdp_("#", last, (ftnlen)1);
	sigerr_("SPICE(TIMESOUTOFORDER)", (ftnlen)22);
	chkout_("DSKW02", (ftnlen)6);
	return 0;
    }

/*     If applicable, check segment boundaries. Check the */
/*     coordinate system as well. */

    if (*corsys == 1 || *corsys == 4) {

/*        Reject invalid latitudes and longitudes. Move */
/*        values that are slightly out of range into range. */

/*        Longitude bounds must be distinct. */

	if (*mncor1 == *mxcor1) {
	    setmsg_("Minimum longitude # radians (# degrees) was equal to ma"
		    "ximum longitude. Longitude bounds must be distinct.", (
		    ftnlen)106);
	    errdp_("#", mncor1, (ftnlen)1);
	    d__1 = *mncor1 * dpr_();
	    errdp_("#", &d__1, (ftnlen)1);
	    sigerr_("SPICE(ZEROBOUNDSEXTENT)", (ftnlen)23);
	    chkout_("DSKW02", (ftnlen)6);
	    return 0;
	}

/*        Check minimum longitude. */

	if (*mncor1 < -twopi_() - 1e-12 || *mncor1 > twopi_() - 1e-12) {
	    setmsg_("Minimum longitude # radians (# degrees) was outside of "
		    "valid range [-2*pi, 2*pi - ANGMRG]", (ftnlen)89);
	    errdp_("#", mncor1, (ftnlen)1);
	    d__1 = *mncor1 * dpr_();
	    errdp_("#", &d__1, (ftnlen)1);
	    sigerr_("SPICE(VALUEOUTOFRANGE)", (ftnlen)22);
	    chkout_("DSKW02", (ftnlen)6);
	    return 0;
	}

/*        The minimum longitude is too small by ANGMRG, at worst. */
/*        Make it greater than or equal to -2*pi. */

/* Computing MAX */
	d__1 = -twopi_();
	segbds[0] = max(d__1,*mncor1);

/*        Check maximum longitude. */

	if (*mxcor1 < -twopi_() + 1e-12 || *mxcor1 > twopi_() + 1e-12) {
	    setmsg_("Maximum longitude # radians (# degrees) was outside of "
		    "valid range [-2*pi+ANGMRG, 2*pi]", (ftnlen)87);
	    errdp_("#", mxcor1, (ftnlen)1);
	    d__1 = *mxcor1 * dpr_();
	    errdp_("#", &d__1, (ftnlen)1);
	    sigerr_("SPICE(VALUEOUTOFRANGE)", (ftnlen)22);
	    chkout_("DSKW02", (ftnlen)6);
	    return 0;
	}

/*        The maximum longitude is too large by ANGMRG, at worst. */
/*        Make it less than or equal to 2*pi. */

/* Computing MIN */
	d__1 = twopi_();
	segbds[1] = min(d__1,*mxcor1);

/*        The longitude extent cannot exceed 2*pi. */

	if (*mxcor1 > *mncor1 + twopi_() + 1e-12) {
	    setmsg_("Longitude bounds #:# radians (#:# degrees) are too far "
		    "apart.", (ftnlen)61);
	    errdp_("#", mxcor1, (ftnlen)1);
	    errdp_("#", mxcor2, (ftnlen)1);
	    d__1 = *mxcor1 * dpr_();
	    errdp_("#", &d__1, (ftnlen)1);
	    d__1 = *mxcor2 * dpr_();
	    errdp_("#", &d__1, (ftnlen)1);
	    sigerr_("SPICE(INVALIDLONEXTENT)", (ftnlen)23);
	    chkout_("DSKW02", (ftnlen)6);
	    return 0;
	}
	if (*mxcor1 < *mncor1 - twopi_() - 1e-12) {
	    setmsg_("Longitude bounds #:# radians (#:# degrees) are too far "
		    "apart.", (ftnlen)61);
	    errdp_("#", mxcor1, (ftnlen)1);
	    errdp_("#", mxcor2, (ftnlen)1);
	    d__1 = *mxcor1 * dpr_();
	    errdp_("#", &d__1, (ftnlen)1);
	    d__1 = *mxcor2 * dpr_();
	    errdp_("#", &d__1, (ftnlen)1);
	    sigerr_("SPICE(INVALIDLONEXTENT)", (ftnlen)23);
	    chkout_("DSKW02", (ftnlen)6);
	    return 0;
	}
	if (segbds[1] > segbds[0]) {

/*           The upper bound exceeds the lower by at most 2*pi + ANGMRG. */
/*           Trim the upper bound to make the difference no more than */
/*           2*pi. */

/* Computing MIN */
	    d__1 = segbds[1], d__2 = segbds[0] + twopi_();
	    segbds[1] = min(d__1,d__2);
	} else if (segbds[1] < segbds[0]) {

/*           The lower bound exceeds the upper by at most 2*pi + ANGMRG. */
/*           Advance the upper bound, if necessary, to make the */
/*           difference no more than 2*pi. */

/* Computing MAX */
	    d__1 = segbds[1], d__2 = segbds[0] - twopi_();
	    segbds[1] = max(d__1,d__2);
	}

/*        Make sure the adjusted longitude bounds don't describe an */
/*        interval that could be interpreted as having length zero, */
/*        if the bounds were placed in order. If the lower bound is */
/*        greater than the upper bound, then the difference between */
/*        the bounds must not be an integer multiple of 2*pi. */

	if (segbds[1] == segbds[0] || segbds[1] == segbds[0] - twopi_()) {
	    setmsg_("After adjustment, minimum longitude # radians (# degree"
		    "s) was equal to maximum longitude. Longitude bounds must"
		    " be distinct.", (ftnlen)124);
	    errdp_("#", segbds, (ftnlen)1);
	    d__1 = *mncor1 * dpr_();
	    errdp_("#", &d__1, (ftnlen)1);
	    sigerr_("SPICE(ZEROBOUNDSEXTENT)", (ftnlen)23);
	    chkout_("DSKW02", (ftnlen)6);
	    return 0;
	}

/*        Check minimum latitude. */

	if (*mncor2 < -halfpi_() - 1e-12 || *mncor2 > halfpi_() - 1e-12) {
	    setmsg_("Minimum latitude # radians (# degrees) was outside of v"
		    "alid range [-pi/2, pi/2 - ANGMRG]", (ftnlen)88);
	    errdp_("#", mncor2, (ftnlen)1);
	    d__1 = *mncor2 * dpr_();
	    errdp_("#", &d__1, (ftnlen)1);
	    sigerr_("SPICE(VALUEOUTOFRANGE)", (ftnlen)22);
	    chkout_("DSKW02", (ftnlen)6);
	    return 0;
	}

/*        Trim the lower latitude bound to make it at least -pi/2. */

/* Computing MAX */
	d__1 = -halfpi_();
	segbds[2] = max(d__1,*mncor2);

/*        Check maximum latitude. */

	if (*mxcor2 < -halfpi_() + 1e-12 || *mxcor2 > halfpi_() + 1e-12) {
	    setmsg_("Maximum latitude # radians (# degrees) was outside of v"
		    "alid range [-pi/2+ANGMRG, pi/2]", (ftnlen)86);
	    errdp_("#", mxcor2, (ftnlen)1);
	    d__1 = *mxcor2 * dpr_();
	    errdp_("#", &d__1, (ftnlen)1);
	    sigerr_("SPICE(VALUEOUTOFRANGE)", (ftnlen)22);
	    chkout_("DSKW02", (ftnlen)6);
	    return 0;
	}

/*        Trim the upper latitude bound to make it no more than -pi/2. */

/* Computing MAX */
	d__1 = -halfpi_();
	segbds[2] = max(d__1,*mncor2);
/* Computing MIN */
	d__1 = halfpi_();
	segbds[3] = min(d__1,*mxcor2);

/*        The latitude bounds must be in order. */

	if (*mxcor2 < *mncor2) {
	    setmsg_("Latitude bounds # and # are out of order.", (ftnlen)41);
	    errdp_("#", mncor2, (ftnlen)1);
	    errdp_("#", mxcor2, (ftnlen)1);
	    sigerr_("SPICE(BOUNDSOUTOFORDER)", (ftnlen)23);
	    chkout_("DSKW02", (ftnlen)6);
	    return 0;
	}
	if (*corsys == 1) {

/*           The coordinate system is latitudinal. Check radius */
/*           bounds. */

	    if (*mncor3 < 0.) {
		setmsg_("Radius lower bound must be non-negative but was #.", 
			(ftnlen)50);
		errdp_("#", mncor3, (ftnlen)1);
		sigerr_("SPICE(VALUEOUTOFRANGE)", (ftnlen)22);
		chkout_("DSKW02", (ftnlen)6);
		return 0;
	    }
	    if (*mxcor3 <= 0.) {
		setmsg_("Radius upper bound must be strictly positive but wa"
			"s #.", (ftnlen)55);
		errdp_("#", mxcor3, (ftnlen)1);
		sigerr_("SPICE(VALUEOUTOFRANGE)", (ftnlen)22);
		chkout_("DSKW02", (ftnlen)6);
		return 0;
	    }
	}
	if (*corsys == 4) {

/*           The coordinate system is planetodetic. Check the coordinate */
/*           parameters as well. */

	    if (corpar[0] <= 0.) {
		setmsg_("Equatorial radius was #; this radius must be strict"
			"ly positive.", (ftnlen)63);
		errdp_("#", corpar, (ftnlen)1);
		sigerr_("SPICE(VALUEOUTOFRANGE)", (ftnlen)22);
		chkout_("DSKW02", (ftnlen)6);
		return 0;
	    }
	    if (corpar[1] >= 1.) {
		setmsg_("Flattening coefficient was #; this value must be st"
			"rictly less than 1.", (ftnlen)70);
		errdp_("#", &corpar[1], (ftnlen)1);
		sigerr_("SPICE(VALUEOUTOFRANGE)", (ftnlen)22);
		chkout_("DSKW02", (ftnlen)6);
		return 0;
	    }

/*           Make sure the surface of minimum altitude is smooth and */
/*           non-self-intersecting. */

	    a = corpar[0];
	    b = a * (1 - corpar[1]);
/* Computing MAX */
/* Computing 2nd power */
	    d__3 = a;
/* Computing 2nd power */
	    d__4 = b;
	    d__1 = -(d__3 * d__3) / b, d__2 = -(d__4 * d__4) / a;
	    altlim = max(d__1,d__2);
	    if (*mncor3 <= altlim) {
		setmsg_("Reference ellipsoid has semi-axis lengths # and #. "
			"The minimum altitude was #. The minimum altitude is "
			"required to be greater than the maximum of {-(A**2)/"
			"B, -(B**2)/A}, which is #.", (ftnlen)181);
		errdp_("#", &a, (ftnlen)1);
		errdp_("#", &b, (ftnlen)1);
		errdp_("#", mncor3, (ftnlen)1);
		errdp_("#", &altlim, (ftnlen)1);
		sigerr_("SPICE(DEGENERATESURFACE)", (ftnlen)24);
		chkout_("DSKW02", (ftnlen)6);
		return 0;
	    }
	}

/*        The bounds of the third coordinate, whether radius or altitude, */
/*        must be in order and must have positive extent. */

	if (*mxcor3 < *mncor3) {
	    if (*corsys == 1) {
		setmsg_("Radius bounds # and # are out of order", (ftnlen)38);
	    } else {
		setmsg_("Altitude bounds # and # are out of order.", (ftnlen)
			41);
	    }
	    errdp_("#", mncor3, (ftnlen)1);
	    errdp_("#", mxcor3, (ftnlen)1);
	    sigerr_("SPICE(BOUNDSOUTOFORDER)", (ftnlen)23);
	    chkout_("DSKW02", (ftnlen)6);
	    return 0;
	}
	if (*mxcor3 == *mncor3) {
	    if (*corsys == 1) {
		setmsg_("Radius bounds # and # must have positive extent but"
			" are equal.", (ftnlen)62);
	    } else {
		setmsg_("Altitude bounds # and # must have positive extent b"
			"ut are equal.", (ftnlen)64);
	    }
	    errdp_("#", mncor3, (ftnlen)1);
	    errdp_("#", mxcor3, (ftnlen)1);
	    sigerr_("SPICE(ZEROBOUNDSEXTENT)", (ftnlen)23);
	    chkout_("DSKW02", (ftnlen)6);
	    return 0;
	}
    } else if (*corsys == 3) {

/*        All coordinate bounds must be in strictly increasing order. */

	if (*mxcor1 <= *mncor1 || *mxcor2 <= *mncor2 || *mxcor3 <= *mncor3) {
	    setmsg_("Rectangular coordinate bounds must be strictly increasi"
		    "ng in each dimension. The bounds were:  X = #:#; Y = #:#"
		    "; Z = #:#.", (ftnlen)121);
	    errdp_("#", mncor1, (ftnlen)1);
	    errdp_("#", mxcor1, (ftnlen)1);
	    errdp_("#", mncor2, (ftnlen)1);
	    errdp_("#", mxcor2, (ftnlen)1);
	    errdp_("#", mncor3, (ftnlen)1);
	    errdp_("#", mxcor3, (ftnlen)1);
	    sigerr_("SPICE(BOUNDSOUTOFORDER)", (ftnlen)23);
	    chkout_("DSKW02", (ftnlen)6);
	    return 0;
	}
	segbds[0] = *mncor1;
	segbds[1] = *mxcor1;
	segbds[2] = *mncor2;
	segbds[3] = *mxcor2;
    } else {
	setmsg_("Coordinate system code # is not recognized.", (ftnlen)43);
	errint_("#", corsys, (ftnlen)1);
	sigerr_("SPICE(NOTSUPPORTED)", (ftnlen)19);
	chkout_("DSKW02", (ftnlen)6);
	return 0;
    }

/*     Check the data class. */

    if (*dclass < 1 || *dclass > 2) {
	setmsg_("Data class # is not recognized.", (ftnlen)31);
	errint_("#", dclass, (ftnlen)1);
	sigerr_("SPICE(NOTSUPPORTED)", (ftnlen)19);
	chkout_("DSKW02", (ftnlen)6);
	return 0;
    }

/*     Check NV and NP. */

/*     Note that we don't apply Euler's law, since the data */
/*     set need not represent a complete surface. */

    if (*nv < 1 || *nv > 16000002) {
	setmsg_("Vertex count NV = #; count must be in the range 1:#.", (
		ftnlen)52);
	errint_("#", nv, (ftnlen)1);
	errint_("#", &c_b116, (ftnlen)1);
	sigerr_("SPICE(VALUEOUTOFRANGE)", (ftnlen)22);
	chkout_("DSKW02", (ftnlen)6);
	return 0;
    }
    if (*np < 1 || *np > 32000000) {
	setmsg_("Plate count NP = #; count must be in the range 1:#.", (
		ftnlen)51);
	errint_("#", np, (ftnlen)1);
	errint_("#", &c_b122, (ftnlen)1);
	sigerr_("SPICE(VALUEOUTOFRANGE)", (ftnlen)22);
	chkout_("DSKW02", (ftnlen)6);
	return 0;
    }

/*     Check the vertex indices in the plates. */

    i__1 = *np;
    for (i__ = 1; i__ <= i__1; ++i__) {
	for (j = 1; j <= 3; ++j) {
	    k = plates[(i__2 = j + i__ * 3 - 4) < plates_dim2 * 3 && 0 <= 
		    i__2 ? i__2 : s_rnge("plates", i__2, "dskw02_", (ftnlen)
		    1446)];
	    if (k < 1 || k > *nv) {
		setmsg_("Vertex index # of plate # was #; vertex indices mus"
			"t be in the range 1:NV. The input NV = #.", (ftnlen)
			92);
		errint_("#", &j, (ftnlen)1);
		errint_("#", &i__, (ftnlen)1);
		errint_("#", &k, (ftnlen)1);
		errint_("#", nv, (ftnlen)1);
		sigerr_("SPICE(BADVERTEXINDEX)", (ftnlen)21);
		chkout_("DSKW02", (ftnlen)6);
		return 0;
	    }
	}
    }

/*     Locate the spatial index elements. Some of the elements are at */
/*     fixed addresses; for others the addresses must be calculated. */

/*     The two components of the spatial index together contain the */
/*     following items: */

/*        VGREXT      is an array containing the extents of the voxel */
/*                    grid in the X, Y, and Z directions of the */
/*                    body-fixed frame. The extents are measured as */
/*                    voxel counts. */

/*        ORIGIN      is the position, in the body-fixed, body-centered */
/*                    reference frame associated with BODY, of the */
/*                    origin of the both the fine and coarse voxel grids */
/*                    associated with this model. */

/*        VOXSIZ      is the voxel edge length in km. */

/*        CGRSCL      is the coarse voxel grid scale factor: the edge */
/*                    length of each coarse voxel is scaled up from the */
/*                    length of a fine voxel edge by this factor. */

/*        CGRPTR      is an array of pointers associated with this */
/*                    model's coarse voxel grid; these pointers map */
/*                    one-dimensional coarse voxel indices to start */
/*                    indices in the fine voxel pointer list. */

/*        VOXNPT      is the cardinality of the fine voxel pointer list. */

/*        VOXPTR      is the fine voxel pointer list. For each fine */
/*                    voxel belonging to a non-empty coarse voxel, there */
/*                    is a pointer in this list that identifies the */
/*                    start index in VOXPLT of the list of plate indices */
/*                    associated with this fine voxel. */

/*                    The start index in VOXPTR of the set of pointers */
/*                    associated with a coarse voxel is given by the */
/*                    element of CGRPTR associated with that coarse */
/*                    voxel. */

/*                    Within a given coarse voxel, each fine voxel has */
/*                    an associated one-dimensional offset from the */
/*                    corner of the coarse voxel closest to the origin */
/*                    of the voxel grids. This offset gives the location */
/*                    in VOXPTR of the plate list pointer for the fine */
/*                    voxel. */

/*        VOXNPL      is the cardinality of the plate list of the fine */
/*                    voxel-plate mapping. */

/*        VOXPLT      is the plate list of the fine voxel-plate mapping. */

/*        VTXPTR      is the vertex pointer list. */

/*        VTXNPL      is the cardinality of the plate list of the */
/*                    vertex-plate mapping. */



/*     Extract double precision elements of the spatial index. */

    moved_(spaixd, &c__6, vtxbds);
    vequ_(&spaixd[6], voxori);
    voxsiz = spaixd[9];

/*     Extract scalars and small fixed-size arrays from the integer */
/*     component of the spatial index. */

/*     Fetch grid extents (in units of whole voxels): */

    movei_(spaixi, &c__3, vgrext);

/*     Fetch coarse grid scale, voxel pointer count, and voxel-plate */
/*     list count. */

    cgrscl = spaixi[3];
    voxnpt = spaixi[4];
    voxnpl = spaixi[5];

/*     Create a pointer to the voxel-plate pointer array. */

    pvoxpt = 100008;

/*     Create a pointer to the voxel-plate list array. */

    pvoxpl = pvoxpt + voxnpt;

/*     Create a pointer to the vertex pointer array. */

    pvtxpt = pvoxpl + voxnpl;

/*     Create a pointer to the vertex-plate list array. */

    pvtxpl = pvtxpt + *nv;

/*     Fetch vertex-plate list size. */

    vtxnpl = spaixi[6];

/*     Check the input parameters. */



/*     Make sure the voxel grid extents are within range. */

    for (i__ = 1; i__ <= 3; ++i__) {
	if (vgrext[(i__1 = i__ - 1) < 3 && 0 <= i__1 ? i__1 : s_rnge("vgrext",
		 i__1, "dskw02_", (ftnlen)1584)] < 1 || vgrext[(i__2 = i__ - 
		1) < 3 && 0 <= i__2 ? i__2 : s_rnge("vgrext", i__2, "dskw02_",
		 (ftnlen)1584)] > 100000000) {
	    setmsg_("Voxel grid extents are = (#, #, #); all be in the range"
		    " 1:#.", (ftnlen)60);
	    errint_("#", vgrext, (ftnlen)1);
	    errint_("#", &vgrext[1], (ftnlen)1);
	    errint_("#", &vgrext[2], (ftnlen)1);
	    errint_("#", &c_b145, (ftnlen)1);
	    sigerr_("SPICE(VALUEOUTOFRANGE)", (ftnlen)22);
	    chkout_("DSKW02", (ftnlen)6);
	    return 0;
	}
    }

/*     Make sure the number of voxels NVXTOT is within range. */

    nvxtot = vgrext[0] * vgrext[1] * vgrext[2];
    if (nvxtot > 100000000) {
	setmsg_("Fine voxel count NVXTOT = #; count must be in the range 1:#."
		, (ftnlen)60);
	errint_("#", &nvxtot, (ftnlen)1);
	errint_("#", &c_b145, (ftnlen)1);
	sigerr_("SPICE(VALUEOUTOFRANGE)", (ftnlen)22);
	chkout_("DSKW02", (ftnlen)6);
	return 0;
    }

/*     Check the coarse voxel scale. It must be at least 1, and its */
/*     cube must not exceed the fine voxel count. */

    d__1 = (doublereal) nvxtot;
    if (cgrscl < 1 || (doublereal) cgrscl > pow_dd(&d__1, &c_b154)) {
	setmsg_("Coarse voxel scale = #; scale must be in the range 1:NVXTOT"
		"**3, where NVXTOT is the total fine voxel count. In this cas"
		"e, NVXTOT = #.", (ftnlen)133);
	errint_("#", &cgrscl, (ftnlen)1);
	errint_("#", &nvxtot, (ftnlen)1);
	sigerr_("SPICE(VALUEOUTOFRANGE)", (ftnlen)22);
	chkout_("DSKW02", (ftnlen)6);
	return 0;
    }

/*     The cube of the coarse scale must divide the total voxel count */
/*     evenly. */

/* Computing 3rd power */
    i__1 = cgrscl;
    q = nvxtot / (i__1 * (i__1 * i__1));
/* Computing 3rd power */
    i__1 = cgrscl;
    r__ = (doublereal) (nvxtot - q * (i__1 * (i__1 * i__1)));
    if (r__ != 0.) {
	setmsg_("Coarse voxel scale = #; the cube of the scale must divide N"
		"VXTOT evenly, where NVXTOT is the total  fine voxel count. I"
		"n this case, NVXTOT = #.", (ftnlen)143);
	errint_("#", &cgrscl, (ftnlen)1);
	errint_("#", &nvxtot, (ftnlen)1);
	sigerr_("SPICE(INCOMPATIBLESCALE)", (ftnlen)24);
	chkout_("DSKW02", (ftnlen)6);
	return 0;
    }

/*     NCGR        is the number of voxels in the coarse voxel grid */
/*                 associated with this model. Since each coarse voxel */
/*                 is a cube containing an integer number of fine */
/*                 voxels, this number is determined by NVXTOT and */
/*                 CGRSCL. */

/* Computing 3rd power */
    i__1 = cgrscl;
    ncgr = nvxtot / (i__1 * (i__1 * i__1));

/*     Make sure NCGR is within range. */

    if (ncgr < 1 || ncgr > 100000) {
	setmsg_("Coarse voxel count = #; count must be in the range 1:#.", (
		ftnlen)55);
	errint_("#", &ncgr, (ftnlen)1);
	errint_("#", &c_b168, (ftnlen)1);
	sigerr_("SPICE(VALUEOUTOFRANGE)", (ftnlen)22);
	chkout_("DSKW02", (ftnlen)6);
	return 0;
    }

/*     Start a new DLA segment. */

    dlabns_(handle);
    if (failed_()) {
	chkout_("DSKW02", (ftnlen)6);
	return 0;
    }

/*     Write the d.p. data to the segment first. In the segregated */
/*     segment, d.p. data will precede integer data, so less */
/*     rearrangement will occur this way. */


/*     First, fill in the DSK segment descriptor. */

    cleard_(&c__24, descr);
    descr[0] = (doublereal) (*surfid);
    descr[1] = (doublereal) (*center);
    descr[2] = (doublereal) (*dclass);
    descr[3] = 2.;
    descr[4] = (doublereal) frmcde;
    descr[5] = (doublereal) (*corsys);
    moved_(corpar, &c__10, &descr[6]);
    descr[16] = segbds[0];
    descr[17] = segbds[1];
    descr[18] = segbds[2];
    descr[19] = segbds[3];
    descr[20] = *mncor3;
    descr[21] = *mxcor3;
    descr[22] = *first;
    descr[23] = *last;

/*     Now write the descriptor into the segment. */

    dasadd_(handle, &c__24, descr);

/*     Add the voxel grid origin and voxel size. */
/*     Finish with the vertex data. */

    dasadd_(handle, &c__6, vtxbds);
    dasadd_(handle, &c__3, voxori);
    dasadd_(handle, &c__1, &voxsiz);
    i__1 = *nv * 3;
    dasadd_(handle, &i__1, vrtces);

/*     Next add the integer data to the segment. */

/*     NV is the number of vertices. */
/*     NP is the number of plates. */
/*     NVXTOT is the number of voxels in the spatial index. */

    dasadi_(handle, &c__1, nv);
    dasadi_(handle, &c__1, np);
    dasadi_(handle, &c__1, &nvxtot);
    dasadi_(handle, &c__3, vgrext);
    dasadi_(handle, &c__1, &cgrscl);
    dasadi_(handle, &c__1, &voxnpt);
    dasadi_(handle, &c__1, &voxnpl);
    dasadi_(handle, &c__1, &vtxnpl);
    i__1 = *np * 3;
    dasadi_(handle, &i__1, plates);
    dasadi_(handle, &voxnpt, &spaixi[pvoxpt - 1]);
    dasadi_(handle, &voxnpl, &spaixi[pvoxpl - 1]);
    dasadi_(handle, nv, &spaixi[pvtxpt - 1]);
    dasadi_(handle, &vtxnpl, &spaixi[pvtxpl - 1]);
    dasadi_(handle, &ncgr, &spaixi[7]);

/*     End the segment. */

    dlaens_(handle);
    chkout_("DSKW02", (ftnlen)6);
    return 0;
} /* dskw02_ */

