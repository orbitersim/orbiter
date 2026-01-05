/* dskmi2.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c_b14 = 16000002;
static integer c_b20 = 32000000;

/* $Procedure DSKMI2 ( DSK, make spatial index for type 2 segment ) */
/* Subroutine */ int dskmi2_(integer *nv, doublereal *vrtces, integer *np, 
	integer *plates, doublereal *finscl, integer *corscl, integer *worksz,
	 integer *voxpsz, integer *voxlsz, logical *makvtl, integer *spxisz, 
	integer *work, doublereal *spaixd, integer *spaixi)
{
    /* System generated locals */
    integer spaixi_dim1, i__1, i__2, i__3, i__4, i__5, i__6;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer);

    /* Local variables */
    extern /* Subroutine */ int zzmkspin_(integer *, integer *, doublereal *, 
	    doublereal *, integer *, integer *, integer *, integer *, integer 
	    *, integer *, doublereal *, doublereal *, integer *, integer *, 
	    integer *, integer *, integer *, doublereal *, integer *);
    integer i__, j;
    extern /* Subroutine */ int chkin_(char *, ftnlen), zzvrtplt_(integer *, 
	    integer *, integer *, integer *, integer *, integer *, integer *, 
	    integer *, integer *), errdp_(char *, doublereal *, ftnlen);
    extern logical failed_(void), return_(void);
    integer nshift, nvxtot, reqsiz, vtlidx, vtpidx, vtxlsz, vxlidx, vxpidx;
    extern /* Subroutine */ int setmsg_(char *, ftnlen), sigerr_(char *, 
	    ftnlen), chkout_(char *, ftnlen), errint_(char *, integer *, 
	    ftnlen);

/* $ Abstract */

/*     Make spatial index for a DSK type 2 segment. The index is */
/*     returned as a pair of arrays, one of type INTEGER and one of type */
/*     DOUBLE PRECISION. These arrays are suitable for use with the DSK */
/*     type 2 writer DSKW02. */

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
/*     IXDFIX     P   Size of fixed-size portion of d.p. index component. */
/*     IXIFIX     P   Size of fixed-size portion of integer index */
/*                    component. */
/*     NV         I   Number of vertices. */
/*     VRTCES     I   Vertices. */
/*     NP         I   Number of plates. */
/*     PLATES     I   Plates. */
/*     FINSCL     I   Fine voxel scale. */
/*     CORSCL     I   Coarse voxel scale. */
/*     WORKSZ     I   Workspace size. */
/*     VOXPSZ     I   Voxel-plate pointer array size. */
/*     VOXLSZ     I   Voxel-plate list array size. */
/*     MAKVTL     I   Vertex-plate list flag. */
/*     SPXISZ     I   Spatial index integer component size. */
/*     WORK      I-O  Workspace. */
/*     SPAIXD     O   Double precision component of spatial index. */
/*     SPAIXI     O   Integer component of spatial index. */

/* $ Detailed_Input */

/*     NV       is the number of vertices belonging to the input */
/*              set of plates. */

/*     VRTCES   is an array of coordinates of the vertices. The Ith */
/*              vertex occupies elements (1:3,I) of this array. */

/*     NP       is the number of plates in the input plate set. */

/*     PLATES   is an array representing the triangular plates of a */
/*              shape model. The elements of PLATES are vertex */
/*              indices; vertex indices are 1-based. The vertex */
/*              indices of the Ith plate occupy elements (1:3,I) of */
/*              this array. */

/*     FINSCL   is the fine voxel scale. This scale determines the */
/*              edge length of the cubical voxels comprising the fine */
/*              voxel grid: the edge length VOXSIZ is approximately */

/*                  FINSCL * {average plate extent} */

/*              where the extents of a plate are the respective */
/*              differences between the maximum and minimum */
/*              coordinate values of the plate's vertices. */

/*              The relationship between VOXSIZ and the average plate */
/*              extent is approximate because the VOXSIZ is adjusted */
/*              so that each dimension of the fine voxel grid is an */
/*              integer multiple of the coarse voxel scale. */

/*              See the $Particulars section below for further */
/*              information on voxel scales. */

/*     CORSCL   is the coarse voxel scale. This integer scale is the */
/*              ratio of the edge length of coarse voxels to */
/*              that of fine voxels. The coarse scale must be */
/*              large enough so that the total number of coarse */
/*              voxels does not exceed MAXCGR (see the $Parameters */
/*              section below). */

/*     WORKSZ   is the second dimension of the workspace array WORK. */
/*              WORKSZ must be at least as large as the greater of */

/*                 - the number of fine voxel-plate associations */

/*                   This number is equal to */

/*                      NP * {average number of fine voxels */
/*                            intersected by each plate} */

/*                 - the number of vertex-plate associations, if */
/*                   the vertex-plate mapping is constructed. */

/*                   This number is equal to */

/*                      NV + ( 3 * NP ) */

/*     VOXPSZ   is the size of the fine voxel-plate pointer array. */
/*              This array maps fine voxels to lists of plates that */
/*              intersect those voxels. VOXPSZ must be at least as */
/*              large as */

/*                       3 */
/*                 CORSCL  * {number of non-empty coarse voxels} */

/*     VOXLSZ   is the size of the fine voxel-plate list array. This */
/*              array contains, for each non-empty fine voxel, the */
/*              count of plates that intersect that voxel and the */
/*              IDs of those plates. VOXLSZ must be at least as large */
/*              as */

/*                      NP * {average number of fine voxels */
/*                            intersected by each plate} */

/*                  +   {number of non-empty fine voxels} */

/*     MAKVTL   is a logical flag that, when set to .TRUE., indicates */
/*              that a  vertex-plate association list is to be */
/*              constructed. */

/*              The amount of workspace that is needed may depend on */
/*              whether a vertex-plate association list is */
/*              constructed. When this list is constructed, the size */
/*              of the integer component of the spatial index is */
/*              increased by the size of the list and the size of a */
/*              vertex-plate pointer array; the total of these sizes */
/*              is */

/*                 ( 2 * NV ) + ( 3 * NP ) */

/*     SPXISZ   is the declared size of the output array SPAIXI. This */
/*              size must be at least as large as the sum of */

/*                 - the fixed-size part of the integer component of */
/*                   the index, which includes the coarse voxel grid; */
/*                   this value is */

/*                      IXIFIX */

/*                 - the size VOXPSZ of the voxel-plate pointer array */

/*                 - the size VOXLSZ of the voxel-plate association */
/*                   list */

/*              plus, if the vertex-plate association list is */
/*              constructed, */

/*                 - the size NV of the vertex-plate pointer array */

/*                 - the size of the vertex-plate association list; */
/*                   this size is */

/*                      NV + ( 3 * NP ) */

/*     WORK     is the workspace array. The array should be declared */
/*              with dimensions */

/*                 (2, WORKSZ) */

/*              See the description of WORKSZ above. */

/* $ Detailed_Output */

/*     WORK     is the workspace array, modified by the operations */
/*              performed by this routine. */

/*     SPAIXD, */
/*     SPAIXI   are, respectively, the double precision and integer */
/*              components of the spatial index of the segment. */

/*              SPAIXD must be declared with size at least IXDFIX. */
/*              SPAIXI must be declared with size at least SPXISZ. */

/* $ Parameters */

/*     IXDFIX   is the size of the double precision component of */
/*              the spatial index. */

/*     IXIFIX   is the size of the fixed-size portion of the integer */
/*              component of the spatial index. */

/*     See the include file */

/*        dsk02.inc */

/*     for declarations of DSK data type 2 (plate model) parameters. */

/*     See the include file */

/*        dla.inc */

/*     for declarations of DLA descriptor sizes and documentation of the */
/*     contents of DLA descriptors. */

/*     See the include file */

/*        dskdsc.inc */

/*     for declarations of DSK descriptor sizes and documentation of the */
/*     contents of DSK descriptors. */

/* $ Exceptions */

/*     1)  If the fine voxel scale is non-positive, the error */
/*         SPICE(BADFINEVOXELSCALE) is signaled. */

/*     2)  If the coarse voxel scale is less than 1, the error */
/*         SPICE(BADCOARSEVOXSCALE) is signaled. */

/*     3)  If NV is less than 3 or greater than MAXVRT, the error */
/*         SPICE(BADVERTEXCOUNT) is signaled. */

/*     4)  If NP is less than 1 or greater than MAXPLT, the error */
/*         SPICE(BADPLATECOUNT) is signaled. */

/*     5)  If the workspace size WORKSZ is less than NP+1, the error */
/*         SPICE(WORKSPACETOOSMALL) is signaled. This is merely a */
/*         sanity check; normally the workspace will need to be */
/*         substantially larger than this reference value. See the */
/*         description of WORKSZ in the header section $Detailed_Input */
/*         above. */

/*     6)  If the voxel-plate pointer array size VOXPSZ is less than 1, */
/*         the error SPICE(PTRARRAYTOOSMALL) is signaled. This is merely */
/*         a sanity check; normally this pointer array will need to be */
/*         substantially larger than this reference value. See the */
/*         description of VOXPSZ in the header section $Detailed_Input */
/*         above. */

/*     7)  If the voxel-plate list array size VOXLSZ is less than NP+1, */
/*         the error SPICE(PLATELISTTOOSMALL) is signaled. This is */
/*         merely a sanity check; normally this array will need to be */
/*         substantially larger than this reference value. See the */
/*         description of VOXLSZ in the header section $Detailed_Input */
/*         above. */

/*     8)  If the size SPXISZ of the integer array SPAIXI is too small */
/*         to contain its constituent structures, where the sizes */
/*         of these structures are derived from the inputs */

/*             NV, NP, VOXPSZ, VOXLSZ */

/*         the error SPICE(INTINDEXTOOSMALL) is signaled. */

/*     9)  If there is insufficient room to create any of the data */
/*         structures contained in the spatial index, an error is */
/*         signaled by a routine in the call tree of this routine. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     Users planning to create DSK files should consider whether the */
/*     SPICE DSK creation utility MKDSK may be suitable for their needs. */

/*     This routine supports use of the DSK type 2 segment writer DSKW02 */
/*     by creating the "spatial index" arrays required as inputs to that */
/*     routine. */

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
/*              PROGRAM DSKMI2_EX1 */
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
/*     B.V. Semenov       (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.1, 03-JUN-2021 (JDR) (BVS) */

/*        Edited the header to comply with NAIF standard. Fixed I/O type */
/*        of arguments WORK, SPAIXD and SPAIXI in $Brief_I/O table. */

/*        Added solution to code example. */

/* -    SPICELIB Version 1.0.0, 13-DEC-2016 (NJB) */

/*        Updated check on NV. */

/*        16-MAR-2016 (NJB) */

/*           Now zeros out the size of the vertex-plate list */
/*           when the list is not created. */

/*        23-JAN-2016 (NJB) */

/*           Original version. */

/* -& */
/* $ Index_Entries */

/*     make spatial index for type 2 DSK segment */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     Local variables */

    /* Parameter adjustments */
    spaixi_dim1 = *spxisz;

    /* Function Body */
    if (return_()) {
	return 0;
    }
    chkin_("DSKMI2", (ftnlen)6);

/*     Perform error checks on inputs. */

    if (*finscl <= 0.) {
	setmsg_("Fine voxel scale = #; scale must be positive. Usually scale"
		" should be > 1.0.", (ftnlen)76);
	errdp_("#", finscl, (ftnlen)1);
	sigerr_("SPICE(BADFINEVOXELSCALE)", (ftnlen)24);
	chkout_("DSKMI2", (ftnlen)6);
	return 0;
    }
    if (*corscl < 1) {
	setmsg_("Coarse voxel scale = #; scale must be >= 1.", (ftnlen)43);
	errint_("#", corscl, (ftnlen)1);
	sigerr_("SPICE(BADCOARSEVOXSCALE)", (ftnlen)24);
	chkout_("DSKMI2", (ftnlen)6);
	return 0;
    }
    if (*nv < 3 || *nv > 16000002) {
	setmsg_("Vertex count NV = #; count must be in the range 3:#.", (
		ftnlen)52);
	errint_("#", nv, (ftnlen)1);
	errint_("#", &c_b14, (ftnlen)1);
	sigerr_("SPICE(BADVERTEXCOUNT)", (ftnlen)21);
	chkout_("DSKMI2", (ftnlen)6);
	return 0;
    }
    if (*np < 1 || *np > 32000000) {
	setmsg_("Plate count NP = #; count must be in the range 1:#.", (
		ftnlen)51);
	errint_("#", np, (ftnlen)1);
	errint_("#", &c_b20, (ftnlen)1);
	sigerr_("SPICE(BADPLATECOUNT)", (ftnlen)20);
	chkout_("DSKMI2", (ftnlen)6);
	return 0;
    }
    if (*worksz < *np + 1) {
	setmsg_("Workspace size = #; size is too small to hold all voxel-pla"
		"te associations. Size should be at least # * (average number"
		" of voxels intersected by each plate).", (ftnlen)157);
	errint_("#", worksz, (ftnlen)1);
	errint_("#", np, (ftnlen)1);
	sigerr_("SPICE(WORKSPACETOOSMALL)", (ftnlen)24);
	chkout_("DSKMI2", (ftnlen)6);
	return 0;
    }
    if (*voxpsz < 1) {
	setmsg_("Voxel-pointer array size = #; size is too small to hold all"
		" voxel-plate list pointers. Size should be at least # * (num"
		"ber of non-empty coarse voxels).", (ftnlen)151);
	errint_("#", voxpsz, (ftnlen)1);
/* Computing 3rd power */
	i__2 = *corscl;
	i__1 = i__2 * (i__2 * i__2);
	errint_("#", &i__1, (ftnlen)1);
	sigerr_("SPICE(PTRARRAYTOOSMALL)", (ftnlen)23);
	chkout_("DSKMI2", (ftnlen)6);
	return 0;
    }
    if (*voxlsz < *np + 1) {
	setmsg_("Voxel-plate list array size = #; size is too small to hold "
		"all voxel-plate associations. Size should be at least # * (a"
		"verage number of voxels intersected by each plate).", (ftnlen)
		170);
	errint_("#", voxlsz, (ftnlen)1);
	errint_("#", np, (ftnlen)1);
	sigerr_("SPICE(PLATELISTTOOSMALL)", (ftnlen)24);
	chkout_("DSKMI2", (ftnlen)6);
	return 0;
    }

/*     Check the size of the integer spatial index array. The */
/*     declared size must be large enough to hold: */

/*        - the fixed-size part of the index, which includes */
/*          the coarse voxel grid */

/*        - the voxel-plate pointer array */

/*        - the voxel-plate association list */

/*     plus, if the vertex-plate association list is constructed, */

/*        - the vertex-plate pointer array */

/*        - the vertex-plate association list */


    reqsiz = *voxpsz + 100007 + *voxlsz;
    if (*makvtl) {

/*        Add on the sizes of the vertex-plate pointer array (NV) */
/*        and the vertex-plate list array (NV + 3*NP). */

	vtxlsz = *nv + *np * 3;
	reqsiz = reqsiz + *nv + vtxlsz;
    } else {
	vtxlsz = 0;
    }
    if (*spxisz < reqsiz) {
	setmsg_("Integer spatial index size = #; size must be at least #.", (
		ftnlen)56);
	errint_("#", spxisz, (ftnlen)1);
	errint_("#", &reqsiz, (ftnlen)1);
	sigerr_("SPICE(INTINDEXTOOSMALL)", (ftnlen)23);
	chkout_("DSKMI2", (ftnlen)6);
	return 0;
    }

/*     Set known values in spatial index arrays. */

    spaixi[(i__1 = 3) < spaixi_dim1 ? i__1 : s_rnge("spaixi", i__1, "dskmi2_",
	     (ftnlen)949)] = *corscl;

/*     Prepare indices in the spatial index arrays. */

/*        VXPIDX is the start index of the voxel pointer array. */

    vxpidx = 100008;

/*        VXLIDX is the start index of the voxel-plate list. This */
/*        list is offset from the start of the pointer array by */
/*        the input size given for that array. The size is the */
/*        total room available, not the room actually used. */

    vxlidx = vxpidx + *voxpsz;

/*     Create spatial index for plates. */

    zzmkspin_(np, plates, vrtces, finscl, corscl, voxpsz, worksz, voxlsz, 
	    work, &spaixi[(i__1 = 0) < spaixi_dim1 ? i__1 : s_rnge("spaixi", 
	    i__1, "dskmi2_", (ftnlen)968)], &spaixd[9], &spaixd[6], &nvxtot, &
	    spaixi[(i__2 = 4) < spaixi_dim1 ? i__2 : s_rnge("spaixi", i__2, 
	    "dskmi2_", (ftnlen)968)], &spaixi[(i__3 = vxpidx - 1) < 
	    spaixi_dim1 && 0 <= i__3 ? i__3 : s_rnge("spaixi", i__3, "dskmi2_"
	    , (ftnlen)968)], &spaixi[(i__4 = 5) < spaixi_dim1 ? i__4 : s_rnge(
	    "spaixi", i__4, "dskmi2_", (ftnlen)968)], &spaixi[(i__5 = vxlidx 
	    - 1) < spaixi_dim1 && 0 <= i__5 ? i__5 : s_rnge("spaixi", i__5, 
	    "dskmi2_", (ftnlen)968)], spaixd, &spaixi[(i__6 = 7) < 
	    spaixi_dim1 ? i__6 : s_rnge("spaixi", i__6, "dskmi2_", (ftnlen)
	    968)]);
    if (failed_()) {
	chkout_("DSKMI2", (ftnlen)6);
	return 0;
    }

/*     At this point the voxel plate list is offset from the */
/*     start of the voxel pointer array by the allocated size */
/*     of the array. We need to shift the plate list so that */
/*     it starts right after the end of the pointer array. */

    nshift = *voxpsz - spaixi[(i__1 = 4) < spaixi_dim1 ? i__1 : s_rnge("spai"
	    "xi", i__1, "dskmi2_", (ftnlen)993)];
    i__2 = spaixi[(i__1 = 5) < spaixi_dim1 ? i__1 : s_rnge("spaixi", i__1, 
	    "dskmi2_", (ftnlen)995)];
    for (i__ = 1; i__ <= i__2; ++i__) {
	j = vxlidx - 1 + i__;
	spaixi[(i__1 = j - nshift - 1) < spaixi_dim1 && 0 <= i__1 ? i__1 : 
		s_rnge("spaixi", i__1, "dskmi2_", (ftnlen)999)] = spaixi[(
		i__3 = j - 1) < spaixi_dim1 && 0 <= i__3 ? i__3 : s_rnge(
		"spaixi", i__3, "dskmi2_", (ftnlen)999)];
    }

/*     Update the voxel list start index to reflect the shift. */

    vxlidx -= nshift;

/*     Create vertex-plate mapping, if requested, as indicated */
/*     by the vertex-plate list size. */

    if (*makvtl) {

/*           VTPIDX is the start index of the vertex pointer array. */

	vtpidx = vxlidx + spaixi[(i__2 = 5) < spaixi_dim1 ? i__2 : s_rnge(
		"spaixi", i__2, "dskmi2_", (ftnlen)1016)];

/*           VXLIDX is the start index of the vertex-plate list. The */
/*           list start is offset from the vertex pointer array by */
/*           the size of the array, which is always NV. */

	vtlidx = vtpidx + *nv;
	zzvrtplt_(nv, np, plates, worksz, &vtxlsz, work, &spaixi[(i__2 = 
		vtpidx - 1) < spaixi_dim1 && 0 <= i__2 ? i__2 : s_rnge("spai"
		"xi", i__2, "dskmi2_", (ftnlen)1024)], &spaixi[(i__1 = 6) < 
		spaixi_dim1 ? i__1 : s_rnge("spaixi", i__1, "dskmi2_", (
		ftnlen)1024)], &spaixi[(i__3 = vtlidx - 1) < spaixi_dim1 && 0 
		<= i__3 ? i__3 : s_rnge("spaixi", i__3, "dskmi2_", (ftnlen)
		1024)]);
    } else {

/*        Zero out the size of the vertex-plate list. */

	spaixi[(i__2 = 6) < spaixi_dim1 ? i__2 : s_rnge("spaixi", i__2, "dsk"
		"mi2_", (ftnlen)1034)] = 0;
    }
    chkout_("DSKMI2", (ftnlen)6);
    return 0;
} /* dskmi2_ */

