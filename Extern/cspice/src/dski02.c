/* dski02.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure DSKI02 ( DSK, fetch integer type 2 data ) */
/* Subroutine */ int dski02_(integer *handle, integer *dladsc, integer *item, 
	integer *start, integer *room, integer *n, integer *values)
{
    /* Initialized data */

    static logical first = TRUE_;

    /* System generated locals */
    integer i__1, i__2;

    /* Local variables */
    integer ncgr, size, b, e, ibase, ibuff[10];
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    extern logical failed_(void);
    static integer cgscal, np, nv;
    extern /* Subroutine */ int dasrdi_(integer *, integer *, integer *, 
	    integer *);
    static integer nvxtot, prvbas, prvhan, voxnpl, voxnpt, vtxnpl;
    extern /* Subroutine */ int setmsg_(char *, ftnlen), errint_(char *, 
	    integer *, ftnlen), sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen);

/* $ Abstract */

/*     Fetch integer data from a type 2 DSK segment. */

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

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     HANDLE     I   DSK file handle. */
/*     DLADSC     I   DLA descriptor. */
/*     ITEM       I   Keyword identifying item to fetch. */
/*     START      I   Start index. */
/*     ROOM       I   Amount of room in output array. */
/*     N          O   Number of values returned. */
/*     VALUES     O   Array containing requested item. */

/* $ Detailed_Input */

/*     HANDLE   is the handle of a DSK file containing a type 2 */
/*              segment from which data are to be fetched. */

/*     DLADSC   is the DLA descriptor associated with the segment */
/*              from which data are to be fetched. */

/*     ITEM     is an integer "keyword" parameter designating the */
/*              item to fetch. In the descriptions below, note */
/*              that "model" refers to the model represented by */
/*              the designated segment. This model may be a */
/*              subset of a larger model. */

/*              Names and meanings of parameters supported by this */
/*              routine are: */

/*                 KWNV       Number of vertices in model. */

/*                 KWNP       Number of plates in model. */

/*                 KWNVXT     Total number of voxels in fine grid. */

/*                 KWVGRX     Voxel grid extent. This extent is */
/*                            an array of three integers */
/*                            indicating the number of voxels in */
/*                            the X, Y, and Z directions in the */
/*                            fine voxel grid. */

/*                 KWCGSC     Coarse voxel grid scale. The extent */
/*                            of the fine voxel grid is related to */
/*                            the extent of the coarse voxel grid */
/*                            by this scale factor. */

/*                 KWVXPS     Size of the voxel-to-plate pointer */
/*                            list. */

/*                 KWVXLS     Voxel-plate correspondence list size. */

/*                 KWVTLS     Vertex-plate correspondence list */
/*                            size. */

/*                 KWPLAT     Plate array. For each plate, this */
/*                            array contains the indices of the */
/*                            plate's three vertices. The ordering */
/*                            of the array members is: */

/*                               Plate 1 vertex index 1 */
/*                               Plate 1 vertex index 2 */
/*                               Plate 1 vertex index 3 */
/*                               Plate 2 vertex index 1 */
/*                                       ... */

/*                 KWVXPT     Voxel-plate pointer list. This list */
/*                            contains pointers that map fine */
/*                            voxels to lists of plates that */
/*                            intersect those voxels. Note that */
/*                            only fine voxels belonging to */
/*                            non-empty coarse voxels are in the */
/*                            domain of this mapping. */

/*                 KWVXPL     Voxel-plate correspondence list. */
/*                            This list contains lists of plates */
/*                            that intersect fine voxels. (This */
/*                            list is the data structure into */
/*                            which the voxel-to-plate pointers */
/*                            point.) This list can contain */
/*                            empty lists. */

/*                 KWVTPT     Vertex-plate pointer list. This list */
/*                            contains pointers that map vertices */
/*                            to lists of plates to which those */
/*                            vertices belong. */

/*                            Note that the size of this list is */
/*                            always NV, the number of vertices. */
/*                            Hence there's no need for a separate */
/*                            keyword for the size of this list. */

/*                 KWVTPL     Vertex-plate correspondence list. */
/*                            This list contains, for each vertex, */
/*                            the indices of the plates to which */
/*                            that vertex belongs. */

/*                 KWCGPT     Coarse voxel grid pointers. This is */
/*                            an array of pointers mapping coarse */
/*                            voxels to lists of pointers in the */
/*                            voxel-plate pointer list. Each */
/*                            non-empty coarse voxel maps to a */
/*                            list of pointers; every fine voxel */
/*                            contained in a non-empty coarse voxel */
/*                            has its own pointers. Grid elements */
/*                            corresponding to empty coarse voxels */
/*                            have null (non-positive) pointers. */

/*              See the INCLUDE file dsk.inc for values */
/*              associated with the keyword parameters. */


/*     START    is the start index within the specified data item */
/*              from which data are to be fetched. The index of */
/*              the first element of each data item is 1. START */
/*              has units of integers; for example, the start */
/*              index of the second plate is 4, since each plate */
/*              occupies three integers. */

/*     ROOM     is the amount of room in the output array. It is */
/*              permissible to provide an output array that has */
/*              too little room to fetch an item in one call. ROOM */
/*              has units of integers: for example, the room */
/*              required to fetch one plate is 3. */

/* $ Detailed_Output */

/*     N        is the number of elements fetched to the output */
/*              array VALUES. N is normally in the range */
/*              1:ROOM; if an error occurs on the call, N is */
/*              undefined. */

/*     VALUES   is a contiguous set of elements of the item */
/*              designated by ITEM. The correspondence of */
/*              VALUES with the elements of the data item is: */

/*                 VALUES(1)      ITEM(START) */
/*                   ...             ... */
/*                 VALUES(N)      ITEM(START+N-1) */

/*              If an error occurs on the call, VALUES is */
/*              undefined. */

/* $ Parameters */

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

/*     4)  If ROOM is non-positive, the error SPICE(VALUEOUTOFRANGE) */
/*         is signaled. */

/*     5)  If the coarse voxel scale read from the designated segment */
/*         is less than 1, the error SPICE(VALUEOUTOFRANGE) is signaled. */

/*     6)  If the input keyword parameter is not recognized, the error */
/*         SPICE(NOTSUPPORTED) is signaled. */

/*     7)  If START is less than 1 or greater than the size of the */
/*         item to be fetched, the error SPICE(INDEXOUTOFRANGE) is */
/*         signaled. */

/* $ Files */

/*     See input argument HANDLE. */

/* $ Particulars */

/*     Most SPICE applications will not need to call this routine. The */
/*     routines DSKV02, DSKP02, and DSKZ02 provide a higher-level */
/*     interface for fetching DSK type 2 vertex and plate data. */

/*     DSK files are built using the DLA low-level format and */
/*     the DAS architecture; DLA files are a specialized type of DAS */
/*     file in which data are organized as a doubly linked list of */
/*     segments. Each segment's data belong to contiguous components of */
/*     character, double precision, and integer type. */

/*     Note that the DSK descriptor for the segment is not needed by */
/*     this routine; the DLA descriptor contains the base address and */
/*     size information for the integer, double precision, and character */
/*     components of the segment, and these suffice for the purpose of */
/*     fetching data. */

/* $ Examples */

/*     The numerical results shown for this example may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */

/*     1) Look up all the vertices associated with each plate */
/*        of the model contained in a specified type 2 segment. */
/*        For this example, we'll show the context of this look-up: */
/*        opening the DSK file for read access, traversing a trivial, */
/*        one-segment list to obtain the segment of interest. */


/*        Example code begins here. */


/*              PROGRAM DSKI02_EX1 */
/*              IMPLICIT NONE */

/*              INCLUDE 'dla.inc' */
/*              INCLUDE 'dskdsc.inc' */
/*              INCLUDE 'dsk02.inc' */

/*        C */
/*        C     Local parameters */
/*        C */
/*              CHARACTER*(*)         FMT */
/*              PARAMETER           ( FMT    = '(1X,A,3(1XE15.8))' ) */

/*              INTEGER               FILSIZ */
/*              PARAMETER           ( FILSIZ = 255 ) */

/*        C */
/*        C     Local variables */
/*        C */
/*              CHARACTER*(FILSIZ)    DSK */

/*              DOUBLE PRECISION      VRTCES ( 3, 3 ) */

/*              INTEGER               DLADSC ( DLADSZ ) */
/*              INTEGER               HANDLE */
/*              INTEGER               I */
/*              INTEGER               J */
/*              INTEGER               K */
/*              INTEGER               N */
/*              INTEGER               NP */
/*              INTEGER               START */
/*              INTEGER               VRTIDS ( 3 ) */

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
/*        C     Find the number of plates in the model. */
/*        C */
/*              CALL DSKI02 ( HANDLE, DLADSC, KWNP, 1, 1, N, NP ) */
/*              WRITE (*,*) 'Number of plates: ', NP */

/*        C */
/*        C     For the first 5 plates, look up the desired data. */
/*        C */
/*              K = MIN(5, NP) */
/*              DO I = 1, K */
/*        C */
/*        C        For the Ith plate, find the associated */
/*        C        vertex IDs.  We must take into account */
/*        C        the fact that each plate has three */
/*        C        vertices when we compute the start */
/*        C        index. */
/*        C */
/*                 START = 3*(I-1)+1 */

/*                 CALL DSKI02 ( HANDLE, DLADSC, KWPLAT, START, */
/*             .                 3,      N,      VRTIDS        ) */

/*                 DO J = 1, 3 */
/*        C */
/*        C            Fetch the vertex associated with */
/*        C            the Jth vertex ID.  Again, each */
/*        C            vertex is a 3-vector.  Note that */
/*        C            the vertices are double-precision */
/*        C            data, so we fetch them using */
/*        C            DSKD02. */
/*        C */
/*                     START = 3*( VRTIDS(J) - 1 ) + 1 */

/*                     CALL DSKD02 ( HANDLE, DLADSC, KWVERT,  START, */
/*             .                     3,      N,      VRTCES(1,J)    ) */
/*                 END DO */

/*        C */
/*        C        Display the vertices of the Ith plate: */
/*        C */
/*                 WRITE (*,*)   ' ' */
/*                 WRITE (*,*)   'Plate number: ', I */
/*                 WRITE (*,FMT) '   Vertex 1: ', (VRTCES(J,1), J=1,3) */
/*                 WRITE (*,FMT) '   Vertex 2: ', (VRTCES(J,2), J=1,3) */
/*                 WRITE (*,FMT) '   Vertex 3: ', (VRTCES(J,3), J=1,3) */

/*              END DO */

/*        C */
/*        C     Close the kernel.  This isn't necessary in a stand- */
/*        C     alone program, but it's good practice in subroutines */
/*        C     because it frees program and system resources. */
/*        C */
/*              CALL DASCLS ( HANDLE ) */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, using the DSK file named phobos512.bds, the output */
/*        was: */


/*        Enter DSK name > phobos512.bds */
/*         Number of plates:      3145728 */

/*         Plate number:            1 */
/*            Vertex 1:  -0.67744400E+01  0.62681500E+01  0.60114900E+01 */
/*            Vertex 2:  -0.67623800E+01  0.62572800E+01  0.60255600E+01 */
/*            Vertex 3:  -0.67571000E+01  0.62775400E+01  0.60209600E+01 */

/*         Plate number:            2 */
/*            Vertex 1:  -0.67744400E+01  0.62681500E+01  0.60114900E+01 */
/*            Vertex 2:  -0.67797300E+01  0.62479000E+01  0.60161000E+01 */
/*            Vertex 3:  -0.67623800E+01  0.62572800E+01  0.60255600E+01 */

/*         Plate number:            3 */
/*            Vertex 1:  -0.67797300E+01  0.62479000E+01  0.60161000E+01 */
/*            Vertex 2:  -0.67676800E+01  0.62370100E+01  0.60301900E+01 */
/*            Vertex 3:  -0.67623800E+01  0.62572800E+01  0.60255600E+01 */

/*         Plate number:            4 */
/*            Vertex 1:  -0.67797300E+01  0.62479000E+01  0.60161000E+01 */
/*            Vertex 2:  -0.67849900E+01  0.62276200E+01  0.60207000E+01 */
/*            Vertex 3:  -0.67676800E+01  0.62370100E+01  0.60301900E+01 */

/*         Plate number:            5 */
/*            Vertex 1:  -0.67849900E+01  0.62276200E+01  0.60207000E+01 */
/*            Vertex 2:  -0.67729900E+01  0.62167400E+01  0.60348200E+01 */
/*            Vertex 3:  -0.67676800E+01  0.62370100E+01  0.60301900E+01 */


/*        Note that only the vertex information for first 5 plates is */
/*        provided. */

/* $ Restrictions */

/*     1)  This routine uses discovery check-in to boost */
/*         execution speed. However, this routine is in */
/*         violation of NAIF standards for use of discovery */
/*         check-in:  routines called from this routine may */
/*         signal errors. If errors are signaled in called */
/*         routines, this routine's name will be missing */
/*         from the traceback message. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     B.V. Semenov       (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.1, 02-JUL-2021 (JDR) (BVS) */

/*        Edited the header to comply with NAIF standard. Extended the */
/*        $Keywords section. Modified code example to reduce the output. */

/* -    SPICELIB Version 1.0.0, 22-NOV-2016 (NJB) */

/*        Added FAILED check after segment attribute fetch calls. */
/*        Re-ordered code so that values are saved only after */
/*        all error checks have passed. Simplified base address */
/*        comparisons. */

/*        15-JAN-2016 (NJB) */

/*           Updated header $Examples and $Particulars sections. */

/*        DSKLIB Version 1.0.2, 11-JUL-2014 (NJB) */

/*           Fixed a trivial header comment typo. */

/*        DSKLIB Version 1.0.1, 13-MAY-2010 (NJB) */

/*           Updated header. */

/*        DSKLIB Version 1.0.0, 27-OCT-2006 (NJB) */

/* -& */
/* $ Index_Entries */

/*     fetch integer data from a type 2 DSK segment */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     IBFSIZ is the size of an integer buffer used to */
/*     read parameters from the segment. */


/*     Local variables */


/*     Saved variables */


/*     Initial values */


/*     Use discovery check-in.  This is done for efficiency; note */
/*     however that this routine does not meet SPICE standards for */
/*     discovery check-in eligibility. */

    if (first) {

/*        Make sure we treat the input handle as new on the first pass. */
/*        Set PRVHAN to an invalid handle value. */

	prvhan = 0;

/*        Set the previous segment base integer address to an invalid */
/*        value as well. */

	prvbas = -1;
	first = FALSE_;
    }
    if (*room <= 0) {
	chkin_("DSKI02", (ftnlen)6);
	setmsg_("ROOM was #; must be positive.", (ftnlen)29);
	errint_("#", room, (ftnlen)1);
	sigerr_("SPICE(VALUEOUTOFRANGE)", (ftnlen)22);
	chkout_("DSKI02", (ftnlen)6);
	return 0;
    }
    ibase = dladsc[2];

/*     Either a new file or new segment in the same file will require */
/*     looking up the segment parameters. To determine whether the */
/*     segment is new, we don't need to compare the entire DLA */
/*     descriptor: just comparing the integer base address of the */
/*     descriptor against the saved integer base address is sufficient. */

/*     DSK type 2 segments always have a non-empty integer component, so */
/*     each type 2 segment in a given file will have a distinct integer */
/*     base address. Segments of other types might not contain integers, */
/*     but they can't share an integer base address with a type 2 */
/*     segment. */

    if (*handle != prvhan || ibase != prvbas) {

/*        Treat the input file and segment as new. */

/*        Read the integer parameters first.  These are located at the */
/*        beginning of the integer component of the segment. */

	i__1 = ibase + 1;
	i__2 = ibase + 10;
	dasrdi_(handle, &i__1, &i__2, ibuff);
	if (failed_()) {
	    return 0;
	}

/*        Check the coarse voxel scale. */

	if (ibuff[6] < 1) {
	    chkin_("DSKI02", (ftnlen)6);
	    setmsg_("Coarse voxel grid scale is #; this scale should be an i"
		    "nteger > 1", (ftnlen)65);
	    errint_("#", &cgscal, (ftnlen)1);
	    sigerr_("SPICE(VALUEOUTOFRANGE)", (ftnlen)22);
	    chkout_("DSKI02", (ftnlen)6);
	    return 0;
	}

/*        All checks have passed. We can safely store the segment */
/*        parameters. */

	nv = ibuff[0];
	np = ibuff[1];
	nvxtot = ibuff[2];
	cgscal = ibuff[6];
	vtxnpl = ibuff[9];
	voxnpt = ibuff[7];
	voxnpl = ibuff[8];

/*        Update the saved handle value. */

	prvhan = *handle;

/*        Update the saved base integer address. */

	prvbas = ibase;
    }

/*     Branch based on the item to be returned. */

/*     Note that we haven't checked the validity of START; we'll do this */
/*     after the IF block. */

    if (*item == 1) {

/*        Return the number of vertices. */

	*n = 1;
	values[0] = nv;

/*        As long as START is valid, we can return. Otherwise, */
/*        let control pass to the error handling block near */
/*        the end of this routine. */

	if (*start == 1) {
	    return 0;
	}
    } else if (*item == 2) {

/*        Return the number of plates. */

	*n = 1;
	values[0] = np;
	if (*start == 1) {
	    return 0;
	}
    } else if (*item == 3) {

/*        Return the total number of voxels. */

	*n = 1;
	values[0] = nvxtot;
	if (*start == 1) {
	    return 0;
	}
    } else if (*item == 4) {

/*        Return the voxel grid extents. */

	size = 3;
	b = ibase + 4 + *start - 1;
    } else if (*item == 5) {

/*        Return the coarse voxel grid scale. */

	*n = 1;
	values[0] = cgscal;
	if (*start == 1) {
	    return 0;
	}
    } else if (*item == 6) {

/*        Return the voxel-plate pointer list size. */

	*n = 1;
	values[0] = voxnpt;
	if (*start == 1) {
	    return 0;
	}
    } else if (*item == 7) {

/*        Return the voxel-plate list size. */

	*n = 1;
	values[0] = voxnpl;
	if (*start == 1) {
	    return 0;
	}
    } else if (*item == 8) {

/*        Return the vertex-plate list size. */

	*n = 1;
	values[0] = vtxnpl;
	if (*start == 1) {
	    return 0;
	}
    } else if (*item == 9) {

/*        Return plate data.  There are 3*NP values in all.  First */
/*        locate the data. */

	size = np * 3;
	b = ibase + 11 + *start - 1;
    } else if (*item == 10) {

/*        Return voxel pointer data.  There are VOXNPT values in all. */
/*        First locate the data. */

	size = voxnpt;
	b = ibase + 11 + np * 3 + *start - 1;
    } else if (*item == 11) {

/*        Return voxel-plate list data.  There are VOXNPL values in all. */
/*        First locate the data. */

	size = voxnpl;
	b = ibase + 11 + np * 3 + voxnpt + *start - 1;
    } else if (*item == 12) {

/*        Return vertex-plate pointer data.  There are NV values in all. */
/*        First locate the data. */

	size = nv;
	b = ibase + 11 + np * 3 + voxnpt + voxnpl + *start - 1;
    } else if (*item == 13) {

/*        Return vertex-plate list data.  There are VTXNPL values in */
/*        all. First locate the data. */

	size = vtxnpl;
	b = ibase + 11 + np * 3 + voxnpt + voxnpl + nv + *start - 1;
    } else if (*item == 14) {

/*        Compute the coarse grid size. */

/* Computing 3rd power */
	i__1 = cgscal;
	ncgr = nvxtot / (i__1 * (i__1 * i__1));

/*        Return the coarse voxel grid occupancy pointers.  There are */

/*           NCGR */

/*        values in all. First locate the data. */

	size = ncgr;
	b = ibase + 11 + np * 3 + voxnpt + voxnpl + nv + vtxnpl + *start - 1;
    } else {
	chkin_("DSKI02", (ftnlen)6);
	setmsg_("Keyword parameter # was not recognized.", (ftnlen)39);
	errint_("#", item, (ftnlen)1);
	sigerr_("SPICE(NOTSUPPORTED)", (ftnlen)19);
	chkout_("DSKI02", (ftnlen)6);
	return 0;
    }

/*     The valid range for START is 1:SIZE. */

    if (*start < 1 || *start > size) {
	chkin_("DSKI02", (ftnlen)6);
	setmsg_("START must be in the range defined by the size of the data "
		"associated with the keyword parameter #, namely 1:#.  Actual"
		" value of START was #.", (ftnlen)141);
	errint_("#", item, (ftnlen)1);
	errint_("#", &size, (ftnlen)1);
	errint_("#", start, (ftnlen)1);
	sigerr_("SPICE(INDEXOUTOFRANGE)", (ftnlen)22);
	chkout_("DSKI02", (ftnlen)6);
	return 0;
    }

/*     Read the requested data.  We already have the start address B. */

/* Computing MIN */
    i__1 = *room, i__2 = size - *start + 1;
    *n = min(i__1,i__2);
    e = b + *n - 1;
    dasrdi_(handle, &b, &e, values);
    return 0;
} /* dski02_ */

