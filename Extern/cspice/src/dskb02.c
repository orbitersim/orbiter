/* dskb02.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__3 = 3;
static integer c__6 = 6;

/* $Procedure DSKB02 ( DSK, fetch type 2 bookkeeping data ) */
/* Subroutine */ int dskb02_(integer *handle, integer *dladsc, integer *nv, 
	integer *np, integer *nvxtot, doublereal *vtxbds, doublereal *voxsiz, 
	doublereal *voxori, integer *vgrext, integer *cgscal, integer *vtxnpl,
	 integer *voxnpt, integer *voxnpl)
{
    /* System generated locals */
    integer i__1, i__2;

    /* Local variables */
    extern /* Subroutine */ int vequ_(doublereal *, doublereal *);
    integer b, e, ibase;
    doublereal dbuff[10];
    integer ibuff[10];
    extern /* Subroutine */ int chkin_(char *, ftnlen), moved_(doublereal *, 
	    integer *, doublereal *), movei_(integer *, integer *, integer *);
    integer dpbase;
    extern /* Subroutine */ int dasrdd_(integer *, integer *, integer *, 
	    doublereal *), dasrdi_(integer *, integer *, integer *, integer *)
	    ;
    extern logical return_(void);
    extern /* Subroutine */ int chkout_(char *, ftnlen);

/* $ Abstract */

/*     Return bookkeeping data from a DSK type 2 segment. */

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
/*     NV         O   Number of vertices in model. */
/*     NP         O   Number of plates in model. */
/*     NVXTOT     O   Number of voxels in fine grid. */
/*     VTXBDS     O   Vertex bounds. */
/*     VOXSIZ     O   Fine voxel edge length. */
/*     VOXORI     O   Fine voxel grid origin. */
/*     VGREXT     O   Fine voxel grid extent. */
/*     CGSCAL     O   Coarse voxel grid scale. */
/*     VTXNPL     O   Size of vertex-plate correspondence list. */
/*     VOXNPT     O   Size of voxel-plate pointer list. */
/*     VOXNPL     O   Size of voxel-plate correspondence list. */

/* $ Detailed_Input */

/*     HANDLE   is the handle of a DSK file containing a type 2 */
/*              segment from which data are to be fetched. */

/*     DLADSC   is the DLA descriptor associated with the segment */
/*              from which data are to be fetched. */

/* $ Detailed_Output */

/*     NV       is the number of vertices in model. */

/*     NP       is the number of plates in model. */

/*     NVXTOT   is the total number of voxels in fine grid. */

/*     VTXBDS   are the vertex bounds. This is an array of six values */
/*              giving the minimum and maximum values of each component */
/*              of the vertex set. VTXBDS has dimensions ( 2, 3 ). */
/*              Units are km. */

/*     VOXSIZ   is the fine grid voxel size. DSK voxels are cubes; the */
/*              edge length of each cube is given by the voxel size. */
/*              This size applies to the fine voxel grid. Units are km. */

/*     VOXORI   is the voxel grid origin. This is the location of the */
/*              voxel grid origin in the body-fixed frame associated */
/*              with the target body. Units are km. */

/*     VGREXT   is the voxel grid extent. This extent is an array of */
/*              three integers indicating the number of voxels in the */
/*              X, Y, and Z directions in the fine voxel grid. */

/*     CGSCAL   is the coarse voxel grid scale. The extent of the fine */
/*              voxel grid is related to the extent of the coarse voxel */
/*              grid by this scale factor. */

/*     VTXNPL   is the vertex-plate correspondence list size. */

/*     VOXNPT   is the size of the voxel-to-plate pointer list. */

/*     VOXNPL   is the voxel-plate correspondence list size. */

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

/* $ Files */

/*     See input argument HANDLE. */

/* $ Particulars */

/*     This routine supports computations involving bookkeeping */
/*     information stored in DSK type 2 segments. User applications */
/*     typically will not need to call this routine. */

/*     DSK files are built using the DLA low-level format and */
/*     the DAS architecture; DLA files are a specialized type of DAS */
/*     file in which data are organized as a doubly linked list of */
/*     segments. Each segment's data belong to contiguous components of */
/*     character, double precision, and integer type. */

/* $ Examples */

/*     The numerical results shown for this example may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */

/*     1) Dump several parameters from the first DLA segment of */
/*        a DSK file. The segment is assumed to be of type 2. */


/*        Example code begins here. */


/*              PROGRAM DSKB02_EX1 */
/*              IMPLICIT NONE */

/*              INCLUDE 'dla.inc' */
/*              INCLUDE 'dskdsc.inc' */
/*              INCLUDE 'dsk02.inc' */

/*        C */
/*        C     Local parameters */
/*        C */
/*              INTEGER               FILSIZ */
/*              PARAMETER           ( FILSIZ = 255 ) */

/*              INTEGER               LNSIZE */
/*              PARAMETER           ( LNSIZE = 80 ) */

/*        C */
/*        C     Local variables */
/*        C */
/*              CHARACTER*(FILSIZ)    DSK */
/*              CHARACTER*(LNSIZE)    OUTLIN */

/*              DOUBLE PRECISION      VOXORI ( 3 ) */
/*              DOUBLE PRECISION      VOXSIZ */
/*              DOUBLE PRECISION      VTXBDS ( 2, 3 ) */

/*              INTEGER               CGSCAL */
/*              INTEGER               DLADSC ( DLADSZ ) */
/*              INTEGER               HANDLE */
/*              INTEGER               NP */
/*              INTEGER               NV */
/*              INTEGER               NVXTOT */
/*              INTEGER               VGREXT ( 3 ) */
/*              INTEGER               VOXNPL */
/*              INTEGER               VOXNPT */
/*              INTEGER               VTXNPL */

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
/*        C     Read and display type 2 bookkeeping data. */
/*        C */
/*              CALL DSKB02 ( HANDLE, DLADSC, NV,     NP,     NVXTOT, */
/*             .              VTXBDS, VOXSIZ, VOXORI, VGREXT, CGSCAL, */
/*             .              VTXNPL, VOXNPT, VOXNPL                 ) */

/*        C */
/*        C     Show vertex and plate counts. */
/*        C */
/*              OUTLIN = 'Number of vertices:                 #' */
/*              CALL REPMI  ( OUTLIN, '#', NV, OUTLIN ) */
/*              CALL TOSTDO ( OUTLIN ) */

/*              OUTLIN = 'Number of plates:                   #' */
/*              CALL REPMI  ( OUTLIN, '#', NP, OUTLIN ) */
/*              CALL TOSTDO ( OUTLIN ) */

/*              OUTLIN = 'Voxel edge length (km):             #' */
/*              CALL REPMF  ( OUTLIN, '#', VOXSIZ, 6, 'E', OUTLIN ) */
/*              CALL TOSTDO ( OUTLIN ) */

/*              OUTLIN = 'Number of voxels:                   #' */
/*              CALL REPMI  ( OUTLIN, '#', NVXTOT, OUTLIN ) */
/*              CALL TOSTDO ( OUTLIN ) */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, using the DSK file named phobos512.bds, the output */
/*        was: */


/*        Enter DSK name > phobos512.bds */
/*        Number of vertices:                 1579014 */
/*        Number of plates:                   3145728 */
/*        Voxel edge length (km):             1.04248E-01 */
/*        Number of voxels:                   11914500 */


/* $ Restrictions */

/*     1)  The caller must verify that the segment associated with */
/*         the input DLA descriptor is a DSK type 2 segment. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     B.V. Semenov       (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.1, 02-JUL-2021 (JDR) (BVS) */

/*        Edited the header to comply with NAIF standard. Added */
/*        solution for code example. */

/* -    SPICELIB Version 1.0.0, 08-FEB-2017 (NJB) */

/*        Updated version info. */

/*        23-JAN-2016 (NJB) */

/*           Removed references to unneeded variables. */
/*           Updated header comments. */

/*        DSKLIB Version 2.0.0, 05-MAY-2010 (NJB) */

/*           Renamed routine from DSKP02 to DSKB02. */

/*        DSKLIB Version 1.0.1, 08-OCT-2009 (NJB) */

/*           Updated header. */

/*        Beta Version 1.0.0, 30-OCT-2006 (NJB) */

/* -& */
/* $ Index_Entries */

/*     fetch parameters from a type 2 DSK segment */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     Local variables */

    if (return_()) {
	return 0;
    }
    chkin_("DSKB02", (ftnlen)6);
    dpbase = dladsc[4];
    ibase = dladsc[2];

/*     Read the integer parameters first.  These are located at the */
/*     beginning of the integer component of the segment. */

    i__1 = ibase + 1;
    i__2 = ibase + 10;
    dasrdi_(handle, &i__1, &i__2, ibuff);
    *nv = ibuff[0];
    *np = ibuff[1];
    *nvxtot = ibuff[2];
    *cgscal = ibuff[6];
    *voxnpt = ibuff[7];
    *voxnpl = ibuff[8];
    *vtxnpl = ibuff[9];
    movei_(&ibuff[3], &c__3, vgrext);

/*     Read the d.p. parameters. */

    b = dpbase + 25;
    e = dpbase + 34;
    dasrdd_(handle, &b, &e, dbuff);
    moved_(dbuff, &c__6, vtxbds);
    vequ_(&dbuff[6], voxori);
    *voxsiz = dbuff[9];
    chkout_("DSKB02", (ftnlen)6);
    return 0;
} /* dskb02_ */

