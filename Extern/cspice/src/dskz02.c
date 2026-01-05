/* dskz02.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;
static integer c__2 = 2;

/* $Procedure DSKZ02 ( DSK, fetch type 2 model size parameters ) */
/* Subroutine */ int dskz02_(integer *handle, integer *dladsc, integer *nv, 
	integer *np)
{
    integer n;
    extern /* Subroutine */ int chkin_(char *, ftnlen), dski02_(integer *, 
	    integer *, integer *, integer *, integer *, integer *, integer *);
    extern logical return_(void);
    extern /* Subroutine */ int chkout_(char *, ftnlen);

/* $ Abstract */

/*     Return plate model size parameters---plate count and */
/*     vertex count---for a type 2 DSK segment. */

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
/*     NV         O   Number of vertices. */
/*     NP         O   Number of plates. */

/* $ Detailed_Input */

/*     HANDLE   is the handle of a DSK file containing a type 2 */
/*              segment from which data are to be fetched. */

/*     DLADSC   is the DLA descriptor associated with the segment */
/*              from which data are to be fetched. */

/* $ Detailed_Output */

/*     NV       is the number of vertices belonging to */
/*              the specified plate model. */

/*     NP       is the number of plates belonging to the */
/*              specified plate model. */

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

/*     This routine enables SPICE-based user applications to */
/*     conveniently fetch the plate and vertex counts of a type 2 DSK */
/*     segment. */

/*     See the routine DSKB02 (DSK, fetch type 2 bookkeeping data) */
/*     for an interface that returns all type 2 DSK segment */
/*     bookkeeping data in a single call. */

/* $ Examples */

/*     The numerical results shown for this example may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */

/*     1) Look up all the vertices associated with each plate */
/*        of the model contained in a specified type 2 segment. For each */
/*        plate, display the plate's vertices and normal vector. */

/*        For this example, we'll show the context of this look-up: */
/*        opening the DSK file for read access, traversing a trivial, */
/*        one-segment list to obtain the segment of interest. */


/*        Example code begins here. */


/*              PROGRAM DSKZ02_EX1 */
/*              IMPLICIT NONE */

/*              INCLUDE 'dla.inc' */
/*              INCLUDE 'dsk02.inc' */


/*              CHARACTER*(*)         FMT */
/*              PARAMETER           ( FMT    = '(1X,A,3(1XE15.8))' ) */


/*              INTEGER               BUFSIZ */
/*              PARAMETER           ( BUFSIZ = 10000 ) */

/*              INTEGER               FILSIZ */
/*              PARAMETER           ( FILSIZ = 255 ) */


/*              CHARACTER*(FILSIZ)    DSK */

/*              DOUBLE PRECISION      NORMAL ( 3 ) */
/*              DOUBLE PRECISION      VERTS  ( 3, BUFSIZ ) */

/*              INTEGER               DLADSC ( DLADSZ ) */
/*              INTEGER               HANDLE */
/*              INTEGER               I */
/*              INTEGER               J */
/*              INTEGER               N */
/*              INTEGER               NNORM */
/*              INTEGER               NP */
/*              INTEGER               NREAD */
/*              INTEGER               NV */
/*              INTEGER               NVTX */
/*              INTEGER               PLATES  ( 3, BUFSIZ ) */
/*              INTEGER               PLIX */
/*              INTEGER               REMAIN */
/*              INTEGER               START */

/*              LOGICAL               FOUND */

/*        C */
/*        C     Prompt for name of DSK and open file for reading. */
/*        C */
/*              CALL PROMPT ( 'Enter DSK name > ', DSK ) */

/*              CALL DASOPR ( DSK, HANDLE ) */

/*              CALL DLABFS ( HANDLE, DLADSC, FOUND ) */

/*              IF ( .NOT. FOUND ) THEN */

/*                 CALL SETMSG ( 'No segment found in file #.' ) */
/*                 CALL ERRCH  ( '#',  DSK                     ) */
/*                 CALL SIGERR ( 'SPICE(NOSEGMENT)'            ) */

/*              END IF */

/*        C */
/*        C     Get segment vertex and plate counts. */
/*        C */
/*              CALL DSKZ02 ( HANDLE, DLADSC, NV, NP ) */

/*              WRITE (*,*) ' ' */
/*              WRITE (*,*) 'Number of vertices: ', NV */
/*              WRITE (*,*) 'Number of plates:   ', NP */
/*        C */
/*        C     Display the vertices of the first 5 plates. */
/*        C */
/*              REMAIN = MIN(5, NP) */
/*              START  = 1 */

/*              DO WHILE ( REMAIN .GT. 0 ) */
/*        C */
/*        C        NREAD is the number of plates we'll read on this */
/*        C        loop pass. */
/*        C */
/*                 NREAD  = MIN ( BUFSIZ, REMAIN ) */

/*                 CALL DSKP02 ( HANDLE, DLADSC, START, NREAD, N, */
/*             .                 PLATES                          ) */

/*                 DO I = 1, N */

/*                    PLIX = START + I - 1 */
/*        C */
/*        C           Read the vertices of the current plate. */
/*        C */
/*                    DO J = 1, 3 */
/*                       CALL DSKV02 ( HANDLE, DLADSC, PLATES(J,I), */
/*             .                       1,      NVTX,   VERTS (1,J)  ) */
/*                    END DO */
/*        C */
/*        C           Display the vertices of the current plate: */
/*        C */
/*                    WRITE (*,*  ) ' ' */
/*                    WRITE (*,*  ) 'Plate number: ', PLIX */
/*                    WRITE (*,FMT) '   Vertex 1: ', (VERTS(J,1), J=1,3) */
/*                    WRITE (*,FMT) '   Vertex 2: ', (VERTS(J,2), J=1,3) */
/*                    WRITE (*,FMT) '   Vertex 3: ', (VERTS(J,3), J=1,3) */

/*        C */
/*        C           Display the normal vector of the current plate: */
/*        C */
/*                    CALL DSKN02 ( HANDLE, DLADSC, PLIX, NORMAL ) */

/*                    WRITE (*,FMT) '   Normal:   ', (NORMAL(J), J=1,3) */

/*                 END DO */

/*                 START  = START  + NREAD */
/*                 REMAIN = REMAIN - NREAD */

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

/*         Number of vertices:      1579014 */
/*         Number of plates:        3145728 */

/*         Plate number:            1 */
/*            Vertex 1:  -0.67744400E+01  0.62681500E+01  0.60114900E+01 */
/*            Vertex 2:  -0.67623800E+01  0.62572800E+01  0.60255600E+01 */
/*            Vertex 3:  -0.67571000E+01  0.62775400E+01  0.60209600E+01 */
/*            Normal:    -0.58197377E+00  0.32128561E+00  0.74704892E+00 */

/*         Plate number:            2 */
/*            Vertex 1:  -0.67744400E+01  0.62681500E+01  0.60114900E+01 */
/*            Vertex 2:  -0.67797300E+01  0.62479000E+01  0.60161000E+01 */
/*            Vertex 3:  -0.67623800E+01  0.62572800E+01  0.60255600E+01 */
/*            Normal:    -0.58145695E+00  0.32198831E+00  0.74714881E+00 */

/*         Plate number:            3 */
/*            Vertex 1:  -0.67797300E+01  0.62479000E+01  0.60161000E+01 */
/*            Vertex 2:  -0.67676800E+01  0.62370100E+01  0.60301900E+01 */
/*            Vertex 3:  -0.67623800E+01  0.62572800E+01  0.60255600E+01 */
/*            Normal:    -0.58159707E+00  0.32264196E+00  0.74675767E+00 */

/*         Plate number:            4 */
/*            Vertex 1:  -0.67797300E+01  0.62479000E+01  0.60161000E+01 */
/*            Vertex 2:  -0.67849900E+01  0.62276200E+01  0.60207000E+01 */
/*            Vertex 3:  -0.67676800E+01  0.62370100E+01  0.60301900E+01 */
/*            Normal:    -0.58312901E+00  0.32056070E+00  0.74645924E+00 */

/*         Plate number:            5 */
/*            Vertex 1:  -0.67849900E+01  0.62276200E+01  0.60207000E+01 */
/*            Vertex 2:  -0.67729900E+01  0.62167400E+01  0.60348200E+01 */
/*            Vertex 3:  -0.67676800E+01  0.62370100E+01  0.60301900E+01 */
/*            Normal:    -0.58366405E+00  0.32306020E+00  0.74496200E+00 */


/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     B.V. Semenov       (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.1, 08-JUL-2020 (JDR) (BVS) */

/*        Edited the header to comply with NAIF standard. Modified code */
/*        example to reduce the output. */

/* -    SPICELIB Version 1.0.0, 02-JUN-2010 (NJB) */

/* -& */
/* $ Index_Entries */

/*     fetch model size parameters from a type 2 DSK segment */

/* -& */

/*     SPICELIB functions */


/*     Local variables */

    if (return_()) {
	return 0;
    }
    chkin_("DSKZ02", (ftnlen)6);
    dski02_(handle, dladsc, &c__1, &c__1, &c__1, &n, nv);
    dski02_(handle, dladsc, &c__2, &c__1, &c__1, &n, np);
    chkout_("DSKZ02", (ftnlen)6);
    return 0;
} /* dskz02_ */

