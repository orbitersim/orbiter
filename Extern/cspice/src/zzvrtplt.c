/* zzvrtplt.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure  ZZVRTPLT  ( create vertex-plate mapping ) */
/* Subroutine */ int zzvrtplt_(integer *nv, integer *np, integer *plates, 
	integer *cellsz, integer *maxlst, integer *cells, integer *vrtptr, 
	integer *nlist, integer *pltlst)
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    extern /* Subroutine */ int zzaddlnk_(integer *, integer *, integer *, 
	    integer *, integer *, integer *, integer *), zzinilnk_(integer *, 
	    integer *, integer *, integer *, integer *);
    static integer i__, j;
    extern /* Subroutine */ int zzuntngl_(integer *, integer *, integer *, 
	    integer *, integer *, integer *, integer *), chkin_(char *, 
	    ftnlen);
    static integer ncell;
    extern logical failed_(void);
    extern /* Subroutine */ int setmsg_(char *, ftnlen), errint_(char *, 
	    integer *, ftnlen), sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen);
    extern logical return_(void);

/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines. Users should not call this routine directly due */
/*     to the volatile nature of this routine. */

/*     Generate a data structure that represents the mapping from */
/*     vertices to the plates containing them. */

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

/*     DSK */
/*     PLATE */
/*     VERTEX */

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
/*     NV         I   Number of vertices. */
/*     NP         I   Number of plates. */
/*     PLATES     I   Plate array. */
/*     CELLSZ     I   Cell array size. */
/*     MAXLST     I   Maximum size of output plate list. */
/*     CELLS     I-O  Workspace array for vertex list construction. */
/*     VRTPTR     O   Array of pointers, by vertex, into plate data list. */
/*     NLIST      O   Length of the plate data list. */
/*     PLTLST     O   Plate data list; first element is the number of */
/*                    plate IDs to follow. */

/* $ Detailed_Input */

/*     NV         Number of vertices. */

/*     NP         Number of plates. */

/*     PLATES     Plate array: this is an array of 3-tuples of */
/*                vertex indices. The elements */

/*                   PLATES(1:3,I) */

/*                are the indices of the vertices of the Ith plate. */


/*     CELLSZ     Size of cell array: explicit workspace dimension used */
/*                for error checking. This value should be */

/*                   3*NP */

/*     MAXLST     Maximum size of output plate list. This size should be */

/*                   3*NP  +  NV */


/*     CELLS      workspace array used to construct the vertex-plate */
/*                mapping. */

/* $ Detailed_Output */

/*     CELLS      Workspace array used to construct the vertex-plate */
/*                mapping. */

/*     VRTPTR     Array of pointers associating plate lists with */
/*                vertices. The Ith element of VRTPTR contains */
/*                the start index in PLTLST of the list associated with */
/*                the Ith vertex. */

/*                The size of this array must be at least NV. */

/*     NLIST      Length of the output plate data list PLTLST. */

/*     PLTLST     Plate data list: for each vertex, there is a count */
/*                of associated plates, followed by the IDs of those */
/*                plates. The count for vertex I is located at */

/*                   PLTLST( VRTPTR(I) ) */

/*                The size of this array must be at least MAXLST. */

/* $ Parameters */

/*     See parameter declarations in */

/*        dsk02.inc */

/* $ Exceptions */

/*     1)  If the input plate count NP is non-positive, the */
/*         error SPICE(BADPLATECOUNT) is signaled. */

/*     2)  If the input vertex count NV is non-positive, the */
/*         error SPICE(BADVERTEXCOUNT) is signaled. */

/*     3)  If the cell array size CELLSZ is less than */

/*            3 * NP */

/*         the error SPICE(CELLARRAYTOOSMALL) is signaled. */

/*     4)  If the plate list size MAXPLT is less than */

/*            ( 3 * NP ) + NV */

/*         the error SPICE(PLATELISTTOOSMALL) is signaled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine supports the DSK type 2 spatial index */
/*     routines */

/*        DSKMI2 */
/*        DSKSI2 */

/* $ Examples */

/*     See usage in DSKMI2. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman    (JPL) */
/*     J.A. Bytof      (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 20-NOV-2020 (NJB) */

/*        Bug fix: in call to ZZADDLNK, size of pointer array was */
/*        changed from NP to NV. */

/* -    SPICELIB Version 1.0.0, 20-MAY-2016 (NJB) */

/*        Updated error handling. Now ZZVRTPLT performs precise checks */
/*        on cell and plate array size inputs. */


/*        22-DEC-2015 (NJB) */

/*           Renamed routine from VRTCOM to ZZVRTPLT. */

/*           CAUTION: argument list change! Removed input workspace */
/*           array for pointer construction. Array was named PNTRS. */

/*           Added error checks for input counts and sizes. */

/*           Now calls updated 3.0.0 version of ZZUNTNGL. */

/*        03-MAY-2014 (NJB) */

/*           Now calls ZZ* versions of supporting routines. */

/*           Changed argument list to include sizes of arrays. */
/*           Changed error handling to make use of array sizes. */
/*           Changed call to UNTNGL to accommodate argument */
/*           list change in that routine. Updated header I/O */
/*           descriptions. */

/*        12-JUL-2011 (NJB) */

/*           Argument list change: the input arrays V1, V2, V3 were */
/*           replaced with the input array PLATES. */

/*        04-MAY-2010 (NJB) */

/*           Now accepts input workspace arrays PNTRS and CELLS. */
/*           Changed INCLUDE file to dsk02.inc. */

/*        08-OCT-2009 (NJB) */

/*           Re-ordered header sections. */

/*        04-FEB-1999 (JAB) */

/* -& */
/* $ Index_Entries */

/*     create vertex to plate map */

/* -& */

/*     SPICELIB functions */


/*     Local variables. */


/*     Standard SPICELIB error handling. */

    if (return_()) {
	return 0;
    }
    chkin_("ZZVRTPLT", (ftnlen)8);
    if (*nv < 1) {
	setmsg_("Vertex count NV = #; count must be positive.be positive.", (
		ftnlen)56);
	errint_("#", nv, (ftnlen)1);
	sigerr_("SPICE(BADVERTEXCOUNT)", (ftnlen)21);
	chkout_("ZZVRTPLT", (ftnlen)8);
	return 0;
    }
    if (*np < 1) {
	setmsg_("Plate count NP = #; count must be positive.be positive.", (
		ftnlen)55);
	errint_("#", np, (ftnlen)1);
	sigerr_("SPICE(BADPLATECOUNT)", (ftnlen)20);
	chkout_("ZZVRTPLT", (ftnlen)8);
	return 0;
    }
    if (*cellsz < *np * 3) {
	setmsg_("Cell array size CELLSZ = #; size must be >= 3*NP. NP is the"
		" plate count #.", (ftnlen)74);
	errint_("#", cellsz, (ftnlen)1);
	errint_("#", np, (ftnlen)1);
	sigerr_("SPICE(CELLARRAYTOOSMALL)", (ftnlen)24);
	chkout_("ZZVRTPLT", (ftnlen)8);
	return 0;
    }
    if (*maxlst < *nv + *np * 3) {
	setmsg_("Plate list array size MAXPLT = #; size must be >= 3*NP + NV"
		", which is #. (NV = vertex count, NP = plate count.)", (
		ftnlen)111);
	errint_("#", maxlst, (ftnlen)1);
	i__1 = *np * 3 + *nv;
	errint_("#", &i__1, (ftnlen)1);
	sigerr_("SPICE(PLATELISTTOOSMALL)", (ftnlen)24);
	chkout_("ZZVRTPLT", (ftnlen)8);
	return 0;
    }

/*     Initialize pointer and cell structure. */

    zzinilnk_(nv, cellsz, &ncell, vrtptr, cells);
    if (failed_()) {
	chkout_("ZZVRTPLT", (ftnlen)8);
	return 0;
    }

/*     Loop over all plate IDS. Add each plate/vertex */
/*     combination to the linked list. */

    i__1 = *np;
    for (i__ = 1; i__ <= i__1; ++i__) {
	for (j = 1; j <= 3; ++j) {

/*           AVAL = PLATES(J,I), vertex J of plate ID I. */
/*           BVAL = I, plate ID value I. */

	    zzaddlnk_(&plates[j + i__ * 3 - 4], &i__, nv, cellsz, vrtptr, &
		    ncell, cells);
	    if (failed_()) {
		chkout_("ZZVRTPLT", (ftnlen)8);
		return 0;
	    }
	}
    }

/*     Generate two linked lists mapping vertex ID to the plates */
/*     including that vertex as a member. */

/*     VRTPTR: An array, indexed by vertex ID. For an array element, */
/*             VRTPTR(VERT_ID), greater than zero, the value identifies */
/*             an index in PLTLST, the value of that PLTLST array */
/*             element  equaling the number of plates that include */
/*             the vertex specified by the ID as a member. The */
/*             condition VRTPTR(VERT_ID) = -1 indicates a bug. */

/*     PLTLST: An array, indexed by the positive entries in VRTPTR. */
/*             The element, N, identified by a VRTPTR value describes */
/*             the number of plates of which the vertex is a member. */

/*                 N = PLTLST( VRTPTR(VERT_ID) ) */

/*             The N elements following PLTLST( VRTPTR(VERT_ID) ), */
/*             contain the IDs of those plates which have the vertex */
/*             as a member. */

    zzuntngl_(nv, cellsz, cells, maxlst, vrtptr, nlist, pltlst);

/*     Standard SPICE error handling. */

    chkout_("ZZVRTPLT", (ftnlen)8);
    return 0;
} /* zzvrtplt_ */

