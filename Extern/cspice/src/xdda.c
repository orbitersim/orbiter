/* xdda.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;
static doublereal c_b67 = 0.;
static doublereal c_b68 = 1.;

/* $Procedure XDDA  ( list voxels intersected by a ray ) */
/* Subroutine */ int xdda_(doublereal *vertex, doublereal *raydir, integer *
	grdext, integer *maxnvx, integer *nvx, integer *voxlst)
{
    /* Initialized data */

    static integer next[3] = { 2,3,1 };

    /* System generated locals */
    integer i__1, i__2, i__3, i__4;
    doublereal d__1;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer);

    /* Local variables */
    integer step[3], i__;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    extern doublereal dpmax_(void);
    extern /* Subroutine */ int errdp_(char *, doublereal *, ftnlen);
    integer iaxis[3];
    doublereal limit;
    extern logical vzero_(doublereal *);
    doublereal ax2err, ax3err, s12, s13;
    extern doublereal brcktd_(doublereal *, doublereal *, doublereal *);
    extern integer brckti_(integer *, integer *, integer *);
    integer icoord[3];
    doublereal maxcmp;
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), setmsg_(char *, ftnlen), errint_(char *, integer *, 
	    ftnlen);
    doublereal vtxoff[3];
    extern logical return_(void);
    integer intvtx[3];
    extern logical zzingrd_(integer *, integer *);

/* $ Abstract */

/*     Return a list of voxels that a given ray intersects in a given */
/*     voxel grid. */

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

/*     GRID */
/*     INTERSECTION */
/*     PLATE */
/*     VOXEL */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     GRDTOL     P   Tolerance for vertex distance from grid. */
/*     VERTEX     I   Voxel grid coordinates of ray's vertex. */
/*     RAYDIR     I   Direction vector of ray. */
/*     GRDEXT     I   Dimensions of grid in voxel units. */
/*     MAXNVX     I   Maximum value of VOXLST. */
/*     NVX        O   Number of voxels in the VOXLST list. */
/*     VOXLST     O   List of voxels intersected by ray. */

/* $ Detailed_Input */

/*     VERTEX   is the voxel grid coordinates of ray's vertex. These */
/*              coordinates are zero-based, double precision offsets from */
/*              the grid's origin. The units of the coordinates are */
/*              voxels, that is, voxel edge lengths. */

/*     RAYDIR   is the direction vector of ray from VERTEX. */

/*     GRDEXT   is the integer 3-vector containing the voxel grid */
/*              extents. These are the dimensions of the voxel grid in */
/*              voxel units, in the X, Y, and Z directions respectively. */

/*     MAXNVX   is the maximum number of voxel coordinate sets that can */
/*              be stored in VOXLST. */

/* $ Detailed_Output */

/*     NVX      is the number of voxel coordinate sets contained in */
/*              VOXLST. */

/*     VOXLST   is the list of coordinate sets of voxels intersected by */
/*              ray. Elements */

/*                 VOXLST(J,I), J = 1, 3 */

/*              are the coordinates of the Ith voxel in the list. These */
/*              coordinates are 1-based integer values. */

/*              The voxels in the output list are ordered by increasing */
/*              distance from the ray's vertex. */

/* $ Parameters */

/*     GRDTOL   is a tolerance value used to determine whether */
/*              VERTEX is too far from the voxel grid. The Ith */
/*              component of VERTEX must not differ from the */
/*              Ith coordinate of the nearest grid point by more */
/*              than */

/*                  GRDTOL * EXTENT(I) */

/* $ Exceptions */

/*     1)  If the input RAYDIR has all zero components, the error */
/*         SPICE(ZEROVECTOR) is signaled. */

/*     2)  If the maximum output list size MAXNVX is non-positive, the */
/*         error SPICE(INVALIDSIZE) is signaled. */

/*     3)  If any element of the grid extents array GRDEXT is */
/*         non-positive, the error SPICE(BADDIMENSIONS) is signaled. */

/*     4)  If the ray's vertex is neither inside, nor within a small */
/*         distance from, the voxel grid, the error */
/*         SPICE(VERTEXNOTINGRID) is signaled. See the description of the */
/*         parameter GRDTOL. */

/*     5)  If the value of the NVX counter (number of intersected voxels) */
/*         exceeds the size of the VOXLST input vector, the error */
/*         SPICE(ARRAYTOOSMALL) is signaled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine supports use of a spatial index for rapid */
/*     selection of plates that could be hit by a specified ray. */

/* $ Examples */

/*     See the routine DSKX02 for a usage example. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J.A. Bytof         (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     B.V. Semenov       (JPL) */
/*     E.D. Wright        (JPL) */

/* $ Version */

/* -    SPICELIB Version 3.1.1, 26-OCT-2021 (NJB) (JDR) */

/*        Edited the header to comply with NAIF standard. */

/*        Typo correction: the description of the error denoted */
/*        by the short message SPICE(VERTEXNOTINGRID) had been */
/*        the negative of what was intended. */

/* -    SPICELIB Version 3.1.0, 02-FEB-2016 (NJB) */

/*        Updated to call ZZINGRD rather than INGRD. */
/*        Minor updates were made to header I/O sections. */

/* -    SPICELIB Version 3.0.0, 11-JUL-2014 (NJB) (EDW) (BVS) (JAB) */

/*        Previously released as DSKLIB: */

/*        DSKLIB Version 3.0.0, 11-JUL-2014 (NJB) */

/*           Bug fix: a correction was made to the computation of */
/*           the vertex offset from the bounding planes of the */
/*           voxel containing the vertex. */

/*           Minor edits were made to comments. */

/*        Last update was 05-JUN-2014 (NJB) */

/*           Bug fix: the use of the MOD function led to a 1-voxel */
/*           size error when the input ray's vertex was on the */
/*           voxel grid boundary. */

/*           An error check for invalid grid dimensions was added. */

/*           Code to prevent arithmetic overflow was added. */

/*           Code was added to prevent the values AX2ERR and AX3ERR from */
/*           ever becoming negative when the components of the ray's */
/*           direction vector in the corresponding directions are zero or */
/*           too small for a voxel step in those directions to occur. */

/*           Renamed the routine's arguments, except for NVX. */

/*           Detailed output descriptions were updated to refer to */
/*           voxel coordinates rather than IDs. References to sorting */
/*           were deleted. */

/*           In-line comments now explain the routine's algorithm. */
/*           Old comments that are no longer applicable were deleted. */

/*        DSKLIB Version 2.1.0, 26-JUL-2010 (NJB) */

/*           Bug fix: voxel space coordinates of input */
/*           vertex are now bracketed within the voxel */
/*           grid. */

/*           This prevents round-off errors from occurring */
/*           when the vertex is slightly outside the grid, */
/*           but may not be appropriate for all applications. */
/*           Therefore it may make sense to make this a */
/*           private routine. */

/*        DSKLIB Version 2.0.0, 20-APR-2010 (NJB) */

/*           Removed commented out lines declaring and calling VOX2ID. */

/*        DSKLIB Version 1.1.0, 08-OCT-2009 (NJB) */

/*           Updated header. */

/*           Bug fix: driving axis for intercept computation is */
/*           now determined by largest component of ray direction vector. */
/*           This fix was made long before this header update. */

/*        DSKLIB Version 1.1.0, 19-OCT-2004 (EDW) */

/*           Added logic to remove duplicate voxel IDs from */
/*           the return list. Extended programming comments. */

/*        DSKLIB Version 1.0.1, 26-AUG-2002 (BVS) */

/*           Replaced WRITE with normal error reporting calls. */

/*        DSKLIB Version 1.0.0, 03-FEB-1999 (JAB) */

/* -& */
/* $ Index_Entries */

/*     list voxels intersected by a ray */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     Local variables */


/*     Saved variables */


/*     Initial values */


/*     Use discovery check-in. */

    if (return_()) {
	return 0;
    }

/*     The algorithm below efficiently determines the set of voxels */
/*     intersected by the input ray. */

/*     This algorithm doesn't compute the intersections of the ray */
/*     with the boundaries of the voxels, nor does it ever compute */
/*     the coordinates of any point on the ray. Instead, it keeps */
/*     track of the voxel boundary planes that the ray passes through. */

/*     The algorithm starts out by determining which voxel contains the */
/*     ray's vertex. It computes the distances from the vertex to the */
/*     "next" voxel boundary planes---those that the ray is headed */
/*     towards. It maintains measurements that enable it to determine */
/*     which boundary plane is hit next. The voxel on the other side of */
/*     that intersection point is the next voxel the ray goes through. */
/*     In the case of ties, any of the candidate "next" voxels may be */
/*     selected. The "next" voxel is added to the output voxel list, the */
/*     measurements of relative distances to the next boundaries are */
/*     updated, and the algorithm continues in this fashion until a */
/*     voxel outside the grid is detected. */

/*     The relative distance measurements from the ray's vertex to */
/*     the "next" boundary planes are defined as follows: */

/*        -  For the primary ray direction---this is the direction */
/*           corresponding to the component of largest magnitude of the */
/*           ray's direction vector---the distance is just the */
/*           difference of the primary coordinates of the next plane and */
/*           of the ray's vertex. */

/*        -  For each axis orthogonal to the primary one, the algorithm */
/*           computes the length of the projection onto the primary axis */
/*           of the portion of the ray extending from the vertex to the */
/*           "next" voxel boundary plane orthogonal to that non-primary */
/*           axis. From that projection length the distance from the */
/*           vertex to the boundary in the primary direction is */
/*           subtracted. */

/*           For the non-primary axes, these differences are stored in */
/*           the respective variables */

/*              AX2ERR */
/*              AX3ERR */

/*           When AX2ERR is negative, the ray will hit the next voxel */
/*           boundary orthogonal to the "second" axis (having its */
/*           index stored in the variable IAXIS(2)) before it hits */
/*           the next boundary orthogonal to the primary axis. The */
/*           quantity AX3ERR behaves similarly. */

/*           If both AX2ERR and AX3ERR are negative, the more negative */
/*           value marks the boundary plane that is hit first. */

/*     The axes will be re-labeled using the variable IAXIS. IAXIS(1) */
/*     will be the index of the primary axis. */

/*     There are a few numeric issues to consider: */

/*        1)  The ratios of the components of the ray's direction vector */
/*            are computed and stored in the variables S12 and S13. Very */
/*            small components acting as denominators could cause */
/*            arithmetic overflow. */

/*        2)  The quantities S12 and S13, while representable as double */
/*            precision numbers, can be quite large. These quantities */
/*            may be added repeatedly to the terms AX2ERR and AX3ERR, */
/*            respectively. These additions could potentially result */
/*            in arithmetic overflow. */

/*     Both of these problems are addressed by the following observation: */

/*        If a component of the ray direction vector is small enough, and */
/*        the corresponding component of the ray's vertex is not on a */
/*        voxel boundary, the ray will exit the grid before reaching a */
/*        bounding plane orthogonal to that component of the direction */
/*        vector. */

/*        If the above situation holds, but the ray's vertex is already */
/*        on a boundary plane orthogonal to the small component, then */
/*        the ray will exit the grid before hitting a parallel boundary */
/*        plane. */

/*     So we can safely treat very small direction components as zero. */


/*     Check if ray direction vector is a zero vector. */

    if (vzero_(raydir)) {
	chkin_("XDDA", (ftnlen)4);
	setmsg_("Ray is the zero vector.", (ftnlen)23);
	sigerr_("SPICE(ZEROVECTOR)", (ftnlen)17);
	chkout_("XDDA", (ftnlen)4);
	return 0;
    }

/*     Check the voxel grid dimensions. */

/* Computing MIN */
    i__1 = min(grdext[0],grdext[1]);
    if (min(i__1,grdext[2]) < 1) {
	chkin_("XDDA", (ftnlen)4);
	setmsg_("Voxel grid dimensions must be strictly positive but are # #"
		" #.", (ftnlen)62);
	errint_("#", grdext, (ftnlen)1);
	errint_("#", &grdext[1], (ftnlen)1);
	errint_("#", &grdext[2], (ftnlen)1);
	sigerr_("SPICE(BADDIMENSIONS)", (ftnlen)20);
	chkout_("XDDA", (ftnlen)4);
	return 0;
    }

/*     Make sure the vertex is not too far from the voxel grid. */

    for (i__ = 1; i__ <= 3; ++i__) {
	if (vertex[(i__1 = i__ - 1) < 3 && 0 <= i__1 ? i__1 : s_rnge("vertex",
		 i__1, "xdda_", (ftnlen)433)] < grdext[(i__2 = i__ - 1) < 3 &&
		 0 <= i__2 ? i__2 : s_rnge("grdext", i__2, "xdda_", (ftnlen)
		433)] * -1e-12 || vertex[(i__3 = i__ - 1) < 3 && 0 <= i__3 ? 
		i__3 : s_rnge("vertex", i__3, "xdda_", (ftnlen)433)] > grdext[
		(i__4 = i__ - 1) < 3 && 0 <= i__4 ? i__4 : s_rnge("grdext", 
		i__4, "xdda_", (ftnlen)433)] * 1.0000000000010001) {
	    chkin_("XDDA", (ftnlen)4);
	    setmsg_("Vertex # # # is outside of voxel grid defined by extent"
		    "s # # #.", (ftnlen)63);
	    errdp_("#", vertex, (ftnlen)1);
	    errdp_("#", &vertex[1], (ftnlen)1);
	    errdp_("#", &vertex[2], (ftnlen)1);
	    errint_("#", grdext, (ftnlen)1);
	    errint_("#", &grdext[1], (ftnlen)1);
	    errint_("#", &grdext[2], (ftnlen)1);
	    sigerr_("SPICE(VERTEXNOTINGRID)", (ftnlen)22);
	    chkout_("XDDA", (ftnlen)4);
	    return 0;
	}
    }

/*     The maximum output voxel array size must be positive. */

    if (*maxnvx < 1) {
	chkin_("XDDA", (ftnlen)4);
	setmsg_("Maximum voxel list size must be positive but was #.", (
		ftnlen)51);
	errint_("#", maxnvx, (ftnlen)1);
	sigerr_("SPICE(INVALIDSIZE)", (ftnlen)18);
	chkout_("XDDA", (ftnlen)4);
	return 0;
    }

/*     Find the largest component of the direction vector. */

    iaxis[0] = 1;
    maxcmp = abs(raydir[0]);
    for (i__ = 2; i__ <= 3; ++i__) {
	if ((d__1 = raydir[(i__1 = i__ - 1) < 3 && 0 <= i__1 ? i__1 : s_rnge(
		"raydir", i__1, "xdda_", (ftnlen)476)], abs(d__1)) > maxcmp) {
	    iaxis[0] = i__;
	    maxcmp = (d__1 = raydir[(i__1 = i__ - 1) < 3 && 0 <= i__1 ? i__1 :
		     s_rnge("raydir", i__1, "xdda_", (ftnlen)478)], abs(d__1))
		    ;
	}
    }

/*     Set the indices of the orthogonal components of the direction */
/*     vector.  We maintain a right-handed relationship between the axes */
/*     labeled by IAXIS(1), IAXIS(2), and IAXIS(3):  the third axis is */
/*     the cross product of the first and second. */

    iaxis[1] = next[(i__1 = iaxis[0] - 1) < 3 && 0 <= i__1 ? i__1 : s_rnge(
	    "next", i__1, "xdda_", (ftnlen)489)];
    iaxis[2] = next[(i__1 = iaxis[1] - 1) < 3 && 0 <= i__1 ? i__1 : s_rnge(
	    "next", i__1, "xdda_", (ftnlen)490)];

/*     Which voxel contains the vertex? Truncate the vertex */
/*     coordinates to integers. Add 1 to each coord to compensate */
/*     for 1 based counting. */

    for (i__ = 1; i__ <= 3; ++i__) {
	intvtx[(i__1 = i__ - 1) < 3 && 0 <= i__1 ? i__1 : s_rnge("intvtx", 
		i__1, "xdda_", (ftnlen)499)] = (integer) vertex[(i__3 = iaxis[
		(i__2 = i__ - 1) < 3 && 0 <= i__2 ? i__2 : s_rnge("iaxis", 
		i__2, "xdda_", (ftnlen)499)] - 1) < 3 && 0 <= i__3 ? i__3 : 
		s_rnge("vertex", i__3, "xdda_", (ftnlen)499)];
	icoord[(i__1 = i__ - 1) < 3 && 0 <= i__1 ? i__1 : s_rnge("icoord", 
		i__1, "xdda_", (ftnlen)501)] = intvtx[(i__2 = i__ - 1) < 3 && 
		0 <= i__2 ? i__2 : s_rnge("intvtx", i__2, "xdda_", (ftnlen)
		501)] + 1;
	icoord[(i__1 = i__ - 1) < 3 && 0 <= i__1 ? i__1 : s_rnge("icoord", 
		i__1, "xdda_", (ftnlen)503)] = brckti_(&icoord[(i__2 = i__ - 
		1) < 3 && 0 <= i__2 ? i__2 : s_rnge("icoord", i__2, "xdda_", (
		ftnlen)503)], &c__1, &grdext[(i__4 = iaxis[(i__3 = i__ - 1) < 
		3 && 0 <= i__3 ? i__3 : s_rnge("iaxis", i__3, "xdda_", (
		ftnlen)503)] - 1) < 3 && 0 <= i__4 ? i__4 : s_rnge("grdext", 
		i__4, "xdda_", (ftnlen)503)]);
	voxlst[iaxis[(i__1 = i__ - 1) < 3 && 0 <= i__1 ? i__1 : s_rnge("iaxis"
		, i__1, "xdda_", (ftnlen)505)] - 1] = icoord[(i__2 = i__ - 1) 
		< 3 && 0 <= i__2 ? i__2 : s_rnge("icoord", i__2, "xdda_", (
		ftnlen)505)];
    }

/*     Initialize the counter for number of voxels the ray intercepts. */
/*     The bracketing done above ensures that the coordinates ICOORD of */
/*     the voxel considered to contain ray's vertex (there is a choice */
/*     to be made if the vertex lies on a voxel boundary) are within the */
/*     grid. */
    *nvx = 1;

/*     Calculate the relative location of vertex within the voxel. The */
/*     coordinates of a voxel's corners are integer values with each */
/*     voxel side length 1 (in voxel coords). */

/*     The variable VTXOFF usually has components equal to the */
/*     fractional parts of the corresponding components of VERTEX( */
/*     IAXIS(I) ), but the components of VTXOFF may be as large as 1 and */
/*     are never less than 0. */

    for (i__ = 1; i__ <= 3; ++i__) {
	d__1 = vertex[(i__3 = iaxis[(i__2 = i__ - 1) < 3 && 0 <= i__2 ? i__2 :
		 s_rnge("iaxis", i__2, "xdda_", (ftnlen)530)] - 1) < 3 && 0 <=
		 i__3 ? i__3 : s_rnge("vertex", i__3, "xdda_", (ftnlen)530)] 
		- (icoord[(i__4 = i__ - 1) < 3 && 0 <= i__4 ? i__4 : s_rnge(
		"icoord", i__4, "xdda_", (ftnlen)530)] - 1);
	vtxoff[(i__1 = i__ - 1) < 3 && 0 <= i__1 ? i__1 : s_rnge("vtxoff", 
		i__1, "xdda_", (ftnlen)530)] = brcktd_(&d__1, &c_b67, &c_b68);
    }

/*     Compute the lower limit on the magnitudes of RAYDIR( IAXIS(2) ) */
/*     and of RAYDIR( IAXIS(3) ) for which we'll treat those components */
/*     of the direction vector as non-zero. */

    limit = 1e-20 / grdext[(i__1 = iaxis[0] - 1) < 3 && 0 <= i__1 ? i__1 : 
	    s_rnge("grdext", i__1, "xdda_", (ftnlen)540)] * (d__1 = raydir[(
	    i__2 = iaxis[0] - 1) < 3 && 0 <= i__2 ? i__2 : s_rnge("raydir", 
	    i__2, "xdda_", (ftnlen)540)], abs(d__1));

/*     If the magnitude of RAYDIR( IAXIS(J) ), J = 2 or 3, is below */
/*     LIMIT, then the ray can pass through the entire grid in the */
/*     IAXIS(1) direction without its IAXIS(J) component changing by */
/*     more than EPS. We'll treat this case as though the IAXIS(J) */
/*     component of the ray were 0. */


/*     Determine the error term initial values and increments. */


    ax2err = dpmax_();
    ax3err = ax2err;
    s12 = 0.;
    s13 = 0.;

/*     Compute the initial relative distance measurement AX2ERR */
/*     for the non-primary axis IAXIS(2). */

    if ((d__1 = raydir[(i__1 = iaxis[1] - 1) < 3 && 0 <= i__1 ? i__1 : s_rnge(
	    "raydir", i__1, "xdda_", (ftnlen)561)], abs(d__1)) > limit) {

/*        For any line segment along the ray, S12 is the ratio of the */
/*        magnitudes of the projections of the segment in the primary */
/*        and the IAXIS(2) directions. */

	s12 = (d__1 = raydir[(i__1 = iaxis[0] - 1) < 3 && 0 <= i__1 ? i__1 : 
		s_rnge("raydir", i__1, "xdda_", (ftnlen)567)] / raydir[(i__2 =
		 iaxis[1] - 1) < 3 && 0 <= i__2 ? i__2 : s_rnge("raydir", 
		i__2, "xdda_", (ftnlen)567)], abs(d__1));
	if (raydir[(i__1 = iaxis[0] - 1) < 3 && 0 <= i__1 ? i__1 : s_rnge(
		"raydir", i__1, "xdda_", (ftnlen)570)] > 0.) {

/*           The primary component of the ray's direction is positive. */
/*           The distance to the next boundary plane in the primary */
/*           direction is */

/*              1.D0 - VTXOFF( IAXIS(1) ) */

	    if (raydir[(i__1 = iaxis[1] - 1) < 3 && 0 <= i__1 ? i__1 : s_rnge(
		    "raydir", i__1, "xdda_", (ftnlen)578)] > 0.) {

/*              The IAXIS(2) component of the ray's direction is */
/*              positive. The distance to the next boundary plane for */
/*              the that axis is */

/*                 1.D0 - VTXOFF(2) */

/*              The corresponding change along the primary axis is */

/*                 S12 * ( 1.D0 - VTXOFF(2) ) */

/*              The "error" term for IAXIS(2) is this value minus the */
/*              distance from the vertex to the next boundary in the */
/*              primary direction. */

		ax2err = s12 * (1. - vtxoff[1]) + vtxoff[0] - 1.;
	    } else {

/*              The IAXIS(2) component of the ray's direction is */
/*              negative. The distance to the next boundary plane for */
/*              the that axis is */

/*                 VTXOFF(2) */

/*              The corresponding change along the primary axis is */

/*                 S12 * VTXOFF(2) */

/*              The "error" term for IAXIS(2) is this value minus the */
/*              distance from the vertex to the next boundary in the */
/*              primary direction. */

		ax2err = s12 * vtxoff[1] + vtxoff[0] - 1.;
	    }
	} else {

/*           The primary component of the ray's direction is negative. */
/*           The distance to the next boundary plane in the primary */
/*           direction is */

/*              VTXOFF( IAXIS(1) ) */
	    if (raydir[(i__1 = iaxis[1] - 1) < 3 && 0 <= i__1 ? i__1 : s_rnge(
		    "raydir", i__1, "xdda_", (ftnlen)624)] > 0.) {

/*              The IAXIS(2) component of the ray's direction is */
/*              positive. The distance to the next boundary plane for */
/*              the that axis is */

/*                 1.D0 - VTXOFF(2) */

/*              The corresponding change along the primary axis is */

/*                 S12 * ( 1.D0 - VTXOFF(2) ) */

/*              The "error" term for IAXIS(2) is this value minus the */
/*              distance from the vertex to the next boundary in the */
/*              primary direction. */
		ax2err = s12 * (1. - vtxoff[1]) - vtxoff[0];
	    } else {

/*              The IAXIS(2) component of the ray's direction is */
/*              negative. The distance to the next boundary plane for */
/*              the that axis is */

/*                 VTXOFF(2) */

/*              The corresponding change along the primary axis is */

/*                 S12 * VTXOFF(2) */

/*              The "error" term for IAXIS(2) is this value minus the */
/*              distance from the vertex to the next boundary in the */
/*              primary direction. */
		ax2err = s12 * vtxoff[1] - vtxoff[0];
	    }
	}
    }

/*     Computations of AX3ERR are analogous to those of AX2ERR. */
/*     See the comments above. */

    if ((d__1 = raydir[(i__1 = iaxis[2] - 1) < 3 && 0 <= i__1 ? i__1 : s_rnge(
	    "raydir", i__1, "xdda_", (ftnlen)671)], abs(d__1)) > limit) {

/*        For any line segment along the ray, S13 is the ratio of the */
/*        magnitudes of the projections of the segment in the primary */
/*        and the IAXIS(3) directions. */

	s13 = (d__1 = raydir[(i__1 = iaxis[0] - 1) < 3 && 0 <= i__1 ? i__1 : 
		s_rnge("raydir", i__1, "xdda_", (ftnlen)677)] / raydir[(i__2 =
		 iaxis[2] - 1) < 3 && 0 <= i__2 ? i__2 : s_rnge("raydir", 
		i__2, "xdda_", (ftnlen)677)], abs(d__1));
	if (raydir[(i__1 = iaxis[0] - 1) < 3 && 0 <= i__1 ? i__1 : s_rnge(
		"raydir", i__1, "xdda_", (ftnlen)679)] > 0.) {
	    if (raydir[(i__1 = iaxis[2] - 1) < 3 && 0 <= i__1 ? i__1 : s_rnge(
		    "raydir", i__1, "xdda_", (ftnlen)681)] > 0.) {
		ax3err = s13 * (1. - vtxoff[2]) + vtxoff[0] - 1.;
	    } else {
		ax3err = s13 * vtxoff[2] + vtxoff[0] - 1.;
	    }
	} else {
	    if (raydir[(i__1 = iaxis[2] - 1) < 3 && 0 <= i__1 ? i__1 : s_rnge(
		    "raydir", i__1, "xdda_", (ftnlen)689)] > 0.) {
		ax3err = s13 * (1. - vtxoff[2]) - vtxoff[0];
	    } else {
		ax3err = s13 * vtxoff[2] - vtxoff[0];
	    }
	}
    }

/*     The "steps" set below are the amounts by which any voxel */
/*     coordinate changes when the "next" voxel is identified. Only one */
/*     coordinate changes at a time. The magnitude of each coordinate */
/*     step is always an integer. The signs of the steps are those of */
/*     the corresponding components of the ray's direction vector. */

/*     We treat direction components smaller than LIMIT as though */
/*     they were zero. Note that the IAXIS(1) component of the */
/*     ray will always have magnitude greater than LIMIT. */

    for (i__ = 1; i__ <= 3; ++i__) {
	if (raydir[(i__2 = iaxis[(i__1 = i__ - 1) < 3 && 0 <= i__1 ? i__1 : 
		s_rnge("iaxis", i__1, "xdda_", (ftnlen)712)] - 1) < 3 && 0 <= 
		i__2 ? i__2 : s_rnge("raydir", i__2, "xdda_", (ftnlen)712)] > 
		limit) {

/*           Positive component direction, positive step. */

	    step[(i__1 = i__ - 1) < 3 && 0 <= i__1 ? i__1 : s_rnge("step", 
		    i__1, "xdda_", (ftnlen)716)] = 1;
	} else if (raydir[(i__2 = iaxis[(i__1 = i__ - 1) < 3 && 0 <= i__1 ? 
		i__1 : s_rnge("iaxis", i__1, "xdda_", (ftnlen)718)] - 1) < 3 
		&& 0 <= i__2 ? i__2 : s_rnge("raydir", i__2, "xdda_", (ftnlen)
		718)] < -limit) {

/*           Negative component direction, negative step. */

	    step[(i__1 = i__ - 1) < 3 && 0 <= i__1 ? i__1 : s_rnge("step", 
		    i__1, "xdda_", (ftnlen)722)] = -1;
	} else {

/*           No component in this direction, no step. */

	    step[(i__1 = i__ - 1) < 3 && 0 <= i__1 ? i__1 : s_rnge("step", 
		    i__1, "xdda_", (ftnlen)728)] = 0;
	}
    }

/*     Follow the ray until it exits the voxel grid. */

    while(zzingrd_(grdext, &voxlst[*nvx * 3 - 3])) {
	if (ax2err < 0. || ax3err < 0.) {

/*           Ray has crossed over into the next voxel in IAXIS(2) or */
/*           IAXIS(3) */

	    if (ax2err < ax3err) {

/*              The boundary plane orthogonal to axis IAXIS(2) was hit. */

		icoord[1] += step[1];
		ax2err += s12;
		++(*nvx);
	    } else {

/*              The boundary plane orthogonal to axis IAXIS(3) was hit. */

		icoord[2] += step[2];
		ax3err += s13;
		++(*nvx);
	    }
	} else {

/*           No change in IAXIS(2) or IAXIS(3), step in IAXIS(1). */

	    icoord[0] += step[0];
	    ++(*nvx);
	    if (step[1] != 0) {
		ax2err += -1.;
	    }
	    if (step[2] != 0) {
		ax3err += -1.;
	    }
	}

/*        Check we have room in VOXLST. */

	if (*nvx > *maxnvx) {
	    chkin_("XDDA", (ftnlen)4);
	    setmsg_("Index larger than array. Index = #1. Array size = #2.", (
		    ftnlen)53);
	    errint_("#1", nvx, (ftnlen)2);
	    errint_("#2", maxnvx, (ftnlen)2);
	    sigerr_("SPICE(ARRAYTOOSMALL)", (ftnlen)20);
	    chkout_("XDDA", (ftnlen)4);
	    return 0;
	}

/*        Pack the voxel indices into VOXLST using */
/*        the values calculated in this loop pass. */

	for (i__ = 1; i__ <= 3; ++i__) {
	    voxlst[iaxis[(i__1 = i__ - 1) < 3 && 0 <= i__1 ? i__1 : s_rnge(
		    "iaxis", i__1, "xdda_", (ftnlen)801)] + *nvx * 3 - 4] = 
		    icoord[(i__2 = i__ - 1) < 3 && 0 <= i__2 ? i__2 : s_rnge(
		    "icoord", i__2, "xdda_", (ftnlen)801)];
	}
    }

/*     Subtract one off the voxel count since the final voxel */
/*     exists outside the grid. */

    --(*nvx);
    return 0;
} /* xdda_ */

