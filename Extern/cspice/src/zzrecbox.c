/* zzrecbox.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure ZZRECBOX (Bounding box for rectangular volume element) */
/* Subroutine */ int zzrecbox_(doublereal *bounds, doublereal *center, 
	doublereal *lx, doublereal *ly, doublereal *lz, doublereal *radius)
{
    /* System generated locals */
    integer i__1, i__2, i__3;
    doublereal d__1, d__2, d__3;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer);

    /* Local variables */
    doublereal diag[3];
    integer i__;
    doublereal l[3];
    extern /* Subroutine */ int chkin_(char *, ftnlen), vpack_(doublereal *, 
	    doublereal *, doublereal *, doublereal *), errdp_(char *, 
	    doublereal *, ftnlen);
    extern doublereal vnorm_(doublereal *);
    doublereal mincor[3], maxcor[3];
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), setmsg_(char *, ftnlen), errint_(char *, integer *, 
	    ftnlen);
    extern logical return_(void);

/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines. Users should not call this routine directly due to the */
/*     volatile nature of this routine. */

/*     Compute a bounding box center and radius for a rectangular volume */
/*     element. */

/*     The outputs are the box's center, dimensions in the X, Y, and Z */
/*     directions, and the box's radius. The box itself coincides with */
/*     the input box. */

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
/*     MATH */

/* $ Declarations */
/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     BOUNDS     I   Array of coordinate bounds for segment. */
/*     CENTER     O   Center of bounding box. */
/*     LX         O   Extent of box in the X direction. */
/*     LY         O   Extent of box in the Y direction. */
/*     LZ         O   Extent of box in the Z direction. */
/*     RADIUS     O   Radius of box. */

/* $ Detailed_Input */

/*     BOUNDS     is a 2 x 3 double precision array containing bounds */
/*                on the rectangular coordinates of the spatial region */
/*                (volume element) covered by a DSK segment. The */
/*                contents of BOUNDS are: */

/*                   BOUNDS(*,1)              X bounds */
/*                   BOUNDS(*,2)              Y bounds */
/*                   BOUNDS(*,3)              Z bounds */

/*                Elements (1,*) are lower bounds; elements (2,*) are */
/*                upper bounds. */

/* $ Detailed_Output */

/*     CENTER     is a double precision 3-vector representing the center */
/*                of the volume specified by BOUNDS. */

/*     LR, */
/*     LT, */
/*     LZ         are, respectively, the extents (edge lengths) of the */
/*                bounding box in the radial, tangential, and Z */
/*                directions. */

/*     RADIUS     is the radius of the sphere that circumscribes the */
/*                box. RADIUS is equal to the length of a line segment */
/*                connecting the center of the box to any corner. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1) If the minimum value of of any coordinate exceeds the maximum, */
/*        the error SPICE(BOUNDSOUTOFORDER) is signaled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine supports the DSK ray-surface intercept segment */
/*     selection algorithms. */

/* $ Examples */

/*     None. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman   (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 04-JUN-2016 (NJB) */

/* -& */
/* $ Index_Entries */

/*     bounding box radius for rectangular volume element */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     Element boundary indices: */


/*     Local variables */


/*     This routine uses discovery check-in. We check RETURN in order to */
/*     avoid performing math operations using invalid operands. */

    if (return_()) {
	return 0;
    }

/*     Get local copies of the bounds of the volume element. */

    for (i__ = 1; i__ <= 3; ++i__) {
	mincor[(i__1 = i__ - 1) < 3 && 0 <= i__1 ? i__1 : s_rnge("mincor", 
		i__1, "zzrecbox_", (ftnlen)191)] = bounds[(i__2 = (i__ << 1) 
		- 2) < 6 && 0 <= i__2 ? i__2 : s_rnge("bounds", i__2, "zzrec"
		"box_", (ftnlen)191)];
	maxcor[(i__1 = i__ - 1) < 3 && 0 <= i__1 ? i__1 : s_rnge("maxcor", 
		i__1, "zzrecbox_", (ftnlen)192)] = bounds[(i__2 = (i__ << 1) 
		- 1) < 6 && 0 <= i__2 ? i__2 : s_rnge("bounds", i__2, "zzrec"
		"box_", (ftnlen)192)];
	l[(i__1 = i__ - 1) < 3 && 0 <= i__1 ? i__1 : s_rnge("l", i__1, "zzre"
		"cbox_", (ftnlen)193)] = maxcor[(i__2 = i__ - 1) < 3 && 0 <= 
		i__2 ? i__2 : s_rnge("maxcor", i__2, "zzrecbox_", (ftnlen)193)
		] - mincor[(i__3 = i__ - 1) < 3 && 0 <= i__3 ? i__3 : s_rnge(
		"mincor", i__3, "zzrecbox_", (ftnlen)193)];
	if (l[(i__1 = i__ - 1) < 3 && 0 <= i__1 ? i__1 : s_rnge("l", i__1, 
		"zzrecbox_", (ftnlen)195)] <= 0.) {
	    chkin_("ZZRECBOX", (ftnlen)8);
	    setmsg_("Coordinate # bounds were #:#; bounds must be strictly i"
		    "ncreasing.", (ftnlen)65);
	    errint_("#", &i__, (ftnlen)1);
	    errdp_("#", &mincor[(i__1 = i__ - 1) < 3 && 0 <= i__1 ? i__1 : 
		    s_rnge("mincor", i__1, "zzrecbox_", (ftnlen)201)], (
		    ftnlen)1);
	    errdp_("#", &maxcor[(i__1 = i__ - 1) < 3 && 0 <= i__1 ? i__1 : 
		    s_rnge("maxcor", i__1, "zzrecbox_", (ftnlen)202)], (
		    ftnlen)1);
	    sigerr_("SPICE(BOUNDSOUTOFORDER)", (ftnlen)23);
	    chkout_("ZZRECBOX", (ftnlen)8);
	    return 0;
	}
    }

/*     Set output box dimensions. */

    *lx = l[0];
    *ly = l[1];
    *lz = l[2];

/*     Compute the coordinates of the center of the box. */

    for (i__ = 1; i__ <= 3; ++i__) {
	center[(i__1 = i__ - 1) < 3 && 0 <= i__1 ? i__1 : s_rnge("center", 
		i__1, "zzrecbox_", (ftnlen)222)] = mincor[(i__2 = i__ - 1) < 
		3 && 0 <= i__2 ? i__2 : s_rnge("mincor", i__2, "zzrecbox_", (
		ftnlen)222)] + l[(i__3 = i__ - 1) < 3 && 0 <= i__3 ? i__3 : 
		s_rnge("l", i__3, "zzrecbox_", (ftnlen)222)] / 2;
    }

/*     The radius is the distance from the center of the box */
/*     to any corner. */

    d__1 = *lx / 2;
    d__2 = *ly / 2;
    d__3 = *lz / 2;
    vpack_(&d__1, &d__2, &d__3, diag);
    *radius = vnorm_(diag);
    return 0;
} /* zzrecbox_ */

