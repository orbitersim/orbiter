/* zzinrec.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure ZZINREC ( DSK, in rectangular element? ) */
/* Subroutine */ int zzinrec_(doublereal *p, doublereal *bounds, doublereal *
	margin, integer *exclud, logical *inside)
{
    /* System generated locals */
    integer i__1, i__2, i__3, i__4;
    doublereal d__1;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer);

    /* Local variables */
    integer i__;
    doublereal l[3], delta[3];
    extern /* Subroutine */ int chkin_(char *, ftnlen), errdp_(char *, 
	    doublereal *, ftnlen);
    doublereal amncor[3], mincor[3], amxcor[3], maxcor[3];
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), setmsg_(char *, ftnlen), errint_(char *, integer *, 
	    ftnlen);
    extern logical return_(void);

/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines. Users should not call this routine directly due to the */
/*     volatile nature of this routine. */

/*     Test a point represented by a set of rectangular coordinates for */
/*     inclusion in a specified rectangular volume element. The test is */
/*     performed using margins for the element. */

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
/*     GEOMETRY */
/*     INTERSECTION */
/*     SURFACE */
/*     TOPOGRAPHY */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     P          I   Input point. */
/*     BOUNDS     I   Coordinate bounds of element. */
/*     MARGIN     I   Element expansion margin. */
/*     EXCLUD     I   Index of coordinate to exclude from test. */
/*     INSIDE     O   Flag indicating whether point is in element. */
/*     NONE       P   Meaning: exclude nothing. */

/* $ Detailed_Input */

/*     P          is a point expressed in Cartesian coordinates. The */
/*                point is to be checked to determine whether it is */
/*                inside the volume element specified by BOUNDS. */


/*     BOUNDS     is an 2x3 array containing the bounds of a volume */
/*                element expressed in rectangular coordinates. BOUNDS */
/*                defines the volume element used in the comparison. In */
/*                the element */

/*                   BOUNDS(I,J) */

/*                J is the coordinate index. J is one of */

/*                   { 1, 2, 3 } */

/*                I is the bound index. */

/*                   I = 1   ->   lower bound */
/*                   I = 2   ->   upper bound */


/*     MARGIN     is a scale factor used to expand the volume element */
/*                (box) described by the input bounds. Each side of the */
/*                element is scaled by MARGIN, and the coordinates of */
/*                the corners of the element are moved outward by the */
/*                resulting distance to create an expanded element. The */
/*                input point is tested for containment within this */
/*                expanded element. */


/*     EXCLUD     is either a coordinate index or the parameter NONE. */

/*                If EXCLUD is set to one of */

/*                   { 1, 2, 3 } */

/*                then the indicated coordinate is excluded from */
/*                comparison with the corresponding volume element */
/*                boundaries. */

/*                If EXCLUD is set to NONE, all coordinates are */
/*                compared. */

/*                Exclusion of coordinates is used in cases where a */
/*                point is known to be on a level surface of a given */
/*                coordinate. For example, if a point is on the plane */
/*                equal to the upper Z bound, the point's Z coordinate */
/*                need not be used in the comparison and in fact can't */
/*                be meaningfully compared, due to round-off errors. */

/* $ Detailed_Output */

/*     INSIDE     is a logical flag that is set to .TRUE. if and */
/*                only if the input coordinates represent a */
/*                point inside or on the surface of the volume */
/*                element, according to the comparisons that are */
/*                performed. */

/*                The value of INSIDE is not affected by the value */
/*                of any excluded coordinate. */

/* $ Parameters */

/*     NONE       when used as a value of the input argument EXCLUD, */
/*                indicates that no coordinates should be excluded from */
/*                comparison. */

/* $ Exceptions */

/*     1)  If MARGIN is negative, the error SPICE(VALUEOUTOFRANGE) */
/*         is signaled. */

/*     2) If EXCLUD is less than NONE or greater than 3, the error */
/*        SPICE(INDEXOUTOFRANGE) is signaled. */

/*     3) If any coordinate upper bound is less than the corresponding */
/*        lower bound, the error SPICE(BOUNDSOUTOFORDER) is signaled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     None. */

/* $ Examples */

/*     See usage in ZZRYTREC. */

/* $ Restrictions */

/*     This is a private routine. It is meant to be used only by the DSK */
/*     subsystem. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman    (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 16-MAY-2016 (NJB) */

/*        Original version 03-OCT-2014 (NJB) */

/* -& */
/* $ Index_Entries */

/*     test point against rectangular element using margin */

/* -& */

/*     SPICELIB functions */


/*     Element boundary indices: */


/*     Local variables */


/*     Check-in is discovery style. */

    if (return_()) {
	return 0;
    }

/*     Assume the point is outside to start. This allows us */
/*     to skip setting INSIDE when we find a boundary test */
/*     failure. */

    *inside = FALSE_;

/*     Reject negative margins. */

    if (*margin < 0.) {
	chkin_("ZZINREC", (ftnlen)7);
	setmsg_("Margin must be non-negative but was #.", (ftnlen)38);
	errdp_("#", margin, (ftnlen)1);
	sigerr_("SPICE(VALUEOUTOFRANGE)", (ftnlen)22);
	chkout_("ZZINREC", (ftnlen)7);
	return 0;
    }

/*     Check the exclusion index. */

    if (*exclud < 0 || *exclud > 3) {
	chkin_("ZZINREC", (ftnlen)7);
	setmsg_("EXCLUD was #; allowed range is 0:3.", (ftnlen)35);
	errint_("#", exclud, (ftnlen)1);
	sigerr_("SPICE(INDEXOUTOFRANGE)", (ftnlen)22);
	chkout_("ZZINREC", (ftnlen)7);
	return 0;
    }

/*     Get local copies of the coordinate bounds. */

    for (i__ = 1; i__ <= 3; ++i__) {
	mincor[(i__1 = i__ - 1) < 3 && 0 <= i__1 ? i__1 : s_rnge("mincor", 
		i__1, "zzinrec_", (ftnlen)272)] = bounds[(i__2 = (i__ << 1) - 
		2) < 6 && 0 <= i__2 ? i__2 : s_rnge("bounds", i__2, "zzinrec_"
		, (ftnlen)272)];
	maxcor[(i__1 = i__ - 1) < 3 && 0 <= i__1 ? i__1 : s_rnge("maxcor", 
		i__1, "zzinrec_", (ftnlen)273)] = bounds[(i__2 = (i__ << 1) - 
		1) < 6 && 0 <= i__2 ? i__2 : s_rnge("bounds", i__2, "zzinrec_"
		, (ftnlen)273)];
	l[(i__1 = i__ - 1) < 3 && 0 <= i__1 ? i__1 : s_rnge("l", i__1, "zzin"
		"rec_", (ftnlen)274)] = maxcor[(i__2 = i__ - 1) < 3 && 0 <= 
		i__2 ? i__2 : s_rnge("maxcor", i__2, "zzinrec_", (ftnlen)274)]
		 - mincor[(i__3 = i__ - 1) < 3 && 0 <= i__3 ? i__3 : s_rnge(
		"mincor", i__3, "zzinrec_", (ftnlen)274)];
	if (l[(i__1 = i__ - 1) < 3 && 0 <= i__1 ? i__1 : s_rnge("l", i__1, 
		"zzinrec_", (ftnlen)276)] < 0.) {
	    chkin_("ZZINREC", (ftnlen)7);
	    setmsg_("Bounds are out of order for index #; bounds are #:#.", (
		    ftnlen)52);
	    errdp_("#", &bounds[(i__1 = (i__ << 1) - 2) < 6 && 0 <= i__1 ? 
		    i__1 : s_rnge("bounds", i__1, "zzinrec_", (ftnlen)281)], (
		    ftnlen)1);
	    errdp_("#", &bounds[(i__1 = (i__ << 1) - 1) < 6 && 0 <= i__1 ? 
		    i__1 : s_rnge("bounds", i__1, "zzinrec_", (ftnlen)282)], (
		    ftnlen)1);
	    sigerr_("SPICE(BOUNDSOUTOFORDER)", (ftnlen)23);
	    chkout_("ZZINREC", (ftnlen)7);
	    return 0;
	}
    }

/*     Compare coordinates to adjusted coordinate */
/*     boundaries. */

    for (i__ = 1; i__ <= 3; ++i__) {
	if (*exclud != i__) {

/*           Create adjusted bounds for the Ith coordinate. */

	    delta[(i__1 = i__ - 1) < 3 && 0 <= i__1 ? i__1 : s_rnge("delta", 
		    i__1, "zzinrec_", (ftnlen)301)] = *margin * (d__1 = l[(
		    i__2 = i__ - 1) < 3 && 0 <= i__2 ? i__2 : s_rnge("l", 
		    i__2, "zzinrec_", (ftnlen)301)], abs(d__1));
	    amncor[(i__1 = i__ - 1) < 3 && 0 <= i__1 ? i__1 : s_rnge("amncor",
		     i__1, "zzinrec_", (ftnlen)303)] = mincor[(i__2 = i__ - 1)
		     < 3 && 0 <= i__2 ? i__2 : s_rnge("mincor", i__2, "zzinr"
		    "ec_", (ftnlen)303)] - delta[(i__3 = i__ - 1) < 3 && 0 <= 
		    i__3 ? i__3 : s_rnge("delta", i__3, "zzinrec_", (ftnlen)
		    303)];
	    amxcor[(i__1 = i__ - 1) < 3 && 0 <= i__1 ? i__1 : s_rnge("amxcor",
		     i__1, "zzinrec_", (ftnlen)304)] = maxcor[(i__2 = i__ - 1)
		     < 3 && 0 <= i__2 ? i__2 : s_rnge("maxcor", i__2, "zzinr"
		    "ec_", (ftnlen)304)] + delta[(i__3 = i__ - 1) < 3 && 0 <= 
		    i__3 ? i__3 : s_rnge("delta", i__3, "zzinrec_", (ftnlen)
		    304)];
	    if (p[(i__1 = i__ - 1) < 3 && 0 <= i__1 ? i__1 : s_rnge("p", i__1,
		     "zzinrec_", (ftnlen)306)] < amncor[(i__2 = i__ - 1) < 3 
		    && 0 <= i__2 ? i__2 : s_rnge("amncor", i__2, "zzinrec_", (
		    ftnlen)306)] || p[(i__3 = i__ - 1) < 3 && 0 <= i__3 ? 
		    i__3 : s_rnge("p", i__3, "zzinrec_", (ftnlen)306)] > 
		    amxcor[(i__4 = i__ - 1) < 3 && 0 <= i__4 ? i__4 : s_rnge(
		    "amxcor", i__4, "zzinrec_", (ftnlen)306)]) {
		return 0;
	    }
	}
    }

/*     All tests that were commanded have been passed. The input */
/*     point is considered to be contained in the expanded volume */
/*     element. */

    *inside = TRUE_;
    return 0;
} /* zzinrec_ */

