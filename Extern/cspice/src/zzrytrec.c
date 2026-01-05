/* zzrytrec.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__0 = 0;

/* $Procedure ZZRYTREC ( DSK, ray touches rectangular element ) */
/* Subroutine */ int zzrytrec_(doublereal *vertex, doublereal *raydir, 
	doublereal *bounds, doublereal *margin, integer *nxpts, doublereal *
	xpt)
{
    /* System generated locals */
    integer i__1, i__2, i__3;
    doublereal d__1;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer);

    /* Local variables */
    extern /* Subroutine */ int vequ_(doublereal *, doublereal *), zzraybox_(
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, logical *);
    integer i__;
    doublereal l[3], delta[3];
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    logical found;
    extern /* Subroutine */ int errdp_(char *, doublereal *, ftnlen);
    logical inside;
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen);
    doublereal boxori[3];
    extern /* Subroutine */ int setmsg_(char *, ftnlen), errint_(char *, 
	    integer *, ftnlen);
    doublereal extent[3];
    extern logical return_(void);
    extern /* Subroutine */ int zzinrec_(doublereal *, doublereal *, 
	    doublereal *, integer *, logical *);

/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines. Users should not call this routine directly due to the */
/*     volatile nature of this routine. */

/*     Find nearest intersection to ray's vertex of ray and */
/*     rectangular volume element. If the vertex is inside */
/*     the element, the vertex is considered to be the solution. */

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

/*     GEOMETRY */
/*     INTERCEPT */
/*     INTERSECTION */
/*     RAY */
/*     SURFACE */
/*     TOPOGRAPHY */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     VERTEX     I   Ray's vertex. */
/*     RAYDIR     I   Ray's direction vector. */
/*     BOUNDS     I   Bounds of rectangular volume element. */
/*     MARGIN     I   Margin used for element expansion. */
/*     NXPTS      O   Number of intercept points. */
/*     XPT        O   Intercept. */

/* $ Detailed_Input */

/*     VERTEX, */
/*     RAYDIR     are, respectively, the vertex and direction vector of */
/*                the ray to be used in the intercept computation. */

/*                Both the vertex and ray direction must be represented */
/*                in the reference frame of the segment to which the */
/*                volume element boundaries correspond. The vertex is */
/*                considered to be an offset from the center of the */
/*                reference frame associated with the segment. */

/*     BOUNDS     is a 2x3 array containing the bounds of a rectangular */
/*                volume element. Normally this is the coverage boundary */
/*                of a DSK segment. In the element */

/*                   BOUNDS(I,J) */

/*                J is the coordinate index. J is one of */

/*                   { 1, 2, 3 } */

/*                I is the bound index. */

/*                   I = 1   ->   lower bound */
/*                   I = 2   ->   upper bound */

/* $ Detailed_Output */

/*     XPT        is the intercept of the ray on the boundary of the */
/*                input volume element, if such an intercept exists. If */
/*                the ray's vertex is inside the element, XPT is set */
/*                equal to the vertex. XPT is valid if and only if FOUND */
/*                is .TRUE. */
/*                XPT is expressed in the reference frame associated */
/*                with the inputs VERTEX and RAYDIR. XPT represents */
/*                an offset from the origin of the coordinate system. */

/*                XPT is valid only if NXPTS is set to 1. */


/*     NXPTS      is the number of intercept points of the ray and */
/*                the volume element. */

/*                Currently there are only two possible values for */
/*                NXPTS: */

/*                   1 for an intersection */
/*                   0 for no intersection */

/*                If the vertex is inside the element, NXPTS is */
/*                set to 1. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1) If the minimum value of any coordinate exceeds the maximum, */
/*        the error will be signaled by a routine in the call tree of */
/*        this routine. */

/* $ Files */

/*     None. However, the input segment boundaries normally have */
/*     been obtained from a loaded DSK file. */

/* $ Particulars */

/*     This routine sits on top of data DSK type-specific ray-segment */
/*     intercept routines such as DSKX02. */

/* $ Examples */

/*     See usage in ZZDSKBUX. */

/* $ Restrictions */

/*     This is a private routine. It is meant to be used only by the DSK */
/*     subsystem. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman    (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 01-MAR-2016 (NJB) */

/* -& */
/* $ Index_Entries */

/*     find intercept of ray on rectangular volume element */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     Element boundary indices: */


/*     Local variables */

    if (return_()) {
	return 0;
    }

/*     Compute the original volume edge lengths from the coordinate */
/*     bounds. */

    for (i__ = 1; i__ <= 3; ++i__) {
	l[(i__1 = i__ - 1) < 3 && 0 <= i__1 ? i__1 : s_rnge("l", i__1, "zzry"
		"trec_", (ftnlen)224)] = bounds[(i__2 = (i__ << 1) - 1) < 6 && 
		0 <= i__2 ? i__2 : s_rnge("bounds", i__2, "zzrytrec_", (
		ftnlen)224)] - bounds[(i__3 = (i__ << 1) - 2) < 6 && 0 <= 
		i__3 ? i__3 : s_rnge("bounds", i__3, "zzrytrec_", (ftnlen)224)
		];
	if (l[(i__1 = i__ - 1) < 3 && 0 <= i__1 ? i__1 : s_rnge("l", i__1, 
		"zzrytrec_", (ftnlen)226)] <= 0.) {
	    chkin_("ZZRYTREC", (ftnlen)8);
	    setmsg_("Coordinate # bounds were #:#; bounds must be strictly i"
		    "ncreasing.", (ftnlen)65);
	    errint_("#", &i__, (ftnlen)1);
	    errdp_("#", &bounds[(i__1 = (i__ << 1) - 2) < 6 && 0 <= i__1 ? 
		    i__1 : s_rnge("bounds", i__1, "zzrytrec_", (ftnlen)232)], 
		    (ftnlen)1);
	    errdp_("#", &bounds[(i__1 = (i__ << 1) - 1) < 6 && 0 <= i__1 ? 
		    i__1 : s_rnge("bounds", i__1, "zzrytrec_", (ftnlen)233)], 
		    (ftnlen)1);
	    sigerr_("SPICE(BADCOORDBOUNDS)", (ftnlen)21);
	    chkout_("ZZRYTREC", (ftnlen)8);
	    return 0;
	}
    }

/*     Determine whether the vertex is inside the element. */
/*     Use double the margin for this test, since we don't */
/*     want to have false negative tests for rays having */
/*     vertices lying on the expanded element boundary. */

    *nxpts = 0;
    d__1 = *margin * 2;
    zzinrec_(vertex, bounds, &d__1, &c__0, &inside);
    if (inside) {

/*        We know the answer. */

	*nxpts = 1;
	vequ_(vertex, xpt);
	return 0;
    }

/*     Expand the box using the specified margin. */

    for (i__ = 1; i__ <= 3; ++i__) {
	delta[(i__1 = i__ - 1) < 3 && 0 <= i__1 ? i__1 : s_rnge("delta", i__1,
		 "zzrytrec_", (ftnlen)269)] = *margin * (d__1 = l[(i__2 = i__ 
		- 1) < 3 && 0 <= i__2 ? i__2 : s_rnge("l", i__2, "zzrytrec_", 
		(ftnlen)269)], abs(d__1));
	boxori[(i__1 = i__ - 1) < 3 && 0 <= i__1 ? i__1 : s_rnge("boxori", 
		i__1, "zzrytrec_", (ftnlen)271)] = bounds[(i__2 = (i__ << 1) 
		- 2) < 6 && 0 <= i__2 ? i__2 : s_rnge("bounds", i__2, "zzryt"
		"rec_", (ftnlen)271)] - delta[(i__3 = i__ - 1) < 3 && 0 <= 
		i__3 ? i__3 : s_rnge("delta", i__3, "zzrytrec_", (ftnlen)271)]
		;
	extent[(i__1 = i__ - 1) < 3 && 0 <= i__1 ? i__1 : s_rnge("extent", 
		i__1, "zzrytrec_", (ftnlen)273)] = l[(i__2 = i__ - 1) < 3 && 
		0 <= i__2 ? i__2 : s_rnge("l", i__2, "zzrytrec_", (ftnlen)273)
		] + delta[(i__3 = i__ - 1) < 3 && 0 <= i__3 ? i__3 : s_rnge(
		"delta", i__3, "zzrytrec_", (ftnlen)273)] * 2;
    }

/*     Find the ray-surface intercept on the expanded element, */
/*     if the intercept exists. */

    zzraybox_(vertex, raydir, boxori, extent, xpt, &found);
    if (found) {
	*nxpts = 1;
    }
    return 0;
} /* zzrytrec_ */

