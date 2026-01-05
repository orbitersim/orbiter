/* zzfovaxi.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__3 = 3;

/* $Procedure   ZZFOVAXI ( Generate an axis vector for polygonal FOV ) */
/* Subroutine */ int zzfovaxi_(char *inst, integer *n, doublereal *bounds, 
	doublereal *axis, ftnlen inst_len)
{
    /* System generated locals */
    integer bounds_dim2, i__1, i__2, i__3;
    doublereal d__1;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer);

    /* Local variables */
    extern /* Subroutine */ int vadd_(doublereal *, doublereal *, doublereal *
	    );
    doublereal uvec[3];
    extern /* Subroutine */ int vhat_(doublereal *, doublereal *);
    extern doublereal vsep_(doublereal *, doublereal *);
    integer next;
    extern /* Subroutine */ int vequ_(doublereal *, doublereal *), zzhullax_(
	    char *, integer *, doublereal *, doublereal *, ftnlen);
    integer i__;
    doublereal v[3];
    extern /* Subroutine */ int chkin_(char *, ftnlen), errch_(char *, char *,
	     ftnlen, ftnlen);
    doublereal limit;
    extern /* Subroutine */ int vcrss_(doublereal *, doublereal *, doublereal 
	    *);
    extern logical vzero_(doublereal *);
    doublereal cp[3];
    extern logical failed_(void);
    logical ok;
    extern /* Subroutine */ int cleard_(integer *, doublereal *);
    extern doublereal halfpi_(void);
    extern /* Subroutine */ int sigerr_(char *, ftnlen), vhatip_(doublereal *)
	    , chkout_(char *, ftnlen), vsclip_(doublereal *, doublereal *), 
	    setmsg_(char *, ftnlen), errint_(char *, integer *, ftnlen);
    extern logical return_(void);
    doublereal sep;

/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines.  Users should not call this routine directly due */
/*     to the volatile nature of this routine. */

/*     Generate an axis of an instrument's polygonal FOV such that all */
/*     of the FOV's boundary vectors have angular separation of strictly */
/*     less than pi/2 radians from this axis. */

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

/*     CK */
/*     FRAMES */
/*     GF */
/*     IK */
/*     KERNEL */

/* $ Keywords */

/*     FOV */
/*     GEOMETRY */
/*     INSTRUMENT */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     MARGIN     P   Minimum complement of FOV cone angle. */
/*     INST       I   Instrument name. */
/*     N          I   Number of FOV boundary vectors. */
/*     BOUNDS     I   FOV boundary vectors. */
/*     AXIS       O   Instrument FOV axis vector. */

/* $ Detailed_Input */

/*     INST       is the name of an instrument with which the field of */
/*                view (FOV) of interest is associated. This name is */
/*                used only to generate long error messages. */

/*     N          is the number of boundary vectors in the array */
/*                BOUNDS. */

/*     BOUNDS     is an array of N vectors emanating from a common */
/*                vertex and defining the edges of a pyramidal region in */
/*                three-dimensional space: this the region within the */
/*                FOV of the instrument designated by INST. The Ith */
/*                vector of BOUNDS resides in elements (1:3,I) of this */
/*                array. */

/*                The vectors contained in BOUNDS are called the */
/*                "boundary vectors" of the FOV. */

/*                The boundary vectors must satisfy the constraints: */

/*                   1)  The boundary vectors must be contained within */
/*                       a right circular cone of angular radius less */
/*                       than than (pi/2) - MARGIN radians; in other */
/*                       words, there must be a vector A such that all */
/*                       boundary vectors have angular separation from */
/*                       A of less than (pi/2)-MARGIN radians. */

/*                   2)  There must be a pair of vectors U, V in BOUNDS */
/*                       such that all other boundary vectors lie in */
/*                       the same half space bounded by the plane */
/*                       containing U and V. Furthermore, all other */
/*                       boundary vectors must have orthogonal */
/*                       projections onto a plane normal to this plane */
/*                       such that the projections have angular */
/*                       separation of at least 2*MARGIN radians from */
/*                       the plane spanned by U and V. */

/*                Given the first constraint above, there is plane PL */
/*                such that each of the set of rays extending the */
/*                boundary vectors intersects PL. (In fact, there is an */
/*                infinite set of such planes.) The boundary vectors */
/*                must be ordered so that the set of line segments */
/*                connecting the intercept on PL of the ray extending */
/*                the Ith vector to that of the (I+1)st, with the Nth */
/*                intercept connected to the first, form a polygon (the */
/*                "FOV polygon") constituting the intersection of the */
/*                FOV pyramid with PL. This polygon may wrap in either */
/*                the positive or negative sense about a ray emanating */
/*                from the FOV vertex and passing through the plane */
/*                region bounded by the FOV polygon. */

/*                The FOV polygon need not be convex; it may be */
/*                self-intersecting as well. */

/*                No pair of consecutive vectors in BOUNDS may be */
/*                linearly dependent. */

/*                The boundary vectors need not have unit length. */


/* $ Detailed_Output */

/*     AXIS       is a unit vector normal to a plane containing the */
/*                FOV polygon. All boundary vectors have angular */
/*                separation from AXIS of not more than */

/*                   ( pi/2 ) - MARGIN */

/*                radians. */

/*                This routine signals an error if it cannot find */
/*                a satisfactory value of AXIS. */

/* $ Parameters */

/*     MARGIN     is a small positive number used to constrain the */
/*                orientation of the boundary vectors. See the two */
/*                constraints described in the Detailed_Input section */
/*                above for specifics. */

/* $ Exceptions */

/*     1)  In the input vector count N is not at least 3, the error */
/*         SPICE(INVALIDCOUNT) is signaled. */

/*     2)  If any pair of consecutive boundary vectors has cross */
/*         product zero, the error SPICE(DEGENERATECASE) is signaled. */
/*         For this test, the first vector is considered the successor */
/*         of the Nth. */

/*     3)  If this routine can't find a face of the convex hull of */
/*         the set of boundary vectors such that this face satisfies */
/*         constraint (2) of the Detailed_Input section above, the */
/*         error SPICE(FACENOTFOUND) is signaled. */

/*     4)  If any boundary vectors have longitude too close to 0 */
/*         or too close to pi radians in the face frame (see discussion */
/*         of the search algorithm's steps 3 and 4 in Particulars */
/*         below), the respective errors SPICE(NOTSUPPORTED) or */
/*         SPICE(FOVTOOWIDE) are signaled. */

/*     5)  If any boundary vectors have angular separation of more than */
/*         (pi/2)-MARGIN radians from the candidate FOV axis, the */
/*         error SPICE(FOVTOOWIDE) is signaled. */

/* $ Files */

/*     The boundary vectors input to this routine are typically */
/*     obtained from an IK file. */

/* $ Particulars */

/*     Normally implementation is not discussed in SPICE headers, but we */
/*     make an exception here because this routine's implementation and */
/*     specification are deeply intertwined. */

/*     This routine first computes the average of the unitized input */
/*     boundary vectors; if this vector satisfies the angular separation */
/*     constraint (1) in Detailed_Input, a unit length copy of this */
/*     vector is returned as the FOV axis. */

/*     If the procedure above fails, an algorithm based on selection */
/*     of a suitable face of the boundary vector's convex hull is tried. */
/*     See the routine ZZHULLAX for details. */

/*     If the second approach fails, an error is signaled. */

/*     Note that it's easy to construct FOVs where the average of the */
/*     boundary vectors doesn't yield a viable axis: a FOV of angular */
/*     width nearly equal to pi radians, with a sufficiently large */
/*     number of boundary vectors on one side and few boundary vectors */
/*     on the other, is one such example. This routine can find an */
/*     axis for many such intractable FOVs---that's why ZZHULLAX */
/*     is called after the simple approach fails. */

/* $ Examples */

/*     See SPICELIB private routine ZZGFFVIN. */

/* $ Restrictions */

/*     1) This is a SPICE private routine. User applications should not */
/*        call this routine. */

/*     2) There may "reasonable" polygonal FOVs that cannot be handled */
/*        by this routine. See the discussions in Detailed_Input, */
/*        Exceptions, and Particulars above for restrictions on the */
/*        input set of FOV boundary vectors. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman    (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 05-MAR-2009 (NJB) */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     Local variables */

    /* Parameter adjustments */
    bounds_dim2 = *n;

    /* Function Body */
    if (return_()) {
	return 0;
    }
    chkin_("ZZFOVAXI", (ftnlen)8);

/*     We must have at least 3 boundary vectors. */

    if (*n < 3) {
	setmsg_("Polygonal FOV requires at least 3 boundary vectors but numb"
		"er supplied for # was #.", (ftnlen)83);
	errch_("#", inst, (ftnlen)1, inst_len);
	errint_("#", n, (ftnlen)1);
	sigerr_("SPICE(INVALIDCOUNT)", (ftnlen)19);
	chkout_("ZZFOVAXI", (ftnlen)8);
	return 0;
    }

/*     Check for linearly dependent consecutive boundary vectors. */

    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {

/*        Set the index of the next ray. When we get to the */
/*        last boundary vector, the next ray is the first. */

	if (i__ == *n) {
	    next = 1;
	} else {
	    next = i__ + 1;
	}

/*        Find the cross product of the first ray with the */
/*        second. Depending on the ordering of the boundary */
/*        vectors, this could be an inward or outward normal, */
/*        in the case the current face is is exterior. */

	vcrss_(&bounds[(i__2 = i__ * 3 - 3) < bounds_dim2 * 3 && 0 <= i__2 ? 
		i__2 : s_rnge("bounds", i__2, "zzfovaxi_", (ftnlen)313)], &
		bounds[(i__3 = next * 3 - 3) < bounds_dim2 * 3 && 0 <= i__3 ? 
		i__3 : s_rnge("bounds", i__3, "zzfovaxi_", (ftnlen)313)], cp);

/*        We insist on consecutive boundary vectors being */
/*        linearly independent. */

	if (vzero_(cp)) {
	    setmsg_("Polygonal FOV must have linearly independent consecutiv"
		    "e boundary but vectors at indices # and # have cross pro"
		    "duct equal to the zero vector. Instrument is #.", (ftnlen)
		    158);
	    errint_("#", &i__, (ftnlen)1);
	    errint_("#", &next, (ftnlen)1);
	    errch_("#", inst, (ftnlen)1, inst_len);
	    sigerr_("SPICE(DEGENERATECASE)", (ftnlen)21);
	    chkout_("ZZFOVAXI", (ftnlen)8);
	    return 0;
	}
    }

/*     First try the average of the FOV unit boundary vectors as */
/*     a candidate axis. In many cases, this simple approach */
/*     does the trick. */

    cleard_(&c__3, axis);
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	vhat_(&bounds[(i__2 = i__ * 3 - 3) < bounds_dim2 * 3 && 0 <= i__2 ? 
		i__2 : s_rnge("bounds", i__2, "zzfovaxi_", (ftnlen)346)], 
		uvec);
	vadd_(uvec, axis, v);
	vequ_(v, axis);
    }
    d__1 = 1. / *n;
    vsclip_(&d__1, axis);

/*     If each boundary vector has sufficiently small */
/*     angular separation from AXIS, we're done. */

    limit = halfpi_() - 1e-12;
    ok = TRUE_;
    i__ = 1;
    while(i__ <= *n && ok) {
	sep = vsep_(&bounds[(i__1 = i__ * 3 - 3) < bounds_dim2 * 3 && 0 <= 
		i__1 ? i__1 : s_rnge("bounds", i__1, "zzfovaxi_", (ftnlen)365)
		], axis);
	if (sep > limit) {
	    ok = FALSE_;
	} else {
	    ++i__;
	}
    }
    if (! ok) {

/*        See whether we can find an axis using a */
/*        method based on finding a face of the convex */
/*        hull of the FOV. ZZHULLAX signals an error */
/*        if it doesn't succeed. */

	zzhullax_(inst, n, bounds, axis, inst_len);
	if (failed_()) {
	    chkout_("ZZFOVAXI", (ftnlen)8);
	    return 0;
	}
    }

/*     At this point AXIS is valid. Make the axis vector unit length. */

    vhatip_(axis);
    chkout_("ZZFOVAXI", (ftnlen)8);
    return 0;
} /* zzfovaxi_ */

