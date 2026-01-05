/* zzocced.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static doublereal c_b50 = 1e-14;
static doublereal c_b51 = 1e-12;
static integer c__9 = 9;

/* $Procedure      ZZOCCED ( Occultation of ellipsoidal bodies ) */
integer zzocced_(doublereal *viewpt, doublereal *centr1, doublereal *semax1, 
	doublereal *centr2, doublereal *semax2)
{
    /* System generated locals */
    integer ret_val, i__1, i__2, i__3, i__4, i__5, i__6;
    doublereal d__1, d__2, d__3;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer);
    double atan2(doublereal, doublereal), cos(doublereal), sin(doublereal);

    /* Local variables */
    extern /* Subroutine */ int vadd_(doublereal *, doublereal *, doublereal *
	    );
    doublereal bigr, limb[9], dist[2], rmat[18]	/* was [3][3][2] */, view[3], 
	    ctrs[6]	/* was [3][2] */;
    extern doublereal vsep_(doublereal *, doublereal *);
    doublereal tilt;
    extern /* Subroutine */ int vsub_(doublereal *, doublereal *, doublereal *
	    ), vequ_(doublereal *, doublereal *);
    doublereal tpos[6]	/* was [3][2] */;
    extern /* Subroutine */ int mtxv_(doublereal *, doublereal *, doublereal *
	    );
    doublereal t2sep;
    extern /* Subroutine */ int zzasryel_(char *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, ftnlen);
    integer i__;
    doublereal r__[6]	/* was [3][2] */;
    integer s;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    doublereal level, xlimb[9];
    extern /* Subroutine */ int errdp_(char *, doublereal *, ftnlen);
    doublereal xasep, minpt[3], t12pos[3];
    extern doublereal vdist_(doublereal *, doublereal *);
    doublereal maxpt[3], xdist[2];
    extern /* Subroutine */ int xpose_(doublereal *, doublereal *), ucrss_(
	    doublereal *, doublereal *, doublereal *);
    extern logical isrot_(doublereal *, doublereal *, doublereal *);
    extern doublereal vnorm_(doublereal *);
    doublereal xview[3];
    extern /* Subroutine */ int unorm_(doublereal *, doublereal *, doublereal 
	    *), vprjp_(doublereal *, doublereal *, doublereal *);
    doublereal smlvu[3], xtpos[6]	/* was [3][2] */;
    extern /* Subroutine */ int el2cgv_(doublereal *, doublereal *, 
	    doublereal *, doublereal *), cgv2el_(doublereal *, doublereal *, 
	    doublereal *, doublereal *);
    extern logical failed_(void);
    doublereal t1opos[3];
    extern /* Subroutine */ int psv2pl_(doublereal *, doublereal *, 
	    doublereal *, doublereal *);
    extern doublereal pi_(void);
    extern /* Subroutine */ int cleard_(integer *, doublereal *), edlimb_(
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *);
    doublereal lmbmaj[3];
    extern doublereal dasine_(doublereal *, doublereal *), halfpi_(void);
    doublereal angcmp, majlen;
    integer bigidx;
    doublereal minang[2], bigctr[3], lplane[4], maxang[2], maxrad[2], lmbmin[
	    3], minrad[2], xr[9]	/* was [3][3] */, minvec[3], minlen, 
	    lmbctr[3], sclmat[9]	/* was [3][3] */, smlmaj[3];
    extern /* Subroutine */ int saelgv_(doublereal *, doublereal *, 
	    doublereal *, doublereal *);
    doublereal tmpmaj[3], raydir[3], minsep, smldir[3], maxsep, smlmat[9]	
	    /* was [3][3] */, smlmin[3], uasize, ubdist;
    integer frtidx;
    doublereal lnorml[3], smlctr[3], tmpmin[3], sclrot[9]	/* was [3][3] 
	    */, trgsep, invray[3], tmpctr[3];
    integer smlidx;
    doublereal ttdist;
    logical sfront;
    extern /* Subroutine */ int setmsg_(char *, ftnlen);
    extern logical return_(void);
    doublereal vpproj[3];
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), errint_(char *, integer *, ftnlen), vminus_(doublereal *,
	     doublereal *);
    doublereal xsmlvu[3], xvwtrg[3];
    extern doublereal det_(doublereal *);
    doublereal vph;
    extern /* Subroutine */ int mxm_(doublereal *, doublereal *, doublereal *)
	    , mxv_(doublereal *, doublereal *, doublereal *);

/* $ Abstract */

/*     Indicate whether one triaxial ellipsoid is occulted by another as */
/*     seen from a specified viewing location. */

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

/*     SPK */
/*     TIME */

/* $ Keywords */

/*     GEOMETRY */

/* $ Declarations */
/* $ Abstract */

/*     Declare ZZOCCED return code parameters, comparison strings */
/*     and other parameters. */

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

/*     GF */

/* $ Keywords */

/*     ELLIPSOID */
/*     GEOMETRY */
/*     GF */
/*     OCCULTATION */

/* $ Restrictions */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman      (JPL) */

/* $ Literature_References */

/*     None. */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 01-SEP-2005 (NJB) */

/* -& */
/*     The function returns an integer code indicating the geometric */
/*     relationship of the three bodies. */

/*     Codes and meanings are: */

/*        -3                    Total occultation of first target by */
/*                              second. */


/*        -2                    Annular occultation of first target by */
/*                              second.  The second target does not */
/*                              block the limb of the first. */


/*        -1                    Partial occultation of first target by */
/*                              second target. */


/*         0                    No occultation or transit:  both objects */
/*                              are completely visible to the observer. */


/*         1                    Partial occultation of second target by */
/*                              first target. */


/*         2                    Annular occultation of second target by */
/*                              first. */


/*         3                    Total occultation of second target by */
/*                              first. */


/*     End include file zzocced.inc */

/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     UBEL       P   Upper bound of SPICELIB ellipse data structure. */
/*     UBPL       P   Upper bound of SPICELIB plane data structure. */
/*     VIEWPT     I   Observation location. */
/*     CENTR1     I   Center of first ellipsoid. */
/*     SEMAX1     I   Semi-major axis matrix for first ellipsoid. */
/*     CENTR2     I   Center of second ellipsoid. */
/*     SEMAX2     I   Semi-major axis matrix for second ellipsoid. */

/*     The function returns an integer code indicating the geometric */
/*     relationship of the three bodies.  Negative codes indicate that */
/*     the first target is partially or fully occulted by the second; */
/*     positive codes indicate that the second target is partially */
/*     or fully occulted by the first; a value of zero indicates no */
/*     occultation. */

/*     See Detailed_Output for the list of codes and meanings. */

/* $ Detailed_Input */

/*     VIEWPT         is a point from which a possible occultation of */
/*                    one ellipsoidal "target" body by another is */
/*                    observed. VIEWPT must be external to both target */
/*                    bodies. */

/*     CENTR1         is the center of the first ellipsoidal target */
/*                    body. */

/*     SEMAX1         is a 3x3 matrix whose columns are semi-axis */
/*                    vectors of the first ellipsoid.  The columns of */
/*                    SEMAX1 must form a right-handed, orthogonal basis: */
/*                    the columns are mutually orthogonal, and the third */
/*                    column points in the direction of the cross */
/*                    product of the first and second.  In other words, */
/*                    if the columns were scaled to unit length, the */
/*                    matrix would be orthogonal. */

/*                    The lengths of the column vectors are the lengths */
/*                    of the ellipsoid's semi-axes.  It is not necessary */
/*                    that the longest semi-axis appear in the first */
/*                    column. */

/*                    An example:  if the first ellipsoid is described */
/*                    by the equation */

/*                         2       2       2 */
/*                        x       y       z */
/*                       ---  +  ---  +  ---   =   1 */
/*                         2       2       2 */
/*                        a       b       c */

/*                    then a corresponding semi-axis matrix would */
/*                    be */

/*                       +-          -+ */
/*                       |  a   0   0 | */
/*                       |  0   b   0 | */
/*                       |  0   0   c | */
/*                       +-          -+ */

/*                    A second example of a valid semi-axis matrix is */

/*                       +-          -+ */
/*                       |  0  -a   0 | */
/*                       |  0   0  -b | */
/*                       |  c   0   0 | */
/*                       +-          -+ */


/*     CENTR2         is the center of the second ellipsoidal target */
/*                    body. */


/*     SEMAX2         is a semi-axis matrix for the second ellipsoidal */
/*                    target body.  See the description of SEMAX1 for */
/*                    details. */


/* $ Detailed_Output */

/*     The function returns an integer code indicating the geometric */
/*     relationship of the three bodies. */

/*     Codes and meanings are: */

/*        TOTAL1                Total occultation of first target by */
/*                              second. */

/*        ANNLR1                Annular occultation of first target by */
/*                              second.  The second target does not */
/*                              block the limb of the first. */

/*        PARTL1                Partial occultation of first target by */
/*                              second target. */

/*        NOOCC                 No occultation or transit:  both objects */
/*                              are completely visible to the observer. */

/*        PARTL2                Partial occultation of second target by */
/*                              first target. */

/*        ANNLR2                Annular occultation of second target by */
/*                              first. */

/*        TOTAL2                Total occultation of second target by */
/*                              first. */

/* $ Parameters */

/*     UBEL           Upper bound of SPICELIB ellipse data structure. */

/*     UBPL           Upper bound of SPICELIB plane data structure. */

/* $ Exceptions */

/*     1)  If the observer is inside either target ellipsoid, the error */
/*         SPICE(NOTDISJOINT) is signaled. */

/*     2)  If this routine determines that the target bodies intersect, */
/*         the error SPICE(NOTDISJOINT) is signaled. */

/*     3)  If any of the semi-axis lengths of either ellipsoid is */
/*         non-positive, the error SPICE(BADAXISLENGTH) is signaled. */

/*     4)  If either semi-axis matrix does not have a right-handed, */
/*         mutually orthogonal set of columns, the error */
/*         SPICE(NOTAROTATION) will be signaled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     For many purposes, modeling extended bodies as tri-axial */
/*     ellipsoids is adequate for determining whether one body is */
/*     occulted by another as seen from a specified observer. */

/*     This routine may be used as a tool to support more higher-level */
/*     occultation tests involving ephemeris objects. */

/* $ Examples */

/*     1)  View a total occultation of one ellipsoid by another */
/*         as seen from a viewing location on the +x axis. */

/*                 PROGRAM EX1 */
/*                 IMPLICIT NONE */

/*           C */
/*           C     SPICELIB functions */
/*           C */
/*                 INTEGER ZZOCCED */

/*           C */
/*           C     Local variables */
/*           C */
/*                 DOUBLE PRECISION      CENTR1 ( 3 ) */
/*                 DOUBLE PRECISION      CENTR2 ( 3 ) */
/*                 DOUBLE PRECISION      SEMAX1 ( 3, 3 ) */
/*                 DOUBLE PRECISION      SEMAX2 ( 3, 3 ) */
/*                 DOUBLE PRECISION      VIEWPT ( 3 ) */

/*                 INTEGER               CODE */

/*           C */
/*           C     Initial values */
/*           C */
/*                 DATA VIEWPT /    2.D1, 0.D0, 0.D0 / */

/*                 DATA CENTR1 /    1.D1, 0.D0, 0.D0 / */

/*                 DATA SEMAX1 /    1.D0, 0.D0, 0.D0, */
/*                .                 0.D0, 5.D0, 0.D0, */
/*                .                 0.D0, 0.D0, 1.D1 / */

/*                 DATA CENTR2 /   -1.D1, 0.D0, 0.D0 / */

/*                 DATA SEMAX2 /    2.D0, 0.D0, 0.D0, */
/*                .                 0.D0, 1.D1, 0.D0, */
/*                .                 0.D0, 0.D0, 2.D1 / */

/*           C */
/*           C     Find the occultation state and write out the */
/*           C     occultation code.  We don't place the ZZOCCED */
/*           C     call directly in the WRITE statement because */
/*           C     ZZOCCED can signal errors; an error signaled in */
/*           C     an I/O statement would cause recursive I/O. */
/*           C */
/*                 CODE = ZZOCCED ( VIEWPT, CENTR1, SEMAX1, */
/*                .                         CENTR2, SEMAX2 ) */

/*                 WRITE (*,*), 'CODE = ', CODE */
/*                 END */

/*     We expect that the smaller ellipsoid, listed first in the call to */
/*     ZZOCCED, completely occults the larger, so the return code should */
/*     be 3. */


/* $ Restrictions */

/*     1) The test done by this routine for intersection of target bodies */
/*        may return a false negative result.  The test is based on */
/*        finding an intersection of spheres inscribed in each target */
/*        body. */

/*        Correct application code should never exercise this test. */

/*     2) This routine relies on ZZASRYEL to determine the minimum and */
/*        maximum angular separation of a specified ray and ellipse. In */
/*        some unusual cases in which multiple extreme values are very */
/*        close, or in which the extrema occur at points very close */
/*        together on the ellipse, ZZASRYEL may locate the incorrect */
/*        extremum.  This can result in erroneous classification of a */
/*        partial occultation as a total occultation or annular transit. */


/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman   (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 17-MAR-2006 (NJB) */

/*        Bug fixes: */

/*           - Test for intersection of viewpoint with targets */
/*             was corrected.  Previous test did not properly account */
/*             for target orientation. */

/*           - Computation of maximum bounding cones of targets */
/*             failed when viewing point was inside either maximum */
/*             bounding sphere.  The algorithm now has a separate */
/*             branch to handle this situation. */

/*           - Computation of minimum bounding cone for target was */
/*             incorrect for the computation done after transformation */
/*             of the targets.  This computation has been corrected. */

/* -    SPICELIB Version 1.0.0, 17-AUG-2005 (NJB) */

/* -& */
/* $ Index_Entries */

/*     occultation test using ellipsoidal bodies */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     Tolerance value for determinant of a rotation matrix.  The */
/*     determinant must differ from 1 by no more than DTOL. */


/*     Tolerance value for norms of columns of a rotation matrix.  The */
/*     norms must differ from 1 by no more than NTOL. */


/*     Tolerance value for argument of arcsine.  The argument should */
/*     have absolute value no greater than 1 + ATOL. */


/*     Local variables */


/*     Overview */
/*     ======================================================= */

/*     This routine starts out by initializing variables and */
/*     performing some error checks on the inputs. */

/*     The routine proceeds to classify the type occultation, */
/*     starting with simple approximation techniques, and if those */
/*     fail, following with more computationally expensive techniques. */

/*     All of the classifications have two elements: */

/*        - Determining the type of overlap: total occultation */
/*          or annular transit, partial occultation, or no */
/*          occultation. */

/*        - Determining which object is in front of the other */
/*          if an overlap exists. */

/*     For each classification, this routine sets the return code to */
/*     indicate the above attributes of the occultation geometry. */

/*     The first classification step is performed using "bounding */
/*     cones." For each ellipsoid, we define a "minimum bounding cone" */
/*     and a "maximum bounding cone."  A minimum bounding cone for an */
/*     ellipsoid has the viewing point as its vertex and is tangent to */
/*     the sphere whose radius is the ellipsoid's minimum semi-axis */
/*     length and whose center coincides with the ellipsoid's center. */

/*     A maximum bounding cone is defined analogously, with the sphere */
/*     having radius equal to the ellipsoid's maximum semi-axis length. */

/*     Since all of the bounding cones intersect in the viewing point, */
/*     it's inaccurate to speak of the cones as "not intersecting." */
/*     However, it's very convenient to ignore this intersection, so */
/*     we'll consider a pair of cones to intersect or "overlap" only if */
/*     they intersect in more than just their common vertex. */

/*     The conditions that can be determined by the initial bounding */
/*     cone tests are as follows: */

/*        1) The maximum bounding cones are disjoint.  This implies */
/*           there is no occultation. */

/*        2) The maximum bounding cone of one ellipsoid is contained */
/*           in the minimum bounding cone of the other.  This implies */
/*           there is a total occultation or annular transit. */

/*        3) The minimum bounding cones of the ellipsoids overlap, */
/*           and neither of these cones is contained in the maximum */
/*           bounding cone of the other ellipsoid.  This implies there */
/*           is a partial occultation. */

/*     If the occultation cannot be classified by the above tests, the */
/*     next step is to change the problem into an equivalent one in */
/*     which one of the ellipsoids is a sphere.  This new problem can be */
/*     attacked by considering the angular separation between the ray */
/*     from the viewing point to the center of the sphere and the limb */
/*     of the other ellipsoid. */

/*     To obtain this simplified geometric configuration, we apply to */
/*     all participating objects a non-singular linear transformation. */
/*     This transformation maps one of the ellipsoids to the unit sphere. */
/*     The viewing point, the center of the ellipsoid mapped to the */
/*     unit sphere, and the center and generating vectors of the limb */
/*     of the other ellipsoid are all subjected to this transformation. */
/*     The result is a collection of objects that yield the same */
/*     occultation state as the original set.  (The reader may want */
/*     to verify that limbs of ellipsoids map to limbs under this */
/*     transformation.) */

/*     The conditions that can be identified immediately using the */
/*     transformed objects are: */

/*        4)  The minimum angular separation between the ray from the */
/*            viewing point to the center of the unit sphere ("the ray" */
/*            henceforth) and the limb of the other ellipsoid is greater */
/*            than the angular radius (one half of the apparent angular */
/*            size as seen from the viewing point) of the unit sphere. */
/*            This implies there is no occultation. */

/*        5)  The minimum angular separation between the ray and the */
/*            limb of the other ellipsoid is negative (meaning the ray */
/*            penetrates the plane region bounded by the limb) and has */
/*            magnitude greater than the angular radius of the unit */
/*            sphere.  This implies the unit sphere is in total */
/*            occultation or in annular transit across the other */
/*            ellipsoid. */

/*     If both of the above tests fail, there is an occultation, but */
/*     it remains to be classified.  We do know at this point that the */
/*     unit sphere extends beyond the other ellipsoid, but we don't */
/*     know whether the other ellipsoid also extends beyond the unit */
/*     sphere.  If it does, we have a partial occultation; if it */
/*     doesn't, the other ellipsoid is totally occulted by the unit */
/*     sphere or is in annular transit across it. */

/*     At this point, we perform a second set of bounding cone tests. */
/*     The reason this may be useful is that the linear transformation */
/*     we've performed gives rise to a new set of bounding cones whose */
/*     containment relationships *are not* necessarily the same as those */
/*     of the original ellipsoids.  The conditions that can be */
/*     identified at this point by the bounding cone tests are: */

/*        6) The bounding cone of the unit sphere (the minimum and */
/*           maximum bounding cones are coincident) contains the maximum */
/*           bounding cone of the other ellipsoid.  This implies the */
/*           latter ellipsoid is in total occultation or annular */
/*           transit. */

/*        7) The bounding cone of the unit sphere does not contain */
/*           the minimum bounding cone of the other ellipsoid.  This */
/*           implies there is a partial occultation. */

/*     If these tests fail, the final step is to find the maximum */
/*     angular separation of the ray and the limb of the other */
/*     ellipsoid.  This separation is signed, with a negative sign */
/*     indicating that the ray penetrates the plane region bounded by */
/*     the limb.  The conditions we can determine using this information */
/*     are: */

/*        8) The maximum *magnitude* of the angular separation of the */
/*           limb and the ray is less than or equal to the angular size */
/*           of the unit sphere. This implies the other ellipsoid is in */
/*           total occultation or annular transit across the unit sphere. */

/*        9) The maximum *magnitude* of the angular separation of the */
/*           limb and the ray is greater than the angular size */
/*           of the unit sphere. This implies there is a partial */
/*           occultation. */




/*     Executable code */
/*     ======================================================= */

/*     Set an initial function value. */

    ret_val = 0;

/*     Standard SPICE error handling. */

    if (return_()) {
	return ret_val;
    }
    chkin_("ZZOCCED", (ftnlen)7);

/*     Extract the radii of the targets from the semi-axis vectors. */
/*     At the same time, create rotation matrices that map vectors */
/*     from the principal axis frames of the targets to the base frame. */

    for (i__ = 1; i__ <= 3; ++i__) {
	unorm_(&semax1[(i__1 = i__ * 3 - 3) < 9 && 0 <= i__1 ? i__1 : s_rnge(
		"semax1", i__1, "zzocced_", (ftnlen)587)], &rmat[(i__2 = (i__ 
		+ 3) * 3 - 12) < 18 && 0 <= i__2 ? i__2 : s_rnge("rmat", i__2,
		 "zzocced_", (ftnlen)587)], &r__[(i__3 = i__ - 1) < 6 && 0 <= 
		i__3 ? i__3 : s_rnge("r", i__3, "zzocced_", (ftnlen)587)]);
	unorm_(&semax2[(i__1 = i__ * 3 - 3) < 9 && 0 <= i__1 ? i__1 : s_rnge(
		"semax2", i__1, "zzocced_", (ftnlen)588)], &rmat[(i__2 = (i__ 
		+ 6) * 3 - 12) < 18 && 0 <= i__2 ? i__2 : s_rnge("rmat", i__2,
		 "zzocced_", (ftnlen)588)], &r__[(i__3 = i__ + 2) < 6 && 0 <= 
		i__3 ? i__3 : s_rnge("r", i__3, "zzocced_", (ftnlen)588)]);
    }

/*     Find the minimum and maximum radii of both targets. */

    for (i__ = 1; i__ <= 2; ++i__) {
/* Computing MIN */
	d__1 = r__[(i__2 = i__ * 3 - 3) < 6 && 0 <= i__2 ? i__2 : s_rnge(
		"r", i__2, "zzocced_", (ftnlen)596)], d__2 = r__[(i__3 = i__ *
		 3 - 2) < 6 && 0 <= i__3 ? i__3 : s_rnge("r", i__3, "zzocced_"
		, (ftnlen)596)], d__1 = min(d__1,d__2), d__2 = r__[(i__4 = 
		i__ * 3 - 1) < 6 && 0 <= i__4 ? i__4 : s_rnge("r", i__4, 
		"zzocced_", (ftnlen)596)];
	minrad[(i__1 = i__ - 1) < 2 && 0 <= i__1 ? i__1 : s_rnge("minrad", 
		i__1, "zzocced_", (ftnlen)596)] = min(d__1,d__2);
/* Computing MAX */
	d__1 = r__[(i__2 = i__ * 3 - 3) < 6 && 0 <= i__2 ? i__2 : s_rnge(
		"r", i__2, "zzocced_", (ftnlen)597)], d__2 = r__[(i__3 = i__ *
		 3 - 2) < 6 && 0 <= i__3 ? i__3 : s_rnge("r", i__3, "zzocced_"
		, (ftnlen)597)], d__1 = max(d__1,d__2), d__2 = r__[(i__4 = 
		i__ * 3 - 1) < 6 && 0 <= i__4 ? i__4 : s_rnge("r", i__4, 
		"zzocced_", (ftnlen)597)];
	maxrad[(i__1 = i__ - 1) < 2 && 0 <= i__1 ? i__1 : s_rnge("maxrad", 
		i__1, "zzocced_", (ftnlen)597)] = max(d__1,d__2);
    }

/*     Make sure the input target radii are positive.  We'll actually do */
/*     a more stringent test later, but we must prevent divide-by-zero */
/*     errors at this point. */

    if (minrad[0] <= 0. || minrad[1] <= 0.) {
	setmsg_("Minimum radii of bodies 1 and 2 are #, #. Target radii must"
		" be positive.", (ftnlen)72);
	errdp_("#", minrad, (ftnlen)1);
	errdp_("#", &minrad[1], (ftnlen)1);
	sigerr_("SPICE(BADAXISLENGTH)", (ftnlen)20);
	chkout_("ZZOCCED", (ftnlen)7);
	return ret_val;
    }

/*     Compute view point-to-target vectors and ranges for both */
/*     target bodies. */

    vequ_(centr1, ctrs);
    vequ_(centr2, &ctrs[3]);
    for (i__ = 1; i__ <= 2; ++i__) {
	vsub_(&ctrs[(i__1 = i__ * 3 - 3) < 6 && 0 <= i__1 ? i__1 : s_rnge(
		"ctrs", i__1, "zzocced_", (ftnlen)626)], viewpt, &tpos[(i__2 =
		 i__ * 3 - 3) < 6 && 0 <= i__2 ? i__2 : s_rnge("tpos", i__2, 
		"zzocced_", (ftnlen)626)]);
	dist[(i__1 = i__ - 1) < 2 && 0 <= i__1 ? i__1 : s_rnge("dist", i__1, 
		"zzocced_", (ftnlen)628)] = vnorm_(&tpos[(i__2 = i__ * 3 - 3) 
		< 6 && 0 <= i__2 ? i__2 : s_rnge("tpos", i__2, "zzocced_", (
		ftnlen)628)]);
	if (dist[(i__1 = i__ - 1) < 2 && 0 <= i__1 ? i__1 : s_rnge("dist", 
		i__1, "zzocced_", (ftnlen)631)] == 0.) {
	    setmsg_("Center of object # coincides with the viewing point.", (
		    ftnlen)52);
	    errint_("#", &i__, (ftnlen)1);
	    sigerr_("SPICE(NOTDISJOINT)", (ftnlen)18);
	    chkout_("ZZOCCED", (ftnlen)7);
	    return ret_val;
	}
    }

/*     Now check the semi-axis matrices.  We'll create new matrices */
/*     from these inputs by scaling the columns of each to unit length. */
/*     the resulting matrices are supposed to be rotations. */

    for (i__ = 1; i__ <= 2; ++i__) {
	if (! isrot_(&rmat[(i__1 = (i__ * 3 + 1) * 3 - 12) < 18 && 0 <= i__1 ?
		 i__1 : s_rnge("rmat", i__1, "zzocced_", (ftnlen)651)], &
		c_b50, &c_b51)) {
	    setmsg_("Matrix derived by unitizing columns of semi-axis matrix"
		    " SEMAX# is not a rotation matrix.  The determinant of th"
		    "is matrix is #.", (ftnlen)126);
	    errint_("#", &i__, (ftnlen)1);
	    d__1 = det_(&rmat[(i__1 = (i__ * 3 + 1) * 3 - 12) < 18 && 0 <= 
		    i__1 ? i__1 : s_rnge("rmat", i__1, "zzocced_", (ftnlen)
		    658)]);
	    errdp_("#", &d__1, (ftnlen)1);
	    sigerr_("SPICE(NOTAROTATION)", (ftnlen)19);
	    chkout_("ZZOCCED", (ftnlen)7);
	    return ret_val;
	}
    }

/*     Find the position of the second target relative to the first. */

    vsub_(&tpos[3], tpos, t12pos);
    ttdist = vnorm_(t12pos);

/*     Make sure the targets are non-intersecting. */

    if (ttdist <= minrad[0] + minrad[1]) {
	setmsg_("Targets must be non-intersecting, but  spheres inscribed in"
		" the targets intersect.", (ftnlen)82);
	sigerr_("SPICE(NOTDISJOINT)", (ftnlen)18);
	chkout_("ZZOCCED", (ftnlen)7);
	return ret_val;
    }

/*     Make sure that the viewing point is outside of both target */
/*     ellipsoids. */

    for (i__ = 1; i__ <= 2; ++i__) {

/*        Transform the Ith target position into the frame of the */
/*        Ith target. */

	mtxv_(&rmat[(i__1 = (i__ * 3 + 1) * 3 - 12) < 18 && 0 <= i__1 ? i__1 :
		 s_rnge("rmat", i__1, "zzocced_", (ftnlen)696)], &tpos[(i__2 =
		 i__ * 3 - 3) < 6 && 0 <= i__2 ? i__2 : s_rnge("tpos", i__2, 
		"zzocced_", (ftnlen)696)], &xtpos[(i__3 = i__ * 3 - 3) < 6 && 
		0 <= i__3 ? i__3 : s_rnge("xtpos", i__3, "zzocced_", (ftnlen)
		696)]);

/*        The viewpoint position is the negative of the target position. */
/*        Since we're squaring the terms involving the target position, */
/*        we omit the minus signs. */

/* Computing 2nd power */
	d__1 = xtpos[(i__1 = i__ * 3 - 3) < 6 && 0 <= i__1 ? i__1 : s_rnge(
		"xtpos", i__1, "zzocced_", (ftnlen)703)] / r__[(i__2 = i__ * 
		3 - 3) < 6 && 0 <= i__2 ? i__2 : s_rnge("r", i__2, "zzocced_",
		 (ftnlen)703)];
/* Computing 2nd power */
	d__2 = xtpos[(i__3 = i__ * 3 - 2) < 6 && 0 <= i__3 ? i__3 : s_rnge(
		"xtpos", i__3, "zzocced_", (ftnlen)703)] / r__[(i__4 = i__ * 
		3 - 2) < 6 && 0 <= i__4 ? i__4 : s_rnge("r", i__4, "zzocced_",
		 (ftnlen)703)];
/* Computing 2nd power */
	d__3 = xtpos[(i__5 = i__ * 3 - 1) < 6 && 0 <= i__5 ? i__5 : s_rnge(
		"xtpos", i__5, "zzocced_", (ftnlen)703)] / r__[(i__6 = i__ * 
		3 - 1) < 6 && 0 <= i__6 ? i__6 : s_rnge("r", i__6, "zzocced_",
		 (ftnlen)703)];
	level = d__1 * d__1 + d__2 * d__2 + d__3 * d__3;
	if (level < 1.) {
	    setmsg_("Viewpoint is inside target #; level surface parameter ="
		    " #.", (ftnlen)58);
	    errint_("#", &i__, (ftnlen)1);
	    errdp_("#", &level, (ftnlen)1);
	    sigerr_("SPICE(NOTDISJOINT)", (ftnlen)18);
	    chkout_("ZZOCCED", (ftnlen)7);
	    return ret_val;
	}
    }

/*     Find the minimum and maximum angular radii of both targets.  Note */
/*     that the distances used as denominators are guaranteed to be */
/*     positive at this point. */

    for (i__ = 1; i__ <= 2; ++i__) {
	d__1 = minrad[(i__2 = i__ - 1) < 2 && 0 <= i__2 ? i__2 : s_rnge("min"
		"rad", i__2, "zzocced_", (ftnlen)728)] / dist[(i__3 = i__ - 1) 
		< 2 && 0 <= i__3 ? i__3 : s_rnge("dist", i__3, "zzocced_", (
		ftnlen)728)];
	minang[(i__1 = i__ - 1) < 2 && 0 <= i__1 ? i__1 : s_rnge("minang", 
		i__1, "zzocced_", (ftnlen)728)] = dasine_(&d__1, &c_b51);
	if (failed_()) {
	    chkout_("ZZOCCED", (ftnlen)7);
	    return ret_val;
	}

/*        The situation is a bit more complicated for the maximum */
/*        bounding sphere, because the observer can be outside both */
/*        ellipsoids but inside one or both maximum bounding spheres. */
/*        We handle that special case separately. */

	if (dist[(i__1 = i__ - 1) < 2 && 0 <= i__1 ? i__1 : s_rnge("dist", 
		i__1, "zzocced_", (ftnlen)741)] >= maxrad[(i__2 = i__ - 1) < 
		2 && 0 <= i__2 ? i__2 : s_rnge("maxrad", i__2, "zzocced_", (
		ftnlen)741)]) {

/*           The viewing point is outside the sphere; we use the sphere */
/*           to define the maximum angular radius. */

	    d__1 = maxrad[(i__2 = i__ - 1) < 2 && 0 <= i__2 ? i__2 : s_rnge(
		    "maxrad", i__2, "zzocced_", (ftnlen)746)] / dist[(i__3 = 
		    i__ - 1) < 2 && 0 <= i__3 ? i__3 : s_rnge("dist", i__3, 
		    "zzocced_", (ftnlen)746)];
	    maxang[(i__1 = i__ - 1) < 2 && 0 <= i__1 ? i__1 : s_rnge("maxang",
		     i__1, "zzocced_", (ftnlen)746)] = dasine_(&d__1, &c_b51);
	    if (failed_()) {
		chkout_("ZZOCCED", (ftnlen)7);
		return ret_val;
	    }
	} else {

/*           The viewing point is outside the Ith ellipsoid but inside */
/*           the nominal bounding sphere.  We can't use the sphere to */
/*           define the maximum bounding cone.  Instead, we bound the */
/*           angular radius of the ellipsoid as follows: */

/*              1) Find the limb of the ellipsoid as seen from the */
/*                 viewing point, and construct the limb plane. */

/*              2) Find the orthogonal projection of the viewing point */
/*                 onto the limb plane; call this project VPPROJ.  The */
/*                 height of the viewing point above VPPROJ is VPH. */

/*              3) Create an upper bound UBDIST on the maximum distance */
/*                 between VPPROJ and any limb point.  Here's where we */
/*                 use a crude but safe estimate:  let UBDIST be the */
/*                 sum of the distance between VPPROJ and the center of */
/*                 the limb and the semi-major axis length of the limb. */
/*                 The triangle inequality shows this is a valid upper */
/*                 bound. */

/*              4) The viewing point and the circle of radius UBDIST */
/*                 centered at VPPROJ define a right circular cone */
/*                 that contains the limb:  this is our choice of */
/*                 the maximum bounding cone.  The arctangent of */
/*                 UBDIST/VPH is the angular radius of this cone. */


/*           The vector XTPOS(*,I) contains the position of the Ith */
/*           target relative to the viewing point, represented in the */
/*           principal axis frame of the Ith target. Let XVWTRG contain */
/*           the inverse of this vector, which is the observer position */
/*           relative to the center of the Ith target, in the principal */
/*           axis frame of the Ith target. */

	    vminus_(&xtpos[(i__1 = i__ * 3 - 3) < 6 && 0 <= i__1 ? i__1 : 
		    s_rnge("xtpos", i__1, "zzocced_", (ftnlen)789)], xvwtrg);
	    edlimb_(&r__[(i__1 = i__ * 3 - 3) < 6 && 0 <= i__1 ? i__1 : 
		    s_rnge("r", i__1, "zzocced_", (ftnlen)791)], &r__[(i__2 = 
		    i__ * 3 - 2) < 6 && 0 <= i__2 ? i__2 : s_rnge("r", i__2, 
		    "zzocced_", (ftnlen)791)], &r__[(i__3 = i__ * 3 - 1) < 6 
		    && 0 <= i__3 ? i__3 : s_rnge("r", i__3, "zzocced_", (
		    ftnlen)791)], xvwtrg, limb);

/*           Extract the limb's center and semi-axis vectors. */

	    el2cgv_(limb, lmbctr, lmbmaj, lmbmin);

/*           Create the limb plane. */

	    psv2pl_(lmbctr, lmbmaj, lmbmin, lplane);

/*           Project the viewing point onto the limb plane.  Find */
/*           the height of the viewing point relative to this plane. */

	    vprjp_(xvwtrg, lplane, vpproj);
	    vph = vdist_(xvwtrg, vpproj);

/*           Find an upper bound on the distance of any limb point from */
/*           VPPROJ. */

	    ubdist = vdist_(vpproj, lmbctr) + vnorm_(lmbmaj);

/*           Find the angular size of the maximum bounding cone.  We */
/*           use the 2-argument arctangent to avoid divide-by-zero */
/*           problems.  The worst that can happen is that VPH is */
/*           zero, which gives us a degenerate cone of angular radius */
/*           pi/2. */

	    maxang[(i__1 = i__ - 1) < 2 && 0 <= i__1 ? i__1 : s_rnge("maxang",
		     i__1, "zzocced_", (ftnlen)824)] = atan2(ubdist, vph);
	}

/*        At this point MAXANG(I) and MINANG(I) are both set for the */
/*        Ith ellipsoid. */

    }

/*     Find the angular separation of the centers of the targets */
/*     seen by the observer. */

    trgsep = vsep_(tpos, &tpos[3]);

/*     If bounding cones defined by the maximum radii don't intersect, */
/*     we're done. */

    if (trgsep > maxang[0] + maxang[1]) {
	ret_val = 0;
	chkout_("ZZOCCED", (ftnlen)7);
	return ret_val;
    }

/*     Use the maximum angular sizes to determine which ellipsoid */
/*     appears to the observer to be "biggest."  This is merely a */
/*     heuristic:  the orientation of the ellipsoids may cause the order */
/*     of the apparent angular sizes to be the opposite. The idea, */
/*     however, is that for "reasonable" cases, we'll correctly identify */
/*     the ellipsoid of larger angular size. This choice is made to */
/*     improve efficiency. */

    if (maxang[0] > maxang[1]) {
	bigidx = 1;
    } else {
	bigidx = 2;
    }

/*     The other index is SMLIDX. */

    smlidx = 3 - bigidx;

/*     We're ready to see whether an occultation condition exists. */
/*     We can efficiently handle some cases by working with bounding */
/*     cones defined by the viewing point, the centers of the targets, */
/*     and spheres centered at the targets having radii equal to the */
/*     minimum and maximum radii of the targets. */

/*     If the two minimum bounding cones have non-trivial intersection */
/*     (of course they always intersect at their common vertex), we're */
/*     guaranteed some sort of occultation.  Check for this case. */

    if (minang[0] + minang[1] > trgsep) {

/*        The minimum bounding cones do overlap. Determine which target */
/*        is "in front" of the other.  We do this determining which */
/*        minimum sphere is in front of the other. */

/*        We'll do the test by examining the angle between the vectors */
/*        from the first target to the observer and the from the first */
/*        target to the second.  If that angle is less than the */
/*        complement of the angular radius of the first target, then the */
/*        minimum sphere of the second target is in transit across the */
/*        first. Otherwise the minimum sphere of the second target is at */
/*        least partially occulted by the first. */

/*        Let T1OPOS be the vector from the first target to the observer. */

	vminus_(tpos, t1opos);

/*        ANGCMP is the angle between a vector from the first target's */
/*        center to its limb and the plane containing the center and */
/*        orthogonal to the vector from the first target's center to the */
/*        observer. */

	angcmp = halfpi_() - minang[0];

/*        T2SEP is the angle between the vector from the first target's */
/*        center to the observer and the vector from the first target */
/*        to the second target. */

	t2sep = vsep_(t1opos, t12pos);
	if (t2sep < angcmp) {

/*           The second target is "in front" of the first. */

	    frtidx = 2;

/*           Set the sign of the return code. */

	    s = -1;
	} else {
	    frtidx = 1;
	    s = 1;
	}

/*        Now classify the occultation.  If the minimum sphere */
/*        of the front target has angular size greater than the maximum */
/*        angular size of the rear target plus the angular separation */
/*        of the target centers, the occultation is total. */

	for (i__ = 1; i__ <= 2; ++i__) {

/*           (The subscript 3-I used below is 2 if I is 1 and vice */
/*           versa.) */

	    if (minang[(i__1 = i__ - 1) < 2 && 0 <= i__1 ? i__1 : s_rnge(
		    "minang", i__1, "zzocced_", (ftnlen)948)] >= maxang[(i__2 
		    = 3 - i__ - 1) < 2 && 0 <= i__2 ? i__2 : s_rnge("maxang", 
		    i__2, "zzocced_", (ftnlen)948)] + trgsep) {

/*              If target I is in front, it totally occults the other */
/*              target.  Otherwise, the other target is in annular */
/*              transit across target I. */

		if (frtidx == i__) {
		    ret_val = s * 3;
		} else {
		    ret_val = s << 1;
		}

/*              We've found the occultation type, so we're done. */

		chkout_("ZZOCCED", (ftnlen)7);
		return ret_val;
	    }
	}

/*        If the angular size of the minimum sphere of *each* target */
/*        plus the angular separation of the centers exceeds the */
/*        maximum angular size of the other target, the occultation */
/*        is partial.  In other words, overlap is guaranteed, but it */
/*        is also guaranteed that neither target is totally blocked */
/*        by the other. */

	if (minang[0] + trgsep > maxang[1] && minang[1] + trgsep > maxang[0]) 
		{

/*           The occultation code is +/- PARTL2, depending on whether */
/*           the first target is in front. */

	    ret_val = s;
	    chkout_("ZZOCCED", (ftnlen)7);
	    return ret_val;
	}

/*        If we get to this point, we were unable to classify the */
/*        occultation using bounding cones alone. */

    }

/*     This is the end of the case of overlapping minimum bounding */
/*     cones. */

/*     We're going apply a linear transformation to the viewing point */
/*     and both targets so as to convert the larger of the targets into */
/*     a sphere. We'll then find the angular separation from the other */
/*     target of the ray from viewing point to the center of the sphere. */
/*     In practice, we must transform the viewing point, the target */
/*     centers, and the limb of the ellipsoid that doesn't get mapped */
/*     to the unit sphere. */

/*     Note that this transformation *does not* preserve angular */
/*     separation, but it preserves set containment relationships. */
/*     In particular, the limbs of the targets map to limbs under */
/*     this transformation, since the limbs are the intersection sets */
/*     of the targets and tangent rays emanating from the viewing point. */

/*     First step:  find the limb of the smaller ellipsoid as */
/*     seen from the viewing point.  We need to map the viewing point */
/*     into the principal axis frame of the smaller ellipsoid first. */
/*     Let SMLMAT be the rotation matrix that effects this mapping. */

    xpose_(&rmat[(i__1 = (smlidx * 3 + 1) * 3 - 12) < 18 && 0 <= i__1 ? i__1 :
	     s_rnge("rmat", i__1, "zzocced_", (ftnlen)1019)], smlmat);

/*     Apply SMLMAT to the vector from the center of the smaller */
/*     ellipsoid to the viewing point. */

    vsub_(viewpt, &ctrs[(i__1 = smlidx * 3 - 3) < 6 && 0 <= i__1 ? i__1 : 
	    s_rnge("ctrs", i__1, "zzocced_", (ftnlen)1025)], smlvu);
    mxv_(smlmat, smlvu, view);

/*     Find the limb of the smaller ellipsoid as seen from VIEW. */

    edlimb_(&r__[(i__1 = smlidx * 3 - 3) < 6 && 0 <= i__1 ? i__1 : s_rnge(
	    "r", i__1, "zzocced_", (ftnlen)1032)], &r__[(i__2 = smlidx * 3 - 
	    2) < 6 && 0 <= i__2 ? i__2 : s_rnge("r", i__2, "zzocced_", (
	    ftnlen)1032)], &r__[(i__3 = smlidx * 3 - 1) < 6 && 0 <= i__3 ? 
	    i__3 : s_rnge("r", i__3, "zzocced_", (ftnlen)1032)], view, limb);

/*     Unpack the limb and map it from the principal axis frame of the */
/*     small ellipsoid back into the original frame. */

    el2cgv_(limb, tmpctr, tmpmaj, tmpmin);
    mtxv_(smlmat, tmpctr, smlctr);
    mtxv_(smlmat, tmpmaj, smlmaj);
    mtxv_(smlmat, tmpmin, smlmin);

/*     At this point SMLCTR is the position of the center of the limb */
/*     relative to the center of the small ellipsoid.  We want to express */
/*     this center relative to the origin; we use the vector SMLCTR for */
/*     this. */

    vadd_(&ctrs[(i__1 = smlidx * 3 - 3) < 6 && 0 <= i__1 ? i__1 : s_rnge(
	    "ctrs", i__1, "zzocced_", (ftnlen)1050)], smlctr, tmpctr);
    vequ_(tmpctr, smlctr);

/*     Create the transformation matrix that maps the larger ellipsoid */
/*     to the unit sphere. */

/*     First compute the scale matrix SCLMAT that scales vector */
/*     components by the reciprocals of the respective semi-axis */
/*     lengths of the large ellipsoid. */

    cleard_(&c__9, sclmat);
    sclmat[0] = 1. / r__[(i__1 = bigidx * 3 - 3) < 6 && 0 <= i__1 ? i__1 : 
	    s_rnge("r", i__1, "zzocced_", (ftnlen)1063)];
    sclmat[4] = 1. / r__[(i__1 = bigidx * 3 - 2) < 6 && 0 <= i__1 ? i__1 : 
	    s_rnge("r", i__1, "zzocced_", (ftnlen)1064)];
    sclmat[8] = 1. / r__[(i__1 = bigidx * 3 - 1) < 6 && 0 <= i__1 ? i__1 : 
	    s_rnge("r", i__1, "zzocced_", (ftnlen)1065)];

/*     Compose the row-scaling matrix SCLMAT with the frame */
/*     transformation required to map vectors to the principal axis */
/*     frame of this ellipsoid.  The result is the transformation */
/*     that maps the larger ellipsoid to the unit sphere. */

/*     We use one matrix SCLROT to perform these composed operations. */

    xpose_(&rmat[(i__1 = (bigidx * 3 + 1) * 3 - 12) < 18 && 0 <= i__1 ? i__1 :
	     s_rnge("rmat", i__1, "zzocced_", (ftnlen)1075)], xr);
    mxm_(sclmat, xr, sclrot);

/*     Transform the viewing point, the large ellipsoid's center vector, */
/*     and vectors defining the limb of the smaller ellipsoid using the */
/*     mapping that converts the larger ellipsoid to the unit sphere. */

/*     Map the viewing point to XVIEW. */

    mxv_(sclrot, viewpt, xview);

/*     Map the center of the large ellipsoid to BIGCTR. */

    mxv_(sclrot, &ctrs[(i__1 = bigidx * 3 - 3) < 6 && 0 <= i__1 ? i__1 : 
	    s_rnge("ctrs", i__1, "zzocced_", (ftnlen)1090)], bigctr);

/*     Map the limb vectors of the smaller ellipsoid. */

    mxv_(sclrot, smlctr, tmpctr);
    vequ_(tmpctr, smlctr);
    mxv_(sclrot, smlmaj, tmpmaj);
    mxv_(sclrot, smlmin, tmpmin);

/*     Find the semi-axes of the transformed limb of the smaller */
/*     ellipsoid. Pack these vectors into the transformed limb data */
/*     structure XLIMB. */

    saelgv_(tmpmaj, tmpmin, smlmaj, smlmin);
    cgv2el_(smlctr, smlmaj, smlmin, xlimb);

/*     Find the direction vector of the ray from the viewing point */
/*     to the transformed center of the large ellipsoid. */

    vsub_(bigctr, xview, raydir);

/*     Find the angular separation of the ray and the transformed */
/*     limb of the small ellipsoid.  The output MINPT is the limb */
/*     point at which the minimum angular separation is attained. */

    zzasryel_("MIN", xlimb, xview, raydir, &minsep, minpt, (ftnlen)3);
    if (failed_()) {
	chkout_("ZZOCCED", (ftnlen)7);
	return ret_val;
    }

/*     Find the angular radius of the unit sphere centered at */
/*     BIGCTR, as seen from XVIEW. */

    bigr = vnorm_(raydir);

/*     Although previous error checks should ensure that BIGR is */
/*     greater than or equal to 1, we'll use a safe arcsine */
/*     computation. */

    d__1 = 1. / bigr;
    uasize = dasine_(&d__1, &c_b51);
    if (failed_()) {
	chkout_("ZZOCCED", (ftnlen)7);
	return ret_val;
    }

/*     At this point, UASIZE is the angular size of the unit sphere */
/*     representing the transformed larger ellipsoid.  MINSEP is the */
/*     angular separation of the ray from the viewing point to the */
/*     center of the unit sphere and the transformed limb of the */
/*     smaller ellipsoid. */

    if (minsep > uasize) {

/*        There's no overlap. */

	ret_val = 0;
	chkout_("ZZOCCED", (ftnlen)7);
	return ret_val;
    }

/*     There's an overlap; now we must classify it. We know the limb */
/*     point MINPT at which the minimum angular separation occurs lies */
/*     in front of or behind the unit sphere, since the angular */
/*     separation at this point is less than or equal to UASIZE. */

/*     Find the vector from the center of the sphere to MINPT. */

    vsub_(minpt, bigctr, minvec);

/*     Get the inverse of the ray's direction vector. */

    vminus_(raydir, invray);

/*     Now we can apply the criterion from the spherical occultation */
/*     algorithm to determine whether MINPT is in front of or behind */
/*     the sphere.  We'll use the logical flag SFRONT to indicate the */
/*     relative position of MINPT. */

/*     Set the sign S used later to set the return code as well. */

    if (vsep_(minvec, invray) <= halfpi_() - uasize) {

/*        MINPT is in front. */

	sfront = TRUE_;
    } else {
	sfront = FALSE_;
    }
    if (sfront && smlidx == 1 || ! sfront && smlidx == 2) {

/*        The first target is in front. */

	s = 1;
    } else {
	s = -1;
    }
    if (minsep <= -uasize) {

/*        Arriving here implies that the "smaller" ellipsoid actually */
/*        appears larger than the other.  Recall that our determination */
/*        of which ellipsoid had larger apparent extent was fallible. */
/*        This situation is not an error condition. */

/*        The ray intersects the interior of the plane region bounded by */
/*        the limb of the "smaller" ellipsoid, and the unit sphere is */
/*        either totally occulted by the smaller ellipsoid or is in */
/*        annular transit across it. */

	if (sfront) {

/*           The point of minimum angular separation on the limb of the */
/*           smaller ellipsoid is in front: we have a total occultation */
/*           of the larger ellipsoid. */

	    ret_val = s * 3;
	} else {

/*           We have an annular transit of the larger ellipsoid */
/*           across the smaller one. */

	    ret_val = s << 1;
	}
    } else {

/*        We know that some type of occultation exists. We know the */
/*        unit sphere is *neither* totally occulted by the other */
/*        ellipsoid nor in annular transit across it. It's possible that */
/*        the other ellipsoid is totally occulted by the unit sphere or */
/*        is in annular transit across it; otherwise we have a partial */
/*        occultation. */

/*        We try two quick classification tests first: */

/*           1) We see whether the maximum bounding cone of the small */
/*              ellipsoid is contained in the cone defined by the */
/*              viewing point and unit sphere. */

/*           2) We see whether the minimum bounding cone of the small */
/*              ellipsoid extends beyond the cone defined by the */
/*              viewing point and unit sphere. */

/*        Note that we need to re-compute the bounding cones for the */
/*        small ellipsoid since we've applied a linear transformation */
/*        to it. */

/*        Note also that these tests are not duplicates of the tests */
/*        performed earlier, since now the bounding cones of the */
/*        ellipsoids have been changed by the transformation applied */
/*        to both. */

/*        The linear transformation applied to the small ellipsoid does */
/*        not preserve distances, so we must re-compute the distance */
/*        from the viewing point to the center of the small ellipsoid. */

	vsub_(xview, smlctr, xsmlvu);
	xdist[(i__1 = smlidx - 1) < 2 && 0 <= i__1 ? i__1 : s_rnge("xdist", 
		i__1, "zzocced_", (ftnlen)1271)] = vnorm_(xsmlvu);

/*        Compute angular radii of bounding cones for the transformed */
/*        limb of the small ellipsoid.  First, capture the semi-axis */
/*        lengths of the limb. */

	majlen = vnorm_(smlmaj);
	minlen = vnorm_(smlmin);
	if (xdist[(i__1 = smlidx - 1) < 2 && 0 <= i__1 ? i__1 : s_rnge("xdist"
		, i__1, "zzocced_", (ftnlen)1281)] >= majlen) {

/*           The viewing point is outside a sphere of radius MAJLEN */
/*           centered at the limb's center.  We use this sphere to */
/*           to define the maximum angular radius.  Note that this */
/*           sphere may have larger angular extent than the small */
/*           ellipsoid, but it's guaranteed to block the small */
/*           ellipsoid. */

	    d__1 = majlen / xdist[(i__2 = smlidx - 1) < 2 && 0 <= i__2 ? i__2 
		    : s_rnge("xdist", i__2, "zzocced_", (ftnlen)1290)];
	    maxang[(i__1 = smlidx - 1) < 2 && 0 <= i__1 ? i__1 : s_rnge("max"
		    "ang", i__1, "zzocced_", (ftnlen)1290)] = dasine_(&d__1, &
		    c_b51);
	    if (failed_()) {
		chkout_("ZZOCCED", (ftnlen)7);
		return ret_val;
	    }
	} else {

/*           We create a maximum bounding cone using the same technique */
/*           we used above for the original, untransformed targets.  In */
/*           this case we already have the components of the limb of */
/*           the transformed, small target. */

/*           Create the limb plane. */

	    psv2pl_(smlctr, smlmaj, smlmin, lplane);

/*           Project the viewing point onto the limb plane.  Find */
/*           the height of the viewing point relative to this plane. */

	    vprjp_(xview, lplane, vpproj);
	    vph = vdist_(xview, vpproj);

/*           Find an upper bound on the distance of any limb point from */
/*           VPPROJ. */

	    ubdist = vdist_(vpproj, smlctr) + majlen;

/*           Find the angular size of the maximum bounding cone.  We */
/*           use the 2-argument arctangent to avoid divide-by-zero */
/*           problems.  The worst that can happen is that VPH is */
/*           zero, which gives us a degenerate cone of angular radius */
/*           pi/2. */

	    maxang[(i__1 = smlidx - 1) < 2 && 0 <= i__1 ? i__1 : s_rnge("max"
		    "ang", i__1, "zzocced_", (ftnlen)1329)] = atan2(ubdist, 
		    vph);
	}

/*        Now find the minimum bounding cone.  The situation is slightly */
/*        complicated by the fact that we have the limb of the */
/*        transformed, small ellipsoid rather than the ellipsoid itself. */
/*        We don't want to use ZZASRYEL here because that routine is */
/*        slow:  we don't want to call it if a quick test will do. So we */
/*        use a somewhat crude estimate that guarantees that all rays */
/*        contained in the small bounding cone intersect the small */
/*        ellipsoid.  The approach is as follows: */

/*           1) Determine the angle between the normal to the limb plane */
/*              pointing towards XVIEW and the viewing point-limb center */
/*              vector. Call this angle TILT. */

/*           2) For a circle having radius equal to the semi-minor axis */
/*              length of the limb, inscribed in the limb, and coplanar */
/*              with the limb, the minimum angular radius of any point */
/*              on the circle, as seen from XVIEW, is associated with */
/*              the point farthest from XVIEW.  The angular separation */
/*              of the vector from the limb center to this point and the */
/*              vector from XVIEW to the limb center is pi/2 + TILT. */
/*              Find the angular radius associated with that point. */

/*        Start out by constructing a normal to the limb plane. */

	ucrss_(smlmaj, smlmin, lnorml);

/*        Choose a value of TILT not exceeding pi/2. */

	tilt = vsep_(lnorml, xsmlvu);
	if (tilt > halfpi_()) {
	    tilt = pi_() - tilt;
	}

/*        Now we have a right triangle whose base is the distance from */
/*        XVIEW to the limb's center plus sin(TILT)*MINLEN, and whose */
/*        height is cos(TILT)*MINLEN. */

/*        Find the angle associated with the corner of the triangle */
/*        associated with the viewing point.  This is the angular */
/*        radius of our minimum bounding cone. */

	minang[(i__1 = smlidx - 1) < 2 && 0 <= i__1 ? i__1 : s_rnge("minang", 
		i__1, "zzocced_", (ftnlen)1380)] = atan2(cos(tilt) * minlen, 
		sin(tilt) * minlen + xdist[(i__2 = smlidx - 1) < 2 && 0 <= 
		i__2 ? i__2 : s_rnge("xdist", i__2, "zzocced_", (ftnlen)1380)]
		);

/*        Compute angular separation of the transformed centers. */

	vsub_(smlctr, xview, smldir);
	xasep = vsep_(raydir, smldir);

/*        Test for inclusion of the maximum bounding cone of the small */
/*        ellipsoid in the circumscribing cone of the sphere. */

	if (xasep + maxang[(i__1 = smlidx - 1) < 2 && 0 <= i__1 ? i__1 : 
		s_rnge("maxang", i__1, "zzocced_", (ftnlen)1394)] <= uasize) {

/*           The small ellipsoid is either in total occultation or */
/*           in annular transit across the sphere. */

	    if (sfront) {

/*              MINPT is in front of the sphere. We have an annular */
/*              transit of the small ellipsoid across the small one. */

		ret_val = s << 1;
	    } else {

/*              MINPT is behind the sphere.  We have a total */
/*              occultation of the small ellipsoid. */

		ret_val = s * 3;
	    }
	    chkout_("ZZOCCED", (ftnlen)7);
	    return ret_val;
	}

/*        Test for non-containment of the minimum bounding cone of the */
/*        small ellipsoid by the circumscribing cone of the sphere. */

	if (xasep + minang[(i__1 = smlidx - 1) < 2 && 0 <= i__1 ? i__1 : 
		s_rnge("minang", i__1, "zzocced_", (ftnlen)1424)] > uasize) {

/*           The small ellipsoid is either in partial occultation or */
/*           in partial transit across the sphere. */

	    ret_val = s;
	    chkout_("ZZOCCED", (ftnlen)7);
	    return ret_val;
	}

/*        Arriving at this point means we've been unable to classify */
/*        the occultation or transit.  We're going to need to compute */
/*        the maximum angular separation of the limb from the ray */
/*        emanating from the viewing point and passing through the */
/*        center of the sphere. */

	zzasryel_("MAX", xlimb, xview, raydir, &maxsep, maxpt, (ftnlen)3);
	if (failed_()) {
	    chkout_("ZZOCCED", (ftnlen)7);
	    return ret_val;
	}
	if (abs(maxsep) <= uasize) {

/*           Whether the ray from the viewing point to the center */
/*           of the unit sphere does nor does not penetrate the plane */
/*           region bounded by the limb of the smaller ellipse, no */
/*           point on that limb has greater angular separation than */
/*           UASIZE from the ray. */

/*           The small ellipsoid is either in total occultation or */
/*           in annular transit across the sphere. */

	    if (sfront) {

/*              MINPT is in front of the sphere. We have an annular */
/*              transit of the small ellipsoid across the smaller. */

		ret_val = s << 1;
	    } else {

/*              MINPT is behind the sphere.  We have a total */
/*              occultation of the small ellipsoid. */

		ret_val = s * 3;
	    }
	} else {

/*           Whether the ray from the viewing point to the center */
/*           of the unit sphere does nor does not penetrate the plane */
/*           region bounded by the limb of the smaller ellipse, some */
/*           point on that limb has greater angular separation than */
/*           UASIZE from the ray. */

/*           The small ellipsoid is either in partial occultation or */
/*           in partial transit across the sphere. */

	    ret_val = s;
	}

/*        We've classified the occultation in the case where the */
/*        maximum angular separation of the ray and limb had to be */
/*        computed. */

/*        This is the end of the code for the case where there is */
/*        overlap, but the unit sphere is *neither* totally occulted by */
/*        the other ellipsoid nor in annular transit across it. */

    }

/*     ZZOCCED has been set. */

    chkout_("ZZOCCED", (ftnlen)7);
    return ret_val;
} /* zzocced_ */

