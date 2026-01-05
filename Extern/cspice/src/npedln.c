/* npedln.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static doublereal c_b12 = 2.;
static doublereal c_b26 = 0.;

/* $Procedure NPEDLN ( Nearest point on ellipsoid to line ) */
/* Subroutine */ int npedln_(doublereal *a, doublereal *b, doublereal *c__, 
	doublereal *linept, doublereal *linedr, doublereal *pnear, doublereal 
	*dist)
{
    /* System generated locals */
    integer i__1;
    doublereal d__1, d__2;

    /* Builtin functions */
    double pow_dd(doublereal *, doublereal *);
    integer s_rnge(char *, integer, char *, integer);

    /* Local variables */
    doublereal cand[9], scla, sclb, sclc, udir[3];
    extern /* Subroutine */ int vscl_(doublereal *, doublereal *, doublereal *
	    );
    integer i__;
    doublereal scale;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    logical found[2];
    doublereal prjel[9];
    extern /* Subroutine */ int errdp_(char *, doublereal *, ftnlen);
    doublereal sclpt[3], prjpl[4], prjpt[3];
    extern /* Subroutine */ int unorm_(doublereal *, doublereal *, doublereal 
	    *), vprjp_(doublereal *, doublereal *, doublereal *), nvc2pl_(
	    doublereal *, doublereal *, doublereal *);
    extern logical failed_(void);
    doublereal candpl[4], pt[6]	/* was [3][2] */;
    extern /* Subroutine */ int inedpl_(doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, logical *);
    logical ifound;
    extern /* Subroutine */ int pjelpl_(doublereal *, doublereal *, 
	    doublereal *);
    doublereal normal[3];
    extern /* Subroutine */ int sigerr_(char *, ftnlen);
    doublereal oppdir[3];
    extern /* Subroutine */ int chkout_(char *, ftnlen), vsclip_(doublereal *,
	     doublereal *), setmsg_(char *, ftnlen);
    logical xfound;
    extern /* Subroutine */ int npelpt_(doublereal *, doublereal *, 
	    doublereal *, doublereal *), vprjpi_(doublereal *, doublereal *, 
	    doublereal *, doublereal *, logical *);
    doublereal prjnpt[3];
    extern logical return_(void);
    extern /* Subroutine */ int vminus_(doublereal *, doublereal *), surfpt_(
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, logical *);
    doublereal mag;

/* $ Abstract */

/*     Find nearest point on a triaxial ellipsoid to a specified line, */
/*     and the distance from the ellipsoid to the line. */

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

/*     ELLIPSES */

/* $ Keywords */

/*     ELLIPSOID */
/*     GEOMETRY */
/*     MATH */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     A          I   Length of ellipsoid's semi-axis in the x direction */
/*     B          I   Length of ellipsoid's semi-axis in the y direction */
/*     C          I   Length of ellipsoid's semi-axis in the z direction */
/*     LINEPT     I   Point on line */
/*     LINEDR     I   Direction vector of line */
/*     PNEAR      O   Nearest point on ellipsoid to line */
/*     DIST       O   Distance of ellipsoid from line */
/*     UBEL       P   Upper bound of array containing SPICE ellipse. */
/*     UBPL       P   Upper bound of array containing SPICE plane. */

/* $ Detailed_Input */

/*     A, */
/*     B, */
/*     C        are the lengths of the semi-axes of a triaxial */
/*              ellipsoid which is centered at the origin and */
/*              oriented so that its axes lie on the x-, y- and */
/*              z- coordinate axes.  A, B, and C are the lengths of */
/*              the semi-axes that point in the x, y, and z */
/*              directions respectively. */

/*     LINEPT */
/*     LINEDR   are, respectively, a point and a direction vector */
/*              that define a line. The line is the set of vectors */

/*                 LINEPT   +   t * LINEDR */

/*              where t is any real number. */

/* $ Detailed_Output */

/*     PNEAR    is the point on the ellipsoid that is closest to */
/*              the line, if the line doesn't intersect the */
/*              ellipsoid. */

/*              If the line intersects the ellipsoid, PNEAR will */
/*              be a point of intersection. If LINEPT is outside */
/*              of the ellipsoid, PNEAR will be the closest point */
/*              of intersection. If LINEPT is inside the */
/*              ellipsoid, PNEAR will not necessarily be the */
/*              closest point of intersection. */


/*     DIST     is the distance of the line from the ellipsoid. */
/*              This is the minimum distance between any point on */
/*              the line and any point on the ellipsoid. */

/*              If the line intersects the ellipsoid, DIST is zero. */

/* $ Parameters */

/*     UBEL     is the upper bound of the array used to contain */
/*              a SPICE ellipse. See the ELLIPSES Required */
/*              Reading for details. */

/*     UBPL     is the upper bound of the array used to contain */
/*              a SPICE plane. See the PLANES Required Reading */
/*              for details. */

/* $ Exceptions */

/*     If this routine detects an error, the output arguments PNEAR and */
/*     DIST are not modified. */

/*     1)  If the length of any semi-axis of the ellipsoid is */
/*         non-positive, the error SPICE(INVALIDAXISLENGTH) is signaled. */

/*     2)  If the line's direction vector is the zero vector, the error */
/*         SPICE(ZEROVECTOR) is signaled. */

/*     3)  If the length of any semi-axis of the ellipsoid is zero after */
/*         the semi-axis lengths are scaled by the reciprocal of the */
/*         magnitude of the longest semi-axis and then squared, the error */
/*         SPICE(DEGENERATECASE) is signaled. */

/*     4)  If the input ellipsoid is extremely flat or needle-shaped */
/*         and has its shortest axis close to perpendicular to the input */
/*         line, numerical problems could cause this routine's algorithm */
/*         to fail, in which case, the error SPICE(DEGENERATECASE) is */
/*         signaled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     For any ellipsoid and line, if the line does not intersect the */
/*     ellipsoid, there is a unique point on the ellipsoid that is */
/*     closest to the line. Therefore, the distance DIST between */
/*     ellipsoid and line is well-defined. The unique line segment of */
/*     length DIST that connects the line and ellipsoid is normal to */
/*     both of these objects at its endpoints. */

/*     If the line intersects the ellipsoid, the distance between the */
/*     line and ellipsoid is zero. */

/* $ Examples */

/*     1)   We can find the distance between an instrument optic axis ray */
/*          and the surface of a body modeled as a tri-axial ellipsoid */
/*          using this routine. If the instrument position and pointing */
/*          unit vector in body-fixed coordinates are */

/*             LINEPT = ( 1.0D6,  2.0D6,  3.0D6 ) */

/*          and */

/*             LINEDR = ( -4.472091234D-1 */
/*                        -8.944182469D-1, */
/*                        -4.472091234D-3  ) */

/*          and the body semi-axes lengths are */

/*             A = 7.0D5 */
/*             B = 7.0D5 */
/*             C = 6.0D5, */

/*          then the call to NPEDLN */

/*             CALL NPEDLN ( A,      B,      C, */
/*            .              LINEPT, LINEDR, */
/*            .              PNEAR,  DIST        ) */

/*          yields a value for PNEAR, the nearest point on the body to */
/*          the optic axis ray, of */


/*             (  -1.6333110792340931E+03, */
/*                -3.2666222157820771E+03, */
/*                 5.9999183350006724E+05  ) */

/*          and a value for DIST, the distance to the ray, of */

/*             2.3899679338299707E+06 */

/*          (These results were obtained on a PC-Linux system under g77.) */

/*          In some cases, it may not be clear that the closest point */
/*          on the line containing an instrument boresight ray is on */
/*          the boresight ray itself; the point may lie on the ray */
/*          having the same vertex as the boresight ray and pointing in */
/*          the opposite direction. To rule out this possibility, we */
/*          can make the following test: */

/*             C */
/*             C     Find the difference vector between the closest point */
/*             C     on the ellipsoid to the line containing the */
/*             C     boresight ray and the boresight ray's vertex. Find */
/*             C     the angular separation between this difference */
/*             C     vector and the boresight ray. If the angular */
/*             C     separation does not exceed pi/2, we have the nominal */
/*             C     geometry. Otherwise, we have an error. */
/*             C */
/*                   CALL  VSUB ( PNEAR,  LINEPT,  DIFF ) */
/*                   SEP = VSEP ( DIFF,   LINEDR        ) */

/*                   IF (  SEP .LE. HALFPI()  ) THEN */

/*                      [ perform normal processing ] */

/*                   ELSE */

/*                      [ handle error case ] */

/*                   END IF */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.4.0, 24-AUG-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. Corrected */
/*        argument name in $Exceptions section. */

/* -    SPICELIB Version 1.3.0, 15-NOV-2005 (NJB) */

/*        Updated to remove non-standard use of duplicate arguments */
/*        in VSCL calls. Changed exponents to DOUBLE PRECISION type */
/*        in the test for underflow of squared, scaled axis lengths. */

/* -    SPICELIB Version 1.2.1, 06-DEC-2002 (NJB) */

/*        Outputs shown in header example have been corrected to */
/*        be consistent with those produced by this routine. */

/* -    SPICELIB Version 1.2.0, 25-NOV-1992 (NJB) */

/*        Bug fix: in the intercept case, PNEAR is now properly */
/*        re-scaled prior to output. Also, an error in the $Examples */
/*        section was corrected. */

/* -    SPICELIB Version 1.1.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.1.0, 04-DEC-1990 (NJB) */

/*        Error message and description changed for non-positive */
/*        axis length error. */

/* -    SPICELIB Version 1.0.0, 02-NOV-1990 (NJB) */

/* -& */
/* $ Index_Entries */

/*     distance between line and ellipsoid */
/*     distance between line of sight and body */
/*     nearest point on ellipsoid to line */

/* -& */
/* $ Revisions */

/* -    SPICELIB Version 1.2.0, 25-NOV-1992 (NJB) */

/*        Bug fix: in the intercept case, PNEAR is now properly */
/*        re-scaled prior to output. Formerly, it was returned without */
/*        having been re-scaled. */

/*        Also, an error in the $Examples section was corrected: the */
/*        line */

/*           CALL  VSUB ( LINEPT,  PNEAR,  DIFF ) */

/*        was replaced by */

/*           CALL  VSUB ( PNEAR,  LINEPT,  DIFF ) */

/*        The in-line comments were re-arranged slightly, and the claim */
/*        that the inverse orthogonal projection of PRJNPT is guaranteed */
/*        to exist was removed. (The check for this exception was already */
/*        being done.) */


/* -    SPICELIB Version 1.1.0, 04-DEC-1990 (NJB) */

/*        Error message and description changed for non-positive */
/*        axis length error. The former message and description did */
/*        not match, and the description was incorrect: it described */
/*        `zero-length', rather than `non-positive' axes as invalid. */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("NPEDLN", (ftnlen)6);
    }

/*     The algorithm used in this routine has two parts.  The first */
/*     part handles the case where the input line and ellipsoid */
/*     intersect.  Our procedure is simple in that case; we just */
/*     call SURFPT twice, passing it first one ray determined by the */
/*     input line, then a ray pointing in the opposite direction. */
/*     The second part of the algorithm handles the case where SURFPT */
/*     doesn't find an intersection. */

/*     Finding the nearest point on the ellipsoid to the line, when the */
/*     two do not intersect, is a matter of following four steps: */

/*     1)  Find the points on the ellipsoid where the surface normal */
/*         is normal to the line's direction.  This set of points is */
/*         an ellipse centered at the origin.  The point we seek MUST */
/*         lie on this `candidate' ellipse. */

/*     2)  Project the candidate ellipse onto a plane that is normal */
/*         to the line's direction.  This projection preserves */
/*         distance from the line; the nearest point to the line on */
/*         this new ellipse is the projection of the nearest point to */
/*         the line on the candidate ellipse, and these two points are */
/*         exactly the same distance from the line.  If computed using */
/*         infinite-precision arithmetic, this projection would be */
/*         guaranteed to be non-degenerate as long as the input */
/*         ellipsoid were non-degenerate.  This can be verified by */
/*         taking the inner product of the scaled normal to the candidate */
/*         ellipse plane and the line's unitized direction vector */
/*         (these vectors are called NORMAL and UDIR in the code below); */
/*         the inner product is strictly greater than 1 if the ellipsoid */
/*         is non-degenerate. */

/*     3)  The nearest point on the line to the projected ellipse will */
/*         be contained in the plane onto which the projection is done; */
/*         we find this point and then find the nearest point to it on */
/*         the projected ellipse.  The distance between these two points */
/*         is the distance between the line and the ellipsoid. */

/*     4)  Finally, we find the point on the candidate ellipse that was */
/*         projected to the nearest point to the line on the projected */
/*         ellipse that was found in step 3.  This is the nearest point */
/*         on the ellipsoid to the line. */




/*                      Glossary of Geometric Variables */


/*            A, */
/*            B, */
/*            C           Input ellipsoid's semi-axis lengths. */

/*            POINT       Point of intersection of line and ellipsoid */
/*                        if the intersection is non-empty. */

/*            CANDPL      Plane containing candidate ellipse. */

/*            NORMAL      Normal vector to the candidate plane CANDPL. */

/*            CAND        Candidate ellipse. */

/*            LINEPT, */
/*            LINEDR,     Point and direction vector on input line. */

/*            UDIR        Unitized line direction vector. */

/*            PRJPL       Projection plane; the candidate ellipse is */
/*                        projected onto this plane to yield PRJEL. */

/*            PRJEL       Projection of the candidate ellipse CAND onto */
/*                        the projection plane PRJEL. */

/*            PRJPT       Projection of line point. */

/*            PRJNPT      Nearest point on projected ellipse to */
/*                        projection of line point. */

/*            PNEAR       Nearest point on ellipsoid to line. */



/*     We need a valid normal vector. */

    unorm_(linedr, udir, &mag);
    if (mag == 0.) {
	setmsg_("Line direction vector is the zero vector. ", (ftnlen)42);
	sigerr_("SPICE(ZEROVECTOR)", (ftnlen)17);
	chkout_("NPEDLN", (ftnlen)6);
	return 0;

/*     The ellipsoid's semi-axes must have positive length. */

    } else if (*a <= 0. || *b <= 0. || *c__ <= 0.) {
	setmsg_("Semi-axes: A = #,  B = #,  C = #.", (ftnlen)33);
	errdp_("#", a, (ftnlen)1);
	errdp_("#", b, (ftnlen)1);
	errdp_("#", c__, (ftnlen)1);
	sigerr_("SPICE(INVALIDAXISLENGTH)", (ftnlen)24);
	chkout_("NPEDLN", (ftnlen)6);
	return 0;
    }

/*     Scale the semi-axes lengths for better numerical behavior. */
/*     If squaring any one of the scaled lengths causes it to */
/*     underflow to zero, we have an error.  Otherwise, scale the */
/*     point on the input line too. */

/* Computing MAX */
    d__1 = abs(*a), d__2 = abs(*b), d__1 = max(d__1,d__2), d__2 = abs(*c__);
    scale = max(d__1,d__2);
    scla = *a / scale;
    sclb = *b / scale;
    sclc = *c__ / scale;
    if (pow_dd(&scla, &c_b12) == 0. || pow_dd(&sclb, &c_b12) == 0. || pow_dd(&
	    sclc, &c_b12) == 0.) {
	setmsg_("Semi-axis too small:  A = #, B = #, C = #. ", (ftnlen)43);
	errdp_("#", a, (ftnlen)1);
	errdp_("#", b, (ftnlen)1);
	errdp_("#", c__, (ftnlen)1);
	sigerr_("SPICE(DEGENERATECASE)", (ftnlen)21);
	chkout_("NPEDLN", (ftnlen)6);
	return 0;
    }

/*     Scale LINEPT.  Because SCALE might be a very small number, */
/*     we avoid computing 1/SCALE; that's why we don't call VSCL here. */

    sclpt[0] = linept[0] / scale;
    sclpt[1] = linept[1] / scale;
    sclpt[2] = linept[2] / scale;

/*     Hand off the intersection case to SURFPT.  SURFPT determines */
/*     whether rays intersect a body, so we treat the line as a pair */
/*     of rays. */

    vminus_(udir, oppdir);
    surfpt_(sclpt, udir, &scla, &sclb, &sclc, pt, found);
    surfpt_(sclpt, oppdir, &scla, &sclb, &sclc, &pt[3], &found[1]);
    for (i__ = 1; i__ <= 2; ++i__) {
	if (found[(i__1 = i__ - 1) < 2 && 0 <= i__1 ? i__1 : s_rnge("found", 
		i__1, "npedln_", (ftnlen)530)]) {
	    *dist = 0.;
	    vscl_(&scale, &pt[(i__1 = i__ * 3 - 3) < 6 && 0 <= i__1 ? i__1 : 
		    s_rnge("pt", i__1, "npedln_", (ftnlen)534)], pnear);
	    chkout_("NPEDLN", (ftnlen)6);
	    return 0;
	}
    }

/*     Getting here means the line doesn't intersect the ellipsoid. */

/*     Find the candidate ellipse CAND.  NORMAL is a normal vector to */
/*     the plane containing the candidate ellipse.   Mathematically the */
/*     ellipse must exist, since it's the intersection of an ellipsoid */
/*     centered at the origin and a plane containing the origin.  Only */
/*     numerical problems can prevent the intersection from being found. */


/* Computing 2nd power */
    d__1 = scla;
    normal[0] = udir[0] / (d__1 * d__1);
/* Computing 2nd power */
    d__1 = sclb;
    normal[1] = udir[1] / (d__1 * d__1);
/* Computing 2nd power */
    d__1 = sclc;
    normal[2] = udir[2] / (d__1 * d__1);
    nvc2pl_(normal, &c_b26, candpl);
    inedpl_(&scla, &sclb, &sclc, candpl, cand, &xfound);
    if (! xfound) {
	setmsg_("Candidate ellipse could not be found.", (ftnlen)37);
	sigerr_("SPICE(DEGENERATECASE)", (ftnlen)21);
	chkout_("NPEDLN", (ftnlen)6);
	return 0;
    }

/*     Project the candidate ellipse onto a plane orthogonal to the */
/*     line.  We'll call the plane PRJPL and the projected ellipse PRJEL. */

    nvc2pl_(udir, &c_b26, prjpl);
    pjelpl_(cand, prjpl, prjel);

/*     Find the point on the line lying in the projection plane, and */
/*     then find the near point PRJNPT on the projected ellipse.  Here */
/*     PRJPT is the point on the line lying in the projection plane. */
/*     The distance between PRJPT and PRJNPT is DIST. */


    vprjp_(sclpt, prjpl, prjpt);
    npelpt_(prjpt, prjel, prjnpt, dist);
    if (failed_()) {
	chkout_("NPEDLN", (ftnlen)6);
	return 0;
    }

/*     Find the near point PNEAR on the ellipsoid by taking the inverse */
/*     orthogonal projection of PRJNPT; this is the point on the */
/*     candidate ellipse that projects to PRJNPT.  Note that the */
/*     output DIST was computed in step 3 and needs only to be re-scaled. */

/*     The inverse projection of PNEAR ought to exist, but may not */
/*     be calculable due to numerical problems (this can only happen */
/*     when the input ellipsoid is extremely flat or needle-shaped). */

    vprjpi_(prjnpt, prjpl, candpl, pnear, &ifound);
    if (! ifound) {
	setmsg_("Inverse projection could not be found.", (ftnlen)38);
	sigerr_("SPICE(DEGENERATECASE)", (ftnlen)21);
	chkout_("NPEDLN", (ftnlen)6);
	return 0;
    }

/*     Undo the scaling. */

    vsclip_(&scale, pnear);
    *dist = scale * *dist;
    chkout_("NPEDLN", (ftnlen)6);
    return 0;
} /* npedln_ */

