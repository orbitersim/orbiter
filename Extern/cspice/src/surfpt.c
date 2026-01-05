/* surfpt.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__2 = 2;
static integer c__3 = 3;
static doublereal c_b19 = 1.;

/* $Procedure SURFPT ( Surface point on an ellipsoid ) */
/* Subroutine */ int surfpt_(doublereal *positn, doublereal *u, doublereal *a,
	 doublereal *b, doublereal *c__, doublereal *point, logical *found)
{
    /* Initialized data */

    static char mssg[32*7] = "Axis A was nonpositive.         " "Axis B was "
	    "nonpositive.         " "Axes A and B were nonpositive.  " "Axis "
	    "C was nonpositive.         " "Axes A and C were nonpositive.  " 
	    "Axes B and C were nonpositive.  " "All three axes were nonposit"
	    "ive.";

    /* System generated locals */
    address a__1[2];
    integer i__1, i__2[2];
    doublereal d__1, d__2;
    char ch__1[35];

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer);
    /* Subroutine */ int s_cat(char *, char **, integer *, integer *, ftnlen);
    double sqrt(doublereal);

    /* Local variables */
    doublereal pmag, ymag, sign;
    extern /* Subroutine */ int vhat_(doublereal *, doublereal *);
    extern doublereal vdot_(doublereal *, doublereal *);
    extern /* Subroutine */ int vsub_(doublereal *, doublereal *, doublereal *
	    ), vequ_(doublereal *, doublereal *);
    doublereal p[3], scale, x[3], y[3];
    extern /* Subroutine */ int chkin_(char *, ftnlen), errch_(char *, char *,
	     ftnlen, ftnlen), errdp_(char *, doublereal *, ftnlen), vlcom_(
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *), vperp_(doublereal *, doublereal *, doublereal *);
    extern doublereal vnorm_(doublereal *);
    doublereal yproj[3];
    extern logical vzero_(doublereal *);
    extern /* Subroutine */ int cleard_(integer *, doublereal *);
    doublereal ux[3];
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), setmsg_(char *, ftnlen);
    extern logical return_(void);
    integer bad;

/* $ Abstract */

/*     Determine the intersection of a line-of-sight vector with the */
/*     surface of an ellipsoid. */

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

/*     ELLIPSOID */
/*     GEOMETRY */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     POSITN     I   Position of the observer in body-fixed frame. */
/*     U          I   Vector from the observer in some direction. */
/*     A          I   Length of ellipsoid semi-axis along the x-axis. */
/*     B          I   Length of ellipsoid semi-axis along the y-axis. */
/*     C          I   Length of ellipsoid semi-axis along the z-axis. */
/*     POINT      O   Point on the ellipsoid pointed to by U. */
/*     FOUND      O   Flag indicating if U points at the ellipsoid. */

/* $ Detailed_Input */

/*     POSITN   is a 3-vector giving the position of an observer with */
/*              respect to the center of an ellipsoid. The vector is */
/*              expressed in a body-fixed reference frame. The semi-axes */
/*              of the ellipsoid are aligned with the X, Y, and Z-axes of */
/*              the body-fixed frame. */

/*     U        is a pointing 3-vector emanating from the observer. */

/*     A        is the length of the semi-axis of the ellipsoid that is */
/*              parallel to the X-axis of the body-fixed reference frame. */

/*     B        is the length of the semi-axis of the ellipsoid that is */
/*              parallel to the Y-axis of the body-fixed reference frame. */

/*     C        is the length of the semi-axis of the ellipsoid that is */
/*              parallel to the Z-axis of the body-fixed reference frame. */

/* $ Detailed_Output */

/*     POINT    is the position of the intercept of the input ray, */
/*              defined by the direction vector U emanating from POSITN, */
/*              on the surface of the input ellipsoid. */

/*              If the ray intersects the ellipsoid, POINT will be */
/*              returned with the body-fixed coordinates of the point */
/*              where the ray first meets the ellipsoid. Otherwise, */
/*              POINT will be returned as (0, 0, 0). */

/*     FOUND    is a logical flag indicating whether or not the ray from */
/*              POSITN with direction U actually intersects the */
/*              ellipsoid. If the ray does intersect the ellipsoid, FOUND */
/*              will be returned as .TRUE. If the ray misses the */
/*              ellipsoid, FOUND will be returned as .FALSE. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the input vector is the zero vector, the error */
/*         SPICE(ZEROVECTOR) is signaled. */

/*     2)  If any of the body's axes is zero, the error */
/*         SPICE(BADAXISLENGTH) is signaled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine assumes that an ellipsoid having semi-axes of */
/*     length A, B and C is given. Moreover, it is assumed that these */
/*     axes are parallel to the X-, Y-, and Z-axes of a reference frame */
/*     whose origin is the geometric center of the ellipsoid---this is */
/*     called the body-fixed reference frame. */

/* $ Examples */

/*     A typical use of SURFPT would be to obtain the planetocentric */
/*     coordinates of the point at which the optic axis of a */
/*     spacecraft-mounted instrument intersects the surface of a target */
/*     body, given the following items. */

/*        1) The epoch (ET) of observation, and the inertial */
/*           pointing (VPNT) of the instrument at this epoch. */

/*        2) The apparent position (VTARG) of the center of the */
/*           target body as seen from the spacecraft at the epoch */
/*           of observation, and the one-way light time (TAU) */
/*           from the target to the spacecraft. */

/*     In order to find the point of intersection, the following */
/*     items are also needed. */

/*        3) The transformation (TIBF) from inertial */
/*           to body-fixed coordinates at epoch ET-TAU. */

/*        4) The radii (R) of the tri-axial ellipsoid */
/*           used to model the target body. */

/*     These may be obtained from the kernel pool via calls to PXFORM */
/*     and BODVRD or BODVCD respectively. */

/*     The position of the observer is just the negative of the */
/*     spacecraft-target vector, VTARG, computed using the VMINUS */
/*     module. (Note that this is NOT the same as the apparent position */
/*     of the spacecraft as seen from the target!) Both vectors must be */
/*     specified in the body-fixed reference frame. The point of */
/*     intersection is found as follows: */

/*         CALL VMINUS ( VTARG, VPOS ) */
/*         CALL MXV    ( TIBF,  VPOS,  VPOS ) */
/*         CALL MXV    ( TIBF,  VPNT,  VPNT ) */

/*         CALL SURFPT ( VPOS, VPNT, R(1), R(2), R(3), VSURF, FOUND ) */

/*     Note that VSURF may or may not be a point of intersection, */
/*     depending on whether FOUND is .TRUE. or .FALSE. Note also that */
/*     VSURF is a vector from the center to the surface of the */
/*     target, in body-fixed coordinates, which may be converted */
/*     directly to planetocentric latitude, longitude, and radius: */

/*         CALL RECLAT ( VSURF, RADIUS, LONG, LAT ) */

/*     To get the inertial vector from the spacecraft to the */
/*     surface point, you must subtract VPOS from VSURF, and rotate */
/*     the resulting vector back to inertial coordinates: */

/*         CALL VSUB ( VSURF, VPOS,  VSURF ) */
/*         CALL MTXV ( TIBF,  VSURF, VSURF ) */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     C.H. Acton         (JPL) */
/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.4.0, 25-MAY-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. Improved */
/*        "POINT" argument description. */

/* -    SPICELIB Version 1.3.0, 03-APR-2006 (NJB) */

/*        Bug fix: intercept point is now always set to the */
/*        ray's vertex when the vertex is on the ellipsoid's */
/*        surface. This routine now uses discovery check-in. */

/* -    SPICELIB Version 1.2.2, 24-OCT-2005 (NJB) */

/*        Updated header to refer to BODVRD and BODVCD instead of */
/*        BODVAR. */

/* -    SPICELIB Version 1.2.1, 27-JUL-2003 (NJB) (CHA) */

/*        Various header corrections were made. The example program */
/*        was upgraded to use real kernels, and the program's output is */
/*        shown. */

/* -    SPICELIB Version 1.2.0, 28-NOV-2002 (NJB) */

/*        Re-implemented intercept computation to reduce loss of */
/*        precision. */

/*        Changed SAVE statement to save only the error message. */
/*        Previously all local variables were saved. */

/* -    SPICELIB Version 1.1.0, 07-AUG-1996 (WLT) */

/*        Added a SAVE statement so that the error message will */
/*        not be lost between separate invocations of the routine. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) */

/* -& */
/* $ Index_Entries */

/*     line of sight intercept with body */
/*     point of intersection between ray and ellipsoid */
/*     surface point of intersection of ray and ellipsoid */

/* -& */
/* $ Revisions */

/* -    SPICELIB Version 1.2.0, 28-NOV-2002 (NJB) */

/*        Re-implemented intercept computation to reduce loss of */
/*        precision. New algorithm maps input ellipsoid to unit */
/*        sphere, finds closest point on input ray to the origin, */
/*        then finds the offset from this point to the surface. */

/* -    Beta Version 2.0.0, 9-JAN-1988 (WLT) */

/*      Short error message SPICE(ZEROAXISLENGTH) changed to */
/*      SPICE(BADAXISLENGTH) */

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

/*     Check the input vector to see if its the zero vector. If it is */
/*     signal an error and return. */

    if (vzero_(u)) {
	chkin_("SURFPT", (ftnlen)6);
	setmsg_("SURFPT: The input vector is the zero vector.", (ftnlen)44);
	sigerr_("SPICE(ZEROVECTOR)", (ftnlen)17);
	chkout_("SURFPT", (ftnlen)6);
	return 0;
    }

/*     Check the axis to make sure that none of them is less than or */
/*     equal to zero. If one is, signal an error and return. */

    bad = 0;
    if (*a <= 0.) {
	++bad;
    }
    if (*b <= 0.) {
	bad += 2;
    }
    if (*c__ <= 0.) {
	bad += 4;
    }
    if (bad > 0) {
	chkin_("SURFPT", (ftnlen)6);
/* Writing concatenation */
	i__2[0] = 32, a__1[0] = mssg + (((i__1 = bad - 1) < 7 && 0 <= i__1 ? 
		i__1 : s_rnge("mssg", i__1, "surfpt_", (ftnlen)365)) << 5);
	i__2[1] = 3, a__1[1] = " ? ";
	s_cat(ch__1, a__1, i__2, &c__2, (ftnlen)35);
	setmsg_(ch__1, (ftnlen)35);
	errch_(" ? ", "The A,B, and C axes were #, #, and # respectively.", (
		ftnlen)3, (ftnlen)50);
	errdp_("#", a, (ftnlen)1);
	errdp_("#", b, (ftnlen)1);
	errdp_("#", c__, (ftnlen)1);
	sigerr_("SPICE(BADAXISLENGTH)", (ftnlen)20);
	chkout_("SURFPT", (ftnlen)6);
	return 0;
    }

/*     We're done with the error checks.  Set the outputs to the */
/*     appropriate values for the "no intersection" case. */

    *found = FALSE_;
    cleard_(&c__3, point);

/*     Apply a linear transformation to the point, direction vector, */
/*     and ellipsoid to transform the problem to one having the unit */
/*     sphere as the target ellipsoid.  (The transformation of the */
/*     ellipsoid is implicit.) */

    x[0] = u[0] / *a;
    x[1] = u[1] / *b;
    x[2] = u[2] / *c__;
    y[0] = positn[0] / *a;
    y[1] = positn[1] / *b;
    y[2] = positn[2] / *c__;

/*     Find the component P of Y (the ray's vertex) orthogonal to X */
/*     (the ray's direction). */

    vperp_(y, x, p);

/*     Find the component of Y parallel to X. */

    vsub_(y, p, yproj);

/*     Find the magnitudes of Y and P. */

    ymag = vnorm_(y);
    pmag = vnorm_(p);

/*     Get a unitized copy of X. */

    vhat_(x, ux);

/*     Now determine whether there's an intersection.  Consider */
/*     the case where Y is outside the sphere first. */

    if (ymag > 1.) {

/*        If P is outside of the sphere, there can be no intersection. */

	if (pmag > 1.) {
	    return 0;
	}

/*        If X points in the same direction as YPROJ, then the ray */
/*        is pointing away from the sphere, and there is no */
/*        intersection. */

	if (vdot_(yproj, x) > 0.) {
	    return 0;
	}

/*        At this point we know there's an intersection. */

	if (pmag == 1.) {

/*           The vector P we've found is the singleton point of */
/*           intersection.  All we have to do is transform P by */
/*           applying the inverse of our original linear transformation. */

	    point[0] = p[0] * *a;
	    point[1] = p[1] * *b;
	    point[2] = p[2] * *c__;
	    *found = TRUE_;
	    return 0;
	}

/*        At this point we know there's a non-trivial intersection. */

/*        Set the sign of the coefficient of UX (a unitized copy */
/*        of X) that will be used to compute the intercept point. */
/*        In this case the coefficient of UX has negative sign because */
/*        the vector we're adding to P points toward Y. */

	sign = -1.;
    } else if (ymag == 1.) {

/*        The ray's vertex is on the surface of the ellipsoid. */
/*        The vertex is the first point of intersection. */

	vequ_(positn, point);
	*found = TRUE_;
	return 0;
    } else {

/*        Y is inside the sphere, so there's definitely an intersection. */
/*        In this case, the intercept is obtained by adding a positive */
/*        multiple of UX to P. */

	sign = 1.;
    }


/*     We have a small amount of work to do:  we'll find the multiple */
/*     of X that when added to P yields the desired intercept point. */

/*     The magnitude of the half-chord connecting P and the surface */
/*     is just */
/*             ____________ */
/*           \/ 1 - PMAG**2 */


/* Computing MAX */
    d__1 = 0., d__2 = 1 - pmag * pmag;
    scale = sqrt((max(d__1,d__2)));

/*     Find the intercept point on the unit sphere. */

    d__1 = sign * scale;
    vlcom_(&c_b19, p, &d__1, ux, point);

/*     Undo our linear transformation. */

    point[0] *= *a;
    point[1] *= *b;
    point[2] *= *c__;
    *found = TRUE_;
    return 0;
} /* surfpt_ */

