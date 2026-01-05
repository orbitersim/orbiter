/* zzdnpt.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static doublereal c_b15 = 1.;

/* $Procedure ZZDNPT ( Derivative of ellipsoid near point ) */
/* Subroutine */ int zzdnpt_(doublereal *state, doublereal *nearp, doublereal 
	*a, doublereal *b, doublereal *c__, doublereal *dnear, doublereal *
	dalt, logical *found)
{
    /* Initialized data */

    static doublereal gradm[9]	/* was [3][3] */ = { 1.,0.,0.,0.,1.,0.,0.,0.,
	    1. };
    static doublereal m[9]	/* was [3][3] */ = { 1.,0.,0.,0.,1.,0.,0.,0.,
	    1. };

    /* System generated locals */
    integer i__1, i__2;
    doublereal d__1;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer);

    /* Local variables */
    doublereal grad[3], temp[3];
    extern doublereal vdot_(doublereal *, doublereal *);
    extern /* Subroutine */ int vsub_(doublereal *, doublereal *, doublereal *
	    );
    extern doublereal vtmv_(doublereal *, doublereal *, doublereal *);
    integer i__;
    doublereal l;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    doublereal denom, dterm[3];
    extern /* Subroutine */ int vlcom_(doublereal *, doublereal *, doublereal 
	    *, doublereal *, doublereal *);
    doublereal norml[3];
    extern /* Subroutine */ int unorm_(doublereal *, doublereal *, doublereal 
	    *);
    doublereal length, lprime;
    extern /* Subroutine */ int chkout_(char *, ftnlen);
    doublereal zenith[3];
    extern logical return_(void);
    extern /* Subroutine */ int mxv_(doublereal *, doublereal *, doublereal *)
	    ;

/* $ Abstract */

/*     Compute the velocity of an ellipsoid surface point nearest to a */
/*     specified position. */

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

/*     DERIVATIVE */
/*     ELLIPSOID */
/*     GEOMETRY */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     STATE      I   State of an object in body-fixed coordinates. */
/*     NEARP      I   Near point on ellipsoid. */
/*     A          I   Length of semi-axis parallel to x-axis. */
/*     B          I   Length of semi-axis parallel to y-axis. */
/*     C          I   Length on semi-axis parallel to z-axis. */
/*     DNEAR      O   Derivative of the nearest point on the ellipsoid. */
/*     DALT       O   Derivative of altitude. */
/*     FOUND      O   Tells whether DNEAR is degenerate. */

/* $ Detailed_Input */

/*     STATE    is a 6-vector giving the position and velocity of some */
/*              object in the body-fixed coordinates of the ellipsoid. */

/*     NEARP    the calculated/derived coordinates of the point on the */
/*              ellipsoid closest to STATE. */

/*              In body-fixed coordinates, the semi-axes of the ellipsoid */
/*              are aligned with the X, Y, and Z-axes of the coordinate */
/*              system. */

/*     A        is the length of the semi-axis of the ellipsoid that is */
/*              parallel to the X-axis of the body-fixed coordinate */
/*              system. */

/*     B        is the length of the semi-axis of the ellipsoid that is */
/*              parallel to the Y-axis of the body-fixed coordinate */
/*              system. */

/*     C        is the length of the semi-axis of the ellipsoid that is */
/*              parallel to the Z-axis of the body-fixed coordinate */
/*              system. */

/* $ Detailed_Output */

/*     DNEAR    is the 3-vector giving the velocity in */
/*              body-fixed coordinates of the point on the ellipsoid, */
/*              closest to the object whose position and velocity are */
/*              represented by STATE. */

/*              While the position component of DNEAR is always */
/*              meaningful, the velocity component of DNEAR will be */
/*              meaningless if FOUND if .FALSE. (See the discussion of */
/*              the meaning of FOUND below.) */

/*     DALT     is rate of change of the altitude, i.e. the range rate. */

/*              Note that the rate of change of altitude is meaningful if */
/*              and only if FOUND is .TRUE. (See the discussion of the */
/*              meaning of FOUND below.) */

/*     FOUND    is a logical flag indicating whether or not the velocity */
/*              portion of DNEAR is meaningful. If the velocity portion */
/*              of DNEAR is meaningful FOUND will be returned with a */
/*              value of .TRUE. Under very rare circumstance the velocity */
/*              of the near point is undefined. Under these circumstances */
/*              FOUND will be returned with the value .FALSE. */

/*              FOUND can be .FALSE. only for states whose position */
/*              components are inside the ellipsoid and then only at */
/*              points on a special surface contained inside the */
/*              ellipsoid called the focal set of the ellipsoid. */

/*              A point in the interior is on this special surface only */
/*              if there are two or more points on the ellipsoid that are */
/*              closest to it. The origin is such a point and the only */
/*              such point if the ellipsoid is a sphere. For */
/*              non-spheroidal ellipsoids the focal set contains small */
/*              portions of the planes of symmetry of the ellipsoid. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the axes are non-positive, an error is signaled by a */
/*         routine in the call tree of this routine. */

/*     2)  If an object is passing through the interior of an ellipsoid */
/*         there are points at which there is more than 1 point on */
/*         the ellipsoid that is closest to the object.  At these */
/*         points the velocity of the near point is undefined. (See */
/*         the description of the output variable FOUND). */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine computes DNEAR from STATE. In addition it returns */
/*     the range rate of altitude. */

/*     Note that this routine can compute DNEAR for STATES outside, */
/*     on, or inside the ellipsoid.  However, DNEAR and derivative */
/*     of altitude do not exist for a "small" set of STATES in the */
/*     interior of the ellipsoid. See the discussion of FOUND above */
/*     for a description of this set of points. */

/* $ Examples */

/*     None. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman    (JPL) */
/*     W.L. Taber      (JPL) */
/*     E.D. Wright     (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 30-SEP-2021 (EDW) */

/* -& */
/* $ Index_Entries */

/*     Velocity of the nearest point on an ellipsoid */
/*     Rate of change of the altitude over an ellipsoid */
/*     Derivative of altitude over an ellipsoid */
/*     Range rate of altitude over an ellipsoid */
/*     Velocity of a ground track */

/* -& */

/*     SPICELIB functions */


/*     Local Variables */


/*     Saved Variables */


/*     Initial Values */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }
    chkin_("ZZDNPT", (ftnlen)6);

/*     Until we have reason to believe otherwise, we set FOUND to TRUE. */

    *found = TRUE_;

/*     Now for the work of this routine.  We need to compute the */
/*     velocity component of NEARP. */

/*     In all of the discussions below we let <,> stand for the */
/*     dot product (inner product). */

/*     Let P be the position (first three components) of STATE */
/*     and let N be the position (first three components) of NEARP. */

/*     The surface of the ellipsoid is described as the level set */
/*     f(x,y,z) = 1 for the function f defined by */

/*                    x**2 + y**2 + z**2 */
/*         f(x,y,z) = ----   ----   ---- */
/*                    A**2   B**2   C**2 */

/*     Let GRAD be the "half" gradient of f, */

/*         (NABLA * f)/2 */

/*     with NABLA the operator */

/*         (Dx, Dy, Dz) */

/*     ("D" indicating partial derivative). Then for some L */

/*           N + L * GRAD = P                         ( 1 ) */

/*     Solve for L */

/*           L * GRAD = P - N */

/*      Apply <,GRAD> to LHS and RHS of expression */

/*           <L * GRAD, GRAD> = < P - N, GRAD > */

/*           L * < GRAD, GRAD > = < P - N, GRAD > */

/*     So that */
/*                < P - N, GRAD > */
/*           L =  -------------- */
/*                < GRAD , GRAD > */

/*      Recall */

/*            < X, X > = |X|**2 , X in Rn, R3 in this case */

/*                          GRAD */
/*             =  < P - N, ------ >  /  | GRAD | */
/*                         |GRAD| */

/*     Since GRAD is computed at a point on the level set f(x,y,z) = 1 */
/*     we don't have to worry about the magnitude of |GRAD| being */
/*     so small that underflow can occur (mostly). */

/*     Note that the half gradient of f can be computed by simple */
/*     vector multiplication */

/*                       [ 1/A**2    0       0    ] [ x ] */
/*        GRAD(x,y,z)  = |   0     1/B**2    0    | | y | */
/*                       [   0       0     1/C**2 ] [ z ] */

/*     We call the matrix above GRADM.  The correct off */
/*     diagonal values have been established in the data statement */
/*     following the declaration section of this routine. */

    gradm[0] = 1. / (*a * *a);
    gradm[4] = 1. / (*b * *b);
    gradm[8] = 1. / (*c__ * *c__);
    vsub_(state, nearp, zenith);
    mxv_(gradm, nearp, grad);
    unorm_(grad, norml, &length);
    l = vdot_(zenith, norml) / length;

/*     We can rewrite equation (1) as */

/*        P = N + L * GRADM * N */

/*     from this it follows that */

/*        P' =  N' + L' * GRADM * N */
/*                 + L  * GRADM * N' */

/*           = ( IDENT + L*GRADM ) * N'   + L' * GRADM * N */

/*           = ( IDENT + L*GRADM ) * N'   + L' * GRAD */

/*     where IDENT is the 3x3 identity matrix. */

/*     Let M be the inverse of the matrix IDENT + L*GRADM. (Provided */
/*     of course that all of the diagonal entries are non-zero). */

/*     If we multiply both sides of the equation above by M */
/*     we have */


/*        M*P'  = N'  + L'* M * GRAD                      ( 2 ) */


/*     Recall now that N' is orthogonal to GRAD (N' lies in the */
/*     tangent plane to the ellipsoid at N and GRAD is normal */
/*     to this tangent plane).  Thus */

/*        < GRAD, M*P' > = L' < GRAD, M * GRAD > */

/*     and */

/*                 < GRAD, M*P'   > */
/*        L'   =   ----------------- */
/*                 < GRAD, M*GRAD > */


/*             =   VTMV ( GRAD, M, P' ) / VTMV ( GRAD, M, GRAD ) */

/*     Let's pause now to compute M and L'. */

/*        This is where things could go bad.  M might not exist (which */
/*        indicates STATE is on the focal set of the ellipsoid).  In */
/*        addition it is conceivable that VTMV ( GRAD, M, GRAD ) is */
/*        zero.  This turns out not to be possible.  However, the */
/*        demonstration of this fact requires delving into the details */
/*        of how N was computed by NEARPT.  Rather than spending a */
/*        lot of time explaining the details we will make an */
/*        unnecessary but inexpensive check that we don't divide by */
/*        zero when computing L'. */

    for (i__ = 1; i__ <= 3; ++i__) {
	dterm[(i__1 = i__ - 1) < 3 && 0 <= i__1 ? i__1 : s_rnge("dterm", i__1,
		 "zzdnpt_", (ftnlen)384)] = l * gradm[(i__2 = i__ + i__ * 3 - 
		4) < 9 && 0 <= i__2 ? i__2 : s_rnge("gradm", i__2, "zzdnpt_", 
		(ftnlen)384)] + 1.;
    }
    for (i__ = 1; i__ <= 3; ++i__) {
	if (dterm[(i__1 = i__ - 1) < 3 && 0 <= i__1 ? i__1 : s_rnge("dterm", 
		i__1, "zzdnpt_", (ftnlen)389)] != 0.) {
	    m[(i__1 = i__ + i__ * 3 - 4) < 9 && 0 <= i__1 ? i__1 : s_rnge(
		    "m", i__1, "zzdnpt_", (ftnlen)390)] = 1. / dterm[(i__2 = 
		    i__ - 1) < 3 && 0 <= i__2 ? i__2 : s_rnge("dterm", i__2, 
		    "zzdnpt_", (ftnlen)390)];
	} else {
	    *found = FALSE_;
	    chkout_("ZZDNPT", (ftnlen)6);
	    return 0;
	}
    }
    denom = vtmv_(grad, m, grad);
    if (denom == 0.) {
	*found = FALSE_;
	chkout_("ZZDNPT", (ftnlen)6);
	return 0;
    }
    lprime = vtmv_(grad, m, &state[3]) / denom;

/*     Now that we have L' we can easily compute N'. Rewriting */
/*     equation (2) from above we have. */

/*        N'  = M * ( P' - L'*GRAD ) */

    d__1 = -lprime;
    vlcom_(&c_b15, &state[3], &d__1, grad, temp);
    mxv_(m, temp, dnear);

/*     Only one thing left to do. Compute the derivative */
/*     of the altitude ALT. This quantity equals the range rate of the */
/*     vector from the near point, N, to the observer object, P. */

/*                               ^ */
/*     Range rate in R3 equals < r, v >. In this case, NORML defines */
/*     the unit vector from N to P. The velocity of P with respect */
/*     to N, */

/*        V = d(P - N) = P' - N' */
/*            -- */
/*            dt */

/*     But as we discussed earlier, N' is orthogonal to NORML (GRAD). */
/*     Thus */

/*          ^ */
/*        < r, v > = < NORML, P' - N' > */
/*                 = < NORML, P'> - < NORML, N'> */
/*                 = < NORML, P'> */

/*        dALT/dt = < NORML, P'> */

/*     Given P' = STATE(4,5,6) */

    *dalt = vdot_(norml, &state[3]);
    chkout_("ZZDNPT", (ftnlen)6);
    return 0;
} /* zzdnpt_ */

