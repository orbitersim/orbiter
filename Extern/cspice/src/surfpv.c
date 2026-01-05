/* surfpv.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static doublereal c_b13 = 1.;

/* $Procedure SURFPV ( Surface point and velocity ) */
/* Subroutine */ int surfpv_(doublereal *stvrtx, doublereal *stdir, 
	doublereal *a, doublereal *b, doublereal *c__, doublereal *stx, 
	logical *found)
{
    /* System generated locals */
    doublereal d__1, d__2, d__3;

    /* Local variables */
    extern /* Subroutine */ int vscl_(doublereal *, doublereal *, doublereal *
	    );
    extern doublereal vdot_(doublereal *, doublereal *);
    extern /* Subroutine */ int vsub_(doublereal *, doublereal *, doublereal *
	    ), vequ_(doublereal *, doublereal *);
    doublereal m, n[3], r__, u[3], v[3], x[3];
    extern /* Subroutine */ int chkin_(char *, ftnlen), dvhat_(doublereal *, 
	    doublereal *);
    doublereal level;
    extern doublereal dpmax_(void);
    doublereal third[3];
    extern /* Subroutine */ int errdp_(char *, doublereal *, ftnlen);
    doublereal dsnum;
    extern doublereal vnorm_(doublereal *);
    extern /* Subroutine */ int vlcom3_(doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *);
    extern logical failed_(void);
    doublereal du[3], dv[3], second[3], stdhat[6];
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), setmsg_(char *, ftnlen), surfnm_(doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *);
    extern logical return_(void);
    extern /* Subroutine */ int surfpt_(doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, logical *)
	    ;
    doublereal udn, vmx[3];

/* $ Abstract */

/*     Find the state (position and velocity) of the surface intercept */
/*     defined by a specified ray, ray velocity, and ellipsoid. */

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
/*     STVRTX     I   State of ray's vertex. */
/*     STDIR      I   State of ray's direction vector. */
/*     A          I   Length of ellipsoid semi-axis along the x-axis. */
/*     B          I   Length of ellipsoid semi-axis along the y-axis. */
/*     C          I   Length of ellipsoid semi-axis along the z-axis. */
/*     STX        O   State of surface intercept. */
/*     FOUND      O   Flag indicating whether intercept state was found. */

/* $ Detailed_Input */

/*     STVRTX   is the state of a ray's vertex. The first three */
/*              components of STVRTX are the vertex's x, y, and z */
/*              position components; the vertex's x, y, and z */
/*              velocity components follow. */

/*              The reference frame relative to which STVRTX is */
/*              specified has axes aligned with with those of a */
/*              triaxial ellipsoid. See the description below of */
/*              the arguments A, B, and C. */

/*              The vertex may be inside or outside of this */
/*              ellipsoid, but not on it, since the surface */
/*              intercept is a discontinuous function at */
/*              vertices on the ellipsoid's surface. */

/*              No assumption is made about the units of length */
/*              and time, but these units must be consistent with */
/*              those of the other inputs. */


/*     STDIR    is the state of the input ray's direction vector. */
/*              The first three components of STDIR are a non-zero */
/*              vector giving the x, y, and z components of the */
/*              ray's direction; the direction vector's x, y, and */
/*              z velocity components follow. */

/*              STDIR is specified relative to the same reference */
/*              frame as is STVRTX. */

/*     A, */
/*     B, */
/*     C        are, respectively, the lengths of a triaxial */
/*              ellipsoid's semi-axes lying along the x, y, and */
/*              z axes of the reference frame relative to which */
/*              STVRTX and STDIR are specified. */

/* $ Detailed_Output */

/*     STX      is the state of the intercept of the input ray on */
/*              the surface of the input ellipsoid. The first */
/*              three components of STX are the intercept's x, y, */
/*              and z position components; the intercept's x, y, */
/*              and z velocity components follow. */

/*              STX is specified relative to the same reference */
/*              frame as are STVRTX and STDIR. */

/*              STX is defined if and only if both the intercept */
/*              and its velocity are computable, as indicated by */
/*              the output argument FOUND. */

/*              The position units of STX are the same as those of */
/*              STVRTX, STDIR, and A, B, and C. The time units are */
/*              the same as those of STVRTX and STDIR. */


/*     FOUND    is a logical flag indicating whether STX is */
/*              defined. FOUND is .TRUE. if and only if both the */
/*              intercept and its velocity are computable. Note */
/*              that in some cases the intercept may computable */
/*              while the velocity is not; this can happen for */
/*              near-tangency cases. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the input ray's direction vector is the zero vector, an */
/*         error is signaled by a routine in the call tree of this */
/*         routine. */

/*     2)  If any of the ellipsoid's axis lengths is nonpositive, an */
/*         error is signaled by a routine in the call tree of this */
/*         routine. */

/*     3)  If the vertex of the ray is on the ellipsoid, the error */
/*         SPICE(INVALIDVERTEX) is signaled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     The position and velocity of the ray's vertex as well as the */
/*     ray's direction vector and velocity vary with time. The */
/*     inputs to SURFPV may be considered the values of these */
/*     vector functions at a particular time, say t0. Thus */

/*        State of vertex:            STVRTX = ( V(t0), V'(t0) ) */

/*        State of direction vector:  STDIR  = ( D(t0), D'(t0) ) */

/*     To determine the intercept point, W(t0), we simply compute the */
/*     intersection of the ray originating at V(t0) in the direction of */
/*     D(t0) with the ellipsoid */

/*           2        2        2 */
/*          x        y        z */
/*        ----- +  ----- +  -----  =  1 */
/*           2        2        2 */
/*          A        B        C */

/*     W(t) is the path of the intercept point along the surface of */
/*     the ellipsoid. To determine the velocity of the intercept point, */
/*     we need to take the time derivative of W(t), and evaluate it at */
/*     t0. Unfortunately W(t) is a complicated expression, and its */
/*     derivative is even more complicated. */

/*     However, we know that the derivative of W(t) at t0, W'(t0), is */
/*     tangent to W(t) at t0. Thus W'(t0) lies in the plane that is */
/*     tangent to the ellipsoid at t0. Let X(t) be the curve in the */
/*     tangent plane that represents the intersection of the ray */
/*     emanating from V(t0) with direction D(t0) with that tangent */
/*     plane. */

/*        X'(t0) = W'(t0) */

/*     The expression for X'(t) is much simpler than that of W'(t); */
/*     SURFPV evaluates X'(t) at t0. */


/*     Derivation of X(t) and X'(t) */
/*     ---------------------------------------------------------------- */

/*     W(t0) is the intercept point. Let N be a surface normal at I(t0). */
/*     Then the tangent plane at W(t0) is the set of points X(t) such */
/*     that */

/*        < X(t) - I(t0), N > = 0 */

/*     X(t) can be expressed as the vector sum of the vertex */
/*     and some scalar multiple of the direction vector, */

/*        X(t) = V(t) + s(t) * D(t) */

/*     where s(t) is a scalar function of time. The derivative of */
/*     X(t) is given by */

/*        X'(t) = V'(t)  +  s(t) * D'(t)  +  s'(t) * D(t) */

/*     We have V(t0), V'(t0), D(t0), D'(t0), W(t0), and N, but to */
/*     evaluate X'(t0), we need s(t0) and s'(t0). We derive an */
/*     expression for s(t) as follows. */

/*     Because X(t) is in the tangent plane, it must satisfy */

/*        < X(t) - W(t0), N > = 0. */

/*     Substituting the expression for X(t) into the equation above */
/*     gives */

/*        < V(t) + s(t) * D(t) - W(t0), N > = 0. */

/*     Thus */

/*        < V(t) - W(t0), N >  +  s(t) * < D(t), N > = 0, */

/*     and */
/*                    < V(t) - W(t0), N > */
/*        s(t)  =  -  ------------------- */
/*                        < D(t), N > */

/*     The derivative of s(t) is given by */

/*        s'(t) = */

/*            < D(t),N > * < V'(t),N >  -  < V(t)-W(t0),N > * < D'(t),N > */
/*        -   ----------------------------------------------------------- */
/*                                             2 */
/*                                  < D(t), N > */

/* $ Examples */

/*     The numerical results shown for this example may differ across */
/*     platforms. The results depend on the SPICE kernels used as input, */
/*     the compiler and supporting libraries, and the machine specific */
/*     arithmetic implementation. */

/*     1) Illustrate the role of the ray vertex velocity and */
/*        ray direction vector velocity via several simple cases. Also */
/*        show the results of a near-tangency computation. */


/*        Example code begins here. */


/*              PROGRAM SURFPV_EX1 */
/*              IMPLICIT NONE */

/*              CHARACTER*(*)         F1 */
/*              PARAMETER           ( F1 = '(A,3E20.12)' ) */

/*              DOUBLE PRECISION      A */
/*              DOUBLE PRECISION      B */
/*              DOUBLE PRECISION      C */
/*              DOUBLE PRECISION      STVRTX ( 6 ) */
/*              DOUBLE PRECISION      STDIR  ( 6 ) */
/*              DOUBLE PRECISION      STX    ( 6 ) */

/*              INTEGER               I */

/*              LOGICAL               FOUND */

/*              A      = 1.D0 */
/*              B      = 2.D0 */
/*              C      = 3.D0 */

/*              WRITE (*,*) ' ' */
/*              WRITE (*,*) 'Ellipsoid radii:' */
/*              WRITE (*,*) '     A = ', A */
/*              WRITE (*,*) '     B = ', B */
/*              WRITE (*,*) '     C = ', C */

/*              WRITE (*,*) ' ' */
/*              WRITE (*,*) 'Case 1: Vertex varies, direction ' */
/*             .         // 'is constant' */
/*              WRITE (*,*) ' ' */

/*              STVRTX( 1 ) =  2.D0 */
/*              STVRTX( 2 ) =  0.D0 */
/*              STVRTX( 3 ) =  0.D0 */
/*              STVRTX( 4 ) =  0.D0 */
/*              STVRTX( 5 ) =  0.D0 */
/*              STVRTX( 6 ) =  3.D0 */


/*              STDIR ( 1 ) = -1.D0 */
/*              STDIR ( 2 ) =  0.D0 */
/*              STDIR ( 3 ) =  0.D0 */
/*              STDIR ( 4 ) =  0.D0 */
/*              STDIR ( 5 ) =  0.D0 */
/*              STDIR ( 6 ) =  0.D0 */

/*              WRITE (*,* ) 'Vertex:' */
/*              WRITE (*,F1) ' ', ( STVRTX(I), I = 1,3 ) */
/*              WRITE (*,* ) 'Vertex velocity:' */
/*              WRITE (*,F1) ' ', ( STVRTX(I), I = 4,6 ) */
/*              WRITE (*,* ) 'Direction:' */
/*              WRITE (*,F1) ' ', ( STDIR(I),  I = 1,3 ) */
/*              WRITE (*,* ) 'Direction velocity:' */
/*              WRITE (*,F1) ' ', ( STDIR(I),  I = 4,6 ) */

/*              CALL SURFPV ( STVRTX, STDIR, A, B, C, STX, FOUND ) */

/*              IF ( .NOT. FOUND ) THEN */
/*                 WRITE (*,*) ' No intercept state found.' */
/*              ELSE */
/*                 WRITE (*,* ) 'Intercept:' */
/*                 WRITE (*,F1) ' ', ( STX(I),  I = 1,3 ) */
/*                 WRITE (*,* ) 'Intercept velocity:' */
/*                 WRITE (*,F1) ' ', ( STX(I),  I = 4,6 ) */
/*                 WRITE (*,* ) ' ' */
/*              END IF */

/*              WRITE (*,*) ' ' */
/*              WRITE (*,*) 'Case 2: Vertex and direction both vary' */
/*              WRITE (*,*) ' ' */

/*              STDIR ( 6 ) =  4.D0 */

/*              WRITE (*,* ) 'Vertex:' */
/*              WRITE (*,F1) ' ', ( STVRTX(I), I = 1,3 ) */
/*              WRITE (*,* ) 'Vertex velocity:' */
/*              WRITE (*,F1) ' ', ( STVRTX(I), I = 4,6 ) */
/*              WRITE (*,* ) 'Direction:' */
/*              WRITE (*,F1) ' ', ( STDIR(I),  I = 1,3 ) */
/*              WRITE (*,* ) 'Direction velocity:' */
/*              WRITE (*,F1) ' ', ( STDIR(I),  I = 4,6 ) */

/*              CALL SURFPV ( STVRTX, STDIR, A, B, C, STX, FOUND ) */

/*              IF ( .NOT. FOUND ) THEN */
/*                 WRITE (*,*) ' No intercept state found.' */
/*              ELSE */
/*                 WRITE (*,* ) 'Intercept:' */
/*                 WRITE (*,F1) ' ', ( STX(I),  I = 1,3 ) */
/*                 WRITE (*,* ) 'Intercept velocity:' */
/*                 WRITE (*,F1) ' ', ( STX(I),  I = 4,6 ) */
/*                 WRITE (*,* ) ' ' */
/*              END IF */

/*              WRITE (*,*) ' ' */
/*              WRITE (*,*) 'Case 3: Vertex and direction both vary;' */
/*              WRITE (*,*) '        near-tangent case.' */
/*              WRITE (*,*) ' ' */

/*              STVRTX( 3 ) =  C - 1.D-15 */
/*              STVRTX( 6 ) =  1.D299 */
/*              STDIR ( 6 ) =  1.D299 */

/*              WRITE (*,* ) 'Vertex:' */
/*              WRITE (*,F1) ' ', ( STVRTX(I), I = 1,3 ) */
/*              WRITE (*,* ) 'Vertex velocity:' */
/*              WRITE (*,F1) ' ', ( STVRTX(I), I = 4,6 ) */
/*              WRITE (*,* ) 'Direction:' */
/*              WRITE (*,F1) ' ', ( STDIR(I),  I = 1,3 ) */
/*              WRITE (*,* ) 'Direction velocity:' */
/*              WRITE (*,F1) ' ', ( STDIR(I),  I = 4,6 ) */

/*              CALL SURFPV ( STVRTX, STDIR, A, B, C, STX, FOUND ) */

/*              IF ( .NOT. FOUND ) THEN */
/*                 WRITE (*,*) ' No intercept state found.' */
/*              ELSE */
/*                 WRITE (*,* ) 'Intercept:' */
/*                 WRITE (*,F1) ' ', ( STX(I),  I = 1,3 ) */
/*                 WRITE (*,* ) 'Intercept velocity:' */
/*                 WRITE (*,F1) ' ', ( STX(I),  I = 4,6 ) */
/*                 WRITE (*,* ) ' ' */
/*              END IF */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*         Ellipsoid radii: */
/*              A =    1.0000000000000000 */
/*              B =    2.0000000000000000 */
/*              C =    3.0000000000000000 */

/*         Case 1: Vertex varies, direction is constant */

/*         Vertex: */
/*           0.200000000000E+01  0.000000000000E+00  0.000000000000E+00 */
/*         Vertex velocity: */
/*           0.000000000000E+00  0.000000000000E+00  0.300000000000E+01 */
/*         Direction: */
/*          -0.100000000000E+01  0.000000000000E+00  0.000000000000E+00 */
/*         Direction velocity: */
/*           0.000000000000E+00  0.000000000000E+00  0.000000000000E+00 */
/*         Intercept: */
/*           0.100000000000E+01  0.000000000000E+00  0.000000000000E+00 */
/*         Intercept velocity: */
/*           0.000000000000E+00  0.000000000000E+00  0.300000000000E+01 */


/*         Case 2: Vertex and direction both vary */

/*         Vertex: */
/*           0.200000000000E+01  0.000000000000E+00  0.000000000000E+00 */
/*         Vertex velocity: */
/*           0.000000000000E+00  0.000000000000E+00  0.300000000000E+01 */
/*         Direction: */
/*          -0.100000000000E+01  0.000000000000E+00  0.000000000000E+00 */
/*         Direction velocity: */
/*           0.000000000000E+00  0.000000000000E+00  0.400000000000E+01 */
/*         Intercept: */
/*           0.100000000000E+01  0.000000000000E+00  0.000000000000E+00 */
/*         Intercept velocity: */
/*           0.000000000000E+00  0.000000000000E+00  0.700000000000E+01 */


/*         Case 3: Vertex and direction both vary; */
/*                 near-tangent case. */

/*         Vertex: */
/*           0.200000000000E+01  0.000000000000E+00  0.300000000000E+01 */
/*         Vertex velocity: */
/*           0.000000000000E+00  0.000000000000E+00  0.100000000000+300 */
/*         Direction: */
/*          -0.100000000000E+01  0.000000000000E+00  0.000000000000E+00 */
/*         Direction velocity: */
/*           0.000000000000E+00  0.000000000000E+00  0.100000000000+300 */
/*         Intercept: */
/*           0.258095682795E-07  0.000000000000E+00  0.300000000000E+01 */
/*         Intercept velocity: */
/*          -0.387453203621+307  0.000000000000E+00  0.299999997419+300 */


/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     J.E. McLean        (JPL) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.1, 22-JUL-2020 (JDR) */

/*        Edited the header to comply with NAIF standard. */

/*        Reformatted example's output to comply with maximum line */
/*        length for header comments. */

/* -    SPICELIB Version 1.0.0, 31-MAR-2009 (NJB) (JEM) (WLT) */

/* -& */
/* $ Index_Entries */

/*     ellipsoid surface point and velocity */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     Local variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }
    chkin_("SURFPV", (ftnlen)6);

/*     Determine the ellipsoid surface intercept point of the ray */
/*     emanating from the observer in the direction of D. We'll call it */
/*     X and it will go in the first three elements of STX once we */
/*     determine the velocity. If there is no intersection, we check */
/*     out. */

/*     SURFPT takes care of some error checking too. It signals an error */
/*     if D is the zero vector or if A, B, or C are bad axis lengths. */

    surfpt_(stvrtx, stdir, a, b, c__, x, found);
    if (failed_() || ! (*found)) {
	chkout_("SURFPV", (ftnlen)6);
	return 0;
    }

/*     No result has been found, since we don't know whether the */
/*     intercept velocity is computable. */

    *found = FALSE_;

/*     Compute the state of a unit vector parallel to the ray's */
/*     direction "D." We know that the norm of D is not zero because */
/*     SURFPT checked it. */

    dvhat_(stdir, stdhat);

/*     The velocity vector of the intercept point goes in the last three */
/*     elements of STX. Let */

/*        X = W(t0)               DX = dX/dt at t0 */
/*        V = V(t0)               DV = dV/dt at t0 */
/*        U = D(t0) / ||D(t0)||   DU = d ( D(t)/||D(t)|| )/dt at t0 */

/*     and N be the unit normal to the ellipsoid surface at X. */
/*     Then, from the derivation in $ Particulars above, */

/*           DX  = */


/*            < V-X,N >       < U,N > < DV,N > - < V-X,N > < DU,N > */
/*      DV -  --------- DU -  ------------------------------------- U */
/*             < U,N >                            2 */
/*                                          < U,N > */

/*     Compute the unit normal at the intercept point, and unpack */
/*     the input states into V, U, DV, and DU. Let V-X = VMX. */

    surfnm_(a, b, c__, x, n);
    vequ_(stvrtx, v);
    vequ_(stdhat, u);
    vequ_(&stvrtx[3], dv);
    vequ_(&stdhat[3], du);
    vsub_(v, x, vmx);

/*     Reject the vertex if it's on the ellipsoid. */
/*     We check this by determining whether the transformed */
/*     vertex is on or in the unit sphere. */

/* Computing 2nd power */
    d__1 = v[0] / *a;
/* Computing 2nd power */
    d__2 = v[1] / *b;
/* Computing 2nd power */
    d__3 = v[2] / *c__;
    level = d__1 * d__1 + d__2 * d__2 + d__3 * d__3;
    if (level == 1.) {
	setmsg_("Ray's vertex (# # #) has level surface parameter #. Vertex "
		"must not be on the ellipsoid.", (ftnlen)88);
	errdp_("#", v, (ftnlen)1);
	errdp_("#", &v[1], (ftnlen)1);
	errdp_("#", &v[2], (ftnlen)1);
	errdp_("#", &level, (ftnlen)1);
	sigerr_("SPICE(INVALIDVERTEX)", (ftnlen)20);
	chkout_("SURFPV", (ftnlen)6);
	return 0;
    }

/*     As the intercept point nears the limb, its velocity may tend to */
/*     infinity. We must check the value of < U,N > before dividing by */
/*     it. If the intercept point is on the limb, then < U,N > = 0. If */
/*     it is near the limb, < U,N > may be so small that dividing by it */
/*     would result in a number that is greater than the maximum double */
/*     precision number for the computer. */

    udn = vdot_(u, n);
    if (udn == 0.) {

/*        The intercept point is on the limb, so its velocity */
/*        is not defined. This means we can't "find" the state */
/*        of the intercept point. */

	chkout_("SURFPV", (ftnlen)6);
	return 0;
    }

/*     Evaluate the second term of the equation for DX, but don't */
/*     divide by < U,N > just yet. */

    d__1 = vdot_(vmx, n);
    vscl_(&d__1, du, second);

/*                                                         2 */
/*     Evaluate the third term, but don't divide by < U,N >  just yet. */

    dsnum = udn * vdot_(dv, n) - vdot_(vmx, n) * vdot_(du, n);
    vscl_(&dsnum, u, third);

/*     We'll use the following test. */

/* Computing MAX */
    d__1 = vnorm_(second), d__2 = vnorm_(third), d__1 = max(d__1,d__2);
    m = max(d__1,1.);

/*     If */

/*           M          DPMAX() */
/*        -------   >   ------- */
/*               2      MARGIN */
/*        < U,N > */


/*     or equivalently */

/*                               2 */
/*        M  >  DPMAX() * < U,N >  / MARGIN */


/*     then the velocity is probably too large to compute. We know that */
/*     we can perform the multiplication above because U and N are both */
/*     unit vectors, so the dot product of U and N is less than or equal */
/*     to one. */

/* Computing 2nd power */
    d__1 = udn;
    if (m > dpmax_() / 10. * (d__1 * d__1)) {
	chkout_("SURFPV", (ftnlen)6);
	return 0;
    }

/*     If < U,N > passed the tests above, we can solve for the */
/*     intercept velocity. */

/*                                                         2 */
/*        DX =  DV  -  SECOND / < U,N >  -  THIRD / < U,N > */


    r__ = 1. / udn;
    d__1 = -r__;
/* Computing 2nd power */
    d__3 = r__;
    d__2 = -(d__3 * d__3);
    vlcom3_(&c_b13, dv, &d__1, second, &d__2, third, &stx[3]);

/*     Since we could compute the velocity, we can assign the */
/*     intercept point, and set the found flag to .TRUE. */

    vequ_(x, stx);
    *found = TRUE_;
    chkout_("SURFPV", (ftnlen)6);
    return 0;
} /* surfpv_ */

