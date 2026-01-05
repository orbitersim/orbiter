/* inrypl.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__3 = 3;
static doublereal c_b16 = 1.;

/* $Procedure INRYPL ( Intersection of ray and plane ) */
/* Subroutine */ int inrypl_(doublereal *vertex, doublereal *dir, doublereal *
	plane, integer *nxpts, doublereal *xpt)
{
    /* System generated locals */
    doublereal d__1, d__2;

    /* Local variables */
    doublereal udir[3];
    extern /* Subroutine */ int vhat_(doublereal *, doublereal *), vscl_(
	    doublereal *, doublereal *, doublereal *);
    extern doublereal vdot_(doublereal *, doublereal *);
    extern /* Subroutine */ int vequ_(doublereal *, doublereal *);
    doublereal scale;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    extern doublereal dpmax_(void);
    extern /* Subroutine */ int vlcom_(doublereal *, doublereal *, doublereal 
	    *, doublereal *, doublereal *);
    doublereal const__, prjvn;
    extern doublereal vnorm_(doublereal *);
    extern logical vzero_(doublereal *);
    extern /* Subroutine */ int pl2nvc_(doublereal *, doublereal *, 
	    doublereal *), cleard_(integer *, doublereal *);
    doublereal mscale, prjdif, sclcon, toobig, normal[3], prjdir;
    extern logical smsgnd_(doublereal *, doublereal *);
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), vsclip_(doublereal *, doublereal *), setmsg_(char *, 
	    ftnlen);
    extern logical return_(void);
    doublereal sclvtx[3];

/* $ Abstract */

/*     Find the intersection of a ray and a plane. */

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

/*     PLANES */

/* $ Keywords */

/*     GEOMETRY */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     VERTEX, */
/*     DIR        I   Vertex and direction vector of ray. */
/*     PLANE      I   A SPICE plane. */
/*     NXPTS      O   Number of intersection points of ray and plane. */
/*     XPT        O   Intersection point, if NXPTS = 1. */

/* $ Detailed_Input */

/*     VERTEX, */
/*     DIR      are a point and direction vector that define a */
/*              ray in three-dimensional space. */

/*     PLANE    is a SPICE plane. */

/* $ Detailed_Output */

/*     NXPTS    is the number of points of intersection of the */
/*              input ray and plane. Values and meanings of */
/*              NXPTS are: */

/*                 0     No intersection. */

/*                 1     One point of intersection. Note that */
/*                       this case may occur when the ray's */
/*                       vertex is in the plane. */

/*                -1     An infinite number of points of */
/*                       intersection; the ray lies in the plane. */


/*     XPT      is the point of intersection of the input ray */
/*              and plane, when there is exactly one point of */
/*              intersection. Otherwise, XPT is the zero vector. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the ray's direction vector is the zero vector, the error */
/*         SPICE(ZEROVECTOR) is signaled. NXPTS and XPT are not */
/*         modified. */

/*     2)  If the ray's vertex is further than DPMAX / 3 from the */
/*         origin, the error SPICE(VECTORTOOBIG) is signaled.  NXPTS */
/*         and XPT are not modified. */

/*     3)  If the input plane is further than DPMAX / 3 from the */
/*         origin, the error SPICE(VECTORTOOBIG) is signaled.  NXPTS */
/*         and XPT are not modified. */

/*     4)  The input plane should be created by one of the SPICELIB */
/*         routines */

/*            NVC2PL */
/*            NVP2PL */
/*            PSV2PL */

/*         Invalid input planes will cause unpredictable results. */

/*     5)  In the interest of good numerical behavior, in the case */
/*         where the ray's vertex is not in the plane, this routine */
/*         considers that an intersection of the ray and plane occurs */
/*         only if the distance between the ray's vertex and the */
/*         intersection point is less than DPMAX / 3. */

/*         If VERTEX is not in the plane and this condition is not */
/*         met, then NXPTS is set to 0 and XPT is set to the zero */
/*         vector. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     The intersection of a ray and plane in three-dimensional space */
/*     can be a the empty set, a single point, or the ray itself. */

/* $ Examples */

/*     1)  Find the camera projection of the center of an extended */
/*         body. For simplicity, we assume: */

/*            -- The camera has no distortion;  the image of a point */
/*               is determined by the intersection of the focal plane */
/*               and the line determined by the point and the camera's */
/*               focal point. */

/*            -- The camera's pointing matrix (C-matrix) is available */
/*               in a C-kernel. */


/*            C */
/*            C     Load Leapseconds and SCLK kernels to support time */
/*            C     conversion. */
/*            C */
/*                  CALL FURNSH ( 'LEAP.KER' ) */
/*                  CALL FURNSH ( 'SCLK.KER' ) */

/*            C */
/*            C     Load an SPK file containing ephemeris data for */
/*            C     observer (a spacecraft, whose NAIF integer code */
/*            C     is SC) and target at the UTC epoch of observation. */
/*            C */
/*                  CALL FURNSH ( 'SPK.BSP' ) */

/*            C */
/*            C     Load a C-kernel containing camera pointing for */
/*            C     the UTC epoch of observation. */
/*            C */
/*                  CALL FURNSH ( 'CK.BC' ) */

/*            C */
/*            C     Find the ephemeris time (barycentric dynamical time) */
/*            C     and encoded spacecraft clock times corresponding to */
/*            C     the UTC epoch of observation. */
/*            C */
/*                  CALL UTC2ET ( UTC, ET          ) */
/*                  CALL SCE2C  ( SC,  ET,  SCLKDP ) */

/*            C */
/*            C     Encode the pointing lookup tolerance. */
/*            C */
/*                  CALL SCTIKS ( SC, TOLCH,  TOLDP  ) */

/*            C */
/*            C     Find the observer-target vector at the observation */
/*            C     epoch. In this example, we'll use a light-time */
/*            C     corrected state vector. */
/*            C */
/*                  CALL SPKEZ ( TARGET,  ET,  'J2000',  'LT',  SC, */
/*                 .             STATE,   LT                        ) */

/*            C */
/*            C     Look up camera pointing. */
/*            C */
/*                  CALL CKGP  ( CAMERA, SCLKDP, TOLDP, 'J2000', CMAT, */
/*                 .             CLKOUT, FOUND                        ) */

/*                  IF ( .NOT. FOUND ) THEN */

/*                     [Handle this case...] */

/*                  END IF */

/*            C */
/*            C     Negate the spacecraft-to-target body vector and */
/*            C     convert it to camera coordinates. */
/*            C */
/*                  CALL VMINUS ( STATE, DIR       ) */
/*                  CALL MXV    ( CMAT,  DIR,  DIR ) */

/*            C */
/*            C     If FL is the camera's focal length, the effective */
/*            C     focal point is */
/*            C */
/*            C        FL * ( 0, 0, 1 ) */
/*            C */
/*                  CALL VSCL ( FL, ZVEC, FOCUS ) */

/*            C */
/*            C     The camera's focal plane contains the origin in */
/*            C     camera coordinates, and the z-vector is orthogonal */
/*            C     to the plane.  Make a SPICE plane representing */
/*            C     the focal plane. */
/*            C */
/*                  CALL NVC2PL ( ZVEC, 0.D0, FPLANE ) */

/*            C */
/*            C     The image of the target body's center in the focal */
/*            C     plane is defined by the intersection with the focal */
/*            C     plane of the ray whose vertex is the focal point and */
/*            C     whose direction is DIR. */
/*            C */
/*                  CALL INRYPL ( FOCUS, DIR, FPLANE, NXPTS, IMAGE ) */

/*                  IF ( NXPTS .EQ. 1 ) THEN */
/*            C */
/*            C        The body center does project to the focal plane. */
/*            C        Check whether the image is actually in the */
/*            C        camera's field of view... */
/*            C */
/*                               . */
/*                               . */
/*                               . */
/*                  ELSE */

/*            C */
/*            C        The body center does not map to the focal plane. */
/*            C        Handle this case... */
/*            C */
/*                               . */
/*                               . */
/*                               . */
/*                  END IF */



/*     2)  Find the Saturn ring plane intercept of a spacecraft-mounted */
/*         instrument's boresight vector.  We want the find the point */
/*         in the ring plane that will be observed by an instrument */
/*         with a give boresight direction at a specified time.  We */
/*         must account for light time and stellar aberration in order */
/*         to find this point.  The intercept point will be expressed */
/*         in Saturn body-fixed coordinates. */

/*         In this example, we assume */

/*            -- The ring plane is equatorial. */

/*            -- Light travels in a straight line. */

/*            -- The light time correction for the ring plane intercept */
/*               can be obtained by performing three light-time */
/*               correction iterations.  If this assumption does not */
/*               lead to a sufficiently accurate result, additional */
/*               iterations can be performed. */

/*            -- A Newtonian approximation of stellar aberration */
/*               suffices. */

/*            -- The boresight vector is given in J2000 coordinates. */

/*            -- The observation epoch is ET ephemeris seconds past */
/*               J2000. */

/*            -- The boresight vector, spacecraft and planetary */
/*               ephemerides, and ring plane orientation are all known */
/*               with sufficient accuracy for the application. */

/*            -- All necessary kernels are loaded by the caller of */
/*               this example routine. */


/*            SUBROUTINE RING_XPT ( SC, ET, BORVEC, SBFXPT, FOUND ) */
/*            IMPLICIT NONE */

/*            CHARACTER*(*)         SC */
/*            DOUBLE PRECISION      ET */
/*            DOUBLE PRECISION      BORVEC ( 3 ) */
/*            DOUBLE PRECISION      SBFXPT ( 3 ) */
/*            LOGICAL               FOUND */

/*      C */
/*      C     SPICELIB functions */
/*      C */
/*            DOUBLE PRECISION      CLIGHT */
/*            DOUBLE PRECISION      VDIST */

/*      C */
/*      C     Local parameters */
/*      C */
/*            INTEGER               UBPL */
/*            PARAMETER           ( UBPL = 4 ) */

/*            INTEGER               SATURN */
/*            PARAMETER           ( SATURN = 699 ) */

/*      C */
/*      C     Local variables */
/*      C */
/*            DOUBLE PRECISION      BORV2  ( 3 ) */
/*            DOUBLE PRECISION      CORVEC ( 3 ) */
/*            DOUBLE PRECISION      LT */
/*            DOUBLE PRECISION      PLANE  ( UBPL ) */
/*            DOUBLE PRECISION      SATSSB ( 6 ) */
/*            DOUBLE PRECISION      SCPOS  ( 3 ) */
/*            DOUBLE PRECISION      SCSSB  ( 6 ) */
/*            DOUBLE PRECISION      STATE  ( 6 ) */
/*            DOUBLE PRECISION      STCORR ( 3 ) */
/*            DOUBLE PRECISION      TAU */
/*            DOUBLE PRECISION      TPMI   ( 3,  3 ) */
/*            DOUBLE PRECISION      XPT    ( 3 ) */
/*            DOUBLE PRECISION      ZVEC   ( 3 ) */

/*            INTEGER               I */
/*            INTEGER               NXPTS */
/*            INTEGER               SCID */

/*            LOGICAL               FND */

/*      C */
/*      C     First step:  account for stellar aberration.  Since the */
/*      C     instrument pointing is given, we need to find the intercept */
/*      C     point such that, when the stellar aberration correction is */
/*      C     applied to the vector from the spacecraft to that point, */
/*      C     the resulting vector is parallel to BORVEC.  An easy */
/*      C     solution is to apply the inverse of the normal stellar */
/*      C     aberration correction to BORVEC, and then solve the */
/*      C     intercept problem with this corrected boresight vector. */
/*      C */
/*      C     Find the position of the observer relative */
/*      C     to the solar system barycenter at ET. */
/*      C */
/*            CALL BODN2C ( SC, SCID, FND ) */

/*            IF ( .NOT. FND ) THEN */

/*               CALL SETMSG ( 'ID code for body # was not found.' ) */
/*               CALL ERRCH  ( '#',  SC                            ) */
/*               CALL SIGERR ( 'SPICE(NOTRANSLATION'               ) */
/*               RETURN */

/*            END IF */

/*            CALL SPKSSB ( SCID, ET, 'J2000', SCSSB ) */

/*      C */
/*      C     We now wish to find the vector CORVEC that, when */
/*      C     corrected for stellar aberration, yields BORVEC. */
/*      C     A good first approximation is obtained by applying */
/*      C     the stellar aberration correction for transmission */
/*      C     to BORVEC. */
/*      C */
/*            CALL STLABX ( BORVEC, SCSSB(4), CORVEC ) */

/*      C */
/*      C     The inverse of the stellar aberration correction */
/*      C     applicable to CORVEC should be a very good estimate of */
/*      C     the correction we need to apply to BORVEC. Apply */
/*      C     this correction to BORVEC to obtain an improved estimate */
/*      C     of CORVEC. */
/*      C */
/*            CALL STELAB ( CORVEC, SCSSB(4), BORV2  ) */
/*            CALL VSUB   ( BORV2,  CORVEC,   STCORR ) */
/*            CALL VSUB   ( BORVEC, STCORR,   CORVEC ) */

/*      C */
/*      C     Because the ring plane intercept may be quite far from */
/*      C     Saturn's center, we cannot assume light time from the */
/*      C     intercept to the observer is well approximated by */
/*      C     light time from Saturn's center to the observer. */
/*      C     We compute the light time explicitly using an iterative */
/*      C     approach. */
/*      C */
/*      C     We can however use the light time from Saturn's center to */
/*      C     the observer to obtain a first estimate of the actual light */
/*      C     time. */
/*      C */
/*            CALL SPKEZR ( 'SATURN', ET, 'J2000', 'LT', SC, */
/*           .               STATE,   LT                       ) */
/*            TAU = LT */

/*      C */
/*      C     Find the ring plane intercept and calculate the */
/*      C     light time from it to the spacecraft. */
/*      C     Perform three iterations. */
/*      C */
/*            I     = 1 */
/*            FOUND = .TRUE. */

/*            DO WHILE (  ( I .LE. 3 )  .AND.  ( FOUND )  ) */
/*      C */
/*      C        Find the position of Saturn relative */
/*      C        to the solar system barycenter at ET-TAU. */
/*      C */
/*               CALL SPKSSB ( SATURN, ET-TAU, 'J2000', SATSSB ) */

/*      C */
/*      C        Find the Saturn-to-observer vector defined by these */
/*      C        two position vectors. */
/*      C */
/*               CALL VSUB ( SCSSB, SATSSB, SCPOS ) */

/*      C */
/*      C        Look up Saturn's pole at ET-TAU; this is the third */
/*      C        column of the matrix that transforms Saturn body-fixed */
/*      C        coordinates to J2000 coordinates. */
/*      C */
/*               CALL PXFORM ( 'IAU_SATURN', 'J2000', ET-TAU, TPMI ) */

/*               CALL MOVED  ( TPMI(1,3), 3, ZVEC ) */

/*      C */
/*      C        Make a SPICE plane representing the ring plane. */
/*      C        We're treating Saturn's center as the origin, so */
/*      C        the plane constant is 0. */
/*      C */
/*               CALL NVC2PL ( ZVEC, 0.D0, PLANE ) */

/*      C */
/*      C        Find the intersection of the ring plane and the */
/*      C        ray having vertex SCPOS and direction vector */
/*      C        CORVEC. */
/*      C */
/*               CALL INRYPL ( SCPOS, CORVEC, PLANE, NXPTS, XPT ) */

/*      C */
/*      C        If the number of intersection points is 1, */
/*      C        find the next light time estimate. */
/*      C */
/*               IF ( NXPTS .EQ. 1 ) THEN */
/*      C */
/*      C           Find the light time (zero-order) from the */
/*      C           intercept point to the spacecraft. */
/*      C */
/*                  TAU  =  VDIST ( SCPOS, XPT )  /  CLIGHT() */
/*                  I    =  I + 1 */

/*               ELSE */

/*                  FOUND = .FALSE. */

/*               END IF */

/*            END DO */

/*      C */
/*      C     At this point, if FOUND is .TRUE., we iterated */
/*      C     3 times, and XPT is our estimate of the */
/*      C     position of the ring plane intercept point */
/*      C     relative to Saturn in the J2000 frame. This is the */
/*      C     point observed by an instrument pointed in direction */
/*      C     BORVEC at ET at mounted on the spacecraft SC. */
/*      C */
/*      C     If FOUND is .FALSE., the boresight ray does not */
/*      C     intersect the ring plane. */
/*      C */
/*      C     As a final step, transform XPT to Saturn body-fixed */
/*      C     coordinates. */
/*      C */
/*            IF ( FOUND ) THEN */

/*               CALL MTXV ( TPMI, XPT, SBFXPT ) */

/*            END IF */

/*            END */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     B.V. Semenov       (JPL) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.3.0, 24-AUG-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 1.2.0, 29-SEP-2016 (NJB) */

/*        Changed from standard to discovery check-in. Fixed typo */
/*        in header. */

/* -    SPICELIB Version 1.1.1, 07-FEB-2008 (BVS) */

/*        Fixed a few typos in the header. */

/* -    SPICELIB Version 1.1.0, 02-SEP-2005 (NJB) */

/*        Updated to remove non-standard use of duplicate arguments */
/*        in VSCL call. */

/* -    SPICELIB Version 1.0.3, 12-DEC-2002 (NJB) */

/*        Header fix: ring plane intercept algorithm was corrected. */
/*        Now light time is computed accurately, and stellar aberration */
/*        is accounted for. Example was turned into a complete */
/*        subroutine. */

/* -    SPICELIB Version 1.0.2, 09-MAR-1999 (NJB) */

/*        Reference to SCE2T replaced by reference to SCE2C. An */
/*        occurrence of ENDIF was replaced by END IF. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 01-APR-1991 (NJB) (WLT) */

/* -& */
/* $ Index_Entries */

/*     intersection of ray and plane */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     Local variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }

/*     We'll give the name TOOBIG to the bound DPMAX() / MARGIN. */
/*     If we let VTXPRJ be the orthogonal projection of VERTEX onto */
/*     PLANE, and let DIFF be the vector VTXPRJ - VERTEX, then */
/*     we know that */

/*        ||  DIFF  ||    <     2 * TOOBIG */

/*     Check the distance of the ray's vertex from the origin. */

    toobig = dpmax_() / 3.;
    if (vnorm_(vertex) >= toobig) {
	chkin_("INRYPL", (ftnlen)6);
	setmsg_("Ray's vertex is too far from the origin.", (ftnlen)40);
	sigerr_("SPICE(VECTORTOOBIG)", (ftnlen)19);
	chkout_("INRYPL", (ftnlen)6);
	return 0;
    }

/*     Check the distance of the plane from the origin.  (The returned */
/*     plane constant IS this distance.) */

    pl2nvc_(plane, normal, &const__);
    if (const__ >= toobig) {
	chkin_("INRYPL", (ftnlen)6);
	setmsg_("Plane is too far from the origin.", (ftnlen)33);
	sigerr_("SPICE(VECTORTOOBIG)", (ftnlen)19);
	chkout_("INRYPL", (ftnlen)6);
	return 0;
    }

/*     Check the ray's direction vector. */

    vhat_(dir, udir);
    if (vzero_(udir)) {
	chkin_("INRYPL", (ftnlen)6);
	setmsg_("Ray's direction vector is the zero vector.", (ftnlen)42);
	sigerr_("SPICE(ZEROVECTOR)", (ftnlen)17);
	chkout_("INRYPL", (ftnlen)6);
	return 0;
    }

/*     That takes care of the error cases.  Now scale the input vertex */
/*     and plane to improve numerical behavior. */

/* Computing MAX */
    d__1 = const__, d__2 = vnorm_(vertex);
    mscale = max(d__1,d__2);
    if (mscale != 0.) {
	d__1 = 1. / mscale;
	vscl_(&d__1, vertex, sclvtx);
	sclcon = const__ / mscale;
    } else {
	vequ_(vertex, sclvtx);
	sclcon = const__;
    }
    if (mscale > 1.) {
	toobig /= mscale;
    }
/*     Find the projection (coefficient) of the ray's vertex along the */
/*     plane's normal direction. */

    prjvn = vdot_(sclvtx, normal);

/*     If this projection is the plane constant, the ray's vertex lies in */
/*     the plane.  We have one intersection or an infinite number of */
/*     intersections.  It all depends on whether the ray actually lies */
/*     in the plane. */

/*     The absolute value of PRJDIF is the distance of the ray's vertex */
/*     from the plane. */

    prjdif = sclcon - prjvn;
    if (prjdif == 0.) {

/*        XPT is the original, unscaled vertex. */

	vequ_(vertex, xpt);
	if (vdot_(normal, udir) == 0.) {

/*           The ray's in the plane. */

	    *nxpts = -1;
	} else {
	    *nxpts = 1;
	}
	return 0;
    }

/*     Ok, the ray's vertex is not in the plane.  The ray may still be */
/*     parallel to or may point away from the plane.  If the ray does */
/*     point towards the plane, mathematicians would say that the */
/*     ray does intersect the plane, but the computer may disagree. */

/*     For this routine to find an intersection, both of the following */
/*     conditions must be met: */

/*        -- The ray must point toward the plane; this happens when */
/*           PRJDIF has the same sign as < UDIR, NORMAL >. */

/*        -- The vector difference XPT - SCLVTX must not overflow. */

/*      Qualitatively, the case of interest looks something like the */
/*      picture below: */


/*                      * SCLVTX */
/*                      |\ */
/*                      | \   <-- UDIR */
/*                      |  \ */
/*    length of this    |   \| */
/*      segment is      |   -* */
/*                      | */
/*     | PRJDIF |   --> | ___________________________ */
/*                      |/                          / */
/*                      |       *                  /   <-- PLANE */
/*                     /|        XPT              / */
/*                    / ^                        / */
/*                   /  | NORMAL                / */
/*                  /   | .                    / */
/*                 /    |/|                   / */
/*                / .---| /                  / */
/*               /  |   |/                  / */
/*              /   `---*                  / */
/*             /          Projection of SCLVTX onto the plane */
/*            /                          / */
/*           /                          / */
/*          ---------------------------- */




/*     Find the projection of the direction vector along the plane's */
/*     normal vector. */

    prjdir = vdot_(udir, normal);

/*     We're done if the ray doesn't point toward the plane.  PRJDIF */
/*     has already been found to be non-zero at this point; PRJDIR is */
/*     zero if the ray and plane are parallel.  The SPICELIB routine */
/*     SMSGND will return a value of .FALSE. if PRJDIR is zero. */

    if (! smsgnd_(&prjdir, &prjdif)) {

/*        The ray is parallel to or points away from the plane. */

	*nxpts = 0;
	cleard_(&c__3, xpt);
	return 0;
    }

/*     The difference XPT - SCLVTX is the hypotenuse of a right triangle */
/*     formed by SCLVTX, XPT, and the orthogonal projection of SCLVTX */
/*     onto the plane.  We'll obtain the hypotenuse by scaling UDIR. */
/*     We must make sure that this hypotenuse does not overflow.  The */
/*     scale factor has magnitude */

/*         | PRJDIF | */
/*       -------------- */
/*         | PRJDIR | */

/*     and UDIR is a unit vector, so as long as */

/*         | PRJDIF |   <   | PRJDIR |  *  TOOBIG */

/*     the hypotenuse is no longer than TOOBIG.  The product can be */
/*     computed safely since PRJDIR has magnitude 1 or less. */

    if (abs(prjdif) >= abs(prjdir) * toobig) {

/*        If the hypotenuse is too long, we say that no intersection */
/*        exists. */

	*nxpts = 0;
	cleard_(&c__3, xpt);
	return 0;
    }

/*     We conclude that it's safe to compute XPT.  Scale UDIR and add */
/*     the result to SCLVTX.  The addition is safe because both addends */
/*     have magnitude no larger than TOOBIG.  The vector thus obtained */
/*     is the intersection point. */

    *nxpts = 1;
    scale = abs(prjdif) / abs(prjdir);
    vlcom_(&c_b16, sclvtx, &scale, udir, xpt);

/*     Re-scale XPT.  This is safe, since TOOBIG has already been */
/*     scaled to allow for any growth of XPT at this step. */

    vsclip_(&mscale, xpt);
    return 0;
} /* inrypl_ */

