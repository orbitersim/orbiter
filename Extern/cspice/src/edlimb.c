/* edlimb.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static doublereal c_b18 = 1.;
static integer c__9 = 9;

/* $Procedure EDLIMB   ( Ellipsoid Limb ) */
/* Subroutine */ int edlimb_(doublereal *a, doublereal *b, doublereal *c__, 
	doublereal *viewpt, doublereal *limb)
{
    /* System generated locals */
    doublereal d__1, d__2, d__3;

    /* Local variables */
    doublereal scla, sclb, sclc;
    extern /* Subroutine */ int vscl_(doublereal *, doublereal *, doublereal *
	    );
    doublereal scla2, sclb2, sclc2, v[3], scale;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    doublereal level;
    extern /* Subroutine */ int moved_(doublereal *, integer *, doublereal *);
    logical found;
    extern /* Subroutine */ int errdp_(char *, doublereal *, ftnlen), vsclg_(
	    doublereal *, doublereal *, integer *, doublereal *);
    doublereal tmpel[9];
    extern /* Subroutine */ int nvc2pl_(doublereal *, doublereal *, 
	    doublereal *);
    doublereal lplane[4];
    extern /* Subroutine */ int inedpl_(doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, logical *);
    doublereal normal[3];
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), setmsg_(char *, ftnlen);
    extern logical return_(void);

/* $ Abstract */

/*     Find the limb of a triaxial ellipsoid, viewed from a specified */
/*     point. */

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

/*     ELLIPSE */
/*     ELLIPSOID */
/*     GEOMETRY */
/*     MATH */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     A          I   Length of ellipsoid semi-axis lying on the x-axis. */
/*     B          I   Length of ellipsoid semi-axis lying on the y-axis. */
/*     C          I   Length of ellipsoid semi-axis lying on the z-axis. */
/*     VIEWPT     I   Location of viewing point. */
/*     LIMB       O   Limb of ellipsoid as seen from viewing point. */

/* $ Detailed_Input */

/*     A, */
/*     B, */
/*     C        are the lengths of the semi-axes of a triaxial */
/*              ellipsoid. The ellipsoid is centered at the */
/*              origin and oriented so that its axes lie on the */
/*              x, y and z axes.  A, B, and C are the lengths of */
/*              the semi-axes that point in the x, y, and z */
/*              directions respectively. */

/*     VIEWPT   is a point from which the ellipsoid is viewed. */
/*              VIEWPT must be outside of the ellipsoid. */

/* $ Detailed_Output */

/*     LIMB     is a SPICE ellipse that represents the limb of */
/*              the ellipsoid. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the length of any semi-axis of the ellipsoid is */
/*         non-positive, the error SPICE(INVALIDAXISLENGTH) is signaled. */
/*         LIMB is not modified. */

/*     2)  If the length of any semi-axis of the ellipsoid is zero after */
/*         the semi-axis lengths are scaled by the reciprocal of the */
/*         magnitude of the longest semi-axis and then squared, the error */
/*         SPICE(DEGENERATECASE) is signaled. LIMB is not modified. */

/*     3)  If the viewing point VIEWPT is inside the ellipse, the error */
/*         SPICE(INVALIDPOINT) is signaled. LIMB is not modified. */

/*     4)  If the geometry defined by the input ellipsoid and viewing */
/*         point is so extreme that the limb cannot be found, the error */
/*         SPICE(DEGENERATECASE) is signaled. */

/*     5)  If the shape of the ellipsoid and the viewing geometry are */
/*         such that the limb is an excessively flat ellipsoid, the */
/*         limb may be a degenerate ellipse. You must determine whether */
/*         this possibility poses a problem for your application. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     The limb of a body, as seen from a viewing point, is the boundary */
/*     of the portion of the body's surface that is visible from that */
/*     viewing point. In this definition, we consider a surface point */
/*     to be `visible' if it can be connected to the viewing point by a */
/*     line segment that doesn't pass through the body. This is a purely */
/*     geometrical definition that ignores the matter of which portions */
/*     of the surface are illuminated, or whether the view is obscured by */
/*     any additional objects. */

/*     If a body is modeled as a triaxial ellipsoid, the limb is always */
/*     an ellipse. The limb is determined by its center, a semi-major */
/*     axis vector, and a semi-minor axis vector. */

/*     We note that the problem of finding the limb of a triaxial */
/*     ellipsoid is mathematically identical to that of finding its */
/*     terminator, if one makes the simplifying assumption that the */
/*     terminator is the limb of the body as seen from the vertex of the */
/*     umbra. So, this routine can be used to solve this simplified */
/*     version of the problem of finding the terminator. */

/* $ Examples */

/*     The numerical results shown for these examples may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */

/*     1) Given an ellipsoid and a viewpoint exterior to it, calculate */
/*        the limb ellipse as seen from that viewpoint. */


/*        Example code begins here. */


/*              PROGRAM EDLIMB_EX1 */
/*              IMPLICIT NONE */

/*        C */
/*        C     Local constants. */
/*        C */
/*              INTEGER                 UBEL */
/*              PARAMETER             ( UBEL =   9 ) */

/*        C */
/*        C     Local variables. */
/*        C */
/*              DOUBLE PRECISION        A */
/*              DOUBLE PRECISION        B */
/*              DOUBLE PRECISION        C */
/*              DOUBLE PRECISION        ECENTR ( 3    ) */
/*              DOUBLE PRECISION        LIMB   ( UBEL ) */
/*              DOUBLE PRECISION        SMAJOR ( 3    ) */
/*              DOUBLE PRECISION        SMINOR ( 3    ) */
/*              DOUBLE PRECISION        VIEWPT ( 3    ) */

/*        C */
/*        C     Define a viewpoint exterior to the ellipsoid. */
/*        C */
/*              DATA                    VIEWPT /  2.D0,  0.D0,  0.D0 / */

/*        C */
/*        C     Define an ellipsoid. */
/*        C */
/*              A = SQRT( 2.D0 ) */
/*              B = 2.D0 * SQRT( 2.D0 ) */
/*              C = SQRT( 2.D0 ) */

/*        C */
/*        C     Calculate the limb ellipse as seen by from the */
/*        C     viewpoint. */
/*        C */
/*              CALL EDLIMB ( A, B, C, VIEWPT, LIMB ) */

/*        C */
/*        C     Output the structure components. */
/*        C */
/*              CALL EL2CGV ( LIMB, ECENTR, SMAJOR, SMINOR ) */

/*              WRITE(*,'(A)') 'Limb ellipse as seen from viewpoint:' */
/*              WRITE(*,'(A,3F11.6)') '   Semi-minor axis:', SMINOR */
/*              WRITE(*,'(A,3F11.6)') '   Semi-major axis:', SMAJOR */
/*              WRITE(*,'(A,3F11.6)') '   Center         :', ECENTR */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*        Limb ellipse as seen from viewpoint: */
/*           Semi-minor axis:   0.000000   0.000000  -1.000000 */
/*           Semi-major axis:   0.000000   2.000000  -0.000000 */
/*           Center         :   1.000000   0.000000   0.000000 */


/*     2) We'd like to find the apparent limb of Jupiter, corrected for */
/*        light time and stellar aberration, as seen from JUNO */
/*        spacecraft's position at a given UTC time. */

/*        Use the meta-kernel shown below to load the required SPICE */
/*        kernels. */


/*           KPL/MK */

/*           File name: edlimb_ex2.tm */

/*           This meta-kernel is intended to support operation of SPICE */
/*           example programs. The kernels shown here should not be */
/*           assumed to contain adequate or correct versions of data */
/*           required by SPICE-based user applications. */

/*           In order for an application to use this meta-kernel, the */
/*           kernels referenced here must be present in the user's */
/*           current working directory. */

/*           The names and contents of the kernels referenced */
/*           by this meta-kernel are as follows: */

/*              File name                           Contents */
/*              ---------                           -------- */
/*              juno_rec_160522_160729_160909.bsp   JUNO s/c ephemeris */
/*              pck00010.tpc                        Planet orientation */
/*                                                  and radii */
/*              naif0012.tls                        Leapseconds */

/*           \begindata */

/*              KERNELS_TO_LOAD = ( 'juno_rec_160522_160729_160909.bsp', */
/*                                  'pck00010.tpc', */
/*                                  'naif0012.tls'  ) */

/*           \begintext */

/*           End of meta-kernel */


/*        Example code begins here. */


/*              PROGRAM EDLIMB_EX2 */
/*              IMPLICIT NONE */

/*        C */
/*        C     Local parameters. */
/*        C */
/*              CHARACTER*(*)           UTCSTR */
/*              PARAMETER             ( UTCSTR = '2016 Jul 14 19:45:00' ) */

/*              INTEGER                 UBEL */
/*              PARAMETER             ( UBEL =   9 ) */

/*        C */
/*        C     Local variables. */
/*        C */
/*              DOUBLE PRECISION        CENTER ( 3    ) */
/*              DOUBLE PRECISION        ET */
/*              DOUBLE PRECISION        JPOS   ( 3    ) */
/*              DOUBLE PRECISION        LIMB   ( UBEL ) */
/*              DOUBLE PRECISION        LT */
/*              DOUBLE PRECISION        RAD    ( 3    ) */
/*              DOUBLE PRECISION        SMAJOR ( 3    ) */
/*              DOUBLE PRECISION        SMINOR ( 3    ) */
/*              DOUBLE PRECISION        SCPJFC ( 3    ) */
/*              DOUBLE PRECISION        SCPOS  ( 3    ) */
/*              DOUBLE PRECISION        TIPM   ( 3, 3 ) */

/*              INTEGER                 N */

/*        C */
/*        C     Load the required kernels. */
/*        C */
/*              CALL FURNSH ( 'edlimb_ex2.tm' ) */

/*        C */
/*        C     Find the viewing point in Jupiter-fixed coordinates. To */
/*        C     do this, find the apparent position of Jupiter as seen */
/*        C     from the spacecraft in Jupiter-fixed coordinates and */
/*        C     negate this vector. In this case we'll use light time */
/*        C     and stellar aberration corrections to arrive at the */
/*        C     apparent limb. JPOS is the Jupiter's position as seen */
/*        C     from the spacecraft.  SCPOS is the spacecraft's position */
/*        C     relative to Jupiter. */
/*        C */
/*              CALL STR2ET ( UTCSTR,    ET ) */
/*              CALL SPKPOS ( 'JUPITER', ET, 'J2000', 'LT+S', 'JUNO', */
/*             .               JPOS,     LT                         ) */

/*              CALL VMINUS ( JPOS, SCPOS ) */

/*        C */
/*        C     Get Jupiter's semi-axis lengths... */
/*        C */
/*              CALL BODVRD ( 'JUPITER', 'RADII', 3, N, RAD ) */

/*        C */
/*        C     ...and the transformation from J2000 to Jupiter */
/*        C     equator and prime meridian coordinates. Note that we */
/*        C     use the orientation of Jupiter at the time of */
/*        C     emission of the light that arrived at the */
/*        C     spacecraft at time ET. */
/*        C */
/*              CALL PXFORM ( 'J2000', 'IAU_JUPITER', ET-LT, TIPM ) */

/*        C */
/*        C     Transform the spacecraft's position into Jupiter- */
/*        C     fixed coordinates. */
/*        C */
/*              CALL MXV ( TIPM, SCPOS, SCPJFC ) */

/*        C */
/*        C     Find the apparent limb.  LIMB is a SPICE ellipse */
/*        C     representing the limb. */
/*        C */
/*              CALL EDLIMB ( RAD(1), RAD(2), RAD(3), SCPJFC, LIMB ) */

/*        C */
/*        C     CENTER, SMAJOR, and SMINOR are the limb's center, */
/*        C     semi-major axis of the limb, and a semi-minor axis */
/*        C     of the limb.  We obtain these from LIMB using the */
/*        C     SPICELIB routine EL2CGV ( Ellipse to center and */
/*        C     generating vectors ). */
/*        C */
/*              CALL EL2CGV ( LIMB, CENTER, SMAJOR, SMINOR ) */

/*        C */
/*        C     Output the structure components. */
/*        C */
/*              WRITE(*,'(A)') 'Apparent limb of Jupiter as seen ' */
/*             .            // 'from JUNO:' */
/*              WRITE(*,'(2A)')       '   UTC time       : ', UTCSTR */
/*              WRITE(*,'(A,3F14.6)') '   Semi-minor axis:', SMINOR */
/*              WRITE(*,'(A,3F14.6)') '   Semi-major axis:', SMAJOR */
/*              WRITE(*,'(A,3F14.6)') '   Center         :', CENTER */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*        Apparent limb of Jupiter as seen from JUNO: */
/*           UTC time       : 2016 Jul 14 19:45:00 */
/*           Semi-minor axis:  12425.547643  -5135.572410  65656.053303 */
/*           Semi-major axis:  27305.667297  66066.222576      0.000000 */
/*           Center         :    791.732472   -327.228993   -153.408849 */


/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.4.0, 24-AUG-2021 (NJB) (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. */
/*        Added complete code example. */

/*        Corrected several header comment typos. */

/* -    SPICELIB Version 1.3.0, 23-OCT-2005 (NJB) */

/*        Updated to remove non-standard use of duplicate arguments */
/*        in VSCLG call. Updated header to refer to BODVCD instead */
/*        of BODVAR. */

/* -    SPICELIB Version 1.2.0, 06-OCT-1993 (NJB) */

/*        Declaration of unused local variable NEAR was removed. */

/* -    SPICELIB Version 1.1.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.1.0, 04-DEC-1990 (NJB) */

/*        Error message and description changed for non-positive */
/*        axis length error. */

/* -    SPICELIB Version 1.0.0, 02-NOV-1990 (NJB) */

/* -& */
/* $ Index_Entries */

/*     ellipsoid limb */

/* -& */
/* $ Revisions */

/* -    SPICELIB Version 1.1.0, 04-DEC-1990 (NJB) */

/*        Error message and description changed for non-positive */
/*        axis length error. The former message and description did */
/*        not match, and the description was incorrect: it described */
/*        `zero-length', rather than `non-positive' axes as invalid. */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     Local variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("EDLIMB", (ftnlen)6);
    }

/*     The semi-axes must have positive length. */

    if (*a <= 0. || *b <= 0. || *c__ <= 0.) {
	setmsg_("Semi-axis lengths:  A = #, B = #, C = #. ", (ftnlen)41);
	errdp_("#", a, (ftnlen)1);
	errdp_("#", b, (ftnlen)1);
	errdp_("#", c__, (ftnlen)1);
	sigerr_("SPICE(INVALIDAXISLENGTH)", (ftnlen)24);
	chkout_("EDLIMB", (ftnlen)6);
	return 0;
    }

/*     Scale the semi-axes lengths for better numerical behavior. */
/*     If squaring any one of the scaled lengths causes it to */
/*     underflow to zero, we cannot continue the computation. Otherwise, */
/*     scale the viewing point too. */

/* Computing MAX */
    d__1 = abs(*a), d__2 = abs(*b), d__1 = max(d__1,d__2), d__2 = abs(*c__);
    scale = max(d__1,d__2);
    scla = *a / scale;
    sclb = *b / scale;
    sclc = *c__ / scale;
/* Computing 2nd power */
    d__1 = scla;
    scla2 = d__1 * d__1;
/* Computing 2nd power */
    d__1 = sclb;
    sclb2 = d__1 * d__1;
/* Computing 2nd power */
    d__1 = sclc;
    sclc2 = d__1 * d__1;
    if (scla2 == 0. || sclb2 == 0. || sclc2 == 0.) {
	setmsg_("Semi-axis too small:  A = #, B = #, C = #. ", (ftnlen)43);
	errdp_("#", a, (ftnlen)1);
	errdp_("#", b, (ftnlen)1);
	errdp_("#", c__, (ftnlen)1);
	sigerr_("SPICE(DEGENERATECASE)", (ftnlen)21);
	chkout_("EDLIMB", (ftnlen)6);
	return 0;
    }
    d__1 = 1. / scale;
    vscl_(&d__1, viewpt, v);

/*     The viewing point must be outside of the ellipsoid.  LEVEL is the */
/*     constant of the level surface that V lies on.  The ellipsoid */
/*     itself is the level surface corresponding to LEVEL = 1. */

/* Computing 2nd power */
    d__1 = v[0];
/* Computing 2nd power */
    d__2 = v[1];
/* Computing 2nd power */
    d__3 = v[2];
    level = d__1 * d__1 / scla2 + d__2 * d__2 / sclb2 + d__3 * d__3 / sclc2;
    if (level < 1.) {
	setmsg_("Viewing point is inside the ellipsoid.", (ftnlen)38);
	sigerr_("SPICE(DEGENERATECASE)", (ftnlen)21);
	chkout_("EDLIMB", (ftnlen)6);
	return 0;
    }

/*     Find a normal vector for the limb plane. */

/*     To compute this vector, we use the fact that the surface normal at */
/*     each limb point is orthogonal to the line segment connecting the */
/*     viewing point and the limb point.   Let the notation */

/*        < a, b > */

/*     indicate the dot product of the vectors a and b.  If we call the */
/*     viewing point V and the limb point X, then */



/*                            X(1)         X(2)         X(3) */
/*        0  =   < V - X,  ( -------- ,   -------- ,   --------  )  > */
/*                                2           2             2 */
/*                            SCLA        SCLB          SCLC */


/*                            X(1)         X(2)         X(3) */
/*           =   <   V,    ( -------- ,   -------- ,   --------  )  > */
/*                                2           2             2 */
/*                            SCLA        SCLB          SCLC */


/*                            X(1)         X(2)         X(3) */
/*            - <   X,    ( -------- ,   -------- ,   --------  )  > */
/*                                2           2             2 */
/*                            SCLA        SCLB          SCLC */

/*                                2           2             2 */
/*                            X(1)        X(2)          X(3) */
/*           =             --------  +   --------  +  -------- */
/*                               2            2             2 */
/*                           SCLA         SCLB          SCLC */


/*           =   1 */


/*     This last equation is just the equation of the scaled ellipsoid. */
/*     We can combine the last two equalities and interchange the */
/*     positions of X and V to obtain */


/*                      V(1)         V(2)         V(3) */
/*        <   X,    ( -------- ,   -------- ,   --------  )  >   =   1 */
/*                          2           2             2 */
/*                      SCLA        SCLB          SCLC */


/*     This is the equation of the limb plane. */


/*     Put together a SPICE plane, LPLANE, that represents the limb */
/*     plane. */

    normal[0] = v[0] / scla2;
    normal[1] = v[1] / sclb2;
    normal[2] = v[2] / sclc2;
    nvc2pl_(normal, &c_b18, lplane);

/*     Find the limb by intersecting the limb plane with the ellipsoid. */

    inedpl_(&scla, &sclb, &sclc, lplane, limb, &found);

/*     FOUND should be true unless we've encountered numerical problems. */

    if (! found) {
	setmsg_("Ellipsoid shape and viewing geometry are too extreme; the l"
		"imb was not found. ", (ftnlen)78);
	sigerr_("SPICE(DEGENERATECASE)", (ftnlen)21);
	chkout_("EDLIMB", (ftnlen)6);
	return 0;
    }

/*     Undo the scaling before returning the limb. */

    vsclg_(&scale, limb, &c__9, tmpel);
    moved_(tmpel, &c__9, limb);
    chkout_("EDLIMB", (ftnlen)6);
    return 0;
} /* edlimb_ */

