/* zzeprc76.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__3 = 3;
static integer c__2 = 2;

/* $Procedure   ZZEPRC76   ( Earth precession, 1976 IAU model ) */
/* Subroutine */ int zzeprc76_(doublereal *et, doublereal *precxf)
{
    doublereal cent, zeta, t, scale, z__, theta, dzeta;
    extern doublereal jyear_(void);
    extern /* Subroutine */ int eul2xf_(doublereal *, integer *, integer *, 
	    integer *, doublereal *);
    doublereal dz, ts, dtheta, eulang[6];
    extern doublereal rpd_(void);

/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines.  Users should not call this routine directly due */
/*     to the volatile nature of this routine. */

/*     Compute the state transformation matrix implementing the IAU 1976 */
/*     precession model. */

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

/*     ROTATION */

/* $ Keywords */

/*     FRAMES */
/*     GEOMETRY */
/*     MATRIX */
/*     PRIVATE */
/*     TRANSFORMATION */

/* $ Declarations */
/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     ET         I   Ephemeris time, in seconds past J2000 TDB. */
/*     PRECXF     O   Precession state transformation matrix at ET. */

/* $ Detailed_Input */

/*     ET             is the epoch at which the precession matrix is */
/*                    to be computed.  ET is barycentric dynamical time, */
/*                    expressed as seconds past J2000. */

/* $ Detailed_Output */

/*     PRECXF         is a 6x6 matrix that transforms states from the */
/*                    J2000 frame to the mean equator and equinox frame */
/*                    of the earth at the epoch ET. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     According to reference [2], the precession model used in this */
/*     routine is that used in the JPL navigation program "Regres." */

/*     The precession matrix is defined using the Euler angles */

/*        zeta ,   z ,  and theta */
/*            A     A            A */


/*     Equation (5-147) of [2] gives the matrix determined by these */
/*     angles as */

/*        A  =  [ -z   ]   [ theta  ]   [ -zeta  ] */
/*                  A   3         A  2         A  3 */


/*     Formulas for the Euler angles are from [2], equation */
/*     (5-143): */
/*                                              2                3 */
/*         zeta   =  2306".2181*T  +  0".30188*T   +  0".017998*T */
/*             A */


/*                                              2                3 */
/*         z      =  2306".2181*T  +  1".09468*T   +  0".018203*T */
/*          A */


/*                                              2                3 */
/*         theta  =  2004".3109*T  -  0".42665*T   -  0".041833*T */
/*              A */

/* $ Examples */

/*     1) Convert a state vector S from J2000 to Earth Mean equator and */
/*        equinox of date coordinates at epoch ET.  Call the resulting */
/*        vector SMOD. */

/*           CALL ZZEPRC76 ( ET,     PRECXF        ) */
/*           CALL MXVG     ( PRECXF, S, 6, 6, SMOD ) */

/* $ Restrictions */

/*     1) This is a SPICE private routine; the routine is subject to */
/*        change without notice.  User applications should not call this */
/*        routine. */

/*     2) Though reference [1] does not specify limitations on the range */
/*        of valid time inputs for this precession model, the fact that */
/*        the rotation angles used in the model are defined by */
/*        polynomials implies that the model is not valid for all time. */

/* $ Literature_References */

/*     [1] "Explanatory Supplement to the Astronomical Almanac" */
/*          edited by P. Kenneth Seidelmann. University Science */
/*          Books, 20 Edgehill Road, Mill Valley, CA 94941 (1992) */

/*     [2] "Section 5, Geocentric Space-Fixed Position, Velocity, and */
/*         Acceleration Vectors of Tracking Station" by T. D. Moyer. */
/*         Draft of JPL Publication documenting the JPL navigation */
/*         program "Regres." */


/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */

/* $ Version */

/* -    SPICELIB Version 2.0.1, 18-APR-2016 (NJB) */

/*        Corrected typo in header abstract: changed "1876" */
/*        to "1976." */

/* -    SPICELIB Version 2.0.0, 18-DEC-2004 (NJB) */

/* -& */
/* $ Index_Entries */

/*     IAU 1976 earth precession transformation */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     Local variables */


/*     No check-in required; this routine does not participate in */
/*     SPICELIB error handling. */


/*     Compute the precession angles first.  The time argument has */
/*     units of Julian centuries.  The polynomial expressions yield */
/*     angles in units of arcseconds prior to scaling.  After scaling, */
/*     the angles are in units of radians. */

    cent = jyear_() * 100.;
    t = *et / cent;
    scale = rpd_() / 3600.;
    zeta = t * (t * (t * .017998 + .30188) + 2306.2181) * scale;
    z__ = t * (t * (t * .018203 + 1.09468) + 2306.2181) * scale;
    theta = t * (t * (t * -.041833 - .42665) + 2004.3109) * scale;
    ts = 1. / cent;
    dzeta = ts * (t * (t * 3 * .017998 + .60375999999999996) + 2306.2181) * 
	    scale;
    dz = ts * (t * (t * 3 * .018203 + 2.1893600000000002) + 2306.2181) * 
	    scale;
    dtheta = ts * (t * (t * 3 * -.041833 - .85329999999999995) + 2004.3109) * 
	    scale;

/*     Now compute the precession matrix. */

    eulang[0] = -z__;
    eulang[1] = theta;
    eulang[2] = -zeta;
    eulang[3] = -dz;
    eulang[4] = dtheta;
    eulang[5] = -dzeta;
    eul2xf_(eulang, &c__3, &c__2, &c__3, precxf);
    return 0;
} /* zzeprc76_ */

