/* zzeprcss.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__3 = 3;
static integer c__2 = 2;

/* $Procedure   ZZEPRCSS   ( Earth precession, 1976 IAU model ) */
/* Subroutine */ int zzeprcss_(doublereal *et, doublereal *precm)
{
    /* System generated locals */
    doublereal d__1, d__2;

    /* Local variables */
    doublereal zeta;
    extern /* Subroutine */ int eul2m_(doublereal *, doublereal *, doublereal 
	    *, integer *, integer *, integer *, doublereal *);
    doublereal t, scale, z__, theta;
    extern doublereal jyear_(void), rpd_(void);

/* $ Abstract */

/*     Return the 1976 IAU Earth precession matrix for a specified time. */

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
/*     TRANSFORMATION */

/* $ Declarations */
/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     ET         I   Ephemeris time, in seconds past J2000. */
/*     PRECM      O   Precession matrix at ET. */

/* $ Detailed_Input */

/*     ET             is the epoch at which the precession matrix is */
/*                    to be computed.  ET is barycentric dynamical time, */
/*                    expressed as seconds past J2000. */

/* $ Detailed_Output */

/*     PRECM          is a 3x3 matrix representing the precession of */
/*                    the Earth from J2000 to the epoch ET.  The */
/*                    rows of PRECM are the basis vectors for the Earth */
/*                    mean equator and equinox frame of date, evaluated */
/*                    at ET. */

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

/*     1) Convert a vector V from J2000 to Earth Mean equator and equinox */
/*        of date coordinates at epoch ET.  Call the resulting vector */
/*        VMOD. */

/*           CALL ZZEPRCSS ( ET,    PRECM       ) */
/*           CALL MXV      ( PRECM, V,     VMOD ) */

/* $ Restrictions */

/*     1)  This is a preliminary version of the routine. */

/*     2)  Though reference [1] does not specify limitations on the */
/*         range of valid time inputs for this precession model, the */
/*         fact that the rotation angles used in the model are defined */
/*         by polynomials implies that the model is not valid for all */
/*         time. */

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

/* -    Beta Version 1.0.0, 24-SEP-1996 (NJB) */

/* -& */
/* $ Index_Entries */

/*     Earth precession matrix based on 1976 IAU model */

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

    t = *et / (jyear_() * 100.);
    scale = rpd_() / 3600.;
    zeta = t * (t * (t * .017998 + .30188) + 2306.2181) * scale;
    z__ = t * (t * (t * .018203 + 1.09468) + 2306.2181) * scale;
    theta = t * (t * (t * -.041833 - .42665) + 2004.3109) * scale;

/*     Now compute the precession matrix. */

    d__1 = -z__;
    d__2 = -zeta;
    eul2m_(&d__1, &theta, &d__2, &c__3, &c__2, &c__3, precm);
    return 0;
} /* zzeprcss_ */

