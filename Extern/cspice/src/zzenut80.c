/* zzenut80.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;
static integer c__3 = 3;

/* $Procedure ZZENUT80 ( Earth nutation transformation, IAU 1980 model ) */
/* Subroutine */ int zzenut80_(doublereal *et, doublereal *nutxf)
{
    doublereal dmob;
    extern /* Subroutine */ int zzmobliq_(doublereal *, doublereal *, 
	    doublereal *), chkin_(char *, ftnlen);
    doublereal dvnut[4];
    extern /* Subroutine */ int eul2xf_(doublereal *, integer *, integer *, 
	    integer *, doublereal *);
    doublereal eulang[6];
    extern /* Subroutine */ int chkout_(char *, ftnlen);
    extern logical return_(void);
    extern /* Subroutine */ int zzwahr_(doublereal *, doublereal *);
    doublereal mob;

/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines.  Users should not call this routine directly due */
/*     to the volatile nature of this routine. */

/*     Compute the state transformation matrix implementing the IAU 1980 */
/*     nutation model. */

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

/*     FRAMES */
/*     MATRIX */
/*     PRIVATE */
/*     TRANSFORMATION */
/*     UTILITY */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  ------------------------------------------------- */
/*     ET         I   Ephemeris time, seconds past J2000. */
/*     NUTXF      O   Nutation transformation matrix. */

/* $ Detailed_Input */

/*     ET             is an epoch, expressed as seconds past J2000 TDB. */

/* $ Detailed_Output */

/*     NUTXF          is a state transformation matrix that maps states */
/*                    from the earth mean equator and equinox of date */
/*                    frame (based on the 1976 IAU precession model) to */
/*                    the earth true equator and equinox frame of date */
/*                    (based on the 1980 IAU nutation model). */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     See the private SPICELIB routine ZZWAHR for a discussion */
/*     of the implementation of the 1980 IAU nutation model. */

/*     See the private SPICELIB routine ZZMOBLIQ for a discussion */
/*     of the implementation of the 1980 IAU earth mean obliquity */
/*     of date model. */

/* $ Examples */

/*     See ZZDYNFRM. */

/* $ Restrictions */

/*     1) This is a SPICE private routine; the routine is subject */
/*        to change without notice.  User applications should not */
/*        call this routine. */

/* $ Author_and_Institution */

/*     N.J. Bachman    (JPL) */

/* $ Literature_References */

/*     [1] "Explanatory Supplement to the Astronomical Almanac" */
/*          edited by P. Kenneth Seidelmann. University Science */
/*          Books, 20 Edgehill Road, Mill Valley, CA 94941 (1992) */

/*     [2] "Section 5, Geocentric Space-Fixed Position, Velocity, and */
/*         Acceleration Vectors of Tracking Station" by T. D. Moyer. */
/*         Draft of JPL Publication documenting the JPL navigation */
/*         program "Regres." */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 18-DEC-2004 (NJB) */

/* -& */

/*     SPICELIB functions */


/*     Local variables */

    if (return_()) {
	return 0;
    }
    chkin_("ZZENUT80", (ftnlen)8);

/*      Get nutation angles and their rates.  We're expecting */

/*         DVNUT(1) = Psi------nutation in longitude (radians) */
/*         DVNUT(2) = Epsilon--nutation in obliquity (radians) */
/*         DVNUT(3) = dPsi/dt     (radians/second) */
/*         DVNUT(4) = dEpsilon/dt (radians/second) */

    zzwahr_(et, dvnut);

/*     Get the mean obliquity of date. */

/*     We're expecting the outputs to be as follows: */

/*         MOB      is the mean obliquity of the ecliptic at epoch */
/*                  ET. The mean obliquity of the ecliptic is the */
/*                  inclination of the ecliptic of date to the */
/*                  mean Earth equator of date.  Output units are */
/*                  radians. */

/*         DMOB     is the time derivative of MOB at ET, expressed */
/*                        in radians per second. */
    zzmobliq_(et, &mob, &dmob);

/*     The nutation rotation N is defined by */


/*         N = [ -MOB - NUOBL ]  [ -NULON ]   [ MOB ] */
/*                             1           3         1 */

/*     where MOBLIQ is the mean obliquity of the earth's ecliptic */
/*     at epoch, NUOB is nutation in obliquity at epoch, and */
/*     NULONG is nutation in longitude at epoch.  Using our */
/*     variable names, the Euler angle sequence is */

/*        [ -MOB - DVNUT(2) ]  [ -DVNUT(1) ]  [ MOB ] */
/*                           1              3        1 */

/*     The rates corresponding to these angles are: */

/*        -DMOB - DVNUT(4),  -DVNUT(3),  DMOB */

/*     We can use EUL2XF to form the state transformation from */
/*     the nutation base frame to the nutation frame. */

    eulang[0] = -mob - dvnut[1];
    eulang[1] = -dvnut[0];
    eulang[2] = mob;
    eulang[3] = -dmob - dvnut[3];
    eulang[4] = -dvnut[2];
    eulang[5] = dmob;
    eul2xf_(eulang, &c__1, &c__3, &c__1, nutxf);
    chkout_("ZZENUT80", (ftnlen)8);
    return 0;
} /* zzenut80_ */

