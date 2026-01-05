/* zzmobliq.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure   ZZMOBLIQ   ( Mean obliquity of date ) */
/* Subroutine */ int zzmobliq_(doublereal *et, doublereal *mob, doublereal *
	dmob)
{
    /* Initialized data */

    static logical first = TRUE_;

    static doublereal year, t;
    extern doublereal jyear_(void);
    static doublereal persec, rad;
    extern doublereal rpd_(void);

/* $ Abstract */

/*     Return the mean obliquity of the ecliptic, and its time */
/*     derivative, at a specified epoch. */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines.  Users should not call this routine directly due */
/*     to the volatile nature of this routine. */

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

/*     GEOMETRY */

/* $ Declarations */
/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     ET         I   Ephemeris time, in seconds past J2000. */
/*     MOB        O   Mean obliquity of the ecliptic at ET. */
/*     DMOB       O   Time derivative of the mean obliquity. */

/* $ Detailed_Input */

/*     ET             is the epoch at which the obliquity of the ecliptic */
/*                    is to be computed.  ET is barycentric dynamical */
/*                    time, expressed as seconds past J2000. */

/* $ Detailed_Output */

/*     MOB            is the mean obliquity of the ecliptic at epoch ET. */
/*                    The mean obliquity of the ecliptic is the */
/*                    inclination of the ecliptic of date to the mean */
/*                    Earth equator of date.  Output units are radians. */

/*     DMOB           is the time derivative of MOB at ET, expressed in */
/*                    radians per second. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     The expression for mean is obliquity is */

/*                          ''        ''            ''         2 */
/*        MOBLIQ   =   84381 .448 - 46 .8150 * T - 0 .00059 * T */

/*                      ''          3 */
/*                   + 0 .001813 * T */

/*     where T indicates Julian centuries past J2000.  This is from */
/*     equation 5-153 of reference [2]. */

/* $ Examples */

/*     See the routine ENUTAT for an example of usage. */

/* $ Restrictions */

/*     1)  This is a preliminary version of the routine. */

/* $ Literature_References */

/*     [1] "Explanatory Supplement to the Astronomical Almanac" */
/*          edited by P. Kenneth Seidelmann. University Science */
/*          Books, 20 Edgehill Road, Mill Valley, CA 94941 (1992) */

/*     [2] "Section 5, Geocentric Space-Fixed Position, Velocity, and */
/*         Acceleration Vectors of Tracking Station" by T. D. Moyer. */
/*         Draft of JPL Publication documenting the JPL navigation */
/*         program "Regres." */


/* $ Author_and_Institution */

/*     W.L. Taber         (JPL) */
/*     N.J. Bachman       (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.1 02-OCT-2021 (NJB) */

/*        Corrected typo in comments. */

/* -    SPICELIB Version 1.0.0 18-JUL-1997 (WLT) */

/*        Adapted Nat's routine to private version making output */
/*        rate be radians/sec. */

/* -    Beta Version 1.0.0, 29-SEP-1996 (NJB) */

/* -& */
/* $ Index_Entries */

/*     compute mean obliquity of date of the ecliptic */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     Coefficients for the mean obliquity: */


/*     Local variables */

    if (first) {
	first = FALSE_;
	year = jyear_();
	rad = rpd_();
	persec = 1. / (year * 100.);
    }

/*     Convert the input epoch to Julian centuries past J2000: */

    t = *et / year / 100.;

/*     Compute the obliquity at epoch.  The polynomial yields arcseconds; */
/*     convert the units to radians. */

    *mob = rad / 3600. * (t * (t * (t * .001813 - 5.9e-4) - 46.815) + 
	    84381.448);
    *dmob = rad / 3600. * (t * (t * 3 * .001813 - .0011800000000000001) - 
	    46.815) * persec;
    return 0;
} /* zzmobliq_ */

