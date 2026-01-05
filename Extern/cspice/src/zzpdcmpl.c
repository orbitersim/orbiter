/* zzpdcmpl.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure ZZPDCMPL (Planetodetic coordinates, compare latitudes ) */
/* Subroutine */ int zzpdcmpl_(doublereal *re, doublereal *f, doublereal *p, 
	doublereal *lat, integer *rel)
{
    /* Initialized data */

    static doublereal apex[3] = { 0.,0.,0. };

    extern /* Subroutine */ int vsub_(doublereal *, doublereal *, doublereal *
	    ), zzelnaxx_(doublereal *, doublereal *, doublereal *, doublereal 
	    *, doublereal *);
    doublereal r__;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    extern logical failed_(void);
    doublereal rp;
    extern doublereal halfpi_(void);
    doublereal offpcl;
    extern /* Subroutine */ int reclat_(doublereal *, doublereal *, 
	    doublereal *, doublereal *);
    doublereal offset[3];
    extern /* Subroutine */ int chkout_(char *, ftnlen);
    doublereal xincpt, yincpt;
    extern logical return_(void);
    doublereal lon;

/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines. Users should not call this routine directly due to the */
/*     volatile nature of this routine. */

/*     Compare the planetodetic latitude of a point in 3-dimensional */
/*     space against a specified value, without converting the point to */
/*     planetodetic coordinates. */

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

/*     COORDINATES */
/*     GEOMETRY */
/*     PLANETODETIC */
/*     LATITUDE */
/*     MATH */

/* $ Declarations */
/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     RE         I   Equatorial radius. */
/*     F          I   Flattening coefficient. */
/*     P          I   Three-dimensional point. */
/*     LAT        I   Planetodetic latitude. */
/*     REL        O   Relation code. */
/*     LT         P   Code indicating latitude of P < LAT. */
/*     EQ         P   Code indicating latitude of P = LAT. */
/*     GT         P   Code indicating latitude of P > LAT. */

/* $ Detailed_Input */

/*     RE, */
/*     F          are, respectively, the equatorial radius */
/*                and flattening coefficient of a biaxial */
/*                spheroid. */

/*                The polar radius RP of the spheroid is */

/*                   RP = RE * ( 1 - F ) */

/*                RP may be less than, equal to, or greater than RE. */


/*     P          is a point (equivalently, a vector) in */
/*                three-dimensional space. P is expressed in Cartesian */
/*                coordinates. */

/*                The units of P must be consistent with those of RE. */


/*     LAT        is a planetodetic latitude value to be compared */
/*                against the planetodetic latitude of P. Units */
/*                are radians. */

/* $ Detailed_Output */

/*     REL        is an integer code that indicates the order */
/*                relation between the planetodetic latitude of P */
/*                and LAT. The planetodetic coordinate system is */
/*                defined by the inputs RE and F. */

/*                The code <rel> indicates that the relation */

/*                   <latitude of P>  <rel>  LAT */

/*                is true. See the Parameters section below for */
/*                the parameter names. */


/* $ Parameters */

/*     LT, */
/*     EQ, */
/*     GT         are, respectively, codes indicating the relationship */
/*                of the planetodetic latitude of the input vector to */
/*                the input latitude value. Let LP represent the */
/*                planetodetic latitude of P. */

/*                   Code LT indicates   LP < LAT */
/*                   Code EQ indicates   LP = LAT */
/*                   Code GT indicates   LP > LAT */

/* $ Exceptions */

/*     1)  If either the equatorial radius or flattening coefficient */
/*         is invalid, the error will be signaled by a routine in the */
/*         call tree of this routine. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine performs a planetodetic latitude comparison more */
/*     efficiently than can be done using a rectangular-to-planetodetic */
/*     coordinate conversion. */

/* $ Examples */

/*     None. */

/* $ Restrictions */

/*     There are some cases for which this routine cannot be applied. */
/*     See the SPICELIB routine ZZPDPLTC and its usage in ZZRYTPDT. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman   (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 19-JAN-2017 (NJB) */

/*       Original version 22-AUG-2015 (NJB) */

/* -& */
/* $ Index_Entries */

/*     compare planetodetic latitude of vector against value */
/*     compare planetodetic latitude of point against value */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     Saved variables */


/*     Initial values */

    if (return_()) {
	return 0;
    }
    chkin_("ZZPDCMPL", (ftnlen)8);

/*     Treat points on the Z axis as a special case. The */
/*     computations performed in the general case may introduce */
/*     round-off errors that will lead to false results for */
/*     this case. */

    if (p[0] == 0. && p[1] == 0.) {
	if (p[2] > 0.) {
	    if (*lat == halfpi_()) {
		*rel = 0;
	    } else {
		*rel = 1;
	    }
	} else if (p[2] == 0.) {

/*           We consider the latitude of P to be zero. */

	    if (*lat > 0.) {
		*rel = -1;
	    } else if (*lat == 0.) {
		*rel = 0;
	    } else {
		*rel = 1;
	    }
	} else {

/*           P(3) < 0. */

	    if (*lat == -halfpi_()) {
		*rel = 0;
	    } else {
		*rel = -1;
	    }
	}
	chkout_("ZZPDCMPL", (ftnlen)8);
	return 0;
    }

/*     Latitude zero is a special case. The planetodetic latitude of the */
/*     input point has the same sign as the Z component of the point. */

    rp = *re * (1. - *f);

/*     Get the y-intercept of the latitude cone for LAT. Note that a */
/*     result is defined for LAT = +/- pi/2. */

    zzelnaxx_(re, &rp, lat, &xincpt, &yincpt);
    if (failed_()) {
	chkout_("ZZPDCMPL", (ftnlen)8);
	return 0;
    }

/*     Ideally YINCPT is zero if and only if LAT is zero. */
/*     We'll group these conditions together. */

    if (*lat == 0. || yincpt == 0.) {
	if (p[2] > 0.) {
	    *rel = 1;
	} else if (p[2] == 0.) {
	    *rel = 0;
	} else {
	    *rel = -1;
	}
	chkout_("ZZPDCMPL", (ftnlen)8);
	return 0;
    }

/*     This is the normal case. */

/*     Find the offset of the point from the latitude cone's apex. */
/*     Create a unit-length copy of the offset vector. */

    apex[2] = yincpt;
    vsub_(p, apex, offset);
/*     We'll use the planetocentric [sic] latitude of the offset */
/*     vector for comparison. */

    reclat_(offset, &r__, &lon, &offpcl);
    if (*lat > 0.) {
	if (yincpt > 0.) {

/*           This is the prolate case. */

	    if (offpcl > *lat) {
		*rel = 1;
	    } else if (offpcl == *lat) {
		*rel = 0;
	    } else {
		*rel = -1;
	    }
	} else {

/*           YINCPT = 0 was handled previously, so YINCPT < 0. */

/*           This is the oblate case. */

/*           In addition to the comparison of angles, we need to know */
/*           the input point is above the X-Y plane in order for the */
/*           GT or EQ relations to hold. */

	    if (p[2] > 0.) {
		if (offpcl > *lat) {
		    *rel = 1;
		} else if (offpcl == *lat) {
		    *rel = 0;
		} else {
		    *rel = -1;
		}
	    } else {

/*              The input latitude is positive, while the point */
/*              is on or below the X-Y plane. */

		*rel = -1;
	    }
	}
    } else {

/*        LAT < 0, since the case LAT = 0 has already been handled. */

	if (yincpt < 0.) {

/*           This is the prolate case. */

	    if (offpcl > *lat) {
		*rel = 1;
	    } else if (offpcl == *lat) {
		*rel = 0;
	    } else {
		*rel = -1;
	    }
	} else {

/*           YINCPT > 0, since the case YINCPT = 0 was handled */
/*           previously. */

/*           This is the oblate case. */

	    if (p[2] < 0.) {
		if (offpcl > *lat) {
		    *rel = 1;
		} else if (offpcl == *lat) {
		    *rel = 0;
		} else {
		    *rel = -1;
		}
	    } else {

/*              The input latitude is negative, while the point */
/*              is on or above the X-Y plane. */

		*rel = 1;
	    }
	}
    }
    chkout_("ZZPDCMPL", (ftnlen)8);
    return 0;
} /* zzpdcmpl_ */

