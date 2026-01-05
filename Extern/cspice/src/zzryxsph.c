/* zzryxsph.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static doublereal c_b2 = 1.;

/* $Procedure ZZRYXSPH ( Intersection of ray and sphere ) */
/* Subroutine */ int zzryxsph_(doublereal *vertex, doublereal *udir, 
	doublereal *r__, doublereal *xpt, logical *found)
{
    /* System generated locals */
    doublereal d__1, d__2;

    /* Builtin functions */
    double sqrt(doublereal);

    /* Local variables */
    doublereal cpar, perp[3];
    extern doublereal vdot_(doublereal *, doublereal *);
    doublereal pmag2, vmag2, s;
    extern /* Subroutine */ int vlcom_(doublereal *, doublereal *, doublereal 
	    *, doublereal *, doublereal *);
    doublereal r2;

/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines. Users should not call this routine directly due to the */
/*     volatile nature of this routine. */

/*     Quickly find the intersection of a ray and a sphere, without */
/*     performing normal error handling. */

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

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     VERTEX, */
/*     UDIR       I   Vertex and unit length direction vector of ray. */
/*     R          I   Radius of sphere. */
/*     XPT        O   Intersection point, if NXPTS = 1. */
/*     FOUND      O   Flag indicating whether intersection exists. */

/* $ Detailed_Input */

/*     VERTEX, */
/*     UDIR           are a point and unit-length direction vector that */
/*                    define a ray in three-dimensional space. The ray */
/*                    is the set of points in 3-dimensional space */

/*                       { X : X = VERTEX + s*UDIR, s >= 0 } */


/*     R              is the radius of a sphere. The sphere is centered */
/*                    at the origin. */

/* $ Detailed_Output */

/*     XPT            is the point of intersection nearest to the ray's */
/*                    vertex of the input ray and sphere, if the */
/*                    intersection exists. Otherwise, XPT is undefined. */

/*     FOUND          is a local flag that is .TRUE. if and only if */
/*                    the input ray intersects the sphere. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  The ray's direction vector must have unit length. The */
/*         outputs of this routine will be invalid otherwise. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine performs a simplified version of the ray-ellipsoid */
/*     intersection computation performed by SURFPT; this routine works */
/*     with spheres only. This routine dispenses with error handling and */
/*     nuanced handling of near- singular geometry in return for speed. */

/*     On a PC/Linux/gfortran/64bit platform on which this routine */
/*     was tested, it ran about 5 times faster than the public */
/*     SPICELIB routine SURFPT. */

/*     This routine is meant to be used only by the DSK subsystem. */

/* $ Examples */

/*     See usage in ZZRYXLAT. */

/* $ Restrictions */

/*     1) All inputs must be checked by the caller; they are not checked */
/*        here. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman   (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 11-JAN-2016 (NJB) */

/* -& */
/* $ Index_Entries */

/*     intersection of ray and sphere */

/* -& */

/*     SPICELIB functions */


/*     Local variables */

    *found = FALSE_;

/*     Find the component of VERTEX orthogonal to UDIR. If the magnitude */
/*     of this component exceeds R, there's no intercept. */

    cpar = vdot_(vertex, udir);
    d__1 = -cpar;
    vlcom_(&c_b2, vertex, &d__1, udir, perp);
    pmag2 = vdot_(perp, perp);
    r2 = *r__ * *r__;

/*     Compare squares of magnitudes, rather than magnitudes, for */
/*     efficiency. */

    if (pmag2 > r2) {
	return 0;
    }
/* Computing MAX */
    d__1 = 0., d__2 = r2 - pmag2;
    s = sqrt((max(d__1,d__2)));
    vmag2 = vdot_(vertex, vertex);
    if (vmag2 > r2) {

/*        If the magnitude of the vertex exceeds R, the vertex is */
/*        outside the sphere. Above, we have compared squares of */
/*        magnitudes for efficiency. */

	if (cpar > 0.) {

/*           The ray points away from the sphere; there can be no */
/*           intersection. */

	    return 0;
	}

/*        Given that an intercept exists, we can find it between VERTEX */
/*        and VPERP by following -UDIR from PERP towards VERTEX. */

	xpt[0] = perp[0] - s * udir[0];
	xpt[1] = perp[1] - s * udir[1];
	xpt[2] = perp[2] - s * udir[2];
    } else if (vmag2 < r2) {

/*        The vertex is inside the sphere. We can calculate the exit */
/*        point by using PERP as a vertex. */

	xpt[0] = perp[0] + s * udir[0];
	xpt[1] = perp[1] + s * udir[1];
	xpt[2] = perp[2] + s * udir[2];
    } else {

/*        PERP is the sole intercept. */

	xpt[0] = perp[0];
	xpt[1] = perp[1];
	xpt[2] = perp[2];
    }
    *found = TRUE_;
    return 0;
} /* zzryxsph_ */

