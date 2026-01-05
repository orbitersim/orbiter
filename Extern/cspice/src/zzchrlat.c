/* zzchrlat.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure ZZCHRLAT ( Chord latitude  ) */
/* Subroutine */ int zzchrlat_(doublereal *midlat, doublereal *dlon, 
	doublereal *eptlat)
{
    /* System generated locals */
    doublereal d__1, d__2;

    /* Builtin functions */
    double sin(doublereal), cos(doublereal), atan2(doublereal, doublereal);

    /* Local variables */
    doublereal mlat;
    extern /* Subroutine */ int chkin_(char *, ftnlen), errdp_(char *, 
	    doublereal *, ftnlen);
    extern doublereal pi_(void), halfpi_(void), brcktd_(doublereal *, 
	    doublereal *, doublereal *);
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), setmsg_(char *, ftnlen);
    extern logical return_(void);

/* $ Abstract */

/*     Given the latitude of a midpoint of chord on a circle of constant */
/*     latitude, and given the longitude extent of the chord, compute */
/*     the latitude of the chord's endpoints. The coordinate system is */
/*     "latitudinal," aka planetocentric. */

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
/*     LATITUDE */

/* $ Declarations */

/*     File: dsktol.inc */


/*     This file contains declarations of tolerance and margin values */
/*     used by the DSK subsystem. */

/*     It is recommended that the default values defined in this file be */
/*     changed only by expert SPICE users. */

/*     The values declared in this file are accessible at run time */
/*     through the routines */

/*        DSKGTL  {DSK, get tolerance value} */
/*        DSKSTL  {DSK, set tolerance value} */

/*     These are entry points of the routine DSKTOL. */

/*        Version 1.0.0 27-FEB-2016 (NJB) */




/*     Parameter declarations */
/*     ====================== */

/*     DSK type 2 plate expansion factor */
/*     --------------------------------- */

/*     The factor XFRACT is used to slightly expand plates read from DSK */
/*     type 2 segments in order to perform ray-plate intercept */
/*     computations. */

/*     This expansion is performed to prevent rays from passing through */
/*     a target object without any intersection being detected. Such */
/*     "false miss" conditions can occur due to round-off errors. */

/*     Plate expansion is done by computing the difference vectors */
/*     between a plate's vertices and the plate's centroid, scaling */
/*     those differences by (1 + XFRACT), then producing new vertices by */
/*     adding the scaled differences to the centroid. This process */
/*     doesn't affect the stored DSK data. */

/*     Plate expansion is also performed when surface points are mapped */
/*     to plates on which they lie, as is done for illumination angle */
/*     computations. */

/*     This parameter is user-adjustable. */


/*     The keyword for setting or retrieving this factor is */


/*     Greedy segment selection factor */
/*     ------------------------------- */

/*     The factor SGREED is used to slightly expand DSK segment */
/*     boundaries in order to select segments to consider for */
/*     ray-surface intercept computations. The effect of this factor is */
/*     to make the multi-segment intercept algorithm consider all */
/*     segments that are sufficiently close to the ray of interest, even */
/*     if the ray misses those segments. */

/*     This expansion is performed to prevent rays from passing through */
/*     a target object without any intersection being detected. Such */
/*     "false miss" conditions can occur due to round-off errors. */

/*     The exact way this parameter is used is dependent on the */
/*     coordinate system of the segment to which it applies, and the DSK */
/*     software implementation. This parameter may be changed in a */
/*     future version of SPICE. */


/*     The keyword for setting or retrieving this factor is */


/*     Segment pad margin */
/*     ------------------ */

/*     The segment pad margin is a scale factor used to determine when a */
/*     point resulting from a ray-surface intercept computation, if */
/*     outside the segment's boundaries, is close enough to the segment */
/*     to be considered a valid result. */

/*     This margin is required in order to make DSK segment padding */
/*     (surface data extending slightly beyond the segment's coordinate */
/*     boundaries) usable: if a ray intersects the pad surface outside */
/*     the segment boundaries, the pad is useless if the intercept is */
/*     automatically rejected. */

/*     However, an excessively large value for this parameter is */
/*     detrimental, since a ray-surface intercept solution found "in" a */
/*     segment can supersede solutions in segments farther from the */
/*     ray's vertex. Solutions found outside of a segment thus can mask */
/*     solutions that are closer to the ray's vertex by as much as the */
/*     value of this margin, when applied to a segment's boundary */
/*     dimensions. */

/*     The keyword for setting or retrieving this factor is */


/*     Surface-point membership margin */
/*     ------------------------------- */

/*     The surface-point membership margin limits the distance */
/*     between a point and a surface to which the point is */
/*     considered to belong. The margin is a scale factor applied */
/*     to the size of the segment containing the surface. */

/*     This margin is used to map surface points to outward */
/*     normal vectors at those points. */

/*     If this margin is set to an excessively small value, */
/*     routines that make use of the surface-point mapping won't */
/*     work properly. */


/*     The keyword for setting or retrieving this factor is */


/*     Angular rounding margin */
/*     ----------------------- */

/*     This margin specifies an amount by which angular values */
/*     may deviate from their proper ranges without a SPICE error */
/*     condition being signaled. */

/*     For example, if an input latitude exceeds pi/2 radians by a */
/*     positive amount less than this margin, the value is treated as */
/*     though it were pi/2 radians. */

/*     Units are radians. */


/*     This parameter is not user-adjustable. */

/*     The keyword for retrieving this parameter is */


/*     Longitude alias margin */
/*     ---------------------- */

/*     This margin specifies an amount by which a longitude */
/*     value can be outside a given longitude range without */
/*     being considered eligible for transformation by */
/*     addition or subtraction of 2*pi radians. */

/*     A longitude value, when compared to the endpoints of */
/*     a longitude interval, will be considered to be equal */
/*     to an endpoint if the value is outside the interval */
/*     differs from that endpoint by a magnitude less than */
/*     the alias margin. */


/*     Units are radians. */


/*     This parameter is not user-adjustable. */

/*     The keyword for retrieving this parameter is */


/*     End of include file dsktol.inc */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     MIDLAT     I   is the latitude of the midpoint of a chord of a */
/*                    latitude circle. */
/*     DLON       I   is the longitude extent of the chord. */
/*     EPTLAT     O   is the latitude of the endpoints of the chord. */

/* $ Detailed_Input */

/*     MIDLAT     is the latitude of the midpoint of a chord of a circle */
/*                of constant latitude. Units are radians. The range of */
/*                MIDLAT is -pi : pi. */

/*     DLON       is the extent in longitude of the chord. DLON is the */
/*                difference of the longitudes of the endpoints of the */
/*                chords, expressed as a non-negative value. Units are */
/*                radians. DLON must be strictly less than pi. */

/* $ Detailed_Output */

/*     EPTLAT     is the latitude of the circle to which the chord */
/*                is tangent. Units are radians. */

/* $ Parameters */

/*     ANGMRG is a tolerance used to determine whether the input latitude */
/*     is within range. See 'dsktol.inc' for further information. */

/* $ Exceptions */

/*     1)  If the input longitude extent DLON is negative, or if the */
/*         extent is greater than or equal to pi radians, the error */
/*         SPICE(VALUEOUTOFRANGE) is signaled. */

/*     2)  If the input latitude is outside the range */

/*             -pi/2 - ANGMRG  :  pi/2 + ANGMRG */

/*         the error SPICE(VALUEOUTOFRANGE) is signaled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     In the remarks below, the coordinate system is presumed to be */
/*     latitudinal. */

/*     A "chord" of a circle is a line segment having endpoints on the */
/*     circle. */

/*     This routine supports partitioning of a tessellated spheroid */
/*     surface into bands of plates covering specified latitude ranges. */
/*     Note that, for a plate edge that is a chord of a positive */
/*     latitude circle, the maximum latitude on the chord is attained at */
/*     the chord's midpoint. So a set of plates lying on or above the */
/*     plate containing the latitude circle cannot actually cover the */
/*     latitude range above the circle: rays emanating from the origin */
/*     can pass above the latitude circle and miss the set of plates. An */
/*     analogous situation exists for circles of negative latitude. */

/*     In order to generate a set of plates that cover a positive */
/*     latitude band, the plates can be arranged so that the maximum */
/*     latitude of the lowest plate edges is less than or equal to the */
/*     lower latitude bound of the band. An analogous solution can be */
/*     derived for bands of negative latitude. This routine can be used */
/*     to solve these problems: it can compute the latitude of a circle */
/*     ---where the circle is horizontal and centered on the z axis--- */
/*     in which a regular polygon must be inscribed so that the maximum */
/*     latitude on the polygon is less than a specified value, or such */
/*     that the minimum latitude on the polygon is greater than a */
/*     specified value. */

/* $ Examples */

/*     The numerical results shown for this example may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */


/*     1)  Find the latitude circle on which a line segment's endpoints */
/*         must lie, if the longitude extent of the segment is 60 */
/*         degrees, and if the latitude of the segment's midpoint is 30 */
/*         degrees. */


/*     Example code begins here. */


/*           PROGRAM EX1 */
/*           IMPLICIT NONE */

/*     C */
/*     C     SPICELIB functions */
/*     C */
/*           DOUBLE PRECISION      DPR */
/*           DOUBLE PRECISION      RPD */

/*     C */
/*     C     Local variables */
/*     C */
/*           DOUBLE PRECISION      DLON */
/*           DOUBLE PRECISION      EP     ( 3, 2 ) */
/*           DOUBLE PRECISION      EPTLAT */
/*           DOUBLE PRECISION      MIDLAT */
/*           DOUBLE PRECISION      MIDLON */
/*           DOUBLE PRECISION      MIDRAD */
/*           DOUBLE PRECISION      MIDPT  ( 3 ) */
/*           DOUBLE PRECISION      OUTLAT */

/*     C */
/*     C     Let the segment have a longitude extent */
/*     C     of 60 degrees. */
/*     C */
/*           DLON   = 60.D0 * RPD() */

/*     C */
/*     C     Let the midpoint of the segment have */
/*     C     a latitude of 30 degrees. */
/*     C */
/*           MIDLAT = 30.D0 * RPD() */

/*     C */
/*     C     Find the latitude of the segment's endpoints. */
/*     C */
/*           CALL ZZCHRLAT ( MIDLAT, DLON, EPTLAT ) */

/*           WRITE (*,*) 'Endpoint latitude (deg) = ', EPTLAT*DPR() */

/*     C */
/*     C     Generate the endpoints and the latitude */
/*     C     of the segment's midpoint. */
/*     C */
/*     C     Note that the scale is arbitrary: we can set it */
/*     C     to 1.0. The longitudes of the endpoints are */
/*     C     arbitrary as well; only the difference is known. */
/*     C */
/*           CALL LATREC ( 1.D0, 0.D0, EPTLAT, EP(1,1) ) */
/*           CALL LATREC ( 1.D0, DLON, EPTLAT, EP(1,2) ) */

/*           CALL VLCOM  ( 0.5D0, EP(1,1), */
/*          .              0.5D0, EP(1,2), MIDPT ) */

/*           CALL RECLAT ( MIDPT, MIDRAD, MIDLON, OUTLAT ) */

/*           WRITE (*,*) 'Check: difference (deg) = ', */
/*          .            (MIDLAT - OUTLAT) * DPR() */

/*           END */


/*     When this program was executed on a PC/Linux/gfortran 64-bit */
/*     platform, the output was: */


/*        Endpoint latitude (deg) =    26.565051177077986 */
/*        Check: difference (deg) =   6.36110936292703354E-015 */


/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman    (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 20-APR-2016 (NJB) (EDW) */

/* -& */
/* $ Index_Entries */

/*     find latitude of chord's endpoints on latitude circle */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     Use discovery check-in. */

    if (return_()) {
	return 0;
    }
    if (*dlon < 0. || *dlon >= pi_()) {
	chkin_("ZZCHRLAT", (ftnlen)8);
	setmsg_("The input longitude extent was #; this value must be in the"
		" range [0 : pi ) radians.", (ftnlen)84);
	errdp_("#", dlon, (ftnlen)1);
	sigerr_("SPICE(VALUEOUTOFRANGE)", (ftnlen)22);
	chkout_("ZZCHRLAT", (ftnlen)8);
	return 0;
    }
    if (abs(*midlat) > halfpi_() + 1e-12) {
	chkin_("ZZCHRLAT", (ftnlen)8);
	setmsg_("The input latitude was #; this value must be in the interva"
		"l -pi/2 : pi/2 (radians).", (ftnlen)84);
	errdp_("#", midlat, (ftnlen)1);
	sigerr_("SPICE(VALUEOUTOFRANGE)", (ftnlen)22);
	chkout_("ZZCHRLAT", (ftnlen)8);
	return 0;
    }

/*     The input latitude is, at worst, slightly out of range. */
/*     Bracket it. */

    d__1 = -halfpi_();
    d__2 = halfpi_();
    mlat = brcktd_(midlat, &d__1, &d__2);

/*     The endpoint latitude EPTLAT is defined by */

/*        EPTLAT = atan ( tan(MLAT) * cos( DLON/2 ) ) */

/*     For numerical robustness, we'll re-write this using */
/*     the two-argument arctangent function and well-behaved */
/*     trig functions as input arguments: */

    *eptlat = atan2(sin(mlat) * cos(*dlon / 2), cos(mlat));
    return 0;
} /* zzchrlat_ */

