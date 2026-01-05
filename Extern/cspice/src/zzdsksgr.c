/* zzdsksgr.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__6 = 6;

/* $Procedure ZZDSKSGR ( DSK, return segment bounding radius ) */
doublereal zzdsksgr_(doublereal *dskdsc)
{
    /* System generated locals */
    integer i__1;
    doublereal ret_val, d__1, d__2, d__3, d__4;

    /* Builtin functions */
    integer i_dnnt(doublereal *), s_rnge(char *, integer, char *, integer);

    /* Local variables */
    doublereal minr;
    integer b;
    doublereal f;
    integer i__, j;
    extern /* Subroutine */ int chkin_(char *, ftnlen), moved_(doublereal *, 
	    integer *, doublereal *), errdp_(char *, doublereal *, ftnlen);
    extern doublereal vnorm_(doublereal *);
    doublereal re, rp;
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), setmsg_(char *, ftnlen), errint_(char *, integer *, 
	    ftnlen);
    integer corsys;
    doublereal mag[3], bds[6]	/* was [2][3] */;

/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines. Users should not call this routine directly due to the */
/*     volatile nature of this routine. */

/*     Return a radius of a bounding sphere for the spatial region */
/*     described by a DSK segment descriptor. The radius is measured */
/*     from the origin of the coordinate system associated from the */
/*     segment. */

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

/*     DAS */
/*     DSK */

/* $ Keywords */

/*     DSK */
/*     GEOMETRY */
/*     UTILITY */

/* $ Declarations */

/*     Include file dskdsc.inc */

/*     This include file declares parameters for DSK segment descriptors. */

/* -       SPICELIB Version 1.0.0 08-FEB-2017 (NJB) */

/*           Updated version info. */

/*           22-JAN-2016 (NJB) */

/*              Added parameter for data class 2. Changed name of data */
/*              class 1 parameter. Corrected data class descriptions. */

/*           13-MAY-2010 (NJB) */

/*              Descriptor now contains two ID codes, one for the */
/*              surface, one for the associated ephemeris object. This */
/*              supports association of multiple surfaces with one */
/*              ephemeris object without creating file management */
/*              issues. */

/*              Room was added for coordinate system definition */
/*              parameters. */

/*               Flag arrays and model ID/component entries were deleted. */

/*            11-SEP-2008 (NJB) */


/*     DSK segment descriptors are implemented as an array of d.p. */
/*     numbers.  Note that each integer descriptor datum occupies one */
/*     d.p. value. */




/*     Segment descriptor parameters */

/*     Each segment descriptor occupies a contiguous */
/*     range of DAS d.p. addresses. */

/*        The DSK segment descriptor layout is: */

/*           +---------------------+ */
/*           | Surface ID code     | */
/*           +---------------------+ */
/*           | Center ID code      | */
/*           +---------------------+ */
/*           | Data class code     | */
/*           +---------------------+ */
/*           | Data type           | */
/*           +---------------------+ */
/*           | Ref frame code      | */
/*           +---------------------+ */
/*           | Coord sys code      | */
/*           +---------------------+ */
/*           | Coord sys parameters|  {10 elements} */
/*           +---------------------+ */
/*           | Min coord 1         | */
/*           +---------------------+ */
/*           | Max coord 1         | */
/*           +---------------------+ */
/*           | Min coord 2         | */
/*           +---------------------+ */
/*           | Max coord 2         | */
/*           +---------------------+ */
/*           | Min coord 3         | */
/*           +---------------------+ */
/*           | Max coord 3         | */
/*           +---------------------+ */
/*           | Start time          | */
/*           +---------------------+ */
/*           | Stop time           | */
/*           +---------------------+ */

/*     Parameters defining offsets for segment descriptor elements */
/*     follow. */


/*     Surface ID code: */


/*     Central ephemeris object NAIF ID: */


/*     Data class: */

/*     The "data class" is a code indicating the category of */
/*     data contained in the segment. */


/*     Data type: */


/*     Frame ID: */


/*     Coordinate system code: */


/*     Coordinate system parameter start index: */


/*     Number of coordinate system parameters: */


/*     Ranges for coordinate bounds: */


/*     Coverage time bounds: */


/*     Descriptor size (24): */


/*     Data class values: */

/*        Class 1 indicates a surface that can be represented as a */
/*                single-valued function of its domain coordinates. */

/*                An example is a surface defined by a function that */
/*                maps each planetodetic longitude and latitude pair to */
/*                a unique altitude. */


/*        Class 2 indicates a general surface. Surfaces that */
/*                have multiple points for a given pair of domain */
/*                coordinates---for example, multiple radii for a given */
/*                latitude and longitude---belong to class 2. */



/*     Coordinate system values: */

/*        The coordinate system code indicates the system to which the */
/*        tangential coordinate bounds belong. */

/*        Code 1 refers to the planetocentric latitudinal system. */

/*        In this system, the first tangential coordinate is longitude */
/*        and the second tangential coordinate is latitude. The third */
/*        coordinate is radius. */



/*        Code 2 refers to the cylindrical system. */

/*        In this system, the first tangential coordinate is radius and */
/*        the second tangential coordinate is longitude. The third, */
/*        orthogonal coordinate is Z. */



/*        Code 3 refers to the rectangular system. */

/*        In this system, the first tangential coordinate is X and */
/*        the second tangential coordinate is Y. The third, */
/*        orthogonal coordinate is Z. */



/*        Code 4 refers to the planetodetic/geodetic system. */

/*        In this system, the first tangential coordinate is longitude */
/*        and the second tangential coordinate is planetodetic */
/*        latitude. The third, orthogonal coordinate is altitude. */



/*     End of include file dskdsc.inc */

/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     DSKDSC     I   is a DSK descriptor. */

/*     The function returns the maximum radius of the segment. */

/* $ Detailed_Input */

/*     DSKDSC         is a DSK descriptor. */

/* $ Detailed_Output */

/*     The function returns the maximum radius attained on the */
/*     boundary of the coverage region of the segment associated */
/*     with the input descriptor. The radius is measured from */
/*     the origin of the coordinate system in which the coverage */
/*     bounds are given. */

/*     Units are km. */

/* $ Parameters */

/*     See the INCLUDE file dskdsc.inc for parameter declarations and */
/*     documentation. */

/* $ Exceptions */

/*     1)  If the coordinate system code in the descriptor is not */
/*         recognized, the error SPICE(NOTSUPPORTED) is signaled. */

/*     2)  If an invalid radius is found, the error */
/*         SPICE(INVALIDVALUE) is signaled. */

/*     3)  If an invalid flattening coefficient is found, the error */
/*         SPICE(INVALIDVALUE) is signaled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine computes an upper bound on the radius of */
/*     the coverage region of a DSK segment. The radius at */
/*     a point on the boundary of the coverage region is */
/*     measured from the origin of the coordinate system. */

/*     This routine can be used to generate a bounding sphere for */
/*     a surface described by one or more DSK segments. Note that */
/*     the DSK segments describing the surface of an object don't */
/*     necessarily have a common central body, so offsets between */
/*     segments' central bodies must be taken into account when */
/*     computing a bounding sphere. */

/* $ Examples */


/*     1) Get a bounding radius for a DSK segment using the */
/*        latitudinal coordinate system. */

/*        Example code begins here. */


/*                 PROGRAM DSKGSR_EX1 */
/*                 IMPLICIT NONE */

/*                 INCLUDE 'dskdsc.inc' */

/*                 DOUBLE PRECISION      ZZDSKSGR */
/*                 DOUBLE PRECISION      PI */

/*                 DOUBLE PRECISION      DSKDSC( DSKDSZ ) */

/*           C */
/*           C     Latitudinal coordinates: */
/*           C */
/*                 CALL CLEARD( DSKDSZ, DSKDSC ) */

/*                 DSKDSC( SYSIDX ) = LATSYS */

/*           C */
/*           C     Fill in the coordinate bounds in the descriptor. */
/*           C */
/*           C     Longitude: */
/*           C */
/*                 DSKDSC(MN1IDX) =  - PI()/2 */
/*                 DSKDSC(MX1IDX) =    PI()/2 */
/*           C */
/*           C     Latitude: */
/*           C */
/*                 DSKDSC(MN2IDX) =    0 */
/*                 DSKDSC(MX2IDX) =    PI()/4 */
/*           C */
/*           C     Radius: */
/*           C */
/*                 DSKDSC(MN3IDX) =    1.D3 */
/*                 DSKDSC(MX3IDX) =    1.1D3 */

/*                 WRITE (*,*) 'Radius bound for latitudinal section: ', */
/*                .            ZZDSKSGR( DSKDSC ) */


/*                 END */


/*        When run on a PC/Linux/gfortran platform, the output */
/*        from this program was: */

/*           Radius bound for latitudinal section:    1100.0000000000000 */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman    (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 13-FEB-2017 (NJB) */

/*        Added case for planetodetic coordinates; deleted case for */
/*        cylindrical coordinates. Recoded algorithm for rectangular */
/*        coordinates. */

/*        22-JUL-2016 (NJB) */

/*           Renamed to ZZDSKSGR. */

/*        14-MAY-2010 (NJB) */

/* -& */
/* $ Index_Entries */

/*     Return upper bound on dsk segment coverage radius */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     Use discovery check-in. */


/*     Set an initial return value. */

    ret_val = -1.;

/*     The radius calculation depends on the coordinate system. */

    corsys = i_dnnt(&dskdsc[5]);
    if (corsys == 1) {

/*        Fetch the minimum radius from the descriptor. */

	minr = dskdsc[20];
	if (minr <= 0.) {
	    chkin_("ZZDSKSGR", (ftnlen)8);
	    setmsg_("Minimum radius was *.", (ftnlen)21);
	    errdp_("*", &minr, (ftnlen)1);
	    sigerr_("SPICE(VALUEOUTOFRANGE)", (ftnlen)22);
	    chkout_("ZZDSKSGR", (ftnlen)8);
	    return ret_val;
	}

/*        This is as simple as it gets. The radius bounds */
/*        correspond to the third coordinate in the descriptor. */

	ret_val = dskdsc[21];
    } else if (corsys == 4) {

/*        Fetch the equatorial radius from the descriptor. */

	re = dskdsc[6];
	if (re <= 0.) {
	    chkin_("ZZDSKSGR", (ftnlen)8);
	    setmsg_("Equatorial radius was *.", (ftnlen)24);
	    errdp_("*", &re, (ftnlen)1);
	    sigerr_("SPICE(VALUEOUTOFRANGE)", (ftnlen)22);
	    chkout_("ZZDSKSGR", (ftnlen)8);
	    return ret_val;
	}

/*        Fetch the flattening coefficient from the descriptor. */

	f = dskdsc[7];
	if (f >= 0. && f < 1.) {

/*           This is the oblate case. */

/*           The maximum radius of an oblate planetodetic boundary */
/*           occurs on the X-Y plane at the maximum height. */

	    ret_val = dskdsc[21] + re;
	} else if (f < 0.) {

/*           This is the prolate case. */

/*           The maximum radius of an prolate planetodetic boundary */
/*           occurs on the poles at the maximum height. */

	    re = dskdsc[6];
	    rp = re * (1. - f);
	    ret_val = dskdsc[21] + rp;
	} else {

/*           We have an invalid flattening coefficient. */

/*           If the flattening coefficient is greater than one, the */
/*           polar radius computed below is negative. If it's equal to */
/*           one, the polar radius is zero. Either case is a problem, so */
/*           signal an error and check out. */

	    chkin_("ZZDSKSGR", (ftnlen)8);
	    setmsg_("Flattening coefficient was *.", (ftnlen)29);
	    errdp_("*", &f, (ftnlen)1);
	    sigerr_("SPICE(VALUEOUTOFRANGE)", (ftnlen)22);
	    chkout_("ZZDSKSGR", (ftnlen)8);
	    return ret_val;
	}
    } else if (corsys == 3) {

/*        The bounding cell of the segment has its extreme value at */
/*        a corner. Just take the maximum of these values. */

/*        First copy the bounds into an appropriately dimensioned */
/*        array. */

	moved_(&dskdsc[16], &c__6, bds);
	b = 16;
	for (i__ = 1; i__ <= 3; ++i__) {
	    j = b + (i__ << 1) - 1;
/* Computing MAX */
	    d__3 = (d__1 = dskdsc[j - 1], abs(d__1)), d__4 = (d__2 = dskdsc[j]
		    , abs(d__2));
	    mag[(i__1 = i__ - 1) < 3 && 0 <= i__1 ? i__1 : s_rnge("mag", i__1,
		     "zzdsksgr_", (ftnlen)347)] = max(d__3,d__4);
	}
	ret_val = vnorm_(mag);
    } else {

/*        Never heard of this coordinate system. */

	chkin_("ZZDSKSGR", (ftnlen)8);
	setmsg_("The coordinate system code # is not recognized.", (ftnlen)47)
		;
	errint_("#", &corsys, (ftnlen)1);
	sigerr_("SPICE(NOTSUPPORTED)", (ftnlen)19);
	chkout_("ZZDSKSGR", (ftnlen)8);
	return ret_val;
    }
    return ret_val;
} /* zzdsksgr_ */

