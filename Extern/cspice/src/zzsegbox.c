/* zzsegbox.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure ZZSEGBOX (Bounding box for DSK segment volume element) */
/* Subroutine */ int zzsegbox_(doublereal *dskdsc, doublereal *boxctr, 
	doublereal *maxr)
{
    /* Builtin functions */
    integer i_dnnt(doublereal *);

    /* Local variables */
    extern /* Subroutine */ int zzrecbox_(doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *), 
	    zzlatbox_(doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *), zzpdtbox_(doublereal *, doublereal *,
	     doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *), chkin_(char *, ftnlen);
    doublereal l1, l2, l3;
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), setmsg_(char *, ftnlen), errint_(char *, integer *, 
	    ftnlen);
    extern logical return_(void);
    integer corsys;

/* $ Abstract */

/*     Create a bounding box for a DSK segment volume element. */
/*     The outputs are the box's center and radius. The center */
/*     is relative to the segment's frame center. */

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
/*     MATH */

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
/*     DSKDSC     I   DSK segment descriptor. */
/*     CENTER     O   Center of bounding box. */
/*     RADIUS     O   Radius of box. */

/* $ Detailed_Input */

/*     DSKDSC     is the DSK descriptor for the segment of interest. */

/* $ Detailed_Output */

/*     CENTER     is a double precision 3-vector representing the center */
/*                of a box tangent to and containing the volume */
/*                specified by the coordinate system, coordinate */
/*                parameters, if any, and bounds specified in the DSK */
/*                segment descriptor. */

/*                The box bounds the surface specified by the input */
/*                descriptor; the offset between the center of the */
/*                segment's reference frame and the segment's central */
/*                body plays no role. */

/*     RADIUS     is the radius of the sphere that circumscribes the */
/*                box. RADIUS is equal to the length of a line segment */
/*                connecting the center of the box to any corner. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If coordinate system specified in the descriptor is not */
/*         recognized, the error SPICE(NOTSUPPORTED) is signaled. */

/*     2)  Any errors that occur while deriving the bounding box */
/*         will be signaled by routines in the call tree of this */
/*         routine. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine consolidates the logic branch needed to compute a */
/*     bounding box for a DSK segment. Using this routine can eliminate */
/*     the need to test higher-level routines using each type of */
/*     coordinate system. */

/*     The box bounds the surface specified by the input descriptor; the */
/*     offset between the center of the segment's reference frame and */
/*     the segment's central body plays no role. */

/*     To compute a body's DSK bounding sphere, centered at the body, */
/*     and taking into account all loaded segments, use ZZDSKSPH. */

/* $ Examples */

/*     None. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman   (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 17-JUN-2016 (NJB) */

/* -& */
/* $ Index_Entries */

/*     bounding box for dsk segment volume element */

/* -& */

/*     SPICELIB functions */


/*     Local variables */

    if (return_()) {
	return 0;
    }
    chkin_("ZZSEGBOX", (ftnlen)8);
    corsys = i_dnnt(&dskdsc[5]);
    if (corsys == 1) {
	zzlatbox_(&dskdsc[16], boxctr, &l1, &l2, &l3, maxr);
    } else if (corsys == 3) {
	zzrecbox_(&dskdsc[16], boxctr, &l1, &l2, &l3, maxr);
    } else if (corsys == 4) {
	zzpdtbox_(&dskdsc[16], &dskdsc[6], boxctr, &l1, &l2, &l3, maxr);
    } else {
	setmsg_("Coordinate system # is not supported.", (ftnlen)37);
	errint_("#", &corsys, (ftnlen)1);
	sigerr_("SPICE(NOTSUPPORTED)", (ftnlen)19);
	chkout_("ZZSEGBOX", (ftnlen)8);
	return 0;
    }
    chkout_("ZZSEGBOX", (ftnlen)8);
    return 0;
} /* zzsegbox_ */

