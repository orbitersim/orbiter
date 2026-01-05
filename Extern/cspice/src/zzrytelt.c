/* zzrytelt.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure ZZRYTELT ( DSK, ray touches coordinate volume element ) */
/* Subroutine */ int zzrytelt_(doublereal *vertex, doublereal *raydir, 
	doublereal *dskdsc, doublereal *margin, integer *nxpts, doublereal *
	xpt)
{
    /* Builtin functions */
    integer i_dnnt(doublereal *);

    /* Local variables */
    extern /* Subroutine */ int zzrytrec_(doublereal *, doublereal *, 
	    doublereal *, doublereal *, integer *, doublereal *), zzrytlat_(
	    doublereal *, doublereal *, doublereal *, doublereal *, integer *,
	     doublereal *), zzrytpdt_(doublereal *, doublereal *, doublereal *
	    , doublereal *, doublereal *, integer *, doublereal *), chkin_(
	    char *, ftnlen), sigerr_(char *, ftnlen), chkout_(char *, ftnlen),
	     setmsg_(char *, ftnlen), errint_(char *, integer *, ftnlen);
    extern logical return_(void);
    integer corsys;

/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines. Users should not call this routine directly due to the */
/*     volatile nature of this routine. */

/*     Find nearest intersection to ray's vertex of ray and */
/*     a coordinate volume element. If the vertex is inside */
/*     the element, the vertex is considered to be the solution. */

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

/*     DSK */

/* $ Keywords */

/*     DSK */
/*     GEOMETRY */
/*     INTERCEPT */
/*     INTERSECTION */
/*     RAY */
/*     SURFACE */
/*     TOPOGRAPHY */

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

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     VERTEX     I   Ray's vertex. */
/*     RAYDIR     I   Ray's direction vector. */
/*     DSKDSC     I   DSK descriptor of segment. */
/*     MARGIN     I   Margin used for element expansion. */
/*     NXPTS      O   Number of intercept points. */
/*     XPT        O   Intercept. */

/* $ Detailed_Input */

/*     VERTEX, */
/*     RAYDIR     are, respectively, the vertex and direction vector of */
/*                the ray to be used in the intercept computation. */

/*                Both the vertex and ray direction must be represented */
/*                in the reference frame of the segment to which the */
/*                volume element boundaries correspond. The vertex is */
/*                considered to be an offset from the center of the */
/*                reference frame associated with the segment. */


/*     DSKDSC     is a DSK segment descriptor. The coordinate system */
/*                and spatial boundaries of a the segment are specified */
/*                by members of this descriptor. */


/* $ Detailed_Output */

/*     XPT        is the intercept of the ray on the boundary of the */
/*                input volume element, if such an intercept exists. If */
/*                the ray's vertex is inside the element, XPT is set */
/*                equal to the vertex. XPT is valid if and only if FOUND */
/*                is .TRUE. */

/*                XPT is expressed in the reference frame associated */
/*                with the inputs VERTEX and RAYDIR. XPT represents */
/*                an offset from the origin of the coordinate system. */

/*                XPT is valid only if NXPTS is set to 1. */


/*     NXPTS      is the number of intercept points of the ray and */
/*                the volume element. */

/*                Currently there are only two possible values for */
/*                NXPTS: */

/*                   1 for an intersection */
/*                   0 for no intersection */

/*                If the vertex is inside the element, NXPTS is */
/*                set to 1. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1) If the coordinate system code in the segment descriptor */
/*        is not recognized, the error SPICE(NOTSUPPORTED) will be */
/*        signaled. */

/* $ Files */

/*     None. However, the input segment boundaries normally have */
/*     been obtained from a loaded DSK file. */

/* $ Particulars */

/*     This routine sits on top of data DSK type-specific ray-segment */
/*     intercept routines such as DSKX02. */

/* $ Examples */

/*     See usage in ZZDSKBUX. */

/* $ Restrictions */

/*     This is a private routine. It is meant to be used only by the DSK */
/*     subsystem. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman    (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 19-JUL-2016 (NJB) */

/* -& */
/* $ Index_Entries */

/*     find intercept of ray on dsk volume element */

/* -& */

/*     SPICELIB functions */


/*     Local variables */

    if (return_()) {
	return 0;
    }
    chkin_("ZZRYTELT", (ftnlen)8);
    corsys = i_dnnt(&dskdsc[5]);
    if (corsys == 1) {
	zzrytlat_(vertex, raydir, &dskdsc[16], margin, nxpts, xpt);
    } else if (corsys == 3) {
	zzrytrec_(vertex, raydir, &dskdsc[16], margin, nxpts, xpt);
    } else if (corsys == 4) {
	zzrytpdt_(vertex, raydir, &dskdsc[16], &dskdsc[6], margin, nxpts, xpt)
		;
    } else {
	setmsg_("Coordinate system # is not supported.", (ftnlen)37);
	errint_("#", &corsys, (ftnlen)1);
	sigerr_("SPICE(BADCOORDSYS)", (ftnlen)18);
	chkout_("ZZRYTELT", (ftnlen)8);
	return 0;
    }
    chkout_("ZZRYTELT", (ftnlen)8);
    return 0;
} /* zzrytelt_ */

