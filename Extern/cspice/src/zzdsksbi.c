/* zzdsksbi.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__24 = 24;
static integer c__8 = 8;
static integer c__3 = 3;

/* $Procedure ZZDSKSBI ( DSK, initialize API segment buffer ) */
/* Subroutine */ int zzdsksbi_(integer *maxbod, integer *stsize, integer *
	btbody, integer *btnbod, integer *btsegp, integer *btstsz, integer *
	sthan, doublereal *stdscr, integer *stdlad, integer *stfree, 
	doublereal *stoff, doublereal *stctr, doublereal *strad)
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    integer i__;
    extern /* Subroutine */ int chkin_(char *, ftnlen), cleard_(integer *, 
	    doublereal *), cleari_(integer *, integer *), chkout_(char *, 
	    ftnlen);
    extern logical return_(void);

/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines. Users should not call this routine directly due to the */
/*     volatile nature of this routine. */

/*     Initialize DSK API segment buffer data structures. */

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

/*     DLA */
/*     DSK */
/*     TOPOGRAPHY */
/*     UTILITY */

/* $ Declarations */

/*     Include file dla.inc */

/*     This include file declares parameters for DLA format */
/*     version zero. */

/*        Version 3.0.1 17-OCT-2016 (NJB) */

/*           Corrected comment: VERIDX is now described as a DAS */
/*           integer address rather than a d.p. address. */

/*        Version 3.0.0 20-JUN-2006 (NJB) */

/*           Changed name of parameter DSCSIZ to DLADSZ. */

/*        Version 2.0.0 09-FEB-2005 (NJB) */

/*           Changed descriptor layout to make backward pointer */
/*           first element.  Updated DLA format version code to 1. */

/*           Added parameters for format version and number of bytes per */
/*           DAS comment record. */

/*        Version 1.0.0 28-JAN-2004 (NJB) */


/*     DAS integer address of DLA version code. */


/*     Linked list parameters */

/*     Logical arrays (aka "segments") in a DAS linked array (DLA) file */
/*     are organized as a doubly linked list.  Each logical array may */
/*     actually consist of character, double precision, and integer */
/*     components.  A component of a given data type occupies a */
/*     contiguous range of DAS addresses of that type.  Any or all */
/*     array components may be empty. */

/*     The segment descriptors in a SPICE DLA (DAS linked array) file */
/*     are connected by a doubly linked list.  Each node of the list is */
/*     represented by a pair of integers acting as forward and backward */
/*     pointers.  Each pointer pair occupies the first two integers of a */
/*     segment descriptor in DAS integer address space.  The DLA file */
/*     contains pointers to the first integers of both the first and */
/*     last segment descriptors. */

/*     At the DLA level of a file format implementation, there is */
/*     no knowledge of the data contents.  Hence segment descriptors */
/*     provide information only about file layout (in contrast with */
/*     the DAF system).  Metadata giving specifics of segment contents */
/*     are stored within the segments themselves in DLA-based file */
/*     formats. */


/*     Parameter declarations follow. */

/*     DAS integer addresses of first and last segment linked list */
/*     pointer pairs.  The contents of these pointers */
/*     are the DAS addresses of the first integers belonging */
/*     to the first and last link pairs, respectively. */

/*     The acronyms "LLB" and "LLE" denote "linked list begin" */
/*     and "linked list end" respectively. */


/*     Null pointer parameter. */


/*     Segment descriptor parameters */

/*     Each segment descriptor occupies a contiguous */
/*     range of DAS integer addresses. */

/*        The segment descriptor layout is: */

/*           +---------------+ */
/*           | BACKWARD PTR  | Linked list backward pointer */
/*           +---------------+ */
/*           | FORWARD PTR   | Linked list forward pointer */
/*           +---------------+ */
/*           | BASE INT ADDR | Base DAS integer address */
/*           +---------------+ */
/*           | INT COMP SIZE | Size of integer segment component */
/*           +---------------+ */
/*           | BASE DP ADDR  | Base DAS d.p. address */
/*           +---------------+ */
/*           | DP COMP SIZE  | Size of d.p. segment component */
/*           +---------------+ */
/*           | BASE CHR ADDR | Base DAS character address */
/*           +---------------+ */
/*           | CHR COMP SIZE | Size of character segment component */
/*           +---------------+ */

/*     Parameters defining offsets for segment descriptor elements */
/*     follow. */


/*     Descriptor size: */


/*     Other DLA parameters: */


/*     DLA format version.  (This number is expected to occur very */
/*     rarely at integer address VERIDX in uninitialized DLA files.) */


/*     Characters per DAS comment record. */


/*     End of include file dla.inc */


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
/*     MAXBOD     I   Maximum size of body table. */
/*     STSIZE     I   Maximum size of segment table. */
/*     BTBODY    I-O  Body table's body ID array. */
/*     BTNBOD    I-O  Number of entries in body table. */
/*     BTSEGP    I-O  Array of pointers from bodies to segment table. */
/*     BTSTSZ    I-O  Array of sizes of body segment lists. */
/*     STHAN     I-O  Array of handles of segments. */
/*     STDSCR    I-O  Array of DSK descriptors of segments. */
/*     STDLAD    I-O  Array of DLA descriptors of segments. */
/*     STFREE    I-O  Index of first free entry in segment table. */
/*     STOFF     I-O  Offsets of segment frame centers from body. */
/*     STCTR     I-O  Centers of bounding spheres for segments. */
/*     STRAD     I-O  Radii of bounding spheres for segments. */

/* $ Detailed_Input */

/*     MAXBOD     is the maximum size of the body table. BTBODY, BTSEGP, */
/*                and BTSTSZ must each be declared by the caller with */
/*                size at least MAXBOD. */

/*     STSIZE     is the size of the segment table. STHAN, STDSCR, */
/*                STDLAD, STOFF, STCTR, and STRAD must be declared large */
/*                enough to hold STSIZE entries. */

/*     BTBODY     is a table of body IDs. */

/*     BTNBOD     is the number of body IDs currently in BTBODY. */

/*     BTSTSZ     is an array of segment list sizes. The Ith entry */
/*                of BTSTSZ is the length of the segment list for the */
/*                Ith body. */

/*     STHAN      is an array of DAS handles. Each entry of STHAN that */
/*                is in use contains the handle of the DAS file */
/*                containing the DSK segment to which that entry */
/*                corresponds. The Ith entries of STHAN, STDSCR, STDLAD, */
/*                STOFF, STCTR, and STRAD correspond to the same DSK */
/*                segment. */

/*     STDSCR     is an array of DSK descriptors. */

/*     STDLAD     is an array of DLA descriptors. */

/*     STFREE     is the index of the first free entry in the segment */
/*                table. */

/*     STOFF      is an array of offsets of segment frame centers */
/*                from the central bodies of the segment. These offsets */
/*                are expressed in the reference frame of the segment. */
/*                They are constant vectors. Units are km. */

/*     STCTR      is an array of centers of outer bounding spheres for */
/*                segments. Each segment has an outer bounding sphere */
/*                that completely encloses that segment. Each center */
/*                is a vector expressed in the reference frame of the */
/*                the segment, and it is an offset from the frame's */
/*                center. Units are km. */

/*     STRAD      is an array of radii of outer bounding spheres of */
/*                segments. Units are km. */



/* $ Detailed_Output */

/*     BTBODY     is the input body ID table, zero-filled. */


/*     BTNBOD     is the number of bodies in the body table, set to zero. */

/*     BTSTSZ     is the array of sizes of segment lists of bodies, */
/*                zero-filled. */

/*     STHAN      is the segment handle array, zero-filled. */


/*     STDSCR     is the segment DSK descriptor array, zero-filled. */


/*     STDLAD     is the segment DLA descriptor array, zero-filled. */


/*     STFREE     is the index of the first free element in each */
/*                segment table array, set to 1. */


/*     STOFF      is the segment frame center offset array, zero-filled. */


/*     STCTR      is the segment bounding sphere center array, */
/*                zero-filled. */


/*     STRAD      is the segment bounding sphere radius array, */
/*                zero-filled. */


/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     None. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine is meant to be used only by the DSK subsystem. */

/*     This routine initializes data structures used by SPICELIB */
/*     geometry APIs to perform efficient computations using */
/*     DSK data. The umbrella routine in which these structures */
/*     are declared is ZZDSKSBF. */

/*     In a sense, ZZDSKSBF sits "above" the ZZDSKBSR subsystem. */
/*     High-level geometry routines access the data stored in the */
/*     ZZDSKSBF arrays directly; they don't call entry points of */
/*     ZZDSKBSR. */

/*     The ZZDSKSBF subsystem maintains two logical tables: a body */
/*     table and a segment table. Both tables are designed to store */
/*     a fixed maximum number of items; there is no "virtual storage" */
/*     concept that applies. */

/*     Items in both tables have priority assigned on a first-in, */
/*     first-out basis. When it becomes necessary to remove an */
/*     item to make room for another, the lowest-indexed item is */
/*     removed first, and the tables are compressed so that the */
/*     remaining items are contiguous. */

/*     When the state of the underlying ZZDSKBSR system changes due to */
/*     loading or unloading of DSK files, the ZZDSKSBF body and segment */
/*     tables must be re-initialized. Entry points of ZZDSKSBF are */
/*     expected to perform this action by calling this routine. */

/* $ Examples */

/*     See usage in ZZDSKSBF. */

/* $ Restrictions */

/*     1) This is a private routine. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman   (JPL) */
/*     B.V. Semenov       (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 14-OCT-2021 (BVS) */

/*        Bug fix: fixed routine name in CHKIN/CHKOUT calls */
/*        (ZZDSKBSI -> ZZDSKSBI). */

/* -    SPICELIB Version 1.0.0, 11-FEB-2016 (NJB) */

/* -& */
/* $ Index_Entries */

/*     initialize DSK API segment buffer */

/* -& */

/*     SPICELIB functions */


/*     Local variables */

    if (return_()) {
	return 0;
    }
    chkin_("ZZDSKSBI", (ftnlen)8);

/*     Clear the body table. */

    *btnbod = 0;
    i__1 = *maxbod;
    for (i__ = 1; i__ <= i__1; ++i__) {
	btbody[i__ - 1] = 0;
	btsegp[i__ - 1] = 0;
	btstsz[i__ - 1] = 0;
    }

/*     Clear the segment table. */

    i__1 = *stsize;
    for (i__ = 1; i__ <= i__1; ++i__) {
	sthan[i__ - 1] = 0;
	cleard_(&c__24, &stdscr[i__ * 24 - 24]);
	cleari_(&c__8, &stdlad[(i__ << 3) - 8]);
	cleard_(&c__3, &stoff[i__ * 3 - 3]);
	cleard_(&c__3, &stctr[i__ * 3 - 3]);
	strad[i__ - 1] = 0.;
    }
    *stfree = 1;
    chkout_("ZZDSKSBI", (ftnlen)8);
    return 0;
} /* zzdsksbi_ */

