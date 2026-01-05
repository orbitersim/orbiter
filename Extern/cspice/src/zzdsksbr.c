/* zzdsksbr.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__24 = 24;
static integer c__8 = 8;
static integer c__3 = 3;

/* $Procedure ZZDSKSBR ( DSK, remove entries from API segment buffer ) */
/* Subroutine */ int zzdsksbr_(integer *needed, integer *maxbod, integer *
	stsize, integer *btbody, integer *btnbod, integer *btsegp, integer *
	btstsz, integer *sthan, doublereal *stdscr, integer *stdlad, integer *
	stfree, doublereal *stoff, doublereal *stctr, doublereal *strad)
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    integer i__, j, avail;
    extern /* Subroutine */ int chkin_(char *, ftnlen), moved_(doublereal *, 
	    integer *, doublereal *), movei_(integer *, integer *, integer *);
    integer nbtdel, nstdel;
    extern /* Subroutine */ int setmsg_(char *, ftnlen), errint_(char *, 
	    integer *, ftnlen), sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen);
    extern logical return_(void);

/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines. Users should not call this routine directly due to the */
/*     volatile nature of this routine. */

/*     Remove entries from DSK API segment buffer data structures */
/*     to make room for a new entry. */

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
/*     NEEDED     I   Number of segment table entries needed. */
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


/*     NEEDED     is the number of segment table entries needed to store */
/*                the segment list for a new body. */

/*     MAXBOD     is the maximum size of the body table. BTBODY, BTSEGP, */
/*                and BTSTSZ must each be declared by the caller with */
/*                size at least MAXBOD. */

/*     STSIZE     is the size of the segment table. STHAN, STDSCR, */
/*                STDLAD, STOFF, STCTR, and STRAD must be declared large */
/*                enough to hold STSIZE entries. */

/*     BTBODY     is a table of body IDs. */

/*     BTNBOD     is the number of body IDs currently in BTBODY. */

/*     BTSEGP     is an array of start indices in the segment table */
/*                of existing entries. */

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


/*     BTBODY     is the input body ID table, modified by this routine. */
/*                If it was necessary to delete a body from the table to */
/*                make room, that has been done. */


/*     BTNBOD     is the number of bodies in the body table. */


/*     BTSEGP     is an array of start indices in the segment table */
/*                of existing entries, updated to reflect deletion of */
/*                segments. */


/*     BTSTSZ     is the array of sizes of segment lists of bodies. If */
/*                it was necessary to make room, entries of BTSTSZ */
/*                will have been deleted. */


/*     STHAN      is the segment handle array. If it was necessary to */
/*                make room, entries of STHAN will have been deleted. */


/*     STDSCR     is the segment DSK descriptor array. If it was */
/*                necessary to make room, entries of STDESR will have */
/*                been deleted. */


/*     STDLAD     is the segment DLA descriptor array. If it was */
/*                necessary to make room, entries of STDLAD will have */
/*                been deleted. */


/*     STFREE     is the index of the first free element in each */
/*                segment table array. */


/*     STOFF      is the segment frame center offset array. */
/*                If it was necessary to make room, entries of */
/*                STOFF will have been deleted. */


/*     STCTR      is the segment bounding sphere center array. */
/*                If it was necessary to make room, entries of */
/*                STCTR will have been deleted. */


/*     STRAD      is the segment bounding sphere radius array. */
/*                If it was necessary to make room, entries of */
/*                STRAD will have been deleted. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If MAXBOD is less than 1, the error SPICE(INVALIDTABLESIZE) */
/*         is signaled. */

/*     2)  If the segment table doesn't have at least NEEDED entries, */
/*         the error SPICE(SEGTABLETOOSMALL) is signaled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine is meant to be used only by the DSK subsystem. */

/*     This routine makes room for new entries in the data structures */
/*     used by SPICELIB geometry APIs to perform efficient computations */
/*     using DSK data. It does so by deleting existing entries, if */
/*     necessary. The umbrella routine in which these structures */
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
/*     expected to perform this action by calling ZZDSKSBI. */

/* $ Examples */

/*     See usage in ZZDSKSBF. */

/* $ Restrictions */

/*     1) This is a private routine. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman   (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 11-JUL-2016 (NJB) */

/*        30-JAN-2016 (NJB) */

/*           Original version. */

/* -& */
/* $ Index_Entries */

/*     remove entries from DSK API segment buffer */

/* -& */

/*     SPICELIB functions */


/*     Local variables */

    if (return_()) {
	return 0;
    }
    chkin_("ZZDSKSBR", (ftnlen)8);
    if (*needed > *stsize) {

/*        There's not enough room in the entire segment table. */

	setmsg_("Size of segment table is #; number of entries requested is "
		"#. The segment table is supposed to be declared with suffici"
		"ent size to accommodate all loaded DSK segments.", (ftnlen)
		167);
	errint_("#", stsize, (ftnlen)1);
	errint_("#", needed, (ftnlen)1);
	sigerr_("SPICE(SEGTABLETOOSMALL)", (ftnlen)23);
	chkout_("ZZDSKSBR", (ftnlen)8);
	return 0;
    }

/*     We can't make room in a body table of zero size. */

    if (*maxbod < 1) {
	setmsg_("Body table size must be at least 1 but is #.", (ftnlen)44);
	errint_("#", maxbod, (ftnlen)1);
	sigerr_("SPICE(INVALIDTABLESIZE)", (ftnlen)23);
	chkout_("ZZDSKSBR", (ftnlen)8);
	return 0;
    }

/*     AVAIL is the number of entries currently available. */

    avail = *stsize - *stfree + 1;
    if (avail < *needed) {

/*        We need to make room in the segment table. */

/*        The entries at the end of the body table have the highest */
/*        priority. We scan forward through this table, summing the */
/*        entry counts for each body, until we have enough entries. Let */
/*        NE represent the number of available entries. NE is initially */
/*        the number of unused entries. */

	nstdel = 0;
	i__ = 1;
	while(i__ <= *btnbod && avail < *needed) {

/*           Add the segment count for the Ith body to the total. */

	    nstdel += btstsz[i__ - 1];
	    avail += nstdel;
	    ++i__;
	}

/*        Backstop: we should always have enough room in the segment */
/*        table at this point. */

	if (avail < *needed) {
	    setmsg_("The requested number of segment entries is #; the size "
		    "STSIZE of the input segment  table is #. This error shou"
		    "ld have been trapped before this point.", (ftnlen)150);
	    errint_("#", needed, (ftnlen)1);
	    errint_("#", stsize, (ftnlen)1);
	    sigerr_("SPICE(BUG)", (ftnlen)10);
	    chkout_("ZZDSKSBR", (ftnlen)8);
	    return 0;
	}

/*        At this point, I is the index of the first retained body, */
/*        unless all were deleted, in which case I is BTNBOD+1. We need */
/*        to delete the segment table entries of the bodies indexed from */
/*        1 to I-1. */

	nbtdel = i__ - 1;
	if (nstdel > 0) {

/*           Adjust the tables to be consistent with the deletions. */

/*           Shift the body table and update the body table pointers. */

	    i__1 = *btnbod;
	    for (i__ = nbtdel + 1; i__ <= i__1; ++i__) {
		j = i__ - nbtdel;
		btbody[j - 1] = btbody[i__ - 1];
		btstsz[j - 1] = btstsz[i__ - 1];
		btsegp[j - 1] = btsegp[i__ - 1] - nstdel;
	    }

/*           Update the body table count. */

	    *btnbod -= nbtdel;

/*           Shift the segment table entries forward by NSTDEL to make */
/*           room at the rear of the table. */

	    i__1 = *stfree - 1;
	    for (i__ = nstdel + 1; i__ <= i__1; ++i__) {
		j = i__ - nstdel;
		sthan[j - 1] = sthan[i__ - 1];
		moved_(&stdscr[i__ * 24 - 24], &c__24, &stdscr[j * 24 - 24]);
		movei_(&stdlad[(i__ << 3) - 8], &c__8, &stdlad[(j << 3) - 8]);
		moved_(&stoff[i__ * 3 - 3], &c__3, &stoff[j * 3 - 3]);
		moved_(&stctr[i__ * 3 - 3], &c__3, &stctr[j * 3 - 3]);
		strad[j - 1] = strad[i__ - 1];
	    }
	    *stfree -= nstdel;
	}
    }
    chkout_("ZZDSKSBR", (ftnlen)8);
    return 0;
} /* zzdsksbr_ */

