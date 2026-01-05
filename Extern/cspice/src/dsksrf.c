/* dsksrf.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__8 = 8;
static logical c_false = FALSE_;

/* $Procedure DSKSRF ( DSK, get surface IDs for body ) */
/* Subroutine */ int dsksrf_(char *dskfnm, integer *bodyid, integer *srfids, 
	ftnlen dskfnm_len)
{
    /* System generated locals */
    integer i__1, i__2;

    /* Builtin functions */
    integer s_cmp(char *, char *, ftnlen, ftnlen), i_dnnt(doublereal *);

    /* Local variables */
    char arch[4];
    extern integer cardi_(integer *);
    extern /* Subroutine */ int chkin_(char *, ftnlen), dskgd_(integer *, 
	    integer *, doublereal *), errch_(char *, char *, ftnlen, ftnlen);
    logical found;
    extern /* Subroutine */ int movei_(integer *, integer *, integer *);
    extern integer sizei_(integer *);
    extern logical failed_(void);
    integer dladsc[8], handle;
    extern /* Subroutine */ int dlabfs_(integer *, integer *, logical *), 
	    dlafns_(integer *, integer *, integer *, logical *), validi_(
	    integer *, integer *, integer *), dascls_(integer *);
    doublereal dskdsc[24];
    extern /* Subroutine */ int getfat_(char *, char *, char *, ftnlen, 
	    ftnlen, ftnlen);
    integer nxtdsc[8];
    extern /* Subroutine */ int chkout_(char *, ftnlen), setmsg_(char *, 
	    ftnlen), sigerr_(char *, ftnlen);
    char kertyp[4];
    extern logical return_(void);
    extern /* Subroutine */ int dasopr_(char *, integer *, ftnlen), dskcls_(
	    integer *, logical *), errint_(char *, integer *, ftnlen), 
	    appndi_(integer *, integer *);
    integer bid, sid;

/* $ Abstract */

/*     Find the set of surface ID codes for all surfaces associated with */
/*     a given body in a specified DSK file. */

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

/*     CELLS */
/*     DAS */
/*     DSK */
/*     NAIF_IDS */
/*     SETS */

/* $ Keywords */

/*     COVERAGE */
/*     SURFACE */
/*     TOPOGRAPHY */

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

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     DSKFNM     I   Name of DSK file. */
/*     BODYID     I   Integer body ID code. */
/*     SRFIDS    I-O  Set of ID codes of surfaces in DSK file. */

/* $ Detailed_Input */

/*     DSKFNM   is the name of a DSK file. This file will be opened for */
/*              read access by this routine. */

/*     BODYID   is the integer ID code of a body for which topographic */
/*              data are present in the specified DSK file. */

/*     SRFIDS   is an initialized SPICE set data structure. */

/*              SRFIDS optionally may contain a set of surface ID codes */
/*              on input; on output, the ID codes already present in */
/*              SRFIDS will be combined with surface ID code set found */
/*              for the body designated by BODYID in the file DSKFNM. */

/*              If SRFIDS contains no data on input, its size and */
/*              cardinality still must be initialized. */

/* $ Detailed_Output */

/*     SRFIDS   is a SPICE set data structure that contains the union */
/*              of its contents upon input with the set of ID codes of */
/*              the surfaces associated with the body designated by */
/*              BODYID, for which segments were found in the indicated */
/*              DSK file. */

/*              The elements of SPICE sets are unique; each ID code in */
/*              SRFIDS appears only once, even if the DSK file contains */
/*              multiple segments for that ID code. */

/*              See the $Examples section below for a complete example */
/*              program showing how to retrieve body and surface ID codes */
/*              from a DSK file. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the input file has transfer format, the error */
/*         SPICE(INVALIDFORMAT) is signaled. */

/*     2)  If the input file is not a transfer file but has architecture */
/*         other than DAS, the error SPICE(INVALIDARCHTYPE) is signaled. */

/*     3)  If the input file is a binary DAS file of type other than DSK, */
/*         the error SPICE(INVALIDFILETYPE) is signaled. */

/*     4)  If the DSK file cannot be opened or read, an error is signaled */
/*         by a routine in the call tree of this routine. */

/*     5)  If the size of the output set argument SRFIDS is insufficient */
/*         to contain the actual number of ID codes of surfaces covered */
/*         by the indicated DSK file, the error SPICE(CELLTOOSMALL) is */
/*         signaled. */

/* $ Files */

/*     See the description of the argument DSKFNM above. */

/* $ Particulars */

/*     This routine provides an API via which applications can determine */
/*     the set of surfaces associated with a given body in a specified */
/*     DSK file. This routine is normally used together with DSKOBJ. */

/* $ Examples */

/*     The numerical results shown for this example may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */

/*     1) Examine a DSK file and identify the set of central bodies */
/*        associated with the segments in the file. For each body, find */
/*        the set of surfaces associated with that body. */


/*        Example code begins here. */


/*        C */
/*        C     Examine a DSK file and identify the set of */
/*        C     central bodies associated with the segments */
/*        C     in the file. For each body, find the */
/*        C     set of surfaces associated with that body. */
/*        C */
/*              PROGRAM DSKSRF_EX1 */
/*              IMPLICIT NONE */
/*        C */
/*        C     SPICELIB functions */
/*        C */
/*              INTEGER               CARDI */
/*        C */
/*        C     Local parameters */
/*        C */
/*              INTEGER               LBCELL */
/*              PARAMETER           ( LBCELL = -5 ) */

/*              INTEGER               FILSIZ */
/*              PARAMETER           ( FILSIZ = 255 ) */

/*              INTEGER               MAXID */
/*              PARAMETER           ( MAXID  = 10000 ) */
/*        C */
/*        C     Local variables */
/*        C */
/*              CHARACTER*(FILSIZ)    DSKFNM */

/*              INTEGER               BODIDS ( LBCELL : MAXID ) */
/*              INTEGER               I */
/*              INTEGER               J */
/*              INTEGER               SRFIDS ( LBCELL : MAXID ) */

/*        C */
/*        C     Initialize body ID and surface ID cells. */
/*        C */
/*              CALL SSIZEI ( MAXID, BODIDS ) */
/*              CALL SSIZEI ( MAXID, SRFIDS ) */

/*        C */
/*        C     Prompt for the name of a DSK file. */
/*        C */
/*              CALL PROMPT ( 'Enter name of DSK file > ', DSKFNM ) */

/*        C */
/*        C     Obtain body ID set for the DSK. */
/*        C */
/*              CALL DSKOBJ ( DSKFNM, BODIDS ) */

/*              DO I = 1, CARDI( BODIDS ) */

/*                 WRITE (*,*) ' ' */
/*                 WRITE (*,*) 'Body ID:     ', BODIDS(I) */
/*        C */
/*        C        Get the surface IDs for the Ith body. */
/*        C */
/*                 CALL DSKSRF ( DSKFNM, BODIDS(I), SRFIDS ) */

/*                 DO J = 1, CARDI( SRFIDS ) */
/*                    WRITE (*,*) '   Surface ID: ', SRFIDS(J) */
/*                 END DO */

/*              END DO */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, using the DSK file named phobos512.bds, the output */
/*        was: */


/*        Enter name of DSK file > phobos512.bds */

/*         Body ID:              401 */
/*            Surface ID:          401 */


/* $ Restrictions */

/*     1)  If an error occurs while this routine is updating the set */
/*         SRFIDS, the set may be corrupted. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 08-OCT-2021 (JDR) (NJB) */

/*        Changed input argument name "DSK" to "DSKFNM" for consistency */
/*        with other routines. */

/*        Bug fix: added call to FAILED after call to GETFAT. */

/*        Edited the header to comply with NAIF standard. Update problem */
/*        statement in $Examples section. Corrected short error message */
/*        in entries #2, #3 and #5 in $Exceptions section. */

/* -    SPICELIB Version 1.0.0, 22-AUG-2016 (NJB) */

/* -& */
/* $ Index_Entries */

/*     find id codes of surfaces in DSK file */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     Local variables */

    if (return_()) {
	return 0;
    }
    chkin_("DSKSRF", (ftnlen)6);

/*     See whether GETFAT thinks we've got a DSK file. */

    getfat_(dskfnm, arch, kertyp, dskfnm_len, (ftnlen)4, (ftnlen)4);
    if (failed_()) {
	chkout_("DSKSRF", (ftnlen)6);
	return 0;
    }
    if (s_cmp(arch, "XFR", (ftnlen)4, (ftnlen)3) == 0) {
	setmsg_("Input file # has architecture #. The file must be a binary "
		"DSK file to be readable by this routine. If the input file i"
		"s an DSK file in transfer format, run TOBIN on the file to c"
		"onvert it to binary format.", (ftnlen)206);
	errch_("#", dskfnm, (ftnlen)1, dskfnm_len);
	errch_("#", arch, (ftnlen)1, (ftnlen)4);
	sigerr_("SPICE(INVALIDFORMAT)", (ftnlen)20);
	chkout_("DSKSRF", (ftnlen)6);
	return 0;
    } else if (s_cmp(arch, "DAS", (ftnlen)4, (ftnlen)3) != 0) {
	setmsg_("Input file # has architecture #. The file must be a binary "
		"DSK file to be readable by this routine. Binary DSK files ha"
		"ve DAS architecture. If you expected the file to be a binary"
		" DSK file, the problem may be due to the file being an old n"
		"on-native file lacking binary file format information. It's "
		"also possible the file has been corrupted.", (ftnlen)341);
	errch_("#", dskfnm, (ftnlen)1, dskfnm_len);
	errch_("#", arch, (ftnlen)1, (ftnlen)4);
	sigerr_("SPICE(INVALIDARCHTYPE)", (ftnlen)22);
	chkout_("DSKSRF", (ftnlen)6);
	return 0;
    } else if (s_cmp(kertyp, "DSK", (ftnlen)4, (ftnlen)3) != 0) {
	setmsg_("Input file # has file type #. The file must be a binary DSK"
		" file to be readable by this routine. If you expected the fi"
		"le to be a binary DSK file, the problem may be due to the fi"
		"le being an old non-native file lacking binary file format i"
		"nformation. It's also possible the file has been corrupted.", 
		(ftnlen)298);
	errch_("#", dskfnm, (ftnlen)1, dskfnm_len);
	errch_("#", kertyp, (ftnlen)1, (ftnlen)4);
	sigerr_("SPICE(INVALIDFILETYPE)", (ftnlen)22);
	chkout_("DSKSRF", (ftnlen)6);
	return 0;
    }

/*     Open the DSK for read access; start a forward search. */

    dasopr_(dskfnm, &handle, dskfnm_len);
    dlabfs_(&handle, nxtdsc, &found);
    if (failed_()) {
	chkout_("DSKSRF", (ftnlen)6);
	return 0;
    }
    while(found && ! failed_()) {

/*        Get the DSK descriptor of the current segment. */
/*        This is where we'll find the body ID code. */

	movei_(nxtdsc, &c__8, dladsc);
	dskgd_(&handle, dladsc, dskdsc);

/*        The body ID is at location CTRIDX ("center index") */
/*        of the DSK descriptor. */

	bid = i_dnnt(&dskdsc[1]);
	if (bid == *bodyid) {
	    sid = i_dnnt(dskdsc);

/*           Append, rather than insert, the new ID. We'll turn the cell */
/*           into a set at the end of the loop. */

/*           Before appending, make sure there's room in the cell for */
/*           another entry. We can't afford to let APPNDI catch an */
/*           out-of-room error, because we would lose the ability to */
/*           close the file. */

	    if (cardi_(srfids) == sizei_(srfids)) {

/*              We're going to signal an error. Close the DSK first. */

		dskcls_(&handle, &c_false);
		setmsg_("Cannot append surface ID # to cell while reading DS"
			"K file #. Cell size is #.", (ftnlen)76);
		errint_("#", &sid, (ftnlen)1);
		errch_("#", dskfnm, (ftnlen)1, dskfnm_len);
		i__1 = sizei_(srfids);
		errint_("#", &i__1, (ftnlen)1);
		sigerr_("SPICE(CELLTOOSMALL)", (ftnlen)19);
		chkout_("DSKSRF", (ftnlen)6);
		return 0;
	    }
	    appndi_(&sid, srfids);
	}

/*        Fetch the DLA descriptor of the next segment. */

	dlafns_(&handle, dladsc, nxtdsc, &found);
    }
    i__1 = sizei_(srfids);
    i__2 = cardi_(srfids);
    validi_(&i__1, &i__2, srfids);
    dascls_(&handle);
    chkout_("DSKSRF", (ftnlen)6);
    return 0;
} /* dsksrf_ */

