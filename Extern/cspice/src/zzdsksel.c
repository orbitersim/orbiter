/* zzdsksel.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__100 = 100;
static integer c__10 = 10;

/* $Procedure ZZDSKSEL ( DSK, segment selection callback umbrella ) */
logical zzdsksel_0_(int n__, integer *surfid, integer *nsurf, integer *srflst,
	 integer *bodyid, integer *dclass, integer *corsys, doublereal *
	corpar, doublereal *cor1, doublereal *cor2, integer *framid, 
	doublereal *pos, doublereal *et, integer *handle, integer *dladsc, 
	doublereal *dskdsc)
{
    /* Initialized data */

    static logical first = TRUE_;

    /* System generated locals */
    integer i__1, i__2;
    doublereal d__1;
    logical ret_val;

    /* Builtin functions */
    integer i_dnnt(doublereal *), s_rnge(char *, integer, char *, integer);

    /* Local variables */
    doublereal rmat[9]	/* was [3][3] */;
    integer surf;
    extern /* Subroutine */ int vequ_(doublereal *, doublereal *);
    doublereal f;
    integer i__;
    doublereal r__, scale;
    extern /* Subroutine */ int chkin_(char *, ftnlen), moved_(doublereal *, 
	    integer *, doublereal *);
    static doublereal savet;
    extern doublereal twopi_(void);
    doublereal co1min, co2min, co1max, co2max;
    static doublereal savco1, savco2;
    doublereal re;
    extern /* Subroutine */ int refchg_(integer *, integer *, doublereal *, 
	    doublereal *);
    integer segfid;
    extern /* Subroutine */ int recgeo_(doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *);
    static integer savbid;
    extern integer bsrchi_(integer *, integer *, integer *);
    static integer savfid;
    extern /* Subroutine */ int shelli_(integer *, integer *);
    doublereal loccor[1];
    extern /* Subroutine */ int reclat_(doublereal *, doublereal *, 
	    doublereal *, doublereal *);
    extern integer touchi_(integer *);
    doublereal locpos[3];
    static doublereal savpar[10];
    static integer savcls, savnsf, savsrf[100], savtrg;
    extern /* Subroutine */ int sigerr_(char *, ftnlen);
    static doublereal pi2, savpos[3];
    integer segsys;
    extern /* Subroutine */ int chkout_(char *, ftnlen), setmsg_(char *, 
	    ftnlen);
    static integer savsys;
    extern /* Subroutine */ int errint_(char *, integer *, ftnlen);
    doublereal alt, lat, lon;
    extern /* Subroutine */ int mxv_(doublereal *, doublereal *, doublereal *)
	    ;

/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines. Users should not call this routine directly due to the */
/*     volatile nature of this routine. */

/*     This is the umbrella routine for DSK segment selection functions */
/*     that are passed by ZZDSKSNS. Entry points for initializing */
/*     segment selection functions are included as well. */

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
/*     SEARCH */
/*     SEGMENT */
/*     SURFACE */
/*     TOPOGRAPHY */

/* $ Declarations */

/*     File: dsk.inc */


/*     Version 1.0.0 05-FEB-2016 (NJB) */

/*     Maximum size of surface ID list. */


/*     End of include file dsk.inc */


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

/*     VARIABLE  I/O  Entry points */
/*     --------  ---  -------------------------------------------------- */
/*     SURFID     I   ZZDSKMSC, ZZDSKSRC */
/*     NSURF      I   ZZDSKSIT */
/*     SRFLST     I   ZZDSKSIT */
/*     BODYID     I   ZZDSKSBD */
/*     DCLASS     I   ZZDSKSRC */
/*     CORSYS     I   ZZDSKMSC */
/*     CORPAR     I   ZZDSKMSC */
/*     COR1       I   ZZDSKMSC, ZZDSKUSC */
/*     COR2       I   ZZDSKMSC, ZZDSKUSC */
/*     FRAMID     I   ZZDSKMSC, ZZDSKSRC */
/*     POS        I   ZZDSKSRC */
/*     ET         I   ZZDSKMSC, ZZDSKSIT, ZZDSKSRC, ZZDSKUSC */
/*     HANDLE     I   ZZDSKBDC, ZZDSKMMC, ZZDSKUMC, ZZDSKMRC, ZZDSKCIT */
/*     DLADSC     I   ZZDSKBDC, ZZDSKMMC, ZZDSKUMC, ZZDSKMRC, ZZDSKCIT */
/*     DSKDSC     I   ZZDSKBDC, ZZDSKMMC, ZZDSKUMC, ZZDSKMRC, ZZDSKCIT */

/* $ Detailed_Input */

/*     See the entry points for descriptions of their inputs. */

/* $ Detailed_Output */

/*     See the entry points for descriptions of their outputs. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If this routine is called directly the error */
/*         SPICE(BOGUSENTRY) is signaled. */

/*     See the entry points for descriptions of exceptions */
/*     applicable to those entry points. */

/* $ Files */

/*     This routine makes use of DSK files loaded by the ZZDSKBSR */
/*     subsystem. */

/*     Frame kernels may be required in order to perform transformations */
/*     from DSK segment frames to the output frame. In some cases, */
/*     additional kernels such as CK kernels and SCLK kernels could be */
/*     required to support the offset vector lookup. */

/* $ Particulars */

/*     The entry points of this routine are used to search for */
/*     segments matching specified criteria among the set of DSK */
/*     segment descriptors stored in the ZZDSKBSR subsystem. */

/*     For each kind of search, there is an initialization entry point */
/*     used to store search parameters, and there is a corresponding */
/*     segment comparison entry point. The comparison entry points are */
/*     callback routines that are passed to ZZDSKSNS. */

/*     The supported search types and associated entry points are */
/*     listed below. */

/*         Match body ID only */
/*         ------------------ */

/*           Initialization:       ZZDSKSBD */
/*           Segment comparison:   ZZDSKBDC */


/*         Match nothing: */

/*           Segment comparison:   ZZDSKNOT */

/*           This routine is used to force the ZZDSKSBF package to */
/*           build a complete segment list for a specified body. */


/*     The remaining entry points are included to support the SMAP */
/*     Alpha DSK Toolkit. They may be useful for DSK type 4 in a */
/*     future SPICELIB version. */


/*         Match body ID, surface list, and time */
/*         ------------------------------------- */

/*           Initialization:       ZZDSKSIT */
/*           Segment comparison:   ZZDSKCIT */


/*         Match body ID, time, and coordinates */
/*         ------------------------------------ */

/*           Initialization:       ZZDSKUSC */
/*           Segment comparison:   ZZDSKUMC */


/*         Match body ID, surface ID, frame ID, coordinate system, */
/*         coordinate parameters, time, and coordinates */
/*         ------------------------------------------------------ */

/*           Initialization:       ZZDSKMSC */
/*           Segment comparison:   ZZDSKMMC */


/*         Match body ID, surface ID, frame ID, data class, time, */
/*         and whether coverage includes a specified position vector */
/*         --------------------------------------------------------- */

/*           Initialization:       ZZDSKSRC */
/*           Segment comparison:   ZZDSKMRC */



/* $ Examples */

/*     None. */

/* $ Restrictions */

/*     This is a private routine. It is meant to be used only by the DSK */
/*     subsystem. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 23-NOV-2020 (JDR) (NJB) */

/*        Fixed typo in short error message in entry point ZZDSKMRC. */
/*        Corrected Brief_I/O descriptions of input DSKDSC in all entry */
/*        points that accept this input argument. */

/* -    SPICELIB Version 1.0.0, 11-JUL-2016 (NJB) */

/*        Added entry point ZZDSKNOT. */

/*        05-FEB-2016 (NJB) */


/*        29-APR-2015 (NJB) */

/*           Updated description of ZZDSKUMC to specify that the body, */
/*           not the surface, is matched. */

/*        10-OCT-2014 (NJB) */

/*           Added time-independent body search routines */

/*              ZZDSKSBD, ZZDSKCBD */

/*        29-SEP-2014 (NJB) */

/*           Updated ZZDSKUSC, ZZDSKUMC to use body IDs. */

/*        18-SEP-2014 (NJB) */

/*           Updated ZZDSKCIT, ZZDSKSIT to use list of surfaces as well */
/*           as a body ID. Updated umbrella ZZDSKSEL to support new */
/*           arguments. */

/*        15-SEP-2014 (NJB) */

/*           Utilities for selecting segments via */
/*           calls to ZZDSKSNS */

/*        13-MAY-2010 (NJB) */

/*           Developed to support SMAP Alpha DSK Toolkit. */

/* -& */
/* $ Index_Entries */

/*     umbrella for dsk segment selection callback */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     Local variables */


/*     Saved variables */


/*     Initial values */

    /* Parameter adjustments */
    if (srflst) {
	}
    if (corpar) {
	}
    if (pos) {
	}
    if (dladsc) {
	}
    if (dskdsc) {
	}

    /* Function Body */
    switch(n__) {
	case 1: goto L_zzdsksbd;
	case 2: goto L_zzdskbdc;
	case 3: goto L_zzdsknot;
	case 4: goto L_zzdsksit;
	case 5: goto L_zzdskcit;
	case 6: goto L_zzdskusc;
	case 7: goto L_zzdskumc;
	case 8: goto L_zzdskmsc;
	case 9: goto L_zzdskmmc;
	case 10: goto L_zzdsksrc;
	case 11: goto L_zzdskmrc;
	}


/*     Set return value. */

    ret_val = FALSE_;

/*     The following no-op calls are used to suppress compiler */
/*     warnings. */

    i__ = touchi_(handle);
    i__ = touchi_(corsys);
    i__ = touchi_(dladsc);
    i__ = touchi_(framid);
    chkin_("ZZDSKSEL", (ftnlen)8);
    sigerr_("SPICE(BOGUSENTRY)", (ftnlen)17);
    chkout_("ZZDSKSEL", (ftnlen)8);
    return ret_val;
/* $Procedure ZZDSKSBD ( DSK, set body ID ) */

L_zzdsksbd:
/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines. Users should not call this routine directly due to the */
/*     volatile nature of this routine. */

/*     Set body ID only for segment matching. */

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

/*     BODY */
/*     DSK */
/*     SEARCH */
/*     SEGMENT */

/* $ Declarations */

/*     INTEGER               BODYID */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     BODYID     I   ID code of target body. */

/*     The function always returns the value .FALSE. */

/* $ Detailed_Input */

/*     BODYID     is the body ID against which DSK segments' body IDs */
/*                are to be compared. */

/* $ Detailed_Output */

/*     The function always returns the value .FALSE. An output */
/*     is necessary because this routine is an entry point of */
/*     a function. The returned value is meaningless. */

/*     This function operates by side effects. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/* $ Files */

/*     This routine does not use SPICE kernels directly, but it */
/*     supports searches for DSK segments. */

/* $ Particulars */

/*     This routine is meant to prepare for DSK segment searches */
/*     using the DSK segment comparison callback */

/*        ZZDSKBDC */

/*     That routine will indicate a match for segments having */
/*     body ID equal to the input BODYID. */

/* $ Examples */

/*     See usage in ZZDSKSBA. */

/* $ Restrictions */

/*     This is a private routine. It is meant to be used only by the DSK */
/*     subsystem. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman    (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 11-JUL-2016 (NJB) */

/* -& */
/* $ Index_Entries */

/*     set body id for body id selection callback */

/* -& */

/*     Set the function's return value. This value has no */
/*     particular meaning and is not meant to be used by */
/*     the caller. This setting suppresses compiler warnings. */

    ret_val = FALSE_;

/*     Save input value. */

    savbid = *bodyid;
    return ret_val;
/* $Procedure ZZDSKBDC ( DSK, check segment's body ID ) */

L_zzdskbdc:
/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines. Users should not call this routine directly due to the */
/*     volatile nature of this routine. */

/*     Check DSK segment for body ID match. */

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

/*     BODY */
/*     DSK */
/*     SEARCH */
/*     SEGMENT */

/* $ Declarations */

/*     INTEGER               HANDLE */
/*     INTEGER               DLADSC ( * ) */
/*     DOUBLE PRECISION      DSKDSC ( * ) */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     HANDLE     I   Handle of DSK containing segment to be checked. */
/*     DLADSC     I   DLA descriptor of segment to be checked. */
/*     DSKDSC     I   DSK descriptor of segment to be checked. */

/*     The function returns .TRUE. if and only if the specified segment */
/*     matches the body ID set via ZZDSKSBD. */

/* $ Detailed_Input */

/*     HANDLE     is the handle of the DSK containing a segment to */
/*                be checked. */

/*     DLADSC, */
/*     DSKDSC     are, respectively, the DLA descriptor and DSK */
/*                descriptor of a segment to be checked. */

/* $ Detailed_Output */

/*     The function returns .TRUE. if and only if the specified segment */
/*     matches the body ID set via ZZDSKSBD. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/* $ Files */

/*     This routine does not use SPICE kernels directly, but it */
/*     supports searches for DSK segments. */

/* $ Particulars */

/*     This routine is used to search for DSK segments that */
/*     have body ID codes matching that set via the latest */
/*     call to */

/*        ZZDSKSBD */

/*     These searches are conducted by ZZDSKSNS. This function */
/*     is passed to that routine as a callback argument. */

/* $ Examples */

/*     See usage in ZZDSKSBA. */

/* $ Restrictions */

/*     This is a private routine. It is meant to be used only by the DSK */
/*     subsystem. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman    (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.1, 23-NOV-2020 (NJB) */

/*        Corrected Brief_I/O description of input DSKDSC. */

/* -    SPICELIB Version 1.0.0, 11-JUL-2016 (NJB) */

/* -& */
/* $ Index_Entries */

/*     dsk body id segment selection callback */

/* -& */

/*     The body ID is the only DSK segment attribute that must match. */

    ret_val = i_dnnt(&dskdsc[1]) == savbid;
    return ret_val;
/* $Procedure ZZDSKNOT ( DSK, match nothing ) */

L_zzdsknot:
/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines. Users should not call this routine directly due to the */
/*     volatile nature of this routine. */

/*     Return .FALSE. for any segment. */

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

/*     BODY */
/*     DSK */
/*     SEARCH */
/*     SEGMENT */

/* $ Declarations */

/*     INTEGER               HANDLE */
/*     INTEGER               DLADSC ( * ) */
/*     DOUBLE PRECISION      DSKDSC ( * ) */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     HANDLE     I   Handle of DSK containing segment to be checked. */
/*     DLADSC     I   DLA descriptor of segment to be checked. */
/*     DSKDSC     I   DSK descriptor of segment to be checked. */

/*     The function returns .FALSE. for all segments. */

/* $ Detailed_Input */

/*     HANDLE     is the handle of the DSK containing a segment to */
/*                be checked. */

/*     DLADSC, */
/*     DSKDSC     are, respectively, the DLA descriptor and DSK */
/*                descriptor of a segment to be checked. */

/* $ Detailed_Output */

/*     The function returns .FALSE. for all segments. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/* $ Files */

/*     This routine does not use SPICE kernels directly, but it */
/*     supports searches for DSK segments. */

/* $ Particulars */

/*     This function is passed to ZZDSKSNS as a callback argument. */

/*     This routine makes it possible to force ZZDSKSNS to build */
/*     a complete segment list for a specified body in linear time. */

/*     By indicating "no match" for any segment, this routine */
/*     prevents a search for matching segments from terminating */
/*     until all available files have been searched. */

/* $ Examples */

/*     See usage in ZZDSKBBL. */

/* $ Restrictions */

/*     This is a private routine. It is meant to be used only by the DSK */
/*     subsystem. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman    (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.1, 23-NOV-2020 (JDR) (NJB) */

/*        Corrected Brief_I/O description of input DSKDSC. */

/* -    SPICELIB Version 1.0.0, 11-JUL-2016 (NJB) */

/* -& */
/* $ Index_Entries */

/*     dsk no-match callback */

/* -& */

/*     Whatever's in this segment, it doesn't match. */

    ret_val = FALSE_;
    return ret_val;
/* $Procedure ZZDSKSIT ( DSK, set body ID, surfaces, and time ) */

L_zzdsksit:
/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines. Users should not call this routine directly due to the */
/*     volatile nature of this routine. */

/*     Set body ID code, surface list, and time for segment matching. */

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

/*     BODY */
/*     DSK */
/*     SEARCH */
/*     SEGMENT */

/* $ Declarations */

/*     INTEGER               BODYID */
/*     INTEGER               NSURF */
/*     INTEGER               SRFLST ( * ) */
/*     DOUBLE PRECISION      ET */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     BODYID     I   ID code of target body. */
/*     NSURF      I   Surface ID count. */
/*     SRFLST     I   Surface ID list. */
/*     ET         I   Epoch, expressed as seconds past J2000 TDB. */

/*     The function always returns the value .FALSE. */

/* $ Detailed_Input */

/*     BODYID     is the body ID against which DSK segments' body IDs */
/*                are to be compared. */

/*     NSURF, */
/*     SRFLST     are, respectively, a surface ID count and a list of */
/*                surface IDs. Matching segments must have a surface */
/*                belonging to this list. */

/*                If the list is empty, all surfaces are considered */
/*                to be matches. */

/*     ET         is an epoch, expressed as seconds past J2000 TDB. */
/*                All matching segments must contain ET in their */
/*                time coverage intervals. */

/* $ Detailed_Output */

/*     The function always returns the value .FALSE. An output */
/*     is necessary because this routine is an entry point of */
/*     a function. The returned value is meaningless. */

/*     This function operates by side effects. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/* $ Files */

/*     This routine does not use SPICE kernels directly, but it */
/*     supports searches for DSK segments. */

/* $ Particulars */

/*     This routine is meant to prepare for DSK segment searches */
/*     using the DSK segment comparison callback */

/*        ZZDSKCIT */

/* $ Examples */

/*     None. */

/* $ Restrictions */

/*     This is a private routine. It is meant to be used only by the DSK */
/*     subsystem. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman    (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 11-JUL-2016 (NJB) */

/* -& */
/* $ Index_Entries */

/*     set body id, surfaces, and time for corresponding callback */

/* -& */

/*     Set return value. */

    ret_val = FALSE_;

/*     Save input values. */

    savbid = *bodyid;
    savet = *et;
    if (*nsurf > 100) {
	chkin_("ZZDSKSIT", (ftnlen)8);
	setmsg_("Maximum allowed surface ID count is #; input count was #.", (
		ftnlen)57);
	errint_("#", &c__100, (ftnlen)1);
	errint_("#", nsurf, (ftnlen)1);
	sigerr_("SPICE(VALUEOUTOFRANGE)", (ftnlen)22);
	chkout_("ZZDSKSIT", (ftnlen)8);
	return ret_val;
    }
    savnsf = *nsurf;
    i__1 = *nsurf;
    for (i__ = 1; i__ <= i__1; ++i__) {
	savsrf[(i__2 = i__ - 1) < 100 && 0 <= i__2 ? i__2 : s_rnge("savsrf", 
		i__2, "zzdsksel_", (ftnlen)973)] = srflst[i__ - 1];
    }
    shelli_(nsurf, savsrf);
    return ret_val;
/* $Procedure ZZDSKCIT ( DSK, check body ID, surface, and time ) */

L_zzdskcit:
/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines. Users should not call this routine directly due to the */
/*     volatile nature of this routine. */

/*     Check DSK segment for body ID, surface, and time coverage match. */

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

/*     BODY */
/*     DSK */
/*     SEARCH */
/*     SEGMENT */

/* $ Declarations */

/*     INTEGER               HANDLE */
/*     INTEGER               DLADSC ( * ) */
/*     DOUBLE PRECISION      DSKDSC ( * ) */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     HANDLE     I   Handle of DSK containing segment to be checked. */
/*     DLADSC     I   DLA descriptor of segment to be checked. */
/*     DSKDSC     I   DSK descriptor of segment to be checked. */

/*     The function returns .TRUE. if and only if the specified segment */
/*     matches the body ID set via ZZDSKSIT. */

/* $ Detailed_Input */

/*     HANDLE     is the handle of the DSK containing a segment to */
/*                be checked. */

/*     DLADSC, */
/*     DSKDSC     are, respectively, the DLA descriptor and DSK */
/*                descriptor of a segment to be checked. */

/* $ Detailed_Output */

/*     The function returns .TRUE. if and only if the specified segment */
/*     matches the body ID set via ZZDSKSIT. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/* $ Files */

/*     This routine does not use SPICE kernels directly, but it */
/*     supports searches for DSK segments. */

/* $ Particulars */

/*     This routine is used to search for DSK segments that */
/*     have body ID codes and time coverage intervals matching the */
/*     values set via the latest call to */

/*        ZZDSKSIT */

/*     These searches are conducted by ZZDSKSNS. This function */
/*     is passed to that routine as a callback argument. */

/* $ Examples */

/*     None. */

/* $ Restrictions */

/*     This is a private routine. It is meant to be used only by the DSK */
/*     subsystem. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman    (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.1, 23-NOV-2020 (JDR) (NJB) */

/*        Corrected Brief_I/O description of input DSKDSC. */

/* -    SPICELIB Version 1.0.0, 11-JUL-2016 (NJB) */

/* -& */
/* $ Index_Entries */

/*     dsk body id and time segment selection callback */

/* -& */

/*     Currently, we don't need to access the DSK file in order */
/*     to make this test, but the required inputs are available. */

    ret_val = FALSE_;
    if (savbid == i_dnnt(&dskdsc[1])) {
	if (savet >= dskdsc[22] && savet <= dskdsc[23]) {
	    if (savnsf < 1) {

/*              There are no surface ID constraints; we have */
/*              a match. */

		ret_val = TRUE_;
	    } else {

/*              We have a match if and only if the surface ID of this */
/*              segment is on the list of allowed surface IDs. */

		surf = i_dnnt(dskdsc);
		ret_val = bsrchi_(&surf, &savnsf, savsrf) > 0;
	    }
	}
    }
    return ret_val;
/* $Procedure ZZDSKUSC ( DSK, set body ID, coordinates, and time ) */

L_zzdskusc:
/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines. Users should not call this routine directly due to the */
/*     volatile nature of this routine. */

/*     Set body ID, time, and coordinates for "raw" unchecked search. */

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

/*     BODY */
/*     DSK */
/*     SEARCH */
/*     SEGMENT */

/* $ Declarations */

/*     INTEGER               BODYID */
/*     DOUBLE PRECISION      ET */
/*     DOUBLE PRECISION      COR1 */
/*     DOUBLE PRECISION      COR2 */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     BODYID     I   ID code of target body. */
/*     ET         I   Epoch, expressed as seconds past J2000 TDB. */
/*     COR1       I   First coordinate. */
/*     COR2       I   Second coordinate. */

/*     The function always returns the value .FALSE. */

/* $ Detailed_Input */

/*     BODYID     is the body ID against which DSK segments' body IDs */
/*                are to be compared. */

/*     ET         is an epoch, expressed as seconds past J2000 TDB. */
/*                All matching segments must contain ET in their */
/*                time coverage intervals. */

/*     COR1, */
/*     COR2       are two coordinates that must be included in the */
/*                spatial region covered by a matching segment. */

/*                These are "domain" coordinates for class 1 */
/*                segments. */

/* $ Detailed_Output */

/*     The function always returns the value .FALSE. An output */
/*     is necessary because this routine is an entry point of */
/*     a function. The returned value is meaningless. */

/*     This function operates by side effects. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/* $ Files */

/*     This routine does not use SPICE kernels directly, but it */
/*     supports searches for DSK segments. */

/* $ Particulars */

/*     This routine is meant to prepare for DSK segment searches */
/*     using the DSK segment comparison callback */

/*        ZZDSKUMC */

/*     This routine is applicable only for cases where the loaded */
/*     segments have a common reference frame and coordinate system, */
/*     and where the segment class is 1 ("single-valued function"). */

/* $ Examples */

/*     None. */

/* $ Restrictions */

/*     This is a private routine. It is meant to be used only by the DSK */
/*     subsystem. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman    (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 11-JUL-2016 (NJB) */

/* -& */
/* $ Index_Entries */

/*     set body id, time, and coordinates for callback */

/* -& */
    ret_val = FALSE_;
    savbid = *bodyid;
    savet = *et;
    savco1 = *cor1;
    savco2 = *cor2;
    return ret_val;
/* $Procedure ZZDSKUMC ( DSK, check body ID, coordinates, and time ) */

L_zzdskumc:
/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines. Users should not call this routine directly due to the */
/*     volatile nature of this routine. */

/*     Perform an unchecked segment match using body ID, time, */
/*     and coordinates. */

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

/*     BODY */
/*     DSK */
/*     SEARCH */
/*     SEGMENT */

/* $ Declarations */

/*     INTEGER               HANDLE */
/*     INTEGER               DLADSC ( * ) */
/*     DOUBLE PRECISION      DSKDSC ( * ) */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     HANDLE     I   Handle of DSK containing segment to be checked. */
/*     DLADSC     I   DLA descriptor of segment to be checked. */
/*     DSKDSC     I   DSK descriptor of segment to be checked. */

/*     The function returns .TRUE. if and only if the specified segment */
/*     matches the body ID set via ZZDSKUSC. */

/* $ Detailed_Input */

/*     HANDLE     is the handle of the DSK containing a segment to */
/*                be checked. */

/*     DLADSC, */
/*     DSKDSC     are, respectively, the DLA descriptor and DSK */
/*                descriptor of a segment to be checked. */

/* $ Detailed_Output */

/*     The function returns .TRUE. if and only if the specified segment */
/*     matches the body ID, time, and coordinates set via ZZDSKUSC. */

/*     A time and coordinate match means that the input values are */
/*     contained in the respective time interval and spatial coverage */
/*     region of the segment. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/* $ Files */

/*     This routine does not use SPICE kernels directly, but it */
/*     supports searches for DSK segments. */

/* $ Particulars */

/*     This routine is used to search for DSK segments that */
/*     have body ID codes and time coverage intervals matching the */
/*     values set via the latest call to */

/*        ZZDSKUSC. */

/*     These searches are conducted by ZZDSKSNS. This function */
/*     is passed to that routine as a callback argument. */

/* $ Examples */

/*     None. */

/* $ Restrictions */

/*     This is a private routine. It is meant to be used only by the DSK */
/*     subsystem. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman    (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.1, 23-NOV-2020 (JDR) (NJB) */

/*        Corrected Brief_I/O description of input DSKDSC. */

/* -    SPICELIB Version 1.0.0, 11-JUL-2016 (NJB) */

/* -& */
/* $ Index_Entries */

/*     dsk body id, time, and coordinate selection callback */

/* -& */

/*     We don't have a match to begin with. */

    ret_val = FALSE_;
    if (first) {
	pi2 = twopi_();
	first = FALSE_;
    }

/*     Check the body ID first. */

    if (savbid == i_dnnt(&dskdsc[1])) {

/*        The body ID matches; check the time coverage. */

	if (savet >= dskdsc[22] && savet <= dskdsc[23]) {

/*           Check the coordinates. Note that we don't */
/*           know whether the frame and coordinates are */
/*           reasonable. It's up to the user to ensure this. */

/*           We do need to know whether the first coordinate */
/*           is a longitude, since we may need to adjust it to */
/*           get it into the range of the segment. */

	    segsys = i_dnnt(&dskdsc[5]);

/*           Make a local copy of the first coordinate. */

	    loccor[0] = savco1;
	    if (segsys == 1 || segsys == 4) {

/*              Adjust segment bounds using a small margin. */

		co1min = dskdsc[16] - 1e-12;
		co1max = dskdsc[17] + 1e-12;
		co2min = dskdsc[18] - 1e-12;
		co2max = dskdsc[19] + 1e-12;

/*              Move longitude into range. */

		if (loccor[0] < co1min) {
		    loccor[0] += pi2;
		} else if (loccor[0] > co1max) {
		    loccor[0] -= pi2;
		}
	    } else {
		scale = 1.0000000000010001;
		co1min = dskdsc[16] - scale * abs(dskdsc[16]);
		co1max = dskdsc[17] + scale * abs(dskdsc[17]);
		co2min = dskdsc[18] - scale * abs(dskdsc[18]);
		co2max = dskdsc[19] + scale * abs(dskdsc[19]);
	    }

/*           Check the first coordinate against the segment's */
/*           corresponding coverage range. */

	    if (loccor[0] < co1min || loccor[0] > co1max) {

/*              The first input coordinate is not covered by this */
/*              segment. */

		return ret_val;
	    }

/*           Compare the second coordinate against the segment's */
/*           corresponding coverage range. */

	    if (savco2 < co2min || savco2 > co2max) {

/*              The second input coordinate is not covered by this */
/*              segment. */

		return ret_val;
	    }

/*           At this point we have a match. */

	    ret_val = TRUE_;
	}

/*        This is the end of the time check block. */

    }

/*     This is the end of the surface ID check block. */

    return ret_val;
/* $Procedure ZZDSKMSC ( DSK, setup matched attribute search ) */

L_zzdskmsc:
/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines. Users should not call this routine directly due to the */
/*     volatile nature of this routine. */

/*     Set inputs for a matched attribute search, also known as a */
/*     "checked" search. */

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

/*     BODY */
/*     DSK */
/*     SEARCH */
/*     SEGMENT */

/* $ Declarations */

/*     INTEGER               BODYID */
/*     INTEGER               SURFID */
/*     INTEGER               FRAMID */
/*     INTEGER               CORSYS */
/*     DOUBLE PRECISION      CORPAR ( * ) */
/*     DOUBLE PRECISION      ET */
/*     DOUBLE PRECISION      COR1 */
/*     DOUBLE PRECISION      COR2 */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     BODYID     I   ID code of target body. */
/*     SURFID     I   ID code of target surface. */
/*     FRAMID     I   ID code of body-fixed reference frame. */
/*     CORSYS     I   Coordinate system code. */
/*     CORPAR     I   Coordinate system parameters. */
/*     ET         I   Epoch, expressed as seconds past J2000 TDB. */
/*     COR1       I   First coordinate. */
/*     COR2       I   Second coordinate. */

/*     The function always returns the value .FALSE. */

/* $ Detailed_Input */

/*     BODYID     is the body ID against which DSK segments' body IDs */
/*                are to be compared. */

/*     SURFID     is the surface ID against which DSK segments' surface */
/*                IDs are to be compared. */

/*     FRAMID     is the reference frame ID against which DSK segments' */
/*                frame IDs are to be compared. */

/*     CORSYS     is the coordinate system code against which DSK */
/*                segments' coordinate systems are to be compared. */

/*     CORPAR     is an array containing the coordinate system */
/*                parameters against which DSK segments' coordinate */
/*                system parameters are to be compared. */

/*                A small margin is used for parameter comparison. */


/*     ET         is an epoch, expressed as seconds past J2000 TDB. */
/*                All matching segments must contain ET in their */
/*                time coverage intervals. */

/*     COR1, */
/*     COR2       are two coordinates that must be included in the */
/*                spatial region covered by a matching segment. */

/*                These are "domain" coordinates for class 1 */
/*                segments. */

/* $ Detailed_Output */

/*     The function always returns the value .FALSE. An output */
/*     is necessary because this routine is an entry point of */
/*     a function. The returned value is meaningless. */

/*     This function operates by side effects. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/* $ Files */

/*     This routine does not use SPICE kernels directly, but it */
/*     supports searches for DSK segments. */

/* $ Particulars */

/*     This routine is meant to prepare for DSK segment searches */
/*     using the DSK segment comparison callback */

/*        ZZDSKMMC */

/*     This routine supports "matched attribute" searches. */
/*     Segment attributes are checked explicitly. */

/* $ Examples */

/*     None. */

/* $ Restrictions */

/*     This is a private routine. It is meant to be used only by the DSK */
/*     subsystem. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman    (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 11-JUL-2016 (NJB) */

/* -& */
/* $ Index_Entries */

/*     set inputs for matched attribute callback */

/* -& */
    ret_val = FALSE_;
    savtrg = *bodyid;
    savbid = *surfid;
    savfid = *framid;
    savsys = *corsys;
    moved_(corpar, &c__10, savpar);
    savet = *et;
    savco1 = *cor1;
    savco2 = *cor2;
    return ret_val;
/* $Procedure ZZDSKMMC ( DSK, matched segment attribute comparison ) */

L_zzdskmmc:
/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines. Users should not call this routine directly due to the */
/*     volatile nature of this routine. */


/*     Perform a matched attribute comparison using target ID, surface */
/*     ID, frame ID, coordinate system, coordinate parameters, time, and */
/*     coordinates. */


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

/*     BODY */
/*     DSK */
/*     SEARCH */
/*     SEGMENT */

/* $ Declarations */

/*     INTEGER               HANDLE */
/*     INTEGER               DLADSC ( * ) */
/*     DOUBLE PRECISION      DSKDSC ( * ) */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     HANDLE     I   Handle of DSK containing segment to be checked. */
/*     DLADSC     I   DLA descriptor of segment to be checked. */
/*     DSKDSC     I   DSK descriptor of segment to be checked. */

/*     The function returns .TRUE. if and only if the specified segment */
/*     matches the body ID set via ZZDSKMSC. */

/* $ Detailed_Input */

/*     HANDLE     is the handle of the DSK containing a segment to */
/*                be checked. */

/*     DLADSC, */
/*     DSKDSC     are, respectively, the DLA descriptor and DSK */
/*                descriptor of a segment to be checked. */

/* $ Detailed_Output */

/*     The function returns .TRUE. if and only if the specified segment */
/*     matches the */

/*        body ID */
/*        surface ID */
/*        frame ID */
/*        coordinate system */
/*        coordinate parameters */
/*        time */
/*        coordinates */

/*     set via ZZDSKMSC. */

/*     A time and coordinate match means that the input values are */
/*     contained in the respective time interval and spatial coverage */
/*     region of the segment. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/* $ Files */

/*     This routine does not use SPICE kernels directly, but it */
/*     supports searches for DSK segments. */

/* $ Particulars */

/*     This routine is used to search for DSK segments that have */
/*     attributes matching the values set via the latest call to */

/*        ZZDSKMSC */

/*     This is a "matched attribute" search, also known as a */
/*     "checked" search. See Detailed_Output above. */

/*     These searches are conducted by ZZDSKSNS. This function */
/*     is passed to that routine as a callback argument. */

/* $ Examples */

/*     None. */

/* $ Restrictions */

/*     This is a private routine. It is meant to be used only by the DSK */
/*     subsystem. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman    (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.1, 23-NOV-2020 (JDR) (NJB) */

/*        Corrected Brief_I/O description of input DSKDSC. */

/* -    SPICELIB Version 1.0.0, 11-JUL-2016 (NJB) */

/* -& */
/* $ Index_Entries */

/*     dsk matched segment attribute callback */

/* -& */

/*     We don't have a match to begin with. */

    ret_val = FALSE_;
    if (first) {
	pi2 = twopi_();
	first = FALSE_;
    }

/*     Check the target ID. */

    if (savtrg != i_dnnt(&dskdsc[1])) {
	return ret_val;
    }

/*     Check the surface ID. */

    if (savbid != i_dnnt(dskdsc)) {
	return ret_val;
    }

/*     Check the frame ID. */

    if (savfid != i_dnnt(&dskdsc[4])) {
	return ret_val;
    }

/*     Check the coordinate system. */

    if (savsys != i_dnnt(&dskdsc[5])) {
	return ret_val;
    }

/*     If the system is planetodetic, check the reference */
/*     ellipsoid parameters. */

    if (savsys == 4) {
	if ((d__1 = savpar[0] - dskdsc[6], abs(d__1)) > 1e-12) {
	    return ret_val;
	}
	if ((d__1 = savpar[1] - dskdsc[7], abs(d__1)) > 1e-12) {
	    return ret_val;
	}
    }

/*     The segment attributes match; check the time coverage. */

    if (savet >= dskdsc[22] && savet <= dskdsc[23]) {

/*        Check the coordinates. */

	segsys = i_dnnt(&dskdsc[5]);

/*        Make a local copy of the first coordinate. */

	loccor[0] = savco1;
	if (segsys == 1 || segsys == 4) {

/*           Adjust segment bounds using a small margin. */

	    co1min = dskdsc[16] - 1e-12;
	    co1max = dskdsc[17] + 1e-12;
	    co2min = dskdsc[18] - 1e-12;
	    co2max = dskdsc[19] + 1e-12;

/*           Move longitude into range. */

	    if (loccor[0] < co1min) {
		loccor[0] += pi2;
	    } else if (loccor[0] > co1max) {
		loccor[0] -= pi2;
	    }
	} else {
	    scale = 1.0000000000010001;
	    co1min = dskdsc[16] - scale * abs(dskdsc[16]);
	    co1max = dskdsc[17] + scale * abs(dskdsc[17]);
	    co2min = dskdsc[18] - scale * abs(dskdsc[18]);
	    co2max = dskdsc[19] + scale * abs(dskdsc[19]);
	}

/*        Check the first coordinate against the segment's */
/*        corresponding coverage range. */

	if (loccor[0] < co1min || loccor[0] > co1max) {

/*           The first input coordinate is not covered by this */
/*           segment. */

	    return ret_val;
	}

/*        Compare the second coordinate against the segment's */
/*        corresponding coverage range. */

	if (savco2 < co2min || savco2 > co2max) {

/*           The second input coordinate is not covered by this */
/*           segment. */

	    return ret_val;
	}

/*        At this point we have a match. */

	ret_val = TRUE_;
    }
    return ret_val;
/* $Procedure ZZDSKSRC ( DSK, setup rectangular coordinate search ) */

L_zzdsksrc:
/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines. Users should not call this routine directly due to the */
/*     volatile nature of this routine. */

/*     Set surface and target ID, data class, ET, and rectangular */
/*     coordinates for a coverage match search. Set the frame ID */
/*     as well; this is needed to define the position vector. */
/*     Matching segments need not use the same frame. */

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

/*     BODY */
/*     DSK */
/*     SEARCH */
/*     SEGMENT */

/* $ Declarations */

/*     INTEGER               SURFID */
/*     INTEGER               BODYID */
/*     INTEGER               DCLASS */
/*     DOUBLE PRECISION      ET */
/*     INTEGER               FRAMID */
/*     DOUBLE PRECISION      POS    ( 3 ) */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     SURFID     I   ID code of target surface. */
/*     BODYID     I   ID code of target body. */
/*     DCLASS     I   Data class of segment. */
/*     ET         I   Epoch, expressed as seconds past J2000 TDB. */
/*     FRAMID     I   ID code of body-fixed reference frame. */
/*     POS        I   Cartesian position vector. */

/*     The function always returns the value .FALSE. */

/* $ Detailed_Input */

/*     SURFID     is the surface ID against which DSK segments' surface */
/*                IDs are to be compared. */

/*     BODYID     is the body ID against which DSK segments' body IDs */
/*                are to be compared. */

/*     DCLASS     is the data class against which DSK segments' data */
/*                classes are to be compared. */

/*     ET         is an epoch, expressed as seconds past J2000 TDB. */
/*                All matching segments must contain ET in their */
/*                time coverage intervals. */


/*     FRAMID     is the reference frame ID of the frame in which */
/*                the input vector POS is expressed. FRAMID is */
/*                not used for comparison with segments' frame IDs. */

/*     POS        is a Cartesian position vector, expressed in */
/*                the frame designated by FRAMID. POS represents */
/*                an offset from the target body's center. */

/*                Segments are considered to "match" POS if the */
/*                latitude and longitude of POS are within the */
/*                spatial boundaries of those segments. */

/* $ Detailed_Output */

/*     The function always returns the value .FALSE. An output */
/*     is necessary because this routine is an entry point of */
/*     a function. The returned value is meaningless. */

/*     This function operates by side effects. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/* $ Files */

/*     This routine does not use SPICE kernels directly, but it */
/*     supports searches for DSK segments. */

/* $ Particulars */

/*     This routine is meant to prepare for DSK segment searches */
/*     using the DSK segment comparison callback */

/*        ZZDSKMRC */

/* $ Examples */

/*     None. */

/* $ Restrictions */

/*     1)  This is a private routine. It is meant to be used only by the */
/*         DSK subsystem. */

/*     2)  This routine works only with segments that use latitudinal or */
/*         planetodetic coordinates. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman    (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 11-JUL-2016 (NJB) */

/* -& */
/* $ Index_Entries */

/*     set inputs for dsk rectangular coordinate callback */

/* -& */
    ret_val = FALSE_;
    savbid = *surfid;
    savtrg = *bodyid;
    savcls = *dclass;
    savet = *et;
    savfid = *framid;
    vequ_(pos, savpos);
    return ret_val;
/* $Procedure ZZDSKMRC ( DSK, rectangular coordinate comparison ) */

L_zzdskmrc:
/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines. Users should not call this routine directly due to the */
/*     volatile nature of this routine. */

/*     Perform a segment match using surface ID, target, data */
/*     class, time, and rectangular coordinates. */

/*     A search using this function must be initialized */
/*     by a call to ZZDSKSRC. */


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

/*     BODY */
/*     DSK */
/*     SEARCH */
/*     SEGMENT */

/* $ Declarations */

/*     INTEGER               HANDLE */
/*     INTEGER               DLADSC ( * ) */
/*     DOUBLE PRECISION      DSKDSC ( * ) */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     HANDLE     I   Handle of DSK containing segment to be checked. */
/*     DLADSC     I   DLA descriptor of segment to be checked. */
/*     DSKDSC     I   DSK descriptor of segment to be checked. */

/*     The function returns .TRUE. if and only if the specified segment */
/*     matches the body ID set via ZZDSKSRC. */

/* $ Detailed_Input */

/*     HANDLE     is the handle of the DSK containing a segment to */
/*                be checked. */

/*     DLADSC, */
/*     DSKDSC     are, respectively, the DLA descriptor and DSK */
/*                descriptor of a segment to be checked. */

/* $ Detailed_Output */

/*     The function returns .TRUE. if and only if the specified segment */
/*     matches the */

/*        surface ID */
/*        body ID */
/*        class */
/*        time */
/*        frame ID */
/*        coordinates */

/*     set via ZZDSKSRC. */

/*     A time and coordinate match means that the input values are */
/*     contained in the respective time interval and spatial coverage */
/*     region of the segment. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/* $ Files */

/*     This routine does not use SPICE kernels directly, but it */
/*     supports searches for DSK segments. */

/* $ Particulars */

/*     This routine is used to search for DSK segments that have */
/*     attributes matching the values set via the latest call to */

/*        ZZDSKSRC */

/*     These searches are conducted by ZZDSKSNS. This function */
/*     is passed to that routine as a callback argument. */

/* $ Examples */

/*     None. */

/* $ Restrictions */

/*     This is a private routine. It is meant to be used only by the DSK */
/*     subsystem. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */

/* $ Version */

/* -    SPICELIB Version 1.0.1, 23-NOV-2020 (JDR) (NJB) */

/*        Fixed typo in short error message SPICE(NOTSUPPORTED). */
/*        Corrected Brief_I/O description of input DSKDSC. */

/* -    SPICELIB Version 1.0.0, 11-JUL-2016 (NJB) */

/* -& */
/* $ Index_Entries */

/*     dsk match rectangular coordinate callback */

/* -& */

/*     We don't have a match to begin with. */

    ret_val = FALSE_;
    if (first) {
	pi2 = twopi_();
	first = FALSE_;
    }
/*     Check the surface ID. */

    if (savbid != i_dnnt(dskdsc)) {
	return ret_val;
    }

/*     Reject any segment whose target (center) ID doesn't match */
/*     the request ID. */

    if (savtrg != i_dnnt(&dskdsc[1])) {
	return ret_val;
    }

/*     Check the time coverage. */

    if (savet < dskdsc[22] || savet > dskdsc[23]) {
	return ret_val;
    }

/*     Check the data class. */

    if (savcls != i_dnnt(&dskdsc[2])) {
	return ret_val;
    }

/*     Check whether the position vector is covered by the segment. */
/*     In order to determine this, we need to transform the vector */
/*     into the frame of the segment, if the frames differ. */

    segfid = i_dnnt(&dskdsc[4]);
    if (savfid == segfid) {

/*        The request frame and segment frame match. Just copy */
/*        the saved vector. */

	vequ_(savpos, locpos);
    } else {

/*        Transform the saved vector to the frame of the */
/*        segment. The transformation epoch is the saved */
/*        value of ET. */

	refchg_(&savfid, &segfid, &savet, rmat);
	mxv_(rmat, savpos, locpos);
    }
/*     We do need to know whether the first coordinate is a longitude, */
/*     since we may need to adjust it to get it into the range of the */
/*     segment. */

    segsys = i_dnnt(&dskdsc[5]);
    if (segsys == 1 || segsys == 4) {

/*        Find the latitude and longitude of the input point, expressed */
/*        in the coordinate system of this segment. */

	if (segsys == 1) {
	    reclat_(locpos, &r__, &lon, &lat);
	} else if (segsys == 4) {
	    re = dskdsc[6];
	    f = dskdsc[7];
	    recgeo_(locpos, &re, &f, &lon, &lat, &alt);
	} else {
	    chkin_("ZZDSKMRC", (ftnlen)8);
	    setmsg_("Backstop error (0): this code should be unreachable.", (
		    ftnlen)52);
	    sigerr_("SPICE(BUG)", (ftnlen)10);
	    chkout_("ZZDSKMRC", (ftnlen)8);
	    return ret_val;
	}

/*        Adjust segment bounds using a small margin. */

	co1min = dskdsc[16] - 1e-12;
	co1max = dskdsc[17] + 1e-12;
	co2min = dskdsc[18] - 1e-12;
	co2max = dskdsc[19] + 1e-12;
/*        Move longitude into range. */

	if (lon < co1min) {
	    lon += pi2;
	} else if (lon > co1max) {
	    lon -= pi2;
	}
    } else {
	chkin_("ZZDSKMRC", (ftnlen)8);
	setmsg_("Only planetocentric and planetodetic coordinates are suppor"
		"ted by this entry point. Segment coordinate system was #.", (
		ftnlen)116);
	errint_("#", &segsys, (ftnlen)1);
	sigerr_("SPICE(NOTSUPPORTED)", (ftnlen)19);
	chkout_("ZZDSKMRC", (ftnlen)8);
	return ret_val;
    }

/*     Check the first coordinate against the segment's */
/*     corresponding coverage range. */

    if (lon < co1min || lon > co1max) {

/*        The first input coordinate is not covered by this */
/*        segment. */

	return ret_val;
    }

/*     Compare the second coordinate against the segment's */
/*     corresponding coverage range. */

    if (lat < co2min || lat > co2max) {

/*        The second input coordinate is not covered by this */
/*        segment. */

	return ret_val;
    }

/*     At this point we have a match. */

    ret_val = TRUE_;
    return ret_val;
} /* zzdsksel_ */

logical zzdsksel_(integer *surfid, integer *nsurf, integer *srflst, integer *
	bodyid, integer *dclass, integer *corsys, doublereal *corpar, 
	doublereal *cor1, doublereal *cor2, integer *framid, doublereal *pos, 
	doublereal *et, integer *handle, integer *dladsc, doublereal *dskdsc)
{
    return zzdsksel_0_(0, surfid, nsurf, srflst, bodyid, dclass, corsys, 
	    corpar, cor1, cor2, framid, pos, et, handle, dladsc, dskdsc);
    }

logical zzdsksbd_(integer *bodyid)
{
    return zzdsksel_0_(1, (integer *)0, (integer *)0, (integer *)0, bodyid, (
	    integer *)0, (integer *)0, (doublereal *)0, (doublereal *)0, (
	    doublereal *)0, (integer *)0, (doublereal *)0, (doublereal *)0, (
	    integer *)0, (integer *)0, (doublereal *)0);
    }

logical zzdskbdc_(integer *handle, integer *dladsc, doublereal *dskdsc)
{
    return zzdsksel_0_(2, (integer *)0, (integer *)0, (integer *)0, (integer *
	    )0, (integer *)0, (integer *)0, (doublereal *)0, (doublereal *)0, 
	    (doublereal *)0, (integer *)0, (doublereal *)0, (doublereal *)0, 
	    handle, dladsc, dskdsc);
    }

logical zzdsknot_(integer *handle, integer *dladsc, doublereal *dskdsc)
{
    return zzdsksel_0_(3, (integer *)0, (integer *)0, (integer *)0, (integer *
	    )0, (integer *)0, (integer *)0, (doublereal *)0, (doublereal *)0, 
	    (doublereal *)0, (integer *)0, (doublereal *)0, (doublereal *)0, 
	    handle, dladsc, dskdsc);
    }

logical zzdsksit_(integer *bodyid, integer *nsurf, integer *srflst, 
	doublereal *et)
{
    return zzdsksel_0_(4, (integer *)0, nsurf, srflst, bodyid, (integer *)0, (
	    integer *)0, (doublereal *)0, (doublereal *)0, (doublereal *)0, (
	    integer *)0, (doublereal *)0, et, (integer *)0, (integer *)0, (
	    doublereal *)0);
    }

logical zzdskcit_(integer *handle, integer *dladsc, doublereal *dskdsc)
{
    return zzdsksel_0_(5, (integer *)0, (integer *)0, (integer *)0, (integer *
	    )0, (integer *)0, (integer *)0, (doublereal *)0, (doublereal *)0, 
	    (doublereal *)0, (integer *)0, (doublereal *)0, (doublereal *)0, 
	    handle, dladsc, dskdsc);
    }

logical zzdskusc_(integer *bodyid, doublereal *et, doublereal *cor1, 
	doublereal *cor2)
{
    return zzdsksel_0_(6, (integer *)0, (integer *)0, (integer *)0, bodyid, (
	    integer *)0, (integer *)0, (doublereal *)0, cor1, cor2, (integer *
	    )0, (doublereal *)0, et, (integer *)0, (integer *)0, (doublereal *
	    )0);
    }

logical zzdskumc_(integer *handle, integer *dladsc, doublereal *dskdsc)
{
    return zzdsksel_0_(7, (integer *)0, (integer *)0, (integer *)0, (integer *
	    )0, (integer *)0, (integer *)0, (doublereal *)0, (doublereal *)0, 
	    (doublereal *)0, (integer *)0, (doublereal *)0, (doublereal *)0, 
	    handle, dladsc, dskdsc);
    }

logical zzdskmsc_(integer *bodyid, integer *surfid, integer *framid, integer *
	corsys, doublereal *corpar, doublereal *et, doublereal *cor1, 
	doublereal *cor2)
{
    return zzdsksel_0_(8, surfid, (integer *)0, (integer *)0, bodyid, (
	    integer *)0, corsys, corpar, cor1, cor2, framid, (doublereal *)0, 
	    et, (integer *)0, (integer *)0, (doublereal *)0);
    }

logical zzdskmmc_(integer *handle, integer *dladsc, doublereal *dskdsc)
{
    return zzdsksel_0_(9, (integer *)0, (integer *)0, (integer *)0, (integer *
	    )0, (integer *)0, (integer *)0, (doublereal *)0, (doublereal *)0, 
	    (doublereal *)0, (integer *)0, (doublereal *)0, (doublereal *)0, 
	    handle, dladsc, dskdsc);
    }

logical zzdsksrc_(integer *surfid, integer *bodyid, integer *dclass, 
	doublereal *et, integer *framid, doublereal *pos)
{
    return zzdsksel_0_(10, surfid, (integer *)0, (integer *)0, bodyid, dclass,
	     (integer *)0, (doublereal *)0, (doublereal *)0, (doublereal *)0, 
	    framid, pos, et, (integer *)0, (integer *)0, (doublereal *)0);
    }

logical zzdskmrc_(integer *handle, integer *dladsc, doublereal *dskdsc)
{
    return zzdsksel_0_(11, (integer *)0, (integer *)0, (integer *)0, (integer 
	    *)0, (integer *)0, (integer *)0, (doublereal *)0, (doublereal *)0,
	     (doublereal *)0, (integer *)0, (doublereal *)0, (doublereal *)0, 
	    handle, dladsc, dskdsc);
    }

