/* zzswffet.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;
static integer c__15000 = 15000;

/* $Procedure ZZSWFFET ( Private, switch frame kernel pool data fetch ) */
/* Subroutine */ int zzswffet_(integer *framid, integer *hdfram, integer *
	frpool, integer *fidlst, integer *basbeg, integer *free, integer *
	bascnt, logical *usetim, logical *binary, integer *clsses, integer *
	clsids, integer *baslst, doublereal *starts, doublereal *stops, 
	integer *framat)
{
    /* System generated locals */
    integer i__1;

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_rnge(char *, integer, char *, integer);

    /* Local variables */
    char kvid[32];
    integer room;
    extern /* Subroutine */ int zzhsiadd_(integer *, integer *, integer *, 
	    integer *, integer *, logical *), zzdynbid_(char *, integer *, 
	    char *, integer *, ftnlen, ftnlen), zzhsiavl_(integer *, integer *
	    ), zzswfcln_(integer *, integer *, integer *, integer *);
    integer i__, j, n;
    extern /* Subroutine */ int etcal_(doublereal *, char *, ftnlen), chkin_(
	    char *, ftnlen), errch_(char *, char *, ftnlen, ftnlen), repmc_(
	    char *, char *, char *, char *, ftnlen, ftnlen, ftnlen, ftnlen);
    logical kvfnd[8];
    extern /* Subroutine */ int repmi_(char *, char *, integer *, char *, 
	    ftnlen, ftnlen, ftnlen), errdp_(char *, doublereal *, ftnlen);
    integer nstop;
    extern logical failed_(void);
    extern /* Subroutine */ int str2et_(char *, doublereal *, ftnlen);
    char basnam[32];
    integer frclid;
    logical inffnd;
    char frname[32], kvclid[32];
    integer center;
    char bastyp[1], kvalgn[32], kvcent[32], kvclas[32], kvfnam[32], kvnams[32*
	    8];
    integer frcent, frclas, nbases, nfravl;
    extern logical return_(void);
    integer nstart;
    char timstr[80];
    logical havtim;
    extern /* Subroutine */ int gipool_(char *, integer *, integer *, integer 
	    *, integer *, logical *, ftnlen), gcpool_(char *, integer *, 
	    integer *, integer *, char *, logical *, ftnlen, ftnlen);
    char kvstop[32];
    extern /* Subroutine */ int chkout_(char *, ftnlen), setmsg_(char *, 
	    ftnlen), errint_(char *, integer *, ftnlen), sigerr_(char *, 
	    ftnlen);
    char kvstrt[32];
    extern /* Subroutine */ int dtpool_(char *, logical *, integer *, char *, 
	    ftnlen, ftnlen), namfrm_(char *, integer *, ftnlen), frinfo_(
	    integer *, integer *, integer *, integer *, logical *);
    char stptyp[1];
    extern /* Subroutine */ int gdpool_(char *, integer *, integer *, integer 
	    *, doublereal *, logical *, ftnlen);
    char strtyp[1];
    logical stpxst, strxst;
    integer fid;
    logical new__;

/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines. Users should not call this routine directly due */
/*     to the volatile nature of this routine. */

/*     Fetch kernel variables for a specified switch frame. Data */
/*     for the new frame are added to the switch frame database. */

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

/*     FRAMES */
/*     KERNEL */
/*     NAIF_IDS */
/*     TIME */

/* $ Keywords */

/*     Private */

/* $ Declarations */
/* $ Abstract */

/*     Include file zzswtchf.inc */

/*     SPICE private file intended solely for the support of SPICE */
/*     routines. Users should not include this file directly due */
/*     to the volatile nature of this file */

/*     Define SPICE-private parameters related to switch frames. */

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

/* $ Parameters */

/*     The values of the parameters described below are provided for */
/*     informational purposes. The values of these parameters may be */
/*     increased in the future. */


/*     MAXFRM      is the maximum number of switch frame specifications */
/*                 that can be buffered concurrently by the switch frame */
/*                 subsystem. */

/*     MAXBAS      is the maximum number of base frame specifications */
/*                 that can be buffered concurrently by the switch frame */
/*                 subsystem. This limit applies to the total base frame */
/*                 count for all switch frames. */

/*     LBSNGL      is the lower bound of a singly linked list pool */
/*                 managed by the integer hash subsystem. */

/* $ Author_and_Institution */

/*     N.J. Bachman    (JPL) */
/*     B.V. Semenov    (JPL) */

/* $ Literature_References */

/*     None. */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 03-DEC-2021 (NJB) (BVS) */

/* -& */

/*     End of INCLUDE file zzswtchf.inc */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     FRAMID     I   The frame ID of a switch frame. */
/*     HDFRAM    I-O  Array of hash collision list head nodes. */
/*     FRPOOL    I-O  Hash collision list pool. */
/*     FIDLST    I-O  Frame IDs corresponding to list nodes. */
/*     BASBEG    I-O  Start indices of base lists of switch frames. */
/*     FREE      I-O  First free index in base frame arrays. */
/*     BASCNT    I-O  Counts of base frames for each switch frame. */
/*     USETIM    I-O  Logical flags indicating presence of time bounds. */
/*     BINARY    I-O  Logical flags indicating binary search usage. */
/*     CLSSES    I-O  Classes of base frames. */
/*     CLSIDS    I-O  Frame class IDs of base frames. */
/*     BASLST    I-O  Array containing lists of base frame IDs. */
/*     STARTS    I-O  Lists of interval start times. */
/*     STOPS     I-O  Lists of interval stop times. */
/*     FRAMAT     O   Index in FIDLST of input switch frame ID. */

/* $ Detailed_Input */

/*     FRAMID      is the integer ID code for a switch reference frame */
/*                 for which kernel pool data are to be fetched. */

/*     HDFRAM      is an array containing head nodes of hash collision */
/*                 lists in FRPOOL. */

/*     FRPOOL      is a singly linked list pool containing collision */
/*                 lists corresponding to hash values. */

/*     FIDLST      is an array of switch frame ID codes. The Ith element */
/*                 of FRPOOL corresponds to the ID code at index I in */
/*                 FIDLST. */

/*     BASBEG      is an array of start indices of base frame lists and */
/*                 base frame attributes. The Ith element of BASBEG is */
/*                 the start index in BASLST of the base frame list of */
/*                 the switch frame having ID at index I in FIDLST. The */
/*                 attributes of the base frame at a given index in */
/*                 BASLST are stored at the same index in parallel */
/*                 arrays. */

/*     FREE        is the index of the first free element in the array */
/*                 BASLST. This is also the index of the first free */
/*                 elements in the parallel arrays CLSSES, CLSIDS, */
/*                 STARTS and STOPS. */

/*     BASCNT      is an array of counts of base frames associated with */
/*                 switch frames. The Ith element of BASCNT is the base */
/*                 frame count of the switch frame having ID at index I */
/*                 in FIDLST. */

/*     USETIM      is an array of logical flags indicating the existence */
/*                 of time intervals associated with the base frames of */
/*                 a given switch frame. The flag at index I of USETIM */
/*                 is .TRUE. if and only if the switch frame having ID */
/*                 at index I of FIDLST has time intervals associated */
/*                 with its base frames. */

/*     BINARY      is an array of logical flags indicating the */
/*                 eligibility of a given switch frame for use of a */
/*                 binary search to locate the base frame time interval */
/*                 containing a given request time. Eligibility means */
/*                 that time intervals exist and that the stop time of */
/*                 the interval at index J is less than or equal to the */
/*                 start time at index J+1. The flag at index I of */
/*                 BINARY is .TRUE. if and only if the switch frame at */
/*                 index I of FIDLST is eligible for binary search. */

/*     CLSSES      is an array of classes of base frames. The element at */
/*                 index I is the class of the base frame at index I of */
/*                 BASLST, if that element of BASLST is defined. */

/*     CLSIDS      is of frame class IDs of base frames. The element */
/*                 at index I is the frame class ID of the base frame */
/*                 at index I of BASLST, if that element of BASLST is */
/*                 defined. */

/*     BASLST      is an array of ID codes of base frames associated */
/*                 with switch frames. The base frame having ID at index */
/*                 BASBEG(I)-1+J of BASLST is the Jth base frame of the */
/*                 switch frame having frame ID at index I of FIDLST. */

/*     STARTS      is an array of base frame applicability interval */
/*                 start times. The Ith element of STARTS is the start */
/*                 time of the base frame at index I of BASLST, if that */
/*                 element is defined and that frame has associated time */
/*                 intervals. For a given switch frame, all bases have */
/*                 associated time intervals or none do. */

/*     STOPS       is an array of base frame applicability interval */
/*                 start times. The Ith element of STARTS is the start */
/*                 time of the base frame at index I of BASLST, if that */
/*                 element is defined and that frame has associated time */
/*                 intervals. For a given switch frame, all bases have */
/*                 associated time intervals or none do. */

/* $ Detailed_Output */

/*     HDFRAM      is the input collision list head node array, updated */
/*                 if necessary to reflect the addition of a node for */
/*                 the switch frame designated by the input frame ID. */

/*     FRPOOL      is the hash collision list pool, updated to reflect */
/*                 the addition of a node for the switch frame */
/*                 designated by the input frame ID. */

/*     FIDLST      is the switch frame ID list, updated to reflect the */
/*                 addition of the input frame ID. */

/*     BASBEG      is the input base frame head list, updated to reflect */
/*                 the addition of an entry for the switch frame */
/*                 designated by the input frame ID. */

/*     FREE        is the common index in BASLST and its parallel arrays */
/*                 of their first free elements, following addition of */
/*                 data associated with the input frame ID. */

/*     BASCNT      is the input base frame count list, updated to */
/*                 reflect the addition of the base frames associated */
/*                 with the input frame ID. */

/*     USETIM      is the input time interval presence flag array, */
/*                 updated to reflect the addition of the flag for the */
/*                 the switch frame designated by the input frame ID. */

/*     BINARY      is the input binary search eligibility array, updated */
/*                 to reflect the addition of the flag for the switch */
/*                 frame designated by the input frame ID. */

/*     CLSSES      is the input class array, updated to reflect the */
/*                 addition of base frame classes for the switch frame */
/*                 designated by the input frame ID. */

/*     CLSIDS      is the input frame class ID array, updated to reflect */
/*                 the addition of base frame frame class IDs for the */
/*                 switch frame designated by the input frame ID. */

/*     BASLST      is the input base frame array, updated to reflect */
/*                 the addition of base frames associated with the */
/*                 switch frame designated by the input frame ID. */

/*     STARTS      is the input base frame interval start time array, */
/*                 updated to reflect the addition of base frames */
/*                 associated with the switch frame designated by the */
/*                 input frame ID. */

/*     STOPS       is the input base frame interval stop time array, */
/*                 updated to reflect the addition of base frames */
/*                 associated with the switch frame designated by the */
/*                 input frame ID. */

/*     FRAMAT      is the common index in the arrays */

/*                    FIDLST, BASBEG, BASCNT, USETIM, BINARY */

/*                 of information associated with the switch frame */
/*                 designated by the input frame ID. */

/* $ Parameters */

/*     LBSNGL      is the lower bound of a singly linked list pool */
/*                 managed by the integer hash subsystem. */

/*     See the include file zzswtchf.inc for SPICE-private parameters */
/*     defining sizes of buffers used by the switch frame subsystem. */

/* $ Exceptions */

/*     1)  If the input frame ID maps to a frame specification for which */
/*         the ID doesn't match, the error SPICE(BADFRAMESPEC) is */
/*         signaled. */

/*     2)  If kernel variables for any of the universal frame */
/*         specification items */

/*            frame ID */
/*            frame name */
/*            frame center */
/*            frame class */
/*            frame class ID */

/*         or the switch frame item */

/*            base frame list */

/*        are not found, the error SPICE(MISSINGFRAMEVAR) is signaled. */

/*     3) If the ID of a base frame cannot be mapped to a name, */
/*        the error SPICE(FRAMENAMENOTFOUND) is signaled. */

/*     4) If the stop time for a given interval is not strictly greater */
/*        than the corresponding start time, the error */
/*        SPICE(BADTIMEBOUNDS) is signaled. */

/*     5) If only one of the start time or stop time kernel variables */
/*        is supplied, the error SPICE(PARTIALFRAMESPEC) is signaled. */

/*     6) If the count of start times doesn't match the count of */
/*        stop times, or if either count doesn't match the count of */
/*        base frames, the error SPICE(COUNTMISMATCH) is signaled. */

/*     7) If an error occurs during kernel variable lookup, the error */
/*        is signaled by a routine in the call tree of this routine. */

/*     8) If an error occurs during time conversion, the error */
/*        is signaled by a routine in the call tree of this routine. */

/* $ Files */

/*     Appropriate kernels must be loaded by the calling program before */
/*     this routine is called. */

/*     The following data are required: */

/*         - A switch frame specification, normally provided by a */
/*           frame kernel. */

/*              > Base frames may be specified by name or frame ID. */

/*              > Time interval bounds may be specified by singly */
/*                quoted strings or by data using the text kernel "@" */
/*                syntax. */

/*         - Frame specifications for base frames. Normally these */
/*           specifications are provided by frame kernels. */

/*           Even though specifications of frames identified by ID code */
/*           are not explicitly required by this routine, they are */
/*           required by the intended caller of this routine ZZSWFXFM. */

/*         - A leapseconds kernel, if time interval bounds are */
/*           specified by time strings. */

/*     In all cases, kernel data are normally loaded once per program */
/*     run, NOT every time this routine is called. */

/* $ Particulars */

/*     This routine obtains switch frame specifications from the kernel */
/*     pool and adds them to the switch frame database. */

/*     If, upon entry, the database already contains the maximum number */
/*     of switch frames, the database will be cleared in order to make */
/*     room for the new switch frame specification. */

/*     If, upon entry, the database doesn't have enough room to buffer */
/*     the base frame data associated with a new switch frame, the */
/*     database will be cleared in order to make room for the new switch */
/*     frame specification. */

/*     Complete error checking of the new switch frame specification is */
/*     performed on each call. */

/* $ Examples */

/*     None. See usage in ZZSWFXFM. */

/* $ Restrictions */

/*     1)  This routine is SPICE-private. User applications must not */
/*         call it. */

/*     2)  To improve efficiency, this routine buffers switch frame */
/*         specifications. The buffers are emptied after any update */
/*         to the kernel pool. Since the process of parsing, and */
/*         buffering parameters provided by, switch frame specifications */
/*         is rather slow, applications using switch frames should */
/*         avoid high-frequency kernel pool updates. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman   (JPL) */
/*     B.V. Semenov   (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 15-DEC-2021 (NJB) (BVS) */

/* -& */
/* $ Index_Entries */

/*     fetch switch frame data from kernel pool */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     Kernel variable name templates: */


/*     Other parameters */


/*     Indices of items in the arrays of kernel variables: */


/*     Local variables */


/*     KVFND(*) indicates whether a specified kernel variable was found. */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }
    chkin_("ZZSWFFET", (ftnlen)8);

/*     No result found yet. */

    *framat = 0;

/*     Create names of kernel variables, using the input frame ID. */

/*     Basic frame specification variables, excluding that for the */
/*     frame ID. We'll need the frame name before we can create the */
/*     name of the kernel variable for the frame ID. */

    repmi_("FRAME_#_CENTER", "#", framid, kvcent, (ftnlen)14, (ftnlen)1, (
	    ftnlen)32);
    repmi_("FRAME_#_CLASS", "#", framid, kvclas, (ftnlen)13, (ftnlen)1, (
	    ftnlen)32);
    repmi_("FRAME_#_CLASS_ID", "#", framid, kvclid, (ftnlen)16, (ftnlen)1, (
	    ftnlen)32);
    repmi_("FRAME_#_NAME", "#", framid, kvfnam, (ftnlen)12, (ftnlen)1, (
	    ftnlen)32);

/*     Switch frame specification variables: */

    repmi_("FRAME_#_ALIGNED_WITH", "#", framid, kvalgn, (ftnlen)20, (ftnlen)1,
	     (ftnlen)32);
    repmi_("FRAME_#_START", "#", framid, kvstrt, (ftnlen)13, (ftnlen)1, (
	    ftnlen)32);
    repmi_("FRAME_#_STOP", "#", framid, kvstop, (ftnlen)12, (ftnlen)1, (
	    ftnlen)32);

/*     Below, we use logical flags to describe the status */
/*     of kernel variable availability: */

/*         KVFND(*) indicates that a variable exists. */

/*     Look up the basic variables. */

    gipool_(kvclas, &c__1, &c__1, &n, &frclas, &kvfnd[3], (ftnlen)32);
    gipool_(kvclid, &c__1, &c__1, &n, &frclid, &kvfnd[4], (ftnlen)32);
    gcpool_(kvfnam, &c__1, &c__1, &n, frname, kvfnd, (ftnlen)32, (ftnlen)32);
    if (failed_()) {
	zzswfcln_(hdfram, frpool, basbeg, framat);
	chkout_("ZZSWFFET", (ftnlen)8);
	return 0;
    }

/*     Fetch the switch frame ID variable. */

    if (kvfnd[0]) {

/*        Use the frame name to fetch the switch frame ID variable. */

	repmc_("FRAME_#", "#", frname, kvid, (ftnlen)7, (ftnlen)1, (ftnlen)32,
		 (ftnlen)32);
	gipool_(kvid, &c__1, &c__1, &n, &fid, &kvfnd[1], (ftnlen)32);
	if (failed_()) {
	    zzswfcln_(hdfram, frpool, basbeg, framat);
	    chkout_("ZZSWFFET", (ftnlen)8);
	    return 0;
	}

/*        The frame ID of the frame specification had better match */
/*        the input frame ID. */

	if (kvfnd[1] && fid != *framid) {
	    zzswfcln_(hdfram, frpool, basbeg, framat);
	    setmsg_("Input frame ID was #, but ID in frame specification fro"
		    "m kernel pool was #. ", (ftnlen)76);
	    errint_("#", framid, (ftnlen)1);
	    errint_("#", &fid, (ftnlen)1);
	    sigerr_("SPICE(BADFRAMESPEC)", (ftnlen)19);
	    chkout_("ZZSWFFET", (ftnlen)8);
	    return 0;
	}

/*        The frame name must always be found. If not, the error */
/*        will be diagnosed in the block below. */

    } else {

/*        We couldn't find the frame name. Indicate the frame ID */
/*        variable [sic] wasn't found, since its found flag won't be set */
/*        by GIPOOL. */

	kvfnd[1] = FALSE_;
    }

/*     Look up the central body of the frame. The name of the kernel */
/*     variable for the body could refer to the frame by name or frame */
/*     ID; the body itself could be specified by name or body ID. */

    if (kvfnd[0]) {
	zzdynbid_(frname, framid, "CENTER", &frcent, (ftnlen)32, (ftnlen)6);
	if (failed_()) {
	    zzswfcln_(hdfram, frpool, basbeg, framat);
	    chkout_("ZZSWFFET", (ftnlen)8);
	    return 0;
	}
	kvfnd[2] = TRUE_;
    } else {
	kvfnd[2] = FALSE_;
    }

/*     Look up the type and count of the base frame list. It */
/*     may have either string or numeric type. */

    dtpool_(kvalgn, &kvfnd[5], &nbases, bastyp, (ftnlen)32, (ftnlen)1);
    if (failed_()) {

/*        This code should be unreachable but is provided for safety. */

	zzswfcln_(hdfram, frpool, basbeg, framat);
	chkout_("ZZSWFFET", (ftnlen)8);
	return 0;
    }

/*     Store the kernel variable names in order to prepare for */
/*     checking availability of required variables. */

    s_copy(kvnams + 32, kvid, (ftnlen)32, (ftnlen)32);
    s_copy(kvnams, kvfnam, (ftnlen)32, (ftnlen)32);
    s_copy(kvnams + 64, kvcent, (ftnlen)32, (ftnlen)32);
    s_copy(kvnams + 96, kvclas, (ftnlen)32, (ftnlen)32);
    s_copy(kvnams + 128, kvclid, (ftnlen)32, (ftnlen)32);
    s_copy(kvnams + 160, kvalgn, (ftnlen)32, (ftnlen)32);

/*     Check for required variables that haven't been supplied. */

    for (i__ = 1; i__ <= 6; ++i__) {

/*        The first 6 items are needed; start and stop times are needed */
/*        if at least one is present. */

	if (! kvfnd[(i__1 = i__ - 1) < 8 && 0 <= i__1 ? i__1 : s_rnge("kvfnd",
		 i__1, "zzswffet_", (ftnlen)654)]) {
	    zzswfcln_(hdfram, frpool, basbeg, framat);
	    setmsg_("Kernel variable #, needed for specification of switch f"
		    "rame having frame ID #, was not found in the kernel pool"
		    ". This can occur when a frame kernel providing the requi"
		    "red switch frame specification has not been loaded, or i"
		    "f the specification is present but is incorrect.", (
		    ftnlen)271);
	    errch_("#", kvnams + (((i__1 = i__ - 1) < 8 && 0 <= i__1 ? i__1 : 
		    s_rnge("kvnams", i__1, "zzswffet_", (ftnlen)666)) << 5), (
		    ftnlen)1, (ftnlen)32);
	    errint_("#", framid, (ftnlen)1);
	    sigerr_("SPICE(MISSINGFRAMEVAR)", (ftnlen)22);
	    chkout_("ZZSWFFET", (ftnlen)8);
	    return 0;
	}
    }

/*     Find out whether time bounds are supplied, and if so, which data */
/*     type is used to represent them. */

    s_copy(kvnams + 192, kvstrt, (ftnlen)32, (ftnlen)32);
    s_copy(kvnams + 224, kvstop, (ftnlen)32, (ftnlen)32);
    dtpool_(kvstrt, &kvfnd[6], &nstart, strtyp, (ftnlen)32, (ftnlen)1);
    dtpool_(kvstop, &kvfnd[7], &nstop, stptyp, (ftnlen)32, (ftnlen)1);
    if (failed_()) {

/*        This code should be unreachable but is provided for safety. */

	zzswfcln_(hdfram, frpool, basbeg, framat);
	chkout_("ZZSWFFET", (ftnlen)8);
	return 0;
    }

/*     Availability of starts and stops is optional...however both */
/*     must be provided if either is, and the counts must match */
/*     those of the base frames. */

/*     Note that it's necessary to perform these checks before buffering */
/*     base frame data, since that process assumes that the number of */
/*     start and stop times is either zero or matches the number of */
/*     base frames. */

    strxst = kvfnd[6];
    stpxst = kvfnd[7];
    havtim = strxst && stpxst;
    if (havtim) {

/*        Make sure that counts of base frames, start times, and */
/*        stop times are equal. */

	if (nstart != nstop || nstart != nbases) {
	    zzswfcln_(hdfram, frpool, basbeg, framat);
	    setmsg_("Kernel variables for the switch frame having frame ID #"
		    " have mismatched sizes: number of base frames = #; numbe"
		    "r of start times = #; number of stop times = #.", (ftnlen)
		    158);
	    errint_("#", framid, (ftnlen)1);
	    errint_("#", &nbases, (ftnlen)1);
	    errint_("#", &nstart, (ftnlen)1);
	    errint_("#", &nstop, (ftnlen)1);
	    sigerr_("SPICE(COUNTMISMATCH)", (ftnlen)20);
	    chkout_("ZZSWFFET", (ftnlen)8);
	    return 0;
	}
    } else {

/*        Check for inconsistent presence of start and stop times. */

	if (strxst || stpxst) {
	    zzswfcln_(hdfram, frpool, basbeg, framat);

/*           Create long message template to be filled in. */

	    setmsg_("Kernel variable #, which specifies base frame applicabi"
		    "lity # times, was not provided for the switch frame havi"
		    "ng frame ID #, while the kernel variable # specifying ba"
		    "se frame applicability # times was provided. Switch fram"
		    "e applicability start and stop times are optional, but b"
		    "oth must be provided if either is.", (ftnlen)313);
	    if (strxst) {

/*              Stop times are missing. */

		errch_("#", kvstop, (ftnlen)1, (ftnlen)32);
		errch_("#", "stop", (ftnlen)1, (ftnlen)4);
		errint_("#", framid, (ftnlen)1);
		errch_("#", kvstrt, (ftnlen)1, (ftnlen)32);
		errch_("#", "start", (ftnlen)1, (ftnlen)5);
	    } else {

/*              Start times are missing. */

		errch_("#", kvstrt, (ftnlen)1, (ftnlen)32);
		errch_("#", "start", (ftnlen)1, (ftnlen)5);
		errint_("#", framid, (ftnlen)1);
		errch_("#", kvstop, (ftnlen)1, (ftnlen)32);
		errch_("#", "stop", (ftnlen)1, (ftnlen)4);
	    }
	    sigerr_("SPICE(PARTIALFRAMESPEC)", (ftnlen)23);
	    chkout_("ZZSWFFET", (ftnlen)8);
	    return 0;
	}
    }

/*     Compute room for the output data. */

    room = 15001 - *free;

/*     Check the available room in the frame ID pool and in the frame */
/*     base arrays. */

    zzhsiavl_(frpool, &nfravl);
    if (nfravl == 0 || room < nbases) {

/*        There's no room for another frame in the frame pool, or */
/*        there's no room for the new frame's data in the base */
/*        frame arrays. */

/*        If we can make enough room by re-initializing the whole */
/*        local frame database, do so. If the new frame has too */
/*        much data to fit in the empty base frame arrays, that's */
/*        an error. */

	if (nbases <= 15000) {

/*           We can fit the frame data in by clearing out the */
/*           database. */

	    zzswfcln_(hdfram, frpool, basbeg, framat);

/*           The database is now initialized. */

	    *free = 1;
	    room = 15000;
	} else {

/*           We can't make enough room. */

/*           Initialize local data structures, for safety. */

	    zzswfcln_(hdfram, frpool, basbeg, framat);
	    setmsg_("The requested frame # has # associated base frames. The"
		    " maximum number that can be supported is #.", (ftnlen)98);
	    errint_("#", framid, (ftnlen)1);
	    errint_("#", &nbases, (ftnlen)1);
	    errint_("#", &c__15000, (ftnlen)1);
	    sigerr_("SPICE(TOOMANYBASEFRAMES)", (ftnlen)24);
	    chkout_("ZZSWFFET", (ftnlen)8);
	    return 0;
	}
    }

/*     Add the frame and its data to the local database. */

/*     Start by adding the frame ID to the frame hash structure. */
/*     The output argument NEW indicates whether the item is new; */
/*     we don't need to check this argument. We also don't need to */
/*     check for failure of ZZHSIADD, since we've already ensured */
/*     there's room in the hash for a new clock. */

    zzhsiadd_(hdfram, frpool, fidlst, framid, framat, &new__);

/*     At this point, FRAMAT is set. */

/*     Store the frame ID, base start index, base count, and time */
/*     interval availability attributes for this frame. */

    fidlst[*framat - 1] = *framid;
    basbeg[*framat - 1] = *free;
    bascnt[*framat - 1] = nbases;
    usetim[*framat - 1] = havtim;

/*     Look up the base frame variables. The base frame list may have */
/*     either string or numeric type. */

    kvfnd[5] = FALSE_;
    if (*(unsigned char *)bastyp == 'N') {

/*        The frames are specified by ID code. We can buffer the frame */
/*        ID codes immediately. */

	gipool_(kvalgn, &c__1, &room, &bascnt[*free - 1], &baslst[*free - 1], 
		&kvfnd[5], (ftnlen)32);
	if (failed_()) {
	    zzswfcln_(hdfram, frpool, basbeg, framat);
	    chkout_("ZZSWFFET", (ftnlen)8);
	    return 0;
	}
    } else if (*(unsigned char *)bastyp == 'C') {

/*        The frames are specified by name. We must convert the */
/*        names to frame ID codes before we can can buffer the IDs. */

	kvfnd[5] = FALSE_;
	i__1 = nbases;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    gcpool_(kvalgn, &i__, &c__1, &n, basnam, &kvfnd[5], (ftnlen)32, (
		    ftnlen)32);
	    if (failed_()) {

/*              This code should be unreachable but is provided for */
/*              safety. */

		zzswfcln_(hdfram, frpool, basbeg, framat);
		chkout_("ZZSWFFET", (ftnlen)8);
		return 0;
	    }
	    namfrm_(basnam, &baslst[*free - 1 + i__ - 1], (ftnlen)32);
	    if (baslst[*free - 1 + i__ - 1] == 0) {
		zzswfcln_(hdfram, frpool, basbeg, framat);
		setmsg_("Base frame name # of switch frame # could not be tr"
			"anslated to a frame ID code ", (ftnlen)79);
		errch_("#", basnam, (ftnlen)1, (ftnlen)32);
		errint_("#", framid, (ftnlen)1);
		sigerr_("SPICE(FRAMENAMENOTFOUND)", (ftnlen)24);
		chkout_("ZZSWFFET", (ftnlen)8);
		return 0;
	    }
	}
    } else {

/*        Backstop: this code should be unreachable. */

	zzswfcln_(hdfram, frpool, basbeg, framat);
	setmsg_("Base frame kernel variable # exists but DTPOOL returned dat"
		"a type # rather than one of the expected values: 'C' or 'N'.",
		 (ftnlen)119);
	errch_("#", kvalgn, (ftnlen)1, (ftnlen)32);
	errch_("#", bastyp, (ftnlen)1, (ftnlen)1);
	sigerr_("SPICE(BUG)", (ftnlen)10);
	chkout_("ZZSWFFET", (ftnlen)8);
	return 0;
    }

/*     Get attributes of each base frame. */

    i__1 = nbases;
    for (i__ = 1; i__ <= i__1; ++i__) {

/*        Store the frame class and frame class ID for this base frame. */

	j = *free - 1 + i__;
	frinfo_(&baslst[j - 1], &center, &clsses[j - 1], &clsids[j - 1], &
		inffnd);
	if (failed_()) {
	    zzswfcln_(hdfram, frpool, basbeg, framat);
	    chkout_("ZZSWFFET", (ftnlen)8);
	    return 0;
	}
	if (! inffnd) {
	    zzswfcln_(hdfram, frpool, basbeg, framat);
	    setmsg_("No specification was found for base frame # of switch f"
		    "rame #.", (ftnlen)62);
	    errint_("#", &baslst[j - 1], (ftnlen)1);
	    errint_("#", framid, (ftnlen)1);
	    sigerr_("SPICE(FRAMEINFONOTFOUND)", (ftnlen)24);
	    chkout_("ZZSWFFET", (ftnlen)8);
	    return 0;
	}
    }

/*     Fetch interval bounds if they're present. */

    if (usetim[*framat - 1]) {

/*        Fetch start times. */

/*        Note that DTPOOL sets the type to 'X' if the target */
/*        kernel variable is not found. */

	if (*(unsigned char *)strtyp == 'N') {

/*           The start times are represented by double precision */
/*           numbers. */

	    gdpool_(kvstrt, &c__1, &room, &nstart, &starts[*free - 1], &kvfnd[
		    6], (ftnlen)32);
	} else if (*(unsigned char *)strtyp == 'C') {

/*           The start times are represented by strings. */

/*           We must convert each string to a numeric value and store */
/*           the value in the start time buffer. */

	    kvfnd[6] = FALSE_;
	    i__1 = nbases;
	    for (i__ = 1; i__ <= i__1; ++i__) {
		gcpool_(kvstrt, &i__, &c__1, &n, timstr, &kvfnd[6], (ftnlen)
			32, (ftnlen)80);
		str2et_(timstr, &starts[*free - 1 + i__ - 1], (ftnlen)80);
	    }
	} else {

/*           Backstop: this code should be unreachable. */

	    zzswfcln_(hdfram, frpool, basbeg, framat);
	    setmsg_("Start time kernel variable # exists but DTPOOL returned"
		    " data type # rather than one of the expected values: 'C'"
		    " or 'N'.", (ftnlen)119);
	    errch_("#", kvstrt, (ftnlen)1, (ftnlen)32);
	    errch_("#", strtyp, (ftnlen)1, (ftnlen)1);
	    sigerr_("SPICE(BUG)", (ftnlen)10);
	    chkout_("ZZSWFFET", (ftnlen)8);
	    return 0;
	}
	if (failed_()) {
	    zzswfcln_(hdfram, frpool, basbeg, framat);
	    chkout_("ZZSWFFET", (ftnlen)8);
	    return 0;
	}

/*        Fetch stop times. */

	if (*(unsigned char *)stptyp == 'N') {

/*           The stop times are represented by double precision numbers. */

	    gdpool_(kvstop, &c__1, &room, &nstop, &stops[*free - 1], &kvfnd[7]
		    , (ftnlen)32);
	} else if (*(unsigned char *)stptyp == 'C') {

/*           The stop times are represented by strings. */

/*           We must convert each string to a numeric value and store */
/*           the value in the stop time buffer. */

	    kvfnd[7] = FALSE_;
	    i__1 = nstop;
	    for (i__ = 1; i__ <= i__1; ++i__) {
		gcpool_(kvstop, &i__, &c__1, &n, timstr, &kvfnd[7], (ftnlen)
			32, (ftnlen)80);
		str2et_(timstr, &stops[*free - 1 + i__ - 1], (ftnlen)80);
	    }
	} else {

/*           Backstop: this code should be unreachable. */

	    zzswfcln_(hdfram, frpool, basbeg, framat);
	    setmsg_("Stop time kernel variable # exists but DTPOOL returned "
		    "data type # rather than one of the expected values: 'C' "
		    "or 'N'.", (ftnlen)118);
	    errch_("#", kvstop, (ftnlen)1, (ftnlen)32);
	    errch_("#", stptyp, (ftnlen)1, (ftnlen)1);
	    sigerr_("SPICE(BUG)", (ftnlen)10);
	    chkout_("ZZSWFFET", (ftnlen)8);
	    return 0;
	}
	if (failed_()) {
	    zzswfcln_(hdfram, frpool, basbeg, framat);
	    chkout_("ZZSWFFET", (ftnlen)8);
	    return 0;
	}

/*        Singleton intervals and out-of-order intervals are not */
/*        allowed. */

	i__1 = nstart;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    j = *free - 1 + i__;
	    if (starts[j - 1] >= stops[j - 1]) {
		zzswfcln_(hdfram, frpool, basbeg, framat);
		setmsg_("Interval time bounds are not strictly increasing at"
			" interval index # for switch frame #. Time bounds ar"
			"e #:# TDB (# TDB : # TDB)", (ftnlen)128);
		errint_("#", &i__, (ftnlen)1);
		errint_("#", framid, (ftnlen)1);
		errdp_("#", &starts[j - 1], (ftnlen)1);
		errdp_("#", &stops[j - 1], (ftnlen)1);
		etcal_(&starts[j - 1], timstr, (ftnlen)80);
		errch_("#", timstr, (ftnlen)1, (ftnlen)80);
		etcal_(&stops[j - 1], timstr, (ftnlen)80);
		errch_("#", timstr, (ftnlen)1, (ftnlen)80);
		sigerr_("SPICE(BADTIMEBOUNDS)", (ftnlen)20);
		chkout_("ZZSWFFET", (ftnlen)8);
		return 0;
	    }
	}
    }

/*     Determine whether binary search on the time intervals is */
/*     possible. */

    if (usetim[*framat - 1]) {
	i__ = 2;
	binary[*framat - 1] = TRUE_;
	while(i__ <= nbases && binary[*framat - 1]) {

/*           Note that proper ordering of start and stop times for */
/*           each interval has already been verified. */

	    j = basbeg[*framat - 1] - 1 + i__;
	    if (stops[j - 2] > starts[j - 1]) {
		binary[*framat - 1] = FALSE_;
	    }
	    ++i__;
	}
    } else {
	binary[*framat - 1] = FALSE_;
    }

/*     We've found the data. The switch frame database has been */
/*     updated. FRAMAT is already set. */

/*     Account for the base frame storage used. */

    *free += nbases;
    chkout_("ZZSWFFET", (ftnlen)8);
    return 0;
} /* zzswffet_ */

