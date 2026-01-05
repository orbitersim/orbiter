/* kplfrm.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__0 = 0;
static integer c__1 = 1;
static integer c__100 = 100;

/* $Procedure KPLFRM ( Kernel pool frame IDs ) */
/* Subroutine */ int kplfrm_(integer *frmcls, integer *idset)
{
    /* System generated locals */
    integer i__1, i__2;

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_rnge(char *, integer, char *, integer);

    /* Local variables */
    integer i__, l, m, n, w;
    extern /* Subroutine */ int chkin_(char *, ftnlen), repmc_(char *, char *,
	     char *, char *, ftnlen, ftnlen, ftnlen, ftnlen);
    logical found;
    extern /* Subroutine */ int repmi_(char *, char *, integer *, char *, 
	    ftnlen, ftnlen, ftnlen);
    extern integer sizei_(integer *);
    integer total, idcode, to;
    extern /* Subroutine */ int scardi_(integer *, integer *);
    char frname[32];
    extern /* Subroutine */ int validi_(integer *, integer *, integer *);
    char kvcode[32];
    integer fclass;
    char kvname[32], kvbuff[32*100], kvclas[32];
    extern /* Subroutine */ int gcpool_(char *, integer *, integer *, integer 
	    *, char *, logical *, ftnlen, ftnlen), gipool_(char *, integer *, 
	    integer *, integer *, integer *, logical *, ftnlen);
    char tmpnam[32];
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), setmsg_(char *, ftnlen), errint_(char *, integer *, 
	    ftnlen);
    char kvtemp[32];
    extern /* Subroutine */ int gnpool_(char *, integer *, integer *, integer 
	    *, char *, logical *, ftnlen, ftnlen);
    extern logical return_(void);

/* $ Abstract */

/*     Return a SPICE set containing the frame IDs of all reference */
/*     frames of a given class having specifications in the kernel pool. */

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
/*     FRAMES */
/*     KERNEL */
/*     NAIF_IDS */
/*     SETS */

/* $ Keywords */

/*     FRAME */
/*     SET */
/*     UTILITY */

/* $ Declarations */
/* $ Abstract */

/*     The parameters below form an enumerated list of the recognized */
/*     frame types. They are: INERTL, PCK, CK, TK, DYN, SWTCH, and ALL. */
/*     The meanings are outlined below. */

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

/*     INERTL      an inertial frame that is listed in the routine */
/*                 CHGIRF and that requires no external file to */
/*                 compute the transformation from or to any other */
/*                 inertial frame. */

/*     PCK         is a frame that is specified relative to some */
/*                 INERTL frame and that has an IAU model that */
/*                 may be retrieved from the PCK system via a call */
/*                 to the routine TISBOD. */

/*     CK          is a frame defined by a C-kernel. */

/*     TK          is a "text kernel" frame.  These frames are offset */
/*                 from their associated "relative" frames by a */
/*                 constant rotation. */

/*     DYN         is a "dynamic" frame.  These currently are */
/*                 parameterized, built-in frames where the full frame */
/*                 definition depends on parameters supplied via a */
/*                 frame kernel. */

/*     SWTCH       is a "switch" frame. These frames have orientation */
/*                 defined by their alignment with base frames selected */
/*                 from a prioritized list. The base frames optionally */
/*                 have associated time intervals of applicability. */

/*     ALL         indicates any of the above classes. This parameter */
/*                 is used in APIs that fetch information about frames */
/*                 of a specified class. */


/* $ Author_and_Institution */

/*     N.J. Bachman    (JPL) */
/*     B.V. Semenov    (JPL) */
/*     W.L. Taber      (JPL) */

/* $ Literature_References */

/*     None. */

/* $ Version */

/* -    SPICELIB Version 5.0.0, 08-OCT-2020 (NJB) (BVS) */

/*       The parameter SWTCH was added to support the switch */
/*       frame class. */

/* -    SPICELIB Version 4.0.0, 08-MAY-2012 (NJB) */

/*       The parameter ALL was added to support frame fetch APIs. */

/* -    SPICELIB Version 3.0.0, 28-MAY-2004 (NJB) */

/*       The parameter DYN was added to support the dynamic frame class. */

/* -    SPICELIB Version 2.0.0, 12-DEC-1996 (WLT) */

/*        Various unused frames types were removed and the */
/*        frame time TK was added. */

/* -    SPICELIB Version 1.0.0, 10-DEC-1995 (WLT) */

/* -& */

/*     End of INCLUDE file frmtyp.inc */

/* $ Abstract */

/*     This file contains the number of inertial reference */
/*     frames that are currently known by the SPICE toolkit */
/*     software. */

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

/*     FRAMES */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     NINERT     P   Number of known inertial reference frames. */

/* $ Parameters */

/*     NINERT     is the number of recognized inertial reference */
/*                frames.  This value is needed by both CHGIRF */
/*                ZZFDAT, and FRAMEX. */

/* $ Author_and_Institution */

/*     W.L. Taber      (JPL) */

/* $ Literature_References */

/*     None. */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 10-OCT-1996 (WLT) */

/* -& */
/* $ Abstract */

/*     This file contains the number of non-inertial reference */
/*     frames that are currently built into the SPICE toolkit */
/*     software. */


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

/*     FRAMES */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     NINERT     P   Number of built-in non-inertial reference frames. */

/* $ Parameters */

/*     NINERT     is the number of built-in non-inertial reference */
/*                frames.  This value is needed by both  ZZFDAT, and */
/*                FRAMEX. */

/* $ Author_and_Institution */

/*     B.V. Semenov    (JPL) */
/*     W.L. Taber      (JPL) */
/*     F.S. Turner     (JPL) */

/* $ Literature_References */

/*     None. */

/* $ Version */

/* -    SPICELIB Version 1.7.0, 26-AUG-2021 (BVS) */

/*        Increased the number of non-inertial frames from 106 to 124 */
/*        in order to accommodate the following PCK based frames: */

/*           IAU_52_EUROPA */
/*           IAU_NIX */
/*           IAU_HYDRA */
/*           IAU_RYUGU */
/*           IAU_ARROKOTH */
/*           IAU_DIDYMOS_BARYCENTER */
/*           IAU_DIDYMOS */
/*           IAU_DIMORPHOS */
/*           IAU_DONALDJOHANSON */
/*           IAU_EURYBATES */
/*           IAU_EURYBATES_BARYCENTER */
/*           IAU_QUETA */
/*           IAU_POLYMELE */
/*           IAU_LEUCUS */
/*           IAU_ORUS */
/*           IAU_PATROCLUS_BARYCENTER */
/*           IAU_PATROCLUS */
/*           IAU_MENOETIUS */

/* -    SPICELIB Version 1.6.0, 30-OCT-2014 (BVS) */

/*        Increased the number of non-inertial frames from 105 to 106 */
/*        in order to accommodate the following PCK based frame: */

/*           IAU_BENNU */

/* -    SPICELIB Version 1.5.0, 11-OCT-2011 (BVS) */

/*        Increased the number of non-inertial frames from 100 to 105 */
/*        in order to accommodate the following PCK based frames: */

/*           IAU_CERES */
/*           IAU_PALLAS */
/*           IAU_LUTETIA */
/*           IAU_DAVIDA */
/*           IAU_STEINS */

/* -    SPICELIB Version 1.4.0, 11-MAY-2010 (BVS) */

/*        Increased the number of non-inertial frames from 96 to 100 */
/*        in order to accommodate the following PCK based frames: */

/*           IAU_BORRELLY */
/*           IAU_TEMPEL_1 */
/*           IAU_VESTA */
/*           IAU_ITOKAWA */

/* -    SPICELIB Version 1.3.0, 12-DEC-2002 (BVS) */

/*        Increased the number of non-inertial frames from 85 to 96 */
/*        in order to accommodate the following PCK based frames: */

/*           IAU_CALLIRRHOE */
/*           IAU_THEMISTO */
/*           IAU_MAGACLITE */
/*           IAU_TAYGETE */
/*           IAU_CHALDENE */
/*           IAU_HARPALYKE */
/*           IAU_KALYKE */
/*           IAU_IOCASTE */
/*           IAU_ERINOME */
/*           IAU_ISONOE */
/*           IAU_PRAXIDIKE */

/* -    SPICELIB Version 1.2.0, 02-AUG-2002 (FST) */

/*        Increased the number of non-inertial frames from 81 to 85 */
/*        in order to accommodate the following PCK based frames: */

/*           IAU_PAN */
/*           IAU_GASPRA */
/*           IAU_IDA */
/*           IAU_EROS */

/* -    SPICELIB Version 1.1.0, 20-FEB-1997 (WLT) */

/*        Increased the number of non-inertial frames from 79 to 81 */
/*        in order to accommodate the following earth rotation */
/*        models: */

/*           ITRF93 */
/*           EARTH_FIXED */

/* -    SPICELIB Version 1.0.0, 10-OCT-1996 (WLT) */

/* -& */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     FRMCLS     I   Frame class. */
/*     IDSET      O   Set of ID codes of frames of the specified class. */

/* $ Detailed_Input */

/*     FRMCLS   is an integer code specifying the frame class or */
/*              classes for which frame ID codes are requested. */
/*              The applicable reference frames are those having */
/*              specifications present in the kernel pool. */

/*              FRMCLS may designate a single class or "all */
/*              classes." */

/*              The include file frmtyp.inc declares parameters */
/*              identifying frame classes. The supported values */
/*              and corresponding meanings of FRMCLS are */

/*                 Parameter      Value    Meaning */
/*                 =========      =====    ================= */
/*                 ALL              -1     All frame classes */
/*                                         specified in the */
/*                                         kernel pool. Class 1 */
/*                                         is not included. */

/*                 INERTL            1     Built-in inertial. */
/*                                         No frames will be */
/*                                         returned in the */
/*                                         output set. */

/*                 PCK               2     PCK-based frame */

/*                 CK                3     CK-based frame */

/*                 TK                4     Fixed rotational */
/*                                         offset ("text */
/*                                         kernel") frame */

/*                 DYN               5     Dynamic frame */

/*                 SWTCH             6     Switch frame */

/* $ Detailed_Output */

/*     IDSET    is a SPICE set containing the ID codes of all */
/*              reference frames having specifications present in */
/*              the kernel pool and belonging to the specified */
/*              class or classes. */

/*              Note that if FRMCLS is set to INERTL, IDSET */
/*              will be empty on output. */

/* $ Parameters */

/*     See the INCLUDE file frmtyp.inc. */

/* $ Exceptions */

/*     1)  If the input frame class argument is not defined in */
/*         frmtyp.inc, the error SPICE(BADFRAMECLASS) is signaled. */

/*     2)  If the size of IDSET is too small to hold the requested frame */
/*         ID set, the error SPICE(SETTOOSMALL) is signaled. */

/*     3)  Frames of class 1 may not be specified in the kernel pool. */
/*         However, for the convenience of users, this routine does not */
/*         signal an error if the input class is set to INERTL. In this */
/*         case the output set will be empty. */

/*     4)  This routine relies on the presence of just three kernel */
/*         variable assignments for a reference frame in order to */
/*         determine that that reference frame has been specified: */

/*            FRAME_<frame name>       = <ID code> */
/*            FRAME_<ID code>_NAME     = <frame name> */

/*         and either */

/*            FRAME_<ID code>_CLASS    = <class> */

/*         or */

/*            FRAME_<frame name>_CLASS = <class> */

/*         It is possible for the presence of an incomplete frame */
/*         specification to trick this routine into incorrectly */
/*         deciding that a frame has been specified. This routine */
/*         does not attempt to diagnose this problem. */

/* $ Files */

/*     Reference frame specifications for frames that are not built in */
/*     are typically established by loading frame kernels. */

/* $ Particulars */

/*     This routine enables SPICE-based applications to conveniently */
/*     find the frame ID codes of reference frames having specifications */
/*     present in the kernel pool. Such frame specifications are */
/*     introduced into the kernel pool either by loading frame kernels */
/*     or by means of calls to the kernel pool "put" API routines */

/*        PCPOOL */
/*        PDPOOL */
/*        PIPOOL */

/*     Given a reference frame's ID code, other attributes of the */
/*     frame can be obtained via calls to the SPICELIB routines */

/*        FRMNAM {Return a frame's name} */
/*        FRINFO {Return a frame's center, class, and class ID} */

/*     This routine has a counterpart */

/*        BLTFRM */

/*     which fetches the frame IDs of all built-in reference frames. */

/* $ Examples */

/*     The numerical results shown for this example may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */

/*     1) Display the IDs and names of all reference frames having */
/*        specifications present in the kernel pool. Group the outputs */
/*        by frame class. Also fetch and display the entire set of IDs */
/*        and names using the parameter ALL. */

/*        Use the meta-kernel shown below to load the required SPICE */
/*        kernels. */


/*           KPL/MK */

/*           File: kplfrm_ex1.tm */

/*           This meta-kernel is intended to support operation of SPICE */
/*           example programs. The kernels shown here should not be */
/*           assumed to contain adequate or correct versions of data */
/*           required by SPICE-based user applications. */

/*           In order for an application to use this meta-kernel, the */
/*           kernels referenced here must be present in the user's */
/*           current working directory. */

/*           The names and contents of the kernels referenced */
/*           by this meta-kernel are as follows: */

/*              File name            Contents */
/*              --------------       -------------------------- */
/*              clem_v20.tf          Clementine FK */
/*              moon_060721.tf       Generic Lunar SPICE frames */


/*           \begindata */

/*              KERNELS_TO_LOAD = ( 'clem_v20.tf' */
/*                                  'moon_060721.tf' ) */
/*           \begintext */

/*           End of meta-kernel */


/*        Example code begins here. */


/*              PROGRAM KPLFRM_EX1 */
/*              IMPLICIT NONE */

/*              INCLUDE 'frmtyp.inc' */
/*        C */
/*        C     SPICELIB functions */
/*        C */
/*              INTEGER               CARDI */
/*        C */
/*        C     Local parameters */
/*        C */
/*              CHARACTER*(*)         META */
/*              PARAMETER           ( META   = 'kplfrm_ex1.tm' ) */

/*              INTEGER               NFRAME */
/*              PARAMETER           ( NFRAME = 1000 ) */

/*              INTEGER               LBCELL */
/*              PARAMETER           ( LBCELL = -5 ) */

/*              INTEGER               LNSIZE */
/*              PARAMETER           ( LNSIZE = 80 ) */

/*              INTEGER               FRNMLN */
/*              PARAMETER           ( FRNMLN = 32 ) */

/*        C */
/*        C     Local variables */
/*        C */
/*              CHARACTER*(FRNMLN)    FRNAME */
/*              CHARACTER*(LNSIZE)    OUTLIN */

/*              INTEGER               I */
/*              INTEGER               IDSET ( LBCELL : NFRAME ) */
/*              INTEGER               J */

/*        C */
/*        C     Initialize the frame set. */
/*        C */
/*              CALL SSIZEI ( NFRAME, IDSET ) */

/*        C */
/*        C     Load kernels that contain frame specifications. */
/*        C */
/*              CALL FURNSH ( META ) */

/*        C */
/*        C     Fetch and display the frames of each class. */
/*        C */
/*              DO I = 1, 7 */

/*                 IF ( I .LT. 7 ) THEN */
/*        C */
/*        C           Fetch the frames of class I. */
/*        C */
/*                    CALL KPLFRM ( I, IDSET ) */

/*                    OUTLIN = 'Number of frames of class #: #' */
/*                    CALL REPMI ( OUTLIN, '#', I,            OUTLIN ) */
/*                    CALL REPMI ( OUTLIN, '#', CARDI(IDSET), OUTLIN ) */

/*                 ELSE */
/*        C */
/*        C           Fetch IDs of all frames specified in the kernel */
/*        C           pool. */
/*        C */
/*                    CALL KPLFRM ( ALL, IDSET ) */

/*                    OUTLIN = 'Number of frames in the kernel pool: #' */
/*                    CALL REPMI ( OUTLIN, '#', CARDI(IDSET), OUTLIN ) */

/*                 END IF */

/*                 CALL TOSTDO ( ' '    ) */
/*                 CALL TOSTDO ( OUTLIN ) */
/*                 CALL TOSTDO ( '   Frame IDs and names' ) */

/*                 DO J = 1, CARDI(IDSET) */
/*                    CALL FRMNAM ( IDSET(J), FRNAME ) */
/*                    WRITE (*,*) IDSET(J), '  ', FRNAME */
/*                 END DO */

/*              END DO */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*        Number of frames of class 1: 0 */
/*           Frame IDs and names */

/*        Number of frames of class 2: 1 */
/*           Frame IDs and names */
/*               31002   MOON_PA_DE403 */

/*        Number of frames of class 3: 1 */
/*           Frame IDs and names */
/*              -40000   CLEM_SC_BUS */

/*        Number of frames of class 4: 11 */
/*           Frame IDs and names */
/*              -40008   CLEM_CPT */
/*              -40007   CLEM_BSTAR */
/*              -40006   CLEM_ASTAR */
/*              -40005   CLEM_LIDAR */
/*              -40004   CLEM_LWIR */
/*              -40003   CLEM_NIR */
/*              -40002   CLEM_UVVIS */
/*              -40001   CLEM_HIRES */
/*               31000   MOON_PA */
/*               31001   MOON_ME */
/*               31003   MOON_ME_DE403 */

/*        Number of frames of class 5: 0 */
/*           Frame IDs and names */

/*        Number of frames of class 6: 0 */
/*           Frame IDs and names */

/*        Number of frames in the kernel pool: 13 */
/*           Frame IDs and names */
/*              -40008   CLEM_CPT */
/*              -40007   CLEM_BSTAR */
/*              -40006   CLEM_ASTAR */
/*              -40005   CLEM_LIDAR */
/*              -40004   CLEM_LWIR */
/*              -40003   CLEM_NIR */
/*              -40002   CLEM_UVVIS */
/*              -40001   CLEM_HIRES */
/*              -40000   CLEM_SC_BUS */
/*               31000   MOON_PA */
/*               31001   MOON_ME */
/*               31002   MOON_PA_DE403 */
/*               31003   MOON_ME_DE403 */


/* $ Restrictions */

/*     1)  This routine will work correctly if the kernel pool contains */
/*         no invalid frame specifications. See the description of */
/*         exception 4 above. Users must ensure that no invalid frame */
/*         specifications are introduced into the kernel pool, either by */
/*         loaded kernels or by means of the kernel pool "put" APIs. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */

/* $ Version */

/* -    SPICELIB Version 2.0.0, 08-AUG-2021 (JDR) (NJB) */

/*        Updated to account for switch frame class. */

/*        Edited the header to comply with NAIF standard. */

/*        Updated Example's kernels set to use Clementine PDS archived */
/*        data. */

/* -    SPICELIB Version 1.1.0, 18-JUN-2015 (NJB) */

/*        Bug fix: previous algorithm failed if the number of frame */
/*        names fetched from the kernel pool on a single call exceeded */
/*        twice the kernel variable buffer size. The count of */
/*        fetched names is now maintained correctly. */

/* -    SPICELIB Version 1.0.0, 22-MAY-2012 (NJB) */

/* -& */
/* $ Index_Entries */

/*     fetch IDs of reference_frames from the kernel_pool */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     Local variables */

    if (return_()) {
	return 0;
    }
    chkin_("KPLFRM", (ftnlen)6);

/*     The output set starts out empty. */

    scardi_(&c__0, idset);

/*     Check the input frame class. */

/*     This block of code must be kept in sync with frmtyp.inc. */

    if (*frmcls > 6 || *frmcls == 0 || *frmcls < -1) {
	setmsg_("Frame class specifier FRMCLS was #; this value is not suppo"
		"rted.", (ftnlen)64);
	errint_("#", frmcls, (ftnlen)1);
	sigerr_("SPICE(BADFRAMECLASS)", (ftnlen)20);
	chkout_("KPLFRM", (ftnlen)6);
	return 0;
    }

/*     Initialize the output buffer index. The */
/*     index is to be incremented prior to each */
/*     write to the buffer. */

    to = 0;

/*     Find all of the kernel variables having names */
/*     that could correspond to frame name assignments. */

/*     We expect that all frame specifications will */
/*     include assignments of the form */

/*         FRAME_<ID code>_NAME = <frame name> */

/*     We may pick up some additional assignments that are not part of */
/*     frame specifications; we plan to filter out as many as possible */
/*     by looking the corresponding frame ID and frame class */
/*     assignments. */

    s_copy(kvtemp, "FRAME_*_NAME", (ftnlen)32, (ftnlen)12);
    gnpool_(kvtemp, &c__1, &c__100, &n, kvbuff, &found, (ftnlen)32, (ftnlen)
	    32);
    total = 0;
    while(n > 0) {
	total += n;

/*        At least one kernel variable was found by the last */
/*        GNPOOL call. Each of these variables is a possible */
/*        frame name. Look up each of these candidate names. */

	i__1 = n;
	for (i__ = 1; i__ <= i__1; ++i__) {

/*           Attempt to fetch the right hand side value for */
/*           the Ith kernel variable found on the previous */
/*           GNPOOL call. */

	    gcpool_(kvbuff + (((i__2 = i__ - 1) < 100 && 0 <= i__2 ? i__2 : 
		    s_rnge("kvbuff", i__2, "kplfrm_", (ftnlen)532)) << 5), &
		    c__1, &c__1, &m, frname, &found, (ftnlen)32, (ftnlen)32);
	    if (found) {

/*              We found a possible frame name. Attempt to look */
/*              up an ID code variable for the name. The assignment */
/*              for the ID code, if present, will have the form */

/*                 FRAME_<name> = <ID code> */

/*              Create the kernel variable name on the left hand */
/*              side of the assignment. */

		s_copy(kvcode, "FRAME_<name>", (ftnlen)32, (ftnlen)12);
		repmc_(kvcode, "<name>", frname, kvcode, (ftnlen)32, (ftnlen)
			6, (ftnlen)32, (ftnlen)32);

/*              Try to fetch the ID code. */

		gipool_(kvcode, &c__1, &c__1, &l, &idcode, &found, (ftnlen)32)
			;
		if (found) {

/*                 We found an integer on the right hand side */
/*                 of the assignment. We probably have a */
/*                 frame specification at this point. Check that */
/*                 the variable */

/*                    FRAME_<ID code>_NAME */

/*                 is present in the kernel pool and maps to */
/*                 the name FRNAME. */

		    s_copy(kvname, "FRAME_<code>_NAME", (ftnlen)32, (ftnlen)
			    17);
		    repmi_(kvname, "<code>", &idcode, kvname, (ftnlen)32, (
			    ftnlen)6, (ftnlen)32);
		    gcpool_(kvname, &c__1, &c__1, &w, tmpnam, &found, (ftnlen)
			    32, (ftnlen)32);
		    if (found) {

/*                    Try to look up the frame class using a */
/*                    kernel variable name of the form */

/*                       FRAME_<integer ID code>_CLASS */

/*                    Create the kernel variable name on the left */
/*                    hand side of the frame class assignment. */

			s_copy(kvclas, "FRAME_<integer>_CLASS", (ftnlen)32, (
				ftnlen)21);
			repmi_(kvclas, "<integer>", &idcode, kvclas, (ftnlen)
				32, (ftnlen)9, (ftnlen)32);

/*                    Look for the frame class. */

			gipool_(kvclas, &c__1, &c__1, &w, &fclass, &found, (
				ftnlen)32);
			if (! found) {

/*                       Try to look up the frame class using a kernel */
/*                       variable name of the form */

/*                          FRAME_<frame name>_CLASS */

			    s_copy(kvclas, "FRAME_<name>_CLASS", (ftnlen)32, (
				    ftnlen)18);
			    repmc_(kvclas, "<name>", frname, kvclas, (ftnlen)
				    32, (ftnlen)6, (ftnlen)32, (ftnlen)32);
			    gipool_(kvclas, &c__1, &c__1, &w, &fclass, &found,
				     (ftnlen)32);
			}

/*                    At this point FOUND indicates whether we found */
/*                    the frame class. */

			if (found) {

/*                       Check whether the frame class is one */
/*                       we want. */

			    if (*frmcls == -1 || *frmcls == fclass) {

/*                          We have a winner. Add it to the output set. */

/*                          First make sure the set is large enough to */
/*                          hold another element. */

				if (to == sizei_(idset)) {
				    setmsg_("Frame ID set argument IDSET has"
					    " size #; required size is at lea"
					    "st #. Make sure that the caller "
					    "of this routine has initialized "
					    "IDSET via SSIZEI.", (ftnlen)144);
				    i__2 = sizei_(idset);
				    errint_("#", &i__2, (ftnlen)1);
				    i__2 = to + 1;
				    errint_("#", &i__2, (ftnlen)1);
				    sigerr_("SPICE(SETTOOSMALL)", (ftnlen)18);
				    chkout_("KPLFRM", (ftnlen)6);
				    return 0;
				}
				++to;
				idset[to + 5] = idcode;
			    }

/*                       End of IF block for processing a frame having */
/*                       a frame class matching the request. */

			}

/*                    End of IF block for finding the frame class. */

		    }

/*                 End of IF block for finding the frame name. */

		}

/*              End of IF block for finding the frame ID. */

	    }

/*           End of IF block for finding string value corresponding to */
/*           the Ith kernel variable matching the name template. */

	}

/*        End of loop for processing last batch of potential */
/*        frame names. */

/*        Fetch next batch of potential frame names. */

	i__1 = total + 1;
	gnpool_(kvtemp, &i__1, &c__100, &n, kvbuff, &found, (ftnlen)32, (
		ftnlen)32);
    }

/*     At this point all kernel variables that matched the frame name */
/*     keyword template have been processed. All frames of the specified */
/*     class or classes have had their ID codes appended to IDSET. In */
/*     general IDSET is not yet a SPICELIB set, since it's not sorted */
/*     and it may contain duplicate values. */

/*     Turn IDSET into a set. VALIDI sorts and removes duplicates. */

    i__1 = sizei_(idset);
    validi_(&i__1, &to, idset);
    chkout_("KPLFRM", (ftnlen)6);
    return 0;
} /* kplfrm_ */

