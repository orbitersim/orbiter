/* tkfram.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__200 = 200;
static integer c__1 = 1;
static integer c__2 = 2;
static integer c__9 = 9;
static doublereal c_b120 = -1.;
static integer c__3 = 3;
static integer c__4 = 4;
static integer c__14 = 14;

/* $Procedure TKFRAM ( TK frame, find position rotation ) */
/* Subroutine */ int tkfram_(integer *frcode, doublereal *rot, integer *frame,
	 logical *found)
{
    /* Initialized data */

    static integer at = 0;
    static doublereal buffd[1800]	/* was [9][200] */ = { 0.,0.,0.,0.,0.,
	    0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
	    0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
	    0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
	    0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
	    0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
	    0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
	    0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
	    0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
	    0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
	    0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
	    0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
	    0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
	    0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
	    0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
	    0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
	    0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
	    0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
	    0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
	    0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
	    0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
	    0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
	    0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
	    0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
	    0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
	    0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
	    0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
	    0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
	    0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
	    0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
	    0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
	    0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
	    0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
	    0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
	    0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
	    0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
	    0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
	    0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
	    0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
	    0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
	    0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
	    0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
	    0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
	    0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
	    0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
	    0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
	    0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
	    0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
	    0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
	    0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
	    0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
	    0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
	    0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
	    0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
	    0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
	    0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
	    0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
	    0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
	    0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
	    0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
	    0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
	    0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
	    0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
	    0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
	    0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
	    0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
	    0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
	    0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
	    0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
	    0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
	    0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
	    0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
	    0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
	    0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
	    0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
	    0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
	    0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
	    0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
	    0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
	    0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
	    0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
	    0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
	    0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0. };
    static integer buffi[200]	/* was [1][200] */ = { 0,0,0,0,0,0,0,0,0,0,0,
	    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0 };
    static logical first = TRUE_;
    static integer idents[200]	/* was [1][200] */ = { 0,0,0,0,0,0,0,0,0,0,0,
	    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0 };

    /* System generated locals */
    address a__1[2];
    integer i__1, i__2[2], i__3;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_cmp(char *, char *, ftnlen, ftnlen);
    /* Subroutine */ int s_cat(char *, char **, integer *, integer *, ftnlen);

    /* Local variables */
    static char name__[32];
    static integer tail;
    static char spec[32], item[32*14];
    static integer idnt[1], axes[3];
    static logical full;
    static integer pool[412]	/* was [2][206] */;
    extern doublereal vdot_(doublereal *, doublereal *);
    static char type__[1];
    static doublereal qtmp[4];
    extern /* Subroutine */ int eul2m_(doublereal *, doublereal *, doublereal 
	    *, integer *, integer *, integer *, doublereal *);
    static integer i__, n, r__, oldid;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    static char agent[32];
    extern /* Subroutine */ int ucase_(char *, char *, ftnlen, ftnlen), 
	    ident_(doublereal *), errch_(char *, char *, ftnlen, ftnlen);
    static doublereal tempd;
    extern /* Subroutine */ int moved_(doublereal *, integer *, doublereal *),
	     repmi_(char *, char *, integer *, char *, ftnlen, ftnlen, ftnlen)
	    , vhatg_(doublereal *, integer *, doublereal *);
    extern integer lnktl_(integer *, integer *);
    static char idstr[32];
    extern integer rtrim_(char *, ftnlen);
    static char units[32];
    static logical found1, found2;
    static integer ar;
    extern logical failed_(void), badkpv_(char *, char *, char *, integer *, 
	    integer *, char *, ftnlen, ftnlen, ftnlen, ftnlen);
    static char frname[32];
    static doublereal angles[3];
    static char oldagt[32];
    static logical buffrd;
    extern /* Subroutine */ int locati_(integer *, integer *, integer *, 
	    integer *, integer *, logical *), frmnam_(integer *, char *, 
	    ftnlen), namfrm_(char *, integer *, ftnlen);
    static logical update;
    static char altnat[32];
    extern /* Subroutine */ int lnkini_(integer *, integer *);
    extern integer lnknfn_(integer *);
    extern /* Subroutine */ int gcpool_(char *, integer *, integer *, integer 
	    *, char *, logical *, ftnlen, ftnlen), gdpool_(char *, integer *, 
	    integer *, integer *, doublereal *, logical *, ftnlen), sigerr_(
	    char *, ftnlen), gipool_(char *, integer *, integer *, integer *, 
	    integer *, logical *, ftnlen), chkout_(char *, ftnlen), sharpr_(
	    doublereal *), dtpool_(char *, logical *, integer *, char *, 
	    ftnlen, ftnlen);
    static doublereal matrix[9]	/* was [3][3] */;
    extern /* Subroutine */ int setmsg_(char *, ftnlen), cvpool_(char *, 
	    logical *, ftnlen), dwpool_(char *, ftnlen), errint_(char *, 
	    integer *, ftnlen);
    static doublereal quatrn[4];
    extern /* Subroutine */ int vsclip_(doublereal *, doublereal *), convrt_(
	    doublereal *, char *, char *, doublereal *, ftnlen, ftnlen);
    extern logical return_(void);
    extern /* Subroutine */ int q2m_(doublereal *, doublereal *), intstr_(
	    integer *, char *, ftnlen), swpool_(char *, integer *, char *, 
	    ftnlen, ftnlen);
    static logical fnd;
    static char alt[32*14];

/* $ Abstract */

/*     Find the position rotation matrix from a Text Kernel (TK) frame */
/*     with the specified frame class ID to its base frame. */

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

/* $ Keywords */

/*     POINTING */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  ---------------------------------------------- */
/*     FRCODE     I   Frame class ID of a TK frame. */
/*     ROT        O   Rotation matrix from TK frame to frame FRAME. */
/*     FRAME      O   Frame ID of the base reference. */
/*     FOUND      O   .TRUE. if the rotation could be determined. */

/* $ Detailed_Input */

/*     FRCODE   is the unique frame class ID of the TK frame for which */
/*              data is being requested. For TK frames the frame class */
/*              ID is always equal to the frame ID. */

/* $ Detailed_Output */

/*     ROT      is a position rotation matrix that converts positions */
/*              relative to the TK frame given by its frame class ID, */
/*              FRCODE, to positions relative to the base frame given by */
/*              its frame ID, FRAME. */

/*              Thus, if a position S has components x,y,z in the TK */
/*              frame, then S has components x', y', z' in the base */
/*              frame. */

/*                 .-  -.     .-     -. .- -. */
/*                 | x' |     |       | | x | */
/*                 | y' |  =  |  ROT  | | y | */
/*                 | z' |     |       | | z | */
/*                 `-  -'     `-     -' `- -' */


/*     FRAME    is the ID code of the base reference frame to which ROT */
/*              will transform positions. */

/*     FOUND    is a logical indicating whether or not a frame definition */
/*              for the TK frame with the frame class ID, FRCODE, was */
/*              constructed from kernel pool data. If ROT and FRAME were */
/*              constructed, FOUND will be returned with the value .TRUE. */
/*              Otherwise it will be returned with the value .FALSE. */

/* $ Parameters */

/*     BUFSIZ   is the number of rotation, frame class ID pairs that can */
/*              have their instance data buffered for the sake of */
/*              improving run-time performance. This value MUST be */
/*              positive and should probably be at least 10. */

/* $ Exceptions */

/*     1)  If some kernel variable associated with this frame is not */
/*         present in the kernel pool, or does not have the proper type */
/*         or dimension, an error is signaled by a routine in the call */
/*         tree of this routine. In such a case FOUND will be set to */
/*         .FALSE. */

/*     2)  If the input FRCODE has the value 0, the error */
/*         SPICE(ZEROFRAMEID) is signaled. FOUND will be set to .FALSE. */

/*     3)  If the name of the frame corresponding to FRCODE cannot be */
/*         determined, the error SPICE(INCOMPLETEFRAME) is signaled. */

/*     4)  If the frame given by FRCODE is defined relative to a frame */
/*         that is unrecognized, the error SPICE(BADFRAMESPEC) is */
/*         signaled. FOUND will be set to .FALSE. */

/*     5)  If the kernel pool specification for the frame given by */
/*         FRCODE is not one of 'MATRIX', 'ANGLES' or 'QUATERNION', */
/*         the error SPICE(UNKNOWNFRAMESPEC) is signaled. FOUND will be */
/*         set to .FALSE. */

/*     6)  If the frame FRCODE is equal to the relative frame ID (i.e. */
/*         the frame is defined relative to itself), the error */
/*         SPICE(BADFRAMESPEC2) is signaled. FOUND will be set to .FALSE. */

/*     7)  If name-based and ID-based forms of any TKFRAME_ keyword */
/*         are detected in the kernel pool at the same time, the error */
/*         SPICE(COMPETINGFRAMESPEC) is signaled. FOUND will be set to */
/*         .FALSE. */

/* $ Files */

/*     This routine makes use of the loaded text kernels to determine */
/*     the rotation from a constant offset TK frame to its base frame. */

/* $ Particulars */

/*     This routine is used to construct the rotation from some frame */
/*     that is a constant rotation offset from some other reference */
/*     frame. This rotation is derived from data stored in the kernel */
/*     pool. */

/*     This routine is intended to be used as a low level routine by the */
/*     frame system software. However, you could use this routine to */
/*     directly retrieve the rotation from an fixed offset TK frame to */
/*     its base frame. */

/* $ Examples */

/*     The numerical results shown for this example may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */

/*     1) Compute the rotation from the DSS-34 topocentric frame to */
/*        its base Earth body-fixed frame and use it to determine the */
/*        geodetic latitude and longitude of the DSS-34 site. */


/*        Use the FK kernel below to load the required topocentric */
/*        reference frame definition for the DSS-34 site. */

/*           earth_topo_050714.tf */


/*        Example code begins here. */


/*              PROGRAM TKFRAM_EX1 */
/*              IMPLICIT NONE */

/*        C */
/*        C     SPICELIB functions. */
/*        C */
/*              DOUBLE PRECISION      DPR */

/*        C */
/*        C     Local parameters */
/*        C */
/*              CHARACTER*(*)         MYTOPO */
/*              PARAMETER           ( MYTOPO = 'DSS-34_TOPO' ) */

/*              INTEGER               MXFRLN */
/*              PARAMETER           ( MXFRLN = 26 ) */

/*        C */
/*        C     Local variables */
/*        C */
/*              CHARACTER*(MXFRLN)    FRNAME */

/*              DOUBLE PRECISION      LAT */
/*              DOUBLE PRECISION      LON */
/*              DOUBLE PRECISION      RAD */
/*              DOUBLE PRECISION      ROT   ( 3, 3 ) */
/*              DOUBLE PRECISION      Z     ( 3    ) */

/*              INTEGER               FRAME */
/*              INTEGER               FRCODE */

/*              LOGICAL               FOUND */

/*        C */
/*        C     Load the FK that contains the topocentric reference */
/*        C     frame definition for DSS-34. */
/*        C */
/*              CALL FURNSH ( 'earth_topo_050714.tf' ) */

/*        C */
/*        C     The name of the topocentric frame is MYTOPO. */
/*        C     First we get the ID code of the topocentric frame. */
/*        C */
/*              CALL NAMFRM ( MYTOPO, FRCODE ) */

/*        C */
/*        C     Next get the rotation from the topocentric frame to */
/*        C     the body-fixed frame. We can use the TK frame ID in */
/*        C     place of the TK frame class ID in this call because */
/*        C     for TK frames these IDs are identical. */
/*        C */
/*              CALL TKFRAM ( FRCODE, ROT, FRAME, FOUND ) */

/*        C */
/*        C     Make sure the topocentric frame is relative to one of */
/*        C     the Earth fixed frames. */
/*        C */
/*              CALL FRMNAM( FRAME, FRNAME ) */

/*              IF (       FRNAME .NE. 'IAU_EARTH' */
/*             .     .AND. FRNAME .NE. 'EARTH_FIXED' */
/*             .     .AND. FRNAME .NE. 'ITRF93'  ) THEN */

/*                 WRITE (*,*) 'The frame ', MYTOPO, */
/*             .               ' does not appear to be ' */
/*                 WRITE (*,*) 'defined relative to an ' */
/*             .            // 'Earth fixed frame.' */
/*                 STOP */

/*              END IF */

/*        C */
/*        C     Things look ok. Get the location of the Z-axis in the */
/*        C     topocentric frame. */
/*        C */
/*              Z(1) = ROT(1,3) */
/*              Z(2) = ROT(2,3) */
/*              Z(3) = ROT(3,3) */

/*        C */
/*        C     Convert the Z vector to latitude, longitude and radius. */
/*        C */
/*              CALL RECLAT ( Z, RAD, LAT, LON ) */

/*              WRITE (*,'(A)') 'The geodetic coordinates of the center' */
/*              WRITE (*,'(A)') 'of the topographic frame are:' */
/*              WRITE (*,*) */
/*              WRITE (*,'(A,F20.13)') '   Latitude  (deg): ', LAT*DPR() */
/*              WRITE (*,'(A,F20.13)') '   Longitude (deg): ', LON*DPR() */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*        The geodetic coordinates of the center */
/*        of the topographic frame are: */

/*           Latitude  (deg):    148.9819650021110 */
/*           Longitude (deg):    -35.3984778756552 */


/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     B.V. Semenov       (JPL) */
/*     W.L. Taber         (JPL) */
/*     F.S. Turner        (JPL) */

/* $ Version */

/* -    SPICELIB Version 2.3.0, 20-AUG-2021 (JDR) (BVS) (NJB) */

/*        BUG FIX: the routine now signals an error if it detects */
/*        name-based and ID-based forms of any TKFRAME_ keyword present */
/*        in the POOL at the same time. This prevents name-based */
/*        keywords from frame definitions loaded with lower priority */
/*        from being used instead of ID-based keywords from frame */
/*        definitions loaded with higher priority. */

/*        BUG FIX: when failing to fetch any frame keywords from the */
/*        POOL or for any other reason, the routine now always returns */
/*        FOUND = .FALSE. Previously FOUND could be set to .TRUE. by a */
/*        DTPOOL call preceding the failure. */

/*        BUG FIX: when failing due to a frame defined relative to */
/*        itself or due to an unrecognized _SPEC, the routine now always */
/*        returns FRAME = 0. Previously FRAME was set to the _RELATIVE */
/*        keyword. */

/*        BUG FIX: the misspelled short error message */
/*        SPICE(INCOMPLETEFRAME) was corrected. The message had been */
/*        spelled correctly in header comments but not in the code. */

/*        Changed to return ROT as identity for all failures; previously */
/*        it was returned this way only for some failures. */

/*        Changed the input argument name ID to FRCODE for consistency */
/*        with other routines. */

/*        Fixed minor typo on the UNKNOWNFRAMESPEC long error message. */

/*        Edited the header to comply with NAIF standard and modern */
/*        SPICE CK and frames terminology. */

/*        Added complete code example based on existing fragments. */

/*        Construction of kernel variable names now uses trimmed */
/*        strings in order to suppress gfortran compile warnings. */

/*        Added DATA statements to initialize BUFFI, BUFFD, and IDENTS. */
/*        This change suppresses ftnchek warnings for variables possibly */
/*        not initialized before use. It is not a bug fix. */

/*        Minor inline comment typos were corrected. */

/* -    SPICELIB Version 2.2.0, 08-JAN-2014 (BVS) */

/*        Added an error check for frames defined relative to */
/*        themselves. */

/*        Increased BUFSIZ from 20 to 200. */

/* -    SPICELIB Version 2.1.0, 23-APR-2009 (NJB) */

/*        Bug fix: watch is deleted only for frames */
/*        that are deleted from the buffer. */

/* -    SPICELIB Version 2.0.0, 19-MAR-2009 (NJB) */

/*        Bug fix: this routine now deletes watches set on */
/*        kernel variables of frames that are discarded from */
/*        the local buffering system. */

/* -    SPICELIB Version 1.2.0, 09-SEP-2005 (NJB) */

/*        Updated to remove non-standard use of duplicate arguments */
/*        in CONVRT, UCRSS, VHATG and VSCL calls. */

/* -    SPICELIB Version 1.1.0, 21-NOV-2001 (FST) */

/*        Updated this routine to dump the buffer of frame ID codes */
/*        it saves when it or one of the modules in its call tree */
/*        signals an error. This fixes a bug where a frame's ID code is */
/*        buffered, but the matrix and kernel pool watcher were not set */
/*        properly. */

/* -    SPICELIB Version 1.0.0, 18-NOV-1996 (WLT) */

/* -& */
/* $ Index_Entries */

/*     Fetch the rotation and frame of a text kernel frame */
/*     Fetch the rotation and frame of a constant offset frame */

/* -& */

/*     Spicelib Functions */


/*     Local Parameters */


/*     Local Variables */


/*     Saved variables */


/*     Initial values */


/*     Programmer's note: this routine makes use of the *implementation* */
/*     of LOCATI. If that routine is changed, the logic this routine */
/*     uses to locate buffered, old frame IDs may need to change as well. */


/*     Before we even check in, if N is less than 1 we can */
/*     just return. */


/*     Perform any initializations that might be needed for this */
/*     routine. */

    if (first) {
	first = FALSE_;
	lnkini_(&c__200, pool);
    }

/*     Now do the standard SPICE error handling.  Sure this is */
/*     a bit unconventional, but nothing will be hurt by doing */
/*     the stuff above first. */

    if (return_()) {
	return 0;
    }
    chkin_("TKFRAM", (ftnlen)6);

/*     So far, we've not FOUND the rotation to the specified frame. */

    *found = FALSE_;

/*     Check the ID to make sure it is non-zero. */

    if (*frcode == 0) {
	lnkini_(&c__200, pool);
	setmsg_("Frame identification codes are required to be non-zero.  Yo"
		"u've specified a frame with ID value zero. ", (ftnlen)102);
	sigerr_("SPICE(ZEROFRAMEID)", (ftnlen)18);
	chkout_("TKFRAM", (ftnlen)6);
	return 0;
    }

/*     Find out whether our linked list pool is already full. */
/*     We'll use this information later to decide whether we're */
/*     going to have to delete a watcher. */

    full = lnknfn_(pool) == 0;
    if (full) {

/*        If the input frame ID is not buffered, we'll need to */
/*        overwrite an existing buffer entry. In this case */
/*        the call to LOCATI we're about to make will overwrite */
/*        the ID code in the slot we're about to use. We need */
/*        this ID code, so extract it now while we have the */
/*        opportunity. The old ID sits at the tail of the list */
/*        whose head node is AT. */

	tail = lnktl_(&at, pool);
	oldid = idents[(i__1 = tail - 1) < 200 && 0 <= i__1 ? i__1 : s_rnge(
		"idents", i__1, "tkfram_", (ftnlen)560)];

/*        Create the name of the agent associated with the old */
/*        frame. */

	s_copy(oldagt, "TKFRAME_#", (ftnlen)32, (ftnlen)9);
	repmi_(oldagt, "#", &oldid, oldagt, (ftnlen)32, (ftnlen)1, (ftnlen)32)
		;
    }

/*     Look up the address of the instance data. */

    idnt[0] = *frcode;
    locati_(idnt, &c__1, idents, pool, &at, &buffrd);
    if (full && ! buffrd) {

/*        Since the buffer is already full, we'll delete the watcher for */
/*        the kernel variables associated with OLDID, since there's no */
/*        longer a need for that watcher. */

/*        First clear the update status of the old agent; DWPOOL won't */
/*        delete an agent with a unchecked update. */

	cvpool_(oldagt, &update, (ftnlen)32);
	dwpool_(oldagt, (ftnlen)32);
    }

/*     Until we have better information we put the identity matrix */
/*     into the output rotation and set FRAME to zero. */

    ident_(rot);
    *frame = 0;

/*     If we have to look up the data for our frame, we do */
/*     it now and perform any conversions and computations that */
/*     will be needed when it's time to convert coordinates to */
/*     directions. */

/*     Construct the name of the agent associated with the */
/*     requested frame.  (Each frame has its own agent). */

    intstr_(frcode, idstr, (ftnlen)32);
    frmnam_(frcode, frname, (ftnlen)32);
    if (s_cmp(frname, " ", (ftnlen)32, (ftnlen)1) == 0) {
	lnkini_(&c__200, pool);
	setmsg_("The Text Kernel (TK) frame with ID code # does not have a r"
		"ecognized name. ", (ftnlen)75);
	errint_("#", frcode, (ftnlen)1);
	sigerr_("SPICE(INCOMPLETEFRAME)", (ftnlen)22);
	chkout_("TKFRAM", (ftnlen)6);
	return 0;
    }
/* Writing concatenation */
    i__2[0] = 8, a__1[0] = "TKFRAME_";
    i__2[1] = rtrim_(idstr, (ftnlen)32), a__1[1] = idstr;
    s_cat(agent, a__1, i__2, &c__2, (ftnlen)32);
    r__ = rtrim_(agent, (ftnlen)32);
/* Writing concatenation */
    i__2[0] = 8, a__1[0] = "TKFRAME_";
    i__2[1] = rtrim_(frname, (ftnlen)32), a__1[1] = frname;
    s_cat(altnat, a__1, i__2, &c__2, (ftnlen)32);
    ar = rtrim_(altnat, (ftnlen)32);

/*     If the frame is buffered, we check the kernel pool to */
/*     see if there has been an update to this frame. */

    if (buffrd) {
	cvpool_(agent, &update, r__);
    } else {

/*        If the frame is not buffered we definitely need to update */
/*        things. */
	update = TRUE_;
    }
    if (! update) {

/*        Just look up the rotation matrix and relative-to */
/*        information from the local buffer. */

	rot[0] = buffd[(i__1 = at * 9 - 9) < 1800 && 0 <= i__1 ? i__1 : 
		s_rnge("buffd", i__1, "tkfram_", (ftnlen)653)];
	rot[1] = buffd[(i__1 = at * 9 - 8) < 1800 && 0 <= i__1 ? i__1 : 
		s_rnge("buffd", i__1, "tkfram_", (ftnlen)654)];
	rot[2] = buffd[(i__1 = at * 9 - 7) < 1800 && 0 <= i__1 ? i__1 : 
		s_rnge("buffd", i__1, "tkfram_", (ftnlen)655)];
	rot[3] = buffd[(i__1 = at * 9 - 6) < 1800 && 0 <= i__1 ? i__1 : 
		s_rnge("buffd", i__1, "tkfram_", (ftnlen)656)];
	rot[4] = buffd[(i__1 = at * 9 - 5) < 1800 && 0 <= i__1 ? i__1 : 
		s_rnge("buffd", i__1, "tkfram_", (ftnlen)657)];
	rot[5] = buffd[(i__1 = at * 9 - 4) < 1800 && 0 <= i__1 ? i__1 : 
		s_rnge("buffd", i__1, "tkfram_", (ftnlen)658)];
	rot[6] = buffd[(i__1 = at * 9 - 3) < 1800 && 0 <= i__1 ? i__1 : 
		s_rnge("buffd", i__1, "tkfram_", (ftnlen)659)];
	rot[7] = buffd[(i__1 = at * 9 - 2) < 1800 && 0 <= i__1 ? i__1 : 
		s_rnge("buffd", i__1, "tkfram_", (ftnlen)660)];
	rot[8] = buffd[(i__1 = at * 9 - 1) < 1800 && 0 <= i__1 ? i__1 : 
		s_rnge("buffd", i__1, "tkfram_", (ftnlen)661)];
	*frame = buffi[(i__1 = at - 1) < 200 && 0 <= i__1 ? i__1 : s_rnge(
		"buffi", i__1, "tkfram_", (ftnlen)663)];
    } else {

/*        Determine how the frame is specified and what it */
/*        is relative to.  The variables that specify */
/*        how the frame is represented and what it is relative to */
/*        are TKFRAME_#_SPEC and TKFRAME_#_RELATIVE where # is */
/*        replaced by the text value of ID or the frame name. */

/* Writing concatenation */
	i__2[0] = r__, a__1[0] = agent;
	i__2[1] = 5, a__1[1] = "_SPEC";
	s_cat(item, a__1, i__2, &c__2, (ftnlen)32);
/* Writing concatenation */
	i__2[0] = r__, a__1[0] = agent;
	i__2[1] = 9, a__1[1] = "_RELATIVE";
	s_cat(item + 32, a__1, i__2, &c__2, (ftnlen)32);
/* Writing concatenation */
	i__2[0] = ar, a__1[0] = altnat;
	i__2[1] = 5, a__1[1] = "_SPEC";
	s_cat(alt, a__1, i__2, &c__2, (ftnlen)32);
/* Writing concatenation */
	i__2[0] = ar, a__1[0] = altnat;
	i__2[1] = 9, a__1[1] = "_RELATIVE";
	s_cat(alt + 32, a__1, i__2, &c__2, (ftnlen)32);

/*        See if the friendlier version of the kernel pool variables */
/*        are available. */

/*        If both forms are present, we signal an error. */

	for (i__ = 1; i__ <= 2; ++i__) {
	    dtpool_(item + (((i__1 = i__ - 1) < 14 && 0 <= i__1 ? i__1 : 
		    s_rnge("item", i__1, "tkfram_", (ftnlen)686)) << 5), &
		    found1, &n, type__, (ftnlen)32, (ftnlen)1);
	    dtpool_(alt + (((i__1 = i__ - 1) < 14 && 0 <= i__1 ? i__1 : 
		    s_rnge("alt", i__1, "tkfram_", (ftnlen)687)) << 5), &
		    found2, &n, type__, (ftnlen)32, (ftnlen)1);
	    if (found1 && found2) {
		lnkini_(&c__200, pool);
		*frame = 0;
		ident_(rot);
		setmsg_("Frame name-based and frame ID-based text kernel (fi"
			"xed-offset) frame definition keywords '#' and '#' ar"
			"e both present in the POOL. Most likely this is beca"
			"use loaded text kernels contain competing definition"
			"s of the '#' frame using different keyword styles, w"
			"hich is not allowed. ", (ftnlen)280);
		errch_("#", item + (((i__1 = i__ - 1) < 14 && 0 <= i__1 ? 
			i__1 : s_rnge("item", i__1, "tkfram_", (ftnlen)704)) 
			<< 5), (ftnlen)1, (ftnlen)32);
		errch_("#", alt + (((i__1 = i__ - 1) < 14 && 0 <= i__1 ? i__1 
			: s_rnge("alt", i__1, "tkfram_", (ftnlen)705)) << 5), 
			(ftnlen)1, (ftnlen)32);
		errch_("#", frname, (ftnlen)1, (ftnlen)32);
		sigerr_("SPICE(COMPETINGFRAMESPEC)", (ftnlen)25);
		chkout_("TKFRAM", (ftnlen)6);
		return 0;
	    }
	    if (found2) {
		s_copy(item + (((i__1 = i__ - 1) < 14 && 0 <= i__1 ? i__1 : 
			s_rnge("item", i__1, "tkfram_", (ftnlen)713)) << 5), 
			alt + (((i__3 = i__ - 1) < 14 && 0 <= i__3 ? i__3 : 
			s_rnge("alt", i__3, "tkfram_", (ftnlen)713)) << 5), (
			ftnlen)32, (ftnlen)32);
	    }
	}

/*        If either the SPEC or RELATIVE frame are missing from */
/*        the kernel pool, we simply return. */

	if (badkpv_("TKFRAM", item, "=", &c__1, &c__1, "C", (ftnlen)6, (
		ftnlen)32, (ftnlen)1, (ftnlen)1) || badkpv_("TKFRAM", item + 
		32, "=", &c__1, &c__1, "C", (ftnlen)6, (ftnlen)32, (ftnlen)1, 
		(ftnlen)1)) {
	    lnkini_(&c__200, pool);
	    *frame = 0;
	    ident_(rot);
	    chkout_("TKFRAM", (ftnlen)6);
	    return 0;
	}

/*        If we make it this far, look up the SPEC and RELATIVE frame. */

	gcpool_(item, &c__1, &c__1, &n, spec, &fnd, (ftnlen)32, (ftnlen)32);
	gcpool_(item + 32, &c__1, &c__1, &n, name__, &fnd, (ftnlen)32, (
		ftnlen)32);

/*        Look up the ID code for this frame. */

	namfrm_(name__, frame, (ftnlen)32);
	if (*frame == 0) {
	    lnkini_(&c__200, pool);
	    ident_(rot);
	    setmsg_("The frame to which frame # is relatively defined is not"
		    " recognized. The kernel pool specification of the relati"
		    "ve frame is '#'.  This is not a recognized frame. ", (
		    ftnlen)161);
	    errint_("#", frcode, (ftnlen)1);
	    errch_("#", name__, (ftnlen)1, (ftnlen)32);
	    sigerr_("SPICE(BADFRAMESPEC)", (ftnlen)19);
	    chkout_("TKFRAM", (ftnlen)6);
	    return 0;
	}

/*        Make sure that the RELATIVE frame ID is distinct from the */
/*        frame ID. If they are the same, SPICE will go into an */
/*        indefinite loop. */

	if (*frame == *frcode) {
	    lnkini_(&c__200, pool);
	    *frame = 0;
	    ident_(rot);
	    setmsg_("Bad fixed offset frame specification: the frame '#' (fr"
		    "ame ID #) is defined relative to itself. SPICE cannot wo"
		    "rk with such frames. ", (ftnlen)132);
	    errch_("#", name__, (ftnlen)1, (ftnlen)32);
	    errint_("#", frcode, (ftnlen)1);
	    sigerr_("SPICE(BADFRAMESPEC2)", (ftnlen)20);
	    chkout_("TKFRAM", (ftnlen)6);
	    return 0;
	}

/*        Convert SPEC to upper case so that we can easily check */
/*        to see if this is one of the expected specification types. */

	ucase_(spec, spec, (ftnlen)32, (ftnlen)32);
	if (s_cmp(spec, "MATRIX", (ftnlen)32, (ftnlen)6) == 0) {

/*           This is the easiest case.  Just grab the matrix */
/*           from the kernel pool (and polish it up a bit just */
/*           to make sure we have a rotation matrix). */

/*           We give preference to the kernel pool variable */
/*           TKFRAME_<name>_MATRIX if it is available. */

/*           If both forms are present, we signal an error. */

/* Writing concatenation */
	    i__2[0] = r__, a__1[0] = agent;
	    i__2[1] = 7, a__1[1] = "_MATRIX";
	    s_cat(item + 64, a__1, i__2, &c__2, (ftnlen)32);
/* Writing concatenation */
	    i__2[0] = ar, a__1[0] = altnat;
	    i__2[1] = 7, a__1[1] = "_MATRIX";
	    s_cat(alt + 64, a__1, i__2, &c__2, (ftnlen)32);
	    dtpool_(item + 64, &found1, &n, type__, (ftnlen)32, (ftnlen)1);
	    dtpool_(alt + 64, &found2, &n, type__, (ftnlen)32, (ftnlen)1);
	    if (found1 && found2) {
		lnkini_(&c__200, pool);
		*frame = 0;
		ident_(rot);
		setmsg_("Frame name-based and frame ID-based text kernel (fi"
			"xed-offset) frame definition keywords '#' and '#' ar"
			"e both present in the POOL. Most likely this is beca"
			"use loaded text kernels contain competing definition"
			"s of the '#' frame using different keyword styles, w"
			"hich is not allowed. ", (ftnlen)280);
		errch_("#", item + 64, (ftnlen)1, (ftnlen)32);
		errch_("#", alt + 64, (ftnlen)1, (ftnlen)32);
		errch_("#", frname, (ftnlen)1, (ftnlen)32);
		sigerr_("SPICE(COMPETINGFRAMESPEC)", (ftnlen)25);
		chkout_("TKFRAM", (ftnlen)6);
		return 0;
	    }
	    if (found2) {
		s_copy(item + 64, alt + 64, (ftnlen)32, (ftnlen)32);
	    }
	    if (badkpv_("TKFRAM", item + 64, "=", &c__9, &c__1, "N", (ftnlen)
		    6, (ftnlen)32, (ftnlen)1, (ftnlen)1)) {
		lnkini_(&c__200, pool);
		*frame = 0;
		ident_(rot);
		chkout_("TKFRAM", (ftnlen)6);
		return 0;
	    }

/*           The variable meets current expectations, look it up */
/*           from the kernel pool. */

	    gdpool_(item + 64, &c__1, &c__9, &n, matrix, &fnd, (ftnlen)32);

/*           In this case the full transformation matrix has been */
/*           specified.  We simply polish it up a bit. */

	    moved_(matrix, &c__9, rot);
	    sharpr_(rot);

/*           The matrix might not be right-handed, so correct */
/*           the sense of the second and third columns if necessary. */

	    if (vdot_(&rot[3], &matrix[3]) < 0.) {
		vsclip_(&c_b120, &rot[3]);
	    }
	    if (vdot_(&rot[6], &matrix[6]) < 0.) {
		vsclip_(&c_b120, &rot[6]);
	    }
	} else if (s_cmp(spec, "ANGLES", (ftnlen)32, (ftnlen)6) == 0) {

/*           Look up the angles, their units and axes for the */
/*           frame specified by ID. (Note that UNITS are optional). */
/*           As in the previous case we give preference to the */
/*           form TKFRAME_<name>_<item> over TKFRAME_<id>_<item>. */

/* Writing concatenation */
	    i__2[0] = r__, a__1[0] = agent;
	    i__2[1] = 7, a__1[1] = "_ANGLES";
	    s_cat(item + 64, a__1, i__2, &c__2, (ftnlen)32);
/* Writing concatenation */
	    i__2[0] = r__, a__1[0] = agent;
	    i__2[1] = 5, a__1[1] = "_AXES";
	    s_cat(item + 96, a__1, i__2, &c__2, (ftnlen)32);
/* Writing concatenation */
	    i__2[0] = r__, a__1[0] = agent;
	    i__2[1] = 6, a__1[1] = "_UNITS";
	    s_cat(item + 128, a__1, i__2, &c__2, (ftnlen)32);
/* Writing concatenation */
	    i__2[0] = ar, a__1[0] = altnat;
	    i__2[1] = 7, a__1[1] = "_ANGLES";
	    s_cat(alt + 64, a__1, i__2, &c__2, (ftnlen)32);
/* Writing concatenation */
	    i__2[0] = ar, a__1[0] = altnat;
	    i__2[1] = 5, a__1[1] = "_AXES";
	    s_cat(alt + 96, a__1, i__2, &c__2, (ftnlen)32);
/* Writing concatenation */
	    i__2[0] = ar, a__1[0] = altnat;
	    i__2[1] = 6, a__1[1] = "_UNITS";
	    s_cat(alt + 128, a__1, i__2, &c__2, (ftnlen)32);

/*           Again, we give preference to the more friendly form */
/*           of TKFRAME specification. */

/*           If both forms are present, we signal an error. */

	    for (i__ = 3; i__ <= 5; ++i__) {
		dtpool_(item + (((i__1 = i__ - 1) < 14 && 0 <= i__1 ? i__1 : 
			s_rnge("item", i__1, "tkfram_", (ftnlen)892)) << 5), &
			found1, &n, type__, (ftnlen)32, (ftnlen)1);
		dtpool_(alt + (((i__1 = i__ - 1) < 14 && 0 <= i__1 ? i__1 : 
			s_rnge("alt", i__1, "tkfram_", (ftnlen)893)) << 5), &
			found2, &n, type__, (ftnlen)32, (ftnlen)1);
		if (found1 && found2) {
		    lnkini_(&c__200, pool);
		    *frame = 0;
		    ident_(rot);
		    setmsg_("Frame name-based and frame ID-based text kernel"
			    " (fixed-offset) frame definition keywords '#' an"
			    "d '#' are both present in the POOL. Most likely "
			    "this is because loaded text kernels contain comp"
			    "eting definitions of the '#' frame using differe"
			    "nt keyword styles, which is not allowed. ", (
			    ftnlen)280);
		    errch_("#", item + (((i__1 = i__ - 1) < 14 && 0 <= i__1 ? 
			    i__1 : s_rnge("item", i__1, "tkfram_", (ftnlen)
			    910)) << 5), (ftnlen)1, (ftnlen)32);
		    errch_("#", alt + (((i__1 = i__ - 1) < 14 && 0 <= i__1 ? 
			    i__1 : s_rnge("alt", i__1, "tkfram_", (ftnlen)911)
			    ) << 5), (ftnlen)1, (ftnlen)32);
		    errch_("#", frname, (ftnlen)1, (ftnlen)32);
		    sigerr_("SPICE(COMPETINGFRAMESPEC)", (ftnlen)25);
		    chkout_("TKFRAM", (ftnlen)6);
		    return 0;
		}
		if (found2) {
		    s_copy(item + (((i__1 = i__ - 1) < 14 && 0 <= i__1 ? i__1 
			    : s_rnge("item", i__1, "tkfram_", (ftnlen)919)) <<
			     5), alt + (((i__3 = i__ - 1) < 14 && 0 <= i__3 ? 
			    i__3 : s_rnge("alt", i__3, "tkfram_", (ftnlen)919)
			    ) << 5), (ftnlen)32, (ftnlen)32);
		}
	    }
	    if (badkpv_("TKFRAM", item + 64, "=", &c__3, &c__1, "N", (ftnlen)
		    6, (ftnlen)32, (ftnlen)1, (ftnlen)1) || badkpv_("TKFRAM", 
		    item + 96, "=", &c__3, &c__1, "N", (ftnlen)6, (ftnlen)32, 
		    (ftnlen)1, (ftnlen)1)) {
		lnkini_(&c__200, pool);
		*frame = 0;
		ident_(rot);
		chkout_("TKFRAM", (ftnlen)6);
		return 0;
	    }
	    s_copy(units, "RADIANS", (ftnlen)32, (ftnlen)7);
	    gdpool_(item + 64, &c__1, &c__3, &n, angles, &fnd, (ftnlen)32);
	    gipool_(item + 96, &c__1, &c__3, &n, axes, &fnd, (ftnlen)32);
	    gcpool_(item + 128, &c__1, &c__1, &n, units, &fnd, (ftnlen)32, (
		    ftnlen)32);

/*           Convert angles to radians. */

	    for (i__ = 1; i__ <= 3; ++i__) {
		convrt_(&angles[(i__1 = i__ - 1) < 3 && 0 <= i__1 ? i__1 : 
			s_rnge("angles", i__1, "tkfram_", (ftnlen)948)], 
			units, "RADIANS", &tempd, (ftnlen)32, (ftnlen)7);
		angles[(i__1 = i__ - 1) < 3 && 0 <= i__1 ? i__1 : s_rnge(
			"angles", i__1, "tkfram_", (ftnlen)949)] = tempd;
	    }

/*           Compute the rotation from instrument frame to CK frame. */

	    eul2m_(angles, &angles[1], &angles[2], axes, &axes[1], &axes[2], 
		    rot);
	    if (failed_()) {
		lnkini_(&c__200, pool);
		*frame = 0;
		ident_(rot);
		chkout_("TKFRAM", (ftnlen)6);
		return 0;
	    }
	} else if (s_cmp(spec, "QUATERNION", (ftnlen)32, (ftnlen)10) == 0) {

/*           Look up the quaternion and convert it to a rotation */
/*           matrix. Again there are two possible variables that */
/*           may point to the quaternion. We give preference to */
/*           the form TKFRAME_<name>_Q over the form TKFRAME_<id>_Q. */

/*           If both forms are present, we signal an error. */

/* Writing concatenation */
	    i__2[0] = r__, a__1[0] = agent;
	    i__2[1] = 2, a__1[1] = "_Q";
	    s_cat(item + 64, a__1, i__2, &c__2, (ftnlen)32);
/* Writing concatenation */
	    i__2[0] = ar, a__1[0] = altnat;
	    i__2[1] = 2, a__1[1] = "_Q";
	    s_cat(alt + 64, a__1, i__2, &c__2, (ftnlen)32);
	    dtpool_(item + 64, &found1, &n, type__, (ftnlen)32, (ftnlen)1);
	    dtpool_(alt + 64, &found2, &n, type__, (ftnlen)32, (ftnlen)1);
	    if (found1 && found2) {
		lnkini_(&c__200, pool);
		*frame = 0;
		ident_(rot);
		setmsg_("Frame name-based and frame ID-based text kernel (fi"
			"xed-offset) frame definition keywords '#' and '#' ar"
			"e both present in the POOL. Most likely this is beca"
			"use loaded text kernels contain competing definition"
			"s of the '#' frame using different keyword styles, w"
			"hich is not allowed. ", (ftnlen)280);
		errch_("#", item + 64, (ftnlen)1, (ftnlen)32);
		errch_("#", alt + 64, (ftnlen)1, (ftnlen)32);
		errch_("#", frname, (ftnlen)1, (ftnlen)32);
		sigerr_("SPICE(COMPETINGFRAMESPEC)", (ftnlen)25);
		chkout_("TKFRAM", (ftnlen)6);
		return 0;
	    }
	    if (found2) {
		s_copy(item + 64, alt + 64, (ftnlen)32, (ftnlen)32);
	    }
	    if (badkpv_("TKFRAM", item + 64, "=", &c__4, &c__1, "N", (ftnlen)
		    6, (ftnlen)32, (ftnlen)1, (ftnlen)1)) {
		lnkini_(&c__200, pool);
		*frame = 0;
		ident_(rot);
		chkout_("TKFRAM", (ftnlen)6);
		return 0;
	    }

/*           In this case we have the quaternion representation. */
/*           Again, we do a small amount of polishing of the input. */

	    gdpool_(item + 64, &c__1, &c__4, &n, quatrn, &fnd, (ftnlen)32);
	    vhatg_(quatrn, &c__4, qtmp);
	    q2m_(qtmp, rot);
	} else {

/*           We don't recognize the SPEC for this frame.  Say */
/*           so.  Also note that perhaps the user needs to upgrade */
/*           the toolkit. */

	    lnkini_(&c__200, pool);
	    *frame = 0;
	    ident_(rot);
	    setmsg_("The frame specification \"# = '#'\" is not one of the r"
		    "ecognized means of specifying a text-kernel constant off"
		    "set frame. This may reflect a typographical error or may"
		    " indicate that you need to consider updating your versio"
		    "n of the SPICE toolkit. ", (ftnlen)245);
	    errch_("#", item, (ftnlen)1, (ftnlen)32);
	    errch_("#", spec, (ftnlen)1, (ftnlen)32);
	    sigerr_("SPICE(UNKNOWNFRAMESPEC)", (ftnlen)23);
	    chkout_("TKFRAM", (ftnlen)6);
	    return 0;
	}

/*        Buffer the identifier, relative frame and rotation matrix. */

	buffd[(i__1 = at * 9 - 9) < 1800 && 0 <= i__1 ? i__1 : s_rnge("buffd",
		 i__1, "tkfram_", (ftnlen)1054)] = rot[0];
	buffd[(i__1 = at * 9 - 8) < 1800 && 0 <= i__1 ? i__1 : s_rnge("buffd",
		 i__1, "tkfram_", (ftnlen)1055)] = rot[1];
	buffd[(i__1 = at * 9 - 7) < 1800 && 0 <= i__1 ? i__1 : s_rnge("buffd",
		 i__1, "tkfram_", (ftnlen)1056)] = rot[2];
	buffd[(i__1 = at * 9 - 6) < 1800 && 0 <= i__1 ? i__1 : s_rnge("buffd",
		 i__1, "tkfram_", (ftnlen)1057)] = rot[3];
	buffd[(i__1 = at * 9 - 5) < 1800 && 0 <= i__1 ? i__1 : s_rnge("buffd",
		 i__1, "tkfram_", (ftnlen)1058)] = rot[4];
	buffd[(i__1 = at * 9 - 4) < 1800 && 0 <= i__1 ? i__1 : s_rnge("buffd",
		 i__1, "tkfram_", (ftnlen)1059)] = rot[5];
	buffd[(i__1 = at * 9 - 3) < 1800 && 0 <= i__1 ? i__1 : s_rnge("buffd",
		 i__1, "tkfram_", (ftnlen)1060)] = rot[6];
	buffd[(i__1 = at * 9 - 2) < 1800 && 0 <= i__1 ? i__1 : s_rnge("buffd",
		 i__1, "tkfram_", (ftnlen)1061)] = rot[7];
	buffd[(i__1 = at * 9 - 1) < 1800 && 0 <= i__1 ? i__1 : s_rnge("buffd",
		 i__1, "tkfram_", (ftnlen)1062)] = rot[8];
	buffi[(i__1 = at - 1) < 200 && 0 <= i__1 ? i__1 : s_rnge("buffi", 
		i__1, "tkfram_", (ftnlen)1064)] = *frame;

/*        If these were not previously buffered, we need to set */
/*        a watch on the various items that might be used to define */
/*        this frame. */

	if (! buffrd) {

/*           Immediately check for an update so that we will */
/*           not redundantly look for this item the next time this */
/*           routine is called. */

/* Writing concatenation */
	    i__2[0] = r__, a__1[0] = agent;
	    i__2[1] = 9, a__1[1] = "_RELATIVE";
	    s_cat(item, a__1, i__2, &c__2, (ftnlen)32);
/* Writing concatenation */
	    i__2[0] = r__, a__1[0] = agent;
	    i__2[1] = 5, a__1[1] = "_SPEC";
	    s_cat(item + 32, a__1, i__2, &c__2, (ftnlen)32);
/* Writing concatenation */
	    i__2[0] = r__, a__1[0] = agent;
	    i__2[1] = 5, a__1[1] = "_AXES";
	    s_cat(item + 64, a__1, i__2, &c__2, (ftnlen)32);
/* Writing concatenation */
	    i__2[0] = r__, a__1[0] = agent;
	    i__2[1] = 7, a__1[1] = "_MATRIX";
	    s_cat(item + 96, a__1, i__2, &c__2, (ftnlen)32);
/* Writing concatenation */
	    i__2[0] = r__, a__1[0] = agent;
	    i__2[1] = 2, a__1[1] = "_Q";
	    s_cat(item + 128, a__1, i__2, &c__2, (ftnlen)32);
/* Writing concatenation */
	    i__2[0] = r__, a__1[0] = agent;
	    i__2[1] = 7, a__1[1] = "_ANGLES";
	    s_cat(item + 160, a__1, i__2, &c__2, (ftnlen)32);
/* Writing concatenation */
	    i__2[0] = r__, a__1[0] = agent;
	    i__2[1] = 6, a__1[1] = "_UNITS";
	    s_cat(item + 192, a__1, i__2, &c__2, (ftnlen)32);
/* Writing concatenation */
	    i__2[0] = ar, a__1[0] = altnat;
	    i__2[1] = 9, a__1[1] = "_RELATIVE";
	    s_cat(item + 224, a__1, i__2, &c__2, (ftnlen)32);
/* Writing concatenation */
	    i__2[0] = ar, a__1[0] = altnat;
	    i__2[1] = 5, a__1[1] = "_SPEC";
	    s_cat(item + 256, a__1, i__2, &c__2, (ftnlen)32);
/* Writing concatenation */
	    i__2[0] = ar, a__1[0] = altnat;
	    i__2[1] = 5, a__1[1] = "_AXES";
	    s_cat(item + 288, a__1, i__2, &c__2, (ftnlen)32);
/* Writing concatenation */
	    i__2[0] = ar, a__1[0] = altnat;
	    i__2[1] = 7, a__1[1] = "_MATRIX";
	    s_cat(item + 320, a__1, i__2, &c__2, (ftnlen)32);
/* Writing concatenation */
	    i__2[0] = ar, a__1[0] = altnat;
	    i__2[1] = 2, a__1[1] = "_Q";
	    s_cat(item + 352, a__1, i__2, &c__2, (ftnlen)32);
/* Writing concatenation */
	    i__2[0] = ar, a__1[0] = altnat;
	    i__2[1] = 7, a__1[1] = "_ANGLES";
	    s_cat(item + 384, a__1, i__2, &c__2, (ftnlen)32);
/* Writing concatenation */
	    i__2[0] = ar, a__1[0] = altnat;
	    i__2[1] = 6, a__1[1] = "_UNITS";
	    s_cat(item + 416, a__1, i__2, &c__2, (ftnlen)32);
	    swpool_(agent, &c__14, item, (ftnlen)32, (ftnlen)32);
	    cvpool_(agent, &update, (ftnlen)32);
	}
    }
    if (failed_()) {
	lnkini_(&c__200, pool);
	*frame = 0;
	ident_(rot);
	chkout_("TKFRAM", (ftnlen)6);
	return 0;
    }

/*     All errors cause the routine to exit before we get to this */
/*     point.  If we reach this point we didn't have an error and */
/*     hence did find the rotation from ID to FRAME. */

    *found = TRUE_;

/*     That's it */

    chkout_("TKFRAM", (ftnlen)6);
    return 0;
} /* tkfram_ */

