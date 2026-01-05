/* zzdynrt0.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__2 = 2;
static integer c__9 = 9;
static integer c__36 = 36;
static integer c__1 = 1;
static integer c__0 = 0;
static integer c__3 = 3;
static doublereal c_b194 = 0.;
static integer c__6 = 6;
static doublereal c_b372 = 1.;
static integer c__20 = 20;
static integer c__10 = 10;

/* $Procedure ZZDYNRT0 ( Dynamic position transformation evaluation ) */
/* Subroutine */ int zzdynrt0_(integer *infram, integer *center, doublereal *
	et, doublereal *rotate, integer *basfrm)
{
    /* Initialized data */

    static char axes[1*3] = "X" "Y" "Z";
    static logical first = TRUE_;
    static char itmcof[32*3] = "ANGLE_1_COEFFS                  " "ANGLE_2_C"
	    "OEFFS                  " "ANGLE_3_COEFFS                  ";
    static char itmsep[32] = "ANGLE_SEP_TOL                   ";
    static char vname[4*2] = "PRI_" "SEC_";

    /* System generated locals */
    address a__1[2];
    integer i__1, i__2, i__3[2];

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer);
    /* Subroutine */ int s_cat(char *, char **, integer *, integer *, ftnlen);
    integer s_cmp(char *, char *, ftnlen, ftnlen);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    double sin(doublereal);

    /* Local variables */
    extern /* Subroutine */ int zzrefch1_(integer *, integer *, doublereal *, 
	    doublereal *);
    doublereal dmob;
    integer degs[3], frid;
    char spec[80];
    integer targ;
    doublereal oblr[9]	/* was [3][3] */;
    integer toid;
    extern /* Subroutine */ int vhat_(doublereal *, doublereal *);
    doublereal pobs[3];
    integer axis[2];
    extern /* Subroutine */ int zzspksb1_(integer *, doublereal *, char *, 
	    doublereal *, ftnlen);
    doublereal tipm[9]	/* was [3][3] */, vflt;
    extern doublereal vsep_(doublereal *, doublereal *);
    doublereal rinv[9]	/* was [3][3] */;
    extern /* Subroutine */ int zzspkez0_(integer *, doublereal *, char *, 
	    char *, integer *, doublereal *, doublereal *, ftnlen, ftnlen), 
	    vsub_(doublereal *, doublereal *, doublereal *), vequ_(doublereal 
	    *, doublereal *);
    doublereal poly[2], rnut[9]	/* was [3][3] */, rout[9]	/* was [3][3] 
	    */;
    extern /* Subroutine */ int zzspkzp1_(integer *, doublereal *, char *, 
	    char *, integer *, doublereal *, doublereal *, ftnlen, ftnlen), 
	    zzdynbid_(char *, integer *, char *, integer *, ftnlen, ftnlen), 
	    zzgftreb_(integer *, doublereal *), zzdynfid_(char *, integer *, 
	    char *, integer *, ftnlen, ftnlen), zzdynoad_(char *, integer *, 
	    char *, integer *, integer *, doublereal *, logical *, ftnlen, 
	    ftnlen), zzdynoac_(char *, integer *, char *, integer *, integer *
	    , char *, logical *, ftnlen, ftnlen, ftnlen), eul2m_(doublereal *,
	     doublereal *, doublereal *, integer *, integer *, integer *, 
	    doublereal *), zzcorepc_(char *, doublereal *, doublereal *, 
	    doublereal *, ftnlen), zzmobliq_(doublereal *, doublereal *, 
	    doublereal *), zzdynvac_(char *, integer *, char *, integer *, 
	    integer *, char *, ftnlen, ftnlen, ftnlen), zzdynvad_(char *, 
	    integer *, char *, integer *, integer *, doublereal *, ftnlen, 
	    ftnlen), zzvalcor_(char *, logical *, ftnlen), zzdynvai_(char *, 
	    integer *, char *, integer *, integer *, integer *, ftnlen, 
	    ftnlen);
    integer i__;
    extern /* Subroutine */ int zzprscor_(char *, logical *, ftnlen);
    integer m, n, frcid;
    doublereal radii[3], delta;
    extern /* Subroutine */ int etcal_(doublereal *, char *, ftnlen), chkin_(
	    char *, ftnlen);
    doublereal epoch;
    extern /* Subroutine */ int ucase_(char *, char *, ftnlen, ftnlen);
    static integer earth;
    extern /* Subroutine */ int errch_(char *, char *, ftnlen, ftnlen);
    doublereal pnear[3];
    integer frcls, iaxes[3];
    doublereal rprec[9]	/* was [3][3] */;
    static char itmra[32*2];
    integer cvobs, frctr;
    extern /* Subroutine */ int moved_(doublereal *, integer *, doublereal *),
	     errdp_(char *, doublereal *, ftnlen);
    doublereal ptemp[3], rtemp[9]	/* was [3][3] */, stemp[6], stobs[6];
    extern logical eqstr_(char *, char *, ftnlen, ftnlen);
    extern /* Subroutine */ int xpose_(doublereal *, doublereal *);
    char units[80];
    doublereal nutxf[36]	/* was [6][6] */, t0;
    extern /* Subroutine */ int bodn2c_(char *, integer *, logical *, ftnlen);
    doublereal v2[6]	/* was [3][2] */;
    extern /* Subroutine */ int bodc2n_(integer *, char *, logical *, ftnlen);
    doublereal ra;
    extern logical failed_(void);
    logical meanec;
    extern /* Subroutine */ int cleard_(integer *, doublereal *);
    char vecdef[80*2];
    static char itmabc[32*2];
    char basnam[32];
    doublereal lt;
    logical negate;
    static char itmdec[32*2];
    doublereal coeffs[60]	/* was [20][3] */;
    char inname__[32], abcorr[5], axname[80];
    extern integer isrchc_(char *, integer *, char *, ftnlen, ftnlen);
    extern logical return_(void);
    char cfrmnm[32], ctrnam[36], cvcorr[5], dynstl[80], dynfam[80], fframs[32*
	    10];
    static char itmaxe[32*2], itmfrm[32*2], itmlat[32*2], itmlon[32*2], 
	    itmobs[32*2], itmspc[32*2], itmtrg[32*2], itmunt[32*2], itmvdf[32*
	    2], itmvec[32*2];
    char nutmod[80], oblmod[80], prcmod[80], rotsta[80], tframs[32*10], 
	    timstr[50], tmpfam[80], velfrm[32];
    doublereal alt, angles[2], ctrpos[3], dec, dirvec[3], eulang[3], fet, lat,
	     lon, minsep, mob, precxf[36]	/* was [6][6] */, r2000[9]	
	    /* was [3][3] */, sep, vet;
    integer cfrmid, fromid;
    static integer j2000;
    integer obs;
    logical corblk[15], fnd, frozen, meaneq, ofdate, trueeq;
    extern /* Subroutine */ int irfnum_(char *, integer *, ftnlen), frmnam_(
	    integer *, char *, ftnlen), chkout_(char *, ftnlen), cmprss_(char 
	    *, integer *, char *, char *, ftnlen, ftnlen, ftnlen), setmsg_(
	    char *, ftnlen), sigerr_(char *, ftnlen), intstr_(integer *, char 
	    *, ftnlen), mxm_(doublereal *, doublereal *, doublereal *), 
	    errint_(char *, integer *, ftnlen), frinfo_(integer *, integer *, 
	    integer *, integer *, logical *), mxv_(doublereal *, doublereal *,
	     doublereal *), cidfrm_(integer *, integer *, char *, logical *, 
	    ftnlen), vminus_(doublereal *, doublereal *), nearpt_(doublereal *
	    , doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *), convrt_(doublereal *, char *, char *, doublereal *,
	     ftnlen, ftnlen), latrec_(doublereal *, doublereal *, doublereal *
	    , doublereal *), stlabx_(doublereal *, doublereal *, doublereal *)
	    , stelab_(doublereal *, doublereal *, doublereal *), twovec_(
	    doublereal *, integer *, doublereal *, integer *, doublereal *), 
	    polyds_(doublereal *, integer *, integer *, doublereal *, 
	    doublereal *), namfrm_(char *, integer *, ftnlen), zzeprc76_(
	    doublereal *, doublereal *), zzenut80_(doublereal *, doublereal *)
	    ;

/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines.  Users should not call this routine directly due */
/*     to the volatile nature of this routine. */

/*     For a specified dynamic frame, find the rotation that maps */
/*     positions from the dynamic frame to its base frame. */

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

/*     CK */
/*     FRAMES */
/*     PCK */
/*     SPK */

/* $ Keywords */

/*     FRAMES */
/*     PRIVATE */

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

/*     Include file zzabcorr.inc */

/*     SPICE private file intended solely for the support of SPICE */
/*     routines.  Users should not include this file directly due */
/*     to the volatile nature of this file */

/*     The parameters below define the structure of an aberration */
/*     correction attribute block. */

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

/*     An aberration correction attribute block is an array of logical */
/*     flags indicating the attributes of the aberration correction */
/*     specified by an aberration correction string.  The attributes */
/*     are: */

/*        - Is the correction "geometric"? */

/*        - Is light time correction indicated? */

/*        - Is stellar aberration correction indicated? */

/*        - Is the light time correction of the "converged */
/*          Newtonian" variety? */

/*        - Is the correction for the transmission case? */

/*        - Is the correction relativistic? */

/*    The parameters defining the structure of the block are as */
/*    follows: */

/*       NABCOR    Number of aberration correction choices. */

/*       ABATSZ    Number of elements in the aberration correction */
/*                 block. */

/*       GEOIDX    Index in block of geometric correction flag. */

/*       LTIDX     Index of light time flag. */

/*       STLIDX    Index of stellar aberration flag. */

/*       CNVIDX    Index of converged Newtonian flag. */

/*       XMTIDX    Index of transmission flag. */

/*       RELIDX    Index of relativistic flag. */

/*    The following parameter is not required to define the block */
/*    structure, but it is convenient to include it here: */

/*       CORLEN    The maximum string length required by any aberration */
/*                 correction string */

/* $ Author_and_Institution */

/*     N.J. Bachman    (JPL) */

/* $ Literature_References */

/*     None. */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 18-DEC-2004 (NJB) */

/* -& */
/*     Number of aberration correction choices: */


/*     Aberration correction attribute block size */
/*     (number of aberration correction attributes): */


/*     Indices of attributes within an aberration correction */
/*     attribute block: */


/*     Maximum length of an aberration correction string: */


/*     End of include file zzabcorr.inc */

/* $ Abstract */

/*     Include file zzdyn.inc */

/*     SPICE private file intended solely for the support of SPICE */
/*     routines.  Users should not include this file directly due */
/*     to the volatile nature of this file */

/*     The parameters defined below are used by the SPICELIB dynamic */
/*     frame subsystem. */

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

/*     This file declares parameters required by the dynamic */
/*     frame routines of the SPICELIB frame subsystem. */

/* $ Restrictions */

/*     The parameter BDNMLN is this routine must be kept */
/*     consistent with the parameter MAXL defined in */

/*        zzbodtrn.inc */


/* $ Author_and_Institution */

/*     N.J. Bachman    (JPL) */

/* $ Literature_References */

/*     None. */

/* $ Version */

/* -    SPICELIB Version 2.0.0, 22-SEP-2020 (NJB) */

/*        Updated to support the product frame family. */

/* -    SPICELIB Version 1.1.0, 12-JAN-2005 (NJB) */

/*        Parameters KWX, KWY, KWZ renamed to KVX, KVY, KVZ. */

/* -    SPICELIB Version 1.0.0, 22-DEC-2004 (NJB) */

/* -& */

/*     String length parameters */
/*     ======================== */


/*     Kernel variable name length.  This parameter must be */
/*     kept consistent with the parameter MAXLEN used in the */
/*     POOL umbrella routine. */


/*     Length of a character kernel pool datum. This parameter must be */
/*     kept consistent with the parameter MAXCHR used in the POOL */
/*     umbrella routine. */


/*     Reference frame name length.  This parameter must be */
/*     kept consistent with the parameter WDSIZE used in the */
/*     FRAMEX umbrella routine. */


/*     Body name length.  This parameter is used to provide a level */
/*     of indirection so the dynamic frame source code doesn't */
/*     have to change if the name of this SPICELIB-scope parameter */
/*     is changed.  The value MAXL used here is defined in the */
/*     INCLUDE file */

/*        zzbodtrn.inc */

/*     Current value of MAXL = 36 */


/*     Numeric parameters */
/*     =================================== */

/*     The parameter MAXCOF is the maximum number of polynomial */
/*     coefficients that may be used to define an Euler angle */
/*     in an "Euler frame" definition */


/*     The parameter MXNFAC is the maximum number of factors in */
/*     a product frame. */


/*     The parameter LBSEP is the default angular separation limit for */
/*     the vectors defining a two-vector frame.  The angular separation */
/*     of the vectors must differ from Pi and 0 by at least this amount. */


/*     The parameter QEXP is used to determine the width of */
/*     the interval DELTA used for the discrete differentiation */
/*     of velocity in the routines ZZDYNFRM, ZZDYNROT, and their */
/*     recursive analogs.  This parameter is appropriate for */
/*     64-bit IEEE double precision numbers; when SPICELIB */
/*     is hosted on platforms where longer mantissas are supported, */
/*     this parameter (and hence this INCLUDE file) will become */
/*     platform-dependent. */

/*     The choice of QEXP is based on heuristics.  It's believed to */
/*     be a reasonable choice obtainable without expensive computation. */

/*     QEXP is the largest power of 2 such that */

/*        1.D0 + 2**QEXP  =  1.D0 */

/*     Given an epoch T0 at which a discrete derivative is to be */
/*     computed, this choice provides a value of DELTA that usually */
/*     contributes no round-off error in the computation of the function */
/*     evaluation epochs */

/*        T0 +/- DELTA */

/*     while providing the largest value of DELTA having this form that */
/*     causes the order of the error term O(DELTA**2) in the quadratic */
/*     function approximation to round to zero.  Note that the error */
/*     itself will normally be small but doesn't necessarily round to */
/*     zero.  Note also that the small function approximation error */
/*     is not a measurement of the error in the discrete derivative */
/*     itself. */

/*     For ET values T0 > 2**27 seconds past J2000, the value of */
/*     DELTA will be set to */

/*        T0 * 2**QEXP */

/*     For smaller values of T0, DELTA should be set to 1.D0. */


/*     Frame kernel parameters */
/*     ======================= */

/*     Parameters relating to kernel variable names (keywords) start */
/*     with the letters */

/*        KW */

/*     Parameters relating to kernel variable values start with the */
/*     letters */

/*        KV */


/*     Generic parameters */
/*     --------------------------------- */

/*     Token used to build the base frame keyword: */


/*     Frame definition style parameters */
/*     --------------------------------- */

/*     Token used to build the frame definition style keyword: */


/*     Token indicating parameterized dynamic frame. */


/*     Freeze epoch parameters */
/*     --------------------------------- */

/*     Token used to build the freeze epoch keyword: */


/*     Rotation state parameters */
/*     --------------------------------- */

/*     Token used to build the rotation state keyword: */


/*     Token indicating rotating rotation state: */


/*     Token indicating inertial rotation state: */


/*     Frame family parameters */
/*     --------------------------------- */

/*     Token used to build the frame family keyword: */


/*     Token indicating mean equator and equinox of date frame. */


/*     Token indicating mean ecliptic and equinox of date frame. */


/*     Token indicating true equator and equinox of date frame. */


/*     Token indicating two-vector frame. */


/*     Token indicating Euler frame. */


/*     Token indicating product frame. */


/*     "Of date" frame family parameters */
/*     --------------------------------- */

/*     Token used to build the precession model keyword: */


/*     Token used to build the nutation model keyword: */


/*     Token used to build the obliquity model keyword: */


/*     Mathematical models used to define "of date" frames will */
/*     likely accrue over time.  We will simply assign them */
/*     numbers. */


/*     Token indicating the Lieske earth precession model: */


/*     Token indicating the IAU 1980 earth nutation model: */


/*     Token indicating the IAU 1980 earth mean obliqity of */
/*     date model.  Note the name matches that of the preceding */
/*     nutation model---this is intentional.  The keyword */
/*     used in the kernel variable definition indicates what */
/*     kind of model is being defined. */


/*     Two-vector frame family parameters */
/*     --------------------------------- */

/*     Token used to build the vector axis keyword: */


/*     Tokens indicating axis values: */


/*     Prefixes used for primary and secondary vector definition */
/*     keywords: */


/*     Token used to build the vector definition keyword: */


/*     Token indicating observer-target position vector: */


/*     Token indicating observer-target velocity vector: */


/*     Token indicating observer-target near point vector: */


/*     Token indicating constant vector: */


/*     Token used to build the vector observer keyword: */


/*     Token used to build the vector target keyword: */


/*     Token used to build the vector frame keyword: */


/*     Token used to build the vector aberration correction keyword: */


/*     Token used to build the constant vector specification keyword: */


/*     Token indicating rectangular coordinates used to */
/*     specify constant vector: */


/*     Token indicating latitudinal coordinates used to */
/*     specify constant vector: */


/*     Token indicating RA/DEC coordinates used to */
/*     specify constant vector: */


/*     Token used to build the cartesian vector literal keyword: */


/*     Token used to build the constant vector latitude keyword: */


/*     Token used to build the constant vector longitude keyword: */


/*     Token used to build the constant vector right ascension keyword: */


/*     Token used to build the constant vector declination keyword: */


/*     Token used to build the angular separation tolerance keyword: */


/*     See the section "Physical unit parameters" below for additional */
/*     parameters applicable to two-vector frames. */


/*     Euler frame family parameters */
/*     --------------------------------- */

/*     Token used to build the epoch keyword: */


/*     Token used to build the Euler axis sequence keyword: */


/*     Tokens used to build the Euler angle coefficients keywords: */


/*     See the section "Physical unit parameters" below for additional */
/*     parameters applicable to Euler frames. */


/*     Product frame family parameters */
/*     --------------------------------- */


/*     Physical unit parameters */
/*     --------------------------------- */

/*     Token used to build the units keyword: */


/*     Token indicating radians: */


/*     Token indicating degrees: */


/*     End of include file zzdyn.inc */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     INFRAM     I   Class ID code for a SPICE dynamic reference frame. */
/*     CENTER     I   ID code for the center of the input frame. */
/*     ET         I   An epoch in seconds past J2000 TDB. */
/*     ROTATE     O   The requested rotation matrix. */
/*     BASFRM     O   Frame ID of base frame associated with INFRAM. */

/* $ Detailed_Input */

/*     INFRAM      is the frame ID code for a dynamic reference frame. */
/*                 Note that this interface differs from that of TKFRAM, */
/*                 which uses a class ID to identify the frame. */

/*                 In this routine, we refer this frame both as the */
/*                 "input frame" and the "defined frame." */

/*     CENTER      is NAIF ID code for the center of the frame */
/*                 designated by INFRAM.  This code, although derivable */
/*                 from INFRAM, is passed in for convenience. */

/*     ET          is an epoch in ephemeris seconds past J2000 for which */
/*                 the caller requests a rotation matrix. */

/* $ Detailed_Output */

/*     ROTATE      is a 3x3 rotation matrix that transforms positions */
/*                 relative to INFRAM to positions relative to BASFRM. */

/*     BASFRM      is the frame ID code of the base frame associated */
/*                 with INFRAM.  The 3x3 matrix ROTATE transforms */
/*                 positions relative to INFRAM to positions relative to */
/*                 BASFRM. The position transformation is performed by */
/*                 left-multiplying by ROTATE a position expressed */
/*                 relative to INFRAM. This is easily accomplished via */
/*                 the subroutine call shown below. */

/*                    CALL MXV ( ROTATE, INPOS, OUTPOS ) */

/* $ Parameters */

/*     See include file zzdyn.inc. */

/* $ Exceptions */

/*     1)  If a dynamic frame evaluation requires unavailable kernel */
/*         data, the error will be diagnosed by routines in the */
/*         call tree of this routine. */

/*     2)  If a precession model is used to implement a frame centered */
/*         at a body for which the model is not applicable, the error */
/*         SPICE(INVALIDSELECTION) will be signaled. */

/*     3)  If a nutation model is used to implement a frame centered */
/*         at a body for which the model is not applicable, the error */
/*         SPICE(INVALIDSELECTION) will be signaled. */

/*     4)  If an obliquity model is used to implement a frame centered */
/*         at a body for which the model is not applicable, the error */
/*         SPICE(INVALIDSELECTION) will be signaled. */

/*     5)  If an unrecognized precession model is specified, the */
/*         error SPICE(NOTSUPPORTED) is signaled. */

/*     6)  If an unrecognized nutation model is specified, the */
/*         error SPICE(NOTSUPPORTED) is signaled. */

/*     7)  If an unrecognized obliquity model is specified, the */
/*         error SPICE(NOTSUPPORTED) is signaled. */

/*     8)  If an attempt to look up the center of a frame does */
/*         not yield data, the error SPICE(FRAMEDATANOTFOUND) is */
/*         signaled. */

/*     9)  In a two-vector frame definition, if a constant vector */
/*         specification method is not recognized, the error */
/*         SPICE(NOTSUPPORTED) is signaled. */

/*     10) In a two-vector frame definition, if a vector definition */
/*         method is not recognized, the error SPICE(NOTSUPPORTED) */
/*         is signaled. */

/*     11) If an unrecognized dynamic frame family is specified, the */
/*          error SPICE(NOTSUPPORTED) is signaled. */

/*     12) If an unrecognized dynamic frame definition style is */
/*         specified, the error SPICE(NOTSUPPORTED) is signaled. */

/*     13) If an unrecognized dynamic frame rotation state is */
/*         specified, the error SPICE(NOTSUPPORTED) is signaled. */

/*     14) If both a freeze epoch and a rotation state are specified, */
/*         the error SPICE(FRAMEDEFERROR) is signaled. */

/*     15) If neither a freeze epoch nor a rotation state are specified */
/*         for an "of date" frame, the error SPICE(FRAMEDEFERROR) is */
/*         signaled. */

/*     16) In a two-vector frame definition, if an invalid axis */
/*         specification is encountered, the error SPICE(INVALIDAXIS) is */
/*         signaled. */

/*     17) In a two-vector frame definition using a target near point */
/*         vector, if the body-fixed frame associated with the target */
/*         is not found, the error SPICE(FRAMEDATANOTFOUND) is signaled. */

/*     18) If a dynamic frame evaluation requires excessive recursion */
/*         depth, the error will be diagnosed by routines in the call */
/*         tree of this routine. */

/*     19) When a two-vector dynamic frame is evaluated, if the */
/*         primary and secondary vectors have angular separation less */
/*         than the minimum allowed value, or if the angular separation */
/*         differs from Pi by less than the minimum allowed value, the */
/*         error SPICE(DEGENERATECASE) is signaled.  The default minimum */
/*         separation is given by the parameter LBSEP; this value may be */
/*         overridden by supplying a different value in the frame */
/*         definition. */

/*     20) If invalid units occur in a frame definition, the error */
/*         will be diagnosed by a routine in the call tree of this */
/*         routine. */

/*     21) If an invalid Euler axis sequence occurs in a frame */
/*         definition, the error will be diagnosed by a routine in the */
/*         call tree of this routine. */

/*     22) If a body RADII vector has other than exactly thee elements, */
/*         the error SPICE(INVALIDCOUNT) is signaled by a routine in */
/*         the call tree of this routine. */

/*     23) If a body RADII vector has any element less-than or */
/*         equal to zero, the error SPICE(BADAXISLENGTH) is signaled by */
/*         a routine in the call tree of this routine. */

/* $ Files */

/*     1) SPK files containing data for each observer and target */
/*        are required to support two-vector frames.  Note that */
/*        observer-target pairs can be implicit, as in the case */
/*        of a constant vector whose frame is evaluated at a */
/*        light-time corrected epoch:  the light time the frame */
/*        center to an observer must be computable in this case, */
/*        which implies the state of the frame center as seen by */
/*        the observer must be computable. */

/*     2) Any of SPK, CK, PCK, and frame kernels will also be required */
/*        if any frames referenced in the definition of INFRAM (as a */
/*        base frame, velocity vector frame, or constant vector frame) */
/*        require them, or if any vectors used to define INFRAM require */
/*        these data in order to be computable. */

/*     3) When CK data are required, one or more associated SCLK kernels */
/*        ---normally, one kernel per spacecraft clock---are */
/*        required as well.  A leapseconds kernel may be required */
/*        whenever an SCLK kernel is required. */

/*     4) When a two-vector frame is defined using a target near point, */
/*        a PCK file giving orientation and providing a triaxial shape */
/*        model for the target body is required. */

/* $ Particulars */

/*     Currently only parameterized dynamic frames are supported by */
/*     this routine. */

/*     Currently supported parameterized dynamic families are: */

/*        Two-vector */
/*        ========== */

/*           Vector definitions */
/*           ------------------ */
/*           Observer-target position */
/*           Observer-target velocity */
/*           Near point on target */
/*           Constant vector in specified frame */


/*        Mean Equator and Equinox of Date */
/*        ================================ */

/*           Bodies and models */
/*           ----------------- */
/*           Earth:  1976 IAU precession model */


/*        Mean Ecliptic and Equinox of Date */
/*        ================================ */

/*           Bodies and models */
/*           ----------------- */
/*           Earth:  1976 IAU precession model */
/*                   1980 IAU mean obliquity model */


/*        True Equator and Equinox of Date */
/*        ================================ */

/*           Bodies and models */
/*           ----------------- */
/*           Earth:  1976 IAU precession model */
/*                   1980 IAU nutation model */


/*        Euler frames */
/*        ============ */

/*           Euler angle definitions */
/*           ----------------------- */
/*           Polynomial */


/*        Product frames */
/*        ============== */

/*           Product definitions */
/*           ------------------- */
/*           Array of "from" frames */
/*           Array of "to" frames */


/* $ Examples */

/*     See ROTGET. */

/* $ Restrictions */

/*     1) This is a SPICE private routine; the routine is subject */
/*        to change without notice.  User applications should not */
/*        call this routine. */

/*     2) Many numerical problems can occur when dynamic frames */
/*        are evaluated.  Users must determine whether dynamic frame */
/*        definitions are suitable for their applications. See the */
/*        Exceptions section for a list of possible problems. */

/*     3) Two-vector frame definitions can suffer extreme loss of */
/*         precision due to near-singular geometry. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman    (JPL) */

/* $ Version */

/* -    SPICELIB Version 2.0.0, 09-OCT-2021 (NJB) (EDW) */

/*        Support for product frames was added. */

/*        Bug fix: aberration correction strings for observer-target */
/*        velocity vectors are now parsed using ZZVALCOR. This improves */
/*        error checking. Note that aberration correction strings for */
/*        constant vectors used in two-vector frame specifications are */
/*        still parsed using ZZPRSCOR. */

/*        Code clean-up: unnecessary call to ZZPRSCOR was removed. */

/*        Added and removed some FAILED() calls. */

/*        Body radii accessed from kernel pool using ZZGFTREB. */

/*        Re-ordered header sections. */

/* -    SPICELIB Version 1.1.0, 24-OCT-2005 (NJB) */

/*        Parameters KWX, KWY, KWZ were renamed to KVX, KVY, KVZ. */

/*        Call to ZZBODVCD was replaced with call to BODVCD. */

/* -    SPICELIB Version 1.0.0, 10-JAN-2005 (NJB) */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     Local variables */


/*     Saved variables */


/*     Initial values */

    if (return_()) {
	return 0;
    }
    chkin_("ZZDYNRT0", (ftnlen)8);
    if (first) {

/*        Get the ID code for the J2000 frame. */

	irfnum_("J2000", &j2000, (ftnlen)5);

/*        Get the ID code for the earth (we needn't check the found */
/*        flag). */

	bodn2c_("EARTH", &earth, &fnd, (ftnlen)5);

/*        Initialize "item" strings used to create kernel variable */
/*        names. */

	for (i__ = 1; i__ <= 2; ++i__) {

/*           Vector axis: */

/* Writing concatenation */
	    i__3[0] = 4, a__1[0] = vname + (((i__2 = i__ - 1) < 2 && 0 <= 
		    i__2 ? i__2 : s_rnge("vname", i__2, "zzdynrt0_", (ftnlen)
		    542)) << 2);
	    i__3[1] = 4, a__1[1] = "AXIS";
	    s_cat(itmaxe + (((i__1 = i__ - 1) < 2 && 0 <= i__1 ? i__1 : 
		    s_rnge("itmaxe", i__1, "zzdynrt0_", (ftnlen)542)) << 5), 
		    a__1, i__3, &c__2, (ftnlen)32);

/*           Vector definition: */

/* Writing concatenation */
	    i__3[0] = 4, a__1[0] = vname + (((i__2 = i__ - 1) < 2 && 0 <= 
		    i__2 ? i__2 : s_rnge("vname", i__2, "zzdynrt0_", (ftnlen)
		    546)) << 2);
	    i__3[1] = 10, a__1[1] = "VECTOR_DEF";
	    s_cat(itmvdf + (((i__1 = i__ - 1) < 2 && 0 <= i__1 ? i__1 : 
		    s_rnge("itmvdf", i__1, "zzdynrt0_", (ftnlen)546)) << 5), 
		    a__1, i__3, &c__2, (ftnlen)32);

/*           Vector aberration correction: */

/* Writing concatenation */
	    i__3[0] = 4, a__1[0] = vname + (((i__2 = i__ - 1) < 2 && 0 <= 
		    i__2 ? i__2 : s_rnge("vname", i__2, "zzdynrt0_", (ftnlen)
		    550)) << 2);
	    i__3[1] = 6, a__1[1] = "ABCORR";
	    s_cat(itmabc + (((i__1 = i__ - 1) < 2 && 0 <= i__1 ? i__1 : 
		    s_rnge("itmabc", i__1, "zzdynrt0_", (ftnlen)550)) << 5), 
		    a__1, i__3, &c__2, (ftnlen)32);

/*           Vector frame: */

/* Writing concatenation */
	    i__3[0] = 4, a__1[0] = vname + (((i__2 = i__ - 1) < 2 && 0 <= 
		    i__2 ? i__2 : s_rnge("vname", i__2, "zzdynrt0_", (ftnlen)
		    554)) << 2);
	    i__3[1] = 5, a__1[1] = "FRAME";
	    s_cat(itmfrm + (((i__1 = i__ - 1) < 2 && 0 <= i__1 ? i__1 : 
		    s_rnge("itmfrm", i__1, "zzdynrt0_", (ftnlen)554)) << 5), 
		    a__1, i__3, &c__2, (ftnlen)32);

/*           Vector observer: */

/* Writing concatenation */
	    i__3[0] = 4, a__1[0] = vname + (((i__2 = i__ - 1) < 2 && 0 <= 
		    i__2 ? i__2 : s_rnge("vname", i__2, "zzdynrt0_", (ftnlen)
		    558)) << 2);
	    i__3[1] = 8, a__1[1] = "OBSERVER";
	    s_cat(itmobs + (((i__1 = i__ - 1) < 2 && 0 <= i__1 ? i__1 : 
		    s_rnge("itmobs", i__1, "zzdynrt0_", (ftnlen)558)) << 5), 
		    a__1, i__3, &c__2, (ftnlen)32);

/*           Vector target: */

/* Writing concatenation */
	    i__3[0] = 4, a__1[0] = vname + (((i__2 = i__ - 1) < 2 && 0 <= 
		    i__2 ? i__2 : s_rnge("vname", i__2, "zzdynrt0_", (ftnlen)
		    562)) << 2);
	    i__3[1] = 6, a__1[1] = "TARGET";
	    s_cat(itmtrg + (((i__1 = i__ - 1) < 2 && 0 <= i__1 ? i__1 : 
		    s_rnge("itmtrg", i__1, "zzdynrt0_", (ftnlen)562)) << 5), 
		    a__1, i__3, &c__2, (ftnlen)32);

/*           Vector longitude: */

/* Writing concatenation */
	    i__3[0] = 4, a__1[0] = vname + (((i__2 = i__ - 1) < 2 && 0 <= 
		    i__2 ? i__2 : s_rnge("vname", i__2, "zzdynrt0_", (ftnlen)
		    566)) << 2);
	    i__3[1] = 9, a__1[1] = "LONGITUDE";
	    s_cat(itmlon + (((i__1 = i__ - 1) < 2 && 0 <= i__1 ? i__1 : 
		    s_rnge("itmlon", i__1, "zzdynrt0_", (ftnlen)566)) << 5), 
		    a__1, i__3, &c__2, (ftnlen)32);

/*           Vector latitude: */

/* Writing concatenation */
	    i__3[0] = 4, a__1[0] = vname + (((i__2 = i__ - 1) < 2 && 0 <= 
		    i__2 ? i__2 : s_rnge("vname", i__2, "zzdynrt0_", (ftnlen)
		    570)) << 2);
	    i__3[1] = 8, a__1[1] = "LATITUDE";
	    s_cat(itmlat + (((i__1 = i__ - 1) < 2 && 0 <= i__1 ? i__1 : 
		    s_rnge("itmlat", i__1, "zzdynrt0_", (ftnlen)570)) << 5), 
		    a__1, i__3, &c__2, (ftnlen)32);

/*           Vector right ascension: */

/* Writing concatenation */
	    i__3[0] = 4, a__1[0] = vname + (((i__2 = i__ - 1) < 2 && 0 <= 
		    i__2 ? i__2 : s_rnge("vname", i__2, "zzdynrt0_", (ftnlen)
		    574)) << 2);
	    i__3[1] = 2, a__1[1] = "RA";
	    s_cat(itmra + (((i__1 = i__ - 1) < 2 && 0 <= i__1 ? i__1 : s_rnge(
		    "itmra", i__1, "zzdynrt0_", (ftnlen)574)) << 5), a__1, 
		    i__3, &c__2, (ftnlen)32);

/*           Vector declination: */

/* Writing concatenation */
	    i__3[0] = 4, a__1[0] = vname + (((i__2 = i__ - 1) < 2 && 0 <= 
		    i__2 ? i__2 : s_rnge("vname", i__2, "zzdynrt0_", (ftnlen)
		    578)) << 2);
	    i__3[1] = 3, a__1[1] = "DEC";
	    s_cat(itmdec + (((i__1 = i__ - 1) < 2 && 0 <= i__1 ? i__1 : 
		    s_rnge("itmdec", i__1, "zzdynrt0_", (ftnlen)578)) << 5), 
		    a__1, i__3, &c__2, (ftnlen)32);

/*           Vector units: */

/* Writing concatenation */
	    i__3[0] = 4, a__1[0] = vname + (((i__2 = i__ - 1) < 2 && 0 <= 
		    i__2 ? i__2 : s_rnge("vname", i__2, "zzdynrt0_", (ftnlen)
		    582)) << 2);
	    i__3[1] = 5, a__1[1] = "UNITS";
	    s_cat(itmunt + (((i__1 = i__ - 1) < 2 && 0 <= i__1 ? i__1 : 
		    s_rnge("itmunt", i__1, "zzdynrt0_", (ftnlen)582)) << 5), 
		    a__1, i__3, &c__2, (ftnlen)32);

/*           Constant vector coordinate specification: */

/* Writing concatenation */
	    i__3[0] = 4, a__1[0] = vname + (((i__2 = i__ - 1) < 2 && 0 <= 
		    i__2 ? i__2 : s_rnge("vname", i__2, "zzdynrt0_", (ftnlen)
		    586)) << 2);
	    i__3[1] = 4, a__1[1] = "SPEC";
	    s_cat(itmspc + (((i__1 = i__ - 1) < 2 && 0 <= i__1 ? i__1 : 
		    s_rnge("itmspc", i__1, "zzdynrt0_", (ftnlen)586)) << 5), 
		    a__1, i__3, &c__2, (ftnlen)32);

/*           Constant vector in Cartesian coordinates, literal value: */

/* Writing concatenation */
	    i__3[0] = 4, a__1[0] = vname + (((i__2 = i__ - 1) < 2 && 0 <= 
		    i__2 ? i__2 : s_rnge("vname", i__2, "zzdynrt0_", (ftnlen)
		    590)) << 2);
	    i__3[1] = 6, a__1[1] = "VECTOR";
	    s_cat(itmvec + (((i__1 = i__ - 1) < 2 && 0 <= i__1 ? i__1 : 
		    s_rnge("itmvec", i__1, "zzdynrt0_", (ftnlen)590)) << 5), 
		    a__1, i__3, &c__2, (ftnlen)32);
	}
	first = FALSE_;
    }

/*     Initialize the output arguments. */

    cleard_(&c__9, rotate);
    *basfrm = 0;

/*     Initialize certain variables to ensure that we don't do */
/*     arithmetic operations using bogus, possibly large, */
/*     undefined values. */

    cleard_(&c__36, nutxf);
    cleard_(&c__9, oblr);
    cleard_(&c__36, precxf);
    cleard_(&c__9, r2000);
    cleard_(&c__9, rtemp);
    cleard_(&c__9, rinv);
    cleard_(&c__9, tipm);
    mob = 0.;
    dmob = 0.;
    t0 = 0.;
    frozen = FALSE_;

/*     Get the input frame name. */

    frmnam_(infram, inname__, (ftnlen)32);

/*     We need the name of the base frame. */

    zzdynfid_(inname__, infram, "RELATIVE", basfrm, (ftnlen)32, (ftnlen)8);
    if (failed_()) {
	chkout_("ZZDYNRT0", (ftnlen)8);
	return 0;
    }
    frmnam_(basfrm, basnam, (ftnlen)32);

/*     The output frame code and name are set. */

/*     Look up the dynamic frame definition style from the kernel pool. */
/*     The kernel variable's name might be specified by name or ID. */

    zzdynvac_(inname__, infram, "DEF_STYLE", &c__1, &n, dynstl, (ftnlen)32, (
	    ftnlen)9, (ftnlen)80);
    if (failed_()) {
	chkout_("ZZDYNRT0", (ftnlen)8);
	return 0;
    }

/*     At this time, the only supported dynamic frame definition style is */
/*     PARAMETERIZED. */

    if (eqstr_(dynstl, "PARAMETERIZED", (ftnlen)80, (ftnlen)13)) {

/*        Parameterized dynamic frames belong to families.  Look up */
/*        the family for this frame. */

	zzdynvac_(inname__, infram, "FAMILY", &c__1, &n, dynfam, (ftnlen)32, (
		ftnlen)6, (ftnlen)80);
	if (failed_()) {
	    chkout_("ZZDYNRT0", (ftnlen)8);
	    return 0;
	}
	cmprss_(" ", &c__0, dynfam, tmpfam, (ftnlen)1, (ftnlen)80, (ftnlen)80)
		;
	ucase_(tmpfam, dynfam, (ftnlen)80, (ftnlen)80);

/*        Determine whether we have an "of-date" frame family. */
/*        The logical flags used here and respective meanings are: */

/*           MEANEQ   Mean equator and equinox of date */
/*           TRUEEQ   True equator and equinox of date */
/*           MEANEC   Mean ecliptic and equinox of date */

	meaneq = s_cmp(dynfam, "MEAN_EQUATOR_AND_EQUINOX_OF_DATE", (ftnlen)80,
		 (ftnlen)32) == 0;
	trueeq = s_cmp(dynfam, "TRUE_EQUATOR_AND_EQUINOX_OF_DATE", (ftnlen)80,
		 (ftnlen)32) == 0;
	meanec = s_cmp(dynfam, "MEAN_ECLIPTIC_AND_EQUINOX_OF_DATE", (ftnlen)
		80, (ftnlen)33) == 0;
	ofdate = meaneq || meanec || trueeq;

/*        Set the evaluation epoch T0.  Normally this epoch is ET, */
/*        but if the frame is frozen, the freeze epoch from the */
/*        frame definition is used. */

/*        Read the freeze epoch into T0 if a freeze epoch was */
/*        specified; let FROZEN receive the FOUND flag value */
/*        returned by ZZDYNOAD. */

	zzdynoad_(inname__, infram, "FREEZE_EPOCH", &c__1, &n, &t0, &frozen, (
		ftnlen)32, (ftnlen)12);
	if (failed_()) {
	    chkout_("ZZDYNRT0", (ftnlen)8);
	    return 0;
	}
	if (! frozen) {

/*           Normal case:  just use the input epoch. */

	    t0 = *et;
	}

/*        Look up the rotation state keyword.  In this routine, */
/*        the rotation state keyword is examined only to support */
/*        semantic checking:  there's no use made of the fact that */
/*        the rotation state is 'ROTATING' or 'INERTIAL'. */

	zzdynoac_(inname__, infram, "ROTATION_STATE", &c__1, &n, rotsta, &fnd,
		 (ftnlen)32, (ftnlen)14, (ftnlen)80);
	if (failed_()) {
	    chkout_("ZZDYNRT0", (ftnlen)8);
	    return 0;
	}
	if (fnd) {

/*           Catch invalid rotation states here. */

	    if (! eqstr_(rotsta, "ROTATING", (ftnlen)80, (ftnlen)8) && ! 
		    eqstr_(rotsta, "INERTIAL", (ftnlen)80, (ftnlen)8)) {
		setmsg_("Definition of frame # contains # specification #. T"
			"he only valid rotation states are # or #. This situa"
			"tion is usually caused by an error in a frame kernel"
			" in which the frame is defined.", (ftnlen)186);
		errch_("#", inname__, (ftnlen)1, (ftnlen)32);
		errch_("#", "ROTATION_STATE", (ftnlen)1, (ftnlen)14);
		errch_("#", rotsta, (ftnlen)1, (ftnlen)80);
		errch_("#", "ROTATING", (ftnlen)1, (ftnlen)8);
		errch_("#", "INERTIAL", (ftnlen)1, (ftnlen)8);
		sigerr_("SPICE(NOTSUPPORTED)", (ftnlen)19);
		chkout_("ZZDYNRT0", (ftnlen)8);
		return 0;
	    }
	}


/*        If the frame is frozen, the rotation state keyword *must be */
/*        absent*. */

	if (frozen && fnd) {
	    setmsg_("Definition of frame # contains both # and # keywords; a"
		    "t most one of these must be present in the frame definit"
		    "ion. This situation is usually caused by an error in a f"
		    "rame kernel in which the frame is defined.", (ftnlen)209);
	    errch_("#", inname__, (ftnlen)1, (ftnlen)32);
	    errch_("#", "FREEZE_EPOCH", (ftnlen)1, (ftnlen)12);
	    errch_("#", "ROTATION_STATE", (ftnlen)1, (ftnlen)14);
	    sigerr_("SPICE(FRAMEDEFERROR)", (ftnlen)20);
	    chkout_("ZZDYNRT0", (ftnlen)8);
	    return 0;
	}

/*        If the frame belongs to an "of date" family, either the */
/*        rotation state must be specified or the frame must be */
/*        frozen. */

	if (ofdate && ! frozen && ! fnd) {
	    setmsg_("Definition of frame #, which belongs to parameterized d"
		    "ynamic frame family #, contains neither # nor # keywords"
		    "; frames in this family require exactly one of these in "
		    "their frame definitions. This situation is usually cause"
		    "d by an error in a frame kernel in which the frame is de"
		    "fined.", (ftnlen)285);
	    errch_("#", inname__, (ftnlen)1, (ftnlen)32);
	    errch_("#", dynfam, (ftnlen)1, (ftnlen)80);
	    errch_("#", "FREEZE_EPOCH", (ftnlen)1, (ftnlen)12);
	    errch_("#", "ROTATION_STATE", (ftnlen)1, (ftnlen)14);
	    sigerr_("SPICE(FRAMEDEFERROR)", (ftnlen)20);
	    chkout_("ZZDYNRT0", (ftnlen)8);
	    return 0;
	}
	if (failed_()) {
	    chkout_("ZZDYNRT0", (ftnlen)8);
	    return 0;
	}

/*        The evaluation epoch T0 is set. */

/*        In this routine, unlike its companion ZZDYNFRM, there is no */
/*        need to make further reference to the rotation state.  Hence */
/*        the flag INERT used in ZZDYNFRM doesn't appear here. */

/*        The following code block performs actions specific to */
/*        the various dynamic frame families. */

	if (ofdate) {

/*           Fetch the name of the true equator and equinox of date */
/*           precession model. */

	    zzdynvac_(inname__, infram, "PREC_MODEL", &c__1, &n, prcmod, (
		    ftnlen)32, (ftnlen)10, (ftnlen)80);
	    if (failed_()) {
		chkout_("ZZDYNRT0", (ftnlen)8);
		return 0;
	    }

/*           Get the precession transformation. */

	    if (eqstr_(prcmod, "EARTH_IAU_1976", (ftnlen)80, (ftnlen)14)) {

/*              This is the 1976 IAU earth precession model. */

/*              Make sure the center of the input frame is the earth. */

		if (*center != earth) {
		    bodc2n_(center, ctrnam, &fnd, (ftnlen)36);
		    if (! fnd) {
			intstr_(center, ctrnam, (ftnlen)36);
		    }
		    setmsg_("Definition of frame # specifies frame center # "
			    "and precession model #. This precession model is"
			    " not applicable to body #. This situation is usu"
			    "ally caused by an error in a frame kernel in whi"
			    "ch the frame is defined.", (ftnlen)215);
		    errch_("#", inname__, (ftnlen)1, (ftnlen)32);
		    errch_("#", ctrnam, (ftnlen)1, (ftnlen)36);
		    errch_("#", "EARTH_IAU_1976", (ftnlen)1, (ftnlen)14);
		    errch_("#", ctrnam, (ftnlen)1, (ftnlen)36);
		    sigerr_("SPICE(INVALIDSELECTION)", (ftnlen)23);
		    chkout_("ZZDYNRT0", (ftnlen)8);
		    return 0;
		}

/*              Look up the precession transformation.  Extract */
/*              the precession rotation matrix. */

		zzeprc76_(&t0, precxf);
		moved_(precxf, &c__3, rprec);
		moved_(&precxf[6], &c__3, &rprec[3]);
		moved_(&precxf[12], &c__3, &rprec[6]);

/*              If we're in the mean-of-date case, invert this */
/*              transformation to obtain the mapping from the */
/*              mean-of-date frame to J2000. */

		if (meaneq) {
		    xpose_(rprec, rtemp);
		}
	    } else {
		setmsg_("Definition of frame # specifies precession model #,"
			" which is not recognized. This situation is usually "
			"caused by an error in a frame kernel in which the fr"
			"ame is defined.", (ftnlen)170);
		errch_("#", inname__, (ftnlen)1, (ftnlen)32);
		errch_("#", prcmod, (ftnlen)1, (ftnlen)80);
		sigerr_("SPICE(NOTSUPPORTED)", (ftnlen)19);
		chkout_("ZZDYNRT0", (ftnlen)8);
		return 0;
	    }

/*           At this point the precession transformation REPREC is set. */
/*           If INFRAM is a mean equator and equinox of date frame, the */
/*           inverse of REPREC is currently stored in RTEMP. */
	    if (trueeq) {

/*              We need a nutation transformation as well. Get the name */
/*              of the nutation model. */

		zzdynvac_(inname__, infram, "NUT_MODEL", &c__1, &n, nutmod, (
			ftnlen)32, (ftnlen)9, (ftnlen)80);
		if (failed_()) {
		    chkout_("ZZDYNRT0", (ftnlen)8);
		    return 0;
		}

/*              Get the nutation transformation. */

		if (eqstr_(nutmod, "EARTH_IAU_1980", (ftnlen)80, (ftnlen)14)) 
			{

/*                 This is the 1980 IAU earth nutation model. */

/*                 Make sure the center is the earth. */

		    if (*center != earth) {
			bodc2n_(center, ctrnam, &fnd, (ftnlen)36);
			if (! fnd) {
			    intstr_(center, ctrnam, (ftnlen)36);
			}
			setmsg_("Definition of frame # specifies frame cente"
				"r # and nutation model #. This nutation mode"
				"l is not applicable to body #.  This situati"
				"on is usually caused by an error in a frame "
				"kernel in which the frame is defined.", (
				ftnlen)212);
			errch_("#", inname__, (ftnlen)1, (ftnlen)32);
			errch_("#", ctrnam, (ftnlen)1, (ftnlen)36);
			errch_("#", "EARTH_IAU_1980", (ftnlen)1, (ftnlen)14);
			errch_("#", ctrnam, (ftnlen)1, (ftnlen)36);
			sigerr_("SPICE(INVALIDSELECTION)", (ftnlen)23);
			chkout_("ZZDYNRT0", (ftnlen)8);
			return 0;
		    }

/*                 Look up the nutation transformation.  Extract */
/*                 the nutation rotation matrix. */

		    zzenut80_(&t0, nutxf);
		    moved_(nutxf, &c__3, rnut);
		    moved_(&nutxf[6], &c__3, &rnut[3]);
		    moved_(&nutxf[12], &c__3, &rnut[6]);

/*                 Find the rotation from the J2000 frame to the earth */
/*                 true of date frame.  Invert. */

		    mxm_(rnut, rprec, rinv);
		    xpose_(rinv, rtemp);
		} else {
		    setmsg_("Definition of frame # specifies nutation model "
			    "#, which is not recognized. This situation is us"
			    "ually caused by an error in a frame kernel in wh"
			    "ich the frame is defined.", (ftnlen)168);
		    errch_("#", inname__, (ftnlen)1, (ftnlen)32);
		    errch_("#", nutmod, (ftnlen)1, (ftnlen)80);
		    sigerr_("SPICE(NOTSUPPORTED)", (ftnlen)19);
		    chkout_("ZZDYNRT0", (ftnlen)8);
		    return 0;
		}
	    } else if (meanec) {

/*              We need a mean obliquity transformation as well. */
/*              Get the name of the obliquity model. */

		zzdynvac_(inname__, infram, "OBLIQ_MODEL", &c__1, &n, oblmod, 
			(ftnlen)32, (ftnlen)11, (ftnlen)80);
		if (failed_()) {
		    chkout_("ZZDYNRT0", (ftnlen)8);
		    return 0;
		}

/*              Get the obliquity transformation. */

		if (eqstr_(oblmod, "EARTH_IAU_1980", (ftnlen)80, (ftnlen)14)) 
			{

/*                 This is the 1980 IAU earth mean obliquity of */
/*                 date model. */

/*                 Make sure the center is the earth. */

		    if (*center != earth) {
			bodc2n_(center, ctrnam, &fnd, (ftnlen)36);
			if (! fnd) {
			    intstr_(center, ctrnam, (ftnlen)36);
			}
			setmsg_("Definition of frame # specifies frame cente"
				"r # and obliquity model #.  This obliquity m"
				"odel is not applicable to body #. This situa"
				"tion is usually caused by an error in a fram"
				"e kernel in which the frame is defined.", (
				ftnlen)214);
			errch_("#", inname__, (ftnlen)1, (ftnlen)32);
			errch_("#", ctrnam, (ftnlen)1, (ftnlen)36);
			errch_("#", "EARTH_IAU_1980", (ftnlen)1, (ftnlen)14);
			errch_("#", ctrnam, (ftnlen)1, (ftnlen)36);
			sigerr_("SPICE(INVALIDSELECTION)", (ftnlen)23);
			chkout_("ZZDYNRT0", (ftnlen)8);
			return 0;
		    }

/*                 Create the obliquity transformation. */
/*                 First look up the obliquity state. */

		    zzmobliq_(&t0, &mob, &dmob);

/*                 The obliquity rotation is about the mean-of-date */
/*                 x-axis.  The other Euler angles are identically */
/*                 zero; the axes are arbitrary, as long as the */
/*                 middle axis is distinct from the other two. */

		    eul2m_(&c_b194, &c_b194, &mob, &c__1, &c__3, &c__1, oblr);

/*                 Find the rotation from the J2000 to the */
/*                 earth mean ecliptic of date frame.  Invert. */

		    mxm_(oblr, rprec, rinv);
		    xpose_(rinv, rtemp);
		} else {
		    setmsg_("Definition of frame # specifies obliquity model"
			    " #, which is not recognized. This situation is u"
			    "sually caused by an error in a frame kernel in w"
			    "hich the frame is defined.", (ftnlen)169);
		    errch_("#", inname__, (ftnlen)1, (ftnlen)32);
		    errch_("#", oblmod, (ftnlen)1, (ftnlen)80);
		    sigerr_("SPICE(NOTSUPPORTED)", (ftnlen)19);
		    chkout_("ZZDYNRT0", (ftnlen)8);
		    return 0;
		}
	    }

/*           At this point, RTEMP contains the rotation from the */
/*           specified mean of date or true of date frame to J2000. */

/*           If the base frame is not J2000, we must find the */
/*           transformation from J2000 to the base frame. */
	    if (*basfrm != j2000) {
		zzrefch1_(&j2000, basfrm, &t0, r2000);
		mxm_(r2000, rtemp, rotate);
	    } else {

/*              Otherwise, RTEMP is the matrix we want. */

		moved_(rtemp, &c__9, rotate);
	    }

/*           Now ROTATE is the state transformation mapping from */
/*           the input frame INFRAM to the base frame BASFRM. */

/*           This is the end of the work specific to "of-date" frames. */
/*           From here we drop out of the IF block. */

	} else if (s_cmp(dynfam, "TWO-VECTOR", (ftnlen)80, (ftnlen)10) == 0) {

/*           The frame belongs to the TWO-VECTOR family. */

/*           Fetch the specifications of the primary and secondary */
/*           axes. */

	    cleard_(&c__6, v2);
	    for (i__ = 1; i__ <= 2; ++i__) {

/*              Get the name of the axis associated with the Ith */
/*              defining vector. */

		zzdynvac_(inname__, infram, itmaxe + (((i__1 = i__ - 1) < 2 &&
			 0 <= i__1 ? i__1 : s_rnge("itmaxe", i__1, "zzdynrt0_"
			, (ftnlen)1109)) << 5), &c__1, &n, axname, (ftnlen)32,
			 (ftnlen)32, (ftnlen)80);
		if (failed_()) {
		    chkout_("ZZDYNRT0", (ftnlen)8);
		    return 0;
		}
		cmprss_(" ", &c__0, axname, axname, (ftnlen)1, (ftnlen)80, (
			ftnlen)80);
		ucase_(axname, axname, (ftnlen)80, (ftnlen)80);

/*              Set the sign flag associated with the axis. */

		negate = *(unsigned char *)axname == '-';
		cmprss_("-", &c__0, axname, axname, (ftnlen)1, (ftnlen)80, (
			ftnlen)80);
		cmprss_("+", &c__0, axname, axname, (ftnlen)1, (ftnlen)80, (
			ftnlen)80);
		axis[(i__1 = i__ - 1) < 2 && 0 <= i__1 ? i__1 : s_rnge("axis",
			 i__1, "zzdynrt0_", (ftnlen)1127)] = isrchc_(axname, &
			c__3, axes, (ftnlen)80, (ftnlen)1);
		if (axis[(i__1 = i__ - 1) < 2 && 0 <= i__1 ? i__1 : s_rnge(
			"axis", i__1, "zzdynrt0_", (ftnlen)1130)] == 0) {
		    setmsg_("Definition of frame # associates vector # with "
			    "axis #.  The only valid axis values are { X, -X,"
			    " Y, -Y, Z, -Z }. This situation is usually cause"
			    "d by an error in a frame kernel in which the fra"
			    "me is defined.", (ftnlen)205);
		    errch_("#", inname__, (ftnlen)1, (ftnlen)32);
		    errint_("#", &i__, (ftnlen)1);
		    errch_("#", axname, (ftnlen)1, (ftnlen)80);
		    sigerr_("SPICE(INVALIDAXIS)", (ftnlen)18);
		    chkout_("ZZDYNRT0", (ftnlen)8);
		    return 0;
		}

/*              Find out how the vector is defined: */

/*                 - Observer-target position vector */
/*                 - Observer-target velocity vector */
/*                 - Observer-target near point vector */
/*                 - Constant vector */

/*              VECDEF(I) indicates the vector definition method */
/*              for the Ith vector. */

		zzdynvac_(inname__, infram, itmvdf + (((i__1 = i__ - 1) < 2 &&
			 0 <= i__1 ? i__1 : s_rnge("itmvdf", i__1, "zzdynrt0_"
			, (ftnlen)1159)) << 5), &c__1, &n, vecdef + ((i__2 = 
			i__ - 1) < 2 && 0 <= i__2 ? i__2 : s_rnge("vecdef", 
			i__2, "zzdynrt0_", (ftnlen)1159)) * 80, (ftnlen)32, (
			ftnlen)32, (ftnlen)80);
		if (failed_()) {
		    chkout_("ZZDYNRT0", (ftnlen)8);
		    return 0;
		}
		cmprss_(" ", &c__0, vecdef + ((i__1 = i__ - 1) < 2 && 0 <= 
			i__1 ? i__1 : s_rnge("vecdef", i__1, "zzdynrt0_", (
			ftnlen)1167)) * 80, vecdef + ((i__2 = i__ - 1) < 2 && 
			0 <= i__2 ? i__2 : s_rnge("vecdef", i__2, "zzdynrt0_",
			 (ftnlen)1167)) * 80, (ftnlen)1, (ftnlen)80, (ftnlen)
			80);
		ucase_(vecdef + ((i__1 = i__ - 1) < 2 && 0 <= i__1 ? i__1 : 
			s_rnge("vecdef", i__1, "zzdynrt0_", (ftnlen)1168)) * 
			80, vecdef + ((i__2 = i__ - 1) < 2 && 0 <= i__2 ? 
			i__2 : s_rnge("vecdef", i__2, "zzdynrt0_", (ftnlen)
			1168)) * 80, (ftnlen)80, (ftnlen)80);
		if (s_cmp(vecdef + ((i__1 = i__ - 1) < 2 && 0 <= i__1 ? i__1 :
			 s_rnge("vecdef", i__1, "zzdynrt0_", (ftnlen)1171)) * 
			80, "OBSERVER_TARGET_POSITION", (ftnlen)80, (ftnlen)
			24) == 0) {

/*                 The vector is the position of a target relative */
/*                 to an observer. */

/*                 We need a target, observer, and aberration correction. */

		    zzdynbid_(inname__, infram, itmtrg + (((i__1 = i__ - 1) < 
			    2 && 0 <= i__1 ? i__1 : s_rnge("itmtrg", i__1, 
			    "zzdynrt0_", (ftnlen)1178)) << 5), &targ, (ftnlen)
			    32, (ftnlen)32);
		    zzdynbid_(inname__, infram, itmobs + (((i__1 = i__ - 1) < 
			    2 && 0 <= i__1 ? i__1 : s_rnge("itmobs", i__1, 
			    "zzdynrt0_", (ftnlen)1180)) << 5), &obs, (ftnlen)
			    32, (ftnlen)32);
		    zzdynvac_(inname__, infram, itmabc + (((i__1 = i__ - 1) < 
			    2 && 0 <= i__1 ? i__1 : s_rnge("itmabc", i__1, 
			    "zzdynrt0_", (ftnlen)1182)) << 5), &c__1, &n, 
			    abcorr, (ftnlen)32, (ftnlen)32, (ftnlen)5);

/*                 Look up the Ith position vector in the J2000 frame. */

		    zzspkzp1_(&targ, &t0, "J2000", abcorr, &obs, &v2[(i__1 = 
			    i__ * 3 - 3) < 6 && 0 <= i__1 ? i__1 : s_rnge(
			    "v2", i__1, "zzdynrt0_", (ftnlen)1188)], &lt, (
			    ftnlen)5, (ftnlen)5);
		    if (failed_()) {
			chkout_("ZZDYNRT0", (ftnlen)8);
			return 0;
		    }

/*                 At this point, V2(*,I) contains position relative to */
/*                 frame J2000. */

		} else if (s_cmp(vecdef + ((i__1 = i__ - 1) < 2 && 0 <= i__1 ?
			 i__1 : s_rnge("vecdef", i__1, "zzdynrt0_", (ftnlen)
			1201)) * 80, "OBSERVER_TARGET_VELOCITY", (ftnlen)80, (
			ftnlen)24) == 0) {

/*                 The vector is the velocity of a target relative */
/*                 to an observer. */

/*                 We need a target, observer, and aberration correction. */

		    zzdynbid_(inname__, infram, itmtrg + (((i__1 = i__ - 1) < 
			    2 && 0 <= i__1 ? i__1 : s_rnge("itmtrg", i__1, 
			    "zzdynrt0_", (ftnlen)1208)) << 5), &targ, (ftnlen)
			    32, (ftnlen)32);
		    zzdynbid_(inname__, infram, itmobs + (((i__1 = i__ - 1) < 
			    2 && 0 <= i__1 ? i__1 : s_rnge("itmobs", i__1, 
			    "zzdynrt0_", (ftnlen)1210)) << 5), &obs, (ftnlen)
			    32, (ftnlen)32);
		    zzdynvac_(inname__, infram, itmabc + (((i__1 = i__ - 1) < 
			    2 && 0 <= i__1 ? i__1 : s_rnge("itmabc", i__1, 
			    "zzdynrt0_", (ftnlen)1212)) << 5), &c__1, &n, 
			    abcorr, (ftnlen)32, (ftnlen)32, (ftnlen)5);

/*                 We need to know the frame in which the velocity is */
/*                 defined. */

		    zzdynfid_(inname__, infram, itmfrm + (((i__1 = i__ - 1) < 
			    2 && 0 <= i__1 ? i__1 : s_rnge("itmfrm", i__1, 
			    "zzdynrt0_", (ftnlen)1219)) << 5), &frid, (ftnlen)
			    32, (ftnlen)32);
		    frmnam_(&frid, velfrm, (ftnlen)32);

/*                 Look up the Ith velocity vector in the velocity frame. */

		    zzspkez0_(&targ, &t0, velfrm, abcorr, &obs, stemp, &lt, (
			    ftnlen)32, (ftnlen)5);
		    if (failed_()) {
			chkout_("ZZDYNRT0", (ftnlen)8);
			return 0;
		    }

/*                 We'll work with the unit velocity vector. */

		    vhat_(&stemp[3], &v2[(i__1 = i__ * 3 - 3) < 6 && 0 <= 
			    i__1 ? i__1 : s_rnge("v2", i__1, "zzdynrt0_", (
			    ftnlen)1236)]);

/*                 We need the epoch VET at which VELFRM is evaluated. */
/*                 This epoch will be used to transform the velocity */
/*                 vector from VELFRM to J2000. */

/*                 Set the default value of VET here. */

		    vet = t0;

/*                 Parse the aberration correction.  Capture the */
/*                 epoch used to evaluate the velocity vector's frame. */

		    zzvalcor_(abcorr, corblk, (ftnlen)5);
		    if (failed_()) {
			chkout_("ZZDYNRT0", (ftnlen)8);
			return 0;
		    }
		    if (corblk[1]) {

/*                    Light time correction is used.  The epoch used */
/*                    to evaluate the velocity vector's frame depends */
/*                    on the frame's observer and center. */

/*                    Look up the velocity frame's center. */

			frinfo_(&frid, &frctr, &frcls, &frcid, &fnd);
			if (! fnd) {
			    setmsg_("In definition of frame #, the frame ass"
				    "ociated with a velocity vector has frame"
				    " ID code #, but no frame center, frame c"
				    "lass, or frame class ID was found by FRI"
				    "NFO.  This situation MAY be caused by an"
				    " error in a frame kernel in which the fr"
				    "ame is defined. The problem also could b"
				    "e indicative of a SPICELIB bug.", (ftnlen)
				    310);
			    errch_("#", inname__, (ftnlen)1, (ftnlen)32);
			    errint_("#", &frid, (ftnlen)1);
			    sigerr_("SPICE(FRAMEDATANOTFOUND)", (ftnlen)24);
			    chkout_("ZZDYNRT0", (ftnlen)8);
			    return 0;
			}
			if (frcls != 1) {

/*                       Obtain light time from the observer to the */
/*                       frame's center. */

			    zzspkzp1_(&frctr, &t0, "J2000", abcorr, &obs, 
				    ctrpos, &vflt, (ftnlen)5, (ftnlen)5);
			    zzcorepc_(abcorr, &t0, &vflt, &vet, (ftnlen)5);
			    if (failed_()) {
				chkout_("ZZDYNRT0", (ftnlen)8);
				return 0;
			    }
			}
		    }

/*                 The velocity frame evaluation epoch VET is now set. */

/*                 We must rotate the velocity vector from the velocity */
/*                 frame (evaluated at VET) to the output frame at T0. */
/*                 We'll do this in two stages, first mapping velocity */
/*                 into the J2000 frame. */

		    if (frid != j2000) {
			zzrefch1_(&frid, &j2000, &vet, r2000);
			if (failed_()) {
			    chkout_("ZZDYNRT0", (ftnlen)8);
			    return 0;
			}
			mxv_(r2000, &v2[(i__1 = i__ * 3 - 3) < 6 && 0 <= i__1 
				? i__1 : s_rnge("v2", i__1, "zzdynrt0_", (
				ftnlen)1326)], ptemp);
			moved_(ptemp, &c__3, &v2[(i__1 = i__ * 3 - 3) < 6 && 
				0 <= i__1 ? i__1 : s_rnge("v2", i__1, "zzdyn"
				"rt0_", (ftnlen)1327)]);
		    }

/*                 At this point, V2(*,I) contains velocity */
/*                 relative to frame J2000. */
		} else if (s_cmp(vecdef + ((i__1 = i__ - 1) < 2 && 0 <= i__1 ?
			 i__1 : s_rnge("vecdef", i__1, "zzdynrt0_", (ftnlen)
			1336)) * 80, "TARGET_NEAR_POINT", (ftnlen)80, (ftnlen)
			17) == 0) {

/*                 The vector points from an observer to the near */
/*                 point to the observer on the target body. */

/*                 We need a target, observer, and aberration correction. */

		    zzdynbid_(inname__, infram, itmtrg + (((i__1 = i__ - 1) < 
			    2 && 0 <= i__1 ? i__1 : s_rnge("itmtrg", i__1, 
			    "zzdynrt0_", (ftnlen)1343)) << 5), &targ, (ftnlen)
			    32, (ftnlen)32);
		    zzdynbid_(inname__, infram, itmobs + (((i__1 = i__ - 1) < 
			    2 && 0 <= i__1 ? i__1 : s_rnge("itmobs", i__1, 
			    "zzdynrt0_", (ftnlen)1345)) << 5), &obs, (ftnlen)
			    32, (ftnlen)32);
		    zzdynvac_(inname__, infram, itmabc + (((i__1 = i__ - 1) < 
			    2 && 0 <= i__1 ? i__1 : s_rnge("itmabc", i__1, 
			    "zzdynrt0_", (ftnlen)1347)) << 5), &c__1, &n, 
			    abcorr, (ftnlen)32, (ftnlen)32, (ftnlen)5);

/*                 The vector points from an observer to the */
/*                 sub-observer point (nearest point to the observer) on */
/*                 the target body.  We need the position of the near */
/*                 point relative to the observer. */

/*                 We'll look up the position of the target center */
/*                 relative to the observer, as well as the position of */
/*                 the near point relative to the target center, both in */
/*                 the body-fixed frame associated with the target. */

/*                 Look up the body-fixed frame associated with the */
/*                 target body. */

		    cidfrm_(&targ, &cfrmid, cfrmnm, &fnd, (ftnlen)32);
		    if (! fnd) {
			setmsg_("Definition of frame # requires definition o"
				"f body-fixed frame associated with target bo"
				"dy #. A call to CIDFRM indicated no body-fix"
				"ed frame is associated with the target body."
				"  This situation can arise when a frame kern"
				"el defining the target's body-fixed frame  l"
				"acks the OBJECT_<ID>_FRAME or OBJECT_<name>_"
				"FRAME keywords.  The problem also could be c"
				"aused by an error in a frame kernel in which"
				" the parameterized two-vector dynamic frame "
				"# is defined.", (ftnlen)452);
			errch_("#", inname__, (ftnlen)1, (ftnlen)32);
			errint_("#", &targ, (ftnlen)1);
			errch_("#", inname__, (ftnlen)1, (ftnlen)32);
			sigerr_("SPICE(FRAMEDATANOTFOUND)", (ftnlen)24);
			chkout_("ZZDYNRT0", (ftnlen)8);
			return 0;
		    }

/*                 Get the radii of the target body. */

		    zzgftreb_(&targ, radii);
		    if (failed_()) {
			chkout_("ZZDYNRT0", (ftnlen)8);
			return 0;
		    }

/*                 Look up the Ith position vector in the target-fixed */
/*                 frame.  Negate the vector to obtain the target-to- */
/*                 observer vector. */

		    zzspkzp1_(&targ, &t0, cfrmnm, abcorr, &obs, ptemp, &lt, (
			    ftnlen)32, (ftnlen)5);

/*                 We check FAILED() here because VMINUS is a simple */
/*                 arithmetic routine that doesn't return on entry. */

		    if (failed_()) {
			chkout_("ZZDYNRT0", (ftnlen)8);
			return 0;
		    }
		    vminus_(ptemp, pobs);
		    nearpt_(pobs, radii, &radii[1], &radii[2], pnear, &alt);
		    if (failed_()) {
			chkout_("ZZDYNRT0", (ftnlen)8);
			return 0;
		    }

/*                 Find the observer-near point vector in the current */
/*                 frame CFRMNM. */

		    vsub_(pnear, pobs, ptemp);

/*                 Rotate the vector to frame J2000.  To get the required */
/*                 rotation matrix, we'll need to obtain the epoch */
/*                 associated with CNMFRM.  Parse the aberration */
/*                 correction and adjust the frame evaluation epoch as */
/*                 needed. */

		    zzcorepc_(abcorr, &t0, &lt, &fet, (ftnlen)5);

/*                 Obtain the matrix for transforming position vectors */
/*                 from the target center frame to the J2000 frame and */
/*                 apply it to the observer-to-near point position */
/*                 vector. */

		    zzrefch1_(&cfrmid, &j2000, &fet, tipm);
		    if (failed_()) {
			chkout_("ZZDYNRT0", (ftnlen)8);
			return 0;
		    }
		    mxv_(tipm, ptemp, &v2[(i__1 = i__ * 3 - 3) < 6 && 0 <= 
			    i__1 ? i__1 : s_rnge("v2", i__1, "zzdynrt0_", (
			    ftnlen)1459)]);

/*                 At this point, V2(*,I) contains position of the near */
/*                 point on the target as seen by the observer, relative */
/*                 to frame J2000. */

		} else if (s_cmp(vecdef + ((i__1 = i__ - 1) < 2 && 0 <= i__1 ?
			 i__1 : s_rnge("vecdef", i__1, "zzdynrt0_", (ftnlen)
			1467)) * 80, "CONSTANT", (ftnlen)80, (ftnlen)8) == 0) 
			{

/*                 The vector is constant in a specified frame. */

/*                 We need a 3-vector and an associated reference */
/*                 frame relative to which the vector is specified. */

/*                 Look up the ID of the frame first. */

		    zzdynfid_(inname__, infram, itmfrm + (((i__1 = i__ - 1) < 
			    2 && 0 <= i__1 ? i__1 : s_rnge("itmfrm", i__1, 
			    "zzdynrt0_", (ftnlen)1476)) << 5), &frid, (ftnlen)
			    32, (ftnlen)32);

/*                 Let FET ("frame ET") be the evaluation epoch for */
/*                 the constant vector's frame.  By default, this */
/*                 frame is just T0, but if we're using light time */
/*                 corrections, FET must be adjusted for one-way */
/*                 light time between the frame's center and the */
/*                 observer. */

/*                 Set the default value of FET here. */

		    fet = t0;

/*                 Optionally, there is an aberration correction */
/*                 associated with the constant vector's frame. */
/*                 If so, an observer must be associated with the */
/*                 frame.  Look up the correction first. */

		    zzdynoac_(inname__, infram, itmabc + (((i__1 = i__ - 1) < 
			    2 && 0 <= i__1 ? i__1 : s_rnge("itmabc", i__1, 
			    "zzdynrt0_", (ftnlen)1496)) << 5), &c__1, &n, 
			    cvcorr, &fnd, (ftnlen)32, (ftnlen)32, (ftnlen)5);
		    if (failed_()) {
			chkout_("ZZDYNRT0", (ftnlen)8);
			return 0;
		    }
		    if (! fnd) {
			s_copy(cvcorr, "NONE", (ftnlen)5, (ftnlen)4);
		    }
		    zzprscor_(cvcorr, corblk, (ftnlen)5);
		    if (failed_()) {
			chkout_("ZZDYNRT0", (ftnlen)8);
			return 0;
		    }
		    if (! corblk[0]) {

/*                    We need to apply an aberration correction to */
/*                    the constant vector. */
/*                    Check for errors in the aberration correction */
/*                    specification. */

/*                       - Light time and stellar aberration corrections */
/*                         are mutually exclusive. */

			if (corblk[1] && corblk[2]) {
			    setmsg_("Definition of frame # specifies aberrat"
				    "ion correction # for constant vector.  L"
				    "ight time and stellar aberration correct"
				    "ions are mutually exclusive for constant"
				    " vectors used in two-vector parameterize"
				    "d dynamic frame definitions.  This situa"
				    "tion is usually caused by an error in a "
				    "frame kernel in which the frame is defin"
				    "ed.", (ftnlen)322);
			    errch_("#", inname__, (ftnlen)1, (ftnlen)32);
			    errch_("#", cvcorr, (ftnlen)1, (ftnlen)5);
			    sigerr_("SPICE(INVALIDOPTION)", (ftnlen)20);
			    chkout_("ZZDYNRT0", (ftnlen)8);
			    return 0;
			}
			if (corblk[1]) {

/*                       Light time correction is used.  The epoch used */
/*                       to evaluate the constant vector's frame depends */
/*                       on the frame's observer and center. */

/*                       Look up the constant vector frame's center. */

			    frinfo_(&frid, &frctr, &frcls, &frcid, &fnd);
			    if (! fnd) {
				setmsg_("In definition of frame #, the frame"
					" associated with a constant vector h"
					"as frame ID code #, but no frame cen"
					"ter, frame class, or frame class ID "
					"was found by FRINFO.  This situation"
					" MAY be caused by an error in a fram"
					"e kernel in which the frame is defin"
					"ed. The problem also could be indica"
					"tive of a SPICELIB bug.", (ftnlen)310)
					;
				errch_("#", inname__, (ftnlen)1, (ftnlen)32);
				errint_("#", &frid, (ftnlen)1);
				sigerr_("SPICE(FRAMEDATANOTFOUND)", (ftnlen)
					24);
				chkout_("ZZDYNRT0", (ftnlen)8);
				return 0;
			    }
			    if (frcls != 1) {

/*                          Look up the observer associated with the */
/*                          constant vector's frame.  This observer, */
/*                          together with the frame's center, determines */
/*                          the evaluation epoch for the frame. */

				zzdynbid_(inname__, infram, itmobs + (((i__1 =
					 i__ - 1) < 2 && 0 <= i__1 ? i__1 : 
					s_rnge("itmobs", i__1, "zzdynrt0_", (
					ftnlen)1590)) << 5), &cvobs, (ftnlen)
					32, (ftnlen)32);

/*                          Obtain light time from the observer to the */
/*                          frame's center. */

				zzspkzp1_(&frctr, &t0, "J2000", cvcorr, &
					cvobs, ctrpos, &lt, (ftnlen)5, (
					ftnlen)5);

/*                          Find the evaluation epoch for the frame. */

				zzcorepc_(cvcorr, &t0, &lt, &fet, (ftnlen)5);
				if (failed_()) {
				    chkout_("ZZDYNRT0", (ftnlen)8);
				    return 0;
				}
			    }
			} else if (corblk[2]) {

/*                       Stellar aberration case. */

/*                       The constant vector must be corrected for */
/*                       stellar aberration induced by the observer's */
/*                       velocity relative to the solar system */
/*                       barycenter.  First, find this velocity in */
/*                       the J2000 frame.  We'll apply the correction */
/*                       later, when the constant vector has been */
/*                       transformed to the J2000 frame. */

			    zzdynbid_(inname__, infram, itmobs + (((i__1 = 
				    i__ - 1) < 2 && 0 <= i__1 ? i__1 : s_rnge(
				    "itmobs", i__1, "zzdynrt0_", (ftnlen)1625)
				    ) << 5), &cvobs, (ftnlen)32, (ftnlen)32);
			    zzspksb1_(&cvobs, &t0, "J2000", stobs, (ftnlen)5);
			    if (failed_()) {
				chkout_("ZZDYNRT0", (ftnlen)8);
				return 0;
			    }
			}
		    }

/*                 Get the constant vector specification. */

		    zzdynvac_(inname__, infram, itmspc + (((i__1 = i__ - 1) < 
			    2 && 0 <= i__1 ? i__1 : s_rnge("itmspc", i__1, 
			    "zzdynrt0_", (ftnlen)1642)) << 5), &c__1, &n, 
			    spec, (ftnlen)32, (ftnlen)32, (ftnlen)80);
		    if (failed_()) {
			chkout_("ZZDYNRT0", (ftnlen)8);
			return 0;
		    }
		    cmprss_(" ", &c__0, spec, spec, (ftnlen)1, (ftnlen)80, (
			    ftnlen)80);
		    ucase_(spec, spec, (ftnlen)80, (ftnlen)80);
		    if (s_cmp(spec, "RECTANGULAR", (ftnlen)80, (ftnlen)11) == 
			    0) {

/*                    The coordinate system is rectangular. */

/*                    Look up the constant vector. */

			zzdynvad_(inname__, infram, itmvec + (((i__1 = i__ - 
				1) < 2 && 0 <= i__1 ? i__1 : s_rnge("itmvec", 
				i__1, "zzdynrt0_", (ftnlen)1659)) << 5), &
				c__3, &n, dirvec, (ftnlen)32, (ftnlen)32);
		    } else if (s_cmp(spec, "LATITUDINAL", (ftnlen)80, (ftnlen)
			    11) == 0 || s_cmp(spec, "RA/DEC", (ftnlen)80, (
			    ftnlen)6) == 0) {

/*                    The coordinate system is latitudinal or RA/DEC. */

/*                    Look up the units associated with the angles. */

			zzdynvac_(inname__, infram, itmunt + (((i__1 = i__ - 
				1) < 2 && 0 <= i__1 ? i__1 : s_rnge("itmunt", 
				i__1, "zzdynrt0_", (ftnlen)1670)) << 5), &
				c__1, &n, units, (ftnlen)32, (ftnlen)32, (
				ftnlen)80);
			if (s_cmp(spec, "LATITUDINAL", (ftnlen)80, (ftnlen)11)
				 == 0) {

/*                       Look up longitude and latitude. */

			    zzdynvad_(inname__, infram, itmlon + (((i__1 = 
				    i__ - 1) < 2 && 0 <= i__1 ? i__1 : s_rnge(
				    "itmlon", i__1, "zzdynrt0_", (ftnlen)1678)
				    ) << 5), &c__1, &n, &lon, (ftnlen)32, (
				    ftnlen)32);
			    zzdynvad_(inname__, infram, itmlat + (((i__1 = 
				    i__ - 1) < 2 && 0 <= i__1 ? i__1 : s_rnge(
				    "itmlat", i__1, "zzdynrt0_", (ftnlen)1681)
				    ) << 5), &c__1, &n, &lat, (ftnlen)32, (
				    ftnlen)32);

/*                       Convert angles from input units to radians. */

			    convrt_(&lon, units, "RADIANS", angles, (ftnlen)
				    80, (ftnlen)7);
			    convrt_(&lat, units, "RADIANS", &angles[1], (
				    ftnlen)80, (ftnlen)7);
			} else {

/*                       Look up RA and DEC. */

			    zzdynvad_(inname__, infram, itmra + (((i__1 = i__ 
				    - 1) < 2 && 0 <= i__1 ? i__1 : s_rnge(
				    "itmra", i__1, "zzdynrt0_", (ftnlen)1694))
				     << 5), &c__1, &n, &ra, (ftnlen)32, (
				    ftnlen)32);
			    zzdynvad_(inname__, infram, itmdec + (((i__1 = 
				    i__ - 1) < 2 && 0 <= i__1 ? i__1 : s_rnge(
				    "itmdec", i__1, "zzdynrt0_", (ftnlen)1697)
				    ) << 5), &c__1, &n, &dec, (ftnlen)32, (
				    ftnlen)32);

/*                       Convert angles from input units to radians. */

			    convrt_(&ra, units, "RADIANS", angles, (ftnlen)80,
				     (ftnlen)7);
			    convrt_(&dec, units, "RADIANS", &angles[1], (
				    ftnlen)80, (ftnlen)7);
			}
			if (failed_()) {
			    chkout_("ZZDYNRT0", (ftnlen)8);
			    return 0;
			}

/*                    Now  produce a direction vector. */

			latrec_(&c_b372, angles, &angles[1], dirvec);
		    } else {
			setmsg_("Definition of two-vector parameterized dyna"
				"mic frame # includes constant vector specifi"
				"cation #, which is not supported.  This situ"
				"ation is usually caused by an error in a fra"
				"me kernel in which the frame is defined.", (
				ftnlen)215);
			errch_("#", inname__, (ftnlen)1, (ftnlen)32);
			errch_("#", spec, (ftnlen)1, (ftnlen)80);
			sigerr_("SPICE(NOTSUPPORTED)", (ftnlen)19);
			chkout_("ZZDYNRT0", (ftnlen)8);
			return 0;
		    }

/*                 At this point, the Cartesian coordinates of the */
/*                 vector relative to the constant vector frame */
/*                 are stored in DIRVEC. */

		    if (frid == j2000) {
			vequ_(dirvec, &v2[(i__1 = i__ * 3 - 3) < 6 && 0 <= 
				i__1 ? i__1 : s_rnge("v2", i__1, "zzdynrt0_", 
				(ftnlen)1744)]);
		    } else {

/*                    Convert the direction vector to the J2000 frame. */

			zzrefch1_(&frid, &j2000, &fet, r2000);
			if (failed_()) {
			    chkout_("ZZDYNRT0", (ftnlen)8);
			    return 0;
			}
			mxv_(r2000, dirvec, &v2[(i__1 = i__ * 3 - 3) < 6 && 0 
				<= i__1 ? i__1 : s_rnge("v2", i__1, "zzdynrt"
				"0_", (ftnlen)1757)]);
		    }

/*                 The constant vector is now represented */
/*                 in the J2000 frame, but we may still need to */
/*                 apply a stellar aberration correction. */

		    if (corblk[2]) {

/*                    Perform the correction appropriate to the */
/*                    radiation travel sense. */

			if (corblk[4]) {

/*                       The correction is for transmission. */

			    stlabx_(&v2[(i__1 = i__ * 3 - 3) < 6 && 0 <= i__1 
				    ? i__1 : s_rnge("v2", i__1, "zzdynrt0_", (
				    ftnlen)1775)], &stobs[3], ptemp);
			} else {

/*                       The correction is for reception. */

			    stelab_(&v2[(i__1 = i__ * 3 - 3) < 6 && 0 <= i__1 
				    ? i__1 : s_rnge("v2", i__1, "zzdynrt0_", (
				    ftnlen)1781)], &stobs[3], ptemp);
			}
			if (failed_()) {
			    chkout_("ZZDYNRT0", (ftnlen)8);
			    return 0;
			}
			vequ_(ptemp, &v2[(i__1 = i__ * 3 - 3) < 6 && 0 <= 
				i__1 ? i__1 : s_rnge("v2", i__1, "zzdynrt0_", 
				(ftnlen)1790)]);
		    }

/*                 At this point, V2(*,I) contains the constant */
/*                 (constant relative to its associated frame, that is) */
/*                 vector as seen by the observer, relative to frame */
/*                 J2000. */

		} else {
		    setmsg_("Definition of two-vector parameterized dynamic "
			    "frame # includes vector definition #, which is n"
			    "ot supported.  This situation is usually caused "
			    "by an error in a frame kernel in which the frame"
			    " is defined.", (ftnlen)203);
		    errch_("#", inname__, (ftnlen)1, (ftnlen)32);
		    errch_("#", vecdef + ((i__1 = i__ - 1) < 2 && 0 <= i__1 ? 
			    i__1 : s_rnge("vecdef", i__1, "zzdynrt0_", (
			    ftnlen)1811)) * 80, (ftnlen)1, (ftnlen)80);
		    sigerr_("SPICE(NOTSUPPORTED)", (ftnlen)19);
		    chkout_("ZZDYNRT0", (ftnlen)8);
		    return 0;
		}

/*              Negate the vector if the axis has negative sign. */

		if (negate) {
		    vminus_(&v2[(i__1 = i__ * 3 - 3) < 6 && 0 <= i__1 ? i__1 :
			     s_rnge("v2", i__1, "zzdynrt0_", (ftnlen)1822)], 
			    ptemp);
		    moved_(ptemp, &c__3, &v2[(i__1 = i__ * 3 - 3) < 6 && 0 <= 
			    i__1 ? i__1 : s_rnge("v2", i__1, "zzdynrt0_", (
			    ftnlen)1823)]);
		}
	    }

/*           Look up the lower bound for the angular separation of */
/*           the defining vectors.  Use the default value if none */
/*           was supplied. */

	    zzdynoad_(inname__, infram, itmsep, &c__1, &n, &minsep, &fnd, (
		    ftnlen)32, (ftnlen)32);
	    if (failed_()) {
		chkout_("ZZDYNRT0", (ftnlen)8);
		return 0;
	    }
	    if (! fnd) {
		minsep = .001;
	    }

/*           Now use our vectors to compute our position transformation */
/*           matrix. */

/*           Check the angular separation of the defining vectors. We */
/*           want to ensure that the vectors are not too close to being */
/*           linearly dependent.  We can handle both cases---separation */
/*           close to 0 or separation close to Pi---by comparing the */
/*           sine of the separation to the sine of the separation limit. */

	    sep = vsep_(v2, &v2[3]);
	    if (sin(sep) < sin(minsep)) {
		etcal_(&t0, timstr, (ftnlen)50);
		setmsg_("Angular separation of vectors defining two-vector p"
			"arameterized dynamic frame # is # (radians); minimum"
			" allowed difference of separation from 0 or Pi is # "
			"radians.  Evaluation epoch is #.  Extreme loss of pr"
			"ecision can occur when defining vectors are nearly l"
			"inearly dependent.  This type of error can be due to"
			" using a dynamic frame outside of the time range for"
			" which it is meant. It also can be due to a conceptu"
			"al error pertaining to the frame's definition, or to"
			" an implementation error in the frame kernel contain"
			"ing the frame definition. However, if you wish to pr"
			"oceed with this computation, the # keyword can be us"
			"ed in the frame definition to adjust the separation "
			"limit.", (ftnlen)681);
		errch_("#", inname__, (ftnlen)1, (ftnlen)32);
		errdp_("#", &sep, (ftnlen)1);
		errdp_("#", &minsep, (ftnlen)1);
		errch_("#", timstr, (ftnlen)1, (ftnlen)50);
		errch_("#", "ANGLE_SEP_TOL", (ftnlen)1, (ftnlen)13);
		sigerr_("SPICE(DEGENERATECASE)", (ftnlen)21);
		chkout_("ZZDYNRT0", (ftnlen)8);
		return 0;
	    }

/*           We have both positions expressed relative to frame J2000 */
/*           at this point.  Find the transformation from INNAME to */
/*           the frame J2000, then from J2000 to frame BASNAM. */

	    twovec_(v2, axis, &v2[3], &axis[1], rinv);
	    xpose_(rinv, rotate);
	    if (*basfrm != j2000) {
		moved_(rotate, &c__9, rtemp);
		zzrefch1_(&j2000, basfrm, &t0, r2000);
		if (failed_()) {
		    chkout_("ZZDYNRT0", (ftnlen)8);
		    return 0;
		}
		mxm_(r2000, rtemp, rotate);
	    }

/*           ROTATE is set. */

/*           This is the end of the work specific to two-vector frames. */
/*           From here we drop out of the IF block. */

	} else if (s_cmp(dynfam, "EULER", (ftnlen)80, (ftnlen)5) == 0) {

/*           The frame belongs to the Euler family. */

/*           We expect to specifications of an axis sequence, units, */
/*           and angles via polynomial coefficients.  We also expect */
/*           to see an ET epoch. */

/*           Look up the epoch first.  Let DELTA represent the offset */
/*           of T0 relative to the epoch. */

/*           Initialize EPOCH so subtraction doesn't overflow if EPOCH */
/*           is invalid due to a lookup error. */

	    epoch = 0.;
	    zzdynvad_(inname__, infram, "EPOCH", &c__1, &n, &epoch, (ftnlen)
		    32, (ftnlen)5);
	    delta = t0 - epoch;

/*           Now the axis sequence. */

	    zzdynvai_(inname__, infram, "AXES", &c__3, &n, iaxes, (ftnlen)32, 
		    (ftnlen)4);

/*           Now the coefficients for the angles. */

	    for (i__ = 1; i__ <= 3; ++i__) {

/*              Initialize N so subtraction doesn't overflow if N */
/*              is invalid due to a lookup error. */

		n = 0;
		zzdynvad_(inname__, infram, itmcof + (((i__1 = i__ - 1) < 3 &&
			 0 <= i__1 ? i__1 : s_rnge("itmcof", i__1, "zzdynrt0_"
			, (ftnlen)1955)) << 5), &c__20, &n, &coeffs[(i__2 = 
			i__ * 20 - 20) < 60 && 0 <= i__2 ? i__2 : s_rnge(
			"coeffs", i__2, "zzdynrt0_", (ftnlen)1955)], (ftnlen)
			32, (ftnlen)32);

/*              Set the polynomial degree for the Ith angle. */

		degs[(i__1 = i__ - 1) < 3 && 0 <= i__1 ? i__1 : s_rnge("degs",
			 i__1, "zzdynrt0_", (ftnlen)1961)] = n - 1;
	    }

/*           Look up the units associated with the angles. */

	    zzdynvac_(inname__, infram, "UNITS", &c__1, &n, units, (ftnlen)32,
		     (ftnlen)5, (ftnlen)80);
	    if (failed_()) {
		chkout_("ZZDYNRT0", (ftnlen)8);
		return 0;
	    }

/*           Evaluate the angles at DELTA.  Convert angles from input */
/*           units to radians. */

	    for (i__ = 1; i__ <= 3; ++i__) {
		polyds_(&coeffs[(i__1 = i__ * 20 - 20) < 60 && 0 <= i__1 ? 
			i__1 : s_rnge("coeffs", i__1, "zzdynrt0_", (ftnlen)
			1981)], &degs[(i__2 = i__ - 1) < 3 && 0 <= i__2 ? 
			i__2 : s_rnge("degs", i__2, "zzdynrt0_", (ftnlen)1981)
			], &c__0, &delta, poly);

/*              Convert units.  Fill in the Euler angle vector. */

		convrt_(poly, units, "RADIANS", &eulang[(i__1 = i__ - 1) < 3 
			&& 0 <= i__1 ? i__1 : s_rnge("eulang", i__1, "zzdynr"
			"t0_", (ftnlen)1985)], (ftnlen)80, (ftnlen)7);
	    }

/*           Produce a position transformation matrix that maps from */
/*           the defined frame to the base frame. */

	    eul2m_(eulang, &eulang[1], &eulang[2], iaxes, &iaxes[1], &iaxes[2]
		    , rotate);

/*           This is the end of the work specific to Euler frames. */
/*           From here we drop out of the IF block. */

	} else if (s_cmp(dynfam, "PRODUCT", (ftnlen)80, (ftnlen)7) == 0) {

/*           The frame belongs to the product family. */

/*           We expect to see specifications of "from" and "to" frames */
/*           that make up the product. */

/*           Look up the "from" and "to" frames that define the product. */

	    zzdynvac_(inname__, infram, "FROM_FRAMES", &c__10, &m, fframs, (
		    ftnlen)32, (ftnlen)11, (ftnlen)32);
	    zzdynvac_(inname__, infram, "TO_FRAMES", &c__10, &n, tframs, (
		    ftnlen)32, (ftnlen)9, (ftnlen)32);
	    if (failed_()) {
		chkout_("ZZDYNRT0", (ftnlen)8);
		return 0;
	    }
	    if (n != m) {
		setmsg_("Definition of product parameterized dynamic frame #"
			" has # \"from\" frames and # \"to\" frames. These co"
			"unts must match.", (ftnlen)115);
		errch_("#", inname__, (ftnlen)1, (ftnlen)32);
		errint_("#", &m, (ftnlen)1);
		errint_("#", &n, (ftnlen)1);
		sigerr_("SPICE(BADFRAMECOUNT)", (ftnlen)20);
		chkout_("ZZDYNRT0", (ftnlen)8);
		return 0;
	    }

/*           The product of the factors is the transformation from the */
/*           base frame to the output frame. We need the inverse of */
/*           this transformation. */

/*           Compute the inverse of the product of the factors; place */
/*           the result in XFORM. */

	    namfrm_(fframs, &fromid, (ftnlen)32);
	    namfrm_(tframs, &toid, (ftnlen)32);
	    zzrefch1_(&toid, &fromid, &t0, rotate);
	    if (failed_()) {
		chkout_("ZZDYNRT0", (ftnlen)8);
		return 0;
	    }
	    i__1 = n;
	    for (i__ = 2; i__ <= i__1; ++i__) {
		moved_(rotate, &c__9, rtemp);
		namfrm_(fframs + (((i__2 = i__ - 1) < 10 && 0 <= i__2 ? i__2 :
			 s_rnge("fframs", i__2, "zzdynrt0_", (ftnlen)2055)) <<
			 5), &fromid, (ftnlen)32);
		namfrm_(tframs + (((i__2 = i__ - 1) < 10 && 0 <= i__2 ? i__2 :
			 s_rnge("tframs", i__2, "zzdynrt0_", (ftnlen)2056)) <<
			 5), &toid, (ftnlen)32);
		zzrefch1_(&toid, &fromid, &t0, rotate);
		if (failed_()) {
		    chkout_("ZZDYNRT0", (ftnlen)8);
		    return 0;
		}
		mxm_(rotate, rtemp, rout);
		moved_(rout, &c__9, rotate);
	    }
	} else {
	    setmsg_("Dynamic frame family # (in definition of frame #) is no"
		    "t supported. This situation is usually caused by an erro"
		    "r in a frame kernel in which the frame is defined.", (
		    ftnlen)161);
	    errch_("#", dynfam, (ftnlen)1, (ftnlen)80);
	    errch_("#", inname__, (ftnlen)1, (ftnlen)32);
	    sigerr_("SPICE(NOTSUPPORTED)", (ftnlen)19);
	    chkout_("ZZDYNRT0", (ftnlen)8);
	    return 0;
	}

/*        This is the end of the IF block that processes the */
/*        parameterized dynamic frame families. */

    } else {
	setmsg_("Dynamic frame style # (in definition of frame #) is not sup"
		"ported. This situation is usually caused by an error in a fr"
		"ame kernel in which the frame is defined.", (ftnlen)160);
	errch_("#", dynstl, (ftnlen)1, (ftnlen)80);
	errch_("#", inname__, (ftnlen)1, (ftnlen)32);
	sigerr_("SPICE(NOTSUPPORTED)", (ftnlen)19);
	chkout_("ZZDYNRT0", (ftnlen)8);
	return 0;
    }

/*     At this point ROTATE is the position transformation matrix */
/*     mapping from the input frame INFRAM to the base frame BASFRM. */

/*     ROTATE and BASFRM is set. */

    chkout_("ZZDYNRT0", (ftnlen)8);
    return 0;
} /* zzdynrt0_ */

