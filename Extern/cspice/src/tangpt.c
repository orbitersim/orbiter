/* tangpt.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__0 = 0;
static integer c__3 = 3;
static integer c__1 = 1;

/* $Procedure TANGPT ( Ray-ellipsoid tangent point ) */
/* Subroutine */ int tangpt_(char *method, char *target, doublereal *et, char 
	*fixref, char *abcorr, char *corloc, char *obsrvr, char *dref, 
	doublereal *dvec, doublereal *tanpt, doublereal *alt, doublereal *
	range, doublereal *srfpt, doublereal *trgepc, doublereal *srfvec, 
	ftnlen method_len, ftnlen target_len, ftnlen fixref_len, ftnlen 
	abcorr_len, ftnlen corloc_len, ftnlen obsrvr_len, ftnlen dref_len)
{
    /* Initialized data */

    static logical first = TRUE_;
    static char prvcor[5] = "     ";
    static char prvloc[15] = "               ";
    static integer prvtcd = 0;
    static logical tanloc = FALSE_;
    static logical usecn = FALSE_;
    static logical uselt = FALSE_;
    static logical usestl = FALSE_;
    static logical xmit = FALSE_;

    /* System generated locals */
    doublereal d__1;

    /* Builtin functions */
    integer s_cmp(char *, char *, ftnlen, ftnlen);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    extern /* Subroutine */ int vadd_(doublereal *, doublereal *, doublereal *
	    ), zzbods2c_(integer *, char *, integer *, logical *, char *, 
	    integer *, logical *, ftnlen, ftnlen);
    doublereal dval, dist;
    extern doublereal vrel_(doublereal *, doublereal *), vdot_(doublereal *, 
	    doublereal *);
    integer nitr;
    extern /* Subroutine */ int vsub_(doublereal *, doublereal *, doublereal *
	    ), vequ_(doublereal *, doublereal *);
    doublereal tpos[3];
    extern /* Subroutine */ int mtxv_(doublereal *, doublereal *, doublereal *
	    ), zzbodvcd_(integer *, char *, integer *, integer *, integer *, 
	    doublereal *, ftnlen);
    doublereal j2dir[3];
    extern /* Subroutine */ int zznamfrm_(integer *, char *, integer *, char *
	    , integer *, ftnlen, ftnlen), zzvalcor_(char *, logical *, ftnlen)
	    , zzctruin_(integer *);
    integer i__;
    doublereal p[3], s;
    extern /* Subroutine */ int chkin_(char *, ftnlen), errch_(char *, char *,
	     ftnlen, ftnlen), edpnt_(doublereal *, doublereal *, doublereal *,
	     doublereal *, doublereal *);
    extern doublereal vdist_(doublereal *, doublereal *);
    logical ltcnv;
    extern logical eqstr_(char *, char *, ftnlen, ftnlen);
    extern doublereal vnorm_(doublereal *);
    extern logical vzero_(doublereal *);
    doublereal j2fixm[9]	/* was [3][3] */, r2jmat[9]	/* was [3][3] 
	    */;
    static logical svfnd1;
    doublereal j2lcus[3];
    static logical svfnd2;
    doublereal j2lpos[3], j2opos[3], j2tpos[3];
    static integer svctr1[2], svctr2[2];
    extern logical failed_(void);
    static integer svctr3[2], svctr4[2];
    integer dfrcde;
    static integer svctr5[2];
    doublereal epcdif;
    extern /* Subroutine */ int cleard_(integer *, doublereal *), refchg_(
	    integer *, integer *, doublereal *, doublereal *);
    doublereal lt;
    integer fxfcde, obscde;
    doublereal refepc, ltdiff;
    extern doublereal clight_(void);
    doublereal tanoff[3];
    integer dclass;
    extern doublereal touchd_(doublereal *);
    doublereal fixdir[3], fixobs[3], ltcent, prvepc, stlloc[3];
    char locstr[15];
    doublereal stlobs[3], stlfix[3], ctrpos[3];
    extern logical return_(void);
    doublereal prevlt, prvsrf[3], prvtan[3], ssbost[6], ssbtst[6], stloff[3], 
	    trgpos[3];
    integer dcentr, dtypid, fxcent, fxclss, fxtyid, trgcde;
    logical attblk[15], stlcnv;
    static char svtarg[36];
    static integer svtcde;
    static char svobsr[36];
    static integer svobsc;
    static char svfref[32];
    static integer svfxfc;
    static char svdref[32];
    static integer svdfrc;
    static doublereal svradi[3];
    static integer svnrad;
    extern /* Subroutine */ int chkout_(char *, ftnlen), setmsg_(char *, 
	    ftnlen), sigerr_(char *, ftnlen), ljucrs_(integer *, char *, char 
	    *, ftnlen, ftnlen), errint_(char *, integer *, ftnlen), frinfo_(
	    integer *, integer *, integer *, integer *, logical *), spkpos_(
	    char *, doublereal *, char *, char *, char *, doublereal *, 
	    doublereal *, ftnlen, ftnlen, ftnlen, ftnlen), vminus_(doublereal 
	    *, doublereal *), spkezp_(integer *, doublereal *, char *, char *,
	     integer *, doublereal *, doublereal *, ftnlen, ftnlen), npedln_(
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *), nplnpt_(doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *);
    logical fnd;
    extern /* Subroutine */ int nearpt_(doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *), spkssb_(
	    integer *, doublereal *, char *, doublereal *, ftnlen), pxform_(
	    char *, char *, doublereal *, doublereal *, ftnlen, ftnlen), 
	    stlabx_(doublereal *, doublereal *, doublereal *), stelab_(
	    doublereal *, doublereal *, doublereal *), mxv_(doublereal *, 
	    doublereal *, doublereal *);

/* $ Abstract */

/*     Compute, for a given observer, ray emanating from the observer, */
/*     and target, the "tangent point": the point on the ray nearest */
/*     to the target's surface. Also compute the point on the target's */
/*     surface nearest to the tangent point. */

/*     The locations of both points are optionally corrected for light */
/*     time and stellar aberration. */

/*     The surface shape is modeled as a triaxial ellipsoid. */

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

/*     ABCORR */
/*     CK */
/*     FRAMES */
/*     NAIF_IDS */
/*     PCK */
/*     SCLK */
/*     SPK */
/*     TIME */

/* $ Keywords */

/*     ELLIPSOID */
/*     GEOMETRY */
/*     RAY */

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

/*     This include file defines the dimension of the counter */
/*     array used by various SPICE subsystems to uniquely identify */
/*     changes in their states. */

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

/*     CTRSIZ      is the dimension of the counter array used by */
/*                 various SPICE subsystems to uniquely identify */
/*                 changes in their states. */

/* $ Author_and_Institution */

/*     B.V. Semenov    (JPL) */

/* $ Literature_References */

/*     None. */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 29-JUL-2013 (BVS) */

/* -& */

/*     End of include file. */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     METHOD     I   Computation method. */
/*     TARGET     I   Name of target body. */
/*     ET         I   Epoch in ephemeris seconds past J2000 TDB. */
/*     FIXREF     I   Body-fixed, body-centered target body frame. */
/*     ABCORR     I   Aberration correction. */
/*     CORLOC     I   Aberration correction locus: 'TANGENT POINT' or */
/*                    'SURFACE POINT'. */
/*     OBSRVR     I   Name of observing body. */
/*     DREF       I   Reference frame of ray direction vector. */
/*     DVEC       I   Ray direction vector. */
/*     TANPT      O   "Tangent point": point on ray nearest to surface. */
/*     ALT        O   Altitude of tangent point above surface. */
/*     RANGE      O   Distance of tangent point from observer. */
/*     SRFPT      O   Point on surface nearest to tangent point. */
/*     TRGEPC     O   Epoch associated with correction locus. */
/*     SRFVEC     O   Vector from observer to surface point SRFPT. */

/* $ Detailed_Input */

/*     METHOD   is a short string providing parameters defining */
/*              the computation method to be used. */

/*              METHOD is currently restricted to the value */

/*                 'ELLIPSOID' */

/*              This value indicates that the target shape is */
/*              modeled as a triaxial ellipsoid. */

/*              METHOD is case-insensitive, and leading and trailing */
/*              blanks in METHOD are not significant. */

/*     TARGET   is the name of the target body. TARGET is */
/*              case-insensitive, and leading and trailing blanks in */
/*              TARGET are not significant. Optionally, you may */
/*              supply a string containing the integer ID code */
/*              for the object. For example both 'MOON' and '301' */
/*              are legitimate strings that indicate the Moon is the */
/*              target body. */

/*              If the target is identified by name rather than ID code, */
/*              the target name must be recognized by SPICE. Radii */
/*              defining a triaxial ellipsoid target shape model must be */
/*              available in the kernel pool. See the $Files section */
/*              below. */

/*     ET       is the epoch associated with the observer, expressed as */
/*              ephemeris seconds past J2000 TDB. ET is the epoch at */
/*              which radiation is received by the observer, when an */
/*              observation is made, or in the case of transmission from */
/*              the observer, at which radiation is emitted. */

/*              ET is the epoch at which the state of the observer */
/*              relative to the solar system barycenter is computed. */

/*              When aberration corrections are not used, ET is also */
/*              the epoch at which the state and orientation of the */
/*              target body are computed. */

/*              When aberration corrections are used, the position */
/*              and orientation of the target body are computed at */
/*              ET-LT or ET+LT, where LT is the one-way light time */
/*              between the aberration correction locus and the */
/*              observer. The sign applied to LT depends on the */
/*              selected correction. See the descriptions of ABCORR */
/*              and CORLOC below for details. */

/*     FIXREF   is the name of a body-fixed reference frame centered on */
/*              the target body. FIXREF may be any such frame supported */
/*              by the SPICE system, including built-in frames */
/*              (documented in frames.req) and frames defined by a */
/*              loaded frame kernel (FK). The string FIXREF is */
/*              case-insensitive, and leading and trailing blanks in */
/*              FIXREF are not significant. */

/*              The output points TANPT and SRFPT, and the */
/*              observer-to-surface point vector SRFVEC will be */
/*              expressed relative to this reference frame. */

/*     ABCORR   indicates the aberration corrections to be applied */
/*              when computing the target's position and orientation. */

/*              See the description of the aberration correction */
/*              locus CORLOC for further details on how aberration */
/*              corrections are applied. */

/*              For remote sensing applications, where the apparent */
/*              tangent or surface point seen by the observer is */
/*              desired, normally one of the corrections */

/*                 'CN+S' or 'NONE' */

/*              should be selected. For applications involving */
/*              transmission from the observer, normally 'XCN+S' or */
/*              'NONE' should be selected. */

/*              Light-time-only corrections can be useful for */
/*              testing but generally don't accurately model geometry */
/*              applicable to remote sensing observations or signal */
/*              transmission. */

/*              The supported options are described below. */

/*              ABCORR may be any of the following: */

/*                 'NONE'     Compute outputs without applying */
/*                            aberration corrections. */

/*                            'NONE' may be suitable when the */
/*                            magnitudes of the aberration */
/*                            corrections are negligible. */

/*              Let LT represent the one-way light time between the */
/*              observer and the aberration correction locus specified */
/*              by CORLOC. The following values of ABCORR apply to the */
/*              "reception" case in which radiation departs from the */
/*              aberration correction locus at the light-time corrected */
/*              epoch ET-LT and arrives at the observer's location at */
/*              ET: */

/*                 'LT'       Correct for one-way light time between */
/*                            the aberration correction locus and */
/*                            the observer, using a Newtonian */
/*                            formulation. This correction yields the */
/*                            position of the aberration correction */
/*                            locus at the moment it emitted radiation */
/*                            arriving at the observer at ET. */

/*                            The light time correction uses an */
/*                            iterative solution of the light time */
/*                            equation. The solution invoked by the */
/*                            'LT' option uses several iterations */
/*                            but does not guarantee convergence. */

/*                            Both the target position as seen by the */
/*                            observer, and rotation of the target */
/*                            body, are corrected for light time. */

/*                 'LT+S'     Correct for one-way light time and */
/*                            stellar aberration using a Newtonian */
/*                            formulation. This option modifies the */
/*                            aberration correction locus solution */
/*                            obtained with the 'LT' option to */
/*                            account for the observer's velocity */
/*                            relative to the solar system */
/*                            barycenter. These corrections yield the */
/*                            apparent aberration correction locus. */

/*                 'CN'       Converged Newtonian light time */
/*                            correction. In solving the light time */
/*                            equation, the 'CN' correction iterates */
/*                            until either the solution converges or */
/*                            a large iteration limit is reached. */
/*                            Both the position and orientation of */
/*                            the target body are corrected for light */
/*                            time. */

/*                 'CN+S'     Converged Newtonian light time and stellar */
/*                            aberration corrections. This option */
/*                            produces a solution that is at least as */
/*                            accurate at that obtainable with the */
/*                            'LT+S' option. Whether the 'CN+S' solution */
/*                            is substantially more accurate depends on */
/*                            the geometry of the participating objects */
/*                            and on the accuracy of the input data. In */
/*                            some cases this routine will execute more */
/*                            slowly when a converged solution is */
/*                            computed. */

/*                            For reception-case applications where */
/*                            aberration corrections are applied, this */
/*                            option should be used, unless the */
/*                            magnitudes of the corrections are */
/*                            negligible. */

/*              The following values of ABCORR apply to the */
/*              "transmission" case in which radiation *departs* from */
/*              the observer's location at ET and arrives at the */
/*              aberration correction locus at the light-time */
/*              corrected epoch ET+LT: */

/*                 'XLT'      "Transmission" case: correct for */
/*                            one-way light time between the */
/*                            aberration correction locus and the */
/*                            observer, using a Newtonian */
/*                            formulation. This correction yields the */
/*                            position of the aberration correction */
/*                            locus at the moment it receives radiation */
/*                            emitted from the observer's location at */
/*                            ET. */

/*                            The light time correction uses an */
/*                            iterative solution of the light time */
/*                            equation. The solution invoked by the */
/*                            'XLT' option uses several iterations */
/*                            but does not guarantee convergence. */

/*                            Both the target position as seen by the */
/*                            observer, and rotation of the target */
/*                            body, are corrected for light time. */

/*                 'XLT+S'    "Transmission" case: correct for */
/*                            one-way light time and stellar */
/*                            aberration using a Newtonian */
/*                            formulation. This option modifies the */
/*                            aberration correction locus solution */
/*                            obtained with the 'XLT' option to */
/*                            account for the observer's velocity */
/*                            relative to the solar system */
/*                            barycenter. */

/*                            Stellar aberration is computed for */
/*                            transmitted, rather than received, */
/*                            radiation. */

/*                            These corrections yield the analog for */
/*                            the transmission case of the apparent */
/*                            aberration correction locus. */

/*                 'XCN'      "Transmission" case: converged Newtonian */
/*                            light time correction. In solving the */
/*                            light time equation, the 'XCN' correction */
/*                            iterates until either the solution */
/*                            converges or a large iteration limit is */
/*                            reached. Both the position and rotation of */
/*                            the target body are corrected for light */
/*                            time. */

/*                 'XCN+S'    "Transmission" case: converged Newtonian */
/*                            light time and stellar aberration */
/*                            corrections. This option produces a */
/*                            solution that is at least as accurate at */
/*                            that obtainable with the 'XLT+S' option. */
/*                            Whether the 'XCN+S' solution is */
/*                            substantially more accurate depends on the */
/*                            geometry of the participating objects and */
/*                            on the accuracy of the input data. In some */
/*                            cases this routine will execute more */
/*                            slowly when a converged solution is */
/*                            computed. */

/*                            For transmission-case applications where */
/*                            aberration corrections are applied, this */
/*                            option should be used, unless the */
/*                            magnitudes of the corrections are */
/*                            negligible. */

/*              Case and embedded blanks are not significant in */
/*              ABCORR. For example, the string */

/*                 'Cn + s' */

/*              is valid. */

/*     CORLOC   specifies the aberration correction "locus," which is */
/*              the fixed point in the frame designated by FIXREF for */
/*              which light time and stellar aberration corrections are */
/*              computed. */

/*              Differential aberration effects across the surface of */
/*              the target body are not considered by this routine. When */
/*              aberration corrections are used, the effective positions */
/*              of the observer and target, and the orientation of the */
/*              target, are computed according to the corrections */
/*              determined for the aberration correction locus. */

/*              The light time used to determine the position and */
/*              orientation of the target body is that between the */
/*              aberration correction locus and the observer. */

/*              The stellar aberration correction applied to the */
/*              position of the target is that computed for the */
/*              aberration correction locus. */

/*              The descriptions below apply only when aberration */
/*              corrections are used. */

/*              The values and meanings of CORLOC are: */

/*                 'TANGENT POINT'    Compute corrections at the */
/*                                    "tangent point," which is the */
/*                                    point on the ray, defined by DREF */
/*                                    and DVEC, nearest to the target's */
/*                                    surface. */

/*                 'SURFACE POINT'    Compute corrections at the */
/*                                    point on the target's surface */
/*                                    nearest to the tangent point. */

/*              Case and leading and trailing blanks are not significant */
/*              in CORLOC. */

/*     OBSRVR   is the name of the observing body. This is typically */
/*              a spacecraft or a surface point on an extended */
/*              ephemeris object. OBSRVR is case-insensitive, and */
/*              leading and trailing blanks in OBSRVR are not */
/*              significant. Optionally, you may supply a string */
/*              containing the integer ID code for the object. For */
/*              example both 'MOON' and '301' are legitimate strings */
/*              that indicate the Moon is the observer. */

/*              If the observer is identified by name rather than ID */
/*              code, the observer name must be recognized by SPICE. See */
/*              the $Files section below. */

/*     DREF     is the name of the reference frame relative to which */
/*              the ray direction vector is expressed. This may be */
/*              any frame supported by the SPICE system, including */
/*              built-in frames (documented in the Frames Required */
/*              Reading) and frames defined by a loaded frame kernel */
/*              (FK). The string DREF is case-insensitive, and */
/*              leading and trailing blanks in DREF are not */
/*              significant. */

/*              When DREF designates a non-inertial frame, the */
/*              orientation of the frame is evaluated at an epoch */
/*              dependent on the frame's center and, if the center is */
/*              not the observer, on the selected aberration */
/*              correction. See the description of the direction */
/*              vector DVEC for details. */

/*     DVEC     is a ray direction vector emanating from the observer. */
/*              The tangent point on the ray and the point on the target */
/*              body's surface nearest to the tangent point are sought. */

/*              DVEC is specified relative to the reference frame */
/*              designated by DREF. */

/*              Non-inertial reference frames are treated as follows: */
/*              if the center of the frame is at the observer's */
/*              location, the frame's orientation is evaluated at ET. */
/*              If the frame's center is located elsewhere, then */
/*              letting LTCENT be the one-way light time between the */
/*              observer and the central body associated with the */
/*              frame, the orientation of the frame is evaluated at */
/*              ET-LTCENT, ET+LTCENT, or ET depending on whether the */
/*              requested aberration correction is, respectively, for */
/*              received radiation, transmitted radiation, or is */
/*              omitted. LTCENT is computed using the method */
/*              indicated by ABCORR. */

/* $ Detailed_Output */

/*     TANPT    is the "tangent point": the point on the ray defined by */
/*              DREF and DVEC nearest to the target body's surface. */

/*              TANPT is a vector originating at the target body's */
/*              center, expressed in the reference frame designated */
/*              by FIXREF, the orientation of which is evaluated at */
/*              TRGEPC (see description below). Units are km. */

/*              If the ray intersects the surface, TANPT is the */
/*              nearest point of intersection to the observer. */

/*              If the ray points away from the surface---that is, if */
/*              the angle between the ray and the outward normal at the */
/*              target surface point nearest to the observer, computed */
/*              using the specified aberration corrections, is less than */
/*              or equal to 90 degrees---then TANPT is set to the */
/*              position of the observer relative to the target center. */

/*              TANPT is computed using the aberration corrections */
/*              specified by ABCORR and CORLOC. */

/*              When the aberration correction locus is set to */
/*              'TANGENT POINT', and the position of TANPT is */
/*              corrected for aberration as specified by ABCORR, the */
/*              resulting point will lie on the input ray. */

/*     ALT      is the altitude of the tangent point above the */
/*              target body's surface. This is the distance between */
/*              TANPT and SRFPT. Units are km. */

/*              If the ray intersects the surface, ALT is set to the */
/*              exact double precision value 0.D0. ALT may be used as */
/*              an indicator of whether a ray-surface intersection */
/*              exists. */

/*     RANGE    is the distance between the observer and the tangent */
/*              point. Units are km. */

/*              If the ray points away from the surface (see the */
/*              description of TANPT above), RANGE is set to the */
/*              exact double precision value 0.D0. RANGE may be used */
/*              as an indicator of whether this geometric condition */
/*              exists. */

/*     SRFPT    is the point on the target body's surface nearest to the */
/*              tangent point. */

/*              SRFPT is a vector originating at the target body's */
/*              center, expressed in the reference frame designated */
/*              by FIXREF, the orientation of which is evaluated at */
/*              TRGEPC (see description below). Units are km. */

/*              SRFPT is computed using the aberration corrections */
/*              specified by ABCORR and CORLOC. */

/*              When the aberration correction locus is set to */
/*              'SURFACE POINT', and the position of SRFPT is */
/*              corrected for aberration as specified by ABCORR, the */
/*              resulting point will lie on the ray emanating from */
/*              the observer and pointing in the direction of SRFVEC. */

/*              If the ray intersects the surface, SRFPT is the point of */
/*              intersection nearest to the observer. */

/*              If the ray points away from the surface (see the */
/*              description of TANPT above), SRFPT is set to the target */
/*              surface point nearest to the observer. */

/*     TRGEPC   is the epoch associated with the aberration correction */
/*              locus. TRGEPC is defined as follows: letting LT be the */
/*              one-way light time between the observer and the */
/*              aberration correction locus, TRGEPC is the epoch ET-LT, */
/*              ET+LT, or ET depending on whether the requested */
/*              aberration correction is, respectively, for received */
/*              radiation, transmitted radiation, or omitted. LT is */
/*              computed using the method indicated by ABCORR. */

/*              TRGEPC is expressed as seconds past J2000 TDB. */

/*              The name TRGEPC, which stands for "target epoch," */
/*              is used for compatibility with other SPICE high-level */
/*              geometry routines. Note that the epoch it designates */
/*              is not associated with the target body's center. */

/*     SRFVEC   is the vector from the observer's position at ET to */
/*              the surface point SRFPT, where the position of SRFPT */
/*              is corrected for aberrations as specified by ABCORR */
/*              and CORLOC. SRFVEC is expressed in the target */
/*              body-fixed reference frame designated by FIXREF, */
/*              evaluated at TRGEPC. Units are km. */

/*              One can use the SPICELIB function VNORM to obtain the */
/*              distance between the observer and SRFPT: */

/*                 DIST = VNORM ( SRFVEC ) */

/*              The observer's position OBSPOS, relative to the */
/*              target body's center, where the center's position is */
/*              corrected for aberration effects as indicated by */
/*              ABCORR and CORLOC, can be computed via the call: */

/*                 CALL VSUB ( SRFPT, SRFVEC, OBSPOS ) */

/*              To transform the vector SRFVEC from the reference frame */
/*              FIXREF at time TRGEPC to a time-dependent reference */
/*              frame REF at time ET, the routine PXFRM2 should be */
/*              called. Let XFORM be the 3x3 matrix representing the */
/*              rotation from the reference frame FIXREF at time */
/*              TRGEPC to the reference frame REF at time ET. Then */
/*              SRFVEC can be transformed to the result REFVEC as */
/*              follows: */

/*                 CALL PXFRM2 ( FIXREF, REF,    TRGEPC, ET, XFORM ) */
/*                 CALL MXV    ( XFORM,  SRFVEC, REFVEC ) */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the specified aberration correction is unrecognized, an */
/*         error is signaled by a routine in the call tree of this */
/*         routine. */

/*     2)  If METHOD is not equivalent to 'ELLIPSOID', when case and */
/*         blanks are ignored in the comparison, the error */
/*         SPICE(NOTSUPPORTED) is signaled. */

/*     3)  If CORLOC is not equivalent to either 'TANGENT POINT' or */
/*         'SURFACE POINT', when case and blanks are ignored, the */
/*         error SPICE(NOTSUPPORTED) is signaled. */

/*     4)  If the direction vector DVEC is the zero vector, the error */
/*         SPICE(ZEROVECTOR) is signaled. */

/*     5)  If either the target or observer input strings cannot be */
/*         converted to an integer ID code, the error */
/*         SPICE(IDCODENOTFOUND) is signaled. */

/*     6)  If OBSRVR and TARGET map to the same NAIF integer ID code, */
/*         the error SPICE(BODIESNOTDISTINCT) is signaled. */

/*     7)  If triaxial radii of the target body have not been loaded */
/*         into the kernel pool prior to a call to this routine, an */
/*         error is signaled by a routine in the call tree of this */
/*         routine. */

/*     8)  If the number of radii associated with the target body is not */
/*         three, the error SPICE(INVALIDCOUNT) is signaled. */

/*     9)  If the input target body-fixed frame FIXREF is not */
/*         recognized, the error SPICE(NOFRAME) is signaled. A frame */
/*         name may fail to be recognized because a required frame */
/*         specification kernel has not been loaded; another cause is a */
/*         misspelling of the frame name. */

/*     10) If the input frame FIXREF is not centered at the target body, */
/*         the error SPICE(INVALIDFRAME) is signaled. */

/*     11) If the reference frame designated by DREF is not recognized */
/*         by the SPICE frame subsystem, the error SPICE(NOFRAME) is */
/*         signaled. */

/*     12) If insufficient ephemeris data have been loaded prior to */
/*         calling TANGPT, an error is signaled by a routine in the call */
/*         tree of this routine. Note that when light time correction is */
/*         used, sufficient ephemeris data must be available to */
/*         propagate the states of both observer and target to the solar */
/*         system barycenter. If light time correction is used and */
/*         the ray's frame DREF is non-inertial, sufficient ephemeris */
/*         data must be available to compute the state of that frame's */
/*         center relative to the solar system barycenter. */

/*     13) If the target and observer have distinct identities but are */
/*         at the same location (for example, the target is Mars and the */
/*         observer is the Mars barycenter), the error */
/*         SPICE(NOSEPARATION) is signaled. */

/*     14) The target must be an extended body: if any of the radii of */
/*         the target body are non-positive, an error is signaled by a */
/*         routine in the call tree of this routine. */

/*     15) If the observer does not coincide with the target, but the */
/*         observer is located inside the ellipsoid modeling the */
/*         target body's shape, the error SPICE(INVALIDGEOMETRY) is */
/*         signaled. */

/*     16) If the transformation between the ray frame DREF and the */
/*         J2000 frame cannot be computed, an error is signaled by a */
/*         routine in the call tree of this routine. */

/*     17) If the transformation between the J2000 frame and the */
/*         target body-fixed, body-centered frame FIXREF cannot be */
/*         computed, an error is signaled by a routine in the call tree */
/*         of this routine. */

/*     18) If the nearest point to the target on the line containing */
/*         the input ray cannot be computed, an error is signaled by a */
/*         routine in the call tree of this routine. This type of error */
/*         may result from degenerate geometry; for example, if after */
/*         scaling the reference ellipsoid axes to make the longest */
/*         semi-axis a unit vector, another scaled axis is so short that */
/*         its squared length underflows to zero, no result can be */
/*         computed. */

/*     19) It is not an error for the ray to intersect the target body */
/*         or to point away from it so that the nearest point */
/*         to the ellipsoid on the line containing the ray lies behind */
/*         the ray's vertex. */

/* $ Files */

/*     Appropriate kernels must be loaded by the calling program before */
/*     this routine is called. */

/*     The following data are required: */

/*     -  SPK data: ephemeris data for target and observer must be */
/*        loaded. If aberration corrections are used, the states of */
/*        target and observer relative to the solar system barycenter */
/*        must be calculable from the available ephemeris data. */
/*        Typically ephemeris data are made available by loading one or */
/*        more SPK files via FURNSH. */

/*     -  PCK data: triaxial radii for the target body must be loaded */
/*        into the kernel pool. Typically this is done by loading a text */
/*        PCK file via FURNSH. */

/*     -  Target orientation data: rotation data for the target body */
/*        must be loaded. These may be provided in a text or binary PCK */
/*        file, or by a CK file. */

/*     The following data may be required: */

/*     -  SPK data: if aberration corrections are used, and if the ray */
/*        frame DREF is non-inertial, ephemeris data for that frame's */
/*        center must be loaded. The state of that object relative to */
/*        the solar system barycenter must be calculable from the */
/*        available ephemeris data. */

/*     -  Frame specifications: if a frame definition is required to */
/*        convert the observer and target states to the body-fixed frame */
/*        of the target, that definition must be available in the kernel */
/*        pool. Similarly, the frame definition required to map between */
/*        the frame designated by DREF and the target body-fixed frame */
/*        must be available. Typically the definitions of frames not */
/*        already built-in to SPICE are supplied by loading a frame */
/*        kernel. */

/*     -  Ray frame orientation data: if the frame to which DREF refers */
/*        is non-inertial, PCK or CK data for the frame's orientation */
/*        are required. If the frame is fixed to a spacecraft instrument */
/*        or structure, at least one CK file will be needed to permit */
/*        transformation of vectors between that frame and both the */
/*        J2000 and the target body-fixed frames. */

/*     -  Ray direction data: if the ray direction is defined by a */
/*        vector expressed in a spacecraft reference frame, an IK may be */
/*        required to provide the coordinates of the ray's direction in */
/*        that frame. */

/*     -  SCLK data: if a CK file is needed, an associated SCLK kernel */
/*        is required to enable conversion between encoded SCLK (used to */
/*        time-tag CK data) and barycentric dynamical time (TDB). */

/*     -  Leapseconds data: if SCLK data are needed, a leapseconds */
/*        kernel usually is needed as well. */

/*     -  Body name-ID mappings: if the target or observer name is */
/*        not built into the SPICE software, the mapping between the */
/*        name and the corresponding ID code must be present in the */
/*        kernel pool. Such mappings are usually introduced by loading */
/*        a frame kernel or other text kernel containing them. */

/*     In all cases, kernel data are normally loaded once per program */
/*     run, NOT every time this routine is called. */

/* $ Particulars */

/*     Given an observer, the direction vector of a ray emanating from */
/*     the observer, and an extended target body represented by a */
/*     triaxial ellipsoid, TANGPT computes the "tangent point": a point */
/*     nearest to the target body's surface nearest to the ray. The */
/*     corresponding surface point nearest to the tangent point is */
/*     computed as well. */

/*     For remote sensing observations, for maximum accuracy, reception */
/*     light time and stellar aberration corrections should be used. */
/*     These corrections model observer-target-ray geometry as it is */
/*     observed. */

/*     For signal transmission applications, for maximum accuracy, */
/*     transmission light time and stellar aberration corrections should */
/*     be used. These corrections model the observer-target-ray geometry */
/*     that applies to the transmitted signal. For example, these */
/*     corrections are needed to calculate the minimum altitude of the */
/*     signal's path over the target body. */

/*     In some cases, the magnitudes of light time and stellar */
/*     aberration corrections are negligible. When these corrections */
/*     can be ignored, significantly faster execution can be achieved */
/*     by setting the input ABCORR to 'NONE'. */

/*     This routine ignores differential aberration effects over the */
/*     target body's surface: it computes corrections only at a */
/*     user-specified point, which is called the "aberration correction */
/*     locus." The user may select either the tangent point or */
/*     corresponding surface point as the locus. In many cases, the */
/*     differences between corrections for these points are very small. */

/*     The $Examples header section below presents geometric cases for */
/*     which aberration correction magnitudes are significant, and cases */
/*     for which they are not. */

/* $ Examples */

/*     The numerical results shown for these examples may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */

/*     1) The following program computes tangent and surface points for */
/*        the MAVEN IUVS instrument. The observer is the MAVEN */
/*        spacecraft; the target body is Mars. The ray direction is */
/*        that of the boresight of the MAVEN IUVS instrument. */

/*        The aberration corrections used in this example are often */
/*        suitable for remote sensing observations: converged Newtonian */
/*        light time and stellar aberration "reception" corrections. In */
/*        some cases it is reasonable to omit aberration corrections; */
/*        see the second and third example programs below for */
/*        demonstrations of the effects of different aberration */
/*        correction choices. */

/*        In this example, the aberration correction locus is the */
/*        tangent point, meaning that converged light time and stellar */
/*        aberration corrections are computed for that point. The epoch */
/*        TRGEPC is used to compute the light time-corrected target */
/*        position and orientation, and the stellar aberration */
/*        correction applicable to the tangent point is applied to the */
/*        observer-target position vector, in order to model apparent */
/*        observation geometry. */

/*        Three geometric cases are covered by this example: */

/*           - The "normal" case, in which the ray defined by the */
/*             MAVEN IUVS boresight passes over Mars at low altitude. */

/*             In the example code, there are two computations that fall */
/*             into this category. */

/*           - The "intercept" case, in which the ray intersects Mars. */

/*           - The "look away" case, in which the elevation of the ray's */
/*             direction vector, measured from the local level plane at */
/*             the sub-spacecraft point, is greater than or equal to 0. */
/*             The aberration corrections used to compute the */
/*             sub-observer point for this case are those applicable to */
/*             the aberration correction locus. */

/*        Use the meta-kernel shown below to load the required SPICE */
/*        kernels. */


/*           KPL/MK */

/*           File name: tangpt_ex1.tm */

/*           This meta-kernel is intended to support operation of SPICE */
/*           example programs. The kernels shown here should not be */
/*           assumed to contain adequate or correct versions of data */
/*           required by SPICE-based user applications. */

/*           In order for an application to use this meta-kernel, the */
/*           kernels referenced here must be present in the user's */
/*           current working directory. */

/*           All kernels referenced by this meta-kernel are available */
/*           from the MAVEN SPICE PDS archive. */

/*           The names and contents of the kernels referenced */
/*           by this meta-kernel are as follows: */

/*           File name                          Contents */
/*           ---------                          -------- */
/*           mar097s.bsp                        Mars satellite ephemeris */
/*           maven_iuvs_v11.ti                  MAVEN IUVS instrument */
/*                                              information */
/*           maven_orb_rec_201001_210101_v1.bsp MAVEN s/c ephemeris */
/*           mvn_v09.tf                         MAVEN frame */
/*                                              specifications */
/*           mvn_app_rel_201005_201011_v01.bc   MAVEN Articulated */
/*                                              Payload Platform */
/*                                              attitude */
/*           mvn_iuvs_rem_201001_201231_v01.bc  MAVEN IUVS instrument */
/*                                              internal mirror */
/*                                              attitude */
/*           mvn_sc_rel_201005_201011_v01.bc    MAVEN s/c attitude */
/*           mvn_sclkscet_00086.tsc             MAVEN SCLK coefficients */
/*           naif0012.tls                       Leapseconds */
/*           pck00010.tpc                       Planet and satellite */
/*                                              orientation and radii */

/*           \begindata */

/*              KERNELS_TO_LOAD = ( */
/*                 'mar097s.bsp', */
/*                 'maven_iuvs_v11.ti', */
/*                 'maven_orb_rec_201001_210101_v1.bsp', */
/*                 'maven_v09.tf', */
/*                 'mvn_app_rel_201005_201011_v01.bc', */
/*                 'mvn_iuvs_rem_201001_201231_v01.bc', */
/*                 'mvn_sc_rel_201005_201011_v01.bc', */
/*                 'mvn_sclkscet_00086.tsc', */
/*                 'naif0012.tls', */
/*                 'pck00010.tpc' ) */

/*           \begintext */

/*           End of meta-kernel */


/*        Example code begins here. */


/*              PROGRAM TANGPT_EX1 */
/*              IMPLICIT NONE */

/*        C */
/*        C     Parameters */
/*        C */
/*              CHARACTER*(*)         FMT1F7 */
/*              PARAMETER           ( FMT1F7 = '(1X,A,F15.7)' ) */

/*              CHARACTER*(*)         FMT3F7 */
/*              PARAMETER           ( FMT3F7 = '(1X,A,3F15.7)' ) */

/*              CHARACTER*(*)         META */
/*              PARAMETER           ( META   = 'tangpt_ex1.tm' ) */

/*              CHARACTER*(*)         TIMFMT */
/*              PARAMETER           ( TIMFMT = */
/*             .          'YYYY-MM-DD HR:MN:SC.###### UTC ::RND' ) */

/*              INTEGER               BDNMLN */
/*              PARAMETER           ( BDNMLN = 36 ) */

/*              INTEGER               CORLEN */
/*              PARAMETER           ( CORLEN = 10 ) */

/*              INTEGER               FRNMLN */
/*              PARAMETER           ( FRNMLN = 32 ) */

/*              INTEGER               LNSIZE */
/*              PARAMETER           ( LNSIZE = 72 ) */

/*              INTEGER               LOCLEN */
/*              PARAMETER           ( LOCLEN = 25 ) */

/*              INTEGER               NCASE */
/*              PARAMETER           ( NCASE  =  3 ) */

/*              INTEGER               NTIMES */
/*              PARAMETER           ( NTIMES =  4 ) */

/*              INTEGER               ROOM */
/*              PARAMETER           ( ROOM   = 12 ) */

/*              INTEGER               SHPLEN */
/*              PARAMETER           ( SHPLEN = 25 ) */

/*              INTEGER               TIMLEN */
/*              PARAMETER           ( TIMLEN = 35 ) */

/*        C */
/*        C     Local variables */
/*        C */
/*              CHARACTER*(CORLEN)    ABCORR */
/*              CHARACTER*(LNSIZE)    CASENM */
/*              CHARACTER*(LNSIZE)    CASES  ( NCASE ) */
/*              CHARACTER*(FRNMLN)    FIXREF */
/*              CHARACTER*(BDNMLN)    INSNAM */
/*              CHARACTER*(LOCLEN)    LOCUS */
/*              CHARACTER*(BDNMLN)    OBSRVR */
/*              CHARACTER*(FRNMLN)    RAYFRM */
/*              CHARACTER*(SHPLEN)    SHAPE */
/*              CHARACTER*(BDNMLN)    TARGET */
/*              CHARACTER*(TIMLEN)    TIMSTR */
/*              CHARACTER*(TIMLEN)    UTCTIM ( NTIMES ) */

/*              DOUBLE PRECISION      ALT */
/*              DOUBLE PRECISION      BOUNDS ( 3, ROOM ) */
/*              DOUBLE PRECISION      ET */
/*              DOUBLE PRECISION      RANGE */
/*              DOUBLE PRECISION      RAYDIR ( 3 ) */
/*              DOUBLE PRECISION      SRFPT  ( 3 ) */
/*              DOUBLE PRECISION      SRFVEC ( 3 ) */
/*              DOUBLE PRECISION      TANPT  ( 3 ) */
/*              DOUBLE PRECISION      TRGEPC */

/*              INTEGER               I */
/*              INTEGER               NVEC */

/*        C */
/*        C     Initial values */
/*        C */
/*              DATA                  CASES / */
/*             .                      'Ray slightly above limb', */
/*             .                      'Intercept', */
/*             .                      'Look-away'    / */

/*              DATA                  INSNAM / 'MAVEN_IUVS' / */

/*              DATA                  UTCTIM / */
/*             .            '2020-10-11 16:01:43.000000 UTC', */
/*             .            '2020-10-11 16:17:43.000000 UTC', */
/*             .            '2020-10-11 16:49:07.000000 UTC', */
/*             .            '2020-10-11 17:12:08.000000 UTC' / */

/*        C */
/*        C     Load kernels. */
/*        C */
/*              CALL FURNSH ( META ) */

/*              WRITE(*,*) ' ' */
/*              WRITE(*, '(A)') 'Instrument: ' // INSNAM */
/*              WRITE(*,*) ' ' */

/*        C */
/*        C     Get the instrument reference frame name and */
/*        C     the instrument boresight direction in the */
/*        C     instrument frame. */
/*        C */
/*              CALL GETFVN ( INSNAM, ROOM,   SHAPE, */
/*             .              RAYFRM, RAYDIR, NVEC, BOUNDS ) */

/*        C */
/*        C     Initialize inputs to TANGPT, except for time. */
/*        C */
/*              TARGET = 'MARS' */
/*              OBSRVR = 'MAVEN' */
/*              FIXREF = 'IAU_MARS' */
/*              ABCORR = 'CN+S' */
/*              LOCUS  = 'TANGENT POINT' */

/*        C */
/*        C     Compute the apparent tangent point for each time. */
/*        C */
/*              WRITE(*,'(A)') 'Aberration correction:       ' // ABCORR */
/*              WRITE(*,'(A)') 'Aberration correction locus: ' // LOCUS */

/*              DO I = 1, NTIMES */

/*                 CALL STR2ET ( UTCTIM(I), ET ) */

/*                 CALL TANGPT ( 'ELLIPSOID', */
/*             .                 TARGET, ET,     FIXREF, ABCORR, */
/*             .                 LOCUS,  OBSRVR, RAYFRM, RAYDIR, */
/*             .                 TANPT,  ALT,    RANGE,  SRFPT, */
/*             .                 TRGEPC, SRFVEC                 ) */

/*        C */
/*        C        Set the label for the geometric case. */
/*        C */
/*                 IF ( ALT .EQ. 0 ) THEN */

/*                    CASENM = CASES(2) */

/*                 ELSE IF ( RANGE .EQ. 0.D0 ) THEN */

/*                    CASENM = CASES(3) */
/*                 ELSE */
/*                    CASENM = CASES(1) */
/*                 END IF */

/*        C */
/*        C        Convert the target epoch to a string for output. */
/*        C */
/*                 CALL TIMOUT ( TRGEPC, TIMFMT, TIMSTR ) */

/*                 WRITE(*,*) ' ' */

/*                 WRITE( *, '(A)') '  Observation Time = ' // UTCTIM(I) */
/*                 WRITE( *, '(A)') '  Target Time      = ' // TIMSTR */

/*                 WRITE(*, FMT1F7) '   ALT    (km) = ', ALT */
/*                 WRITE(*, FMT1F7) '   RANGE  (km) = ', RANGE */
/*                 WRITE(*, FMT3F7) '   TANPT  (km) = ', TANPT */
/*                 WRITE(*, FMT3F7) '   SRFPT  (km) = ', SRFPT */
/*                 WRITE(*, FMT3F7) '   SRFVEC (km) = ', SRFVEC */

/*                 WRITE( *, '(A)') '    Geometric case = ' // CASENM */

/*              END DO */

/*              WRITE(*,*) ' ' */

/*              END */


/*        When this program was executed on a PC/Linux/gfortran/64-bit */
/*        platform, the output was: */


/*        Instrument: MAVEN_IUVS */

/*        Aberration correction:       CN+S */
/*        Aberration correction locus: TANGENT POINT */

/*          Observation Time = 2020-10-11 16:01:43.000000 UTC */
/*          Target Time      = 2020-10-11 16:01:42.983021 UTC */
/*            ALT    (km) =      99.4262977 */
/*            RANGE  (km) =    5090.1928435 */
/*            TANPT  (km) =   -2273.0408575   1072.4423944  -2415.6104827 */
/*            SRFPT  (km) =   -2208.5678350   1042.0234063  -2346.3031728 */
/*            SRFVEC (km) =   -2138.0677257   3050.4078643   3470.3929222 */
/*            Geometric case = Ray slightly above limb */

/*          Observation Time = 2020-10-11 16:17:43.000000 UTC */
/*          Target Time      = 2020-10-11 16:17:42.993820 UTC */
/*            ALT    (km) =       0.0000000 */
/*            RANGE  (km) =    1852.8381880 */
/*            TANPT  (km) =     752.0909507  -1781.3912506  -2775.5390159 */
/*            SRFPT  (km) =     752.0909507  -1781.3912506  -2775.5390159 */
/*            SRFVEC (km) =    -700.9743439   1162.4766255   1261.0679662 */
/*            Geometric case = Intercept */

/*          Observation Time = 2020-10-11 16:49:07.000000 UTC */
/*          Target Time      = 2020-10-11 16:49:06.998907 UTC */
/*            ALT    (km) =     218.2661426 */
/*            RANGE  (km) =     327.7912133 */
/*            TANPT  (km) =    2479.8672359  -1772.2350525   1931.8678816 */
/*            SRFPT  (km) =    2330.3561559  -1665.3870838   1814.0966731 */
/*            SRFVEC (km) =      77.3692694    325.9571470   -207.0099587 */
/*            Geometric case = Ray slightly above limb */

/*          Observation Time = 2020-10-11 17:12:08.000000 UTC */
/*          Target Time      = 2020-10-11 17:12:08.000000 UTC */
/*            ALT    (km) =     969.2772042 */
/*            RANGE  (km) =       0.0000000 */
/*            TANPT  (km) =     -58.1087763   2034.6474343   3844.2010767 */
/*            SRFPT  (km) =     -45.2530638   1584.5115999   2985.8825113 */
/*            SRFVEC (km) =      12.8557125   -450.1358344   -858.3185654 */
/*            Geometric case = Look-away */


/*     2) The following program computes tangent and surface points for */
/*        the MRO MCS A1 instrument, for a single epoch. The observer is */
/*        the MRO spacecraft; the target body is Mars. The ray direction */
/*        is that of the boresight of the MRO MCS A1 instrument. */

/*        The aberration corrections used in this example are converged */
/*        Newtonian light time and stellar aberration corrections, */
/*        converged Newtonian light time alone, and "none." */

/*        For remote sensing observations made by a spacecraft in low */
/*        orbit about Mars, both the combination of light time and */
/*        stellar aberration corrections and omission of aberration */
/*        corrections may be valid. See the output of this program and */
/*        of the third example program below for examples of how results */
/*        differ due to the choice of aberration corrections. */

/*        Use of light time corrections alone is presented to */
/*        illustrate, by way of contrast, the effect of this choice. */
/*        This choice can be useful for testing but is unlikely to be */
/*        correct for modeling actual observation geometry. */

/*        Separate computations are performed using both the tangent */
/*        point and the corresponding surface point---the nearest point */
/*        on the target surface to the tangent point---as the aberration */
/*        correction locus. */

/*        Three geometric cases are covered by this example: */

/*           - The "normal" case, in which the ray defined by the */
/*             MRO MCS A1 boresight passes over Mars at low altitude. */

/*             In the example code, there are two computations that fall */
/*             into this category. */

/*           - The "intercept" case, in which the ray intersects Mars. */

/*           - The "look away" case, in which the elevation of the ray's */
/*             direction vector, measured from the local level plane at */
/*             the sub-spacecraft point, is greater than or equal to 0. */
/*             The target position and orientation used for this */
/*             computation are the same as those used to compute the */
/*             aberration correction locus. */


/*        Use the meta-kernel shown below to load the required SPICE */
/*        kernels. */


/*           KPL/MK */

/*           File: tangpt_ex2.tm */

/*           This meta-kernel is intended to support operation of SPICE */
/*           example programs. The kernels shown here should not be */
/*           assumed to contain adequate or correct versions of data */
/*           required by SPICE-based user applications. */

/*           In order for an application to use this meta-kernel, the */
/*           kernels referenced here must be present in the user's */
/*           current working directory. */

/*           All kernels referenced by this meta-kernel are available */
/*           from the MRO SPICE PDS archive. */

/*           The names and contents of the kernels referenced */
/*           by this meta-kernel are as follows: */

/*              File name                       Contents */
/*              ---------                       -------- */
/*              mar097.bsp                      Mars satellite ephemeris */
/*              mro_mcs_psp_201001_201031.bc    MRO MCS attitude */
/*              mro_mcs_v10.ti                  MRO MCS instrument */
/*                                              information */
/*              mro_psp57_ssd_mro95a.bsp        MRO s/c ephemeris */
/*              mro_sc_psp_201027_201102.bc     MRO s/c bus attitude */
/*              mro_sclkscet_00095_65536.tsc    MRO SCLK coefficients */
/*              mro_v16.tf                      MRO frame specifications */
/*              naif0012.tls                    Leapseconds */
/*              pck00008.tpc                    Planet and satellite */
/*                                              orientation and radii */

/*           \begindata */

/*              KERNELS_TO_LOAD = ( */
/*                 'mar097.bsp', */
/*                 'mro_mcs_psp_201001_201031.bc', */
/*                 'mro_mcs_v10.ti', */
/*                 'mro_psp57_ssd_mro95a.bsp', */
/*                 'mro_sc_psp_201027_201102.bc', */
/*                 'mro_sclkscet_00095_65536.tsc', */
/*                 'mro_v16.tf', */
/*                 'naif0012.tls', */
/*                 'pck00008.tpc' ) */

/*           \begintext */

/*           End of meta-kernel */


/*        Example code begins here. */


/*              PROGRAM TANGPT_EX2 */
/*              IMPLICIT NONE */

/*        C */
/*        C     Parameters */
/*        C */
/*              CHARACTER*(*)         FMT1F7 */
/*              PARAMETER           ( FMT1F7  = '(1X,A,F15.7)' ) */

/*              CHARACTER*(*)         FMT3F7 */
/*              PARAMETER           ( FMT3F7  = '(1X,A,3F15.7)' ) */

/*              CHARACTER*(*)         FMT1F4 */
/*              PARAMETER           ( FMT1F4 = '(1X,A,F10.4)' ) */

/*              CHARACTER*(*)         FMT3F4 */
/*              PARAMETER           ( FMT3F4 = '(1X,A,3F10.4)' ) */

/*              CHARACTER*(*)         META */
/*              PARAMETER           ( META   = 'tangpt_ex2.tm' ) */

/*              CHARACTER*(*)         TIMFMT */
/*              PARAMETER           ( TIMFMT = */
/*             .          'YYYY-MM-DD HR:MN:SC.###### UTC ::RND' ) */

/*              INTEGER               BDNMLN */
/*              PARAMETER           ( BDNMLN = 36 ) */

/*              INTEGER               CORLEN */
/*              PARAMETER           ( CORLEN = 10 ) */

/*              INTEGER               FRNMLN */
/*              PARAMETER           ( FRNMLN = 32 ) */

/*              INTEGER               LOCLEN */
/*              PARAMETER           ( LOCLEN = 25 ) */

/*              INTEGER               NCASE */
/*              PARAMETER           ( NCASE  =  5 ) */

/*              INTEGER               ROOM */
/*              PARAMETER           ( ROOM   =  4 ) */

/*              INTEGER               SHPLEN */
/*              PARAMETER           ( SHPLEN = 25 ) */

/*              INTEGER               TIMLEN */
/*              PARAMETER           ( TIMLEN = 35 ) */

/*        C */
/*        C     Local variables */
/*        C */
/*              CHARACTER*(CORLEN)    ABCORR */
/*              CHARACTER*(CORLEN)    CORRS  ( NCASE ) */
/*              CHARACTER*(FRNMLN)    FIXREF */
/*              CHARACTER*(BDNMLN)    INSNAM */
/*              CHARACTER*(LOCLEN)    LOCI   ( NCASE ) */
/*              CHARACTER*(LOCLEN)    LOCUS */
/*              CHARACTER*(BDNMLN)    OBSRVR */
/*              CHARACTER*(FRNMLN)    RAYFRM */
/*              CHARACTER*(SHPLEN)    SHAPE */
/*              CHARACTER*(BDNMLN)    TARGET */
/*              CHARACTER*(TIMLEN)    TIMSTR */
/*              CHARACTER*(TIMLEN)    UTCTIM */

/*              DOUBLE PRECISION      ALT */
/*              DOUBLE PRECISION      BOUNDS ( 3, ROOM ) */
/*              DOUBLE PRECISION      DIFF   ( 3 ) */
/*              DOUBLE PRECISION      ET */
/*              DOUBLE PRECISION      RANGE */
/*              DOUBLE PRECISION      RAYDIR ( 3 ) */
/*              DOUBLE PRECISION      SRFPT  ( 3 ) */
/*              DOUBLE PRECISION      SRFVEC ( 3 ) */
/*              DOUBLE PRECISION      SVALT */
/*              DOUBLE PRECISION      SVEPOC */
/*              DOUBLE PRECISION      SVRANG */
/*              DOUBLE PRECISION      SVSRFP ( 3 ) */
/*              DOUBLE PRECISION      SVSRFV ( 3 ) */
/*              DOUBLE PRECISION      SVTANP ( 3 ) */
/*              DOUBLE PRECISION      TANPT  ( 3 ) */
/*              DOUBLE PRECISION      TRGEPC */

/*              INTEGER               I */
/*              INTEGER               NVEC */

/*        C */
/*        C     Initial values */
/*        C */
/*              DATA                  CORRS  / 'CN+S', 'CN+S', */
/*             .                               'CN',   'CN', */
/*             .                               'NONE'           / */

/*              DATA                  INSNAM / 'MRO_MCS_A1'     / */

/*              DATA                  LOCI   / 'TANGENT POINT', */
/*             .                               'SURFACE POINT', */
/*             .                               'TANGENT POINT', */
/*             .                               'SURFACE POINT', */
/*             .                               'TANGENT POINT'  / */

/*              DATA                  UTCTIM / */
/*             .              '2020-10-31 00:01:23.111492 UTC'  / */

/*        C */
/*        C     Load kernels. */
/*        C */
/*              CALL FURNSH ( META ) */

/*              WRITE( *, * ) ' ' */
/*              WRITE( *, '(A)') 'Instrument: ' // INSNAM */

/*        C */
/*        C     Get the instrument reference frame name and */
/*        C     the instrument boresight direction in the */
/*        C     instrument frame. */
/*        C */
/*              CALL GETFVN ( INSNAM, ROOM,   SHAPE, */
/*             .              RAYFRM, RAYDIR, NVEC, BOUNDS ) */

/*        C */
/*        C     Initialize inputs to TANGPT that are common to all */
/*        C     cases. */
/*        C */
/*              TARGET = 'MARS' */
/*              OBSRVR = 'MRO' */
/*              FIXREF = 'IAU_MARS' */

/*        C */
/*        C     Compute the apparent tangent point for each case. */
/*        C */
/*              DO I = 1, NCASE */

/*                 WRITE(*,*) ' ' */

/*                 ABCORR = CORRS(I) */
/*                 LOCUS  = LOCI(I) */

/*                 WRITE(*, '(A)') 'Aberration correction:       ' */
/*             .   //               ABCORR */

/*                 WRITE(*, '(A)') 'Aberration correction locus: ' */
/*             .   //               LOCUS */

/*                 WRITE(*,*) ' ' */

/*                 CALL STR2ET ( UTCTIM, ET ) */

/*                 CALL TANGPT ( 'ELLIPSOID', */
/*             .                 TARGET, ET,     FIXREF, ABCORR, */
/*             .                 LOCUS,  OBSRVR, RAYFRM, RAYDIR, */
/*             .                 TANPT,  ALT,    RANGE,  SRFPT, */
/*             .                 TRGEPC, SRFVEC                 ) */

/*        C */
/*        C        Convert the target epoch to a string for output. */
/*        C */
/*                 CALL TIMOUT ( TRGEPC, TIMFMT, TIMSTR ) */

/*                 WRITE(*, '(A)') '  Observation Time = ' */
/*             .   //               UTCTIM */

/*                 WRITE(*, '(A)') '  Target Time      = ' */
/*             .   //                 TIMSTR */

/*                 WRITE(*, FMT1F7) '   ALT    (km) = ', ALT */
/*                 WRITE(*, FMT1F7) '   RANGE  (km) = ', RANGE */
/*                 WRITE(*, FMT3F7) '   TANPT  (km) = ', TANPT */
/*                 WRITE(*, FMT3F7) '   SRFPT  (km) = ', SRFPT */
/*                 WRITE(*, FMT3F7) '   SRFVEC (km) = ', SRFVEC */

/*                 IF ( I .EQ. 1 ) THEN */
/*        C */
/*        C           Save results for comparison. */
/*        C */
/*                    SVALT  = ALT */
/*                    SVEPOC = TRGEPC */
/*                    SVRANG = RANGE */
/*                    CALL VEQU( TANPT,  SVTANP ) */
/*                    CALL VEQU( SRFPT,  SVSRFP ) */
/*                    CALL VEQU( SRFVEC, SVSRFV ) */

/*                 ELSE */
/*        C */
/*        C           Compare results to CN+S, tangent point */
/*        C           locus case. */
/*        C */
/*                    WRITE(*,*) ' ' */

/*                    WRITE(*, '(A)') */
/*             .      '  Differences from case 1 outputs:' */

/*                    WRITE(*, FMT1F4) */
/*             .      '   Target time delta (ms) = ', */
/*             .      1.D3 * ( TRGEPC - SVEPOC ) */

/*                    WRITE(*, FMT1F4) */
/*             .      '   ALT    delta (m) = ', */
/*             .      1.D3 * ( ALT - SVALT ) */

/*                    WRITE(*, FMT1F4) */
/*             .      '   RANGE  delta (m) = ', */
/*             .      1.D3 * ( RANGE - SVRANG  ) */

/*                    CALL VSUB   ( TANPT, SVTANP, DIFF ) */
/*                    CALL VSCLIP ( 1.D3,  DIFF ) */
/*                    WRITE(*, FMT3F4) */
/*             .      '   TANPT  delta (m) = ', DIFF */

/*                    CALL VSUB   ( SRFPT, SVSRFP, DIFF ) */
/*                    CALL VSCLIP ( 1.D3,  DIFF ) */
/*                    WRITE(*, FMT3F4) */
/*             .      '   SRFPT  delta (m) = ', DIFF */

/*                    CALL VSUB   ( SRFVEC, SVSRFV, DIFF ) */
/*                    CALL VSCLIP ( 1.D3,   DIFF ) */
/*                    WRITE(*, FMT3F4) */
/*             .      '   SRFVEC delta (m) = ', DIFF */

/*                 END IF */

/*                 WRITE(*,*) ' ' */

/*              END DO */

/*              END */


/*        When this program was executed on a PC/Linux/gfortran/64-bit */
/*        platform, the output was: */


/*        Instrument: MRO_MCS_A1 */

/*        Aberration correction:       CN+S */
/*        Aberration correction locus: TANGENT POINT */

/*          Observation Time = 2020-10-31 00:01:23.111492 UTC */
/*          Target Time      = 2020-10-31 00:01:23.106946 UTC */
/*            ALT    (km) =      39.1034486 */
/*            RANGE  (km) =    1362.8659249 */
/*            TANPT  (km) =   -2530.9040220  -1630.9806346   1644.3612074 */
/*            SRFPT  (km) =   -2502.1342299  -1612.4406294   1625.4496512 */
/*            SRFVEC (km) =    -589.3842679   -234.0892764  -1206.9635473 */


/*        Aberration correction:       CN+S */
/*        Aberration correction locus: SURFACE POINT */

/*          Observation Time = 2020-10-31 00:01:23.111492 UTC */
/*          Target Time      = 2020-10-31 00:01:23.106944 UTC */
/*            ALT    (km) =      39.1014434 */
/*            RANGE  (km) =    1362.8679108 */
/*            TANPT  (km) =   -2530.9025464  -1630.9796845   1644.3602376 */
/*            SRFPT  (km) =   -2502.1342295  -1612.4406300   1625.4496511 */
/*            SRFVEC (km) =    -589.3866439   -234.0905954  -1206.9643086 */

/*          Differences from case 1 outputs: */
/*            Target time delta (ms) =    -0.0019 */
/*            ALT    delta (m) =    -2.0052 */
/*            RANGE  delta (m) =     1.9859 */
/*            TANPT  delta (m) =     1.4757    0.9501   -0.9698 */
/*            SRFPT  delta (m) =     0.0004   -0.0006   -0.0000 */
/*            SRFVEC delta (m) =    -2.3760   -1.3189   -0.7614 */


/*        Aberration correction:       CN */
/*        Aberration correction locus: TANGENT POINT */

/*          Observation Time = 2020-10-31 00:01:23.111492 UTC */
/*          Target Time      = 2020-10-31 00:01:23.106946 UTC */
/*            ALT    (km) =      39.1714711 */
/*            RANGE  (km) =    1362.8658567 */
/*            TANPT  (km) =   -2530.9135880  -1631.0820975   1644.3878335 */
/*            SRFPT  (km) =   -2502.0942100  -1612.5090527   1625.4434517 */
/*            SRFVEC (km) =    -589.3346511   -234.0562242  -1206.9963133 */

/*          Differences from case 1 outputs: */
/*            Target time delta (ms) =     0.0000 */
/*            ALT    delta (m) =    68.0225 */
/*            RANGE  delta (m) =    -0.0683 */
/*            TANPT  delta (m) =    -9.5660 -101.4629   26.6261 */
/*            SRFPT  delta (m) =    40.0199  -68.4233   -6.1994 */
/*            SRFVEC delta (m) =    49.6168   33.0522  -32.7661 */


/*        Aberration correction:       CN */
/*        Aberration correction locus: SURFACE POINT */

/*          Observation Time = 2020-10-31 00:01:23.111492 UTC */
/*          Target Time      = 2020-10-31 00:01:23.106944 UTC */
/*            ALT    (km) =      39.1714973 */
/*            RANGE  (km) =    1362.8658326 */
/*            TANPT  (km) =   -2530.9135902  -1631.0821391   1644.3878436 */
/*            SRFPT  (km) =   -2502.0941931  -1612.5090815   1625.4434492 */
/*            SRFVEC (km) =    -589.3346210   -234.0562071  -1206.9963050 */

/*          Differences from case 1 outputs: */
/*            Target time delta (ms) =    -0.0019 */
/*            ALT    delta (m) =    68.0487 */
/*            RANGE  delta (m) =    -0.0924 */
/*            TANPT  delta (m) =    -9.5682 -101.5045   26.6362 */
/*            SRFPT  delta (m) =    40.0368  -68.4521   -6.2020 */
/*            SRFVEC delta (m) =    49.6469   33.0694  -32.7577 */


/*        Aberration correction:       NONE */
/*        Aberration correction locus: TANGENT POINT */

/*          Observation Time = 2020-10-31 00:01:23.111492 UTC */
/*          Target Time      = 2020-10-31 00:01:23.111492 UTC */
/*            ALT    (km) =      39.1090103 */
/*            RANGE  (km) =    1362.9233525 */
/*            TANPT  (km) =   -2530.9082604  -1630.9831041   1644.3638384 */
/*            SRFPT  (km) =   -2502.1343747  -1612.4404639   1625.4495931 */
/*            SRFVEC (km) =    -589.4063032   -234.0970874  -1207.0162978 */

/*          Differences from case 1 outputs: */
/*            Target time delta (ms) =     4.5460 */
/*            ALT    delta (m) =     5.5616 */
/*            RANGE  delta (m) =    57.4276 */
/*            TANPT  delta (m) =    -4.2384   -2.4695    2.6310 */
/*            SRFPT  delta (m) =    -0.1448    0.1655   -0.0581 */
/*            SRFVEC delta (m) =   -22.0352   -7.8109  -52.7505 */


/*     3) The following program computes tangent and surface points for */
/*        a ray pointing from the Goldstone DSN station DSS-14 to the */
/*        location of the MRO spacecraft, for a single epoch. The target */
/*        body is Mars. */

/*        The aberration corrections used in this example are */

/*           CN+S */
/*           XCN+S */
/*           CN */
/*           NONE */

/*        Results using CN+S corrections are computed for both locus */
/*        choices: TANGENT POINT and SURFACE POINT. */

/*        For each case other than the one using CN+S corrections for */
/*        the TANGENT POINT locus, differences between results for the */
/*        former and latter case are shown. */

/*        Use the meta-kernel shown below to load the required SPICE */
/*        kernels. */


/*           KPL/MK */

/*           File: tangpt_ex3.tm */

/*           This meta-kernel is intended to support operation of SPICE */
/*           example programs. The kernels shown here should not be */
/*           assumed to contain adequate or correct versions of data */
/*           required by SPICE-based user applications. */

/*           In order for an application to use this meta-kernel, the */
/*           kernels referenced here must be present in the user's */
/*           current working directory. */

/*           All kernels referenced by this meta-kernel are available */
/*           from the NAIF SPICE server in the generic kernels area */
/*           or from the MRO SPICE PDS archive. */

/*           The names and contents of the kernels referenced */
/*           by this meta-kernel are as follows: */

/*              File name                       Contents */
/*              ---------                       -------- */
/*              mar097.bsp                      Mars satellite ephemeris */
/*              mro_psp57_ssd_mro95a.bsp        MRO s/c ephemeris */
/*              earthstns_itrf93_201023.bsp     DSN station locations */
/*              naif0012.tls                    Leapseconds */
/*              pck00010.tpc                    Planet and satellite */
/*                                              orientation and radii */
/*              earth_latest_high_prec.bpc      High accuracy Earth */
/*                                              attitude */

/*           \begindata */

/*              KERNELS_TO_LOAD = ( */
/*                 'mar097.bsp' */
/*                 'mro_psp57_ssd_mro95a.bsp' */
/*                 'earthstns_itrf93_201023.bsp' */
/*                 'naif0012.tls' */
/*                 'pck00010.tpc' */
/*                 'earth_latest_high_prec.bpc' ) */

/*           \begintext */

/*           End of meta-kernel */


/*        Example code begins here. */


/*              PROGRAM TANGPT_EX3 */
/*              IMPLICIT NONE */

/*        C */
/*        C     Parameters */
/*        C */
/*              CHARACTER*(*)         FMT1F3 */
/*              PARAMETER           ( FMT1F3  = '(1X,A,F14.3)' ) */

/*              CHARACTER*(*)         FMT3F3 */
/*              PARAMETER           ( FMT3F3  = '(1X,A,3F14.3)' ) */

/*              CHARACTER*(*)         FMT1F6 */
/*              PARAMETER           ( FMT1F6  = '(1X,A,F14.6)' ) */

/*              CHARACTER*(*)         FMTMF3 */
/*              PARAMETER           ( FMTMF3 = '(1X,A,2F13.3,F10.3)' ) */

/*              CHARACTER*(*)         META */
/*              PARAMETER           ( META   = 'tangpt_ex3.tm' ) */

/*              CHARACTER*(*)         TIMFMT */
/*              PARAMETER           ( TIMFMT = */
/*             .          'YYYY-MM-DD HR:MN:SC.###### UTC ::RND' ) */

/*              INTEGER               BDNMLN */
/*              PARAMETER           ( BDNMLN = 36 ) */

/*              INTEGER               CORLEN */
/*              PARAMETER           ( CORLEN = 10 ) */

/*              INTEGER               FRNMLN */
/*              PARAMETER           ( FRNMLN = 32 ) */

/*              INTEGER               LOCLEN */
/*              PARAMETER           ( LOCLEN = 25 ) */

/*              INTEGER               NCASE */
/*              PARAMETER           ( NCASE  =  5 ) */

/*              INTEGER               TIMLEN */
/*              PARAMETER           ( TIMLEN = 35 ) */

/*        C */
/*        C     Local variables */
/*        C */
/*              CHARACTER*(CORLEN)    ABCORR */
/*              CHARACTER*(CORLEN)    CORRS  ( NCASE ) */
/*              CHARACTER*(FRNMLN)    FIXREF */
/*              CHARACTER*(LOCLEN)    LOCI   ( NCASE ) */
/*              CHARACTER*(LOCLEN)    LOCUS */
/*              CHARACTER*(BDNMLN)    OBSRVR */
/*              CHARACTER*(FRNMLN)    RAYFRM */
/*              CHARACTER*(BDNMLN)    SC */
/*              CHARACTER*(BDNMLN)    TARGET */
/*              CHARACTER*(TIMLEN)    TIMSTR */
/*              CHARACTER*(TIMLEN)    UTCTIM */

/*              DOUBLE PRECISION      ALT */
/*              DOUBLE PRECISION      DIFF   ( 3 ) */
/*              DOUBLE PRECISION      ET */
/*              DOUBLE PRECISION      RANGE */
/*              DOUBLE PRECISION      RAYDIR ( 3 ) */
/*              DOUBLE PRECISION      RAYLT */
/*              DOUBLE PRECISION      SRFPT  ( 3 ) */
/*              DOUBLE PRECISION      SRFVEC ( 3 ) */
/*              DOUBLE PRECISION      SVALT */
/*              DOUBLE PRECISION      SVEPOC */
/*              DOUBLE PRECISION      SVRANG */
/*              DOUBLE PRECISION      SVSRFP ( 3 ) */
/*              DOUBLE PRECISION      SVSRFV ( 3 ) */
/*              DOUBLE PRECISION      SVTANP ( 3 ) */
/*              DOUBLE PRECISION      TANPT  ( 3 ) */
/*              DOUBLE PRECISION      TRGEPC */

/*              INTEGER               I */

/*        C */
/*        C     Initial values */
/*        C */
/*              DATA                  CORRS  / 'CN+S', 'XCN+S', */
/*             .                               'CN',   'NONE', */
/*             .                               'CN+S'          / */

/*              DATA                  LOCI   / 'TANGENT POINT', */
/*             .                               'TANGENT POINT', */
/*             .                               'TANGENT POINT', */
/*             .                               'TANGENT POINT', */
/*             .                               'SURFACE POINT'  / */

/*              DATA                  UTCTIM / */
/*             .              '2020-12-30 00:00:00 UTC'  / */

/*        C */
/*        C     Load kernels. */
/*        C */
/*              CALL FURNSH ( META ) */

/*        C */
/*        C     Set name of spacecraft used to define ray direction. */
/*        C */
/*              SC = 'MRO' */

/*        C */
/*        C     Initialize inputs to TANGPT that are common to all */
/*        C     cases. */
/*        C */
/*              TARGET = 'MARS' */
/*              OBSRVR = 'DSS-14' */
/*              FIXREF = 'IAU_MARS' */
/*              RAYFRM = 'J2000' */

/*        C */
/*        C     Convert observation time to TDB seconds past J2000. */
/*        C */
/*              CALL STR2ET ( UTCTIM, ET ) */

/*        C */
/*        C     Generate ray direction vector. Use apparent position */
/*        C     of the MRO spacecraft. */
/*        C */
/*              CALL SPKPOS ( SC,     ET,     RAYFRM, */
/*             .              'CN+S', OBSRVR, RAYDIR, RAYLT ) */

/*              WRITE(*,*) ' ' */
/*              WRITE( *, '(A)') 'Observer:   ' // OBSRVR */
/*              WRITE( *, '(A)') 'Target:     ' // TARGET */
/*              WRITE( *, '(A)') 'Spacecraft: ' // SC */

/*        C */
/*        C     Compute the apparent tangent point for each case. */
/*        C */
/*              DO I = 1, NCASE */

/*                 WRITE(*,*) ' ' */

/*                 ABCORR = CORRS(I) */
/*                 LOCUS  = LOCI(I) */

/*                 WRITE(*, '(A)') 'Aberration correction:       ' */
/*             .   //               ABCORR */

/*                 WRITE(*, '(A)') 'Aberration correction locus: ' */
/*             .   //               LOCUS */

/*                 WRITE(*,*) ' ' */

/*        C */
/*        C        Compute tangent point. */
/*        C */
/*                 CALL TANGPT ( 'ELLIPSOID', */
/*             .                 TARGET, ET,     FIXREF, ABCORR, */
/*             .                 LOCUS,  OBSRVR, RAYFRM, RAYDIR, */
/*             .                 TANPT,  ALT,    RANGE,  SRFPT, */
/*             .                 TRGEPC, SRFVEC                 ) */

/*        C */
/*        C        Convert the target epoch to a string for output. */
/*        C */
/*                 CALL TIMOUT ( TRGEPC, TIMFMT, TIMSTR ) */

/*                 WRITE(*, '(A)') '  Observation Time = ' */
/*             .   //               UTCTIM */

/*                 WRITE(*, '(A)') '  Target Time      = ' */
/*             .   //                 TIMSTR */

/*                 WRITE(*, FMT1F3) '   ALT    (km) = ', ALT */
/*                 WRITE(*, FMT1F3) '   RANGE  (km) = ', RANGE */
/*                 WRITE(*, FMT3F3) '   TANPT  (km) = ', TANPT */
/*                 WRITE(*, FMT3F3) '   SRFPT  (km) = ', SRFPT */
/*                 WRITE(*, FMT3F3) '   SRFVEC (km) = ', SRFVEC */

/*                 IF ( I .EQ. 1 ) THEN */
/*        C */
/*        C           Save results for comparison. */
/*        C */
/*                    SVALT  = ALT */
/*                    SVEPOC = TRGEPC */
/*                    SVRANG = RANGE */
/*                    CALL VEQU( TANPT,  SVTANP ) */
/*                    CALL VEQU( SRFPT,  SVSRFP ) */
/*                    CALL VEQU( SRFVEC, SVSRFV ) */

/*                 ELSE */
/*        C */
/*        C           Compare results to CN+S, tangent point */
/*        C           locus case. */
/*        C */
/*                    WRITE(*,*) ' ' */

/*                    WRITE(*, '(A)') */
/*             .      '  Differences from case 1 outputs:' */

/*                    WRITE(*, FMT1F6) */
/*             .      '   Target time delta (s) = ', */
/*             .      TRGEPC - SVEPOC */

/*                    WRITE(*, FMTMF3) */
/*             .      '   ALT    delta (km) = ', ALT - SVALT */

/*                    WRITE(*, FMTMF3) */
/*             .      '   RANGE  delta (km) = ', RANGE - SVRANG */

/*                    CALL VSUB   ( TANPT, SVTANP, DIFF ) */
/*                    WRITE(*, FMTMF3) */
/*             .      '   TANPT  delta (km) = ', DIFF */

/*                    CALL VSUB   ( SRFPT, SVSRFP, DIFF ) */
/*                    WRITE(*, FMTMF3) */
/*             .      '   SRFPT  delta (km) = ', DIFF */

/*                    CALL VSUB   ( SRFVEC, SVSRFV, DIFF ) */
/*                    WRITE(*, FMTMF3) */
/*             .      '   SRFVEC delta (km) = ', DIFF */

/*                 END IF */

/*                 WRITE(*,*) ' ' */

/*              END DO */

/*              END */


/*        When this program was executed on a PC/Linux/gfortran/64-bit */
/*        platform, the output was: */


/*        Observer:   DSS-14 */
/*        Target:     MARS */
/*        Spacecraft: MRO */

/*        Aberration correction:       CN+S */
/*        Aberration correction locus: TANGENT POINT */

/*          Observation Time = 2020-12-30 00:00:00 UTC */
/*          Target Time      = 2020-12-29 23:52:40.613204 UTC */
/*            ALT    (km) =        140.295 */
/*            RANGE  (km) =  131724847.608 */
/*            TANPT  (km) =       1351.574      1182.155     -3029.495 */
/*            SRFPT  (km) =       1298.181      1135.455     -2908.454 */
/*            SRFVEC (km) =  121233989.354  -5994858.328  51164606.676 */


/*        Aberration correction:       XCN+S */
/*        Aberration correction locus: TANGENT POINT */

/*          Observation Time = 2020-12-30 00:00:00 UTC */
/*          Target Time      = 2020-12-30 00:07:19.347692 UTC */
/*            ALT    (km) =       4921.539 */
/*            RANGE  (km) =  131713124.520 */
/*            TANPT  (km) =       -413.404     -8220.856     -1193.471 */
/*            SRFPT  (km) =       -168.808     -3356.879      -483.938 */
/*            SRFVEC (km) =  120615301.766 -13523495.083  51160641.665 */

/*          Differences from case 1 outputs: */
/*            Target time delta (s) =     878.734488 */
/*            ALT    delta (km) =      4781.244 */
/*            RANGE  delta (km) =    -11723.089 */
/*            TANPT  delta (km) =     -1764.978    -9403.011  1836.024 */
/*            SRFPT  delta (km) =     -1466.989    -4492.334  2424.517 */
/*            SRFVEC delta (km) =   -618687.588 -7528636.755 -3965.012 */


/*        Aberration correction:       CN */
/*        Aberration correction locus: TANGENT POINT */

/*          Observation Time = 2020-12-30 00:00:00 UTC */
/*          Target Time      = 2020-12-29 23:52:40.613219 UTC */
/*            ALT    (km) =       3409.162 */
/*            RANGE  (km) =  131724843.177 */
/*            TANPT  (km) =       1933.641      5183.696     -3951.091 */
/*            SRFPT  (km) =        965.945      2589.501     -1962.095 */
/*            SRFVEC (km) =  121233070.966  -5997405.747  51166472.910 */

/*          Differences from case 1 outputs: */
/*            Target time delta (s) =       0.000015 */
/*            ALT    delta (km) =      3268.868 */
/*            RANGE  delta (km) =        -4.431 */
/*            TANPT  delta (km) =       582.067     4001.541  -921.596 */
/*            SRFPT  delta (km) =      -332.236     1454.046   946.360 */
/*            SRFVEC delta (km) =      -918.388    -2547.420  1866.234 */


/*        Aberration correction:       NONE */
/*        Aberration correction locus: TANGENT POINT */

/*          Observation Time = 2020-12-30 00:00:00 UTC */
/*          Target Time      = 2020-12-30 00:00:00.000000 UTC */
/*            ALT    (km) =        781.382 */
/*            RANGE  (km) =  131718986.013 */
/*            TANPT  (km) =        615.190     -3545.867     -2111.285 */
/*            SRFPT  (km) =        500.266     -2883.463     -1713.075 */
/*            SRFVEC (km) =  120983074.323  -9765994.151  51162607.074 */

/*          Differences from case 1 outputs: */
/*            Target time delta (s) =     439.386796 */
/*            ALT    delta (km) =       641.087 */
/*            RANGE  delta (km) =     -5861.595 */
/*            TANPT  delta (km) =      -736.384    -4728.022   918.210 */
/*            SRFPT  delta (km) =      -797.915    -4018.919  1195.379 */
/*            SRFVEC delta (km) =   -250915.031 -3771135.823 -1999.603 */


/*        Aberration correction:       CN+S */
/*        Aberration correction locus: SURFACE POINT */

/*          Observation Time = 2020-12-30 00:00:00 UTC */
/*          Target Time      = 2020-12-29 23:52:40.613204 UTC */
/*            ALT    (km) =        140.308 */
/*            RANGE  (km) =  131724847.611 */
/*            TANPT  (km) =       1351.579      1182.159     -3029.507 */
/*            SRFPT  (km) =       1298.181      1135.455     -2908.454 */
/*            SRFVEC (km) =  121233989.351  -5994858.332  51164606.689 */

/*          Differences from case 1 outputs: */
/*            Target time delta (s) =       0.000000 */
/*            ALT    delta (km) =         0.013 */
/*            RANGE  delta (km) =         0.003 */
/*            TANPT  delta (km) =         0.005        0.004    -0.012 */
/*            SRFPT  delta (km) =        -0.000        0.000     0.000 */
/*            SRFVEC delta (km) =        -0.003       -0.005     0.013 */


/* $ Restrictions */

/*     1)  This routine is applicable only to computations for which */
/*         radiation paths can be modeled as straight lines. */

/*     2)  This routine does not account for differential aberration */
/*         corrections across the target body surface: when aberration */
/*         corrections are used, the entire target ellipsoid's position */
/*         and orientation are modified by the corrections that apply at */
/*         the aberration correction locus. */

/*     3)  A cautionary note: if aberration corrections are used, and if */
/*         DREF is the target body-fixed frame, the epoch at which that */
/*         frame is evaluated is offset from ET by the light time */
/*         between the observer and the *center* of the target body. */
/*         This light time normally will differ from the light time */
/*         between the observer and the tangent or surface point. */
/*         Consequently the orientation of the target body-fixed frame */
/*         at TRGEPC will not match that of the target body-fixed frame */
/*         at the epoch associated with DREF. As a result, various */
/*         derived quantities may not be as expected: for example, */
/*         SRFVEC would not be parallel to DVEC. */

/*         In many applications the errors arising from this frame */
/*         discrepancy may be insignificant; however a safe approach is */
/*         to always use as DREF a frame other than the target */
/*         body-fixed frame. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     M. Costa Sitja     (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 20-OCT-2021 (NJB) (MCS) */

/* -& */
/* $ Index_Entries */

/*     find ray-ellipsoid tangent point */
/*     find nearest point to ray on ellipsoid */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     Limit on relative error in light time; this is used to terminate */
/*     the solution loop. */

/*     This value is meant to ensure timely loop termination in cases */
/*     where extended precision computations cause successive values to */
/*     differ by amounts having magnitude less than the minimum */
/*     relative value representable by IEEE-754 conforming, 64-bit */
/*     double precision floating point numbers. */

/*     The convergence tests used here are not necessarily sensitive to */
/*     use of extended precision, but it is possible that future changes */
/*     to the code could make them so. */

/*     In most situations, use of this value enforces convergence. In */
/*     rare cases, successive approximate solutions will differ by */
/*     small, non-zero amounts but will not converge. In those cases, */
/*     iteration will be terminated when the iteration count limit, */
/*     which is dependent on the choice of aberration correction, is */
/*     reached. */


/*     Upper bound on converged solution iterations. */


/*     Upper bound on non-converged solution iterations. */


/*     Saved body name length. */


/*     Saved frame name length. */


/*     Locus string length. */


/*     Code for the frame J2000. */


/*     Local variables */


/*     Saved body name/ID item declarations. */


/*     Saved frame name/ID item declarations. */


/*     Saved target radii declarations. */


/*     Saved surface name/ID item declarations. To be used if DSK */
/*     shapes are supported. */

/*      INTEGER               SVCTR6 ( CTRSIZ ) */

/*     Saved variables */


/*     Saved name/ID items. */


/*     Saved frame name/ID items. */


/*     Saved target radii items. */


/*     To be used if DSK shapes are supported: */

/*     Saved surface name/ID items. */

/*      SAVE                  SVCTR6 */


/*     Initial values */

    if (return_()) {
	return 0;
    }
    chkin_("TANGPT", (ftnlen)6);

/*     Counter initialization is done separately. */

    if (first) {

/*        Initialize counters. */

	zzctruin_(svctr1);
	zzctruin_(svctr2);
	zzctruin_(svctr3);
	zzctruin_(svctr4);
	zzctruin_(svctr5);

/*        To be used if DSK shapes are supported: */

/*         CALL ZZCTRUIN( SVCTR6 ) */
    }

/*     Parse the aberration correction specifier, if it's new. */

    if (first || s_cmp(abcorr, prvcor, abcorr_len, (ftnlen)5) != 0) {

/*        PRVCOR is updated only when a valid correction has been */
/*        recognized. PRVCOR is blank on the first pass; afterward */
/*        it is always valid. */

/*        The aberration correction flag differs from the value it */
/*        had on the previous call, if any. Analyze the new flag. */

	zzvalcor_(abcorr, attblk, abcorr_len);
	if (failed_()) {
	    chkout_("TANGPT", (ftnlen)6);
	    return 0;
	}

/*        Set logical flags indicating the attributes of the requested */
/*        correction: */

/*           XMIT is .TRUE. when the correction is for transmitted */
/*           radiation. */

/*           USELT is .TRUE. when any type of light time correction */
/*           (normal or converged Newtonian) is specified. */

/*           USECN indicates converged Newtonian light time correction. */

/*           USESTL indicates stellar aberration corrections. */


/*        The above definitions are consistent with those used by */
/*        ZZVALCOR. */

	xmit = attblk[4];
	uselt = attblk[1];
	usecn = attblk[3];
	usestl = attblk[2];

/*        The aberration correction flag is valid; save it. */

	s_copy(prvcor, abcorr, (ftnlen)5, abcorr_len);

/*        FIRST will be set to .FALSE. later, after all first-pass */
/*        actions have been performed. */

    }

/*     Get the sign S prefixing LT in the expression for TRGEPC. */
/*     When light time correction is not used, setting S = 0 */
/*     allows us to seamlessly set TRGEPC equal to ET. */

    if (uselt) {
	if (xmit) {
	    s = 1.;
	} else {
	    s = -1.;
	}
    } else {
	s = 0.;
    }

/*     The method cannot be anything other than 'ELLIPSOID'. */

    if (! eqstr_(method, "ELLIPSOID", method_len, (ftnlen)9)) {
	setmsg_("Method is currently restricted to ELLIPSOID, but input valu"
		"e was #.", (ftnlen)67);
	errch_("#", method, (ftnlen)1, method_len);
	sigerr_("SPICE(NOTSUPPORTED)", (ftnlen)19);
	chkout_("TANGPT", (ftnlen)6);
	return 0;
    }

/*     Code from this point onward assumes the target shape is modeled */
/*     as a triaxial ellipsoid. */

/*     If we're using aberration corrections, the aberration correction */
/*     locus must be equivalent to one of 'TANGENT POINT' or 'SURFACE */
/*     POINT'. TANLOC is set to .TRUE. if and only if the locus is the */
/*     tangent point. */

    if (first || s_cmp(corloc, prvloc, corloc_len, (ftnlen)15) != 0) {

/*        Left justify the input locus string, convert to upper case, */
/*        and compress all embedded blanks for comparison. */

	ljucrs_(&c__0, corloc, locstr, corloc_len, (ftnlen)15);
	if (s_cmp(locstr, "TANGENTPOINT", (ftnlen)15, (ftnlen)12) == 0) {
	    tanloc = TRUE_;
	} else if (s_cmp(locstr, "SURFACEPOINT", (ftnlen)15, (ftnlen)12) == 0)
		 {
	    tanloc = FALSE_;
	} else {
	    setmsg_("Aberration correction locus must be one of TANGENT POIN"
		    "T or SURFACE POINT but was #.", (ftnlen)84);
	    errch_("#", corloc, (ftnlen)1, corloc_len);
	    sigerr_("SPICE(NOTSUPPORTED)", (ftnlen)19);
	    chkout_("TANGPT", (ftnlen)6);
	    return 0;
	}

/*        At this point we have a valid locus. TANLOC is set. */
/*        Save the input locus string so we can check for */
/*        a change on the next call. */

	s_copy(prvloc, corloc, (ftnlen)15, corloc_len);
    }

/*     Check for a zero ray direction vector. */

    if (vzero_(dvec)) {
	setmsg_("Input ray direction was the zero vector; this vector must b"
		"e non-zero.", (ftnlen)70);
	sigerr_("SPICE(ZEROVECTOR)", (ftnlen)17);
	chkout_("TANGPT", (ftnlen)6);
	return 0;
    }

/*     Obtain integer codes for the target and observer. */

    zzbods2c_(svctr1, svtarg, &svtcde, &svfnd1, target, &trgcde, &fnd, (
	    ftnlen)36, target_len);
    if (failed_()) {
	chkout_("TANGPT", (ftnlen)6);
	return 0;
    }
    if (! fnd) {
	setmsg_("The target, '#', is not a recognized name for an ephemeris "
		"object. The cause of this problem may be that you need an up"
		"dated version of the SPICE Toolkit, or that you failed to lo"
		"ad a kernel containing a name-ID mapping for this body.", (
		ftnlen)234);
	errch_("#", target, (ftnlen)1, target_len);
	sigerr_("SPICE(IDCODENOTFOUND)", (ftnlen)21);
	chkout_("TANGPT", (ftnlen)6);
	return 0;
    }
    zzbods2c_(svctr2, svobsr, &svobsc, &svfnd2, obsrvr, &obscde, &fnd, (
	    ftnlen)36, obsrvr_len);
    if (failed_()) {
	chkout_("TANGPT", (ftnlen)6);
	return 0;
    }
    if (! fnd) {
	setmsg_("The observer, '#', is not a recognized name for an ephemeri"
		"s object. The cause of this problem may be that you need an "
		"updated version of the SPICE Toolkit, or that you failed to "
		"load a kernel containing a name-ID mapping for this body.", (
		ftnlen)236);
	errch_("#", obsrvr, (ftnlen)1, obsrvr_len);
	sigerr_("SPICE(IDCODENOTFOUND)", (ftnlen)21);
	chkout_("TANGPT", (ftnlen)6);
	return 0;
    }

/*     Check the input body codes. If they are equal, signal */
/*     an error. */

    if (obscde == trgcde) {
	setmsg_("The observing body and target body are the same. Both are #."
		, (ftnlen)60);
	errch_("#", obsrvr, (ftnlen)1, obsrvr_len);
	sigerr_("SPICE(BODIESNOTDISTINCT)", (ftnlen)24);
	chkout_("TANGPT", (ftnlen)6);
	return 0;
    }

/*     Get the target body's ellipsoid radii. */

    if (first || trgcde != prvtcd) {

/*        This the first pass, or else the target body has changed. We */
/*        need to get radii for the new target body. */

/*        Re-initialize the counter used to detect changes to the target */
/*        body radii. */

	zzctruin_(svctr5);
	prvtcd = trgcde;
    }
    zzbodvcd_(&trgcde, "RADII", &c__3, svctr5, &svnrad, svradi, (ftnlen)5);
    if (failed_()) {
	chkout_("TANGPT", (ftnlen)6);
	return 0;
    }
    if (svnrad != 3) {
	setmsg_("Number of radii associated with target body # is #; number "
		"must be 3.", (ftnlen)69);
	errch_("#", target, (ftnlen)1, target_len);
	errint_("#", &svnrad, (ftnlen)1);
	sigerr_("SPICE(INVALIDCOUNT)", (ftnlen)19);
	chkout_("TANGPT", (ftnlen)6);
	return 0;
    }

/*     At this point, we've performed all first-pass actions. */

    first = FALSE_;

/*     Determine the attributes of the frame designated by FIXREF. */

    zznamfrm_(svctr3, svfref, &svfxfc, fixref, &fxfcde, (ftnlen)32, 
	    fixref_len);
    frinfo_(&fxfcde, &fxcent, &fxclss, &fxtyid, &fnd);
    if (failed_()) {
	chkout_("TANGPT", (ftnlen)6);
	return 0;
    }
    if (! fnd) {
	setmsg_("Reference frame # is not recognized by the SPICE frame subs"
		"ystem. Possibly a required frame definition kernel has not b"
		"een loaded.", (ftnlen)130);
	errch_("#", fixref, (ftnlen)1, fixref_len);
	sigerr_("SPICE(NOFRAME)", (ftnlen)14);
	chkout_("TANGPT", (ftnlen)6);
	return 0;
    }

/*     Make sure that FIXREF is centered at the target body's center. */

    if (fxcent != trgcde) {
	setmsg_("Reference frame # is not centered at the target body #. The"
		" ID code of the frame center is #.", (ftnlen)93);
	errch_("#", fixref, (ftnlen)1, fixref_len);
	errch_("#", target, (ftnlen)1, target_len);
	errint_("#", &fxcent, (ftnlen)1);
	sigerr_("SPICE(INVALIDFRAME)", (ftnlen)19);
	chkout_("TANGPT", (ftnlen)6);
	return 0;
    }

/*     Determine the attributes of the frame designated by DREF. */

    zznamfrm_(svctr4, svdref, &svdfrc, dref, &dfrcde, (ftnlen)32, dref_len);
    frinfo_(&dfrcde, &dcentr, &dclass, &dtypid, &fnd);
    if (failed_()) {
	chkout_("TANGPT", (ftnlen)6);
	return 0;
    }
    if (! fnd) {
	setmsg_("Reference frame # is not recognized by the SPICE frame subs"
		"ystem. Possibly a required frame definition kernel has not b"
		"een loaded.", (ftnlen)130);
	errch_("#", dref, (ftnlen)1, dref_len);
	sigerr_("SPICE(NOFRAME)", (ftnlen)14);
	chkout_("TANGPT", (ftnlen)6);
	return 0;
    }

/*     Get the position of the target relative to the observer. If */
/*     light time corrections are used, this gives us an initial */
/*     light time estimate and initial target epoch. */

    spkpos_(target, et, fixref, abcorr, obsrvr, tpos, &lt, target_len, 
	    fixref_len, abcorr_len, obsrvr_len);
    if (failed_()) {
	chkout_("TANGPT", (ftnlen)6);
	return 0;
    }
    if (vzero_(tpos)) {
	setmsg_("Observer # and target # have distinct ID codes but the dist"
		"ance between these objects is zero.", (ftnlen)94);
	errch_("#", obsrvr, (ftnlen)1, obsrvr_len);
	errch_("#", target, (ftnlen)1, target_len);
	sigerr_("SPICE(NOSEPARATION)", (ftnlen)19);
	chkout_("TANGPT", (ftnlen)6);
	return 0;
    }

/*     Negate the target's position to obtain the position of the */
/*     observer relative to the target. */

    vminus_(tpos, fixobs);

/*     Now we can check whether the observer is inside the ellipsoid. */
/*     Find the point on the ellipsoid that lies on the line between */
/*     FIXOBS and the ellipsoid's center. */

    edpnt_(fixobs, svradi, &svradi[1], &svradi[2], p);
    if (failed_()) {
	chkout_("TANGPT", (ftnlen)6);
	return 0;
    }
    if (vnorm_(p) >= vnorm_(fixobs)) {
	setmsg_("Observer # is inside ellipsoid representing target body # s"
		"hape.", (ftnlen)64);
	errch_("#", obsrvr, (ftnlen)1, obsrvr_len);
	errch_("#", target, (ftnlen)1, target_len);
	sigerr_("SPICE(INVALIDGEOMETRY)", (ftnlen)22);
	chkout_("TANGPT", (ftnlen)6);
	return 0;
    }

/*     The target epoch is dependent on the aberration correction. The */
/*     coefficient S has been set to give us the correct answer for each */
/*     case. */

    *trgepc = *et + s * lt;

/*     Transform the direction vector from frame DREF to the body-fixed */
/*     frame associated with the target. The epoch TRGEPC associated */
/*     with the body-fixed frame has been set already. */

/*     We'll compute the transformation in two parts: first */
/*     from frame DREF to J2000, then from J2000 to the target */
/*     frame. */

/*     The orientation of the ray's frame is evaluated at ET in any */
/*     of the following situations: */

/*        - The frame is inertial */
/*        - Light time corrections are not used */
/*        - The frame is centered at the observer */

/*     Let REFEPC be the epoch of participation of the observer. */

    if (dclass == 1 || dcentr == obscde || ! uselt) {
	refepc = *et;
    } else {

/*        The epoch at which the orientation of ray frame is evaluated */
/*        is the epoch of participation of the center of that frame. */

/*        Find the light time from the observer to the center of */
/*        frame DREF. */

	spkezp_(&dcentr, et, "J2000", abcorr, &obscde, ctrpos, &ltcent, (
		ftnlen)5, abcorr_len);
	if (failed_()) {
	    chkout_("TANGPT", (ftnlen)6);
	    return 0;
	}
	refepc = *et + s * ltcent;
    }

/*     The epoch REFEPC associated with frame DREF has been set. */

/*     Compute the ray direction in the J2000 frame. */

    if (dfrcde == 1) {
	vequ_(dvec, j2dir);
    } else {
	refchg_(&dfrcde, &c__1, &refepc, r2jmat);
	if (failed_()) {
	    chkout_("TANGPT", (ftnlen)6);
	    return 0;
	}
	mxv_(r2jmat, dvec, j2dir);
    }

/*     Now transform the ray direction from the J2000 frame to the */
/*     target body-fixed frame at TRGEPC. */

    refchg_(&c__1, &fxfcde, trgepc, j2fixm);
    if (failed_()) {
	chkout_("TANGPT", (ftnlen)6);
	return 0;
    }
    mxv_(j2fixm, j2dir, fixdir);

/*     We have all the inputs needed to make initial estimates of */
/*     our outputs. */

    npedln_(svradi, &svradi[1], &svradi[2], fixobs, fixdir, srfpt, alt);
    if (failed_()) {
	chkout_("TANGPT", (ftnlen)6);
	return 0;
    }

/*     Compute the observer-to-surface point vector for the initial */
/*     estimated solution. */

    vsub_(srfpt, fixobs, srfvec);

/*     Now compute the tangent point. */

/*     Start by finding the nearest point to SRFPT on the line */
/*     containing the input ray. Note that although we're setting the */
/*     value of the outputs TANPT here, we're not done yet. */

/*     We retain the altitude found by NPEDLN, since the following */
/*     call can introduce round-off error in the intercept case. */

    nplnpt_(fixobs, fixdir, srfpt, tanpt, &dval);

/*     Note that TANPT might not be valid here, if we're in the look- */
/*     away case. We'll deal with this case below. */

/*     A SPICE error should not be possible here but we check anyway */
/*     for safety. */

    if (failed_()) {
	chkout_("TANGPT", (ftnlen)6);
	return 0;
    }
    if (! uselt) {

/*        Aberration corrections are not used. */

	*trgepc = *et;

/*        If TANPT is on or behind the ray's vertex, reset TANPT to be */
/*        the vertex, and set the range to zero. Reset the surface point */
/*        SRFPT to the nearest point on the target to the observer, and */
/*        set ALT to the altitude of the observer with respect to that */
/*        point. */

/*        Note that if aberration corrections were used, then the test */
/*        for the ray pointing away from the target could give a */
/*        different result. We handle that case separately later on. */

	vsub_(tanpt, fixobs, tanoff);
	if (vdot_(tanoff, fixdir) <= 0.) {

/*           TANPT is on or behind the ray's vertex. */

	    vequ_(fixobs, tanpt);
	    *range = 0.;
	    nearpt_(fixobs, svradi, &svradi[1], &svradi[2], srfpt, alt);
	    if (failed_()) {
		chkout_("TANGPT", (ftnlen)6);
		return 0;
	    }

/*           Compute SRFVEC using our newly computed value of SRFPT. */

	    vsub_(srfpt, fixobs, srfvec);
	} else {

/*           The tangent point lies ahead of the observer. */

	    if (*alt == 0.) {

/*              This is the geometric intercept case. ALT, SRFPT, TANPT, */
/*              and SRFVEC are already set. To eliminate any error in */
/*              TANPT, we set it equal to SRFPT. */

		vequ_(srfpt, tanpt);
		*range = vnorm_(srfvec);
	    } else {

/*              This is the normal geometric case. All outputs */
/*              other than range are already set. */

		*range = vdist_(fixobs, tanpt);
	    }
	}
	chkout_("TANGPT", (ftnlen)6);
	return 0;
    }

/*     Still here? Then we are using aberration corrections. The outputs */
/*     we've computed serve as first estimates for converged values. */

/*     Since we're using light time corrections, we're going to make an */
/*     estimate of light time to the aberration correction locus, then */
/*     re-do our computation of the target position and orientation */
/*     using the new light time value. */

/*     Note that for non-converged light time, we perform several more */
/*     iterations. The initial light time correction was for the target */
/*     center. */

    if (usecn) {
	nitr = 10;
    } else {
	nitr = 3;
    }

/*     Compute new light time estimate and new target epoch. */

    if (tanloc) {

/*        Compute distance to the tangent point. */

	dist = vdist_(fixobs, tanpt);
    } else {

/*        Compute distance to the surface point. */

	dist = vnorm_(srfvec);
    }

/*     We'll need the state of the observer relative to the solar system */
/*     barycenter. This state need be computed just once. The position */
/*     of the target relative to the solar system barycenter will need */
/*     to be re-computed on each loop iteration. */

    spkssb_(&obscde, et, "J2000", ssbost, (ftnlen)5);
    if (failed_()) {
	chkout_("TANGPT", (ftnlen)6);
	return 0;
    }

/*     Compute light time based on distance to the aberration correction */
/*     locus; compute the new target epoch based on the light time. */

    lt = dist / clight_();
    *trgepc = *et + s * lt;
    prevlt = 0.;
    prvepc = *trgepc;
    i__ = 0;
    ltdiff = 1.;
    epcdif = 1.;

/*     Initialize STLCNV, the flag that indicates whether stellar */
/*     aberration corrections have converged. */

    if (usestl) {
	stlcnv = FALSE_;
    } else {
	stlcnv = TRUE_;
    }

/*     Initialize LTCNV, the flag that indicates whether light time */
/*     corrections have converged. */

    ltcnv = FALSE_;

/*     The loop terminates if both light time and stellar aberration */
/*     correction have converged or if the maximum number of iterations */
/*     has been reached. */

/*     Light time correction convergence is indicated when either of */
/*     these conditions are met: */

/*         - The relative difference between successive light time */
/*           estimates becomes less than CNVLIM */

/*         - The target epoch doesn't change */

/*     Stellar aberration convergence is indicated when both of */
/*     these conditions are met: */

/*        - The relative difference between successive values of TANPT */
/*          becomes less than CNVLIM */

/*        - The relative difference between successive values of SRFPT */
/*          becomes less than CNVLIM */


    while(i__ < nitr && ! (ltcnv && stlcnv)) {
	if (usestl) {

/*           Track the output points in order to test convergence of */
/*           the stellar aberration correction. */

	    vequ_(tanpt, prvtan);
	    vequ_(srfpt, prvsrf);
	}

/*        Get the J2000-relative state of the target relative to */
/*        the solar system barycenter at the target epoch. The */
/*        observer's position doesn't change. */

	spkssb_(&trgcde, trgepc, "J2000", ssbtst, (ftnlen)5);
	if (failed_()) {
	    chkout_("TANGPT", (ftnlen)6);
	    return 0;
	}

/*        Convert the position of the observer relative to the solar */
/*        system barycenter from the J2000 frame to the target frame at */
/*        TRGEPC. */

/*        SSBOST contains the J2000-relative state of the observer */
/*        relative to the solar system barycenter at ET. */

	vsub_(ssbost, ssbtst, j2opos);
	pxform_("J2000", fixref, trgepc, j2fixm, (ftnlen)5, fixref_len);
	if (failed_()) {
	    chkout_("TANGPT", (ftnlen)6);
	    return 0;
	}

/*        If we're using stellar aberration corrections, which we */
/*        normally should do if we're using light time corrections, */
/*        compute the stellar aberration correction for the light time */
/*        corrected aberration correction locus. */

	if (usestl) {

/*           Get the position of the aberration correction locus */
/*           relative to the observer in the J2000 frame. The locus is */
/*           expressed as an offset from the target center. */

/*           First convert the locus from the target body-fixed frame to */
/*           the J2000 frame. */

	    if (tanloc) {
		mtxv_(j2fixm, tanpt, j2lcus);
	    } else {
		mtxv_(j2fixm, srfpt, j2lcus);
	    }

/*           Compute the position of the locus relative to the observer */
/*           in the J2000 frame. */

	    vminus_(j2opos, j2tpos);
	    vadd_(j2tpos, j2lcus, j2lpos);

/*           Correct the vector from the observer to the aberration */
/*           correction locus for stellar aberration and retain the */
/*           offset STLOFF from the uncorrected vector to the corrected */
/*           vector. */

	    if (xmit) {
		stlabx_(j2lpos, &ssbost[3], stlloc);
	    } else {
		stelab_(j2lpos, &ssbost[3], stlloc);
	    }
	    vsub_(stlloc, j2lpos, stloff);

/*           Convert the stellar aberration correction offset to the */
/*           target body-fixed frame at TRGEPC. */

	    mxv_(j2fixm, stloff, stlfix);
	} else {

/*           We're not using stellar aberration correction, so just */
/*           zero out the offset. */

	    cleard_(&c__3, stlfix);
	}

/*        Convert the observer's position relative to the target from */
/*        the J2000 frame to the target frame at the target epoch. Let */
/*        TRGPOS be the negative of this vector. */

	mxv_(j2fixm, j2opos, fixobs);
	vminus_(fixobs, trgpos);

/*        Convert the ray direction vector from the J2000 frame */
/*        to the target frame at the target epoch. */

	mxv_(j2fixm, j2dir, fixdir);

/*        The ray-ellipsoid near point computation must be performed */
/*        using the apparent target. We've accounted for light time, */
/*        but stellar aberration must be accounted for as well. The */
/*        apparent target is shifted due to stellar aberration by the */
/*        body-fixed vector STLFIX. Equivalently, we can shift the */
/*        observer position by -STLFIX. */

/*        If stellar aberration correction was not commanded, then */
/*        STLFIX is the zero vector. */

	vsub_(fixobs, stlfix, stlobs);

/*        Re-compute the surface point and ray altitude. */

	npedln_(svradi, &svradi[1], &svradi[2], stlobs, fixdir, srfpt, alt);
	if (failed_()) {
	    chkout_("TANGPT", (ftnlen)6);
	    return 0;
	}

/*        Now compute the tangent point. */

/*        Start by finding the nearest point to SRFPT on the line */
/*        containing the input ray. */

/*        We retain the altitude found by NPEDLN, since the following */
/*        call can introduce round-off error in the intercept case. */

	if (*alt != 0.) {
	    nplnpt_(stlobs, fixdir, srfpt, tanpt, &dval);
	} else {
	    vequ_(srfpt, tanpt);
	}

/*        The output SRFVEC extends from the observer to the apparent */
/*        position of the surface point. */

	vsub_(srfpt, stlobs, srfvec);

/*        We may need to update TANPT, SRFPT and SRFVEC if the tangent */
/*        point is behind the observer (the look-away case). */

	vsub_(tanpt, stlobs, tanoff);
	if (vdot_(tanoff, fixdir) <= 0.) {

/*           TANPT is on or behind the ray's vertex. Reset TANPT to be */
/*           the vertex. */

	    vequ_(stlobs, tanpt);
	    *range = 0.;

/*           In this case, the surface point is considered to be the */
/*           nearest point on the target to the observer. The altitude */
/*           of the observer above this point is the tangent point's */
/*           altitude. */

	    nearpt_(stlobs, svradi, &svradi[1], &svradi[2], srfpt, alt);
	    if (failed_()) {
		chkout_("TANGPT", (ftnlen)6);
		return 0;
	    }
	    vsub_(srfpt, tanpt, srfvec);

/*           Set the light time and the target epoch, based on the */
/*           locus. */

	    if (tanloc) {
		lt = 0.;
		*trgepc = *et;
	    } else {
		lt = *alt / clight_();
		*trgepc = *et + s * lt;
	    }
	} else {

/*           This is the normal case. */

/*           Compute a new light time estimate and new target epoch. */
/*           Light time estimates are computed using the light-time */
/*           corrected position of the aberration correction locus; */
/*           stellar aberration does not apply. Therefore we use FIXOBS */
/*           as the observer position for the distance computations. */

	    if (tanloc) {

/*              Compute distance to the tangent point. */

		dist = vdist_(fixobs, tanpt);
	    } else {

/*              Compute distance to the surface point. */

		dist = vdist_(fixobs, srfpt);
	    }

/*           Compute a new light time estimate and a new target epoch. */

	    lt = dist / clight_();
	    *trgepc = *et + s * lt;
	}

/*        Compute the changes in the light time and target epoch for */
/*        this loop pass. Determine whether light time and stellar */
/*        aberration have converged. */

	lt = touchd_(&lt);
	ltdiff = (d__1 = lt - prevlt, abs(d__1));
	epcdif = (d__1 = *trgepc - prvepc, abs(d__1));
	prevlt = lt;
	prvepc = *trgepc;
	++i__;
	ltcnv = ltdiff < abs(lt) * 1e-16 || epcdif == 0.;
	if (usestl) {
	    stlcnv = vrel_(tanpt, prvtan) < 1e-16 && vrel_(srfpt, prvsrf) < 
		    1e-16;
	}
    }
    *range = vdist_(stlobs, tanpt);
    chkout_("TANGPT", (ftnlen)6);
    return 0;
} /* tangpt_ */

