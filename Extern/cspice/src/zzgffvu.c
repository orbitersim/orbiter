/* zzgffvu.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__30000 = 30000;
static integer c__3 = 3;
static integer c__20000 = 20000;
static integer c__9 = 9;
static integer c__4 = 4;
static integer c__0 = 0;
static integer c__10000 = 10000;
static doublereal c_b86 = 1.;
static doublereal c_b116 = 2.;

/* $Procedure ZZGFFVU ( GF, instrument FOV utilities ) */
/* Subroutine */ int zzgffvu_0_(int n__, char *inst, char *tshape, doublereal 
	*raydir, char *target, char *tframe, char *abcorr, char *obsrvr, 
	doublereal *time, logical *vistat, ftnlen inst_len, ftnlen tshape_len,
	 ftnlen target_len, ftnlen tframe_len, ftnlen abcorr_len, ftnlen 
	obsrvr_len)
{
    /* Initialized data */

    static doublereal svorig[3] = { 0.,0.,0. };

    /* System generated locals */
    integer i__1, i__2, i__3;
    doublereal d__1, d__2;

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_cmp(char *, char *, ftnlen, ftnlen), s_rnge(char *, integer, 
	    char *, integer);
    double pow_dd(doublereal *, doublereal *), sqrt(doublereal);

    /* Local variables */
    extern /* Subroutine */ int vadd_(doublereal *, doublereal *, doublereal *
	    );
    doublereal limb[9];
    extern integer zzwind2d_(integer *, doublereal *, doublereal *);
    extern /* Subroutine */ int vhat_(doublereal *, doublereal *), vscl_(
	    doublereal *, doublereal *, doublereal *);
    extern doublereal vdot_(doublereal *, doublereal *), vsep_(doublereal *, 
	    doublereal *);
    extern /* Subroutine */ int vsub_(doublereal *, doublereal *, doublereal *
	    ), vequ_(doublereal *, doublereal *), mtxv_(doublereal *, 
	    doublereal *, doublereal *), zzgftreb_(integer *, doublereal *), 
	    zzcorepc_(char *, doublereal *, doublereal *, doublereal *, 
	    ftnlen);
    doublereal pnt2d[3];
    extern /* Subroutine */ int zzvalcor_(char *, logical *, ftnlen), 
	    zzfovaxi_(char *, integer *, doublereal *, doublereal *, ftnlen);
    integer i__;
    extern /* Subroutine */ int zzprscor_(char *, logical *, ftnlen);
    doublereal l;
    integer w;
    doublereal x[3], y[3], z__[3];
    extern /* Subroutine */ int frame_(doublereal *, doublereal *, doublereal 
	    *), chkin_(char *, ftnlen), zzelvupy_(doublereal *, doublereal *, 
	    doublereal *, integer *, doublereal *, logical *), ucase_(char *, 
	    char *, ftnlen, ftnlen), errch_(char *, char *, ftnlen, ftnlen);
    doublereal bsite[3], coord[2];
    logical found;
    extern /* Subroutine */ int errdp_(char *, doublereal *, ftnlen);
    doublereal stobs[6], vtemp[3], fovpt[3], m1[9]	/* was [3][3] */;
    extern doublereal vnorm_(doublereal *);
    doublereal m2[9]	/* was [3][3] */;
    static integer svobs;
    extern /* Subroutine */ int ljust_(char *, char *, ftnlen, ftnlen);
    extern logical vzero_(doublereal *);
    extern /* Subroutine */ int ucrss_(doublereal *, doublereal *, doublereal 
	    *);
    integer nxpts;
    extern /* Subroutine */ int bods2c_(char *, integer *, logical *, ftnlen),
	     vrotv_(doublereal *, doublereal *, doublereal *, doublereal *), 
	    el2cgv_(doublereal *, doublereal *, doublereal *, doublereal *), 
	    cgv2el_(doublereal *, doublereal *, doublereal *, doublereal *), 
	    nvc2pl_(doublereal *, doublereal *, doublereal *);
    doublereal vtemp2[3];
    extern logical failed_(void);
    extern /* Subroutine */ int cleard_(integer *, doublereal *);
    doublereal escale;
    extern /* Subroutine */ int edlimb_(doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *);
    doublereal lt;
    integer framid;
    extern doublereal halfpi_(void);
    doublereal ellrad[3];
    extern /* Subroutine */ int stelab_(doublereal *, doublereal *, 
	    doublereal *);
    doublereal fvlimb[9];
    extern logical return_(void);
    static char svifrm[32], svinam[36], svishp[9], svtfrm[32], svtnam[36], 
	    svtshp[9], svcorr[5];
    doublereal ctrext, ettarg, insmat[9]	/* was [3][3] */, obspos[3], 
	    semipt[6]	/* was [3][2] */;
    static doublereal svarad, svbnds[30000]	/* was [3][10000] */, svedct[
	    3], svfaxi[3], svfovm[9]	/* was [3][3] */, svfpol[20000]	/* 
	    was [2][10000] */, svfsmx[9]	/* was [3][3] */, svfvct[3], 
	    svplan[4], svrdir[3], svsemi[6]	/* was [3][2] */, svtrad[3], 
	    svxmag[2];
    doublereal trgctr[3], trgsmx[9]	/* was [3][3] */;
    integer clssid, frcent, frclss, ocstat;
    static integer svinst, svnvrt, svtarg;
    logical attblk[15];
    static logical svetrg, svuray, svustl, svxmit;
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), setmsg_(char *, ftnlen), namfrm_(char *, integer *, 
	    ftnlen);
    doublereal dir[3];
    extern /* Subroutine */ int frinfo_(integer *, integer *, integer *, 
	    integer *, logical *), errint_(char *, integer *, ftnlen), 
	    cmprss_(char *, integer *, char *, char *, ftnlen, ftnlen, ftnlen)
	    , getfov_(integer *, integer *, char *, char *, doublereal *, 
	    integer *, doublereal *, ftnlen, ftnlen), vhatip_(doublereal *), 
	    inrypl_(doublereal *, doublereal *, doublereal *, integer *, 
	    doublereal *);
    extern doublereal dpr_(void);
    doublereal sep;
    extern /* Subroutine */ int vsclip_(doublereal *, doublereal *), spkezp_(
	    integer *, doublereal *, char *, char *, integer *, doublereal *, 
	    doublereal *, ftnlen, ftnlen), pxform_(char *, char *, doublereal 
	    *, doublereal *, ftnlen, ftnlen), vminus_(doublereal *, 
	    doublereal *), spkssb_(integer *, doublereal *, char *, 
	    doublereal *, ftnlen), stlabx_(doublereal *, doublereal *, 
	    doublereal *);
    doublereal pos[3];
    extern /* Subroutine */ int mxm_(doublereal *, doublereal *, doublereal *)
	    , mxv_(doublereal *, doublereal *, doublereal *);
    doublereal xpt[3];
    extern integer zzocced_(doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *);

/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines.  Users should not call this routine directly due */
/*     to the volatile nature of this routine. */

/*     This routine contains the entry points that produce the */
/*     computations needed for solving for target visibility states */
/*     in the geometry finding routines. */

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
/*     GF */
/*     NAIF_IDS */
/*     PCK */
/*     SCLK */
/*     SPK */
/*     TIME */

/* $ Keywords */

/*     EVENT */
/*     FOV */
/*     GEOMETRY */
/*     INSTRUMENT */
/*     SEARCH */

/* $ Declarations */
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

/*     Declare ZZOCCED return code parameters, comparison strings */
/*     and other parameters. */

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

/*     GF */

/* $ Keywords */

/*     ELLIPSOID */
/*     GEOMETRY */
/*     GF */
/*     OCCULTATION */

/* $ Restrictions */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman      (JPL) */

/* $ Literature_References */

/*     None. */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 01-SEP-2005 (NJB) */

/* -& */
/*     The function returns an integer code indicating the geometric */
/*     relationship of the three bodies. */

/*     Codes and meanings are: */

/*        -3                    Total occultation of first target by */
/*                              second. */


/*        -2                    Annular occultation of first target by */
/*                              second.  The second target does not */
/*                              block the limb of the first. */


/*        -1                    Partial occultation of first target by */
/*                              second target. */


/*         0                    No occultation or transit:  both objects */
/*                              are completely visible to the observer. */


/*         1                    Partial occultation of second target by */
/*                              first target. */


/*         2                    Annular occultation of second target by */
/*                              first. */


/*         3                    Total occultation of second target by */
/*                              first. */


/*     End include file zzocced.inc */

/* $ Abstract */

/*     This file contains public, global parameter declarations */
/*     for the SPICELIB Geometry Finder (GF) subsystem. */

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

/*     GF */

/* $ Keywords */

/*     GEOMETRY */
/*     ROOT */

/* $ Restrictions */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman      (JPL) */
/*     L.E. Elson        (JPL) */
/*     E.D. Wright       (JPL) */

/* $ Literature_References */

/*     None. */

/* $ Version */

/* -    SPICELIB Version 2.0.0  29-NOV-2016 (NJB) */

/*        Upgraded to support surfaces represented by DSKs. */

/*        Bug fix: removed declaration of NVRMAX parameter. */

/* -    SPICELIB Version 1.3.0, 01-OCT-2011 (NJB) */

/*       Added NWILUM parameter. */

/* -    SPICELIB Version 1.2.0, 14-SEP-2010 (EDW) */

/*       Added NWPA parameter. */

/* -    SPICELIB Version 1.1.0, 08-SEP-2009 (EDW) */

/*       Added NWRR parameter. */
/*       Added NWUDS parameter. */

/* -    SPICELIB Version 1.0.0, 21-FEB-2009 (NJB) (LSE) (EDW) */

/* -& */

/*     Root finding parameters: */

/*     CNVTOL is the default convergence tolerance used by the */
/*     high-level GF search API routines. This tolerance is */
/*     used to terminate searches for binary state transitions: */
/*     when the time at which a transition occurs is bracketed */
/*     by two times that differ by no more than CNVTOL, the */
/*     transition time is considered to have been found. */

/*     Units are TDB seconds. */


/*     NWMAX is the maximum number of windows allowed for user-defined */
/*     workspace array. */

/*        DOUBLE PRECISION      WORK   ( LBCELL : MW, NWMAX ) */

/*     Currently no more than twelve windows are required; the three */
/*     extra windows are spares. */

/*     Callers of GFEVNT can include this file and use the parameter */
/*     NWMAX to declare the second dimension of the workspace array */
/*     if necessary. */


/*     Callers of GFIDST should declare their workspace window */
/*     count using NWDIST. */


/*     Callers of GFSEP should declare their workspace window */
/*     count using NWSEP. */


/*     Callers of GFRR should declare their workspace window */
/*     count using NWRR. */


/*     Callers of GFUDS should declare their workspace window */
/*     count using NWUDS. */


/*     Callers of GFPA should declare their workspace window */
/*     count using NWPA. */


/*     Callers of GFILUM should declare their workspace window */
/*     count using NWILUM. */


/*     ADDWIN is a parameter used to expand each interval of the search */
/*     (confinement) window by a small amount at both ends in order to */
/*     accommodate searches using equality constraints. The loaded */
/*     kernel files must accommodate these expanded time intervals. */


/*     FRMNLN is a string length for frame names. */


/*     FOVTLN -- maximum length for FOV string. */


/*     Specify the character strings that are allowed in the */
/*     specification of field of view shapes. */


/*     Character strings that are allowed in the */
/*     specification of occultation types: */


/*     Occultation target shape specifications: */


/*     Specify the number of supported occultation types and occultation */
/*     type string length: */


/*     Instrument field-of-view (FOV) parameters */

/*     Maximum number of FOV boundary vectors: */


/*     FOV shape parameters: */

/*        circle */
/*        ellipse */
/*        polygon */
/*        rectangle */


/*     End of file gf.inc. */

/* $ Brief_I/O */

/*     VARIABLE  I/O  Entry points */
/*     --------  ---  -------------------------------------------------- */
/*     UBEL       P   All */
/*     UBPL       P   All */
/*     INST       I   ZZGFFVIN */
/*     TSHAPE     I   ZZGFFVIN */
/*     RAYDIR     I   ZZGFFVIN */
/*     TARGET     I   ZZGFFVIN */
/*     TFRAME     I   ZZGFFVIN */
/*     ABCORR     I   ZZGFFVIN */
/*     OBSRVR     I   ZZGFFVIN */
/*     TIME       I   ZZGFFVST */
/*     OCSTAT     O   ZZGFFVST */

/* $ Detailed_Input */

/*     See entry points. */

/* $ Detailed_Output */

/*     See entry points. */

/* $ Parameters */

/*     See INCLUDE files */

/*        gf.inc */
/*        zzgf.inc */

/* $ Exceptions */

/*     See entry points. */

/* $ Files */

/*     Appropriate SPK and instrument kernels must be loaded by the */
/*     calling program before this routine is called.  PCK, CK and SCLK */
/*     kernels may be required as well. */

/*     The following data are required: */

/*        - SPK data:  ephemeris data for target and observer that */
/*          describes the ephemeris of these objects for the period */
/*          defined by the confinement window, 'CNFINE' must be */
/*          loaded.  If aberration corrections are used, the states of */
/*          target and observer relative to the solar system barycenter */
/*          must be calculable from the available ephemeris data. */
/*          Typically ephemeris data are made available by loading one */
/*          or more SPK files via FURNSH. */

/*        - Frame data:  if a frame definition is required to convert */
/*          the observer and target states to the body-fixed frame of */
/*          the target, that definition must be available in the kernel */
/*          pool. Typically the definitions of frames not already */
/*          built-in to SPICE are supplied by loading a frame kernel. */

/*          Data defining the reference frame associated with the */
/*          instrument designated by INST must be available in the kernel */
/*          pool. Additionally the name INST must be associated with an */
/*          ID code. Normally these data are  made available by loading */
/*          a frame kernel via FURNSH. */

/*        - IK data: the kernel pool must contain data such that */
/*          the SPICELIB routine GETFOV may be called to obtain */
/*          parameters for INST. Normally such data are provided by */
/*          an IK via FURNSH. */

/*     The following data may be required: */

/*        - PCK data: bodies modeled as triaxial ellipsoids must have */
/*          orientation data provided by variables in the kernel pool. */
/*          Typically these data are made available by loading a text */
/*          PCK file via FURNSH. */

/*          Bodies modeled as triaxial ellipsoids must have semi-axis */
/*          lengths provided by variables in the kernel pool. Typically */
/*          these data are made available by loading a text PCK file via */
/*          FURNSH. */

/*        - CK data: if the instrument frame is fixed to a spacecraft, */
/*          at least one CK file will be needed to permit transformation */
/*          of vectors between that frame and both J2000 and the target */
/*          body-fixed frame. */

/*        - SCLK data:  if a CK file is needed, an associated SCLK */
/*          kernel is required to enable conversion between encoded SCLK */
/*          (used to time-tag CK data) and barycentric dynamical time */
/*          (TDB). */

/*     Kernel data are normally loaded once per program run, NOT every */
/*     time this routine is called. */

/* $ Particulars */

/*     This routine is designed to determine whether a specified */
/*     target intersects the space bounded by the FOV of a specified */
/*     instrument at a specified epoch. The target may be represented */
/*     by a ray, or the target may be an ephemeris object. */

/*     This routine contains two entry points that support searches */
/*     for target visibility periods performed using ZZGFSOLV: */

/*        ZZGFFVIN   Saves the user-supplied inputs defining the */
/*                   visibility computation to be performed. */
/*                   Initializes the visibility search. */

/*        ZZGFFVST   Returns the visibility state for a specified */
/*                   time. */

/* $ Examples */

/*     See GFFOVE. */

/* $ Restrictions */

/*     This is a SPICELIB private routine; it should not be called by */
/*     user applications. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman   (JPL) */
/*     L.S. Elson     (JPL) */
/*     W.L. Taber     (JPL) */
/*     E.D. Wright    (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.2.0, 24-NOV-2021 (NJB) (EDW) */

/*        Body radii accessed from kernel pool using ZZGFTREB. */

/*        Bug fix: boresight is now scaled to unit length. */

/*        Bug fix: in the case of a circular or elliptical FOV, where */
/*        the target shape is an ellipsoid, this routine creates an */
/*        ellipsoid such that its limb coincides with the FOV. */
/*        Previously this ellipsoid was allowed to be very large when */
/*        the FOV itself was large. Now the sum of the maximum ellipsoid */
/*        radius plus the distance between the ellipsoid center and the */
/*        observer is limited to FVEMAX km. See entry point ZZGFFVIN for */
/*        details. These changes were made on 09-JUN-2017. Minor updates */
/*        were made on the date given by the version line. */

/* -    SPICELIB Version 1.1.0, 04-JUN-2013 (EDW) */

/*        Edit to ZZGFFVIN: */

/*        ABCORR now stripped of all spaces before saving. */
/*        Specifically, the call */

/*        CALL LJUST ( ABCORR, SVCORR ) */

/*        replaced with */

/*        CALL CMPRSS ( ' ', 0, ABCORR, SVCORR ) */

/* -    SPICELIB Version 1.0.0, 05-MAR-2009 (NJB) (LSE) (WLT) (EDW) */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     ATOL is a tolerance value for computing FOV angular radius. */
/*     The angular radius must not exceed pi/2 - ATOL radians. */


/*     Local variables */


/*     Saved variables */


/*     Initial values */

    /* Parameter adjustments */
    if (raydir) {
	}

    /* Function Body */
    switch(n__) {
	case 1: goto L_zzgffvin;
	case 2: goto L_zzgffvst;
	}


/*     Below we initialize the list of visibility types. */


/*     This routine should never be called directly. */

    chkin_("ZZGFFVU", (ftnlen)7);
    sigerr_("SPICE(BOGUSENTRY)", (ftnlen)17);
    chkout_("ZZGFFVU", (ftnlen)7);
    return 0;
/* $Procedure  ZZGFFVIN ( GF, visibility initialization ) */

L_zzgffvin:
/* $ Abstract */

/*    Perform initialization functions for visibility state */
/*    determination. */

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

/*     NAIF_IDS */
/*     FRAMES */
/*     PCK */
/*     SPK */
/*     TIME */

/* $ Keywords */

/*     SEARCH */
/*     GEOMETRY */
/*     VISIBILITY */

/* $ Declarations */

/*     CHARACTER*(*)         INST */
/*     CHARACTER*(*)         TSHAPE */
/*     DOUBLE PRECISION      RAYDIR ( 3 ) */
/*     CHARACTER*(*)         TARGET */
/*     CHARACTER*(*)         TFRAME */
/*     CHARACTER*(*)         ABCORR */
/*     CHARACTER*(*)         OBSRVR */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     INST       I   Name of the instrument. */
/*     TSHAPE     I   Type of shape model used for target body. */
/*     RAYDIR     I   Ray's direction vector. */
/*     TARGET     I   Name of the target body. */
/*     TFRAME     I   Body-fixed, body-centered frame for target body. */
/*     ABCORR     I   Aberration correction flag. */
/*     OBSRVR     I   Name of the observing body. */

/* $ Detailed_Input */


/*     INST       indicates the name of the instrument, such as a */
/*                spacecraft-mounted framing camera, the field of view */
/*                (FOV) of which is to be used for a target intersection */
/*                search: times when the specified target intersects the */
/*                region of space corresponding to the FOV are sought. */

/*                The position of the instrument designated by INST is */
/*                considered to coincide with that of the ephemeris */
/*                object designated by the input argument OBSRVR (see */
/*                description below). */

/*                INST must have a corresponding NAIF ID and a frame */
/*                defined, as is normally done in a frame kernel. It */
/*                must also have an associated reference frame and a FOV */
/*                shape, boresight and boundary vertices (or reference */
/*                vector and reference angles) defined, as is usually */
/*                done in an instrument kernel. */

/*                See the header of the SPICELIB routine GETFOV for a */
/*                description of the required parameters associated with */
/*                an instrument. */


/*     TSHAPE     is a string indicating the geometric model used to */
/*                represent the location and shape of the target body. */
/*                The target body may be represented by either an */
/*                ephemeris object or a ray emanating from the observer. */

/*                The supported values of TSHAPE are: */

/*                   'ELLIPSOID'     The target is an ephemeris object. */

/*                                   The target's shape is represented */
/*                                   using triaxial ellipsoid model, */
/*                                   with radius values provided via the */
/*                                   kernel pool. A kernel variable */
/*                                   having a name of the form */

/*                                      'BODYnnn_RADII' */

/*                                   where nnn represents the NAIF */
/*                                   integer code associated with the */
/*                                   body, must be present in the kernel */
/*                                   pool. This variable must be */
/*                                   associated with three numeric */
/*                                   values giving the lengths of the */
/*                                   ellipsoid's X, Y, and Z semi-axes. */

/*                   'POINT'         The target is an ephemeris object. */
/*                                   The body is treated as a single */
/*                                   point. */

/*                   'RAY'           The target is NOT an ephemeris */
/*                                   object. Instead, the target is */
/*                                   considered to be represented by the */
/*                                   ray emanating from the observer's */
/*                                   location and having direction */
/*                                   vector RAYDIR. The target is */
/*                                   considered to be visible if and */
/*                                   only if the ray is contained within */
/*                                   the space bounded by the instrument */
/*                                   FOV. */

/*                Case and leading or trailing blanks are not */
/*                significant in the string TSHAPE. */


/*     RAYDIR     is the direction vector associated with a ray */
/*                representing the target. RAYDIR is used if and only */
/*                if TSHAPE (see description above) indicates the */
/*                target is modeled as a ray. */


/*     TARGET     is the name of the target body, the appearances of */
/*                which in the specified instrument's field of view are */
/*                sought. The body must be an ephemeris object. */

/*                Optionally, you may supply the integer NAIF ID code */
/*                for the body as a string. For example both 'MOON' and */
/*                '301' are legitimate strings that designate the Moon. */

/*                Case and leading or trailing blanks are not */
/*                significant in the string TARGET. */

/*                The input argument TARGET is used if and only if the */
/*                target is NOT modeled as ray; equivalently, the input */
/*                argument TSHAPE (see description above) does not */
/*                contain a string equivalent---that is, ignoring case */
/*                and leading and trailing blanks---to 'RAY'. */

/*                TARGET may be set to a blank string if the target is */
/*                modeled as a ray. */


/*     TFRAME     is the name of the reference frame associated with the */
/*                target. Examples of such names are 'IAU_SATURN' */
/*                (for Saturn) and 'ITRF93' (for the Earth). */

/*                If the target is an ephemeris object modeled as an */
/*                ellipsoid, TFRAME must designate a body-fixed */
/*                reference frame centered on the target body. */

/*                If the target is an ephemeris object modeled as a */
/*                point, TFRAME is ignored; TFRAME should be left blank. */

/*                If the target is modeled as a ray, TFRAME may */
/*                designate any reference frame. Since light time */
/*                corrections are not supported for rays, the */
/*                orientation of the frame is always evaluated at the */
/*                epoch associated with the observer, as opposed to the */
/*                epoch associated with the light-time corrected */
/*                position of the frame center. */

/*                Case and leading or trailing blanks bracketing a */
/*                non-blank frame name are not significant in the string */
/*                TFRAME. */


/*     ABCORR     indicates the aberration corrections to be applied */
/*                when computing the target's position and orientation. */
/*                The supported values of ABCORR depend on the target */
/*                representation. */

/*                If the target is represented by a ray, the aberration */
/*                correction options are */

/*                   'NONE'          No correction. */
/*                   'S'             Stellar aberration correction, */
/*                                   reception case. */
/*                   'XS'            Stellar aberration correction, */
/*                                   transmission case. */

/*                If the target is an ephemeris object, the aberration */
/*                correction options are those supported by the SPICE */
/*                SPK system. For remote sensing applications, where the */
/*                apparent position and orientation of the target seen */
/*                by the observer are desired, normally either of the */
/*                corrections */

/*                   'LT+S' */
/*                   'CN+S' */

/*                should be used. These and the other supported options */
/*                are described below. */

/*                Supported aberration correction options for */
/*                observation (case where radiation is received by */
/*                observer at ET) are: */

/*                   'NONE'       No correction. */
/*                   'LT'         Light time only */
/*                   'LT+S'       Light time and stellar aberration. */
/*                   'CN'         Converged Newtonian (CN) light time. */
/*                   'CN+S'       CN light time and stellar aberration. */

/*                Supported aberration correction options for */
/*                transmission (case where radiation is emitted from */
/*                observer at ET) are: */

/*                   'XLT'        Light time only. */
/*                   'XLT+S'      Light time and stellar aberration. */
/*                   'XCN'        Converged Newtonian (CN) light time. */
/*                   'XCN+S'      CN light time and stellar aberration. */

/*                For detailed information, see the geometry finder */
/*                required reading, gf.req. */

/*                Case, leading and trailing blanks are not significant */
/*                in the string ABCORR. */


/*     OBSRVR     is the name of the body from which the target is */
/*                observed. The instrument designated by INST is treated */
/*                as if it were co-located with the observer. */

/*                Optionally, you may supply the integer NAIF ID code */
/*                for the body as a string. */

/*                Case and leading or trailing blanks are not */
/*                significant in the string OBSRVR. */

/* $ Detailed_Output */

/*     None. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the observer's name cannot be mapped to an ID code, the */
/*         error SPICE(IDCODENOTFOUND) is signaled. */

/*     2)  If the target is an ephemeris object and its name cannot be */
/*         mapped to an ID code, the error SPICE(IDCODENOTFOUND) is */
/*         signaled. If the target is represented by a ray, the input */
/*         target name argument is ignored. */

/*     3)  If target is an ephemeris object, and the observer and */
/*         target have the same ID codes, the error */
/*         SPICE(BODIESNOTDISTINCT) is signaled. */

/*     4)  If target is an ephemeris object, and the target shape */
/*         is not equivalent to PTSHAP (point) or EDSHAP (ellipsoid), */
/*         the error SPICE(INVALIDSHAPE) is signaled. */

/*     5)  If target is an ephemeris object, the target shape is */
/*         equivalent to EDSHAP (ellipsoid), and the reference frame */
/*         argument TFRAME is blank, the error SPICE(INVALIDFRAME) is */
/*         signaled. */

/*     6)  If target is an ephemeris object, the target shape is */
/*         equivalent to EDSHAP (ellipsoid), and the reference frame */
/*         argument TFRAME cannot be mapped to a frame ID code, the */
/*         error SPICE(INVALIDFRAME) is signaled. */

/*     7)  If target is an ephemeris object, the target shape is */
/*         equivalent to EDSHAP (ellipsoid), and the reference frame */
/*         argument TFRAME's ID cannot be mapped to a frame description, */
/*         the error SPICE(FRAMEINFONOTFOUND) is signaled. */

/*     8)  If target is an ephemeris object, the target shape is */
/*         equivalent to EDSHAP (ellipsoid), and the reference frame */
/*         specified by TFRAME is not centered on the target body, the */
/*         error SPICE(INVALIDFRAME) is signaled. */

/*     9)  If the target is represented by a ray and the aberration */
/*         correction flag calls for light time correction, the error */
/*         SPICE(INVALIDOPTION) is signaled. */

/*     10) If target is an ephemeris object and the aberration */
/*         correction flag calls for a correction not supported by */
/*         the SPICE SPK system, the error is diagnosed by a routine */
/*         in the call tree of this routine. */

/*     11) If target is an ephemeris object, the target shape is */
/*         equivalent to EDSHAP (ellipsoid), and the kernel pool */
/*         does not contain radii for the target body, */
/*         not  target body, the error is diagnosed by a routine */
/*         in the call tree of this routine. */

/*     12) If target is an ephemeris object, the target shape is */
/*         equivalent to EDSHAP (ellipsoid), and the kernel pool */
/*         contains the wrong number of radii for the target body, the */
/*         error SPICE(INVALIDDIMENSION) is signaled. */

/*     13) If target is an ephemeris object, the target shape is */
/*         equivalent to EDSHAP (ellipsoid), and the kernel pool */
/*         contains one or more non-positive radii for the target body, */
/*         the error SPICE(BADAXISLENGTH) is signaled. */

/*     14) If the target is represented by a ray and the ray's */
/*         direction vector is zero, the error SPICE(ZEROVECTOR) is */
/*         signaled. */

/*     15) If the instrument name INST cannot be mapped to an ID code, */
/*         the error SPICE(IDCODENOTFOUND) is signaled. */

/*     16) If an error occurs while fetching the instrument parameters */
/*         from the kernel pool, the error will be diagnosed by a */
/*         routine in the call tree of this routine. */

/*     17) If any ray defined by the observer's position and one of */
/*         the instrument FOV's boundary vectors fails to intersect */
/*         the "FOV plane"---a plane normal to the instrument FOV axis */
/*         and intersected by the FOV axis at distance 1 km from the */
/*         observer---the error SPICE(DEGENERATECASE) is signaled. */

/*     18) If the FOV is circular or elliptical and the FOV's radius */
/*         or one of the FOV's semi-axis lengths is zero, the error */
/*         SPICE(DEGENERATECASE) is signaled. */

/*     19) If the maximum angular separation of the instrument */
/*         FOV axis and any FOV boundary vector exceeds the limit */
/*         (which is slightly less than 90 degrees), either the error */
/*         SPICE(FOVTOOWIDE) will be signaled or the error will be */
/*         diagnosed by a routine in the call tree of this routine. */


/* $ Files */

/*     See the header of the umbrella routine ZZGFFVU. */

/* $ Particulars */

/*     This entry point initializes the parameters needed by the */
/*     occultation state determination entry point ZZGFFVST. */

/* $ Examples */

/*     See implementation of GFFOVE. */

/* $ Restrictions */

/*     1) The reference frame associated with INST must be */
/*        centered at the observer or must be inertial. No check is done */
/*        to ensure this. */

/*     2) This is a SPICELIB private routine; it should not be called by */
/*        user applications. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman   (JPL) */
/*     L.S. Elson     (JPL) */
/*     W.L. Taber     (JPL) */
/*     E.D. Wright    (JPL) */

/* $ Version */

/* -    SPICELIB version 1.3.0, 25-AUG-2021 (EDW) */

/*        Body radii accessed from kernel pool using ZZGFTREB. */

/* -    SPICELIB Version 1.2.0, 01-NOV-2020 (NJB) */

/*        Bug fix: boresight is now scaled to unit length. */

/*        Bug fix: in the case of a circular or elliptical FOV, where */
/*        the target shape is an ellipsoid, this routine creates an */
/*        ellipsoid such that its limb coincides with the FOV. */
/*        Previously this ellipsoid was allowed to be very large when */
/*        the FOV itself was large. Now the sum of the maximum ellipsoid */
/*        radius plus the distance between the ellipsoid center and the */
/*        observer is limited to FVEMAX km. These changes were made on */
/*        09-JUN-2017. Minor updates were made on the date given by the */
/*        version line. */

/* -    SPICELIB Version 1.1.0, 04-JUN-2013 (EDW) */

/*        ABCORR now stripped of all spaces before saving. */
/*        Specifically, the call */

/*        CALL LJUST ( ABCORR, SVCORR ) */

/*        replaced with */

/*        CALL CMPRSS ( ' ', 0, ABCORR, SVCORR ) */

/* -    SPICELIB Version 1.0.0, 05-MAR-2009 (NJB) (LSE) (WLT) (EDW) */

/* -& */
/* $ Revisions */

/* -    SPICELIB Version 1.2.0, 29-OCT-2020 (NJB) */

/*        Bug fix: in the case of a circular or elliptical FOV, where */
/*        the target shape is an ellipsoid, this routine creates an */
/*        ellipsoid such that its limb coincides with the FOV. Previously */
/*        this ellipsoid was allowed to be very large when the FOV itself */
/*        was large. Now the sum of the maximum ellipsoid radius plus */
/*        the distance between the ellipsoid center and the observer is */
/*        limited to FVEMAX km. */

/*        Renamed local LOGICAL variable SVXTRG (save extended target) */
/*        to SVETRG (save ellipsoidal target), since it does not apply */
/*        to DSK targets. Note that DSK targets currently are not */
/*        supported. */

/* -& */

/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }
    chkin_("ZZGFFVIN", (ftnlen)8);

/*     To avoid portability problems, initialize all */
/*     saved variables that aren't initialized via DATA */
/*     statements and aren't guaranteed to be initialized */
/*     for all cases. */

    cleard_(&c__30000, svbnds);
    cleard_(&c__3, svedct);
    cleard_(&c__3, svfaxi);
    cleard_(&c__20000, svfpol);
    cleard_(&c__9, svfsmx);
    cleard_(&c__4, svplan);
    cleard_(&c__3, svrdir);
    svtarg = 0;
    s_copy(svtfrm, " ", (ftnlen)32, (ftnlen)1);
    s_copy(svtnam, " ", (ftnlen)36, (ftnlen)1);
    cleard_(&c__3, svtrad);
    svustl = FALSE_;
    svxmit = FALSE_;

/*     Find the NAIF ID for OBSRVR. */

    bods2c_(obsrvr, &svobs, &found, obsrvr_len);
    if (! found) {
	setmsg_("The observer, '#', is not a recognized name for an ephemeri"
		"s object. The cause of this problem may be that you need an "
		"updated version of the SPICE Toolkit. ", (ftnlen)157);
	errch_("#", obsrvr, (ftnlen)1, obsrvr_len);
	sigerr_("SPICE(IDCODENOTFOUND)", (ftnlen)21);
	chkout_("ZZGFFVIN", (ftnlen)8);
	return 0;
    }

/*     Process the target shape specifier here. */

/*     Save a left-justified, upper case version of the target shape */
/*     specifier. */

    ljust_(tshape, svtshp, tshape_len, (ftnlen)9);
    ucase_(svtshp, svtshp, (ftnlen)9, (ftnlen)9);

/*     Note for maintenance programmer: these checks will */
/*     require modification to handle DSK-based shapes. */

    if (s_cmp(svtshp, "POINT", (ftnlen)9, (ftnlen)5) != 0 && s_cmp(svtshp, 
	    "ELLIPSOID", (ftnlen)9, (ftnlen)9) != 0 && s_cmp(svtshp, "RAY", (
	    ftnlen)9, (ftnlen)3) != 0) {
	setmsg_("The target shape specification, '#', is not recognized.", (
		ftnlen)55);
	errch_("#", tshape, (ftnlen)1, tshape_len);
	sigerr_("SPICE(INVALIDSHAPE)", (ftnlen)19);
	chkout_("ZZGFFVIN", (ftnlen)8);
	return 0;
    }

/*     We'll use the logical variable USERAY to indicate that the */
/*     target is modeled as ray. */

    svuray = s_cmp(svtshp, "RAY", (ftnlen)9, (ftnlen)3) == 0;

/*     Indicate whether we have an ellipsoidal target. SVETRG is .TRUE. */
/*     if and only we have one. */

    svetrg = s_cmp(svtshp, "ELLIPSOID", (ftnlen)9, (ftnlen)9) == 0;

/*     If the target is an ephemeris object, obtain its ID code. */
/*     Save the target object's name, if applicable. */

    if (! svuray) {
	bods2c_(target, &svtarg, &found, target_len);
	if (! found) {
	    setmsg_("The target object, '#', is not a recognized name for an"
		    " ephemeris object. The cause of this problem may be that"
		    " you need an updated version of the SPICE Toolkit. ", (
		    ftnlen)162);
	    errch_("#", target, (ftnlen)1, target_len);
	    sigerr_("SPICE(IDCODENOTFOUND)", (ftnlen)21);
	    chkout_("ZZGFFVIN", (ftnlen)8);
	    return 0;
	}

/*        Save the target's name. */

	s_copy(svtnam, target, (ftnlen)36, target_len);

/*        Make sure the observer and target are distinct. */

	if (svtarg == svobs) {
	    setmsg_("The observer and target must be distinct objects, but a"
		    "re not: OBSRVR = #; TARGET = #;", (ftnlen)86);
	    errch_("#", obsrvr, (ftnlen)1, obsrvr_len);
	    errch_("#", target, (ftnlen)1, target_len);
	    sigerr_("SPICE(BODIESNOTDISTINCT)", (ftnlen)24);
	    chkout_("ZZGFFVIN", (ftnlen)8);
	    return 0;
	}
    }

/*     Process the target frame. The target frame is defined except */
/*     when the target is an ephemeris object modeled as a point. */

    if (svuray || svetrg) {

/*        We'll use the target frame argument. Look up the target */
/*        frame's ID code. But first, check for a blank frame name, */
/*        since this may be a common problem for the GF FOV system. */

	if (s_cmp(tframe, " ", tframe_len, (ftnlen)1) == 0) {
	    setmsg_("The target is not modeled as a point, but the associate"
		    "d frame name is blank.", (ftnlen)77);
	    sigerr_("SPICE(INVALIDFRAME)", (ftnlen)19);
	    chkout_("ZZGFFVIN", (ftnlen)8);
	    return 0;
	}
	namfrm_(tframe, &framid, tframe_len);
	if (framid == 0) {
	    setmsg_("The target frame name # is not recognized.", (ftnlen)42);
	    errch_("#", tframe, (ftnlen)1, tframe_len);
	    sigerr_("SPICE(INVALIDFRAME)", (ftnlen)19);
	    chkout_("ZZGFFVIN", (ftnlen)8);
	    return 0;
	}

/*        Save the target frame name. */

	ljust_(tframe, svtfrm, tframe_len, (ftnlen)32);
	ucase_(svtfrm, svtfrm, (ftnlen)32, (ftnlen)32);

/*        Obtain the center of the frame. If the target is an ephemeris */
/*        object, we must verify the frame center is the target. */

	frinfo_(&framid, &frcent, &frclss, &clssid, &found);
	if (! found) {

/*           Since we mapped the frame name to an ID code, we expect to */
/*           find the frame info. Getting here may be a sign of an */
/*           invalid frame kernel. */

	    setmsg_("Frame ID found for # body-fixed frame # but FRINFO coul"
		    "dn't find frame info. This may be due to a frame kernel "
		    "error.", (ftnlen)117);
	    errch_("#", target, (ftnlen)1, target_len);
	    sigerr_("SPICE(FRAMEINFONOTFOUND)", (ftnlen)24);
	    chkout_("ZZGFFVIN", (ftnlen)8);
	    return 0;
	}
	if (svetrg) {

/*           We have an ellipsoidal target. Check the target frame's */
/*           center. */

	    if (frcent != svtarg) {

/*              The supposed body-fixed frame for the target isn't */
/*              actually centered on the target. */

		setmsg_("Supposed body-fixed frame # for target # is actuall"
			"y centered on body #.", (ftnlen)72);
		errch_("#", tframe, (ftnlen)1, tframe_len);
		errch_("#", target, (ftnlen)1, target_len);
		errint_("#", &frcent, (ftnlen)1);
		sigerr_("SPICE(INVALIDFRAME)", (ftnlen)19);
		chkout_("ZZGFFVIN", (ftnlen)8);
		return 0;
	    }
	}
    }

/*     Process the aberration correction specifier. */

    if (svuray) {

/*        The target is represented by a ray. Check and save the */
/*        aberration correction. */

	zzprscor_(abcorr, attblk, abcorr_len);
	if (failed_()) {
	    chkout_("ZZGFFVIN", (ftnlen)8);
	    return 0;
	}

/*        Reject aberration correction flags calling for any type of */
/*        light time correction. However, stellar aberration corrections */
/*        are allowed: note this is the reverse of the situation for */
/*        ephemeris objects. The allowed aberration correction flags are */

/*           'NONE', 'S', 'XS' */

	if (attblk[1]) {
	    setmsg_("Aberration correction flag # calls for light time corre"
		    "ctions; these are not supported for targets represented "
		    "by rays.", (ftnlen)119);
	    errch_("#", abcorr, (ftnlen)1, abcorr_len);
	    sigerr_("SPICE(INVALIDOPTION)", (ftnlen)20);
	    chkout_("ZZGFFVIN", (ftnlen)8);
	    return 0;
	}

/*        Save flags indicating whether to use stellar aberration */
/*        corrections and indicating the sense of radiation travel. */

	svustl = attblk[2];
	svxmit = attblk[4];
    } else {

/*        The target is an ephemeris object. */

/*        Check the aberration correction. If SPKEZR can't handle it, */
/*        neither can we. */

	zzvalcor_(abcorr, attblk, abcorr_len);
	if (failed_()) {
	    chkout_("ZZGFFVIN", (ftnlen)8);
	    return 0;
	}
    }

/*     Remove all spaces from ABCORR then convert to uppercase. Save */
/*     this version of the aberration correction specifier. */

    cmprss_(" ", &c__0, abcorr, svcorr, (ftnlen)1, abcorr_len, (ftnlen)5);
    ucase_(svcorr, svcorr, (ftnlen)5, (ftnlen)5);

/*     Process the target body's radii, if applicable. */

    if (svetrg) {

/*        Fetch and check the radii. */

	zzgftreb_(&svtarg, svtrad);
	if (failed_()) {
	    chkout_("ZZGFFVIN", (ftnlen)8);
	    return 0;
	}

/*        Checks of radii have been completed. */

    } else {

/*        We don't have an ellipsoidal target body: zero out radius */
/*        values for this target. */

	cleard_(&c__3, svtrad);
    }

/*     Check the direction vector, if applicable. */

    if (svuray) {

/*        Make sure the direction vector is non-zero. Save a unit-length */
/*        copy of the vector. */

	if (vzero_(raydir)) {
	    setmsg_("Input ray direction was the zero vector; this vector mu"
		    "st be non-zero.", (ftnlen)70);
	    sigerr_("SPICE(ZEROVECTOR)", (ftnlen)17);
	    chkout_("ZZGFFVIN", (ftnlen)8);
	    return 0;
	}
	vhat_(raydir, svrdir);
    }

/*     Look up the instrument's ID code. */

    bods2c_(inst, &svinst, &found, inst_len);
    if (! found) {
	setmsg_("'#' is not a recognized name for an instrument. The cause o"
		"f this problem may be that you have not loaded a required fr"
		"ame kernel or instrument kernel.", (ftnlen)151);
	errch_("#", inst, (ftnlen)1, inst_len);
	sigerr_("SPICE(IDCODENOTFOUND)", (ftnlen)21);
	chkout_("ZZGFFVIN", (ftnlen)8);
	return 0;
    }

/*     Save the instrument's name. */

    ljust_(inst, svinam, inst_len, (ftnlen)36);
    ucase_(svinam, svinam, (ftnlen)36, (ftnlen)36);

/*     Look up the instrument parameters. */

    getfov_(&svinst, &c__10000, svishp, svifrm, bsite, &svnvrt, svbnds, (
	    ftnlen)9, (ftnlen)32);
    if (failed_()) {
	chkout_("ZZGFFVIN", (ftnlen)8);
	return 0;
    }

/*     Scale boresight vector to unit length. */

    vhatip_(bsite);

/*     Make sure the instrument shape specifier is left-justified */
/*     and in upper case. */

    ljust_(svishp, svishp, (ftnlen)9, (ftnlen)9);
    ucase_(svishp, svishp, (ftnlen)9, (ftnlen)9);

/*     If the instrument's shape is 'RECTANGLE', map it to */
/*     'POLYGON' */

    if (s_cmp(svishp, "RECTANGLE", (ftnlen)9, (ftnlen)9) == 0) {
	s_copy(svishp, "POLYGON", (ftnlen)9, (ftnlen)7);
    }

/*     Save an axis vector for the FOV. For circular and ellipsoidal */
/*     FOVs, the boresight serves as this axis. For polygonal FOVs */
/*     (rectangular FOVs are included), we'll generate an axis vector. */

    if (s_cmp(svishp, "POLYGON", (ftnlen)9, (ftnlen)7) == 0) {
	zzfovaxi_(inst, &svnvrt, svbnds, svfaxi, inst_len);
	if (failed_()) {
	    chkout_("ZZGFFVIN", (ftnlen)8);
	    return 0;
	}
    } else {
	vequ_(bsite, svfaxi);
    }

/*     Check the angular radius of the FOV. */

/*     Compute the angular radius of the FOV. We'll use this to define a */
/*     "bounding cone" centered on the FOV axis and having its apex at */
/*     the observer. This cone will be used for a preliminary FOV */
/*     exclusion test. */

    svarad = 0.;
    i__1 = svnvrt;
    for (i__ = 1; i__ <= i__1; ++i__) {
/* Computing MAX */
	d__1 = svarad, d__2 = vsep_(&svbnds[(i__2 = i__ * 3 - 3) < 30000 && 0 
		<= i__2 ? i__2 : s_rnge("svbnds", i__2, "zzgffvu_", (ftnlen)
		1282)], svfaxi);
	svarad = max(d__1,d__2);
    }

/*     Our algorithms can't handle FOVs with angular radius of 90 */
/*     degrees. */

    if (svarad > halfpi_() - 1e-6) {
	setmsg_("FOV angular radius of # degrees exceeds limit of # degrees.",
		 (ftnlen)59);
	d__1 = svarad * dpr_();
	errdp_("#", &d__1, (ftnlen)1);
	d__1 = (halfpi_() - 1e-6) * dpr_();
	errdp_("#", &d__1, (ftnlen)1);
	sigerr_("SPICE(FOVTOOWIDE)", (ftnlen)17);
	chkout_("ZZGFFVIN", (ftnlen)8);
	return 0;
    }

/*     Convert the FOV shape specifier to a left-justified, upper */
/*     case form. */

    ljust_(svishp, svishp, (ftnlen)9, (ftnlen)9);
    ucase_(svishp, svishp, (ftnlen)9, (ftnlen)9);

/*     We can make the search more efficient by computing any */
/*     required, time-invariant quantities here in the initialization */
/*     routine. */

/*     Compute the FOV plane SVPLAN, which is represented in the */
/*     instrument frame. The origin will be considered to be located at */
/*     the observer. The plane is normal to the FOV axis, at distance 1 */
/*     unit from the observer. */

    nvc2pl_(svfaxi, &c_b86, svplan);

/*     Find the point on the plane closest to the origin. This is */
/*     the center of the FOV. */

    vhat_(svfaxi, svfvct);

/*     If applicable, perform the computations required for an */
/*     elliptical FOV, where the target representation is arbitrary, or */
/*     a circular FOV when the target is an ellipsoid. Note that */
/*     these computations are not needed for the combination of a */
/*     circular FOV and a point or ray target. */

    if (s_cmp(svishp, "ELLIPSE", (ftnlen)9, (ftnlen)7) == 0 || s_cmp(svishp, 
	    "CIRCLE", (ftnlen)9, (ftnlen)6) == 0 && svetrg) {

/*        Also compute the center, semi-axis vectors, and semi-axis */
/*        lengths of the FOV. If the FOV is circular, we create an */
/*        artificial, second semi-axis vector. */

	if (s_cmp(svishp, "CIRCLE", (ftnlen)9, (ftnlen)6) == 0) {

/*           We have a circular FOV. We'll create an artificial, second */
/*           boundary vector, which will give rise to a second */
/*           semi-axis. */

	    d__1 = halfpi_();
	    vrotv_(svbnds, svfaxi, &d__1, &svbnds[3]);
	}

/*        Now find the endpoints of the semi-axes in this plane. */

	for (i__ = 1; i__ <= 2; ++i__) {
	    inrypl_(svorig, &svbnds[(i__1 = i__ * 3 - 3) < 30000 && 0 <= i__1 
		    ? i__1 : s_rnge("svbnds", i__1, "zzgffvu_", (ftnlen)1355)]
		    , svplan, &nxpts, &semipt[(i__2 = i__ * 3 - 3) < 6 && 0 <=
		     i__2 ? i__2 : s_rnge("semipt", i__2, "zzgffvu_", (ftnlen)
		    1355)]);
	    if (nxpts != 1) {
		setmsg_("Error creating FOV semi-axis vectors, NXPTS = #. Th"
			"is may indicate an error in the IK parameters for #.",
			 (ftnlen)103);
		errint_("#", &nxpts, (ftnlen)1);
		errch_("#", inst, (ftnlen)1, inst_len);
		sigerr_("SPICE(DEGENERATECASE)", (ftnlen)21);
		chkout_("ZZGFFVIN", (ftnlen)8);
		return 0;
	    }

/*           Compute and find the length of each semi-axis vector. */

	    vsub_(&semipt[(i__1 = i__ * 3 - 3) < 6 && 0 <= i__1 ? i__1 : 
		    s_rnge("semipt", i__1, "zzgffvu_", (ftnlen)1375)], svfvct,
		     &svsemi[(i__2 = i__ * 3 - 3) < 6 && 0 <= i__2 ? i__2 : 
		    s_rnge("svsemi", i__2, "zzgffvu_", (ftnlen)1375)]);
	    svxmag[(i__1 = i__ - 1) < 2 && 0 <= i__1 ? i__1 : s_rnge("svxmag",
		     i__1, "zzgffvu_", (ftnlen)1377)] = vnorm_(&svsemi[(i__2 =
		     i__ * 3 - 3) < 6 && 0 <= i__2 ? i__2 : s_rnge("svsemi", 
		    i__2, "zzgffvu_", (ftnlen)1377)]);
	    if (svxmag[(i__1 = i__ - 1) < 2 && 0 <= i__1 ? i__1 : s_rnge(
		    "svxmag", i__1, "zzgffvu_", (ftnlen)1379)] == 0.) {
		setmsg_("FOV semi-axis #* for @ has zero length.", (ftnlen)39)
			;
		errint_("*", &i__, (ftnlen)1);
		errch_("@", inst, (ftnlen)1, inst_len);
		sigerr_("SPICE(DEGENERATECASE)", (ftnlen)21);
		chkout_("ZZGFFVIN", (ftnlen)8);
		return 0;
	    }
	}
    }

/*     If we have an ellipsoidal target, and the FOV is circular or */
/*     elliptical, we'll create an ellipsoid whose limb coincides with */
/*     the FOV. This allows use to later use ZZOCCED to determine the */
/*     target's visibility. */

    if ((s_cmp(svishp, "CIRCLE", (ftnlen)9, (ftnlen)6) == 0 || s_cmp(svishp, 
	    "ELLIPSE", (ftnlen)9, (ftnlen)7) == 0) && svetrg) {


/*        Create an ellipsoid whose semi-axes are consistent with the */
/*        ellipse in SVPLAN defined by SEMIPT. Caution: after we */
/*        create this ellipsoid, we'll scale and shift it so it doesn't */
/*        extend too far from the observer. */

/*        Recall the origin is that of the instrument frame. The plane */
/*        SVPLAN is normal to the FOV axis and has distance 1 km from */
/*        the origin. */

/*        To start out, select the center of the ellipsoid. We place the */
/*        center along the direction defined by the FOV axis, at a */
/*        distance beyond SVPLAN (that is, on the side of the plane */
/*        opposite the observer), such that a sphere centered at this */
/*        point would have a limb consisting of a circle of radius */
/*        SVXMAG(1). If CTREXT is the distance of the ellipsoid center */
/*        from SVFVCT, then the limb geometry requires */

/*           CTREXT / SVXMAG(1) = SVXMAG(1) / 1 */


/* Computing 2nd power */
	d__1 = svxmag[0];
	ctrext = d__1 * d__1;

/*        The ellipsoid's center is SVEDCT. */

	d__1 = ctrext + 1.;
	vscl_(&d__1, svfvct, svedct);

/*        NOTE: in the code and discussion that follow, there are */
/*        references to both the FOV center SVFVCT and the ellipsoid */
/*        center SVEDCT. Note that the directions of the ellipsoid's */
/*        semi-axes point from the FOV center, NOT the ellipsoid center, */
/*        toward the intercepts of the FOV boundary vectors on the */
/*        FOV plane. */

/*        Compute the radius of the sphere centered at SVEDCT. The */
/*        ellipsoid's semi-axes pointing in the FOV axis direction and */
/*        in the direction from SVFVCT toward SEMIPT(*,1) will have this */
/*        length. */

	ellrad[2] = svxmag[0] * sqrt(pow_dd(svxmag, &c_b116) + 1.);
	ellrad[0] = ellrad[2];

/*        Compute the corresponding columns of the FOV semi-axis matrix. */

/*        The ellipsoid's third axis points along the FOV axis. Note */
/*        that SVFVCT is a unit vector pointing in the desired */
/*        direction. */

	vscl_(&ellrad[2], svfvct, &svfsmx[6]);

/*        The first ellipsoid semi-axis is associated with SEMIPT(*,1) */
/*        and also has length ELLRAD(3): */

	vhat_(svsemi, vtemp);
	vscl_(ellrad, vtemp, svfsmx);

/*        The ellipsoid's second semi-axis points from SVFVCT toward */
/*        SEMIPT(*,2). The ratio of its length to that of the other */
/*        semi-axis is the ratio of the length of the FOV's second */
/*        semi-axis to that of its first. Note that we've already ruled */
/*        out divide-by-zero errors here. */

	ellrad[1] = svxmag[1] / svxmag[0] * ellrad[2];

/*        We define the third axis using a cross product to */
/*        ensure we produce a matrix with positive determinant. */

	ucrss_(&svfsmx[6], svfsmx, vtemp);
	vscl_(&ellrad[1], vtemp, &svfsmx[3]);

/*        Scale the ellipsoid and the distance of its center from */
/*        the observer so that the ellipsoid is of reasonable size */
/*        and doesn't extend too far from the observer. Caution: this */
/*        modification means the ellipsoid no longer intersects the FOV */
/*        plane at the FOV boundary. The ellipsoid is still usable with */
/*        ZZOCCED, which is the ellipsoid's raison d'etre. */

	escale = 1e-5 / (vnorm_(svedct) + max(ellrad[0],ellrad[1]));
	for (i__ = 1; i__ <= 3; ++i__) {
	    vsclip_(&escale, &svfsmx[(i__1 = i__ * 3 - 3) < 9 && 0 <= i__1 ? 
		    i__1 : s_rnge("svfsmx", i__1, "zzgffvu_", (ftnlen)1491)]);
	}
	vsclip_(&escale, svedct);
    }
    if (s_cmp(svishp, "CIRCLE", (ftnlen)9, (ftnlen)6) == 0 && ! svetrg) {

/*        We have a circular FOV and a point or ray target model. */
/*        In this case, our FOV inclusion test is simple as can */
/*        be: we just compare the angular separation of the */
/*        target and FOV axis against the angular radius of the */
/*        FOV. Compute and save this angular radius. */

	svarad = vsep_(svfaxi, svbnds);
    } else if ((s_cmp(svishp, "RECTANGLE", (ftnlen)9, (ftnlen)9) == 0 || 
	    s_cmp(svishp, "POLYGON", (ftnlen)9, (ftnlen)7) == 0) && ! svetrg) 
	    {

/*        We have a rectangular or polygonal FOV and a ray or point */
/*        target. */

/*        We're going to represent the FOV boundary by a polygon */
/*        in the FOV plane SVPLAN. We want to be able to use a */
/*        2-dimensional winding number computation to decide whether */
/*        the target is within the FOV. We'll need a reference */
/*        frame with the Z-axis parallel to the FOV axis vector; */
/*        we'll represent the intersections of the boundary vectors */
/*        with the FOV plane in this frame. Then our 2D polygon */
/*        will have vertices given by the (X,Y) components of each */
/*        intersection. */

	vequ_(svfaxi, z__);
	frame_(z__, x, y);
	for (i__ = 1; i__ <= 3; ++i__) {
	    svfovm[(i__1 = i__ * 3 - 3) < 9 && 0 <= i__1 ? i__1 : s_rnge(
		    "svfovm", i__1, "zzgffvu_", (ftnlen)1532)] = x[(i__2 = 
		    i__ - 1) < 3 && 0 <= i__2 ? i__2 : s_rnge("x", i__2, 
		    "zzgffvu_", (ftnlen)1532)];
	    svfovm[(i__1 = i__ * 3 - 2) < 9 && 0 <= i__1 ? i__1 : s_rnge(
		    "svfovm", i__1, "zzgffvu_", (ftnlen)1533)] = y[(i__2 = 
		    i__ - 1) < 3 && 0 <= i__2 ? i__2 : s_rnge("y", i__2, 
		    "zzgffvu_", (ftnlen)1533)];
	    svfovm[(i__1 = i__ * 3 - 1) < 9 && 0 <= i__1 ? i__1 : s_rnge(
		    "svfovm", i__1, "zzgffvu_", (ftnlen)1534)] = z__[(i__2 = 
		    i__ - 1) < 3 && 0 <= i__2 ? i__2 : s_rnge("z", i__2, 
		    "zzgffvu_", (ftnlen)1534)];
	}

/*        Compute the intersections of the FOV boundary vectors with the */
/*        FOV plane. For each intercept, find the vector pointing from */
/*        the FOV center to that intercept. Transform each such */
/*        difference vector into the FOV frame. Save the projection onto */
/*        the FOV frame's X-Y plane. */

	i__1 = svnvrt;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    inrypl_(svorig, &svbnds[(i__2 = i__ * 3 - 3) < 30000 && 0 <= i__2 
		    ? i__2 : s_rnge("svbnds", i__2, "zzgffvu_", (ftnlen)1546)]
		    , svplan, &nxpts, xpt);
	    if (nxpts != 1) {
		setmsg_("Error finding FOV plane intercept of FOV boundary v"
			"ector #, NXPTS = #. This may indicate an error in th"
			"e IK parameters for #.", (ftnlen)125);
		errint_("#", &i__, (ftnlen)1);
		errint_("#", &nxpts, (ftnlen)1);
		errch_("#", inst, (ftnlen)1, inst_len);
		sigerr_("SPICE(DEGENERATECASE)", (ftnlen)21);
		chkout_("ZZGFFVIN", (ftnlen)8);
		return 0;
	    }
	    vsub_(xpt, svfvct, vtemp);
	    mxv_(svfovm, vtemp, vtemp2);
	    svfpol[(i__2 = (i__ << 1) - 2) < 20000 && 0 <= i__2 ? i__2 : 
		    s_rnge("svfpol", i__2, "zzgffvu_", (ftnlen)1567)] = 
		    vtemp2[0];
	    svfpol[(i__2 = (i__ << 1) - 1) < 20000 && 0 <= i__2 ? i__2 : 
		    s_rnge("svfpol", i__2, "zzgffvu_", (ftnlen)1568)] = 
		    vtemp2[1];
	}
    }
    chkout_("ZZGFFVIN", (ftnlen)8);
    return 0;
/* $Procedure ZZGFFVST ( GF, "is target in FOV?"  ) */

L_zzgffvst:
/* $ Abstract */

/*     Indicate whether the target is currently in the instrument FOV. */

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

/*     SPK */
/*     TIME */

/* $ Keywords */

/*     SEARCH */
/*     GEOMETRY */

/* $ Declarations */

/*     DOUBLE PRECISION      TIME */
/*     LOGICAL               VISTAT */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     TIME       I   TDB epoch (in seconds past J2000) */
/*     VISTAT     O   .TRUE. if the object is visible, .FALSE. */
/*                    otherwise. */

/* $ Detailed_Input */

/*     TIME       is the epoch of interest in TDB seconds past the */
/*                J2000 epoch. */

/* $ Detailed_Output */

/*     VISTAT     is a logical flag indicating the state of visibility. */
/*                If the target is in the instrument FOV at epoch TIME, */
/*                where target and instrument are those specified by the */
/*                last call to ZZGFFVIN, VISTAT is returned with the */
/*                value .TRUE.; otherwise VISTAT is .FALSE. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1) If any SPK lookup fails, the error will be diagnosed by */
/*        routines in the call tree of this routine. */

/*     2) If any frame transformation lookup fails, the error will be */
/*        diagnosed by routines in the call tree of this routine. */

/*     3) If the FOV is polygonal, the target is an ellipsoid, */
/*        and while testing whether the target is visible, an error */
/*        occurs due to FOV errors not detected in the initialization */
/*        step, the error will be diagnosed by routines in the call tree */
/*        of this routine. */

/*     4) If the FOV is circular or elliptical, the target is an */
/*        ellipsoid, and while testing whether the target is visible, an */
/*        error occurs due to degenerate geometry of the limb, FOV, or */
/*        both, the error will be diagnosed by routines in the call tree */
/*        of this routine. */

/*     5) If the target shape is not recognized, the error will be */
/*        diagnosed by routines in the call tree of this routine. */

/*     6)  If a body RADII vector has other than exactly thee elements, */
/*         the error SPICE(INVALIDCOUNT) is signaled by a routine in */
/*         the call tree of this routine. */

/*     7)  If a body RADII vector has any element less-than or */
/*         equal to zero, the error SPICE(BADAXISLENGTH) is signaled by */
/*         a routine in the call tree of this routine. */

/* $ Files */

/*     See the Files header section of the umbrella routine ZZGFFVU. */

/* $ Particulars */

/*     This routine determines the visibility state of the */
/*     configuration specified by the last call to ZZGFFVIN and the */
/*     input time value. */

/* $ Examples */

/*     See the umbrella routine ZZGFFVU. */

/* $ Restrictions */

/*     This is a SPICELIB private routine; it should not be called by */
/*     user applications. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman   (JPL) */
/*     L.S. Elson     (JPL) */
/*     W.L. Taber     (JPL) */
/*     E.D. Wright    (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 05-MAR-2009 (NJB) (LSE) (WLT) (EDW) */

/* -& */

/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }
    chkin_("ZZGFFVST", (ftnlen)8);

/*     Initialize the state output. */

    *vistat = FALSE_;

/*     The algorithm for the state determination depends on the */
/*     target model and the FOV shape. */

    if (svetrg) {

/*        The target is an ephemeris object modeled as an ellipsoid. */
/*        There are two branches here: one for a rectangular/ */
/*        polygonal FOV and one for a circular/elliptical FOV. */

/*        Start by finding the observer-target position vector in the */
/*        target body-fixed frame. */

	spkezp_(&svtarg, time, svtfrm, svcorr, &svobs, pos, &lt, (ftnlen)32, (
		ftnlen)5);

/*        Compute the target epoch. */

	zzcorepc_(svcorr, time, &lt, &ettarg, (ftnlen)5);

/*        Find the transformation from the target frame at ETTARG to the */
/*        instrument frame at TIME. We'll need to use J2000 as an */
/*        intermediate frame. */

	pxform_(svtfrm, "J2000", &ettarg, m1, (ftnlen)32, (ftnlen)5);
	pxform_("J2000", svifrm, time, m2, (ftnlen)5, (ftnlen)32);
	if (failed_()) {
	    chkout_("ZZGFFVST", (ftnlen)8);
	    return 0;
	}
	mxm_(m2, m1, insmat);
	if (s_cmp(svishp, "RECTANGLE", (ftnlen)9, (ftnlen)9) == 0 || s_cmp(
		svishp, "POLYGON", (ftnlen)9, (ftnlen)7) == 0) {

/*           The FOV is a rectangle or other polygon; we treat both */
/*           cases the same way. */

/*           Negate POS to obtain the position of the observer with */
/*           respect to the target. */

	    vminus_(pos, obspos);

/*           Find the limb in the target body-fixed frame. */

	    edlimb_(svtrad, &svtrad[1], &svtrad[2], obspos, limb);

/*           Transform the limb from the target frame at ETTARG */
/*           to the instrument frame at TIME. The matrix INSMAT */
/*           effects just this transformation. We unpack the center */
/*           and semi-axis vectors of LIMB, transform them, and */
/*           pack the results into FVLIMB. Below, M1 and M2 are */
/*           simply temporary 3x3 matrices. */

	    el2cgv_(limb, m1, &m1[3], &m1[6]);

/*           Before performing the frame transformation on the */
/*           limb's center, translate the center so that the */
/*           observer is at the origin. Since POS is expressed */
/*           in the target body-fixed frame, this is a convenient */
/*           place for the translation. */

	    vadd_(pos, m1, vtemp);
	    vequ_(vtemp, m1);
	    for (i__ = 1; i__ <= 3; ++i__) {
		mxv_(insmat, &m1[(i__1 = i__ * 3 - 3) < 9 && 0 <= i__1 ? i__1 
			: s_rnge("m1", i__1, "zzgffvu_", (ftnlen)1807)], &m2[(
			i__2 = i__ * 3 - 3) < 9 && 0 <= i__2 ? i__2 : s_rnge(
			"m2", i__2, "zzgffvu_", (ftnlen)1807)]);
	    }
	    cgv2el_(m2, &m2[3], &m2[6], fvlimb);

/*           All geometric objects in the following call are expressed */
/*           in the instrument reference frame. */

/*           The target is in the FOV if and only if ZZELVUPY finds an */
/*           intersection, so we use VISTAT as the "found" flag. */

	    zzelvupy_(fvlimb, svorig, svfaxi, &svnvrt, svbnds, vistat);
	} else if (s_cmp(svishp, "CIRCLE", (ftnlen)9, (ftnlen)6) == 0 || 
		s_cmp(svishp, "ELLIPSE", (ftnlen)9, (ftnlen)7) == 0) {

/*           The FOV is a circle or ellipse. For both FOV shapes, */
/*           we represent the FOV by an ellipsoid in the FOV */
/*           frame. We can then use ZZOCCED to determine whether */
/*           there's any overlap of this ellipsoid and the target. */

/*           We'll perform the occultation test in the instrument frame, */
/*           so we'll need to represent the observer-target position */
/*           and target semi-axes in that frame. */

/*           Transform the target position to the instrument frame. */

	    mxv_(insmat, pos, trgctr);

/*           The columns of INSMAT are the target body's semi-axis */
/*           direction vectors; we scale these by the target radii */
/*           to obtain the semi-axis matrix for the target. */

	    for (i__ = 1; i__ <= 3; ++i__) {
		vscl_(&svtrad[(i__1 = i__ - 1) < 3 && 0 <= i__1 ? i__1 : 
			s_rnge("svtrad", i__1, "zzgffvu_", (ftnlen)1845)], &
			insmat[(i__2 = i__ * 3 - 3) < 9 && 0 <= i__2 ? i__2 : 
			s_rnge("insmat", i__2, "zzgffvu_", (ftnlen)1845)], &
			trgsmx[(i__3 = i__ * 3 - 3) < 9 && 0 <= i__3 ? i__3 : 
			s_rnge("trgsmx", i__3, "zzgffvu_", (ftnlen)1845)]);
	    }
	    ocstat = zzocced_(svorig, svedct, svfsmx, trgctr, trgsmx);

/*           A return code of zero indicates no occultation. Any other */
/*           return code indicates a non-empty intersection of the */
/*           target and FOV. */

	    *vistat = ocstat != 0;
	} else {

/*           This is an unexpected FOV shape. We should have prevented */
/*           this problem in the initialization step, but we repeat the */
/*           check here for safety. */

	    setmsg_("The target body # has shape #; the only supported shape"
		    "s are ELLIPSOID, POINT, and RAY.", (ftnlen)87);
	    errch_("#", svtnam, (ftnlen)1, (ftnlen)36);
	    errch_("#", svishp, (ftnlen)1, (ftnlen)9);
	    sigerr_("SPICE(INVALIDSHAPE)", (ftnlen)19);
	    chkout_("ZZGFFVST", (ftnlen)8);
	    return 0;
	}

/*        This is the end of the ellipsoidal target case. At this */
/*        point, VISTAT is set. */

    } else {

/*        The target is a ray or an ephemeris object modeled as a point. */
/*        In either case, we want to obtain the aberration-corrected */
/*        observer-target vector. */

	if (svuray) {

/*           The target is represented by a ray expressed in the */
/*           frame SVTFRM. */

/*           Normally we'd need to correct the orientation of SVTFRM */
/*           for light time between the center of that frame and the */
/*           observer. But since light time corrections are not allowed */
/*           for targets represented by rays, we evaluate SVTFRM */
/*           at the current epoch TIME. */

	    pxform_(svtfrm, svifrm, time, insmat, (ftnlen)32, (ftnlen)32);
	    if (failed_()) {
		chkout_("ZZGFFVST", (ftnlen)8);
		return 0;
	    }

/*           Transform the ray's direction vector to the instrument */
/*           frame. */

	    mxv_(insmat, svrdir, dir);

/*           If we need to correct the ray's direction for stellar */
/*           aberration, do it now. */

	    if (svustl) {

/*              Find the state of the observer relative to the */
/*              solar system barycenter in the J2000 frame. */

		spkssb_(&svobs, time, "J2000", stobs, (ftnlen)5);

/*              Convert the direction vector to the J2000 frame. */

		pxform_(svifrm, "J2000", time, m1, (ftnlen)32, (ftnlen)5);
		if (failed_()) {
		    chkout_("ZZGFFVST", (ftnlen)8);
		    return 0;
		}
		mxv_(m1, dir, vtemp);

/*              Apply the stellar aberration correction. */

		if (svxmit) {

/*                 Use the transmission correction. */

		    stlabx_(vtemp, &stobs[3], vtemp2);
		} else {
		    stelab_(vtemp, &stobs[3], vtemp2);
		}

/*              Map the direction vector back to the instrument */
/*              frame. */

		mtxv_(m1, vtemp2, dir);
	    }

/*           The target direction in the instrument frame DIR has */
/*           been computed. */

	} else {

/*           The target is an ephemeris object. Look up the */
/*           target's position relative to the observer. */

/*           Note for the maintenance programmer: don't think of */
/*           changing this call to look up the position in the */
/*           instrument frame. :) Since we don't have a guarantee that */
/*           the instrument frame is centered on the observer (the frame */
/*           could be J2000, for example), and since we don't want to */
/*           correct the orientation of the instrument frame for light */
/*           time, we look up the direction vector in the J2000 frame */
/*           and then map it to the instrument frame. */

	    spkezp_(&svtarg, time, "J2000", svcorr, &svobs, vtemp, &lt, (
		    ftnlen)5, (ftnlen)5);
	    pxform_("J2000", svifrm, time, m1, (ftnlen)5, (ftnlen)32);
	    if (failed_()) {
		chkout_("ZZGFFVST", (ftnlen)8);
		return 0;
	    }
	    mxv_(m1, vtemp, dir);
	}
	if (failed_()) {
	    chkout_("ZZGFFVST", (ftnlen)8);
	    return 0;
	}

/*        The observer-target direction vector DIR is set. */

/*        The determination of whether the ray is in the FOV depends */
/*        on the FOV shape. */

	sep = vsep_(dir, svfaxi);
	if (s_cmp(svishp, "CIRCLE", (ftnlen)9, (ftnlen)6) == 0) {

/*           Just compare the angular separation of POS with the */
/*           FOV axis direction against the FOV angular radius SVARAD. */

	    *vistat = sep <= svarad;
	} else if (sep > svarad) {

/*           The FOV is an ellipse or polygon. */

/*           The angular separation of target and FOV axis is */
/*           greater than the angular radius of the exclusion */
/*           cone. The target can't be seen. */

	    *vistat = FALSE_;
	} else {

/*           The FOV is an ellipse or polygon. */

/*           The angular separation of target and FOV axis is */
/*           less than or equal to than the angular radius of the */
/*           exclusion code, so the target may be visible. */

/*           Find the intersection of the ray emanating from the */
/*           observer, and having direction vector POS, with the FOV */
/*           plane. */

	    inrypl_(svorig, dir, svplan, &nxpts, xpt);

/*           If there's no intersection, the target isn't visible. */

	    if (nxpts == 0) {
		*vistat = FALSE_;
	    } else if (nxpts != 1) {

/*              "This can't happen." :) */

		setmsg_("By construction, the vertex of the observer-target "
			"ray can't lie in the FOV plane. If somehow it does, "
			"we have a serious problem.", (ftnlen)129);
		sigerr_("SPICE(BUG)", (ftnlen)10);
		chkout_("ZZGFFVST", (ftnlen)8);
		return 0;
	    } else {

/*              NXPTS is 1. */

/*              Find the vector from the center of the FOV to XPT. */
/*              Call this vector FOVPT. */

		vsub_(xpt, svfvct, fovpt);
		if (s_cmp(svishp, "ELLIPSE", (ftnlen)9, (ftnlen)7) == 0) {

/*                 The FOV shape is elliptical. To decide whether FOVPT */
/*                 is within the FOV, compute the level surface */
/*                 parameter */

/*                                   2              2 */
/*                    L  =  ( x / a )   +  ( y / b ) */

/*                 and compare L to 1. We'll use the variable COORD */
/*                 to represent the coordinates (x,y). */

/*                 We've already eliminated zero divisors in the */
/*                 initialization routine. */

		    for (i__ = 1; i__ <= 2; ++i__) {
			coord[(i__1 = i__ - 1) < 2 && 0 <= i__1 ? i__1 : 
				s_rnge("coord", i__1, "zzgffvu_", (ftnlen)
				2074)] = vdot_(fovpt, &svsemi[(i__2 = i__ * 3 
				- 3) < 6 && 0 <= i__2 ? i__2 : s_rnge("svsemi"
				, i__2, "zzgffvu_", (ftnlen)2074)]) / svxmag[(
				i__3 = i__ - 1) < 2 && 0 <= i__3 ? i__3 : 
				s_rnge("svxmag", i__3, "zzgffvu_", (ftnlen)
				2074)];
		    }
		    d__1 = coord[0] / svxmag[0];
		    d__2 = coord[1] / svxmag[1];
		    l = pow_dd(&d__1, &c_b116) + pow_dd(&d__2, &c_b116);

/*                 The target is visible if FOVPT is inside the FOV */
/*                 ellipse; this condition is indicated by L <= 1. */

		    *vistat = l <= 1.;
		} else if (s_cmp(svishp, "POLYGON", (ftnlen)9, (ftnlen)7) == 
			0) {

/*                The FOV is a polygon. Convert FOVPT to the FOV frame, */
/*                then find the winding number of the FOV about the X-Y */
/*                projection of FOVPT. */

		    mxv_(svfovm, fovpt, vtemp);
		    pnt2d[0] = vtemp[0];
		    pnt2d[1] = vtemp[1];
		    w = zzwind2d_(&svnvrt, svfpol, pnt2d);

/*                 Any non-zero winding number indicates that the */
/*                 FOV polygon wraps around the point representing */
/*                 the intercept of the target direction with the */
/*                 FOV plane. */

		    *vistat = w != 0;
		} else {

/*                 This is an unexpected FOV shape. We should have */
/*                 prevented this problem in the initialization step, */
/*                 but we repeat the check here for safety. */

		    setmsg_("Instrument #'s FOV has shape #; the only suppor"
			    "ted shapes are ELLIPSE, CIRCLE, and POLYGON.", (
			    ftnlen)91);
		    errch_("#", svinam, (ftnlen)1, (ftnlen)36);
		    errch_("#", svishp, (ftnlen)1, (ftnlen)9);
		    sigerr_("SPICE(INVALIDSHAPE)", (ftnlen)19);
		    chkout_("ZZGFFVST", (ftnlen)8);
		    return 0;
		}

/*              We've performed visibility tests for elliptical or */
/*              polygonal FOVs. VISTAT is set. */

	    }

/*           We've processed the intercept found by the INRYPL call, */
/*           or, if the intercept count was not 1, indicated that the */
/*           target is not visible. VISTAT is set. */

	}

/*        We've processed both the ray and point ephemeris object */
/*        cases. VISTAT is set. */

    }

/*     We've processed all target representation/FOV shape cases. */

    chkout_("ZZGFFVST", (ftnlen)8);
    return 0;
} /* zzgffvu_ */

/* Subroutine */ int zzgffvu_(char *inst, char *tshape, doublereal *raydir, 
	char *target, char *tframe, char *abcorr, char *obsrvr, doublereal *
	time, logical *vistat, ftnlen inst_len, ftnlen tshape_len, ftnlen 
	target_len, ftnlen tframe_len, ftnlen abcorr_len, ftnlen obsrvr_len)
{
    return zzgffvu_0_(0, inst, tshape, raydir, target, tframe, abcorr, obsrvr,
	     time, vistat, inst_len, tshape_len, target_len, tframe_len, 
	    abcorr_len, obsrvr_len);
    }

/* Subroutine */ int zzgffvin_(char *inst, char *tshape, doublereal *raydir, 
	char *target, char *tframe, char *abcorr, char *obsrvr, ftnlen 
	inst_len, ftnlen tshape_len, ftnlen target_len, ftnlen tframe_len, 
	ftnlen abcorr_len, ftnlen obsrvr_len)
{
    return zzgffvu_0_(1, inst, tshape, raydir, target, tframe, abcorr, obsrvr,
	     (doublereal *)0, (logical *)0, inst_len, tshape_len, target_len, 
	    tframe_len, abcorr_len, obsrvr_len);
    }

/* Subroutine */ int zzgffvst_(doublereal *time, logical *vistat)
{
    return zzgffvu_0_(2, (char *)0, (char *)0, (doublereal *)0, (char *)0, (
	    char *)0, (char *)0, (char *)0, time, vistat, (ftnint)0, (ftnint)
	    0, (ftnint)0, (ftnint)0, (ftnint)0, (ftnint)0);
    }

