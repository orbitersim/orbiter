/* zzgfocu.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__100 = 100;
static integer c__4 = 4;
static integer c__0 = 0;
static integer c__3 = 3;
static doublereal c_b185 = 1e-12;

/* $Procedure ZZGFOCU ( GF, occultation utilities ) */
/* Subroutine */ int zzgfocu_0_(int n__, char *occtyp, char *front, char *
	fshape, char *fframe, char *back, char *bshape, char *bframe, char *
	obsrvr, char *abcorr, doublereal *time, logical *ocstat, ftnlen 
	occtyp_len, ftnlen front_len, ftnlen fshape_len, ftnlen fframe_len, 
	ftnlen back_len, ftnlen bshape_len, ftnlen bframe_len, ftnlen 
	obsrvr_len, ftnlen abcorr_len)
{
    /* Initialized data */

    static integer ncalls = 0;
    static doublereal svorig[3] = { 0.,0.,0. };
    static char svtyps[7*4] = "ANNULAR" "ANY    " "PARTIAL" "FULL   ";

    /* System generated locals */
    integer i__1, i__2, i__3;
    doublereal d__1;

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_cmp(char *, char *, ftnlen, ftnlen), s_rnge(char *, integer, 
	    char *, integer);

    /* Local variables */
    doublereal srad;
    extern /* Subroutine */ int vscl_(doublereal *, doublereal *, doublereal *
	    );
    extern doublereal vsep_(doublereal *, doublereal *);
    extern /* Subroutine */ int vsub_(doublereal *, doublereal *, doublereal *
	    ), zzgftreb_(integer *, doublereal *), zzminrad_(doublereal *), 
	    zzcorepc_(char *, doublereal *, doublereal *, doublereal *, 
	    ftnlen), zzmaxrad_(doublereal *), zzvalcor_(char *, logical *, 
	    ftnlen);
    doublereal t2sep;
    extern /* Subroutine */ int zzsudski_(integer *, integer *, integer *, 
	    integer *);
    integer i__;
    extern /* Subroutine */ int zzprsmet_(integer *, char *, integer *, char *
	    , char *, logical *, integer *, integer *, char *, char *, ftnlen,
	     ftnlen, ftnlen, ftnlen, ftnlen);
    doublereal radii[3];
    extern /* Subroutine */ int minad_(doublereal *, integer *, doublereal *, 
	    integer *), maxad_(doublereal *, integer *, doublereal *, integer 
	    *), chkin_(char *, ftnlen);
    char shape[9];
    integer idobs;
    extern /* Subroutine */ int errch_(char *, char *, ftnlen, ftnlen), 
	    ucase_(char *, char *, ftnlen, ftnlen);
    doublereal bdist, fdist;
    integer trgid;
    logical found;
    extern /* Subroutine */ int moved_(doublereal *, integer *, doublereal *);
    doublereal mtemp[9]	/* was [3][3] */, tdist;
    integer nsurf;
    extern logical eqstr_(char *, char *, ftnlen, ftnlen);
    static integer svobs;
    extern doublereal vnorm_(doublereal *);
    extern /* Subroutine */ int ljust_(char *, char *, ftnlen, ftnlen), 
	    bods2c_(char *, integer *, logical *, ftnlen);
    integer idback;
    extern logical failed_(void);
    extern /* Subroutine */ int cleard_(integer *, doublereal *);
    integer occode;
    extern doublereal dasine_(doublereal *, doublereal *), halfpi_(void);
    extern integer isrchc_(char *, integer *, char *, ftnlen, ftnlen);
    doublereal bckfrt[3], bckobs[3];
    extern logical return_(void);
    char fixfrm[32], pntdef[20], posnam[10], shpstr[9], subtyp[20];
    static char svbfrm[32], svbmth[500], svbnam[36], svbshp[9], svcorr[5], 
	    svffrm[32], svfmth[500], svfnam[36], svfshp[9], svonam[36], 
	    svtype[7];
    char trmtyp[20];
    doublereal bckpos[3], bsmaxs[9]	/* was [3][3] */, etbcor, etfcor, 
	    frtbck[3], frtobs[3], frtpos[3], fsmaxs[9]	/* was [3][3] */, 
	    ltback, ltfrnt, maxang, minang, spoint[3], srfvec[3];
    static doublereal svbrad[3], svfrad[3], svmnbr, svmnfr, svmxbr, svmxfr;
    doublereal trgepc, trgsep;
    integer center, clssid, ffrmid, frclss, idfrnt, loc, occnum, srflst[100];
    static integer svback, svfrnt;
    logical attblk[15], pntocc, pri;
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), setmsg_(char *, ftnlen), suffix_(char *, integer *, char 
	    *, ftnlen, ftnlen), namfrm_(char *, integer *, ftnlen), frinfo_(
	    integer *, integer *, integer *, integer *, logical *), errint_(
	    char *, integer *, ftnlen), spkezp_(integer *, doublereal *, char 
	    *, char *, integer *, doublereal *, doublereal *, ftnlen, ftnlen),
	     pxform_(char *, char *, doublereal *, doublereal *, ftnlen, 
	    ftnlen), vminus_(doublereal *, doublereal *), sincpt_(char *, 
	    char *, doublereal *, char *, char *, char *, char *, doublereal *
	    , doublereal *, doublereal *, doublereal *, logical *, ftnlen, 
	    ftnlen, ftnlen, ftnlen, ftnlen, ftnlen);
    extern integer zzocced_(doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *);

/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines. Users should not call this routine directly due */
/*     to the volatile nature of this routine. */

/*     This routine contains the entry points that produce the */
/*     computations needed for solving for occultation states */
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

/*     GF */
/*     NAIF_IDS */
/*     PCK */
/*     SPK */
/*     TIME */

/* $ Keywords */

/*     GEOMETRY */
/*     OCCULTATION */
/*     SEARCH */

/* $ Declarations */

/*     File: dsk.inc */


/*     Version 1.0.0 05-FEB-2016 (NJB) */

/*     Maximum size of surface ID list. */


/*     End of include file dsk.inc */

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


/*     File: zzdsk.inc */


/*     Version 4.0.0 13-NOV-2015 (NJB) */

/*        Changed parameter LBTLEN to CVTLEN. */
/*        Added parameter LMBCRV. */

/*     Version 3.0.0 05-NOV-2015 (NJB) */

/*        Added parameters */

/*           CTRCOR */
/*           ELLCOR */
/*           GUIDED */
/*           LBTLEN */
/*           PNMBRL */
/*           TANGNT */
/*           TMTLEN */
/*           UMBRAL */

/*     Version 2.0.0 04-MAR-2015 (NJB) */

/*        Removed declaration of parameter SHPLEN. */
/*        This name is already in use in the include */
/*        file gf.inc. */

/*     Version 1.0.0 26-JAN-2015 (NJB) */


/*     Parameters supporting METHOD string parsing: */


/*     Local method length. */


/*     Length of sub-point type string. */


/*     Length of curve type string. */


/*     Limb type parameter codes. */


/*     Length of terminator type string. */


/*     Terminator type and limb parameter codes. */


/*     Length of aberration correction locus string. */


/*     Aberration correction locus codes. */


/*     End of include file zzdsk.inc */

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

/* $ Brief_I/O */

/*     VARIABLE  I/O  ENTRY POINTS */
/*     --------  ---  -------------------------------------------------- */
/*     OCCTYP     I   ZZGFOCIN */
/*     FRONT      I   ZZGFOCIN */
/*     FSHAPE     I   ZZGFOCIN */
/*     FFRAME     I   ZZGFOCIN */
/*     BACK       I   ZZGFOCIN */
/*     BSHAPE     I   ZZGFOCIN */
/*     BFRAME     I   ZZGFOCIN */
/*     OBSRVR     I   ZZGFOCIN */
/*     ABCORR     I   ZZGFOCIN */
/*     TIME       I   ZZGFOCST */
/*     OCSTAT     O   ZZGFOCST */

/* $ Detailed_Input */

/*     See entry points. */

/* $ Detailed_Output */

/*     See entry points. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  See entry points. */

/* $ Files */

/*     Appropriate SPICE kernels must be loaded by the calling program */
/*     before this routine is called. */

/*     The following data are required: */

/*     -  SPK data: the calling application must load ephemeris data */
/*        for the target, source and observer that cover the time */
/*        period specified by the window CNFINE. If aberration */
/*        corrections are used, the states of the target bodies and of */
/*        the observer relative to the solar system barycenter must be */
/*        calculable from the available ephemeris data. Typically */
/*        ephemeris data */
/*        are made available by loading one or more SPK files via */
/*        FURNSH. */

/*     -  PCK data: bodies modeled as triaxial ellipsoids must have */
/*        semi-axis lengths provided by variables in the kernel pool. */
/*        Typically these data are made available by loading a text */
/*        PCK file via FURNSH. */

/*     -  FK data: if either of the reference frames designated by */
/*        BFRAME or FFRAME are not built in to the SPICE system, */
/*        one or more FKs specifying these frames must be loaded. */

/*     The following data may be required: */

/*     -  DSK data: if either FSHAPE or BSHAPE indicates that DSK */
/*        data are to be used, DSK files containing topographic data */
/*        for the target body must be loaded. If a surface list is */
/*        specified, data for at least one of the listed surfaces must */
/*        be loaded. */

/*     -  Surface name-ID associations: if surface names are specified */
/*        in FSHAPE or BSHAPE, the association of these names with */
/*        their corresponding surface ID codes must be established by */
/*        assignments of the kernel variables */

/*           NAIF_SURFACE_NAME */
/*           NAIF_SURFACE_CODE */
/*           NAIF_SURFACE_BODY */

/*        Normally these associations are made by loading a text */
/*        kernel containing the necessary assignments. An example */
/*        of such a set of assignments is */

/*           NAIF_SURFACE_NAME += 'Mars MEGDR 128 PIXEL/DEG' */
/*           NAIF_SURFACE_CODE += 1 */
/*           NAIF_SURFACE_BODY += 499 */

/*     -  CK data: either of the body-fixed frames to which FFRAME or */
/*        BFRAME refer might be a CK frame. If so, at least one CK */
/*        file will be needed to permit transformation of vectors */
/*        between that frame and the J2000 frame. */

/*     -  SCLK data: if a CK file is needed, an associated SCLK */
/*        kernel is required to enable conversion between encoded SCLK */
/*        (used to time-tag CK data) and barycentric dynamical time */
/*        (TDB). */

/*     Kernel data are normally loaded once per program run, NOT every */
/*     time this routine is called. */

/* $ Particulars */

/*     This routine is designed to determine whether a specified */
/*     type of occultation or transit is in progress at a specified */
/*     epoch. Three methods of modeling the shapes of the target */
/*     bodies are supported: */

/*        1)  Model both target bodies as triaxial ellipsoids. For this */
/*            case, the user may choose between occultations that are */
/*            partial, full or annular. See the entry header for */
/*            ZZGFOCIN for an explanation of these terms. */

/*        2)  Treat one target body as a point object and the other */
/*            target body as a triaxial ellipsoid. The only supported */
/*            occultation type is "ANY" for this case. */

/*        3)  Treat one target body as a point object and the other */
/*            target body as an extended shape modeled by DSK data. */
/*            The only supported occultation type is "ANY" for this */
/*            case. */

/*     This routine contains two entry points that support searches */
/*     for occultations performed using ZZGFSOLV: */

/*        ZZGFOCIN   Saves the user-supplied inputs defining the */
/*                   occultation computation to be performed. */
/*                   Initializes the occultation search. */

/*        ZZGFOCST   Returns the occultation state for a specified */
/*                   time. */

/* $ Examples */

/*     See GFOCCE. */

/* $ Restrictions */

/*     1)  This is a SPICELIB private routine; it should not be called by */
/*         user applications. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     L.S. Elson         (JPL) */
/*     W.L. Taber         (JPL) */
/*     I.M. Underwood     (JPL) */
/*     E.D. Wright        (JPL) */

/* $ Version */

/* -    SPICELIB Version 2.1.0, 17-OCT-2021 (EDW) (JDR) */

/*        Body radii accessed from kernel pool using ZZGFTREB in */
/*        ZZGFOCIN. */

/*        Edited the header of the umbrella routine and all its entry */
/*        points to comply with NAIF standard. Added $Index_Entries. */

/* -    SPICELIB Version 2.0.0, 21-FEB-2017 (NJB) */

/*        Modified FAILED tests. */

/*        31-DEC-2016 (NJB) */

/*        Corrected long error message for blank frame string. */
/*        Previously the order of ERRCH and SIGERR calls was */
/*        inverted. */

/*        Updated long error message for incorrect body-fixed */
/*        frame center, so the ID of the target body is included. */

/*        24-FEB-2016 (NJB) */

/*        DSK capability was integrated. */

/*        04-MAR-2015 (NJB) */

/*        Initial updates to support surfaces represented by DSKs. */

/* -    SPICELIB Version 1.1.0, 18-MAY-2014 (NJB) */

/*        Bug fix: in entry point ZZGFOCIN, corrected long error message */
/*        for the case in which a body-fixed reference frame is not */
/*        centered on the correct body. */

/* -    SPICELIB Version 1.0.0, 05-MAR-2009 (NJB) (LSE) (WLT) (IMU) (EDW) */

/* -& */
/* $ Index_Entries */

/*     solving for occultation states in GF routines */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     ALPHA is a bound for the fraction of the speed of light */
/*     at which target body may move, relative to the solar */
/*     system barycenter. */


/*     ATOL is a tolerance value for computing arc sine. */


/*     Local variables */


/*     Saved variables */

    switch(n__) {
	case 1: goto L_zzgfocin;
	case 2: goto L_zzgfocst;
	}


/*     Initial values */


/*     Below we initialize the list of occultation types. */


/*     This routine should never be called directly. */

    chkin_("ZZGFOCU", (ftnlen)7);
    sigerr_("SPICE(BOGUSENTRY)", (ftnlen)17);
    chkout_("ZZGFOCU", (ftnlen)7);
    return 0;
/* $Procedure ZZGFOCIN ( GF, occultation initialization ) */

L_zzgfocin:
/* $ Abstract */

/*     Perform initialization functions for occultation state */
/*     determination. */

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

/*     GEOMETRY */
/*     OCCULTATION */
/*     SEARCH */

/* $ Declarations */

/*     CHARACTER*(*)         OCCTYP */
/*     CHARACTER*(*)         FRONT */
/*     CHARACTER*(*)         FSHAPE */
/*     CHARACTER*(*)         FFRAME */
/*     CHARACTER*(*)         BACK */
/*     CHARACTER*(*)         BSHAPE */
/*     CHARACTER*(*)         BFRAME */
/*     CHARACTER*(*)         OBSRVR */
/*     CHARACTER*(*)         ABCORR */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     OCCTYP     I   Type of occultation. */
/*     FRONT      I   Name of body occulting the other. */
/*     FSHAPE     I   Type of shape model used for front body. */
/*     FFRAME     I   Body-fixed, body-centered frame for front body. */
/*     BACK       I   Name of body occulted by the other. */
/*     BSHAPE     I   Type of shape model used for back body. */
/*     BFRAME     I   Body-fixed, body-centered frame for back body. */
/*     OBSRVR     I   Name of the observing body. */
/*     ABCORR     I   Aberration correction flag. */

/* $ Detailed_Input */

/*     OCCTYP   indicates the type of occultation that is to be found. */
/*              The full set of possible values of OCCTYP may be used */
/*              when both target bodies are modeled as ellipsoids. */
/*              When either target is modeled as a point, OCCTYP must */
/*              be set to 'ANY' (see description below). */

/*              Supported values of OCCTYP and corresponding */
/*              definitions are: */


/*                 'FULL'      denotes the full occultation of the */
/*                             body designated by BACK by the body */
/*                             designated by FRONT, as seen from the */
/*                             location of the observer. In other */
/*                             words, the occulted body is completely */
/*                             invisible as seen from the observer's */
/*                             location. */

/*                 'ANNULAR'   denotes an annular occultation: the */
/*                             body designated by FRONT blocks part */
/*                             of, but not the limb of, the body */
/*                             designated by BACK, as seen from the */
/*                             location of the observer. */

/*                 'PARTIAL'   denotes a partial, non-annular */
/*                             occultation: the body designated by */
/*                             FRONT blocks part, but not all, of the */
/*                             limb of the body designated by BACK, as */
/*                             seen from the location of the observer. */

/*                 'ANY'       denotes any of the above three types of */
/*                             occultations: 'PARTIAL', 'ANNULAR', or */
/*                             'FULL'. */

/*                             'ANY' should be used to search for */
/*                             times when the body designated by FRONT */
/*                             blocks any part of the body designated */
/*                             by BACK. */

/*                             The option 'ANY' must be used if either */
/*                             the front or back target body is */
/*                             modeled as a point. */

/*              Case and leading or trailing blanks are not */
/*              significant in the string OCCTYP. */


/*     FRONT    is the name of the target body that occults---that is, */
/*              passes in front of---the other. Optionally, you may */
/*              supply the integer NAIF ID code for the body as a */
/*              string. For example both 'MOON' and '301' are */
/*              legitimate strings that designate the Moon. */

/*              Case and leading or trailing blanks are not */
/*              significant in the string FRONT. */



/*     FSHAPE   is a string indicating the geometric model used to */
/*              represent the shape of the front target body. The */
/*              supported options are: */

/*                 'ELLIPSOID' */

/*                     Use a triaxial ellipsoid model with radius */
/*                     values provided via the kernel pool. A kernel */
/*                     variable having a name of the form */

/*                        'BODYnnn_RADII' */

/*                     where nnn represents the NAIF integer code */
/*                     associated with the body, must be present in */
/*                     the kernel pool. This variable must be */
/*                     associated with three numeric values giving the */
/*                     lengths of the ellipsoid's X, Y, and Z */
/*                     semi-axes. */

/*                 'POINT' */

/*                     Treat the body as a single point. When a point */
/*                     target is specified, the occultation type must */
/*                     be set to 'ANY'. */

/*                 'DSK/UNPRIORITIZED[/SURFACES = <surface list>]' */

/*                     Use topographic data provided by DSK files to */
/*                     model the body's shape. These data must be */
/*                     provided by loaded DSK files. */

/*                     The surface list specification is optional. The */
/*                     syntax of the list is */

/*                        <surface 1> [, <surface 2>...] */

/*                     If present, it indicates that data only for the */
/*                     listed surfaces are to be used; however, data */
/*                     need not be available for all surfaces in the */
/*                     list. If absent, loaded DSK data for any surface */
/*                     associated with the target body are used. */

/*                     The surface list may contain surface names or */
/*                     surface ID codes. Names containing blanks must */
/*                     be delimited by double quotes, for example */

/*                        SURFACES = "Mars MEGDR 128 PIXEL/DEG" */

/*                     If multiple surfaces are specified, their names */
/*                     or IDs must be separated by commas. */

/*                     See the $Particulars section below for details */
/*                     concerning use of DSK data. */

/*              The combinations of the shapes of the target bodies */
/*              FRONT and BACK must be one of: */

/*                 One ELLIPSOID, one POINT */
/*                 Two ELLIPSOIDs */
/*                 One DSK, one POINT */

/*              Case and leading or trailing blanks are not */
/*              significant in the string FSHAPE. */


/*     FFRAME   is the name of the body-fixed, body-centered reference */
/*              frame associated with the front target body. Examples */
/*              of such names are 'IAU_SATURN' (for Saturn) and */
/*              'ITRF93' (for the Earth). */

/*              If the front target body is modeled as a point, FFRAME */
/*              should be left blank. */

/*              Case and leading or trailing blanks bracketing a */
/*              non-blank frame name are not significant in the string */
/*              FFRAME. */


/*     BACK     is the name of the target body that is occulted */
/*              by---that is, passes in back of---the other. */
/*              Optionally, you may supply the integer NAIF ID code */
/*              for the body as a string. For example both 'MOON' and */
/*              '301' are legitimate strings that designate the Moon. */

/*              Case and leading or trailing blanks are not */
/*              significant in the string BACK. */


/*     BSHAPE   is the shape specification for the body designated */
/*              by BACK. See the description of FSHAPE above for */
/*              details. */


/*     BFRAME   is the name of the body-fixed, body-centered reference */
/*              frame associated with the ``back'' target body. */
/*              Examples of such names are 'IAU_SATURN' (for Saturn) */
/*              and 'ITRF93' (for the Earth). */

/*              If the back target body is modeled as a point, BFRAME */
/*              should be left blank. */

/*              Case and leading or trailing blanks bracketing a */
/*              non-blank frame name are not significant in the string */
/*              BFRAME. */


/*     OBSRVR   is the name of the body from which the occultation is */
/*              observed. Optionally, you may supply the integer NAIF */
/*              ID code for the body as a string. */

/*              Case and leading or trailing blanks are not */
/*              significant in the string OBSRVR. */


/*     ABCORR   indicates the aberration corrections to be applied to */
/*              the state of the target body to account for one-way */
/*              light time. Stellar aberration corrections are */
/*              ignored if specified, since these corrections don't */
/*              improve the accuracy of the occultation determination. */

/*              See the header of the SPICE routine SPKEZR for a */
/*              detailed description of the aberration correction */
/*              options. For convenience, the options supported by */
/*              this routine are listed below: */

/*                 'NONE'     Apply no correction. */

/*                 'LT'       "Reception" case: correct for */
/*                            one-way light time using a Newtonian */
/*                            formulation. */

/*                 'CN'       "Reception" case: converged */
/*                            Newtonian light time correction. */

/*                 'XLT'      "Transmission" case: correct for */
/*                            one-way light time using a Newtonian */
/*                            formulation. */

/*                 'XCN'      "Transmission" case: converged */
/*                            Newtonian light time correction. */

/*              Case and blanks are not significant in the string */
/*              ABCORR. */

/* $ Detailed_Output */

/*     None. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If name of either target or the observer cannot be translated */
/*         to a NAIF ID code, the error SPICE(IDCODENOTFOUND) is */
/*         signaled. */

/*     2)  If either of the target bodies FRONT or BACK coincides with */
/*         the observer body OBSRVR, or if the targets coincide, */
/*         the error SPICE(BODIESNOTDISTINCT) is signaled. */

/*     3)  If either of the body model specifiers FSHAPE or BSHAPE */
/*         is not recognized, the error SPICE(INVALIDSHAPE) is signaled. */

/*     4)  If both of the body model specifiers FSHAPE and BSHAPE */
/*         specify point targets, the error SPICE(INVALIDSHAPECOMBO) */
/*         is signaled. */

/*     5)  If one of the target shape arguments FSHAPE and BSHAPE */
/*         specifies a DSK model, and the other argument does not */
/*         specify a point target, the error SPICE(INVALIDSHAPECOMBO) */
/*         is signaled. */

/*     6)  If an unrecognized value of OCCTYP is seen, the error */
/*         SPICE(INVALIDOCCTYPE) is signaled. */

/*     7)  If one target body is modeled as a point and OCCTYP is not */
/*         set to 'ANY', the error SPICE(BADTYPESHAPECOMBO) is signaled. */

/*     8)  If a target body-fixed reference frame associated with a */
/*         non-point target is not recognized, the error */
/*         SPICE(INVALIDFRAME) is signaled. */

/*     9)  If a target body-fixed reference frame is not centered at */
/*         the corresponding target body, the error */
/*         SPICE(INVALIDFRAME) is signaled. */

/*     10) If the aberration correction string is invalid, an error */
/*         is signaled by a routine in the call tree of this */
/*         routine. */

/*     11) If either FSHAPE or BSHAPE specifies that the target surface */
/*         is represented by DSK data, and no DSK files are loaded for */
/*         the specified target, an error is signaled by a routine in */
/*         the call tree of this routine. */

/*     12) If either FSHAPE or BSHAPE specifies that the target surface */
/*         is represented by DSK data, but the shape specification is */
/*         invalid, an error is signaled by a routine in the call tree */
/*         of this routine. */

/*     13) If a body RADII vector has other than exactly thee elements, */
/*         an error is signaled by a routine in the call tree of this */
/*         routine. */

/*     14) If a body RADII vector has any element less-than or equal to */
/*         zero, an error is signaled by a routine in the call tree of */
/*         this routine. */

/* $ Files */

/*     See the header of the umbrella routine ZZGFOCU. */

/* $ Particulars */

/*     This entry point initializes the parameters needed by the */
/*     occultation state determination entry point ZZGFOCST. */

/* $ Examples */

/*     See implementation of GFOCCE. */

/* $ Restrictions */

/*     1)  This is a SPICELIB private routine; it should not be called by */
/*         user applications. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     L.S. Elson         (JPL) */
/*     W.L. Taber         (JPL) */
/*     E.D. Wright        (JPL) */

/* $ Version */

/* -    SPICELIB Version 2.1.0, 07-SEP-2021 (EDW) (JDR) */

/*        Body radii accessed from kernel pool using ZZGFTREB. */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 2.0.0, 21-FEB-2017 (NJB) */

/*        Modified FAILED tests. */

/*        20-FEB-2016 (NJB) */

/*        DSK capability was integrated. */

/*        04-MAR-2015 (NJB) */

/*        Initial updates to support surfaces represented by DSKs. */

/* -    SPICELIB Version 1.1.0, 18-MAY-2014 (NJB) */

/*        Bug fix: corrected long error message for the case in which a */
/*        body-fixed reference frame is not centered on the correct */
/*        body. */

/* -    SPICELIB Version 1.0.0, 15-APR-2009 (LSE) (WLT) (NJB) (EDW) */

/* -& */
/* $ Index_Entries */

/*     initialization for occultation state determination */

/* -& */

/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }
    chkin_("ZZGFOCIN", (ftnlen)8);

/*     Find NAIF IDs for FRONT, BACK, and OBSRVR. */

    bods2c_(front, &idfrnt, &found, front_len);
    if (! found) {
	setmsg_("The front target object, '#', is not a recognized name for "
		"an ephemeris object. The cause of this problem may be that y"
		"ou need an updated version of the SPICE toolkit. ", (ftnlen)
		168);
	errch_("#", front, (ftnlen)1, front_len);
	sigerr_("SPICE(IDCODENOTFOUND)", (ftnlen)21);
	chkout_("ZZGFOCIN", (ftnlen)8);
	return 0;
    }
    bods2c_(back, &idback, &found, back_len);
    if (! found) {
	setmsg_("The back target object, '#', is not a recognized name for a"
		"n ephemeris object. The cause of this problem may be that yo"
		"u need an updated version of the SPICE toolkit. ", (ftnlen)
		167);
	errch_("#", back, (ftnlen)1, back_len);
	sigerr_("SPICE(IDCODENOTFOUND)", (ftnlen)21);
	chkout_("ZZGFOCIN", (ftnlen)8);
	return 0;
    }
    bods2c_(obsrvr, &idobs, &found, obsrvr_len);
    if (! found) {
	setmsg_("The observer, '#', is not a recognized name for an ephemeri"
		"s object. The cause of this problem may be that you need an "
		"updated version of the SPICE toolkit. ", (ftnlen)157);
	errch_("#", obsrvr, (ftnlen)1, obsrvr_len);
	sigerr_("SPICE(IDCODENOTFOUND)", (ftnlen)21);
	chkout_("ZZGFOCIN", (ftnlen)8);
	return 0;
    }

/*     Make sure the observer and both targets are distinct. */

    if (idfrnt == idback || idfrnt == idobs || idback == idobs) {
	setmsg_("The observer and both targets must be distinct objects, but"
		" are not: OBSRVR = #; FRONT = #; BACK = #.", (ftnlen)101);
	errch_("#", obsrvr, (ftnlen)1, obsrvr_len);
	errch_("#", front, (ftnlen)1, front_len);
	errch_("#", back, (ftnlen)1, back_len);
	sigerr_("SPICE(BODIESNOTDISTINCT)", (ftnlen)24);
	chkout_("ZZGFOCIN", (ftnlen)8);
	return 0;
    }

/*     Save the objects' names. We'll need these if */
/*     we need to call SINCPT. */

    s_copy(svfnam, front, (ftnlen)36, front_len);
    s_copy(svbnam, back, (ftnlen)36, back_len);
    s_copy(svonam, obsrvr, (ftnlen)36, obsrvr_len);

/*     Store the ID codes, shape specifications, and body-fixed, */
/*     body-centered frame names of the objects involved in this event. */
/*     The shape arguments must be parsed in case they contain */
/*     DSK specifications. */

    svfrnt = idfrnt;
    s_copy(svffrm, fframe, (ftnlen)32, fframe_len);
    svback = idback;
    s_copy(svbfrm, bframe, (ftnlen)32, bframe_len);
    svobs = idobs;

/*     Save the input shape strings. These will be examined later, */
/*     but we need them in their original form for computations */
/*     involving DSK data. In the variable names below, "MTH" */
/*     stands for "method"---the name used in SPICE geometry */
/*     APIs for this type of input string. */

    s_copy(svfmth, fshape, (ftnlen)500, fshape_len);
    s_copy(svbmth, bshape, (ftnlen)500, bshape_len);

/*     Parse the front body shape string. */

    if (eqstr_(fshape, "POINT", fshape_len, (ftnlen)5)) {
	s_copy(svfshp, "POINT", (ftnlen)9, (ftnlen)5);
    } else if (eqstr_(fshape, "ELLIPSOID", fshape_len, (ftnlen)9)) {
	s_copy(svfshp, "ELLIPSOID", (ftnlen)9, (ftnlen)9);
    } else {
	zzprsmet_(&idfrnt, svfmth, &c__100, shpstr, subtyp, &pri, &nsurf, 
		srflst, pntdef, trmtyp, (ftnlen)500, (ftnlen)9, (ftnlen)20, (
		ftnlen)20, (ftnlen)20);
	if (failed_()) {
	    chkout_("ZZGFOCIN", (ftnlen)8);
	    return 0;
	}
	if (eqstr_(shpstr, "DSK", (ftnlen)9, (ftnlen)3)) {
	    s_copy(svfshp, "DSK", (ftnlen)9, (ftnlen)3);
	} else {
	    setmsg_("Front target shape from FSHAPE string was <#>. Valid sh"
		    "apes are ELLIPSOID, POINT, and DSK.", (ftnlen)90);
	    errch_("#", fshape, (ftnlen)1, fshape_len);
	    sigerr_("SPICE(INVALIDSHAPE)", (ftnlen)19);
	    chkout_("ZZGFOCIN", (ftnlen)8);
	    return 0;
	}
    }

/*     Parse the back body shape string. */

    if (eqstr_(bshape, "POINT", bshape_len, (ftnlen)5)) {
	s_copy(svbshp, "POINT", (ftnlen)9, (ftnlen)5);
    } else if (eqstr_(bshape, "ELLIPSOID", bshape_len, (ftnlen)9)) {
	s_copy(svbshp, "ELLIPSOID", (ftnlen)9, (ftnlen)9);
    } else {
	zzprsmet_(&idfrnt, svbmth, &c__100, shpstr, subtyp, &pri, &nsurf, 
		srflst, pntdef, trmtyp, (ftnlen)500, (ftnlen)9, (ftnlen)20, (
		ftnlen)20, (ftnlen)20);
	if (failed_()) {
	    chkout_("ZZGFOCIN", (ftnlen)8);
	    return 0;
	}
	if (eqstr_(shpstr, "DSK", (ftnlen)9, (ftnlen)3)) {
	    s_copy(svbshp, "DSK", (ftnlen)9, (ftnlen)3);
	} else {
	    setmsg_("Back target shape from BSHAPE string was <#>. Valid sha"
		    "pes are ELLIPSOID, POINT, and DSK.", (ftnlen)89);
	    errch_("#", bshape, (ftnlen)1, bshape_len);
	    sigerr_("SPICE(INVALIDSHAPE)", (ftnlen)19);
	    chkout_("ZZGFOCIN", (ftnlen)8);
	    return 0;
	}
    }

/*     Check for invalid shape combinations. */

    if (s_cmp(svfshp, "POINT", (ftnlen)9, (ftnlen)5) == 0 && s_cmp(svbshp, 
	    "POINT", (ftnlen)9, (ftnlen)5) == 0) {
	setmsg_("Both front and back objects have POINT shape specifications"
		"; only one point shape is allowed. The other shape must be E"
		"LLIPSOID or DSK.", (ftnlen)135);
	sigerr_("SPICE(INVALIDSHAPECOMBO)", (ftnlen)24);
	chkout_("ZZGFOCIN", (ftnlen)8);
	return 0;
    } else if (s_cmp(svfshp, "DSK", (ftnlen)9, (ftnlen)3) == 0 && s_cmp(
	    svbshp, "POINT", (ftnlen)9, (ftnlen)5) != 0 || s_cmp(svbshp, 
	    "DSK", (ftnlen)9, (ftnlen)3) == 0 && s_cmp(svfshp, "POINT", (
	    ftnlen)9, (ftnlen)5) != 0) {
	setmsg_("Front target shape from FSHAPE string was <#>; back target "
		"shape from BSHAPE was <#>. When one shape is DSK, the other "
		"must be POINT.", (ftnlen)133);
	errch_("#", svfshp, (ftnlen)1, (ftnlen)9);
	errch_("#", svbshp, (ftnlen)1, (ftnlen)9);
	sigerr_("SPICE(INVALIDSHAPECOMBO)", (ftnlen)24);
	chkout_("ZZGFOCIN", (ftnlen)8);
	return 0;
    }

/*     Save a single upper-case character representing the occultation */
/*     type string. */

    ljust_(occtyp, svtype, occtyp_len, (ftnlen)7);
    ucase_(svtype, svtype, (ftnlen)7, (ftnlen)7);

/*     Check the occultation type. */

    occnum = isrchc_(svtype, &c__4, svtyps, (ftnlen)7, (ftnlen)7);
    if (occnum == 0) {
	setmsg_("The occultation type # is not recognized.  Supported types "
		"are: #, #, #,  #.", (ftnlen)76);
	errch_("#", occtyp, (ftnlen)1, occtyp_len);
	for (i__ = 1; i__ <= 4; ++i__) {
	    errch_("#", svtyps + ((i__1 = i__ - 1) < 4 && 0 <= i__1 ? i__1 : 
		    s_rnge("svtyps", i__1, "zzgfocu_", (ftnlen)1114)) * 7, (
		    ftnlen)1, (ftnlen)7);
	}
	sigerr_("SPICE(INVALIDOCCTYPE)", (ftnlen)21);
	chkout_("ZZGFOCIN", (ftnlen)8);
	return 0;
    }

/*     If we have a point target, the occultation type must */
/*     be 'ANY'. */

    if (s_cmp(svfshp, "POINT", (ftnlen)9, (ftnlen)5) == 0 || s_cmp(svbshp, 
	    "POINT", (ftnlen)9, (ftnlen)5) == 0) {
	if (s_cmp(svtype, "ANY", (ftnlen)7, (ftnlen)3) != 0) {
	    setmsg_("Occultation type # is not allowed when either target bo"
		    "dy is modeled as a point. Set OCCTYP to ANY for use with"
		    " point targets.", (ftnlen)126);
	    errch_("#", occtyp, (ftnlen)1, occtyp_len);
	    sigerr_("SPICE(BADTYPESHAPECOMBO)", (ftnlen)24);
	    chkout_("ZZGFOCIN", (ftnlen)8);
	    return 0;
	}
    }

/*     Check the aberration correction. If SPKEZR can't handle it, */
/*     neither can we. */

    zzvalcor_(abcorr, attblk, abcorr_len);
    if (failed_()) {
	chkout_("ZZGFOCIN", (ftnlen)8);
	return 0;
    }

/*     Create a local aberration correction string without */
/*     a stellar aberration correction specifier. */

    if (attblk[0]) {
	s_copy(svcorr, "NONE", (ftnlen)5, (ftnlen)4);
    } else {

/*        The correction string specified either Newtonian or converged */
/*        light time correction. */

	if (attblk[4]) {
	    s_copy(svcorr, "X", (ftnlen)5, (ftnlen)1);
	} else {
	    s_copy(svcorr, " ", (ftnlen)5, (ftnlen)1);
	}
	if (attblk[3]) {
	    suffix_("CN", &c__0, svcorr, (ftnlen)2, (ftnlen)5);
	} else {
	    suffix_("LT", &c__0, svcorr, (ftnlen)2, (ftnlen)5);
	}
    }

/*     Check the front and back targets' shapes, frames */
/*     and radii. */

    for (i__ = 1; i__ <= 2; ++i__) {
	if (i__ == 1) {
	    s_copy(posnam, "front", (ftnlen)10, (ftnlen)5);
	    s_copy(fixfrm, fframe, (ftnlen)32, fframe_len);
	    trgid = idfrnt;
	    s_copy(shape, svfshp, (ftnlen)9, (ftnlen)9);
	} else {
	    s_copy(posnam, "back", (ftnlen)10, (ftnlen)4);
	    s_copy(fixfrm, bframe, (ftnlen)32, bframe_len);
	    trgid = idback;
	    s_copy(shape, svbshp, (ftnlen)9, (ftnlen)9);
	}
	if (s_cmp(shape, "ELLIPSOID", (ftnlen)9, (ftnlen)9) == 0) {

/*           Fetch and check the radii. */

	    zzgftreb_(&trgid, radii);
	    if (failed_()) {
		chkout_("ZZGFOCIN", (ftnlen)8);
		return 0;
	    }

/*           Checks of radii have been completed. */

	    if (i__ == 1) {
		moved_(radii, &c__3, svfrad);

/*              Select smallest and largest semi-axis lengths of body */
/*              for later tests. */

		minad_(svfrad, &c__3, &svmnfr, &loc);
		maxad_(svfrad, &c__3, &svmxfr, &loc);
	    } else {
		moved_(radii, &c__3, svbrad);
		minad_(svbrad, &c__3, &svmnbr, &loc);
		maxad_(svbrad, &c__3, &svmxbr, &loc);
	    }
	    if (failed_()) {
		chkout_("ZZGFOCIN", (ftnlen)8);
		return 0;
	    }
	}

/*        We've performed radii checks for an ellipsoidal target. */
/*        Minimum and maximum bounding radii are set, if the target */
/*        shape is modeled as an ellipsoid. */


/*        Check body-fixed frame for extended targets. */

	if (s_cmp(shape, "ELLIPSOID", (ftnlen)9, (ftnlen)9) == 0 || s_cmp(
		shape, "DSK", (ftnlen)9, (ftnlen)3) == 0) {

/*           The target is ellipsoidal or is modeled using DSK data; */
/*           there must be a target body-fixed frame associated with */
/*           this body. */

	    if (s_cmp(fixfrm, " ", (ftnlen)32, (ftnlen)1) == 0) {
		setmsg_("The # target shape is represented by an ellipsoid o"
			"r by DSK data, but the associated body-fixed frame n"
			"ame is blank.", (ftnlen)116);
		errch_("#", posnam, (ftnlen)1, (ftnlen)10);
		sigerr_("SPICE(INVALIDFRAME)", (ftnlen)19);
		chkout_("ZZGFOCIN", (ftnlen)8);
		return 0;
	    } else {

/*              Look up the target's body-fixed frame ID code. */

		namfrm_(fixfrm, &ffrmid, (ftnlen)32);
		if (failed_()) {
		    chkout_("ZZGFOCIN", (ftnlen)8);
		    return 0;
		}
		if (ffrmid == 0) {
		    setmsg_("The # target's body-fixed frame name # is not r"
			    "ecognized.", (ftnlen)57);
		    errch_("#", posnam, (ftnlen)1, (ftnlen)10);
		    errch_("#", fixfrm, (ftnlen)1, (ftnlen)32);
		    sigerr_("SPICE(INVALIDFRAME)", (ftnlen)19);
		    chkout_("ZZGFOCIN", (ftnlen)8);
		    return 0;
		}

/*              Obtain the center of the frame and verify it's the */
/*              Ith target. */

		frinfo_(&ffrmid, &center, &frclss, &clssid, &found);
		if (failed_()) {
		    chkout_("ZZGFOCIN", (ftnlen)8);
		    return 0;
		}
		if (! found) {

/*                 Since we mapped the frame name to an ID code, we */
/*                 expect to find the frame info. So control should */
/*                 never reach this point. */

		    setmsg_("Frame ID found for # body-fixed frame # but FRI"
			    "NFO couldn't find frame info.", (ftnlen)76);
		    errch_("#", posnam, (ftnlen)1, (ftnlen)10);
		    errch_("#", fixfrm, (ftnlen)1, (ftnlen)32);
		    sigerr_("SPICE(BUG)", (ftnlen)10);
		    chkout_("ZZGFOCIN", (ftnlen)8);
		    return 0;
		}
		if (center != trgid) {

/*                 The body-fixed frame for the current target */
/*                 isn't actually centered on the body. */

		    setmsg_("Supposed body-fixed frame # for # target # is a"
			    "ctually centered on body #.", (ftnlen)74);
		    errch_("#", fixfrm, (ftnlen)1, (ftnlen)32);
		    errch_("#", posnam, (ftnlen)1, (ftnlen)10);
		    errint_("#", &trgid, (ftnlen)1);
		    errint_("#", &center, (ftnlen)1);
		    sigerr_("SPICE(INVALIDFRAME)", (ftnlen)19);
		    chkout_("ZZGFOCIN", (ftnlen)8);
		    return 0;
		}
	    }
	}

/*        We've performed frame checks for an extended target. */


/*        Obtain radii of inner and outer bounding spheres for */
/*        DSK targets. */

	if (s_cmp(shape, "DSK", (ftnlen)9, (ftnlen)3) == 0) {

/*           Note that TRGID and FFRMID refer to the current */
/*           target (out of two); "FFRMID" means "fixed frame ID." */

	    zzsudski_(&trgid, &nsurf, srflst, &ffrmid);
	    if (failed_()) {
		chkout_("ZZGFOCIN", (ftnlen)8);
		return 0;
	    }
	    if (i__ == 1) {
		zzminrad_(&svmnfr);
		zzmaxrad_(&svmxfr);
	    } else {
		zzminrad_(&svmnbr);
		zzmaxrad_(&svmxbr);
	    }
	}

/*        Initialize bounding radii and body-fixed frame */
/*        names for point targets. */

	if (s_cmp(shape, "POINT", (ftnlen)9, (ftnlen)5) == 0) {

/*           Zero out radius values for this target; set the */
/*           frame to blank. */

	    if (i__ == 1) {
		cleard_(&c__3, svfrad);
		svmnfr = 0.;
		svmxfr = 0.;
		s_copy(svffrm, " ", (ftnlen)32, (ftnlen)1);
	    } else {
		cleard_(&c__3, svbrad);
		svmnbr = 0.;
		svmxbr = 0.;
		s_copy(svbfrm, " ", (ftnlen)32, (ftnlen)1);
	    }
	}

/*        We've performed shape, and if applicable, frame and radii */
/*        checks for the Ith target. Bounding radii have been obtained */
/*        for extended targets. */

    }

/*     We've performed shape, and if applicable, frame and radii */
/*     checks for both targets. */

    chkout_("ZZGFOCIN", (ftnlen)8);
    return 0;
/* $Procedure ZZGFOCST ( GF, "in occultation?"  ) */

L_zzgfocst:
/* $ Abstract */

/*     See if the object is currently occulted. */

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

/*     GEOMETRY */
/*     OCCULTATION */
/*     SEARCH */

/* $ Declarations */

/*     DOUBLE PRECISION      TIME */
/*     LOGICAL               OCSTAT */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     TIME       I   TDB epoch (in seconds past J2000) */
/*     OCSTAT     O   .TRUE. if the object is occulted, .FALSE. */
/*                    otherwise. */

/* $ Detailed_Input */

/*     TIME     is the epoch of interest in TDB seconds past the */
/*              J2000 epoch. */

/* $ Detailed_Output */

/*     OCSTAT   is a logical flag indicating the state of */
/*              occultation. If the configuration initialized by */
/*              ZZGFOCIN is in occultation at the epoch TIME, OCSTAT is */
/*              returned with the value .TRUE. Otherwise it is */
/*              returned with the value .FALSE. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If any SPK lookup fails, an error is signaled by a routine in */
/*         the call tree of this routine. */

/*     2)  If any frame transformation lookup fails, an error is signaled */
/*         by a routine in the call tree of this routine. */

/*     3)  If any occultation computation is done for ellipsoidal */
/*         targets, and if either semi-axis matrix is invalid, an error */
/*         is signaled by a routine in the call tree of this */
/*         routine. */

/*     4)  If any two of the bodies defining the occultation geometry */
/*         intersect, either error, the error SPICE(NOTDISJOINT) is */
/*         signaled by this routine, or, the error is signaled by a */
/*         routine in the call tree of this routine. */

/*     5)  If the body model specifiers FSHAPE and BSHAPE don't specify */
/*         either two ellipsoidal targets or one ellipsoidal target and */
/*         one point target, the error SPICE(INVALIDSHAPECOMBO) */
/*         is signaled. */

/* $ Files */

/*     See the $Files header section of the umbrella routine ZZGFOCU. */

/* $ Particulars */

/*     This routine determines the occultation state of the */
/*     configuration specified by the last call to ZZGFOCIN and the */
/*     input time value. */

/* $ Examples */

/*     See the umbrella routine ZZGFOCU. */

/* $ Restrictions */

/*     1)  This is a SPICELIB private routine; it should not be called by */
/*         user applications. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     L.S. Elson         (JPL) */
/*     W.L. Taber         (JPL) */
/*     E.D. Wright        (JPL) */

/* $ Version */

/* -    SPICELIB Version 2.0.1, 11-SEP-2021 (JDR) */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 2.0.0, 21-FEB-2017 (NJB) */

/*        Modified FAILED tests. */

/*        20-FEB-2016 (NJB) */

/*        DSK capability was integrated. */

/*        04-MAR-2015 (NJB) */

/*        Initial updates to support surfaces represented by DSKs. */

/* -    SPICELIB Version 1.0.0, 30-DEC-2008 (NJB) (LSE) (WLT) (EDW) */

/* -& */
/* $ Index_Entries */

/*     None. */

/* -& */

/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }
    chkin_("ZZGFOCST", (ftnlen)8);

/*     Initialize the state output. */

    *ocstat = FALSE_;

/*     Get the apparent positions of FRONT and BACK as seen from the */
/*     observer. */

    spkezp_(&svfrnt, time, "J2000", svcorr, &svobs, frtpos, &ltfrnt, (ftnlen)
	    5, (ftnlen)5);
    spkezp_(&svback, time, "J2000", svcorr, &svobs, bckpos, &ltback, (ftnlen)
	    5, (ftnlen)5);
    if (failed_()) {
	chkout_("ZZGFOCST", (ftnlen)8);
	return 0;
    }

/*     Handle the cases of one and two extended targets */
/*     separately. */

    if (s_cmp(svbshp, "ELLIPSOID", (ftnlen)9, (ftnlen)9) == 0 && s_cmp(svfshp,
	     "ELLIPSOID", (ftnlen)9, (ftnlen)9) == 0) {

/*        The caller has selected a test for a partial, annular or full */
/*        occultation using ellipsoidal shape models. */

/*        Look up the axes of each target body in the J2000 frame at the */
/*        light time corrected epoch for that body. */

	zzcorepc_(svcorr, time, &ltback, &etbcor, (ftnlen)5);
	pxform_(svbfrm, "J2000", &etbcor, mtemp, (ftnlen)32, (ftnlen)5);
	if (failed_()) {
	    chkout_("ZZGFOCST", (ftnlen)8);
	    return 0;
	}

/*        Scale the columns of MTEMP by the axis lengths of the back */
/*        target. */

	for (i__ = 1; i__ <= 3; ++i__) {
	    vscl_(&svbrad[(i__1 = i__ - 1) < 3 && 0 <= i__1 ? i__1 : s_rnge(
		    "svbrad", i__1, "zzgfocu_", (ftnlen)1626)], &mtemp[(i__2 =
		     i__ * 3 - 3) < 9 && 0 <= i__2 ? i__2 : s_rnge("mtemp", 
		    i__2, "zzgfocu_", (ftnlen)1626)], &bsmaxs[(i__3 = i__ * 3 
		    - 3) < 9 && 0 <= i__3 ? i__3 : s_rnge("bsmaxs", i__3, 
		    "zzgfocu_", (ftnlen)1626)]);
	}
	zzcorepc_(svcorr, time, &ltfrnt, &etfcor, (ftnlen)5);
	pxform_(svffrm, "J2000", &etfcor, mtemp, (ftnlen)32, (ftnlen)5);
	if (failed_()) {
	    chkout_("ZZGFOCST", (ftnlen)8);
	    return 0;
	}

/*        Scale the columns of MTEMP by the axis lengths of the second */
/*        target. */

	for (i__ = 1; i__ <= 3; ++i__) {
	    vscl_(&svfrad[(i__1 = i__ - 1) < 3 && 0 <= i__1 ? i__1 : s_rnge(
		    "svfrad", i__1, "zzgfocu_", (ftnlen)1642)], &mtemp[(i__2 =
		     i__ * 3 - 3) < 9 && 0 <= i__2 ? i__2 : s_rnge("mtemp", 
		    i__2, "zzgfocu_", (ftnlen)1642)], &fsmaxs[(i__3 = i__ * 3 
		    - 3) < 9 && 0 <= i__3 ? i__3 : s_rnge("fsmaxs", i__3, 
		    "zzgfocu_", (ftnlen)1642)]);
	}

/*        Classify the occultation state of BACK by FRONT as seen from */
/*        the observer. */

	occode = zzocced_(svorig, bckpos, bsmaxs, frtpos, fsmaxs);
	if (failed_()) {
	    chkout_("ZZGFOCST", (ftnlen)8);
	    return 0;
	}
	if (occode == 0) {

/*           Neither body occults the other. */

	    *ocstat = FALSE_;
	} else if (s_cmp(svtype, "ANY", (ftnlen)7, (ftnlen)3) == 0 && occode <
		 0) {

/*           The "of" body (target 1) is at least partially occulted by */
/*           the BY object. */

	    *ocstat = TRUE_;
	} else if (s_cmp(svtype, "FULL", (ftnlen)7, (ftnlen)4) == 0 && occode 
		== -3) {

/*           The BACK body is in total occultation. */

	    *ocstat = TRUE_;
	} else if (s_cmp(svtype, "ANNULAR", (ftnlen)7, (ftnlen)7) == 0 && 
		occode == -2) {

/*           The  BACK body is in annular occultation. */

	    *ocstat = TRUE_;
	} else if (s_cmp(svtype, "PARTIAL", (ftnlen)7, (ftnlen)7) == 0 && 
		occode == -1) {

/*           The BACK body is partially occulted. */

	    *ocstat = TRUE_;
	} else {

/*           The occultation state doesn't match the requested state. */

	    *ocstat = FALSE_;
	}
	chkout_("ZZGFOCST", (ftnlen)8);
	return 0;
    } else if (s_cmp(svfshp, "ELLIPSOID", (ftnlen)9, (ftnlen)9) == 0 && s_cmp(
	    svbshp, "POINT", (ftnlen)9, (ftnlen)5) == 0 || s_cmp(svfshp, 
	    "DSK", (ftnlen)9, (ftnlen)3) == 0 && s_cmp(svbshp, "POINT", (
	    ftnlen)9, (ftnlen)5) == 0 || s_cmp(svfshp, "POINT", (ftnlen)9, (
	    ftnlen)5) == 0 && s_cmp(svbshp, "ELLIPSOID", (ftnlen)9, (ftnlen)9)
	     == 0 || s_cmp(svfshp, "POINT", (ftnlen)9, (ftnlen)5) == 0 && 
	    s_cmp(svbshp, "DSK", (ftnlen)9, (ftnlen)3) == 0) {

/*        One of the targets is modeled as a point; the other is */
/*        modeled as an ellipsoid or a DSK shape. */

/*        If the front target is an ellipsoid or a DSK shape and the */
/*        back target is a point, we'll classify the geometry as a */
/*        "point occultation." Otherwise we have a "point transit" case. */
/*        We'll set the logical flag PNTOCC to .TRUE. to indicate a */
/*        point occultation. */

	pntocc = s_cmp(svbshp, "POINT", (ftnlen)9, (ftnlen)5) == 0;

/*        We're going to start out by doing some error checking. */
/*        We're looking for intersections of the participating */
/*        objects: these should never occur. */

/*        Let BDIST, FDIST be the distances from the observer */
/*        to the back and front targets, respectively. */

	bdist = vnorm_(bckpos);
	fdist = vnorm_(frtpos);

/*        Find the vector from BACK to FRONT.  We'll use this later, */
/*        but we want it now in order to make sure that BACK doesn't */
/*        intersect FRONT. */

	vsub_(frtpos, bckpos, bckfrt);
	if (pntocc) {

/*           The front target is an extended shape. */

	    if (fdist <= svmnfr) {

/*              The observer is INSIDE the front target. We */
/*              treat this as an error. */

		setmsg_("Observer is inside front target body.", (ftnlen)37);
		sigerr_("SPICE(NOTDISJOINT)", (ftnlen)18);
		chkout_("ZZGFOCST", (ftnlen)8);
		return 0;
	    } else if (bdist == 0.) {
		setmsg_("Back target coincides with observer.", (ftnlen)36);
		sigerr_("SPICE(NOTDISJOINT)", (ftnlen)18);
		chkout_("ZZGFOCST", (ftnlen)8);
		return 0;
	    } else if (vnorm_(bckfrt) <= svmnfr) {
		setmsg_("BACK target is inside FRONT target.", (ftnlen)35);
		sigerr_("SPICE(NOTDISJOINT)", (ftnlen)18);
		chkout_("ZZGFOCST", (ftnlen)8);
		return 0;
	    }
	} else {

/*           The back target is an extended shape. */

	    if (bdist <= svmnbr) {

/*              The observer is INSIDE the back target. We */
/*              treat this as an error. */

		setmsg_("Observer is inside back target body.", (ftnlen)36);
		sigerr_("SPICE(NOTDISJOINT)", (ftnlen)18);
		chkout_("ZZGFOCST", (ftnlen)8);
		return 0;
	    } else if (fdist == 0.) {
		setmsg_("Front target coincides with observer.", (ftnlen)37);
		sigerr_("SPICE(NOTDISJOINT)", (ftnlen)18);
		chkout_("ZZGFOCST", (ftnlen)8);
		return 0;
	    } else if (vnorm_(bckfrt) <= svmnbr) {
		setmsg_("FRONT target is inside BACK target.", (ftnlen)35);
		sigerr_("SPICE(NOTDISJOINT)", (ftnlen)18);
		chkout_("ZZGFOCST", (ftnlen)8);
		return 0;
	    }
	}

/*        Find angular separation of the target centers as */
/*        seen by the observer. */

	trgsep = vsep_(bckpos, frtpos);

/*        Find angular radius of the outer bounding sphere of the */
/*        extended target, as seen by the observer. */

/*        In computing this angular radius, scale up the bounding */
/*        sphere to compensate for the light time error we've made */
/*        by computing light time to the target's center. The */
/*        correct value to use is light time to the limb point having */
/*        minimum angular separation from the point target. */

/*        Presuming the extended target can move no faster than */
/*        alpha*c (where c represents the speed of light in a vacuum), */
/*        and considering the fact that the light time error cannot */
/*        exceed r/c, where r is the radius of the outer bounding sphere */
/*        of the ellipsoid, we find that the magnitude of the position */
/*        error of the extended target cannot exceed alpha*r. Then the */
/*        correctly positioned target---that is, located at */
/*        the position corresponding to the correct light time */
/*        correction---must be contained in the outer bounding */
/*        sphere we've found, if we scale the sphere up by 1+alpha. */

/*        Perform the test only if the observer is outside the */
/*        outer bounding sphere of the extended target. */

	if (pntocc) {
	    srad = svmxfr * 1.01;
	    tdist = fdist;
	} else {
	    srad = svmxbr * 1.01;
	    tdist = bdist;
	}
	if (srad < tdist) {
	    d__1 = srad / tdist;
	    maxang = dasine_(&d__1, &c_b185);
	    if (failed_()) {
		chkout_("ZZGFOCST", (ftnlen)8);
		return 0;
	    }
	    if (trgsep > maxang) {

/*              No occultation is possible. */

		*ocstat = FALSE_;
		chkout_("ZZGFOCST", (ftnlen)8);
		return 0;
	    }
	}

/*        We'll need the negatives of the observer-target vectors in */
/*        several places later, so compute them now. */

	vminus_(frtpos, frtobs);
	vminus_(bckpos, bckobs);

/*        Now check for an occulted state assuming a spherical extended */
/*        body with radius equal to the minimum semi-axis. Again, */
/*        adjust the sphere for our light time error. */

	if (pntocc) {
	    d__1 = svmnfr * .98999999999999999 / fdist;
	    minang = dasine_(&d__1, &c_b185);
	} else {
	    d__1 = svmnbr * .98999999999999999 / bdist;
	    minang = dasine_(&d__1, &c_b185);
	}
	if (failed_()) {
	    chkout_("ZZGFOCST", (ftnlen)8);
	    return 0;
	}
	if (trgsep < minang) {

/*           The targets must overlap as seen from the observer. */

	    if (pntocc) {

/*              Examine the angle between the vector from FRONT to the */
/*              observer and the vector from FRONT to BACK.  If that */
/*              angle is greater than or equal to the complement of the */
/*              angular radius of FRONT, then FRONT occults BACK. First */
/*              find the position of FRONT and BACK relative to each */
/*              other. */

		vminus_(bckfrt, frtbck);
		t2sep = vsep_(frtobs, frtbck);
		if (t2sep > halfpi_() - minang) {

/*                 There must be an occultation. */

		    *ocstat = TRUE_;
		} else {

/*                 There can't be an occultation: the "back" object */
/*                 is actually in transit across the "front" object. */

		    *ocstat = FALSE_;
		}
	    } else {

/*              We're looking for a point transit condition. */

		t2sep = vsep_(bckobs, bckfrt);
		if (t2sep < halfpi_() - minang) {

/*                 There must be a transit. */

		    *ocstat = TRUE_;
		} else {

/*                 There can't be a transit: the "back" object */
/*                 actually occults the "front" object. */

		    *ocstat = FALSE_;
		}
	    }

/*           OCSTAT has been set. */

	    chkout_("ZZGFOCST", (ftnlen)8);
	    return 0;
	}

/*        If we've reached this point, we have a situation where we */
/*        can't classify the geometry using bounding spheres. Instead, */
/*        we'll see whether the observer-point target vector intersects */
/*        the extended body. */

	if (pntocc) {

/*           The front body is the extended one. */

	    ++ncalls;
	    sincpt_(svfmth, svfnam, time, svffrm, svcorr, svonam, "J2000", 
		    bckpos, spoint, &trgepc, srfvec, &found, (ftnlen)500, (
		    ftnlen)36, (ftnlen)32, (ftnlen)5, (ftnlen)36, (ftnlen)5);
	    if (failed_()) {
		chkout_("ZZGFOCST", (ftnlen)8);
		return 0;
	    }
	    if (found) {

/*              There's an intercept. If the distance from the observer */
/*              to the intercept is less than the distance from the */
/*              observer to the back target, then the back target is */
/*              occulted; otherwise there's a point transit, which is */
/*              not considered an occultation in this case. */

		*ocstat = vnorm_(srfvec) < bdist;
	    } else {

/*              There's no overlap and hence no occultation. */

		*ocstat = FALSE_;
	    }
	} else {

/*           The back body is the extended one. */

	    sincpt_(svbmth, svbnam, time, svbfrm, svcorr, svonam, "J2000", 
		    frtpos, spoint, &trgepc, srfvec, &found, (ftnlen)500, (
		    ftnlen)36, (ftnlen)32, (ftnlen)5, (ftnlen)36, (ftnlen)5);
	    if (failed_()) {
		chkout_("ZZGFOCST", (ftnlen)8);
		return 0;
	    }
	    if (found) {

/*              There's an intercept. If the distance from the observer */
/*              to the intercept is greater than the distance from the */
/*              observer to the front target, then the front target is */
/*              in transit across the back target; otherwise there's a */
/*              point occultation, which is not considered a transit in */
/*              this case. */

		*ocstat = vnorm_(srfvec) > fdist;
	    } else {

/*              There's no overlap and hence no occultation. */

		*ocstat = FALSE_;
	    }
	}
    } else {

/*        Bad combination of shapes. We expect this situation to have */
/*        been caught at initialization time, but make this check for */
/*        safety. */

	setmsg_("The combination of shapes of front and back targets is not "
		"supported: front shape = #; back shape = #.", (ftnlen)102);
	errch_("#", svfshp, (ftnlen)1, (ftnlen)9);
	errch_("#", svbshp, (ftnlen)1, (ftnlen)9);
	sigerr_("SPICE(INVALIDSHAPECOMBO)", (ftnlen)24);
	chkout_("ZZGFOCST", (ftnlen)8);
	return 0;
    }
    chkout_("ZZGFOCST", (ftnlen)8);
    return 0;
} /* zzgfocu_ */

/* Subroutine */ int zzgfocu_(char *occtyp, char *front, char *fshape, char *
	fframe, char *back, char *bshape, char *bframe, char *obsrvr, char *
	abcorr, doublereal *time, logical *ocstat, ftnlen occtyp_len, ftnlen 
	front_len, ftnlen fshape_len, ftnlen fframe_len, ftnlen back_len, 
	ftnlen bshape_len, ftnlen bframe_len, ftnlen obsrvr_len, ftnlen 
	abcorr_len)
{
    return zzgfocu_0_(0, occtyp, front, fshape, fframe, back, bshape, bframe, 
	    obsrvr, abcorr, time, ocstat, occtyp_len, front_len, fshape_len, 
	    fframe_len, back_len, bshape_len, bframe_len, obsrvr_len, 
	    abcorr_len);
    }

/* Subroutine */ int zzgfocin_(char *occtyp, char *front, char *fshape, char *
	fframe, char *back, char *bshape, char *bframe, char *obsrvr, char *
	abcorr, ftnlen occtyp_len, ftnlen front_len, ftnlen fshape_len, 
	ftnlen fframe_len, ftnlen back_len, ftnlen bshape_len, ftnlen 
	bframe_len, ftnlen obsrvr_len, ftnlen abcorr_len)
{
    return zzgfocu_0_(1, occtyp, front, fshape, fframe, back, bshape, bframe, 
	    obsrvr, abcorr, (doublereal *)0, (logical *)0, occtyp_len, 
	    front_len, fshape_len, fframe_len, back_len, bshape_len, 
	    bframe_len, obsrvr_len, abcorr_len);
    }

/* Subroutine */ int zzgfocst_(doublereal *time, logical *ocstat)
{
    return zzgfocu_0_(2, (char *)0, (char *)0, (char *)0, (char *)0, (char *)
	    0, (char *)0, (char *)0, (char *)0, (char *)0, time, ocstat, (
	    ftnint)0, (ftnint)0, (ftnint)0, (ftnint)0, (ftnint)0, (ftnint)0, (
	    ftnint)0, (ftnint)0, (ftnint)0);
    }

