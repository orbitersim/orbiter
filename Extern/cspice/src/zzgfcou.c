/* zzgfcou.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__0 = 0;
static integer c__1 = 1;
static integer c__7 = 7;
static integer c__3 = 3;

/* $Procedure ZZGFCOU ( GF, coordinate utility package ) */
/* Subroutine */ int zzgfcou_0_(int n__, char *vecdef, char *method, char *
	target, doublereal *et, char *ref, char *abcorr, char *obsrvr, char *
	dref, doublereal *dvec, char *crdsys, char *crdnam, logical *decres, 
	doublereal *crdval, logical *crdfnd, U_fp udfunc, ftnlen vecdef_len, 
	ftnlen method_len, ftnlen target_len, ftnlen ref_len, ftnlen 
	abcorr_len, ftnlen obsrvr_len, ftnlen dref_len, ftnlen crdsys_len, 
	ftnlen crdnam_len)
{
    /* Initialized data */

    static char sysnms[32*7] = "RECTANGULAR                     " "LATITUDIN"
	    "AL                     " "RA/DEC                          " "SPH"
	    "ERICAL                       " "CYLINDRICAL                     " 
	    "GEODETIC                        " "PLANETOGRAPHIC              "
	    "    ";
    static char crdnms[32*3*7] = "X                               " "Y      "
	    "                         " "Z                               " 
	    "RADIUS                          " "LONGITUDE                   "
	    "    " "LATITUDE                        " "RANGE                 "
	    "          " "RIGHT ASCENSION                 " "DECLINATION     "
	    "                " "RADIUS                          " "COLATITUDE"
	    "                      " "LONGITUDE                       " "RADI"
	    "US                          " "LONGITUDE                       " 
	    "Z                               " "LONGITUDE                   "
	    "    " "LATITUDE                        " "ALTITUDE              "
	    "          " "LONGITUDE                       " "LATITUDE        "
	    "                " "ALTITUDE                        ";
    static doublereal y[3] = { 0.,1.,0. };

    /* System generated locals */
    integer i__1;

    /* Builtin functions */
    integer s_cmp(char *, char *, ftnlen, ftnlen), s_rnge(char *, integer, 
	    char *, integer);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    double cos(doublereal), sin(doublereal);

    /* Local variables */
    static doublereal svre;
    extern /* Subroutine */ int zzgftreb_(integer *, doublereal *), zzgfcost_(
	    char *, char *, integer *, doublereal *, char *, char *, integer *
	    , char *, integer *, doublereal *, doublereal *, doublereal *, 
	    logical *, ftnlen, ftnlen, ftnlen, ftnlen, ftnlen), zzvalcor_(
	    char *, logical *, ftnlen), zzgfcprx_(doublereal *, char *, 
	    doublereal *, doublereal *, integer *, integer *, ftnlen), etcal_(
	    doublereal *, char *, ftnlen), chkin_(char *, ftnlen), ucase_(
	    char *, char *, ftnlen, ftnlen), errch_(char *, char *, ftnlen, 
	    ftnlen);
    integer class__;
    extern /* Subroutine */ int moved_(doublereal *, integer *, doublereal *);
    logical found;
    doublereal value;
    extern /* Subroutine */ int errdp_(char *, doublereal *, ftnlen);
    doublereal state[6];
    static char svcrd[32], svref[32];
    static integer svobs;
    extern /* Subroutine */ int ljust_(char *, char *, ftnlen, ftnlen);
    extern logical vzero_(doublereal *);
    extern /* Subroutine */ int bods2c_(char *, integer *, logical *, ftnlen),
	     bodc2s_(integer *, char *, ftnlen);
    extern logical failed_(void);
    extern doublereal pi_(void);
    extern /* Subroutine */ int cleard_(integer *, doublereal *);
    extern logical bodfnd_(integer *, char *, ftnlen);
    extern /* Subroutine */ int recrad_(doublereal *, doublereal *, 
	    doublereal *, doublereal *);
    integer frcode;
    extern integer isrchc_(char *, integer *, char *, ftnlen, ftnlen);
    extern logical return_(void);
    static char svcorr[20], svcsys[32], svdref[32], svmeth[200], svrcnm[36], 
	    svvdef[32];
    char timstr[40];
    doublereal coords[3];
    static doublereal svdvec[3], svradi[3];
    integer cdsign[3], clssid;
    static integer svcidx, svdctr, svrctr, svsens, svtarg;
    integer sysidx;
    logical attblk[6];
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), setmsg_(char *, ftnlen), cmprss_(char *, integer *, char 
	    *, char *, ftnlen, ftnlen, ftnlen);
    doublereal alt, lat;
    extern /* Subroutine */ int namfrm_(char *, integer *, ftnlen), frinfo_(
	    integer *, integer *, integer *, integer *, logical *), errint_(
	    char *, integer *, ftnlen), recpgr_(char *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, ftnlen);
    doublereal lon;
    extern /* Subroutine */ int reclat_(doublereal *, doublereal *, 
	    doublereal *, doublereal *), recsph_(doublereal *, doublereal *, 
	    doublereal *, doublereal *), reccyl_(doublereal *, doublereal *, 
	    doublereal *, doublereal *), recgeo_(doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *);
    static doublereal svf;
    extern /* Subroutine */ int zzgfcoq_(char *, char *, integer *, 
	    doublereal *, char *, char *, integer *, char *, doublereal *, 
	    char *, integer *, doublereal *, doublereal *, char *, doublereal 
	    *, logical *, ftnlen, ftnlen, ftnlen, ftnlen, ftnlen, ftnlen, 
	    ftnlen);

/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines. Users should not call this routine directly due to the */
/*     volatile nature of this routine. */

/*     This is the umbrella routine for the entry points needed by */
/*     GFEVNT (or other GF routines) in order to solve for time windows */
/*     on which specified mathematical conditions involving coordinates */
/*     are satisfied. */

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
/*     GF */
/*     NAIF_IDS */
/*     PCK */
/*     SPK */
/*     TIME */

/* $ Keywords */

/*     GEOMETRY */
/*     PRIVATE */
/*     SEARCH */

/* $ Declarations */
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

/*     SPICE private include file intended solely for the support of */
/*     SPICE routines. Users should not include this routine in their */
/*     source code due to the volatile nature of this file. */

/*     This file contains private, global parameter declarations */
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
/*     E.D. Wright       (JPL) */

/* $ Literature_References */

/*     None. */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 17-FEB-2009 (NJB) (EDW) */

/* -& */

/*     The set of supported coordinate systems */

/*        System          Coordinates */
/*        ----------      ----------- */
/*        Rectangular     X, Y, Z */
/*        Latitudinal     Radius, Longitude, Latitude */
/*        Spherical       Radius, Colatitude, Longitude */
/*        RA/Dec          Range, Right Ascension, Declination */
/*        Cylindrical     Radius, Longitude, Z */
/*        Geodetic        Longitude, Latitude, Altitude */
/*        Planetographic  Longitude, Latitude, Altitude */

/*     Below we declare parameters for naming coordinate systems. */
/*     User inputs naming coordinate systems must match these */
/*     when compared using EQSTR. That is, user inputs must */
/*     match after being left justified, converted to upper case, */
/*     and having all embedded blanks removed. */


/*     Below we declare names for coordinates. Again, user */
/*     inputs naming coordinates must match these when */
/*     compared using EQSTR. */


/*     Note that the RA parameter value below matches */

/*        'RIGHT ASCENSION' */

/*     when extra blanks are compressed out of the above value. */


/*     Parameters specifying types of vector definitions */
/*     used for GF coordinate searches: */

/*     All string parameter values are left justified, upper */
/*     case, with extra blanks compressed out. */

/*     POSDEF indicates the vector is defined by the */
/*     position of a target relative to an observer. */


/*     SOBDEF indicates the vector points from the center */
/*     of a target body to the sub-observer point on */
/*     that body, for a given observer and target. */


/*     SOBDEF indicates the vector points from the center */
/*     of a target body to the surface intercept point on */
/*     that body, for a given observer, ray, and target. */


/*     Number of workspace windows used by ZZGFREL: */


/*     Number of additional workspace windows used by ZZGFLONG: */


/*     Index of "existence window" used by ZZGFCSLV: */


/*     Progress report parameters: */

/*     MXBEGM, */
/*     MXENDM    are, respectively, the maximum lengths of the progress */
/*               report message prefix and suffix. */

/*     Note: the sum of these lengths, plus the length of the */
/*     "percent complete" substring, should not be long enough */
/*     to cause wrap-around on any platform's terminal window. */


/*     Total progress report message length upper bound: */


/*     End of file zzgf.inc. */

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

/* $ Brief_I/O */

/*     VARIABLE  I/O  ENTRY POINTS */
/*     --------  ---  -------------------------------------------------- */
/*     VECDEF     I   COIN */
/*     METHOD     I   COIN */
/*     TARGET     I   COIN */
/*     ET         I   COIN, CODC, COG, COCD, COCG, COSD, COSG, COEX */
/*     REF        I   COIN */
/*     ABCORR     I   COIN */
/*     OBSRVR     I   COIN */
/*     DREF       I   COIN */
/*     DVEC       I   COIN */
/*     CRDSYS     I   COIN */
/*     CRDNAM     I   COIN */
/*     DECRES     O   CODC, COCD, COSD */
/*     CRDVAL     O   COG,  COCG, COSG */
/*     CRDFND     O   COEX */

/* $ Detailed_Input */

/*     See individual entry points. */

/* $ Detailed_Output */

/*     See individual entry points. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     See the entry points for descriptions of exceptions specific */
/*     to those routines. */

/*     1)  If this routine is called directly, the error */
/*         SPICE(BOGUSENTRY) is signaled. */

/* $ Files */

/*     This suite of routines doesn't directly participate in SPICE */
/*     kernel loading or unloading. However, a variety of SPICE kernels */
/*     must be loaded in order for these utilities to work: */

/*     -  Since all coordinate computations supported by this routine */
/*        depend on observer-target vectors, at a minimum, SPK files */
/*        providing ephemeris data enabling computation of these */
/*        vectors are required. */

/*     -  If non-inertial reference frames are used, then PCK */
/*        files, frame kernels, C-kernels, and SCLK kernels may be */
/*        needed. */

/*     -  If the coordinate of interest is defined in terms of a target */
/*        surface point, then (currently) a PCK providing radii for a */
/*        triaxial shape model must be loaded. */

/*     -  If geodetic coordinates are used, then a PCK providing radii */
/*        for a triaxial shape model must be loaded. */

/*     See the $Files section of GFEVNT's header for further information. */

/* $ Particulars */

/*     This routine serves as the umbrella routine for entry points */
/*     needed by GFEVNT or other GF routines in order to solve for time */
/*     windows on which specified mathematical conditions involving */
/*     coordinates are satisfied. For brevity, we may refer to such a */
/*     time window as the "solution window" or "coordinate solution */
/*     window." */

/*     The entry points of this package are */

/*        ZZGFCOIN      an initialization routine that must be called */
/*                      to define the coordinate of interest. This */
/*                      routine must be called at least once before */
/*                      any of the other entry points are called, but */
/*                      it may be called as many times as necessary */
/*                      to initialize new computations. */

/*                      Below, the phrase "the coordinate" refers */
/*                      to the coordinate established by the latest */
/*                      call to ZZGFCOIN. For example, the coordinate */
/*                      may be the "geodetic latitude of the sub-moon */
/*                      point on the earth, relative to the IAU_EARTH */
/*                      reference frame, computed using light time and */
/*                      stellar aberration corrections." */

/*        ZZGFCODC      indicates whether the coordinate is strictly */
/*                      decreasing as a function of time, at a specified */
/*                      time. */

/*        ZZGFCOG       returns the coordinate value at a specified */
/*                      time. */

/*        ZZGFCOEX      indicates whether the coordinate is computable */
/*                      at a specified time. ZZGFCOEX is used to */
/*                      determine the time window over which a specified */
/*                      target surface intercept and its time derivative */
/*                      is computable. */


/*        The following entry points support solution window */
/*        computations for conditions involving longitude or right */
/*        ascension. They may have applications for relations involving */
/*        other angular coordinates. */

/*        ZZGFCOCD      indicates whether the cosine of the coordinate is */
/*                      strictly decreasing as a function of time, at a */
/*                      specified time. */

/*        ZZGFCOSD      indicates whether the sine of the coordinate is */
/*                      strictly decreasing as a function of time, at a */
/*                      specified time. */

/*        ZZGFCOCG      returns the cosine of the coordinate at a */
/*                      specified time. */

/*        ZZGFCOSG      returns the sine of the coordinate at a */
/*                      specified time. */

/* $ Examples */

/*     See the code of GFEVNT and ZZGFLONG for usage examples. */

/* $ Restrictions */

/*     1)  The interface and functionality of this set of routines may */
/*         change without notice. These routines should be called only */
/*         by SPICELIB routines. */

/*     2)  ZZGFCOIN must be called prior to use of any of the other */
/*         entry points. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     E.D. Wright        (JPL) */

/* $ Version */

/* -    SPICELIB Version 3.1.0, 17-OCT-2021 (EDW) (JDR) */

/*        ZZGFCOIN, body radii accessed from kernel pool */
/*        using ZZGFTREB. */

/*        Edited the header of the umbrella routine and all its entry */
/*        points to comply with NAIF standard. */

/* -    SPICELIB Version 3.0.0, 05-APR-2011 (EDW) */

/*        Code edits to implement use of ZZGFRELX. */
/*        These edits include removal of unneeded routines: */

/*           ZZGFCOUR */
/*           ZZGFCOLT */
/*           ZZGFCOCL */
/*           ZZGFCOSL */

/*        and corresponding unused variables. */

/*        Corresponding update to header entries. */

/* -    SPICELIB Version 2.0.0, 12-MAY-2009 (NJB) */

/*        Upgraded to support targets and observers having */
/*        no names associated with their ID codes. */

/* -    SPICELIB Version 1.0.0, 05-MAR-2009 (NJB) */

/* -& */
/* $ Index_Entries */

/*     umbrella routine for finding coordinate events */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     Length of an aberration correction name string. */


/*     Length of a reference frame name. */


/*     Length of a body name. */


/*     Length of a coordinate system name. */


/*     Length of a vector definition name. */


/*     Number of recognized coordinate systems. */


/*     Maximum length of a coordinate name. */


/*     Maximum length of computation method name. */


/*     Time string length. */


/*     Local Variables */


/*     Saved Variables */


/*     Initial values */


/*     Names of supported coordinate systems. */

/*     The Ith coordinate system in the array SYSNMS has coordinates */
/*     in the Ith row of the array CRDNMS. This association must be */
/*     preserved when this routine is updated. */

    /* Parameter adjustments */
    if (dvec) {
	}

    /* Function Body */
    switch(n__) {
	case 1: goto L_zzgfcoin;
	case 2: goto L_zzgfcog;
	case 3: goto L_zzgfcodc;
	case 4: goto L_zzgfcoex;
	case 5: goto L_zzgfcocg;
	case 6: goto L_zzgfcosg;
	case 7: goto L_zzgfcocd;
	case 8: goto L_zzgfcosd;
	}


/*     Names of coordinate triples for the supported coordinate */
/*     systems. */

/*     The order of the coordinate names in the Ith row of this array */
/*     matches the order of the outputs of the corresponding */
/*     SPICELIB routine REC*, which maps a Cartesian vector to */
/*     the Ith coordinate system in the array SYSNMS. Again, this */
/*     order must be preserved. */


/*     This routine should never be called. */

    if (return_()) {
	return 0;
    }
    chkin_("ZZGFCOU", (ftnlen)7);
    sigerr_("SPICE(BOGUSENTRY)", (ftnlen)17);
    chkout_("ZZGFCOU", (ftnlen)7);
    return 0;
/* $Procedure ZZGFCOIN ( GF, coordinate search initialization ) */

L_zzgfcoin:
/* $ Abstract */

/*     Initialize a coordinate search. */

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
/*     SPK */
/*     TIME */
/*     NAIF_IDS */
/*     FRAMES */

/* $ Keywords */

/*     GEOMETRY */
/*     PRIVATE */
/*     ROOT */

/* $ Declarations */

/*     CHARACTER*(*)         VECDEF */
/*     CHARACTER*(*)         METHOD */
/*     CHARACTER*(*)         TARGET */
/*     CHARACTER*(*)         REF */
/*     CHARACTER*(*)         ABCORR */
/*     CHARACTER*(*)         OBSRVR */
/*     CHARACTER*(*)         DREF */
/*     DOUBLE PRECISION      DVEC */
/*     CHARACTER*(*)         CRDSYS */
/*     CHARACTER*(*)         CRDNAM */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     VECDEF     I   Vector definition. */
/*     METHOD     I   Computation method. */
/*     TARGET     I   Target name. */
/*     REF        I   Reference frame name. */
/*     ABCORR     I   Aberration correction. */
/*     OBSRVR     I   Observer name. */
/*     DREF       I   Ray's direction vector frame. */
/*     DVEC       I   Ray's direction vector. */
/*     CRDSYS     I   Coordinate system name. */
/*     CRDNAM     I   Coordinate name. */

/* $ Detailed_Input */

/*     VECDEF   every coordinate computed by this routine is a */
/*              function of an underlying vector. VECDEF is a short */
/*              string describing the means by which the vector of */
/*              interest is defined. Only parameters from the Fortran */
/*              INCLUDE file zzgf.inc should be used. Parameter names */
/*              and meanings are: */

/*                 POSDEF               Vector is position of */
/*                                      target relative to observer. */

/*                 SOBDEF               Vector is sub-observer */
/*                                      point on target body. Vector */
/*                                      points from target body */
/*                                      center to sub-observer point. */
/*                                      The target must be an extended */
/*                                      body modeled as a triaxial */
/*                                      ellipsoid. */

/*                 SINDEF               Vector is ray-surface intercept */
/*                                      point on target body. Vector */
/*                                      points from target body */
/*                                      center to sub-observer point. */
/*                                      The target must be an extended */
/*                                      body modeled as a triaxial */
/*                                      ellipsoid. */

/*              Case, leading and trailing blanks ARE significant */
/*              in the string VECDEF. */


/*     METHOD   is a string specifying the computational method */
/*              applicable to the vector of interest. When VECDEF */
/*              is the parameter */

/*                 SOBDEF */

/*              METHOD should be set to one of the values accepted */
/*              by the SPICELIB routine SUBPNT. */

/*              When VECDEF is the parameter */

/*                 SINDEF */

/*              METHOD should be set to one of the values accepted */
/*              by the SPICELIB routine SINCPT. */

/*              METHOD is ignored if VECDEF is set to */

/*                 POSDEF */

/*              Case, leading and trailing blanks are not significant */
/*              in the string METHOD. */


/*     TARGET   is the name of the target object. */


/*     REF      is the name of the reference frame relative to which */
/*              the vector of interest is specified. The specified */
/*              condition applies to the specified coordinate of */
/*              of this vector in frame REF. */

/*              When geodetic coordinates are used, the reference */
/*              ellipsoid is assumed to be that associated with */
/*              the central body of the frame designated by REF. */
/*              In this case, the central body of the frame must */
/*              be an extended body. */

/*              Case, leading and trailing blanks are not significant */
/*              in the string REF. */


/*     ABCORR   indicates the aberration corrections to be applied to */
/*              the state of the target body to account for one-way */
/*              light time and stellar aberration. The orientation */
/*              of the target body will also be corrected for one-way */
/*              light time when light time corrections are requested. */

/*              Supported aberration correction options for */
/*              observation (case where radiation is received by */
/*              observer at ET) are: */

/*                'NONE'          No correction. */
/*                'LT'            Light time only. */
/*                'LT+S'          Light time and stellar aberration. */
/*                'CN'            Converged Newtonian (CN) light time. */
/*                'CN+S'          CN light time and stellar aberration. */

/*              Supported aberration correction options for */
/*              transmission (case where radiation is emitted from */
/*              observer at ET) are: */

/*                'XLT'           Light time only. */
/*                'XLT+S'         Light time and stellar aberration. */
/*                'XCN'           Converged Newtonian (CN) light time. */
/*                'XCN+S'         CN light time and stellar aberration. */

/*              For detailed information, see the geometry finder */
/*              required reading, gf.req. Also see the header of */
/*              SPKEZR, which contains a detailed discussion of */
/*              aberration corrections. */

/*              Case, leading and trailing blanks are not significant */
/*              in the string ABCORR. */


/*     OBSRVR   is the name of the observer. */


/*     DREF     is the name of the reference frame relative to which a */
/*              ray's direction vector is expressed. This may be any */
/*              frame supported by the SPICE system, including */
/*              built-in frames (documented in the Frames Required */
/*              Reading) and frames defined by a loaded frame kernel */
/*              (FK). The string DREF is case-insensitive, and leading */
/*              and trailing blanks in FIXREF are not significant. */

/*              When DREF designates a non-inertial frame, the */
/*              orientation of the frame is evaluated at an epoch */
/*              dependent on the frame's center and, if the center is */
/*              not the observer, on the selected aberration */
/*              correction. See the description of the direction */
/*              vector DVEC for details. */


/*     DVEC     ray direction vector emanating from the observer. The */
/*              intercept with the target body's surface of the ray */
/*              defined by the observer and DVEC is sought. */

/*              DVEC is specified relative to the reference frame */
/*              designated by DREF. */

/*              Non-inertial reference frames are treated as follows: */
/*              if the center of the frame is at the observer's */
/*              location, the frame is evaluated at ET. If the frame's */
/*              center is located elsewhere, then letting LTCENT be */
/*              the one-way light time between the observer and the */
/*              central body associated with the frame, the */
/*              orientation of the frame is evaluated at ET-LTCENT, */
/*              ET+LTCENT, or ET depending on whether the requested */
/*              aberration correction is, respectively, for received */
/*              radiation, transmitted radiation, or is omitted. */
/*              LTCENT is computed using the method indicated by */
/*              ABCORR. */


/*     CRDSYS   is the name of the coordinate system to which the */
/*              coordinate of interest belongs. Allowed values are */
/*              those defined in the GF Fortran INCLUDE file */

/*                 zzgf.inc. */

/*              Note that when geodetic or planetographic coordinates */
/*              are used, the reference ellipsoid is that associated */
/*              with the central body of the reference frame */
/*              designated by REF. The central body must be an */
/*              extended body in this case. */

/*              Case, leading and trailing blanks are not significant */
/*              in the string CRDSYS. */


/*     CRDNAM   is the name of the coordinate of interest: this is */
/*              the coordinate to which the specified condition */
/*              applies. The set of coordinate names is a function of */
/*              the coordinate system. Allowed values are those */
/*              defined in the GF Fortran INCLUDE file */

/*                 zzgf.inc. */

/*              Case, leading and trailing blanks are not significant */
/*              in the string CRDNAM. */

/* $ Detailed_Output */

/*     None. This routine operates by side effects. See $Particulars */
/*     for a description of the action of this routine. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If either the observer or target names cannot be mapped */
/*         to ID codes, the error SPICE(IDCODENOTFOUND) is signaled. */

/*     2)  If the observer and target have the same ID codes, the */
/*         error SPICE(BODIESNOTDISTINCT) is signaled. */

/*     3)  If the vector definition VECDEF is not recognized, */
/*         the error SPICE(NOTSUPPORTED) is signaled. */

/*     4)  If the computation method METHOD is not recognized, */
/*         the error SPICE(NOTSUPPORTED) is signaled. */

/*     5)  If the aberration correction ABCORR is not recognized, */
/*         the error SPICE(NOTSUPPORTED) is signaled. */

/*     6)  If the coordinate system name CRDSYS is not recognized, */
/*         the error SPICE(NOTSUPPORTED) is signaled. */

/*     7)  If the coordinate name CRDNAM is not recognized, */
/*         the error SPICE(NOTSUPPORTED) is signaled. */

/*     8)  If the frame REF is not recognized by the frames subsystem, */
/*         the error SPICE(NOFRAME) is signaled. */

/*     9)  If VECDEF calls for a computation involving a target surface */
/*         intercept point and the name and ID code of the frame DREF */
/*         associated with the target body are not available from the */
/*         frame subsystem, the error SPICE(NOFRAME) is signaled. */

/*     10) If VECDEF calls for a computation involving a target surface */
/*         intercept point and the direction vector DVEC is the zero */
/*         vector, the error SPICE(ZEROVECTOR) is signaled. */

/*     11) If VECDEF calls for a computation involving a target surface */
/*         point and the radii defining the reference ellipsoid */
/*         associated with the target body are not available in the */
/*         kernel pool, an error is signaled by a routine in the */
/*         call tree of this routine. */

/*     12) If VECDEF calls for a computation involving a target surface */
/*         point and the frame REF is not centered on the target body, */
/*         the error SPICE(INVALIDFRAME) is signaled. */

/*     13) If geodetic or planetographic coordinates are used and the */
/*         radii defining the reference ellipsoid associated with the */
/*         center of the frame REF are not available in the kernel pool, */
/*         an error is signaled by a routine in the call tree of this */
/*         routine. */

/*     14) If geodetic or planetographic coordinates are used and the */
/*         first equatorial radius of the reference ellipsoid associated */
/*         with the center of the frame REF is zero, the error */
/*         SPICE(DIVIDEBYZERO) is signaled. */

/*     15) If geodetic or planetographic coordinates are used and the */
/*         equatorial radii of the reference ellipsoid associated */
/*         with the center of the frame REF are unequal, the error */
/*         SPICE(NOTSUPPORTED) is signaled. */

/*     16) If geodetic or planetographic coordinates are used and the */
/*         reference ellipsoid associated with the center of the frame */
/*         REF is degenerate (one or more radii are non-positive), */
/*         the error SPICE(DEGENERATECASE) is signaled. */

/*     17) If a body RADII vector has other than exactly thee elements, */
/*         an error is signaled by a routine in the call tree of this */
/*         routine. */

/*     18) If a body RADII vector has any element less-than or equal to */
/*         zero, an error is signaled by a routine in the call tree of */
/*         this routine. */

/* $ Files */

/*     See the discussion in the $Files section of the header of the */
/*     umbrella subroutine ZZGFCOU. */

/* $ Particulars */

/*     This routine's main purpose is to support GFEVNT. Many of */
/*     the geometric quantities supported by GFEVNT are simply */
/*     coordinates of a vector in some reference frame. */

/*     The entry points that deal with sines and cosines of coordinates */
/*     support solving problems involving constraints on */
/*     longitude or right ascension. See ZZGFLONG for usage examples. */

/* $ Examples */

/*     See GFEVNT and ZZGFLONG. */

/* $ Restrictions */

/*     1)  The interface and functionality of this set of routines may */
/*         change without notice. These routines should be called only */
/*         by SPICELIB routines. */

/*     2)  ZZGFCOIN must be called prior to use of any of the other */
/*         entry points. */

/*     3)  This routine has the following couplings with other */
/*         SPICE routines: */

/*            - The set of allowed aberration corrections must */
/*              be kept in sync with the set supported by the */
/*              SPK API routines. */

/*            - The set of vector definitions must be kept in */
/*              sync with the set supported by GFEVNT. */

/*            - The set of supported coordinate systems must be kept in */
/*              sync with the set supported by zzgf.inc. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     E.D. Wright        (JPL) */

/* $ Version */

/* -    SPICELIB Version 3.1.0, 11-SEP-2021 (EDW) (JDR) */

/*        Body radii accessed from kernel pool using ZZGFTREB. */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 3.0.0, 05-APR-2011 (EDW) */

/*        REFVAL removed from routine argument list due to use */
/*        of ZZGFRELX to calculate the events. */

/* -    SPICELIB Version 2.0.0, 12-MAY-2009 (NJB) */

/*        Upgraded to support targets and observers having */
/*        no names associated with their ID codes. */

/* -    SPICELIB Version 1.0.0, 05-MAR-2009 (NJB) */

/* -& */
/* $ Index_Entries */

/*     coordinate initialization routine */

/* -& */
    if (return_()) {
	return 0;
    }
    chkin_("ZZGFCOIN", (ftnlen)8);

/*     Find NAIF IDs for TARGET and OBSRVR. */

    bods2c_(target, &svtarg, &found, target_len);
    if (! found) {
	setmsg_("The target object, '#', is not a recognized name for an eph"
		"emeris object. The cause of this problem may be that you nee"
		"d an updated version of the SPICE Toolkit. ", (ftnlen)162);
	errch_("#", target, (ftnlen)1, target_len);
	sigerr_("SPICE(IDCODENOTFOUND)", (ftnlen)21);
	chkout_("ZZGFCOIN", (ftnlen)8);
	return 0;
    }
    bods2c_(obsrvr, &svobs, &found, obsrvr_len);
    if (! found) {
	setmsg_("The observer, '#', is not a recognized name for an ephemeri"
		"s object. The cause of this problem may be that you need an "
		"updated version of the SPICE toolkit. ", (ftnlen)157);
	errch_("#", obsrvr, (ftnlen)1, obsrvr_len);
	sigerr_("SPICE(IDCODENOTFOUND)", (ftnlen)21);
	chkout_("ZZGFCOIN", (ftnlen)8);
	return 0;
    }

/*     Make sure the observer and target are distinct. */

    if (svtarg == svobs) {
	setmsg_("The observer and target must be distinct objects, but are n"
		"ot: OBSRVR = #; TARGET = #.", (ftnlen)86);
	errch_("#", obsrvr, (ftnlen)1, obsrvr_len);
	errch_("#", target, (ftnlen)1, target_len);
	sigerr_("SPICE(BODIESNOTDISTINCT)", (ftnlen)24);
	chkout_("ZZGFCOIN", (ftnlen)8);
	return 0;
    }

/*     Squeeze all blanks out of the aberration correction */
/*     string; ensure the string is in upper case. */

    cmprss_(" ", &c__0, abcorr, svcorr, (ftnlen)1, abcorr_len, (ftnlen)20);
    ucase_(svcorr, svcorr, (ftnlen)20, (ftnlen)20);

/*     Check the aberration correction. If SPKEZR can't handle it, */
/*     neither can we. */

    zzvalcor_(svcorr, attblk, (ftnlen)20);
    if (failed_()) {
	chkout_("ZZGFCOIN", (ftnlen)8);
	return 0;
    }

/*     Store a compressed, upper case, left-justified copy of VECDEF. */

    ljust_(vecdef, svvdef, vecdef_len, (ftnlen)32);
    cmprss_(" ", &c__1, svvdef, svvdef, (ftnlen)1, (ftnlen)32, (ftnlen)32);
    ucase_(svvdef, svvdef, (ftnlen)32, (ftnlen)32);

/*     Check SVVDEF. */

    if (s_cmp(svvdef, "POSITION", (ftnlen)32, (ftnlen)8) != 0 && s_cmp(svvdef,
	     "SUB-OBSERVER POINT", (ftnlen)32, (ftnlen)18) != 0 && s_cmp(
	    svvdef, "SURFACE INTERCEPT POINT", (ftnlen)32, (ftnlen)23) != 0) {

/*        We don't recognize this vector definition. */

	setmsg_("The vector definition # is not supported.", (ftnlen)41);
	errch_("#", vecdef, (ftnlen)1, vecdef_len);
	sigerr_("SPICE(NOTSUPPORTED)", (ftnlen)19);
	chkout_("ZZGFCOIN", (ftnlen)8);
	return 0;
    }

/*     Store a compressed, upper case, left-justified copy of CRDSYS. */

    ljust_(crdsys, svcsys, crdsys_len, (ftnlen)32);
    cmprss_(" ", &c__0, svcsys, svcsys, (ftnlen)1, (ftnlen)32, (ftnlen)32);
    ucase_(svcsys, svcsys, (ftnlen)32, (ftnlen)32);
    sysidx = isrchc_(svcsys, &c__7, sysnms, (ftnlen)32, (ftnlen)32);
    if (sysidx == 0) {

/*        We don't recognize this system name. */

	setmsg_("The coordinate system # is not supported.", (ftnlen)41);
	errch_("#", crdsys, (ftnlen)1, crdsys_len);
	sigerr_("SPICE(NOTSUPPORTED)", (ftnlen)19);
	chkout_("ZZGFCOIN", (ftnlen)8);
	return 0;
    }

/*     Store a compressed, upper case, left-justified copy of CRDNAM. */

    ljust_(crdnam, svcrd, crdnam_len, (ftnlen)32);
    cmprss_(" ", &c__1, svcrd, svcrd, (ftnlen)1, (ftnlen)32, (ftnlen)32);
    ucase_(svcrd, svcrd, (ftnlen)32, (ftnlen)32);

/*     Find and save the index of the coordinate name in the list of */
/*     supported names. */

    svcidx = isrchc_(svcrd, &c__3, crdnms + (((i__1 = sysidx * 3 - 3) < 21 && 
	    0 <= i__1 ? i__1 : s_rnge("crdnms", i__1, "zzgfcou_", (ftnlen)
	    1011)) << 5), (ftnlen)32, (ftnlen)32);
    if (svcidx == 0) {

/*        We don't recognize this coordinate name. */

	setmsg_("The coordinate name # belonging to the coordinate system # "
		"is not recognized.", (ftnlen)77);
	errch_("#", crdnam, (ftnlen)1, crdnam_len);
	errch_("#", crdsys, (ftnlen)1, crdsys_len);
	sigerr_("SPICE(NOTSUPPORTED)", (ftnlen)19);
	chkout_("ZZGFCOIN", (ftnlen)8);
	return 0;
    }

/*     Store an upper case, left-justified copy of REF. */

    ljust_(ref, svref, ref_len, (ftnlen)32);
    ucase_(svref, svref, (ftnlen)32, (ftnlen)32);

/*     The remaining work is a function of the vector definition */
/*     and the coordinate system. */

    if (s_cmp(svvdef, "SUB-OBSERVER POINT", (ftnlen)32, (ftnlen)18) == 0 || 
	    s_cmp(svvdef, "SURFACE INTERCEPT POINT", (ftnlen)32, (ftnlen)23) 
	    == 0 || s_cmp(svcsys, "GEODETIC", (ftnlen)32, (ftnlen)8) == 0 || 
	    s_cmp(svcsys, "PLANETOGRAPHIC", (ftnlen)32, (ftnlen)14) == 0) {

/*        The coordinate is defined using a sub-observer point or */
/*        a surface intercept point, OR we're using geodetic or */
/*        planetographic coordinates. In any of these cases, we */
/*        need the center of the input reference frame and the */
/*        radii associated with this center. */

	namfrm_(svref, &frcode, (ftnlen)32);

/*        Save the frame REF's center ID in SVRCTR. */

	frinfo_(&frcode, &svrctr, &class__, &clssid, &found);
	if (! found) {
	    setmsg_("Frame system did not recognize frame #.", (ftnlen)39);
	    errch_("#", ref, (ftnlen)1, ref_len);
	    sigerr_("SPICE(NOFRAME)", (ftnlen)14);
	    chkout_("ZZGFCOIN", (ftnlen)8);
	    return 0;
	}

/*        For sub-observer point and surface intercept vector */
/*        definitions, make sure the input frame's center is */
/*        the target body. */

	if (s_cmp(vecdef, "SUB-OBSERVER POINT", vecdef_len, (ftnlen)18) == 0 
		|| s_cmp(vecdef, "SURFACE INTERCEPT POINT", vecdef_len, (
		ftnlen)23) == 0) {
	    if (svrctr != svtarg) {
		setmsg_("Vector definition method is #, but input reference "
			"frame # has center #. For this vector definition, th"
			"e frame must be centered on the target body #.", (
			ftnlen)149);
		errch_("#", vecdef, (ftnlen)1, vecdef_len);
		errch_("#", ref, (ftnlen)1, ref_len);
		errint_("#", &svrctr, (ftnlen)1);
		errch_("#", target, (ftnlen)1, target_len);
		sigerr_("SPICE(INVALIDFRAME)", (ftnlen)19);
		chkout_("ZZGFCOIN", (ftnlen)8);
		return 0;
	    }
	}

/*        At this point, we know the frame REF is centered on the */
/*        target if the computation method is SINDEF or SOBDEF. */
/*        Fetch the radii of the body acting as the frame center. */


/*        Ensure the radii data exists. If not, return an error message */
/*        with useful information. */

	if (! bodfnd_(&svrctr, "RADII", (ftnlen)5)) {
	    if (s_cmp(svcsys, "GEODETIC", (ftnlen)32, (ftnlen)8) == 0 || 
		    s_cmp(svcsys, "PLANETOGRAPHIC", (ftnlen)32, (ftnlen)14) ==
		     0) {
		setmsg_("No RADII data in kernel pool for frame '#' center b"
			"ody #. Geodetic and planetographic coordinates requi"
			"re a reference frame centered on a finite body. Conf"
			"irm the proper input frame. Bodies {0,..,9} represen"
			"t barycenters and so lack physical properties.", (
			ftnlen)253);
		errch_("#", ref, (ftnlen)1, ref_len);
		errint_("#", &svrctr, (ftnlen)1);
		sigerr_("SPICE(BADFRAME)", (ftnlen)15);
		chkout_("ZZGFCOIN", (ftnlen)8);
	    } else {
		setmsg_("No RADII data in kernel pool for frame '#' center b"
			"ody #. Confirm the proper input frame. Bodies {0,..,"
			"9} represent barycenters and so lack physical proper"
			"ties.", (ftnlen)160);
		errch_("#", ref, (ftnlen)1, ref_len);
		errint_("#", &svrctr, (ftnlen)1);
		sigerr_("SPICE(BADFRAME)", (ftnlen)15);
		chkout_("ZZGFCOIN", (ftnlen)8);
	    }
	    return 0;
	}

/*        We know the kernel pool contains data for body SVRCTR. */

	zzgftreb_(&svrctr, svradi);
	if (failed_()) {
	    chkout_("ZZGFCOIN", (ftnlen)8);
	    return 0;
	}

/*        For geodetic and planetographic coordinates, we need to save */
/*        the equatorial radius and flattening coefficient. For other */
/*        coordinate systems, these quantities aren't needed. */

/*        At this point, we also check for unequal equatorial radii, */
/*        which are not allowed with geodetic or planetographic */
/*        coordinates. */

	if (s_cmp(svcsys, "GEODETIC", (ftnlen)32, (ftnlen)8) == 0 || s_cmp(
		svcsys, "PLANETOGRAPHIC", (ftnlen)32, (ftnlen)14) == 0) {
	    if (svradi[0] != svradi[1]) {
		setmsg_("Central body # of reference frame # has radii # # #"
			". Unequal equatorial ellipsoid radii are not support"
			"ed for # coordinates. ", (ftnlen)125);
		errint_("#", &svrctr, (ftnlen)1);
		errch_("#", ref, (ftnlen)1, ref_len);
		errdp_("#", svradi, (ftnlen)1);
		errdp_("#", &svradi[1], (ftnlen)1);
		errdp_("#", &svradi[2], (ftnlen)1);
		errch_("#", crdsys, (ftnlen)1, crdsys_len);
		sigerr_("SPICE(NOTSUPPORTED)", (ftnlen)19);
		chkout_("ZZGFCOIN", (ftnlen)8);
		return 0;
	    }

/*           Save the equatorial radius of the central body. */

	    svre = svradi[0];

/*           Save the flattening coefficient of the central body. Note */
/*           that we've ensured the denominator is non-zero. */

	    svf = (svradi[0] - svradi[2]) / svradi[0];
	} else {
	    svre = 0.;
	    svf = 0.;
	}

/*        Save the computation method, if required. */

	if (s_cmp(vecdef, "SUB-OBSERVER POINT", vecdef_len, (ftnlen)18) == 0 
		|| s_cmp(vecdef, "SURFACE INTERCEPT POINT", vecdef_len, (
		ftnlen)23) == 0) {

/*           The coordinate is defined using a sub-observer point or */
/*           a surface intercept point. */

/*           Store an upper case, left-justified copy of METHOD. */

	    ljust_(method, svmeth, method_len, (ftnlen)200);
	    ucase_(svmeth, svmeth, (ftnlen)200, (ftnlen)200);
	} else {

/*           Simply initialize SVMETH with a blank string. */

	    s_copy(svmeth, " ", (ftnlen)200, (ftnlen)1);
	}

/*        If we're using planetographic coordinates, we'll need the */
/*        longitude sense. Recall that the body with which these */
/*        coordinates are associated is the center of REF. Find the */
/*        longitude of the +Y axis. */

	if (s_cmp(svcsys, "PLANETOGRAPHIC", (ftnlen)32, (ftnlen)14) == 0) {
	    bodc2s_(&svrctr, svrcnm, (ftnlen)36);
	    recpgr_(svrcnm, y, &svre, &svf, &lon, &lat, &alt, (ftnlen)36);

/*           Planetographic longitude ranges from 0 to 2*pi, so */
/*           longitudes corresponding to positive Y values are */
/*           in the range pi to 2*pi. */

	    if (lon > pi_()) {
		svsens = -1;
	    } else {
		svsens = 1;
	    }
	} else {
	    svsens = 0;
	}
    }

/*     If we're using a surface intercept vector definition, we'll */
/*     need to check and store the variables associated with the */
/*     ray. */

    if (s_cmp(svvdef, "SURFACE INTERCEPT POINT", (ftnlen)32, (ftnlen)23) == 0)
	     {
	if (vzero_(dvec)) {
	    setmsg_("Ray's direction vector is the zero vector. This variabl"
		    "e might be uninitialized.", (ftnlen)80);
	    sigerr_("SPICE(ZEROVECTOR)", (ftnlen)17);
	}

/*        Save DVEC and DREF. */

	moved_(dvec, &c__3, svdvec);
	s_copy(svdref, dref, (ftnlen)32, dref_len);

/*        Save the center of DREF. */

	namfrm_(svdref, &frcode, (ftnlen)32);
	frinfo_(&frcode, &svdctr, &class__, &clssid, &found);
	if (! found) {
	    setmsg_("Frame system did not recognize frame #.", (ftnlen)39);
	    errch_("#", dref, (ftnlen)1, dref_len);
	    sigerr_("SPICE(NOFRAME)", (ftnlen)14);
	    chkout_("ZZGFCOIN", (ftnlen)8);
	    return 0;
	}
    } else {

/*        Simply give initial values to SVDREF, SVDCTR, and SVDVEC. */

	s_copy(svdref, " ", (ftnlen)32, (ftnlen)1);
	svdctr = 0;
	cleard_(&c__3, svdvec);
    }
    chkout_("ZZGFCOIN", (ftnlen)8);
    return 0;
/* $Procedure ZZGFCOG ( GF, get coordinate ) */

L_zzgfcog:
/* $ Abstract */

/*     Compute the coordinate defined by the last call to ZZGFCOIN is at */
/*     the specified epoch. */

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
/*     SPK */
/*     TIME */
/*     NAIF_IDS */
/*     FRAMES */

/* $ Keywords */

/*     GEOMETRY */
/*     PRIVATE */
/*     ROOT */

/* $ Declarations */

/*     DOUBLE PRECISION      ET */
/*     DOUBLE PRECISION      CRDVAL */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     ET         I   Computation epoch. */
/*     CRDVAL     O   Coordinate at epoch. */

/* $ Detailed_Input */

/*     ET       is the computation epoch, expressed as seconds */
/*              past J2000 TDB. */

/* $ Detailed_Output */

/*     CRDVAL   is the coordinate defined by the previous call to */
/*              ZZGFCOIN, evaluated at the epoch ET. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the coordinate cannot be computed at ET, the */
/*         error SPICE(NOTCOMPUTABLE) is signaled. */

/*     2)  If an error occurs while this routine computes the coordinate */
/*         defined by ZZGFCOIN, the error is signaled by a routine */
/*         in the call tree of this routine. */

/* $ Files */

/*     See the $Files header section of the umbrella routine ZZGFCOU. */

/* $ Particulars */

/*     None. */

/* $ Examples */

/*     See ZZGFLONG. */

/* $ Restrictions */

/*     1)  The interface and functionality of this set of routines may */
/*         change without notice. These routines should be called only */
/*         by SPICELIB routines. */

/*     2)  ZZGFCOIN must be called prior to use of any of the other */
/*         entry points. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */

/* $ Version */

/* -    SPICELIB Version 1.0.1, 11-SEP-2021 (JDR) */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 1.0.0, 05-MAR-2009 (NJB) */

/* -& */
/* $ Index_Entries */

/*     get coordinate */

/* -& */

/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("ZZGFCOG", (ftnlen)7);
    }
    zzgfcoq_(svvdef, svmeth, &svtarg, et, svref, svcorr, &svobs, svdref, 
	    svdvec, svcsys, &svrctr, &svre, &svf, svcrd, crdval, &found, (
	    ftnlen)32, (ftnlen)200, (ftnlen)32, (ftnlen)20, (ftnlen)32, (
	    ftnlen)32, (ftnlen)32);
    if (! found) {
	etcal_(et, timstr, (ftnlen)40);
	setmsg_("Coordinate # could not be computed at # TDB", (ftnlen)43);
	errch_("#", svcrd, (ftnlen)1, (ftnlen)32);
	errch_("#", timstr, (ftnlen)1, (ftnlen)40);
	sigerr_("SPICE(NOTCOMPUTABLE)", (ftnlen)20);
	chkout_("ZZGFCOG", (ftnlen)7);
	return 0;
    }
    chkout_("ZZGFCOG", (ftnlen)7);
    return 0;
/* $Procedure ZZGFCODC ( GF, is coordinate decreasing? ) */

L_zzgfcodc:
/* $ Abstract */

/*     Indicate whether the coordinate defined by the last call to */
/*     ZZGFCOIN is decreasing at the specified epoch. */

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
/*     SPK */
/*     TIME */
/*     NAIF_IDS */
/*     FRAMES */

/* $ Keywords */

/*     GEOMETRY */
/*     PRIVATE */
/*     ROOT */

/* $ Declarations */

/*     DOUBLE PRECISION      ET */
/*     LOGICAL               DECRES */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     ET         I   Computation epoch. */
/*     DECRES     O   Flag indicating if coordinate is decreasing. */

/* $ Detailed_Input */

/*     ET       is the computation epoch, expressed as seconds */
/*              past J2000 TDB. */

/* $ Detailed_Output */

/*     DECRES   is a logical flag indicating whether */
/*              the coordinate defined by the previous call to */
/*              ZZGFCOIN is strictly decreasing at the epoch ET. */
/*              DECRES is .FALSE. if the coordinate */
/*              is decreasing and .TRUE. otherwise. */

/*              In cases where the coordinate is undefined */
/*              at ET, DECRES is set to .FALSE. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  In cases where the any intermediate quantity required by */
/*         this routine is undefined, DECRES is set to .FALSE. This */
/*         situation occurs when the Jacobian of the coordinate system */
/*         with respect to rectangular coordinates is undefined at ET. */

/*     2)  If an error occurs while this routine computes the coordinate */
/*         defined by ZZGFCOIN, the error is signaled by a routine in the */
/*         call tree of this routine. */

/*     3)  If an error occurs while this routine computes the derivative */
/*         with respect to time of the coordinate defined by ZZGFCOIN, */
/*         the error is signaled by a routine in the call tree of this */
/*         routine. */

/* $ Files */

/*     See the $Files header section of the umbrella routine ZZGFCOU. */

/* $ Particulars */

/*     A function f(x) is strictly decreasing at x0 if and only if there */
/*     exists some delta > 0 such that for all dx satisfying */

/*        0  <  dx  < delta */

/*     we have */

/*        f(x0)       <  f(x0 + dx) */

/*     and */

/*        f(x0 - dx)  <  f(x) */

/*     Note that a strictly decreasing function need not be */
/*     differentiable in a neighborhood of x0; it can have jump */
/*     discontinuities in any neighborhood of x0 and even at x0. */

/* $ Examples */

/*     See ZZGFLONG. */

/* $ Restrictions */

/*     1)  The interface and functionality of this set of routines may */
/*         change without notice. These routines should be called only */
/*         by SPICELIB routines. */

/*     2)  ZZGFCOIN must be called prior to use of any of the other */
/*         entry points. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     E.D. Wright        (JPL) */

/* $ Version */

/* -    SPICELIB Version 2.0.1, 11-SEP-2021 (JDR) */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 2.0.0, 05-APR-2011 (EDW) */

/*        Added UDFUNC to argument list for use of ZZGFRELX when */
/*        calculating the events. */

/* -    SPICELIB Version 1.0.0, 05-MAR-2009 (NJB) */

/* -& */
/* $ Index_Entries */

/*     is coordinate decreasing */

/* -& */

/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }
    chkin_("ZZGFCODC", (ftnlen)8);

/*     Fetch the state from which the coordinate is derived. If the */
/*     state can't be computed, we consider the coordinate to be */
/*     "not decreasing." */

    zzgfcost_(svvdef, svmeth, &svtarg, et, svref, svcorr, &svobs, svdref, &
	    svdctr, svdvec, svradi, state, &found, (ftnlen)32, (ftnlen)200, (
	    ftnlen)32, (ftnlen)20, (ftnlen)32);
    if (! found) {
	*decres = FALSE_;
	etcal_(et, timstr, (ftnlen)40);
	setmsg_("Coordinate # could not be computed at # TDB", (ftnlen)43);
	errch_("#", svcrd, (ftnlen)1, (ftnlen)32);
	errch_("#", timstr, (ftnlen)1, (ftnlen)40);
	sigerr_("SPICE(NOTCOMPUTABLE)", (ftnlen)20);
	chkout_("ZZGFCODC", (ftnlen)8);
	return 0;
    }

/*     Compute the proxy for the derivative with respect to time of the */
/*     coordinate. This proxy gives us the sign of the derivative, which */
/*     is all we need to determine whether the coordinate is decreasing. */

    zzgfcprx_(state, svcsys, &svre, &svf, &svsens, cdsign, (ftnlen)32);

/*     The quantity is decreasing if and only if the derivative */
/*     is negative. This is indicated by a "sign" of -1. */

    *decres = cdsign[(i__1 = svcidx - 1) < 3 && 0 <= i__1 ? i__1 : s_rnge(
	    "cdsign", i__1, "zzgfcou_", (ftnlen)1679)] == -1;
    chkout_("ZZGFCODC", (ftnlen)8);
    return 0;
/* $Procedure ZZGFCOEX ( GF, does coordinate state exist? ) */

L_zzgfcoex:
/* $ Abstract */

/*     Indicate whether the state of coordinate defined by the last call */
/*     to ZZGFCOIN is computable at the specified epoch. */

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
/*     SPK */
/*     TIME */
/*     NAIF_IDS */
/*     FRAMES */

/* $ Keywords */

/*     GEOMETRY */
/*     PRIVATE */
/*     ROOT */

/* $ Declarations */

/*     DOUBLE PRECISION      ET */
/*     LOGICAL               CRDFND */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     ET         I   Computation epoch. */
/*     CRDFND     O   Flag indicating if coordinate state is computable. */

/* $ Detailed_Input */

/*     ET       is the computation epoch, expressed as seconds */
/*              past J2000 TDB. */

/* $ Detailed_Output */

/*     CRDFND   is a logical flag indicating whether the state of */
/*              the coordinate defined by the previous call to */
/*              ZZGFCOIN is computable at the epoch ET. DECRES is */
/*              .TRUE. if the coordinate is computable and .FALSE. */
/*              otherwise. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If an error occurs while this routine attempts to compute the */
/*         coordinate defined by ZZGFCOIN, the error is signaled by a */
/*         routine in the call tree of this routine. */

/* $ Files */

/*     See the $Files header section of the umbrella routine ZZGFCOU. */

/* $ Particulars */

/*     This routine is used by the GF system to compute a time window */
/*     over which a specified coordinate state is computable. */

/*     Coordinates defined by surface intercepts may fail to be */
/*     computable because either */

/*     -  the surface intercept does not exist */

/*     -  the velocity of the intercept is not computable */

/* $ Examples */

/*     See ZZGFCSLV. */

/* $ Restrictions */

/*     1)  The interface and functionality of this set of routines may */
/*         change without notice. These routines should be called only */
/*         by SPICELIB routines. */

/*     2)  ZZGFCOIN must be called prior to use of any of the other */
/*         entry points. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     E.D. Wright        (JPL) */

/* $ Version */

/* -    SPICELIB Version 2.0.1, 11-SEP-2021 (JDR) */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 2.0.0, 05-APR-2011 (EDW) */

/*        Added UDFUNC to argument list for use of ZZGFRELX when */
/*        calculating the events. */

/* -    SPICELIB Version 1.0.0, 05-MAR-2009 (NJB) */

/* -& */
/* $ Index_Entries */

/*     does coordinate state exist */
/*     is coordinate state computable */

/* -& */

/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }
    chkin_("ZZGFCOEX", (ftnlen)8);

/*     Simply attempt to compute the state. The returned found flag */
/*     is the result. */

    zzgfcost_(svvdef, svmeth, &svtarg, et, svref, svcorr, &svobs, svdref, &
	    svdctr, svdvec, svradi, state, crdfnd, (ftnlen)32, (ftnlen)200, (
	    ftnlen)32, (ftnlen)20, (ftnlen)32);
    chkout_("ZZGFCOEX", (ftnlen)8);
    return 0;
/* $Procedure ZZGFCOCG ( GF, get cosine of coordinate ) */

L_zzgfcocg:
/* $ Abstract */

/*     Compute the cosine of the coordinate defined by the last call to */
/*     ZZGFCOIN is at the specified epoch. */

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
/*     SPK */
/*     TIME */
/*     NAIF_IDS */
/*     FRAMES */

/* $ Keywords */

/*     GEOMETRY */
/*     PRIVATE */
/*     ROOT */

/* $ Declarations */

/*     DOUBLE PRECISION      ET */
/*     DOUBLE PRECISION      CRDVAL */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     ET         I   Computation epoch. */
/*     CRDVAL     O   Cosine of coordinate at epoch. */

/* $ Detailed_Input */

/*     ET       is the computation epoch, expressed as seconds */
/*              past J2000 TDB. */

/* $ Detailed_Output */

/*     CRDVAL   is the cosine of the coordinate defined by the */
/*              previous call to ZZGFCOIN, evaluated at the epoch */
/*              ET. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If an error occurs while this routine computes the coordinate */
/*         defined by ZZGFCOIN, the error is signaled by a routine in the */
/*         call tree of this routine. */

/* $ Files */

/*     See the $Files header section of the umbrella routine ZZGFCOU. */

/* $ Particulars */

/*     None. */

/* $ Examples */

/*     See ZZGFLONG. */

/* $ Restrictions */

/*     1)  The interface and functionality of this set of routines may */
/*         change without notice. These routines should be called only */
/*         by SPICELIB routines. */

/*     2)  ZZGFCOIN must be called prior to use of any of the other */
/*         entry points. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */

/* $ Version */

/* -    SPICELIB Version 1.0.1, 11-SEP-2021 (JDR) */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 1.0.0, 05-MAR-2009 (NJB) */

/* -& */
/* $ Index_Entries */

/*     get cosine of coordinate */

/* -& */

/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }
    chkin_("ZZGFCOCG", (ftnlen)8);
    zzgfcoq_(svvdef, svmeth, &svtarg, et, svref, svcorr, &svobs, svdref, 
	    svdvec, svcsys, &svrctr, &svre, &svf, svcrd, &value, &found, (
	    ftnlen)32, (ftnlen)200, (ftnlen)32, (ftnlen)20, (ftnlen)32, (
	    ftnlen)32, (ftnlen)32);
    if (! found) {
	etcal_(et, timstr, (ftnlen)40);
	setmsg_("Coordinate # could not be computed at # TDB", (ftnlen)43);
	errch_("#", svcrd, (ftnlen)1, (ftnlen)32);
	errch_("#", timstr, (ftnlen)1, (ftnlen)40);
	sigerr_("SPICE(NOTCOMPUTABLE)", (ftnlen)20);
	chkout_("ZZGFCOCG", (ftnlen)8);
	return 0;
    }
    *crdval = cos(value);
    chkout_("ZZGFCOCG", (ftnlen)8);
    return 0;
/* $Procedure ZZGFCOSG ( GF, get sine of coordinate ) */

L_zzgfcosg:
/* $ Abstract */

/*     Compute the sine of the coordinate defined by the last call to */
/*     ZZGFCOIN is at the specified epoch. */

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
/*     SPK */
/*     TIME */
/*     NAIF_IDS */
/*     FRAMES */

/* $ Keywords */

/*     GEOMETRY */
/*     PRIVATE */
/*     ROOT */

/* $ Declarations */

/*     DOUBLE PRECISION      ET */
/*     DOUBLE PRECISION      CRDVAL */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     ET         I   Computation epoch. */
/*     CRDVAL     O   Sine of coordinate at epoch. */

/* $ Detailed_Input */

/*     ET       is the computation epoch, expressed as seconds */
/*              past J2000 TDB. */

/* $ Detailed_Output */

/*     CRDVAL   is the sine of the coordinate defined by the */
/*              previous call to ZZGFCOIN, evaluated at the epoch */
/*              ET. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If an error occurs while this routine computes the coordinate */
/*         defined by ZZGFCOIN, the error is signaled by a routine in the */
/*         call tree of this routine. */

/* $ Files */

/*     See the $Files header section of the umbrella routine ZZGFCOU. */

/* $ Particulars */

/*     None. */

/* $ Examples */

/*     See ZZGFLONG. */

/* $ Restrictions */

/*     1)  The interface and functionality of this set of routines may */
/*         change without notice. These routines should be called only */
/*         by SPICELIB routines. */

/*     2)  ZZGFCOIN must be called prior to use of any of the other */
/*         entry points. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */

/* $ Version */

/* -    SPICELIB Version 1.0.1, 11-SEP-2021 (JDR) */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 1.0.0, 05-MAR-2009 (NJB) */

/* -& */
/* $ Index_Entries */

/*     get sine of coordinate */

/* -& */
    if (return_()) {
	return 0;
    }
    chkin_("ZZGFCOSG", (ftnlen)8);
    zzgfcoq_(svvdef, svmeth, &svtarg, et, svref, svcorr, &svobs, svdref, 
	    svdvec, svcsys, &svrctr, &svre, &svf, svcrd, &value, &found, (
	    ftnlen)32, (ftnlen)200, (ftnlen)32, (ftnlen)20, (ftnlen)32, (
	    ftnlen)32, (ftnlen)32);
    if (! found) {
	etcal_(et, timstr, (ftnlen)40);
	setmsg_("Coordinate # could not be computed at # TDB", (ftnlen)43);
	errch_("#", svcrd, (ftnlen)1, (ftnlen)32);
	errch_("#", timstr, (ftnlen)1, (ftnlen)40);
	sigerr_("SPICE(NOTCOMPUTABLE)", (ftnlen)20);
	chkout_("ZZGFCOSG", (ftnlen)8);
	return 0;
    }
    *crdval = sin(value);
    chkout_("ZZGFCOSG", (ftnlen)8);
    return 0;
/* $Procedure ZZGFCOCD ( GF, is cosine of coordinate decreasing? ) */

L_zzgfcocd:
/* $ Abstract */

/*     Indicate whether the cosine of the coordinate defined by the */
/*     last call to ZZGFCOIN is decreasing at the specified epoch. */

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
/*     SPK */
/*     TIME */
/*     NAIF_IDS */
/*     FRAMES */

/* $ Keywords */

/*     GEOMETRY */
/*     PRIVATE */
/*     ROOT */

/* $ Declarations */

/*     DOUBLE PRECISION      ET */
/*     LOGICAL               DECRES */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     ET         I   Computation epoch. */
/*     DECRES     O   Flag indicating if cos of coordinate is decreasing. */

/* $ Detailed_Input */

/*     ET       is the computation epoch, expressed as seconds */
/*              past J2000 TDB. */

/* $ Detailed_Output */

/*     DECRES   is a logical flag indicating whether the cosine of */
/*              the coordinate defined by the previous call to */
/*              ZZGFCOIN is strictly decreasing at the epoch ET. */
/*              DECRES is .FALSE. if the cosine of the coordinate */
/*              is decreasing and .TRUE. otherwise. */

/*              In cases where the coordinate is undefined */
/*              at ET, DECRES is set to .FALSE. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  In cases where the any intermediate quantity required by */
/*         this routine is undefined, DECRES is set to .FALSE. This */
/*         situation occurs when the Jacobian of the coordinate system */
/*         with respect to rectangular coordinates is undefined at ET. */

/*     2)  If an error occurs while this routine computes the coordinate */
/*         defined by ZZGFCOIN, the error is signaled by a routine in the */
/*         call tree of this routine. */

/*     3)  If an error occurs while this routine computes the derivative */
/*         with respect to time of the coordinate defined by ZZGFCOIN, */
/*         the error is signaled by a routine in the call tree of this */
/*         routine. */

/* $ Files */

/*     See the $Files header section of the umbrella routine ZZGFCOU. */

/* $ Particulars */

/*     A function f(x) is strictly decreasing at x0 if and only if there */
/*     exists some delta > 0 such that for all dx satisfying */

/*        0  <  dx  < delta */

/*     we have */

/*        f(x0)       <  f(x0 + dx) */

/*     and */

/*        f(x0 - dx)  <  f(x) */

/*     Note that a strictly decreasing function need not be */
/*     differentiable in a neighborhood of x0; it can have jump */
/*     discontinuities in any neighborhood of x0 and even at x0. */

/* $ Examples */

/*     See ZZGFLONG. */

/* $ Restrictions */

/*     1)  The interface and functionality of this set of routines may */
/*         change without notice. These routines should be called only */
/*         by SPICELIB routines. */

/*     2)  ZZGFCOIN must be called prior to use of any of the other */
/*         entry points. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     E.D. Wright        (JPL) */

/* $ Version */

/* -    SPICELIB Version 2.0.1, 11-SEP-2021 (JDR) */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 2.0.0, 05-APR-2011 (EDW) */

/*        Added UDFUNC to argument list for use of ZZGFRELX when */
/*        calculating the events. */

/* -    SPICELIB Version 1.0.0, 05-MAR-2009 (NJB) */

/* -& */
/* $ Index_Entries */

/*     is cosine of coordinate decreasing */

/* -& */

/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }
    chkin_("ZZGFCOCD", (ftnlen)8);

/*     The derivative of cosine of the coordinate Q is */

/*         - sin ( Q(ET) ) * d( Q(ET) )/d(ET) */

/*     Look up the individual terms. Start with the Cartesian */
/*     state vector from whose position component Q is */
/*     derived. */

    zzgfcost_(svvdef, svmeth, &svtarg, et, svref, svcorr, &svobs, svdref, &
	    svdctr, svdvec, svradi, state, &found, (ftnlen)32, (ftnlen)200, (
	    ftnlen)32, (ftnlen)20, (ftnlen)32);
    if (! found) {
	*decres = FALSE_;
	etcal_(et, timstr, (ftnlen)40);
	setmsg_("Coordinate # could not be computed at # TDB", (ftnlen)43);
	errch_("#", svcrd, (ftnlen)1, (ftnlen)32);
	errch_("#", timstr, (ftnlen)1, (ftnlen)40);
	sigerr_("SPICE(NOTCOMPUTABLE)", (ftnlen)20);
	chkout_("ZZGFCOCD", (ftnlen)8);
	return 0;
    }

/*     At this point we assume the state whose coordinate is to be */
/*     computed resides in STATE. Convert the position portion of STATE */
/*     to the specified coordinate system. */

    if (s_cmp(svcsys, "RECTANGULAR", (ftnlen)32, (ftnlen)11) == 0) {

/*        No conversion needed for rectangular coordinates. */

	moved_(state, &c__3, coords);
    } else if (s_cmp(svcsys, "LATITUDINAL", (ftnlen)32, (ftnlen)11) == 0) {
	reclat_(state, coords, &coords[1], &coords[2]);
    } else if (s_cmp(svcsys, "RA/DEC", (ftnlen)32, (ftnlen)6) == 0) {
	recrad_(state, coords, &coords[1], &coords[2]);
    } else if (s_cmp(svcsys, "SPHERICAL", (ftnlen)32, (ftnlen)9) == 0) {
	recsph_(state, coords, &coords[1], &coords[2]);
    } else if (s_cmp(svcsys, "CYLINDRICAL", (ftnlen)32, (ftnlen)11) == 0) {
	reccyl_(state, coords, &coords[1], &coords[2]);
    } else if (s_cmp(svcsys, "GEODETIC", (ftnlen)32, (ftnlen)8) == 0) {
	recgeo_(state, &svre, &svf, coords, &coords[1], &coords[2]);
    } else if (s_cmp(svcsys, "PLANETOGRAPHIC", (ftnlen)32, (ftnlen)14) == 0) {
	recpgr_(svrcnm, state, &svre, &svf, coords, &coords[1], &coords[2], (
		ftnlen)36);
    } else {

/*        We should never arrive here. */

	setmsg_("The coordinate system # is not supported.", (ftnlen)41);
	errch_("#", crdsys, (ftnlen)1, crdsys_len);
	sigerr_("SPICE(BUG)", (ftnlen)10);
	chkout_("ZZGFCOCD", (ftnlen)8);
	return 0;
    }

/*     Pick off the coordinate value. */

    value = coords[(i__1 = svcidx - 1) < 3 && 0 <= i__1 ? i__1 : s_rnge("coo"
	    "rds", i__1, "zzgfcou_", (ftnlen)2444)];

/*     Compute the proxy for the derivative with respect to time of the */
/*     coordinate. This proxy gives us the sign of the derivative, which */
/*     is all we need to determine whether the coordinate is decreasing. */

    zzgfcprx_(state, svcsys, &svre, &svf, &svsens, cdsign, (ftnlen)32);

/*     The derivative of the coordinate is negative if the "sign" is -1. */

    *decres = -sin(value) * cdsign[(i__1 = svcidx - 1) < 3 && 0 <= i__1 ? 
	    i__1 : s_rnge("cdsign", i__1, "zzgfcou_", (ftnlen)2456)] < 0.;
    chkout_("ZZGFCOCD", (ftnlen)8);
    return 0;
/* $Procedure ZZGFCOSD ( GF, is sine of coordinate decreasing? ) */

L_zzgfcosd:
/* $ Abstract */

/*     Indicate whether the sine of the coordinate defined by the */
/*     last call to ZZGFCOIN is decreasing at the specified epoch. */

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
/*     SPK */
/*     TIME */
/*     NAIF_IDS */
/*     FRAMES */

/* $ Keywords */

/*     GEOMETRY */
/*     PRIVATE */
/*     ROOT */

/* $ Declarations */

/*     DOUBLE PRECISION      ET */
/*     LOGICAL               DECRES */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     ET         I   Computation epoch. */
/*     DECRES     O   Flag indicating if sine of coordinate is */
/*                    decreasing. */

/* $ Detailed_Input */

/*     ET       is the computation epoch, expressed as seconds */
/*              past J2000 TDB. */

/* $ Detailed_Output */

/*     DECRES   is a logical flag indicating whether the sine */
/*              of the coordinate defined by the previous call to */
/*              ZZGFCOIN is strictly decreasing at the epoch ET. */
/*              DECRES is .FALSE. if the sine of the coordinate is */
/*              decreasing and .TRUE. otherwise. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  In cases where the any intermediate quantity required by */
/*         this routine is undefined, DECRES is set to .FALSE. This */
/*         situation occurs when the Jacobian of the coordinate system */
/*         with respect to rectangular coordinates is undefined at ET. */

/*     2)  If an error occurs while this routine computes the coordinate */
/*         defined by ZZGFCOIN, the error is signaled by a routine in the */
/*         call tree of this routine. */

/*     3)  If an error occurs while this routine computes the derivative */
/*         with respect to time of the coordinate defined by ZZGFCOIN, */
/*         the error is signaled by a routine in the call tree of this */
/*         routine. */

/* $ Files */

/*     See the $Files header section of the umbrella routine ZZGFCOU. */

/* $ Particulars */

/*     A function f(x) is strictly decreasing at x0 if and only if there */
/*     exists some delta > 0 such that for all dx satisfying */

/*        0  <  dx  < delta */

/*     we have */

/*        f(x0)       <  f(x0 + dx) */

/*     and */

/*        f(x0 - dx)  <  f(x) */

/*     Note that a strictly decreasing function need not be */
/*     differentiable in a neighborhood of x0; it can have jump */
/*     discontinuities in any neighborhood of x0 and even at x0. */

/* $ Examples */

/*     See ZZGFLONG. */

/* $ Restrictions */

/*     1)  The interface and functionality of this set of routines may */
/*         change without notice. These routines should be called only */
/*         by SPICELIB routines. */

/*     2)  ZZGFCOIN must be called prior to use of any of the other */
/*         entry points. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     E.D. Wright        (JPL) */

/* $ Version */

/* -    SPICELIB Version 2.0.1, 11-SEP-2021 (JDR) */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 2.0.0, 05-APR-2011 (EDW) */

/*        Added UDFUNC to argument list for use of ZZGFRELX when */
/*        calculating the events. */

/* -    SPICELIB Version 1.0.0, 05-MAR-2009 (NJB) */

/* -& */
/* $ Index_Entries */

/*     is sine of coordinate decreasing */

/* -& */

/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }
    chkin_("ZZGFCOSD", (ftnlen)8);

/*     The derivative of the sine of the coordinate Q is */

/*        cos ( Q(ET) ) * d( Q(ET) )/d(ET) */

/*     Look up the individual terms. Start with the Cartesian state */
/*     vector from whose position component Q is derived. */

    zzgfcost_(svvdef, svmeth, &svtarg, et, svref, svcorr, &svobs, svdref, &
	    svdctr, svdvec, svradi, state, &found, (ftnlen)32, (ftnlen)200, (
	    ftnlen)32, (ftnlen)20, (ftnlen)32);
    if (! found) {
	*decres = FALSE_;
	etcal_(et, timstr, (ftnlen)40);
	setmsg_("Coordinate # could not be computed at # TDB", (ftnlen)43);
	errch_("#", svcrd, (ftnlen)1, (ftnlen)32);
	errch_("#", timstr, (ftnlen)1, (ftnlen)40);
	sigerr_("SPICE(NOTCOMPUTABLE)", (ftnlen)20);
	chkout_("ZZGFCOSD", (ftnlen)8);
	return 0;
    }

/*     At this point we assume the state whose coordinate is to be */
/*     computed resides in STATE. Convert the position portion of STATE */
/*     to the specified coordinate system. */

    if (s_cmp(svcsys, "RECTANGULAR", (ftnlen)32, (ftnlen)11) == 0) {

/*        No conversion needed for rectangular coordinates. */

	moved_(state, &c__3, coords);
    } else if (s_cmp(svcsys, "LATITUDINAL", (ftnlen)32, (ftnlen)11) == 0) {
	reclat_(state, coords, &coords[1], &coords[2]);
    } else if (s_cmp(svcsys, "RA/DEC", (ftnlen)32, (ftnlen)6) == 0) {
	recrad_(state, coords, &coords[1], &coords[2]);
    } else if (s_cmp(svcsys, "SPHERICAL", (ftnlen)32, (ftnlen)9) == 0) {
	recsph_(state, coords, &coords[1], &coords[2]);
    } else if (s_cmp(svcsys, "CYLINDRICAL", (ftnlen)32, (ftnlen)11) == 0) {
	reccyl_(state, coords, &coords[1], &coords[2]);
    } else if (s_cmp(svcsys, "GEODETIC", (ftnlen)32, (ftnlen)8) == 0) {
	recgeo_(state, &svre, &svf, coords, &coords[1], &coords[2]);
    } else if (s_cmp(svcsys, "PLANETOGRAPHIC", (ftnlen)32, (ftnlen)14) == 0) {
	recpgr_(svrcnm, state, &svre, &svf, coords, &coords[1], &coords[2], (
		ftnlen)36);
    } else {

/*        We should never arrive here. */

	setmsg_("The coordinate system # is not supported.", (ftnlen)41);
	errch_("#", crdsys, (ftnlen)1, crdsys_len);
	sigerr_("SPICE(BUG)", (ftnlen)10);
	chkout_("ZZGFCOSD", (ftnlen)8);
	return 0;
    }

/*     Pick off the coordinate value. */

    value = coords[(i__1 = svcidx - 1) < 3 && 0 <= i__1 ? i__1 : s_rnge("coo"
	    "rds", i__1, "zzgfcou_", (ftnlen)2724)];

/*     Compute the proxy for the derivative with respect to time of the */
/*     coordinate. This proxy gives us the sign of the derivative, which */
/*     is all we need to determine whether the coordinate is decreasing. */

    zzgfcprx_(state, svcsys, &svre, &svf, &svsens, cdsign, (ftnlen)32);

/*     The derivative of the coordinate is negative if the "sign" is -1. */

    *decres = cos(value) * cdsign[(i__1 = svcidx - 1) < 3 && 0 <= i__1 ? i__1 
	    : s_rnge("cdsign", i__1, "zzgfcou_", (ftnlen)2736)] < 0.;
    chkout_("ZZGFCOSD", (ftnlen)8);
    return 0;
} /* zzgfcou_ */

/* Subroutine */ int zzgfcou_(char *vecdef, char *method, char *target, 
	doublereal *et, char *ref, char *abcorr, char *obsrvr, char *dref, 
	doublereal *dvec, char *crdsys, char *crdnam, logical *decres, 
	doublereal *crdval, logical *crdfnd, U_fp udfunc, ftnlen vecdef_len, 
	ftnlen method_len, ftnlen target_len, ftnlen ref_len, ftnlen 
	abcorr_len, ftnlen obsrvr_len, ftnlen dref_len, ftnlen crdsys_len, 
	ftnlen crdnam_len)
{
    return zzgfcou_0_(0, vecdef, method, target, et, ref, abcorr, obsrvr, 
	    dref, dvec, crdsys, crdnam, decres, crdval, crdfnd, udfunc, 
	    vecdef_len, method_len, target_len, ref_len, abcorr_len, 
	    obsrvr_len, dref_len, crdsys_len, crdnam_len);
    }

/* Subroutine */ int zzgfcoin_(char *vecdef, char *method, char *target, char 
	*ref, char *abcorr, char *obsrvr, char *dref, doublereal *dvec, char *
	crdsys, char *crdnam, ftnlen vecdef_len, ftnlen method_len, ftnlen 
	target_len, ftnlen ref_len, ftnlen abcorr_len, ftnlen obsrvr_len, 
	ftnlen dref_len, ftnlen crdsys_len, ftnlen crdnam_len)
{
    return zzgfcou_0_(1, vecdef, method, target, (doublereal *)0, ref, abcorr,
	     obsrvr, dref, dvec, crdsys, crdnam, (logical *)0, (doublereal *)
	    0, (logical *)0, (U_fp)0, vecdef_len, method_len, target_len, 
	    ref_len, abcorr_len, obsrvr_len, dref_len, crdsys_len, crdnam_len)
	    ;
    }

/* Subroutine */ int zzgfcog_(doublereal *et, doublereal *crdval)
{
    return zzgfcou_0_(2, (char *)0, (char *)0, (char *)0, et, (char *)0, (
	    char *)0, (char *)0, (char *)0, (doublereal *)0, (char *)0, (char 
	    *)0, (logical *)0, crdval, (logical *)0, (U_fp)0, (ftnint)0, (
	    ftnint)0, (ftnint)0, (ftnint)0, (ftnint)0, (ftnint)0, (ftnint)0, (
	    ftnint)0, (ftnint)0);
    }

/* Subroutine */ int zzgfcodc_(U_fp udfunc, doublereal *et, logical *decres)
{
    return zzgfcou_0_(3, (char *)0, (char *)0, (char *)0, et, (char *)0, (
	    char *)0, (char *)0, (char *)0, (doublereal *)0, (char *)0, (char 
	    *)0, decres, (doublereal *)0, (logical *)0, udfunc, (ftnint)0, (
	    ftnint)0, (ftnint)0, (ftnint)0, (ftnint)0, (ftnint)0, (ftnint)0, (
	    ftnint)0, (ftnint)0);
    }

/* Subroutine */ int zzgfcoex_(U_fp udfunc, doublereal *et, logical *crdfnd)
{
    return zzgfcou_0_(4, (char *)0, (char *)0, (char *)0, et, (char *)0, (
	    char *)0, (char *)0, (char *)0, (doublereal *)0, (char *)0, (char 
	    *)0, (logical *)0, (doublereal *)0, crdfnd, udfunc, (ftnint)0, (
	    ftnint)0, (ftnint)0, (ftnint)0, (ftnint)0, (ftnint)0, (ftnint)0, (
	    ftnint)0, (ftnint)0);
    }

/* Subroutine */ int zzgfcocg_(doublereal *et, doublereal *crdval)
{
    return zzgfcou_0_(5, (char *)0, (char *)0, (char *)0, et, (char *)0, (
	    char *)0, (char *)0, (char *)0, (doublereal *)0, (char *)0, (char 
	    *)0, (logical *)0, crdval, (logical *)0, (U_fp)0, (ftnint)0, (
	    ftnint)0, (ftnint)0, (ftnint)0, (ftnint)0, (ftnint)0, (ftnint)0, (
	    ftnint)0, (ftnint)0);
    }

/* Subroutine */ int zzgfcosg_(doublereal *et, doublereal *crdval)
{
    return zzgfcou_0_(6, (char *)0, (char *)0, (char *)0, et, (char *)0, (
	    char *)0, (char *)0, (char *)0, (doublereal *)0, (char *)0, (char 
	    *)0, (logical *)0, crdval, (logical *)0, (U_fp)0, (ftnint)0, (
	    ftnint)0, (ftnint)0, (ftnint)0, (ftnint)0, (ftnint)0, (ftnint)0, (
	    ftnint)0, (ftnint)0);
    }

/* Subroutine */ int zzgfcocd_(U_fp udfunc, doublereal *et, logical *decres)
{
    return zzgfcou_0_(7, (char *)0, (char *)0, (char *)0, et, (char *)0, (
	    char *)0, (char *)0, (char *)0, (doublereal *)0, (char *)0, (char 
	    *)0, decres, (doublereal *)0, (logical *)0, udfunc, (ftnint)0, (
	    ftnint)0, (ftnint)0, (ftnint)0, (ftnint)0, (ftnint)0, (ftnint)0, (
	    ftnint)0, (ftnint)0);
    }

/* Subroutine */ int zzgfcosd_(U_fp udfunc, doublereal *et, logical *decres)
{
    return zzgfcou_0_(8, (char *)0, (char *)0, (char *)0, et, (char *)0, (
	    char *)0, (char *)0, (char *)0, (doublereal *)0, (char *)0, (char 
	    *)0, decres, (doublereal *)0, (logical *)0, udfunc, (ftnint)0, (
	    ftnint)0, (ftnint)0, (ftnint)0, (ftnint)0, (ftnint)0, (ftnint)0, (
	    ftnint)0, (ftnint)0);
    }

