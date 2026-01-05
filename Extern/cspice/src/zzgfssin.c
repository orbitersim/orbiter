/* zzgfssin.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__3 = 3;
static integer c__6 = 6;
static doublereal c_b51 = 1.;

/* $Procedure      ZZGFSSIN ( GF, state of surface intercept point ) */
/* Subroutine */ int zzgfssin_(char *method, integer *trgid, doublereal *et, 
	char *fixref, char *abcorr, integer *obsid, char *dref, integer *dctr,
	 doublereal *dvec, doublereal *radii, doublereal *state, logical *
	found, ftnlen method_len, ftnlen fixref_len, ftnlen abcorr_len, 
	ftnlen dref_len)
{
    /* Initialized data */

    static logical first = TRUE_;
    static integer prvobs = 0;
    static integer prvtrg = 0;
    static char svobs[36] = "                                    ";
    static char svtarg[36] = "                                    ";

    /* System generated locals */
    integer i__1;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer);

    /* Local variables */
    logical geom;
    extern /* Subroutine */ int vhat_(doublereal *, doublereal *), vscl_(
	    doublereal *, doublereal *, doublereal *);
    extern doublereal vdot_(doublereal *, doublereal *);
    logical xmit;
    extern /* Subroutine */ int mxvg_(doublereal *, doublereal *, integer *, 
	    integer *, doublereal *);
    doublereal upos[3];
    extern /* Subroutine */ int zzstelab_(logical *, doublereal *, doublereal 
	    *, doublereal *, doublereal *, doublereal *), zzvalcor_(char *, 
	    logical *, ftnlen), zzcorsxf_(logical *, doublereal *, doublereal 
	    *, doublereal *);
    integer i__;
    doublereal t;
    extern /* Subroutine */ int vaddg_(doublereal *, doublereal *, integer *, 
	    doublereal *);
    doublereal scale;
    extern /* Subroutine */ int chkin_(char *, ftnlen), errch_(char *, char *,
	     ftnlen, ftnlen);
    doublereal savel[3];
    extern /* Subroutine */ int moved_(doublereal *, integer *, doublereal *),
	     vsubg_(doublereal *, doublereal *, integer *, doublereal *);
    doublereal ltctr, stemp[6];
    extern logical eqstr_(char *, char *, ftnlen, ftnlen);
    doublereal xform[36]	/* was [6][6] */;
    logical uselt;
    extern /* Subroutine */ int bodc2s_(integer *, char *, ftnlen);
    doublereal j2dsta[6], ssbtg0[6];
    extern logical failed_(void);
    doublereal sa[3];
    extern /* Subroutine */ int cleard_(integer *, doublereal *);
    doublereal lt, drfepc;
    integer frcode;
    extern doublereal clight_(void);
    extern logical return_(void);
    doublereal corxfi[36]	/* was [6][6] */, corxfm[36]	/* was [6][6] 
	    */, ctrsta[6], dcorxf[36]	/* was [6][6] */, dltctr, drxfrm[36]	
	    /* was [6][6] */, fxdsta[6], fxosta[6], fxpsta[6], acc[3], fxpvel[
	    3], fxtsta[6], ltsign, obspnt[6], obssta[12]	/* was [6][2] 
	    */, obstrg[6], pntsta[6], sastat[6], spoint[3], srfvec[3], ssbobs[
	    6], ssbtrg[6], trgepc;
    integer center, clssid, frclss;
    logical attblk[6], fnd, usestl;
    extern /* Subroutine */ int chkout_(char *, ftnlen), setmsg_(char *, 
	    ftnlen), sigerr_(char *, ftnlen), sxform_(char *, char *, 
	    doublereal *, doublereal *, ftnlen, ftnlen), namfrm_(char *, 
	    integer *, ftnlen), frinfo_(integer *, integer *, integer *, 
	    integer *, logical *), errint_(char *, integer *, ftnlen);
    doublereal dlt;
    extern /* Subroutine */ int spkgeo_(integer *, doublereal *, char *, 
	    integer *, doublereal *, doublereal *, ftnlen), vminug_(
	    doublereal *, integer *, doublereal *), surfpv_(doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, logical *), spkacs_(integer *, doublereal *, char *,
	     char *, integer *, doublereal *, doublereal *, doublereal *, 
	    ftnlen, ftnlen), sincpt_(char *, char *, doublereal *, char *, 
	    char *, char *, char *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, logical *, ftnlen, ftnlen, ftnlen, ftnlen, ftnlen, 
	    ftnlen), spkssb_(integer *, doublereal *, char *, doublereal *, 
	    ftnlen), qderiv_(integer *, doublereal *, doublereal *, 
	    doublereal *, doublereal *), invstm_(doublereal *, doublereal *);

/* $ Abstract */

/*     SPICE private routine intended solely for the support of SPICE */
/*     routines. Users should not call this routine directly due to the */
/*     volatile nature of this routine. */

/*     Return the state of a ray-target surface intercept point used to */
/*     define coordinates referenced in a GF search. */

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

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     METHOD     I   Computation method. */
/*     TRGID      I   Target ID code. */
/*     ET         I   Computation epoch. */
/*     FIXREF     I   Reference frame name. */
/*     ABCORR     I   Aberration correction. */
/*     OBSID      I   Observer ID code. */
/*     DREF       I   Reference frame of ray's direction vector. */
/*     DCTR       I   DREF's center ID code. */
/*     DVEC       I   Ray's direction vector. */
/*     RADII      I   Target radii. */
/*     STATE      O   State used to define coordinates. */
/*     FOUND      O   Flag indicating whether state was found. */

/* $ Detailed_Input */

/*     METHOD      is a short string providing parameters defining */
/*                 the computation method to be used. Any value */
/*                 supported by SUBPNT may be used. */


/*     TRGID      is the NAIF ID code of the target object. */

/*                *This routine assumes that the target is modeled */
/*                as a tri-axial ellipsoid.* */


/*     ET         is the time, expressed as ephemeris seconds past J2000 */
/*                TDB, at which the specified state is to be computed. */


/*     FIXREF     is the name of the reference frame relative to which */
/*                the state of interest is specified. */

/*                FIXREF must be centered on the target body. */

/*                Case, leading and trailing blanks are not significant */
/*                in the string FIXREF. */


/*     ABCORR     indicates the aberration corrections to be applied to */
/*                the state of the target body to account for one-way */
/*                light time and stellar aberration. The orientation */
/*                of the target body will also be corrected for one-way */
/*                light time when light time corrections are requested. */

/*                Supported aberration correction options for */
/*                observation (case where radiation is received by */
/*                observer at ET) are: */

/*                   NONE           No correction. */
/*                   LT             Light time only. */
/*                   LT+S           Light time and stellar aberration. */
/*                   CN             Converged Newtonian (CN) light time. */
/*                   CN+S           CN light time and stellar aberration. */

/*                Supported aberration correction options for */
/*                transmission (case where radiation is emitted from */
/*                observer at ET) are: */

/*                   XLT            Light time only. */
/*                   XLT+S          Light time and stellar aberration. */
/*                   XCN            Converged Newtonian (CN) light time. */
/*                   XCN+S          CN light time and stellar aberration. */

/*                For detailed information, see the geometry finder */
/*                required reading, gf.req.  Also see the header of */
/*                SPKEZR, which contains a detailed discussion of */
/*                aberration corrections. */

/*                Case, leading and trailing blanks are not significant */
/*                in the string ABCORR. */


/*     OBSID      is the NAIF ID code of the observer. */


/*     DREF        is the name of the reference frame relative to which */
/*                 a ray's direction vector is expressed. This may be */
/*                 any frame supported by the SPICE system, including */
/*                 built-in frames (documented in the Frames Required */
/*                 Reading) and frames defined by a loaded frame kernel */
/*                 (FK). The string DREF is case-insensitive, and */
/*                 leading and trailing blanks in DREF are not */
/*                 significant. */

/*                 When DREF designates a non-inertial frame, the */
/*                 orientation of the frame is evaluated at an epoch */
/*                 dependent on the frame's center and, if the center is */
/*                 not the observer, on the selected aberration */
/*                 correction. See the description of the direction */
/*                 vector DVEC for details. */


/*     DCTR        is the NAIF ID code of the body at which the frame */
/*                 designated by DREF is centered. While DCTR can */
/*                 be obtained from the FRAMEX system, passing in */
/*                 the ID code is more efficient. DCTR should be looked */
/*                 up by the coordinate search utility initialization */
/*                 routine before a search is performed. */


/*     DVEC        Ray direction vector emanating from the observer. The */
/*                 intercept with the target body's surface of the ray */
/*                 defined by the observer and DVEC is sought. */

/*                 DVEC is specified relative to the reference frame */
/*                 designated by DREF. */

/*                 Non-inertial reference frames are treated as follows: */
/*                 if the center of the frame is at the observer's */
/*                 location, the frame is evaluated at ET. If the */
/*                 frame's center is located elsewhere, then letting */
/*                 LTCENT be the one-way light time between the observer */
/*                 and the central body associated with the frame, the */
/*                 orientation of the frame is evaluated at ET-LTCENT, */
/*                 ET+LTCENT, or ET depending on whether the requested */
/*                 aberration correction is, respectively, for received */
/*                 radiation, transmitted radiation, or is omitted. */
/*                 LTCENT is computed using the method indicated by */
/*                 ABCORR. */


/*     RADII      is an array containing three radii defining */
/*                a reference ellipsoid for the target body. */

/* $ Detailed_Output */

/*     STATE     is the state of the surface intercept point at ET. */
/*               The first three components of STATE contain the */
/*               surface intercept point itself; the last three */
/*               components contain the derivative with respect to */
/*               time of the intercept. The state is expressed */
/*               relative to the body-fixed frame designated by */
/*               FIXREF. */

/*               Units are km and km/s. */

/*               STATE is defined if and only if the output flag FOUND */
/*               is set to .TRUE. */


/*     FOUND     is a logical flag indicating whether the requested */
/*               state was found. FOUND is set to .TRUE. if and only */
/*               if */

/*                  - the surface intercept exists */

/*                  - the velocity of the surface intercept */
/*                    is computable */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the aberration correction ABCORR is not recognized, */
/*         the error will be diagnosed by routines in the call tree */
/*         of this routine. */

/*     2)  If the frame FIXREF is not recognized by the frames */
/*         subsystem, the error will be diagnosed by routines in the */
/*         call tree of this routine. */

/*     3)  FIXREF must be centered on the target body; if it isn't, */
/*         the error will be diagnosed by routines in the call tree */
/*         of this routine. */

/*     4)  Any error that occurs while look up the state of the target */
/*         or observer will be diagnosed by routines in the call tree of */
/*         this routine. */

/*     5)  Any error that occurs while look up the orientation of */
/*         the target will be diagnosed by routines in the call tree of */
/*         this routine. */

/*     6)  If the input method is not recognized, the error */
/*         SPICE(NOTSUPPORTED) will be signaled. */

/*     7)  The input ray direction frame center DCTR must be compatible */
/*         with the ray direction frame DREF. This routine *does not */
/*         check* the validity of DCTR. */

/*     8)  If the input ray's direction vector is the zero vector, the */
/*         error will be diagnosed by a routine in the call tree of this */
/*         routine. */

/* $ Files */

/*     Appropriate kernels must be loaded by the calling program before */
/*     this routine is called. */

/*     The following data are required: */

/*        - SPK data: ephemeris data for target and observer must be */
/*          loaded. If aberration corrections are used, the states of */
/*          target and observer relative to the solar system barycenter */
/*          must be calculable from the available ephemeris data. */
/*          Typically ephemeris data are made available by loading one */
/*          or more SPK files via FURNSH. */

/*        - PCK data: if the target body shape is modeled as an */
/*          ellipsoid, triaxial radii for the target body must be loaded */
/*          into the kernel pool. Typically this is done by loading a */
/*          text PCK file via FURNSH. */

/*        - Further PCK data: rotation data for the target body must be */
/*          loaded. These may be provided in a text or binary PCK file. */

/*        - Frame data: if a frame definition is required to convert the */
/*          observer and target states to the body-fixed frame of the */
/*          target, that definition must be available in the kernel */
/*          pool. Typically the definition is supplied by loading a */
/*          frame kernel via FURNSH. */

/*     In all cases, kernel data are normally loaded once per program */
/*     run, NOT every time this routine is called. */

/* $ Particulars */

/*     This routine isolates the computation of the surface intercept */
/*     state (that is, the surface intercept point and its derivative */
/*     with respect to time). */

/*     This routine is used by the GF coordinate utility routines in */
/*     order to solve for time windows on which specified mathematical */
/*     conditions involving coordinates are satisfied. The role of */
/*     this routine is to provide Cartesian state vectors enabling */
/*     the GF coordinate utilities to determine the signs of the */
/*     derivatives with respect to time of coordinates of interest. */

/* $ Examples */

/*     See ZZGFCOST. */

/* $ Restrictions */

/*     1)  This routine is restricted to use with ellipsoidal target */
/*         shape models. */

/*     2)  The computations performed by this routine are intended */
/*         to be compatible with those performed by the SPICE */
/*         routine SUBPNT. If that routine changes, this routine */
/*         may need to be updated. */

/*     3)  This routine presumes that error checking of inputs */
/*         has, where possible, already been performed by the */
/*         GF coordinate utility initialization routine. */

/*     4)  The interface and functionality of this set of routines may */
/*         change without notice. These routines should be called only */
/*         by SPICELIB routines. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman   (JPL) */

/* $ Version */

/* -    SPICELIB Version 2.2.0 09-OCT-2021 (NJB) */

/*        Bug fix: now returns after detecting bad aberration */
/*        correction string. */

/*        Bug fix: aberration correction strings are now parsed using */
/*        ZZVALCOR. This improves error checking. */

/* -    SPICELIB Version 2.1.0 08-MAR-2017 (NJB) */

/*        Bug fix: now returns after SINCPT call if FOUND is .FALSE. */

/* -    SPICELIB Version 2.0.0 12-MAY-2009 (NJB) */

/*        Upgraded to support targets and observers having */
/*        no names associated with their ID codes. */

/* -    SPICELIB Version 1.0.0 05-MAR-2009 (NJB) */

/* -& */
/* $ Index_Entries */

/*     surface intercept state */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     Local variables */


/*     Saved variables */


/*     Initial values */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }
    chkin_("ZZGFSSIN", (ftnlen)8);

/*     No result has been found. */

    *found = FALSE_;
    if (first || *trgid != prvtrg) {
	bodc2s_(trgid, svtarg, (ftnlen)36);
	prvtrg = *trgid;
    }
    if (first || *obsid != prvobs) {
	bodc2s_(obsid, svobs, (ftnlen)36);
	prvobs = *obsid;
    }
    first = FALSE_;

/*     Parse the aberration correction specifier. */

    zzvalcor_(abcorr, attblk, abcorr_len);
    if (failed_()) {
	chkout_("ZZGFSSIN", (ftnlen)8);
	return 0;
    }
    geom = attblk[0];
    uselt = attblk[1];
    usestl = attblk[2];
    xmit = attblk[4];

/*     Set the sign associated with the light time correction. */

    if (xmit) {
	ltsign = 1.;
    } else {
	ltsign = -1.;
    }

/*     Decide whether the surface intercept point is computed using */
/*     the "near point" or "surface intercept" method. Only */
/*     ellipsoids may be used a shape models for this computation. */

    if (! eqstr_(method, "Ellipsoid", method_len, (ftnlen)9)) {
	setmsg_("Surface intercept point computation method # is not support"
		"ed by this routine.", (ftnlen)78);
	errch_("#", method, (ftnlen)1, method_len);
	sigerr_("SPICE(NOTSUPPORTED)", (ftnlen)19);
	chkout_("ZZGFSSIN", (ftnlen)8);
	return 0;
    }
    if (geom) {

/*        This is the geometric case. */

/*        No light time correction is involved, so all frames are */
/*        evaluated at the observation epoch. */

/*        Compute the state transformation from DREF to J2000. */

	sxform_(dref, "J2000", et, dcorxf, dref_len, (ftnlen)5);

/*        Transform the ray's direction vector from DREF to the J2000 */
/*        frame. The velocity of DVEC in frame DREF is zero. */

	moved_(dvec, &c__3, stemp);
	cleard_(&c__3, &stemp[3]);
	mxvg_(dcorxf, stemp, &c__6, &c__6, j2dsta);

/*        We need to check the body-fixed reference frame here. */

	namfrm_(fixref, &frcode, fixref_len);
	frinfo_(&frcode, &center, &frclss, &clssid, &fnd);
	if (failed_()) {
	    chkout_("ZZGFSSIN", (ftnlen)8);
	    return 0;
	}
	if (! fnd) {
	    setmsg_("Input reference frame # was not recognized.", (ftnlen)43)
		    ;
	    errch_("#", fixref, (ftnlen)1, fixref_len);
	    sigerr_("SPICE(NOFRAME)", (ftnlen)14);
	    chkout_("ZZGFSSIN", (ftnlen)8);
	    return 0;
	}
	if (center != *trgid) {
	    setmsg_("Input reference frame # is centered on body # instead o"
		    "f body #.", (ftnlen)64);
	    errch_("#", fixref, (ftnlen)1, fixref_len);
	    errint_("#", &center, (ftnlen)1);
	    errint_("#", trgid, (ftnlen)1);
	    sigerr_("SPICE(INVALIDFRAME)", (ftnlen)19);
	    chkout_("ZZGFSSIN", (ftnlen)8);
	    return 0;
	}
/*        Get the state of the target with respect to the observer, */
/*        expressed relative to the target body-fixed frame. We don't */
/*        need to propagate states to the solar system barycenter in */
/*        this case. */

	spkgeo_(trgid, et, fixref, obsid, fxtsta, &lt, fixref_len);
	if (failed_()) {
	    chkout_("ZZGFSSIN", (ftnlen)8);
	    return 0;
	}

/*        Compute the state of the observer with respect to the target */
/*        in the body-fixed frame. */

	vminug_(fxtsta, &c__6, fxosta);

/*        Transform the state of the direction vector from the J2000 */
/*        frame to the target body-fixed frame at TRGEPC. Since no */
/*        light time corrections are involved, the state transformation */
/*        matrix from SXFORM works just fine. */

	sxform_("J2000", fixref, et, xform, (ftnlen)5, fixref_len);
	mxvg_(xform, j2dsta, &c__6, &c__6, fxdsta);

/*        Now we can obtain the surface velocity of the surface intercept */
/*        point. */

	surfpv_(fxosta, fxdsta, radii, &radii[1], &radii[2], fxpsta, found);

/*        It's not an error for SURFPV to be unable to compute an */
/*        intercept state; return now if the state was not */
/*        computable. */

	if (! (*found)) {
	    chkout_("ZZGFSSIN", (ftnlen)8);
	    return 0;
	}
    } else if (uselt) {

/*        Light time and possibly stellar aberration corrections */
/*        are applied. */

/*        Compute the state transformation from DREF to J2000. */

	if (*obsid == *dctr) {

/*           DREF is centered on the observer, so there's no light time */
/*           correction. */

	    sxform_(dref, "J2000", et, dcorxf, dref_len, (ftnlen)5);
	} else {

/*           Find the epoch DRFEPC associated with the input direction */
/*           vector's reference frame DREF. We use SPK rules for */
/*           determining the epoch, just as in SINCPT. Let DLTCTR be the */
/*           rate of change of light time between the frame center and */
/*           the observer. */


/*           Find the light time from the observer to the center of */
/*           frame DREF. */

	    spkacs_(dctr, et, "J2000", abcorr, obsid, ctrsta, &ltctr, &dltctr,
		     (ftnlen)5, abcorr_len);
	    if (failed_()) {
		chkout_("ZZGFSSIN", (ftnlen)8);
		return 0;
	    }
	    drfepc = *et + ltsign * ltctr;

/*           Compute the state of the input direction vector in the */
/*           J2000 frame at DRFEPC. Correct the state transformation for */
/*           the rate of change of light time. */

	    sxform_(dref, "J2000", &drfepc, drxfrm, dref_len, (ftnlen)5);
	    zzcorsxf_(&xmit, &dltctr, drxfrm, dcorxf);
	}

/*        The velocity of DVEC in frame DREF is zero. */

	moved_(dvec, &c__3, stemp);
	cleard_(&c__3, &stemp[3]);
	mxvg_(dcorxf, stemp, &c__6, &c__6, j2dsta);

/*        We'll transform J2DSTA to the target body-fixed frame at */
/*        the target epoch once we've computed the required */
/*        state transformation matrix. This occurs just before */
/*        we use this state vector in a call to SURFPV. */

/*        Most our work consists of getting ready to call the SPICELIB */
/*        routine SURFPV. In order to make this call, we'll need the */
/*        velocity of the observer relative to the target body's center */
/*        in the target body-fixed frame. We must evaluate the rotation */
/*        state of the target at the correct epoch, and account for the */
/*        rate of change of light time, if light time corrections are */
/*        used. The algorithm we use depends on the algorithm used in */
/*        SINCPT, since we're computing the derivative with respect to */
/*        time of the solution found by that routine. */

/*        In this algorithm, we must take into account the fact that */
/*        SINCPT performs light time and stellar aberration corrections */
/*        for the surface intercept point, not for the center of the */
/*        target body. */

/*        If light time and stellar aberration corrections are used, */

/*        - Find the aberration corrected surface intercept point and */
/*          the light time-corrected epoch TRGEPC associated */
/*          with the surface intercept point. */

/*        - Use TRGEPC to find the position of the target relative */
/*          to the solar system barycenter. */

/*        - Use TRGEPC to find the orientation of the target relative */
/*          to the J2000 reference frame. */

/*        - Find the light-time corrected position of the */
/*          surface intercept point; use this to compute the */
/*          stellar aberration offset that applies to the */
/*          surface intercept point, as well as the velocity of */
/*          this offset. */

/*        - Find the corrected state of the target center as seen */
/*          from the observer, where the corrections are those */
/*          applicable to the surface intercept point. */

/*        - Negate the corrected target center state to obtain */
/*          the state of the observer relative to the target. */

/*        - Express the state of the observer relative to the */
/*          target in the target body fixed frame at TRGEPC. */


/*        Below, we'll use the convention that vectors expressed */
/*        relative to the body-fixed frame have names of the form */

/*        FX* */

/*        Note that SINCPT will signal an error if FIXREF is not */
/*        actually centered on the target body. */

	sincpt_(method, svtarg, et, fixref, abcorr, svobs, dref, dvec, spoint,
		 &trgepc, srfvec, found, method_len, (ftnlen)36, fixref_len, 
		abcorr_len, (ftnlen)36, dref_len);

/*        It's not an error for SINCPT to be unable to compute an */
/*        intercept point; return now if the intercept was not found. */

	if (! (*found)) {
	    chkout_("ZZGFSSIN", (ftnlen)8);
	    return 0;
	}

/*        Get J2000-relative states of observer and target with respect */
/*        to the solar system barycenter at their respective epochs of */
/*        participation. */

	spkssb_(obsid, et, "J2000", ssbobs, (ftnlen)5);
	spkssb_(trgid, &trgepc, "J2000", ssbtg0, (ftnlen)5);

/*        Get the uncorrected J2000 to body-fixed to state */
/*        transformation at TRGEPC. */

	sxform_("J2000", fixref, &trgepc, xform, (ftnlen)5, fixref_len);
	if (failed_()) {
	    chkout_("ZZGFSSIN", (ftnlen)8);
	    return 0;
	}

/*        Initialize the state of the surface intercept point in the */
/*        body-fixed frame. At this point we don't know the point's */
/*        velocity; set it to zero. */

	moved_(spoint, &c__3, fxpsta);
	cleard_(&c__3, &fxpsta[3]);
	if (usestl) {

/*           We're going to need the acceleration of the observer */
/*           relative to the SSB. Compute this now. */

	    for (i__ = 1; i__ <= 2; ++i__) {

/*              The epoch is ET -/+ TDELTA. */

		t = *et + ((i__ << 1) - 3) * 1.;
		spkssb_(obsid, &t, "J2000", &obssta[(i__1 = i__ * 6 - 6) < 12 
			&& 0 <= i__1 ? i__1 : s_rnge("obssta", i__1, "zzgfss"
			"in_", (ftnlen)806)], (ftnlen)5);
	    }
	    if (failed_()) {
		chkout_("ZZGFSSIN", (ftnlen)8);
		return 0;
	    }

/*           Compute the observer's acceleration using a quadratic */
/*           approximation. */

	    qderiv_(&c__3, &obssta[3], &obssta[9], &c_b51, acc);
	}

/*        The rest of the algorithm is iterative. On the first */
/*        iteration, we don't have a good estimate of the velocity */
/*        of the surface intercept point relative to the body-fixed */
/*        frame. Since we're using this velocity as an input */
/*        to the aberration velocity computations, we */
/*        expect that treating this velocity as zero on the first */
/*        pass yields a reasonable estimate. On the second pass, */
/*        we'll use the velocity derived on the first pass. */

	cleard_(&c__3, fxpvel);

/*        We'll also estimate the rate of change of light time */
/*        as zero on the first pass. */

	dlt = 0.;
	for (i__ = 1; i__ <= 3; ++i__) {

/*           Correct the target's velocity for the rate of */
/*           change of light time. */

	    if (xmit) {
		scale = dlt + 1.;
	    } else {
		scale = 1. - dlt;
	    }

/*           Scale the velocity portion of the target state to */
/*           correct the velocity for the rate of change of light */
/*           time. */

	    moved_(ssbtg0, &c__3, ssbtrg);
	    vscl_(&scale, &ssbtg0[3], &ssbtrg[3]);

/*           Get the state of the target with respect to the observer. */

	    vsubg_(ssbtrg, ssbobs, &c__6, obstrg);

/*           Correct the J2000 to body-fixed state transformation matrix */
/*           for the rate of change of light time. */

	    zzcorsxf_(&xmit, &dlt, xform, corxfm);

/*           Invert CORXFM to obtain the corrected */
/*           body-fixed to J2000 state transformation. */

	    invstm_(corxfm, corxfi);

/*           Convert the surface intercept point state to the J2000 */
/*           frame. */

	    mxvg_(corxfi, fxpsta, &c__6, &c__6, pntsta);

/*           Find the J2000-relative state of the surface intercept */
/*           point with respect to the target. */

	    vaddg_(obstrg, pntsta, &c__6, obspnt);
	    if (usestl) {

/*              Now compute the stellar aberration correction */
/*              applicable to OBSPNT. We need the velocity of */
/*              this correction as well. */

		zzstelab_(&xmit, acc, &ssbobs[3], obspnt, sa, savel);
		moved_(sa, &c__3, sastat);
		moved_(savel, &c__3, &sastat[3]);

/*              Adding the stellar aberration state to the target center */
/*              state gives us the state of the target center with */
/*              respect to the observer, corrected for the aberrations */
/*              applicable to the surface intercept point. */

		vaddg_(obstrg, sastat, &c__6, stemp);
	    } else {
		moved_(obstrg, &c__6, stemp);
	    }

/*           Convert STEMP to the body-fixed reference frame. */

	    mxvg_(corxfm, stemp, &c__6, &c__6, fxtsta);

/*           At long last, compute the state of the observer */
/*           with respect to the target in the body-fixed frame. */

	    vminug_(fxtsta, &c__6, fxosta);

/*           Transform the state of the direction vector from the */
/*           J2000 frame to the target body-fixed frame at TRGEPC. */

	    mxvg_(corxfm, j2dsta, &c__6, &c__6, fxdsta);

/*           Now we can obtain the surface velocity of the */
/*           surface intercept point. */

	    surfpv_(fxosta, fxdsta, radii, &radii[1], &radii[2], fxpsta, 
		    found);

/*           It's not an error for SURFPV to be unable to compute an */
/*           intercept state; return now if the state was not */
/*           computable. */

	    if (! (*found)) {
		chkout_("ZZGFSSIN", (ftnlen)8);
		return 0;
	    }

/*           At this point we can update the surface point */
/*           velocity and light time derivative estimates. */

/*           In order to compute the light time rate, we'll */
/*           need the J2000-relative velocity of the surface intercept */
/*           point with respect to the observer. First convert */
/*           the surface intercept state to the J2000 frame, then */
/*           add the result to the state of the target center */
/*           with respect to the observer. */

	    mxvg_(corxfi, fxpsta, &c__6, &c__6, pntsta);
	    vaddg_(obstrg, pntsta, &c__6, obspnt);

/*           Now that we have an improved estimate of the */
/*           surface intercept state, we can estimate the rate of */
/*           change of light time as */

/*              range rate */
/*              ---------- */
/*                  c */


/*           If we're correcting for stellar aberration, *ideally* we */
/*           should remove that correction now, since the light time */
/*           rate is based on light time between the observer and the */
/*           light-time corrected surface intercept point. But the error */
/*           made by including stellar aberration is too small to make */
/*           it worthwhile to make this adjustment. */

	    vhat_(obspnt, upos);
	    dlt = vdot_(&obspnt[3], upos) / clight_();

/*           With FXPVEL and DLT updated, we'll repeat our */
/*           computations. */

	}
    } else {

/*        We should never get here. */

	setmsg_("Aberration correction # was not recognized.", (ftnlen)43);
	errch_("#", abcorr, (ftnlen)1, abcorr_len);
	sigerr_("SPICE(NOTSUPPORTED)", (ftnlen)19);
	chkout_("ZZGFSSIN", (ftnlen)8);
	return 0;
    }

/*     Copy the computed state to the output argument STATE. */
/*     FOUND has already been set to .TRUE. by SURFPV. */

    moved_(fxpsta, &c__6, state);
    chkout_("ZZGFSSIN", (ftnlen)8);
    return 0;
} /* zzgfssin_ */

