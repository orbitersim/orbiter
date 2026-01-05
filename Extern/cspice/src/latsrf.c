/* latsrf.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__100 = 100;
static integer c__3 = 3;
static doublereal c_b43 = 1.;

/* $Procedure LATSRF ( Latitudinal grid to surface points ) */
/* Subroutine */ int latsrf_(char *method, char *target, doublereal *et, char 
	*fixref, integer *npts, doublereal *lonlat, doublereal *srfpts, 
	ftnlen method_len, ftnlen target_len, ftnlen fixref_len)
{
    /* Initialized data */

    static logical first = TRUE_;
    static char prvmth[500] = "                                             "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "       ";
    static integer svprvt = 0;

    /* System generated locals */
    integer i__1;
    doublereal d__1;

    /* Builtin functions */
    integer s_cmp(char *, char *, ftnlen, ftnlen);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    extern /* Subroutine */ int zzbods2c_(integer *, char *, integer *, 
	    logical *, char *, integer *, logical *, ftnlen, ftnlen);
    extern doublereal vdot_(doublereal *, doublereal *);
    extern /* Subroutine */ int zzbodvcd_(integer *, char *, integer *, 
	    integer *, integer *, doublereal *, ftnlen), zzmaxrad_(doublereal 
	    *), zznamfrm_(integer *, char *, integer *, char *, integer *, 
	    ftnlen, ftnlen), zzsudski_(integer *, integer *, integer *, 
	    integer *), zzctruin_(integer *);
    integer i__;
    extern /* Subroutine */ int zzprsmet_(integer *, char *, integer *, char *
	    , char *, logical *, integer *, integer *, char *, char *, ftnlen,
	     ftnlen, ftnlen, ftnlen, ftnlen);
    integer n;
    extern /* Subroutine */ int zzsrftrk_(integer *, logical *), zzraysfx_(
	    doublereal *, doublereal *, doublereal *, doublereal *, logical *)
	    ;
    doublereal r__, x[3];
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    static integer shape;
    extern /* Subroutine */ int errch_(char *, char *, ftnlen, ftnlen), 
	    edpnt_(doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *), errdp_(char *, doublereal *, ftnlen);
    static integer nsurf;
    extern logical eqstr_(char *, char *, ftnlen, ftnlen);
    static logical svfnd1;
    static integer svctr1[2], svctr2[2];
    extern logical failed_(void);
    static integer svctr3[2];
    integer trgcde, fixfid, fxclid;
    extern /* Subroutine */ int latrec_(doublereal *, doublereal *, 
	    doublereal *, doublereal *);
    extern logical return_(void);
    char lmbtyp[20], shpstr[9], subtyp[20];
    doublereal raydir[3];
    integer fxcent;
    char trmtyp[20];
    integer fxclss;
    static integer srfctr[2], srflst[100];
    logical surfup;
    static char svtarg[36];
    static integer svtcde;
    static char svfref[32];
    static integer svfxfc;
    static doublereal svradi[3];
    extern /* Subroutine */ int setmsg_(char *, ftnlen), sigerr_(char *, 
	    ftnlen), chkout_(char *, ftnlen), frinfo_(integer *, integer *, 
	    integer *, integer *, logical *), errint_(char *, integer *, 
	    ftnlen), vminus_(doublereal *, doublereal *);
    logical fnd;
    extern doublereal dpr_(void);
    static logical pri;

/* $ Abstract */

/*     Map array of planetocentric longitude/latitude coordinate pairs */
/*     to surface points on a specified target body. */

/*     The surface of the target body may be represented by a triaxial */
/*     ellipsoid or by topographic data provided by DSK files. */

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
/*     FRAMES */
/*     PCK */
/*     SPK */
/*     TIME */

/* $ Keywords */

/*     COORDINATES */
/*     DSK */
/*     GEOMETRY */
/*     SURFACE */

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

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     METHOD     I   Computation method. */
/*     TARGET     I   Name of target body. */
/*     ET         I   Epoch in TDB seconds past J2000 TDB. */
/*     FIXREF     I   Body-fixed, body-centered target body frame. */
/*     NPTS       I   Number of coordinate pairs in input array. */
/*     LONLAT     I   Array of longitude/latitude coordinate pairs. */
/*     SRFPTS     O   Array of surface points. */

/* $ Detailed_Input */

/*     METHOD   is a short string providing parameters defining */
/*              the computation method to be used. In the syntax */
/*              descriptions below, items delimited by brackets */
/*              are optional. */

/*              METHOD may be assigned the following values: */

/*                 'ELLIPSOID' */

/*                    The surface point computation uses a triaxial */
/*                    ellipsoid to model the surface of the target */
/*                    body. The ellipsoid's radii must be available */
/*                    in the kernel pool. */

/*                 'DSK/UNPRIORITIZED[/SURFACES = <surface list>]' */

/*                    The surface point computation uses topographic */
/*                    data to model the surface of the target body. */
/*                    These data must be provided by loaded DSK */
/*                    files. */

/*                    The surface list specification is optional. The */
/*                    syntax of the list is */

/*                       <surface 1> [, <surface 2>...] */

/*                    If present, it indicates that data only for the */
/*                    listed surfaces are to be used; however, data */
/*                    need not be available for all surfaces in the */
/*                    list. If absent, loaded DSK data for any surface */
/*                    associated with the target body are used. */

/*                    The surface list may contain surface names or */
/*                    surface ID codes. Names containing blanks must */
/*                    be delimited by double quotes, for example */

/*                       SURFACES = "Mars MEGDR 128 PIXEL/DEG" */

/*                    If multiple surfaces are specified, their names */
/*                    or IDs must be separated by commas. */

/*                    See the $Particulars section below for details */
/*                    concerning use of DSK data. */


/*              Neither case nor white space are significant in */
/*              METHOD, except within double-quoted strings. For */
/*              example, the string ' eLLipsoid ' is valid. */

/*              Within double-quoted strings, blank characters are */
/*              significant, but multiple consecutive blanks are */
/*              considered equivalent to a single blank. Case is */
/*              not significant. So */

/*                 "Mars MEGDR 128 PIXEL/DEG" */

/*              is equivalent to */

/*                 " mars megdr  128  pixel/deg " */

/*              but not to */

/*                 "MARS MEGDR128PIXEL/DEG" */


/*     TARGET   is the name of the target body. TARGET is */
/*              case-insensitive, and leading and trailing blanks in */
/*              TARGET are not significant. Optionally, you may */
/*              supply a string containing the integer ID code for */
/*              the object. For example both 'MOON' and '301' are */
/*              legitimate strings that indicate the Moon is the */
/*              target body. */

/*              When the target body's surface is represented by a */
/*              tri-axial ellipsoid, this routine assumes that a */
/*              kernel variable representing the ellipsoid's radii is */
/*              present in the kernel pool. Normally the kernel */
/*              variable would be defined by loading a PCK file. */


/*     ET       is the epoch for which target surface data will be */
/*              selected, if the surface is modeled using DSK data. */
/*              In this case, only segments having time coverage that */
/*              includes the epoch ET will be used. */

/*              ET is ignored if the target is modeled as an */
/*              ellipsoid. */

/*              ET is expressed as TDB seconds past J2000 TDB. */


/*     FIXREF   is the name of a body-fixed reference frame centered */
/*              on the target body. */

/*              If the target shape is given by DSK data, FIXREF may */
/*              designate any such frame supported by the SPICE */
/*              system, including built-in frames (documented in the */
/*              Frames Required Reading) and frames defined by a */
/*              loaded frame kernel (FK). */

/*              When the target surface is modeled as an ellipsoid, */
/*              the reference frame designated by FIXREF (described */
/*              below) must have its coordinate axes aligned with the */
/*              respective principal axes of the reference ellipsoid. */

/*              The string FIXREF is case-insensitive, and leading */
/*              and trailing blanks in FIXREF are not significant. */

/*              The output surface points in the array SRFPTS will be */
/*              expressed relative to this reference frame. */


/*     NPTS     is the number of coordinate pairs in the array LONLAT. */


/*     LONLAT   is an array of pairs of planetocentric longitudes and */
/*              latitudes of surface points. Elements */

/*                 LONLAT(1,I) */
/*                 LONLAT(2,I) */

/*              are, respectively, the planetocentric longitude and */
/*              latitude of the Ith surface point. */

/*              The units of longitude and latitude are radians. */

/* $ Detailed_Output */

/*     SRFPTS   is an array of target body surface points */
/*              corresponding to the pairs of coordinates in the */
/*              input LONLAT array. Elements */

/*                 SRFPTS(1,I) */
/*                 SRFPTS(2,I) */
/*                 SRFPTS(3,I) */

/*              are the Cartesian coordinates, expressed in the */
/*              reference frame designated by FIXREF, of the surface */
/*              point corresponding to the Ith pair of input */
/*              coordinates. */

/*              If there are multiple solutions for a given input */
/*              coordinate pair, this routine will return the point */
/*              at those coordinates having the greatest distance */
/*              from the origin of the coordinate system. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the target body name input string cannot be converted to an */
/*         integer ID code, the error SPICE(IDCODENOTFOUND) is signaled. */

/*     2)  If the input target body-fixed frame FIXREF is not */
/*         recognized, the error SPICE(NOFRAME) is signaled. A frame */
/*         name may fail to be recognized because a required frame */
/*         specification kernel has not been loaded; another cause is a */
/*         misspelling of the frame name. */

/*     3)  If the input frame FIXREF is not centered at the target body, */
/*         the error SPICE(INVALIDFRAME) is signaled. */

/*     4)  If data are not available to convert between the frame */
/*         FIXREF and the frame of a DSK segment of interest, an error */
/*         is signaled by a routine in the call tree of this */
/*         routine. */

/*     5)  If the input argument METHOD cannot be parsed, an error */
/*         is signaled by either this routine or a routine in */
/*         the call tree of this routine. */

/*     6)  If the computation method specifies an ellipsoidal target */
/*         model, and if triaxial radii of the target body have not been */
/*         loaded into the kernel pool prior to calling LATSRF, an error */
/*         is signaled by a routine in the call tree of this routine. */

/*     7)  If the computation method specifies an ellipsoidal target */
/*         model, and if any of the radii of the target body are */
/*         non-positive, an error is signaled by a routine in the call */
/*         tree of this routine. The target must be an extended body. */

/*     8)  If METHOD specifies that the target surface is represented by */
/*         DSK data, and no DSK files are loaded for the specified */
/*         target, an error is signaled by a routine in the call tree */
/*         of this routine. */

/*     9)  If METHOD specifies that the target surface is represented */
/*         by DSK data, and data representing the portion of the surface */
/*         corresponding to the coordinates provided in LONLAT are not */
/*         available, an error is signaled by a routine in the call */
/*         tree of this routine. */

/*     10) If a surface point cannot be computed because the ray */
/*         corresponding to a longitude/latitude pair fails to intersect */
/*         the target surface as defined by the plate model, the error */
/*         SPICE(NOINTERCEPT) is signaled. */

/*     11) If the surface point corresponding to a longitude/latitude */
/*         pair in LONLAT does not have matching longitude and latitude */
/*         (because it is on the opposite side of the origin), the error */
/*         SPICE(SHAPENOTSUPPORTED) is signaled. */

/*     12) If the radii are not available in the kernel pool, an error is */
/*         signaled by a routine in the call tree of this routine. */

/*     13) If the target shape is "ellipsoid" and not all radii of the */
/*         ellipsoid are strictly positive, the error */
/*         SPICE(BADAXISLENGTH) is signaled. */

/* $ Files */

/*     Appropriate kernels must be loaded by the calling program before */
/*     this routine is called. */

/*     The following data are required: */

/*     -  Shape data for the target body: */

/*           PCK data: */

/*              If the target shape is modeled as an ellipsoid, */
/*              triaxial radii for the target body must be loaded into */
/*              the kernel pool. Typically this is done by loading a */
/*              text PCK file via FURNSH. */

/*           DSK data: */

/*              If the target shape is modeled by DSK data, DSK files */
/*              containing topographic data for the target body must be */
/*              loaded. If a surface list is specified, data for at */
/*              least one of the listed surfaces must be loaded. */

/*     -  Target body orientation data: these may be provided in a */
/*        text or binary PCK file. In some cases, target body */
/*        orientation may be provided by one more more CK files. In */
/*        either case, data are made available by loading the files */
/*        via FURNSH. */

/*     The following data may be required: */

/*     -  Frame data: if a frame definition is required to convert */
/*        between the body-fixed frame of the target and the frame of */
/*        a DSK segment providing topographic data, that definition */
/*        must be available in the kernel pool. Typically the */
/*        definition is supplied by loading a frame kernel via FURNSH. */

/*     -  Surface name-ID associations: if surface names are specified */
/*        in METHOD, the association of these names with their */
/*        corresponding surface ID codes must be established by */
/*        assignments of the kernel variables */

/*           NAIF_SURFACE_NAME */
/*           NAIF_SURFACE_CODE */
/*           NAIF_SURFACE_BODY */

/*        Normally these associations are made by loading a text */
/*        kernel containing the necessary assignments. An example of */
/*        such a set of assignments is */

/*           NAIF_SURFACE_NAME += 'Mars MEGDR 128 PIXEL/DEG' */
/*           NAIF_SURFACE_CODE += 1 */
/*           NAIF_SURFACE_BODY += 499 */

/*     -  SCLK data: if the target body's orientation is provided by */
/*        CK files, an associated SCLK kernel must be loaded. */

/*     In all cases, kernel data are normally loaded once per program */
/*     run, NOT every time this routine is called. */

/* $ Particulars */

/*     This routine is intended to be used for target body surfaces that */
/*     have a unique radius for each pair of planetocentric longitude */
/*     and latitude coordinates. */

/*     If the target surface is represented by topographic data, it is */
/*     possible for there to be multiple surface points at a given */
/*     planetocentric longitude and latitude. For example, this can */
/*     occur if the surface has features such as cliffs, caves, or */
/*     arches. */

/*     For more complex surfaces, the routine */

/*        DSKSXV {DSK, ray-surface intercept, vectorized} */

/*     may be more suitable. That routine works with rays having vertices */
/*     anywhere outside of the target body. */


/*     Planetocentric coordinates */
/*     ========================== */

/*     Planetocentric longitude and latitude are defined as follows: */

/*        Longitude of a point P is the angle between the prime meridian */
/*        and the meridian containing P. The direction of increasing */
/*        longitude is from the +X axis towards the +Y axis. */

/*        Latitude of a point P is the angle from the XY plane of the */
/*        ray from the origin through the point. */


/*     Using DSK data */
/*     ============== */

/*        DSK loading and unloading */
/*        ------------------------- */

/*        DSK files providing data used by this routine are loaded by */
/*        calling FURNSH and can be unloaded by calling UNLOAD or */
/*        KCLEAR. See the documentation of FURNSH for limits on numbers */
/*        of loaded DSK files. */

/*        For run-time efficiency, it's desirable to avoid frequent */
/*        loading and unloading of DSK files. When there is a reason to */
/*        use multiple versions of data for a given target body---for */
/*        example, if topographic data at varying resolutions are to be */
/*        used---the surface list can be used to select DSK data to be */
/*        used for a given computation. It is not necessary to unload */
/*        the data that are not to be used. This recommendation presumes */
/*        that DSKs containing different versions of surface data for a */
/*        given body have different surface ID codes. */


/*        DSK data priority */
/*        ----------------- */

/*        A DSK coverage overlap occurs when two segments in loaded DSK */
/*        files cover part or all of the same domain---for example, a */
/*        given longitude-latitude rectangle---and when the time */
/*        intervals of the segments overlap as well. */

/*        When DSK data selection is prioritized, in case of a coverage */
/*        overlap, if the two competing segments are in different DSK */
/*        files, the segment in the DSK file loaded last takes */
/*        precedence. If the two segments are in the same file, the */
/*        segment located closer to the end of the file takes */
/*        precedence. */

/*        When DSK data selection is unprioritized, data from competing */
/*        segments are combined. For example, if two competing segments */
/*        both represent a surface as sets of triangular plates, the */
/*        union of those sets of plates is considered to represent the */
/*        surface. */

/*        Currently only unprioritized data selection is supported. */
/*        Because prioritized data selection may be the default behavior */
/*        in a later version of the routine, the UNPRIORITIZED keyword is */
/*        required in the METHOD argument. */


/*        Syntax of the METHOD input argument */
/*        ----------------------------------- */

/*        The keywords and surface list in the METHOD argument */
/*        are called "clauses." The clauses may appear in any */
/*        order, for example */

/*           DSK/<surface list>/UNPRIORITIZED */
/*           DSK/UNPRIORITIZED/<surface list> */
/*           UNPRIORITIZED/<surface list>/DSK */

/*        The simplest form of the METHOD argument specifying use of */
/*        DSK data is one that lacks a surface list, for example: */

/*           'DSK/UNPRIORITIZED' */

/*        For applications in which all loaded DSK data for the target */
/*        body are for a single surface, and there are no competing */
/*        segments, the above string suffices. This is expected to be */
/*        the usual case. */

/*        When, for the specified target body, there are loaded DSK */
/*        files providing data for multiple surfaces for that body, the */
/*        surfaces to be used by this routine for a given call must be */
/*        specified in a surface list, unless data from all of the */
/*        surfaces are to be used together. */

/*        The surface list consists of the string */

/*           SURFACES = */

/*        followed by a comma-separated list of one or more surface */
/*        identifiers. The identifiers may be names or integer codes in */
/*        string format. For example, suppose we have the surface */
/*        names and corresponding ID codes shown below: */

/*           Surface Name                              ID code */
/*           ------------                              ------- */
/*           'Mars MEGDR 128 PIXEL/DEG'                1 */
/*           'Mars MEGDR 64 PIXEL/DEG'                 2 */
/*           'Mars_MRO_HIRISE'                         3 */

/*        If data for all of the above surfaces are loaded, then */
/*        data for surface 1 can be specified by either */

/*           'SURFACES = 1' */

/*        or */

/*           'SURFACES = "Mars MEGDR 128 PIXEL/DEG"' */

/*        Double quotes are used to delimit the surface name because */
/*        it contains blank characters. */

/*        To use data for surfaces 2 and 3 together, any */
/*        of the following surface lists could be used: */

/*           'SURFACES = 2, 3' */

/*           'SURFACES = "Mars MEGDR  64 PIXEL/DEG", 3' */

/*           'SURFACES = 2, Mars_MRO_HIRISE' */

/*           'SURFACES = "Mars MEGDR 64 PIXEL/DEG", Mars_MRO_HIRISE' */

/*        An example of a METHOD argument that could be constructed */
/*        using one of the surface lists above is */

/*           'DSK/UNPRIORITIZED/SURFACES = "Mars MEGDR 64 PIXEL/DEG", 3' */

/* $ Examples */

/*     The numerical results shown for this example may differ across */
/*     platforms. The results depend on the SPICE kernels used as input, */
/*     the compiler and supporting libraries, and the machine specific */
/*     arithmetic implementation. */


/*     1) In the following example program, a DSK file containing a */
/*        type 2 segment is used to provide a plate model representation */
/*        of the surface of Phobos. */

/*        Find the surface points on a target body corresponding to a */
/*        given planetocentric longitude/latitude grid. */


/*        Example code begins here. */


/*              PROGRAM LATSRF_EX1 */
/*              IMPLICIT NONE */
/*        C */
/*        C     SPICELIB functions */
/*        C */
/*              DOUBLE PRECISION      DPR */
/*              DOUBLE PRECISION      RPD */
/*        C */
/*        C     Local parameters */
/*        C */
/*              CHARACTER*(*)         FMT1 */
/*              PARAMETER           ( FMT1   = '(1X,A,F11.6)' ) */

/*              INTEGER               BDNMLN */
/*              PARAMETER           ( BDNMLN = 36 ) */

/*              INTEGER               FILSIZ */
/*              PARAMETER           ( FILSIZ = 255 ) */

/*              INTEGER               FRNMLN */
/*              PARAMETER           ( FRNMLN = 32 ) */

/*              INTEGER               LNSIZE */
/*              PARAMETER           ( LNSIZE = 79 ) */

/*              INTEGER               MAXN */
/*              PARAMETER           ( MAXN   = 100 ) */

/*              INTEGER               MTHLEN */
/*              PARAMETER           ( MTHLEN = 80 ) */
/*        C */
/*        C     Local variables */
/*        C */
/*              CHARACTER*(FILSIZ)    DSK */
/*              CHARACTER*(FRNMLN)    FIXREF */
/*              CHARACTER*(MTHLEN)    METHOD */
/*              CHARACTER*(LNSIZE)    OUTLIN */
/*              CHARACTER*(BDNMLN)    TARGET */

/*              DOUBLE PRECISION      DLAT */
/*              DOUBLE PRECISION      DLON */
/*              DOUBLE PRECISION      ET */
/*              DOUBLE PRECISION      GRID   ( 2, MAXN ) */
/*              DOUBLE PRECISION      LAT */
/*              DOUBLE PRECISION      LAT0 */
/*              DOUBLE PRECISION      LON */
/*              DOUBLE PRECISION      LON0 */
/*              DOUBLE PRECISION      SRFPTS ( 3, MAXN ) */
/*              DOUBLE PRECISION      XLAT */
/*              DOUBLE PRECISION      XLON */
/*              DOUBLE PRECISION      XR */

/*              INTEGER               I */
/*              INTEGER               J */
/*              INTEGER               N */
/*              INTEGER               NLAT */
/*              INTEGER               NLON */

/*        C */
/*        C     Set target, reference frame, and epoch. */
/*        C */
/*              TARGET = 'PHOBOS' */
/*              FIXREF = 'IAU_PHOBOS' */
/*              ET     = 0.D0 */
/*        C */
/*        C     Use DSK data to represent the surface. */
/*        C */
/*              METHOD = 'DSK/UNPRIORITIZED' */
/*        C */
/*        C     Set the grid dimensions. */
/*        C */
/*              NLON   = 3 */
/*              NLAT   = 2 */
/*        C */
/*        C     Derive evenly spaced grid separations and starting */
/*        C     values in the longitude and latitude dimensions. */
/*        C     Units are degrees. */
/*        C */
/*              LAT0 = 90.D0 */
/*              LON0 =  0.D0 */

/*              DLAT = 180.D0 / (NLAT + 1) */
/*              DLON = 360.D0 /  NLON */
/*        C */
/*        C     Prompt for the name of the DSK to read. */
/*        C */
/*              CALL PROMPT ( 'Enter DSK name    > ', DSK ) */
/*        C */
/*        C     Load the DSK file. */
/*        C */
/*              CALL FURNSH ( DSK ) */
/*        C */
/*        C     Now generate the grid points.  We generate */
/*        C     points along latitude bands, working from */
/*        C     north to south.  The latitude range is selected */
/*        C     to range from +30 to -30 degrees.  Longitude */
/*        C     ranges from 0 to 240 degrees.  The increment */
/*        C     is 90 degrees for latitude and 120 degrees for */
/*        C     longitude. */
/*        C */
/*              N = 0 */

/*              DO I = 1, NLAT */

/*                 LAT = RPD() * ( LAT0 - I*DLAT ) */

/*                 DO J = 1, NLON */

/*                    N   = N + 1 */
/*                    LON = RPD() * ( LON0 + (J-1)*DLON ) */

/*                    GRID(1,N) = LON */
/*                    GRID(2,N) = LAT */

/*                 END DO */

/*              END DO */
/*        C */
/*        C     Find the surface points corresponding to the grid points. */
/*        C */
/*              CALL LATSRF ( METHOD, TARGET, ET, */
/*             .              FIXREF, N,      GRID, SRFPTS ) */
/*        C */
/*        C     Print out the surface points in latitudinal */
/*        C     coordinates and compare the derived lon/lat values */
/*        C     to those of the input grid. */
/*        C */
/*              DO I = 1, N */
/*        C */
/*        C        Use RECRAD rather than RECLAT to produce */
/*        C        non-negative longitudes. */
/*        C */
/*                 CALL RECRAD ( SRFPTS(1,I), XR, XLON, XLAT ) */

/*                 WRITE (*,*) ' ' */

/*                 OUTLIN = 'Intercept for grid point #:' */
/*                 CALL REPMI ( OUTLIN, '#', I, OUTLIN ) */

/*                 WRITE(*,*)  OUTLIN */
/*                 OUTLIN = '  Cartesian coordinates: (#, #, #)' */

/*                 DO J = 1, 3 */
/*                    CALL REPMF( OUTLIN, '#', SRFPTS(J,I), */
/*             .                  8,      'F', OUTLIN      ) */
/*                 END DO */

/*                 WRITE (*,*) OUTLIN */

/*                 WRITE (*,*)    '  Latitudinal Coordinates:' */
/*                 WRITE (*,FMT1) '   Longitude (deg): ', XLON*DPR() */
/*                 WRITE (*,FMT1) '   Latitude  (deg): ', XLAT*DPR() */
/*                 WRITE (*,FMT1) '   Radius     (km): ', XR */
/*                 WRITE (*,*)    ' ' */
/*                 WRITE (*,*)    '  Original Grid Coordinates:' */
/*                 WRITE (*,FMT1) '   Longitude (deg): ', GRID(1,I)*DPR() */
/*                 WRITE (*,FMT1) '   Latitude  (deg): ', GRID(2,I)*DPR() */

/*              END DO */
/*              WRITE (*,*) ' ' */
/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, using the DSK file named phobos512.bds, the output */
/*        was: */


/*        Enter DSK name    > phobos512.bds */

/*         Intercept for grid point 1: */
/*           Cartesian coordinates: (9.5706817, 0.00000000, 5.5256356) */
/*           Latitudinal Coordinates: */
/*            Longitude (deg):    0.000000 */
/*            Latitude  (deg):   30.000000 */
/*            Radius     (km):   11.051271 */

/*           Original Grid Coordinates: */
/*            Longitude (deg):    0.000000 */
/*            Latitude  (deg):   30.000000 */

/*         Intercept for grid point 2: */
/*           Cartesian coordinates: (-4.7586430, 8.2422114, 5.4948076) */
/*           Latitudinal Coordinates: */
/*            Longitude (deg):  120.000000 */
/*            Latitude  (deg):   30.000000 */
/*            Radius     (km):   10.989615 */

/*           Original Grid Coordinates: */
/*            Longitude (deg):  120.000000 */
/*            Latitude  (deg):   30.000000 */

/*         Intercept for grid point 3: */
/*           Cartesian coordinates: (-4.5704268, -7.9162115, 5.2774743) */
/*           Latitudinal Coordinates: */
/*            Longitude (deg):  240.000000 */
/*            Latitude  (deg):   30.000000 */
/*            Radius     (km):   10.554949 */

/*           Original Grid Coordinates: */
/*            Longitude (deg):  240.000000 */
/*            Latitude  (deg):   30.000000 */

/*         Intercept for grid point 4: */
/*           Cartesian coordinates: (10.959385, 0.00000000, -6.3274040) */
/*           Latitudinal Coordinates: */
/*            Longitude (deg):    0.000000 */
/*            Latitude  (deg):  -30.000000 */
/*            Radius     (km):   12.654808 */

/*           Original Grid Coordinates: */
/*            Longitude (deg):    0.000000 */
/*            Latitude  (deg):  -30.000000 */

/*         Intercept for grid point 5: */
/*           Cartesian coordinates: (-4.8830077, 8.4576174, -5.6384116) */
/*           Latitudinal Coordinates: */
/*            Longitude (deg):  120.000000 */
/*            Latitude  (deg):  -30.000000 */
/*            Radius     (km):   11.276823 */

/*           Original Grid Coordinates: */
/*            Longitude (deg):  120.000000 */
/*            Latitude  (deg):  -30.000000 */

/*         Intercept for grid point 6: */
/*           Cartesian coordinates: (-4.5322568, -7.8500991, -5.2333994) */
/*           Latitudinal Coordinates: */
/*            Longitude (deg):  240.000000 */
/*            Latitude  (deg):  -30.000000 */
/*            Radius     (km):   10.466799 */

/*           Original Grid Coordinates: */
/*            Longitude (deg):  240.000000 */
/*            Latitude  (deg):  -30.000000 */


/* $ Restrictions */

/*     1)  This routine assumes that the origin of the body-fixed */
/*         reference frame associated with the target body is located in */
/*         the interior of that body. */

/*     2)  The results returned by this routine may not be meaningful */
/*         if the target surface has multiple surface points associated */
/*         with some (longitude, latitude) coordinates. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 06-JUL-2021 (NJB) (JDR) */

/*        Bug fix: removed spurious blank from long error */
/*        message. */

/*        Edited the header to comply with NAIF standard. Modified */
/*        the grid dimensions and output format in the code example to */
/*        reduce the number of lines and their length in the solution. */

/* -    SPICELIB Version 1.0.0, 21-FEB-2017 (NJB) */

/*        Original version 01-JUL-2016 (NJB) */

/* -& */
/* $ Index_Entries */

/*     map latitudinal coordinates to Cartesian surface points */
/*     map latitudinal coordinates to DSK surface points */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     Local variables */


/*     Saved name/ID item declarations. */


/*     Saved frame name/ID item declarations. */


/*     Saved target radius values. */


/*     Saved name/ID items. */


/*     Saved frame name/ID items. */


/*     Saved target radius values. */


/*     Saved values */


/*     Initial values */

    if (return_()) {
	return 0;
    }
    chkin_("LATSRF", (ftnlen)6);
    if (first) {

/*        Initialize local surface counter. */

	zzctruin_(srfctr);

/*        Initialize target, frame, and radius counters. */

	zzctruin_(svctr1);
	zzctruin_(svctr2);
	zzctruin_(svctr3);
    }

/*     Obtain integer code for the target. */

    zzbods2c_(svctr1, svtarg, &svtcde, &svfnd1, target, &trgcde, &fnd, (
	    ftnlen)36, target_len);
    if (! fnd) {
	setmsg_("The target, '#', is not a recognized name for an ephemeris "
		"object. The cause of this problem may be that you need an up"
		"dated version of the SPICE Toolkit, or that you failed to lo"
		"ad a kernel containing a name-ID mapping for this body.", (
		ftnlen)234);
	errch_("#", target, (ftnlen)1, target_len);
	sigerr_("SPICE(IDCODENOTFOUND)", (ftnlen)21);
	chkout_("LATSRF", (ftnlen)6);
	return 0;
    }

/*     Determine the attributes of the frame designated by FIXREF. */

    zznamfrm_(svctr2, svfref, &svfxfc, fixref, &fixfid, (ftnlen)32, 
	    fixref_len);
    frinfo_(&fixfid, &fxcent, &fxclss, &fxclid, &fnd);
    if (failed_()) {
	chkout_("LATSRF", (ftnlen)6);
	return 0;
    }
    if (! fnd) {
	setmsg_("Reference frame # is not recognized by the SPICE frame subs"
		"ystem. Possibly a required frame definition kernel has not b"
		"een loaded.", (ftnlen)130);
	errch_("#", fixref, (ftnlen)1, fixref_len);
	sigerr_("SPICE(NOFRAME)", (ftnlen)14);
	chkout_("LATSRF", (ftnlen)6);
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
	chkout_("LATSRF", (ftnlen)6);
	return 0;
    }

/*     Check whether the surface name/ID mapping has been updated. */

    zzsrftrk_(srfctr, &surfup);

/*     Initialize the SINCPT utility package for the next computation. */
/*     The choice of initialization routine depends on the target */
/*     surface type. */

    if (first || surfup || s_cmp(method, prvmth, method_len, (ftnlen)500) != 
	    0) {

/*        Set the previous method string to an invalid value, so it */
/*        cannot match any future, valid input. This will force this */
/*        routine to parse the input method on the next call if any */
/*        failure occurs in this branch. Once success is assured, we can */
/*        record the current method in the previous method string. */

	s_copy(prvmth, " ", (ftnlen)500, (ftnlen)1);

/*        Parse the method string. If the string is valid, the */
/*        outputs SHAPE and SUBTYP will always be be set. However, */
/*        SUBTYP is not used in this routine. */

/*        For DSK shapes, the surface list array and count will be set */
/*        if the method string contains a surface list. */

	zzprsmet_(&trgcde, method, &c__100, shpstr, subtyp, &pri, &nsurf, 
		srflst, lmbtyp, trmtyp, method_len, (ftnlen)9, (ftnlen)20, (
		ftnlen)20, (ftnlen)20);
	if (failed_()) {
	    chkout_("LATSRF", (ftnlen)6);
	    return 0;
	}
	if (eqstr_(shpstr, "ELLIPSOID", (ftnlen)9, (ftnlen)9)) {
	    shape = 1;
	} else if (eqstr_(shpstr, "DSK", (ftnlen)9, (ftnlen)3)) {
	    shape = 2;
	} else {

/*           This is a backstop check. */

	    setmsg_("[1] Returned shape value from method string was <#>.", (
		    ftnlen)52);
	    errch_("#", shpstr, (ftnlen)1, (ftnlen)9);
	    sigerr_("SPICE(BUG)", (ftnlen)10);
	    chkout_("LATSRF", (ftnlen)6);
	    return 0;
	}

/*        There should be no subtype specification in the method */
/*        string. */

	if (s_cmp(subtyp, " ", (ftnlen)20, (ftnlen)1) != 0) {
	    setmsg_("Spurious sub-observer point type <#> was present in the"
		    " method string #. The sub-observer type is valid in the "
		    "method strings for SUBPNT and SUBSLR, but is not applica"
		    "ble for LATSRF.", (ftnlen)182);
	    errch_("#", subtyp, (ftnlen)1, (ftnlen)20);
	    errch_("#", method, (ftnlen)1, method_len);
	    sigerr_("SPICE(INVALIDMETHOD)", (ftnlen)20);
	    chkout_("LATSRF", (ftnlen)6);
	    return 0;
	}
	s_copy(prvmth, method, (ftnlen)500, method_len);
    }

/*     At this point, the first pass actions were successful. */

    first = FALSE_;

/*     Check the target body shape. */

    if (shape == 1) {
	if (trgcde != svprvt) {

/*           Reset counter to force lookup. */

	    zzctruin_(svctr3);
	}

/*        Look up target radii using counter. */

	zzbodvcd_(&trgcde, "RADII", &c__3, svctr3, &n, svradi, (ftnlen)5);
	if (failed_()) {
	    chkout_("LATSRF", (ftnlen)6);
	    return 0;
	}
/* Computing MIN */
	d__1 = min(svradi[0],svradi[1]);
	if (min(d__1,svradi[2]) <= 0.) {
	    setmsg_("Body # radii should be positive but were # # #.", (
		    ftnlen)47);
	    errch_("#", target, (ftnlen)1, target_len);
	    errdp_("#", svradi, (ftnlen)1);
	    errdp_("#", &svradi[1], (ftnlen)1);
	    errdp_("#", &svradi[2], (ftnlen)1);
	    sigerr_("SPICE(BADAXISLENGTH)", (ftnlen)20);
	    chkout_("LATSRF", (ftnlen)6);
	    return 0;
	}

/*        The radii are valid. Update the previous target ID. */

	svprvt = trgcde;

/*        Generate surface points. */

	i__1 = *npts;
	for (i__ = 1; i__ <= i__1; ++i__) {

/*           Let X be a point having norm 1 and located at the Ith input */
/*           longitude and latitude. */

	    latrec_(&c_b43, &lonlat[(i__ << 1) - 2], &lonlat[(i__ << 1) - 1], 
		    x);

/*           Scale X to place it on the ellipsoid surface. */

	    edpnt_(x, svradi, &svradi[1], &svradi[2], &srfpts[i__ * 3 - 3]);
	    if (failed_()) {
		chkout_("LATSRF", (ftnlen)6);
		return 0;
	    }
	}
    } else if (shape == 2) {

/*        Initialize the DSK ray-surface intercept algorithm to use a */
/*        DSK model for the surface of the target body. */

	zzsudski_(&trgcde, &nsurf, srflst, &fixfid);

/*        Get the radius of an outer bounding sphere for the body. Scale */
/*        up to avoid getting too close to the target surface. */

	zzmaxrad_(&r__);
	r__ *= 2;
	if (failed_()) {
	    chkout_("LATSRF", (ftnlen)6);
	    return 0;
	}

/*        Generate surface points. */

	i__1 = *npts;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    latrec_(&r__, &lonlat[(i__ << 1) - 2], &lonlat[(i__ << 1) - 1], x)
		    ;
	    vminus_(x, raydir);

/*           Find the ray-surface intercept for the ray emanating */
/*           from X and pointing in the -X direction, where the */
/*           surface is represented by DSK data for the specified */
/*           body and surface list (the surface list was supplied */
/*           to ZZSUDSKI). */

	    zzraysfx_(x, raydir, et, &srfpts[i__ * 3 - 3], &fnd);
	    if (failed_()) {
		chkout_("LATSRF", (ftnlen)6);
		return 0;
	    }
	    if (! fnd) {
		setmsg_("No surface point was found on body # at planetocent"
			"ric longitude # (# deg), latitude # (# deg). This pr"
			"oblem may be due to insufficient DSK data having bee"
			"n loaded for the body. It also could be due to the b"
			"ody having a shape not suitable for this computation"
			", for example, a torus.", (ftnlen)282);
		errch_("#", target, (ftnlen)1, target_len);
		errdp_("#", &lonlat[(i__ << 1) - 2], (ftnlen)1);
		d__1 = lonlat[(i__ << 1) - 2] * dpr_();
		errdp_("#", &d__1, (ftnlen)1);
		errdp_("#", &lonlat[(i__ << 1) - 1], (ftnlen)1);
		d__1 = lonlat[(i__ << 1) - 1] * dpr_();
		errdp_("#", &d__1, (ftnlen)1);
		sigerr_("SPICE(POINTNOTFOUND)", (ftnlen)20);
		chkout_("LATSRF", (ftnlen)6);
		return 0;
	    }

/*           Make sure the intercept is on the correct side of the */
/*           object. */

	    if (vdot_(x, &srfpts[i__ * 3 - 3]) < 0.) {
		setmsg_("A surface point was found on body # for the input p"
			"lanetocentric longitude # (# deg), latitude # (# deg"
			"), but this point is on the opposite side of the bod"
			"y. This likely indicates the the body does not conta"
			"in the origin of the coordinate system. LATSRF does "
			"not work with such surfaces. Consider using DSKSXV f"
			"or this computation.", (ftnlen)331);
		errch_("#", target, (ftnlen)1, target_len);
		errdp_("#", &lonlat[(i__ << 1) - 2], (ftnlen)1);
		d__1 = lonlat[(i__ << 1) - 2] * dpr_();
		errdp_("#", &d__1, (ftnlen)1);
		errdp_("#", &lonlat[(i__ << 1) - 1], (ftnlen)1);
		d__1 = lonlat[(i__ << 1) - 1] * dpr_();
		errdp_("#", &d__1, (ftnlen)1);
		sigerr_("SPICE(SHAPENOTSUPPORTED)", (ftnlen)24);
		chkout_("LATSRF", (ftnlen)6);
		return 0;
	    }
	}
    } else {
	setmsg_("Input method <#> does not specify the target shape as eithe"
		"r ELLIPSOID or DSK.", (ftnlen)78);
	errch_("#", method, (ftnlen)1, method_len);
	sigerr_("SPICE(NOTSUPPORTED)", (ftnlen)19);
	chkout_("LATSRF", (ftnlen)6);
	return 0;
    }
    chkout_("LATSRF", (ftnlen)6);
    return 0;
} /* latsrf_ */

