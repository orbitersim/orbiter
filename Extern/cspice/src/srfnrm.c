/* srfnrm.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__100 = 100;
static integer c__3 = 3;
static integer c__4 = 4;
static doublereal c_b45 = .5;

/* $Procedure SRFNRM ( Map surface points to outward normal vectors ) */
/* Subroutine */ int srfnrm_(char *method, char *target, doublereal *et, char 
	*fixref, integer *npts, doublereal *srfpts, doublereal *normls, 
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
    double pow_dd(doublereal *, doublereal *);

    /* Local variables */
    extern /* Subroutine */ int zzbods2c_(integer *, char *, integer *, 
	    logical *, char *, integer *, logical *, ftnlen, ftnlen);
    static doublereal maxr;
    extern /* Subroutine */ int zzbodvcd_(integer *, char *, integer *, 
	    integer *, integer *, doublereal *, ftnlen), zznamfrm_(integer *, 
	    char *, integer *, char *, integer *, ftnlen, ftnlen), zzsbfnrm_(
	    integer *, integer *, integer *, doublereal *, integer *, 
	    doublereal *, doublereal *), zzctruin_(integer *);
    integer i__;
    extern /* Subroutine */ int zzprsmet_(integer *, char *, integer *, char *
	    , char *, logical *, integer *, integer *, char *, char *, ftnlen,
	     ftnlen, ftnlen, ftnlen, ftnlen);
    integer n;
    extern /* Subroutine */ int zzsrftrk_(integer *, logical *), chkin_(char *
	    , ftnlen);
    static integer shape;
    extern /* Subroutine */ int errch_(char *, char *, ftnlen, ftnlen);
    doublereal level;
    extern /* Subroutine */ int errdp_(char *, doublereal *, ftnlen);
    doublereal limit, a2, b2, c2;
    static integer nsurf;
    extern logical eqstr_(char *, char *, ftnlen, ftnlen);
    static logical svfnd1;
    static integer svctr1[2], svctr2[2];
    extern logical failed_(void);
    static integer svctr3[2];
    integer trgcde;
    extern logical return_(void);
    char lmbtyp[20], shpstr[9], subtyp[20], trmtyp[20];
    doublereal ptsrfm;
    integer fixfid, fxcent, fxclss, fxclid;
    static integer srfctr[2], srflst[100];
    logical surfup;
    static char svtarg[36];
    static integer svtcde;
    static char svfref[32];
    static integer svfxfc;
    static doublereal svradi[3];
    extern /* Subroutine */ int chkout_(char *, ftnlen), setmsg_(char *, 
	    ftnlen), sigerr_(char *, ftnlen), frinfo_(integer *, integer *, 
	    integer *, integer *, logical *);
    logical fnd;
    extern /* Subroutine */ int errint_(char *, integer *, ftnlen), dskgtl_(
	    integer *, doublereal *), surfnm_(doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *), vhatip_(doublereal *);
    static logical pri;

/* $ Abstract */

/*     Map array of surface points on a specified target body to */
/*     the corresponding unit length outward surface normal vectors. */

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


/*     File: dsktol.inc */


/*     This file contains declarations of tolerance and margin values */
/*     used by the DSK subsystem. */

/*     It is recommended that the default values defined in this file be */
/*     changed only by expert SPICE users. */

/*     The values declared in this file are accessible at run time */
/*     through the routines */

/*        DSKGTL  {DSK, get tolerance value} */
/*        DSKSTL  {DSK, set tolerance value} */

/*     These are entry points of the routine DSKTOL. */

/*        Version 1.0.0 27-FEB-2016 (NJB) */




/*     Parameter declarations */
/*     ====================== */

/*     DSK type 2 plate expansion factor */
/*     --------------------------------- */

/*     The factor XFRACT is used to slightly expand plates read from DSK */
/*     type 2 segments in order to perform ray-plate intercept */
/*     computations. */

/*     This expansion is performed to prevent rays from passing through */
/*     a target object without any intersection being detected. Such */
/*     "false miss" conditions can occur due to round-off errors. */

/*     Plate expansion is done by computing the difference vectors */
/*     between a plate's vertices and the plate's centroid, scaling */
/*     those differences by (1 + XFRACT), then producing new vertices by */
/*     adding the scaled differences to the centroid. This process */
/*     doesn't affect the stored DSK data. */

/*     Plate expansion is also performed when surface points are mapped */
/*     to plates on which they lie, as is done for illumination angle */
/*     computations. */

/*     This parameter is user-adjustable. */


/*     The keyword for setting or retrieving this factor is */


/*     Greedy segment selection factor */
/*     ------------------------------- */

/*     The factor SGREED is used to slightly expand DSK segment */
/*     boundaries in order to select segments to consider for */
/*     ray-surface intercept computations. The effect of this factor is */
/*     to make the multi-segment intercept algorithm consider all */
/*     segments that are sufficiently close to the ray of interest, even */
/*     if the ray misses those segments. */

/*     This expansion is performed to prevent rays from passing through */
/*     a target object without any intersection being detected. Such */
/*     "false miss" conditions can occur due to round-off errors. */

/*     The exact way this parameter is used is dependent on the */
/*     coordinate system of the segment to which it applies, and the DSK */
/*     software implementation. This parameter may be changed in a */
/*     future version of SPICE. */


/*     The keyword for setting or retrieving this factor is */


/*     Segment pad margin */
/*     ------------------ */

/*     The segment pad margin is a scale factor used to determine when a */
/*     point resulting from a ray-surface intercept computation, if */
/*     outside the segment's boundaries, is close enough to the segment */
/*     to be considered a valid result. */

/*     This margin is required in order to make DSK segment padding */
/*     (surface data extending slightly beyond the segment's coordinate */
/*     boundaries) usable: if a ray intersects the pad surface outside */
/*     the segment boundaries, the pad is useless if the intercept is */
/*     automatically rejected. */

/*     However, an excessively large value for this parameter is */
/*     detrimental, since a ray-surface intercept solution found "in" a */
/*     segment can supersede solutions in segments farther from the */
/*     ray's vertex. Solutions found outside of a segment thus can mask */
/*     solutions that are closer to the ray's vertex by as much as the */
/*     value of this margin, when applied to a segment's boundary */
/*     dimensions. */

/*     The keyword for setting or retrieving this factor is */


/*     Surface-point membership margin */
/*     ------------------------------- */

/*     The surface-point membership margin limits the distance */
/*     between a point and a surface to which the point is */
/*     considered to belong. The margin is a scale factor applied */
/*     to the size of the segment containing the surface. */

/*     This margin is used to map surface points to outward */
/*     normal vectors at those points. */

/*     If this margin is set to an excessively small value, */
/*     routines that make use of the surface-point mapping won't */
/*     work properly. */


/*     The keyword for setting or retrieving this factor is */


/*     Angular rounding margin */
/*     ----------------------- */

/*     This margin specifies an amount by which angular values */
/*     may deviate from their proper ranges without a SPICE error */
/*     condition being signaled. */

/*     For example, if an input latitude exceeds pi/2 radians by a */
/*     positive amount less than this margin, the value is treated as */
/*     though it were pi/2 radians. */

/*     Units are radians. */


/*     This parameter is not user-adjustable. */

/*     The keyword for retrieving this parameter is */


/*     Longitude alias margin */
/*     ---------------------- */

/*     This margin specifies an amount by which a longitude */
/*     value can be outside a given longitude range without */
/*     being considered eligible for transformation by */
/*     addition or subtraction of 2*pi radians. */

/*     A longitude value, when compared to the endpoints of */
/*     a longitude interval, will be considered to be equal */
/*     to an endpoint if the value is outside the interval */
/*     differs from that endpoint by a magnitude less than */
/*     the alias margin. */


/*     Units are radians. */


/*     This parameter is not user-adjustable. */

/*     The keyword for retrieving this parameter is */


/*     End of include file dsktol.inc */

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
/*     NPTS       I   Number of surface points in input array. */
/*     SRFPTS     I   Array of surface points. */
/*     NORMLS     O   Array of outward, unit length normal vectors. */
/*     PTMEMM     P   Default point-surface membership margin. */

/* $ Detailed_Input */

/*     METHOD   is a short string providing parameters defining */
/*              the computation method to be used. In the syntax */
/*              descriptions below, items delimited by brackets */
/*              are optional. */

/*              METHOD may be assigned the following values: */

/*                 'ELLIPSOID' */

/*                    The normal vector computation uses a triaxial */
/*                    ellipsoid to model the surface of the target */
/*                    body. The ellipsoid's radii must be available */
/*                    in the kernel pool. */


/*                 'DSK/UNPRIORITIZED[/SURFACES = <surface list>]' */

/*                    The normal vector computation uses topographic */
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
/*              on the target body. FIXREF may be any such frame */
/*              supported by the SPICE system, including built-in */
/*              frames (documented in the Frames Required Reading) */
/*              and frames defined by a loaded frame kernel (FK). The */
/*              string FIXREF is case-insensitive, and leading and */
/*              trailing blanks in FIXREF are not significant. */

/*              The input surface points in the array SRFPTS are */
/*              expressed relative to this reference frame, as are */
/*              the normal vectors computed by this routine. */


/*     NPTS     is the number of surface points in the array SRFPTS. */


/*     SRFPTS   is an array of target body surface points. Elements */

/*                 SRFPTS(1,I) */
/*                 SRFPTS(2,I) */
/*                 SRFPTS(3,I) */

/*              are the Cartesian coordinates, expressed in the */
/*              reference frame designated by FIXREF, of the Ith */
/*              surface point in the array. Each surface point */
/*              represents an offset from the center of that frame. */

/*              All surface points must actually be "on" the surface, */
/*              that is, the distance of each point from the surface */
/*              must be less than a small margin. See the $Parameters */
/*              section below for a description of this margin. */

/* $ Detailed_Output */

/*     NORMLS   is an array of unit length, outward normal vectors */
/*              corresponding to the points in SRFPTS. Elements */

/*                 NORMLS(1,I) */
/*                 NORMLS(2,I) */
/*                 NORMLS(3,I) */

/*              are the Cartesian coordinates, expressed in the */
/*              reference frame designated by FIXREF, of the Ith */
/*              normal vector in the array. */

/* $ Parameters */

/*     PTMEMM   is the default point-surface membership margin. This */
/*              margin limits the distance an input point can be from */
/*              a surface and still be considered to lie on that */
/*              surface. */

/*              The details of the application of PTMEMM are */
/*              implementation-dependent. In the DSK case, roughly */
/*              speaking, a point-surface distance limit within a DSK */
/*              segment is set to */

/*                 PTMEMM * MAXR */

/*              where MAXR is the radius of an outer bounding sphere */
/*              for the segment. */

/*              For shapes modeled as ellipsoids, the expression */
/*              above is applied to the maximum radius of the */
/*              ellipsoid. */

/*              See the include file */

/*                 dsktol.inc */

/*              for the declaration of PTMEMM. */

/*              This margin can be overridden. See dsktol.inc */
/*              and DSKSTL for details. */

/* $ Exceptions */

/*     1)  If the target body name specified in the input string cannot */
/*         be converted to an integer ID code, the error */
/*         SPICE(IDCODENOTFOUND) is signaled. */

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
/*         loaded into the kernel pool prior to calling SRFNRM, an error */
/*         is signaled by a routine in the call tree of this routine. */

/*     7)  If the computation method specifies an ellipsoidal target */
/*         model, and if any of the radii of the target body are */
/*         non-positive, an error is signaled by a routine in the call */
/*         tree of this routine. The target must be an extended body. */

/*     8)  If METHOD specifies that the target surface is represented by */
/*         DSK data, and no DSK files are loaded for the specified */
/*         target, an error is signaled by a routine in the call tree */
/*         of this routine. */

/*     9)  If METHOD specifies that the target surface is represented by */
/*         DSK data, and data representing the portion of the surface */
/*         corresponding to the surface points provided in SRFPTS are */
/*         not available, an error is signaled by a routine in the */
/*         call tree of this routine. */

/*     10) If an input surface point is not within a small tolerance */
/*         of the specified surface, the error SPICE(POINTNOTONSURFACE) */
/*         is signaled. See the $Parameters section for details. */

/*     11) If the radii are not available in the kernel pool, an error is */
/*         signaled by a routine in the call tree of this routine. */

/*     12) If the target shape is "ellipsoid" and not all radii of the */
/*         ellipsoid are strictly positive, the error */
/*         SPICE(BADAXISLENGTH) is signaled. */

/* $ Files */

/*     Appropriate kernels must be loaded by the calling program before */
/*     this routine is called. */

/*     The following data are required: */

/*     -  Shape data for the target body: */

/*          PCK data: */

/*             If the target shape is modeled as an ellipsoid, */
/*             triaxial radii for the target body must be loaded into */
/*             the kernel pool. Typically this is done by loading a */
/*             text PCK file via FURNSH. */

/*          DSK data: */

/*             If the target shape is modeled by DSK data, DSK files */
/*             containing topographic data for the target body must be */
/*             loaded. If a surface list is specified, data for at */
/*             least one of the listed surfaces must be loaded. */

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

/*     1) Compute outward normal vectors at surface points on a target */
/*        body, where the points correspond to a given planetocentric */
/*        longitude/latitude grid. Use both ellipsoid and DSK shape */
/*        models. */

/*        Use the meta-kernel shown below to load the required SPICE */
/*        kernels. */


/*           KPL/MK */

/*           File: srfnrm_ex1.tm */

/*           This meta-kernel is intended to support operation of SPICE */
/*           example programs. The kernels shown here should not be */
/*           assumed to contain adequate or correct versions of data */
/*           required by SPICE-based user applications. */

/*           In order for an application to use this meta-kernel, the */
/*           kernels referenced here must be present in the user's */
/*           current working directory. */

/*           The names and contents of the kernels referenced */
/*           by this meta-kernel are as follows: */

/*              File name                        Contents */
/*              ---------                        -------- */
/*              pck00010.tpc                     Planet orientation and */
/*                                               radii */
/*              phobos512.bds                    DSK based on */
/*                                               Gaskell ICQ Q=512 */
/*                                               plate model */
/*           \begindata */

/*              KERNELS_TO_LOAD = ( 'pck00010.tpc', */
/*                                  'phobos512.bds' ) */
/*           \begintext */

/*           End of meta-kernel */


/*        Example code begins here. */


/*              PROGRAM SRFNRM_EX1 */
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

/*              CHARACTER*(*)         META */
/*              PARAMETER           ( META   = 'srfnrm_ex1.tm' ) */


/*              INTEGER               BDNMLN */
/*              PARAMETER           ( BDNMLN = 36 ) */

/*              INTEGER               FILSIZ */
/*              PARAMETER           ( FILSIZ = 255 ) */

/*              INTEGER               FRNMLN */
/*              PARAMETER           ( FRNMLN = 32 ) */

/*              INTEGER               LNSIZE */
/*              PARAMETER           ( LNSIZE = 79 ) */

/*              INTEGER               MAXN */
/*              PARAMETER           ( MAXN   = 100000 ) */

/*              INTEGER               MTHLEN */
/*              PARAMETER           ( MTHLEN = 80 ) */
/*        C */
/*        C     Local variables */
/*        C */
/*              CHARACTER*(FRNMLN)    FIXREF */
/*              CHARACTER*(MTHLEN)    METHOD ( 2 ) */
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
/*              DOUBLE PRECISION      NORMLS ( 3, MAXN, 2 ) */
/*              DOUBLE PRECISION      NRMLAT */
/*              DOUBLE PRECISION      NRMLON */
/*              DOUBLE PRECISION      NRMRAD */
/*              DOUBLE PRECISION      SRFPTS ( 3, MAXN, 2 ) */
/*              DOUBLE PRECISION      XLAT */
/*              DOUBLE PRECISION      XLON */
/*              DOUBLE PRECISION      XR */

/*              INTEGER               I */
/*              INTEGER               J */
/*              INTEGER               N */
/*              INTEGER               NLAT */
/*              INTEGER               NLON */

/*        C */
/*        C     Saved variables */
/*        C */
/*              SAVE                  GRID */
/*              SAVE                  NORMLS */
/*              SAVE                  SRFPTS */

/*        C */
/*        C     Load kernels. */
/*        C */
/*              CALL FURNSH ( META ) */
/*        C */
/*        C     Set target, reference frame, and epoch. */
/*        C */
/*              TARGET = 'PHOBOS' */
/*              FIXREF = 'IAU_PHOBOS' */
/*              ET     = 0.D0 */
/*        C */
/*        C     Use both a reference ellipsoid and DSK data */
/*        C     to represent the surface. */
/*        C */
/*              METHOD(1) = 'ELLIPSOID' */
/*              METHOD(2) = 'DSK/UNPRIORITIZED' */
/*        C */
/*        C     Set the grid dimensions. */
/*        C */
/*              NLON   = 6 */
/*              NLAT   = 3 */
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
/*        C     Now generate the grid points.  We generate */
/*        C     points along latitude bands, working from */
/*        C     north to south.  The latitude range is selected */
/*        C     to range from +45 to -45 degrees.  Longitude */
/*        C     ranges from 0 to 300 degrees.  The increment */
/*        C     is 45 degrees for latitude and 60 degrees for */
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
/*        C */
/*        C     Compute outward normal vectors at the surface points, */
/*        C     using both surface representations. */
/*        C */
/*              DO I = 1, 2 */

/*                 CALL LATSRF ( METHOD(I),    TARGET, ET, */
/*             .                 FIXREF,       N,      GRID, */
/*             .                 SRFPTS(1,1,I)              ) */

/*                 CALL SRFNRM ( METHOD(I),    TARGET, ET, */
/*             .                 FIXREF,       N,      SRFPTS(1,1,I), */
/*             .                 NORMLS(1,1,I)                      ) */
/*              END DO */


/*              WRITE (*,*) 'Number of grid points: ', N */

/*        C */
/*        C     Print out the surface points in latitudinal */
/*        C     coordinates and compare the derived lon/lat values */
/*        C     to those of the input grid for the first 3 points. */
/*        C */
/*              DO I = 1, 3 */
/*        C */
/*        C        Use RECRAD rather than RECLAT to produce */
/*        C        non-negative longitudes. */
/*        C */
/*                 CALL RECRAD ( SRFPTS(1,I,1), XR, XLON, XLAT ) */

/*                 WRITE (*,*) ' ' */

/*                 OUTLIN = ' Surface point for grid point #:' */
/*                 CALL REPMI  ( OUTLIN, '#', I, OUTLIN ) */
/*                 CALL TOSTDO ( OUTLIN ) */

/*                 WRITE (*,*)    '  Latitudinal Coordinates:' */
/*                 WRITE (*,FMT1) '    Longitude           (deg): ', */
/*             .                  XLON*DPR() */
/*                 WRITE (*,FMT1) '    Latitude            (deg): ', */
/*             .                  XLAT*DPR() */
/*                 WRITE (*,FMT1) '    Ellipsoid Radius     (km): ', */
/*             .                  XR */

/*                 CALL RECRAD ( SRFPTS(1,I,2), XR, XLON, XLAT ) */

/*                 WRITE (*,FMT1) '    DSK Radius           (km): ', */
/*             .                  XR */
/*        C */
/*        C        Convert the Ith normal vector to latitudinal */
/*        C        coordinates. */
/*        C */
/*                 CALL RECRAD ( NORMLS(1,I,1), NRMRAD, NRMLON, NRMLAT ) */

/*                 WRITE (*,*)    '  Ellipsoid normal vector direction:' */
/*                 WRITE (*,FMT1) '    Longitude           (deg): ', */
/*             .                       NRMLON*DPR() */
/*                 WRITE (*,FMT1) '    Latitude            (deg): ', */
/*             .                      NRMLAT*DPR() */

/*                 CALL RECRAD ( NORMLS(1,I,2), NRMRAD, NRMLON, NRMLAT ) */

/*                 WRITE (*,*)    '  DSK normal vector direction:' */
/*                 WRITE (*,FMT1) '    Longitude           (deg): ', */
/*             .                  NRMLON*DPR() */
/*                 WRITE (*,FMT1) '    Latitude            (deg): ', */
/*             .                  NRMLAT*DPR() */

/*              END DO */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*         Number of grid points:           18 */

/*         Surface point for grid point 1: */
/*           Latitudinal Coordinates: */
/*             Longitude           (deg):    0.000000 */
/*             Latitude            (deg):   45.000000 */
/*             Ellipsoid Radius     (km):   10.542977 */
/*             DSK Radius           (km):   10.156402 */
/*           Ellipsoid normal vector direction: */
/*             Longitude           (deg):    0.000000 */
/*             Latitude            (deg):   63.895146 */
/*           DSK normal vector direction: */
/*             Longitude           (deg):  341.337568 */
/*             Latitude            (deg):   62.610726 */

/*         Surface point for grid point 2: */
/*           Latitudinal Coordinates: */
/*             Longitude           (deg):   60.000000 */
/*             Latitude            (deg):   45.000000 */
/*             Ellipsoid Radius     (km):   10.172847 */
/*             DSK Radius           (km):   10.131412 */
/*           Ellipsoid normal vector direction: */
/*             Longitude           (deg):   66.059787 */
/*             Latitude            (deg):   58.877649 */
/*           DSK normal vector direction: */
/*             Longitude           (deg):   48.859884 */
/*             Latitude            (deg):   56.924717 */

/*         Surface point for grid point 3: */
/*           Latitudinal Coordinates: */
/*             Longitude           (deg):  120.000000 */
/*             Latitude            (deg):   45.000000 */
/*             Ellipsoid Radius     (km):   10.172847 */
/*             DSK Radius           (km):   10.423766 */
/*           Ellipsoid normal vector direction: */
/*             Longitude           (deg):  113.940213 */
/*             Latitude            (deg):   58.877649 */
/*           DSK normal vector direction: */
/*             Longitude           (deg):  118.553200 */
/*             Latitude            (deg):   55.906774 */


/*        Note that only the first 3 points of the grid are */
/*        presented in the output (the rest of the points are not */
/*        shown due to their large number). */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */

/* $ Version */

/* -    SPICELIB Version 1.0.1, 08-JUL-2020 (JDR) */

/*        Edited the header to comply with NAIF standard. */
/*        Limited the number of grid points presented in the output of */
/*        the code example to three. */

/* -    SPICELIB Version 1.0.0, 22-FEB-2017 (NJB) */

/*        Added FAILED call. */

/*        01-JUL-2016 (NJB) */

/* -& */
/* $ Index_Entries */

/*     map Cartesian surface points to normal vectors */
/*     compute normal vectors on topographic surface */
/*     compute normal vectors on DSK surface */

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
    chkin_("SRFNRM", (ftnlen)6);
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
    if (failed_()) {
	chkout_("SRFNRM", (ftnlen)6);
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
	chkout_("SRFNRM", (ftnlen)6);
	return 0;
    }

/*     Determine the attributes of the frame designated by FIXREF. */

    zznamfrm_(svctr2, svfref, &svfxfc, fixref, &fixfid, (ftnlen)32, 
	    fixref_len);
    frinfo_(&fixfid, &fxcent, &fxclss, &fxclid, &fnd);
    if (failed_()) {
	chkout_("SRFNRM", (ftnlen)6);
	return 0;
    }
    if (! fnd) {
	setmsg_("Reference frame # is not recognized by the SPICE frame subs"
		"ystem. Possibly a required frame definition kernel has not b"
		"een loaded.", (ftnlen)130);
	errch_("#", fixref, (ftnlen)1, fixref_len);
	sigerr_("SPICE(NOFRAME)", (ftnlen)14);
	chkout_("SRFNRM", (ftnlen)6);
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
	chkout_("SRFNRM", (ftnlen)6);
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
	    chkout_("SRFNRM", (ftnlen)6);
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
	    chkout_("SRFNRM", (ftnlen)6);
	    return 0;
	}

/*        There should be no subtype specification in the method */
/*        string. */

	if (s_cmp(subtyp, " ", (ftnlen)20, (ftnlen)1) != 0) {
	    setmsg_("Spurious sub-observer point type <#> was present in the"
		    " method string #. The sub-observer type is valid in the "
		    "method strings for SUBPNT and SUBSLR, but is not applica"
		    "ble for SRFNRM.", (ftnlen)182);
	    errch_("#", subtyp, (ftnlen)1, (ftnlen)20);
	    errch_("#", method, (ftnlen)1, method_len);
	    sigerr_("SPICE(INVALIDMETHOD)", (ftnlen)20);
	    chkout_("SRFNRM", (ftnlen)6);
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
	    chkout_("SRFNRM", (ftnlen)6);
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
	    chkout_("SRFNRM", (ftnlen)6);
	    return 0;
	}

/*        The radii are valid. Update the previous target ID. */

	svprvt = trgcde;

/*        Compute the point-surface distance limit. */

/* Computing MAX */
	d__1 = max(svradi[0],svradi[1]);
	maxr = max(d__1,svradi[2]);
	dskgtl_(&c__4, &ptsrfm);
	limit = ptsrfm * maxr;

/*        Generate normal vectors. */

	i__1 = *npts;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    a2 = svradi[0] * svradi[0];
	    b2 = svradi[1] * svradi[1];
	    c2 = svradi[2] * svradi[2];
	    d__1 = srfpts[i__ * 3 - 3] * srfpts[i__ * 3 - 3] / a2 + srfpts[
		    i__ * 3 - 2] * srfpts[i__ * 3 - 2] / b2 + srfpts[i__ * 3 
		    - 1] * srfpts[i__ * 3 - 1] / c2;
	    level = pow_dd(&d__1, &c_b45);

/*           The test below is a distance test if the target shape */
/*           is a sphere. For other ellipsoids, it's an approximation. */

	    if ((d__1 = level - 1., abs(d__1)) >= limit) {
		setmsg_("Input point at index # is not on the target body su"
			"rface. The level surface parameter (x/a)**2 + (y/b)*"
			"*2 + (z/c)**2 for this point is #.", (ftnlen)137);
		errint_("#", &i__, (ftnlen)1);
		errdp_("#", &level, (ftnlen)1);
		sigerr_("SPICE(POINTNOTONSURFACE)", (ftnlen)24);
		chkout_("SRFNRM", (ftnlen)6);
		return 0;
	    }
	    surfnm_(svradi, &svradi[1], &svradi[2], &srfpts[i__ * 3 - 3], &
		    normls[i__ * 3 - 3]);
	    if (failed_()) {
		chkout_("SRFNRM", (ftnlen)6);
		return 0;
	    }
	}
    } else if (shape == 2) {

/*        Generate normal vectors. */

	i__1 = *npts;
	for (i__ = 1; i__ <= i__1; ++i__) {

/*           Use the DSK API segment buffering system to efficiently */
/*           select relevant segments and compute normals. */

	    zzsbfnrm_(&trgcde, &nsurf, srflst, et, &fixfid, &srfpts[i__ * 3 - 
		    3], &normls[i__ * 3 - 3]);
	    if (failed_()) {
		chkout_("SRFNRM", (ftnlen)6);
		return 0;
	    }

/*           Make sure normals have unit length. */

	    vhatip_(&normls[i__ * 3 - 3]);
	}
    } else {
	setmsg_("Input method <#> does not specify the target shape as eithe"
		"r ELLIPSOID or DSK.", (ftnlen)78);
	errch_("#", method, (ftnlen)1, method_len);
	sigerr_("SPICE(NOTSUPPORTED)", (ftnlen)19);
	chkout_("SRFNRM", (ftnlen)6);
	return 0;
    }
    chkout_("SRFNRM", (ftnlen)6);
    return 0;
} /* srfnrm_ */

