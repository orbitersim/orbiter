/* zzgfcoq.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__7 = 7;
static integer c__3 = 3;

/* $Procedure      ZZGFCOQ ( GF, return coordinate quantity ) */
/* Subroutine */ int zzgfcoq_(char *vecdef, char *method, integer *trgid, 
	doublereal *et, char *ref, char *abcorr, integer *obsid, char *dref, 
	doublereal *dvec, char *crdsys, integer *ctrid, doublereal *re, 
	doublereal *f, char *crdnam, doublereal *value, logical *found, 
	ftnlen vecdef_len, ftnlen method_len, ftnlen ref_len, ftnlen 
	abcorr_len, ftnlen dref_len, ftnlen crdsys_len, ftnlen crdnam_len)
{
    /* Initialized data */

    static logical first = TRUE_;
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
    static integer prvctr = 0;
    static integer prvobs = 0;
    static integer prvtrg = 0;
    static char obsnam[36] = "                                    ";
    static char trgnam[36] = "                                    ";

    /* System generated locals */
    integer i__1;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_cmp(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    extern /* Subroutine */ int chkin_(char *, ftnlen), errch_(char *, char *,
	     ftnlen, ftnlen), moved_(doublereal *, integer *, doublereal *), 
	    bodc2s_(integer *, char *, ftnlen);
    extern logical failed_(void);
    doublereal lt;
    extern /* Subroutine */ int recrad_(doublereal *, doublereal *, 
	    doublereal *, doublereal *);
    extern integer isrchc_(char *, integer *, char *, ftnlen, ftnlen);
    static char ctrnam[36];
    extern logical return_(void);
    char sysnam[32];
    doublereal coords[3], trgepc, srfvec[3];
    integer crdidx, sysidx;
    extern /* Subroutine */ int setmsg_(char *, ftnlen), sigerr_(char *, 
	    ftnlen), chkout_(char *, ftnlen), spkezp_(integer *, doublereal *,
	     char *, char *, integer *, doublereal *, doublereal *, ftnlen, 
	    ftnlen), subpnt_(char *, char *, doublereal *, char *, char *, 
	    char *, doublereal *, doublereal *, doublereal *, ftnlen, ftnlen, 
	    ftnlen, ftnlen, ftnlen), sincpt_(char *, char *, doublereal *, 
	    char *, char *, char *, char *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, logical *, ftnlen, ftnlen, ftnlen, 
	    ftnlen, ftnlen, ftnlen), reclat_(doublereal *, doublereal *, 
	    doublereal *, doublereal *), recsph_(doublereal *, doublereal *, 
	    doublereal *, doublereal *), reccyl_(doublereal *, doublereal *, 
	    doublereal *, doublereal *), recgeo_(doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *), recpgr_(
	    char *, doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, ftnlen);
    doublereal pos[3];

/* $ Abstract */

/*     SPICE private routine intended solely for the support of SPICE */
/*     routines. Users should not call this routine directly due to the */
/*     volatile nature of this routine. */

/*     Return the value of a specified coordinate of a vector. */

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

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     VECDEF     I   Vector definition. */
/*     METHOD     I   Computation method. */
/*     TRGID      I   Target ID code. */
/*     ET         I   Computation epoch. */
/*     REF        I   Reference frame name. */
/*     ABCORR     I   Aberration correction. */
/*     OBSID      I   Observer ID code. */
/*     DREF       I   Reference frame of ray's direction vector. */
/*     DVEC       I   Ray's direction vector. */
/*     CRDSYS     I   Coordinate system name. */
/*     CTRID      I   Frame center ID code. */
/*     RE         I   Equatorial radius of central body. */
/*     F          I   Flattening coefficient of central body. */
/*     CRDNAM     I   Coordinate name. */
/*     VALUE      O   Coordinate value. */
/*     FOUND      O   Flag indicating if coordinate was computed. */

/* $ Detailed_Input */


/*     VECDEF     Every coordinate computed by this routine is a */
/*                function of an underlying vector. VECDEF is a short */
/*                string describing the means by which the vector of */
/*                interest is defined. Only parameters from the Fortran */
/*                INCLUDE file zzgf.inc should be used. Parameter names */
/*                and meanings are: */

/*                   POSDEF               Vector is position of */
/*                                        target relative to observer. */

/*                   SOBDEF               Vector is sub-observer */
/*                                        point on target body.  Vector */
/*                                        points from target body */
/*                                        center to sub-observer point. */
/*                                        The target must be an extended */
/*                                        body modeled as a triaxial */
/*                                        ellipsoid. */

/*                   SINDEF               Vector is ray-surface intercept */
/*                                        point on target body. Vector */
/*                                        points from target body */
/*                                        center to sub-observer point. */
/*                                        The target must be an extended */
/*                                        body modeled as a triaxial */
/*                                        ellipsoid. */

/*                Case, leading and trailing blanks ARE significant */
/*                in the string VECDEF. */


/*     METHOD     is a string specifying the computational method */
/*                applicable to the vector of interest. When VECDEF */
/*                is the parameter */

/*                   SOBDEF */

/*                METHOD should be set to one of the values accepted */
/*                by the SPICELIB routine SUBPNT. */

/*                When VECDEF is the parameter */

/*                   SINDEF */

/*                METHOD should be set to one of the values accepted */
/*                by the SPICELIB routine SINCPT. */

/*                METHOD is ignored if VECDEF is set to */

/*                   POSDEF */

/*                Case, leading and trailing blanks are not significant */
/*                in the string METHOD. */


/*     TRGID      is the NAIF ID code of the target object. */


/*     ET         is the time, expressed as ephemeris seconds past J2000 */
/*                TDB, at which the specified coordinate is to be */
/*                computed. */


/*     REF        is the name of the reference frame relative to which */
/*                the vector of interest is specified.  The specified */
/*                condition applies to the specified coordinate of */
/*                of this vector in frame REF. */

/*                When geodetic or planetographic coordinates are used, */
/*                the reference ellipsoid is assumed to be that */
/*                associated with the central body of the frame */
/*                designated by REF. In this case, the central body of */
/*                the frame must be an extended body. */

/*                Case, leading and trailing blanks are not significant */
/*                in the string REF. */


/*     ABCORR     indicates the aberration corrections to be applied to */
/*                the state of the target body to account for one-way */
/*                light time and stellar aberration.  The orientation */
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


/*     DREF       is the name of the reference frame relative to which a */
/*                ray's direction vector is expressed. This may be any */
/*                frame supported by the SPICE system, including */
/*                built-in frames (documented in the Frames Required */
/*                Reading) and frames defined by a loaded frame kernel */
/*                (FK). The string DREF is case-insensitive, and leading */
/*                and trailing blanks in FIXREF are not significant. */

/*                When DREF designates a non-inertial frame, the */
/*                orientation of the frame is evaluated at an epoch */
/*                dependent on the frame's center and, if the center is */
/*                not the observer, on the selected aberration */
/*                correction. See the description of the direction */
/*                vector DVEC for details. */


/*     DVEC       Ray direction vector emanating from the observer. The */
/*                intercept with the target body's surface of the ray */
/*                defined by the observer and DVEC is sought. */

/*                DVEC is specified relative to the reference frame */
/*                designated by DREF. */

/*                Non-inertial reference frames are treated as follows: */
/*                if the center of the frame is at the observer's */
/*                location, the frame is evaluated at ET. If the frame's */
/*                center is located elsewhere, then letting LTCENT be */
/*                the one-way light time between the observer and the */
/*                central body associated with the frame, the */
/*                orientation of the frame is evaluated at ET-LTCENT, */
/*                ET+LTCENT, or ET depending on whether the requested */
/*                aberration correction is, respectively, for received */
/*                radiation, transmitted radiation, or is omitted. */
/*                LTCENT is computed using the method indicated by */
/*                ABCORR. */


/*     CRDSYS     is the name of the coordinate system to which the */
/*                coordinate of interest belongs. Allowed values are */
/*                those defined in the GF Fortran INCLUDE file */

/*                   zzgf.inc. */

/*                Case, leading and trailing blanks ARE significant */
/*                in the string CRDSYS. */


/*     CTRID      is the NAIF ID code of the input frame REF's center. */


/*     RE         is the equatorial radius associated with the body */
/*                designated by CTRID. RE is used only when the */
/*                coordinate system is GEOSYS or PGRSYS; otherwise */
/*                RE may be set to 0.D0. */

/*     F          is the flattening coefficient associated with the body */
/*                designated by CTRID. RE is used only when the */
/*                coordinate system is GEOSYS or PGRSYS; otherwise RE */
/*                may be set to 0.D0. */


/*     CRDNAM     is the name of the coordinate of interest:  this is */
/*                the coordinate to which the specified condition */
/*                applies.  The set of coordinate names is a function of */
/*                the coordinate system. Allowed values are those */
/*                defined in the GF Fortran INCLUDE file */

/*                   zzgf.inc. */

/*                Case, leading and trailing blanks ARE significant */
/*                in the string CRDNAM. */


/* $ Detailed_Output */

/*     VALUE     is the specified coordinate, evaluated at the epoch ET. */
/*               Coordinates having dimensions of length have units of */
/*               km. Coordinates having angular dimensions have units of */
/*               radians. */

/*               VALUE is defined if and only if the output argument */
/*               FOUND is set to .TRUE. */


/*     FOUND     is a logical flag indicating whether the requested */
/*               coordinate could be computed. FOUND is set to .FALSE. */
/*               if and only if the vector definition is SINDEF and */
/*               either */

/*                  - no surface intercept is found */

/*                  - the velocity of the surface intercept is not */
/*                    computable */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the vector definition VECDEF is not recognized, */
/*         the error SPICE(NOTSUPPORTED) is signaled. */

/*     2)  If the vector definition is either SOBDEF or SINDEF */
/*         and the computation method METHOD is not recognized, */
/*         the error will be diagnosed by routines in the call tree */
/*         of this routine. */

/*     3)  If the aberration correction ABCORR is not recognized, */
/*         the error will be diagnosed by routines in the call tree */
/*         of this routine. */

/*     4)  If the coordinate system name CRDSYS is not recognized, */
/*         the error SPICE(NOTSUPPORTED) is signaled. */

/*     5)  If the coordinate name CRDNAM is not recognized, */
/*         the error SPICE(NOTSUPPORTED) is signaled. */

/*     6)  If the frame REF is not recognized by the frames subsystem, */
/*         the error will be diagnosed by routines in the call tree */
/*         of this routine. */

/*     7)  If VECDEF calls for a computation involving a target surface */
/*         point and the radii defining the reference ellipsoid */
/*         associated with the target body are not available in the */
/*         kernel pool, the error will be diagnosed by routines in the */
/*         call tree of this routine. */

/*     8)  If VECDEF calls for a computation involving a target surface */
/*         point and the name and ID code of the frame associated with */
/*         the target body is not available from the frame subsystem, */
/*         the error will be diagnosed by routines in the call tree */
/*         of this routine. */

/*     9)  If ephemeris data are required but not available to compute */
/*         the state of the target, the coordinate frame REF's center, */
/*         or the input ray's frame DREF's center relative to the */
/*         observer, the error will be diagnosed by routines in the call */
/*         tree of this routine. */

/*     10) If orientation data for the frame REF are not available, */
/*         the error will be diagnosed by routines in the call tree */
/*         of this routine. */

/*     11) If orientation data for the frame DREF are required but */
/*         not available, the error will be diagnosed by routines in the */
/*         call tree of this routine. */

/* $ Files */

/*     This routine doesn't directly participate in SPICE kernel loading */
/*     or unloading.  However, a variety of SPICE kernels must be loaded */
/*     in order for this routine to work: */

/*        - Since all coordinate computations supported by this routine */
/*          depend on observer-target vectors, at a minimum, SPK files */
/*          providing ephemeris data enabling computation of these */
/*          vectors are required. */

/*        - If non-inertial reference frames are used, then PCK */
/*          files, frame kernels, C-kernels, and SCLK kernels may be */
/*          needed. */

/*        - If the coordinate of interest is defined in terms of a target */
/*          surface point, then (currently) a PCK providing radii for a */
/*          triaxial shape model must be loaded. */

/*        - If geodetic or planetographic coordinates are used, then a */
/*          PCK providing radii for a triaxial shape model must be */
/*          loaded. */

/*     See the Files section of GFEVNT's header for further information. */

/* $ Particulars */

/*     This routine is used by the GF coordinate utility routines in */
/*     order to solve for time windows on which specified mathematical */
/*     conditions involving coordinates are satisfied. */

/* $ Examples */

/*     See ZZGFCOU. */

/* $ Restrictions */

/*     1)  The interface and functionality of this set of routines may */
/*         change without notice.  These routines should be called only */
/*         by SPICELIB routines. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman   (JPL) */

/* $ Version */

/* -    SPICELIB Version 2.0.0 12-MAY-2009 (NJB) */

/*        Upgraded to support targets and observers having */
/*        no names associated with their ID codes. */

/* -    SPICELIB Version 1.0.0 05-MAR-2009 (NJB) */

/* -& */
/* $ Index_Entries */

/*     compute coordinates of a vector */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     Local variables */


/*     Saved variables */


/*     Initial values */


/*     The Ith coordinate system in the array SYSNMS has coordinates */
/*     in the Ith row of the array CRDNMS. This association must be */
/*     preserved when this routine is updated. */


/*     The order of the coordinate names in the Ith row of this array */
/*     matches the order of the outputs of the corresponding */
/*     SPICELIB routine REC*, which maps a Cartesian vector to */
/*     the Ith coordinate system in the array SYSNMS. Again, this */
/*     order must be preserved. */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }
    chkin_("ZZGFCOQ", (ftnlen)7);

/*     No result was found yet. */

    *found = FALSE_;

/*     Find the index of the coordinate system name in the list of */
/*     supported names. */

    sysidx = isrchc_(crdsys, &c__7, sysnms, crdsys_len, (ftnlen)32);
    if (sysidx == 0) {

/*        We don't recognize this system name. */

	setmsg_("The coordinate system # is not supported.", (ftnlen)41);
	errch_("#", crdsys, (ftnlen)1, crdsys_len);
	sigerr_("SPICE(NOTSUPPORTED)", (ftnlen)19);
	chkout_("ZZGFCOQ", (ftnlen)7);
	return 0;
    }
    s_copy(sysnam, sysnms + (((i__1 = sysidx - 1) < 7 && 0 <= i__1 ? i__1 : 
	    s_rnge("sysnms", i__1, "zzgfcoq_", (ftnlen)560)) << 5), (ftnlen)
	    32, (ftnlen)32);

/*     Find the index of the coordinate name in the list of */
/*     supported names. */

    crdidx = isrchc_(crdnam, &c__3, crdnms + (((i__1 = sysidx * 3 - 3) < 21 &&
	     0 <= i__1 ? i__1 : s_rnge("crdnms", i__1, "zzgfcoq_", (ftnlen)
	    566)) << 5), crdnam_len, (ftnlen)32);
    if (crdidx == 0) {

/*        We don't recognize this coordinate name. */

	setmsg_("The coordinate name # belonging to the coordinate system # "
		"is not recognized.", (ftnlen)77);
	errch_("#", crdnam, (ftnlen)1, crdnam_len);
	errch_("#", crdsys, (ftnlen)1, crdsys_len);
	sigerr_("SPICE(NOTSUPPORTED)", (ftnlen)19);
	chkout_("ZZGFCOQ", (ftnlen)7);
	return 0;
    }

/*     Look up the target and observer names if these will be */
/*     needed. The SUBPNT and SINCPT interfaces require them. */
/*     The RECPGR interface requires the frame center ID code */
/*     as well. */

    if (s_cmp(vecdef, "SUB-OBSERVER POINT", vecdef_len, (ftnlen)18) == 0 || 
	    s_cmp(vecdef, "SURFACE INTERCEPT POINT", vecdef_len, (ftnlen)23) 
	    == 0 || s_cmp(sysnam, "PLANETOGRAPHIC", (ftnlen)32, (ftnlen)14) ==
	     0) {
	if (first || *trgid != prvtrg) {
	    bodc2s_(trgid, trgnam, (ftnlen)36);
	    prvtrg = *trgid;
	}
	if (first || *obsid != prvobs) {
	    bodc2s_(obsid, obsnam, (ftnlen)36);
	    prvobs = *obsid;
	}
	if (first || *ctrid != prvctr) {
	    bodc2s_(ctrid, ctrnam, (ftnlen)36);
	    prvctr = *ctrid;
	}
	first = FALSE_;
    }
    if (s_cmp(vecdef, "POSITION", vecdef_len, (ftnlen)8) == 0) {

/*        Find the observer-target position vector. */

	spkezp_(trgid, et, ref, abcorr, obsid, pos, &lt, ref_len, abcorr_len);
    } else if (s_cmp(vecdef, "SUB-OBSERVER POINT", vecdef_len, (ftnlen)18) == 
	    0) {

/*        The caller has requested a sub-observer point coordinate */
/*        computation. */

	subpnt_(method, trgnam, et, ref, abcorr, obsnam, pos, &trgepc, srfvec,
		 method_len, (ftnlen)36, ref_len, abcorr_len, (ftnlen)36);
    } else if (s_cmp(vecdef, "SURFACE INTERCEPT POINT", vecdef_len, (ftnlen)
	    23) == 0) {

/*        The caller has requested a surface intercept point coordinate */
/*        computation. */

	sincpt_(method, trgnam, et, ref, abcorr, obsnam, dref, dvec, pos, &
		trgepc, srfvec, found, method_len, (ftnlen)36, ref_len, 
		abcorr_len, (ftnlen)36, dref_len);

/*        Without an intercept, there's nothing left to do here. */

	if (! (*found)) {
	    chkout_("ZZGFCOQ", (ftnlen)7);
	    return 0;
	}
    } else {
	setmsg_("The coordinate quantity # is not recognized.", (ftnlen)44);
	errch_("#", vecdef, (ftnlen)1, vecdef_len);
	sigerr_("SPICE(NOTSUPPORTED)", (ftnlen)19);
	chkout_("ZZGFCOQ", (ftnlen)7);
	return 0;
    }

/*     If we already encountered an error while trying to compute */
/*     the vector of interest, return now. */

    if (failed_()) {
	chkout_("ZZGFCOQ", (ftnlen)7);
	return 0;
    }

/*     At this point we assume the vector whose coordinate is */
/*     to be computed resides in POS. Convert POS to the */
/*     specified coordinate system. */

    if (s_cmp(sysnam, "RECTANGULAR", (ftnlen)32, (ftnlen)11) == 0) {

/*        No conversion needed for rectangular coordinates. */

	moved_(pos, &c__3, coords);
    } else if (s_cmp(sysnam, "LATITUDINAL", (ftnlen)32, (ftnlen)11) == 0) {
	reclat_(pos, coords, &coords[1], &coords[2]);
    } else if (s_cmp(sysnam, "RA/DEC", (ftnlen)32, (ftnlen)6) == 0) {
	recrad_(pos, coords, &coords[1], &coords[2]);
    } else if (s_cmp(sysnam, "SPHERICAL", (ftnlen)32, (ftnlen)9) == 0) {
	recsph_(pos, coords, &coords[1], &coords[2]);
    } else if (s_cmp(sysnam, "CYLINDRICAL", (ftnlen)32, (ftnlen)11) == 0) {
	reccyl_(pos, coords, &coords[1], &coords[2]);
    } else if (s_cmp(sysnam, "GEODETIC", (ftnlen)32, (ftnlen)8) == 0) {
	recgeo_(pos, re, f, coords, &coords[1], &coords[2]);
    } else if (s_cmp(sysnam, "PLANETOGRAPHIC", (ftnlen)32, (ftnlen)14) == 0) {
	recpgr_(ctrnam, pos, re, f, coords, &coords[1], &coords[2], (ftnlen)
		36);
    } else {

/*        We should never arrive here. */

	setmsg_("The coordinate system # is not supported.", (ftnlen)41);
	errch_("#", crdsys, (ftnlen)1, crdsys_len);
	sigerr_("SPICE(BUG)", (ftnlen)10);
	chkout_("ZZGFCOQ", (ftnlen)7);
	return 0;
    }

/*     Set the return value. */

/*     CRDIDX indicates the index of the coordinate of interest */
/*     in the list of coordinates for the input coordinate system. */

    *value = coords[(i__1 = crdidx - 1) < 3 && 0 <= i__1 ? i__1 : s_rnge(
	    "coords", i__1, "zzgfcoq_", (ftnlen)733)];

/*     Having made it this far means the result was found. */

    *found = TRUE_;
    chkout_("ZZGFCOQ", (ftnlen)7);
    return 0;
} /* zzgfcoq_ */

