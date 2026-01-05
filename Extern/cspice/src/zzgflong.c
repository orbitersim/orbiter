/* zzgflong.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__15 = 15;
static integer c__7 = 7;
static integer c__0 = 0;
static integer c__1 = 1;
static doublereal c_b69 = 1.;
static doublereal c_b70 = 0.;

/* $Procedure ZZGFLONG ( GF, longitude solver ) */
/* Subroutine */ int zzgflong_(char *vecdef, char *method, char *target, char 
	*ref, char *abcorr, char *obsrvr, char *dref, doublereal *dvec, char *
	crdsys, char *crdnam, char *relate, doublereal *refval, doublereal *
	tol, doublereal *adjust, U_fp udstep, U_fp udrefn, logical *rpt, U_fp 
	udrepi, U_fp udrepu, U_fp udrepf, logical *bail, L_fp udbail, integer 
	*mw, integer *nw, doublereal *work, doublereal *cnfine, doublereal *
	result, ftnlen vecdef_len, ftnlen method_len, ftnlen target_len, 
	ftnlen ref_len, ftnlen abcorr_len, ftnlen obsrvr_len, ftnlen dref_len,
	 ftnlen crdsys_len, ftnlen crdnam_len, ftnlen relate_len)
{
    /* Initialized data */

    static char ops[6*7] = "<     " "=     " ">     " "LOCMIN" "ABSMIN" "LOC"
	    "MAX" "ABSMAX";
    static doublereal y[3] = { 0.,1.,0. };

    /* System generated locals */
    integer work_dim1, work_dim2, work_offset, i__1, i__2, i__3, i__4;
    doublereal d__1, d__2;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer), s_cmp(char *, char *, 
	    ftnlen, ftnlen);
    double cos(doublereal);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    double sqrt(doublereal), sin(doublereal), atan2(doublereal, doublereal);

    /* Local variables */
    integer head, node, left, quad;
    logical flip;
    integer next;
    extern /* Subroutine */ int zzgfcodc_(), zzgfcocd_();
    extern /* Subroutine */ int zzgfcocg_(doublereal *, doublereal *);
    extern /* Subroutine */ int zzgfcosd_();
    extern /* Subroutine */ int zzgfcoin_(char *, char *, char *, char *, 
	    char *, char *, char *, doublereal *, char *, char *, ftnlen, 
	    ftnlen, ftnlen, ftnlen, ftnlen, ftnlen, ftnlen, ftnlen, ftnlen), 
	    zzgfcosg_(doublereal *, doublereal *);
    extern /* Subroutine */ int zzgfudlt_();
    extern /* Subroutine */ int zzgfrelx_(U_fp, U_fp, U_fp, U_fp, S_fp, char *
	    , doublereal *, doublereal *, doublereal *, doublereal *, integer 
	    *, integer *, doublereal *, logical *, U_fp, U_fp, U_fp, char *, 
	    char *, logical *, L_fp, doublereal *, ftnlen, ftnlen, ftnlen);
    integer i__;
    extern integer cardd_(doublereal *);
    integer n, s;
    extern logical elemi_(integer *, integer *);
    extern /* Subroutine */ int chkin_(char *, ftnlen), ucase_(char *, char *,
	     ftnlen, ftnlen), errch_(char *, char *, ftnlen, ftnlen), lnkan_(
	    integer *, integer *);
    integer class__, compl;
    logical found;
    doublereal value;
    integer right;
    extern /* Subroutine */ int errdp_(char *, doublereal *, ftnlen), copyd_(
	    doublereal *, doublereal *), repmi_(char *, char *, integer *, 
	    char *, ftnlen, ftnlen, ftnlen);
    integer total, f1, f2;
    char rlist[32*7];
    doublereal r2ovr2, start;
    extern /* Subroutine */ int ljust_(char *, char *, ftnlen, ftnlen);
    extern doublereal twopi_(void);
    integer q1, q2, q3, q4;
    extern /* Subroutine */ int bodc2s_(integer *, char *, ftnlen);
    extern logical failed_(void);
    extern doublereal pi_(void);
    doublereal cv, et;
    integer nl;
    extern integer isrchc_(char *, integer *, char *, ftnlen, ftnlen), 
	    lnknxt_(integer *, integer *), wncard_(doublereal *);
    extern logical return_(void), smsgnd_(doublereal *, doublereal *);
    char nrmcrd[32], nrmsys[32], prxcrd[32], prxfun[50], prxsys[32], rctrnm[
	    36], rptpre[80*2], rptsuf[80*2], tmplat[80], prxrel[6];
    doublereal cmpval, extval, locref, loctol, prxval, sv, xrfval;
    integer clssid, frcode, needwn[13], refctr;
    doublereal alt, lat;
    integer region[3], wh, bot, wwpool[26]	/* was [2][13] */;
    extern /* Subroutine */ int setmsg_(char *, ftnlen);
    doublereal lon;
    integer res;
    extern /* Subroutine */ int errint_(char *, integer *, ftnlen), sigerr_(
	    char *, ftnlen), chkout_(char *, ftnlen), ssized_(integer *, 
	    doublereal *), lnkini_(integer *, integer *);
    integer top;
    char uop[6];
    extern /* Subroutine */ int cmprss_(char *, integer *, char *, char *, 
	    ftnlen, ftnlen, ftnlen), scardd_(integer *, doublereal *);
    integer wix[7];
    extern /* Subroutine */ int namfrm_(char *, integer *, ftnlen), frinfo_(
	    integer *, integer *, integer *, integer *, logical *), recpgr_(
	    char *, doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, ftnlen), wninsd_(doublereal *, 
	    doublereal *, doublereal *), wndifd_(doublereal *, doublereal *, 
	    doublereal *), wnunid_(doublereal *, doublereal *, doublereal *), 
	    lnkila_(integer *, integer *, integer *), wnintd_(doublereal *, 
	    doublereal *, doublereal *), ssizei_(integer *, integer *), 
	    insrti_(integer *, integer *), lnkfsl_(integer *, integer *, 
	    integer *), zzgfcog_(doublereal *, doublereal *);
    integer res1, res2;

/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines.  Users should not call this routine directly due */
/*     to the volatile nature of this routine. */

/*     This routine determines time windows when the longitude */
/*     or right ascension of a specified vector satisfies a specified */
/*     mathematical condition. */

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
/*     NAIF_IDS */
/*     FRAMES */

/* $ Keywords */

/*     EPHEMERIS */
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
/*     LBCELL     P   Cell lower bound. */
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
/*     RELATE     I   Relational operator. */
/*     REFVAL     I   Reference value. */
/*     TOL        I   Convergence tolerance. */
/*     ADJUST     I   Absolute extremum adjustment value. */
/*     UDSTEP     I   Step size routine. */
/*     UDREFN     I   Search refinement routine. */
/*     RPT        I   Progress report flag. */
/*     UDREPI     I   Progress report initialization routine. */
/*     UDREPU     I   Progress report update routine. */
/*     UDREPF     I   Progress report termination routine. */
/*     BAIL       I   Bail-out flag. */
/*     UDBAIL     I   Bail-out status function. */
/*     MW         I   Workspace window size. */
/*     NW         I   Workspace window count. */
/*     WORK      I-O  Workspace window array. */
/*     CNFINE     I   Confinement window. */
/*     RESULT     O   Result window. */

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


/*     TARGET     is the name of the target object. */


/*     REF        is the name of the reference frame relative to which */
/*                the vector of interest is specified. The specified */
/*                condition applies to the specified coordinate of */
/*                of this vector in frame REF. */

/*                When geodetic coordinates are used, the reference */
/*                ellipsoid is assumed to be that associated with */
/*                the central body of the frame designated by REF. */
/*                In this case, the central body of the frame must */
/*                be an extended body. */

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

/*                  'NONE'          No correction. */
/*                  'LT'            Light time only. */
/*                  'LT+S'          Light time and stellar aberration. */
/*                  'CN'            Converged Newtonian (CN) light time. */
/*                  'CN+S'          CN light time and stellar aberration. */

/*                Supported aberration correction options for */
/*                transmission (case where radiation is emitted from */
/*                observer at ET) are: */

/*                  'XLT'           Light time only. */
/*                  'XLT+S'         Light time and stellar aberration. */
/*                  'XCN'           Converged Newtonian (CN) light time. */
/*                  'XCN+S'         CN light time and stellar aberration. */

/*                For detailed information, see the geometry finder */
/*                required reading, gf.req.  Also see the header of */
/*                SPKEZR, which contains a detailed discussion of */
/*                aberration corrections. */

/*                Case, leading and trailing blanks are not significant */
/*                in the string ABCORR. */


/*     OBSRVR     is the name of the observer. */


/*     DREF       is the name of the reference frame relative to which a */
/*                ray's direction vector is expressed. This may be any */
/*                frame supported by the SPICE system, including */
/*                built-in frames (documented in the Frames Required */
/*                Reading) and frames defined by a loaded frame kernel */
/*                (FK). The string DREF is case-insensitive, and leading */
/*                and trailing blanks in DREF are not significant. */

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

/*                CRDSYS must refer to a system in which longitude */

/*                or right ascension is a coordinate. Note that when */
/*                geodetic coordinates are used, the reference ellipsoid */
/*                is that associated with the central body of the */
/*                reference frame designated by REF. The central body */
/*                must be an extended body in this case. */

/*                Case, leading and trailing blanks are not significant */
/*                in the string CRDSYS. */


/*     CRDNAM     is the name of the coordinate of interest: this is */
/*                the coordinate to which the specified condition */
/*                applies. Supported coordinates are */

/*                   Planetocentric longitude */
/*                   Right ascension */

/*                which are designated respectively by the parameters */

/*                   LONCRD */
/*                   RACRD */

/*                See the INCLUDE file */

/*                   zzgf.inc */

/*                for the declarations of these parameters. */

/*                For the */

/*                   Latitudinal */
/*                   Geodetic */
/*                   Spherical */

/*                coordinate systems, longitude lies in the range */

/*                   ( -pi, pi ] */

/*                For the */

/*                   Cylindrical */
/*                   Planetographic */

/*                coordinate systems, longitude lies in the range */

/*                   [ 0, 2*pi ) */

/*                Right ascension lies in the range */

/*                   [ 0, 2*pi ) */

/*                Case, leading and trailing blanks are not significant */
/*                in the string CRDNAM. */


/*     RELATE      is a relational operator used to define a constraint */
/*                 on longitude or right ascension of the specified */
/*                 vector. The result window found by this routine */
/*                 indicates the time intervals where the constraint is */
/*                 satisfied. Supported values of RELATE and */
/*                 corresponding meanings are shown below: */

/*                    '>'      Longitude or RA is greater than the */
/*                             reference value REFVAL. */

/*                    '='      Longitude or RA is equal to the reference */
/*                             value REFVAL. */

/*                    '<'      Longitude or RA is less than the */
/*                             reference value REFVAL. */


/*                   'ABSMAX'  Longitude or RA is at an absolute maximum. */

/*                   'ABSMIN'  Longitude or RA is at an absolute */
/*                             minimum. */

/*                   'LOCMAX'  Longitude or RA is at a local maximum. */

/*                   'LOCMIN'  Longitude or RA is at a local minimum. */

/*                The caller may indicate that the region of interest */
/*                is the set of time intervals where the quantity is */
/*                within a specified tolerance of an absolute extremum. */
/*                The argument ADJUST (described below) is used to */
/*                specify this tolerance. */

/*                Local extrema are considered to exist only in the */
/*                interiors of the intervals comprising the confinement */
/*                window: a local extremum cannot exist at a boundary */
/*                point of the confinement window. */

/*                Case is not significant in the string RELATE. */


/*     REFVAL     is the reference value used to define equality or */
/*                inequality conditions. */

/*                REFVAL has units of radians. */

/*                When the coordinate of interest is longitude, REFVAL */
/*                is interpreted as though it were translated, if */
/*                necessary, by an integer multiple of 2*pi to place it */
/*                in the standard range for longitude: (-pi, pi]. */
/*                Similarly, when the coordinate of interest is right */
/*                ascension, REFVAL is interpreted as though it were */
/*                translated, if necessary, by an integer multiple of */
/*                2*pi into the range [0, 2*pi). */

/*                Example:  suppose REFVAL is set to -4.5. Then the */
/*                          condition */

/*                   longitude equals REFVAL */

/*                is interpreted as */

/*                   longitude equals -0.5 * pi */

/*                so the solution window for this condition may well */
/*                be non-empty. */

/*                REFVAL is ignored if RELATE is not an equality or */
/*                inequality operator. */


/*     TOL        is a tolerance value used to determine convergence of */
/*                root-finding operations. TOL is measured in TDB */
/*                seconds and is greater than zero. */


/*     ADJUST     The amount by which the coordinate is allowed to vary */
/*                from an absolute extremum. ADJUST is not used for */
/*                equality or inequality conditions. ADJUST must not be */
/*                negative. */

/*                If ADJUST is positive and a search for an absolute */
/*                minimum is performed, the resulting schedule contains */
/*                time intervals when the specified coordinate has */
/*                values between */

/*                         ABSMIN */
/*                   and   MIN ( ABSMIN + ADJUST, MX ) */

/*                where MX is the maximum value of the coordinate's */
/*                range. */

/*                If the search is for an absolute maximum, the */
/*                corresponding range is  between */

/*                         MAX ( ABSMAX - ADJUST, MN ) */
/*                   and   ABSMAX */

/*                where MN is the minimum value of the coordinate's */
/*                range. */


/*     UDSTEP     is a routine that computes a time step used to search */
/*                for a transition of the state of the specified */
/*                coordinate. In the context of this routine's */
/*                algorithm, a "state transition" occurs where the */
/*                coordinate's time derivative changes from negative to */
/*                non-negative or vice versa. */

/*                This routine relies on UDSTEP returning step sizes */
/*                small enough so that state transitions within the */
/*                confinement window are not overlooked.  There must */
/*                never be two roots A and B separated by less than */
/*                STEP, where STEP is the minimum step size returned by */
/*                UDSTEP for any value of ET in the interval [A, B]. */

/*                The calling sequence for UDSTEP is: */

/*                   CALL UDSTEP ( ET, STEP ) */

/*                where: */

/*                   ET      is the input start time from which the */
/*                           algorithm is to search forward for a state */
/*                           transition. ET is expressed as seconds past */
/*                           J2000 TDB. ET is a DOUBLE PRECISION number. */

/*                   STEP    is the output step size. STEP indicates */
/*                           how far to advance ET so that ET and */
/*                           ET+STEP may bracket a state transition and */
/*                           definitely do not bracket more than one */
/*                           state transition. STEP is a DOUBLE */
/*                           PRECISION number. Units are TDB seconds. */

/*                If a constant step size is desired, the routine GFSTEP */
/*                may be used. GFSTEP returns the step size that was set */
/*                via the most recent call to GFSSTP. */


/*     UDREFN     is the name of the externally specified routine that */
/*                computes a refinement in the times that bracket a */
/*                transition point. In other words, once a pair of */
/*                times have been detected such that the system is in */
/*                different states at each of the two times, UDREFN */
/*                selects an intermediate time which should be closer to */
/*                the transition state than one of the two known times. */
/*                The calling sequence for UDREFN is: */

/*                   CALL UDREFN ( T1, T2, S1, S2, T ) */

/*                where the inputs are: */

/*                   T1    is a time when the system is in state S1. T1 */
/*                         is a DOUBLE PRECISION number. */

/*                   T2    is a time when the system is in state S2. T2 */
/*                         is a DOUBLE PRECISION number and is assumed */
/*                         to be larger than T1. */

/*                   S1    is the state of the system at time T1. */
/*                         S1 is a LOGICAL value. */

/*                   S2    is the state of the system at time T2. */
/*                         S2 is a LOGICAL value. */

/*                UDREFN may use or ignore the S1 and S2 values. */

/*                The output is: */

/*                   T     is next time to check for a state transition. */
/*                         T is a DOUBLE PRECISION number between T1 and */
/*                         T2. */

/*                If a simple bisection method is desired, the routine */
/*                GFREFN may be used. This is the default option. */


/*     RPT        is a logical variable which controls whether the */
/*                progress reporter is on or off; setting RPT */
/*                to .TRUE. enables progress reporting. */


/*     UDREPI     is a user-defined subroutine that initializes a */
/*                progress report. When progress reporting is */
/*                enabled, UDREPI is called at the start of a search */
/*                pass (see the implementation of ZZGFREL for details on */
/*                search passes).  The calling sequence of UDREPI is */

/*                   UDREPI ( CNFINE, RPTPRE, RPTSUF ) */

/*                   DOUBLE PRECISION    CNFINE ( LBCELL : * ) */
/*                   CHARACTER*(*)       RPTPRE */
/*                   CHARACTER*(*)       RPTSUF */

/*                where */

/*                   CNFINE */

/*                is the confinement window passed into ZZGFREL, and */

/*                   RPTPRE */
/*                   RPTSUF */

/*                are prefix and suffix strings used in the progress */
/*                report:  these strings are intended to bracket a */
/*                representation of the fraction of work done. */

/*                SPICELIB provides the default progress reporting */
/*                initialization routine GFREPI. If GFREPI is used, then */
/*                the progress reporting update and termination routines */
/*                GFREPU and GFREPF must be used as well. */


/*     UDREPU     is a user-defined subroutine that updates the */
/*                progress report for a search pass. The calling */
/*                sequence of UDREPU is */

/*                   UDREPU (IVBEG, IVEND, ET ) */

/*                   DOUBLE PRECISION      ET */
/*                   DOUBLE PRECISION      IVBEG */
/*                   DOUBLE PRECISION      IVEND */

/*                where ET is an epoch belonging to the confinement */
/*                window, IVBEG and IVEND are the start and stop times, */
/*                respectively of the current confinement window */
/*                interval.  The ratio of the measure of the portion */
/*                of CNFINE that precedes ET to the measure of CNFINE */
/*                would be a logical candidate for the search's */
/*                completion percentage; however the method of */
/*                measurement is up to the user. */


/*     UDREPF     is a user-defined subroutine that finalizes a */
/*                progress report. UDREPF has no arguments. */


/*     BAIL       is a logical flag indicating whether or not interrupt */
/*                signal handling is enabled. Setting BAIL to .TRUE. */
/*                enables interrupt signal handling: the GF system will */
/*                then call UDBAIL to check for interrupt signals. */


/*     UDBAIL     is the name of a user defined logical function that */
/*                checks to see whether an interrupt signal has been */
/*                issued from, e.g. the keyboard. UDBAIL is used only */
/*                when BAIL is set to .TRUE. If interrupt handling is */
/*                not used, the SPICELIB function GFBAIL should be */
/*                passed in as the actual bail-out function argument. */


/*     MW         is the cell size of the windows in the workspace array */
/*                WORK. */


/*     NW         is the number of windows in the workspace array WORK. */
/*                NW must be at least as large as the parameter NWMAX. */


/*     WORK       is an array used to store workspace windows. This */
/*                array has dimensions ( LBCELL : MW, NW). */

/*                The windows contained WORK that used by this routine */
/*                are initialized here to have size MW. The other */
/*                elements of WORK are not modified. */


/*     CNFINE     is a SPICE window that confines the bounds of the */
/*                search. */

/*                For coordinates defined by ray-target surface */
/*                intercepts, the intercept and its time derivative are */
/*                expected to be computable on the confinement window. */


/*     RESULT     is an initialized SPICE window. RESULT must be large */
/*                enough to hold all of the intervals, within the */
/*                confinement window, on which the specified condition */
/*                is met. */

/*                RESULT must be initialized by the caller via the */
/*                SPICELIB routine SSIZED. */

/* $ Detailed_Output */


/*     WORK       has undefined contents on output, with the exception */
/*                of the windows occupying the range */

/*                   ( LBCELL : NW, EXWIDX : NWMAX ) */

/*                which are not modified by this routine. */

/*     RESULT     is a SPICELIB window containing the intersection of */
/*                the confinement window and the set of time intervals */
/*                when the value of the specified coordinate satisfies */
/*                constraints specified by RELATE and ADJUST. */

/*                For coordinates defined by ray-target surface */
/*                intercepts, RESULT is further restricted to the window */
/*                over which the intercept and its derivative with */
/*                respect to time are computable. See the description of */
/*                CNFINE above for details. */

/*                Due to computational accuracy limitations, the */
/*                coordinate of interest *may not satisfy the */
/*                specified condition* at all points belonging to */
/*                RESULT.  For example, if the caller specifies */
/*                a tolerance of 1.E-6 seconds and seeks the */
/*                solution set for the condition */

/*                   The planetocentric longitude of the geometric */
/*                   earth-sun vector in the J2000 frame is greater */
/*                   than or equal to zero */

/*                the right endpoints of some intervals in RESULT may be */
/*                times that map to negative longitude values very close */
/*                to -pi radians. */

/*                The user (of SPICE API routines dependent on this */
/*                routine) may wish to contract RESULT using WNCOND in */
/*                order to guarantee that the specified condition */
/*                is satisfied on RESULT. Selection of a suitable */
/*                contraction value is dependent on the user's */
/*                requirements and the specific problem to be solved. */

/* $ Parameters */

/*     LBCELL     is the SPICELIB cell lower bound. */

/* $ Exceptions */

/*     1)  In order for this routine to produce correct results, */
/*         the external step size routine UDGSTP must return step sizes */
/*         appropriate for the problem at hand.  Step sizes that */
/*         are too large may cause this routine to miss roots; */
/*         step sizes that are too small may cause this routine to */
/*         run unacceptably slowly and in some cases, find spurious */
/*         roots. */

/*         This routine does not diagnose invalid step sizes, */
/*         except that if the step size is non-positive, the error */
/*         SPICE(VALUEOUTOFRANGE) is signaled. */

/*     2)  In order for this routine to produce correct results, */
/*         the convergence tolerance TOL must be appropriate for the */
/*         problem at hand.  The error in any interval endpoint */
/*         contained in RESULT should be expected to be no smaller */
/*         than TOL; depending on the behavior of the coordinate */
/*         and the condition, the error could be much larger.  For */
/*         example, for some functions, finding correct, unique */
/*         extrema is notoriously difficult. */

/*         The user should keep in mind that the minimum separation */
/*         between successive values of ET is about 1.E-7 seconds */
/*         for SPICE platforms and values of ET not extremely close to */
/*         J2000. */

/*         This routine does not diagnose invalid tolerance values, */
/*         except that if the tolerance is non-positive, the error */
/*         SPICE(VALUEOUTOFRANGE) is signaled. */

/*     3)  A negative value for ADJUST causes the routine to signal */
/*         the error SPICE(VALUEOUTOFRANGE). A non-zero value for ADJUST */
/*         when RELATE has any value other than "ABSMIN" or "ABSMAX", */
/*         causes the routine to signal the error SPICE(INVALIDVALUE). */

/*     4)  If the operator string RELATE doesn't contain a recognized */
/*         value, the error SPICE(NOTRECOGNIZED) is signaled. */

/*     5)  If any error occurs while initializing the coordinate */
/*         utility package, the error will be diagnosed by routines */
/*         in the call tree of ZZGFCOIN. */

/*     6)  If any error occurs while performing computations */
/*         to determine if a quantity of interest is decreasing */
/*         at a specified time, the error will be diagnosed by */
/*         routines in the call tree of this routine. */

/*     7)  If any error occurs while performing computations */
/*         to determine if a quantity of interest is less than a */
/*         specified reference value at a specified time, the error will */
/*         be diagnosed by routines in the call tree of this routine. */

/*     8)  If an error (typically cell overflow) occurs while performing */
/*         window arithmetic, the error will be diagnosed by */
/*         routines in the call trees of window routines called by */
/*         this routine. */

/*     9)  Due to numerical errors, in particular, */

/*            - Truncation error in time values */
/*            - Finite tolerance value */
/*            - Errors in computed geometric quantities */

/*         it is *normal* that the condition of interest is not */
/*         satisfied on the entire result window. */

/*         The result window may need to be contracted slightly by the */
/*         caller to achieve desired results, in particular to remove */
/*         times where discontinuities of longitude or right ascension */
/*         are crossed. */

/*     10) Most relational conditions involving longitude or */
/*         right ascension make sense only when latitude or declination */
/*         is bounded away from +/- pi/2 radians.  Users should */
/*         select the confinement window accordingly. */

/*     11) The user must take care when searching for an extremum */
/*         (ABSMAX, ABSMIN, LOCMAX, LOCMIN) of  LONGITUDE or */
/*         RIGHT ASCENSION values. Since these quantities are cyclical, */
/*         rather than monotonically increasing or decreasing, an */
/*         extremum may be hard to interpret. In particular, if an */
/*         extremum is found near the cycle boundary (- PI for */
/*         longitude, 2 PI for RIGHT ASCENSION) it may not be */
/*         numerically reasonable. For example, the search for times */
/*         when a longitude coordinate is at its absolute maximum may */
/*         result in a time when the longitude value is - PI, due to */
/*         roundoff error. */

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

/*        - If geodetic coordinates are used, then a PCK providing radii */
/*          for a triaxial shape model must be loaded. */

/*     See the Files section of GFEVNT's header for further information. */


/* $ Particulars */

/*     Since this is a private SPICELIB routine, the header comments */
/*     make many references to the routine's implementation.  This */
/*     is done to help the maintenance programmer understand the */
/*     routine; however, these comments may themselves need to be */
/*     updated if the GF subsystem implementation changes. */

/*     This routine determines time windows when the longitude or right */
/*     ascension of a specified vector satisfies a specified */
/*     mathematical condition.  This routine can (in some cases, by */
/*     means of multiple calls) answer questions such as */

/*        When does the moon pass over the earth's prime meridian? */

/*        Given a time window when the geodetic latitude of the MGS */
/*        spacecraft relative to the IAU_MARS frame is between -30 : +30 */
/*        degrees, when within this window is the planetographic */
/*        longitude of the spacecraft between 15 and 16 degrees? */

/*     For brevity, throughout this routine, we'll refer to the vector */
/*     whose longitude or right ascension is of interest as "the vector" */
/*     or "the vector of interest." We'll also call the longitude or */
/*     right ascension "the coordinate" or "the coordinate of interest." */

/*     A note concerning processing speed:  the algorithm used by this */
/*     routine takes a "divide and conquer" approach that involves, in */
/*     many cases, multiple calls to the low-level GF root finding */
/*     routines.  So the user can expect most longitude or right */
/*     ascension computations to be relatively slow.  Using a */
/*     confinement window that is more easily computed, say one */
/*     involving latitude constraints, can be very helpful. */

/* $ Examples */

/*     See usage in GFEVNT. */

/* $ Restrictions */

/*     1)  The interface and functionality of this routine may change */
/*         without notice.  This routine should be called only by */
/*         SPICELIB routines. */

/*     2)  Root-finding problems of the sort solved by this routine are, */
/*         when a computer is involved, replete with mathematical */
/*         complications. We've tried to cover all the angles in the */
/*         Detailed_Input, Detailed_Output, and Exceptions header */
/*         sections.  No doubt some issues remain unaddressed.  Correct */
/*         usage of this routine depends in good measure on the user */
/*         posing "reasonable" problems to solve. */

/*     3)  The kernel files to be used by ZZGFLONG must be loaded */
/*         (normally via the SPICELIB routine FURNSH) before ZZGFLONG is */
/*         called. */

/*     4)  This routine has the side effect of re-initializing the */
/*         coordinate quantity utility package. Callers may themselves */
/*         need to re-initialize the coordinate quantity utility package */
/*         after calling this routine. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     E.D. Wright    (JPL) */
/*     N.J. Bachman   (JPL) */

/* $ Version */

/* -    SPICELIB Version 2.1.1 03-OCT-2021 (NJB) */

/*        Corrected typo in comments. */

/* -    SPICELIB Version 2.1.0 04-APR-2011 (EDW) */

/*        Replaced use of routines ZZGFREL with ZZGFRELX. ZZGFCOIN */
/*        argument list edited to remove the unneeded argument REFVAL. */

/*        The 2.1.0 changes should not affect the numerical results */
/*        of GF coordinate computations. */

/* -    SPICELIB Version 2.0.0 12-MAY-2009 (NJB) */

/*        Upgraded to support targets and observers having */
/*        no names associated with their ID codes. */

/* -    SPICELIB Version 1.0.0  23-FEB-2009 (NJB) (EDW) */

/* -& */

/*     SPICELIB functions */


/*     Entry points in the coordinate utility package. */
/*     We have the usual GF entry points for the coordinate, plus */
/*     utilities for the cosine and sine of the coordinate. */

/*     Names and meanings: */

/*        ZZGFCODC      Is coordinate decreasing? */
/*        ZZGFCOG       Get coordinate value. */
/*        ZZGFCOCD      Is cosine of the coordinate decreasing? */
/*        ZZGFCOCG      Get cosine of the coordinate value. */
/*        ZZGFCOSD      Is sine of the coordinate decreasing? */
/*        ZZGFCOSG      Get sine of the coordinate value. */


/*    The routine to test UDFUNC < REFVAL. */


/*     Local parameters */



/*     Margin for branch cut avoidance. Units are radians: */


/*     Margin for local extrema search. Units are radians: */


/*     Short alias for LBCELL: */


/*     Number of supported comparison operators: */


/*     Assorted string lengths: */

/*     Maximum body name length: */


/*     NAMLEN is the maximum length of both a frame name and of */
/*     any kernel pool variable name. */


/*     OPLEN is the maximum string length for comparison operators. */
/*     OPLEN may grow if new comparisons are added. */


/*     FUNLEN is the length of the function name string. */


/*     CRDLEN is the maximum length of a coordinate name. */


/*     SYSLEN is the maximum length of a coordinate system name. */


/*     RPTLEN is the maximum length of a progress reporter message. */


/*     Local variables */


/*     Saved variables */


/*     Initial values */

    /* Parameter adjustments */
    work_dim1 = *mw + 6;
    work_dim2 = *nw;
    work_offset = work_dim1 - 5;

    /* Function Body */

/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("ZZGFLONG", (ftnlen)8);
    }

/*     Overview */
/*     ======== */


/*     Terminology */
/*     ----------- */

/*        - Proxy function */

/*          In many cases, instead of finding a time window */
/*          where the coordinate of interest satisfies a specified */
/*          condition, we'll find a time window where a second, related */
/*          function satisfies a related condition.  We'll call this */
/*          second function the "proxy function." */

/*          The proxy function will be one that is "better behaved" */
/*          than the original in the domain of interest.  For */
/*          example, when searching for times when longitude is */
/*          equal to pi radians, we may instead intersect the */
/*          confinement window with a window on which cosine of */
/*          longitude is negative, and then within that more */
/*          restricted intersection, find the times when the sine */
/*          of longitude is zero.  In this example sine(longitude) */
/*          is a proxy function for longitude. */

/*        - Resolution of a function */

/*          Below we'll refer to the "resolution" of a proxy function. */
/*          In order to find roots accurately, it's necessary for */
/*          a proxy function to change a by a reasonable amount */
/*          when the function it represents changes.  Mathematically, */
/*          the magnitude of the derivative of the proxy function */
/*          with respect to the function it represents should not */
/*          be too much less than 1.  An example of a *bad* choice */
/*          of a proxy function would be to use cosine of longitude */
/*          as a proxy function for longitude in a confinement */
/*          window in which longitude is close to zero.  This */
/*          choice would lead to considerable loss of accuracy.  On */
/*          the other hand, sine of longitude would be a reasonable */
/*          proxy function for this case. */

/*        - The unit circle */

/*          In the discussion below, we'll freely associate angular */
/*          coordinates with locations on the unit circle. For example, */
/*          we might say "longitude is in the upper half of the unit */
/*          circle." */

/*        - Window aliases */

/*          We're going to make extensive use workspace windows. */
/*          In many cases, we'll need to reuse various windows for */
/*          different purposes at different times.  So instead */
/*          of using mnemonic parameter names for window indices, */
/*          we'll use variables we call window aliases.  For example, */
/*          when we want to use the 8th workspace window to hold */
/*          the window of times when longitude is in the upper half */
/*          of the unit circle, we'll set the alias UPPER equal to */
/*          8, so we can refer to the window by */

/*              WORK( LB, UPPER ) */

/*          and keep track of what we're using the window for. */

/*          Some of the aliases aren't wonderful names:  we use */
/*          F1, F2, etc.  to represent "free" window 1, 2, etc. */


/*     Algorithm */
/*     --------- */

/*        -  Equality */

/*           We use sine or cosine of the coordinate as proxy functions. */
/*           The proxy function having the better resolution is */
/*           selected.  For example, to find times when right ascension */
/*           is 2*pi/3, we search for the times when cosine of right */
/*           ascension is equal to -1/2. Since these searches can produce */
/*           spurious roots, we cast out any such roots after completing */
/*           the search. */


/*        -  Local extrema */

/*           We first find local extrema in the right and left half */
/*           circles, using longitude as a proxy function on the right */
/*           half and right ascension on the left. */


/*        -  Absolute extrema */

/*           We deal with absolute extrema before inequalities because */
/*           this allows us to use the code (later in this routine) for */
/*           inequality relations when the user specifies a non-zero */
/*           ADJUST value. When ADJUST is non-zero, having the actual */
/*           extreme value in hand, we can easily solve for the window */
/*           in which the coordinate is greater than */

/*              <absolute maximum> - ADJUST */

/*           or less than */

/*              <absolute minimum> + ADJUST */

/*           Below, "Searching in a region" means that we find the */
/*           window when the coordinate is in the region (and of course */
/*           in the confinement window), then use this window as the */
/*           confinement window. */

/*           Finding absolute extrema is a matter of successively */
/*           searching for extrema in different parts of the unit */
/*           circle.  For example, when we search for an absolute */
/*           maximum of longitude, we first search in the second */
/*           quadrant, then if we find nothing, the right half circle, */
/*           then if we find nothing, the fourth quadrant. */

/*           We always use longitude as a proxy function on the right */
/*           half circle and right ascension as a proxy function on */
/*           the left half circle. */


/*        -  Inequality */

/*           In general, we use  proxy functions and break up the unit */
/*           circle into regions where the proxy functions are single */
/*           valued. The exact solution approach depends on where the */
/*           reference value is.  For example, to find the window on */
/*           which longitude is less than 3*pi/4, we first search */
/*           for the solution in the second quadrant.  We then */
/*           combine this result window with the window of times */
/*           when longitude is in the right half circle, and with */
/*           the window of times when longitude is in the third */
/*           quadrant. */


/*     Code layout */
/*     ----------- */

/*        We've tried to arrange the code to minimize calls to */
/*        ZZGFREL, primarily because these expensive in terms of */
/*        run time.  They also take up a lot of space. */

/*        The code starts out by re-formulating the constraint, */
/*        if necessary, as one applying to planetocentric longitude */
/*        or right ascension. This simplifies the subsequent logic. */

/*        Equality searches are handled before the rest. The routine */
/*        exits after processing a search having an equality constraint. */

/*        Searches for local extrema are handled next. Again, the */
/*        routine exits after processing these types of searches. */

/*        The next portion of the code is devoted to dealing with */
/*        absolute extrema. If the search is for absolute extrema and */
/*        AJDUST is non-zero, we use the results from this portion of */
/*        the code to set up an inequality search, which is done below. */

/*        After the portion of the code dealing with absolute extrema */
/*        with ADJUST equal to zero, we perform setup functions to */
/*        prepare to call ZZGFREL. In general, what's happening here is */
/*        that we're deciding what regions of the unit circle we're */
/*        going to use in our solution, and we prepare to find windows */
/*        when the coordinate is in the various regions of interest. */
/*        This setup code includes assignment of window aliases, */
/*        selection of proxy functions, and setting flags indicating */
/*        which windows corresponding to search regions must be */
/*        computed. */

/*        Next, the windows corresponding to times when the coordinate */
/*        is in the selected regions are found using ZZGFREL. */


/*     Check the workspace window count. */

    if (*nw < 15) {
	setmsg_("Workspace window count was # but must be at least #.", (
		ftnlen)52);
	errint_("#", nw, (ftnlen)1);
	errint_("#", &c__15, (ftnlen)1);
	sigerr_("SPICE(TOOFEWWINDOWS)", (ftnlen)20);
	chkout_("ZZGFLONG", (ftnlen)8);
	return 0;
    }

/*     We can't initialize the whole workspace, but we can initialize */
/*     the windows we actually own. Do so. */

    for (i__ = 1; i__ <= 7; ++i__) {
	ssized_(mw, &work[(i__1 = (i__ + 5) * work_dim1 - 5 - work_offset) < 
		work_dim1 * work_dim2 && 0 <= i__1 ? i__1 : s_rnge("work", 
		i__1, "zzgflong_", (ftnlen)1291)]);
    }

/*     Initialize the workspace window pool. Set up the parallel */
/*     array of window indices. */

    lnkini_(&c__7, wwpool);
    for (i__ = 1; i__ <= 7; ++i__) {
	wix[(i__1 = i__ - 1) < 7 && 0 <= i__1 ? i__1 : s_rnge("wix", i__1, 
		"zzgflong_", (ftnlen)1301)] = i__ + 5;
    }

/*     Get an upper case, left-justified version of the */
/*     requested comparison operation. */

    ljust_(relate, uop, relate_len, (ftnlen)6);
    ucase_(uop, uop, (ftnlen)6, (ftnlen)6);

/*     Reject bad operators. */

/*     Use the original operator string in the error message. */

    i__ = isrchc_(uop, &c__7, ops, (ftnlen)6, (ftnlen)6);
    if (i__ == 0) {
	setmsg_("The comparison operator, # is not recognized.  Supported qu"
		"antities are: <, =, >, LOCMIN, ABSMIN, LOCMAX, ABSMAX.", (
		ftnlen)113);
	errch_("#", relate, (ftnlen)1, relate_len);
	sigerr_("SPICE(NOTRECOGNIZED)", (ftnlen)20);
	chkout_("ZZGFLONG", (ftnlen)8);
	return 0;
    }

/*     Make sure TOL is positive. */

    if (*tol <= 0.) {
	setmsg_("TOL was #; must be positive.", (ftnlen)28);
	errdp_("#", tol, (ftnlen)1);
	sigerr_("SPICE(VALUEOUTOFRANGE)", (ftnlen)22);
	chkout_("ZZGFLONG", (ftnlen)8);
	return 0;
    }

/*     We'll use a local tolerance equal to 1/5 of the input value. */
/*     This will allow us to keep the total round-off error within */
/*     the desired tolerance. */

/* Computing MAX */
    d__1 = 1e-7, d__2 = *tol / 10.;
    loctol = max(d__1,d__2);

/*     Make sure ADJUST is non-negative. */

    if (*adjust < 0.) {
	setmsg_("ADJUST was #; must be non-negative.", (ftnlen)35);
	errdp_("#", adjust, (ftnlen)1);
	sigerr_("SPICE(VALUEOUTOFRANGE)", (ftnlen)22);
	chkout_("ZZGFLONG", (ftnlen)8);
	return 0;
    }

/*    Confirm ADJUST equals zero unless UOP (RELATE) has value */
/*    "ABSMAX" or "ABSMIN." */

    if (s_cmp(uop, "ABSMIN", (ftnlen)6, (ftnlen)6) != 0 && s_cmp(uop, "ABSMAX"
	    , (ftnlen)6, (ftnlen)6) != 0) {
	if (*adjust != 0.) {
	    setmsg_("ADJUST should have value zero for all comparison operat"
		    "ors except ABSMAX and ABSMIN", (ftnlen)83);
	    sigerr_("SPICE(INVALIDVALUE)", (ftnlen)19);
	    chkout_("ZZGFLONG", (ftnlen)8);
	    return 0;
	}
    }

/*     Get an upper case, left-justified, compressed versions of the */
/*     coordinate system and coordinate names. */

    ljust_(crdsys, nrmsys, crdsys_len, (ftnlen)32);
    cmprss_(" ", &c__0, nrmsys, nrmsys, (ftnlen)1, (ftnlen)32, (ftnlen)32);
    ucase_(nrmsys, nrmsys, (ftnlen)32, (ftnlen)32);
    ljust_(crdnam, nrmcrd, crdnam_len, (ftnlen)32);
    cmprss_(" ", &c__1, nrmcrd, nrmcrd, (ftnlen)1, (ftnlen)32, (ftnlen)32);
    ucase_(nrmcrd, nrmcrd, (ftnlen)32, (ftnlen)32);

/*     Make an initial call to the coordinate utility initialization */
/*     routine to invoke error checking. We don't want to have */
/*     to duplicate the checking here. Later, when necessary, we'll */
/*     re-initialize the utilities. */

    zzgfcoin_(vecdef, method, target, ref, abcorr, obsrvr, dref, dvec, nrmsys,
	     nrmcrd, vecdef_len, method_len, target_len, ref_len, abcorr_len, 
	    obsrvr_len, dref_len, (ftnlen)32, (ftnlen)32);
    if (failed_()) {
	chkout_("ZZGFLONG", (ftnlen)8);
	return 0;
    }

/*     We've done the basic error checking. Empty the result window and */
/*     return now if the confinement window is empty. */

    if (wncard_(cnfine) == 0) {
	scardd_(&c__0, result);
	chkout_("ZZGFLONG", (ftnlen)8);
	return 0;
    }

/*     Initialize the total number of search passes performed. */

    total = 0;

/*     To eliminate special cases, we'll check for inequality */
/*     constraints that are always met or can't be met. */

    if (s_cmp(nrmsys, "CYLINDRICAL", (ftnlen)32, (ftnlen)11) == 0 || s_cmp(
	    nrmsys, "PLANETOGRAPHIC", (ftnlen)32, (ftnlen)14) == 0 || s_cmp(
	    nrmsys, "RA/DEC", (ftnlen)32, (ftnlen)6) == 0) {
	if (cos(*refval) == 1.) {

/*           The reference value lies on the branch cut at 0. */

	    if (s_cmp(uop, "<", (ftnlen)6, (ftnlen)1) == 0) {

/*              These coordinates can never be less than zero. */

		scardd_(&c__0, result);
		chkout_("ZZGFLONG", (ftnlen)8);
		return 0;
	    } else if (s_cmp(uop, ">", (ftnlen)6, (ftnlen)1) == 0) {

/*              The solution is the whole confinement window. This */
/*              is because the inequality operators really act like */
/*              '>=' and '<=' operators, and because we assume the */
/*              quantity is increasing or decreasing except on a */
/*              set of measure zero. */

		copyd_(cnfine, result);
		chkout_("ZZGFLONG", (ftnlen)8);
		return 0;
	    }
	}
    } else if (s_cmp(nrmsys, "GEODETIC", (ftnlen)32, (ftnlen)8) == 0 || s_cmp(
	    nrmsys, "LATITUDINAL", (ftnlen)32, (ftnlen)11) == 0 || s_cmp(
	    nrmsys, "SPHERICAL", (ftnlen)32, (ftnlen)9) == 0) {
	if (cos(*refval) == -1.) {

/*           The reference value lies on the branch cut at pi. */

	    if (s_cmp(uop, "<", (ftnlen)6, (ftnlen)1) == 0) {

/*              The solution is the whole confinement window. */

		copyd_(cnfine, result);
		chkout_("ZZGFLONG", (ftnlen)8);
		return 0;
	    } else if (s_cmp(uop, ">", (ftnlen)6, (ftnlen)1) == 0) {

/*              These coordinates can never be greater */
/*              than pi. */

		scardd_(&c__0, result);
		chkout_("ZZGFLONG", (ftnlen)8);
		return 0;
	    }
	}
    }

/*     At this point, we make some adjustments to simplify the */
/*     remaining code. We map the input coordinate system to */
/*     either "latitudinal" or "RA/DEC" and modify the */
/*     constraint if the original system is "planetographic." */
/*     The longitude coordinate is renamed accordingly, if necessary. */
/*     The mapping is as follows: */

/*        Spherical      ( longitude range is (-pi, pi] ) -> Latitudinal */

/*        Cylindrical    ( longitude range is [0, 2*pi] ) -> RA/Dec */
/*           Longitude                                    -> RA */

/*        Planetographic ( longitude range is [0, 2*pi] ) -> RA/Dec */
/*           Longitude                                    -> RA */


/*     For planetographic coordinates, if the longitude is positive */
/*     west, and since REFVAL does not lie on the branch cut, we can */
/*     make the following additional adjustments: */

/*        Input relational operator           Transformed operator */
/*        -------------------------           -------------------- */
/*        ABSMAX                              ABSMIN */
/*        ABSMAX - ADJUST                     ABSMIN + ADJUST */
/*        ABSMIN                              ABSMAX */
/*        ABSMIN + AJDUST                     ABSMAX - ADJUST */
/*        LOCMAX                              LOCMIN */
/*        LOCMIN                              LOCMAX */
/*        <        REFVAL                     >        2*pi - REFVAL */
/*        >        REFVAL                     <        2*pi - REFVAL */
/*        =        REFVAL                     =        2*pi - REFVAL */


    xrfval = *refval;
    if (s_cmp(nrmsys, "SPHERICAL", (ftnlen)32, (ftnlen)9) == 0) {
	s_copy(nrmsys, "LATITUDINAL", (ftnlen)32, (ftnlen)11);
	xrfval = *refval;
    } else if (s_cmp(nrmsys, "CYLINDRICAL", (ftnlen)32, (ftnlen)11) == 0) {
	s_copy(nrmsys, "RA/DEC", (ftnlen)32, (ftnlen)6);
	s_copy(nrmcrd, "RIGHT ASCENSION", (ftnlen)32, (ftnlen)15);
	xrfval = *refval;
    } else if (s_cmp(nrmsys, "PLANETOGRAPHIC", (ftnlen)32, (ftnlen)14) == 0) {
	s_copy(nrmsys, "RA/DEC", (ftnlen)32, (ftnlen)6);
	s_copy(nrmcrd, "RIGHT ASCENSION", (ftnlen)32, (ftnlen)15);

/*        If the planetographic coordinates are positive West, we'll */
/*        need to transform the constraint and reference value. */

/*        Get the name of the central body of frame REF. */

/*        NOTE: We omit error checking here because ZZGFCOIN has done */
/*        it already. */

	namfrm_(ref, &frcode, ref_len);
	frinfo_(&frcode, &refctr, &class__, &clssid, &found);
	if (failed_()) {
	    chkout_("ZZGFLONG", (ftnlen)8);
	    return 0;
	}
	if (! found) {
	    setmsg_("FRINFO didn't find data for frame # which has frame ID "
		    "code #. This frame should have been validated by ZZGFCOI"
		    "N.", (ftnlen)113);
	    errch_("#", ref, (ftnlen)1, ref_len);
	    errint_("#", &frcode, (ftnlen)1);
	    sigerr_("SPICE(BUG)", (ftnlen)10);
	    chkout_("ZZGFLONG", (ftnlen)8);
	    return 0;
	}
	bodc2s_(&refctr, rctrnm, (ftnlen)36);

/*        Find the longitude of the +Y axis. If this longitude */
/*        is greater than pi, the sense is positive West. Note */
/*        that we don't need to use realistic values of the */
/*        equatorial radius and flattening factor: 1 and 0, */
/*        respectively, are just fine. */

	recpgr_(rctrnm, y, &c_b69, &c_b70, &lon, &lat, &alt, (ftnlen)36);

/*        Planetographic longitude ranges from 0 to 2*pi, so */
/*        longitudes corresponding to positive Y values are */
/*        in the range pi to 2*pi. */

	if (lon > pi_()) {

/*           Planetographic longitude for the frame center is positive */
/*           West. */

/*           Note that no action is required to modify non-zero */
/*           extremum adjustment values. */

	    if (s_cmp(uop, "ABSMAX", (ftnlen)6, (ftnlen)6) == 0) {
		s_copy(uop, "ABSMIN", (ftnlen)6, (ftnlen)6);
	    } else if (s_cmp(uop, "ABSMIN", (ftnlen)6, (ftnlen)6) == 0) {
		s_copy(uop, "ABSMAX", (ftnlen)6, (ftnlen)6);
	    } else if (s_cmp(uop, "LOCMAX", (ftnlen)6, (ftnlen)6) == 0) {
		s_copy(uop, "LOCMIN", (ftnlen)6, (ftnlen)6);
	    } else if (s_cmp(uop, "LOCMIN", (ftnlen)6, (ftnlen)6) == 0) {
		s_copy(uop, "LOCMAX", (ftnlen)6, (ftnlen)6);
	    } else if (s_cmp(uop, "=", (ftnlen)6, (ftnlen)1) == 0) {
		xrfval = twopi_() - *refval;
	    } else if (s_cmp(uop, "<", (ftnlen)6, (ftnlen)1) == 0) {
		s_copy(uop, ">", (ftnlen)6, (ftnlen)1);
		xrfval = twopi_() - *refval;
	    } else if (s_cmp(uop, ">", (ftnlen)6, (ftnlen)1) == 0) {
		s_copy(uop, "<", (ftnlen)6, (ftnlen)1);
		xrfval = twopi_() - *refval;
	    } else {

/*              We shouldn't get here. */

		setmsg_("Unexpected UOP value: #", (ftnlen)23);
		errch_("#", uop, (ftnlen)1, (ftnlen)6);
		sigerr_("SPICE(BUG)", (ftnlen)10);
		chkout_("ZZGFLONG", (ftnlen)8);
		return 0;
	    }
	} else {

/*           Longitude is positive East, so we treat */
/*           the constraint as though the coordinate were RA. */

	    xrfval = *refval;
	}
    }

/*     From this point on, we use: */

/*        Coordinate system:  NRMSYS */
/*        Coordinate:         NRMCRD */
/*        Operator:           UOP */
/*        Reference value:    XRFVAL */


/*     The result window must be initialized by the caller of the GF */
/*     system (usually a user application). We simply empty the result */
/*     window here. */

    scardd_(&c__0, result);

/*     We use the constant 0.5 * 2**0.5 quite a bit.  Create a */
/*     "macro" variable for it. */

    r2ovr2 = sqrt(2.) / 2.;

/*     Set the progress report suffix strings. */

    s_copy(rptsuf, "done.", (ftnlen)80, (ftnlen)5);
    s_copy(rptsuf + 80, "done.", (ftnlen)80, (ftnlen)5);

/*     Case:  '=' */

    if (s_cmp(uop, "=", (ftnlen)6, (ftnlen)1) == 0) {

/*        Equality constraints are the simplest to handle, so we'll get */
/*        them out of the way now. Our approach is to use sine or cosine */
/*        as proxy functions; we'll select the proxy function with the */
/*        highest resolution at the reference value. For the proxy */
/*        function f, our proxy constraint is */

/*           f(x) = f(XRFVAL) */

/*        This may yield spurious roots; we'll delete these after we've */
/*        done our search. */

/*        Find the sine and cosine of the reference value. We'll use */
/*        these both to locate the quadrant of the reference value and */
/*        to have continuously differentiable functions to work with. */
/*        Note that if the original reference value is not in the */
/*        standard range, this presents no problem. */

	cv = cos(xrfval);
	sv = sin(xrfval);

/*        Decide which proxy function to use. */

	if (abs(sv) >= r2ovr2) {

/*           The reference value lies in the top or bottom quarter of */
/*           the unit circle. The "comparison value" CMPVAL will be */
/*           used later to delete solutions with matching sines but */
/*           non-matching cosines. */

	    s_copy(prxfun, "COS", (ftnlen)50, (ftnlen)3);
	    prxval = cv;
	    cmpval = sv;
	} else {
	    s_copy(prxfun, "SIN", (ftnlen)50, (ftnlen)3);
	    prxval = sv;
	    cmpval = cv;
	}

/*        Set up the progress reporting prefix strings. We have one */
/*        ZZGFREL call which performs two passes. */

	s_copy(rptpre, "Coordinate pass 1 of 2", (ftnlen)80, (ftnlen)22);
	s_copy(rptpre + 80, "Coordinate pass 2 of 2", (ftnlen)80, (ftnlen)22);

/*        Allocate a workspace window. */

	lnkan_(wwpool, &node);
	f1 = wix[(i__1 = node - 1) < 7 && 0 <= i__1 ? i__1 : s_rnge("wix", 
		i__1, "zzgflong_", (ftnlen)1753)];

/*        Make sure the coordinate utilities have been initialized */
/*        with the actual values we'll use for our search. */

	zzgfcoin_(vecdef, method, target, ref, abcorr, obsrvr, dref, dvec, 
		nrmsys, nrmcrd, vecdef_len, method_len, target_len, ref_len, 
		abcorr_len, obsrvr_len, dref_len, (ftnlen)32, (ftnlen)32);

/*        Now we're ready to compute the window in which our proxy */
/*        function satisfies the proxy constraint. */

	if (s_cmp(prxfun, "SIN", (ftnlen)50, (ftnlen)3) == 0) {

/*           Find the window where the sine of the coordinate satisfies */
/*           the proxy constraint. */

	    zzgfrelx_((U_fp)udstep, (U_fp)udrefn, (U_fp)zzgfcosd_, (U_fp)
		    zzgfudlt_, (S_fp)zzgfcosg_, "=", &prxval, &loctol, &c_b70,
		     cnfine, mw, nw, work, rpt, (U_fp)udrepi, (U_fp)udrepu, (
		    U_fp)udrepf, rptpre, rptsuf, bail, (L_fp)udbail, &work[(
		    i__1 = f1 * work_dim1 - 5 - work_offset) < work_dim1 * 
		    work_dim2 && 0 <= i__1 ? i__1 : s_rnge("work", i__1, 
		    "zzgflong_", (ftnlen)1773)], (ftnlen)1, (ftnlen)80, (
		    ftnlen)80);
	} else {

/*           Find the window where the cosine of the coordinate */
/*           satisfies the proxy constraint. */

	    zzgfrelx_((U_fp)udstep, (U_fp)udrefn, (U_fp)zzgfcocd_, (U_fp)
		    zzgfudlt_, (S_fp)zzgfcocg_, "=", &prxval, &loctol, &c_b70,
		     cnfine, mw, nw, work, rpt, (U_fp)udrepi, (U_fp)udrepu, (
		    U_fp)udrepf, rptpre, rptsuf, bail, (L_fp)udbail, &work[(
		    i__1 = f1 * work_dim1 - 5 - work_offset) < work_dim1 * 
		    work_dim2 && 0 <= i__1 ? i__1 : s_rnge("work", i__1, 
		    "zzgflong_", (ftnlen)1788)], (ftnlen)1, (ftnlen)80, (
		    ftnlen)80);
	}
	if (failed_()) {
	    chkout_("ZZGFLONG", (ftnlen)8);
	    return 0;
	}

/*        Handle interrupts if necessary. */

	if (*bail) {
	    if ((*udbail)()) {
		chkout_("ZZGFLONG", (ftnlen)8);
		return 0;
	    }
	}

/*        Remove any spurious results. */

	n = cardd_(&work[(i__1 = f1 * work_dim1 - 5 - work_offset) < 
		work_dim1 * work_dim2 && 0 <= i__1 ? i__1 : s_rnge("work", 
		i__1, "zzgflong_", (ftnlen)1817)]);
	i__1 = n;
	for (i__ = 1; i__ <= i__1; i__ += 2) {
	    start = work[(i__2 = i__ + f1 * work_dim1 - work_offset) < 
		    work_dim1 * work_dim2 && 0 <= i__2 ? i__2 : s_rnge("work",
		     i__2, "zzgflong_", (ftnlen)1821)];
	    if (s_cmp(prxfun, "SIN", (ftnlen)50, (ftnlen)3) == 0) {

/*              Get the cosine of the coordinate at the interval start */
/*              time. If this cosine has the same sign as the cosine of */
/*              the reference value, we have a winner. Note that the */
/*              cosines of spurious values won't ever be close to the */
/*              correct values, so round-off isn't an issue. */

		zzgfcocg_(&start, &value);
	    } else {

/*              Same deal, but here we're using sines. */

		zzgfcosg_(&start, &value);
	    }
	    if (smsgnd_(&cmpval, &value)) {

/*              This is a winner. */

		wninsd_(&start, &start, result);
	    }
	}

/*        All done. */

	chkout_("ZZGFLONG", (ftnlen)8);
	return 0;
    }

/*     Case: local minimum or local maximum */

    if (s_cmp(uop, "LOCMAX", (ftnlen)6, (ftnlen)6) == 0 || s_cmp(uop, "LOCMIN"
	    , (ftnlen)6, (ftnlen)6) == 0) {

/*        This algorithm uses 4 ZZGFREL calls, 2 of which perform */
/*        2 passes and 2 of which perform 1 pass. */

	s_copy(rptsuf, "done.", (ftnlen)80, (ftnlen)5);
	s_copy(rptsuf + 80, "done.", (ftnlen)80, (ftnlen)5);

/*        Empty the result window. */

	scardd_(&c__0, result);

/*        We'll first find two windows covering the left and right */
/*        halves of the unit circle, with both halves extended */
/*        slightly to ensure no roots are missed. We start by */
/*        finding the window on which the cosine of the coordinate */
/*        is less than cos(LCXMRG) (which is a small, positive number). */

	lnkan_(wwpool, &node);
	left = wix[(i__1 = node - 1) < 7 && 0 <= i__1 ? i__1 : s_rnge("wix", 
		i__1, "zzgflong_", (ftnlen)1884)];
	s_copy(rptpre, "Coordinate pass 1 of 6", (ftnlen)80, (ftnlen)22);
	s_copy(rptpre + 80, "Coordinate pass 2 of 6", (ftnlen)80, (ftnlen)22);
	s_copy(prxrel, "<", (ftnlen)6, (ftnlen)1);
	prxval = cos(1e-12);
	zzgfcoin_(vecdef, method, target, ref, abcorr, obsrvr, dref, dvec, 
		nrmsys, nrmcrd, vecdef_len, method_len, target_len, ref_len, 
		abcorr_len, obsrvr_len, dref_len, (ftnlen)32, (ftnlen)32);
	zzgfrelx_((U_fp)udstep, (U_fp)udrefn, (U_fp)zzgfcocd_, (U_fp)
		zzgfudlt_, (S_fp)zzgfcocg_, prxrel, &prxval, &loctol, &c_b70, 
		cnfine, mw, nw, work, rpt, (U_fp)udrepi, (U_fp)udrepu, (U_fp)
		udrepf, rptpre, rptsuf, bail, (L_fp)udbail, &work[(i__1 = 
		left * work_dim1 - 5 - work_offset) < work_dim1 * work_dim2 &&
		 0 <= i__1 ? i__1 : s_rnge("work", i__1, "zzgflong_", (ftnlen)
		1896)], (ftnlen)6, (ftnlen)80, (ftnlen)80);

/*        Handle interrupts if necessary. */

	if (*bail) {
	    if ((*udbail)()) {
		chkout_("ZZGFLONG", (ftnlen)8);
		return 0;
	    }
	}

/*        Now search for the time period when the cosine of the */
/*        coordinate is greater than -cos(LCXMRG). We can save some time */
/*        by searching within the window designated by LEFT for the */
/*        complement of this window and then complementing the result of */
/*        that search. */

	lnkan_(wwpool, &node);
	compl = wix[(i__1 = node - 1) < 7 && 0 <= i__1 ? i__1 : s_rnge("wix", 
		i__1, "zzgflong_", (ftnlen)1923)];
	lnkan_(wwpool, &node);
	right = wix[(i__1 = node - 1) < 7 && 0 <= i__1 ? i__1 : s_rnge("wix", 
		i__1, "zzgflong_", (ftnlen)1926)];
	s_copy(rptpre, "Coordinate pass 3 of 6", (ftnlen)80, (ftnlen)22);
	s_copy(rptpre + 80, "Coordinate pass 4 of 6", (ftnlen)80, (ftnlen)22);
	s_copy(prxrel, "<", (ftnlen)6, (ftnlen)1);
	prxval = -cos(1e-12);
	zzgfcoin_(vecdef, method, target, ref, abcorr, obsrvr, dref, dvec, 
		nrmsys, nrmcrd, vecdef_len, method_len, target_len, ref_len, 
		abcorr_len, obsrvr_len, dref_len, (ftnlen)32, (ftnlen)32);
	zzgfrelx_((U_fp)udstep, (U_fp)udrefn, (U_fp)zzgfcocd_, (U_fp)
		zzgfudlt_, (S_fp)zzgfcocg_, prxrel, &prxval, &loctol, &c_b70, 
		&work[(i__1 = left * work_dim1 - 5 - work_offset) < work_dim1 
		* work_dim2 && 0 <= i__1 ? i__1 : s_rnge("work", i__1, "zzgf"
		"long_", (ftnlen)1939)], mw, nw, work, rpt, (U_fp)udrepi, (
		U_fp)udrepu, (U_fp)udrepf, rptpre, rptsuf, bail, (L_fp)udbail,
		 &work[(i__2 = compl * work_dim1 - 5 - work_offset) < 
		work_dim1 * work_dim2 && 0 <= i__2 ? i__2 : s_rnge("work", 
		i__2, "zzgflong_", (ftnlen)1939)], (ftnlen)6, (ftnlen)80, (
		ftnlen)80);

/*        Handle interrupts if necessary. */

	if (*bail) {
	    if ((*udbail)()) {
		chkout_("ZZGFLONG", (ftnlen)8);
		return 0;
	    }
	}

/*        WORK(LB,COMPL) contains the complement of the window */
/*        we want. */

	wndifd_(cnfine, &work[(i__1 = compl * work_dim1 - 5 - work_offset) < 
		work_dim1 * work_dim2 && 0 <= i__1 ? i__1 : s_rnge("work", 
		i__1, "zzgflong_", (ftnlen)1962)], &work[(i__2 = right * 
		work_dim1 - 5 - work_offset) < work_dim1 * work_dim2 && 0 <= 
		i__2 ? i__2 : s_rnge("work", i__2, "zzgflong_", (ftnlen)1962)]
		);

/*        We're now going to find local extrema of the coordinate in the */
/*        windows indexed by LEFT and RIGHT. */

	for (i__ = 1; i__ <= 2; ++i__) {
	    if (i__ == 1) {

/*              The sector we're searching is indexed by LEFT. */
/*              We'll use RA as a proxy function, since RA has no */
/*              singularity on the left half circle. */

		s = left;
		s_copy(prxsys, "RA/DEC", (ftnlen)32, (ftnlen)6);
		s_copy(prxcrd, "RIGHT ASCENSION", (ftnlen)32, (ftnlen)15);
		lnkan_(wwpool, &node);
		res1 = wix[(i__1 = node - 1) < 7 && 0 <= i__1 ? i__1 : s_rnge(
			"wix", i__1, "zzgflong_", (ftnlen)1981)];
		res = res1;
		s_copy(rptpre, "Coordinate pass 5 of 6", (ftnlen)80, (ftnlen)
			22);
		s_copy(rptpre + 80, " ", (ftnlen)80, (ftnlen)1);
	    } else {
		s = right;
		s_copy(prxsys, "LATITUDINAL", (ftnlen)32, (ftnlen)11);
		s_copy(prxcrd, "LONGITUDE", (ftnlen)32, (ftnlen)9);
		lnkan_(wwpool, &node);
		res2 = wix[(i__1 = node - 1) < 7 && 0 <= i__1 ? i__1 : s_rnge(
			"wix", i__1, "zzgflong_", (ftnlen)1994)];
		res = res2;
		s_copy(rptpre, "Coordinate pass 6 of 6", (ftnlen)80, (ftnlen)
			22);
		s_copy(rptpre + 80, " ", (ftnlen)80, (ftnlen)1);
	    }
	    zzgfcoin_(vecdef, method, target, ref, abcorr, obsrvr, dref, dvec,
		     prxsys, prxcrd, vecdef_len, method_len, target_len, 
		    ref_len, abcorr_len, obsrvr_len, dref_len, (ftnlen)32, (
		    ftnlen)32);
	    zzgfrelx_((U_fp)udstep, (U_fp)udrefn, (U_fp)zzgfcodc_, (U_fp)
		    zzgfudlt_, (S_fp)zzgfcog_, uop, &c_b70, &loctol, &c_b70, &
		    work[(i__1 = s * work_dim1 - 5 - work_offset) < work_dim1 
		    * work_dim2 && 0 <= i__1 ? i__1 : s_rnge("work", i__1, 
		    "zzgflong_", (ftnlen)2007)], mw, nw, work, rpt, (U_fp)
		    udrepi, (U_fp)udrepu, (U_fp)udrepf, rptpre, rptsuf, bail, 
		    (L_fp)udbail, &work[(i__2 = res * work_dim1 - 5 - 
		    work_offset) < work_dim1 * work_dim2 && 0 <= i__2 ? i__2 :
		     s_rnge("work", i__2, "zzgflong_", (ftnlen)2007)], (
		    ftnlen)6, (ftnlen)80, (ftnlen)80);

/*           Handle interrupts if necessary. */

	    if (*bail) {
		if ((*udbail)()) {
		    chkout_("ZZGFLONG", (ftnlen)8);
		    return 0;
		}
	    }
	}

/*        Combine the contributions of both searches in RESULT. */

	wnunid_(&work[(i__1 = res1 * work_dim1 - 5 - work_offset) < work_dim1 
		* work_dim2 && 0 <= i__1 ? i__1 : s_rnge("work", i__1, "zzgf"
		"long_", (ftnlen)2031)], &work[(i__2 = res2 * work_dim1 - 5 - 
		work_offset) < work_dim1 * work_dim2 && 0 <= i__2 ? i__2 : 
		s_rnge("work", i__2, "zzgflong_", (ftnlen)2031)], result);

/*        End of the LOCMIN and LOCMAX cases. RESULT is set. */

	chkout_("ZZGFLONG", (ftnlen)8);
	return 0;
    }

/*     The remaining operators are: ABSMAX, ABSMIN, '<', '>'. */

/*     Initialize the window aliases. A value of zero indicates the */
/*     corresponding region hasn't been computed. */

    top = 0;
    bot = 0;
    right = 0;
    left = 0;
    q1 = 0;
    q2 = 0;
    q3 = 0;
    q4 = 0;
    s = 0;
    wh = 0;
    f1 = 0;
    f2 = 0;

/*     If we have an absolute extremum or inequality relation, */
/*     we'll need to find times when the coordinate is in the */
/*     various quadrants. We'll start out by setting up windows */
/*     for the times when the coordinate is in the top and right */
/*     halves of the unit circle. */

/*     The ZZGFREL call below involves two passes. */

    if (s_cmp(uop, "ABSMAX", (ftnlen)6, (ftnlen)6) == 0 || s_cmp(uop, "ABSMIN"
	    , (ftnlen)6, (ftnlen)6) == 0) {
	if (*adjust == 0.) {
	    s_copy(tmplat, "Coordinate pass # of 7", (ftnlen)80, (ftnlen)22);
	} else {
	    s_copy(tmplat, "Coordinate pass # of 7-9", (ftnlen)80, (ftnlen)24)
		    ;
	}
    } else {

/*        Ordinary inequality searches use 8 passes. */

	s_copy(tmplat, "Coordinate pass # of 8", (ftnlen)80, (ftnlen)22);
    }
    for (i__ = 1; i__ <= 2; ++i__) {
	repmi_(tmplat, "#", &i__, rptpre + ((i__1 = i__ - 1) < 2 && 0 <= i__1 
		? i__1 : s_rnge("rptpre", i__1, "zzgflong_", (ftnlen)2087)) * 
		80, (ftnlen)80, (ftnlen)1, (ftnlen)80);
    }

/*     Find the window where the sine of the coordinate is greater than */
/*     the sine of the branch cut avoidance tolerance. */

/*     Make sure the coordinate utilities have been initialized */
/*     with the actual values we'll use for our search. */

    lnkan_(wwpool, &node);
    head = node;
    top = wix[(i__1 = node - 1) < 7 && 0 <= i__1 ? i__1 : s_rnge("wix", i__1, 
	    "zzgflong_", (ftnlen)2099)];
    prxval = sin(1e-11);
    zzgfcoin_(vecdef, method, target, ref, abcorr, obsrvr, dref, dvec, nrmsys,
	     nrmcrd, vecdef_len, method_len, target_len, ref_len, abcorr_len, 
	    obsrvr_len, dref_len, (ftnlen)32, (ftnlen)32);
    zzgfrelx_((U_fp)udstep, (U_fp)udrefn, (U_fp)zzgfcosd_, (U_fp)zzgfudlt_, (
	    S_fp)zzgfcosg_, ">", &prxval, &loctol, &c_b70, cnfine, mw, nw, 
	    work, rpt, (U_fp)udrepi, (U_fp)udrepu, (U_fp)udrepf, rptpre, 
	    rptsuf, bail, (L_fp)udbail, &work[(i__1 = top * work_dim1 - 5 - 
	    work_offset) < work_dim1 * work_dim2 && 0 <= i__1 ? i__1 : s_rnge(
	    "work", i__1, "zzgflong_", (ftnlen)2107)], (ftnlen)1, (ftnlen)80, 
	    (ftnlen)80);

/*     2 passes done. */

    total = 2;
    if (*bail) {
	if ((*udbail)()) {
	    chkout_("ZZGFLONG", (ftnlen)8);
	    return 0;
	}
    }

/*     Find the window where the sine of the coordinate is less than */
/*     the negative of the sine of the branch cut avoidance tolerance. */

/*     Make sure the coordinate utilities have been initialized */
/*     with the actual values we'll use for our search. */

/*     The ZZGFREL call below involves two passes. */

    for (i__ = 1; i__ <= 2; ++i__) {
	i__2 = total + i__;
	repmi_(tmplat, "#", &i__2, rptpre + ((i__1 = i__ - 1) < 2 && 0 <= 
		i__1 ? i__1 : s_rnge("rptpre", i__1, "zzgflong_", (ftnlen)
		2138)) * 80, (ftnlen)80, (ftnlen)1, (ftnlen)80);
    }
    lnkan_(wwpool, &node);
    lnkila_(&head, &node, wwpool);
    bot = wix[(i__1 = node - 1) < 7 && 0 <= i__1 ? i__1 : s_rnge("wix", i__1, 
	    "zzgflong_", (ftnlen)2144)];
    prxval = -sin(1e-11);
    zzgfcoin_(vecdef, method, target, ref, abcorr, obsrvr, dref, dvec, nrmsys,
	     nrmcrd, vecdef_len, method_len, target_len, ref_len, abcorr_len, 
	    obsrvr_len, dref_len, (ftnlen)32, (ftnlen)32);
    zzgfrelx_((U_fp)udstep, (U_fp)udrefn, (U_fp)zzgfcosd_, (U_fp)zzgfudlt_, (
	    S_fp)zzgfcosg_, "<", &prxval, &loctol, &c_b70, cnfine, mw, nw, 
	    work, rpt, (U_fp)udrepi, (U_fp)udrepu, (U_fp)udrepf, rptpre, 
	    rptsuf, bail, (L_fp)udbail, &work[(i__1 = bot * work_dim1 - 5 - 
	    work_offset) < work_dim1 * work_dim2 && 0 <= i__1 ? i__1 : s_rnge(
	    "work", i__1, "zzgflong_", (ftnlen)2153)], (ftnlen)1, (ftnlen)80, 
	    (ftnlen)80);

/*     4 passes done. */

    total += 2;
    if (*bail) {
	if ((*udbail)()) {
	    chkout_("ZZGFLONG", (ftnlen)8);
	    return 0;
	}
    }

/*     Find the window where the cosine of the coordinate is */
/*     greater than zero. */


/*     The ZZGFREL call below involves two passes. */

    for (i__ = 1; i__ <= 2; ++i__) {
	i__2 = total + i__;
	repmi_(tmplat, "#", &i__2, rptpre + ((i__1 = i__ - 1) < 2 && 0 <= 
		i__1 ? i__1 : s_rnge("rptpre", i__1, "zzgflong_", (ftnlen)
		2182)) * 80, (ftnlen)80, (ftnlen)1, (ftnlen)80);
    }

/*     We'll keep all of the allocated nodes linked together. */
/*     Since the order of the nodes is unimportant, we insert */
/*     each new node following the head node; this is non-standard */
/*     but ensures the list head doesn't change until we delete */
/*     nodes from the list. */

    lnkan_(wwpool, &node);
    lnkila_(&head, &node, wwpool);
    right = wix[(i__1 = node - 1) < 7 && 0 <= i__1 ? i__1 : s_rnge("wix", 
	    i__1, "zzgflong_", (ftnlen)2194)];
    zzgfcoin_(vecdef, method, target, ref, abcorr, obsrvr, dref, dvec, nrmsys,
	     nrmcrd, vecdef_len, method_len, target_len, ref_len, abcorr_len, 
	    obsrvr_len, dref_len, (ftnlen)32, (ftnlen)32);
    zzgfrelx_((U_fp)udstep, (U_fp)udrefn, (U_fp)zzgfcocd_, (U_fp)zzgfudlt_, (
	    S_fp)zzgfcocg_, ">", &c_b70, &loctol, &c_b70, cnfine, mw, nw, 
	    work, rpt, (U_fp)udrepi, (U_fp)udrepu, (U_fp)udrepf, rptpre, 
	    rptsuf, bail, (L_fp)udbail, &work[(i__1 = right * work_dim1 - 5 - 
	    work_offset) < work_dim1 * work_dim2 && 0 <= i__1 ? i__1 : s_rnge(
	    "work", i__1, "zzgflong_", (ftnlen)2200)], (ftnlen)1, (ftnlen)80, 
	    (ftnlen)80);

/*     6 passes done. */

    total += 2;
    if (*bail) {
	if ((*udbail)()) {
	    chkout_("ZZGFLONG", (ftnlen)8);
	    return 0;
	}
    }
    if (failed_()) {
	chkout_("ZZGFLONG", (ftnlen)8);
	return 0;
    }

/*     Now find the absolute extremum, if this was requested. */

    if (s_cmp(uop, "ABSMAX", (ftnlen)6, (ftnlen)6) == 0 || s_cmp(uop, "ABSMIN"
	    , (ftnlen)6, (ftnlen)6) == 0) {

/*        If we're looking for an absolute extremum and the */
/*        adjustment value is 0, each ZZGFREL call executes */
/*        on search pass; otherwise these calls execute two */
/*        search passes. */

	if (s_cmp(nrmcrd, "LONGITUDE", (ftnlen)32, (ftnlen)9) == 0) {

/*           We need windows when the coordinate is in quadrants 2 and */
/*           3. We can derive these from the windows TOP and RIGHT */
/*           without additional searches. */

	    lnkan_(wwpool, &node);
	    lnkila_(&head, &node, wwpool);
	    q2 = wix[(i__1 = node - 1) < 7 && 0 <= i__1 ? i__1 : s_rnge("wix",
		     i__1, "zzgflong_", (ftnlen)2246)];
	    lnkan_(wwpool, &node);
	    lnkila_(&head, &node, wwpool);
	    q3 = wix[(i__1 = node - 1) < 7 && 0 <= i__1 ? i__1 : s_rnge("wix",
		     i__1, "zzgflong_", (ftnlen)2250)];

/*           Compute windows for the second and third quadrants. Note */
/*           that these windows are bounded away from the branch cut */
/*           at pi radians, since windows TOP and BOT have been */
/*           trimmed. */

	    wndifd_(&work[(i__1 = top * work_dim1 - 5 - work_offset) < 
		    work_dim1 * work_dim2 && 0 <= i__1 ? i__1 : s_rnge("work",
		     i__1, "zzgflong_", (ftnlen)2258)], &work[(i__2 = right * 
		    work_dim1 - 5 - work_offset) < work_dim1 * work_dim2 && 0 
		    <= i__2 ? i__2 : s_rnge("work", i__2, "zzgflong_", (
		    ftnlen)2258)], &work[(i__3 = q2 * work_dim1 - 5 - 
		    work_offset) < work_dim1 * work_dim2 && 0 <= i__3 ? i__3 :
		     s_rnge("work", i__3, "zzgflong_", (ftnlen)2258)]);
	    wndifd_(&work[(i__1 = bot * work_dim1 - 5 - work_offset) < 
		    work_dim1 * work_dim2 && 0 <= i__1 ? i__1 : s_rnge("work",
		     i__1, "zzgflong_", (ftnlen)2259)], &work[(i__2 = right * 
		    work_dim1 - 5 - work_offset) < work_dim1 * work_dim2 && 0 
		    <= i__2 ? i__2 : s_rnge("work", i__2, "zzgflong_", (
		    ftnlen)2259)], &work[(i__3 = q3 * work_dim1 - 5 - 
		    work_offset) < work_dim1 * work_dim2 && 0 <= i__3 ? i__3 :
		     s_rnge("work", i__3, "zzgflong_", (ftnlen)2259)]);
	    if (s_cmp(uop, "ABSMAX", (ftnlen)6, (ftnlen)6) == 0) {
		region[0] = q2;
		region[1] = right;
		region[2] = q3;
	    } else {
		region[0] = q3;
		region[1] = right;
		region[2] = q2;
	    }
	} else if (s_cmp(nrmcrd, "RIGHT ASCENSION", (ftnlen)32, (ftnlen)15) ==
		 0) {

/*           We need windows when the coordinate is in quadrants 1 and */
/*           4, and the window when the coordinate is in the left half */
/*           of the unit circle. We can derive these from the windows */
/*           TOP and RIGHT without additional searches. */

	    lnkan_(wwpool, &node);
	    lnkila_(&head, &node, wwpool);
	    q1 = wix[(i__1 = node - 1) < 7 && 0 <= i__1 ? i__1 : s_rnge("wix",
		     i__1, "zzgflong_", (ftnlen)2282)];
	    lnkan_(wwpool, &node);
	    lnkila_(&head, &node, wwpool);
	    left = wix[(i__1 = node - 1) < 7 && 0 <= i__1 ? i__1 : s_rnge(
		    "wix", i__1, "zzgflong_", (ftnlen)2286)];
	    lnkan_(wwpool, &node);
	    lnkila_(&head, &node, wwpool);
	    q4 = wix[(i__1 = node - 1) < 7 && 0 <= i__1 ? i__1 : s_rnge("wix",
		     i__1, "zzgflong_", (ftnlen)2290)];

/*           Compute windows for the first and fourth quadrants. Note */
/*           that these windows are bounded away from the branch cut */
/*           at pi radians, since windows TOP and BOT have been */
/*           trimmed. Also compute the window LEFT, which is the */
/*           complement of window RIGHT. */

	    wnintd_(&work[(i__1 = right * work_dim1 - 5 - work_offset) < 
		    work_dim1 * work_dim2 && 0 <= i__1 ? i__1 : s_rnge("work",
		     i__1, "zzgflong_", (ftnlen)2299)], &work[(i__2 = top * 
		    work_dim1 - 5 - work_offset) < work_dim1 * work_dim2 && 0 
		    <= i__2 ? i__2 : s_rnge("work", i__2, "zzgflong_", (
		    ftnlen)2299)], &work[(i__3 = q1 * work_dim1 - 5 - 
		    work_offset) < work_dim1 * work_dim2 && 0 <= i__3 ? i__3 :
		     s_rnge("work", i__3, "zzgflong_", (ftnlen)2299)]);
	    wnintd_(&work[(i__1 = right * work_dim1 - 5 - work_offset) < 
		    work_dim1 * work_dim2 && 0 <= i__1 ? i__1 : s_rnge("work",
		     i__1, "zzgflong_", (ftnlen)2300)], &work[(i__2 = bot * 
		    work_dim1 - 5 - work_offset) < work_dim1 * work_dim2 && 0 
		    <= i__2 ? i__2 : s_rnge("work", i__2, "zzgflong_", (
		    ftnlen)2300)], &work[(i__3 = q4 * work_dim1 - 5 - 
		    work_offset) < work_dim1 * work_dim2 && 0 <= i__3 ? i__3 :
		     s_rnge("work", i__3, "zzgflong_", (ftnlen)2300)]);
	    wndifd_(cnfine, &work[(i__1 = right * work_dim1 - 5 - work_offset)
		     < work_dim1 * work_dim2 && 0 <= i__1 ? i__1 : s_rnge(
		    "work", i__1, "zzgflong_", (ftnlen)2301)], &work[(i__2 = 
		    left * work_dim1 - 5 - work_offset) < work_dim1 * 
		    work_dim2 && 0 <= i__2 ? i__2 : s_rnge("work", i__2, 
		    "zzgflong_", (ftnlen)2301)]);
	    if (s_cmp(uop, "ABSMAX", (ftnlen)6, (ftnlen)6) == 0) {
		region[0] = q4;
		region[1] = left;
		region[2] = q1;
	    } else {
		region[0] = q1;
		region[1] = left;
		region[2] = q4;
	    }
	} else {

/*           We're not expecting to see a coordinate other than */
/*           longitude or RA here. */

	    setmsg_("Unexpected coordinate # (0)", (ftnlen)27);
	    errch_("#", nrmcrd, (ftnlen)1, (ftnlen)32);
	    sigerr_("SPICE(BUG)", (ftnlen)10);
	    chkout_("ZZGFLONG", (ftnlen)8);
	    return 0;
	}

/*        Now search the list of regions for the specified */
/*        extremum. */

	found = FALSE_;
	i__ = 1;
	while(i__ <= 3 && ! found) {

/*           Search region I. Set the reference and adjustment */
/*           values to 0 for this search. */

/*           The ZZGFREL call below executes 1 pass, since it's */
/*           doing an absolute extremum search with 0 adjustment */
/*           value (even if ADJUST is non-zero). */

	    i__1 = total + 1;
	    repmi_(tmplat, "#", &i__1, rptpre, (ftnlen)80, (ftnlen)1, (ftnlen)
		    80);
	    s_copy(rptpre + 80, " ", (ftnlen)80, (ftnlen)1);
	    scardd_(&c__0, result);

/*           Perform our searches with functions that have no branch */
/*           cuts near the region boundaries. */

	    if (region[(i__1 = i__ - 1) < 3 && 0 <= i__1 ? i__1 : s_rnge(
		    "region", i__1, "zzgflong_", (ftnlen)2351)] == q1 || 
		    region[(i__2 = i__ - 1) < 3 && 0 <= i__2 ? i__2 : s_rnge(
		    "region", i__2, "zzgflong_", (ftnlen)2351)] == q4 || 
		    region[(i__3 = i__ - 1) < 3 && 0 <= i__3 ? i__3 : s_rnge(
		    "region", i__3, "zzgflong_", (ftnlen)2351)] == right) {
		s_copy(prxsys, "LATITUDINAL", (ftnlen)32, (ftnlen)11);
		s_copy(prxcrd, "LONGITUDE", (ftnlen)32, (ftnlen)9);
	    } else {
		s_copy(prxsys, "RA/DEC", (ftnlen)32, (ftnlen)6);
		s_copy(prxcrd, "RIGHT ASCENSION", (ftnlen)32, (ftnlen)15);
	    }
	    zzgfcoin_(vecdef, method, target, ref, abcorr, obsrvr, dref, dvec,
		     prxsys, prxcrd, vecdef_len, method_len, target_len, 
		    ref_len, abcorr_len, obsrvr_len, dref_len, (ftnlen)32, (
		    ftnlen)32);
	    zzgfrelx_((U_fp)udstep, (U_fp)udrefn, (U_fp)zzgfcodc_, (U_fp)
		    zzgfudlt_, (S_fp)zzgfcocg_, uop, &c_b70, &loctol, &c_b70, 
		    &work[(i__2 = region[(i__1 = i__ - 1) < 3 && 0 <= i__1 ? 
		    i__1 : s_rnge("region", i__1, "zzgflong_", (ftnlen)2366)] 
		    * work_dim1 - 5 - work_offset) < work_dim1 * work_dim2 && 
		    0 <= i__2 ? i__2 : s_rnge("work", i__2, "zzgflong_", (
		    ftnlen)2366)], mw, nw, work, rpt, (U_fp)udrepi, (U_fp)
		    udrepu, (U_fp)udrepf, rptpre, rptsuf, bail, (L_fp)udbail, 
		    result, (ftnlen)6, (ftnlen)80, (ftnlen)80);

/*           ZZGFREL will have performed a pass only if the confinement */
/*           window was non-empty. */

	    if (cardd_(&work[(i__2 = region[(i__1 = i__ - 1) < 3 && 0 <= i__1 
		    ? i__1 : s_rnge("region", i__1, "zzgflong_", (ftnlen)2379)
		    ] * work_dim1 - 5 - work_offset) < work_dim1 * work_dim2 
		    && 0 <= i__2 ? i__2 : s_rnge("work", i__2, "zzgflong_", (
		    ftnlen)2379)]) > 0) {

/*              Another pass has been completed. */

		++total;
	    }
	    if (*bail) {
		if ((*udbail)()) {
		    chkout_("ZZGFLONG", (ftnlen)8);
		    return 0;
		}
	    }
	    if (wncard_(result) > 0) {

/*              We found an extremum. We don't have to search further. */

		found = TRUE_;
	    } else {
		++i__;
	    }
	}
	if (*adjust == 0.) {

/*           The result we have is the final result. */

	    chkout_("ZZGFLONG", (ftnlen)8);
	    return 0;
	}

/*        This is the case of an absolute extremum search with */
/*        non-zero adjustment value. */

/*        We'll need to obtain the extreme value. */

	et = result[6];
	zzgfcoin_(vecdef, method, target, ref, abcorr, obsrvr, dref, dvec, 
		nrmsys, nrmcrd, vecdef_len, method_len, target_len, ref_len, 
		abcorr_len, obsrvr_len, dref_len, (ftnlen)32, (ftnlen)32);
	zzgfcog_(&et, &extval);

/*        Re-set the operator and reference value to enable */
/*        us to conduct an inequality search. */

	if (s_cmp(uop, "ABSMAX", (ftnlen)6, (ftnlen)6) == 0) {
	    if (s_cmp(nrmcrd, "LONGITUDE", (ftnlen)32, (ftnlen)9) == 0) {
/* Computing MAX */
		d__1 = extval - *adjust, d__2 = -pi_();
		xrfval = max(d__1,d__2);
	    } else {
/* Computing MAX */
		d__1 = extval - *adjust;
		xrfval = max(d__1,0.);
	    }
	    s_copy(uop, ">", (ftnlen)6, (ftnlen)1);
	} else {
	    if (s_cmp(nrmcrd, "LONGITUDE", (ftnlen)32, (ftnlen)9) == 0) {
/* Computing MIN */
		d__1 = extval + *adjust, d__2 = pi_();
		xrfval = min(d__1,d__2);
	    } else {
/* Computing MIN */
		d__1 = extval + *adjust, d__2 = twopi_();
		xrfval = min(d__1,d__2);
	    }
	    s_copy(uop, "<", (ftnlen)6, (ftnlen)1);
	}
    }

/*     Case: inequality */

/*     Searches for absolute extrema with non-zero adjustment values */
/*     also use this code block. */

    if (s_cmp(uop, "<", (ftnlen)6, (ftnlen)1) == 0 || s_cmp(uop, ">", (ftnlen)
	    6, (ftnlen)1) == 0) {

/*        We'll find the window when the coordinate is less than */
/*        the reference value. If the relation is '>', we'll */
/*        complement the result. Let FLIP indicate whether */
/*        we need to take the complement of our result at the */
/*        end of the search. */

	if (s_cmp(uop, ">", (ftnlen)6, (ftnlen)1) == 0) {
	    s_copy(uop, "<", (ftnlen)6, (ftnlen)1);
	    flip = TRUE_;
	} else {
	    flip = FALSE_;
	}

/*        We'll need the sine and cosine of the reference value. */

	cv = cos(xrfval);
	sv = sin(xrfval);

/*        Determine the quadrant QUAD of the reference value. */

	locref = atan2(sv, cv);
	if (locref < -pi_() / 2) {
	    quad = 3;
	} else if (locref < 0.) {
	    quad = 4;
	} else if (locref < pi_() / 2) {
	    quad = 1;
	} else {
	    quad = 2;
	}

/*        Create a list of region windows to compute. The order */
/*        of list items is significant: the regions will */
/*        be computed in the order in which they're listed. */

	if (s_cmp(nrmcrd, "LONGITUDE", (ftnlen)32, (ftnlen)9) == 0) {
	    nl = 2;
	    s_copy(rlist, "Q2", (ftnlen)32, (ftnlen)2);
	    s_copy(rlist + 32, "Q3", (ftnlen)32, (ftnlen)2);
	} else {
	    nl = 3;
	    s_copy(rlist, "LEFT", (ftnlen)32, (ftnlen)4);
	    s_copy(rlist + 32, "Q1", (ftnlen)32, (ftnlen)2);
	    s_copy(rlist + 64, "Q4", (ftnlen)32, (ftnlen)2);
	}

/*        Compute all of the region windows. */

/*        We make use of the fact that windows TOP and RIGHT */
/*        have already been computed. */

	i__1 = nl;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    if (s_cmp(rlist + (((i__2 = i__ - 1) < 7 && 0 <= i__2 ? i__2 : 
		    s_rnge("rlist", i__2, "zzgflong_", (ftnlen)2532)) << 5), 
		    "LEFT", (ftnlen)32, (ftnlen)4) == 0 && left == 0) {
		lnkan_(wwpool, &node);
		lnkila_(&head, &node, wwpool);
		left = wix[(i__2 = node - 1) < 7 && 0 <= i__2 ? i__2 : s_rnge(
			"wix", i__2, "zzgflong_", (ftnlen)2536)];
		wndifd_(cnfine, &work[(i__2 = right * work_dim1 - 5 - 
			work_offset) < work_dim1 * work_dim2 && 0 <= i__2 ? 
			i__2 : s_rnge("work", i__2, "zzgflong_", (ftnlen)2538)
			], &work[(i__3 = left * work_dim1 - 5 - work_offset) <
			 work_dim1 * work_dim2 && 0 <= i__3 ? i__3 : s_rnge(
			"work", i__3, "zzgflong_", (ftnlen)2538)]);
	    } else if (s_cmp(rlist + (((i__2 = i__ - 1) < 7 && 0 <= i__2 ? 
		    i__2 : s_rnge("rlist", i__2, "zzgflong_", (ftnlen)2540)) 
		    << 5), "Q1", (ftnlen)32, (ftnlen)2) == 0 && q1 == 0) {
		if (q1 == 0) {
		    lnkan_(wwpool, &node);
		    lnkila_(&head, &node, wwpool);
		    q1 = wix[(i__2 = node - 1) < 7 && 0 <= i__2 ? i__2 : 
			    s_rnge("wix", i__2, "zzgflong_", (ftnlen)2546)];
		}
		wnintd_(&work[(i__2 = right * work_dim1 - 5 - work_offset) < 
			work_dim1 * work_dim2 && 0 <= i__2 ? i__2 : s_rnge(
			"work", i__2, "zzgflong_", (ftnlen)2550)], &work[(
			i__3 = top * work_dim1 - 5 - work_offset) < work_dim1 
			* work_dim2 && 0 <= i__3 ? i__3 : s_rnge("work", i__3,
			 "zzgflong_", (ftnlen)2550)], &work[(i__4 = q1 * 
			work_dim1 - 5 - work_offset) < work_dim1 * work_dim2 
			&& 0 <= i__4 ? i__4 : s_rnge("work", i__4, "zzgflong_"
			, (ftnlen)2550)]);
	    } else if (s_cmp(rlist + (((i__2 = i__ - 1) < 7 && 0 <= i__2 ? 
		    i__2 : s_rnge("rlist", i__2, "zzgflong_", (ftnlen)2553)) 
		    << 5), "Q2", (ftnlen)32, (ftnlen)2) == 0 && q2 == 0) {
		lnkan_(wwpool, &node);
		lnkila_(&head, &node, wwpool);
		q2 = wix[(i__2 = node - 1) < 7 && 0 <= i__2 ? i__2 : s_rnge(
			"wix", i__2, "zzgflong_", (ftnlen)2557)];
		wndifd_(&work[(i__2 = top * work_dim1 - 5 - work_offset) < 
			work_dim1 * work_dim2 && 0 <= i__2 ? i__2 : s_rnge(
			"work", i__2, "zzgflong_", (ftnlen)2559)], &work[(
			i__3 = right * work_dim1 - 5 - work_offset) < 
			work_dim1 * work_dim2 && 0 <= i__3 ? i__3 : s_rnge(
			"work", i__3, "zzgflong_", (ftnlen)2559)], &work[(
			i__4 = q2 * work_dim1 - 5 - work_offset) < work_dim1 *
			 work_dim2 && 0 <= i__4 ? i__4 : s_rnge("work", i__4, 
			"zzgflong_", (ftnlen)2559)]);
	    } else if (s_cmp(rlist + (((i__2 = i__ - 1) < 7 && 0 <= i__2 ? 
		    i__2 : s_rnge("rlist", i__2, "zzgflong_", (ftnlen)2562)) 
		    << 5), "Q3", (ftnlen)32, (ftnlen)2) == 0 && q3 == 0) {

/*              Note: we need the bottom window in order to compute Q3! */

		lnkan_(wwpool, &node);
		lnkila_(&head, &node, wwpool);
		q3 = wix[(i__2 = node - 1) < 7 && 0 <= i__2 ? i__2 : s_rnge(
			"wix", i__2, "zzgflong_", (ftnlen)2568)];
		wndifd_(&work[(i__2 = bot * work_dim1 - 5 - work_offset) < 
			work_dim1 * work_dim2 && 0 <= i__2 ? i__2 : s_rnge(
			"work", i__2, "zzgflong_", (ftnlen)2570)], &work[(
			i__3 = right * work_dim1 - 5 - work_offset) < 
			work_dim1 * work_dim2 && 0 <= i__3 ? i__3 : s_rnge(
			"work", i__3, "zzgflong_", (ftnlen)2570)], &work[(
			i__4 = q3 * work_dim1 - 5 - work_offset) < work_dim1 *
			 work_dim2 && 0 <= i__4 ? i__4 : s_rnge("work", i__4, 
			"zzgflong_", (ftnlen)2570)]);
	    } else if (s_cmp(rlist + (((i__2 = i__ - 1) < 7 && 0 <= i__2 ? 
		    i__2 : s_rnge("rlist", i__2, "zzgflong_", (ftnlen)2573)) 
		    << 5), "Q4", (ftnlen)32, (ftnlen)2) == 0 && q4 == 0) {

/*              NOTE: We need the bottom window in order to compute Q4! */

		lnkan_(wwpool, &node);
		lnkila_(&head, &node, wwpool);
		q4 = wix[(i__2 = node - 1) < 7 && 0 <= i__2 ? i__2 : s_rnge(
			"wix", i__2, "zzgflong_", (ftnlen)2579)];
		wnintd_(&work[(i__2 = right * work_dim1 - 5 - work_offset) < 
			work_dim1 * work_dim2 && 0 <= i__2 ? i__2 : s_rnge(
			"work", i__2, "zzgflong_", (ftnlen)2581)], &work[(
			i__3 = bot * work_dim1 - 5 - work_offset) < work_dim1 
			* work_dim2 && 0 <= i__3 ? i__3 : s_rnge("work", i__3,
			 "zzgflong_", (ftnlen)2581)], &work[(i__4 = q4 * 
			work_dim1 - 5 - work_offset) < work_dim1 * work_dim2 
			&& 0 <= i__4 ? i__4 : s_rnge("work", i__4, "zzgflong_"
			, (ftnlen)2581)]);
	    }
	}
	if (failed_()) {
	    chkout_("ZZGFLONG", (ftnlen)8);
	    return 0;
	}

/*        Now decide the sector and proxy function we'll use to */
/*        search for the time when the reference value is hit. */

	if (s_cmp(nrmcrd, "LONGITUDE", (ftnlen)32, (ftnlen)9) == 0) {
	    if (quad == 1) {
		s = right;
		s_copy(prxfun, "LONGITUDE", (ftnlen)50, (ftnlen)9);
	    } else if (quad == 2) {
		s = q2;
		s_copy(prxfun, "RIGHT ASCENSION", (ftnlen)50, (ftnlen)15);
	    } else if (quad == 3) {
		s = q3;
		s_copy(prxfun, "RIGHT ASCENSION", (ftnlen)50, (ftnlen)15);
	    } else {
		s = right;
		s_copy(prxfun, "LONGITUDE", (ftnlen)50, (ftnlen)9);
	    }
	} else {
	    if (quad == 1) {
		s = q1;
		s_copy(prxfun, "LONGITUDE", (ftnlen)50, (ftnlen)9);
	    } else if (quad == 2) {
		s = left;
		s_copy(prxfun, "RIGHT ASCENSION", (ftnlen)50, (ftnlen)15);
	    } else if (quad == 3) {
		s = left;
		s_copy(prxfun, "RIGHT ASCENSION", (ftnlen)50, (ftnlen)15);
	    } else {
		s = q4;
		s_copy(prxfun, "LONGITUDE", (ftnlen)50, (ftnlen)9);
	    }
	}

/*        Set the proxy reference value based on the input */
/*        reference value and the choice of proxy function. */

	if (s_cmp(prxfun, "LONGITUDE", (ftnlen)50, (ftnlen)9) == 0) {
	    prxval = atan2(sv, cv);
	} else {
	    prxval = atan2(sv, cv);
	    if (prxval < 0.) {
		prxval += twopi_();
	    }
	}

/*        We're going to need additional windows in order to search */
/*        quadrant Q. At this point, we're going to de-allocate all */
/*        windows except those needed for the upcoming searches. */

/*        Create the set NEEDWN of the windows we need to retain. */

	ssizei_(&c__7, needwn);
	if (s_cmp(nrmcrd, "LONGITUDE", (ftnlen)32, (ftnlen)9) == 0) {
	    insrti_(&q2, needwn);
	    insrti_(&q3, needwn);
	    insrti_(&right, needwn);
	} else {
	    insrti_(&q1, needwn);
	    insrti_(&q4, needwn);
	    insrti_(&left, needwn);
	}

/*        Now delete all windows not referenced by NEEDWN. */

	node = head;
	while(node > 0) {

/*           Find the next node in the list. */

	    next = lnknxt_(&node, wwpool);
	    if (! elemi_(&wix[(i__1 = node - 1) < 7 && 0 <= i__1 ? i__1 : 
		    s_rnge("wix", i__1, "zzgflong_", (ftnlen)2698)], needwn)) 
		    {

/*              Delete NODE; update HEAD if we deleted the head node. */

		lnkfsl_(&node, &node, wwpool);
		if (head == node) {
		    head = next;
		}
	    }

/*           Prepare to look at the next node. */

	    node = next;
	}
	if (s_cmp(nrmcrd, "LONGITUDE", (ftnlen)32, (ftnlen)9) == 0) {

/*           This is a longitude search. */

/*           For each quadrant, identify or compute the window on which */
/*           the constraint is automatically satisfied. Store the result */
/*           in workspace window F1. If this window is empty, set F1 to */
/*           0. */

	    if (quad == 1) {
		f1 = q3;
	    } else if (quad == 2) {
		lnkan_(wwpool, &node);
		lnkila_(&head, &node, wwpool);
		f1 = wix[(i__1 = node - 1) < 7 && 0 <= i__1 ? i__1 : s_rnge(
			"wix", i__1, "zzgflong_", (ftnlen)2735)];
		wnunid_(&work[(i__1 = q3 * work_dim1 - 5 - work_offset) < 
			work_dim1 * work_dim2 && 0 <= i__1 ? i__1 : s_rnge(
			"work", i__1, "zzgflong_", (ftnlen)2737)], &work[(
			i__2 = right * work_dim1 - 5 - work_offset) < 
			work_dim1 * work_dim2 && 0 <= i__2 ? i__2 : s_rnge(
			"work", i__2, "zzgflong_", (ftnlen)2737)], &work[(
			i__3 = f1 * work_dim1 - 5 - work_offset) < work_dim1 *
			 work_dim2 && 0 <= i__3 ? i__3 : s_rnge("work", i__3, 
			"zzgflong_", (ftnlen)2737)]);
	    } else if (quad == 3) {
		f1 = 0;
	    } else {

/*              QUAD is 4. */

		f1 = q3;
	    }
	} else {

/*           We're working with RA. */

	    if (quad == 1) {
		f1 = 0;
	    } else if (quad == 2) {
		f1 = q1;
	    } else if (quad == 3) {
		f1 = q1;
	    } else {

/*              QUAD is 4. */

		lnkan_(wwpool, &node);
		lnkila_(&head, &node, wwpool);
		f1 = wix[(i__1 = node - 1) < 7 && 0 <= i__1 ? i__1 : s_rnge(
			"wix", i__1, "zzgflong_", (ftnlen)2774)];
		wnunid_(&work[(i__1 = left * work_dim1 - 5 - work_offset) < 
			work_dim1 * work_dim2 && 0 <= i__1 ? i__1 : s_rnge(
			"work", i__1, "zzgflong_", (ftnlen)2776)], &work[(
			i__2 = q1 * work_dim1 - 5 - work_offset) < work_dim1 *
			 work_dim2 && 0 <= i__2 ? i__2 : s_rnge("work", i__2, 
			"zzgflong_", (ftnlen)2776)], &work[(i__3 = f1 * 
			work_dim1 - 5 - work_offset) < work_dim1 * work_dim2 
			&& 0 <= i__3 ? i__3 : s_rnge("work", i__3, "zzgflong_"
			, (ftnlen)2776)]);
	    }
	}
	if (failed_()) {
	    chkout_("ZZGFLONG", (ftnlen)8);
	    return 0;
	}

/*        Search sector S to find times when the relation */

/*           PRXFUN PRXREL PRXVAL */

/*        holds. */

/*        Allocate window F2 to hold the result of the search. */


	for (i__ = 1; i__ <= 2; ++i__) {
	    i__2 = total + i__;
	    repmi_(tmplat, "#", &i__2, rptpre + ((i__1 = i__ - 1) < 2 && 0 <= 
		    i__1 ? i__1 : s_rnge("rptpre", i__1, "zzgflong_", (ftnlen)
		    2798)) * 80, (ftnlen)80, (ftnlen)1, (ftnlen)80);
	}
	lnkan_(wwpool, &node);
	lnkila_(&head, &node, wwpool);
	f2 = wix[(i__1 = node - 1) < 7 && 0 <= i__1 ? i__1 : s_rnge("wix", 
		i__1, "zzgflong_", (ftnlen)2804)];
	scardd_(&c__0, &work[(i__1 = f2 * work_dim1 - 5 - work_offset) < 
		work_dim1 * work_dim2 && 0 <= i__1 ? i__1 : s_rnge("work", 
		i__1, "zzgflong_", (ftnlen)2806)]);
	if (s_cmp(prxfun, "LONGITUDE", (ftnlen)50, (ftnlen)9) == 0) {

/*           Initialize the proxy search in sector S, then perform the */
/*           search. */

	    zzgfcoin_(vecdef, method, target, ref, abcorr, obsrvr, dref, dvec,
		     "LATITUDINAL", "LONGITUDE", vecdef_len, method_len, 
		    target_len, ref_len, abcorr_len, obsrvr_len, dref_len, (
		    ftnlen)11, (ftnlen)9);
	    zzgfrelx_((U_fp)udstep, (U_fp)udrefn, (U_fp)zzgfcodc_, (U_fp)
		    zzgfudlt_, (S_fp)zzgfcog_, "<", &prxval, &loctol, &c_b70, 
		    &work[(i__1 = s * work_dim1 - 5 - work_offset) < 
		    work_dim1 * work_dim2 && 0 <= i__1 ? i__1 : s_rnge("work",
		     i__1, "zzgflong_", (ftnlen)2818)], mw, nw, work, rpt, (
		    U_fp)udrepi, (U_fp)udrepu, (U_fp)udrepf, rptpre, rptsuf, 
		    bail, (L_fp)udbail, &work[(i__2 = f2 * work_dim1 - 5 - 
		    work_offset) < work_dim1 * work_dim2 && 0 <= i__2 ? i__2 :
		     s_rnge("work", i__2, "zzgflong_", (ftnlen)2818)], (
		    ftnlen)1, (ftnlen)80, (ftnlen)80);
	} else {

/*           Initialize the proxy search in sector S, then perform the */
/*           search. */

	    zzgfcoin_(vecdef, method, target, ref, abcorr, obsrvr, dref, dvec,
		     "RA/DEC", "RIGHT ASCENSION", vecdef_len, method_len, 
		    target_len, ref_len, abcorr_len, obsrvr_len, dref_len, (
		    ftnlen)6, (ftnlen)15);
	    zzgfrelx_((U_fp)udstep, (U_fp)udrefn, (U_fp)zzgfcodc_, (U_fp)
		    zzgfudlt_, (S_fp)zzgfcog_, "<", &prxval, &loctol, &c_b70, 
		    &work[(i__1 = s * work_dim1 - 5 - work_offset) < 
		    work_dim1 * work_dim2 && 0 <= i__1 ? i__1 : s_rnge("work",
		     i__1, "zzgflong_", (ftnlen)2837)], mw, nw, work, rpt, (
		    U_fp)udrepi, (U_fp)udrepu, (U_fp)udrepf, rptpre, rptsuf, 
		    bail, (L_fp)udbail, &work[(i__2 = f2 * work_dim1 - 5 - 
		    work_offset) < work_dim1 * work_dim2 && 0 <= i__2 ? i__2 :
		     s_rnge("work", i__2, "zzgflong_", (ftnlen)2837)], (
		    ftnlen)1, (ftnlen)80, (ftnlen)80);
	}

/*        7 + 0:2 passes done for adjusted extrema. */

	if (*bail) {
	    if ((*udbail)()) {
		chkout_("ZZGFLONG", (ftnlen)8);
		return 0;
	    }
	}

/*        Combine the contents of windows F1 and F2 to obtain */
/*        the result. */

	if (f1 != 0) {
	    wnunid_(&work[(i__1 = f1 * work_dim1 - 5 - work_offset) < 
		    work_dim1 * work_dim2 && 0 <= i__1 ? i__1 : s_rnge("work",
		     i__1, "zzgflong_", (ftnlen)2864)], &work[(i__2 = f2 * 
		    work_dim1 - 5 - work_offset) < work_dim1 * work_dim2 && 0 
		    <= i__2 ? i__2 : s_rnge("work", i__2, "zzgflong_", (
		    ftnlen)2864)], result);
	} else {
	    copyd_(&work[(i__1 = f2 * work_dim1 - 5 - work_offset) < 
		    work_dim1 * work_dim2 && 0 <= i__1 ? i__1 : s_rnge("work",
		     i__1, "zzgflong_", (ftnlen)2868)], result);
	}

/*        Last step: complement the result if necessary. */

	if (flip) {

/*           Create the window relative to which we'll find */
/*           the complement of RESULT. The window we seek */
/*           is not CNFINE, but rather a union of windows */
/*           that avoids the branch cut. */

	    lnkan_(wwpool, &node);
	    wh = wix[(i__1 = node - 1) < 7 && 0 <= i__1 ? i__1 : s_rnge("wix",
		     i__1, "zzgflong_", (ftnlen)2884)];
	    if (s_cmp(nrmcrd, "LONGITUDE", (ftnlen)32, (ftnlen)9) == 0) {
		wnunid_(&work[(i__1 = q2 * work_dim1 - 5 - work_offset) < 
			work_dim1 * work_dim2 && 0 <= i__1 ? i__1 : s_rnge(
			"work", i__1, "zzgflong_", (ftnlen)2888)], &work[(
			i__2 = right * work_dim1 - 5 - work_offset) < 
			work_dim1 * work_dim2 && 0 <= i__2 ? i__2 : s_rnge(
			"work", i__2, "zzgflong_", (ftnlen)2888)], &work[(
			i__3 = f2 * work_dim1 - 5 - work_offset) < work_dim1 *
			 work_dim2 && 0 <= i__3 ? i__3 : s_rnge("work", i__3, 
			"zzgflong_", (ftnlen)2888)]);
		wnunid_(&work[(i__1 = q3 * work_dim1 - 5 - work_offset) < 
			work_dim1 * work_dim2 && 0 <= i__1 ? i__1 : s_rnge(
			"work", i__1, "zzgflong_", (ftnlen)2889)], &work[(
			i__2 = f2 * work_dim1 - 5 - work_offset) < work_dim1 *
			 work_dim2 && 0 <= i__2 ? i__2 : s_rnge("work", i__2, 
			"zzgflong_", (ftnlen)2889)], &work[(i__3 = wh * 
			work_dim1 - 5 - work_offset) < work_dim1 * work_dim2 
			&& 0 <= i__3 ? i__3 : s_rnge("work", i__3, "zzgflong_"
			, (ftnlen)2889)]);
	    } else {
		wnunid_(&work[(i__1 = q1 * work_dim1 - 5 - work_offset) < 
			work_dim1 * work_dim2 && 0 <= i__1 ? i__1 : s_rnge(
			"work", i__1, "zzgflong_", (ftnlen)2891)], &work[(
			i__2 = left * work_dim1 - 5 - work_offset) < 
			work_dim1 * work_dim2 && 0 <= i__2 ? i__2 : s_rnge(
			"work", i__2, "zzgflong_", (ftnlen)2891)], &work[(
			i__3 = f2 * work_dim1 - 5 - work_offset) < work_dim1 *
			 work_dim2 && 0 <= i__3 ? i__3 : s_rnge("work", i__3, 
			"zzgflong_", (ftnlen)2891)]);
		wnunid_(&work[(i__1 = q4 * work_dim1 - 5 - work_offset) < 
			work_dim1 * work_dim2 && 0 <= i__1 ? i__1 : s_rnge(
			"work", i__1, "zzgflong_", (ftnlen)2892)], &work[(
			i__2 = f2 * work_dim1 - 5 - work_offset) < work_dim1 *
			 work_dim2 && 0 <= i__2 ? i__2 : s_rnge("work", i__2, 
			"zzgflong_", (ftnlen)2892)], &work[(i__3 = wh * 
			work_dim1 - 5 - work_offset) < work_dim1 * work_dim2 
			&& 0 <= i__3 ? i__3 : s_rnge("work", i__3, "zzgflong_"
			, (ftnlen)2892)]);
	    }

/*           We use F2 as a temporary window index, since F2 is */
/*           guaranteed to exist at this point and is distinct from WH. */

	    wndifd_(&work[(i__1 = wh * work_dim1 - 5 - work_offset) < 
		    work_dim1 * work_dim2 && 0 <= i__1 ? i__1 : s_rnge("work",
		     i__1, "zzgflong_", (ftnlen)2899)], result, &work[(i__2 = 
		    f2 * work_dim1 - 5 - work_offset) < work_dim1 * work_dim2 
		    && 0 <= i__2 ? i__2 : s_rnge("work", i__2, "zzgflong_", (
		    ftnlen)2899)]);
	    copyd_(&work[(i__1 = f2 * work_dim1 - 5 - work_offset) < 
		    work_dim1 * work_dim2 && 0 <= i__1 ? i__1 : s_rnge("work",
		     i__1, "zzgflong_", (ftnlen)2900)], result);
	}
    }
    chkout_("ZZGFLONG", (ftnlen)8);
    return 0;
} /* zzgflong_ */

