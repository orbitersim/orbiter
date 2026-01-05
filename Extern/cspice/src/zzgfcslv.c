/* zzgfcslv.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__15 = 15;
static integer c__7 = 7;
static integer c__0 = 0;
static integer c__1 = 1;
static logical c_false = FALSE_;
static doublereal c_b36 = 0.;

/* $Procedure ZZGFCSLV ( GF, coordinate solver ) */
/* Subroutine */ int zzgfcslv_(char *vecdef, char *method, char *target, char 
	*ref, char *abcorr, char *obsrvr, char *dref, doublereal *dvec, char *
	crdsys, char *crdnam, char *relate, doublereal *refval, doublereal *
	tol, doublereal *adjust, U_fp udstep, U_fp udrefn, logical *rpt, S_fp 
	udrepi, U_fp udrepu, S_fp udrepf, logical *bail, L_fp udbail, integer 
	*mw, integer *nw, doublereal *work, doublereal *cnfine, doublereal *
	result, ftnlen vecdef_len, ftnlen method_len, ftnlen target_len, 
	ftnlen ref_len, ftnlen abcorr_len, ftnlen obsrvr_len, ftnlen dref_len,
	 ftnlen crdsys_len, ftnlen crdnam_len, ftnlen relate_len)
{
    /* Initialized data */

    static char cnames[6*7] = ">     " "=     " "<     " "ABSMAX" "ABSMIN" 
	    "LOCMAX" "LOCMIN";
    static char rptpre[55*3] = "Coordinate pass 1 of #                      "
	    "           " "Coordinate pass 2 of #                            "
	    "     " "Intercept existence pass 1 of 1                        ";
    static char rptsuf[13*3] = "done.        " "done.        " "done.        "
	    ;

    /* System generated locals */
    integer work_dim1, work_dim2, work_offset, i__1, i__2, i__3;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer), s_cmp(char *, char *, 
	    ftnlen, ftnlen);

    /* Local variables */
    extern /* Subroutine */ int zzgfcodc_();
    extern /* Subroutine */ int zzgfcoin_(char *, char *, char *, char *, 
	    char *, char *, char *, doublereal *, char *, char *, ftnlen, 
	    ftnlen, ftnlen, ftnlen, ftnlen, ftnlen, ftnlen, ftnlen, ftnlen);
    extern /* Subroutine */ int zzgfcoex_();
    extern /* Subroutine */ int zzgflong_(char *, char *, char *, char *, 
	    char *, char *, char *, doublereal *, char *, char *, char *, 
	    doublereal *, doublereal *, doublereal *, U_fp, U_fp, logical *, 
	    S_fp, U_fp, S_fp, logical *, L_fp, integer *, integer *, 
	    doublereal *, doublereal *, doublereal *, ftnlen, ftnlen, ftnlen, 
	    ftnlen, ftnlen, ftnlen, ftnlen, ftnlen, ftnlen, ftnlen);
    extern /* Subroutine */ int zzgfudlt_();
    extern /* Subroutine */ int zzgfrelx_(U_fp, U_fp, U_fp, U_fp, U_fp, char *
	    , doublereal *, doublereal *, doublereal *, doublereal *, integer 
	    *, integer *, doublereal *, logical *, S_fp, U_fp, S_fp, char *, 
	    char *, logical *, L_fp, doublereal *, ftnlen, ftnlen, ftnlen);
    integer i__;
    extern /* Subroutine */ int chkin_(char *, ftnlen), ucase_(char *, char *,
	     ftnlen, ftnlen), errch_(char *, char *, ftnlen, ftnlen);
    doublereal excon;
    extern /* Subroutine */ int errdp_(char *, doublereal *, ftnlen), copyd_(
	    doublereal *, doublereal *), repmi_(char *, char *, integer *, 
	    char *, ftnlen, ftnlen, ftnlen);
    integer npass;
    doublereal start;
    extern /* Subroutine */ int ljust_(char *, char *, ftnlen, ftnlen);
    extern logical failed_(void);
    extern /* Subroutine */ int zzgfsolvx_(U_fp, U_fp, U_fp, U_fp, logical *, 
	    L_fp, logical *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, logical *, U_fp, doublereal *), scardd_(integer *, 
	    doublereal *);
    extern integer isrchc_(char *, integer *, char *, ftnlen, ftnlen), 
	    wncard_(doublereal *);
    extern logical return_(void);
    char loccrd[80], locvdf[80], prebuf[55*3];
    doublereal finish;
    logical localx, noadjx;
    extern /* Subroutine */ int setmsg_(char *, ftnlen), errint_(char *, 
	    integer *, ftnlen), sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), ssized_(integer *, doublereal *), cmprss_(char *, 
	    integer *, char *, char *, ftnlen, ftnlen, ftnlen), wnfetd_(
	    doublereal *, integer *, doublereal *, doublereal *), wncond_(
	    doublereal *, doublereal *, doublereal *);
    integer loc;
    extern /* Subroutine */ int udf_();
    char uop[6];
    extern /* Subroutine */ int zzgfcog_();

/* $ Abstract */

/*     Perform a coordinate search. */

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
/*     CNTRCT     P   Existence window contraction magnitude. */
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

/*                Note that when geodetic coordinates are used, the */
/*                reference ellipsoid is that associated with the */
/*                central body of the reference frame designated by REF. */
/*                The central body must be an extended body in this */
/*                case. */

/*                Case, leading and trailing blanks are not significant */
/*                in the string CRDSYS. */


/*     CRDNAM     is the name of the coordinate of interest:  this is */
/*                the coordinate to which the specified condition */
/*                applies.  The set of coordinate names is a function of */
/*                the coordinate system. Allowed values are those */
/*                defined in the GF Fortran INCLUDE file */

/*                   zzgf.inc. */

/*                Case, leading and trailing blanks are not significant */
/*                in the string CRDNAM. */


/*     RELATE      is a relational operator used to define a constraint */
/*                 on the specified coordinate. The result window found */
/*                 by this routine indicates the time intervals where */
/*                 the constraint is satisfied. Supported values of */
/*                 RELATE and corresponding meanings are shown below: */

/*                    '>'      Coordinate is greater than the reference */
/*                             value REFVAL. */

/*                    '='      Coordinate is equal to the reference */
/*                             value REFVAL. */

/*                    '<'      Coordinate is less than the reference */
/*                             value REFVAL. */


/*                   'ABSMAX'  Coordinate is at an absolute maximum. */

/*                   'ABSMIN'  Coordinate is at an absolute  minimum. */

/*                   'LOCMAX'  Coordinate is at a local maximum. */

/*                   'LOCMIN'  Coordinate is at a local minimum. */

/*                The caller may indicate that the region of interest */
/*                is the set of time intervals where the coordinate is */
/*                within a specified tolerance of an absolute extremum. */
/*                The argument ADJUST (described below) is used to */
/*                specify this tolerance. */

/*                Local extrema are considered to exist only in the */
/*                interiors of the intervals comprising the confinement */
/*                window:  a local extremum cannot exist at a boundary */
/*                point of the confinement window. */

/*                Case is not significant in the string RELATE. */


/*     REFVAL     is the reference value used to define equality or */
/*                inequality conditions. */

/*                If the coordinate has the dimension "length," then */
/*                REFVAL has units of kilometers. */

/*                If the coordinate has the dimension "angle," then */
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
/*                values between ABSMIN and ABSMIN + ADJUST. */

/*                If the search is for an absolute maximum, the */
/*                corresponding range is  between ABSMAX - ADJUST and */
/*                ABSMAX. */


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


/*     CNFINE     is a SPICE window that confines the bounds of the */
/*                search. */

/*                For coordinates defined by ray-target surface */
/*                intercepts, the effective confinement window is */
/*                obtained by searching for times within CNFINE when the */
/*                specified intercept and its derivative with respect to */
/*                time are computable. The window resulting from this */
/*                search is then contracted by CNTRCT+TOL seconds at */
/*                both left and right endpoints; this contracted window */
/*                is called the "existence window," since the surface */
/*                intercept and its time derivative are expected to be */
/*                computable on this contracted window. The user must */
/*                select CNFINE so that this requirement is met. */


/*     RESULT     is an initialized SPICE window. RESULT must be large */
/*                enough to hold all of the intervals, within the */
/*                confinement window, on which the specified condition */
/*                is met. */

/*                RESULT must be initialized by the caller via the */
/*                SPICELIB routine SSIZED. */


/* $ Detailed_Output */

/*     WORK       has undefined contents on output. */


/*     RESULT     is a SPICELIB window containing the intersection of */
/*                the confinement window and the set of time intervals */
/*                when the value of the specified coordinate satisfies */
/*                constraints specified by RELATE and ADJUST. */

/*                For coordinates defined by ray-target surface */
/*                intercepts, RESULT is further restricted to the window */
/*                over which the intercept and its derivative with */
/*                respect to time are computable. See the description of */
/*                CNFINE above for details. */

/* $ Parameters */

/*     LBCELL     is the lower bound for SPICELIB cells. */

/*     CNTRCT     is the contraction magnitude used to prepare the */
/*                "existence window" for use as a confinement window. */
/*                The existence window is applicable only to coordinates */
/*                of surface intercepts: it is the result of contracting */
/*                the window over which the surface intercept and its */
/*                time derivative are computable by CNTRCT+TOL. Units */
/*                are TDB seconds. */

/* $ Exceptions */

/*     1)  If the workspace window count NW is less than NWMAX, the */
/*         error SPICE(TOOFEWWINDOWS) is signaled. */

/*     2)  If the workspace window size MW is less than 2, the */
/*         error SPICE(WINDOWSTOOSMALL) is signaled. */

/*     3)  If a workspace window or the result window is too small */
/*         to accommodate the required number of intervals, */
/*         the error will be diagnosed by routines in the call tree */
/*         of this routine. */

/*     4)  If either the observer or target names cannot be mapped */
/*         to ID codes, the error will be diagnosed by routines in the */
/*         call tree of this routine. */

/*     5)  If the observer and target have the same ID codes, the */
/*         error will be diagnosed by routines in the call tree of this */
/*         routine. */

/*     6)  If the vector definition VECDEF is not recognized, */
/*         the error will be diagnosed by routines in the call tree */
/*         of this routine. */

/*     7)  If the computation method METHOD is not recognized, */
/*         the error will be diagnosed by routines in the call tree */
/*         of this routine. */

/*     8)  If the aberration correction ABCORR is not recognized, */
/*         the error will be diagnosed by routines in the call tree */
/*         of this routine. */

/*     9)  If the coordinate system name CRDSYS is not recognized, */
/*         the error will be diagnosed by routines in the call tree */
/*         of this routine. */

/*     10) If the coordinate name CRDNAM is not recognized, */
/*         the error will be diagnosed by routines in the call tree */
/*         of this routine. */

/*     11) If the frame REF is not recognized by the frames subsystem, */
/*         the error will be diagnosed by routines in the call tree */
/*         of this routine. */

/*     12) If VECDEF calls for a computation involving a target surface */
/*         intercept point and the name and ID code of the frame DREF */
/*         associated with the target body are not available from the */
/*         frame subsystem, the error will be diagnosed by routines in */
/*         the call tree of this routine. */

/*     13) If VECDEF calls for a computation involving a target surface */
/*         intercept point and the direction vector DVEC is the zero */
/*         vector, the error will be diagnosed by routines in the call */
/*         tree of this routine. */

/*     14) If VECDEF calls for a computation involving a target surface */
/*         point and the radii defining the reference ellipsoid */
/*         associated with the target body are not available in the */
/*         kernel pool, the error will be diagnosed by routines in the */
/*         call tree of this routine. */

/*     15) If VECDEF calls for a computation involving a target surface */
/*         point and the frame REF is not centered on the target body, */
/*         the error will be diagnosed by routines in the call tree */
/*         of this routine. */

/*     16) If geodetic or planetographic coordinates are used and the */
/*         radii defining the reference ellipsoid associated with the */
/*         center of the frame REF are not available in the kernel pool, */
/*         the error will be diagnosed by routines in the call tree of */
/*         this routine. */

/*     17) If geodetic or planetographic coordinates are used and the */
/*         first equatorial radius of the reference ellipsoid associated */
/*         with the center of the frame REF is zero, the error will be */
/*         diagnosed by routines in the call tree of this routine. */

/*     18) If geodetic or planetographic coordinates are used and the */
/*         equatorial radii of the reference ellipsoid associated */
/*         with the center of the frame REF are unequal, the error */
/*         SPICE(NOTSUPPORTED) is signaled. */

/*     19) If geodetic or planetographic coordinates are used and the */
/*         reference ellipsoid associated with the center of the frame */
/*         REF is degenerate (one or more radii are non-positive), */
/*         the error will be diagnosed by routines in the call tree */
/*         of this routine. */

/*     20) If ADJUST is negative, the error SPICE(VALUEOUTOFRANGE) */
/*         is signaled. */

/*     21) If TOL is non-positive, the error SPICE(VALUEOUTOFRANGE) */
/*         is signaled. */

/*     21) If RELATE is not a supported relational operator */
/*         specification, the error SPICE(NOTRECOGNIZED) is signaled. */

/* $ Files */

/*     See the discussion in the Files section of the header of the */
/*     umbrella subroutine ZZGFCOU. */

/* $ Particulars */

/*     This routine handles coordinate search set-up and execution */
/*     activities for GFEVNT. */

/*     For a surface intercept coordinate search, this routine finds the */
/*     "existence window," within the input confinement window, for the */
/*     surface intercept and its time derivative. The existence window */
/*     is contracted by CNTRCT seconds; this contracted window is then */
/*     used as the confinement window for the search. */

/* $ Examples */

/*     See GFEVNT and ZZGFLONG. */

/* $ Restrictions */

/*     1)  The interface and functionality of this set of routines may */
/*         change without notice.  These routines should be called only */
/*         by SPICELIB routines. */

/*     2)  ZZGFCSLV must be called prior to use of any of the other */
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

/*     N.J. Bachman   (JPL) */
/*     E.D. Wright    (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.3.0, 03-OCT-2021 (NJB) */

/*        Variable CNAMES is now saved. Corrected typos in comments. */

/* -    SPICELIB Version 1.2.0, 04-APR-2011 (EDW) */

/*        Replaced use of routines ZZGFREL with ZZGFRELX, and */
/*        ZZGFSOLV with ZZGFSOLVX. ZZGFCOIN argument list edited */
/*        to remove the unneeded argument REFVAL. */

/*        The code changes for ZZGFRELX use should not affect the */
/*        numerical results of GF computations. */

/* -    SPICELIB Version 1.0.0 06-MAR-2009 (NJB) (EDW) */

/* -& */
/* $ Index_Entries */

/*     coordinate search */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     Number of supported comparison operators: */


/*     MAXOP is the maximum string length for comparison operators. */
/*     MAXOP may grow if new comparisons are added. */


/*     Local variables */


/*     Saved variables */


/*     Initial values */


/*     Below we initialize the list of comparison operator names. */

    /* Parameter adjustments */
    work_dim1 = *mw + 6;
    work_dim2 = *nw;
    work_offset = work_dim1 - 5;

    /* Function Body */

/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }
    chkin_("ZZGFCSLV", (ftnlen)8);

/*     Check the workspace window count. */

    if (*nw < 15) {
	setmsg_("Workspace window count was # but must be at least #.", (
		ftnlen)52);
	errint_("#", nw, (ftnlen)1);
	errint_("#", &c__15, (ftnlen)1);
	sigerr_("SPICE(TOOFEWWINDOWS)", (ftnlen)20);
	chkout_("ZZGFCSLV", (ftnlen)8);
	return 0;
    }

/*     Check the workspace window size. The minimum size that */
/*     makes any sense is 2. */

    if (*mw < 2) {
	setmsg_("Workspace window size was # but must be at least 2.", (
		ftnlen)51);
	errint_("#", mw, (ftnlen)1);
	sigerr_("SPICE(WINDOWSTOOSMALL)", (ftnlen)22);
	chkout_("ZZGFCSLV", (ftnlen)8);
	return 0;
    }

/*     Make sure ADJUST is non-negative. */

    if (*adjust < 0.) {
	setmsg_("ADJUST was #; must be non-negative.", (ftnlen)35);
	errdp_("#", adjust, (ftnlen)1);
	sigerr_("SPICE(VALUEOUTOFRANGE)", (ftnlen)22);
	chkout_("ZZGFCSLV", (ftnlen)8);
	return 0;
    }

/*     Make sure TOL is positive. */

    if (*tol <= 0.) {
	setmsg_("TOL was #; must be positive.", (ftnlen)28);
	errdp_("#", tol, (ftnlen)1);
	sigerr_("SPICE(VALUEOUTOFRANGE)", (ftnlen)22);
	chkout_("ZZGFCSLV", (ftnlen)8);
	return 0;
    }

/*     Make sure that the requested comparison operation is one we */
/*     recognize. */

    ljust_(relate, uop, relate_len, (ftnlen)6);
    ucase_(uop, uop, (ftnlen)6, (ftnlen)6);
    loc = isrchc_(uop, &c__7, cnames, (ftnlen)6, (ftnlen)6);
    if (loc == 0) {
	setmsg_("The comparison operator, # is not recognized.  Supported op"
		"erators are: >,=,<,ABSMAX,ABSMIN,LOCMAX,LOCMIN.", (ftnlen)106)
		;
	errch_("#", relate, (ftnlen)1, relate_len);
	sigerr_("SPICE(NOTRECOGNIZED)", (ftnlen)20);
	chkout_("ZZGFCSLV", (ftnlen)8);
	return 0;
    }

/*     Initialize the workspace windows. */

    i__1 = *nw;
    for (i__ = 1; i__ <= i__1; ++i__) {
	ssized_(mw, &work[(i__2 = i__ * work_dim1 - 5 - work_offset) < 
		work_dim1 * work_dim2 && 0 <= i__2 ? i__2 : s_rnge("work", 
		i__2, "zzgfcslv_", (ftnlen)974)]);
    }

/*     Initialize the result window. */

    scardd_(&c__0, result);

/*     Create a left-justified, compressed copy of the */
/*     input vector definition method. */

    ljust_(vecdef, locvdf, vecdef_len, (ftnlen)80);
    cmprss_(" ", &c__1, locvdf, locvdf, (ftnlen)1, (ftnlen)80, (ftnlen)80);
    ucase_(locvdf, locvdf, (ftnlen)80, (ftnlen)80);

/*     If the vector definition method is "surface intercept," */
/*     find the "existence window": the window over which */
/*     the intercept and its time derivative are computable. */

    if (s_cmp(locvdf, "SURFACE INTERCEPT POINT", (ftnlen)80, (ftnlen)23) == 0)
	     {

/*        Initialize the search for the existence window. */

	zzgfcoin_(vecdef, method, target, ref, abcorr, obsrvr, dref, dvec, 
		crdsys, crdnam, vecdef_len, method_len, target_len, ref_len, 
		abcorr_len, obsrvr_len, dref_len, crdsys_len, crdnam_len);
	if (failed_()) {
	    chkout_("ZZGFCSLV", (ftnlen)8);
	    return 0;
	}

/*        This routine presumes that UDSTEP has been initialized, so we */
/*        don't attempt to reset the step. */

/*        If progress reporting is enabled, initialize the progress */
/*        report for the existence window search. */

	if (*rpt) {
	    (*udrepi)(cnfine, rptpre + 110, rptsuf + 26, (ftnlen)55, (ftnlen)
		    13);
	}

/*        ZZGFSOLV will add the result of each search to the workspace */
/*        window */

/*           WORK(LBCELL,EXWIDX) */

/*        Initialize this window. */

	ssized_(mw, &work[(i__1 = work_dim1 * 13 - 5 - work_offset) < 
		work_dim1 * work_dim2 && 0 <= i__1 ? i__1 : s_rnge("work", 
		i__1, "zzgfcslv_", (ftnlen)1027)]);

/*        Search each interval of the confinement window. */

	i__1 = wncard_(cnfine);
	for (i__ = 1; i__ <= i__1; ++i__) {
	    wnfetd_(cnfine, &i__, &start, &finish);
	    zzgfsolvx_((U_fp)udf_, (U_fp)zzgfcoex_, (U_fp)udstep, (U_fp)
		    udrefn, bail, (L_fp)udbail, &c_false, &c_b36, &start, &
		    finish, tol, rpt, (U_fp)udrepu, &work[(i__2 = work_dim1 * 
		    13 - 5 - work_offset) < work_dim1 * work_dim2 && 0 <= 
		    i__2 ? i__2 : s_rnge("work", i__2, "zzgfcslv_", (ftnlen)
		    1036)]);
	    if (failed_()) {
		chkout_("ZZGFCSLV", (ftnlen)8);
		return 0;
	    }

/*           If interrupt processing is enabled, check to see */
/*           whether an interrupt has occurred. */

	    if (*bail) {
		if ((*udbail)()) {
		    chkout_("ZZGFCSLV", (ftnlen)8);
		    return 0;
		}
	    }
	}

/*        If progress reporting is enabled, terminate the report */
/*        for this pass. */

	if (*rpt) {
	    (*udrepf)();
	}

/*        For safety, contract the existence window. Store */
/*        the result in the workspace. */

	excon = *tol + 1.;
	wncond_(&excon, &excon, &work[(i__1 = work_dim1 * 13 - 5 - 
		work_offset) < work_dim1 * work_dim2 && 0 <= i__1 ? i__1 : 
		s_rnge("work", i__1, "zzgfcslv_", (ftnlen)1074)]);
    } else {

/*        Simply copy the confinement window to the workspace. */

	copyd_(cnfine, &work[(i__1 = work_dim1 * 13 - 5 - work_offset) < 
		work_dim1 * work_dim2 && 0 <= i__1 ? i__1 : s_rnge("work", 
		i__1, "zzgfcslv_", (ftnlen)1080)]);
    }

/*     If progress reporting is enabled, set the report prefix array */
/*     according to the quantity and the relational operator. */

    if (*rpt) {

/*        We'll use the logical flag LOCALX to indicate a local extremum */
/*        operator and the flag NOADJX to indicate an absolute extremum */
/*        operator with zero adjustment. */

	localx = s_cmp(uop, "LOCMIN", (ftnlen)6, (ftnlen)6) == 0 || s_cmp(uop,
		 "LOCMAX", (ftnlen)6, (ftnlen)6) == 0;
	noadjx = *adjust == 0. && (s_cmp(uop, "ABSMIN", (ftnlen)6, (ftnlen)6) 
		== 0 || s_cmp(uop, "ABSMAX", (ftnlen)6, (ftnlen)6) == 0);
	if (localx || noadjx) {

/*           These operators correspond to 1-pass searches. */

	    npass = 1;
	} else {
	    npass = 2;
	}

/*        Fill in the prefix strings. */

	i__1 = npass;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    repmi_(rptpre + ((i__2 = i__ - 1) < 3 && 0 <= i__2 ? i__2 : 
		    s_rnge("rptpre", i__2, "zzgfcslv_", (ftnlen)1115)) * 55, 
		    "#", &npass, prebuf + ((i__3 = i__ - 1) < 3 && 0 <= i__3 ?
		     i__3 : s_rnge("prebuf", i__3, "zzgfcslv_", (ftnlen)1115))
		     * 55, (ftnlen)55, (ftnlen)1, (ftnlen)55);
	}
    }

/*     Create a left-justified, compressed, upper case copy of the */
/*     input coordinate name. */

    ljust_(crdnam, loccrd, crdnam_len, (ftnlen)80);
    cmprss_(" ", &c__1, loccrd, loccrd, (ftnlen)1, (ftnlen)80, (ftnlen)80);
    ucase_(loccrd, loccrd, (ftnlen)80, (ftnlen)80);

/*     If the coordinate of interest is longitude or right ascension, we */
/*     have a special case, since the mapping from Cartesian to */
/*     latitudinal coordinates has a branch discontinuity. */

    if (s_cmp(loccrd, "LONGITUDE", (ftnlen)80, (ftnlen)9) == 0 || s_cmp(
	    loccrd, "RIGHT ASCENSION", (ftnlen)80, (ftnlen)15) == 0) {

/*        The coordinate is longitude or right ascension. */

	zzgflong_(vecdef, method, target, ref, abcorr, obsrvr, dref, dvec, 
		crdsys, crdnam, relate, refval, tol, adjust, (U_fp)udstep, (
		U_fp)udrefn, rpt, (S_fp)udrepi, (U_fp)udrepu, (S_fp)udrepf, 
		bail, (L_fp)udbail, mw, nw, work, &work[(i__1 = work_dim1 * 
		13 - 5 - work_offset) < work_dim1 * work_dim2 && 0 <= i__1 ? 
		i__1 : s_rnge("work", i__1, "zzgfcslv_", (ftnlen)1138)], 
		result, vecdef_len, method_len, target_len, ref_len, 
		abcorr_len, obsrvr_len, dref_len, crdsys_len, crdnam_len, 
		relate_len);
    } else {

/*        This is the normal case. */

/*        Initialize the coordinate quantity utilities. */

	zzgfcoin_(vecdef, method, target, ref, abcorr, obsrvr, dref, dvec, 
		crdsys, crdnam, vecdef_len, method_len, target_len, ref_len, 
		abcorr_len, obsrvr_len, dref_len, crdsys_len, crdnam_len);

/*        Perform the search. */

	zzgfrelx_((U_fp)udstep, (U_fp)udrefn, (U_fp)zzgfcodc_, (U_fp)
		zzgfudlt_, (U_fp)zzgfcog_, relate, refval, tol, adjust, &work[
		(i__1 = work_dim1 * 13 - 5 - work_offset) < work_dim1 * 
		work_dim2 && 0 <= i__1 ? i__1 : s_rnge("work", i__1, "zzgfcs"
		"lv_", (ftnlen)1159)], mw, nw, work, rpt, (S_fp)udrepi, (U_fp)
		udrepu, (S_fp)udrepf, prebuf, rptsuf, bail, (L_fp)udbail, 
		result, relate_len, (ftnlen)55, (ftnlen)13);
    }
    chkout_("ZZGFCSLV", (ftnlen)8);
    return 0;
} /* zzgfcslv_ */

