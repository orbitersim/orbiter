/* zzgfcost.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure      ZZGFCOST ( GF, coordinate definition state ) */
/* Subroutine */ int zzgfcost_(char *vecdef, char *method, integer *trgid, 
	doublereal *et, char *ref, char *abcorr, integer *obsid, char *dref, 
	integer *dctr, doublereal *dvec, doublereal *radii, doublereal *state,
	 logical *found, ftnlen vecdef_len, ftnlen method_len, ftnlen ref_len,
	 ftnlen abcorr_len, ftnlen dref_len)
{
    /* Builtin functions */
    integer s_cmp(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    extern /* Subroutine */ int zzgfssob_(char *, integer *, doublereal *, 
	    char *, char *, integer *, doublereal *, doublereal *, ftnlen, 
	    ftnlen, ftnlen), zzgfssin_(char *, integer *, doublereal *, char *
	    , char *, integer *, char *, integer *, doublereal *, doublereal *
	    , doublereal *, logical *, ftnlen, ftnlen, ftnlen, ftnlen), 
	    chkin_(char *, ftnlen), errch_(char *, char *, ftnlen, ftnlen), 
	    spkez_(integer *, doublereal *, char *, char *, integer *, 
	    doublereal *, doublereal *, ftnlen, ftnlen);
    doublereal lt;
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), setmsg_(char *, ftnlen);
    extern logical return_(void);

/* $ Abstract */

/*     SPICE private routine intended solely for the support of SPICE */
/*     routines. Users should not call this routine directly due to the */
/*     volatile nature of this routine. */

/*     Return a state vector used to define coordinates referenced in a */
/*     GF search. */

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
/*     DCTR       I   ID code of ray frame's center. */
/*     DVEC       I   Ray's direction vector. */
/*     RADII      I   Radii of reference ellipsoid. */
/*     STATE      O   State used to define coordinates. */
/*     FOUND      O   Flag indicating if state was computed. */

/* $ Detailed_Input */


/*     VECDEF     States computed by this routine consist of a an */
/*                underlying vector and the vector's velocity. VECDEF is */
/*                a short string describing the means by which the */
/*                vector of interest is defined. Only parameters from */
/*                the Fortran INCLUDE file zzgf.inc should be used. */
/*                Parameter names and meanings are: */

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
/*                TDB, at which the specified state is to be computed. */


/*     REF        is the name of the reference frame relative to which */
/*                the state of interest is specified. */

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
/*                and trailing blanks in DREF are not significant. */

/*                When DREF designates a non-inertial frame, the */
/*                orientation of the frame is evaluated at an epoch */
/*                dependent on the frame's center and, if the center is */
/*                not the observer, on the selected aberration */
/*                correction. See the description of the direction */
/*                vector DVEC for details. */


/*     DCTR       is the ID code of the object at which the frame */
/*                designated by DREF is centered. Although DCTR */
/*                can be derived from DREF, in the interest of */
/*                efficiency, DCTR is obtained by the caller, */
/*                normally during search initialization. */


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


/*     RADII      is a double precision array containing the three */
/*                radii of a reference ellipsoid associated with */
/*                the target body. */

/*                RADII is ignored if the input vector definition */
/*                is POSDEF; in this case the caller may set the */
/*                elements of RADII to zero. */


/* $ Detailed_Output */

/*     STATE      is the specified state vector, evaluated at the epoch */
/*                ET. The position component of STATE is the vector */
/*                defined by VECDEF and the other inputs. The velocity */
/*                component of STATE is the derivative with respect to */
/*                time of the position component. Units are km and km/s. */

/*                STATE is defined if and only if the output argument */
/*                FOUND is set to .TRUE. */


/*     FOUND      is a logical flag indicating whether the requested */
/*                state could be computed. FOUND is set to .FALSE. if */
/*                and only if the vector definition is SINDEF and either */

/*                - the surface intercept is not found */

/*                - the surface intercept velocity is not computable */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the vector definition VECDEF is not recognized, */
/*         the error SPICE(NOTSUPPORTED) is signaled. */

/*     2)  If the computation method METHOD is not recognized, */
/*         the error will be diagnosed by routines in the call tree */
/*         of this routine. */

/*     3)  If the aberration correction ABCORR is not recognized, */
/*         the error will be diagnosed by routines in the call tree */
/*         of this routine. */

/*     4)  If the frame REF is not recognized by the frames subsystem, */
/*         the error will be diagnosed by routines in the call tree */
/*         of this routine. */

/*     5)  If VECDEF calls for a computation involving a target surface */
/*         point and the name and ID code of the frame associated with */
/*         the target body is not available from the frame subsystem, */
/*         the error SPICE(NOFRAME) is signaled. */

/*     6)  If VECDEF calls for a computation involving a target surface */
/*         point and ID codes of target and observer can't be converted */
/*         to names, the error  will be diagnosed by routines in the */
/*         call tree of this routine. */

/*     7)  If ephemeris data are required but not available to compute */
/*         the state of the target, the coordinate frame REF's center, */
/*         or the input ray's frame DREF's center relative to the */
/*         observer, the error will be diagnosed by routines in the call */
/*         tree of this routine. */

/*     8)  If orientation data for the frame REF are not available, */
/*         the error will be diagnosed by routines in the call tree */
/*         of this routine. */

/*     9)  If orientation data for the frame DREF are required but */
/*         not available, the error will be diagnosed by routines in the */
/*         call tree of this routine. */

/*     10) If the input radii don't define a valid triaxial ellipsoid, */
/*         the error will be diagnosed by routines in the call tree of */
/*         this routine. */

/* $ Files */

/*     This routine doesn't directly participate in SPICE kernel loading */
/*     or unloading.  However, a variety of SPICE kernels must be loaded */
/*     in order for this routine to work: */

/*        - SPK files providing ephemeris data enabling computation of */
/*          the specified state vector are required. */

/*        - If non-inertial reference frames are used, then PCK */
/*          files, frame kernels, C-kernels, and SCLK kernels may be */
/*          needed. */

/*        - If the state of interest is defined in terms of a target */
/*          surface point, then (currently) a PCK providing radii for a */
/*          triaxial shape model must be loaded. */

/*     See the Files section of GFEVNT's header for further information. */

/* $ Particulars */

/*     This routine is used by the GF coordinate utility routines in */
/*     order to solve for time windows on which specified mathematical */
/*     conditions involving coordinates are satisfied. The role of */
/*     this routine is to provide Cartesian state vectors enabling */
/*     the GF coordinate utilities to determine the signs of the */
/*     derivatives with respect to time of coordinates of interest. */

/*     This routine has a secondary purpose: enabling the GF system */
/*     to determine, via a binary state search, the window over */
/*     which a coordinate of interest is computable. This "computability */
/*     window" must be found before any search involving a constraint */
/*     on a coordinate of a surface intercept point can be performed. */

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

/* -    SPICELIB Version 1.0.0 05-MAR-2009 (NJB) */

/* -& */
/* $ Index_Entries */

/*     compute state defining coordinate */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }
    chkin_("ZZGFCOST", (ftnlen)8);

/*     No result was found yet. */

    *found = FALSE_;
    if (s_cmp(vecdef, "POSITION", vecdef_len, (ftnlen)8) == 0) {

/*        Find the observer-target state vector. */

	spkez_(trgid, et, ref, abcorr, obsid, state, &lt, ref_len, abcorr_len)
		;
	*found = TRUE_;
    } else if (s_cmp(vecdef, "SUB-OBSERVER POINT", vecdef_len, (ftnlen)18) == 
	    0) {

/*        The caller has requested the state of a sub-observer point. */

	zzgfssob_(method, trgid, et, ref, abcorr, obsid, radii, state, 
		method_len, ref_len, abcorr_len);
	*found = TRUE_;
    } else if (s_cmp(vecdef, "SURFACE INTERCEPT POINT", vecdef_len, (ftnlen)
	    23) == 0) {

/*        The caller has requested the state of a surface intercept */
/*        point. */

	zzgfssin_(method, trgid, et, ref, abcorr, obsid, dref, dctr, dvec, 
		radii, state, found, method_len, ref_len, abcorr_len, 
		dref_len);
    } else {
	setmsg_("The coordinate quantity # is not recognized.", (ftnlen)44);
	errch_("#", vecdef, (ftnlen)1, vecdef_len);
	sigerr_("SPICE(NOTSUPPORTED)", (ftnlen)19);
	chkout_("ZZGFCOST", (ftnlen)8);
	return 0;
    }

/*     At this point, one of the following is true: */

/*        - the state vector was found and */
/*          FOUND is .TRUE. */

/*        - FOUND is .FALSE. */

/*        - a SPICE error occurred */

    chkout_("ZZGFCOST", (ftnlen)8);
    return 0;
} /* zzgfcost_ */

