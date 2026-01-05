/* subpt.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure SUBPT ( Sub-observer point ) */
/* Subroutine */ int subpt_(char *method, char *target, doublereal *et, char *
	abcorr, char *obsrvr, doublereal *spoint, doublereal *alt, ftnlen 
	method_len, ftnlen target_len, ftnlen abcorr_len, ftnlen obsrvr_len)
{
    /* Initialized data */

    static doublereal origin[3] = { 0.,0.,0. };
    static logical first = TRUE_;

    extern /* Subroutine */ int zzbods2c_(integer *, char *, integer *, 
	    logical *, char *, integer *, logical *, ftnlen, ftnlen), 
	    zzgftreb_(integer *, doublereal *), zzctruin_(integer *);
    doublereal radii[3];
    extern /* Subroutine */ int chkin_(char *, ftnlen), errch_(char *, char *,
	     ftnlen, ftnlen);
    logical found;
    extern doublereal vdist_(doublereal *, doublereal *);
    extern /* Subroutine */ int spkez_(integer *, doublereal *, char *, char *
	    , integer *, doublereal *, doublereal *, ftnlen, ftnlen);
    extern logical eqstr_(char *, char *, ftnlen, ftnlen);
    static logical svfnd1, svfnd2;
    static integer svctr1[2], svctr2[2];
    extern logical failed_(void);
    integer obscde;
    doublereal lt;
    integer frcode;
    extern /* Subroutine */ int cidfrm_(integer *, integer *, char *, logical 
	    *, ftnlen);
    char frname[80];
    integer trgcde;
    static integer svtcde;
    extern /* Subroutine */ int nearpt_(doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *), sigerr_(
	    char *, ftnlen), chkout_(char *, ftnlen);
    static integer svobsc;
    extern /* Subroutine */ int setmsg_(char *, ftnlen);
    doublereal tstate[6];
    static char svtarg[36];
    extern logical return_(void);
    static char svobsr[36];
    extern /* Subroutine */ int vminus_(doublereal *, doublereal *), surfpt_(
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, logical *);
    doublereal pos[3];

/* $ Abstract */

/*     Deprecated: This routine has been superseded by the SPICELIB */
/*     routine SUBPNT. This routine is supported for purposes of */
/*     backward compatibility only. */

/*     Compute the rectangular coordinates of the sub-observer point on */
/*     a target body at a particular epoch, optionally corrected for */
/*     planetary (light time) and stellar aberration. Return these */
/*     coordinates expressed in the body-fixed frame associated with the */
/*     target body. Also, return the observer's altitude above the */
/*     target body. */

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
/*     PCK */
/*     SPK */
/*     TIME */

/* $ Keywords */

/*     GEOMETRY */

/* $ Declarations */
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

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     METHOD     I   Computation method. */
/*     TARGET     I   Name of target body. */
/*     ET         I   Epoch in ephemeris seconds past J2000 TDB. */
/*     ABCORR     I   Aberration correction. */
/*     OBSRVR     I   Name of observing body. */
/*     SPOINT     O   Sub-observer point on the target body. */
/*     ALT        O   Altitude of the observer above the target body. */

/* $ Detailed_Input */

/*     METHOD   is a short string specifying the computation method */
/*              to be used. The choices are: */

/*                 'Near point'       The sub-observer point is */
/*                                    defined as the nearest point on */
/*                                    the target relative to the */
/*                                    observer. */

/*                 'Intercept'        The sub-observer point is */
/*                                    defined as the target surface */
/*                                    intercept of the line */
/*                                    containing the observer and the */
/*                                    target's center. */

/*              In both cases, the intercept computation treats the */
/*              surface of the target body as a triaxial ellipsoid. */
/*              The ellipsoid's radii must be available in the kernel */
/*              pool. */

/*              Neither case nor white space are significant in */
/*              METHOD. For example, the string ' NEARPOINT' is */
/*              valid. */


/*     TARGET   is the name of a target body. Optionally, you may */
/*              supply the integer ID code for the object as */
/*              an integer string. For example both 'MOON' and */
/*              '301' are legitimate strings that indicate the */
/*              moon is the target body. This routine assumes */
/*              that this body is modeled by a tri-axial ellipsoid, */
/*              and that a PCK file containing its radii has been */
/*              loaded into the kernel pool via FURNSH. */

/*     ET       is the epoch in ephemeris seconds past J2000 at which */
/*              the sub-observer point on the target body is to be */
/*              computed. */


/*     ABCORR   indicates the aberration corrections to be applied */
/*              when computing the observer-target state.  ABCORR */
/*              may be any of the following. */

/*                 'NONE'     Apply no correction. Return the */
/*                            geometric sub-observer point on the */
/*                            target body. */

/*                 'LT'       Correct for planetary (light time) */
/*                            aberration. Both the state and rotation */
/*                            of the target body are corrected for */
/*                            light time. */

/*                 'LT+S'     Correct for planetary (light time) and */
/*                            stellar aberrations. Both the state and */
/*                            rotation of the target body are */
/*                            corrected for light time. */


/*                 'CN'       Converged Newtonian light time */
/*                            correction. In solving the light time */
/*                            equation, the 'CN' correction iterates */
/*                            until the solution converges (three */
/*                            iterations on all supported platforms). */
/*                            Whether the 'CN+S' solution is */
/*                            substantially more accurate than the */
/*                            'LT' solution depends on the geometry */
/*                            of the participating objects and on the */
/*                            accuracy of the input data. In all */
/*                            cases this routine will execute more */
/*                            slowly when a converged solution is */
/*                            computed. See the $Particulars section */
/*                            of SPKEZR for a discussion of precision */
/*                            of light time corrections. */

/*                            Both the state and rotation of the */
/*                            target body are corrected for light */
/*                            time. */

/*                 'CN+S'     Converged Newtonian light time */
/*                            correction and stellar aberration */
/*                            correction. */

/*                            Both the state and rotation of the */
/*                            target body are corrected for light */
/*                            time. */

/*     OBSRVR   is the name of the observing body. This is typically */
/*              a spacecraft, the earth, or a surface point on the */
/*              earth. Optionally, you  may supply the ID code of */
/*              the object as an integer string. For example, both */
/*              'EARTH' and '399' are legitimate strings to supply */
/*              to indicate the observer is Earth. */

/* $ Detailed_Output */

/*     SPOINT   is the sub-observer point on the target body at ET */
/*              expressed relative to the body-fixed frame of the */
/*              target body. */

/*              The sub-observer point is defined either as the point */
/*              on the target body that is closest to the observer, */
/*              or the target surface intercept of the line from the */
/*              observer to the target's center; the input argument */
/*              METHOD selects the definition to be used. */

/*              The body-fixed frame, which is time-dependent, is */
/*              evaluated at ET if ABCORR is 'NONE'; otherwise the */
/*              frame is evaluated at ET-LT, where LT is the one-way */
/*              light time from target to observer. */

/*              The state of the target body is corrected for */
/*              aberration as specified by ABCORR; the corrected */
/*              state is used in the geometric computation. As */
/*              indicated above, the rotation of the target is */
/*              retarded by one-way light time if ABCORR specifies */
/*              that light time correction is to be done. */


/*     ALT      is the "altitude" of the observer above the target */
/*              body. When METHOD specifies a "near point" */
/*              computation, ALT is truly altitude in the standard */
/*              geometric sense: the length of a segment dropped from */
/*              the observer to the target's surface, such that the */
/*              segment is perpendicular to the surface at the */
/*              contact point SPOINT. */

/*              When METHOD specifies an "intercept" computation, ALT */
/*              is still the length of the segment from the observer */
/*              to the surface point SPOINT, but this segment in */
/*              general is not perpendicular to the surface. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     If any of the listed errors occur, the output arguments are */
/*     left unchanged. */

/*     1)  If the input argument METHOD is not recognized, the error */
/*         SPICE(DUBIOUSMETHOD) is signaled. */

/*     2)  If either of the input body names TARGET or OBSRVR cannot be */
/*         mapped to NAIF integer codes, the error SPICE(IDCODENOTFOUND) */
/*         is signaled. */

/*     3)  If OBSRVR and TARGET map to the same NAIF integer ID codes, */
/*         the error SPICE(BODIESNOTDISTINCT) is signaled. */

/*     4)  If frame definition data enabling the evaluation of the state */
/*         of the target relative to the observer in target body-fixed */
/*         coordinates have not been loaded prior to calling SUBPT, an */
/*         error is signaled by a routine in the call tree of this */
/*         routine. */

/*     5)  If the specified aberration correction is not recognized, an */
/*         error is signaled by a routine in the call tree of this */
/*         routine. */

/*     6)  If insufficient ephemeris data have been loaded prior to */
/*         calling SUBPT, an error is signaled by a */
/*         routine in the call tree of this routine. */

/*     7)  If the triaxial radii of the target body have not been loaded */
/*         into the kernel pool prior to calling SUBPT, an error is */
/*         signaled by a routine in the call tree of this routine. */

/*     8)  If the size of the TARGET body radii kernel variable is not */
/*         three, an error is signaled by a routine in the call tree of */
/*         this routine. */

/*     9)  If any of the three TARGET body radii is less-than or equal to */
/*         zero, an error is signaled by a routine in the call tree of */
/*         this routine. */

/*     10) If PCK data supplying a rotation model for the target body */
/*         have not been loaded prior to calling SUBPT, an error is */
/*         signaled by a routine in the call tree of this routine. */

/* $ Files */

/*     Appropriate SPK, PCK, and frame kernels must be loaded */
/*     prior by the calling program before this routine is called. */

/*     The following data are required: */

/*     -  SPK data: ephemeris data for target and observer must be */
/*        loaded. If aberration corrections are used, the states of */
/*        target and observer relative to the solar system barycenter */
/*        must be calculable from the available ephemeris data. */
/*        Typically ephemeris data are made available by loading one */
/*        or more SPK files via FURNSH. */

/*     -  PCK data: triaxial radii for the target body must be loaded */
/*        into the kernel pool. Typically this is done by loading a */
/*        text PCK file via FURNSH. */

/*     -  Further PCK data: rotation data for the target body must */
/*        be loaded. These may be provided in a text or binary PCK */
/*        file. Either type of file may be loaded via FURNSH. */

/*     -  Frame data: if a frame definition is required to convert */
/*        the observer and target states to the body-fixed frame of */
/*        the target, that definition must be available in the kernel */
/*        pool. Typically the definition is supplied by loading a */
/*        frame kernel via FURNSH. */

/*     In all cases, kernel data are normally loaded once per program */
/*     run, NOT every time this routine is called. */

/* $ Particulars */

/*     SUBPT computes the sub-observer point on a target body. */
/*     (The sub-observer point is commonly called the sub-spacecraft */
/*     point when the observer is a spacecraft.) SUBPT also */
/*     determines the altitude of the observer above the target body. */

/*     There are two different popular ways to define the sub-observer */
/*     point:  "nearest point on target to observer" or "target surface */
/*     intercept of line containing observer and target." These */
/*     coincide when the target is spherical and generally are distinct */
/*     otherwise. */

/*     When comparing sub-point computations with results from sources */
/*     other than SPICE, it's essential to make sure the same geometric */
/*     definitions are used. */

/* $ Examples */

/*     The numerical results shown for this example may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */

/*     In the following example program, the file */

/*        spk_m_031103-040201_030502.bsp */

/*     is a binary SPK file containing data for Mars Global Surveyor, */
/*     Mars, and the Sun for a time interval bracketing the date */

/*         2004 JAN 1 12:00:00 UTC. */

/*     pck00007.tpc is a planetary constants kernel file containing */
/*     radii and rotation model constants. naif0007.tls is a */
/*     leapseconds kernel. */

/*     Find the sub-observer point of the Mars Global Surveyor (MGS) */
/*     spacecraft on Mars for a specified time. Perform the computation */
/*     twice, using both the "intercept" and "near point" options. */


/*           IMPLICIT NONE */

/*           CHARACTER*25          METHOD ( 2 ) */

/*           INTEGER               I */

/*           DOUBLE PRECISION      ALT */
/*           DOUBLE PRECISION      DPR */
/*           DOUBLE PRECISION      ET */
/*           DOUBLE PRECISION      LAT */
/*           DOUBLE PRECISION      LON */
/*           DOUBLE PRECISION      RADIUS */
/*           DOUBLE PRECISION      SPOINT ( 3 ) */

/*           DATA                  METHOD / 'Intercept', 'Near point' / */

/*     C */
/*     C     Load kernel files. */
/*     C */
/*           CALL FURNSH ( 'naif0007.tls'                   ) */
/*           CALL FURNSH ( 'pck00007.tpc'                   ) */
/*           CALL FURNSH ( 'spk_m_031103-040201_030502.bsp' ) */

/*     C */
/*     C     Convert the UTC request time to ET (seconds past */
/*     C     J2000, TDB). */
/*     C */
/*           CALL STR2ET ( '2004 JAN 1 12:00:00', ET ) */

/*     C */
/*     C     Compute sub-spacecraft point using light time and stellar */
/*     C     aberration corrections.  Use the "target surface intercept" */
/*     C     definition of sub-spacecraft point on the first loop */
/*     C     iteration, and use the "near point" definition on the */
/*     C     second. */
/*     C */
/*           DO I = 1, 2 */

/*              CALL SUBPT ( METHOD(I), */
/*          .               'MARS',     ET,     'LT+S', */
/*          .               'MGS',      SPOINT,  ALT    ) */

/*     C */
/*     C        Convert rectangular coordinates to planetocentric */
/*     C        latitude and longitude.  Convert radians to degrees. */
/*     C */
/*              CALL RECLAT ( SPOINT, RADIUS, LON, LAT  ) */

/*              LON = LON * DPR () */
/*              LAT = LAT * DPR () */

/*     C */
/*     C        Write the results. */
/*     C */
/*              WRITE (*,*) ' ' */
/*              WRITE (*,*) 'Computation method: ', METHOD(I) */
/*              WRITE (*,*) ' ' */
/*              WRITE (*,*) '  Radius                   (km)  = ', RADIUS */
/*              WRITE (*,*) '  Planetocentric Latitude  (deg) = ', LAT */
/*              WRITE (*,*) '  Planetocentric Longitude (deg) = ', LON */
/*              WRITE (*,*) '  Altitude                 (km)  = ', ALT */
/*              WRITE (*,*) ' ' */

/*           END DO */

/*           END */


/*     When this program is executed, the output will be: */


/*        Computation method: Intercept */

/*          Radius                   (km)  =   3387.97077 */
/*          Planetocentric Latitude  (deg) =  -39.7022724 */
/*          Planetocentric Longitude (deg) =  -159.226663 */
/*          Altitude                 (km)  =   373.173506 */


/*        Computation method: Near point */

/*          Radius                   (km)  =   3387.9845 */
/*          Planetocentric Latitude  (deg) =  -39.6659329 */
/*          Planetocentric Longitude (deg) =  -159.226663 */
/*          Altitude                 (km)  =   373.166636 */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     C.H. Acton         (JPL) */
/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     J.E. McLean        (JPL) */
/*     B.V. Semenov       (JPL) */
/*     E.D. Wright        (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.4.0, 01-NOV-2021 (EDW) (JDR) */

/*        Body radii accessed from kernel pool using ZZGFTREB. */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 1.3.0, 04-JUL-2014 (NJB) (BVS) */

/*        Discussion of light time corrections was updated. Assertions */
/*        that converged light time corrections are unlikely to be */
/*        useful were removed. */

/*     Last update was 19-SEP-2013 (BVS) */

/*        Updated to save the input body names and ZZBODTRN state */
/*        counters and to do name-ID conversions only if the counters */
/*        have changed. */

/* -    SPICELIB Version 1.2.3, 18-MAY-2010 (BVS) */

/*        Index line now states that this routine is deprecated. */

/* -    SPICELIB Version 1.2.2, 17-MAR-2009 (EDW) */

/*        Typo correction in $Required_Reading, changed */
/*        FRAME to FRAMES. */

/* -    SPICELIB Version 1.2.1, 07-FEB-2008 (NJB) */

/*        $Abstract now states that this routine is deprecated. */

/* -    SPICELIB Version 1.2.0, 24-OCT-2005 (NJB) */

/*        Replaced call to BODVAR with call to BODVCD. */

/* -    SPICELIB Version 1.1.0, 21-JUL-2004 (EDW) */

/*        Changed BODN2C call to BODS2C giving the routine */
/*        the capability to accept string representations of */
/*        integer IDs for TARGET and OBSRVR. */

/* -    SPICELIB Version 1.0.1, 27-JUL-2003 (NJB) (CHA) */

/*        Various header corrections were made. The example program */
/*        was upgraded to use real kernels, and the program's output is */
/*        shown. */

/* -    SPICELIB Version 1.0.0, 03-SEP-1999 (NJB) (JEM) */

/* -& */
/* $ Index_Entries */

/*     DEPRECATED sub-observer point */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     Saved body name length. */


/*     Local variables */


/*     Saved name/ID item declarations. */


/*     Saved variables */


/*     Saved name/ID items. */


/*     Initial values */


/*     Initial values. */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("SUBPT", (ftnlen)5);
    }

/*     Initialization. */

    if (first) {

/*        Initialize counters. */

	zzctruin_(svctr1);
	zzctruin_(svctr2);
	first = FALSE_;
    }

/*     Obtain integer codes for the target and observer. */

/*     Target... */

    zzbods2c_(svctr1, svtarg, &svtcde, &svfnd1, target, &trgcde, &found, (
	    ftnlen)36, target_len);
    if (! found) {
	setmsg_("The target, '#', is not a recognized name for an ephemeris "
		"object. The cause of this problem may be that you need an up"
		"dated version of the SPICE Toolkit. ", (ftnlen)155);
	errch_("#", target, (ftnlen)1, target_len);
	sigerr_("SPICE(IDCODENOTFOUND)", (ftnlen)21);
	chkout_("SUBPT", (ftnlen)5);
	return 0;
    }

/*     ...observer. */

    zzbods2c_(svctr2, svobsr, &svobsc, &svfnd2, obsrvr, &obscde, &found, (
	    ftnlen)36, obsrvr_len);
    if (! found) {
	setmsg_("The observer, '#', is not a recognized name for an ephemeri"
		"s object. The cause of this problem may be that you need an "
		"updated version of the SPICE Toolkit. ", (ftnlen)157);
	errch_("#", obsrvr, (ftnlen)1, obsrvr_len);
	sigerr_("SPICE(IDCODENOTFOUND)", (ftnlen)21);
	chkout_("SUBPT", (ftnlen)5);
	return 0;
    }

/*     Check the input body codes.  If they are equal, signal */
/*     an error. */

    if (obscde == trgcde) {
	setmsg_("In computing the sub-observer point, the observing body and"
		" target body are the same. Both are #.", (ftnlen)97);
	errch_("#", obsrvr, (ftnlen)1, obsrvr_len);
	sigerr_("SPICE(BODIESNOTDISTINCT)", (ftnlen)24);
	chkout_("SUBPT", (ftnlen)5);
	return 0;
    }

/*     Get the radii of the target body from the kernel pool. */

    zzgftreb_(&trgcde, radii);
    if (failed_()) {
	chkout_("SUBPT", (ftnlen)5);
	return 0;
    }

/*     Find the name of the body-fixed frame associated with the */
/*     target body.  We'll want the state of the target relative to */
/*     the observer in this body-fixed frame. */

    cidfrm_(&trgcde, &frcode, frname, &found, (ftnlen)80);
    if (! found) {
	setmsg_("No body-fixed frame is associated with target body #; a fra"
		"me kernel must be loaded to make this association.  Consult "
		"the FRAMES Required Reading for details.", (ftnlen)159);
	errch_("#", target, (ftnlen)1, target_len);
	sigerr_("SPICE(NOFRAME)", (ftnlen)14);
	chkout_("SUBPT", (ftnlen)5);
	return 0;
    }

/*     Determine the position of the observer in target */
/*     body-fixed coordinates. */

/*         -  Call SPKEZR to compute the position of the target */
/*            body as seen from the observing body and the light time */
/*            (LT) between them.  SPKEZR returns a state which is */
/*            the position and velocity, but we'll only use the position */
/*            which is the first three elements.  We request that the */
/*            coordinates of POS be returned relative to the body fixed */
/*            reference frame associated with the target body, using */
/*            aberration corrections specified by the input argument */
/*            ABCORR. */

/*         -  Call VMINUS to negate the direction of the vector (POS) */
/*            so it will be the position of the observer as seen from */
/*            the target body in target body fixed coordinates. */

/*            Note that this result is not the same as the result of */
/*            calling SPKEZR with the target and observer switched.  We */
/*            computed the vector FROM the observer TO the target in */
/*            order to get the proper light time and stellar aberration */
/*            corrections (if requested).  Now we need the inverse of */
/*            that corrected vector in order to compute the sub-point. */

    spkez_(&trgcde, et, frname, abcorr, &obscde, tstate, &lt, (ftnlen)80, 
	    abcorr_len);

/*     Negate the target's state to obtain the position of the observer */
/*     relative to the target. */

    vminus_(tstate, pos);

/*     Find the sub-point and "altitude" (distance from observer to */
/*     sub-point) using the specified geometric definition. */

    if (eqstr_(method, "Near point", method_len, (ftnlen)10)) {

/*        Locate the nearest point to the observer on the target. */

	nearpt_(pos, radii, &radii[1], &radii[2], spoint, alt);
    } else if (eqstr_(method, "Intercept", method_len, (ftnlen)9)) {
	surfpt_(origin, pos, radii, &radii[1], &radii[2], spoint, &found);

/*        Since the line in question passes through the center of the */
/*        target, there will always be a surface intercept.  So we should */
/*        never have FOUND = .FALSE. */

	if (! found) {
	    setmsg_("Call to SURFPT returned FOUND=FALSE even though vertex "
		    "of ray is at target center. This indicates a bug. Please"
		    " contact NAIF.", (ftnlen)125);
	    sigerr_("SPICE(BUG)", (ftnlen)10);
	    chkout_("SUBPT", (ftnlen)5);
	    return 0;
	}

/*        SURFPT doesn't compute altitude, so do it here. */

	*alt = vdist_(pos, spoint);
    } else {
	setmsg_("The computation method # was not recognized. Allowed values"
		" are \"Near point\" and \"Intercept.\"", (ftnlen)93);
	errch_("#", method, (ftnlen)1, method_len);
	sigerr_("SPICE(DUBIOUSMETHOD)", (ftnlen)20);
	chkout_("SUBPT", (ftnlen)5);
	return 0;
    }
    chkout_("SUBPT", (ftnlen)5);
    return 0;
} /* subpt_ */

