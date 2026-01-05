/* fovray.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure FOVRAY ( Is ray in FOV at time? ) */
/* Subroutine */ int fovray_(char *inst, doublereal *raydir, char *rframe, 
	char *abcorr, char *obsrvr, doublereal *et, logical *visibl, ftnlen 
	inst_len, ftnlen rframe_len, ftnlen abcorr_len, ftnlen obsrvr_len)
{
    extern /* Subroutine */ int zzgffvin_(char *, char *, doublereal *, char *
	    , char *, char *, char *, ftnlen, ftnlen, ftnlen, ftnlen, ftnlen, 
	    ftnlen), zzgffvst_(doublereal *, logical *), chkin_(char *, 
	    ftnlen);
    extern logical failed_(void);
    extern /* Subroutine */ int chkout_(char *, ftnlen);
    extern logical return_(void);

/* $ Abstract */

/*     Determine if a specified ray is within the field-of-view (FOV) of */
/*     a specified instrument at a given time. */

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

/*     CK */
/*     FRAMES */
/*     KERNEL */
/*     NAIF_IDS */
/*     PCK */
/*     SPK */
/*     TIME */

/* $ Keywords */

/*     EVENT */
/*     FOV */
/*     GEOMETRY */
/*     INSTRUMENT */

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

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  ------------------------------------------------- */
/*     INST       I   Name or ID code string of the instrument. */
/*     RAYDIR     I   Ray's direction vector. */
/*     RFRAME     I   Reference frame of ray's direction vector. */
/*     ABCORR     I   Aberration correction flag. */
/*     OBSRVR     I   Name or ID code string of the observer. */
/*     ET         I   Time of the observation (seconds past J2000). */
/*     VISIBL     O   Visibility flag (.TRUE./.FALSE.). */

/* $ Detailed_Input */

/*     INST     indicates the name of an instrument, such as a */
/*              spacecraft-mounted framing camera. The field of view */
/*              (FOV) of the instrument will be used to determine if */
/*              the direction from the observer to a target, */
/*              represented as a ray, is visible with respect to the */
/*              instrument. */

/*              The position of the instrument INST is considered to */
/*              coincide with that of the ephemeris object OBSRVR (see */
/*              description below). */

/*              The size of the instrument's FOV is constrained by the */
/*              following: There must be a vector A such that all of */
/*              the instrument's FOV boundary vectors have an angular */
/*              separation from A of less than (pi/2)-MARGIN radians */
/*              (see description below). For FOVs that are circular or */
/*              elliptical, the vector A is the boresight. For FOVs */
/*              that are rectangular or polygonal, the vector A is */
/*              calculated. */

/*              See the header of the SPICELIB routine GETFOV for a */
/*              description of the required parameters associated with */
/*              an instrument. */

/*              Both object names and NAIF IDs are accepted. For */
/*              example, both 'CASSINI_ISS_NAC' and '-82360' are */
/*              accepted. Case and leading or trailing blanks are not */
/*              significant in the string. */

/*     RAYDIR   is the direction vector defining a ray of interest. */
/*              The ray emanates from the location of the ephemeris */
/*              object designated by the input argument OBSRVR and */
/*              is expressed relative to the reference frame */
/*              designated by RFRAME (see description below). */

/*     RFRAME   is the name of the reference frame associated with */
/*              the input ray's direction vector RAYDIR. Note: RFRAME */
/*              does not need to be the instrument's reference frame. */

/*              Since light time corrections are not supported for */
/*              rays, the orientation of the frame is always evaluated */
/*              at the epoch associated with the observer, as opposed */
/*              to the epoch associated with the light-time corrected */
/*              position of the frame center. */

/*              Case, leading and trailing blanks are not significant */
/*              in the string. */

/*     ABCORR   indicates the aberration corrections to be applied */
/*              when computing the ray's direction. */

/*              The supported aberration correction options are: */

/*                 'NONE'          No correction. */
/*                 'S'             Stellar aberration correction, */
/*                                 reception case. */
/*                 'XS'            Stellar aberration correction, */
/*                                 transmission case. */

/*              For detailed information, see the geometry finder */
/*              required reading, gf.req. */

/*              Case, leading and trailing blanks are not significant */
/*              in the string. */

/*     OBSRVR   is the name of the body from which the target */
/*              represented by RAYDIR is observed. The instrument */
/*              designated by INST is treated as if it were co-located */
/*              with the observer. */

/*              Both object names and NAIF IDs are accepted. For */
/*              example, both 'CASSINI' and '-82' are accepted. Case */
/*              and leading or trailing blanks are not significant in */
/*              the string. */

/*     ET       is the observation time in seconds past the J2000 */
/*              epoch. */

/* $ Detailed_Output */

/*     VISIBL   is .TRUE. if the ray is "visible", or in the */
/*              field-of-view, of INST at the time ET. Otherwise, */
/*              VISIBL is .FALSE. */

/* $ Parameters */

/*     MAXVRT   is the maximum number of vertices that may be used */
/*              to define the boundary of the specified instrument's */
/*              field of view. */

/*     MARGIN   is a small positive number used to constrain the */
/*              orientation of the boundary vectors of polygonal */
/*              FOVs. Such FOVs must satisfy the following */
/*              constraints: */

/*                 1)  The boundary vectors must be contained within */
/*                     a right circular cone of angular radius less */
/*                     than than (pi/2) - MARGIN radians; in other */
/*                     words, there must be a vector A such that all */
/*                     boundary vectors have angular separation from */
/*                     A of less than (pi/2)-MARGIN radians. */

/*                 2)  There must be a pair of boundary vectors U, V */
/*                     such that all other boundary vectors lie in */
/*                     the same half space bounded by the plane */
/*                     containing U and V. Furthermore, all other */
/*                     boundary vectors must have orthogonal */
/*                     projections onto a specific plane normal to */
/*                     this plane (the normal plane contains the */
/*                     angle bisector defined by U and V) such that */
/*                     the projections have angular separation of at */
/*                     least 2*MARGIN radians from the plane spanned */
/*                     by U and V. */

/*              MARGIN is currently set to 1.D-12. */

/*     See INCLUDE file gf.inc for declarations and descriptions of */
/*     parameters used throughout the GF system. */

/* $ Exceptions */

/*     1)  If the observer's name cannot be mapped to a NAIF ID code, the */
/*         error SPICE(IDCODENOTFOUND) is signaled. */

/*     2)  If the aberration correction flag calls for light time */
/*         correction, the error SPICE(INVALIDOPTION) is signaled. */

/*     3)  If the ray's direction vector is zero, the error */
/*         SPICE(ZEROVECTOR) is signaled. */

/*     4)  If the instrument name INST does not have corresponding NAIF */
/*         ID code, an error is signaled by a routine in the call */
/*         tree of this routine. */

/*     5)  If the FOV parameters of the instrument are not present in */
/*         the kernel pool, an error is signaled by a routine */
/*         in the call tree of this routine. */

/*     6)  If the FOV boundary has more than MAXVRT vertices, an error */
/*         is signaled by a routine in the call tree of this */
/*         routine. */

/*     7)  If the instrument FOV shape is a polygon or rectangle, and */
/*         this routine cannot find a ray R emanating from the FOV vertex */
/*         such that maximum angular separation of R and any FOV boundary */
/*         vector is within the limit (pi/2)-MARGIN radians, an error is */
/*         signaled by a routine in the call tree of this routine. If the */
/*         FOV is any other shape, the same error check will be applied */
/*         with the instrument boresight vector serving the role of R. */

/*     8)  If the loaded kernels provide insufficient data to compute a */
/*         requested state vector, an error is signaled by a */
/*         routine in the call tree of this routine. */

/*     9)  If an error occurs while reading an SPK or other kernel file, */
/*         the error is signaled by a routine in the call tree */
/*         of this routine. */

/* $ Files */

/*     Appropriate SPICE kernels must be loaded by the calling program */
/*     before this routine is called. */

/*     The following data are required: */

/*     -  SPK data: ephemeris data for the observer at the time */
/*        ET. If aberration corrections are used, the state of the */
/*        observer relative to the solar system barycenter */
/*        must be calculable from the available ephemeris data. */

/*     -  Data defining the reference frame in which the instrument's */
/*        FOV is defined must be available in the kernel pool. */
/*        Additionally the name INST must be associated with an ID */
/*        code. */

/*     -  IK data: the kernel pool must contain data such that */
/*        the SPICELIB routine GETFOV may be called to obtain */
/*        parameters for INST. */

/*     The following data may be required: */

/*     -  CK data: if the frame in which the instrument's FOV is */
/*        defined is fixed to a spacecraft, at least one CK file will */
/*        be needed to permit transformation of vectors between that */
/*        frame and the J2000 frame. */

/*     -  SCLK data: if a CK file is needed, an associated SCLK */
/*        kernel is required to enable conversion between encoded SCLK */
/*        (used to time-tag CK data) and barycentric dynamical time */
/*        (TDB). */

/*     -  Since the input ray direction may be expressed in any */
/*        frame, additional FKs, CKs, SCLK kernels, PCKs, and SPKs */
/*        may be required to map the direction to the J2000 frame. */

/*     Kernel data are normally loaded via FURNSH once per program run, */
/*     NOT every time this routine is called. */

/* $ Particulars */

/*     To treat the target as an ephemeris object rather than a ray, use */
/*     the higher-level SPICELIB routine FOVTRG. FOVTRG may be used to */
/*     determine if ephemeris objects such as Saturn are visible in an */
/*     instrument's FOV at a given time. */

/* $ Examples */

/*     The numerical results shown for this example may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */

/*     1) The Cassini Ultraviolet Imaging Spectrograph (UVIS) */
/*        has been used to measure variations in starlight as */
/*        rings and moons occult Cassini's view of the stars. */
/*        One of these events happened at 2008-054T21:31:55.158 UTC. */
/*        Let's verify that Epsilon CMa (Adhara) was in the */
/*        Cassini UVIS field-of-view at the observation time. */

/*        Use the meta-kernel shown below to load the required SPICE */
/*        kernels. */


/*           KPL/MK */

/*           File name: fovray_ex1.tm */

/*           This meta-kernel is intended to support operation of SPICE */
/*           example programs. The kernels shown here should not be */
/*           assumed to contain adequate or correct versions of data */
/*           required by SPICE-based user applications. */

/*           In order for an application to use this meta-kernel, the */
/*           kernels referenced here must be present in the user's */
/*           current working directory. */

/*           The names and contents of the kernels referenced */
/*           by this meta-kernel are as follows: */

/*              File name                      Contents */
/*              ---------                      -------- */
/*              naif0010.tls                   Leapseconds */
/*              cpck26Jan2007.tpc              Satellite orientation and */
/*                                             radii */
/*              cas00145.tsc                   Cassini SCLK */
/*              cas_v40.tf                     Cassini frames */
/*              cas_uvis_v06.ti                Cassini UVIS instrument */
/*              080428R_SCPSE_08045_08067.bsp  Merged spacecraft, */
/*                                             planetary, and satellite */
/*                                             ephemeris */
/*              08052_08057ra.bc               Orientation for Cassini */

/*           \begindata */

/*              KERNELS_TO_LOAD = ( 'cpck26Jan2007.tpc' */
/*                                  'naif0010.tls' */
/*                                  'cas00145.tsc' */
/*                                  'cas_v40.tf' */
/*                                  'cas_uvis_v06.ti' */
/*                                  '080428R_SCPSE_08045_08067.bsp' */
/*                                  '08052_08057ra.bc') */

/*           \begintext */


/*        Example code begins here. */


/*              PROGRAM FOVRAY_EX1 */
/*              IMPLICIT NONE */
/*        C */
/*        C     SPICELIB functions */
/*        C */
/*        C     Returns radians per degree. */
/*        C */
/*              DOUBLE PRECISION      RPD */

/*        C */
/*        C     Local parameters */
/*        C */
/*              CHARACTER*(*)         META */
/*              PARAMETER           ( META   =  'fovray_ex1.tm' ) */

/*              CHARACTER*(*)         TIMFMT */
/*              PARAMETER           ( TIMFMT = */
/*             .      'YYYY-MON-DD HR:MN:SC.##::TDB (TDB)' ) */

/*        C */
/*        C     This is the UTC time of the observation. */
/*        C */
/*              CHARACTER*(*)         TIME */
/*              PARAMETER           ( TIME = '2008-054T21:31:55.158' ) */

/*        C */
/*        C     Local variables */
/*        C */
/*              CHARACTER*(30)        TIMSTR */

/*              DOUBLE PRECISION      DEC */
/*              DOUBLE PRECISION      ET */
/*              DOUBLE PRECISION      RA */
/*              DOUBLE PRECISION      RAYDIR ( 3 ) */

/*              LOGICAL               VISIBL */

/*        C */
/*        C     RA and DEC are the right ascension and declination */
/*        C     of Epsilon CMa in degrees. */
/*        C */
/*              RA   = 104.656 */
/*              DEC  = -28.972 */

/*        C */
/*        C     Load the kernels. */
/*        C */
/*              CALL FURNSH ( META ) */

/*        C */
/*        C     Convert the observation time from UTC to ET. */
/*        C */
/*              CALL STR2ET ( TIME, ET ) */

/*        C */
/*        C     Create a unit direction vector pointing from Cassini */
/*        C     to the specified star. For details on corrections such */
/*        C     as parallax, please see the example in GFRFOV. */
/*        C */
/*              CALL RADREC ( 1.D0, RA*RPD(), DEC*RPD(), RAYDIR ) */

/*        C */
/*        C     Is the star in the field-of-view of Cassini's UVIS? */
/*        C */
/*              CALL FOVRAY ( 'CASSINI_UVIS_FUV_OCC',  RAYDIR, */
/*             .              'J2000', 'S', 'CASSINI', ET, VISIBL ) */

/*        C */
/*        C     Put the time in a specified format for output. */
/*        C */
/*              CALL TIMOUT ( ET, TIMFMT, TIMSTR ) */

/*              IF ( VISIBL ) THEN */
/*                 WRITE(*,*) 'Epsilon CMa was visible from the ', */
/*             .              'Cassini UVIS instrument at ' */
/*                 WRITE(*,*) TIMSTR */
/*              END IF */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*         Epsilon CMa was visible from the Cassini UVIS instrument at */
/*         2008-FEB-23 21:33:00.34 (TDB) */


/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     S.C. Krening       (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.1, 03-JUL-2021 (JDR) */

/*        Edited header to comply with NAIF standard. Corrected the */
/*        value of MARGIN in the $Parameters section. */

/* -    SPICELIB Version 1.0.0, 15-FEB-2012 (SCK) (NJB) */

/* -& */
/* $ Index_Entries */

/*     Ray in instrument FOV at specified time */
/*     Ray in instrument field_of_view at specified time */

/* -& */

/*     SPICELIB functions */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }
    chkin_("FOVRAY", (ftnlen)6);

/*     Note to maintenance programmer: input exception checks */
/*     are delegated to ZZGFFVU. If the implementation of that */
/*     routine changes, or if this routine is modified to call */
/*     a different routine in place of ZZGFFVU, then the error */
/*     handling performed by ZZGFFVU will have to be performed */
/*     here or in a routine called by this routine. */

/*     Initialize the visibility calculation. */

    zzgffvin_(inst, "RAY", raydir, " ", rframe, abcorr, obsrvr, inst_len, (
	    ftnlen)3, (ftnlen)1, rframe_len, abcorr_len, obsrvr_len);
    if (failed_()) {
	chkout_("FOVRAY", (ftnlen)6);
	return 0;
    }

/*     Calculate the visibility state. */

    zzgffvst_(et, visibl);
    chkout_("FOVRAY", (ftnlen)6);
    return 0;
} /* fovray_ */

