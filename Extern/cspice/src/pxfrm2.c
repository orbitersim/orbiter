/* pxfrm2.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;

/* $Procedure PXFRM2 ( Position Transform Matrix, Different Epochs ) */
/* Subroutine */ int pxfrm2_(char *from, char *to, doublereal *etfrom, 
	doublereal *etto, doublereal *rotate, ftnlen from_len, ftnlen to_len)
{
    /* Initialized data */

    static logical first = TRUE_;

    static char svto[32];
    extern /* Subroutine */ int zznamfrm_(integer *, char *, integer *, char *
	    , integer *, ftnlen, ftnlen), zzctruin_(integer *);
    integer fcode;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    integer tcode;
    extern /* Subroutine */ int errch_(char *, char *, ftnlen, ftnlen);
    doublereal jf[9]	/* was [3][3] */;
    static integer svctr1[2], svctr2[2];
    doublereal tj[9]	/* was [3][3] */;
    extern /* Subroutine */ int refchg_(integer *, integer *, doublereal *, 
	    doublereal *);
    static integer svfcod, svtcde;
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), setmsg_(char *, ftnlen);
    static char svfrom[32];
    extern logical return_(void);
    extern /* Subroutine */ int mxm_(doublereal *, doublereal *, doublereal *)
	    ;

/* $ Abstract */

/*     Return the 3x3 matrix that transforms position vectors from one */
/*     specified frame at a specified epoch to another specified */
/*     frame at another specified epoch. */

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

/* $ Keywords */

/*     FRAMES */
/*     TRANSFORM */

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
/*     FROM       I   Name of the frame to transform from. */
/*     TO         I   Name of the frame to transform to. */
/*     ETFROM     I   Evaluation time of FROM frame. */
/*     ETTO       I   Evaluation time of TO frame. */
/*     ROTATE     O   A position transformation matrix from */
/*                    frame FROM to frame TO. */

/* $ Detailed_Input */

/*     FROM     is the name of a reference frame recognized by */
/*              SPICELIB that corresponds to the input ETFROM. */


/*     TO       is the name of a reference frame recognized by */
/*              SPICELIB that corresponds to the desired output */
/*              at ETTO. */


/*     ETFROM   is the epoch in ephemeris seconds past the epoch */
/*              of J2000 (TDB) corresponding to the FROM reference */
/*              frame. */


/*     ETTO     is the epoch in ephemeris seconds past the epoch */
/*              of J2000 (TDB) that corresponds to the TO reference */
/*              frame. */

/* $ Detailed_Output */

/*     ROTATE   is the transformation matrix that relates the reference */
/*              frame FROM at epoch ETFROM to the frame TO at epoch */
/*              ETTO. */

/*              If (X, Y, Z) is a position relative to the reference */
/*              frame FROM at time ETFROM then the vector ( X', Y', */
/*              Z') is the same position relative to the frame TO at */
/*              epoch ETTO. Here the vector ( X', Y', Z' ) is defined */
/*              by the equation: */

/*                .-    -.     .-        -.   .-   -. */
/*                |  X'  |     |          |   |  X  | */
/*                |  Y'  |  =  |  ROTATE  | * |  Y  | */
/*                |  Z'  |     |          |   |  Z  | */
/*                `-    -'     `-        -'   `-   -' */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If sufficient information has not been supplied via loaded */
/*         SPICE kernels to compute the transformation between the */
/*         two frames, an error is signaled by a routine */
/*         in the call tree of this routine. */

/*     2)  If either frame FROM or TO is not recognized, the error */
/*         SPICE(UNKNOWNFRAME) is signaled. */

/* $ Files */

/*     Appropriate kernels must be loaded by the calling program before */
/*     this routine is called. Kernels that may be required include */
/*     SPK files, PCK files, frame kernels, C-kernels, and SCLK kernels. */

/*     Such kernel data are normally loaded once per program */
/*     run, NOT every time this routine is called. */

/* $ Particulars */

/*     PXFRM2 is most commonly used to transform a position between */
/*     time-dependent reference frames. */

/*     For more examples of where to use PXFRM2, please see: */

/*           SINCPT */
/*           SURFPT */
/*           SUBSLR */
/*           ILUMIN */

/* $ Examples */

/*     The numerical results shown for this example may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */

/*     1) Suppose that MGS has taken a picture of Mars at time ETREC with */
/*        the MOC narrow angle camera. We want to know the latitude and */
/*        longitude associated with two pixels projected to Mars' */
/*        surface: the boresight and one along the boundary of the */
/*        field of view (FOV). Due to light time, the photons taken in */
/*        the picture left Mars at time ETEMIT, when Mars was at a */
/*        different state than at time ETREC. */

/*        In order to solve this problem, we could use the SINCPT */
/*        routine for both pixels, but this would be slow. Instead, we */
/*        will assume that the light time for each pixel is the same. We */
/*        will call SINCPT once to get the light time and surface point */
/*        associated with the boresight. Then, we will rotate one of the */
/*        FOV boundary vectors from the camera frame at ETREC to the */
/*        body-fixed Mars frame at ETEMIT, and call the faster routine */
/*        SURFPT to retrieve the surface point for one of the FOV */
/*        boundary vectors. */

/*        This example problem could be extended to find the latitude */
/*        and longitude associated with every pixel in an instrument's */
/*        field of view, but this example is simplified to only solve */
/*        for two pixels: the boresight and one along the boundary of */
/*        the field of view. */

/*        Assumptions: */

/*           1)  The light times from the surface points in the camera's */
/*               field of view to the camera are equal. */

/*           2)  The camera offset from the center of gravity of the */
/*               spacecraft is zero. If the data are more accurate */
/*               and precise, this assumption can be easily discarded. */

/*           3)  An ellipsoid shape model for the target body is */
/*               sufficient. */

/*           4)  The boundary field of view vector returned from GETFOV */
/*               is associated with a boundary field of view pixel. If */
/*               this example were extended to include a geometric camera */
/*               model, this assumption would not be needed since the */
/*               direction vectors associated with each pixel would be */
/*               calculated from the geometric camera model. */

/*        Use the meta-kernel shown below to load the required SPICE */
/*        kernels. */


/*           KPL/MK */

/*           File name: pxfrm2_ex1.tm */

/*           This is the meta-kernel file for the example problem for */
/*           the subroutine PXFRM2. These kernel files can be found in */
/*           the NAIF archives. */

/*           In order for an application to use this meta-kernel, the */
/*           kernels referenced here must be present in the user's */
/*           current working directory. */

/*           The names and contents of the kernels referenced */
/*           by this meta-kernel are as follows: */

/*              File name                     Contents */
/*              ---------                     -------- */
/*              de421.bsp                     Planetary ephemeris */
/*              pck00009.tpc                  Planet orientation and */
/*                                            radii */
/*              naif0009.tls                  Leapseconds */
/*              mgs_ext12_ipng_mgs95j.bsp     MGS ephemeris */
/*              mgs_moc_v20.ti                MGS MOC instrument */
/*                                            parameters */
/*              mgs_sclkscet_00061.tsc        MGS SCLK coefficients */
/*              mgs_sc_ext12.bc               MGS s/c bus attitude */

/*           \begindata */

/*           KERNELS_TO_LOAD = ( 'de421.bsp', */
/*                               'pck00009.tpc', */
/*                               'naif0009.tls', */
/*                               'mgs_ext12_ipng_mgs95j.bsp', */
/*                               'mgs_moc_v20.ti', */
/*                               'mgs_sclkscet_00061.tsc', */
/*                               'mgs_sc_ext12.bc' ) */

/*           \begintext */

/*           End of meta-kernel. */


/*        Example code begins here. */


/*              PROGRAM PXFRM2_EX1 */
/*              IMPLICIT NONE */
/*        C */
/*        C     SPICELIB functions */
/*        C */
/*        C     Degrees per radian */
/*        C */
/*              DOUBLE PRECISION      DPR */
/*        C */
/*        C     Distance between two vectors */
/*        C */
/*              DOUBLE PRECISION      VDIST */
/*        C */
/*        C     Local parameters */
/*        C */
/*        C     ABCORR is the desired light time and stellar */
/*        C     aberration correction setting. */
/*        C */
/*              CHARACTER*(*)         ABCORR */
/*              PARAMETER           ( ABCORR = 'CN+S' ) */
/*        C */
/*        C     MGS_MOC_NA is the name of the camera that took */
/*        C     the picture being analyzed. */
/*        C */
/*              CHARACTER*(*)         CAMERA */
/*              PARAMETER           ( CAMERA = 'MGS_MOC_NA' ) */

/*              CHARACTER*(*)         METAKR */
/*              PARAMETER           ( METAKR = 'pxfrm2_ex1.tm' ) */

/*              INTEGER               FRNMLN */
/*              PARAMETER           ( FRNMLN = 32 ) */

/*              INTEGER               NCORNR */
/*              PARAMETER           ( NCORNR = 4 ) */

/*              INTEGER               SHPLEN */
/*              PARAMETER           ( SHPLEN = 80 ) */

/*        C */
/*        C     Local variables */
/*        C */
/*        C     OBSREF is the observer reference frame on MGS. */
/*        C */
/*              CHARACTER*(FRNMLN)    OBSREF */
/*              CHARACTER*(SHPLEN)    SHAPE */

/*              DOUBLE PRECISION      BOUNDS ( 3, NCORNR ) */
/*              DOUBLE PRECISION      BNDVEC ( 3 ) */
/*              DOUBLE PRECISION      BSIGHT ( 3 ) */
/*        C */
/*        C     ETEMIT is the time at which the photons were */
/*        C     emitted from Mars.  ETREC is the time at */
/*        C     which the picture was taken by MGS. */
/*        C */
/*              DOUBLE PRECISION      ETREC */
/*              DOUBLE PRECISION      ETEMIT */
/*              DOUBLE PRECISION      DIST */
/*        C */
/*        C     LAT and LON are the latitude and longitude */
/*        C     associated with one of the boundary FOV vectors. */
/*        C */
/*              DOUBLE PRECISION      LAT */
/*              DOUBLE PRECISION      LON */
/*        C */
/*        C     PMGSMR is the opposite of the apparent position of */
/*        C     Mars with respect to MGS. */
/*        C */
/*              DOUBLE PRECISION      PMGSMR ( 3 ) */
/*        C */
/*        C     RADII is a vector of the semi-axes of Mars. */
/*        C */
/*              DOUBLE PRECISION      RADII  ( 3 ) */
/*              DOUBLE PRECISION      RADIUS */
/*        C */
/*        C     ROTATE is a position transformation matrix from */
/*        C     the camera frame at ETREC to the IAU_MARS frame */
/*        C     at ETEMIT. */
/*        C */
/*              DOUBLE PRECISION      ROTATE ( 3, 3 ) */
/*              DOUBLE PRECISION      SPOINT ( 3 ) */
/*              DOUBLE PRECISION      SRFVEC ( 3 ) */
/*              DOUBLE PRECISION      TMP    ( 3 ) */

/*              INTEGER               CAMID */
/*              INTEGER               DIM */
/*              INTEGER               N */

/*              LOGICAL               FOUND */
/*        C */
/*        C     ------------------ Program Setup ------------------ */
/*        C */
/*        C     Load kernel files via the meta-kernel. */
/*        C */
/*              CALL FURNSH ( METAKR ) */
/*        C */
/*        C     Convert the time the picture was taken from a */
/*        C     UTC time string to seconds past J2000, TDB. */
/*        C */
/*              CALL STR2ET ( '2003 OCT 13 06:00:00 UTC', ETREC ) */
/*        C */
/*        C     Assume the one-way light times from different */
/*        C     surface points on Mars to MGS within the camera's */
/*        C     FOV are equal. This means the photons that make */
/*        C     up different pixels were all emitted from Mars at */
/*        C     ETEMIT and received by the MGS MOC camera at ETREC. It */
/*        C     would be slow to process images using SINCPT for every */
/*        C     pixel. Instead, we will use SINCPT on the */
/*        C     boresight pixel and use SURFPT for one of the FOV */
/*        C     boundary pixels. If this example program were extended */
/*        C     to include all of the camera's pixels, SURFPT would */
/*        C     be used for the remaining pixels. */
/*        C */
/*        C     Get the MGS MOC Narrow angle camera (MGS_MOC_NA) */
/*        C     ID code. Then look up the field of view (FOV) */
/*        C     parameters by calling GETFOV. */
/*        C */
/*              CALL BODN2C ( CAMERA, CAMID, FOUND ) */

/*              IF ( .NOT. FOUND ) THEN */

/*                 CALL SETMSG ( 'Could not find ID code for ' // */
/*             .                 'instrument #.'               ) */
/*                 CALL ERRCH  ( '#', CAMERA                   ) */
/*                 CALL SIGERR ( 'SPICE(NOTRANSLATION)'        ) */

/*              END IF */
/*        C */
/*        C     GETFOV will return the name of the camera-fixed frame */
/*        C     in the string OBSREF, the camera boresight vector in */
/*        C     the array BSIGHT, and the FOV corner vectors in the */
/*        C     array BOUNDS. */
/*        C */
/*              CALL GETFOV ( CAMID,  NCORNR, SHAPE, OBSREF, */
/*             .              BSIGHT, N,      BOUNDS       ) */

/*              WRITE (*,*) 'Observation Reference frame:  ', OBSREF */

/*        C */
/*        C     ----------- Boresight Surface Intercept ----------- */
/*        C */
/*        C     Retrieve the time, surface intercept point, and vector */
/*        C     from MGS to the boresight surface intercept point */
/*        C     in IAU_MARS coordinates. */
/*        C */
/*              CALL SINCPT ( 'ELLIPSOID', 'MARS',  ETREC, 'IAU_MARS', */
/*             .               ABCORR,     'MGS',   OBSREF, BSIGHT, */
/*             .               SPOINT,      ETEMIT, SRFVEC, FOUND  ) */

/*              IF ( .NOT. FOUND ) THEN */

/*                 CALL SETMSG ( 'Intercept not found for the ' // */
/*             .                 'boresight vector.'  ) */
/*                 CALL SIGERR ( 'SPICE(NOINTERCEPT)' ) */

/*              END IF */
/*        C */
/*        C     Convert the intersection point of the boresight */
/*        C     vector and Mars from rectangular into latitudinal */
/*        C     coordinates. Convert radians to degrees. */
/*        C */
/*              CALL RECLAT ( SPOINT, RADIUS, LON, LAT ) */

/*              LON = LON * DPR () */
/*              LAT = LAT * DPR () */

/*              WRITE (*,*) 'Boresight surface intercept ' // */
/*             .            'coordinates:' */
/*              WRITE (*,*) '   Radius    (km) :  ', RADIUS */
/*              WRITE (*,*) '   Latitude  (deg):  ', LAT */
/*              WRITE (*,*) '   Longitude (deg):  ', LON */

/*        C */
/*        C     ------- A Boundary FOV Surface Intercept (SURFPT) ------- */
/*        C */
/*        C     Now we will transform one of the FOV corner vectors into */
/*        C     the IAU_MARS frame so the surface intercept point can be */
/*        C     calculated using SURFPT, which is faster than SUBPNT. */
/*        C */
/*        C     If this example program were extended to include all */
/*        C     of the pixels in the camera's FOV, a few steps, such as */
/*        C     finding the rotation matrix from the camera frame to the */
/*        C     IAU_MARS frame, looking up the radii values for Mars, */
/*        C     and finding the position of MGS with respect to Mars */
/*        C     could be done once and used for every pixel. */
/*        C */
/*        C     Find the rotation matrix from the ray's reference */
/*        C     frame at the time the photons were received (ETREC) */
/*        C     to IAU_MARS at the time the photons were emitted */
/*        C     (ETEMIT). */
/*        C */
/*              CALL PXFRM2 ( OBSREF, 'IAU_MARS', ETREC, ETEMIT, ROTATE ) */

/*        C */
/*        C     Look up the radii values for Mars. */
/*        C */
/*              CALL BODVRD ( 'MARS', 'RADII', 3, DIM, RADII ) */

/*        C */
/*        C     Find the position of the center of Mars with respect */
/*        C     to MGS.  The position of the observer with respect */
/*        C     to Mars is required for the call to SURFPT.  Note: */
/*        C     the apparent position of MGS with respect to Mars is */
/*        C     not the same as the negative of Mars with respect to MGS. */
/*        C */
/*              CALL VSUB   ( SPOINT, SRFVEC, PMGSMR ) */

/*        C */
/*        C     The selected boundary FOV pixel must be rotated into the */
/*        C     IAU_MARS reference frame. */
/*        C */
/*              CALL MXV    ( ROTATE, BOUNDS(1,1), BNDVEC ) */

/*        C */
/*        C     Calculate the surface point of the boundary FOV */
/*        C     vector. */
/*        C */
/*              CALL SURFPT ( PMGSMR,   BNDVEC, RADII(1), RADII(2), */
/*             .              RADII(3), SPOINT, FOUND ) */

/*              IF ( .NOT. FOUND ) THEN */

/*                 CALL SETMSG ( 'Could not calculate surface point.') */
/*                 CALL SIGERR ( 'SPICE(NOTFOUND)' ) */

/*              END IF */

/*              CALL VEQU   ( SPOINT, TMP ) */
/*        C */
/*        C     Convert the intersection point of the boundary */
/*        C     FOV vector and Mars from rectangular into */
/*        C     latitudinal coordinates. Convert radians */
/*        C     to degrees. */
/*        C */
/*              CALL RECLAT ( SPOINT, RADIUS, LON, LAT ) */

/*              LON = LON * DPR () */
/*              LAT = LAT * DPR () */

/*              WRITE (*,*) 'Boundary vector surface intercept ' // */
/*             .               'coordinates using SURFPT:' */
/*              WRITE (*,*) '   Radius    (km) :  ', RADIUS */
/*              WRITE (*,*) '   Latitude  (deg):  ', LAT */
/*              WRITE (*,*) '   Longitude (deg):  ', LON */
/*              WRITE (*,*) '   Emit time using' */
/*              WRITE (*,*) '   boresight LT(s):  ', ETEMIT */

/*        C */
/*        C     ----  A Boundary FOV Surface Intercept Verification ----- */
/*        C */
/*        C     For verification only, we will calculate the surface */
/*        C     intercept coordinates for the selected boundary vector */
/*        C     using SINCPT and compare to the faster SURFPT method. */
/*        C */
/*              CALL SINCPT ( 'ELLIPSOID', 'MARS',  ETREC, 'IAU_MARS', */
/*             .               ABCORR,     'MGS',   OBSREF, BOUNDS(1,1), */
/*             .               SPOINT,      ETEMIT, SRFVEC, FOUND ) */

/*              IF ( .NOT. FOUND ) THEN */

/*                 CALL SETMSG ( 'Intercept not found for the ' // */
/*             .                 'boresight vector.'  ) */
/*                 CALL SIGERR ( 'SPICE(NOINTERCEPT)' ) */

/*              END IF */
/*        C */
/*        C     Convert the intersection point of the selected boundary */
/*        C     vector and Mars from rectangular into latitudinal */
/*        C     coordinates. Convert radians to degrees. */
/*        C */
/*              CALL RECLAT ( SPOINT, RADIUS, LON, LAT ) */

/*              LON = LON * DPR () */
/*              LAT = LAT * DPR () */

/*              WRITE (*,*) 'Boundary vector surface intercept ' // */
/*             .               'coordinates using SINCPT:' */
/*              WRITE (*,*) '   Radius    (km) :  ', RADIUS */
/*              WRITE (*,*) '   Latitude  (deg):  ', LAT */
/*              WRITE (*,*) '   Longitude (deg):  ', LON */
/*              WRITE (*,*) '   Emit time using' */
/*              WRITE (*,*) '   boundary LT(s) :  ', ETEMIT */

/*        C */
/*        C     We expect this to be a very small distance. */
/*        C */
/*              DIST = VDIST ( TMP, SPOINT ) */

/*              WRITE (*,*) 'Distance between surface points' */
/*              WRITE (*,*) 'of the selected boundary vector using' */
/*              WRITE (*,*) 'SURFPT and SINCPT:' */
/*              WRITE (*,*) '   Distance (mm):     ', DIST*(1.E6) */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*         Observation Reference frame:  MGS_MOC_NA */
/*         Boresight surface intercept coordinates: */
/*            Radius    (km) :     3384.9404101592791 */
/*            Latitude  (deg):    -48.479579821639035 */
/*            Longitude (deg):    -123.43645396290199 */
/*         Boundary vector surface intercept coordinates using SURFPT: */
/*            Radius    (km) :     3384.9411359300038 */
/*            Latitude  (deg):    -48.477481877892430 */
/*            Longitude (deg):    -123.47407986665237 */
/*            Emit time using */
/*            boresight LT(s):     119296864.18105948 */
/*         Boundary vector surface intercept coordinates using SINCPT: */
/*            Radius    (km) :     3384.9411359139667 */
/*            Latitude  (deg):    -48.477481924252700 */
/*            Longitude (deg):    -123.47407904898704 */
/*            Emit time using */
/*            boundary LT(s) :     119296864.18105946 */
/*         Distance between surface points */
/*         of the selected boundary vector using */
/*         SURFPT and SINCPT: */
/*            Distance (mm):        32.139880286899256 */


/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     S.C. Krening       (JPL) */
/*     B.V. Semenov       (JPL) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.1, 13-AUG-2021 (JDR) */

/*        Edited the header to comply with NAIF standard. Updated code */
/*        example comments and format. */

/* -    SPICELIB Version 1.0.0, 23-SEP-2013 (SCK) (WLT) (BVS) */

/* -& */
/* $ Index_Entries */

/*     Position transformation matrix for different epochs */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */

/*     JCODE represents the NAIF ID of the J2000 reference frame. */
/*     The J2000 frame has a NAIF ID of 1. Any inertial reference */
/*     frame could have been used for this program instead of J2000. */


/*     Saved frame name length. */


/*     Local variables */


/*     Saved frame name/ID item declarations. */


/*     Saved frame name/ID items. */


/*     Initial values. */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }
    chkin_("PXFRM2", (ftnlen)6);

/*     Initialization. */

    if (first) {

/*        Initialize counters. */

	zzctruin_(svctr1);
	zzctruin_(svctr2);
	first = FALSE_;
    }

/*     The frame names must be converted to their corresponding IDs. */

    zznamfrm_(svctr1, svfrom, &svfcod, from, &fcode, (ftnlen)32, from_len);
    zznamfrm_(svctr2, svto, &svtcde, to, &tcode, (ftnlen)32, to_len);

/*     Only non-zero ID codes are legitimate frame ID codes.  Zero */
/*     indicates that the frame was not recognized. */

    if (fcode != 0 && tcode != 0) {

/*        The following three lines of code calculate the following: */

/*        1)  [JF]      The rotation matrix is calculated from the frame */
/*                          FROM to the inertial J2000 frame at ETFROM. */
/*        2)  [TJ]      The rotation matrix is calculated from the J2000 */
/*                          frame to the TO frame at ETTO. */
/*        3)  [ROTATE]  The rotation matrix from frame FROM at ETFROM to */
/*                          frame TO at ETTO is given by the following: */

/*                              [ROTATE] = [TF] = [TJ][JF] */

	refchg_(&fcode, &c__1, etfrom, jf);
	refchg_(&c__1, &tcode, etto, tj);
	mxm_(tj, jf, rotate);
    } else if (fcode == 0 && tcode == 0) {
	setmsg_("Neither frame # nor # was recognized as a known reference f"
		"rame. ", (ftnlen)65);
	errch_("#", from, (ftnlen)1, from_len);
	errch_("#", to, (ftnlen)1, to_len);
	sigerr_("SPICE(UNKNOWNFRAME)", (ftnlen)19);
    } else if (fcode == 0) {
	setmsg_("The frame # was not recognized as a known reference frame. ",
		 (ftnlen)59);
	errch_("#", from, (ftnlen)1, from_len);
	sigerr_("SPICE(UNKNOWNFRAME)", (ftnlen)19);
    } else {

/*        TCODE is zero */

	setmsg_("The frame # was not recognized as a known reference frame. ",
		 (ftnlen)59);
	errch_("#", to, (ftnlen)1, to_len);
	sigerr_("SPICE(UNKNOWNFRAME)", (ftnlen)19);
    }
    chkout_("PXFRM2", (ftnlen)6);
    return 0;
} /* pxfrm2_ */

