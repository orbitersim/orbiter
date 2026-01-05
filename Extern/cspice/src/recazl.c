/* recazl.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure RECAZL ( Rectangular coordinates to AZ/EL ) */
/* Subroutine */ int recazl_(doublereal *rectan, logical *azccw, logical *
	elplsz, doublereal *range, doublereal *az, doublereal *el)
{
    /* System generated locals */
    doublereal d__1;

    /* Local variables */
    extern doublereal twopi_(void);
    extern /* Subroutine */ int recrad_(doublereal *, doublereal *, 
	    doublereal *, doublereal *);

/* $ Abstract */

/*     Convert rectangular coordinates of a point to range, azimuth and */
/*     elevation. */

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

/*     None. */

/* $ Keywords */

/*     CONVERSION */
/*     COORDINATES */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     RECTAN     I   Rectangular coordinates of a point. */
/*     AZCCW      I   Flag indicating how Azimuth is measured. */
/*     ELPLSZ     I   Flag indicating how Elevation is measured. */
/*     RANGE      O   Distance of the point from the origin. */
/*     AZ         O   Azimuth in radians. */
/*     EL         O   Elevation in radians. */

/* $ Detailed_Input */

/*     RECTAN   are the rectangular coordinates of a point. */

/*     AZCCW    is a flag indicating how azimuth is measured. */

/*              If AZCCW is .TRUE., azimuth increases in the */
/*              counterclockwise direction; otherwise it increases in */
/*              the clockwise direction. */

/*     ELPLSZ   is a flag indicating how elevation is measured. */

/*              If ELPLSZ is .TRUE., elevation increases from */
/*              the XY plane toward +Z; otherwise toward -Z. */

/* $ Detailed_Output */

/*     RANGE    is the distance of the point from the origin. */

/*              The units associated with RANGE are those associated */
/*              with the input point. */

/*     AZ       is the azimuth of the point. This is the angle between */
/*              the projection onto the XY plane of the vector from the */
/*              origin to the point and the +X axis of the reference */
/*              frame. AZ is zero at the +X axis. */

/*              The way azimuth is measured depends on the value of the */
/*              logical flag AZCCW. See the description of the argument */
/*              AZCCW for details. */

/*              AZ is output in radians. The range of AZ is [0, 2*pi]. */

/*     EL       is the elevation of the point. This is the angle between */
/*              the vector from the origin to the point and the XY */
/*              plane. EL is zero at the XY plane. */

/*              The way elevation is measured depends on the value of */
/*              the logical flag ELPLSZ. See the description of the */
/*              argument ELPLSZ for details. */

/*              EL is output in radians. The range of EL is [-pi/2, */
/*              pi/2]. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/*     1)  If the X and Y components of RECTAN are both zero, the */
/*         azimuth is set to zero. */

/*     2)  If RECTAN is the zero vector, azimuth and elevation */
/*         are both set to zero. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine returns the range, azimuth, and elevation of a point */
/*     specified in rectangular coordinates. */

/*     The output is defined by the distance from the center of the */
/*     reference frame (range), the angle from a reference vector */
/*     (azimuth), and the angle above the XY plane of the reference */
/*     frame (elevation). */

/*     The way azimuth and elevation are measured depends on the values */
/*     given by the user to the AZCCW and ELPLSZ logical flags. See the */
/*     descriptions of these input arguments for details. */

/* $ Examples */

/*     The numerical results shown for these examples may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */

/*     1) Create four tables showing a variety of rectangular */
/*        coordinates and the corresponding range, azimuth and */
/*        elevation, resulting from the different choices of the AZCCW */
/*        and ELPLSZ flags. */

/*        Corresponding rectangular coordinates and azimuth, elevation */
/*        and range are listed to three decimal places. Output angles */
/*        are in degrees. */


/*        Example code begins here. */


/*              PROGRAM RECAZL_EX1 */
/*              IMPLICIT NONE */

/*        C */
/*        C     SPICELIB functions */
/*        C */
/*              DOUBLE PRECISION      DPR */

/*        C */
/*        C     Local parameters. */
/*        C */
/*              INTEGER               NREC */
/*              PARAMETER           ( NREC = 11 ) */

/*        C */
/*        C     Local variables. */
/*        C */
/*              CHARACTER*(30)        MSG */

/*              DOUBLE PRECISION      AZ */
/*              DOUBLE PRECISION      EL */
/*              DOUBLE PRECISION      RANGE */
/*              DOUBLE PRECISION      RECTAN ( 3, NREC ) */

/*              INTEGER               I */
/*              INTEGER               J */
/*              INTEGER               K */
/*              INTEGER               N */

/*              LOGICAL               AZCCW  ( 2 ) */
/*              LOGICAL               ELPLSZ ( 2 ) */

/*        C */
/*        C     Define the input rectangular coordinates and the */
/*        C     different choices of the AZCCW and ELPLSZ flags. */
/*        C */
/*              DATA                  RECTAN / */
/*             .                  0.D0,         0.D0,         0.D0, */
/*             .                  1.D0,         0.D0,         0.D0, */
/*             .                  0.D0,         1.D0,         0.D0, */
/*             .                  0.D0,         0.D0,         1.D0, */
/*             .                 -1.D0,         0.D0,         0.D0, */
/*             .                  0.D0,        -1.D0,         0.D0, */
/*             .                  0.D0,         0.D0,        -1.D0, */
/*             .                  1.D0,         1.D0,         0.D0, */
/*             .                  1.D0,         0.D0,         1.D0, */
/*             .                  0.D0,         1.D0,         1.D0, */
/*             .                  1.D0,         1.D0,         1.D0  / */

/*              DATA                  AZCCW   /  .FALSE.,  .TRUE.  / */
/*              DATA                  ELPLSZ  /  .FALSE.,  .TRUE.  / */

/*        C */
/*        C     Create a table for each combination of AZCCW and ELPLSZ. */
/*        C */
/*              DO I = 1, 2 */

/*                 DO J = 1, 2 */

/*        C */
/*        C           Display the flag settings. */
/*        C */
/*                    MSG = 'AZCCW = #; ELPLSZ = #' */
/*                    CALL REPML ( MSG, '#', AZCCW(I),  'C', MSG ) */
/*                    CALL REPML ( MSG, '#', ELPLSZ(J), 'C', MSG ) */

/*                    WRITE(*,*) */
/*                    WRITE(*,'(A)') MSG */

/*        C */
/*        C           Print the banner. */
/*        C */
/*                    WRITE(*,*) */
/*                    WRITE(*,'(A)') '  RECT(1)  RECT(2)  RECT(3) ' */
/*             .       //            '  RANGE      AZ       EL' */
/*                    WRITE(*,'(A)') '  -------  -------  ------- ' */
/*             .       //            ' -------  -------  -------' */

/*        C */
/*        C           Do the conversion. Output angles in degrees. */
/*        C */
/*                    DO N = 1, NREC */

/*                       CALL RECAZL( RECTAN(1,N), AZCCW(I), ELPLSZ(J), */
/*             .                      RANGE,       AZ,       EL        ) */

/*                       WRITE (*,'(6F9.3)') ( RECTAN(K,N), K=1,3 ), */
/*             .                           RANGE, AZ * DPR(), EL * DPR() */

/*                    END DO */

/*                 END DO */

/*              END DO */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*        AZCCW = False; ELPLSZ = False */

/*          RECT(1)  RECT(2)  RECT(3)   RANGE      AZ       EL */
/*          -------  -------  -------  -------  -------  ------- */
/*            0.000    0.000    0.000    0.000    0.000    0.000 */
/*            1.000    0.000    0.000    1.000    0.000    0.000 */
/*            0.000    1.000    0.000    1.000  270.000    0.000 */
/*            0.000    0.000    1.000    1.000    0.000  -90.000 */
/*           -1.000    0.000    0.000    1.000  180.000    0.000 */
/*            0.000   -1.000    0.000    1.000   90.000    0.000 */
/*            0.000    0.000   -1.000    1.000    0.000   90.000 */
/*            1.000    1.000    0.000    1.414  315.000    0.000 */
/*            1.000    0.000    1.000    1.414    0.000  -45.000 */
/*            0.000    1.000    1.000    1.414  270.000  -45.000 */
/*            1.000    1.000    1.000    1.732  315.000  -35.264 */

/*        AZCCW = False; ELPLSZ = True */

/*          RECT(1)  RECT(2)  RECT(3)   RANGE      AZ       EL */
/*          -------  -------  -------  -------  -------  ------- */
/*            0.000    0.000    0.000    0.000    0.000    0.000 */
/*            1.000    0.000    0.000    1.000    0.000    0.000 */
/*            0.000    1.000    0.000    1.000  270.000    0.000 */
/*            0.000    0.000    1.000    1.000    0.000   90.000 */
/*           -1.000    0.000    0.000    1.000  180.000    0.000 */
/*            0.000   -1.000    0.000    1.000   90.000    0.000 */
/*            0.000    0.000   -1.000    1.000    0.000  -90.000 */
/*            1.000    1.000    0.000    1.414  315.000    0.000 */
/*            1.000    0.000    1.000    1.414    0.000   45.000 */
/*            0.000    1.000    1.000    1.414  270.000   45.000 */
/*            1.000    1.000    1.000    1.732  315.000   35.264 */

/*        AZCCW = True; ELPLSZ = False */

/*          RECT(1)  RECT(2)  RECT(3)   RANGE      AZ       EL */
/*          -------  -------  -------  -------  -------  ------- */
/*            0.000    0.000    0.000    0.000    0.000    0.000 */
/*            1.000    0.000    0.000    1.000    0.000    0.000 */
/*            0.000    1.000    0.000    1.000   90.000    0.000 */
/*            0.000    0.000    1.000    1.000    0.000  -90.000 */
/*           -1.000    0.000    0.000    1.000  180.000    0.000 */
/*            0.000   -1.000    0.000    1.000  270.000    0.000 */
/*            0.000    0.000   -1.000    1.000    0.000   90.000 */
/*            1.000    1.000    0.000    1.414   45.000    0.000 */
/*            1.000    0.000    1.000    1.414    0.000  -45.000 */
/*            0.000    1.000    1.000    1.414   90.000  -45.000 */
/*            1.000    1.000    1.000    1.732   45.000  -35.264 */

/*        AZCCW = True; ELPLSZ = True */

/*          RECT(1)  RECT(2)  RECT(3)   RANGE      AZ       EL */
/*          -------  -------  -------  -------  -------  ------- */
/*            0.000    0.000    0.000    0.000    0.000    0.000 */
/*            1.000    0.000    0.000    1.000    0.000    0.000 */
/*            0.000    1.000    0.000    1.000   90.000    0.000 */
/*            0.000    0.000    1.000    1.000    0.000   90.000 */
/*           -1.000    0.000    0.000    1.000  180.000    0.000 */
/*            0.000   -1.000    0.000    1.000  270.000    0.000 */
/*            0.000    0.000   -1.000    1.000    0.000  -90.000 */
/*            1.000    1.000    0.000    1.414   45.000    0.000 */
/*            1.000    0.000    1.000    1.414    0.000   45.000 */
/*            0.000    1.000    1.000    1.414   90.000   45.000 */
/*            1.000    1.000    1.000    1.732   45.000   35.264 */


/*     2) Compute the apparent azimuth and elevation of Venus as seen */
/*        from the DSS-14 station. */

/*        Task Description */
/*        ================ */

/*        In this example, we will obtain the apparent position of */
/*        Venus as seen from the DSS-14 station in the DSS-14 topocentric */
/*        reference frame. We will use a station frames kernel and */
/*        transform the resulting rectangular coordinates to azimuth, */
/*        elevation and range using AZLREC. */

/*        In order to introduce the usage of the logical flags AZCCW */
/*        and ELPLSZ, we will request the azimuth to be measured */
/*        clockwise and the elevation positive towards the +Z */
/*        axis of the DSS-14_TOPO reference frame. */


/*        Kernels */
/*        ======= */

/*        Use the meta-kernel shown below to load the required SPICE */
/*        kernels. */


/*           KPL/MK */

/*           File name: recazl_ex2.tm */

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
/*              de430.bsp                        Planetary ephemeris */
/*              naif0011.tls                     Leapseconds */
/*              earth_720101_070426.bpc          Earth historical */
/*                                               binary PCK */
/*              earthstns_itrf93_050714.bsp      DSN station SPK */
/*              earth_topo_050714.tf             DSN station FK */

/*           \begindata */

/*           KERNELS_TO_LOAD = ( 'de430.bsp', */
/*                               'naif0011.tls', */
/*                               'earth_720101_070426.bpc', */
/*                               'earthstns_itrf93_050714.bsp', */
/*                               'earth_topo_050714.tf'         ) */

/*           \begintext */

/*           End of meta-kernel. */


/*        Example code begins here. */


/*              PROGRAM RECAZL_EX2 */
/*              IMPLICIT NONE */

/*        C */
/*        C     SPICELIB functions */
/*        C */
/*              DOUBLE PRECISION      DPR */

/*        C */
/*        C     Local parameters */
/*        C */
/*              CHARACTER*(*)         FMT0 */
/*              PARAMETER           ( FMT0   = '(3F21.8)'  ) */

/*              CHARACTER*(*)         FMT1 */
/*              PARAMETER           ( FMT1   = '(A,F20.8)' ) */

/*              CHARACTER*(*)         META */
/*              PARAMETER           ( META   = 'recazl_ex2.tm' ) */

/*              INTEGER               BDNMLN */
/*              PARAMETER           ( BDNMLN = 36 ) */

/*              INTEGER               CORLEN */
/*              PARAMETER           ( CORLEN = 10 ) */

/*              INTEGER               FRNMLN */
/*              PARAMETER           ( FRNMLN = 32 ) */

/*              INTEGER               TIMLEN */
/*              PARAMETER           ( TIMLEN = 40 ) */

/*        C */
/*        C     Local variables */
/*        C */
/*              CHARACTER*(CORLEN)    ABCORR */
/*              CHARACTER*(BDNMLN)    OBS */
/*              CHARACTER*(TIMLEN)    OBSTIM */
/*              CHARACTER*(FRNMLN)    REF */
/*              CHARACTER*(BDNMLN)    TARGET */

/*              DOUBLE PRECISION      AZ */
/*              DOUBLE PRECISION      EL */
/*              DOUBLE PRECISION      ET */
/*              DOUBLE PRECISION      LT */
/*              DOUBLE PRECISION      PTARG  ( 3 ) */
/*              DOUBLE PRECISION      R */

/*              INTEGER               I */

/*              LOGICAL               AZCCW */
/*              LOGICAL               ELPLSZ */

/*        C */
/*        C     Load SPICE kernels. */
/*        C */
/*              CALL FURNSH ( META ) */

/*        C */
/*        C     Convert the observation time to seconds past J2000 TDB. */
/*        C */
/*              OBSTIM = '2003 OCT 13 06:00:00.000000 UTC' */

/*              CALL STR2ET ( OBSTIM, ET ) */

/*        C */
/*        C     Set the target, observer, observer frame, and */
/*        C     aberration corrections. */
/*        C */
/*              TARGET = 'VENUS' */
/*              OBS    = 'DSS-14' */
/*              REF    = 'DSS-14_TOPO' */
/*              ABCORR = 'CN+S' */

/*        C */
/*        C     Compute the observer-target position. */
/*        C */
/*              CALL SPKPOS ( TARGET, ET, REF, ABCORR, OBS, PTARG, LT ) */

/*        C */
/*        C     Compute azimuth, elevation and range of Venus */
/*        C     as seen from DSS-14, with azimuth increasing */
/*        C     clockwise and elevation positive towards +Z */
/*        C     axis of the DSS-14_TOPO reference frame */
/*        C */
/*              AZCCW  = .FALSE. */
/*              ELPLSZ = .TRUE. */

/*              CALL RECAZL ( PTARG, AZCCW, ELPLSZ, R, AZ, EL ) */

/*        C */
/*        C     Express both angles in degrees. */
/*        C */
/*              EL =   EL * DPR() */
/*              AZ =   AZ * DPR() */

/*        C */
/*        C     Display the computed position, the range and */
/*        C     the angles. */
/*        C */
/*              WRITE (*,*) */
/*              WRITE (*,'(2A)') 'Target:                ', TARGET */
/*              WRITE (*,'(2A)') 'Observation time:      ', OBSTIM */
/*              WRITE (*,'(2A)') 'Observer center:       ', OBS */
/*              WRITE (*,'(2A)') 'Observer frame:        ', REF */
/*              WRITE (*,'(2A)') 'Aberration correction: ', ABCORR */
/*              WRITE (*,*) */
/*              WRITE (*,'(A)')  'Observer-target position (km):' */
/*              WRITE (*,FMT0)  PTARG */
/*              WRITE (*,FMT1)  'Light time (s):       ', LT */
/*              WRITE (*,*) */
/*              WRITE (*,FMT1) 'Target azimuth          (deg): ', AZ */
/*              WRITE (*,FMT1) 'Target elevation        (deg): ', EL */
/*              WRITE (*,FMT1) 'Observer-target distance (km): ', R */
/*              WRITE (*,*) */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*        Target:                VENUS */
/*        Observation time:      2003 OCT 13 06:00:00.000000 UTC */
/*        Observer center:       DSS-14 */
/*        Observer frame:        DSS-14_TOPO */
/*        Aberration correction: CN+S */

/*        Observer-target position (km): */
/*            66886767.37916669   146868551.77222887  -185296611.10841593 */
/*        Light time (s):               819.63862811 */

/*        Target azimuth          (deg):         294.48543372 */
/*        Target elevation        (deg):         -48.94609726 */
/*        Observer-target distance (km):   245721478.99272084 */


/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     S.C. Krening       (JPL) */
/*     B.V. Semenov       (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 07-SEP-2021 (JDR) (NJB) (SCK) (BVS) */

/* -& */
/* $ Index_Entries */

/*     rectangular coordinates to range, az and el */
/*     rectangular to range, azimuth and elevation */
/*     convert rectangular coordinates to range, az and el */
/*     convert rectangular to range, azimuth and elevation */

/* -& */

/*     SPICELIB functions */


/*     Call the subroutine RECRAD to convert the rectangular coordinates */
/*     into right ascension and declination.  In RECRAD, the right */
/*     ascension is measured counterclockwise from +X axis about the +Z */
/*     axis, and the declination is measured positive from the XY plane */
/*     towards +Z axis. */

/*     The header of RECRAD says in part: */

/*        The range of RA is [0, 2*pi]. */
/*        The range of DEC is [-pi/2, pi/2]. */

/*     The range of AZ in the following call is that of RA. */
/*     The range of EL is that of DEC. */

    recrad_(rectan, range, az, el);

/*     If AZCCW is set to .FALSE. the azimuth is measured clockwise from */
/*     the +X axis about the +Z axis. */

    if (! (*azccw)) {

/*        Azimuth increases in the clockwise direction. */

/*        Map AZ to its 2*pi complement if AZ is non-zero. Don't map */
/*        zero to 2*pi. The value of AZ returned from RECRAD is never */
/*        negative, but it may be zero. */

	if (*az > 0.) {

/*            Replace AZ with its 2*pi complement. */

/*            This assignment requires that ACOS( -1.D0 ) be exactly */
/*            equal to DATAN2( 0.D0, <non-zero value> ). This should be */
/*            true for any correct arithmetic implementation. */

/*            Although we expect the result to always be non-negative, */
/*            we take no chances. */

/* Computing MAX */
	    d__1 = twopi_() - *az;
	    *az = max(d__1,0.);
	}
    }

/*     If ELPLSZ is set to .FALSE. the elevation is measured positive */
/*     from the XY plane toward the -Z axis. */

    if (! (*elplsz)) {

/*        Negate only non-zero values of EL. Avoid creating */
/*        -0.D0 values, which affect printed outputs generated */
/*        by example programs. */

	if (*el != 0.) {
	    *el = -(*el);
	}
    }
    return 0;
} /* recazl_ */

