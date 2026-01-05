/* azlrec.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure AZLREC ( AZ/EL to rectangular coordinates ) */
/* Subroutine */ int azlrec_(doublereal *range, doublereal *az, doublereal *
	el, logical *azccw, logical *elplsz, doublereal *rectan)
{
    extern /* Subroutine */ int latrec_(doublereal *, doublereal *, 
	    doublereal *, doublereal *);
    doublereal elevat, azimut;

/* $ Abstract */

/*     Convert from range, azimuth and elevation of a point to */
/*     rectangular coordinates. */

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
/*     RANGE      I   Distance of the point from the origin. */
/*     AZ         I   Azimuth in radians. */
/*     EL         I   Elevation in radians. */
/*     AZCCW      I   Flag indicating how azimuth is measured. */
/*     ELPLSZ     I   Flag indicating how elevation is measured. */
/*     RECTAN     O   Rectangular coordinates of a point. */

/* $ Detailed_Input */

/*     RANGE    is the distance of the point from the origin. The */
/*              input should be in terms of the same units in which */
/*              the output is desired. */

/*              Although negative values for RANGE are allowed, its */
/*              use may lead to undesired results. See the $Exceptions */
/*              section for a discussion on this topic. */

/*     AZ       is the azimuth of the point. This is the angle between */
/*              the projection onto the XY plane of the vector from */
/*              the origin to the point and the +X axis of the */
/*              reference frame. AZ is zero at the +X axis. */

/*              The way azimuth is measured depends on the value of */
/*              the logical flag AZCCW. See the description of the */
/*              argument AZCCW for details. */

/*              The range (i.e., the set of allowed values) of AZ is */
/*              unrestricted. See the $Exceptions section for a */
/*              discussion on the AZ range. */

/*              Units are radians. */

/*     EL       is the elevation of the point. This is the angle */
/*              between the vector from the origin to the point and */
/*              the XY plane. EL is zero at the XY plane. */

/*              The way elevation is measured depends on the value of */
/*              the logical flag ELPLSZ. See the description of the */
/*              argument ELPLSZ for details. */

/*              The range (i.e., the set of allowed values) of EL is */
/*              [-pi/2, pi/2], but no error checking is done to ensure */
/*              that EL is within this range. See the $Exceptions */
/*              section for a discussion on the EL range. */

/*              Units are radians. */

/*     AZCCW    is a flag indicating how the azimuth is measured. */

/*              If AZCCW is .TRUE., the azimuth increases in the */
/*              counterclockwise direction; otherwise it increases */
/*              in the clockwise direction. */

/*     ELPLSZ   is a flag indicating how the elevation is measured. */

/*              If ELPLSZ is .TRUE., the elevation increases from */
/*              the XY plane toward +Z; otherwise toward -Z. */

/* $ Detailed_Output */

/*     RECTAN   is an array containing the rectangular coordinates of */
/*              the point. */

/*              The units associated with the point are those */
/*              associated with the input RANGE. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/*     1)  If the value of the input argument RANGE is negative */
/*         the output rectangular coordinates will be negated, i.e. */
/*         the resulting array will be of the same length */
/*         but opposite direction to the one that would be obtained */
/*         with a positive input argument RANGE of value ||RANGE||. */

/*     2)  If the value of the input argument EL is outside the */
/*         range [-pi/2, pi/2], the results may not be as */
/*         expected. */

/*     3)  If the value of the input argument AZ is outside the */
/*         range [0, 2*pi], the value will be mapped to a value */
/*         inside the range that differs from the input value by an */
/*         integer multiple of 2*pi. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine converts the azimuth, elevation, and range */
/*     of a point into the associated rectangular coordinates. */

/*     The input is defined by the distance from the center of */
/*     the reference frame (range), the angle from a reference */
/*     vector (azimuth), and the angle above the XY plane of the */
/*     reference frame (elevation). */

/*     The way azimuth and elevation are measured depends on the */
/*     values given by the user to the AZCCW and ELPLSZ logical */
/*     flags. See the descriptions of these input arguments */
/*     for details. */

/* $ Examples */

/*     The numerical results shown for these examples may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */

/*     1) Create four tables showing a variety of azimuth/elevation */
/*        coordinates and the corresponding rectangular coordinates, */
/*        resulting from the different choices of the AZCCW and ELPLSZ */
/*        flags. */

/*        Corresponding azimuth/elevation and rectangular coordinates */
/*        are listed to three decimal places. Input angles are in */
/*        degrees. */


/*        Example code begins here. */


/*              PROGRAM AZLREC_EX1 */
/*              IMPLICIT NONE */

/*        C */
/*        C     SPICELIB functions */
/*        C */
/*              DOUBLE PRECISION      RPD */

/*        C */
/*        C     Local parameters. */
/*        C */
/*              INTEGER               NREC */
/*              PARAMETER           ( NREC = 11 ) */

/*        C */
/*        C     Local variables. */
/*        C */
/*              CHARACTER*(30)        MSG */

/*              DOUBLE PRECISION      AZ     ( NREC ) */
/*              DOUBLE PRECISION      EL     ( NREC ) */
/*              DOUBLE PRECISION      RANGE  ( NREC ) */
/*              DOUBLE PRECISION      RAZ */
/*              DOUBLE PRECISION      REL */
/*              DOUBLE PRECISION      RECTAN ( 3 ) */

/*              INTEGER               I */
/*              INTEGER               J */
/*              INTEGER               K */
/*              INTEGER               N */

/*              LOGICAL               AZCCW  ( 2 ) */
/*              LOGICAL               ELPLSZ ( 2 ) */

/*        C */
/*        C     Define the input azimuth/elevation coordinates and the */
/*        C     different choices of the AZCCW and ELPLSZ flags. */
/*        C */
/*              DATA                  RANGE   / */
/*             .                            0.D0,    1.D0,    1.D0, */
/*             .                            1.D0,    1.D0,    1.D0, */
/*             .                            1.D0,    1.414D0, 1.414D0, */
/*             .                            1.414D0, 1.732D0           / */

/*              DATA                  AZ     / */
/*             .                            0.D0,    0.D0,   270.D0, */
/*             .                            0.D0,  180.D0,    90.D0, */
/*             .                            0.D0,  315.D0,     0.D0, */
/*             .                          270.D0,  315.D0            / */

/*              DATA                  EL     / */
/*             .                            0.D0,    0.D0,     0.D0, */
/*             .                          -90.D0,    0.D0,     0.D0, */
/*             .                           90.D0,    0.D0,   -45.D0, */
/*             .                          -45.D0,  -35.264D0         / */

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
/*                    WRITE(*,'(A)') '   RANGE      AZ       EL   ' */
/*             .       //            ' RECT(1)  RECT(2)  RECT(3)' */
/*                    WRITE(*,'(A)') '  -------  -------  ------- ' */
/*             .       //            ' -------  -------  -------' */

/*        C */
/*        C           Do the conversion. Input angles in degrees. */
/*        C */
/*                    DO N = 1, NREC */

/*                       RAZ = AZ(N) * RPD() */
/*                       REL = EL(N) * RPD() */

/*                       CALL AZLREC ( RANGE(N), RAZ,       REL, */
/*             .                       AZCCW(I), ELPLSZ(J), RECTAN ) */

/*                       WRITE (*,'(6F9.3)')  RANGE(N), AZ(N), EL(N), */
/*             .                              RECTAN */

/*                    END DO */

/*                 END DO */

/*              END DO */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*        AZCCW = False; ELPLSZ = False */

/*           RANGE      AZ       EL    RECT(1)  RECT(2)  RECT(3) */
/*          -------  -------  -------  -------  -------  ------- */
/*            0.000    0.000    0.000    0.000    0.000    0.000 */
/*            1.000    0.000    0.000    1.000    0.000    0.000 */
/*            1.000  270.000    0.000   -0.000    1.000    0.000 */
/*            1.000    0.000  -90.000    0.000    0.000    1.000 */
/*            1.000  180.000    0.000   -1.000   -0.000    0.000 */
/*            1.000   90.000    0.000    0.000   -1.000    0.000 */
/*            1.000    0.000   90.000    0.000    0.000   -1.000 */
/*            1.414  315.000    0.000    1.000    1.000    0.000 */
/*            1.414    0.000  -45.000    1.000    0.000    1.000 */
/*            1.414  270.000  -45.000   -0.000    1.000    1.000 */
/*            1.732  315.000  -35.264    1.000    1.000    1.000 */

/*        AZCCW = False; ELPLSZ = True */

/*           RANGE      AZ       EL    RECT(1)  RECT(2)  RECT(3) */
/*          -------  -------  -------  -------  -------  ------- */
/*            0.000    0.000    0.000    0.000    0.000    0.000 */
/*            1.000    0.000    0.000    1.000    0.000    0.000 */
/*            1.000  270.000    0.000   -0.000    1.000    0.000 */
/*            1.000    0.000  -90.000    0.000    0.000   -1.000 */
/*            1.000  180.000    0.000   -1.000   -0.000    0.000 */
/*            1.000   90.000    0.000    0.000   -1.000    0.000 */
/*            1.000    0.000   90.000    0.000    0.000    1.000 */
/*            1.414  315.000    0.000    1.000    1.000    0.000 */
/*            1.414    0.000  -45.000    1.000    0.000   -1.000 */
/*            1.414  270.000  -45.000   -0.000    1.000   -1.000 */
/*            1.732  315.000  -35.264    1.000    1.000   -1.000 */

/*        AZCCW = True; ELPLSZ = False */

/*           RANGE      AZ       EL    RECT(1)  RECT(2)  RECT(3) */
/*          -------  -------  -------  -------  -------  ------- */
/*            0.000    0.000    0.000    0.000    0.000    0.000 */
/*            1.000    0.000    0.000    1.000    0.000    0.000 */
/*            1.000  270.000    0.000   -0.000   -1.000    0.000 */
/*            1.000    0.000  -90.000    0.000    0.000    1.000 */
/*            1.000  180.000    0.000   -1.000    0.000    0.000 */
/*            1.000   90.000    0.000    0.000    1.000    0.000 */
/*            1.000    0.000   90.000    0.000    0.000   -1.000 */
/*            1.414  315.000    0.000    1.000   -1.000    0.000 */
/*            1.414    0.000  -45.000    1.000    0.000    1.000 */
/*            1.414  270.000  -45.000   -0.000   -1.000    1.000 */
/*            1.732  315.000  -35.264    1.000   -1.000    1.000 */

/*        AZCCW = True; ELPLSZ = True */

/*           RANGE      AZ       EL    RECT(1)  RECT(2)  RECT(3) */
/*          -------  -------  -------  -------  -------  ------- */
/*            0.000    0.000    0.000    0.000    0.000    0.000 */
/*            1.000    0.000    0.000    1.000    0.000    0.000 */
/*            1.000  270.000    0.000   -0.000   -1.000    0.000 */
/*            1.000    0.000  -90.000    0.000    0.000   -1.000 */
/*            1.000  180.000    0.000   -1.000    0.000    0.000 */
/*            1.000   90.000    0.000    0.000    1.000    0.000 */
/*            1.000    0.000   90.000    0.000    0.000    1.000 */
/*            1.414  315.000    0.000    1.000   -1.000    0.000 */
/*            1.414    0.000  -45.000    1.000    0.000   -1.000 */
/*            1.414  270.000  -45.000   -0.000   -1.000   -1.000 */
/*            1.732  315.000  -35.264    1.000   -1.000   -1.000 */


/*     2) Compute the right ascension and declination of the pointing */
/*        direction of DSS-14 station at a given epoch. */

/*        Task Description */
/*        ================ */

/*        In this example, we will obtain the right ascension and */
/*        declination of the pointing direction of the DSS-14 station at */
/*        a given epoch, by converting the station's pointing direction */
/*        given in azimuth and elevation to rectangular coordinates */
/*        in the station topocentric reference frame and applying a */
/*        frame transformation from DSS-14_TOPO to J2000, in order to */
/*        finally obtain the corresponding right ascension and */
/*        declination of the pointing vector. */

/*        In order to introduce the usage of the logical flags AZCCW */
/*        and ELPLSZ, we will assume that the azimuth is measured */
/*        counterclockwise and the elevation negative towards +Z */
/*        axis of the DSS-14_TOPO reference frame. */

/*        Kernels */
/*        ======= */

/*        Use the meta-kernel shown below to load the required SPICE */
/*        kernels. */


/*           KPL/MK */

/*           File name: azlrec_ex2.tm */

/*           This meta-kernel is intended to support operation of SPICE */
/*           example programs. The kernels shown here should not be */
/*           assumed to contain adequate or correct versions of data */
/*           required by SPICE-based user applications. */

/*           In order for an application to use this meta-kernel, the */
/*           kernels referenced here must be present in the user's */
/*           current working directory. */

/*           The names and contents of the kernels referenced */
/*           by this meta-kernel are as follows: */

/*             File name                        Contents */
/*             ---------                        -------- */
/*             naif0011.tls                     Leapseconds */
/*             earth_720101_070426.bpc          Earth historical */
/*                                              binary PCK */
/*             earth_topo_050714.tf             DSN station FK */

/*           \begindata */

/*           KERNELS_TO_LOAD = ( 'naif0011.tls', */
/*                               'earth_720101_070426.bpc', */
/*                               'earth_topo_050714.tf'     ) */

/*           \begintext */

/*           End of meta-kernel. */


/*        Example code begins here. */


/*              PROGRAM AZLREC_EX2 */
/*              IMPLICIT NONE */

/*        C */
/*        C     SPICELIB functions */
/*        C */
/*              DOUBLE PRECISION      DPR */
/*              DOUBLE PRECISION      RPD */

/*        C */
/*        C     Local parameters */
/*        C */
/*              CHARACTER*(*)         FMT0 */
/*              PARAMETER           ( FMT0   = '(A,3F15.8)' ) */

/*              CHARACTER*(*)         FMT1 */
/*              PARAMETER           ( FMT1   = '(A,F15.8)' ) */

/*              CHARACTER*(*)         META */
/*              PARAMETER           ( META   = 'azlrec_ex2.tm' ) */

/*              INTEGER               FRNMLN */
/*              PARAMETER           ( FRNMLN = 32 ) */

/*              INTEGER               TIMLEN */
/*              PARAMETER           ( TIMLEN = 40 ) */

/*        C */
/*        C     Local variables */
/*        C */
/*              CHARACTER*(40)        MSG */
/*              CHARACTER*(TIMLEN)    OBSTIM */
/*              CHARACTER*(FRNMLN)    REF */

/*              DOUBLE PRECISION      AZ */
/*              DOUBLE PRECISION      AZR */
/*              DOUBLE PRECISION      DEC */
/*              DOUBLE PRECISION      EL */
/*              DOUBLE PRECISION      ELR */
/*              DOUBLE PRECISION      ET */
/*              DOUBLE PRECISION      JPOS   ( 3 ) */
/*              DOUBLE PRECISION      PTARG  ( 3 ) */
/*              DOUBLE PRECISION      R */
/*              DOUBLE PRECISION      RA */
/*              DOUBLE PRECISION      RANGE */
/*              DOUBLE PRECISION      ROTATE ( 3, 3 ) */

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
/*        C     Set the local topocentric frame */
/*        C */
/*              REF    = 'DSS-14_TOPO' */

/*        C */
/*        C     Set the station's pointing direction in azimuth and */
/*        C     elevation. Set arbitrarily the range to 1.0. Azimuth */
/*        C     and elevation shall be given in radians. Azimuth */
/*        C     increases counterclockwise and elevation is negative */
/*        C     towards +Z (above the local horizon) */
/*        C */
/*              AZ     =   75.00 */
/*              EL     =  -27.25 */
/*              AZR    =   AZ * RPD() */
/*              ELR    =   EL * RPD() */
/*              R      =    1.00 */
/*              AZCCW  = .TRUE. */
/*              ELPLSZ = .FALSE. */

/*        C */
/*        C     Obtain the rectangular coordinates of the station's */
/*        C     pointing direction. */
/*        C */
/*              CALL AZLREC ( R, AZR, ELR, AZCCW, ELPLSZ, PTARG ) */

/*        C */
/*        C     Transform the station's pointing vector from the */
/*        C     local topocentric frame to J2000. */
/*        C */
/*              CALL PXFORM ( REF,   'J2000',   ET, ROTATE ) */
/*              CALL MXV    ( ROTATE,  PTARG, JPOS         ) */

/*        C */
/*        C     Compute the right ascension and declination. */
/*        C     Express both angles in degrees. */
/*        C */
/*              CALL RECRAD ( JPOS, RANGE, RA, DEC ) */
/*              RA =   RA * DPR() */
/*              DEC =   DEC * DPR() */

/*        C */
/*        C     Display the computed pointing vector, the input */
/*        C     data and resulting the angles. */
/*        C */
/*              WRITE (*,*) */
/*              WRITE (*,FMT1) 'Pointing azimuth    (deg): ', AZ */
/*              WRITE (*,FMT1) 'Pointing elevation  (deg): ', EL */

/*              CALL REPML ( 'Azimuth counterclockwise?: #', '#', */
/*             .              AZCCW, 'C', MSG                    ) */
/*              WRITE (*,'(A)') MSG */

/*              CALL REPML ( 'Elevation positive +Z?   : #', '#', */
/*             .              ELPLSZ, 'C', MSG                   ) */
/*              WRITE (*,'(A)') MSG */

/*              WRITE (*,'(2A)') 'Observation epoch        : ', OBSTIM */
/*              WRITE (*,*) */
/*              WRITE (*,'(A)') 'Pointing direction (normalized):   ' */
/*              WRITE (*,FMT0) '  ', ( PTARG(I), I = 1, 3 ) */
/*              WRITE (*,*) */
/*              WRITE (*,FMT1) 'Pointing right ascension (deg): ', RA */
/*              WRITE (*,FMT1) 'Pointing declination (deg):     ', DEC */
/*              WRITE (*,*) */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*        Pointing azimuth    (deg):     75.00000000 */
/*        Pointing elevation  (deg):    -27.25000000 */
/*        Azimuth counterclockwise?: True */
/*        Elevation positive +Z?   : False */
/*        Observation epoch        : 2003 OCT 13 06:00:00.000000 UTC */

/*        Pointing direction (normalized): */
/*               0.23009457     0.85872462     0.45787392 */

/*        Pointing right ascension (deg):    280.06179939 */
/*        Pointing declination (deg):         26.92826084 */


/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 08-SEP-2021 (JDR) (NJB) */

/* -& */
/* $ Index_Entries */

/*     range, az and el to rectangular coordinates */
/*     range, azimuth and elevation to rectangular */
/*     convert range, az and el to rectangular coordinates */
/*     convert range, azimuth and elevation to rectangular */

/* -& */

/*     Local variables */


/*     Error free routine. No check-in. */

    azimut = *az;
    elevat = *el;

/*     We are going to use RADREC to convert the azimuth and elevation */
/*     to rectangular coordinates. In RADREC, the right ascension */
/*     is measured counterclockwise from +X axis about the +Z axis, and */
/*     the declination is measured positive from the XY plane towards */
/*     +Z axis. */

/*     Check the AZCCW and ELPLSZ flags and convert AZ and EL to the */
/*     coordinate system used by RADREC. */

/*     If AZCCW is set to .FALSE. the azimuth is measured clockwise */
/*     from the +X axis about the +Z axis. */

    if (! (*azccw)) {

/*        We can simply negate AZ; we don't need to map it into the */
/*        range [0, 2*pi]. LATREC will accept it as is. */

/*        Negate only non-zero values of AZ. Avoid creating */
/*        -0.D0 values, which affect printed outputs generated */
/*        by example programs. */

	if (*az != 0.) {
	    azimut = -(*az);
	}
    }

/*     If ELPLSZ is set to .FALSE. the elevation is measured positive */
/*     from the XY plane towards the -Z axis. */

    if (! (*elplsz)) {

/*        Negate only non-zero values of EL. Avoid creating */
/*        -0.D0 values, which affect printed outputs generated */
/*        by example programs. */

	if (*el != 0.) {
	    elevat = -(*el);
	}
    }

/*     In principle, we could call the subroutine RADREC to convert AZ */
/*     and EL to rectangular coordinates. RADREC simply passes its */
/*     inputs to LATREC, so we bypass the middleman. */

/*     We rely on LATREC to handle the case of RANGE < 0. */

    latrec_(range, &azimut, &elevat, rectan);
    return 0;
} /* azlrec_ */

