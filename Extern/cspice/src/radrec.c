/* radrec.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure RADREC ( Range, RA and DEC to rectangular coordinates ) */
/* Subroutine */ int radrec_(doublereal *range, doublereal *ra, doublereal *
	dec, doublereal *rectan)
{
    extern /* Subroutine */ int latrec_(doublereal *, doublereal *, 
	    doublereal *, doublereal *);

/* $ Abstract */

/*     Convert from range, right ascension, and declination to */
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
/*     --------  ---  --------------------------------------------------- */
/*     RANGE      I   Distance of a point from the origin. */
/*     RA         I   Right ascension of point in radians. */
/*     DEC        I   Declination of point in radians. */
/*     RECTAN     O   Rectangular coordinates of the point. */

/* $ Detailed_Input */

/*     RANGE    is the distance of the point from the origin. Input */
/*              should be in terms of the same units in which the */
/*              output is desired. */

/*     RA       is the right ascension of the point. This is the angular */
/*              distance measured toward the east from the prime */
/*              meridian to the meridian containing the input point. */
/*              The direction of increasing right ascension is from */
/*              the +X axis towards the +Y axis. */

/*              The range (i.e., the set of allowed values) of */
/*              RA is unrestricted. Units are radians. */

/*     DEC      is the declination of the point. This is the angle from */
/*              the XY plane of the ray from the origin through the */
/*              point. */

/*              The range (i.e., the set of allowed values) of */
/*              DEC is unrestricted. Units are radians. */

/* $ Detailed_Output */

/*     RECTAN   is the array containing the rectangular coordinates of */
/*              the point. */

/*              The units associated with RECTAN are those */
/*              associated with the input RANGE. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine converts the right ascension, declination, and range */
/*     of a point into the associated rectangular coordinates. */

/*     The input is defined by a distance from a central reference point, */
/*     an angle from a reference meridian, and an angle above the equator */
/*     of a sphere centered at the central reference point. */

/* $ Examples */

/*     The numerical results shown for these examples may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */

/*     1) Convert to the J2000 frame the right ascension and declination */
/*        of an object initially expressed with respect to the B1950 */
/*        reference frame. */


/*        Example code begins here. */


/*              PROGRAM RADREC_EX1 */
/*              IMPLICIT NONE */

/*        C */
/*        C     SPICELIB functions. */
/*        C */
/*              DOUBLE PRECISION      DPR */
/*              DOUBLE PRECISION      RPD */

/*        C */
/*        C     Local variables */
/*        C */
/*              DOUBLE PRECISION      DECB */
/*              DOUBLE PRECISION      DECJ */
/*              DOUBLE PRECISION      MTRANS ( 3, 3 ) */
/*              DOUBLE PRECISION      R */
/*              DOUBLE PRECISION      RAB */
/*              DOUBLE PRECISION      RAJ */
/*              DOUBLE PRECISION      V1950  ( 3    ) */
/*              DOUBLE PRECISION      V2000  ( 3    ) */

/*        C */
/*        C     Set the initial right ascension and declination */
/*        C     coordinates of the object, given with respect */
/*        C     to the B1950 reference frame. */
/*        C */
/*              RAB  = 135.88680896D0 */
/*              DECB =  17.50151037D0 */

/*        C */
/*        C     Convert RAB and DECB to a 3-vector expressed in */
/*        C     the B1950 frame. */
/*        C */
/*              CALL RADREC ( 1.D0, RAB * RPD(), DECB * RPD(), V1950 ) */

/*        C */
/*        C     We use the SPICELIB routine PXFORM to obtain the */
/*        C     transformation  matrix for converting vectors between */
/*        C     the B1950 and J2000 reference frames.  Since */
/*        C     both frames are inertial, the input time value we */
/*        C     supply to PXFORM is arbitrary.  We choose zero */
/*        C     seconds past the J2000 epoch. */
/*        C */
/*              CALL PXFORM ( 'B1950', 'J2000', 0.D0, MTRANS ) */

/*        C */
/*        C     Transform the vector to the J2000 frame. */
/*        C */
/*              CALL MXV ( MTRANS, V1950, V2000 ) */

/*        C */
/*        C     Find the right ascension and declination of the */
/*        C     J2000-relative vector. */
/*        C */
/*              CALL RECRAD ( V2000, R, RAJ, DECJ ) */

/*        C */
/*        C     Output the results. */
/*        C */
/*              WRITE(*,*) 'Right ascension (B1950 frame): ', RAB */
/*              WRITE(*,*) 'Declination (B1950 frame)    : ', DECB */

/*              WRITE(*,*) 'Right ascension (J2000 frame): ', */
/*             .                                       RAJ * DPR() */
/*              WRITE(*,*) 'Declination (J2000 frame)    : ', */
/*             .                                      DECJ * DPR() */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*         Right ascension (B1950 frame):    135.88680896000000 */
/*         Declination (B1950 frame)    :    17.501510369999998 */
/*         Right ascension (J2000 frame):    136.58768235448090 */
/*         Declination (J2000 frame)    :    17.300442748830637 */


/*     2) Define a set of 15 right ascension-declination data pairs for */
/*        the Earth's pole at different ephemeris epochs, convert them */
/*        to rectangular coordinates and compute the angular separation */
/*        between these coordinates and the IAU_EARTH pole given by a */
/*        PCK kernel. */

/*        Use the PCK kernel below to load the required triaxial */
/*        ellipsoidal shape model and orientation data for the Earth. */

/*           pck00010.tpc */


/*        Example code begins here. */


/*              PROGRAM RADREC_EX2 */
/*              IMPLICIT NONE */

/*        C */
/*        C     SPICELIB functions. */
/*        C */
/*              DOUBLE PRECISION      DPR */
/*              DOUBLE PRECISION      RPD */
/*              DOUBLE PRECISION      VSEP */

/*        C */
/*        C     Local parameters. */
/*        C */
/*              INTEGER               NCOORD */
/*              PARAMETER           ( NCOORD = 15 ) */

/*        C */
/*        C     Local variables */
/*        C */
/*              DOUBLE PRECISION      DEC    ( NCOORD ) */
/*              DOUBLE PRECISION      ET     ( NCOORD ) */
/*              DOUBLE PRECISION      POLE   ( 3      ) */
/*              DOUBLE PRECISION      MTRANS ( 3, 3   ) */
/*              DOUBLE PRECISION      RA     ( NCOORD ) */
/*              DOUBLE PRECISION      V2000  ( 3      ) */
/*              DOUBLE PRECISION      Z      ( 3      ) */

/*              INTEGER               I */

/*        C */
/*        C     Define a set of 15 right ascension-declination */
/*        C     coordinate pairs (in degrees) for the Earth's pole */
/*        C     and the array of corresponding ephemeris times in */
/*        C     J2000 TDB seconds. */
/*        C */
/*              DATA                  RA   / */
/*             .           180.003739D0, 180.003205D0, 180.002671D0, */
/*             .           180.002137D0, 180.001602D0, 180.001068D0, */
/*             .           180.000534D0, 360.000000D0, 359.999466D0, */
/*             .           359.998932D0, 359.998397D0, 359.997863D0, */
/*             .           359.997329D0, 359.996795D0, 359.996261D0  / */

/*              DATA                  DEC  / */
/*             .            89.996751D0,  89.997215D0,  89.997679D0, */
/*             .            89.998143D0,  89.998608D0,  89.999072D0, */
/*             .            89.999536D0,  90.000000D0,  89.999536D0, */
/*             .            89.999072D0,  89.998607D0,  89.998143D0, */
/*             .            89.997679D0,  89.997215D0,  89.996751D0  / */

/*              DATA                  ET   /   -18408539.52023917D0, */
/*             .         -15778739.49107254D0, -13148939.46190590D0, */
/*             .         -10519139.43273926D0,  -7889339.40357262D0, */
/*             .          -5259539.37440598D0,  -2629739.34523934D0, */
/*             .                60.68392730D0,   2629860.71309394D0, */
/*             .           5259660.74226063D0,   7889460.77142727D0, */
/*             .          10519260.80059391D0,  13149060.82976055D0, */
/*             .          15778860.85892719D0,  18408660.88809383D0  / */

/*              DATA                  Z    /  0.D0, 0.D0, 1.D0  / */

/*        C */
/*        C     Load a PCK kernel. */
/*        C */
/*              CALL FURNSH ( 'pck00010.tpc' ) */

/*        C */
/*        C     Print the banner out. */
/*        C */
/*              WRITE(*,'(A)') '       ET           Angular difference' */
/*              WRITE(*,'(A)') '------------------  ------------------' */

/*              DO I = 1, NCOORD */

/*        C */
/*        C        Convert the right ascension and declination */
/*        C        coordinates (in degrees) to rectangular. */
/*        C */
/*                 CALL RADREC ( 1.D0, RA(I) * RPD(), DEC(I) * RPD(), */
/*             .                 V2000                               ) */

/*        C */
/*        C        Retrieve the transformation matrix from the J2000 */
/*        C        frame to the IAU_EARTH frame. */
/*        C */
/*                 CALL PXFORM ( 'J2000', 'IAU_EARTH', ET(I), MTRANS ) */

/*        C */
/*        C        Rotate the V2000 vector into IAU_EARTH. This vector */
/*        C        should equal (round-off) the Z direction unit vector. */
/*        C */
/*                 CALL MXV ( MTRANS, V2000, POLE ) */

/*        C */
/*        C        Output the ephemeris time and the angular separation */
/*        C        between the rotated vector and the Z direction unit */
/*        C        vector. */
/*        C */
/*                 WRITE(*,'(F18.8,2X,F18.16)') ET(I), */
/*             .                                VSEP( POLE, Z ) * DPR() */

/*              END DO */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*               ET           Angular difference */
/*        ------------------  ------------------ */
/*        -18408539.52023917  0.0000001559918278 */
/*        -15778739.49107254  0.0000000106799881 */
/*        -13148939.46190590  0.0000001773517911 */
/*        -10519139.43273926  0.0000003440236194 */
/*         -7889339.40357262  0.0000004893045693 */
/*         -5259539.37440598  0.0000003226327536 */
/*         -2629739.34523934  0.0000001559609507 */
/*               60.68392730  0.0000000107108706 */
/*          2629860.71309394  0.0000001773826862 */
/*          5259660.74226063  0.0000003440544891 */
/*          7889460.77142727  0.0000004892736740 */
/*         10519260.80059391  0.0000003226018712 */
/*         13149060.82976055  0.0000001559300556 */
/*         15778860.85892719  0.0000000107417474 */
/*         18408660.88809383  0.0000001774135760 */


/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     [1]  L. Taff, "Celestial Mechanics, A Computational Guide for the */
/*          Practitioner," Wiley, 1985 */

/* $ Author_and_Institution */

/*     C.H. Acton         (JPL) */
/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     H.A. Neilan        (JPL) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 06-JUL-2021 (JDR) */

/*        Added IMPLICIT NONTE statement. */

/*        Edited the header to comply with NAIF standard. Removed */
/*        unnecessary $Revisions section. */

/*        Added complete  code example based on existing code fragment */
/*        and a second example. */

/* -    SPICELIB Version 1.0.2, 30-JUL-2003 (NJB) (CHA) */

/*        Various header changes were made to improve clarity. Some */
/*        minor header corrections were made. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (HAN) */

/* -& */
/* $ Index_Entries */

/*     range ra and dec to rectangular coordinates */
/*     right_ascension and declination to rectangular */

/* -& */

/*     Convert from range, right ascension, and declination to */
/*     rectangular coordinates by calling the routine LATREC. */

    latrec_(range, ra, dec, rectan);
    return 0;
} /* radrec_ */

