/* oscltx.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__12 = 12;
static doublereal c_b5 = 0.;
static doublereal c_b6 = 1e-10;
static integer c__1 = 1;
static integer c__3 = 3;
static doublereal c_b13 = .66666666666666663;
static doublereal c_b14 = .33333333333333331;
static doublereal c_b15 = 1.5;

/* $Procedure OSCLTX ( Extended osculating elements from state ) */
/* Subroutine */ int oscltx_(doublereal *state, doublereal *et, doublereal *
	mu, doublereal *elts)
{
    /* Initialized data */

    static logical pass1 = TRUE_;
    static doublereal limit = 0.;

    /* System generated locals */
    doublereal d__1;

    /* Builtin functions */
    double pow_dd(doublereal *, doublereal *);

    /* Local variables */
    doublereal rmag, vmag, rhat[3], pole[3], xmat[9]	/* was [3][3] */;
    extern doublereal vdot_(doublereal *, doublereal *);
    extern /* Subroutine */ int vequ_(doublereal *, doublereal *);
    doublereal a, b, e, range;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    extern doublereal exact_(doublereal *, doublereal *, doublereal *), 
	    dpmax_(void);
    doublereal rperi[3];
    extern /* Subroutine */ int vlcom_(doublereal *, doublereal *, doublereal 
	    *, doublereal *, doublereal *);
    doublereal c1, c2;
    extern /* Subroutine */ int ucrss_(doublereal *, doublereal *, doublereal 
	    *), unorm_(doublereal *, doublereal *, doublereal *);
    extern doublereal vnorm_(doublereal *), twopi_(void);
    extern logical failed_(void);
    doublereal eccvec[3];
    extern /* Subroutine */ int cleard_(integer *, doublereal *), recrad_(
	    doublereal *, doublereal *, doublereal *, doublereal *);
    doublereal nu;
    logical cmptau;
    extern /* Subroutine */ int oscelt_(doublereal *, doublereal *, 
	    doublereal *, doublereal *);
    doublereal mucubr;
    extern /* Subroutine */ int chkout_(char *, ftnlen), twovec_(doublereal *,
	     integer *, doublereal *, integer *, doublereal *);
    extern logical return_(void);
    doublereal ecc, dec, vel[3], tau;
    extern /* Subroutine */ int mxv_(doublereal *, doublereal *, doublereal *)
	    ;

/* $ Abstract */

/*     Determine the set of osculating conic orbital elements that */
/*     corresponds to the state (position, velocity) of a body at some */
/*     epoch. In additional to the classical elements, return the true */
/*     anomaly, semi-major axis, and period, if applicable. */

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

/*     CONIC */
/*     ELEMENTS */
/*     EPHEMERIS */

/* $ Declarations */

/*     Include file oscltx.inc */

/* $ Abstract */

/*     This file declares public parameters related to */
/*     the SPICELIB routine OSCLTX. */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 25-JAN-2017 (NJB) */

/* -& */

/*     Size of the OSCLTX output argument array ELTS. */

/*     The output array ELTS is intended to contain unused space to hold */
/*     additional elements that may be added in a later version of this */
/*     routine. In order to maintain forward compatibility, user */
/*     applications should declare ELTS as follows: */

/*     DOUBLE PRECISION   ELTS( OSCXSZ ) */


/*     End of include file oscltx.inc */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     STATE      I   State of body at epoch of elements. */
/*     ET         I   Epoch of elements. */
/*     MU         I   Gravitational parameter (GM) of primary body. */
/*     ELTS       O   Extended set of classical conic elements. */

/* $ Detailed_Input */

/*     STATE    is the state (position and velocity) of the body */
/*              at some epoch. Components are x, y, z, dx/dt, dy/dt, */
/*              dz/dt. STATE must be expressed relative to an */
/*              inertial reference frame. Units are km and km/sec. */


/*     ET       is the epoch of the input state, in ephemeris seconds */
/*              past J2000. */

/*                                                    3    2 */
/*     MU       is the gravitational parameter (GM, km /sec ) of */
/*              the primary body. */

/* $ Detailed_Output */

/*     ELTS     are equivalent conic elements describing the orbit */
/*              of the body around its primary. The elements are, */
/*              in order: */

/*                 RP      Perifocal distance. */
/*                 ECC     Eccentricity. */
/*                 INC     Inclination. */
/*                 LNODE   Longitude of the ascending node. */
/*                 ARGP    Argument of periapsis. */
/*                 M0      Mean anomaly at epoch. */
/*                 T0      Epoch. */
/*                 MU      Gravitational parameter. */
/*                 NU      True anomaly at epoch. */
/*                 A       Semi-major axis. A is set to zero if */
/*                         it is not computable. */
/*                 TAU     Orbital period. Applicable only for */
/*                         elliptical orbits. Set to zero otherwise. */

/*              The epoch of the elements is the epoch of the input */
/*              state. Units are km, rad, rad/sec. The same elements */
/*              are used to describe all three types (elliptic, */
/*              hyperbolic, and parabolic) of conic orbit. */

/*              User applications should declare ELTS using the */
/*              parameter */

/*                 OSCXSZ */

/*              See the $Parameters section below. */

/* $ Parameters */

/*     OSCXSZ   is the size of the output elements array ELTS. OSCXSZ */
/*              is declared in the Fortran include file */

/*                 oscltx.inc */

/*              The output array ELTS is intended to contain unused */
/*              space to hold additional elements that may be added */
/*              in a later version of this routine. In order to */
/*              maintain forward compatibility, user applications */
/*              should declare ELTS as follows: */

/*                 DOUBLE PRECISION   ELTS( OSCXSZ ) */

/* $ Exceptions */

/*     1)  If MU is not positive, the error SPICE(NONPOSITIVEMASS) */
/*         is signaled. */

/*     2)  If the specific angular momentum vector derived from STATE */
/*         is the zero vector, the error SPICE(DEGENERATECASE) */
/*         is signaled. */

/*     3)  If the position or velocity vectors derived from STATE */
/*         is the zero vector, the error SPICE(DEGENERATECASE) */
/*         is signaled. */

/*     4)  If the inclination is determined to be zero or 180 degrees, */
/*         the longitude of the ascending node is set to zero. */

/*     5)  If the eccentricity is determined to be zero, the argument of */
/*         periapse is set to zero. */

/*     6)  If the eccentricity of the orbit is very close to but not */
/*         equal to zero, the argument of periapse may not be accurately */
/*         determined. */

/*     7)  For inclinations near but not equal to 0 or 180 degrees, */
/*         the longitude of the ascending node may not be determined */
/*         accurately. The argument of periapse and mean anomaly may */
/*         also be inaccurate. */

/*     8)  For eccentricities very close to but not equal to 1, the */
/*         results of this routine are unreliable. */

/*     9)  If the specific angular momentum vector is non-zero but */
/*         "close" to zero, the results of this routine are unreliable. */

/*     10) If STATE is expressed relative to a non-inertial reference */
/*         frame, the resulting elements are invalid. No error checking */
/*         is done to detect this problem. */

/*     11) The semi-major axis and period may not be computable for */
/*         orbits having eccentricity too close to 1. If the semi-major */
/*         axis is not computable, both it and the period are set to */
/*         zero. If the period is not computable, it is set to zero. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine returns in the first 8 elements of the array ELTS */
/*     the outputs computed by OSCELT, and in addition returns in */
/*     elements 9-11 the quantities: */

/*        ELTS(9)   true anomaly at ET, in radians. */

/*        ELTS(10)  orbital semi-major axis at ET, in km. Valid */
/*                  if and only if this value is non-zero. */

/*                  The semi-major axis won't be computable if the */
/*                  eccentricity of the orbit is too close to 1. */
/*                  In this case A is set to zero. */

/*        ELTS(11)  orbital period. If the period is not computable, */
/*                  TAU is set to zero. */

/*     The SPICELIB routine CONICS is an approximate inverse of this */
/*     routine: CONICS maps a set of osculating elements and a time to a */
/*     state vector. */

/* $ Examples */

/*     The numerical results shown for these examples may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */

/*     1) Determine the osculating conic orbital elements of Phobos */
/*        with respect to Mars at some arbitrary time in the J2000 */
/*        inertial reference frame, including true anomaly, semi-major */
/*        axis and period. */

/*        Use the meta-kernel shown below to load the required SPICE */
/*        kernels. */


/*           KPL/MK */

/*           File name: oscltx_ex1.tm */

/*           This meta-kernel is intended to support operation of SPICE */
/*           example programs. The kernels shown here should not be */
/*           assumed to contain adequate or correct versions of data */
/*           required by SPICE-based user applications. */

/*           In order for an application to use this meta-kernel, the */
/*           kernels referenced here must be present in the user's */
/*           current working directory. */

/*           The names and contents of the kernels referenced */
/*           by this meta-kernel are as follows: */

/*              File name                     Contents */
/*              ---------                     -------- */
/*              mar097.bsp                    Mars satellite ephemeris */
/*              gm_de431.tpc                  Gravitational constants */
/*              naif0012.tls                  Leapseconds */


/*           \begindata */

/*              KERNELS_TO_LOAD = ( 'mar097.bsp', */
/*                                  'gm_de431.tpc', */
/*                                  'naif0012.tls'  ) */

/*           \begintext */

/*           End of meta-kernel */


/*        Example code begins here. */


/*              PROGRAM OSCLTX_EX1 */
/*              IMPLICIT NONE */

/*              INCLUDE 'oscltx.inc' */

/*        C */
/*        C     SPICELIB functions */
/*        C */
/*              DOUBLE PRECISION      DPR */

/*        C */
/*        C     Local variables. */
/*        C */
/*              DOUBLE PRECISION      ELTS   ( OSCXSZ ) */
/*              DOUBLE PRECISION      ET */
/*              DOUBLE PRECISION      LT */
/*              DOUBLE PRECISION      MU     ( 1 ) */
/*              DOUBLE PRECISION      STATE  ( 6 ) */
/*              DOUBLE PRECISION      STEP */

/*              INTEGER               DIM */

/*        C */
/*        C     Load the meta kernel listing the needed SPK, LSK and */
/*        C     PCK with gravitational parameters kernels. */
/*        C */
/*              CALL FURNSH ( 'oscltx_ex1.tm' ) */

/*        C */
/*        C     Convert the time string to ephemeris time */
/*        C */
/*              CALL STR2ET ( 'Dec 25, 2007', ET ) */

/*        C */
/*        C     Retrieve the state of Phobos with respect to Mars in */
/*        C     J2000. */
/*        C */
/*              CALL SPKEZR ( 'PHOBOS', ET, 'J2000', 'NONE', 'MARS', */
/*             .               STATE,   LT                          ) */

/*        C */
/*        C     Read the gravitational parameter for Mars. */
/*        C */
/*              CALL BODVRD ( 'MARS', 'GM', 1, DIM, MU ) */

/*        C */
/*        C     Convert the state 6-vector to the elts 8-vector. Note: */
/*        C     BODVRD returns data as arrays, so to access the */
/*        C     gravitational parameter (the only value in the array), */
/*        C     we use MU(1). */
/*        C */
/*              CALL OSCLTX ( STATE, ET, MU(1), ELTS ) */

/*        C */
/*        C     Output the elts vector. */
/*        C */
/*              WRITE(*,'(A,F21.10)') */
/*             .  'Perifocal distance          (km): ', ELTS(1) */
/*              WRITE(*,'(A,F21.10)') */
/*             .  'Eccentricity                    : ', ELTS(2) */
/*              WRITE(*,'(A,F21.10)') */
/*             .  'Inclination                (deg): ', ELTS(3) * DPR( ) */
/*              WRITE(*,'(A,F21.10)') */
/*             .  'Lon of ascending node      (deg): ', ELTS(4) * DPR( ) */
/*              WRITE(*,'(A,F21.10)') */
/*             .  'Argument of periapsis      (deg): ', ELTS(5) * DPR( ) */
/*              WRITE(*,'(A,F21.10)') */
/*             .  'Mean anomaly at epoch      (deg): ', ELTS(6) * DPR( ) */
/*              WRITE(*,'(A,F21.10)') */
/*             .  'Epoch                        (s): ', ELTS(7) */
/*              WRITE(*,'(A,F21.10)') */
/*             .  'Gravitational parameter (km3/s2): ', ELTS(8) */
/*              WRITE(*,'(A,F21.10)') */
/*             .  'True anomaly at epoch      (deg): ', ELTS(9) * DPR( ) */
/*              WRITE(*,'(A,F21.10)') */
/*             .  'Orbital semi-major axis     (km): ', ELTS(10) */
/*              WRITE(*,'(A,F21.10)') */
/*             .  'Orbital period               (s): ', ELTS(11) */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*        Perifocal distance          (km):       9232.5746716211 */
/*        Eccentricity                    :          0.0156113904 */
/*        Inclination                (deg):         38.1225231660 */
/*        Lon of ascending node      (deg):         47.0384055902 */
/*        Argument of periapsis      (deg):        214.1546430017 */
/*        Mean anomaly at epoch      (deg):        340.5048466068 */
/*        Epoch                        (s):  251812865.1837092042 */
/*        Gravitational parameter (km3/s2):      42828.3736206991 */
/*        True anomaly at epoch      (deg):        339.8966628076 */
/*        Orbital semi-major axis     (km):       9378.9938051492 */
/*        Orbital period               (s):      27577.0908930612 */


/*     2) Calculate the history of Phobos's orbital period at intervals */
/*        of six months for a time interval of 10 years. */

/*        Use the meta-kernel from the first example. */


/*        Example code begins here. */


/*              PROGRAM OSCLTX_EX2 */
/*              IMPLICIT NONE */

/*              INCLUDE 'oscltx.inc' */

/*        C */
/*        C     SPICELIB functions */
/*        C */
/*              DOUBLE PRECISION      DPR */
/*              DOUBLE PRECISION      SPD */

/*        C */
/*        C     Local parameters. */
/*        C */
/*              INTEGER               TIMLEN */
/*              PARAMETER           ( TIMLEN = 24 ) */

/*        C */
/*        C     Local variables. */
/*        C */
/*              CHARACTER*(TIMLEN)    UTCSTR */

/*              DOUBLE PRECISION      ELTS   ( OSCXSZ ) */
/*              DOUBLE PRECISION      ET */
/*              DOUBLE PRECISION      LT */
/*              DOUBLE PRECISION      MU     ( 1 ) */
/*              DOUBLE PRECISION      STATE  ( 6 ) */
/*              DOUBLE PRECISION      STEP */

/*              INTEGER               DIM */
/*              INTEGER               I */

/*        C */
/*        C     Load the meta kernel listing the needed SPK, LSK and */
/*        C     PCK with gravitational parameters kernels. */
/*        C */
/*              CALL FURNSH ( 'oscltx_ex1.tm' ) */

/*        C */
/*        C     Read the gravitational parameter for Mars. */
/*        C */
/*              CALL BODVRD ( 'MARS', 'GM', 1, DIM, MU ) */

/*        C */
/*        C     Convert the time string to ephemeris time */
/*        C */
/*              CALL STR2ET ( 'Jan 1, 2000 12:00:00', ET ) */

/*        C */
/*        C     A step of six months - in seconds. */
/*        C */
/*              STEP = 180.0D0 * SPD( ) */

/*        C */
/*        C     10 years in steps of six months starting */
/*        C     approximately Jan 1, 2000. */
/*        C */
/*              WRITE(*,'(A)') '        UCT Time             Period' */
/*              WRITE(*,'(A)') '------------------------  ------------' */

/*              DO I= 1, 20 */

/*        C */
/*        C        Retrieve the state; convert to osculating elements. */
/*        C */
/*                 CALL SPKEZR ( 'PHOBOS', ET, 'J2000', 'NONE', 'MARS', */
/*             .                  STATE,   LT                         ) */
/*                 CALL OSCLTX ( STATE, ET, MU(1), ELTS ) */

/*        C */
/*        C        Convert the ephemeris time to calendar UTC. */
/*        C */
/*                 CALL ET2UTC ( ET, 'C', 3, UTCSTR ) */

/*                 WRITE(*,'(A,2X,F12.6)') UTCSTR,  ELTS(11) */

/*                 ET = ET + STEP */

/*              END DO */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*                UCT Time             Period */
/*        ------------------------  ------------ */
/*        2000 JAN 01 12:00:00.000  27575.419249 */
/*        2000 JUN 29 12:00:00.000  27575.124052 */
/*        2000 DEC 26 12:00:00.000  27574.987749 */
/*        2001 JUN 24 12:00:00.000  27574.273163 */
/*        2001 DEC 21 12:00:00.000  27573.096137 */
/*        2002 JUN 19 11:59:59.999  27572.262064 */
/*        2002 DEC 16 12:00:00.000  27572.336386 */
/*        2003 JUN 14 11:59:59.999  27572.576986 */
/*        2003 DEC 11 12:00:00.001  27572.441912 */
/*        2004 JUN 08 11:59:59.999  27572.338535 */
/*        2004 DEC 05 12:00:00.001  27572.964737 */
/*        2005 JUN 03 11:59:59.999  27574.450440 */
/*        2005 NOV 30 12:00:00.001  27575.627595 */
/*        2006 MAY 29 11:59:58.999  27576.174100 */
/*        2006 NOV 25 11:59:59.001  27576.702123 */
/*        2007 MAY 24 11:59:58.999  27577.625008 */
/*        2007 NOV 20 11:59:59.001  27578.959155 */
/*        2008 MAY 18 11:59:58.999  27579.545076 */
/*        2008 NOV 14 11:59:59.001  27578.920610 */
/*        2009 MAY 13 11:59:57.999  27577.800624 */


/* $ Restrictions */

/*     1)  The input state vector must be expressed relative to an */
/*         inertial reference frame. */

/*     2)  Osculating elements are generally not useful for */
/*         high-accuracy work. */

/*     3)  Accurate osculating elements may be difficult to derive for */
/*         near-circular or near-equatorial orbits. Osculating elements */
/*         for such orbits should be used with caution. */

/*     4)  Extracting osculating elements from a state vector is a */
/*         mathematically simple but numerically challenging task. The */
/*         mapping from a state vector to equivalent elements is */
/*         undefined for certain state vectors, and the mapping is */
/*         difficult to implement with finite precision arithmetic for */
/*         states near the subsets of R6 where singularities occur. */

/*         In general, the elements found by this routine can have */
/*         two kinds of problems: */

/*         -  The elements are not accurate but still represent */
/*            the input state accurately. The can happen in */
/*            cases where the inclination is near zero or 180 */
/*            degrees, or for near-circular orbits. */

/*         -  The elements are garbage. This can occur when */
/*            the eccentricity of the orbit is close to but */
/*            not equal to 1. In general, any inputs that cause */
/*            great loss of precision in the computation of the */
/*            specific angular momentum vector or the eccentricity */
/*            vector will result in invalid outputs. */

/*         For further details, see the $Exceptions section. */

/*         Users of this routine should carefully consider whether */
/*         it is suitable for their applications. One recommended */
/*         "sanity check" on the outputs is to supply them to the */
/*         SPICELIB routine CONICS and compare the resulting state */
/*         vector with the one supplied to this routine. */

/* $ Literature_References */

/*     [1]  R. Bate, D. Mueller, and J. White, "Fundamentals of */
/*          Astrodynamics," Dover Publications Inc., 1971. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */

/* $ Version */

/* -    SPICELIB Version 1.0.1, 06-JUL-2021 (JDR) */

/*        Edited the header to comply with NAIF standard. Added complete */
/*        code examples to $Examples section. */

/* -    SPICELIB Version 1.0.0, 02-FEB-2017 (NJB) */

/*        12-MAR-2015 (NJB) */

/*           Re-arranged test for small E to avoid overflow. */
/*           Changed definition of B to make the maximum value */
/*           of TAU equal to LIMIT. Removed test comparing */
/*           E/LIMIT to RMAG. */

/*        11-NOV-2014 (NJB) */

/*           Original version. Based on OSCELT version 1.3.1, */
/*           28-FEB-2008 */

/* -& */
/* $ Index_Entries */

/*     extended conic elements from state */
/*     extended osculating elements from state */
/*     convert state to extended osculating elements */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     Local variables */

    if (return_()) {
	return 0;
    }
    chkin_("OSCLTX", (ftnlen)6);
    if (pass1) {
	limit = dpmax_() / 200.;
	pass1 = FALSE_;
    }

/*     Initialize the members of ELTS that won't be set by OSCELT. This */
/*     is necessary because this routine may return early if A or TAU */
/*     cannot be computed. */

    cleard_(&c__12, &elts[8]);
    nu = 0.;
    tau = 0.;
    a = 0.;

/*     Compute osculating elements using OSCELT. Take advantage */
/*     of OSCELT's error handling. */

    oscelt_(state, et, mu, elts);
    if (failed_()) {
	chkout_("OSCLTX", (ftnlen)6);
	return 0;
    }

/*     Due to checks made by OSCELT, we know that MU and RMAG */
/*     are strictly positive. */


/*     Compute the true anomaly at ET. For the non-degenerate case, we */
/*     use equation 2.4-5 from [1] to compute the eccentricity vector: */

/*        _    1          2   mu    _     _  _  _ */
/*        e = ---- * [ ( v - ---- ) r  - <r, v> v ]                   (1) */
/*             mu             r */


/*     where */

/*        _ */
/*        e      is the eccentricity vector */

/*        _  _ */
/*        r, v   are, respectively, the object's position and */
/*               velocity vectors expressed in the inertial */
/*               reference frame of the input state. */
/*                                                    _     _ */
/*        r, v   are, respectively, the magnitudes of r and v */

/*        mu     is the GM value of the center of motion */



    unorm_(state, rhat, &rmag);
    vequ_(&state[3], vel);
    vmag = vnorm_(vel);
    ecc = exact_(&elts[1], &c_b5, &c_b6);
    if (ecc != 0.) {
/*                                 _   _ */
/*        C1 is the coefficient of r/||r|| in (1) above: */

/* Computing 2nd power */
	d__1 = vmag;
	c1 = 1. / *mu * (rmag * (d__1 * d__1) - *mu);
/*                                 _ */
/*        C2 is the coefficient of v in (1) above: */

	c2 = -(1. / *mu) * vdot_(state, vel);

/*        ECCVEC is the eccentricity vector: */

	vlcom_(&c1, rhat, &c2, vel, eccvec);

/*        POLE is the orbital pole unit vector. */

	ucrss_(state, vel, pole);

/*        Compute the transformation from the frame of the input state */
/*        to the perifocal frame. */

	twovec_(eccvec, &c__1, pole, &c__3, xmat);
	if (failed_()) {
	    chkout_("OSCLTX", (ftnlen)6);
	    return 0;
	}

/*        Transform the input position to the perifocal frame and */
/*        compute its "right ascension." This is the true anomaly NU, */
/*        confined to the range [0, 2*pi) radians. */

	mxv_(xmat, state, rperi);
	recrad_(rperi, &range, &nu, &dec);
    } else {

/*        The orbit is circular or nearly so. The mean anomaly can */
/*        be used as the true anomaly. */

	nu = elts[5];
    }
    elts[8] = nu;

/*     The semi-major axis A satisfies the relationship */

/*                mu */
/*        E =  - ----                                                 (2) */
/*                2A */

/*     where E represents the specific energy of the orbit, as long */
/*     as A is computable. */

/*     If the orbit is not parabolic or too close to parabolic, */
/*     we can recover A from (2) above: */

/*                mu */
/*        A =  - ---- */
/*                2E */

/*     To make sure that A is computable, we require */



/*        |mu/E| < LIMIT */

/*     so */

/*        |mu/LIMIT| < |E| */

/*     We compute E from the specific kinetic and potential */
/*     energy of the orbit: */

/*               2 */
/*              v     mu */
/*        E =  --- - ----                                             (3) */
/*              2     r */

/*     The second term on the right hand side is computable if */

/*        mu/LIMIT < r */


    if (elts[1] == 1.) {

/*        This is the parabolic case. */

	chkout_("OSCLTX", (ftnlen)6);
	return 0;
    }
    if (rmag <= *mu / limit) {

/*        We need RMAG > MU/LIMIT to make E computable. */

/*        We can't safely compute E. */

	chkout_("OSCLTX", (ftnlen)6);
	return 0;
    }

/*     We assume V and MU are small enough not to cause overflow. */

/* Computing 2nd power */
    d__1 = vmag;
    e = d__1 * d__1 * .5 - *mu / rmag;
    if (abs(e) < abs(*mu) / limit) {

/*        We can't safely compute A. The case of E equal to 0 */
/*        gets the code to this point. */

	chkout_("OSCLTX", (ftnlen)6);
	return 0;
    }

/*     At this point we can compute A (which is stored in ELTS(10)). */

    a = -(*mu) / (e * 2);
    elts[9] = a;

/*     If the orbit is elliptical, compute the period. */

    ecc = elts[1];
    if (ecc < 1.) {

/*        The period is given by the formula */

/*                             3       1/2 */
/*           tau = 2 * pi * ( a  / mu ) */

/*        We need to make sure this computation doesn't */
/*        overflow. */

/*        If mu >= 1 then we can express the constraint */
/*        on a as */

/*                   1/3                         2/3 */
/*           ( a / mu    )  <  ( LIMIT / (2*pi) ) */


/*        Otherwise, we require that */

/*                                 2/3     1/3 */
/*           a < ( LIMIT / (2*pi) )    * mu */



/*        Note that the case of non-positive MU has already */
/*        been ruled out. */

	d__1 = limit / twopi_();
	b = pow_dd(&d__1, &c_b13);
	mucubr = pow_dd(mu, &c_b14);
	if (*mu >= 1.) {
	    cmptau = a / mucubr < b;
	} else {
	    cmptau = a < b * mucubr;
	}
	if (cmptau) {
	    d__1 = a / mucubr;
	    tau = twopi_() * pow_dd(&d__1, &c_b15);
	}
    }

/*     Assign the remaining members of ELTS. */

    elts[10] = tau;
    chkout_("OSCLTX", (ftnlen)6);
    return 0;
} /* oscltx_ */

