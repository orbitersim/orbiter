/* xfmsta.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__0 = 0;
static integer c__6 = 6;
static doublereal c_b58 = 0.;

/* $Procedure XFMSTA ( Transform state between coordinate systems ) */
/* Subroutine */ int xfmsta_(doublereal *istate, char *icosys, char *ocosys, 
	char *body, doublereal *ostate, ftnlen icosys_len, ftnlen ocosys_len, 
	ftnlen body_len)
{
    /* Initialized data */

    static char cosys[40*6] = "RECTANGULAR                             " 
	    "CYLINDRICAL                             " "LATITUDINAL         "
	    "                    " "SPHERICAL                               " 
	    "GEODETIC                                " "PLANETOGRAPHIC      "
	    "                    ";
    static logical first = TRUE_;

    /* System generated locals */
    integer i__1, i__2;
    doublereal d__1, d__2;

    /* Builtin functions */
    double sqrt(doublereal);
    integer s_rnge(char *, integer, char *, integer);

    /* Local variables */
    extern /* Subroutine */ int zzbods2c_(integer *, char *, integer *, 
	    logical *, char *, integer *, logical *, ftnlen, ftnlen);
    doublereal ivel[3], ipos[3];
    extern /* Subroutine */ int vequ_(doublereal *, doublereal *);
    integer isys, osys;
    extern /* Subroutine */ int zzgftreb_(integer *, doublereal *);
    doublereal f;
    extern /* Subroutine */ int zzctruin_(integer *);
    integer i__, j;
    doublereal radii[3];
    extern /* Subroutine */ int chkin_(char *, ftnlen), errch_(char *, char *,
	     ftnlen, ftnlen), vpack_(doublereal *, doublereal *, doublereal *,
	     doublereal *);
    extern doublereal dpmax_(void);
    logical found;
    extern /* Subroutine */ int errdp_(char *, doublereal *, ftnlen), vequg_(
	    doublereal *, integer *, doublereal *);
    doublereal sqtmp;
    char isysu[40], osysu[40];
    static logical svfnd1;
    static integer svctr1[2];
    extern logical failed_(void);
    doublereal jacobi[9]	/* was [3][3] */;
    extern /* Subroutine */ int georec_(doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *), drdgeo_(
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *), recgeo_(doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *), dgeodr_(
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *);
    integer bodyid;
    extern integer isrchc_(char *, integer *, char *, ftnlen, ftnlen);
    static integer svbdid;
    extern /* Subroutine */ int latrec_(doublereal *, doublereal *, 
	    doublereal *, doublereal *), drdlat_(doublereal *, doublereal *, 
	    doublereal *, doublereal *), cylrec_(doublereal *, doublereal *, 
	    doublereal *, doublereal *), drdcyl_(doublereal *, doublereal *, 
	    doublereal *, doublereal *);
    doublereal toobig;
    extern /* Subroutine */ int sphrec_(doublereal *, doublereal *, 
	    doublereal *, doublereal *), drdsph_(doublereal *, doublereal *, 
	    doublereal *, doublereal *), pgrrec_(char *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, ftnlen), drdpgr_(char *, doublereal *, doublereal *,
	     doublereal *, doublereal *, doublereal *, doublereal *, ftnlen), 
	    reccyl_(doublereal *, doublereal *, doublereal *, doublereal *), 
	    reclat_(doublereal *, doublereal *, doublereal *, doublereal *), 
	    sigerr_(char *, ftnlen), recsph_(doublereal *, doublereal *, 
	    doublereal *, doublereal *), chkout_(char *, ftnlen), recpgr_(
	    char *, doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, ftnlen), dcyldr_(doublereal *, 
	    doublereal *, doublereal *, doublereal *), dlatdr_(doublereal *, 
	    doublereal *, doublereal *, doublereal *), ljucrs_(integer *, 
	    char *, char *, ftnlen, ftnlen), setmsg_(char *, ftnlen), dsphdr_(
	    doublereal *, doublereal *, doublereal *, doublereal *);
    static char svbody[36];
    extern /* Subroutine */ int dpgrdr_(char *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, ftnlen);
    extern logical return_(void);
    extern /* Subroutine */ int mxv_(doublereal *, doublereal *, doublereal *)
	    ;

/* $ Abstract */

/*     Transform a state between coordinate systems. */

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
/*     COORDINATE */
/*     EPHEMERIS */
/*     STATE */

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
/*     --------  ---  ------------------------------------------------- */
/*     ISTATE     I   Input state. */
/*     ICOSYS     I   Current (input) coordinate system. */
/*     OCOSYS     I   Desired (output) coordinate system. */
/*     BODY       I   Name or NAIF ID of body with which */
/*                    coordinates are associated (if applicable). */
/*     OSTATE     O   Converted output state. */

/* $ Detailed_Input */

/*     ISTATE   is a state vector in the input ICOSYS coordinate */
/*              system representing position and velocity. */

/*              All angular measurements must be in radians. */

/*              Note: body radii values taken from the kernel */
/*              pool are used when converting to or from geodetic or */
/*              planetographic coordinates. It is the user's */
/*              responsibility to verify the distance inputs are in */
/*              the same units as the radii in the kernel pool, */
/*              typically kilometers. */

/*     ICOSYS   is the name of the coordinate system that the input */
/*              state vector ISTATE is currently in. */

/*              ICOSYS may be any of the following: */

/*                 'RECTANGULAR' */
/*                 'CYLINDRICAL' */
/*                 'LATITUDINAL' */
/*                 'SPHERICAL' */
/*                 'GEODETIC' */
/*                 'PLANETOGRAPHIC' */

/*              Leading spaces, trailing spaces, and letter case */
/*              are ignored. For example, ' cyLindRical  ' would be */
/*              accepted. */

/*     OCOSYS   is the name of the coordinate system that the state */
/*              should be converted to. */

/*              Please see the description of ICOSYS for details. */

/*     BODY     is the name or NAIF ID of the body associated with the */
/*              planetographic or geodetic coordinate system. */

/*              If neither of the coordinate system choices are */
/*              geodetic or planetographic, BODY is ignored. It may */
/*              be a blank string. */

/*              Examples of accepted body names or IDs are: */

/*                 'Earth' */
/*                 '399' */

/*              Leading spaces, trailing spaces, and letter case are */
/*              ignored. */

/* $ Detailed_Output */

/*     OSTATE   is the state vector that has been converted to the */
/*              output coordinate system OCOSYS. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If either the input or output coordinate system is not */
/*         recognized, the error SPICE(COORDSYSNOTREC) is signaled. */

/*     2)  If the input body name cannot be converted to a NAIF ID */
/*         (applies to geodetic and planetographic coordinate */
/*         systems), the error SPICE(IDCODENOTFOUND) is signaled. */

/*     3)  If the input state ISTATE is not valid, meaning the position */
/*         but not the velocity is along the z-axis, the error */
/*         SPICE(INVALIDSTATE) is signaled. */

/*         Note: If both the input position and velocity are along */
/*         the z-axis and the output coordinate system is not */
/*         rectangular, the velocity can still be calculated even */
/*         though the Jacobian is undefined. This case will not */
/*         signal an error. An example of the input position and */
/*         velocity along the z-axis is below. */

/*                       Term    Value */
/*                       -----   ------ */
/*                         x       0 */
/*                         y       0 */
/*                         z       z */
/*                       dx/dt     0 */
/*                       dy/dt     0 */
/*                       dz/dt   dz_dt */

/*     4)  If either the input or output coordinate system is */
/*         geodetic or planetographic and at least one of the body's */
/*         radii is less than or equal to zero, the error */
/*         SPICE(INVALIDRADIUS) is signaled. */

/*     5)  If either the input or output coordinate system is */
/*         geodetic or planetographic and the difference of the */
/*         equatorial and polar radii divided by the equatorial radius */
/*         would produce numeric overflow, the error */
/*         SPICE(INVALIDRADIUS) is signaled. */

/*     6)  If the product of the Jacobian and velocity components */
/*         may lead to numeric overflow, the error */
/*         SPICE(NUMERICOVERFLOW) is signaled. */

/*     7)  If radii for BODY are not found in the kernel pool, an error */
/*         is signaled by a routine in the call tree of this routine. */

/*     8)  If the size of the BODY radii kernel variable is not three, */
/*         an error is signaled by a routine in the call tree of this */
/*         routine. */

/*     9)  If any of the three BODY radii is less-than or equal to zero, */
/*         an error is signaled by a routine in the call tree of this */
/*         routine. */

/*     10) If body's equatorial radii are not equal and either the */
/*         input or output coordinate system is geodetic or */
/*         planetographic, the error SPICE(NOTSUPPORTED) is signaled. */

/* $ Files */

/*     SPK, PCK, CK, and FK kernels may be required. */

/*     If the input or output coordinate systems are either geodetic or */
/*     planetographic, a PCK providing the radii of the body */
/*     name BODY must be loaded via FURNSH. */

/*     Kernel data are normally loaded once per program run, NOT every */
/*     time this routine is called. */

/* $ Particulars */

/*     Input Order */
/*     ------------------------------------------- */

/*     The input and output states will be structured by the */
/*     following descriptions. */

/*     For rectangular coordinates, the state vector is the following */
/*     in which X, Y, and Z are the rectangular position components and */
/*     DX, DY, and DZ are the time derivatives of each position */
/*     component. */

/*             ISTATE = ( X, Y, Z, DX, DY, DZ ) */

/*     For cylindrical coordinates, the state vector is the following */
/*     in which R is the radius, LONG is the longitudes, Z is the */
/*     height, and DR, DLONG, and DZ are the time derivatives of each */
/*     position component. */

/*             ISTATE = ( R, LONG, Z, DR, DLONG, DZ ) */

/*     For latitudinal coordinates, the state vector is the following */
/*     in which R is the radius, LONG is the longitude, LAT is the */
/*     latitude, and DR, DLONG, and DLAT are the time derivatives of */
/*     each position component. */

/*             ISTATE = ( R, LONG, LAT, DR, DLONG, DLAT ) */

/*     For spherical coordinates, the state vector is the following in */
/*     which R is the radius, COLAT is the colatitude, LONG is the */
/*     longitude, and DR, DCOLAT, and DLONG are the time derivatives of */
/*     each position component. */

/*             ISTATE = ( R, COLAT, LONG, DR, DCOLAT, DLONG ) */

/*     For geodetic coordinates, the state vector is the following in */
/*     which LONG is the longitude, LAT is the latitude, ALT is the */
/*     altitude, and DLONG, DLAT, and DALT are the time derivatives of */
/*     each position component. */

/*             ISTATE = ( LONG, LAT, ALT, DLONG, DLAT, DALT ) */

/*     For planetographic coordinates, the state vector is the */
/*     following in which LONG is the longitude, LAT is the latitude, */
/*     ALT is the altitude, and DLONG, DLAT, and DALT are the time */
/*     derivatives of each position component. */

/*             ISTATE = ( LONG, LAT, ALT, DLONG, DLAT, DALT ) */


/*     Input Boundaries */
/*     ------------------------------------------- */

/*     There are intervals the input angles must fall within if */
/*     the input coordinate system is not rectangular. These */
/*     intervals are provided below. */

/*        Input variable    Input meaning   Input interval [rad] */
/*        --------------    -------------   ------------------------ */
/*            LONG           Longitude        0     <= LONG  <  2*pi */
/*            LAT            Latitude        -pi/2  <= LAT   <= pi/2 */
/*            COLAT          Colatitude       0     <= COLAT <= pi */

/* $ Examples */

/*     The numerical results shown for these examples may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */

/*     1) Find the apparent state of Phoebe as seen by CASSINI in the */
/*        J2000 frame at 2004 Jun 11 19:32:00. Transform the state */
/*        from rectangular to latitudinal coordinates. For verification, */
/*        transform the state back from latitudinal to rectangular */
/*        coordinates. */

/*        Use the meta-kernel shown below to load the required SPICE */
/*        kernels. */


/*           KPL/MK */

/*           File name: xfmsta_ex1.tm */

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
/*              pck00010.tpc                     Planet orientation and */
/*                                               radii */
/*              naif0012.tls                     Leapseconds */
/*              041014R_SCPSE_01066_04199.bsp    CASSINI, planetary and */
/*                                               Saturn Satellite */
/*                                               ephemeris */


/*           \begindata */

/*              KERNELS_TO_LOAD = ( 'naif0012.tls', */
/*                                  '041014R_SCPSE_01066_04199.bsp', */
/*                                  'pck00010.tpc'                 ) */

/*           \begintext */

/*           End of meta-kernel */


/*        Example code begins here. */


/*              PROGRAM XFMSTA_EX1 */
/*              IMPLICIT NONE */
/*        C */
/*        C     Local parameters */
/*        C */
/*        C     METAKR is the meta-kernel's filename. */
/*        C */
/*              CHARACTER*(*)         METAKR */
/*              PARAMETER           ( METAKR = 'xfmsta_ex1.tm' ) */

/*              CHARACTER*(*)         FORM */
/*              PARAMETER           ( FORM = '(F16.6, F16.6, F16.6)' ) */

/*        C */
/*        C     Local variables */
/*        C */
/*        C     STAREC is the state of Phoebe with respect to CASSINI in */
/*        C     rectangular coordinates. STALAT is the state rotated into */
/*        C     latitudinal coordinates. STREC2 is the state transformed */
/*        C     back into rectangular coordinates from latitudinal. */
/*        C */
/*              DOUBLE PRECISION      STAREC (6) */
/*              DOUBLE PRECISION      STALAT (6) */
/*              DOUBLE PRECISION      STREC2 (6) */

/*        C */
/*        C     ET is the ephemeris time (TDB) corresponding to the */
/*        C     observation. */
/*        C */
/*              DOUBLE PRECISION      ET */
/*              DOUBLE PRECISION      LT */

/*              INTEGER               I */

/*        C */
/*        C     The required kernels must be loaded. */
/*        C */
/*              CALL FURNSH ( METAKR ) */

/*        C */
/*        C     Calculate the state at 2004 Jun 11 19:32:00 UTC. */
/*        C */
/*              CALL STR2ET ( '2004-JUN-11-19:32:00', ET ) */

/*        C */
/*        C     Calculate the apparent state of Phoebe as seen by */
/*        C     CASSINI in the J2000 frame. */
/*        C */
/*              CALL SPKEZR ( 'PHOEBE',  ET, 'IAU_PHOEBE', 'LT+S', */
/*             .              'CASSINI', STAREC, LT              ) */

/*        C */
/*        C     Transform the state from rectangular to latitudinal. */
/*        C     Notice that since neither the input nor output */
/*        C     coordinate frames are 'geodetic' or 'planetographic', */
/*        C     the input for the body name is a blank string. */
/*        C */
/*              CALL XFMSTA ( STAREC, 'RECTANGULAR', 'LATITUDINAL', ' ', */
/*             .              STALAT ) */

/*        C */
/*        C     Transform the state back to rectangular from latitudinal */
/*        C     for verification. This result should be very similar to */
/*        C     STAREC. */
/*        C */
/*              CALL XFMSTA ( STALAT, 'LATITUDINAL', 'RECTANGULAR',' ', */
/*             .              STREC2 ) */

/*        C */
/*        C     Report the results. */
/*        C */
/*              WRITE (*,*)    ' ' */
/*              WRITE (*,*)    'Phoebe as seen by CASSINI - rectangular' */
/*              WRITE (*,*)    '  Position [km]:' */
/*              WRITE (*,FORM) (STAREC(I), I = 1, 3) */
/*              WRITE (*,*)    '  Velocity [km/s]:' */
/*              WRITE (*,FORM) (STAREC(I), I = 4, 6) */
/*              WRITE (*,*)    ' ' */
/*              WRITE (*,*)    'Phoebe as seen by CASSINI - latitudinal' */
/*              WRITE (*,*)    '  Position [km, rad, rad]:' */
/*              WRITE (*,FORM) (STALAT(I), I = 1, 3) */
/*              WRITE (*,*)    '  Velocity [km/s, rad/s, rad/s]:' */
/*              WRITE (*,FORM) (STALAT(I), I = 4, 6) */
/*              WRITE (*,*)    ' ' */
/*              WRITE (*,*)    'Verification: ' */
/*              WRITE (*,*)    'Phoebe as seen by CASSINI - rectangular' */
/*              WRITE (*,*)    '  Position [km]:' */
/*              WRITE (*,FORM) (STREC2(I), I = 1, 3) */
/*              WRITE (*,*)    '  Velocity [km/s]:' */
/*              WRITE (*,FORM) (STREC2(I), I = 4, 6) */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*         Phoebe as seen by CASSINI - rectangular */
/*           Position [km]: */
/*            -2059.271283     -942.128329      -95.837672 */
/*           Velocity [km/s]: */
/*                3.910113       -4.228139       -1.526561 */

/*         Phoebe as seen by CASSINI - latitudinal */
/*           Position [km, rad, rad]: */
/*             2266.580876       -2.712515       -0.042296 */
/*           Velocity [km/s, rad/s, rad/s]: */
/*               -1.730462        0.002416       -0.000706 */

/*         Verification: */
/*         Phoebe as seen by CASSINI - rectangular */
/*           Position [km]: */
/*            -2059.271283     -942.128329      -95.837672 */
/*           Velocity [km/s]: */
/*                3.910113       -4.228139       -1.526561 */


/*     2) Transform a given state from cylindrical to planetographic */
/*        coordinates with respect to Earth. */

/*        Use the PCK kernel below to load the required triaxial */
/*        ellipsoidal shape model and orientation data for the Earth. */

/*           pck00010.tpc */


/*        Example code begins here. */


/*              PROGRAM XFMSTA_EX2 */
/*              IMPLICIT NONE */

/*        C */
/*        C     Local parameters */
/*        C */
/*              CHARACTER*(*)         FORM */
/*              PARAMETER           ( FORM = '(F16.6, F16.6, F16.6)' ) */

/*        C */
/*        C     Local variables */
/*        C */
/*        C     STACYL is the state in cylindrical coordinates. */
/*        C */
/*              DOUBLE PRECISION      STACYL (6) */
/*        C */
/*        C     STAPLN is the state transformed into planetographic */
/*        C     coordinates. */
/*        C */
/*              DOUBLE PRECISION      STAPLN (6) */
/*        C */
/*        C     STCYL2 is the state transformed back into */
/*        C     cylindrical coordinates from planetographic. */
/*        C */
/*              DOUBLE PRECISION      STCYL2 (6) */

/*              INTEGER               I */

/*              DATA STACYL / 1.0D0, 0.5D0, 0.5D0, 0.2D0, 0.1D0, -0.2D0 / */

/*        C */
/*        C     The required kernels must be loaded. */
/*        C */
/*              CALL FURNSH ( 'pck00010.tpc' ) */

/*        C */
/*        C     Transform the state from cylindrical to planetographic. */
/*        C     Note that since one of the coordinate systems is */
/*        C     planetographic, the body name must be input. */
/*        C */
/*              CALL XFMSTA ( STACYL, 'CYLINDRICAL', 'PLANETOGRAPHIC', */
/*             .              'EARTH', STAPLN ) */

/*        C */
/*        C     Transform the state back to cylindrical from */
/*        C     planetographic for verification. The result should be */
/*        C     very close to STACYL. */
/*        C */
/*              CALL XFMSTA ( STAPLN, 'PLANETOGRAPHIC', 'CYLINDRICAL', */
/*             .              'EARTH', STCYL2 ) */

/*        C */
/*        C     Report the results. */
/*        C */
/*              WRITE (*,*)    'Cylindrical state' */
/*              WRITE (*,*)    '  Position [km, rad, km]:' */
/*              WRITE (*,FORM) (STACYL(I), I = 1, 3) */
/*              WRITE (*,*)    '  Velocity [km/s, rad/s, km/s]:' */
/*              WRITE (*,FORM) (STACYL(I), I = 4, 6) */
/*              WRITE (*,*)    ' ' */
/*              WRITE (*,*) 'Planetographic state' */
/*              WRITE (*,*)    '  Position [rad, rad, km]:' */
/*              WRITE (*,FORM) (STAPLN(I), I = 1, 3) */
/*              WRITE (*,*)    '  Velocity [rad/s, rad/s, km/s]:' */
/*              WRITE (*,FORM) (STAPLN(I), I = 4, 6) */
/*              WRITE (*,*)    ' ' */
/*              WRITE (*,*)    'Verification:  Cylindrical state' */
/*              WRITE (*,*)    '  Position [km, rad, km]:' */
/*              WRITE (*,FORM) (STCYL2(I), I = 1, 3) */
/*              WRITE (*,*)    '  Velocity [km/s, rad/s, km/s]:' */
/*              WRITE (*,FORM) (STCYL2(I), I = 4, 6) */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*         Cylindrical state */
/*           Position [km, rad, km]: */
/*                1.000000        0.500000        0.500000 */
/*           Velocity [km/s, rad/s, km/s]: */
/*                0.200000        0.100000       -0.200000 */

/*         Planetographic state */
/*           Position [rad, rad, km]: */
/*                0.500000        1.547722    -6356.240364 */
/*           Velocity [rad/s, rad/s, km/s]: */
/*                0.100000       -0.004722       -0.195332 */

/*         Verification:  Cylindrical state */
/*           Position [km, rad, km]: */
/*                1.000000        0.500000        0.500000 */
/*           Velocity [km/s, rad/s, km/s]: */
/*                0.200000        0.100000       -0.200000 */


/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     S.C. Krening       (JPL) */
/*     B.V. Semenov       (JPL) */
/*     E.D. Wright        (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.2.0, 01-NOV-2021 (EDW) (JDR) */

/*        Body radii accessed from kernel pool using ZZGFTREB. */

/*        Edited he header to comply with NAIF standard. */
/*        Added missing begintext tag to the meta-kernel of example #1. */
/*        Modified example #2 to furnish a single kernel. */

/*        Updated Examples' kernels set to use PDS archived data. */

/* -    SPICELIB Version 1.1.0, 09-FEB-2017 (BVS) */

/*        BUG FIX: the routine no longer allows converting to and from */
/*        geodetic and planetographic coordinates for bodies with */
/*        unequal equatorial radii. Previously it arbitrarily picked the */
/*        first and the third radii to compute body's flattening */
/*        coefficient. */

/* -    SPICELIB Version 1.0.0, 22-APR-2014 (SCK) (BVS) */

/* -& */
/* $ Index_Entries */

/*     state transformation between coordinate systems */
/*     convert state */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */

/*     Potentially large numbers produced by transforming the */
/*     velocity using the Jacobian must not exceed DPMAX()/MARGIN: */


/*     The size of each coordinate system name must not exceed */
/*     CHSIZ characters. */


/*     NCOSYS is the number of coordinate systems supported by */
/*     this routine. */


/*     The following integer parameters represent the coordinate */
/*     systems supported by this routine. */


/*     Saved body name length. */


/*     Local variables */

/*     COSYS is the array of supported coordinate system names. */
/*     ISYSU and OSYSU are the input and output coordinate systems */
/*     from the user that are made insensitive to case or leading and */
/*     trailing spaces. */


/*     IPOS and IVEL are the input position and velocity translated */
/*     into rectangular. */


/*     For transformations including either geodetic or planetographic */
/*     coordinate systems, RADII is an array of the radii values */
/*     associated with the input body. These values will be loaded */
/*     from the kernel pool. */


/*     JACOBI is the Jacobian matrix that converts the velocity */
/*     coordinates between systems. */


/*     The flattening coefficient, F, is calculated when either */
/*     geodetic or planetographic coordinate systems are included */
/*     in the transformation. */


/*     SQTMP and TOOBIG are used to check for possible numeric */
/*     overflow situations. */


/*     BODYID is only used when the input or output coordinate */
/*     systems are geodetic or planetographic. The BODYID is the NAID ID */
/*     associated with the input body name. */


/*     ISYS and OSYS are the integer codes corresponding to the */
/*     input and output coordinate systems. I and J are iterators. */


/*     Saved name/ID item declarations. */


/*     Saved variables */


/*     Saved name/ID items. */


/*     Assign the names of the coordinate systems to a character */
/*     array in which each coordinate system name is located at */
/*     the index of the integer ID of the coordinate system. */


/*     Initial values. */


/*     There are three main sections of this routine: */

/*       1)  Error handling and initialization. */
/*       2)  Conversion of the input to rectangular coordinates. */
/*       3)  Conversion from rectangular to the output coordinates. */

/*     Error handling and initialization */
/*     ---------------------------------------------------------------- */

/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }
    chkin_("XFMSTA", (ftnlen)6);

/*     Initialization. */

    if (first) {

/*        Initialize counter. */

	zzctruin_(svctr1);
	first = FALSE_;
    }

/*     Remove initial and trailing spaces. */
/*     Convert the input coordinate systems to upper case. */

    ljucrs_(&c__0, icosys, isysu, icosys_len, (ftnlen)40);
    ljucrs_(&c__0, ocosys, osysu, ocosys_len, (ftnlen)40);

/*     Check to see if the input and output coordinate systems */
/*     provided by the user are acceptable. Store the integer */
/*     code of the input and output coordinate systems into */
/*     ISYS and OSYS. */

    isys = isrchc_(isysu, &c__6, cosys, (ftnlen)40, (ftnlen)40);
    osys = isrchc_(osysu, &c__6, cosys, (ftnlen)40, (ftnlen)40);

/*     If the coordinate systems are not acceptable, an error is */
/*     signaled. */

    if (isys == 0 || osys == 0) {
	if (isys == 0 && osys == 0) {

/*           Both the input and the output coordinate systems were not */
/*           recognized. */

	    setmsg_("Input coordinate system # and output coordinate system "
		    "# are not recognized.", (ftnlen)76);
	    errch_("#", icosys, (ftnlen)1, icosys_len);
	    errch_("#", ocosys, (ftnlen)1, ocosys_len);
	    sigerr_("SPICE(COORDSYSNOTREC)", (ftnlen)21);
	    chkout_("XFMSTA", (ftnlen)6);
	    return 0;
	} else if (isys == 0) {

/*           The input coordinate system was not recognized. */

	    setmsg_("Input coordinate system # was not recognized", (ftnlen)
		    44);
	    errch_("#", icosys, (ftnlen)1, icosys_len);
	    sigerr_("SPICE(COORDSYSNOTREC)", (ftnlen)21);
	    chkout_("XFMSTA", (ftnlen)6);
	    return 0;
	} else {

/*           The output coordinate system was not recognized. */

	    setmsg_("Output coordinate system # was not recognized", (ftnlen)
		    45);
	    errch_("#", ocosys, (ftnlen)1, ocosys_len);
	    sigerr_("SPICE(COORDSYSNOTREC)", (ftnlen)21);
	    chkout_("XFMSTA", (ftnlen)6);
	    return 0;
	}
    }

/*     If the input and output coordinate systems are equal, set the */
/*     output equal to the input since no conversion needs to take */
/*     place. */

    if (isys == osys) {
	vequg_(istate, &c__6, ostate);
	chkout_("XFMSTA", (ftnlen)6);
	return 0;
    }

/*     If converting to or from either geodetic or planetographic, the */
/*     NAIF ID must be found from the input body name BODY. If the */
/*     body name does not have a valid NAIF ID code, an error is */
/*     signaled. If the NAIF ID is valid, the radii of the body are */
/*     located and the flattening coefficient is calculated. */

    if (osys == 5 || osys == 6 || isys == 5 || isys == 6) {

/*        Find the NAIF ID code */

	zzbods2c_(svctr1, svbody, &svbdid, &svfnd1, body, &bodyid, &found, (
		ftnlen)36, body_len);

/*        If the body's name was found, find the body's radii and */
/*        compute flattening coefficient. Otherwise, signal an error. */

	if (found) {
	    zzgftreb_(&bodyid, radii);
	    if (failed_()) {
		chkout_("XFMSTA", (ftnlen)6);
		return 0;
	    }

/*           If the difference of the equatorial and polar radii */
/*           divided by the equatorial radius is greater than DPMAX, */
/*           a numeric overflow may occur, so an error is signaled. */

	    if (sqrt((d__1 = radii[0] - radii[2], abs(d__1))) / sqrt((abs(
		    radii[0]))) >= sqrt(dpmax_())) {
		setmsg_("The equatorial radius for # has a value of # and a "
			"polar radius of #. The flattening coefficient cannot"
			" be calculated due to numeric overflow.", (ftnlen)142)
			;
		errch_("#", body, (ftnlen)1, body_len);
		errdp_("#", radii, (ftnlen)1);
		errdp_("#", &radii[2], (ftnlen)1);
		sigerr_("SPICE(INVALIDRADIUS)", (ftnlen)20);
		chkout_("XFMSTA", (ftnlen)6);
		return 0;
	    }

/*           At this point, we also check for unequal equatorial radii, */
/*           which are not allowed with geodetic or planetographic */
/*           coordinates. */

	    if (radii[0] != radii[1]) {
		setmsg_("The body # has radii (#, #, #). Unequal equatorial "
			"ellipsoid radii are not supported for # and # coordi"
			"nates.", (ftnlen)109);
		errch_("#", body, (ftnlen)1, body_len);
		errdp_("#", radii, (ftnlen)1);
		errdp_("#", &radii[1], (ftnlen)1);
		errdp_("#", &radii[2], (ftnlen)1);
		errch_("#", cosys + 160, (ftnlen)1, (ftnlen)40);
		errch_("#", cosys + 200, (ftnlen)1, (ftnlen)40);
		sigerr_("SPICE(NOTSUPPORTED)", (ftnlen)19);
		chkout_("XFMSTA", (ftnlen)6);
		return 0;
	    }

/*           Calculate the flattening coefficient, F. */

	    f = (radii[0] - radii[2]) / radii[0];
	} else {
	    setmsg_("The input body name # does not have a valid NAIF ID cod"
		    "e.", (ftnlen)57);
	    errch_("#", body, (ftnlen)1, body_len);
	    sigerr_("SPICE(IDCODENOTFOUND)", (ftnlen)21);
	    chkout_("XFMSTA", (ftnlen)6);
	    return 0;
	}
    }

/*     Conversion of the input to rectangular coordinates */
/*     ---------------------------------------------------------------- */

/*     First, the position and velocity coordinates will be converted */
/*     into rectangular coordinates. If the input system is not */
/*     rectangular, then the velocity coordinates must be translated */
/*     into rectangular using the Jacobian. If the input system is */
/*     rectangular, then the input state must simply be saved into IPOS */
/*     and IVEL. */

/*     TOOBIG is used for preventing numerical overflow. The square */
/*     roots of values are used to safely check if overflow will occur. */

    toobig = sqrt(dpmax_() / 100.);
    if (isys != 1) {

/*        To rectangular... */

	if (isys == 2) {

/*                  ... from cylindrical */

	    cylrec_(istate, &istate[1], &istate[2], ipos);
	    drdcyl_(istate, &istate[1], &istate[2], jacobi);
	} else if (isys == 3) {

/*                  ... from latitudinal */

	    latrec_(istate, &istate[1], &istate[2], ipos);
	    drdlat_(istate, &istate[1], &istate[2], jacobi);
	} else if (isys == 4) {

/*                  ... from spherical */

	    sphrec_(istate, &istate[1], &istate[2], ipos);
	    drdsph_(istate, &istate[1], &istate[2], jacobi);
	} else if (isys == 5) {

/*                  ... from geodetic */

	    georec_(istate, &istate[1], &istate[2], radii, &f, ipos);
	    if (failed_()) {
		chkout_("XFMSTA", (ftnlen)6);
		return 0;
	    }
	    drdgeo_(istate, &istate[1], &istate[2], radii, &f, jacobi);
	} else if (isys == 6) {

/*                  ... from planetographic */

	    pgrrec_(body, istate, &istate[1], &istate[2], radii, &f, ipos, 
		    body_len);
	    if (failed_()) {
		chkout_("XFMSTA", (ftnlen)6);
		return 0;
	    }
	    drdpgr_(body, istate, &istate[1], &istate[2], radii, &f, jacobi, 
		    body_len);
	} else {
	    setmsg_("This error should never occur. This is an intermediate "
		    "step in which a non-rectangular input state should be tr"
		    "ansferred to rectangular.  The input coordinate system i"
		    "s not recognized, yet was not caught by an earlier check."
		    , (ftnlen)224);
	    sigerr_("SPICE(BUG1)", (ftnlen)11);
	    chkout_("XFMSTA", (ftnlen)6);
	    return 0;
	}

/*        Some DRD* routines are not error free. Be safe and check */
/*        FAILED to not use un-initialized JACOBI. */

	if (failed_()) {
	    chkout_("XFMSTA", (ftnlen)6);
	    return 0;
	}

/*        If the multiplication of the Jacobian and velocity can cause */
/*        overflow, signal an error. */

	for (i__ = 1; i__ <= 3; ++i__) {
	    for (j = 1; j <= 3; ++j) {
		sqtmp = sqrt((d__1 = jacobi[(i__1 = i__ + j * 3 - 4) < 9 && 0 
			<= i__1 ? i__1 : s_rnge("jacobi", i__1, "xfmsta_", (
			ftnlen)1074)], abs(d__1))) * sqrt((d__2 = istate[(
			i__2 = j + 2) < 6 && 0 <= i__2 ? i__2 : s_rnge("ista"
			"te", i__2, "xfmsta_", (ftnlen)1074)], abs(d__2)));
		if (sqtmp > toobig) {
		    setmsg_("The product of the Jacobian and velocity may ca"
			    "use numeric overflow.", (ftnlen)68);
		    sigerr_("SPICE(NUMERICOVERFLOW)", (ftnlen)22);
		    chkout_("XFMSTA", (ftnlen)6);
		    return 0;
		}
	    }
	}

/*        Transform the velocity into rectangular coordinates. */

	mxv_(jacobi, &istate[3], ivel);
    } else if (isys == 1) {

/*        If the input coordinate system is rectangular, the input */
/*        position does not need to be translated into rectangular. */

	vequ_(istate, ipos);
	vequ_(&istate[3], ivel);
    } else {
	setmsg_("This error should never occur. This is an ELSE statement. I"
		"f the input coordinate system is not rectangular, the IF sho"
		"uld be executed. If the input coordinate system is rectangul"
		"ar, the ELSE IF should be executed.", (ftnlen)214);
	sigerr_("SPICE(BUG2)", (ftnlen)11);
	chkout_("XFMSTA", (ftnlen)6);
	return 0;
    }

/*     Conversion from rectangular into the output coordinates */
/*     ---------------------------------------------------------------- */

/*     Convert to the output coordinate system. If the output */
/*     coordinate system is not rectangular, four calculations must */
/*     be made: */

/*       1)  Verify the position and velocity are not along the z-axis. */
/*           If the position and velocity are along the z-axis, the */
/*           velocity can still be converted even though the */
/*           Jacobian is not defined. If the position is along the */
/*           z-axis but the velocity is not, the velocity cannot be */
/*           converted to the output coordinate system. */

/*       2)  Calculate the Jacobian from rectangular to the output */
/*           coordinate system and verify the product of the Jacobian */
/*           and velocity will not cause numeric overflow. */

/*       3)  Transform the position to the output coordinate system. */

/*       4)  Transform the velocity to the output coordinates using */
/*           the Jacobian and the rectangular velocity IVEL. */

    if (osys != 1) {

/*        From rectangular for the case when the input position is along */
/*        the z-axis ... */

	if (abs(ipos[0]) + abs(ipos[1]) == 0.) {
	    if (abs(ivel[0]) + abs(ivel[1]) == 0.) {

/*              If the velocity is along the z-axis, then the velocity */
/*              can be computed in the output coordinate frame even */
/*              though the Jacobian is not defined. */

		if (osys == 2) {

/*                  ... to cylindrical */

		    vpack_(&c_b58, &c_b58, &ivel[2], &ostate[3]);
		    reccyl_(ipos, ostate, &ostate[1], &ostate[2]);
		} else if (osys == 3) {

/*                  ... to latitudinal */

		    vpack_(&ivel[2], &c_b58, &c_b58, &ostate[3]);
		    reclat_(ipos, ostate, &ostate[1], &ostate[2]);
		} else if (osys == 4) {

/*                  ... to spherical */

		    vpack_(&ivel[2], &c_b58, &c_b58, &ostate[3]);
		    recsph_(ipos, ostate, &ostate[1], &ostate[2]);
		} else if (osys == 5) {

/*                  ... to geodetic */

		    vpack_(&c_b58, &c_b58, &ivel[2], &ostate[3]);
		    recgeo_(ipos, radii, &f, ostate, &ostate[1], &ostate[2]);
		} else if (osys == 6) {

/*                  ... to planetographic */

		    vpack_(&c_b58, &c_b58, &ivel[2], &ostate[3]);
		    recpgr_(body, ipos, radii, &f, ostate, &ostate[1], &
			    ostate[2], body_len);
		} else {
		    setmsg_("This error should never occur. This is an inter"
			    "mediate step in which a position and velocity al"
			    "ong the z-axis are converted to a non-rectangula"
			    "r coordinate system from rectangular. The output"
			    " coordinate system is not recognized, yet was no"
			    "t caught by an earlier check.", (ftnlen)268);
		    sigerr_("SPICE(BUG3)", (ftnlen)11);
		    chkout_("XFMSTA", (ftnlen)6);
		    return 0;
		}

/*              The output state has been calculated for the special */
/*              case of the position and velocity existing along the */
/*              z-axis. */

		chkout_("XFMSTA", (ftnlen)6);
		return 0;
	    } else {

/*              The Jacobian is undefined and the velocity cannot be */
/*              converted since it is not along the z-axis. */
/*              Signal an error. */

		setmsg_("Invalid input state: z axis.", (ftnlen)28);
		sigerr_("SPICE(INVALIDSTATE)", (ftnlen)19);
		chkout_("XFMSTA", (ftnlen)6);
		return 0;
	    }
	}

/*        From rectangular for cases when the input position is not along */
/*        the z-axis ... */

	if (osys == 2) {

/*                  ... to cylindrical */

	    dcyldr_(ipos, &ipos[1], &ipos[2], jacobi);
	    reccyl_(ipos, ostate, &ostate[1], &ostate[2]);
	} else if (osys == 3) {

/*                  ... to latitudinal */

	    dlatdr_(ipos, &ipos[1], &ipos[2], jacobi);
	    reclat_(ipos, ostate, &ostate[1], &ostate[2]);
	} else if (osys == 4) {

/*                  ... to spherical */

	    dsphdr_(ipos, &ipos[1], &ipos[2], jacobi);
	    recsph_(ipos, ostate, &ostate[1], &ostate[2]);
	} else if (osys == 5) {

/*                  ... to geodetic */

	    dgeodr_(ipos, &ipos[1], &ipos[2], radii, &f, jacobi);
	    recgeo_(ipos, radii, &f, ostate, &ostate[1], &ostate[2]);
	} else if (osys == 6) {

/*                  ... to planetographic */

	    dpgrdr_(body, ipos, &ipos[1], &ipos[2], radii, &f, jacobi, 
		    body_len);
	    recpgr_(body, ipos, radii, &f, ostate, &ostate[1], &ostate[2], 
		    body_len);
	} else {
	    setmsg_("This error should never occur. This is an intermediate "
		    "step in which a state is converted to a non-rectangular "
		    "coordinate system from rectangular. The output coordinat"
		    "e system is not recognized, yet was not caught by an ear"
		    "lier check.", (ftnlen)234);
	    sigerr_("SPICE(BUG4)", (ftnlen)11);
	    chkout_("XFMSTA", (ftnlen)6);
	    return 0;
	}

/*        Many D*DR and REC* routines are not error free. Be safe and */
/*        check FAILED to not use un-initialized JACOBI. */

	if (failed_()) {
	    chkout_("XFMSTA", (ftnlen)6);
	    return 0;
	}

/*        If the multiplication of the Jacobian and velocity can cause */
/*        overflow, signal an error. */

	for (i__ = 1; i__ <= 3; ++i__) {
	    for (j = 1; j <= 3; ++j) {
		sqtmp = sqrt((d__1 = jacobi[(i__1 = i__ + j * 3 - 4) < 9 && 0 
			<= i__1 ? i__1 : s_rnge("jacobi", i__1, "xfmsta_", (
			ftnlen)1334)], abs(d__1))) * sqrt((d__2 = ivel[(i__2 =
			 j - 1) < 3 && 0 <= i__2 ? i__2 : s_rnge("ivel", i__2,
			 "xfmsta_", (ftnlen)1334)], abs(d__2)));
		if (sqtmp > toobig) {
		    setmsg_("The product of the Jacobian and velocity may ca"
			    "use numeric overflow.", (ftnlen)68);
		    sigerr_("SPICE(NUMERICOVERFLOW)", (ftnlen)22);
		    chkout_("XFMSTA", (ftnlen)6);
		    return 0;
		}
	    }
	}

/*        Calculate the velocity in the output coordinate system. */

	mxv_(jacobi, ivel, &ostate[3]);
    } else if (osys == 1) {

/*        If the output coordinate system is rectangular, the position */
/*        and velocity components of the output state are set equal to */
/*        the rectangular IPOS and IVEL, respectively, because the */
/*        components have already been converted to rectangular. */

	vequ_(ipos, ostate);
	vequ_(ivel, &ostate[3]);
    } else {
	setmsg_("This error should never occur. This is an ELSE statement. I"
		"f the output coordinate system is not rectangular, the IF sh"
		"ould be executed. If the output coordinate system is rectang"
		"ular, the ELSE IF should be executed.", (ftnlen)216);
	sigerr_("SPICE(BUG5)", (ftnlen)11);
	chkout_("XFMSTA", (ftnlen)6);
	return 0;
    }
    chkout_("XFMSTA", (ftnlen)6);
    return 0;
} /* xfmsta_ */

