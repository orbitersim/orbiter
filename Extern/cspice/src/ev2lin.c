/* ev2lin.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static doublereal c_b91 = .66666666666666663;
static doublereal c_b92 = 3.5;
static doublereal c_b153 = 1.5;
static integer c__20 = 20;

/* $Procedure EV2LIN ( Evaluate "two-line" element data, near-earth ) */
/* Subroutine */ int ev2lin_(doublereal *et, doublereal *geophs, doublereal *
	elems, doublereal *state)
{
    /* Initialized data */

    static logical doinit = TRUE_;

    /* System generated locals */
    integer i__1;
    doublereal d__1;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer);
    double cos(doublereal), sin(doublereal), sqrt(doublereal), pow_dd(
	    doublereal *, doublereal *), d_mod(doublereal *, doublereal *), 
	    atan2(doublereal, doublereal);

    /* Local variables */
    static integer head;
    static doublereal coef, eeta, delm, aodp, delo, capu, xmdf, aynl, elsq, 
	    temp;
    static integer last;
    static doublereal rdot, cosu, tokm;
    static integer list[12]	/* was [2][6] */;
    static doublereal sinu, coef1, t2cof, t3cof, t4cof, t5cof, temp1, temp2, 
	    temp3, temp4, temp5, cos2u, temp6, mov1m, sin2u, a, e, f;
    static integer i__, j;
    static doublereal m;
    static integer n;
    static doublereal r__, s, u, betal, omega, betao;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    static doublereal epoch, ecose, aycof, delmo, esine, a3ovk2, tcube, cosik,
	     tempa, bstar, cosio, xincl, etasq, rfdot, sinik, a1, rdotk, c1, 
	    c2, c3, c4, c5, cosuk, d2, d3, j2, j3, j4, qomso, d4, lower;
    extern doublereal twopi_(void);
    static doublereal q1, q2, psisq, qoms24, s4, sinio, sinmo, sinuk, tempe, 
	    betao2, betao3, betao4, templ, tfour, upper, x1m5th, x1mth2, 
	    x3thm1, x7thm1, fmod2p, theta2, theta4, xinck, xlcof, xmcof, 
	    xmdot, xnode, xnodp;
    static integer count;
    static doublereal xndd6o;
    static integer after;
    static logical recog, unrec;
    static doublereal ae, xhdot1;
    extern /* Subroutine */ int errdp_(char *, doublereal *, ftnlen);
    static doublereal xndt2o, ke, ao, fl, eo, qoms2t, er, fu, pl, omgadf, rk, 
	    qo, uk, so, xl;
    static integer before;
    static doublereal xn, omegao, delomg;
    extern doublereal brcktd_(doublereal *, doublereal *, doublereal *);
    static doublereal omgcof, perige, ux, uy, uz, fprime, elemnt[60]	/* 
	    was [10][6] */, tsince, ae2, ae3, ae4, epsiln, xnodeo, cosnok, 
	    lstgeo[8], omgdot, ck2, cosepw, ck4, prelim[174]	/* was [29][6]
	     */, rfdotk, sinepw, sinnok, vx, tokmps, vy, pinvsq, vz, xnodcf, 
	    xnoddf, xnodek, epwnxt, xnodot;
    static logical newgeo;
    extern /* Subroutine */ int setmsg_(char *, ftnlen), errint_(char *, 
	    integer *, ftnlen), sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen);
    static doublereal eta, axn, ayn, epw, est, tsi, xll, xmo, xno, xmp, tsq, 
	    xlt, xmx, xmy, del1, c1sq, pix2;

/* $ Abstract */

/*     Deprecated: This routine has been superseded by the SPICELIB */
/*     routine EVSGP4. *ALL* TLE evaluations should use that routine. */
/*     NAIF supports EV2LIN only for backward compatibility. */

/*     Evaluate NORAD two-line element data for near-earth, earth */
/*     orbiting spacecraft (that is spacecraft with orbital periods */
/*     less-than 225 minutes). This evaluator uses algorithms as */
/*     described in Hoots 1980 [1]. */

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

/*     EPHEMERIS */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     ET         I   Epoch in seconds past ephemeris epoch J2000. */
/*     GEOPHS     I   Geophysical constants */
/*     ELEMS      I   Two-line element data */
/*     STATE      O   Evaluated state */
/*     NMODL      P   Parameter controlling number of buffered elements. */

/* $ Detailed_Input */

/*     ET       is the epoch in TDB seconds past ephemeris epoch J2000 */
/*              at which a state should be produced from the */
/*              input elements. */

/*     GEOPHS   is a collection of 8 geophysical constants needed */
/*              for computing a state. The order of these */
/*              constants must be: */

/*                 GEOPHS(1) = J2 gravitational harmonic for Earth. */
/*                 GEOPHS(2) = J3 gravitational harmonic for Earth. */
/*                 GEOPHS(3) = J4 gravitational harmonic for Earth. */

/*              These first three constants are dimensionless. */

/*                 GEOPHS(4) = KE: Square root of the GM for Earth where */
/*                             GM is expressed in Earth radii cubed per */
/*                             minutes squared. */

/*                 GEOPHS(5) = QO: High altitude bound for atmospheric */
/*                             model in km. */

/*                 GEOPHS(6) = SO: Low altitude bound for atmospheric */
/*                             model in km. */

/*                 GEOPHS(7) = RE: Equatorial radius of the earth in km. */

/*                 GEOPHS(8) = AE: Distance units/earth radius */
/*                             (normally 1) */

/*              Below are currently recommended values for these */
/*              items: */

/*                 J2 =    1.082616D-3 */
/*                 J3 =   -2.53881D-6 */
/*                 J4 =   -1.65597D-6 */

/*              The next item is the square root of GM for the Earth */
/*              given in units of earth-radii**1.5/Minute */

/*                 KE =    7.43669161D-2 */

/*              The next two items define the top and bottom of the */
/*              atmospheric drag model used by the type 10 ephemeris */
/*              type. Don't adjust these unless you understand the full */
/*              implications of such changes. */

/*                 QO =  120.0D0 */
/*                 SO =   78.0D0 */

/*              The ER value is the equatorial radius in km of the Earth */
/*              as used by NORAD. */

/*                 ER = 6378.135D0 */

/*              The value of AE is the number of distance units per */
/*              Earth radii used by the NORAD state propagation */
/*              software. The value should be 1 unless you've got a very */
/*              good understanding of the NORAD routine SGP4 and the */
/*              affect of changing this value. */

/*                 AE =    1.0D0 */

/*     ELEMS    is an array containing two-line element data */
/*              as prescribed below. The elements NDD6O and BSTAR */
/*              must already be scaled by the proper exponent stored */
/*              in the two line elements set. Moreover, the */
/*              various items must be converted to the units shown */
/*              here. */

/*                 ELEMS (  1 ) = NDT20 in radians/minute**2 */
/*                 ELEMS (  2 ) = NDD60 in radians/minute**3 */
/*                 ELEMS (  3 ) = BSTAR */
/*                 ELEMS (  4 ) = INCL  in radians */
/*                 ELEMS (  5 ) = NODE0 in radians */
/*                 ELEMS (  6 ) = ECC */
/*                 ELEMS (  7 ) = OMEGA in radians */
/*                 ELEMS (  8 ) = M0    in radians */
/*                 ELEMS (  9 ) = N0    in radians/minute */
/*                 ELEMS ( 10 ) = EPOCH of the elements in seconds */
/*                                past ephemeris epoch J2000. */

/* $ Detailed_Output */

/*     STATE    is the state produced by evaluating the input elements */
/*              at the input epoch ET. Units are km and km/sec relative */
/*              to the TEME reference frame. */

/* $ Parameters */

/*     NMODL    is a parameter that controls how many element sets */
/*              can be buffered internally. Since there are a lot */
/*              of computations that are independent of time these */
/*              are buffered and only computed if an unbuffered */
/*              model is supplied. This value should always */
/*              be at least 2. Increasing it a great deal is not */
/*              advised since the time needed to search the */
/*              buffered elements for a match increases linearly */
/*              with the NMODL. Empirically, 6 seems to be a good */
/*              break even value for NMODL. */

/* $ Exceptions */

/*     1)  No checks are made on the reasonableness of the inputs. */

/*     2)  If the number of iterations for the calculation exceeds the */
/*         limit for this value, the error SPICE(ITERATIONEXCEEDED) is */
/*         signaled. This error should signal only for bad (nonphysical) */
/*         TLEs. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine evaluates NORAD two-line element sets for */
/*     near-earth orbiting satellites using the algorithms described in */
/*     Hoots 1980 [1]. This code is an adaptation of */
/*     the NORAD routine SGP4 to eliminate common blocks, allow */
/*     buffering of models and intermediate parameters and double */
/*     precision calculations. */

/*     Near earth is as an orbital period of less than 225 */
/*     minutes. */

/* $ Examples */

/*     The numerical results shown for this example may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */

/*     1) Suppose you have a set of two-line elements for the LUME 1 */
/*        cubesat. This example shows how you can use this routine */
/*        together with the routine GETELM to propagate a state to an */
/*        epoch of interest. */

/*        Use the meta-kernel shown below to load the required SPICE */
/*        kernels. */


/*           KPL/MK */

/*           File name: ev2lin_ex1.tm */

/*           This meta-kernel is intended to support operation of SPICE */
/*           example programs. The kernels shown here should not be */
/*           assumed to contain adequate or correct versions of data */
/*           required by SPICE-based user applications. */

/*           In order for an application to use this meta-kernel, the */
/*           kernels referenced here must be present in the user's */
/*           current working directory. */

/*           The names and contents of the kernels referenced */
/*           by this meta-kernel are as follows: */

/*              File name           Contents */
/*              ---------           ------------------------------------ */
/*              naif0012.tls        Leapseconds */
/*              geophysical.ker     geophysical constants for evaluation */
/*                                  of two-line element sets. */

/*           \begindata */

/*              KERNELS_TO_LOAD = ( 'naif0012.tls', */
/*                                  'geophysical.ker'  ) */

/*           \begintext */

/*           End of meta-kernel */


/*        Example code begins here. */


/*              PROGRAM EV2LIN_EX1 */
/*              IMPLICIT NONE */

/*        C */
/*        C     Local parameters. */
/*        C */
/*              CHARACTER*(*)         TIMSTR */
/*              PARAMETER           ( TIMSTR = '2020-05-26 02:25:00' ) */

/*              INTEGER               PNAMLN */
/*              PARAMETER           ( PNAMLN = 2  ) */

/*              INTEGER               TLELLN */
/*              PARAMETER           ( TLELLN = 69 ) */

/*        C */
/*        C     The LUME-1 cubesat is an Earth orbiting object; set */
/*        C     the center ID to the Earth ID. */
/*        C */
/*              INTEGER               CENTER */
/*              PARAMETER           ( CENTER  = 399     ) */

/*        C */
/*        C     Local variables. */
/*        C */
/*              CHARACTER*(PNAMLN)    NOADPN ( 8  ) */
/*              CHARACTER*(TLELLN)    TLE    ( 2  ) */

/*              DOUBLE PRECISION      ELEMS  ( 10 ) */
/*              DOUBLE PRECISION      EPOCH */
/*              DOUBLE PRECISION      ET */
/*              DOUBLE PRECISION      GEOPHS ( 8  ) */
/*              DOUBLE PRECISION      STATE  ( 6  ) */

/*              INTEGER               I */
/*              INTEGER               N */

/*        C */
/*        C     These are the variables that will hold the constants */
/*        C     required by EV2LIN. These constants are available from */
/*        C     the loaded PCK file, which provides the actual values */
/*        C     and units as used by NORAD propagation model. */
/*        C */
/*        C        Constant   Meaning */
/*        C        --------   ------------------------------------------ */
/*        C        J2         J2 gravitational harmonic for Earth. */
/*        C        J3         J3 gravitational harmonic for Earth. */
/*        C        J4         J4 gravitational harmonic for Earth. */
/*        C        KE         Square root of the GM for Earth. */
/*        C        QO         High altitude bound for atmospheric model. */
/*        C        SO         Low altitude bound for atmospheric model. */
/*        C        ER         Equatorial radius of the Earth. */
/*        C        AE         Distance units/earth radius. */
/*        C */
/*              DATA          NOADPN  /  'J2', 'J3', 'J4', 'KE', */
/*             .                         'QO', 'SO', 'ER', 'AE'  / */

/*        C */
/*        C     Define the Two-Line Element set for LUME-1. */
/*        C */
/*              TLE(1)  = '1 43908U 18111AJ  20146.60805006  .00000806' */
/*             .      //                   '  00000-0  34965-4 0  9999' */
/*              TLE(2)  = '2 43908  97.2676  47.2136 0020001 220.6050 ' */
/*             .      //                   '139.3698 15.24999521 78544' */

/*        C */
/*        C     Load the PCK file that provides the geophysical */
/*        C     constants required for the evaluation of the two-line */
/*        C     elements sets. Load also an LSK, as it is required by */
/*        C     GETELM to perform time conversions. Use a meta-kernel for */
/*        C     convenience. */
/*        C */
/*              CALL FURNSH ( 'ev2lin_ex1.tm' ) */

/*        C */
/*        C     Retrieve the data from the kernel, and place it on */
/*        C     the GEOPHS array. */
/*        C */
/*              DO I = 1, 8 */

/*                 CALL BODVCD ( CENTER, NOADPN(I), 1, N, GEOPHS(I) ) */

/*              END DO */

/*        C */
/*        C     Convert the Two Line Elements lines to the element sets. */
/*        C     Set the lower bound for the years to be the beginning */
/*        C     of the space age. */
/*        C */
/*              CALL GETELM ( 1957, TLE, EPOCH, ELEMS ) */

/*        C */
/*        C     Now propagate the state using EV2LIN to the epoch of */
/*        C     interest. */
/*        C */
/*              CALL STR2ET ( TIMSTR, ET ) */
/*              CALL EV2LIN ( ET, GEOPHS, ELEMS, STATE ) */

/*        C */
/*        C     Display the results. */
/*        C */
/*              WRITE(*,'(2A)')       'Epoch   : ', TIMSTR */
/*              WRITE(*,'(A,3F16.8)') 'Position:', (STATE(I), I=1,3) */
/*              WRITE(*,'(A,3F16.8)') 'Velocity:', (STATE(I), I=4,6) */


/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*        Epoch   : 2020-05-26 02:25:00 */
/*        Position:  -4644.60403398  -5038.95025540   -337.27141117 */
/*        Velocity:     -0.45719025      0.92884817     -7.55917355 */


/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     [1]  F. Hoots and R. Roehrich, "Spacetrack Report #3: Models for */
/*          Propagation of the NORAD Element Sets," U.S. Air Force */
/*          Aerospace Defense Command, Colorado Springs, CO, 1980. */

/*     [2]  F. Hoots, "Spacetrack Report #6: Models for Propagation of */
/*          Space Command Element Sets,"  U.S. Air Force Aerospace */
/*          Defense Command, Colorado Springs, CO, 1986. */

/*     [3]  F. Hoots, P. Schumacher and R. Glover, "History of Analytical */
/*          Orbit Modeling in the U. S. Space Surveillance System," */
/*          Journal of Guidance, Control, and Dynamics. 27(2):174-185, */
/*          2004. */

/*     [4]  D. Vallado, P. Crawford, R. Hujsak and T. Kelso, "Revisiting */
/*          Spacetrack Report #3," paper AIAA 2006-6753 presented at the */
/*          AIAA/AAS Astrodynamics Specialist Conference, Keystone, CO., */
/*          August 21-24, 2006. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     W.L. Taber         (JPL) */
/*     E.D. Wright        (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.1, 01-NOV-2021 (JDR) (EDW) */

/*        Declared routine as deprecated. */

/*        Corrected the description of QO and SO constants in the */
/*        detailed description of the input argument GEOPHS and the input */
/*        element names in ELEMS. */

/*        Added Spacetrack Report #3 to literature references. */

/*        Edited the header to comply with NAIF standard. Added complete */
/*        code example based on existing fragments. */

/* -    SPICELIB Version 1.1.0, 15-SEP-2014 (EDW) */

/*        Added error check to prevent infinite loop in */
/*        calculation of EST. */

/* -    SPICELIB Version 1.0.3, 02-JAN-2008 (EDW) */

/*        Corrected error in the calculation of the C4 term */
/*        identified by Curtis Haase. */

/*        Minor edit to the COEF1 declaration strictly */
/*        identifying the constant as a double. */

/*        From: */

/*           COEF1  = COEF  / PSISQ**3.5 */

/*        To: */

/*           COEF1  = COEF  / PSISQ**3.5D0 */

/* -    SPICELIB Version 1.0.2, 08-JUL-2004 (EDW) */

/*        Corrected error in the calculation of the C2 term. */
/*        Reordered C1, C2 calculations to avoid division */
/*        by BSTAR. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1998 (EDW) */

/*        Corrected error in header describing the GEOPHS array. */

/* -    SPICELIB Version 1.0.0, 14-JAN-1994 (WLT) */

/* -& */
/* $ Index_Entries */

/*     DEPRECATED Evaluate NORAD two-line element data. */

/* -& */

/*     SPICELIB functions */


/*     Local Parameters */


/*     The following parameters give the location of the various */
/*     geophysical parameters needed for the two line element */
/*     sets. */

/*     KJ2  --- location of J2 */
/*     KJ3  --- location of J3 */
/*     KJ4  --- location if J4 */
/*     KKE  --- location of KE = sqrt(GM) in earth-radii**1.5/MIN */
/*     KQO  --- upper bound of atmospheric model in KM */
/*     KSO  --- lower bound of atmospheric model in KM */
/*     KER  --- earth equatorial radius in KM. */
/*     KAE  --- distance units/earth radius */



/*     An enumeration of the various components of the */
/*     elements array---ELEMS */

/*     KNDT20 */
/*     KNDD60 */
/*     KBSTAR */
/*     KINCL */
/*     KNODE0 */
/*     KECC */
/*     KOMEGA */
/*     KMO */
/*     KNO */


/*     The parameters NEXT and PREV are used in our linked list */
/*     LIST(NEXT,I) points to the list item the occurs after */
/*     list item I.  LIST ( PREV, I ) points to the list item */
/*     that precedes list item I. */
/*     NEXT */
/*     PREV */


/*     There are a number of preliminary quantities that are needed */
/*     to compute the state.  Those that are not time dependent and */
/*     depend only upon the elements are stored in a buffer so that */
/*     if an element set matches a saved set, these preliminary */
/*     quantities  will not be recomputed.  PRSIZE is the parameter */
/*     used to declare the needed room. */


/*     When we perform bisection in the solution of Kepler's equation */
/*     we don't want to bisect too many times. */


/*     Numerical Constants */


/*     Local variables */

/*     Geophysical Quantities */


/*     Elements */


/*     Intermediate quantities. The time independent quantities */
/*     are calculated only as new elements come into the routine. */

    chkin_("EV2LIN", (ftnlen)6);

/*     Rather than always making function calls we store the */
/*     values of the PI dependent constants the first time */
/*     through the routine. */

    if (doinit) {
	doinit = FALSE_;
	pix2 = twopi_();
	for (i__ = 1; i__ <= 8; ++i__) {
	    lstgeo[(i__1 = i__ - 1) < 8 && 0 <= i__1 ? i__1 : s_rnge("lstgeo",
		     i__1, "ev2lin_", (ftnlen)807)] = 0.;
	}
	for (i__ = 1; i__ <= 6; ++i__) {
	    for (j = 1; j <= 10; ++j) {
		elemnt[(i__1 = j + i__ * 10 - 11) < 60 && 0 <= i__1 ? i__1 : 
			s_rnge("elemnt", i__1, "ev2lin_", (ftnlen)812)] = 0.;
	    }
	}

/*        Set up our doubly linked list of most recently used */
/*        models.  Here's how things are supposed to be arranged: */

/*        LIST(NEXT,I)   points to the ephemeris model that was used */
/*                       most recently after ephemeris model I. */
/*        LIST(PREV,I)   points to the latest ephemeris model used */
/*                       that was used more recently than I. */

/*        HEAD           points to the most recently used ephemeris */
/*                       model. */


	head = 1;
	list[(i__1 = (head << 1) - 1) < 12 && 0 <= i__1 ? i__1 : s_rnge("list"
		, i__1, "ev2lin_", (ftnlen)831)] = 0;
	list[0] = 2;
	for (i__ = 2; i__ <= 5; ++i__) {
	    list[(i__1 = (i__ << 1) - 2) < 12 && 0 <= i__1 ? i__1 : s_rnge(
		    "list", i__1, "ev2lin_", (ftnlen)836)] = i__ + 1;
	    list[(i__1 = (i__ << 1) - 1) < 12 && 0 <= i__1 ? i__1 : s_rnge(
		    "list", i__1, "ev2lin_", (ftnlen)837)] = i__ - 1;
	}
	list[10] = 0;
	list[11] = 5;
    }

/*     We update the geophysical parameters only if there */
/*     has been a change from the last time they were */
/*     supplied. */

    if (lstgeo[7] != geophs[7] || lstgeo[6] != geophs[6] || lstgeo[0] != 
	    geophs[0] || lstgeo[1] != geophs[1] || lstgeo[2] != geophs[2] || 
	    lstgeo[3] != geophs[3] || lstgeo[4] != geophs[4] || lstgeo[5] != 
	    geophs[5]) {
	for (i__ = 1; i__ <= 8; ++i__) {
	    lstgeo[(i__1 = i__ - 1) < 8 && 0 <= i__1 ? i__1 : s_rnge("lstgeo",
		     i__1, "ev2lin_", (ftnlen)859)] = geophs[i__ - 1];
	}
	j2 = geophs[0];
	j3 = geophs[1];
	j4 = geophs[2];
	ke = geophs[3];
	qo = geophs[4];
	so = geophs[5];
	er = geophs[6];
	ae = geophs[7];
	ae2 = ae * ae;
	ae3 = ae * ae2;
	ae4 = ae * ae3;
	ck2 = j2 * .5 * ae2;
	a3ovk2 = j3 * -2. * ae / j2;
	ck4 = j4 * -.375 * ae4;
	qomso = qo - so;
	q1 = qomso * ae / er;
	q2 = q1 * q1;
	qoms2t = q2 * q2;
	s = ae * (so / er + 1.);

/*        When we've finished up we will need to convert everything */
/*        back to KM and KM/SEC  the two variables below give the */
/*        factors we shall need to do this. */

	tokm = er / ae;
	tokmps = tokm / 60.;
	newgeo = TRUE_;
    } else {
	newgeo = FALSE_;
    }

/*     Fetch all of the pieces of this model. */

    epoch = elems[9];
    xndt2o = elems[0];
    xndd6o = elems[1];
    bstar = elems[2];
    xincl = elems[3];
    xnodeo = elems[4];
    eo = elems[5];
    omegao = elems[6];
    xmo = elems[7];
    xno = elems[8];

/*     See if this model is already buffered, start at the first */
/*     model in the list (the most recently used model). */

    unrec = TRUE_;
    n = head;
    while(n != 0 && unrec) {

/*        The actual order of the elements is such that we can */
/*        usually tell that a stored model is different from */
/*        the one under consideration by looking at the */
/*        end of the list first.  Hence we start with I = NELEMS */
/*        and decrement I until we have looked at everything */
/*        or found a mismatch. */

	recog = TRUE_;
	i__ = 10;
	while(recog && i__ > 0) {
	    recog = recog && elemnt[(i__1 = i__ + n * 10 - 11) < 60 && 0 <= 
		    i__1 ? i__1 : s_rnge("elemnt", i__1, "ev2lin_", (ftnlen)
		    935)] == elems[i__ - 1];
	    --i__;
	}
	unrec = ! recog;
	if (unrec) {
	    last = n;
	    n = list[(i__1 = (n << 1) - 2) < 12 && 0 <= i__1 ? i__1 : s_rnge(
		    "list", i__1, "ev2lin_", (ftnlen)943)];
	}
    }
    if (n == 0) {
	n = last;
    }

/*     Either N points to a recognized item or it points to the */
/*     tail of the list where the least recently used items is */
/*     located.  In either case N must be made the head of the */
/*     list.  (If it is already the head of the list we don't */
/*     have to bother with anything.) */

    if (n != head) {

/*        Find the items that come before and after N and */
/*        link them together. */

	before = list[(i__1 = (n << 1) - 1) < 12 && 0 <= i__1 ? i__1 : s_rnge(
		"list", i__1, "ev2lin_", (ftnlen)964)];
	after = list[(i__1 = (n << 1) - 2) < 12 && 0 <= i__1 ? i__1 : s_rnge(
		"list", i__1, "ev2lin_", (ftnlen)965)];
	list[(i__1 = (before << 1) - 2) < 12 && 0 <= i__1 ? i__1 : s_rnge(
		"list", i__1, "ev2lin_", (ftnlen)967)] = after;
	if (after != 0) {
	    list[(i__1 = (after << 1) - 1) < 12 && 0 <= i__1 ? i__1 : s_rnge(
		    "list", i__1, "ev2lin_", (ftnlen)970)] = before;
	}

/*        Now the guy that will come after N is the current */
/*        head of the list.  N will have no predecessor. */

	list[(i__1 = (n << 1) - 2) < 12 && 0 <= i__1 ? i__1 : s_rnge("list", 
		i__1, "ev2lin_", (ftnlen)976)] = head;
	list[(i__1 = (n << 1) - 1) < 12 && 0 <= i__1 ? i__1 : s_rnge("list", 
		i__1, "ev2lin_", (ftnlen)977)] = 0;

/*        The predecessor the current head of the list becomes N */

	list[(i__1 = (head << 1) - 1) < 12 && 0 <= i__1 ? i__1 : s_rnge("list"
		, i__1, "ev2lin_", (ftnlen)981)] = n;

/*        and finally, N becomes the head of the list. */

	head = n;
    }
    if (recog && ! newgeo) {

/*        We can just look up the intermediate values from */
/*        computations performed on a previous call to this */
/*        routine. */

	aodp = prelim[(i__1 = n * 29 - 29) < 174 && 0 <= i__1 ? i__1 : s_rnge(
		"prelim", i__1, "ev2lin_", (ftnlen)996)];
	aycof = prelim[(i__1 = n * 29 - 28) < 174 && 0 <= i__1 ? i__1 : 
		s_rnge("prelim", i__1, "ev2lin_", (ftnlen)997)];
	c1 = prelim[(i__1 = n * 29 - 27) < 174 && 0 <= i__1 ? i__1 : s_rnge(
		"prelim", i__1, "ev2lin_", (ftnlen)998)];
	c4 = prelim[(i__1 = n * 29 - 26) < 174 && 0 <= i__1 ? i__1 : s_rnge(
		"prelim", i__1, "ev2lin_", (ftnlen)999)];
	c5 = prelim[(i__1 = n * 29 - 25) < 174 && 0 <= i__1 ? i__1 : s_rnge(
		"prelim", i__1, "ev2lin_", (ftnlen)1000)];
	cosio = prelim[(i__1 = n * 29 - 24) < 174 && 0 <= i__1 ? i__1 : 
		s_rnge("prelim", i__1, "ev2lin_", (ftnlen)1001)];
	d2 = prelim[(i__1 = n * 29 - 23) < 174 && 0 <= i__1 ? i__1 : s_rnge(
		"prelim", i__1, "ev2lin_", (ftnlen)1002)];
	d3 = prelim[(i__1 = n * 29 - 22) < 174 && 0 <= i__1 ? i__1 : s_rnge(
		"prelim", i__1, "ev2lin_", (ftnlen)1003)];
	d4 = prelim[(i__1 = n * 29 - 21) < 174 && 0 <= i__1 ? i__1 : s_rnge(
		"prelim", i__1, "ev2lin_", (ftnlen)1004)];
	delmo = prelim[(i__1 = n * 29 - 20) < 174 && 0 <= i__1 ? i__1 : 
		s_rnge("prelim", i__1, "ev2lin_", (ftnlen)1005)];
	eta = prelim[(i__1 = n * 29 - 19) < 174 && 0 <= i__1 ? i__1 : s_rnge(
		"prelim", i__1, "ev2lin_", (ftnlen)1006)];
	omgcof = prelim[(i__1 = n * 29 - 18) < 174 && 0 <= i__1 ? i__1 : 
		s_rnge("prelim", i__1, "ev2lin_", (ftnlen)1007)];
	omgdot = prelim[(i__1 = n * 29 - 17) < 174 && 0 <= i__1 ? i__1 : 
		s_rnge("prelim", i__1, "ev2lin_", (ftnlen)1008)];
	perige = prelim[(i__1 = n * 29 - 16) < 174 && 0 <= i__1 ? i__1 : 
		s_rnge("prelim", i__1, "ev2lin_", (ftnlen)1009)];
	sinio = prelim[(i__1 = n * 29 - 15) < 174 && 0 <= i__1 ? i__1 : 
		s_rnge("prelim", i__1, "ev2lin_", (ftnlen)1010)];
	sinmo = prelim[(i__1 = n * 29 - 14) < 174 && 0 <= i__1 ? i__1 : 
		s_rnge("prelim", i__1, "ev2lin_", (ftnlen)1011)];
	t2cof = prelim[(i__1 = n * 29 - 13) < 174 && 0 <= i__1 ? i__1 : 
		s_rnge("prelim", i__1, "ev2lin_", (ftnlen)1012)];
	t3cof = prelim[(i__1 = n * 29 - 12) < 174 && 0 <= i__1 ? i__1 : 
		s_rnge("prelim", i__1, "ev2lin_", (ftnlen)1013)];
	t4cof = prelim[(i__1 = n * 29 - 11) < 174 && 0 <= i__1 ? i__1 : 
		s_rnge("prelim", i__1, "ev2lin_", (ftnlen)1014)];
	t5cof = prelim[(i__1 = n * 29 - 10) < 174 && 0 <= i__1 ? i__1 : 
		s_rnge("prelim", i__1, "ev2lin_", (ftnlen)1015)];
	x1mth2 = prelim[(i__1 = n * 29 - 9) < 174 && 0 <= i__1 ? i__1 : 
		s_rnge("prelim", i__1, "ev2lin_", (ftnlen)1016)];
	x3thm1 = prelim[(i__1 = n * 29 - 8) < 174 && 0 <= i__1 ? i__1 : 
		s_rnge("prelim", i__1, "ev2lin_", (ftnlen)1017)];
	x7thm1 = prelim[(i__1 = n * 29 - 7) < 174 && 0 <= i__1 ? i__1 : 
		s_rnge("prelim", i__1, "ev2lin_", (ftnlen)1018)];
	xlcof = prelim[(i__1 = n * 29 - 6) < 174 && 0 <= i__1 ? i__1 : s_rnge(
		"prelim", i__1, "ev2lin_", (ftnlen)1019)];
	xmcof = prelim[(i__1 = n * 29 - 5) < 174 && 0 <= i__1 ? i__1 : s_rnge(
		"prelim", i__1, "ev2lin_", (ftnlen)1020)];
	xmdot = prelim[(i__1 = n * 29 - 4) < 174 && 0 <= i__1 ? i__1 : s_rnge(
		"prelim", i__1, "ev2lin_", (ftnlen)1021)];
	xnodcf = prelim[(i__1 = n * 29 - 3) < 174 && 0 <= i__1 ? i__1 : 
		s_rnge("prelim", i__1, "ev2lin_", (ftnlen)1022)];
	xnodot = prelim[(i__1 = n * 29 - 2) < 174 && 0 <= i__1 ? i__1 : 
		s_rnge("prelim", i__1, "ev2lin_", (ftnlen)1023)];
	xnodp = prelim[(i__1 = n * 29 - 1) < 174 && 0 <= i__1 ? i__1 : s_rnge(
		"prelim", i__1, "ev2lin_", (ftnlen)1024)];
    } else {

/*        Compute all of the intermediate items needed. */
/*        First, the inclination dependent constants. */

	cosio = cos(xincl);
	sinio = sin(xincl);
	theta2 = cosio * cosio;
	theta4 = theta2 * theta2;
	x3thm1 = theta2 * 3. - 1.;
	x7thm1 = theta2 * 7. - 1.;
	x1mth2 = 1. - theta2;
	x1m5th = 1. - theta2 * 5.;

/*        Eccentricity dependent constants */

	betao = sqrt(1. - eo * eo);
	betao2 = 1. - eo * eo;
	betao3 = betao * betao2;
	betao4 = betao2 * betao2;

/*        Semi-major axis and ascending node related constants. */

	d__1 = ke / xno;
	a1 = pow_dd(&d__1, &c_b91);
	del1 = ck2 * 1.5 * x3thm1 / (a1 * a1 * betao3);
	ao = a1 * (1. - del1 * (del1 * (del1 * 134. / 81. + 1.) + 
		.33333333333333331));
	delo = ck2 * 1.5 * x3thm1 / (ao * ao * betao3);
	xnodp = xno / (delo + 1.);
	aodp = ao / (1. - delo);
	s4 = s;
	qoms24 = qoms2t;
	perige = er * (aodp * (1. - eo) - ae);

/*        For perigee below 156 km, the values of S and QOMS2T are */
/*        altered. */

	if (perige < 156.) {
	    s4 = perige - 78.;
	    if (perige <= 98.) {
		s4 = 20.;
	    }
/* Computing 4th power */
	    d__1 = (120. - s4) * ae / er, d__1 *= d__1;
	    qoms24 = d__1 * d__1;
	    s4 = ae + s4 / er;
	}

/*        The next block is simply a pretty print of the code in */
/*        sgp4 from label number 10 through the label 90. */

	pinvsq = 1. / (aodp * aodp * betao4);
	tsi = 1. / (aodp - s4);
	eta = aodp * eo * tsi;
	etasq = eta * eta;
	eeta = eo * eta;
/* Computing 4th power */
	d__1 = tsi, d__1 *= d__1;
	coef = qoms24 * (d__1 * d__1);
	psisq = (d__1 = 1. - etasq, abs(d__1));
	coef1 = coef / pow_dd(&psisq, &c_b92);
	c2 = coef1 * xnodp * (aodp * (etasq * 1.5 + 1. + eeta * (etasq + 4.)) 
		+ ck2 * .75 * (tsi / psisq) * x3thm1 * (etasq * (etasq * 3. + 
		24.) + 8.));
	c1 = c2 * bstar;
	c3 = coef * tsi * a3ovk2 * xnodp * ae * sinio / eo;
	c4 = xnodp * 2. * coef1 * aodp * betao2 * (eta * (etasq * .5 + 2.) + 
		eo * (etasq * 2. + .5) - ck2 * tsi / (aodp * psisq) * 2. * (
		x3thm1 * -3. * (1. - eeta * 2. + etasq * (1.5 - eeta * .5)) + 
		cos(omegao * 2.) * .75 * x1mth2 * (etasq * 2. - eeta * (etasq 
		+ 1.))));
	c5 = coef1 * 2. * aodp * betao2 * ((etasq + eeta) * 2.75 + 1. + eeta *
		 etasq);
	temp1 = ck2 * 3. * pinvsq * xnodp;
	temp2 = temp1 * ck2 * pinvsq;
	temp3 = ck4 * 1.25 * pinvsq * pinvsq * xnodp;
	xmdot = xnodp + temp1 * .5 * betao * x3thm1 + temp2 * .0625 * betao * 
		(13. - theta2 * 78. + theta4 * 137.);
	omgdot = temp1 * -.5 * x1m5th + temp2 * .0625 * (7. - theta2 * 114. + 
		theta4 * 395.) + temp3 * (3. - theta2 * 36. + theta4 * 49.);
	xhdot1 = -temp1 * cosio;
	xnodot = xhdot1 + cosio * (temp2 * .5 * (4. - theta2 * 19.) + temp3 * 
		2. * (3. - theta2 * 7.));
	omgcof = bstar * c3 * cos(omegao);
	xmcof = -bstar * .66666666666666663 * coef * ae / eeta;
	xnodcf = betao2 * 3.5 * xhdot1 * c1;
	t2cof = c1 * 1.5;
	aycof = a3ovk2 * .25 * sinio;
	xlcof = aycof * .5 * (cosio * 5. + 3.) / (cosio + 1.);
/* Computing 3rd power */
	d__1 = eta * cos(xmo) + 1.;
	delmo = d__1 * (d__1 * d__1);
	sinmo = sin(xmo);

/*        For perigee less than 220 kilometers, the ISIMP flag is set */
/*        and the equations are truncated to linear variation in SQRT */
/*        A and quadratic variation in mean anomaly.  Also, the C3 */
/*        term, the Delta OMEGA term, and the Delta M term are */
/*        dropped.  (Note: Normally we would just use */

	if (perige >= 220.) {
	    c1sq = c1 * c1;
	    d2 = tsi * 4. * c1sq * aodp;
	    temp = d2 * tsi * c1 * .33333333333333331;
	    d3 = temp * (s4 + aodp * 17.);
	    d4 = temp * tsi * c1 * aodp * .5 * (aodp * 221. + s4 * 31.);
	    t3cof = d2 + c1sq * 2.;
	    t4cof = (d3 * 3. + c1 * (d2 * 12. + c1sq * 10.)) * .25;
	    t5cof = (d4 * 3. + c1 * 12. * d3 + d2 * 6. * d2 + c1sq * 15. * (
		    d2 * 2. + c1sq)) * .2;
	}

/*        Now store the intermediate computations so that if we */
/*        should hit this model again we can just look up the needed */
/*        results from the above computations. */

	prelim[(i__1 = n * 29 - 29) < 174 && 0 <= i__1 ? i__1 : s_rnge("prel"
		"im", i__1, "ev2lin_", (ftnlen)1194)] = aodp;
	prelim[(i__1 = n * 29 - 28) < 174 && 0 <= i__1 ? i__1 : s_rnge("prel"
		"im", i__1, "ev2lin_", (ftnlen)1195)] = aycof;
	prelim[(i__1 = n * 29 - 27) < 174 && 0 <= i__1 ? i__1 : s_rnge("prel"
		"im", i__1, "ev2lin_", (ftnlen)1196)] = c1;
	prelim[(i__1 = n * 29 - 26) < 174 && 0 <= i__1 ? i__1 : s_rnge("prel"
		"im", i__1, "ev2lin_", (ftnlen)1197)] = c4;
	prelim[(i__1 = n * 29 - 25) < 174 && 0 <= i__1 ? i__1 : s_rnge("prel"
		"im", i__1, "ev2lin_", (ftnlen)1198)] = c5;
	prelim[(i__1 = n * 29 - 24) < 174 && 0 <= i__1 ? i__1 : s_rnge("prel"
		"im", i__1, "ev2lin_", (ftnlen)1199)] = cosio;
	prelim[(i__1 = n * 29 - 23) < 174 && 0 <= i__1 ? i__1 : s_rnge("prel"
		"im", i__1, "ev2lin_", (ftnlen)1200)] = d2;
	prelim[(i__1 = n * 29 - 22) < 174 && 0 <= i__1 ? i__1 : s_rnge("prel"
		"im", i__1, "ev2lin_", (ftnlen)1201)] = d3;
	prelim[(i__1 = n * 29 - 21) < 174 && 0 <= i__1 ? i__1 : s_rnge("prel"
		"im", i__1, "ev2lin_", (ftnlen)1202)] = d4;
	prelim[(i__1 = n * 29 - 20) < 174 && 0 <= i__1 ? i__1 : s_rnge("prel"
		"im", i__1, "ev2lin_", (ftnlen)1203)] = delmo;
	prelim[(i__1 = n * 29 - 19) < 174 && 0 <= i__1 ? i__1 : s_rnge("prel"
		"im", i__1, "ev2lin_", (ftnlen)1204)] = eta;
	prelim[(i__1 = n * 29 - 18) < 174 && 0 <= i__1 ? i__1 : s_rnge("prel"
		"im", i__1, "ev2lin_", (ftnlen)1205)] = omgcof;
	prelim[(i__1 = n * 29 - 17) < 174 && 0 <= i__1 ? i__1 : s_rnge("prel"
		"im", i__1, "ev2lin_", (ftnlen)1206)] = omgdot;
	prelim[(i__1 = n * 29 - 16) < 174 && 0 <= i__1 ? i__1 : s_rnge("prel"
		"im", i__1, "ev2lin_", (ftnlen)1207)] = perige;
	prelim[(i__1 = n * 29 - 15) < 174 && 0 <= i__1 ? i__1 : s_rnge("prel"
		"im", i__1, "ev2lin_", (ftnlen)1208)] = sinio;
	prelim[(i__1 = n * 29 - 14) < 174 && 0 <= i__1 ? i__1 : s_rnge("prel"
		"im", i__1, "ev2lin_", (ftnlen)1209)] = sinmo;
	prelim[(i__1 = n * 29 - 13) < 174 && 0 <= i__1 ? i__1 : s_rnge("prel"
		"im", i__1, "ev2lin_", (ftnlen)1210)] = t2cof;
	prelim[(i__1 = n * 29 - 12) < 174 && 0 <= i__1 ? i__1 : s_rnge("prel"
		"im", i__1, "ev2lin_", (ftnlen)1211)] = t3cof;
	prelim[(i__1 = n * 29 - 11) < 174 && 0 <= i__1 ? i__1 : s_rnge("prel"
		"im", i__1, "ev2lin_", (ftnlen)1212)] = t4cof;
	prelim[(i__1 = n * 29 - 10) < 174 && 0 <= i__1 ? i__1 : s_rnge("prel"
		"im", i__1, "ev2lin_", (ftnlen)1213)] = t5cof;
	prelim[(i__1 = n * 29 - 9) < 174 && 0 <= i__1 ? i__1 : s_rnge("prelim"
		, i__1, "ev2lin_", (ftnlen)1214)] = x1mth2;
	prelim[(i__1 = n * 29 - 8) < 174 && 0 <= i__1 ? i__1 : s_rnge("prelim"
		, i__1, "ev2lin_", (ftnlen)1215)] = x3thm1;
	prelim[(i__1 = n * 29 - 7) < 174 && 0 <= i__1 ? i__1 : s_rnge("prelim"
		, i__1, "ev2lin_", (ftnlen)1216)] = x7thm1;
	prelim[(i__1 = n * 29 - 6) < 174 && 0 <= i__1 ? i__1 : s_rnge("prelim"
		, i__1, "ev2lin_", (ftnlen)1217)] = xlcof;
	prelim[(i__1 = n * 29 - 5) < 174 && 0 <= i__1 ? i__1 : s_rnge("prelim"
		, i__1, "ev2lin_", (ftnlen)1218)] = xmcof;
	prelim[(i__1 = n * 29 - 4) < 174 && 0 <= i__1 ? i__1 : s_rnge("prelim"
		, i__1, "ev2lin_", (ftnlen)1219)] = xmdot;
	prelim[(i__1 = n * 29 - 3) < 174 && 0 <= i__1 ? i__1 : s_rnge("prelim"
		, i__1, "ev2lin_", (ftnlen)1220)] = xnodcf;
	prelim[(i__1 = n * 29 - 2) < 174 && 0 <= i__1 ? i__1 : s_rnge("prelim"
		, i__1, "ev2lin_", (ftnlen)1221)] = xnodot;
	prelim[(i__1 = n * 29 - 1) < 174 && 0 <= i__1 ? i__1 : s_rnge("prelim"
		, i__1, "ev2lin_", (ftnlen)1222)] = xnodp;

/*        Finally, move these elements in the storage area */
/*        for checking the next time through. */

	for (i__ = 1; i__ <= 10; ++i__) {
	    elemnt[(i__1 = i__ + n * 10 - 11) < 60 && 0 <= i__1 ? i__1 : 
		    s_rnge("elemnt", i__1, "ev2lin_", (ftnlen)1228)] = elems[
		    i__ - 1];
	}
    }

/*     Now that all of the introductions are out of the way */
/*     we can get down to business. */

/*     Compute the time since the epoch for this model. */

    tsince = *et - epoch;

/*     and convert it to minutes */

    tsince /= 60.;
    xmdf = xmo + xmdot * tsince;
    omgadf = omegao + omgdot * tsince;
    xnoddf = xnodeo + xnodot * tsince;
    omega = omgadf;
    xmp = xmdf;
    tsq = tsince * tsince;
    xnode = xnoddf + xnodcf * tsq;
    tempa = 1. - c1 * tsince;
    tempe = bstar * c4 * tsince;
    templ = t2cof * tsq;
    if (perige > 220.) {
	tcube = tsq * tsince;
	tfour = tcube * tsince;
	delomg = omgcof * tsince;
/* Computing 3rd power */
	d__1 = eta * cos(xmdf) + 1.;
	delm = xmcof * (d__1 * (d__1 * d__1) - delmo);
	temp = delomg + delm;
	xmp = xmdf + temp;
	omega = omgadf - temp;
	tempa = tempa - d2 * tsq - d3 * tcube - d4 * tfour;
	tempe += bstar * c5 * (sin(xmp) - sinmo);
	templ = templ + tcube * t3cof + tfour * (t4cof + tsince * t5cof);
    }
/* Computing 2nd power */
    d__1 = tempa;
    a = aodp * (d__1 * d__1);
    xl = xmp + omega + xnode + xnodp * templ;
    e = eo - tempe;

/*     The parameter BETA used to be needed, but it's only use */
/*     was in the computation of TEMP below where it got squared */
/*     so we'll remove it from the list of things to compute. */

/*     BETA =  DSQRT( 1.0D0 - E*E ) */

    xn = ke / pow_dd(&a, &c_b153);

/*     Long period periodics */

    temp = 1. / (a * (1. - e * e));
    aynl = temp * aycof;
    ayn = e * sin(omega) + aynl;
    axn = e * cos(omega);
    xll = temp * xlcof * axn;
    xlt = xl + xll;

/*     Solve Kepler's equation. */

/*     We are going to solve for the roots of this equation by */
/*     using a mixture of Newton's method and the prescription for */
/*     root finding outlined in the SPICE routine UNITIM. */

/*     We are going to solve the equation */

/*           U = EPW - AXN * SIN(EPW)  +  AYN * COS(EPW) */

/*     Where */

/*        AYN  = E    * SIN(OMEGA)   +    AYNL */
/*        AXN  = E    * COS(OMEGA) */

/*     And */

/*        AYNL =  -0.50D0  * SINIO * AE * J3   / (J2*A*(1.0D0 - E*E)) */

/*     Since this is a low earth orbiter (period less than 225 minutes) */
/*     The maximum value E can take (without having the orbiter */
/*     plowing fields) is approximately 0.47 and AYNL will not be */
/*     more than about .01.  ( Typically E will be much smaller */
/*     on the order of about .1 )  Thus we can initially */
/*     view the problem of solving the equation for EPW as a */
/*     function of the form */

/*     U = EPW + F ( EPW )                                   (1) */

/*     Where F( EPW ) = -AXN*SIN(EPW) + AYN*COS(EPW) */

/*     Note that | F'(EPW) | < M =  DSQRT( AXN**2 + AYN**2 ) < 0.48 */

/*     From the above discussion it is evident that F is a contraction */
/*     mapping.  So that we can employ the same techniques as were */
/*     used in the routine UNITIM to get our first approximations of */
/*     the root.  Once we have some good first approximations, we */
/*     will speed up the root finding by using Newton's method for */
/*     finding a zero of a function.  The function we will work on */
/*     is */

/*        f  (x) = x - U - AXN*SIN(x) + AYN*COS(x)         (2) */

/*     By applying Newton's method we will go from linear to */
/*     quadratic convergence. */

/*     We will keep track of our error bounds along the way so */
/*     that we will know how many iterations to perform in each */
/*     phase of the root extraction. */

/*     few steps using bisection. */


/*     For the benefit of those interested */
/*     here's the basics of what we'll do. */

/*        Whichever EPW satisfies equation (1) will be */
/*        unique. The uniqueness of the solution is ensured because the */
/*        expression on the right-hand side of the equation is */
/*        monotone increasing in EPW. */

/*        Let's suppose that EPW is the solution, then the following */
/*        is true. */

/*           EPW = U - F(EPW) */

/*        but we can also replace the EPW on the right hand side of the */
/*        equation by U - F(EPW).  Thus */

/*           EPW = U - F( U - F(EPW)) */

/*               = U - F( U - F( U - F(EPW))) */

/*               = U - F( U - F( U - F( U - F(EPW)))) */

/*               = U - F( U - F( U - F( U - F( U - F(EPW))))) */
/*               . */
/*               . */
/*               . */
/*               = U - F( U - F( U - F( U - F( U - F(U - ... ))) */

/*        and so on, for as long as we have patience to perform the */
/*        substitutions. */

/*        The point of doing this recursive substitution is that we */
/*        hope to move EPW to an insignificant part of the computation. */
/*        This would seem to have a reasonable chance of success since */
/*        F is a bounded and has a small derivative. */

/*        Following this idea, we will attempt to solve for EPW using */
/*        the recursive method outlined below. */

/*     We will make our first guess at EPW, call it EPW_0. */

/*        EPW_0 = U */

/*     Our next guess, EPW_1, is given by: */

/*        EPW_1 = U - F(EPW_0) */

/*     And so on: */

/*        EPW_2 = U - F(EPW_1)        [ = U - F(U - F(U))          ] */
/*        EPW_3 = U - F(EPW_2)        [ = U - F(U - F(U - F(U)))   ] */
/*           . */
/*           . */
/*           . */
/*        EPW_n = U - F(EPW_(n-1))    [ = U - F(U - F(U - F(U...)))] */

/*        The questions to ask at this point are: */

/*           1) Do the EPW_i's converge? */
/*           2) If they converge, do they converge to EPW? */
/*           3) If they converge to EPW, how fast do they get there? */

/*        1) The sequence of approximations converges. */

/*           | EPW_n - EPW_(n-1) | =  [ U - F( EPW_(n-1) ) ] */
/*                                 -  [ U - F( EPW_(n-2) ) ] */

/*                                 =  [ F( EPW_(n-2) ) - F( EPW_(n-1)) ] */

/*     The function F has an important property. The absolute */
/*     value of its derivative is always less than M. */
/*     This means that for any pair of real numbers s,t */

/*        | F(t) - F(s) |  < M*| t - s |. */

/*     From this observation, we can see that */

/*        | EPW_n - EPW_(n-1) | < M*| EPW_(n-1) - EPW_(n-2) | */

/*     With this fact available, we could (with a bit more work) */
/*     conclude that the sequence of EPW_i's converges and that */
/*     it converges at a rate that is at least as fast as the */
/*     sequence M, M**2, M**3.  In fact the difference */
/*        |EPW - EPW_N| < M/(1-M) * | EPW_N - EPW_(N-1) | */

/*                       < M/(1-M) * M**N | EPW_1 - EPW_0 | */

/*     2) If we let EPW be the limit of the EPW_i's then it follows */
/*        that */

/*               EPW = U - F(EPW). */


/*     or that */

/*               U = EPW + F(EPW). */

/*     We will use this technique to get an approximation that */
/*     is within a tolerance of EPW and then switch to */
/*     a Newton's method. (We'll compute the tolerance using */
/*     the value of M given above). */


/*     For the Newton's method portion of the problem, recall */
/*     from Taylor's formula that: */

/*        f(x) = f(x_0) + f'(x_0)(x-x_0) +  f''(c)/2 (x-x_0)**2 */

/*     for some c between x and x_0 */

/*     If x happens to be a zero of f then we can rearrange the */
/*     terms above to get */

/*                       f(x_0)       f''(c) */
/*           x = x_0 -   -------  +  -------- ( x - x_0)**2 */
/*                       f'(x_0)      f'(x_0) */

/*     Thus the error in the Newton approximation */


/*                       f(x_0) */
/*           x = x_0  -  ------- */
/*                       f'(x_0) */

/*     is */

/*                     f''(c) */
/*                    -------- ( x - x_0)**2 */
/*                     f'(x_0) */

/*     Thus if we can bound f'' and pick a good first */
/*     choice for x_0 (using the first method outlined */
/*     above we can get quadratic convergence.) */

/*     In our case we have */

/*        f  (x) = x - U - AXN*SIN(x) + AYN*COS(x) */
/*        f' (x) = 1     - AXN*COS(x) - AYN*SIN(x) */
/*        f''(x) =         AXN*SIN(x) - AYN*COS(x) */

/*     So that: */

/*        f' (x) >  1 - M */

/*        f''(x) <  M */

/*     Thus the error in the Newton's approximation is */
/*     at most */

/*        M/(1-M) * ( x - x_0 )**2 */

/*     Thus as long as our original estimate (determined using */
/*     the contraction method) gets within a reasonable tolerance */
/*     of x, we can use Newton's method to achieve faster */
/*     convergence. */

    m = sqrt(axn * axn + ayn * ayn);
    mov1m = (d__1 = m / (1. - m), abs(d__1));
    d__1 = xlt - xnode;
    fmod2p = d_mod(&d__1, &pix2);
    if (fmod2p < 0.) {
	fmod2p += pix2;
    }
    capu = fmod2p;
    epw = capu;
    est = 1.;
    count = 0;
    while(est > .125) {
	++count;
	if (count > 20) {
	    setmsg_("EST iteration count of #1 exceeded at time ET #2. This "
		    "error may indicate a bad TLE set.", (ftnlen)88);
	    errint_("#1", &c__20, (ftnlen)2);
	    errdp_("#2", et, (ftnlen)2);
	    sigerr_("SPICE(ITERATIONEXCEEDED)", (ftnlen)24);
	    chkout_("EV2LIN", (ftnlen)6);
	    return 0;
	}
	epwnxt = capu - axn * sin(epw) + ayn * cos(epw);
	est = mov1m * (d__1 = epwnxt - epw, abs(d__1));
	epw = epwnxt;
    }

/*     We need to be able to add something to EPW and not */
/*     get EPW (but not too much). */

    epsiln = est;
    if (epsiln + epw != epw) {

/*        Now we switch over to Newton's method.  Note that */
/*        since our error estimate is less than 1/8, six iterations */
/*        of Newton's method should get us to within 1/2**96 of */
/*        the correct answer (If there were no round off to contend */
/*        with). */

	for (i__ = 1; i__ <= 5; ++i__) {
	    sinepw = sin(epw);
	    cosepw = cos(epw);
	    f = epw - capu - axn * sinepw + ayn * cosepw;
	    fprime = 1. - axn * cosepw - ayn * sinepw;
	    epwnxt = epw - f / fprime;

/*           Our new error estimate comes from the discussion */
/*           of convergence of Newton's method. */

	    epw = epwnxt;
	    if (epw + est != epw) {
		epsiln = est;
		est = mov1m * est * est;
	    }
	}
    }

/*     Finally, we use bisection to avoid the problems of */
/*     round-off that may be present in Newton's method.  Since */
/*     we've gotten quite close to the answer (theoretically */
/*     anyway)  we won't have to perform many bisection passes. */

/*     First we must bracket the root.  Note that we will */
/*     increase EPSILN so that we don't spend much time */
/*     determining the bracketing interval.  Also if the first */
/*     addition of EPSILN to EPW doesn't modify it, were set up */
/*     to just quit.  This happens only if F is sufficiently */
/*     close to zero that it can't alter EPW by adding it to */
/*     or subtracting it from EPW. */

    sinepw = sin(epw);
    cosepw = cos(epw);
    f = epw - capu - axn * sinepw + ayn * cosepw;
/* Computing MAX */
    d__1 = abs(f);
    epsiln = max(d__1,epsiln);
    if (f == 0.) {
	lower = epw;
	upper = epw;
    } else if (f > 0.) {
	fu = f;
	upper = epw;
	lower = epw - epsiln;
	epw = lower;
	while(f > 0. && lower != upper) {
	    epw -= epsiln;
	    f = epw - capu - axn * sin(epw) + ayn * cos(epw);
	    epsiln *= 2.;
	}
	lower = epw;
	fl = f;
	if (f == 0.) {
	    upper = lower;
	}
    } else if (f < 0.) {
	fl = f;
	lower = epw;
	upper = epw + epsiln;
	epw = upper;
	while(f < 0. && lower != upper) {
	    epw += epsiln;
	    f = epw - capu - axn * sin(epw) + ayn * cos(epw);
	    epsiln *= 2.;
	}
	upper = epw;
	fu = f;
	if (f == 0.) {
	    lower = epw;
	}
    }

/*     Finally, bisect until we can do no more. */

    count = 0;
    while(upper > lower && count < 20) {
	++count;
	d__1 = (upper + lower) * .5;
	epw = brcktd_(&d__1, &lower, &upper);

/*        EPW eventually will not be different from one of the */
/*        two bracketing values.  If this is the time, we need */
/*        to decide on a value for EPW.  That's done below. */

	if (epw == upper || epw == lower) {
	    if (-fl < fu) {
		epw = lower;
		upper = lower;
	    } else {
		epw = upper;
		lower = upper;
	    }
	} else {
	    f = epw - capu - axn * sin(epw) + ayn * cos(epw);
	    if (f > 0.) {
		upper = epw;
		fu = f;
	    } else if (f < 0.) {
		lower = epw;
		fl = f;
	    } else {
		lower = epw;
		upper = epw;
	    }
	}
    }

/*     Short period preliminary quantities */

    sinepw = sin(epw);
    cosepw = cos(epw);
    temp3 = axn * sinepw;
    temp4 = ayn * cosepw;
    temp5 = axn * cosepw;
    temp6 = ayn * sinepw;
    ecose = temp5 + temp6;
    esine = temp3 - temp4;
    elsq = axn * axn + ayn * ayn;
    temp = 1. - elsq;
    pl = a * temp;
    r__ = a * (1. - ecose);
    temp1 = 1. / r__;
    rdot = ke * temp1 * sqrt(a) * esine;
    rfdot = ke * temp1 * sqrt(pl);
    temp2 = a * temp1;
    betal = sqrt(temp);
    temp3 = 1. / (betal + 1.);
    cosu = temp2 * (cosepw - axn + ayn * esine * temp3);
    sinu = temp2 * (sinepw - ayn - axn * esine * temp3);

/*     Compute the angle from the x-axis of the point ( COSU, SINU ) */

    if (sinu != 0. || cosu != 0.) {
	u = atan2(sinu, cosu);
	if (u < 0.) {
	    u += pix2;
	}
    } else {
	u = 0.;
    }
    sin2u = sinu * 2. * cosu;
    cos2u = cosu * 2. * cosu - 1.;
    temp = 1. / pl;
    temp1 = ck2 * temp;
    temp2 = temp1 * temp;

/*     Update for short periodics */

    rk = r__ * (1. - temp2 * 1.5 * betal * x3thm1) + temp1 * .5 * x1mth2 * 
	    cos2u;
    uk = u - temp2 * .25 * x7thm1 * sin2u;
    xnodek = xnode + temp2 * 1.5 * cosio * sin2u;
    xinck = xincl + temp2 * 1.5 * cosio * cos2u * sinio;
    rdotk = rdot - xn * temp1 * x1mth2 * sin2u;
    rfdotk = rfdot + xn * temp1 * (x1mth2 * cos2u + x3thm1 * 1.5);

/*     Orientation vectors */

    sinuk = sin(uk);
    cosuk = cos(uk);
    sinik = sin(xinck);
    cosik = cos(xinck);
    sinnok = sin(xnodek);
    cosnok = cos(xnodek);
    xmx = -sinnok * cosik;
    xmy = cosnok * cosik;
    ux = xmx * sinuk + cosnok * cosuk;
    uy = xmy * sinuk + sinnok * cosuk;
    uz = sinik * sinuk;
    vx = xmx * cosuk - cosnok * sinuk;
    vy = xmy * cosuk - sinnok * sinuk;
    vz = sinik * cosuk;

/*     Position and velocity */

    state[0] = tokm * rk * ux;
    state[1] = tokm * rk * uy;
    state[2] = tokm * rk * uz;
    state[3] = tokmps * (rdotk * ux + rfdotk * vx);
    state[4] = tokmps * (rdotk * uy + rfdotk * vy);
    state[5] = tokmps * (rdotk * uz + rfdotk * vz);
    chkout_("EV2LIN", (ftnlen)6);
    return 0;
} /* ev2lin_ */

