/* evsgp4.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;

/* $Procedure EVSGP4 ( Evaluate "two-line" element data ) */
/* Subroutine */ int evsgp4_(doublereal *et, doublereal *geophs, doublereal *
	elems, doublereal *state)
{
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    doublereal t1;
    extern logical failed_(void);
    extern /* Subroutine */ int chkout_(char *, ftnlen);
    extern logical return_(void);
    extern /* Subroutine */ int xxsgp4e_(doublereal *, doublereal *), 
	    xxsgp4i_(doublereal *, doublereal *, integer *);

/* $ Abstract */

/*     Evaluate NORAD two-line element data for earth orbiting */
/*     spacecraft. This evaluator uses algorithms as described */
/*     in Vallado 2006 [4]. */

/*     This routine supersedes SPICELIB routines EV2LIN and DPSPCE. */

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
/* $Procedure ZZSGP4 ( SGP4 parameters ) */

/* $ Abstract */

/*      Parameter assignments for SGP4 algorithm as expressed */
/*      by Vallado [2]. */

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

/*     None. */

/* $ Declarations */

/*     None. */

/* $ Brief_I/O */

/*     None. */

/* $ Detailed_Input */

/*     None. */

/* $ Detailed_Output */

/*     None. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     None. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     None. */

/* $ Examples */

/*     J2    = GEOPHS(K_J2) */
/*     J3    = GEOPHS(K_J3) */
/*     J4    = GEOPHS(K_J4) */
/*     ER    = GEOPHS(K_ER) */
/*     XKE   = GEOPHS(K_KE) */

/*     TUMIN = 1.D0/XKE */
/*     J3OJ2 = J3/J2 */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*   [1] Hoots, F. R., and Roehrich, R. L. 1980. "Models for */
/*       Propagation of the NORAD Element Sets." Spacetrack Report #3. */
/*       U.S. Air Force: Aerospace Defense Command. */

/*   [2] Vallado, David, Crawford, Paul, Hujsak, Richard, and Kelso, T.S. */
/*       2006. Revisiting Spacetrack Report #3. Paper AIAA 2006-6753 */
/*       presented at the AIAA/AAS Astrodynamics Specialist Conference, */
/*       August 21-24, 2006. Keystone, CO. */

/* $ Author_and_Institution */

/*     E. D. Wright    (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.0, MAY-27-2020 (EDW) */

/*        Updated descriptions of GEOPHS constants to be consistent */
/*        with what's used in other routines. */

/* -    SPICELIB Version 1.0.0 22-JUL-2014 (EDW) */

/* -& */
/* $ Index_Entries */

/*  SGP4 */

/* -& */

/*      WGS gravitational constants IDs. */


/*      Gravitational constant indices. */


/*     The following parameters give the indices in the GEOPHS */
/*     array of the various geophysical parameters needed for */
/*     the two line element sets. */

/*     K_J2  --- index of J2 gravitational harmonic for earth */
/*     K_J3  --- index of J3 gravitational harmonic for earth */
/*     K_J4  --- index of J4 gravitational harmonic for earth */
/*     K_KE  --- index of KE = sqrt(GM) in earth-radii**1.5/MIN */
/*     K_QO  --- index of high altitude bound for atmospheric */
/*               model in km */
/*     K_SO  --- index of low altitude bound for atmospheric */
/*               model in km */
/*     K_ER  --- index of earth equatorial radius in km */
/*     K_AE  --- index of distance units/earth radius */


/*     Operation mode values, OPMODE. */


/*     An enumeration of the various components of the */
/*     elements array---ELEMS */

/*     KNDT20  --- location of NDT20 */
/*     KNDD60  --- location of NDD60 */
/*     KBSTAR  --- location of BSTAR */
/*     KINCL   --- location of INCL */
/*     KNODE0  --- location of NODE0 */
/*     KECC    --- location of ECC */
/*     KOMEGA  --- location of OMEGA */
/*     KMO     --- location of MO */
/*     KNO     --- location of NO */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     ET         I   Epoch in seconds past ephemeris epoch J2000. */
/*     GEOPHS     I   Geophysical constants */
/*     ELEMS      I   Two-line element data */
/*     STATE      O   Evaluated state */

/* $ Detailed_Input */

/*     ET       is the epoch in seconds past ephemeris epoch J2000 */
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

/*     AFSPC    set the SGP4 propagator to use the original */
/*              Space Track #3 GST algorithm as described in Hoots [1]; */
/*              value defined in zzsgp4.inc. */

/* $ Exceptions */

/*     1)  No checks are made on the reasonableness of the inputs. */

/*     2)  If a problem occurs when evaluating the elements, an */
/*         error is signaled by a routine in the call tree of this */
/*         routine. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine evaluates any NORAD two-line element sets for */
/*     near-earth orbiting satellites using the algorithms described in */
/*     Vallado 2006 [4]. */

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

/*           File name: evsgp4_ex1.tm */

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

/*           The geophysical.ker is a PCK file that is provided with the */
/*           SPICE toolkit under the "/data" directory. */

/*           \begindata */

/*              KERNELS_TO_LOAD = ( 'naif0012.tls', */
/*                                  'geophysical.ker'  ) */

/*           \begintext */

/*           End of meta-kernel */


/*        Example code begins here. */


/*              PROGRAM EVSGP4_EX1 */
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
/*        C     required by EVSGP4. These constants are available from */
/*        C     the loaded PCK file, which provides the actual values */
/*        C     and units as used by NORAD propagation model. */
/*        C */
/*        C     Constant   Meaning */
/*        C     --------   ------------------------------------------ */
/*        C     J2         J2 gravitational harmonic for Earth. */
/*        C     J3         J3 gravitational harmonic for Earth. */
/*        C     J4         J4 gravitational harmonic for Earth. */
/*        C     KE         Square root of the GM for Earth. */
/*        C     QO         High altitude bound for atmospheric model. */
/*        C     SO         Low altitude bound for atmospheric model. */
/*        C     ER         Equatorial radius of the Earth. */
/*        C     AE         Distance units/earth radius. */
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
/*        C     Load the MK file that includes the PCK file that provides */
/*        C     the geophysical constants required for the evaluation of */
/*        C     the two-line elements sets and the LSK, as it is required */
/*        C     by GETELM to perform time conversions. */
/*        C */
/*              CALL FURNSH ( 'evsgp4_ex1.tm' ) */

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
/*        C     Now propagate the state using EVSGP4 to the epoch of */
/*        C     interest. */
/*        C */
/*              CALL STR2ET ( TIMSTR, ET ) */
/*              CALL EVSGP4 ( ET, GEOPHS, ELEMS, STATE ) */

/*        C */
/*        C     Display the results. */
/*        C */
/*              WRITE(*,'(2A)')       'Epoch   : ', TIMSTR */
/*              WRITE(*,'(A,3F16.8)') 'Position:', (STATE(I), I=1,3) */
/*              WRITE(*,'(A,3F16.8)') 'Velocity:', (STATE(I), I=4,6) */


/*              END */


/*        When this program was executed on a PC/Linux/gfortran/64-bit */
/*        platform, the output was: */


/*        Epoch   : 2020-05-26 02:25:00 */
/*        Position:  -4644.60403398  -5038.95025539   -337.27141116 */
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

/*     M. Costa Sitja     (JPL) */
/*     E.D. Wright        (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 02-NOV-2021 (EDW) (MCS) */

/* -& */
/* $ Index_Entries */

/*     Evaluate NORAD two-line element data using SGP4. */

/* -& */

/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }
    chkin_("EVSGP4", (ftnlen)6);

/*     Evaluate TLE. */


/*     Initialize. */

    xxsgp4i_(geophs, elems, &c__1);
    if (failed_()) {
	chkout_("EVSGP4", (ftnlen)6);
	return 0;
    }

/*     Calculate time from epoch in minutes. */

    t1 = elems[9];
    t1 = (*et - t1) / 60.;

/*     Compute state. */

    xxsgp4e_(&t1, state);
    if (failed_()) {
	chkout_("EVSGP4", (ftnlen)6);
	return 0;
    }

/*     Checkout, then return. */

    chkout_("EVSGP4", (ftnlen)6);
    return 0;
} /* evsgp4_ */

