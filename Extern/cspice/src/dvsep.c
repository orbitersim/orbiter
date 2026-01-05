/* dvsep.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure DVSEP ( Derivative of separation angle ) */
doublereal dvsep_(doublereal *s1, doublereal *s2)
{
    /* System generated locals */
    doublereal ret_val, d__1;

    /* Local variables */
    extern doublereal vdot_(doublereal *, doublereal *);
    doublereal numr;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    doublereal denom;
    extern /* Subroutine */ int dvhat_(doublereal *, doublereal *), vcrss_(
	    doublereal *, doublereal *, doublereal *);
    extern doublereal vnorm_(doublereal *), zzdiv_(doublereal *, doublereal *)
	    ;
    extern logical vzero_(doublereal *);
    doublereal u1[6], u2[6];
    extern /* Subroutine */ int chkout_(char *, ftnlen);
    doublereal pcross[3];
    extern logical return_(void);

/* $ Abstract */

/*     Calculate the time derivative of the separation angle between */
/*     two input states, S1 and S2. */

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

/*     DERIVATIVES */
/*     GEOMETRY */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     S1         I   State vector of the first body. */
/*     S2         I   State vector of the second body. */

/*     The function returns the time derivative of the separation angle */
/*     between the two input states, S1 and S2. */

/* $ Detailed_Input */

/*     S1, */
/*     S2       are, respectively, the state vector of the first and */
/*              second target bodies as seen from the observer. */

/*              An implicit assumption exists that both states lie in the */
/*              same reference frame with the same observer for the same */
/*              epoch. If this is not the case, the numerical result has */
/*              no meaning. */

/* $ Detailed_Output */

/*     The function returns the double precision value of the time */
/*     derivative of the angular separation between S1 and S2. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If numeric overflow and underflow cases are detected, an error */
/*         is signaled by a routine in the call tree of this routine. */

/*     2)  If called in 'RETURN' mode, the function returns 0. */

/*     3)  Linear dependent position components of S1 and S1 constitutes */
/*         a non-error exception. The function returns 0 for this case. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     In this discussion, the notation */

/*        < V1, V2 > */

/*     indicates the dot product of vectors V1 and V2. The notation */

/*        V1 x V2 */

/*     indicates the cross product of vectors V1 and V2. */

/*     To start out, note that we need consider only unit vectors, */
/*     since the angular separation of any two non-zero vectors */
/*     equals the angular separation of the corresponding unit vectors. */
/*     Call these vectors U1 and U2; let their velocities be V1 and V2. */

/*     For unit vectors having angular separation */

/*        THETA */

/*     the identity */

/*        || U1 x U1 || = ||U1|| * ||U2|| * sin(THETA)                (1) */

/*     reduces to */

/*        || U1 x U2 || = sin(THETA)                                  (2) */

/*     and the identity */

/*        | < U1, U2 > | = || U1 || * || U2 || * cos(THETA)           (3) */

/*     reduces to */

/*        | < U1, U2 > | = cos(THETA)                                 (4) */

/*     Since THETA is an angular separation, THETA is in the range */

/*        0 : Pi */

/*     Then letting s be +1 if cos(THETA) > 0 and -1 if cos(THETA) < 0, */
/*     we have for any value of THETA other than 0 or Pi */


/*                                  2          1/2 */
/*        cos(THETA) = s * ( 1 - sin (THETA)  )                       (5) */

/*     or */

/*                                  2          1/2 */
/*        < U1, U2 > = s * ( 1 - sin (THETA)  )                       (6) */


/*     At this point, for any value of THETA other than 0 or Pi, */
/*     we can differentiate both sides with respect to time (T) */
/*     to obtain */

/*                                                      2        -1/2 */
/*        < U1, V2 > + < V1, U2 > =    s * (1/2)(1 - sin (THETA)) */

/*                                   * (-2) sin(THETA)*cos(THETA) */

/*                                   * d(THETA)/dT                   (7a) */


/*     Using equation (5), and noting that s = 1/s, we can cancel */
/*     the cosine terms on the right hand side */

/*                                                      -1 */
/*        < U1, V2 > + < V1, U2 > =    (1/2)(cos(THETA)) */

/*                                   * (-2) sin(THETA)*cos(THETA) */

/*                                   * d(THETA)/dT                   (7b) */

/*     With (7b) reducing to */

/*        < U1, V2 > + < V1, U2 > = - sin(THETA) * d(THETA)/dT        (8) */

/*     Using equation (2) and switching sides, we obtain */

/*        || U1 x U2 || * d(THETA)/dT  =  - < U1, V2 > - < V1, U2 >   (9) */

/*     or, provided U1 and U2 are linearly independent, */

/*        d(THETA)/dT = ( - < U1, V2 > - < V1, U2 > ) / ||U1 x U2||  (10) */

/*     Note for times when U1 and U2 have angular separation 0 or Pi */
/*     radians, the derivative of angular separation with respect to */
/*     time doesn't exist. (Consider the graph of angular separation */
/*     with respect to time; typically the graph is roughly v-shaped at */
/*     the singular points.) */

/* $ Examples */

/*     The numerical results shown for this example may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */

/*     1) Calculate the time derivative of the angular separation of */
/*        the Earth and Moon as seen from the Sun at a given time. */


/*        Use the meta-kernel shown below to load the required SPICE */
/*        kernels. */


/*           KPL/MK */

/*           File name: dvsep_ex1.tm */

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
/*              de421.bsp                     Planetary ephemeris */
/*              pck00010.tpc                  Planet orientation and */
/*                                            radii */
/*              naif0012.tls                  Leapseconds */


/*           \begindata */

/*              KERNELS_TO_LOAD = ( 'de421.bsp', */
/*                                  'pck00010.tpc', */
/*                                  'naif0012.tls'  ) */

/*           \begintext */

/*           End of meta-kernel */


/*        Example code begins here. */


/*              PROGRAM DVSEP_EX1 */
/*              IMPLICIT NONE */

/*              DOUBLE PRECISION      ET */
/*              DOUBLE PRECISION      LT */
/*              DOUBLE PRECISION      DSEPT */
/*              DOUBLE PRECISION      STATEE (6) */
/*              DOUBLE PRECISION      STATEM (6) */

/*              INTEGER               STRLEN */
/*              PARAMETER           ( STRLEN = 64 ) */

/*              CHARACTER*(STRLEN)    BEGSTR */

/*              DOUBLE PRECISION      DVSEP */

/*        C */
/*        C     Load kernels. */
/*        C */
/*              CALL FURNSH ('dvsep_ex1.tm') */

/*        C */
/*        C     An arbitrary time. */
/*        C */
/*              BEGSTR = 'JAN 1 2009' */
/*              CALL STR2ET( BEGSTR, ET ) */

/*        C */
/*        C     Calculate the state vectors Sun to Moon, and */
/*        C     Sun to Earth at ET. */
/*        C */
/*        C */
/*              CALL SPKEZR ( 'EARTH', ET, 'J2000', 'NONE', 'SUN', */
/*             .               STATEE, LT) */

/*              CALL SPKEZR ( 'MOON', ET, 'J2000', 'NONE', 'SUN', */
/*             .               STATEM, LT) */

/*        C */
/*        C     Calculate the time derivative of the angular separation */
/*        C     of the Earth and Moon as seen from the Sun at ET. */
/*        C */
/*              DSEPT = DVSEP( STATEE, STATEM ) */
/*              WRITE(*,*) 'Time derivative of angular' */
/*              WRITE(*,*) '   separation (rad/sec): ', DSEPT */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*         Time derivative of angular */
/*            separation (rad/sec):    3.8121193666132696E-009 */


/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     E.D. Wright        (JPL) */

/* $ Version */

/* -    SPICELIB Version 2.0.1, 06-JUL-2021 (JDR) */

/*        Edited the header to comply with NAIF standard. Added problem */
/*        statement and meta-kernel to the example. Modified output to */
/*        comply with maximum line length of header comments. */

/* -    SPICELIB Version 2.0.0, 21-MAR-2014 (EDW) */

/*        Reimplemented algorithm using ZZDIV. */

/* -    SPICELIB Version 1.0.1, 15-MAR-2010 (EDW) */

/*        Trivial header format clean-up. */

/* -    SPICELIB Version 1.0.0, 31-MAR-2009 (EDW) */

/* -& */
/* $ Index_Entries */

/*     time derivative of angular separation */

/* -& */

/*     SPICELIB functions */


/*     Local variables */

    if (return_()) {
	ret_val = 0.;
	return ret_val;
    }
    chkin_("DVSEP", (ftnlen)5);

/*     Compute the unit vectors and corresponding time derivatives */
/*     for the input state vectors. */

    dvhat_(s1, u1);
    dvhat_(s2, u2);

/*     Calculate the cross product vector of U1 and U2. As both vectors */
/*     have magnitude one, the magnitude of the cross product equals */
/*     sin(THETA), with THETA the angle between S1 and S2. */

    vcrss_(u1, u2, pcross);

/*     Handle the parallel and anti-parallel cases. */

    if (vzero_(pcross)) {
	ret_val = 0.;
	chkout_("DVSEP", (ftnlen)5);
	return ret_val;
    }

/*     Now calculate the time derivative of the angular separation */
/*     between S1 and S2. */


/*     Separately calculate the numerator and denominator. */

    numr = vdot_(u1, &u2[3]) + vdot_(&u1[3], u2);
    denom = vnorm_(pcross);

/*     ZZDIV checks for over- or underflow. Finite precision */
/*     arithmetic is a pain. */

    d__1 = -numr;
    ret_val = zzdiv_(&d__1, &denom);

/*     Return, the expectation exists that a FAILED() call */
/*     follows the DVSEP call. */

    chkout_("DVSEP", (ftnlen)5);
    return ret_val;
} /* dvsep_ */

