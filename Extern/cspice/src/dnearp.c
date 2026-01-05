/* dnearp.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure DNEARP ( Derivative of near point ) */
/* Subroutine */ int dnearp_(doublereal *state, doublereal *a, doublereal *b, 
	doublereal *c__, doublereal *dnear, doublereal *dalt, logical *found)
{
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    extern logical failed_(void);
    extern /* Subroutine */ int nearpt_(doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *), chkout_(
	    char *, ftnlen);
    extern logical return_(void);
    extern /* Subroutine */ int zzdnpt_(doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, logical *);

/* $ Abstract */

/*     Compute the state (position and velocity) of an ellipsoid surface */
/*     point nearest to the position component of a specified state. */

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

/*     DERIVATIVE */
/*     ELLIPSOID */
/*     GEOMETRY */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     STATE      I   State of an object in body-fixed coordinates. */
/*     A          I   Length of semi-axis parallel to X-axis. */
/*     B          I   Length of semi-axis parallel to Y-axis. */
/*     C          I   Length on semi-axis parallel to Z-axis. */
/*     DNEAR      O   State of the nearest point on the ellipsoid. */
/*     DALT       O   Altitude and derivative of altitude. */
/*     FOUND      O   Flag that indicates whether DNEAR is degenerate. */

/* $ Detailed_Input */

/*     STATE    is a 6-vector giving the position and velocity of some */
/*              object in the body-fixed coordinates of the ellipsoid. */

/*              In body-fixed coordinates, the semi-axes of the ellipsoid */
/*              are aligned with the X, Y, and Z-axes of the coordinate */
/*              system. */

/*     A        is the length of the semi-axis of the ellipsoid that is */
/*              parallel to the X-axis of the body-fixed coordinate */
/*              system. */

/*     B        is the length of the semi-axis of the ellipsoid that is */
/*              parallel to the Y-axis of the body-fixed coordinate */
/*              system. */

/*     C        is the length of the semi-axis of the ellipsoid that is */
/*              parallel to the Z-axis of the body-fixed coordinate */
/*              system. */

/* $ Detailed_Output */

/*     DNEAR    is the 6-vector giving the position and velocity in */
/*              body-fixed coordinates of the point on the ellipsoid, */
/*              closest to the object whose position and velocity are */
/*              represented by STATE. */

/*              While the position component of DNEAR is always */
/*              meaningful, the velocity component of DNEAR will be */
/*              meaningless if FOUND if .FALSE. (See the discussion of */
/*              the meaning of FOUND below.) */

/*     DALT     is an array of two double precision numbers. The first */
/*              gives the altitude of STATE with respect to the */
/*              ellipsoid. The second gives the rate of change of the */
/*              altitude. */

/*              Note that the rate of change of altitude is meaningful if */
/*              and only if FOUND is .TRUE. (See the discussion of the */
/*              meaning of FOUND below.) */

/*     FOUND    is a logical flag indicating whether or not the velocity */
/*              portion of DNEAR is meaningful. If the velocity portion */
/*              of DNEAR is meaningful FOUND will be returned with a */
/*              value of .TRUE. Under very rare circumstance the velocity */
/*              of the near point is undefined. Under these circumstances */
/*              FOUND will be returned with the value .FALSE. */

/*              FOUND can be .FALSE. only for states whose position */
/*              components are inside the ellipsoid and then only at */
/*              points on a special surface contained inside the */
/*              ellipsoid called the focal set of the ellipsoid. */

/*              A point in the interior is on this special surface only */
/*              if there are two or more points on the ellipsoid that are */
/*              closest to it. The origin is such a point and the only */
/*              such point if the ellipsoid is a sphere. For */
/*              non-spheroidal ellipsoids the focal set contains small */
/*              portions of the planes of symmetry of the ellipsoid. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the axes are non-positive, an error is signaled by a */
/*         routine in the call tree of this routine. */

/*     2)  If an object is passing through the interior of an ellipsoid */
/*         there are points at which there is more than 1 point on the */
/*         ellipsoid that is closest to the object. At these points the */
/*         velocity of the near point is undefined. (See the description */
/*         of the output variable FOUND). */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     If an object is moving relative to some triaxial body along a */
/*     trajectory C(t) then there is a companion trajectory N(t) that */
/*     gives the point on the ellipsoid that is closest to C(t) as a */
/*     function of `t'. The instantaneous position and velocity of C(t), */
/*     STATE, are sufficient to compute the instantaneous position and */
/*     velocity of N(t), DNEAR. */

/*     This routine computes DNEAR from STATE. In addition it returns the */
/*     altitude and rate of change of altitude. */

/*     Note that this routine can compute DNEAR for STATE outside, on, */
/*     or inside the ellipsoid. However, the velocity of DNEAR and */
/*     derivative of altitude do not exist for a "small" set of STATE */
/*     in the interior of the ellipsoid. See the discussion of FOUND */
/*     above for a description of this set of points. */

/* $ Examples */

/*     The numerical results shown for these examples may differ across */
/*     platforms. The results depend on the SPICE kernels used as input, */
/*     the compiler and supporting libraries, and the machine specific */
/*     arithmetic implementation. */

/*     1) Suppose you wish to compute the velocity of the ground track */
/*        of a satellite as it passes over a location on Mars and that */
/*        the moment of passage has been previously determined. (We */
/*        assume that the spacecraft is close enough to the surface that */
/*        light time corrections do not matter.) */

/*        Use the meta-kernel shown below to load the required SPICE */
/*        kernels. */


/*           KPL/MK */

/*           File: dnearp_ex1.tm */

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
/*              de430.bsp                        Planetary ephemeris */
/*              mar097.bsp                       Mars satellite ephemeris */
/*              mro_psp4_ssd_mro95a.bsp          MRO ephemeris */

/*           \begindata */

/*              KERNELS_TO_LOAD = ( 'pck00010.tpc', */
/*                                  'naif0012.tls', */
/*                                  'de430.bsp', */
/*                                  'mar097.bsp', */
/*                                  'mro_psp4_ssd_mro95a.bsp' ) */

/*           \begintext */

/*           End of meta-kernel */


/*        Example code begins here. */


/*              PROGRAM DNEARP_EX1 */
/*              IMPLICIT NONE */

/*        C */
/*        C     SPICELIB functions */
/*        C */
/*              DOUBLE PRECISION      VNORM */

/*        C */
/*        C     Local parameters */
/*        C */
/*              CHARACTER*(*)         BODYNM */
/*              PARAMETER           ( BODYNM = 'MARS' ) */

/*              CHARACTER*(*)         META */
/*              PARAMETER           ( META   = 'dnearp_ex1.tm' ) */

/*        C */
/*        C     Local variables */
/*        C */
/*              DOUBLE PRECISION      A */
/*              DOUBLE PRECISION      B */
/*              DOUBLE PRECISION      C */
/*              DOUBLE PRECISION      DALT   ( 2 ) */
/*              DOUBLE PRECISION      DNEAR  ( 6 ) */
/*              DOUBLE PRECISION      ET */
/*              DOUBLE PRECISION      LT */
/*              DOUBLE PRECISION      RADII  ( 3 ) */
/*              DOUBLE PRECISION      STATE  ( 6 ) */
/*              DOUBLE PRECISION      GTVEL  ( 3 ) */

/*              INTEGER               DIM */

/*              LOGICAL               FOUND */

/*        C */
/*        C     Load kernel files via the meta-kernel. */
/*        C */
/*              CALL FURNSH ( META ) */

/*        C */
/*        C     Convert the TDB input time string to seconds past */
/*        C     J2000, TDB. */
/*        C */
/*              CALL STR2ET ( '2007 SEP 30 00:00:00 TDB', ET ) */

/*        C */
/*        C     First get the axes of the body. */
/*        C */
/*              CALL BODVRD ( BODYNM, 'RADII', 3, DIM, RADII ) */
/*              CALL VUPACK ( RADII, A, B, C ) */

/*        C */
/*        C     Get the geometric state of the spacecraft with */
/*        C     respect to BODYNM in the body-fixed reference frame */
/*        C     at ET and compute the state of the sub-spacecraft point. */
/*        C */
/*              CALL SPKEZR ( 'MRO',  ET,    'IAU_MARS', 'NONE', */
/*             .              BODYNM, STATE, LT                  ) */
/*              CALL DNEARP ( STATE, A, B, C, DNEAR, DALT, FOUND ) */

/*              IF ( FOUND ) THEN */

/*        C */
/*        C        DNEAR contains the state of the subspacecraft point. */
/*        C */
/*                 CALL VEQU ( DNEAR(4), GTVEL ) */

/*                 WRITE(*,'(A,3F10.6)') */
/*             .        'Ground-track velocity (km/s):', GTVEL */
/*                 WRITE(*,'(A,F10.6)') */
/*             .        'Ground-track speed    (km/s):', VNORM( GTVEL ) */

/*              ELSE */

/*                 WRITE(*,*) 'DNEAR is degenerate.' */

/*              END IF */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*        Ground-track velocity (km/s):  0.505252  1.986553 -2.475506 */
/*        Ground-track speed    (km/s):  3.214001 */


/*     2) Suppose you wish to compute the one-way doppler shift of a */
/*        radar mounted on board a spacecraft as it passes over some */
/*        region. Moreover, assume that for your purposes it is */
/*        sufficient to neglect effects of atmosphere, topography and */
/*        antenna pattern for the sake of this computation. */

/*        Use the meta-kernel from Example 1 above. */


/*        Example code begins here. */


/*              PROGRAM DNEARP_EX2 */
/*              IMPLICIT NONE */

/*        C */
/*        C     SPICELIB functions */
/*        C */
/*              DOUBLE PRECISION      CLIGHT */

/*        C */
/*        C     Local parameters */
/*        C */
/*              CHARACTER*(*)         BODYNM */
/*              PARAMETER           ( BODYNM = 'MARS' ) */

/*              CHARACTER*(*)         META */
/*              PARAMETER           ( META   = 'dnearp_ex1.tm' ) */

/*        C */
/*        C     Define the central frequency of the radar, */
/*        C     in megahertz. */
/*        C */
/*              DOUBLE PRECISION      RCFRQ */
/*              PARAMETER           ( RCFRQ  = 20.D0 ) */

/*        C */
/*        C     Local variables */
/*        C */
/*              DOUBLE PRECISION      A */
/*              DOUBLE PRECISION      B */
/*              DOUBLE PRECISION      C */
/*              DOUBLE PRECISION      DALT   ( 2 ) */
/*              DOUBLE PRECISION      DNEAR  ( 6 ) */
/*              DOUBLE PRECISION      ET */
/*              DOUBLE PRECISION      LT */
/*              DOUBLE PRECISION      RADII  ( 3 ) */
/*              DOUBLE PRECISION      SHIFT */
/*              DOUBLE PRECISION      STATE  ( 6 ) */

/*              INTEGER               DIM */

/*              LOGICAL               FOUND */

/*        C */
/*        C     Load kernel files via the meta-kernel. */
/*        C */
/*              CALL FURNSH ( META ) */

/*        C */
/*        C     Convert the TDB input time string to seconds past */
/*        C     J2000, TDB. */
/*        C */
/*              CALL STR2ET ( '2007 SEP 30 00:00:00 TDB', ET ) */

/*        C */
/*        C     First get the axes of the body. */
/*        C */
/*              CALL BODVRD ( BODYNM, 'RADII', 3, DIM, RADII ) */
/*              CALL VUPACK ( RADII, A, B, C ) */

/*        C */
/*        C     Get the geometric state of the spacecraft with */
/*        C     respect to BODYNM in the body-fixed reference frame */
/*        C     at ET and compute the state of the sub-spacecraft point. */
/*        C */
/*              CALL SPKEZR ( 'MRO',  ET,    'IAU_MARS', 'NONE', */
/*             .              BODYNM, STATE, LT                  ) */
/*              CALL DNEARP ( STATE, A, B, C, DNEAR, DALT, FOUND ) */

/*              IF ( FOUND ) THEN */

/*        C */
/*        C        The change in frequency is given by multiplying SHIFT */
/*        C        times the carrier frequency */
/*        C */
/*                 SHIFT = ( DALT(2) / CLIGHT() ) */
/*                 WRITE(*,'(A,F20.16)') 'Central frequency (MHz):', */
/*             .                          RCFRQ */
/*                 WRITE(*,'(A,F20.16)') 'Doppler shift     (MHz):', */
/*             .                          RCFRQ * SHIFT */

/*              ELSE */

/*                 WRITE(*,*) 'DNEAR is degenerate.' */

/*              END IF */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*        Central frequency (MHz): 20.0000000000000000 */
/*        Doppler shift     (MHz): -0.0000005500991159 */


/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     W.L. Taber         (JPL) */
/*     E.D. Wright        (JPL) */

/* $ Version */

/* -    SPICELIB Version 2.0.0, 26-OCT-2021 (JDR) (EDW) */

/*        Reimplemented routine using ZZDNPT. */

/*        Edited the header to comply with NAIF standard. Added complete */
/*        code examples, based on the existing code fragments. */

/* -    SPICELIB Version 1.1.2, 26-JUN-2008 (NJB) */

/*        Corrected spelling error in abstract; re-wrote */
/*        abstract text. */

/* -    SPICELIB Version 1.1.1, 24-OCT-2005 (NJB) */

/*        Header update: changed references to BODVAR to references */
/*        to BODVCD. */

/* -    SPICELIB Version 1.1.0, 05-MAR-1998 (WLT) */

/*        In the previous version of the routine FOUND could be */
/*        returned without being set to .TRUE. when the velocity */
/*        of the near point and rate of change of altitude */
/*        could be determined. This error has been corrected. */

/* -    SPICELIB Version 1.0.0, 15-JUN-1995 (WLT) */

/* -& */
/* $ Index_Entries */

/*     Velocity of the nearest point on an ellipsoid */
/*     Rate of change of the altitude over an ellipsoid */
/*     Derivative of altitude over an ellipsoid */
/*     Velocity of a ground track */

/* -& */

/*     Spicelib functions */


/*     Local Variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }
    chkin_("DNEARP", (ftnlen)6);

/*     Until we have reason to believe otherwise, we set FOUND to TRUE. */

    *found = TRUE_;

/*     First we need to compute the near point. */

    nearpt_(state, a, b, c__, dnear, dalt);

/*     Make sure nothing went bump in the dark innards of NEARPT. */

    if (failed_()) {
	*found = FALSE_;
	chkout_("DNEARP", (ftnlen)6);
	return 0;
    }

/*     Calculate the derivative of the near point and altitude. */

    zzdnpt_(state, dnear, a, b, c__, &dnear[3], &dalt[1], found);

/*     Check error status. Bail out if failure in call. */

    if (failed_()) {
	*found = FALSE_;
	chkout_("DNEARP", (ftnlen)6);
	return 0;
    }
    chkout_("DNEARP", (ftnlen)6);
    return 0;
} /* dnearp_ */

