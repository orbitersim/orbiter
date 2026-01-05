/* recrad.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure RECRAD ( Rectangular coordinates to RA and DEC ) */
/* Subroutine */ int recrad_(doublereal *rectan, doublereal *range, 
	doublereal *ra, doublereal *dec)
{
    extern doublereal twopi_(void);
    extern /* Subroutine */ int reclat_(doublereal *, doublereal *, 
	    doublereal *, doublereal *);

/* $ Abstract */

/*     Convert rectangular coordinates to range, right ascension, */
/*     and declination. */

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
/*     RANGE      O   Distance of the point from the origin. */
/*     RA         O   Right ascension in radians. */
/*     DEC        O   Declination in radians. */

/* $ Detailed_Input */

/*     RECTAN   are the rectangular coordinates of a point. */

/* $ Detailed_Output */

/*     RANGE    is the distance of the point from the origin. */

/*              The units associated with RANGE are those */
/*              associated with the input RECTAN. */


/*     RA       is the right ascension of RECTAN. This is the angular */
/*              distance measured toward the east from the prime */
/*              meridian to the meridian containing the input point. */
/*              The direction of increasing right ascension is from */
/*              the +X axis towards the +Y axis. */

/*              RA is output in radians. The range of RA is [0, 2*pi]. */


/*     DEC      is the declination of RECTAN. This is the angle from */
/*              the XY plane of the ray from the origin through the */
/*              point. */

/*              DEC is output in radians. The range of DEC is */
/*              [-pi/2, pi/2]. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/*     1)  If the X and Y components of RECTAN are both zero, the */
/*         right ascension is set to zero. */

/*     2)  If RECTAN is the zero vector, right ascension and declination */
/*         are both set to zero. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine returns the range, right ascension, and declination */
/*     of a point specified in rectangular coordinates. */

/*     The output is defined by a distance from a central reference */
/*     point, an angle from a reference meridian, and an angle above */
/*     the equator of a sphere centered at the central reference */
/*     point. */

/* $ Examples */

/*     The following code fragment converts right ascension and */
/*     declination from the B1950 reference frame to the J2000 frame. */

/*        C */
/*        C     Convert RA and DEC to a 3-vector expressed in */
/*        C     the B1950 frame. */
/*        C */
/*              CALL RADREC ( 1.D0, RA, DEC, V1950 ) */
/*        C */
/*        C     We use the SPICELIB routine PXFORM to obtain the */
/*        C     transformation  matrix for converting vectors between */
/*        C     the B1950 and J2000 reference frames. Since */
/*        C     both frames are inertial, the input time value we */
/*        C     supply to PXFORM is arbitrary. We choose zero */
/*        C     seconds past the J2000 epoch. */
/*        C */
/*              CALL PXFORM ( 'B1950', 'J2000', 0.D0, MTRANS ) */
/*        C */
/*        C     Transform the vector to the J2000 frame. */
/*        C */
/*              CALL MXV ( MTRANS, V1950, V2000 ) */
/*        C */
/*        C     Find the RA and DEC of the J2000-relative vector. */
/*        C */
/*              CALL RECRAD ( V2000, R, RA, DEC ) */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     C.H. Acton         (JPL) */
/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     H.A. Neilan        (JPL) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 12-AUG-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 1.0.2, 30-JUL-2003 (NJB) (CHA) */

/*        Various header changes were made to improve clarity. Some */
/*        minor header corrections were made. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (HAN) */

/* -& */
/* $ Index_Entries */

/*     rectangular coordinates to ra and dec */
/*     rectangular to right_ascension and declination */

/* -& */

/*     SPICELIB functions */


/*     Call the subroutine RECLAT to convert the rectangular coordinates */
/*     into latitudinal coordinates.  In RECLAT, the longitude ( which */
/*     is returned to this subroutine as RA ) ranges from - pi to pi */
/*     radians.   Because the right ascension ranges from zero to */
/*     two pi radians, whenever RA is negative two pi must be added to */
/*     it. */

    reclat_(rectan, range, ra, dec);
    if (*ra < 0.) {
	*ra += twopi_();
    }
    return 0;
} /* recrad_ */

