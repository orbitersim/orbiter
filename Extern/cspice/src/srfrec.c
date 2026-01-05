/* srfrec.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static doublereal c_b4 = 1.;

/* $Procedure SRFREC ( Surface to rectangular coordinates ) */
/* Subroutine */ int srfrec_(integer *body, doublereal *lon, doublereal *lat, 
	doublereal *rectan)
{
    doublereal uvec[3];
    extern /* Subroutine */ int zzgftreb_(integer *, doublereal *);
    doublereal radii[3];
    extern /* Subroutine */ int chkin_(char *, ftnlen), edpnt_(doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *);
    extern logical failed_(void);
    extern /* Subroutine */ int latrec_(doublereal *, doublereal *, 
	    doublereal *, doublereal *), chkout_(char *, ftnlen);
    extern logical return_(void);

/* $ Abstract */

/*     Convert planetocentric latitude and longitude of a surface */
/*     point on a specified body to rectangular coordinates. */

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

/*     KERNEL */
/*     NAIF_IDS */

/* $ Keywords */

/*     CONVERSION */
/*     COORDINATES */
/*     TRANSFORMATION */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     BODY       I   NAIF integer code of an extended body. */
/*     LON        I   Longitude of point in radians. */
/*     LAT        I   Latitude of point in radians. */
/*     RECTAN     O   Rectangular coordinates of the point. */

/* $ Detailed_Input */

/*     BODY     is the NAIF integer code of an extended body on which */
/*              a surface point of interest is located. The body is */
/*              modeled as a triaxial ellipsoid. */

/*     LON      is the longitude of the input point. This is the */
/*              angle between the prime meridian and the meridian */
/*              containing the point. The direction of increasing */
/*              longitude is from the +X axis towards the +Y axis. */

/*              Longitude is measured in radians. On input, the */
/*              range of longitude is unrestricted. */

/*     LAT      is the latitude of the input point. This is the angle */
/*              from the XY plane of the ray from the origin through */
/*              the point. */

/*              Latitude is measured in radians. On input, the range */
/*              of latitude is unrestricted. */

/* $ Detailed_Output */

/*     RECTAN   are the rectangular coordinates of the input surface */
/*              point. Units are the same as those used to define the */
/*              radii of BODY. Normally, these units are km. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If radii for BODY are not found in the kernel pool, an error */
/*         is signaled by a routine in the call tree of this routine. */

/*     2)  If the size of the BODY body radii kernel variable is not */
/*         three, an error is signaled by a routine in the call tree of */
/*         this routine. */

/*     3)  If any of the three BODY body radii is less-than or equal to */
/*         zero, an error is signaled by a routine in the call tree of */
/*         this routine. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine returns the rectangular coordinates of a surface */
/*     point on an extended body with known radii, where the location */
/*     of the surface point is specified in planetocentric latitudinal */
/*     coordinates. */

/*     Latitudinal coordinates are defined by a distance from a central */
/*     reference point, an angle from a reference meridian, and an angle */
/*     above the equator of a sphere centered at the central reference */
/*     point. In this case, the distance from the central reference */
/*     point is not required as an input because the fact that the */
/*     point is on the body's surface allows one to deduce this quantity. */

/*     Below are two tables that demonstrate by example the relationship */
/*     between rectangular and latitudinal coordinates. */

/*     Listed in the first table (under R, LON and LAT) are */
/*     latitudinal coordinate triples that approximately represent */
/*     points whose rectangular coordinates are taken from the set */
/*     {-1, 0, 1}.  (Angular quantities are given in degrees.) */


/*          R          LON       LAT    RECTAN(1)   RECTAN(2)  RECTAN(3) */
/*         --------------------------   -------------------------------- */
/*         0.0000    0.0000    0.0000      0.0000      0.0000     0.0000 */
/*         1.0000    0.0000    0.0000      1.0000      0.0000     0.0000 */
/*         1.0000   90.0000    0.0000      0.0000      1.0000     0.0000 */
/*         1.0000    0.0000   90.0000      0.0000      0.0000     1.0000 */
/*         1.0000  180.0000    0.0000     -1.0000      0.0000     0.0000 */
/*         1.0000  -90.0000    0.0000      0.0000     -1.0000     0.0000 */
/*         1.0000    0.0000  -90.0000      0.0000      0.0000    -1.0000 */
/*         1.4142   45.0000    0.0000      1.0000      1.0000     0.0000 */
/*         1.4142    0.0000   45.0000      1.0000      0.0000     1.0000 */
/*         1.4142   90.0000   45.0000      0.0000      1.0000     1.0000 */
/*         1.7320   45.0000   35.2643      1.0000      1.0000     1.0000 */


/*     This routine is related to the SPICELIB routine LATREC, which */
/*     accepts a radius, longitude, and latitude as inputs and produces */
/*     equivalent rectangular coordinates as outputs. */

/* $ Examples */

/*     The numerical results shown for this example may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */

/*     1) Find the rectangular coordinates of the point */

/*           100 degrees planetocentric longitude */
/*           -35 degrees planetocentric latitude */

/*        on the Earth; then convert these coordinates back to */
/*        latitudinal coordinates. We should be able to recover */
/*        our original longitude and latitude values. */

/*        Use the PCK kernel below to load the required triaxial */
/*        ellipsoidal shape model and orientation data for the Earth. */

/*           pck00008.tpc */


/*        Example code begins here. */


/*              PROGRAM SRFREC_EX1 */
/*              IMPLICIT NONE */

/*        C */
/*        C     SPICELIB functions */
/*        C */
/*              DOUBLE PRECISION      DPR */
/*              DOUBLE PRECISION      RPD */

/*        C */
/*        C     Local variables */
/*        C */
/*              DOUBLE PRECISION      LAT */
/*              DOUBLE PRECISION      LON */
/*              DOUBLE PRECISION      X     ( 3 ) */
/*              DOUBLE PRECISION      RADIUS */

/*        C */
/*        C     Load the kernel pool with a PCK file that contains */
/*        C     values for the radii of the Earth. */
/*        C */
/*              CALL FURNSH ( 'pck00008.tpc' ) */

/*        C */
/*        C     Find X, the rectangular coordinates of the */
/*        C     surface point defined by LAT and LON.  The */
/*        C     NAIF integer code for the Earth is 399. */
/*        C     (See the NAIF_IDS required reading file for */
/*        C     the complete set of codes.) */
/*        C */
/*              LON  =  100.D0 */
/*              LAT   =  -35.D0 */

/*              WRITE (*,*) 'Original latitudinal coordinates' */
/*              WRITE (*,*) ' ' */
/*              WRITE (*,*) '  Longitude (deg) = ', LON */
/*              WRITE (*,*) '  Latitude  (deg) = ', LAT */

/*        C */
/*        C     Convert angles to radians on input to SRFREC. */
/*        C */
/*              CALL SRFREC ( 399, LON*RPD(), LAT*RPD(), X ) */

/*              WRITE (*,*) ' ' */
/*              WRITE (*,*) 'Rectangular coordinates ' */
/*              WRITE (*,*) ' ' */
/*              WRITE (*,*) '  X (km)          = ', X(1) */
/*              WRITE (*,*) '  Y (km)          = ', X(2) */
/*              WRITE (*,*) '  Z (km)          = ', X(3) */

/*        C */
/*        C     Now try to recover the original latitudinal */
/*        C     coordinates from the rectangular coordinates */
/*        C     found by SRFREC. */
/*        C */
/*              CALL RECLAT ( X, RADIUS, LON, LAT ) */

/*        C */
/*        C     Convert angles to degrees for display. */
/*        C */
/*              WRITE (*,*) ' ' */
/*              WRITE (*,*) 'Latitudinal coordinates recovered '   // */
/*             .            'from rectangular coordinates' */
/*              WRITE (*,*) ' ' */
/*              WRITE (*,*) '  Longitude (deg) = ', LON * DPR() */
/*              WRITE (*,*) '  Latitude  (deg) = ', LAT * DPR() */
/*              WRITE (*,*) '  Radius    (km)  = ', RADIUS */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*         Original latitudinal coordinates */

/*           Longitude (deg) =    100.00000000000000 */
/*           Latitude  (deg) =   -35.000000000000000 */

/*         Rectangular coordinates */

/*           X (km)          =   -906.24942866761364 */
/*           Y (km)          =    5139.5959088415748 */
/*           Z (km)          =   -3654.3008396462560 */

/*         Latitudinal coordinates recovered from rectangular coordinates */

/*           Longitude (deg) =    100.00000000000000 */
/*           Latitude  (deg) =   -35.000000000000000 */
/*           Radius    (km)  =    6371.0790891167535 */


/* $ Restrictions */

/*     1)  A PCK text kernel containing the body radius definitions */
/*         required by this routine must be loaded into the kernel */
/*         pool prior to any calls to this routine. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     W.L. Taber         (JPL) */
/*     E.D. Wright        (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.3.0, 01-NOV-2021 (JDR) (EDW) */

/*        Body radii accessed from kernel pool using ZZGFTREB. */

/*        Changed the input argument name LONG to LON for consistency */
/*        with other routines. */

/*        Edited the header to comply with NAIF standard. Modified code */
/*        example output format. Minor edits to header. */

/* -    SPICELIB Version 1.2.0, 19-APR-2016 (NJB) */

/*        Re-implemented ellipsoid surface point computation */
/*        using EDPNT. */

/* -    SPICELIB Version 1.1.0, 03-NOV-2005 (NJB) */

/*        Call to BODVAR was replaced with call to BODVCD. */

/*        Various header updates were made to clarify description */
/*        of routine's functionality. Example program was updated */
/*        as well. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 03-SEP-1991 (NJB) (WLT) */

/* -& */
/* $ Index_Entries */

/*     convert body-fixed latitudinal coordinates to rectangular */
/*     convert surface latitudinal coordinates to rectangular */
/*     surface point latitudinal coordinates to rectangular */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }
    chkin_("SRFREC", (ftnlen)6);

/*     Look up the body's radii. */

    zzgftreb_(body, radii);
    if (failed_()) {
	chkout_("SRFREC", (ftnlen)6);
	return 0;
    }

/*     Find the unit vector pointing from the body center to the */
/*     input surface point. */

    latrec_(&c_b4, lon, lat, uvec);

/*     Find out where the ray defined by this vector intersects the */
/*     surface.  This intercept is the point we're looking for. */

    edpnt_(uvec, radii, &radii[1], &radii[2], rectan);
    chkout_("SRFREC", (ftnlen)6);
    return 0;
} /* srfrec_ */

