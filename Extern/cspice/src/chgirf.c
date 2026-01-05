/* chgirf.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static doublereal c_b6 = 0.;
static integer c__1 = 1;
static integer c__9 = 9;
static integer c__21 = 21;

/* $Procedure CHGIRF ( Change inertial reference frames ) */
/* Subroutine */ int chgirf_0_(int n__, integer *refa, integer *refb, 
	doublereal *rotab, char *name__, integer *index, ftnlen name_len)
{
    /* Initialized data */

    static logical ready = FALSE_;
    static char frames[16*21] = "J2000           " "B1950           " "FK4  "
	    "           " "DE-118          " "DE-96           " "DE-102      "
	    "    " "DE-108          " "DE-111          " "DE-114          " 
	    "DE-122          " "DE-125          " "DE-130          " "GALACT"
	    "IC        " "DE-200          " "DE-202          " "MARSIAU      "
	    "   " "ECLIPJ2000      " "ECLIPB1950      " "DE-140          " 
	    "DE-142          " "DE-143          ";
    static char bases[16*21] = "J2000           " "J2000           " "B1950 "
	    "          " "B1950           " "B1950           " "B1950        "
	    "   " "B1950           " "B1950           " "B1950           " 
	    "B1950           " "B1950           " "B1950           " "FK4   "
	    "          " "J2000           " "J2000           " "J2000        "
	    "   " "J2000           " "B1950           " "J2000           " 
	    "J2000           " "J2000           ";
    static char defs[80*21] = "0.0  1                                       "
	    "                                   " "1152.84248596724 3  -1002."
	    "26108439117  2  1153.04066200330  3                   " "0.525  "
	    "3                                                               "
	    "         " "0.53155  3                                          "
	    "                            " "0.4107  3                        "
	    "                                               " "0.1359  3     "
	    "                                                                "
	    "  " "0.4775  3                                                  "
	    "                     " "0.5880  3                               "
	    "                                        " "0.5529  3            "
	    "                                                           " 
	    "0.5316  3                                                      "
	    "                 " "0.5754  3                                   "
	    "                                    " "0.5247  3                "
	    "                                                       " "117720"
	    "0.0  3  225360.0  1  1016100.0  3                               "
	    "          " "0.0  3                                             "
	    "                             " "0.0  3                          "
	    "                                                " "324000.0D0 3 "
	    "133610.4D0 2 -152348.4D0 3                                      "
	    "   " "84381.448 1                                               "
	    "                      " "84404.836 1                            "
	    "                                         " "1152.71013777252 3  "
	    "-1002.25042010533  2  1153.75719544491  3                   " 
	    "1152.72061453864 3  -1002.25052830351  2  1153.74663857521  3  "
	    "                 " "1153.03919093833, 3, -1002.24822382286, 2, 1"
	    "153.42900222357, 3                  ";
    static integer dframe = 0;

    /* System generated locals */
    integer i__1, i__2;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer), s_cmp(char *, char *, 
	    ftnlen, ftnlen);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    static integer axis;
    static char word[25];
    extern /* Subroutine */ int mxmt_(doublereal *, doublereal *, doublereal *
	    );
    static integer b, i__, j, p;
    static doublereal angle;
    extern /* Subroutine */ int chkin_(char *, ftnlen), moved_(doublereal *, 
	    integer *, doublereal *);
    extern integer wdcnt_(char *, ftnlen);
    extern /* Subroutine */ int nthwd_(char *, integer *, char *, integer *, 
	    ftnlen, ftnlen);
    static doublereal trans[189]	/* was [9][21] */;
    static char error[25];
    extern logical eqstr_(char *, char *, ftnlen, ftnlen);
    static doublereal radang;
    extern integer esrchc_(char *, integer *, char *, ftnlen, ftnlen), 
	    isrchc_(char *, integer *, char *, ftnlen, ftnlen);
    extern /* Subroutine */ int nparsd_(char *, doublereal *, char *, integer 
	    *, ftnlen, ftnlen), sigerr_(char *, ftnlen), nparsi_(char *, 
	    integer *, char *, integer *, ftnlen, ftnlen), chkout_(char *, 
	    ftnlen), rotate_(doublereal *, integer *, doublereal *);
    static doublereal tmpmat[9]	/* was [3][3] */;
    extern /* Subroutine */ int setmsg_(char *, ftnlen), errint_(char *, 
	    integer *, ftnlen), rotmat_(doublereal *, doublereal *, integer *,
	     doublereal *), convrt_(doublereal *, char *, char *, doublereal *
	    , ftnlen, ftnlen);
    extern logical return_(void);
    static integer loc;
    extern /* Subroutine */ int mxm_(doublereal *, doublereal *, doublereal *)
	    ;

/* $ Abstract */

/*     Support changes among a standard set of inertial coordinate */
/*     reference frames. */

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

/*     FRAMES */

/* $ Keywords */

/*     CONVERSION */
/*     COORDINATES */
/*     EPHEMERIS */
/*     FRAMES */
/*     MATRIX */
/*     ROTATION */
/*     TRANSFORMATION */
/*     VECTOR */

/* $ Declarations */
/* $ Abstract */

/*     This file contains the number of inertial reference */
/*     frames that are currently known by the SPICE toolkit */
/*     software. */

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

/*     FRAMES */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     NINERT     P   Number of known inertial reference frames. */

/* $ Parameters */

/*     NINERT     is the number of recognized inertial reference */
/*                frames.  This value is needed by both CHGIRF */
/*                ZZFDAT, and FRAMEX. */

/* $ Author_and_Institution */

/*     W.L. Taber      (JPL) */

/* $ Literature_References */

/*     None. */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 10-OCT-1996 (WLT) */

/* -& */
/* $ Brief_I/O */

/*     VARIABLE  I/O  ENTRY POINTS */
/*     --------  ---  -------------------------------------------------- */
/*     REFA       I   IRFROT */
/*     REFB       I   IRFROT */
/*     ROTAB      O   IRFROT */
/*     NAME      I-O  IRFNUM, IRFNAM, IRFDEF */
/*     INDEX     I-O  IRFNUM, IRFNAM */

/* $ Detailed_Input */

/*     See entry points IRFROT, IRFNUM, IRFNAM, and IRFDEF. */

/* $ Detailed_Output */

/*     See entry points IRFROT, IRFNUM, and IRFNAM. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If CHGIRF is called directly, the signal, the error */
/*         SPICE(BOGUSENTRY) is signaled. */

/*     2)  See entry points IRFROT, IRFNUM, IRFNAM, and IRFDEF for */
/*         exceptions specific to those routines. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     CHGIRF exists only as an umbrella for data to be shared */
/*     by its entry points (IRFROT, IRFNUM, IRFNAM, and IRFDEF). */
/*     It should never be called directly. */

/* $ Examples */

/*     See entry points IRFROT, IRFNUM, IRFNAM, and IRFDEF. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     [1]  J. Lieske, "Precession Matrix Based on IAU (1976) System of */
/*          Astronomical Constants," Astron. Astrophys. 73, 282-284, */
/*          1979. */

/*     [2]  E. M. Standish, Jr., "Orientation of the JPL Ephemerides, */
/*          DE 200/LE 200, to the Dynamical Equinox of J2000," Astron. */
/*          Astrophys. 114, 297-302, 1982. */

/*     [3]  E. M. Standish, Jr., "Conversion of Ephemeris Coordinates */
/*          from the B1950 System to the J2000 System," JPL IOM */
/*          314.6-581, 24 June 1985. */

/*     [4]  E. M. Standish, Jr., "The Equinox Offsets of the JPL */
/*          Ephemeris," JPL IOM 314.6-929, 26 February 1988. */

/*     [5]  J. Lieske, "Expressions for the Precession Quantities Based */
/*          upon the IAU (1976) System of Astronomical Constants," */
/*          Astron. Astrophys. 58, 1-16, 1977. */

/*     [6]  L. Bass and R. Cesarone, "Mars Observer Planetary Constants */
/*          and Models," JPL D-3444, November 1990. */

/*     [7]  P. Kenneth Seidelmann (Ed.), "Explanatory Supplement to the */
/*          Astronomical Almanac," University Science Books, 1992. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     B.V. Semenov       (JPL) */
/*     W.L. Taber         (JPL) */
/*     I.M. Underwood     (JPL) */
/*     E.D. Wright        (JPL) */

/* $ Version */

/* -    SPICELIB Version 4.5.0, 06-NOV-2021 (JDR) (NJB) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. */

/*        Corrected sign error in comments describing the angles used to */
/*        convert between the J2000 and B1950 reference frames. Also */
/*        corrected a typo in the comments. */

/* -    SPICELIB Version 4.4.0, 24-SEP-2013 (BVS) */

/*        Updated entry point IRFNUM to treat J2000 as a special case */
/*        and to not participate of CHKIN/CHOUT to increase efficiency. */

/* -    SPICELIB Version 4.3.0, 25-AUG-2005 (NJB) */

/*        Updated to remove non-standard use of duplicate arguments */
/*        in CONVRT, ROTMAT and MXM calls. */

/* -    SPICELIB Version 4.2.1, 04-JAN-2002 (EDW) */

/*        Added DE-143 to header description for IRFROT. */

/* -    SPICELIB Version 4.2.0, 10-APR-1997 (WLT) */

/*        A descriptive diagnostic was added to the entry points */
/*        IRFROT and IRFDEF. Before they simply signaled the error */
/*        with no diagnostic. */

/* -    SPICELIB Version 4.1.0, 14-OCT-1996 (WLT) */

/*        The number of inertial frames recognized is now stored */
/*        in the include file ninert.inc. */

/* -    SPICELIB Version 4.0.0, 20-MAY-1996 (WLT) */

/*        The inertial frame DE-143 was added to the list of recognized */
/*        inertial frames. */

/* -    SPICELIB Version 3.0.0, 20-MAR-1995 (WLT) */

/*        The inertial frames DE-140 and DE-142  were added to the */
/*        list of recognized inertial frames. */

/* -    SPICELIB Version 2.0.0, 30-JUL-1993 (WLT) */

/*        The transformation from J2000 to B1950 was upgraded */
/*        so that the transformation matrix produced matches */
/*        the matrix given in [1]. */

/*        The frame MARSIAU was added to the list */
/*        of recognized frames. This is the standard mars */
/*        referenced inertial frame used by the Mars Observer */
/*        project. */

/*        Values for the obliquity of the ecliptic were taken */
/*        from the Explanatory Supplement [7] to the Astronomical */
/*        Almanac (1992) at both the epochs J2000 and B1950 and */
/*        used to define the mean ecliptic and equinox frames */
/*        ECLIPJ2000 and ECLIPB1950. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (IMU) */

/* -& */
/* $ Index_Entries */

/*     change inertial reference frames */

/* -& */
/* $ Revisions */

/* -    SPICELIB Version 4.3.0, 25-AUG-2005 (NJB) */

/*        Updated to remove non-standard use of duplicate arguments */
/*        in CONVRT, ROTMAT  and MXM calls. */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     Local variables */


/*     Saved variables */


/*     Initial values */


/*     Each frame is defined in terms of another frame, except for */
/*     the root frame, which is defined in terms of itself. For now, */
/*     the root frame is the standard IAU reference frame, J2000, */
/*     defined by the Earth mean equator and dynamical equinox of */
/*     Julian year 2000. */

/*     Each definition consists of a series of rotations, each */
/*     through some angle (in arc seconds) and about some axis. */
/*     The rotations are listed in the opposite order in which */
/*     they are to be performed, so as to correspond more closely */
/*     to conventional notation. For example, the definition */

/*        FRAMES(i) = 'F2' */
/*        BASES(i)  = 'F1' */
/*        DEFS(i)   = '22.34  3   31.21  2   0.449  1' */

/*     means that a vector in frame F1 is converted to the equivalent */
/*     vector in frame F2 by applying the following rotation: */

/*        -                                            - */
/*        v    = ( [ 22.34 ]  [ 31.21 ]  [ 0.449 ]  )  v */
/*         F2               3          2          1     F1 */

/*     where the notation */

/*        [ theta ] */
/*                 a */

/*     means ``rotate through angle theta about axis a.'' */

/*     New frames may be added by: */

/*        1) Increasing the value of MAXF. */

/*        2) Adding new values for FRAMES, BASES, and DEFS. */

/*     The actual transformations (TRANS) will be computed during */
/*     initialization. */

/*     Note that BASES must be the name of a previously defined */
/*     reference frame, and that no frame should appear more than */
/*     once in FRAMES. */

/*     Note also that the list of valid reference frames maintained */
/*     by CHGIRF must be updated whenever new frames are added. */

    /* Parameter adjustments */
    if (rotab) {
	}

    /* Function Body */
    switch(n__) {
	case 1: goto L_irfrot;
	case 2: goto L_irfnum;
	case 3: goto L_irfnam;
	case 4: goto L_irfdef;
	}


/*     The root frame is mostly for show. Rotate by 0 arc seconds */
/*     about the x-axis to obtain the identity matrix. */


/*     The B1950 reference frame is obtained by precessing the J2000 */
/*     frame backwards from Julian year 2000 to Besselian year 1950, */
/*     using the 1976 IAU precession model. */

/*     The rotation from B1950 to J2000 is */

/*        [ -z ]  [ theta ]  [ -zeta ] */
/*              3          2          3 */

/*     So the rotation from J2000 to B1950 is the transpose, */

/*        [ zeta ]  [ -theta ]  [ z ] */
/*                3           2      3 */

/*     The values for z, theta, and zeta are computed from the formulas */
/*     given in table 5 of [5]. */

/*        z     =  1153.04066200330" */
/*        theta =  1002.26108439117" */
/*        zeta  =  1152.84248596724" */


/*     The FK4 reference frame is derived from the B1950 frame by */
/*     applying the equinox offset determined by Fricke. This is just */
/*     the rotation */

/*        [ 0.525" ] */
/*                  3 */


/*     The DE-118 reference frame is nearly identical to the FK4 */
/*     reference frame. It is also derived from the B1950 frame. */
/*     Only the offset is different: */

/*        [ 0.53155" ] */
/*                    3 */

/*     In [2], Standish uses two separate rotations, */

/*        [ 0.00073" ]  P [ 0.5316" ] */
/*                    3              3 */

/*     (where P is the precession matrix used above to define the */
/*     B1950 frame). The major effect of the second rotation is to */
/*     correct for truncating the magnitude of the first rotation. */
/*     At his suggestion, we will use the untruncated value, and */
/*     stick to a single rotation. */


/*     Most of the other DE reference frames may be defined relative */
/*     to either the DE-118 or B1950 frames. The values below are taken */
/*     from [4]. */

/*        DE number   Offset from DE-118   Offset from B1950 */
/*        ---------   ------------------   ----------------- */
/*               96             +0.1209"            +0.4107" */
/*              102             +0.3956"            +0.1359" */
/*              108             +0.0541"            +0.4775" */
/*              111             -0.0564"            +0.5880" */
/*              114             -0.0213"            +0.5529" */
/*              122             +0.0000"            +0.5316" */
/*              125             -0.0438"            +0.5754" */
/*              130             +0.0069"            +0.5247" */

/*     We will use B1950 for now, since the offsets generally have */
/*     more significant digits. */


/*     The Galactic System II reference frame is defined by the */
/*     following rotations: */

/*             o          o            o */
/*        [ 327  ]  [ 62.6  ]  [ 282.25  ] */
/*                3          1            3 */

/*      In the absence of better information, we will assume that */
/*      it is derived from the FK4 frame. Converting the angles from */
/*      degrees to arc seconds, */

/*           o */
/*        327      = 1177200" */
/*            o */
/*        62.6     =  225360" */
/*              o */
/*        282.25   = 1016100" */


/*     According to Standish, the various DE-200 frames are identical */
/*     with J2000, because he rotates the ephemerides before releasing */
/*     them (in order to avoid problems like the one that this routine */
/*     is designed to solve). Because we have to have something, we */
/*     will use */

/*             o */
/*        [ 0.0 ] */
/*               3 */


/*     The values for the transformation from J2000 to MARSIAU_MO */
/*     are derived from the constants given for the pole of Mars */
/*     on page 8-2 of reference [6]. */


/*     The value for the obliquity of the ecliptic at J2000  is */
/*     taken from page  114 of [7] equation 3.222-1.  This agrees */
/*     with the expression given in [5] */


/*     The value for the obliquity of the ecliptic at B1950  is */
/*     taken from page  171 of [7]. */


/*     The frame for DE-140 is simply DE-400 rotated by the rotation: */

/*      0.9999256765384668  0.0111817701197967  0.0048589521583895 */
/*     -0.0111817701797229  0.9999374816848701 -0.0000271545195858 */
/*     -0.0048589520204830 -0.0000271791849815  0.9999881948535965 */

/*     Note that the DE-400 frame is J2000. */

/*     The transpose of this is the frame from DE140 to DE400. To get */
/*     the euler angles below, the matrix given above was copied into */
/*     a matrix XFORM. */

/*     This matrix was transposed to give the transformation from */
/*     DE-140 to J2000. */

/*        CALL XPOSE ( XFORM, XFORM ) */

/*     Using the SPICE routine M2EUL, the euler representation of the */
/*     transformation from DE140 to J2000 was constructed. */

/*        CALL M2EUL ( XFORM, 3, 2, 3, A1, A2, A3 ) */

/*     Angles were converted to the range from -180 to 180 degrees */
/*     and converted to arcseconds.  At this point we have the */
/*     euler representation from DE-140 to J2000. */

/*        [ A1 ]  [ A2 ]  [ A3 ] */
/*              3       2       3 */

/*     To get the Euler representation of the transformation from */
/*     J2000 to DE-140  we use. */

/*        [ -A3 ]  [ -A2 ] [ -A1 ] */
/*               3        2       3 */

/*     This method was used because it yields a nicer form of */
/*     representation than the straight forward transformation. */
/*     Note that these numbers are quite close to the values used */
/*     for the transformation from J2000 to B1950 */


/*     The frame for DE-142 is simply DE-402 rotated by the rotation: */

/*      0.9999256765402605  0.0111817697320531  0.0048589526815484 */
/*     -0.0111817697907755  0.9999374816892126 -0.0000271547693170 */
/*     -0.0048589525464121 -0.0000271789392288  0.9999881948510477 */

/*     Note that the DE-402 frame is J2000. */

/*     The Euler angles giving the transformation for J2000 to */
/*     DE-142 were constructed in the same way as the transformation */
/*     from J2000 to DE140.  Only the input matrix changed to use the */
/*     one given above. */


/*     The frame for DE-143 is simply DE-403 rotated by the rotation: */

/*      0.9999256765435852  0.0111817743077255  0.0048589414674762 */
/*     -0.0111817743300355  0.9999374816382505 -0.0000271622115251 */
/*     -0.0048589414161348 -0.0000271713942366  0.9999881949053349 */

/*     Note that the DE-403 frame is J2000. */

/*     The Euler angles giving the transformation for J2000 to */
/*     DE-143 were constructed in the same way as the transformation */
/*     from J2000 to DE140.  Only the input matrix changed to use the */
/*     one given above. */


/*     Until defined (by a call to IRFDEF), the default frame is */
/*     undefined. */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("CHGIRF", (ftnlen)6);
    }
    sigerr_("SPICE(BOGUSENTRY)", (ftnlen)17);
    chkout_("CHGIRF", (ftnlen)6);
    return 0;
/* $Procedure IRFROT ( Inertial reference frame rotation ) */

L_irfrot:
/* $ Abstract */

/*     Compute the matrix needed to rotate vectors between two */
/*     standard inertial reference frames. */

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

/*     FRAMES */

/* $ Keywords */

/*     CONVERSION */
/*     COORDINATES */
/*     EPHEMERIS */
/*     FRAMES */
/*     MATRIX */
/*     ROTATION */
/*     TRANSFORMATION */
/*     VECTOR */

/* $ Declarations */

/*     INTEGER               REFA */
/*     INTEGER               REFB */
/*     DOUBLE PRECISION      ROTAB    ( 3, 3 ) */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     REFA, */
/*     REFB       I   Indices of target reference frames (A,B). */
/*     ROTAB      O   Rotation from frame A to frame B. */

/* $ Detailed_Input */

/*     REFA, */
/*     REFB     are the indices of two standard inertial reference */
/*              frames. The complete set of supported frames is shown */
/*              below. */

/*                 Index  Name      Description */
/*                 -----  --------  -------------------------------- */
/*                  1    J2000      Earth mean equator, dynamical */
/*                                  equinox of J2000 */

/*                  2    B1950      Earth mean equator, dynamical */
/*                                  equinox of B1950 */

/*                  3    FK4        Fundamental Catalog (4) */

/*                  4    DE-118     JPL Developmental Ephemeris (118) */

/*                  5    DE-96      JPL Developmental Ephemeris ( 96) */

/*                  6    DE-102     JPL Developmental Ephemeris (102) */

/*                  7    DE-108     JPL Developmental Ephemeris (108) */

/*                  8    DE-111     JPL Developmental Ephemeris (111) */

/*                  9    DE-114     JPL Developmental Ephemeris (114) */

/*                 10    DE-122     JPL Developmental Ephemeris (122) */

/*                 11    DE-125     JPL Developmental Ephemeris (125) */

/*                 12    DE-130     JPL Developmental Ephemeris (130) */

/*                 13    GALACTIC   Galactic System II */

/*                 14    DE-200     JPL Developmental Ephemeris (200) */

/*                 15    DE-202     JPL Developmental Ephemeris (202) */

/*                 16    MARSIAU    Mars Observer inertial frame */
/*                                  defined relative to MARS. */

/*                 17    ECLIPJ2000 Earth mean ecliptic and equinox */
/*                                  of the epoch J2000 */

/*                 18    ECLIPB1950 Earth mean ecliptic and equinox */
/*                                  of the Besselian date 1950. */

/*                 19    DE-140    JPL Developmental Ephemeris (140) */

/*                 20    DE-142    JPL Developmental Ephemeris (142) */

/*                 21    DE-143    JPL Developmental Ephemeris (143) */

/* $ Detailed_Output */

/*     ROTAB    is the rotation which, when applied to a vector v */
/*              in reference frame A, */
/*                 _            _ */
/*                 v  = (ROTAB) v */
/*                  B            A */

/*              yields the same vector in reference frame B. The */
/*              inverse rotation is performed by applying the */
/*              transpose, */
/*                 _           T _ */
/*                 v  = (ROTAB)  v */
/*                  A             B */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If either REFA or REFB is outside the range [1,MAXF], */
/*         where MAXF is the number of supported frames, the error */
/*         SPICE(IRFNOTREC) is signaled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     IRFROT exists primarily for use by the ephemeris and star */
/*     catalog readers in the SPICELIB toolkit library. */

/* $ Examples */

/*     In the following code fragment, IRFROT is used to rotate */
/*     vectors originally referenced to the DE-118 coordinate frame */
/*     to equivalent vectors referenced to the IAU standard J2000 */
/*     reference frame. */

/*        CALL IRFROT ( 4, 1, R ) */

/*        CALL MXV ( R, SC1950, SC2000 ) */
/*        CALL MXV ( R, MP1950, MP2000 ) */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     [1]  J. Lieske, "Precession Matrix Based on IAU (1976) System of */
/*          Astronomical Constants," Astron. Astrophys. 73, 282-284, */
/*          1979. */

/*     [2]  E. M. Standish, Jr., "Orientation of the JPL Ephemerides, */
/*          DE 200/LE 200, to the Dynamical Equinox of J2000," Astron. */
/*          Astrophys. 114, 297-302, 1982. */

/*     [3]  E. M. Standish, Jr., "Conversion of Ephemeris Coordinates */
/*          from the B1950 System to the J2000 System," JPL IOM */
/*          314.6-581, 24 June 1985. */

/*     [4]  E. M. Standish, Jr., "The Equinox Offsets of the JPL */
/*          Ephemeris," JPL IOM 314.6-929, 26 February 1988. */

/*     [5]  J. Lieske, "Expressions for the Precession Quantities Based */
/*          upon the IAU (1976) System of Astronomical Constants," */
/*          Astron. Astrophys. 58, 1-16, 1977. */

/*     [6]  L. Bass and R. Cesarone, "Mars Observer Planetary Constants */
/*          and Models," JPL D-3444, November 1990. */

/*     [7]  P. Kenneth Seidelmann (Ed.), "Explanatory Supplement to the */
/*          Astronomical Almanac," University Science Books, 1992. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     B.V. Semenov       (JPL) */
/*     W.L. Taber         (JPL) */
/*     I.M. Underwood     (JPL) */
/*     E.D. Wright        (JPL) */

/* $ Version */

/* -    SPICELIB Version 4.4.0, 17-JUN-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. Corrected */
/*        argument name in $Brief_I/O section: ROTAB was MATRIX. Added */
/*        $Literature_References. */

/* -    SPICELIB Version 4.3.0, 24-SEP-2013 (BVS) */

/*        Updated to do discovery check-in/check-out. */

/* -    SPICELIB Version 4.2.1, 04-JAN-2002 (EDW) */

/*        Added DE-143 to header description for IRFROT. */

/* -    SPICELIB Version 4.2.0, 10-APR-1997 (WLT) */

/*        A descriptive diagnostic was added to the entry points */
/*        IRFROT and IRFDEF. Before they simply signaled the error */
/*        with no diagnostic. */

/* -    SPICELIB Version 4.1.0, 14-OCT-1996 (WLT) */

/*        The number of inertial frames recognized is now stored */
/*        in the include file ninert.inc. */

/* -    SPICELIB Version 4.0.0, 20-MAY-1996 (WLT) */

/*        The inertial frame DE-143 was added to the list of recognized */
/*        inertial frames. */

/* -    SPICELIB Version 3.0.0, 20-MAR-1995 (WLT) */

/*        The inertial frames DE-140 and DE-142  were added to the */
/*        list of recognized inertial frames. */

/* -    SPICELIB Version 2.0.0, 30-JUL-1993 (WLT) */

/*        The transformation from J2000 to B1950 was upgraded */
/*        so that the transformation matrix produced matches */
/*        the matrix given in [1]. */

/*        The frame MARSIAU was added to the list */
/*        of recognized frames. This is the standard mars */
/*        referenced inertial frame used by the Mars Observer */
/*        project. */

/*        Values for the obliquity of the ecliptic were taken */
/*        from the Explanatory Supplement [7] to the Astronomical */
/*        Almanac (1992) at both the epochs J2000 and B1950 and */
/*        used to define the mean ecliptic and equinox frames */
/*        ECLIPJ2000 and ECLIPB1950. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (IMU) */

/* -& */
/* $ Index_Entries */

/*     inertial reference frame rotation */

/* -& */

/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }

/*     If it has not been done already, construct the transformation */
/*     from the root frame to each supported reference frame. */

/*     Begin by constructing the identity matrix (rotating by zero */
/*     radians about the x-axis). Apply the rotations indicated in */
/*     the frame definition (from right to left) to get the incremental */
/*     rotation from the base frame. The final rotation is */

/*        R             = (R           ) (R          ) */
/*         root->frame      base->frame    root->base */

    if (! ready) {
	chkin_("IRFROT", (ftnlen)6);
	for (i__ = 1; i__ <= 21; ++i__) {
	    rotate_(&c_b6, &c__1, &trans[(i__1 = i__ * 9 - 9) < 189 && 0 <= 
		    i__1 ? i__1 : s_rnge("trans", i__1, "chgirf_", (ftnlen)
		    930)]);
	    for (j = wdcnt_(defs + ((i__1 = i__ - 1) < 21 && 0 <= i__1 ? i__1 
		    : s_rnge("defs", i__1, "chgirf_", (ftnlen)932)) * 80, (
		    ftnlen)80); j >= 2; j += -2) {
		nthwd_(defs + ((i__1 = i__ - 1) < 21 && 0 <= i__1 ? i__1 : 
			s_rnge("defs", i__1, "chgirf_", (ftnlen)934)) * 80, &
			j, word, &loc, (ftnlen)80, (ftnlen)25);
		nparsi_(word, &axis, error, &p, (ftnlen)25, (ftnlen)25);
		i__2 = j - 1;
		nthwd_(defs + ((i__1 = i__ - 1) < 21 && 0 <= i__1 ? i__1 : 
			s_rnge("defs", i__1, "chgirf_", (ftnlen)937)) * 80, &
			i__2, word, &loc, (ftnlen)80, (ftnlen)25);
		nparsd_(word, &angle, error, &p, (ftnlen)25, (ftnlen)25);
		convrt_(&angle, "ARCSECONDS", "RADIANS", &radang, (ftnlen)10, 
			(ftnlen)7);
		rotmat_(&trans[(i__1 = i__ * 9 - 9) < 189 && 0 <= i__1 ? i__1 
			: s_rnge("trans", i__1, "chgirf_", (ftnlen)942)], &
			radang, &axis, tmpmat);
		moved_(tmpmat, &c__9, &trans[(i__1 = i__ * 9 - 9) < 189 && 0 
			<= i__1 ? i__1 : s_rnge("trans", i__1, "chgirf_", (
			ftnlen)943)]);
	    }
	    b = isrchc_(bases + (((i__1 = i__ - 1) < 21 && 0 <= i__1 ? i__1 : 
		    s_rnge("bases", i__1, "chgirf_", (ftnlen)947)) << 4), &
		    i__, frames, (ftnlen)16, (ftnlen)16);
	    mxm_(&trans[(i__1 = i__ * 9 - 9) < 189 && 0 <= i__1 ? i__1 : 
		    s_rnge("trans", i__1, "chgirf_", (ftnlen)949)], &trans[(
		    i__2 = b * 9 - 9) < 189 && 0 <= i__2 ? i__2 : s_rnge(
		    "trans", i__2, "chgirf_", (ftnlen)949)], tmpmat);
	    moved_(tmpmat, &c__9, &trans[(i__1 = i__ * 9 - 9) < 189 && 0 <= 
		    i__1 ? i__1 : s_rnge("trans", i__1, "chgirf_", (ftnlen)
		    950)]);
	}
	chkout_("IRFROT", (ftnlen)6);
	ready = TRUE_;
    }

/*     If the transformations have been defined, we can proceed with */
/*     the business at hand: determining the rotation from one frame */
/*     to another. To get from frame A to frame B, the rotation is */

/*                                     T */
/*        R     = (R       ) (R       ) */
/*         A->B     root->B    root->A */

/*     If A and B are the same frame, the rotation is just the identity. */
/*     In theory, computing */

/*                                     T */
/*        R     = (R       ) (R       ) */
/*         A->A     root->A    root->A */

/*     should work, but why risk roundoff problems? */

    if (*refa < 1 || *refa > 21) {
	chkin_("IRFROT", (ftnlen)6);
	setmsg_("A request has been made to obtain the transformation from i"
		"nertial reference frame # to inertial reference frame #. Unf"
		"ortunately # is not the id-code of a known inertial frame. ", 
		(ftnlen)178);
	errint_("#", refa, (ftnlen)1);
	errint_("#", refb, (ftnlen)1);
	errint_("#", refa, (ftnlen)1);
	sigerr_("SPICE(IRFNOTREC)", (ftnlen)16);
	chkout_("IRFROT", (ftnlen)6);
    } else if (*refb < 1 || *refb > 21) {
	chkin_("IRFROT", (ftnlen)6);
	setmsg_("A request has been made to obtain the transformation from i"
		"nertial reference frame # to inertial reference frame #. Unf"
		"ortunately # is not the id-code of a known inertial frame. ", 
		(ftnlen)178);
	errint_("#", refa, (ftnlen)1);
	errint_("#", refb, (ftnlen)1);
	errint_("#", refb, (ftnlen)1);
	sigerr_("SPICE(IRFNOTREC)", (ftnlen)16);
	chkout_("IRFROT", (ftnlen)6);
    } else if (*refa == *refb) {
	rotate_(&c_b6, &c__1, rotab);
    } else {
	mxmt_(&trans[(i__1 = *refb * 9 - 9) < 189 && 0 <= i__1 ? i__1 : 
		s_rnge("trans", i__1, "chgirf_", (ftnlen)1011)], &trans[(i__2 
		= *refa * 9 - 9) < 189 && 0 <= i__2 ? i__2 : s_rnge("trans", 
		i__2, "chgirf_", (ftnlen)1011)], rotab);
    }
    return 0;
/* $Procedure IRFNUM ( Inertial reference frame number ) */

L_irfnum:
/* $ Abstract */

/*     Return the index of one of the standard inertial reference */
/*     frames supported by IRFROT. */

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

/*     FRAMES */

/* $ Keywords */

/*     CONVERSION */
/*     COORDINATES */
/*     EPHEMERIS */
/*     FRAMES */
/*     MATRIX */
/*     ROTATION */
/*     TRANSFORMATION */
/*     VECTOR */

/* $ Declarations */

/*     CHARACTER*(*)         NAME */
/*     INTEGER               INDEX */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     NAME       I   Name of standard inertial reference frame. */
/*     INDEX      O   Index of frame. */

/* $ Detailed_Input */

/*     NAME     is the name of one of the standard inertial */
/*              reference frames supported by IRFROT, or */
/*              'DEFAULT'. */

/* $ Detailed_Output */

/*     INDEX    is the index of the frame specified by NAME. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If NAME is not recognized, INDEX is zero. */

/*     2)  If no default frame has been specified, INDEX is zero. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     IRFNUM is supplied as a convenience, to allow users to refer to */
/*     the various standard inertial reference frames by name. */

/* $ Examples */

/*     In the following example, the rotation from DE-118 to FK4 is */
/*     computed without knowing the indices of these frames. */

/*        CALL IRFNUM ( 'DE-118', A ) */
/*        CALL IRFNUM ( 'FK4',    B ) */

/*        CALL IRFROT ( A, B, ROTAB ) */

/*     IRFNUM can be used to rotate vectors into the default frame, */
/*     as illustrated by the following code fragment. */

/*        CALL IRFNUM ( 'FK4',     A ) */
/*        CALL IRFNUM ( 'DEFAULT', B ) */

/*        CALL IRFROT ( A, B, ROTAB                 ) */
/*        CALL MXV    (       ROTAB, OLDVEC, NEWVEC ) */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     [1]  J. Lieske, "Precession Matrix Based on IAU (1976) System of */
/*          Astronomical Constants," Astron. Astrophys. 73, 282-284, */
/*          1979. */

/*     [2]  E. M. Standish, Jr., "Orientation of the JPL Ephemerides, */
/*          DE 200/LE 200, to the Dynamical Equinox of J2000," Astron. */
/*          Astrophys. 114, 297-302, 1982. */

/*     [3]  E. M. Standish, Jr., "Conversion of Ephemeris Coordinates */
/*          from the B1950 System to the J2000 System," JPL IOM */
/*          314.6-581, 24 June 1985. */

/*     [4]  E. M. Standish, Jr., "The Equinox Offsets of the JPL */
/*          Ephemeris," JPL IOM 314.6-929, 26 February 1988. */

/*     [5]  J. Lieske, "Expressions for the Precession Quantities Based */
/*          upon the IAU (1976) System of Astronomical Constants," */
/*          Astron. Astrophys. 58, 1-16, 1977. */

/*     [6]  L. Bass and R. Cesarone, "Mars Observer Planetary Constants */
/*          and Models," JPL D-3444, November 1990. */

/*     [7]  P. Kenneth Seidelmann (Ed.), "Explanatory Supplement to the */
/*          Astronomical Almanac," University Science Books, 1992. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     B.V. Semenov       (JPL) */
/*     W.L. Taber         (JPL) */
/*     I.M. Underwood     (JPL) */
/*     E.D. Wright        (JPL) */

/* $ Version */

/* -    SPICELIB Version 4.4.0, 17-JUN-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. Added */
/*        $Literature_References. */

/* -    SPICELIB Version 4.3.0, 24-SEP-2013 (BVS) */

/*        Updated to treat J2000 as a special case and to not */
/*        CHKIN/CHOUT to increase efficiency. */

/* -    SPICELIB Version 4.2.1, 04-JAN-2002 (EDW) */

/*        Added DE-143 to header description for IRFROT. */

/* -    SPICELIB Version 4.2.0, 10-APR-1997 (WLT) */

/*        A descriptive diagnostic was added to the entry points */
/*        IRFROT and IRFDEF. Before they simply signaled the error */
/*        with no diagnostic. */

/* -    SPICELIB Version 4.1.0, 14-OCT-1996 (WLT) */

/*        The number of inertial frames recognized is now stored */
/*        in the include file ninert.inc. */

/* -    SPICELIB Version 4.0.0, 20-MAY-1996 (WLT) */

/*        The inertial frame DE-143 was added to the list of recognized */
/*        inertial frames. */

/* -    SPICELIB Version 3.0.0, 20-MAR-1995 (WLT) */

/*        The inertial frames DE-140 and DE-142  were added to the */
/*        list of recognized inertial frames. */

/* -    SPICELIB Version 2.0.0, 30-JUL-1993 (WLT) */

/*        The transformation from J2000 to B1950 was upgraded */
/*        so that the transformation matrix produced matches */
/*        the matrix given in [1]. */

/*        The frame MARSIAU was added to the list */
/*        of recognized frames. This is the standard mars */
/*        referenced inertial frame used by the Mars Observer */
/*        project. */

/*        Values for the obliquity of the ecliptic were taken */
/*        from the Explanatory Supplement [7] to the Astronomical */
/*        Almanac (1992) at both the epochs J2000 and B1950 and */
/*        used to define the mean ecliptic and equinox frames */
/*        ECLIPJ2000 and ECLIPB1950. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (IMU) */

/* -& */
/* $ Index_Entries */

/*     inertial reference frame number */

/* -& */

/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }
    if (s_cmp(name__, "J2000", name_len, (ftnlen)5) == 0 || s_cmp(name__, 
	    "j2000", name_len, (ftnlen)5) == 0) {
	*index = 1;
	return 0;
    }
    if (eqstr_(name__, "DEFAULT", name_len, (ftnlen)7)) {
	*index = dframe;
    } else {
	*index = esrchc_(name__, &c__21, frames, name_len, (ftnlen)16);
    }
    return 0;
/* $Procedure IRFNAM ( Inertial reference frame name ) */

L_irfnam:
/* $ Abstract */

/*     Return the name of one of the standard inertial reference */
/*     frames supported by IRFROT. */

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

/*     FRAMES */

/* $ Keywords */

/*     CONVERSION */
/*     COORDINATES */
/*     EPHEMERIS */
/*     FRAMES */
/*     MATRIX */
/*     ROTATION */
/*     TRANSFORMATION */
/*     VECTOR */

/* $ Declarations */

/*     INTEGER               INDEX */
/*     CHARACTER*(*)         NAME */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     INDEX      I   Index of standard inertial reference frame. */
/*     NAME       O   Name of frame. */

/* $ Detailed_Input */

/*     INDEX    is the index of one of the standard inertial */
/*              reference frames supported by IRFROT. */

/* $ Detailed_Output */

/*     NAME     is the name of the frame specified by INDEX. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If INDEX is not the index of a supported frame, NAME is blank. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     IRFNAM is supplied as a convenience, to allow users to determine */
/*     the names of standard inertial reference frames referred to only */
/*     by index (as in the segment descriptors of a GEF ephemeris file). */

/* $ Examples */

/*     In the following example, the identity of a rotation from DE-118 */
/*     to FK4 is deduced from the indices used to create the rotation. */

/*        CALL IRFROT ( A, B, ROTAB ) */

/*        CALL IRFNAM ( A, NAME(1) ) */
/*        CALL IRFNAM ( B, NAME(2) ) */

/*        WRITE (6,*) 'Rotation from ' // NAME(1) // ' to ' // NAME(2) */

/*     Note that the name of the default reference frame can only be */
/*     recovered from the number: */

/*        CALL IRFNUM ( 'DEFAULT', DINDEX        ) */
/*        CALL IRFNAM (            DINDEX, DNAME ) */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     [1]  J. Lieske, "Precession Matrix Based on IAU (1976) System of */
/*          Astronomical Constants," Astron. Astrophys. 73, 282-284, */
/*          1979. */

/*     [2]  E. M. Standish, Jr., "Orientation of the JPL Ephemerides, */
/*          DE 200/LE 200, to the Dynamical Equinox of J2000," Astron. */
/*          Astrophys. 114, 297-302, 1982. */

/*     [3]  E. M. Standish, Jr., "Conversion of Ephemeris Coordinates */
/*          from the B1950 System to the J2000 System," JPL IOM */
/*          314.6-581, 24 June 1985. */

/*     [4]  E. M. Standish, Jr., "The Equinox Offsets of the JPL */
/*          Ephemeris," JPL IOM 314.6-929, 26 February 1988. */

/*     [5]  J. Lieske, "Expressions for the Precession Quantities Based */
/*          upon the IAU (1976) System of Astronomical Constants," */
/*          Astron. Astrophys. 58, 1-16, 1977. */

/*     [6]  L. Bass and R. Cesarone, "Mars Observer Planetary Constants */
/*          and Models," JPL D-3444, November 1990. */

/*     [7]  P. Kenneth Seidelmann (Ed.), "Explanatory Supplement to the */
/*          Astronomical Almanac," University Science Books, 1992. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     W.L. Taber         (JPL) */
/*     I.M. Underwood     (JPL) */
/*     E.D. Wright        (JPL) */

/* $ Version */

/* -    SPICELIB Version 4.3.0, 17-JUN-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. Added */
/*        $Literature_References. */

/* -    SPICELIB Version 4.2.1, 04-JAN-2002 (EDW) */

/*        Added DE-143 to header description for IRFROT. */

/* -    SPICELIB Version 4.2.0, 10-APR-1997 (WLT) */

/*        A descriptive diagnostic was added to the entry points */
/*        IRFROT and IRFDEF. Before they simply signaled the error */
/*        with no diagnostic. */

/* -    SPICELIB Version 4.1.0, 14-OCT-1996 (WLT) */

/*        The number of inertial frames recognized is now stored */
/*        in the include file ninert.inc. */

/* -    SPICELIB Version 4.0.0, 20-MAY-1996 (WLT) */

/*        The inertial frame DE-143 was added to the list of recognized */
/*        inertial frames. */

/* -    SPICELIB Version 3.0.0, 20-MAR-1995 (WLT) */

/*        The inertial frames DE-140 and DE-142  were added to the */
/*        list of recognized inertial frames. */

/* -    SPICELIB Version 2.0.0, 30-JUL-1993 (WLT) */

/*        The transformation from J2000 to B1950 was upgraded */
/*        so that the transformation matrix produced matches */
/*        the matrix given in [1]. */

/*        The frame MARSIAU was added to the list */
/*        of recognized frames. This is the standard mars */
/*        referenced inertial frame used by the Mars Observer */
/*        project. */

/*        Values for the obliquity of the ecliptic were taken */
/*        from the Explanatory Supplement [7] to the Astronomical */
/*        Almanac (1992) at both the epochs J2000 and B1950 and */
/*        used to define the mean ecliptic and equinox frames */
/*        ECLIPJ2000 and ECLIPB1950. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (IMU) */

/* -& */
/* $ Index_Entries */

/*     inertial reference frame name */

/* -& */

/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("IRFNAM", (ftnlen)6);
    }
    if (*index < 1 || *index > 21) {
	s_copy(name__, " ", name_len, (ftnlen)1);
    } else {
	s_copy(name__, frames + (((i__1 = *index - 1) < 21 && 0 <= i__1 ? 
		i__1 : s_rnge("frames", i__1, "chgirf_", (ftnlen)1488)) << 4),
		 name_len, (ftnlen)16);
    }
    chkout_("IRFNAM", (ftnlen)6);
    return 0;
/* $Procedure IRFDEF ( Inertial reference frame, default ) */

L_irfdef:
/* $ Abstract */

/*     Specify a standard inertial reference frame as the default */
/*     frame for a program. */

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

/*     FRAMES */

/* $ Keywords */

/*     CONVERSION */
/*     COORDINATES */
/*     EPHEMERIS */
/*     FRAMES */
/*     MATRIX */
/*     ROTATION */
/*     TRANSFORMATION */
/*     VECTOR */

/* $ Declarations */

/*     INTEGER               INDEX */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     INDEX      I   Index of default frame. */

/* $ Detailed_Input */

/*     INDEX    is the index of one of the standard inertial */
/*              reference frames supported by IRFROT. */

/* $ Detailed_Output */

/*     None. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If INDEX is outside the range [1,MAXF], where MAXF is the */
/*         number of supported frames, the error SPICE(IRFNOTREC) is */
/*         signaled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     IRFDEF allows tools to be written at a relatively high level */
/*     without requiring the reference frame to be tramp coupled or */
/*     placed in global memory. */

/* $ Examples */

/*     Typically, the calling program will select a default frame */
/*     during initialization, */

/*        C */
/*        C     Use J2000 for all ephemeris, star data. */
/*        C */
/*              CALL IRFDEF ( 1 ) */

/*     and recover the default frame at lower levels, */

/*        C */
/*        C     Rotate all vectors into the default frame. */
/*        C */
/*              CALL IRFNUM ( 'DEFAULT', REFD   ) */

/*              DO I = 1, NVEC */
/*                 CALL IRFROT ( REFIN, REFD, ROT           ) */
/*                 CALL MXV                   ROT, VEC, VEC ) */
/*              END DO */

/*     Note that many utilities accept 'DEFAULT' as the name of */
/*     an inertial reference frame, */

/*        CALL SPKEZ ( TARGET, ..., 'DEFAULT', ... ) */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     [1]  J. Lieske, "Precession Matrix Based on IAU (1976) System of */
/*          Astronomical Constants," Astron. Astrophys. 73, 282-284, */
/*          1979. */

/*     [2]  E. M. Standish, Jr., "Orientation of the JPL Ephemerides, */
/*          DE 200/LE 200, to the Dynamical Equinox of J2000," Astron. */
/*          Astrophys. 114, 297-302, 1982. */

/*     [3]  E. M. Standish, Jr., "Conversion of Ephemeris Coordinates */
/*          from the B1950 System to the J2000 System," JPL IOM */
/*          314.6-581, 24 June 1985. */

/*     [4]  E. M. Standish, Jr., "The Equinox Offsets of the JPL */
/*          Ephemeris," JPL IOM 314.6-929, 26 February 1988. */

/*     [5]  J. Lieske, "Expressions for the Precession Quantities Based */
/*          upon the IAU (1976) System of Astronomical Constants," */
/*          Astron. Astrophys. 58, 1-16, 1977. */

/*     [6]  L. Bass and R. Cesarone, "Mars Observer Planetary Constants */
/*          and Models," JPL D-3444, November 1990. */

/*     [7]  P. Kenneth Seidelmann (Ed.), "Explanatory Supplement to the */
/*          Astronomical Almanac," University Science Books, 1992. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     W.L. Taber         (JPL) */
/*     I.M. Underwood     (JPL) */
/*     E.D. Wright        (JPL) */

/* $ Version */

/* -    SPICELIB Version 4.3.0, 17-JUN-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. Added */
/*        $Literature_References. */

/* -    SPICELIB Version 4.2.1, 04-JAN-2002 (EDW) */

/*        Added DE-143 to header description for IRFROT. */

/* -    SPICELIB Version 4.2.0, 10-APR-1997 (WLT) */

/*        A descriptive diagnostic was added to the entry points */
/*        IRFROT and IRFDEF. Before they simply signaled the error */
/*        with no diagnostic. */

/* -    SPICELIB Version 4.1.0, 14-OCT-1996 (WLT) */

/*        The number of inertial frames recognized is now stored */
/*        in the include file ninert.inc. */

/* -    SPICELIB Version 4.0.0, 20-MAY-1996 (WLT) */

/*        The inertial frame DE-143 was added to the list of recognized */
/*        inertial frames. */

/* -    SPICELIB Version 3.0.0, 20-MAR-1995 (WLT) */

/*        The inertial frames DE-140 and DE-142  were added to the */
/*        list of recognized inertial frames. */

/* -    SPICELIB Version 2.0.0, 30-JUL-1993 (WLT) */

/*        The transformation from J2000 to B1950 was upgraded */
/*        so that the transformation matrix produced matches */
/*        the matrix given in [1]. */

/*        The frame MARSIAU was added to the list */
/*        of recognized frames. This is the standard mars */
/*        referenced inertial frame used by the Mars Observer */
/*        project. */

/*        Values for the obliquity of the ecliptic were taken */
/*        from the Explanatory Supplement [7] to the Astronomical */
/*        Almanac (1992) at both the epochs J2000 and B1950 and */
/*        used to define the mean ecliptic and equinox frames */
/*        ECLIPJ2000 and ECLIPB1950. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (IMU) */

/* -& */
/* $ Index_Entries */

/*     inertial reference frame default */

/* -& */

/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("IRFDEF", (ftnlen)6);
    }

/*     There's not much to do, except save the value for later use. */

    if (*index < 1 || *index > 21) {
	setmsg_("The reference frame with id-code # is not a recognized iner"
		"tial reference frame. ", (ftnlen)81);
	errint_("#", index, (ftnlen)1);
	sigerr_("SPICE(IRFNOTREC)", (ftnlen)16);
    } else {
	dframe = *index;
    }
    chkout_("IRFDEF", (ftnlen)6);
    return 0;
} /* chgirf_ */

/* Subroutine */ int chgirf_(integer *refa, integer *refb, doublereal *rotab, 
	char *name__, integer *index, ftnlen name_len)
{
    return chgirf_0_(0, refa, refb, rotab, name__, index, name_len);
    }

/* Subroutine */ int irfrot_(integer *refa, integer *refb, doublereal *rotab)
{
    return chgirf_0_(1, refa, refb, rotab, (char *)0, (integer *)0, (ftnint)0)
	    ;
    }

/* Subroutine */ int irfnum_(char *name__, integer *index, ftnlen name_len)
{
    return chgirf_0_(2, (integer *)0, (integer *)0, (doublereal *)0, name__, 
	    index, name_len);
    }

/* Subroutine */ int irfnam_(integer *index, char *name__, ftnlen name_len)
{
    return chgirf_0_(3, (integer *)0, (integer *)0, (doublereal *)0, name__, 
	    index, name_len);
    }

/* Subroutine */ int irfdef_(integer *index)
{
    return chgirf_0_(4, (integer *)0, (integer *)0, (doublereal *)0, (char *)
	    0, index, (ftnint)0);
    }

