/* spkw15.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__15 = 15;
static integer c__16 = 16;

/* $Procedure SPKW15 ( SPK, write a type 15 segment ) */
/* Subroutine */ int spkw15_(integer *handle, integer *body, integer *center, 
	char *frame, doublereal *first, doublereal *last, char *segid, 
	doublereal *epoch, doublereal *tp, doublereal *pa, doublereal *p, 
	doublereal *ecc, doublereal *j2flg, doublereal *pv, doublereal *gm, 
	doublereal *j2, doublereal *radius, ftnlen frame_len, ftnlen 
	segid_len)
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    extern /* Subroutine */ int vhat_(doublereal *, doublereal *);
    doublereal mypa[3];
    extern doublereal vdot_(doublereal *, doublereal *), vsep_(doublereal *, 
	    doublereal *);
    extern /* Subroutine */ int vequ_(doublereal *, doublereal *);
    doublereal mytp[3];
    integer i__;
    doublereal angle;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    doublereal descr[5];
    integer value;
    extern /* Subroutine */ int errdp_(char *, doublereal *, ftnlen);
    extern logical vzero_(doublereal *);
    extern /* Subroutine */ int dafada_(doublereal *, integer *), dafbna_(
	    integer *, doublereal *, char *, ftnlen), dafena_(void);
    extern logical failed_(void);
    doublereal record[16];
    extern integer lastnb_(char *, ftnlen);
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), setmsg_(char *, ftnlen), errint_(char *, integer *, 
	    ftnlen), spkpds_(integer *, integer *, char *, integer *, 
	    doublereal *, doublereal *, doublereal *, ftnlen);
    extern logical return_(void);
    extern doublereal dpr_(void);
    doublereal dot;

/* $ Abstract */

/*     Write an SPK segment of type 15 given a type 15 data record. */

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

/*     SPK */

/* $ Keywords */

/*     EPHEMERIS */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     HANDLE     I   Handle of an SPK file open for writing. */
/*     BODY       I   Body code for ephemeris object. */
/*     CENTER     I   Body code for the center of motion of the body. */
/*     FRAME      I   The reference frame of the states. */
/*     FIRST      I   First valid time for which states can be computed. */
/*     LAST       I   Last valid time for which states can be computed. */
/*     SEGID      I   Segment identifier. */
/*     EPOCH      I   Epoch of the periapse. */
/*     TP         I   Trajectory pole vector. */
/*     PA         I   Periapsis vector. */
/*     P          I   Semi-latus rectum. */
/*     ECC        I   Eccentricity. */
/*     J2FLG      I   J2 processing flag. */
/*     PV         I   Central body pole vector. */
/*     GM         I   Central body GM. */
/*     J2         I   Central body J2. */
/*     RADIUS     I   Equatorial radius of central body. */

/* $ Detailed_Input */

/*     HANDLE   is the file handle of an SPK file that has been */
/*              opened for writing. */

/*     BODY     is the NAIF ID for the body whose states are */
/*              to be recorded in an SPK file. */

/*     CENTER   is the NAIF ID for the center of motion associated */
/*              with BODY. */

/*     FRAME    is the reference frame that states are referenced to, */
/*              for example 'J2000'. */

/*     FIRST    are the bounds on the ephemeris times, expressed as */
/*     LAST     seconds past J2000. */

/*     SEGID    is the segment identifier. An SPK segment identifier */
/*              may contain up to 40 characters. */

/*     EPOCH    is the epoch of the orbit elements at periapse */
/*              in ephemeris seconds past J2000. */

/*     TP       is a vector parallel to the angular momentum vector */
/*              of the orbit at epoch expressed relative to FRAME. A */
/*              unit vector parallel to TP will be stored in the */
/*              output segment. */

/*     PA       is a vector parallel to the position vector of the */
/*              trajectory at periapsis of EPOCH expressed relative */
/*              to FRAME. A unit vector parallel to PA will be */
/*              stored in the output segment. */

/*     P        is the semi-latus rectum--- p in the equation: */

/*                 r = p/(1 + ECC*COS(Nu)) */

/*     ECC      is the eccentricity. */

/*     J2FLG    is the J2 processing flag describing what J2 */
/*              corrections are to be applied when the orbit is */
/*              propagated. */

/*              All J2 corrections are applied if the value of J2FLG */
/*              is not 1, 2 or 3. */

/*              If the value of the flag is 3 no corrections are */
/*              done. */

/*              If the value of the flag is 1 no corrections are */
/*              computed for the precession of the line of apsides. */
/*              However, regression of the line of nodes is */
/*              performed. */

/*              If the value of the flag is 2 no corrections are */
/*              done for the regression of the line of nodes. */
/*              However, precession of the line of apsides is */
/*              performed. */

/*              Note that J2 effects are computed only if the orbit */
/*              is elliptic and does not intersect the central body. */

/*     PV       is a vector parallel to the north pole vector of the */
/*              central body expressed relative to FRAME. A unit */
/*              vector parallel to PV will be stored in the output */
/*              segment. */

/*     GM       is the central body GM. */

/*     J2       is the central body J2 (dimensionless). */

/*     RADIUS   is the equatorial radius of the central body. */

/*     Units are radians, km, seconds. */

/* $ Detailed_Output */

/*     None. A type 15 segment is written to the file attached */
/*     to HANDLE. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the eccentricity is less than zero, the error */
/*         SPICE(BADECCENTRICITY) is signaled. */

/*     2)  If the semi-latus rectum is 0, the error */
/*         SPICE(BADLATUSRECTUM) is signaled. */

/*     3)  If the pole vector, trajectory pole vector or periapsis vector */
/*         have zero length, the error SPICE(BADVECTOR) is signaled. */

/*     4)  If the trajectory pole vector and the periapsis vector are */
/*         not orthogonal, the error SPICE(BADINITSTATE) is signaled. */
/*         The test for orthogonality is very crude. The routine simply */
/*         checks that the dot product of the unit vectors parallel */
/*         to the trajectory pole and periapse vectors is less than */
/*         0.00001. This check is intended to catch blunders, not to */
/*         enforce orthogonality to double precision capacity. */

/*     5)  If the mass of the central body is non-positive, the error */
/*         SPICE(NONPOSITIVEMASS) is signaled. */

/*     6)  If the radius of the central body is negative, the error */
/*         SPICE(BADRADIUS) is signaled. */

/*     7)  If the segment identifier has more than 40 non-blank */
/*         characters, the error SPICE(SEGIDTOOLONG) is signaled. */

/*     8)  If the segment identifier contains non-printing characters, */
/*         the error SPICE(NONPRINTABLECHARS) is signaled. */

/*     9)  If there are inconsistencies in the BODY, CENTER, FRAME or */
/*         FIRST and LAST times, an error is signaled by */
/*         a routine in the call tree of this routine. */

/* $ Files */

/*     A new type 15 SPK segment is written to the SPK file attached */
/*     to HANDLE. */

/* $ Particulars */

/*     This routine writes an SPK type 15 data segment to the open SPK */
/*     file according to the format described in the type 15 section of */
/*     the SPK Required Reading. The SPK file must have been opened with */
/*     write access. */

/*     This routine is provided to provide direct support for the MASL */
/*     precessing orbit formulation. */

/* $ Examples */

/*     Suppose that at time EPOCH you have the J2000 periapsis */
/*      state of some object relative to some central body and would */
/*      like to create a type 15 SPK segment to model the motion of */
/*      the object using simple regression and precession of the */
/*      line of nodes and apsides. The following code fragment */
/*      illustrates how you can prepare such a segment. We shall */
/*      assume that you have in hand the J2000 direction of the */
/*      central body's pole vector, its GM, J2 and equatorial */
/*      radius. In addition we assume that you have opened an SPK */
/*      file for write access and that it is attached to HANDLE. */

/*     (If your state is at an epoch other than periapse the */
/*      fragment below will NOT produce a "correct" type 15 segment */
/*      for modeling the motion of your object.) */

/*      C */
/*      C     First we get the osculating elements. */
/*      C */
/*            CALL OSCELT ( STATE, EPOCH, GM, ELTS ) */

/*      C */
/*      C     From these collect the eccentricity and semi-latus rectum. */
/*      C */
/*            ECC = ELTS ( 2 ) */
/*            P   = ELTS ( 1 ) * ( 1.0D0 + ECC ) */
/*      C */
/*      C     Next get the trajectory pole vector and the */
/*      C     periapsis vector. */
/*      C */
/*            CALL UCRSS ( STATE(1), STATE(4), TP ) */
/*            CALL VHAT  ( STATE(1),           PA ) */

/*      C */
/*      C     Enable both J2 corrections. */
/*      C */

/*           J2FLG = 0.0D0 */

/*      C */
/*      C     Now add the segment. */
/*      C */

/*            CALL SPKW15 ( HANDLE, BODY,  CENTER, FRAME,  FIRST, LAST, */
/*            .              SEGID,  EPOCH, TP,     PA,    P,     ECC, */
/*            .              J2FLG,  PV,    GM,     J2,    RADIUS      ) */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 2.1.0, 03-JUN-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 2.0.0, 29-MAY-2012 (NJB) */

/*        Input vectors that nominally have unit length */
/*        are mapped to local copies that actually do */
/*        have unit length. The applicable inputs are TP, PA, */
/*        and PV. The Detailed Input header section was updated */
/*        to reflect the change. */

/*        Some typos in error messages were corrected. */

/* -    SPICELIB Version 1.0.0, 28-NOV-1994 (WLT) */

/* -& */
/* $ Index_Entries */

/*     Write a type 15 SPK segment */

/* -& */

/*     SPICELIB Functions */


/*     Local Variables */


/*     Segment descriptor size */


/*     Segment identifier size */


/*     SPK data type */


/*     Range of printing characters */


/*     Number of items in a segment */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }
    chkin_("SPKW15", (ftnlen)6);

/*     Fetch the various entities from the inputs and put them into */
/*     the data record, first the epoch. */

    record[0] = *epoch;

/*     Convert TP and PA to unit vectors. */

    vhat_(pa, mypa);
    vhat_(tp, mytp);

/*     The trajectory pole vector. */

    vequ_(mytp, &record[1]);

/*     The periapsis vector. */

    vequ_(mypa, &record[4]);

/*     Semi-latus rectum ( P in the P/(1 + ECC*COS(Nu)  ), */
/*     and eccentricity. */

    record[7] = *p;
    record[8] = *ecc;

/*     J2 processing flag. */

    record[9] = *j2flg;

/*     Central body pole vector. */

    vhat_(pv, &record[10]);

/*     The central mass, J2 and radius of the central body. */

    record[13] = *gm;
    record[14] = *j2;
    record[15] = *radius;

/*     Check all the inputs here for obvious failures.  It's much */
/*     better to check them now and quit than it is to get a bogus */
/*     segment into an SPK file and diagnose it later. */

    if (*p <= 0.) {
	setmsg_("The semi-latus rectum supplied to the SPK type 15 evaluator"
		" was non-positive.  This value must be positive. The value s"
		"upplied was #.", (ftnlen)133);
	errdp_("#", p, (ftnlen)1);
	sigerr_("SPICE(BADLATUSRECTUM)", (ftnlen)21);
	chkout_("SPKW15", (ftnlen)6);
	return 0;
    } else if (*ecc < 0.) {
	setmsg_("The eccentricity supplied for a type 15 segment is negative"
		".  It must be non-negative. The value supplied to the type 1"
		"5 evaluator was #. ", (ftnlen)138);
	errdp_("#", ecc, (ftnlen)1);
	sigerr_("SPICE(BADECCENTRICITY)", (ftnlen)22);
	chkout_("SPKW15", (ftnlen)6);
	return 0;
    } else if (*gm <= 0.) {
	setmsg_("The mass supplied for the central body of a type 15 segment"
		" was non-positive. Masses must be positive.  The value suppl"
		"ied was #. ", (ftnlen)130);
	errdp_("#", gm, (ftnlen)1);
	sigerr_("SPICE(NONPOSITIVEMASS)", (ftnlen)22);
	chkout_("SPKW15", (ftnlen)6);
	return 0;
    } else if (vzero_(tp)) {
	setmsg_("The trajectory pole vector supplied to SPKW15 had length ze"
		"ro. The most likely cause of this problem is an uninitialize"
		"d vector.", (ftnlen)128);
	sigerr_("SPICE(BADVECTOR)", (ftnlen)16);
	chkout_("SPKW15", (ftnlen)6);
	return 0;
    } else if (vzero_(pa)) {
	setmsg_("The periapse vector supplied to SPKW15 had length zero. The"
		" most likely cause of this problem is an uninitialized vecto"
		"r.", (ftnlen)121);
	sigerr_("SPICE(BADVECTOR)", (ftnlen)16);
	chkout_("SPKW15", (ftnlen)6);
	return 0;
    } else if (vzero_(pv)) {
	setmsg_("The central pole vector supplied to SPKW15 had length zero."
		" The most likely cause of this problem is an uninitialized v"
		"ector. ", (ftnlen)126);
	sigerr_("SPICE(BADVECTOR)", (ftnlen)16);
	chkout_("SPKW15", (ftnlen)6);
	return 0;
    } else if (*radius < 0.) {
	setmsg_("The central body radius was negative. It must be zero or po"
		"sitive.  The value supplied was #. ", (ftnlen)94);
	errdp_("#", radius, (ftnlen)1);
	sigerr_("SPICE(BADRADIUS)", (ftnlen)16);
	chkout_("SPKW15", (ftnlen)6);
	return 0;
    }

/*     One final check.  Make sure the pole and periapse vectors are */
/*     orthogonal. (We will use a very crude check but this should */
/*     rule out any obvious errors.) */

    dot = vdot_(mypa, mytp);
    if (abs(dot) > 1e-5) {
	angle = vsep_(pa, tp) * dpr_();
	setmsg_("The periapsis and trajectory pole vectors are not orthogona"
		"l. The angle between them is # degrees. ", (ftnlen)99);
	errdp_("#", &angle, (ftnlen)1);
	sigerr_("SPICE(BADINITSTATE)", (ftnlen)19);
	chkout_("SPKW15", (ftnlen)6);
	return 0;
    }

/*     Make sure the segment identifier is not too long. */

    if (lastnb_(segid, segid_len) > 40) {
	setmsg_("Segment identifier contains more than 40 characters.", (
		ftnlen)52);
	sigerr_("SPICE(SEGIDTOOLONG)", (ftnlen)19);
	chkout_("SPKW15", (ftnlen)6);
	return 0;
    }

/*     Make sure it has only printing characters. */

    i__1 = lastnb_(segid, segid_len);
    for (i__ = 1; i__ <= i__1; ++i__) {
	value = *(unsigned char *)&segid[i__ - 1];
	if (value < 32 || value > 126) {
	    setmsg_("The segment identifier contains the nonprintable charac"
		    "ter having ascii code #.", (ftnlen)79);
	    errint_("#", &value, (ftnlen)1);
	    sigerr_("SPICE(NONPRINTABLECHARS)", (ftnlen)24);
	    chkout_("SPKW15", (ftnlen)6);
	    return 0;
	}
    }

/*     All of the obvious checks have been performed on the input */
/*     record.  Create the segment descriptor. (FIRST and LAST are */
/*     checked by SPKPDS as well as consistency between BODY and CENTER). */

    spkpds_(body, center, frame, &c__15, first, last, descr, frame_len);
    if (failed_()) {
	chkout_("SPKW15", (ftnlen)6);
	return 0;
    }

/*     Begin a new segment. */

    dafbna_(handle, descr, segid, segid_len);
    if (failed_()) {
	chkout_("SPKW15", (ftnlen)6);
	return 0;
    }
    dafada_(record, &c__16);
    if (! failed_()) {
	dafena_();
    }
    chkout_("SPKW15", (ftnlen)6);
    return 0;
} /* spkw15_ */

