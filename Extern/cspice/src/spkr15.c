/* spkr15.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__2 = 2;
static integer c__6 = 6;

/* $Procedure SPKR15 ( Read SPK record from segment, type 15 ) */
/* Subroutine */ int spkr15_(integer *handle, doublereal *descr, doublereal *
	et, doublereal *record)
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    integer type__, begin;
    extern /* Subroutine */ int chkin_(char *, ftnlen), dafus_(doublereal *, 
	    integer *, integer *, doublereal *, integer *), dafgda_(integer *,
	     integer *, integer *, doublereal *);
    doublereal dc[2];
    integer ic[6];
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), setmsg_(char *, ftnlen), errint_(char *, integer *, 
	    ftnlen);
    extern logical return_(void);
    integer end;

/* $ Abstract */

/*     Read a single SPK data record from a segment of type 15 */
/*     (Precessing Conic Propagation). */

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
/*     HANDLE     I   File handle. */
/*     DESCR      I   Segment descriptor. */
/*     ET         I   Target epoch. */
/*     RECORD     O   Data record. */

/* $ Detailed_Input */

/*     HANDLE, */
/*     DESCR    are the file handle and segment descriptor for */
/*              a SPK segment of type 15. */

/*     ET       is a target epoch, for which a data record from */
/*              a specific segment is required. */

/* $ Detailed_Output */

/*     RECORD   is the record from the specified segment which, */
/*              when evaluated at epoch ET, will give the state */
/*              (position and velocity) of some body, relative */
/*              to some center, in some inertial reference frame. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the segment specified by DESCR is not a type 15 segment, */
/*         the error SPICE(WRONGSPKTYPE) is signaled. */

/*     2)  A type 15 segment should have exactly 16 values. If this */
/*         is not the case, the error SPICE(MALFORMEDSEGMENT) is */
/*         signaled. */

/* $ Files */

/*     See argument HANDLE. */

/* $ Particulars */

/*     This routine reads all of the data from a type 15 SPK segment. */

/*     The structure of the data retrieved in RECORD is: */

/*         RECORD(1)             epoch of the orbit elements at periapse */
/*                               in ephemeris seconds past J2000. */
/*         RECORD(2)-RECORD(4)   unit trajectory pole vector */
/*         RECORD(5)-RECORD(7)   unit periapsis vector */
/*         RECORD(8)             semi-latus rectum---p in the */
/*                               equation: */

/*                               r = p/(1 + ECC*COS(Nu)) */

/*         RECORD(9)             eccentricity */
/*         RECORD(10)            J2 processing flag describing */
/*                               what J2 corrections are to be */
/*                               applied when the orbit is */
/*                               propagated. */

/*                                Value       Meaning */
/*                                -----  ----------------------------- */
/*                                1      Regress line of nodes only. */
/*                                2      Precess line of apsides only. */
/*                                3      Don't use J2 corrections. */
/*                                Other  Regress line of nodes */
/*                                       and precess line of apsides. */

/*         RECORD(11)-RECORD(13) unit central body pole vector */
/*         RECORD(14)            central body GM */
/*         RECORD(15)            central body J2 */
/*         RECORD(16)            central body radius */

/*     Except for J2, units are radians, km, seconds. */

/* $ Examples */

/*     The data returned by the SPKRnn routine is in its rawest form, */
/*     taken directly from the segment. As such, it will be meaningless */
/*     to a user unless he/she understands the structure of the data type */
/*     completely. Given that understanding, however, the SPKRnn */
/*     routines might be used to "dump" and check segment data for a */
/*     particular epoch. */


/*     C */
/*     C     Get a segment applicable to a specified body and epoch. */
/*     C */
/*           CALL SPKSFS ( BODY, ET, HANDLE, DESCR, IDENT, FOUND ) */

/*     C */
/*     C     Look at parts of the descriptor. */
/*     C */
/*           CALL DAFUS ( DESCR, 2, 6, DCD, ICD ) */
/*           CENTER = ICD( 2 ) */
/*           REF    = ICD( 3 ) */
/*           TYPE   = ICD( 4 ) */

/*           IF ( TYPE .EQ. 15 ) THEN */
/*              CALL SPKR15 ( HANDLE, DESCR, ET, RECORD ) */
/*                  . */
/*                  .  Look at the RECORD data. */
/*                  . */
/*           END IF */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     S. Schlaifer       (JPL) */
/*     W.L. Taber         (JPL) */
/*     E.D. Wright        (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.1, 27-AUG-2021 (JDR) */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 1.1.0, 07-SEP-2001 (EDW) */

/*        Replaced DAFRDA call with DAFGDA. */
/*        Added IMPLICIT NONE. */

/* -    SPICELIB Version 1.0.0, 15-NOV-1994 (WLT) (SS) */

/* -& */
/* $ Index_Entries */

/*     read record from type_15 SPK segment */

/* -& */

/*     SPICELIB Functions */


/*     Local Variables */


/*     The difference between the first and last address of a type 15 */
/*     segment should be 15. */


/*     Standard Spice Error Handling. */

    if (return_()) {
	return 0;
    }
    chkin_("SPKR15", (ftnlen)6);

/*     Unpack the segment descriptor. */

    dafus_(descr, &c__2, &c__6, dc, ic);
    type__ = ic[3];
    begin = ic[4];
    end = ic[5];

/*     Make sure that this really is a type 15 data segment. */

    if (type__ != 15) {
	setmsg_("You are attempting to locate type 15 data in a type # data "
		"segment.", (ftnlen)67);
	errint_("#", &type__, (ftnlen)1);
	sigerr_("SPICE(WRONGSPKTYPE)", (ftnlen)19);
	chkout_("SPKR15", (ftnlen)6);
	return 0;
    }

/*     Since it doesn't cost much we make sure that the segment has */
/*     the correct amount of data. */

    if (end - begin != 15) {
	setmsg_("A type 15 segment should contain exactly 16 double precisio"
		"n values.  The segment supplied had #.  The segment is badly"
		" formed. ", (ftnlen)128);
	i__1 = end - begin + 1;
	errint_("#", &i__1, (ftnlen)1);
	sigerr_("SPICE(MALFORMEDSEGMENT)", (ftnlen)23);
	chkout_("SPKR15", (ftnlen)6);
	return 0;
    }

/*     Read the data for the record. */

    dafgda_(handle, &begin, &end, record);
    chkout_("SPKR15", (ftnlen)6);
    return 0;
} /* spkr15_ */

