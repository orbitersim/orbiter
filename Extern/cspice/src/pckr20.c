/* pckr20.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__2 = 2;
static integer c__6 = 6;
static integer c__1 = 1;
static integer c__3 = 3;

/* $Procedure PCKR20 ( PCK, read record from segment, type 20 ) */
/* Subroutine */ int pckr20_(integer *handle, doublereal *descr, doublereal *
	et, doublereal *record)
{
    /* System generated locals */
    integer i__1, i__2;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer);

    /* Local variables */
    integer nrec;
    doublereal init;
    integer size, i__, begin;
    extern /* Subroutine */ int chkin_(char *, ftnlen), dafus_(doublereal *, 
	    integer *, integer *, doublereal *, integer *);
    integer recno;
    extern /* Subroutine */ int moved_(doublereal *, integer *, doublereal *),
	     dafgda_(integer *, integer *, integer *, doublereal *);
    doublereal dc[2];
    integer ic[6];
    doublereal recbeg, dscale;
    integer recadr;
    extern /* Subroutine */ int remlad_(integer *, integer *, doublereal *, 
	    integer *);
    doublereal tscale, initjd, radius, intlen, initfr;
    extern /* Subroutine */ int chkout_(char *, ftnlen);
    integer recsiz;
    extern /* Subroutine */ int vsclip_(doublereal *, doublereal *);
    integer nterms;
    doublereal intrvl;
    extern logical return_(void);
    extern doublereal j2000_(void);
    integer end;
    doublereal mid;
    integer loc;
    extern doublereal spd_(void);
    doublereal pos[3];

/* $ Abstract */

/*     Read a single PCK data record from a segment of type 20 */
/*     (Chebyshev, derivative coefficients only). */

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

/*     PCK */

/* $ Keywords */

/*     ORIENTATION */
/*     ROTATION */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     HANDLE     I   File handle. */
/*     DESCR      I   Segment descriptor. */
/*     ET         I   Evaluation epoch. */
/*     RECORD     O   Data record. */

/* $ Detailed_Input */

/*     HANDLE, */
/*     DESCR    are the file handle and segment descriptor for */
/*              a PCK segment of type 20. */

/*     ET       is an epoch for which a data record from a specific */
/*              segment is required. ET is expressed as seconds past */
/*              J2000 TDB. */

/* $ Detailed_Output */

/*     RECORD   is the record from the specified segment which, when */
/*              evaluated at epoch ET, will give Euler angles and */
/*              Euler angle rates representing the orientation and */
/*              angular velocity of the body-fixed reference frame */
/*              associated with the segment. */

/*              The structure of the record is as follows: */

/*                 +--------------------------------------+ */
/*                 | record size (excluding this element) | */
/*                 +--------------------------------------+ */
/*                 | Coverage interval midpoint           | */
/*                 +--------------------------------------+ */
/*                 | Coverage interval radius             | */
/*                 +--------------------------------------+ */
/*                 | Coeffs for ANGLE_1 rate              | */
/*                 +--------------------------------------+ */
/*                 | Coeffs for ANGLE_2 rate              | */
/*                 +--------------------------------------+ */
/*                 | Coeffs for ANGLE_3 rate              | */
/*                 +--------------------------------------+ */
/*                 | ANGLE_1 at interval midpoint         | */
/*                 +--------------------------------------+ */
/*                 | ANGLE_2 at interval midpoint         | */
/*                 +--------------------------------------+ */
/*                 | ANGLE_3 at interval midpoint         | */
/*                 +--------------------------------------+ */

/*              In the above record */

/*                 - Times are expressed as seconds past J2000 TDB. */
/*                 - Angular components have units of radians. */
/*                 - Rate coefficients have units of radians/s. */

/*              RECORD must be declared by the caller with size large */
/*              enough to accommodate the largest record that can be */
/*              returned by this routine. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If an issue is detected while looking up PCK data, an error is */
/*         signaled by a routine in the call tree of this routine. */

/* $ Files */

/*     See argument HANDLE. */

/* $ Particulars */

/*     See the PCK Required Reading file for a description of the */
/*     structure of a data type 20 (Chebyshev polynomials, */
/*     derivative coefficients only) segment. */

/* $ Examples */

/*     The data returned by the PCKRnn routine is in its rawest form, */
/*     taken directly from the segment. As such, it will be meaningless */
/*     to a user unless he/she understands the structure of the data type */
/*     completely. Given that understanding, however, the PCKRxx */
/*     routines might be used to "dump" and check segment data for a */
/*     particular epoch. */


/*     C */
/*     C     Get a segment applicable to a specified frame class ID */
/*     C     and epoch. */
/*     C */
/*           CALL PCKSFS ( CLSSID, ET, HANDLE, DESCR, IDENT, FOUND ) */

/*     C */
/*     C     Look at parts of the descriptor. */
/*     C */
/*           CALL DAFUS ( DESCR, 2, 6, DCD, ICD ) */
/*           REF    = ICD( 2 ) */
/*           TYPE   = ICD( 3 ) */

/*           IF ( TYPE .EQ. 20 ) THEN */
/*              CALL PCKR20 ( HANDLE, DESCR, ET, RECORD ) */
/*                  . */
/*                  .  Look at the RECORD data. */
/*                  . */
/*           END IF */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     I.M. Underwood     (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.1, 12-AUG-2021 (JDR) */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 1.0.0, 17-JAN-2014 (NJB) (IMU) */

/* -& */
/* $ Index_Entries */

/*     read record from type_20 PCK segment */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }
    chkin_("PCKR20", (ftnlen)6);

/*     Unpack the segment descriptor. */

    dafus_(descr, &c__2, &c__6, dc, ic);
    begin = ic[3];
    end = ic[4];

/*     The segment is made up of a number of logical records, each */
/*     having the same size, and covering the same length of time. */

/*     We can determine which record to return using the input epoch, */
/*     the integer and fractional parts of the initial time of the first */
/*     record's coverage interval, and the length of the interval */
/*     covered by each record. These constants are located at the end of */
/*     the segment, along with the size of each logical record and the */
/*     total number of records. */

/*     For convenience, we'll fetch the segment's distance and time */
/*     scales in the same call. */

    i__1 = end - 6;
    dafgda_(handle, &i__1, &end, record);
    dscale = record[0];
    tscale = record[1];
    initjd = record[2];
    initfr = record[3];
    intlen = record[4];
    recsiz = (integer) record[5];
    nrec = (integer) record[6];

/*     NTERMS is the number of rate coefficients per component, */
/*     plus 1 (for the angle component). */

    nterms = recsiz / 3;

/*     Convert the initial epoch and interval length to */
/*     seconds past J2000 TDB. */

    init = (initjd - j2000_() + initfr) * spd_();
    intrvl = intlen * spd_();

/*     Locate the record containing the coefficients to use. */

    recno = (integer) ((*et - init) / intrvl) + 1;
/* Computing MAX */
    i__1 = 1, i__2 = min(recno,nrec);
    recno = max(i__1,i__2);

/*     Compute the midpoint and radius of the record at */
/*     index RECNO. We want to compute the midpoint in such */
/*     a way that we take advantage of interval lengths that */
/*     are exactly representable, when we have them. */

/*     RECBEG is the record start time, minus the fractional */
/*     part of the segment start time, expressed as seconds */
/*     past J2000. We'll account for the fractional part of the */
/*     start time below when we compute MID. */

    recbeg = (initjd - j2000_() + (recno - 1) * intlen) * spd_();
    radius = intrvl / 2.;
    mid = recbeg + initfr * spd_() + radius;

/*     Compute the address of the desired record. */

    recadr = (recno - 1) * recsiz + begin;

/*     Along with the record, return the size, midpoint, and */
/*     radius of the record. */

    record[0] = record[5] + 2;
    record[1] = mid;
    record[2] = radius;
    i__1 = recadr + recsiz - 1;
    dafgda_(handle, &recadr, &i__1, &record[3]);

/*     We're going to re-arrange the record: the angle components */
/*     will be transferred to the end of the record, and the record */
/*     contents will be left-shifted to fill in the free elements. */

    for (i__ = 1; i__ <= 3; ++i__) {
	pos[(i__1 = i__ - 1) < 3 && 0 <= i__1 ? i__1 : s_rnge("pos", i__1, 
		"pckr20_", (ftnlen)326)] = record[i__ * nterms + 2];
    }
    size = recsiz + 3;

/*     Remove the angle elements from the record. */

    for (i__ = 1; i__ <= 3; ++i__) {

/*        LOC is the index of the element to delete. After the first */
/*        removal, we must account for the resulting left shift when */
/*        calculating the indices of subsequent elements to be removed. */

	loc = i__ * nterms + 3 - (i__ - 1);
	remlad_(&c__1, &loc, record, &size);

/*        Note that SIZE is an in-out argument; on output it */
/*        indicates the size of the array after removal of */
/*        the indicated element(s). */

    }

/*     Convert the angles to radians. */

    vsclip_(&dscale, pos);

/*     Append the angles to the record. Since we inserted three */
/*     elements at the start of the record and deleted three angle */
/*     elements, the target index is the same as if we had copied the */
/*     record directly to the output array. */

    moved_(pos, &c__3, &record[recsiz]);

/*     Convert the angular rate Chebyshev coefficients to units of */
/*     radians/s. */

    i__1 = recsiz;
    for (i__ = 4; i__ <= i__1; ++i__) {
	record[i__ - 1] *= dscale / tscale;
    }
    chkout_("PCKR20", (ftnlen)6);
    return 0;
} /* pckr20_ */

