/* spkr02.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__2 = 2;
static integer c__6 = 6;

/* $Procedure SPKR02 ( SPK, read record from segment, type 2 ) */
/* Subroutine */ int spkr02_(integer *handle, doublereal *descr, doublereal *
	et, doublereal *record)
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    integer nrec;
    doublereal init;
    integer begin;
    extern /* Subroutine */ int chkin_(char *, ftnlen), dafus_(doublereal *, 
	    integer *, integer *, doublereal *, integer *);
    integer recno;
    extern /* Subroutine */ int dafgda_(integer *, integer *, integer *, 
	    doublereal *);
    doublereal dc[2];
    integer ic[6], recadr;
    doublereal intlen;
    extern /* Subroutine */ int chkout_(char *, ftnlen);
    integer recsiz;
    extern logical return_(void);
    integer end;

/* $ Abstract */

/*     Read a single SPK data record from a segment of type 2 */
/*     (Chebyshev, position only). */

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
/*     ET         I   Evaluation epoch. */
/*     RECORD     O   Data record. */

/* $ Detailed_Input */

/*     HANDLE, */
/*     DESCR    are the file handle and segment descriptor for */
/*              an SPK segment of type 2. */

/*     ET       is an epoch for which a data record from the */
/*              specified segment is required. ET is expressed as */
/*              seconds past J2000 TDB. */

/* $ Detailed_Output */

/*     RECORD   is an array of data from the specified segment which, */
/*              when evaluated at epoch ET, will give the state */
/*              (position and velocity) of the target body identified */
/*              by the input segment descriptor. The descriptor */
/*              specifies the center of motion and reference frame of */
/*              the state. */

/*              The structure of the record is as follows: */

/*                 +--------------------------------------+ */
/*                 | record size (excluding this element) | */
/*                 +--------------------------------------+ */
/*                 | Coverage interval midpoint           | */
/*                 +--------------------------------------+ */
/*                 | Coverage interval radius             | */
/*                 +--------------------------------------+ */
/*                 | Coeffs for X position component      | */
/*                 +--------------------------------------+ */
/*                 | Coeffs for Y position component      | */
/*                 +--------------------------------------+ */
/*                 | Coeffs for Z position component      | */
/*                 +--------------------------------------+ */

/*              In the above record */

/*                 - Times are expressed as seconds past J2000 TDB. */
/*                 - Position components have units of km. */

/*              RECORD must be declared by the caller with size large */
/*              enough to accommodate the largest record that can be */
/*              returned by this routine. See the INCLUDE file */
/*              spkrec.inc for the correct record length. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If an error occurs while looking up SPK data, the error is */
/*         signaled by a routine in the call tree of this routine. */

/* $ Files */

/*     See argument HANDLE. */

/* $ Particulars */

/*     See the SPK Required Reading file for a description of the */
/*     structure of a data type 2 (Chebyshev polynomials, position */
/*     only) segment. */

/* $ Examples */

/*     The data returned by the SPKRnn routine is in its rawest form, */
/*     taken directly from the segment. As such, it will be meaningless */
/*     to a user unless he/she understands the structure of the data type */
/*     completely. Given that understanding, however, the SPKRxx */
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

/*           IF ( TYPE .EQ. 2 ) THEN */
/*              CALL SPKR02 ( HANDLE, DESCR, ET, RECORD ) */
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
/*     H.A. Neilan        (JPL) */
/*     W.L. Taber         (JPL) */
/*     I.M. Underwood     (JPL) */
/*     E.D. Wright        (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.2, 14-APR-2021 (JDR) */

/*        Edited the header to comply with NAIF standard. Moved SPK */
/*        required reading from $Literature_References to */
/*        $Required_Reading section. */

/* -    SPICELIB Version 1.1.1, 18-JAN-2014 (NJB) */

/*        Enhanced header and in-line documentation. */

/* -    SPICELIB Version 1.1.0, 07-SEP-2001 (EDW) */

/*        Replaced DAFRDA call with DAFGDA. */
/*        Added IMPLICIT NONE. */

/* -    SPICELIB Version 1.0.3, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.2, 23-AUG-1991 (HAN) */

/*        SPK02 was removed from the $Required_Reading section of the */
/*        header. The information in the SPK02 Required Reading file */
/*        is now part of the SPK Required Reading file. */

/* -    SPICELIB Version 1.0.1, 22-MAR-1990 (HAN) */

/*        Literature references added to the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU) */

/* -& */
/* $ Index_Entries */

/*     read record from type_2 SPK segment */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("SPKR02", (ftnlen)6);
    }

/*     Unpack the segment descriptor. */

    dafus_(descr, &c__2, &c__6, dc, ic);
    begin = ic[4];
    end = ic[5];

/*     The segment is made up of a number of logical records, each */
/*     having the same size, and covering the same length of time. */

/*     We can determine which record to return using the input epoch, */
/*     the initial time of the first record's coverage interval, and the */
/*     length of the interval covered by each record. These constants */
/*     are located at the end of the segment, along with the size of */
/*     each logical record and the total number of records. */

    i__1 = end - 3;
    dafgda_(handle, &i__1, &end, record);
    init = record[0];
    intlen = record[1];
    recsiz = (integer) record[2];
    nrec = (integer) record[3];
    recno = (integer) ((*et - init) / intlen) + 1;
    recno = min(recno,nrec);

/*     Compute the address of the desired record. */

    recadr = (recno - 1) * recsiz + begin;

/*     Along with the record, return the size of the record. */

    record[0] = record[2];
    i__1 = recadr + recsiz - 1;
    dafgda_(handle, &recadr, &i__1, &record[1]);
    chkout_("SPKR02", (ftnlen)6);
    return 0;
} /* spkr02_ */

