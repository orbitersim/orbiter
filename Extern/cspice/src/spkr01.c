/* spkr01.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__2 = 2;
static integer c__6 = 6;
static integer c__100 = 100;

/* $Procedure SPKR01 ( Read SPK record from segment, type 1 ) */
/* Subroutine */ int spkr01_(integer *handle, doublereal *descr, doublereal *
	et, doublereal *record)
{
    /* System generated locals */
    integer i__1, i__2, i__3;

    /* Local variables */
    doublereal data[100];
    integer offd, offe, nrec, ndir, offr, i__, begin;
    extern /* Subroutine */ int chkin_(char *, ftnlen), dafus_(doublereal *, 
	    integer *, integer *, doublereal *, integer *);
    integer recno;
    extern /* Subroutine */ int dafgda_(integer *, integer *, integer *, 
	    doublereal *);
    doublereal dc[2];
    integer ic[6];
    extern /* Subroutine */ int chkout_(char *, ftnlen);
    extern integer lstltd_(doublereal *, integer *, doublereal *);
    extern logical return_(void);
    integer end, off;

/* $ Abstract */

/*     Read a single SPK data record from a segment of type 1 */
/*     (Difference Lines). */

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
/*              a SPK segment of type 1. */

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

/*     1)  If an error occurs while looking up SPK data, the error is */
/*         signaled by a routine in the call tree of this routine. */

/* $ Files */

/*     See argument HANDLE. */

/* $ Particulars */

/*     See the SPK Required Reading file for a description of the */
/*     structure of a data type 1 segment. */

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

/*           IF ( TYPE .EQ. 1 ) THEN */
/*              CALL SPKR01 ( HANDLE, DESCR, ET, RECORD ) */
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
/*     H.A. Neilan        (JPL) */
/*     W.L. Taber         (JPL) */
/*     I.M. Underwood     (JPL) */
/*     E.D. Wright        (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.1, 14-APR-2021 (JDR) */

/*        Edited the header to comply with NAIF standard. Added entry #1 */
/*        to $Exceptions section. Moved SPK required reading from */
/*        $Literature_References to $Required_Reading section. */

/* -    SPICELIB Version 1.1.0, 07-SEP-2001 (EDW) */

/*        Replaced DAFRDA call with DAFGDA. */

/* -    SPICELIB Version 1.0.3, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.2, 23-AUG-1991 (HAN) */

/*        SPK01 was removed from the $Required_Reading section of the */
/*        header. The information in the SPK01 Required Reading file */
/*        is now part of the SPK Required Reading file. */

/* -    SPICELIB Version 1.0.1, 22-MAR-1990 (HAN) */

/*        Literature references added to the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU) */

/* -& */
/* $ Index_Entries */

/*     read record from type_1 SPK segment */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("SPKR01", (ftnlen)6);
    }

/*     Unpack the segment descriptor. */

    dafus_(descr, &c__2, &c__6, dc, ic);
    begin = ic[4];
    end = ic[5];

/*     Get the number of records in the segment. From that, we can */
/*     compute */

/*        NDIR      The number of directory epochs. */

/*        OFFD      The offset of the first directory epoch. */

/*        OFFE      The offset of the first epoch. */


/*     the number of directory epochs. */

    dafgda_(handle, &end, &end, data);
    nrec = (integer) data[0];
    ndir = nrec / 100;
    offd = end - ndir - 1;
    offe = end - ndir - nrec - 1;

/*     What we want is the record number: once we have that, we can */
/*     compute the offset of the record from the beginning of the */
/*     segment, grab it, and go. But how to find it? */

/*     Ultimately, we want the first record whose epoch is greater */
/*     than or equal to ET. If there are 100 or fewer records, all */
/*     the record epochs can be examined in a single group. */

    if (nrec <= 100) {
	i__1 = offe + 1;
	i__2 = offe + nrec;
	dafgda_(handle, &i__1, &i__2, data);
	recno = lstltd_(et, &nrec, data) + 1;
	offr = begin - 1 + (recno - 1) * 71;
	i__1 = offr + 1;
	i__2 = offr + 71;
	dafgda_(handle, &i__1, &i__2, record);
	chkout_("SPKR01", (ftnlen)6);
	return 0;
    }

/*     Searching directories is a little more difficult. */

/*     The directory contains epochs 100, 200, and so on. Once we */
/*     find the first directory epoch greater than or equal to ET, */
/*     we can grab the corresponding set of 100 record epochs, and */
/*     search them. */

    i__1 = ndir;
    for (i__ = 1; i__ <= i__1; ++i__) {
	i__2 = offd + i__;
	i__3 = offd + i__;
	dafgda_(handle, &i__2, &i__3, data);
	if (data[0] >= *et) {
	    off = offe + (i__ - 1) * 100;
	    i__2 = off + 1;
	    i__3 = off + 100;
	    dafgda_(handle, &i__2, &i__3, data);
	    recno = (i__ - 1) * 100 + lstltd_(et, &c__100, data) + 1;
	    offr = begin - 1 + (recno - 1) * 71;
	    i__2 = offr + 1;
	    i__3 = offr + 71;
	    dafgda_(handle, &i__2, &i__3, record);
	    chkout_("SPKR01", (ftnlen)6);
	    return 0;
	}
    }

/*     If ET is greater than the final directory epoch, we want one */
/*     of the final records. */

    i__ = nrec % 100;
    i__1 = end - ndir - i__;
    i__2 = end - ndir - 1;
    dafgda_(handle, &i__1, &i__2, data);
    recno = ndir * 100 + lstltd_(et, &i__, data) + 1;
    offr = begin - 1 + (recno - 1) * 71;
    i__1 = offr + 1;
    i__2 = offr + 71;
    dafgda_(handle, &i__1, &i__2, record);
    chkout_("SPKR01", (ftnlen)6);
    return 0;
} /* spkr01_ */

