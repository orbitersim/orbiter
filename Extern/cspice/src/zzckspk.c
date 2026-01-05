/* zzckspk.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__2 = 2;
static integer c__6 = 6;
static integer c__72 = 72;
static integer c__100 = 100;
static integer c__0 = 0;
static integer c__10 = 10;
static integer c__1 = 1;

/* $Procedure      ZZCKSPK ( SPK or CK ) */
/* Subroutine */ int zzckspk_(integer *handle, char *ckspk, ftnlen ckspk_len)
{
    /* System generated locals */
    integer i__1, i__2;

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    integer from, last, size, nspk, type__;
    logical ck2ok;
    extern /* Subroutine */ int zzsizeok_(integer *, integer *, integer *, 
	    integer *, logical *, integer *), dafgs_(doublereal *), chkin_(
	    char *, ftnlen), dafus_(doublereal *, integer *, integer *, 
	    doublereal *, integer *);
    logical found;
    doublereal times[2];
    integer first;
    logical spkok;
    extern /* Subroutine */ int dafgda_(integer *, integer *, integer *, 
	    doublereal *);
    doublereal dc[2];
    integer ic[6];
    extern /* Subroutine */ int daffna_(logical *);
    extern logical failed_(void);
    extern /* Subroutine */ int dafbfs_(integer *), dafhsf_(integer *, 
	    integer *, integer *);
    integer to;
    doublereal chcktm;
    integer angvel;
    doublereal lastdp;
    integer thisnd;
    extern /* Subroutine */ int chkout_(char *, ftnlen);
    integer thisni;
    extern logical return_(void);
    doublereal frsttm, sum[5];
    integer nck2;

/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines.  Users should not call this routine directly due */
/*     to the volatile nature of this routine. */

/*     This routine determines whether or not a DAF file attached to */
/*     the supplied handle is an SPK, CK or unknown file. */

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

/*      None. */

/* $ Keywords */

/*      PRIVATE */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     HANDLE     I   the handle of a DAF file open for read access. */
/*     CKSPK      O   the type of the DAF file (SPK,CK or ?) */

/* $ Detailed_Input */

/*     HANDLE     is the handle of a DAF file open for read. */

/* $ Detailed_Output */

/*     CKSPK      is a string containing one of the following 3 values */
/*                'SPK', 'CK' or '?' */

/* $ Parameters */

/*     None. */

/* $ Files */

/*     None. */

/* $ Exceptions */

/*     None. */

/* $ Particulars */

/*     This routine examines the first segment of a DAF that is */
/*     a candidate for being an SPK or CK and returns a diagnosis */
/*     of the type of the file. */

/* $ Examples */

/*     None. */

/* $ Restrictions */

/*     None. */

/* $ Author_and_Institution */

/*     W.L. Taber      (JPL) */

/* $ Literature_References */

/*     None. */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 07-SEP-2001 (EDW) */

/*        Replaced DAFRDA call with DAFGDA. */
/*        Added IMPLICIT NONE. */

/* -    SPICELIB Version 1.0.0, 03-DEC-1999 (WLT) */

/* -& */

/*     Local parameters */


/*     The following parameters point to the various slots in the */
/*     integer portion of the DAF descriptor where the values are */
/*     located. */


/*     These parameters give the number of integer and double precision */
/*     components of the descriptor for SPK and CK files. */


/*     The size of a summary. */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }
    chkin_("ZZCKSPK", (ftnlen)7);

/*     Make sure the values of ND and NI associated with this file */
/*     have the correct values. */

    dafhsf_(handle, &thisnd, &thisni);
    if (thisnd != 2 || thisni != 6) {
	s_copy(ckspk, "?", ckspk_len, (ftnlen)1);
	chkout_("ZZCKSPK", (ftnlen)7);
	return 0;
    }

/*     We've got the correct values for ND and NI, examine the descriptor */
/*     for the first array. */

    dafbfs_(handle);
    daffna_(&found);
    if (failed_()) {
	s_copy(ckspk, "?", ckspk_len, (ftnlen)1);
	chkout_("ZZCKSPK", (ftnlen)7);
	return 0;
    }

/*     If we don't find any segments, we don't have a clue about */
/*     the file type. */

    if (! found) {
	s_copy(ckspk, "?", ckspk_len, (ftnlen)1);
	chkout_("ZZCKSPK", (ftnlen)7);
	return 0;
    }

/*     Unpack the summary record. */

    dafgs_(sum);
    dafus_(sum, &c__2, &c__6, dc, ic);

/*     Look at the slot where the angular velocity flag would */
/*     be located if this is a CK file. */

    angvel = ic[3];
    type__ = ic[2];

/*     Test 1. The value of ANGVEL may do the trick */
/*     right at the start. */

    if (angvel == 0) {
	s_copy(ckspk, "CK", ckspk_len, (ftnlen)2);
	chkout_("ZZCKSPK", (ftnlen)7);
	return 0;
    }
    if (angvel > 1) {
	s_copy(ckspk, "SPK", ckspk_len, (ftnlen)3);
	chkout_("ZZCKSPK", (ftnlen)7);
	return 0;
    }

/*     Test 2. If this is an SPK file, it has a type 01 segment. */
/*     See if this is something orbiting the solar system */
/*     barycenter. */

    if (ic[1] == 0) {
	s_copy(ckspk, "SPK", ckspk_len, (ftnlen)3);
	chkout_("ZZCKSPK", (ftnlen)7);
	return 0;
    }

/*     Test 3. This is the super test.  Compute the size of the */
/*     segment and fetch the last d.p. from the segment. */

    first = ic[4];
    last = ic[5];
    size = last - first + 1;

/*     Check the size of the array to see if it has any chance */
/*     of being an SPK and if it does get the number of MDA records. */

    i__1 = size - 1;
    zzsizeok_(&i__1, &c__72, &c__100, &c__0, &spkok, &nspk);
    if (! spkok) {
	s_copy(ckspk, "CK", ckspk_len, (ftnlen)2);
	chkout_("ZZCKSPK", (ftnlen)7);
	return 0;
    }
    dafgda_(handle, &last, &last, &lastdp);

/*     See if the last number in the file is the allowed number of */
/*     MDA records.  If not, this must be a CK segment. */

    if (lastdp != (doublereal) nspk) {
	s_copy(ckspk, "CK", ckspk_len, (ftnlen)2);
	chkout_("ZZCKSPK", (ftnlen)7);
	return 0;
    }

/*     If we are still here, the last d.p. in the segment matches the */
/*     expected number of MDA records.  If the potential CK type is */
/*     not 2, we must have an SPK file. */

    if (type__ != 2) {
	s_copy(ckspk, "SPK", ckspk_len, (ftnlen)3);
	chkout_("ZZCKSPK", (ftnlen)7);
	return 0;
    }

/*     We are getting down to the nitty gritty here. See if the */
/*     size is compatible with a type 02 C-kernel. */

    zzsizeok_(&size, &c__10, &c__100, &c__1, &ck2ok, &nck2);
    if (! ck2ok) {
	s_copy(ckspk, "SPK", ckspk_len, (ftnlen)3);
	chkout_("ZZCKSPK", (ftnlen)7);
	return 0;
    }

/*     So much for being nice. We need to examine the structure of the */
/*     actual data in the segment.  There are two cases to consider: */
/*     when there is 1 or fewer type 02 CK directory records and when */
/*     there is more than 1.  Note that to get to this point there must */
/*     be at least 1 directory value if this is a CK type 02 segment. */
/*     (To see this check the sizes when ZZSIZEOK returns TRUE for */
/*     both type 1 SPK and type 02 CK. The only such sizes in which */
/*     there the number CK type 02 directory values is one or fewer */
/*     are SIZE = 1081, 1441, and 1801 which correspond to (NSPK,NCK2) = */
/*     (15,108), (20,144), (25, 180).  In all of these cases there is */
/*     exactly 1 ck type 02 directory value.) */

    if (nck2 < 201) {

/*        Recall that MDA record contains its stop time as the first */
/*        entry of the record.  These epochs show up duplicated in the */
/*        epochs portion of the segment. */

/*        If this is a type 01 SPK segment, there are no directory */
/*        records and the first epoch shows up in the slot NSPK before */
/*        the last slot of the segment.  If it is a type 02 CK segment */
/*        the last stop tick shows up in this slot.  We need to look */
/*        at this value to see what's up. */

	i__1 = last - nspk;
	i__2 = last - nspk;
	dafgda_(handle, &i__1, &i__2, &frsttm);

/*        Now (under the assumption that we have an SPK segment) look */
/*        up the epoch from the last MDA record--- the NSPK'th */
/*        record.  This epoch must be greater than the first epoch */
/*        in the array of epochs. */
	from = first + (nspk - 1) * 71;
	to = from;
	dafgda_(handle, &from, &to, &chcktm);

/*        If this is a type 02 segment.  The value we just picked out */
/*        will come from the array of stop ticks.  The array of stop */
/*        ticks is non-decreasing so: */

	if (chcktm > frsttm) {
	    s_copy(ckspk, "SPK", ckspk_len, (ftnlen)3);
	} else {
	    s_copy(ckspk, "CK", ckspk_len, (ftnlen)2);
	}
    } else {

/*        In this case there are at least 2 directory records if we */
/*        have a CK.  We read the last potential tick value and the */
/*        first potential directory value..  Note that the last potential */
/*        stop tick must be greater than the first potential directory */
/*        record. */

	from = last - (nck2 - 1) / 100;
	to = from + 1;
	dafgda_(handle, &from, &to, times);

/*        If we happen to have a TYPE 01 SPK segment we've just */
/*        read two consecutive values from the epochs sub-array of the */
/*        segment. Here's a sketch of why this is so: */

/*          The number of directory records for a CK type 02 segment is */
/*          (NCK2-1)/100  which is the same as SIZE/1001. */

/*          The number of directory records for an SPK type 01 segment is */
/*          (NSPK-1)/100  which is the same as SIZE/7201. */

/*          The number of stop ticks for type 02 CK is NCK2 ~ SIZE/10 */

/*          The number of epochs for a type 01 SPK is  NSPK ~ SIZE/72 */

/*        so NSPK directories < NCK2 directories < NCK2 directories + 1 */
/*        < NSPK + NSPK directories < NCK2.  Consequently, the */
/*        two values just read are either the last stop tick and the */
/*        first CK directory value or two consecutive epochs. */
/*        In the first case TIMES(1) > TIMES(2), in the later case */
/*        we have TIMES(1) < TIMES(2) */

	if (times[0] > times[1]) {
	    s_copy(ckspk, "CK", ckspk_len, (ftnlen)2);
	} else {
	    s_copy(ckspk, "SPK", ckspk_len, (ftnlen)3);
	}
    }
    chkout_("ZZCKSPK", (ftnlen)7);
    return 0;
} /* zzckspk_ */

