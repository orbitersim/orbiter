/* spkr21.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__2 = 2;
static integer c__6 = 6;
static integer c__25 = 25;
static integer c__100 = 100;

/* $Procedure SPKR21 ( Read SPK record from segment, type 21 ) */
/* Subroutine */ int spkr21_(integer *handle, doublereal *descr, doublereal *
	et, doublereal *record)
{
    /* System generated locals */
    integer i__1, i__2, i__3;

    /* Builtin functions */
    integer i_dnnt(doublereal *);

    /* Local variables */
    doublereal data[100];
    integer offd, offe, nrec, ndir, offr, i__, begin;
    extern /* Subroutine */ int chkin_(char *, ftnlen), dafus_(doublereal *, 
	    integer *, integer *, doublereal *, integer *);
    integer recno;
    extern /* Subroutine */ int dafgda_(integer *, integer *, integer *, 
	    doublereal *);
    doublereal dc[2];
    integer ic[6], maxdim, dflsiz;
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), setmsg_(char *, ftnlen), errint_(char *, integer *, 
	    ftnlen);
    extern integer lstltd_(doublereal *, integer *, doublereal *);
    extern logical return_(void);
    integer end, off;

/* $ Abstract */

/*     Read a single SPK data record from a segment of type 21 */
/*     (Extended Difference Lines). */

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
/*     TIME */

/* $ Keywords */

/*     EPHEMERIS */

/* $ Declarations */
/* $ Abstract */

/*     Declare parameters specific to SPK type 21. */

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

/*     SPK */

/* $ Restrictions */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman      (JPL) */

/* $ Literature_References */

/*     None. */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 25-DEC-2013 (NJB) */

/* -& */

/*     MAXTRM      is the maximum number of terms allowed in each */
/*                 component of the difference table contained in a type */
/*                 21 SPK difference line. MAXTRM replaces the fixed */
/*                 table parameter value of 15 used in SPK type 1 */
/*                 segments. */

/*                 Type 21 segments have variable size. Let MAXDIM be */
/*                 the dimension of each component of the difference */
/*                 table within each difference line. Then the size */
/*                 DLSIZE of the difference line is */

/*                    ( 4 * MAXDIM ) + 11 */

/*                 MAXTRM is the largest allowed value of MAXDIM. */



/*     End of include file spk21.inc. */

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
/*              a SPK segment of type 21. */

/*     ET       is an epoch for which a data record from a specific */
/*              segment is required. The epoch is represented as */
/*              seconds past J2000 TDB. */

/* $ Detailed_Output */

/*     RECORD   is a data record which, when evaluated at epoch ET, */
/*              will give the state (position and velocity) of an */
/*              ephemeris object, relative to its center of motion, */
/*              in an inertial reference frame. */

/*              The contents of RECORD are as follows: */

/*                 RECORD(1):         The difference table size per */
/*                                    Cartesian component. Call this */
/*                                    size MAXDIM; then the difference */
/*                                    line (MDA) size DLSIZE is */

/*                                      ( 4 * MAXDIM ) + 11 */

/*                 RECORD(2) */
/*                    ... */
/*                 RECORD(1+DLSIZE):  An extended difference line. */
/*                                    The contents are: */

/*                    Dimension  Description */
/*                    ---------  ---------------------------------- */
/*                    1          Reference epoch of difference line */
/*                    MAXDIM     Stepsize function vector */
/*                    1          Reference position vector,  x */
/*                    1          Reference velocity vector,  x */
/*                    1          Reference position vector,  y */
/*                    1          Reference velocity vector,  y */
/*                    1          Reference position vector,  z */
/*                    1          Reference velocity vector,  z */
/*                    MAXDIM,3   Modified divided difference */
/*                               arrays (MDAs) */
/*                    1          Maximum integration order plus 1 */
/*                    3          Integration order array */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the maximum table size of the input record exceeds */
/*         MAXTRM, the error SPICE(DIFFLINETOOLARGE) is signaled. */

/*     2)  If an error occurs while looking up SPK data, the error is */
/*         signaled by a routine in the call tree of this routine. */

/* $ Files */

/*     See argument HANDLE. */

/* $ Particulars */

/*     See the SPK Required Reading file for a description of the */
/*     structure of a data type 21 segment. */

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
/*              CALL SPKR21 ( HANDLE, DESCR, ET, RECORD ) */
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
/*     F.T. Krogh         (JPL) */
/*     W.L. Taber         (JPL) */
/*     I.M. Underwood     (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.1, 14-APR-2021 (JDR) */

/*        Edited the header to comply with NAIF standard. Moved SPK */
/*        required reading from $Literature_References to */
/*        $Required_Reading section. */

/* -    SPICELIB Version 1.0.0, 16-JAN-2014 (NJB) (FTK) (WLT) (IMU) */

/* -& */
/* $ Index_Entries */

/*     read record from type_21 SPK segment */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     Local variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }
    chkin_("SPKR21", (ftnlen)6);

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

/*     We'll fetch the difference table dimension as well. */

    i__1 = end - 1;
    dafgda_(handle, &i__1, &end, data);
    nrec = i_dnnt(&data[1]);
    ndir = nrec / 100;
    offd = end - ndir - 2;
    offe = offd - nrec;
    maxdim = i_dnnt(data);
    if (maxdim > 25) {
	setmsg_("The input record has a maximum table dimension of #, while "
		"the maximum supported by this routine is #. It is possible t"
		"hat this problem is due to your SPICE Toolkit being out of d"
		"ate.", (ftnlen)183);
	errint_("#", &maxdim, (ftnlen)1);
	errint_("#", &c__25, (ftnlen)1);
	sigerr_("SPICE(DIFFLINETOOLARGE)", (ftnlen)23);
	chkout_("SPKR21", (ftnlen)6);
	return 0;
    }

/*     The difference line dimension per component is the */
/*     first element of the output record. */

    record[0] = (doublereal) maxdim;

/*     Set the difference line size. */

    dflsiz = (maxdim << 2) + 11;

/*     What we want is the record number: once we have that, we can */
/*     compute the offset of the record from the beginning of the */
/*     segment, grab it, and go. But how to find it? */

/*     Ultimately, we want the first record whose epoch is greater */
/*     than or equal to ET. If there are BUFSIZ or fewer records, all */
/*     the record epochs can be examined in a single group. */

    if (nrec <= 100) {
	i__1 = offe + 1;
	i__2 = offe + nrec;
	dafgda_(handle, &i__1, &i__2, data);
	recno = lstltd_(et, &nrec, data) + 1;
	offr = begin - 1 + (recno - 1) * dflsiz;
	i__1 = offr + 1;
	i__2 = offr + dflsiz;
	dafgda_(handle, &i__1, &i__2, &record[1]);
	chkout_("SPKR21", (ftnlen)6);
	return 0;
    }

/*     Searching directories is a little more difficult. */

/*     The directory contains epochs BUFSIZ, 2*BUFSIZ, and so on. Once */
/*     we find the first directory epoch greater than or equal to ET, we */
/*     can grab the corresponding set of BUFSIZ record epochs, and */
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
	    offr = begin - 1 + (recno - 1) * dflsiz;
	    i__2 = offr + 1;
	    i__3 = offr + dflsiz;
	    dafgda_(handle, &i__2, &i__3, &record[1]);
	    chkout_("SPKR21", (ftnlen)6);
	    return 0;
	}
    }

/*     If ET is greater than the final directory epoch, we want one */
/*     of the final records. */

    i__ = nrec % 100;
    i__1 = end - ndir - i__ - 1;
    i__2 = end - ndir - 2;
    dafgda_(handle, &i__1, &i__2, data);
    recno = ndir * 100 + lstltd_(et, &i__, data) + 1;
    offr = begin - 1 + (recno - 1) * dflsiz;
    i__1 = offr + 1;
    i__2 = offr + dflsiz;
    dafgda_(handle, &i__1, &i__2, &record[1]);
    chkout_("SPKR21", (ftnlen)6);
    return 0;
} /* spkr21_ */

