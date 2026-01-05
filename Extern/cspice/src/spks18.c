/* spks18.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;

/* $Procedure SPKS18 ( S/P Kernel, subset, type 18 ) */
/* Subroutine */ int spks18_(integer *handle, integer *baddr, integer *eaddr, 
	doublereal *begin, doublereal *end)
{
    /* System generated locals */
    integer i__1, i__2, i__3;
    doublereal d__1;

    /* Builtin functions */
    integer i_dnnt(doublereal *);

    /* Local variables */
    doublereal data[12];
    integer offe, nrec, ndir, i__;
    extern /* Subroutine */ int chkin_(char *, ftnlen), dafada_(doublereal *, 
	    integer *), dafgda_(integer *, integer *, integer *, doublereal *)
	    ;
    integer wnszm1, offset, packsz;
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), setmsg_(char *, ftnlen), errint_(char *, integer *, 
	    ftnlen);
    extern logical return_(void);
    integer winsiz, subtyp, rec[2];

/* $ Abstract */

/*     Extract a subset of the data in an SPK segment of type 18 */
/*     into a new segment. */

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
/*     DAF */

/* $ Keywords */

/*     EPHEMERIS */

/* $ Declarations */
/* $ Abstract */

/*     Declare parameters specific to SPK type 18. */

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

/* -    SPICELIB Version 1.0.0, 18-AUG-2002 (NJB) */

/* -& */

/*     SPK type 18 subtype codes: */


/*     Subtype 0:  Hermite interpolation, 12-element packets, order */
/*                 reduction at boundaries to preceding number */
/*                 equivalent to 3 mod 4. */


/*     Subtype 1:  Lagrange interpolation, 6-element packets, order */
/*                 reduction at boundaries to preceding odd number. */


/*     Packet sizes associated with the various subtypes: */


/*     End of include file spk18.inc. */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     HANDLE     I   Handle of file containing source segment. */
/*     BADDR      I   Beginning address in file of source segment. */
/*     EADDR      I   Ending address in file of source segment. */
/*     BEGIN      I   Beginning (initial epoch) of subset. */
/*     END        I   End (final epoch) of subset. */

/* $ Detailed_Input */

/*     HANDLE, */
/*     BADDR, */
/*     EADDR    are the file handle assigned to an SPK file, and the */
/*              beginning and ending addresses of a segment within */
/*              that file. Together they determine a complete set of */
/*              ephemeris data, from which a subset is to be */
/*              extracted. */

/*     BEGIN, */
/*     END      are the initial and final epochs (ephemeris time) */
/*              of the subset. */

/*              The first epoch for which there will be ephemeris */
/*              data in the new segment will be the greatest time */
/*              in the source segment that is less than or equal */
/*              to BEGIN. */

/*              The last epoch for which there will be ephemeris */
/*              data in the new segment will be the smallest time */
/*              in the source segment that is greater than or equal */
/*              to END. */

/* $ Detailed_Output */

/*     See $Files section. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  This routine relies on the caller to ensure that the */
/*         interval [BEGIN, END] is contained in the coverage */
/*         interval of the segment. */

/*     2)  If BEGIN > END, no data are written to the target file. */

/*     3)  If an unexpected SPK type 18 subtype is found in the input */
/*         segment, the error SPICE(INVALIDVALUE) is signaled. */

/* $ Files */

/*     Data are extracted from the file connected to the input */
/*     handle, and written to the current DAF open for writing. */

/*     The segment descriptor and summary must already have been written */
/*     prior to calling this routine. The segment must be ended */
/*     external to this routine. */

/* $ Particulars */

/*     This routine is intended solely for use as a utility by the */
/*     routine SPKSUB. */

/*     It transfers a subset of a type 18 SPK data segment to */
/*     a properly initialized segment of a second SPK file. */

/*     The exact structure of a segment of data type 18 is described */
/*     in the section on type 18 in the SPK Required Reading. */

/* $ Examples */

/*     This routine is intended only for use as a utility by SPKSUB. */
/*     To use this routine successfully, you must: */

/*        Open the SPK file from which to extract data. */
/*        Locate the segment from which data should be extracted. */

/*        Open the SPK file to which this data should be written. */
/*        Begin a new segment (array). */
/*        Write the summary information for the array. */

/*        Call this routine to extract the appropriate data from the */
/*        SPK open for read. */

/*        End the array to which this routine writes data. */

/*     Much of this procedure is carried out by the routine SPKSUB. The */
/*     examples of that routine illustrate more fully the process */
/*     described above. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     W.L. Taber         (JPL) */
/*     I.M. Underwood     (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.1, 03-JUN-2021 (JDR) */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 1.1.0, 22-DEC-2012 (NJB) */

/*        Bug fix: code applicable to SPK type 9 for */
/*        creating padding in the output segment was */
/*        deleted. */

/* -    SPICELIB Version 1.0.0, 16-AUG-2002 (NJB) (WLT) (IMU) */

/* -& */
/* $ Index_Entries */

/*     subset type_18 SPK segment */

/* -& */
/* $ Revisions */

/* -    SPICELIB Version 1.1.0, 22-DEC-2012 (NJB) */

/*        Bug fix: code applicable to SPK type 9 for */
/*        creating padding in the output segment was */
/*        deleted. */

/*        The offending code was meant to ensure that */
/*        the output segment's size is at least the */
/*        window size corresponding to the segment's */
/*        interpolation degree. This is correct behavior */
/*        for SPK types 9 and 13; for these types, */
/*        segments are not allowed to have sizes less */
/*        than the nominal window size. */

/*        However, for type 18, segments can have as */
/*        few as two data packets, regardless of their */
/*        interpolation degree. The code that creates */
/*        padding packets in this case reads from */
/*        invalid locations. */

/*        Also, the variable WINSIZ was introduced, and */
/*        comments indicating that the stored size */
/*        parameter in the segment control area is the */
/*        window size minus one were corrected. */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     Local variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("SPKS18", (ftnlen)6);
    }

/*     See whether there's any work to do; return immediately if not. */

    if (*begin > *end) {
	chkout_("SPKS18", (ftnlen)6);
	return 0;
    }

/*     Read the segment structure metadata. */

/*     Get the type 18 segment subtype.  Next get the quantity "window */
/*     size." Also get the number of records in the segment. */

    i__1 = *eaddr - 2;
    dafgda_(handle, &i__1, eaddr, data);
    subtyp = i_dnnt(data);
    winsiz = i_dnnt(&data[1]);
    nrec = i_dnnt(&data[2]);

/*     Set the packet size based on the subtype. */

    if (subtyp == 0) {
	packsz = 12;
    } else if (subtyp == 1) {
	packsz = 6;
    } else {
	setmsg_("Unexpected SPK type 18 subtype found in type 18 record.", (
		ftnlen)55);
	errint_("#", &subtyp, (ftnlen)1);
	sigerr_("SPICE(INVALIDVALUE)", (ftnlen)19);
	chkout_("SPKS18", (ftnlen)6);
	return 0;
    }

/*     From the number of records, we can compute */

/*        NDIR      The number of directory epochs. */

/*        OFFE      The offset of the first epoch. */

    ndir = (nrec - 1) / 100;
    offe = *eaddr - ndir - nrec - 3;

/*     Examine the epochs in forward order, looking for the first */
/*     epoch greater than or equal to END (or the final epoch, */
/*     whichever comes first). This epoch corresponds to the last */
/*     state to be transferred. */

    rec[1] = 1;
    i__1 = offe + rec[1];
    i__2 = offe + rec[1];
    dafgda_(handle, &i__1, &i__2, data);
    while(rec[1] < nrec && data[0] < *end) {
	++rec[1];
	i__1 = offe + rec[1];
	i__2 = offe + rec[1];
	dafgda_(handle, &i__1, &i__2, data);
    }

/*     Let WNSZM1 be one less than the window size. */

/*     Make sure that there are WNSZM1/2 additional states to the right */
/*     of the one having index REC(2), if possible.  If not, take as */
/*     many states as we can. */

    wnszm1 = winsiz - 1;
/* Computing MIN */
    i__1 = nrec, i__2 = rec[1] + wnszm1 / 2;
    rec[1] = min(i__1,i__2);

/*     Now examine the epochs in reverse order, looking for the first */
/*     epoch less than or equal to BEGIN (or the initial epoch, */
/*     whichever comes first). This epoch corresponds to the first */
/*     state to be transferred. */

    rec[0] = nrec;
    i__1 = offe + rec[0];
    i__2 = offe + rec[0];
    dafgda_(handle, &i__1, &i__2, data);
    while(rec[0] > 1 && data[0] > *begin) {
	--rec[0];
	i__1 = offe + rec[0];
	i__2 = offe + rec[0];
	dafgda_(handle, &i__1, &i__2, data);
    }

/*     Make sure that there are WNSZM1/2 additional states to the left */
/*     of the one having index REC(1), if possible.  If not, take as */
/*     many states as we can. */

/* Computing MAX */
    i__1 = 1, i__2 = rec[0] - wnszm1 / 2;
    rec[0] = max(i__1,i__2);

/*     Copy states REC(1) through REC(2) to the output file. */

    i__1 = rec[1];
    for (i__ = rec[0]; i__ <= i__1; ++i__) {
	offset = *baddr - 1 + (i__ - 1) * packsz;
	i__2 = offset + 1;
	i__3 = offset + packsz;
	dafgda_(handle, &i__2, &i__3, data);
	dafada_(data, &packsz);
    }

/*     Copy epochs REC(1) through REC(2) to the output file. */

    i__1 = rec[1];
    for (i__ = rec[0]; i__ <= i__1; ++i__) {
	i__2 = offe + i__;
	i__3 = offe + i__;
	dafgda_(handle, &i__2, &i__3, data);
	dafada_(data, &c__1);
    }

/*     Put every 100'th epoch into the directory, except the last */
/*     epoch, if that epoch's index would be a multiple of 100. */

    i__1 = rec[1] - 1;
    for (i__ = rec[0] + 99; i__ <= i__1; i__ += 100) {
	i__2 = offe + i__;
	i__3 = offe + i__;
	dafgda_(handle, &i__2, &i__3, data);
	dafada_(data, &c__1);
    }

/*     Store subtype, the window size, and the number of */
/*     records to end the segment. */

    d__1 = (doublereal) subtyp;
    dafada_(&d__1, &c__1);
    d__1 = (doublereal) winsiz;
    dafada_(&d__1, &c__1);
    d__1 = (doublereal) (rec[1] - rec[0] + 1);
    dafada_(&d__1, &c__1);
    chkout_("SPKS18", (ftnlen)6);
    return 0;
} /* spks18_ */

