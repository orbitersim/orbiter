/* spks01.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__71 = 71;
static integer c__1 = 1;

/* $Procedure SPKS01 ( S/P Kernel, subset, type 1 ) */
/* Subroutine */ int spks01_(integer *handle, integer *baddr, integer *eaddr, 
	doublereal *begin, doublereal *end)
{
    /* System generated locals */
    integer i__1, i__2, i__3;

    /* Local variables */
    doublereal data[71];
    integer offe, nrec, ndir, last, i__;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    integer first;
    extern /* Subroutine */ int dafada_(doublereal *, integer *), dafgda_(
	    integer *, integer *, integer *, doublereal *);
    integer offset;
    extern /* Subroutine */ int chkout_(char *, ftnlen);
    extern logical return_(void);

/* $ Abstract */

/*     Extract a subset of the data in a SPK segment of type 1 */
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

/* $ Keywords */

/*     EPHEMERIS */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     HANDLE     I   Handle of source segment. */
/*     BADDR      I   Beginning address of source segment. */
/*     EADDR      I   Ending address of source segment. */
/*     BEGIN      I   Beginning (initial epoch) of subset. */
/*     END        I   End (final epoch) of subset. */

/* $ Detailed_Input */

/*     HANDLE, */
/*     BADDR, */
/*     EADDR    are the file handle assigned to a SPK file, and the */
/*              beginning and ending addresses of a segment within */
/*              the file. Together they determine a complete set of */
/*              ephemeris data, from which a subset is to be */
/*              extracted. */

/*     BEGIN, */
/*     END      are the initial and final epochs (ephemeris time) */
/*              of the subset. */

/* $ Detailed_Output */

/*     None. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If an error occurs while reading data from the source SPK */
/*         file, the error is signaled by a routine in the call tree of */
/*         this routine. */

/*     2)  If an error occurs while writing data to the output SPK file, */
/*         the error is signaled by a routine in the call tree of this */
/*         routine. */

/* $ Files */

/*     See argument HANDLE. */

/* $ Particulars */

/*     The exact structure of a segment of data type 1 is detailed in */
/*     the SPK Required Reading file. */

/* $ Examples */

/*     None. */

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

/*        Edited the header to comply with NAIF standard. Completed */
/*        $Exceptions section. Moved SPK required reading from */
/*        $Literature_References to $Required_Reading section. */

/* -    SPICELIB Version 1.1.0, 07-SEP-2001 (EDW) */

/*        Replaced DAFRDA call with DAFGDA. */
/*        Added IMPLICIT NONE. */

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

/*     subset type_1 SPK segment */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("SPKS01", (ftnlen)6);
    }

/*     Get the number of records in the segment. From that, we can */
/*     compute */

/*        NDIR      The number of directory epochs. */

/*        OFFE      The offset of the first epoch. */


/*     the number of directory epochs. */

    dafgda_(handle, eaddr, eaddr, data);
    nrec = (integer) data[0];
    ndir = nrec / 100;
    offe = *eaddr - ndir - nrec - 1;

/*     Well, the new segment has already been begun. We just have to */
/*     decide what to move, and move it (using DAFADA). */

/*     Let's agree right now that speed is not of the greatest */
/*     importance here. We can probably do this with two passes */
/*     through the record epochs, and one pass through the records. */

/*        1) Determine the first and last records to be included */
/*           in the subset. */

/*        2) Move the records. */

/*        3) Write the epochs. */

/*     We can leap through the epochs one last time to get the */
/*     directory epochs. */


/*     First pass: which records are to be moved? */

    first = 0;
    last = 0;
    i__1 = nrec;
    for (i__ = 1; i__ <= i__1; ++i__) {
	i__2 = offe + i__;
	i__3 = offe + i__;
	dafgda_(handle, &i__2, &i__3, data);
	if (first == 0 && data[0] >= *begin) {
	    first = i__;
	}
	if (first != 0 && last == 0 && data[0] >= *end) {
	    last = i__;
	}
    }

/*     Second pass. Move the records. */

    offset = *baddr - 1 + (first - 1) * 71;
    i__1 = last;
    for (i__ = first; i__ <= i__1; ++i__) {
	i__2 = offset + 1;
	i__3 = offset + 71;
	dafgda_(handle, &i__2, &i__3, data);
	dafada_(data, &c__71);
	offset += 71;
    }

/*     Third pass. Move the epochs. */

    i__1 = last;
    for (i__ = first; i__ <= i__1; ++i__) {
	i__2 = offe + i__;
	i__3 = offe + i__;
	dafgda_(handle, &i__2, &i__3, data);
	dafada_(data, &c__1);
    }

/*     Get every 100'th epoch for the directory. */

    i__1 = last;
    for (i__ = first + 99; i__ <= i__1; i__ += 100) {
	i__2 = offe + i__;
	i__3 = offe + i__;
	dafgda_(handle, &i__2, &i__3, data);
	dafada_(data, &c__1);
    }

/*     Add the number of records, and we're done. */

    data[0] = (doublereal) (last - first + 1);
    dafada_(data, &c__1);
    chkout_("SPKS01", (ftnlen)6);
    return 0;
} /* spks01_ */

