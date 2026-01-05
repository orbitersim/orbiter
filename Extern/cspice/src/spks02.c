/* spks02.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__4 = 4;

/* $Procedure SPKS02 ( S/P Kernel, subset, type 2 ) */
/* Subroutine */ int spks02_(integer *handle, integer *baddr, integer *eaddr, 
	doublereal *begin, doublereal *end)
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    doublereal data[50];
    integer addr__, nrec;
    doublereal init;
    integer last, move;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    integer first;
    extern /* Subroutine */ int dafada_(doublereal *, integer *), dafgda_(
	    integer *, integer *, integer *, doublereal *);
    integer remain;
    doublereal intlen;
    extern /* Subroutine */ int chkout_(char *, ftnlen);
    integer recsiz;
    extern logical return_(void);

/* $ Abstract */

/*     Extract a subset of the data in a SPK segment of type 2 */
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
/*              of the subset to be extracted. */

/* $ Detailed_Output */

/*     None. This routine writes data to the SPK file currently */
/*     open for write access. */

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

/*     The exact structure of a segment of data type 2 is detailed in */
/*     the SPK Required Reading file. */

/* $ Examples */

/*     None. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     H.A. Neilan        (JPL) */
/*     W.L. Taber         (JPL) */
/*     R.E. Thurman       (JPL) */
/*     E.D. Wright        (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.2, 14-APR-2021 (JDR) */

/*        Edited the header to comply with NAIF standard. Moved SPK */
/*        required reading from $Literature_References to */
/*        $Required_Reading section. */

/* -    SPICELIB Version 1.1.1, 30-DEC-2013 (NJB) */

/*        Enhanced header documentation. */

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

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (RET) */

/* -& */
/* $ Index_Entries */

/*     subset type_2 SPK segment */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("SPKS02", (ftnlen)6);
    }

/*     The segment is made up of a number of logical records, each */
/*     having the same size, and covering the same length of time. */

/*     We can determine which records to extract by comparing the input */
/*     epochs with the initial time of the segment and the length of the */
/*     interval covered by each record.  These final two constants are */
/*     located at the end of the segment, along with the size of each */
/*     logical record and the total number of records. */

    i__1 = *eaddr - 3;
    dafgda_(handle, &i__1, eaddr, data);
    init = data[0];
    intlen = data[1];
    recsiz = (integer) data[2];
    nrec = (integer) data[3];
    first = (integer) ((*begin - init) / intlen) + 1;
    first = min(first,nrec);
    last = (integer) ((*end - init) / intlen) + 1;
    last = min(last,nrec);

/*     The number of records to be moved. */

    nrec = last - first + 1;

/*     We're going to move the data in chunks of 50 d.p. words.  Compute */
/*     the number of words left to move, the address of the beginning */
/*     of the records to move, and the number to move this time. */

    remain = nrec * recsiz;
    addr__ = *baddr + (first - 1) * recsiz;
    move = min(50,remain);
    while(remain > 0) {
	i__1 = addr__ + move - 1;
	dafgda_(handle, &addr__, &i__1, data);
	dafada_(data, &move);
	remain -= move;
	addr__ += move;
	move = min(50,remain);
    }

/*     That's all the records we have to move. But there are still four */
/*     final numbers left to write: */

/*        1)  The initial time for the polynomials (INIT). */
/*        2)  The time interval length for each polynomial (INTLEN). */
/*        3)  The record size (RECSIZ). */
/*        4)  The number of records (NREC). */

/*     INIT and NREC will probably be different for the new segment (in */
/*     fact, NREC has already been changed), the other two will not. */

    init += (first - 1) * intlen;
    data[0] = init;
    data[1] = intlen;
    data[2] = (doublereal) recsiz;
    data[3] = (doublereal) nrec;
    dafada_(data, &c__4);
    chkout_("SPKS02", (ftnlen)6);
    return 0;
} /* spks02_ */

