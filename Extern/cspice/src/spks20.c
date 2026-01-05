/* spks20.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__7 = 7;

/* $Procedure SPKS20 ( S/P Kernel, subset, type 20 ) */
/* Subroutine */ int spks20_(integer *handle, integer *baddr, integer *eaddr, 
	doublereal *begin, doublereal *end)
{
    /* System generated locals */
    integer i__1, i__2;

    /* Builtin functions */
    double d_int(doublereal *);

    /* Local variables */
    doublereal data[100];
    integer addr__, nrec, last, move;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    doublereal btime;
    integer first;
    extern /* Subroutine */ int dafada_(doublereal *, integer *), dafgda_(
	    integer *, integer *, integer *, doublereal *);
    doublereal dscale, subbeg, tscale;
    integer remain;
    doublereal subijd, initjd, intlen, subifr, initfr;
    extern /* Subroutine */ int chkout_(char *, ftnlen);
    integer recsiz;
    doublereal intrvl;
    extern logical return_(void);
    extern doublereal j2000_(void), spd_(void);

/* $ Abstract */

/*     Extract a subset of the data in a SPK segment of type 20 */
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

/*     The exact structure of a segment of data type 20 is detailed in */
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
/*     R.E. Thurman       (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.1, 14-APR-2021 (JDR) */

/*        Edited the header to comply with NAIF standard. Moved SPK */
/*        required reading from $Literature_References to */
/*        $Required_Reading section. */

/* -    SPICELIB Version 1.0.0, 23-DEC-2013 (NJB) (RET) */

/* -& */
/* $ Index_Entries */

/*     subset type_20 SPK segment */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     Local variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }
    chkin_("SPKS20", (ftnlen)6);

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

    i__1 = *eaddr - 6;
    dafgda_(handle, &i__1, eaddr, data);
    dscale = data[0];
    tscale = data[1];
    initjd = data[2];
    initfr = data[3];
    intlen = data[4];
    recsiz = (integer) data[5];
    nrec = (integer) data[6];
    btime = (initjd - j2000_() + initfr) * spd_();
    intrvl = intlen * spd_();
    first = (integer) ((*begin - btime) / intrvl) + 1;
/* Computing MAX */
    i__1 = 1, i__2 = min(first,nrec);
    first = max(i__1,i__2);
    last = (integer) ((*end - btime) / intrvl) + 1;
/* Computing MAX */
    i__1 = 1, i__2 = min(last,nrec);
    last = max(i__1,i__2);

/*     The number of records to be moved. */

    nrec = last - first + 1;

/*     We're going to move the data in chunks of BUFSIZ d.p. words. */
/*     Compute the number of words left to move, the address of the */
/*     beginning of the records to move, and the number to move this */
/*     time. */

    remain = nrec * recsiz;
    addr__ = *baddr + (first - 1) * recsiz;
    move = min(100,remain);
    while(remain > 0) {
	i__1 = addr__ + move - 1;
	dafgda_(handle, &addr__, &i__1, data);
	dafada_(data, &move);
	remain -= move;
	addr__ += move;
	move = min(100,remain);
    }

/*     That's all the records we have to move. But there are still seven */
/*     final numbers left to write: */

/*        1)  The distance scale (DSCALE). */
/*        2)  The time scale (TSCALE). */
/*        3)  The initial integer Julian date of the start time of the */
/*            first record. */
/*        4)  The fractional part of the state time of the first */
/*            record. */
/*        5)  The time interval length for each polynomial in days */
/*            (INTLEN). */
/*        6)  The record size (RECSIZ). */
/*        7)  The number of records (NREC). */



/*     Let SUBBEG be the subset begin time expressed as a TDB Julian */
/*     date. */

    subbeg = j2000_() + (btime + (first - 1) * intrvl) / spd_();
    subijd = d_int(&subbeg);
    subifr = subbeg - subijd;
    data[0] = dscale;
    data[1] = tscale;
    data[2] = subijd;
    data[3] = subifr;
    data[4] = intlen;
    data[5] = (doublereal) recsiz;
    data[6] = (doublereal) nrec;
    dafada_(data, &c__7);
    chkout_("SPKS20", (ftnlen)6);
    return 0;
} /* spks20_ */

