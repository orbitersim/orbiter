/* dafwda.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;
static integer c__128 = 128;

/* $Procedure DAFWDA ( DAF, write data to address ) */
/* Subroutine */ int dafwda_(integer *handle, integer *begin, integer *end, 
	doublereal *data)
{
    /* Initialized data */

    static doublereal buffer[128] = { 0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
	    0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
	    0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
	    0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
	    0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
	    0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
	    0.,0.,0.,0.,0. };

    /* System generated locals */
    integer i__1, i__2;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer);

    /* Local variables */
    integer begr, begw, endr, endw, next, n;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    integer recno;
    extern /* Subroutine */ int moved_(doublereal *, integer *, doublereal *);
    logical found;
    integer first;
    extern /* Subroutine */ int cleard_(integer *, doublereal *), dafrdr_(
	    integer *, integer *, integer *, integer *, doublereal *, logical 
	    *), dafarw_(integer *, integer *, integer *), dafwdr_(integer *, 
	    integer *, doublereal *), sigerr_(char *, ftnlen), chkout_(char *,
	     ftnlen), setmsg_(char *, ftnlen), errint_(char *, integer *, 
	    ftnlen);
    extern logical return_(void);

/* $ Abstract */

/*     Write or rewrite the double precision data bounded by two */
/*     addresses within a DAF. */

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

/*     DAF */

/* $ Keywords */

/*     FILES */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     HANDLE     I   Handle of a DAF. */
/*     BEGIN, */
/*     END        I   Initial, final address within file. */
/*     DATA       I   Data to be stored between BEGIN and END. */

/* $ Detailed_Input */

/*     HANDLE   is the handle of a DAF. */

/*     BEGIN, */
/*     END      are the initial and final addresses of a contiguous */
/*              set of double precision numbers within a DAF. */
/*              Presumably, these make up all or part of a */
/*              particular array. */

/*     DATA     are the double precision data to be stored between */
/*              the specified addresses within the specified file. */

/* $ Detailed_Output */

/*     None. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If BEGIN is zero or negative, the error SPICE(DAFNEGADDR) */
/*         is signaled. */

/*     2)  If the BEGIN > END, the error SPICE(DAFBEGGTEND) */
/*         is signaled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     The principal reason that DAFs are so easy to use is that */
/*     the data in each DAF are considered to be one long contiguous */
/*     set of double precision numbers. You can store data anywhere */
/*     within a DAF without knowing (or caring) about the physical */
/*     records in which they are stored. */

/*     Of course, if you are merely adding arrays to a DAF, */
/*     you should not use DAFWDA directly, but should use DAFANA */
/*     (add new array) and its entry points, since these update */
/*     the appropriate bookkeeping records automatically. */

/* $ Examples */

/*     The following code fragment illustrates the use of DAFWDA */
/*     to update an imaginary array. The array begins with a directory */
/*     containing 11 epochs. Each pair of epochs bounds an */
/*     interval, and each interval is covered by a set of eight */
/*     osculating elements. */

/*     By accident, the elements were written with the wrong value for */
/*     the GM of the central body (the last element in each set). Each */
/*     set must be retrieved, updated,and rewritten. */

/*        CALL DAFUS ( SUM, ND, NI, DC, IC ) */
/*        BEGIN = IC(5) */

/*        DO I = 1, 10 */
/*           OFFSET = BEGIN + 11 + (I - 1) * 8 */

/*           CALL DAFRDA ( HANDLE, OFFSET+1, OFFSET+8, ELEMENTS ) */
/*           ELEMENTS(8) = NEW_GM */

/*           CALL DAFWDA ( HANDLE, OFFSET+1, OFFSET+8, ELEMENTS ) */
/*        END DO */

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

/* $ Version */

/* -    SPICELIB Version 1.1.0, 27-OCT-2021 (JDR) (NJB) */

/*        Added IMPLICIT NONE statement. */

/*        Local variable BUFFER is now initialized and saved. */

/*        Edited the header to comply with NAIF standard. Moved DAF */
/*        required reading from $Literature_References to */
/*        $Required_Reading section. */

/* -    SPICELIB Version 1.0.2, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.1, 22-MAR-1990 (HAN) */

/*        Literature references added to the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU) */

/* -& */
/* $ Index_Entries */

/*     write data to DAF address */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     Saved variables */


/*     Initial values */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }
    chkin_("DAFWDA", (ftnlen)6);

/*     Bad addresses? */

    if (*begin <= 0) {
	setmsg_("Negative beginning address: #", (ftnlen)29);
	errint_("#", begin, (ftnlen)1);
	sigerr_("SPICE(DAFNEGADDR)", (ftnlen)17);
	chkout_("DAFWDA", (ftnlen)6);
	return 0;
    } else if (*begin > *end) {
	setmsg_("Beginning address (#) greater than ending address (#)", (
		ftnlen)53);
	errint_("#", begin, (ftnlen)1);
	errint_("#", end, (ftnlen)1);
	sigerr_("SPICE(DAFBEGGTEND)", (ftnlen)18);
	chkout_("DAFWDA", (ftnlen)6);
	return 0;
    }

/*     Convert raw addresses to record/word representations. */

    dafarw_(begin, &begr, &begw);
    dafarw_(end, &endr, &endw);

/*     The first and last records may have to be read, updated, and */
/*     rewritten. Any records in between may be written directly. */

    next = 1;
    i__1 = endr;
    for (recno = begr; recno <= i__1; ++recno) {
	if (recno == begr || recno == endr) {
	    dafrdr_(handle, &recno, &c__1, &c__128, buffer, &found);
	    if (! found) {
		cleard_(&c__128, buffer);
	    }
	}
	if (begr == endr) {
	    first = begw;
	    n = endw - begw + 1;
	} else if (recno == begr) {
	    first = begw;
	    n = 128 - begw + 1;
	} else if (recno == endr) {
	    first = 1;
	    n = endw;
	} else {
	    first = 1;
	    n = 128;
	}
	moved_(&data[next - 1], &n, &buffer[(i__2 = first - 1) < 128 && 0 <= 
		i__2 ? i__2 : s_rnge("buffer", i__2, "dafwda_", (ftnlen)281)])
		;
	next += n;
	dafwdr_(handle, &recno, buffer);
    }
    chkout_("DAFWDA", (ftnlen)6);
    return 0;
} /* dafwda_ */

