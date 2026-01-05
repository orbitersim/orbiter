/* dafrrr.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;
static integer c__128 = 128;

/* $Procedure DAFRRR ( DAF, remove reserved records ) */
/* Subroutine */ int dafrrr_(integer *handle, integer *resv)
{
    /* System generated locals */
    integer i__1, i__2;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer);

    /* Local variables */
    char crec[1000];
    doublereal drec[128];
    integer decr, free, word, next;
    extern /* Subroutine */ int dafgs_(doublereal *), chkin_(char *, ftnlen), 
	    dafps_(integer *, integer *, doublereal *, integer *, doublereal *
	    );
    integer bward;
    extern /* Subroutine */ int dafus_(doublereal *, integer *, integer *, 
	    doublereal *, integer *);
    integer fward;
    extern /* Subroutine */ int dafws_(doublereal *);
    integer recno;
    logical found;
    doublereal dc[125];
    integer ic[250];
    extern /* Subroutine */ int daffna_(logical *);
    integer nd;
    extern logical failed_(void);
    extern /* Subroutine */ int dafbfs_(integer *);
    integer begblk, ni;
    extern /* Subroutine */ int dafsih_(integer *, char *, ftnlen);
    char ifname[60];
    integer endblk;
    extern /* Subroutine */ int dafrcr_(integer *, integer *, char *, ftnlen),
	     dafrdr_(integer *, integer *, integer *, integer *, doublereal *,
	     logical *), dafrfr_(integer *, integer *, integer *, char *, 
	    integer *, integer *, integer *, ftnlen), dafarw_(integer *, 
	    integer *, integer *), dafwcr_(integer *, integer *, char *, 
	    ftnlen), dafwdr_(integer *, integer *, doublereal *), dafwfr_(
	    integer *, integer *, integer *, char *, integer *, integer *, 
	    integer *, ftnlen);
    integer remove;
    extern /* Subroutine */ int chkout_(char *, ftnlen);
    extern logical return_(void);
    doublereal sum[125];

/* $ Abstract */

/*     Remove a specified number of reserved records from a Double */
/*     Precision Array File (DAF). */

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
/*     HANDLE     I   DAF, opened for writing. */
/*     RESV       I   Number of records to remove. */

/* $ Detailed_Input */

/*     HANDLE   is the handle associated with a DAF that has been */
/*              opened with write access. */

/*     RESV     is the number of reserved records to be removed */
/*              from the specified file. */

/* $ Detailed_Output */

/*     None. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If RESV is less than one, the file is not changed. */

/*     2)  If RESV is greater than the number of reserved records in the */
/*         file, all of the reserved records are removed. */

/* $ Files */

/*     See argument HANDLE. */

/* $ Particulars */

/*     Normally, the reserved records in an array file are reserved */
/*     when the file is created. However, it may occasionally become */
/*     desirable to remove reserved records---when their contents are */
/*     significantly reduced, for example. */

/*     The records nearest the end of the file are removed. Note */
/*     that the physical size of the file is not reduced when reserved */
/*     records are removed. */

/* $ Examples */

/*     For the following call to DAFRRR, assume that HANDLE is the file */
/*     handle for a DAF file that has been opened for write access, and */
/*     that the DAF file already contains 12 reserved records (located in */
/*     records 2-13 of the physical file). */

/*        CALL DAFRRR ( HANDLE, 7 ) */

/*     After this call to DAFRRR, the number of reserved records has been */
/*     decreased by 7, leaving only the first five of the original */
/*     reserved records, physical records 2-6. */

/* $ Restrictions */

/*     1)  This routine will only remove reserve records from DAFs open */
/*         for write. These files are implicitly of the native binary */
/*         file format. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     K.R. Gehringer     (JPL) */
/*     W.L. Taber         (JPL) */
/*     F.S. Turner        (JPL) */
/*     I.M. Underwood     (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.3.0, 02-JUN-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 1.2.0, 16-NOV-2001 (FST) */

/*        Added a call to DAFSIH to prevent this routine from */
/*        attempting to write to non-native binary file formats. */
/*        This will provide a more useful error diagnostic with */
/*        little impact on performance. */

/* -    SPICELIB Version 1.1.0, 30-SEP-1993 (KRG) */

/*        $Detailed_Input and $Examples section of the header were */
/*        modified. */

/*        Added calls to the FORTRAN intrinsic functions INT and */
/*        DBLE in the code that updates the summary record. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 18-JUL-1990 (IMU) */

/* -& */
/* $ Index_Entries */

/*     remove DAF reserved records */

/* -& */
/* $ Revisions */

/* -    SPICELIB Version 1.1.0, 30-SEP-1993 (KRG) */

/*        $Detailed_Input section was modified. References to any */
/*        specific routines by name as a method for opening a DAF file */
/*        for write access were removed. The assumption is that a person */
/*        using DAF files would already know something about opening and */
/*        closing the files. */

/*        $Examples section was modified. References to any specific */
/*        routines by name as a method for opening a DAF file for writing */
/*        were removed, and the example was reworded in such a way that */
/*        the use of the subroutine remained clear. */

/*        Added calls to the INT intrinsic function to convert a DP */
/*        number to an integer before assigning it to NEXT or ENDBLK, */
/*        both of which are integer variables. Also added calls to INT */
/*        in IF statements where comparisons were made between DP numbers */
/*        and INTEGERs, when integral values were actually being */
/*        compared. */

/*        Added calls to the intrinsic function DBLE to convert an */
/*        integer, REMOVE, into a DP number when doing some arithmetic. */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */

/*     IFNLEN      is the length of a DAF internal file name. */


/*     WPR         is the maximum number of double precision */
/*                 numbers per record.  WPR stands for words */
/*                 per record. */


/*     MAXD,       are the maximum number of double precision */
/*     MAXI,       numbers, integers, and characters, respectively, */
/*     MAXC        not including space reserved for control information. */


/*     Local variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("DAFRRR", (ftnlen)6);
    }

/*     Before proceeding any further, check that the DAF associated */
/*     with HANDLE is available for write access. */

    dafsih_(handle, "WRITE", (ftnlen)5);
    if (failed_()) {
	chkout_("DAFRRR", (ftnlen)6);
	return 0;
    }

/*     Get the contents of the file record. If it fails, then just check */
/*     out and return, as an appropriate error message should have */
/*     already been set. */

    dafrfr_(handle, &nd, &ni, ifname, &fward, &bward, &free, (ftnlen)60);
    if (failed_()) {
	chkout_("DAFRRR", (ftnlen)6);
	return 0;
    }

/*     Don't remove more than the current number of reserved records! */
/*     If there are none, check out. */

/* Computing MIN */
    i__1 = *resv, i__2 = fward - 2;
    remove = min(i__1,i__2);
    if (remove < 1) {
	chkout_("DAFRRR", (ftnlen)6);
	return 0;
    }

/*     Okay, here's the plan. We are just going to move records */
/*     forward, starting with the first summary record in the file */
/*     and ending with the last data record. */

/*     After everything has been moved, the initial and final */
/*     addresses of all the arrays have to be decremented by the */
/*     same amount: the number of words per record (128) times */
/*     the number of records removed. */

    decr = remove << 7;

/*     Records will be moved in `blocks', where each block contains */

/*        -- a summary record */

/*        -- a name record */

/*        -- one or more data records */

/*     Most blocks lie between one summary record and the next. */
/*     The final block lies between the final summary record and */
/*     whatever data record contains the first free address. */

/*     BEGBLK is initially the first summary record location. */

    begblk = fward;
    while(begblk > 0 && ! failed_()) {

/*        Move the summary record first. The location of the next */
/*        summary record determines the end of this block, and the */
/*        beginning of the next. */

/*        Be sure to adjust the forward and backward pointers; */
/*        otherwise, we won't be able to find the summaries again. */

	recno = begblk;
	dafrdr_(handle, &recno, &c__1, &c__128, drec, &found);
	if ((integer) drec[0] > 0) {
	    endblk = (integer) drec[0] - 1;
	    next = (integer) drec[0];
	} else {
	    dafarw_(&free, &endblk, &word);
	    next = 0;
	}
	if ((integer) drec[0] > 0) {
	    drec[0] -= (doublereal) remove;
	}
	if ((integer) drec[1] > 0) {
	    drec[1] -= (doublereal) remove;
	}
	i__1 = recno - remove;
	dafwdr_(handle, &i__1, drec);

/*        Then the name record. */

	recno = begblk + 1;
	dafrcr_(handle, &recno, crec, (ftnlen)1000);
	i__1 = recno - remove;
	dafwcr_(handle, &i__1, crec, (ftnlen)1000);

/*        Finally, the data records. */

	i__1 = endblk;
	for (recno = begblk + 2; recno <= i__1; ++recno) {
	    dafrdr_(handle, &recno, &c__1, &c__128, drec, &found);
	    i__2 = recno - remove;
	    dafwdr_(handle, &i__2, drec);
	}

/*        Start the next block, if one exists. */

	begblk = next;
    }

/*     Rewrite the file record, to reflect the new organization of */
/*     the file. */

    fward -= remove;
    bward -= remove;
    free -= decr;
    dafwfr_(handle, &nd, &ni, ifname, &fward, &bward, &free, (ftnlen)60);

/*     Get the summary for each array, decrement the addresses (stored */
/*     in the final two integer components), and replace the summary. */

    dafbfs_(handle);
    daffna_(&found);
    while(found && ! failed_()) {
	dafgs_(sum);
	dafus_(sum, &nd, &ni, dc, ic);
	ic[(i__1 = ni - 2) < 250 && 0 <= i__1 ? i__1 : s_rnge("ic", i__1, 
		"dafrrr_", (ftnlen)407)] = ic[(i__2 = ni - 2) < 250 && 0 <= 
		i__2 ? i__2 : s_rnge("ic", i__2, "dafrrr_", (ftnlen)407)] - 
		decr;
	ic[(i__1 = ni - 1) < 250 && 0 <= i__1 ? i__1 : s_rnge("ic", i__1, 
		"dafrrr_", (ftnlen)408)] = ic[(i__2 = ni - 1) < 250 && 0 <= 
		i__2 ? i__2 : s_rnge("ic", i__2, "dafrrr_", (ftnlen)408)] - 
		decr;
	dafps_(&nd, &ni, dc, ic, sum);
	dafws_(sum);
	daffna_(&found);
    }
    chkout_("DAFRRR", (ftnlen)6);
    return 0;
} /* dafrrr_ */

