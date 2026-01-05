/* dafarr.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;
static integer c__128 = 128;

/* $Procedure DAFARR ( DAF, add reserved records ) */
/* Subroutine */ int dafarr_(integer *handle, integer *resv)
{
    /* System generated locals */
    integer i__1, i__2;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer);

    /* Local variables */
    char crec[1000];
    doublereal drec[128];
    integer free, incr, word, next, i__;
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
    extern /* Subroutine */ int dafrdr_(integer *, integer *, integer *, 
	    integer *, doublereal *, logical *), dafrcr_(integer *, integer *,
	     char *, ftnlen), dafrfr_(integer *, integer *, integer *, char *,
	     integer *, integer *, integer *, ftnlen), dafarw_(integer *, 
	    integer *, integer *), dafwcr_(integer *, integer *, char *, 
	    ftnlen), dafwdr_(integer *, integer *, doublereal *), dafwfr_(
	    integer *, integer *, integer *, char *, integer *, integer *, 
	    integer *, ftnlen), chkout_(char *, ftnlen);
    extern logical return_(void);
    doublereal sum[125];

/* $ Abstract */

/*     Add a specified number of reserved records to a Double Precision */
/*     Array File (DAF). */

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
/*     HANDLE     I   Handle of a DAF file opened for writing. */
/*     RESV       I   Number of records to reserve. */

/* $ Detailed_Input */

/*     HANDLE   is the handle associated with a DAF file that has */
/*              been opened with write access. */

/*     RESV     is the number of reserved records to be added */
/*              to the specified file. */

/* $ Detailed_Output */

/*     None. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If RESV is less than one, the file is not changed. */

/* $ Files */

/*     See argument HANDLE. */

/* $ Particulars */

/*     Normally, the reserved records in an array file are reserved */
/*     when the file is created. However, it may occasionally become */
/*     necessary to add reserved records---when the contents of one */
/*     file are appended to another, for example. (In this case, any */
/*     information in the reserved records of either file should */
/*     be included in the resulting file.) */

/*     The new reserved records are appended to the old ones. The new */
/*     reserved records are also NULL filled. */

/* $ Examples */

/*     In the following call to DAFARR, assume that HANDLE is the file */
/*     handle for a DAF file that has been opened for write access, and */
/*     that the DAF file already contains 12 reserved records (located in */
/*     records 2-13 of the physical file). */

/*        CALL DAFARR ( HANDLE, 7 ) */

/*     After this call, the DAF file attached to HANDLE will contain 19 */
/*     reserved records. The new reserved records are located in */
/*     records 14-20 of the physical file. */

/* $ Restrictions */

/*     1)  This routine will only add reserved records to DAFs open for */
/*         write. These files are implicitly of the native binary file */
/*         format. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     K.R. Gehringer     (JPL) */
/*     W.L. Taber         (JPL) */
/*     F.S. Turner        (JPL) */
/*     I.M. Underwood     (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.6.0, 13-AUG-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 1.5.0, 16-NOV-2001 (FST) */

/*        Added a call to DAFSIH to prevent this routine from */
/*        attempting to write to non-native binary file formats. */
/*        This will provide a more useful error diagnostic with */
/*        little impact on performance. */

/* -    SPICELIB Version 1.4.0, 08-MAR-1996 (KRG) */

/*        Added code to write NULL filled records to the file for the */
/*        new reserved records. */

/* -    SPICELIB Version 1.3.0, 12-MAY-1994 (KRG) */

/*        Added a missing call to CHKOUT before the RETURN statement in */
/*        the test */

/*              IF ( RESV .LT. 1 ) THEN */
/*                 RETURN */
/*              END IF */

/* -    SPICELIB Version 1.2.0, 30-SEP-1993 (KRG) */

/*        $Detailed_Input and $Examples section of the header were */
/*        modified. */

/*        Added calls to the FORTRAN intrinsic functions INT and */
/*        DBLE in the code that updates the summary record. */

/*        Modified an IF loop to make logic clearer. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 17-JUL-1990 (IMU) */

/* -& */
/* $ Index_Entries */

/*     add DAF reserved records */

/* -& */
/* $ Revisions */

/* -    SPICELIB Version 1.4.0, 08-MAR-1996 (KRG) */

/*        Added code to write NULL filled records to the file for the */
/*        new reserved records. */

/* -    SPICELIB Version 1.3.0, 12-MAY-1994 (KRG) */

/*        Added a missing call to CHKOUT before the RETURN statement in */
/*        the test */

/*              IF ( RESV .LT. 1 ) THEN */
/*                 RETURN */
/*              END IF */

/* -    SPICELIB Version 1.2.0, 30-SEP-1993 (KRG) */

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
/*        number to an integer before assigning it to NEXT, which is an */
/*        integer variable. Also added calls to INT in IF statements */
/*        where comparisons were made between DP numbers and INTEGERs, */
/*        when integral values were actually being compared. */

/*        Added calls to the intrinsic function DBLE to convert an */
/*        integer, RESV, into a DP number when doing some arithmetic. */

/*        Took an ELSE IF clause out of the initial IF return  ELSE */
/*        check in END IF at the beginning of the routine. Replaced the */
/*        code: */

/*              IF ( RETURN () ) THEN */
/*                 RETURN */

/*              ELSE IF ( RESV .LT. 1 ) THEN */
/*                 RETURN */

/*              ELSE */
/*                 CALL CHKIN ( 'DAFARR' ) */
/*              END IF */

/*        with the equivalent code: */

/*              IF ( RETURN () ) THEN */
/*                 RETURN */
/*              ELSE */
/*                 CALL CHKIN ( 'DAFARR' ) */
/*              END IF */

/*        C */
/*        C     Check to see if the number of records to be reserved is */
/*        C     less than one. If so, just return without changing */
/*        C     anything. */
/*        C */
/*              IF ( RESV .LT. 1 ) THEN */
/*                 RETURN */
/*              END IF */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 17-JUL-1990 (IMU) */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */

/*     IFNLEN      is the length of a DAF internal file name. */


/*     WPR         is the maximum number of double precision numbers */
/*                 (words) per record. */

/*     MAXD,       are the maximum number of double precision */
/*     MAXI,       numbers, integers, and characters, respectively, */
/*     MAXC        per record, not including space reserved for */
/*                 control information (3 dp numbers are reserved). */
/*                 There are two integers per double precision word, */
/*                 and eight characters per word. */


/*     Local variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("DAFARR", (ftnlen)6);
    }


/*     Check to see if the number of records to be reserved is less than */
/*     one. If so, just return without changing anything. */

    if (*resv < 1) {
	chkout_("DAFARR", (ftnlen)6);
	return 0;
    }

/*     Before proceeding any further, check that the DAF associated */
/*     with HANDLE is available for write access. */

    dafsih_(handle, "WRITE", (ftnlen)5);
    if (failed_()) {
	chkout_("DAFARR", (ftnlen)6);
	return 0;
    }

/*     Get the contents of the file record. If it fails, then just check */
/*     out and return, as an appropriate error message should have */
/*     already been set. */

    dafrfr_(handle, &nd, &ni, ifname, &fward, &bward, &free, (ftnlen)60);
    if (failed_()) {
	chkout_("DAFARR", (ftnlen)6);
	return 0;
    }

/*     Okay, here's the plan. We are just going to move records */
/*     in the direction of the end of the file, starting */
/*     with the last record in the file and ending with the first */
/*     summary record. */

/*     After everything has been moved, the initial and final */
/*     addresses of all the arrays have to be incremented by the */
/*     same amount: the number of words per record (128) times */
/*     the number of new records. */

    incr = *resv << 7;

/*     Before we do that, however, we should write some bogus records */
/*     to the end of the file, to make sure we don't run out of space */
/*     later on. If this doesn't work, we will leave the logical */
/*     contents of the file uncorrupted (although it may get larger). */

    dafarw_(&free, &recno, &word);
    i__1 = *resv;
    for (i__ = 1; i__ <= i__1; ++i__) {
	i__2 = recno + i__;
	dafwdr_(handle, &i__2, drec);
    }
    if (failed_()) {
	chkout_("DAFARR", (ftnlen)6);
	return 0;
    }

/*     Records will be moved in `blocks', where each block contains */

/*        -- a summary record */

/*        -- a name record */

/*        -- one or more data records */

/*     The first block to be moved (that is, the last block in */
/*     the file) lies between the final summary record (BWARD) and */
/*     whatever record contains the first free address in the file. */

    begblk = bward;
    dafarw_(&free, &endblk, &word);
    while(begblk > 0 && ! failed_()) {

/*        Move the data records first. */

	i__1 = begblk + 2;
	for (recno = endblk; recno >= i__1; --recno) {
	    dafrdr_(handle, &recno, &c__1, &c__128, drec, &found);
	    i__2 = recno + *resv;
	    dafwdr_(handle, &i__2, drec);
	}

/*        Then the name record. */

	recno = begblk + 1;
	dafrcr_(handle, &recno, crec, (ftnlen)1000);
	i__1 = recno + *resv;
	dafwcr_(handle, &i__1, crec, (ftnlen)1000);

/*        Finally, the summary record. */

/*        To find the beginning of the next block, look at the backward */
/*        pointer from the summary record of the current block. */

/*        Be sure to adjust the forward and backward pointers; */
/*        otherwise, we won't be able to find the summaries again. */

	recno = begblk;
	dafrdr_(handle, &recno, &c__1, &c__128, drec, &found);
	next = (integer) drec[1];
	if ((integer) drec[0] > 0) {
	    drec[0] += (doublereal) (*resv);
	}
	if ((integer) drec[1] > 0) {
	    drec[1] += (doublereal) (*resv);
	}
	i__1 = recno + *resv;
	dafwdr_(handle, &i__1, drec);

/*        The next block ends just before the current block begins. */

	endblk = begblk - 1;
	begblk = next;
    }

/*     Rewrite the file record, to reflect the new organization of */
/*     the file. */

    fward += *resv;
    bward += *resv;
    free += incr;
    dafwfr_(handle, &nd, &ni, ifname, &fward, &bward, &free, (ftnlen)60);

/*     Get the summary for each array, increment the addresses (stored */
/*     in the final two integer components), and replace the summary. */

    dafbfs_(handle);
    daffna_(&found);
    while(found && ! failed_()) {
	dafgs_(sum);
	dafus_(sum, &nd, &ni, dc, ic);
	ic[(i__1 = ni - 2) < 250 && 0 <= i__1 ? i__1 : s_rnge("ic", i__1, 
		"dafarr_", (ftnlen)488)] = ic[(i__2 = ni - 2) < 250 && 0 <= 
		i__2 ? i__2 : s_rnge("ic", i__2, "dafarr_", (ftnlen)488)] + 
		incr;
	ic[(i__1 = ni - 1) < 250 && 0 <= i__1 ? i__1 : s_rnge("ic", i__1, 
		"dafarr_", (ftnlen)489)] = ic[(i__2 = ni - 1) < 250 && 0 <= 
		i__2 ? i__2 : s_rnge("ic", i__2, "dafarr_", (ftnlen)489)] + 
		incr;
	dafps_(&nd, &ni, dc, ic, sum);
	dafws_(sum);
	daffna_(&found);
    }

/*     Write NULL filled records to the reserved record area. */

    for (i__ = 1; i__ <= 1000; ++i__) {
	*(unsigned char *)&crec[i__ - 1] = '\0';
    }
    i__ = fward - *resv;
    i__1 = i__ + *resv - 1;
    for (recno = i__; recno <= i__1; ++recno) {
	dafwcr_(handle, &recno, crec, (ftnlen)1000);
    }
    chkout_("DAFARR", (ftnlen)6);
    return 0;
} /* dafarr_ */

