/* dafrcr.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static logical c_false = FALSE_;
static integer c__1 = 1;

/* $Procedure DAFRCR ( DAF, read character record ) */
/* Subroutine */ int dafrcr_(integer *handle, integer *recno, char *crec, 
	ftnlen crec_len)
{
    /* System generated locals */
    integer i__1;

    /* Builtin functions */
    integer i_len(char *, ftnlen), s_rdue(cilist *), do_uio(integer *, char *,
	     ftnlen), e_rdue(void);

    /* Local variables */
    integer unit;
    extern /* Subroutine */ int zzddhhlu_(integer *, char *, logical *, 
	    integer *, ftnlen), chkin_(char *, ftnlen);
    extern logical failed_(void);
    extern /* Subroutine */ int dafsih_(integer *, char *, ftnlen), sigerr_(
	    char *, ftnlen), chkout_(char *, ftnlen), setmsg_(char *, ftnlen);
    integer iostat;
    extern /* Subroutine */ int errint_(char *, integer *, ftnlen);
    extern logical return_(void);

    /* Fortran I/O blocks */
    static cilist io___3 = { 1, 0, 1, 0, 0 };


/* $ Abstract */

/*     Read the contents of a character record from a DAF. */

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
/*     HANDLE     I   Handle of DAF. */
/*     RECNO      I   Record number of character record. */
/*     CREC       O   Character record. */

/* $ Detailed_Input */

/*     HANDLE   is the handle associated with a DAF. */

/*     RECNO    is the record number of a character record within */
/*              the file. */

/* $ Detailed_Output */

/*     CREC     contains the first 1000 characters of the specified */
/*              record from the specified file. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the declared length of CREC is not 1000 characters, */
/*         the error SPICE(DAFBADRECLEN) is signaled. */

/*     2)  If the specified record cannot (for some reason) be read, */
/*         the error SPICE(DAFCRNOTFOUND) is signaled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     Unlike double precision records, character records are */
/*     not buffered. Also, while failing to find a specific double */
/*     precision record is indicated through the calling sequence, */
/*     failing to find a character record results in an error. */

/* $ Examples */

/*     In the following example, matching summary and name records are */
/*     read from a DAF: */

/*        CALL DAFGDR ( HANDLE, NEXT,   DREC, FOUND ) */
/*        CALL DAFRCR ( HANDLE, NEXT+1, CREC        ) */

/*     Note that a character record always immediately follows a summary */
/*     record. */

/* $ Restrictions */

/*     1)  This routine is only used to read records on environments */
/*         whose characters are a single byte in size. Updates */
/*         to this routine and routines in its call tree may be */
/*         required to properly handle other cases. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     H.A. Neilan        (JPL) */
/*     W.L. Taber         (JPL) */
/*     F.S. Turner        (JPL) */
/*     I.M. Underwood     (JPL) */

/* $ Version */

/* -    SPICELIB Version 2.1.0, 17-JUN-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. Moved DAF */
/*        required reading from $Literature_References to */
/*        $Required_Reading section. */

/* -    SPICELIB Version 2.0.0, 16-NOV-2001 (FST) */

/*        Updated this routine to make proper use of the new */
/*        handle manager functionality installed underneath */
/*        DAF. */

/* -    SPICELIB Version 1.0.2, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.1, 22-MAR-1990 (HAN) */

/*        Literature references added to the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU) */

/* -& */
/* $ Index_Entries */

/*     read DAF character record */

/* -& */
/* $ Revisions */

/* -    SPICELIB Version 2.0.0, 16-NOV-2001 (FST) */

/*        This routine now makes use of the handle manager */
/*        code. A call to DAFSIH was inserted just after */
/*        the standard SPICE error handling code at the */
/*        head of the module. This was done to insure that */
/*        the caller is referring to a legitimately loaded */
/*        DAF. The penalty for performing this check is */
/*        a binary search on the number of loaded files, */
/*        which should be small compared to the actual READ */
/*        performed below. */

/*        The call to DAFHLU has been replaced with ZZDDHHLU, */
/*        since calls to DAFHLU locks handles to their logical */
/*        units. */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("DAFRCR", (ftnlen)6);
    }

/*     Check to be sure that HANDLE is attached to a file that is open */
/*     with read access.  If the call fails, check out and return. */

    dafsih_(handle, "READ", (ftnlen)4);
    if (failed_()) {
	chkout_("DAFRCR", (ftnlen)6);
	return 0;
    }

/*     Now make certain that the string to receive the contents of */
/*     the character record is the appropriate length. */

    if (i_len(crec, crec_len) != 1000) {
	setmsg_("Expected length of character record is 1000. Passed string "
		"has length #", (ftnlen)71);
	i__1 = i_len(crec, crec_len);
	errint_("#", &i__1, (ftnlen)1);
	sigerr_("SPICE(DAFBADCRECLEN)", (ftnlen)20);
    } else {

/*        Retrieve a logical unit for this handle.  This has the */
/*        side-effect of locking this UNIT to HANDLE. */

	zzddhhlu_(handle, "DAF", &c_false, &unit, (ftnlen)3);
	if (failed_()) {
	    chkout_("DAFRCR", (ftnlen)6);
	    return 0;
	}
	io___3.ciunit = unit;
	io___3.cirec = *recno;
	iostat = s_rdue(&io___3);
	if (iostat != 0) {
	    goto L100001;
	}
	iostat = do_uio(&c__1, crec, crec_len);
	if (iostat != 0) {
	    goto L100001;
	}
	iostat = e_rdue();
L100001:
	if (iostat != 0) {
	    setmsg_("Could not read record #. IOSTAT was #.", (ftnlen)38);
	    errint_("#", recno, (ftnlen)1);
	    errint_("#", &iostat, (ftnlen)1);
	    sigerr_("SPICE(DAFCRNOTFOUND)", (ftnlen)20);
	}
    }
    chkout_("DAFRCR", (ftnlen)6);
    return 0;
} /* dafrcr_ */

