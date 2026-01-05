/* dafwcr.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static logical c_false = FALSE_;
static integer c__1 = 1;

/* $Procedure DAFWCR ( DAF, write character record ) */
/* Subroutine */ int dafwcr_(integer *handle, integer *recno, char *crec, 
	ftnlen crec_len)
{
    /* System generated locals */
    integer i__1;

    /* Builtin functions */
    integer i_len(char *, ftnlen), s_wdue(cilist *), do_uio(integer *, char *,
	     ftnlen), e_wdue(void);

    /* Local variables */
    integer unit;
    extern /* Subroutine */ int zzddhhlu_(integer *, char *, logical *, 
	    integer *, ftnlen), chkin_(char *, ftnlen), dafsih_(integer *, 
	    char *, ftnlen), sigerr_(char *, ftnlen), chkout_(char *, ftnlen),
	     setmsg_(char *, ftnlen);
    integer iostat;
    extern /* Subroutine */ int errint_(char *, integer *, ftnlen);
    extern logical return_(void);

    /* Fortran I/O blocks */
    static cilist io___3 = { 1, 0, 0, 0, 0 };


/* $ Abstract */

/*     Write or rewrite the contents of a character record to */
/*     a DAF. */

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
/*     CREC       I   Character record. */

/* $ Detailed_Input */

/*     HANDLE   is the handle associated with a DAF. */

/*     RECNO    is the record number of a character record within */
/*              the file. If the record does not already exist, it */
/*              is created. Otherwise its contents are overwritten. */

/*     CREC     contains the first 1000 characters of the specified */
/*              record. */

/* $ Detailed_Output */

/*     None. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the specified file is not open for write access, an error */
/*         is signaled by a routine in the call tree of this routine. */

/*     2)  If the declared length of CREC is not 1000 characters, */
/*         the error SPICE(DAFBADRECLEN) is signaled. */

/*     3)  If the specified record cannot (for some reason) be written, */
/*         the error SPICE(DAFWRITEFAIL) is signaled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     Unlike double precision records, character records are */
/*     not buffered. */

/* $ Examples */

/*     In the following example, matching summary and name records are */
/*     written to a DAF: */

/*        CALL DAFWDR ( HANDLE, NEXT,   DREC ) */
/*        CALL DAFWCR ( HANDLE, NEXT+1, CREC ) */

/*     Note that a character record always immediately follows a summary */
/*     record. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     H.A. Neilan        (JPL) */
/*     W.L. Taber         (JPL) */
/*     F.S. Turner        (JPL) */
/*     I.M. Underwood     (JPL) */

/* $ Version */

/* -    SPICELIB Version 2.1.0, 14-APR-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. Moved DAF */
/*        required reading from $Literature_References to */
/*        $Required_Reading section. */

/* -    SPICELIB Version 2.0.0, 27-NOV-2001 (FST) */

/*        Updated this routine to utilize new handle manager */
/*        interfaces. Replaced the check of the input handle's */
/*        sign with the appropriate call to DAFSIH. */

/* -    SPICELIB Version 1.0.2, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.1, 22-MAR-1990 (HAN) */

/*        Literature references added to the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU) */

/* -& */
/* $ Index_Entries */

/*     write DAF character record */

/* -& */
/* $ Revisions */

/* -    SPICELIB Version 2.0.0, 27-NOV-2001 (FST) */

/*        The call to DAFHLU has been replaced with a call to */
/*        ZZDDHHLU, the handle manager interface for retrieving */
/*        a logical unit. DAFHLU is no longer used, since it */
/*        locks the unit returned to its HANDLE, tying up resources */
/*        in the handle manager. A call to DAFSIH was inserted to */
/*        make certain that HANDLE is present in DAFAH's file table, */
/*        rather than simply checking the sign of HANDLE. */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("DAFWCR", (ftnlen)6);
    }
    zzddhhlu_(handle, "DAF", &c_false, &unit, (ftnlen)3);

/*     Look out for */

/*       -- Writing to a file that is open for read-only. */

/*       -- Trying to write a record that doesn't have length 1000. */

/*       -- Failed write. */

    dafsih_(handle, "WRITE", (ftnlen)5);
    if (i_len(crec, crec_len) != 1000) {
	setmsg_("Expected length of character record is 1000. Length of pass"
		"ed record is #", (ftnlen)73);
	i__1 = i_len(crec, crec_len);
	errint_("#", &i__1, (ftnlen)1);
	sigerr_("SPICE(DAFBADCRECLEN)", (ftnlen)20);
    } else {
	io___3.ciunit = unit;
	io___3.cirec = *recno;
	iostat = s_wdue(&io___3);
	if (iostat != 0) {
	    goto L100001;
	}
	iostat = do_uio(&c__1, crec, crec_len);
	if (iostat != 0) {
	    goto L100001;
	}
	iostat = e_wdue();
L100001:
	if (iostat != 0) {
	    setmsg_("Character record write failed. Value of IOSTAT was #", (
		    ftnlen)52);
	    errint_("#", &iostat, (ftnlen)1);
	    sigerr_("SPICE(DAFWRITEFAIL)", (ftnlen)19);
	}
    }
    chkout_("DAFWCR", (ftnlen)6);
    return 0;
} /* dafwcr_ */

