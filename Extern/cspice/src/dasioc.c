/* dasioc.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;

/* $Procedure DASIOC ( DAS, Fortran I/O, character ) */
/* Subroutine */ int dasioc_(char *action, integer *unit, integer *recno, 
	char *record, ftnlen action_len, ftnlen record_len)
{
    /* Builtin functions */
    integer s_rdue(cilist *), do_uio(integer *, char *, ftnlen), e_rdue(void),
	     s_wdue(cilist *), e_wdue(void);

    /* Local variables */
    extern /* Subroutine */ int chkin_(char *, ftnlen), errch_(char *, char *,
	     ftnlen, ftnlen);
    extern logical eqstr_(char *, char *, ftnlen, ftnlen);
    extern /* Subroutine */ int errfnm_(char *, integer *, ftnlen), sigerr_(
	    char *, ftnlen), chkout_(char *, ftnlen), setmsg_(char *, ftnlen);
    integer iostat;
    extern /* Subroutine */ int errint_(char *, integer *, ftnlen);
    extern logical return_(void);

    /* Fortran I/O blocks */
    static cilist io___2 = { 1, 0, 1, 0, 0 };
    static cilist io___3 = { 1, 0, 0, 0, 0 };


/* $ Abstract */

/*     Perform Fortran reads and writes of DAS character records. */

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

/*     DAS */

/* $ Keywords */

/*     DAS */
/*     FILES */
/*     UTILITY */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     ACTION     I   Action to take (read or write). */
/*     UNIT       I   Fortran unit connected to DAS file. */
/*     RECNO      I   Number of record to read or write. */
/*     RECORD    I-O  DAS character record. */

/* $ Detailed_Input */

/*     ACTION   is a character string specifying whether to read */
/*              from or write to the specified DAS file. Possible */
/*              values are: */

/*                 'READ' */
/*                 'WRITE' */

/*              Case and leading or trailing blanks are not */
/*              significant. */


/*     UNIT     is the Fortran unit number connected to the DAS */
/*              file that is to be read or written. Given the */
/*              handle of the DAS file, the unit number can be */
/*              obtained using ZZDDHHLU. */

/*     RECNO    is the Fortran record number of the record to be */
/*              read or written. */

/*     RECORD   is a character array whose contents are to be */
/*              written to record RECNO, if ACTION is WRITE. */

/* $ Detailed_Output */

/*     RECORD   is a character array whose contents are to be */
/*              set equal to those of record RECNO, if ACTION is */
/*              READ. */

/* $ Parameters */

/*     NWC      is the number of characters in a DAS character */
/*              record. */

/* $ Exceptions */

/*     1)  If the value of ACTION is not recognized, the error */
/*         SPICE(UNRECOGNIZEDACTION) is signaled. */

/*     2)  If a Fortran read error occurs, the error */
/*         SPICE(DASFILEREADFAILED) is signaled. */

/*     3)  If a Fortran write error occurs, the error */
/*         SPICE(DASFILEWRITEFAILED) is signaled. */

/* $ Files */

/*     See the description of the argument UNIT in $Detailed_Input. */

/* $ Particulars */

/*     Normally, routines outside of SPICELIB will not need to call this */
/*     routine directly. Writes to DAS files should be performed using */
/*     the DASADx and DASUDx routines; reads should be performed using */
/*     the DASRDx routines. */

/*     This routines centralizes I/O and the concomitant error handling */
/*     for DAS character records. */

/*     Although most DAS routines use file handles to identify DAS */
/*     files, this routine uses Fortran logical units for this purpose. */
/*     Using unit numbers allows the DASIOx routines to be called from */
/*     any DAS routine, including entry points of DASFM.  (DASFM */
/*     contains as entry points the routines DASHLU and DASLUH, which */
/*     map between handles and unit numbers.) */

/* $ Examples */

/*     1)  Read and print to the screen character records number 10 */
/*         through 20 from the DAS file designated by HANDLE. */

/*            CHARACTER*(NWC)       RECORD */

/*                           . */
/*                           . */
/*                           . */

/*            CALL ZZDDHHLU ( HANDLE, 'DAS', .FALSE., UNIT ) */
/*            CALL DASHFN   ( HANDLE, NAME ) */

/*            DO I = 1, 20 */

/*               CALL DASIOC ( 'READ', UNIT, 10, RECORD ) */

/*               LABEL = 'Contents of the # record in DAS file #: ' */

/*               CALL REPMOT ( LABEL,  '#',  I,  'L',   LABEL ) */
/*               CALL REPMC  ( LABEL,  '#',      NAME,  LABEL ) */

/*               WRITE (*,*) LABEL */
/*               WRITE (*,*) ' ' */
/*               WRITE (*,*) RECORD */

/*            END DO */



/*     2)  Write the contents of the string RECORD to record number */
/*         10 in the DAS file designated by HANDLE. */


/*            CHARACTER*(NWC)       RECORD */

/*                           . */
/*                           . */
/*                           . */

/*            CALL ZZDDHHLU ( HANDLE,  'DAS', .FALSE., UNIT   ) */
/*            CALL DASIOC   ( 'WRITE', UNIT,  10,      RECORD ) */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 12-AUG-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 1.0.1, 05-FEB-2015 (NJB) */

/*        Header was updated to refer to ZZDDHHLU. */

/* -    SPICELIB Version 1.0.0, 30-JUN-1992 (NJB) (WLT) */

/* -& */
/* $ Index_Entries */

/*     perform Fortran reads of character records */
/*     perform Fortran writes of character records */
/*     perform low-level I/O for DAS routines */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     Use discovery check-in. */

    if (return_()) {
	return 0;
    }
    if (eqstr_(action, "READ", action_len, (ftnlen)4)) {

/*        We're supposed to read the file. */

	io___2.ciunit = *unit;
	io___2.cirec = *recno;
	iostat = s_rdue(&io___2);
	if (iostat != 0) {
	    goto L100001;
	}
	iostat = do_uio(&c__1, record, (ftnlen)1024);
	if (iostat != 0) {
	    goto L100001;
	}
	iostat = e_rdue();
L100001:
	if (iostat != 0) {
	    chkin_("DASIOC", (ftnlen)6);
	    setmsg_("Could not read DAS character record.  File = #  Record "
		    "number = #.  IOSTAT = #.", (ftnlen)79);
	    errfnm_("#", unit, (ftnlen)1);
	    errint_("#", recno, (ftnlen)1);
	    errint_("#", &iostat, (ftnlen)1);
	    sigerr_("SPICE(DASFILEREADFAILED)", (ftnlen)24);
	    chkout_("DASIOC", (ftnlen)6);
	    return 0;
	}
    } else if (eqstr_(action, "WRITE", action_len, (ftnlen)5)) {

/*        We're supposed to write to the file. */

	io___3.ciunit = *unit;
	io___3.cirec = *recno;
	iostat = s_wdue(&io___3);
	if (iostat != 0) {
	    goto L100002;
	}
	iostat = do_uio(&c__1, record, (ftnlen)1024);
	if (iostat != 0) {
	    goto L100002;
	}
	iostat = e_wdue();
L100002:
	if (iostat != 0) {
	    chkin_("DASIOC", (ftnlen)6);
	    setmsg_("Could not write DAS character record.  File = #  Record"
		    " number = #.  IOSTAT = #.", (ftnlen)80);
	    errfnm_("#", unit, (ftnlen)1);
	    errint_("#", recno, (ftnlen)1);
	    errint_("#", &iostat, (ftnlen)1);
	    sigerr_("SPICE(DASFILEWRITEFAILED)", (ftnlen)25);
	    chkout_("DASIOC", (ftnlen)6);
	    return 0;
	}
    } else {

/*        The requested action is a little too weird. */

	chkin_("DASIOC", (ftnlen)6);
	setmsg_("Action was #; should be READ or WRITE", (ftnlen)37);
	errch_("#", action, (ftnlen)1, action_len);
	sigerr_("SPICE(UNRECOGNIZEDACTION)", (ftnlen)25);
	chkout_("DASIOC", (ftnlen)6);
	return 0;
    }
    return 0;
} /* dasioc_ */

