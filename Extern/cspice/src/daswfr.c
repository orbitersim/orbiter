/* daswfr.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static logical c_false = FALSE_;
static integer c__1 = 1;

/* $Procedure DASWFR ( DAS write file record ) */
/* Subroutine */ int daswfr_(integer *handle, char *idword, char *ifname, 
	integer *nresvr, integer *nresvc, integer *ncomr, integer *ncomc, 
	ftnlen idword_len, ftnlen ifname_len)
{
    /* Builtin functions */
    integer s_rdue(cilist *), do_uio(integer *, char *, ftnlen), e_rdue(void);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_wdue(cilist *), e_wdue(void);

    /* Local variables */
    integer free;
    char tail[932];
    integer unit;
    extern /* Subroutine */ int zzddhhlu_(integer *, char *, logical *, 
	    integer *, ftnlen), chkin_(char *, ftnlen);
    extern logical failed_(void);
    integer oldcch, locncc, oldcrc;
    extern /* Subroutine */ int dashfs_(integer *, integer *, integer *, 
	    integer *, integer *, integer *, integer *, integer *, integer *);
    char locifn[60];
    integer oldrch;
    extern /* Subroutine */ int dassih_(integer *, char *, ftnlen);
    integer lastla[3];
    char locidw[8];
    integer locncr, locnvc, oldrrc;
    char format[8];
    integer lastrc[3];
    extern /* Subroutine */ int errfnm_(char *, integer *, ftnlen), sigerr_(
	    char *, ftnlen), chkout_(char *, ftnlen);
    integer lastwd[3];
    extern /* Subroutine */ int dasufs_(integer *, integer *, integer *, 
	    integer *, integer *, integer *, integer *, integer *, integer *),
	     setmsg_(char *, ftnlen);
    integer iostat, locnvr;
    extern /* Subroutine */ int errint_(char *, integer *, ftnlen);
    extern logical return_(void);
    char ifn[60];

    /* Fortran I/O blocks */
    static cilist io___3 = { 1, 0, 1, 0, 1 };
    static cilist io___13 = { 1, 0, 0, 0, 1 };


/* $ Abstract */

/*     Update the contents of the file record of a specified DAS file. */

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
/*     HANDLE     I   DAS file handle. */
/*     IDWORD     I   ID word. */
/*     IFNAME     I   DAS internal file name. */
/*     NRESVR     I   Number of reserved records in file. */
/*     NRESVC     I   Number of characters in use in reserved rec. area. */
/*     NCOMR      I   Number of comment records in file. */
/*     NCOMC      I   Number of characters in use in comment area. */

/* $ Detailed_Input */

/*     HANDLE   is a file handle for a DAS file open for writing. */

/*     IDWORD   is the `ID word' contained in the first eight */
/*              characters of the file record. */

/*     IFNAME   is the internal file name of the DAS file. The */
/*              maximum length of the internal file name is 60 */
/*              characters. */

/*     NRESVR   is the number of reserved records in the DAS file */
/*              specified by HANDLE. */

/*     NRESVC   is the number of characters in use in the reserved */
/*              record area of the DAS file specified by HANDLE. */

/*     NCOMR    is the number of comment records in the DAS file */
/*              specified by HANDLE. */

/*     NCOMC    is the number of characters in use in the comment area */
/*              of the DAS file specified by HANDLE. */

/* $ Detailed_Output */

/*     None. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the handle passed to this routine is not the handle of an */
/*         open DAS file, an error is signaled by a routine in the call */
/*         tree of this routine. */

/*     2)  If the specified DAS file is not open for write access, an */
/*         error is signaled by a routine in the call tree of this */
/*         routine. */

/*     3)  If the attempt to read the file record fails, the error */
/*         SPICE(DASREADFAIL) is signaled. */

/*     4)  If the file write attempted by this routine fails, the error */
/*         SPICE(DASFILEWRITEFAILED) is signaled. */

/* $ Files */

/*     See the description of HANDLE under $Detailed_Input. */

/* $ Particulars */

/*     This routine provides a convenient way of updating the internal */
/*     file name of a DAS file. */

/*     The `ID word' contained in the file record is a string of eight */
/*     characters that identifies the file as a DAS file and optionally */
/*     indicates a specific file format, for example, `EK'. */

/* $ Examples */

/*     1)  Update the internal file name of an existing DAS file. */

/*            C */
/*            C     Open the file for writing. */
/*            C */
/*                  CALL DASOPW ( FNAME, HANDLE  ) */

/*            C */
/*            C     Retrieve the ID word and current reserved record */
/*            C     and comment area record and character counts. */
/*            C */
/*                  CALL DASRFR ( HANDLE, */
/*                 .              IDWORD, */
/*                 .              IFNAME, */
/*                 .              NRESVR, */
/*                 .              NRESVC, */
/*                 .              NCOMR, */
/*                 .              NCOMC  ) */

/*            C */
/*            C     Set the internal file name and update the file */
/*            C     with it. */
/*            C */
/*                  IFNAME = 'New internal file name' */

/*                  CALL DASWFR ( HANDLE, */
/*                 .              IDWORD, */
/*                 .              IFNAME, */
/*                 .              NRESVR, */
/*                 .              NRESVC, */
/*                 .              NCOMR, */
/*                 .              NCOMC  ) */

/* $ Restrictions */

/*     1)  The DAS file must have a binary file format native to the host */
/*         system. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     K.R. Gehringer     (JPL) */
/*     W.L. Taber         (JPL) */
/*     F.S. Turner        (JPL) */

/* $ Version */

/* -    SPICELIB Version 3.1.1, 02-JUN-2021 (JDR) */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 3.1.0, 05-FEB-2015 (NJB) */

/*        Updated to support integration with the handle */
/*        manager subsystem. */

/* -    SPICELIB Version 3.0.0, 11-DEC-2001 (FST) */

/*        This routine was modified to accommodate the preservation */
/*        of the FTP validation and binary file format strings that */
/*        are not part of the DAS file record. */

/* -    SPICELIB Version 2.0.0, 27-OCT-1993 (KRG) */

/*        Removed references to specific DAS file open routines in the */
/*        $Detailed_Input section of the header. This was done in order */
/*        to minimize documentation changes if these open routines ever */
/*        change. */

/*        Added a check of FAILED after the call to DASHLU which will */
/*        check out and return if DASHLU fails. This is so that when in */
/*        return mode of the error handling the READ following the call */
/*        to DASHLU will not be executed. */

/*        Reworded some of the descriptions contained in the */
/*        $Detailed_Output section of the header so that they were more */
/*        clear. */

/* -    SPICELIB Version 1.0.0, 24-NOV-1992 (NJB) (WLT) */

/* -& */
/* $ Index_Entries */

/*     write DAS file record */
/*     write DAS internal file name */
/*     update DAS internal file name */

/* -& */
/* $ Revisions */

/* -    SPICELIB Version 3.0.0, 11-DEC-2001 (FST) */

/*        In order to preserve the additional information that */
/*        now resides in the file record, this routine reads */
/*        the entire record into local buffers, including the */
/*        TAILEN characters that follow the actual data content. */
/*        The contents of the local buffers that correspond to */
/*        information brought in from the call sequence of the */
/*        routine are ignored when the record is rewritten. */
/*        However, the ID word, the file format string, and the */
/*        trailing TAILEN characters that contain the FTP validation */
/*        string are rewritten along with the input values. */

/*        This routine does not simply replace the FTP validation */
/*        string with the components from ZZFTPSTR, since that */
/*        would possibly validate a corrupt file created using a newer */
/*        Toolkit. */

/*        The string arguments passed into this routine are now */
/*        copied to local buffers of the appropriate length. */

/* -    SPICELIB Version 2.0.0, 27-OCT-1993 (KRG) */

/*        Removed references to specific DAS file open routines in the */
/*        $Detailed_Input section of the header. This was done in order */
/*        to minimize documentation changes if these open routines ever */
/*        change. */

/*        Added a check of FAILED after the call to DASHLU which will */
/*        check out and return if DASHLU fails. This is so that when in */
/*        return mode of the error handling the READ following the call */
/*        to DASHLU will not be executed. */

/*        Reworded some of the descriptions contained in the */
/*        $Detailed_Output section of the header so that they were more */
/*        clear. */

/* -    SPICELIB Version 1.0.0, 24-NOV-1992 (NJB) (WLT) */

/* -& */

/*     SPICELIB functions */


/*     Local Parameters */


/*     The parameter TAILEN determines the tail length of a DAS file */
/*     record.  This is the number of bytes (characters) that */
/*     occupy the portion of the file record that follows the */
/*     integer holding the first free address.  For environments */
/*     with a 32 bit word length, 1 byte characters, and DAS */
/*     record sizes of 1024 bytes, we have: */

/*           8 bytes - IDWORD */
/*          60 bytes - IFNAME */
/*           4 bytes - NRESVR (32 bit integer) */
/*           4 bytes - NRESVC (32 bit integer) */
/*           4 bytes - NCOMR  (32 bit integer) */
/*         + 4 bytes - NCOMC  (32 bit integer) */
/*          --------- */
/*          84 bytes - (All file records utilize this space.) */

/*     So the size of the remaining portion (or tail) of the DAS */
/*     file record for computing environments as described above */
/*     would be: */

/*        1024 bytes - DAS record size */
/*      -    8 bytes - DAS Binary File Format Word */
/*      -   84 bytes - (from above) */
/*       ------------ */
/*         932 bytes - DAS file record tail length */

/*     Note: environments that do not have a 32 bit word length, */
/*     1 byte characters, and a DAS record size of 1024 bytes, will */
/*     require the adjustment of this parameter. */


/*     Local variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }
    chkin_("DASWFR", (ftnlen)6);

/*     Check to be sure that HANDLE is attached to a file that is open */
/*     with write access.  If the call fails, check out and return. */

    dassih_(handle, "WRITE", (ftnlen)5);

/*     Get the logical unit for this DAS file. */

    zzddhhlu_(handle, "DAS", &c_false, &unit, (ftnlen)3);
    if (failed_()) {
	chkout_("DASWFR", (ftnlen)6);
	return 0;
    }

/*     In order to maintain the integrity of the file ID word, the */
/*     file FORMAT, and the FTP string if present, we need to */
/*     read the entire file record into the appropriate sized local */
/*     buffers. The values of the LOCxxx variables are simply */
/*     ignored, since the caller passes new values in for updates. */

    io___3.ciunit = unit;
    iostat = s_rdue(&io___3);
    if (iostat != 0) {
	goto L100001;
    }
    iostat = do_uio(&c__1, locidw, (ftnlen)8);
    if (iostat != 0) {
	goto L100001;
    }
    iostat = do_uio(&c__1, locifn, (ftnlen)60);
    if (iostat != 0) {
	goto L100001;
    }
    iostat = do_uio(&c__1, (char *)&locnvr, (ftnlen)sizeof(integer));
    if (iostat != 0) {
	goto L100001;
    }
    iostat = do_uio(&c__1, (char *)&locnvc, (ftnlen)sizeof(integer));
    if (iostat != 0) {
	goto L100001;
    }
    iostat = do_uio(&c__1, (char *)&locncr, (ftnlen)sizeof(integer));
    if (iostat != 0) {
	goto L100001;
    }
    iostat = do_uio(&c__1, (char *)&locncc, (ftnlen)sizeof(integer));
    if (iostat != 0) {
	goto L100001;
    }
    iostat = do_uio(&c__1, format, (ftnlen)8);
    if (iostat != 0) {
	goto L100001;
    }
    iostat = do_uio(&c__1, tail, (ftnlen)932);
    if (iostat != 0) {
	goto L100001;
    }
    iostat = e_rdue();
L100001:
    if (iostat != 0) {
	setmsg_("Attempt to read the file record failed for file '#'. IOSTAT"
		" = #", (ftnlen)63);
	errfnm_("#", &unit, (ftnlen)1);
	errint_("#", &iostat, (ftnlen)1);
	sigerr_("SPICE(DASREADFAIL)", (ftnlen)18);
	chkout_("DASWFR", (ftnlen)6);
	return 0;
    }

/*     Set the value of the internal file name and IDWORD before */
/*     writing.  This is to guarantee that their lengths are ok. */

    s_copy(ifn, ifname, (ftnlen)60, ifname_len);
    s_copy(locidw, idword, (ftnlen)8, idword_len);
    io___13.ciunit = unit;
    iostat = s_wdue(&io___13);
    if (iostat != 0) {
	goto L100002;
    }
    iostat = do_uio(&c__1, locidw, (ftnlen)8);
    if (iostat != 0) {
	goto L100002;
    }
    iostat = do_uio(&c__1, ifn, (ftnlen)60);
    if (iostat != 0) {
	goto L100002;
    }
    iostat = do_uio(&c__1, (char *)&(*nresvr), (ftnlen)sizeof(integer));
    if (iostat != 0) {
	goto L100002;
    }
    iostat = do_uio(&c__1, (char *)&(*nresvc), (ftnlen)sizeof(integer));
    if (iostat != 0) {
	goto L100002;
    }
    iostat = do_uio(&c__1, (char *)&(*ncomr), (ftnlen)sizeof(integer));
    if (iostat != 0) {
	goto L100002;
    }
    iostat = do_uio(&c__1, (char *)&(*ncomc), (ftnlen)sizeof(integer));
    if (iostat != 0) {
	goto L100002;
    }
    iostat = do_uio(&c__1, format, (ftnlen)8);
    if (iostat != 0) {
	goto L100002;
    }
    iostat = do_uio(&c__1, tail, (ftnlen)932);
    if (iostat != 0) {
	goto L100002;
    }
    iostat = e_wdue();
L100002:
    if (iostat != 0) {
	setmsg_("Could not write file record.  File was #.  IOSTAT was #.", (
		ftnlen)56);
	errfnm_("#", &unit, (ftnlen)1);
	errint_("#", &iostat, (ftnlen)1);
	sigerr_("SPICE(DASFILEWRITEFAILED)", (ftnlen)25);
	chkout_("DASWFR", (ftnlen)6);
	return 0;
    }

/*     Update the file summary, in case the values of the reserved */
/*     record or comment area counts have changed. */

    dashfs_(handle, &oldrrc, &oldrch, &oldcrc, &oldcch, &free, lastla, lastrc,
	     lastwd);
    dasufs_(handle, nresvr, nresvc, ncomr, ncomc, &free, lastla, lastrc, 
	    lastwd);
    chkout_("DASWFR", (ftnlen)6);
    return 0;
} /* daswfr_ */

