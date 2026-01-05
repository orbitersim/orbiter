/* spcrfl.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static logical c_false = FALSE_;
static integer c__1 = 1;

/* $Procedure SPCRFL ( SPK and CK, read first line of comments ) */
/* Subroutine */ int spcrfl_0_(int n__, integer *handle, char *line, logical *
	eoc, ftnlen line_len)
{
    /* System generated locals */
    integer i__1;

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_rdue(cilist *), do_uio(integer *, char *, ftnlen), e_rdue(void);

    /* Local variables */
    static integer dafu, free;
    static char temp[1000], null[1];
    extern /* Subroutine */ int zzddhhlu_(integer *, char *, logical *, 
	    integer *, ftnlen), chkin_(char *, ftnlen);
    static integer bward, fward, nd;
    extern logical failed_(void);
    static logical called;
    static integer ni;
    static char ifname[60];
    static integer hanbuf;
    extern /* Subroutine */ int dafrfr_(integer *, integer *, integer *, char 
	    *, integer *, integer *, integer *, ftnlen);
    static char record[1000];
    static logical eocsav;
    extern /* Subroutine */ int errfnm_(char *, integer *, ftnlen), sigerr_(
	    char *, ftnlen), chkout_(char *, ftnlen);
    static integer tmplen;
    extern /* Subroutine */ int setmsg_(char *, ftnlen);
    static integer iostat;
    extern /* Subroutine */ int errint_(char *, integer *, ftnlen);
    extern logical return_(void);
    static integer posnul, rec, bol, eol;
    static char eot[1];
    extern integer pos_(char *, char *, integer *, ftnlen, ftnlen);
    static integer nrr;

    /* Fortran I/O blocks */
    static cilist io___16 = { 1, 0, 1, 0, 0 };
    static cilist io___21 = { 1, 0, 1, 0, 0 };
    static cilist io___24 = { 1, 0, 1, 0, 0 };


/* $ Abstract */

/*     Read the first line of text from the comment area */
/*     of a binary SPK or CK file. */

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

/*     SPC */

/* $ Keywords */

/*     FILES */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     HANDLE     I   Handle assigned to binary SPK or CK file. */
/*     LINE       O   First line of text from the comment area. */
/*     EOC        O   End of comments? */

/* $ Detailed_Input */

/*     HANDLE   is the handle assigned to the binary SPK or CK file */
/*              which has been opened for read access. */

/*              Use the SPICELIB routine DAFOPR to open the file for read */
/*              access and get HANDLE, unless SPKLEF or CKLPF has already */
/*              been called and returned the handle. This file is */
/*              unchanged by calling SPCRFL. */

/* $ Detailed_Output */

/*     LINE     is the first line of text from the comment area of */
/*              the SPK or CK file specified by HANDLE. LINE may */
/*              be blank. */

/*     EOC      is .TRUE. if the comment area is empty. If there */
/*              are comments in the comment area, then EOC is .FALSE. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the comment area of the SPK or CK file is empty, LINE */
/*         will be blank. */

/*     2)  If the first line of comments in the comment area is longer */
/*         than the declared length of LINE, it will be truncated to */
/*         fit into the variable. */

/*     3)  If there is a problem reading from the comment area, the error */
/*         SPICE(FILEREADFAILED) is signaled. */

/*     4)  If the comments are not in the correct format, the error */
/*         SPICE(FORMATERROR) is signaled. */

/* $ Files */

/*     See argument HANDLE. */

/* $ Particulars */

/*     The structure of SPK and CK files accommodates comments in */
/*     addition to data. The following routines are available */
/*     for accessing the comment area of a binary SPK or CK file: */

/*           SPCAC           add comments */

/*           SPCEC           extract comments */

/*           SPCDC           delete comments */

/*           SPCRFL          read first line of comments */

/*           SPCRNL          read next line of comments */

/*     Note that comments must consist of only text, that is, printable */
/*     ASCII characters, specifically ASCII 32-126. This excludes */
/*     tabs (ASCII 9) and control characters. */

/*     The SPC conversion routines---SPCB2A, SPCA2B, SPCB2T, and */
/*     SPCT2B---include these comments when converting SPK and CK */
/*     files between binary and text formats. */

/* $ Examples */

/*     Suppose we have a binary SPK file called A.BSP. The following */
/*     code fragment searches the comment area for a lines containing */
/*     the character string `SOURCE' and writes the lines to standard */
/*     output. */

/*      C */
/*      C     Open the binary SPK file and get its handle. */
/*      C */
/*            CALL DAFOPR ( 'A.BSP', HANDLE ) */

/*      C */
/*      C     Read the first line of comments. */
/*      C */
/*            CALL SPCRFL ( HANDLE, LINE, EOC ) */

/*      C */
/*      C     Search for the string 'SOURCE' in the line. If */
/*      C     it is found, write the line. Then get the next */
/*      C     line of comments and repeat as long as we're not */
/*      C     at the end. */
/*      C */
/*            DO WHILE ( .NOT. EOC ) */

/*               IF (  POS ( LINE, 'SOURCE', 1 ) .NE. 0  ) THEN */
/*                  WRITE (*,*) LINE */
/*               END IF */

/*               CALL SPCRNL ( LINE, EOC ) */

/*            END DO */

/* $ Restrictions */

/*     1)  This routine assumes that the comment area of the binary SPK */
/*         or CK file contains only text stored by SPCAC. Comments */
/*         written any other way may not be handled properly. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     K.R. Gehringer     (JPL) */
/*     J.E. McLean        (JPL) */
/*     W.L. Taber         (JPL) */
/*     F.S. Turner        (JPL) */

/* $ Version */

/* -    SPICELIB Version 2.1.0, 17-AUG-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. */

/*        Moved the contents of the $Files section to the description of */
/*        HANDLE in $Detailed_Input section, and referred to it from */
/*        $Files. */

/* -    SPICELIB Version 2.0.0, 16-NOV-2001 (FST) */

/*        Updated this routine to utilize the new handle manager */
/*        interfaces. */

/* -    SPICELIB Version 1.1.0, 27-JUL-1992 (KRG) */

/*        Removed a call to the SPICELIB subroutine SUFFIX() which */
/*        was used to join two parts of a comment line that may be */
/*        broken across two comment records. The problem was, SUFFIX */
/*        cannot know about leading/embedded blanks when it appends, so */
/*        blanks were inadvertently removed when they happened to be */
/*        stored at the end of comment record. */

/*        Added the variable TMPLEN to record the length of the first */
/*        part of a comment line that may be broken across comment */
/*        records. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 15-APR-1991 (JEM) */

/* -& */
/* $ Index_Entries */

/*     read the first comment line of an SPK or CK file */

/* -& */
/* $ Revisions */

/* -    SPICELIB Version 2.0.0, 16-NOV-2001 (FST) */

/*        Calls to DAFHLU now lock handles to their logical units. */
/*        While at first glance it may seem this is the appropriate */
/*        course of action due to the buffering of the logical unit */
/*        by this routine for its entry point, adding a call to */
/*        ZZDDHUNL in the entry point removes the need to lock DAFU */
/*        to its handle. The value of HANDLE is now buffered in */
/*        HANBUF, to allow the entry point to retrieve a logical */
/*        unit. */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */

/*     IFNLEN      is the length of a DAF internal file name. */

/*     MAXCPR      is the maximum number of characters per DAF record and */
/*                 hence the maximum comment line length. */


/*     Local variables */


/*     Saved variables */


/*     Standard SPICE error handling. */

    switch(n__) {
	case 1: goto L_spcrnl;
	}

    if (return_()) {
	return 0;
    } else {
	chkin_("SPCRFL", (ftnlen)6);
    }

/*     SPCRFL has been called for this file. */

    called = TRUE_;

/*     Read the file record to find out if the DAF contains any */
/*     reserved records.  The reserved records in an array file */
/*     are stored between the first record and the first summary */
/*     record.  FWARD is the record number of that first summary */
/*     record, and NRR is the number of reserved records in the file. */

/*     If there are no reserved records, there are no comments. */

    dafrfr_(handle, &nd, &ni, ifname, &fward, &bward, &free, (ftnlen)60);
    nrr = fward - 2;
    if (nrr == 0) {
	s_copy(line, " ", line_len, (ftnlen)1);
	*eoc = TRUE_;
	eocsav = *eoc;
	chkout_("SPCRFL", (ftnlen)6);
	return 0;
    }

/*     We need to read directly from the SPK/CK file, using a logical */
/*     unit instead of a handle. */

    zzddhhlu_(handle, "DAF", &c_false, &dafu, (ftnlen)3);
    if (failed_()) {
	chkout_("SPCRFL", (ftnlen)6);
	return 0;
    }

/*     Buffer the value of HANDLE. */

    hanbuf = *handle;

/*     In the comment area, NULL means end-of-line, and EOT means */
/*     end-of-transmission, or in other words, end-of-comments. */

    *(unsigned char *)null = '\0';
    *(unsigned char *)eot = '\4';

/*     Read the first reserved record. */

    rec = 2;
    io___16.ciunit = dafu;
    io___16.cirec = rec;
    iostat = s_rdue(&io___16);
    if (iostat != 0) {
	goto L100001;
    }
    iostat = do_uio(&c__1, record, (ftnlen)1000);
    if (iostat != 0) {
	goto L100001;
    }
    iostat = e_rdue();
L100001:
    if (iostat != 0) {
	setmsg_("Error reading comment area of the binary file named FNM at "
		"record #.  Value of IOSTAT is #.", (ftnlen)91);
	errint_("#", &rec, (ftnlen)1);
	errint_("#", &iostat, (ftnlen)1);
	errfnm_("FNM", &dafu, (ftnlen)3);
	sigerr_("SPICE(FILEREADFAILED)", (ftnlen)21);
	chkout_("SPCRFL", (ftnlen)6);
	return 0;
    }

/*     The first line of comments begins with the first character */
/*     of the record.  A NULL character specifies the end. */

    posnul = pos_(record, null, &c__1, (ftnlen)1000, (ftnlen)1);
    if (posnul == 0) {

/*        No NULL is in the record, so LINE is just the whole */
/*        record.  (The maximum length of a line written to */
/*        the comment area by SPCAC is MAXCPR characters). */

	eol = 1000;
    } else {

/*        The end of the line precedes the NULL character. */

	eol = posnul - 1;
    }

/*     Now we have the position of the end of the first line. */
/*     Assign it to LINE.  We're not yet at the end of comments, */
/*     since we have a line to return.  If the first character */
/*     was a NULL, the line is blank. */

    if (eol == 0) {
	s_copy(line, " ", line_len, (ftnlen)1);
    } else {
	s_copy(line, record, line_len, eol);
    }
    *eoc = FALSE_;
    eocsav = *eoc;
    chkout_("SPCRFL", (ftnlen)6);
    return 0;
/* $Procedure SPCRNL ( SPK and CK, read next line of comments ) */

L_spcrnl:
/* $ Abstract */

/*     Continue reading lines from the comment area of a binary */
/*     SPK or CK file specified by the most recent call to */
/*     the routine SPCRFL. */

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

/*     SPC */

/* $ Keywords */

/*     FILES */

/* $ Declarations */

/*     IMPLICIT NONE */

/*     CHARACTER*(*)         LINE */
/*     LOGICAL               EOC */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     LINE       O   Next line of text from the comment area. */
/*     EOC        O   End of comments? */

/* $ Detailed_Input */

/*     None. */

/* $ Detailed_Output */

/*     LINE     is the next line of text from the comment area of */
/*              the SPK or CK file. LINE may be blank. */
/*              SPCRFL reads the first line of comments from */
/*              a specified binary SPK or CK file. Once SPCRFL */
/*              has been called, SPCRNL may be called repetitively */
/*              to read the next lines of the comment area until */
/*              the end. */

/*     EOC      is .TRUE. if there are no more comments to read. */
/*              Otherwise, EOC is .FALSE. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If SPCRFL is not called prior to calling SPCRNL, the error */
/*         SPICE(SPCRFLNOTCALLED) is signaled. */

/*     2)  If the most recent call to SPCRFL returned EOC with the value */
/*         true, then SPCRNL will return EOC with the same value. */

/*     3)  If EOC is .TRUE., LINE will be blank. */

/*     4)  If the first line of comments in the comment area is longer */
/*         than the declared length of LINE, it will be truncated to */
/*         fit into the variable. */

/*     5)  If there is a problem reading from the comment area, the error */
/*         SPICE(FILEREADFAILED) is signaled. */

/*     6)  If the comments are not in the correct format, the error */
/*         SPICE(FORMATERROR) is signaled. */

/* $ Files */

/*     The handle of the binary SPK or CK is specified with the routine */
/*     SPCRFL. */

/* $ Particulars */

/*     The structure of SPK and CK files accommodates comments in */
/*     addition to data. The following five routines are available */
/*     for accessing the comment area of a binary SPK or CK file: */

/*           SPCAC           add comments */

/*           SPCEC           extract comments */

/*           SPCDC           delete comments */

/*           SPCRFL          read first line of comments */

/*           SPCRNL          read next line of comments */

/*     Note that comments must consist of only text, that is, printable */
/*     ASCII characters, specifically ASCII 32-126. This excludes */
/*     tabs (ASCII 9) and control characters. */

/*     The SPC conversion routines---SPCB2A, SPCA2B, SPCB2T, and */
/*     SPCT2B---include these comments when converting SPK and CK */
/*     files between binary and text formats. */

/* $ Examples */

/*     Suppose we have a binary SPK file called A.BSP. The following */
/*     code fragment searches the comment area for a lines containing */
/*     the character string `SOURCE' and writes the lines to standard */
/*     output. */

/*      C */
/*      C     Open the binary SPK file and get its handle. */
/*      C */
/*            CALL DAFOPR ( 'A.BSP', HANDLE ) */

/*      C */
/*      C     Read the first line of comments. */
/*      C */
/*            CALL SPCRFL ( HANDLE, LINE, EOC ) */

/*      C */
/*      C     Search for the string 'SOURCE' in the line. If */
/*      C     it is found, write the line. Then get the next */
/*      C     line of comments and repeat as long as we're not */
/*      C     at the end. */
/*      C */
/*            DO WHILE ( .NOT. EOC ) */

/*               IF (  POS ( LINE, 'SOURCE', 1 ) .NE. 0  ) THEN */
/*                  WRITE (*,*) LINE */
/*               END IF */

/*               CALL SPCRNL ( LINE, EOC ) */

/*            END DO */

/* $ Restrictions */

/*     1)  This routine assumes that the comment area of the binary SPK */
/*         or CK file contains only text stored by SPCAC. Comments */
/*         written any other way may not be handled properly. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     K.R. Gehringer     (JPL) */
/*     J.E. McLean        (JPL) */
/*     W.L. Taber         (JPL) */
/*     F.S. Turner        (JPL) */

/* $ Version */

/* -    SPICELIB Version 2.1.0, 17-AUG-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 2.0.0, 16-NOV-2001 (FST) */

/*        Updated this entry point to utilize the handle manager */
/*        interfaces. See the $Revisions section of the subroutine */
/*        header above for a detailed discussion of the changes. */

/* -    SPICELIB Version 1.1.0, 27-JUL-1992 (KRG) */

/*        Removed a call to the SPICELIB subroutine SUFFIX() which */
/*        was used to join two parts of a comment line that may be */
/*        broken across two comment records. The problem was, SUFFIX */
/*        cannot know about leading/embedded blanks when it appends, so */
/*        blanks were inadvertently removed when they happened to be */
/*        stored at the end of comment record. */

/*        Added the variable TMPLEN to record the length of the first */
/*        part of a comment line that may be broken across comment */
/*        records. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 15-APR-1991 (JEM) */

/* -& */
/* $ Index_Entries */

/*     continue reading comments from an SPK or CK file */
/*     read the next comment line of an SPK or CK file */

/* -& */

/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("SPCRNL", (ftnlen)6);
    }

/*     If SPCRFL hasn't been called, then we don't know which */
/*     file to read from. */

    if (! called) {
	setmsg_("You must call SPCRFL to read the first line of comments bef"
		"ore calling SPCRNL to read the next line.", (ftnlen)100);
	sigerr_("SPICE(SPCRFLNOTCALLED)", (ftnlen)22);
	chkout_("SPCRNL", (ftnlen)6);
	return 0;
    }

/*     If we were at the end of comments before, then we're still */
/*     at the end. */

    if (eocsav) {
	s_copy(line, " ", line_len, (ftnlen)1);
	*eoc = TRUE_;
	chkout_("SPCRNL", (ftnlen)6);
	return 0;
    }

/*     Retrieve a logical unit for HANBUF. */

    zzddhhlu_(&hanbuf, "DAF", &c_false, &dafu, (ftnlen)3);
    if (failed_()) {
	chkout_("SPCRNL", (ftnlen)6);
	return 0;
    }

/*     RECORD contains the last line and EOL is the position of */
/*     the end of that line.  Now we need to determine the */
/*     position of the beginning of the next line (BOL).  There */
/*     is a NULL between EOL and BOL, so BOL is two more than */
/*     EOL.  If that puts BOL off the end of the current RECORD, */
/*     then we have to go to the next record. */

    bol = eol + 2;
    if (bol > 1000) {
	bol += -1000;
	++rec;

/*        Check to make sure that we're not reading past the */
/*        reserved records.  FWARD is the "forward list pointer". */
/*        It is the number of the first record after the reserved */
/*        records. */

	if (rec >= fward) {
	    setmsg_("The comment area of the binary file named FNM is format"
		    "ted incorrectly. The end of the comments is not marked a"
		    "s it should be in record #. Calling SPCDC or DAFRRR will"
		    " remove the comment area and eliminate this format error"
		    ". Comments should be written ONLY by SPCAC.", (ftnlen)266)
		    ;
	    i__1 = rec - 1;
	    errint_("#", &i__1, (ftnlen)1);
	    errfnm_("FNM", &dafu, (ftnlen)3);
	    sigerr_("SPICE(FORMATERROR)", (ftnlen)18);
	    chkout_("SPCRNL", (ftnlen)6);
	    return 0;
	}

/*        All clear to read the record. */

	io___21.ciunit = dafu;
	io___21.cirec = rec;
	iostat = s_rdue(&io___21);
	if (iostat != 0) {
	    goto L100002;
	}
	iostat = do_uio(&c__1, record, (ftnlen)1000);
	if (iostat != 0) {
	    goto L100002;
	}
	iostat = e_rdue();
L100002:
	if (iostat != 0) {
	    setmsg_("Error reading comment area of the binary file named FNM"
		    " at record #.  Value of IOSTAT is #.", (ftnlen)91);
	    errint_("#", &rec, (ftnlen)1);
	    errint_("#", &iostat, (ftnlen)1);
	    errfnm_("FNM", &dafu, (ftnlen)3);
	    sigerr_("SPICE(FILEREADFAILED)", (ftnlen)21);
	    chkout_("SPCRNL", (ftnlen)6);
	    return 0;
	}
    }

/*     RECORD is now the record of the file that contains the */
/*     beginning of the next line (BOL).  The line may not */
/*     exist or may be blank or may be a character string. */

    if (*(unsigned char *)&record[bol - 1] == *(unsigned char *)eot) {

/*        There isn't a next line to get.  We're at the end of */
/*        the comments. */

	s_copy(line, " ", line_len, (ftnlen)1);
	*eoc = TRUE_;
	eocsav = *eoc;
	chkout_("SPCRNL", (ftnlen)6);
	return 0;
    }
    if (*(unsigned char *)&record[bol - 1] == *(unsigned char *)null) {

/*        Just a NULL means a blank line. */

	eol = bol - 1;
	s_copy(line, " ", line_len, (ftnlen)1);
	*eoc = FALSE_;
	eocsav = *eoc;
	chkout_("SPCRNL", (ftnlen)6);
	return 0;
    }

/*     The beginning of the next line is a character.  Now we have */
/*     to find the end.  It precedes the next NULL. */

    posnul = pos_(record, null, &bol, (ftnlen)1000, (ftnlen)1);
    if (posnul != 0) {
	eol = posnul - 1;
	s_copy(line, record + (bol - 1), line_len, eol - (bol - 1));
	*eoc = FALSE_;
	eocsav = *eoc;
    } else {

/*        There is no NULL in the rest of the record, so we have to */
/*        read the next record to find it.  Save the first part */
/*        of the line in TEMP. */

	s_copy(temp, record + (bol - 1), (ftnlen)1000, 1000 - (bol - 1));
	tmplen = 1000 - bol + 1;
	++rec;

/*        Check to make sure that we're not reading past the */
/*        reserved records.  FWARD is the "forward list pointer". */
/*        It is the number of the first record after the reserved */
/*        records. */

	if (rec >= fward) {
	    setmsg_("The comment area of the binary file named FNM is format"
		    "ted incorrectly. The end of the comments is not marked a"
		    "s it should be in record #. Calling SPCDC or DAFRRR will"
		    " remove the comment area and eliminate this format error"
		    ". Comments should be written ONLY by SPCAC.", (ftnlen)266)
		    ;
	    i__1 = rec - 1;
	    errint_("#", &i__1, (ftnlen)1);
	    errfnm_("FNM", &dafu, (ftnlen)3);
	    sigerr_("SPICE(FORMATERROR)", (ftnlen)18);
	    chkout_("SPCRNL", (ftnlen)6);
	    return 0;
	}

/*        All clear to read the record. */

	io___24.ciunit = dafu;
	io___24.cirec = rec;
	iostat = s_rdue(&io___24);
	if (iostat != 0) {
	    goto L100003;
	}
	iostat = do_uio(&c__1, record, (ftnlen)1000);
	if (iostat != 0) {
	    goto L100003;
	}
	iostat = e_rdue();
L100003:
	if (iostat != 0) {
	    setmsg_("Error reading comment area of the binary file named FNM"
		    " at record #.  Value of IOSTAT is #.", (ftnlen)91);
	    errint_("#", &rec, (ftnlen)1);
	    errint_("#", &iostat, (ftnlen)1);
	    errfnm_("FNM", &dafu, (ftnlen)3);
	    sigerr_("SPICE(FILEREADFAILED)", (ftnlen)21);
	    chkout_("SPCRNL", (ftnlen)6);
	    return 0;
	}

/*        There should be a null in this new record.  If there isn't */
/*        then something is wrong.  The maximum length of a line is */
/*        MAXCPR characters according to SPCAC.  So BOL and the NULL */
/*        should be in the same record or in adjacent records. */

	posnul = pos_(record, null, &c__1, (ftnlen)1000, (ftnlen)1);
	if (posnul == 0) {
	    setmsg_("Cannot find the end of the line.  There is something wr"
		    "ong with the format of thecomments.", (ftnlen)90);
	    sigerr_("SPICE(FORMATERROR)", (ftnlen)18);
	    chkout_("SPCRNL", (ftnlen)6);
	    return 0;
	}
	eol = posnul - 1;

/*        EOL is zero if the NULL was the first character of the */
/*        new record.  Otherwise, concatenate the two parts of */
/*        the line from the two adjacent records.  Then assign the */
/*        values of LINE and EOC. */

	if (eol != 0) {
	    i__1 = tmplen;
	    s_copy(temp + i__1, record, 1000 - i__1, eol);
	}
	s_copy(line, temp, line_len, (ftnlen)1000);
	*eoc = FALSE_;
	eocsav = *eoc;
    }
    chkout_("SPCRNL", (ftnlen)6);
    return 0;
} /* spcrfl_ */

/* Subroutine */ int spcrfl_(integer *handle, char *line, logical *eoc, 
	ftnlen line_len)
{
    return spcrfl_0_(0, handle, line, eoc, line_len);
    }

/* Subroutine */ int spcrnl_(char *line, logical *eoc, ftnlen line_len)
{
    return spcrfl_0_(1, (integer *)0, line, eoc, line_len);
    }

