/* dasac.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static logical c_false = FALSE_;

/* $Procedure DASAC ( DAS add comments ) */
/* Subroutine */ int dasac_(integer *handle, integer *n, char *buffer, ftnlen 
	buffer_len)
{
    /* Initialized data */

    static logical first = TRUE_;

    /* System generated locals */
    integer i__1, i__2, i__3;

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    extern /* Subroutine */ int zzddhhlu_(integer *, char *, logical *, 
	    integer *, ftnlen);
    integer i__, j, space;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    integer ncomc, recno, ncomr;
    extern logical failed_(void);
    extern /* Subroutine */ int dasacr_(integer *, integer *);
    char ifname[60], crecrd[1024];
    extern /* Subroutine */ int dasioc_(char *, integer *, integer *, char *, 
	    ftnlen, ftnlen), dassih_(integer *, char *, ftnlen);
    integer nchars;
    extern integer lastnb_(char *, ftnlen);
    integer length, newrec, daslun;
    extern /* Subroutine */ int dasrfr_(integer *, char *, char *, integer *, 
	    integer *, integer *, integer *, ftnlen, ftnlen);
    char idword[8];
    static char eolmrk[1];
    extern /* Subroutine */ int errfnm_(char *, integer *, ftnlen), sigerr_(
	    char *, ftnlen), chkout_(char *, ftnlen), daswfr_(integer *, char 
	    *, char *, integer *, integer *, integer *, integer *, ftnlen, 
	    ftnlen);
    integer nresvc;
    extern /* Subroutine */ int setmsg_(char *, ftnlen), errint_(char *, 
	    integer *, ftnlen);
    integer rinuse, curpos;
    extern logical return_(void);
    integer nresvr;

/* $ Abstract */

/*     Add comments from a buffer of character strings to the comment */
/*     area of a binary DAS file, appending them to any comments which */
/*     are already present in the file's comment area. */

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

/*     FILES */
/*     UTILITY */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     HANDLE    I    DAS handle of a file opened with write access. */
/*     N         I    Number of comments to put into the comment area. */
/*     BUFFER    I    Buffer of lines to be put into the comment area. */

/* $ Detailed_Input */

/*     HANDLE   is the file handle of a binary DAS file which has been */
/*              opened with write access. */

/*     N        is the number of comments in BUFFER that are to be */
/*              added to the comment area of the binary DAS file */
/*              attached to HANDLE. */

/*     BUFFER   is a buffer containing comments which are to be added */
/*              to the comment area of the binary DAS file attached */
/*              to HANDLE. */

/* $ Detailed_Output */

/*     None. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the number of comments to be added is not positive, the */
/*         error SPICE(INVALIDARGUMENT) is signaled. */

/*     2)  If a non printing ASCII character is encountered in the */
/*         comments, the error SPICE(ILLEGALCHARACTER) is signaled. */

/*     3)  If the binary DAS file attached to HANDLE is not open with */
/*         write access, an error is signaled by a routine in the call */
/*         tree of this routine. */

/* $ Files */

/*     See argument HANDLE in $Detailed_Input. */

/* $ Particulars */

/*     Binary DAS files contain a data area which is reserved for storing */
/*     annotations or descriptive textual information about the data */
/*     contained in a file. This area is referred to as the ``comment */
/*     area'' of the file. The comment area of a DAS file is a line */
/*     oriented medium for storing textual information. The comment */
/*     area preserves any leading or embedded white space in the line(s) */
/*     of text which are stored so that the appearance of the */
/*     information will be unchanged when it is retrieved (extracted) at */
/*     some other time. Trailing blanks, however, are NOT preserved, */
/*     due to the way that character strings are represented in */
/*     standard Fortran 77. */

/*     This routine will take a buffer of text lines and add (append) */
/*     them to the comment area of a binary DAS file. If there are no */
/*     comments in the comment area of the file, then space will be */
/*     allocated and the text lines in BUFFER will then placed into the */
/*     comment area. The text lines may contain only printable ASCII */
/*     characters (decimal values 32 - 126). */

/*     There is NO maximum length imposed on the significant portion */
/*     of a text line that may be placed into the comment area of a */
/*     DAS file. The maximum length of a line stored in the comment */
/*     area should be reasonable, however, so that they may be easily */
/*     extracted. A good value for this would be 255 characters, as */
/*     this can easily accommodate ``screen width'' lines as well as */
/*     long lines which may contain some other form of information. */

/* $ Examples */

/*     Let */

/*           HANDLE   be the handle for a DAS file which has been opened */
/*                    with write access. */

/*           N        be the number of lines of text to be added to the */
/*                    comment area of the binary DAS file attached to */
/*                    HANDLE. */

/*           BUFFER   is a list of text lines to be added to the comment */
/*                    area of the binary DAS file attached to HANDLE. */

/*     The call */

/*           CALL DASAC ( HANDLE, N, BUFFER ) */

/*     will append the first N line(s) in BUFFER to the comment area */
/*     of the binary DAS file attached to HANDLE. */

/* $ Restrictions */

/*     1)  This routine uses constants that are specific to the ASCII */
/*         character sequence. The results of using this routine with */
/*         a different character sequence are unpredictable. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     K.R. Gehringer     (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.2.0, 13-AUG-2021 (JDR) */

/*        Added IMPLICIT NONE standard. */

/*        Edited the header to comply with NAIF standard. Removed */
/*        unnecessary $Revisions section and fixed the first two entry */
/*        version lines (Beta -> SPICELIB). */

/* -    SPICELIB Version 1.1.0, 05-FEB-2015 (NJB) (KRG) */

/*        Updated to use ZZDDHHLU. */

/* -    SPICELIB Version 1.0.1, 12-MAY-1994 (KRG) */

/*        Fixed a typo in the $Particulars section. */

/* -    SPICELIB Version 1.0.0, 23-NOV-1992 (KRG) */

/* -& */
/* $ Index_Entries */

/*     add comments to a binary DAS file */
/*     append comments to a DAS file comment area */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */

/*     Length of a DAS character record, in characters. */


/*     Maximum and minimum decimal values for the printable ASCII */
/*     characters. */


/*     Decimal value for the DAS comment area end-of-line (EOL) marker. */


/*     Length of a DAS file ID word. */


/*     Length of a DAS file internal filename. */


/*     Local variables */


/*     Saved variables */


/*     Initial values */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("DASAC", (ftnlen)5);
    }

/*     The lines of text in BUFFER will be ``packed'' into DAS comment */
/*     records: the significant portion of each comment line from BUFFER */
/*     will be terminated by the special character EOLMRK to indicate the */
/*     end of the line. When a comment record is full or all of the */
/*     comments have been added to the file, the comment record will be */
/*     written to the comment area of the binary DAS file. */

/*     If this is the first time that this routine has been called, */
/*     we need to initialize the character value for the end-of-line */
/*     marker. */

    if (first) {
	first = FALSE_;
	*(unsigned char *)eolmrk = '\0';
    }

/*     Verify that the DAS file attached to HANDLE is opened with write */
/*     access. */

    dassih_(handle, "WRITE", (ftnlen)5);
    if (failed_()) {
	chkout_("DASAC", (ftnlen)5);
	return 0;
    }

/*     Convert the DAS file handle to its corresponding Fortran logical */
/*     unit number for reading and writing comment records. */

    zzddhhlu_(handle, "DAS", &c_false, &daslun, (ftnlen)3);
    if (failed_()) {
	chkout_("DASAC", (ftnlen)5);
	return 0;
    }

/*     Check for a nonpositive number of lines in the buffer. */

    if (*n <= 0) {
	setmsg_("The number of comment lines to be added to the binary DAS f"
		"ile # was not positive: #.", (ftnlen)85);
	errfnm_("#", &daslun, (ftnlen)1);
	errint_("#", n, (ftnlen)1);
	sigerr_("SPICE(INVALIDARGUMENT)", (ftnlen)22);
	chkout_("DASAC", (ftnlen)5);
	return 0;
    }

/*     Count the number of characters in the buffer ignoring trailing */
/*     blanks on nonblank lines and blank lines. The count will be */
/*     modified to include the contribution of blank lines later. This */
/*     count is used to determine the number of character records to be */
/*     added to the binary DAS file attached to HANDLE. */

    nchars = 0;
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {

/*        Get the length of the significant portion of a comment line. */

	length = lastnb_(buffer + (i__ - 1) * buffer_len, buffer_len);

/*        Scan the comment line for non printing characters. */

	i__2 = length;
	for (j = 1; j <= i__2; ++j) {

/*           Check to see that the characters in the buffer are all */
/*           printing ASCII characters. The bounds for printing ASCII */
/*           characters are given by MAXPCH and MINPCH, which are */
/*           defined in the $ Local Parameters section of the header. */

	    if (*(unsigned char *)&buffer[(i__ - 1) * buffer_len + (j - 1)] > 
		    126 || *(unsigned char *)&buffer[(i__ - 1) * buffer_len + 
		    (j - 1)] < 32) {
		setmsg_("A nonprinting character was encountered in the comm"
			"ent buffer. Value: #", (ftnlen)71);
		i__3 = *(unsigned char *)&buffer[(i__ - 1) * buffer_len + (j 
			- 1)];
		errint_("#", &i__3, (ftnlen)1);
		sigerr_("SPICE(ILLEGALCHARACTER)", (ftnlen)23);
		chkout_("DASAC", (ftnlen)5);
		return 0;
	    }
	}

/*        Increment the number of characters by the length of the */
/*        significant portion of the current line in the buffer. */

	nchars += length;
    }

/*     We need to include the number of end of line markers in the */
/*     number of characters, so add the number of comment lines to */
/*     be added, N, to the number of characters, NCHARS. This is where */
/*     the contribution of any blank lines gets added to the character */
/*     count. */

    nchars += *n;

/*     Get the current number of comment records and comment characters */
/*     from the DAS file attached to HANDLE. We will also get back some */
/*     extra stuff that we do not use. */

    dasrfr_(handle, idword, ifname, &nresvr, &nresvc, &ncomr, &ncomc, (ftnlen)
	    8, (ftnlen)60);
    if (failed_()) {
	chkout_("DASAC", (ftnlen)5);
	return 0;
    }

/*     Determine the amount of free space in the comment area. If */
/*     there are some comment records allocated, the space available */
/*     is the number of comment records allocated times the length of */
/*     a comment record, minus the number of comment characters already */
/*     used. Otherwise, the space available is zero. */

    if (ncomr > 0) {
	space = (ncomr << 10) - ncomc;
    } else {
	space = 0;
    }

/*     Determine the number of new comment records which are necessary */
/*     to store all of the comments from the buffer. */

    if (nchars > space) {

/*        If there are more characters to store than available space */
/*        we need at least one new record. */

	newrec = (nchars - space - 1) / 1024 + 1;
    } else {

/*        Otherwise, we do not need any new records. */

	newrec = 0;
    }

/*     Now add the necessary number of comment records to the file, */
/*     if we need to add any. */

    if (newrec > 0) {
	dasacr_(handle, &newrec);
	if (failed_()) {
	    chkout_("DASAC", (ftnlen)5);
	    return 0;
	}

/*        Update the value for the number of comment records to include */
/*        those that were just added. We need this value when we write */
/*        the file record at the end of the routine to update the number */
/*        comment characters, NCOMC. */

	ncomr += newrec;
    }

/*     At this point, we know that we have enough space to write all of */
/*     the comments in BUFFER to the comment area. Either there was */
/*     enough space already there, or we figured out how many new comment */
/*     records were needed, and we added them to the file. So, now we */
/*     begin ``packing'' the comments into DAS character records and */
/*     writing them to the file. */

/*     We begin by reading the last comment record if there is one. */
/*     Otherwise we just initialize the appropriate variables. */

    if (ncomc == 0) {

/*        If there are no comments in the comment area, then we need to */
/*        skip the file record and the reserved records, if any. The */
/*        first available comment record is the record immediately */
/*        after the last reserved record, so we set RECNO accordingly. */
/*        We also initialize the current position in the comment record, */
/*        and the comment record itself. */

	recno = nresvr + 2;
	curpos = 1;
	s_copy(crecrd, " ", (ftnlen)1024, (ftnlen)1);
    } else {

/*        If there are comments in the comment area, then we need to skip */
/*        the file record, the reserved records, if any, and any comment */
/*        records which have been filled. The first comment record */
/*        with space available is the record immediately following the */
/*        last completely filled comment record. So calculate the number */
/*        of comment records in use, and set RECNO appropriately. Then */
/*        calculate the initial position and read in the comment record. */

	rinuse = ncomc / 1024 + 1;
	recno = nresvr + 1 + rinuse;
	curpos = ncomc - (rinuse - 1 << 10) + 1;
	dasioc_("READ", &daslun, &recno, crecrd, (ftnlen)4, (ftnlen)1024);
	if (failed_()) {
	    chkout_("DASAC", (ftnlen)5);
	    return 0;
	}
    }

/*     Begin ``packing'' the comments from the input buffer into the */
/*     comment records, writing the comment records to the DAS file */
/*     as they become filled. */

    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {

/*        Get the length of the significant portion of a comment line. */

	length = lastnb_(buffer + (i__ - 1) * buffer_len, buffer_len);

/*        Process the comment line. */

	i__2 = length;
	for (j = 1; j <= i__2; ++j) {

/*           If we have filled the comment record while processing */
/*           comment line BUFFER(I), write out the comment record, */
/*           increment the record number, RECNO, and reset the values */
/*           of the current position and the comment record. */

	    if (curpos > 1024) {
		dasioc_("WRITE", &daslun, &recno, crecrd, (ftnlen)5, (ftnlen)
			1024);
		if (failed_()) {
		    chkout_("DASAC", (ftnlen)5);
		    return 0;
		}
		++recno;
		curpos = 1;
		s_copy(crecrd, " ", (ftnlen)1024, (ftnlen)1);
	    }
	    *(unsigned char *)&crecrd[curpos - 1] = *(unsigned char *)&buffer[
		    (i__ - 1) * buffer_len + (j - 1)];
	    ++curpos;
	}

/*        Check to see if we happened to exactly fill the comment record */
/*        when we finished processing comment line BUFFER(I). If we */
/*        did, CURPOS will be 1 greater than MXCREC, and we will need */
/*        to write the comment record to the file, increment the record */
/*        number, RECNO, and reset the values of the current position */
/*        and the comment record. */

	if (curpos > 1024) {
	    dasioc_("WRITE", &daslun, &recno, crecrd, (ftnlen)5, (ftnlen)1024)
		    ;
	    if (failed_()) {
		chkout_("DASAC", (ftnlen)5);
		return 0;
	    }
	    ++recno;
	    curpos = 1;
	    s_copy(crecrd, " ", (ftnlen)1024, (ftnlen)1);
	}

/*        Append the end-of-line marker to the comment line that we just */
/*        placed into the comment record. */

	*(unsigned char *)&crecrd[curpos - 1] = *(unsigned char *)eolmrk;
	++curpos;
    }

/*     We have now finished processing all of the comment lines in */
/*     BUFFER, so we need write the current record to the file. This */
/*     record will always contain something, so we always need to write */
/*     it. */

    dasioc_("WRITE", &daslun, &recno, crecrd, (ftnlen)5, (ftnlen)1024);
    if (failed_()) {
	chkout_("DASAC", (ftnlen)5);
	return 0;
    }

/*     And finally, we need to update the number of comment characters */
/*     in the file record by adding NCHARS, and writing the file record. */

    ncomc += nchars;
    daswfr_(handle, idword, ifname, &nresvr, &nresvc, &ncomr, &ncomc, (ftnlen)
	    8, (ftnlen)60);

/*     Check out and leave DASAC. A test of FAILED should be done by */
/*     the calling routine to catch an error that may occur during */
/*     the call to DASWFR. */

    chkout_("DASAC", (ftnlen)5);
    return 0;
} /* dasac_ */

