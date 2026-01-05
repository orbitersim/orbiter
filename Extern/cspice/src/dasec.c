/* dasec.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static logical c_false = FALSE_;
static integer c__5000 = 5000;

/* $Procedure DASEC  ( DAS extract comments ) */
/* Subroutine */ int dasec_(integer *handle, integer *bufsiz, integer *n, 
	char *buffer, logical *done, ftnlen buffer_len)
{
    /* Initialized data */

    static logical first = TRUE_;

    /* System generated locals */
    integer i__1, i__2, i__3;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer), i_len(char *, ftnlen);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    extern /* Subroutine */ int zzddhhlu_(integer *, char *, logical *, 
	    integer *, ftnlen);
    integer i__, j, k;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    integer ncomc, recno, index, ncomr;
    char ch[1];
    extern logical failed_(void);
    char ifname[60];
    static integer filhan[5000];
    static char crecrd[1024];
    extern /* Subroutine */ int dasioc_(char *, integer *, integer *, char *, 
	    ftnlen, ftnlen);
    static integer filchr[5000];
    extern /* Subroutine */ int dassih_(integer *, char *, ftnlen);
    extern integer isrchi_(integer *, integer *, integer *);
    integer linlen, nchars, daslun;
    static integer filcnt[5000];
    char idword[8];
    static integer lsthan, nfiles, lstrec[5000];
    extern /* Subroutine */ int chkout_(char *, ftnlen);
    integer numcom;
    extern /* Subroutine */ int sigerr_(char *, ftnlen);
    integer nresvc;
    extern /* Subroutine */ int dasrfr_(integer *, char *, char *, integer *, 
	    integer *, integer *, integer *, ftnlen, ftnlen), setmsg_(char *, 
	    ftnlen), errint_(char *, integer *, ftnlen), errfnm_(char *, 
	    integer *, ftnlen);
    integer curpos;
    extern logical return_(void);
    integer nresvr;
    static integer lstpos[5000];
    logical eol;

/* $ Abstract */

/*     Extract comments from the comment area of a binary DAS file. */

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
/* $ Abstract */

/*     This file contains public, global parameter declarations */
/*     for the SPICELIB Direct Access Segregated (DAS) subsystem. */

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

/*     None. */

/* $ Restrictions */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 07-APR-2020 (JDR) */

/*        Added CHARDT, DPDT and INTDT parameters. */

/* -    SPICELIB Version 1.0.0, 10-FEB-2017 (NJB) */

/* -& */

/*     Parameter declarations follow. */


/*     DAS file table size: */

/*        The parameter name is FTSIZE. The value of the parameter is */
/*        defined in the include file */

/*           zzddhman.inc */

/*        That value is duplicated here, since zzddhman.inc contains */
/*        other declarations that conflict with some of those in DAS */
/*        routines. */


/*     Capacity of DAS data records: */

/*        -- NWD double precision numbers. */
/*        -- NWI integers. */
/*        -- NWC characters. */

/*     These parameters are named to enhance ease of maintenance of */
/*     the code; the values should not be changed. */

/*     DAS data type specifiers used in all DAS routines that require */
/*     a data type either as input or to extract data from an output */
/*     array. */

/*     CHARDT, */
/*     DPDT, */
/*     INTDT    are data type specifiers which indicate CHARACTER, */
/*              DOUBLE PRECISION, and INTEGER respectively. These */
/*              parameters are used in all DAS routines that require a */
/*              data type specifier. */


/*     End of include file das.inc */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     HANDLE     I   Handle of binary DAS file open with read access. */
/*     BUFSIZ     I   Maximum size, in lines, of BUFFER. */
/*     N          O   Number of comments extracted from the DAS file. */
/*     BUFFER     O   Buffer in which extracted comments are placed. */
/*     DONE       O   Indicates whether all comments have been extracted. */

/* $ Detailed_Input */

/*     HANDLE   is the file handle of a binary DAS file which has been */
/*              opened with read access. */

/*     BUFSIZ   is the maximum number of comments that may be placed into */
/*              BUFFER. This would typically be the declared array size */
/*              for the Fortran character string array passed into this */
/*              routine. */

/* $ Detailed_Output */

/*     N        is the number of comment lines extracted from the comment */
/*              area of the binary DAS file attached to HANDLE. This */
/*              number will be <= BUFSIZ on output. If N = BUFSIZ and */
/*              DONE <> .TRUE. then there are more comments left to to */
/*              extract. If N = 0, then DONE = .TRUE., i.e., there were */
/*              no comments in the comment area. If there are comments */
/*              in the comment area, or comments remaining after the */
/*              extraction process has begun, N > 0, always. */

/*     BUFFER   is a list of at most BUFSIZ comments which have been */
/*              extracted from the comment area of the binary DAS */
/*              file attached to HANDLE. */

/*     DONE     is a logical flag indicating whether or not all of the */
/*              comment lines from the comment area of the DAS file have */
/*              been read. This variable has the value .TRUE. after the */
/*              last comment line has been read. It will have the value */
/*              .FALSE. otherwise. */

/*              If there are no comments in the comment area, this */
/*              variable will have the value .TRUE., and N = 0. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the size of the output line buffer is is not positive, */
/*         the error SPICE(INVALIDARGUMENT) is signaled. */

/*     2)  If a comment line in a DAS file is longer than the length */
/*         of a character string array element of BUFFER, the error */
/*         SPICE(COMMENTTOOLONG) is signaled. */

/*     3)  If there is a mismatch between the number of comment */
/*         characters found and the number of comment characters */
/*         expected, the error SPICE(BADDASCOMMENTAREA) is signaled. */

/*     4)  If the binary DAS file attached to HANDLE is not open for */
/*         reading, an error is signaled by a routine in the call tree of */
/*         this routine. */

/* $ Files */

/*     See argument HANDLE in $Detailed_Input. */

/* $ Particulars */

/*     Binary DAS files contain an area which is reserved for storing */
/*     annotations or descriptive textual information describing the data */
/*     contained in a file. This area is referred to as the ``comment */
/*     area'' of the file. The comment area of a DAS file is a line */
/*     oriented medium for storing textual information. The comment */
/*     area preserves any leading or embedded white space in the line(s) */
/*     of text which are stored, so that the appearance of the of */
/*     information will be unchanged when it is retrieved (extracted) at */
/*     some other time. Trailing blanks, however, are NOT preserved, */
/*     due to the way that character strings are represented in */
/*     standard Fortran 77. */

/*     This routine will read the comments from the comment area of */
/*     a binary DAS file, placing them into a line buffer. If the line */
/*     buffer is not large enough to hold the entire comment area, */
/*     the portion read will be returned to the caller, and the DONE */
/*     flag will be set to .FALSE. This allows the comment area to be */
/*     read in ``chunks,'' a buffer at a time. After all of the comment */
/*     lines have been read, the DONE flag will be set to .TRUE. */

/*     This routine can be used to ``simultaneously'' extract comments */
/*     from the comment areas of multiple binary DAS files. See Example */
/*     2 in the $Examples section. */

/* $ Examples */

/*     Example 1 */
/*     --------- */

/*     The following example will extract the entire comment area of a */
/*     binary DAS file attached to HANDLE, displaying the comments on */
/*     the terminal screen. */

/*     Let */

/*        BUFFER  have the following declaration: */

/*           CHARACTER*(80)  BUFFER(25) */

/*        HANDLE  be the handle of an open binary DAS file. */

/*     then */

/*        BUFSIZ = 25 */
/*        DONE   = .FALSE. */

/*        DO WHILE ( .NOT. DONE ) */

/*           CALL DASEC( HANDLE, BUFSIZ, N, BUFFER, DONE ) */

/*           DO I = 1, N */

/*              WRITE (*,*) BUFFER(I) */

/*           END DO */

/*        END DO */

/*     Example 2 */
/*     --------- */

/*     The following example demonstrates the use of this routine to */
/*     simultaneously read the comment areas of multiple DAS files. */
/*     For each file, the comments will be displayed on the screen as */
/*     they are extracted. */

/*     Let */

/*        BUFFER  have the following declaration: */

/*           CHARACTER*(80)  BUFFER(25) */

/*        NUMFIL     be the number of binary DAS files that are to have */
/*                   their comment areas displayed. */

/*        DASNAM(I)  Be a list of filenames for the DAS files which are */
/*                   to have their comment areas displayed. */

/*        HANDLE(I)  be a list of handles for the DAS files which are */
/*                   to have their comment areas displayed. */

/*        DONE(I)    be a list of logical flags indicating whether */
/*                   we are done extracting the comment area from the */
/*                   DAS file attached to HANDLE(I) */

/*     then */

/*            BUFSIZ = 25 */

/*            DO I = 1, NUMFIL */

/*               DONE(I)   = .FALSE. */
/*               HANDLE(I) = 0 */

/*            END DO */
/*     C */
/*     C      Open the DAS files. */
/*     C */
/*            DO I = 1, NUMFIL */

/*               CALL DASOPR ( DASNAM(I), HANDLE(I) ) */

/*            END DO */
/*     C */
/*     C      While there are still some comments left to read in at */
/*     C      least one of the files, read them and display them. */
/*     C */
/*            DO WHILE ( .NOT. ALLTRU( DONE, NUMFIL ) ) */

/*               DO I = 1, NUMFIL */

/*                  IF ( .NOT. DONE(I) ) THEN */

/*                     WRITE (*,*) */
/*                     WRITE (*,*) 'File: ', DASNAM(I)(:RTRIM(DASNAM(I))) */
/*                     WRITE (*,*) */
/*                     N = 0 */

/*                     CALL DASEC ( HANDLE(I), */
/*           .                      BUFSIZ, */
/*           .                      N, */
/*           .                      BUFFER, */
/*           .                      DONE(I) ) */

/*                     DO J = 1, N */

/*                        WRITE (*,*) BUFFER(J)(:RTRIM(BUFFER(J))) */

/*                     END DO */

/*                  END IF */

/*               END DO */

/*            END DO */

/* $ Restrictions */

/*     1)  The comment area may consist only of printing ASCII */
/*         characters, decimal values 32 - 126. */

/*     2)  There is NO maximum length imposed on the significant portion */
/*         of a text line that may be placed into the comment area of a */
/*         DAS file. The maximum length of a line stored in the comment */
/*         area should be kept reasonable, so that they may be easily */
/*         extracted. A good value for this would be 255 characters, as */
/*         this can easily accommodate "screen width" lines as well as */
/*         long lines which may contain some other form of information. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     K.R. Gehringer     (JPL) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.4.1, 20-AUG-2021 (JDR) */

/*        Edited the header to comply with NAIF standard. Removed */
/*        reference to non-described parameters from entry #1 in */
/*        $Restrictions section. */

/* -    SPICELIB Version 1.4.0, 10-FEB-2017 (NJB) */

/*        Updated to use ZZDDHHLU. */

/*        Now imports parameter FTSIZE from das.inc. */

/* -    SPICELIB Version 1.3.0, 18-JUN-1999 (WLT) */

/*        Changed name used in CHKOUT to be consistent with the CHKIN */
/*        value. */

/* -    SPICELIB Version 1.2.0, 04-AUG-1994 (KRG) */

/*        Rearranged some of the code to avoid always reading the file */
/*        record. Now we look for the input HANDLE in the file table */
/*        first, and only read the file record if we do not find it. Also */
/*        added a new array to be saved: FILCNT. This is the number of */
/*        comment characters in a file; we save it now rather than */
/*        reading it every time. */

/*        Fixed a bug. If the Fortran character string array elements */
/*        have exactly the same length as a comment in the comment area, */
/*        this routine would halt rather unexpectedly from a memory over */
/*        run. */

/* -    SPICELIB Version 1.1.0, 22-NOV-1993 (KRG) */

/*        Changed the value of the parameter FTSIZE from 20 to 21. This */
/*        change makes the value of FTSIZE in DASEC compatible with the */
/*        value in DASFM. See DASFM for a discussion of the reasons for */
/*        the increase in the value. */

/* -    SPICELIB Version 1.0.0, 23-NOV-1992 (KRG) */

/* -& */
/* $ Index_Entries */

/*     extract comments from a DAS file */

/* -& */
/* $ Revisions */

/* -    SPICELIB Version 1.2.0, 04-AUG-1994 (KRG) */

/*        Rearranged some of the code to avoid always reading the file */
/*        record. Now we look for the input HANDLE in the file table */
/*        first, and only read the file record if we do not find it. Also */
/*        added a new array to be saved: FILCNT. This is the number of */
/*        comment characters in a file; we save it now rather than */
/*        reading it every time. */

/*        Fixed a bug. If the Fortran character string array elements */
/*        have exactly the same length as a comment in the comment area, */
/*        this routine would halt rather unexpectedly from a memory over */
/*        run. This occurred when attempting to clear, i.e., blank pad, */
/*        the portion of a character string element that extended beyond */
/*        the text in a comment line. A test has been added to verify */
/*        that blank padding can be performed. */

/* -    SPICELIB Version 1.1.0, 22-NOV-1993 (KRG) */

/*        Changed the value of the parameter FTSIZE from 20 to 21. This */
/*        change makes the value of FTSIZE in DASEC compatible with the */
/*        value in DASFM. See DASFM for a discussion of the reasons for */
/*        the increase in the value. */

/* -    SPICELIB Version 1.0.0, 23-NOV-1992 (KRG) */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     Length of a DAS character record, in characters. */


/*     Maximum and minimum decimal values for the printable ASCII */
/*     characters. */


/*     Decimal value for the DAS comment area end-of-line (EOL) marker. */


/*     Maximum length of a filename. */


/*     Length of a DAS file ID word. */


/*     Length of a DAS file internal filename. */


/*     Local variables */


/*     The file table declarations for keeping track of which files */
/*     are currently in the process of having comments extracted. */


/*     Saved variables */


/*     Save all of the file table information. */


/*     Initial values */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("DASEC", (ftnlen)5);
    }

/*     If this is the first time that this routine has been called, */
/*     we need to initialize the character value of the end-of-line */
/*     marker, and the file table variables. */

    if (first) {
	first = FALSE_;
	nfiles = 0;
	lsthan = -1;
	for (i__ = 1; i__ <= 5000; ++i__) {
	    filcnt[(i__1 = i__ - 1) < 5000 && 0 <= i__1 ? i__1 : s_rnge("fil"
		    "cnt", i__1, "dasec_", (ftnlen)494)] = 0;
	    filchr[(i__1 = i__ - 1) < 5000 && 0 <= i__1 ? i__1 : s_rnge("fil"
		    "chr", i__1, "dasec_", (ftnlen)495)] = 0;
	    filhan[(i__1 = i__ - 1) < 5000 && 0 <= i__1 ? i__1 : s_rnge("fil"
		    "han", i__1, "dasec_", (ftnlen)496)] = 0;
	    lstrec[(i__1 = i__ - 1) < 5000 && 0 <= i__1 ? i__1 : s_rnge("lst"
		    "rec", i__1, "dasec_", (ftnlen)497)] = 0;
	    lstpos[(i__1 = i__ - 1) < 5000 && 0 <= i__1 ? i__1 : s_rnge("lst"
		    "pos", i__1, "dasec_", (ftnlen)498)] = 0;
	}
    }

/*     Verify that the DAS file attached to HANDLE is opened for reading */
/*     by calling the routine to signal an invalid access mode on a */
/*     handle. */

    dassih_(handle, "READ", (ftnlen)4);
    if (failed_()) {
	chkout_("DASEC", (ftnlen)5);
	return 0;
    }

/*     Check for a nonpositive BUFFER size. */

    if (*bufsiz <= 0) {
	setmsg_("The output buffer size was not positive: #.", (ftnlen)43);
	errint_("#", bufsiz, (ftnlen)1);
	sigerr_("SPICE(INVALIDARGUMENT)", (ftnlen)22);
	chkout_("DASEC", (ftnlen)5);
	return 0;
    }

/*     Convert the DAS file handle to its corresponding Fortran logical */
/*     unit number for reading the comment records. */

    zzddhhlu_(handle, "DAS", &c_false, &daslun, (ftnlen)3);
    if (failed_()) {
	chkout_("DASEC", (ftnlen)5);
	return 0;
    }

/*     Get the length of a single character string in the buffer. */

    linlen = i_len(buffer, buffer_len);

/*     If we have extracted comments from at least one file and we */
/*     didn't finish, get the index for that file in the file table. */

    if (nfiles > 0) {
	index = isrchi_(handle, &nfiles, filhan);
    } else {
	index = 0;
    }

/*     Check to see if we found HANDLE in the file handle table. If */
/*     we did, INDEX will be > 0. */

    if (index > 0) {

/*        Set the record number and the starting position accordingly, */
/*        i.e., where we left off when we last read from that file. */

	recno = lstrec[(i__1 = index - 1) < 5000 && 0 <= i__1 ? i__1 : s_rnge(
		"lstrec", i__1, "dasec_", (ftnlen)566)];
	curpos = lstpos[(i__1 = index - 1) < 5000 && 0 <= i__1 ? i__1 : 
		s_rnge("lstpos", i__1, "dasec_", (ftnlen)567)];
	nchars = filchr[(i__1 = index - 1) < 5000 && 0 <= i__1 ? i__1 : 
		s_rnge("filchr", i__1, "dasec_", (ftnlen)568)];
	ncomc = filcnt[(i__1 = index - 1) < 5000 && 0 <= i__1 ? i__1 : s_rnge(
		"filcnt", i__1, "dasec_", (ftnlen)569)];
    } else {

/*        We have not yet read any comments from this file, so start at */
/*        the start. To get to the first comment record, we need to skip */
/*        the file record and any reserved records that are in the file. */
/*        The first comment record immediately follows the last reserved */
/*        record. */

/*        Get the current number of comment records and comment */
/*        characters from the DAS file attached to HANDLE. We will also */
/*        get back some extra stuff that we do not use. */

	dasrfr_(handle, idword, ifname, &nresvr, &nresvc, &ncomr, &ncomc, (
		ftnlen)8, (ftnlen)60);
	if (failed_()) {
	    chkout_("DASEC", (ftnlen)5);
	    return 0;
	}

/*        If the number of comment characters, NCOMC, is equal to zero, */
/*        then we have no comments to read, so set the number of comments */
/*        to zero, set DONE to .TRUE., check out,  and return. */

	if (ncomc == 0) {
	    *n = 0;
	    *done = TRUE_;
	    chkout_("DASEC", (ftnlen)5);
	    return 0;
	}
	recno = nresvr + 2;
	curpos = 1;
	nchars = 0;
    }

/*     Begin reading the comment area into the buffer. */

    if (*handle != lsthan) {

/*        If the current DAS handle is not the same as the handle on */
/*        the last call, then we need to read in the appropriate record */
/*        from the DAS file comment area. Otherwise the record was saved, */
/*        so we don't need to read it in. */

	dasioc_("READ", &daslun, &recno, crecrd, (ftnlen)4, (ftnlen)1024);
    }

/*     Initialize the BUFFER line counter, I, and the line position */
/*     counter, J. */

    i__ = 1;
    j = 1;
    *done = FALSE_;
    while(i__ <= *bufsiz && ! (*done)) {
	eol = FALSE_;
	while(! eol) {
	    ++nchars;
	    *(unsigned char *)ch = *(unsigned char *)&crecrd[curpos - 1];
	    if (*(unsigned char *)ch == 0) {
		eol = TRUE_;
		if (j <= linlen) {
		    s_copy(buffer + ((i__ - 1) * buffer_len + (j - 1)), " ", 
			    buffer_len - (j - 1), (ftnlen)1);
		}
	    } else {
		if (j <= linlen) {
		    *(unsigned char *)&buffer[(i__ - 1) * buffer_len + (j - 1)
			    ] = *(unsigned char *)ch;
		    ++j;
		} else {
		    setmsg_("The output buffer line length (#) was not long "
			    "enough to contain a comment line with length #.", 
			    (ftnlen)94);
		    errint_("#", &linlen, (ftnlen)1);
		    errint_("#", &i__, (ftnlen)1);
		    sigerr_("SPICE(COMMENTTOOLONG)", (ftnlen)21);
		    chkout_("DASEC", (ftnlen)5);
		    return 0;
		}
	    }

/*           If we have reached the end of the current comment record, */
/*           read in the next one and reset the current position. */
/*           Otherwise, just increment the current position. */

	    if (curpos == 1024) {
		++recno;
		dasioc_("READ", &daslun, &recno, crecrd, (ftnlen)4, (ftnlen)
			1024);
		curpos = 1;
	    } else {
		++curpos;
	    }

/*           Check to make sure that it is safe to continue, i.e., */
/*           that the number of comment characters we have processed */
/*           has not exceeded the number of comment characters in the */
/*           comment area of the DAS file. */

	    if (nchars > ncomc) {
		setmsg_("Count of comment characters (#) exceeds the number "
			"of comment characters (#) in the DAS file #.", (
			ftnlen)95);
		errint_("#", &nchars, (ftnlen)1);
		errint_("#", &ncomc, (ftnlen)1);
		errfnm_("#", &daslun, (ftnlen)1);
		sigerr_("SPICE(BADDASCOMMENTAREA)", (ftnlen)24);
		chkout_("DASEC", (ftnlen)5);
		return 0;
	    }
	}

/*        We have just completed a comment line, so we save the comment */
/*        number, increment the buffer line counter, I, and reset the */
/*        buffer line position counter, J. */

	numcom = i__;
	++i__;
	j = 1;

/*        Check for the end of the comments. */

	if (nchars == ncomc) {

/*           If we have reached the end of the comments, signaled */
/*           by having processed all of the comment characters, NCOMC, */
/*           then we are done. So, set DONE to .TRUE. and remove the */
/*           entry for this file from the file table. */

	    *done = TRUE_;
	    lsthan = -1;

/*           0 <= INDEX <= NFILES, and we only want to remove things */
/*           from the file table if: */

/*              1) There are files in the file table, NFILES > 0 */
/*              2) The file we are currently reading from is in the */
/*                 file table, INDEX > 0. */

/*           So, if INDEX > 0, we know that there are files in the file */
/*           table, and that we are currently reading from one of them. */

	    if (index > 0) {
		i__1 = nfiles - 1;
		for (k = index; k <= i__1; ++k) {
		    filcnt[(i__2 = k - 1) < 5000 && 0 <= i__2 ? i__2 : s_rnge(
			    "filcnt", i__2, "dasec_", (ftnlen)745)] = filcnt[(
			    i__3 = k) < 5000 && 0 <= i__3 ? i__3 : s_rnge(
			    "filcnt", i__3, "dasec_", (ftnlen)745)];
		    filchr[(i__2 = k - 1) < 5000 && 0 <= i__2 ? i__2 : s_rnge(
			    "filchr", i__2, "dasec_", (ftnlen)746)] = filchr[(
			    i__3 = k) < 5000 && 0 <= i__3 ? i__3 : s_rnge(
			    "filchr", i__3, "dasec_", (ftnlen)746)];
		    filhan[(i__2 = k - 1) < 5000 && 0 <= i__2 ? i__2 : s_rnge(
			    "filhan", i__2, "dasec_", (ftnlen)747)] = filhan[(
			    i__3 = k) < 5000 && 0 <= i__3 ? i__3 : s_rnge(
			    "filhan", i__3, "dasec_", (ftnlen)747)];
		    lstrec[(i__2 = k - 1) < 5000 && 0 <= i__2 ? i__2 : s_rnge(
			    "lstrec", i__2, "dasec_", (ftnlen)748)] = lstrec[(
			    i__3 = k) < 5000 && 0 <= i__3 ? i__3 : s_rnge(
			    "lstrec", i__3, "dasec_", (ftnlen)748)];
		    lstpos[(i__2 = k - 1) < 5000 && 0 <= i__2 ? i__2 : s_rnge(
			    "lstpos", i__2, "dasec_", (ftnlen)749)] = lstpos[(
			    i__3 = k) < 5000 && 0 <= i__3 ? i__3 : s_rnge(
			    "lstpos", i__3, "dasec_", (ftnlen)749)];
		}
		--nfiles;
	    }
	}
    }

/*     Set the number of comment lines in the buffer */

    *n = numcom;

/*     At this point, we have either filled the buffer or we have */
/*     finished reading in the comment area. Find out what has */
/*     happened and act accordingly. */

    if (! (*done)) {

/*        If we are not done, then we have filled the buffer, so save */
/*        everything that needs to be saved in the file table before */
/*        exiting. */

	if (index == 0) {

/*           This was the first time that the comment area of this file */
/*           has been read, so add it to the file table and save all of */
/*           its information if there is room in the file table. */

	    if (nfiles >= 5000) {
		setmsg_("The file table is full with # files, and another fi"
			"le could not be added.", (ftnlen)73);
		errint_("#", &c__5000, (ftnlen)1);
		sigerr_("SPICE(FILETABLEFULL)", (ftnlen)20);
		chkout_("DASEC", (ftnlen)5);
		return 0;
	    }
	    ++nfiles;
	    filcnt[(i__1 = nfiles - 1) < 5000 && 0 <= i__1 ? i__1 : s_rnge(
		    "filcnt", i__1, "dasec_", (ftnlen)793)] = ncomc;
	    filchr[(i__1 = nfiles - 1) < 5000 && 0 <= i__1 ? i__1 : s_rnge(
		    "filchr", i__1, "dasec_", (ftnlen)794)] = nchars;
	    filhan[(i__1 = nfiles - 1) < 5000 && 0 <= i__1 ? i__1 : s_rnge(
		    "filhan", i__1, "dasec_", (ftnlen)795)] = *handle;
	    lstrec[(i__1 = nfiles - 1) < 5000 && 0 <= i__1 ? i__1 : s_rnge(
		    "lstrec", i__1, "dasec_", (ftnlen)796)] = recno;
	    lstpos[(i__1 = nfiles - 1) < 5000 && 0 <= i__1 ? i__1 : s_rnge(
		    "lstpos", i__1, "dasec_", (ftnlen)797)] = curpos;
	    lsthan = *handle;
	} else {

/*           The comment area of this file is already in the file table, */
/*           so just update its information. */

	    filchr[(i__1 = index - 1) < 5000 && 0 <= i__1 ? i__1 : s_rnge(
		    "filchr", i__1, "dasec_", (ftnlen)805)] = nchars;
	    lstrec[(i__1 = index - 1) < 5000 && 0 <= i__1 ? i__1 : s_rnge(
		    "lstrec", i__1, "dasec_", (ftnlen)806)] = recno;
	    lstpos[(i__1 = index - 1) < 5000 && 0 <= i__1 ? i__1 : s_rnge(
		    "lstpos", i__1, "dasec_", (ftnlen)807)] = curpos;
	    lsthan = *handle;
	}
    }
    chkout_("DASEC", (ftnlen)5);
    return 0;
} /* dasec_ */

