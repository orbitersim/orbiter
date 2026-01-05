/* spcac.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static logical c_false = FALSE_;
static integer c__1 = 1;

/* $Procedure SPCAC ( SPK and CK, add comments ) */
/* Subroutine */ int spcac_(integer *handle, integer *unit, char *bmark, char 
	*emark, ftnlen bmark_len, ftnlen emark_len)
{
    /* System generated locals */
    integer i__1, i__2;
    cilist ci__1;
    alist al__1;

    /* Builtin functions */
    integer f_rew(alist *), s_cmp(char *, char *, ftnlen, ftnlen);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_rdue(cilist *), do_uio(integer *, char *, ftnlen), e_rdue(void),
	     i_indx(char *, char *, ftnlen, ftnlen), s_rsfe(cilist *), do_fio(
	    integer *, char *, ftnlen), e_rsfe(void), s_wdue(cilist *), 
	    e_wdue(void);

    /* Local variables */
    char data[1002];
    integer dafu, free;
    char line[1000], null[1];
    extern /* Subroutine */ int zzddhhlu_(integer *, char *, logical *, 
	    integer *, ftnlen);
    integer c__, i__, l, bline, space, eline;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    integer bward, chars, fward;
    extern /* Subroutine */ int locln_(integer *, char *, char *, char *, 
	    integer *, integer *, logical *, ftnlen, ftnlen, ftnlen);
    integer lines;
    logical found;
    integer total, start, nd;
    extern logical failed_(void);
    integer ni;
    extern /* Subroutine */ int dafsih_(integer *, char *, ftnlen);
    char ifname[60];
    integer nr;
    extern /* Subroutine */ int dafarr_(integer *, integer *), dafrfr_(
	    integer *, integer *, integer *, char *, integer *, integer *, 
	    integer *, ftnlen);
    char record[1000];
    extern integer lastnb_(char *, ftnlen);
    extern /* Subroutine */ int errfnm_(char *, integer *, ftnlen);
    extern integer countc_(integer *, integer *, integer *, char *, ftnlen);
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), setmsg_(char *, ftnlen);
    integer iostat;
    extern /* Subroutine */ int errint_(char *, integer *, ftnlen);
    integer lastrr, poseot;
    extern logical return_(void);
    integer rec, eol;
    char eot[1];
    integer nrr, pos;

    /* Fortran I/O blocks */
    static cilist io___24 = { 1, 0, 1, 0, 0 };
    static cilist io___33 = { 1, 0, 0, 0, 0 };
    static cilist io___34 = { 1, 0, 0, 0, 0 };


/* $ Abstract */

/*     Store text from a text file in the comment area of a binary SPK */
/*     or CK file, appending it to whatever text may already have */
/*     been stored there. */

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
/*     UNIT       I   Logical unit connected to comment file. */
/*     BMARK      I   Beginning marker. */
/*     EMARK      I   Ending marker. */

/* $ Detailed_Input */

/*     HANDLE   is the handle assigned to the binary SPK or CK file */
/*              which has been opened for write access. */

/*              Use the SPICELIB routine DAFOPW to open the file for */
/*              write access and get HANDLE. Upon exit, this binary file */
/*              will contain the specified text from the comment file in */
/*              its comment area, appended to whatever text may already */
/*              have been stored there. SPCAC will include an extra blank */
/*              line between the original text and the appended text. */

/*     UNIT     is the logical unit connected to the comment file. */
/*              This file must contain only text (printable */
/*              ASCII characters, namely ASCII 32-126).  Open this */
/*              file with read access and get its UNIT using TXTOPR. */

/*     BMARK, */
/*     EMARK    are markers that delimit a group of consecutive */
/*              lines in the text file (UNIT), that get stored in the */
/*              comment area of the binary file (HANDLE). */

/*              The group of lines begins with the line that */
/*              immediately follows the first line of the file */
/*              equivalent to BMARK. It ends with line that */
/*              precedes the next line of the file equivalent to */
/*              EMARK, including blank lines. Leading and */
/*              trailing blanks are ignored when testing for */
/*              equivalence. */

/*              By convention, if BMARK is blank, the first line of */
/*              the group is the first line of the file; if EMARK is */
/*              blank, the last line of the group is the last line */
/*              of the file. */

/*              If a marker is non-blank and is not found, or if */
/*              non-blank markers are on successive lines in the text */
/*              file, nothing gets stored in the comment area of */
/*              the binary file. */

/* $ Detailed_Output */

/*     None. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the specified DAF file is not open for write access, an */
/*         error is signaled by a routine in the call tree of this */
/*         routine. */

/*     2)  If there is a problem reading from the comment area of the */
/*         binary file, the error SPICE(FILEREADFAILED) is signaled. */

/*     3)  If there is a problem writing to the comment area of the */
/*         binary file, the error SPICE(FILEWRITEFAILED) is signaled. */

/*     4)  If there is a problem reading from the text file, an */
/*         error is signaled by a routine in the call tree of this */
/*         routine. */

/*     5)  If a non-printing ASCII character is encountered in the */
/*         comments, an error is signaled by a routine in the call tree */
/*         of this routine. */

/* $ Files */

/*     See arguments HANDLE and UNIT. */

/* $ Particulars */

/*     The structure of SPK and CK files accommodates comments in */
/*     addition to data. The following three routines are available */
/*     for accessing the comment area of a binary SPK or CK file: */

/*           SPCAC           add comments */

/*           SPCEC           extract comments */

/*           SPCDC           delete comments */

/*     Note that comments must consist of only text, that is, printable */
/*     ASCII characters, specifically ASCII 32-126. This excludes */
/*     tabs (ASCII 9) and control characters. */

/*     The SPC conversion routines---SPCB2A, SPCA2B, SPCB2T, and */
/*     SPCT2B---include these comments when converting SPK and CK */
/*     files between binary and text formats. */

/* $ Examples */

/*     Suppose we have a binary SPK file called A.BSP and we have */
/*     a text file called COMMENTS.TXT that contains comments */
/*     about the data in the SPK file. */

/*     The following code fragment stores the entire contents of */
/*     COMMENTS.TXT in the comment area of A.BSP. */

/*            CALL DAFOPW ( 'A.BSP', HANDLE ) */

/*            CALL TXTOPR ( 'COMMENTS.TXT', UNIT ) */

/*            BMARK = ' ' */
/*            EMARK = ' ' */

/*            CALL SPCAC  ( HANDLE, UNIT, BMARK, EMARK ) */

/*            CLOSE ( UNIT ) */

/*     Now suppose MORE.TXT is a text file that contains additional */
/*     information about the data in A.BSP, as well as information */
/*     about several other SPK files.  The contents of MORE.TXT are */

/*               \begin A info */

/*                 DATAFILE = A */
/*                 SOURCE   = JPL, 1990 September 12 */
/*                 MISSION  = Galileo */

/*               \end A info */

/*               \begin B info */

/*                 DATAFILE = B */
/*                 SOURCE   = JPL, 1988 August 1 */
/*                 MISSION  = Voyager 2 */

/*               \end B info */

/*               \begin C info */

/*                 DATAFILE = C */
/*                 SOURCE   = JPL, 1994 January 31 */
/*                 MISSION  = Mars Observer */

/*               \end C info */

/*     This code fragment stores only the information that pertains */
/*     to A.BSP, and appends it to the text from COMMENTS.TXT that */
/*     has already been stored in the comment area of A.BSP */

/*            CALL TXTOPR ( 'MORE.TXT', UNIT ) */

/*            BMARK = '\begin A info' */
/*            EMARK = '\end A info' */

/*            CALL SPCAC  ( HANDLE, UNIT, BMARK, EMARK ) */

/*            CLOSE ( UNIT ) */

/*            CALL DAFCLS ( HANDLE ) */

/*     Note that, ignoring leading and trailing blanks, BMARK and */
/*     EMARK are exactly equivalent to lines in the text file. */
/*     If the assignment had been instead BMARK = '\ begin A info', */
/*     with an extra space between the slash and the word begin, */
/*     SPCAC would not have found the marker and no comments from */
/*     the text file would be written to the binary file. */

/* $ Restrictions */

/*     1)  The lines in the comment file should not exceed 1000 */
/*         characters in length. SPCAC truncates lines longer than */
/*         this on the right. */

/*     2)  Use TXTOPR to open text files for read access and get */
/*         the logical unit. System dependencies regarding */
/*         opening text files have been isolated in the routines */
/*         TXTOPN and TXTOPR. */

/*     3)  This routine assumes that the comment area of the binary SPK */
/*         or CK file contains only text stored by SPCAC. Comments */
/*         written any other way may not be handled properly. */

/*     4)  The comment area of the binary SPK or CK file must contain */
/*         only one EOT character. This routine seeks back from the */
/*         last reserved record searching for the first EOT it */
/*         encounters. Thus the multiple EOT's will cause the appended */
/*         comments to be invisible to any reader that starts at the */
/*         first reserved record and reads until the first EOT present. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     K.R. Gehringer     (JPL) */
/*     J.E. McLean        (JPL) */
/*     W.L. Taber         (JPL) */
/*     F.S. Turner        (JPL) */

/* $ Version */

/* -    SPICELIB Version 2.1.0, 13-AUG-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. */

/*        Moved the contents of the $Files section to the description of */
/*        HANDLE and UNIT in $Detailed_Input section, and referred to */
/*        them from $Files. Removed unnecessary entries from $Revisions */
/*        section. */

/* -    SPICELIB Version 2.0.0, 16-NOV-2001 (FST) */

/*        Updated this routine to utilize new handle manager */
/*        interfaces. */

/* -    SPICELIB Version 1.3.0, 12-FEB-1999 (FST) */

/*        Modified the EOT search code to seek back through any */
/*        reserved records, as opposed to just the last one. This */
/*        provides the flexibility to use DAFOPN to reserve records */
/*        that may ultimately be used for storing comments. As a direct */
/*        result of these changes the SPICE(MISSINGEOT) error is no */
/*        longer signaled, since if no EOT is found in the reserved */
/*        records, they are considered available for writes. */

/* -    SPICELIB Version 1.2.0, 12-MAY-1994 (KRG) */

/*        Added an IF statement so that DAFARR is called only if new */
/*        reserved records need to be added to the comment area. */

/* -    SPICELIB Version 1.1.0, 09-APR-1993 (KRG) */

/*        Added code to initialize the variable LASTRR to zero. This */
/*        variable is used in a function call, MAX ( LASTRR-1, 1 ), */
/*        regardless of whether or not any reserved records are in */
/*        the file. Thus the need to initialize it. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 05-APR-1991 (JEM) */

/* -& */
/* $ Index_Entries */

/*     add comments to SPK or CK file */

/* -& */
/* $ Revisions */

/* -    SPICELIB Version 2.0.0, 16-NOV-2001 (FST) */

/*        This routine now utilizes DAFSIH to determine if */
/*        HANDLE is open for WRITE access. The call to DAFHLU */
/*        has been replaced with a call to ZZDDHHLU, the handle */
/*        manager interface for retrieving a logical unit. */
/*        DAFHLU is no longer used, since it locks the unit */
/*        returned to its HANDLE, tying up resources in the */
/*        handle manager. */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */

/*     IFNLEN      is the length of a DAF internal file name. */

/*     MAXCPR      is the maximum number of characters per DAF record and */
/*                 hence the maximum comment line length. */


/*     Local variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("SPCAC", (ftnlen)5);
    }

/*     Before doing anything, determine if the file associated with */
/*     HANDLE is available for WRITE access. */

    dafsih_(handle, "WRITE", (ftnlen)5);
    if (failed_()) {
	chkout_("SPCAC", (ftnlen)5);
	return 0;
    }

/*     Rewind the comment file - we'll start the search for BMARK */
/*     and EMARK at the beginning.  Once we have located the markers, */
/*     count the number of lines between them and the number of */
/*     characters in those lines, ignoring trailing blanks. */

/*     We rewind the file so that we know where the file pointer is. */
/*     LOCLN will compute BLINE and ELINE taking the current position */
/*     of the file pointer as line 1. */

    al__1.aerr = 0;
    al__1.aunit = *unit;
    f_rew(&al__1);
    locln_(unit, bmark, emark, line, &bline, &eline, &found, bmark_len, 
	    emark_len, (ftnlen)1000);

/*     If the markers are not found, or if BMARK and EMARK are on */
/*     successive lines, there is nothing to put in the comment area. */

    if (! found) {
	chkout_("SPCAC", (ftnlen)5);
	return 0;
    }

/*     Adjust BLINE and ELINE so we are pointing to the group of lines */
/*     BETWEEN the markers.  Check and make sure there is at least one */
/*     line in the group. */

    if (s_cmp(bmark, " ", bmark_len, (ftnlen)1) != 0) {
	++bline;
    }
    if (s_cmp(emark, " ", emark_len, (ftnlen)1) != 0) {
	--eline;
    }
    if (bline > eline) {
	chkout_("SPCAC", (ftnlen)5);
	return 0;
    }

/*     Calculate the number of lines and the total number of characters */
/*     in those lines.  The characters must all be printable, or */
/*     else COUNTC will signal an error. */

    lines = eline - bline + 1;
    chars = countc_(unit, &bline, &eline, line, (ftnlen)1000);
    if (failed_()) {
	chkout_("SPCAC", (ftnlen)5);
	return 0;
    }

/*     Read the file record to find out if the DAF contains any */
/*     reserved records.  The reserved records in an array file */
/*     are stored between the first record (the file record) and */
/*     the first summary record.  FWARD is the record number of */
/*     that first summary record, and NRR is the number of reserved */
/*     records in the file. */

    dafrfr_(handle, &nd, &ni, ifname, &fward, &bward, &free, (ftnlen)60);
    nrr = fward - 2;

/*     Get the logical unit for reading from and writing to the DAF. */

    zzddhhlu_(handle, "DAF", &c_false, &dafu, (ftnlen)3);
    if (failed_()) {
	chkout_("SPCAC", (ftnlen)5);
	return 0;
    }

/*     Assign the value of NULL and EOT.  NULL gets appended to the */
/*     end of each line of text.  EOT gets appended to the end of */
/*     all the comments.  Assign initial values for SPACE, RECORD, */
/*     and START. */

    *(unsigned char *)null = '\0';
    *(unsigned char *)eot = '\4';
    space = 0;
    s_copy(record, " ", (ftnlen)1000, (ftnlen)1);
    start = 0;
    lastrr = 0;
    if (nrr != 0) {

/*        At this point, we know there exist reserved records in the */
/*        DAF. We need to search from the last record to the first, */
/*        seeking for the EOT (end of transmission) character, as it */
/*        marks the end of the comment region. */

	lastrr = fward - 1;
	i__ = lastrr + 1;
	poseot = 0;
	while(i__ > 1 && poseot == 0) {

/*           Decrement the counter now.  This keeps it in */
/*           sync with the exit conditions. */

	    --i__;
	    io___24.ciunit = dafu;
	    io___24.cirec = i__;
	    iostat = s_rdue(&io___24);
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
		setmsg_("Error reading comment area of binary file named FIL"
			"E.  IOSTAT = *.", (ftnlen)66);
		errint_("*", &iostat, (ftnlen)1);
		errfnm_("FILE", &dafu, (ftnlen)4);
		sigerr_("SPICE(FILEREADFAILED)", (ftnlen)21);
		chkout_("SPCAC", (ftnlen)5);
		return 0;
	    }

/*           Call INDEX. If POSEOT is 0, then RECORD doesn't contain */
/*           the EOT character. */

	    poseot = i_indx(record, eot, (ftnlen)1000, (ftnlen)1);
	}

/*        The amount of free space in the reserved records of the */
/*        files is determined by the number of empty reserved */
/*        records ( LASTRR - I ), and the number of characters used */
/*        in last record with data (MAXCPR - POSEOT). */

	space = (lastrr - i__ + 1) * 1000 - poseot;

/*        Adjust the value of LASTRR to indicate the record where */
/*        the EOT lies.  From here on out, the purpose of this */
/*        variable is to indicate where to start dumping comments. */

	lastrr = i__;

/*        If POSEOT is 0, then there are no comments in the file, but */
/*        there are reserved records.  Branch on this: */

	if (poseot == 0) {

/*           Leaving this string index at zero may be causing all sorts */
/*           of warning bells to go off in your head. However, before */
/*           this index value is used to address the contents of a */
/*           string it's incremented by 1. */

	    start = poseot;

/*        Handle the case when POSEOT is non-zero. */

	} else {

/*           Replace the end-of-transmission character with a new line */
/*           character (we use null), so a blank line will come between */
/*           the old text and new text in the comment area.  START is the */
/*           position after which the first character of the new text */
/*           goes. */

	    *(unsigned char *)&record[poseot - 1] = *(unsigned char *)null;
	    start = poseot;
	}
    }

/*     Compute the number of records (NR) needed to store all of these */
/*     characters. */

/*     Each line should end with a null (ASCII 0) character.  The final */
/*     line should also be followed by an end-of-transmission (ASCII 4) */
/*     character.  So the total is the number of characters, plus the */
/*     number of lines, plus one for the EOT. */

/*     If the TOTAL fits in the SPACE available in the last reserved */
/*     record, we don't need to reserve any more.  Otherwise compute */
/*     the number we need. */

    total = chars + lines + 1;
    if (total - space > 0) {
	nr = (total - space - 1) / 1000 + 1;
    } else {
	nr = 0;
    }

/*     Reserve the records to create a comment area large enough */
/*     to hold it all, if we need to.  If we can't do it, there's no */
/*     point in going on. */

    if (nr > 0) {
	dafarr_(handle, &nr);
	if (failed_()) {
	    chkout_("SPCAC", (ftnlen)5);
	    return 0;
	}
    }

/*     Load the group of lines in the comment file into the reserved */
/*     records. Keep adding lines to the current record until it has */
/*     been filled, then write it to the DAF, and begin a new record. */

/* Computing MAX */
    i__1 = lastrr - 1;
    rec = max(i__1,1);
    pos = start;

/*     Rewind the text file then skip past the lines that we don't want */
/*     to position the file pointer at the correct record. */

    al__1.aerr = 0;
    al__1.aunit = *unit;
    f_rew(&al__1);
    i__1 = bline - 1;
    for (l = 1; l <= i__1; ++l) {
	ci__1.cierr = 1;
	ci__1.ciend = 1;
	ci__1.ciunit = *unit;
	ci__1.cifmt = "(A)";
	iostat = s_rsfe(&ci__1);
	if (iostat != 0) {
	    goto L100002;
	}
	iostat = do_fio(&c__1, line, (ftnlen)1000);
	if (iostat != 0) {
	    goto L100002;
	}
	iostat = e_rsfe();
L100002:
	if (iostat != 0) {
	    setmsg_("Error reading line # in text file named FILE.  IOSTAT ="
		    " *.", (ftnlen)58);
	    errint_("#", &l, (ftnlen)1);
	    errint_("*", &iostat, (ftnlen)1);
	    errfnm_("FILE", unit, (ftnlen)4);
	    sigerr_("SPICE(FILEREADFAILED)", (ftnlen)21);
	    chkout_("SPCAC", (ftnlen)5);
	    return 0;
	}
    }

/*     Start reading the lines that we do want.  LINE is MAXCPR long */
/*     so that's the maximum number of characters that are read. */

    i__1 = lines;
    for (l = 1; l <= i__1; ++l) {
	ci__1.cierr = 1;
	ci__1.ciend = 1;
	ci__1.ciunit = *unit;
	ci__1.cifmt = "(A)";
	iostat = s_rsfe(&ci__1);
	if (iostat != 0) {
	    goto L100003;
	}
	iostat = do_fio(&c__1, line, (ftnlen)1000);
	if (iostat != 0) {
	    goto L100003;
	}
	iostat = e_rsfe();
L100003:
	if (iostat != 0) {
	    setmsg_("Error reading line # in text file named FILE.  IOSTAT ="
		    " *.", (ftnlen)58);
	    i__2 = l + bline - 1;
	    errint_("#", &i__2, (ftnlen)1);
	    errint_("*", &iostat, (ftnlen)1);
	    errfnm_("FILE", unit, (ftnlen)4);
	    sigerr_("SPICE(FILEREADFAILED)", (ftnlen)21);
	    chkout_("SPCAC", (ftnlen)5);
	    return 0;
	}

/*        Each line is followed by a null character. */

	s_copy(data, line, (ftnlen)1002, (ftnlen)1000);
	eol = lastnb_(data, (ftnlen)1002) + 1;
	*(unsigned char *)&data[eol - 1] = *(unsigned char *)null;

/*        The final line is followed by an additional */
/*        end-of-transmission character. */

	if (l == lines) {
	    ++eol;
	    *(unsigned char *)&data[eol - 1] = *(unsigned char *)eot;
	}

/*        Moving characters one at a time is slower, but simpler, */
/*        than trying to move them in blocks. */

	i__2 = eol;
	for (c__ = 1; c__ <= i__2; ++c__) {

/*           If the current record is full, write it to the DAF. */

	    if (pos == 1000) {
		++rec;
		io___33.ciunit = dafu;
		io___33.cirec = rec;
		iostat = s_wdue(&io___33);
		if (iostat != 0) {
		    goto L100004;
		}
		iostat = do_uio(&c__1, record, (ftnlen)1000);
		if (iostat != 0) {
		    goto L100004;
		}
		iostat = e_wdue();
L100004:
		if (iostat == 0) {
		    s_copy(record, " ", (ftnlen)1000, (ftnlen)1);
		    pos = 0;
		} else {
		    setmsg_("Error writing to record # of the binary file na"
			    "med FILE. IOSTAT = *.", (ftnlen)68);
		    errint_("#", &rec, (ftnlen)1);
		    errint_("*", &iostat, (ftnlen)1);
		    errfnm_("FILE", &dafu, (ftnlen)4);
		    sigerr_("SPICE(FILEWRITEFAILED)", (ftnlen)22);
		    chkout_("SPCAC", (ftnlen)5);
		    return 0;
		}
	    }

/*           Add the next character to the current record. */

	    ++pos;
	    *(unsigned char *)&record[pos - 1] = *(unsigned char *)&data[c__ 
		    - 1];
	}
    }

/*     Write the final record to the DAF. */

    ++rec;
    io___34.ciunit = dafu;
    io___34.cirec = rec;
    iostat = s_wdue(&io___34);
    if (iostat != 0) {
	goto L100005;
    }
    iostat = do_uio(&c__1, record, (ftnlen)1000);
    if (iostat != 0) {
	goto L100005;
    }
    iostat = e_wdue();
L100005:
    if (iostat != 0) {
	setmsg_("Error writing the final record, record #, of the binary fil"
		"e named FILE.  IOSTAT = *.", (ftnlen)85);
	errint_("#", &rec, (ftnlen)1);
	errint_("*", &iostat, (ftnlen)1);
	errfnm_("FILE", &dafu, (ftnlen)4);
	sigerr_("SPICE(FILEWRITEFAILED)", (ftnlen)22);
	chkout_("SPCAC", (ftnlen)5);
	return 0;
    }
    chkout_("SPCAC", (ftnlen)5);
    return 0;
} /* spcac_ */

