/* locln.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;

/* $Procedure LOCLN ( Locate lines in a text file ) */
/* Subroutine */ int locln_(integer *unit, char *bmark, char *emark, char *
	line, integer *bline, integer *eline, logical *found, ftnlen 
	bmark_len, ftnlen emark_len, ftnlen line_len)
{
    /* System generated locals */
    integer i__1;
    cilist ci__1;

    /* Builtin functions */
    integer s_rsfe(cilist *), do_fio(integer *, char *, ftnlen), e_rsfe(void),
	     s_cmp(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    extern /* Subroutine */ int chkin_(char *, ftnlen), errch_(char *, char *,
	     ftnlen, ftnlen);
    extern integer ltrim_(char *, ftnlen);
    extern /* Subroutine */ int ljust_(char *, char *, ftnlen, ftnlen);
    logical bfound, efound;
    integer bltemp, eltemp;
    extern /* Subroutine */ int errfnm_(char *, integer *, ftnlen), sigerr_(
	    char *, ftnlen), chkout_(char *, ftnlen), setmsg_(char *, ftnlen);
    integer iostat;
    extern /* Subroutine */ int errint_(char *, integer *, ftnlen);
    extern logical return_(void);
    logical eof;

/* $ Abstract */

/*     Locate a group of lines in a text file delimited by markers. */

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

/*     None. */

/* $ Keywords */

/*     FILES */
/*     TEXT */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     UNIT       I   Logical unit connected to text file. */
/*     BMARK      I   Begin marker. */
/*     EMARK      I   End marker. */
/*     LINE      I-O  Workspace. */
/*     BLINE      O   Beginning line. */
/*     ELINE      O   Ending line. */
/*     FOUND      O   Markers found? */

/* $ Detailed_Input */

/*     UNIT     is a logical unit that has been connected to a */
/*              text file by the calling program. Use the routine */
/*              TXTOPR to open the file for read access and get its */
/*              logical unit. The file pointer may be pointing to */
/*              any line in the file due to previous read statements, */
/*              for example, or due to previous calls to LOCLN. */

/*     BMARK, */
/*     EMARK    are markers that delimit some group of lines in */
/*              the part of the file following the current position */
/*              of the file pointer. The group begins with the */
/*              first line equivalent to BMARK and ends with the */
/*              next line equivalent to EMARK, ignoring leading */
/*              and trailing blanks. */

/*              If BMARK is blank, the group of lines begins with */
/*              the first line following the current position of the */
/*              file pointer; if EMARK is blank, the group of lines */
/*              ends with the last line in the file. */

/*      LINE       on input, is an arbitrary character string whose */
/*              contents are ignored. LINE is used to read lines */
/*              from the file connected to UNIT; its function */
/*              is to determine the maximum length of the lines */
/*              that can be read from the file. Lines longer */
/*              than the declared length of LINE are truncated */
/*              as they are read. */

/* $ Detailed_Output */

/*     LINE     on output, is undefined. */

/*     BLINE, */
/*     ELINE    are the line numbers of the first and last lines */
/*              in the group delimited by BMARK and EMARK. */

/*              By convention, the first line read by the routine */
/*              is line 1; the second line is line 2; and so on. */
/*              If BMARK is blank, BLINE will be 1. */

/*     FOUND    is .TRUE. if a group of lines delimited by BMARK and */
/*              EMARK is found, and is .FALSE. otherwise. ELINE is */
/*              the last line read by LOCLN, so if FOUND is .TRUE., */
/*              the file pointer will be positioned on the line */
/*              after ELINE. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If FOUND is .FALSE., the values of BLINE and ELINE are not */
/*         changed. */

/*     2)  If an error occurs while reading from the input file, */
/*         the error SPICE(FILEREADFAILED) is signaled. */

/*     3)  Lines in the file that are longer than the declared length of */
/*         LINE are truncated as they are read. If the truncation of */
/*         line containing a marker causes truncation of that marker, */
/*         it will not match the input value for that marker, so */
/*         FOUND will be .FALSE. */

/* $ Files */

/*     See argument UNIT. */

/* $ Particulars */

/*     This routine locates delimited groups of lines in a text file. */
/*     This allows files to be partitioned into sub-files; it also */
/*     allows related inputs to be grouped together in a relatively */
/*     free-format way. */

/* $ Examples */

/*     1) Let FILE.TXT be a text file that contains the following lines. */
/*        (The lines are numbered for reference, but these numbers do */
/*        not appear in the file). */

/*           1    BEGIN POEM */
/*           2       Oh snail, */
/*           3       Climb Mount Fuji, */
/*           4       But slowly, slowly! */
/*           5    END POEM */
/*           6 */
/*           7    BEGIN PROSE */
/*           8       Lady, one of us has this book open */
/*           9       to the wrong page. */
/*           10   END PROSE */
/*           11 */
/*           12   BEGIN POEM */
/*           13      John Keats, John Keats, */
/*           14      John, */
/*           15      Put your scarf on. */
/*           16   END POEM */
/*           17 */
/*           18   BEGIN QUOTE */
/*           19      That's not writing. That's typing. */
/*           20 */
/*           21               (Truman Capote on Jack Kerouac) */
/*           22   END QUOTE */
/*           23 */
/*           24   BEGIN POEM */
/*           25      Twice five syllables */
/*           26      Plus seven isn't much, but */
/*           27      That's haiku for you. */
/*           28   BEGIN POEM */
/*           29 */
/*           30   BEGIN EQUATION */
/*           31            2 */
/*           32      e = mc */
/*           33   END EQUATION */

/*     Then the code fragment */

/*           CALL TXTOPR ( 'FILE.TXT', UNIT ) */

/*           BMARK = 'BEGIN POEM' */
/*           EMARK = 'END POEM' */

/*           CALL LOCLN ( UNIT, BMARK, EMARK, LINE, B, E, FOUND ) */

/*           DO WHILE ( FOUND ) */
/*              WRITE (*,*) 'Found poem between lines ', B, ' and ', E */

/*              CALL LOCLN ( UNIT, BMARK, EMARK, LINE, B, E, FOUND ) */
/*           END DO */

/*     produces the following report: */

/*           Found poem between lines   1 and   5 */
/*           Found poem between lines   7 and  11 */
/*           Found poem between lines   8 and  12 */

/*     Note that line numbers are returned relative to the position */
/*     of the file pointer when LOCLN is called. The following code */
/*     fragment generates the numbers relative to the start of the */
/*     file. */

/*           REWIND ( UNIT ) */

/*           OFFSET = 0 */
/*           CALL LOCLN ( UNIT, BMARK, EMARK, LINE, B, E, FOUND ) */

/*           DO WHILE ( FOUND ) */
/*              WRITE (*,*) 'Found poem between lines ', */
/*          .                OFFSET + B, */
/*          .                ' and ', */
/*          .                OFFSET + E */

/*              OFFSET = OFFSET + E */
/*              CALL LOCLN ( UNIT, BMARK, EMARK, LINE, B, E, FOUND ) */
/*           END DO */

/*           CLOSE ( UNIT ) */

/*     The following report is produced: */

/*           Found poem between lines   1 and   5 */
/*           Found poem between lines  12 and  16 */
/*           Found poem between lines  24 and  28 */


/*     2) Given the same file, the code fragment */

/*           CALL TXTOPR ( 'FILE.TXT', UNIT ) */

/*           CALL LOCLN ( UNIT, */
/*          .             'begin poem', */
/*          .             'end poem', */
/*          .             LINE, */
/*          .             B, */
/*          .             E, */
/*          .             FOUND ) */

/*           CLOSE ( UNIT ) */

/*     finds nothing because case is significant: FOUND is false, */
/*     and B and E are unchanged. */

/*     3) This code fragment */

/*           CALL TXTOPR ( 'FILE.TXT', UNIT ) */

/*           CALL LOCLN ( UNIT, */
/*          .             ' ', */
/*          .             'BEGIN PROSE', */
/*          .             LINE, */
/*          .             B, */
/*          .             E, */
/*          .             FOUND ) */

/*           CLOSE ( UNIT ) */

/*     when executed on the same file returns B = 1 and E = 7. */
/*     In effect, a blank begin marker "matches" the first line */
/*     that is read. */

/*     Similarly, a blank end marker "matches" the last line of */
/*     the file, the code fragment */

/*           CALL TXTOPR ( 'FILE.TXT', UNIT ) */

/*           CALL LOCLN ( UNIT, */
/*          .             'BEGIN QUOTE', */
/*          .             ' ', */
/*          .             LINE, */
/*          .             B, */
/*          .             E, */
/*          .             FOUND ) */

/*           CLOSE ( UNIT ) */

/*     when executed on the same file returns B = 18 and E = 33. */
/*     If both markers are blank, LOCLN basically counts the lines */
/*     in the file. */

/*     4) The code fragment */

/*           CALL TXTOPR ( 'FILE.TXT', UNIT ) */

/*           MARK = 'BEGIN POEM' */

/*           CALL LOCLN ( UNIT, MARK, MARK, LINE, FIRST, SECOND, FOUND ) */

/*           CLOSE ( UNIT ) */

/*     returns FIRST = 1 and SECOND = 12 -- the first two lines that */
/*     are equivalent to MARK. */

/*     5) Nesting is not supported. That is, if UNIT is connected to */
/*     a file containing the following lines (ignoring line numbers), */

/*           1   Begin Object */
/*           2     Begin Object */
/*           3       Begin Object */
/*           4         Just kidding! */
/*           5       End Object */
/*           6     End Object */
/*           7   End Object */

/*           REWIND ( UNIT ) */

/*           CALL LOCLN ( UNIT, */
/*          .             'Begin Object' */
/*          .             'End Object', */
/*          .             LINE, */
/*          .             B, */
/*          .             E, */
/*          .             FOUND ) */

/*     returns B = 1 and E = 5, not E = 7. */

/*     6) Let UNIT be connected to a text file containing the */
/*     following lines, again ignoring line numbers which are */
/*     listed for easy reference. */

/*           1    The first case tests the capability of ... */
/*           2 */
/*           3    NEW CASE */
/*           4       TARGET = JUPITER */
/*           5       EPOCH  = 21 JUN 1992 13:04 */
/*           6    END CASE */
/*           7 */
/*           8    The next case uses a different target and a slightly */
/*           9    longer exposure time... */
/*           10 */
/*           11   NEW CASE */
/*           12      TARGET   = IO */
/*           13      EPOCH    = 21 JUN 1992 13:04 */
/*           14      EXPOSURE = 2.44 SECONDS */
/*           15   END CASE */
/*           16 */
/*           17   The next case changes targets in order to... */
/*           18 */
/*           19   NEW CASE */
/*           20      TARGET   = EUROPA */
/*           21      EPOCH    = 21 JUN 1992 13:04 */
/*           22      EXPOSURE = 2.44 SECONDS */
/*           23   END CASE */

/*     Then the code fragment */

/*           REWIND ( UNIT ) */

/*           BMARK = 'NEW CASE' */
/*           EMARK = 'END CASE' */

/*           CASES  = 0 */
/*           OFFSET = 0 */
/*           CALL LOCLN ( UNIT, BMARK, EMARK, LINE, B, E, FOUND ) */

/*           DO WHILE ( FOUND ) */
/*              CASES      = CASES  + 1 */
/*              BEG(CASES) = OFFSET + B */
/*              END(CASES) = OFFSET + E */

/*              OFFSET = OFFSET + E */
/*              CALL LOCLN ( UNIT, BMARK, EMARK, LINE, B, E, FOUND ) */
/*           END DO */

/*     saves the locations of the various input cases (skipping past */
/*     the intervening commentary) in the arrays BEG and END. After */
/*     running the code, CASES, BEG, and END have the following values: */

/*           CASES = 3 */
/*           BEG   = 3,  11,  19 */
/*           END   = 6,  15,  23 */

/*     The following code fragment retrieves the i'th case. */

/*           REWIND ( UNIT ) */

/*           DO J = 1, BEG(I) - 1 */
/*              READ (UNIT,FMT='(A)') LINE */
/*           END DO */

/*           DO J = BEG(I), END(I) */
/*              READ (UNIT,FMT='(A)') LINE */
/*               . */
/*               .  Process the line */
/*               . */
/*           END DO */

/*     While this isn't an incredibly efficient way to process */
/*     large files, it can be effective for smaller files. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     J.E. McLean        (JPL) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 12-AUG-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 05-APR-1991 (JEM) */

/* -& */
/* $ Index_Entries */

/*     locate lines in a text file */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("LOCLN", (ftnlen)5);
    }

/*     We'll use temporary variables BLTEMP and ELTEMP for BLINE and */
/*     ELINE until we know that both markers have been found.  We'll */
/*     use BFOUND to indicate whether or not BMARK was found, and */
/*     EFOUND to indicate whether or not EMARK was found.  EOF */
/*     indicates end of file. */

    bltemp = 0;
    bfound = FALSE_;
    efound = FALSE_;
    eof = FALSE_;

/*     Read through the file, line by line, searching for the first */
/*     occurrence of BMARK and counting lines as we go.  Once we */
/*     find BMARK, we'll start searching for EMARK.  After each read */
/*     we'll check for I/O errors. */

    while(! bfound && ! eof) {
	ci__1.cierr = 1;
	ci__1.ciend = 1;
	ci__1.ciunit = *unit;
	ci__1.cifmt = "(A)";
	iostat = s_rsfe(&ci__1);
	if (iostat != 0) {
	    goto L100001;
	}
	iostat = do_fio(&c__1, line, line_len);
	if (iostat != 0) {
	    goto L100001;
	}
	iostat = e_rsfe();
L100001:

/*        An end-of-file condition is indicated by a negative value */
/*        for IOSTAT. Any other non-zero value indicates some other */
/*        error. */

	if (iostat > 0) {
	    setmsg_("While searching for BMARK = #, an attempt to read the f"
		    "ile named FILENAME failed.  The value of IOSTAT is #.", (
		    ftnlen)108);
	    errch_("#", bmark, (ftnlen)1, bmark_len);
	    errint_("#", &iostat, (ftnlen)1);
	    errfnm_("FILENAME", unit, (ftnlen)8);
	    sigerr_("SPICE(FILEREADFAILED)", (ftnlen)21);
	    chkout_("LOCLN", (ftnlen)5);
	    return 0;
	} else if (iostat < 0) {
	    eof = TRUE_;
	} else {

/*           The read was successful, so count the line then */
/*           check for a match. */

	    ++bltemp;
	    ljust_(line, line, line_len, line_len);

/*           By convention, if BMARK is blank, it matches the */
/*           first line that we read.  If it is not blank, we */
/*           compare it to the line just read, ignoring leading */
/*           and trailing blanks. */

	    if (s_cmp(bmark, " ", bmark_len, (ftnlen)1) == 0) {
		bfound = TRUE_;
	    } else {
		i__1 = ltrim_(bmark, bmark_len) - 1;
		if (s_cmp(bmark + i__1, line, bmark_len - i__1, line_len) == 
			0) {
		    bfound = TRUE_;
		}
	    }
	}
    }

/*     Start the search for EMARK starting from where we left off. */

    eltemp = bltemp;
    while(! efound && ! eof) {
	ci__1.cierr = 1;
	ci__1.ciend = 1;
	ci__1.ciunit = *unit;
	ci__1.cifmt = "(A)";
	iostat = s_rsfe(&ci__1);
	if (iostat != 0) {
	    goto L100002;
	}
	iostat = do_fio(&c__1, line, line_len);
	if (iostat != 0) {
	    goto L100002;
	}
	iostat = e_rsfe();
L100002:

/*        An end-of-file condition is indicated by a negative value */
/*        for IOSTAT. Any other non-zero value indicates some other */
/*        error. */

	if (iostat > 0) {
	    setmsg_("While searching for EMARK = #, an attempt to read the f"
		    "ile named FILENAME failed.  The value of IOSTAT is #.", (
		    ftnlen)108);
	    errch_("#", emark, (ftnlen)1, emark_len);
	    errint_("#", &iostat, (ftnlen)1);
	    errfnm_("FILENAME", unit, (ftnlen)8);
	    sigerr_("SPICE(FILEREADFAILED)", (ftnlen)21);
	    chkout_("LOCLN", (ftnlen)5);
	    return 0;
	} else if (iostat < 0) {
	    eof = TRUE_;

/*           By convention, if EMARK is blank, it matches the */
/*           last line in the file. */

	    if (s_cmp(emark, " ", emark_len, (ftnlen)1) == 0) {
		efound = TRUE_;
	    }
	} else {

/*           The read was successful, so count the line and check for */
/*           a match. */

	    ++eltemp;
	    ljust_(line, line, line_len, line_len);
	    if (s_cmp(emark, " ", emark_len, (ftnlen)1) != 0) {
		i__1 = ltrim_(emark, emark_len) - 1;
		if (s_cmp(emark + i__1, line, emark_len - i__1, line_len) == 
			0) {
		    efound = TRUE_;
		}
	    }
	}
    }

/*     Assign the line numbers to BLINE and ELINE only if both markers */
/*     were found. */

    *found = bfound && efound;
    if (*found) {
	*bline = bltemp;
	*eline = eltemp;
    }
    chkout_("LOCLN", (ftnlen)5);
    return 0;
} /* locln_ */

