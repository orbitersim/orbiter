/* countc.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;

/* $Procedure COUNTC ( Count characters in a text file ) */
integer countc_(integer *unit, integer *bline, integer *eline, char *line, 
	ftnlen line_len)
{
    /* System generated locals */
    integer ret_val;
    cilist ci__1;
    alist al__1;

    /* Builtin functions */
    integer f_rew(alist *), s_rsfe(cilist *), do_fio(integer *, char *, 
	    ftnlen), e_rsfe(void), s_cmp(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    logical done;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    integer chars, linect;
    extern integer lastnb_(char *, ftnlen);
    extern /* Subroutine */ int errfnm_(char *, integer *, ftnlen), sigerr_(
	    char *, ftnlen), chkout_(char *, ftnlen), setmsg_(char *, ftnlen);
    integer iostat;
    extern /* Subroutine */ int errint_(char *, integer *, ftnlen), astrip_(
	    char *, char *, char *, char *, ftnlen, ftnlen, ftnlen, ftnlen);
    extern logical return_(void);

/* $ Abstract */

/*     Count the characters in a group of lines in a text file. */

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

/*     CHARACTERS */
/*     FILES */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     UNIT       I   Logical unit connected to text file. */
/*     BLINE      I   Beginning line number. */
/*     ELINE      I   Ending line number. */
/*     LINE      I-O  Workspace. */

/*     The function returns the number of characters in a group of lines */
/*     in a text file. */

/* $ Detailed_Input */

/*     UNIT     is a logical unit that has been connected to a */
/*              text file by the calling program. Use the routine */
/*              TXTOPR to open the file for read access and get its */
/*              logical unit. A text file is a formatted, */
/*              sequential file that contains only printable */
/*              characters:  ASCII 32-126. */

/*     BLINE, */
/*     ELINE    are line numbers in the text file. BLINE is */
/*              the line where the count will begin, and ELINE */
/*              is the line where the count will end. The */
/*              number of characters in the beginning and ending */
/*              lines are included in the total count. */

/*              By convention, line 1 is the first line of the file. */

/*     LINE     on input, is an arbitrary character string whose */
/*              contents are ignored. LINE is used to read lines */
/*              from the file connected to UNIT; its function */
/*              is to determine the maximum length of the lines */
/*              that can be read from the file. Lines longer */
/*              than the declared length of LINE are truncated */
/*              as they are read. */

/* $ Detailed_Output */

/*     LINE     on output, is undefined. */

/*     The function returns the number of characters in the group of */
/*     lines in the text file beginning with BLINE and ending with ELINE. */
/*     Trailing blanks on a line are not included in the count. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If an error occurs while reading from the input file, */
/*         the error SPICE(FILEREADFAILED) is signaled. */

/*     2)  If a non-printing ASCII character is encountered during */
/*         the count, the error SPICE(INVALIDTEXT) is signaled. */

/*     3)  If BLINE is greater than ELINE or if the file does not */
/*         contain both of this lines, the error SPICE(CANNOTFINDGRP) */
/*         is signaled. */

/* $ Files */

/*     See argument UNIT. COUNTC rewinds the text file connected to */
/*     UNIT and then steps through the file. The next read statement */
/*     after calling COUNTC would return the line after ELINE. */

/* $ Particulars */

/*     This routine counts characters in a group of lines in a text */
/*     file. Using COUNTC, you can determine in advance how much space */
/*     is required to store those characters. */

/* $ Examples */

/*     The following code fragment opens an existing text file for */
/*     read access and counts the characters that it contains in */
/*     the first five lines. We'll assume that the longest line */
/*     in the file is 80 characters. */

/*        INTEGER               COUNTC */
/*        INTEGER               UNIT */
/*        INTEGER               N */
/*        CHARACTER*(80)        LINE */

/*        CALL TXTOPR ( 'DATA.TXT', UNIT ) */

/*        N = COUNTC ( UNIT, 1, 5, LINE ) */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     J.E. McLean        (JPL) */
/*     H.A. Neilan        (JPL) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.2.0, 18-MAR-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 1.1.0, 17-MAY-1994 (HAN) */

/*        Set the default function value to either 0, 0.0D0, .FALSE., */
/*        or blank depending on the type of the function. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 05-APR-1991 (JEM) */

/* -& */
/* $ Index_Entries */

/*     count characters in a text file */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	ret_val = 0;
	return ret_val;
    } else {
	chkin_("COUNTC", (ftnlen)6);
	ret_val = 0;
    }

/*     First, see if the line numbers make sense. */

    if (*bline > *eline || *bline <= 0) {
	setmsg_("The line numbers do not make sense:  BLINE = # and  ELINE ="
		" #.", (ftnlen)62);
	errint_("#", bline, (ftnlen)1);
	errint_("#", eline, (ftnlen)1);
	sigerr_("SPICE(CANNOTFINDGRP)", (ftnlen)20);
	chkout_("COUNTC", (ftnlen)6);
	return ret_val;
    }

/*     Read through the file, line by line, beginning with the first */
/*     line in the file, checking for I/O errors, and counting */
/*     characters in the lines between and including BLINE and ELINE. */

    al__1.aerr = 0;
    al__1.aunit = *unit;
    f_rew(&al__1);
    linect = 0;
    chars = 0;
    done = FALSE_;
    while(! done) {
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
/*        error.  If IOSTAT is zero, the read was successful. */

	if (iostat > 0) {
	    setmsg_("Error reading text file named FILENAME.The value of IOS"
		    "TAT is #.", (ftnlen)64);
	    errint_("#", &iostat, (ftnlen)1);
	    errfnm_("FILENAME", unit, (ftnlen)8);
	    sigerr_("SPICE(FILEREADFAILED)", (ftnlen)21);
	    chkout_("COUNTC", (ftnlen)6);
	    return ret_val;
	} else if (iostat < 0) {
	    setmsg_("Reached end of file unexpectedly at line # in file FILE"
		    ".  BLINE = # and ELINE = #.", (ftnlen)82);
	    errint_("#", &linect, (ftnlen)1);
	    errint_("#", bline, (ftnlen)1);
	    errint_("#", eline, (ftnlen)1);
	    errfnm_("FILE", unit, (ftnlen)4);
	    sigerr_("SPICE(CANNOTFINDGRP)", (ftnlen)20);
	    chkout_("COUNTC", (ftnlen)6);
	    return ret_val;
	} else {

/*           We've read a line successfully, so add it to the line count. */
/*           If this line is in the group delimited by BLINE and ELINE, */
/*           count the characters in it, and if this line is ELINE, we're */
/*           done. */

	    ++linect;
	    if (linect >= *bline && linect <= *eline) {

/*              Add the number of characters in this line to the count. */
/*              If LINE is blank, LASTNB will return 0 which is just */
/*              what we want. */

		chars += lastnb_(line, line_len);

/*              Remove the printable characters from the line.  If */
/*              any characters remain, signal an error. */

		astrip_(line, " ", "~", line, line_len, (ftnlen)1, (ftnlen)1, 
			line_len);
		if (s_cmp(line, " ", line_len, (ftnlen)1) != 0) {
		    setmsg_("Non-printing ASCII characters were found when c"
			    "ounting characters on line number # in file FILE"
			    "NAME.", (ftnlen)100);
		    errint_("#", &linect, (ftnlen)1);
		    errfnm_("FILENAME", unit, (ftnlen)8);
		    sigerr_("SPICE(INVALIDTEXT)", (ftnlen)18);
		    chkout_("COUNTC", (ftnlen)6);
		    return ret_val;
		}
	    }
	    if (linect == *eline) {
		done = TRUE_;
	    }
	}
    }

/*     Assign the final character count. */

    ret_val = chars;
    chkout_("COUNTC", (ftnlen)6);
    return ret_val;
} /* countc_ */

