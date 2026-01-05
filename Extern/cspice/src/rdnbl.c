/* rdnbl.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure RDNBL ( Read non-blank line ) */
/* Subroutine */ int rdnbl_(char *file, char *line, logical *eof, ftnlen 
	file_len, ftnlen line_len)
{
    /* Builtin functions */
    integer s_cmp(char *, char *, ftnlen, ftnlen);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    extern logical failed_(void);
    extern /* Subroutine */ int chkout_(char *, ftnlen), rdtext_(char *, char 
	    *, logical *, ftnlen, ftnlen);
    extern logical return_(void);

/* $ Abstract */

/*     Read the next non-blank line of text from a text file. */

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
/*     FILE       I   Input text file. */
/*     LINE       O   Next non-blank line from the input text file. */
/*     EOF        O   End-of-file indicator. */

/* $ Detailed_Input */

/*     FILE     is the name of the text file from which the next */
/*              line is to be read. If the file is not currently */
/*              open, it is opened with a logical unit determined */
/*              at run time, and the first line of the file is */
/*              returned. Otherwise, the next line not yet read */
/*              from the file is read and returned. */

/* $ Detailed_Output */

/*     LINE     is next non-blank line of text in the specified file. */

/*     EOF      is .TRUE. when the end of the file is reached, and is */
/*              otherwise .FALSE. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If either the end of the file is reached or an error occurs */
/*         before a non-blank line is found, LINE is blank. */

/* $ Files */

/*     See input FILES. */

/* $ Particulars */

/*     RDNBL simply calls RDTEXT until one of two things happens: */

/*        1. A non-blank line is found (in which case the line */
/*           is returned). */

/*        2. The end of the file is reached (in which case the */
/*           file is closed, a blank line is returned, and the */
/*           end-of-file indicator becomes .TRUE.) */

/* $ Examples */

/*     Let FILE.1 contain the following lines. */

/*        Mary had a little lamb */

/*        Everywhere that Mary went */



/*        Its fleece was white as snow. */
/*        The lamb was sure to go. */

/*     Then the code fragment */

/*        DO I = 1, 4 */
/*           CALL RDNBL ( 'FILE.1', LINE, EOF ) */
/*           WRITE (*,*) LINE */
/*        END DO */

/*     produces the following output: */

/*        Mary had a little lamb */
/*        Everywhere that Mary went */
/*        Its fleece was white as snow. */
/*        The lamb was sure to go. */

/*     In fact, the following code fragment removes all of the blank */
/*     lines from an arbitrary text file (FILE). */

/*        CALL RDNBL ( FILE, LINE, EOF ) */

/*        DO WHILE ( .NOT. EOF ) */
/*           WRITE (*,*) LINE( : RTRIM(LINE) ) */

/*           CALL RDNBL ( FILE, LINE, EOF ) */
/*        END DO */

/*     Note that because RDNBL calls RDTEXT, calls to either routine */
/*     can be interspersed. For example, RDNBL can be used to skip */
/*     blank lines at the beginning of the file, leaving the rest to */
/*     be processed: */

/*        CALL RDNBL ( FILE, LINE, EOF ) */

/*        DO WHILE ( .NOT. EOF ) */
/*           < do something with LINE > */

/*           CALL RDTEXT ( FILE, LINE, EOF ) */
/*        END DO */

/* $ Restrictions */

/*     1)  Any restrictions that apply to RDTEXT apply to RDNBL as well. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     I.M. Underwood     (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 12-AUG-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 1.0.0, 07-AUG-1994 (IMU) */

/* -& */
/* $ Index_Entries */

/*     read a non-blank line from a text file */

/* -& */

/*     SPICELIB functions */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("RDNBL", (ftnlen)5);
    }

/*     Return as soon as a non-blank line is found. Otherwise, keep */
/*     looking until either the end of the file is reached or RDTEXT */
/*     manages to fail. */

    rdtext_(file, line, eof, file_len, line_len);
    while(! (*eof) && ! failed_()) {
	if (s_cmp(line, " ", line_len, (ftnlen)1) != 0) {
	    chkout_("RDNBL", (ftnlen)5);
	    return 0;
	} else {
	    rdtext_(file, line, eof, file_len, line_len);
	}
    }

/*     Didn't find anything? */

    s_copy(line, " ", line_len, (ftnlen)1);
    chkout_("RDNBL", (ftnlen)5);
    return 0;
} /* rdnbl_ */

