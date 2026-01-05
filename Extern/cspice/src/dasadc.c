/* dasadc.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;

/* $Procedure DASADC ( DAS, add data, character ) */
/* Subroutine */ int dasadc_(integer *handle, integer *n, integer *bpos, 
	integer *epos, char *data, ftnlen data_len)
{
    /* Initialized data */

    static char record[1024] = "                                            "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                    ";

    /* System generated locals */
    integer i__1, i__2, i__3;

    /* Builtin functions */
    integer i_len(char *, ftnlen);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    integer free;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    integer ncomc, lastc, recno, ncomr, nmove, rcpos;
    extern /* Subroutine */ int dasa2l_(integer *, integer *, integer *, 
	    integer *, integer *, integer *, integer *);
    extern logical failed_(void);
    integer clbase;
    extern /* Subroutine */ int dascud_(integer *, integer *, integer *), 
	    dashfs_(integer *, integer *, integer *, integer *, integer *, 
	    integer *, integer *, integer *, integer *);
    integer lastla[3];
    extern /* Subroutine */ int dasurc_(integer *, integer *, integer *, 
	    integer *, char *, ftnlen), daswrc_(integer *, integer *, char *, 
	    ftnlen);
    integer lastrc[3], clsize, nmoved;
    extern /* Subroutine */ int sigerr_(char *, ftnlen);
    integer numchr;
    extern /* Subroutine */ int chkout_(char *, ftnlen);
    integer lastwd[3], nresvc;
    extern /* Subroutine */ int setmsg_(char *, ftnlen), errint_(char *, 
	    integer *, ftnlen);
    integer wordno;
    extern logical return_(void);
    integer nresvr, nwritn, chr, elt;

/* $ Abstract */

/*     Add character data to a DAS file. */

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

/*     ARRAY */
/*     ASSIGNMENT */
/*     DAS */
/*     FILES */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     HANDLE     I   DAS file handle. */
/*     N          I   Number of characters to add to file. */
/*     BPOS, */
/*     EPOS       I   Begin and end positions of substrings. */
/*     DATA       I   Array providing the set of substrings to be added */
/*                    to the character data in the DAS file. */

/* $ Detailed_Input */

/*     HANDLE   is a file handle of a DAS file opened for writing. */

/*     N        is the total number of characters to add to the specified */
/*              DAS file. */

/*     BPOS, */
/*     EPOS     are the begin and end character positions that define the */
/*              substrings in each of the elements of the input array. */
/*              This routine writes the first N characters from the */
/*              specified set of substrings to the specified DAS file. */

/*     DATA     is an array of strings, some portion of whose contents */
/*              are to be added to the specified DAS file. Specifically, */
/*              the first N characters of the substrings */

/*                 DATA(I)(BPOS:EPOS),    I = 1, ... */

/*              are appended to the character data in the file. */

/*              DATA must be declared at least as */

/*                 CHARACTER*(EPOS)      DATA   ( R ) */

/*              with the dimension R being at least */

/*                 R = INT( ( N + SUBLEN - 1 ) / SUBLEN ) */

/*              and SUBLEN, the length of each of the substrings in */
/*              the array to be added to the DAS file, being */

/*                 SUBLEN  =  EPOS - BPOS + 1 */

/*              The order of characters in the input substrings is */
/*              considered to increase from left to right within each */
/*              element of DATA, and to increase with the indices of the */
/*              elements of DATA. */

/* $ Detailed_Output */

/*     None. */

/*     See $Particulars for a description of the effect of this routine. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the input file handle is invalid, an error is signaled */
/*         by a routine in the call tree of this routine. */

/*     2)  If EPOS or BPOS are outside of the range */

/*            [  1,  LEN( DATA(1) )  ] */

/*         or if EPOS < BPOS, the error SPICE(BADSUBSTRINGBOUNDS) is */
/*         signaled. */

/*     3)  If the input count N is less than 1, no data will be */
/*         added to the specified DAS file. */

/*     4)  If an I/O error occurs during the data addition attempted */
/*         by this routine, the error is signaled by a routine in the */
/*         call tree of this routine. */

/*     5)  If N is greater than the number of characters in the */
/*         specified set of input substrings, the results of calling */
/*         this routine are unpredictable. This routine cannot */
/*         detect this error. */

/* $ Files */

/*     See the description of the argument HANDLE in $Detailed_Input. */

/* $ Particulars */

/*     DAS is a low-level format meant to store and transmit data. As */
/*     such, character data in DAS files are not interpreted by SPICELIB */
/*     DAS input or output routines. There are no limits on which */
/*     character values may be placed in the virtual character array of a */
/*     DAS file. */

/*     This routine adds character data to a DAS file by "appending" them */
/*     after any character data already in the file. The sense in which */
/*     the data are "appended" is that the data will occupy a range of */
/*     logical addresses for character data that immediately follow the */
/*     last logical address of a character that is occupied at the time */
/*     this routine is called. The diagram below illustrates this */
/*     addition: */

/*        +-------------------------+ */
/*        |    (already in use)     |  Character logical address 1 */
/*        +-------------------------+ */
/*                    . */
/*                    . */
/*                    . */
/*        +-------------------------+  Last character logical address */
/*        |   (already in use)      |  in use before call to DASADC */
/*        +-------------------------+ */
/*        |  DATA(1)(BPOS:BPOS)     |  First added character */
/*        +-------------------------+ */
/*        |  DATA(1)(BPOS+1:BPOS+1) | */
/*        +-------------------------+ */
/*                     . */
/*                     . */
/*                     . */
/*        +-------------------------+ */
/*        |  DATA(1)(EPOS:EPOS)     | */
/*        +-------------------------+ */
/*        |  DATA(2)(BPOS:BPOS)     | */
/*        +-------------------------+ */
/*                     . */
/*                     . */
/*                     . */
/*        +-------------------------+ */
/*        |  DATA(R)(C:C)           |  N'th added character---here R is */
/*        +-------------------------+ */
/*                                        INT( (N+L-1)/L ) */

/*                                     where L = EPOS - BPOS + 1, and */
/*                                     C is */

/*                                        BPOS + ( N - (R-1)*L ) - 1 */


/*     The logical organization of the characters in the DAS file is */
/*     independent of the order of addition to the file or physical */
/*     location of any data of integer or double precision type. */

/*     The actual physical write operations that add the input array */
/*     DATA to the indicated DAS file may not take place before this */
/*     routine returns, since the DAS system buffers data that are */
/*     written as well as data that are read. In any case, the data */
/*     will be flushed to the file at the time the file is closed, if */
/*     not earlier. A physical write of all buffered records can be */
/*     forced by calling the SPICELIB routine DASWBR (DAS, write */
/*     buffered records). */

/*     In order to update character logical addresses that already */
/*     contain data, the SPICELIB routine DASUDC (DAS, update data, */
/*     character) should be used. */

/* $ Examples */

/*     The numerical results shown for this example may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */

/*     1) The following example demonstrates the capabilities of the */
/*        DAS character data routines. The reader should notice that */
/*        in these interfaces, the character data are treated not as */
/*        strings (or arrays of strings) but as a stream of single */
/*        characters: DAS character data are not limited to */
/*        human-readable text. For example, one can store images or */
/*        DEM data as DAS character data. */

/*        The example shows how to add a variable amount of character */
/*        data to a new DAS file, how to update some of the character */
/*        logical addresses within that file, and how to read that */
/*        data out to a different array. */


/*        Example code begins here. */


/*              PROGRAM DASADC_EX1 */
/*              IMPLICIT NONE */

/*        C */
/*        C     Local parameters. */
/*        C */
/*              CHARACTER*(*)         FNAME */
/*              PARAMETER           ( FNAME = 'dasadc_ex1.das' ) */

/*              CHARACTER*(*)         TYPE */
/*              PARAMETER           ( TYPE  = 'TEST'           ) */

/*        C */
/*        C     Local variables. */
/*        C */
/*              CHARACTER*(22)        CDATIN ( 3  ) */
/*              CHARACTER*(30)        CDATOU ( 10 ) */

/*              INTEGER               HANDLE */
/*              INTEGER               I */

/*              DATA CDATOU  / '..............................', */
/*             .               '..............................', */
/*             .               '..............................', */
/*             .               '..............................', */
/*             .               '..............................', */
/*             .               '..............................', */
/*             .               '..............................', */
/*             .               '..............................', */
/*             .               '         1         2         3', */
/*             .               '123456789012345678901234567890' / */

/*        C */
/*        C     Open a new DAS file. Use the file name as the internal */
/*        C     file name, and reserve no records for comments. */
/*        C */
/*              CALL DASONW ( FNAME, TYPE, FNAME, 0, HANDLE ) */

/*        C */
/*        C     Set the input data. Note that these data will be */
/*        C     considered as a binary data stream: DAS character data */
/*        C     are not limited to human-readable text. For example, */
/*        C     one can store images or DEM data as DAS character data. */
/*        C */
/*              CDATIN ( 1 ) = '--F-345678901234567890' */
/*              CDATIN ( 2 ) = '--S-345678901234567890' */
/*              CDATIN ( 3 ) = '--T-IRDxxxxxxxxxxxxxxx' */

/*        C */
/*        C     Add the last 20 characters of the first two elements */
/*        C     of CDATIN, and the 3rd character from the third one. */
/*        C */
/*              CALL DASADC ( HANDLE, 41, 3, 22, CDATIN ) */

/*        C */
/*        C     Update the 10th, 20th and 30th character in the DAS */
/*        C     file with a vertical bar. */
/*        C */
/*              DO I = 1, 3 */

/*                 CALL DASUDC ( HANDLE, I*10, I*10, 1, 1, '|' ) */

/*              END DO */

/*        C */
/*        C     Close the file. */
/*        C */
/*              CALL DASCLS ( HANDLE ) */

/*        C */
/*        C     Now verify the addition of data by opening the */
/*        C     file for read access and retrieving the data. */
/*        C */
/*              CALL DASOPR ( FNAME, HANDLE ) */

/*        C */
/*        C     Read the 41 characters that we stored on the DAS */
/*        C     file. Update the data on the CDATOU array, placing */
/*        C     6 characters on each element, starting from the */
/*        C     10th position. */
/*        C */
/*              CALL DASRDC ( HANDLE, 1, 41, 10, 15, CDATOU ) */

/*        C */
/*        C     Dump the data to the screen. Note that the last */
/*        C     three lines should remain unmodified, and that */
/*        C     only 5 characters will be written on the 7th line. */
/*        C */
/*              WRITE (*,*) */
/*              WRITE (*,*) 'Data from "', FNAME, '":' */
/*              WRITE (*,*) */

/*              DO I = 1, 10 */
/*                 WRITE (*,*) CDATOU(I) */
/*              END DO */

/*        C */
/*        C     Close the file. */
/*        C */
/*              CALL DASCLS ( HANDLE ) */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*         Data from "dasadc_ex1.das": */

/*         .........F-3456............... */
/*         .........789|12............... */
/*         .........345678............... */
/*         .........9|S-34............... */
/*         .........56789|............... */
/*         .........123456............... */
/*         .........7890T................ */
/*         .............................. */
/*                  1         2         3 */
/*         123456789012345678901234567890 */


/*        Note that after run completion, a new DAS file exists in the */
/*        output directory. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     K.R. Gehringer     (JPL) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.3.0, 08-OCT-2021 (JDR) (NJB) */

/*        Added IMPLICIT NONE statement. Updated the code to avoid that */
/*        DASCUD is called with a negative number of character words */
/*        when the input count N is negative. */

/*        Made local variable RECORD a saved variable which is */
/*        initialized by a DATA statement. */

/*        Bug fix: added FAILED call after DASHFS call. */

/*        Edited the header to comply with NAIF standard. */

/*        Replaced example code with one that demonstrates the usage and */
/*        effect of all DAS character data routines. */

/*        Updated entries in the $Revisions section. */

/* -    SPICELIB Version 1.2.0, 10-APR-2014 (NJB) */

/*        Deleted declarations of unused parameters. */

/*        Corrected header comments: routine that flushes */
/*        written, buffered records is DASWBR, not DASWUR. */

/* -    SPICELIB Version 1.1.1, 19-DEC-1995 (NJB) */

/*        Corrected title of permuted index entry section. */

/* -    SPICELIB Version 1.1.0, 12-MAY-1994 (KRG) (NJB) */

/*        Test of FAILED() added to loop termination condition. */

/*        Removed references to specific DAS file open routines in the */
/*        $Detailed_Input section of the header. This was done in order */
/*        to minimize documentation changes if the DAS open routines ever */
/*        change. */

/*        Modified the $Examples section to demonstrate the new ID word */
/*        format which includes a file type and to include a call to the */
/*        new routine DASONW, open new, which makes use of the file */
/*        type. Also, a variable for the type of the file to be created */
/*        was added. */

/* -    SPICELIB Version 1.0.0, 11-NOV-1992 (NJB) (WLT) */

/* -& */
/* $ Index_Entries */

/*     add character data to a DAS file */

/* -& */
/* $ Revisions */

/* -    SPICELIB Version 1.1.0, 12-MAY-1994 (KRG) (NJB) */

/*        Test of FAILED() added to loop termination condition. Without */
/*        this test, an infinite loop could result if DASA2L, DASURC or */
/*        DASWRC signaled an error inside the loop. */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     Local variables */


/*     Saved variables */


/*     Initial values */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }
    chkin_("DASADC", (ftnlen)6);

/*     Make sure BPOS and EPOS are OK; stop here if not. */

    if (*bpos < 1 || *epos < 1 || *bpos > i_len(data, data_len) || *epos > 
	    i_len(data, data_len)) {
	setmsg_("Substring bounds must be in range [1,#]. Actual range [BPOS"
		",EPOS] was [#,#].", (ftnlen)76);
	i__1 = i_len(data, data_len);
	errint_("#", &i__1, (ftnlen)1);
	errint_("#", bpos, (ftnlen)1);
	errint_("#", epos, (ftnlen)1);
	sigerr_("SPICE(BADSUBSTRINGBOUNDS)", (ftnlen)25);
	chkout_("DASADC", (ftnlen)6);
	return 0;
    } else if (*epos < *bpos) {
	setmsg_("Substring upper bound must not be less than lower bound.  A"
		"ctual range [BPOS,EPOS] was [#,#].", (ftnlen)93);
	errint_("#", bpos, (ftnlen)1);
	errint_("#", epos, (ftnlen)1);
	sigerr_("SPICE(BADSUBSTRINGBOUNDS)", (ftnlen)25);
	chkout_("DASADC", (ftnlen)6);
	return 0;
    }

/*     Get the file summary for this DAS. */

    dashfs_(handle, &nresvr, &nresvc, &ncomr, &ncomc, &free, lastla, lastrc, 
	    lastwd);
    if (failed_()) {
	chkout_("DASADC", (ftnlen)6);
	return 0;
    }
    lastc = lastla[0];

/*     We will keep track of the location that we wish to write to */
/*     with the variables RECNO and WORDNO.  RECNO will be the record */
/*     number of the record we'll write to; WORDNO will be the number */
/*     preceding the word index, within record number RECNO, that we'll */
/*     write to.  For example, if we're about to write to the first */
/*     character in record 10, RECNO will be 10 and WORDNO will be 0.  Of */
/*     course, when WORDNO reaches NWC, we'll have to find a free record */
/*     before writing anything. */

/*     Prepare the variables RECNO and WORDNO:  use the physical location */
/*     of the last character address, if there are any character data in */
/*     the file.  Otherwise, RECNO becomes the first record available for */
/*     character data. */

    if (lastc >= 1) {
	dasa2l_(handle, &c__1, &lastc, &clbase, &clsize, &recno, &wordno);
    } else {
	recno = free;
	wordno = 0;
    }

/*     Set the number of character words already written.  Keep */
/*     writing to the file until this number equals the number of */
/*     elements in DATA. */

/*     Note that if N is non-positive, the loop doesn't get */
/*     exercised. */

/*     Also initialize the array element index and position of the */
/*     character to be moved next. */

    nwritn = 0;
    elt = 1;
    chr = *bpos;
    while(nwritn < *n && ! failed_()) {

/*        Write as much data as we can (or need to) into the current */
/*        record.  We assume that RECNO, WORDNO, and NWRITN have */
/*        been set correctly at this point. */

/*        Find out how many words to write into the current record. */
/*        There may be no space left in the current record. */

/* Computing MIN */
	i__1 = *n - nwritn, i__2 = 1024 - wordno;
	numchr = min(i__1,i__2);
	if (numchr > 0) {

/*           Write NUMCHR words into the current record.  If the record */
/*           is new, write the entire record.  Otherwise, just update */
/*           the part we're interested in. */

/*           In either case, we'll first fill in characters WORDNO+1 */
/*           through WORDNO + NUMCHR of the string RECORD. */


/*           So far, we haven't moved any characters. */

	    nmoved = 0;
	    rcpos = wordno;
	    while(nmoved < numchr) {

/*              Find out how many characters in the current array */
/*              element we should move. */

		if (chr > *epos) {
		    ++elt;
		    chr = *bpos;
		}
/* Computing MIN */
		i__1 = numchr - nmoved, i__2 = *epos - chr + 1;
		nmove = min(i__1,i__2);
		i__1 = rcpos;
		s_copy(record + i__1, data + ((elt - 1) * data_len + (chr - 1)
			), rcpos + nmove - i__1, data_len - (chr - 1));
		nmoved += nmove;
		rcpos += nmove;
		chr += nmove;
	    }

/*           Now we can write or update the file with RECORD. */

	    if (wordno == 0) {

/*              The record has not yet been written, so write out the */
/*              entire record. */

		daswrc_(handle, &recno, record, (ftnlen)1024);
	    } else {

/*              Update elements WORDNO+1 through WORDNO+NUMCHR. */

		i__1 = wordno;
		i__2 = wordno + 1;
		i__3 = wordno + numchr;
		dasurc_(handle, &recno, &i__2, &i__3, record + i__1, wordno + 
			numchr - i__1);
	    }
	    nwritn += numchr;
	    wordno += numchr;
	} else {

/*           It's time to start on a new record.  If the record we */
/*           just finished writing to (or just attempted writing to, */
/*           if it was full) was FREE or a higher-numbered record, */
/*           then we are writing to a contiguous set of data records: */
/*           the next record to write to is the immediate successor */
/*           of the last one.  Otherwise, FREE is the next record */
/*           to write to. */

/*           We intentionally leave FREE at the value it had before */
/*           we starting adding data to the file. */

	    if (recno >= free) {
		++recno;
	    } else {
		recno = free;
	    }
	    wordno = 0;
	}
    }

/*     Update the DAS file directories to reflect the addition of NWRITN */
/*     character words.  DASCUD will also update the file summary */
/*     accordingly. */

    dascud_(handle, &c__1, &nwritn);
    chkout_("DASADC", (ftnlen)6);
    return 0;
} /* dasadc_ */

