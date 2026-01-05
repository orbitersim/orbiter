/* dasrdc.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;

/* $Procedure DASRDC ( DAS, read data, character ) */
/* Subroutine */ int dasrdc_(integer *handle, integer *first, integer *last, 
	integer *bpos, integer *epos, char *data, ftnlen data_len)
{
    /* System generated locals */
    integer i__1, i__2;

    /* Builtin functions */
    integer i_len(char *, ftnlen);

    /* Local variables */
    integer l, n, nread;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    integer recno, nmove, rcpos;
    extern /* Subroutine */ int dasa2l_(integer *, integer *, integer *, 
	    integer *, integer *, integer *, integer *);
    extern logical failed_(void);
    integer clbase;
    extern /* Subroutine */ int dasrrc_(integer *, integer *, integer *, 
	    integer *, char *, ftnlen);
    integer nmoved, clsize;
    extern /* Subroutine */ int sigerr_(char *, ftnlen);
    integer numchr;
    extern /* Subroutine */ int chkout_(char *, ftnlen), setmsg_(char *, 
	    ftnlen), errint_(char *, integer *, ftnlen);
    integer wordno, chr, elt;

/* $ Abstract */

/*     Read character data from a range of DAS logical addresses. */

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
/*     FIRST, */
/*     LAST       I   Range of DAS character logical addresses. */
/*     BPOS, */
/*     EPOS       I   Begin and end positions of substrings. */
/*     DATA       O   Data having addresses FIRST through LAST. */

/* $ Detailed_Input */

/*     HANDLE   is a file handle for an open DAS file. */

/*     FIRST, */
/*     LAST     are a range of DAS character logical addresses. */
/*              FIRST and LAST must be greater than or equal to */
/*              1 and less than or equal to the highest character */
/*              logical address in the DAS file designated by */
/*              HANDLE. */

/*     BPOS, */
/*     EPOS     are the begin and end character positions that define the */
/*              substrings in each of the elements of the output array */
/*              DATA into which character data is to be read. */

/* $ Detailed_Output */

/*     DATA     is an array of strings. On output, the character words in */
/*              the logical address range FIRST through LAST are copied */
/*              into the characters */

/*                 DATA(1)(BPOS:BPOS), */
/*                 DATA(1)(BPOS+1:BPOS+1), */
/*                             . */
/*                             . */
/*                             . */
/*                 DATA(1)(EPOS:EPOS), */
/*                 DATA(2)(BPOS:BPOS), */
/*                 DATA(2)(BPOS+1:BPOS+1), */
/*                             . */
/*                             . */
/*                             . */
/*                 DATA(R)(BPOS:BPOS) */
/*                 DATA(R)(BPOS+1:BPOS+1) */
/*                             . */
/*                             . */
/*                             . */

/*              in that order. Note that the character positions of DATA */
/*              **other** than the ones shown in the diagram remain */
/*              unmodified. */

/*              DATA must be declared at least as */

/*                 CHARACTER*(EPOS)        DATA   ( R ) */

/*              with the dimension R being at least */

/*                 R = INT( ( LAST - FIRST + SUBLEN ) / SUBLEN ) */

/*              and SUBLEN, the length of each of the substrings read */
/*              into the array elements from the DAS file, being */

/*                 SUBLEN  =  EPOS - BPOS + 1 */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the input file handle is invalid, an error is signaled */
/*         by a routine in the call tree of this routine. DATA will */
/*         not be modified. */

/*     2)  If EPOS or BPOS are outside of the range */

/*            [  1,  LEN( DATA(1) )  ] */

/*         or if EPOS < BPOS, the error SPICE(BADSUBSTRINGBOUNDS) is */
/*         signaled. */

/*     3)  If FIRST or LAST are out of range, an error is signaled by a */
/*         routine in the call tree of this routine. DATA will not be */
/*         modified. */

/*     4)  If FIRST is greater than LAST, DATA is left unchanged. */

/*     5)  If DATA is declared with length less than */

/*            ( LAST - FIRST + ( EPOS-BPOS+1 )  ) / ( EPOS-BPOS+1 ) */

/*         the error cannot be diagnosed by this routine. */

/* $ Files */

/*     See the description of the argument HANDLE in $Detailed_Input. */

/* $ Particulars */

/*     DAS is a low-level format meant to store and transmit data. As */
/*     such, character data in DAS files are not interpreted by SPICELIB */
/*     DAS input or output routines. There are no limits on which */
/*     character values may be placed in the virtual character array of a */
/*     DAS file. */

/*     This routine provides random read access to the character data in */
/*     a DAS file. These data are logically structured as a */
/*     one-dimensional array of characters. */

/*     However, since Fortran programs usually use strings rather than */
/*     arrays of individual characters, the interface of this routine */
/*     provides for extraction of data from a DAS file into an array of */
/*     strings. */

/*     DASRDC allows the caller to control the amount of character data */
/*     read into each array element. This feature allows a program to */
/*     read character data into an array that has a different string */
/*     length from the one used to write the character data, without */
/*     losing the correspondence between input and output array elements. */
/*     For example, an array of strings of 32 characters can be written */
/*     to a DAS file and read back by DASRDC into a buffer of strings */
/*     having length 80 characters, mapping each 32-character string to */
/*     characters 1--32 of the output buffer. */

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


/*              PROGRAM DASRDC_EX1 */
/*              IMPLICIT NONE */

/*        C */
/*        C     Local parameters. */
/*        C */
/*              CHARACTER*(*)         FNAME */
/*              PARAMETER           ( FNAME = 'dasrdc_ex1.das' ) */

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


/*         Data from "dasrdc_ex1.das": */

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

/* -    SPICELIB Version 1.3.0, 09-OCT-2021 (JDR) (NJB) */

/*        Added IMPLICIT NONE statement. */

/*        Added FAILED call following DASA2L call. */

/*        Updated entries in $Revisions section. */

/*        Edited the header to comply with NAIF standard. */

/*        Replaced example code with one that demonstrates the usage and */
/*        effect of all DAS character data routines. */

/* -    SPICELIB Version 1.2.2, 03-JUL-1996 (NJB) */

/*        Various errors in the header comments were fixed. */

/* -    SPICELIB Version 1.2.1, 19-DEC-1995 (NJB) */

/*        Corrected title of permuted index entry section. */

/* -    SPICELIB Version 1.2.0, 03-NOV-1995 (NJB) */

/*        Routine now uses discovery check-in. FAILED test moved inside */
/*        loops. */

/* -    SPICELIB Version 1.2.0, 14-SEP-1995 (NJB) */

/*        Bug fix: reference to DASADS in CHKOUT calls corrected. */

/* -    SPICELIB Version 1.1.0, 12-MAY-1994 (KRG) (NJB) */

/*        Test of FAILED() added to loop termination conditions. */

/*        Removed references to specific DAS file open routines in the */
/*        $Detailed_Input section of the header. This was done in order */
/*        to minimize documentation changes if the DAS open routines ever */
/*        change. */

/*        Modified the $Examples section to demonstrate the new ID word */
/*        format which includes a file type and to include a call to the */
/*        new routine DASONW, open new for write, which makes use of the */
/*        file type. Also,  a variable for the type of the file to be */
/*        created was added. */

/* -    SPICELIB Version 1.0.0, 12-NOV-1992 (NJB) (WLT) */

/* -& */
/* $ Index_Entries */

/*     read character data from a DAS file */

/* -& */
/* $ Revisions */

/* -    SPICELIB Version 1.2.0, 03-NOV-1995 (NJB) */

/*        Routine now uses discovery check-in. FAILED test moved inside */
/*        loops. */

/* -    SPICELIB Version 1.2.0, 03-NOV-1995 (NJB) */

/*        Bug fix: reference to DASADS in CHKOUT calls corrected. */
/*        These references have been changed to 'DASRDC'. */


/* -    SPICELIB Version 1.1.0, 12-MAY-1994 (KRG) (NJB) */

/*        Test of FAILED() added to loop termination conditions. Without */
/*        this test, an infinite loop could result if DASA2L or DASRRC */
/*        signaled an error inside the loops. */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     Local variables */


/*     Make sure BPOS and EPOS are ok; stop here if not. */

    if (*bpos < 1 || *epos < 1 || *bpos > i_len(data, data_len) || *epos > 
	    i_len(data, data_len)) {
	chkin_("DASRDC", (ftnlen)6);
	setmsg_("Substring bounds must be in range [1,#]. Actual range [BPOS"
		",EPOS] was [#,#].", (ftnlen)76);
	i__1 = i_len(data, data_len);
	errint_("#", &i__1, (ftnlen)1);
	errint_("#", bpos, (ftnlen)1);
	errint_("#", epos, (ftnlen)1);
	sigerr_("SPICE(BADSUBSTRINGBOUNDS)", (ftnlen)25);
	chkout_("DASRDC", (ftnlen)6);
	return 0;
    } else if (*epos < *bpos) {
	chkin_("DASRDC", (ftnlen)6);
	setmsg_("Substring upper bound must not be less than lower bound.  A"
		"ctual range [BPOS,EPOS] was [#,#].", (ftnlen)93);
	errint_("#", bpos, (ftnlen)1);
	errint_("#", epos, (ftnlen)1);
	sigerr_("SPICE(BADSUBSTRINGBOUNDS)", (ftnlen)25);
	chkout_("DASRDC", (ftnlen)6);
	return 0;
    }

/*     Find out the physical location of the first character to read.  If */
/*     FIRST is out of range, DASA2L will cause an error to be signaled. */

    dasa2l_(handle, &c__1, first, &clbase, &clsize, &recno, &wordno);
    if (failed_()) {
	return 0;
    }

/*     Get the length of the elements of DATA.  Count the total number */
/*     of characters to read. */

    l = *epos - *bpos + 1;
    n = *last - *first + 1;
    nread = 0;

/*     Read as much data from record RECNO as is necessary and possible. */

/* Computing MIN */
    i__1 = n, i__2 = 1024 - wordno + 1;
    numchr = min(i__1,i__2);
    elt = 1;
    chr = *bpos;
    nmoved = 0;
    rcpos = wordno;
    while(nmoved < numchr) {
	if (failed_()) {
	    return 0;
	}
	if (chr > *epos) {
	    ++elt;
	    chr = *bpos;
	}

/*        Find out how many characters to move from the current record */
/*        to the current array element. */

/* Computing MIN */
	i__1 = numchr - nmoved, i__2 = *epos - chr + 1;
	nmove = min(i__1,i__2);
	i__1 = rcpos + nmove - 1;
	dasrrc_(handle, &recno, &rcpos, &i__1, data + ((elt - 1) * data_len + 
		(chr - 1)), chr + nmove - 1 - (chr - 1));
	nmoved += nmove;
	rcpos += nmove;
	chr += nmove;
    }
    nread = numchr;
    ++recno;

/*     Read from as many additional records as necessary. */

    while(nread < n) {
	if (failed_()) {
	    return 0;
	}

/*        At this point, RECNO is the correct number of the */
/*        record to read from next.  CLBASE is the number */
/*        of the first record of the cluster we're about */
/*        to read from. */


	if (recno < clbase + clsize) {

/*           We can continue reading from the current cluster.  Find */
/*           out how many elements to read from the current record, */
/*           and read them. */

/* Computing MIN */
	    i__1 = n - nread;
	    numchr = min(i__1,1024);
	    nmoved = 0;
	    rcpos = 1;
	    while(nmoved < numchr && ! failed_()) {
		if (chr > *epos) {
		    ++elt;
		    chr = *bpos;
		}

/*              Find out how many characters to move from the current */
/*              record to the current array element. */

/* Computing MIN */
		i__1 = numchr - nmoved, i__2 = *epos - chr + 1;
		nmove = min(i__1,i__2);
		i__1 = rcpos + nmove - 1;
		dasrrc_(handle, &recno, &rcpos, &i__1, data + ((elt - 1) * 
			data_len + (chr - 1)), chr + nmove - 1 - (chr - 1));
		nmoved += nmove;
		rcpos += nmove;
		chr += nmove;
	    }
	    nread += numchr;
	    ++recno;
	} else {

/*           We must find the next character cluster to */
/*           read from.  The first character in this */
/*           cluster has address FIRST + NREAD. */

	    i__1 = *first + nread;
	    dasa2l_(handle, &c__1, &i__1, &clbase, &clsize, &recno, &wordno);
	}
    }
    return 0;
} /* dasrdc_ */

