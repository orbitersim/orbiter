/* dasudc.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;

/* $Procedure DASUDC ( DAS, update data, character ) */
/* Subroutine */ int dasudc_(integer *handle, integer *first, integer *last, 
	integer *bpos, integer *epos, char *data, ftnlen data_len)
{
    /* System generated locals */
    integer i__1, i__2;

    /* Builtin functions */
    integer i_len(char *, ftnlen);

    /* Local variables */
    integer l, n;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    integer lastc, lastd, recno, lasti, nmove, rcpos;
    extern /* Subroutine */ int dasa2l_(integer *, integer *, integer *, 
	    integer *, integer *, integer *, integer *);
    extern logical failed_(void);
    integer clbase;
    extern /* Subroutine */ int daslla_(integer *, integer *, integer *, 
	    integer *), dasurc_(integer *, integer *, integer *, integer *, 
	    char *, ftnlen);
    integer nmoved, clsize;
    extern /* Subroutine */ int sigerr_(char *, ftnlen);
    integer numchr;
    extern /* Subroutine */ int chkout_(char *, ftnlen), setmsg_(char *, 
	    ftnlen), errint_(char *, integer *, ftnlen);
    integer wordno;
    extern logical return_(void);
    integer nwritn, chr, elt;

/* $ Abstract */

/*     Update character data in a specified range of DAS logical */
/*     addresses with substrings of a character array. */

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
/*     DATA       I   Data having addresses FIRST through LAST. */

/* $ Detailed_Input */

/*     HANDLE   is a file handle of a DAS file opened for writing. */

/*     FIRST, */
/*     LAST     are the first and last of a range of DAS logical */
/*              addresses of characters. These addresses satisfy */
/*              the inequality */

/*                 1  <=   FIRST   <=   LAST   <=   LASTC */

/*              where LASTC is the last character logical address */
/*              in use in the DAS file designated by HANDLE. */

/*     BPOS, */
/*     EPOS     are the begin and end character positions that define the */
/*              substrings in each of the elements of the input array */
/*              that are to replace the data in the range of DAS */
/*              character addresses given by FIRST and LAST. */

/*     DATA     is an array of strings. The contents of the specified */
/*              substrings of the elements of the array DATA will be */
/*              written to the indicated DAS file in order: */
/*              DATA(1)(BPOS:BPOS) will be written to character logical */
/*              address FIRST; DATA(1)(BPOS+1:BPOS+1) will be written to */
/*              the character logical address FIRST+1, and so on; in this */
/*              ordering scheme, character (BPOS:BPOS) of DATA(I+1) is */
/*              the successor of character (EPOS:EPOS) of DATA(I). */

/*              DATA must be declared at least as */

/*                 CHARACTER*(EPOS)      DATA   ( R ) */

/*              with the dimension R being at least */

/*                 R = INT( ( LAST - FIRST + SUBLEN ) / SUBLEN ) */

/*              and SUBLEN, the length of each of the substrings in */
/*              the array to be written to the DAS file, being */

/*                 SUBLEN  =  EPOS - BPOS + 1 */

/* $ Detailed_Output */

/*     None. */

/*     See $Particulars for a description of the effect of this routine. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the input file handle is invalid, an error is signaled by */
/*         a routine in the call tree of this routine. */

/*     2)  Only logical addresses that already contain data may be */
/*         updated: if either FIRST or LAST are outside the range */

/*            [ 1,  LASTC ] */

/*         where LASTC is the last character logical address that */
/*         currently contains data in the indicated DAS file, the error */
/*         SPICE(INVALIDADDRESS) is signaled. The DAS file will not be */
/*         modified. */

/*     3)  If EPOS or BPOS are outside of the range */

/*            [  1,  LEN( DATA(1) )  ] */

/*         the error SPICE(INVALIDINDEX) is signaled. */

/*     4)  If BPOS is greater than EPOS, the error */
/*         SPICE(INDICESOUTOFORDER) is signaled. */

/*     5)  If FIRST > LAST but both addresses are valid, this routine */
/*         will not modify the indicated DAS file. No error will be */
/*         signaled. */

/*     6)  If an I/O error occurs during the data update attempted */
/*         by this routine, the error is signaled by a routine in the */
/*         call tree of this routine. FIRST and LAST will not be */
/*         modified. */

/* $ Files */

/*     See the description of the argument HANDLE in $Detailed_Input. */

/* $ Particulars */

/*     DAS is a low-level format meant to store and transmit data. As */
/*     such, character data in DAS files are not interpreted by SPICELIB */
/*     DAS input or output routines. There are no limits on which */
/*     character values may be placed in the virtual character array of a */
/*     DAS file. */

/*     This routine replaces the character data in the specified range */
/*     of logical addresses within a DAS file with the contents of the */
/*     specified substrings of the input array DATA. */

/*     The actual physical write operations that update the indicated */
/*     DAS file with the contents of the input array DATA may not take */
/*     place before this routine returns, since the DAS system buffers */
/*     data that are written as well as data that are read. In any case, */
/*     the data will be flushed to the file at the time the file is */
/*     closed, if not earlier. A physical write of all buffered */
/*     records can be forced by calling the SPICELIB routine DASWBR */
/*     (DAS, write buffered records). */

/*     In order to append character data to a DAS file, filling in a */
/*     range of character logical addresses that starts immediately */
/*     after the last character logical address currently in use, the */
/*     SPICELIB routine DASADC (DAS add data, character) should be */
/*     used. */

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


/*              PROGRAM DASUDC_EX1 */
/*              IMPLICIT NONE */

/*        C */
/*        C     Local parameters. */
/*        C */
/*              CHARACTER*(*)         FNAME */
/*              PARAMETER           ( FNAME = 'dasudc_ex1.das' ) */

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


/*         Data from "dasudc_ex1.das": */

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

/* -    SPICELIB Version 2.0.0, 19-MAY-2021 (NJB) (JDR) */

/*        Added error checks for invalid begin and end indices BPOS */
/*        and EPOS. */

/*        Added IMPLICIT NONE statement. */

/*        Updated entries in $Exceptions and $Revisions sections and */
/*        removed reference to nonexistent API from $Particulars. */

/*        Edited the header to comply with NAIF standard. */

/*        Replaced example code with one that demonstrates the usage and */
/*        effect of all DAS character data routines. */

/*        Updated entries in $Revisions section. */

/* -    SPICELIB Version 1.3.0, 10-APR-2014 (NJB) */

/*        Deleted declarations of unused parameters. */

/*        Corrected header comments: routine that flushes */
/*        written, buffered records is DASWBR, not DASWUR. */

/* -    SPICELIB Version 1.2.1, 19-DEC-1995 (NJB) */

/*        Corrected title of permuted index entry section. */

/* -    SPICELIB Version 1.2.0, 12-MAY-1995 (NJB) */

/*        Bug fix: routine handled values of BPOS incorrectly when */
/*        BPOS > 1. */

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

/*     update a range of DAS logical addresses using substrings */
/*     write substrings to a range of DAS logical addresses */

/* -& */
/* $ Revisions */

/* -    SPICELIB Version 1.2.0, 12-MAY-1995 (NJB) */

/*        Bug fix: routine handled values of BPOS incorrectly when */
/*        BPOS > 1. This was due to the incorrect initialization */
/*        of the internal variables CHR and ELT. The initialization */
/*        was corrected. */

/* -    SPICELIB Version 1.1.0, 12-MAY-1994 (KRG) (NJB) */

/*        Tests of FAILED() added to loop termination conditions. */
/*        Without these tests, infinite loops could result if DASA2L or */
/*        DASURC signaled an error inside the loops. */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     Local variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }
    chkin_("DASUDC", (ftnlen)6);

/*     Get the last logical addresses in use in this DAS file. */

    daslla_(handle, &lastc, &lastd, &lasti);

/*     Validate the input addresses. */

    if (*first < 1 || *first > lastc || *last < 1 || *last > lastc) {
	setmsg_("FIRST was #. LAST was #. Valid range is [1,#].", (ftnlen)46);
	errint_("#", first, (ftnlen)1);
	errint_("#", last, (ftnlen)1);
	errint_("#", &lastc, (ftnlen)1);
	sigerr_("SPICE(INVALIDADDRESS)", (ftnlen)21);
	chkout_("DASUDC", (ftnlen)6);
	return 0;
    }

/*     Make sure BPOS and EPOS are valid and compatible with the string */
/*     length of the input array. */

    if (*bpos < 1 || *bpos > i_len(data, data_len)) {
	setmsg_("String begin index must be in the range #:# but was #.", (
		ftnlen)54);
	errint_("#", &c__1, (ftnlen)1);
	i__1 = i_len(data, data_len);
	errint_("#", &i__1, (ftnlen)1);
	errint_("#", bpos, (ftnlen)1);
	sigerr_("SPICE(INVALIDINDEX)", (ftnlen)19);
	chkout_("DASUDC", (ftnlen)6);
	return 0;
    }
    if (*epos < 1 || *epos > i_len(data, data_len)) {
	setmsg_("String end index must be in the range #:# but was #.", (
		ftnlen)52);
	errint_("#", &c__1, (ftnlen)1);
	i__1 = i_len(data, data_len);
	errint_("#", &i__1, (ftnlen)1);
	errint_("#", epos, (ftnlen)1);
	sigerr_("SPICE(INVALIDINDEX)", (ftnlen)19);
	chkout_("DASUDC", (ftnlen)6);
	return 0;
    }
    if (*bpos > *epos) {
	setmsg_("String begin index # must be less than or equal to the end "
		"index #.", (ftnlen)67);
	errint_("#", bpos, (ftnlen)1);
	errint_("#", epos, (ftnlen)1);
	sigerr_("SPICE(INDICESOUTOFORDER)", (ftnlen)24);
	chkout_("DASUDC", (ftnlen)6);
	return 0;
    }

/*     Get the length of the substrings of DATA.  Count the total number */
/*     of characters to write. */

    l = *epos - *bpos + 1;
    n = *last - *first + 1;
    nwritn = 0;

/*     Find out the physical location of the first character to update. */

    dasa2l_(handle, &c__1, first, &clbase, &clsize, &recno, &wordno);

/*     Write as much data into record RECNO as is necessary and possible. */

/*     NUMCHR is the number of characters to write to the current record. */

/*     ELT is the index of the element of the input array that we're */
/*     taking data from.  CHR is the position in that array element of */
/*     the next character to move to the file. */

/*     NMOVED is the number of characters we've moved into the current */
/*     record so far. */

/*     RCPOS is the character position we'll write to next in the current */
/*     record. */

/* Computing MIN */
    i__1 = n, i__2 = 1024 - wordno + 1;
    numchr = min(i__1,i__2);
    elt = 1;
    chr = *bpos;
    nmoved = 0;
    rcpos = wordno;
    while(nmoved < numchr && ! failed_()) {
	if (chr > *epos) {
	    ++elt;
	    chr = *bpos;
	}

/*        Find out how many characters to move from the current array */
/*        element to the current record. */

/* Computing MIN */
	i__1 = numchr - nmoved, i__2 = *epos - chr + 1;
	nmove = min(i__1,i__2);

/*        Update the current record. */

	i__1 = rcpos + nmove - 1;
	dasurc_(handle, &recno, &rcpos, &i__1, data + ((elt - 1) * data_len + 
		(chr - 1)), chr + nmove - 1 - (chr - 1));
	nmoved += nmove;
	rcpos += nmove;
	chr += nmove;
    }
    nwritn = numchr;
    ++recno;

/*     Update as many additional records as necessary. */

    while(nwritn < n && ! failed_()) {

/*        At this point, RECNO is the correct number of the record to */
/*        write to next.  CLBASE is the number of the first record of */
/*        the cluster we're about to write to. */

	if (recno < clbase + clsize) {

/*           We can continue writing the current cluster.  Find */
/*           out how many elements to write to the current record, */
/*           and write them. */

/* Computing MIN */
	    i__1 = n - nwritn;
	    numchr = min(i__1,1024);
	    nmoved = 0;
	    rcpos = 1;
	    while(nmoved < numchr && ! failed_()) {
		if (chr > l) {
		    ++elt;
		    chr = *bpos;
		}

/*              Find out how many characters to move from the array */
/*              element to the current record. */

/* Computing MIN */
		i__1 = numchr - nmoved, i__2 = *epos - chr + 1;
		nmove = min(i__1,i__2);
		i__1 = rcpos + nmove - 1;
		dasurc_(handle, &recno, &rcpos, &i__1, data + ((elt - 1) * 
			data_len + (chr - 1)), chr + nmove - 1 - (chr - 1));
		nmoved += nmove;
		rcpos += nmove;
		chr += nmove;
	    }
	    nwritn += numchr;
	    ++recno;
	} else {

/*           We must find the next character cluster to write to. */
/*           The first character in this cluster has address FIRST + */
/*           NWRITN. */

	    i__1 = *first + nwritn;
	    dasa2l_(handle, &c__1, &i__1, &clbase, &clsize, &recno, &wordno);
	}
    }
    chkout_("DASUDC", (ftnlen)6);
    return 0;
} /* dasudc_ */

