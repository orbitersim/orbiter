/* rdenci.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__9 = 9;
static integer c__1 = 1;

/* $Procedure RDENCI  ( Read encoded integers from text file ) */
/* Subroutine */ int rdenci_(integer *unit, integer *n, integer *data)
{
    /* System generated locals */
    integer i__1, i__2;

    /* Builtin functions */
    integer s_rsle(cilist *), s_rnge(char *, integer, char *, integer), 
	    do_lio(integer *, integer *, char *, ftnlen), e_rsle(void);

    /* Local variables */
    char work[64*64];
    integer i__;
    extern /* Subroutine */ int chkin_(char *, ftnlen), errch_(char *, char *,
	     ftnlen, ftnlen);
    logical error;
    integer nitms;
    extern /* Subroutine */ int hx2int_(char *, integer *, logical *, char *, 
	    ftnlen, ftnlen);
    integer itmbeg;
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen);
    char errmsg[80];
    extern /* Subroutine */ int setmsg_(char *, ftnlen);
    integer iostat;
    extern /* Subroutine */ int errint_(char *, integer *, ftnlen);
    extern logical return_(void);

    /* Fortran I/O blocks */
    static cilist io___4 = { 1, 0, 1, 0, 0 };


/* $ Abstract */

/*     Read N encoded integers from a text file, decoding them into */
/*     their equivalent integers. */

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

/*     CONVERSION */
/*     NUMBERS */
/*     UTILITY */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     UNIT      I    Fortran unit number of input text file. */
/*     N         I    Number of integers to read and decode. */
/*     DATA      O    List of decoded integers. */

/* $ Detailed_Input */

/*     UNIT     is the Fortran unit number for a previously opened text */
/*              file. All reading will begin at the CURRENT POSITION */
/*              in the text file. */

/*     N        is the number of encoded integers to be read from the */
/*              text file attached to UNIT. */

/* $ Detailed_Output */

/*     DATA     is the list of decoded integers read from the text file */
/*              attached to UNIT. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If N, the number of data items, is not positive, the error */
/*         SPICE(INVALIDARGUMENT) is signaled. */

/*     2)  If an error occurs while reading from the text file attached */
/*         to UNIT, the error SPICE(FILEREADFAILED) is signaled. */

/*     3)  If an error occurs while decoding a number, the error */
/*         SPICE(DECODINGERROR) is signaled. */

/* $ Files */

/*     See the description of UNIT in $Detailed_Input. */

/* $ Particulars */

/*     This routine will read N encoded integers beginning at the */
/*     current position in a previously opened text file. The current */
/*     position in a file is defined to be the text line immediately */
/*     following the last text line that was written or read. The */
/*     integers will be decoded and placed into a list of integers */
/*     which will be passed back to the caller. The encoded integers */
/*     are represented as quoted character strings so that a Fortran */
/*     list directed read may be used to read the encoded values, */
/*     rather than a formatted read with the format specifier */
/*     FMT = '(A)'. */

/*     This routine is one of a pair of routines which are used to */
/*     encode and decode integers: */

/*           WRENCI -- Encode and write integers to a file. */
/*           RDENCI -- Read and decode integers from a file. */

/*     The encoding/decoding of integers is performed to provide a */
/*     portable means for transferring data values. */

/*     Currently the encoded integers are represented as signed */
/*     hexadecimal numbers See INT2HX.FOR and HX2INT.FOR for details. */

/* $ Examples */

/*     Suppose we have the following input file which contains the values */
/*     1 - 100 encoded, and that the input file has already been opened */
/*     for reading. The arrow, '-->', indicates the current position in */
/*     the file. */

/*        -->'1' '2' '3' '4' '5' '6' '7' '8' '9' 'A' 'B' 'C' 'D' 'E' */
/*           'F' '10' '11' '12' '13' '14' '15' '16' '17' '18' '19' */
/*           '1A' '1B' '1C' '1D' '1E' '1F' '20' '21' '22' '23' '24' */
/*           '25' '26' '27' '28' '29' '2A' '2B' '2C' '2D' '2E' '2F' */
/*           '30' '31' '32' '33' '34' '35' '36' '37' '38' '39' '3A' */
/*           '3B' '3C' '3D' '3E' '3F' '40' */
/*           '41' '42' '43' '44' '45' '46' '47' '48' '49' '4A' '4B' */
/*           '4C' '4D' '4E' '4F' '50' '51' '52' '53' '54' '55' '56' */
/*           '57' '58' '59' '5A' '5B' '5C' '5D' '5E' '5F' '60' '61' */
/*           '62' '63' '64' */

/*     Then, the following code fragment would read and decode these */
/*     100 values. */

/*           N = 100 */
/*           CALL RDENCI( UNIT, N, DATA ) */

/*      Upon returning, the array data would contain the values 1 - 100. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     K.R. Gehringer     (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 03-JUN-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. Fixed I/O type */
/*        of argument DATA in $Brief_I/O table. */

/* -    SPICELIB Version 1.0.0, 19-OCT-1992 (KRG) */

/* -& */
/* $ Index_Entries */

/*     read and decode encoded integers from a text file */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     Local variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("RDENCI", (ftnlen)6);
    }

/*     Check to see if the number of data items is less than or equal */
/*     to zero. If it is, signal an error. */

    if (*n < 1) {
	setmsg_("The number of data items to be read was not positive: #.", (
		ftnlen)56);
	errint_("#", n, (ftnlen)1);
	sigerr_("SPICE(INVALIDARGUMENT)", (ftnlen)22);
	chkout_("RDENCI", (ftnlen)6);
	return 0;
    }

/*     Initialize the beginning location to place the decoded data */
/*     items. */

    itmbeg = 1;

/*     We read in the encoded numbers in blocks of size WRKSIZ, and if */
/*     there was not a read error we will attempt to decode the numbers. */
/*     We signal an error if either: */

/*                (1) there is a read error */
/*                (2) there is an error decoding a number. */

/*     NOTE: EOF is interpreted as a read error because we know a priori */
/*           exactly how many data items we need to read: N. */

/*     Begin decoding the encoded data items read from the input file */
/*     in blocks of size NITMS. Each time the number of data items */
/*     NITMS is reached, decode the encoded numbers into the data array. */

    while(itmbeg <= *n) {

/*        The number of items is either the size of the workspace, or */
/*        the number of data items which remain to be processed, which */
/*        should always be less than or equal to the size of the */
/*        workspace. */

/* Computing MIN */
	i__1 = 64, i__2 = *n - itmbeg + 1;
	nitms = min(i__1,i__2);

/*        Read in a block of data items to be decoded. */

	io___4.ciunit = *unit;
	iostat = s_rsle(&io___4);
	if (iostat != 0) {
	    goto L100001;
	}
	i__1 = nitms;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    iostat = do_lio(&c__9, &c__1, work + (((i__2 = i__ - 1) < 64 && 0 
		    <= i__2 ? i__2 : s_rnge("work", i__2, "rdenci_", (ftnlen)
		    261)) << 6), (ftnlen)64);
	    if (iostat != 0) {
		goto L100001;
	    }
	}
	iostat = e_rsle();
L100001:

/*        Check to see if we got a read error: IOSTAT .NE. 0. If we did, */
/*        then signal an error. EOF is considered to be a read error, */
/*        since we know exactly how many data items we expect to read. */

	if (iostat != 0) {
	    setmsg_("Error reading from logical unit #, IOSTAT = #.", (ftnlen)
		    46);
	    errint_("#", unit, (ftnlen)1);
	    errint_("#", &iostat, (ftnlen)1);
	    sigerr_("SPICE(FILEREADFAILED)", (ftnlen)21);
	    chkout_("RDENCI", (ftnlen)6);
	    return 0;
	}

/*        Begin to decode the data items into the data array. Signal an */
/*        error if we cannot decode a data item. */

	i__2 = nitms;
	for (i__ = 1; i__ <= i__2; ++i__) {
	    hx2int_(work + (((i__1 = i__ - 1) < 64 && 0 <= i__1 ? i__1 : 
		    s_rnge("work", i__1, "rdenci_", (ftnlen)284)) << 6), &
		    data[itmbeg + i__ - 2], &error, errmsg, (ftnlen)64, (
		    ftnlen)80);
	    if (error) {
		setmsg_("Decoding error occurred while attempting to decode "
			"item #: #. #", (ftnlen)63);
		errint_("#", &i__, (ftnlen)1);
		errch_("#", work + (((i__1 = i__ - 1) < 64 && 0 <= i__1 ? 
			i__1 : s_rnge("work", i__1, "rdenci_", (ftnlen)290)) 
			<< 6), (ftnlen)1, (ftnlen)64);
		errch_("#", errmsg, (ftnlen)1, (ftnlen)80);
		sigerr_("SPICE(DECODINGERROR)", (ftnlen)20);
		chkout_("RDENCI", (ftnlen)6);
		return 0;
	    }
	}

/*        Position the data item pointer at the next location to begin */
/*        placing the decoded items in the array DATA, and continue */
/*        processing the until done. */

	itmbeg += nitms;
    }
    chkout_("RDENCI", (ftnlen)6);
    return 0;
} /* rdenci_ */

