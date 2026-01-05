/* spct2b.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__0 = 0;
static integer c__1 = 1;

/* $Procedure SPCT2B ( SPK and CK, text to binary ) */
/* Subroutine */ int spct2b_(integer *unit, char *binary, ftnlen binary_len)
{
    /* System generated locals */
    integer i__1;
    cilist ci__1;
    olist o__1;
    cllist cl__1;

    /* Builtin functions */
    integer s_rsfe(cilist *), do_fio(integer *, char *, ftnlen), e_rsfe(void),
	     s_cmp(char *, char *, ftnlen, ftnlen), f_open(olist *), s_wsfe(
	    cilist *), e_wsfe(void), f_clos(cllist *);

    /* Local variables */
    char line[1000];
    extern /* Subroutine */ int spcac_(integer *, integer *, char *, char *, 
	    ftnlen, ftnlen), chkin_(char *, ftnlen);
    extern integer ltrim_(char *, ftnlen), rtrim_(char *, ftnlen);
    extern /* Subroutine */ int daft2b_(integer *, char *, integer *, ftnlen);
    integer handle;
    extern /* Subroutine */ int dafcls_(integer *), dafopw_(char *, integer *,
	     ftnlen);
    integer scrtch;
    extern /* Subroutine */ int errfnm_(char *, integer *, ftnlen), sigerr_(
	    char *, ftnlen), chkout_(char *, ftnlen), getlun_(integer *), 
	    setmsg_(char *, ftnlen);
    integer iostat;
    extern /* Subroutine */ int errint_(char *, integer *, ftnlen);
    extern logical return_(void);

/* $ Abstract */

/*     Reconstruct a binary SPK or CK file including comments */
/*     from a text file opened by the calling program. */

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
/*     UNIT       I   Logical unit connected to the text format file. */
/*     BINARY     I   Name of a binary SPK or CK file to be created. */

/* $ Detailed_Input */

/*     UNIT     is the logical unit connected to an existing text */
/*              format SPK or CK file that may contain comments in */
/*              the appropriate SPC format, as written by SPCB2A or */
/*              SPCB2T. This file must be opened for read access */
/*              using the routine TXTOPR. */

/*              This file may contain text that precedes and */
/*              follows the SPK or CK data and comments, however, */
/*              when calling this routine, the file pointer must be */
/*              in a position in the file such that the next line */
/*              returned by a READ statement is */

/*                   ''NAIF/DAF'' */

/*              which marks the beginning of the data. */

/*     BINARY   is the name of a binary SPK or CK file to be created. */
/*              The binary file contains the same data and comments */
/*              as the text file, but in the binary format required */
/*              for use with the SPICELIB reader subroutines. */

/* $ Detailed_Output */

/*     None. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If there is a problem opening or writing to the binary file, */
/*         an error is signaled by a routine in the call tree of this */
/*         routine. */

/*     2)  If there is a problem reading from the text file, the */
/*         error SPICE(FILEREADFAILED) is signaled. */

/*     3)  If there is a problem opening a scratch file, the error */
/*         SPICE(FILEOPENERROR) is signaled. */

/*     4)  If there is a problem writing to the scratch file, the */
/*         error SPICE(FILEWRITEFAILED) is signaled. */

/* $ Files */

/*     See arguments UNIT and BINARY above. */

/*     This routine uses a Fortran scratch file to temporarily store the */
/*     lines of comments if there are any. */

/* $ Particulars */

/*     The SPICELIB SPK and CK reader subroutines read binary files. */
/*     However, because different computing environments have different */
/*     binary representations of numbers, you must convert SPK and CK */
/*     files to text format when porting from one system to another. */
/*     After converting the file to text, you can transfer it using */
/*     a transfer protocol program like Kermit or FTP. Then, convert */
/*     the text file back to binary format. */

/*     The following is a list of the SPICELIB routines that convert */
/*     SPK and CK files between binary and text format: */

/*        SPCA2B    converts text to binary. It opens the text file, */
/*                  creates a new binary file, and closes both files. */

/*        SPCB2A    converts binary to text. It opens the binary file, */
/*                  creates a new text file, and closes both files. */

/*        SPCT2B    converts text to binary. It creates a new binary */
/*                  file and closes it. The text file is open on */
/*                  entrance and exit. */

/*        SPCB2T    converts binary to text. It opens the binary */
/*                  file and closes it. The text file is open on */
/*                  entrance and exit */

/*     See the SPC required reading for more information */
/*     about SPC routines and the SPK and CK file formats. */

/* $ Examples */

/*     1)  The following code fragment creates a text file containing */
/*         text format SPK data and comments preceded and followed */
/*         by a standard label. */

/*         The SPICELIB routine TXTOPN opens a new text file and TXTOPR */
/*         opens an existing text file for read access. TEXT and */
/*         BINARY are character strings that contain the names of the */
/*         text and binary files. */

/*            CALL TXTOPN ( TEXT, UNIT ) */

/*            (Write header label to UNIT) */

/*            CALL SPCB2T ( BINARY, UNIT ) */

/*            (Write trailing label to UNIT) */

/*            CLOSE ( UNIT ) */


/*         The following code fragment reconverts the text format */
/*         SPK data and comments back into binary format. */

/*            CALL TXTOPR ( TEXT, UNIT ) */

/*            (Read, or just read past, header label from UNIT) */

/*            CALL SPCT2B ( UNIT, BINARY ) */

/*            (Read trailing label from UNIT, if desired ) */

/*            CLOSE ( UNIT ) */


/*     2)  Suppose three text format SPK files have been appended */
/*         together into one text file called THREE.TSP. The following */
/*         code fragment converts each set of data and comments into */
/*         its own binary file. */

/*            CALL TXTOPR ( 'THREE.TSP', UNIT  ) */

/*            CALL SPCT2B ( UNIT, 'FIRST.BSP'  ) */
/*            CALL SPCT2B ( UNIT, 'SECOND.BSP' ) */
/*            CALL SPCT2B ( UNIT, 'THIRD.BSP'  ) */

/*            CLOSE ( UNIT ) */

/* $ Restrictions */

/*     1)  This routine assumes that the data and comments in the */
/*         text format SPK or CK file come from a binary file */
/*         and were written by one of the routines SPCB2A or SPCB2T. */
/*         Data and/or comments written any other way may not be */
/*         in the correct format and, therefore, may not be handled */
/*         properly. */

/*     2)  Older versions of SPK and CK files did not have a comment */
/*         area. These files, in text format, may still be converted */
/*         to binary using SPCT2B. However, upon exit, the file pointer */
/*         will not be in position ready to read the first line of text */
/*         after the data. Instead, the next READ statement after */
/*         calling SPCT2B will return the second line of text after */
/*         the data. Therefore, example 1 may not work as desired */
/*         if the trailing label begins on the first line after the */
/*         data. To solve this problem, use DAFT2B instead of SPCT2B. */

/*     3)  UNIT must be obtained via TXTOPR. Use TXTOPR to open text */
/*         files for read access and get the logical unit. System */
/*         dependencies regarding opening text files have been isolated */
/*         in the routines TXTOPN and TXTOPR. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     J.E. McLean        (JPL) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 13-AUG-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 05-APR-1991 (JEM) */

/* -& */
/* $ Index_Entries */

/*     text SPK or CK to binary */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     Local variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("SPCT2B", (ftnlen)6);
    }

/*     DAFT2B creates the new binary file and writes the data to */
/*     it.  If the 'NAIF/DAF' keyword is not the first line that */
/*     it reads from the text file, it will signal an error. */
/*     Initially, no records are reserved. */

    daft2b_(unit, binary, &c__0, binary_len);

/*     The comments follow the data and are surrounded by markers. */
/*     BMARK should be the next line that we read.  If it isn't, */
/*     then this is an old file, created before the comment area */
/*     existed.  In this case, we've read one line too far, but */
/*     we can't backspace because the file was written using list- */
/*     directed formatting (See the ANSI standard).  All we can do */
/*     is check out, leaving the file pointer where it is, but */
/*     that's better than signaling an error. */

    ci__1.cierr = 1;
    ci__1.ciend = 1;
    ci__1.ciunit = *unit;
    ci__1.cifmt = "(A)";
    iostat = s_rsfe(&ci__1);
    if (iostat != 0) {
	goto L100001;
    }
    iostat = do_fio(&c__1, line, (ftnlen)1000);
    if (iostat != 0) {
	goto L100001;
    }
    iostat = e_rsfe();
L100001:
    if (iostat > 0) {
	setmsg_("Error reading the text file named FNM.  Value of IOSTAT is "
		"#.", (ftnlen)61);
	errint_("#", &iostat, (ftnlen)1);
	errfnm_("FNM", unit, (ftnlen)3);
	sigerr_("SPICE(FILEREADFAILED)", (ftnlen)21);
	chkout_("SPCT2B", (ftnlen)6);
	return 0;
    }
    i__1 = ltrim_(line, (ftnlen)1000) - 1;
    if (s_cmp(line + i__1, "~NAIF/SPC BEGIN COMMENTS~", 1000 - i__1, (ftnlen)
	    25) != 0 || iostat < 0) {
	chkout_("SPCT2B", (ftnlen)6);
	return 0;
    }

/*     We're not at the end of the file, and the line we read */
/*     is BMARK, so we write the comments to a scratch file. */
/*     We do this because we have to use SPCAC to add the comments */
/*     to the comment area of the binary file, and SPCAC rewinds */
/*     the file.  It's okay for SPCAC to rewind a scratch file, */
/*     but it's not okay to rewind the file connected to UNIT -- */
/*     we don't know the initial location of the file pointer. */

    getlun_(&scrtch);
    o__1.oerr = 1;
    o__1.ounit = scrtch;
    o__1.ofnm = 0;
    o__1.orl = 0;
    o__1.osta = "SCRATCH";
    o__1.oacc = "SEQUENTIAL";
    o__1.ofm = "FORMATTED";
    o__1.oblnk = 0;
    iostat = f_open(&o__1);
    if (iostat != 0) {
	setmsg_("Error opening a scratch file.  File name was FNM.  Value of"
		" IOSTAT is #.", (ftnlen)72);
	errint_("#", &iostat, (ftnlen)1);
	errfnm_("FNM", &scrtch, (ftnlen)3);
	sigerr_("SPICE(FILEOPENERROR)", (ftnlen)20);
	chkout_("SPCT2B", (ftnlen)6);
	return 0;
    }
    ci__1.cierr = 1;
    ci__1.ciunit = scrtch;
    ci__1.cifmt = "(A)";
    iostat = s_wsfe(&ci__1);
    if (iostat != 0) {
	goto L100002;
    }
    iostat = do_fio(&c__1, line, rtrim_(line, (ftnlen)1000));
    if (iostat != 0) {
	goto L100002;
    }
    iostat = e_wsfe();
L100002:
    if (iostat != 0) {
	setmsg_("Error writing to scratch file. File name is FNM.  Value of "
		"IOSTAT is #.", (ftnlen)71);
	errint_("#", &iostat, (ftnlen)1);
	errfnm_("FNM", &scrtch, (ftnlen)3);
	sigerr_("SPICE(FILEWRITEFAILED)", (ftnlen)22);
	chkout_("SPCT2B", (ftnlen)6);
	return 0;
    }

/*     Continue reading lines from the text file and storing them */
/*     in the scratch file until we get to the end marker. */

    for(;;) { /* while(complicated condition) */
	i__1 = ltrim_(line, (ftnlen)1000) - 1;
	if (!(s_cmp(line + i__1, "~NAIF/SPC END COMMENTS~", 1000 - i__1, (
		ftnlen)23) != 0))
		break;
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
	    setmsg_("Error reading the text file named FNM.  Value of IOSTAT"
		    " is #.", (ftnlen)61);
	    errint_("#", &iostat, (ftnlen)1);
	    errfnm_("FNM", unit, (ftnlen)3);
	    sigerr_("SPICE(FILEREADFAILED)", (ftnlen)21);
	    chkout_("SPCT2B", (ftnlen)6);
	    return 0;
	}
	ci__1.cierr = 1;
	ci__1.ciunit = scrtch;
	ci__1.cifmt = "(A)";
	iostat = s_wsfe(&ci__1);
	if (iostat != 0) {
	    goto L100004;
	}
	iostat = do_fio(&c__1, line, rtrim_(line, (ftnlen)1000));
	if (iostat != 0) {
	    goto L100004;
	}
	iostat = e_wsfe();
L100004:
	if (iostat != 0) {
	    setmsg_("Error writing to scratch file.  File name is FNM.  Valu"
		    "e of IOSTAT is #.", (ftnlen)72);
	    errint_("#", &iostat, (ftnlen)1);
	    errfnm_("FNM", &scrtch, (ftnlen)3);
	    sigerr_("SPICE(FILEWRITEFAILED)", (ftnlen)22);
	    chkout_("SPCT2B", (ftnlen)6);
	    return 0;
	}
    }

/*     Open the new binary file and add the comments that have been */
/*     stored temporarily in a scratch file. */

    dafopw_(binary, &handle, binary_len);
    spcac_(&handle, &scrtch, "~NAIF/SPC BEGIN COMMENTS~", "~NAIF/SPC END COM"
	    "MENTS~", (ftnlen)25, (ftnlen)23);

/*     Close the files.  The scratch file is automatically deleted. */

    dafcls_(&handle);
    cl__1.cerr = 0;
    cl__1.cunit = scrtch;
    cl__1.csta = 0;
    f_clos(&cl__1);
    chkout_("SPCT2B", (ftnlen)6);
    return 0;
} /* spct2b_ */

