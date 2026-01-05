/* spcb2t.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__9 = 9;
static integer c__1 = 1;

/* $Procedure SPCB2T ( SPK and CK, binary to text ) */
/* Subroutine */ int spcb2t_(char *binary, integer *unit, ftnlen binary_len)
{
    /* Builtin functions */
    integer s_wsle(cilist *), do_lio(integer *, integer *, char *, ftnlen), 
	    e_wsle(void);

    /* Local variables */
    extern /* Subroutine */ int chkin_(char *, ftnlen), spcec_(integer *, 
	    integer *), dafb2t_(char *, integer *, ftnlen);
    integer handle;
    extern /* Subroutine */ int dafcls_(integer *), dafopr_(char *, integer *,
	     ftnlen), errfnm_(char *, integer *, ftnlen), sigerr_(char *, 
	    ftnlen), chkout_(char *, ftnlen), setmsg_(char *, ftnlen);
    integer iostat;
    extern /* Subroutine */ int errint_(char *, integer *, ftnlen);
    extern logical return_(void);

    /* Fortran I/O blocks */
    static cilist io___2 = { 1, 0, 0, 0, 0 };
    static cilist io___4 = { 1, 0, 0, 0, 0 };


/* $ Abstract */

/*     Convert the contents of a binary SPK or CK file to text, */
/*     including comments if present, and write them to a text file */
/*     opened by the calling program. */

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
/*     BINARY     I   Name of an existing binary SPK or CK file. */
/*     UNIT       I   Logical unit connected to a text file. */

/* $ Detailed_Input */

/*     BINARY   is the name of an existing binary SPK or CK file */
/*              that may contain comments in its comment area. */

/*     UNIT     is the logical unit connected to a text file that */
/*              has been opened for write access. Use the routine */
/*              TXTOPN to open this file. Upon exit, this file will */
/*              contain the same data and comments as the binary */
/*              file, but in text format which is more suitable for */
/*              transfer between heterogeneous computing environments. */

/* $ Detailed_Output */

/*     None. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If there is a problem opening or reading from the binary file, */
/*         an error is signaled by a routine in the call tree of this */
/*         routine. */

/*     2)  If there is a problem writing to the text file, */
/*         the error SPICE(FILEWRITEFAILED) is signaled. */

/* $ Files */

/*     See arguments BINARY and UNIT above. */

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

/*     The following code fragment creates a text file containing */
/*     text format SPK data and comments preceded and followed */
/*     by a standard label. */

/*     The SPICELIB routine TXTOPN opens a new text file and TXTOPR */
/*     opens an existing text file for read access. TEXT and */
/*     BINARY are character strings that contain the names of the */
/*     text and binary files. */

/*            CALL TXTOPN ( TEXT, UNIT ) */

/*            (Write header label to UNIT) */

/*            CALL SPCB2T ( BINARY, UNIT ) */

/*            (Write trailing label to UNIT) */

/*            CLOSE ( UNIT ) */


/*     The following code fragment reconverts the text format */
/*     SPK data and comments back into binary format. */

/*            CALL TXTOPR ( TEXT, UNIT ) */

/*            (Read, or just read past, header label from UNIT) */

/*            CALL SPCT2B ( UNIT, BINARY ) */

/*            (Read trailing label from UNIT, if desired ) */

/*            CLOSE ( UNIT ) */

/* $ Restrictions */

/*     1)  This routine assumes that the comment area of the binary SPK */
/*         or CK file contains only text stored by SPCAC. Comments */
/*         written any other way may not be handled properly. */

/*     2)  UNIT must be obtained via TXTOPN. Use TXTOPN to open new */
/*         text files for write access and get the logical unit. */
/*         System dependencies regarding opening text files have */
/*         been isolated in the routines TXTOPN and TXTOPR. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     J.E. McLean        (JPL) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 03-JUN-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 05-APR-1991 (JEM) */

/* -& */
/* $ Index_Entries */

/*     binary SPK or CK to text */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */

/*     IFNLEN is the length of a DAF internal file name. */


/*     Local variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("SPCB2T", (ftnlen)6);
    }

/*     First, convert the binary data to text and write it to */
/*     the text file. */

    dafb2t_(binary, unit, binary_len);

/*     Next, write the begin comments marker. */

    io___2.ciunit = *unit;
    iostat = s_wsle(&io___2);
    if (iostat != 0) {
	goto L100001;
    }
    iostat = do_lio(&c__9, &c__1, "~NAIF/SPC BEGIN COMMENTS~", (ftnlen)25);
    if (iostat != 0) {
	goto L100001;
    }
    iostat = e_wsle();
L100001:
    if (iostat != 0) {
	setmsg_("Error writing the begin comments marker to the text file na"
		"med FNM.  IOSTAT = #.", (ftnlen)80);
	errfnm_("FNM", unit, (ftnlen)3);
	errint_("#", &iostat, (ftnlen)1);
	sigerr_("SPICE(FILEWRITEFAILED)", (ftnlen)22);
	chkout_("SPCB2T", (ftnlen)6);
	return 0;
    }

/*     Open the DAF for read access, extract the comments from */
/*     it and write them to the text file, then close the DAF. */
/*     If the comment area of the binary file is empty, SPCEC */
/*     writes nothing to the text file, but even so, we still */
/*     want the markers. */

    dafopr_(binary, &handle, binary_len);
    spcec_(&handle, unit);
    dafcls_(&handle);

/*     Finally, write the end comments marker. */

    io___4.ciunit = *unit;
    iostat = s_wsle(&io___4);
    if (iostat != 0) {
	goto L100002;
    }
    iostat = do_lio(&c__9, &c__1, "~NAIF/SPC END COMMENTS~", (ftnlen)23);
    if (iostat != 0) {
	goto L100002;
    }
    iostat = e_wsle();
L100002:
    if (iostat != 0) {
	setmsg_("Error writing the end comments marker to the text file name"
		"d FNM.  IOSTAT = #.", (ftnlen)78);
	errfnm_("FNM", unit, (ftnlen)3);
	errint_("#", &iostat, (ftnlen)1);
	sigerr_("SPICE(FILEWRITEFAILED)", (ftnlen)22);
	chkout_("SPCB2T", (ftnlen)6);
	return 0;
    }
    chkout_("SPCB2T", (ftnlen)6);
    return 0;
} /* spcb2t_ */

