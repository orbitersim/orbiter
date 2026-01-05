/* zzdafnfr.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__3 = 3;
static integer c__1 = 1;

/* $Procedure ZZDAFNFR ( Private --- DAF write New File Record ) */
/* Subroutine */ int zzdafnfr_(integer *lun, char *idword, integer *nd, 
	integer *ni, char *ifname, integer *fward, integer *bward, integer *
	free, char *format, ftnlen idword_len, ftnlen ifname_len, ftnlen 
	format_len)
{
    /* Initialized data */

    static logical first = TRUE_;

    /* System generated locals */
    address a__1[3];
    integer i__1[3];
    cllist cl__1;

    /* Builtin functions */
    /* Subroutine */ int s_cat(char *, char **, integer *, integer *, ftnlen),
	     s_copy(char *, char *, ftnlen, ftnlen);
    integer s_wdue(cilist *), do_uio(integer *, char *, ftnlen), e_wdue(void),
	     f_clos(cllist *);

    /* Local variables */
    integer i__;
    extern /* Subroutine */ int zzftpstr_(char *, char *, char *, char *, 
	    ftnlen, ftnlen, ftnlen, ftnlen);
    char delim[1];
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    extern integer rtrim_(char *, ftnlen);
    char locifn[60], locidw[8], locfmt[8], nullch[1], lftbkt[6];
    extern /* Subroutine */ int errfnm_(char *, integer *, ftnlen), sigerr_(
	    char *, ftnlen);
    char rgtbkt[6];
    extern /* Subroutine */ int chkout_(char *, ftnlen);
    integer iostat;
    extern /* Subroutine */ int setmsg_(char *, ftnlen);
    static char prenul[603];
    extern /* Subroutine */ int errint_(char *, integer *, ftnlen);
    extern logical return_(void);
    static char ftpstr[28], pstnul[297];
    char tststr[16];

    /* Fortran I/O blocks */
    static cilist io___15 = { 1, 0, 0, 0, 1 };


/* $ Abstract */

/*    Write the file record to a new DAF file. */

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

/*     DAF */
/*     UTILITY */

/* $ Declarations */
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


/*     Include Section:  Private FTP Validation String Parameters */

/*        zzftprms.inc Version 1    01-MAR-1999 (FST) */

/*     This include file centralizes the definition of string sizes */
/*     and other parameters that are necessary to properly implement */
/*     the FTP error detection scheme for binary kernels. */

/*     Before making any alterations to the contents of this file, */
/*     refer to the header of ZZFTPSTR for a detailed discussion of */
/*     the FTP validation string. */

/*     Size of FTP Test String Component: */


/*     Size of Maximum Expanded FTP Validation String: */

/*      (This indicates the size of a buffer to hold the test */
/*       string sequence from a possibly corrupt file. Empirical */
/*       evidence strongly indicates that expansion due to FTP */
/*       corruption at worst doubles the number of characters. */
/*       So take 3*SIZSTR to be on the safe side.) */


/*     Size of FTP Validation String Brackets: */


/*     Size of FTP Validation String: */


/*     Size of DELIM. */


/*     Number of character clusters present in the validation string. */


/*     End Include Section:  Private FTP Validation String Parameters */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     LUN        I   Logical unit number of an open DAF file. */
/*     IDWORD     I   DAF ID word. */
/*     ND         I   Number of double precision components in a summary. */
/*     NI         I   Number of integer components in a summary. */
/*     IFNAME     I   Internal filename. */
/*     FWARD      I   First descriptor record. */
/*     BWARD      I   Last descriptor record. */
/*     FREE       I   First free address. */
/*     FORMAT     I   File binary format identifier string. */

/* $ Detailed_Input */

/*     LUN        is a logical unit number of a DAF whose first record is */
/*                to be created with a DAF file record bearing the */
/*                attributes specified by the other arguments. */

/*     IDWORD     is the 'ID word' contained in the first eight */
/*                characters of the file record. */

/*     ND,        are the number of double precision and integer */
/*     NI         components, respectively, in each array summary */
/*                in the specified file. */

/*     IFNAME     is the internal filename to be stored in the file */
/*                record for identification purposes. */

/*     FWARD,     are the record numbers of the first and last */
/*     BWARD      descriptor records in the DAF file, respectively. */
/*                FWARD is greater than 2 whenever reserved records */
/*                are present. */

/*     FREE       is the first free address pointer.  This integer */
/*                stores the first free DAF address for writing the */
/*                next array to be appended to the file. */

/*     FORMAT     is a character string that indicates what the numeric */
/*                binary format the DAF is utilizing. */

/* $ Detailed_Output */

/*     None. */

/* $ Parameters */

/*     See include file zzftprms.inc. */

/* $ Exceptions */

/*     1) If any errors occur from the WRITE to the logical unit LUN, */
/*        the error SPICE(DAFWRITEFAIL) is signaled. Before returning */
/*        to the caller, the file attached to LUN is closed and deleted. */

/* $ Files */

/*     This routine writes to the first record of the DAF whose */
/*     logical unit is LUN. */

/* $ Particulars */

/*     This routine assembles the file record and writes it to the */
/*     first record in a DAF.  Its purpose is to write new file */
/*     records only.  For updates, use DAFWFR. */

/*     Make certain the caller checks FAILED() after this returns, since */
/*     on error it closes and deletes the file. */

/* $ Examples */

/*     None. */

/* $ Restrictions */

/*     1) An individual character must occupy 1 byte of space and */
/*        conform to the ASCII standard. */

/*     2) The word size for the machine should be at least 32 bits, */
/*        else the computations to null pad the gaps in the file */
/*        record may overstep record boundaries. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     F.S. Turner     (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.2, 02-OCT-2021 (NJB) */

/*        Corrected typos in comments. Reordered header sections. */

/* -    SPICELIB Version 1.0.1, 11-DEC-2001 (FST) */

/*        Corrected the omission of IDWORD from the Brief_I/O and */
/*        Detailed_Input sections of the module header. */

/* -    SPICELIB Version 1.0.0, 02-MAR-1999 (FST) */


/* -& */

/*     SPICELIB Functions */


/*     Local Parameters */

/*     Amount of space measured in characters necessary to */
/*     null pad between the last character of FORMAT and the */
/*     first character of FTPSTR to keep FTPSTR at character */
/*     700 in a 1024 byte record. */


/*     Amount of space measured in characters necessary to */
/*     null pad from the last character of FTPSTR to the */
/*     end of the file record. Note: This value assumes the */
/*     length of the file record is 1024 bytes.  The DAF */
/*     specification only requires the presence of 1000 */
/*     characters, so this may require modification for */
/*     non-standard platforms. */


/*     Lengths of internal file name, ID word, and format word. */


/*     Local Variables */


/*     Saved Variables */


/*     Data Statements */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("ZZDAFNFR", (ftnlen)8);
    }

/*     On the first pass, format the PRENUL and PSTNUL strings, */
/*     and build FTPSTR from its components. */

    if (first) {

/*        Store NULL into NULLCH. */

	*(unsigned char *)nullch = '\0';

/*        Set all of the characters of PRENUL to nulls. */

	for (i__ = 1; i__ <= 603; ++i__) {
	    *(unsigned char *)&prenul[i__ - 1] = *(unsigned char *)nullch;
	}

/*        Set all of the characters of PSTNUL to nulls. */

	for (i__ = 1; i__ <= 297; ++i__) {
	    *(unsigned char *)&pstnul[i__ - 1] = *(unsigned char *)nullch;
	}

/*        Build FTPSTR from its components that come back from */
/*        ZZFTPSTR.  This private SPICE routine returns the */
/*        following components: */
/* 7 */
/*           TSTSTR - The test component of the FTP string */
/*           LFTBKT - The left bracketing, printable, component of */
/*                    the FTP string. */
/*           RGTBKT - The right bracketing, printable, component of */
/*                    the FTP string. */
/*           DELIM  - The printable delimiter that separates the */
/*                    individual test character blocks in TSTSTR. */

/*        which are assembled into the FTP string as it appears in */
/*        the DAF file record. */

	zzftpstr_(tststr, lftbkt, rgtbkt, delim, (ftnlen)16, (ftnlen)6, (
		ftnlen)6, (ftnlen)1);
/* Writing concatenation */
	i__1[0] = rtrim_(lftbkt, (ftnlen)6), a__1[0] = lftbkt;
	i__1[1] = rtrim_(tststr, (ftnlen)16), a__1[1] = tststr;
	i__1[2] = rtrim_(rgtbkt, (ftnlen)6), a__1[2] = rgtbkt;
	s_cat(ftpstr, a__1, i__1, &c__3, (ftnlen)28);

/*        Stop this block from executing except on the first pass. */

	first = FALSE_;
    }

/*     Make local copies of each of the string arguments.  This way we */
/*     maintain the proper sizes for each of the string objects, in */
/*     the event larger or smaller strings are passed in. */

    s_copy(locidw, idword, (ftnlen)8, idword_len);
    s_copy(locifn, ifname, (ftnlen)60, ifname_len);
    s_copy(locfmt, format, (ftnlen)8, format_len);

/*     Write the file record components out to the first record of the */
/*     file. */

    io___15.ciunit = *lun;
    iostat = s_wdue(&io___15);
    if (iostat != 0) {
	goto L100001;
    }
    iostat = do_uio(&c__1, locidw, (ftnlen)8);
    if (iostat != 0) {
	goto L100001;
    }
    iostat = do_uio(&c__1, (char *)&(*nd), (ftnlen)sizeof(integer));
    if (iostat != 0) {
	goto L100001;
    }
    iostat = do_uio(&c__1, (char *)&(*ni), (ftnlen)sizeof(integer));
    if (iostat != 0) {
	goto L100001;
    }
    iostat = do_uio(&c__1, locifn, (ftnlen)60);
    if (iostat != 0) {
	goto L100001;
    }
    iostat = do_uio(&c__1, (char *)&(*fward), (ftnlen)sizeof(integer));
    if (iostat != 0) {
	goto L100001;
    }
    iostat = do_uio(&c__1, (char *)&(*bward), (ftnlen)sizeof(integer));
    if (iostat != 0) {
	goto L100001;
    }
    iostat = do_uio(&c__1, (char *)&(*free), (ftnlen)sizeof(integer));
    if (iostat != 0) {
	goto L100001;
    }
    iostat = do_uio(&c__1, locfmt, (ftnlen)8);
    if (iostat != 0) {
	goto L100001;
    }
    iostat = do_uio(&c__1, prenul, (ftnlen)603);
    if (iostat != 0) {
	goto L100001;
    }
    iostat = do_uio(&c__1, ftpstr, (ftnlen)28);
    if (iostat != 0) {
	goto L100001;
    }
    iostat = do_uio(&c__1, pstnul, (ftnlen)297);
    if (iostat != 0) {
	goto L100001;
    }
    iostat = e_wdue();
L100001:

/*     Check IOSTAT for errors. */

    if (iostat != 0) {

/*        Since we are unable to write to the file record, make */
/*        certain the output file is destroyed. */

	setmsg_("Attempt to write file '#' failed. Value of IOSTAT was #. Th"
		"e file has been deleted.", (ftnlen)83);
	errfnm_("#", lun, (ftnlen)1);
	errint_("#", &iostat, (ftnlen)1);
	cl__1.cerr = 0;
	cl__1.cunit = *lun;
	cl__1.csta = "DELETE";
	f_clos(&cl__1);
	sigerr_("SPICE(DAFWRITEFAIL)", (ftnlen)19);
	chkout_("ZZDAFNFR", (ftnlen)8);
	return 0;
    }
    chkout_("ZZDAFNFR", (ftnlen)8);
    return 0;
} /* zzdafnfr_ */

