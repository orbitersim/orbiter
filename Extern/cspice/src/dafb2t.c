/* dafb2t.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static logical c_false = FALSE_;
static integer c__1 = 1;
static integer c__3 = 3;
static integer c__9 = 9;
static integer c__5 = 5;

/* $Procedure DAFB2T ( DAF, binary to text ) */
/* Subroutine */ int dafb2t_(char *binary, integer *text, ftnlen binary_len)
{
    /* System generated locals */
    address a__1[3];
    integer i__1[3], i__2, i__3;
    char ch__1[10], ch__2[62], ch__3[1002];

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_rdue(cilist *), do_uio(integer *, char *, ftnlen), e_rdue(void),
	     s_wsle(cilist *);
    /* Subroutine */ int s_cat(char *, char **, integer *, integer *, ftnlen);
    integer do_lio(integer *, integer *, char *, ftnlen), e_wsle(void), 
	    s_rnge(char *, integer, char *, integer);

    /* Local variables */
    char name__[1000];
    integer free;
    extern /* Subroutine */ int zzddhhlu_(integer *, char *, logical *, 
	    integer *, ftnlen);
    integer i__;
    extern /* Subroutine */ int dafgn_(char *, ftnlen);
    integer begin;
    extern /* Subroutine */ int dafgs_(doublereal *), chkin_(char *, ftnlen);
    integer bward;
    extern /* Subroutine */ int dafus_(doublereal *, integer *, integer *, 
	    doublereal *, integer *);
    integer fward;
    extern /* Subroutine */ int errch_(char *, char *, ftnlen, ftnlen);
    integer chunk;
    logical found;
    integer csize, isize, lsize;
    extern /* Subroutine */ int dafgda_(integer *, integer *, integer *, 
	    doublereal *);
    doublereal dc[125];
    integer ic[250];
    extern /* Subroutine */ int daffna_(logical *);
    integer nd;
    extern logical failed_(void);
    extern /* Subroutine */ int dafbfs_(integer *);
    integer ni, handle;
    extern /* Subroutine */ int dafcls_(integer *);
    char ifname[60];
    extern /* Subroutine */ int dafrfr_(integer *, integer *, integer *, char 
	    *, integer *, integer *, integer *, ftnlen);
    doublereal buffer[100];
    integer daflun;
    extern /* Subroutine */ int dafopr_(char *, integer *, ftnlen);
    char idword[8];
    extern /* Subroutine */ int errfnm_(char *, integer *, ftnlen), sigerr_(
	    char *, ftnlen), chkout_(char *, ftnlen), setmsg_(char *, ftnlen);
    integer iostat;
    extern /* Subroutine */ int errint_(char *, integer *, ftnlen);
    extern logical return_(void);
    integer end;
    doublereal sum[125];

    /* Fortran I/O blocks */
    static cilist io___5 = { 1, 0, 1, 0, 1 };
    static cilist io___12 = { 1, 0, 0, 0, 0 };
    static cilist io___13 = { 1, 0, 0, 0, 0 };
    static cilist io___14 = { 1, 0, 0, 0, 0 };
    static cilist io___15 = { 1, 0, 0, 0, 0 };
    static cilist io___23 = { 1, 0, 0, 0, 0 };
    static cilist io___24 = { 1, 0, 0, 0, 0 };
    static cilist io___25 = { 1, 0, 0, 0, 0 };
    static cilist io___27 = { 1, 0, 0, 0, 0 };
    static cilist io___33 = { 1, 0, 0, 0, 0 };
    static cilist io___34 = { 1, 0, 0, 0, 0 };
    static cilist io___35 = { 1, 0, 0, 0, 0 };
    static cilist io___36 = { 1, 0, 0, 0, 0 };
    static cilist io___37 = { 1, 0, 0, 0, 0 };
    static cilist io___38 = { 1, 0, 0, 0, 0 };


/* $ Abstract */

/*     Deprecated: This routine has been superseded by the SPICELIB */
/*     routine DAFBT. NAIF supports this routine only to provide backward */
/*     compatibility. */

/*     Write the contents of a binary DAF to a text file opened by */
/*     the calling program. */

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

/*     DAF */

/* $ Keywords */

/*     FILES */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     BINARY     I   Name of an existing binary DAF. */
/*     TEXT       I   Logical unit connected to text file. */

/* $ Detailed_Input */

/*     BINARY   is the name of an existing binary DAF. */

/*     TEXT     is a logical unit number, to which a text file has */
/*              been connected by the calling program, and into */
/*              which the contents of BINARY are to be written */
/*              (in a form more suitable for transfer between */
/*              heterogeneous computing environments). */

/* $ Detailed_Output */

/*     None. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If for some reason the text file cannot be written, */
/*         the error SPICE(DAFWRITEFAIL) is signaled. */

/*     2)  If for some reason the ID word cannot be read from the DAF */
/*         file, the error SPICE(DAFREADFAIL) is signaled. */

/* $ Files */

/*     See arguments BINARY, TEXT. */

/* $ Particulars */

/*     This routine has been made obsolete by the new DAF binary to text */
/*     conversion routine DAFBT. This routine remains available for */
/*     reasons of backward compatibility. We strongly recommend that you */
/*     use the new conversion routines for any new software development. */
/*     Please see the header of the routine DAFBT for details. */

/*     Any binary DAF may be transferred between heterogeneous */
/*     Fortran environments by converting it to an equivalent file */
/*     containing only ASCII characters. Such a file can be transferred */
/*     almost universally, using any number of established protocols */
/*     (Kermit, FTP, and so on). Once transferred, the ASCII file can */
/*     be converted to a binary file, using the representations */
/*     native to the new host environment. */

/*     There are two pairs of routines that can be used to convert */
/*     DAFs between binary and text. The first pair, DAFB2A */
/*     and DAFA2B, works with complete files. That is, DAFB2A creates */
/*     a complete ASCII file containing all of the information in */
/*     a particular binary file, and nothing else; this file can */
/*     be fed directly into DAFA2B to produce a complete binary file. */
/*     In each case, the names of the files are specified. */

/*     A related pair of routines, DAFB2T and DAFT2B, assume that */
/*     the ASCII data are to be stored in the midst of a text file. */
/*     This allows the calling program to surround the data with */
/*     standardized labels, to append several binary files into a */
/*     single text file, and so on. */

/*     Note that the contents of reserved records in the binary file */
/*     are not written by this routine (although they may be stored */
/*     in the ASCII file by the calling program). */

/* $ Examples */

/*     DAFB2A and DAFA2B are typically used for simple file transfers. */
/*     If file A.DAF is a binary DAF in environment 1, it can be */
/*     transferred to environment 2 in three steps. */

/*        1) Convert it to ASCII: */

/*              CALL DAFB2A ( 'A.DAF', 'A.ASCII' ) */

/*        2) Transfer the ASCII file, using FTP, Kermit, or some other */
/*           file transfer utility: */

/*              ftp> put a.ascii */

/*        3) Convert it to binary on the new machine, */

/*              CALL DAFA2B ( 'A.ASCII', 'A.DAF', RESV ) */

/*     Note that DAFB2A and DAFA2B work in any standard Fortran-77 */
/*     environment. */

/*     If the file needs to contain other information---a standard */
/*     label, for instance---the first and third steps must be modified */
/*     to use DAFB2T and DAFT2B. The first step becomes */

/*        (Open a text file) */
/*        (Write the label) */
/*        CALL DAFB2T ( BINARY, UNIT  ) */
/*        (Close the text file) */

/*     The third step becomes */

/*        (Open the text file) */
/*        (Read the label) */
/*        CALL DAFT2B ( UNIT, BINARY, RESV ) */
/*        (Close the text file) */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     K.R. Gehringer     (JPL) */
/*     H.A. Neilan        (JPL) */
/*     W.L. Taber         (JPL) */
/*     F.S. Turner        (JPL) */
/*     I.M. Underwood     (JPL) */
/*     E.D. Wright        (JPL) */

/* $ Version */

/* -    SPICELIB Version 3.1.0, 26-OCT-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. Moved DAF */
/*        required reading from $Literature_References to */
/*        $Required_Reading section. */

/* -    SPICELIB Version 3.0.1, 26-JUL-2012 (EDW) */

/*        Edited $Abstract section to use "Deprecated" keyword */
/*        and state replacement routine. */

/*        Eliminated unneeded $Revisions section. */

/* -    SPICELIB Version 3.0.0, 16-NOV-2001 (FST) */

/*        This routine still uses a naked READ to retrieve the */
/*        file IDWORD from the first 8 characters stored in the */
/*        file record. It may be that future environments */
/*        will have characters whose storage exceeds 1 byte, */
/*        in which case this routine will require modification. */
/*        One possibility is to call the private file record */
/*        reader ZZDAFGFR, which must address the translation */
/*        for all supported non-native binary file formats on this */
/*        platform. */

/*        The existing call to DAFHLU was replaced with ZZDDHHLU. */
/*        The call to DAFRDA was replaced with a call to the new, */
/*        translation-aware routine DAFGDA. */

/* -    SPICELIB Version 2.0.0, 04-OCT-1993 (KRG) */

/*        Added the variable IDWORD to the routine for storing the ID */
/*        word from the file being converted. This replaces a hard coded */
/*        value of 'NAIF/DAF', and supports the new interpretation of the */
/*        ID word. */

/*        Removed the error SPICE(DAFNOIDWORD) as it was no longer */
/*        relevant. */

/*        There were no checks of the IOSTAT variable after attempting to */
/*        write to the text file, a single test of the IOSTAT variable */
/*        was made at the end of the routine. This was not adequate to */
/*        detect errors when writing to the text file. So after all of */
/*        these write statements, an IF ... END IF block was added to */
/*        signal an error if IOSTAT .NE. 0. */

/*           IF ( IOSTAT .NE. 0 ) THEN */

/*              CALL DAFCLS ( HANDLE                                ) */
/*              CALL SETMSG ( 'The attempt to write to file ''#''' // */
/*        .                   ' failed. IOSTAT = #.'                ) */
/*              CALL ERRFNM ( '#', TEXT                             ) */
/*              CALL SIGERR ( SPICE(DAFWRITEFAIL)                 ) */
/*              CALL CHKOUT ( 'DAFB2T'                              ) */
/*              RETURN */

/*           END IF */

/*        Removed the code from the end of the routine that purported to */
/*        check for read errors: */

/*           C */
/*           C     If any write screws up, they should all screw up. Why */
/*           C     make a billion separate checks? */
/*           C */
/*                 IF ( IOSTAT .NE. 0 ) THEN */
/*                    CALL SETMSG ( 'Value of IOSTAT was: #. ' ) */
/*                    CALL ERRINT ( '#', IOSTAT                ) */
/*                    CALL SIGERR ( SPICE(DAFWRITEFAIL)      ) */
/*                  END IF */

/*        The answer to the question is: */

/*           You have to do a billion separate checks because the IOSTAT */
/*           value is only valid for the most recently executed write. */

/*        Added the following error message to the routine: */

/*           C     2) If for some reason the ID word cannot be read from */
/*           C        the DAF file, the error SPICE(DAFREADFAIL) will be */
/*           C        signaled. */

/*        because the file ID word is now read from the binary DAF file */
/*        rather than being hard coded as 'NAIF/DAF' in this routine. */

/*        Added a statement to the $Particulars section to the effect */
/*        that this routine has been made obsolete by the introduction of */
/*        the routine DAFBT, and that we strongly recommend the use of */
/*        the new routine. */

/*        Modified the $Abstract section to reflect the fact that this */
/*        routine is obsolete. */

/* -    SPICELIB Version 1.0.2, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.1, 22-MAR-1990 (HAN) */

/*        Literature references added to the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU) */

/* -& */
/* $ Index_Entries */

/*     DEPRECATED binary DAF to text */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     Local variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("DAFB2T", (ftnlen)6);
    }

/*     Initialize the IDWORD. */

    s_copy(idword, " ", (ftnlen)8, (ftnlen)1);

/*     Open the binary file for reading and read the ID word from the */
/*     first record of the file. */

    dafopr_(binary, &handle, binary_len);
    if (failed_()) {
	chkout_("DAFB2T", (ftnlen)6);
	return 0;
    }

/*     At this point, we know that we have a DAF file, because we were */
/*     able to successfully open it, so we will attempt to proceed with */
/*     the file conversion process. */

/*     Convert the DAF file handle to its equivalent Fortran logical */
/*     unit. We need to do this in order to accurately move the file */
/*     ID word to the text file. */

    zzddhhlu_(&handle, "DAF", &c_false, &daflun, (ftnlen)3);
    if (failed_()) {
	chkout_("DAFB2T", (ftnlen)6);
	return 0;
    }
    io___5.ciunit = daflun;
    iostat = s_rdue(&io___5);
    if (iostat != 0) {
	goto L100001;
    }
    iostat = do_uio(&c__1, idword, (ftnlen)8);
    if (iostat != 0) {
	goto L100001;
    }
    iostat = e_rdue();
L100001:
    if (iostat != 0) {
	setmsg_("Could not read ID word from file '#'. IOSTAT = #.", (ftnlen)
		49);
	errch_("#", binary, (ftnlen)1, binary_len);
	errint_("#", &iostat, (ftnlen)1);
	sigerr_("SPICE(DAFREADFAIL)", (ftnlen)18);
	chkout_("DAFB2T", (ftnlen)6);
	return 0;
    }

/*     Get the contents of the file record. The ASCII file begins */
/*     with the ID word which is followed by the summary format, */
/*     which is followed by the internal file name. */

    dafrfr_(&handle, &nd, &ni, ifname, &fward, &bward, &free, (ftnlen)60);
    if (failed_()) {
	chkout_("DAFB2T", (ftnlen)6);
	return 0;
    }
    io___12.ciunit = *text;
    iostat = s_wsle(&io___12);
    if (iostat != 0) {
	goto L100002;
    }
/* Writing concatenation */
    i__1[0] = 1, a__1[0] = "'";
    i__1[1] = 8, a__1[1] = idword;
    i__1[2] = 1, a__1[2] = "'";
    s_cat(ch__1, a__1, i__1, &c__3, (ftnlen)10);
    iostat = do_lio(&c__9, &c__1, ch__1, (ftnlen)10);
    if (iostat != 0) {
	goto L100002;
    }
    iostat = e_wsle();
L100002:
    if (iostat != 0) {
	dafcls_(&handle);
	setmsg_("The attempt to write to file '#' failed. IOSTAT = #.", (
		ftnlen)52);
	errfnm_("#", text, (ftnlen)1);
	errint_("#", &iostat, (ftnlen)1);
	sigerr_("SPICE(DAFWRITEFAIL)", (ftnlen)19);
	chkout_("DAFB2T", (ftnlen)6);
	return 0;
    }
    io___13.ciunit = *text;
    iostat = s_wsle(&io___13);
    if (iostat != 0) {
	goto L100003;
    }
    iostat = do_lio(&c__3, &c__1, (char *)&nd, (ftnlen)sizeof(integer));
    if (iostat != 0) {
	goto L100003;
    }
    iostat = e_wsle();
L100003:
    if (iostat != 0) {
	dafcls_(&handle);
	setmsg_("The attempt to write to file '#' failed. IOSTAT = #.", (
		ftnlen)52);
	errfnm_("#", text, (ftnlen)1);
	errint_("#", &iostat, (ftnlen)1);
	sigerr_("SPICE(DAFWRITEFAIL)", (ftnlen)19);
	chkout_("DAFB2T", (ftnlen)6);
	return 0;
    }
    io___14.ciunit = *text;
    iostat = s_wsle(&io___14);
    if (iostat != 0) {
	goto L100004;
    }
    iostat = do_lio(&c__3, &c__1, (char *)&ni, (ftnlen)sizeof(integer));
    if (iostat != 0) {
	goto L100004;
    }
    iostat = e_wsle();
L100004:
    if (iostat != 0) {
	dafcls_(&handle);
	setmsg_("The attempt to write to file '#' failed. IOSTAT = #.", (
		ftnlen)52);
	errfnm_("#", text, (ftnlen)1);
	errint_("#", &iostat, (ftnlen)1);
	sigerr_("SPICE(DAFWRITEFAIL)", (ftnlen)19);
	chkout_("DAFB2T", (ftnlen)6);
	return 0;
    }
    io___15.ciunit = *text;
    iostat = s_wsle(&io___15);
    if (iostat != 0) {
	goto L100005;
    }
/* Writing concatenation */
    i__1[0] = 1, a__1[0] = "'";
    i__1[1] = 60, a__1[1] = ifname;
    i__1[2] = 1, a__1[2] = "'";
    s_cat(ch__2, a__1, i__1, &c__3, (ftnlen)62);
    iostat = do_lio(&c__9, &c__1, ch__2, (ftnlen)62);
    if (iostat != 0) {
	goto L100005;
    }
    iostat = e_wsle();
L100005:
    if (iostat != 0) {
	dafcls_(&handle);
	setmsg_("The attempt to write to file '#' failed. IOSTAT = #.", (
		ftnlen)52);
	errfnm_("#", text, (ftnlen)1);
	errint_("#", &iostat, (ftnlen)1);
	sigerr_("SPICE(DAFWRITEFAIL)", (ftnlen)19);
	chkout_("DAFB2T", (ftnlen)6);
	return 0;
    }

/*     Each array is preceded by a '1', which indicates that more */
/*     arrays are to come. The array itself begins with the name */
/*     and the summary components, and ends with the name again. */
/*     The elements are written in arbitrary chunks. The final */
/*     chunk is followed by a '0', which indicates that no chunks */
/*     remain. */

/*     Write the arrays in forward order. */

    lsize = nd + (ni - 1) / 2 + 1;
    isize = lsize << 3;
    dafbfs_(&handle);
    daffna_(&found);
    if (failed_()) {
	chkout_("DAFB2T", (ftnlen)6);
	return 0;
    }
    while(found) {
	dafgs_(sum);
	dafgn_(name__, (ftnlen)1000);
	dafus_(sum, &nd, &ni, dc, ic);
	if (failed_()) {
	    chkout_("DAFB2T", (ftnlen)6);
	    return 0;
	}
	io___23.ciunit = *text;
	iostat = s_wsle(&io___23);
	if (iostat != 0) {
	    goto L100006;
	}
	iostat = do_lio(&c__9, &c__1, "1", (ftnlen)1);
	if (iostat != 0) {
	    goto L100006;
	}
	iostat = e_wsle();
L100006:
	if (iostat != 0) {
	    dafcls_(&handle);
	    setmsg_("The attempt to write to file '#' failed. IOSTAT = #.", (
		    ftnlen)52);
	    errfnm_("#", text, (ftnlen)1);
	    errint_("#", &iostat, (ftnlen)1);
	    sigerr_("SPICE(DAFWRITEFAIL)", (ftnlen)19);
	    chkout_("DAFB2T", (ftnlen)6);
	    return 0;
	}
	io___24.ciunit = *text;
	iostat = s_wsle(&io___24);
	if (iostat != 0) {
	    goto L100007;
	}
/* Writing concatenation */
	i__1[0] = 1, a__1[0] = "'";
	i__1[1] = isize, a__1[1] = name__;
	i__1[2] = 1, a__1[2] = "'";
	s_cat(ch__3, a__1, i__1, &c__3, (ftnlen)1002);
	iostat = do_lio(&c__9, &c__1, ch__3, isize + 2);
	if (iostat != 0) {
	    goto L100007;
	}
	iostat = e_wsle();
L100007:
	if (iostat != 0) {
	    dafcls_(&handle);
	    setmsg_("The attempt to write to file '#' failed. IOSTAT = #.", (
		    ftnlen)52);
	    errfnm_("#", text, (ftnlen)1);
	    errint_("#", &iostat, (ftnlen)1);
	    sigerr_("SPICE(DAFWRITEFAIL)", (ftnlen)19);
	    chkout_("DAFB2T", (ftnlen)6);
	    return 0;
	}
	io___25.ciunit = *text;
	iostat = s_wsle(&io___25);
	if (iostat != 0) {
	    goto L100008;
	}
	i__2 = nd;
	for (i__ = 1; i__ <= i__2; ++i__) {
	    iostat = do_lio(&c__5, &c__1, (char *)&dc[(i__3 = i__ - 1) < 125 
		    && 0 <= i__3 ? i__3 : s_rnge("dc", i__3, "dafb2t_", (
		    ftnlen)540)], (ftnlen)sizeof(doublereal));
	    if (iostat != 0) {
		goto L100008;
	    }
	}
	iostat = e_wsle();
L100008:
	if (iostat != 0) {
	    dafcls_(&handle);
	    setmsg_("The attempt to write to file '#' failed. IOSTAT = #.", (
		    ftnlen)52);
	    errfnm_("#", text, (ftnlen)1);
	    errint_("#", &iostat, (ftnlen)1);
	    sigerr_("SPICE(DAFWRITEFAIL)", (ftnlen)19);
	    chkout_("DAFB2T", (ftnlen)6);
	    return 0;
	}
	io___27.ciunit = *text;
	iostat = s_wsle(&io___27);
	if (iostat != 0) {
	    goto L100009;
	}
	i__3 = ni - 2;
	for (i__ = 1; i__ <= i__3; ++i__) {
	    iostat = do_lio(&c__3, &c__1, (char *)&ic[(i__2 = i__ - 1) < 250 
		    && 0 <= i__2 ? i__2 : s_rnge("ic", i__2, "dafb2t_", (
		    ftnlen)555)], (ftnlen)sizeof(integer));
	    if (iostat != 0) {
		goto L100009;
	    }
	}
	iostat = e_wsle();
L100009:
	if (iostat != 0) {
	    dafcls_(&handle);
	    setmsg_("The attempt to write to file '#' failed. IOSTAT = #.", (
		    ftnlen)52);
	    errfnm_("#", text, (ftnlen)1);
	    errint_("#", &iostat, (ftnlen)1);
	    sigerr_("SPICE(DAFWRITEFAIL)", (ftnlen)19);
	    chkout_("DAFB2T", (ftnlen)6);
	    return 0;
	}
	begin = ic[(i__2 = ni - 2) < 250 && 0 <= i__2 ? i__2 : s_rnge("ic", 
		i__2, "dafb2t_", (ftnlen)570)];
	end = ic[(i__2 = ni - 1) < 250 && 0 <= i__2 ? i__2 : s_rnge("ic", 
		i__2, "dafb2t_", (ftnlen)571)];
	while(begin <= end) {
/* Computing MIN */
	    i__2 = begin + 99;
	    chunk = min(i__2,end);
	    csize = chunk - begin + 1;
	    dafgda_(&handle, &begin, &chunk, buffer);
	    if (failed_()) {
		chkout_("DAFB2T", (ftnlen)6);
		return 0;
	    }
	    io___33.ciunit = *text;
	    iostat = s_wsle(&io___33);
	    if (iostat != 0) {
		goto L100010;
	    }
	    iostat = do_lio(&c__3, &c__1, (char *)&csize, (ftnlen)sizeof(
		    integer));
	    if (iostat != 0) {
		goto L100010;
	    }
	    iostat = e_wsle();
L100010:
	    if (iostat != 0) {
		dafcls_(&handle);
		setmsg_("The attempt to write to file '#' failed. IOSTAT = #."
			, (ftnlen)52);
		errfnm_("#", text, (ftnlen)1);
		errint_("#", &iostat, (ftnlen)1);
		sigerr_("SPICE(DAFWRITEFAIL)", (ftnlen)19);
		chkout_("DAFB2T", (ftnlen)6);
		return 0;
	    }
	    io___34.ciunit = *text;
	    iostat = s_wsle(&io___34);
	    if (iostat != 0) {
		goto L100011;
	    }
	    i__2 = csize;
	    for (i__ = 1; i__ <= i__2; ++i__) {
		iostat = do_lio(&c__5, &c__1, (char *)&buffer[(i__3 = i__ - 1)
			 < 100 && 0 <= i__3 ? i__3 : s_rnge("buffer", i__3, 
			"dafb2t_", (ftnlen)602)], (ftnlen)sizeof(doublereal));
		if (iostat != 0) {
		    goto L100011;
		}
	    }
	    iostat = e_wsle();
L100011:
	    if (iostat != 0) {
		dafcls_(&handle);
		setmsg_("The attempt to write to file '#' failed. IOSTAT = #."
			, (ftnlen)52);
		errfnm_("#", text, (ftnlen)1);
		errint_("#", &iostat, (ftnlen)1);
		sigerr_("SPICE(DAFWRITEFAIL)", (ftnlen)19);
		chkout_("DAFB2T", (ftnlen)6);
		return 0;
	    }
	    begin += 100;
	}
	io___35.ciunit = *text;
	iostat = s_wsle(&io___35);
	if (iostat != 0) {
	    goto L100012;
	}
	iostat = do_lio(&c__9, &c__1, "0", (ftnlen)1);
	if (iostat != 0) {
	    goto L100012;
	}
	iostat = e_wsle();
L100012:
	if (iostat != 0) {
	    dafcls_(&handle);
	    setmsg_("The attempt to write to file '#' failed. IOSTAT = #.", (
		    ftnlen)52);
	    errfnm_("#", text, (ftnlen)1);
	    errint_("#", &iostat, (ftnlen)1);
	    sigerr_("SPICE(DAFWRITEFAIL)", (ftnlen)19);
	    chkout_("DAFB2T", (ftnlen)6);
	    return 0;
	}
	io___36.ciunit = *text;
	iostat = s_wsle(&io___36);
	if (iostat != 0) {
	    goto L100013;
	}
/* Writing concatenation */
	i__1[0] = 1, a__1[0] = "'";
	i__1[1] = isize, a__1[1] = name__;
	i__1[2] = 1, a__1[2] = "'";
	s_cat(ch__3, a__1, i__1, &c__3, (ftnlen)1002);
	iostat = do_lio(&c__9, &c__1, ch__3, isize + 2);
	if (iostat != 0) {
	    goto L100013;
	}
	iostat = e_wsle();
L100013:
	if (iostat != 0) {
	    dafcls_(&handle);
	    setmsg_("The attempt to write to file '#' failed. IOSTAT = #.", (
		    ftnlen)52);
	    errfnm_("#", text, (ftnlen)1);
	    errint_("#", &iostat, (ftnlen)1);
	    sigerr_("SPICE(DAFWRITEFAIL)", (ftnlen)19);
	    chkout_("DAFB2T", (ftnlen)6);
	    return 0;
	}
	daffna_(&found);
	if (failed_()) {
	    chkout_("DAFB2T", (ftnlen)6);
	    return 0;
	}
    }

/*     A final '0' indicates that no arrays remain. The first shall be */
/*     last: the internal file name brings up the rear. */

    io___37.ciunit = *text;
    iostat = s_wsle(&io___37);
    if (iostat != 0) {
	goto L100014;
    }
    iostat = do_lio(&c__9, &c__1, "0", (ftnlen)1);
    if (iostat != 0) {
	goto L100014;
    }
    iostat = e_wsle();
L100014:
    if (iostat != 0) {
	dafcls_(&handle);
	setmsg_("The attempt to write to file '#' failed. IOSTAT = #.", (
		ftnlen)52);
	errfnm_("#", text, (ftnlen)1);
	errint_("#", &iostat, (ftnlen)1);
	sigerr_("SPICE(DAFWRITEFAIL)", (ftnlen)19);
	chkout_("DAFB2T", (ftnlen)6);
	return 0;
    }
    io___38.ciunit = *text;
    iostat = s_wsle(&io___38);
    if (iostat != 0) {
	goto L100015;
    }
/* Writing concatenation */
    i__1[0] = 1, a__1[0] = "'";
    i__1[1] = 60, a__1[1] = ifname;
    i__1[2] = 1, a__1[2] = "'";
    s_cat(ch__2, a__1, i__1, &c__3, (ftnlen)62);
    iostat = do_lio(&c__9, &c__1, ch__2, (ftnlen)62);
    if (iostat != 0) {
	goto L100015;
    }
    iostat = e_wsle();
L100015:
    if (iostat != 0) {
	dafcls_(&handle);
	setmsg_("The attempt to write to file '#' failed. IOSTAT = #.", (
		ftnlen)52);
	errfnm_("#", text, (ftnlen)1);
	errint_("#", &iostat, (ftnlen)1);
	sigerr_("SPICE(DAFWRITEFAIL)", (ftnlen)19);
	chkout_("DAFB2T", (ftnlen)6);
	return 0;
    }

/*     Close only the binary file. */

    dafcls_(&handle);
    chkout_("DAFB2T", (ftnlen)6);
    return 0;
} /* dafb2t_ */

