/* zzascii.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;

/* $Procedure ZZASCII ( determine/verify EOL terminators in a text file ) */
/* Subroutine */ int zzascii_(char *file, char *line, logical *check, char *
	termin, ftnlen file_len, ftnlen line_len, ftnlen termin_len)
{
    /* System generated locals */
    olist o__1;
    cllist cl__1;

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer i_len(char *, ftnlen), f_open(olist *), f_clos(cllist *), s_rdue(
	    cilist *), do_uio(integer *, char *, ftnlen), e_rdue(void);

    /* Local variables */
    extern /* Subroutine */ int zzplatfm_(char *, char *, ftnlen, ftnlen);
    integer i__;
    extern /* Subroutine */ int chkin_(char *, ftnlen), errch_(char *, char *,
	     ftnlen, ftnlen);
    extern integer rtrim_(char *, ftnlen);
    extern logical eqstr_(char *, char *, ftnlen, ftnlen);
    integer maccnt, reclen;
    char native[5];
    integer number, doscnt;
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), getlun_(integer *), setmsg_(char *, ftnlen);
    integer iostat;
    extern /* Subroutine */ int errint_(char *, integer *, ftnlen);
    extern logical return_(void);
    integer unxcnt;

    /* Fortran I/O blocks */
    static cilist io___5 = { 1, 0, 1, 0, 1 };


/* $ Abstract */

/*     Returns a string indicating the line terminators of an ASCII file */
/*     and, if requested, stops execution if the terminator does match */
/*     the one that is native to the platform on which the toolkit was */
/*     compiled. */

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

/*     FILE TYPE */

/* $ Declarations */
/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     FILE       I   Name of the text file to scan. */
/*     LINE       I   The work string for file reads. */
/*     CHECK      I   Flag directing to check for mismatched EOL. */
/*     TERMIN     0   The deduced terminator ID. */

/* $ Detailed_Input */

/*     FILE       the name of the ASCII file to scan for a line */
/*                terminator */

/*     LINE       a character string of sufficient length to perform the */
/*                line reads from FILE. */

/*     CHECK      a logical flag that, if set to .TRUE., instructs this */
/*                routine to check terminator that has been determined */
/*                against the one that is native to the platform, on */
/*                which the toolkit was compiled, and to generate error */
/*                if it was not the case. If set to .FALSE., instructs */
/*                the routine to bypass the check. */

/* $ Detailed_Output */

/*     TERMIN     the terminator ID extracted from FILE. The possible */
/*                values: */

/*                'CR'    - carriage return (Mac classic) */
/*                'LF'    - line feed (Unix) */
/*                'CR-LF' - carriage return and line feed (DOS) */
/*                '?'     - unable to determine, possibly */
/*                          due to an error event */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1) A SPICE(STRINGTOOSHORT) error signals if LINE has length less */
/*        than 3. */

/*     2) A SPICE(FILEOPENFAILED) error signals if the file of interest */
/*        fails to open, i.e. IOSTAT < 0. */

/*     3) A text kernel found to contain non-native line terminators */
/*        and abort of the run was requested by causes this routine to */
/*        signal the error SPICE(INCOMPATIBLEEOL). */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     The function scans a string read from a text file to determine */
/*     the native platform of the file. The functions response is */
/*     unpredictable if it scans a binary file. */

/* $ Examples */

/*     To return EOL terminator for a given file: */

/*         CHARACTER*(5)    TERMIN */
/*         CHARACTER*(64)   LINE */

/*          ... given a file name */
/*          ... and a line long enough to hold a text string */
/*              from FILE */

/*         CALL ZZASCII( FILE, LINE, .FALSE., TERMIN ) */

/*         CALL TOSTDO( 'FOUND FILE TERMINATOR '//TERMIN ) */

/*     To stop if EOL terminator for a given file, if detected */
/*     successfully, is not native to this platform: */

/*         CHARACTER*(5)    TERMIN */
/*         CHARACTER*(64)   LINE */

/*          ... given a file name */
/*          ... and a line long enough to hold a text string */
/*              from FILE */

/*         CALL ZZASCII( FILE, LINE, .TRUE., TERMIN ) */

/*     If the EOL terminator was not native, the call will generate */
/*     SPICE(INCOMPATIBLEEOL) error. */

/* $ Restrictions */

/*     1) The terminator detection is not performed if the read from */
/*        the file fails because the file is smaller than the allocated */
/*        LINE size or for any other reason. */

/*     2) The terminator detection is not possible if the length of the */
/*        first text line in the file exceeds the length of the LINE */
/*        work space. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     E.D. Wright      (JPL) */
/*     B.V. Semenov     (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.26.0, 28-NOV-2021 (BVS) */

/*        Updated for MAC-OSX-M1-64BIT-CLANG_C. */

/* -    SPICELIB Version 1.25.0, 10-MAR-2014 (BVS) */

/*        Updated for SUN-SOLARIS-64BIT-INTEL. */

/* -    SPICELIB Version 1.24.0, 10-MAR-2014 (BVS) */

/*        Updated for PC-LINUX-64BIT-IFORT. */

/* -    SPICELIB Version 1.23.0, 10-MAR-2014 (BVS) */

/*        Updated for PC-CYGWIN-GFORTRAN. */

/* -    SPICELIB Version 1.22.0, 10-MAR-2014 (BVS) */

/*        Updated for PC-CYGWIN-64BIT-GFORTRAN. */

/* -    SPICELIB Version 1.21.0, 10-MAR-2014 (BVS) */

/*        Updated for PC-CYGWIN-64BIT-GCC_C. */

/* -    SPICELIB Version 1.20.0, 13-MAY-2010 (BVS) */

/*        Updated for SUN-SOLARIS-INTEL. */

/* -    SPICELIB Version 1.19.0, 13-MAY-2010 (BVS) */

/*        Updated for SUN-SOLARIS-INTEL-CC_C. */

/* -    SPICELIB Version 1.18.0, 13-MAY-2010 (BVS) */

/*        Updated for SUN-SOLARIS-INTEL-64BIT-CC_C. */

/* -    SPICELIB Version 1.17.0, 13-MAY-2010 (BVS) */

/*        Updated for SUN-SOLARIS-64BIT-NATIVE_C. */

/* -    SPICELIB Version 1.16.0, 13-MAY-2010 (BVS) */

/*        Updated for PC-WINDOWS-64BIT-IFORT. */

/* -    SPICELIB Version 1.15.0, 13-MAY-2010 (BVS) */

/*        Updated for PC-LINUX-64BIT-GFORTRAN. */

/* -    SPICELIB Version 1.14.0, 13-MAY-2010 (BVS) */

/*        Updated for PC-64BIT-MS_C. */

/* -    SPICELIB Version 1.13.0, 13-MAY-2010 (BVS) */

/*        Updated for MAC-OSX-64BIT-INTEL_C. */

/* -    SPICELIB Version 1.12.0, 13-MAY-2010 (BVS) */

/*        Updated for MAC-OSX-64BIT-IFORT. */

/* -    SPICELIB Version 1.11.0, 13-MAY-2010 (BVS) */

/*        Updated for MAC-OSX-64BIT-GFORTRAN. */

/* -    SPICELIB Version 1.10.0, 18-MAR-2009 (BVS) */

/*        Updated for PC-LINUX-GFORTRAN. */

/* -    SPICELIB Version 1.9.0, 18-MAR-2009 (BVS) */

/*        Updated for MAC-OSX-GFORTRAN. */

/* -    SPICELIB Version 1.8.0, 19-FEB-2008 (BVS) */

/*        Updated for PC-LINUX-IFORT. */

/* -    SPICELIB Version 1.7.0, 14-NOV-2006 (BVS) */

/*        Updated for PC-LINUX-64BIT-GCC_C. */

/* -    SPICELIB Version 1.6.0, 14-NOV-2006 (BVS) */

/*        Updated for MAC-OSX-INTEL_C. */

/* -    SPICELIB Version 1.5.0, 14-NOV-2006 (BVS) */

/*        Updated for MAC-OSX-IFORT. */

/* -    SPICELIB Version 1.4.0, 14-NOV-2006 (BVS) */

/*        Updated for PC-WINDOWS-IFORT. */

/* -    SPICELIB Version 1.3.1, 26-OCT-2006 (EDW) */

/*        Expanded error message explanation the */
/*        routine outputs when the file-of-interest */
/*        includes non-native text line terminators. */

/* -    SPICELIB Version 1.3.0, 26-OCT-2005 (BVS) */

/*        Updated for SUN-SOLARIS-64BIT-GCC_C. */

/* -    SPICELIB Version 1.2.0, 03-JAN-2005 (BVS) */

/*        Updated for PC-CYGWIN_C. */

/* -    SPICELIB Version 1.1.0, 03-JAN-2005 (BVS) */

/*        Updated for PC-CYGWIN. */

/* -    SPICELIB Version 1.0.0, 17-FEB-2004 (EDW) (BVS) */

/* -& */
/* $ Index_Entries */

/*     determine ascii text file end-of-line type */

/* -& */

/*     SPICELIB functions. */


/*     Local parameters. */


/*     Local variables. */


/*     Discovery check-in. Can't determine the terminator in RETURN */
/*     mode. */

    if (return_()) {
	s_copy(termin, "?", termin_len, (ftnlen)1);
	return 0;
    }

/*     Check-in to the error system. */

    chkin_("ZZASCII", (ftnlen)7);

/*     Retrieve the native line terminator. */

    zzplatfm_("TEXT_FORMAT", native, (ftnlen)11, (ftnlen)5);

/*     If it is VAX, return immediately with undefined terminator. */

    if (eqstr_(native, "VAX", (ftnlen)5, (ftnlen)3)) {
	s_copy(termin, "?", termin_len, (ftnlen)1);
	chkout_("ZZASCII", (ftnlen)7);
	return 0;
    }

/*     Set the record length that will be used to read data from */
/*     the file. */

    reclen = i_len(line, line_len);

/*     Check the length of the work string is sufficient to perform the */
/*     operations. Less than 3 is a no-op. */

    if (i_len(line, line_len) < 3) {
	s_copy(termin, "?", termin_len, (ftnlen)1);
	setmsg_("Work string lacks sufficient length to perform operation.", (
		ftnlen)57);
	sigerr_("SPICE(STRINGTOOSHORT)", (ftnlen)21);
	chkout_("ZZASCII", (ftnlen)7);
	return 0;
    }

/*     Find a free logical unit for file access. */

    getlun_(&number);

/*     Open the file for DIRECT access. */

    o__1.oerr = 1;
    o__1.ounit = number;
    o__1.ofnmlen = rtrim_(file, file_len);
    o__1.ofnm = file;
    o__1.orl = reclen;
    o__1.osta = "OLD";
    o__1.oacc = "DIRECT";
    o__1.ofm = 0;
    o__1.oblnk = 0;
    iostat = f_open(&o__1);
    if (iostat != 0) {

/*        The open failed, can't determine the terminator if the routine */
/*        can't open the file. */

	s_copy(termin, "?", termin_len, (ftnlen)1);

/*        Execute a close, J.I.C. */

	cl__1.cerr = 0;
	cl__1.cunit = number;
	cl__1.csta = 0;
	f_clos(&cl__1);
	setmsg_("File open failed for file '$1'. IOSTAT  value $2.", (ftnlen)
		49);
	errch_("$1", file, (ftnlen)2, file_len);
	errint_("$2", &iostat, (ftnlen)2);
	sigerr_("SPICE(FILEOPENFAIL)", (ftnlen)19);
	chkout_("ZZASCII", (ftnlen)7);
	return 0;
    }

/*     Read a line into the LINE variable assigned by the user. */

    s_copy(line, " ", line_len, (ftnlen)1);
    io___5.ciunit = number;
    iostat = s_rdue(&io___5);
    if (iostat != 0) {
	goto L100001;
    }
    iostat = do_uio(&c__1, line, line_len);
    if (iostat != 0) {
	goto L100001;
    }
    iostat = e_rdue();
L100001:
    if (iostat != 0) {

/*        If something went wrong during this read, a part or the whole */
/*        returned line may contain garbage. Instead of examining it and */
/*        making wrong determination based on it, set terminator to */
/*        undefined and return. */

	s_copy(termin, "?", termin_len, (ftnlen)1);

/*        Execute a close, J.I.C. */

	cl__1.cerr = 0;
	cl__1.cunit = number;
	cl__1.csta = 0;
	f_clos(&cl__1);
	chkout_("ZZASCII", (ftnlen)7);
	return 0;
    }

/*     We have a line of text data. Use ICHAR to scan for carriage */
/*     returns and line feeds and count how may of various recognized */
/*     line termination sequences are in this line. */

    doscnt = 0;
    unxcnt = 0;
    maccnt = 0;
    i__ = 1;
    while(i__ < i_len(line, line_len)) {

/*        Check for ICHAR values of 10 (LF) and 13 (CR). */

	if (*(unsigned char *)&line[i__ - 1] == 10) {

/*           Found a UNIX line terminator LF. */

	    ++unxcnt;
	} else if (*(unsigned char *)&line[i__ - 1] == 13) {

/*           Found CR, increment character counter and check */
/*           the next character. */

	    ++i__;
	    if (*(unsigned char *)&line[i__ - 1] == 10) {

/*              Found a DOS line terminator CR+LF. */

		++doscnt;
	    } else {

/*              Found a Classic Mac line terminator CR. */

		++maccnt;
	    }
	}
	++i__;
    }

/*     Examine the counters. */

    if (doscnt > 0 && unxcnt == 0 && maccnt == 0) {

/*        Only DOS terminator counter is non-zero. ID the file as DOS. */

	s_copy(termin, "CR-LF", termin_len, (ftnlen)5);
    } else if (doscnt == 0 && unxcnt > 0 && maccnt == 0) {

/*        Only Unix terminator counter is non-zero. ID the file as UNIX. */

	s_copy(termin, "LF", termin_len, (ftnlen)2);
    } else if (doscnt == 0 && unxcnt == 0 && maccnt > 0) {

/*        Only Mac terminator counter is non-zero. ID the file as Mac */
/*        Classic. */

	s_copy(termin, "CR", termin_len, (ftnlen)2);
    } else {

/*        We can get here in two cases. First if the line did not */
/*        contain any CRs or LFs. Second if the line contained more than */
/*        one kind of terminators. In either case the format of the file */
/*        is unclear. */

	s_copy(termin, "?", termin_len, (ftnlen)1);
    }

/*     Close the file. */

    cl__1.cerr = 0;
    cl__1.cunit = number;
    cl__1.csta = 0;
    f_clos(&cl__1);

/*     If we were told check the terminator against the native one, do */
/*     it. */

    if (*check) {

/*        If the terminator was identified and does not match the native */
/*        one, error out. */

	if (! eqstr_(termin, native, termin_len, (ftnlen)5) && ! eqstr_(
		termin, "?", termin_len, (ftnlen)1)) {
	    setmsg_("Text file '$1' contains lines terminated with '$2' whil"
		    "e the expected terminator for this platform is '$3'. SPI"
		    "CE cannot process the file in the current form. This pro"
		    "blem likely occurred because the file was copied in bina"
		    "ry mode between operating systems where the operating sy"
		    "stems use different text line terminators. Try convertin"
		    "g the file to native text form using a utility such as d"
		    "os2unix or unix2dos.", (ftnlen)411);
	    errch_("$1", file, (ftnlen)2, file_len);
	    errch_("$2", termin, (ftnlen)2, termin_len);
	    errch_("$3", native, (ftnlen)2, (ftnlen)5);
	    sigerr_("SPICE(INCOMPATIBLEEOL)", (ftnlen)22);
	    chkout_("ZZASCII", (ftnlen)7);
	    return 0;
	}
    }
    chkout_("ZZASCII", (ftnlen)7);
    return 0;
} /* zzascii_ */

