/* zzdafgfr.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__4 = 4;
static logical c_false = FALSE_;
static integer c__1 = 1;

/* $Procedure ZZDAFGFR ( Private --- DAF Get File Record ) */
/* Subroutine */ int zzdafgfr_(integer *handle, char *idword, integer *nd, 
	integer *ni, char *ifname, integer *fward, integer *bward, integer *
	free, logical *found, ftnlen idword_len, ftnlen ifname_len)
{
    /* Initialized data */

    static logical first = TRUE_;
    static integer natbff = 0;

    /* System generated locals */
    integer i__1;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer), s_rdue(cilist *), 
	    do_uio(integer *, char *, ftnlen), e_rdue(void);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    integer ibff, iamh;
    extern /* Subroutine */ int zzddhgsd_(char *, integer *, char *, ftnlen, 
	    ftnlen), zzddhnfo_(integer *, char *, integer *, integer *, 
	    integer *, logical *, ftnlen), zzddhhlu_(integer *, char *, 
	    logical *, integer *, ftnlen), zzplatfm_(char *, char *, ftnlen, 
	    ftnlen), zzxlatei_(integer *, char *, integer *, integer *, 
	    ftnlen);
    integer i__;
    char fname[255];
    integer iarch;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    integer locnd;
    extern /* Subroutine */ int ucase_(char *, char *, ftnlen, ftnlen);
    integer locni;
    extern /* Subroutine */ int errch_(char *, char *, ftnlen, ftnlen);
    extern logical failed_(void);
    logical locfnd;
    char chrbuf[1024], locifn[60];
    extern integer isrchc_(char *, integer *, char *, ftnlen, ftnlen);
    integer cindex, locbwd;
    char locidw[8];
    integer locfre;
    static char strbff[8*4];
    integer locfwd;
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), setmsg_(char *, ftnlen);
    integer iostat;
    extern /* Subroutine */ int errint_(char *, integer *, ftnlen);
    extern logical return_(void);
    char tmpstr[8];
    integer lun;

    /* Fortran I/O blocks */
    static cilist io___13 = { 1, 0, 1, 0, 1 };
    static cilist io___21 = { 1, 0, 1, 0, 1 };


/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines.  Users should not call this routine directly due */
/*     to the volatile nature of this routine. */

/*     Read the contents of the file record of a DAF. */

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

/*     PRIVATE */

/* $ Declarations */

/* $ Abstract */

/*     Parameter declarations for the DAF/DAS handle manager. */

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

/*     DAF, DAS */

/* $ Keywords */

/*     PRIVATE */

/* $ Particulars */

/*     This include file contains parameters defining limits and */
/*     integer codes that are utilized in the DAF/DAS handle manager */
/*     routines. */

/* $ Restrictions */

/*     None. */

/* $ Author_and_Institution */

/*     F.S. Turner       (JPL) */

/* $ Literature_References */

/*     None. */

/* $ Version */

/* -    SPICELIB Version 2.6.0, 28-NOV-2021 (BVS) */

/*        Updated for MAC-OSX-M1-64BIT-CLANG_C. */

/* -    SPICELIB Version 2.5.0, 10-MAR-2014 (BVS) */

/*        Updated for SUN-SOLARIS-64BIT-INTEL. */

/* -    SPICELIB Version 2.4.0, 10-MAR-2014 (BVS) */

/*        Updated for PC-LINUX-64BIT-IFORT. */

/* -    SPICELIB Version 2.3.0, 10-MAR-2014 (BVS) */

/*        Updated for PC-CYGWIN-GFORTRAN. */

/* -    SPICELIB Version 2.2.0, 10-MAR-2014 (BVS) */

/*        Updated for PC-CYGWIN-64BIT-GFORTRAN. */

/* -    SPICELIB Version 2.1.0, 10-MAR-2014 (BVS) */

/*        Updated for PC-CYGWIN-64BIT-GCC_C. */

/* -    SPICELIB Version 2.0.0, 12-APR-2012 (BVS) */

/*        Increased FTSIZE (from 1000 to 5000). */

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

/* -    SPICELIB Version 1.3.0, 26-OCT-2005 (BVS) */

/*        Updated for SUN-SOLARIS-64BIT-GCC_C. */

/* -    SPICELIB Version 1.2.0, 03-JAN-2005 (BVS) */

/*        Updated for PC-CYGWIN_C. */

/* -    SPICELIB Version 1.1.0, 03-JAN-2005 (BVS) */

/*        Updated for PC-CYGWIN. */

/* -    SPICELIB Version 1.0.1, 17-JUL-2002 */

/*        Added MAC-OSX environments. */

/* -    SPICELIB Version 1.0.0, 07-NOV-2001 */

/* -& */

/*     Unit and file table size parameters. */

/*     FTSIZE     is the maximum number of files (DAS and DAF) that a */
/*                user may have open simultaneously. */


/*     RSVUNT     is the number of units protected from being locked */
/*                to a particular handle by ZZDDHHLU. */


/*     SCRUNT     is the number of units protected for use by scratch */
/*                files. */


/*     UTSIZE     is the maximum number of logical units this manager */
/*                will utilize at one time. */


/*     Access method enumeration.  These parameters are used to */
/*     identify which access method is associated with a particular */
/*     handle.  They need to be synchronized with the STRAMH array */
/*     defined in ZZDDHGSD in the following fashion: */

/*        STRAMH ( READ   ) = 'READ' */
/*        STRAMH ( WRITE  ) = 'WRITE' */
/*        STRAMH ( SCRTCH ) = 'SCRATCH' */
/*        STRAMH ( NEW    ) = 'NEW' */

/*     These values are used in the file table variable FTAMH. */


/*     Binary file format enumeration.  These parameters are used to */
/*     identify which binary file format is associated with a */
/*     particular handle.  They need to be synchronized with the STRBFF */
/*     array defined in ZZDDHGSD in the following fashion: */

/*        STRBFF ( BIGI3E ) = 'BIG-IEEE' */
/*        STRBFF ( LTLI3E ) = 'LTL-IEEE' */
/*        STRBFF ( VAXGFL ) = 'VAX-GFLT' */
/*        STRBFF ( VAXDFL ) = 'VAX-DFLT' */

/*     These values are used in the file table variable FTBFF. */


/*     Some random string lengths... more documentation required. */
/*     For now this will have to suffice. */


/*     Architecture enumeration.  These parameters are used to identify */
/*     which file architecture is associated with a particular handle. */
/*     They need to be synchronized with the STRARC array defined in */
/*     ZZDDHGSD in the following fashion: */

/*        STRARC ( DAF ) = 'DAF' */
/*        STRARC ( DAS ) = 'DAS' */

/*     These values will be used in the file table variable FTARC. */


/*     For the following environments, record length is measured in */
/*     characters (bytes) with eight characters per double precision */
/*     number. */

/*     Environment: Sun, Sun FORTRAN */
/*     Source:      Sun Fortran Programmer's Guide */

/*     Environment: PC, MS FORTRAN */
/*     Source:      Microsoft Fortran Optimizing Compiler User's Guide */

/*     Environment: Macintosh, Language Systems FORTRAN */
/*     Source:      Language Systems FORTRAN Reference Manual, */
/*                  Version 1.2, page 12-7 */

/*     Environment: PC/Linux, g77 */
/*     Source:      Determined by experiment. */

/*     Environment: PC, Lahey F77 EM/32 Version 4.0 */
/*     Source:      Lahey F77 EM/32 Language Reference Manual, */
/*                  page 144 */

/*     Environment: HP-UX 9000/750, FORTRAN/9000 Series 700 computers */
/*     Source:      FORTRAN/9000 Reference-Series 700 Computers, */
/*                  page 5-110 */

/*     Environment: NeXT Mach OS (Black Hardware), */
/*                  Absoft Fortran Version 3.2 */
/*     Source:      NAIF Program */


/*     The following parameter defines the size of a string used */
/*     to store a filenames on this target platform. */


/*     The following parameter controls the size of the character record */
/*     buffer used to read data from non-native files. */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     HANDLE     I   Handle of the DAF. */
/*     IDWORD     O   DAF ID Word that indicates file type. */
/*     ND         O   Number of double precision components in summaries. */
/*     NI         O   Number of integer components in summaries. */
/*     IFNAME     O   Internal file name. */
/*     FWARD      O   Forward list pointer. */
/*     BWARD      O   Backward list pointer. */
/*     FREE       O   Free address pointer. */
/*     FOUND      O   Logical indicating whether the record was found. */

/* $ Detailed_Input */

/*     HANDLE     is the handle associated with the DAF. */

/* $ Detailed_Output */

/*     IDWORD     is a character string identifying the architecture */
/*                and type of a SPICE binary kernel.  In this case */
/*                it will be a string identifying the type of DAF. */

/*     ND, */
/*     NI         are the number of double precision and integer */
/*                components, respectively, in each array summary in */
/*                the specified file. */

/*     IFNAME     is the internal file name stored in the first */
/*                (or file) record of the specified file. */

/*     FWARD      is the forward list pointer. This points to the */
/*                first summary record in the file. (Records between */
/*                the first record and the first summary record are */
/*                reserved when the file is created, and are invisible */
/*                to DAF routines.) */

/*     BWARD      is the backward list pointer. This points */
/*                to the final summary record in the file. */

/*     FREE       is the free address pointer. This contains the */
/*                first free address in the file. (That is, the */
/*                initial address of the next array to be added */
/*                to the file.) */

/*     FOUND      is TRUE when the file record is found, and is */
/*                FALSE otherwise. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1) SPICE(HANDLENOTFOUND) is signaled if HANDLE can not be */
/*        found in the set of loaded handles.  The output arguments */
/*        are unmodified when this error occurs. */

/*     2) Routines in the call tree of this routine may trap and */
/*        signal errors.  The output arguments are unmodified in */
/*        these cases. */

/* $ Files */

/*     This routine reads data from the DAF associated with HANDLE. */
/*     This action may result in connecting a logical unit to the */
/*     file, if the handle manager has rotated the file out of the */
/*     unit table. */

/* $ Particulars */

/*     This routine reads the publicly available components of */
/*     file records from native and supported non-native DAFs. */

/*     The size of the character buffer and the number of records */
/*     read may have to change to support new environments.  As of */
/*     the original release of this routine, all systems currently */
/*     supported have a 1 kilobyte record length. */

/* $ Examples */

/*     See DAFRFR for sample usage. */

/* $ Restrictions */

/*     1) Numeric data when read as characters from a file preserves */
/*        the bit patterns present in the file in memory. */

/*     2) A record of double precision data is at most 1024 characters */
/*        in length. */

/*     3) Future updates to this module must preserve the fact that */
/*        FOUND is returned as FALSE whenever an error occurs.  An */
/*        incompletely translated or extracted file record is NOT */
/*        FOUND. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     F.S. Turner     (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.2, 03-OCT-2021 (NJB) */

/*        Corrected typo in comments. */

/* -    SPICELIB Version 1.0.1, 24-JUL-2014 (NJB) */

/*        Corrected typo in Procedure line of header. */
/*        Re-ordered header sections. */

/* -    SPICELIB Version 1.0.0, 12-NOV-2001 (FST) */


/* -& */

/*     SPICELIB Functions */


/*     Local Parameters */

/*     Record Number of the file record in a DAF. */


/*     Length of the IDWORD string. */


/*     Length of the internal filename string. */


/*     Starting location in bytes of the internal filename in the */
/*     file record. */


/*     Size of an integer in bytes. */


/*     Local Variables */


/*     Saved Variables */


/*     Data Statements */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("ZZDAFGFR", (ftnlen)8);
    }

/*     Perform some initialization tasks. */

    if (first) {

/*        Populate STRBFF, the buffer that contains the labels */
/*        for each binary file format. */

	for (i__ = 1; i__ <= 4; ++i__) {
	    zzddhgsd_("BFF", &i__, strbff + (((i__1 = i__ - 1) < 4 && 0 <= 
		    i__1 ? i__1 : s_rnge("strbff", i__1, "zzdafgfr_", (ftnlen)
		    284)) << 3), (ftnlen)3, (ftnlen)8);
	}

/*        Fetch the native binary file format and determine its */
/*        integer code. */

	zzplatfm_("FILE_FORMAT", tmpstr, (ftnlen)11, (ftnlen)8);
	ucase_(tmpstr, tmpstr, (ftnlen)8, (ftnlen)8);
	natbff = isrchc_(tmpstr, &c__4, strbff, (ftnlen)8, (ftnlen)8);
	if (natbff == 0) {
	    setmsg_("The binary file format, '#', is not supported by this v"
		    "ersion of the toolkit. This is a serious problem, contac"
		    "t NAIF.", (ftnlen)118);
	    errch_("#", tmpstr, (ftnlen)1, (ftnlen)8);
	    sigerr_("SPICE(BUG)", (ftnlen)10);
	    chkout_("ZZDAFGFR", (ftnlen)8);
	    return 0;
	}

/*        Do not perform initialization tasks again. */

	first = FALSE_;
    }

/*     Assume the data record will not be found, until it has been read */
/*     from the file, and if necessary, successfully translated. */

    *found = FALSE_;

/*     Retrieve information regarding the file from the handle manager. */
/*     The value of IARCH is not a concern, since this is a DAF routine */
/*     all values passed into handle manager entry points will have */
/*     'DAF' as their architecture arguments. */

    zzddhnfo_(handle, fname, &iarch, &ibff, &iamh, &locfnd, (ftnlen)255);
    if (! locfnd) {
	setmsg_("Unable to locate file associated with HANDLE, #.  The most "
		"likely cause of this is the file that you are trying to read"
		" has been closed.", (ftnlen)136);
	errint_("#", handle, (ftnlen)1);
	sigerr_("SPICE(HANDLENOTFOUND)", (ftnlen)21);
	chkout_("ZZDAFGFR", (ftnlen)8);
	return 0;
    }

/*     Now get a logical unit for the handle.  Check FAILED() in */
/*     case an error occurs. */

    zzddhhlu_(handle, "DAF", &c_false, &lun, (ftnlen)3);
    if (failed_()) {
	chkout_("ZZDAFGFR", (ftnlen)8);
	return 0;
    }

/*     Branch based on whether the binary file format is native */
/*     or not.  Only supported formats can be opened by ZZDDHOPN, */
/*     so no check of IBFF is required. */

    if (ibff == natbff) {

/*        In the native case, just read the components of the file */
/*        record from the file. */

	io___13.ciunit = lun;
	iostat = s_rdue(&io___13);
	if (iostat != 0) {
	    goto L100001;
	}
	iostat = do_uio(&c__1, locidw, (ftnlen)8);
	if (iostat != 0) {
	    goto L100001;
	}
	iostat = do_uio(&c__1, (char *)&locnd, (ftnlen)sizeof(integer));
	if (iostat != 0) {
	    goto L100001;
	}
	iostat = do_uio(&c__1, (char *)&locni, (ftnlen)sizeof(integer));
	if (iostat != 0) {
	    goto L100001;
	}
	iostat = do_uio(&c__1, locifn, (ftnlen)60);
	if (iostat != 0) {
	    goto L100001;
	}
	iostat = do_uio(&c__1, (char *)&locfwd, (ftnlen)sizeof(integer));
	if (iostat != 0) {
	    goto L100001;
	}
	iostat = do_uio(&c__1, (char *)&locbwd, (ftnlen)sizeof(integer));
	if (iostat != 0) {
	    goto L100001;
	}
	iostat = do_uio(&c__1, (char *)&locfre, (ftnlen)sizeof(integer));
	if (iostat != 0) {
	    goto L100001;
	}
	iostat = e_rdue();
L100001:

/*        Since this routine does not signal any IOSTAT based */
/*        errors, return if a non-zero value is assigned to IOSTAT. */

	if (iostat != 0) {
	    chkout_("ZZDAFGFR", (ftnlen)8);
	    return 0;
	}

/*     Process the non-native binary file format case. */

    } else {

/*        Read the data record as characters. */

	io___21.ciunit = lun;
	iostat = s_rdue(&io___21);
	if (iostat != 0) {
	    goto L100002;
	}
	iostat = do_uio(&c__1, chrbuf, (ftnlen)1024);
	if (iostat != 0) {
	    goto L100002;
	}
	iostat = e_rdue();
L100002:

/*        Again, since this routine does not signal any IOSTAT */
/*        based errors, return if one occurs. */

	if (iostat != 0) {
	    chkout_("ZZDAFGFR", (ftnlen)8);
	    return 0;
	}

/*        Assign the character components of the file record. */

	s_copy(locidw, chrbuf, (ftnlen)8, (ftnlen)8);
	s_copy(locifn, chrbuf + 16, (ftnlen)60, (ftnlen)60);

/*        Convert the integer components. */

	cindex = 9;
	zzxlatei_(&ibff, chrbuf + (cindex - 1), &c__1, &locnd, (ftnlen)4);
	cindex += 4;
	zzxlatei_(&ibff, chrbuf + (cindex - 1), &c__1, &locni, (ftnlen)4);
	cindex = 77;
	zzxlatei_(&ibff, chrbuf + (cindex - 1), &c__1, &locfwd, (ftnlen)4);
	cindex += 4;
	zzxlatei_(&ibff, chrbuf + (cindex - 1), &c__1, &locbwd, (ftnlen)4);
	cindex += 4;
	zzxlatei_(&ibff, chrbuf + (cindex - 1), &c__1, &locfre, (ftnlen)4);
	if (failed_()) {
	    chkout_("ZZDAFGFR", (ftnlen)8);
	    return 0;
	}
    }

/*     Transfer the contents of the record to the output arguments */
/*     and return to the caller. */

    *found = TRUE_;
    s_copy(idword, locidw, idword_len, (ftnlen)8);
    *nd = locnd;
    *ni = locni;
    s_copy(ifname, locifn, ifname_len, (ftnlen)60);
    *fward = locfwd;
    *bward = locbwd;
    *free = locfre;
    chkout_("ZZDAFGFR", (ftnlen)8);
    return 0;
} /* zzdafgfr_ */

