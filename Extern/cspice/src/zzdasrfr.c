/* zzdasrfr.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static logical c_false = FALSE_;
static integer c__2 = 2;
static integer c__1 = 1;

/* $Procedure      ZZDASRFR ( DAS, read file record ) */
/* Subroutine */ int zzdasrfr_(integer *handle, char *idword, char *ifname, 
	integer *nresvr, integer *nresvc, integer *ncomr, integer *ncomc, 
	ftnlen idword_len, ftnlen ifname_len)
{
    /* Initialized data */

    static logical first = TRUE_;
    static integer natbff = -1;

    /* Builtin functions */
    integer s_rdue(cilist *), do_uio(integer *, char *, ftnlen), e_rdue(void);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    integer ibff, unit;
    extern /* Subroutine */ int zzddhnfc_(integer *), zzddhppf_(integer *, 
	    integer *, integer *), zzddhhlu_(integer *, char *, logical *, 
	    integer *, ftnlen), zzxlatei_(integer *, char *, integer *, 
	    integer *, ftnlen), chkin_(char *, ftnlen);
    extern logical failed_(void);
    char chrbuf[1024];
    extern /* Subroutine */ int errfnm_(char *, integer *, ftnlen), sigerr_(
	    char *, ftnlen);
    char tmpifn[60];
    extern /* Subroutine */ int chkout_(char *, ftnlen), setmsg_(char *, 
	    ftnlen);
    integer iostat;
    char tmpidw[8];
    extern /* Subroutine */ int errint_(char *, integer *, ftnlen);
    extern logical return_(void);

    /* Fortran I/O blocks */
    static cilist io___6 = { 1, 0, 1, 0, 1 };
    static cilist io___9 = { 1, 0, 1, 0, 1 };


/* $ Abstract */

/*     Return the contents of the file record of a specified DAS file. */

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

/*     DAS */
/*     FILES */
/*     UTILITY */

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

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     HANDLE     I   DAS file handle. */
/*     IDWORD     O   ID word. */
/*     IFNAME     O   DAS internal file name. */
/*     NRESVR     O   Number of reserved records in file. */
/*     NRESVC     O   Number of characters in use in reserved rec. area. */
/*     NCOMR      O   Number of comment records in file. */
/*     NCOMC      O   Number of characters in use in comment area. */

/* $ Detailed_Input */

/*     HANDLE         is a file handle for a previously opened DAS file. */

/* $ Detailed_Output */

/*     IDWORD      is the `ID word' contained in the first eight */
/*                 characters of the file record. */

/*     IFNAME      is the internal file name of the DAS file.  The */
/*                 maximum length of the internal file name is 60 */
/*                 characters. */

/*     NRESVR      is the number of reserved records in the DAS file */
/*                 specified by HANDLE. */

/*     NRESVC      is the number of characters in use in the reserved */
/*                 record area of the DAS file specified by HANDLE. */

/*     NCOMR       is the number of comment records in the DAS file */
/*                 specified by HANDLE. */

/*     NCOMC       is the number of characters in use in the comment area */
/*                 of the DAS file specified by HANDLE. */


/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1) If the file read attempted by this routine fails, the error */
/*        SPICE(DASFILEREADFAILED) will be signaled. */

/*     2) If the input file handle is invalid, the error will diagnosed */
/*        by a routine in the call tree of this routine. */

/*     3) If a logical unit cannot be obtained for the file designated */
/*        by HANDLE, the error will diagnosed by a routine in the call */
/*        tree of this routine. */

/*     4) If the file's binary format is unrecognized, the error will */
/*        diagnosed by a routine in the call tree of this routine. */

/*     5) If the file designated by HANDLE has non-native binary format, */
/*        and if any numeric components of the file record cannot be */
/*        translated to native format, the error will diagnosed */
/*        by a routine in the call tree of this routine. */

/* $ Files */

/*     See the description of HANDLE under $Detailed_Input. */

/* $ Particulars */

/*     This routine provides a convenient way of retrieving the */
/*     information contained in the file record of a DAS file. */

/* $ Examples */

/*     1)  Obtain the internal file name of an existing DAS file. */


/*            C */
/*            C     Open the file for reading. */
/*            C */
/*                  CALL DASOPR ( FNAME, HANDLE  ) */

/*            C */
/*            C     Retrieve the internal file name and print it. */
/*            C */

/*                  CALL ZZDASRFR ( HANDLE, */
/*                 .                IDWORD, */
/*                 .                IFNAME, */
/*                 .                NRESVR, */
/*                 .                NRESVC, */
/*                 .                NCOMR, */
/*                 .                NCOMC  ) */


/*                  WRITE (*,*) 'Internal file name is: ' */
/*                  WRITE (*,*)  IFNAME */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     K.R. Gehringer (JPL) */
/*     N.J. Bachman   (JPL) */
/*     W.L. Taber     (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 05-FEB-2015 (NJB) */

/*        Based on DASRFR Version 1.0.0, 15-JUL-1992 (NJB) (WLT) */

/* -& */
/* $ Index_Entries */

/*     read DAS file record */
/*     read DAS internal file name */

/* -& */
/* $ Revisions */

/*     None. */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     Parameters for positions of file record elements: */


/*     ID word begin and end: */


/*     Internal file name begin and end: */


/*     Reserved record count begin and end: */


/*     Reserved record character count begin and end: */


/*     Comment area record count begin and end: */


/*     Comment area character count begin and end: */


/*     Local variables */


/*     Saved variables */


/*     Initial values */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }
    chkin_("ZZDASRFR", (ftnlen)8);

/*     On the first pass through this routine, get the integer code of */
/*     the host system's native binary file format. */

    if (first) {
	zzddhnfc_(&natbff);
	if (failed_()) {
	    chkout_("ZZDASRFR", (ftnlen)8);
	    return 0;
	}
	first = FALSE_;
    }

/*     Get a logical unit for this DAS file. */

    zzddhhlu_(handle, "DAS", &c_false, &unit, (ftnlen)3);

/*     Get the integer code for the file's binary format. */

    zzddhppf_(&unit, &c__2, &ibff);
    if (failed_()) {
	chkout_("ZZDASRFR", (ftnlen)8);
	return 0;
    }
    if (ibff == natbff) {

/*        We're looking at a native file. Just read the file record. */

	io___6.ciunit = unit;
	iostat = s_rdue(&io___6);
	if (iostat != 0) {
	    goto L100001;
	}
	iostat = do_uio(&c__1, tmpidw, (ftnlen)8);
	if (iostat != 0) {
	    goto L100001;
	}
	iostat = do_uio(&c__1, tmpifn, (ftnlen)60);
	if (iostat != 0) {
	    goto L100001;
	}
	iostat = do_uio(&c__1, (char *)&(*nresvr), (ftnlen)sizeof(integer));
	if (iostat != 0) {
	    goto L100001;
	}
	iostat = do_uio(&c__1, (char *)&(*nresvc), (ftnlen)sizeof(integer));
	if (iostat != 0) {
	    goto L100001;
	}
	iostat = do_uio(&c__1, (char *)&(*ncomr), (ftnlen)sizeof(integer));
	if (iostat != 0) {
	    goto L100001;
	}
	iostat = do_uio(&c__1, (char *)&(*ncomc), (ftnlen)sizeof(integer));
	if (iostat != 0) {
	    goto L100001;
	}
	iostat = e_rdue();
L100001:
	if (iostat != 0) {
	    setmsg_("Could not DAS read file record. File was #.  IOSTAT was"
		    " #.", (ftnlen)58);
	    errfnm_("#", &unit, (ftnlen)1);
	    errint_("#", &iostat, (ftnlen)1);
	    sigerr_("SPICE(DASFILEREADFAILED)", (ftnlen)24);
	    chkout_("ZZDASRFR", (ftnlen)8);
	    return 0;
	}
	s_copy(idword, tmpidw, idword_len, (ftnlen)8);
	s_copy(ifname, tmpifn, ifname_len, (ftnlen)60);
    } else {

/*        The file is non-native. */

/*        We don't check the access mode of the file, because we're */
/*        not going to reject files that are open for writing. */

/*        We'll read the file record as a character string and then */
/*        pick it apart. */

	io___9.ciunit = unit;
	iostat = s_rdue(&io___9);
	if (iostat != 0) {
	    goto L100002;
	}
	iostat = do_uio(&c__1, chrbuf, (ftnlen)1024);
	if (iostat != 0) {
	    goto L100002;
	}
	iostat = e_rdue();
L100002:
	if (iostat != 0) {
	    setmsg_("Could not read DAS file record. File is #. IOSTAT was #"
		    ". File's BFF integer code is #.", (ftnlen)86);
	    errfnm_("#", &unit, (ftnlen)1);
	    errint_("#", &iostat, (ftnlen)1);
	    errint_("#", &ibff, (ftnlen)1);
	    sigerr_("SPICE(DASFILEREADFAILED)", (ftnlen)24);
	    chkout_("ZZDASRFR", (ftnlen)8);
	    return 0;
	}

/*        Set the string output arguments. */

	s_copy(idword, chrbuf, idword_len, (ftnlen)8);
	s_copy(ifname, chrbuf + 8, ifname_len, (ftnlen)60);

/*        The integer output arguments require translation. */

	zzxlatei_(&ibff, chrbuf + 68, &c__1, nresvr, (ftnlen)4);
	zzxlatei_(&ibff, chrbuf + 72, &c__1, nresvc, (ftnlen)4);
	zzxlatei_(&ibff, chrbuf + 76, &c__1, ncomr, (ftnlen)4);
	zzxlatei_(&ibff, chrbuf + 80, &c__1, ncomc, (ftnlen)4);
    }
    chkout_("ZZDASRFR", (ftnlen)8);
    return 0;
} /* zzdasrfr_ */

