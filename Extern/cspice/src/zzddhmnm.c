/* zzddhmnm.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;
static integer c__20 = 20;

/* $Procedure ZZDDHMNM ( Return unique enough DP number for a file ) */
doublereal zzddhmnm_(integer *unit)
{
    /* Initialized data */

    static logical first = TRUE_;
    static integer natbff = 0;

    /* System generated locals */
    integer i__1;
    doublereal ret_val;

    /* Builtin functions */
    integer s_rdue(cilist *), do_uio(integer *, char *, ftnlen), e_rdue(void),
	     s_cmp(char *, char *, ftnlen, ftnlen), s_rnge(char *, integer, 
	    char *, integer);

    /* Local variables */
    char arch[8], type__[8];
    extern /* Subroutine */ int zzddhini_(integer *, integer *, integer *, 
	    char *, char *, char *, ftnlen, ftnlen, ftnlen), zzddhppf_(
	    integer *, integer *, integer *), zzxlatei_(integer *, char *, 
	    integer *, integer *, ftnlen);
    integer i__;
    extern /* Subroutine */ int chkin_(char *, ftnlen), idw2at_(char *, char *
	    , char *, ftnlen, ftnlen, ftnlen);
    extern logical failed_(void);
    extern integer isrchi_(integer *, integer *, integer *);
    char idword[8], strbff[8*4];
    static integer supbff[4];
    extern /* Subroutine */ int chkout_(char *, ftnlen);
    char stramh[8*4], strarc[8*2];
    integer intarr[20], iostat;
    char strbuf[80];
    integer supidx;
    extern logical return_(void);
    static integer numsup;
    integer bff;
    doublereal mnm;

    /* Fortran I/O blocks */
    static cilist io___10 = { 1, 0, 1, 0, 1 };
    static cilist io___18 = { 1, 0, 1, 0, 1 };
    static cilist io___20 = { 1, 0, 1, 0, 0 };


/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines. Users should not call this routine directly due */
/*     to the volatile nature of this routine. */

/*     Return a unique enough DP number ("Magic NuMber") computed */
/*     using the contents of the file attached to the specified unit. */

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
/*     UNIT       I   Logical unit attached to a file. */

/*     The function returns a unique enough DP number computed */
/*     using the contents of the file attached to the UNIT. */

/* $ Detailed_Input */

/*     UNIT        is the logical unit attached to a file opened for */
/*                 direct access prior to calling this routine. */

/* $ Detailed_Output */

/*     The function returns a DP number, computed using a few integers */
/*     read from the file attached to the UNIT, that is unique enough */
/*     for the handle manager to check if two files opened for READ */
/*     access are not the same file. */

/*     If reading the first record of the file or any of the lower level */
/*     routines called by this function fail for any reason, the */
/*     returned value is set to 0.D0. */

/* $ Parameters */

/*     IDLEN       is the length of the ID word. */

/*     NINTS       is the number of integers that will be read from */
/*                 a particular record of the file. NINTS must be */
/*                 big enough to make sure that unique pointers from */
/*                 the file record of DAF and DAS files are read and */
/*                 used to compute the output value. */

/*     The function also uses some DDH general parameters from */
/*     zzddhman.inc. */

/* $ Exceptions */

/*     Error free. */

/*     This routine and routines in its call tree signal several */
/*     SPICE(BUG) exceptions. They are signaled if the module or modules */
/*     in its calling tree are improperly configured to run on this */
/*     platform. */

/* $ Files */

/*     The input UNIT must be attached to a file opened for direct */
/*     access prior to calling this function. */

/* $ Particulars */

/*     This function reads the first IDLEN characters (assumed to be the */
/*     ID word) followed by NINTS integers from the first record of the */
/*     direct access file attached to the input UNIT. */

/*     If successful, it examines the ID word to determine the file */
/*     architecture. */

/*     For DAF files it then tries to determine the binary format. */

/*     For DAF files in the native binary format, it adds up NINTS */
/*     integers read from the first record to get initial output value. */
/*     Then it reads additional NINTS integers from the first descriptor */
/*     record and, if the second read is successful, it adds these */
/*     additional integers to the output value. */

/*     For DAF files in a non-native binary format supported by run-time */
/*     translation, it re-read NINTS integers from the first record as */
/*     characters, converts them to NINTS integers using ZZXLATEI, and */
/*     adds them up to get initial output value. Then it reads as */
/*     characters NINTS integers from the first descriptor record of the */
/*     file, converts them to NINTS integers using ZZXLATEI and ,if the */
/*     second read is successful, it adds these additional integers to */
/*     the output value. */

/*     For DAS files, text kernels and unrecognized files, it simply */
/*     adds up NINTS integers to get the output value. */

/*     If the initial read is not successful, the output value is set to */
/*     zero. */

/* $ Examples */

/*     See the caller routine, ZZDDHF2H. */

/* $ Restrictions */

/*     The input UNIT must be attached to a file opened for direct */
/*     access prior to calling this function. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     B.B. Semenov    (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 26-APR-2012 (BVS) */

/* -& */

/*     SPICELIB Functions */


/*     Local parameters. */

/*     Character buffer size consistent with the number of integers */
/*     that will be read from the file. */


/*     Minimum and maximum values for the range of ASCII printing */
/*     characters. */


/*     Local variables. */


/*     Saved variables. */


/*     Data statements. */


/*     Set default output value to zero. */

    mnm = 0.;
    ret_val = 0.;

/*     Standard SPICE error handling. */

    if (return_()) {
	return ret_val;
    } else {
	chkin_("ZZDDHMNM", (ftnlen)8);
    }

/*     Perform some initialization tasks. */

    if (first) {
	zzddhini_(&natbff, supbff, &numsup, stramh, strarc, strbff, (ftnlen)8,
		 (ftnlen)8, (ftnlen)8);

/*        Check FAILED() to handle the unlikely event that */
/*        ZZDDHINI signaled SPICE(BUG). */

	if (failed_()) {
	    chkout_("ZZDDHMNM", (ftnlen)8);
	    return ret_val;
	}

/*        Do not perform initialization tasks again. */

	first = FALSE_;
    }

/*     Read ID word string followed by NINTS integers from the first */
/*     record of the file. */

    io___10.ciunit = *unit;
    iostat = s_rdue(&io___10);
    if (iostat != 0) {
	goto L100001;
    }
    iostat = do_uio(&c__1, idword, (ftnlen)8);
    if (iostat != 0) {
	goto L100001;
    }
    iostat = do_uio(&c__20, (char *)&intarr[0], (ftnlen)sizeof(integer));
    if (iostat != 0) {
	goto L100001;
    }
    iostat = e_rdue();
L100001:
    if (iostat == 0) {

/*        Read succeeded. Try to determine the file architecture and */
/*        type from the ID word. To do this, mimic the part of GETFAT */
/*        that deals only with the ID word. First replace any non */
/*        printing ASCII characters in the ID word with blanks, then use */
/*        IDW2AT on the "cleaned" ID word to get architecture and type. */

	for (i__ = 1; i__ <= 8; ++i__) {
	    if (*(unsigned char *)&idword[i__ - 1] < 32 || *(unsigned char *)&
		    idword[i__ - 1] > 126) {
		*(unsigned char *)&idword[i__ - 1] = ' ';
	    }
	}
	idw2at_(idword, arch, type__, (ftnlen)8, (ftnlen)8, (ftnlen)8);

/*        Compute the output value based on the file architecture. */

	if (s_cmp(arch, "DAF", (ftnlen)8, (ftnlen)3) == 0) {

/*           For DAF files, try to get the file's binary format. */

	    zzddhppf_(unit, &c__1, &bff);
	    if (failed_()) {
		chkout_("ZZDDHMNM", (ftnlen)8);
		return ret_val;
	    }

/*           If the file is in a non-native format, we will need to read */
/*           the first record again, now directly as characters, and */
/*           translate these character to native integers. */

	    if (bff != natbff) {

/*              First, check if run-time translation is supported for */
/*              this BFF. This check, stolen from ZZDDHMAN, is needed */
/*              because ZZXLATEI accepts only BFFs for which translation */
/*              is guaranteed to be supported on this platform. If it */
/*              is not supported, simply get out (note that the default */
/*              return value was set to zero at the start.) */

		supidx = isrchi_(&bff, &numsup, supbff);
		if (supidx == 0) {
		    chkout_("ZZDDHMNM", (ftnlen)8);
		    return ret_val;
		}

/*              Read the first record as characters and do translation. */

		io___18.ciunit = *unit;
		iostat = s_rdue(&io___18);
		if (iostat != 0) {
		    goto L100002;
		}
		iostat = do_uio(&c__1, idword, (ftnlen)8);
		if (iostat != 0) {
		    goto L100002;
		}
		iostat = do_uio(&c__1, strbuf, (ftnlen)80);
		if (iostat != 0) {
		    goto L100002;
		}
		iostat = e_rdue();
L100002:
		zzxlatei_(&bff, strbuf, &c__20, intarr, (ftnlen)80);
		if (failed_()) {
		    chkout_("ZZDDHMNM", (ftnlen)8);
		    return ret_val;
		}
	    }

/*           Add integers from the file record to the output value. */

	    for (i__ = 1; i__ <= 20; ++i__) {
		mnm += intarr[(i__1 = i__ - 1) < 20 && 0 <= i__1 ? i__1 : 
			s_rnge("intarr", i__1, "zzddhmnm_", (ftnlen)354)];
	    }

/*           Read more integers from the start of the first descriptor */
/*           record without regard to the file's binary format and, */
/*           if successful, add them to the total. */

	    io___20.ciunit = *unit;
	    io___20.cirec = intarr[17];
	    iostat = s_rdue(&io___20);
	    if (iostat != 0) {
		goto L100003;
	    }
	    iostat = do_uio(&c__20, (char *)&intarr[0], (ftnlen)sizeof(
		    integer));
	    if (iostat != 0) {
		goto L100003;
	    }
	    iostat = e_rdue();
L100003:
	    if (iostat == 0) {
		for (i__ = 1; i__ <= 20; ++i__) {
		    mnm += intarr[(i__1 = i__ - 1) < 20 && 0 <= i__1 ? i__1 : 
			    s_rnge("intarr", i__1, "zzddhmnm_", (ftnlen)367)];
		}
	    }
	} else if (s_cmp(arch, "DAS", (ftnlen)8, (ftnlen)3) == 0) {

/*           For DAS files, for now, add up integers from the first */
/*           record to get the output value. */

	    for (i__ = 1; i__ <= 20; ++i__) {
		mnm += intarr[(i__1 = i__ - 1) < 20 && 0 <= i__1 ? i__1 : 
			s_rnge("intarr", i__1, "zzddhmnm_", (ftnlen)379)];
	    }
	} else {

/*           For all other files, add up integers from the first record */
/*           to get the output value. */

	    for (i__ = 1; i__ <= 20; ++i__) {
		mnm += intarr[(i__1 = i__ - 1) < 20 && 0 <= i__1 ? i__1 : 
			s_rnge("intarr", i__1, "zzddhmnm_", (ftnlen)389)];
	    }
	}
    } else {

/*        The read of the file record failed. Do nothing as the output */
/*        value has already been set at the start of the function. */

    }
    ret_val = mnm;
    chkout_("ZZDDHMNM", (ftnlen)8);
    return ret_val;
} /* zzddhmnm_ */

