/* zzddhini.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__4 = 4;

/* $Procedure ZZDDHINI ( Private --- DDH Initialize Structures ) */
/* Subroutine */ int zzddhini_(integer *natbff, integer *supbff, integer *
	numsup, char *stramh, char *strarc, char *strbff, ftnlen stramh_len, 
	ftnlen strarc_len, ftnlen strbff_len)
{
    /* Builtin functions */
    integer s_cmp(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    logical done;
    extern /* Subroutine */ int zzddhgsd_(char *, integer *, char *, ftnlen, 
	    ftnlen), zzplatfm_(char *, char *, ftnlen, ftnlen);
    integer i__;
    extern /* Subroutine */ int chkin_(char *, ftnlen), ucase_(char *, char *,
	     ftnlen, ftnlen), errch_(char *, char *, ftnlen, ftnlen);
    extern integer isrchc_(char *, integer *, char *, ftnlen, ftnlen);
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), setmsg_(char *, ftnlen), nextwd_(char *, char *, char *, 
	    ftnlen, ftnlen, ftnlen);
    char linstr[36];
    extern logical return_(void);
    char tmpstr[8];

/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines.  Users should not call this routine directly due */
/*     to the volatile nature of this routine. */

/*     Initialize ZZDDHMAN data structures. */

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
/*     NATBFF     O   Native binary file format. */
/*     SUPBFF     O   List of supported binary file formats for reading. */
/*     NUMSUP     O   Number of entries returned in SUPBFF. */
/*     STRAMH     O   List of labels for METHOD ID codes */
/*     STRARC     O   List of labels for ARCH ID codes */
/*     STRBFF     O   List of labels for BFF ID codes */

/* $ Detailed_Input */

/*     None. */

/* $ Detailed_Output */

/*     NATBFF     is an integer code for the binary file format native */
/*                to this system as described in the include file */
/*                'zzddhman.inc'.  Possible values are the parameters: */

/*                   BIGI3E */
/*                   LTLI3E */
/*                   VAXGFL */
/*                   VAXDFL */

/*     SUPBFF     is an array of integer codes for the binary file */
/*                formats supports for reading.  At the very minimum */
/*                it includes NATBFF, but potentially NUMBFF entries. */
/*                See the include file 'zzddhman.inc'. */

/*     NUMSUP     is the number of entries in the SUPBFF list. */

/*     STRAMH     is a list of strings containing the labels for the */
/*                access method ID codes defined in the include file */
/*                'zzddhman.inc'.  These values are retrieved from */
/*                ZZDDHGSD.  See it for details. */

/*     STRARC     is a list of strings containing the labels for the */
/*                file architecture ID codes defined in the include file */
/*                'zzddhman.inc'.  These values are retrieved from */
/*                ZZDDHGSD.  See it for details. */

/*     STRBFF     is a list of strings containing the labels for the */
/*                binary file format ID codes defined in the include */
/*                file 'zzddhman.inc'.  These values are retrieved from */
/*                ZZDDHGSD.  See it for details. */

/* $ Parameters */

/*     See the include file 'zzddhman.inc'. */

/* $ Exceptions */

/*     None. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine populates a variety data structures that ZZDDHMAN */
/*     requires to perform its functions. */

/* $ Examples */

/*     See ZZDDHMAN for sample usage. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     F.S. Turner     (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 03-OCT-2021 (NJB) */

/*        Corrected typo in long error message. Reordered header */
/*        sections. */

/* -    SPICELIB Version 1.0.0, 13-AUGUST-2001 (FST) */


/* -& */

/*     SPICELIB Functions */


/*     Local Variables. */


/*     Standard SPICE error handling with discovery check in/out. */

    if (return_()) {
	return 0;
    }

/*     Populate the STR### arrays. */

    for (i__ = 1; i__ <= 4; ++i__) {
	zzddhgsd_("METHOD", &i__, stramh + (i__ - 1) * stramh_len, (ftnlen)6, 
		stramh_len);
    }
    for (i__ = 1; i__ <= 2; ++i__) {
	zzddhgsd_("ARCH", &i__, strarc + (i__ - 1) * strarc_len, (ftnlen)4, 
		strarc_len);
    }
    for (i__ = 1; i__ <= 4; ++i__) {
	zzddhgsd_("BFF", &i__, strbff + (i__ - 1) * strbff_len, (ftnlen)3, 
		strbff_len);
    }

/*     Get the native binary file format. */

    zzplatfm_("FILE_FORMAT", tmpstr, (ftnlen)11, (ftnlen)8);
    ucase_(tmpstr, tmpstr, (ftnlen)8, (ftnlen)8);
    *natbff = isrchc_(tmpstr, &c__4, strbff, (ftnlen)8, strbff_len);
    if (*natbff == 0) {
	chkin_("ZZDDHINI", (ftnlen)8);
	setmsg_("The binary file format, '#', is not supported by this versi"
		"on of the toolkit. This is a serious problem, contact NAIF.", 
		(ftnlen)118);
	errch_("#", tmpstr, (ftnlen)1, (ftnlen)8);
	sigerr_("SPICE(BUG)", (ftnlen)10);
	chkout_("ZZDDHINI", (ftnlen)8);
	return 0;
    }

/*     Now fetch the list of supported binary file formats. */

    zzplatfm_("READS_BFF", linstr, (ftnlen)9, (ftnlen)36);

/*     Parse the wordlist that is sitting in LINSTR. */

    i__ = 0;
    done = FALSE_;
    while(! done) {

/*        Increment the counter and pop the next word */
/*        off. */

	++i__;
	nextwd_(linstr, tmpstr, linstr, (ftnlen)36, (ftnlen)8, (ftnlen)36);

/*        See if we're done. */

	done = i__ > 4 || s_cmp(tmpstr, " ", (ftnlen)8, (ftnlen)1) == 0;

/*        If we're not done, then convert this string to the */
/*        appropriate integer code. */

	if (! done) {
	    supbff[i__ - 1] = isrchc_(tmpstr, &c__4, strbff, (ftnlen)8, 
		    strbff_len);

/*           Check to see if the binary file format listed */
/*           is properly supported. */

	    if (supbff[i__ - 1] == 0) {
		chkin_("ZZDDHINI", (ftnlen)8);
		setmsg_("The binary file format, '#', is not supported by th"
			"is version of the toolkit. This is a serious problem"
			", contact NAIF.            ", (ftnlen)130);
		errch_("#", tmpstr, (ftnlen)1, (ftnlen)8);
		sigerr_("SPICE(BUG)", (ftnlen)10);
		chkout_("ZZDDHINI", (ftnlen)8);
		return 0;
	    }
	}
    }

/*     Now setup NUMSUP.  Given the way the WHILE loop above executes, */
/*     we need to subtract one from I to get the number of entries added */
/*     to SUPBFF.  This smacks of kludge... but it works. */

    *numsup = i__ - 1;
    return 0;
} /* zzddhini_ */

