/* zzxlatei.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__4 = 4;

/* $Procedure ZZXLATEI ( Private --- Translate Integers ) */
/* Subroutine */ int zzxlatei_(integer *inbff, char *input, integer *space, 
	integer *output, ftnlen input_len)
{
    /* Initialized data */

    static logical first = TRUE_;
    static integer natbff = 0;

    /* System generated locals */
    integer i__1, i__2;
    char ch__1[1];

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer), i_len(char *, ftnlen);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    extern /* Subroutine */ int zzddhgsd_(char *, integer *, char *, ftnlen, 
	    ftnlen), zzplatfm_(char *, char *, ftnlen, ftnlen);
    integer i__, j;
    extern /* Subroutine */ int chkin_(char *, ftnlen), ucase_(char *, char *,
	     ftnlen, ftnlen), errch_(char *, char *, ftnlen, ftnlen);
    integer value;
    extern integer isrchc_(char *, integer *, char *, ftnlen, ftnlen);
    static integer bigint;
    static char strbff[8*4];
    integer lenipt;
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen);
    extern integer intmin_(void);
    extern /* Subroutine */ int setmsg_(char *, ftnlen), errint_(char *, 
	    integer *, ftnlen);
    static integer smlint;
    integer numint;
    extern logical return_(void);
    char tmpstr[8];

/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines.  Users should not call this routine directly due */
/*     to the volatile nature of this routine. */

/*     Convert integers from one binary file format to another. */

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
/*     INBFF      I   Binary file format code for integers in INPUT. */
/*     INPUT      I   String containing integers read as characters. */
/*     SPACE      I   Number of integers that can be placed in OUTPUT. */
/*     OUTPUT     O   Translated integer values. */

/* $ Detailed_Input */

/*     INBFF      is an integer code that indicates the binary file */
/*                format of INPUT.  Acceptable values are the */
/*                parameters: */

/*                   BIGI3E */
/*                   LTLI3E */
/*                   VAXGFL */
/*                   VAXDFL */

/*                as defined in the include file 'zzddhman.inc'. */

/*     INPUT      is a string containing a group of integers read */
/*                from a file as a character string.  The length of */
/*                this string must be a multiple of the number of */
/*                bytes used to store an integer in a file utilizing */
/*                INBFF. */

/*     SPACE      is the number of integers that OUTPUT has room to */
/*                store. */

/* $ Detailed_Output */

/*     OUTPUT     is an array of integers containing the translated */
/*                values from INPUT into the native binary format. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/*     This routine signals several SPICE(BUG) exceptions.  They are */
/*     signaled when improperly specified inputs are passed into the */
/*     routine or if the module or modules in its calling tree are */
/*     improperly configured to run on this platform.  Callers that */
/*     prevent invalid inputs from being passed into this routine */
/*     need not check in.  See the $Restrictions section for a */
/*     discussion of input argument restrictions. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine translates integers from a non-native integer format */
/*     read from a file as a sequence of characters to the native format. */

/* $ Examples */

/*     See ZZDAFGFR, ZZDAFGSR. */

/* $ Restrictions */

/*     1) Numeric data when read as characters from a file preserve */
/*        the bit patterns present in the file in memory. */

/*     2) A byte is 8 bits, and a character is some multiple of */
/*        bytes. */

/*     3) The intrinsic ICHAR preserves the bit pattern of the character */
/*        byte read from a file.  Namely if one examines the integer */
/*        created the 8 least significant bits will be precisely those */
/*        found in the character. */

/*     4) The size of integers on the target environment are a multiple */
/*        of some number of bytes. */

/*     5) The length of the INPUT string is a multiple of the number */
/*        of bytes for an integer in the INBFF format. */

/*     6) INBFF is supported for reading on this platform, and not */
/*        equivalent to NATBFF on this platform. */

/*     7) This routine must support all of the non-native translations */
/*        required by the 'READS_BFF' key in ZZPLATFM. */

/*     8) The character label corresponding to INBFF must be one of the */
/*        non-native entries in the value of 'READS_BFF' returned by */
/*        ZZPLATFM for this environment. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     F.S. Turner     (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.27.0, 28-NOV-2021 (BVS) */

/*        Updated for MAC-OSX-M1-64BIT-CLANG_C. */

/* -    SPICELIB Version 1.26.1, 01-OCT-2021 (NJB) */

/*        Fixed typo in comments. */

/* -    SPICELIB Version 1.26.0, 15-NOV-2015 (EDW) */

/*        Recast J = 4*(-1+I) + 1 as J = -3+4*I to accommodate */
/*        f2c processing. */

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

/* -    SPICELIB Version 1.3.0, 26-OCT-2005 (BVS) */

/*        Updated for SUN-SOLARIS-64BIT-GCC_C. */

/* -    SPICELIB Version 1.2.0, 03-JAN-2005 (BVS) */

/*        Updated for PC-CYGWIN_C. */

/* -    SPICELIB Version 1.1.0, 03-JAN-2005 (BVS) */

/*        Updated for PC-CYGWIN. */

/* -    SPICELIB Version 1.0.1, 17-JUL-2002 (BVS) */

/*        Added MAC-OSX environments. */

/* -    SPICELIB Version 1.0.0, 12-NOV-2001 (FST) */


/* -& */

/*     SPICELIB Functions */


/*     Local Parameters */


/*     These parameters are used for arithmetic shifting. */


/*     Local Variables */


/*     Statement Functions */


/*     Saved Variables */


/*     Data Statements */


/*     Statement Function Definitions */

/*     This function controls the conversion of characters to integers. */
/*     On some supported environments, ICHAR is not sufficient to */
/*     produce the desired results.  This however, is not the case */
/*     with this particular environment. */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("ZZXLATEI", (ftnlen)8);
    }

/*     Perform some initialization tasks. */

    if (first) {

/*        Populate STRBFF with the appropriate binary file */
/*        format labels. */

	for (i__ = 1; i__ <= 4; ++i__) {
	    zzddhgsd_("BFF", &i__, strbff + (((i__1 = i__ - 1) < 4 && 0 <= 
		    i__1 ? i__1 : s_rnge("strbff", i__1, "zzxlatei_", (ftnlen)
		    374)) << 3), (ftnlen)3, (ftnlen)8);
	}

/*        Fetch the native binary file format. */

	zzplatfm_("FILE_FORMAT", tmpstr, (ftnlen)11, (ftnlen)8);
	ucase_(tmpstr, tmpstr, (ftnlen)8, (ftnlen)8);
	natbff = isrchc_(tmpstr, &c__4, strbff, (ftnlen)8, (ftnlen)8);
	if (natbff == 0) {
	    setmsg_("The binary file format, '#', is not supported by this v"
		    "ersion of the toolkit. This is a serious problem, contac"
		    "t NAIF.", (ftnlen)118);
	    errch_("#", tmpstr, (ftnlen)1, (ftnlen)8);
	    sigerr_("SPICE(BUG)", (ftnlen)10);
	    chkout_("ZZXLATEI", (ftnlen)8);
	    return 0;
	}

/*        Store the largest value a 32-bit integer can actually */
/*        hold. */

	bigint = 2147483647;

/*        Prepare the smallest value a 32-bit integer can actually */
/*        store, regardless of what INTMIN returns. */

	smlint = intmin_();

/*        Set SMLINT to the appropriate value if INTMIN is too large. */

	if (smlint == -2147483647) {
	    --smlint;
	}

/*        Do not perform initialization tasks again. */

	first = FALSE_;
    }

/*     Check to see if INBFF is valid.  This should never occur if this */
/*     routine is called properly. */

    if (*inbff < 1 || *inbff > 4) {
	setmsg_("The integer code used to indicate the binary file format of"
		" the input integers, #, is out of range.  This error should "
		"never occur.", (ftnlen)131);
	errint_("#", inbff, (ftnlen)1);
	sigerr_("SPICE(BUG)", (ftnlen)10);
	chkout_("ZZXLATEI", (ftnlen)8);
	return 0;
    }

/*     Retrieve the length of the input string. */

    lenipt = i_len(input, input_len);

/*     Now branch based on the value of NATBFF. */

    if (natbff == 1) {
	if (*inbff == 2) {

/*           Check to see that the length of the input string is */
/*           appropriate.  Since this is a string containing LTL-IEEE */
/*           integers and this is a BIG-IEEE machine, characters are */
/*           1-byte and integers are 4-bytes.  So the length of INPUT */
/*           must be a multiple of 4. */

	    numint = lenipt / 4;
	    if (lenipt - (numint << 2) != 0) {
		setmsg_("The input string that is to be translated from the "
			"binary format # to format # has a length that is not"
			" a multiple of 4 bytes.  This error should never occ"
			"ur.", (ftnlen)158);
		errch_("#", strbff + (((i__1 = *inbff - 1) < 4 && 0 <= i__1 ? 
			i__1 : s_rnge("strbff", i__1, "zzxlatei_", (ftnlen)
			469)) << 3), (ftnlen)1, (ftnlen)8);
		errch_("#", strbff + (((i__1 = natbff - 1) < 4 && 0 <= i__1 ? 
			i__1 : s_rnge("strbff", i__1, "zzxlatei_", (ftnlen)
			470)) << 3), (ftnlen)1, (ftnlen)8);
		sigerr_("SPICE(BUG)", (ftnlen)10);
		chkout_("ZZXLATEI", (ftnlen)8);
		return 0;
	    }

/*           Verify there is enough room to store the results of */
/*           the translation. */

	    if (numint > *space) {
		setmsg_("The caller specified that # integers are to be tran"
			"slated from binary format # to #.  However there is "
			"only room to hold # integers in the output array.  T"
			"his error should never occur.", (ftnlen)184);
		errint_("#", &numint, (ftnlen)1);
		errch_("#", strbff + (((i__1 = *inbff - 1) < 4 && 0 <= i__1 ? 
			i__1 : s_rnge("strbff", i__1, "zzxlatei_", (ftnlen)
			489)) << 3), (ftnlen)1, (ftnlen)8);
		errch_("#", strbff + (((i__1 = natbff - 1) < 4 && 0 <= i__1 ? 
			i__1 : s_rnge("strbff", i__1, "zzxlatei_", (ftnlen)
			490)) << 3), (ftnlen)1, (ftnlen)8);
		errint_("#", space, (ftnlen)1);
		sigerr_("SPICE(BUG)", (ftnlen)10);
		chkout_("ZZXLATEI", (ftnlen)8);
		return 0;
	    }

/*           Start looping over each 4 character package in INPUT and */
/*           converting them to integers. */

	    i__1 = numint;
	    for (i__ = 1; i__ <= i__1; ++i__) {

/*              Compute the substring index of the first character */
/*              in INPUT for this integer. */

		j = (i__ << 2) - 3;

/*              Now arrange the bytes properly.  Since these characters */
/*              were read from a file utilizing LTL-IEEE, we know that */
/*              J is the least significant byte and that (J+3) is the */
/*              most significant. */

/*              INPUT: */

/*                      ------------------------------------- */
/*                 . . .|     |  J  | J+1 | J+2 | J+3 |     |. . . */
/*                      ------------------------------------- */

/*              From this we construct OUTPUT(I) using the following */
/*              relation: */

/*                      INPUT(J:J) */
/*                      INPUT(J+1:J+1) shifted 8 bits to the MSb */
/*                      INPUT(J+2:J+2) shifted 16 bits to the MSb */
/*                   +  INPUT(J+3:J+3) shifted 24 bits to the MSb */
/*                   ------------------------- */
/*                      OUTPUT(I) */


/*              Utilize the military extension bit manipulation */
/*              intrinsics to perform the necessary computations. */
/*              It has been determined empirically that on this */
/*              environment it is faster than arithmetic. */

		*(unsigned char *)&ch__1[0] = *(unsigned char *)&input[j - 1];
		value = *(unsigned char *)&ch__1[0];
		output[i__ - 1] = value;
		i__2 = j;
		s_copy(ch__1, input + i__2, (ftnlen)1, j + 1 - i__2);
		value = *(unsigned char *)&ch__1[0];
		value <<= 8;
		output[i__ - 1] |= value;
		i__2 = j + 1;
		s_copy(ch__1, input + i__2, (ftnlen)1, j + 2 - i__2);
		value = *(unsigned char *)&ch__1[0];
		value <<= 16;
		output[i__ - 1] |= value;
		i__2 = j + 2;
		s_copy(ch__1, input + i__2, (ftnlen)1, j + 3 - i__2);
		value = *(unsigned char *)&ch__1[0];
		value <<= 24;
		output[i__ - 1] |= value;
	    }
	} else {
	    setmsg_("Unable to translate integers from binary file format # "
		    "to #.  This error should never occur and is indicative o"
		    "f a bug.  Contact NAIF.", (ftnlen)134);
	    errch_("#", strbff + (((i__1 = *inbff - 1) < 4 && 0 <= i__1 ? 
		    i__1 : s_rnge("strbff", i__1, "zzxlatei_", (ftnlen)560)) 
		    << 3), (ftnlen)1, (ftnlen)8);
	    errch_("#", strbff + (((i__1 = natbff - 1) < 4 && 0 <= i__1 ? 
		    i__1 : s_rnge("strbff", i__1, "zzxlatei_", (ftnlen)561)) 
		    << 3), (ftnlen)1, (ftnlen)8);
	    sigerr_("SPICE(BUG)", (ftnlen)10);
	    chkout_("ZZXLATEI", (ftnlen)8);
	    return 0;
	}
    } else if (natbff == 2) {
	if (*inbff == 1) {

/*           Check to see that the length of the input string is */
/*           appropriate.  Since this is a string containing BIG-IEEE */
/*           integers and this is a LTL-IEEE machine, characters are */
/*           1-byte and integers are 4-bytes.  So the length of INPUT */
/*           must be a multiple of 4. */

	    numint = lenipt / 4;
	    if (lenipt - (numint << 2) != 0) {
		setmsg_("The input string that is to be translated from the "
			"binary format # to format # has a length that is not"
			" a multiple of 4 bytes.  This error should never occ"
			"ur.", (ftnlen)158);
		errch_("#", strbff + (((i__1 = *inbff - 1) < 4 && 0 <= i__1 ? 
			i__1 : s_rnge("strbff", i__1, "zzxlatei_", (ftnlen)
			588)) << 3), (ftnlen)1, (ftnlen)8);
		errch_("#", strbff + (((i__1 = natbff - 1) < 4 && 0 <= i__1 ? 
			i__1 : s_rnge("strbff", i__1, "zzxlatei_", (ftnlen)
			589)) << 3), (ftnlen)1, (ftnlen)8);
		sigerr_("SPICE(BUG)", (ftnlen)10);
		chkout_("ZZXLATEI", (ftnlen)8);
		return 0;
	    }

/*           Verify there is enough room to store the results of */
/*           the translation. */

	    if (numint > *space) {
		setmsg_("The caller specified that # integers are to be tran"
			"slated from binary format # to #.  However there is "
			"only room to hold # integers in the output array.  T"
			"his error should never occur.", (ftnlen)184);
		errint_("#", &numint, (ftnlen)1);
		errch_("#", strbff + (((i__1 = *inbff - 1) < 4 && 0 <= i__1 ? 
			i__1 : s_rnge("strbff", i__1, "zzxlatei_", (ftnlen)
			608)) << 3), (ftnlen)1, (ftnlen)8);
		errch_("#", strbff + (((i__1 = natbff - 1) < 4 && 0 <= i__1 ? 
			i__1 : s_rnge("strbff", i__1, "zzxlatei_", (ftnlen)
			609)) << 3), (ftnlen)1, (ftnlen)8);
		errint_("#", space, (ftnlen)1);
		sigerr_("SPICE(BUG)", (ftnlen)10);
		chkout_("ZZXLATEI", (ftnlen)8);
		return 0;
	    }

/*           Start looping over each 4 character package in INPUT and */
/*           converting them to integers. */

	    i__1 = numint;
	    for (i__ = 1; i__ <= i__1; ++i__) {

/*              Compute the substring index of the first character */
/*              in INPUT for this integer. */

		j = (i__ << 2) - 3;

/*              Now arrange the bytes properly.  Since these characters */
/*              were read from a file utilizing BIG-IEEE, we know that */
/*              J is the most significant byte and that (J+3) is the */
/*              least significant. */

/*              INPUT: */

/*                      ------------------------------------- */
/*                 . . .|     |  J  | J+1 | J+2 | J+3 |     |. . . */
/*                      ------------------------------------- */

/*              From this we construct OUTPUT(I) using the following */
/*              relation: */

/*                      INPUT(J+3:J+3) */
/*                      INPUT(J+2:J+2)*SHFT8 */
/*                      INPUT(J+1:J+1)*SHFT16 */
/*                   +  INPUT(J:J)*SHFT24 */
/*                   ------------------------- */
/*                      OUTPUT(I) */


/*              Utilize the military extension bit manipulation */
/*              intrinsics to perform the necessary computations. */
/*              It has been determined empirically that on this */
/*              environment it is faster than arithmetic. */

		i__2 = j + 2;
		s_copy(ch__1, input + i__2, (ftnlen)1, j + 3 - i__2);
		value = *(unsigned char *)&ch__1[0];
		output[i__ - 1] = value;
		i__2 = j + 1;
		s_copy(ch__1, input + i__2, (ftnlen)1, j + 2 - i__2);
		value = *(unsigned char *)&ch__1[0];
		value <<= 8;
		output[i__ - 1] |= value;
		i__2 = j;
		s_copy(ch__1, input + i__2, (ftnlen)1, j + 1 - i__2);
		value = *(unsigned char *)&ch__1[0];
		value <<= 16;
		output[i__ - 1] |= value;
		*(unsigned char *)&ch__1[0] = *(unsigned char *)&input[j - 1];
		value = *(unsigned char *)&ch__1[0];
		value <<= 24;
		output[i__ - 1] |= value;
	    }
	} else {
	    setmsg_("Unable to translate integers from binary file format # "
		    "to #.  This error should never occur and is indicative o"
		    "f a bug.  Contact NAIF.", (ftnlen)134);
	    errch_("#", strbff + (((i__1 = *inbff - 1) < 4 && 0 <= i__1 ? 
		    i__1 : s_rnge("strbff", i__1, "zzxlatei_", (ftnlen)679)) 
		    << 3), (ftnlen)1, (ftnlen)8);
	    errch_("#", strbff + (((i__1 = natbff - 1) < 4 && 0 <= i__1 ? 
		    i__1 : s_rnge("strbff", i__1, "zzxlatei_", (ftnlen)680)) 
		    << 3), (ftnlen)1, (ftnlen)8);
	    sigerr_("SPICE(BUG)", (ftnlen)10);
	    chkout_("ZZXLATEI", (ftnlen)8);
	    return 0;
	}

/*     The native binary file format on this platform is not supported */
/*     for the conversion of integers.  This is a bug, as this branch */
/*     of code should never be reached in normal operation. */

    } else {
	setmsg_("The native binary file format of this toolkit build, #, is "
		"not currently supported for translation of integers from non"
		"-native formats.", (ftnlen)135);
	errch_("#", strbff + (((i__1 = natbff - 1) < 4 && 0 <= i__1 ? i__1 : 
		s_rnge("strbff", i__1, "zzxlatei_", (ftnlen)698)) << 3), (
		ftnlen)1, (ftnlen)8);
	sigerr_("SPICE(BUG)", (ftnlen)10);
	chkout_("ZZXLATEI", (ftnlen)8);
	return 0;
    }
    chkout_("ZZXLATEI", (ftnlen)8);
    return 0;
} /* zzxlatei_ */

