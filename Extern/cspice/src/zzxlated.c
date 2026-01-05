/* zzxlated.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__4 = 4;
static integer c__128 = 128;

/* $Procedure ZZXLATED ( Private --- Translate Double Precision Numbers ) */
/* Subroutine */ int zzxlated_(integer *inbff, char *input, integer *space, 
	doublereal *output, ftnlen input_len)
{
    /* Initialized data */

    static logical first = TRUE_;
    static integer natbff = 0;

    /* System generated locals */
    integer i__1, i__2, i__3;
    char ch__1[1];
    static doublereal equiv_0[128];

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer), i_len(char *, ftnlen);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    extern /* Subroutine */ int zzddhgsd_(char *, integer *, char *, ftnlen, 
	    ftnlen), zzplatfm_(char *, char *, ftnlen, ftnlen);
    integer i__, j, k;
    extern /* Subroutine */ int chkin_(char *, ftnlen), ucase_(char *, char *,
	     ftnlen, ftnlen), errch_(char *, char *, ftnlen, ftnlen);
    integer value;
    extern /* Subroutine */ int moved_(doublereal *, integer *, doublereal *);
    integer numdp;
    extern integer isrchc_(char *, integer *, char *, ftnlen, ftnlen);
    static integer bigint;
#define dpbufr (equiv_0)
    static char strbff[8*4];
#define inbufr ((integer *)equiv_0)
    integer lenipt;
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen);
    extern integer intmin_(void);
    extern /* Subroutine */ int setmsg_(char *, ftnlen), errint_(char *, 
	    integer *, ftnlen);
    static integer smlint;
    extern logical return_(void);
    char tmpstr[8];
    integer outpos;

/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines.  Users should not call this routine directly due */
/*     to the volatile nature of this routine. */

/*     Convert double precision values from one binary file format */
/*     to another. */

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
/*     INBFF      I   Binary file format of d.p. values in INPUT. */
/*     INPUT      I   String containing d.p. values read as characters. */
/*     SPACE      I   Number of d.p. values that can be placed in OUTPUT. */
/*     OUTPUT     O   Translated d.p. values. */

/* $ Detailed_Input */

/*     INBFF      is an integer code that indicates the binary file */
/*                format of INPUT.  Acceptable values are the */
/*                parameters: */

/*                   BIGI3E */
/*                   LTLI3E */
/*                   VAXGFL */
/*                   VAXDFL */

/*                as defined in the include file 'zzddhman.inc'. */

/*     INPUT      is a string containing a group of d.p. values read */
/*                from a file as a character string.  The length of */
/*                this string must be a multiple of the number of */
/*                bytes used to store a d.p. value in a file utilizing */
/*                INBFF. */

/*     SPACE      is the number of d.p. values that OUTPUT has room to */
/*                store. */

/* $ Detailed_Output */

/*     OUTPUT     is an array of double precision values containing */
/*                the translated values from INPUT into the native */
/*                binary format. */

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

/*     This routine translates double precision values from a non-native */
/*     binary format read from a file as a sequence of characters to the */
/*     native format. */

/* $ Examples */

/*     See ZZDAFGSR or ZZDAFGDR. */

/* $ Restrictions */

/*     1) Numeric data when read as characters from a file preserves */
/*        the bit patterns present in the file in memory. */

/*     2) The intrinsic ICHAR preserves the bit pattern of the character */
/*        byte read from a file.  Namely if one examines the integer */
/*        created the 8 least significant bits will be precisely those */
/*        found in the character. */

/*     3) The size of double precision values on the target environment */
/*        are a multiple of some number of bytes. */

/*     4) The length of the INPUT string is a multiple of the number */
/*        of bytes for a double precision value in the INBFF format. */

/*     5) INBFF is supported for reading on this platform, and not */
/*        equivalent to NATBFF on this platform. */

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

/*        Recast J = 8*(-1+I) + 1 as J = -7+8*I to accommodate */
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


/*     Length of the double precision and integer buffers that */
/*     are equivalenced. */


/*     These parameters are used for arithmetic shifting. */


/*     Local Variables */


/*     Equivalence DPBUFR to INBUFR. */


/*     Statement Functions */


/*     Saved Variables */


/*     Data Statements */


/*     Statement Function Definitions */

/*     This function controls the conversion of characters to integers. */
/*     On some supported environments, ICHAR is not sufficient to */
/*     produce the desired results.  This, however, is not the case */
/*     with this particular environment. */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("ZZXLATED", (ftnlen)8);
    }

/*     Perform some initialization tasks. */

    if (first) {

/*        Populate STRBFF. */

	for (i__ = 1; i__ <= 4; ++i__) {
	    zzddhgsd_("BFF", &i__, strbff + (((i__1 = i__ - 1) < 4 && 0 <= 
		    i__1 ? i__1 : s_rnge("strbff", i__1, "zzxlated_", (ftnlen)
		    387)) << 3), (ftnlen)3, (ftnlen)8);
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
	    chkout_("ZZXLATED", (ftnlen)8);
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

/*     Check to see if INBFF makes sense. */

    if (*inbff < 1 || *inbff > 4) {
	setmsg_("The integer code used to indicate the binary file format of"
		" the input integers, #, is out of range.  This error should "
		"never occur.", (ftnlen)131);
	errint_("#", inbff, (ftnlen)1);
	sigerr_("SPICE(BUG)", (ftnlen)10);
	chkout_("ZZXLATED", (ftnlen)8);
	return 0;
    }

/*     Retrieve the length of the input string, and set the position */
/*     into the output buffer to the beginning. */

    lenipt = i_len(input, input_len);
    outpos = 1;

/*     Now branch based on NATBFF. */

    if (natbff == 1) {
	if (*inbff == 2) {

/*           Check to see that the length of the input string is */
/*           appropriate.  Since this is a string containing LTL-IEEE */
/*           d.p. values, and this is a BIG-IEEE machine characters */
/*           are 1-byte and d.p. values are 8-bytes.  So the length */
/*           of INPUT must be a multiple of 8. */

	    numdp = lenipt / 8;
	    if (lenipt - (numdp << 3) != 0) {
		setmsg_("The input string that is to be translated from the "
			"binary format # to format # has a length that is not"
			" a multiple of 4 bytes.  This error should never occ"
			"ur.", (ftnlen)158);
		errch_("#", strbff + (((i__1 = *inbff - 1) < 4 && 0 <= i__1 ? 
			i__1 : s_rnge("strbff", i__1, "zzxlated_", (ftnlen)
			483)) << 3), (ftnlen)1, (ftnlen)8);
		errch_("#", strbff + (((i__1 = natbff - 1) < 4 && 0 <= i__1 ? 
			i__1 : s_rnge("strbff", i__1, "zzxlated_", (ftnlen)
			484)) << 3), (ftnlen)1, (ftnlen)8);
		sigerr_("SPICE(BUG)", (ftnlen)10);
		chkout_("ZZXLATED", (ftnlen)8);
		return 0;
	    }

/*           Verify there is enough room to store the results of */
/*           the translation. */

	    if (numdp > *space) {
		setmsg_("The caller specified that # double precision number"
			"s are to be translated from binary format # to #.  H"
			"owever there is only room to hold # integers in the "
			"output array.  This error should never occur.", (
			ftnlen)200);
		errint_("#", &numdp, (ftnlen)1);
		errch_("#", strbff + (((i__1 = *inbff - 1) < 4 && 0 <= i__1 ? 
			i__1 : s_rnge("strbff", i__1, "zzxlated_", (ftnlen)
			504)) << 3), (ftnlen)1, (ftnlen)8);
		errch_("#", strbff + (((i__1 = natbff - 1) < 4 && 0 <= i__1 ? 
			i__1 : s_rnge("strbff", i__1, "zzxlated_", (ftnlen)
			505)) << 3), (ftnlen)1, (ftnlen)8);
		errint_("#", space, (ftnlen)1);
		sigerr_("SPICE(BUG)", (ftnlen)10);
		chkout_("ZZXLATED", (ftnlen)8);
		return 0;
	    }

/*           The remainder of this branch is devoted to translating */
/*           and copying blocks of DPBLEN double precision numbers */
/*           into OUTPUT.  Initialize K, the integer index into the */
/*           buffer equivalenced to DPBUFR. */

	    k = 1;

/*           Start looping over each 8 character package in INPUT and */
/*           converting it to double precision numbers. */

	    i__1 = numdp;
	    for (i__ = 1; i__ <= i__1; ++i__) {

/*              Compute the substring index of the first character */
/*              in INPUT for this integer. */

		j = (i__ << 3) - 7;

/*              Now arrange the bytes properly.  Since these characters */
/*              were read from a file utilizing LTL-IEEE: */

/*                      . */
/*                      . */
/*                      . */
/*                   ------- */
/*                  |   J   |  - Least Significant Byte of Mantissa */
/*                   ------- */
/*                  |  J+1  |  - Sixth Most Significant Mantissa Byte */
/*                   ------- */
/*                  |  J+2  |  - Fifth Most Significant Mantissa Byte */
/*                   ------- */
/*                  |  J+3  |  - Fourth Most Significant Mantissa Byte */
/*                   ------- */
/*                  |  J+4  |  - Third Most Significant Mantissa Byte */
/*                   ------- */
/*                  |  J+5  |  - Second Most Significant Mantissa Byte */
/*                   ------- */
/*                  |  J+6  |  - Tail of Exponent, Most Significant */
/*                   -------     Bits of the Mantissa */
/*                  |  J+7  |  - Sign Bit, Head of Exponent */
/*                   ------- */
/*                      . */
/*                      . */
/*                      . */

/*              Now rearrange the bytes to place them in the */
/*              proper order for d.p. values on BIG-IEEE machines. */
/*              This is accomplished in the following manner: */

/*                     INPUT(J+4:J+4) */
/*                     INPUT(J+5:J+5)*SHFT8 */
/*                     INPUT(J+6:J+6)*SHFT16 */
/*                  +  INPUT(J+7:J+7)*SHFT24 */
/*                  ------------------------- */
/*                     INBUFR(K) */

/*                     INPUT(J:J) */
/*                     INPUT(J+1:J+1)*SHFT8 */
/*                     INPUT(J+2:J+2)*SHFT16 */
/*                  +  INPUT(J+3:J+3)*SHFT24 */
/*                  ------------------------- */
/*                     INBUFR(K+1) */


/*              Utilize the military extension bit manipulation */
/*              intrinsics to perform the necessary computations. */
/*              It has been determined empirically that on these */
/*              environments this is faster than arithmetic. */

		i__2 = j + 3;
		s_copy(ch__1, input + i__2, (ftnlen)1, j + 4 - i__2);
		value = *(unsigned char *)&ch__1[0];
		inbufr[(i__2 = k - 1) < 256 && 0 <= i__2 ? i__2 : s_rnge(
			"inbufr", i__2, "zzxlated_", (ftnlen)586)] = value;
		i__2 = j + 4;
		s_copy(ch__1, input + i__2, (ftnlen)1, j + 5 - i__2);
		value = *(unsigned char *)&ch__1[0];
		value <<= 8;
		inbufr[(i__2 = k - 1) < 256 && 0 <= i__2 ? i__2 : s_rnge(
			"inbufr", i__2, "zzxlated_", (ftnlen)590)] = inbufr[(
			i__3 = k - 1) < 256 && 0 <= i__3 ? i__3 : s_rnge(
			"inbufr", i__3, "zzxlated_", (ftnlen)590)] | value;
		i__2 = j + 5;
		s_copy(ch__1, input + i__2, (ftnlen)1, j + 6 - i__2);
		value = *(unsigned char *)&ch__1[0];
		value <<= 16;
		inbufr[(i__2 = k - 1) < 256 && 0 <= i__2 ? i__2 : s_rnge(
			"inbufr", i__2, "zzxlated_", (ftnlen)594)] = inbufr[(
			i__3 = k - 1) < 256 && 0 <= i__3 ? i__3 : s_rnge(
			"inbufr", i__3, "zzxlated_", (ftnlen)594)] | value;
		i__2 = j + 6;
		s_copy(ch__1, input + i__2, (ftnlen)1, j + 7 - i__2);
		value = *(unsigned char *)&ch__1[0];
		value <<= 24;
		inbufr[(i__2 = k - 1) < 256 && 0 <= i__2 ? i__2 : s_rnge(
			"inbufr", i__2, "zzxlated_", (ftnlen)598)] = inbufr[(
			i__3 = k - 1) < 256 && 0 <= i__3 ? i__3 : s_rnge(
			"inbufr", i__3, "zzxlated_", (ftnlen)598)] | value;
		*(unsigned char *)&ch__1[0] = *(unsigned char *)&input[j - 1];
		value = *(unsigned char *)&ch__1[0];
		inbufr[(i__2 = k) < 256 && 0 <= i__2 ? i__2 : s_rnge("inbufr",
			 i__2, "zzxlated_", (ftnlen)602)] = value;
		i__2 = j;
		s_copy(ch__1, input + i__2, (ftnlen)1, j + 1 - i__2);
		value = *(unsigned char *)&ch__1[0];
		value <<= 8;
		inbufr[(i__2 = k) < 256 && 0 <= i__2 ? i__2 : s_rnge("inbufr",
			 i__2, "zzxlated_", (ftnlen)606)] = inbufr[(i__3 = k) 
			< 256 && 0 <= i__3 ? i__3 : s_rnge("inbufr", i__3, 
			"zzxlated_", (ftnlen)606)] | value;
		i__2 = j + 1;
		s_copy(ch__1, input + i__2, (ftnlen)1, j + 2 - i__2);
		value = *(unsigned char *)&ch__1[0];
		value <<= 16;
		inbufr[(i__2 = k) < 256 && 0 <= i__2 ? i__2 : s_rnge("inbufr",
			 i__2, "zzxlated_", (ftnlen)610)] = inbufr[(i__3 = k) 
			< 256 && 0 <= i__3 ? i__3 : s_rnge("inbufr", i__3, 
			"zzxlated_", (ftnlen)610)] | value;
		i__2 = j + 2;
		s_copy(ch__1, input + i__2, (ftnlen)1, j + 3 - i__2);
		value = *(unsigned char *)&ch__1[0];
		value <<= 24;
		inbufr[(i__2 = k) < 256 && 0 <= i__2 ? i__2 : s_rnge("inbufr",
			 i__2, "zzxlated_", (ftnlen)614)] = inbufr[(i__3 = k) 
			< 256 && 0 <= i__3 ? i__3 : s_rnge("inbufr", i__3, 
			"zzxlated_", (ftnlen)614)] | value;

/*              Check to see if the local buffer is full and the */
/*              double precision numbers need to be moved into the */
/*              next block of OUTPUT. */

		if (k == 255) {
		    moved_(dpbufr, &c__128, &output[outpos - 1]);
		    outpos += 128;
		    k = 1;

/*              Otherwise, increment K. */

		} else {
		    k += 2;
		}
	    }

/*           Copy any remaining double precision numbers from DPBUFR */
/*           into OUTPUT. */

	    if (k != 1) {
		i__1 = k / 2;
		moved_(dpbufr, &i__1, &output[outpos - 1]);
	    }
	} else {
	    setmsg_("Unable to translate double precision values from binary"
		    " file format # to #. This error should never occur and i"
		    "s indicative of a bug.  Contact NAIF.", (ftnlen)148);
	    errch_("#", strbff + (((i__1 = *inbff - 1) < 4 && 0 <= i__1 ? 
		    i__1 : s_rnge("strbff", i__1, "zzxlated_", (ftnlen)651)) 
		    << 3), (ftnlen)1, (ftnlen)8);
	    errch_("#", strbff + (((i__1 = natbff - 1) < 4 && 0 <= i__1 ? 
		    i__1 : s_rnge("strbff", i__1, "zzxlated_", (ftnlen)652)) 
		    << 3), (ftnlen)1, (ftnlen)8);
	    sigerr_("SPICE(BUG)", (ftnlen)10);
	    chkout_("ZZXLATED", (ftnlen)8);
	    return 0;
	}
    } else if (natbff == 2) {
	if (*inbff == 1) {

/*           Check to see that the length of the input string is */
/*           appropriate.  Since this is a string containing BIG-IEEE */
/*           d.p. values, and this is a LTL-IEEE machine characters */
/*           are 1-byte and d.p. values are 8-bytes.  So the length */
/*           of INPUT must be a multiple of 8. */

	    numdp = lenipt / 8;
	    if (lenipt - (numdp << 3) != 0) {
		setmsg_("The input string that is to be translated from the "
			"binary format # to format # has a length that is not"
			" a multiple of 4 bytes.  This error should never occ"
			"ur.", (ftnlen)158);
		errch_("#", strbff + (((i__1 = *inbff - 1) < 4 && 0 <= i__1 ? 
			i__1 : s_rnge("strbff", i__1, "zzxlated_", (ftnlen)
			679)) << 3), (ftnlen)1, (ftnlen)8);
		errch_("#", strbff + (((i__1 = natbff - 1) < 4 && 0 <= i__1 ? 
			i__1 : s_rnge("strbff", i__1, "zzxlated_", (ftnlen)
			680)) << 3), (ftnlen)1, (ftnlen)8);
		sigerr_("SPICE(BUG)", (ftnlen)10);
		chkout_("ZZXLATED", (ftnlen)8);
		return 0;
	    }

/*           Verify there is enough room to store the results of */
/*           the translation. */

	    if (numdp > *space) {
		setmsg_("The caller specified that # double precision number"
			"s are to be translated from binary format # to #.  H"
			"owever there is only room to hold # integers in the "
			"output array.  This error should never occur.", (
			ftnlen)200);
		errint_("#", &numdp, (ftnlen)1);
		errch_("#", strbff + (((i__1 = *inbff - 1) < 4 && 0 <= i__1 ? 
			i__1 : s_rnge("strbff", i__1, "zzxlated_", (ftnlen)
			700)) << 3), (ftnlen)1, (ftnlen)8);
		errch_("#", strbff + (((i__1 = natbff - 1) < 4 && 0 <= i__1 ? 
			i__1 : s_rnge("strbff", i__1, "zzxlated_", (ftnlen)
			701)) << 3), (ftnlen)1, (ftnlen)8);
		errint_("#", space, (ftnlen)1);
		sigerr_("SPICE(BUG)", (ftnlen)10);
		chkout_("ZZXLATED", (ftnlen)8);
		return 0;
	    }

/*           The remainder of this branch is devoted to translating */
/*           and copying blocks of DPBLEN double precision numbers */
/*           into OUTPUT.  Initialize K, the integer index into the */
/*           buffer equivalenced to DPBUFR. */

	    k = 1;

/*           Start looping over each 8 character package in INPUT and */
/*           converting them to double precision numbers. */

	    i__1 = numdp;
	    for (i__ = 1; i__ <= i__1; ++i__) {

/*              Compute the substring index of the first character */
/*              in INPUT for this integer. */

		j = (i__ << 3) - 7;

/*              Now arrange the bytes properly.  Since these characters */
/*              were read from a file utilizing BIG-IEEE: */

/*                      . */
/*                      . */
/*                      . */
/*                   ------- */
/*                  |   J   |  - Sign Bit, Head of Exponent */
/*                   ------- */
/*                  |  J+1  |  - Tail of Exponent, Most Significant */
/*                   -------     Bits of the Mantissa */
/*                  |  J+2  |  - Second Most Significant Mantissa Byte */
/*                   ------- */
/*                  |  J+3  |  - Third Most Significant Mantissa Byte */
/*                   ------- */
/*                  |  J+4  |  - Fourth Most Significant Mantissa Byte */
/*                   ------- */
/*                  |  J+5  |  - Fifth Most Significant Mantissa Byte */
/*                   ------- */
/*                  |  J+6  |  - Sixth Most Significant Mantissa Byte */
/*                   ------- */
/*                  |  J+7  |  - Least Significant Byte of Mantissa */
/*                   ------- */
/*                      . */
/*                      . */
/*                      . */

/*              Now rearrange the bytes to place them in the */
/*              proper order for d.p. values on LTL-IEEE machines. */
/*              This is accomplished in the following manner: */

/*                     INPUT(J+7:J+7) */
/*                     INPUT(J+6:J+6)*SHFT8 */
/*                     INPUT(J+5:J+5)*SHFT16 */
/*                  +  INPUT(J+4:J+4)*SHFT24 */
/*                  ------------------------- */
/*                     INBUFR(K) */

/*                     INPUT(J+3:J+3) */
/*                     INPUT(J+2:J+2)*SHFT8 */
/*                     INPUT(J+1:J+1)*SHFT16 */
/*                  +  INPUT(J:J)*SHFT24 */
/*                  ------------------------- */
/*                     INBUFR(K+1) */


/*              Utilize the military extension bit manipulation */
/*              intrinsics to perform the necessary computations. */
/*              It has been determined empirically that on these */
/*              environments this is faster than arithmetic. */

		i__2 = j + 6;
		s_copy(ch__1, input + i__2, (ftnlen)1, j + 7 - i__2);
		value = *(unsigned char *)&ch__1[0];
		inbufr[(i__2 = k - 1) < 256 && 0 <= i__2 ? i__2 : s_rnge(
			"inbufr", i__2, "zzxlated_", (ftnlen)782)] = value;
		i__2 = j + 5;
		s_copy(ch__1, input + i__2, (ftnlen)1, j + 6 - i__2);
		value = *(unsigned char *)&ch__1[0];
		value <<= 8;
		inbufr[(i__2 = k - 1) < 256 && 0 <= i__2 ? i__2 : s_rnge(
			"inbufr", i__2, "zzxlated_", (ftnlen)786)] = inbufr[(
			i__3 = k - 1) < 256 && 0 <= i__3 ? i__3 : s_rnge(
			"inbufr", i__3, "zzxlated_", (ftnlen)786)] | value;
		i__2 = j + 4;
		s_copy(ch__1, input + i__2, (ftnlen)1, j + 5 - i__2);
		value = *(unsigned char *)&ch__1[0];
		value <<= 16;
		inbufr[(i__2 = k - 1) < 256 && 0 <= i__2 ? i__2 : s_rnge(
			"inbufr", i__2, "zzxlated_", (ftnlen)790)] = inbufr[(
			i__3 = k - 1) < 256 && 0 <= i__3 ? i__3 : s_rnge(
			"inbufr", i__3, "zzxlated_", (ftnlen)790)] | value;
		i__2 = j + 3;
		s_copy(ch__1, input + i__2, (ftnlen)1, j + 4 - i__2);
		value = *(unsigned char *)&ch__1[0];
		value <<= 24;
		inbufr[(i__2 = k - 1) < 256 && 0 <= i__2 ? i__2 : s_rnge(
			"inbufr", i__2, "zzxlated_", (ftnlen)794)] = inbufr[(
			i__3 = k - 1) < 256 && 0 <= i__3 ? i__3 : s_rnge(
			"inbufr", i__3, "zzxlated_", (ftnlen)794)] | value;
		i__2 = j + 2;
		s_copy(ch__1, input + i__2, (ftnlen)1, j + 3 - i__2);
		value = *(unsigned char *)&ch__1[0];
		inbufr[(i__2 = k) < 256 && 0 <= i__2 ? i__2 : s_rnge("inbufr",
			 i__2, "zzxlated_", (ftnlen)798)] = value;
		i__2 = j + 1;
		s_copy(ch__1, input + i__2, (ftnlen)1, j + 2 - i__2);
		value = *(unsigned char *)&ch__1[0];
		value <<= 8;
		inbufr[(i__2 = k) < 256 && 0 <= i__2 ? i__2 : s_rnge("inbufr",
			 i__2, "zzxlated_", (ftnlen)802)] = inbufr[(i__3 = k) 
			< 256 && 0 <= i__3 ? i__3 : s_rnge("inbufr", i__3, 
			"zzxlated_", (ftnlen)802)] | value;
		i__2 = j;
		s_copy(ch__1, input + i__2, (ftnlen)1, j + 1 - i__2);
		value = *(unsigned char *)&ch__1[0];
		value <<= 16;
		inbufr[(i__2 = k) < 256 && 0 <= i__2 ? i__2 : s_rnge("inbufr",
			 i__2, "zzxlated_", (ftnlen)806)] = inbufr[(i__3 = k) 
			< 256 && 0 <= i__3 ? i__3 : s_rnge("inbufr", i__3, 
			"zzxlated_", (ftnlen)806)] | value;
		*(unsigned char *)&ch__1[0] = *(unsigned char *)&input[j - 1];
		value = *(unsigned char *)&ch__1[0];
		value <<= 24;
		inbufr[(i__2 = k) < 256 && 0 <= i__2 ? i__2 : s_rnge("inbufr",
			 i__2, "zzxlated_", (ftnlen)810)] = inbufr[(i__3 = k) 
			< 256 && 0 <= i__3 ? i__3 : s_rnge("inbufr", i__3, 
			"zzxlated_", (ftnlen)810)] | value;

/*              Check to see if the local buffer is full and the */
/*              double precision numbers need to be moved into the */
/*              next block of OUTPUT. */

		if (k == 255) {
		    moved_(dpbufr, &c__128, &output[outpos - 1]);
		    outpos += 128;
		    k = 1;

/*              Otherwise, increment K. */

		} else {
		    k += 2;
		}
	    }

/*           Copy any remaining double precision numbers from DPBUFR */
/*           into OUTPUT. */

	    if (k != 1) {
		i__1 = k / 2;
		moved_(dpbufr, &i__1, &output[outpos - 1]);
	    }
	} else {
	    setmsg_("Unable to translate double precision values from binary"
		    " file format # to #. This error should never occur and i"
		    "s indicative of a bug.  Contact NAIF.", (ftnlen)148);
	    errch_("#", strbff + (((i__1 = *inbff - 1) < 4 && 0 <= i__1 ? 
		    i__1 : s_rnge("strbff", i__1, "zzxlated_", (ftnlen)847)) 
		    << 3), (ftnlen)1, (ftnlen)8);
	    errch_("#", strbff + (((i__1 = natbff - 1) < 4 && 0 <= i__1 ? 
		    i__1 : s_rnge("strbff", i__1, "zzxlated_", (ftnlen)848)) 
		    << 3), (ftnlen)1, (ftnlen)8);
	    sigerr_("SPICE(BUG)", (ftnlen)10);
	    chkout_("ZZXLATED", (ftnlen)8);
	    return 0;
	}

/*     The native binary file format on this platform is not supported */
/*     for the conversion of integers.  This is a bug, as this branch */
/*     of code should never be reached in normal operation. */

    } else {
	setmsg_("The native binary file format of this toolkit build, #, is "
		"not currently supported for translation of double precision "
		"numbers from non-native formats.", (ftnlen)151);
	errch_("#", strbff + (((i__1 = natbff - 1) < 4 && 0 <= i__1 ? i__1 : 
		s_rnge("strbff", i__1, "zzxlated_", (ftnlen)866)) << 3), (
		ftnlen)1, (ftnlen)8);
	sigerr_("SPICE(BUG)", (ftnlen)10);
	chkout_("ZZXLATED", (ftnlen)8);
	return 0;
    }
    chkout_("ZZXLATED", (ftnlen)8);
    return 0;
} /* zzxlated_ */

#undef inbufr
#undef dpbufr


