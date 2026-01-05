/* zzdafgsr.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__4 = 4;
static logical c_false = FALSE_;
static integer c__1 = 1;
static integer c__128 = 128;

/* $Procedure ZZDAFGSR ( Private --- DAF Get Summary/Descriptor Record ) */
/* Subroutine */ int zzdafgsr_(integer *handle, integer *recno, integer *nd, 
	integer *ni, doublereal *dprec, logical *found)
{
    /* Initialized data */

    static logical first = TRUE_;
    static integer natbff = 0;

    /* System generated locals */
    integer i__1, i__2;
    static doublereal equiv_0[128];

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer), s_rdue(cilist *), 
	    do_uio(integer *, char *, ftnlen), e_rdue(void);

    /* Local variables */
    integer ibff, iamh, left, nsum;
    extern /* Subroutine */ int zzddhgsd_(char *, integer *, char *, ftnlen, 
	    ftnlen), zzddhnfo_(integer *, char *, integer *, integer *, 
	    integer *, logical *, ftnlen), zzddhhlu_(integer *, char *, 
	    logical *, integer *, ftnlen), zzxlated_(integer *, char *, 
	    integer *, doublereal *, ftnlen), zzplatfm_(char *, char *, 
	    ftnlen, ftnlen), zzxlatei_(integer *, char *, integer *, integer *
	    , ftnlen);
    integer i__;
    char fname[255];
    integer iarch;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
#define dpbuf (equiv_0)
    extern /* Subroutine */ int ucase_(char *, char *, ftnlen, ftnlen);
#define inbuf ((integer *)equiv_0)
    extern /* Subroutine */ int errch_(char *, char *, ftnlen, ftnlen), 
	    moved_(doublereal *, integer *, doublereal *);
    extern logical failed_(void);
    logical locfnd;
    char chrbuf[1024];
    integer cindex;
    extern integer isrchc_(char *, integer *, char *, ftnlen, ftnlen);
    integer dindex;
    static char strbff[8*4];
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), setmsg_(char *, ftnlen);
    integer iostat;
    extern /* Subroutine */ int errint_(char *, integer *, ftnlen);
    extern logical return_(void);
    char tmpstr[8];
    integer sumsiz, lun;

    /* Fortran I/O blocks */
    static cilist io___15 = { 1, 0, 1, 0, 0 };
    static cilist io___16 = { 1, 0, 1, 0, 0 };


/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines.  Users should not call this routine directly due */
/*     to the volatile nature of this routine. */

/*     Read a summary/descriptor record from a DAF. */

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
/*     RECNO      I   Record number. */
/*     ND         I   Number of double precision components in a summary. */
/*     NI         I   Number of integer components in a summary. */
/*     DPREC      O   Contents of the record. */
/*     FOUND      O   Logical indicating whether the record was found. */

/* $ Detailed_Input */

/*     HANDLE     is the handle associated with the DAF. */

/*     RECNO      is the record number of a particular summary record */
/*                within the DAF, whose contents are to be read. */
/*     ND, */
/*     NI         are the number of double precision and integer */
/*                components, respectively, in each array summary */
/*                in the specified file. */

/* $ Detailed_Output */

/*     DPREC      contains the contents of the specified record from */
/*                the DAF associated with HANDLE, properly translated */
/*                for use on the native environment. */

/*     FOUND      is TRUE when the specified record is found, and is */
/*                FALSE otherwise. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1) SPICE(HANDLENOTFOUND) is signaled if HANDLE can not be */
/*        found in the set of loaded handles. */

/*     2) Routines in the call tree of this routine may trap and */
/*        signal errors. */

/* $ Files */

/*     This routine reads data from the DAF associated with HANDLE. */
/*     This action may result in connecting a logical unit to the */
/*     file, if the handle manager has rotated the file out of the */
/*     unit table. */

/* $ Particulars */

/*     This routine reads summary records of double precision */
/*     numbers which contain integers packed through an EQUIVALENCE */
/*     statement from native and supported non-native DAFs. */

/*     The size of the character buffer and the number of records */
/*     read may have to change to support new environments.  As of */
/*     the original release of this routine, all systems currently */
/*     supported have a 1 kilobyte record length. */

/* $ Examples */

/*     See DAFGSR for sample usage. */

/* $ Restrictions */

/*     1) Numeric data when read as characters from a file preserves */
/*        the bit patterns present in the file in memory. */

/*     2) A record of double precision data is at most 1024 characters */
/*        in length. */

/*     3) DPREC has enough space to store 128 double precision numbers. */

/*     4) Characters a byte-sized, 8 characters constitute a double */
/*        precision number, and 4 characters an integer. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     F.S. Turner     (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.1, 02-OCT-2021 (NJB) */

/*        Corrected typo in comments. Reordered header sections. */

/* -    SPICELIB Version 1.0.0, 12-NOV-2001 (FST) */


/* -& */

/*     SPICELIB Functions */


/*     Local Parameters */

/*     Length in bytes of double precision numbers and integers. */


/*     Local Variables */


/*     Equivalence DPBUF to INBUF. */


/*     Saved Variables */


/*     Data Statements */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("ZZDAFGSR", (ftnlen)8);
    }

/*     Perform some initialization tasks. */

    if (first) {

/*        Populate STRBFF, the buffer that contains the labels */
/*        for each binary file format. */

	for (i__ = 1; i__ <= 4; ++i__) {
	    zzddhgsd_("BFF", &i__, strbff + (((i__1 = i__ - 1) < 4 && 0 <= 
		    i__1 ? i__1 : s_rnge("strbff", i__1, "zzdafgsr_", (ftnlen)
		    239)) << 3), (ftnlen)3, (ftnlen)8);
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
	    chkout_("ZZDAFGSR", (ftnlen)8);
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
	chkout_("ZZDAFGSR", (ftnlen)8);
	return 0;
    }

/*     Now get a logical unit for the handle.  Check FAILED() */
/*     in case an error occurs. */

    zzddhhlu_(handle, "DAF", &c_false, &lun, (ftnlen)3);
    if (failed_()) {
	*found = FALSE_;
	chkout_("ZZDAFGSR", (ftnlen)8);
	return 0;
    }

/*     Branch based on whether the binary file format is native */
/*     or not.  Only supported formats can be opened by ZZDDHOPN, */
/*     so no check of IBFF is required. */

    if (ibff == natbff) {

/*        In the native case, just read the array of double precision */
/*        numbers from the file.  The packed integers will be */
/*        processed properly by the READ. */

	io___15.ciunit = lun;
	io___15.cirec = *recno;
	iostat = s_rdue(&io___15);
	if (iostat != 0) {
	    goto L100001;
	}
	for (i__ = 1; i__ <= 128; ++i__) {
	    iostat = do_uio(&c__1, (char *)&dpbuf[(i__1 = i__ - 1) < 128 && 0 
		    <= i__1 ? i__1 : s_rnge("dpbuf", i__1, "zzdafgsr_", (
		    ftnlen)319)], (ftnlen)sizeof(doublereal));
	    if (iostat != 0) {
		goto L100001;
	    }
	}
	iostat = e_rdue();
L100001:

/*        Since this routine does not signal any IOSTAT based */
/*        errors, return if a non-zero value is assigned to IOSTAT. */

	if (iostat != 0) {
	    chkout_("ZZDAFGSR", (ftnlen)8);
	    return 0;
	}

/*     Process the non-native binary file format case. */

    } else {

/*        Read the record as characters. */

	io___16.ciunit = lun;
	io___16.cirec = *recno;
	iostat = s_rdue(&io___16);
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
	    chkout_("ZZDAFGSR", (ftnlen)8);
	    return 0;
	}

/*        Translate the summary record.  First extract the leading */
/*        3 double precision numbers from the summary record as these */
/*        respectively are NEXT, PREV, and NSUM. */

	zzxlated_(&ibff, chrbuf, &c__128, dpbuf, (ftnlen)24);

/*        Check FAILED() in case the translation process fails for */
/*        any reason. */

	if (failed_()) {
	    chkout_("ZZDAFGSR", (ftnlen)8);
	    return 0;
	}

/*        Convert NSUM to an integer, and compute the number of */
/*        double precision numbers required to store each individual */
/*        summary in the record. */

	nsum = (integer) dpbuf[2];
	sumsiz = *nd + (*ni + 1) / 2;

/*        Convert each of the summaries one at a time. */

	i__1 = nsum;
	for (i__ = 1; i__ <= i__1; ++i__) {

/*           Set the start index into the double precision array */
/*           to receive the components.  Also set the character */
/*           substring index to the start location for this summary. */
/*           In the diagram below, each box represents a double */
/*           precision number.  The figure assumes SUMSIZ is 5 */
/*           double precision numbers: */

/*                 |--- 1 ---|--- 2 ---|--- 3 ---|   |- (I-1) -| */
/*           -------------------------------------   ------------- */
/*           | | | | | | | | | | | | | | | | | | |...| | | | | | |... */
/*           -------------------------------------   ------------- */
/*           |-----|                                            ^ */
/*              ^                                               | */
/*              |                                            Summary */
/*           NEXT, PREV, NSUM                                 Start */

	    dindex = (i__ - 1) * sumsiz + 4;
	    cindex = (dindex - 1 << 3) + 1;

/*           First, check to see if there are any double precision */
/*           numbers to translate.  If so, translate, and then */
/*           increment DINDEX and CINDEX accordingly. */

	    if (*nd > 0) {

/*              DPBUF has room for 128 double precision numbers */
/*              total.  Compute the amount of space left in the */
/*              buffer. */

		left = 128 - (i__ - 1) * sumsiz - 3;
		zzxlated_(&ibff, chrbuf + (cindex - 1), &left, &dpbuf[(i__2 = 
			dindex - 1) < 128 && 0 <= i__2 ? i__2 : s_rnge("dpbuf"
			, i__2, "zzdafgsr_", (ftnlen)416)], cindex + (*nd << 
			3) - 1 - (cindex - 1));

/*              If the translation routine fails for any reason, */
/*              check out and return. */

		if (failed_()) {
		    chkout_("ZZDAFGSR", (ftnlen)8);
		    return 0;
		}
		dindex += *nd;
		cindex += *nd << 3;
	    }

/*           At this point DINDEX and CINDEX are pointing at the */
/*           locations for the packed integers in the record. */
/*           Use DINDEX to compute the index into INBUF, the */
/*           equivalenced integer buffer and translate. */

	    if (*ni > 0) {

/*              INBUF has room for 256 integers total.  Compute */
/*              the amount of space left in the buffer.  Since */
/*              it is equivalenced to DPBUF, account for the */
/*              double precision numbers that were just added. */

		left = 256 - (i__ - 1 << 1) * sumsiz - (*nd << 1) - 6;
		zzxlatei_(&ibff, chrbuf + (cindex - 1), &left, &inbuf[(i__2 = 
			(dindex << 1) - 2) < 256 && 0 <= i__2 ? i__2 : s_rnge(
			"inbuf", i__2, "zzdafgsr_", (ftnlen)451)], cindex + (*
			ni << 2) - 1 - (cindex - 1));

/*              If the translation routine fails for any reason, */
/*              check out and return. */

		if (failed_()) {
		    chkout_("ZZDAFGSR", (ftnlen)8);
		    return 0;
		}

/*              Now check to see if NI is odd.  If so, then zero */
/*              the last integer occupied by the newly translated */
/*              summary.  This is necessary to purge any garbage */
/*              present in memory. */

		if (*ni % 2 == 1) {
		    inbuf[(i__2 = (dindex << 1) - 1 + *ni - 1) < 256 && 0 <= 
			    i__2 ? i__2 : s_rnge("inbuf", i__2, "zzdafgsr_", (
			    ftnlen)472)] = 0;
		}
	    }
	}

/*        Translating garbage is a bad idea in general, so set */
/*        the any remaining double precision numbers in the summary */
/*        record to 0. */

	dindex = nsum * sumsiz + 4;
	for (i__ = dindex; i__ <= 128; ++i__) {
	    dpbuf[(i__1 = i__ - 1) < 128 && 0 <= i__1 ? i__1 : s_rnge("dpbuf",
		     i__1, "zzdafgsr_", (ftnlen)487)] = 0.;
	}
    }

/*     Transfer the DPs to the output argument and return to the */
/*     caller. */

    *found = TRUE_;
    moved_(dpbuf, &c__128, dprec);
    chkout_("ZZDAFGSR", (ftnlen)8);
    return 0;
} /* zzdafgsr_ */

#undef inbuf
#undef dpbuf


