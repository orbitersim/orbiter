/* zzddhppf.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;
static integer c__2 = 2;
static integer c__5 = 5;
static integer c__4 = 4;

/* $Procedure ZZDDHPPF ( Private --- DDH Prepare Preexisting File ) */
/* Subroutine */ int zzddhppf_(integer *unit, integer *arch, integer *bff)
{
    /* Initialized data */

    static logical first = TRUE_;

    /* System generated locals */
    integer i__1;
    char ch__1[1];

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer), s_rdue(cilist *), 
	    do_uio(integer *, char *, ftnlen), e_rdue(void);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    static char null[1];
    extern /* Subroutine */ int zzddhgsd_(char *, integer *, char *, ftnlen, 
	    ftnlen), zzddhivf_(char *, integer *, logical *, ftnlen), 
	    zzftpchk_(char *, logical *, ftnlen), zzplatfm_(char *, char *, 
	    ftnlen, ftnlen);
    integer i__, fdrec;
    extern /* Subroutine */ int zzftpstr_(char *, char *, char *, char *, 
	    ftnlen, ftnlen, ftnlen, ftnlen), chkin_(char *, ftnlen), ucase_(
	    char *, char *, ftnlen, ftnlen), errch_(char *, char *, ftnlen, 
	    ftnlen);
    logical found;
    extern /* Subroutine */ int idw2at_(char *, char *, char *, ftnlen, 
	    ftnlen, ftnlen);
    char filarc[4], bffidw[8], chrrec[1000];
    extern integer isrchc_(char *, integer *, char *, ftnlen, ftnlen);
    static char ftpdlm[1], ftpmem[16], ftplft[6], strarc[8*2], strbff[8*5];
    integer iostat, tstarc;
    static char ftprgt[6];
    char filtyp[4];
    logical ftperr;
    integer ftppos;
    extern /* Subroutine */ int setmsg_(char *, ftnlen);
    extern logical return_(void);
    extern /* Subroutine */ int errint_(char *, integer *, ftnlen), sigerr_(
	    char *, ftnlen), chkout_(char *, ftnlen), errfnm_(char *, integer 
	    *, ftnlen);
    extern integer pos_(char *, char *, integer *, ftnlen, ftnlen);

    /* Fortran I/O blocks */
    static cilist io___11 = { 1, 0, 1, 0, 1 };
    static cilist io___20 = { 1, 0, 1, 0, 0 };


/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines.  Users should not call this routine directly due */
/*     to the volatile nature of this routine. */

/*     Prepare preexisting binary file for entry into the handle */
/*     table. */

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
/*     UNIT       I   Logical unit attached to the binary file. */
/*     ARCH       I   Integer code indicating the file architecture. */
/*     BFF        O   Integer code indicating the binary file format. */

/* $ Detailed_Input */

/*     UNIT       is a logical unit attached to the binary file to be */
/*                prepared for inclusion into the handle table. */

/*     ARCH       is an integer that indicates the architecture of */
/*                the file attached to UNIT.  Acceptable values are */
/*                the parameters: */

/*                   DAF */
/*                   DAS */

/*                defined in ZZDDHMAN.INC. */

/* $ Detailed_Output */

/*     BFF        is an integer that indicates the binary file format */
/*                of the DAF attached to UNIT.  Possible values are */
/*                the parameters: */

/*                   BIGI3E */
/*                   LTLI3E */
/*                   VAXGFL */
/*                   VAXDFL */

/*                defined in ZZDDHMAN.INC. */

/* $ Parameters */

/*     See the include file ZZDDHMAN.INC. */

/* $ Exceptions */

/*     1) SPICE(UNKNOWNFILARC) is signaled when ARCH is not in the */
/*        range of codes for known file architectures or the binary */
/*        file's ID word is unknown to IDW2AT.  BFF is set to 0 when */
/*        this error is signaled.  UNIT is not closed. */

/*     2) SPICE(FILEREADFAILED) is signaled when either of the two */
/*        READ statements in the module returns non-zero IOSTAT, thus */
/*        indicating read failure.  BFF is set to 0 in this case.  Unit */
/*        is not closed. */

/*     3) SPICE(FILARCMISMATCH) is signaled when the file attached to */
/*        UNIT is determined to utilize an architecture that is */
/*        different from the one to which the input argument ARCH */
/*        refers.  Unit is not closed. */

/*     4) SPICE(UNKNOWNBFF) is signaled whenever the binary file */
/*        format detection algorithm reaches a state of uncertainty */
/*        for DAFs.  This can be the result of several conditions, */
/*        an empty pre-N0052 DAF, reading a DAF with an unknown BFF */
/*        from a future toolkit, etc.  In all cases, BFF is set to 0. */
/*        Unit is not closed. */

/*     5) If a pre-FTP string binary is loaded, no FTP based */
/*        diagnostics are performed, and the file is assumed to be */
/*        in proper, working order. */

/* $ Files */

/*     This routine reads at least one, and potentially, several records */
/*     from the file attached to UNIT. */

/* $ Particulars */

/*     This routine exists to prepare a binary file for inclusion */
/*     in the handle table in ZZDDHMAN.  This includes verifying */
/*     that the file is suitable to load and determining the binary */
/*     file format where possible. */

/*     For DAF files: */

/*        The binary file format of old (pre-N0050) binaries is */
/*        detectable if the file is non-empty and undamaged. */
/*        New files contain the binary file format identification */
/*        string in the file record along with the FTP error */
/*        detection string.  They are correctly identified in most */
/*        cases, including damaged. */

/*     For DAS files: */

/*        The binary file format of old (pre-N0052) binaries is */
/*        not detectable.  This this module will assume that any */
/*        old DAS binaries are of the native format.  New binaries */
/*        include the binary file format identification string as */
/*        well as the FTP error detection string.  They are */
/*        correctly identified in most cases as well. */

/*     FTP Error Detection: */

/*        FTP error detection occurs when at least part of the */
/*        detection string is detected in the file record.  When */
/*        absent, no errors are signaled and the file is then */
/*        assumed to be an old binary.  In the event that the FTP */
/*        detection string is present, and additional unknown */
/*        sequences are present, diagnostics are only performed on */
/*        sequences known to this version of the toolkit. */

/* $ Examples */

/*     See ZZDDHMAN for sample usage. */

/* $ Restrictions */

/*     1) The file attached to UNIT was written on a platform whose */
/*        characters are of a single byte in length. */

/*     2) Numeric data when read as characters from the UNIT */
/*        preserves the bit patterns present in the file in */
/*        memory. */

/*     3) The intrinsic ICHAR preserves the bit pattern of the */
/*        character byte read from a file.  Namely if one examines */
/*        the integer created the 8 least significant bits will be */
/*        precisely those found in the character. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     F.S. Turner     (JPL) */

/* $ Version */

/* -    SPICELIB Version 2.26.0, 28-NOV-2021 (BVS) */

/*        Updated for MAC-OSX-M1-64BIT-CLANG_C. */

/* -    SPICELIB Version 2.25.0, 10-MAR-2014 (BVS) */

/*        Updated for SUN-SOLARIS-64BIT-INTEL. */

/* -    SPICELIB Version 2.24.0, 10-MAR-2014 (BVS) */

/*        Updated for PC-LINUX-64BIT-IFORT. */

/* -    SPICELIB Version 2.23.0, 10-MAR-2014 (BVS) */

/*        Updated for PC-CYGWIN-GFORTRAN. */

/* -    SPICELIB Version 2.22.0, 10-MAR-2014 (BVS) */

/*        Updated for PC-CYGWIN-64BIT-GFORTRAN. */

/* -    SPICELIB Version 2.21.0, 10-MAR-2014 (BVS) */

/*        Updated for PC-CYGWIN-64BIT-GCC_C. */

/* -    SPICELIB Version 2.20.0, 13-MAY-2010 (BVS) */

/*        Updated for SUN-SOLARIS-INTEL. */

/* -    SPICELIB Version 2.19.0, 13-MAY-2010 (BVS) */

/*        Updated for SUN-SOLARIS-INTEL-CC_C. */

/* -    SPICELIB Version 2.18.0, 13-MAY-2010 (BVS) */

/*        Updated for SUN-SOLARIS-INTEL-64BIT-CC_C. */

/* -    SPICELIB Version 2.17.0, 13-MAY-2010 (BVS) */

/*        Updated for SUN-SOLARIS-64BIT-NATIVE_C. */

/* -    SPICELIB Version 2.16.0, 13-MAY-2010 (BVS) */

/*        Updated for PC-WINDOWS-64BIT-IFORT. */

/* -    SPICELIB Version 2.15.0, 13-MAY-2010 (BVS) */

/*        Updated for PC-LINUX-64BIT-GFORTRAN. */

/* -    SPICELIB Version 2.14.0, 13-MAY-2010 (BVS) */

/*        Updated for PC-64BIT-MS_C. */

/* -    SPICELIB Version 2.13.0, 13-MAY-2010 (BVS) */

/*        Updated for MAC-OSX-64BIT-INTEL_C. */

/* -    SPICELIB Version 2.12.0, 13-MAY-2010 (BVS) */

/*        Updated for MAC-OSX-64BIT-IFORT. */

/* -    SPICELIB Version 2.11.0, 13-MAY-2010 (BVS) */

/*        Updated for MAC-OSX-64BIT-GFORTRAN. */

/* -    SPICELIB Version 2.10.0, 18-MAR-2009 (BVS) */

/*        Updated for PC-LINUX-GFORTRAN. */

/* -    SPICELIB Version 2.9.0, 18-MAR-2009 (BVS) */

/*        Updated for MAC-OSX-GFORTRAN. */

/* -    SPICELIB Version 2.8.0, 19-FEB-2008 (BVS) */

/*        Updated for PC-LINUX-IFORT. */

/* -    SPICELIB Version 2.7.0, 14-NOV-2006 (BVS) */

/*        Updated for PC-LINUX-64BIT-GCC_C. */

/* -    SPICELIB Version 2.6.0, 14-NOV-2006 (BVS) */

/*        Updated for MAC-OSX-INTEL_C. */

/* -    SPICELIB Version 2.5.0, 14-NOV-2006 (BVS) */

/*        Updated for MAC-OSX-IFORT. */

/* -    SPICELIB Version 2.4.0, 14-NOV-2006 (BVS) */

/*        Updated for PC-WINDOWS-IFORT. */

/* -    SPICELIB Version 2.3.0, 26-OCT-2005 (BVS) */

/*        Updated for SUN-SOLARIS-64BIT-GCC_C. */

/* -    SPICELIB Version 2.2.0, 03-JAN-2005 (BVS) */

/*        Updated for PC-CYGWIN_C. */

/* -    SPICELIB Version 2.1.0, 03-JAN-2005 (BVS) */

/*        Updated for PC-CYGWIN. */

/* -    SPICELIB Version 2.0.1, 17-JUL-2002 (BVS) */

/*        Added MAC-OSX environments. */

/* -    SPICELIB Version 2.0.0, 06-FEB-2002 (FST) */

/*        This routine was updated to load binaries created by */
/*        N0051 versions of Sun Solaris Native C Toolkits.  See */
/*        the Revisions section for details. */

/* -    SPICELIB Version 1.0.0, 04-OCT-2001 (FST) */


/* -& */
/* $ Revisions */

/* -    SPICELIB Version 2.0.0, 06-FEB-2002 (FST) */

/*        Shortly after releasing N0052, a few of our users */
/*        discovered that they were unable to load binary */
/*        DAFs created with the N0051 Sun Solaris Native C */
/*        (SUN-SOLARIS-NATIVE_C) Toolkits.  The reason for this */
/*        is the previous version of ZZDDHPPF released with N0052 */
/*        assumed that if a DAF file record possessed a valid */
/*        FTP error detection string, then it must contain a */
/*        binary file format ID string as well.  Both were added */
/*        to the DAF file record in N0050. */

/*        However, a bug in the N0051 version of the ZZPLATFM */
/*        master file, the source of the binary file format ID */
/*        string for a given platform, neglected to assign a */
/*        value to the string.  Since it was a C environment */
/*        and the implementation of ZZPLATFM resulted in the */
/*        string being a static variable, it was initialized */
/*        to nulls and written into the file. */

/*        This version of ZZDDHPPF has been extended to recognize */
/*        a null binary file format ID, and apply the byte */
/*        examination algorithm used on pre-N0050 DAFs to determine */
/*        its format. */

/* -& */

/*     SPICELIB Functions */


/*     Local Parameters */

/*     Number of characters to be read in from a record. */


/*     Bounding indices for the window that brackets the FTP */
/*     error detection string in the file record. */


/*     Index of the start of the binary file format identification */
/*     string in DAF binaries. */


/*     Index of the start of the binary file format identification */
/*     string in DAS binaries. */


/*     Size of the binary format identification string. */


/*     Index of the first byte of NI in the DAF file record. */


/*     Index of the first byte of NSUM in the DAF descriptor record. */


/*     Index of the first byte of FDREC in the DAF file record. */


/*     Integer code such that CHAR(INTNUL) produces the NULL character. */


/*     IDW2AT Output Argument Lengths. */


/*     NULLID is the index of the extended STRBFF "NULL" string ID. */


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
	chkin_("ZZDDHPPF", (ftnlen)8);
    }

/*     If this is the first time into the routine, populate local */
/*     copies of reference values.  This includes the names of the */
/*     BFF parameters, the names of the ARCH parameters, and the */
/*     local copy of the FTP string. */

    if (first) {

/*        Construct and store the NULL valued byte. */

	*(unsigned char *)null = '\0';

/*        Retrieve the BFF and ARCH names. */

	for (i__ = 1; i__ <= 4; ++i__) {
	    zzddhgsd_("BFF", &i__, strbff + (((i__1 = i__ - 1) < 5 && 0 <= 
		    i__1 ? i__1 : s_rnge("strbff", i__1, "zzddhppf_", (ftnlen)
		    508)) << 3), (ftnlen)3, (ftnlen)8);
	}
	for (i__ = 1; i__ <= 2; ++i__) {
	    zzddhgsd_("ARCH", &i__, strarc + (((i__1 = i__ - 1) < 2 && 0 <= 
		    i__1 ? i__1 : s_rnge("strarc", i__1, "zzddhppf_", (ftnlen)
		    512)) << 3), (ftnlen)4, (ftnlen)8);
	}

/*        Extend STRBFF to include the null BFFID.  This addresses */
/*        the N0051 Sun Solaris Native C toolkit binary files. */

	for (i__ = 1; i__ <= 8; ++i__) {
	    *(unsigned char *)&strbff[i__ + 31] = *(unsigned char *)null;
	}

/*        Fetch the FTP string. */

	zzftpstr_(ftpmem, ftplft, ftprgt, ftpdlm, (ftnlen)16, (ftnlen)6, (
		ftnlen)6, (ftnlen)1);

/*        Set FIRST to FALSE so we will not reassign any of these values. */

	first = FALSE_;
    }

/*     Get the simple consistency checks out of the way first.  Is */
/*     the input ARCH value valid? */

    if (*arch <= 0 || *arch > 2) {
	*bff = 0;
	setmsg_("The integer code, '#' indicating the file architecture to e"
		"xamine is out of range.", (ftnlen)82);
	errint_("#", arch, (ftnlen)1);
	sigerr_("SPICE(UNKNOWNFILARC)", (ftnlen)20);
	chkout_("ZZDDHPPF", (ftnlen)8);
	return 0;
    }

/*     Read the first record from the file as a string of NUMCHR */
/*     characters. */

    io___11.ciunit = *unit;
    iostat = s_rdue(&io___11);
    if (iostat != 0) {
	goto L100001;
    }
    iostat = do_uio(&c__1, chrrec, (ftnlen)1000);
    if (iostat != 0) {
	goto L100001;
    }
    iostat = e_rdue();
L100001:

/*     Check for read failure. */

    if (iostat != 0) {
	*bff = 0;
	setmsg_("Error reading the file record from the binary DAF file '#'."
		"  IOSTAT = #.", (ftnlen)72);
	errfnm_("#", unit, (ftnlen)1);
	errint_("#", &iostat, (ftnlen)1);
	sigerr_("SPICE(FILEREADFAILED)", (ftnlen)21);
	chkout_("ZZDDHPPF", (ftnlen)8);
	return 0;
    }

/*     First check the ID word from the input file. */

    idw2at_(chrrec, filarc, filtyp, (ftnlen)8, (ftnlen)4, (ftnlen)4);

/*     Now locate FILARC in the STRARC array. */

    tstarc = isrchc_(filarc, &c__2, strarc, (ftnlen)4, (ftnlen)8);

/*     If FILARC was not found, signal an appropriate error. */

    if (tstarc == 0) {
	*bff = 0;
	setmsg_("The file, #, has a unidentified file architecture.  Check t"
		"hat this file is a properly created binary SPICE kernel.", (
		ftnlen)115);
	errfnm_("#", unit, (ftnlen)1);
	sigerr_("SPICE(UNKNOWNFILARC)", (ftnlen)20);
	chkout_("ZZDDHPPF", (ftnlen)8);
	return 0;

/*     Otherwise we have an architecture mismatch error, if */
/*     FILARC does not agree with ARCH. */

    } else if (tstarc != *arch) {
	*bff = 0;
	setmsg_("A request to load the # file, $, has been made by the % sys"
		"tem.  This operation is not permitted.", (ftnlen)97);
	errch_("#", strarc + (((i__1 = tstarc - 1) < 2 && 0 <= i__1 ? i__1 : 
		s_rnge("strarc", i__1, "zzddhppf_", (ftnlen)612)) << 3), (
		ftnlen)1, (ftnlen)8);
	errfnm_("$", unit, (ftnlen)1);
	errch_("%", strarc + (((i__1 = *arch - 1) < 2 && 0 <= i__1 ? i__1 : 
		s_rnge("strarc", i__1, "zzddhppf_", (ftnlen)614)) << 3), (
		ftnlen)1, (ftnlen)8);
	sigerr_("SPICE(FILARCHMISMATCH)", (ftnlen)22);
	chkout_("ZZDDHPPF", (ftnlen)8);
	return 0;
    }

/*     Now check for possible FTP transfer errors. */

    zzftpchk_(chrrec + 499, &ftperr, (ftnlen)501);
    if (ftperr) {
	*bff = 0;
	setmsg_("FTP transfer error detected.  This binary $, '#', has most "
		"likely been corrupted by an ASCII mode FTP transfer. Obtain "
		"the file using IMAGE or BINARY transfer mode from the source."
		, (ftnlen)180);
	errch_("$", strarc + (((i__1 = tstarc - 1) < 2 && 0 <= i__1 ? i__1 : 
		s_rnge("strarc", i__1, "zzddhppf_", (ftnlen)635)) << 3), (
		ftnlen)1, (ftnlen)8);
	errfnm_("#", unit, (ftnlen)1);
	sigerr_("SPICE(FTPXFERERROR)", (ftnlen)19);
	chkout_("ZZDDHPPF", (ftnlen)8);
	return 0;
    }

/*     Now this search is redundant, but the presence of the */
/*     FTPLFT string in the latter half of the file record */
/*     is fairly conclusive evidence that this is a "new" binary, */
/*     and we can expect to locate the binary file format */
/*     identification string. */

    ftppos = pos_(chrrec + 499, ftplft, &c__1, (ftnlen)501, (ftnlen)6);

/*     Check to see if we found FTPLFT.  If so extract the binary */
/*     file format ID word from the file record. */

    if (ftppos != 0) {

/*        Extract BFFIDW from CHRREC. */

	if (*arch == 1) {
	    s_copy(bffidw, chrrec + 88, (ftnlen)8, (ftnlen)8);
	} else if (*arch == 2) {
	    s_copy(bffidw, chrrec + 84, (ftnlen)8, (ftnlen)8);
	}

/*        See if we can find BFFIDW in the STRBFF list. */

	*bff = isrchc_(bffidw, &c__5, strbff, (ftnlen)8, (ftnlen)8);

/*        Check to see if BFF is 0, if it is, signal an error since */
/*        this indicates an unrecognized BFF. */

	if (*bff == 0) {
	    setmsg_("The file '#' utilizes the binary file format '#'.  This"
		    " format is currently unknown to this toolkit.  A toolkit"
		    " update may be in order.", (ftnlen)135);
	    errfnm_("#", unit, (ftnlen)1);
	    errch_("#", bffidw, (ftnlen)1, (ftnlen)8);
	    sigerr_("SPICE(UNKNOWNBFF)", (ftnlen)17);
	    chkout_("ZZDDHPPF", (ftnlen)8);
	    return 0;
	}

/*        See if we have a NULLID situation, if not check out and */
/*        return as swe have identified the BFF. */

	if (*bff != 5) {
	    chkout_("ZZDDHPPF", (ftnlen)8);
	    return 0;
	}
    }

/*     There is no FTP string, if the file is a DAS, we have to */
/*     assume it is of the native architecture. */

    if (*arch == 2) {
	zzplatfm_("FILE_FORMAT", bffidw, (ftnlen)11, (ftnlen)8);
	ucase_(bffidw, bffidw, (ftnlen)8, (ftnlen)8);
	*bff = isrchc_(bffidw, &c__4, strbff, (ftnlen)8, (ftnlen)8);
	if (*bff == 0) {
	    setmsg_("The native architecture for this platform is unknown to"
		    " this version of the toolkit. This is a severe problem t"
		    "hat should never occur, please contact NAIF.", (ftnlen)
		    155);
	    sigerr_("SPICE(BUG)", (ftnlen)10);
	    chkout_("ZZDDHPPF", (ftnlen)8);
	    return 0;
	}
	chkout_("ZZDDHPPF", (ftnlen)8);
	return 0;
    }

/*     If we reach this point, then we are either dealing with */
/*     an old DAF (created by a pre-N0050 toolkit) or one of the */
/*     DAFs created by the N0051 Sun Solaris Native C version of */
/*     the toolkit.  This requires an examination of the bits */
/*     and bytes in the file that works this way: */

/*        Since in a valid DAF, 2 <= NI <= 250, we can easily */
/*        determine whether the 4 bytes used to store NI in the */
/*        file record are little or big endian.  If we discover */
/*        that the integer is encoded as big-endian, then stop */
/*        as this file must be 'BIG-IEEE'.  If it is little */
/*        endian, then locate the first descriptor record */
/*        in the file. */

/*        Read the first descriptor record.  Extract NSUM, the */
/*        3rd DP from the record.  If it is 0.0D0, signal an error */
/*        as this is an empty DAF and we can not determine its */
/*        type.  If it's non-zero, then check to see if the first */
/*        4 bytes are "0s".  If they are it must be 'LTL-IEEE'. */
/*        Otherwise pass it off to ZZDDHIVF to discriminate between */
/*        'VAX-GFLT' and 'VAX-DFLT'.  We know the first 4 bytes must */
/*        be "0s" in the 'LTL-IEEE" case, since NSUM is subject to */
/*        the following inequality: 1 <= NSUM <= 125 */

/*     Having laid out the scheme, let's get to it.  First take a */
/*     look at the four character bytes that hold NI.  These bytes */
/*     be one of the following: */

/*        Little Endian:  VAL, 0, 0, 0 */
/*           Big Endian:    0, 0, 0, VAL */

/*     where VAL is some non-zero value. */

    if (*(unsigned char *)&chrrec[12] == *(unsigned char *)null && *(unsigned 
	    char *)&chrrec[13] == *(unsigned char *)null && *(unsigned char *)
	    &chrrec[14] == *(unsigned char *)null && *(unsigned char *)&
	    chrrec[15] != *(unsigned char *)null) {
	*bff = 1;
    } else if (*(unsigned char *)&chrrec[12] != *(unsigned char *)null && *(
	    unsigned char *)&chrrec[13] == *(unsigned char *)null && *(
	    unsigned char *)&chrrec[14] == *(unsigned char *)null && *(
	    unsigned char *)&chrrec[15] == *(unsigned char *)null) {

/*        At this point we know we are dealing with a little endian */
/*        file.  Locate the first descriptor record. */

	*(unsigned char *)&ch__1[0] = *(unsigned char *)&chrrec[76];
	fdrec = *(unsigned char *)&ch__1[0];
	*(unsigned char *)&ch__1[0] = *(unsigned char *)&chrrec[77];
	fdrec = (*(unsigned char *)&ch__1[0] << 4) + fdrec;
	*(unsigned char *)&ch__1[0] = *(unsigned char *)&chrrec[78];
	fdrec = (*(unsigned char *)&ch__1[0] << 8) + fdrec;
	*(unsigned char *)&ch__1[0] = *(unsigned char *)&chrrec[79];
	fdrec = (*(unsigned char *)&ch__1[0] << 12) + fdrec;

/*        Read the record into CHRREC. */

	io___20.ciunit = *unit;
	io___20.cirec = fdrec;
	iostat = s_rdue(&io___20);
	if (iostat != 0) {
	    goto L100002;
	}
	iostat = do_uio(&c__1, chrrec, (ftnlen)1000);
	if (iostat != 0) {
	    goto L100002;
	}
	iostat = e_rdue();
L100002:

/*        Check for read failure. */

	if (iostat != 0) {
	    *bff = 0;
	    setmsg_("Error reading a descriptor record from the binary DAF f"
		    "ile '#'.  IOSTAT = #.", (ftnlen)76);
	    errfnm_("#", unit, (ftnlen)1);
	    errint_("#", &iostat, (ftnlen)1);
	    sigerr_("SPICE(FILEREADFAILED)", (ftnlen)21);
	    chkout_("ZZDDHPPF", (ftnlen)8);
	    return 0;
	}

/*        Now examine the NSUM DP in this record to determine the */
/*        architecture. */

	if (*(unsigned char *)&chrrec[16] == *(unsigned char *)null && *(
		unsigned char *)&chrrec[17] == *(unsigned char *)null && *(
		unsigned char *)&chrrec[18] == *(unsigned char *)null && *(
		unsigned char *)&chrrec[19] == *(unsigned char *)null && *(
		unsigned char *)&chrrec[20] == *(unsigned char *)null && *(
		unsigned char *)&chrrec[21] == *(unsigned char *)null && *(
		unsigned char *)&chrrec[22] == *(unsigned char *)null && *(
		unsigned char *)&chrrec[23] == *(unsigned char *)null) {

/*           In this case we have an empty DAF, and can not distinguish */
/*           between little endian formats.  Signal an error and return. */

	    *bff = 0;
	    setmsg_("The DAF, '#', appears to contain no data.  As such, its"
		    " binary file format can not be determined which prevents"
		    " it from being loaded.", (ftnlen)133);
	    errfnm_("#", unit, (ftnlen)1);
	    sigerr_("SPICE(UNKNOWNBFF)", (ftnlen)17);
	    chkout_("ZZDDHPPF", (ftnlen)8);
	    return 0;
	} else if (*(unsigned char *)&chrrec[16] == *(unsigned char *)null && 
		*(unsigned char *)&chrrec[17] == *(unsigned char *)null && *(
		unsigned char *)&chrrec[18] == *(unsigned char *)null && *(
		unsigned char *)&chrrec[19] == *(unsigned char *)null) {

/*           In this case the file is little endian IEEE.  Set BFF. */

	    *bff = 2;
	} else {

/*           We are probably looking at a VAX file.  Find out which */
/*           format. */

	    zzddhivf_(chrrec + 16, bff, &found, (ftnlen)8);
	    if (! found) {
		*bff = 0;
		setmsg_("Unable to determine the binary file format of DAF '"
			"#'.", (ftnlen)54);
		errfnm_("#", unit, (ftnlen)1);
		sigerr_("SPICE(UNKNOWNBFF)", (ftnlen)17);
		chkout_("ZZDDHPPF", (ftnlen)8);
		return 0;
	    }
	}
    } else {
	*bff = 0;
    }
    chkout_("ZZDDHPPF", (ftnlen)8);
    return 0;
} /* zzddhppf_ */

