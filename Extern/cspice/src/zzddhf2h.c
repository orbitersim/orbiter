/* zzddhf2h.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure ZZDDHF2H ( Private --- DDH Filename to Handle ) */
/* Subroutine */ int zzddhf2h_(char *fname, integer *ftabs, integer *ftamh, 
	integer *ftarc, integer *ftbff, integer *fthan, char *ftnam, integer *
	ftrtm, doublereal *ftmnm, integer *nft, integer *utcst, integer *
	uthan, logical *utlck, integer *utlun, integer *nut, logical *exists, 
	logical *opened, integer *handle, logical *found, doublereal *mnm, 
	ftnlen fname_len, ftnlen ftnam_len)
{
    /* System generated locals */
    olist o__1;
    cllist cl__1;
    inlist ioin__1;

    /* Builtin functions */
    integer s_cmp(char *, char *, ftnlen, ftnlen), f_inqu(inlist *), f_open(
	    olist *), f_clos(cllist *);

    /* Local variables */
    integer unit;
    extern doublereal zzddhmnm_(integer *);
    extern /* Subroutine */ int zzddhgtu_(integer *, integer *, logical *, 
	    integer *, integer *, integer *), zzddhrmu_(integer *, integer *, 
	    integer *, integer *, logical *, integer *, integer *);
    integer i__;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    integer rchar;
    extern /* Subroutine */ int errch_(char *, char *, ftnlen, ftnlen);
    extern integer rtrim_(char *, ftnlen);
    extern logical failed_(void);
    extern integer isrchi_(integer *, integer *, integer *);
    logical locopn;
    extern /* Subroutine */ int sigerr_(char *, ftnlen);
    integer uindex;
    logical locexs;
    extern /* Subroutine */ int chkout_(char *, ftnlen), setmsg_(char *, 
	    ftnlen);
    integer iostat;
    extern /* Subroutine */ int errint_(char *, integer *, ftnlen);
    extern logical return_(void);

/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines.  Users should not call this routine directly due */
/*     to the volatile nature of this routine. */

/*     Convert filename to a handle. */

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
/*     FNAME      I   Name of the file to convert to a handle. */
/*     FTABS, */
/*     FTAMH, */
/*     FTARC, */
/*     FTBFF, */
/*     FTHAN, */
/*     FTNAM, */
/*     FTRTM, */
/*     FTMNM      I   File table. */
/*     NFT        I   Number of entries in the file table. */
/*     UTCST, */
/*     UTHAN, */
/*     UTLCK, */
/*     UTLUN     I/O  Unit table. */
/*     NUT       I/O  Number of entries in the unit table. */
/*     EXISTS     O   Logical indicating if FNAME exists. */
/*     OPENED     O   Logical indicating if FNAME is opened. */
/*     HANDLE     O   Handle associated with FNAME. */
/*     FOUND      O   Logical indicating if FNAME's HANDLE was found. */
/*     MNM        O   Unique DP (Magic NuMber) associated with FNAME. */

/* $ Detailed_Input */

/*     FNAME      is the name of the file to locate in the file table. */

/*     FTABS, */
/*     FTAMH, */
/*     FTARC, */
/*     FTBFF, */
/*     FTHAN, */
/*     FTNAM, */
/*     FTRTM, */
/*     FTMNM      are the arrays respectively containing the absolute */
/*                value of the handle, access method, architecture, */
/*                binary file format, handle, name, RTRIM and */
/*                magic number columns of the file table. */

/*     NFT        is the number of entries in the file table. */

/*     UTCST, */
/*     UTHAN, */
/*     UTLCK, */
/*     UTLUN      are the arrays respectively containing the cost, */
/*                handle, locked, and logical unit columns of the unit */
/*                table. */

/*     NUT        is the number of entries in the unit table. */

/* $ Detailed_Output */

/*     UTCST, */
/*     UTHAN, */
/*     UTLCK, */
/*     UTLUN      are the arrays respectively containing the cost, */
/*                handle, locked, and logical unit columns of the unit */
/*                table.  If ZZDDHF2H requires a logical unit, then */
/*                it will borrow one from the unit table.  Depending */
/*                on the state of the table passed in from the caller */
/*                one of three possible scenarios may occur (Recall */
/*                that 'zero-cost' rows are ones whose units are */
/*                reserved with RESLUN and not currently connected */
/*                to any file.) */

/*                   A 'zero-cost' row exists in the table, in */
/*                   which case the row is used temporarily and */
/*                   may be removed depending on the number of entries */
/*                   in the file table (NFT). */

/*                   The unit table is full (NUT=UTSIZE), in which */
/*                   case the unit with the lowest cost that is not */
/*                   locked to its handle will be disconnected, used, */
/*                   and then returned to the table as a 'zero-cost' */
/*                   row before returning to the caller. */

/*                   The unit table is not full (NUT<UTSIZE) and there */
/*                   are no 'zero-cost' rows.  In this case NUT is */
/*                   temporarily increased by one, and the new row */
/*                   is used.  After this routine no longer requires */
/*                   the unit, depending on the number of entries in */
/*                   the file table (NFT) the row may be left in the */
/*                   table as a 'zero-handle' row or removed entirely. */

/*                In the event an error is signaled, the contents of the */
/*                unit table are placed into a usable state before */
/*                returning to the caller. */

/*     NUT        is the number of entries in the unit table. Since */
/*                this routine borrows a unit from the unit table, which */
/*                may involve allocation of a new unit, this value may */
/*                change. */

/*     EXISTS     is a logical if set to TRUE, indicates that FNAME */
/*                exists.  If FALSE, FNAME does not exist.  In the event */
/*                an exception is signaled the value is undefined. */

/*     OPENED     is a logical if set to TRUE, indicates that FNAME */
/*                is opened and attached to a logical unit.  If FALSE, */
/*                FNAME is not attached to a unit.  In the event an */
/*                exception is signaled the value is undefined. */

/*     HANDLE     is the handle in the file table associated with */
/*                FNAME.  If FOUND is FALSE, then HANDLE is returned as */
/*                0. */

/*     FOUND      is a logical if TRUE indicates that FNAME was found */
/*                in the file table.  If FALSE indicates that it was not */
/*                located. */

/*     MNM        is a unique (enough) DP number -- the Magic NuMber -- */
/*                associated with FNAME computed by this examining the */
/*                file contents. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1) If any of the INQUIRE statements this routine performs fail, */
/*        the error SPICE(INQUIREFAILED) is signaled. FOUND is set to */
/*        FALSE and HANDLE to 0. */

/*     2) If the attempt to open FNAME fails, then SPICE(FILEOPENFAILED) */
/*        is signaled. FOUND is set to FALSE, and HANDLE to 0. */

/*     3) If FNAME is determined not to be loaded into the file table */
/*        then FOUND is set to FALSE and HANDLE is set to 0. */

/* $ Files */

/*     If the file named by FNAME is not connected to a logical unit, */
/*     this routine will open it for direct access to complete its */
/*     examination. */

/* $ Particulars */

/*     This routine encapsulates the logic necessary to determine if */
/*     a particular filename names a file already loaded into the */
/*     DAF/DAS handle manager.  If it discovers the file is loaded, */
/*     the routine returns the handle to the caller. */

/* $ Examples */

/*     See ZZDDHFNH for sample usage. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     F.S. Turner     (JPL) */
/*     E.D. Wright     (JPL) */
/*     B.V. Semenov    (JPL) */

/* $ Version */

/* -    SPICELIB Version 3.0.1, 02-OCT-2021 (NJB) */

/*        Corrected typos in comments. */

/* -    SPICELIB Version 3.0.0, 26-APR-2012 (BVS) */

/*        Changed calling sequence to include FTMNM and MNM. Change */
/*        algorithm to compute MNM and use it to bypass n^2 INQUIREs */
/*        for files opened for READ access, if possible. */

/* -    SPICELIB Version 2.0.1, 24-APR-2003 (EDW) */

/*        Added MAC-OSX-F77 to the list of platforms */
/*        that require READONLY to read write protected */
/*        kernels. */

/* -    SPICELIB Version 2.0.0, 05-AUG-2002 (FST) */

/*        Bug fix: this module was updated to allow proper loading */
/*        of read-only files on VAX environments. */

/* -    SPICELIB Version 1.0.0, 04-OCT-2001 (FST) */


/* -& */
/* $ Revisions */

/* -    SPICELIB Version 2.0.0, 05-AUG-2002 (FST) */

/*        An OPEN statement that is exercised by this module under */
/*        certain circumstances, failed to pass the non-standard */
/*        READONLY option for the VAX environments.  This had the */
/*        undesirable side-effect of not permitting files available */
/*        only for READ access to be opened. */

/*        This file was promoted from a standard portable module */
/*        to a master file. */

/* -& */

/*     SPICELIB Functions */


/*     Local Variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("ZZDDHF2H", (ftnlen)8);
    }

/*     First check to see if FNAME is blank.  If so, set FOUND to .FALSE. */
/*     and return.  ZZDDHOPN prevents any blank filenames from being */
/*     loaded into the file table. */

    if (s_cmp(fname, " ", fname_len, (ftnlen)1) == 0) {
	*found = FALSE_;
	*handle = 0;
	*opened = FALSE_;
	*exists = FALSE_;
	chkout_("ZZDDHF2H", (ftnlen)8);
	return 0;
    }

/*     Start by trimming the file name in preparation for the INQUIRE. */

    rchar = rtrim_(fname, fname_len);

/*     Now INQUIRE on the input file FNAME. */

    ioin__1.inerr = 1;
    ioin__1.infilen = rchar;
    ioin__1.infile = fname;
    ioin__1.inex = &locexs;
    ioin__1.inopen = &locopn;
    ioin__1.innum = &unit;
    ioin__1.innamed = 0;
    ioin__1.inname = 0;
    ioin__1.inacc = 0;
    ioin__1.inseq = 0;
    ioin__1.indir = 0;
    ioin__1.infmt = 0;
    ioin__1.inform = 0;
    ioin__1.inunf = 0;
    ioin__1.inrecl = 0;
    ioin__1.innrec = 0;
    ioin__1.inblank = 0;
    iostat = f_inqu(&ioin__1);

/*     Check IOSTAT for failure. */

    if (iostat != 0) {
	*found = FALSE_;
	*handle = 0;
	setmsg_("INQUIRE failed. Value of IOSTAT was #.", (ftnlen)38);
	errint_("#", &iostat, (ftnlen)1);
	sigerr_("SPICE(INQUIREFAILED)", (ftnlen)20);
	chkout_("ZZDDHF2H", (ftnlen)8);
	return 0;
    }

/*     First, set some of the output arguments.  Remember, some */
/*     systems consider non-existent files as open.  Compensate for */
/*     this unusual behavior. */

    *exists = locexs;
    *opened = locopn && *exists;

/*     Now check to see if the file exists.  If it does not, then */
/*     set FOUND to false and HANDLE to 0 as non-existent files */
/*     can not possibly be present in the file table. */

    if (! (*exists)) {
	*found = FALSE_;
	*handle = 0;
	chkout_("ZZDDHF2H", (ftnlen)8);
	return 0;
    }

/*     Now check to see if the file is opened.  If it is, we need to */
/*     determine whether or not the logical unit to which it is */
/*     attached is present in the unit table. */

    if (*opened) {

/*        Since the file is opened, see if we can find its unit */
/*        in the unit table. */

	uindex = isrchi_(&unit, nut, utlun);

/*        When UINDEX is 0, the file is opened, but not by */
/*        the DAF/DAS handle manager.  Set FOUND to FALSE, HANDLE */
/*        to 0, and return to the caller. */

	if (uindex == 0) {
	    *handle = 0;
	    *found = FALSE_;
	    chkout_("ZZDDHF2H", (ftnlen)8);
	    return 0;
	}

/*        If we end up here, then we found UNIT in the unit table. */
/*        Set FOUND to TRUE if the handle associated with UNIT is */
/*        non-zero. */

	*handle = uthan[uindex - 1];
	*found = *handle != 0;
	chkout_("ZZDDHF2H", (ftnlen)8);
	return 0;
    }

/*     At this point, we took action for all simple cases.  Now */
/*     we need to find out if FNAME is one of the files in the */
/*     file table that isn't open.  To determine this, we open FNAME, */
/*     and then INQUIRE on every file in the table.  To do this, we */
/*     need a unit. Get one. */

    zzddhgtu_(utcst, uthan, utlck, utlun, nut, &uindex);
    if (failed_()) {
	*handle = 0;
	*found = FALSE_;
	chkout_("ZZDDHF2H", (ftnlen)8);
	return 0;
    }

/*     Now open the file (which we know exists and isn't open). Since */
/*     we effectively are just borrowing this unit, we are not going to */
/*     set UTHAN or UTCST from the defaults that ZZDDHGTU sets up. */

    o__1.oerr = 1;
    o__1.ounit = utlun[uindex - 1];
    o__1.ofnmlen = rchar;
    o__1.ofnm = fname;
    o__1.orl = 1024;
    o__1.osta = "OLD";
    o__1.oacc = "DIRECT";
    o__1.ofm = 0;
    o__1.oblnk = 0;
    iostat = f_open(&o__1);

/*     Check IOSTAT. */

    if (iostat != 0) {

/*        Since an error has occurred, set FOUND to false and HANDLE */
/*        to 0. */

	*found = FALSE_;
	*handle = 0;

/*        Close the unit and remove it from the unit table. */

	cl__1.cerr = 0;
	cl__1.cunit = utlun[uindex - 1];
	cl__1.csta = 0;
	f_clos(&cl__1);
	zzddhrmu_(&uindex, nft, utcst, uthan, utlck, utlun, nut);

/*        Signal the error and return. */

	setmsg_("Attempt to open file '#' failed. Value of IOSTAT was #.", (
		ftnlen)55);
	errch_("#", fname, (ftnlen)1, fname_len);
	errint_("#", &iostat, (ftnlen)1);
	sigerr_("SPICE(FILEOPENFAILED)", (ftnlen)21);
	chkout_("ZZDDHF2H", (ftnlen)8);
	return 0;
    }

/*     Get a unique enough DP number -- the Magic NuMber (MNM) ;) -- for */
/*     this file. */

    *mnm = zzddhmnm_(&utlun[uindex - 1]);

/*     Now loop through all the files in the file table. Unfortunately */
/*     we have no other choice. */

    i__ = 1;
    *found = FALSE_;
    while(i__ <= *nft && ! (*found)) {

/*        If this file's magic number is non-zero and is different from */
/*        the magic number of the currently checked, opened-for-READ */
/*        file, we will declare that these files are not the same file */
/*        and will skip INQUIRE. In all other cases we will do INQUIRE */
/*        and check UNITs. */

	if (*mnm != 0. && (*mnm != ftmnm[i__ - 1] && ftamh[i__ - 1] == 1)) {

/*           These files are not the same file. Clear IOSTAT and set */
/*           UNIT to not match the UNIT of the input file. */

	    iostat = 0;
	    unit = utlun[uindex - 1] + 1;
	} else {

/*           Do the INQUIRE. ;( */

	    ioin__1.inerr = 1;
	    ioin__1.infilen = ftrtm[i__ - 1];
	    ioin__1.infile = ftnam + (i__ - 1) * ftnam_len;
	    ioin__1.inex = &locexs;
	    ioin__1.inopen = &locopn;
	    ioin__1.innum = &unit;
	    ioin__1.innamed = 0;
	    ioin__1.inname = 0;
	    ioin__1.inacc = 0;
	    ioin__1.inseq = 0;
	    ioin__1.indir = 0;
	    ioin__1.infmt = 0;
	    ioin__1.inform = 0;
	    ioin__1.inunf = 0;
	    ioin__1.inrecl = 0;
	    ioin__1.innrec = 0;
	    ioin__1.inblank = 0;
	    iostat = f_inqu(&ioin__1);
	}

/*        Check IOSTAT. */

	if (iostat != 0) {

/*           Since we have an error condition, set FOUND to FALSE */
/*           and HANDLE to 0. */

	    *found = FALSE_;
	    *handle = 0;

/*           Close the unit and clean up the unit table. */

	    cl__1.cerr = 0;
	    cl__1.cunit = utlun[uindex - 1];
	    cl__1.csta = 0;
	    f_clos(&cl__1);
	    zzddhrmu_(&uindex, nft, utcst, uthan, utlck, utlun, nut);

/*           Signal the error and return. */

	    setmsg_("INQUIRE failed. Value of IOSTAT was #.", (ftnlen)38);
	    errint_("#", &iostat, (ftnlen)1);
	    sigerr_("SPICE(INQUIREFAILED)", (ftnlen)20);
	    chkout_("ZZDDHF2H", (ftnlen)8);
	    return 0;
	}

/*        Now check to see if FILE exists, is currently open. and */
/*        its UNIT matches UTLUN(UINDEX). */

	if (locexs && locopn && unit == utlun[uindex - 1]) {
	    *handle = fthan[i__ - 1];
	    *found = TRUE_;

/*        Otherwise, continue searching. */

	} else {
	    ++i__;
	}
    }

/*     Check to see if we found the file in the file table. */

    if (! (*found)) {
	*handle = 0;
    }

/*     Close the unit and clean up the unit table. */

    cl__1.cerr = 0;
    cl__1.cunit = utlun[uindex - 1];
    cl__1.csta = 0;
    f_clos(&cl__1);
    zzddhrmu_(&uindex, nft, utcst, uthan, utlck, utlun, nut);
    chkout_("ZZDDHF2H", (ftnlen)8);
    return 0;
} /* zzddhf2h_ */

