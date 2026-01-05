/* zzddhgtu.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure ZZDDHGTU ( Private --- DDH Get Unit ) */
/* Subroutine */ int zzddhgtu_(integer *utcst, integer *uthan, logical *utlck,
	 integer *utlun, integer *nut, integer *uindex)
{
    /* System generated locals */
    integer i__1;
    cllist cl__1;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer), f_clos(cllist *);

    /* Local variables */
    logical done;
    integer i__;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    extern logical failed_(void);
    extern /* Subroutine */ int orderi_(integer *, integer *, integer *), 
	    frelun_(integer *), sigerr_(char *, ftnlen), getlun_(integer *), 
	    chkout_(char *, ftnlen);
    integer orderv[23];
    extern /* Subroutine */ int setmsg_(char *, ftnlen);
    extern logical return_(void);

/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines.  Users should not call this routine directly due */
/*     to the volatile nature of this routine. */

/*     Get or prepare an entry in the unit table to receive a new */
/*     file. */

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
/*     UTCST, */
/*     UTHAN, */
/*     UTLCK, */
/*     UTLUN,    I/O   Unit table. */
/*     NUT       I/O   Number of entries in the unit table. */
/*     UINDEX     O    Row in the unit table that can be replaced. */

/* $ Detailed_Input */

/*     UTCST, */
/*     UTHAN, */
/*     UTLCK, */
/*     UTLUN,     are the arrays respectively containing the cost, */
/*                handle, locked, and logical unit columns of the */
/*                unit table. */

/*     NUT        is the number of entries in the unit table. */

/* $ Detailed_Output */

/*     UTCST, */
/*     UTHAN, */
/*     UTLCK, */
/*     UTLUN,     are the arrays respectively containing the cost, */
/*                handle, locked, and logical unit columns of the */
/*                unit table.  This may change as a new unit is */
/*                added or old ones are removed. */

/*     NUT        is the number of entries in the unit table.  This may */
/*                change as new entries are added. */

/*     UINDEX     is the index of the row where the new unit should */
/*                be attached. */

/* $ Parameters */

/*     None. */

/* $ Files */

/*     This routine may disconnect a file from its logical unit, to */
/*     successfully process the caller's request for a unit. */

/* $ Exceptions */

/*     1) If GETLUN fails to assign a logical unit for any reason to */
/*        the row of interest, this routine sets the logical unit to -1, */
/*        since negative logical units in Fortran are not permitted. */

/* $ Particulars */

/*      This routine only manipulates the contents of the unit table. */
/*      Any "zero" cost rows in the table indicate rows where the */
/*      listed logical unit has been reserved, but no file is currently */
/*      attached. */

/*      Callers of this routine should check FAILED since this */
/*      routine may invoke GETLUN. */

/* $ Examples */

/*     See ZZDDHHLU for sample usage. */

/* $ Restrictions */

/*     1) This routine must not be used to retrieve a unit for a */
/*        file that is already connected to a unit listed in the */
/*        unit table. */

/* $ Author_and_Institution */

/*     F.S. Turner     (JPL) */

/* $ Literature_References */

/*     None. */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 29-MAY-2001 (FST) */


/* -& */

/*     SPICELIB Functions */


/*     Local Variables */


/*     Standard SPICE discovery error handling. */

    if (return_()) {
	return 0;
    }

/*     First check the case when the unit table is completely empty. */

    if (*nut == 0) {
	*nut = 1;
	*uindex = 1;
	utcst[*uindex - 1] = 0;
	uthan[*uindex - 1] = 0;
	utlck[*uindex - 1] = FALSE_;
	getlun_(&utlun[*uindex - 1]);

/*        Check FAILED to see if GETLUN signaled an error.  If so, then */
/*        return an invalid unit to the caller. */

	if (failed_()) {
	    utlun[*uindex - 1] = -1;
	    return 0;
	}

/*        If we end up here, then GETLUN succeeded and we have the new */
/*        unit.  Now return. */

	return 0;
    }

/*     If we reach here, then the table contains at least one entry. */
/*     Order the table rows by cost. */

    orderi_(utcst, nut, orderv);

/*     Now check to for '0' cost rows as this indicates rows whose */
/*     logical units are reserved for this suite of routines usage, */
/*     but are not currently assigned a file. */

    if (utcst[orderv[0] - 1] <= 0) {
	*uindex = orderv[0];

/*        '0' cost rows end up in the unit table as the result of a */
/*        row deletion, occurring when excess files are present. */
/*        When this process occurs, the logical unit listed in this */
/*        row is reserved for this module's usage only with RESLUN. */
/*        Free it, since we're about to reassign it. */

	frelun_(&utlun[*uindex - 1]);
	return 0;
    }

/*     Now if no '0' cost rows exist, check to see if we can */
/*     expand the table. */

    if (*nut < 23) {

/*        Now increment NUT and set UINDEX. */

	++(*nut);
	*uindex = *nut;

/*        Prepare the default values for the new row. */

	utcst[*uindex - 1] = 0;
	uthan[*uindex - 1] = 0;
	utlck[*uindex - 1] = FALSE_;
	getlun_(&utlun[*uindex - 1]);

/*        Check FAILED to see if GETLUN signaled an error.  If so, then */
/*        return an invalid unit to the caller. */

	if (failed_()) {
	    utlun[*uindex - 1] = -1;
	    return 0;
	}

/*        If we end up here, then GETLUN worked properly.  Now return. */

	return 0;
    }

/*     If we reach here, then we have no zero-cost rows and a full unit */
/*     table.  Now it's time to determine which entry in the table to */
/*     bump.  We do this by stepping through the order vector until */
/*     we find the first 'non-locked' row. */

    i__ = 0;
    done = FALSE_;
    while(! done && i__ != *nut) {
	++i__;
	done = ! utlck[orderv[(i__1 = i__ - 1) < 23 && 0 <= i__1 ? i__1 : 
		s_rnge("orderv", i__1, "zzddhgtu_", (ftnlen)279)] - 1];
    }

/*     Before going any further, signal an error if we discover */
/*     we have not found a row. */

    if (! done) {
	*uindex = 0;
	chkin_("ZZDDHGTU", (ftnlen)8);
	setmsg_("The unit table is full and all entries are locked.  This sh"
		"ould never happen. Contact NAIF.", (ftnlen)91);
	sigerr_("SPICE(BUG)", (ftnlen)10);
	chkout_("ZZDDHGTU", (ftnlen)8);
	return 0;
    }

/*     Clear UTCST and UTHAN since we intend to disconnect */
/*     the unit upon return. */

    utcst[orderv[(i__1 = i__ - 1) < 23 && 0 <= i__1 ? i__1 : s_rnge("orderv", 
	    i__1, "zzddhgtu_", (ftnlen)304)] - 1] = 0;
    uthan[orderv[(i__1 = i__ - 1) < 23 && 0 <= i__1 ? i__1 : s_rnge("orderv", 
	    i__1, "zzddhgtu_", (ftnlen)305)] - 1] = 0;

/*     Set UINDEX and CLSLUN, then return. */

    *uindex = orderv[(i__1 = i__ - 1) < 23 && 0 <= i__1 ? i__1 : s_rnge("ord"
	    "erv", i__1, "zzddhgtu_", (ftnlen)310)];

/*     At this point we need to close the unit from the row of interest. */

    cl__1.cerr = 0;
    cl__1.cunit = utlun[*uindex - 1];
    cl__1.csta = 0;
    f_clos(&cl__1);
    return 0;
} /* zzddhgtu_ */

