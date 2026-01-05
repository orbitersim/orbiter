/* zzscad01.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__8 = 8;
static integer c__0 = 0;
static integer c__10 = 10;
static integer c__1000 = 1000;
static integer c__1001 = 1001;
static integer c_b28 = 320018;
static integer c_b31 = 320019;
static integer c__1 = 1;

/* $Procedure ZZSCAD01 ( SPICE private, add clock to database, type 01 ) */
/* Subroutine */ int zzscad01_(integer *sc, integer *hdsclk, integer *scpool, 
	integer *clklst, integer *dpfree, doublereal *dpbuff, integer *ifree, 
	integer *intbuf, integer *scbase, integer *sclkat)
{
    /* Initialized data */

    static char namlst[60*8] = "SCLK01_COEFFICIENTS                         "
	    "                " "SCLK_PARTITION_START                         "
	    "               " "SCLK_PARTITION_END                            "
	    "              " "SCLK01_OFFSETS                                 "
	    "             " "SCLK01_MODULI                                   "
	    "            " "SCLK01_N_FIELDS                                  "
	    "           " "SCLK01_OUTPUT_DELIM                               "
	    "          " "SCLK01_TIME_SYSTEM                                 "
	    "         ";
    static integer kvmaxn[8] = { 300000,9999,9999,10,10,1,1,1 };
    static integer ibix[8] = { 6,7,8,10,9,0,0,0 };

    /* System generated locals */
    integer i__1, i__2, i__3;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    integer noff, nmod, nsys;
    extern /* Subroutine */ int zzhsiadd_(integer *, integer *, integer *, 
	    integer *, integer *, logical *), zzhsiavl_(integer *, integer *);
    integer i__, n, ibase;
    extern /* Subroutine */ int scld01_(char *, integer *, integer *, integer 
	    *, doublereal *, ftnlen), scli01_(char *, integer *, integer *, 
	    integer *, integer *, ftnlen), chkin_(char *, ftnlen), errch_(
	    char *, char *, ftnlen, ftnlen), movec_(char *, integer *, char *,
	     ftnlen, ftnlen);
    logical found;
    extern /* Subroutine */ int repmi_(char *, char *, integer *, char *, 
	    ftnlen, ftnlen, ftnlen);
    integer npart, iroom, prvsc;
    extern logical failed_(void);
    integer dpbase, ncoeff;
    char kvname[60*8];
    integer dproom;
    extern logical return_(void);
    char shrtms[25], kvtype[1*8];
    integer kvsize[8], ndpval, npartb, nparte, nscavl;
    extern /* Subroutine */ int suffix_(char *, integer *, char *, ftnlen, 
	    ftnlen), chkout_(char *, ftnlen), setmsg_(char *, ftnlen), 
	    errint_(char *, integer *, ftnlen), sigerr_(char *, ftnlen), 
	    dtpool_(char *, logical *, integer *, char *, ftnlen, ftnlen);
    logical new__;
    extern /* Subroutine */ int zzscin01_(integer *, integer *, integer *, 
	    integer *, integer *, integer *);

/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines. Users should not call this routine directly due */
/*     to the volatile nature of this routine. */

/*     Add to the SCLK type 01 database data for a specified SCLK. */

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

/*     SCLK */

/* $ Keywords */

/*     SCLK */
/*     TIME */

/* $ Declarations */
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


/*     Include File:  SPICELIB Error Handling Parameters */

/*        errhnd.inc  Version 2    18-JUN-1997 (WLT) */

/*           The size of the long error message was */
/*           reduced from 25*80 to 23*80 so that it */
/*           will be accepted by the Microsoft Power Station */
/*           FORTRAN compiler which has an upper bound */
/*           of 1900 for the length of a character string. */

/*        errhnd.inc  Version 1    29-JUL-1997 (NJB) */



/*     Maximum length of the long error message: */


/*     Maximum length of the short error message: */


/*     End Include File:  SPICELIB Error Handling Parameters */


/*     Include file zzsclk01.inc */

/*     SPICE private file intended solely for the support of SPICE */
/*     routines. Users should not include this file directly due */
/*     to the volatile nature of this file. */

/*     Include file for SPICE-private declarations of SC01. */

/* -    Version 1.0.0, 11-NOV-2020 */


/*    Parameters copied from sclk.inc: */

/*        NOTE: these declarations are duplicated in the include file */
/*        sclk.inc. Those declarations must be kept in sync with */
/*        these ones. */


/*     Number of supported SCLK field delimiters: */


/*     Supported SCLK string field delimiters: */


/*     Time system codes */


/*     Maximum number of partitions: */


/*     Maximum number of coefficient records: */


/*     Maximum number of fields in an SCLK string: */


/*     Length of strings used to represent D.P. numbers: */


/*     End of duplicated declarations. */


/*     Indices of integer data items, relative to base index into */
/*     the integer data buffer: */


/*     Index of number of fields */


/*     Index of delimiter code */


/*     Index of time system code */


/*     Index of coefficient count. The count is 3 x the number of */
/*     coefficient records. */


/*     Index of the number of partitions */


/*     Index of the base index in the double precision buffer of the */
/*     coefficient set */


/*     Index of the base index in the double precision buffer of the */
/*     partition start times */


/*     Index of the base index in the double precision buffer of the */
/*     partition end times */


/*     Index of the base index in the double precision buffer of the */
/*     SCLK field moduli */


/*     Index of the base index in the double precision buffer of the */
/*     SCLK field offsets */


/*     Number of integer values per clock */


/*     Data structure parameters */


/*     Maximum number of clocks */


/*     DP buffer size */

/*     The buffer is large enough to hold data for one clock having the */
/*     maximum amount of data. */


/*     Integer buffer size */


/*     Lower bound of control area of singly linked list: */


/*     The add-only hash pool for frame IDs is singly linked. */


/*     End include file zzsc01.inc */

/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     SC         I   SCLK ID code. */
/*     HDSCLK    I-O  Array of heads of hash collision lists. */
/*     SCPOOL    I-O  Singly linked collision list pool. */
/*     CLKLST    I-O  Array of SCLK codes. */
/*     DPFREE    I-O  Index of first free entry in d.p. data buffer. */
/*     DPBUFF    I-O  D.p. (double precision) data buffer */
/*     IFREE     I-O  Index of first free entry in integer data buffer. */
/*     INTBUF    I-O  Integer data buffer. */
/*     SCBASE    I-O  Base index in integer buffer corresponding to SC. */
/*     SCLKAT     O   Index in CLKLST of SC. */

/* $ Detailed_Input */

/*     SC          is the SCLK ID of a type 1 clock. Data for this clock */
/*                 are to be added to the type 1 database. */

/*     HDSCLK      is an array containing the head node indices of the */
/*                 SCLK ID collision lists stored in SCPOOL. */

/*     SCPOOL      is a singly linked list pool containing collision */
/*                 lists for sets of SCLK IDs that have the same hash */
/*                 values. */

/*     CLKLST      is an array of SCLK IDs, which is parallel to the */
/*                 data portion of SCPOOL. Each allocated node of */
/*                 SCPOOL is the index in CLKLST of the associated */
/*                 SCLK ID. */

/*     DPFREE      is index in the database's double precision buffer of */
/*                 the first free element. */

/*     DPBUFF      is the double precision buffer of the type 1 */
/*                 database. */

/*     IFREE       is index in the database's integer buffer of the */
/*                 first free element. */

/*     INTBUF      is the integer buffer of the type 1 database. */

/*     SCBASE      is an array of base addresses in the array INTBUF of */
/*                 the integer data sets associated with the clocks */
/*                 in the type 1 database. The Ith element of SCBASE is */
/*                 the base address of the clock at index I in the */
/*                 array CLKLST. */

/*                 The "base" address of an integer data set immediately */
/*                 precedes the first index of that data set: the index */
/*                 in INTBUF of the first element of the integer data of */
/*                 CLKLST(I) is SCBASE(I)+1. */

/* $ Detailed_Output */

/*     HDSCLK      is the input list head array, updated as needed to */
/*                 reflect the addition of the new clock. Note that the */
/*                 array won't be updated if the clock is added to a */
/*                 pre-existing collision list. */

/*     SCPOOL      is the input singly linked list pool, updated to */
/*                 reflect the addition of the new clock. */

/*     CLKLST      is an array of SCLK IDs, updated to reflect the */
/*                 addition of the new clock. */

/*     DPFREE      is index in the database's double precision buffer of */
/*                 the first free element after addition of data for the */
/*                 new clock. */

/*     DPBUFF      is the double precision buffer of the type 1 */
/*                 database, updated to reflect the addition of the new */
/*                 clock. */

/*     IFREE       is index in the database's integer buffer of the */
/*                 first free element after addition of data for the new */
/*                 clock. */

/*     INTBUF      is the integer buffer of the type 1 database, updated */
/*                 to reflect the addition of the new clock. */

/*     SCBASE      is an array of base addresses in the array INTBUF, */
/*                 updated to reflect the addition of the new clock. */

/*     SCLKAT      is the index in CLKLST at which the input clock ID */
/*                 SC is stored. */

/* $ Parameters */

/*     See the include files zzsc01.inc and zzctr.inc. */

/* $ Exceptions */


/*     1)  If any of the kernel variables containing SCLK coefficients, */
/*         partition start or stop times, moduli, or offsets are not */
/*         found, the error SPICE(KERNELVARNOTFOUND) will be signaled. */

/*     2)  If any integer kernel variables are not found, the error will */
/*         be signaled by a routine in the call tree of this routine. */

/*     3)  If any kernel variable used by this routine does not have */
/*         numeric type, the error will be signaled by a routine in the */
/*         call tree of this routine. */

/*     4)  If the counts of partition start or stop times do not match, */
/*         the error SPICE(NUMPARTSUNEQUAL) will be signaled. */

/*     5)  If the count of partitions exceeds the limit MXPART, */
/*         the error SPICE(TOOMANYPARTITIONS) will be signaled. */

/*     6)  If the count of coefficients exceeds the limit 3*MXCOEF, */
/*         the error SPICE(TOOMANYCOEFFS) will be signaled. */

/*     7)  If any kernel variable other than the coefficient set or */
/*         the partition bounds has size that exceeds the applicable */
/*         limit, the error SPICE(KERNELVARTOOLARGE) will be signaled. */

/*     8)  If an error occurs while this routine looks up data from the */
/*         kernel pool, the error will be signaled by a routine in the */
/*         call tree of this routine. */

/*     9)  If an error occurs while this routine adds data to a hash, */
/*         the error will be signaled by a routine in the call tree of */
/*         this routine. */

/*     10) If the database contains entries for the maximum number */
/*         of clocks and an addition is requested, the database will */
/*         be re-initialized, and an entry for the new clock will be */
/*         created. This case is not an error. */

/* $ Files */

/*     Appropriate kernels must be loaded by the calling program before */
/*     this routine is called. */

/*     The following data are required: */

/*        - An SCLK kernel providing data for the SCLK designated by */
/*          the input ID code SC. */

/*     In all cases, kernel data are normally loaded once per program */
/*     run, NOT every time this routine is called. */

/* $ Particulars */

/*     None. */

/* $ Examples */

/*     None. See usage in ZZSCUP01. */

/* $ Restrictions */

/*     This is a SPICE-private routine. It should not be called directly */
/*     by user application code. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman   (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 01-DEC-2021 (NJB) */

/* -& */
/* $ Index_Entries */

/*     add sclk to type 1 sclk database */

/* -& */
/* $ Revisions */

/*     None. */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     Following are parameters for the indices within the */
/*     array NAMLST of the kernel variable names used by the */
/*     SC01 entry points. */

/*     NOTE: indices for the d.p. variables must be listed first. */


/*     Maximum number of elements in coefficient array: */


/*     Local variables */


/*     Saved values */


/*     Initial values */

    if (return_()) {
	return 0;
    }
    chkin_("ZZSCAD01", (ftnlen)8);

/*     Make up a list of names of kernel variables that we'll use. */
/*     The first name in the list is SCLK_KERNEL_ID, which does not */
/*     require the addition of a spacecraft code suffix.  For the */
/*     rest of the names, we'll have to add the suffix. */

    movec_(namlst, &c__8, kvname, (ftnlen)60, (ftnlen)60);
    for (i__ = 1; i__ <= 8; ++i__) {
	suffix_("_#", &c__0, kvname + ((i__1 = i__ - 1) < 8 && 0 <= i__1 ? 
		i__1 : s_rnge("kvname", i__1, "zzscad01_", (ftnlen)373)) * 60,
		 (ftnlen)2, (ftnlen)60);
	i__3 = -(*sc);
	repmi_(kvname + ((i__1 = i__ - 1) < 8 && 0 <= i__1 ? i__1 : s_rnge(
		"kvname", i__1, "zzscad01_", (ftnlen)374)) * 60, "#", &i__3, 
		kvname + ((i__2 = i__ - 1) < 8 && 0 <= i__2 ? i__2 : s_rnge(
		"kvname", i__2, "zzscad01_", (ftnlen)374)) * 60, (ftnlen)60, (
		ftnlen)1, (ftnlen)60);
    }

/*     Check available room in the clock hash and in the numeric data */
/*     buffers. If there's no room, re-initialize the entire data */
/*     structure. */

    zzhsiavl_(scpool, &nscavl);
    if (nscavl == 0) {
	zzscin01_(hdsclk, scpool, clklst, dpfree, ifree, &prvsc);
    }

/*     Add this clock to the hash. */

    zzhsiadd_(hdsclk, scpool, clklst, sc, sclkat, &new__);
    if (failed_()) {

/*        This code should be unreachable but is provided for safety. */

	zzscin01_(hdsclk, scpool, clklst, dpfree, ifree, &prvsc);
	chkout_("ZZSCAD01", (ftnlen)8);
	return 0;
    }

/*     Check room in the integer buffer. */

    iroom = 1001 - *ifree;
    if (iroom < 10 || iroom > 1000) {

/*        This code should be unreachable but is provided for safety. */

	i__ = *ifree;
	zzscin01_(hdsclk, scpool, clklst, dpfree, ifree, &prvsc);
	setmsg_("IROOM was #; must be in range #:#. IFREE was #; must be in "
		"range 1:#.", (ftnlen)69);
	errint_("#", &iroom, (ftnlen)1);
	errint_("#", &c__10, (ftnlen)1);
	errint_("#", &c__1000, (ftnlen)1);
	errint_("#", &i__, (ftnlen)1);
	errint_("#", &c__1001, (ftnlen)1);
	sigerr_("SPICE(BUG)", (ftnlen)10);
	chkout_("ZZSCAD01", (ftnlen)8);
	return 0;
    }

/*     Compute available room for double precision data. */

    dproom = 320019 - *dpfree;
    if (dproom < 0 || dproom > 320018) {

/*        This code should be unreachable but is provided for safety. */

	i__ = *dpfree;
	zzscin01_(hdsclk, scpool, clklst, dpfree, ifree, &prvsc);
	setmsg_("DPROOM was #; must be in range 0:#. DPFREE was #; must be i"
		"n range 1:#.", (ftnlen)71);
	errint_("#", &dproom, (ftnlen)1);
	errint_("#", &c_b28, (ftnlen)1);
	errint_("#", &i__, (ftnlen)1);
	errint_("#", &c_b31, (ftnlen)1);
	sigerr_("SPICE(BUG)", (ftnlen)10);
	chkout_("ZZSCAD01", (ftnlen)8);
	return 0;
    }

/*     Set the base address for the next item to be added to the */
/*     integer buffer. The index of the first element of the item */
/*     is the successor of IBASE. */

    ibase = *ifree - 1;
    scbase[*sclkat - 1] = ibase;

/*     Fetch from the kernel pool the integer items we need: */

/*        -  The number of fields in an (unabridged) SCLK string */
/*        -  The output delimiter code */
/*        -  The parallel time system code (optional; defaults to TDB) */

    scli01_(namlst + 300, sc, &c__1, &n, &intbuf[ibase], (ftnlen)60);
    scli01_(namlst + 360, sc, &c__1, &n, &intbuf[ibase + 1], (ftnlen)60);
    scli01_(namlst + 420, sc, &c__1, &nsys, &intbuf[ibase + 2], (ftnlen)60);
    if (failed_()) {
	zzscin01_(hdsclk, scpool, clklst, dpfree, ifree, &prvsc);
	chkout_("ZZSCAD01", (ftnlen)8);
	return 0;
    }

/*     The time system code need not be provided in the kernel pool. */
/*     Set it to the default value if it's not present. */

    if (nsys == 0) {
	intbuf[ibase + 2] = 1;
    }

/*     Check for presence in the kernel pool of all required d.p. kernel */
/*     variables. */

/*     Get and check the sizes of the d.p. arrays: coefficients, */
/*     partitions, moduli, and offsets. */

/*     We obtain sizes of d.p. kernel variables rather than using sizes */
/*     returned by SCLD01 because we'll read data directly into our */
/*     passed-in buffers. We need to know in advance how much room is */
/*     needed. */

    for (i__ = 1; i__ <= 5; ++i__) {
	dtpool_(kvname + ((i__1 = i__ - 1) < 8 && 0 <= i__1 ? i__1 : s_rnge(
		"kvname", i__1, "zzscad01_", (ftnlen)505)) * 60, &found, &
		kvsize[(i__2 = i__ - 1) < 8 && 0 <= i__2 ? i__2 : s_rnge(
		"kvsize", i__2, "zzscad01_", (ftnlen)505)], kvtype + ((i__3 = 
		i__ - 1) < 8 && 0 <= i__3 ? i__3 : s_rnge("kvtype", i__3, 
		"zzscad01_", (ftnlen)505)), (ftnlen)60, (ftnlen)1);
	if (failed_()) {

/*           This code should be unreachable but is provided for */
/*           safety. Initialize local data structures. ZZSCIN01 sets */
/*           PRVSC to 0. */

	    zzscin01_(hdsclk, scpool, clklst, dpfree, ifree, &prvsc);
	    chkout_("ZZSCAD01", (ftnlen)8);
	    return 0;
	}

/*        Check that each required d.p. kernel variable was found. */

	if (! found) {
	    zzscin01_(hdsclk, scpool, clklst, dpfree, ifree, &prvsc);
	    setmsg_("Kernel variable # for spacecraft clock # was not found."
		    " An SCLK kernel for this clock may not have been loaded.",
		     (ftnlen)111);
	    errch_("#", kvname + ((i__1 = i__ - 1) < 8 && 0 <= i__1 ? i__1 : 
		    s_rnge("kvname", i__1, "zzscad01_", (ftnlen)532)) * 60, (
		    ftnlen)1, (ftnlen)60);
	    errint_("#", sc, (ftnlen)1);
	    sigerr_("SPICE(KERNELVARNOTFOUND)", (ftnlen)24);
	    chkout_("ZZSCAD01", (ftnlen)8);
	    return 0;
	}

/*        Check the number of values associated with the kernel */
/*        variable. */

	if (kvsize[(i__1 = i__ - 1) < 8 && 0 <= i__1 ? i__1 : s_rnge("kvsize",
		 i__1, "zzscad01_", (ftnlen)544)] > kvmaxn[(i__2 = i__ - 1) < 
		8 && 0 <= i__2 ? i__2 : s_rnge("kvmaxn", i__2, "zzscad01_", (
		ftnlen)544)]) {
	    zzscin01_(hdsclk, scpool, clklst, dpfree, ifree, &prvsc);
	    setmsg_("The number of values associated with the kernel variabl"
		    "e # for clock # is #, which exceeds the limit #.", (
		    ftnlen)103);
	    errch_("#", kvname + ((i__1 = i__ - 1) < 8 && 0 <= i__1 ? i__1 : 
		    s_rnge("kvname", i__1, "zzscad01_", (ftnlen)552)) * 60, (
		    ftnlen)1, (ftnlen)60);
	    errint_("#", sc, (ftnlen)1);
	    errint_("#", &kvsize[(i__1 = i__ - 1) < 8 && 0 <= i__1 ? i__1 : 
		    s_rnge("kvsize", i__1, "zzscad01_", (ftnlen)554)], (
		    ftnlen)1);
	    errint_("#", &kvmaxn[(i__1 = i__ - 1) < 8 && 0 <= i__1 ? i__1 : 
		    s_rnge("kvmaxn", i__1, "zzscad01_", (ftnlen)555)], (
		    ftnlen)1);

/*           Customize the short error message a bit. */

	    if (i__ == 1) {
		s_copy(shrtms, "SPICE(TOOMANYCOEFFS)", (ftnlen)25, (ftnlen)20)
			;
	    } else if (i__ == 2 || i__ == 3) {
		s_copy(shrtms, "SPICE(TOOMANYPARTITIONS)", (ftnlen)25, (
			ftnlen)24);
	    } else {
		s_copy(shrtms, "SPICE(KERNELVARTOOLARGE)", (ftnlen)25, (
			ftnlen)24);
	    }
	    sigerr_(shrtms, (ftnlen)25);
	    chkout_("ZZSCAD01", (ftnlen)8);
	    return 0;
	}
    }

/*     Store kernel variable sizes as scalars for convenience. */

    ncoeff = kvsize[0];
    npartb = kvsize[1];
    nparte = kvsize[2];
    nmod = kvsize[4];
    noff = kvsize[3];

/*     NDPVAL is the number of d.p. values that must be buffered to */
/*     support this clock. */

    ndpval = ncoeff + npartb + nparte + nmod + noff;

/*     Check whether sizes of partition start and end kernel variables */
/*     match. */

    if (npartb != nparte) {
	zzscin01_(hdsclk, scpool, clklst, dpfree, ifree, &prvsc);
	setmsg_("The numbers of partition start times # and stop times # are"
		" unequal for spacecraft clock #.", (ftnlen)91);
	errint_("#", &npartb, (ftnlen)1);
	errint_("#", &nparte, (ftnlen)1);
	errint_("#", sc, (ftnlen)1);
	sigerr_("SPICE(NUMPARTSUNEQUAL)", (ftnlen)22);
	chkout_("ZZSCAD01", (ftnlen)8);
	return 0;
    } else {
	npart = npartb;
    }
    if (ndpval > 320018) {

/*        We couldn't make enough room even if we dumped all buffered */
/*        data. */

/*        This code should be unreachable but is provided for safety. */
/*        We've already checked the existence, types, and sizes of the */
/*        d.p. kernel variables. */

	zzscin01_(hdsclk, scpool, clklst, dpfree, ifree, &prvsc);
	setmsg_("Total number of double precision data values for SCLK # is "
		"#; this count exceeds the maximum supported count #.", (
		ftnlen)111);
	errint_("#", sc, (ftnlen)1);
	errint_("#", &ndpval, (ftnlen)1);
	errint_("#", &c_b28, (ftnlen)1);
	sigerr_("SPICE(BUG)", (ftnlen)10);
	chkout_("ZZSCAD01", (ftnlen)8);
	return 0;
    } else if (ndpval > dproom) {

/*        We don't have room for this clock, but we will if we dump */
/*        the existing buffered data. */

	zzscin01_(hdsclk, scpool, clklst, dpfree, ifree, &prvsc);
    }

/*     Store in the integer buffer the sizes of the coefficient array */
/*     and the number of partitions. */

    intbuf[ibase + 3] = ncoeff;
    intbuf[ibase + 4] = npart;

/*     Fetch from the kernel pool the d.p. items we need: */

/*        -  The SCLK coefficients array */
/*        -  The partition start times */
/*        -  The partition end times */
/*        -  The moduli of the fields of an SCLK string */
/*        -  The offsets for each clock field. */

/*     Set the base index in the d.p. buffer of the next d.p. item */
/*     to store. The first element of the item has index DPBASE+1. */

/*     We don't update DPFREE until all d.p. data have been fetched */
/*     successfully. */

    dpbase = *dpfree - 1;
    for (i__ = 1; i__ <= 5; ++i__) {

/*        Store in the integer buffer the base index in the d.p. */
/*        buffer of the kernel variable's values. */

	intbuf[ibase + ibix[(i__1 = i__ - 1) < 8 && 0 <= i__1 ? i__1 : s_rnge(
		"ibix", i__1, "zzscad01_", (ftnlen)675)] - 1] = dpbase;

/*        Fetch d.p. values into buffer. */

	scld01_(namlst + ((i__1 = i__ - 1) < 8 && 0 <= i__1 ? i__1 : s_rnge(
		"namlst", i__1, "zzscad01_", (ftnlen)679)) * 60, sc, &kvmaxn[(
		i__2 = i__ - 1) < 8 && 0 <= i__2 ? i__2 : s_rnge("kvmaxn", 
		i__2, "zzscad01_", (ftnlen)679)], &n, &dpbuff[dpbase], (
		ftnlen)60);
	if (failed_()) {

/*           This code could be reached if the kernel variable has */
/*           character type. */

	    zzscin01_(hdsclk, scpool, clklst, dpfree, ifree, &prvsc);
	    chkout_("ZZSCAD01", (ftnlen)8);
	    return 0;
	}

/*        Update the d.p. buffer base index. */

	dpbase += n;
    }

/*     Account for buffer usage for the new clock. */

    *dpfree += ndpval;
    *ifree += 10;
    chkout_("ZZSCAD01", (ftnlen)8);
    return 0;
} /* zzscad01_ */

