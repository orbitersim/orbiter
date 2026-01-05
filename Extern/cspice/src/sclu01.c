/* sclu01.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__0 = 0;
static integer c__1 = 1;
static integer c__9 = 9;
static integer c__14 = 14;

/* $Procedure SCLU01 ( SCLK look up, type 1 ) */
/* Subroutine */ int sclu01_0_(int n__, char *name__, integer *sc, integer *
	maxnv, integer *n, integer *ival, doublereal *dval, ftnlen name_len)
{
    /* Initialized data */

    static char namlst[80*9] = "SCLK01_COEFFICIENTS                         "
	    "                                    " "SCLK_PARTITION_START     "
	    "                                                       " "SCLK_P"
	    "ARTITION_END                                                    "
	    "          " "SCLK01_N_FIELDS                                    "
	    "                             " "SCLK01_OFFSETS                  "
	    "                                                " "SCLK01_MODULI"
	    "                                                                "
	    "   " "SCLK01_OUTPUT_DELIM                                       "
	    "                      " "SCLK01_KERNEL_ID                       "
	    "                                         " "SCLK01_TIME_SYSTEM  "
	    "                                                            ";
    static integer lb[9] = { 3,1,1,1,1,1,1,1,0 };
    static integer ub[9] = { 300000,9999,9999,1,10,10,1,1,1 };
    static char nfdmsg[320] = "# not found. Did you load the SCLK kernel?   "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                   ";
    static char nummsg[320] = "# values found for #: valid range is #:#.    "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                   ";
    static char bvlmsg[320] = "Invalid value found for #:  #.               "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                   ";

    /* System generated locals */
    integer i__1, i__2;

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_cmp(char *, char *, ftnlen, ftnlen), s_rnge(char *, integer, 
	    char *, integer);

    /* Local variables */
    char type__[1];
    integer i__;
    extern /* Subroutine */ int chkin_(char *, ftnlen), errch_(char *, char *,
	     ftnlen, ftnlen), repmc_(char *, char *, char *, char *, ftnlen, 
	    ftnlen, ftnlen, ftnlen), repmd_(char *, char *, doublereal *, 
	    integer *, char *, ftnlen, ftnlen, ftnlen);
    logical found;
    extern /* Subroutine */ int repmi_(char *, char *, integer *, char *, 
	    ftnlen, ftnlen, ftnlen);
    extern logical failed_(void);
    integer nf, nfield;
    extern integer isrchc_(char *, integer *, char *, ftnlen, ftnlen);
    char kvname[32];
    extern /* Subroutine */ int gipool_(char *, integer *, integer *, integer 
	    *, integer *, logical *, ftnlen), sigerr_(char *, ftnlen);
    char tmpnam[80];
    extern /* Subroutine */ int chkout_(char *, ftnlen);
    char errmsg[320];
    extern /* Subroutine */ int dtpool_(char *, logical *, integer *, char *, 
	    ftnlen, ftnlen), setmsg_(char *, ftnlen), errint_(char *, integer 
	    *, ftnlen), suffix_(char *, integer *, char *, ftnlen, ftnlen), 
	    gdpool_(char *, integer *, integer *, integer *, doublereal *, 
	    logical *, ftnlen);
    extern logical return_(void);

/* $ Abstract */

/*     Look up type 1 SCLK kernel data. */

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

/*     KERNEL */
/*     SCLK */

/* $ Keywords */

/*     UTILITY */

/* $ Declarations */
/* $ Abstract */

/*     Include file sclk.inc */

/*     SPICE private file intended solely for the support of SPICE */
/*     routines.  Users should not include this file directly due */
/*     to the volatile nature of this file */

/*     The parameters below define sizes and limits used by */
/*     the SCLK system. */

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

/* $ Parameters */

/*     See the declaration section below. */

/* $ Author_and_Institution */

/*     N.J. Bachman    (JPL) */

/* $ Literature_References */

/*     None. */

/* $ Version */

/* -    SPICELIB Version 2.0.1, 20-OCT-2020 (NJB) */

/*        Increased MXCOEF to 100000. */

/*        Updated comments with reminder to keep constants declared */
/*        in the include file zzsc01.inc synced with constants in */
/*        this file. */

/* -    SPICELIB Version 2.0.0, 24-MAY-2010 (NJB) */

/*        Increased value of maximum coefficient record count */
/*        parameter MXCOEF from 10K to 50K. */

/* -    SPICELIB Version 1.0.0, 11-FEB-2008 (NJB) */

/* -& */

/*        NOTE: many of the declarations present here are duplicated */
/*        in the include file zzsc01.inc. Declarations in that file */
/*        must be kept in sync with those in this file. The */
/*        duplicated declarations are: */

/*           NDELIM */
/*           DELIMS */
/*           MXPART */
/*           MXCOEF */
/*           MXNFLD */
/*           DPLEN */


/*     Number of supported SCLK field delimiters: */


/*     Supported SCLK string field delimiters: */


/*     Maximum number of partitions: */


/*     Partition string length. */

/*     Since the maximum number of partitions is given by MXPART is */
/*     9999, PRTSTR needs at most 4 characters for the partition number */
/*     and one character for the slash. */


/*     Maximum number of coefficient records: */


/*     Maximum number of fields in an SCLK string: */


/*     Length of strings used to represent D.P. */
/*     numbers: */


/*     Maximum number of supported parallel time systems: */


/*     End of include file sclk.inc */

/* $ Brief_I/O */

/*     VARIABLE  I/O  ENTRY POINTS */
/*     --------  ---  -------------------------------------------------- */
/*     NAME       I   SCLD01, SCLI01 */
/*     SC         I   SCLD01, SCLI01 */
/*     MAXNV      I   SCLD01, SCLI01 */
/*     N          O   SCLD01, SCLI01 */
/*     IVAL       O   SCLI01 */
/*     DVAL       O   SCLD01 */
/*     MXCOEF     P   SCLD01, SCLI01 */
/*     MXPART     P   SCLD01, SCLI01 */
/*     MXNFLD     P   SCLD01, SCLI01 */
/*     NDELIM     P   SCLI01 */
/*     MXTSYS     P   SCLI01 */

/* $ Detailed_Input */

/*     See entry points SCLI01, SCLD01. */

/* $ Detailed_Output */

/*     See entry points SCLI01, SCLD01. */

/* $ Parameters */

/*     See the INCLUDE file sclk.inc for descriptions and values */
/*     of the global parameters used by this routine and */
/*     its entry points. */

/* $ Exceptions */

/*     1)  If SCLU01 is called directly, the error SPICE(BOGUSENTRY) is */
/*         signaled. */

/*     See entry points SCLI01, SCLD01 for descriptions of exceptions */
/*     specific to those routines. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine is a utility whose purpose is to localize error */
/*     checking for type 1 SCLK kernel pool lookups in a single place. */

/*     SLCU01 exists solely as an umbrella routine in which the */
/*     variables for its entry points are declared.  SCLU01 should never */
/*     be called directly. */

/* $ Examples */

/*     See entry points SCLI01, SCLD01. */

/* $ Restrictions */

/*     1)  SCLU01 handles lookups of type 1 SCLK data only. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     W.L. Taber         (JPL) */
/*     E.D. Wright        (JPL) */

/* $ Version */

/* -    SPICELIB Version 3.0.0, 01-DEC-2021 (NJB) (JDR) */

/*        New checks on item sizes have been added: sizes are now */
/*        compared against upper bounds as well as lower bounds. */
/*        Previously only lower bounds were used. */

/*        Bug fix: corrected index error in SCLD01 modulus range check. */
/*        Corrected comments about count checks in SCLI01. Made */
/*        cosmetic changes to code and comments in both SCLD01 and */
/*        SCLI01, and in this routine. Deleted unused parameter MXNCFF. */

/*        Edited the umbrella routine and all its entry points headers */
/*        to comply with NAIF standard. */

/* -    SPICELIB Version 2.3.0, 05-FEB-2008 (NJB) */

/*        Values of parameters */

/*           MXCOEF, MXPART, MXNFLD, NDELIM, MXTSYS */

/*        are now provided by the INCLUDE file sclk.inc. */

/* -    SPICELIB Version 2.2.0, 20-NOV-2006 (NJB) (EDW) */

/*        Entry points SCLI01 and SCLD01 were update to use kernel pool */
/*        fetch routines GIPOOL and GDPOOL respectively. Formerly these */
/*        entry points called the deprecated routine RTPOOL. */

/*        All headers have been updated to remove warnings about memory */
/*        corruption that could occur due to use of RTPOOL. */

/*        Header references to LDPOOL were replaced with references to */
/*        FURNSH. */

/* -    SPICELIB Version 2.1.0, 19-OCT-1992 (NJB) */

/*        Entry points SCLI01 and SCLD01 were updated to fix a bug: */
/*        if a kernel pool lookup fails, the number of elements returned */
/*        N is now set to zero. */

/* -    SPICELIB Version 2.0.0, 17-APR-1992 (NJB) (WLT) */

/*        Entry point SCLI01 was updated to handle a time */
/*        system specification for the `parallel' time system */
/*        in the SCLK kernel. Comment section for permuted index */
/*        source lines was added following the header. */

/* -    SPICELIB Version 1.0.0, 06-SEP-1990 (NJB) */

/* -& */
/* $ Index_Entries */

/*     lookup type_1 spacecraft_clock */

/* -& */
/* $ Revisions */

/* -    SPICELIB Version 2.1.0, 19-OCT-1992 (NJB) */

/*        Entry points SCLI01 and SCLD01 were updated to fix a bug: */
/*        if a kernel pool lookup fails, the number of elements returned */
/*        N is now set to zero. Formerly, these routines returned */
/*        whatever value was returned by RTPOOL.  RTPOOL, however, */
/*        does not set N to zero when the data item requested from it */
/*        is not found. */

/* -    SPICELIB Version 2.0.0, 17-APR-1992 (NJB) (WLT) */

/*        Entry point SCLI01 was updated to handle a time */
/*        system specification for the `parallel' time system */
/*        in the SCLK kernel. The update consists of these */
/*        changes: */

/*           -- The parameter MXTSYS is now defined. */

/*           -- The local saved variable NAMLST has been expanded */
/*              to include the name SCLK01_TIME_SYSTEM */

/*           -- The local saved variable LB has been expanded to */
/*              include the lower bound for the number of returned */
/*              values when SCLK01_TIME_SYSTEM_nn is looked up in */
/*              the kernel pool. */

/*           -- SCLI01 checks the value returned by RTPOOL when */
/*              SCLK01_TIME_SYSTEM_nn is looked up to verify that */
/*              it is within the range [1, MXTSYS]. */

/*        Also, a comment section for permuted index source lines was */
/*        added following the header. */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     MXCOFA is the maximum size of the coefficient array. */


/*     COFIDX is the index of the coefficient name in NAMLST. */


/*     NFLIDX is the index of the SCLK field count in NAMLST. */


/*     OFFIDX is the index of the SCLK offsets in NAMLST. */


/*     MODIDX is the index of the SCLK moduli in NAMLST. */


/*     DELIDX is the index of the delimiter code name in NAMLST.  If */
/*     the declaration of NAMLST or assignment of values to NAMLST */
/*     changes, this parameter value may have to change. */


/*     SYSIDX is the index of the time system in NAMLST. */


/*     Local variables */


/*     Saved variables */


/*     Initial values */


/*     Names of type 1 SCLK items and lower bounds on the number of */
/*     associated values. */

    /* Parameter adjustments */
    if (ival) {
	}
    if (dval) {
	}

    /* Function Body */
    switch(n__) {
	case 1: goto L_scli01;
	case 2: goto L_scld01;
	}


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }
    chkin_("SCLU01", (ftnlen)6);
    sigerr_("SPICE(BOGUSENTRY)", (ftnlen)17);
    chkout_("SCLU01", (ftnlen)6);
    return 0;
/* $Procedure SCLI01 ( SCLK lookup of integer data, type 1 ) */

L_scli01:
/* $ Abstract */

/*     Look up integer type 1 SCLK data from the kernel pool. */

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

/*     KERNEL */
/*     SCLK */

/* $ Keywords */

/*     UTILITY */

/* $ Declarations */

/*     CHARACTER*(*)         NAME */
/*     INTEGER               SC */
/*     INTEGER               MAXNV */
/*     INTEGER               N */
/*     INTEGER               IVAL   ( * ) */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     NAME, */
/*     SC         I   Name of kernel data item, NAIF spacecraft ID code. */
/*     MAXNV      I   Maximum number of integer values to return. */
/*     N          O   Number of values actually returned. */
/*     IVAL       O   Returned integer values. */
/*     MXNFLD     P   Maximum number of fields in an SCLK string. */
/*     NDELIM     P   Maximum number of delimiter codes. */
/*     MXTSYS     P   Maximum number of supported parallel time systems. */

/* $ Detailed_Input */

/*     NAME, */
/*     SC       are, respectively, a name and a NAIF integer code */
/*              of a spacecraft that together define the name of a */
/*              requested kernel data item. NAME is the full name */
/*              as it appears in the SCLK kernel, except that it */
/*              lacks the final underscore and spacecraft integer */
/*              code (actually, the negative of the spacecraft */
/*              code).  This routine combines NAME and SC to */
/*              make up the appropriate kernel variable name. */

/*              For example, to look up data associated with the */
/*              name */

/*                 SCLK01_N_FIELDS_77 */

/*              you would supply NAME as */

/*                 SCLK01_N_FIELDS */

/*              and SC as -77. */


/*     MAXNV    is the maximum number of values to return.  MAXNV */
/*              is used to prevent SCLI01 from writing past the end */
/*              of the supplied array IVAL. */

/* $ Detailed_Output */

/*     N        is the number of values actually returned. */

/*     IVAL     is an array containing the requested integer */
/*              kernel data item. */

/* $ Parameters */

/*     MXNFLD   is an upper bound on the number of fields in a */
/*              SCLK string. */

/*     NDELIM   is the number of delimiter codes. */

/*     MXTSYS   is the maximum number of supported parallel time */
/*              systems that SCLK values may be mapped to or from. */

/* $ Exceptions */

/*     1)  If item specified by NAME and SC is not found in the kernel */
/*         pool, and if the presence of the item is required, the error */
/*         SPICE(KERNELVARNOTFOUND) is signaled. The output arguments */
/*         are not modified. */

/*         If the specified item is not required, the output argument N */
/*         will take the value 0, and the output argument IVAL is not */
/*         modified. */

/*     2)  If the item specified by NAME and SC is found but does not */
/*         have numeric type, the error SPICE(BADKERNELVARTYPE) is */
/*         signaled. */

/*     3)  This routine can check certain data for validity. If any of */
/*         these items have invalid values, the error */
/*         SPICE(VALUEOUTOFRANGE) is signaled. The output arguments are */
/*         not modified. The values in question are: */

/*            -  The number of fields of a SCLK string */
/*            -  The number of delimiter codes */
/*            -  The output delimiter code */
/*            -  The time system code */

/*     4)  If the dimension of the requested item exceeds MAXNV, the */
/*         error SPICE(ARRAYTOOSMALL) is signaled. */

/*     5)  If the dimension of the requested item is outside of the */
/*         limits for that item, the error SPICE(INVALIDSIZE) is */
/*         signaled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     The purpose of this routine is to localize error checking for */
/*     lookups of type 1 SCLK kernel pool data. This routine handles */
/*     lookups of integer data. */

/* $ Examples */

/*     1)  To get the number of SCLK fields for the Galileo spacecraft */
/*         clock, you can use the code fragment below: */

/*            C */
/*            C     Load the SCLK kernel in question. We use a */
/*            C     made-up name for the kernel file; you would use */
/*            C     the actual name of your kernel file instead if you */
/*            C     were to carry out this procedure. */
/*            C */
/*                  CALL FURNSH ( 'SAMPLE_GLL_SCLK.KER' ) */

/*                  SC   = -77 */
/*                  NAME = 'SCLK01_N_FIELDS' */

/*                  CALL SCLI01 ( NAME, SC, MXNFLD, N, NFIELD ) */


/*         After this subroutine call, NFIELD has the value 4. */

/* $ Restrictions */

/*     1)  SCLI01 assumes that a SCLK kernel appropriate to the */
/*         spacecraft identified by SC has been loaded. */

/*     2)  SCLI01 handles lookups of type 1 SCLK data only. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     W.L. Taber         (JPL) */
/*     E.D. Wright        (JPL) */

/* $ Version */

/* -    SPICELIB Version 3.0.0, 01-DEC-2021 (NJB) (JDR) */

/*        New checks on item sizes have been added: sizes are now */
/*        compared against upper bounds as well as lower bounds. */
/*        Previously only lower bounds were used. */

/*        Corrected comments about count checks. Made cosmetic changes */
/*        to code and comments. */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 2.2.0, 20-NOV-2006 (NJB) (EDW) */

/*        Routine was updated to use GIPOOL instead of RTPOOL. Header */
/*        has been updated to remove warnings about memory corruption and */
/*        to document exception handling for output buffer overflow */
/*        errors. */

/*        Header references to LDPOOL were replaced with references to */
/*        FURNSH. */

/* -    SPICELIB Version 2.1.0, 19-OCT-1992 (NJB) */

/*        This entry point was updated to fix a bug: if a kernel pool */
/*        lookup fails, the number of elements returned N is now set to */
/*        zero. */

/* -    SPICELIB Version 2.0.0, 17-APR-1992 (NJB) (WLT) */

/*        SCLI01 was updated to handle a time system specification for */
/*        the `parallel' time system in the SCLK kernel. Some */
/*        corrections and other minor enhancements were made to the */
/*        header. Comment section for permuted index source lines was */
/*        added following the header. */

/* -    SPICELIB Version 1.0.0, 06-SEP-1990 (NJB) */

/* -& */
/* $ Index_Entries */

/*     lookup of type_1 spacecraft_clock integer data */
/*     lookup type_1 spacecraft_clock integer data */

/* -& */
/* $ Revisions */

/* -    SPICELIB Version 2.1.0, 19-OCT-1992 (NJB) */

/*        This entry point was updated to fix a bug: if a kernel pool */
/*        lookup fails, the number of elements returned N is now set to */
/*        zero. Formerly, this routine returned whatever value was */
/*        returned by RTPOOL.  RTPOOL, however, does not set N to zero */
/*        when the data item requested from it is not found. */

/* -    SPICELIB Version 2.0.0, 17-APR-1992 (NJB) (WLT) */

/*        Entry point SCLI01 was updated to handle a time */
/*        system specification for the `parallel' time system */
/*        in the SCLK kernel. The update consists of these */
/*        changes: */

/*           -- The parameter MXTSYS is now defined. */

/*           -- The local saved variable NAMLST has been expanded */
/*              to include the name SCLK01_TIME_SYSTEM */

/*           -- The local saved variable LB has been expanded to */
/*              include the lower bound for the number of returned */
/*              values when SCLK01_TIME_SYSTEM_nn is looked up in */
/*              the kernel pool. */

/*           -- SCLI01 checks the value returned by RTPOOL when */
/*              SCLK01_TIME_SYSTEM_nn is looked up to verify that */
/*              it is within the range [1, MXTSYS]. */

/*        Also, a comment section for permuted index source lines was */
/*        added following the header. */

/*        The $Exceptions header section was updated accordingly. */

/*        Some corrections and other minor enhancements were made to the */
/*        header. */

/* -& */

/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }
    chkin_("SCLI01", (ftnlen)6);

/*     Form the name of the kernel pool data item, and do the lookup. */
/*     Note that eventually we should use a kernel pool lookup entry */
/*     that allows us to specify the maximum number of entries that */
/*     can be returned. */

    s_copy(tmpnam, name__, (ftnlen)80, name_len);
    suffix_("_#", &c__0, tmpnam, (ftnlen)2, (ftnlen)80);
    i__1 = -(*sc);
    repmi_(tmpnam, "#", &i__1, tmpnam, (ftnlen)80, (ftnlen)1, (ftnlen)80);

/*     Make sure we have enough room for the item in our output */
/*     array. Look up the dimension of the item. */

    dtpool_(tmpnam, &found, n, type__, (ftnlen)80, (ftnlen)1);
    if (*n > *maxnv) {
	setmsg_("Item # for SCLK # has size # but output array has size #.", (
		ftnlen)57);
	errch_("#", tmpnam, (ftnlen)1, (ftnlen)80);
	errint_("#", sc, (ftnlen)1);
	errint_("#", n, (ftnlen)1);
	errint_("#", maxnv, (ftnlen)1);
	sigerr_("SPICE(ARRAYTOOSMALL)", (ftnlen)20);
	chkout_("SCLI01", (ftnlen)6);
	return 0;
    }

/*     Check the data type of the variable. */

    if (found && *(unsigned char *)type__ != 'N') {
	setmsg_("Kernel variable # for spacecraft clock # does not have nume"
		"ric type.", (ftnlen)68);
	errch_("#", tmpnam, (ftnlen)1, (ftnlen)80);
	errint_("#", sc, (ftnlen)1);
	sigerr_("SPICE(BADKERNELVARTYPE)", (ftnlen)23);
	chkout_("SCLI01", (ftnlen)6);
	return 0;
    }
    gipool_(tmpnam, &c__1, maxnv, n, ival, &found, (ftnlen)80);
    if (failed_()) {
	chkout_("SCLI01", (ftnlen)6);
	return 0;
    }

/*     Make sure we found what we were looking for, if the item */
/*     is required. */

    if (! found) {

/*        Currently, the only item that is NOT required is the time */
/*        system specification.  In any case, no values will be returned. */

	*n = 0;
	if (s_cmp(name__, namlst + 640, name_len, (ftnlen)80) == 0) {
	    chkout_("SCLI01", (ftnlen)6);
	    return 0;
	} else {
	    setmsg_(nfdmsg, (ftnlen)320);
	    errch_("#", tmpnam, (ftnlen)1, (ftnlen)80);
	    sigerr_("SPICE(KERNELVARNOTFOUND)", (ftnlen)24);
	    chkout_("SCLI01", (ftnlen)6);
	    return 0;
	}
    }

/*     Now we must check that the number of returned values is in the */
/*     appropriate range. We test for the following conditions: */

/*        - The SCLK field count kernel variable is a scalar. */

/*        - The delimiter code kernel variable is a scalar. */

/*        - The time system code kernel variable, if present, is a */
/*          scalar. At this point in the code, the variable is known */
/*          to be present. */

/*     Note that the kernel pool doesn't allow a kernel variable to */
/*     not have associated values. Checks against the lower bound 1 are */
/*     done just to simplify the code. */

/*     See if the input name is in the list of items we know about. */
/*     If it is, perform the bound checks that apply. */

    i__ = isrchc_(name__, &c__9, namlst, name_len, (ftnlen)80);
    if (i__ != 0) {
	if (*n < lb[(i__1 = i__ - 1) < 9 && 0 <= i__1 ? i__1 : s_rnge("lb", 
		i__1, "sclu01_", (ftnlen)760)] || *n > ub[(i__2 = i__ - 1) < 
		9 && 0 <= i__2 ? i__2 : s_rnge("ub", i__2, "sclu01_", (ftnlen)
		760)]) {
	    repmi_(nummsg, "#", n, errmsg, (ftnlen)320, (ftnlen)1, (ftnlen)
		    320);
	    repmc_(errmsg, "#", tmpnam, errmsg, (ftnlen)320, (ftnlen)1, (
		    ftnlen)80, (ftnlen)320);
	    repmi_(errmsg, "#", &lb[(i__1 = i__ - 1) < 9 && 0 <= i__1 ? i__1 :
		     s_rnge("lb", i__1, "sclu01_", (ftnlen)764)], errmsg, (
		    ftnlen)320, (ftnlen)1, (ftnlen)320);
	    repmi_(errmsg, "#", &ub[(i__1 = i__ - 1) < 9 && 0 <= i__1 ? i__1 :
		     s_rnge("ub", i__1, "sclu01_", (ftnlen)765)], errmsg, (
		    ftnlen)320, (ftnlen)1, (ftnlen)320);
	    setmsg_(errmsg, (ftnlen)320);
	    sigerr_("SPICE(SIZEOUTOFRANGE)", (ftnlen)21);
	    chkout_("SCLI01", (ftnlen)6);
	    return 0;
	}
    }

/*     Check values of kernel variables: */

/*        - The output delimiter code is at least 1 and is not */
/*          greater than the number of delimiters. */

/*        - The field count is at least 1 and is not greater than */
/*          MXNFLD. */

/*        - The time system code is at least 1 and is not greater */
/*          than MXTSYS. */

    if (s_cmp(name__, namlst + 480, name_len, (ftnlen)80) == 0) {
	if (ival[0] < 1 || ival[0] > 5) {
	    repmc_(bvlmsg, "#", tmpnam, errmsg, (ftnlen)320, (ftnlen)1, (
		    ftnlen)80, (ftnlen)320);
	    repmi_(errmsg, "#", ival, errmsg, (ftnlen)320, (ftnlen)1, (ftnlen)
		    320);
	    setmsg_(errmsg, (ftnlen)320);
	    sigerr_("SPICE(VALUEOUTOFRANGE)", (ftnlen)22);
	    chkout_("SCLI01", (ftnlen)6);
	    return 0;
	}
    }
    if (s_cmp(name__, namlst + 240, name_len, (ftnlen)80) == 0) {
	if (ival[0] < 1 || ival[0] > 10) {
	    repmc_(bvlmsg, "#", tmpnam, errmsg, (ftnlen)320, (ftnlen)1, (
		    ftnlen)80, (ftnlen)320);
	    repmi_(errmsg, "#", ival, errmsg, (ftnlen)320, (ftnlen)1, (ftnlen)
		    320);
	    setmsg_(errmsg, (ftnlen)320);
	    sigerr_("SPICE(VALUEOUTOFRANGE)", (ftnlen)22);
	    chkout_("SCLI01", (ftnlen)6);
	    return 0;
	}
    }
    if (s_cmp(name__, namlst + 640, name_len, (ftnlen)80) == 0) {
	if (ival[0] < 1 || ival[0] > 2) {
	    repmc_(bvlmsg, "#", tmpnam, errmsg, (ftnlen)320, (ftnlen)1, (
		    ftnlen)80, (ftnlen)320);
	    repmi_(errmsg, "#", ival, errmsg, (ftnlen)320, (ftnlen)1, (ftnlen)
		    320);
	    setmsg_(errmsg, (ftnlen)320);
	    sigerr_("SPICE(VALUEOUTOFRANGE)", (ftnlen)22);
	    chkout_("SCLI01", (ftnlen)6);
	    return 0;
	}
    }
    chkout_("SCLI01", (ftnlen)6);
    return 0;
/* $Procedure SCLD01 ( SCLK lookup of double precision data, type 1 ) */

L_scld01:
/* $ Abstract */

/*     Look up double precision type 1 SCLK data from the kernel pool. */

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

/*     KERNEL */
/*     SCLK */

/* $ Keywords */

/*     UTILITY */

/* $ Declarations */

/*     CHARACTER*(*)         NAME */
/*     INTEGER               SC */
/*     INTEGER               MAXNV */
/*     INTEGER               N */
/*     DOUBLE PRECISION      DVAL   ( * ) */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     NAME, */
/*     SC         I   Name of kernel data item, NAIF spacecraft ID code. */
/*     MAXNV      I   Maximum number of d.p. values to return. */
/*     N          O   Number of values actually returned. */
/*     DVAL       O   Requested kernel data item. */
/*     MXCOEF     P   Maximum number of coefficient sets in SCLK kernel. */

/* $ Detailed_Input */

/*     NAME, */
/*     SC       are, respectively, a name and a NAIF integer code */
/*              of a spacecraft that together define the name of a */
/*              requested kernel data item. NAME is the full name */
/*              as it appears in the SCLK kernel, except that it */
/*              lacks the final underscore and spacecraft integer */
/*              code (actually, the negative of the spacecraft */
/*              code).  This routine combines NAME and SC to */
/*              make up the appropriate kernel variable name. */

/*              For example, to look up data associated with the */
/*              name */

/*                 SCLK01_COEFFICIENTS_77 */

/*              you would supply NAME as */

/*                 SCLK01_COEFFICIENTS */

/*              and SC as -77. */


/*     MAXNV    is the maximum number of values to return.  MAXNV */
/*              is used to prevent SCLD01 from writing past the end */
/*              of the supplied array DVAL. */

/* $ Detailed_Output */

/*     N        is the number of values actually returned. */

/*     DVAL     is an array containing the requested double */
/*              precision kernel data item. */

/* $ Parameters */

/*     MXCOEF   is the maximum number of coefficient sets in the */
/*              array COEFFS that defines the mapping between */
/*              encoded type 1 SCLK and a parallel time system. */
/*              This array has dimension 3 x MXCOEF. The value of */
/*              MXCOEF may be increased as required. */

/* $ Exceptions */

/*     1)  If item specified by NAME and SC is not found in the kernel */
/*         pool, the error SPICE(KERNELVARNOTFOUND) is signaled. The */
/*         output arguments are not modified. */

/*     2)  If the item specified by NAME and SC is found but does not */
/*         have numeric type, the error SPICE(BADKERNELVARTYPE) is */
/*         signaled. */

/*     3)  This routine can check certain data for validity. If any of */
/*         these items have invalid values, the error */
/*         SPICE(VALUEOUTOFRANGE) is signaled. The output arguments are */
/*         not modified. The values in question are: */

/*            - The number of coefficients. */
/*            - The number of partition start values. */
/*            - The number of partition end values. */
/*            - The number of moduli. */
/*            - The values of the moduli (lower bounds) */
/*            - The number of offsets. */
/*            - The number of kernel identifiers. */

/*     4)  If the partition times or SCLK coefficients themselves */
/*         are invalid, this routine does nothing about it. It is */
/*         simply not possible to detect all of the possible errors */
/*         that these data may be subject to. */

/*     5)  If the dimension of the requested item exceeds MAXNV, the */
/*         error SPICE(ARRAYTOOSMALL) is signaled. */

/*     6)  If the dimension of the requested item is outside of the */
/*         limits for that item, the error SPICE(INVALIDSIZE) is */
/*         signaled. */

/*     7)  If the dimension of the coefficient kernel variable is */
/*         not a multiple of 3, the error SPICE(INVALIDSIZE) is */
/*         signaled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     The purpose of this routine is to localize error checking for */
/*     lookups of type 1 SCLK kernel pool data. This routine handles */
/*     lookups of double precision data. */

/* $ Examples */

/*     1)  Check a NAIF SCLK kernel for accuracy by converting the */
/*         encoded SCLK coefficients to strings with partition numbers */
/*         and converting the parallel times to UTC strings. Print out */
/*         the results in tabular form. In this example, the spacecraft */
/*         is Mars Observer, which has NAIF ID code -94. We could */
/*         make the program work for Galileo by using the NAIF ID code */
/*         -77 instead of -94. */

/*            C */
/*            C     Load the SCLK kernel in question, and also load */
/*            C     a leapseconds kernel. We use made-up names for the */
/*            C     kernel file; you would use the actual names of your */
/*            C     kernel files instead if you were to carry out this */
/*            C     procedure. */
/*            C */
/*                  CALL FURNSH ( 'SAMPLE_MO_SCLK.KER' ) */
/*                  CALL FURNSH ( 'LEAPSECONDS.KER'    ) */

/*                  CONAME =  SCLK01_COEFFICIENTS */
/*                  SC     =  -94 */

/*            C */
/*            C     Grab the coefficients. */
/*            C */
/*                  CALL SCLD01 ( CONAME, SC, 3*MXCOEF, NCOEFF, COEFFS ) */

/*            C */
/*            C     The SCLK coefficients are in the first row of the */
/*            C     coefficients array; the parallel times are in the */
/*            C     second. Since the parallel time system used for MO */
/*            C     is terrestrial dynamical time (TDT), we will convert */
/*            C     the parallel time values to ET (TDB) first and then */
/*            C     convert the resulting times to UTC. */
/*            C */
/*            C     In a more robust algorithm, we'd look up the parallel */
/*            C     time system code used in the SCLK kernel rather than */
/*            C     assume that it is a particular system. We omit this */
/*            C     check for simplicity. */
/*            C */
/*            C     We decode the SCLK coefficients using SCDECD. Write */
/*            C     out the results to a file we'll call COMPARE.DAT. */
/*            C */
/*                  OUTFIL = 'COMPARE.DAT' */

/*                  CALL WRLINE ( OUTFIL, '    SCLK               UTC' ) */
/*                  CALL WRLINE ( OUTFIL, ' '                          ) */

/*                  DO I = 1, NCOEFF / 3 */

/*                     CALL SCDECD ( -94,  COEFF(1,I),  CLKSTR ) */
/*            C */
/*            C        Convert the parallel time coefficients, which are */
/*            C        given in TDT, to ET. UNITIM returns this value. */
/*            C */
/*                     CALL ET2UTC ( UNITIM ( COEFF(2,I), 'TDT', 'TDB' ), */
/*                 .                 'D', */
/*                 .                  3, */
/*                 .                  UTC    ) */

/*                     LINE = ' SCLK        UTC ' */

/*                     CALL REPMC  ( LINE, 'SCLK', CLKSTR, LINE ) */
/*                     CALL REPMC  ( LINE, 'UTC',  UTC,    LINE ) */

/*                     CALL WRLINE ( OUTFIL, LINE ) */

/*                  END DO */

/* $ Restrictions */

/*     1)  SCLD01 assumes that a SCLK kernel appropriate to the */
/*         spacecraft identified by SC has been loaded. */

/*     2)  SCLD01 handles lookups of type 1 SCLK data only. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     W.L. Taber         (JPL) */
/*     E.D. Wright        (JPL) */

/* $ Version */

/* -    SPICELIB Version 3.0.0, 01-DEC-2021 (NJB) (JDR) */

/*        New checks on item sizes have been added: sizes are now */
/*        compared against upper bounds as well as lower bounds. */
/*        Previously only lower bounds were used. A check has been */
/*        added to verify that the coefficient count is a multiple */
/*        of 3. */

/*        Bug fix: corrected index error in modulus range check. Made */
/*        cosmetic changes to code and comments. */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 2.2.0, 20-NOV-2006 (NJB) (EDW) */

/*        Routine was updated to use GDPOOL instead of RTPOOL. Header */
/*        has been updated to remove warnings about memory corruption and */
/*        to document exception handling for output buffer overflow */
/*        errors. */

/*        Header references to LDPOOL were replaced with references to */
/*        FURNSH. */

/* -    SPICELIB Version 2.1.0, 19-OCT-1992 (NJB) */

/*        This entry point was updated to fix a bug: if a kernel pool */
/*        lookup fails, the number of elements returned N is now set to */
/*        zero. */

/* -    SPICELIB Version 2.0.0, 17-APR-1992 (NJB) (WLT) */

/*        One constant was changed in the code for clarity; no functional */
/*        change results from this. Some corrections and other minor */
/*        enhancements were made to the header. Comment section for */
/*        permuted index source lines was added following the header. */

/* -    SPICELIB Version 1.0.0, 06-SEP-1990 (NJB) */

/* -& */
/* $ Index_Entries */

/*     lookup of type_1 spacecraft_clock d.p. data */
/*     lookup type_1 spacecraft_clock d.p. data */

/* -& */
/* $ Revisions */

/* -    SPICELIB Version 2.1.0, 19-OCT-1992 (NJB) */

/*        This entry point was updated to fix a bug: if a kernel pool */
/*        lookup fails, the number of elements returned N is now set to */
/*        zero. Formerly, this routine returned whatever value was */
/*        returned by RTPOOL.  RTPOOL, however, does not set N to zero */
/*        when the data item requested from it is not found. */

/* -    SPICELIB Version 2.0.0, 17-APR-1992 (NJB) (WLT) */

/*        The constant 1 was changed to 1.D0 in the test for the */
/*        validity of the moduli for a spacecraft clock. The change */
/*        was made simply for clarity. */

/*        Some corrections and other minor enhancements were made to the */
/*        header. Comment section for permuted index source lines was */
/*        added following the header. */

/* -& */

/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }
    chkin_("SCLD01", (ftnlen)6);

/*     Form the name of the kernel pool datum, and do the lookup. */

    s_copy(tmpnam, name__, (ftnlen)80, name_len);
    suffix_("_#", &c__0, tmpnam, (ftnlen)2, (ftnlen)80);
    i__1 = -(*sc);
    repmi_(tmpnam, "#", &i__1, tmpnam, (ftnlen)80, (ftnlen)1, (ftnlen)80);

/*     Make sure we have enough room for the item in our output */
/*     array. Look up the dimension of the item. */

    dtpool_(tmpnam, &found, n, type__, (ftnlen)80, (ftnlen)1);
    if (*n > *maxnv) {
	setmsg_("Item # has size # but output array has size #.", (ftnlen)46);
	errch_("#", tmpnam, (ftnlen)1, (ftnlen)80);
	errint_("#", n, (ftnlen)1);
	errint_("#", maxnv, (ftnlen)1);
	sigerr_("SPICE(ARRAYTOOSMALL)", (ftnlen)20);
	chkout_("SCLD01", (ftnlen)6);
	return 0;
    }

/*     Check the data type of the variable. */

    if (found && *(unsigned char *)type__ != 'N') {
	setmsg_("Kernel variable # for spacecraft clock # does not have nume"
		"ric type.", (ftnlen)68);
	errch_("#", tmpnam, (ftnlen)1, (ftnlen)80);
	errint_("#", sc, (ftnlen)1);
	sigerr_("SPICE(BADKERNELVARTYPE)", (ftnlen)23);
	chkout_("SCLD01", (ftnlen)6);
	return 0;
    }
    gdpool_(tmpnam, &c__1, maxnv, n, dval, &found, (ftnlen)80);
    if (failed_()) {

/*        This code should be unreachable but is provided for safety. */

	chkout_("SCLD01", (ftnlen)6);
	return 0;
    }

/*     Make sure we found what we were looking for. */

    if (! found) {

/*        No values are returned in this case. */

	*n = 0;
	setmsg_(nfdmsg, (ftnlen)320);
	errch_("#", tmpnam, (ftnlen)1, (ftnlen)80);
	sigerr_("SPICE(KERNELVARNOTFOUND)", (ftnlen)24);
	chkout_("SCLD01", (ftnlen)6);
	return 0;
    }

/*     Now we must check that the number of returned values is in the */
/*     appropriate range. We test for the following conditions: */

/*        - The number of coefficients is at least 3 and no more than */
/*          3 * MXCOEF */

/*        - The number of partition start values is at least 1 and no */
/*          more than MXPART */

/*        - The number of partition end values is at least 1 and no more */
/*          than MXPART */

/*        - The number of moduli is at least 1, no more than MXNFLD, */
/*          and matches the field count. */

/*        - The number of offsets is at least 1, no more than MXNFLD, */
/*          and matches the field count. */

/*     Note that the kernel pool doesn't allow a kernel variable to */
/*     not have associated values. Checks against the lower bound 1 are */
/*     done just to simplify the code. */

/*     See if the input name is in the list of items we know about. */
/*     If it is, perform the bounds checks that apply. */

    i__ = isrchc_(name__, &c__9, namlst, name_len, (ftnlen)80);
    if (i__ != 0) {
	if (*n < lb[(i__1 = i__ - 1) < 9 && 0 <= i__1 ? i__1 : s_rnge("lb", 
		i__1, "sclu01_", (ftnlen)1260)] || *n > ub[(i__2 = i__ - 1) < 
		9 && 0 <= i__2 ? i__2 : s_rnge("ub", i__2, "sclu01_", (ftnlen)
		1260)]) {
	    repmi_(nummsg, "#", n, errmsg, (ftnlen)320, (ftnlen)1, (ftnlen)
		    320);
	    repmc_(errmsg, "#", tmpnam, errmsg, (ftnlen)320, (ftnlen)1, (
		    ftnlen)80, (ftnlen)320);
	    repmi_(errmsg, "#", &lb[(i__1 = i__ - 1) < 9 && 0 <= i__1 ? i__1 :
		     s_rnge("lb", i__1, "sclu01_", (ftnlen)1264)], errmsg, (
		    ftnlen)320, (ftnlen)1, (ftnlen)320);
	    repmi_(errmsg, "#", &ub[(i__1 = i__ - 1) < 9 && 0 <= i__1 ? i__1 :
		     s_rnge("ub", i__1, "sclu01_", (ftnlen)1265)], errmsg, (
		    ftnlen)320, (ftnlen)1, (ftnlen)320);
	    setmsg_(errmsg, (ftnlen)320);
	    sigerr_("SPICE(SIZEOUTOFRANGE)", (ftnlen)21);
	    chkout_("SCLD01", (ftnlen)6);
	    return 0;
	}
    }
    if (s_cmp(name__, namlst, name_len, (ftnlen)80) == 0) {
	if (*n / 3 * 3 != *n) {
	    setmsg_("Coefficient count for # must be multiple of 3 but was #."
		    , (ftnlen)56);
	    errch_("#", tmpnam, (ftnlen)1, (ftnlen)80);
	    errint_("#", n, (ftnlen)1);
	    sigerr_("SPICE(INVALIDSIZE)", (ftnlen)18);
	    chkout_("SCLD01", (ftnlen)6);
	    return 0;
	}
    }

/*     Check the values of the moduli themselves. Also check the */
/*     moduli count against the field count. */

    if (s_cmp(name__, namlst + 400, name_len, (ftnlen)80) == 0) {
	i__1 = *n;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    if (dval[i__ - 1] < 1.) {
		repmc_(bvlmsg, "#", tmpnam, errmsg, (ftnlen)320, (ftnlen)1, (
			ftnlen)80, (ftnlen)320);
		repmd_(errmsg, "#", &dval[i__ - 1], &c__14, errmsg, (ftnlen)
			320, (ftnlen)1, (ftnlen)320);
		setmsg_(errmsg, (ftnlen)320);
		sigerr_("SPICE(VALUEOUTOFRANGE)", (ftnlen)22);
		chkout_("SCLD01", (ftnlen)6);
		return 0;
	    }
	}
    }
    if (s_cmp(name__, namlst + 400, name_len, (ftnlen)80) == 0 || s_cmp(
	    name__, namlst + 320, name_len, (ftnlen)80) == 0) {

/*        Get the field count for this clock. */

	s_copy(kvname, namlst + 240, (ftnlen)32, (ftnlen)32);
	suffix_("_#", &c__0, kvname, (ftnlen)2, (ftnlen)32);
	i__1 = -(*sc);
	repmi_(kvname, "#", &i__1, kvname, (ftnlen)32, (ftnlen)1, (ftnlen)32);
	gipool_(kvname, &c__1, &c__1, &nf, &nfield, &found, (ftnlen)32);
	if (failed_()) {
	    chkout_("SCLD01", (ftnlen)6);
	    return 0;
	}
	if (! found) {
	    setmsg_("Field count was not found for SCLK #.", (ftnlen)37);
	    errint_("#", sc, (ftnlen)1);
	    sigerr_("SPICE(KERNELVARNOTFOUND)", (ftnlen)24);
	    chkout_("SCLD01", (ftnlen)6);
	    return 0;
	}
	if (*n != nfield) {
	    if (s_cmp(name__, namlst + 400, name_len, (ftnlen)80) == 0) {
		setmsg_("Modulus count # does not match field count # for SC"
			"LK #.", (ftnlen)56);
	    } else {
		setmsg_("Offset count # does not match field count # for SCL"
			"K #.", (ftnlen)55);
	    }
	    errint_("#", n, (ftnlen)1);
	    errint_("#", &nfield, (ftnlen)1);
	    errint_("#", sc, (ftnlen)1);
	    sigerr_("SPICE(INVALIDSIZE)", (ftnlen)18);
	    chkout_("SCLD01", (ftnlen)6);
	    return 0;
	}
    }
    chkout_("SCLD01", (ftnlen)6);
    return 0;
} /* sclu01_ */

/* Subroutine */ int sclu01_(char *name__, integer *sc, integer *maxnv, 
	integer *n, integer *ival, doublereal *dval, ftnlen name_len)
{
    return sclu01_0_(0, name__, sc, maxnv, n, ival, dval, name_len);
    }

/* Subroutine */ int scli01_(char *name__, integer *sc, integer *maxnv, 
	integer *n, integer *ival, ftnlen name_len)
{
    return sclu01_0_(1, name__, sc, maxnv, n, ival, (doublereal *)0, name_len)
	    ;
    }

/* Subroutine */ int scld01_(char *name__, integer *sc, integer *maxnv, 
	integer *n, doublereal *dval, ftnlen name_len)
{
    return sclu01_0_(2, name__, sc, maxnv, n, (integer *)0, dval, name_len);
    }

