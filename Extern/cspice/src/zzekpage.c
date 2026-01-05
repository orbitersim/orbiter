/* zzekpage.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;
static doublereal c_b15 = 0.;
static integer c__128 = 128;
static integer c__0 = 0;
static integer c__256 = 256;
static integer c__8 = 8;
static integer c__2 = 2;
static integer c__1024 = 1024;
static integer c__7 = 7;
static integer c__12 = 12;
static integer c__3 = 3;
static integer c__13 = 13;
static integer c__4 = 4;
static integer c__9 = 9;
static integer c__14 = 14;
static integer c__6 = 6;
static integer c__5 = 5;
static integer c__11 = 11;
static integer c__10 = 10;
static integer c__16 = 16;
static integer c__15 = 15;

/* $Procedure  ZZEKPAGE ( Private: Manage EK DAS paging system ) */
/* Subroutine */ int zzekpage_0_(int n__, integer *handle, integer *type__, 
	integer *addrss, char *stat, integer *p, char *pagec, doublereal *
	paged, integer *pagei, integer *base, integer *value, ftnlen stat_len,
	 ftnlen pagec_len)
{
    /* System generated locals */
    integer i__1;
    doublereal d__1;

    /* Builtin functions */
    integer i_dnnt(doublereal *), i_len(char *, ftnlen);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    static integer addr__;
    extern /* Subroutine */ int zzekpgch_(integer *, char *, ftnlen);
    static integer e, l, freec, freed;
    static char cfill[1024];
    static doublereal dfill[128];
    static integer freei;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    static integer ifill[256];
    extern /* Subroutine */ int fillc_(char *, integer *, char *, ftnlen, 
	    ftnlen), filld_(doublereal *, integer *, doublereal *), filli_(
	    integer *, integer *, integer *), errch_(char *, char *, ftnlen, 
	    ftnlen);
    static integer lastc, lastd, lasti;
    static doublereal dpptr;
    extern logical eqstr_(char *, char *, ftnlen, ftnlen);
    extern /* Subroutine */ int dasadc_(integer *, integer *, integer *, 
	    integer *, char *, ftnlen), dasadd_(integer *, integer *, 
	    doublereal *);
    extern logical failed_(void);
    extern /* Subroutine */ int dasadi_(integer *, integer *, integer *);
    static char encpag[5];
    static integer nfreec, nfreed;
    extern /* Subroutine */ int daslla_(integer *, integer *, integer *, 
	    integer *);
    static integer nfreei;
    extern /* Subroutine */ int dasudi_(integer *, integer *, integer *, 
	    integer *), dasrdi_(integer *, integer *, integer *, integer *), 
	    dassih_(integer *, char *, ftnlen), dasrdc_(integer *, integer *, 
	    integer *, integer *, integer *, char *, ftnlen), errhan_(char *, 
	    integer *, ftnlen), prtdec_(char *, integer *, ftnlen), dasrdd_(
	    integer *, integer *, integer *, doublereal *), dasudc_(integer *,
	     integer *, integer *, integer *, integer *, char *, ftnlen), 
	    dasudd_(integer *, integer *, integer *, doublereal *), sigerr_(
	    char *, ftnlen), prtenc_(integer *, char *, ftnlen), chkout_(char 
	    *, ftnlen);
    static integer forwrd;
    extern /* Subroutine */ int setmsg_(char *, ftnlen), errint_(char *, 
	    integer *, ftnlen);
    static integer npc, npd, npi;

/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines.  Users should not call this routine directly due */
/*     to the volatile nature of this routine. */

/*     Manage EK DAS paging system. */

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

/*     EK */

/* $ Keywords */

/*     EK */
/*     PRIVATE */

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


/*     Include Section:  EK Architecture Version Parameters */

/*        ekarch.inc  Version 1    01-NOV-1995 (NJB) */


/*     The following parameter indicates the EK file architecture */
/*     version.  EK files read by the EK system must have the */
/*     architecture expected by the reader software; the architecture ID */
/*     below is used to test for compatibility. */

/*     Architecture code: */


/*     End Include Section:  EK Architecture Version Parameters */

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


/*     Include Section:  EK Das Paging Parameters */

/*        ekpage.inc  Version 4    25-AUG-1995 (NJB) */



/*     The EK DAS paging system makes use of the integer portion */
/*     of an EK file's DAS address space to store the few numbers */
/*     required to describe the system's state.  The allocation */
/*     of DAS integer addresses is shown below. */


/*                       DAS integer array */

/*        +--------------------------------------------+ */
/*        |            EK architecture code            |  Address = 1 */
/*        +--------------------------------------------+ */
/*        |      Character page size (in DAS words)    | */
/*        +--------------------------------------------+ */
/*        |        Character page base address         | */
/*        +--------------------------------------------+ */
/*        |      Number of character pages in file     | */
/*        +--------------------------------------------+ */
/*        |   Number of character pages on free list   | */
/*        +--------------------------------------------+ */
/*        |      Character free list head pointer      |  Address = 6 */
/*        +--------------------------------------------+ */
/*        |                                            |  Addresses = */
/*        |           Metadata for d.p. pages          |    7--11 */
/*        |                                            | */
/*        +--------------------------------------------+ */
/*        |                                            |  Addresses = */
/*        |         Metadata for integer pages         |    12--16 */
/*        |                                            | */
/*        +--------------------------------------------+ */
/*                              . */
/*                              . */
/*                              . */
/*        +--------------------------------------------+ */
/*        |                                            |  End Address = */
/*        |                Unused space                |  integer page */
/*        |                                            |  end */
/*        +--------------------------------------------+ */
/*        |                                            |  Start Address = */
/*        |             First integer page             |  integer page */
/*        |                                            |  base */
/*        +--------------------------------------------+ */
/*                              . */
/*                              . */
/*                              . */
/*        +--------------------------------------------+ */
/*        |                                            | */
/*        |              Last integer page             | */
/*        |                                            | */
/*        +--------------------------------------------+ */

/*     The following parameters indicate positions of elements in the */
/*     paging system metadata array: */



/*     Number of metadata items per data type: */


/*     Character metadata indices: */


/*     Double precision metadata indices: */


/*     Integer metadata indices: */


/*     Size of metadata area: */


/*     Page sizes, in units of DAS words of the appropriate type: */


/*     Default page base addresses: */


/*     End Include Section:  EK Das Paging Parameters */

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


/*     Include Section:  EK Data Types */

/*        ektype.inc Version 1  27-DEC-1994 (NJB) */


/*     Within the EK system, data types of EK column contents are */
/*     represented by integer codes.  The codes and their meanings */
/*     are listed below. */

/*     Integer codes are also used within the DAS system to indicate */
/*     data types; the EK system makes no assumptions about compatibility */
/*     between the codes used here and those used in the DAS system. */


/*     Character type: */


/*     Double precision type: */


/*     Integer type: */


/*     `Time' type: */

/*     Within the EK system, time values are represented as ephemeris */
/*     seconds past J2000 (TDB), and double precision numbers are used */
/*     to store these values.  However, since time values require special */
/*     treatment both on input and output, and since the `TIME' column */
/*     has a special role in the EK specification and code, time values */
/*     are identified as a type distinct from double precision numbers. */


/*     End Include Section:  EK Data Types */

/* $ Brief_I/O */

/*     Variable  I/O  Entries */
/*     --------  ---  -------------------------------------------------- */
/*     HANDLE     I   PGIN, PGAN, PGAL, PGFR, PGRx, PGWx, PGST. */
/*     TYPE       I   PGBS, PGPG. */
/*     ADDRSS     I   PGPG. */
/*     STAT       I   PGST. */
/*     P         I-O  PGAN, PGAL, PGFR, PGRx, PGWx, PGBS, PGPG. */
/*     PAGEC     I-O  PGRC, PGWC. */
/*     PAGED     I-O  PGRD, PGWD. */
/*     PAGEI     I-O  PGRI, PGWI. */
/*     BASE       O   PGAN, PGAL, PGBS, PGPG. */
/*     VALUE      O   PGST. */

/* $ Detailed_Input */

/*     See the entry points for descriptions of their inputs. */

/* $ Detailed_Output */

/*     See the entry points for descriptions of their outputs. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If this routine is called directly, the error */
/*         SPICE(BOGUSENTRY) will be signaled. */

/*     See the entry points for discussions of errors particular to */
/*     those routines. */

/* $ Files */

/*     This suite of routines provides paged access to DAS files.  Only */
/*     DAS files initialized via a call to ZZEKPGIN may be written or */
/*     read by these routines. */

/* $ Particulars */

/*     The EK paging system provides a means for the rest of the EK */
/*     system to allocate and deallocate contiguous blocks of DAS */
/*     addresses of character, d.p. and integer type.  The rest of the EK */
/*     system never accesses EK files directly; it only reads and writes */
/*     pages allocated via this system. */

/*     Much of the page allocation and de-allocation performed by */
/*     higher-level routines is done via the routines ZZEKAPS and */
/*     ZZEKDPS; those routines should be called if applicable, rather */
/*     than ZZEKPGAL, ZZEKPGAN, or ZZEKPGFR. */

/* $ Examples */

/*     Initialization:               see EKOPN. */
/*     Page allocation:              see EKAPS. */
/*     Writing:                      see ZZEKAD01, ZZEKAD02, ZZEKAD03. */
/*     Reading:                      see ZZEKRD01, ZZEKRD02, ZZEKRD03. */
/*     Freeing pages:                see ZZEKDPS. */
/*     Address-to-page mapping:      see EKDELR. */
/*     Page number-to-base mapping:  see ZZEKAD0x */

/* $ Restrictions */

/*     1) Only `empty' DAS files may be initialized for paged access. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     B.V. Semenov       (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.2.0, 14-OCT-2021 (BVS) */

/*        Bug fix: fixed routine name in CHKIN/CHKOUT calls in */
/*        entry point ZZEKPGPG (ZZEKPGBS -> ZZEKPGPG). */

/* -    SPICELIB Version 1.1.0, 07-FEB-2015 (NJB) */

/*        Now uses ERRHAN to insert DAS file name into */
/*        long error messages. */

/* -    Beta Version 1.0.0, 20-OCT-1995 (NJB) */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     Note:  the integer fill buffer should be as large as the maximum */
/*     of the integer page size and the metadata area size. */


/*     Saved variables */

    /* Parameter adjustments */
    if (paged) {
	}
    if (pagei) {
	}

    /* Function Body */
    switch(n__) {
	case 1: goto L_zzekpgin;
	case 2: goto L_zzekpgan;
	case 3: goto L_zzekpgal;
	case 4: goto L_zzekpgfr;
	case 5: goto L_zzekpgrc;
	case 6: goto L_zzekpgrd;
	case 7: goto L_zzekpgri;
	case 8: goto L_zzekpgwc;
	case 9: goto L_zzekpgwd;
	case 10: goto L_zzekpgwi;
	case 11: goto L_zzekpgbs;
	case 12: goto L_zzekpgpg;
	case 13: goto L_zzekpgst;
	}

    sigerr_("SPICE(BOGUSENTRY)", (ftnlen)17);
    return 0;
/* $Procedure  ZZEKPGIN ( Private: Initialize DAS for paged access ) */

L_zzekpgin:
/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines.  Users should not call this routine directly due */
/*     to the volatile nature of this routine. */

/*     Initialize an open DAS file for paged access. */

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

/*     EK */

/* $ Keywords */

/*     EK */
/*     PRIVATE */

/* $ Declarations */

/*     INTEGER               HANDLE */

/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     HANDLE     I   DAS file handle. */

/* $ Detailed_Input */

/*     HANDLE         is a handle of a DAS file open for write access. */
/*                    The file must be empty:  the last address of */
/*                    each type (character, d.p. and integer) must be */
/*                    zero. */

/* $ Detailed_Output */

/*     None.  This routine operates by side effects; see $Particulars */
/*     for a description of the effect of this routine. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the DAS file designated by HANDLE is not empty, the error */
/*         SPICE(DASNOTEMPTY) is signaled. */

/*     2)  Any read or write errors detected during reading or writing */
/*         the DAS file will be diagnosed by routines called by this */
/*         routine. */

/* $ Files */

/*     This suite of routines provides paged access to DAS files.  Only */
/*     DAS files initialized via a call to ZZEKPGIN may be written or */
/*     read by these routines. */

/* $ Particulars */

/*     This routine initializes a DAS file for paged access. */
/*     Initialization consists of: */

/*        - Setting up the metadata area.  This structure is defined in */
/*          the include file ekpage.inc.  For each data type, there is */
/*          a free list pointer and an allocated page count. */

/*        - Writing the architecture code to the file.  This code is */
/*          defined in the include file ekarch.inc. */

/* $ Examples */

/*     See EKOPN. */

/* $ Restrictions */

/*     1) Only `empty' DAS files may be initialized for paged access. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 07-FEB-2015 (NJB) */

/*        Now uses ERRHAN to insert DAS file name into */
/*        long error messages. */

/* -    Beta Version 1.0.0, 18-OCT-1995 (NJB) */

/* -& */
    chkin_("ZZEKPGIN", (ftnlen)8);

/*     The file must be open for write access. */

    dassih_(handle, "WRITE", (ftnlen)5);
    if (failed_()) {
	chkout_("ZZEKPGIN", (ftnlen)8);
	return 0;
    }

/*     Find out which addresses are already in use.  A file containing */
/*     data cannot be initialized. */

    daslla_(handle, &lastc, &lastd, &lasti);
    if (lastc > 0 || lastd > 0 || lasti > 0) {
	setmsg_("File # contains data; LASTC = #; LASTD = #; LASTI = #.", (
		ftnlen)54);
	errhan_("#", handle, (ftnlen)1);
	errint_("#", &lastc, (ftnlen)1);
	errint_("#", &lastd, (ftnlen)1);
	errint_("#", &lasti, (ftnlen)1);
	sigerr_("SPICE(DASNOTEMPTY)", (ftnlen)18);
	chkout_("ZZEKPGIN", (ftnlen)8);
	return 0;
    }

/*     Initialize our fill buffers. */

    fillc_(" ", &c__1, cfill, (ftnlen)1, (ftnlen)1024);
    filld_(&c_b15, &c__128, dfill);
    filli_(&c__0, &c__256, ifill);

/*     Initialize enough integer addresses to hold the metadata area. */

    dasadi_(handle, &c__256, ifill);

/*     Set the architecture code. */

    dasudi_(handle, &c__1, &c__1, &c__8);

/*     Set the page sizes and base addresses. */

    dasudi_(handle, &c__2, &c__2, &c__1024);
    dasudi_(handle, &c__7, &c__7, &c__128);
    dasudi_(handle, &c__12, &c__12, &c__256);
    dasudi_(handle, &c__3, &c__3, &c__0);
    dasudi_(handle, &c__8, &c__8, &c__0);
    dasudi_(handle, &c__13, &c__13, &c__256);

/*     Since the integer fill value is zero, and since zero is */
/*     interpreted as null pointer, all pointers are initialized. */

    chkout_("ZZEKPGIN", (ftnlen)8);
    return 0;
/* $Procedure  ZZEKPGAN ( Private: EK, allocate new page ) */

L_zzekpgan:
/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines.  Users should not call this routine directly due */
/*     to the volatile nature of this routine. */

/*     Allocate a new page of a specified data type. */

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

/*     EK */

/* $ Keywords */

/*     EK */
/*     PRIVATE */

/* $ Declarations */

/*     INTEGER               HANDLE */
/*     INTEGER               TYPE */
/*     INTEGER               P */
/*     INTEGER               BASE */

/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     HANDLE     I   Paged EK file handle. */
/*     TYPE       I   Data type of page to allocate. */
/*     P          O   Page number. */
/*     BASE       O   DAS base address of page. */

/* $ Detailed_Input */

/*     HANDLE         is a handle of a paged EK file.  The file must */
/*                    be open for write access. */

/*     TYPE           is the data type of the page to allocate.  The */
/*                    type may be CHR, DP, or INT.  Values of these */
/*                    parameters are defined in ektype.inc. */

/* $ Detailed_Output */

/*     P              is the number of an allocated page.  The returned */
/*                    page is never taken from the free list; it is */
/*                    the lowest-addressed page of the specified type */
/*                    that has never been allocated. */

/*     BASE           is the base DAS address of the page.  This address */
/*                    is the predecessor of the first DAS word of the */
/*                    page. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the DAS file designated by HANDLE is not open for paged */
/*         write access, the error will be diagnosed by routines called */
/*         by this routine. */

/*     2)  Any read or write errors detected during reading or writing */
/*         the DAS file will be diagnosed by routines called by this */
/*         routine. */

/*     3)  If the requested data type is not recognized, the error */
/*         SPICE(INVALIDTYPE) is signaled. */

/* $ Files */

/*     This suite of routines provides paged access to DAS files.  Only */
/*     DAS files initialized via a call to ZZEKPGIN may be written or */
/*     read by these routines. */

/* $ Particulars */

/*     The pages returned by this routine lie on DAS record boundaries. */
/*     Successive requests for pages of the same data type will return */
/*     pages that are adjacent in the DAS address space of that type. */
/*     In fact, the main reason to call this routine rather than */
/*     ZZEKPGAL is to allocate adjacent pages. */

/*     Use ZZEKPGAL for normal allocation. */

/* $ Examples */

/*     See EKAPS. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */

/* $ Version */

/* -    Beta Version 1.0.0, 18-OCT-1995 (NJB) */

/* -& */
    chkin_("ZZEKPGAN", (ftnlen)8);

/*     Validate the file. */

    zzekpgch_(handle, "WRITE", (ftnlen)5);
    if (failed_()) {
	chkout_("ZZEKPGAN", (ftnlen)8);
	return 0;
    }
    if (*type__ == 1) {

/*        The new page follows the last character address. */

	dasadc_(handle, &c__1024, &c__1, &c__1024, cfill, (ftnlen)1024);

/*        Update the character page count. */

	dasrdi_(handle, &c__4, &c__4, &npc);
	i__1 = npc + 1;
	dasudi_(handle, &c__4, &c__4, &i__1);

/*        Set the page number and base address. */

	*p = npc + 1;
	*base = npc << 10;
    } else if (*type__ == 2) {
	dasadd_(handle, &c__128, dfill);
	dasrdi_(handle, &c__9, &c__9, &npd);
	i__1 = npd + 1;
	dasudi_(handle, &c__9, &c__9, &i__1);
	*p = npd + 1;
	*base = npd << 7;
    } else if (*type__ == 3) {
	dasadi_(handle, &c__256, ifill);
	dasrdi_(handle, &c__14, &c__14, &npi);
	i__1 = npi + 1;
	dasudi_(handle, &c__14, &c__14, &i__1);
	*p = npi + 1;
	*base = (npi << 8) + 256;
    } else {
	setmsg_("The data type code # was not recognized.", (ftnlen)40);
	errint_("#", type__, (ftnlen)1);
	sigerr_("SPICE(INVALIDTYPE)", (ftnlen)18);
	chkout_("ZZEKPGAN", (ftnlen)8);
	return 0;
    }
    chkout_("ZZEKPGAN", (ftnlen)8);
    return 0;
/* $Procedure  ZZEKPGAL ( Private: EK, allocate page ) */

L_zzekpgal:
/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines.  Users should not call this routine directly due */
/*     to the volatile nature of this routine. */

/*     Allocate a page of a specified data type.  The page need not */
/*     be new:  free pages are returned if possible. */

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

/*     EK */

/* $ Keywords */

/*     EK */
/*     PRIVATE */

/* $ Declarations */

/*     INTEGER               HANDLE */
/*     INTEGER               TYPE */
/*     INTEGER               P */
/*     INTEGER               BASE */

/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     HANDLE     I   Paged EK file handle. */
/*     TYPE       I   Data type of page to allocate. */
/*     P          O   Page number. */
/*     BASE       O   DAS base address of page. */

/* $ Detailed_Input */

/*     HANDLE         is a handle of a paged EK file.  The file must */
/*                    be open for write access. */

/*     TYPE           is the data type of the page to allocate.  The */
/*                    type may be CHR, DP, or INT.  Values of these */
/*                    parameters are defined in ektype.inc. */

/* $ Detailed_Output */

/*     P              is the number of an allocated page.  The returned */
/*                    page is taken from the free list if the free list */
/*                    is non-empty; otherwise, a new page is returned. */

/*     BASE           is the base DAS address of the page.  This address */
/*                    is the predecessor of the first DAS word of the */
/*                    page. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the DAS file designated by HANDLE is not open for paged */
/*         write access, the error will be diagnosed by routines called */
/*         by this routine. */

/*     2)  Any read or write errors detected during reading or writing */
/*         the DAS file will be diagnosed by routines called by this */
/*         routine. */

/*     3)  If the requested data type is not recognized, the error */
/*         SPICE(INVALIDTYPE) is signaled. */

/* $ Files */

/*     This suite of routines provides paged access to DAS files.  Only */
/*     DAS files initialized via a call to ZZEKPGIN may be written or */
/*     read by these routines. */

/* $ Particulars */

/*     This routine should be used for page allocation, except for */
/*     applications requiring allocation of contiguous pages.  If */
/*     contiguous pages are required, use ZZEKPGAN. */

/* $ Examples */

/*     See EKAPS. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */

/* $ Version */

/* -    Beta Version 1.0.0, 18-OCT-1995 (NJB) */

/* -& */
    chkin_("ZZEKPGAL", (ftnlen)8);

/*     Validate the file. */

    zzekpgch_(handle, "WRITE", (ftnlen)5);
    if (failed_()) {
	chkout_("ZZEKPGAL", (ftnlen)8);
	return 0;
    }
    if (*type__ == 1) {

/*        If the character free list is non-empty, take a page from */
/*        that list. */

	dasrdi_(handle, &c__6, &c__6, &freec);
	if (freec > 0) {

/*           We'll return the first free page. */

	    *p = freec;

/*           The new head of the list is the successor of FREEC, if */
/*           any.  Obtain the forward pointer from the page. */

	    addr__ = (freec - 1 << 10) + 1;
	    i__1 = addr__ + 4;
	    dasrdc_(handle, &addr__, &i__1, &c__1, &c__5, encpag, (ftnlen)5);
	    prtdec_(encpag, &forwrd, (ftnlen)5);
	    freec = forwrd;

/*           Decrement the free page count, and write the free pointer */
/*           back to the file. */

	    dasrdi_(handle, &c__5, &c__5, &nfreec);
	    i__1 = nfreec - 1;
	    dasudi_(handle, &c__5, &c__5, &i__1);
	    dasudi_(handle, &c__6, &c__6, &freec);

/*           Set base address. */

	    *base = *p - 1 << 10;
	} else {

/*           The new page follows the last character address. */

	    dasadc_(handle, &c__1024, &c__1, &c__1024, cfill, (ftnlen)1024);

/*           Update the character page count. */

	    dasrdi_(handle, &c__4, &c__4, &npc);
	    i__1 = npc + 1;
	    dasudi_(handle, &c__4, &c__4, &i__1);

/*           Set the page number and base address. */

	    *p = npc + 1;
	    *base = npc << 10;
	}
    } else if (*type__ == 2) {

/*        If the d.p. free list is non-empty, take a page from */
/*        that list. */

	dasrdi_(handle, &c__11, &c__11, &freed);
	if (freed > 0) {

/*           We'll return the first free page. */

	    *p = freed;

/*           The new head of the list is the successor of FREED, if */
/*           any.  Obtain the forward pointer from the page. */

	    addr__ = (freed - 1 << 7) + 1;
	    dasrdd_(handle, &addr__, &addr__, &dpptr);
	    freed = i_dnnt(&dpptr);

/*           Decrement the free page count, and write the free pointer */
/*           back to the file. */

	    dasrdi_(handle, &c__10, &c__10, &nfreed);
	    i__1 = nfreed - 1;
	    dasudi_(handle, &c__10, &c__10, &i__1);
	    dasudi_(handle, &c__11, &c__11, &freed);

/*           Set base address. */

	    *base = *p - 1 << 7;
	} else {

/*           The new page follows the last d.p. address. */

	    dasadd_(handle, &c__128, dfill);

/*           Update the d.p. page count. */

	    dasrdi_(handle, &c__9, &c__9, &npd);
	    i__1 = npd + 1;
	    dasudi_(handle, &c__9, &c__9, &i__1);

/*           Set the page number and base address. */

	    *p = npd + 1;
	    *base = npd << 7;
	}
    } else if (*type__ == 3) {

/*        If the integer free list is non-empty, take a page from */
/*        that list. */

	dasrdi_(handle, &c__16, &c__16, &freei);
	if (freei > 0) {

/*           We'll return the first free page. */

	    *p = freei;

/*           The new head of the list is the successor of FREEI, if */
/*           any.  Obtain the forward pointer from the page. */

	    addr__ = (freei - 1 << 8) + 257;
	    dasrdi_(handle, &addr__, &addr__, &freei);

/*           Decrement the free page count, and write the free pointer */
/*           back to the file. */

	    dasrdi_(handle, &c__15, &c__15, &nfreei);
	    i__1 = nfreei - 1;
	    dasudi_(handle, &c__15, &c__15, &i__1);
	    dasudi_(handle, &c__16, &c__16, &freei);

/*           Set base address. */

	    *base = (*p - 1 << 8) + 256;
	} else {
	    dasadi_(handle, &c__256, ifill);
	    dasrdi_(handle, &c__14, &c__14, &npi);
	    i__1 = npi + 1;
	    dasudi_(handle, &c__14, &c__14, &i__1);
	    *p = npi + 1;
	    *base = (npi << 8) + 256;
	}
    } else {
	setmsg_("The data type code # was not recognized.", (ftnlen)40);
	errint_("#", type__, (ftnlen)1);
	sigerr_("SPICE(INVALIDTYPE)", (ftnlen)18);
	chkout_("ZZEKPGAL", (ftnlen)8);
	return 0;
    }
    chkout_("ZZEKPGAL", (ftnlen)8);
    return 0;
/* $Procedure  ZZEKPGFR ( Private: EK, free page ) */

L_zzekpgfr:
/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines.  Users should not call this routine directly due */
/*     to the volatile nature of this routine. */

/*     Free a specified page. */

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

/*     EK */

/* $ Keywords */

/*     EK */
/*     PRIVATE */

/* $ Declarations */

/*     INTEGER               HANDLE */
/*     INTEGER               TYPE */
/*     INTEGER               P */

/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     HANDLE     I   Paged EK file handle. */
/*     TYPE       I   Data type of page to allocate. */
/*     P          I   Page number. */

/* $ Detailed_Input */

/*     HANDLE         is a handle of a paged EK file.  The file must */
/*                    be open for write access. */

/*     TYPE           is the data type of the page to allocate.  The */
/*                    type may be CHR, DP, or INT.  Values of these */
/*                    parameters are defined in ektype.inc. */

/*     P              is the number of an allocated page to be freed. */

/* $ Detailed_Output */

/*    None.  See $Particulars for a description of the effect of this */
/*    routine. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the DAS file designated by HANDLE is not open for paged */
/*         write access, the error will be diagnosed by routines called */
/*         by this routine. */

/*     2)  Any read or write errors detected during reading or writing */
/*         the DAS file will be diagnosed by routines called by this */
/*         routine. */

/*     3)  If the requested data type is not recognized, the error */
/*         SPICE(INVALIDTYPE) is signaled. */

/*     4)  If the number of the page to be freed is not that of an */
/*         allocated page of the specified type, the error */
/*         SPICE(INVALIDINDEX) is signaled. */

/* $ Files */

/*     This suite of routines provides paged access to DAS files.  Only */
/*     DAS files initialized via a call to ZZEKPGIN may be written or */
/*     read by these routines. */

/* $ Particulars */

/*     This routine should be used for page deallocation.  The input */
/*     page is placed at the head of the free list of the specified */
/*     data type. */

/* $ Examples */

/*     See EKDPS. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */

/* $ Version */

/* -    Beta Version 1.0.0, 18-OCT-1995 (NJB) */

/* -& */
    chkin_("ZZEKPGFR", (ftnlen)8);

/*     Check the file. */

    zzekpgch_(handle, "WRITE", (ftnlen)5);
    if (failed_()) {
	chkout_("ZZEKPGFR", (ftnlen)8);
	return 0;
    }
    if (*type__ == 1) {

/*        Validate the page number.  Find out how many pages are */
/*        out there. */

	dasrdi_(handle, &c__4, &c__4, &npc);
	if (*p < 1 || *p > npc) {
	    setmsg_("Attempt to free non-existent CHR page. Page number = #;"
		    " valid range is 1:#", (ftnlen)74);
	    errint_("#", p, (ftnlen)1);
	    errint_("#", &npc, (ftnlen)1);
	    sigerr_("SPICE(INVALIDINDEX)", (ftnlen)19);
	    chkout_("ZZEKPGFR", (ftnlen)8);
	    return 0;
	}

/*        Get the current character free pointer and free page count. */

	dasrdi_(handle, &c__6, &c__6, &freec);
	dasrdi_(handle, &c__5, &c__5, &nfreec);

/*        Insert into the freed page a pointer to the head of the */
/*        free list. */

	prtenc_(&freec, encpag, (ftnlen)5);
	addr__ = (*p - 1 << 10) + 1;
	i__1 = addr__ + 4;
	dasudc_(handle, &addr__, &i__1, &c__1, &c__5, encpag, (ftnlen)5);

/*        Update the current character free pointer and free page count. */

	dasudi_(handle, &c__6, &c__6, p);
	i__1 = nfreec + 1;
	dasudi_(handle, &c__5, &c__5, &i__1);
    } else if (*type__ == 2) {

/*        Validate the page number.  Find out how many pages are */
/*        out there. */

	dasrdi_(handle, &c__9, &c__9, &npd);
	if (*p < 1 || *p > npd) {
	    setmsg_("Attempt to free non-existent DP page. Page number = #; "
		    "valid range is 1:#", (ftnlen)73);
	    errint_("#", p, (ftnlen)1);
	    errint_("#", &npd, (ftnlen)1);
	    sigerr_("SPICE(INVALIDINDEX)", (ftnlen)19);
	    chkout_("ZZEKPGFR", (ftnlen)8);
	    return 0;
	}

/*        Get the current d.p. free pointer and free page count. */

	dasrdi_(handle, &c__11, &c__11, &freed);
	dasrdi_(handle, &c__10, &c__10, &nfreed);

/*        Insert into the freed page a pointer to the head of the */
/*        free list. */

	addr__ = (*p - 1 << 7) + 1;
	d__1 = (doublereal) freed;
	dasudd_(handle, &addr__, &addr__, &d__1);

/*        Update the current d.p. free pointer and free page count. */

	dasudi_(handle, &c__11, &c__11, p);
	i__1 = nfreed + 1;
	dasudi_(handle, &c__10, &c__10, &i__1);
    } else if (*type__ == 3) {

/*        Validate the page number.  Find out how many pages are */
/*        out there. */

	dasrdi_(handle, &c__14, &c__14, &npi);
	if (*p < 1 || *p > npi) {
	    setmsg_("Attempt to free non-existent INT page. Page number = #;"
		    " valid range is 1:#", (ftnlen)74);
	    errint_("#", p, (ftnlen)1);
	    errint_("#", &npi, (ftnlen)1);
	    sigerr_("SPICE(INVALIDINDEX)", (ftnlen)19);
	    chkout_("ZZEKPGFR", (ftnlen)8);
	    return 0;
	}

/*        Get the current integer free pointer and free page count. */

	dasrdi_(handle, &c__16, &c__16, &freei);
	dasrdi_(handle, &c__15, &c__15, &nfreei);

/*        Insert into the freed page a pointer to the head of the */
/*        free list. */

	addr__ = (*p - 1 << 8) + 257;
	dasudi_(handle, &addr__, &addr__, &freei);

/*        Update the current integer free pointer and free page count. */

	dasudi_(handle, &c__16, &c__16, p);
	i__1 = nfreei + 1;
	dasudi_(handle, &c__15, &c__15, &i__1);
    } else {
	setmsg_("The data type code # was not recognized.", (ftnlen)40);
	errint_("#", type__, (ftnlen)1);
	sigerr_("SPICE(INVALIDTYPE)", (ftnlen)18);
	chkout_("ZZEKPGFR", (ftnlen)8);
	return 0;
    }
    chkout_("ZZEKPGFR", (ftnlen)8);
    return 0;
/* $Procedure  ZZEKPGRC ( Private: EK, read character page ) */

L_zzekpgrc:
/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines.  Users should not call this routine directly due */
/*     to the volatile nature of this routine. */

/*     Read a specified character page. */

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

/*     EK */

/* $ Keywords */

/*     EK */
/*     PRIVATE */

/* $ Declarations */

/*     INTEGER               HANDLE */
/*     INTEGER               P */
/*     CHARACTER*(*)         PAGEC */

/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     HANDLE     I   Paged EK file handle. */
/*     P          I   Page number. */
/*     PAGEC      O   Character page. */

/* $ Detailed_Input */

/*     HANDLE         is a handle of a paged EK file.  The file may */
/*                    be open for read or write access. */

/*     P              is the number of a character page to read. */

/* $ Detailed_Output */

/*     PAGEC          is a string containing the contents of the */
/*                    specified page.  PAGEC should be declared with */
/*                    length at PGSIZC characters.  This parameter is */
/*                    declared in the include file ekpage.inc. */

/*                    If PAGEC has length less than PGSIZC characters, */
/*                    the output will be truncated on the right.  If */
/*                    PAGEC is longer than PGSIZC characters, the output */
/*                    will be padded with trailing blanks. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  Any errors detected during reading the DAS file will be */
/*         diagnosed by routines called by this routine. */

/*     2)  If the number of the page to read is not that of an */
/*         allocated character page, the error SPICE(INVALIDINDEX) is */
/*         signaled. */

/* $ Files */

/*     This suite of routines provides paged access to DAS files.  Only */
/*     DAS files initialized via a call to ZZEKPGIN may be written or */
/*     read by these routines. */

/* $ Particulars */

/*     This routine should be used to read character pages. */

/* $ Examples */

/*     See ZZEKRD03. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */

/* $ Version */

/* -    Beta Version 1.0.0, 18-OCT-1995 (NJB) */

/* -& */

/*     Use discovery check-in. */


/*     Find out how many character pages are in use. */

    dasrdi_(handle, &c__4, &c__4, &npc);
    if (*p < 1 || *p > npc) {
	chkin_("ZZEKPGRC", (ftnlen)8);
	setmsg_("CHR page = #; valid range is [1:#]", (ftnlen)34);
	errint_("#", p, (ftnlen)1);
	errint_("#", &npc, (ftnlen)1);
	sigerr_("SPICE(INVALIDINDEX)", (ftnlen)19);
	chkout_("ZZEKPGRC", (ftnlen)8);
	return 0;
    }
    l = i_len(pagec, pagec_len);
    e = min(l,1024);
    addr__ = (*p - 1 << 10) + 1;
    i__1 = addr__ + 1023;
    dasrdc_(handle, &addr__, &i__1, &c__1, &e, pagec, pagec_len);
    if (l > e) {
	i__1 = e;
	s_copy(pagec + i__1, " ", pagec_len - i__1, (ftnlen)1);
    }
    return 0;
/* $Procedure  ZZEKPGRD ( Private: EK, read d.p. page ) */

L_zzekpgrd:
/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines.  Users should not call this routine directly due */
/*     to the volatile nature of this routine. */

/*     Read a specified double precision page. */

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

/*     EK */

/* $ Keywords */

/*     EK */
/*     PRIVATE */

/* $ Declarations */

/*     INTEGER               HANDLE */
/*     INTEGER               P */
/*     DOUBLE PRECISION      PAGED ( * ) */

/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     HANDLE     I   Paged EK file handle. */
/*     P          I   Page number. */
/*     PAGED      O   Double precision page. */

/* $ Detailed_Input */

/*     HANDLE         is a handle of a paged EK file.  The file may */
/*                    be open for read or write access. */

/*     P              is the number of a double precision page to read. */

/* $ Detailed_Output */

/*     PAGED          is a double precision array containing the contents */
/*                    of the specified page.  PAGED should be declared */
/*                    with dimension PGSIZD.  This parameter is */
/*                    declared in the include file ekpage.inc. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  Any errors detected during reading the DAS file will be */
/*         diagnosed by routines called by this routine. */

/*     2)  If the number of the page to read is not that of an */
/*         allocated double precision page, the error SPICE(INVALIDINDEX) */
/*         is signaled. */

/* $ Files */

/*     This suite of routines provides paged access to DAS files.  Only */
/*     DAS files initialized via a call to ZZEKPGIN may be written or */
/*     read by these routines. */

/* $ Particulars */

/*     This routine should be used to read double precision pages. */

/* $ Examples */

/*     See ZZEKRD02. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */

/* $ Version */

/* -    Beta Version 1.0.0, 18-OCT-1995 (NJB) */

/* -& */

/*     Use discovery check-in. */


/*     Find out how many d.p. pages are in use. */

    dasrdi_(handle, &c__9, &c__9, &npd);
    if (*p < 1 || *p > npd) {
	chkin_("ZZEKPGRD", (ftnlen)8);
	setmsg_("DP page = #; valid range is [1:#]", (ftnlen)33);
	errint_("#", p, (ftnlen)1);
	errint_("#", &npd, (ftnlen)1);
	sigerr_("SPICE(INVALIDINDEX)", (ftnlen)19);
	chkout_("ZZEKPGRD", (ftnlen)8);
	return 0;
    }
    addr__ = (*p - 1 << 7) + 1;
    i__1 = addr__ + 127;
    dasrdd_(handle, &addr__, &i__1, paged);
    return 0;
/* $Procedure  ZZEKPGRI ( Private: EK, read integer page ) */

L_zzekpgri:
/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines.  Users should not call this routine directly due */
/*     to the volatile nature of this routine. */

/*     Read a specified integer page. */

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

/*     EK */

/* $ Keywords */

/*     EK */
/*     PRIVATE */

/* $ Declarations */

/*     INTEGER               HANDLE */
/*     INTEGER               P */
/*     INTEGER               PAGEI ( * ) */

/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     HANDLE     I   Paged EK file handle. */
/*     P          I   Page number. */
/*     PAGEI      O   Integer page. */

/* $ Detailed_Input */

/*     HANDLE         is a handle of a paged EK file.  The file may */
/*                    be open for read or write access. */

/*     P              is the number of an integer page to read. */

/* $ Detailed_Output */

/*     PAGEI          is an integer array containing the contents */
/*                    of the specified page.  PAGEI should be declared */
/*                    with dimension PGSIZI.  This parameter is */
/*                    declared in the include file ekpage.inc. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  Any errors detected during reading the DAS file will be */
/*         diagnosed by routines called by this routine. */

/*     2)  If the number of the page to read is not that of an */
/*         allocated double precision page, the error SPICE(INVALIDINDEX) */
/*         is signaled. */

/* $ Files */

/*     This suite of routines provides paged access to DAS files.  Only */
/*     DAS files initialized via a call to ZZEKPGIN may be written or */
/*     read by these routines. */

/* $ Particulars */

/*     This routine should be used to read integer pages. */

/* $ Examples */

/*     See ZZEKRD01. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */

/* $ Version */

/* -    Beta Version 1.0.0, 18-OCT-1995 (NJB) */

/* -& */

/*     Use discovery check-in. */


/*     Find out how many integer pages are in use. */

    dasrdi_(handle, &c__14, &c__14, &npi);
    if (*p < 1 || *p > npi) {
	chkin_("ZZEKPGRI", (ftnlen)8);
	setmsg_("INT page = #; valid range is [1:#]", (ftnlen)34);
	errint_("#", p, (ftnlen)1);
	errint_("#", &npi, (ftnlen)1);
	sigerr_("SPICE(INVALIDINDEX)", (ftnlen)19);
	chkout_("ZZEKPGRI", (ftnlen)8);
	return 0;
    }
    addr__ = (*p - 1 << 8) + 257;
    i__1 = addr__ + 255;
    dasrdi_(handle, &addr__, &i__1, pagei);
    return 0;
/* $Procedure  ZZEKPGWC ( Private: EK, write character page ) */

L_zzekpgwc:
/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines.  Users should not call this routine directly due */
/*     to the volatile nature of this routine. */

/*     Write a specified character page. */

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

/*     EK */

/* $ Keywords */

/*     EK */
/*     PRIVATE */

/* $ Declarations */

/*     INTEGER               HANDLE */
/*     INTEGER               P */
/*     CHARACTER*(*)         PAGEC */

/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     HANDLE     I   Paged EK file handle. */
/*     P          I   Page number. */
/*     PAGEC      I   Character page. */

/* $ Detailed_Input */

/*     HANDLE         is a handle of a paged EK file.  The file must */
/*                    be open for write access. */

/*     P              is the number of an allocated character page to */
/*                    write. */

/*     PAGEC          is a string to be written to the specified page. */
/*                    PAGEC must be declared with length at PGSIZC */
/*                    characters.  This parameter is declared in the */
/*                    include file ekpage.inc. */

/* $ Detailed_Output */

/*     None.  See $Particulars for a description of the effect of this */
/*     routine. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  Any errors detected during reading or writing the DAS file */
/*         will be diagnosed by routines called by this routine. */

/*     2)  If the number of the page to write is not that of an */
/*         allocated character page, the error SPICE(INVALIDINDEX) is */
/*         signaled. */

/*     3)  If the input string has length less than PGSIZC characters, */
/*         the error SPICE(STRINGTOOSHORT) is signaled. */

/* $ Files */

/*     This suite of routines provides paged access to DAS files.  Only */
/*     DAS files initialized via a call to ZZEKPGIN may be written or */
/*     read by these routines. */

/* $ Particulars */

/*     This routine writes the input string to the DAS address range */
/*     corresponding to the specified page.  The file must be closed */
/*     properly (via EKCLS) in order to make the change permanent. */

/* $ Examples */

/*     See ZZEKAD03. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */

/* $ Version */

/* -    Beta Version 1.0.0, 18-OCT-1995 (NJB) */

/* -& */

/*     Use discovery check-in. */

/*     Validate the file. */

    zzekpgch_(handle, "WRITE", (ftnlen)5);
    if (failed_()) {
	return 0;
    }

/*     Find out how many character pages are in use. */

    dasrdi_(handle, &c__4, &c__4, &npc);
    if (*p < 1 || *p > npc) {
	chkin_("ZZEKPGWC", (ftnlen)8);
	setmsg_("CHR page = #; valid range is [1:#]", (ftnlen)34);
	errint_("#", p, (ftnlen)1);
	errint_("#", &npc, (ftnlen)1);
	sigerr_("SPICE(INVALIDINDEX)", (ftnlen)19);
	chkout_("ZZEKPGWC", (ftnlen)8);
	return 0;
    }
    l = i_len(pagec, pagec_len);
    if (l < 1024) {
	chkin_("ZZEKPGWC", (ftnlen)8);
	setmsg_("Input CHR page size = #; valid size is [#:]", (ftnlen)43);
	errint_("#", &l, (ftnlen)1);
	errint_("#", &c__1024, (ftnlen)1);
	sigerr_("SPICE(STRINGTOOSHORT)", (ftnlen)21);
	chkout_("ZZEKPGWC", (ftnlen)8);
	return 0;
    }
    addr__ = (*p - 1 << 10) + 1;
    i__1 = addr__ + 1023;
    dasudc_(handle, &addr__, &i__1, &c__1, &c__1024, pagec, pagec_len);
    return 0;
/* $Procedure  ZZEKPGWD ( Private: EK, write d.p. page ) */

L_zzekpgwd:
/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines.  Users should not call this routine directly due */
/*     to the volatile nature of this routine. */

/*     Write a specified double precision page. */

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

/*     EK */

/* $ Keywords */

/*     EK */
/*     PRIVATE */

/* $ Declarations */

/*     INTEGER               HANDLE */
/*     INTEGER               P */
/*     DOUBLE PRECISION      PAGED ( * ) */

/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     HANDLE     I   Paged EK file handle. */
/*     P          I   Page number. */
/*     PAGED      I   Double precision page. */

/* $ Detailed_Input */

/*     HANDLE         is a handle of a paged EK file.  The file must */
/*                    be open for write access. */

/*     P              is the number of an allocated double precision */
/*                    page to write. */

/*     PAGED          is a double precision array to be written to */
/*                    the specified page.  PAGED must be declared with */
/*                    dimension at PGSIZD.  This parameter is */
/*                    declared in the include file ekpage.inc. */

/* $ Detailed_Output */

/*     None.  See $Particulars for a description of the effect of this */
/*     routine. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  Any errors detected during reading or writing the DAS file */
/*         will be diagnosed by routines called by this routine. */

/*     2)  If the number of the page to write is not that of an */
/*         allocated d.p. page, the error SPICE(INVALIDINDEX) is */
/*         signaled. */

/* $ Files */

/*     This suite of routines provides paged access to DAS files.  Only */
/*     DAS files initialized via a call to ZZEKPGIN may be written or */
/*     read by these routines. */

/* $ Particulars */

/*     This routine writes the input array to the DAS address range */
/*     corresponding to the specified page.  The file must be closed */
/*     properly (via EKCLS) in order to make the change permanent. */

/* $ Examples */

/*     See ZZEKAD02. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */

/* $ Version */

/* -    Beta Version 1.0.0, 18-OCT-1995 (NJB) */

/* -& */

/*     Use discovery check-in. */


/*     Validate the file. */

    zzekpgch_(handle, "WRITE", (ftnlen)5);
    if (failed_()) {
	return 0;
    }

/*     Find out how many d.p. pages are in use. */

    dasrdi_(handle, &c__9, &c__9, &npd);
    if (*p < 1 || *p > npd) {
	chkin_("ZZEKPGWD", (ftnlen)8);
	setmsg_("DP page = #; valid range is [1:#]", (ftnlen)33);
	errint_("#", p, (ftnlen)1);
	errint_("#", &npd, (ftnlen)1);
	sigerr_("SPICE(INVALIDINDEX)", (ftnlen)19);
	chkout_("ZZEKPGWD", (ftnlen)8);
	return 0;
    }
    addr__ = (*p - 1 << 7) + 1;
    i__1 = addr__ + 127;
    dasudd_(handle, &addr__, &i__1, paged);
    return 0;
/* $Procedure  ZZEKPGWI ( Private: EK, write integer page ) */

L_zzekpgwi:
/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines.  Users should not call this routine directly due */
/*     to the volatile nature of this routine. */

/*     Write a specified integer page. */

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

/*     EK */

/* $ Keywords */

/*     EK */
/*     PRIVATE */

/* $ Declarations */

/*     INTEGER               HANDLE */
/*     INTEGER               P */
/*     INTEGER               PAGEI ( * ) */

/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     HANDLE     I   Paged EK file handle. */
/*     P          I   Page number. */
/*     PAGEI      I   Integer page. */

/* $ Detailed_Input */

/*     HANDLE         is a handle of a paged EK file.  The file must */
/*                    be open for write access. */

/*     P              is the number of an allocated integer */
/*                    page to write. */

/*     PAGEI          is an integer array to be written to */
/*                    the specified page.  PAGEI must be declared with */
/*                    dimension at PGSIZI.  This parameter is */
/*                    declared in the include file ekpage.inc. */

/* $ Detailed_Output */

/*     None.  See $Particulars for a description of the effect of this */
/*     routine. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  Any errors detected during reading or writing the DAS file */
/*         will be diagnosed by routines called by this routine. */

/*     2)  If the number of the page to write is not that of an */
/*         allocated integer page, the error SPICE(INVALIDINDEX) is */
/*         signaled. */

/* $ Files */

/*     This suite of routines provides paged access to DAS files.  Only */
/*     DAS files initialized via a call to ZZEKPGIN may be written or */
/*     read by these routines. */

/* $ Particulars */

/*     This routine writes the input array to the DAS address range */
/*     corresponding to the specified page.  The file must be closed */
/*     properly (via EKCLS) in order to make the change permanent. */

/* $ Examples */

/*     See ZZEKAD01. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */

/* $ Version */

/* -    Beta Version 1.0.0, 18-OCT-1995 (NJB) */

/* -& */

/*     Use discovery check-in. */

/*     Validate the file. */

    zzekpgch_(handle, "WRITE", (ftnlen)5);
    if (failed_()) {
	return 0;
    }

/*     Find out how many integer pages are in use. */

    dasrdi_(handle, &c__14, &c__14, &npi);
    if (*p < 1 || *p > npi) {
	chkin_("ZZEKPGWI", (ftnlen)8);
	setmsg_("INT page = #; valid range is [1:#]", (ftnlen)34);
	errint_("#", p, (ftnlen)1);
	errint_("#", &npi, (ftnlen)1);
	sigerr_("SPICE(INVALIDINDEX)", (ftnlen)19);
	chkout_("ZZEKPGWI", (ftnlen)8);
	return 0;
    }
    addr__ = (*p - 1 << 8) + 257;
    i__1 = addr__ + 255;
    dasudi_(handle, &addr__, &i__1, pagei);
    return 0;
/* $Procedure  ZZEKPGBS ( Private: EK, map page to base address ) */

L_zzekpgbs:
/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines.  Users should not call this routine directly due */
/*     to the volatile nature of this routine. */

/*     Map a page to its base address. */

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

/*     EK */

/* $ Keywords */

/*     EK */
/*     PRIVATE */

/* $ Declarations */

/*     INTEGER               TYPE */
/*     INTEGER               P */
/*     INTEGER               BASE */

/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     TYPE       I   Data type of page. */
/*     P          I   Page number. */
/*     BASE       O   DAS base address of page. */

/* $ Detailed_Input */

/*     TYPE           is the data type of the page whose base address */
/*                    is requested.  The type may be CHR, DP, or INT. */
/*                    Values of these parameters are defined in */
/*                    ektype.inc. */

/*     P              is the number of the page of interest. */

/* $ Detailed_Output */

/*     BASE           is the base DAS address of the page.  This address */
/*                    is the predecessor of the first DAS word of the */
/*                    page. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the requested data type is not recognized, the error */
/*         SPICE(INVALIDTYPE) is signaled. */

/*     2)  Range checking is not performed on the input page number P. */

/* $ Files */

/*     This suite of routines provides paged access to DAS files.  Only */
/*     DAS files initialized via a call to ZZEKPGIN may be written or */
/*     read by these routines. */

/* $ Particulars */

/*     This routine provides translation from page numbers to DAS */
/*     addresses. */

/* $ Examples */

/*     See EKDELR. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */

/* $ Version */

/* -    Beta Version 1.0.0, 18-OCT-1995 (NJB) */

/* -& */
    if (*type__ == 1) {
	*base = *p - 1 << 10;
    } else if (*type__ == 2) {
	*base = *p - 1 << 7;
    } else if (*type__ == 3) {
	*base = (*p - 1 << 8) + 256;
    } else {
	chkin_("ZZEKPGBS", (ftnlen)8);
	setmsg_("The data type code # was not recognized.", (ftnlen)40);
	errint_("#", type__, (ftnlen)1);
	sigerr_("SPICE(INVALIDTYPE)", (ftnlen)18);
	chkout_("ZZEKPGBS", (ftnlen)8);
	return 0;
    }
    return 0;
/* $Procedure  ZZEKPGPG ( Private: EK, map address to page ) */

L_zzekpgpg:
/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines.  Users should not call this routine directly due */
/*     to the volatile nature of this routine. */

/*     Map a DAS address to the number of the page containing it.  Also */
/*     return the base address of the page. */

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

/*     EK */

/* $ Keywords */

/*     EK */
/*     PRIVATE */

/* $ Declarations */

/*     INTEGER               TYPE */
/*     INTEGER               ADDRSS */
/*     INTEGER               P */
/*     INTEGER               BASE */

/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     TYPE       I   Data type of address. */
/*     ADDRSS     I   DAS address to be mapped. */
/*     P          O   Page number. */
/*     BASE       O   DAS base address of page. */

/* $ Detailed_Input */

/*     TYPE           is the data type of a DAS address to be mapped to */
/*                    a page number.  The type may be CHR, DP, or INT. */
/*                    Values of these parameters are defined in */
/*                    ektype.inc. */

/*     ADDRSS         is a DAS address to be mapped to a page number. */

/* $ Detailed_Output */

/*     P              is the number of the page containing the input */
/*                    address. */

/*     BASE           is the base DAS address of the page.  This address */
/*                    is the predecessor of the first DAS word of the */
/*                    page. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the requested data type is not recognized, the error */
/*         SPICE(INVALIDTYPE) is signaled. */

/*     2)  Range checking is not performed on the input address. */

/* $ Files */

/*     This suite of routines provides paged access to DAS files.  Only */
/*     DAS files initialized via a call to ZZEKPGIN may be written or */
/*     read by these routines. */

/* $ Particulars */

/*     This routine provides translation from DAS addresses to page */
/*     numbers. */

/* $ Examples */

/*     See ZZEKAD01. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     B.V. Semenov       (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 14-OCT-2021 (BVS) */

/*        Bug fix: fixed routine name in CHKIN/CHKOUT calls (ZZEKPGBS */
/*        -> ZZEKPGPG). */

/* -    Beta Version 1.0.0, 18-OCT-1995 (NJB) */

/* -& */
    if (*type__ == 1) {
	*p = (*addrss + 1023) / 1024;
	*base = *p - 1 << 10;
    } else if (*type__ == 2) {
	*p = (*addrss + 127) / 128;
	*base = *p - 1 << 7;
    } else if (*type__ == 3) {
	*p = (*addrss - 1) / 256;
	*base = (*p - 1 << 8) + 256;
    } else {
	chkin_("ZZEKPGPG", (ftnlen)8);
	setmsg_("The data type code # was not recognized.", (ftnlen)40);
	errint_("#", type__, (ftnlen)1);
	sigerr_("SPICE(INVALIDTYPE)", (ftnlen)18);
	chkout_("ZZEKPGPG", (ftnlen)8);
	return 0;
    }
    return 0;
/* $Procedure  ZZEKPGST ( Private: EK, return paging statistics ) */

L_zzekpgst:
/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines.  Users should not call this routine directly due */
/*     to the volatile nature of this routine. */

/*     Return paging statistics. */

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

/*     EK */

/* $ Keywords */

/*     EK */
/*     PRIVATE */

/* $ Declarations */

/*     INTEGER               HANDLE */
/*     CHARACTER*(*)         STAT */
/*     INTEGER               VALUE */

/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     HANDLE     I   Paged EK file handle. */
/*     STAT       I   Name of requested statistic. */
/*     VALUE      O   Value of requested statistic. */

/* $ Detailed_Input */

/*     HANDLE         is a handle of a paged EK file.  The file may */
/*                    be open for read or write access. */

/*     STAT           is the name of the requested statistic.  Possible */
/*                    values and meanings of STAT are: */

/*                      'N_C_ALLOC'     Number of character pages */
/*                                      allocated.  Pages on the free */
/*                                      list are not included. */

/*                      'N_D_ALLOC'     Number of d.p. pages allocated. */

/*                      'N_I_ALLOC'     Number of integer pages */
/*                                      allocated. */

/*                      'N_C_FREE'      Number of pages in character free */
/*                                      list. */

/*                      'N_D_FREE'      Number of pages in d.p. free */
/*                                      list. */

/*                      'N_I_FREE'      Number of pages in integer free */
/*                                      list. */

/* $ Detailed_Output */

/*     VALUE          is the value of the requested statistic. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the requested statistic is not recognized, the error */
/*         SPICE(INVALIDOPTION) is signaled. */

/* $ Files */

/*     This suite of routines provides paged access to DAS files.  Only */
/*     DAS files initialized via a call to ZZEKPGIN may be written or */
/*     read by these routines. */

/* $ Particulars */

/*     This routine provides translation from DAS addresses to page */
/*     numbers. */

/* $ Examples */

/*     1)  Find the number of pages on the integer free list of the */
/*         paged EK designated by HANDLE: */

/*            CALL ZZEKPGST ( HANDLE, 'N_I_FREE', NFREE ) */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */

/* $ Version */

/* -    Beta Version 1.0.0, 18-OCT-1995 (NJB) */

/* -& */
    chkin_("ZZEKPGST", (ftnlen)8);
    if (eqstr_(stat, "N_C_ALLOC", stat_len, (ftnlen)9)) {
	dasrdi_(handle, &c__4, &c__4, value);
    } else if (eqstr_(stat, "N_D_ALLOC", stat_len, (ftnlen)9)) {
	dasrdi_(handle, &c__9, &c__9, value);
    } else if (eqstr_(stat, "N_I_ALLOC", stat_len, (ftnlen)9)) {
	dasrdi_(handle, &c__14, &c__14, value);
    } else if (eqstr_(stat, "N_C_FREE", stat_len, (ftnlen)8)) {
	dasrdi_(handle, &c__5, &c__5, value);
    } else if (eqstr_(stat, "N_D_FREE", stat_len, (ftnlen)8)) {
	dasrdi_(handle, &c__10, &c__10, value);
    } else if (eqstr_(stat, "N_I_FREE", stat_len, (ftnlen)8)) {
	dasrdi_(handle, &c__15, &c__15, value);
    } else {
	setmsg_("Statistic # is not supported.", (ftnlen)29);
	errch_("#", stat, (ftnlen)1, stat_len);
	sigerr_("SPICE(INVALIDOPTION)", (ftnlen)20);
	chkout_("ZZEKPGST", (ftnlen)8);
	return 0;
    }
    chkout_("ZZEKPGST", (ftnlen)8);
    return 0;
} /* zzekpage_ */

/* Subroutine */ int zzekpage_(integer *handle, integer *type__, integer *
	addrss, char *stat, integer *p, char *pagec, doublereal *paged, 
	integer *pagei, integer *base, integer *value, ftnlen stat_len, 
	ftnlen pagec_len)
{
    return zzekpage_0_(0, handle, type__, addrss, stat, p, pagec, paged, 
	    pagei, base, value, stat_len, pagec_len);
    }

/* Subroutine */ int zzekpgin_(integer *handle)
{
    return zzekpage_0_(1, handle, (integer *)0, (integer *)0, (char *)0, (
	    integer *)0, (char *)0, (doublereal *)0, (integer *)0, (integer *)
	    0, (integer *)0, (ftnint)0, (ftnint)0);
    }

/* Subroutine */ int zzekpgan_(integer *handle, integer *type__, integer *p, 
	integer *base)
{
    return zzekpage_0_(2, handle, type__, (integer *)0, (char *)0, p, (char *)
	    0, (doublereal *)0, (integer *)0, base, (integer *)0, (ftnint)0, (
	    ftnint)0);
    }

/* Subroutine */ int zzekpgal_(integer *handle, integer *type__, integer *p, 
	integer *base)
{
    return zzekpage_0_(3, handle, type__, (integer *)0, (char *)0, p, (char *)
	    0, (doublereal *)0, (integer *)0, base, (integer *)0, (ftnint)0, (
	    ftnint)0);
    }

/* Subroutine */ int zzekpgfr_(integer *handle, integer *type__, integer *p)
{
    return zzekpage_0_(4, handle, type__, (integer *)0, (char *)0, p, (char *)
	    0, (doublereal *)0, (integer *)0, (integer *)0, (integer *)0, (
	    ftnint)0, (ftnint)0);
    }

/* Subroutine */ int zzekpgrc_(integer *handle, integer *p, char *pagec, 
	ftnlen pagec_len)
{
    return zzekpage_0_(5, handle, (integer *)0, (integer *)0, (char *)0, p, 
	    pagec, (doublereal *)0, (integer *)0, (integer *)0, (integer *)0, 
	    (ftnint)0, pagec_len);
    }

/* Subroutine */ int zzekpgrd_(integer *handle, integer *p, doublereal *paged)
{
    return zzekpage_0_(6, handle, (integer *)0, (integer *)0, (char *)0, p, (
	    char *)0, paged, (integer *)0, (integer *)0, (integer *)0, (
	    ftnint)0, (ftnint)0);
    }

/* Subroutine */ int zzekpgri_(integer *handle, integer *p, integer *pagei)
{
    return zzekpage_0_(7, handle, (integer *)0, (integer *)0, (char *)0, p, (
	    char *)0, (doublereal *)0, pagei, (integer *)0, (integer *)0, (
	    ftnint)0, (ftnint)0);
    }

/* Subroutine */ int zzekpgwc_(integer *handle, integer *p, char *pagec, 
	ftnlen pagec_len)
{
    return zzekpage_0_(8, handle, (integer *)0, (integer *)0, (char *)0, p, 
	    pagec, (doublereal *)0, (integer *)0, (integer *)0, (integer *)0, 
	    (ftnint)0, pagec_len);
    }

/* Subroutine */ int zzekpgwd_(integer *handle, integer *p, doublereal *paged)
{
    return zzekpage_0_(9, handle, (integer *)0, (integer *)0, (char *)0, p, (
	    char *)0, paged, (integer *)0, (integer *)0, (integer *)0, (
	    ftnint)0, (ftnint)0);
    }

/* Subroutine */ int zzekpgwi_(integer *handle, integer *p, integer *pagei)
{
    return zzekpage_0_(10, handle, (integer *)0, (integer *)0, (char *)0, p, (
	    char *)0, (doublereal *)0, pagei, (integer *)0, (integer *)0, (
	    ftnint)0, (ftnint)0);
    }

/* Subroutine */ int zzekpgbs_(integer *type__, integer *p, integer *base)
{
    return zzekpage_0_(11, (integer *)0, type__, (integer *)0, (char *)0, p, (
	    char *)0, (doublereal *)0, (integer *)0, base, (integer *)0, (
	    ftnint)0, (ftnint)0);
    }

/* Subroutine */ int zzekpgpg_(integer *type__, integer *addrss, integer *p, 
	integer *base)
{
    return zzekpage_0_(12, (integer *)0, type__, addrss, (char *)0, p, (char *
	    )0, (doublereal *)0, (integer *)0, base, (integer *)0, (ftnint)0, 
	    (ftnint)0);
    }

/* Subroutine */ int zzekpgst_(integer *handle, char *stat, integer *value, 
	ftnlen stat_len)
{
    return zzekpage_0_(13, handle, (integer *)0, (integer *)0, stat, (integer 
	    *)0, (char *)0, (doublereal *)0, (integer *)0, (integer *)0, 
	    value, stat_len, (ftnint)0);
    }

