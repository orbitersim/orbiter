/* sc01.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__10 = 10;
static integer c__30 = 30;
static integer c__0 = 0;
static integer c__1 = 1;
static integer c__3 = 3;
static integer c__2 = 2;

/* $Procedure SC01 ( Spacecraft clock, type 1 ) */
/* Subroutine */ int sc01_0_(int n__, integer *sc, char *clkstr, doublereal *
	ticks, doublereal *sclkdp, doublereal *et, integer *npartn, 
	doublereal *parbeg, doublereal *parend, integer *clktyp, ftnlen 
	clkstr_len)
{
    /* Initialized data */

    static char bvlmsg[320] = "Invalid value of #. Value was #.             "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                   ";
    static integer dpfree = 1;
    static integer ifree = 1;
    static char del[1*5] = "." ":" "-" "," " ";
    static logical pass1 = TRUE_;
    static integer prvsc = 0;
    static integer cofbas = 0;
    static integer strbas = 0;
    static integer endbas = 0;
    static integer modbas = 0;
    static integer offbas = 0;

    /* System generated locals */
    integer i__1, i__2, i__3, i__4;
    doublereal d__1;

    /* Builtin functions */
    integer s_cmp(char *, char *, ftnlen, ftnlen), s_rnge(char *, integer, 
	    char *, integer);
    double d_nint(doublereal *), d_lg10(doublereal *);
    integer i_len(char *, ftnlen);
    double d_int(doublereal *), d_mod(doublereal *, doublereal *);
    integer i_indx(char *, char *, ftnlen, ftnlen);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    static doublereal dval, rate;
    static integer pntr;
    extern /* Subroutine */ int zzpctrck_(integer *, logical *), zzctruin_(
	    integer *);
    static integer i__, j, n;
    extern /* Subroutine */ int chkin_(char *, ftnlen), errch_(char *, char *,
	     ftnlen, ftnlen);
    static logical found;
    extern /* Subroutine */ int errdp_(char *, doublereal *, ftnlen);
    extern integer sumai_(integer *, integer *);
    extern /* Subroutine */ int repmi_(char *, char *, integer *, char *, 
	    ftnlen, ftnlen, ftnlen), moved_(doublereal *, integer *, 
	    doublereal *);
    static integer npart;
    static doublereal const__;
    static integer lower;
    static char error[240];
    static integer upper, delcde;
    extern logical failed_(void);
    static integer needed, middle, scbase[100];
    static char dpchar[30];
    static integer ncoeff, nfield;
    static doublereal dpbuff[320018];
    static integer hdsclk[100];
    static doublereal tikdif;
    static integer scpool[106], clklst[100];
    static char kvname[32];
    extern doublereal unitim_(doublereal *, char *, char *, ftnlen, ftnlen);
    static doublereal cmptks[10], cmpval[10], maxwid;
    extern logical return_(void);
    static doublereal mxtick, partim, savcmp, tikmsc, timdif;
    static integer cmpwid[10], intbuf[1000], length[10], polctr[2], timsys;
    static logical samclk, update;
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), setmsg_(char *, ftnlen), lparsm_(char *, char *, integer 
	    *, integer *, char *, ftnlen, ftnlen, ftnlen), errint_(char *, 
	    integer *, ftnlen), nparsd_(char *, doublereal *, char *, integer 
	    *, ftnlen, ftnlen), dpstrf_(doublereal *, integer *, char *, char 
	    *, ftnlen, ftnlen), prefix_(char *, integer *, char *, ftnlen, 
	    ftnlen), suffix_(char *, integer *, char *, ftnlen, ftnlen), 
	    gipool_(char *, integer *, integer *, integer *, integer *, 
	    logical *, ftnlen);
    static integer pad, end;
    static char cmp[30*10];
    static doublereal rem;
    extern /* Subroutine */ int zzscin01_(integer *, integer *, integer *, 
	    integer *, integer *, integer *), zzscup01_(integer *, integer *, 
	    integer *, integer *, integer *, integer *, doublereal *, integer 
	    *, integer *, integer *, integer *, integer *, integer *, integer 
	    *, integer *, integer *, integer *, integer *, integer *, integer 
	    *, integer *);

/* $ Abstract */

/*     Perform time conversions between different representations of */
/*     type 1 spacecraft clock. */

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
/*     TIME */

/* $ Keywords */

/*     CONVERSION */
/*     TIME */

/* $ Declarations */
/* $ Abstract */

/*     This include file defines the dimension of the counter */
/*     array used by various SPICE subsystems to uniquely identify */
/*     changes in their states. */

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

/*     CTRSIZ      is the dimension of the counter array used by */
/*                 various SPICE subsystems to uniquely identify */
/*                 changes in their states. */

/* $ Author_and_Institution */

/*     B.V. Semenov    (JPL) */

/* $ Literature_References */

/*     None. */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 29-JUL-2013 (BVS) */

/* -& */

/*     End of include file. */


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

/*     VARIABLE  I/O  ENTRY POINTS */
/*     --------  ---  -------------------------------------------------- */
/*     SC         I   (All) */
/*     CLKSTR    I-O  SCTK01, SCFM01 */
/*     TICKS     I-O  SCTK01, SCFM01 */
/*     SCLKDP    I-O  SCTE01, SCET01, SCEC01 */
/*     ET        I-O  SCTE01, SCET01, SCEC01 */
/*     NPARTN     O   SCPR01 */
/*     PARBEG     O   SCPR01 */
/*     PAREND     O   SCPR01 */
/*     CLKTYP     O   SCTY01 */
/*     MXCOEF     P   SCTE01, SCET01 */
/*     MXPART     P   (All) */
/*     DELIMS     P   SCTK01, SCFM01 */
/*     MXNFLD     P   SCTK01, SCFM01 */
/*     DPLEN      P   SCTK01, SCFM01 */

/* $ Detailed_Input */

/*     See the entry points SCTK01, SCFM01, SCET01, SCTE01, SCEC01. */

/* $ Detailed_Output */

/*     See the entry points SCTK01, SCFM01, SCET01, SCTE01, SCEC01. */

/* $ Parameters */

/*     MXCOEF   is the maximum number of coefficient sets in the */
/*              array COEFFS that defines the mapping between */
/*              encoded type 1 SCLK and a parallel time system, */
/*              such as TDB or TDT. This array has dimension */
/*              3 x MXCOEF. The value of MXCOEF may be increased */
/*              as required. */

/*     MXPART   is the maximum number of partitions for any type 1 */
/*              spacecraft clock. Type 1 SCLK kernels contain */
/*              start and stop times for each partition. The value */
/*              of MXPART may be increased as required. */

/*     MXNFLD   is an upper bound on the number of components in */
/*              the clock string. */

/*     DPLEN    is an upper bound on the width of the individual */
/*              components of the clock string. */

/*     DELIMS   are the characters that are accepted delimiters of */
/*              the clock components in the input SCLK string. */

/* $ Exceptions */

/*     1)  If SC01 is called directly, the error SPICE(BOGUSENTRY) */
/*         is signaled. */

/*     2)  See the entry points SCTK01, SCFM01, SCET01, SCTE01 for a */
/*         description of the exceptions specific to those routines. */

/* $ Files */

/*     An SCLK kernel appropriate to the spacecraft clock identified */
/*     by SC must be loaded at the time any entry point of this */
/*     routine is called. */

/*     If the SCLK kernel used with this routine does not map SCLK */
/*     directly to barycentric dynamical time, a leapseconds kernel */
/*     must be loaded at the time any entry point of this routine is */
/*     called. */

/*     Normally kernels are loaded by user applications once at the */
/*     start of execution. It is not necessary to load them repeatedly. */

/* $ Particulars */

/*     SC01 serves as an umbrella routine under which the shared */
/*     variables of its entry points are declared.  SC01 should */
/*     never be called directly. */

/*     The entry points of SC01 are */

/*        SCTK01 ( SCLK to ticks,          type 1 ) */
/*        SCFM01 ( Format,                 type 1 ) */
/*        SCET01 ( ET to ticks,            type 1 ) */
/*        SCEC01 ( ET to continuous ticks, type 1 ) */
/*        SCTE01 ( Ticks to ET,            type 1 ) */
/*        SCTY01 ( Return type of SCLK ) */
/*        SCPR01 ( Return partition start and stop times, type 1 ) */

/* $ Examples */

/*     See the entry points. */

/* $ Restrictions */

/*     1)  Efficiency of the conversion routines in this package */
/*         depends on the assumption that the kernel pool is updated */
/*         relatively infrequently. Any modification of kernel pool data, */
/*         including setting watches, will cause entry points of this */
/*         package to re-buffer saved SCLK data. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     J.M. Lynch         (JPL) */
/*     B.V. Semenov       (JPL) */
/*     E.D. Wright        (JPL) */

/* $ Version */

/* -    SPICELIB Version 4.0.0, 01-DEC-2021 (NJB) (JDR) */

/*        Added entry points SCPR01 (return partition data) and */
/*        SCTY01 (indicate whether clock is type 1). */

/*        Updated all entry points to support buffering of data for */
/*        multiple clocks. */

/*        Added error checks for invalid time system codes. */

/*        Edited headers of umbrella routine and all entry points to */
/*        comply with NAIF standard. */

/* -    SPICELIB Version 3.4.0, 09-SEP-2013 (BVS) */

/*        Updated to keep track of the POOL counter and call ZZCVPOOL. */

/* -    SPICELIB Version 3.3.0, 05-MAR-2009 (NJB) */

/*        Bug fix: the entry points of this routine now keep track of */
/*        whether their kernel pool look-ups succeeded. If not, a kernel */
/*        pool lookup is attempted on the next call to any entry point */
/*        of this routine. */

/* -    SPICELIB Version 3.2.0, 17-FEB-2008 (NJB) */

/*        Bug fix: changed maximum value arguments to 1 in */
/*        calls to SCLI01 to fetch NFIELD and DELCDE values. */

/*        Bug fix: spaces between fields are now inserted */
/*        correctly when the output field delimiter is blank. */

/* -    SPICELIB Version 3.1.1, 22-AUG-2006 (EDW) */

/*        Replaced references to LDPOOL with references */
/*        to FURNSH. */

/* -    SPICELIB Version 3.1.0, 24-JAN-2003 (BVS) */

/*        Increased MXCOEF to 10000. */

/* -    SPICELIB Version 3.0.0, 09-MAR-1999 (NJB) */

/*        Added new entry point SCEC01. Removed some extraneous */
/*        C's from column 1; these had been added by a wayward */
/*        preprocessor. */

/*        Removed local variable RNDCLK; entry point SCTE01 no longer */
/*        creates a rounded version of its input argument. */

/*        Updated/fixed various comments here and in entry SCET01. */

/* -    SPICELIB Version 2.1.0, 07-JUL-1996 (NJB) */

/*        Removed declaration, DATA and SAVE statements for unused */
/*        variables NFDMSG and OLDID. */

/* -    SPICELIB Version 2.0.0, 17-APR-1992 (NJB) */

/*        All entry points were updated to handle SCLK kernels that */
/*        map between SCLK and a variety of time systems; formerly */
/*        only TDB was supported. All entry points have had corrections */
/*        and additions made to their headers. Comment section for */
/*        permuted index source lines was added following the header. */

/* -    SPICELIB Version 1.0.0, 03-SEP-1990 (NJB) (JML) */

/* -& */
/* $ Index_Entries */

/*     type_1 spacecraft_clock */

/* -& */
/* $ Revisions */

/* -    SPICELIB Version 3.3.0, 05-MAR-2009 (NJB) */

/*        Bug fix: the entry points of this routine now keep track of */
/*        whether their kernel pool look-ups succeeded. If not, a kernel */
/*        pool lookup is attempted on the next call to any entry point */
/*        of this routine. */

/*        All entry points of this routine look up the same kernel */
/*        variables, and use the saved variable UPDATE to indicate that */
/*        a kernel pool look-up is needed. A look-up failure occurring */
/*        in any entry point will now prevent all entry points from */
/*        relying on stored kernel data. */


/* -    SPICELIB Version 3.2.0, 17-FEB-2008 (NJB) */

/*        Bug fix: changed maximum value arguments to 1 in */
/*        calls to SCLI01 to fetch NFIELD and DELCDE values. */

/*        Bug fix: spaces between fields are now inserted */
/*        correctly when the output field delimiter is blank. */

/*        Unused parameter INITID was removed. */

/* -    SPICELIB Version 3.1.0, 24-JAN-2003 (BVS) */

/*        Increased MXCOEF to 10000. */

/* -    SPICELIB Version 3.0.0, 09-MAR-1999 (NJB) */

/*        Added new entry point SCEC01. Removed some extraneous */
/*        C's from column 1; these had been added by a wayward */
/*        preprocessor. */

/*        Removed local variable RNDCLK; entry point SCTE01 no longer */
/*        creates a rounded version of its input argument. */

/*        Updated/fixed various comments here and in entry SCET01. */

/* -    SPICELIB Version 2.1.0, 07-JUL-1996 (NJB) */

/*        Removed declaration, DATA and SAVE statements for unused */
/*        variables NFDMSG and OLDID. */

/* -    SPICELIB Version 2.0.0, 17-APR-1992 (NJB) */

/*        Entry points SCET01 and SCTE01 were updated to handle a time */
/*        system specification for the `parallel' time system */
/*        in the SCLK kernel. Formerly, the only time system that */
/*        an SCLK kernel could map SCLK to was TDB. Now TDT is */
/*        supported, and the mechanism for allowing other parallel */
/*        time systems is in place. */

/*        To support a new parallel time system, it is necessary */
/*        to */

/*           -- Update SCTE01 so that after the routine converts an input */
/*              tick value to a value in the parallel system, the */
/*              resulting value is converted to TDB. See the current */
/*              treatment of TDT in that routine for an example of how */
/*              this is done. */

/*           -- Update SCET01 so that the input TDB value can be */
/*              converted to a value in the new parallel system when */
/*              required. This converted value is then used as an input */
/*              to the interpolation algorithm performed in SCET01. See */
/*              the current treatment of TDT in that routine for an */
/*              example of how this is done. */

/*           -- Update the parameter MXTSYS in SCLU01 to indicate the */
/*              new number of supported parallel time systems. */

/*           -- Update the SCLK Required Reading to document the */
/*              description of the currently supported parallel time */
/*              systems. */

/*        See the named entry points for further details. */

/*        The kernel pool routines SWPOOL and CVPOOL are now used */
/*        to determine when it is necessary to look up kernel pool */
/*        constants. The variable UPDATE is now used to indicate */
/*        when it is necessary to look up the kernel variables used by */
/*        this suite of routines. All of the entry points SCFM01, */
/*        SCTK01, SCET01, and SCTE01 were affected by this update. */

/*        All of the entry points have had their headers updated to */
/*        discuss the fact that a leapseconds kernel will now need to be */
/*        loaded in order to use SCLK kernels that map between SCLK and */
/*        a parallel time system other than TDB. */

/*        In this routine, a comment section for permuted index */
/*        source lines was added following the header. */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     Local variables */


/*     SCLK pool: */


/*     Other local variables */


/*     Declarations of statement functions */


/*     Saved variables */


/*     There are at least a half dozen distinct items to save. We're */
/*     safer just saving everything. */

/*     Maintenance programming note: the coefficient buffer */
/*     should be saved in any event to prevent memory problems */
/*     on some platforms. */


/*     Initial values */

    /* Parameter adjustments */
    if (parbeg) {
	}
    if (parend) {
	}

    /* Function Body */
    switch(n__) {
	case 1: goto L_sctk01;
	case 2: goto L_scfm01;
	case 3: goto L_scte01;
	case 4: goto L_scet01;
	case 5: goto L_scec01;
	case 6: goto L_scty01;
	case 7: goto L_scpr01;
	}


/*     NEW: Statement functions */


/*     Function COEFFS returns the Ith element of the Jth coefficient */
/*     record for the record set starting at index COFBAS+1 in the */
/*     double precision SCLK data buffer. */

/*     Function PREND returns the Ith partition end time for the */
/*     partition end time set starting at index ENDBAS+1 in the double */
/*     precision SCLK data buffer. */


/*     Function PRSTRT returns the Ith partition start time for the */
/*     partition start time set starting at index STRBAS+1 in the double */
/*     precision SCLK data buffer. */


/*     Function MODULI returns the Ith field modulus for the modulus */
/*     array starting at index MODBAS+1 in the double */
/*     precision SCLK data buffer. */


/*     Function OFFSET returns the Ith field offset for the offset */
/*     array starting at index OFFBAS+1 in the double */
/*     precision SCLK data buffer. */


/*     For later implementation: */

/*     Function CMPWID returns the width in characters required to */
/*     represent as a string the maximum count of the Ith field. */

/*      CMPWID( I ) = INTBUF( OFFCMP + I ) */

/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }
    chkin_("SC01", (ftnlen)4);
    sigerr_("SPICE(BOGUSENTRY)", (ftnlen)17);
    chkout_("SC01", (ftnlen)4);
    return 0;
/* $Procedure SCTK01 ( Convert type 1 SCLK string to ticks ) */

L_sctk01:
/* $ Abstract */

/*     Convert a character representation of a type 1 spacecraft clock */
/*     count to ticks. */

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

/*     CONVERSION */
/*     TIME */

/* $ Declarations */

/*     CHARACTER*(*)         CLKSTR */
/*     DOUBLE PRECISION      TICKS */
/*     INTEGER               SC */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     SC         I   NAIF spacecraft ID code. */
/*     CLKSTR     I   Character representation of a clock count. */
/*     TICKS      O   Number of ticks represented by the clock count. */

/* $ Detailed_Input */

/*     SC       is a NAIF spacecraft identification code. See the */
/*              $Examples section below, and also the NAIF_IDS */
/*              required reading file for a complete list of body ID */
/*              codes. */


/*     CLKSTR   on input is the character representation of a */
/*              spacecraft clock count (SCLK), without a partition */
/*              number. */

/*              Using Galileo as an example, a SCLK string without */
/*              a partition number has the form */

/*                             wwwwwwww:xx:y:z */

/*              where z is a mod-8 counter (values 0-7) which */
/*              increments approximately once every 8 1/3 ms., y is a */
/*              mod-10 counter (values 0-9) which increments once */
/*              every time z turns over, i.e., approximately once every */
/*              66 2/3 ms., xx is a mod-91 (values 0-90) counter */
/*              which increments once every time y turns over, i.e., */
/*              once every 2/3 seconds. wwwwwwww is the Real-Time */
/*              Image Count (RIM), which increments once every time */
/*              xx turns over, i.e., once every 60 2/3 seconds. The */
/*              roll-over expression for the RIM is 16777215, which */
/*              corresponds to approximately 32 years. */

/*              wwwwwwww, xx, y, and z are referred to interchangeably */
/*              as the fields or components of the spacecraft count. */
/*              SCLK components may be separated by any of the */
/*              single character delimiters in the string DELIMS, with */
/*              any number of spaces separating the components and */
/*              the delimiters. The presence of the RIM component */
/*              is required. Successive components may be omitted, and */
/*              in such cases are assumed to represent zero values. */

/*              Values for the individual components may exceed the */
/*              maximum expected values. For instance, '0:0:0:9' is */
/*              an acceptable Galileo clock string, and indicates the */
/*              same time interval as '0:0:1:1'. */

/*              Consecutive delimiters containing no intervening digits */
/*              are treated as if they delimit zero components, except */
/*              in the case of blanks.  Consecutive blanks are treated */
/*              as a single blank. */

/*              Trailing zeros should always be included to match the */
/*              length of the counter.  For example, a Galileo clock */
/*              count of '25684.90' should not be represented as */
/*              '25684.9'. */

/*              Some spacecraft clock components have offset, or */
/*              starting, values different from zero. For example, */
/*              with an offset value of 1, a mod 20 counter would */
/*              cycle from 1 to 20 instead of from 0 to 19. */

/*              See the SCLK required reading for a detailed */
/*              description of the Galileo, Mars Observer, and Voyager */
/*              clock formats. */

/*              See the $Examples section in SCTK01, below. */

/* $ Detailed_Output */

/*     TICKS    is the number of "ticks" corresponding to the input */
/*              spacecraft clock string CLKSTR.  "Ticks" are the units */
/*              in which encoded SCLK strings are represented. */

/*              A typical Galileo SCLK string looks like */

/*                           'wwwwwwww xx y z', */

/*              as described above. Since z is the mod-8 (one tick) */
/*              counter, the number of ticks represented by y is 8*y. */
/*              And since y is the mod-10 counter, the number of ticks */
/*              represented by xx is 10*8*xx. The total number of */
/*              ticks represented by the above string is */

/*                            wwwwwwww( 7280 ) + */
/*                                  xx(   80 ) + */
/*                                   y(    8 ) + */
/*                                   z */

/*              Clock strings for other spacecraft are converted in */
/*              a similar manner. */

/*              See $Examples below. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  This routine assumes that that an SCLK kernel appropriate to */
/*         the spacecraft clock identified by the input argument SC has */
/*         been loaded. If an SCLK kernel has not been loaded, does not */
/*         contain all of the required data, or contains invalid data, an */
/*         error is signaled by a routine in the call tree of this */
/*         routine. The output argument TICKS will not be modified. */

/*         The variables that must be set by the SCLK kernel are: */

/*            -  The number of fields in an (unabridged) SCLK string */
/*            -  The output delimiter code */
/*            -  The parallel time system code */
/*            -  The moduli of the fields of an SCLK string */
/*            -  The offsets for each clock field. */
/*            -  The SCLK coefficients array */
/*            -  The partition start times */
/*            -  The partition end times */

/*     2)  When using SCLK kernels that map SCLK to a time system other */
/*         than ET (also called barycentric dynamical time---`TDB'), it */
/*         is necessary to have a leapseconds kernel loaded at the time */
/*         this routine is called. If a leapseconds kernel is required */
/*         for conversion between SCLK and ET but is not loaded, an error */
/*         is signaled by a routine in the call tree of this routine. The */
/*         output argument TICKS will not be modified. */

/*         The time system that an SCLK kernel maps SCLK to is indicated */
/*         by the variable SCLK_TIME_SYSTEM_nn in the kernel, where nn */
/*         is the negative of the NAIF integer code for the spacecraft. */
/*         The time system used in a kernel is TDB if and only if the */
/*         variable is assigned the value 1. */


/*     3)  If any of the following kernel variables have invalid values, */
/*         the error will be diagnosed by routines called by this */
/*         routine: */

/*            -  The time system code */
/*            -  The number of SCLK coefficients */
/*            -  The number of partition start times */
/*            -  The number of partition end times */
/*            -  The number of fields of a SCLK string */
/*            -  The number of moduli for a SCLK string */

/*         If the number of values for any item read from the kernel */
/*         pool exceeds the maximum allowed value, it is may not be */
/*         possible to diagnose the error correctly, since overwriting */
/*         of memory may occur. This particular type of error is not */
/*         diagnosed by this routine. */


/*     4)  The input argument CLKSTR may be invalid for a variety of */
/*         reasons: */

/*            -- One of the extracted clock components cannot be parsed */
/*               as an integer */

/*            -- CLKSTR contains too many components */

/*            -- the value  of one of the components is less than the */
/*               offset value */

/*         If any of these conditions is detected, the error */
/*         SPICE(INVALIDSCLKSTRING) is signaled. The output argument */
/*         TICKS will not be modified. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine converts a character string representation of a */
/*     spacecraft clock count into the number of ticks represented */
/*     by the clock count. An important distinction between this type */
/*     of conversion and that carried out by SCENCD is that this routine */
/*     treats spacecraft clock times as representations of time */
/*     intervals, not absolute times. */

/*     This routine does not make use of any partition information. */
/*     See SCENCD for details on how to make use of partition numbers. */

/* $ Examples */

/*     1)  Below are some examples illustrating various inputs and the */
/*         resulting outputs for the Galileo spacecraft. */

/*         CLKSTR                TICKS */
/*         ----------------      -------------------- */
/*         '0:0:0:1'             1 */
/*         '0:0:1'               8 */
/*         '0:1'                 80 */
/*         '1'                   7280 */
/*         '1 0 0 0'             7280 */
/*         '1,0,0,0'             7280 */
/*         '1:90'                14480 */
/*         '1:9'                 8000 */
/*         '1:09'                8000 */
/*         '0-0-10'              80   |--  Third component is supposed */
/*         '0-1-0'               80   |    to be a mod-10 count. */
/*         '0/1/0'               Error: '/' is not an accepted delimiter. */
/*         '1: 00 : 0 : 1'       7281 */
/*         '1:::1'               7281 */
/*         '1.1.1.1.1'           Error: Too many components */
/*         '1.1.1.1.'            Error: The last delimiter signals that */
/*                                      a fifth component will follow. */


/*         The following examples are for the Voyager 2 spacecraft. Note */
/*         that the last component of the Voyager clock has an offset */
/*         value of 1. */

/*         CLKSTR                TICKS */
/*         ----------------      -------------------- */
/*         '0.0.001'             0 */
/*         '0:0:002'             1 */
/*         '0:01'                800 */
/*         '1'                   48000 */
/*         '1.0'                 48000 */
/*         '1.0.0'               Error: The 3rd component is never 0. */
/*         '0.0:100'             99 */
/*         '0-60-1'              48000 */
/*         '1-1-1'               48800 */
/*         '1-1-2'               48801 */

/* $ Restrictions */

/*     1)  An SCLK kernel appropriate to the spacecraft clock identified */
/*         by SC must be loaded at the time this routine is called. */

/*     2)  If the SCLK kernel used with this routine does not map SCLK */
/*         directly to barycentric dynamical time, a leapseconds kernel */
/*         must be loaded at the time this routine is called. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     J.M. Lynch         (JPL) */
/*     B.V. Semenov       (JPL) */
/*     R.E. Thurman       (JPL) */

/* $ Version */

/* -    SPICELIB Version 3.0.0, 01-DEC-2021 (NJB) (JDR) */

/*        Updated to support buffering of data for multiple clocks. */
/*        This entry point tracks kernel pool changes but no longer */
/*        sets or uses watches. */

/*        Updated long error message. */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 2.3.0, 09-SEP-2013 (BVS) */

/*        Updated to keep track of the POOL counter and call ZZCVPOOL. */

/* -    SPICELIB Version 2.2.0, 05-MAR-2009 (NJB) */

/*        Bug fix: this routine now keeps track of whether its */
/*        kernel pool look-up succeeded. If not, a kernel pool */
/*        lookup is attempted on the next call to this routine. */

/* -    SPICELIB Version 2.1.0, 09-NOV-2007 (NJB) */

/*        Bug fix: changed maximum value arguments to 1 in */
/*        calls to SCLI01 to fetch NFIELD and DELCDE values. */

/* -    SPICELIB Version 2.0.0, 17-APR-1992 (NJB) */

/*        Header was updated, particularly $Exceptions and $Restrictions */
/*        sections. Kernel pool watch is now set on required kernel */
/*        variables. Comment section for permuted index source lines */
/*        was added following the header. */

/* -    SPICELIB Version 1.0.0, 04-SEP-1990 (NJB) (JML) (RET) */

/* -& */
/* $ Index_Entries */

/*     convert type_1 spacecraft_clock string to ticks */

/* -& */
/* $ Revisions */

/* -    SPICELIB Version 2.1.0, 09-NOV-2007 (NJB) */

/*        Bug fix: changed maximum value arguments to 1 in */
/*        calls to SCLI01 to fetch NFIELD and DELCDE values. */

/* -    SPICELIB Version 2.0.0, 17-APR-1992 (NJB) */

/*        This routine now uses the new kernel pool watch capability */
/*        to determine when it is necessary to look up SCLK variables. */
/*        This method of checking for kernel pool updates replaces the */
/*        previously used once-per-call lookup of the SCLK_KERNEL_ID */
/*        kernel variable. */

/*        The header was updated to discuss the fact that a leapseconds */
/*        kernel will now need to be loaded in order to use SCLK kernels */
/*        that map between SCLK and a parallel time system other than */
/*        TDB. The $Exceptions and $Restrictions sections were affected. */

/*        A comment section for permuted index source lines was added */
/*        following the header. */

/* -& */

/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }
    chkin_("SCTK01", (ftnlen)6);
    if (pass1) {

/*        Initialize local data structures. This routine is error-free. */

	zzscin01_(hdsclk, scpool, clklst, &dpfree, &ifree, &prvsc);
	zzctruin_(polctr);
	pass1 = FALSE_;
    }

/*     Get parameters and data for this clock from the type 1 database. */
/*     Update the database if necessary. */

    zzscup01_(sc, polctr, hdsclk, scpool, clklst, &dpfree, dpbuff, &ifree, 
	    intbuf, scbase, &prvsc, &nfield, &delcde, &timsys, &ncoeff, &
	    npart, &cofbas, &strbas, &endbas, &modbas, &offbas);
    if (failed_()) {

/*        For safety, initialize the database on the next call to */
/*        any entry point of SC01. Setting PASS1 to .TRUE. */
/*        accomplishes this. */

	pass1 = TRUE_;
	chkout_("SCTK01", (ftnlen)6);
	return 0;
    }

/*     If our clock string is blank, we can stop now. */

    if (s_cmp(clkstr, " ", clkstr_len, (ftnlen)1) == 0) {
	pass1 = TRUE_;
	setmsg_("CLKSTR is blank.", (ftnlen)16);
	sigerr_("SPICE(INVALIDSCLKSTRING)", (ftnlen)24);
	chkout_("SCTK01", (ftnlen)6);
	return 0;
    }

/*     Determine how many ticks is each field is worth. */

    cmptks[(i__1 = nfield - 1) < 10 && 0 <= i__1 ? i__1 : s_rnge("cmptks", 
	    i__1, "sc01_", (ftnlen)989)] = 1.;
    for (i__ = nfield - 1; i__ >= 1; --i__) {
	i__3 = i__ + 1;
	cmptks[(i__1 = i__ - 1) < 10 && 0 <= i__1 ? i__1 : s_rnge("cmptks", 
		i__1, "sc01_", (ftnlen)992)] = cmptks[(i__2 = i__) < 10 && 0 
		<= i__2 ? i__2 : s_rnge("cmptks", i__2, "sc01_", (ftnlen)992)]
		 * dpbuff[(i__4 = modbas + i__3 - 1) < 320018 && 0 <= i__4 ? 
		i__4 : s_rnge("dpbuff", i__4, "sc01_", (ftnlen)992)];
    }

/*     Parse the clock components from the input string. There should */
/*     be at most NFIELD of them, but, in order to check for too long */
/*     a clock string, we'll let LPARSM take up to MXNFLD components and */
/*     then test for an error. */

/*     NOTE: LPARSM is error-free. */

    lparsm_(clkstr, ".:-, ", &c__10, &n, cmp, clkstr_len, (ftnlen)5, (ftnlen)
	    30);

/*     If the string has too many fields for the specified spacecraft */
/*     then signal an error. */

    if (n > nfield) {
	pass1 = TRUE_;
	setmsg_("CLKSTR has # fields, which is too many.", (ftnlen)39);
	errint_("#", &n, (ftnlen)1);
	sigerr_("SPICE(INVALIDSCLKSTRING)", (ftnlen)24);
	chkout_("SCTK01", (ftnlen)6);
	return 0;
    }

/*     Convert each of the components into numbers.  Error if any */
/*     of the conversions screw up.  NPARSD doesn't assign a value */
/*     to ' ', so assign the numeric value of the blank components */
/*     to be equal to the offset value. */

    i__1 = n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	if (s_cmp(cmp + ((i__2 = i__ - 1) < 10 && 0 <= i__2 ? i__2 : s_rnge(
		"cmp", i__2, "sc01_", (ftnlen)1029)) * 30, " ", (ftnlen)30, (
		ftnlen)1) == 0) {
	    cmpval[(i__2 = i__ - 1) < 10 && 0 <= i__2 ? i__2 : s_rnge("cmpval"
		    , i__2, "sc01_", (ftnlen)1030)] = dpbuff[(i__3 = offbas + 
		    i__ - 1) < 320018 && 0 <= i__3 ? i__3 : s_rnge("dpbuff", 
		    i__3, "sc01_", (ftnlen)1030)];
	} else {
	    nparsd_(cmp + ((i__2 = i__ - 1) < 10 && 0 <= i__2 ? i__2 : s_rnge(
		    "cmp", i__2, "sc01_", (ftnlen)1032)) * 30, &cmpval[(i__3 =
		     i__ - 1) < 10 && 0 <= i__3 ? i__3 : s_rnge("cmpval", 
		    i__3, "sc01_", (ftnlen)1032)], error, &pntr, (ftnlen)30, (
		    ftnlen)240);
	}
	if (s_cmp(error, " ", (ftnlen)240, (ftnlen)1) != 0) {
	    pass1 = TRUE_;
	    setmsg_("Could not parse SCLK component # from # as a number.", (
		    ftnlen)52);
	    errch_("#", cmp + ((i__2 = i__ - 1) < 10 && 0 <= i__2 ? i__2 : 
		    s_rnge("cmp", i__2, "sc01_", (ftnlen)1042)) * 30, (ftnlen)
		    1, (ftnlen)30);
	    errch_("#", clkstr, (ftnlen)1, clkstr_len);
	    sigerr_("SPICE(INVALIDSCLKSTRING)", (ftnlen)24);
	    chkout_("SCTK01", (ftnlen)6);
	    return 0;
	}

/*        Subtract off the offset value so that we can do base ten */
/*        arithmetic.  Also, if any of the components become negative */
/*        as a result of the subtraction, then that component must */
/*        have been invalid. */

	savcmp = cmpval[(i__2 = i__ - 1) < 10 && 0 <= i__2 ? i__2 : s_rnge(
		"cmpval", i__2, "sc01_", (ftnlen)1056)];
	cmpval[(i__2 = i__ - 1) < 10 && 0 <= i__2 ? i__2 : s_rnge("cmpval", 
		i__2, "sc01_", (ftnlen)1057)] = cmpval[(i__3 = i__ - 1) < 10 
		&& 0 <= i__3 ? i__3 : s_rnge("cmpval", i__3, "sc01_", (ftnlen)
		1057)] - dpbuff[(i__4 = offbas + i__ - 1) < 320018 && 0 <= 
		i__4 ? i__4 : s_rnge("dpbuff", i__4, "sc01_", (ftnlen)1057)];
	if (d_nint(&cmpval[(i__2 = i__ - 1) < 10 && 0 <= i__2 ? i__2 : s_rnge(
		"cmpval", i__2, "sc01_", (ftnlen)1059)]) < 0.) {
	    pass1 = TRUE_;
	    setmsg_("Component number # in the SCLK string, counting left to"
		    " right, is invalid: component # is less than field offse"
		    "t #.", (ftnlen)115);
	    errint_("#", &i__, (ftnlen)1);
	    errdp_("#", &savcmp, (ftnlen)1);
	    errdp_("#", &dpbuff[(i__2 = offbas + i__ - 1) < 320018 && 0 <= 
		    i__2 ? i__2 : s_rnge("dpbuff", i__2, "sc01_", (ftnlen)
		    1068)], (ftnlen)1);
	    sigerr_("SPICE(INVALIDSCLKSTRING)", (ftnlen)24);
	    chkout_("SCTK01", (ftnlen)6);
	    return 0;
	}
    }

/*     Convert to ticks by multiplying the value of each component by */
/*     the number of ticks each component count represents, and then */
/*     add up the results. */

    *ticks = 0.;
    i__1 = n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	*ticks += cmpval[(i__2 = i__ - 1) < 10 && 0 <= i__2 ? i__2 : s_rnge(
		"cmpval", i__2, "sc01_", (ftnlen)1085)] * cmptks[(i__3 = i__ 
		- 1) < 10 && 0 <= i__3 ? i__3 : s_rnge("cmptks", i__3, "sc01_"
		, (ftnlen)1085)];
    }

/*     Keep track of the last spacecraft clock ID encountered. */

    prvsc = *sc;
    chkout_("SCTK01", (ftnlen)6);
    return 0;
/* $Procedure SCFM01 ( Convert ticks to a type 1 SCLK string. ) */

L_scfm01:
/* $ Abstract */

/*     Convert a number of ticks to an equivalent type 1 spacecraft clock */
/*     string. */

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

/*     CONVERSION */
/*     TIME */

/* $ Declarations */

/*     INTEGER               SC */
/*     DOUBLE PRECISION      TICKS */
/*     CHARACTER*(*)         CLKSTR */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     SC         I   NAIF spacecraft identification code. */
/*     TICKS      I   Number of ticks represented by a clock count. */
/*     CLKSTR     O   Character string representation of the clock count. */

/* $ Detailed_Input */

/*     SC       is a NAIF spacecraft identification code. See the */
/*              $Examples section below, and also the KERNEL required */
/*              reading file for a complete list of body ID codes. */


/*     TICKS    is the number of ticks to be converted to a spacecraft */
/*              clock string, where a tick is defined to be */
/*              the smallest time increment expressible by the */
/*              spacecraft clock. */

/*              If TICKS contains a fractional part, the string that */
/*              results is the same as if TICKS had been rounded to */
/*              the nearest whole number. */

/*              See $Examples below. */

/* $ Detailed_Output */

/*     CLKSTR   on output is the character string representation of */
/*              the spacecraft clock count. The returned string has */
/*              the form */

/*                               'wwwwwwww:xx:y:z', */

/*              where the number of components and the width of each */
/*              one are different for each spacecraft. The delimiter */
/*              used is determined by a kernel pool variable and is */
/*              one of the five specified by the parameter DELIMS. */
/*              See $Examples below. */

/*              If CLKSTR is not long enough to accommodate the */
/*              formatted tick value, the result will be truncated on */
/*              the right. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  This routine assumes that that an SCLK kernel appropriate to */
/*         the spacecraft clock identified by the input argument SC has */
/*         been loaded. If an SCLK kernel has not been loaded, does not */
/*         contain all of the required data, or contains invalid data, an */
/*         error is signaled by a routine in the call tree of this */
/*         routine. The output argument CLKSTR will not be modified. */

/*         The variables that must be set by the SCLK kernel are: */

/*            -  The number of fields in an (unabridged) SCLK string */
/*            -  The output delimiter code */
/*            -  The parallel time system code */
/*            -  The moduli of the fields of an SCLK string */
/*            -  The offsets for each clock field. */
/*            -  The SCLK coefficients array */
/*            -  The partition start times */
/*            -  The partition end times */

/*     2)  When using SCLK kernels that map SCLK to a time system other */
/*         than ET (also called barycentric dynamical time---`TDB'), it */
/*         is necessary to have a leapseconds kernel loaded at the time */
/*         this routine is called. If a leapseconds kernel is required */
/*         for conversion between SCLK and ET but is not loaded, an error */
/*         is signaled by a routine in the call tree of this routine. The */
/*         output argument CLKSTR will not be modified. */

/*         The time system that an SCLK kernel maps SCLK to is indicated */
/*         by the variable SCLK_TIME_SYSTEM_nn in the kernel, where nn */
/*         is the negative of the NAIF integer code for the spacecraft. */
/*         The time system used in a kernel is TDB if and only if the */
/*         variable is assigned the value 1. */

/*     3)  If any of the following kernel variables have invalid values, */
/*         the error will be diagnosed by routines called by this */
/*         routine: */

/*            -  The time system code */
/*            -  The number of SCLK coefficients */
/*            -  The number of partition start times */
/*            -  The number of partition end times */
/*            -  The number of fields of a SCLK string */
/*            -  The number of moduli for a SCLK string */

/*         If the number of values for any item read from the kernel */
/*         pool exceeds the maximum allowed value, it is may not be */
/*         possible to diagnose the error correctly, since overwriting */
/*         of memory may occur. This particular type of error is not */
/*         diagnosed by this routine. */

/*     4)  If the input value for TICKS is negative, the error */
/*         SPICE(VALUEOUTOFRANGE) is signaled. The output argument */
/*         CLKSTR will not be modified. */

/*     5)  If the output argument CLKSTR is too short to accommodate */
/*         the output string produced by this routine, the error */
/*         SPICE(SCLKTRUNCATED) is signaled. The output string */
/*         CLKSTR will not be modified. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     The routine determines the values of the components of the */
/*     spacecraft clock count that is equivalent to the number TICKS. */
/*     The information needed to perform this operation, such as the */
/*     number of clock components and their moduli, is provided by */
/*     an SCLK kernel file. Normally, your program should load this */
/*     file during initialization. */

/*     This routine does not make use of any partition information. */
/*     See SCDECD for details on how to make use of partition numbers. */

/* $ Examples */

/*     Below are some examples illustrating various inputs and the */
/*      resulting outputs for the Galileo spacecraft. */

/*         TICKS                 CLKSTR */
/*         ----------------      -------------------- */
/*         -1                    Error: Ticks must be a positive number */
/*         0                     '0:00:0:0' */
/*         1                     '0:00:0:1' */
/*         1.3                   '0:00:0:1' */
/*         1.5                   '0:00:0:2' */
/*         2                     '0:00:0:2' */
/*         7                     '0:00:0:7' */
/*         8                     '0:00:1:0' */
/*         80                    '0:01:0:0' */
/*         88                    '0:01:1:0' */
/*         7279                  '0:90:9:7' */
/*         7280                  '1:00:0:0' */
/*         1234567890            '169583:45:6:2' */


/*     The following examples are for the Voyager 2 spacecraft. */
/*     Note that the third component of the Voyager clock has an */
/*     offset value of one. */

/*         TICKS                 CLKSTR */
/*         ----------------      -------------------- */
/*         -1                    Error: Ticks must be a positive number */
/*         0                     '00000 00 001' */
/*         1                     '00000 00 002' */
/*         1.3                   '00000:00:002' */
/*         1.5                   '00000.00.003' */
/*         2                     '00000-00-003' */
/*         799                   '00000,00,800' */
/*         800                   '00000 01 001' */
/*         47999                 '00000 59 800' */
/*         48000                 '00001 00 001' */
/*         3145727999            '65535 59 800' */

/* $ Restrictions */

/*     1)  An SCLK kernel appropriate to the spacecraft clock identified */
/*         by SC must be loaded at the time this routine is called. */

/*     2)  If the SCLK kernel used with this routine does not map SCLK */
/*         directly to barycentric dynamical time, a leapseconds kernel */
/*         must be loaded at the time this routine is called. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     J.M. Lynch         (JPL) */
/*     B.V. Semenov       (JPL) */
/*     R.E. Thurman       (JPL) */

/* $ Version */

/* -    SPICELIB Version 3.0.0, 01-DEC-2021 (NJB) (JDR) */

/*        Updated to support buffering of data for multiple clocks. */
/*        This entry point tracks kernel pool changes but no longer */
/*        sets or uses watches. */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 2.3.0, 09-SEP-2013 (BVS) */

/*        Updated to keep track of the POOL counter and call ZZCVPOOL. */

/* -    SPICELIB Version 2.2.0, 05-MAR-2009 (NJB) */

/*        Bug fix: this routine now keeps track of whether its */
/*        kernel pool look-up succeeded. If not, a kernel pool */
/*        lookup is attempted on the next call to this routine. */

/* -    SPICELIB Version 2.1.0, 17-FEB-2008 (NJB) */

/*        Bug fix: changed maximum value arguments to 1 in */
/*        calls to SCLI01 to fetch NFIELD and DELCDE values. */

/*        Bug fix: spaces between fields are now inserted */
/*        correctly when the output field delimiter is blank. */

/* -    SPICELIB Version 2.0.1, 18-JUL-1996 (NJB) */

/*        Misspelling in header fixed. */

/* -    SPICELIB Version 2.0.0, 17-APR-1992 (NJB) */

/*        Error is now signaled if truncation of output string occurs. */
/*        Header was updated, particularly $Exceptions and $Restrictions */
/*        sections. Kernel pool watch is now set on required kernel */
/*        variables. Comment section for permuted index source lines */
/*        was added following the header. */

/* -    SPICELIB Version 1.0.0, 06-SEP-1990 (NJB) (JML) (RET) */

/* -& */
/* $ Index_Entries */

/*     convert ticks to a type_1 spacecraft_clock string */

/* -& */
/* $ Revisions */

/* -    SPICELIB Version 2.1.0, 17-FEB-2008 (NJB) */

/*        Bug fix: changed maximum value arguments to 1 in */
/*        calls to SCLI01 to fetch NFIELD and DELCDE values. */

/*        Bug fix: spaces between fields are now inserted */
/*        correctly when the output field delimiter is blank. */

/* -    SPICELIB Version 2.0.0, 17-APR-1992 (NJB) (WLT) */

/*        An error is now signaled if truncation of output string */
/*        occurs. */

/*        The header was updated to discuss exception handling when */
/*        the output string is truncated. The header was also expanded */
/*        to discuss the fact that a leapseconds kernel will now need to */
/*        be loaded in order to use SCLK kernels that map between SCLK */
/*        and a parallel time system other than TDB. The $Exceptions */
/*        and $Restrictions sections were affected. */

/*        This routine now uses the new kernel pool watch capability */
/*        to determine when it is necessary to look up SCLK variables. */
/*        This method of checking for kernel pool updates replaces the */
/*        previously used once-per-call lookup of the SCLK_KERNEL_ID */
/*        kernel variable. */

/*        A comment section for permuted index source lines was added */
/*        following the header. */

/* -& */

/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }
    chkin_("SCFM01", (ftnlen)6);

/*     On the first pass through the subroutine, or if the spacecraft */
/*     clock ID changes, we will set watches on the SCLK kernel */
/*     variables for the current clock. */

    if (pass1) {

/*        Initialize local data structures. This routine is error-free. */

	zzscin01_(hdsclk, scpool, clklst, &dpfree, &ifree, &prvsc);
	zzctruin_(polctr);
	pass1 = FALSE_;
    }

/*     Get parameters and data for this clock from the type 1 database. */
/*     Update the database if necessary. */

    zzscup01_(sc, polctr, hdsclk, scpool, clklst, &dpfree, dpbuff, &ifree, 
	    intbuf, scbase, &prvsc, &nfield, &delcde, &timsys, &ncoeff, &
	    npart, &cofbas, &strbas, &endbas, &modbas, &offbas);
    if (failed_()) {

/*        For safety, initialize the database on the next call to */
/*        any entry point of SC01. Setting PASS1 to .TRUE. */
/*        accomplishes this. */

	pass1 = TRUE_;
	chkout_("SCFM01", (ftnlen)6);
	return 0;
    }

/*     Determine how many ticks each field is worth. */

    cmptks[(i__1 = nfield - 1) < 10 && 0 <= i__1 ? i__1 : s_rnge("cmptks", 
	    i__1, "sc01_", (ftnlen)1467)] = 1.;
    for (i__ = nfield - 1; i__ >= 1; --i__) {
	i__3 = i__ + 1;
	cmptks[(i__1 = i__ - 1) < 10 && 0 <= i__1 ? i__1 : s_rnge("cmptks", 
		i__1, "sc01_", (ftnlen)1470)] = cmptks[(i__2 = i__) < 10 && 0 
		<= i__2 ? i__2 : s_rnge("cmptks", i__2, "sc01_", (ftnlen)1470)
		] * dpbuff[(i__4 = modbas + i__3 - 1) < 320018 && 0 <= i__4 ? 
		i__4 : s_rnge("dpbuff", i__4, "sc01_", (ftnlen)1470)];
    }

/*     Determine the width of each field. */

    i__1 = nfield;
    for (i__ = 1; i__ <= i__1; ++i__) {
	maxwid = dpbuff[(i__2 = modbas + i__ - 1) < 320018 && 0 <= i__2 ? 
		i__2 : s_rnge("dpbuff", i__2, "sc01_", (ftnlen)1478)] + 
		dpbuff[(i__3 = offbas + i__ - 1) < 320018 && 0 <= i__3 ? i__3 
		: s_rnge("dpbuff", i__3, "sc01_", (ftnlen)1478)] - 1.;
	d__1 = maxwid + .5;
	cmpwid[(i__2 = i__ - 1) < 10 && 0 <= i__2 ? i__2 : s_rnge("cmpwid", 
		i__2, "sc01_", (ftnlen)1480)] = (integer) d_lg10(&d__1) + 1;
    }

/*     Check whether the output string is long enough to contain the */
/*     string we're about to assemble.  We need room for (NFIELD - 1) */
/*     delimiters as well as for the numeric fields. */

    needed = nfield - 1 + sumai_(cmpwid, &nfield);
    if (i_len(clkstr, clkstr_len) < needed) {
	pass1 = TRUE_;
	setmsg_("Output argument has declared length #; required length is #"
		". Input tick value was #.", (ftnlen)84);
	i__1 = i_len(clkstr, clkstr_len);
	errint_("#", &i__1, (ftnlen)1);
	errint_("#", &needed, (ftnlen)1);
	errdp_("#", ticks, (ftnlen)1);
	sigerr_("SPICE(SCLKTRUNCATED)", (ftnlen)20);
	chkout_("SCFM01", (ftnlen)6);
	return 0;
    }

/*     Need to check that TICKS is a positive number. */

    if (d_nint(ticks) < 0.) {
	pass1 = TRUE_;
	setmsg_("Negative value for SCLK ticks: #", (ftnlen)32);
	errdp_("#", ticks, (ftnlen)1);
	sigerr_("SPICE(VALUEOUTOFRANGE)", (ftnlen)22);
	chkout_("SCFM01", (ftnlen)6);
	return 0;
    }

/*     Determine the value of each of the components. This is done by */
/*     successively dividing by the number of ticks each component value */
/*     is worth. */

    rem = d_nint(ticks);
    i__1 = nfield - 1;
    for (i__ = 1; i__ <= i__1; ++i__) {
	d__1 = rem / cmptks[(i__3 = i__ - 1) < 10 && 0 <= i__3 ? i__3 : 
		s_rnge("cmptks", i__3, "sc01_", (ftnlen)1530)];
	cmpval[(i__2 = i__ - 1) < 10 && 0 <= i__2 ? i__2 : s_rnge("cmpval", 
		i__2, "sc01_", (ftnlen)1530)] = d_int(&d__1) + dpbuff[(i__4 = 
		offbas + i__ - 1) < 320018 && 0 <= i__4 ? i__4 : s_rnge("dpb"
		"uff", i__4, "sc01_", (ftnlen)1530)];
	rem = d_mod(&rem, &cmptks[(i__2 = i__ - 1) < 10 && 0 <= i__2 ? i__2 : 
		s_rnge("cmptks", i__2, "sc01_", (ftnlen)1531)]);
    }
    cmpval[(i__1 = nfield - 1) < 10 && 0 <= i__1 ? i__1 : s_rnge("cmpval", 
	    i__1, "sc01_", (ftnlen)1535)] = rem + dpbuff[(i__2 = offbas + 
	    nfield - 1) < 320018 && 0 <= i__2 ? i__2 : s_rnge("dpbuff", i__2, 
	    "sc01_", (ftnlen)1535)];

/*     Convert the values of each component from double precision */
/*     numbers to character strings. */

    i__1 = nfield;
    for (i__ = 1; i__ <= i__1; ++i__) {

/*        NOTE: DPSTRF is error-free. */

	dpstrf_(&cmpval[(i__2 = i__ - 1) < 10 && 0 <= i__2 ? i__2 : s_rnge(
		"cmpval", i__2, "sc01_", (ftnlen)1545)], &c__30, "F", dpchar, 
		(ftnlen)1, (ftnlen)30);
	end = i_indx(dpchar, ".", (ftnlen)30, (ftnlen)1) - 1;
	length[(i__2 = i__ - 1) < 10 && 0 <= i__2 ? i__2 : s_rnge("length", 
		i__2, "sc01_", (ftnlen)1548)] = end - 1;
	s_copy(cmp + ((i__2 = i__ - 1) < 10 && 0 <= i__2 ? i__2 : s_rnge(
		"cmp", i__2, "sc01_", (ftnlen)1549)) * 30, dpchar + 1, (
		ftnlen)30, end - 1);
    }

/*     Pad on the left with zeros if necessary. */

    i__1 = nfield;
    for (i__ = 1; i__ <= i__1; ++i__) {
	pad = cmpwid[(i__2 = i__ - 1) < 10 && 0 <= i__2 ? i__2 : s_rnge("cmp"
		"wid", i__2, "sc01_", (ftnlen)1558)] - length[(i__3 = i__ - 1) 
		< 10 && 0 <= i__3 ? i__3 : s_rnge("length", i__3, "sc01_", (
		ftnlen)1558)];
	if (pad > 0) {
	    i__2 = pad;
	    for (j = 1; j <= i__2; ++j) {
		prefix_("0", &c__0, cmp + ((i__3 = i__ - 1) < 10 && 0 <= i__3 
			? i__3 : s_rnge("cmp", i__3, "sc01_", (ftnlen)1563)) *
			 30, (ftnlen)1, (ftnlen)30);
	    }
	}
    }

/*     Construct the clock string with a delimiter separating */
/*     each field. */

    s_copy(clkstr, cmp, clkstr_len, (ftnlen)30);
    i__1 = nfield;
    for (i__ = 2; i__ <= i__1; ++i__) {
	if (*(unsigned char *)&del[(i__2 = delcde - 1) < 5 && 0 <= i__2 ? 
		i__2 : s_rnge("del", i__2, "sc01_", (ftnlen)1578)] != ' ') {
	    prefix_(del + ((i__2 = delcde - 1) < 5 && 0 <= i__2 ? i__2 : 
		    s_rnge("del", i__2, "sc01_", (ftnlen)1580)), &c__0, cmp + 
		    ((i__3 = i__ - 1) < 10 && 0 <= i__3 ? i__3 : s_rnge("cmp",
		     i__3, "sc01_", (ftnlen)1580)) * 30, (ftnlen)1, (ftnlen)
		    30);
	    suffix_(cmp + ((i__2 = i__ - 1) < 10 && 0 <= i__2 ? i__2 : s_rnge(
		    "cmp", i__2, "sc01_", (ftnlen)1581)) * 30, &c__0, clkstr, 
		    (ftnlen)30, clkstr_len);
	} else {
	    suffix_(cmp + ((i__2 = i__ - 1) < 10 && 0 <= i__2 ? i__2 : s_rnge(
		    "cmp", i__2, "sc01_", (ftnlen)1583)) * 30, &c__1, clkstr, 
		    (ftnlen)30, clkstr_len);
	}
    }

/*     Keep track of the last spacecraft clock ID encountered. */

    prvsc = *sc;
    chkout_("SCFM01", (ftnlen)6);
    return 0;
/* $Procedure SCTE01 ( Ticks to ET, type 01 ) */

L_scte01:
/* $ Abstract */

/*     Convert encoded type 1 spacecraft clock (`ticks') to ephemeris */
/*     seconds past J2000 (ET). */

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
/*     TIME */

/* $ Keywords */

/*     CONVERSION */
/*     TIME */

/* $ Declarations */

/*     INTEGER               SC */
/*     DOUBLE PRECISION      SCLKDP */
/*     DOUBLE PRECISION      ET */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     SC         I   NAIF spacecraft ID code. */
/*     SCLKDP     I   Type 1 SCLK, encoded as ticks since clock start. */
/*     ET         I   Ephemeris time, seconds past J2000. */

/* $ Detailed_Input */

/*     SC       is a NAIF ID code for a spacecraft, one of whose */
/*              clock values is represented by SCLKDP. */

/*     SCLKDP   is an encoded type 1 spacecraft clock value */
/*              produced by the routine SCENCD. SCLKDP is a */
/*              count of ticks since spacecraft clock start: */
/*              partition information IS included in the encoded */
/*              value. */

/* $ Detailed_Output */

/*     ET       is the ephemeris time, seconds past J2000, that */
/*              corresponds to SCLKDP. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  This routine assumes that that an SCLK kernel appropriate to */
/*         the spacecraft clock identified by the input argument SC has */
/*         been loaded. If an SCLK kernel has not been loaded, does not */
/*         contain all of the required data, or contains invalid data, an */
/*         error is signaled by a routine in the call tree of this */
/*         routine. The output argument ET will not be modified. */

/*         The variables that must be set by the SCLK kernel are: */

/*            -  The number of fields in an (unabridged) SCLK string */
/*            -  The output delimiter code */
/*            -  The parallel time system code */
/*            -  The moduli of the fields of an SCLK string */
/*            -  The offsets for each clock field. */
/*            -  The SCLK coefficients array */
/*            -  The partition start times */
/*            -  The partition end times */

/*     2)  When using SCLK kernels that map SCLK to a time system other */
/*         than ET (also called barycentric dynamical time---`TDB'), it */
/*         is necessary to have a leapseconds kernel loaded at the time */
/*         this routine is called. If a leapseconds kernel is required */
/*         for conversion between SCLK and ET but is not loaded, an error */
/*         is signaled by a routine in the call tree of this routine. The */
/*         output argument ET will not be modified. */

/*         The time system that an SCLK kernel maps SCLK to is indicated */
/*         by the variable SCLK_TIME_SYSTEM_nn in the kernel, where nn */
/*         is the negative of the NAIF integer code for the spacecraft. */
/*         The time system used in a kernel is TDB if and only if the */
/*         variable is assigned the value 1. */


/*     3)  If any of the following kernel variables have invalid values, */
/*         the error will be diagnosed by routines called by this */
/*         routine: */

/*            -  The number of SCLK coefficients */
/*            -  The number of partition start times */
/*            -  The number of partition end times */
/*            -  The number of fields of a SCLK string */
/*            -  The number of moduli for a SCLK string */

/*         If the number of values for any item read from the kernel */
/*         pool exceeds the maximum allowed value, it is may not be */
/*         possible to diagnose the error correctly, since overwriting */
/*         of memory may occur. This particular type of error is not */
/*         diagnosed by this routine. */

/*     4)  If the time system code is not recognized, the error */
/*         SPICE(VALUEOUTOFRANGE) is signaled. */

/*     5)  If the input SCLK value SCLKDP is out of range, the error */
/*         SPICE(VALUEOUTOFRANGE) is signaled. The output argument ET */
/*         will not be modified. */

/*     6)  If the SCLK rate used to interpolate SCLK values is */
/*         nonpositive, the error SPICE(VALUEOUTOFRANGE) is signaled. */
/*         The output argument SCLKDP will not be modified. */

/*     7)  If the partition times or SCLK coefficients themselves */
/*         are invalid, this routine will almost certainly give */
/*         incorrect results. This routine cannot diagnose errors */
/*         in the partition times or SCLK coefficients, except possibly */
/*         by crashing. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     SCTE01 is not usually called by routines external to SPICELIB. */
/*     The conversion routine SCT2E converts any type of encoded */
/*     spacecraft clock value produced by SCENCD to ephemeris seconds */
/*     past J2000.  SCT2E is the preferred user interface routine */
/*     because its interface specification does not refer to spacecraft */
/*     clock types. However, direct use of SCTE01 by user routines is */
/*     not prohibited. */

/* $ Examples */

/*     1)  Convert an encoded type 1 SCLK value to ET: */

/*         During program initialization, load the leapseconds and SCLK */
/*         kernels. We will assume that these files are named */
/*         "LEAPSECONDS.KER" and "SCLK.KER".  You must substitute the */
/*         actual names of these files in your code. */

/*            CALL CLPOOL */
/*            CALL FURNSH ( 'LEAPSECONDS.KER' ) */
/*            CALL FURNSH ( 'SCLK.KER'        ) */

/*         If SCLKDP is an encoded spacecraft clock value, if SC */
/*         is the NAIF integer code for the spacecraft whose */
/*         SCLK <--> ET mapping is defined by the data in SCLK.KER, */
/*         then the call */

/*            CALL SCTE01 ( SC, SCLKDP, ET ) */

/*         will return the ET value corresponding to SCLKDP. */

/*         For example, if SC is -77, indicating the Galileo spacecraft, */
/*         and if a Galileo SCLK kernel is loaded, then if SCLKDP */
/*         is set to */

/*            7.2800000000000E+05 */

/*         the call */

/*            CALL SCTE01 ( SC, SCLKDP, ET ) */

/*         returns ET as */

/*            -3.2286984854565E+08 */

/*         on a VAX 11/780 running VMS 5.3, Fortran 5.5. */

/* $ Restrictions */

/*     1)  An SCLK kernel appropriate to the spacecraft clock identified */
/*         by SC must be loaded at the time this routine is called. */

/*     2)  If the SCLK kernel used with this routine does not map SCLK */
/*         directly to barycentric dynamical time, a leapseconds kernel */
/*         must be loaded at the time this routine is called. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     B.V. Semenov       (JPL) */

/* $ Version */

/* -    SPICELIB Version 4.0.0, 01-DEC-2021 (NJB) (JDR) */

/*        Updated to support buffering of data for multiple clocks. */
/*        This entry point tracks kernel pool changes but no longer */
/*        sets or uses watches. */

/*        A check for invalid time system code was added. */

/*        A check for nonpositive clock rate was added. */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 3.3.0, 09-SEP-2013 (BVS) */

/*        Updated to keep track of the POOL counter and call ZZCVPOOL. */

/* -    SPICELIB Version 3.2.0, 05-MAR-2009 (NJB) */

/*        Bug fix: this routine now keeps track of whether its */
/*        kernel pool look-up succeeded. If not, a kernel pool */
/*        lookup is attempted on the next call to this routine. */

/* -    SPICELIB Version 3.1.0, 09-NOV-2007 (NJB) */

/*        Bug fix: changed maximum value arguments to 1 in */
/*        calls to SCLI01 to fetch NFIELD and DELCDE values. */

/* -    SPICELIB Version 3.0.0, 06-JAN-1998 (NJB) */

/*        Removed local variable RNDCLK; this entry point no longer */
/*        creates a rounded version of its input argument. Use of */
/*        ANINT to round coefficients has been discontinued. */

/* -    SPICELIB Version 2.0.0, 17-APR-1992 (NJB) */

/*        This routine was updated to handle SCLK kernels that use */
/*        TDT as their `parallel' time system. Header was updated, */
/*        particularly $Exceptions and $Restrictions. Watch is now */
/*        set on required kernel variables. Comment section for */
/*        permuted index source lines was added following the header. */

/* -    SPICELIB Version 1.0.0, 21-AUG-1990 (NJB) */

/* -& */
/* $ Index_Entries */

/*     type_1 ticks to ephemeris time */

/* -& */
/* $ Revisions */

/* -    SPICELIB Version 3.1.0, 09-NOV-2007 (NJB) */

/*        Bug fix: changed maximum value arguments to 1 in */
/*        calls to SCLI01 to fetch NFIELD and DELCDE values. */

/* -    SPICELIB Version 3.0.0, 06-JAN-1998 (NJB) */

/*        Removed local variable RNDCLK; this entry point no longer */
/*        creates a rounded version of its input argument. Use of */
/*        ANINT to round coefficients has been discontinued. */

/* -    SPICELIB Version 2.0.0, 17-APR-1992 (NJB) (WLT) */

/*        This routine was updated to handle a time system specification */
/*        for the `parallel' time system used in the SCLK kernel. */

/*        Specific changes include: */

/*           -- The time system code is looked up along with the */
/*              other SCLK specification parameters. */

/*           -- The time value arrived at by interpolation of the */
/*              SCLK-to-parallel time mapping is converted to TDB */
/*              if the parallel time system is TDT. */

/*        The header was expanded to discuss the fact that a leapseconds */
/*        kernel will now need to be loaded in order to use SCLK kernels */
/*        that map between SCLK and a parallel time system other than */
/*        TDB. The $Exceptions and $Restrictions sections were affected. */

/*        This routine now uses the new kernel pool watch capability */
/*        to determine when it is necessary to look up SCLK variables. */
/*        This method of checking for kernel pool updates replaces the */
/*        previously used once-per-call lookup of the SCLK_KERNEL_ID */
/*        kernel variable. */

/*        A comment section for permuted index source lines was added */
/*        following the header. */

/* -& */

/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }
    chkin_("SCTE01", (ftnlen)6);
    if (pass1) {

/*        Initialize local data structures. This routine is error-free. */

	zzscin01_(hdsclk, scpool, clklst, &dpfree, &ifree, &prvsc);
	zzctruin_(polctr);
	pass1 = FALSE_;
    }

/*     Get parameters and data for this clock from the type 1 database. */
/*     Update the database if necessary. */

    zzscup01_(sc, polctr, hdsclk, scpool, clklst, &dpfree, dpbuff, &ifree, 
	    intbuf, scbase, &prvsc, &nfield, &delcde, &timsys, &ncoeff, &
	    npart, &cofbas, &strbas, &endbas, &modbas, &offbas);
    if (failed_()) {
	pass1 = TRUE_;
	chkout_("SCTE01", (ftnlen)6);
	return 0;
    }

/*     To check whether SCLKDP is in range, we must find the end time */
/*     of the last partition, in total ticks since spacecraft clock */
/*     start. */

    mxtick = 0.;
    i__1 = npart;
    for (i__ = 1; i__ <= i__1; ++i__) {
	d__1 = dpbuff[(i__2 = endbas + i__ - 1) < 320018 && 0 <= i__2 ? i__2 :
		 s_rnge("dpbuff", i__2, "sc01_", (ftnlen)1960)] - dpbuff[(
		i__3 = strbas + i__ - 1) < 320018 && 0 <= i__3 ? i__3 : 
		s_rnge("dpbuff", i__3, "sc01_", (ftnlen)1960)] + mxtick;
	mxtick = d_nint(&d__1);
    }

/*     We now check that SCLKDP is in range.  COEFFS(1,1) and */
/*     MXTICK are, respectively, the first and last absolute */
/*     tick values of the clock. */

    if (*sclkdp < dpbuff[(i__1 = cofbas + c__1 + (c__1 - 1) * 3 - 1) < 320018 
	    && 0 <= i__1 ? i__1 : s_rnge("dpbuff", i__1, "sc01_", (ftnlen)
	    1968)] || *sclkdp > mxtick) {
	pass1 = TRUE_;
	setmsg_(bvlmsg, (ftnlen)320);
	errch_("#", "SCLKDP", (ftnlen)1, (ftnlen)6);
	errdp_("#", sclkdp, (ftnlen)1);
	sigerr_("SPICE(VALUEOUTOFRANGE)", (ftnlen)22);
	chkout_("SCTE01", (ftnlen)6);
	return 0;
    }

/*     Ok, if we made it this far, we can actually interpret the tick */
/*     value.  But by this time, we're not in very good mood. */


/*     Find the tick value in COEFFS closest to the rounded input tick */
/*     value.  The tick values in COEFFS are monotone increasing, so we */
/*     can do a binary search to find index of the greatest tick value */
/*     in the coefficient array that is less than or equal to SCLKDP. */

/*     There are two cases: */

/*        1) SCLKDP is bounded by the least and greatest SCLK */
/*           coefficients in the array.  In this case, we must search */
/*           the array for a consecutive pair of records whose SCLK */
/*           values bound SCLKDP. */

/*        2) SCLKDP is greater than or equal to all of the SCLK */
/*           coefficients.  In that case, we don't need to search:  the */
/*           last SCLK value in the array is the one we want. */

    i__1 = ncoeff / 3;
    if (*sclkdp < dpbuff[(i__2 = cofbas + c__1 + (i__1 - 1) * 3 - 1) < 320018 
	    && 0 <= i__2 ? i__2 : s_rnge("dpbuff", i__2, "sc01_", (ftnlen)
	    2005)]) {
	lower = 1;
	upper = ncoeff / 3;

/*        In the following loop, we maintain an invariant: */

/*           COEFFS( 1, LOWER )  <  SCLKDP  <  COEFFS( 1, UPPER ) */
/*                               - */

/*        At each step, we decrease the distance between LOWER and */
/*        UPPER, while keeping the above statement true.  The loop */
/*        terminates when LOWER = UPPER - 1. */

/*        Note that we start out with if LOWER < UPPER, since we've */
/*        already made sure that the invariant expression above is true. */

	while(lower < upper - 1) {
	    middle = (lower + upper) / 2;
	    if (*sclkdp < dpbuff[(i__1 = cofbas + c__1 + (middle - 1) * 3 - 1)
		     < 320018 && 0 <= i__1 ? i__1 : s_rnge("dpbuff", i__1, 
		    "sc01_", (ftnlen)2027)]) {
		upper = middle;
	    } else {
		lower = middle;
	    }
	}

/*        We've got SCLKDP trapped between two tick values that are */
/*        `adjacent' in the list: */

/*           COEFFS ( 1, LOWER )  and */
/*           COEFFS ( 1, UPPER ) */

/*        since the second value must be greater than the first.  So */

/*           COEFFS( 1, LOWER ) */

/*        is the last tick value in the coefficients array less than or */
/*        equal to SCLKDP. */

    } else {

/*        SCLKDP is greater than or equal to all of the SCLK */
/*        coefficients in the coefficients array. */

	lower = ncoeff / 3;
    }

/*     Now we evaluate a linear polynomial to find the time value that */
/*     corresponds to SCLKDP.  The coefficients of the polynomial are */
/*     the time and rate (in units of seconds per tick) that correspond */
/*     to the tick value */

/*        COEFFS( 1, LOWER ) */

/*     We call these coefficients CONST and RATE.  The rates in the */
/*     coefficients array are in units of seconds per most significant */
/*     SCLK count, so we use the conversion factor TIKMSC to change the */
/*     rate to seconds per tick. */

    tikmsc = 1.;
    for (i__ = nfield; i__ >= 2; --i__) {
	tikmsc *= dpbuff[(i__1 = modbas + i__ - 1) < 320018 && 0 <= i__1 ? 
		i__1 : s_rnge("dpbuff", i__1, "sc01_", (ftnlen)2075)];
    }
    if (dpbuff[(i__1 = cofbas + c__3 + (lower - 1) * 3 - 1) < 320018 && 0 <= 
	    i__1 ? i__1 : s_rnge("dpbuff", i__1, "sc01_", (ftnlen)2078)] <= 
	    0.) {
	pass1 = TRUE_;
	setmsg_("Invalid SCLK rate.", (ftnlen)18);
	sigerr_("SPICE(VALUEOUTOFRANGE)", (ftnlen)22);
	chkout_("SCTE01", (ftnlen)6);
	return 0;
    }
    tikdif = *sclkdp - dpbuff[(i__1 = cofbas + c__1 + (lower - 1) * 3 - 1) < 
	    320018 && 0 <= i__1 ? i__1 : s_rnge("dpbuff", i__1, "sc01_", (
	    ftnlen)2089)];
    const__ = dpbuff[(i__1 = cofbas + c__2 + (lower - 1) * 3 - 1) < 320018 && 
	    0 <= i__1 ? i__1 : s_rnge("dpbuff", i__1, "sc01_", (ftnlen)2090)];
    rate = dpbuff[(i__1 = cofbas + c__3 + (lower - 1) * 3 - 1) < 320018 && 0 
	    <= i__1 ? i__1 : s_rnge("dpbuff", i__1, "sc01_", (ftnlen)2091)] / 
	    tikmsc;
    partim = const__ + rate * tikdif;

/*     Convert the parallel time to TDB, if the system is not TDB. */
/*     We don't need to check the validity of TIMSYS, because SCLI01 */
/*     already made this check. */

    if (timsys == 1) {
	*et = partim;
    } else if (timsys == 2) {
	*et = unitim_(&partim, "TDT", "TDB", (ftnlen)3, (ftnlen)3);
	if (failed_()) {
	    chkout_("SCTE01", (ftnlen)6);
	    return 0;
	}
    } else {

/*        This code should be unreachable. It's present for safety. */

	pass1 = TRUE_;
	setmsg_("Invalid time system code # was found for SCLK #.", (ftnlen)
		48);
	errint_("#", &timsys, (ftnlen)1);
	errint_("#", sc, (ftnlen)1);
	sigerr_("SPICE(VALUEOUTOFRANGE)", (ftnlen)22);
	chkout_("SCTE01", (ftnlen)6);
	return 0;
    }

/*     Keep track of the last spacecraft clock ID encountered. */

    prvsc = *sc;
    chkout_("SCTE01", (ftnlen)6);
    return 0;
/* $Procedure SCET01 ( ET to discrete ticks, type 1 ) */

L_scet01:
/* $ Abstract */

/*     Convert ephemeris seconds past J2000 (ET) to discrete encoded */
/*     type 1 spacecraft clock (`ticks'). */

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
/*     TIME */

/* $ Keywords */

/*     CONVERSION */
/*     TIME */

/* $ Declarations */

/*     INTEGER               SC */
/*     DOUBLE PRECISION      ET */
/*     DOUBLE PRECISION      SCLKDP */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     SC         I   NAIF spacecraft ID code. */
/*     ET         I   Ephemeris time, seconds past J2000. */
/*     SCLKDP     O   Type 1 SCLK, encoded as ticks since clock start. */

/* $ Detailed_Input */

/*     SC       is a NAIF ID code for a spacecraft, one of whose */
/*              clock values is represented by SCLKDP. */

/*     ET       is an ephemeris time, specified in seconds past */
/*              J2000, whose equivalent encoded SCLK value is */
/*              desired. */

/* $ Detailed_Output */

/*     SCLKDP   is the encoded type 1 spacecraft clock value */
/*              that corresponds to ET. The value is obtained */
/*              by mapping ET, using the piecewise linear mapping */
/*              defined by the SCLK kernel, to a value that may */
/*              have a non-zero fractional part, and then */
/*              rounding this value to the nearest double precision */
/*              whole number. */

/*              SCLKDP represents total time since spacecraft */
/*              clock start and hence does reflect partition */
/*              information. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  This routine assumes that that an SCLK kernel appropriate to */
/*         the spacecraft clock identified by the input argument SC has */
/*         been loaded. If an SCLK kernel has not been loaded, does not */
/*         contain all of the required data, or contains invalid data, an */
/*         error is signaled by a routine in the call tree of this */
/*         routine. The output argument SCLKDP will not be modified. */

/*         The variables that must be set by the SCLK kernel are: */

/*            -  The number of fields in an (unabridged) SCLK string */
/*            -  The output delimiter code */
/*            -  The parallel time system code */
/*            -  The moduli of the fields of an SCLK string */
/*            -  The offsets for each clock field. */
/*            -  The SCLK coefficients array */
/*            -  The partition start times */
/*            -  The partition end times */

/*     2)  When using SCLK kernels that map SCLK to a time system other */
/*         than ET (also called barycentric dynamical time---`TDB'), it */
/*         is necessary to have a leapseconds kernel loaded at the time */
/*         this routine is called. If a leapseconds kernel is required */
/*         for conversion between SCLK and ET but is not loaded, an error */
/*         is signaled by a routine in the call tree of this routine. The */
/*         output argument SCLKDP will not be modified. */

/*         The time system that an SCLK kernel maps SCLK to is indicated */
/*         by the variable SCLK_TIME_SYSTEM_nn in the kernel, where nn */
/*         is the negative of the NAIF integer code for the spacecraft. */
/*         The time system used in a kernel is TDB if and only if the */
/*         variable is assigned the value 1. */

/*     3)  If any of the following kernel variables have invalid values, */
/*         the error will be diagnosed by routines called by this */
/*         routine: */

/*            -  The number of SCLK coefficients */
/*            -  The number of partition start times */
/*            -  The number of partition end times */
/*            -  The number of fields of a SCLK string */
/*            -  The number of moduli for a SCLK string */

/*         If the number of values for any item read from the kernel */
/*         pool exceeds the maximum allowed value, it is may not be */
/*         possible to diagnose the error correctly, since overwriting */
/*         of memory may occur. This particular type of error is not */
/*         diagnosed by this routine. */

/*     4)  If the time system code is not recognized, the error */
/*         SPICE(VALUEOUTOFRANGE) is signaled. */

/*     5)  If the input ephemeris time value ET is out of range, the */
/*         error SPICE(VALUEOUTOFRANGE) is signaled. The output argument */
/*         SCLKDP will not be modified. */

/*     6)  If the SCLK rate used to interpolate SCLK values is */
/*         nonpositive, the error SPICE(VALUEOUTOFRANGE) is signaled. */
/*         The output argument SCLKDP will not be modified. */

/*     7)  If the partition times or SCLK coefficients themselves */
/*         are invalid, this routine will almost certainly give */
/*         incorrect results. This routine cannot diagnose errors */
/*         in the partition times or SCLK coefficients, except possibly */
/*         by crashing. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     Normally, the newer entry point SCEC01 (ET to continuous ticks, */
/*     type 1) should be used in place of this routine. */

/*     SCET01 is not usually called by routines external to SPICELIB. */
/*     The conversion routine SCE2T converts ephemeris seconds past J2000 */
/*     to any type of discrete, encoded type 1 spacecraft clock value. */
/*     For conversion to continuous, encoded SCLK, SCE2C is the preferred */
/*     user interface routine because its interface specification does */
/*     not refer to spacecraft clock types. For conversion to discrete, */
/*     encoded SCLK, SCE2T is the preferred interface routine. */

/*     However, direct use of SCET01 by user routines is not prohibited. */

/* $ Examples */

/*     1)  Converting ET to encoded type 1 SCLK: */

/*         During program initialization, load the leapseconds and SCLK */
/*         kernels. We will assume that these files are named */
/*         "LEAPSECONDS.KER" and "SCLK.KER".  You must substitute the */
/*         actual names of these files in your code. */

/*            CALL CLPOOL */
/*            CALL FURNSH ( 'LEAPSECONDS.KER' ) */
/*            CALL FURNSH ( 'SCLK.KER'        ) */

/*         If SC is -77, indicating the Galileo spacecraft, and */
/*         ET is set to */

/*            -3.2286984854565E+08 */

/*         then the call */

/*            CALL SCET01 ( SC, ET, SCLKDP ) */

/*         returns SCLKDP as */

/*            7.2800000000000E+05 */

/*         on a VAX 11/780 running VMS 5.3, Fortran 5.5. Note that */
/*         the result should be the same (except for the output format) */
/*         on most computers, since the result is a double precision */
/*         whole number. */

/* $ Restrictions */

/*     1)  An SCLK kernel appropriate to the spacecraft clock identified */
/*         by SC must be loaded at the time this routine is called. */

/*     2)  If the SCLK kernel used with this routine does not map SCLK */
/*         directly to barycentric dynamical time, a leapseconds kernel */
/*         must be loaded at the time this routine is called. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     B.V. Semenov       (JPL) */
/*     E.D. Wright        (JPL) */

/* $ Version */

/* -    SPICELIB Version 3.0.0, 01-DEC-2021 (NJB) (JDR) */

/*        Updated to support buffering of data for multiple clocks. */
/*        This entry point tracks kernel pool changes but no longer */
/*        sets or uses watches. */

/*        A check for invalid time system code was added. */

/*        Edited the header to comply with NAIF standard. */

/*        Corrected description of invalid clock rate exception. */

/* -    SPICELIB Version 2.3.0, 09-SEP-2013 (BVS) */

/*        Updated to keep track of the POOL counter and call ZZCVPOOL. */

/* -    SPICELIB Version 2.2.0, 05-MAR-2009 (NJB) */

/*        Bug fix: this routine now keeps track of whether its */
/*        kernel pool look-up succeeded. If not, a kernel pool */
/*        lookup is attempted on the next call to this routine. */

/* -    SPICELIB Version 2.1.0, 09-NOV-2007 (NJB) */

/*        Bug fix: changed maximum value arguments to 1 in */
/*        calls to SCLI01 to fetch NFIELD and DELCDE values. */

/* -    SPICELIB Version 2.0.3, 22-AUG-2006 (EDW) */

/*        Replaced references to LDPOOL with references */
/*        to FURNSH. */

/* -    SPICELIB Version 2.0.2, 09-MAR-1999 (NJB) */

/*        Comments were updated; references to SCE2C and SCEC01 were */
/*        added. */

/* -    SPICELIB Version 2.0.1, 18-JUL-1996 (NJB) */

/*        Typo in comment fixed. */

/* -    SPICELIB Version 2.0.0, 17-APR-1992 (NJB) */

/*        This routine was updated to handle SCLK kernels that use */
/*        TDT as their `parallel' time system. Header was updated, */
/*        particularly $Exceptions and $Restrictions. Watch is now */
/*        set on required kernel variables. Comment section for */
/*        permuted index source lines was added following the header. */

/* -    SPICELIB Version 1.0.0, 04-SEP-1990 (NJB) */

/* -& */
/* $ Index_Entries */

/*     ephemeris time to type_1 ticks */

/* -& */
/* $ Revisions */

/* -    SPICELIB Version 2.1.0, 09-NOV-2007 (NJB) */

/*        Bug fix: changed maximum value arguments to 1 in */
/*        calls to SCLI01 to fetch NFIELD and DELCDE values. */

/* -    SPICELIB Version 2.0.0, 17-APR-1992 (NJB) */

/*        This routine was updated to handle a time system specification */
/*        for the `parallel' time system used in the SCLK kernel. */

/*        Specific changes include: */

/*           -- The time system code is looked up along with the */
/*              other SCLK specification parameters. */

/*           -- The input TDB value is converted, if necessary, to the */
/*              time system used in the parallel-time-to-SCLK mapping */
/*              defined by the current SCLK coefficients for the */
/*              specified spacecraft clock. This conversion is performed */
/*              prior to determination by interpolation of the */
/*              corresponding encoded SCLK value. */

/*        The header was expanded to discuss the fact that a leapseconds */
/*        kernel will now need to be loaded in order to use SCLK kernels */
/*        that map between SCLK and a parallel time system other than */
/*        TDB. The $Exceptions and $Restrictions sections were affected. */

/*        This routine now uses the new kernel pool watch capability */
/*        to determine when it is necessary to look up SCLK variables. */
/*        This method of checking for kernel pool updates replaces the */
/*        previously used once-per-call lookup of the SCLK_KERNEL_ID */
/*        kernel variable. */

/*        A comment section for permuted index source lines was added */
/*        following the header. */

/* -& */

/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }
    chkin_("SCET01", (ftnlen)6);
    if (pass1) {

/*        Initialize local data structures. This routine is error-free. */

	zzscin01_(hdsclk, scpool, clklst, &dpfree, &ifree, &prvsc);
	zzctruin_(polctr);
	pass1 = FALSE_;
    }

/*     Get parameters and data for this clock from the type 1 database. */
/*     Update the database if necessary. */

    zzscup01_(sc, polctr, hdsclk, scpool, clklst, &dpfree, dpbuff, &ifree, 
	    intbuf, scbase, &prvsc, &nfield, &delcde, &timsys, &ncoeff, &
	    npart, &cofbas, &strbas, &endbas, &modbas, &offbas);
    if (failed_()) {
	pass1 = TRUE_;
	chkout_("SCET01", (ftnlen)6);
	return 0;
    }

/*     Convert the input TDB time to the parallel time system, if the */
/*     parallel system is not TDB. */

/*     We don't need to check the validity of TIMSYS, because SCLI01 */
/*     already made this check. */

    if (timsys == 1) {
	partim = *et;
    } else if (timsys == 2) {
	partim = unitim_(et, "TDB", "TDT", (ftnlen)3, (ftnlen)3);
	if (failed_()) {
	    chkout_("SCET01", (ftnlen)6);
	    return 0;
	}
    } else {

/*        This code should be unreachable. It's present for safety. */

	pass1 = TRUE_;
	setmsg_("Invalid time system code # was found for SCLK #.", (ftnlen)
		48);
	errint_("#", &timsys, (ftnlen)1);
	errint_("#", sc, (ftnlen)1);
	sigerr_("SPICE(VALUEOUTOFRANGE)", (ftnlen)22);
	chkout_("SCET01", (ftnlen)6);
	return 0;
    }

/*     We'd like to ascertain whether PARTIM is between the minimum */
/*     time value in the coefficients array and the end time */
/*     corresponding to the number of ticks since spacecraft clock */
/*     start at the end of the last partition. */

/*     Checking the time value is a special case; we'll convert the time */
/*     value to ticks, and then check whether the resulting value is */
/*     less than the total number of ticks since spacecraft clock start */
/*     at the end of the last partition.  So, this check is performed */
/*     at the end of the routine. */

/*     Find the time value in COEFFS closest to the input time value. */
/*     The time values are ordered, so we can do a binary search for the */
/*     closest one.  When the search is done, we will have found the */
/*     index of the greatest time value in the coefficient array that */
/*     is less than or equal to PARTIM. */


/*     There are three cases: */

/*        1) PARTIM is less than the least time coefficient in the array. */
/*           In this case, we'll use the first coefficient set in the */
/*           kernel to extrapolate from.  We don't automatically treat */
/*           this case as an error because PARTIM could round up to the */
/*           minimum tick value when converted to ticks. */

/*        2) PARTIM is bounded by the least and greatest time */
/*           coefficients in the array.  In this case, we must search */
/*           the array for a consecutive pair of records whose time */
/*           values bound PARTIM. */

/*        3) PARTIM is greater than or equal to all of the time */
/*           coefficients.  In that case, we don't need to search:  the */
/*           last time value in the array is the one we want. */


    if (partim < dpbuff[(i__1 = cofbas + c__2 + (c__1 - 1) * 3 - 1) < 320018 
	    && 0 <= i__1 ? i__1 : s_rnge("dpbuff", i__1, "sc01_", (ftnlen)
	    2575)]) {

/*        The coefficient set to use for extrapolation is the first. */

	lower = 1;
    } else /* if(complicated condition) */ {
	i__1 = ncoeff / 3;
	if (partim < dpbuff[(i__2 = cofbas + c__2 + (i__1 - 1) * 3 - 1) < 
		320018 && 0 <= i__2 ? i__2 : s_rnge("dpbuff", i__2, "sc01_", (
		ftnlen)2582)]) {

/*        In the following loop, we maintain an invariant: */

/*           COEFFS( 2, LOWER )  <   PARTIM   <   COEFFS( 2, UPPER ) */
/*                               - */

/*        At each step, we decrease the distance between LOWER and */
/*        UPPER, while keeping the above statement true.  The loop */
/*        terminates when LOWER = UPPER - 1. */

/*        Note that we start out with if LOWER < UPPER, since we've */
/*        already made sure that the invariant expression above is true. */

	    lower = 1;
	    upper = ncoeff / 3;
	    while(lower < upper - 1) {
		middle = (lower + upper) / 2;
		if (partim < dpbuff[(i__1 = cofbas + c__2 + (middle - 1) * 3 
			- 1) < 320018 && 0 <= i__1 ? i__1 : s_rnge("dpbuff", 
			i__1, "sc01_", (ftnlen)2604)]) {
		    upper = middle;
		} else {
		    lower = middle;
		}
	    }

/*        We've got PARTIM trapped between two time values that are */
/*        `adjacent' in the list: */

/*           COEFFS ( 2, LOWER )  and */
/*           COEFFS ( 2, UPPER ) */

/*        since the second value must be greater than the first.  So */

/*           COEFFS( 2, LOWER ) */

/*        is the last time value in the coefficients array less than or */
/*        equal to PARTIM. */

	} else {

/*        PARTIM is greater than or equal to all of the time values in */
/*        the coefficients array. */

	    lower = ncoeff / 3;
	}
    }

/*     Now we evaluate a linear polynomial to find the tick value that */
/*     corresponds to PARTIM.  The coefficients of the polynomial are */
/*     the tick value and rate (in units of ticks per second) that */
/*     correspond to the time value */

/*        COEFFS( 2, LOWER ) */

/*     We call these coefficients CONST and RATE.  The rates in the */
/*     coefficients array are in units of seconds per most significant */
/*     clock count, so we use the conversion factor TIKMSC (`ticks per */
/*     most significant count') to change the rate to seconds per tick. */

/*     One other thing:  SCLKDP should be an integral number of ticks. */
/*     We use the generic `nearest whole number' function ANINT to */
/*     ensure this. */

    timdif = partim - dpbuff[(i__1 = cofbas + c__2 + (lower - 1) * 3 - 1) < 
	    320018 && 0 <= i__1 ? i__1 : s_rnge("dpbuff", i__1, "sc01_", (
	    ftnlen)2653)];
    const__ = dpbuff[(i__1 = cofbas + c__1 + (lower - 1) * 3 - 1) < 320018 && 
	    0 <= i__1 ? i__1 : s_rnge("dpbuff", i__1, "sc01_", (ftnlen)2654)];
    if (dpbuff[(i__1 = cofbas + c__3 + (lower - 1) * 3 - 1) < 320018 && 0 <= 
	    i__1 ? i__1 : s_rnge("dpbuff", i__1, "sc01_", (ftnlen)2656)] <= 
	    0.) {
	pass1 = TRUE_;
	setmsg_("Invalid SCLK rate.", (ftnlen)18);
	sigerr_("SPICE(VALUEOUTOFRANGE)", (ftnlen)22);
	chkout_("SCET01", (ftnlen)6);
	return 0;
    }
    tikmsc = 1.;
    for (i__ = nfield; i__ >= 2; --i__) {
	tikmsc *= dpbuff[(i__1 = modbas + i__ - 1) < 320018 && 0 <= i__1 ? 
		i__1 : s_rnge("dpbuff", i__1, "sc01_", (ftnlen)2671)];
    }
    rate = 1. / (dpbuff[(i__1 = cofbas + c__3 + (lower - 1) * 3 - 1) < 320018 
	    && 0 <= i__1 ? i__1 : s_rnge("dpbuff", i__1, "sc01_", (ftnlen)
	    2674)] / tikmsc);
    d__1 = const__ + rate * timdif;
    *sclkdp = d_nint(&d__1);

/*     Now, we'll see whether the SCLK value we've found is meaningful. */
/*     If it's too large, that's because the input PARTIM was beyond the */
/*     maximum value we can handle.  To check whether PARTIM is in */
/*     range, we must find the end time of the last partition, in total */
/*     ticks since spacecraft clock start. */

    d__1 = dpbuff[(i__1 = endbas + c__1 - 1) < 320018 && 0 <= i__1 ? i__1 : 
	    s_rnge("dpbuff", i__1, "sc01_", (ftnlen)2685)] - dpbuff[(i__2 = 
	    strbas + c__1 - 1) < 320018 && 0 <= i__2 ? i__2 : s_rnge("dpbuff",
	     i__2, "sc01_", (ftnlen)2685)];
    mxtick = d_nint(&d__1);
    i__1 = npart;
    for (i__ = 2; i__ <= i__1; ++i__) {
	d__1 = dpbuff[(i__2 = endbas + i__ - 1) < 320018 && 0 <= i__2 ? i__2 :
		 s_rnge("dpbuff", i__2, "sc01_", (ftnlen)2688)] - dpbuff[(
		i__3 = strbas + i__ - 1) < 320018 && 0 <= i__3 ? i__3 : 
		s_rnge("dpbuff", i__3, "sc01_", (ftnlen)2688)] + mxtick;
	mxtick = d_nint(&d__1);
    }

/*     Make sure that ET does not precede the ET corresponding to */
/*     the clock's minimum tick value or exceed the ET corresponding to */
/*     the clock's maximum tick value.  We'll do the comparison */
/*     using the tick value that ET mapped to and the minimum and */
/*     maximum tick values of the spacecraft clock. */

/*     Convert SCLKDP and COEFFS(1,1) to whole numbers, so that */
/*     direct comparisons without tolerances are possible. */

    *sclkdp = d_nint(sclkdp);
    dval = d_nint(&dpbuff[(i__1 = cofbas + c__1 + (c__1 - 1) * 3 - 1) < 
	    320018 && 0 <= i__1 ? i__1 : s_rnge("dpbuff", i__1, "sc01_", (
	    ftnlen)2702)]);
    if (*sclkdp < dval || *sclkdp > mxtick) {
	pass1 = TRUE_;
	setmsg_(bvlmsg, (ftnlen)320);
	errch_("#", "ET", (ftnlen)1, (ftnlen)2);
	errdp_("#", et, (ftnlen)1);
	sigerr_("SPICE(VALUEOUTOFRANGE)", (ftnlen)22);
	chkout_("SCET01", (ftnlen)6);
	return 0;
    }

/*     Keep track of the last spacecraft clock ID encountered. */

    prvsc = *sc;
    chkout_("SCET01", (ftnlen)6);
    return 0;
/* $Procedure SCEC01 ( ET to continuous ticks, type 1 ) */

L_scec01:
/* $ Abstract */

/*     Convert ephemeris seconds past J2000 (ET) to continuous encoded */
/*     type 1 spacecraft clock (`ticks').  The output value need not be */
/*     integral. */

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
/*     TIME */

/* $ Keywords */

/*     CONVERSION */
/*     TIME */

/* $ Declarations */

/*     INTEGER               SC */
/*     DOUBLE PRECISION      ET */
/*     DOUBLE PRECISION      SCLKDP */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     SC         I   NAIF spacecraft ID code. */
/*     ET         I   Ephemeris time, seconds past J2000. */
/*     SCLKDP     O   Type 1 SCLK, encoded as continuous ticks since */
/*                    clock start. */

/* $ Detailed_Input */

/*     SC       is a NAIF ID code for a spacecraft, one of whose */
/*              clock values is represented by SCLKDP. */

/*     ET       is an ephemeris time, specified in seconds past */
/*              J2000, whose equivalent encoded SCLK value is */
/*              desired. */

/* $ Detailed_Output */

/*     SCLKDP   is the continuous encoded type 1 spacecraft clock */
/*              value corresponding to ET. The value is obtained */
/*              by mapping ET, using the piecewise linear mapping */
/*              defined by the SCLK kernel, to a value that may */
/*              have a non-zero fractional part. Unlike the output */
/*              of SCET01, SCLKDP is not rounded by this routine. */

/*              SCLKDP represents total time since spacecraft */
/*              clock start and hence does reflect partition */
/*              information. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  This routine assumes that that an SCLK kernel appropriate to */
/*         the spacecraft clock identified by the input argument SC has */
/*         been loaded. If an SCLK kernel has not been loaded, does not */
/*         contain all of the required data, or contains invalid data, an */
/*         error is signaled by a routine in the call tree of this */
/*         routine. The output argument SCLKDP will not be modified. */

/*         The variables that must be set by the SCLK kernel are: */

/*            -  The number of fields in an (unabridged) SCLK string */
/*            -  The output delimiter code */
/*            -  The parallel time system code */
/*            -  The moduli of the fields of an SCLK string */
/*            -  The offsets for each clock field. */
/*            -  The SCLK coefficients array */
/*            -  The partition start times */
/*            -  The partition end times */

/*     2)  When using SCLK kernels that map SCLK to a time system other */
/*         than ET (also called barycentric dynamical time---`TDB'), it */
/*         is necessary to have a leapseconds kernel loaded at the time */
/*         this routine is called. If a leapseconds kernel is required */
/*         for conversion between SCLK and ET but is not loaded, an error */
/*         is signaled by a routine in the call tree of this routine. The */
/*         output argument SCLKDP will not be modified. */

/*         The time system that an SCLK kernel maps SCLK to is indicated */
/*         by the variable SCLK_TIME_SYSTEM_nn in the kernel, where nn */
/*         is the negative of the NAIF integer code for the spacecraft. */
/*         The time system used in a kernel is TDB if and only if the */
/*         variable is assigned the value 1. */

/*     3)  If any of the following kernel variables have invalid values, */
/*         the error will be diagnosed by routines called by this */
/*         routine: */

/*            -  The number of SCLK coefficients */
/*            -  The number of partition start times */
/*            -  The number of partition end times */
/*            -  The number of fields of a SCLK string */
/*            -  The number of moduli for a SCLK string */

/*         If the number of values for any item read from the kernel */
/*         pool exceeds the maximum allowed value, it is may not be */
/*         possible to diagnose the error correctly, since overwriting */
/*         of memory may occur. This particular type of error is not */
/*         diagnosed by this routine. */

/*     4)  If the time system code is not recognized, the error */
/*         SPICE(VALUEOUTOFRANGE) is signaled. */

/*     5)  If the input ephemeris time value ET is out of range, the */
/*         error SPICE(VALUEOUTOFRANGE) is signaled. The output argument */
/*         SCLKDP will not be modified. */

/*     6)  If the SCLK rate used to interpolate SCLK values is */
/*         nonpositive, the error SPICE(VALUEOUTOFRANGE) is signaled. */
/*         The output argument SCLKDP will not be modified. */

/*     7)  If the partition times or SCLK coefficients themselves */
/*         are invalid, this routine will almost certainly give */
/*         incorrect results. This routine cannot diagnose errors */
/*         in the partition times or SCLK coefficients, except possibly */
/*         by crashing. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     SCEC01 is not usually called by routines external to SPICELIB. */
/*     The conversion routine SCE2C converts ephemeris seconds */
/*     past J2000 to any type of encoded spacecraft clock value. */
/*     SCE2C is the preferred user interface routine because its */
/*     interface specification does not refer to spacecraft clock types. */
/*     However, direct use of SCEC01 by user routines is not prohibited. */

/* $ Examples */

/*     1)  Converting ET to encoded type 1 SCLK: */

/*         During program initialization, load the leapseconds and SCLK */
/*         kernels. We will assume that these files are named */
/*         "LEAPSECONDS.KER" and "SCLK.KER".  You must substitute the */
/*         actual names of these files in your code. */

/*            CALL CLPOOL */
/*            CALL FURNSH ( 'LEAPSECONDS.KER' ) */
/*            CALL FURNSH ( 'SCLK.KER'        ) */

/*         If SC is -77, indicating the Galileo spacecraft, and */
/*         ET is set to */

/*            -27848635.8149248 */

/*         then the call */

/*            CALL SCEC01 ( SC, ET, SCLKDP ) */

/*         returns SCLKDP as */

/*            35425287435.8554 */

/*         on a NeXT workstation running NEXTSTEP 3.3. */

/* $ Restrictions */

/*     1)  An SCLK kernel appropriate to the spacecraft clock identified */
/*         by SC must be loaded at the time this routine is called. */

/*     2)  If the SCLK kernel used with this routine does not map SCLK */
/*         directly to barycentric dynamical time, a leapseconds kernel */
/*         must be loaded at the time this routine is called. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     B.V. Semenov       (JPL) */

/* $ Version */

/* -    SPICELIB Version 2.0.0, 01-DEC-2021 (NJB) (JDR) */

/*        Updated to support buffering of data for multiple clocks. */
/*        This entry point tracks kernel pool changes but no longer */
/*        sets or uses watches. */

/*        A check for invalid time system code was added. */

/*        Edited the header to comply with NAIF standard. */

/*        Corrected description of invalid clock rate exception. */

/* -    SPICELIB Version 1.4.0, 09-SEP-2013 (BVS) */

/*        Updated to keep track of the POOL counter and call ZZCVPOOL. */

/* -    SPICELIB Version 1.3.0, 05-MAR-2009 (NJB) */

/*        Bug fix: this routine now keeps track of whether its */
/*        kernel pool look-up succeeded. If not, a kernel pool */
/*        lookup is attempted on the next call to this routine. */

/* -    SPICELIB Version 1.2.0, 09-NOV-2007 (NJB) */

/*        Bug fix: this routine now keeps track of whether its */
/*        kernel pool look-up succeeded. If not, a kernel pool */
/*        lookup is attempted on the next call to this routine. */

/* -    SPICELIB Version 1.1.0, 09-NOV-2007 (NJB) */

/*        Bug fix: changed maximum value arguments to 1 in */
/*        calls to SCLI01 to fetch NFIELD and DELCDE values. */

/* -    SPICELIB Version 1.0.0, 13-FEB-1999 (NJB) */

/* -& */
/* $ Index_Entries */

/*     ephemeris time to continuous type_1 ticks */

/* -& */
/* $ Revisions */

/* -    SPICELIB Version 1.1.0, 09-NOV-2007 (NJB) */

/*        Bug fix: changed maximum value arguments to 1 in */
/*        calls to SCLI01 to fetch NFIELD and DELCDE values. */

/* -& */

/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }
    chkin_("SCEC01", (ftnlen)6);
    if (pass1) {

/*        Initialize local data structures. This routine is error-free. */

	zzscin01_(hdsclk, scpool, clklst, &dpfree, &ifree, &prvsc);
	zzctruin_(polctr);
	pass1 = FALSE_;
    }

/*     Get parameters and data for this clock from the type 1 database. */
/*     Update the database if necessary. */

    zzscup01_(sc, polctr, hdsclk, scpool, clklst, &dpfree, dpbuff, &ifree, 
	    intbuf, scbase, &prvsc, &nfield, &delcde, &timsys, &ncoeff, &
	    npart, &cofbas, &strbas, &endbas, &modbas, &offbas);
    if (failed_()) {
	pass1 = TRUE_;
	chkout_("SCEC01", (ftnlen)6);
	return 0;
    }

/*     Convert the input TDB time to the parallel time system, if the */
/*     parallel system is not TDB. */

/*     We don't need to check the validity of TIMSYS, because SCLI01 */
/*     already made this check. */

    if (timsys == 1) {
	partim = *et;
    } else if (timsys == 2) {
	partim = unitim_(et, "TDB", "TDT", (ftnlen)3, (ftnlen)3);
	if (failed_()) {
	    chkout_("SCEC01", (ftnlen)6);
	    return 0;
	}
    } else {

/*        This code should be unreachable. It's present for safety. */

	pass1 = TRUE_;
	setmsg_("Invalid time system code # was found for SCLK #.", (ftnlen)
		48);
	errint_("#", &timsys, (ftnlen)1);
	errint_("#", sc, (ftnlen)1);
	sigerr_("SPICE(VALUEOUTOFRANGE)", (ftnlen)22);
	chkout_("SCEC01", (ftnlen)6);
	return 0;
    }

/*     We'd like to ascertain whether PARTIM is between the minimum */
/*     time value in the coefficients array and the end time */
/*     corresponding to the number of ticks since spacecraft clock */
/*     start at the end of the last partition. */

/*     Checking the time value is a special case; we'll convert the time */
/*     value to ticks, and then check whether the resulting value is */
/*     less than the total number of ticks since spacecraft clock start */
/*     at the end of the last partition.  So, this check is performed */
/*     at the end of the routine. */

/*     Find the time value in COEFFS closest to the input time value. */
/*     The time values are ordered, so we can do a binary search for the */
/*     closest one.  When the search is done, we will have found the */
/*     index of the greatest time value in the coefficient array that */
/*     is less than or equal to PARTIM. */


/*     There are two cases: */

/*        1) PARTIM is bounded by the least and greatest time */
/*           coefficients in the array.  In this case, we must search */
/*           the array for a consecutive pair of records whose time */
/*           values bound PARTIM. */

/*        2) PARTIM is greater than or equal to all of the time */
/*           coefficients.  In that case, we don't need to search:  the */
/*           last time value in the array is the one we want. */


    if (partim < dpbuff[(i__1 = cofbas + c__2 + (c__1 - 1) * 3 - 1) < 320018 
	    && 0 <= i__1 ? i__1 : s_rnge("dpbuff", i__1, "sc01_", (ftnlen)
	    3104)]) {

/*        PARTIM precedes the coverage of the kernel. */

	pass1 = TRUE_;
	setmsg_(bvlmsg, (ftnlen)320);
	errch_("#", "ET", (ftnlen)1, (ftnlen)2);
	errdp_("#", et, (ftnlen)1);
	sigerr_("SPICE(VALUEOUTOFRANGE)", (ftnlen)22);
	chkout_("SCEC01", (ftnlen)6);
	return 0;
    } else /* if(complicated condition) */ {
	i__1 = ncoeff / 3;
	if (partim < dpbuff[(i__2 = cofbas + c__2 + (i__1 - 1) * 3 - 1) < 
		320018 && 0 <= i__2 ? i__2 : s_rnge("dpbuff", i__2, "sc01_", (
		ftnlen)3118)]) {

/*        In the following loop, we maintain an invariant: */

/*           COEFFS( 2, LOWER )  <   PARTIM   <   COEFFS( 2, UPPER ) */
/*                               - */

/*        At each step, we decrease the distance between LOWER and */
/*        UPPER, while keeping the above statement true.  The loop */
/*        terminates when LOWER = UPPER - 1. */

/*        Note that we start out with if LOWER < UPPER, since we've */
/*        already made sure that the invariant expression above is true. */

	    lower = 1;
	    upper = ncoeff / 3;
	    while(lower < upper - 1) {
		middle = (lower + upper) / 2;
		if (partim < dpbuff[(i__1 = cofbas + c__2 + (middle - 1) * 3 
			- 1) < 320018 && 0 <= i__1 ? i__1 : s_rnge("dpbuff", 
			i__1, "sc01_", (ftnlen)3140)]) {
		    upper = middle;
		} else {
		    lower = middle;
		}
	    }

/*        We've got PARTIM trapped between two time values that are */
/*        `adjacent' in the list: */

/*           COEFFS ( 2, LOWER )  and */
/*           COEFFS ( 2, UPPER ) */

/*        since the second value must be greater than the first.  So */

/*           COEFFS( 2, LOWER ) */

/*        is the last time value in the coefficients array less than or */
/*        equal to PARTIM. */

	} else {

/*        PARTIM is greater than or equal to all of the time values in */
/*        the coefficients array. */

	    lower = ncoeff / 3;
	}
    }

/*     Now we evaluate a linear polynomial to find the tick value that */
/*     corresponds to PARTIM.  The coefficients of the polynomial are */
/*     the tick value and rate (in units of ticks per second) that */
/*     correspond to the time value */

/*        COEFFS( 2, LOWER ) */

/*     We call these coefficients CONST and RATE.  The rates in the */
/*     coefficients array are in units of seconds per most significant */
/*     clock count, so we use the conversion factor TIKMSC (`ticks per */
/*     most significant count') to change the rate to seconds per tick. */

    timdif = partim - dpbuff[(i__1 = cofbas + c__2 + (lower - 1) * 3 - 1) < 
	    320018 && 0 <= i__1 ? i__1 : s_rnge("dpbuff", i__1, "sc01_", (
	    ftnlen)3185)];
    const__ = dpbuff[(i__1 = cofbas + c__1 + (lower - 1) * 3 - 1) < 320018 && 
	    0 <= i__1 ? i__1 : s_rnge("dpbuff", i__1, "sc01_", (ftnlen)3186)];
    if (dpbuff[(i__1 = cofbas + c__3 + (lower - 1) * 3 - 1) < 320018 && 0 <= 
	    i__1 ? i__1 : s_rnge("dpbuff", i__1, "sc01_", (ftnlen)3188)] <= 
	    0.) {
	pass1 = TRUE_;
	setmsg_("Invalid SCLK rate #.", (ftnlen)20);
	errdp_("#", &dpbuff[(i__1 = cofbas + c__3 + (lower - 1) * 3 - 1) < 
		320018 && 0 <= i__1 ? i__1 : s_rnge("dpbuff", i__1, "sc01_", (
		ftnlen)3193)], (ftnlen)1);
	sigerr_("SPICE(VALUEOUTOFRANGE)", (ftnlen)22);
	chkout_("SCEC01", (ftnlen)6);
	return 0;
    }
    tikmsc = 1.;
    for (i__ = nfield; i__ >= 2; --i__) {
	tikmsc *= dpbuff[(i__1 = modbas + i__ - 1) < 320018 && 0 <= i__1 ? 
		i__1 : s_rnge("dpbuff", i__1, "sc01_", (ftnlen)3204)];
    }
    rate = 1. / (dpbuff[(i__1 = cofbas + c__3 + (lower - 1) * 3 - 1) < 320018 
	    && 0 <= i__1 ? i__1 : s_rnge("dpbuff", i__1, "sc01_", (ftnlen)
	    3207)] / tikmsc);
    *sclkdp = const__ + rate * timdif;

/*     Now, we'll see whether the SCLK value we've found is meaningful. */
/*     If it's too large, that's because the input PARTIM was beyond the */
/*     maximum value we can handle.  To check whether PARTIM is in */
/*     range, we must find the end time of the last partition, in total */
/*     ticks since spacecraft clock start. */

    d__1 = dpbuff[(i__1 = endbas + c__1 - 1) < 320018 && 0 <= i__1 ? i__1 : 
	    s_rnge("dpbuff", i__1, "sc01_", (ftnlen)3218)] - dpbuff[(i__2 = 
	    strbas + c__1 - 1) < 320018 && 0 <= i__2 ? i__2 : s_rnge("dpbuff",
	     i__2, "sc01_", (ftnlen)3218)];
    mxtick = d_nint(&d__1);
    i__1 = npart;
    for (i__ = 2; i__ <= i__1; ++i__) {
	d__1 = dpbuff[(i__2 = endbas + i__ - 1) < 320018 && 0 <= i__2 ? i__2 :
		 s_rnge("dpbuff", i__2, "sc01_", (ftnlen)3221)] - dpbuff[(
		i__3 = strbas + i__ - 1) < 320018 && 0 <= i__3 ? i__3 : 
		s_rnge("dpbuff", i__3, "sc01_", (ftnlen)3221)] + mxtick;
	mxtick = d_nint(&d__1);
    }

/*     Make sure that ET does not exceed the ET corresponding to */
/*     the clock's maximum tick value.  We'll do the comparison */
/*     using the tick value that ET mapped to and the maximum tick */
/*     value of the spacecraft clock. */

    if (*sclkdp > mxtick) {
	pass1 = TRUE_;
	setmsg_(bvlmsg, (ftnlen)320);
	errch_("#", "ET", (ftnlen)1, (ftnlen)2);
	errdp_("#", et, (ftnlen)1);
	sigerr_("SPICE(VALUEOUTOFRANGE)", (ftnlen)22);
	chkout_("SCEC01", (ftnlen)6);
	return 0;
    }

/*     Keep track of the last spacecraft clock ID encountered. */

    prvsc = *sc;
    chkout_("SCEC01", (ftnlen)6);
    return 0;
/* $Procedure SCTY01 ( SCLK type, using type 1 SCLK database ) */

L_scty01:
/* $ Abstract */

/*     Return the type of a specified clock. The clock need not be */
/*     type 1. */

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
/*     TIME */

/* $ Keywords */

/*     CONVERSION */
/*     TIME */

/* $ Declarations */

/*     INTEGER               SC */
/*     INTEGER               CLKTYP */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     SC         I   NAIF spacecraft clock ID code. */
/*     CLKTYP     O   NAIF spacecraft clock type. */

/* $ Detailed_Input */

/*     SC       is a NAIF ID code for a spacecraft clock. */

/* $ Detailed_Output */

/*     CLKTYP   is the integer spacecraft clock type. The type */
/*              need not be 1. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If a kernel variable identifying requested SCLK type is not */
/*         found, the error SPICE(KERNELVARNOTFOUND) is signaled. */

/*     2)  If an error occurs in the course of updating the type 1 SCLK */
/*         database, the error is signaled by a routine in the call tree */
/*         of this routine. */

/*     3)  It is not an error to request the type of a clock that is not */
/*         type 1. */

/* $ Files */

/*     Appropriate kernels must be loaded by the calling program before */
/*     this routine is called. */

/*     The following data are required: */

/*     -  An SCLK kernel providing data for the SCLK designated by */
/*        the input ID code SC. */

/*     In all cases, kernel data are normally loaded once per program */
/*     run, NOT every time this routine is called. */

/* $ Particulars */

/*     SCLK "type" is a SPICE-specific concept. It indicates which */
/*     mechanism is used by SPICE for representing time as measured */
/*     by a specified clock, and for converting those time measurements */
/*     to standard time systems such as TDB. */

/*     This routine speeds up determination of a spacecraft clock's type */
/*     by querying the type 1 SCLK database for information about that */
/*     clock. If the clock is not found, the kernel pool is searched, */
/*     and if the clock is determined to be type 1, information about it */
/*     is stored in the type 1 database. If the clock is not type 1, */
/*     the type is returned, but the type 1 database is not modified. */

/*     This is the only entry point of SC01 for which it is valid to */
/*     supply the ID of a clock not of type 1. */

/* $ Examples */

/*     See usage in the SPICELIB function SCTYPE. */

/* $ Restrictions */

/*     1)  This is essentially a SPICE-private routine. It should not */
/*         be called directly by user application code. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 01-DEC-2021 (NJB) (JDR) */

/*        New entry point. */

/* -& */
/* $ Index_Entries */

/*     return SCLK type, using type 1 database */

/* -& */
    if (return_()) {
	return 0;
    }
    chkin_("SCTY01", (ftnlen)6);

/*     At this point, PRVSC is either 0 or the ID of the last type 1 */
/*     clock for which a successful call to an entry point of SC01 */
/*     was made. PRVSC is never assigned the code of a clock that isn't */
/*     type 1. */

    if (pass1) {

/*        Initialize local data structures. */

	zzscin01_(hdsclk, scpool, clklst, &dpfree, &ifree, &prvsc);
	zzctruin_(polctr);
	pass1 = FALSE_;
    }

/*     The clock type is unknown at this point. */

    *clktyp = 0;

/*     Reinitialize the SC01 database if the kernel pool has been */
/*     updated. The call below syncs POLCTR. */

    zzpctrck_(polctr, &update);
    if (update) {

/*        Initialize local data structures. */

	zzscin01_(hdsclk, scpool, clklst, &dpfree, &ifree, &prvsc);
	samclk = FALSE_;
    } else {

/*        We never indicate the clock is the "same" as the previous */
/*        one unless the clock is type 1. */

	samclk = *sc != 0 && *sc == prvsc;
    }
    if (! samclk) {

/*        Get the SCLK type of the input SCLK. */

	s_copy(kvname, "SCLK_DATA_TYPE_#", (ftnlen)32, (ftnlen)16);
	i__1 = -(*sc);
	repmi_(kvname, "#", &i__1, kvname, (ftnlen)32, (ftnlen)1, (ftnlen)32);
	gipool_(kvname, &c__1, &c__1, &n, clktyp, &found, (ftnlen)32);
	if (failed_() || ! found) {
	    if (failed_()) {
		pass1 = TRUE_;
		chkout_("SCTY01", (ftnlen)6);
		return 0;
	    }
	    if (! found) {

/*              The GIPOOL call succeeded but the kernel variable was */
/*              not found. */

		pass1 = TRUE_;
		setmsg_("Kernel variable # was not found in the kernel pool.",
			 (ftnlen)51);
		errch_("#", kvname, (ftnlen)1, (ftnlen)32);
		sigerr_("SPICE(KERNELVARNOTFOUND)", (ftnlen)24);
		chkout_("SCTY01", (ftnlen)6);
		return 0;
	    }
	}

/*        At this point, the clock type is set. */

	if (*clktyp != 1) {

/*           The clock is not type 1, so we don't add it to the */
/*           SC01 database. */

	    prvsc = 0;
	    chkout_("SCTY01", (ftnlen)6);
	    return 0;
	}

/*        The SCLK is type 1 but may not be in the SC01 database, and */
/*        in any case is not the last type 1 clock seen on a successful */
/*        call to any SC01 entry point. Update the SCLK parameters and, */
/*        if necessary, the database. */

	zzscup01_(sc, polctr, hdsclk, scpool, clklst, &dpfree, dpbuff, &ifree,
		 intbuf, scbase, &prvsc, &nfield, &delcde, &timsys, &ncoeff, &
		npart, &cofbas, &strbas, &endbas, &modbas, &offbas);
	if (failed_()) {
	    pass1 = TRUE_;
	    chkout_("SCTY01", (ftnlen)6);
	    return 0;
	}
    }

/*     We can arrive here only if the clock type is 1. */

    *clktyp = 1;
    prvsc = *sc;
    chkout_("SCTY01", (ftnlen)6);
    return 0;
/* $Procedure SCPR01 ( SCLK partition bounds, type 1  ) */

L_scpr01:
/* $ Abstract */

/*     Return partition data for a specified type 1 clock. */

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
/*     TIME */

/* $ Keywords */

/*     CONVERSION */
/*     TIME */

/* $ Declarations */

/*     INTEGER               SC */
/*     INTEGER               NPARTN */
/*     DOUBLE PRECISION      PARBEG ( * ) */
/*     DOUBLE PRECISION      PAREND ( * ) */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     SC         I   NAIF spacecraft clock ID code. */
/*     NPARTN     O   Number of partitions. */
/*     PARBEG     O   Partition begin times. */
/*     PAREND     O   Partition end times. */

/* $ Detailed_Input */

/*     SC       is a NAIF ID code for a spacecraft clock. */

/* $ Detailed_Output */

/*     NPARTN   is the number of partitions for the clock */
/*              designated by SC. */

/*     PARBEG, */
/*     PAREND   are, respectively, arrays of partition begin and */
/*              end times for the clock designated by SC. The */
/*              times are expressed as encoded SCLK. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If an error occurs in the course of updating the type 1 SCLK */
/*         database, the error is signaled by a */
/*         routine in the call tree of this routine. */

/* $ Files */

/*     Appropriate kernels must be loaded by the calling program before */
/*     this routine is called. */

/*     The following data are required: */

/*     -  An SCLK kernel providing data for the SCLK designated by */
/*        the input ID code SC. */

/*     In all cases, kernel data are normally loaded once per program */
/*     run, NOT every time this routine is called. */

/* $ Particulars */

/*     None. */

/* $ Examples */

/*     See usage in the SPICELIB function SCPART. */

/* $ Restrictions */

/*     1)  This is essentially a SPICE-private routine. It should not */
/*         be called directly by user application code. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 01-DEC-2021 (NJB) (JDR) */

/*        New entry point. */

/* -& */
/* $ Index_Entries */

/*     return partition data for a type 1 SCLK */

/* -& */
    if (return_()) {
	return 0;
    }
    chkin_("SCPR01", (ftnlen)6);
    if (pass1) {

/*        Initialize local data structures. This routine is error-free. */

	zzscin01_(hdsclk, scpool, clklst, &dpfree, &ifree, &prvsc);
	zzctruin_(polctr);
	pass1 = FALSE_;
    }

/*     Get partition data for this clock. Update the type 1 database */
/*     if necessary. */

    zzscup01_(sc, polctr, hdsclk, scpool, clklst, &dpfree, dpbuff, &ifree, 
	    intbuf, scbase, &prvsc, &nfield, &delcde, &timsys, &ncoeff, &
	    npart, &cofbas, &strbas, &endbas, &modbas, &offbas);
    if (failed_()) {
	pass1 = TRUE_;
	chkout_("SCPR01", (ftnlen)6);
	return 0;
    }

/*     Set the output partition arguments. */

    moved_(&dpbuff[(i__1 = strbas) < 320018 && 0 <= i__1 ? i__1 : s_rnge(
	    "dpbuff", i__1, "sc01_", (ftnlen)3708)], &npart, parbeg);
    moved_(&dpbuff[(i__1 = endbas) < 320018 && 0 <= i__1 ? i__1 : s_rnge(
	    "dpbuff", i__1, "sc01_", (ftnlen)3709)], &npart, parend);
    *npartn = npart;
    prvsc = *sc;
    chkout_("SCPR01", (ftnlen)6);
    return 0;
} /* sc01_ */

/* Subroutine */ int sc01_(integer *sc, char *clkstr, doublereal *ticks, 
	doublereal *sclkdp, doublereal *et, integer *npartn, doublereal *
	parbeg, doublereal *parend, integer *clktyp, ftnlen clkstr_len)
{
    return sc01_0_(0, sc, clkstr, ticks, sclkdp, et, npartn, parbeg, parend, 
	    clktyp, clkstr_len);
    }

/* Subroutine */ int sctk01_(integer *sc, char *clkstr, doublereal *ticks, 
	ftnlen clkstr_len)
{
    return sc01_0_(1, sc, clkstr, ticks, (doublereal *)0, (doublereal *)0, (
	    integer *)0, (doublereal *)0, (doublereal *)0, (integer *)0, 
	    clkstr_len);
    }

/* Subroutine */ int scfm01_(integer *sc, doublereal *ticks, char *clkstr, 
	ftnlen clkstr_len)
{
    return sc01_0_(2, sc, clkstr, ticks, (doublereal *)0, (doublereal *)0, (
	    integer *)0, (doublereal *)0, (doublereal *)0, (integer *)0, 
	    clkstr_len);
    }

/* Subroutine */ int scte01_(integer *sc, doublereal *sclkdp, doublereal *et)
{
    return sc01_0_(3, sc, (char *)0, (doublereal *)0, sclkdp, et, (integer *)
	    0, (doublereal *)0, (doublereal *)0, (integer *)0, (ftnint)0);
    }

/* Subroutine */ int scet01_(integer *sc, doublereal *et, doublereal *sclkdp)
{
    return sc01_0_(4, sc, (char *)0, (doublereal *)0, sclkdp, et, (integer *)
	    0, (doublereal *)0, (doublereal *)0, (integer *)0, (ftnint)0);
    }

/* Subroutine */ int scec01_(integer *sc, doublereal *et, doublereal *sclkdp)
{
    return sc01_0_(5, sc, (char *)0, (doublereal *)0, sclkdp, et, (integer *)
	    0, (doublereal *)0, (doublereal *)0, (integer *)0, (ftnint)0);
    }

/* Subroutine */ int scty01_(integer *sc, integer *clktyp)
{
    return sc01_0_(6, sc, (char *)0, (doublereal *)0, (doublereal *)0, (
	    doublereal *)0, (integer *)0, (doublereal *)0, (doublereal *)0, 
	    clktyp, (ftnint)0);
    }

/* Subroutine */ int scpr01_(integer *sc, integer *npartn, doublereal *parbeg,
	 doublereal *parend)
{
    return sc01_0_(7, sc, (char *)0, (doublereal *)0, (doublereal *)0, (
	    doublereal *)0, npartn, parbeg, parend, (integer *)0, (ftnint)0);
    }

