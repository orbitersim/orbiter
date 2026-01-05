/* scdecd.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__9999 = 9999;
static integer c__0 = 0;

/* $Procedure SCDECD ( Decode spacecraft clock ) */
/* Subroutine */ int scdecd_(integer *sc, doublereal *sclkdp, char *sclkch, 
	ftnlen sclkch_len)
{
    /* System generated locals */
    integer i__1, i__2, i__3, i__4, i__5;
    doublereal d__1;

    /* Builtin functions */
    double d_nint(doublereal *);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_rnge(char *, integer, char *, integer), i_len(char *, ftnlen);

    /* Local variables */
    integer part, i__;
    extern /* Subroutine */ int chkin_(char *, ftnlen), errdp_(char *, 
	    doublereal *, ftnlen);
    doublereal ticks;
    extern /* Subroutine */ int scfmt_(integer *, doublereal *, char *, 
	    ftnlen);
    static doublereal pstop[9999];
    extern logical failed_(void);
    extern integer lastnb_(char *, ftnlen);
    integer prelen;
    extern integer lstled_(doublereal *, integer *, doublereal *);
    extern /* Subroutine */ int sigerr_(char *, ftnlen);
    integer suflen;
    extern /* Subroutine */ int scpart_(integer *, integer *, doublereal *, 
	    doublereal *), chkout_(char *, ftnlen), prefix_(char *, integer *,
	     char *, ftnlen, ftnlen), setmsg_(char *, ftnlen), errint_(char *,
	     integer *, ftnlen), suffix_(char *, integer *, char *, ftnlen, 
	    ftnlen);
    integer nparts;
    static doublereal pstart[9999];
    extern logical return_(void);
    extern /* Subroutine */ int intstr_(integer *, char *, ftnlen);
    static doublereal ptotls[9999];
    char prtstr[5];

/* $ Abstract */

/*     Convert a double precision encoding of spacecraft clock time into */
/*     a character representation. */

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

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     SC         I   NAIF spacecraft identification code. */
/*     SCLKDP     I   Encoded representation of a spacecraft clock count. */
/*     SCLKCH     O   Character representation of a clock count. */
/*     MXPART     P   Maximum number of spacecraft clock partitions. */

/* $ Detailed_Input */

/*     SC       is the NAIF integer code of the spacecraft whose */
/*              clock's time is being decoded. */

/*     SCLKDP   is the double precision encoding of a clock time in */
/*              units of ticks since the spacecraft clock start time. */
/*              This value does reflect partition information. */

/*              An analogy may be drawn between a spacecraft clock */
/*              and a standard wall clock. The number of ticks */
/*              corresponding to the wall clock string */

/*                 hh:mm:ss */

/*              would be the number of seconds represented by that */
/*              time. */

/*              For example: */

/*                 Clock string      Number of ticks */
/*                 ------------      --------------- */
/*                   00:00:10              10 */
/*                   00:01:00              60 */
/*                   00:10:00             600 */
/*                   01:00:00            3600 */

/*              If SCLKDP contains a fractional part the result */
/*              is the same as if SCLKDP had been rounded to the */
/*              nearest whole number. */

/* $ Detailed_Output */

/*     SCLKCH   is the character representation of the clock count. */
/*              The exact form that SCLKCH takes depends on the */
/*              spacecraft. */

/*              Nevertheless, SCLKCH will have the following general */
/*              format: */

/*                 'pp/sclk_string' */

/*              'pp' is an integer greater than or equal to one and */
/*              represents a "partition number". */

/*              Each mission is divided into some number of partitions. */
/*              A new partition starts when the spacecraft clock */
/*              resets, either to zero, or to some other */
/*              value. Thus, the first partition for any mission */
/*              starts with launch, and ends with the first clock */
/*              reset. The second partition starts immediately when */
/*              the first stopped, and so on. */

/*              In order to be completely unambiguous about a */
/*              particular time, you need to specify a partition number */
/*              along with the standard clock string. */

/*              Information about when partitions occur for different */
/*              missions is contained in a spacecraft clock kernel */
/*              file which needs to be loaded into the kernel pool */
/*              before calling SCDECD. */

/*              The routine SCPART may be used to read the partition */
/*              start and stop times, in encoded units of ticks, from */
/*              the kernel file. */

/*              Since the end time of one partition is coincident with */
/*              the begin time of the next, two different time strings */
/*              with different partition numbers can encode into the */
/*              same value. */

/*              For example, if partition 1 ends at time t1, and */
/*              partition 2 starts at time t2, then */

/*                 '1/t1' and '2/t2' */

/*              will be encoded into the same value, say X. SCDECD */
/*              always decodes such values into the latter of the */
/*              two partitions. In this example, */

/*                 CALL SCDECD ( X, SC, CLKSTR ) */

/*              will result in */

/*                 CLKSTR = '2/t2'. */

/*              'sclk_string' is a spacecraft specific clock string, */
/*              typically consisting of a number of components */
/*              separated by delimiters. */

/*              Using Galileo as an example, the full format is */

/*                 wwwwwwww:xx:y:z */

/*              where z is a mod-8 counter (values 0-7) which */
/*              increments approximately once every 8 1/3 ms., y is a */
/*              mod-10 counter (values 0-9) which increments once */
/*              every time z turns over, i.e., approximately once every */
/*              66 2/3 ms., xx is a mod-91 (values 0-90) counter */
/*              which increments once every time y turns over, i.e., */
/*              once every 2/3 seconds. wwwwwwww is the Real-Time Image */
/*              Count (RIM), which increments once every time xx turns */
/*              over, i.e., once every 60 2/3 seconds. The roll-over */
/*              expression for the RIM is 16777215, which corresponds */
/*              to approximately 32 years. */

/*              wwwwwwww, xx, y, and z are referred to interchangeably */
/*              as the fields or components of the spacecraft clock. */
/*              SCLK components may be separated by any of these five */
/*              characters: ' '  ':'  ','  '-'  '.' */
/*              The delimiter used is determined by a kernel pool */
/*              variable and can be adjusted by the user. */

/*              Some spacecraft clock components have offset, or */
/*              starting, values different from zero. For example, */
/*              with an offset value of 1, a mod 20 counter would */
/*              cycle from 1 to 20 instead of from 0 to 19. */

/*              See the SCLK required reading for a detailed */
/*              description of the Voyager and Mars Observer clock */
/*              formats. */

/* $ Parameters */

/*     MXPART   is the maximum number of spacecraft clock partitions */
/*              expected in the kernel file for any one spacecraft. */
/*              See the INCLUDE file sclk.inc for this parameter's */
/*              value. */

/* $ Exceptions */

/*     1)  If kernel variables required by this routine are unavailable, */
/*         an error is signaled by a routine in the call tree of this */
/*         routine. SCLKCH will be returned as a blank string in this */
/*         case. */

/*     2)  If the number of partitions in the kernel file for spacecraft */
/*         SC exceeds the parameter MXPART, the error */
/*         SPICE(TOOMANYPARTS) is signaled. SCLKCH will be returned */
/*         as a blank string in this case. */

/*     3)  If the encoded value does not fall in the boundaries of the */
/*         mission, the error SPICE(VALUEOUTOFRANGE) is signaled. */
/*         SCLKCH will be returned as a blank string in this case. */

/*     4)  If the declared length of SCLKCH is not large enough to */
/*         contain the output clock string, the error */
/*         SPICE(SCLKTRUNCATED) is signaled by either this routine or a */
/*         routine in the call tree of this routine. On output SCLKCH */
/*         will contain a portion of the truncated clock string. */

/* $ Files */

/*     A kernel file containing spacecraft clock partition information */
/*     for the desired spacecraft must be loaded, using the routine */
/*     FURNSH, before calling this routine. */

/* $ Particulars */

/*     In general, it is difficult to compare spacecraft clock counts */
/*     numerically since there are too many clock components for a */
/*     single comparison. The routine SCENCD provides a method of */
/*     assigning a single double precision number to a spacecraft's */
/*     clock count, given one of its character representations. */

/*     This routine performs the inverse operation to SCENCD, converting */
/*     an encoded double precision number to character format. */

/*     To convert the number of ticks since the start of the mission to */
/*     a clock format character string, SCDECD: */

/*        1) Determines the spacecraft clock partition that TICKS falls */
/*           in. */

/*        2) Subtracts off the number of ticks occurring in previous */
/*           partitions, to get the number of ticks since the beginning */
/*           of the current partition. */

/*        3) Converts the resulting ticks to clock format and forms the */
/*           string */

/*              'partition_number/clock_string' */

/* $ Examples */

/*     The numerical results shown for this example may differ across */
/*     platforms. The results depend on the SPICE kernels used as input, */
/*     the compiler and supporting libraries, and the machine specific */
/*     arithmetic implementation. */

/*     1) Double precision encodings of spacecraft clock counts are used */
/*        to tag pointing data in the C-kernel. */

/*        In the following example, pointing for a sequence of images */
/*        from the CASSINI Imaging Science Subsystem (ISS) is requested */
/*        from the C-kernel using an array of character spacecraft clock */
/*        counts as input. The clock counts attached to the output are */
/*        then decoded to character and compared with the input strings. */

/*        Use the CK kernel below to load the CASSINI image navigated */
/*        spacecraft pointing and orientation data. */

/*           04153_04182ca_ISS.bc */


/*        Use the SCLK kernel below to load the CASSINI spacecraft clock */
/*        time correlation data required for the conversion between */
/*        spacecraft clock string representation and double precision */
/*        encoding of spacecraft clock counts. */

/*           cas00071.tsc */


/*        Example code begins here. */


/*              PROGRAM SCDECD_EX1 */
/*              IMPLICIT NONE */

/*        C */
/*        C     Local parameters. */
/*        C */
/*        C     The instrument we want pointing for is the CASSINI */
/*        C     spacecraft. The reference frame we want is */
/*        C     J2000. The spacecraft is CASSINI. */
/*        C */
/*              INTEGER               SC */
/*              PARAMETER           ( SC     = -82 ) */

/*              INTEGER               INST */
/*              PARAMETER           ( INST   = -82000 ) */

/*              CHARACTER*(*)         REF */
/*              PARAMETER           ( REF    = 'J2000' ) */

/*              CHARACTER*(*)         CK */
/*              PARAMETER           ( CK     = '04153_04182ca_ISS.bc' ) */

/*              CHARACTER*(*)         SCLK */
/*              PARAMETER           ( SCLK   = 'cas00071.tsc' ) */

/*              INTEGER               NPICS */
/*              PARAMETER           ( NPICS  = 4 ) */

/*              CHARACTER*(*)         CLKTOL */
/*              PARAMETER           ( CLKTOL = '1.0' ) */

/*              INTEGER               MAXLEN */
/*              PARAMETER           ( MAXLEN = 30 ) */

/*        C */
/*        C     Local variables. */
/*        C */
/*              CHARACTER*(25)        SCLKIN (4) */
/*              CHARACTER*(25)        SCLKOUT */

/*              DOUBLE PRECISION      CMAT   (3,3) */
/*              DOUBLE PRECISION      TIMEIN */
/*              DOUBLE PRECISION      TIMEOUT */
/*              DOUBLE PRECISION      TOL */

/*              INTEGER               I */
/*              INTEGER               J */
/*              INTEGER               K */

/*              LOGICAL               FOUND */

/*        C */
/*        C     Set the input SCLK strings. */
/*        C */
/*              DATA                  SCLKIN /  '1/1465644279.0', */
/*             .                                '1/1465644281.0', */
/*             .                                '1/1465644351.0', */
/*             .                                '1/1465644361.0'  / */

/*        C */
/*        C     Load the appropriate files. We need */
/*        C */
/*        C        1. CK file containing pointing data. */
/*        C        2. Spacecraft clock kernel file. */
/*        C */
/*              CALL FURNSH ( CK   ) */
/*              CALL FURNSH ( SCLK ) */

/*        C */
/*        C     Convert the tolerance string to ticks. */
/*        C */
/*              CALL SCTIKS ( SC, CLKTOL, TOL ) */

/*              DO I= 1, NPICS */

/*                 CALL SCENCD ( SC, SCLKIN(I), TIMEIN ) */

/*                 CALL CKGP ( INST, TIMEIN,  TOL,  REF, */
/*             .               CMAT, TIMEOUT, FOUND     ) */

/*                 WRITE(*,*) */
/*                 WRITE(*,'(2A)') 'Input s/c clock count : ', SCLKIN(I) */

/*                 IF ( FOUND ) THEN */

/*                    CALL SCDECD ( SC, TIMEOUT, SCLKOUT ) */

/*                    WRITE(*,'(2A)') 'Output s/c clock count: ', */
/*             .                                          SCLKOUT */
/*                    WRITE(*,'(A)') 'Output C-Matrix:' */

/*                    DO J = 1, 3 */

/*                       WRITE(*,'(3F21.15)') ( CMAT(J,K), K = 1, 3 ) */

/*                    END DO */

/*                 ELSE */

/*                    WRITE(*,'(A)') 'No pointing found.' */

/*                 END IF */

/*              END DO */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*        Input s/c clock count : 1/1465644279.0 */
/*        No pointing found. */

/*        Input s/c clock count : 1/1465644281.0 */
/*        Output s/c clock count: 1/1465644281.171 */
/*        Output C-Matrix: */
/*           -0.335351455948710    0.864374440205611    0.374694846658341 */
/*           -0.937887426812980   -0.343851965210223   -0.046184419961653 */
/*            0.088918927227039   -0.366909598048763    0.925997176691424 */

/*        Input s/c clock count : 1/1465644351.0 */
/*        Output s/c clock count: 1/1465644351.071 */
/*        Output C-Matrix: */
/*           -0.335380929397586    0.864363638262230    0.374693385378623 */
/*           -0.937874292008090   -0.343889838107825   -0.046169163264003 */
/*            0.088946301703530   -0.366899550417080    0.925998528787713 */

/*        Input s/c clock count : 1/1465644361.0 */
/*        No pointing found. */


/* $ Restrictions */

/*     1)  Assumes that an SCLK kernel file appropriate for the clock */
/*         designated by SC is loaded in the kernel pool at the time */
/*         this routine is called. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     J.M. Lynch         (JPL) */
/*     W.L. Taber         (JPL) */
/*     R.E. Thurman       (JPL) */
/*     E.D. Wright        (JPL) */

/* $ Version */

/* -    SPICELIB Version 2.2.0, 18-NOV-2021 (NJB) (JDR) */

/*        Now variables PSTART, PSTOP, and PTOTLS are saved. Made minor */
/*        changes to formatting of code. */

/*        Edited the header to comply with NAIF standard. Added complete */
/*        code example based on existing example fragments using PDS */
/*        archived CASSINI data. */

/*        Added FAILED() call after SCFMT call. */

/*        Removed unnecessary entries in $Revisions section. */

/* -    SPICELIB Version 2.1.0, 05-FEB-2008 (NJB) */

/*        Values of parameter MXPART and PARTLN are now */
/*        provided by the INCLUDE file sclk.inc. */

/* -    SPICELIB Version 2.0.1, 22-AUG-2006 (EDW) */

/*        Replaced references to LDPOOL with references */
/*        to FURNSH. */

/* -    SPICELIB Version 2.0.0, 17-APR-1992 (JML) (WLT) */

/*        The routine was changed to signal an error when SCLKCH is */
/*        not long enough to contain the output spacecraft clock */
/*        string. */

/*        FAILED is now checked after calling SCPART. */

/*        References to CLPOOL were deleted. */

/*        Miscellaneous minor updates to the header were performed. */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 06-SEP-1990 (JML) (RET) */

/* -& */
/* $ Index_Entries */

/*     decode spacecraft_clock */

/* -& */
/* $ Revisions */

/* -    SPICELIB Version 2.0.0, 17-APR-1992 (JML) (WLT) */

/*        The routine was changed to signal an error when SCLKCH is */
/*        not long enough to contain the output spacecraft clock */
/*        string. Previously, the SCLK routines simply truncated */
/*        the clock string on the right. It was determined that */
/*        since this truncation could easily go undetected by the */
/*        user ( only the leftmost field of a clock string is */
/*        required when clock string is used as an input to a */
/*        SCLK routine ), it would be better to signal an error */
/*        when this happens. */

/*        FAILED is checked after calling SCPART in case an */
/*        error has occurred reading the kernel file and the */
/*        error action is not set to 'abort'. */

/*        References to CLPOOL were deleted. */

/*        Miscellaneous minor updates to the header were performed. */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     Saved variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }
    chkin_("SCDECD", (ftnlen)6);

/*     Use a working copy of the input. */

    ticks = d_nint(sclkdp);
    s_copy(sclkch, " ", sclkch_len, (ftnlen)1);

/*     Read the partition start and stop times (in ticks) for this */
/*     mission. Error if there are too many of them.  Also need to */
/*     check FAILED in case error handling is not in ABORT or */
/*     DEFAULT mode. */

    scpart_(sc, &nparts, pstart, pstop);
    if (failed_()) {
	chkout_("SCDECD", (ftnlen)6);
	return 0;
    }
    if (nparts > 9999) {

/*        This code should be unreachable. It is included for safety. */

	setmsg_("The number of partitions, #, for spacecraft # exceeds the v"
		"alue for parameter MXPART, #.", (ftnlen)88);
	errint_("#", &nparts, (ftnlen)1);
	errint_("#", sc, (ftnlen)1);
	errint_("#", &c__9999, (ftnlen)1);
	sigerr_("SPICE(TOOMANYPARTS)", (ftnlen)19);
	chkout_("SCDECD", (ftnlen)6);
	return 0;
    }

/*     For each partition, compute the total number of ticks in that */
/*     partition plus all preceding partitions. */

    d__1 = pstop[0] - pstart[0];
    ptotls[0] = d_nint(&d__1);
    i__1 = nparts;
    for (i__ = 2; i__ <= i__1; ++i__) {
	d__1 = ptotls[(i__3 = i__ - 2) < 9999 && 0 <= i__3 ? i__3 : s_rnge(
		"ptotls", i__3, "scdecd_", (ftnlen)595)] + pstop[(i__4 = i__ 
		- 1) < 9999 && 0 <= i__4 ? i__4 : s_rnge("pstop", i__4, "scd"
		"ecd_", (ftnlen)595)] - pstart[(i__5 = i__ - 1) < 9999 && 0 <= 
		i__5 ? i__5 : s_rnge("pstart", i__5, "scdecd_", (ftnlen)595)];
	ptotls[(i__2 = i__ - 1) < 9999 && 0 <= i__2 ? i__2 : s_rnge("ptotls", 
		i__2, "scdecd_", (ftnlen)595)] = d_nint(&d__1);
    }

/*     The partition corresponding to the input ticks is the first one */
/*     whose tick total is greater than the input value.  The one */
/*     exception is when the input ticks is equal to the total number */
/*     of ticks represented by all the partitions.  In this case the */
/*     partition number is the last one, i.e. NPARTS. */

/*     Error if TICKS comes before the first partition (that is, if it's */
/*     negative), or after the last one. */

    if (ticks == ptotls[(i__1 = nparts - 1) < 9999 && 0 <= i__1 ? i__1 : 
	    s_rnge("ptotls", i__1, "scdecd_", (ftnlen)608)]) {
	part = nparts;
    } else {
	part = lstled_(&ticks, &nparts, ptotls) + 1;
    }
    if (ticks < 0. || part > nparts) {
	setmsg_("Value for ticks, #, does not fall in any partition for spac"
		"ecraft #.", (ftnlen)68);
	errdp_("#", &ticks, (ftnlen)1);
	errint_("#", sc, (ftnlen)1);
	sigerr_("SPICE(VALUEOUTOFRANGE)", (ftnlen)22);
	chkout_("SCDECD", (ftnlen)6);
	return 0;
    }

/*     To get the count in this partition, subtract off the total of */
/*     the preceding partition counts and add the beginning count for */
/*     this partition. */

    if (part == 1) {
	ticks += pstart[(i__1 = part - 1) < 9999 && 0 <= i__1 ? i__1 : s_rnge(
		"pstart", i__1, "scdecd_", (ftnlen)633)];
    } else {
	ticks = ticks + pstart[(i__1 = part - 1) < 9999 && 0 <= i__1 ? i__1 : 
		s_rnge("pstart", i__1, "scdecd_", (ftnlen)635)] - ptotls[(
		i__2 = part - 2) < 9999 && 0 <= i__2 ? i__2 : s_rnge("ptotls",
		 i__2, "scdecd_", (ftnlen)635)];
    }

/*     Now create the output SCLK clock string. */

/*     First convert from ticks to clock string format. */

    scfmt_(sc, &ticks, sclkch, sclkch_len);
    if (failed_()) {
	chkout_("SCDECD", (ftnlen)6);
	return 0;
    }

/*     Now convert the partition number to a character string and prefix */
/*     it to the output string. */

    intstr_(&part, prtstr, (ftnlen)5);
    suffix_("/", &c__0, prtstr, (ftnlen)1, (ftnlen)5);
    prelen = lastnb_(prtstr, (ftnlen)5);
    suflen = lastnb_(sclkch, sclkch_len);
    if (i_len(sclkch, sclkch_len) - suflen < prelen) {
	setmsg_("Output string too short to contain clock string. Input tick"
		" value: #, requires string of length #, but declared length "
		"is #.", (ftnlen)124);
	errdp_("#", sclkdp, (ftnlen)1);
	i__1 = prelen + suflen;
	errint_("#", &i__1, (ftnlen)1);
	i__1 = i_len(sclkch, sclkch_len);
	errint_("#", &i__1, (ftnlen)1);
	sigerr_("SPICE(SCLKTRUNCATED)", (ftnlen)20);
	chkout_("SCDECD", (ftnlen)6);
	return 0;
    }
    prefix_(prtstr, &c__0, sclkch, (ftnlen)5, sclkch_len);
    chkout_("SCDECD", (ftnlen)6);
    return 0;
} /* scdecd_ */

