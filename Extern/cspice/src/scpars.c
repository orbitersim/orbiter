/* scpars.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__9999 = 9999;
static integer c__1 = 1;

/* $Procedure SCPARS ( Parse a spacecraft clock string ) */
/* Subroutine */ int scpars_(integer *sc, char *sclkch, logical *error, char *
	msg, doublereal *sclkdp, ftnlen sclkch_len, ftnlen msg_len)
{
    /* System generated locals */
    integer i__1, i__2, i__3, i__4, i__5;
    doublereal d__1;

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_cmp(char *, char *, ftnlen, ftnlen), i_len(char *, ftnlen);
    double d_nint(doublereal *);
    integer s_rnge(char *, integer, char *, integer);

    /* Local variables */
    extern integer cpos_(char *, char *, integer *, ftnlen, ftnlen);
    integer part, i__;
    extern /* Subroutine */ int chkin_(char *, ftnlen), repmc_(char *, char *,
	     char *, char *, ftnlen, ftnlen, ftnlen, ftnlen), scps01_(integer 
	    *, char *, logical *, char *, doublereal *, ftnlen, ftnlen), 
	    repmi_(char *, char *, integer *, char *, ftnlen, ftnlen, ftnlen);
    doublereal ticks;
    integer dtype, pnter;
    char psmsg[255];
    logical pserr;
    doublereal pstop[9999];
    extern logical failed_(void);
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), scpart_(integer *, integer *, doublereal *, doublereal *)
	    , nparsi_(char *, integer *, char *, integer *, ftnlen, ftnlen), 
	    setmsg_(char *, ftnlen), errint_(char *, integer *, ftnlen);
    extern integer sctype_(integer *);
    integer nparts;
    doublereal pstart[9999];
    extern logical return_(void);
    char strerr[255];
    doublereal ptotls[9999];
    integer pos;

/* $ Abstract */

/*     Parse a character representation of spacecraft clock time and */
/*     encode it as a double precision number. */

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
/*     SCLKCH     I   Character representation of a spacecraft clock. */
/*     ERROR      O   Flag to indicate if string parsed correctly. */
/*     MSG        O   Error message if string did not parse. */
/*     SCLKDP     O   Encoded representation of the clock count. */
/*     MXPART     P   Maximum number of spacecraft clock partitions. */

/* $ Detailed_Input */

/*     SC       is the standard NAIF ID of the spacecraft whose clock's */
/*              time is being encoded. */

/*     SCLKCH   is the character representation of some spacecraft's */
/*              clock count. */

/*              SCLKCH will have the following general format: */

/*                           'pp/sclk_string', or just */
/*                              'sclk_string' */

/*              'pp' is an integer greater than or equal to one */
/*              and is called the partition number. */

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
/*              file, which needs to be loaded into the kernel pool, */
/*              using the routines CLPOOL and FURNSH. */

/*              The routine SCPART is used to read the partition */
/*              start and stop times, in encoded units of SCLK (called */
/*              "ticks" -- see SCLKDP below) from the kernel file. */

/*              If the partition number is included, it must be */
/*              separated from the rest of the string by a '/'. */
/*              Any number of spaces may separate the partition number, */
/*              the '/', and the rest of the clock string. */


/*              If the partition number is omitted, a default partition */
/*              will be assumed. The default partition is the lowest- */
/*              numbered partition that contains the given clock time. */
/*              If the clock time does not fall in any of the */
/*              partition boundaries then an error is signaled. */


/*              'sclk_string' is a spacecraft specific clock string. */
/*              Using Galileo as an example, the full format is */

/*                             wwwwwwww:xx:y:z */

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
/*              SCLK components may be separated by any of these */
/*              five characters: ' '  ':'  ','  '-'  '.' */
/*              Any number of spaces can separate the components and */
/*              the delimiters. The presence of the RIM component */
/*              is required. Successive components may be omitted, and */
/*              in such cases are assumed to represent zero values. */

/*              Values for the individual components may exceed the */
/*              maximum expected values. For instance, '0:0:0:9' is */
/*              an acceptable Galileo clock string, and will convert */
/*              to the same number of ticks as '0:0:1:1'. */

/*              Consecutive delimiters containing no intervening digits */
/*              are treated as if they delimit zero components. */

/*              Trailing zeros should always be included to match the */
/*              length of the counter.  For example, a Galileo clock */
/*              count of '25684.90' should not be represented as */
/*              '25684.9'. */

/*              Some spacecraft clock components have offset, or */
/*              starting, values different from zero. For example, */
/*              with an offset value of 1, a mod 20 counter would */
/*              cycle from 1 to 20 instead of from 0 to 19. */

/*              See the SCLK required reading for a detailed */
/*              description of the Voyager and Mars Observer clock */
/*              formats. */

/* $ Detailed_Output */

/*     ERROR    is .TRUE. if an error occurred parsing the input clock */
/*              string and converting it to ticks. */

/*     MSG      is the message generated if an error occurred parsing */
/*              the input clock string. */

/*     SCLKDP   is the double precision encoding of SCLKCH. */

/*              The encoding is such that order and proximity will be */
/*              preserved. That is, if t1, t2, and t3 are spacecraft */
/*              clock times, and t1*, t2*, and t3* are their encodings, */
/*              then if */

/*                            t1 < t2 < t3, and */

/*              t2 is closer to t1 than to t3, you will have the result */
/*              that */

/*                           t1* < t2* < t3*, and */

/*              t2* is closer to t1* than to t3*. */

/*              The units of encoded SCLK are "ticks since the start of */
/*              the mission", where a "tick" is defined to be the */
/*              shortest time increment expressible by a particular */
/*              spacecraft's clock. */

/*              Each clock string without partition number represents */
/*              a certain number of ticks, but you need to include */
/*              partition information to determine the relative */
/*              position of that time in relation to the start of the */
/*              mission. */

/*              Since the end time of one partition is coincident */
/*              with the begin time of the next, there are two */
/*              different representations for this instant, and they */
/*              will both yield the same encoding. */

/*              For example, if partition 1 has an end time of t1, and */
/*              partition 2 has a begin time of t2, then if we did */

/*                 CALL SCENCD ( '1/t1', SC, X ) and */
/*                 CALL SCENCD ( '2/t2', SC, Y ), then */

/*                                X = Y. */

/*              The individual routines TIKSnn, where nn is the */
/*              clock type code, contain more detailed information */
/*              on the conversion process. */

/* $ Parameters */

/*     MXPART   is the maximum number of spacecraft clock partitions */
/*              expected in the kernel file for any one spacecraft. */
/*              See the INCLUDE file sclk.inc for this parameter's */
/*              value. */

/* $ Exceptions */

/*     This routine uses both the normal SPICELIB error handling and */
/*     an ERROR flag and message. Errors that deal with kernel pool */
/*     data that are missing or invalid are treated in the usual way. */
/*     Errors that arise solely from parsing the input clock string */
/*     do not signal SPICELIB errors, but instead use the ERROR flag */
/*     and MSG string. */

/*     In the case of any SPICELIB error occurring, ERROR is initialized */
/*     to .TRUE. and MSG to 'SPICELIB error detected.'. */

/*     1)  If the number of partitions in the kernel file for spacecraft */
/*         SC exceeds the parameter MXPART, the error */
/*         SPICE(TOOMANYPARTS) is signaled. */

/*     2)  If the data type of the clock for the specified spacecraft is */
/*         of a data type not recognized by this routine, the error */
/*         SPICE(NOTSUPPORTED) is signaled. */

/*     If a partition number is included in the SCLK string, the */
/*     following errors may occur: */

/*     3)  The partition number cannot be parsed as an integer. */

/*     4)  The partition number is not in the range of the number of */
/*         partitions found in the kernel pool. */

/*     5)  The clock count does not fall in the boundaries of the */
/*         specified partition. */

/*     If a partition number is not included in the SCLK string, the */
/*     following exception may occur: */

/*     6)  The clock count does not fall in the boundaries of any */
/*         partition found in the kernel pool. */

/*     The actual parsing of the remainder of the clock string is */
/*     performed by data type specific routines. The following exceptions */
/*     may occur: */

/*     7)  The input spacecraft clock string is blank. */

/*     8)  The remainder clock string cannot be parsed by the SCLK data */
/*         type specific routine. The precise issue detected while */
/*         parsing the clock string is provided in the MSG string. */

/* $ Files */

/*     A kernel file containing spacecraft clock partition information */
/*     for the desired spacecraft must be loaded, using the routines */
/*     CLPOOL and FURNSH, before calling this routine. */

/* $ Particulars */

/*     In general, it is difficult to compare spacecraft clock counts */
/*     numerically since there are too many clock components for a */
/*     single comparison. This routine provides a method of assigning a */
/*     single double precision number to a spacecraft's clock count, */
/*     given one of its character representations. */

/*     The routine SCDECD performs the inverse operation to SCENCD, */
/*     converting an encoded double precision number to character format. */

/*     To convert the string to ticks since the start of the mission, */
/*     SCENCD */

/*        1) Converts the non-partition portion of the string to */
/*           ticks, using the routine SCTIKS. */

/*        2) Determines the partition number for the clock time, */
/*           either by getting it directly from the input string, or */
/*           determining the default partition if none was specified. */

/*        3) Includes partition start and stop times, which are also */
/*           measured in ticks, to compute the number of ticks */
/*           since the beginning of the mission of the clock time. */

/* $ Examples */

/*     Double precision encodings of spacecraft clock counts are used to */
/*     tag pointing data in the C-kernel. */

/*     In the following example, pointing for a sequence of images from */
/*     the Voyager 2 narrow angle camera is requested from the C-kernel */
/*     using an array of character spacecraft clock counts as input. */
/*     The clock counts attached to the output are then decoded to */
/*     character and compared with the input strings. */

/*           CHARACTER*(25)     SCLKIN   ( 4 ) */
/*           CHARACTER*(25)     SCLKOUT */
/*           CHARACTER*(25)     CLKTOL */

/*           DOUBLE PRECISION   TIMEIN */
/*           DOUBLE PRECISION   TIMOUT */
/*           DOUBLE PRECISION   CMAT     ( 3, 3 ) */

/*           INTEGER            NPICS */
/*           INTEGER            SC */

/*           DATA  NPICS     /  4                   / */

/*           DATA  SCLKIN    / '2 / 20538:39:768', */
/*          .                  '2 / 20543:21:768', */
/*          .                  '2 / 20550:37', */
/*          .                  '2 / 20561:59'       / */

/*           DATA  CLKTOL   /  '      0:01:000'     / */

/*     C */
/*     C     The instrument we want pointing for is the Voyager 2 */
/*     C     narrow angle camera. The reference frame we want is */
/*     C     J2000. The spacecraft is Voyager 2. */
/*     C */
/*           INST = -32001 */
/*           REF  = 'J2000' */
/*           SC   = -32 */

/*     C */
/*     C     Load the appropriate files. We need */
/*     C */
/*     C     1) CK file containing pointing data. */
/*     C     2) Spacecraft clock kernel file, for SCENCD and SCDECD. */
/*     C */
/*           CALL CKLPF  ( 'VGR2NA.CK' ) */
/*           CALL CLPOOL */
/*           CALL FURNSH ( 'SCLK.KER'  ) */

/*     C */
/*     C     Convert the tolerance string to ticks. */
/*     C */
/*           CALL SCTIKS ( SC, CLKTOL, TOL ) */

/*           DO I = 1, NPICS */

/*              CALL SCENCD ( SC, SCLKIN( I ), TIMEIN ) */

/*              CALL CKGP   ( INST, TIMEIN, TOL, REF, CMAT, TIMOUT, */
/*          .                 FOUND ) */

/*              CALL SCDECD ( SC, TIMOUT, SCLKOUT ) */

/*              WRITE (*,*) */
/*              WRITE (*,*) 'Input  s/c clock count: ', SCLKIN( I ) */
/*              WRITE (*,*) 'Output s/c clock count: ', SCLKOUT */
/*              WRITE (*,*) 'Output C-Matrix:        ', CMAT */
/*              WRITE (*,*) */

/*           END DO */

/*     The output from such a program might look like: */


/*           Input  s/c clock count:  2 / 20538:39:768 */
/*           Output s/c clock count:  2/20538:39:768 */
/*           Output C-Matrix:  'first C-matrix' */

/*           Input  s/c clock count:  2 / 20543:21:768 */
/*           Output s/c clock count:  2/20543:22:768 */
/*           Output C-Matrix:  'second C-matrix' */

/*           Input  s/c clock count:  2 / 20550:37 */
/*           Output s/c clock count:  2/20550:36:768 */
/*           Output C-Matrix:  'third C-matrix' */

/*           Input  s/c clock count:  2 / 20561:59 */
/*           Output s/c clock count:  2/20561:58:768 */
/*           Output C-Matrix:  'fourth C-matrix' */

/* $ Restrictions */

/*     None. */

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

/* -    SPICELIB Version 1.3.0, 22-NOV-2021 (JDR) (NJB) */

/*        Bug fix: out-of-range character positions of SCLKCH are */
/*        no longer referenced. */

/*        Edited the header to comply with NAIF standard. */

/*        Added entries #7 and #8 to $Exceptions section. */

/* -    SPICELIB Version 1.2.0, 05-FEB-2008 (NJB) */

/*        The values of parameter MXPART and is now */
/*        provided by the INCLUDE file sclk.inc. */

/* -    SPICELIB Version 1.1.1, 22-AUG-2006 (EDW) */

/*        Replaced references to LDPOOL with references */
/*        to FURNSH. */

/* -    SPICELIB Version 1.1.0, 18-JUN-1999 (WLT) */

/*        Make CHKIN and CHKOUT arguments consistent. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 03-SEP-1990 (JML) (RET) */

/* -& */
/* $ Index_Entries */

/*     encode spacecraft_clock */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }
    chkin_("SCPARS", (ftnlen)6);

/*     This routine handles errors in two different ways. */

/*     1) Errors relating to parsing the input clock string */
/*        will not use the normal SPICELIB error handling. */
/*        Instead they will use the ERROR and MSG arguments */
/*        to this routine. */

/*     2) Errors relating to missing or invalid data in the */
/*        kernel pool will use the normal SPICELIB error */
/*        handling. */

/*     In the event that a SPICE error occurs somewhere, ERROR */
/*     and MSG will be initialized to the following values: */

    *error = TRUE_;
    s_copy(msg, "SPICELIB error detected.", msg_len, (ftnlen)24);
    dtype = sctype_(sc);
    if (failed_()) {
	chkout_("SCPARS", (ftnlen)6);
	return 0;
    }
    if (dtype != 1) {
	setmsg_("Clock type # is not supported.", (ftnlen)30);
	errint_("#", &dtype, (ftnlen)1);
	sigerr_("SPICE(NOTSUPPORTED)", (ftnlen)19);
	chkout_("SCPARS", (ftnlen)6);
	return 0;
    }

/*     Read the partition start and stop times (in ticks) for this */
/*     mission. Error if there are too many of them. */

    scpart_(sc, &nparts, pstart, pstop);
    if (failed_()) {

/*        This code should be unreachable but is provided for safety. */
/*        Missing partition data should be caught by the earlier SCTYPE */
/*        call, which accesses the type 1 SCLK database. */

	chkout_("SCPARS", (ftnlen)6);
	return 0;
    }
    if (nparts > 9999) {

/*        This code should be unreachable but is provided for safety. */

	setmsg_("The number of partitions, #, for spacecraft # exceeds the v"
		"alue for parameter MXPART, #.", (ftnlen)88);
	errint_("#", &nparts, (ftnlen)1);
	errint_("#", sc, (ftnlen)1);
	errint_("#", &c__9999, (ftnlen)1);
	sigerr_("SPICE(TOOMANYPARTS)", (ftnlen)19);
	chkout_("SCPARS", (ftnlen)6);
	return 0;
    }

/*     First check if the string is blank. */

    if (s_cmp(sclkch, " ", sclkch_len, (ftnlen)1) == 0) {
	*error = TRUE_;
	s_copy(msg, "Input spacecraft clock string is blank.", msg_len, (
		ftnlen)39);
	chkout_("SCPARS", (ftnlen)6);
	return 0;
    }

/*     Convert the non-partition clock string to a tick value. */
/*     This conversion depends on the data type of the clock. */

    pos = cpos_(sclkch, "/", &c__1, sclkch_len, (ftnlen)1);
    if (pos == 1) {

/*        The slash character is first character in the string which */
/*        means that the partition number is not there. */

	s_copy(msg, "Unable to parse the partition number from SCLK string #."
		, msg_len, (ftnlen)56);
	repmc_(msg, "#", sclkch, msg, msg_len, (ftnlen)1, sclkch_len, msg_len)
		;
	chkout_("SCPARS", (ftnlen)6);
	return 0;
    }
    if (pos == i_len(sclkch, sclkch_len)) {
	s_copy(msg, "SCLK string ends with slash.", msg_len, (ftnlen)28);
	chkout_("SCPARS", (ftnlen)6);
	return 0;
    }

/*     Parse the portion of the clock string following the slash, */
/*     if any, or the whole string if the slash is absent. */

    i__1 = pos;
    scps01_(sc, sclkch + i__1, &pserr, psmsg, &ticks, sclkch_len - i__1, (
	    ftnlen)255);
    if (failed_()) {

/*        This code should be unreachable but is provided for safety. */
/*        Missing SCLK data should be caught by the earlier SCTYPE call, */
/*        which accesses the type 1 SCLK database. */

	chkout_("SCPARS", (ftnlen)6);
	return 0;
    }

/*     Check if the SCPSxx routine encountered a problem. */

    if (pserr) {
	*error = TRUE_;
	s_copy(msg, psmsg, msg_len, (ftnlen)255);
	chkout_("SCPARS", (ftnlen)6);
	return 0;
    }

/*     Find the partition that this clock time falls in. */

/*     For each partition, compute the total number of ticks in that */
/*     partition plus all preceding partitions. */

    d__1 = pstop[0] - pstart[0];
    ptotls[0] = d_nint(&d__1);
    i__1 = nparts;
    for (i__ = 2; i__ <= i__1; ++i__) {
	d__1 = ptotls[(i__3 = i__ - 2) < 9999 && 0 <= i__3 ? i__3 : s_rnge(
		"ptotls", i__3, "scpars_", (ftnlen)640)] + pstop[(i__4 = i__ 
		- 1) < 9999 && 0 <= i__4 ? i__4 : s_rnge("pstop", i__4, "scp"
		"ars_", (ftnlen)640)] - pstart[(i__5 = i__ - 1) < 9999 && 0 <= 
		i__5 ? i__5 : s_rnge("pstart", i__5, "scpars_", (ftnlen)640)];
	ptotls[(i__2 = i__ - 1) < 9999 && 0 <= i__2 ? i__2 : s_rnge("ptotls", 
		i__2, "scpars_", (ftnlen)640)] = d_nint(&d__1);
    }

/*     Determine the partition number for the input clock string: */

/*        If it was included in the string make sure it's valid for */
/*        this mission. */

/*           Error if */

/*           1) The partition number can't be parsed. */

/*           2) The partition number is not in the range 1 to the number */
/*              of partitions. */

/*           3) The clock count does not fall in the boundaries of the */
/*              specified partition. */

/*        If it wasn't included, determine the default partition for */
/*        this clock count. */

/*           Error if */

/*           1) The clock count does not fall in the boundaries of any */
/*              of the partitions. */


    if (pos > 1) {

/*        Try to parse the partition number. */

	part = 0;
	nparsi_(sclkch, &part, strerr, &pnter, pos - 1, (ftnlen)255);

/*        Make sure that the number parsed is correct. */

	if (s_cmp(strerr, " ", (ftnlen)255, (ftnlen)1) != 0) {

/*          Was not able to parse a number. */

	    s_copy(msg, "Unable to parse the partition number from SCLK stri"
		    "ng #.", msg_len, (ftnlen)56);
	    repmc_(msg, "#", sclkch, msg, msg_len, (ftnlen)1, sclkch_len, 
		    msg_len);
	    chkout_("SCPARS", (ftnlen)6);
	    return 0;
	} else if (part <= 0 || part > nparts) {

/*           The parsed number does not fall in the range of valid */
/*           numbers. */

	    s_copy(msg, "Partition number # taken from SCLK string # is not "
		    "in acceptable range 1 to #.", msg_len, (ftnlen)78);
	    repmi_(msg, "#", &part, msg, msg_len, (ftnlen)1, msg_len);
	    repmc_(msg, "#", sclkch, msg, msg_len, (ftnlen)1, sclkch_len, 
		    msg_len);
	    repmi_(msg, "#", &nparts, msg, msg_len, (ftnlen)1, msg_len);
	    chkout_("SCPARS", (ftnlen)6);
	    return 0;
	} else if (ticks < pstart[(i__1 = part - 1) < 9999 && 0 <= i__1 ? 
		i__1 : s_rnge("pstart", i__1, "scpars_", (ftnlen)709)] || 
		ticks > pstop[(i__2 = part - 1) < 9999 && 0 <= i__2 ? i__2 : 
		s_rnge("pstop", i__2, "scpars_", (ftnlen)709)]) {

/*           The TICKS value does not fall in the range of valid */
/*           values for the partition number parsed from the input */
/*           clock string. */

	    s_copy(msg, "SCLK count from # does not fall in the boundaries o"
		    "f partition number #.", msg_len, (ftnlen)72);
	    i__1 = pos;
	    repmc_(msg, "#", sclkch + i__1, msg, msg_len, (ftnlen)1, 
		    sclkch_len - i__1, msg_len);
	    repmi_(msg, "#", &part, msg, msg_len, (ftnlen)1, msg_len);
	    chkout_("SCPARS", (ftnlen)6);
	    return 0;
	}
    } else {

/*        The partition number was not included in the string. */
/*        Determine the partition from the TICKS value that the */
/*        clock string converted to. */

	part = 1;
	while(part <= nparts && (ticks < pstart[(i__1 = part - 1) < 9999 && 0 
		<= i__1 ? i__1 : s_rnge("pstart", i__1, "scpars_", (ftnlen)
		738)] || ticks > pstop[(i__2 = part - 1) < 9999 && 0 <= i__2 ?
		 i__2 : s_rnge("pstop", i__2, "scpars_", (ftnlen)738)])) {
	    ++part;
	}
	if (part > nparts) {
	    s_copy(msg, "SCLK count # does not fall in the boundaries of any"
		    " of the partitions for spacecraft #.", msg_len, (ftnlen)
		    87);
	    i__1 = pos;
	    repmc_(msg, "#", sclkch + i__1, msg, msg_len, (ftnlen)1, 
		    sclkch_len - i__1, msg_len);
	    repmi_(msg, "#", sc, msg, msg_len, (ftnlen)1, msg_len);
	    chkout_("SCPARS", (ftnlen)6);
	    return 0;
	}
    }

/*     Now we have a valid partition number, and the number of ticks for */
/*     the clock string. To convert to ticks since the start of the */
/*     mission, add in the total number of ticks in preceding partitions */
/*     and subtract off the starting ticks value for this partition. */

    if (part > 1) {
	*sclkdp = ticks - pstart[(i__1 = part - 1) < 9999 && 0 <= i__1 ? i__1 
		: s_rnge("pstart", i__1, "scpars_", (ftnlen)769)] + ptotls[(
		i__2 = part - 2) < 9999 && 0 <= i__2 ? i__2 : s_rnge("ptotls",
		 i__2, "scpars_", (ftnlen)769)];
    } else {
	*sclkdp = ticks - pstart[(i__1 = part - 1) < 9999 && 0 <= i__1 ? i__1 
		: s_rnge("pstart", i__1, "scpars_", (ftnlen)771)];
    }
    *error = FALSE_;
    s_copy(msg, " ", msg_len, (ftnlen)1);
    chkout_("SCPARS", (ftnlen)6);
    return 0;
} /* scpars_ */

