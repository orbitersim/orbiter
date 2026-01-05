/* sct2e.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure SCT2E ( SCLK ticks to ET ) */
/* Subroutine */ int sct2e_(integer *sc, doublereal *sclkdp, doublereal *et)
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    extern /* Subroutine */ int chkin_(char *, ftnlen), scte01_(integer *, 
	    doublereal *, doublereal *), sigerr_(char *, ftnlen), chkout_(
	    char *, ftnlen), setmsg_(char *, ftnlen), errint_(char *, integer 
	    *, ftnlen);
    extern integer sctype_(integer *);
    extern logical return_(void);

/* $ Abstract */

/*     Convert encoded spacecraft clock (`ticks') to ephemeris */
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

/*     CK */
/*     SCLK */
/*     SPK */
/*     TIME */

/* $ Keywords */

/*     CONVERSION */
/*     TIME */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     SC         I   NAIF spacecraft ID code. */
/*     SCLKDP     I   SCLK, encoded as ticks since spacecraft clock */
/*                    start. */
/*     ET         O   Ephemeris time, seconds past J2000. */

/* $ Detailed_Input */

/*     SC       is a NAIF integer code for a spacecraft, one of */
/*              whose encoded clock values is represented by */
/*              SCLKDP. */

/*     SCLKDP   is an encoded spacecraft clock value. SCLKDP */
/*              represents time measured from spacecraft clock */
/*              start: partition information IS reflected in the */
/*              encoded value. */

/* $ Detailed_Output */

/*     ET       is the epoch, specified as ephemeris seconds past */
/*              J2000, that corresponds to SCLKDP. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If an SCLK kernel has not been loaded, does not contain all of */
/*         the required data, or contains invalid data, an error is */
/*         signaled by a routine in the call tree of this routine. The */
/*         output argument ET will not be modified. This routine assumes */
/*         that that an SCLK kernel appropriate to the spacecraft clock */
/*         identified by the input argument SC has been loaded. */

/*     2)  If a leapseconds kernel is required for conversion between */
/*         SCLK and ET but is not loaded, an error is signaled by a */
/*         routine in the call tree of this routine. The output argument */
/*         ET will not be modified. When using SCLK kernels that map SCLK */
/*         to a time system other than ET (also called barycentric */
/*         dynamical time---`TDB'), it is necessary to have a leapseconds */
/*         kernel loaded at the time this routine is called. */

/*         The time system that an SCLK kernel maps SCLK to is indicated */
/*         by the variable SCLK_TIME_SYSTEM_nn in the kernel, where nn */
/*         is the negative of the NAIF integer code for the spacecraft. */
/*         The time system used in a kernel is TDB if and only if the */
/*         variable is assigned the value 1. */


/*     3)  If the clock type for the spacecraft clock identified by */
/*         SC is not supported by this routine, the error */
/*         SPICE(NOTSUPPORTED) is signaled. The output argument ET */
/*         will not be modified. */

/*     4)  If the input argument SCLKDP is invalid, an error is signaled */
/*         by a routine in the call tree of this routine. The output */
/*         argument ET will not be modified. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine operates on encoded SCLK values. These values */
/*     are obtained by calling the SPICELIB routine SCENCD or other */
/*     SCLK conversion routines. The advantage of encoded SCLK, as */
/*     opposed to character string representations of SCLK is that */
/*     encoded SCLK values are easy to perform arithmetic operations on. */
/*     Additionally, working with encoded SCLK reduces the overhead of */
/*     repeated conversion of character strings to integers or double */
/*     precision numbers. */

/*     To convert a string representation of an SCLK value to ET, use */
/*     the SPICELIB routine SCS2E. */

/*     See the SCLK Required Reading for a list of the entire set of */
/*     SCLK conversion routines. */

/* $ Examples */

/*     1)  Encode a Galileo SCLK string, and convert the encoded value */
/*         to ET; use these time values to look up both GLL orbiter */
/*         scan platform's pointing and the GLL--Earth state vector */
/*         for an epoch specified by an SCLK string. */

/*            During program initialization, load the leapseconds and */
/*            SCLK kernels. We will pretend that these files are named */
/*            "LEAPSECONDS.KER" and "GLLSCLK.KER".  To use this code */
/*            fragment, you must substitute the actual names of these */
/*            kernel files for the names used here. */

/*               C */
/*               C     Load leapseconds and SCLK kernels: */
/*               C */
/*                     CALL FURNSH ( 'LEAPSECONDS.KER' ) */
/*                     CALL FURNSH ( 'SCLK.KER'        ) */

/*            The mission is Galileo, which has spacecraft ID -77. */
/*            Let's assume that the SCLK string is */

/*                     1 / 1900000:00:00 */

/*            The number 1, followed by a slash, indicates that the */
/*            epoch is in the first partition. */

/*            The next step is to encode this SCLK string, and also */
/*            find the corresponding ET value: */

/*                     CALL SCENCD ( -77, '1/ 1900000:00:00', SCLKDP ) */
/*                     CALL SCT2E  ( -77,  SCLKDP,            ET     ) */

/*            We'll assume that you've already loaded SPK and CK files */
/*            containing ephemeris data for the GLL orbiter and the */
/*            Earth, as well as scan platform pointing.  Now you're */
/*            ready to call both CKGP, which expects the input epoch to */
/*            be specified by an encoded SCLK string, and SPKEZ, which */
/*            expects the epoch to be specified as an ephemeris time. */

/*               C */
/*               C     Find scan platform pointing CMAT and s/c--target */
/*               C     vector (first 3 components of STATE) at epoch. */
/*               C     We assume that CK and SPK kernels have been loaded */
/*               C     already, via CKLPF and SPKLEF respectively. */
/*               C */
/*                     SCANPL = -77001 */
/*                     EARTH  =    399 */

/*                     CALL CKGP  ( SCANPL, */
/*                    .             SCLKDP, */
/*                    .             TOL, */
/*                    .             REFSYS, */
/*                    .             CMAT, */
/*                    .             CLKOUT, */
/*                    .             FOUND   ) */

/*                     IF ( .NOT. FOUND ) THEN */

/*                        [ Indicate to user that pointing was not */
/*                          available ] */

/*                     END IF */


/*                     CALL SPKEZ ( EARTH, */
/*                    .             ET, */
/*                    .             REFSYS, */
/*                    .             CORR, */
/*                    .             -77, */
/*                    .             STATE, */
/*                    .             LT      ) */



/*     2)  Convert an encoded Voyager 2 SCLK value to UTC, using calendar */
/*         format, with 3 digits of precision. */

/*            Again, your initialization code must load the leapseconds */
/*            and SCLK kernels: */

/*               C */
/*               C     Load leapseconds and SCLK kernels: */
/*               C */
/*                     CALL FURNSH ( 'LEAPSECONDS.KER' ) */
/*                     CALL FURNSH ( 'VGR2SCLK.KER'    ) */


/*            To find the UTC value corresponding to the encoded */
/*            Voyager 2 SCLK value SCLKDP, you can use the code fragment */

/*                     CALL SCT2E  ( -32,  SCLKDP,     ET  ) */
/*                     CALL ET2UTC (  ET,  'C',    3,  UTC ) */

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
/*     W.L. Taber         (JPL) */
/*     E.D. Wright        (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 06-JUL-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. Removed */
/*        unnecessary $Revisions section. */

/*        Moved the required readings present in $Literature_References */
/*        section to $Required_Reading. */

/* -    SPICELIB Version 1.0.4, 22-AUG-2006 (EDW) */

/*        Replaced references to LDPOOL with references */
/*        to FURNSH. */

/* -    SPICELIB Version 1.0.3, 09-MAR-1999 (NJB) */

/*        Explicit list of SCLK conversion routines in $Particulars */
/*        section has been replaced by a pointer to the SCLK Required */
/*        Reading. */

/* -    SPICELIB Version 1.0.2, 10-APR-1992 (NJB) (WLT) */

/*        The $Brief_I/O section now lists ET correctly as an output */
/*        from this routine. Header was updated to reflect possibility */
/*        of needing to load a leapseconds kernel before calling this */
/*        routine. Comment section for permuted index source lines was */
/*        added following the header. */

/* -    SPICELIB Version 1.0.1, 12-OCT-1990 (NJB) */

/*        $Restrictions section no longer states that you must load the */
/*        leapseconds kernel prior to calling this routine. */

/*        The examples have been slightly re-written. In particular, */
/*        they no longer use calls to CLPOOL. */

/* -    SPICELIB Version 1.0.0, 03-SEP-1990 (NJB) */

/* -& */
/* $ Index_Entries */

/*     spacecraft_clock ticks to ephemeris time */

/* -& */

/*     SPICELIB functions */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("SCT2E", (ftnlen)5);
    }

/*     Just hand off the conversion to the appropriate routine. */

    if (sctype_(sc) == 1) {
	scte01_(sc, sclkdp, et);
    } else {
	setmsg_("Clock type # is not supported.", (ftnlen)30);
	i__1 = sctype_(sc);
	errint_("#", &i__1, (ftnlen)1);
	sigerr_("SPICE(NOTSUPPORTED)", (ftnlen)19);
	chkout_("SCT2E", (ftnlen)5);
	return 0;
    }
    chkout_("SCT2E", (ftnlen)5);
    return 0;
} /* sct2e_ */

