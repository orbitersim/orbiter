/* scs2e.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure SCS2E ( SCLK string to ET ) */
/* Subroutine */ int scs2e_(integer *sc, char *sclkch, doublereal *et, ftnlen 
	sclkch_len)
{
    extern /* Subroutine */ int sct2e_(integer *, doublereal *, doublereal *),
	     chkin_(char *, ftnlen), scencd_(integer *, char *, doublereal *, 
	    ftnlen);
    doublereal sclkdp;
    extern /* Subroutine */ int chkout_(char *, ftnlen);
    extern logical return_(void);

/* $ Abstract */

/*     Convert a spacecraft clock string to ephemeris seconds past */
/*     J2000 (ET). */

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
/*     SC         I   NAIF integer code for a spacecraft. */
/*     SCLKCH     I   An SCLK string. */
/*     ET         O   Ephemeris time, seconds past J2000. */

/* $ Detailed_Input */

/*     SC       is a NAIF ID code for a spacecraft, one of whose */
/*              clock values is represented by SCLKCH. The set of */
/*              supported spacecraft clocks is listed in the SCLK */
/*              Required Reading. */

/*     SCLKCH   is a character string representation of the */
/*              spacecraft clock value that corresponds to ET, for */
/*              the spacecraft specified by the input argument SC. */
/*              SCLKCH is an absolute spacecraft clock time, so */
/*              partition information should be included in this */
/*              string. The precise format of SCLKCH is specified */
/*              in the SCLK Required Reading. */

/* $ Detailed_Output */

/*     ET       is the epoch, specified as ephemeris seconds past */
/*              J2000, that corresponds to SCLKCH. */

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

/*     3)  If the value of SCLKCH is invalid, an error is signaled by a */
/*         routine in the call tree of this routine. The output argument */
/*         ET will not be modified. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine is provided as a convenience; it is simply shorthand */
/*     for the code fragment */

/*        CALL SCENCD ( SC, SCLKCH, SCLKDP ) */
/*        CALL SCT2E  ( SC, SCLKDP, ET     ) */

/*     See the SCLK Required Reading for a list of the entire set of */
/*     SCLK conversion routines. */

/* $ Examples */

/*     1)  Find the state (position and velocity) of Jupiter, as seen */
/*         from the Galileo spacecraft, at the epoch corresponding to */
/*         the SCLK value */

/*            2 / 3110578:89:09 */

/*         The digit `2', followed by the forward slash, indicates that */
/*         the time value is in the second mission partition. */


/*            During program initialization, load the leapseconds and */
/*            SCLK kernels. We will pretend that these files are named */
/*            "LEAPSECONDS.KER" and "GLLSCLK.KER".  To use this code */
/*            fragment, you must substitute the actual names of these */
/*            kernel files for the names used here. */

/*               C */
/*               C     Load leapseconds and SCLK kernels: */
/*               C */
/*                     CALL FURNSH ( 'LEAPSECONDS.KER' ) */
/*                     CALL FURNSH ( 'GLLSCLK.KER'     ) */

/*               C */
/*               C     Load an SPK file (again, a fictitious file) */
/*               C     containing an ephemeris for Jupiter and the */
/*               C     GLL orbiter's trajectory. */
/*               C */
/*                     CALL SPKLEF ( 'GLLSPK.KER', HANDLE ) */

/*            The Galileo spacecraft ID is -77.  Convert our SCLK */
/*            string to ephemeris seconds past J2000, which is the */
/*            time representation expected by SPKEZ. */

/*                     CALL SCS2E ( -77, '2 / 3110578:89:09', ET ) */


/*            Find the state of Jupiter (body 599) as seen from Galileo */
/*            at time ET.  To use SPKEZ, you must first load an SPK */
/*            kernel, using the routine SPKLEF. */

/*                     CALL SPKEZ ( 599, */
/*                    .             ET, */
/*                    .             REFSYS, */
/*                    .             CORR, */
/*                    .             -77, */
/*                    .             STATE, */
/*                    .             LT      ) */



/*     2)  Convert a Voyager 2 SCLK value to UTC, using calendar format, */
/*         with 3 digits of precision in the seconds component. */

/*            Again, your initialization code must load the leapseconds */
/*            and SCLK kernels: */

/*               C */
/*               C     Load leapseconds and SCLK kernels: */
/*               C */
/*                     CALL FURNSH ( 'LEAPSECONDS.KER' ) */
/*                     CALL FURNSH ( 'VGR2SCLK.KER'    ) */


/*            To find the UTC value corresponding to Voyager 2 SCLK */
/*            string */

/*                     11389.20.768 */

/*            you can use the code fragment */

/*                     CALL SCS2E  ( -32,  '11389.29.768',  ET  ) */
/*                     CALL ET2UTC (  ET,  'C',      3,     UTC ) */

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
/*        section to $Required_Reading and added CK to the list. */

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

/*     spacecraft_clock string to ephemeris time */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("SCS2E", (ftnlen)5);
    }

/*     Encode SCLKCH, and convert the result to ET. */

    scencd_(sc, sclkch, &sclkdp, sclkch_len);
    sct2e_(sc, &sclkdp, et);
    chkout_("SCS2E", (ftnlen)5);
    return 0;
} /* scs2e_ */

