/* zzgfrrq.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure ZZGFRRQ ( Private - GF, range rate between objects ) */
/* Subroutine */ int zzgfrrq_(doublereal *et, integer *targ, integer *obs, 
	char *abcorr, doublereal *value, ftnlen abcorr_len)
{
    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    doublereal state[6];
    extern /* Subroutine */ int spkez_(integer *, doublereal *, char *, char *
	    , integer *, doublereal *, doublereal *, ftnlen, ftnlen);
    extern logical failed_(void);
    doublereal lt;
    extern /* Subroutine */ int chkout_(char *, ftnlen);
    extern doublereal dvnorm_(doublereal *);
    extern logical return_(void);
    char ref[5];

/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines.  Users should not call this routine directly due */
/*     to the volatile nature of this routine. */

/*     Compute the apparent range rate between two ephemeris objects. */

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

/*     RANGE RATE */
/*     EPHEMERIS */
/*     GEOMETRY */
/*     SEARCH */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     ET         I   Ephemeris seconds past J2000 TDB */
/*     TARG       I   Target body ID */
/*     OBS        I   Observer body ID */
/*     ABCORR     I   Aberration correction flag */
/*     REF        I   Reference frame of the range rate */
/*     VALUE      O   Value of range rate between objects */

/* $ Detailed_Input */

/*     ET       is the time in ephemeris seconds past J2000 TDB at */
/*              which the range rate is to be measured. */

/*     TARG     the SPICE integer ID for the target body. */

/*     OBS      the SPICE integer ID for the observer. */

/*     ABCORR   the string description of the aberration corrections to */
/*              apply to the state evaluations to account for one-way */
/*              light time and stellar aberration. */

/*              Any aberration correction accepted by the SPICE */
/*              routine SPKEZR is accepted here. See the header */
/*              of SPKEZR for a detailed description of the */
/*              aberration correction options. For convenience, */
/*              the options are listed below: */

/*                 'NONE'     Apply no correction. Returns the "true" */
/*                            geometric state. */

/*                 'LT'       "Reception" case:  correct for */
/*                            one-way light time using a Newtonian */
/*                            formulation. */

/*                 'LT+S'     "Reception" case:  correct for */
/*                            one-way light time and stellar */
/*                            aberration using a Newtonian */
/*                            formulation. */

/*                 'CN'       "Reception" case:  converged */
/*                            Newtonian light time correction. */

/*                 'CN+S'     "Reception" case:  converged */
/*                            Newtonian light time and stellar */
/*                            aberration corrections. */

/*                 'XLT'      "Transmission" case:  correct for */
/*                            one-way light time using a Newtonian */
/*                            formulation. */

/*                 'XLT+S'    "Transmission" case:  correct for */
/*                            one-way light time and stellar */
/*                            aberration using a Newtonian */
/*                            formulation. */

/*                 'XCN'      "Transmission" case:  converged */
/*                            Newtonian light time correction. */

/*                 'XCN+S'    "Transmission" case:  converged */
/*                            Newtonian light time and stellar */
/*                            aberration corrections. */

/*              The ABCORR string lacks sensitivity to case, leading */
/*              and trailing blanks. */

/* $ Detailed_Output */

/*     VALUE    is the optionally light-time corrected range */
/*              rate of TARG observed from OBS. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     None. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine determines the apparent range rate of a target, */
/*     TARG, as seen from an observer, OBS, at epoch ET. */

/* $ Examples */

/*     None. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     W.L. Taber     (JPL) */
/*     L.S. Elson     (JPL) */

/* $ Version */

/* -    SPICELIB version 1.0.1 08-JUL-2010 (EDW) */

/*        Minor typo correction to comments. */

/* -    SPICELIB version 1.0.0 09-JUN-2009 (NJB)(EDW) */

/* -& */
/* $ Index_Entries */

/*   compute the range rate between two objects. */

/* -& */

/*     SPICELIB functions. */


/*     Local Variables. */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("ZZGFRRQ", (ftnlen)7);
    }

/*     We just want the range rate of TARG relative to OBS. */
/*     This calculation is invariant with respect to reference */
/*     frame; we use 'J2000'. */

    s_copy(ref, "J2000", (ftnlen)5, (ftnlen)5);
    spkez_(targ, et, ref, abcorr, obs, state, &lt, (ftnlen)5, abcorr_len);
    if (failed_()) {
	chkout_("ZZGFRRQ", (ftnlen)7);
	return 0;
    }

/*     Calculate the derivative from the STATE vector. */

    *value = dvnorm_(state);

/*     All done. */

    chkout_("ZZGFRRQ", (ftnlen)7);
    return 0;
} /* zzgfrrq_ */

