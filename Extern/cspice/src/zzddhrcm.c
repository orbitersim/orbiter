/* zzddhrcm.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure ZZDDHRCM ( Private --- DDH Request Count ) */
/* Subroutine */ int zzddhrcm_(integer *nut, integer *utcst, integer *reqcnt)
{
    /* System generated locals */
    integer i__1, i__2, i__3;

    /* Local variables */
    integer i__;
    extern integer intmax_(void);

/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines.  Users should not call this routine directly due */
/*     to the volatile nature of this routine. */

/*     Manage augmentation of the handle to logical unit request counter */
/*     and the cost column in the unit table. */

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
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     NUT        I   Number of entries in the unit table. */
/*     UTCST     I/O  Cost column of the unit table. */
/*     REQCNT    I/O  Value of the HLU request counter. */

/* $ Detailed_Input */

/*     NUT        is the number of entries in the unit table. */

/*     UTCST      is the current cost column of the unit table. */

/*     REQCNT     is the current value of the HLU request counter to */
/*                adjust. */

/* $ Detailed_Output */

/*     UTCST      is the updated cost column of the unit table.  In */
/*                the nominal case, UTCST will not be adjusted, but */
/*                if REQCNT overflows, then adjustments will be made */
/*                to approximately preserve the priority. */

/*     REQCNT     is the updated value of the request counter. */
/*                Nominally this will be 1 more than the input */
/*                value.  However, in the case where REQCNT will */
/*                exceed INTMAX it will be assigned to INTMAX()/2 + 1. */

/* $ Parameters */

/*     None. */

/* $ Files */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/*     1) If REQCNT on input is INTMAX(), then REQCNT on output will */
/*        be assigned INTMAX()/2 + 1 and the UTCST column will be */
/*        recomputed. */

/* $ Particulars */

/*     This module manages the request counter and the cost column of */
/*     the unit table which is used to determine the expense of */
/*     disconnecting a handle from its logical unit. */

/*     In the nominal mode of operation, the request counter is simply */
/*     incremented by one and the cost column remains untouched. */
/*     However, when the request counter passed into the routine is */
/*     INTMAX, then REQCNT is not incremented.  In an attempt to preserve */
/*     the relationships between costs, all entries in the cost column */
/*     are halved and REQCNT is set to INTMAX()/2 + 1.  This has the */
/*     effect of preserving the cost relationships between rows, except */
/*     in half the cases where subsequent cost values are present. */

/*     The occurrence of rollover is rare, and thus the destruction of */
/*     relative cost relationships as well. */

/* $ Examples */

/*     See ZZDDHHLU for sample usage. */

/* $ Restrictions */

/*     None. */

/* $ Author_and_Institution */

/*     F.S. Turner     (JPL) */

/* $ Literature_References */

/*     None. */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 04-OCT-2001 (FST) */


/* -& */

/*     SPICELIB Functions */


/*     Local Variables */


/*     Check to see if REQCNT is INTMAX, otherwise just increment */
/*     REQCNT. */

    if (*reqcnt == intmax_()) {
	*reqcnt = intmax_() / 2 + 1;
	i__1 = *nut;
	for (i__ = 1; i__ <= i__1; ++i__) {
/* Computing MAX */
	    i__2 = 1, i__3 = utcst[i__ - 1] / 2;
	    utcst[i__ - 1] = max(i__2,i__3);
	}
    } else {
	++(*reqcnt);
    }
    return 0;
} /* zzddhrcm_ */

