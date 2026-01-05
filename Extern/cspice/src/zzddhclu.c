/* zzddhclu.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure ZZDDHCLU ( Private --- DDH Count Locks ) */
integer zzddhclu_(logical *utlck, integer *nut)
{
    /* System generated locals */
    integer ret_val, i__1;

    /* Local variables */
    integer i__;

/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines.  Users should not call this routine directly due */
/*     to the volatile nature of this routine. */

/*     Retrieve the number of locked units in the unit table. */

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
/*     UTLCK      I   Lock column of the unit table. */
/*     NUT        I   Number of entries in the unit table. */

/*     This function return the number of locked units in the unit table. */

/* $ Detailed_Input */

/*     UTLCK      is the lock column of the unit table.  TRUE entries in */
/*                this column indicate a handle is locked to a particular */
/*                unit. */

/*     NUT        is the number of entries in the unit table. */

/* $ Detailed_Output */

/*     The function returns the number of locked units in the unit table. */

/* $ Parameters */

/*     None. */

/* $ Files */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/* $ Particulars */

/*     This routine simply encapsulates some logic used in several */
/*     places in ZZDDHMAN. */

/* $ Examples */

/*     See ZZDDHMAN for sample usage. */

/* $ Restrictions */

/*     None. */

/* $ Author_and_Institution */

/*     F.S. Turner     (JPL) */

/* $ Literature_References */

/*     None. */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 04-OCT-2001 (FST) */


/* -& */

/*     Local Variables */


/*     Loop through UTLCK counting the number of TRUE values. */

    ret_val = 0;
    i__1 = *nut;
    for (i__ = 1; i__ <= i__1; ++i__) {
	if (utlck[i__ - 1]) {
	    ++ret_val;
	}
    }
    return ret_val;
} /* zzddhclu_ */

