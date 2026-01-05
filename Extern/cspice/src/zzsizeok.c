/* zzsizeok.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure      ZZSIZEOK ( Determine if the size of a segment is ok ) */
/* Subroutine */ int zzsizeok_(integer *size, integer *psize, integer *dsize, 
	integer *offset, logical *ok, integer *n)
{
    integer a, q, r__;
    extern /* Subroutine */ int rmaini_(integer *, integer *, integer *, 
	    integer *);
    integer pd1;

/* $ Abstract */


/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines.  Users should not call this routine directly due */
/*     to the volatile nature of this routine. */

/*     This routine exists to determine whether or not the type of */
/*     a DAF segment is compatible with the sizes allowed for SPK */
/*     type 01 segments or CK type 02 segments.  However, more generally */
/*     it determines whether or not the integer equation: */

/*         SIZE = PSIZE*N + (N-OFFSET)/DSIZE */

/*     can be satisfied for some value of N.  Moreover, if such */
/*     an N exists (there can be only one) it returns that value. */

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
/*     NUMERIC */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     SIZE       I   Left hand side of the equation in the abstract */
/*     PSIZE      I   Coefficient of N (packet size). */
/*     DSIZE      I   Divisor of N-OFFSET (directory size). */
/*     OFFSET     I   Offset used in computation of number of directories */
/*     OK         O   TRUE if a solution for N exists. */
/*     N          O   Value of N if there is a solution, 0 otherwise. */

/* $ Detailed_Input */

/*     SIZE       Constant terms in the equation given in the abstract. */
/*     PSIZE */
/*     DSIZE */

/*     OFFSET     Constant term in the equation above.  It should be */
/*                1 or 0. */

/* $ Detailed_Output */

/*     OK         is TRUE if an integer solution for N exists.  Otherwise */
/*                it is returned FALSE. */

/*     N          is the solution to the equation in the abstract */
/*                if such a solution exists.  Otherwise it is returned */
/*                with the value zero. */

/* $ Parameters */

/*     None. */

/* $ Files */

/*     None. */

/* $ Exceptions */

/*     1)  If SIZE, PSIZE, or DSIZE is less than 1, OK is set to FALSE */
/*         N is set to zero and no attempt is made at finding a */
/*         solution. */

/* $ Particulars */

/*     This routine determines whether or not the integer arithmetic */
/*     equation */

/*         SIZE = PSIZE*N + (N-1)/DSIZE */

/*     has a solution for N and if so returns the value of N. */

/*     The routine is intended for checking the sizes of segments */
/*     for SPK type 01 and CK type 02.  For SPK type 01, */

/*        SIZE   = segment size - 1 */
/*        PSIZE  = 72 */
/*        DSIZE  = 100 */
/*        OFFSET = 0 */


/*     for CK type 02, */

/*        SIZE   = segment size */
/*        PSIZE  = 10 */
/*        DSIZE  = 100 */
/*        OFFSET = 1 */

/* $ Examples */

/*     None. */

/* $ Restrictions */

/*     None. */

/* $ Author_and_Institution */

/*     W.L. Taber      (JPL) */

/* $ Literature_References */

/*     None. */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 02-DEC-1999 (WLT) */


/* -& */

/*     Here's the scoop. */

/*     Suppose N is a solution to SIZE = PSIZE*N + (N-OFFSET)/DSIZE */
/*     N can be represented uniquely as */

/*         N = q*DSIZE + r */

/*     where OFFSET <= r <= DSIZE+OFFSET-1.  Therefore there must */
/*     be values q and r such that */

/*        SIZE = PSIZE*(q*DSIZE + r ) + ( q*DSIZE + r - 1 ) / DSIZE */

/*             = PSIZE*DSIZE*q + q + PSIZE*r */

/*             = (PSIZE*DSIZE+1)*q + PSIZE*r */

/*     But SIZE can be represented uniquely as */

/*           SIZE = (PSIZE*DSIZE+1)*k + a */

/*     where  0 <= a < (PSIZE*DSIZE+1). */

/*     But   PSIZE*OFFSET < PSIZE*r < (PSIZE*DSIZE+OFFSET-1), */
/*    therefore  it must be that */

/*              SIZE mod(PSIZE*DSIZE+1) = PSIZE*r */
/*     and                            q = k */

/*     Hence, there is a solution to our equation if and only if */

/*          PSIZE divides SIZE mod(PSIZE*DSIZE+1) */
/*     and  OFFSET*PSIZE <= SIZE mod(PSIZE*DSIZE+1) */


/*     Handle the exceptional case first. */

    if (*size <= 0 || *dsize <= 0 || *psize <= 0) {
	*n = 0;
	*ok = FALSE_;
	return 0;
    }
    pd1 = *psize * *dsize + 1;
    rmaini_(size, &pd1, &q, &a);
    if (*offset * *psize > a) {
	*n = 0;
	*ok = FALSE_;
	return 0;
    }
    if (a == a / *psize * *psize) {
	r__ = a / *psize;
	*n = *dsize * q + r__;
	*ok = TRUE_;
    } else {
	*ok = FALSE_;
	*n = 0;
    }
    return 0;
} /* zzsizeok_ */

