/* zzck4d2i.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure ZZCK4D2I ( Unpack a set of integers from DP number ) */
/* Subroutine */ int zzck4d2i_(doublereal *dpcoef, integer *nsets, doublereal 
	*parcod, integer *i__)
{
    /* System generated locals */
    integer i__1;

    /* Builtin functions */
    double pow_di(doublereal *, integer *);

    /* Local variables */
    integer k;
    doublereal x;

/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines. Users should not call this routine directly due */
/*     to the volatile nature of this routine. */

/*     This is the routine that unpacks a set integer numbers stored in */
/*     a single double precision number. */

/*     Its current specific use is to "uncompress" seven integer numbers */
/*     representing numbers of polynomial coefficients in a logical */
/*     type 4 CK record from a single DP number stored in a physical */
/*     type 4 CK record in a file. */

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

/*     None. */

/* $ Declarations */
/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     DPCOEF     I   DP number containing packed integer numbers. */
/*     NSETS      I   Number of integer components packed in DPCOEF. */
/*     PARCOD     I   Packing base. */
/*     I          O   Array of NSETS integer components. */

/* $ Detailed_Input */

/*     DPCOEF     is a DP number containing NSETS integers packed */
/*                together. */

/*     NSETS      is the number of integers packed in the DPCOEF. */

/*     PARCOD     is the packing base. */

/* $ Detailed_Output */

/*     I          is an array containing unpacked integers. */

/* $ Parameters */

/*     None. */

/* $ Files */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/* $ Particulars */

/*     This routine unpacks NSETS integers packed in a single double */
/*     precision number using base specified by PARCOD and stored them */
/*     in the array I. The integers are packed in the DP using the */
/*     following algorithm: */

/*        [DPCOEF]= PARCOD ** ( NSETS - 1 ) * I( 1 )        + */
/*                  PARCOD ** ( NSETS - 2 ) * I( 2 )        + */
/*                  ... */
/*                  PARCOD ** 1             * I( NSETS - 1 )+ */
/*                  PARCOD ** 0             * I( NSETS ) */
/*     where: */

/*        I(1:NSETS) - is an array of integer numbers with values */
/*                     in the range [0:PARCOD-1]. */

/* $ Examples */

/*     None. */

/* $ Restrictions */

/*     Output integer array I must have enough space to hold NSETS */
/*     numbers. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     Y.K. Zaiko     (JPL) */
/*     B.V. Semenov   (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 05-MAY-1999 (YKZ) (BVS) */

/* -& */

/*     Local variables. */


/*     Let's unpack it! */

    i__1 = *nsets - 1;
    x = pow_di(parcod, &i__1);
    i__1 = *nsets - 1;
    for (k = 0; k <= i__1; ++k) {
	i__[*nsets - k - 1] = (integer) (*dpcoef / x);
	*dpcoef -= i__[*nsets - k - 1] * x;
	x /= *parcod;
    }

/*     All done. */

    return 0;
} /* zzck4d2i_ */

