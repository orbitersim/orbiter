/* zzck4i2d.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure      ZZCK4I2D ( Pack set of integers into a single DP ) */
/* Subroutine */ int zzck4i2d_(integer *i__, integer *nsets, doublereal *
	parcod, doublereal *dpcoef)
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    integer k;
    doublereal x;

/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines. Users should not call this routine directly due */
/*     to the volatile nature of this routine. */

/*     This is the routine that packs a set integer numbers into a */
/*     single double precision number. */

/*     Its current specific use is to "compress" seven integer numbers */
/*     representing numbers of polynomial coefficients in a logical */
/*     type 4 CK record into a single DP number stored in a physical */
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
/*     I          I   Array of NSETS integer components. */
/*     NSETS      I   Number of integer components in input array I. */
/*     PARCOD     I   Packing base. */
/*     DPCOEF     O   DP number containing NSETS packed integer numbers. */

/* $ Detailed_Input */

/*     I          is an array containing integers to be packed. */

/*     NSETS      is the number of elements in the array I. */

/*     PARCOD     is the packing base. */

/* $ Detailed_Output */

/*     DPCOEF     is a DP number containing elements of the input */
/*                array packed together. */

/* $ Parameters */

/*     None. */

/* $ Files */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/*     No checking is done to determine whether elements of the array */
/*     I are within range [0:PARCOD-1] and whether PARCOD**NSETS will */
/*     cause DPCOEF mantissa overflow. */

/* $ Particulars */

/*     This routine packs NSETS elements of the array I into a single */
/*     double precision variable using base specified by PARCOD. When */
/*     packed the double precision number DPCOEF represents NSETS of */
/*     integer elements of the array I as follows: */

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

/*     1) No checking is done to determine whether elements of the */
/*        array I are within range [0:PARCOD-1] to prevent "overflow" */
/*        of particular */

/*     2) No checking is done to determine whether PARCOD**NSETS */
/*        will cause DPCOEF mantissa overflow. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     Y.K. Zaiko     (JPL) */
/*     B.V. Semenov   (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 05-MAY-1999 (YKZ) (BVS) */

/* -& */

/*     Local variables */


/*     Let's pack it! */

    *dpcoef = 0.;
    x = 1.;
    i__1 = *nsets;
    for (k = 1; k <= i__1; ++k) {
	*dpcoef += i__[k - 1] * x;
	x *= *parcod;
    }

/*     All done. */

    return 0;
} /* zzck4i2d_ */

