/* mequg.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure MEQUG  ( Matrix equal to another, general dimension ) */
/* Subroutine */ int mequg_(doublereal *m1, integer *nr, integer *nc, 
	doublereal *mout)
{
    /* System generated locals */
    integer m1_dim1, m1_offset, mout_dim1, mout_offset, i__1;

    /* Local variables */
    extern /* Subroutine */ int moved_(doublereal *, integer *, doublereal *);

/* $ Abstract */

/*     Set one double precision matrix of arbitrary size equal to */
/*     another. */

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

/*     ASSIGNMENT */
/*     MATRIX */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     M1         I   Input matrix. */
/*     NR         I   Row dimension of M1 (and also MOUT). */
/*     NC         I   Column dimension of M1 (and also MOUT). */
/*     MOUT       O   Output matrix equal to M1. */

/* $ Detailed_Input */

/*     M1       is an arbitrary-sized double precision matrix. */
/*              There are no restrictions on what it may contain. */

/*     NR       is the number of rows in the input matrix. */

/*     NC       is the number of columns in the input matrix. */

/* $ Detailed_Output */

/*     MOUT     is a NRxNC matrix set to be equal to M1. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/*     1)  If NR < 1 or NC < 1, the elements of the matrix MOUT are not */
/*         assigned any values. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     None. */

/* $ Examples */

/*     If  M1 = | 1.0D0   2.0D0 | */
/*              |               | */
/*              | 2.0D0   4.0D0 | */
/*              |               | */
/*              | 4.0D0   6.0D0 | */

/*     the call */

/*     CALL MEQUG ( M1, 3, 2, MOUT ) */

/*     produces the matrix */

/*       MOUT = | 1.0D0   2.0D0 | */
/*              |               | */
/*              | 2.0D0   4.0D0 | */
/*              |               | */
/*              | 4.0D0   6.0D0 | */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     W.M. Owen          (JPL) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 04-JUL-2021 (JDR) */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (WMO) */

/* -& */
/* $ Index_Entries */

/*     equal to another n-dimensional matrix */

/* -& */
    /* Parameter adjustments */
    mout_dim1 = *nr;
    mout_offset = mout_dim1 + 1;
    m1_dim1 = *nr;
    m1_offset = m1_dim1 + 1;

    /* Function Body */
    i__1 = *nr * *nc;
    moved_(m1, &i__1, mout);

    return 0;
} /* mequg_ */

