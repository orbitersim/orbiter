/* xposbl.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure XPOSBL ( Transpose a matrix by blocks    ) */
/* Subroutine */ int xposbl_(doublereal *bmat, integer *nrow, integer *ncol, 
	integer *bsize, doublereal *btmat)
{
    /* System generated locals */
    integer bmat_dim1, bmat_dim2, bmat_offset, btmat_dim1, btmat_dim2, 
	    btmat_offset, i__1, i__2, i__3, i__4, i__5, i__6, i__7, i__8;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer);

    /* Local variables */
    doublereal temp;
    integer i__, j;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    integer cb, rb;
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), setmsg_(char *, ftnlen), errint_(char *, integer *, 
	    ftnlen);

/* $ Abstract */

/*     Transpose the square blocks within a matrix. */

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

/*     MATH */
/*     MATRIX */
/*     TRANSFORMATION */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     BMAT       I   a matrix composed of square block submatrices */
/*     NROW       I   the number of rows in the matrix BMAT */
/*     NCOL       I   the number of columns in the matrix BMAT */
/*     BSIZE      I   the size of the square blocks in BMAT */
/*     BTMAT      O   the result of transposing the blocks of BMAT */

/* $ Detailed_Input */

/*     NROW     is the number of rows in the input matrix. */

/*     NCOL     is the number of columns in the input matrix. */

/*     BSIZE    is the number of rows and columns in each block */
/*              of the input matrix. */

/*     BMAT     is a block structured matrix. In other words */
/*              it looks like: */


/*                 -                                - */
/*                |      :      :       :     :      | */
/*                |  B   :  B   :   B   :     :  B   | */
/*                |   11 :   12 :    13 : ... :   1C | */
/*                |......:......:.......:     :......| */
/*                |      :      :       :     :      | */
/*                |  B   :  B   :   B   :     :  B   | */
/*                |   21 :   22 :    23 : ... :   2C | */
/*                |......:......:.......:     :......| */
/*                |      .                           | */
/*                |      .                           | */
/*                |      .                           | */
/*                |......................     .......| */
/*                |      :      :       :     :      | */
/*                |  B   :  B   :   B   :     :  B   | */
/*                |   R1 :   R2 :    R3 : ... :   RC | */
/*                |......:......:.......:     :......| */
/*                 -                                - */

/*              where each B   is a square matrix of BSIZE rows and */
/*                          ij */
/*              and columns. */

/* $ Detailed_Output */

/*     BTMAT    is the matrix obtained from BMAT when each of its */
/*              blocks is transposed. Given the description of */
/*              BMAT above, BTMAT looks like: */


/*                  -                                - */
/*                 |   t  :   t  :    t  :     :   t  | */
/*                 |  B   :  B   :   B   :     :  B   | */
/*                 |   11 :   12 :    13 : ... :   1C | */
/*                 |......:......:.......:     :......| */
/*                 |      :      :       :     :      | */
/*                 |   t  :   t  :    t  :     :   t  | */
/*                 |  B   :  B   :   B   :     :  B   | */
/*                 |   21 :   22 :    23 : ... :   2C | */
/*                 |......:......:.......:     :......| */
/*                 |      .                           | */
/*                 |      .                           | */
/*                 |      .                           | */
/*                 |......................     .......| */
/*                 |      :      :       :     :      | */
/*                 |   t  :   t  :    t  :     :   t  | */
/*                 |  B   :  B   :   B   :     :  B   | */
/*                 |   R1 :   R2 :    R3 : ... :   RC | */
/*                 |......:......:.......:     :......| */
/*                  -                                - */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the number of rows input is not positive, the error */
/*         SPICE(BADROWCOUNT) is signaled. */

/*     2)  If the number of columns input is not positive, the error */
/*         SPICE(BADCOLUMNCOUNT) is signaled. */

/*     3)  If the block size input is not positive, the error */
/*         SPICE(BADBLOCKSIZE) is signaled. */

/*     4)  If BMAT cannot be partitioned into an integer number of */
/*         blocks, the error SPICE(BLOCKSNOTEVEN) is signaled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine transposes the blocks of a block structured */
/*     matrix. This operation is valuable, as it is a means */
/*     for computing the inverse of a state transformation matrix */
/*     (see the example below). */

/* $ Examples */

/*     The following code fragment illustrates how you would convert */
/*     a state relative to earth-fixed coordinates to J2000 coordinates. */

/*     C */
/*     C     We want to state earthfixed coordinates (399) to J2000 */
/*     C     coordinates */
/*     C */
/*           BODY = 399 */
/*           REF  = 'J2000' */

/*     C */
/*     C     Get the 6 by 6 state transformation matrix from J2000 */
/*     C     coordinates to earthfixed coordinates. */
/*     C */
/*           CALL TISBOD ( REF, BODY, ET, TISPM ) */

/*     C */
/*     C     The inverse of TISPM can be obtained by transposing the */
/*     C     3 by 3 blocks of the 6 by 6 matrix TISPM. */
/*     C */
/*           CALL XPOSBL ( TISPM, 6, 6, 3, TSPMI ) */


/*     C */
/*     C     Now transform the earthfixed state (ESTATE) to the */
/*     C     inertial state (ISTATE). */
/*     C */
/*           CALL MXVG   ( TSPMI, ESTATE, 6, 6, ISTATE ) */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 16-AUG-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. Updated */
/*        $Exceptions section. */

/* -    SPICELIB Version 1.0.2, 22-APR-2010 (NJB) */

/*        Header correction: assertions that the output */
/*        can overwrite the input have been removed. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 05-NOV-1990 (WLT) */

/* -& */
/* $ Index_Entries */

/*     transpose a matrix by blocks */

/* -& */

/*     Local variables */


/*     Ok. Here's what's going to happen. */

/*     The matrix has the form: */

/*       -                                - */
/*      |      :      :       :     :      | */
/*      |  B   :  B   :   B   :     :  B   | */
/*      |   11 :   12 :    13 : ... :   1C | */
/*      |......:......:.......:     :......| */
/*      |      :      :       :     :      | */
/*      |  B   :  B   :   B   :     :  B   | */
/*      |   21 :   22 :    23 : ... :   2C | */
/*      |......:......:.......:     :......| */
/*      |      .                           | */
/*      |      .                           | */
/*      |      .                           | */
/*      |......................     .......| */
/*      |      :      :       :     :      | */
/*      |  B   :  B   :   B   :     :  B   | */
/*      |   R1 :   R2 :    R3 : ... :   RC | */
/*      |......:......:.......:     :......| */

/*      Where each block B   is a square matrix. */
/*                        ij */

/*      All we really need to do is figure out how to transpose any */
/*      of the blocks.  Once that is done we can just cycle over */
/*      all of the blocks in the matrix. */

/*      So what does the ij block look like? Well, this is it. */


/*             a              a               ...    a */
/*              RB+1 CB+1      RB+1 CB+2              RB+1 CB+BSIZE */

/*             a              a               ...    a */
/*              RB+2 CB+1      RB+2 CB+2              RB+2 CB+BSIZE */

/*             a              a               ...    a */
/*              RB+3 CB+1      RB+3 CB+2              RB+3 CB+BSIZE */

/*                                 . */
/*                                 . */
/*                                 . */

/*              a              a                 ... a */
/*               RB+BSIZE CB+1  RB+BSIZE CB+2         RB+BSIZE CB+BSIZE */


/*     where RB = (i-1)*BSIZE, and CB = (j-1)*BSIZE.  But inspection of */
/*     this block shows that to transpose it we simply need to swap */
/*     the entries */

/*                a           and  a */
/*                 RB+m CB+n        RB+n CB+m */

/*     where m and n range over all integers from 1 to BSIZE. */


/*     Let's first check to make sure that the requested operation */
/*     makes sense.  Are all of the integers positive? */

    /* Parameter adjustments */
    btmat_dim1 = *nrow;
    btmat_dim2 = *ncol;
    btmat_offset = btmat_dim1 + 1;
    bmat_dim1 = *nrow;
    bmat_dim2 = *ncol;
    bmat_offset = bmat_dim1 + 1;

    /* Function Body */
    if (*bsize < 1) {
	chkin_("XPOSBL", (ftnlen)6);
	setmsg_("The block size is not positive. The block size is #.", (
		ftnlen)52);
	errint_("#", bsize, (ftnlen)1);
	sigerr_("SPICE(BADBLOCKSIZE)", (ftnlen)19);
	chkout_("XPOSBL", (ftnlen)6);
	return 0;
    }
    if (*nrow < 1) {
	chkin_("XPOSBL", (ftnlen)6);
	setmsg_("The number of rows in the matrix is not positive. The numbe"
		"r of rows is #.", (ftnlen)74);
	errint_("#", nrow, (ftnlen)1);
	sigerr_("SPICE(BADROWCOUNT)", (ftnlen)18);
	chkout_("XPOSBL", (ftnlen)6);
	return 0;
    }
    if (*ncol < 1) {
	chkin_("XPOSBL", (ftnlen)6);
	setmsg_("The number of columns in the matrix is not positive. The nu"
		"mber of columns is #.", (ftnlen)80);
	errint_("#", ncol, (ftnlen)1);
	sigerr_("SPICE(BADCOLUMNCOUNT)", (ftnlen)21);
	chkout_("XPOSBL", (ftnlen)6);
	return 0;
    }

/*     Is there a whole number of blocks in the matrix. */

    if (*ncol % *bsize != 0 || *nrow % *bsize != 0) {
	chkin_("XPOSBL", (ftnlen)6);
	setmsg_("The block size does not evenly divide both the number of ro"
		"ws and the number of columns. The block size is #; the numbe"
		"r of rows is #; the number of columns is #. ", (ftnlen)163);
	errint_("#", bsize, (ftnlen)1);
	errint_("#", nrow, (ftnlen)1);
	errint_("#", ncol, (ftnlen)1);
	sigerr_("SPICE(BLOCKSNOTEVEN)", (ftnlen)20);
	chkout_("XPOSBL", (ftnlen)6);
	return 0;
    }

/*     If we get to this point we are ready to do the transposes. */
/*     Cycle over all of the blocks in the matrix. */

    i__1 = *ncol - 1;
    i__2 = *bsize;
    for (cb = 0; i__2 < 0 ? cb >= i__1 : cb <= i__1; cb += i__2) {
	i__3 = *nrow - 1;
	i__4 = *bsize;
	for (rb = 0; i__4 < 0 ? rb >= i__3 : rb <= i__3; rb += i__4) {

/*           OK. Transpose block ( RB, CB ). */

	    i__5 = *bsize;
	    for (i__ = 1; i__ <= i__5; ++i__) {
		i__6 = i__;
		for (j = 1; j <= i__6; ++j) {
		    if (i__ == j) {
			btmat[(i__7 = rb + i__ + (cb + j) * btmat_dim1 - 
				btmat_offset) < btmat_dim1 * btmat_dim2 && 0 
				<= i__7 ? i__7 : s_rnge("btmat", i__7, "xpos"
				"bl_", (ftnlen)375)] = bmat[(i__8 = rb + i__ + 
				(cb + j) * bmat_dim1 - bmat_offset) < 
				bmat_dim1 * bmat_dim2 && 0 <= i__8 ? i__8 : 
				s_rnge("bmat", i__8, "xposbl_", (ftnlen)375)];
		    } else {
			temp = bmat[(i__7 = rb + i__ + (cb + j) * bmat_dim1 - 
				bmat_offset) < bmat_dim1 * bmat_dim2 && 0 <= 
				i__7 ? i__7 : s_rnge("bmat", i__7, "xposbl_", 
				(ftnlen)377)];
			btmat[(i__7 = rb + i__ + (cb + j) * btmat_dim1 - 
				btmat_offset) < btmat_dim1 * btmat_dim2 && 0 
				<= i__7 ? i__7 : s_rnge("btmat", i__7, "xpos"
				"bl_", (ftnlen)378)] = bmat[(i__8 = rb + j + (
				cb + i__) * bmat_dim1 - bmat_offset) < 
				bmat_dim1 * bmat_dim2 && 0 <= i__8 ? i__8 : 
				s_rnge("bmat", i__8, "xposbl_", (ftnlen)378)];
			btmat[(i__7 = rb + j + (cb + i__) * btmat_dim1 - 
				btmat_offset) < btmat_dim1 * btmat_dim2 && 0 
				<= i__7 ? i__7 : s_rnge("btmat", i__7, "xpos"
				"bl_", (ftnlen)379)] = temp;
		    }
		}
	    }
	}
    }
    return 0;
} /* xposbl_ */

