/* vupack.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure VUPACK ( Unpack three scalar components from a vector ) */
/* Subroutine */ int vupack_(doublereal *v, doublereal *x, doublereal *y, 
	doublereal *z__)
{
/* $ Abstract */

/*     Unpack three scalar components from a vector. */

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

/*     VECTOR */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     V          I   Input 3-dimensional vector. */
/*     X, */
/*     Y, */
/*     Z          O   Scalar components of the vector. */

/* $ Detailed_Input */

/*     V        is a double precision 3-dimensional vector. */

/* $ Detailed_Output */

/*     X, */
/*     Y, */
/*     Z        are the double precision scalar components of the */
/*              vector V. The following equalities hold: */

/*                 X = V(1) */
/*                 Y = V(2) */
/*                 Z = V(3) */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     Basically, this is just shorthand notation for the common */
/*     sequence */

/*        X = V(1) */
/*        Y = V(2) */
/*        Z = V(3) */

/*     The routine is useful largely for two reasons. First, it */
/*     reduces the chance that the programmer will make a "cut and */
/*     paste" mistake, like */

/*        X = V(1) */
/*        Y = V(1) */
/*        Z = V(1) */

/*     Second, it makes conversions between equivalent units simpler, */
/*     and clearer. For instance, the sequence */

/*        X = V(1) * RPD() */
/*        Y = V(2) * RPD() */
/*        Z = V(3) * RPD() */

/*     can be replaced by the (nearly) equivalent sequence */

/*        CALL VSCLIP ( RPD(),  V    ) */
/*        CALL VUPACK ( V,   X, Y, Z ) */

/* $ Examples */

/*     The numerical results shown for this example may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */

/*     1) Suppose that you have an instrument kernel that provides, */
/*        within a single keyword, the three frequencies used by the */
/*        instrument, and that you want to use these frequencies */
/*        independently within your code. */

/*        The following code example demonstrates how to use VUPACK */
/*        to get these frequencies into independent scalar variables. */

/*        Use the kernel shown below, an IK defining the three */
/*        frequencies used by an instrument with NAIF ID -999001. */


/*           KPL/IK */

/*           File name: vupack_ex1.ti */

/*           The keyword below define the three frequencies used by a */
/*           hypothetical instrument (NAIF ID -999001). They correspond */
/*           to three filters: red, green and blue. Frequencies are */
/*           given in micrometers. */

/*           \begindata */

/*              INS-999001_FREQ_RGB   = (  0.65,  0.55, 0.475 ) */
/*              INS-999001_FREQ_UNITS = ( 'MICROMETERS'       ) */

/*           \begintext */


/*           End of IK */


/*        Example code begins here. */


/*              PROGRAM VUPACK_EX1 */
/*              IMPLICIT NONE */

/*        C */
/*        C     Local parameters. */
/*        C */
/*              CHARACTER*(*)         IKNAME */
/*              PARAMETER           ( IKNAME = 'vupack_ex1.ti' ) */

/*              CHARACTER*(*)         KEYWRD */
/*              PARAMETER           ( KEYWRD = 'INS-999001_FREQ_RGB' ) */

/*        C */
/*        C     Local variables. */
/*        C */
/*              DOUBLE PRECISION      DDATA  ( 3 ) */
/*              DOUBLE PRECISION      RED */
/*              DOUBLE PRECISION      GREEN */
/*              DOUBLE PRECISION      BLUE */

/*              INTEGER               N */

/*              LOGICAL               FOUND */

/*        C */
/*        C     Load the instrument kernel. */
/*        C */
/*              CALL FURNSH ( IKNAME ) */

/*        C */
/*        C     Get the frequency data from the kernel pool. */
/*        C */
/*              CALL GDPOOL ( KEYWRD, 1, 3, N, DDATA, FOUND ) */

/*              IF ( FOUND ) THEN */

/*                 CALL VUPACK ( DDATA, RED, GREEN, BLUE ) */
/*                 WRITE(*,'(A,F6.2)') 'Blue  (nm): ', BLUE  * 1000.D0 */
/*                 WRITE(*,'(A,F6.2)') 'Green (nm): ', GREEN * 1000.D0 */
/*                 WRITE(*,'(A,F6.2)') 'Red   (nm): ', RED   * 1000.D0 */

/*              ELSE */

/*                 WRITE(*,*) 'No data found in the kernel pool for ', */
/*             .              KEYWRD */

/*              END IF */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*        Blue  (nm): 475.00 */
/*        Green (nm): 550.00 */
/*        Red   (nm): 650.00 */


/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     W.L. Taber         (JPL) */
/*     I.M. Underwood     (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.2, 07-SEP-2020 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. Added complete */
/*        code example. */

/*        Fixed order of operands in equalities presented in */
/*        $Detailed_Output. Updated code fragments in $Particulars to */
/*        use in-place vector-scaling API. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU) */

/* -& */
/* $ Index_Entries */

/*     unpack three scalar components from a vector */

/* -& */

/*     Just shorthand, like it says above. */

    *x = v[0];
    *y = v[1];
    *z__ = v[2];
    return 0;
} /* vupack_ */

