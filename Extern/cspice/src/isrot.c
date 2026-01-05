/* isrot.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure ISROT ( Indicate whether a matrix is a rotation matrix ) */
logical isrot_(doublereal *m, doublereal *ntol, doublereal *dtol)
{
    /* System generated locals */
    doublereal d__1, d__2, d__3, d__4, d__5, d__6;
    logical ret_val;

    /* Local variables */
    doublereal unit[9]	/* was [3][3] */, d__;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    logical detok;
    extern /* Subroutine */ int errdp_(char *, doublereal *, ftnlen);
    doublereal n1, n2, n3;
    extern /* Subroutine */ int unorm_(doublereal *, doublereal *, doublereal 
	    *);
    extern doublereal brcktd_(doublereal *, doublereal *, doublereal *);
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), setmsg_(char *, ftnlen);
    logical normok;
    extern logical return_(void);
    extern doublereal det_(doublereal *);

/* $ Abstract */

/*     Indicate whether a 3x3 matrix is a rotation matrix. */

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

/*     ROTATION */

/* $ Keywords */

/*     ERROR */
/*     MATRIX */
/*     ROTATION */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     M          I   A matrix to be tested. */
/*     NTOL       I   Tolerance for the norms of the columns of M. */
/*     DTOL       I   Tolerance for the determinant of a matrix whose */
/*                    columns are the unitized columns of M. */

/*     The function returns .TRUE. if and only if M is a rotation matrix. */

/* $ Detailed_Input */

/*     M        is a 3x3 matrix to be tested. */

/*     NTOL     is the tolerance for the norms of the columns */
/*              of M. */

/*     DTOL     is the tolerance for the determinant of a matrix */
/*              whose columns are the unitized columns of M. */

/* $ Detailed_Output */

/*     The function returns .TRUE. if and only if M is found to be a */
/*     rotation matrix. The criteria that M must meet are: */

/*     1) The norm of each column of M must satisfy the relation */

/*           1.D0 - NTOL  <   || column ||   <  1.D0 + NTOL */
/*                        -                  - */

/*     2) The determinant of the matrix whose columns are the */
/*        unitized columns of M must satisfy */

/*           1.D0 - DTOL  <   determinant   <  1.D0 + DTOL */
/*                        -                 - */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If either of NTOL or DTOL is negative, the error */
/*         SPICE(VALUEOUTOFRANGE) is signaled. ISROT returns the */
/*         value .FALSE. in this case. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine is an error checking `filter'; its purpose is to */
/*     detect gross errors, such as uninitialized matrices. Matrices */
/*     that do not pass the tests used by this routine hardly qualify as */
/*     rotation matrices. The test criteria can be adjusted by varying */
/*     the parameters NTOL and DTOL. */

/*     A property of rotation matrices is that their columns form a */
/*     right-handed, orthonormal basis in 3-dimensional space. The */
/*     converse is true: all 3x3 matrices with this property are */
/*     rotation matrices. */

/*     An ordered set of three vectors V1, V2, V3 forms a right-handed, */
/*     orthonormal basis if and only if */

/*        1)   || V1 ||  =  || V2 ||  =  || V3 ||  =  1 */

/*        2)   V3 = V1 x V2. Since V1, V2, and V3 are unit vectors, */
/*             we also have */

/*             < V3, V1 x V2 > = 1. */

/*             This quantity is the determinant of the matrix whose */
/*             columns are V1, V2 and V3. */

/*     When finite precision numbers are used, rotation matrices will */
/*     usually fail to satisfy these criteria exactly. We must use */
/*     criteria that indicate approximate conformance to the criteria */
/*     listed above. We choose */

/*        1)   |   || Vi ||  -  1   |   <   NTOL,  i = 1, 2, 3. */
/*                                      - */

/*        2)   Let */

/*                       Vi */
/*                Ui = ------ ,   i = 1, 2, 3. */
/*                     ||Vi|| */

/*             Then we require */

/*                | < U3, U1 x U2 > - 1 |  <  DTOL; */
/*                                         - */

/*             equivalently, letting U be the matrix whose columns */
/*             are U1, U2, and U3, we insist on */

/*                | det(U) - 1 |  <  DTOL. */
/*                                _ */

/* $ Examples */

/*     1)  We have obtained an instrument pointing matrix C from a */
/*         C-kernel, and we wish to test whether it is in fact a */
/*         rotation matrix. We can use ISROT to check this: */

/*            C */
/*            C    Obtain pointing matrix: */
/*            C */
/*                 CALL CKGP ( INST, TIMEIN, TOL, REF, C, TIMOUT, FOUND ) */

/*            C */
/*            C    Verify that C is a rotation: */
/*            C */
/*                 IF ( .NOT. ISROT ( C )  ) THEN */

/*                    [ perform exception handling ] */

/*                 ELSE */

/*                    [ code for the normal case goes here ] */

/*                 END IF */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     H.A. Neilan        (JPL) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.2.0, 17-JUN-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 1.1.0, 17-MAY-1994 (HAN) */

/*        If the value of the function RETURN is .TRUE. upon execution of */
/*        this module, this function is assigned a default value of */
/*        either 0, 0.0D0, .FALSE., or blank depending on the type of the */
/*        function. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 06-SEP-1990 (NJB) */

/* -& */
/* $ Index_Entries */

/*     indicate whether a matrix is a rotation matrix */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	ret_val = FALSE_;
	return ret_val;
    } else {
	chkin_("ISROT", (ftnlen)5);
    }

/*     Tolerances must be non-negative. */

    if (*ntol < 0.) {
	ret_val = FALSE_;
	setmsg_("NTOL should be non-negative; it is #.", (ftnlen)37);
	errdp_("#", ntol, (ftnlen)1);
	sigerr_("SPICE(VALUEOUTOFRANGE)", (ftnlen)22);
	chkout_("ISROT", (ftnlen)5);
	return ret_val;
    } else if (*dtol < 0.) {
	ret_val = FALSE_;
	setmsg_("DTOL should be non-negative; it is #.", (ftnlen)37);
	errdp_("#", dtol, (ftnlen)1);
	sigerr_("SPICE(VALUEOUTOFRANGE)", (ftnlen)22);
	chkout_("ISROT", (ftnlen)5);
	return ret_val;
    }

/*     The columns of M must resemble unit vectors.  If the norms are */
/*     outside of the allowed range, M is not a rotation matrix. */

/*     Also, the columns of M are required to be pretty nearly */
/*     orthogonal.  The discrepancy is gauged by taking the determinant */
/*     of the matrix UNIT, computed below, whose columns are the */
/*     unitized columns of M. */

    unorm_(m, unit, &n1);
    unorm_(&m[3], &unit[3], &n2);
    unorm_(&m[6], &unit[6], &n3);
    d__ = det_(unit);
    d__1 = 1. - *ntol;
    d__2 = *ntol + 1.;
    d__3 = 1. - *ntol;
    d__4 = *ntol + 1.;
    d__5 = 1. - *ntol;
    d__6 = *ntol + 1.;
    normok = n1 == brcktd_(&n1, &d__1, &d__2) && n2 == brcktd_(&n2, &d__3, &
	    d__4) && n3 == brcktd_(&n3, &d__5, &d__6);
    d__1 = 1. - *dtol;
    d__2 = *dtol + 1.;
    detok = d__ == brcktd_(&d__, &d__1, &d__2);
    if (normok && detok) {
	ret_val = TRUE_;
    } else {
	ret_val = FALSE_;
    }
    chkout_("ISROT", (ftnlen)5);
    return ret_val;
} /* isrot_ */

