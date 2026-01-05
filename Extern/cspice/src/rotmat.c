/* rotmat.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__9 = 9;

/* $Procedure ROTMAT ( Rotate a matrix ) */
/* Subroutine */ int rotmat_(doublereal *m1, doublereal *angle, integer *
	iaxis, doublereal *mout)
{
    /* Initialized data */

    static integer indexs[5] = { 3,1,2,3,1 };

    /* System generated locals */
    integer i__1, i__2, i__3;

    /* Builtin functions */
    double sin(doublereal), cos(doublereal);
    integer s_rnge(char *, integer, char *, integer);

    /* Local variables */
    integer temp;
    doublereal c__;
    integer i__;
    doublereal s;
    extern /* Subroutine */ int moved_(doublereal *, integer *, doublereal *);
    doublereal prodm[9]	/* was [3][3] */;
    integer i1, i2, i3;

/* $ Abstract */

/*     Apply a rotation of ANGLE radians about axis IAXIS to a matrix. */
/*     This rotation is thought of as rotating the coordinate system. */

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

/*     MATRIX */
/*     ROTATION */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     M1         I   Matrix to be rotated. */
/*     ANGLE      I   Angle of rotation (radians). */
/*     IAXIS      I   Axis of rotation (X=1, Y=2, Z=3). */
/*     MOUT       O   Resulting rotated matrix [ANGLE]      * M1 */
/*                                                   IAXIS */

/* $ Detailed_Input */

/*     M1       is a 3x3 matrix to which a rotation is to be applied. In */
/*              matrix algebra, the components of the matrix are relevant */
/*              in one particular coordinate system. Applying ROTMAT */
/*              changes the components of M1 so that they are relevant to */
/*              a rotated coordinate system. */

/*     ANGLE    is the angle in radians through which the original */
/*              coordinate system is to be rotated. */

/*     IAXIS    is the index for the axis of the original coordinate */
/*              system about which the rotation by ANGLE is to be */
/*              performed. IAXIS = 1,2 or 3 designates the X-, Y- or */
/*              Z-axis, respectively. */

/* $ Detailed_Output */

/*     MOUT     is the matrix resulting from the application of the */
/*              specified rotation to the input matrix M1. If */

/*                 [ANGLE] */
/*                        IAXIS */

/*              denotes the rotation matrix by ANGLE radians about IAXIS, */
/*              (refer to the routine ROTATE) then MOUT is given by the */
/*              following matrix equation: */

/*                 MOUT = [ANGLE]      * M1 */
/*                               IAXIS */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/*     1)  If the axis index is not in the range 1 to 3, it will be */
/*         treated the same as that integer 1, 2, or 3 that is congruent */
/*         to it mod 3. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     None. */

/* $ Examples */

/*     Suppose that to rotate a set of inertial axes to body fixed */
/*     axes, one must first roll the coordinate axes about the x-axis by */
/*     angle R to get x', y', z'. From this one must pitch about the y' */
/*     axis by angle P to get x'', y'', z''.  And finally yaw the x'', */
/*     y'', z'' about the z'' axis by angle Y to obtain the */
/*     transformation to bodyfixed coordinates. If ID is the identity */
/*     matrix, then the following code fragment generates the */
/*     transformation from inertial to body fixed. */

/*        CALL ROTMAT ( ID, R,     1,     M1   ) */
/*        CALL ROTMAT ( M1, P,     2,     M2   ) */
/*        CALL ROTMAT ( M2, Y,     3,     TIBF ) */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     W.M. Owen          (JPL) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 27-MAY-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. Reformatted */
/*        arguments' description. */

/* -    SPICELIB Version 1.0.2, 23-APR-2010 (NJB) */

/*        Header correction: assertions that the output */
/*        can overwrite the input have been removed. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (WMO) */

/* -& */
/* $ Index_Entries */

/*     rotate a matrix */

/* -& */
/* $ Revisions */

/* -    Beta Version 1.1.0, 3-JAN-1989 (WLT) */

/*     Upgrade the routine to work with negative axis indexes. Also take */
/*     care of the funky way the indices (other than the input) were */
/*     obtained via the MOD function. It works but isn't as clear */
/*     (or fast) as just reading the axes from data. */

/* -& */

/*  Get the sine and cosine of ANGLE */

    s = sin(*angle);
    c__ = cos(*angle);

/*  Get indices for axes. The first index is for the axis of rotation. */
/*  The next two axes follow in right hand order (XYZ).  First get the */
/*  non-negative value of IAXIS mod 3 . */

    temp = (*iaxis % 3 + 3) % 3;
    i1 = indexs[(i__1 = temp) < 5 && 0 <= i__1 ? i__1 : s_rnge("indexs", i__1,
	     "rotmat_", (ftnlen)214)];
    i2 = indexs[(i__1 = temp + 1) < 5 && 0 <= i__1 ? i__1 : s_rnge("indexs", 
	    i__1, "rotmat_", (ftnlen)215)];
    i3 = indexs[(i__1 = temp + 2) < 5 && 0 <= i__1 ? i__1 : s_rnge("indexs", 
	    i__1, "rotmat_", (ftnlen)216)];

/*  Calculate the output matrix column by column */

    for (i__ = 1; i__ <= 3; ++i__) {
	prodm[(i__1 = i1 + i__ * 3 - 4) < 9 && 0 <= i__1 ? i__1 : s_rnge(
		"prodm", i__1, "rotmat_", (ftnlen)221)] = m1[(i__2 = i1 + i__ 
		* 3 - 4) < 9 && 0 <= i__2 ? i__2 : s_rnge("m1", i__2, "rotma"
		"t_", (ftnlen)221)];
	prodm[(i__1 = i2 + i__ * 3 - 4) < 9 && 0 <= i__1 ? i__1 : s_rnge(
		"prodm", i__1, "rotmat_", (ftnlen)222)] = c__ * m1[(i__2 = i2 
		+ i__ * 3 - 4) < 9 && 0 <= i__2 ? i__2 : s_rnge("m1", i__2, 
		"rotmat_", (ftnlen)222)] + s * m1[(i__3 = i3 + i__ * 3 - 4) < 
		9 && 0 <= i__3 ? i__3 : s_rnge("m1", i__3, "rotmat_", (ftnlen)
		222)];
	prodm[(i__1 = i3 + i__ * 3 - 4) < 9 && 0 <= i__1 ? i__1 : s_rnge(
		"prodm", i__1, "rotmat_", (ftnlen)223)] = -s * m1[(i__2 = i2 
		+ i__ * 3 - 4) < 9 && 0 <= i__2 ? i__2 : s_rnge("m1", i__2, 
		"rotmat_", (ftnlen)223)] + c__ * m1[(i__3 = i3 + i__ * 3 - 4) 
		< 9 && 0 <= i__3 ? i__3 : s_rnge("m1", i__3, "rotmat_", (
		ftnlen)223)];
    }

/*  Move the buffered matrix into MOUT. */

    moved_(prodm, &c__9, mout);

    return 0;
} /* rotmat_ */

