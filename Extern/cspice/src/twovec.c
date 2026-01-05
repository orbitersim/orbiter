/* twovec.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__9 = 9;

/* $Procedure TWOVEC ( Two vectors defining an orthonormal frame ) */
/* Subroutine */ int twovec_(doublereal *axdef, integer *indexa, doublereal *
	plndef, integer *indexp, doublereal *mout)
{
    /* Initialized data */

    static integer seqnce[5] = { 1,2,3,1,2 };

    /* System generated locals */
    integer i__1, i__2, i__3;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer);

    /* Local variables */
    extern /* Subroutine */ int vhat_(doublereal *, doublereal *), chkin_(
	    char *, ftnlen), moved_(doublereal *, integer *, doublereal *);
    doublereal mtemp[9]	/* was [3][3] */;
    integer i1, i2, i3;
    extern /* Subroutine */ int xpose_(doublereal *, doublereal *), ucrss_(
	    doublereal *, doublereal *, doublereal *), sigerr_(char *, ftnlen)
	    , chkout_(char *, ftnlen), setmsg_(char *, ftnlen), errint_(char *
	    , integer *, ftnlen);
    extern logical return_(void);

/* $ Abstract */

/*     Find the transformation to the right-handed frame having a */
/*     given vector as a specified axis and having a second given */
/*     vector lying in a specified coordinate plane. */

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

/*     AXES */
/*     FRAME */
/*     ROTATION */
/*     TRANSFORMATION */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  ------------------------------------------------- */
/*     AXDEF      I   Vector defining a principal axis. */
/*     INDEXA     I   Principal axis number of AXDEF (X=1, Y=2, Z=3). */
/*     PLNDEF     I   Vector defining (with AXDEF) a principal plane. */
/*     INDEXP     I   Second axis number (with INDEXA) of principal */
/*                    plane. */
/*     MOUT       O   Output rotation matrix. */

/* $ Detailed_Input */

/*     AXDEF    is a vector defining one of the principle axes of a */
/*              coordinate frame. */

/*     INDEXA   is a number that determines which of the three */
/*              coordinate axes contains AXDEF. */

/*              If INDEXA is 1 then AXDEF defines the X axis of the */
/*              coordinate frame. */

/*              If INDEXA is 2 then AXDEF defines the Y axis of the */
/*              coordinate frame. */

/*              If INDEXA is 3 then AXDEF defines the Z axis of the */
/*              coordinate frame. */

/*     PLNDEF   is a vector defining (with AXDEF) a principal plane of */
/*              the coordinate frame. AXDEF and PLNDEF must be */
/*              linearly independent. */

/*     INDEXP   is the second axis of the principal frame determined */
/*              by AXDEF and PLNDEF.  INDEXA, INDEXP must be different */
/*              and be integers from 1 to 3. */

/*              If INDEXP is 1, the second axis of the principal */
/*              plane is the X-axis. */

/*              If INDEXP is 2, the second axis of the principal */
/*              plane is the Y-axis. */

/*              If INDEXP is 3, the second axis of the principal plane */
/*              is the Z-axis. */

/* $ Detailed_Output */

/*     MOUT     is a rotation matrix that transforms coordinates given */
/*              in the input frame to the frame determined by AXDEF, */
/*              PLNDEF, INDEXA and INDEXP. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If INDEXA or INDEXP is not in the set {1,2,3}, the error */
/*         SPICE(BADINDEX) is signaled. */

/*     2)  If INDEXA and INDEXP are the same, the error */
/*         SPICE(UNDEFINEDFRAME) is signaled. */

/*     3)  If the cross product of the vectors AXDEF and PLNDEF is zero, */
/*         the error SPICE(DEPENDENTVECTORS) is signaled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     Given two linearly independent vectors there is a unique */
/*     right-handed coordinate frame having: */

/*        AXDEF lying along the INDEXA axis. */

/*        PLNDEF lying in the INDEXA-INDEXP coordinate plane. */

/*     This routine determines the transformation matrix that transforms */
/*     from coordinates used to represent the input vectors to the */
/*     the system determined by AXDEF and PLNDEF. Thus a vector */
/*     (x,y,z) in the input coordinate system will have coordinates */

/*                     t */
/*        MOUT* (x,y,z) */

/*     in the frame determined by AXDEF and PLNDEF. */

/* $ Examples */

/*     The rotation matrix TICC from inertial to Sun-Canopus */
/*     (celestial) coordinates is found by the call */

/*        CALL TWOVEC (Sun vector, 3, Canopus vector, 1, TICC) */

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

/* -    SPICELIB Version 1.2.0, 06-JUL-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. Removed */
/*        unnecessary $Revisions section. */

/* -    SPICELIB Version 1.1.0, 31-AUG-2005 (NJB) */

/*        Updated to remove non-standard use of duplicate arguments */
/*        in VSCL call. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (WMO) (WLT) */

/* -& */
/* $ Index_Entries */

/*     define an orthonormal frame from two vectors */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     Saved variables */


/*     Initial values */


/*     Standard SPICE error handling */

    if (return_()) {
	return 0;
    } else {
	chkin_("TWOVEC", (ftnlen)6);
    }

/*     Check for obvious bad inputs. */

    if (max(*indexp,*indexa) > 3 || min(*indexp,*indexa) < 1) {
	setmsg_("The definition indexes must lie in the range from 1 to 3.  "
		"The value of INDEXA was #. The value of INDEXP was #. ", (
		ftnlen)113);
	errint_("#", indexa, (ftnlen)1);
	errint_("#", indexp, (ftnlen)1);
	sigerr_("SPICE(BADINDEX)", (ftnlen)15);
	chkout_("TWOVEC", (ftnlen)6);
	return 0;
    } else if (*indexa == *indexp) {
	setmsg_("The values of INDEXA and INDEXP were the same, namely #.  T"
		"hey are required to be different.", (ftnlen)92);
	errint_("#", indexa, (ftnlen)1);
	sigerr_("SPICE(UNDEFINEDFRAME)", (ftnlen)21);
	chkout_("TWOVEC", (ftnlen)6);
	return 0;
    }

/*     Get indices for right-handed axes */

/*     First AXDEF ... */

    i1 = *indexa;

/*     ... then the other two. */

    i2 = seqnce[(i__1 = *indexa) < 5 && 0 <= i__1 ? i__1 : s_rnge("seqnce", 
	    i__1, "twovec_", (ftnlen)271)];
    i3 = seqnce[(i__1 = *indexa + 1) < 5 && 0 <= i__1 ? i__1 : s_rnge("seqnce"
	    , i__1, "twovec_", (ftnlen)272)];

/*     Row I1 contains normalized AXDEF (store in columns for now) */

    vhat_(axdef, &mout[(i__1 = i1 * 3 - 3) < 9 && 0 <= i__1 ? i__1 : s_rnge(
	    "mout", i__1, "twovec_", (ftnlen)277)]);

/*     Obtain rows I2 and I3 using cross products.  Which order to use */
/*     depends on whether INDEXP = I2 (next axis in right-handed order) */
/*     or INDEXP = I3 (previous axis in right-handed order). */

    if (*indexp == i2) {
	ucrss_(axdef, plndef, &mout[(i__1 = i3 * 3 - 3) < 9 && 0 <= i__1 ? 
		i__1 : s_rnge("mout", i__1, "twovec_", (ftnlen)286)]);
	ucrss_(&mout[(i__1 = i3 * 3 - 3) < 9 && 0 <= i__1 ? i__1 : s_rnge(
		"mout", i__1, "twovec_", (ftnlen)287)], axdef, &mout[(i__2 = 
		i2 * 3 - 3) < 9 && 0 <= i__2 ? i__2 : s_rnge("mout", i__2, 
		"twovec_", (ftnlen)287)]);
    } else {
	ucrss_(plndef, axdef, &mout[(i__1 = i2 * 3 - 3) < 9 && 0 <= i__1 ? 
		i__1 : s_rnge("mout", i__1, "twovec_", (ftnlen)291)]);
	ucrss_(axdef, &mout[(i__1 = i2 * 3 - 3) < 9 && 0 <= i__1 ? i__1 : 
		s_rnge("mout", i__1, "twovec_", (ftnlen)292)], &mout[(i__2 = 
		i3 * 3 - 3) < 9 && 0 <= i__2 ? i__2 : s_rnge("mout", i__2, 
		"twovec_", (ftnlen)292)]);
    }

/*     Finally, check to see that we actually got something non-zero */
/*     in one of the one columns of MOUT(1,I2) and MOUT(1,I3) (we need */
/*     only check one of them since they are related by a cross product). */

    if (mout[(i__1 = i2 * 3 - 3) < 9 && 0 <= i__1 ? i__1 : s_rnge("mout", 
	    i__1, "twovec_", (ftnlen)301)] == 0. && mout[(i__2 = i2 * 3 - 2) <
	     9 && 0 <= i__2 ? i__2 : s_rnge("mout", i__2, "twovec_", (ftnlen)
	    301)] == 0. && mout[(i__3 = i2 * 3 - 1) < 9 && 0 <= i__3 ? i__3 : 
	    s_rnge("mout", i__3, "twovec_", (ftnlen)301)] == 0.) {
	setmsg_("The input vectors AXDEF and PLNDEF are linearly dependent.", 
		(ftnlen)58);
	sigerr_("SPICE(DEPENDENTVECTORS)", (ftnlen)23);
    }

/*     Transpose MOUT. */

    xpose_(mout, mtemp);
    moved_(mtemp, &c__9, mout);
    chkout_("TWOVEC", (ftnlen)6);
    return 0;
} /* twovec_ */

