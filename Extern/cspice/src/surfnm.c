/* surfnm.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__2 = 2;

/* $Procedure SURFNM ( Surface normal vector on an ellipsoid ) */
/* Subroutine */ int surfnm_(doublereal *a, doublereal *b, doublereal *c__, 
	doublereal *point, doublereal *normal)
{
    /* Initialized data */

    static char mssg[32*7] = "Axis A was nonpositive.         " "Axis B was "
	    "nonpositive.         " "Axes A and B were nonpositive.  " "Axis "
	    "C was nonpositive.         " "Axes A and C were nonpositive.  " 
	    "Axes B and C were nonpositive.  " "All three axes were nonposit"
	    "ive.";

    /* System generated locals */
    address a__1[2];
    integer i__1, i__2[2];
    doublereal d__1;
    char ch__1[35];

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer);
    /* Subroutine */ int s_cat(char *, char **, integer *, integer *, ftnlen);

    /* Local variables */
    static doublereal m;
    extern /* Subroutine */ int chkin_(char *, ftnlen), errch_(char *, char *,
	     ftnlen, ftnlen), errdp_(char *, doublereal *, ftnlen);
    static doublereal a1, b1, c1;
    extern /* Subroutine */ int sigerr_(char *, ftnlen), vhatip_(doublereal *)
	    , chkout_(char *, ftnlen), setmsg_(char *, ftnlen);
    extern logical return_(void);
    static integer bad;

/* $ Abstract */

/*     Compute the outward-pointing, unit normal vector at a point on */
/*     the surface of an ellipsoid. */

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

/*     ELLIPSOID */
/*     GEOMETRY */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     A          I   Length of the ellipsoid semi-axis along the X-axis. */
/*     B          I   Length of the ellipsoid semi-axis along the Y-axis. */
/*     C          I   Length of the ellipsoid semi-axis along the Z-axis. */
/*     POINT      I   Body-fixed coordinates of a point on the ellipsoid. */
/*     NORMAL     O   Outward pointing unit normal to ellipsoid at POINT. */

/* $ Detailed_Input */

/*     A        is the length of the semi-axis of the ellipsoid that is */
/*              parallel to the X-axis of the body-fixed reference frame. */

/*     B        is the length of the semi-axis of the ellipsoid that is */
/*              parallel to the Y-axis of the body-fixed reference frame. */

/*     C        is the length of the semi-axis of the ellipsoid that is */
/*              parallel to the Z-axis of the body-fixed reference frame. */

/*     POINT    is a 3-vector giving the bodyfixed coordinates of a point */
/*              on the ellipsoid. In bodyfixed coordinates, the semi-axes */
/*              of the ellipsoid are aligned with the X, Y, and Z-axes of */
/*              the reference frame. */

/* $ Detailed_Output */

/*     NORMAL   is the unit vector pointing away from the ellipsoid and */
/*              normal to the ellipsoid at POINT. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If any of the axes are non-positive, the error */
/*         SPICE(BADAXISLENGTH) is signaled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine computes the outward pointing unit normal vector to */
/*     the ellipsoid having semi-axes of length A, B, and C from the */
/*     point POINT. */

/* $ Examples */

/*     A typical use of SURFNM would be to find the angle of incidence */
/*     of the light from the sun at a point on the surface of an */
/*     ellipsoid. */

/*     Let Q be a 3-vector representing the rectangular body-fixed */
/*     coordinates of a point on the ellipsoid (we are assuming that */
/*     the axes of the ellipsoid are aligned with the axes of the */
/*     body fixed frame.) Let V be the vector from Q to the sun in */
/*     bodyfixed coordinates. Then the following code fragment could */
/*     be used to compute angle of incidence of sunlight at Q. */

/*        CALL SURFNM   ( A, B, C, Q, NRML ) */

/*        INCIDN = VSEP ( V,          NRML ) */

/* $ Restrictions */

/*     1)  It is assumed that the input POINT is indeed on the ellipsoid. */
/*         No checking for this is done. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     B.V. Semenov       (JPL) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.4.0, 26-OCT-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. Removed */
/*        unnecessary $Revisions section. */

/*        Updated the documentation to refer to "reference frame" instead */
/*        of "coordinate system" as per NAIF conventions. */

/* -    SPICELIB Version 1.3.2, 23-FEB-2016 (NJB) */

/*        Corrected some typos in the header. */

/* -    SPICELIB Version 1.3.1, 18-MAY-2010 (BVS) */

/*        Removed "C$" marker from text in the header. */

/* -    SPICELIB Version 1.3.0, 02-SEP-2005 (NJB) */

/*        Updated to remove non-standard use of duplicate arguments */
/*        in VHAT call. */

/* -    SPICELIB Version 1.2.0, 07-AUG-1996 (WLT) */

/*        Added a SAVE statement so that the error message will */
/*        not be lost between separate invocations of the routine. */

/* -    SPICELIB Version 1.1.0, 21-JUL-1995 (WLT) */

/*        A typo in the $Examples section was corrected */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) */

/* -& */
/* $ Index_Entries */

/*     surface normal vector on an ellipsoid */

/* -& */

/*     SPICELIB Functions */


/*     Local Variables */


/*     Initial values */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("SURFNM", (ftnlen)6);
    }

/*     Check the axes to make sure that none of them is less than or */
/*     equal to zero. If one is, signal an error and return. */

    bad = 0;
    if (*a <= 0.) {
	++bad;
    }
    if (*b <= 0.) {
	bad += 2;
    }
    if (*c__ <= 0.) {
	bad += 4;
    }
    if (bad > 0) {
/* Writing concatenation */
	i__2[0] = 32, a__1[0] = mssg + (((i__1 = bad - 1) < 7 && 0 <= i__1 ? 
		i__1 : s_rnge("mssg", i__1, "surfnm_", (ftnlen)243)) << 5);
	i__2[1] = 3, a__1[1] = " ? ";
	s_cat(ch__1, a__1, i__2, &c__2, (ftnlen)35);
	setmsg_(ch__1, (ftnlen)35);
	errch_(" ? ", "The A,B, and C axes were #, #, and # respectively.", (
		ftnlen)3, (ftnlen)50);
	errdp_("#", a, (ftnlen)1);
	errdp_("#", b, (ftnlen)1);
	errdp_("#", c__, (ftnlen)1);
	sigerr_("SPICE(BADAXISLENGTH)", (ftnlen)20);
	chkout_("SURFNM", (ftnlen)6);
	return 0;
    }

/*     Mathematically we want to compute (Px/a**2, Py/b**2, Pz/c**2) */
/*     and then convert this to a unit vector. However, computationally */
/*     this can blow up in our faces.  But note that only the ratios */
/*     a/b, b/c and a/c are important in computing the unit normal. */
/*     We can use the trick below to avoid the unpleasantness of */
/*     multiplication and division overflows. */

/* Computing MIN */
    d__1 = min(*a,*b);
    m = min(d__1,*c__);

/*     M can be divided by A,B or C without fear of an overflow */
/*     occurring. */

    a1 = m / *a;
    b1 = m / *b;
    c1 = m / *c__;

/*     All of the terms A1,B1,C1 are less than 1. Thus no overflows */
/*     can occur. */

    normal[0] = point[0] * (a1 * a1);
    normal[1] = point[1] * (b1 * b1);
    normal[2] = point[2] * (c1 * c1);
    vhatip_(normal);
    chkout_("SURFNM", (ftnlen)6);
    return 0;
} /* surfnm_ */

