/* psv2pl.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure PSV2PL ( Point and spanning vectors to plane ) */
/* Subroutine */ int psv2pl_(doublereal *point, doublereal *span1, doublereal 
	*span2, doublereal *plane)
{
    extern doublereal vdot_(doublereal *, doublereal *);
    extern /* Subroutine */ int vequ_(doublereal *, doublereal *), chkin_(
	    char *, ftnlen), ucrss_(doublereal *, doublereal *, doublereal *);
    extern logical vzero_(doublereal *);
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen);
    doublereal tmpvec[3];
    extern /* Subroutine */ int setmsg_(char *, ftnlen);
    extern logical return_(void);
    extern /* Subroutine */ int vminus_(doublereal *, doublereal *);

/* $ Abstract */

/*     Make a SPICE plane from a point and two spanning vectors. */

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

/*     PLANES */

/* $ Keywords */

/*     GEOMETRY */
/*     MATH */
/*     PLANE */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     POINT, */
/*     SPAN1, */
/*     SPAN2      I   A point and two spanning vectors defining a plane. */
/*     PLANE      O   An array representing the plane. */

/* $ Detailed_Input */

/*     POINT, */
/*     SPAN1, */
/*     SPAN2    are, respectively, a point and two spanning vectors that */
/*              define a geometric plane in three-dimensional space. The */
/*              plane is the set of vectors */

/*                 POINT   +   s * SPAN1   +   t * SPAN2 */

/*              where `s' and `t' are real numbers. The spanning vectors */
/*              SPAN1 and SPAN2 must be linearly independent, but they */
/*              need not be orthogonal or unitized. */

/* $ Detailed_Output */

/*     PLANE    is a SPICE plane that represents the geometric plane */
/*              defined by POINT, SPAN1, and SPAN2. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If SPAN1 and SPAN2 are linearly dependent, i.e. the vectors */
/*         POINT, SPAN1, and SPAN2 do not define a plane, the error */
/*         SPICE(DEGENERATECASE) is signaled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     SPICELIB geometry routines that deal with planes use the `plane' */
/*     data type to represent input and output planes. This data type */
/*     makes the subroutine interfaces simpler and more uniform. */

/*     The SPICELIB routines that produce SPICE planes from data that */
/*     define a plane are: */

/*        NVC2PL ( Normal vector and constant to plane ) */
/*        NVP2PL ( Normal vector and point to plane    ) */
/*        PSV2PL ( Point and spanning vectors to plane ) */

/*     The SPICELIB routines that convert SPICE planes to data that */
/*     define a plane are: */

/*        PL2NVC ( Plane to normal vector and constant ) */
/*        PL2NVP ( Plane to normal vector and point    ) */
/*        PL2PSV ( Plane to point and spanning vectors ) */

/*     Any of these last three routines may be used to convert this */
/*     routine's output, PLANE, to another representation of a */
/*     geometric plane. */

/* $ Examples */

/*     1)  Project a vector V orthogonally onto a plane defined by */
/*         POINT, SPAN1, and SPAN2. PROJ is the projection we want; it */
/*         is the closest vector in the plane to V. */

/*            CALL PSV2PL ( POINT,   SPAN1,   SPAN2,   PLANE ) */
/*            CALL VPRJP  ( V,       PLANE,   PROJ           ) */


/*     2)  Find the plane determined by a spacecraft's position vector */
/*         relative to a central body and the spacecraft's velocity */
/*         vector. We assume that all vectors are given in the same */
/*         coordinate system. */

/*            C */
/*            C     POS is the spacecraft's position, relative to */
/*            C     the central body. VEL is the spacecraft's velocity */
/*            C     vector. POS is a point (vector, if you like) in */
/*            C     the orbit plane, and it is also one of the spanning */
/*            C     vectors of the plane. */
/*            C */
/*                  CALL PSV2PL ( POS, POS, VEL, PLANE ) */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     [1]  G. Thomas and R. Finney, "Calculus and Analytic Geometry," */
/*          7th Edition, Addison Wesley, 1988. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.2.0, 24-AUG-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 1.1.0, 31-AUG-2005 (NJB) */

/*        Updated to remove non-standard use of duplicate arguments */
/*        in VMINUS call. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 01-NOV-1990 (NJB) */

/* -& */
/* $ Index_Entries */

/*     point and spanning vectors to plane */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     The contents of SPICE planes are as follows: */

/*        Elements NMLPOS through NMLPOS + 2 contain a unit normal */
/*        vector for the plane. */

/*        Element CONPOS contains a constant for the plane;  every point */
/*        X in the plane satisfies */

/*           < X, PLANE(NMLPOS) >  =  PLANE(CONPOS). */

/*        The plane constant is the distance of the plane from the */
/*        origin; the normal vector, scaled by the constant, is the */
/*        closest point in the plane to the origin. */



/*     Local variables */


/*     This routine checks in only if an error is discovered. */

    if (return_()) {
	return 0;
    }

/*     Find the unitized cross product of SPAN1 and SPAN2; this is our */
/*     unit normal vector, or possibly its inverse. */

    ucrss_(span1, span2, plane);
    if (vzero_(plane)) {
	chkin_("PSV2PL", (ftnlen)6);
	setmsg_("Spanning vectors are parallel.", (ftnlen)30);
	sigerr_("SPICE(DEGENERATECASE)", (ftnlen)21);
	chkout_("PSV2PL", (ftnlen)6);
	return 0;
    }

/*     Find the plane constant corresponding to the unit normal */
/*     vector we've found. */

    plane[3] = vdot_(plane, point);

/*     The constant should be the distance of the plane from the */
/*     origin.  If the constant is negative, negate both it and the */
/*     normal vector. */

    if (plane[3] < 0.) {
	plane[3] = -plane[3];
	vminus_(plane, tmpvec);
	vequ_(tmpvec, plane);
    }
    return 0;
} /* psv2pl_ */

