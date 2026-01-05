/* nvp2pl.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure NVP2PL ( Normal vector and point to plane ) */
/* Subroutine */ int nvp2pl_(doublereal *normal, doublereal *point, 
	doublereal *plane)
{
    extern /* Subroutine */ int vhat_(doublereal *, doublereal *);
    extern doublereal vdot_(doublereal *, doublereal *);
    extern /* Subroutine */ int vequ_(doublereal *, doublereal *), chkin_(
	    char *, ftnlen);
    extern logical vzero_(doublereal *);
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen);
    doublereal tmpvec[3];
    extern /* Subroutine */ int setmsg_(char *, ftnlen);
    extern logical return_(void);
    extern /* Subroutine */ int vminus_(doublereal *, doublereal *);

/* $ Abstract */

/*     Make a SPICE plane from a normal vector and a point. */

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
/*     NORMAL, */
/*     POINT      I   A normal vector and a point defining a plane. */
/*     PLANE      O   An array representing the plane. */

/* $ Detailed_Input */

/*     NORMAL, */
/*     POINT    are, respectively, a normal vector and point that */
/*              define a plane in three-dimensional space. NORMAL */
/*              need not be a unit vector. Let the symbol < a, b > */
/*              indicate the inner product of vectors a and b; */
/*              then the geometric plane is the set of vectors X */
/*              in three-dimensional space that satisfy */

/*                 < X - POINT, NORMAL >  =  0. */

/* $ Detailed_Output */

/*     PLANE    is a SPICE plane that represents the geometric */
/*              plane defined by POINT and NORMAL. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the input vector NORMAL is the zero vector, the error */
/*         SPICE(ZEROVECTOR) is signaled. */

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

/*     1)  Project a vector V orthogonally onto a plane defined by POINT */
/*         and NORMAL. PROJ is the projection we want; it is the */
/*         closest vector in the plane to V. */

/*            CALL NVP2PL ( NORMAL, POINT,  PLANE ) */
/*            CALL VPRJP  ( V,      PLANE,  PROJ  ) */


/*     2)  Given a point in a plane and a normal vector, find the */
/*         distance of the plane from the origin. We make a */
/*         `plane' from the point and normal, then convert the */
/*         plane to a unit normal and constant. The constant CONST */
/*         is (according to the specification of PL2NVC) the distance of */
/*         the plane from the origin. */

/*            CALL NVP2PL ( NORMAL, POINT,  PLANE ) */
/*            CALL PL2NVC ( PLANE,  NORMAL, CONST ) */

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

/*        Added IMPILCIT NONE statement. */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 1.1.0, 30-AUG-2005 (NJB) */

/*        Updated to remove non-standard use of duplicate arguments */
/*        in VMINUS call. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 01-NOV-1990 (NJB) */

/* -& */
/* $ Index_Entries */

/*     normal vector and point to plane */

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

/*     The normal vector must be non-zero. */

    if (vzero_(normal)) {
	chkin_("NVP2PL", (ftnlen)6);
	setmsg_("Plane's normal must be non-zero.", (ftnlen)32);
	sigerr_("SPICE(ZEROVECTOR)", (ftnlen)17);
	chkout_("NVP2PL", (ftnlen)6);
	return 0;
    }
    vhat_(normal, plane);
    plane[3] = vdot_(point, plane);

/*     The constant should be the distance of the plane from the */
/*     origin.  If the constant is negative, negate both it and the */
/*     normal vector. */

    if (plane[3] < 0.) {
	plane[3] = -plane[3];
	vminus_(plane, tmpvec);
	vequ_(tmpvec, plane);
    }
    return 0;
} /* nvp2pl_ */

