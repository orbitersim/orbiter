/* pl2nvp.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure PL2NVP ( Plane to normal vector and point ) */
/* Subroutine */ int pl2nvp_(doublereal *plane, doublereal *normal, 
	doublereal *point)
{
    extern /* Subroutine */ int vscl_(doublereal *, doublereal *, doublereal *
	    );
    doublereal const__;
    extern /* Subroutine */ int pl2nvc_(doublereal *, doublereal *, 
	    doublereal *);

/* $ Abstract */

/*     Return a unit normal vector and point that define a specified */
/*     plane. */

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
/*     PLANE      I   A SPICE plane. */
/*     NORMAL, */
/*     POINT      O   A unit normal vector and point that define PLANE. */

/* $ Detailed_Input */

/*     PLANE    is a SPICE plane. */

/* $ Detailed_Output */

/*     NORMAL, */
/*     POINT    are, respectively, a unit normal vector and point */
/*              that define the geometric plane represented by */
/*              PLANE. Let the symbol < A, B > indicate the inner */
/*              product of vectors A and B; then the geometric */
/*              plane is the set of vectors X in three-dimensional */
/*              space that satisfy */

/*                 < X - POINT, NORMAL >  =  0. */

/*              POINT is always the closest point in the input */
/*              plane to the origin. POINT is always a */
/*              non-negative scalar multiple of NORMAL. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/*     1)  The input plane MUST have been created by one of the SPICELIB */
/*         routines */

/*            NVC2PL ( Normal vector and constant to plane ) */
/*            NVP2PL ( Normal vector and point to plane    ) */
/*            PSV2PL ( Point and spanning vectors to plane ) */

/*         Otherwise, the results of this routine are unpredictable. */

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

/* $ Examples */

/*     1)  Given a plane normal and constant, find a point in */
/*         the plane. POINT is the point we seek. */

/*            CALL NVC2PL ( NORMAL, CONST,  PLANE ) */
/*            CALL PL2NVP ( PLANE,  NORMAL, POINT ) */

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

/* -    SPICELIB Version 1.1.0, 24-AUG-2021 (NJB) (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 01-NOV-1990 (NJB) */

/* -& */
/* $ Index_Entries */

/*     plane to normal vector and point */

/* -& */

/*     Local variables */


/*     Note for programmers: validity of the input plane is not */
/*     checked in the interest of efficiency. The input plane will be */
/*     valid if it was created by one of the SPICE plane construction */
/*     routines. */

/*     Extract the unit normal and constant from the plane. Scaling the */
/*     unit normal by the constant gives us the closest point in the */
/*     plane to the origin. */
    pl2nvc_(plane, normal, &const__);
    vscl_(&const__, normal, point);
    return 0;
} /* pl2nvp_ */

