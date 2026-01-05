/* pjelpl.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure PJELPL ( Project ellipse onto plane ) */
/* Subroutine */ int pjelpl_(doublereal *elin, doublereal *plane, doublereal *
	elout)
{
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    doublereal const__;
    extern /* Subroutine */ int vperp_(doublereal *, doublereal *, doublereal 
	    *), vprjp_(doublereal *, doublereal *, doublereal *), el2cgv_(
	    doublereal *, doublereal *, doublereal *, doublereal *), cgv2el_(
	    doublereal *, doublereal *, doublereal *, doublereal *), pl2nvc_(
	    doublereal *, doublereal *, doublereal *);
    doublereal prjvc1[3], prjvc2[3], center[3], normal[3], smajor[3];
    extern /* Subroutine */ int chkout_(char *, ftnlen);
    doublereal prjctr[3], sminor[3];
    extern logical return_(void);

/* $ Abstract */

/*     Project an ellipse onto a plane, orthogonally. */

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

/*     ELLIPSES */
/*     PLANES */

/* $ Keywords */

/*     ELLIPSE */
/*     GEOMETRY */
/*     MATH */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     ELIN       I   A SPICE ellipse to be projected. */
/*     PLANE      I   A plane onto which ELIN is to be projected. */
/*     ELOUT      O   A SPICE ellipse resulting from the projection. */

/* $ Detailed_Input */

/*     ELIN, */
/*     PLANE    are, respectively, a SPICE ellipse and a */
/*              SPICE plane. The geometric ellipse represented */
/*              by ELIN is to be orthogonally projected onto the */
/*              geometric plane represented by PLANE. */

/* $ Detailed_Output */

/*     ELOUT    is a SPICE ellipse that represents the geometric */
/*              ellipse resulting from orthogonally projecting the */
/*              ellipse represented by INEL onto the plane */
/*              represented by PLANE. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the input plane is invalid, an error is signaled by a */
/*         routine in the call tree of this routine. */

/*     2)  The input ellipse may be degenerate--its semi-axes may be */
/*         linearly dependent. Such ellipses are allowed as inputs. */

/*     3)  The ellipse resulting from orthogonally projecting the input */
/*         ellipse onto a plane may be degenerate, even if the input */
/*         ellipse is not. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     Projecting an ellipse orthogonally onto a plane can be thought of */
/*     finding the points on the plane that are `under' or `over' the */
/*     ellipse, with the `up' direction considered to be perpendicular */
/*     to the plane. More mathematically, the orthogonal projection is */
/*     the set of points Y in the plane such that for some point X in */
/*     the ellipse, the vector Y - X is perpendicular to the plane. */
/*     The orthogonal projection of an ellipse onto a plane yields */
/*     another ellipse. */

/* $ Examples */

/*     1)  With  CENTER  = ( 1.D0,  1.D0,  1.D0 ), */
/*               VECT1   = ( 2.D0,  0.D0,  0.D0 ), */
/*               VECT2   = ( 0.D0,  1.D0,  1.D0 ), */
/*               NORMAL  = ( 0.D0,  0.D0,  1.D0 ), */

/*         the code fragment */

/*               CALL NVC2PL ( NORMAL,  0.D0,    PLANE           ) */
/*               CALL CGV2EL ( CENTER,  VECT1,   VECT2,   ELIN   ) */
/*               CALL PJELPL ( ELIN,    PLANE,   ELOUT           ) */
/*               CALL EL2CGV ( ELOUT,   PRJCTR,  PRJMAJ,  PRJMIN ) */

/*         returns */

/*               PRJCTR  = ( 1.D0,  1.D0,  0.D0 ) */
/*               PRJMAJ  = ( 2.D0,  0.D0,  0.D0 ) */
/*               PRJMIN  = ( 0.D0,  1.D0,  0.D0 ) */


/*     2)  With  VECT1   = ( 2.D0,  0.D0,  0.D0 ), */
/*               VECT2   = ( 1.D0,  1.D0,  1.D0 ), */
/*               CENTER  = ( 0.D0,  0.D0,  0.D0 ), */
/*               NORMAL  = ( 0.D0,  0.D0,  1.D0 ), */

/*         the code fragment */

/*               CALL NVC2PL ( NORMAL,  0.D0,    PLANE           ) */
/*               CALL CGV2EL ( CENTER,  VECT1,   VECT2,   ELIN   ) */
/*               CALL PJELPL ( ELIN,    PLANE,   ELOUT           ) */
/*               CALL EL2CGV ( ELOUT,   PRJCTR,  PRJMAJ,  PRJMIN ) */

/*         returns */

/*               PRJCTR  = ( 0.D0,  0.D0,  0.D0 ) */

/*               PRJMAJ  = ( -2.227032728823213D0, */
/*                           -5.257311121191336D-1, */
/*                            0.D0                  ) */

/*               PRJMIN  = (  2.008114158862273D-1, */
/*                           -8.506508083520399D-1, */
/*                            0.D0                  ) */



/*     3)    An example of actual use: Suppose we wish to compute the */
/*           distance from an ellipsoid to a line. Let the line be */
/*           defined by a point P and a direction vector DIRECT; the */
/*           line is the set of points */

/*              P   +   t * DIRECT, */

/*           where t is any real number. Let the ellipsoid have semi- */
/*           axis lengths A, B, and C. */

/*           We can reduce the problem to that of finding the distance */
/*           between the line and an ellipse on the ellipsoid surface by */
/*           considering the fact that the surface normal at the nearest */
/*           point to the line will be orthogonal to DIRECT; the set of */
/*           surface points where this condition holds lies in a plane, */
/*           and hence is an ellipse on the surface. The problem can be */
/*           further simplified by projecting the ellipse orthogonally */
/*           onto the plane defined by */

/*              < X, DIRECT >  =  0. */

/*           The problem is then a two dimensional one: find the */
/*           distance of the projected ellipse from the intersection of */
/*           the line and this plane (which is necessarily one point). */
/*           A `paraphrase' of the relevant code is: */


/*              C     Step 1. Find the candidate ellipse CAND. */
/*              C               NORMAL is a normal vector to the plane */
/*              C               containing the candidate ellipse. The */
/*              C               ellipse must exist, since it's the */
/*              C               intersection of an ellipsoid centered at */
/*              C               the origin and a plane containing the */
/*              C               origin. For this reason, we don't check */
/*              C               INEDPL's `found flag' FOUND below. */
/*              C */
/*                    NORMAL(1)  =  DIRECT(1) / A**2 */
/*                    NORMAL(2)  =  DIRECT(2) / B**2 */
/*                    NORMAL(3)  =  DIRECT(3) / C**2 */

/*                    CALL NVC2PL ( NORMAL, 0.D0, CANDPL ) */

/*                    CALL INEDPL ( A, B, C, CANDPL, CAND, FOUND ) */

/*              C */
/*              C     Step 2. Project the candidate ellipse onto a */
/*              C               plane orthogonal to the line. We'll */
/*              C               call the plane PRJPL and the */
/*              C               projected ellipse PRJEL. */
/*              C */
/*                     CALL NVC2PL ( DIRECT,  0.D0,   PRJPL ) */
/*                     CALL PJELPL ( CAND,    PRJPL,  PRJEL ) */

/*              C */
/*              C     Step 3. Find the point on the line lying in the */
/*              C               projection plane, and then find the */
/*              C               near point PJNEAR on the projected */
/*              C               ellipse. Here PRJPT is the point on the */
/*              C               input line that lies in the projection */
/*              C               plane. The distance between PRJPT and */
/*              C               PJNEAR is DIST. */

/*                    CALL VPRJP  ( LINEPT,  PRJPL,  PRJPT         ) */
/*                    CALL NPEDPT ( PRJEL,   PRJPT,  PJNEAR,  DIST ) */

/*              C */
/*              C     Step 4. Find the near point PNEAR on the */
/*              C              ellipsoid by taking the inverse */
/*              C              orthogonal projection of PJNEAR; this is */
/*              C              the point on the candidate ellipse that */
/*              C              projects to PJNEAR. Note that the output */
/*              C              DIST was computed in step 3. */
/*              C */
/*              C              The inverse projection of PJNEAR is */
/*              C              guaranteed to exist, so we don't have to */
/*              C              check FOUND. */
/*              C */
/*                    CALL VPRJPI ( PJNEAR, PRJPL, CANDPL, PNEAR, FOUND ) */


/*           The value of DIST returned is the distance we're looking */
/*           for. */

/*           The procedure described here is carried out in the routine */
/*           NPEDLN. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 24-AUG-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 02-NOV-1990 (NJB) */

/* -& */
/* $ Index_Entries */

/*     project ellipse onto plane */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("PJELPL", (ftnlen)6);
    }

/*     Find generating vectors of the input ellipse. */

    el2cgv_(elin, center, smajor, sminor);

/*     Find a normal vector for the input plane. */

    pl2nvc_(plane, normal, &const__);

/*     Find the components of the semi-axes that are orthogonal to the */
/*     input plane's normal vector.  The components are generating */
/*     vectors for the projected plane. */

    vperp_(smajor, normal, prjvc1);
    vperp_(sminor, normal, prjvc2);

/*     Find the projection of the ellipse's center onto the input plane. */
/*     This is the center of the projected ellipse. */

/*     In case the last assertion is non-obvious, note that the */
/*     projection we're carrying out is the composition of a linear */
/*     mapping (projection to a plane containing the origin and parallel */
/*     to PLANE) and a translation mapping (adding the closest point to */
/*     the origin in PLANE to every point), and both linear mappings and */
/*     translations carry the center of an ellipse to the center of the */
/*     ellipse's image.  Let's state this using mathematical symbols. */
/*     Let L be a linear mapping and let T be a translation mapping, */
/*     say */

/*        T(x) = x + A. */

/*     Then */

/*           T  (  L ( center + cos(theta)smajor + sin(theta)sminor )  ) */

/*        =  A  +  L ( center + cos(theta)smajor + sin(theta)sminor ) */

/*        =  A  +  L (center) */
/*              +  cos(theta) L(smajor) */
/*              +  sin(theta) L(sminor) */

/*     From the form of this last expression, we see that we have an */
/*     ellipse centered at */

/*           A  +  L (center) */

/*        =  T  (  L (center)  ) */

/*     This last term is the image of the center of the original ellipse, */
/*     as we wished to demonstrate. */

/*     Now in the case of orthogonal projection onto a plane PL, L can be */
/*     taken as the orthogonal projection onto a parallel plane PL' */
/*     containing the origin.  Then L is a linear mapping.  Let M be */
/*     the multiple of the normal vector of PL such that M is contained */
/*     in PL (M is the closest point in PL to the origin).  Then the */
/*     orthogonal projection mapping onto PL, which we will name PRJ, */
/*     can be defined by */

/*       PRJ (x)  =  L (x)  +  M. */

/*     So PRJ is the composition of a translation and a linear mapping, */
/*     as claimed. */


    vprjp_(center, plane, prjctr);

/*     Put together the projected ellipse. */

    cgv2el_(prjctr, prjvc1, prjvc2, elout);
    chkout_("PJELPL", (ftnlen)6);
    return 0;
} /* pjelpl_ */

