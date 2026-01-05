/* insang.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure INSANG ( Inside Tetrahedral Angle ) */
/* Subroutine */ int insang_(doublereal *v, doublereal *e1, doublereal *e2, 
	doublereal *e3, logical *found, doublereal *scale)
{
    extern doublereal vdot_(doublereal *, doublereal *);
    doublereal denom, norm12[3], norm31[3], norm23[3];
    extern /* Subroutine */ int vcrss_(doublereal *, doublereal *, doublereal 
	    *);
    doublereal en, vn12, vn31, vn23;

/* $ Abstract */

/*     Determine if a given vector lies inside the solid tetrahedral */
/*     angle determined by 3 vectors. If it does, return the */
/*     point where the scale factor such that SCALE*V lies in the */
/*     plane spanned by E1, E2, and E3. */

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
/*     V          I   Vector to test for "betweenness" */
/*     E1         I   First edge of the tetrahedral angle */
/*     E2         I   Second edge of the tetrahedral angle */
/*     E3         I   Third edge of the tetrahedral angle */
/*     FOUND      O   Indicates whether V lies in the solid angle */
/*     SCALE      O   Scale times V is in the triangle E1,E2,E3 */

/* $ Detailed_Input */

/*     V        is a 3-vector. This is the vector to test to see */
/*              if it lies between the 3 vectors E1, E2 and E3 */

/*     E1, */
/*     E2, */
/*     E3       are the three edges of a solid tetrahedral angle. (See */
/*              particulars for a discussion of the solid angle). */

/* $ Detailed_Output */

/*     FOUND    indicates that V lies inside the solid tetrahedral */
/*              angle determined by E1, E2 and E3. */


/*     SCALE    if V lies inside the solid tetrahedral angle given */
/*              by E1, E2 and E3, SCALE*V is the point is the positive */
/*              scalar multiple of V that pierces the triangle */
/*              determined by the points E1, E2, E3. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/*     1)  If E1, E2 and E3 are not linearly independent, the routine */
/*         returns .FALSE. SCALE will be set to 0. */

/*     2)  If V is the zero vector, the routine returns .FALSE. */
/*         SCALE will be set to 0. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     Given 3 linearly independent vectors E1, E2, and E3 the */
/*     set of vectors a*E1 + b*E2 + c*E3  where a, b, and c */
/*     are non-negative form a region of space that is a tetrahedral */
/*     solid angle. If you cut this solid angle with a plane */
/*     that intersects all three rays from the origin determined */
/*     by E1, E2 and E3 you will get a tetrahedron (a 4-sided */
/*     solid with each face a triangle). */

/*     This routine determines whether the ray associated with */
/*     a vector V lies inside the tetrahedral angle E1,E2,E3. */
/*     Moreover, if V lies inside this angle, this routine returns */
/*     the scale factor SCALE such that the point SCALE*V */
/*     lies in the plane containing the points E1, E2 and E3. */
/*     This is necessarily a point in the triangle determined by */
/*     E1, E2 and E3. */

/* $ Examples */

/*     Suppose you have a triangle in space specified by three */
/*     vertices P1, P2 and P3 and that an observer at location */
/*     OBS is looking along the ray emanating from OBS with */
/*     direction V. Does this ray intersect the triangle */
/*     P1, P2, P3?  Using this routine, you can answer this */
/*     question and give the point of intersection if there is */
/*     one. Here's how. */

/*     First construct the vectors from OBS to the corners of */
/*     the triangle. */

/*     CALL VSUB ( P1, OBS, E1 ) */
/*     CALL VSUB ( P2, OBS, E2 ) */
/*     CALL VSUB ( P3, OBS, E3 ) */

/*     Now see if V lies between the vectors E1, E2, E3 and return */
/*     the intersection point if it does. */

/*     CALL INSANG ( V, E1, E2, E3, FOUND, SCALE ) */

/*     If there was an intersection, add SCALE*V to OBS to get the */
/*     point of intersection. Otherwise say there was no intersection. */

/*     IF ( FOUND ) THEN */

/*        CALL VLCOM ( 1.0D0, OBS, SCALE, V, POINT ) */

/*        WRITE (*,*) 'The ray intersects the triangle at: */
/*        WRITE (*,*) POINT(1) */
/*        WRITE (*,*) POINT(2) */
/*        WRITE (*,*) POINT(3) */

/*     ELSE */

/*        WRITE (*,*) 'There is no intersection.' */

/*     END IF */

/* $ Restrictions */

/*     1)  This routine can suffer from extreme loss of precision if the */
/*         vectors E1, E2, E3 are too long compared to the lengths of the */
/*         line segments formed by their pairwise differences. */

/*         The user of this routine must ensure that the inputs are */
/*         suitable. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.3, 12-AUG-2021 (JDR) */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 1.0.2, 02-FEB-2016 (NJB) */

/*        Fixed comment typos. Updated $Restrictions. */

/* -    SPICELIB Version 1.0.1, 08-OCT-2009 (NJB) */

/*        Updated header. */

/* -    SPICELIB Version 1.0.0, 09-JUN-1996 (WLT) */

/* -& */
/* $ Index_Entries */

/*     Determine if a vector lies in a solid tetrahedral angle */

/* -& */

/*     SPICELIB Functions */


/*     Local Variables */


/*     Our initial value for SCALE is zero.  When we have better */
/*     information, we'll change this. */

    *scale = 0.;

/*     First we construct a normal to the plane spanned by E1 and E2 */
/*     and make sure that we don't get a zero vector.  If we */
/*     get the zero vector, E1 and E2 are linearly dependent so we */
/*     set the value of FOUND to FALSE and return. */

    vcrss_(e1, e2, norm12);

/*     First make sure V and E3 are in the same half space */
/*     bounded by E1 and E2.  If they are not, we can return. */

    vn12 = vdot_(v, norm12);
    en = vdot_(e3, norm12);

/*     Determine whether NORML and E3 are perpendicular.  If they */
/*     are perpendicular, E3 is a linear combination of E1 and E2. */
/*     In this case set FOUND to FALSE and return. */

    if (en == 0.) {
	*found = FALSE_;
	return 0;
    }

/*     Now check to see if V and E3 are in the same half space.  If */
/*     not, we can stop and return the value FALSE. */

    if (en > 0. && vn12 < 0.) {
	*found = FALSE_;
	return 0;
    } else if (en < 0. && vn12 > 0.) {
	*found = FALSE_;
	return 0;
    }

/*     Now check that V and E1 are on the same side of the plane */
/*     spanned by E2 and E3.  Note we don't have to compute EN */
/*     again <( E2 x E3 ), E1 >  because of the vector identity */

/*       < (E1 x E2), E3 > =  < (E2 x E3), E1 > = < (E3 x E1), E2 > */

    vcrss_(e2, e3, norm23);
    vn23 = vdot_(v, norm23);

/*     The following tests are the same as in the previous case. */

    if (en > 0. && vn23 < 0.) {
	*found = FALSE_;
	return 0;
    } else if (en < 0. && vn23 > 0.) {
	*found = FALSE_;
	return 0;
    }

/*     Finally check to see if V and E2 are in the same half space */
/*     bounded by E3 and E2 */

    vcrss_(e3, e1, norm31);
    vn31 = vdot_(v, norm31);
    if (en > 0. && vn31 < 0.) {
	*found = FALSE_;
	return 0;
    } else if (en < 0. && vn31 > 0.) {
	*found = FALSE_;
	return 0;
    }

/*     If you get this far, we know that V is lies in the intersection */
/*     of the half spaces determined by the various combinations of */
/*     E1, E2 and E3. */

    *found = TRUE_;

/*     Now find the intersection. First get a normal to the triangle. */
/*     One way to get the normal is to find the vector cross */
/*     product */

/*       NORML = ( E2 - E1 ) x ( E3 - E1 ) */

/*     However, this can be rewritten as: */

/*        NORML = E2 x E3 - E1 x E3 - E2 x E1 + E1 x E1 */

/*              = E2 x E3 + E3 x E1 + E1 x E2 */

/*     But we already have the three components E2 x E3, ... etc. */
/*     in the vectors NORM12, NORM23, NORM31 */

/*     Now we need to find the scalar multiple t*V such that */

/*        < tV - E1, NORML > = 0 */

/*     But this can be rewritten as: */

/*        t < V, NORML > = < E1, NORML > */

/*     Solving for t yields */

/*      t = < E1, NORML > / < V, NORML > */

/*        = < E1, E1xE2 + E2xE3 + E3xE1 > / <  V, E1xE2 + E2xE3 + E3xE1 > */

/*        = ( 0 + <E1, E2xE3> + 0 ) / (<V,E1xE2> + <V,E2xE3> + <V,E3xE1>) */

/*        =  EN / ( VN12 + VN23 + VN31 ) */

    denom = vn12 + vn23 + vn31;
    if (denom == 0.) {
	*found = FALSE_;
    } else {
	*found = TRUE_;
	*scale = en / denom;
    }
    return 0;
} /* insang_ */

