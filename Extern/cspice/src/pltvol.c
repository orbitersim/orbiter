/* pltvol.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure PLTVOL ( Compute volume of plate model ) */
doublereal pltvol_(integer *nv, doublereal *vrtces, integer *np, integer *
	plates)
{
    /* System generated locals */
    integer vrtces_dim2, plates_dim2, i__1, i__2, i__3, i__4;
    doublereal ret_val;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer);

    /* Local variables */
    extern /* Subroutine */ int vequ_(doublereal *, doublereal *);
    integer i__, j;
    doublereal m[9]	/* was [3][3] */;
    extern /* Subroutine */ int chkin_(char *, ftnlen), sigerr_(char *, 
	    ftnlen), chkout_(char *, ftnlen), setmsg_(char *, ftnlen), 
	    errint_(char *, integer *, ftnlen);
    extern logical return_(void);
    extern doublereal det_(doublereal *);

/* $ Abstract */

/*     Compute the volume of a three-dimensional region bounded by a */
/*     collection of triangular plates. */

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

/*     DSK */

/* $ Keywords */

/*     DSK */
/*     GEOMETRY */
/*     MATH */
/*     TOPOGRAPHY */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     NV         I   Number of vertices. */
/*     VRTCES     I   Array of vertices. */
/*     NP         I   Number of triangular plates. */
/*     PLATES     I   Array of plates. */

/*     The function returns the volume of the spatial region bounded */
/*     by the plates. */

/* $ Detailed_Input */

/*     NV       is the number of vertices comprising the plate */
/*              model. */

/*     VRTCES   is an array containing the plate model's vertices. */
/*              Elements */

/*                 VRTCES( 1, I ) */
/*                 VRTCES( 2, I ) */
/*                 VRTCES( 3, I ) */

/*              are, respectively, the X, Y, and Z components of */
/*              the Ith vertex. */

/*              This routine doesn't associate units with the */
/*              vertices. */


/*     NP       is the number of triangular plates comprising the */
/*              plate model. */

/*     PLATES   is an array containing 3-tuples of integers */
/*              representing the model's plates. The elements of */
/*              PLATES are vertex indices. The vertex indices are */
/*              1-based: vertices have indices ranging from 1 to */
/*              NV. The elements */

/*                 PLATES( 1, I ) */
/*                 PLATES( 2, I ) */
/*                 PLATES( 3, I ) */

/*              are, respectively, the indices of the vertices */
/*              comprising the Ith plate. */

/*              Note that the order of the vertices of a plate is */
/*              significant: the vertices must be ordered in the */
/*              positive (counterclockwise) sense with respect to */
/*              the outward normal direction associated with the */
/*              plate. In other words, if V1, V2, V3 are the */
/*              vertices of a plate, then */

/*                 ( V2 - V1 )  x  ( V3 - V2 ) */

/*              points in the outward normal direction. Here */
/*              "x" denotes the vector cross product operator. */

/* $ Detailed_Output */

/*     The function returns the volume of the spatial region bounded */
/*     by the plates. */

/*     If the components of the vertex array have length unit L, then the */
/*     output volume has units */

/*         3 */
/*        L */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  The input plate model must define a spatial region with */
/*         a boundary. This routine does not check the inputs to */
/*         verify this condition. See the $Restrictions section below. */

/*     2)  If the number of vertices is less than 4, the error */
/*         SPICE(TOOFEWVERTICES) is signaled. */

/*     3)  If the number of plates is less than 4, the error */
/*         SPICE(TOOFEWPLATES) is signaled. */

/*     4)  If any plate contains a vertex index outside of the range */

/*            [1, NV] */

/*         the error SPICE(INDEXOUTOFRANGE) is signaled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine computes the volume of a spatial region bounded by */
/*     a set of triangular plates. If the plate set does not actually */
/*     form the boundary of a spatial region, the result of this routine */
/*     is invalid. */

/*     Examples: */

/*        Valid inputs */
/*        ------------ */
/*        Tetrahedron */
/*        Box */
/*        Tiled ellipsoid */
/*        Two disjoint boxes */

/*        Invalid inputs */
/*        -------------- */
/*        Single plate */
/*        Tiled ellipsoid with one plate removed */
/*        Two boxes with intersection having positive volume */

/* $ Examples */

/*     The numerical results shown for this example may differ across */
/*     platforms. The results depend on the SPICE kernels used as input */
/*     (if any), the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */


/*     1) Compute the volume of the pyramid defined by the four */
/*        triangular plates whose vertices are the 3-element */
/*        subsets of the set of vectors */

/*           ( 0, 0, 0 ) */
/*           ( 1, 0, 0 ) */
/*           ( 0, 1, 0 ) */
/*           ( 0, 0, 1 ) */


/*        Example code begins here. */


/*        C */
/*        C     Compute the volume of a plate model representing the */
/*        C     pyramid with one vertex at the origin and the other */
/*        C     vertices coinciding with the standard basis vectors. */
/*        C */
/*              PROGRAM PLTVOL_EX1 */
/*              IMPLICIT NONE */

/*        C */
/*        C     SPICELIB functions */
/*        C */
/*              DOUBLE PRECISION      PLTVOL */
/*        C */
/*        C     Local parameters */
/*        C */
/*              INTEGER               NVERT */
/*              PARAMETER           ( NVERT  = 4 ) */

/*              INTEGER               NPLATE */
/*              PARAMETER           ( NPLATE = 4 ) */
/*        C */
/*        C     Local variables */
/*        C */
/*              DOUBLE PRECISION      VRTCES ( 3, NVERT  ) */
/*              DOUBLE PRECISION      VOL */

/*              INTEGER               PLATES ( 3, NPLATE ) */

/*        C */
/*        C     Let the notation */
/*        C */
/*        C        < A, B > */
/*        C */
/*        C     denote the dot product of vectors A and B. */
/*        C */
/*        C     The plates defined below lie in the following planes, */
/*        C     respectively: */
/*        C */
/*        C        Plate 1:    { P :  < P, (-1,  0,  0) > = 0 } */
/*        C        Plate 2:    { P :  < P, ( 0, -1,  0) > = 0 } */
/*        C        Plate 3:    { P :  < P, ( 0,  0, -1) > = 0 } */
/*        C        Plate 4:    { P :  < P, ( 1,  1,  1) > = 1 } */
/*        C */
/*              DATA                  PLATES /    1,     4,     3, */
/*             .                                  1,     2,     4, */
/*             .                                  1,     3,     2, */
/*             .                                  2,     3,     4 / */

/*              DATA                  VRTCES / 0.D0,  0.D0,  0.D0, */
/*             .                               1.D0,  0.D0,  0.D0, */
/*             .                               0.D0,  1.D0,  0.D0, */
/*             .                               0.D0,  0.D0,  1.D0 / */


/*              VOL = PLTVOL ( NVERT, VRTCES, NPLATE, PLATES ) */

/*              WRITE (*,*) 'Expected volume =    1/6' */
/*              WRITE (*,*) 'Computed volume = ', VOL */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*         Expected volume =    1/6 */
/*         Computed volume =   0.16666666666666666 */


/* $ Restrictions */

/*     1)  The plate collection must describe a surface and enclose a */
/*         volume such that the divergence theorem (see [1]) is */
/*         applicable. */

/* $ Literature_References */

/*     [1]  T. Apostol, "Calculus, Vol. II," John Wiley & Sons, 1969. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */

/* $ Version */

/* -    SPICELIB Version 1.0.1, 08-JUL-2020 (JDR) */

/*        Edited the header to comply with NAIF standard. Added DSK to */
/*        $Required_Reading. Updated code example comments. */

/* -    SPICELIB Version 1.0.0, 24-OCT-2016 (NJB) */

/*        Based on original 11-FEB-2011 */

/* -& */
/* $ Index_Entries */

/*     compute plate model volume */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     The function must have an initial value. */

    /* Parameter adjustments */
    vrtces_dim2 = *nv;
    plates_dim2 = *np;

    /* Function Body */
    ret_val = 0.;

/*     This routine uses discovery check-in. */

    if (return_()) {
	return ret_val;
    }

/*     Check the vertex and plate counts. */

    if (*nv < 4) {
	chkin_("PLTVOL", (ftnlen)6);
	setmsg_("At least 4 vertices are needed, but NV = #.", (ftnlen)43);
	errint_("#", nv, (ftnlen)1);
	sigerr_("SPICE(TOOFEWVERTICES)", (ftnlen)21);
	chkout_("PLTVOL", (ftnlen)6);
	return ret_val;
    }
    if (*np < 4) {
	chkin_("PLTVOL", (ftnlen)6);
	setmsg_("At least 4 plates are needed, but NP = #.", (ftnlen)41);
	errint_("#", np, (ftnlen)1);
	sigerr_("SPICE(TOOFEWPLATES)", (ftnlen)19);
	chkout_("PLTVOL", (ftnlen)6);
	return ret_val;
    }

/*     Make sure the vertex indices are in the range [1, NV]. */

    i__1 = *np;
    for (i__ = 1; i__ <= i__1; ++i__) {
	for (j = 1; j <= 3; ++j) {
	    if (plates[(i__2 = j + i__ * 3 - 4) < plates_dim2 * 3 && 0 <= 
		    i__2 ? i__2 : s_rnge("plates", i__2, "pltvol_", (ftnlen)
		    361)] < 1 || plates[(i__3 = j + i__ * 3 - 4) < 
		    plates_dim2 * 3 && 0 <= i__3 ? i__3 : s_rnge("plates", 
		    i__3, "pltvol_", (ftnlen)361)] > *nv) {
		chkin_("PLTVOL", (ftnlen)6);
		setmsg_("Vertex indices must be in the range [1, NV] for all"
			" SPICE language versions. The input value of NV was "
			"#. Vertex index # in plate # was #. (The vertex and "
			"plate numbers in this message are 1-based as well.)", 
			(ftnlen)206);
		errint_("#", nv, (ftnlen)1);
		errint_("#", &j, (ftnlen)1);
		errint_("#", &i__, (ftnlen)1);
		errint_("#", &plates[(i__2 = j + i__ * 3 - 4) < plates_dim2 * 
			3 && 0 <= i__2 ? i__2 : s_rnge("plates", i__2, "pltv"
			"ol_", (ftnlen)374)], (ftnlen)1);
		sigerr_("SPICE(INDEXOUTOFRANGE)", (ftnlen)22);
		chkout_("PLTVOL", (ftnlen)6);
		return ret_val;
	    }
	}
    }

/*     The volume computation below requires only a few lines of code, */
/*     but it might not be obvious that it works. An explanation */
/*     follows. */


/*        Notation */
/*        -------- */

/*        The expression */

/*           A x B */

/*        denotes the cross product of vectors A and B. The notation */

/*           < A, B > */

/*        denotes the dot product of these vectors. */

/*        The expression */

/*           ||A|| */

/*        denotes the Euclidean (L2) norm of A, and */

/*           ^ */
/*           A */

/*        is the unit-length vector aligned with A, namely */

/*           A / ||A|| */

/*        provided A is non-zero. */


/*        Geometric assumptions */
/*        --------------------- */

/*        The algorithm used here assumes, but does not attempt to */
/*        verify, that the input plate model represents the boundary of */
/*        a spatial region. The region has an "inside" and an "outside," */
/*        so it makes sense to speak of surface normal vectors that are */
/*        either "inward pointing" or "outward pointing." */
/*        The input plates are assumed to have vertex indices ordered in */
/*        the positive sense about their respective outward normal */
/*        vectors. So if a plate's vertices are A, B, C, then the */
/*        plate's sides are */

/*           B - A,  C - B,  A - C */

/*        and all of */

/*           ( B - A ) x ( C - B ) */
/*           ( C - B ) x ( A - C ) */
/*           ( A - C ) x ( B - A ) */

/*        are outward normal vectors. */

/*        Each plate lies in a plane, so all points on the plate satisfy */
/*        an equation of the form */

/*           < P, N > = constant */

/*        where N is an outward normal vector for that plate. If the */
/*        plane constant is positive, we say that the outward normal */
/*        vector "points away" from the origin; if the constant is */
/*        negative, we say that the outward normal "points toward" the */
/*        origin. This is a linguistic shortcut for the unwieldy phrases */
/*        "the component of the outward normal parallel to P points away */
/*        from the origin" or ... (the negative counterpart phrase). */


/*        Relationship of volume and surface */
/*        ---------------------------------- */

/*        Given a suitable 3-D spatial region, the divergence theorem */
/*        (see [1]) says that the volume integral, over that region, of */
/*        the divergence of a vector field F (div F) is equal to the */
/*        surface integral of F over the boundary of the region. So if */
/*        one picks a vector field F having divergence identically equal */
/*        to 1, the surface integral of F equals the volume of the */
/*        region. */

/*        We can use this fact to set up a surface integral that yields */
/*        the volume enclosed by a set of triangular plates. */


/*        A faster algorithm */
/*        ------------------ */

/*        However, there's a more efficient approach: we can compute the */
/*        volume of the model by summing the signed volumes of the */
/*        pyramids whose bases are the plates and whose apexes coincide */
/*        with the origin. Plates with outward normal vectors pointing */
/*        away from the origin contribute positive volume increments; */
/*        plates with outward normal vectors pointing toward the origin */
/*        contribute negative volume increments. */

/*        We'll show that these two methods are equivalent, and we'll */
/*        use the more efficient method in our code. */


/*        The surface integral */
/*        -------------------- */

/*        The field */

/*           F( x, y, z )  =  (1/3)( x, y, z ) */

/*        has divergence 1. Let the vectors */

/*           A, B, C */

/*        be the vertices of a plate, and let */

/*           N = ( B - A )  x  ( C - B ) */

/*        be an outward normal of the plate. Then */

/*           < N, X > */

/*        is constant for all X in the plane containing the plate, so we */
/*        can pick the vertex A to stand in for X. */

/*        Then the surface integral of F over the plate is simply */

/*             ^ */
/*           < N,  A/3 > * plate_area */


/*        Equivalence of the methods */
/*        -------------------------- */

/*        If we show that the above integral equals the volume */
/*        contribution of the pyramid corresponding to the plate, the */
/*        validity of summing the signed pyramid volumes is established. */

/*        Below, let */

/*           S1, S2 be the plate sides (B-A), (C-B). */

/*           N be the outward normal vector S1 x S2. */

/*           plate_area be...the area of the plate. */

/*        Then */

/*           N                  =   S1 x S2 */
/*                              =   ( B - A ) x ( C - B ) */
/*                              =   ( B x C ) - ( A x C ) + ( A x B ) */

/*        and since A is orthogonal to both */

/*           A x C */
/*           A x B */

/*        we have */

/*             ^ */
/*           < N,  A >          =   <  ( B x C )/||N||,  A  > */


/*        Then */

/*           plate_area         =    (1/2) * ||( S1 x S2 )|| */
/*                              =    ||N|| / 2 */

/*        So the surface integral of F over the plate is */

/*                            ^ */
/*           plate_area * ( < N, A/3 > ) */

/*                              =  (1/6) * ||N|| */
/*                                       * < (B x C)/||N||, A > */

/*                              =  < ( B x C ), A > / 6 */

/*                              =  < ( A X B ), C > / 6 */

/*        On the other hand, letting */

/*           base_area */

/*        denote the area of the triangle defined by A, B, and the */
/*        origin, the signed volume of the pyramid defined by A, B, and */
/*        C is */


/*           1/3 * base_area * height   =  1/3 *  < (A x B)/2,  C  > */

/*                                      =  < ( A X B ), C > / 6 */

/*        This shows the signed pyramid volume is equal to the surface */
/*        integral of F over the plate. */



/*     We proceed to compute the signed volume of each pyramid whose */
/*     base is a plate and whose vertex is the origin. */

/*     Note that PLTVOL has already been initialized to 0.D0. */

    i__1 = *np;
    for (i__ = 1; i__ <= i__1; ++i__) {

/*        Pack the vertices of the current plate into a 3x3 matrix. */

	for (j = 1; j <= 3; ++j) {
	    vequ_(&vrtces[(i__3 = plates[(i__2 = j + i__ * 3 - 4) < 
		    plates_dim2 * 3 && 0 <= i__2 ? i__2 : s_rnge("plates", 
		    i__2, "pltvol_", (ftnlen)594)] * 3 - 3) < vrtces_dim2 * 3 
		    && 0 <= i__3 ? i__3 : s_rnge("vrtces", i__3, "pltvol_", (
		    ftnlen)594)], &m[(i__4 = j * 3 - 3) < 9 && 0 <= i__4 ? 
		    i__4 : s_rnge("m", i__4, "pltvol_", (ftnlen)594)]);
	}

/*        The determinant of M gives the volume of the parallelepiped */
/*        spanned by the origin-vertex vectors. The corresponding */
/*        pyramid has volume 1/3 * base_area * height, and */
/*        the area of the pyramid's base is half that of base of the */
/*        parallelepiped. So the determinant must be divided by 6. */

	ret_val += det_(m) / 6.;
    }

/*     No check-out required, since the routine is not checked in */
/*     at this point. */

    return ret_val;
} /* pltvol_ */

