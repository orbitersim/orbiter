/* zzrtnmat.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__9 = 9;

/* $Procedure ZZRTNMAT ( RTN transformation matrix ) */
/* Subroutine */ int zzrtnmat_(doublereal *v, doublereal *m)
{
    /* Initialized data */

    static doublereal z__[3] = { 0.,0.,1. };

    /* System generated locals */
    integer i__1, i__2;

    /* Builtin functions */
    double atan2(doublereal, doublereal), cos(doublereal), sin(doublereal);
    integer s_rnge(char *, integer, char *, integer);

    /* Local variables */
    doublereal east[3];
    extern /* Subroutine */ int vhat_(doublereal *, doublereal *);
    doublereal vlon[3];
    integer i__;
    extern /* Subroutine */ int chkin_(char *, ftnlen), errdp_(char *, 
	    doublereal *, ftnlen);
    doublereal north[3];
    extern /* Subroutine */ int ucrss_(doublereal *, doublereal *, doublereal 
	    *), cleard_(integer *, doublereal *), sigerr_(char *, ftnlen), 
	    chkout_(char *, ftnlen), setmsg_(char *, ftnlen);
    extern logical return_(void);
    doublereal rad[3], lon;

/* $ Abstract */

/*     Given a vector, return a transformation matrix that maps from the */
/*     vector's base reference frame to the RTN */
/*     (radial-tangential-normal) frame associated with the vector. */

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

/*     FRAMES */
/*     MATRIX */
/*     ROTATION */

/* $ Declarations */
/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     V          I   A 3-dimensional vector. */
/*     M          O   Base frame to RTN frame rotation matrix. */

/* $ Detailed_Input */

/*     V              is any vector that does not lie on the Z-axis */
/*                    of the reference frame relative to which the */
/*                    vector is expressed: at least one of V's X or */
/*                    Y components must be non-zero. */

/* $ Detailed_Output */

/*     M              is a rotation matrix that transforms vectors */
/*                    from the base frame of V---that is, the reference */
/*                    frame relative to which V is expressed---to */
/*                    the RTN (radial, tangential, normal) frame */
/*                    defined by V. */

/*                    The basis vectors of the RTN frame are defined */
/*                    as follows: */

/*                       Axis 1: radial direction R. This axis is */
/*                               parallel to V. */

/*                       Axis 2: tangential direction T. This axis */
/*                               is parallel to Z x V, where Z is */
/*                               the third axis of V's base frame. */

/*                       Axis 3: normal direction N. This axis is */
/*                               parallel to R x T. */

/*                   The unit vectors R, T, N are, respectively, the */
/*                   first, second and third rows of M. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1) If the input vector V has X and Y components equal to zero, */
/*        the error SPICE(DEGENERATECASE) is signaled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     The RTN frame supported this routine is a generalization of */
/*     the frame defined by a solar system object's spin axis and */
/*     and a position vector (often the position of a spacecraft */
/*     relative to the center of the object). */

/*     If the base frame of the input vector V is the body-fixed, */
/*     body-centered planetocentric frame associated with a solar system */
/*     object such as a planet or satellite, then the R, T, N directions */
/*     correspond to the "up", "East," and "North" directions at the */
/*     location indicated by V. */

/* $ Examples */

/*     1) Get the RTN transformation matrix for the vector ( 1, 0, 1 ): */


/*           IMPLICIT NONE */

/*           DOUBLE PRECISION      V ( 3 ) */
/*           DOUBLE PRECISION      M ( 3, 3 ) */
/*           INTEGER               I */
/*           INTEGER               J */

/*           CALL VPACK    ( 1.D0, 0.D0, 1.D0, V ) */

/*           CALL ZZRTNMAT ( V, M ) */

/*           DO I = 1, 3 */
/*              WRITE(*,'(3E15.7)') ( M(I,J), J = 1, 3 ) */
/*           END DO */

/*           END */

/*        When this program was executed on a PC/Linux/g77 system, the */
/*        output was */

/*            0.7071068E+00  0.0000000E+00  0.7071068E+00 */
/*            0.0000000E+00  0.1000000E+01  0.0000000E+00 */
/*           -0.7071068E+00  0.0000000E+00  0.7071068E+00 */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman   (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 05-MAR-2009 (NJB) */

/* -& */
/* $ Index_Entries */

/*     transformation to radial, tangential, normal frame */
/*     transformation to rtn frame */

/* -& */

/*     SPICELIB functions */


/*     Local variables */

/*     Internally, we're going to use the more */
/*     descriptive names EAST for the "tangential" */
/*     direction and NORTH for the "normal" direction. */


/*     Saved variables */


/*     Initial values */


/*     Use discovery check-in. Just test the RETURN status. */

    if (return_()) {
	return 0;
    }
    if (v[0] == 0. && v[1] == 0.) {
	cleard_(&c__9, m);
	chkin_("ZZRTNMAT", (ftnlen)8);
	setmsg_("Input vector (# # #) lies on Z-axis; tangential and normal "
		"directions are undefined.", (ftnlen)84);
	errdp_("#", v, (ftnlen)1);
	errdp_("#", &v[1], (ftnlen)1);
	errdp_("#", &v[2], (ftnlen)1);
	sigerr_("SPICE(DEGENERATECASE)", (ftnlen)21);
	chkout_("ZZRTNMAT", (ftnlen)8);
	return 0;
    } else {

/*        The two-argument arctangent function gives us a */
/*        robust way of determining the longitude of V, even */
/*        when the magnitude of V is very small. */

	lon = atan2(v[1], v[0]);

/*        Let VLON be a unit vector in the x-y plane whose */
/*        longitude is LON. */

	vlon[0] = cos(lon);
	vlon[1] = sin(lon);
	vlon[2] = 0.;

/*        We can compute the East and North vectors */
/*        without much loss of precision, since VLON is */
/*        orthogonal to Z and EAST is orthogonal to V. */

	ucrss_(z__, vlon, east);
	ucrss_(v, east, north);
	vhat_(v, rad);

/*        The rows of M are the basis vectors of */
/*        the radial/East/North frame: */

	for (i__ = 1; i__ <= 3; ++i__) {
	    m[(i__1 = i__ * 3 - 3) < 9 && 0 <= i__1 ? i__1 : s_rnge("m", i__1,
		     "zzrtnmat_", (ftnlen)258)] = rad[(i__2 = i__ - 1) < 3 && 
		    0 <= i__2 ? i__2 : s_rnge("rad", i__2, "zzrtnmat_", (
		    ftnlen)258)];
	    m[(i__1 = i__ * 3 - 2) < 9 && 0 <= i__1 ? i__1 : s_rnge("m", i__1,
		     "zzrtnmat_", (ftnlen)259)] = east[(i__2 = i__ - 1) < 3 &&
		     0 <= i__2 ? i__2 : s_rnge("east", i__2, "zzrtnmat_", (
		    ftnlen)259)];
	    m[(i__1 = i__ * 3 - 1) < 9 && 0 <= i__1 ? i__1 : s_rnge("m", i__1,
		     "zzrtnmat_", (ftnlen)260)] = north[(i__2 = i__ - 1) < 3 
		    && 0 <= i__2 ? i__2 : s_rnge("north", i__2, "zzrtnmat_", (
		    ftnlen)260)];
	}
    }
    return 0;
} /* zzrtnmat_ */

