/* el2cgv.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure EL2CGV ( Ellipse to center and generating vectors ) */
/* Subroutine */ int el2cgv_(doublereal *ellips, doublereal *center, 
	doublereal *smajor, doublereal *sminor)
{
    extern /* Subroutine */ int vequ_(doublereal *, doublereal *);

/* $ Abstract */

/*     Convert a SPICE ellipse to a center vector and two generating */
/*     vectors. The selected generating vectors are semi-axes of the */
/*     ellipse. */

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

/* $ Keywords */

/*     ELLIPSE */
/*     GEOMETRY */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     ELLIPS     I   A SPICE ellipse. */
/*     CENTER, */
/*     SMAJOR, */
/*     SMINOR     O   Center and semi-axes of ELLIPS. */

/* $ Detailed_Input */

/*     ELLIPS   is a SPICE ellipse. */

/* $ Detailed_Output */

/*     CENTER, */
/*     SMAJOR, */
/*     SMINOR   are, respectively, a center vector, a semi-major */
/*              axis vector, and a semi-minor axis vector that */
/*              generate the input ellipse. This ellipse is the */
/*              set of points */

/*                 CENTER + cos(theta) SMAJOR + sin(theta) SMINOR */

/*              where theta ranges over the interval (-pi, pi]. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     SPICE ellipses serve to simplify calling sequences and reduce */
/*     the chance for error in declaring and describing argument lists */
/*     involving ellipses. */

/*     The set of ellipse conversion routines is */

/*        CGV2EL ( Center and generating vectors to ellipse ) */
/*        EL2CGV ( Ellipse to center and generating vectors ) */

/*     A word about the output of this routine: the semi-major axis of */
/*     an ellipse is a vector of largest possible magnitude in the set */

/*        cos(theta) VEC1  +  sin(theta) VEC2, */

/*     where theta is in the interval (-pi, pi].  There are two such */
/*     vectors; they are additive inverses of each other. The semi-minor */
/*     axis is an analogous vector of smallest possible magnitude. The */
/*     semi-major and semi-minor axes are orthogonal to each other. If */
/*     SMAJOR and SMINOR are choices of semi-major and semi-minor axes, */
/*     then the input ellipse can also be represented as the set of */
/*     points */


/*        CENTER + cos(theta) SMAJOR + sin(theta) SMINOR */

/*     where theta ranges over the interval (-pi, pi]. */

/* $ Examples */

/*     The numerical results shown for these examples may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */

/*     1) Given a SPICE ellipse structure, extract its components into */
/*        independent variables. */


/*        Example code begins here. */


/*              PROGRAM EL2CGV_EX1 */
/*              IMPLICIT NONE */

/*        C */
/*        C     Local constants. */
/*        C */
/*              INTEGER                 UBEL */
/*              PARAMETER             ( UBEL =   9 ) */

/*        C */
/*        C     Local variables. */
/*        C */
/*              DOUBLE PRECISION        CENTER ( 3    ) */
/*              DOUBLE PRECISION        ECENTR ( 3    ) */
/*              DOUBLE PRECISION        ELLIPS ( UBEL ) */
/*              DOUBLE PRECISION        SMAJOR ( 3    ) */
/*              DOUBLE PRECISION        SMINOR ( 3    ) */
/*              DOUBLE PRECISION        VEC1   ( 3    ) */
/*              DOUBLE PRECISION        VEC2   ( 3    ) */

/*              INTEGER                 I */

/*        C */
/*        C     Define the center and two linearly independent */
/*        C     generating vectors of an ellipse (the vectors need not */
/*        C     be linearly independent). */
/*        C */
/*              DATA                    CENTER / -1.D0,  1.D0, -1.D0 / */

/*              DATA                    VEC1   /  1.D0,  1.D0,  1.D0 / */

/*              DATA                    VEC2   /  1.D0, -1.D0,  1.D0 / */

/*        C */
/*        C     Create the ELLIPS. */
/*        C */
/*              CALL CGV2EL ( CENTER, VEC1, VEC2, ELLIPS ) */

/*        C */
/*        C     In a real application, please use SPICELIB API EL2CGV */
/*        C     to retrieve the center and generating vectors from the */
/*        C     ellipse structure (see next block). */
/*        C */
/*              WRITE(*,'(A)') 'SPICE ellipse:' */
/*              WRITE(*,'(A,3F10.6)') '   Semi-minor axis:', */
/*             .                                    ( ELLIPS(I), I=7,9 ) */
/*              WRITE(*,'(A,3F10.6)') '   Semi-major axis:', */
/*             .                                    ( ELLIPS(I), I=4,6 ) */
/*              WRITE(*,'(A,3F10.6)') '   Center         :', */
/*             .                                    ( ELLIPS(I), I=1,3 ) */
/*              WRITE(*,*) ' ' */

/*        C */
/*        C     Obtain the center and generating vectors from the */
/*        C     ELLIPS. */
/*        C */
/*              CALL EL2CGV ( ELLIPS, ECENTR, SMAJOR, SMINOR ) */

/*              WRITE(*,'(A)') 'SPICE ellipse (using EL2CGV):' */
/*              WRITE(*,'(A,3F10.6)') '   Semi-minor axis:', SMINOR */
/*              WRITE(*,'(A,3F10.6)') '   Semi-major axis:', SMAJOR */
/*              WRITE(*,'(A,3F10.6)') '   Center         :', ECENTR */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*        SPICE ellipse: */
/*           Semi-minor axis:  0.000000  1.414214  0.000000 */
/*           Semi-major axis:  1.414214 -0.000000  1.414214 */
/*           Center         : -1.000000  1.000000 -1.000000 */

/*        SPICE ellipse (using EL2CGV): */
/*           Semi-minor axis:  0.000000  1.414214  0.000000 */
/*           Semi-major axis:  1.414214 -0.000000  1.414214 */
/*           Center         : -1.000000  1.000000 -1.000000 */


/*     2) Given an ellipsoid and a viewpoint exterior to it, calculate */
/*        the limb ellipse as seen from that viewpoint. */


/*        Example code begins here. */


/*              PROGRAM EL2CGV_EX2 */
/*              IMPLICIT NONE */

/*        C */
/*        C     Local constants. */
/*        C */
/*              INTEGER                 UBEL */
/*              PARAMETER             ( UBEL =   9 ) */

/*        C */
/*        C     Local variables. */
/*        C */
/*              DOUBLE PRECISION        A */
/*              DOUBLE PRECISION        B */
/*              DOUBLE PRECISION        C */
/*              DOUBLE PRECISION        ECENTR ( 3    ) */
/*              DOUBLE PRECISION        LIMB   ( UBEL ) */
/*              DOUBLE PRECISION        SMAJOR ( 3    ) */
/*              DOUBLE PRECISION        SMINOR ( 3    ) */
/*              DOUBLE PRECISION        VIEWPT ( 3    ) */

/*        C */
/*        C     Define a viewpoint exterior to the ellipsoid. */
/*        C */
/*              DATA                    VIEWPT /  2.D0,  0.D0,  0.D0 / */

/*        C */
/*        C     Define an ellipsoid. */
/*        C */
/*              A = SQRT( 2.D0 ) */
/*              B = 2.D0 * SQRT( 2.D0 ) */
/*              C = SQRT( 2.D0 ) */

/*        C */
/*        C     Calculate the limb ellipse as seen by from the */
/*        C     viewpoint. */
/*        C */
/*              CALL EDLIMB ( A, B, C, VIEWPT, LIMB ) */

/*        C */
/*        C     Output the structure components. */
/*        C */
/*              CALL EL2CGV ( LIMB, ECENTR, SMAJOR, SMINOR ) */

/*              WRITE(*,'(A)') 'Limb ellipse as seen from viewpoint:' */
/*              WRITE(*,'(A,3F11.6)') '   Semi-minor axis:', SMINOR */
/*              WRITE(*,'(A,3F11.6)') '   Semi-major axis:', SMAJOR */
/*              WRITE(*,'(A,3F11.6)') '   Center         :', ECENTR */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*        Limb ellipse as seen from viewpoint: */
/*           Semi-minor axis:   0.000000   0.000000  -1.000000 */
/*           Semi-major axis:   0.000000   2.000000  -0.000000 */
/*           Center         :   1.000000   0.000000   0.000000 */


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
/*        Added complete code example. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 02-NOV-1990 (NJB) */

/* -& */
/* $ Index_Entries */

/*     ellipse to center and generating vectors */

/* -& */

/*     Local parameters */


/*     SPICE ellipses contain a center vector, a semi-major */
/*     axis vector, and a semi-minor axis vector.  These are */
/*     located, respectively, in elements */

/*        CTRPOS through CTRPOS + 1 */

/*        MAJPOS through MAJPOS + 1 */

/*        MINPOS through MINPOS + 1 */



/*     The center of the ellipse is held in the first three elements. */
/*     The semi-major and semi-minor axes come next. */

    vequ_(ellips, center);
    vequ_(&ellips[3], smajor);
    vequ_(&ellips[6], sminor);
    return 0;
} /* el2cgv_ */

