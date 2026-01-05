/* pltnrm.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure PLTNRM ( DSK, compute outward normal of plate ) */
/* Subroutine */ int pltnrm_(doublereal *v1, doublereal *v2, doublereal *v3, 
	doublereal *normal)
{
    extern /* Subroutine */ int vsub_(doublereal *, doublereal *, doublereal *
	    );
    doublereal edge1[3], edge2[3];
    extern /* Subroutine */ int vcrss_(doublereal *, doublereal *, doublereal 
	    *);

/* $ Abstract */

/*     Compute an outward normal vector of a triangular plate. */
/*     The vector does not necessarily have unit length. */

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
/*     FILES */
/*     TOPOGRAPHY */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     V1, */
/*     V2, */
/*     V3         I   Vertices of a plate. */
/*     NORMAL     O   Plate's outward normal vector. */

/* $ Detailed_Input */

/*     V1, */
/*     V2, */
/*     V3       are vertices of a triangular plate. */

/* $ Detailed_Output */

/*     NORMAL   is an outward normal vector of the plate defined by */
/*              the input vertices. The order of the vertices is */
/*              used to determine the choice of normal direction: */
/*              the normal vector is */

/*                 ( V2 - V1 ) x ( V3 - V2 ) */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  The input plate may be degenerate: it may be a line segment */
/*         or a point. These are not considered to be erroneous inputs. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine saves computation time by not scaling the output */
/*     vector to unit length. The caller can scale the vector using */
/*     the routine VHAT. */

/* $ Examples */

/*     The numerical results shown for this example may differ across */
/*     platforms. The results depend on the SPICE kernels used as input */
/*     (if any), the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */

/*     1) Compute an upward normal of an equilateral triangle lying */
/*        in the X-Y plane and centered at the origin. */

/*        Example code begins here. */


/*              PROGRAM PLTNRM_EX1 */
/*              IMPLICIT NONE */

/*              DOUBLE PRECISION      NORMAL ( 3 ) */
/*              DOUBLE PRECISION      S */
/*              DOUBLE PRECISION      V1     ( 3 ) */
/*              DOUBLE PRECISION      V2     ( 3 ) */
/*              DOUBLE PRECISION      V3     ( 3 ) */


/*              S = SQRT(3.D0)/2 */

/*              CALL VPACK (    S,  -0.5D0,  0.D0, V1 ) */
/*              CALL VPACK ( 0.D0,    1.D0,  0.D0, V2 ) */
/*              CALL VPACK (   -S,  -0.5D0,  0.D0, V3 ) */


/*              CALL PLTNRM ( V1, V2, V3, NORMAL ) */

/*              WRITE (*, '(A,3F18.14)' ) 'NORMAL = ', NORMAL */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*        NORMAL =   0.00000000000000  0.00000000000000  2.59807621135332 */


/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */

/* $ Version */

/* -    SPICELIB Version 1.0.1, 08-JUL-2020 (JDR) */

/*        Edited the header to comply with NAIF standard.. */
/*        Changed code example output format for the solution to fit */
/*        within the $Examples section without modifications. */

/* -    SPICELIB Version 1.0.0, 26-JAN-2016 (NJB) */

/* -& */
/* $ Index_Entries */

/*     compute normal vector of triangular plate from vertices */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     This routine is error-free. */


/*     Type 2 plate vertices are ordered in the positive */
/*     (right-handed) sense about the outward normal. */

    vsub_(v2, v1, edge1);
    vsub_(v3, v2, edge2);
    vcrss_(edge1, edge2, normal);
    return 0;
} /* pltnrm_ */

