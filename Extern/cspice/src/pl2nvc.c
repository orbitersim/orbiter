/* pl2nvc.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure PL2NVC ( Plane to normal vector and constant ) */
/* Subroutine */ int pl2nvc_(doublereal *plane, doublereal *normal, 
	doublereal *konst)
{
    extern /* Subroutine */ int vequ_(doublereal *, doublereal *);

/* $ Abstract */

/*     Return a unit normal vector and constant that define a specified */
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
/*     KONST      O   A normal vector and constant defining the */
/*                    geometric plane represented by PLANE. */
/*     UBPL       P   SPICE plane upper bound. */

/* $ Detailed_Input */

/*     PLANE    is a SPICE plane. */

/* $ Detailed_Output */

/*     NORMAL, */
/*     KONST    are, respectively, a unit normal vector and */
/*              constant that define the geometric plane */
/*              represented by PLANE. Let the symbol < A, B > */
/*              indicate the inner product of vectors A and B; */
/*              then the geometric plane is the set of vectors X */
/*              in three-dimensional space that satisfy */

/*                 < X,  NORMAL >  =  KONST. */

/*              NORMAL is a unit vector. KONST is the distance of */
/*              the plane from the origin; */

/*                 KONST * NORMAL */

/*              is the closest point in the plane to the origin. */

/* $ Parameters */

/*     UBPL     is the upper bound of a SPICE plane array. */

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

/*     The numerical results shown for these examples may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */

/*     1) Determine the distance of a plane from the origin, and */
/*        confirm the result by calculating the dot product (inner */
/*        product) of a vector from the origin to the plane and a */
/*        vector in that plane. */

/*        The dot product between these two vectors should be zero, */
/*        to double precision round-off, so orthogonal to that */
/*        precision. */


/*        Example code begins here. */


/*              PROGRAM PL2NVC_EX1 */
/*              IMPLICIT NONE */

/*        C */
/*        C     SPICELIB functions. */
/*        C */
/*              DOUBLE PRECISION        VDOT */

/*        C */
/*        C     Local constants. */
/*        C */
/*              INTEGER                 UBPL */
/*              PARAMETER             ( UBPL =   4 ) */

/*        C */
/*        C     Local variables. */
/*        C */
/*              DOUBLE PRECISION        DOTP */
/*              DOUBLE PRECISION        KONST */
/*              DOUBLE PRECISION        PLANE  ( UBPL ) */
/*              DOUBLE PRECISION        NORMAL ( 3    ) */
/*              DOUBLE PRECISION        PLNVEC ( 3    ) */
/*              DOUBLE PRECISION        POINT  ( 3    ) */
/*              DOUBLE PRECISION        VEC    ( 3    ) */

/*        C */
/*        C     Define the plane with a vector normal to the plan */
/*        C     and a point in the plane. */
/*        C */
/*              DATA                    NORMAL / -1.D0,  5.D0,   -3.5D0 / */
/*              DATA                    POINT  /  9.D0, -0.65D0, -12.D0 / */

/*        C */
/*        C     Create the SPICE plane from the normal and point. */
/*        C */
/*              CALL NVP2PL ( NORMAL, POINT, PLANE ) */

/*        C */
/*        C     Calculate the normal vector and constant defining */
/*        C     the plane. The constant value is the distance from */
/*        C     the origin to the plane. */
/*        C */
/*              CALL PL2NVC ( PLANE, NORMAL, KONST ) */
/*              WRITE(*,'(A,F12.7)') 'Distance to the plane:', KONST */

/*        C */
/*        C     Confirm the results. Calculate a vector */
/*        C     from the origin to the plane. */
/*        C */
/*              CALL VSCL ( KONST, NORMAL, VEC ) */
/*              WRITE(*,'(A,3F12.7)') 'Vector from origin   :', VEC */
/*              WRITE(*,*) ' ' */

/*        C */
/*        C     Now calculate a vector in the plane from the */
/*        C     location in the plane defined by VEC. */
/*        C */
/*              CALL VSUB ( VEC, POINT, PLNVEC ) */

/*        C */
/*        C     These vectors should be orthogonal. */
/*        C */
/*              WRITE(*,'(A,F12.7)') 'dot product          :', */
/*             .                     VDOT( PLNVEC, VEC ) */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*        Distance to the plane:   4.8102899 */
/*        Vector from origin   :  -0.7777778   3.8888889  -2.7222222 */

/*        dot product          :  -0.0000000 */


/*     2) Apply a linear transformation represented by a matrix to */
/*        a plane represented by a normal vector and a constant. */

/*        Find a normal vector and constant for the transformed plane. */


/*        Example code begins here. */


/*              PROGRAM PL2NVC_EX2 */
/*              IMPLICIT NONE */

/*        C */
/*        C     Local constants. */
/*        C */
/*              INTEGER                 UBPL */
/*              PARAMETER             ( UBPL =   4 ) */

/*        C */
/*        C     Local variables. */
/*        C */
/*              DOUBLE PRECISION        AXDEF  ( 3    ) */
/*              DOUBLE PRECISION        KONST */
/*              DOUBLE PRECISION        PLANE  ( UBPL ) */
/*              DOUBLE PRECISION        M      ( 3, 3 ) */
/*              DOUBLE PRECISION        NORMAL ( 3    ) */
/*              DOUBLE PRECISION        PLNDEF ( 3    ) */
/*              DOUBLE PRECISION        POINT  ( 3    ) */
/*              DOUBLE PRECISION        SPAN1  ( 3    ) */
/*              DOUBLE PRECISION        SPAN2  ( 3    ) */
/*              DOUBLE PRECISION        TKONST */
/*              DOUBLE PRECISION        TNORML ( 3    ) */
/*              DOUBLE PRECISION        TPLANE ( UBPL ) */
/*              DOUBLE PRECISION        TPOINT ( 3    ) */
/*              DOUBLE PRECISION        TSPAN1 ( 3    ) */
/*              DOUBLE PRECISION        TSPAN2 ( 3    ) */

/*        C */
/*        C     Set the normal vector and the constant defining the */
/*        C     initial plane. */
/*        C */
/*              DATA                    NORMAL / */
/*             .               -0.1616904D0, 0.8084521D0, -0.5659165D0 / */

/*              DATA                    KONST  /  4.8102899D0 / */

/*        C */
/*        C     Define a transformation matrix to the right-handed */
/*        C     reference frame having the +i unit vector as primary */
/*        C     axis, aligned to the original frame's +X axis, and */
/*        C     the -j unit vector as second axis, aligned to the +Y */
/*        C     axis. */
/*        C */
/*              DATA                    AXDEF  /  1.D0,  0.D0,  0.D0 / */
/*              DATA                    PLNDEF /  0.D0, -1.D0,  0.D0 / */


/*              CALL TWOVEC ( AXDEF, 1, PLNDEF, 2, M ) */

/*        C */
/*        C     Make a SPICE plane from NORMAL and KONST, and then */
/*        C     find a point in the plane and spanning vectors for the */
/*        C     plane.  NORMAL need not be a unit vector. */
/*        C */
/*              CALL NVC2PL ( NORMAL, KONST,  PLANE         ) */
/*              CALL PL2PSV ( PLANE,  POINT,  SPAN1,  SPAN2 ) */

/*        C */
/*        C     Apply the linear transformation to the point and */
/*        C     spanning vectors.  All we need to do is multiply */
/*        C     these vectors by M, since for any linear */
/*        C     transformation T, */
/*        C */
/*        C           T ( POINT  +  t1 * SPAN1     +  t2 * SPAN2 ) */
/*        C */
/*        C        =  T (POINT)  +  t1 * T(SPAN1)  +  t2 * T(SPAN2), */
/*        C */
/*        C     which means that T(POINT), T(SPAN1), and T(SPAN2) */
/*        C     are a point and spanning vectors for the transformed */
/*        C     plane. */
/*        C */
/*              CALL MXV ( M, POINT, TPOINT ) */
/*              CALL MXV ( M, SPAN1, TSPAN1 ) */
/*              CALL MXV ( M, SPAN2, TSPAN2 ) */

/*        C */
/*        C     Make a new SPICE plane TPLANE from the */
/*        C     transformed point and spanning vectors, and find a */
/*        C     unit normal and constant for this new plane. */
/*        C */
/*              CALL PSV2PL ( TPOINT, TSPAN1, TSPAN2, TPLANE ) */
/*              CALL PL2NVC ( TPLANE, TNORML, TKONST         ) */

/*        C */
/*        C     Print the results. */
/*        C */
/*              WRITE(*,'(A,3F12.7)') 'Unit normal vector:', TNORML */
/*              WRITE(*,'(A,F12.7)')  'Constant          :', TKONST */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*        Unit normal vector:  -0.1616904  -0.8084521   0.5659165 */
/*        Constant          :   4.8102897 */


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

/*        Changed the output argument name CONST to KONST for consistency */
/*        with other routines. */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. Added complete */
/*        code examples. Added documentation of the parameter UBPL. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 01-NOV-1990 (NJB) */

/* -& */
/* $ Index_Entries */

/*     plane to normal vector and constant */

/* -& */

/*     The contents of SPICE planes are as follows: */

/*        Elements NMLPOS through NMLPOS + 2 contain a unit normal */
/*        vector for the plane. */

/*        Element CONPOS contains a constant for the plane;  every point */
/*        X in the plane satisfies */

/*           < X, PLANE(NMLPOS) >  =  PLANE(CONPOS). */

/*        The plane constant is the distance of the plane from the */
/*        origin; the normal vector, scaled by the constant, is the */
/*        closest point in the plane to the origin. */



/*     Note for programmers: validity of the input plane is not */
/*     checked in the interest of efficiency. The input plane will be */
/*     valid if it was created by one of the SPICE plane construction */
/*     routines. */

/*     Unpack the plane. */

    vequ_(plane, normal);
    *konst = plane[3];
    return 0;
} /* pl2nvc_ */

