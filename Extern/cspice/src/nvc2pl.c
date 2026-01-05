/* nvc2pl.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure NVC2PL ( Normal vector and constant to plane ) */
/* Subroutine */ int nvc2pl_(doublereal *normal, doublereal *konst, 
	doublereal *plane)
{
    extern /* Subroutine */ int vequ_(doublereal *, doublereal *), chkin_(
	    char *, ftnlen), unorm_(doublereal *, doublereal *, doublereal *),
	     sigerr_(char *, ftnlen), chkout_(char *, ftnlen);
    doublereal tmpvec[3];
    extern /* Subroutine */ int setmsg_(char *, ftnlen);
    extern logical return_(void);
    extern /* Subroutine */ int vminus_(doublereal *, doublereal *);
    doublereal mag;

/* $ Abstract */

/*     Make a SPICE plane from a normal vector and a constant. */

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
/*     KONST      I   A normal vector and constant defining a plane. */
/*     PLANE      O   An array representing the plane. */

/* $ Detailed_Input */

/*     NORMAL, */
/*     KONST    are, respectively, a normal vector and constant */
/*              defining a plane. NORMAL need not be a unit */
/*              vector. Let the symbol < a, b > indicate the inner */
/*              product of vectors a and b; then the geometric */
/*              plane is the set of vectors X in three-dimensional */
/*              space that satisfy */

/*                 < X,  NORMAL >  =  KONST. */

/* $ Detailed_Output */

/*     PLANE    is a SPICE plane that represents the geometric */
/*              plane defined by NORMAL and KONST. */

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

/*     The numerical results shown for these examples may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */

/*     1) Construct a SPICE plane from a normal vector and a constant. */


/*        Example code begins here. */


/*              PROGRAM NVC2PL_EX1 */
/*              IMPLICIT NONE */

/*        C */
/*        C     Local constants. */
/*        C */
/*              INTEGER                 UBPL */
/*              PARAMETER             ( UBPL =   4 ) */

/*        C */
/*        C     Local variables. */
/*        C */
/*              DOUBLE PRECISION        KONST */
/*              DOUBLE PRECISION        PLANE  ( UBPL ) */
/*              DOUBLE PRECISION        NORMAL ( 3    ) */
/*              DOUBLE PRECISION        OKONST */
/*              DOUBLE PRECISION        ONORML ( 3    ) */

/*        C */
/*        C     Set the normal vector and the constant defining the */
/*        C     plane. */
/*        C */
/*              DATA                    NORMAL /  1.D0, 1.D0, 1.D0 / */

/*              KONST = 23.D0 */

/*              WRITE(*,'(A)') 'Inputs:' */
/*              WRITE(*,'(A,3F12.7)') '   Normal vector:', NORMAL */
/*              WRITE(*,'(A,F12.7)')  '   Constant     :', KONST */
/*              WRITE(*,*) ' ' */

/*        C */
/*        C     Make a SPICE plane from NORMAL and KONST. */
/*        C     NORMAL need not be a unit vector. */
/*        C */
/*              CALL NVC2PL ( NORMAL, KONST,  PLANE         ) */

/*        C */
/*        C     Print the results. */
/*        C */
/*              CALL PL2NVC ( PLANE, ONORML, OKONST ) */
/*              WRITE(*,'(A)') 'Generated plane:' */
/*              WRITE(*,'(A,3F12.7)') '   Normal vector:', ONORML */
/*              WRITE(*,'(A,F12.7)')  '   Constant     :', OKONST */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*        Inputs: */
/*           Normal vector:   1.0000000   1.0000000   1.0000000 */
/*           Constant     :  23.0000000 */

/*        Generated plane: */
/*           Normal vector:   0.5773503   0.5773503   0.5773503 */
/*           Constant     :  13.2790562 */


/*     2) Apply a linear transformation represented by a matrix to */
/*        a plane represented by a normal vector and a constant. */

/*        Find a normal vector and constant for the transformed plane. */


/*        Example code begins here. */


/*              PROGRAM NVC2PL_EX2 */
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

/*     1)  No checking is done to prevent arithmetic overflow. */

/* $ Literature_References */

/*     [1]  G. Thomas and R. Finney, "Calculus and Analytic Geometry," */
/*          7th Edition, Addison Wesley, 1988. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.2.0, 24-AUG-2021 (JDR) */

/*        Changed the input argument name CONST to KONST for consistency */
/*        with other routines. */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. Removed */
/*        unnecessary $Revisions section. */

/*        Added complete code examples. */

/* -    SPICELIB Version 1.1.1, 02-NOV-2009 (NJB) */

/*        Corrected header typo. */

/* -    SPICELIB Version 1.1.0, 30-AUG-2005 (NJB) */

/*        Updated to remove non-standard use of duplicate arguments */
/*        in VMINUS call. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 01-NOV-1990 (NJB) */

/* -& */
/* $ Index_Entries */

/*     normal vector and constant to plane */

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
    unorm_(normal, plane, &mag);

/*     The normal vector must be non-zero. */

    if (mag == 0.) {
	chkin_("NVC2PL", (ftnlen)6);
	setmsg_("Plane's normal must be non-zero.", (ftnlen)32);
	sigerr_("SPICE(ZEROVECTOR)", (ftnlen)17);
	chkout_("NVC2PL", (ftnlen)6);
	return 0;
    }

/*     To find the plane constant corresponding to the unitized normal */
/*     vector, we observe that */

/*        < X, NORMAL > = KONST, */

/*     so */

/*        < X, NORMAL / || NORMAL || >   =   KONST / || NORMAL || */


    plane[3] = *konst / mag;

/*     The constant should be the distance of the plane from the */
/*     origin.  If the constant is negative, negate both it and the */
/*     normal vector. */

    if (plane[3] < 0.) {
	plane[3] = -plane[3];
	vminus_(plane, tmpvec);
	vequ_(tmpvec, plane);
    }
    return 0;
} /* nvc2pl_ */

