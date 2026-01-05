/* xf2eul.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__6 = 6;

/* $Procedure XF2EUL ( State transformation to Euler angles ) */
/* Subroutine */ int xf2eul_0_(int n__, doublereal *xform, integer *axisa, 
	integer *axisb, integer *axisc, doublereal *eulang, logical *unique)
{
    /* Initialized data */

    static doublereal delta[9]	/* was [3][3] */ = { 0.,-1.,1.,1.,0.,-1.,-1.,
	    1.,0. };
    static integer next[3] = { 2,3,1 };

    /* System generated locals */
    integer i__1, i__2;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer);
    double cos(doublereal), sin(doublereal);

    /* Local variables */
    doublereal drdt[9]	/* was [3][3] */;
    extern /* Subroutine */ int mxmt_(doublereal *, doublereal *, doublereal *
	    ), m2eul_(doublereal *, integer *, integer *, integer *, 
	    doublereal *, doublereal *, doublereal *), eul2m_(doublereal *, 
	    doublereal *, doublereal *, integer *, integer *, integer *, 
	    doublereal *);
    integer a, b;
    doublereal d__;
    integer i__, j, k, l;
    doublereal r__[9]	/* was [3][3] */, u, v, omega[3];
    extern /* Subroutine */ int chkin_(char *, ftnlen), moved_(doublereal *, 
	    integer *, doublereal *);
    doublereal ca;
    extern logical failed_(void);
    doublereal sa, domega[3], locang[6];
    integer locaxa, locaxb, locaxc;
    extern /* Subroutine */ int chkout_(char *, ftnlen);
    doublereal drdtrt[9]	/* was [3][3] */;
    extern logical return_(void);
    doublereal solutn[9]	/* was [3][3] */;
    extern /* Subroutine */ int mxm_(doublereal *, doublereal *, doublereal *)
	    , mxv_(doublereal *, doublereal *, doublereal *);

/* $ Abstract */

/*     Convert a state transformation matrix to Euler angles and their */
/*     derivatives, given a specified set of axes. */

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

/*     PCK */
/*     ROTATION */

/* $ Keywords */

/*     ANGLES */
/*     DERIVATIVES */
/*     STATE */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     XFORM      I   A state transformation matrix. */
/*     AXISA      I   Axis A of the Euler angle factorization. */
/*     AXISB      I   Axis B of the Euler angle factorization. */
/*     AXISC      I   Axis C of the Euler angle factorization. */
/*     EULANG     O   An array of Euler angles and their derivatives. */
/*     UNIQUE     O   Indicates if EULANG is a unique representation. */

/* $ Detailed_Input */

/*     XFORM    is a state transformation matrix from some frame FRAME1 */
/*              to another frame FRAME2. Pictorially, XFORM has the */
/*              structure shown here. */

/*                 .-             -. */
/*                 |       |       | */
/*                 |   R   |   0   | */
/*                 |       |       | */
/*                 |-------+-------| */
/*                 |       |       | */
/*                 | dR/dt |   R   | */
/*                 |       |       | */
/*                 `-             -' */

/*              where R is a rotation matrix that varies with respect to */
/*              time and dR/dt is its time derivative. */

/*              More specifically, if S1 is the state of some object */
/*              in FRAME1, then S2, the state of the same object */
/*              relative to FRAME2 is given by */

/*                 S2 = XFORM * S1 */

/*              where "*" denotes the matrix vector product. */

/*     AXISA, */
/*     AXISB, */
/*     AXISC    are the axes desired for the factorization of R. */

/*              All must be in the range from 1 to 3. Moreover */
/*              it must be the case that AXISA and AXISB are distinct */
/*              and that AXISB and AXISC are distinct. */

/*              Every rotation matrix can be represented as a product */
/*              of three rotation matrices about the principal axes */
/*              of a reference frame. */

/*                 R =  [ ALPHA ]      [ BETA ]      [ GAMMA ] */
/*                               AXISA         AXISB          AXISC */

/*              The value 1 corresponds to the X axis. */
/*              The value 2 corresponds to the Y axis. */
/*              The value 3 corresponds to the Z axis. */

/* $ Detailed_Output */

/*     EULANG   is the set of Euler angles corresponding to the */
/*              specified factorization. */

/*              If we represent R as shown here: */

/*                 R =  [ ALPHA ]      [ BETA ]      [ GAMMA ] */
/*                               AXISA         AXISB          AXISC */

/*              then */

/*                 EULANG(1) = ALPHA */
/*                 EULANG(2) = BETA */
/*                 EULANG(3) = GAMMA */
/*                 EULANG(4) = dALPHA/dt */
/*                 EULANG(5) = dBETA/dt */
/*                 EULANG(6) = dGAMMA/dt */

/*              The range of ALPHA and GAMMA is (-pi, pi]. */

/*              The range of BETA depends on the exact set of */
/*              axes used for the factorization. For */
/*              factorizations in which the first and third axes */
/*              are the same, the range of BETA is [0, pi]. */

/*              For factorizations in which the first and third */
/*              axes are different, the range of BETA is */
/*              [-pi/2, pi/2]. */

/*              For rotations such that ALPHA and GAMMA are not */
/*              uniquely determined, ALPHA and dALPHA/dt will */
/*              always be set to zero; GAMMA and dGAMMA/dt are */
/*              then uniquely determined. */

/*     UNIQUE   is a logical that indicates whether or not the */
/*              values in EULANG are uniquely determined. If */
/*              the values are unique then UNIQUE will be set to */
/*              .TRUE. If the values are not unique and some */
/*              components ( EULANG(1) and EULANG(4) ) have been set */
/*              to zero, then UNIQUE will have the value .FALSE. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If any of AXISA, AXISB, or AXISC do not have values in */

/*            { 1, 2, 3 } */

/*         an error is signaled by a routine in the call tree of this */
/*         routine. */

/*     2)  If AXISB is equal to AXISC or AXISA, an error is signaled by a */
/*         routine in the call tree of this routine. An arbitrary */
/*         rotation matrix cannot be expressed using a sequence of Euler */
/*         angles unless the second rotation axis differs from the other */
/*         two. */

/*     3)  If the input matrix XFORM is not a rotation matrix, an error */
/*         is signaled by a routine in the call tree of this routine. */

/*     4)  If EULANG(1) and EULANG(3) are not uniquely determined, */
/*         EULANG(1) is set to zero, and EULANG(3) is determined. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     A word about notation: the symbol */

/*        [ x ] */
/*             i */

/*     indicates a coordinate system rotation of x radians about the */
/*     ith coordinate axis. To be specific, the symbol */

/*        [ x ] */
/*             1 */

/*     indicates a coordinate system rotation of x radians about the */
/*     first, or x-, axis; the corresponding matrix is */

/*        .-                    -. */
/*        |  1      0       0    | */
/*        |                      | */
/*        |  0    cos(x)  sin(x) | */
/*        |                      | */
/*        |  0   -sin(x)  cos(x) | */
/*        `-                    -' */

/*     Remember, this is a COORDINATE SYSTEM rotation by x radians; this */
/*     matrix, when applied to a vector, rotates the vector by -x */
/*     radians, not x radians. Applying the matrix to a vector yields */
/*     the vector's representation relative to the rotated coordinate */
/*     system. */

/*     The analogous rotation about the second, or y-, axis is */
/*     represented by */

/*        [ x ] */
/*             2 */

/*     which symbolizes the matrix */

/*        .-                    -. */
/*        | cos(x)   0   -sin(x) | */
/*        |                      | */
/*        |  0       1      0    | */
/*        |                      | */
/*        | sin(x)   0    cos(x) | */
/*        `-                    -' */

/*     and the analogous rotation about the third, or z-, axis is */
/*     represented by */

/*        [ x ] */
/*             3 */

/*     which symbolizes the matrix */

/*        .-                    -. */
/*        |  cos(x)  sin(x)   0  | */
/*        |                      | */
/*        | -sin(x)  cos(x)   0  | */
/*        |                      | */
/*        |  0        0       1  | */
/*        `-                    -' */

/*     The input matrix is assumed to be the product of three */
/*     rotation matrices, each one of the form */

/*        .-                    -. */
/*        |  1      0       0    | */
/*        |                      | */
/*        |  0    cos(r)  sin(r) |     (rotation of r radians about the */
/*        |                      |      x-axis), */
/*        |  0   -sin(r)  cos(r) | */
/*        `-                    -' */


/*        .-                    -. */
/*        | cos(s)   0   -sin(s) | */
/*        |                      | */
/*        |  0       1      0    |     (rotation of s radians about the */
/*        |                      |      y-axis), */
/*        | sin(s)   0    cos(s) | */
/*        `-                    -' */

/*     or */

/*        .-                    -. */
/*        |  cos(t)  sin(t)   0  | */
/*        |                      | */
/*        | -sin(t)  cos(t)   0  |     (rotation of t radians about the */
/*        |                      |      z-axis), */
/*        |  0        0       1  | */
/*        `-                    -' */

/*     where the second rotation axis is not equal to the first or */
/*     third. Any rotation matrix can be factored as a sequence of */
/*     three such rotations, provided that this last criterion is met. */

/*     This routine is related to the routine EUL2XF which produces */
/*     a state transformation from an input set of axes, Euler angles */
/*     and derivatives. */

/*     The two subroutine calls shown here will not change */
/*     XFORM except for round off errors. */

/*        CALL XF2EUL ( XFORM,  AXISA, AXISB, AXISC, EULANG, UNIQUE ) */
/*        CALL EUL2XF ( EULANG, AXISA, AXISB, AXISC, XFORM          ) */

/*     On the other hand the two calls */

/*        CALL EUL2XF ( EULANG, AXISA, AXISB, AXISC, XFORM          ) */
/*        CALL XF2EUL ( XFORM,  AXISA, AXISB, AXISC, EULANG, UNIQUE ) */

/*     will leave EULANG unchanged only if the components of EULANG */
/*     are in the range produced by XF2EUL and the Euler representation */
/*     of the rotation component of XFORM is unique within that range. */

/* $ Examples */

/*     The numerical results shown for this example may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */

/*     1) Determine the rate of change of the right ascension and */
/*        declination of the pole of the moon, from the state */
/*        transformation matrix that transforms J2000 states to object */
/*        fixed states. */

/*        Recall that the rotation component of the state transformation */
/*        matrix is given by */

/*           [W]  [HALFPI-DEC]  [RA+HALFPI] */
/*              3             1            3 */


/*        Use the meta-kernel shown below to load the required SPICE */
/*        kernels. */


/*           KPL/MK */

/*           File name: xf2eul_ex1.tm */

/*           This meta-kernel is intended to support operation of SPICE */
/*           example programs. The kernels shown here should not be */
/*           assumed to contain adequate or correct versions of data */
/*           required by SPICE-based user applications. */

/*           In order for an application to use this meta-kernel, the */
/*           kernels referenced here must be present in the user's */
/*           current working directory. */

/*           The names and contents of the kernels referenced */
/*           by this meta-kernel are as follows: */

/*              File name                     Contents */
/*              ---------                     -------- */
/*              pck00010.tpc                  Planet orientation and */
/*                                            radii */
/*              naif0012.tls                  Leapseconds */


/*           \begindata */

/*              KERNELS_TO_LOAD = ( 'pck00010.tpc', */
/*                                  'naif0012.tls'  ) */

/*           \begintext */

/*           End of meta-kernel */


/*        Example code begins here. */


/*              PROGRAM XF2EUL_EX1 */
/*              IMPLICIT NONE */

/*        C */
/*        C     SPICELIB functions. */
/*        C */
/*              DOUBLE PRECISION      HALFPI */

/*        C */
/*        C     Local parameters. */
/*        C */
/*              CHARACTER*(*)         META */
/*              PARAMETER           ( META   = 'xf2eul_ex1.tm' ) */

/*              CHARACTER*(*)         UTCSTR */
/*              PARAMETER           ( UTCSTR = 'May 15, 2007' ) */

/*        C */
/*        C     Local variables. */
/*        C */
/*              DOUBLE PRECISION      EULANG ( 6    ) */
/*              DOUBLE PRECISION      ET */
/*              DOUBLE PRECISION      FTMTRX ( 6, 6 ) */

/*              INTEGER               I */
/*              INTEGER               J */

/*              LOGICAL               UNIQUE */

/*        C */
/*        C     Load SPICE kernels. */
/*        C */
/*              CALL FURNSH ( META ) */

/*        C */
/*        C     Convert the input time to seconds past J2000 TDB. */
/*        C */
/*              CALL STR2ET ( UTCSTR, ET ) */

/*        C */
/*        C     Get the transformation matrix from J2000 frame to */
/*        C     IAU_MOON. */
/*        C */
/*              CALL SXFORM ( 'J2000', 'IAU_MOON', ET, FTMTRX ) */

/*        C */
/*        C     Convert the transformation matrix to */
/*        C     Euler angles (3-1-3). */
/*        C */
/*              CALL XF2EUL ( FTMTRX, 3, 1, 3, EULANG, UNIQUE ) */

/*        C */
/*        C     Display the results. */
/*        C */
/*              IF ( UNIQUE ) THEN */

/*                 WRITE(*,'(2A)') 'UTC: ', UTCSTR */
/*                 WRITE(*,'(A,F20.16)') 'W       = ', EULANG(1) */
/*                 WRITE(*,'(A,F20.16)') 'DEC     = ', */
/*             .                  HALFPI() - EULANG(2) */
/*                 WRITE(*,'(A,F20.16)') 'RA      = ', */
/*             .                  EULANG(3) - HALFPI() */
/*                 WRITE(*,'(A,F20.16)') 'dW/dt   = ', EULANG(4) */
/*                 WRITE(*,'(A,F20.16)') 'dDEC/dt = ', EULANG(5) */
/*                 WRITE(*,'(A,F20.16)') 'dRA/dt  = ', EULANG(6) */

/*              ELSE */

/*                 WRITE(*,*) 'The values in EULANG are not uniquely ' */
/*             .          //  'determined.' */

/*              END IF */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*        UTC: May 15, 2007 */
/*        W       =  -2.6490877296701645 */
/*        DEC     =   1.1869108599473206 */
/*        RA      =  -1.5496443908099826 */
/*        dW/dt   =   0.0000026578085601 */
/*        dDEC/dt =   0.0000000004021737 */
/*        dRA/dt  =   0.0000000039334471 */


/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     W.L. Taber         (JPL) */
/*     E.D. Wright        (JPL) */

/* $ Version */

/* -    SPICELIB Version 2.1.0, 26-OCT-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. Removed */
/*        unnecessary $Revisions section. */

/*        Added complete code example based on existing fragment. */
/*        Corrected input argument name in $Exceptions section. */

/* -    SPICELIB Version 2.0.1, 25-APR-2007 (EDW) */

/*        Corrected code in EUL2EF entry point $Examples section, example */
/*        showed a XF2EUL call: */

/*           CALL XF2EUL ( XFORM,  1, 2, 3, RPYANG ) */

/*        The proper form of the call: */

/*           CALL XF2EUL ( XFORM,  1, 2, 3, RPYANG, UNIQUE ) */

/* -    SPICELIB Version 2.0.0, 31-OCT-2005 (NJB) */

/*        Entry point EUL2XF was updated to allow axis sequences */
/*        in which the second angle is not distinct from the first */
/*        or third. */

/* -    SPICELIB Version 1.0.0, 31-JUL-1995 (WLT) */

/* -& */
/* $ Index_Entries */

/*     Euler angles and derivatives from state transformation */

/* -& */

/*     Spicelib Functions */


/*     Parameters */


/*     Local variables */


/*     Saved variables */


/*     Initial values */

/*     Keep in mind that matrices are stored in column order first so */
/*     the matrix below looks like the transpose of what's needed.  But */
/*     in fact it is the correct thing. */

    switch(n__) {
	case 1: goto L_eul2xf;
	}


/*     The computation of the non-derivative terms EULANG is handled */
/*     by the SPICE routine M2EUL.  This routine contributes by */
/*     determining the derivative components of EULANG. */

/*     To understand the code below a rather lengthy derivation is */
/*     required.  If you're not interested in the details of this */
/*     derivation skip down to the  IF ( RETURN() ) THEN line of */
/*     code below. */

/*     First we note that if b is one of the basis vectors i,j, or k */
/*     or the opposite of one of these (-i, -j, or -k) then */

/*       [ ANGLE ]  * b  = COS( {1 - |<e_n,b>|}*ANGLE )b */
/*                n */
/*                       - SIN( ANGLE ) e_n x b */

/*     where <,> denotes the dot product, and x is used to denote the */
/*     cross product operation and e_1, e_2, and e_3 are the standard */
/*     basis vectors i, j, and k respectively. */

/*     Using M2EUL we can readily determine the values of ALPHA, BETA */
/*     and GAMMA such that */


/*        R   = [ ALPHA ]  [ BETA ]  [ GAMMA ] */
/*                       A         B          C */


/*    From this equation we have: */

/*        dR/dt =   dALPHA/dt OMEGA [ ALPHA ]  [ BETA ]  [ GAMMA ] */
/*                                 A         A         B          C */

/*              +   dBETA/dt  [ ALPHA ] OMEGA  [ BETA ]  [ GAMMA ] */
/*                                     A     B         B          C */

/*              +   dGAMMA/dt [ ALPHA ] [ BETA ]  OMEGA [ GAMMA ] */
/*                                     A        B      C         C */

/*     where OMEGA   is the cross product matrix. */
/*                n */


/*         [   0      D_3n    -D_2n  ] */
/*         |  -D_3n    0       D_1n  | */
/*         [   D_2n  -D_1n      0    ] */


/*     (D_ij   denotes the Kronecker delta.)  Note that OMEGA * v */
/*                                                           n */
/*     yields -e  x  v  for all vectors v. */
/*              n */

/*     Multiplying both sides of the equation for dR/dt by the transpose */
/*     of R yields: */

/*            T */
/*     dR/dt*R  = dALPHA/dt OMEGA */
/*                                A */

/*              + dBETA/dt  [ ALPHA ] OMEGA  [ -ALPHA ] */
/*                                   A     B           A */

/*              + dGAMMA/dt [ ALPHA ] [ BETA ] OMEGA [ -BETA ]  [-ALPHA] */
/*                                   A        B     C         B         A */
/*                        T */
/*     The product dR/dt*R  is a skew symmetric matrix and hence can */
/*     be represented as a cross product, */
/*               T */
/*        dR/dt*R  V  = W x V */

/*     for all vectors V, provided that */

/*                       T */
/*        W(1) =  dR/dt*R  (3,2) */

/*                       T */
/*        W(2) =  dR/dt*R  (1,3) */

/*                       T */
/*        W(3) =  dR/dt*R  (2,1) */

/*     For any vector V, there is a corresponding skew symmetric */
/*     matrix CROSS{V}  such that CROSS{V} * W  = V x W for all vectors */
/*     W.  Moreover, if ROT is any rotation, then */

/*                                           T */
/*           CROSS{ROT(V)} = ROT CROSS{V} ROT */

/*     This can easily be verified by noting that */

/*        ROT(VxU) = ROT(V) X ROT(U) */

/*     From these observations it follows that */


/*        W =   -dALPHA/dt e_A */


/*          -    dBETA/dt [ALPHA]  e_B */
/*                               A */

/*          -    dGAMMA/dt [ ALPHA ] [ BETA ] e_C */
/*                                  A        B */


/*        W =   -dALPHA/dt e_A */


/*          -    dBETA/dt {    COS ( ALPHA (1 - |<e_A,e_B>|)) e_B */

/*                          -  SIN ( ALPHA ) e_A x e_B } */


/*          -    dGAMMA/dt [ ALPHA ] {    COS(BETA(1 - |<e_B,e_C>|)) e_C */
/*                                  A */
/*                                     -  SIN (BETA) e_B x e_C } */

/*     But <e_A,e_B> = 0 = <e_B,e_C> so that the above expression */
/*     simplifies to */

/*        W =   -dALPHA/dt e_A */


/*          -    dBETA/dt {COS(ALPHA)e_B -  SIN(ALPHA) e_A x e_B} */


/*          -    dGAMMA/dt [ ALPHA ] {COS(BETA)e_C - SIN(BETA)e_B x e_C} */
/*                                  A */

/*     If we let L = 6 - A - B, then by construction e_L is the third */
/*     vector needed to complete the basis containing e_A and e_B. */
/*     Let D be +1 or -1, so that D*e_L = e_A x e_B */
/*     (note D = <e_L,e_A x e_B> ) */

/*     Then applying our rotation formula again and simplifying we have */

/*     W =   -dALPHA/dt e_A */


/*       -  dBETA/dt {COS(ALPHA)e_B -  D*SIN(ALPHA) e_L } */


/*       -  dGAMMA/dt COS(BETA){ COS(ALPHA(1-<e_A , e_C>))e_C */
/*                              -SIN(ALPHA)   e_A x e_C } */

/*       +  dGAMMA/dt SIN(BETA){ COS(ALPHA(1-|<e_A,e_B x e_C>|))e_B x e_C */
/*                              -SIN(ALPHA) e_A x (e_B x e_C ) */


/*     Now we have two cases: 1) e_A = e_C or 2)  e_C = e_L */

/*     Case 1. e_A = e_C */
/*     ==================== */

/*        W =   -dALPHA/dt e_A */


/*          -  dBETA/dt {COS(ALPHA)e_B -  D*SIN(ALPHA) e_L } */


/*          -  dGAMMA/dt COS(BETA)e_A */

/*          -  dGAMMA/dt D*SIN(BETA)COS(ALPHA)e_L */

/*          -  dGAMMA/dt SIN(BETA)SIN(ALPHA)e_B */


/*        W = e_A{-dALPHA/dt - COS(BETA)dGAMMA/dt} */
/*          + e_B{ -COS(ALPHA)dBETA/dt -   SIN(ALPHA)SIN(BETA)dGAMMA/dt} */
/*          + e_L{D*SIN(ALPHA)dBETA/dt - D*COS(ALPHA)SIN(BETA)dGAMMA/dt} */


/*        let U =    COS(BETA) */
/*            V =  D*SIN(BETA) */

/*        then */

/*        W = e_A{-dALPHA/dt                                -U*dGAMMA/dt} */
/*          + e_B{         -COS(ALPHA)dBETA/dt -D*SIN(ALPHA)*V*dGAMMA/dt} */
/*          + e_L{        D*SIN(ALPHA)dBETA/dt   -COS(ALPHA)*V*dGAMMA/dt} */


/*     Case 2. e_L = e_C */
/*     ==================== */

/*        W =   -dALPHA/dt e_A */


/*          -  dBETA/dt {COS(ALPHA)e_B -  D*SIN(ALPHA) e_L } */


/*          -  dGAMMA/dt COS(BETA){ COS(ALPHA)e_L */
/*                                 -D*SIN(ALPHA)e_B } */

/*          +  dGAMMA/dt SIN(BETA) D*e_A */


/*       W  = e_A{-dALPHA/dt + D*SIN(BETA)dGAMMA/dt} */
/*          + e_B{-COS(ALPHA)dBETA/dt  - D*SIN(ALPHA)COS(BETA)dGAMMA/dt} */
/*          + e_L{D*SIN(ALPHA)dBETA/dt -   COS(ALPHA)COS(BETA)dGAMMA/dt} */


/*       Let U = -D*SIN(BETA) */
/*           V =    COS(BETA) */

/*       then */

/*       W  = e_A{-dALPHA/dt                  -              U*dGAMMA/dt} */
/*          + e_B{       -COS(ALPHA)*dBETA/dt - D*SIN(ALPHA)*V*dGAMMA/dt} */
/*          + e_L{      D*SIN(ALPHA)dBETA/dt  -   COS(ALPHA)*V*dGAMMA/dt} */

/*     As we can see from the above, by choosing appropriate assignments */
/*     for U and V, the two cases can be unified in a single expression. */

/*     Substituting CA and SA for COS(ALPHA) and SIN(ALPHA) and */
/*     re-writing the last expression in matrix form we have: */


/*                          [ -1     0      0 ][ 1  0  U ] [dALPHA/dt] */
/*      W  = {e_A  e_B  e_L}|  0   -CA  -D*SA || 0  1  0 | |dBETA /dt| */
/*                          [  0  D*SA    -CA ][ 0  0  V ] [dGAMMA/dt] */


/*     If we let E_n stand for the transpose of e_n, then solving for */
/*     the derivative vector we have: */

/*     [dALPHA/dt]   [ 1 0 -U/V ] [ -1     0     0] [ E_A ] */
/*     |dBETA /dt| = | 0 1   0  | |  0   -CA  D*SA| | E_B | W */
/*     [dGAMMA/dt]   [ 0 0  1/V ] [  0 -D*SA   -CA] [ E_L ] */


/*     But since the matrix product E_n W is <e_n,W> = W(n) this can */
/*     be rewritten as */

/*     [dALPHA/dt]   [ -1  U*D*SA/V  U*CA/V ] [ W(A) ] */
/*     |dBETA /dt| = |  0   -CA      D*SA   | [ W(B) | */
/*     [dGAMMA/dt]   [  0   -D*SA/V   -CA/V ] [ W(L) ] */


/*     Thus we see that there is a relatively elementary computation */
/*     required to determine the derivatives of the three Euler angles */
/*     returned by M2EUL. */


/*     Standard SPICE exception handling. */

    if (return_()) {
	return 0;
    }
    chkin_("XF2EUL", (ftnlen)6);

/*     Get the rotation and derivative of the rotation separately. */

    for (i__ = 1; i__ <= 3; ++i__) {
	k = i__ + 3;
	for (j = 1; j <= 3; ++j) {
	    r__[(i__1 = i__ + j * 3 - 4) < 9 && 0 <= i__1 ? i__1 : s_rnge(
		    "r", i__1, "xf2eul_", (ftnlen)841)] = xform[(i__2 = i__ + 
		    j * 6 - 7) < 36 && 0 <= i__2 ? i__2 : s_rnge("xform", 
		    i__2, "xf2eul_", (ftnlen)841)];
	    drdt[(i__1 = i__ + j * 3 - 4) < 9 && 0 <= i__1 ? i__1 : s_rnge(
		    "drdt", i__1, "xf2eul_", (ftnlen)842)] = xform[(i__2 = k 
		    + j * 6 - 7) < 36 && 0 <= i__2 ? i__2 : s_rnge("xform", 
		    i__2, "xf2eul_", (ftnlen)842)];
	}
    }

/*     We have to do it sooner or later so we take care of getting */
/*     the various Euler angles now.  This will take care of all the */
/*     bad axis cases too so we don't have to check here. */

    m2eul_(r__, axisa, axisb, axisc, eulang, &eulang[1], &eulang[2]);
    if (failed_()) {
	chkout_("XF2EUL", (ftnlen)6);
	return 0;
    }

/*     Construct local copies of the axes, determine L and D from the */
/*     derivation above. */

    a = *axisa;
    b = *axisb;
    l = 6 - a - b;
    d__ = delta[(i__1 = a + b * 3 - 4) < 9 && 0 <= i__1 ? i__1 : s_rnge("del"
	    "ta", i__1, "xf2eul_", (ftnlen)867)];

/*                      t */
/*     Compute DR/DT * R   and extract OMEGA */

    mxmt_(drdt, r__, drdtrt);

/*     The vector corresponding to DRDTRT is computed as shown below. */

/*        w(1) = drdtrt (3,2) */
/*        w(2) = drdtrt (1,3) */
/*        w(3) = drdtrt (2,1) */

/*     However, we need the 3-vector */

/*        w(A) */
/*        w(B) */
/*        w(L) */

/*     We'll call this vector omega. It's computed as shown here. */

/*        omega(1) = w(A) = d*drdtrt(L,B) */
/*        omega(2) = w(B) = d*drdtrt(A,L) */
/*        omega(3) = w(L) = d*drdtrt(B,A) */

    omega[0] = d__ * drdtrt[(i__1 = l + b * 3 - 4) < 9 && 0 <= i__1 ? i__1 : 
	    s_rnge("drdtrt", i__1, "xf2eul_", (ftnlen)895)];
    omega[1] = d__ * drdtrt[(i__1 = a + l * 3 - 4) < 9 && 0 <= i__1 ? i__1 : 
	    s_rnge("drdtrt", i__1, "xf2eul_", (ftnlen)896)];
    omega[2] = d__ * drdtrt[(i__1 = b + a * 3 - 4) < 9 && 0 <= i__1 ? i__1 : 
	    s_rnge("drdtrt", i__1, "xf2eul_", (ftnlen)897)];

/*     Compute the various sines and cosines that we need. */

    ca = cos(eulang[0]);
    sa = sin(eulang[0]);
    if (*axisa == *axisc) {
	u = cos(eulang[1]);
	v = d__ * sin(eulang[1]);
    } else {
	u = -d__ * sin(eulang[1]);
	v = cos(eulang[1]);
    }

/*     To avoid floating point overflows we make sure that we */
/*     can perform a division by V.  We do this by looking at U. */
/*     If it has absolute value 1, then we set V equal to zero. */
/*     After all U*U + V*V = 1 if SIN and COS and various arithmetic */
/*     operations work perfectly. */

    if (abs(u) == 1.) {
	v = 0.;
    }

/*     We have to look at the singular case first. Recall from above that */

/*        [ W(A) ]   [ -1     0     -U   ][dALPHA/dt] */
/*        | W(B) | = |  0   -CA  -D*SA*V ||dBETA /dt| */
/*        [ W(C) ]   [  0  D*SA    -CA*V ][dGAMMA/dt] */

/*     The singularity arises if V = 0.  In this case the equation */
/*     becomes:  ( Note that U  is plus or minus 1 so that division */
/*     by U is the same as multiplication by U. ) */

/*        [ OMEGA(1) ]   [ -1     0  -U  ][dALPHA/dt] */
/*        | OMEGA(2) | = |  0   -CA   0  ||dBETA /dt| */
/*        [ OMEGA(3) ]   [  0  D*SA   0  ][dGAMMA/dt] */

    if (v == 0.) {
	*unique = FALSE_;
	eulang[3] = 0.;
	eulang[5] = -u * omega[0];

/*        We solve for EULANG(DBETA) by selecting the more stable of */
/*        the two available equations. */

	if (abs(ca) > abs(sa)) {
	    eulang[4] = -omega[1] / ca;
	} else {
	    eulang[4] = d__ * omega[2] / sa;
	}
	chkout_("XF2EUL", (ftnlen)6);
	return 0;
    }

/*     The matrix needed to compute the derivatives uniquely */
/*     exists.  Construct it and carry out the multiplication. */

/*     [dALPHA/dt]   [ -1  U*D*SA/V  U*CA/V ] [ OMEGA(1) ] */
/*     |dBETA /dt| = |  0   -CA      D*SA   | [ OMEGA(2) | */
/*     [dGAMMA/dt]   [  0   -D*SA/V   -CA/V ] [ OMEGA(3) ] */

    *unique = TRUE_;
    solutn[0] = -1.;
    solutn[1] = 0.;
    solutn[2] = 0.;
    solutn[3] = u * d__ * sa / v;
    solutn[4] = -ca;
    solutn[5] = -d__ * sa / v;
    solutn[6] = u * ca / v;
    solutn[7] = d__ * sa;
    solutn[8] = -ca / v;
    mxv_(solutn, omega, &eulang[3]);
    chkout_("XF2EUL", (ftnlen)6);
    return 0;
/* $Procedure EUL2XF ( Euler angles and derivative to transformation ) */

L_eul2xf:
/* $ Abstract */

/*     Compute a state transformation from an Euler angle factorization */
/*     of a rotation and the derivatives of those Euler angles. */

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

/*     ANGLES */
/*     DERIVATIVES */
/*     STATE */

/* $ Declarations */

/*     DOUBLE PRECISION      EULANG ( 6 ) */
/*     INTEGER               AXISA */
/*     INTEGER               AXISB */
/*     INTEGER               AXISC */
/*     DOUBLE PRECISION      XFORM  ( 6, 6 ) */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     EULANG     I   An array of Euler angles and their derivatives. */
/*     AXISA      I   Axis A of the Euler angle factorization. */
/*     AXISB      I   Axis B of the Euler angle factorization. */
/*     AXISC      I   Axis C of the Euler angle factorization. */
/*     XFORM      O   A state transformation matrix. */

/* $ Detailed_Input */

/*     EULANG   is the set of Euler angles corresponding to the */
/*              specified factorization. */

/*              If we represent R as shown here: */

/*                 R =  [ ALPHA ]      [ BETA ]      [ GAMMA ] */
/*                               AXISA         AXISB          AXISC */

/*              then */

/*                 EULANG(1) = ALPHA */
/*                 EULANG(2) = BETA */
/*                 EULANG(3) = GAMMA */
/*                 EULANG(4) = dALPHA/dt */
/*                 EULANG(5) = dBETA/dt */
/*                 EULANG(6) = dGAMMA/dt */


/*     AXISA, */
/*     AXISB, */
/*     AXISC    are the axes desired for the factorization of R. */

/*              All must be in the range from 1 to 3. Moreover */
/*              it must be the case that AXISA and AXISB are distinct */
/*              and that AXISB and AXISC are distinct. */

/*              Every rotation matrix can be represented as a product */
/*              of three rotation matrices about the principal axes */
/*              of a reference frame. */

/*                 R =  [ ALPHA ]      [ BETA ]      [ GAMMA ] */
/*                               AXISA         AXISB          AXISC */

/*              The value 1 corresponds to the X axis. */
/*              The value 2 corresponds to the Y axis. */
/*              The value 3 corresponds to the Z axis. */

/* $ Detailed_Output */

/*     XFORM    is the state transformation matrix corresponding to R */
/*              and dR/dt as described above. Pictorially, */

/*                 .-             -. */
/*                 |       |       | */
/*                 |   R   |   0   | */
/*                 |       |       | */
/*                 |-------+-------| */
/*                 |       |       | */
/*                 | dR/dt |   R   | */
/*                 |       |       | */
/*                 `-             -' */

/*              where R is a rotation matrix that varies with respect to */
/*              time and dR/dt is its time derivative. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If any of AXISA, AXISB, or AXISC do not have values in */

/*            { 1, 2, 3 } */

/*         an error is signaled by a routine in the call tree of this */
/*         routine. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     A word about notation: the symbol */

/*        [ x ] */
/*             i */

/*     indicates a coordinate system rotation of x radians about the */
/*     ith coordinate axis. To be specific, the symbol */

/*        [ x ] */
/*             1 */

/*     indicates a coordinate system rotation of x radians about the */
/*     first, or x-, axis; the corresponding matrix is */

/*        .-                    -. */
/*        |  1      0       0    | */
/*        |                      | */
/*        |  0    cos(x)  sin(x) | */
/*        |                      | */
/*        |  0   -sin(x)  cos(x) | */
/*        `-                    -' */

/*     Remember, this is a COORDINATE SYSTEM rotation by x radians; this */
/*     matrix, when applied to a vector, rotates the vector by -x */
/*     radians, not x radians. Applying the matrix to a vector yields */
/*     the vector's representation relative to the rotated coordinate */
/*     system. */

/*     The analogous rotation about the second, or y-, axis is */
/*     represented by */

/*        [ x ] */
/*             2 */

/*     which symbolizes the matrix */

/*        .-                    -. */
/*        | cos(x)   0   -sin(x) | */
/*        |                      | */
/*        |  0       1      0    | */
/*        |                      | */
/*        | sin(x)   0    cos(x) | */
/*        `-                    -' */

/*     and the analogous rotation about the third, or z-, axis is */
/*     represented by */

/*        [ x ] */
/*             3 */

/*     which symbolizes the matrix */

/*        .-                    -. */
/*        |  cos(x)  sin(x)   0  | */
/*        |                      | */
/*        | -sin(x)  cos(x)   0  | */
/*        |                      | */
/*        |  0        0       1  | */
/*        `-                    -' */

/*     The input matrix is assumed to be the product of three */
/*     rotation matrices, each one of the form */

/*        .-                    -. */
/*        |  1      0       0    | */
/*        |                      | */
/*        |  0    cos(r)  sin(r) |     (rotation of r radians about the */
/*        |                      |      x-axis), */
/*        |  0   -sin(r)  cos(r) | */
/*        `-                    -' */


/*        .-                    -. */
/*        | cos(s)   0   -sin(s) | */
/*        |                      | */
/*        |  0       1      0    |     (rotation of s radians about the */
/*        |                      |      y-axis), */
/*        | sin(s)   0    cos(s) | */
/*        `-                    -' */

/*     or */

/*        .-                    -. */
/*        |  cos(t)  sin(t)   0  | */
/*        |                      | */
/*        | -sin(t)  cos(t)   0  |     (rotation of t radians about the */
/*        |                      |      z-axis), */
/*        |  0        0       1  | */
/*        `-                    -' */

/*     where the second rotation axis is not equal to the first or */
/*     third. Any rotation matrix can be factored as a sequence of */
/*     three such rotations, provided that this last criterion is met. */

/*     This routine is intended to provide an inverse for XF2EUL. */

/*     The two subroutine calls shown here will not change */
/*     XFORM except for round off errors. */

/*        CALL XF2EUL ( XFORM,  AXISA, AXISB, AXISC, EULANG, UNIQUE ) */
/*        CALL EUL2XF ( EULANG, AXISA, AXISB, AXISC, XFORM          ) */

/*     On the other hand the two calls */

/*        CALL EUL2XF ( EULANG, AXISA, AXISB, AXISC, XFORM          ) */
/*        CALL XF2EUL ( XFORM,  AXISA, AXISB, AXISC, EULANG, UNIQUE ) */

/*     will leave EULANG unchanged only if the components of EULANG */
/*     are in the range produced by XF2EUL and the Euler representation */
/*     of the rotation component of XFORM is unique within that range. */

/* $ Examples */

/*     The numerical results shown for this example may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */

/*     1) Suppose you have a set of Euler angles and their derivatives */
/*        for a 3 1 3 rotation, and that you would like to determine */
/*        the equivalent angles and derivatives for a 1 2 3 rotation. */

/*           R = [ALPHA]  [BETA]  [GAMMA] */
/*                      3       1        3 */

/*           R = [ROLL]  [PITCH]  [YAW] */
/*                     1        2      3 */

/*        The following code example will perform the desired */
/*        computation. */


/*        Example code begins here. */


/*              PROGRAM EUL2XF_EX1 */
/*              IMPLICIT NONE */

/*        C */
/*        C     Local variables. */
/*        C */
/*              DOUBLE PRECISION      ABGANG ( 6    ) */
/*              DOUBLE PRECISION      RPYANG ( 6    ) */
/*              DOUBLE PRECISION      XFORM  ( 6, 6 ) */

/*              LOGICAL               UNIQUE */

/*        C */
/*        C     Define the initial set of Euler angles. */
/*        C */
/*              ABGANG(1) =  0.01D0 */
/*              ABGANG(2) =  0.03D0 */
/*              ABGANG(3) =  0.09D0 */
/*              ABGANG(4) = -0.001D0 */
/*              ABGANG(5) = -0.003D0 */
/*              ABGANG(6) = -0.009D0 */

/*        C */
/*        C     Compute the equivalent angles and derivatives for a */
/*        C     1-2-3 rotation. */
/*        C */
/*              CALL EUL2XF ( ABGANG, 3, 1, 3, XFORM  ) */
/*              CALL XF2EUL ( XFORM,  1, 2, 3, RPYANG, UNIQUE ) */

/*              IF ( UNIQUE )  THEN */

/*                 WRITE(*,'(A)') '1-2-3 equivalent rotation to input ' */
/*             .               // '(radians):' */
/*                 WRITE(*,'(2(A,F13.9))') 'Roll  ', RPYANG(1), */
/*             .                           ', dRoll/dt  ', RPYANG(4) */
/*                 WRITE(*,'(2(A,F13.9))') 'Pitch ', RPYANG(2), */
/*             .                           ', dPitch/dt ', RPYANG(5) */
/*                 WRITE(*,'(2(A,F13.9))') 'Yaw   ', RPYANG(3), */
/*             .                           ', dYaw/dt   ', RPYANG(6) */

/*              ELSE */

/*                 WRITE(*,*) 'The values in RPYANG are not uniquely ' */
/*             .          //  'determined.' */

/*              END IF */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*        1-2-3 equivalent rotation to input (radians): */
/*        Roll    0.029998501, dRoll/dt   -0.002999550 */
/*        Pitch  -0.000299950, dPitch/dt   0.000059980 */
/*        Yaw     0.099995501, dYaw/dt    -0.009998650 */


/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     W.L. Taber         (JPL) */
/*     E.D. Wright        (JPL) */

/* $ Version */

/* -    SPICELIB Version 2.1.0, 26-OCT-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. Added complete */
/*        code example based on existing fragment. */

/* -    SPICELIB Version 2.0.1, 25-APR-2007 (EDW) */

/*        Corrected code in $Examples section, example showed */
/*        a XF2EUL call: */

/*           CALL XF2EUL ( XFORM,  1, 2, 3, RPYANG ) */

/*        The proper form of the call: */

/*           CALL XF2EUL ( XFORM,  1, 2, 3, RPYANG, UNIQUE ) */

/* -    SPICELIB Version 2.0.0, 31-OCT-2005 (NJB) */

/*        Restriction that second axis must differ from both the first */
/*        and third axes was removed. */

/* -    SPICELIB Version 1.0.0, 31-JUL-1995 (WLT) */

/* -& */
/* $ Index_Entries */

/*     State transformation from Euler angles and derivatives */

/* -& */

/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }
    chkin_("EUL2XF", (ftnlen)6);

/*     We're going to work with a local copy LOCANG of the euler angle */
/*     state vector EULANG.  We'll also use a local set of axis */
/*     numbers. */

    moved_(eulang, &c__6, locang);
    locaxa = *axisa;
    locaxb = *axisb;
    locaxc = *axisc;

/*     Parts of the following algorithm depend on the central axis */
/*     being different from the first and third axes.  We'll adjust */
/*     the axes and angles to make this so, if necessary. */

    if (*axisb == *axisa || *axisb == *axisc) {
	if (*axisb == *axisa) {

/*           The first angle will "absorb" the second, and the */
/*           second will be set to zero.  All we do here is select */
/*           the first angle. */

	    i__ = 1;
	} else {
	    i__ = 3;
	}

/*        Absorb the second angle into the selected angle and set the */
/*        second angle to zero.  The same goes for the angular rates. */

	locang[(i__1 = i__ - 1) < 6 && 0 <= i__1 ? i__1 : s_rnge("locang", 
		i__1, "xf2eul_", (ftnlen)1416)] = locang[(i__2 = i__ - 1) < 6 
		&& 0 <= i__2 ? i__2 : s_rnge("locang", i__2, "xf2eul_", (
		ftnlen)1416)] + locang[1];
	locang[1] = 0.;
	locang[(i__1 = i__ + 2) < 6 && 0 <= i__1 ? i__1 : s_rnge("locang", 
		i__1, "xf2eul_", (ftnlen)1419)] = locang[(i__2 = i__ + 2) < 6 
		&& 0 <= i__2 ? i__2 : s_rnge("locang", i__2, "xf2eul_", (
		ftnlen)1419)] + locang[4];
	locang[4] = 0.;

/*        Pick a second axis that doesn't match the others.  Since */
/*        the rotation angle about the second axis is zero, all that */
/*        matters here is picking a distinct axis. */

	if (*axisc == next[(i__1 = *axisa - 1) < 3 && 0 <= i__1 ? i__1 : 
		s_rnge("next", i__1, "xf2eul_", (ftnlen)1427)]) {

/*           The first axis is the predecessor of the third, so we pick */
/*           the successor of the third. */

	    locaxb = next[(i__1 = *axisc - 1) < 3 && 0 <= i__1 ? i__1 : 
		    s_rnge("next", i__1, "xf2eul_", (ftnlen)1432)];
	} else {

/*           Either the third axis is the predecessor of the first or */
/*           matches the first, so the successor of the first is our */
/*           choice. */

	    locaxb = next[(i__1 = *axisa - 1) < 3 && 0 <= i__1 ? i__1 : 
		    s_rnge("next", i__1, "xf2eul_", (ftnlen)1440)];
	}
    }

/*     The following local variables are set: */

/*        LOCANG(*), LOCAXA, LOCAXB, LOCAXC */

/*     These variables describe the input rotation, but the second */
/*     axis is now guaranteed to differ from the first and third. */

/*     The derivation for everything that is about to happen here */
/*     is included in the previous entry point. */

    eul2m_(locang, &locang[1], &locang[2], &locaxa, &locaxb, &locaxc, r__);
    if (failed_()) {
	chkout_("EUL2XF", (ftnlen)6);
	return 0;
    }

/*     Construct local copies of the axes, determine L and D from the */
/*     derivation above. */

    a = locaxa;
    b = locaxb;
    l = 6 - a - b;
    d__ = delta[(i__1 = a + b * 3 - 4) < 9 && 0 <= i__1 ? i__1 : s_rnge("del"
	    "ta", i__1, "xf2eul_", (ftnlen)1472)];

/*     Compute the various sines and cosines that we need. */

    ca = cos(locang[0]);
    sa = sin(locang[0]);
    if (locaxa == locaxc) {
	u = cos(locang[1]);
	v = d__ * sin(locang[1]);
    } else {
	u = -d__ * sin(locang[1]);
	v = cos(locang[1]);
    }

/*                            t */
/*     Next we compute dR/dt R.  Recall from the derivation above */
/*     that */


/*        [ W(A) ]   [ -1     0     -U   ][dALPHA/dt] */
/*        | W(B) | = |  0   -CA  -D*SA*V ||dBETA /dt| */
/*        [ W(L) ]   [  0  D*SA    -CA*V ][dGAMMA/dt] */

/*     In the previous entry point we used OMEGA for the vector */
/*     of rearranged components of W. */

/*        OMEGA(1) = W(A) = D*DRDTRT(L,B) */
/*        OMEGA(2) = W(B) = D*DRDTRT(A,L) */
/*        OMEGA(3) = W(L) = D*DRDTRT(B,A) */

/*        DRDTRT(L,B) = D*OMEGA(1) */
/*        DRDTRT(A,L) = D*OMEGA(2) */
/*        DRDTRT(B,A) = D*OMEGA(3) */

/*        [ DRDTRT(L,B) ]   [ -D     0     -D*U ][dALPHA/dt] */
/*        | DRDTRT(A,L) | = |  0 -D*CA    -SA*V ||dBETA /dt| */
/*        [ DRDTRT(B,A) ]   [  0    SA  -D*CA*V ][dGAMMA/dt] */

/*     We set up the matrix of this equation in SOLUTN below */
/*     and compute D*OMEGA which we denote by the variable DOMEGA. */

    solutn[0] = -d__;
    solutn[1] = 0.;
    solutn[2] = 0.;
    solutn[3] = 0.;
    solutn[4] = -d__ * ca;
    solutn[5] = sa;
    solutn[6] = -d__ * u;
    solutn[7] = -sa * v;
    solutn[8] = -d__ * ca * v;
    mxv_(solutn, &locang[3], domega);
    drdtrt[(i__1 = l + b * 3 - 4) < 9 && 0 <= i__1 ? i__1 : s_rnge("drdtrt", 
	    i__1, "xf2eul_", (ftnlen)1530)] = domega[0];
    drdtrt[(i__1 = b + l * 3 - 4) < 9 && 0 <= i__1 ? i__1 : s_rnge("drdtrt", 
	    i__1, "xf2eul_", (ftnlen)1531)] = -domega[0];
    drdtrt[(i__1 = a + l * 3 - 4) < 9 && 0 <= i__1 ? i__1 : s_rnge("drdtrt", 
	    i__1, "xf2eul_", (ftnlen)1533)] = domega[1];
    drdtrt[(i__1 = l + a * 3 - 4) < 9 && 0 <= i__1 ? i__1 : s_rnge("drdtrt", 
	    i__1, "xf2eul_", (ftnlen)1534)] = -domega[1];
    drdtrt[(i__1 = b + a * 3 - 4) < 9 && 0 <= i__1 ? i__1 : s_rnge("drdtrt", 
	    i__1, "xf2eul_", (ftnlen)1536)] = domega[2];
    drdtrt[(i__1 = a + b * 3 - 4) < 9 && 0 <= i__1 ? i__1 : s_rnge("drdtrt", 
	    i__1, "xf2eul_", (ftnlen)1537)] = -domega[2];
    drdtrt[0] = 0.;
    drdtrt[4] = 0.;
    drdtrt[8] = 0.;
    mxm_(drdtrt, r__, drdt);
    for (j = 1; j <= 3; ++j) {
	for (i__ = 1; i__ <= 3; ++i__) {
	    xform[(i__1 = i__ + j * 6 - 7) < 36 && 0 <= i__1 ? i__1 : s_rnge(
		    "xform", i__1, "xf2eul_", (ftnlen)1547)] = r__[(i__2 = 
		    i__ + j * 3 - 4) < 9 && 0 <= i__2 ? i__2 : s_rnge("r", 
		    i__2, "xf2eul_", (ftnlen)1547)];
	    xform[(i__1 = i__ + 3 + (j + 3) * 6 - 7) < 36 && 0 <= i__1 ? i__1 
		    : s_rnge("xform", i__1, "xf2eul_", (ftnlen)1548)] = r__[(
		    i__2 = i__ + j * 3 - 4) < 9 && 0 <= i__2 ? i__2 : s_rnge(
		    "r", i__2, "xf2eul_", (ftnlen)1548)];
	    xform[(i__1 = i__ + 3 + j * 6 - 7) < 36 && 0 <= i__1 ? i__1 : 
		    s_rnge("xform", i__1, "xf2eul_", (ftnlen)1549)] = drdt[(
		    i__2 = i__ + j * 3 - 4) < 9 && 0 <= i__2 ? i__2 : s_rnge(
		    "drdt", i__2, "xf2eul_", (ftnlen)1549)];
	    xform[(i__1 = i__ + (j + 3) * 6 - 7) < 36 && 0 <= i__1 ? i__1 : 
		    s_rnge("xform", i__1, "xf2eul_", (ftnlen)1550)] = 0.;
	}
    }
    chkout_("EUL2XF", (ftnlen)6);
    return 0;
} /* xf2eul_ */

/* Subroutine */ int xf2eul_(doublereal *xform, integer *axisa, integer *
	axisb, integer *axisc, doublereal *eulang, logical *unique)
{
    return xf2eul_0_(0, xform, axisa, axisb, axisc, eulang, unique);
    }

/* Subroutine */ int eul2xf_(doublereal *eulang, integer *axisa, integer *
	axisb, integer *axisc, doublereal *xform)
{
    return xf2eul_0_(1, xform, axisa, axisb, axisc, eulang, (logical *)0);
    }

