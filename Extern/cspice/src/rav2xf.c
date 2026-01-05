/* rav2xf.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure RAV2XF ( Rotation and angular velocity to transform ) */
/* Subroutine */ int rav2xf_(doublereal *rot, doublereal *av, doublereal *
	xform)
{
    /* System generated locals */
    integer i__1, i__2;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer);

    /* Local variables */
    integer i__, j;
    doublereal omegat[9]	/* was [3][3] */, drotdt[9]	/* was [3][3] 
	    */;
    extern /* Subroutine */ int mxm_(doublereal *, doublereal *, doublereal *)
	    ;

/* $ Abstract */

/*     Determine a state transformation matrix from a rotation matrix */
/*     and the angular velocity of the rotation. */

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

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     ROT        I   Rotation matrix. */
/*     AV         I   Angular velocity vector. */
/*     XFORM      O   State transformation associated with ROT and AV. */

/* $ Detailed_Input */

/*     ROT      is a rotation matrix that gives the transformation from */
/*              some frame FRAME1 to another frame FRAME2. */

/*     AV       is the angular velocity of the transformation. */
/*              In other words, if P is the position of a fixed */
/*              point in FRAME2, then from the point of view of */
/*              FRAME1, P rotates (in a right handed sense) about */
/*              an axis parallel to AV. Moreover the rate of rotation */
/*              in radians per unit time is given by the length of */
/*              AV. */

/*              More formally, the velocity V of P in FRAME1 is */
/*              given by */
/*                                 T */
/*                 V  =  AV x ( ROT  * P ) */

/* $ Detailed_Output */

/*     XFORM    is a state transformation matrix associated */
/*              with ROT and AV. If S1 is the state of an object */
/*              with respect to FRAME1, then the state S2 of the */
/*              object with respect to FRAME2 is given by */

/*                 S2  =  XFORM * S1 */

/*              where "*" denotes Matrix-Vector multiplication. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/*     1)  No checks are performed on ROT to ensure that it is indeed */
/*         a rotation matrix. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine is essentially a macro routine for converting */
/*     a rotation and angular velocity of the rotation to the */
/*     equivalent state transformation matrix. */

/*     This routine is an inverse of XF2RAV. */

/* $ Examples */

/*     The numerical results shown for this example may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */

/*     1) The following example program uses CKGPAV to get C-matrix */
/*        and associated angular velocity vector for an image whose */
/*        SCLK count (un-encoded character string version) is known. */

/*        From that matrix and angular velocity vector, the associated */
/*        state transformation matrix is obtained. */

/*        Note that we need to load a SCLK kernel to convert from clock */
/*        string to "ticks." Although not required for older spacecraft */
/*        clocks, most modern spacecraft ones require a leapseconds */
/*        kernel to be loaded in addition to a SCLK kernel. */


/*        Use the meta-kernel shown below to load the required SPICE */
/*        kernels. */


/*           KPL/MK */

/*           File name: rav2xf_ex1.tm */

/*           This meta-kernel is intended to support operation of SPICE */
/*           example programs. The kernels shown here should not be */
/*           assumed to contain adequate or correct versions of data */
/*           required by SPICE-based user applications. */

/*           In order for an application to use this meta-kernel, the */
/*           kernels referenced here must be present in the user's */
/*           current working directory. */

/*           The names and contents of the kernels referenced */
/*           by this meta-kernel are as follows: */

/*              File name              Contents */
/*              --------------------   ----------------------- */
/*              cas00071.tsc           CASSINI SCLK */
/*              04161_04164ra.bc       CASSINI spacecraft */
/*                                     reconstructed CK */

/*           \begindata */

/*             KERNELS_TO_LOAD = ( 'cas00071.tsc' */
/*                                 '04161_04164ra.bc' ) */

/*           \begintext */

/*           End of meta-kernel */


/*        Example code begins here. */


/*              PROGRAM RAV2XF_EX1 */
/*              IMPLICIT NONE */

/*        C */
/*        C     Constants for this program. */
/*        C */
/*        C     -- The code for the CASSINI spacecraft clock is -82. */
/*        C */
/*        C     -- The code for CASSINI spacecraft reference frame is */
/*        C        -82000. */
/*        C */
/*        C    --  Spacecraft clock tolerance is 1.0 seconds. This may */
/*        C        not be an acceptable tolerance for some applications. */
/*        C        It must be converted to "ticks" (units of encoded */
/*        C        SCLK) for input to CKGPAV. */
/*        C */
/*        C     -- The reference frame we want is J2000. */
/*        C */
/*              CHARACTER*(*)         META */
/*              PARAMETER           ( META   = 'rav2xf_ex1.tm' ) */

/*              CHARACTER*(*)         REFFRM */
/*              PARAMETER           ( REFFRM = 'J2000' ) */

/*              CHARACTER*(*)         SCLKCH */
/*              PARAMETER           ( SCLKCH = '1/1465476046.160' ) */

/*              CHARACTER*(*)         SCLTOL */
/*              PARAMETER           ( SCLTOL = '1.0' ) */

/*              INTEGER               SCID */
/*              PARAMETER           ( SCID   = -82    ) */

/*              INTEGER               INSTID */
/*              PARAMETER           ( INSTID = -82000 ) */

/*        C */
/*        C     Local variables. */
/*        C */
/*              DOUBLE PRECISION      AV     ( 3 ) */
/*              DOUBLE PRECISION      CLKOUT */
/*              DOUBLE PRECISION      CMAT   ( 3, 3 ) */
/*              DOUBLE PRECISION      FXMAT  ( 6, 6 ) */
/*              DOUBLE PRECISION      SCLKDP */
/*              DOUBLE PRECISION      TOLTIK */

/*              INTEGER               I */
/*              INTEGER               J */

/*              LOGICAL               FOUND */

/*        C */
/*        C     Load kernels. */
/*        C */
/*              CALL FURNSH ( META ) */

/*        C */
/*        C     Convert tolerance from CASSINI formatted character */
/*        C     string SCLK to ticks which are units of encoded SCLK. */
/*        C */
/*              CALL SCTIKS ( SCID, SCLTOL, TOLTIK ) */

/*        C */
/*        C     CKGPAV requires encoded spacecraft clock. */
/*        C */
/*              CALL SCENCD ( SCID, SCLKCH, SCLKDP ) */

/*              CALL CKGPAV ( INSTID, SCLKDP, TOLTIK, REFFRM, */
/*             .              CMAT,   AV,     CLKOUT, FOUND ) */

/*        C */
/*        C     Recall that CMAT and AV are the rotation and angular */
/*        C     velocity of the transformation from J2000 to the */
/*        C     spacecraft frame. */
/*        C */
/*              IF ( FOUND ) THEN */

/*        C */
/*        C        Display CMAT and AV. */
/*        C */
/*                 WRITE(*,'(A)') 'Rotation matrix:' */
/*                 DO I = 1, 3 */

/*                    WRITE(*,'(3F10.6)') (CMAT(I,J), J=1,3 ) */

/*                 END DO */

/*                 WRITE(*,'(A)') 'Angular velocity:' */
/*                 WRITE(*,'(3F20.16)') AV */

/*        C */
/*        C        Get state transformation from J2000 to the spacecraft */
/*        C        frame. */
/*        C */
/*                 CALL RAV2XF ( CMAT,  AV, FXMAT ) */

/*        C */
/*        C        Display the results. */
/*        C */
/*                 WRITE(*,*) */
/*                 WRITE(*,'(A)') 'State transformation matrix:' */
/*                 DO I = 1, 6 */

/*                    WRITE(*,'(6F10.6)') (FXMAT(I,J), J=1,6 ) */

/*                 END DO */

/*              ELSE */

/*                 WRITE(*,*) 'No rotation matrix/angular velocity ' */
/*             .          //  'found for ', SCLKCH */

/*              END IF */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*        Rotation matrix: */
/*         -0.604984  0.796222 -0.005028 */
/*         -0.784160 -0.596891 -0.169748 */
/*         -0.138158 -0.098752  0.985475 */
/*        Angular velocity: */
/*          0.0000032866819065 -0.0000099372638338  0.0000197597699770 */

/*        State transformation matrix: */
/*         -0.604984  0.796222 -0.005028  0.000000  0.000000  0.000000 */
/*         -0.784160 -0.596891 -0.169748  0.000000  0.000000  0.000000 */
/*         -0.138158 -0.098752  0.985475  0.000000  0.000000  0.000000 */
/*         -0.000016 -0.000012 -0.000003 -0.604984  0.796222 -0.005028 */
/*          0.000013 -0.000015 -0.000010 -0.784160 -0.596891 -0.169748 */
/*         -0.000008 -0.000006 -0.000002 -0.138158 -0.098752  0.985475 */


/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.1, 04-JUL-2021 (JDR) */

/*        Corrected $Abstract section, which described XF2RAV instead of */
/*        this routine. */

/*        Edited the header to comply with NAIF standard. Added complete */
/*        code example based existing fragment. */

/*         Added ROTATION to the required readings. */

/* -    SPICELIB Version 1.1.0, 28-JUL-1997 (WLT) */

/*        The example in version 1.0.0 was incorrect. The example */
/*        in version 1.1.0 fixes the previous problem. */

/* -    SPICELIB Version 1.0.0, 18-SEP-1995 (WLT) */

/* -& */
/* $ Index_Entries */

/*     State transformation to rotation and angular velocity */

/* -& */

/*     A state transformation matrix XFORM has the following form */


/*         [      |     ] */
/*         |  R   |  0  | */
/*         |      |     | */
/*         | -----+-----| */
/*         |  dR  |     | */
/*         |  --  |  R  | */
/*         [  dt  |     ] */


/*     where R is a rotation and dR/dt is the time derivative of that */
/*     rotation.  From this we can immediately fill in most of the */
/*     state transformation matrix. */

    for (i__ = 1; i__ <= 3; ++i__) {
	for (j = 1; j <= 3; ++j) {
	    xform[(i__1 = i__ + j * 6 - 7) < 36 && 0 <= i__1 ? i__1 : s_rnge(
		    "xform", i__1, "rav2xf_", (ftnlen)374)] = rot[(i__2 = i__ 
		    + j * 3 - 4) < 9 && 0 <= i__2 ? i__2 : s_rnge("rot", i__2,
		     "rav2xf_", (ftnlen)374)];
	    xform[(i__1 = i__ + 3 + (j + 3) * 6 - 7) < 36 && 0 <= i__1 ? i__1 
		    : s_rnge("xform", i__1, "rav2xf_", (ftnlen)375)] = rot[(
		    i__2 = i__ + j * 3 - 4) < 9 && 0 <= i__2 ? i__2 : s_rnge(
		    "rot", i__2, "rav2xf_", (ftnlen)375)];
	    xform[(i__1 = i__ + (j + 3) * 6 - 7) < 36 && 0 <= i__1 ? i__1 : 
		    s_rnge("xform", i__1, "rav2xf_", (ftnlen)376)] = 0.;
	}
    }

/*     Now for the rest. */

/*     Recall that ROT is a transformation that converts positions */
/*     in some frame FRAME1 to positions in a second frame FRAME2. */

/*     The angular velocity matrix OMEGA (the cross product matrix */
/*     corresponding to AV) has the following property. */

/*     If P is the position of an object that is stationary with */
/*     respect to FRAME2 then the velocity V of that object in FRAME1 */
/*     is given by: */
/*                          t */
/*         V  =  OMEGA * ROT  *  P */

/*     But V is also given by */

/*                    t */
/*               d ROT */
/*         V =   -----  * P */
/*                 dt */

/*     So that */
/*                                  t */
/*                    t        d ROT */
/*         OMEGA * ROT    =   ------- */
/*                               dt */

/*     Hence */

/*          d ROT                 t */
/*          -----   =  ROT * OMEGA */
/*            dt */


/*     From this discussion we can see that we need OMEGA transpose. */
/*     Here it is. */

    omegat[0] = 0.;
    omegat[1] = -av[2];
    omegat[2] = av[1];
    omegat[3] = av[2];
    omegat[4] = 0.;
    omegat[5] = -av[0];
    omegat[6] = -av[1];
    omegat[7] = av[0];
    omegat[8] = 0.;
    mxm_(rot, omegat, drotdt);
    for (i__ = 1; i__ <= 3; ++i__) {
	for (j = 1; j <= 3; ++j) {
	    xform[(i__1 = i__ + 3 + j * 6 - 7) < 36 && 0 <= i__1 ? i__1 : 
		    s_rnge("xform", i__1, "rav2xf_", (ftnlen)434)] = drotdt[(
		    i__2 = i__ + j * 3 - 4) < 9 && 0 <= i__2 ? i__2 : s_rnge(
		    "drotdt", i__2, "rav2xf_", (ftnlen)434)];
	}
    }
    return 0;
} /* rav2xf_ */

