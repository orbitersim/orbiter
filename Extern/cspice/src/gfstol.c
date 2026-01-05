/* gfstol.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c_n2 = -2;
static integer c__3 = 3;

/* $Procedure GFSTOL ( GF, set a tolerance value for GF ) */
/* Subroutine */ int gfstol_(doublereal *value)
{
    extern /* Subroutine */ int chkin_(char *, ftnlen), errdp_(char *, 
	    doublereal *, ftnlen);
    logical ok;
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), setmsg_(char *, ftnlen);
    extern logical return_(void);
    extern /* Subroutine */ int zzholdd_(integer *, integer *, logical *, 
	    doublereal *);

/* $ Abstract */

/*     Override the default GF convergence value used in the high */
/*     level GF routines. */

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

/*     GF */

/* $ Keywords */

/*     GEOMETRY */

/* $ Declarations */
/* $ Abstract */

/*     SPICE private routine intended solely for the support of SPICE */
/*     routines. Users should not call this routine directly due to the */
/*     volatile nature of this routine. */

/*     This file contains parameter declarations for the ZZHOLDD */
/*     routine. */

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

/*     None. */

/* $ Declarations */

/*     None. */

/* $ Brief_I/O */

/*     None. */

/* $ Detailed_Input */

/*     None. */

/* $ Detailed_Output */

/*     None. */

/* $ Parameters */

/*     GEN       general value, primarily for testing. */

/*     GF_REF    user defined GF reference value. */

/*     GF_TOL    user defined GF convergence tolerance. */

/*     GF_DT     user defined GF step for numeric differentiation. */

/* $ Exceptions */

/*     None. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     None. */

/* $ Examples */

/*     None. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     E.D. Wright    (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.0  03-DEC-2013 (EDW) */

/* -& */

/*     OP codes. The values exist in the integer domain */
/*     [ -ZZNOP, -1], */


/*     Current number of OP codes. */


/*     ID codes. The values exist in the integer domain */
/*     [ 1, NID], */


/*     General use, primarily testing. */


/*     The user defined GF reference value. */


/*     The user defined GF convergence tolerance. */


/*     The user defined GF step for numeric differentiation. */


/*     Current number of ID codes, dimension of array */
/*     in ZZHOLDD. Bad things can happen if this parameter */
/*     does not have the proper value. */


/*     End of file zzholdd.inc. */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     ZZPUT      P   ZZHOLDD stores a DP value. */
/*     GF_TOL     P   ZZHOLDD acts on the GF subsystem tolerance. */
/*     VALUE      I   Double precision value returned or to store. */

/* $ Detailed_Input */

/*     VALUE    is the scalar double precision value to use as the GF */
/*              subsystem convergence tolerance. This value will override */
/*              the default tolerance, CNVTOL, defined in gf.inc. Units */
/*              are TDB seconds. */

/* $ Detailed_Output */

/*     None. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If VALUE is not strictly greater-than-zero, the error */
/*         SPICE(INVALIDTOLERANCE) is signaled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     The high level GF routines (see gf.req for a listing) use a */
/*     default value for the convergence tolerance, CNVTOL, defined in */
/*     gf.inc. It may occur that a GF search run needs a different */
/*     convergence tolerance. GFSTOL programmatically changes the */
/*     tolerance used by those routines. */

/* $ Examples */

/*     The numerical results shown for this example may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */

/*     1) Perform a search for occultation events of the sun by earth as */
/*        observed from the Moon center. Search during the interval from */
/*        14 A.D. SEP 1 to 14 A.D. SEP 30 (Julian). */

/*        Use the meta-kernel shown below to load the required SPICE */
/*        kernels. */


/*           KPL/MK */

/*           File name: gfstol_ex1.tm */

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
/*              pck00009.tpc                  Planet orientation and */
/*                                            radii */
/*              naif0009.tls                  Leapseconds */

/*           \begindata */

/*              KERNELS_TO_LOAD = ( 'pck00009.tpc', */
/*                                  'naif0009.tls'  ) */

/*           \begintext */

/*           End of meta-kernel. */


/*        Use the SPK kernel below to load the required ephemeris, */
/*        covering year 14 AD. */

/*           de408.bsp */


/*        Example code begins here. */


/*              PROGRAM GFSTOL_EX1 */
/*              IMPLICIT NONE */

/*        C */
/*        C     SPICELIB functions */
/*        C */
/*              INTEGER               WNCARD */

/*        C */
/*        C     Local parameters */
/*        C */
/*              CHARACTER*(*)         TIMFMT */
/*              PARAMETER           ( TIMFMT = */
/*             .             'YYYY ERA MON DD HR:MN:SC.#### ::JCAL' ) */

/*              INTEGER               MAXWIN */
/*              PARAMETER           ( MAXWIN = 2 * 100 ) */

/*              INTEGER               TIMLEN */
/*              PARAMETER           ( TIMLEN = 40 ) */

/*              INTEGER               LBCELL */
/*              PARAMETER           ( LBCELL = -5 ) */

/*        C */
/*        C     Local variables */
/*        C */
/*              CHARACTER*(TIMLEN)    WIN0 */
/*              CHARACTER*(TIMLEN)    WIN1 */
/*              CHARACTER*(TIMLEN)    BEGSTR */
/*              CHARACTER*(TIMLEN)    ENDSTR */

/*              DOUBLE PRECISION      CNFINE ( LBCELL : 2      ) */
/*              DOUBLE PRECISION      RESULT ( LBCELL : MAXWIN ) */
/*              DOUBLE PRECISION      ET0 */
/*              DOUBLE PRECISION      ET1 */
/*              DOUBLE PRECISION      LEFT */
/*              DOUBLE PRECISION      RIGHT */
/*              DOUBLE PRECISION      STEP */

/*              INTEGER               I */

/*              LOGICAL               OK */

/*        C */
/*        C     Saved variables */
/*        C */
/*        C     The confinement and result windows CNFINE and RESULT are */
/*        C     saved because this practice helps to prevent stack */
/*        C     overflow. */
/*        C */
/*              SAVE                  CNFINE */
/*              SAVE                  RESULT */

/*        C */
/*        C     Load kernels. */
/*        C */
/*              CALL FURNSH ( 'gfstol_ex1.tm' ) */

/*        C */
/*        C     Use an SPK covering year 14 AD. */
/*        C */
/*              CALL FURNSH ( 'de408.bsp' ) */

/*        C */
/*        C     Initialize the confinement and result windows. */
/*        C */
/*              CALL SSIZED ( 2,      CNFINE ) */
/*              CALL SSIZED ( MAXWIN, RESULT ) */

/*        C */
/*        C     Obtain the TDB time bounds of the confinement */
/*        C     window, which is a single interval in this case. */
/*        C */
/*              WIN0 = '14 A.D. SEP 1  00:00:00' */
/*              WIN1 = '14 A.D. SEP 30 00:00:00' */

/*              CALL STR2ET ( WIN0, ET0 ) */
/*              CALL STR2ET ( WIN1, ET1 ) */

/*        C */
/*        C     Insert the time bounds into the confinement */
/*        C     window. */
/*        C */
/*              CALL WNINSD ( ET0, ET1, CNFINE ) */

/*        C */
/*        C     Select a 3-minute step. We'll ignore any occultations */
/*        C     lasting less than 3 minutes. */
/*        C */
/*              STEP = 180.D0 */

/*        C */
/*        C     Perform the search. ET0 and ET1 have values ~-6*10^10, */
/*        C     CNVTOL has value 10^-6, so double precision addition or */
/*        C     subtraction of ET0 and ET1 with CNVTOL returns a result */
/*        C     indistinguishable from ET0 and ET1. */
/*        C */
/*        C     Reduce the GF convergence tolerance by an order of */
/*        C     magnitude to resolve this condition. */
/*        C */
/*              CALL GFSTOL ( 1D-5 ) */

/*              CALL GFOCLT ( 'ANY', */
/*             .              'EARTH', 'ellipsoid', 'IAU_EARTH', */
/*             .              'SUN',   'ellipsoid', 'IAU_SUN', */
/*             .              'LT',    'MOON',       STEP, */
/*             .               CNFINE,  RESULT  ) */


/*              IF ( WNCARD(RESULT) .EQ. 0 ) THEN */

/*                 WRITE (*,*) 'No occultation was found.' */

/*              ELSE */

/*                 DO I = 1, WNCARD(RESULT) */

/*        C */
/*        C           Fetch and display each occultation interval. */
/*        C */
/*                    CALL WNFETD ( RESULT, I, LEFT, RIGHT ) */

/*                    CALL TIMOUT ( LEFT,  TIMFMT, BEGSTR ) */
/*                    CALL TIMOUT ( RIGHT, TIMFMT, ENDSTR ) */

/*                    WRITE (*,*) 'Interval ', I */
/*                    WRITE (*,*) '   Start time: '//BEGSTR */
/*                    WRITE (*,*) '   Stop time:  '//ENDSTR */

/*                 END DO */

/*              END IF */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*         Interval            1 */
/*            Start time:   14 A.D. SEP 27 05:02:02.8250 */
/*            Stop time:    14 A.D. SEP 27 09:33:31.6995 */


/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     E.D. Wright        (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.1, 06-JUL-2021 (JDR) */

/*        Edited the header to comply with NAIF standard. */

/*        Added SAVE statements for CNFINE and RESULT variables in code */
/*        example. */

/* -    SPICELIB Version 1.0.0, 18-APR-2014 (EDW) */

/* -& */
/* $ Index_Entries */

/*     change default convergence tolerance for GF routines */

/* -& */

/*     SPICELIB functions */


/*     Local variables. */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }

/*     This routine wraps a call to ZZHOLDD with the appropriate ID and */
/*     OP value. */


/*     Check tolerance value. */

    if (*value <= 0.) {
	chkin_("GFSTOL", (ftnlen)6);
	setmsg_("Convergence tolerance must be greater-than zero. Input VALU"
		"E = #.", (ftnlen)65);
	errdp_("#", value, (ftnlen)1);
	sigerr_("SPICE(INVALIDTOLERANCE)", (ftnlen)23);
	chkout_("GFSTOL", (ftnlen)6);
	return 0;
    } else {
	zzholdd_(&c_n2, &c__3, &ok, value);
    }
    return 0;
} /* gfstol_ */

