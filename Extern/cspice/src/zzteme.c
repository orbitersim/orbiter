/* zzteme.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__6 = 6;
static integer c__3 = 3;
static integer c__1 = 1;

/* $Procedure ZZTEME ( J2000 to TEME at epoch ) */
/* Subroutine */ int zzteme_(doublereal *et, doublereal *j2tm, doublereal *
	tm2j)
{
    doublereal xj2000[6], zj2000[6];
    extern /* Subroutine */ int mxvg_(doublereal *, doublereal *, integer *, 
	    integer *, doublereal *);
    doublereal m1inv[36]	/* was [6][6] */, m2inv[36]	/* was [6][6] 
	    */, z__[6];
    extern /* Subroutine */ int chkin_(char *, ftnlen), zztwovxf_(doublereal *
	    , integer *, doublereal *, integer *, doublereal *), moved_(
	    doublereal *, integer *, doublereal *);
    doublereal m1[36]	/* was [6][6] */, m2[36]	/* was [6][6] */;
    extern /* Subroutine */ int chkout_(char *, ftnlen);
    extern logical return_(void);
    extern /* Subroutine */ int invstm_(doublereal *, doublereal *), 
	    zzeprc76_(doublereal *, doublereal *), zzenut80_(doublereal *, 
	    doublereal *);

/* $ Abstract */

/*     J2000 to TEME, and TEME to J2000, probably. */

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

/*     FRAMES */

/* $ Keywords */

/*     FRAMES */

/* $ Declarations */
/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     ET         I   Epoch of the state transformation operator. */
/*     J2TM       O   The state transformation operator J2000 to TEME. */
/*     TM2J       O   The state transformation operator TEME to J2000. */

/* $ Detailed_Input */

/*     ET         time expressed in terms of TDB seconds past epoch J2000 */
/*                for which to to calculate the J2000 frame to TEME frame */
/*                transformation operator. */

/* $ Detailed_Output */

/*     J2TM       the 6x6 operator that transforms cartesian states from */
/*                the J2000 to TEME reference frame. */

/*     TM2J       the 6x6 operator that transforms cartesian states from */
/*                the TEME to J2000 reference frame. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     None. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     TETE    True equator true equinox */
/*     MEME    Mean equator mean equinox */
/*     TEME    True equator mean equinox */

/* $ Examples */

/*     None. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman   (JPL) */
/*     S.C. Krening   (JPL) */
/*     E.D. Wright    (JPL) */

/* $ Version */

/* -    SPICELIB Version 2.0.0, 10-OCT-2021 (EDW) */

/*        Routine now returns TEME to J2000 mapping operator. */

/* -    SPICELIB Version 1.0.0, 06-MAR-2012 (NJB) (SCK) (EDW) */

/* -& */
/* $ Index_Entries */

/*   transformation J2000 frame to pseudo TEME */
/*   transformation pseudo TEME to J2000 */

/* -& */

/*     SPICELIB functions */


/*     Local variables */

    if (return_()) {
	return 0;
    }
    chkin_("ZZTEME", (ftnlen)6);

/*     Extract the MEME +X vector and its derivative, both */
/*     expressed relative to the J2000 frame, into X. */

    zzeprc76_(et, m1);
    invstm_(m1, m1inv);
    moved_(m1inv, &c__6, xj2000);

/*     Extract the TETE +Z vector and its derivative, both */
/*     expressed relative to the MEME frame, into Z. */

    zzenut80_(et, m2);
    invstm_(m2, m2inv);
    moved_(&m2inv[12], &c__6, z__);

/*     Transform Z to the J2000 frame. */

    mxvg_(m1inv, z__, &c__6, &c__6, zj2000);

/*     Compute the TEME to J2000 state transformation, and the */
/*     inverse. */

    zztwovxf_(zj2000, &c__3, xj2000, &c__1, tm2j);
    invstm_(tm2j, j2tm);
    chkout_("ZZTEME", (ftnlen)6);
    return 0;
} /* zzteme_ */

