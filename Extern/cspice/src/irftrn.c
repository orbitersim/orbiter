/* irftrn.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure IRFTRN ( Inertial reference frame transformation ) */
/* Subroutine */ int irftrn_(char *refa, char *refb, doublereal *rotab, 
	ftnlen refa_len, ftnlen refb_len)
{
    integer codea, codeb;
    extern /* Subroutine */ int chkin_(char *, ftnlen), chkout_(char *, 
	    ftnlen), irfnum_(char *, integer *, ftnlen), irfrot_(integer *, 
	    integer *, doublereal *);
    extern logical return_(void);

/* $ Abstract */

/*     Return the matrix that transforms vectors from one specified */
/*     inertial reference frame to another. */

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

/*     SPK */

/* $ Keywords */

/*     CONVERSION */
/*     COORDINATES */
/*     FRAMES */
/*     MATRIX */
/*     ROTATION */
/*     TRANSFORMATION */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     REFA       I   Name of reference frame to transform vectors FROM. */
/*     REFB       I   Name of reference frame to transform vectors TO. */
/*     ROTAB      O   REFA-to-REFB transformation matrix. */

/* $ Detailed_Input */

/*     REFA, */
/*     REFB     are the names of two inertial reference frames. Any names */
/*              accepted by the routine IRFNUM may be used. See */
/*              $Particulars for a list of some of the more commonly used */
/*              inertial reference frame names. */

/* $ Detailed_Output */

/*     ROTAB    is a rotation matrix that transforms the */
/*              coordinates of a vector V relative to the */
/*              reference frame specified by REFA to the */
/*              coordinates of V relative to the reference frame */
/*              specified by REFB. The transformation is carried */
/*              out by the matrix multiplication */

/*                 V = ROTAB * V. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If either of the input reference frame names is invalid, an */
/*         error is signaled by a routine in the call tree of this */
/*         routine. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     Normally applications should call the more general, higher level */
/*     routine PXFORM instead of this routine. */

/*     This routine is a macro that replaces the code fragment */

/*        CALL IRFNUM ( REFA,  CODEA        ) */
/*        CALL IRFNUM ( REFB,  CODEB        ) */
/*        CALL IRFROT ( CODEA, CODEB, ROTAB ) */


/*     Among the reference frame names accepted by IRFNUM are: */

/*        'J2000' */
/*        'B1950' */
/*        'FK4' */
/*        'DE-96' */
/*        'DE-102' */
/*        'DE-108' */
/*        'DE-111' */
/*        'DE-114' */
/*        'DE-118' */
/*        'DE-122' */
/*        'DE-125' */
/*        'DE-130' */
/*        'DE-200' */
/*        'DE-202' */
/*        'GALACTIC' */

/*     See the SPICELIB routine GHGIRF for details. */

/* $ Examples */

/*     1)  Transform a vector V1950 from the B1950 to the J2000 */
/*         reference frame. */

/*            C */
/*            C     Ask IRFTRN for the matrix that transforms vectors */
/*            C     from the B1950 to the J2000 reference frame. */
/*            C */
/*                  CALL IRFTRN ( 'B1950', 'J2000', TRANS ) */

/*            C */
/*            C     Now transform V1950 to the J2000 reference frame. */
/*            C */
/*                  CALL MXV ( TRANS, V1950, V2000 ) */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 20-AUG-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 1.0.2, 28-SEP-2004 (NJB) */

/*        Corrected comment in code example in header. Made other minor */
/*        updates to header. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 30-AUG-1991 (NJB) */

/* -& */
/* $ Index_Entries */

/*     transformation from one inertial frame to another */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("IRFTRN", (ftnlen)6);
    }

/*     Encode the reference frame names, and find the transformation */
/*     matrix. */

    irfnum_(refa, &codea, refa_len);
    irfnum_(refb, &codeb, refb_len);
    irfrot_(&codea, &codeb, rotab);
    chkout_("IRFTRN", (ftnlen)6);
    return 0;
} /* irftrn_ */

