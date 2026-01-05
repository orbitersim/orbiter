/* gfbail.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure GFBAIL ( GF, default bailout function ) */
logical gfbail_(void)
{
    /* System generated locals */
    logical ret_val;

/* $ Abstract */

/*     Serve as a placeholder for an interrupt detection function. */

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

/*     INTERRUPT */
/*     UTILITY */

/* $ Declarations */
/* $ Brief_I/O */

/*     The function returns the value .FALSE. in all cases. */

/* $ Detailed_Input */

/*     None. */

/* $ Detailed_Output */

/*     The function returns the value .FALSE. in all cases. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine serves as a stub for interrupt function input */
/*     arguments in GF mid-level search routines such as */

/*        GFEVNT */
/*        GFOCCE */
/*        GFFOVE */

/*     Those routines allow the caller to pass in a custom interrupt */
/*     detection function. */

/*     Searches conducted with the GF APIs can be unexpectedly */
/*     time-consuming. When such searches are carried out by an */
/*     interactive application, it can be useful to be able to stop a */
/*     search without stopping the application itself. This enables a */
/*     user to avoid loss of previous work that may have been performed */
/*     during the program run. */

/*     The mid-level GF search APIs named above provide partial support */
/*     for interrupt handling. They allow the caller to pass in an */
/*     interrupt detection function; when their input "bail-out" flag */
/*     argument is set to .TRUE. by the caller, the low-level GF */
/*     root-finding routines invoked by these APIs will, over regular, */
/*     short time intervals (these intervals are usually determined by */
/*     the completion of loop passes), call the interrupt detection */
/*     function. These routines will return immediately if the function */
/*     indicates that an interrupt has occurred. */

/*     However, SPICELIB doesn't fully support interrupt handling */
/*     because ANSI Fortran 77 doesn't provide the features necessary to */
/*     implement an interrupt detection function. */

/*     Some Fortran platforms do provide non-standard routines that */
/*     support interrupt handling, so for these systems, SPICE users may */
/*     be able to create their own interrupt detection routines. Such */
/*     routines should have calling sequences identical to that of this */
/*     function. These routines should have a "reset" feature that */
/*     enables an application to make them return .FALSE. after an */
/*     interrupt has been indicated and processed. */

/*     For platforms where interrupt detection can't be implemented, or */
/*     in cases where applications must call mid-level GF APIs but don't */
/*     need interrupt handling, this routine can be used. */

/*     This routine has no interrupt detection capability: it always */
/*     returns the value .FALSE. */

/*     Developers of SPICE-based applications who have the choice of */
/*     writing code in Fortran or C may wish to consider the fact that */
/*     the CSPICE Toolkit does support interrupt detection: gfbail_c, */
/*     the CSPICE analog of this routine, is fully functional on all */
/*     platforms on which CSPICE is supported. */

/* $ Examples */

/*     This example shows how to call a mid-level GF search API that */
/*     requires an input interrupt detection function. */

/*     If a custom interrupt detection function is available, it */
/*     can be referenced exactly as is GFBAIL in this example. */

/*     The code fragment below is from the first code example in the */
/*     header of */

/*        gfocce.for */

/*     Only the portions of that program relevant to use of GFBAIL are */
/*     copied here. Deleted portions of code are indicated by ellipses. */

/*     Note that GFBAIL is the third-to-last argument in the */
/*     GFOCCE call. */


/*              PROGRAM EX1 */

/*              IMPLICIT NONE */

/*              ... */

/*              LOGICAL               GFBAIL */
/*              EXTERNAL              GFBAIL */

/*              ... */

/*        C */
/*        C     Turn on progress reporting; turn off interrupt */
/*        C     handling. */
/*        C */

/*              ... */

/*              BAIL = .FALSE. */

/*        C */
/*        C     Perform the search. */
/*        C */
/*              CALL GFOCCE ( 'ANY', */
/*             .              'MOON',   'ellipsoid',  'IAU_MOON', */
/*             .              'SUN',    'ellipsoid',  'IAU_SUN', */
/*             .              'LT',     'EARTH',      CNVTOL, */
/*             .              GFSTEP,   GFREFN,       RPT, */
/*             .              GFREPI,   GFREPU,       GFREPF, */
/*             .              BAIL,     GFBAIL,       CNFINE,  RESULT ) */


/*             ... */

/* $ Restrictions */

/*     1)  This is a stub routine. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     L.S. Elson         (JPL) */
/*     E.D. Wright        (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.2, 17-AUG-2021 (JDR) */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 1.0.1, 23-JUN-2010 (EDW) */

/*        Minor edit to $Declarations for correct header format. */

/* -    SPICELIB Version 1.0.0, 05-MAR-2009 (NJB) (LSE) (EDW) */

/* -& */
/* $ Index_Entries */

/*     GF standard bail out routine */

/* -& */
    ret_val = FALSE_;
    return ret_val;
} /* gfbail_ */

