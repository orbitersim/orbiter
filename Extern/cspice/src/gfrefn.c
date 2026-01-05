/* gfrefn.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure GFREFN ( GF, default refinement estimator ) */
/* Subroutine */ int gfrefn_(doublereal *t1, doublereal *t2, logical *s1, 
	logical *s2, doublereal *t)
{
    doublereal x;
    extern doublereal brcktd_(doublereal *, doublereal *, doublereal *);

/* $ Abstract */

/*     Estimate, using a bisection method, the next abscissa value at */
/*     which a state change occurs. This is the default GF refinement */
/*     method. */

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

/*     SEARCH */
/*     UTILITY */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     T1         I   One of two values bracketing a state change. */
/*     T2         I   The other value that brackets a state change. */
/*     S1         I   State at T1. */
/*     S2         I   State at T2. */
/*     T          O   New value at which to check for transition. */

/* $ Detailed_Input */

/*     T1       is one of two abscissa values (usually times) */
/*              bracketing a state change. */

/*     T2       is the other abscissa value that brackets a state change. */

/*     S1       is the system state at T1. This argument is provided */
/*              for forward compatibility; it's not currently used. */

/*     S2       is the system state at T2. This argument is provided */
/*              for forward compatibility; it's not currently used. */

/* $ Detailed_Output */

/*     T        is the midpoint of T1 and T2. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     "Refinement" means reducing the size of a bracketing interval on */
/*     the real line in which a solution is known to lie. In the GF */
/*     setting, the solution is the time of a state transition of a */
/*     binary function. */

/*     This routine supports solving for locations of bracketed state */
/*     transitions by the bisection method. This is the default */
/*     refinement method used by the GF system. */

/*     The argument list of this routine is compatible with the GF */
/*     system's general root finding routine. Refinement routines created */
/*     by users must have the same argument list in order to be used by */
/*     the GF mid-level APIs such as GFOCCE and GFFOVE. */

/* $ Examples */

/*     The following code fragment from an example program in the header */
/*     of GFOCCE shows the routine passed as the 12th argument. */

/*        C */
/*        C     Define as EXTERNAL the routines to pass to GFOCCE. */
/*        C */
/*              EXTERNAL              GFSTEP */
/*              EXTERNAL              GFREFN */
/*              EXTERNAL              GFREPI */
/*              EXTERNAL              GFREPU */
/*              EXTERNAL              GFREPF */
/*              EXTERNAL              GFBAIL */

/*                 ... initialize for the search ... */

/*              CALL GFOCCE ( 'ANY', */
/*             .              'MOON',   'ellipsoid',  'IAU_MOON', */
/*             .              'SUN',    'ellipsoid',  'IAU_SUN', */
/*             .              'LT',     'EARTH',       CNVTOL, */
/*             .               GFSTEP,   GFREFN,       RPT, */
/*             .               GFREPI,   GFREPU,       GFREPF, */
/*             .               BAIL,     GFBAIL,       CNFINE,  RESULT ) */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     E.D. Wright        (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.1, 26-OCT-2021 (JDR) */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 1.0.0, 03-MAR-2009 (NJB) (EDW) */

/* -& */
/* $ Index_Entries */

/*     GF standard step refinement */

/* -& */

/*     SPICELIB functions */


/*     Local variables. */

    x = *t1 * .5 + *t2 * .5;
    *t = brcktd_(&x, t1, t2);
    return 0;
} /* gfrefn_ */

