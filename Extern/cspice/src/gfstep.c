/* gfstep.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure GFSTEP ( GF, step size ) */
/* Subroutine */ int gfstep_0_(int n__, doublereal *time, doublereal *step)
{
    /* Initialized data */

    static logical svinit = FALSE_;
    static doublereal svstep = -1.;

    extern /* Subroutine */ int chkin_(char *, ftnlen), errdp_(char *, 
	    doublereal *, ftnlen), sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), setmsg_(char *, ftnlen);

/* $ Abstract */

/*     Return the time step set by the most recent call to GFSSTP. */

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
/*     TIME */

/* $ Keywords */

/*     GEOMETRY */
/*     SEARCH */
/*     UTILITY */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     TIME       I   Ignored ET value. */
/*     STEP       O   Time step to take. */

/* $ Detailed_Input */

/*     TIME     is an ignored double precision number. This argument */
/*              is present so the argument list of this routine is */
/*              compatible with the GF step size routine argument list */
/*              specification. */

/*              When this routine is called from within the GF */
/*              root-finding system, either the initial ET value of the */
/*              current interval of the confinement window, or the */
/*              value resulting from the last search step, is passed in */
/*              via the TIME argument. */

/* $ Detailed_Output */

/*     STEP     is the output step size. This is the value set by the */
/*              most recent call to GFSSTP. Units are TDB seconds. */

/*              STEP is used in the GF search root-bracketing process. */
/*              STEP indicates how far to advance TIME so that TIME and */
/*              TIME+STEP may bracket a state transition and definitely */
/*              do not bracket more than one state transition. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If this routine is called before a step size has been */
/*         set via a call to GFSSTP, the error SPICE(NOTINITIALIZED) */
/*         is signaled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine returns the time step set by the most recent call to */
/*     GFSSTP. */

/* $ Examples */

/*     1) In normal usage of a high-level GF API routine, the caller */
/*        will pass in a constant step size STEP. The API routine will */
/*        then make the call */

/*           CALL GFSSTP ( STEP ) */

/*        Subsequent calls to GFSTEP during the search process conducted */
/*        by the API routine will return STEP. */


/*     2) User applications can pass GFSTEP to mid-level GF API routines */
/*        expecting a step size routine as an input argument. For */
/*        example, the GF API routine GFOCCE can be called as follows: */


/*           Set the step size. */

/*           CALL GFSSTP ( STEP ) */


/*           Look for solutions. (GFSTEP is the 11th argument.) */

/*           CALL GFOCCE ( OCCTYP,  FRONT,   FSHAPE,  FFRAME, */
/*          .              BACK,    BSHAPE,  BFRAME,  ABCORR, */
/*          .              OBSRVR,  CNVTOL,  GFSTEP,  GFREFN, */
/*          .              RPT,     GFREPI,  GFREPU,  GFREPF, */
/*          .              BAIL,    GFBAIL,  CNFINE,  RESULT ) */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     L.S. Elson         (JPL) */
/*     W.L. Taber         (JPL) */
/*     I.M. Underwood     (JPL) */
/*     E.D. Wright        (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.1, 03-JUN-2021 (JDR) */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 1.1.0, 31-AUG-2010 (EDW) */

/*        Expanded error message on STEP for clarity. */

/*        Added TIME = TIME declaration to eliminate unused dummy */
/*        variable warning during compilation. */

/* -    SPICELIB Version 1.0.0, 05-MAR-2009 (NJB) (LSE) (IMU) (WLT) (EDW) */

/* -& */
/* $ Index_Entries */

/*     GF get constant step size */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     Saved variables */


/*     Initial values */

    switch(n__) {
	case 1: goto L_gfsstp;
	}


/*     Discovery check-in. */

    if (! svinit) {
	chkin_("GFSTEP", (ftnlen)6);
	setmsg_("Step size was never initialized.", (ftnlen)32);
	sigerr_("SPICE(NOTINITIALIZED)", (ftnlen)21);
	chkout_("GFSTEP", (ftnlen)6);
	return 0;
    }

/*     Set STEP to the saved value from the last call to GFSSTP. */

    *step = svstep;
    *time = *time;
    return 0;
/* $Procedure GFSSTP ( Geometry finder set step size ) */

L_gfsstp:
/* $ Abstract */

/*     Set the step size to be returned by GFSTEP. */

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
/*     TIME */

/* $ Keywords */

/*     GEOMETRY */
/*     SEARCH */
/*     UTILITY */

/* $ Declarations */

/*     DOUBLE PRECISION      STEP */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     STEP       I   Time step to take. */

/* $ Detailed_Input */

/*     STEP     is the output step size to be returned by the next call */
/*              GFSTEP. Units are TDB seconds. */

/*              STEP is used in the GF search root-bracketing process. */
/*              STEP indicates how far to advance TIME so that TIME and */
/*              TIME+STEP may bracket a state transition and definitely */
/*              do not bracket more than one state transition. */

/* $ Detailed_Output */

/*     None. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the input step size is non-positive, the error */
/*         SPICE(INVALIDSTEP) is signaled. The stored step value */
/*         is not updated. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     See the header of GFSTEP above. */

/* $ Examples */

/*     See the header of GFSTEP above. */

/* $ Restrictions */

/*     1)  This routine must be called before the first time */
/*         GFSTEP is called during a program run. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     L.S. Elson         (JPL) */
/*     E.D. Wright        (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.1, 03-JUN-2021 (JDR) */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 1.1.0, 31-AUG-2010 (EDW) */

/*        Expanded error message on STEP for clarity. */

/* -    SPICELIB Version 1.0.0, 15-APR-2009 (LSE) (NJB) */

/* -& */
/* $ Index_Entries */

/*     GF set constant step size */

/* -& */

/*     Check the step size. */

    if (*step <= 0.) {
	chkin_("GFSSTP", (ftnlen)6);
	setmsg_("Step has value #; step size must be positive.", (ftnlen)45);
	errdp_("#", step, (ftnlen)1);
	sigerr_("SPICE(INVALIDSTEP)", (ftnlen)18);
	chkout_("GFSSTP", (ftnlen)6);
	return 0;
    }
    svstep = *step;
    svinit = TRUE_;
    return 0;
} /* gfstep_ */

/* Subroutine */ int gfstep_(doublereal *time, doublereal *step)
{
    return gfstep_0_(0, time, step);
    }

/* Subroutine */ int gfsstp_(doublereal *step)
{
    return gfstep_0_(1, (doublereal *)0, step);
    }

