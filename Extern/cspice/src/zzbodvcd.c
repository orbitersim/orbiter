/* zzbodvcd.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure ZZBODVCD ( Get d.p. kernel variable for body, with bypass ) */
/* Subroutine */ int zzbodvcd_(integer *bodyid, char *item, integer *maxn, 
	integer *varctr, integer *n, doublereal *values, ftnlen item_len)
{
    extern /* Subroutine */ int zzpctrck_(integer *, logical *), chkin_(char *
	    , ftnlen);
    extern logical failed_(void);
    extern /* Subroutine */ int bodvcd_(integer *, char *, integer *, integer 
	    *, doublereal *, ftnlen);
    logical update;
    extern /* Subroutine */ int chkout_(char *, ftnlen);
    extern logical return_(void);

/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines. Users should not call this routine directly due to the */
/*     volatile nature of this routine. */

/*     Fetch from the kernel pool the double precision values of an item */
/*     associated with a body, where the body is specified by an integer */
/*     ID code. Perform lookup only if kernel variable or kernel pool */
/*     state has changed; otherwise, bypass kernel pool lookup and */
/*     return input values. */

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

/*     KERNEL */
/*     NAIF_IDS */

/* $ Keywords */

/*     CONSTANTS */

/* $ Declarations */
/* $ Abstract */

/*     This include file defines the dimension of the counter */
/*     array used by various SPICE subsystems to uniquely identify */
/*     changes in their states. */

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

/* $ Parameters */

/*     CTRSIZ      is the dimension of the counter array used by */
/*                 various SPICE subsystems to uniquely identify */
/*                 changes in their states. */

/* $ Author_and_Institution */

/*     B.V. Semenov    (JPL) */

/* $ Literature_References */

/*     None. */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 29-JUL-2013 (BVS) */

/* -& */

/*     End of include file. */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     BODYID     I   Body ID code. */
/*     ITEM       I   Item for which values are desired. */
/*     MAXN       I   Maximum number of values that may be returned. */
/*     VARCTR    I-O  POOL state counter saved by the caller. */
/*     N         I-O  Number of values returned. */
/*     VALUES    I-O  Array of double precision values. */

/* $ Detailed_Input */

/*     BODYID     is the NAIF integer ID code for a body of interest. */
/*                For example, if the body is the earth, the code is */
/*                399. */


/*     ITEM       is the item to be returned. Together, the NAIF ID */
/*                code of the body and the item name combine to form a */
/*                kernel variable name, e.g., */

/*                      'BODY599_RADII' */
/*                      'BODY401_POLE_RA' */

/*                The values associated with the kernel variable having */
/*                the name constructed as shown are sought. Below */
/*                we'll take the shortcut of calling this kernel variable */
/*                the "requested kernel variable." */

/*                Note that ITEM *is* case-sensitive. This attribute */
/*                is inherited from the case-sensitivity of kernel */
/*                variable names. */


/*     MAXN       is the maximum number of values that may be returned. */
/*                The output array VALUES must be declared with size at */
/*                least MAXN.  It's an error to supply an output array */
/*                that is too small to hold all of the values associated */
/*                with the requested kernel variable. */


/*     VARCTR     is, on input, a counter used to represent the kernel */
/*                pool state at the time the kernel variable designated */
/*                by BODYID and ITEM was set. */

/*                Note that a distinct actual argument corresponding to */
/*                VARCTR is required for each kernel variable to be */
/*                updated by this routine. */


/*     N          is, on input, the dimension of the saved kernel */
/*                variable designated by BODYID and ITEM. */


/*     VALUES     is, on input, the set of values associated with the */
/*                kernel variable designated by BODYID and ITEM. */

/* $ Detailed_Output */

/*     VARCTR     is, on output, a counter used to represent the current */
/*                kernel pool state. */

/*     N          is, on output, the dimension of the kernel variable */
/*                designated by BODYID and ITEM. */

/*     VALUES     is, on output, the set of values associated with the */
/*                kernel variable designated by BODYID and ITEM. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the requested kernel variable is not found in the kernel */
/*         pool, the error SPICE(KERNELVARNOTFOUND) is signaled. */

/*     2)  If the requested kernel variable is found but the associated */
/*         values aren't numeric, an error is signaled by a routine in */
/*         the call tree of this routine. */

/*     3)  The output array VALUES must be declared with sufficient size */
/*         to contain all of the values associated with the requested */
/*         kernel variable.  If the dimension of VALUES indicated by */
/*         MAXN is too small to contain the requested values, an error */
/*         is signaled by a routine in the call tree of this routine. */

/*     4)  If the input dimension MAXN indicates there is more room in */
/*         VALUES than there really is---for example, if MAXN is 10 but */
/*         values is declared with dimension 5---and the dimension of */
/*         the requested kernel variable is larger than the actual */
/*         dimension of VALUES, then this routine may overwrite memory. */
/*         The results are unpredictable. */

/*     5)  If a lookup of the requested variable fails, the argument */
/*         N is set to zero. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine is meant to enable efficient use of BODVCD. This */
/*     routine calls BODVCD only if the requested kernel variable values */
/*     may have changed from their previous values. */

/*     This routine simplifies looking up PCK kernel variables by */
/*     constructing names of requested kernel variables and by */
/*     performing error checking. */

/*     This routine is intended for use in cases where the maximum */
/*     number of values that may be returned is known at compile time. */
/*     The caller fetches all of the values associated with the */
/*     specified kernel variable via a single call to this routine. If */
/*     the number of values to be fetched cannot be known until run */
/*     time, the lower-level routine GDPOOL (an entry point of POOL) */
/*     should be used instead. GDPOOL supports fetching arbitrary */
/*     amounts of data in multiple "chunks." */

/*     This routine is intended for use in cases where the requested */
/*     kernel variable is expected to be present in the kernel pool. If */
/*     the variable is not found or has the wrong data type, this */
/*     routine signals an error. In cases where it is appropriate to */
/*     indicate absence of an expected kernel variable by returning a */
/*     boolean "found flag" with the value .FALSE., again the routine */
/*     GDPOOL should be used. */

/* $ Examples */

/*     1)  The call below can be used to fetch initial values of the */
/*         radii for the earth (body 399). */

/*         When the kernel variable */

/*            BODY399_RADII */

/*         is present in the kernel pool---normally because a PCK */
/*         defining this variable has been loaded---the code fragment */

/*            CALL ZZCTRUIN ( CTR1 ) */

/*            BODYID = 399 */
/*            ITEM   = 'RADII' */

/*            CALL ZZBODVCD ( BODYID, ITEM, MAXN, CTR1, N, RADII ) */

/*         returns the dimension and values associated with the variable */
/*         'BODY399_RADII', for example, */

/*            DIM      = 3 */
/*            RADII(1) = 6378.140 */
/*            RADII(2) = 6378.140 */
/*            RADII(3) = 6356.755 */

/*         The call to ZZCTRUIN initializes CTR1, which forces ZZBODVCD */
/*         to update the output values */


/*     2)  After the call in example (1) has been made, the variable */

/*            BODY399_RADII */

/*         can be set to its current value by the call */

/*            CALL ZZBODVCD ( BODYID, ITEM, MAXN, CTR1, N, RADII ) */

/*         If the kernel pool has not been modified since the previous */
/*         call, the arguments */

/*            CTR1 */
/*            N */
/*            RADII */

/*         are unchanged on output. */


/*     3)  After the call in example (2) has been made, the variable */

/*            BODY499_POLE_RA */

/*         can be set to its current value by the code fragment */

/*            CALL ZZCTRUIN ( CTR2 ) */

/*            BODY   = 499 */
/*            ITEM   = 'POLE_RA' */

/*            CALL ZZBODVCD ( BODYID, ITEM, MAXN, CTR2, N, RA ) */

/*         On output, the arguments */

/*            N */
/*            RA */

/*         will be updated to (based on PCK pck00010.tpc): */

/*            3 */
/*            499 */
/*            'POLE_RA' */
/*            317.68143   -0.1061      0. */

/*         CTR2 will be updated if the kernel pool contents have */
/*         changed. */


/*     4) The call */

/*            BODY   = 399 */
/*            ITEM   = 'radii' */

/*            CALL ZZBODVCD ( BODYID, ITEM, MAXN, CTR1, N, VALUES ) */

/*        usually will cause a SPICE(KERNELVARNOTFOUND) error to be */
/*        signaled, because this call will attempt to look up the */
/*        values associated with a kernel variable of the name */

/*           'BODY399_radii' */

/*        Since kernel variable names are case sensitive, this */
/*        name is not considered to match the name */

/*           'BODY399_RADII' */

/*        which normally would be present after a text PCK */
/*        containing data for all planets and satellites has */
/*        been loaded. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman    (JPL) */
/*     B.V. Semenov    (JPL) */
/*     W.L. Taber      (JPL) */
/*     I.M. Underwood  (JPL) */

/* $ Version */

/* -    SPICELIB Version 2.0.0, 20-OCT-2015 (NJB) (BVS) (WLT) (IMU) */

/* -& */
/* $ Index_Entries */

/*     fetch constants for a body from the kernel pool */
/*     physical constants for a body */

/* -& */

/*     SPICELIB functions */


/*     Local variables */

    if (return_()) {
	return 0;
    }
    chkin_("ZZBODVCD", (ftnlen)8);

/*     See whether the kernel pool state has changed since the */
/*     user counter was set. Update the user counter if so. */

    zzpctrck_(varctr, &update);

/*     If the pool was updated, or if we're looking at a new variable, */
/*     update the kernel variable values, size, and found flag. */
/*     Otherwise do nothing. */

    if (update) {
	bodvcd_(bodyid, item, maxn, n, values, item_len);
	if (failed_()) {
	    *n = 0;
	}
    }
    chkout_("ZZBODVCD", (ftnlen)8);
    return 0;
} /* zzbodvcd_ */

