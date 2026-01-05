/* zzgfudlt.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c_n1 = -1;
static integer c__2 = 2;

/* $Procedure ZZGFUDLT ( Private --- GF, scalar function < ref value ) */
/* Subroutine */ int zzgfudlt_(S_fp udfunc, doublereal *et, logical *isless)
{
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    doublereal udval;
    extern logical failed_(void);
    logical ok;
    doublereal refval;
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), setmsg_(char *, ftnlen);
    extern logical return_(void);
    extern /* Subroutine */ int zzholdd_(integer *, integer *, logical *, 
	    doublereal *);

/* $ Abstract */

/*     SPICE private routine intended solely for the support of SPICE */
/*     routines. Users should not call this routine directly due to the */
/*     volatile nature of this routine. */

/*     This routine determines if the value of the scalar quantity */
/*     function is less than a previously defined reference value. */

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
/*     MATH */

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

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     ZZGET      P   ZZHOLDD retrieves a stored DP value. */
/*     GF_REF     P   ZZHOLDD acts on the reference value */
/*                    for a GF search. */
/*     UDFUNC     I   Name of the routine that computes the scalar value */
/*                    of interest at ET. */
/*     ET         I   Time in TDB seconds for which to evaluate UDFUNC. */
/*     ISLESS     O   Boolean indicating if the scalar value is less than */
/*                    reference value. */

/* $ Detailed_Input */

/*     UDFUNC     the routine that returns the value of the scalar */
/*                quantity of interest at time ET. The calling sequence */
/*                for UDFUNC is: */

/*                   CALL UDFUNC ( ET, VALUE ) */

/*                where: */

/*                   ET      a double precision value representing */
/*                           ephemeris time, expressed as seconds past */
/*                           J2000 TDB, at which to determine the scalar */
/*                           value. */

/*                   VALUE   is the value of the geometric quantity */
/*                           at ET. */

/*     ET         a double precision value representing ephemeris time, */
/*                expressed as seconds past J2000 TDB at which to */
/*                determine the value of UDFUNC. */

/* $ Detailed_Output */

/*     ISLESS     a scalar boolean indicating if the value of UDFUNC at */
/*                ET is less than REFVAL (true) or not (false). */

/*                 Functionally: */

/*                   ISLESS = UDFUNC( ET )  <  REFVAL */

/* $ Parameters */

/*    None. */

/* $ Exceptions */

/*     1) ZZHOLDD will signal the error SPICE(ZZHOLDNOPUT) if this */
/*        routine is called prior to storing a reference value */
/*        using a ZZHOLDD "PUT" operation. */

/* $ Files */

/*    None. */

/* $ Particulars */

/*     A ZZHOLDD "PUT" stored the reference value used in the logical */
/*     operation. A ZZHOLDD "GET" retrieves the value. */

/* $ Examples */

/*    See GFUDS. */

/* $ Restrictions */

/*    None. */

/* $ Literature_References */

/*    None. */

/* $ Author_and_Institution */

/*    N.J. Bachman   (JPL) */
/*    E.D. Wright    (JPL) */

/* $ Version */

/* -   SPICELIB Version 1.1.0, 06-AUG-2010 (EDW) */

/*       Change in ZZHOLDD functionality required edit to ZZHOLDD */
/*       call and inclusion of "zzholdd.inc" parameters file. */

/* -   SPICELIB Version 1.0.0, 16-FEB-2010 (EDW) */

/* -& */
/* $ Index_Entries */

/*    function less than reference value */

/* -& */

/*     SPICELIB functions */


/*     Local Variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }
    chkin_("ZZGFUDLT", (ftnlen)8);

/*     Default value of false for ISLESS. */

    *isless = FALSE_;

/*     Call the routine, return the scalar value corresponding to ET. */

    (*udfunc)(et, &udval);

/*     Check for an error, return if found. */

    if (failed_()) {
	chkout_("ZZGFUDLT", (ftnlen)8);
	return 0;
    }

/*     Retrieve the stored reference value. Signal an error if a */
/*     get call fails. */

    zzholdd_(&c_n1, &c__2, &ok, &refval);
    if (! ok) {
	setmsg_("ZZHOLDD GET failed. This indicates a logic error in the GF "
		"code due either to a failure to store the GF reference value"
		" or a post store reset of ZZHOLDD.", (ftnlen)153);
	sigerr_("SPICE(ZZHOLDDGETFAILED)", (ftnlen)23);
	chkout_("ZZGFUDLT", (ftnlen)8);
	return 0;
    }
    *isless = udval < refval;
    chkout_("ZZGFUDLT", (ftnlen)8);
    return 0;
} /* zzgfudlt_ */

