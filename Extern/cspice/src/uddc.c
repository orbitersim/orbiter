/* uddc.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure UDDC ( Derivative of function less than zero, df(x)/dx < 0 ) */
/* Subroutine */ int uddc_(U_fp udfunc, doublereal *x, doublereal *dx, 
	logical *isdecr)
{
    extern /* Subroutine */ int uddf_(U_fp, doublereal *, doublereal *, 
	    doublereal *), chkin_(char *, ftnlen);
    doublereal deriv;
    extern logical failed_(void);
    extern /* Subroutine */ int chkout_(char *, ftnlen);
    extern logical return_(void);

/* $ Abstract */

/*     Return .TRUE. if the derivative of the callback function UDFUNC */
/*     at a given abscissa value is negative. */

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

/*     DERIVATIVE */
/*     MATH */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     UDFUNC     I   The routine that computes the scalar value */
/*                    of interest. */
/*     X          I   Independent variable of UDFUNC. */
/*     DX         I   Interval from X for derivative calculation. */
/*     ISDECR     O   Boolean indicating if the derivative is negative. */

/* $ Detailed_Input */

/*     UDFUNC   is the routine that returns the value of the scalar */
/*              quantity function of interest at X. The calling */
/*              sequence for UDFUNC is: */

/*                 CALL UDFUNC ( X, VALUE ) */

/*              where: */

/*                 X       the double precision value of the */
/*                         independent variable of the function */
/*                         at which to determine the scalar value. */

/*                 VALUE   the double precision value returned by */
/*                         UDFUNC at X. */

/*              Functionally: */

/*                 VALUE = UDFUNC ( X ) */

/*     X        is a scalar double precision value at which to determine */
/*              the derivative of UDFUNC. */

/*              For many SPICE uses, X will represent ephemeris time, */
/*              expressed as seconds past J2000 TDB. */

/*     DX       is a scalar double precision value representing half the */
/*              interval in units of X separating the evaluation */
/*              values of UDFUNC; the evaluations occur at (X + DX) */
/*              and (X - DX). */

/*              DX may be negative but must be non-zero. */

/* $ Detailed_Output */

/*     ISDECR   is a scalar boolean indicating if the first derivative */
/*              of UDFUNC with respect to the independent variable */
/*              at X is less than zero. */

/*              Functionally: */

/*                            d UDFUNC(x) | */
/*                 ISDECR =   ----------- |  <  0 */
/*                                 dx     | */
/*                                         X */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If DX has a value of zero, an error is signaled by a routine */
/*         in the call tree of this routine. */

/* $ Files */

/*     If the evaluation of UDFUNC requires SPICE kernel data, the */
/*     appropriate kernels must be loaded before calling this routine. */

/*     -  SPK data: the calling application must load ephemeris data */
/*        for the targets, observer, and any intermediate objects in */
/*        a chain connecting the targets and observer for the time */
/*        used in the evaluation. If aberration corrections are */
/*        used, the states of target and observer relative to the */
/*        solar system barycenter must be calculable from the */
/*        available ephemeris data. */

/*     -  If non-inertial reference frames are used, then PCK */
/*        files, frame kernels, C-kernels, and SCLK kernels may be */
/*        needed. */

/*     Such kernel data are normally loaded once per program run, NOT */
/*     every time this routine is called. */

/* $ Particulars */

/*     This routine only wraps a UDDF call, examining the sign of the */
/*     derivative value returned by UDDF. Please refer to this routine */
/*     for further details. */

/* $ Examples */

/*     See GFUDS. */

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

/* -    SPICELIB Version 1.0.0, 31-MAR-2010 (EDW) (NJB) */

/* -& */
/* $ Index_Entries */

/*     first derivative of scalar function less than zero */

/* -& */

/*     SPICELIB functions */


/*     Local Variables */

    if (return_()) {
	return 0;
    }
    chkin_("UDDC", (ftnlen)4);
    *isdecr = FALSE_;

/*     Numerically calculate the derivative of UDFUNC at X. */

    uddf_((U_fp)udfunc, x, dx, &deriv);
    if (failed_()) {
	chkout_("UDDC", (ftnlen)4);
	return 0;
    }
    *isdecr = deriv < 0.;
    chkout_("UDDC", (ftnlen)4);
    return 0;
} /* uddc_ */

