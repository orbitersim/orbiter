/* kepleq.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure KEPLEQ ( Solve Kepler's Equation --- Equinoctial Form ) */
doublereal kepleq_(doublereal *ml, doublereal *h__, doublereal *k)
{
    /* System generated locals */
    doublereal ret_val;

    /* Builtin functions */
    double cos(doublereal), sin(doublereal);

    /* Local variables */
    doublereal evec[2];
    extern /* Subroutine */ int chkin_(char *, ftnlen), errdp_(char *, 
	    doublereal *, ftnlen);
    doublereal e2;
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), setmsg_(char *, ftnlen);
    extern doublereal kpsolv_(doublereal *);

/* $ Abstract */

/*     Solve the equinoctial version of Kepler's equation. */

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

/*     SPK */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     ML         I   Mean longitude. */
/*     H          I   h component of equinoctial elements. */
/*     K          I   k component of equinoctial elements. */

/*     The function returns the solution to the equinoctial version of */
/*     Kepler's equation, given the mean longitude and the h and k */
/*     components of the equinoctial elements. */

/* $ Detailed_Input */

/*     ML       is the mean longitude of some body following two body */
/*              motion. (Mean longitude = Mean anomaly + argument of */
/*              periapse + longitude of ascending node.) */

/*     H        is the h component of the equinoctial element set */
/*              ( h = ECC*SIN( arg of periapse + long ascending node) ) */

/*     K        is the k component of the equinoctial element set */
/*              ( k = ECC*COS( arg of periapse + long ascending node) ) */

/* $ Detailed_Output */

/*     The function returns the solution to the equinoctial version of */
/*     Kepler's equation, given the mean longitude and the h and k */
/*     components of the equinoctial elements. */

/*     The solution is the value of F such that */

/*        ML = F + H * COS(F) - K * SIN(F) */

/*     Note that ECC = DSQRT ( K*K + H*H ) */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the sum of the squares of H and K is not less than .9, */
/*         the error SPICE(ECCOUTOFBOUNDS) is signaled. */

/*     2)  If the iteration for a solution to the equinoctial Kepler's */
/*         equation does not converge in 10 or fewer steps, the error */
/*         SPICE(NOCONVERGENCE) is signaled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine solves the equinoctial element version of */
/*     Kepler's equation. */

/*        ML = F + H * COS(F) - K * SIN(F) */

/*     Here F is an offset from the eccentric anomaly E. */

/*        F = E - argument of periapse - longitude of ascending node. */

/*     where E is eccentric anomaly. */

/* $ Examples */

/*     None. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     [1]  W. Owen and R. Vaughan, "Optical Navigation Program */
/*          Mathematical Models," JPL Engineering Memorandum 314-513, */
/*          August 9, 1991. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.1, 26-AUG-2021 (JDR) */

/*        Edited the header to comply with NAIF standard. Updated */
/*        $Procedure section for consistency with KPSOLV routine. */

/* -    SPICELIB Version 1.0.0, 11-DEC-1996 (WLT) */

/* -& */
/* $ Index_Entries */

/*     Solve the equinoctial version of Kepler's equation */

/* -& */

/*     SPICELIB Functions */


/*     Local variables */


/*     Make sure that H and K are in the expected range. */

    e2 = *h__ * *h__ + *k * *k;
    if (e2 >= .81) {
	ret_val = 0.;
	chkin_("KEPLEQ", (ftnlen)6);
	setmsg_("The values of H and K supplied to KEPLEQ must satisfy the i"
		"nequality H*H + K*K < ECC**2 where ECC is the eccentricity t"
		"hreshold of 0.9.  The values of H and K are: # and # respect"
		"ively. H*H + K*K = #. ", (ftnlen)201);
	errdp_("#", h__, (ftnlen)1);
	errdp_("#", k, (ftnlen)1);
	errdp_("#", &e2, (ftnlen)1);
	sigerr_("SPICE(ECCOUTOFBOUNDS)", (ftnlen)21);
	chkout_("KEPLEQ", (ftnlen)6);
	return ret_val;
    }

/*     Instead of solving the equation */

/*            ML  = F + H*DCOS(F) - K*DSIN(F) */

/*     We set X equal to F - ML and solve the equivalent equation */

/*            0   = X + H*DCOS(ML+X) - K*DSIN(ML+X) */

/*                = X + H*{DCOS(ML)*DCOS(X) - DSIN(ML)*DSIN(X)} */
/*                    - K*{DSIN(ML)*DCOS(X) + DCOS(ML)*DSIN(X)} */

/*                = X + { H*DCOS(ML) - K*DSIN(ML) }*DCOS(X) */
/*                    - { H*DSIN(ML) + K*DCOS(ML) }*DSIN(X) */


/*     We can rearrange this to: */

/*                                 -                    -     -       - */
/*                                |  DCOS(ML)  -DSIN(ML) |   | DCOS(X) | */
/*            0 = X + [ H  -K ] * |  DSIN(ML)   DCOS(ML) | * | DSIN(X) | */
/*                                 -                    -     -       - */

/*     Finally if we let */

/*                                        -                    - */
/*                                       |  DCOS(ML)  -DSIN(ML) | */
/*      EVEC =  [ EX  EY ] = [ -H  K ] * |  DSIN(ML)   DCOS(ML) | */
/*                                        -                    - */

/*     and */

/*              DCOS(X) */
/*      U(X) =  DSIN(X) */

/*     Then we can rewrite the equation as: */

/*        0  =  X - < EVEC, U(X) > */

/*     where <,> denotes the dot product operation.  Note that X */
/*     is necessarily in the range from -ECC to ECC where ECC = | EVEC | */

/*     Once we've computed X, F is just ML + X. */

/*     For those of you who are fans of the classical keplerian */
/*     elements: */

/*        x = F - ML = E - M */

/*     where E denotes eccentric anomaly and M denotes mean anomaly. */

/*     The routine KPEVEC returns the value of X that solves */
/*     the equation X - < EVEC, UVEC(X) > */

    evec[0] = -(*h__) * cos(*ml) + *k * sin(*ml);
    evec[1] = *h__ * sin(*ml) + *k * cos(*ml);
    ret_val = *ml + kpsolv_(evec);
    return ret_val;
} /* kepleq_ */

