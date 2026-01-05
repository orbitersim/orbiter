/* zzsecprt.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure ZZSECPRT ( Calculate dot terms for DPSPCE perturbation ) */
/* Subroutine */ int zzsecprt_(integer *isynfl, doublereal *dg, doublereal *
	del, doublereal *xni, doublereal *omegao, doublereal *atime, 
	doublereal *omgdot, doublereal *xli, doublereal *xfact, doublereal *
	xldot, doublereal *xndot, doublereal *xnddt)
{
    /* Builtin functions */
    double sin(doublereal), cos(doublereal);

    /* Local variables */
    doublereal xomi, x2omi, x2li;

/* $ Abstract */

/*    Routine to calculate the dot terms for the secular perturbation */
/*    of a vehicle. */

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

/*     SECULAR PERTURBATION */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     ISYNFL     I   Resonance flag */
/*     DG         I   Parameter array */
/*     DEL        I   Parameter array of delta values */
/*     XNI        I   An intermediate linear term passed from the */
/*                    calling routine */
/*     OMEGAO     I   Original argument of perigee */
/*     ATIME      I   An intermediate time term passed from the calling */
/*                    routine */
/*     OMGDOT     I   Time rate of change of argument of perigee */
/*     XLI        I   An intermediate angular term passed from the */
/*                    calling routine */
/*     XFACT      I   The value BFACT - XNQ */
/*     XLDOT      O   Time rate of change of XL */
/*     XNDOT      O   Time rate of change of XN */
/*     XNDDT      O   Time rate of change of XNDOT */

/* $ Detailed_Input */

/*     ISYNFL      is the flag used to indicate the need for resonance */
/*                 calculations. */

/*     DG          is the parameter array replacing the Dxxxx values. */

/*     DEL         is the parameter array replacing DEL1, DEL2 and DEL3. */

/*     XNI         is an intermediate linear term passed from the main */
/*                 term for the calculation of XLDOT = XNI  +  XFACT */

/*     OMEGAO      is the original value for the argument of perigee. */

/*     ATIME       is an intermediate time term passed from the main */
/*                 routine used to calculate the time dependent */
/*                 argument of perigee term XOMI */

/*     OMGDOT      is the time derivative of the argument of the perigee. */

/*     XLI         is an intermediate angular term */

/*     XFACT       is the value BFACT - XNQ calculated in ZZDPINIT */

/* $ Detailed_Output */

/*     XLDOT       time derivative of the XL term. */

/*     XNDOT       time derivative of the XN term. */

/*     XNDDT       second time derivative of XN, time derivative of the */
/*                 time derivative. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     None. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This subroutine was constructed from a section of code in ZZDPSEC */
/*     in the original Spacetrack 3 report.  The code block was called */
/*     using a set of conditional GO TO's.  The block has been written as */
/*     this subroutine to improve clarity and maintainability and to */
/*     conform to the NAIF style standard. */

/* $ Examples */

/*     None needed. */

/* $ Restrictions */

/*     1)  This routine should be called only by ZZDPSEC as part of the */
/*     DPSPCE subroutine package.  It has no other use. */

/* $ Literature_References */

/*     Spacetrack 3 report. */

/* $ Author_and_Institution */

/*     E.D. Wright      (JPL) */

/* $ Version */

/* -   SPICELIB Version 1.1.1, 03-OCT-2021 (NJB) */

/*       Reordered header sections. Corrected typo in comments. */

/* -   SPICELIB Version 1.1.0, 24-MAR-1999 (EDW) */

/*       Correction made to format of Version descriptions. */
/*       Name of variable OMGDT changed to OMGDOT to be consistent */
/*       with name usage in other deep space two line elements */
/*       routines. */

/* -   SPICELIB Version 1.0.0, MAY-19-1997 (EDW) */


/* -& */
/* $ Index_Entries */

/*     perturbed dot terms */

/* -& */

/*     Local variables. */


/*     Calculate the dot terms with respect to the state of the */
/*     resonance flag. */

    if (*isynfl == 0) {

/*        Resonance flag set. */

	xomi = *omegao + *omgdot * *atime;
	x2omi = xomi + xomi;
	x2li = *xli + *xli;
	*xndot = dg[0] * sin(x2omi + *xli - 5.7686396) + dg[1] * sin(*xli - 
		5.7686396) + dg[2] * sin(xomi + *xli - .95240898) + dg[3] * 
		sin(-xomi + *xli - .95240898) + dg[4] * sin(x2omi + x2li - 
		1.8014998) + dg[5] * sin(x2li - 1.8014998) + dg[6] * sin(xomi 
		+ *xli - 1.050833) + dg[7] * sin(-xomi + *xli - 1.050833) + 
		dg[8] * sin(xomi + x2li - 4.4108898) + dg[9] * sin(-xomi + 
		x2li - 4.4108898);
	*xnddt = dg[0] * cos(x2omi + *xli - 5.7686396) + dg[1] * cos(*xli - 
		5.7686396) + dg[2] * cos(xomi + *xli - .95240898) + dg[3] * 
		cos(-xomi + *xli - .95240898) + dg[6] * cos(xomi + *xli - 
		1.050833) + dg[7] * cos(-xomi + *xli - 1.050833) + (dg[4] * 
		cos(x2omi + x2li - 1.8014998) + dg[5] * cos(x2li - 1.8014998) 
		+ dg[8] * cos(xomi + x2li - 4.4108898) + dg[9] * cos(xomi + 
		x2li - 4.4108898)) * 2.;
    } else {

/*        Resonance flag not set */

	*xndot = del[0] * sin(*xli - .13130908) + del[1] * sin((*xli - 
		2.8843198) * 2.) + del[2] * sin((*xli - .37448087) * 3.);
	*xnddt = del[0] * cos(*xli - .13130908) + del[1] * 2. * cos((*xli - 
		2.8843198) * 2.) + del[2] * 3. * cos((*xli - .37448087) * 3.);
    }
    *xldot = *xni + *xfact;
    *xnddt *= *xldot;

/*     Hi!  What are you doing way down here?  Did you bring pizza? */

    return 0;
} /* zzsecprt_ */

