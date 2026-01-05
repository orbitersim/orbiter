/* sypopd.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;

/* $Procedure SYPOPD ( Pop a value from a particular symbol ) */
/* Subroutine */ int sypopd_(char *name__, char *tabsym, integer *tabptr, 
	doublereal *tabval, doublereal *value, logical *found, ftnlen 
	name_len, ftnlen tabsym_len)
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    integer nval, nptr, nsym;
    extern integer cardc_(char *, ftnlen), cardd_(doublereal *), cardi_(
	    integer *);
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    extern integer sumai_(integer *, integer *);
    extern /* Subroutine */ int scardc_(integer *, char *, ftnlen), scardd_(
	    integer *, doublereal *), remlac_(integer *, integer *, char *, 
	    integer *, ftnlen);
    extern integer bsrchc_(char *, integer *, char *, ftnlen, ftnlen);
    extern /* Subroutine */ int remlad_(integer *, integer *, doublereal *, 
	    integer *), scardi_(integer *, integer *), remlai_(integer *, 
	    integer *, integer *, integer *);
    integer locval;
    extern /* Subroutine */ int chkout_(char *, ftnlen);
    integer locsym;
    extern logical return_(void);

/* $ Abstract */

/*     Pop a value associated with a particular symbol in a double */
/*     precision symbol table. The first value associated with the */
/*     symbol is removed, and subsequent values are moved forward. */

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

/*     SYMBOLS */

/* $ Keywords */

/*     SYMBOLS */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     NAME       I   Name of the symbol whose associated value is to be */
/*                    popped. */
/*     TABSYM, */
/*     TABPTR, */
/*     TABVAL    I-O  Components of the symbol table. */
/*     VALUE      O   Value that was popped. */
/*     FOUND      O   .TRUE. if the symbol exists, .FALSE. otherwise. */

/* $ Detailed_Input */

/*     NAME     is the name of the symbol whose associated value is to */
/*              be popped. */

/*     TABSYM, */
/*     TABPTR, */
/*     TABVAL   are the components of a double precision symbol table. */

/* $ Detailed_Output */

/*     TABSYM, */
/*     TABPTR, */
/*     TABVAL   are the components of a double precision symbol table. */

/*              The value is removed from the symbol table, and the */
/*              remaining values associated with the symbol are moved */
/*              forward in the value table. If no other values are */
/*              associated with the symbol, the symbol is removed from */
/*              the symbol table. */

/*     VALUE    is the value that was popped. This value was the first */
/*              value in the symbol table that was associated with the */
/*              symbol NAME. */

/*     FOUND    is .TRUE. if NAME is in the symbol table, otherwise */
/*              it is .FALSE. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     None. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     If there are no remaining values associated with the symbol */
/*     after VALUE has been popped, the symbol is removed from the */
/*     symbol table. */

/* $ Examples */

/*     The contents of the symbol table are: */

/*        BODY4_POLE_RA -->    3.17681D2 */
/*                             1.08D-1 */
/*                             0.0D0 */
/*        DELTA_T_A     -->    3.2184D1 */
/*        K             -->    1.657D-3 */
/*        MEAN_ANOM     -->    6.239996D0 */
/*                             1.99096871D-7 */
/*        ORBIT_ECC     -->    1.671D-2 */

/*     The call, */

/*        CALL SYPOPD ( 'MEAN_ANOM', TABSYM, TABPTR, TABVAL, */
/*       .                                   VALUE,  FOUND   ) */

/*     modifies the contents of the symbol table to be: */

/*        BODY4_POLE_RA -->    3.17681D2 */
/*                             1.08D-1 */
/*                             0.0D0C */
/*        DELTA_T_A     -->    3.2184D1 */
/*        K             -->    1.657D-3 */
/*        MEAN_ANOM     -->    1.99096871D-7 */
/*        ORBIT_ECC     -->    1.671D-2 */

/*     FOUND is .TRUE., and VALUE is 6.239996D0. */


/*     The next call, */

/*        CALL SYPOPD ( 'K', TABSYM, TABPTR, TABVAL, VALUE, FOUND ) */

/*     modifies the contents of the symbol table to be: */

/*        BODY4_POLE_RA -->    3.17681D2 */
/*                             1.08D-1 */
/*                             0.0D0C */
/*        DELTA_T_A     -->    3.2184D1 */
/*        MEAN_ANOM     -->    1.99096871D-7 */
/*        ORBIT_ECC     -->    1.671D-2 */

/*      FOUND is .TRUE., and VALUE is  1.657D-3. Note that because */
/*      "K" had only one value associated with it, it was removed */
/*      from the symbol table. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     H.A. Neilan        (JPL) */
/*     W.L. Taber         (JPL) */
/*     I.M. Underwood     (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 08-APR-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU) (HAN) */

/* -& */
/* $ Index_Entries */

/*     pop a value from a particular symbol */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("SYPOPD", (ftnlen)6);
    }

/*     How many symbols to start with? */

    nsym = cardc_(tabsym, tabsym_len);
    nptr = cardi_(tabptr);
    nval = cardd_(tabval);

/*     Is this symbol even in the table? */

    locsym = bsrchc_(name__, &nsym, tabsym + tabsym_len * 6, name_len, 
	    tabsym_len);

/*     If it's not in the table, it's definitely a problem. */

    if (locsym == 0) {
	*found = FALSE_;

/*     If it is in the table, we can proceed without fear of overflow. */

    } else {
	*found = TRUE_;

/*        Begin by saving and removing the initial value for this */
/*        symbol from the value table. */

	i__1 = locsym - 1;
	locval = sumai_(&tabptr[6], &i__1) + 1;
	*value = tabval[locval + 5];
	remlad_(&c__1, &locval, &tabval[6], &nval);
	scardd_(&nval, tabval);

/*        If this was the sole value for the symbol, remove the */
/*        symbol from the name and pointer tables. Otherwise just */
/*        decrement the dimension. */

	if (tabptr[locsym + 5] == 1) {
	    remlac_(&c__1, &locsym, tabsym + tabsym_len * 6, &nsym, 
		    tabsym_len);
	    scardc_(&nsym, tabsym, tabsym_len);
	    remlai_(&c__1, &locsym, &tabptr[6], &nptr);
	    scardi_(&nptr, tabptr);
	} else {
	    --tabptr[locsym + 5];
	}
    }
    chkout_("SYPOPD", (ftnlen)6);
    return 0;
} /* sypopd_ */

