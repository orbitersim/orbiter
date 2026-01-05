/* sydelc.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;

/* $Procedure SYDELC ( Delete a symbol from the symbol table ) */
/* Subroutine */ int sydelc_(char *name__, char *tabsym, integer *tabptr, 
	char *tabval, ftnlen name_len, ftnlen tabsym_len, ftnlen tabval_len)
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    integer nval, nptr, nsym;
    extern integer cardc_(char *, ftnlen), cardi_(integer *);
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    extern integer sumai_(integer *, integer *);
    extern /* Subroutine */ int scardc_(integer *, char *, ftnlen), remlac_(
	    integer *, integer *, char *, integer *, ftnlen);
    extern integer bsrchc_(char *, integer *, char *, ftnlen, ftnlen);
    extern /* Subroutine */ int scardi_(integer *, integer *), remlai_(
	    integer *, integer *, integer *, integer *);
    integer dimval, locval;
    extern /* Subroutine */ int chkout_(char *, ftnlen);
    integer locsym;
    extern logical return_(void);

/* $ Abstract */

/*     Delete a symbol from a character symbol table. The symbol and its */
/*     associated values are deleted. */

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
/*     NAME       I   Name of the symbol to be deleted. */
/*     TABSYM, */
/*     TABPTR, */
/*     TABVAL    I-O  Components of the symbol table. */

/* $ Detailed_Input */

/*     NAME     is the name of the symbol to be deleted from the symbol */
/*              table. */

/*              If the symbol does not exist, the symbol table remains */
/*              unchanged. This subroutine is case sensitive. NAME must */
/*              the symbol exactly. */

/*     TABSYM, */
/*     TABPTR, */
/*     TABVAL   are the components of a character symbol table. */

/*              On input, the table may or may not contain the symbol */
/*              NAME. */

/* $ Detailed_Output */

/*     TABSYM, */
/*     TABPTR, */
/*     TABVAL   are the components of a character symbol table. */

/*              On output, the symbol table no longer contains the symbol */
/*              NAME or its associated values. If NAME is not a symbol, */
/*              the components of the symbol table remain unchanged. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     None. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     None. */

/* $ Examples */

/*     In the following example the subroutine SYDELC is used to delete */
/*     the symbol "BOHR" and its values from the symbol table. */

/*     The contents of the symbol table are: */

/*        BOHR      -->   HYDROGEN ATOM */
/*        EINSTEIN  -->   SPECIAL RELATIVITY */
/*                        PHOTOELECTRIC EFFECT */
/*                        BROWNIAN MOTION */
/*        FERMI     -->   NUCLEAR FISSION */

/*     The call */

/*        CALL SYDELC ( 'BOHR', TABSYM, TABPTR, TABVAL ) */

/*     deletes the symbol "BOHR" from the symbol table. The components */
/*     of the symbol table on output are: */

/*        EINSTEIN  -->   SPECIAL RELATIVITY */
/*                        PHOTOELECTRIC EFFECT */
/*                        BROWNIAN MOTION */
/*        FERMI     -->   NUCLEAR FISSION */

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

/*     delete a symbol from a symbol table */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("SYDELC", (ftnlen)6);
    }

/*     How many symbols to start with? */

    nsym = cardc_(tabsym, tabsym_len);
    nptr = cardi_(tabptr);
    nval = cardc_(tabval, tabval_len);

/*     Is this symbol even in the table? */

    locsym = bsrchc_(name__, &nsym, tabsym + tabsym_len * 6, name_len, 
	    tabsym_len);

/*     If it's not in the table, we're done. If it is, we can proceed */
/*     without fear of overflow. */

    if (locsym > 0) {
	i__1 = locsym - 1;
	locval = sumai_(&tabptr[6], &i__1) + 1;
	dimval = tabptr[locsym + 5];
	remlac_(&c__1, &locsym, tabsym + tabsym_len * 6, &nsym, tabsym_len);
	scardc_(&nsym, tabsym, tabsym_len);
	remlai_(&c__1, &locsym, &tabptr[6], &nptr);
	scardi_(&nptr, tabptr);
	remlac_(&dimval, &locval, tabval + tabval_len * 6, &nval, tabval_len);
	scardc_(&nval, tabval, tabval_len);
    }
    chkout_("SYDELC", (ftnlen)6);
    return 0;
} /* sydelc_ */

