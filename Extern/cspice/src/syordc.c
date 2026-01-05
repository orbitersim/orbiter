/* syordc.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure SYORDC ( Order the components of a single symbol ) */
/* Subroutine */ int syordc_(char *name__, char *tabsym, integer *tabptr, 
	char *tabval, ftnlen name_len, ftnlen tabsym_len, ftnlen tabval_len)
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    integer nsym;
    extern integer cardc_(char *, ftnlen);
    integer n;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    extern integer sumai_(integer *, integer *), bsrchc_(char *, integer *, 
	    char *, ftnlen, ftnlen);
    extern /* Subroutine */ int shellc_(integer *, char *, ftnlen);
    integer locval;
    extern /* Subroutine */ int chkout_(char *, ftnlen);
    integer locsym;
    extern logical return_(void);

/* $ Abstract */

/*     Order the components of a single symbol in a character symbol */
/*     table. The components are ordered according to the ASCII collating */
/*     sequence. */

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
/*     NAME       I   Name of the symbol whose components are to be */
/*                    ordered. */
/*     TABSYM, */
/*     TABPTR, */
/*     TABVAL    I-O  Components of the symbol table. */

/* $ Detailed_Input */

/*     NAME     is the name of the symbol whose components are to be */
/*              ordered. */

/*     TABSYM, */
/*     TABPTR, */
/*     TABVAL   are the components of a character symbol table. */

/* $ Detailed_Output */

/*     TABSYM, */
/*     TABPTR, */
/*     TABVAL   are the components of a character symbol table. */

/*              On output, the components of the symbol are sorted */
/*              according to ASCII collating sequence. If NAME is not in */
/*              the symbol table, the symbol table is not modified. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     None. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     If the symbol NAME is not in the symbol table, the symbol table */
/*     is not modified. */

/* $ Examples */

/*     The contents of the symbol table are: */

/*        BOHR      -->   HYDROGEN ATOM */
/*        EINSTEIN  -->   SPECIAL RELATIVITY */
/*                        PHOTOELECTRIC EFFECT */
/*                        BROWNIAN MOTION */
/*        FERMI     -->   NUCLEAR FISSION */

/*     The call, */

/*        CALL SYORDC ( 'EINSTEIN', TABSYM, TABPTR, TABVAL ) */

/*     modifies the contents of the symbol table to be: */

/*        BOHR      -->   HYDROGEN ATOM */
/*        EINSTEIN  -->   BROWNIAN MOTION */
/*                        PHOTOELECTRIC EFFECT */
/*                        SPECIAL RELATIVITY */
/*        FERMI     -->   NUCLEAR FISSIONC */


/*     Note that the call, */

/*        CALL SYORDC ( 'MAXWELL', TABSYM, TABPTR, TABVAL ) */

/*     will not modify the symbol table because the symbol "MAXWELL" is */
/*     not in the symbol table. */

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

/*     order the components of a single symbol */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("SYORDC", (ftnlen)6);
    }

/*     How many symbols? */

    nsym = cardc_(tabsym, tabsym_len);

/*     Is this symbol even in the table? */

    locsym = bsrchc_(name__, &nsym, tabsym + tabsym_len * 6, name_len, 
	    tabsym_len);

/*     If so, sort the components in place. */

    if (locsym > 0) {
	i__1 = locsym - 1;
	locval = sumai_(&tabptr[6], &i__1) + 1;
	n = tabptr[locsym + 5];
	shellc_(&tabptr[locsym + 5], tabval + (locval + 5) * tabval_len, 
		tabval_len);
    }
    chkout_("SYORDC", (ftnlen)6);
    return 0;
} /* syordc_ */

