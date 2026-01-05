/* sypshc.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;

/* $Procedure SYPSHC ( Push a value onto a particular symbol ) */
/* Subroutine */ int sypshc_(char *name__, char *value, char *tabsym, integer 
	*tabptr, char *tabval, ftnlen name_len, ftnlen value_len, ftnlen 
	tabsym_len, ftnlen tabval_len)
{
    /* System generated locals */
    integer i__1;

    /* Builtin functions */
    integer s_cmp(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    integer nval, nsym;
    extern integer cardc_(char *, ftnlen);
    extern /* Subroutine */ int chkin_(char *, ftnlen), errch_(char *, char *,
	     ftnlen, ftnlen);
    extern integer sizec_(char *, ftnlen), sumai_(integer *, integer *);
    extern /* Subroutine */ int scardc_(integer *, char *, ftnlen), inslac_(
	    char *, integer *, integer *, char *, integer *, ftnlen, ftnlen);
    integer locval;
    extern integer lstlec_(char *, integer *, char *, ftnlen, ftnlen);
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), setmsg_(char *, ftnlen);
    integer locsym;
    logical oldsym;
    extern /* Subroutine */ int sysetc_(char *, char *, char *, integer *, 
	    char *, ftnlen, ftnlen, ftnlen, ftnlen);
    extern logical return_(void);

/* $ Abstract */

/*     Push a value onto a particular symbol in a character symbol table. */
/*     The previous value(s) associated with the symbol is extended at */
/*     the front. A new symbol is created if necessary. */

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
/*     NAME       I   Name of the symbol onto which the value is to be */
/*                    pushed. */
/*     VALUE      I   Value that is to be pushed onto the symbol NAME. */
/*     TABSYM, */
/*     TABPTR, */
/*     TABVAL    I-O  Components of the symbol table. */

/* $ Detailed_Input */

/*     NAME     is the name of the symbol onto which the value is to */
/*              be pushed. */

/*              If NAME is not in the symbol table, a new symbol is */
/*              created. */

/*     TABSYM, */
/*     TABPTR, */
/*     TABVAL   are the components of a character symbol table. */

/* $ Detailed_Output */

/*     TABSYM, */
/*     TABPTR, */
/*     TABVAL   are the components of a character symbol table. */

/*              The value is added to the symbol table at the front of */
/*              the previous value(s) associated with the symbol NAME. If */
/*              NAME is not originally in the symbol table, a new symbol */
/*              is created. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the addition of the new value to the symbol table causes an */
/*         overflow in the value table, the error SPICE(VALUETABLEFULL) */
/*         is signaled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     If the symbol NAME is not in the symbol table, a new symbol */
/*     is created. */

/* $ Examples */

/*     The contents of the symbol table are: */

/*        BOHR      -->   HYDROGEN ATOM */
/*        EINSTEIN  -->   SPECIAL RELATIVITY */
/*                        PHOTOELECTRIC EFFECT */
/*                        BROWNIAN MOTION */
/*        FERMI     -->   NUCLEAR FISSION */
/*        PAULI     -->   EXCLUSION PRINCIPLE */

/*     The call, */

/*        CALL SYPSHC ( 'PAULI', 'NEUTRINO', TABSYM, TABPTR, TABVAL ) */

/*     modifies the contents of the symbol table to be: */

/*        BOHR      -->   HYDROGEN ATOM */
/*        EINSTEIN  -->   SPECIAL RELATIVITY */
/*                        PHOTOELECTRIC EFFECT */
/*                        BROWNIAN MOTION */
/*        FERMI     -->   NUCLEAR FISSION */
/*        PAULI     -->   NEUTRINO */
/*                        EXCLUSION PRINCIPLE */

/*     The next call, */

/*        CALL SYPSHC ( 'MILLIKAN', 'PHOTOELECTRIC EFFECT', */
/*       .               TABSYM,     TABPTR,                TABVAL ) */

/*     modifies the contents of the symbol table to be: */

/*        BOHR      -->   HYDROGEN ATOM */
/*        EINSTEIN  -->   SPECIAL RELATIVITY */
/*                        PHOTOELECTRIC EFFECT */
/*                        BROWNIAN MOTION */
/*        FERMI     -->   NUCLEAR FISSION */
/*        MILLIKAN  -->   PHOTOELECTRIC EFFECT */
/*        PAULI     -->   NEUTRINO */
/*                        EXCLUSION PRINCIPLE */

/*     Note that a new symbol "MILLIKAN" was created by the last call. */

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

/*     push a value onto a particular symbol */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("SYPSHC", (ftnlen)6);
    }

/*     How many symbols to start with? */

    nsym = cardc_(tabsym, tabsym_len);
    nval = cardc_(tabval, tabval_len);

/*     Where does this symbol belong? Is it already in the table? */

    locsym = lstlec_(name__, &nsym, tabsym + tabsym_len * 6, name_len, 
	    tabsym_len);
    oldsym = locsym != 0 && s_cmp(tabsym + (locsym + 5) * tabsym_len, name__, 
	    tabsym_len, name_len) == 0;

/*     If it's not already in the table, use SET to create a brand new */
/*     symbol. */

    if (! oldsym) {
	sysetc_(name__, value, tabsym, tabptr, tabval, name_len, value_len, 
		tabsym_len, tabval_len);

/*     If it is in the table, we can't proceed unless we know that we */
/*     have enough room for one extra addition in the value table. */

    } else if (nval >= sizec_(tabval, tabval_len)) {
	setmsg_("SYPSHC: The addition of the value $ to the symbol # causes "
		"an overflow in the value table.", (ftnlen)90);
	errch_("$", value, (ftnlen)1, value_len);
	errch_("#", name__, (ftnlen)1, name_len);
	sigerr_("SPICE(VALUETABLEFULL)", (ftnlen)21);

/*     If there's room, add the new value to the value table. Add one */
/*     to the dimension, and put the value in the right place. */

    } else {
	i__1 = locsym - 1;
	locval = sumai_(&tabptr[6], &i__1) + 1;
	inslac_(value, &c__1, &locval, tabval + tabval_len * 6, &nval, 
		value_len, tabval_len);
	scardc_(&nval, tabval, tabval_len);
	++tabptr[locsym + 5];
    }
    chkout_("SYPSHC", (ftnlen)6);
    return 0;
} /* sypshc_ */

