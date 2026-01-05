/* sydupi.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;

/* $Procedure SYDUPI ( Create a duplicate of a symbol ) */
/* Subroutine */ int sydupi_(char *name__, char *copy, char *tabsym, integer *
	tabptr, integer *tabval, ftnlen name_len, ftnlen copy_len, ftnlen 
	tabsym_len)
{
    /* System generated locals */
    integer i__1;

    /* Builtin functions */
    integer s_cmp(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    integer nval, nptr, nsym, i__;
    extern integer cardc_(char *, ftnlen), cardi_(integer *);
    extern /* Subroutine */ int chkin_(char *, ftnlen), errch_(char *, char *,
	     ftnlen, ftnlen);
    extern integer sizec_(char *, ftnlen), sumai_(integer *, integer *), 
	    sizei_(integer *);
    extern /* Subroutine */ int scardc_(integer *, char *, ftnlen), scardi_(
	    integer *, integer *), remlai_(integer *, integer *, integer *, 
	    integer *), inslac_(char *, integer *, integer *, char *, integer 
	    *, ftnlen, ftnlen);
    integer dimval[2];
    extern /* Subroutine */ int inslai_(integer *, integer *, integer *, 
	    integer *, integer *);
    integer locval[2];
    extern integer lstlec_(char *, integer *, char *, ftnlen, ftnlen);
    extern /* Subroutine */ int sigerr_(char *, ftnlen);
    integer newval;
    extern /* Subroutine */ int chkout_(char *, ftnlen), setmsg_(char *, 
	    ftnlen);
    integer locsym[2];
    logical oldsym[2];
    extern logical return_(void);
    integer newsym;

/* $ Abstract */

/*     Create a duplicate of a symbol within an integer symbol table. */
/*     If a symbol with the new name already exists, its components */
/*     are replaced. */

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
/*     NAME       I   Name of the symbol to be duplicated. */
/*     COPY       I   Name of the new symbol. */
/*     TABSYM, */
/*     TABPTR, */
/*     TABVAL    I-O  Components of the symbol table. */

/* $ Detailed_Input */

/*     NAME     is the name of the symbol to be duplicated. */

/*              The components associated with NAME will be given to the */
/*              new symbol COPY. If NAME is not in the symbol table, */
/*              no duplicate symbol can be made. */

/*     COPY     is the name of the new symbol. If a symbol with the name */
/*              COPY already exists in the symbol table, its components */
/*              are replaced by the components of NAME. */

/*     TABSYM, */
/*     TABPTR, */
/*     TABVAL   are the components of an integer symbol table. */

/* $ Detailed_Output */

/*     TABSYM, */
/*     TABPTR, */
/*     TABVAL   are the components of an integer symbol table. */

/*              On output, the symbol table contains a new symbol COPY */
/*              whose components are the same as the components of NAME. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the symbol NAME is not in the symbol table, the error */
/*         SPICE(NOSUCHSYMBOL) is signaled. */

/*     2)  If duplication of the symbol causes an overflow in the */
/*         name table, the error SPICE(NAMETABLEFULL) is signaled. */

/*     3)  If duplication of the symbol causes an overflow in the */
/*         pointer table, the error SPICE(POINTERTABLEFULL) is signaled. */

/*     4)  If duplication of the symbol causes an overflow in the */
/*         value table, the error SPICE(VALUETABLEFULL) is signaled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     If the symbol NAME is not in the symbol table, no duplicate symbol */
/*     can be made. */

/*     If the symbol COPY is already in the symbol table, its components */
/*     are replaced by the components of NAME. */

/* $ Examples */

/*     The contents of the symbol table are: */

/*        books   -->   5 */
/*        erasers -->   6 */
/*        pencils -->  12 */
/*        pens    -->  10 */
/*                     12 */
/*                     24 */

/*     The code, */

/*        CALL SYDUPI ( 'books', 'tablets', TABSYM, TABPTR, TABVAL ) */

/*     produces the symbol table: */

/*        books   -->   5 */
/*        erasers -->   6 */
/*        pencils -->  12 */
/*        pens    -->  10 */
/*                     12 */
/*                     24 */
/*        tablets -->   5 */

/*     The code, */

/*        CALL SYDUPC ( 'desks', 'chairs', TABSYM, TABPTR, TABVAL ) */

/*     produces the error SPICE(NOSUCHSYMBOL) because the symbol */
/*     "desks" is not in the symbol table. */

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

/*     create a duplicate of a symbol */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     Standard SPICE error handling */

    if (return_()) {
	return 0;
    } else {
	chkin_("SYDUPI", (ftnlen)6);
    }

/*     How many symbols to start with? */

    nsym = cardc_(tabsym, tabsym_len);
    nptr = cardi_(tabptr);
    nval = cardi_(tabval);

/*     Where do these symbols belong? Are they already in the table? */

    locsym[0] = lstlec_(name__, &nsym, tabsym + tabsym_len * 6, name_len, 
	    tabsym_len);
    locsym[1] = lstlec_(copy, &nsym, tabsym + tabsym_len * 6, copy_len, 
	    tabsym_len);
    oldsym[0] = locsym[0] != 0 && s_cmp(tabsym + (locsym[0] + 5) * tabsym_len,
	     name__, tabsym_len, name_len) == 0;
    oldsym[1] = locsym[1] != 0 && s_cmp(tabsym + (locsym[1] + 5) * tabsym_len,
	     copy, tabsym_len, copy_len) == 0;

/*     If the original symbol is not in the table, we can't make a copy. */

    if (! oldsym[0]) {
	setmsg_("SYDUPI: The symbol to be duplicated, #, is not in the symbo"
		"l table.", (ftnlen)67);
	errch_("#", name__, (ftnlen)1, name_len);
	sigerr_("SPICE(NOSUCHSYMBOL)", (ftnlen)19);

/*     Otherwise, we need to know the dimension, to check for overflow. */

    } else {
	i__1 = locsym[0] - 1;
	locval[0] = sumai_(&tabptr[6], &i__1) + 1;
	dimval[0] = tabptr[locsym[0] + 5];

/*        If the new symbol already exists, we need to know its dimension */
/*        too, for the same reason. */

	if (oldsym[1]) {
	    i__1 = locsym[1] - 1;
	    locval[1] = sumai_(&tabptr[6], &i__1) + 1;
	    dimval[1] = tabptr[locsym[1] + 5];
	    newsym = 0;
	} else {
	    locval[1] = sumai_(&tabptr[6], &locsym[1]) + 1;
	    dimval[1] = 0;
	    newsym = 1;
	}
	newval = dimval[0] - dimval[1];

/*        Can we make a copy without overflow? */

	if (nsym + newsym > sizec_(tabsym, tabsym_len)) {
	    setmsg_("SYDUPI: Duplication of the symbol # causes an overflow "
		    "in the name table.", (ftnlen)73);
	    errch_("#", name__, (ftnlen)1, name_len);
	    sigerr_("SPICE(NAMETABLEFULL)", (ftnlen)20);
	} else if (nptr + newsym > sizei_(tabptr)) {
	    setmsg_("SYDUPI: Duplication of the symbol # causes an overflow "
		    "in the pointer table.", (ftnlen)76);
	    errch_("#", name__, (ftnlen)1, name_len);
	    sigerr_("SPICE(POINTERTABLEFULL)", (ftnlen)23);
	} else if (nval + newval > sizei_(tabval)) {
	    setmsg_("SYDUPI: Duplication of the symbol # causes an overflow "
		    "in the value table.", (ftnlen)74);
	    errch_("#", name__, (ftnlen)1, name_len);
	    sigerr_("SPICE(VALUETABLEFULL)", (ftnlen)21);

/*        Looks like we can. */

	} else {

/*           If the copy exists, remove the current contents and */
/*           change the dimension. Otherwise add the new name and */
/*           dimension to the name and pointer tables. */

	    if (dimval[1] > 0) {
		remlai_(&dimval[1], &locval[1], &tabval[6], &nval);
		scardi_(&nval, tabval);
		tabptr[locsym[1] + 5] = dimval[0];
		if (locval[0] > locval[1]) {
		    locval[0] -= dimval[1];
		}
	    } else {
		i__1 = locsym[1] + 1;
		inslac_(copy, &c__1, &i__1, tabsym + tabsym_len * 6, &nsym, 
			copy_len, tabsym_len);
		scardc_(&nsym, tabsym, tabsym_len);
		i__1 = locsym[1] + 1;
		inslai_(dimval, &c__1, &i__1, &tabptr[6], &nptr);
		scardi_(&nptr, tabptr);
	    }

/*           In either case, allocate space for the new symbol values, */
/*           and copy them in one by one. (INSLAx won't work if the */
/*           copy is earlier in the table than the original.) */

	    i__1 = locval[1];
	    for (i__ = nval; i__ >= i__1; --i__) {
		tabval[i__ + dimval[0] + 5] = tabval[i__ + 5];
	    }
	    if (locval[0] > locval[1]) {
		locval[0] += dimval[0];
	    }
	    i__1 = dimval[0] - 1;
	    for (i__ = 0; i__ <= i__1; ++i__) {
		tabval[locval[1] + i__ + 5] = tabval[locval[0] + i__ + 5];
	    }
	    i__1 = nval + dimval[0];
	    scardi_(&i__1, tabval);
	}
    }
    chkout_("SYDUPI", (ftnlen)6);
    return 0;
} /* sydupi_ */

