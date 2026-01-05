/* syputc.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;

/* $Procedure SYPUTC ( Set the values associated with a symbol ) */
/* Subroutine */ int syputc_(char *name__, char *values, integer *n, char *
	tabsym, integer *tabptr, char *tabval, ftnlen name_len, ftnlen 
	values_len, ftnlen tabsym_len, ftnlen tabval_len)
{
    /* System generated locals */
    integer i__1;

    /* Builtin functions */
    integer s_cmp(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    integer nval, nptr, nsym;
    extern integer cardc_(char *, ftnlen), cardi_(integer *);
    extern /* Subroutine */ int chkin_(char *, ftnlen), errch_(char *, char *,
	     ftnlen, ftnlen);
    extern integer sizec_(char *, ftnlen), sumai_(integer *, integer *), 
	    sizei_(integer *);
    extern /* Subroutine */ int scardc_(integer *, char *, ftnlen), remlac_(
	    integer *, integer *, char *, integer *, ftnlen), scardi_(integer 
	    *, integer *), inslac_(char *, integer *, integer *, char *, 
	    integer *, ftnlen, ftnlen);
    integer dimval;
    extern /* Subroutine */ int inslai_(integer *, integer *, integer *, 
	    integer *, integer *);
    integer locval;
    extern integer lstlec_(char *, integer *, char *, ftnlen, ftnlen);
    extern /* Subroutine */ int sigerr_(char *, ftnlen);
    integer newval;
    extern /* Subroutine */ int chkout_(char *, ftnlen), setmsg_(char *, 
	    ftnlen);
    integer locsym;
    logical oldsym;
    extern logical return_(void);
    integer newsym;

/* $ Abstract */

/*     Set the values of a particular symbol in a character symbol table. */
/*     If the symbol already exists, the previous values associated with */
/*     it are removed, otherwise a new symbol is created. */

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
/*     NAME       I   Name of the symbol whose associated values are to */
/*                    be put into the symbol table. */
/*     VALUES     I   Values to be associated with the symbol NAME. */
/*     N          I   Number of values in VALUES. */
/*     TABSYM, */
/*     TABPTR, */
/*     TABVAL    I-O  Components of the symbol table. */

/* $ Detailed_Input */

/*     NAME     is the name of the symbol whose associated values are */
/*              to be set. */

/*              If NAME has values associated with it, they are removed, */
/*              and the elements of VALUES become the values associated */
/*              with NAME. If NAME is not in the symbol table, a new */
/*              symbol is created, provided there is room in the symbol */
/*              table. */

/*     VALUES   are the new values associated with the symbol NAME. */

/*     N        is the number of elements in the VALUES array. */

/*     TABSYM, */
/*     TABPTR, */
/*     TABVAL   are the components of a character symbol table. */

/* $ Detailed_Output */

/*     TABSYM, */
/*     TABPTR, */
/*     TABVAL   are the components of a character symbol table. */

/*              If NAME has values associated with it, they are removed, */
/*              and the elements of VALUES become the values associated */
/*              with NAME. If NAME is not in the symbol table, a new */
/*              symbol is created, provided there is room in the symbol */
/*              table. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the addition of a new symbol causes an overflow in the */
/*         name table, the error SPICE(NAMETABLEFULL) is signaled. */

/*     2)  If the addition of a new symbol causes an overflow in the */
/*         pointer table, the error SPICE(POINTERTABLEFULL) is signaled. */

/*     3)  If the addition of new values causes an overflow in the */
/*         value table, the error SPICE(VALUETABLEFULL) is signaled. */

/*     4)  If N < 1, the error SPICE(INVALIDARGUMENT) is signaled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This subroutine is like SYSETC, but SYPUTC allows **several** */
/*     values to be associated with a symbol. */

/*     If NAME has values associated with it, they are removed, and */
/*     the elements of VALUES become the values associated with NAME. */
/*     If NAME is not in the symbol table, a new symbol is created, */
/*     provided there is room in the symbol table. */

/* $ Examples */

/*     The contents of the symbol table are: */

/*        BOHR      -->   HYDROGEN ATOM */
/*        FERMI     -->   NUCLEAR FISSION */
/*        PAULI     -->   NEUTRINO */

/*     If VALUES contains the elements, */

/*          SPECIAL RELATIVITY */
/*          PHOTOELECTRIC EFFECT */
/*          BROWNIAN MOTION */

/*     the call */

/*        CALL SYPUTC ( 'EINSTEIN', VALUES, 3, TABSYM, TABPTR, TABVAL ) */

/*     modifies the contents of the symbol table to be: */

/*        BOHR      -->   HYDROGEN ATOM */
/*        ENISTEIN  -->   SPECIAL RELATIVITY */
/*                        PHOTOELECTRIC EFFECT */
/*                        BROWNIAN MOTION */
/*        FERMI     -->   NUCLEAR FISSION */
/*        PAULI     -->   NEUTRINO */

/*     The call, */

/*        CALL SYPUTC ( 'PAULI', VALUES, 3, TABSYM, TABPTR, TABVAL ) */

/*     modifies the contents of the symbol table to be: */

/*        BOHR      -->   HYDROGEN ATOM */
/*        ENISTEIN  -->   SPECIAL RELATIVITY */
/*                        PHOTOELECTRIC EFFECT */
/*                        BROWNIAN MOTION */
/*        FERMI     -->   NUCLEAR FISSION */
/*        PAULI     -->   SPECIAL RELATIVITY */
/*                        PHOTOELECTRIC EFFECT */
/*                        BROWNIAN MOTION */

/*     Note that the previous values associated with PAULI have been */
/*     replaced by the values in VALUES. */

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

/*        Edited the header to comply with NAIF standard. Removed */
/*        unnecessary $Revisions section. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU) (HAN) */

/* -& */
/* $ Index_Entries */

/*     set the values associated with a symbol */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("SYPUTC", (ftnlen)6);
    }

/*     Check to see if the number of values is a valid quantity. */

    if (*n < 1) {
	setmsg_("SYPUTC: The dimension of the values array isless than one.", 
		(ftnlen)58);
	sigerr_("SPICE(INVALIDARGUMENT)", (ftnlen)22);
	chkout_("SYPUTC", (ftnlen)6);
	return 0;
    }

/*     How many symbols to start with? */

    nsym = cardc_(tabsym, tabsym_len);
    nptr = cardi_(tabptr);
    nval = cardc_(tabval, tabval_len);

/*     Where does this symbol belong? is it already in the table? */

    locsym = lstlec_(name__, &nsym, tabsym + tabsym_len * 6, name_len, 
	    tabsym_len);
    oldsym = locsym != 0 && s_cmp(tabsym + (locsym + 5) * tabsym_len, name__, 
	    tabsym_len, name_len) == 0;

/*     If the new symbol already exists, we need to know its dimension */
/*     to check for overflow. */

    if (oldsym) {
	i__1 = locsym - 1;
	locval = sumai_(&tabptr[6], &i__1) + 1;
	dimval = tabptr[locsym + 5];
	newsym = 0;
    } else {
	locval = sumai_(&tabptr[6], &locsym) + 1;
	dimval = 0;
	newsym = 1;
    }
    newval = *n - dimval;

/*     Can we do this without overflow? */

    if (nsym + newsym > sizec_(tabsym, tabsym_len)) {
	setmsg_("SYPUTC: Addition of the new symbol # causes an overflow in "
		"the name table.", (ftnlen)74);
	errch_("#", name__, (ftnlen)1, name_len);
	sigerr_("SPICE(NAMETABLEFULL)", (ftnlen)20);
    } else if (nptr + newsym > sizei_(tabptr)) {
	setmsg_("SYPUTC: Addition of the new symbol # causes an overflow in "
		"the pointer table.", (ftnlen)77);
	errch_("#", name__, (ftnlen)1, name_len);
	sigerr_("SPICE(POINTERTABLEFULL)", (ftnlen)23);
    } else if (nval + newval > sizec_(tabval, tabval_len)) {
	setmsg_("SYPUTC: Addition of the new symbol # causes an overflow in "
		"the value table.", (ftnlen)75);
	errch_("#", name__, (ftnlen)1, name_len);
	sigerr_("SPICE(VALUETABLEFULL)", (ftnlen)21);

/*     Looks like we can. */

    } else {

/*        If the symbol exists, remove the current contents and */
/*        change the dimension. Otherwise add the new name and */
/*        dimension to the name and pointer tables. */

	if (dimval > 0) {
	    remlac_(&dimval, &locval, tabval + tabval_len * 6, &nval, 
		    tabval_len);
	    scardc_(&nval, tabval, tabval_len);
	    tabptr[locsym + 5] = *n;
	} else {
	    i__1 = locsym + 1;
	    inslac_(name__, &c__1, &i__1, tabsym + tabsym_len * 6, &nsym, 
		    name_len, tabsym_len);
	    scardc_(&nsym, tabsym, tabsym_len);
	    i__1 = locsym + 1;
	    inslai_(n, &c__1, &i__1, &tabptr[6], &nptr);
	    scardi_(&nptr, tabptr);
	}

/*        In either case, insert the values from the input array into */
/*        the value table. */

	inslac_(values, n, &locval, tabval + tabval_len * 6, &nval, 
		values_len, tabval_len);
	scardc_(&nval, tabval, tabval_len);
    }
    chkout_("SYPUTC", (ftnlen)6);
    return 0;
} /* syputc_ */

