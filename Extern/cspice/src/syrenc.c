/* syrenc.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__0 = 0;
static integer c__1 = 1;

/* $Procedure SYRENC ( Rename an existing symbol ) */
/* Subroutine */ int syrenc_(char *old, char *new__, char *tabsym, integer *
	tabptr, char *tabval, ftnlen old_len, ftnlen new_len, ftnlen 
	tabsym_len, ftnlen tabval_len)
{
    /* System generated locals */
    integer i__1;

    /* Builtin functions */
    integer s_cmp(char *, char *, ftnlen, ftnlen);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    integer nsym;
    extern integer cardc_(char *, ftnlen);
    extern /* Subroutine */ int chkin_(char *, ftnlen), errch_(char *, char *,
	     ftnlen, ftnlen);
    extern integer sumai_(integer *, integer *), bsrchc_(char *, integer *, 
	    char *, ftnlen, ftnlen);
    integer olddim, oldloc;
    extern /* Subroutine */ int swapac_(integer *, integer *, integer *, 
	    integer *, char *, ftnlen);
    integer oldval;
    extern /* Subroutine */ int sydelc_(char *, char *, integer *, char *, 
	    ftnlen, ftnlen, ftnlen), swapai_(integer *, integer *, integer *, 
	    integer *, integer *);
    extern integer lstlec_(char *, integer *, char *, ftnlen, ftnlen);
    integer newloc;
    extern /* Subroutine */ int sigerr_(char *, ftnlen);
    integer newval;
    extern /* Subroutine */ int chkout_(char *, ftnlen), setmsg_(char *, 
	    ftnlen);
    extern logical return_(void);

/* $ Abstract */

/*     Rename an existing symbol in a character symbol table. */

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
/*     OLD        I   Name of the symbol to be renamed. */
/*     NEW        I   New name of the symbol. */
/*     TABSYM, */
/*     TABPTR, */
/*     TABVAL    I-O  Components of the symbol table. */

/* $ Detailed_Input */

/*     OLD      is the name of the symbol to be renamed. If OLD is */
/*              not in the symbol table, the tables are not modified. */

/*     NEW      is the new name of the symbol. If the symbol NEW */
/*              already exists in the symbol table, it is deleted. */
/*              OLD is then renamed to NEW. */

/*     TABSYM, */
/*     TABPTR, */
/*     TABVAL   are components of the character symbol table. */

/* $ Detailed_Output */

/*     TABSYM, */
/*     TABPTR, */
/*     TABVAL   are components of the character symbol table. */
/*              The values previously associated with OLD are now */
/*              associated with NEW. If OLD is not in the symbol */
/*              table, the symbol tables are not modified. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the symbol OLD is not in the symbol table, the error */
/*         SPICE(NOSUCHSYMBOL) is signaled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     None. */

/* $ Examples */

/*     The contents of the symbol table are: */

/*        BOHR      -->   HYDROGEN ATOM */
/*        EINSTEIN  -->   SPECIAL RELATIVITY */
/*                        PHOTOELECTRIC EFFECT */
/*                        BROWNIAN MOTION */
/*        FERMI     -->   NUCLEAR FISSION */
/*        HAHN      -->   NUCLEAR FISSION */
/*        PAULI     -->   EXCLUSION PRINCIPLE */
/*                        NEUTRINO */

/*     The call, */

/*     CALL SYRENC ( 'FERMI', 'STRASSMAN', TABSYM, TABPTR, TABVAL ) */

/*     modifies the contents of the symbol table to be: */

/*        BOHR      -->   HYDROGEN ATOM */
/*        EINSTEIN  -->   SPECIAL RELATIVITY */
/*                        PHOTOELECTRIC EFFECT */
/*                        BROWNIAN MOTION */
/*        HAHN      -->   NUCLEAR FISSION */
/*        PAULI     -->   EXCLUSION PRINCIPLE */
/*                        NEUTRINO */
/*        STRASSMAN -->   NUCLEAR FISSION */


/*     The next call, */

/*     CALL SYRENC ( 'HAHN', 'STRASSMAN', TABSYM, TABPTR, TABVAL ) */

/*     modifies the contents of the symbol table to be: */

/*        BOHR      -->   HYDROGEN ATOM */
/*        EINSTEIN  -->   SPECIAL RELATIVITY */
/*                        PHOTOELECTRIC EFFECT */
/*                        BROWNIAN MOTION */
/*        PAULI     -->   EXCLUSION PRINCIPLE */
/*                        NEUTRINO */
/*        HAHN      -->   NUCLEAR FISSION */

/*     Note that the symbol "STRASSMAN" was deleted from the table, */
/*     and the symbol "HAHN" was then renamed to "STRASSMAN". If the */
/*     new symbol exists, it is deleted from the table before its name */
/*     is given to another symbol. */


/*     The next call, */

/*     CALL SYRENC ( 'FERMI', 'HAHN', TABSYM, TABPTR, TABVAL ) */

/*     does not modify the contents of the symbol table. It signals */
/*     the error SPICE(NOSUCHSYMBOL) because the symbol "FERMI" does */
/*     not exist in the symbol table. */

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

/* -    SPICELIB Version 1.1.0, 03-JUN-2021 (JDR) */

/*        Added IMPLICT NONE statement. */

/*        Edited the header to comply with NAIF standard. Fixed I/O type */
/*        of arguments TABSYM, TABPTR and TABVAL in $Brief_I/O table. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU) (HAN) */

/* -& */
/* $ Index_Entries */

/*     rename an existing symbol */

/* -& */
/* $ Revisions */

/* -    Beta Version 2.0.0, 28-DEC-1989 (HAN) */

/*        Changed the call to SYDELD to a call to SYDELC. The variable */
/*        TABVAL of type character was being passed to a dummy argument */
/*        of type double precision. */

/* -    Beta Version 1.1.0, 17-FEB-1989 (NJB) */

/*        Declaration of the unused function SIZEC removed. */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("SYRENC", (ftnlen)6);
    }

/*     Where was the old symbol? */

    nsym = cardc_(tabsym, tabsym_len);
    oldloc = bsrchc_(old, &nsym, tabsym + tabsym_len * 6, old_len, tabsym_len)
	    ;

/*     An overflow is simply not possible here. The only thing that can */
/*     go wrong is that the old symbol does not exist. */

    if (oldloc == 0) {
	setmsg_("SYRENC: The symbol # is not in the symbol table.", (ftnlen)
		48);
	errch_("#", old, (ftnlen)1, old_len);
	sigerr_("SPICE(NOSUCHSYMBOL)", (ftnlen)19);

/*     Are these the same symbol? */

    } else if (s_cmp(new__, old, new_len, old_len) != 0) {

/*        If the new symbol already exists, delete it. */

	sydelc_(new__, tabsym, tabptr, tabval, new_len, tabsym_len, 
		tabval_len);
	nsym = cardc_(tabsym, tabsym_len);
	oldloc = bsrchc_(old, &nsym, tabsym + tabsym_len * 6, old_len, 
		tabsym_len);

/*        Swap N elements at the old location with zero elements */
/*        at the new location. */

	newloc = lstlec_(new__, &nsym, tabsym + tabsym_len * 6, new_len, 
		tabsym_len) + 1;
	i__1 = oldloc - 1;
	oldval = sumai_(&tabptr[6], &i__1) + 1;
	i__1 = newloc - 1;
	newval = sumai_(&tabptr[6], &i__1) + 1;
	olddim = tabptr[oldloc + 5];
	swapac_(&olddim, &oldval, &c__0, &newval, tabval + tabval_len * 6, 
		tabval_len);

/*        Move the name and dimension the same way. */

	swapac_(&c__1, &oldloc, &c__0, &newloc, tabsym + tabsym_len * 6, 
		tabsym_len);
	swapai_(&c__1, &oldloc, &c__0, &newloc, &tabptr[6]);
	if (oldloc < newloc) {
	    --newloc;
	}
	s_copy(tabsym + (newloc + 5) * tabsym_len, new__, tabsym_len, new_len)
		;
    }
    chkout_("SYRENC", (ftnlen)6);
    return 0;
} /* syrenc_ */

