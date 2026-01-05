/* sysetd.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;

/* $Procedure SYSETD ( Set the value associated with a symbol ) */
/* Subroutine */ int sysetd_(char *name__, doublereal *value, char *tabsym, 
	integer *tabptr, doublereal *tabval, ftnlen name_len, ftnlen 
	tabsym_len)
{
    /* System generated locals */
    integer i__1;

    /* Builtin functions */
    integer s_cmp(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    integer nval, nptr, nsym;
    extern integer cardc_(char *, ftnlen), cardd_(doublereal *), cardi_(
	    integer *);
    extern /* Subroutine */ int chkin_(char *, ftnlen), errch_(char *, char *,
	     ftnlen, ftnlen);
    extern integer sizec_(char *, ftnlen), sized_(doublereal *), sumai_(
	    integer *, integer *), sizei_(integer *);
    extern /* Subroutine */ int scardc_(integer *, char *, ftnlen), scardd_(
	    integer *, doublereal *), remlad_(integer *, integer *, 
	    doublereal *, integer *), scardi_(integer *, integer *), inslac_(
	    char *, integer *, integer *, char *, integer *, ftnlen, ftnlen), 
	    inslad_(doublereal *, integer *, integer *, doublereal *, integer 
	    *);
    integer dimval;
    extern /* Subroutine */ int inslai_(integer *, integer *, integer *, 
	    integer *, integer *);
    integer locval;
    extern integer lstlec_(char *, integer *, char *, ftnlen, ftnlen);
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), setmsg_(char *, ftnlen);
    integer locsym;
    logical oldsym;
    extern logical return_(void);

/* $ Abstract */

/*     Set the value of a particular symbol in a double precision symbol */
/*     table. If the symbol already exists, the previous values */
/*     associated with it are removed, otherwise a new symbol is created. */

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
/*                    set. */
/*     VALUE      I   Associated value of the symbol NAME. */
/*     TABSYM, */
/*     TABPTR, */
/*     TABVAL    I-O  Components of the symbol table. */

/* $ Detailed_Input */

/*     NAME     is the name of the symbol whose associated value is to */
/*              be set. */

/*              If NAME has values associated with it, they are removed, */
/*              and VALUE becomes the only value associated with NAME. If */
/*              NAME is not in the symbol table, a new symbol is created, */
/*              provided there is room in the symbol table. */

/*     VALUE    is the new value associated with the symbol NAME. */

/*     TABSYM, */
/*     TABPTR, */
/*     TABVAL   are the components of a double precision symbol table. */

/* $ Detailed_Output */

/*     TABSYM, */
/*     TABPTR, */
/*     TABVAL   are the components of a double precision symbol table. */

/*              If NAME has values associated with it, they are removed, */
/*              and VALUE becomes the only value associated with NAME. If */
/*              NAME is not in the symbol table, a new symbol is created, */
/*              provided there is room in the symbol table. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the addition of a new symbol causes an overflow in the */
/*         name table, the error SPICE(NAMETABLEFULL) is signaled. */

/*     2)  If the addition of a new symbol causes an overflow in the */
/*         pointer table, the error SPICE(POINTERTABLEFULL) is signaled. */

/*     3)  If the addition of a new symbol causes an overflow in the */
/*         value table, the error SPICE(VALUETABLEFULL) is signaled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     If NAME has values associated with it, they are removed, and VALUE */
/*     becomes the only value associated with NAME. If NAME is not in the */
/*     symbol table, a new symbol is created, provided there is room in */
/*     the symbol table. */

/* $ Examples */

/*     The contents of the symbol table are: */

/*        DELTA_T_A -->    3.2184D1 */
/*        K         -->    1.657D-3 */
/*        MEAN_ANOM -->    6.239996D0 */
/*                         1.99096871D-7 */
/*        ORBIT_ECC -->    1.671D-2 */

/*     The call, */

/*     CALL SYSETD ( 'ORBIT_ECC', 1.67125D-2, TABSYM, TABPTR, TABVAL ) */

/*     modifies the contents of the symbol table to be: */

/*        DELTA_T_A -->    3.2184D1 */
/*        K         -->    1.657D-3 */
/*        MEAN_ANOM -->    6.239996D0 */
/*                         1.99096871D-7 */
/*        ORBIT_ECC -->    1.67125D-2 */

/*     Note that the previous value associated with the symbol */
/*     "ORBIT_ECC" has been deleted, and now the value 1.67125D-2 */
/*     is associated with the symbol. */


/*     The next call, */

/*     CALL SYSETD ( 'EB', 1.671D-2, TABSYM, TABPTR, TABVAL ) */

/*     modifies the contents of the symbol table to be: */

/*        DELTA_T_A -->    3.2184D1 */
/*        EB        -->    1.671D-2 */
/*        K         -->    1.657D-3 */
/*        MEAN_ANOM -->    6.239996D0 */
/*                         1.99096871D-7 */
/*        ORBIT_ECC -->    1.67125D-2 */

/*     Note that the new symbol "EB" was created by the last call. */
/*     A new symbol is created only if there is room in the symbol */
/*     table. */

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

/* -    SPICELIB Version 1.1.0, 17-JUN-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU) (HAN) */

/* -& */
/* $ Index_Entries */

/*     set the value associated with a symbol */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("SYSETD", (ftnlen)6);
    }

/*     How many symbols to start with? */

    nsym = cardc_(tabsym, tabsym_len);
    nptr = cardi_(tabptr);
    nval = cardd_(tabval);

/*     Where does this symbol belong? Is it already in the table? */

    locsym = lstlec_(name__, &nsym, tabsym + tabsym_len * 6, name_len, 
	    tabsym_len);
    oldsym = locsym != 0 && s_cmp(tabsym + (locsym + 5) * tabsym_len, name__, 
	    tabsym_len, name_len) == 0;

/*     If it's already in the table, there's no chance of overflow. */
/*     Leave the name where it is. Remove all but one of the existing */
/*     values, replacing that with the new value. And set the dimension */
/*     to one. */

    if (oldsym) {
	i__1 = locsym - 1;
	locval = sumai_(&tabptr[6], &i__1) + 1;
	dimval = tabptr[locsym + 5];
	if (dimval > 1) {
	    i__1 = dimval - 1;
	    remlad_(&i__1, &locval, &tabval[6], &nval);
	    scardd_(&nval, tabval);
	}
	tabptr[locsym + 5] = 1;
	tabval[locval + 5] = *value;

/*     Otherwise, we can't proceed unless we know that we have enough */
/*     room for one extra addition in all three tables. */

    } else if (nsym >= sizec_(tabsym, tabsym_len)) {
	setmsg_("SYSETD: Addition of the new symbol # causes an overflow in "
		"the name table.", (ftnlen)74);
	errch_("#", name__, (ftnlen)1, name_len);
	sigerr_("SPICE(NAMETABLEFULL)", (ftnlen)20);
    } else if (nptr >= sizei_(tabptr)) {
	setmsg_("SYSETD: Addition of the new symbol # causes an overflow in "
		"the pointer table.", (ftnlen)77);
	errch_("#", name__, (ftnlen)1, name_len);
	sigerr_("SPICE(POINTERTABLEFULL)", (ftnlen)23);
    } else if (nval >= sized_(tabval)) {
	setmsg_("SYSETD: Addition of the new symbol # causes an overflow in "
		"the value table.", (ftnlen)75);
	errch_("#", name__, (ftnlen)1, name_len);
	sigerr_("SPICE(VALUETABLEFULL)", (ftnlen)21);

/*     If there's room, add the new name to the name table. Give the */
/*     symbol dimension one, and put the value in the right place. */

    } else {
	i__1 = locsym + 1;
	inslac_(name__, &c__1, &i__1, tabsym + tabsym_len * 6, &nsym, 
		name_len, tabsym_len);
	scardc_(&nsym, tabsym, tabsym_len);
	i__1 = locsym + 1;
	inslai_(&c__1, &c__1, &i__1, &tabptr[6], &nptr);
	scardi_(&nptr, tabptr);
	locval = sumai_(&tabptr[6], &locsym) + 1;
	inslad_(value, &c__1, &locval, &tabval[6], &nval);
	scardd_(&nval, tabval);
    }
    chkout_("SYSETD", (ftnlen)6);
    return 0;
} /* sysetd_ */

