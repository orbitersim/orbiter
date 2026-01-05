/* syenqi.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;

/* $Procedure SYENQI ( Enqueue a value onto a symbol ) */
/* Subroutine */ int syenqi_(char *name__, integer *value, char *tabsym, 
	integer *tabptr, integer *tabval, ftnlen name_len, ftnlen tabsym_len)
{
    /* Builtin functions */
    integer s_cmp(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    integer nval, nsym;
    extern integer cardc_(char *, ftnlen), cardi_(integer *);
    extern /* Subroutine */ int chkin_(char *, ftnlen), errch_(char *, char *,
	     ftnlen, ftnlen);
    extern integer sumai_(integer *, integer *), sizei_(integer *);
    extern /* Subroutine */ int scardi_(integer *, integer *), inslai_(
	    integer *, integer *, integer *, integer *, integer *);
    integer locval;
    extern integer lstlec_(char *, integer *, char *, ftnlen, ftnlen);
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), setmsg_(char *, ftnlen), errint_(char *, integer *, 
	    ftnlen);
    integer locsym;
    logical oldsym;
    extern logical return_(void);
    extern /* Subroutine */ int syseti_(char *, integer *, char *, integer *, 
	    integer *, ftnlen, ftnlen);

/* $ Abstract */

/*     Enqueue a value onto a particular symbol in an integer */
/*     symbol table. If the symbol is not in the table, a new one */
/*     is created. */

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
/*     NAME       I   Name of the symbol onto which the value is */
/*                    enqueued. */
/*     VALUE      I   Value to be enqueued. */
/*     TABSYM, */
/*     TABPTR, */
/*     TABVAL    I-O  Components of the symbol table. */

/* $ Detailed_Input */

/*     NAME     is the name of the symbol onto which the value is to */
/*              be enqueued. If NAME is not in the symbol table, a new */
/*              symbol having the value VALUE is created. */

/*     VALUE    is the value to be enqueued onto the symbol, NAME. */
/*              The value is inserted in the value table after the */
/*              last value associated with the symbol. */

/*     TABSYM, */
/*     TABPTR, */
/*     TABVAL   are the components of an integer symbol table. */
/*              The symbol NAME may or may not be in the symbol */
/*              table. */

/* $ Detailed_Output */

/*     TABSYM, */
/*     TABPTR, */
/*     TABVAL   are the components of an integer symbol table. */

/*              On output, the value table contains the new value in */
/*              addition to the old values associated with the symbol */
/*              NAME. The pointer table is updated to reflect the change */
/*              in the dimension of the symbol. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the addition of the new value to the symbol table causes an */
/*         overflow in the value table, the error SPICE(VALUETABLEFULL) */
/*         is signaled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     If the symbol NAME is not in the symbol table, a new symbol is */
/*     created which has the value VALUE. */

/* $ Examples */

/*     The contents of the symbol table are: */

/*        books   -->   5 */
/*        erasers -->   6 */
/*        pencils -->  12 */
/*        pens    -->  10 */
/*                     12 */
/*                     24 */

/*     The call, */

/*        CALL SYENQI ( 'books', 12, TABSYM, TABPTR, TABVAL ) */

/*     produces the symbol table: */

/*        books   -->   5 */
/*                     12 */
/*        erasers -->   6 */
/*        pencils -->  12 */
/*        pens    -->  10 */
/*                     12 */
/*                     24 */

/*     The next call, */

/*        CALL SYENQI ( 'desks', 23, TABSYM, TABPTR, TABVAL ) */

/*     then produces the symbol table: */

/*        books   -->   5 */
/*                     12 */
/*        desks   -->  23 */
/*        erasers -->   6 */
/*        pencils -->  12 */
/*        pens    -->  10 */
/*                     12 */
/*                     24 */

/*     Notice that the symbol "desks" was created by the last call. */

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

/*     enqueue a value onto a symbol */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("SYENQI", (ftnlen)6);
    }

/*     How many symbols to start with? */

    nsym = cardc_(tabsym, tabsym_len);
    nval = cardi_(tabval);

/*     Where does this symbol belong? Is it already in the table? */

    locsym = lstlec_(name__, &nsym, tabsym + tabsym_len * 6, name_len, 
	    tabsym_len);
    oldsym = locsym != 0 && s_cmp(tabsym + (locsym + 5) * tabsym_len, name__, 
	    tabsym_len, name_len) == 0;

/*     If it's not already in the table, use SET to create a brand new */
/*     symbol. */

    if (! oldsym) {
	syseti_(name__, value, tabsym, tabptr, tabval, name_len, tabsym_len);

/*     If it is in the table, we can't proceed unless we know that we */
/*     have enough room for one extra addition in the value table. */

    } else if (nval >= sizei_(tabval)) {
	setmsg_("SYENQI: The addition of the value $ to the symbol # causes "
		"an overflow in the value table.", (ftnlen)90);
	errint_("$", value, (ftnlen)1);
	errch_("#", name__, (ftnlen)1, name_len);
	sigerr_("SPICE(VALUETABLEFULL)", (ftnlen)21);

/*     If there's room, add the new value to the value table. Add one */
/*     to the dimension, and put the value in the right place. */

    } else {
	locval = sumai_(&tabptr[6], &locsym) + 1;
	inslai_(value, &c__1, &locval, &tabval[6], &nval);
	scardi_(&nval, tabval);
	++tabptr[locsym + 5];
    }
    chkout_("SYENQI", (ftnlen)6);
    return 0;
} /* syenqi_ */

