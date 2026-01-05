/* syselc.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure SYSELC ( Select a subset of the values of a symbol ) */
/* Subroutine */ int syselc_(char *name__, integer *begin, integer *end, char 
	*tabsym, integer *tabptr, char *tabval, char *values, logical *found, 
	ftnlen name_len, ftnlen tabsym_len, ftnlen tabval_len, ftnlen 
	values_len)
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    integer nsym;
    extern integer cardc_(char *, ftnlen);
    integer n;
    extern /* Subroutine */ int chkin_(char *, ftnlen), movec_(char *, 
	    integer *, char *, ftnlen, ftnlen);
    extern integer sumai_(integer *, integer *), bsrchc_(char *, integer *, 
	    char *, ftnlen, ftnlen);
    integer locval;
    extern /* Subroutine */ int chkout_(char *, ftnlen);
    integer locsym;
    extern logical return_(void);

/* $ Abstract */

/*     Select a subset of the values associated with a particular */
/*     symbol in a character symbol table. */

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
/*                    be returned. */
/*     BEGIN      I   Index of the first associated value to be returned. */
/*     END        I   Index of the last associated value to be returned. */
/*     TABSYM, */
/*     TABPTR, */
/*     TABVAL     I   Components of the symbol table. */
/*     VALUES     O   Subset of the values associated with the symbol */
/*                    NAME. */
/*     FOUND      O   .TRUE. if the subset of values exists. */

/* $ Detailed_Input */

/*     NAME     is the name of the symbol whose subset of associated */
/*              values to be returned. If NAME is not in the symbol */
/*              table, FOUND is .FALSE. */

/*     BEGIN    is the index of the first associated value to be */
/*              returned. If BEGIN is out of range (BEGIN < 1 or */
/*              BEGIN > END) FOUND is .FALSE. */

/*     END      is the index of the last associated value to be */
/*              returned. If END is out of range (END < 1 or */
/*              END > is greater than the dimension of NAME) */
/*              FOUND is .FALSE. */

/*     TABSYM, */
/*     TABPTR, */
/*     TABVAL   are components of the character symbol table. */

/* $ Detailed_Output */

/*     VALUES   is a subset of the values associated with the */
/*              symbol NAME. If the subset specified by BEGIN and */
/*              END exists, as many values as will fit in VALUES */
/*              are returned. If the subset does not exist, no */
/*              values are returned and FOUND is .FALSE. */

/*     FOUND    is .TRUE. if the subset of values is exists. */
/*              FOUND is .FALSE. if BEGIN < 1, BEGIN > END, END < 1, */
/*              END > the dimension of NAME, or NAME is not */
/*              in the symbol table. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If there is an issue while reading the components of a */
/*         character symbol table, an error is signaled by a routine in */
/*         the call tree of this routine. This normally indicates that */
/*         the character symbol table is corrupted. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     FOUND will be false if the bounds of the subset specified by */
/*     BEGIN and END are out of range. Values of BEGIN and END that */
/*     specify bounds out of range are BEGIN < 1, BEGIN > END, */
/*     END < 1, or END > the dimension of NAME. FOUND is also false */
/*     if the symbol NAME is not in the symbol table. */

/* $ Examples */

/*     The contents of the symbol table are: */

/*        BOHR      -->   HYDROGEN ATOM */
/*        EINSTEIN  -->   SPECIAL RELATIVITY */
/*                        PHOTOELECTRIC EFFECT */
/*                        BROWNIAN MOTION */
/*                        GENERAL RELATIVITY */
/*        FERMI     -->   NUCLEAR FISSION */
/*        PAULI     -->   EXCLUSION PRINCIPLE */
/*                        NEUTRINO */

/*     Let the dimension of the array VALUES be 4. */


/*     The ouput values of VALUES and FOUND for the input values of */
/*     NAME, BEGIN, and END are contained in this table: */

/*        NAME         BEGIN    END        VALUES               FOUND */
/*        -----------  -----    ---    ---------------------   ------- */
/*        EINSTEIN       2       3     PHOTOELECTRIC EFFECT     .TRUE. */
/*                                     BROWNIAN MOTION */

/*        EINSTEIN       1       4     SPECIAL RELATIVITY       .TRUE. */
/*                                     PHOTOELECTRIC EFFECT */
/*                                     BROWNIAN MOTION */
/*                                     GENERAL RELATIVITY */

/*        MAXWELL        1       5                             .FALSE. */

/*        PAULI          2       1                             .FALSE. */

/*        PAULI          1      -2                             .FALSE. */

/*        BOHR           1       5                             .FALSE. */
/*        ------------------------------------------------------------ */


/*     Note that FOUND is .FALSE. for examples 3 through 6 because: */

/*     -  In the 3rd example, the symbol 'MAXWELL' is not in the symbol */
/*        table. */

/*     -  In the 4th example, BEGIN > END. */

/*     -  In the 5th example, END < 0. */

/*     -  In the 6th example, END is greater than the dimension of the */
/*        symbol 'BOHR'. */

/* $ Restrictions */

/*     1)  This subroutine does not check to see if the output array */
/*         VALUES is large enough to hold the selected set of values. */
/*         The caller must provide the required space. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     H.A. Neilan        (JPL) */
/*     W.L. Taber         (JPL) */
/*     I.M. Underwood     (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 16-AUG-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. Moved entry */
/*        from $Exceptions to $Restrictions and added entry #1 in */
/*        $Exceptions. */

/* -    SPICELIB Version 1.0.2, 03-NOV-2005 (NJB) */

/*        Various header corrections were made. In particular, */
/*        the header no longer asserts that this routine will */
/*        "return as many values as will fit" in the output array */
/*        VALUES. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU) (HAN) */

/* -& */
/* $ Index_Entries */

/*     select a subset of the values of a symbol */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("SYSELC", (ftnlen)6);
    }

/*     How many symbols to start with? */

    nsym = cardc_(tabsym, tabsym_len);

/*     Is this symbol even in the table? */

    locsym = bsrchc_(name__, &nsym, tabsym + tabsym_len * 6, name_len, 
	    tabsym_len);

/*     If it's not in the table, it's definitely a problem. */

    if (locsym == 0) {
	*found = FALSE_;
    } else {

/*        We could still have a problem: do these components exist? */
/*        Does this request even make sense? */

	n = tabptr[locsym + 5];
	if (*begin >= 1 && *begin <= n && *end >= 1 && *end <= n && *begin <= 
		*end) {
	    *found = TRUE_;
	    i__1 = locsym - 1;
	    locval = sumai_(&tabptr[6], &i__1) + 1;
	    i__1 = *end - *begin + 1;
	    movec_(tabval + (locval + *begin + 4) * tabval_len, &i__1, values,
		     tabval_len, values_len);
	} else {
	    *found = FALSE_;
	}
    }
    chkout_("SYSELC", (ftnlen)6);
    return 0;
} /* syselc_ */

