/* zzgpnm.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure      ZZGPNM ( Get position of a name ) */
/* Subroutine */ int zzgpnm_(integer *namlst, integer *nmpool, char *names, 
	integer *datlst, integer *dppool, doublereal *dpvals, integer *chpool,
	 char *chvals, char *varnam, logical *found, integer *lookat, integer 
	*nameat, ftnlen names_len, ftnlen chvals_len, ftnlen varnam_len)
{
    /* Builtin functions */
    integer s_cmp(char *, char *, ftnlen, ftnlen);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    integer head, node, tail;
    logical full;
    extern /* Subroutine */ int chkin_(char *, ftnlen), lnkan_(integer *, 
	    integer *), lnkila_(integer *, integer *, integer *);
    extern integer lnknfn_(integer *);
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), setmsg_(char *, ftnlen);
    extern integer zzhash_(char *, ftnlen);
    extern logical return_(void);

/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines.  Users should not call this routine directly due */
/*     to the volatile nature of this routine. */

/*     Locate the node in the array NAMES where a variable is located */
/*     or will be inserted. */

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

/*     PRIVATE KERNEL */

/* $ Keywords */

/*     FILES */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     NAMLST    I/O  array of collision resolution list heads */
/*     NMPOOL    I/O  linked list pool of collision resolution lists */
/*     NAMES     I/O  array of names of kernel pool variables */
/*     DATLST    I/O  array of heads of lists of variable values */
/*     DPPOOL    I/O  linked list pool of pointer lists to d.p. values */
/*     DPVALS    I/O  array of d.p. kernel pool values */
/*     CHPOOL    I/O  linked list pool of pointer lists to string values */
/*     CHVALS    I/O  array of string kernel pool values */
/*     VARNAM     I   A name to find/put into the kernel pool name list. */
/*     FOUND      O   TRUE if VARNAM is already in the list of names */
/*     LOOKAT     O   The value ZZHASH(VARNAM). */
/*     NAMEAT     O   The location where VARNAM is to be located. */

/* $ Detailed_Input */

/*     NAMLST    this collection of arrays together with the hash */
/*     NMPOOL    function ZZHASH provide the mechanism for storing */
/*     NAMES     and retrieving kernel pool variables. */
/*     DATLST */
/*     DPPOOL    Given a potential variable name NAME the function */
/*     DPVALS    ZZHASH(NAME) gives the location in the array in */
/*     CHPOOL    NAMLST where one should begin looking for the */
/*     CHVALS    kernel pool variable NAME. */
/*               If NAMLST( ZZHASH(NAME) ) is zero, there is no kernel */
/*               pool variable corresponding to NAME.  If it is non-zero */
/*               then NAMLST is the head node of a linked list of names */
/*               that evaluate to the same integer under the function */
/*               ZZHASH.  Letting NODE = NAMLST( ZZHASH(NAME) ) check */
/*               NAMES(NODE) for equality with NAME.  If there is */
/*               no match find the next node ( NMPOOL(NEXT,NODE) ) until */
/*               a match occurs or all nodes of the list have been */
/*               examined.  To insert a new NAME allocate a node NEW from */
/*               the free list of NMPOOL and append it to the tail of the */
/*               list pointed to by NAMLST ( ZZHASH(NAME) ). */

/*               Once a node for NAME is located (call it NAMEAT) */
/*               the values for NAME can be found by examining */
/*               DATLST(NAMEAT).  If zero, no values have yet been */
/*               given to NAME.  If less than zero, -DATLST(NAMEAT) */
/*               is the head node of a list in CHPOOL that gives the */
/*               indexes of the values of NAME in CHVALS.  If greater */
/*               than zero, DATLST(NAMEAT) is the head node of a list */
/*               in DPPOOL that gives the indexes of the values of NAME */
/*               in DPVALS. */

/*     VARNAM    is the name of a variable that is either already present */
/*               or that should be placed in the kernel pool */
/* $ Detailed_Output */

/*     NAMLST     is the same structure as input but updated to */
/*     NMPOOL     include the new variable specified by VARNAM if */
/*     NAMES      it is a new name. */
/*     DATLST */
/*     DPPOOL */
/*     DPVALS */
/*     CHPOOL */
/*     CHVALS */

/*     FOUND      is TRUE if VARNAM was already present in the name list. */

/*     LOOKAT     is the location in NAMLST where the head of the */
/*                ZZHASH collision linked list is stored. */

/*     NAMEAT     is the location within the array NAMES where VARNAM */
/*                is located. */

/* $ Parameters */

/*     None. */

/* $ Files */

/*     None. */

/* $ Exceptions */

/*     1) If the NAMES array cannot accomodate any more kernel variable */
/*        names, the error 'SPICE(KERNELPOOLFULL)' is signalled. */

/* $ Particulars */

/*     This is a utility routine designed to assist the kernel pool */
/*     entry points PDPOOL, PCPOOL and PIPOOL.  It handles the task */
/*     of inserting a new variable name into the kernel pool name */
/*     structure and returns information on the location of that */
/*     name. */

/* $ Examples */

/*     See the entry points PDPOOL, PCPOOL or PIPOOL. */

/* $ Restrictions */

/*     None. */

/* $ Author_and_Institution */

/*     W.L. Taber      (JPL) */

/* $ Literature_References */

/*     None. */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 29-MAR-1999 (WLT) */


/* -& */

/*     SPICELIB Functions */


/*     Parameters */


/*     Local Variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }
    chkin_("ZZGPNM", (ftnlen)6);
    *nameat = 0;


/*     Locate this variable name in the name pool or insert it */
/*     if it isn't there.  The location will be NAMEAT and */
/*     we will use the variable FOUND to indicate whether or */
/*     not it was already present. */

    *lookat = zzhash_(varnam, varnam_len);
    node = namlst[*lookat - 1];
    full = lnknfn_(nmpool) <= 0;
    *found = FALSE_;

/*     See if this name (or one colliding with it in the */
/*     hash scheme) has already been stored in the name list. */

    if (node > 0) {
	head = node;
	tail = -nmpool[(head << 1) + 11];
	while(node > 0 && ! (*found)) {
	    *found = s_cmp(names + (node - 1) * names_len, varnam, names_len, 
		    varnam_len) == 0;
	    *nameat = node;
	    node = nmpool[(node << 1) + 10];
	}
	if (! (*found) && ! full) {

/*           We didn't find this name on the conflict resolution */
/*           list. Allocate a new slot for it. */

	    lnkan_(nmpool, &node);
	    lnkila_(&tail, &node, nmpool);
	    s_copy(names + (node - 1) * names_len, varnam, names_len, 
		    varnam_len);
	    *nameat = node;
	}
    } else if (! full) {

/*        Nothing like this variable name (in the hashing sense) */
/*        has been loaded so far.  We need to allocate */
/*        a name slot for this variable. */

	lnkan_(nmpool, &node);
	namlst[*lookat - 1] = node;
	s_copy(names + (node - 1) * names_len, varnam, names_len, varnam_len);
	*nameat = node;
    }

/*     If the name pool was full and we didn't find this name */
/*     we've got an error. Diagnose it and return. */

    if (full && ! (*found)) {
	setmsg_("The kernel pool does not have room for any more variables.", 
		(ftnlen)58);
	sigerr_("SPICE(KERNELPOOLFULL)", (ftnlen)21);
	chkout_("ZZGPNM", (ftnlen)6);
	return 0;
    }
    chkout_("ZZGPNM", (ftnlen)6);
    return 0;
} /* zzgpnm_ */

