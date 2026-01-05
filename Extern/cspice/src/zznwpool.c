/* zznwpool.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure ZZNWPOOL ( Private: notify watchers of update ) */
/* Subroutine */ int zznwpool_(char *varnam, char *wtvars, integer *wtptrs, 
	integer *wtpool, char *wtagnt, char *agtwrk, char *notify, char *
	agents, ftnlen varnam_len, ftnlen wtvars_len, ftnlen wtagnt_len, 
	ftnlen agtwrk_len, ftnlen notify_len, ftnlen agents_len)
{
    extern /* Subroutine */ int zzgapool_(char *, char *, integer *, integer *
	    , char *, char *, ftnlen, ftnlen, ftnlen, ftnlen), chkin_(char *, 
	    ftnlen), copyc_(char *, char *, ftnlen, ftnlen), unionc_(char *, 
	    char *, char *, ftnlen, ftnlen, ftnlen), chkout_(char *, ftnlen);
    extern logical return_(void);

/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines.  Users should not call this routine directly due to the */
/*     volatile nature of this routine. */

/*     Union the set of agents for a specified, watched kernel variable */
/*     with the set of agents on the kernel pool's update notification */
/*     list. */

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

/*     KERNEL */

/* $ Keywords */

/*     KERNEL */
/*     PRIVATE */
/*     UTILITY */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     VARNAM     I   Kernel variable name. */
/*     WTVARS     I   Watched kernel variable set. */
/*     WTPTRS     I   Pointers from variables into the watch pool. */
/*     WTPOOL     I   Watch pool used for managing agent names. */
/*     WTAGNT     I   Array of agent names. */
/*     AGTWRK    I-O  Agent workspace cell. */
/*     NOTIFY    I-O  Another agent workspace cell. */
/*     AGENTS    I-O  Set of agents to be notified of updates. */

/* $ Detailed_Input */

/*     VARNAM      is the name of a kernel variable. */

/*     WTVARS      is a SPICE set containing the contents of the kernel */
/*                 pool watcher system's set WTVARS. */

/*     WTPTRS      is an array containing the contents of the kernel */
/*                 pool watcher system's array WTPTRS. */

/*     WTPOOL      is a SPICE doubly linked list pool containing the */
/*                 contents of the kernel pool watcher system's pool */
/*                 WTPOOL. */

/*     WTAGNT      is an array containing the contents of the kernel */
/*                 pool watcher system's array WTAGNT. */

/*     AGTWRK, */
/*     NOTIFY      are two workspace cells used to hold list of agents. */
/*                 Both cells must have size at least equal to MXNOTE. */

/* $ Detailed_Output */

/*     AGTWRK, */
/*     NOTIFY      are the input workspace cells after use. Contents */
/*                 of these cells are undefined. */

/*     AGTSET      is a SPICE set containing the names of the agents */
/*                 associated with the kernel variable designated by */
/*                 VARNAM. */

/* $ Parameters */

/*     MXNOTE      Maximum size of the agent list WTAGNT in POOL. */
/*                 See that routine for the parameter's value. */


/* $ Exceptions */

/*     1) If the output set AGENTS is too small to hold the result */
/*        of the union performed by this routine, the error will be */
/*        diagnosed by routines in the call tree of this routine. */

/*     2) If either workspace cell AGTWRK or NOTIFY has insufficient */
/*        size, the error will be diagnosed by routines in the call tree */
/*        of this routine. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine is not part of the SPICELIB API. This routine */
/*     may be removed in a later version of the SPICE Toolkit, or */
/*     its interface may change. */

/*     SPICE-based application code should not call this routine. */

/*     This routine centralizes the work of updating the kernel */
/*     pool's update notification list to account for an update */
/*     of a specified kernel variable. Most kernel pool entry */
/*     points that perform kernel pool updates should call this */
/*     routine to update the notification list. */

/* $ Examples */

/*     See POOL entry point SWPOOL. */

/* $ Restrictions */

/*     1) This is a private routine. See $Particulars above. */

/*     2) Contents of the input arrays are assumed to be valid. */
/*        The output returned by this routine is meaningless */
/*        otherwise. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman    (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 18-MAR-2009 (NJB) */

/* -& */
/* $ Index_Entries */

/*     add agents to watcher system notification list */

/* -& */
/* $ Revisions */

/*     None. */

/* -& */

/*     SPICELIB functions */

    if (return_()) {
	return 0;
    }
    chkin_("ZZNWPOOL", (ftnlen)8);

/*     Fetch the agents watching VARNAM into the set NOTIFY. */

    zzgapool_(varnam, wtvars, wtptrs, wtpool, wtagnt, notify, varnam_len, 
	    wtvars_len, wtagnt_len, notify_len);

/*     Compute the union of NOTIFY and the agent list AGENTS. */
/*     Place the result in the workspace set AGTWRK; then copy */
/*     the result to AGENTS. */

    unionc_(notify, agents, agtwrk, notify_len, agents_len, agtwrk_len);
    copyc_(agtwrk, agents, agtwrk_len, agents_len);
    chkout_("ZZNWPOOL", (ftnlen)8);
    return 0;
} /* zznwpool_ */

