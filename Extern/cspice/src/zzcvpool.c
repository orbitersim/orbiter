/* zzcvpool.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure ZZCVPOOL ( Private---Check variable update, with counter ) */
/* Subroutine */ int zzcvpool_(char *agent, integer *usrctr, logical *update, 
	ftnlen agent_len)
{
    extern /* Subroutine */ int zzpctrck_(integer *, logical *), chkin_(char *
	    , ftnlen), chkout_(char *, ftnlen), cvpool_(char *, logical *, 
	    ftnlen);
    extern logical return_(void);

/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines. Users should not call this routine directly due to the */
/*     volatile nature of this routine. */

/*     Determine whether or not any of the POOL variables that are to be */
/*     watched and have AGENT on their distribution list have been */
/*     updated, but do the full watcher check only if the POOL state */
/*     counter has changed. */

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

/*     SYMBOLS */
/*     UTILITY */

/* $ Declarations */
/* $ Abstract */

/*     This include file defines the dimension of the counter */
/*     array used by various SPICE subsystems to uniquely identify */
/*     changes in their states. */

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

/* $ Parameters */

/*     CTRSIZ      is the dimension of the counter array used by */
/*                 various SPICE subsystems to uniquely identify */
/*                 changes in their states. */

/* $ Author_and_Institution */

/*     B.V. Semenov    (JPL) */

/* $ Literature_References */

/*     None. */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 29-JUL-2013 (BVS) */

/* -& */

/*     End of include file. */

/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     AGENT      I   Name of the agent to check for notices */
/*     USRCTR    I/O  POOL state counter tracked by the caller */
/*     UPDATE     O   .TRUE. if variables for AGENT have been updated */

/* $ Detailed_Input */

/*     AGENT       is the name of a subroutine, entry point, or */
/*                 significant portion of code that needs to access */
/*                 variables in the kernel pool. Generally this agent */
/*                 will buffer these variables internally and fetch them */
/*                 from the kernel pool only when they are updated. */

/*     USRCTR      is the value of the POOL state counter tracked by */
/*                 (saved in) the caller (user) routine specifically */
/*                 for this agent. */

/* $ Detailed_Output */

/*     USRCTR      is the current POOL state counter. */

/*     UPDATE      is a logical flag that will be set to true if the */
/*                 variables in the kernel pool that are required by */
/*                 AGENT have been updated since the last call to */
/*                 CVPOOL. */

/* $ Parameters */

/*     CTRSIZ      is the dimension of the counter array used by */
/*                 various SPICE subsystems to uniquely identify */
/*                 changes in their states. This parameter is */
/*                 defined in the private include file 'zzctr.inc'. */

/* $ Exceptions */

/*     None. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine uses ZZPCTRCK to check the caller's saved POOL state */
/*     counter specific for the input AGENT against the current POOL */
/*     state counter to bypass the call to CVPOOL in cases when the POOL */
/*     has not changed. */

/*     If the POOL state counter specific for the input AGENT did not */
/*     change it bypasses the call to CVPOOL and returns the UPDATE flag */
/*     set to .FALSE. */

/*     If the POOL state counter specific for the input AGENT did change */
/*     it calls CVPOOL and returns the UPDATE flag set to whatever value */
/*     CVPOOL sets it to. */

/*     For details on ZZPCTRCK and CVPOOL see POOL the umbrella routine. */

/* $ Examples */

/*     Suppose that you have an application subroutine, TASK, that */
/*     needs to access to two large independent data set in the kernel */
/*     pool. If this data could be kept in local storage and kernel pool */
/*     queries performed only when the data in the kernel pool has been */
/*     updated, the routine can perform much more efficiently. */

/*     The code fragment below illustrates how you might make use of this */
/*     feature. */

/*     C */
/*     C     On the first call to this routine set two separate */
/*     C     watchers for the two sets of POOL variables that we are */
/*     C     interested in and initialize the two POOL state counters, */
/*     C     one for each POOL agent. */
/*     C */
/*           IF ( FIRST ) THEN */


/*              FIRST = .FALSE. */
/*              HAVE1 = .FALSE. */
/*              HAVE2 = .FALSE. */

/*              CALL ZZCTRUIN ( USRCT1 ) */
/*              CALL ZZCTRUIN ( USRCT2 ) */

/*              CALL SWPOOL ( 'TASK1', NNAMS1, NAMES1 ) */
/*              CALL SWPOOL ( 'TASK2', NNAMS2, NAMES2 ) */

/*           END IF */

/*     C */
/*     C     If any of the variables has been updated, fetch */
/*     C     it from the kernel pool. (Note that this also */
/*     C     handles getting variables for the first time.) */
/*     C     We use HAVE1 to indicate the fetch succeeded. If it */
/*     C     didn't, we need to attempt the fetch on the next */
/*     C     pass into this routine. Do the first set of variable. */
/*     C */
/*           CALL CVPOOL ( 'TASK1', USRCT1, UPDATE ) */

/*           IF (  UPDATE  .OR (.NOT. HAVE1 ) ) THEN */

/*              CALL GDPOOL ( 'TASK1_VAR_1', 1, M, N1, VALS1, FOUND(1) ) */
/*              CALL GDPOOL ( 'TASK1_VAR_2', 1, M, N2, VALS2, FOUND(2) ) */
/*                      . */
/*              CALL GDPOOL ( 'TASK1_VAR_N', 1, M, NN, VALSN, FOUND(N) ) */

/*           END IF */

/*           IF ( FAILED() ) THEN */
/*                 . */
/*              do something about the failure */
/*                 . */
/*           ELSE */
/*              HAVE1 = .TRUE. */
/*           END IF */

/*     C */
/*     C     Do the second set of variable. */
/*     C */
/*           CALL CVPOOL ( 'TASK2', USRCT2, UPDATE ) */

/*           IF (  UPDATE  .OR (.NOT. HAVE2 ) ) THEN */

/*              CALL GDPOOL ( 'TASK2_VAR_1', 1, M, N1, VALS1, FOUND(1) ) */
/*              CALL GDPOOL ( 'TASK2_VAR_2', 1, M, N2, VALS2, FOUND(2) ) */
/*                      . */
/*              CALL GDPOOL ( 'TASK2_VAR_N', 1, M, NN, VALSN, FOUND(N) ) */

/*           END IF */

/*           IF ( FAILED() ) THEN */
/*                 . */
/*              do something about the failure */
/*                 . */
/*           ELSE */
/*              HAVE2 = .TRUE. */
/*           END IF */


/* $ Restrictions */

/*     The POOL counter passed to the routine by the caller must be */
/*     specific to the POOL agent that's being checked. */

/*     The caller routines watching more than one agent must set up a */
/*     separate local POOL counter for each agent. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     B.V. Semenov   (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 06-SEP-2013 (BVS) */

/* -& */
/* $ Index_Entries */

/*     Check the kernel pool for updated variables, with counter */

/* -& */

/*     SPICELIB functions. */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }

/*     Check/update counter. */

    zzpctrck_(usrctr, update);

/*     If counter was updated, check in and call CVPOOL. */

    if (*update) {
	chkin_("ZZCVPOOL", (ftnlen)8);
	cvpool_(agent, update, agent_len);
	chkout_("ZZCVPOOL", (ftnlen)8);
    }
    return 0;
} /* zzcvpool_ */

