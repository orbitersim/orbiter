/* zzbods2c.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure ZZBODS2C ( Body name to ID translation, with bypass ) */
/* Subroutine */ int zzbods2c_(integer *usrctr, char *savnam, integer *savcde,
	 logical *savfnd, char *name__, integer *code, logical *found, ftnlen 
	savnam_len, ftnlen name_len)
{
    /* Builtin functions */
    integer s_cmp(char *, char *, ftnlen, ftnlen);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    extern /* Subroutine */ int zzbctrck_(integer *, logical *), chkin_(char *
	    , ftnlen), bods2c_(char *, integer *, logical *, ftnlen);
    logical update;
    extern /* Subroutine */ int chkout_(char *, ftnlen);
    extern logical return_(void);

/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines.  Users should not call this routine directly due */
/*     to the volatile nature of this routine. */

/*     Translate a string containing a body name or ID code to an */
/*     integer code, but bypass calling BODS2C and return saved values */
/*     provided by the caller if the name is the same as the saved name */
/*     and the ZZBODTRN state did not change. */

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

/*     NAIF_IDS */

/* $ Keywords */

/*     PRIVATE */
/*     BODY */

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
/*     USRCTR    I/O  ZZBODTRN state counter saved by the caller. */
/*     SAVNAM    I/O  Body name saved by the caller. */
/*     SAVCDE    I/O  Body ID code saved by the caller. */
/*     SAVFND    I/O  Translation success flag saved in the caller. */
/*     NAME       I   Body name. */
/*     CODE       O   Body ID code. */
/*     FOUND      O   Translation success flag. */
/*     CTRSIZ     P   Counter array size. */

/* $ Detailed_Input */

/*     USRCTR      is the value of the ZZBODTRN state counter tracked by */
/*                 (saved in) the caller (user) routine specifically for */
/*                 this body name/ID/found flag triplet of variables. */

/*     SAVNAM      is the body name saved in the caller routine */
/*                 specifically for this body name/ID/found flag triplet */
/*                 of variables. For detailed description of allowed */
/*                 values see description of the NAME argument in */
/*                 BODS2C. */

/*     SAVCDE      is the body ID code saved in the caller routine */
/*                 specifically for this body name/ID/found flag triplet */
/*                 of variables. For detailed description of allowed */
/*                 values see description of the CODE argument in */
/*                 BODS2C. */

/*     SAVFND      is the body name to ID translation success flag saved */
/*                 in the caller routine specifically for this body */
/*                 name/ID/found flag triplet of variables. SAVFND */
/*                 should .TRUE. if NAME had a translation or represents */
/*                 an integer. */

/*     NAME        is the input body name. For detailed description of */
/*                 allowed values see description of the NAME argument */
/*                 in BODS2C. */

/* $ Detailed_Output */

/*     USRCTR      is the current ZZBODTRN state counter. */

/*     SAVNAM      is the body name saved in the caller routine */
/*                 specifically for this body name/ID/found flag triplet */
/*                 of variables. On the output SAVNAM always equals */
/*                 NAME. */

/*     SAVCDE      is the body ID code saved in the caller routine */
/*                 specifically for this body name/ID/found flag triplet */
/*                 of variables. On the output SAVCDE always equals */
/*                 CODE. */

/*     SAVFND      is the body name to ID translation success flag saved */
/*                 in the caller routine specifically for this body */
/*                 name/ID/found flag triplet of variables. On the */
/*                 output SAVFND always equals FOUND. */

/*     CODE        is the output body ID code. For detailed description */
/*                 of possible values see description of the CODE */
/*                 argument in BODS2C. On the output CODE always equals */
/*                 SAVCDE. */

/*     FOUND       is the body name to ID translation success flag. */
/*                 FOUND is .TRUE. if NAME has a translation or */
/*                 represents an integer. Otherwise, FOUND is .FALSE. */
/*                 On the output FOUND always equals SAVFND. */

/* $ Parameters */

/*     CTRSIZ      is the dimension of the counter array used by */
/*                 various SPICE subsystems to uniquely identify */
/*                 changes in their states. This parameter is */
/*                 defined in the private include file 'zzctr.inc'. */

/* $ Exceptions */

/*     1) Errors may be signaled by routines in the call tree of this */
/*        routine. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine translates a string containing a body name or ID code */
/*     to an integer code but bypasses calling BODS2C to do the */
/*     translation and simply returns the saved ID value and translation */
/*     success flag provided by the caller if the input name is the same */
/*     as the saved name provided by the caller, the saved translation */
/*     success flag provided by the caller is .TRUE. and the ZZBODTRN */
/*     state counter tracked by the caller is the same as the current */
/*     ZZBODTRN state counter. */

/*     The ZZBODTRN state counter and name/ID/found flag triplet of */
/*     saved variables tracked by the caller must be specific for each */
/*     body of interest. I.e. if the caller routine needs to call this */
/*     routine to do translations for a target body and for an observer */
/*     body, the caller routine mush use must have its own set of saved */
/*     ZZBODTRN state counter and name/ID/found flag variables for each */
/*     of the two bodies . */

/* $ Examples */

/*     This example shows how a routine that needs to do body name-ID */
/*     conversion for two distinct bodies can do it using ZZBODS2C. */

/*           SUBROUTINE <name> ( TARG, OBS, ... ) */
/*           ... */
/*           INCLUDE               'zzctr.inc' */
/*           ... */
/*     C */
/*     C     Saved body name length. */
/*     C */
/*           INTEGER               MAXL */
/*           PARAMETER           ( MAXL  = 36 ) */
/*           .... */
/*     C */
/*     C */
/*     C     Saved name/ID item declarations. */
/*     C */
/*           INTEGER               SVCTR1 ( CTRSIZ ) */
/*           CHARACTER*(MAXL)      SVTARG */
/*           INTEGER               SVTGID */
/*           LOGICAL               SVFND1 */
/*           INTEGER               SVCTR2 ( CTRSIZ ) */
/*           CHARACTER*(MAXL)      SVOBSN */
/*           INTEGER               SVOBSI */
/*           LOGICAL               SVFND2 */
/*           LOGICAL               FIRST */
/*     C */
/*     C     Saved name/ID items. */
/*     C */
/*           SAVE                  SVCTR1 */
/*           SAVE                  SVTARG */
/*           SAVE                  SVTGID */
/*           SAVE                  SVFND1 */
/*           SAVE                  SVCTR2 */
/*           SAVE                  SVOBSN */
/*           SAVE                  SVOBSI */
/*           SAVE                  SVFND2 */
/*           SAVE                  FIRST */
/*     C */
/*     C     Initial values. */
/*     C */
/*           DATA                  FIRST   / .TRUE. / */
/*           ... */
/*     C */
/*     C     Initialization. */
/*     C */
/*           IF ( FIRST ) THEN */
/*     C */
/*     C        Initialize ZZBODTRN counters. */
/*     C */
/*              CALL ZZCTRUIN( SVCTR1 ) */
/*              CALL ZZCTRUIN( SVCTR2 ) */
/*              FIRST = .FALSE. */
/*           END IF */
/*     C */
/*     C     Starting from translation of target name to ID. */
/*     C */
/*           CALL ZZBODS2C ( SVCTR1, SVTARG, SVTGID, SVFND1, */
/*          .                TARG, TARGID, FOUND ) */

/*           IF ( .NOT. FOUND ) THEN */
/*              CALL SETMSG ( '...' ) */
/*              CALL SIGERR ( 'SPICE(TARGIDCODENOTFOUND)' ) */
/*              CALL CHKOUT ( '<name>' ) */
/*              RETURN */
/*           END IF */
/*     C */
/*     C     Now do the same for observer */
/*     C */
/*           CALL ZZBODS2C ( SVCTR2, SVOBSN, SVOBSI, SVFND2, */
/*          .                OBS, OBSID, FOUND ) */

/*           IF ( .NOT. FOUND ) THEN */
/*              CALL SETMSG ( '...' ) */
/*              CALL SIGERR ( 'SPICE(OBSIDCODENOTFOUND)' ) */
/*              CALL CHKOUT ( '<name>' ) */
/*              RETURN */
/*           END IF */
/*           ... */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     B.V. Semenov    (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 21-SEP-2013 (BVS) */

/* -& */

/*     SPICE functions. */


/*     Local variables. */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }

/*     Check/update ZZBODTRN state counter. */

    zzbctrck_(usrctr, &update);

/*     Check update flag, saved found flag, and saved name against the */
/*     input. */

    if (! update && *savfnd && s_cmp(savnam, name__, savnam_len, name_len) == 
	    0) {

/*        No change in body-name mapping state, the saved name was */
/*        successfully resolved earlier, and input and saved names are */
/*        the same. Return saved ID and FOUND. */

	*code = *savcde;
	*found = *savfnd;
    } else {

/*        Check in because BODS2C may fail. */

	chkin_("ZZBODS2C", (ftnlen)8);

/*        Body-name mapping state changed, or the saved name was never */
/*        successfully resolved earlier, or input and saved names are */
/*        different. Call BODS2C to look up ID and FOUND and reset saved */
/*        values. */

	bods2c_(name__, code, found, name_len);
	s_copy(savnam, name__, savnam_len, name_len);
	*savcde = *code;
	*savfnd = *found;

/*        Check out. */

	chkout_("ZZBODS2C", (ftnlen)8);
    }
    return 0;
} /* zzbods2c_ */

