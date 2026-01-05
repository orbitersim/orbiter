/* zzctr.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure ZZCTR ( Manipulate Counter Array ) */
/* Subroutine */ int zzctr_0_(int n__, integer *newctr, integer *oldctr, 
	logical *update)
{
    /* Initialized data */

    static logical first = TRUE_;

    extern /* Subroutine */ int chkin_(char *, ftnlen);
    static integer ctrhgh;
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen);
    extern integer intmin_(void), intmax_(void);
    extern /* Subroutine */ int setmsg_(char *, ftnlen);
    static integer ctrlow;
    extern logical return_(void);

/* $ Abstract */

/*     Manipulate counter array. */

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

/*     None. */

/* $ Keywords */

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

/*     Variable  I/O  Entry */
/*     --------  ---  -------------------------------------------------- */
/*     NEWCTR     I   ZZCTRCHK */
/*     OLDCTR    I/O  ZZCTRUIN, ZZCTRSIN, ZZCTRINC, ZZCTRCHK */
/*     UPDATE     O   ZZCTRCHK */

/*     CTRSIZ     P   (All) */

/* $ Detailed_Input */

/*     See the ENTRY points for a discussion of their arguments. */

/* $ Detailed_Output */

/*     See the ENTRY points for a discussion of their arguments. */

/* $ Parameters */

/*     CTRSIZ      is the dimension of the counter array used by */
/*                 various SPICE subsystems to identify */
/*                 changes in their states. This parameter is */
/*                 defined in the include file. */

/* $ Exceptions */

/*     1) If ZZCTR is called directly, the error SPICE(BOGUSENTRY) is */
/*        signaled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     ZZCTR should never be called directly, but should instead be */
/*     accessed only through its entry points. */

/*     The purpose of this routine is to manipulate counter arrays used */
/*     by various SPICE subsystems to identify changes in their states. */

/*           ZZCTRUIN       Initialize counter array kept by a user */
/*                          routine. */

/*           ZZCTRSIN       Initialize counter array kept by a */
/*                          subsystem routine. */

/*           ZZCTRINC       Increment counter array. */

/*           ZZCTRCHK       Check/update counter array. */

/*     A counter array consists of CTRSIZ elements representing */
/*     cascading counters. The fastest counter is at index 1, the */
/*     slowest counter is at index CTRSIZ. At the start of counting all */
/*     counter array elements are set to INTMIN. In the process of */
/*     counting the fastest element is incremented by one. As with any */
/*     cascading counters when the fastest counter reaches INTMAX it */
/*     rolls back to INTMIN and the next counter is incremented by 1. */
/*     When all counters reach INTMAX, ZZCTRINC signals an error. */

/* $ Examples */

/*     The counter array gives subsystems like POOL a way to provide to */
/*     the user routines of that subsystem's services a simple indicator */
/*     of whether the subsystem's state has changed, meaning that some */
/*     saved values may have become stale and need to be updated or some */
/*     other action needs to be taken in the user routines. */

/*     For that the subsystem initializes a counter array ("subsystem */
/*     counter") using ZZCTRSIN, saves it, increments it using ZZCTRINC */
/*     each time the subsystem's state changes, and resets a user */
/*     counter to subsystem counter and indicates the need for update */
/*     when counters are not the same, like this: */

/*        C */
/*        C     Include zzctr.inc to access CTRSIZ. */
/*        C */
/*              INCLUDE              'zzctr.inc' */
/*              ... */
/*        C */
/*        C     In input/output variable declarations declare user */
/*        C     user counter (USRCTR) as I/O and the update flag */
/*        C     (UPDATE) as O. */
/*        C */
/*              INTEGER               USRCTR ( CTRSIZ ) */
/*              LOGICAL               UPDATE */
/*              ... */
/*        C */
/*        C     In local variable declarations declare and save the */
/*        C     subsystem counter (SUBCTR) that will incremented */
/*        C     with each change of state. */
/*        C */
/*              INTEGER               SUBCTR ( CTRSIZ ) */
/*              ... */
/*              SAVE                  SUBCTR */
/*              ... */
/*        C */
/*        C     In all places where initialization is done set */
/*        C     subsystem counter using ZZCTRSIN. */
/*        C */
/*              IF ( FIRST ) THEN */
/*                 ... */
/*                 CALL ZZCTRSIN( SUBCTR ) */
/*                 FIRST = .FALSE. */
/*              END IF */
/*              ... */
/*        C */
/*        C     In all places where the subsystem state changes */
/*        C     increment the subsystem counter using ZZCTRINC. */
/*        C */
/*              CALL ZZCTRINC( SUBCTR ) */
/*              ... */
/*        C */
/*        C     Make a special entry that checks and resets input */
/*        C     user counter to the current subsystem counter and */
/*        C     indicates whether it was updated using ZZCTRCHK. */
/*        C */
/*              ENTRY <entry_name> ( USRCTR, UPDATE ) */

/*              CALL ZZCTRCHK( SUBCTR, USRCTR, UPDATE ) */

/*              RETURN */
/*              ... */

/*     The users of the subsystem services initialize a counter array */
/*     ("user counter") using ZZCTRUIN, save it, and check against the */
/*     subsystem's counter and update, if needed, using the subsystem's */
/*     entry point, like this: */

/*        C */
/*        C     Include zzctr.inc to access CTRSIZ. */
/*        C */
/*              INCLUDE              'zzctr.inc' */
/*              ... */
/*        C */
/*        C     In local variable declarations declare and save */
/*        C     user counter. Also declare an update flag. */
/*        C */
/*              INTEGER               USRCTR ( CTRSIZ ) */
/*              LOGICAL               UPDATE */
/*              ... */
/*              SAVE                  USRCTR */
/*              ... */
/*        C */
/*        C     In all places where initialization is done initialize */
/*        C     the user counter using ZZCTRUIN to ensure update on */
/*        C     the first check. */
/*        C */
/*              IF ( FIRST ) THEN */
/*                 ... */
/*                 CALL ZZCTRUIN( USRCTR ) */
/*                 FIRST = .FALSE. */
/*              END IF */
/*              ... */
/*        C */
/*        C     In all places where there is a need to check for */
/*        C     the subsystem state change call subsystem's entry */
/*        C     that checks and updates the user counter based on */
/*        C     value of the subsystem counter. */
/*        C */
/*              CALL <entry_name> ( USRCTR, UPDATE ) */

/*              IF ( UPDATE ) THEN */

/*        C */
/*        C        Do what needs to be done when the subsystem */
/*        C        state has changed. */
/*        C */
/*                 ... */

/*              END IF */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     B.V. Semenov    (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 29-JUL-2013 (BVS) */

/* -& */
/* $ Index_Entries */

/*     manipulate counter array */

/* -& */

/*     SPICELIB functions */


/*     Local variables. */


/*     Save EVERYTHING. */


/*     Initial values. */

    /* Parameter adjustments */
    if (newctr) {
	}

    /* Function Body */
    switch(n__) {
	case 1: goto L_zzctruin;
	case 2: goto L_zzctrsin;
	case 3: goto L_zzctrinc;
	case 4: goto L_zzctrchk;
	}


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }

/*     This routine should never be called. If this routine is called, */
/*     an error is signaled. */

    chkin_("ZZCTR", (ftnlen)5);
    setmsg_("ZZCTR: You have called an entry which performs performs no run-"
	    "time function. This may indicate a bug. Please check the documen"
	    "tation for the subroutine ZZCTR.", (ftnlen)159);
    sigerr_("SPICE(BOGUSENTRY)", (ftnlen)17);
    chkout_("ZZCTR", (ftnlen)5);
    return 0;
/* $Procedure ZZCTRUIN ( CounTeR array, User counter INitialization ) */

L_zzctruin:
/* $ Abstract */

/*     Set counter array to the initial values for the user routines. */

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

/*     None. */

/* $ Keywords */

/*     UTILITY */

/* $ Declarations */

/*     INTEGER               OLDCTR ( CTRSIZ ) */

/* $ Brief_I/O */

/*     Variable  I/O  Entry */
/*     --------  ---  -------------------------------------------------- */
/*     OLDCTR     O   Counter array to be set to initial user values. */

/*     CTRSIZ     P   Counter array size. */

/* $ Detailed_Input */

/*     None. */

/* $ Detailed_Output */

/*     OLDCTR      is the counter array set to the initial values for */
/*                 the user routines. */

/* $ Parameters */

/*     CTRSIZ      is the dimension of the counter array used by */
/*                 various SPICE subsystems to identify */
/*                 changes in their states. */

/* $ Exceptions */

/*     None. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This type of initialization must be done by all user routines */
/*     to ensure update on the first check. */

/* $ Examples */

/*     See umbrella Examples. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     B.V. Semenov    (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 29-JUL-2013 (BVS) */

/* -& */
/* $ Index_Entries */

/*     initialize counter array to user values */

/* -& */

/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }

/*     Initialize the high and low values. */

    if (first) {
	ctrhgh = intmax_();
	ctrlow = intmin_();
	first = FALSE_;
    }

/*     Set counter. */

    oldctr[0] = ctrhgh;
    oldctr[1] = ctrhgh;
    return 0;
/* $Procedure ZZCTRSIN ( CounTeR array, Subsystem counter INitialization ) */

L_zzctrsin:
/* $ Abstract */

/*     Set counter array to the initial values for the subsystem */
/*     routines. */

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

/*     None. */

/* $ Keywords */

/*     UTILITY */

/* $ Declarations */

/*     INTEGER               OLDCTR ( CTRSIZ ) */

/* $ Brief_I/O */

/*     Variable  I/O  Entry */
/*     --------  ---  -------------------------------------------------- */
/*     OLDCTR     O   Counter array to be set to initial subsystem values */

/*     CTRSIZ     P   Counter array size. */

/* $ Detailed_Input */

/*     None. */

/* $ Detailed_Output */

/*     OLDCTR      is the counter array set to the initial values for */
/*                 the subsystem routines. */

/* $ Parameters */

/*     CTRSIZ      is the dimension of the counter array used by */
/*                 various SPICE subsystems to identify */
/*                 changes in their states. */

/* $ Exceptions */

/*     None. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This type of initialization must be done by all subsystem */
/*     routines to set the counters to the lowest initial values. */

/* $ Examples */

/*     See umbrella Examples. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     B.V. Semenov    (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 29-JUL-2013 (BVS) */

/* -& */
/* $ Index_Entries */

/*     initialize counter array to subsystem values */

/* -& */

/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }

/*     Initialize the high and low values. */

    if (first) {
	ctrhgh = intmax_();
	ctrlow = intmin_();
	first = FALSE_;
    }

/*     Set counter. */

    oldctr[0] = ctrlow;
    oldctr[1] = ctrlow;
    return 0;
/* $Procedure ZZCTRINC ( CounTeR Array, INCrement counter ) */

L_zzctrinc:
/* $ Abstract */

/*     Increment counter array. */

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

/*     None. */

/* $ Keywords */

/*     UTILITY */

/* $ Declarations */

/*     INTEGER               OLDCTR ( CTRSIZ ) */

/* $ Brief_I/O */

/*     Variable  I/O  Entry */
/*     --------  ---  -------------------------------------------------- */
/*     OLDCTR    I/O  Counter array to be incremented. */

/*     CTRSIZ     P   Counter array size. */

/* $ Detailed_Input */

/*     OLDCTR      is the counter array to be incremented. */

/* $ Detailed_Output */

/*     OLDCTR      is the counter array that was incremented. */

/* $ Parameters */

/*     CTRSIZ      is the dimension of the counter array used by */
/*                 various SPICE subsystems to identify */
/*                 changes in their states. */

/* $ Exceptions */

/*     1) If all elements of the counter array on the input are equal */
/*        to INTMAX, the error '(SPICEISTIRED)' is signaled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     None. */

/* $ Examples */

/*     See umbrella Examples. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     B.V. Semenov    (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 29-JUL-2013 (BVS) */

/* -& */
/* $ Index_Entries */

/*     increment counter array */

/* -& */

/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }

/*     Initialize the high and low values. */

    if (first) {
	ctrhgh = intmax_();
	ctrlow = intmin_();
	first = FALSE_;
    }

/*     Signal an error if both input counter array elements have high */
/*     values. */

    if (oldctr[0] == ctrhgh && oldctr[1] == ctrhgh) {
	chkin_("ZZCTRINC", (ftnlen)8);
	setmsg_("A subsystem state counter overflowed. For this to happen th"
		"ere must be a SPICE bug or you must have been running your S"
		"PICE-based application for a very long time. Please contact "
		"NAIF.and report the circumstances under which this happened.",
		 (ftnlen)239);
	sigerr_("SPICE(SPICEISTIRED)", (ftnlen)19);
	chkout_("ZZCTRINC", (ftnlen)8);
	return 0;
    }

/*     Increment counters. */

    if (oldctr[0] == ctrhgh) {
	oldctr[0] = ctrlow;
	++oldctr[1];
    } else {
	++oldctr[0];
    }
    return 0;
/* $Procedure ZZCTRCHK ( CounTeR array, CHecK and update ) */

L_zzctrchk:
/* $ Abstract */

/*     Check and update, if needed, counter array. */

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

/*     None. */

/* $ Keywords */

/*     UTILITY */

/* $ Declarations */

/*     INTEGER               NEWCTR ( CTRSIZ ) */
/*     INTEGER               OLDCTR ( CTRSIZ ) */
/*     LOGICAL               UPDATE */

/* $ Brief_I/O */

/*     Variable  I/O  Entry */
/*     --------  ---  -------------------------------------------------- */
/*     NEWCTR     I   New counter value. */
/*     OLDCTR    I/O  Old counter value. */
/*     UPDATE     O   Update flag. */

/*     CTRSIZ     P   Counter array size. */

/* $ Detailed_Input */

/*     NEWCTR      is the new counter value against which the old */
/*                 value should be checked. */

/*     OLDCTR      is the old counter value. */

/* $ Detailed_Output */

/*     OLDCTR      is the updated the old counter value, set equal to */
/*                 the new counter value if counters were different or */
/*                 kept the same if counters were the same. */

/*     UPDATE      is the logical flag indicating whether the old */
/*                 counter was updated. */

/* $ Parameters */

/*     CTRSIZ      is the dimension of the counter array used by */
/*                 various SPICE subsystems to identify */
/*                 changes in their states. */

/* $ Exceptions */

/*     Error free. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     None. */

/* $ Examples */

/*     See umbrella Examples. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     B.V. Semenov    (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 29-JUL-2013 (BVS) */

/* -& */
/* $ Index_Entries */

/*     check and update counter array */

/* -& */

/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }

/*     Do counters differ? */

    *update = newctr[0] != oldctr[0] || newctr[1] != oldctr[1];

/*     If they do, set old counter to new value. */

    if (*update) {
	oldctr[0] = newctr[0];
	oldctr[1] = newctr[1];
    }
    return 0;
} /* zzctr_ */

/* Subroutine */ int zzctr_(integer *newctr, integer *oldctr, logical *update)
{
    return zzctr_0_(0, newctr, oldctr, update);
    }

/* Subroutine */ int zzctruin_(integer *oldctr)
{
    return zzctr_0_(1, (integer *)0, oldctr, (logical *)0);
    }

/* Subroutine */ int zzctrsin_(integer *oldctr)
{
    return zzctr_0_(2, (integer *)0, oldctr, (logical *)0);
    }

/* Subroutine */ int zzctrinc_(integer *oldctr)
{
    return zzctr_0_(3, (integer *)0, oldctr, (logical *)0);
    }

/* Subroutine */ int zzctrchk_(integer *newctr, integer *oldctr, logical *
	update)
{
    return zzctr_0_(4, newctr, oldctr, update);
    }

