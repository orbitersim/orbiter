/* zzholdd.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;
static integer c__4 = 4;

/* $Procedure ZZHOLDD ( Private --- hold a scalar DP ) */
/* Subroutine */ int zzholdd_(integer *op, integer *id, logical *ok, 
	doublereal *value)
{
    /* Initialized data */

    static logical init = TRUE_;

    /* System generated locals */
    integer i__1;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer);

    /* Local variables */
    integer i__;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    static logical first[4];
    extern integer brckti_(integer *, integer *, integer *);
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen);
    static doublereal svalue[4];
    extern /* Subroutine */ int setmsg_(char *, ftnlen), errint_(char *, 
	    integer *, ftnlen);
    extern logical return_(void);

/* $ Abstract */

/*     SPICE private routine intended solely for the support of SPICE */
/*     routines. Users should not call this routine directly due to the */
/*     volatile nature of this routine. */

/*     Persistently store double precision values or retrieve stored */
/*     double precision values. That's it, not really rocket science. */

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

/*     STORE DP VALUE */

/* $ Declarations */
/* $ Abstract */

/*     SPICE private routine intended solely for the support of SPICE */
/*     routines. Users should not call this routine directly due to the */
/*     volatile nature of this routine. */

/*     This file contains parameter declarations for the ZZHOLDD */
/*     routine. */

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

/*     None. */

/* $ Declarations */

/*     None. */

/* $ Brief_I/O */

/*     None. */

/* $ Detailed_Input */

/*     None. */

/* $ Detailed_Output */

/*     None. */

/* $ Parameters */

/*     GEN       general value, primarily for testing. */

/*     GF_REF    user defined GF reference value. */

/*     GF_TOL    user defined GF convergence tolerance. */

/*     GF_DT     user defined GF step for numeric differentiation. */

/* $ Exceptions */

/*     None. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     None. */

/* $ Examples */

/*     None. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     E.D. Wright    (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.0  03-DEC-2013 (EDW) */

/* -& */

/*     OP codes. The values exist in the integer domain */
/*     [ -ZZNOP, -1], */


/*     Current number of OP codes. */


/*     ID codes. The values exist in the integer domain */
/*     [ 1, NID], */


/*     General use, primarily testing. */


/*     The user defined GF reference value. */


/*     The user defined GF convergence tolerance. */


/*     The user defined GF step for numeric differentiation. */


/*     Current number of ID codes, dimension of array */
/*     in ZZHOLDD. Bad things can happen if this parameter */
/*     does not have the proper value. */


/*     End of file zzholdd.inc. */

/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     OP         I   Key for operation to execute. */
/*     ID         I   The ID for the item to apply OP. */
/*     OK         O   Boolean indicating success of get operation. */
/*     VALUE     I-O  Double precision value returned or to store. */

/* $ Detailed_Input */

/*     OP          The scalar integer key for the operation to execute. */
/*                 Proper values of OP: */

/*                    ZZPUT     store a double precision value for */
/*                              later use (put). */

/*                    ZZGET     retrieve a stored double precision */
/*                              value (get). */

/*                    ZZRESET   reset function to require a ZZPUT prior */
/*                              to a subsequent ZZGET (clear). */

/*     ID          The scalar integer ID for the item to get/put etc. */
/*                 Proper values of ID: */

/*                    GEN       general value, primarily for testing. */

/*                    GF_REF    user defined GF reference value. */

/*                    GF_TOL    user defined GF convergence tolerance. */

/*                    GF_DT     user defined GF step for numeric */
/*                              differentiation. */

/*     VALUE       The scalar double precision value to store (put). */

/*     The include file "zzholdd.inc" lists all accepted values for */
/*     ID and OP. */

/* $ Detailed_Output */

/*     OK          The logical flag indicating if a get operation */
/*                 returned a valid value for ID. OK returns false if a */
/*                 get operation occurs before a put. */

/*                 This argument has no meaning except when performing */
/*                 a get operation. */

/*     VALUE       The scalar double precision value retrieved (get). */

/* $ Parameters */

/*    None. */

/* $ Exceptions */

/*     1)  The error SPICE(UNKNOWNID) signals if the value of ID is */
/*         not one of those coded in zzholdd.inc. */

/*     2)  The error SPICE(UNKNOWNOP) signals if the value of OP is */
/*         not one of those coded in zzholdd.inc. */

/* $ Files */

/*    zzholdd.inc */

/* $ Particulars */

/*     This routine simply stores double precision values for later */
/*     retrieval. */

/*     A get operation may succeed or fail based on whether */
/*     a put operation preceded the put. */

/*        A ZZHOLDD get operation for an ID called before a put operation */
/*        for that ID returns with OK as false, VALUE as 0. */

/*        A ZZHOLDD get operation for an ID called after a put operation */
/*        for that ID returns with OK as true, VALUE as the value */
/*        assigned by the put. */

/* $ Examples */

/*     The numerical results shown for these examples may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */

/*     Store values using ZZHOLDD then attempt to retrieve the values. */

/*           PROGRAM ZZHOLDD_T */
/*           IMPLICIT NONE */

/*           INCLUDE 'zzholdd.inc' */

/*           DOUBLE PRECISION     VALUE */
/*           DOUBLE PRECISION     X */
/*           DOUBLE PRECISION     Y */
/*           DOUBLE PRECISION     Z */
/*           LOGICAL              OK */

/*           X = -11.D0 */
/*           Y =  22.D0 */
/*           Z = -33.D0 */

/*     C */
/*     C     Perform a put then get. */
/*     C */
/*           VALUE = 0.D0 */
/*           OK    = .FALSE. */
/*           CALL ZZHOLDD ( ZZPUT, GEN, OK, X) */
/*           CALL ZZHOLDD ( ZZGET, GEN, OK, VALUE ) */

/*           IF (OK) THEN */
/*              WRITE(*,*) 'Check 1 ', VALUE */
/*           ELSE */
/*              WRITE(*,*) 'Error 1 ' */
/*           END IF */

/*     C */
/*     C     Reset then get without put. */
/*     C */
/*           VALUE = 0.D0 */
/*           OK    = .FALSE. */

/*           CALL ZZHOLDD ( ZZRESET,   GEN, OK, VALUE ) */
/*           CALL ZZHOLDD ( ZZGET,     GEN, OK, VALUE ) */

/*           IF (OK) THEN */
/*              WRITE(*,*) 'Error 2 ' */
/*           ELSE */
/*              WRITE(*,*) 'Check 2 ', VALUE */
/*           END IF */

/*     C */
/*     C     Now put. */
/*     C */
/*           CALL ZZHOLDD ( ZZPUT, GEN, OK, Y) */
/*           CALL ZZHOLDD ( ZZGET, GEN, OK, VALUE ) */

/*           IF (OK) THEN */
/*              WRITE(*,*) 'Check 3 ', VALUE */
/*           ELSE */
/*              WRITE(*,*) 'Error 3 ' */
/*           END IF */


/*     C */
/*     C     Now another put with a different value. */
/*     C */
/*           CALL ZZHOLDD ( ZZPUT, GEN, OK, Z) */
/*           CALL ZZHOLDD ( ZZGET, GEN, OK, VALUE ) */

/*           IF (OK) THEN */
/*              WRITE(*,*) 'Check 4 ', VALUE */
/*           ELSE */
/*              WRITE(*,*) 'Error 4 ' */
/*           END IF */

/*           END */

/*   The program outputs: */

/*      Check 1   -11.000000000000000 */
/*      Check 2    0.0000000000000000 */
/*      Check 3    22.000000000000000 */
/*      Check 4   -33.000000000000000 */

/*    As expected. */

/* $ Restrictions */

/*    None. */

/* $ Literature_References */

/*    None. */

/* $ Author_and_Institution */

/*    E.D. Wright    (JPL) */

/* $ Version */

/* -   SPICELIB Version 1.2.0  07-OCT-2021 (EDW) */

/*      Added explicit output argument assignments for */
/*      return-on-entry case. */

/* -   SPICELIB Version 1.1.0  03-DEC-2013 (EDW) */

/*       Added ID and OK arguments to routine, generalizing use. */

/*       Added RETURN() check. */

/* -   SPICELIB Version 1.0.0  16-FEB-2010 (EDW) */

/* -& */
/* $ Index_Entries */

/*    store a double precision value */
/*    retrieve a stored double precision value */

/* -& */

/*     SPICELIB functions */


/*     Local variables. */


/*     Standard SPICE error handling. */

    if (return_()) {
	*value = 0.;
	*ok = FALSE_;
	return 0;
    }

/*     Confirm a proper ID value. */

    if (brckti_(id, &c__1, &c__4) != *id) {
	*value = 0.;
	*ok = FALSE_;
	chkin_("ZZHOLDD", (ftnlen)7);
	setmsg_("ID value unknown. ID value #1 not an element of [1, #2]. Co"
		"nfirmthe ID value exists in the zzholdd.inc parameter file.", 
		(ftnlen)118);
	errint_("#1", id, (ftnlen)2);
	errint_("#2", &c__4, (ftnlen)2);
	sigerr_("SPICE(UNKNOWNID)", (ftnlen)16);
	chkout_("ZZHOLDD", (ftnlen)7);
	return 0;
    }

/*     Initialize the FIRST array; perform once per program run. */

    if (init) {
	for (i__ = 1; i__ <= 4; ++i__) {
	    first[(i__1 = i__ - 1) < 4 && 0 <= i__1 ? i__1 : s_rnge("first", 
		    i__1, "zzholdd_", (ftnlen)325)] = TRUE_;
	}
	init = FALSE_;
    }

/*     Perform the operation as described by OP. */

    if (*op == -1) {

/*        Attempt to retrieve a stored double precision value for ID. */

/*          - Return the value stored by a put operation and OK */
/*            as true. */

/*          - If no previous set to this ID, return value as zero and */
/*            OK as false. */

	if (first[(i__1 = *id - 1) < 4 && 0 <= i__1 ? i__1 : s_rnge("first", 
		i__1, "zzholdd_", (ftnlen)348)]) {
	    *value = 0.;
	    *ok = FALSE_;
	} else {

/*           Return the stored value. */

	    *value = svalue[(i__1 = *id - 1) < 4 && 0 <= i__1 ? i__1 : s_rnge(
		    "svalue", i__1, "zzholdd_", (ftnlen)358)];
	    *ok = TRUE_;
	}
    } else if (*op == -2) {

/*        Store a value for later use. Set FIRST to false */
/*        so subsequent get calls will work. */

	if (first[(i__1 = *id - 1) < 4 && 0 <= i__1 ? i__1 : s_rnge("first", 
		i__1, "zzholdd_", (ftnlen)370)]) {
	    first[(i__1 = *id - 1) < 4 && 0 <= i__1 ? i__1 : s_rnge("first", 
		    i__1, "zzholdd_", (ftnlen)372)] = FALSE_;
	}
	svalue[(i__1 = *id - 1) < 4 && 0 <= i__1 ? i__1 : s_rnge("svalue", 
		i__1, "zzholdd_", (ftnlen)376)] = *value;
    } else if (*op == -3) {

/*        Reset FIRST( ID ) forcing a put before a get. */

	first[(i__1 = *id - 1) < 4 && 0 <= i__1 ? i__1 : s_rnge("first", i__1,
		 "zzholdd_", (ftnlen)383)] = TRUE_;
    } else {

/*        Unknown value for 'OP'. Signal an error. */

	*value = 0.;
	*ok = FALSE_;
	chkin_("ZZHOLDD", (ftnlen)7);
	setmsg_("Unknown operation. Confirm the OP value # exists in the zzh"
		"oldd.inc parameter file.", (ftnlen)83);
	errint_("#", op, (ftnlen)1);
	sigerr_("SPICE(UNKNOWNOP)", (ftnlen)16);
	chkout_("ZZHOLDD", (ftnlen)7);
	return 0;
    }
    return 0;
} /* zzholdd_ */

