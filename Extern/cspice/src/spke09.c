/* spke09.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__6 = 6;

/* $Procedure SPKE09 ( S/P Kernel, evaluate, type 9 ) */
/* Subroutine */ int spke09_(doublereal *et, doublereal *record, doublereal *
	state)
{
    /* System generated locals */
    integer i__1;

    /* Builtin functions */
    integer i_dnnt(doublereal *), s_rnge(char *, integer, char *, integer);

    /* Local variables */
    integer i__, n;
    extern /* Subroutine */ int moved_(doublereal *, integer *, doublereal *);
    doublereal locrec[198];
    extern doublereal lgrint_(integer *, doublereal *, doublereal *, 
	    doublereal *, doublereal *);
    extern /* Subroutine */ int xposeg_(doublereal *, integer *, integer *, 
	    doublereal *);
    extern logical return_(void);
    integer xstart, ystart;

/* $ Abstract */

/*     Evaluate a single SPK data record from a segment of type 9 */
/*     (discrete states, evaluated by Lagrange interpolation). */

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

/*     SPK */

/* $ Keywords */

/*     EPHEMERIS */

/* $ Declarations */
/* $ Abstract */

/*     Declare SPK data record size.  This record is declared in */
/*     SPKPVN and is passed to SPK reader (SPKRxx) and evaluator */
/*     (SPKExx) routines. */

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

/*     SPK */

/* $ Keywords */

/*     SPK */

/* $ Restrictions */

/*     1) If new SPK types are added, it may be necessary to */
/*        increase the size of this record.  The header of SPKPVN */
/*        should be updated as well to show the record size */
/*        requirement for each data type. */

/* $ Author_and_Institution */

/*     N.J. Bachman      (JPL) */

/* $ Literature_References */

/*     None. */

/* $ Version */

/* -    SPICELIB Version 2.0.0, 05-OCT-2012 (NJB) */

/*        Updated to support increase of maximum degree to 27 for types */
/*        2, 3, 8, 9, 12, 13, 18, and 19. See SPKPVN for a list */
/*        of record size requirements as a function of data type. */

/* -    SPICELIB Version 1.0.0, 16-AUG-2002 (NJB) */

/* -& */

/*     End include file spkrec.inc */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     ET         I   Target epoch. */
/*     RECORD    I-O  Data record. */
/*     STATE      O   State (position and velocity). */

/* $ Detailed_Input */

/*     ET       is a target epoch, at which a state vector is to */
/*              be computed. */

/*     RECORD   is a data record which, when evaluated at epoch ET, */
/*              will give the state (position and velocity) of some */
/*              body, relative to some center, in some inertial */
/*              reference frame. Normally, the caller of this routine */
/*              will obtain RECORD by calling SPKR09. */

/*              The structure of the record is as follows: */

/*                 +----------------------+ */
/*                 | number of states (n) | */
/*                 +----------------------+ */
/*                 | state 1 (6 elts.)    | */
/*                 +----------------------+ */
/*                 | state 2 (6 elts.)    | */
/*                 +----------------------+ */
/*                             . */
/*                             . */
/*                             . */
/*                 +----------------------+ */
/*                 | state n (6 elts.)    | */
/*                 +----------------------+ */
/*                 | epochs 1--n          | */
/*                 +----------------------+ */

/* $ Detailed_Output */

/*     RECORD   is the input record, modified by use as a work area. */
/*              On output, RECORD no longer contains useful */
/*              information. */

/*     STATE    is the state. In order, the elements are */

/*                 X, Y, Z, X', Y', and Z' */

/*              Units are km and km/sec. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  The caller of this routine must ensure that the input record */
/*         is appropriate for the supplied ET value. Otherwise, */
/*         arithmetic overflow may result. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     The exact format and structure of type 9 (unequally spaced */
/*     discrete states, evaluated by Lagrange interpolation) segments are */
/*     described in the SPK Required Reading file. */

/* $ Examples */

/*     The SPKEnn routines are almost always used in conjunction with */
/*     the corresponding SPKRnn routines, which read the records from */
/*     SPK files. */

/*     The data returned by the SPKRnn routine is in its rawest form, */
/*     taken directly from the segment. As such, it will be meaningless */
/*     to a user unless he/she understands the structure of the data type */
/*     completely. Given that understanding, however, the SPKRnn */
/*     routines might be used to examine raw segment data before */
/*     evaluating it with the SPKEnn routines. */


/*     C */
/*     C     Get a segment applicable to a specified body and epoch. */
/*     C */
/*           CALL SPKSFS ( BODY, ET, HANDLE, DESCR, IDENT, FOUND ) */

/*     C */
/*     C     Look at parts of the descriptor. */
/*     C */
/*           CALL DAFUS ( DESCR, 2, 6, DCD, ICD ) */
/*           CENTER = ICD( 2 ) */
/*           REF    = ICD( 3 ) */
/*           TYPE   = ICD( 4 ) */

/*           IF ( TYPE .EQ. 9 ) THEN */

/*              CALL SPKR09 ( HANDLE, DESCR, ET, RECORD ) */
/*                  . */
/*                  .  Look at the RECORD data. */
/*                  . */
/*              CALL SPKE09 ( ET, RECORD, STATE ) */
/*                  . */
/*                  .  Check out the evaluated state. */
/*                  . */
/*           END IF */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     R.E. Thurman       (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.2.0, 12-AUG-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 1.1.0, 31-AUG-2005 (NJB) */

/*        Updated to remove non-standard use of duplicate arguments */
/*        in XPOSEG and LGRINT calls. */

/* -    SPICELIB Version 1.0.0, 14-AUG-1993 (NJB) (RET) */

/* -& */
/* $ Index_Entries */

/*     evaluate type_9 SPK segment */

/* -& */
/* $ Revisions */

/* -    SPICELIB Version 1.1.0, 31-AUG-2005 (NJB) */

/*        Updated to remove non-standard use of duplicate arguments */
/*        in XPOSEG and LGRINT calls. */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     Indices of input record elements: */

/*        -- size */
/*        -- start of state information */


/*     Local variables */


/*     Discovery check-in. */

    if (return_()) {
	return 0;
    }

/*     We'll transpose the state information in the input record */
/*     so that contiguous pieces of it can be shoved directly into the */
/*     interpolation routine LGRINT.  We allow LGRINT to overwrite the */
/*     state values in the input record, since this saves local storage */
/*     and does no harm.  (See the header of LGRINT for a description of */
/*     its work space usage.) */

    n = i_dnnt(record);
    xposeg_(&record[1], &c__6, &n, locrec);
    i__1 = n * 6;
    moved_(locrec, &i__1, &record[1]);

/*     We interpolate each state component in turn. */

    xstart = n * 6 + 2;
    for (i__ = 1; i__ <= 6; ++i__) {
	ystart = n * (i__ - 1) + 2;
	state[(i__1 = i__ - 1) < 6 && 0 <= i__1 ? i__1 : s_rnge("state", i__1,
		 "spke09_", (ftnlen)273)] = lgrint_(&n, &record[xstart - 1], &
		record[ystart - 1], locrec, et);
    }
    return 0;
} /* spke09_ */

