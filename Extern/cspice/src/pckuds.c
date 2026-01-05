/* pckuds.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__2 = 2;
static integer c__5 = 5;

/* $Procedure PCKUDS (PCK, unpack segment descriptor ) */
/* Subroutine */ int pckuds_(doublereal *descr, integer *body, integer *frame,
	 integer *type__, doublereal *first, doublereal *last, integer *begin,
	 integer *end)
{
    extern /* Subroutine */ int chkin_(char *, ftnlen), dafus_(doublereal *, 
	    integer *, integer *, doublereal *, integer *);
    integer ipart[5];
    extern logical failed_(void);
    doublereal dppart[2];
    extern /* Subroutine */ int chkout_(char *, ftnlen);
    extern logical return_(void);

/* $ Abstract */

/*     Unpack the contents of a PCK segment descriptor */

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

/*     PCK. */

/* $ Keywords */

/*     PCK */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     DESCR      I   A PCK segment descriptor. */
/*     BODY       O   The NAIF ID code for the body of the segment. */
/*     FRAME      O   The code for the inertial frame of this segment. */
/*     TYPE       O   The type of PCK segment. */
/*     FIRST      O   The first epoch for which the segment is valid. */
/*     LAST       O   The last  epoch for which the segment is valid. */
/*     BEGIN      O   Beginning DAF address of the segment. */
/*     END        O   Ending DAF address of the segment. */

/* $ Detailed_Input */

/*     DESCR    is a PCK segment descriptor. */

/* $ Detailed_Output */

/*     BODY     is the NAIF ID code for the body of the segment. */

/*     FRAME    is the SPICE ID code for the inertial frame to which */
/*              the body fixed orientation is referenced. */

/*     TYPE     is the type of PCK segment. */

/*     FIRST    is the first epoch for which the segment has */
/*              orientation data. */

/*     LAST     is the last epoch for which the segment has */
/*              orientation data. */

/*     BEGIN    is the starting address of the data associated */
/*              with this descriptor. */

/*     END      is the last address of the data associated with */
/*              this descriptor. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     None. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine extracts the contents of a PCK segment */
/*     descriptor into the components needed for reading and */
/*     evaluating the data stored in the segment. It serves */
/*     as a macro for expanding the PCK segment descriptor. */

/* $ Examples */

/*     Suppose you wished to summarize a particular PCK segment */
/*     and that you have the descriptor for that segment in hand. */
/*     The following code fragment shows how you might use this */
/*     routine to create a summary message concerning the segment. */

/*     CALL PCKUDS ( DESCR, BODY, FRAME, TYPE, FIRST, LAST ) */

/*     Convert the start and stop times to ephemeris calendar strings */

/*     CALL ETCAL ( FIRST, FSTCAL ) */
/*     CALL ETCAL ( LAST,  LSTCAL ) */

/*     WRITE (*,*) */
/*     WRITE (*,*) 'Body     : ', BODY */
/*     WRITE (*,*) 'Frame ID : ', FRAME */
/*     WRITE (*,*) 'Data Type: ', TYPE */
/*     WRITE (*,*) */
/*     WRITE (*,*) 'Segment Start : ', FSTCAL */
/*     WRITE (*,*) 'Segment Stop  : ', LSTCAL */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     E.D. Wright        (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.2, 02-OCT-2021 (JDR) */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 1.0.1, 03-JAN-2014 (EDW) */

/*        Minor edits to $Procedure; clean trailing whitespace. */
/*        Corrected order of header sections to conform to NAIF */
/*        standard. */

/* -    SPICELIB Version 1.0.0, 1994-JAN-4 (WLT) */

/* -& */
/* $ Index_Entries */

/*     Unpack and PCK segment descriptor */

/* -& */

/*     SPICELIB Functions */


/*     Local Parameters */


/*     Local Variables */


/*     Standard SPICELIB error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("PCKUDS", (ftnlen)6);
    }

/*     No judgments are made about the descriptor when we */
/*     unpack it.  If things were done right when the descriptor */
/*     was created, it should be fine now. */

    dafus_(descr, &c__2, &c__5, dppart, ipart);
    if (failed_()) {
	chkout_("PCKUDS", (ftnlen)6);
	return 0;
    }
    *body = ipart[0];
    *frame = ipart[1];
    *type__ = ipart[2];
    *begin = ipart[3];
    *end = ipart[4];
    *first = dppart[0];
    *last = dppart[1];
    chkout_("PCKUDS", (ftnlen)6);
    return 0;
} /* pckuds_ */

