/* spkuds.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__2 = 2;
static integer c__6 = 6;

/* $Procedure SPKUDS ( SPK - unpack segment descriptor ) */
/* Subroutine */ int spkuds_(doublereal *descr, integer *body, integer *
	center, integer *frame, integer *type__, doublereal *first, 
	doublereal *last, integer *baddrs, integer *eaddrs)
{
    extern /* Subroutine */ int chkin_(char *, ftnlen), dafus_(doublereal *, 
	    integer *, integer *, doublereal *, integer *);
    integer ipart[6];
    extern logical failed_(void);
    doublereal dppart[2];
    extern /* Subroutine */ int chkout_(char *, ftnlen);
    extern logical return_(void);

/* $ Abstract */

/*     Unpack the contents of an SPK segment descriptor. */

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

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     DESCR      I   An SPK segment descriptor. */
/*     BODY       O   The NAIF ID code for the body of the segment. */
/*     CENTER     O   The center of motion for BODY. */
/*     FRAME      O   The code for the frame of this segment. */
/*     TYPE       O   The type of SPK segment. */
/*     FIRST      O   The first epoch for which the segment is valid. */
/*     LAST       O   The last  epoch for which the segment is valid. */
/*     BADDRS     O   Beginning DAF address of the segment. */
/*     EADDRS     O   Ending DAF address of the segment. */

/* $ Detailed_Input */

/*     DESCR    is an SPK segment descriptor. */

/* $ Detailed_Output */

/*     BODY     is the NAIF ID code for the body of the segment. */

/*     CENTER   is the center of motion for BODY. */

/*     FRAME    is the SPICE integer code for the frame to which states */
/*              for the body are be referenced. */

/*     TYPE     is the type of SPK segment. */

/*     FIRST    is the first epoch for which the segment has */
/*              ephemeris data. */

/*     LAST     is the last epoch for which the segment has */
/*              ephemeris data. */

/*     BADDRS   is the starting address of the data associated */
/*              with this descriptor. */

/*     EADDRS   is the last address of the data associated with */
/*              this descriptor. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/*     1)  If the input descriptor DESCR is invalid, it's possible for */
/*         the output times to contain bit patterns that don't represent */
/*         normal double precision values. This error is not diagnosed. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine extracts the contents of an SPK segment */
/*     descriptor into the components needed for reading and */
/*     evaluating the data stored in the segment. It serves */
/*     as a macro for expanding the SPK segment descriptor. */

/* $ Examples */

/*     Suppose you wished to summarize a particular SPK segment */
/*     and that you have the descriptor for that segment in hand. */
/*     The following code fragment shows how you might use this */
/*     routine to create a summary message concerning the segment. */

/*        CALL SPKUDS ( DESCR, BODY,  CENTER, FRAME, */
/*       .              TYPE,  FIRST, LAST,   BADDR, EADDR ) */

/*     Convert the start and stop times to ephemeris calendar strings */

/*        CALL ETCAL ( FIRST, FSTCAL ) */
/*        CALL ETCAL ( LAST,  LSTCAL ) */

/*        WRITE (*,*) */
/*        WRITE (*,*) 'Body     : ', BODY */
/*        WRITE (*,*) 'Center   : ', CENTER */
/*        WRITE (*,*) 'Frame ID : ', FRAME */
/*        WRITE (*,*) 'Data Type: ', TYPE */
/*        WRITE (*,*) */
/*        WRITE (*,*) 'Segment Start : ', FSTCAL */
/*        WRITE (*,*) 'Segment Stop  : ', LSTCAL */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     K.R. Gehringer     (JPL) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 02-OCT-2021 (JDR) (NJB) */

/*        Changed output argument names "BEGIN" and "END" to "BADDRS" and */
/*        "EADDRS" for consistency with other routines. */

/*        Edited the header to comply with NAIF standard. Added entry */
/*        #1 to $Exceptions section and declared the routine error free. */

/* -    SPICELIB Version 1.0.0, 04-JAN-1994 (WLT) (KRG) */

/* -& */
/* $ Index_Entries */

/*     Unpack and SPK segment descriptor */

/* -& */

/*     Spicelib Functions */


/*     Local Parameters */

/*     Values of ND and NI for SPK files. */


/*     Local Variables */


/*     Standard introductory error handling preparations. */

    if (return_()) {
	return 0;
    }
    chkin_("SPKUDS", (ftnlen)6);

/*     No judgments are made about the descriptor when we */
/*     unpack it.  If things were done right when the descriptor */
/*     was created, it should be fine now. */

    dafus_(descr, &c__2, &c__6, dppart, ipart);
    if (failed_()) {
	chkout_("SPKUDS", (ftnlen)6);
	return 0;
    }
    *body = ipart[0];
    *center = ipart[1];
    *frame = ipart[2];
    *type__ = ipart[3];
    *baddrs = ipart[4];
    *eaddrs = ipart[5];
    *first = dppart[0];
    *last = dppart[1];
    chkout_("SPKUDS", (ftnlen)6);
    return 0;
} /* spkuds_ */

