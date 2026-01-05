/* pckr03.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;

/* $Procedure PCKR03 ( PCK, read record from type 3 segment ) */
/* Subroutine */ int pckr03_(integer *handle, doublereal *descr, doublereal *
	et, doublereal *record)
{
    integer ends, indx;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    logical found;
    doublereal value;
    extern /* Subroutine */ int errdp_(char *, doublereal *, ftnlen), sgfcon_(
	    integer *, doublereal *, integer *, integer *, doublereal *), 
	    sigerr_(char *, ftnlen), chkout_(char *, ftnlen), sgfpkt_(integer 
	    *, doublereal *, integer *, integer *, doublereal *, integer *), 
	    sgfrvi_(integer *, doublereal *, doublereal *, doublereal *, 
	    integer *, logical *), setmsg_(char *, ftnlen);
    extern logical return_(void);

/* $ Abstract */

/*     Read a single PCK data record from a segment of type 03. */

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

/*     PCK */

/* $ Keywords */

/*     PCK */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     HANDLE     I   File handle for a PCK file. */
/*     DESCR      I   Descriptor for a type 03 PCK segment. */
/*     ET         I   Target epoch for orientation information. */
/*     RECORD     O   Data record associated with epoch ET. */

/* $ Detailed_Input */

/*     HANDLE   is the file handle for a type 03 PCK segment. */

/*     DESCR    is the segment descriptor for a type 03 PCK segment. */

/*     ET       is a target epoch, for which a data record from */
/*              the specified segment is required. */

/* $ Detailed_Output */

/*     RECORD   is the record from the specified segment which, */
/*              when evaluated at epoch ET, will give the RA, DEC, */
/*              W and body fixed angular rates for the body associated */
/*              with the segment. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  It is assumed that the descriptor and handle supplied are */
/*         for a properly constructed type 03 segment. No checks are */
/*         performed to ensure this. */

/*     2)  If the input ET value is not within the range specified */
/*         in the segment descriptor, the error SPICE(TIMEOUTOFBOUNDS) */
/*         is signaled. */

/*     3)  If any issue is detected while reading the PCK data, an error */
/*         is signaled by a routine in the call tree of this routine. */

/* $ Files */

/*     See argument HANDLE. */

/* $ Particulars */

/*     This subroutine reads a type 03 PCK record from the segment */
/*     specified by HANDLE and DESCR. The record read will contain */
/*     sufficient information to to compute RA, DEC, W and body fixed */
/*     angular rates for the body associated with the segment for epoch */
/*     ET. */

/*     See the PCK Required Reading file for a description of the */
/*     structure of a type 03 PCK segment. */

/* $ Examples */

/*     The data returned by the PCKRnn routine is in its rawest form, */
/*     taken directly from the segment. As such, it will be meaningless */
/*     to a user unless he/she understands the structure of the data type */
/*     completely. Given that understanding, however, the PCKRnn */
/*     routines might be used to "dump" and check segment data for a */
/*     particular epoch. */


/*     C */
/*     C     Get a segment applicable to a specified body and epoch. */
/*     C */
/*           CALL PCKSFS ( BODY, ET, HANDLE, DESCR, IDENT, FOUND ) */

/*     C */
/*     C     Look at parts of the descriptor. */
/*     C */
/*           CALL DAFUS ( DESCR, 2, 6, DCD, ICD ) */
/*           CENTER = ICD( 2 ) */
/*           REF    = ICD( 3 ) */
/*           TYPE   = ICD( 4 ) */

/*           IF ( TYPE .EQ. 03 ) THEN */
/*              CALL PCKR03 ( HANDLE, DESCR, ET, RECORD ) */
/*                  . */
/*                  .  Look at the RECORD data. */
/*                  . */
/*           END IF */

/* $ Restrictions */

/*     1)  It is assumed that the descriptor and handle supplied are */
/*         for a properly constructed type 03 segment. No checks are */
/*         performed to ensure this. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     K.R. Gehringer     (JPL) */
/*     E.D. Wright        (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.2, 12-AUG-2021 (JDR) */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 1.0.1, 03-JAN-2014 (EDW) */

/*        Minor edits to $Procedure; clean trailing whitespace. */

/* -    SPICELIB Version 1.0.0, 20-SEP-1995 (KRG) */

/* -& */
/* $ Index_Entries */

/*     read record from type_03 PCK segment */

/* -& */

/*     SPICELIB functions */


/*     Local Parameters */

/*     The number of constant values stored with a type 03 segment */
/*     segment. */


/*     The beginning location in the output record for the non-constant */
/*     segment data. */


/*     Local Variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("PCKR03", (ftnlen)6);
    }

/*     Check the request time against the time bounds in the segment */
/*     descriptor. In order to get the right data back from the generic */
/*     segment calls below, we need to be sure that the desired epoch */
/*     falls within the bounds of the segment, as specified by the */
/*     descriptor. The first two elements of the descriptor are the start */
/*     time for the segment and the stop time for the segment, */
/*     respectively. */

    if (*et < descr[0] || *et > descr[1]) {
	setmsg_("Request time # is outside of descriptor bounds # : #.", (
		ftnlen)53);
	errdp_("#", et, (ftnlen)1);
	errdp_("#", descr, (ftnlen)1);
	errdp_("#", &descr[1], (ftnlen)1);
	sigerr_("SPICE(TIMEOUTOFBOUNDS)", (ftnlen)22);
	chkout_("PCKR03", (ftnlen)6);
	return 0;
    }

/*     Fetch the constants and store them in the first part of */
/*     the output RECORD. */

    sgfcon_(handle, descr, &c__1, &c__1, record);

/*     Locate the time in the file less than or equal to the input ET. */

    sgfrvi_(handle, descr, et, &value, &indx, &found);

/*     Fetch the data record. */

    sgfpkt_(handle, descr, &indx, &indx, &record[1], &ends);
    chkout_("PCKR03", (ftnlen)6);
    return 0;
} /* pckr03_ */

