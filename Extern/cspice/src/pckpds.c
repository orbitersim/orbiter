/* pckpds.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__2 = 2;
static integer c__5 = 5;

/* $Procedure PCKPDS ( PCK, pack descriptor ) */
/* Subroutine */ int pckpds_(integer *body, char *frame, integer *type__, 
	doublereal *first, doublereal *last, doublereal *descr, ftnlen 
	frame_len)
{
    extern /* Subroutine */ int etcal_(doublereal *, char *, ftnlen), chkin_(
	    char *, ftnlen), dafps_(integer *, integer *, doublereal *, 
	    integer *, doublereal *), errch_(char *, char *, ftnlen, ftnlen), 
	    errdp_(char *, doublereal *, ftnlen);
    integer ipart[5], refcod;
    char calfst[40], callst[40];
    doublereal dppart[2];
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), irfnum_(char *, integer *, ftnlen), setmsg_(char *, 
	    ftnlen), errint_(char *, integer *, ftnlen);
    extern logical return_(void);

/* $ Abstract */

/*     Perform routine error checks and if all checks pass, pack the */
/*     descriptor for a PCK segment */

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
/*     BODY       I   The NAIF ID code for the body of the segment. */
/*     FRAME      I   The inertial frame for this segment. */
/*     TYPE       I   The type of PCK segment to create. */
/*     FIRST      I   The first epoch for which the segment is valid. */
/*     LAST       I   The last  epoch for which the segment is valid. */
/*     DESCR      O   A PCK segment descriptor. */

/* $ Detailed_Input */

/*     BODY     is the NAIF ID code for the body of the segment. */

/*     FRAME    is a string that names the inertial frame to which */
/*              states for the body shall be referenced. */

/*     TYPE     is the type of PCK segment to create. */

/*     FIRST    is the first epoch for which the segment will have */
/*              ephemeris data. */

/*     LAST     is the last epoch for which the segment will have */
/*              ephemeris data. */

/* $ Detailed_Output */

/*     DESCR    is a valid PCK segment descriptor to use */
/*              when creating a DAF segment for this body. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the value of BODY is the ID code of a barycenter --codes 0, */
/*         1, ..., 9, the error SPICE(BARYCENTERIDCODE) is signaled. */

/*     2)  If FRAME is not one of the known SPICE inertial reference */
/*         frames, the error SPICE(INVALIDREFFRAME) is signaled. */

/*     3)  If FIRST is greater than or equal to LAST, the error */
/*         SPICE(BADDESCRTIMES) is signaled. */

/*     4)  If the value of TYPE is outside the range 2 to 1000 */
/*         (inclusive), the error SPICE(UNKNOWNPCKTYPE) is signaled. This */
/*         does not ensure that the TYPE is a legitimate PCK segment */
/*         type, but it is a simple check that helps avoid problems that */
/*         arise from uninitialized values or improperly ordered calling */
/*         arguments. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This is a utility routine for validating and creating */
/*     the descriptor for a PCK segment. It is intended for */
/*     use only by routines that create PCK segments. */

/* $ Examples */

/*     Suppose that you wish to create a PCK segment of type X */
/*     and that you are writing a routine to handle the details */
/*     of the segment creation. This routine can be used to */
/*     ensure that the descriptor needed for the segment is */
/*     properly formed and that the information in that descriptor */
/*     is reasonable. */

/*     Having collected the needed information you can create the */
/*     descriptor and then begin a new segment as shown below. */

/*     CALL PCKPDS ( BODY, FRAME, TYPE, FIRST, LAST, DESCR ) */
/*     CALL DAFBNA ( HANDLE, DESCR,  SEGID ) */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     W.L. Taber         (JPL) */
/*     E.D. Wright        (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.2, 20-AUG-2021 (JDR) */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 1.0.1, 03-JAN-2014 (EDW) */

/*        Minor edits to $Procedure; clean trailing whitespace. */
/*        Corrected order of header sections to conform to NAIF */
/*        standard. */

/* -    SPICELIB Version 1.0.0, 04-JAN-1995 (WLT) */

/* -& */
/* $ Index_Entries */

/*     Validate and pack a PCK segment descriptor */

/* -& */

/*     Spicelib Functions */


/*     Local Parameters */

/*     ND and NI values for a PCK file. */


/*     Length of a calender string. */


/*     Local Variables */


/*     Standard SPICELIB error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("PCKPDS", (ftnlen)6);
    }

/*     We do not support orientation models for barycenters. */

    if (*body >= 0 && *body <= 9) {
	setmsg_("You have attempted to create a segment  for for a barycente"
		"r, and the PCK system does not support this.", (ftnlen)103);
	sigerr_("SPICE(BARYCENTERIDCODE)", (ftnlen)23);
	chkout_("PCKPDS", (ftnlen)6);
	return 0;
    }

/*     Get the NAIF integer code for the reference frame. */

    irfnum_(frame, &refcod, frame_len);
    if (refcod == 0) {
	setmsg_("The reference frame # is not supported.", (ftnlen)39);
	errch_("#", frame, (ftnlen)1, frame_len);
	sigerr_("SPICE(INVALIDREFFRAME)", (ftnlen)22);
	chkout_("PCKPDS", (ftnlen)6);
	return 0;
    }

/*     The segment stop time should be greater then the begin time. */

    if (*first >= *last) {

/*        We've got an error. Get the calendar string for the first */
/*        and last epochs. */

	etcal_(first, calfst, (ftnlen)40);
	etcal_(last, callst, (ftnlen)40);
	setmsg_("The segment start time: # (#) is at orafter the segment sto"
		"p time # (#). ", (ftnlen)73);
	errdp_("#", first, (ftnlen)1);
	errch_("#", calfst, (ftnlen)1, (ftnlen)40);
	errdp_("#", last, (ftnlen)1);
	errch_("#", callst, (ftnlen)1, (ftnlen)40);
	sigerr_("SPICE(BADDESCRTIMES)", (ftnlen)20);
	chkout_("PCKPDS", (ftnlen)6);
	return 0;
    }

/*     The type must be something reasonable.  The interval from */
/*     2 to 1000 is what we are calling reasonable these days. */

    if (*type__ <= 1 || *type__ > 1000) {
	setmsg_("The type specified, #, is not supported within the PCK syst"
		"em. ", (ftnlen)63);
	errint_("#", type__, (ftnlen)1);
	sigerr_("SPICE(UNKNOWNPCKTYPE)", (ftnlen)21);
	chkout_("PCKPDS", (ftnlen)6);
	return 0;
    }

/*     Well, that's it.  As far as we can determine these seem to be */
/*     reasonable values to put into a descriptor.   Do it. */

    ipart[0] = *body;
    ipart[1] = refcod;
    ipart[2] = *type__;
    ipart[3] = 0;
    ipart[4] = 0;
    dppart[0] = *first;
    dppart[1] = *last;
    dafps_(&c__2, &c__5, dppart, ipart, descr);
    chkout_("PCKPDS", (ftnlen)6);
    return 0;
} /* pckpds_ */

