/* zzckcv04.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__2 = 2;
static integer c__6 = 6;

/* $Procedure ZZCKCV04 ( Private --- C-kernel segment coverage, type 04 ) */
/* Subroutine */ int zzckcv04_(integer *handle, integer *arrbeg, integer *
	arrend, integer *sclkid, doublereal *tol, char *timsys, doublereal *
	schedl, ftnlen timsys_len)
{
    /* System generated locals */
    integer i__1;
    doublereal d__1;

    /* Local variables */
    integer nrec, ends[2];
    doublereal left;
    extern /* Subroutine */ int sct2e_(integer *, doublereal *, doublereal *);
    integer i__;
    extern /* Subroutine */ int chkin_(char *, ftnlen), dafps_(integer *, 
	    integer *, doublereal *, integer *, doublereal *);
    doublereal descr[5];
    extern /* Subroutine */ int cknr04_(integer *, doublereal *, integer *), 
	    errch_(char *, char *, ftnlen, ftnlen);
    logical istdb;
    extern /* Subroutine */ int errdp_(char *, doublereal *, ftnlen);
    doublereal right;
    extern logical eqstr_(char *, char *, ftnlen, ftnlen);
    doublereal dc[2];
    integer ic[6];
    doublereal et;
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), sgfpkt_(integer *, doublereal *, integer *, integer *, 
	    doublereal *, integer *);
    doublereal values[143];
    extern integer intmax_(void);
    extern /* Subroutine */ int setmsg_(char *, ftnlen), wninsd_(doublereal *,
	     doublereal *, doublereal *);
    extern logical return_(void);

/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines.  Users should not call this routine directly due */
/*     to the volatile nature of this routine. */

/*     Determine the "window" of coverage of a type 04 C-kernel segment. */

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

/*     CK */
/*     DAF */

/* $ Keywords */

/*     CK */
/*     UTILITY */
/*     PRIVATE */

/* $ Declarations */
/* $ Abstract */

/*     Declarations of the CK data type specific and general CK low */
/*     level routine parameters. */

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

/*     CK.REQ */

/* $ Keywords */

/*     CK */

/* $ Restrictions */

/*     1) If new CK types are added, the size of the record passed */
/*        between CKRxx and CKExx must be registered as separate */
/*        parameter. If this size will be greater than current value */
/*        of the CKMRSZ parameter (which specifies the maximum record */
/*        size for the record buffer used inside CKPFS) then it should */
/*        be assigned to CKMRSZ as a new value. */

/* $ Author_and_Institution */

/*     N.J. Bachman      (JPL) */
/*     B.V. Semenov      (JPL) */

/* $ Literature_References */

/*     CK Required Reading. */

/* $ Version */

/* -    SPICELIB Version 3.0.0, 27-JAN-2014 (NJB) */

/*        Updated to support CK type 6. Maximum degree for */
/*        type 5 was updated to be consistent with the */
/*        maximum degree for type 6. */

/* -    SPICELIB Version 2.0.0, 19-AUG-2002 (NJB) */

/*        Updated to support CK type 5. */

/* -    SPICELIB Version 1.0.0, 05-APR-1999 (BVS) */

/* -& */

/*     Number of quaternion components and number of quaternion and */
/*     angular rate components together. */


/*     CK Type 1 parameters: */

/*     CK1DTP   CK data type 1 ID; */

/*     CK1RSZ   maximum size of a record passed between CKR01 */
/*              and CKE01. */


/*     CK Type 2 parameters: */

/*     CK2DTP   CK data type 2 ID; */

/*     CK2RSZ   maximum size of a record passed between CKR02 */
/*              and CKE02. */


/*     CK Type 3 parameters: */

/*     CK3DTP   CK data type 3 ID; */

/*     CK3RSZ   maximum size of a record passed between CKR03 */
/*              and CKE03. */


/*     CK Type 4 parameters: */

/*     CK4DTP   CK data type 4 ID; */

/*     CK4PCD   parameter defining integer to DP packing schema that */
/*              is applied when seven number integer array containing */
/*              polynomial degrees for quaternion and angular rate */
/*              components packed into a single DP number stored in */
/*              actual CK records in a file; the value of must not be */
/*              changed or compatibility with existing type 4 CK files */
/*              will be lost. */

/*     CK4MXD   maximum Chebychev polynomial degree allowed in type 4 */
/*              records; the value of this parameter must never exceed */
/*              value of the CK4PCD; */

/*     CK4SFT   number of additional DPs, which are not polynomial */
/*              coefficients, located at the beginning of a type 4 */
/*              CK record that passed between routines CKR04 and CKE04; */

/*     CK4RSZ   maximum size of type 4 CK record passed between CKR04 */
/*              and CKE04; CK4RSZ is computed as follows: */

/*                 CK4RSZ = ( CK4MXD + 1 ) * QAVSIZ + CK4SFT */


/*     CK Type 5 parameters: */


/*     CK5DTP   CK data type 5 ID; */

/*     CK5MXD   maximum polynomial degree allowed in type 5 */
/*              records. */

/*     CK5MET   number of additional DPs, which are not polynomial */
/*              coefficients, located at the beginning of a type 5 */
/*              CK record that passed between routines CKR05 and CKE05; */

/*     CK5MXP   maximum packet size for any subtype.  Subtype 2 */
/*              has the greatest packet size, since these packets */
/*              contain a quaternion, its derivative, an angular */
/*              velocity vector, and its derivative.  See ck05.inc */
/*              for a description of the subtypes. */

/*     CK5RSZ   maximum size of type 5 CK record passed between CKR05 */
/*              and CKE05; CK5RSZ is computed as follows: */

/*                 CK5RSZ = ( CK5MXD + 1 ) * CK5MXP + CK5MET */


/*     CK Type 6 parameters: */


/*     CK6DTP   CK data type 6 ID; */

/*     CK6MXD   maximum polynomial degree allowed in type 6 */
/*              records. */

/*     CK6MET   number of additional DPs, which are not polynomial */
/*              coefficients, located at the beginning of a type 6 */
/*              CK record that passed between routines CKR06 and CKE06; */

/*     CK6MXP   maximum packet size for any subtype.  Subtype 2 */
/*              has the greatest packet size, since these packets */
/*              contain a quaternion, its derivative, an angular */
/*              velocity vector, and its derivative.  See ck06.inc */
/*              for a description of the subtypes. */

/*     CK6RSZ   maximum size of type 6 CK record passed between CKR06 */
/*              and CKE06; CK6RSZ is computed as follows: */

/*                 CK6RSZ = CK6MET + ( CK6MXD + 1 ) * ( CK6PS3 + 1 ) */

/*              where CK6PS3 is equal to the parameter CK06PS3 defined */
/*              in ck06.inc. Note that the subtype having the largest */
/*              packet size (subtype 2) does not give rise to the */
/*              largest record size, because that type is Hermite and */
/*              requires half the window size used by subtype 3 for a */
/*              given polynomial degree. */


/*     The parameter CK6PS3 must be in sync with C06PS3 defined in */
/*     ck06.inc. */



/*     Maximum record size that can be handled by CKPFS. This value */
/*     must be set to the maximum of all CKxRSZ parameters (currently */
/*     CK5RSZ.) */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     HANDLE     I   Handle of a C-kernel open for read access */
/*     ARRBEG     I   Beginning DAF address */
/*     ARREND     I   Ending DAF address */
/*     SCLKID     I   ID of SCLK associated with segment. */
/*     TOL        I   Tolerance in ticks. */
/*     TIMSYS     I   Time system used to represent coverage. */
/*     SCHEDL    I/O  An initialized window/schedule of interval */
/*     CK4RSZ     P   C-kernel Type 04 Maximum Record Size */

/* $ Detailed_Input */

/*     HANDLE     is the handle of some DAF that is open for reading. */

/*     ARRBEG     is the beginning address of a type 04 segment */

/*     ARREND     is the ending address of a type 04 segment. */


/*     SCLKID     is the ID code of the spacecraft clock associated with */
/*                the object for which the segment contains pointing. */
/*                This is the ID code used by the SCLK conversion */
/*                routines. */

/*     TOL        is a tolerance value expressed in ticks of the */
/*                spacecraft clock associated with the segment. Before */
/*                each interval is inserted into the coverage window, */
/*                the intervals are expanded by TOL:  the left endpoint */
/*                of each interval is reduced by TOL and the right */
/*                endpoint is increased by TOL.  Any intervals that */
/*                overlap as a result of the expansion are merged. */

/*                The coverage window returned when TOL > 0 indicates */
/*                the coverage provided by the file to the CK readers */
/*                CKGPAV and CKGP when that value of TOL is passed to */
/*                them as an input. */


/*     TIMSYS     is a string indicating the time system used in the */
/*                output coverage window.  TIMSYS may have the values: */

/*                   'SCLK'    Elements of SCHEDL are expressed in */
/*                             encoded SCLK ("ticks"), where the clock */
/*                             is associated with the object designated */
/*                             by IDCODE. */

/*                   'TDB'     Elements of SCHEDL are expressed as */
/*                             seconds past J2000 TDB. */

/*                TIMSYS must be consistent with the system used for */
/*                the contents of SCHEDL on input, if any. */


/*     SCHEDL     is a schedule (window) of intervals, to which the */
/*                intervals of coverage for this segment will be added. */

/* $ Detailed_Output */

/*     SCHEDL     the input schedule updated to include the intervals */
/*                of coverage for this segment. */

/* $ Parameters */

/*     CK4RSZ     is the maximum length of a CK4 record (with angular */
/*                velocity). Defined in the include file 'ckparam.inc'. */

/* $ Files */

/*     This routine reads the contents of the file associated with */
/*     HANDLE to locate coverage intervals. */

/* $ Exceptions */

/*     1) Routines in the call tree of this routine may signal errors */
/*        if insufficient room in SCHEDL exists or other error */
/*        conditions relating to file access arise. */

/*     2) If TOL is negative, the error SPICE(VALUEOUTOFRANGE) is */
/*        signaled. */

/*     3) If TIMSYS is not recognized, the error SPICE(INVALIDOPTION) */
/*        is signaled. */

/*     4) If a time conversion error occurs, the error will be */
/*        diagnosed by a routine in the call tree of this routine. */

/* $ Particulars */

/*     This is a utility routine that determines the intervals */
/*     of coverage for a type 04 C-kernel segment. */

/* $ Examples */

/*     See CKCOV. */

/* $ Restrictions */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman    (JPL) */
/*     F.S. Turner     (JPL) */

/* $ Literature_References */

/*     None. */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 30-DEC-2004 (NJB) (FST) */

/* -& */

/*     SPICELIB Functions */


/*     Local Parameters */


/*     Local Variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }
    chkin_("ZZCKCV04", (ftnlen)8);

/*     Check tolerance value. */

    if (*tol < 0.) {
	setmsg_("Tolerance must be non-negative; actual value was #.", (
		ftnlen)51);
	errdp_("#", tol, (ftnlen)1);
	sigerr_("SPICE(VALUEOUTOFRANGE)", (ftnlen)22);
	chkout_("ZZCKCV04", (ftnlen)8);
	return 0;
    }

/*     Set a logical flag indicating whether the time systm is SCLK. */

    istdb = eqstr_(timsys, "TDB", timsys_len, (ftnlen)3);

/*     Check time system. */

    if (! istdb) {
	if (! eqstr_(timsys, "SCLK", timsys_len, (ftnlen)4)) {
	    setmsg_("Time system spec TIMSYS was #; allowed values are SCLK "
		    "and TDB.", (ftnlen)63);
	    errch_("#", timsys, (ftnlen)1, timsys_len);
	    sigerr_("SPICE(INVALIDOPTION)", (ftnlen)20);
	    chkout_("ZZCKCV04", (ftnlen)8);
	    return 0;
	}
    }

/*     Build a descriptor record that satisfies the requirements */
/*     of CKNR04 and SGFPKT. */

/*     Note: This is a hack dependent on the implementation of */
/*     the generic segments routines.  But for C-kernels it */
/*     should always work, as ND and NI aren't changing any */
/*     time soon. */

    ic[0] = intmax_();
    ic[1] = intmax_();
    ic[2] = 4;
    ic[3] = intmax_();
    ic[4] = *arrbeg;
    ic[5] = *arrend;
    dc[0] = 0.;
    dc[1] = 0.;
    dafps_(&c__2, &c__6, dc, ic, descr);

/*     Determine the number of records in the array. */

    cknr04_(handle, descr, &nrec);
    i__1 = nrec;
    for (i__ = 1; i__ <= i__1; ++i__) {

/*        Extract each packet of pointing coefficients. */

	sgfpkt_(handle, descr, &i__, &i__, values, ends);

/*        Compute the left and right end points of the interval */
/*        of coverage related to this packet. */

	left = values[0] - values[1];
	right = values[0] + values[1];

/*        Adjust the interval using the tolerance. */

	if (*tol > 0.) {
/* Computing MAX */
	    d__1 = left - *tol;
	    left = max(d__1,0.);
	    right += *tol;
	}

/*        Convert the time to TDB if necessary. */

	if (istdb) {
	    sct2e_(sclkid, &left, &et);
	    left = et;
	    sct2e_(sclkid, &right, &et);
	    right = et;
	}

/*        Store the results in the schedule. */

	wninsd_(&left, &right, schedl);
    }
    chkout_("ZZCKCV04", (ftnlen)8);
    return 0;
} /* zzckcv04_ */

