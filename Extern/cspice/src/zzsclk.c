/* zzsclk.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__10 = 10;
static integer c__2 = 2;
static integer c__7 = 7;

/* $Procedure    ZZSCLK ( Is there and SCLK for a CKID ) */
logical zzsclk_(integer *ckid, integer *sclkid)
{
    /* Initialized data */

    static logical first = TRUE_;

    /* System generated locals */
    address a__1[2];
    integer i__1, i__2[2], i__3;
    logical ret_val;

    /* Builtin functions */
    /* Subroutine */ int s_cat(char *, char **, integer *, integer *, ftnlen);
    integer s_rnge(char *, integer, char *, integer), s_cmp(char *, char *, 
	    ftnlen, ftnlen);

    /* Local variables */
    char sclk[32], type__[32];
    integer i__, n;
    extern integer cardi_(integer *);
    extern logical elemi_(integer *, integer *);
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    char agent[32];
    logical watch, found;
    extern integer sizei_(integer *);
    static integer known[16];
    logical keepid;
    static integer passed[16];
    logical update;
    extern /* Subroutine */ int chkout_(char *, ftnlen), dtpool_(char *, 
	    logical *, integer *, char *, ftnlen, ftnlen);
    static integer dtsize[7];
    extern /* Subroutine */ int cvpool_(char *, logical *, ftnlen);
    char sclkvr[32*7];
    extern /* Subroutine */ int ssizei_(integer *, integer *), removi_(
	    integer *, integer *), insrti_(integer *, integer *);
    extern logical return_(void);
    extern /* Subroutine */ int intstr_(integer *, char *, ftnlen), swpool_(
	    char *, integer *, char *, ftnlen, ftnlen);

/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines.  Users should not call this routine directly due */
/*     to the volatile nature of this routine. */

/*     Determine whether or not an SCLK kernel is available for mapping */
/*     ET to Ticks and back again for a particular C-kernel ID. */

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

/*     UTITILITY */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     CKID       I   CK ID-code for the object of interest. */
/*     SCLKID     I   Idcode to pass to the SCLOCK routines for CKID */

/*     The function returns TRUE is SCLK information is available. */

/* $ Detailed_Input */

/*     CKID       is the C-kernel ID-code for some object for which */
/*                and SCLK is required. */

/*     SCLKID     is the ID-code to pass to SCE2C to convert ET times */
/*                to ticks. */

/* $ Detailed_Output */

/*     The function returns TRUE if an SCLK specification is present */
/*     in the kernel pool that is suitable for mapping ticks to ET and */
/*     back for the C-kernel object specified by CKID.  If such */
/*     information is not available, or is deemed to be corrupt or */
/*     incomplete, the function returns FALSE. */

/* $ Parameters */

/*     None. */

/* $ Files */

/*     None. */

/* $ Exceptions */

/*     None. */

/* $ Particulars */

/*     This is a utility for checking that sufficient SCLK information */
/*     is available for mapping between ET and SCLK for the object */
/*     specified by CKID */

/* $ Examples */

/*     None. */

/* $ Restrictions */

/*     None. */

/* $ Author_and_Institution */

/*     W.L. Taber      (JPL) */

/* $ Literature_References */

/*     None. */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 15-AUG-2000 (WLT) */

/*        Removed the check fo the SCLK Time system as it is not */
/*        formally required for an SCLK specification to be complete. */

/* -    SPICELIB Version 1.0.0, 17-FEB-2000 (WLT) */

/* -& */
/* $ Index_Entries */

/*     Determine whether a file specifies and SCLK */

/* -& */
/*     SPICELIB Functions */


/*     Local Variables */

    ret_val = FALSE_;

/*     Standard SPICE error handling. */

    if (return_()) {
	return ret_val;
    }
    chkin_("ZZSCLK", (ftnlen)6);
    if (first) {
	first = FALSE_;
	dtsize[0] = 1;
	dtsize[1] = 1;
	dtsize[2] = 1;
	dtsize[3] = 1;
	dtsize[4] = 3;
	dtsize[5] = 1;
	dtsize[6] = 1;
	ssizei_(&c__10, known);
	ssizei_(&c__10, passed);
    }

/*     We've got a text kernel (or meta kernel). See if there is an */
/*     SCLK kernel loaded for the CKID provided in the calling inputs. */
/*     If not, we'll use the default -CKID/1000 for the SCLK ID. */

    i__1 = -(*sclkid);
    intstr_(&i__1, sclk, (ftnlen)32);
/* Writing concatenation */
    i__2[0] = 6, a__1[0] = "ZZSCLK";
    i__2[1] = 32, a__1[1] = sclk;
    s_cat(agent, a__1, i__2, &c__2, (ftnlen)32);

/*     See if this is an ID-code we've encountered before.  If it */
/*     is we can make use of stored knowledge about this ID-code. */

    if (elemi_(sclkid, known)) {
	watch = FALSE_;
	keepid = TRUE_;
	cvpool_(agent, &update, (ftnlen)32);
    } else if (cardi_(known) < sizei_(known)) {

/*        The SCLKID specified is not in the list of SCLKIDs for */
/*        this routine and there is room left in the pool of */
/*        SCLKIDs to keep track of one more.  Put this ID into */
/*        the list of known IDS */

	insrti_(sclkid, known);
	update = TRUE_;
	watch = TRUE_;
	keepid = TRUE_;
    } else {
	update = TRUE_;
	keepid = FALSE_;
	watch = FALSE_;
    }
    if (! update) {

/*        Nothing has changed in the kernel pool w.r.t this agent. */
/*        The test for an SCLK will not have changed either. */

	ret_val = elemi_(sclkid, passed);
	chkout_("ZZSCLK", (ftnlen)6);
	return ret_val;
    }

/*     If we are still here, we need to look in the kernel pool */
/*     to see if we have an SCLK for this object. */

/*     Construct all of the expected SCLK variables are */
/*     available for this SCLK. */

/* Writing concatenation */
    i__2[0] = 15, a__1[0] = "SCLK_DATA_TYPE_";
    i__2[1] = 32, a__1[1] = sclk;
    s_cat(sclkvr, a__1, i__2, &c__2, (ftnlen)32);
/* Writing concatenation */
    i__2[0] = 16, a__1[0] = "SCLK01_N_FIELDS_";
    i__2[1] = 32, a__1[1] = sclk;
    s_cat(sclkvr + 32, a__1, i__2, &c__2, (ftnlen)32);
/* Writing concatenation */
    i__2[0] = 14, a__1[0] = "SCLK01_MODULI_";
    i__2[1] = 32, a__1[1] = sclk;
    s_cat(sclkvr + 64, a__1, i__2, &c__2, (ftnlen)32);
/* Writing concatenation */
    i__2[0] = 15, a__1[0] = "SCLK01_OFFSETS_";
    i__2[1] = 32, a__1[1] = sclk;
    s_cat(sclkvr + 96, a__1, i__2, &c__2, (ftnlen)32);
/* Writing concatenation */
    i__2[0] = 20, a__1[0] = "SCLK01_COEFFICIENTS_";
    i__2[1] = 32, a__1[1] = sclk;
    s_cat(sclkvr + 128, a__1, i__2, &c__2, (ftnlen)32);
/* Writing concatenation */
    i__2[0] = 21, a__1[0] = "SCLK_PARTITION_START_";
    i__2[1] = 32, a__1[1] = sclk;
    s_cat(sclkvr + 160, a__1, i__2, &c__2, (ftnlen)32);
/* Writing concatenation */
    i__2[0] = 19, a__1[0] = "SCLK_PARTITION_END_";
    i__2[1] = 32, a__1[1] = sclk;
    s_cat(sclkvr + 192, a__1, i__2, &c__2, (ftnlen)32);

/*     If we are supposed to watch for this agent, we add him to */
/*     the list of kernel pool agents. */

    if (watch) {
	swpool_(agent, &c__7, sclkvr, (ftnlen)32, (ftnlen)32);
	cvpool_(agent, &update, (ftnlen)32);
    }

/*     Check for all of the required variables and structure in */
/*     the kernel pool. */

    for (i__ = 1; i__ <= 7; ++i__) {
	dtpool_(sclkvr + (((i__1 = i__ - 1) < 7 && 0 <= i__1 ? i__1 : s_rnge(
		"sclkvr", i__1, "zzsclk_", (ftnlen)276)) << 5), &found, &n, 
		type__, (ftnlen)32, (ftnlen)32);
	if (! found || s_cmp(type__, "N", (ftnlen)32, (ftnlen)1) != 0 || n / 
		dtsize[(i__1 = i__ - 1) < 7 && 0 <= i__1 ? i__1 : s_rnge(
		"dtsize", i__1, "zzsclk_", (ftnlen)278)] * dtsize[(i__3 = i__ 
		- 1) < 7 && 0 <= i__3 ? i__3 : s_rnge("dtsize", i__3, "zzscl"
		"k_", (ftnlen)278)] != n) {

/*           We don't have adequate SCLK data for the specified */
/*           object.  Remove this AGENT from the list of agents */
/*           that have passed the test. */

	    removi_(sclkid, passed);
	    chkout_("ZZSCLK", (ftnlen)6);
	    return ret_val;
	}
    }

/*     Once we get to this point, we know we have SCLK data.  If */
/*     there is room to WATCH for this agent, */

    if (keepid) {
	insrti_(sclkid, passed);
    }

/*     As far as we can tell, everything looks ok. */

    ret_val = TRUE_;
    chkout_("ZZSCLK", (ftnlen)6);
    return ret_val;
} /* zzsclk_ */

