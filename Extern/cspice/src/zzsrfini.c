/* zzsrfini.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__0 = 0;

/* $Procedure ZZSRFINI ( Private --- Surface-Code Hash Initialization ) */
/* Subroutine */ int zzsrfini_(char *nornam, integer *codes, integer *bodies, 
	integer *nvals, integer *maxval, integer *snmhls, integer *snmpol, 
	integer *snmidx, integer *sidhls, integer *sidpol, integer *sididx, 
	ftnlen nornam_len)
{
    /* Builtin functions */
    integer s_cmp(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    integer head, node;
    logical full;
    extern /* Subroutine */ int zzhscini_(integer *, integer *, integer *), 
	    zzhsiini_(integer *, integer *, integer *);
    integer i__;
    extern /* Subroutine */ int chkin_(char *, ftnlen), errch_(char *, char *,
	     ftnlen, ftnlen);
    logical idnew;
    extern /* Subroutine */ int cleari_(integer *, integer *);
    integer itemat;
    logical namnew, lfound;
    integer lookat;
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), setmsg_(char *, ftnlen), errint_(char *, integer *, 
	    ftnlen), cmprss_(char *, integer *, char *, char *, ftnlen, 
	    ftnlen, ftnlen);
    char sqshnm[36];
    extern integer zzhash2_(char *, integer *, ftnlen), zzhashi_(integer *, 
	    integer *);

/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines.  Users should not call this routine directly due to the */
/*     volatile nature of this routine. */

/*     Initialize the name-based and ID-based hashes used for efficient */
/*     access to surface-name mapping arrays. This routine should be */
/*     called by ZZSRFTRN and ZZSRFKER only. */

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

/*     Declare public surface name/ID mapping parameters. */

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

/*     CONVERSION */
/*     NAME */
/*     STRING */
/*     SURFACE */

/* $ Restrictions */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman      (JPL) */

/* $ Literature_References */

/*     None. */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 02-DEC-2015 (NJB) */

/* -& */

/*     Maximum number of surface name/ID mapping entries: */


/*     Maximum length of a surface name string: */


/*     End of file srftrn.inc. */

/* $ Abstract */

/*     SPICE private include file intended solely for the support of */
/*     SPICE routines. User software should not include this file */
/*     due to the volatile nature of this file. */

/*     Declare private surface name/ID mapping parameters. */

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

/*     CONVERSION */
/*     NAME */
/*     STRING */
/*     SURFACE */

/* $ Restrictions */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman      (JPL) */

/* $ Literature_References */

/*     None. */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 04-FEB-2017 (NJB) */

/*        Original version 03-DEC-2015 (NJB) */

/* -& */

/*     Size of the lists and hashes storing the POOL-defined name/ID */
/*     mappings. To ensure efficient hashing, this size is set to the */
/*     first prime number greater than MXNSRF defined in the public */
/*     include file */

/*        srftrn.inc. */


/*     Singly-linked list pool lower bound: */


/*     End of file zzsrftrn.inc. */

/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     NORNAM     I   Array of normalized surface names */
/*     CODES      I   Array of surface ID codes for NAMES/NORNAM */
/*     BODIES     I   Array of body ID codes */
/*     NVALS      I   Length of NAMES, NORNAM, and CODES arrays */
/*     MAXVAL     I   Size of the hash arrays */
/*     SNMHLS     O   Surface name-based hash head node pointer list */
/*     SNMPOL     O   Surface name-based hash node collision list */
/*     SNMIDX     O   Surface name-based hash index storage array */
/*     SIDHLS     O   Surface ID-based hash head node pointer list */
/*     SIDPOL     O   Surface ID-based hash node collision list */
/*     SIDIDX     O   Surface ID-based hash index storage array */
/*     LBSNGL     P   Lower bound of hash pool arrays */
/*     SFNMLN     P   Maximum length of surface name strings */

/* $ Detailed_Input */

/*     NORNAM    is the array of normalized surface names, made from */
/*               elements of NAMES by upper-casing, left-justifying, and */
/*               compressing groups of spaces to a single space. This */
/*               represents the canonical member of the equivalence */
/*               class to which each parallel entry in NAMES belongs. */

/*               This array is parallel to CODES and BODIES. */

/*     CODES     is the array of surface codes extracted. This array is */
/*               parallel to NAMES and NORNAM. */

/*     BODIES    is the array of body ID codes associated with the input */
/*               surface names. */

/*     NVALS     is the number of items contained in NAMES, NORNAM, */
/*               CODES. */

/*     MAXVAL    is the output hash size. */

/* $ Detailed_Output */

/*     All output arrays must be declared with the dimension MAXVAL. */
/*     MAXVAL must be greater than or equal to NVALS. */

/*     SNMHLS */
/*     SNMPOL    are the surface name-based hash head node pointer and */
/*               collision lists. Together with the arrays SNMIDX, */
/*               NORNAM and BODIES, they enable mapping pairs of */
/*               normalized surface names and body ID codes to surface */
/*               ID codes. */

/*     SNMIDX    is the surface name-based hash index storage array. It */
/*               maps nodes in the name collision list to entries in the */
/*               parallel NORNAM and BODIES arrays. */

/*     SIDHLS */
/*     SIDPOL    are the surface ID-based hash head node pointer and */
/*               collision lists. Together with the arrays SIDIDX, */
/*               CODES and BODIES, they enable mapping pairs of */
/*               surface ID codes and body ID codes to surface */
/*               names. */

/*     SIDIDX    is the surface ID-based hash index storage array. It */
/*               maps nodes in the ID collision list to entries in the */
/*               parallel CODES and BODIES arrays. */

/* $ Parameters */

/*     LBSNGL    is the lower bound of the hashes' collision list array. */

/*     SFNMLN    is the maximum length of a surface name. Defined in the */
/*               include file 'srftrn.inc'. */

/* $ Exceptions */

/*     This routine is meant to signal no errors under normal */
/*     circumstances. The only errors it can signal would be caused by */
/*     bugs. */

/*     1) If the input number of bodies NVALS is not less than or equal */
/*        to the size of the output hash, the error 'SPICE(BUG1)' will be */
/*        signaled. */

/*     2) If registering an ID in the output ID-based hash fails, the */
/*        error 'SPICE(BUG2)' will be signaled. */

/*     3) If registering a name in the output name-based hash fails, */
/*        the error 'SPICE(BUG3)' will be signaled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This is a utility routine used for initializing the hashes */
/*     facilitating efficient surface name-ID translation in ZZSRFTRN. */

/*     The order of mappings in the input arrays determines their */
/*     priority, with the mapping having the lowest priority being first */
/*     and the mapping with the highest priority being last. */

/*     If more than one entry with a particular normalized name and body */
/*     ID is present in the input arrays, only the latest entry is */
/*     registered in the name-based hash. */

/*     If more than one entry with a particular surface ID and body ID */
/*     is present in the input arrays, only the latest entry that maps */
/*     to a not-yet-registered normalized name is registered in the */
/*     ID-based hash. Registering IDs only for not-yet-registered names */
/*     achieves masking all IDs with the lower priority in cases when a */
/*     single normalized name and body ID map to more than one surface */
/*     ID. */

/* $ Examples */

/*     See the routine ZZSRFTRN. */

/* $ Restrictions */

/*     1)  This routine is intended only for use by ZZSRFTRN and */
/*         ZZSRFKER. */

/*     2)  All output hash arrays must be declared with the same */
/*         dimension which is greater than or equal to MAXVAL. */

/*     3)  The order of mappings in the input arrays determines the */
/*         priority, with the mapping with the lowest priority being the */
/*         first and the mapping with the highest priority being the */
/*         last. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     B.V. Semenov       (JPL) */
/*     M.J. Spencer       (JPL) */
/*     W.L. Taber         (JPL) */
/*     F.S. Turner        (JPL) */
/*     E.D. Wright        (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 05-NOV-2021 (NJB) */

/*        Made minor formatting changes to code. */
/*        Updated inline comments to clarify logic. */

/* -    SPICELIB Version 1.0.0, 03-DEC-2015 (NJB) (BVS) (EDW) */

/* -& */

/*     SPICELIB functions. */


/*     Hash control area items. */


/*     Local Variables */


/*     This routine uses discovery check-in. */


/*     Consistency check. */

    if (*maxval < *nvals) {
	chkin_("ZZSRFINI", (ftnlen)8);
	setmsg_("There is an inconsistency between the number of input bodie"
		"s and the size of the output hashes. The number of input bod"
		"ies was #. The size of the output hashes was #.", (ftnlen)166)
		;
	errint_("#", nvals, (ftnlen)1);
	errint_("#", maxval, (ftnlen)1);
	sigerr_("SPICE(BUG1)", (ftnlen)11);
	chkout_("ZZSRFINI", (ftnlen)8);
	return 0;
    }

/*     Initialize output hashes. Set all collision list pointers */
/*     to 0, which is the null value. */

    zzhsiini_(maxval, sidhls, sidpol);
    zzhscini_(maxval, snmhls, snmpol);
    cleari_(&sidpol[5], &sidpol[6]);
    cleari_(&snmpol[5], &snmpol[6]);

/*     Loop through the input arrays to populate hashes. We do it */
/*     backwards to pick and register only the highest priority (latest) */
/*     values for each pair of normalized surface name and body ID code. */

/*     If multiple surface ID codes are associated with a name, only */
/*     the highest priority ID is associated with the name in the */
/*     mapping data structures. */

    for (i__ = *nvals; i__ >= 1; --i__) {

/*        Register this normalized surface name and body ID, but only if */
/*        the name from this pair is not already in the hash. */

/*        We must traverse the collision list for the normalized surface */
/*        name "manually," since we have to check the body ID for each */
/*        matching name. */

/*        Use hash function to get index of the head node. We apply the */
/*        hash function to a version of the normalized name that has */
/*        all blanks compressed out. (A normalized name may have single */
/*        blanks separating words comprising the name.) */

	cmprss_(" ", &c__0, nornam + (i__ - 1) * nornam_len, sqshnm, (ftnlen)
		1, nornam_len, (ftnlen)36);
	lookat = zzhash2_(sqshnm, &snmpol[5], (ftnlen)36);
	head = snmhls[lookat - 1];

/*        Indicate name and body were not found to begin with. */

	lfound = FALSE_;
	itemat = 0;
	namnew = TRUE_;

/*        See if this normalized name and corresponding body ID are, */
/*        respectively, in the normalized name list and body ID list. */
/*        Note that the body ID list is not a parallel array to the */
/*        normalized name array: we use the name pool pointer array */
/*        SNMIDX to indicate the location of the body ID corresponding */
/*        to a name. */

	node = head;
	if (node > 0) {

/*           Start at the head node and check each normalized name saved */
/*           for this hash value until we find a name and body ID that */
/*           match or run out of items in the collision list. */

	    while(node > 0 && ! lfound) {
		lfound = s_cmp(nornam + (snmidx[node - 1] - 1) * nornam_len, 
			nornam + (i__ - 1) * nornam_len, nornam_len, 
			nornam_len) == 0 && bodies[snmidx[node - 1] - 1] == 
			bodies[i__ - 1];
		itemat = node;
		node = snmpol[node + 5];
	    }
	    namnew = ! lfound;
	}

/*        ITEMAT is the node at which a name match was found by the */
/*        above loop, if a match was found. ITEMAT is the tail node of */
/*        the list if no match was found. It is 0 if the list is empty. */

/*        ITEMAT will be used below only if the list is non-empty and */
/*        no match was found, in which case ITEMAT is non-zero. */

	if (namnew) {

/*           We need to add the current normalized name and BODY ID */
/*           to the hash. Make sure there's room. */

	    full = snmpol[4] > snmpol[5];
	    if (full) {
		chkin_("ZZSRFINI", (ftnlen)8);
		setmsg_("Could not add name # body ID # to the hash.", (
			ftnlen)43);
		errch_("#", nornam + (i__ - 1) * nornam_len, (ftnlen)1, 
			nornam_len);
		errint_("#", &bodies[i__ - 1], (ftnlen)1);
		sigerr_("SPICE(BUG2)", (ftnlen)11);
		chkout_("ZZSRFINI", (ftnlen)8);
		return 0;
	    } else {

/*              Store the item at the first free location in */
/*              the collision pool. */

		node = snmpol[4];
		++snmpol[4];
		if (head > 0) {

/*                 Link the new entry at the tail of the applicable */
/*                 collision list. The index of the tail node is ITEMAT. */

		    snmpol[itemat + 5] = node;
		} else {

/*                 Insert the new head node into the head list. */

		    snmhls[lookat - 1] = node;
		}

/*              Set the index in the data arrays for the new pool */
/*              entry. */

		snmidx[node - 1] = i__;
	    }

/*           NAMNEW indicates that the Ith normalized name and body ID */
/*           pair was not in the hash prior to the above block of code. */

/*           We may have a situation when a single normalized surface */
/*           name and body ID pair maps to more than one surface ID. In */
/*           such cases we want to completely mask all surface IDs with */
/*           the lower priority. This is easy to do by simply not */
/*           attempting to register any more surface IDs if the name was */
/*           already registered due to a higher-indexed assignment. */

/*           Register this surface ID and body ID pair, but only if it */
/*           is not already in the hash. */

/*           We must traverse the collision list for the normalized */
/*           surface name "manually," since we have to check the body ID */
/*           for each matching surface ID. */

/*           Use hash function to get index of the head node. */

	    lookat = zzhashi_(&codes[i__ - 1], &sidpol[5]);
	    head = sidhls[lookat - 1];

/*           Indicate surface ID and body were not found to begin with. */

	    lfound = FALSE_;
	    itemat = 0;
	    idnew = TRUE_;

/*           See if this surface ID and corresponding body ID are, */
/*           respectively, in the surface ID list and body ID list. */

	    node = head;
	    if (node > 0) {

/*              Start at the head node and check each surface ID saved */
/*              for this hash value until we find a surface ID and body */
/*              ID that match or run out of items in this collision */
/*              list. */

		while(node > 0 && ! lfound) {
		    lfound = codes[sididx[node - 1] - 1] == codes[i__ - 1] && 
			    bodies[sididx[node - 1] - 1] == bodies[i__ - 1];
		    itemat = node;
		    node = sidpol[node + 5];
		}
		idnew = ! lfound;
	    }

/*           ITEMAT is the node at which a surface ID code match was */
/*           found by the above loop, if a match was found. ITEMAT is */
/*           the tail node of the list if no match was found. It is 0 if */
/*           the list is empty. */

/*           ITEMAT will be used below only if the list is non-empty and */
/*           no match was found, in which case ITEMAT is non-zero. */

	    if (idnew) {

/*              We need to add the current surface ID and BODY ID */
/*              to the hash. Make sure there's room. */

		full = sidpol[4] > sidpol[5];
		if (full) {
		    chkin_("ZZSRFINI", (ftnlen)8);
		    setmsg_("Could not add surface ID # body ID # to the has"
			    "h.", (ftnlen)49);
		    errint_("#", &codes[i__ - 1], (ftnlen)1);
		    errint_("#", &bodies[i__ - 1], (ftnlen)1);
		    sigerr_("SPICE(BUG3)", (ftnlen)11);
		    chkout_("ZZSRFINI", (ftnlen)8);
		    return 0;
		} else {

/*                 Store the item at the first free location in the */
/*                 collision pool. */

		    node = sidpol[4];
		    ++sidpol[4];
		    if (head > 0) {

/*                    Link the new entry at the tail of the applicable */
/*                    collision list. The index of the tail node is */
/*                    ITEMAT. */

			sidpol[itemat + 5] = node;
		    } else {

/*                    Insert the new head node into the head list. */

			sidhls[lookat - 1] = node;
		    }

/*                 Set the index in the data arrays for the new pool */
/*                 entry. */

		    sididx[node - 1] = i__;
		}
	    }

/*           We've processed the new (surface ID, body ID) pair. */

	}

/*        We've processed the Ith mapping between (surface name, body */
/*        ID) and (surface ID, body ID). */

    }
    return 0;
} /* zzsrfini_ */

