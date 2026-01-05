/* zzbodini.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure ZZBODINI ( Private --- Body-Code Hash Initialization ) */
/* Subroutine */ int zzbodini_(char *names, char *nornam, integer *codes, 
	integer *nvals, integer *maxval, integer *bnmlst, integer *bnmpol, 
	char *bnmnms, integer *bnmidx, integer *bidlst, integer *bidpol, 
	integer *bidids, integer *bididx, ftnlen names_len, ftnlen nornam_len,
	 ftnlen bnmnms_len)
{
    integer item;
    extern /* Subroutine */ int zzhscadd_(integer *, integer *, char *, char *
	    , integer *, logical *, ftnlen, ftnlen), zzhsiadd_(integer *, 
	    integer *, integer *, integer *, integer *, logical *), zzhscini_(
	    integer *, integer *, integer *), zzhsiini_(integer *, integer *, 
	    integer *);
    integer i__;
    extern /* Subroutine */ int chkin_(char *, ftnlen), errch_(char *, char *,
	     ftnlen, ftnlen), sigerr_(char *, ftnlen), chkout_(char *, ftnlen)
	    , setmsg_(char *, ftnlen), errint_(char *, integer *, ftnlen);
    logical new__;

/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines.  Users should not call this routine directly due */
/*     to the volatile nature of this routine. */

/*     Initialize the name-based and ID-based hashes used for efficient */
/*     access to body-name mapping arrays. This routine should be called */
/*     by ZZBODTRN and ZZBODKER only. */

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

/*     This include file lists the parameter collection */
/*     defining the number of SPICE ID -> NAME mappings. */

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

/*     MAXL        is the maximum length of a body name. */

/*     MAXP        is the maximum number of additional names that may */
/*                 be added via the ZZBODDEF interface. */

/*     NPERM       is the count of the mapping assignments built into */
/*                 SPICE. */

/*     MAXE        is the size of the lists and hashes storing combined */
/*                 built-in and ZZBODDEF-defined name/ID mappings. To */
/*                 ensure efficient hashing this size is the set to the */
/*                 first prime number greater than ( MAXP + NPERM ). */

/*     NROOM       is the size of the lists and hashes storing the */
/*                 POOL-defined name/ID mappings. To ensure efficient */
/*                 hashing and to provide the ability to store nearly as */
/*                 many names as can fit in the POOL, this size is */
/*                 set to the first prime number less than MAXLIN */
/*                 defined in the POOL umbrella routine. */

/* $ Required_Reading */

/*     naif_ids.req */

/* $ Keywords */

/*     BODY */
/*     CONVERSION */

/* $ Author_and_Institution */

/*     B.V. Semenov (JPL) */
/*     E.D. Wright  (JPL) */

/* $ Version */

/* -    SPICELIB Version 2.0.0, 10-DEC-2021 (BVS)(EDW) */

/*        Increased NROOM to 14983. Added a comment note explaining */
/*        NROOM and MAXE */

/* -    SPICELIB Version 1.0.0, 20-MAY-2010 (EDW) */

/*        N0064 version with MAXP = 150, NPERM = 563, */
/*        MAXE = MAXP + NPERM, and NROOM = 2000. */

/*     A script generates this file. Do not edit by hand. */
/*     Edit the creation script to modify the contents of */
/*     ZZBODTRN.INC. */


/*     Maximum size of a NAME string */


/*     Maximum number of additional names that may be added via the */
/*     ZZBODDEF interface. */


/*     Count of default SPICE mapping assignments. */


/*     Size of the lists and hashes storing the built-in and */
/*     ZZBODDEF-defined name/ID mappings. To ensure efficient hashing */
/*     this size is the set to the first prime number greater than */
/*     ( MAXP + NPERM ). */


/*     Size of the lists and hashes storing the POOL-defined name/ID */
/*     mappings. To ensure efficient hashing and to provide the ability */
/*     to store nearly as many names as can fit in the POOL, this size */
/*     is set to the first prime number less than MAXLIN defined in */
/*     the POOL umbrella routine. */

/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     NAMES      I   Array of names */
/*     NORNAM     I   Array of normalized names */
/*     CODES      I   Array of ID codes for NAMES/NORNAM */
/*     NVALS      I   Length of NAMES, NORNAM, and CODES arrays */
/*     MAXVAL     I   Size of the hash arrays */
/*     BNMLST     O   Body name-based hash head node pointer list */
/*     BNMPOL     O   Body name-based hash node collision list */
/*     BNMNMS     O   Body name-based hash item list */
/*     BNMIDX     O   Body name-based hash index storage array */
/*     BIDLST     O   Body ID-based hash head node pointer list */
/*     BIDPOL     O   Body ID-based hash node collision list */
/*     BIDIDS     O   Body ID-based hash item list */
/*     BIDIDX     O   Body ID-based hash index storage array */
/*     LBPOOL     P   Lower bound of hash pool arrays */
/*     MAXL       P   Maximum length of body name strings */

/* $ Detailed_Input */

/*     NAMES     is the array of body names. This array is parallel to */
/*               NORNAM and CODES. */

/*     NORNAM    is the array of normalized body names, made from */
/*               elements of NAMES by upper-casing, left-justifying, and */
/*               compressing groups of spaces to a single space. This */
/*               represents the canonical member of the equivalence */
/*               class each parallel entry in NAMES belongs. */

/*     CODES     is the array of body codes extracted. This array is */
/*               parallel to NAMES and NORNAM. */

/*     NVALS     is the number of items contained in NAMES, NORNAM, */
/*               CODES. */

/*     MAXVAL    is the output hash size. */

/* $ Detailed_Output */

/*     All output arrays must be declared with the dimension MAXVAL. */
/*     MAXVAL must be greater than or equal to NVALS. */

/*     BNMLST */
/*     BNMPOL */
/*     BNMNMS    are the body name-based hash head node pointer, node */
/*               collision, and item lists. Together they return the */
/*               index of the element in the BNMIDX index storage array */
/*               that stores the index of the body items in the input */
/*               storage arrays. */

/*     BNMIDX    is the body name-based hash index storage array */
/*               containing at the index determined by the hash for a */
/*               given normalized name the index corresponding to this */
/*               name in the NAMES, NORNAM, and CODES arrays. */

/*     BIDLST */
/*     BIDPOL */
/*     BIDIDS    are the body ID-based hash head node pointer, node */
/*               collision, and item lists. Together they return the */
/*               index of the element in the BNMIDX index storage array */
/*               that stores the index of the body items in the input */
/*               storage arrays. */

/*     BIDIDX    is the body ID-based hash index storage array */
/*               containing at the index determined by the hash for a */
/*               given ID the index corresponding to the same ID in the */
/*               NAMES, NORNAM, and CODES arrays. */

/* $ Parameters */

/*     LBPOOL    is the lower bound of the hashes' collision list array. */

/*     MAXL      is the maximum length of a body name. Defined in the */
/*               include file 'zzbodtrn.inc'. */

/* $ Exceptions */

/*     1) If the input number of bodies NVALS is not less than or equal */
/*        to the size of the output hash, the error 'SPICE(BUG1)' will be */
/*        signaled. */

/*     2) If registering an ID in the output ID-based hash fails, the */
/*        error 'SPICE(BUG2)' will be signaled. */

/*     3) If registering an name in the output name-based hash fails, */
/*        the error 'SPICE(BUG3)' will be signaled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This is a utility routine used for initializing the hashes */
/*     facilitating efficient body name-ID translation in ZZBODTRN. */

/*     The order of mapping in the input arrays determines the priority, */
/*     with the mapping with the lowest priority being first and the */
/*     mapping with the highest priority being last. */

/*     If more than one entry with a particular normalized name is */
/*     present in the input arrays, only the latest entry is registered */
/*     in the name-based hash. */

/*     If more than one entry with a particular ID is present in the */
/*     input arrays, only the latest entry that maps to a not-yet */
/*     registered normalized name is registered in the ID-based hash. */
/*     Registering IDs only for not-yet registered names achieves masking */
/*     all IDs with the lower priority in cases when a single normalized */
/*     name maps to more than one ID. */

/* $ Examples */

/*     See the routine ZZBODTRN. */

/* $ Restrictions */

/*     1) This routine is intended only for use by ZZBODTRN and */
/*        ZZBODKER. */

/*     2) All output hash arrays must be declared with the same dimension */
/*        which is greater than or equal to MAXVAL. */

/*     3) The order of mappings in the input arrays determines the */
/*        priority, with the mapping with the lowest priority being */
/*        the first and the mapping with the highest priority being */
/*        the last. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     B.V. Semenov       (JPL) */
/*     M.J. Spencer       (JPL) */
/*     W.L. Taber         (JPL) */
/*     F.S. Turner        (JPL) */
/*     E.D. Wright        (JPL) */

/* $ Version */

/* -    SPICELIB Version 4.0.0, 16-SEP-2013 (BVS) */

/*        Changed routine's calling sequence by dropping name and ID */
/*        order vectors and adding name- and ID-based hashes and */
/*        modified it to initialize hashes instead of the order arrays. */

/* -    SPICELIB Version 3.0.0, 23-AUG-2002 (FST) */

/*        Implemented changes to support the new precedence */
/*        system. */

/*        Altered the calling sequence of ZZBODINI to remove */
/*        unused arguments.  This routine also no longer computes */
/*        NORNAM from NAMES, since it is used in a more general */
/*        capacity. */

/*        Updated module header and comments to document additional */
/*        assumptions this module now makes about its inputs. */

/*        This routine is now error free. */

/* -    SPICELIB Version 2.1.1, 07-MAR-2002 (EDW) */

/*        Modified error logic to allow duplicate */
/*        NAME -> CODE mappings without signaling an error. */
/*        The mapping operation is a no-op, but might */
/*        cause a user problems if an error signals. */

/* -    SPICELIB Version 2.1.0, 12-AUG-2001 (EDW) */

/*        Modified logic for all ZZBOD routines to function with */
/*        equivalence class concept. A body name now exists */
/*        as a member of an equivalence class named by the */
/*        normalized form of the body name. To facilitate this */
/*        concept, an addition name vector, NORNAM, and */
/*        order vector, ORDNOM, now exist. */

/* -    SPICELIB Version 1.0.0, 25-SEP-1995 (BVS) (WLT) */

/*        Renamed to ZZBODINI and filled out the comments on what this */
/*        routine does and how it works. */

/* -& */

/*     Local Variables */


/*     Consistency check. */

    if (*maxval < *nvals) {
	chkin_("ZZBODINI", (ftnlen)8);
	setmsg_("There is an inconsistency between the number of input bodie"
		"s and the size of the output hashes. The number of input bod"
		"ies was #. The size of the output hashes was #.", (ftnlen)166)
		;
	errint_("#", nvals, (ftnlen)1);
	errint_("#", maxval, (ftnlen)1);
	sigerr_("SPICE(BUG1)", (ftnlen)11);
	chkout_("ZZBODINI", (ftnlen)8);
	return 0;
    }

/*     Initialize output hashes. */

    zzhsiini_(maxval, bidlst, bidpol);
    zzhscini_(maxval, bnmlst, bnmpol);

/*     Loop through the input arrays to populate hashes. We do it */
/*     backwards to pick and register only the highest priority (latest) */
/*     values for each normalized name. */

    for (i__ = *nvals; i__ >= 1; --i__) {

/*        Register this normalized name, but only if it is not already */
/*        in the hash. */

	zzhscadd_(bnmlst, bnmpol, bnmnms, nornam + (i__ - 1) * 36, &item, &
		new__, (ftnlen)36, (ftnlen)36);
	if (new__) {
	    if (item != 0) {
		bnmidx[item - 1] = i__;
	    } else {
		chkin_("ZZBODINI", (ftnlen)8);
		setmsg_("Could not add name # to the hash.", (ftnlen)33);
		errch_("#", nornam + (i__ - 1) * 36, (ftnlen)1, (ftnlen)36);
		sigerr_("SPICE(BUG3)", (ftnlen)11);
		chkout_("ZZBODINI", (ftnlen)8);
	    }
	}

/*        We may have a situation when a single normalized name maps to */
/*        more than one ID. In such cases we want to completely mask all */
/*        IDs with the lower priority. This is easy to do by simply not */
/*        attempting to register any more IDs if the name is already */
/*        registered. */

	if (new__) {

/*           Register this ID, but only if it is not already in the hash. */

	    zzhsiadd_(bidlst, bidpol, bidids, &codes[i__ - 1], &item, &new__);
	    if (new__) {
		if (item != 0) {
		    bididx[item - 1] = i__;
		} else {
		    chkin_("ZZBODINI", (ftnlen)8);
		    setmsg_("Could not add ID # to the hash.", (ftnlen)31);
		    errint_("#", &codes[i__ - 1], (ftnlen)1);
		    sigerr_("SPICE(BUG2)", (ftnlen)11);
		    chkout_("ZZBODINI", (ftnlen)8);
		    return 0;
		}
	    }
	}
    }
    return 0;
} /* zzbodini_ */

