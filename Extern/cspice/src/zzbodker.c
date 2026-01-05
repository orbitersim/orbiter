/* zzbodker.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;
static integer c__14983 = 14983;

/* $Procedure ZZBODKER ( Private --- Process Body-Name Kernel Pool Maps ) */
/* Subroutine */ int zzbodker_(char *names, char *nornam, integer *codes, 
	integer *nvals, logical *extker, integer *bnmlst, integer *bnmpol, 
	char *bnmnms, integer *bnmidx, integer *bidlst, integer *bidpol, 
	integer *bidids, integer *bididx, ftnlen names_len, ftnlen nornam_len,
	 ftnlen bnmnms_len)
{
    /* Initialized data */

    static char nbc[32] = "NAIF_BODY_CODE                  ";
    static char nbn[32] = "NAIF_BODY_NAME                  ";

    /* System generated locals */
    integer i__1, i__2, i__3;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer), s_cmp(char *, char *, 
	    ftnlen, ftnlen);

    /* Local variables */
    char type__[1*2];
    integer nsiz[2];
    extern /* Subroutine */ int zzbodini_(char *, char *, integer *, integer *
	    , integer *, integer *, integer *, char *, integer *, integer *, 
	    integer *, integer *, integer *, ftnlen, ftnlen, ftnlen);
    integer i__;
    extern /* Subroutine */ int chkin_(char *, ftnlen), errch_(char *, char *,
	     ftnlen, ftnlen);
    logical found;
    extern logical failed_(void);
    logical plfind[2];
    extern /* Subroutine */ int gcpool_(char *, integer *, integer *, integer 
	    *, char *, logical *, ftnlen, ftnlen), gipool_(char *, integer *, 
	    integer *, integer *, integer *, logical *, ftnlen), chkout_(char 
	    *, ftnlen), sigerr_(char *, ftnlen), dtpool_(char *, logical *, 
	    integer *, char *, ftnlen, ftnlen), setmsg_(char *, ftnlen), 
	    errint_(char *, integer *, ftnlen), ljucrs_(integer *, char *, 
	    char *, ftnlen, ftnlen);
    extern logical return_(void);
    integer num[2];

/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines.  Users should not call this routine directly due */
/*     to the volatile nature of this routine. */

/*     This routine processes the kernel pool vectors NAIF_BODY_NAME */
/*     and NAIF_BODY_CODE into the lists and hashes required by ZZBODTRN */
/*     to successfully compute code-name mappings. */

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

/*     BODY */

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
/*     NAMES      O   Array of kernel pool assigned names. */
/*     NORNAM     O   Array of normalized kernel pool assigned names. */
/*     CODES      O   Array of ID codes for NAMES/NORNAM. */
/*     NVALS      O   Length of NAMES, NORNAM, and CODES arrays. */
/*     EXTKER     O   Logical indicating presence of kernel pool names. */
/*     BNMLST     O   Body name-based hash head node pointer list */
/*     BNMPOL     O   Body name-based hash node collision list */
/*     BNMNMS     O   Body name-based hash item list */
/*     BNMIDX     O   Body name-based hash index storage array */
/*     BIDLST     O   Body ID-based hash head node pointer list */
/*     BIDPOL     O   Body ID-based hash node collision list */
/*     BIDIDS     O   Body ID-based hash item list */
/*     BIDIDX     O   Body ID-based hash index storage array */
/*     LBPOOL     P   Lower bound of hash pool arrays */
/*     MAXL       P   Maximum length of body name strings. */
/*     NROOM      P   Maximum length of kernel pool data vectors. */

/* $ Detailed_Input */

/*     None. */

/* $ Detailed_Output */

/*     NAMES     is the array of names extracted from the kernel pool */
/*               vector NAIF_BODY_NAME. This array is parallel to */
/*               NORNAM and CODES. */

/*     NORNAM    the array of names extracted from the kernel pool */
/*               vector NAIF_BODY_NAME.  After extraction, each entry is */
/*               converted to uppercase, and groups of spaces are */
/*               compressed to a single space. This represents the */
/*               canonical member of the equivalence class each parallel */
/*               entry in NAMES belongs. */

/*     CODES     the array of codes extracted from the kernel pool */
/*               vector NAIF_BODY_CODE.  This array is parallel to NAMES */
/*               and NORNAM. */

/*     NVALS     the number of items contained in NAMES, NORNAM, and */
/*               CODES. */

/*     EXTKER    is a logical that indicates to the caller whether any */
/*               kernel pool name-code maps have been defined. If EXTKER */
/*               is .FALSE., then the kernel pool variables */
/*               NAIF_BODY_CODE and NAIF_BODY_NAME are empty and only */
/*               the built-in and ZZBODDEF code-name mappings need */
/*               consideration. If .TRUE., then the values returned by */
/*               this module need consideration. */

/*     BNMLST */
/*     BNMPOL */
/*     BNMNMS    are the body name-based hash head node pointer, node */
/*               collision, and item lists. Together they return the */
/*               index of the element in the BNMIDX index storage array */
/*               that stores the index of the body items in the NAMES, */
/*               NORNAM, and CODES arrays. */

/*     BNMIDX    is the body name-based hash index storage array */
/*               containing at the index determined by the hash for a */
/*               given normalized name the index corresponding to this */
/*               name in the NAMES, NORNAM, and CODES arrays. */

/*     BIDLST */
/*     BIDPOL */
/*     BIDIDS    are the body ID-based hash head node pointer, node */
/*               collision, and item lists. Together they return the */
/*               index of the element in the BNMIDX index storage array */
/*               that stores the index of the body items in the */
/*               NAMES, NORNAM, and CODES arrays. */

/*     BIDIDX    is the body ID-based hash index storage array */
/*               containing at the index determined by the hash for a */
/*               given ID the index corresponding to this ID in the */
/*               NAMES, NORNAM, and CODES arrays. */

/* $ Parameters */

/*     LBPOOL    is the lower bound of the hashes' collision list array. */

/*     MAXL      is the maximum length of a body name.  Defined in the */
/*               include file 'zzbodtrn.inc'. */

/*     NROOM     is the maximum number of kernel pool data items that */
/*               can be processed from the NAIF_BODY_CODE and */
/*               NAIF_BODY_NAME lists. */

/* $ Exceptions */

/*     1) The error SPICE(MISSINGKPV) is signaled when one of the */
/*        NAIF_BODY_CODE and NAIF_BODY_NAME keywords is present in the */
/*        kernel pool and the other is not. */

/*     2) The error SPICE(KERVARTOOBIG) is signaled if one or both of */
/*        the NAIF_BODY_CODE and NAIF_BODY_NAME kernel pool vectors */
/*        have a cardinality that exceeds NROOM. */

/*     3) The error SPICE(BADDIMENSIONS) is signaled if the cardinality */
/*        of the NAIF_BODY_CODE and NAIF_BODY_NAME kernel pool vectors do */
/*        not match. */

/*     4) The error SPICE(BLANKNAMEASSIGNED) is signaled if an entry */
/*        in the NAIF_BODY_NAME kernel pool vector is a blank string. */
/*        ID codes may not be assigned to a blank string. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine examines the contents of the kernel pool, ingests */
/*     the contents of the NAIF_BODY_CODE and NAIF_BODY_NAME keywords, */
/*     and produces name/code lists and hashes that ZZBODTRN requires to */
/*     resolve code to name and name to code mappings. */

/*     The NAMES and CODES arrays stored all values provided in the */
/*     corresponding POOL variables. No attempt to remove duplicates, */
/*     change order, or do any other alterations to these arrays is made */
/*     by this routine. */

/*     The order of mapping in the NAMES, NORNAM, and CODES arrays */
/*     determines the priority, with the mapping with the lowest */
/*     priority being first and the mapping with the highest priority */
/*     being last. */

/*     If more than one entry with a particular normalized name is */
/*     present in the NORNAM array, only the latest entry is registered */
/*     in the name-based hash. */

/*     If more than one entry with a particular ID is present in the */
/*     CODES array, only the latest entry that maps to a not-yet */
/*     registered normalized name is registered in the ID-based hash. */
/*     Registering IDs only for not-yet registered names achieves masking */
/*     all IDs with the lower priority in cases when a single normalized */
/*     name maps to more than one ID. */

/* $ Examples */

/*     None. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     B.V. Semenov   (JPL) */
/*     F.S. Turner    (JPL) */
/*     E.D. Wright    (JPL) */

/* $ Version */

/* -    SPICELIB Version 2.0.0, 16-SEP-2013 (BVS) */

/*        Changed routine's calling sequence by dropping name and ID */
/*        order vectors and adding name- and ID-based hashes and */
/*        modified it to initialize hashes instead of the order arrays. */

/* -    SPICELIB Version 1.0.0, 23-AUG-2002 (EDW) (FST) */

/* -& */

/*     SPICELIB Functions */


/*     Local Parameters */


/*     Local Variables */


/*     Saved Variables */


/*     Data Statements */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("ZZBODKER", (ftnlen)8);
    }

/*     Until the code below proves otherwise, we shall assume */
/*     we lack kernel pool name/code mappings. */

    *extker = FALSE_;

/*     Check for the external body ID variables in the kernel pool. */

    gcpool_(nbn, &c__1, &c__14983, num, names, plfind, (ftnlen)32, (ftnlen)36)
	    ;
    gipool_(nbc, &c__1, &c__14983, &num[1], codes, &plfind[1], (ftnlen)32);
    if (failed_()) {
	chkout_("ZZBODKER", (ftnlen)8);
	return 0;
    }

/*     Examine PLFIND(1) and PLFIND(2) for problems. */

    if (plfind[0] != plfind[1]) {

/*        If they are not both present or absent, signal an error. */

	setmsg_("The kernel pool vector, #, used in mapping between names an"
		"d ID-codes is absent, while # is not.  This is often due to "
		"an improperly constructed text kernel.  Check loaded kernels"
		" for these keywords.", (ftnlen)199);
	if (plfind[0]) {
	    errch_("#", nbc, (ftnlen)1, (ftnlen)32);
	    errch_("#", nbn, (ftnlen)1, (ftnlen)32);
	} else {
	    errch_("#", nbn, (ftnlen)1, (ftnlen)32);
	    errch_("#", nbc, (ftnlen)1, (ftnlen)32);
	}
	sigerr_("SPICE(MISSINGKPV)", (ftnlen)17);
	chkout_("ZZBODKER", (ftnlen)8);
	return 0;
    } else if (! plfind[0]) {

/*        Return if both keywords are absent. */

	chkout_("ZZBODKER", (ftnlen)8);
	return 0;
    }

/*     If we reach here, then both kernel pool variables are present. */
/*     Perform some simple sanity checks on their lengths. */

    dtpool_(nbn, &found, nsiz, type__, (ftnlen)32, (ftnlen)1);
    dtpool_(nbc, &found, &nsiz[1], type__ + 1, (ftnlen)32, (ftnlen)1);
    if (failed_()) {
	chkout_("ZZBODKER", (ftnlen)8);
	return 0;
    }
    if (nsiz[0] > 14983 || nsiz[1] > 14983) {
	setmsg_("The kernel pool vectors used to define the names/ID-codes m"
		"appingexceeds the max size. The size of the NAME vector is #"
		"1. The size of the CODE vector is #2. The max number allowed"
		" of elements is #3.", (ftnlen)198);
	errint_("#1", nsiz, (ftnlen)2);
	errint_("#2", &nsiz[1], (ftnlen)2);
	errint_("#3", &c__14983, (ftnlen)2);
	sigerr_("SPICE(KERVARTOOBIG)", (ftnlen)19);
	chkout_("ZZBODKER", (ftnlen)8);
	return 0;
    } else if (nsiz[0] != nsiz[1]) {
	setmsg_("The kernel pool vectors used for mapping between names and "
		"ID-codes are not the same size.  The size of the name vector"
		", NAIF_BODY_NAME is #. The size of the ID-code vector, NAIF_"
		"BODY_CODE is #. You need to examine the ID-code kernel you l"
		"oaded and correct the mismatch.", (ftnlen)270);
	errint_("#", nsiz, (ftnlen)1);
	errint_("#", &nsiz[1], (ftnlen)1);
	sigerr_("SPICE(BADDIMENSIONS)", (ftnlen)20);
	chkout_("ZZBODKER", (ftnlen)8);
	return 0;
    }

/*     Compute the canonical member of the equivalence class of NAMES, */
/*     NORNAM. This normalization compresses groups of spaces into a */
/*     single space, left justifies the string, and upper-cases the */
/*     contents.  While passing through the NAMES array, look for any */
/*     blank strings and signal an appropriate error. */

    *nvals = num[0];
    i__1 = *nvals;
    for (i__ = 1; i__ <= i__1; ++i__) {

/*        Check for blank strings. */

	if (s_cmp(names + ((i__2 = i__ - 1) < 14983 && 0 <= i__2 ? i__2 : 
		s_rnge("names", i__2, "zzbodker_", (ftnlen)403)) * 36, " ", (
		ftnlen)36, (ftnlen)1) == 0) {
	    setmsg_("An attempt to assign the code, #, to a blank string was"
		    " made.  Check loaded text kernels for a blank string in "
		    "the NAIF_BODY_NAME array.", (ftnlen)136);
	    errint_("#", &i__, (ftnlen)1);
	    sigerr_("SPICE(BLANKNAMEASSIGNED)", (ftnlen)24);
	    chkout_("ZZBODKER", (ftnlen)8);
	    return 0;
	}

/*        Compute the canonical member of the equivalence class. */

	ljucrs_(&c__1, names + ((i__2 = i__ - 1) < 14983 && 0 <= i__2 ? i__2 :
		 s_rnge("names", i__2, "zzbodker_", (ftnlen)419)) * 36, 
		nornam + ((i__3 = i__ - 1) < 14983 && 0 <= i__3 ? i__3 : 
		s_rnge("nornam", i__3, "zzbodker_", (ftnlen)419)) * 36, (
		ftnlen)36, (ftnlen)36);
    }

/*     Populate hashes required by ZZBODTRN. */

    zzbodini_(names, nornam, codes, nvals, &c__14983, bnmlst, bnmpol, bnmnms, 
	    bnmidx, bidlst, bidpol, bidids, bididx, (ftnlen)36, (ftnlen)36, (
	    ftnlen)36);
    if (failed_()) {
	chkout_("ZZBODKER", (ftnlen)8);
	return 0;
    }

/*     We're on the home stretch if we make it to this point. Set EXTKER */
/*     to .TRUE., check out and return. */

    *extker = TRUE_;
    chkout_("ZZBODKER", (ftnlen)8);
    return 0;
} /* zzbodker_ */

