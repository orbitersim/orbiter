/* zzbodblt.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;
static integer c__692 = 692;
static integer c__2 = 2;
static integer c__3 = 3;

/* $Procedure ZZBODBLT ( Private --- Retrieve Built-In Body-Code Maps ) */
/* Subroutine */ int zzbodblt_0_(int n__, integer *room, char *names, char *
	nornam, integer *codes, integer *nvals, char *device, char *reqst, 
	ftnlen names_len, ftnlen nornam_len, ftnlen device_len, ftnlen 
	reqst_len)
{
    /* Initialized data */

    static logical first = TRUE_;

    /* System generated locals */
    address a__1[2], a__2[3];
    integer i__1, i__2, i__3[2], i__4[3];

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer);
    /* Subroutine */ int s_cat(char *, char **, integer *, integer *, ftnlen);

    /* Local variables */
    integer i__;
    extern /* Subroutine */ int chkin_(char *, ftnlen), ucase_(char *, char *,
	     ftnlen, ftnlen), movec_(char *, integer *, char *, ftnlen, 
	    ftnlen), movei_(integer *, integer *, integer *);
    extern logical eqstr_(char *, char *, ftnlen, ftnlen);
    extern /* Subroutine */ int ljust_(char *, char *, ftnlen, ftnlen);
    char zzint[36];
    static integer bltcod[692];
    static char bltnam[36*692];
    extern /* Subroutine */ int orderc_(char *, integer *, integer *, ftnlen);
    extern integer lastnb_(char *, ftnlen);
    extern /* Subroutine */ int orderi_(integer *, integer *, integer *), 
	    sigerr_(char *, ftnlen), chkout_(char *, ftnlen);
    static char bltnor[36*692];
    extern /* Subroutine */ int wrline_(char *, char *, ftnlen, ftnlen), 
	    setmsg_(char *, ftnlen), errint_(char *, integer *, ftnlen), 
	    cmprss_(char *, integer *, char *, char *, ftnlen, ftnlen, ftnlen)
	    ;
    integer zzocod[692];
    char zzline[75];
    integer zzonam[692];
    extern logical return_(void);
    extern /* Subroutine */ int intstr_(integer *, char *, ftnlen);
    char zzrqst[4];
    extern /* Subroutine */ int zzidmap_(integer *, char *, ftnlen);

/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines.  Users should not call this routine directly due */
/*     to the volatile nature of this routine. */

/*     This is the umbrella routine that contains entry points to */
/*     access the built-in body name-code mappings. */

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

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     ROOM       I   ZZBODGET */
/*     NAMES      O   ZZBODGET */
/*     NORNAM     O   ZZBODGET */
/*     CODES      O   ZZBODGET */
/*     NVALS      O   ZZBODGET */
/*     DEVICE     I   ZZBODLST */
/*     REQST      I   ZZBODLST */

/* $ Detailed_Input */

/*     See the entry points for a discussion of their arguments. */

/* $ Detailed_Output */

/*     See the entry points for a discussion of their arguments. */

/* $ Parameters */

/*     See the include file 'zzbodtrn.inc' for the list of parameters */
/*     this routine utilizes. */

/* $ Exceptions */

/*     1) The error SPICE(BOGUSENTRY) is signaled if ZZBODBLT is */
/*        called directly. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     ZZBODBLT should never be called directly, instead access */
/*     the entry points: */

/*        ZZBODGET      Fetch the built-in body name/code list. */

/*        ZZBODLST      Output the name-ID mapping list. */

/* $ Examples */

/*     See ZZBODTRN and its entry points for details. */

/* $ Restrictions */

/*     1) No duplicate entries should appear in the built-in */
/*        BLTNAM list. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     F.S. Turner     (JPL) */
/*     B.V. Semenov    (JPL) */

/* $ Version */

/* -    SPICELIB Version 2.3.2, 01-OCT-2021 (NJB) */

/*        Corrected typos in comments. */

/* -    SPICELIB Version 2.3.1, 27-FEB-2007 (EDW) */

/*        Completed the ZZBODLST declarations section. */

/* -    SPICELIB Version 2.3.0, 17-MAR-2003 (EDW) */

/*        Added a call to ZZIDMAP to retrieve the default */
/*        mapping list. "zzbodtrn.inc" no longer */
/*        contains the default mapping list. */

/* -    SPICELIB Version 2.2.0  21-FEB-2003 (BVS) */

/*        Changed MER-A and MER-B to MER-1 and MER-2. */

/* -    SPICELIB Version 2.1.0  04-DEC-2002 (EDW) */

/*       Added new assignments to the default collection: */

/*       -226     ROSETTA */
/*        517     CALLIRRHOE */
/*        518     THEMISTO */
/*        519     MAGACLITE */
/*        520     TAYGETE */
/*        521     CHALDENE */
/*        522     HARPALYKE */
/*        523     KALYKE */
/*        524     IOCASTE */
/*        525     ERINOME */
/*        526     ISONOE */
/*        527     PRAXIDIKE */

/* -    SPICELIB Version 2.0.0, 23-AUG-2002 (FST) */

/*        Initial release.  This begins at Version 2.0.0 because */
/*        the entry point ZZBODLST was cut out of ZZBODTRN and */
/*        placed here at Version 1.0.0. */
/* -& */
/* $ Revisions */

/* -    SPICELIB Version 2.0.0, 23-AUG-2002 (FST) */

/*        The entries following this one were copied from */
/*        the version section of ZZBODTRN.  SPICELIB has */
/*        been changed to ZZBODTRN for convenience in noting */
/*        version information relevant for that module. */

/*        This was done to carry the history of body name-code */
/*        additions with this new umbrella. */

/*        Added to the collection: */
/*        -236   MESSENGER */

/* -    ZZBODTRN Version 3.2.0, 14-AUG-2002 (EDW) */

/*        Added the ZZBODKIK entry point. */

/*        Moved the NAIF_BODY_NAME/CODE to subroutine */
/*        ZZBODKER. No change in logic. */

/*        Added logic to enforce the precedence masking; */
/*        logic removes duplicate assignments of ZZBODDEF. */
/*        Removed the NAMENOTUNIQUE error block. */

/* -    ZZBODTRN Version 3.1.5, 27-NOV-2001 (EDW) */

/*        Added to the collection: */
/*        -200   CONTOUR */
/*        -146   LUNAR-A */
/*        -135   DRTS-W */

/*        Added the subroutine ZZBODLST as an entry point. */
/*        The routine outputs the current name-ID mapping */
/*        list to some output device. */

/* -    ZZBODTRN Version 3.1.0, 17-OCT-2001 (EDW) */

/*        To improve clarity, the BEGXX block initialization now */
/*        exists in the include file zzbodtrn.inc. */

/*        Removed the comments concerning the 851, 852, ... temporary */
/*        codes. */

/*        Set the WNAMES assignment to NAIF_BODY_CODE, NAIF_BODY_NAME */
/*        as a DATA statement. */

/*        Edited headers to match information in naif_ids required */
/*        reading. */

/*        Edited headers, removed typos and bad grammar, clarified */
/*        descriptions. */

/*        Added to the collection */
/*        -41    MARS EXPRESS, MEX */
/*        -44    BEAGLE 2, BEAGLE2 */
/*        -70    DEEP IMPACT IMPACTOR SPACECRAFT */
/*        -94    MO, MARS OBSERVER */
/*        -140   DEEP IMPACT FLYBY SPACECRAFT */
/*        -172   SLCOMB, STARLIGHT COMBINER */
/*        -205   SLCOLL, STARLIGHT COLLECTOR */
/*        -253   MER-A */
/*        -254   MER-B */

/*        Corrected typo, vehicle -188 should properly be MUSES-C, */
/*        previous versions listed the name as MUSES-B. */

/*        Removed from collection */
/*        -84    MARS SURVEYOR 01 LANDER */
/*        -154   EOS-PM1 */
/*        -200   PLUTO EXPRESS 1, PEX1 */
/*        -202   PLUTO EXPRESS 2, PEX2 */

/* -    ZZBODTRN Version 3.0.0, 29-MAR-2000 (WLT) */

/*        The ID codes for Cluster 1, 2, 3 and 4 were added.  The */
/*        ID coded for Pluto Express were removed.  The ID codes */
/*        for Pluto-Kuiper Express, Pluto-Kuiper Express Simulation */
/*        and Contour were added. */

/* -    ZZBODTRN Version 2.0.0, 26-JAN-1998 (EDW) */

/*        The Galileo probe ID -228 replaces the incorrect ID -344. */
/*        DSS stations 5 through 65 added to the collection. */

/*        Added to the collection */
/*        -107   TROPICAL RAINFALL MEASURING MISSION, TRMM */
/*        -154,  EOS-PM1 */
/*        -142   EOS-AM1 */
/*        -151   AXAF */
/*        -1     GEOTAIL */
/*        -13    POLAR */
/*        -21    SOHO */
/*        -8     WIND */
/*        -25    LUNAR PROSPECTOR, LPM */
/*        -116   MARS POLAR LANDER, MPL */
/*        -127   MARS CLIMATE ORBITER, MCO */
/*        -188   MUSES-C */
/*        -97    TOPEX/POSEIDON */
/*        -6     PIONEER-6, P6 */
/*        -7     PIONEER-7, P7 */
/*        -20    PIONEER-8, P8 */
/*        -23    PIONEER-10, P10 */
/*        -24    PIONEER-11, P11 */
/*        -178   NOZOMI, PLANET-B */
/*        -79    SPACE INFRARED TELESCOPE FACILITY, SIRTF */
/*        -29    STARDUST, SDU */
/*        -47    GENESIS */
/*        -48    HUBBLE SPACE TELESCOPE, HST */
/*        -200   PLUTO EXPRESS 1, PEX1 */
/*        -202   PLUTO EXPRESS 2, PEX2 */
/*        -164   YOHKOH, SOLAR-A */
/*        -165   MAP */
/*        -166   IMAGE */
/*        -53    MARS SURVEYOR 01 ORBITER */
/*         618   PAN */
/*         716   CALIBAN */
/*         717   SYCORAX */
/*        -30    DS-1 (low priority) */
/*        -58    HALCA */
/*        -150   HUYGEN PROBE, CASP */
/*        -55    ULS */

/*        Modified ZZBODC2N and ZZBODN2C so the user may load an */
/*        external IDs kernel to override or supplement the standard */
/*        collection.  The kernel must be loaded prior a call to */
/*        ZZBODC2N or ZZBODN2C. */

/* -    ZZBODTRN Version 1.1.0, 22-MAY-1996 (WLT) */

/*        Added the id-code for Comet Hyakutake, Comet Hale-Bopp, */
/*        Mars 96, Cassini Simulation, MGS Simulation. */

/* -    ZZBODTRN Version 1.0.0, 25-SEP-1995 (BVS) */

/*        Renamed umbrella subroutine and entry points to */
/*        correspond private routine convention (ZZ...). Added IDs for */
/*        tracking stations Goldstone (399001), Canberra (399002), */
/*        Madrid (399003), Usuda (399004). */

/* -    ZZBODTRN Version 2.2.0, 01-AUG-1995 (HAN) */

/*        Added the IDs for Near Earth Asteroid Rendezvous (-93), */
/*        Mars Pathfinder (-53), Ulysses (-55), VSOP (-58), */
/*        Radioastron (-59), Cassini spacecraft (-82), and Cassini */
/*        Huygens probe (-150). */
/*        Mars Observer (-94) was replaced with Mars Global */
/*        Surveyor (-94). */

/* -    ZZBODTRN Version 2.1.0, 15-MAR-1995 (KSZ) (HAN) */

/*        Two Shoemaker Levy 9 fragments were added, Q1 and P2 */
/*        (IDs 50000022 and 50000023). Two asteroids were added, */
/*        Eros and Mathilde (IDs 2000433 and 2000253). The */
/*        Saturnian satellite Pan (ID 618) was added. */

/* -    ZZBODTRN Version 2.0.0, 03-FEB-1995 (NJB) */

/*        The Galileo probe (ID -344) has been added to the permanent */
/*        collection. */

/* -    ZZBODTRN Version 1.0.0, 29-APR-1994 (MJS) */

/*        SPICELIB symbol tables are no longer used. Instead, two order */
/*        vectors are used to index the NAMES and CODES arrays. Also, */
/*        this version does not support reading body name ID pairs from a */
/*        file. */

/* -    ZZBODTRN Version 2.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    ZZBODTRN Version 2.0.0, 15-JUL-1991 (WLT) */

/*       The body id's for the Uranian satellites discovered by Voyager */
/*       were modified to conform to those established by the IAU */
/*       nomenclature committee.  In addition the id's for Gaspra and */
/*       Ida were added. */

/* -    ZZBODTRN Version 1.0.0,  7-MAR-1991 (WLT) */

/*       Some items previously considered errors were removed */
/*       and some minor modifications were made to improve the */
/*       robustness of the routines. */

/* -    ZZBODTRN Version 1.0.0, 28-JUN-1990 (JEM) */

/* -& */

/*     SPICELIB Functions */


/*     Local Parameters */


/*     Local Variables */


/*     Saved Variables */


/*     Data Statements */

    /* Parameter adjustments */
    if (names) {
	}
    if (nornam) {
	}
    if (codes) {
	}

    /* Function Body */
    switch(n__) {
	case 1: goto L_zzbodget;
	case 2: goto L_zzbodlst;
	}


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("ZZBODBLT", (ftnlen)8);
	sigerr_("SPICE(BOGUSENTRY)", (ftnlen)17);
	chkout_("ZZBODBLT", (ftnlen)8);
    }
    return 0;
/* $Procedure ZZBODGET ( Private --- Body-Code Get Built-In List ) */

L_zzbodget:
/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines.  Users should not call this routine directly due */
/*     to the volatile nature of this routine. */

/*     Retrieve a copy of the built-in body name-code mapping lists. */

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

/*     PRIVATE */
/*     BODY */

/* $ Declarations */

/*     INTEGER               ROOM */
/*     CHARACTER*(*)         NAMES  ( * ) */
/*     CHARACTER*(*)         NORNAM ( * ) */
/*     INTEGER               CODES  ( * ) */
/*     INTEGER               NVALS */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     ROOM       I   Space available in NAMES, NORNAM, and CODES. */
/*     NAMES      O   Array of built-in body names. */
/*     NORNAM     O   Array of normalized built-in body names. */
/*     CODES      O   Array of built-in ID codes for NAMES/NORNAM. */
/*     NVALS      O   Length of NAMES, NORNAM, CODES, and ORDNOM arrays. */

/* $ Detailed_Input */

/*     ROOM       is the maximum number of entries that NAMES, NORNAM, */
/*                and CODES may receive. */

/* $ Detailed_Output */

/*     NAMES      the array of built-in names.  This array is parallel */
/*                to NORNAM and CODES. */

/*     NORNAM     the array of normalized built-in body names.  This */
/*                array is computed from the NAMES array by compressing */
/*                groups of spaces into a single space, left-justifying */
/*                the name, and uppercasing the letters. */

/*     CODES      the array of built-in codes associated with NAMES */
/*                and NORNAM entries. */

/*     NVALS      the number of items returned in NAMES, NORNAM, */
/*                and CODES. */

/* $ Parameters */

/*     NPERM      the number of permanent, or built-in, body name-code */
/*                mappings. */

/* $ Exceptions */

/*     1) SPICE(BUG) is signaled if ROOM is less than NPERM, the */
/*        amount of space required to store the entire list of */
/*        body names and codes. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine simply copies it's local buffered version of the */
/*     built-in name-code mappings to the output arguments. */

/* $ Examples */

/*     See ZZBODTRN for sample usage. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     F.S. Turner     (JPL) */

/* $ Version */

/* -    SPICELIB Version 2.1.0, 17-MAR-2003 (EDW) */

/*        Added a call to ZZIDMAP to retrieve the default */
/*        mapping list. "zzbodtrn.inc" no longer */
/*        contains the default mapping list. */

/* -    SPICELIB Version 2.0.0, 23-AUG-2002 (FST) */

/* -& */

/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("ZZBODGET", (ftnlen)8);
    }

/*     On the first invocation compute the normalized forms of BLTNAM */
/*     and store them in BLTNOR. */

    if (first) {

/*        Retrieve the default mapping list. */

	zzidmap_(bltcod, bltnam, (ftnlen)36);
	for (i__ = 1; i__ <= 692; ++i__) {
	    ljust_(bltnam + ((i__1 = i__ - 1) < 692 && 0 <= i__1 ? i__1 : 
		    s_rnge("bltnam", i__1, "zzbodblt_", (ftnlen)569)) * 36, 
		    bltnor + ((i__2 = i__ - 1) < 692 && 0 <= i__2 ? i__2 : 
		    s_rnge("bltnor", i__2, "zzbodblt_", (ftnlen)569)) * 36, (
		    ftnlen)36, (ftnlen)36);
	    ucase_(bltnor + ((i__1 = i__ - 1) < 692 && 0 <= i__1 ? i__1 : 
		    s_rnge("bltnor", i__1, "zzbodblt_", (ftnlen)570)) * 36, 
		    bltnor + ((i__2 = i__ - 1) < 692 && 0 <= i__2 ? i__2 : 
		    s_rnge("bltnor", i__2, "zzbodblt_", (ftnlen)570)) * 36, (
		    ftnlen)36, (ftnlen)36);
	    cmprss_(" ", &c__1, bltnor + ((i__1 = i__ - 1) < 692 && 0 <= i__1 
		    ? i__1 : s_rnge("bltnor", i__1, "zzbodblt_", (ftnlen)571))
		     * 36, bltnor + ((i__2 = i__ - 1) < 692 && 0 <= i__2 ? 
		    i__2 : s_rnge("bltnor", i__2, "zzbodblt_", (ftnlen)571)) *
		     36, (ftnlen)1, (ftnlen)36, (ftnlen)36);
	}

/*        Do not do this again. */

	first = FALSE_;
    }

/*     Copy the contents of BLTNAM, BLTNOR, and BLTCOD to the output */
/*     arguments, but only if there is sufficient room. */

    if (*room < 692) {
	setmsg_("Insufficient room to copy the stored body name-code mapping"
		"s to the output arguments.  Space required is #, but the cal"
		"ler supplied #.", (ftnlen)134);
	errint_("#", &c__692, (ftnlen)1);
	errint_("#", room, (ftnlen)1);
	sigerr_("SPICE(BUG)", (ftnlen)10);
	chkout_("ZZBODGET", (ftnlen)8);
	return 0;
    }
    movec_(bltnam, &c__692, names, (ftnlen)36, names_len);
    movec_(bltnor, &c__692, nornam, (ftnlen)36, nornam_len);
    movei_(bltcod, &c__692, codes);
    *nvals = 692;
    chkout_("ZZBODGET", (ftnlen)8);
    return 0;
/* $Procedure ZZBODLST ( Output permanent collection to some device. ) */

L_zzbodlst:
/* $ Abstract */

/*     Output the complete list of built-in body/ID mappings to */
/*     some output device. Thw routine generates 2 lists: one */
/*     sorted by ID number, one sorted by name. */

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

/*     NONE. */

/* $ Keywords */

/*     BODY */

/* $ Declarations */

/*      CHARACTER*(*)         DEVICE */
/*      CHARACTER*(*)         REQST */

/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     DEVICE     I   Device name to receive the output. */
/*     REQST      I   Data list name to output. */

/* $ Detailed_Input */

/*     DEVICE         identifies the device to receive the */
/*                    body/ID mapping list. WRLINE performs the */
/*                    output function and so DEVICE may have */
/*                    the values 'SCREEN' (to generate a screen dump), */
/*                    'NULL' (do nothing), or a device name (a */
/*                    file, or any other name valid in a FORTRAN OPEN */
/*                    statement). */

/*     REQST          A case insensitive string indicating the data */
/*                    set to output. REQST may have the value 'ID', */
/*                    'NAME', or 'BOTH'. 'ID' outputs the name/ID mapping */
/*                    ordered by ID number from least to highest value. */
/*                    'NAME' outputs the name/ID mapping ordered by ASCII */
/*                    sort on the name string. 'BOTH' outputs both */
/*                    ordered lists. */

/* $ Detailed_Output */

/*     None. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     None. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     The entry point outputs ordered lists of the name/ID mappings */
/*     defined in ZZBODTRN. */

/* $ Examples */

/*     1. Write both sorted lists to screen. */

/*     PROGRAM X */

/*     CALL ZZBODLST( 'SCREEN', 'BOTH' ) */

/*     END */

/*     2. Write an ID number sorted list to a file, "body.txt". */

/*     PROGRAM X */

/*     CALL ZZBODLST( 'body.txt', 'ID' ) */

/*     END */

/* With SCREEN output of the form: */

/*   Total number of name/ID mappings: 414 */

/*   ID to name mappings. */
/*   -550                                 | M96 */
/*   -550                                 | MARS 96 */
/*   -550                                 | MARS-96 */
/*   -550                                 | MARS96 */
/*   -254                                 | MER-2 */
/*   -253                                 | MER-1 */

/*     ..                                   .. */

/*   50000020                             | SHOEMAKER-LEVY 9-B */
/*   50000021                             | SHOEMAKER-LEVY 9-A */
/*   50000022                             | SHOEMAKER-LEVY 9-Q1 */
/*   50000023                             | SHOEMAKER-LEVY 9-P2 */

/*   Name to ID mappings. */
/*   1978P1                               | 901 */
/*   1979J1                               | 515 */

/*     ..                                   .. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     F.S. Turner    (JPL) */
/*     E.D. Wright    (JPL) */

/* $ Version */

/* -    SPICELIB Version 2.1.2, 01-OCT-2021 (NJB) */

/*        Corrected typos in comments. */

/* -    SPICELIB Version 2.1.1, 27-FEB-2007 (EDW) */

/*        Completed the ZZBODLST declarations section. */

/* -    SPICELIB Version 2.1.0, 17-MAR-2003 (EDW) */

/*        Added a call to ZZIDMAP to retrieve the default */
/*        mapping list. "zzbodtrn.inc" no longer */
/*        contains the default mapping list. */

/* -    SPICELIB Version 2.0.0, 23-AUG-2002 (FST) */

/*        This entry point was moved into ZZBODBLT and some */
/*        variable names were changed to refer to variables */
/*        in the umbrella. */

/* -    SPICELIB Version 1.0.0, 26-NOV-2001 (EDW) */

/* -& */
    if (return_()) {
	return 0;
    } else {
	chkin_("ZZBODLST", (ftnlen)8);
    }

/*     Upper case the ZZRQST value. */

    ucase_(reqst, zzrqst, reqst_len, (ftnlen)4);
    intstr_(&c__692, zzint, (ftnlen)36);
/* Writing concatenation */
    i__3[0] = 34, a__1[0] = "Total number of name/ID mappings: ";
    i__3[1] = 36, a__1[1] = zzint;
    s_cat(zzline, a__1, i__3, &c__2, (ftnlen)75);
    wrline_(device, zzline, device_len, lastnb_(zzline, (ftnlen)75));

/*     Retrieve the current set of name/ID mappings */

    zzidmap_(bltcod, bltnam, (ftnlen)36);

/*      Branch as defined by the value of ZZRQST. 'ID' or 'BOTH'. */

    if (eqstr_(zzrqst, "ID", (ftnlen)4, (ftnlen)2) || eqstr_(zzrqst, "BOTH", (
	    ftnlen)4, (ftnlen)4)) {
	orderi_(bltcod, &c__692, zzocod);
	wrline_(device, " ", device_len, (ftnlen)1);
	wrline_(device, "ID to name mappings.", device_len, (ftnlen)20);
	for (i__ = 1; i__ <= 692; ++i__) {
	    intstr_(&bltcod[(i__2 = zzocod[(i__1 = i__ - 1) < 692 && 0 <= 
		    i__1 ? i__1 : s_rnge("zzocod", i__1, "zzbodblt_", (ftnlen)
		    820)] - 1) < 692 && 0 <= i__2 ? i__2 : s_rnge("bltcod", 
		    i__2, "zzbodblt_", (ftnlen)820)], zzint, (ftnlen)36);
/* Writing concatenation */
	    i__4[0] = 36, a__2[0] = zzint;
	    i__4[1] = 3, a__2[1] = " | ";
	    i__4[2] = 36, a__2[2] = bltnam + ((i__2 = zzocod[(i__1 = i__ - 1) 
		    < 692 && 0 <= i__1 ? i__1 : s_rnge("zzocod", i__1, "zzbo"
		    "dblt_", (ftnlen)822)] - 1) < 692 && 0 <= i__2 ? i__2 : 
		    s_rnge("bltnam", i__2, "zzbodblt_", (ftnlen)822)) * 36;
	    s_cat(zzline, a__2, i__4, &c__3, (ftnlen)75);
	    wrline_(device, zzline, device_len, lastnb_(zzline, (ftnlen)75));
	}
    }

/*     ... 'NAME' or 'BOTH'. */

    if (eqstr_(zzrqst, "NAME", (ftnlen)4, (ftnlen)4) || eqstr_(zzrqst, "BOTH",
	     (ftnlen)4, (ftnlen)4)) {
	orderc_(bltnam, &c__692, zzonam, (ftnlen)36);
	wrline_(device, " ", device_len, (ftnlen)1);
	wrline_(device, "Name to ID mappings.", device_len, (ftnlen)20);
	for (i__ = 1; i__ <= 692; ++i__) {
	    intstr_(&bltcod[(i__2 = zzonam[(i__1 = i__ - 1) < 692 && 0 <= 
		    i__1 ? i__1 : s_rnge("zzonam", i__1, "zzbodblt_", (ftnlen)
		    842)] - 1) < 692 && 0 <= i__2 ? i__2 : s_rnge("bltcod", 
		    i__2, "zzbodblt_", (ftnlen)842)], zzint, (ftnlen)36);
/* Writing concatenation */
	    i__4[0] = 36, a__2[0] = bltnam + ((i__2 = zzonam[(i__1 = i__ - 1) 
		    < 692 && 0 <= i__1 ? i__1 : s_rnge("zzonam", i__1, "zzbo"
		    "dblt_", (ftnlen)844)] - 1) < 692 && 0 <= i__2 ? i__2 : 
		    s_rnge("bltnam", i__2, "zzbodblt_", (ftnlen)844)) * 36;
	    i__4[1] = 3, a__2[1] = " | ";
	    i__4[2] = 36, a__2[2] = zzint;
	    s_cat(zzline, a__2, i__4, &c__3, (ftnlen)75);
	    wrline_(device, zzline, device_len, lastnb_(zzline, (ftnlen)75));
	}
    }
    chkout_("ZZBODLST", (ftnlen)8);
    return 0;
} /* zzbodblt_ */

/* Subroutine */ int zzbodblt_(integer *room, char *names, char *nornam, 
	integer *codes, integer *nvals, char *device, char *reqst, ftnlen 
	names_len, ftnlen nornam_len, ftnlen device_len, ftnlen reqst_len)
{
    return zzbodblt_0_(0, room, names, nornam, codes, nvals, device, reqst, 
	    names_len, nornam_len, device_len, reqst_len);
    }

/* Subroutine */ int zzbodget_(integer *room, char *names, char *nornam, 
	integer *codes, integer *nvals, ftnlen names_len, ftnlen nornam_len)
{
    return zzbodblt_0_(1, room, names, nornam, codes, nvals, (char *)0, (char 
	    *)0, names_len, nornam_len, (ftnint)0, (ftnint)0);
    }

/* Subroutine */ int zzbodlst_(char *device, char *reqst, ftnlen device_len, 
	ftnlen reqst_len)
{
    return zzbodblt_0_(2, (integer *)0, (char *)0, (char *)0, (integer *)0, (
	    integer *)0, device, reqst, (ftnint)0, (ftnint)0, device_len, 
	    reqst_len);
    }

