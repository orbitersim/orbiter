/* zzbodtrn.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__853 = 853;
static integer c__1 = 1;

/* $Procedure ZZBODTRN ( Private --- Body name and code translation ) */
/* Subroutine */ int zzbodtrn_0_(int n__, char *name__, integer *code, 
	logical *found, integer *usrctr, logical *update, ftnlen name_len)
{
    /* Initialized data */

    static logical bodchg = FALSE_;
    static logical first = TRUE_;
    static logical extker = FALSE_;
    static logical nodata = TRUE_;
    static integer nwatch = 2;
    static char wnames[32*2] = "NAIF_BODY_NAME                  " "NAIF_BODY"
	    "_CODE                  ";

    /* System generated locals */
    integer i__1, i__2, i__3;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_cmp(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    extern /* Subroutine */ int zzhscchk_(integer *, integer *, char *, char *
	    , integer *, ftnlen, ftnlen), zzbodget_(integer *, char *, char *,
	     integer *, integer *, ftnlen, ftnlen), zzbodini_(char *, char *, 
	    integer *, integer *, integer *, integer *, integer *, char *, 
	    integer *, integer *, integer *, integer *, integer *, ftnlen, 
	    ftnlen, ftnlen), zzbodker_(char *, char *, integer *, integer *, 
	    logical *, integer *, integer *, char *, integer *, integer *, 
	    integer *, integer *, integer *, ftnlen, ftnlen, ftnlen), 
	    zzhsichk_(integer *, integer *, integer *, integer *, integer *), 
	    zzctrchk_(integer *, integer *, logical *), zzctrinc_(integer *), 
	    zzctrsin_(integer *), zzcvpool_(char *, integer *, logical *, 
	    ftnlen), zzctruin_(integer *);
    static integer i__, j;
    extern /* Subroutine */ int chkin_(char *, ftnlen), errch_(char *, char *,
	     ftnlen, ftnlen);
    static integer index;
    extern logical failed_(void);
    static integer defcod[853];
    static char defnam[36*853];
    static integer didids[853], dididx[853], kercod[14983], kidids[14983], 
	    codidx, didpol[859], kididx[14983];
    static char defnor[36*853], kernam[36*14983];
    static integer dnmidx[853], defsiz, didlst[853], kidpol[14989], dnmpol[
	    859], knmidx[14983], kidlst[14983];
    static char dnmnms[36*853], tmpnam[36];
    static logical lupdte;
    static char kernor[36*14983];
    static integer dnmlst[853], knmpol[14989];
    static char knmnms[36*14983];
    static integer subctr[2], kersiz, knmlst[14983], pulctr[2];
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), ljucrs_(integer *, char *, char *, ftnlen, ftnlen), 
	    setmsg_(char *, ftnlen);
    extern logical return_(void);
    extern /* Subroutine */ int errint_(char *, integer *, ftnlen), swpool_(
	    char *, integer *, char *, ftnlen, ftnlen);

/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines.  Users should not call this routine directly due */
/*     to the volatile nature of this routine. */

/*     This is the umbrella routine that contains entry points to */
/*     translate between body names and NAIF integer codes, and */
/*     for definition of new name/code pairs. */

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

/* $ Abstract */

/*     This include file defines the dimension of the counter */
/*     array used by various SPICE subsystems to uniquely identify */
/*     changes in their states. */

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

/*     CTRSIZ      is the dimension of the counter array used by */
/*                 various SPICE subsystems to uniquely identify */
/*                 changes in their states. */

/* $ Author_and_Institution */

/*     B.V. Semenov    (JPL) */

/* $ Literature_References */

/*     None. */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 29-JUL-2013 (BVS) */

/* -& */

/*     End of include file. */

/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     NAME      I/O  ZZBODN2C, ZZBODDEF, ZZBODC2N */
/*     CODE      I/O  ZZBODC2N, ZZBODDEF, ZZBODN2C */
/*     FOUND      O   ZZBODN2C and ZZBODC2N */
/*     USRCTR    I/O  ZZBCTRCK */
/*     UPDATE     O   ZZBCTRCK */
/*     MAXL       P   (All) */
/*     MAXP       P   ZZBODDEF */
/*     NPERM      P   (All) */
/*     MAXE       P   (All) */
/*     NROOM      P   (All) */
/*     CTRSIZ     P   (All) */

/* $ Detailed_Input */

/*     See the entry points for a discussion of their arguments. */

/* $ Detailed_Output */

/*     See the entry points for a discussion of their arguments. */

/* $ Parameters */

/*     These parameters are defined in zzbodtrn.inc: */

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

/*     This parameter is defined in zzctr.inc: */

/*     CTRSIZ     is the dimension of the counter array used by various */
/*                SPICE subsystems to identify changes in their states. */

/* $ Exceptions */

/*     1) The error SPICE(BOGUSENTRY) is signaled if ZZBODTRN */
/*        is called directly. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     ZZBODTRN should never be called, instead access the entry */
/*     points: */

/*        ZZBODN2C      Body name to code */

/*        ZZBODC2N      Body code to name */

/*        ZZBODDEF      Body name/code definition */

/*        ZZBODKIK      Force an examination of the kernel pool */
/*                      variables, subsequent processing and */
/*                      the generation of any error messages */
/*                      resultant from the processing. */

/*        ZZBODRST      Reset the mappings provided via the ZZBODDEF */
/*                      interface. */

/*        ZZBCTRCK      Check and, if needed, update the caller's copy */
/*                      of the ZZBODTRN state counter. */

/*     ZZBODN2C and ZZBODC2N perform translations between body names */
/*     and their corresponding integer codes used in SPK and PCK files */
/*     and associated routines.  A default set of name/code */
/*     pairs are automatically defined during the first call to */
/*     any of the entry points.  Additional name/code pairs may */
/*     be defined via ZZBODDEF for two purposes: */

/*        1) to associate another, perhaps more familiar or */
/*           abbreviated name with a previously defined body */
/*           integer code */

/*        2) to define a new body integer code and name */

/*     Each body name maps to a unique integer code, but more than */
/*     one name may map to a code.  Associating more than one */
/*     integer code with a particular name creates ambiguity. */
/*     Therefore the name-code mapping system establishes a */
/*     clearly defined precedence structure that assures at any */
/*     given instant only one code is assigned to a particular */
/*     name. */

/*     Entries provided via the kernel pool variables are examined */
/*     first to resolve name-code mappings.  The last listed entries */
/*     in the kernel pool arrays NAIF_BODY_CODE and NAIF_BODY_NAME */
/*     resolve any ambiguities that occur.  For example, consider */
/*     the following text kernel excerpt: */

/*        \begindata */

/*           NAIF_BODY_NAME += 'NAME' */
/*           NAIF_BODY_CODE += 1000 */

/*           NAIF_BODY_NAME += 'NAME' */
/*           NAIF_BODY_CODE += 1001 */

/*        \begintext */

/*     If, after loading this kernel, the following calls are made: */

/*        CALL ZZBODN2C ( 'NAME', CODE,  NAMFND ) */

/*        CALL ZZBODC2N ( 1000,   NAME0, FND000 ) */
/*        CALL ZZBODC2N ( 1001,   NAME1, FND001 ) */

/*     The values of CODE, NAMFND, NAME0, FND000, NAME1, and FND001 */
/*     will be: */

/*        NAMFND = .TRUE.,  CODE  = 1001 */
/*        FND000 = .FALSE., NAME0 remains unchanged */
/*        FND001 = .TRUE.,  NAME1 = 'NAME' */

/*     FND000 is .FALSE., because this name-code mapping is masked */
/*     by the higher precedent 'NAME' <-> 1001 mapping. */

/*     If the name-code mapping is not resolved by the entries */
/*     provided in the kernel pool, the values assigned via the */
/*     ZZBODDEF interface are examined next.  As with the kernel */
/*     pool, the last assignment made via the ZZBODDEF interface */
/*     has the highest precedence.  Lastly, if the name-code */
/*     mapping is not resolved by the contents of ZZBODDEF, the */
/*     built-in mappings are examined.  In actuality, the built-in */
/*     mappings represent an initial state of the ZZBODDEF listings. */
/*     As changes are made to this listing, the original mappings */
/*     are discarded. */

/*     For the case in which multiple names map to a single code, a */
/*     ZZBODC2N call returns the name last assigned to that code - a */
/*     LIFO situation. */

/* $ Examples */

/*     1) The following code fragment shows SPKEZ compute the state */
/*        (position and velocity) of Jupiter as seen from the Galileo */
/*        Orbiter.  It requires the NAIF integer codes of the target */
/*        and observer, so we use ZZBODN2C to convert names to integer */
/*        codes for those bodies. */

/*           CALL ZZBODN2C ( 'JUPITER',         TARGET, FOUND ) */

/*           CALL ZZBODN2C ( 'GALILEO ORBITER', OBSRVR, FOUND ) */

/*           CALL SPKEZ    ( TARGET, EPOCH, FRAME, ABCORR, */
/*          .                OBSRVR, STATE, LT             ) */


/*     2) This example assumes ZZBODDEF has not been called. */
/*        Thus, only the set of default name/code pairs has been */
/*        defined. */

/*        Given these names, ZZBODN2C returns the following codes: */

/*           Name                         Code    Found? */
/*           ------------------------   ------    ------ */
/*           'EARTH'                       399    Yes */
/*           '  Earth '                    399    Yes */
/*           'EMB'                           3    Yes */
/*           'Solar System Barycenter'       0    Yes */
/*           'SolarSystemBarycenter'         -    No */
/*           'SSB'                           0    Yes */
/*           'Voyager 2'                   -32    Yes */
/*           'U.S.S. Enterprise'             -    No */
/*           ' '                             -    No */
/*           'Halley's Comet'                -    No */

/*        and, given these codes, ZZBODC2N returns the following */
/*        names: */

/*           Code        Name                        Found? */
/*           -------     -------------------         ------ */
/*           399         'EARTH'                     Yes */
/*             0         'SOLAR SYSTEM BARYCENTER'   Yes */
/*             3         'EARTH BARYCENTER'          Yes */
/*           -77         'GALILEO ORBITER'           Yes */
/*            11          -                          No */
/*            -1         'GEOTAIL'                   Yes */

/*     3) This example shows the method to define a name/code pair. */
/*        You may associate a new name with a previously defined */
/*        code: */

/*           CALL ZZBODDEF ( 'JB', 5 ) */

/*        You may also define the name and integer code for a new */
/*        body: */

/*           CALL ZZBODDEF ( 'Asteroid Frank', 20103456 ) */

/*        After these calls to ZZBODDEF, ZZBODN2C would return */
/*        the following translations: */

/*           Name                         Code    Found? */
/*           ------------------------   ------    ------ */
/*           'JB'                            5    Yes */
/*           'Jupiter Barycenter'            5    Yes */
/*           'ASTEROID FRANK'         20103456    Yes */
/*           'ASTEROIDFRANK'                 -    No */
/*           'Frank'                         -    No */

/*        and ZZBODC2N returns these translations: */

/*           Code        Name                     Found? */
/*           -------     -------------------      ------ */
/*                  5    'JB'                     Yes */
/*           20103456    'Asteroid Frank'         Yes */

/*        ZZBODC2N exactly returns the string as used in the */
/*        body name/ID mapping definition. */

/*     4) To use an external IDs kernel, simply load via a FURNSH */
/*        call. */

/*           CALL FURNSH ( 'ids.ker' ) */

/*        With ids.ker listing data such as: */

/*           \begintext */

/*           Define an additional set of body, ID code mappings. */

/*           \begindata */

/*           NAIF_BODY_CODE  = ( 22, 23, 24, 25 ) */

/*           NAIF_BODY_NAME  = ( 'LARRY', 'MOE', 'CURLEY', 'SHEMP' ) */

/*        Which maps the names defined in NAIF_BODY_NAME */
/*        to the corresponding index of NAIF_BODY_CODE, i.e. */
/*        LARRY -> 22, MOE -> 23, etc, and the IDs in NAIF_BODY_CODE */
/*        map to the corresponding index of NAIF_BODY_NAME. */

/*        NOTE:  When using an external NAME-ID kernel, all ID codes */
/*        MUST be listed in the kernel variable NAIF_BODY_CODE, and */
/*        all names MUST be listed in the kernel variable */
/*        NAIF_BODY_NAME. */

/*     5) Suppose you ran the utility program SPACIT to summarize */
/*        an SPK ephemeris file and the following data was output */
/*        to the terminal screen. */

/*           ---------------------------------------------------------- */
/*           Segment identifier: JPL archive 21354 */
/*           Body        : -77                         Center     : 399 */
/*           From        : 1990 DEC 08 18:00:00.000 */
/*           To          : 1990 DEC 10 21:10:00.000 */
/*           Reference   : DE-200                      SPK Type    :1 */
/*           ---------------------------------------------------------- */

/*        You could write a program to translate the body codes */
/*        shown in the SPACIT output: */

/*           CALL ZZBODC2N ( -77, BODY,   FOUND ) */
/*           CALL ZZBODC2N ( 399, CENTER, FOUND ) */

/*           IF ( FOUND ) THEN */

/*              WRITE ( *,* ) 'BODY:    -77 = ', BODY */
/*              WRITE ( *,* ) 'CENTER:  399 = ', CENTER */

/*           END IF */

/*        You could also read the body and center codes directly from */
/*        the SPK files, using the appropriate DAF routines, and then */
/*        translate them, as above. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman   (JPL) */
/*     J.E. McLean    (JPL) */
/*     H.A. Neilan    (JPL) */
/*     B.V. Semenov   (JPL) */
/*     M.J. Spencer   (JPL) */
/*     W.L. Taber     (JPL) */
/*     F.S. Turner    (JPL) */
/*     E.D. Wright    (JPL) */
/*     K.S. Zukor     (JPL) */

/* $ Version */

/* -    SPICELIB Version 5.0.0, 16-SEP-2013 (BVS) */

/*        Changed to use name- and ID-based hashes instead of the order */
/*        arrays (see inline comments next to hash declarations for */
/*        descriptions of the hashes). */

/*        Changed to keep track of the POOL's and ZZBODTRN internal */
/*        states using state counters. */

/*        Changed to call ZZCVPOOL to bypass watcher look-ups if the */
/*        POOL counter did not change. */

/*        Added ZZBCTRCK routine to allow a caller to check the ZZBODTRN */
/*        state counter against the caller's saved counter. */

/*        Changed umbrella calling sequence to include USRCTR and UPDATE */
/*        arguments. */

/* -    SPICELIB Version 4.3.0, 05-MAR-2009 (NJB) */

/*        Bug fixes: the entry points ZZBODN2C, ZZBODC2N, and ZZBODKIK */
/*        now keep track of whether their kernel pool look-ups */
/*        succeeded. If not, a kernel pool lookup is attempted on the */
/*        next call to any entry point that calls ZZBODKER. */

/* -    SPICELIB Version 4.0.2, 19-SEP-2006 (EDW) */

/*        Added text to previously empty Declarations section. */

/* -    SPICELIB Version 4.0.1, 17-APR-2003 (EDW) */

/*        Corrected typo in header docs. */

/* -    SPICELIB Version 4.0.0, 23-AUG-2002 (FST) */

/*        Cleaned up  ZZBODTRN routine/entry point source code */
/*        and private subroutines used exclusively by ZZBODTRN */
/*        to process name-code mappings. */

/*        ZZBODLST has been removed from this umbrella and */
/*        added to the ZZBODBLT umbrella. */

/*        The built-in (permanent collection) of name-code */
/*        mappings has been moved from this umbrella into */
/*        the ZZBODBLT umbrella.  The collection is retrieved */
/*        from the entry point ZZBODGET in ZZBODBLT. */

/*        See the Revisions section below for details. */

/* -    SPICELIB Version 3.2.0, 14-AUG-2002 (EDW) */

/*        Added the ZZBODKIK entry point. */

/*        Moved the NAIF_BODY_NAME/CODE to subroutine */
/*        ZZBODKER. No change in logic. */

/*        Added logic to enforce the precedence masking; */
/*        logic removes duplicate assignments of ZZBODDEF. */
/*        Removed the NAMENOTUNIQUE error block. */

/* -    SPICELIB Version 3.1.5, 27-NOV-2001 (EDW) */

/*        Added to the collection: */
/*        -200   CONTOUR */
/*        -146   LUNAR-A */
/*        -135   DRTS-W */

/*        Added the subroutine ZZBODLST as an entry point. */
/*        The routine outputs the current name-ID mapping */
/*        list to some output device. */

/* -    SPICELIB Version 3.1.0, 17-OCT-2001 (EDW) */

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

/* -    SPICELIB Version 3.0.0, 29-MAR-2000 (WLT) */

/*        The ID codes for Cluster 1, 2, 3 and 4 were added.  The */
/*        ID coded for Pluto Express were removed.  The ID codes */
/*        for Pluto-Kuiper Express, Pluto-Kuiper Express Simulation */
/*        and Contour were added. */

/* -    SPICELIB Version 2.0.0, 26-JAN-1998 (EDW) */

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

/* -    SPICELIB Version 1.1.0, 22-MAY-1996 (WLT) */

/*        Added the id-code for Comet Hyakutake, Comet Hale-Bopp, */
/*        Mars 96, Cassini Simulation, MGS Simulation. */

/* -    SPICELIB Version 1.0.0, 25-SEP-1995 (BVS) */

/*        Renamed umbrella subroutine and entry points to */
/*        correspond private routine convention (ZZ...). Added IDs for */
/*        tracking stations Goldstone (399001), Canberra (399002), */
/*        Madrid (399003), Usuda (399004). */

/* -    Beta Version 2.2.0, 01-AUG-1995 (HAN) */

/*        Added the IDs for Near Earth Asteroid Rendezvous (-93), */
/*        Mars Pathfinder (-53), Ulysses (-55), VSOP (-58), */
/*        Radioastron (-59), Cassini spacecraft (-82), and Cassini */
/*        Huygens probe (-150). */
/*        Mars Observer (-94) was replaced with Mars Global */
/*        Surveyor (-94). */

/* -    Beta Version 2.1.0, 15-MAR-1995 (KSZ) (HAN) */

/*        Two Shoemaker Levy 9 fragments were added, Q1 and P2 */
/*        (IDs 50000022 and 50000023). Two asteroids were added, */
/*        Eros and Mathilde (IDs 2000433 and 2000253). The */
/*        Saturnian satellite Pan (ID 618) was added. */

/* -    Beta Version 2.0.0, 03-FEB-1995 (NJB) */

/*        The Galileo probe (ID -344) has been added to the permanent */
/*        collection. */

/* -    Beta Version 1.0.0, 29-APR-1994 (MJS) */

/*        SPICELIB symbol tables are no longer used. Instead, two order */
/*        vectors are used to index the NAMES and CODES arrays. Also, */
/*        this version does not support reading body name ID pairs from a */
/*        file. */

/* -    MOSPICE  Version 2.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    MOSPICE  Version 2.0.0, 15-JUL-1991 (WLT) */

/*       The body id's for the Uranian satellites discovered by Voyager */
/*       were modified to conform to those established by the IAU */
/*       nomenclature committee.  In addition the id's for Gaspra and */
/*       Ida were added. */

/* -    MOSPICE  Version 1.0.0,  7-MAR-1991 (WLT) */

/*       Some items previously considered errors were removed */
/*       and some minor modifications were made to improve the */
/*       robustness of the routines. */

/* -    GLLSPICE Version 1.0.0, 28-JUN-1990 (JEM) */

/* -& */
/* $ Revisions */

/* -    SPICELIB Version 4.0.0, 23-AUG-2002 (FST) */

/*        For clarity, some variable names have changed.  The */
/*        mappings from the old names to the new are provided */
/*        below: */

/*           Old      New     Function */
/*           ---      ---     -------- */
/*           NAMES    DEFNAM  Name definition as provided with ZZBODDEF */
/*           NORNAM   DEFNOR  Normalized name definitions */
/*           CODES    DEFCOD  Integer codes mapping to entries in DEFNAM */
/*           ORDCOD   DEFOCD  "Modified" order vector for DEFCOD */
/*           ORDNOM   DEFONR  Order vector for DEFNOR */
/*           NNAM     DEFSIZ  Size of DEFNAM, DEFNOR, DEFCOD, and DEFONR */
/*           NCOD     DEFOSZ  Size of DEFOCD */

/*           CVALS    KERNAM  Name definition as provided from pool */
/*           CVLNOM   KERNOM  Normalized name definitions */
/*           IVALS    KERCOD  Integer codes mapping to entries in KERNAM */
/*           XORDCD   KEROCD  "Modified" order vector for KERCOD */
/*           XORNOM   KERONR  Order vector for KERNOR */
/*           NUM(1)   DEFSIZ  Size of KERNAM, KERNOR, KERCOD, and KERONR */
/*           NUM(2)   DEFOSZ  Size of KEROCD */

/*        The reason for changing the names in this fashion, */
/*        is simply that these are two instances of variables */
/*        that have the same properties and utility.  The first */
/*        set implements the ZZBODDEF style mappings, and the */
/*        second implements the kernel pool style mappings. */

/*        ZZBODDEF now properly signals an error when a caller */
/*        attempts to use it to assign a blank string an ID code. */
/*        This should have never been allowed, but somehow */
/*        slipped by in previous versions. */

/*        The argument lists for ZZBODKER and ZZBODINI have */
/*        changed as of previous versions.  Some arguments */
/*        were removed, as they were no longer necessary. */

/*        ZZBODINI no longer normalizes the input name array; */
/*        rather it simply computes the order vector for the */
/*        normalized array input and the "modified" order */
/*        vector for the input code array.  This was done to */
/*        save from unnecessarily recomputing the normalization */
/*        array. */

/*        An additional umbrella has been added to the set of */
/*        modules of which ZZBODTRN makes use: ZZBODBLT.  This */
/*        umbrella houses the data statements that used to be */
/*        present in this module, which defines the "built-in" */
/*        name-code mappings.  These mappings, as of the changes */
/*        in N0053, store the mappings the define the initial */
/*        state of the DEF* arrays.  It contains two entry */
/*        points: */

/*           ZZBODGET    retrieve the initial values of DEFNAM, */
/*                       DEFNOR, DEFCOD, and DEFSIZ. */

/*           ZZBODLST    dump the "built-in" codes to a device. */

/*        ZZBODLST used to be present in this umbrella, but the */
/*        creation of ZZBODBLT made moving it there the logical */
/*        choice. */

/*        The entry point ZZBODRST has been added to the */
/*        ZZBODTRN umbrella.  This entry point resets the */
/*        state of the DEF* arrays to their initial values. */
/*        This effectively resets any changes made via the */
/*        ZZBODDEF interface.  It does not effect the kernel */
/*        pool mappings. */

/*        To support ZZBODRST, a logical BODCHG has been added */
/*        to the list of saved variables.  This variable */
/*        indicates when ZZBODDEF has been used to change the */
/*        built-in body list. */

/* -& */

/*     SPICELIB Functions */


/*     Local Parameters */


/*     Length of watched POOL keywords. */


/*     Lower bound of the built-in/BODDEF and kernel-defined hash */
/*     collision list arrays. */


/*     Local variables. */


/*     DEFNAM, DEFNOR, and DEFCOD are the name, normalized name, and ID */
/*     code lists storing built-in/BODDEF name-code mappings. DEFSIZ is */
/*     the current size of the lists */


/*     Name-based hash for built-in/BODDEF bodies. DNMLST, DNMPOL, and */
/*     DNMNMS provide the index in DNMIDX which stores the index of the */
/*     body name, normalized name, and ID in the arrays DEFNAM, DEFNOR, */
/*     DEFCOD. */


/*     ID-based hash for built-in/BODDEF bodies. DIDLST, DIDPOL, and */
/*     DIDIDS provide the index in DIDIDX which stores the index of the */
/*     body name, normalized name, and ID in the arrays DEFNAM, DEFNOR, */
/*     DEFCOD. */


/*     KERNAM, KERNOR, and KERCOD are the name, normalized name, and ID */
/*     code lists storing kernel POOL name-code mappings. KERSIZ is the */
/*     current size of the lists */


/*     Name-based hash for kernel POOL bodies. KNMLST, KNMPOL, and */
/*     KNMNMS provide the index in KNMIDX which stores the index of the */
/*     body name, normalized name, and ID in the arrays KERNAM, KERNOR, */
/*     KERCOD. */


/*     ID-based hash for kernel POOL bodies. KIDLST, KIDPOL, and KIDIDS */
/*     provide the index in KIDIDX which stores the index of the body */
/*     name, normalized name, and ID in the arrays KERNAM, KERNOR, */
/*     KERCOD. */


/*     ZZBODTRN state counter and POOL state counter. */


/*     Flag indicating whether the built-in list was altered by BODDEF */
/*     calls. */


/*     Flag indicating whether valid kernel POOL defined mappings */
/*     are present in the KERNAM, KERNOR, and KERCOD lists. */


/*     Flag indicating whether valid kernel POOL defined mappings */
/*     were successfully fetched from the POOL. */


/*     Other variables. */


/*     Save all variables. */


/*     Data statements. */

    /* Parameter adjustments */
    if (usrctr) {
	}

    /* Function Body */
    switch(n__) {
	case 1: goto L_zzbodn2c;
	case 2: goto L_zzbodc2n;
	case 3: goto L_zzboddef;
	case 4: goto L_zzbodkik;
	case 5: goto L_zzbodrst;
	case 6: goto L_zzbctrck;
	}


/*     Standard SPICELIB error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("ZZBODTRN", (ftnlen)8);
	sigerr_("SPICE(BOGUSENTRY)", (ftnlen)17);
	chkout_("ZZBODTRN", (ftnlen)8);
    }
    return 0;
/* $Procedure ZZBODN2C ( Private --- Body name to code ) */

L_zzbodn2c:
/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines.  Users should not call this routine directly due */
/*     to the volatile nature of this routine. */

/*     Translate a body name to the corresponding SPICE integer code. */

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
/*     CONVERSION */

/* $ Declarations */

/*     CHARACTER*(*)         NAME */
/*     INTEGER               CODE */
/*     LOGICAL               FOUND */

/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     NAME       I   Body name to be translated. */
/*     CODE       O   Integer code for that body. */
/*     FOUND      O   True if translated, otherwise false. */
/*     MAXL       P   Max name length. */

/* $ Detailed_Input */

/*     NAME       is an arbitrary name of a body which could be */
/*                a planet, satellite, barycenter, spacecraft, */
/*                asteroid, comet, or other ephemeris object. */

/*                Case and leading and trailing blanks in a name */
/*                are not significant.  However, when a name consists */
/*                of more than one word, they must be separated by */
/*                at least one blank, i.e., all of the following */
/*                strings are equivalent names: */

/*                   'JUPITER BARYCENTER' */
/*                   'Jupiter Barycenter' */
/*                   'JUPITER BARYCENTER   ' */
/*                   'JUPITER    BARYCENTER' */
/*                   '   JUPITER BARYCENTER' */

/*                However, 'JUPITERBARYCENTER' is not equivalent to */
/*                the names above. */

/*                When ignoring trailing blanks, NAME must have fewer */
/*                than MAXL characters. */

/* $ Detailed_Output */

/*     CODE       is the NAIF or user defined integer code for the */
/*                named body. */

/*     FOUND      return as true if NAME has a translation. */
/*                Otherwise, FOUND returns as false. */

/* $ Parameters */

/*     MAXL       is the maximum length of a body name.  Defined in */
/*                the include file 'zzbodtrn.inc'. */

/* $ Exceptions */

/*     Errors may be signaled by routines in the call tree of this */
/*     routine. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     ZZBODN2C is one of three related entry points, */

/*        ZZBODN2C      Body name to code */

/*        ZZBODC2N      Body code to name */

/*        ZZBODDEF      Body name/code definition */

/*     ZZBODN2C and ZZBODC2N perform translations between body names */
/*     and their corresponding integer codes used in SPK and PCK files */
/*     and associated routines.  A default set of name/code */
/*     pairs are automatically defined during the first call to */
/*     any of the entry points.  Additional name/code pairs may */
/*     be defined via ZZBODDEF. */

/* $ Examples */

/*     See the Examples section of the ZZBODTRN umbrella header. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman   (JPL) */
/*     J.E. McLean    (JPL) */
/*     B.V. Semenov   (JPL) */
/*     M.J. Spencer   (JPL) */
/*     W.L. Taber     (JPL) */
/*     F.S. Turner    (JPL) */
/*     E.D. Wright    (JPL) */

/* $ Version */

/* -    SPICELIB Version 5.0.0, 16-SEP-2013 (BVS) */

/*        Changed to use name-based hashes instead of the order arrays. */

/*        Changed to keep track of the POOL's and ZZBODTRN internal */
/*        states using state counters. */

/*        Changed to call ZZCVPOOL to bypass watcher look-ups if the */
/*        POOL counter did not change. */

/* -    SPICELIB Version 4.1.0, 05-MAR-2009 (NJB) */

/*        Bug fix: this routine now keeps track of whether its */
/*        kernel pool look-up succeeded. If not, a kernel pool */
/*        lookup is attempted on the next call to any entry */
/*        point that calls ZZBODKER. */

/* -    SPICELIB Version 4.0.0, 23-AUG-2002 (FST) */

/*        Cleaned up module header and source.  See the Revisions */
/*        section of ZZBODTRN for detailed update information. */

/* -    SPICELIB Version 3.1.0, 12-FEB-2001 (EDW) */

/*        Added logic to ensure the routine returns the NAME string */
/*        in the same format as when defined (case and space). */
/*        Added logic to handle error response in ZZBODINI. */

/*        To improve clarity, the BEGXX block initialization now */
/*        exists in the include file zzbodtrn.inc. */

/*        Removed the comments concerning the 851, 852, ... temporary */
/*        codes. */

/*        Set the WNAMES assignment to NAIF_BODY_CODE, NAIF_BODY_NAME */
/*        as a DATA statement. */

/*        Edited headers, removed typos and bad grammar, clarified */
/*        descriptions. */

/* -    SPICELIB Version 3.0.0, 29-MAR-2000 (WLT) */

/*        The ID codes for Cluster 1, 2, 3 and 4 were added.  The */
/*        ID coded for Pluto Express were removed.  The ID codes */
/*        for Pluto-Kuiper Express, Pluto-Kuiper Express Simulation */
/*        and Contour were added. */

/* -    SPICELIB Version 2.0.0, 21-JAN-1999 (EDW) */

/*        Added code to use the external name/ID kernel. */

/* -    SPICELIB Version 1.1.0, 29-FEB-1996 (WLT) */

/*        Added the id-code for Comet Hyakutake, Comet Hale-Bopp. */

/* -    SPICELIB Version 1.0.0, 25-SEP-1995 (BVS) */

/*        Renamed to ZZBODN2C (BVS) */

/* -    Beta Version 1.0.0, 29-APR-1994 (MJS) */

/*        SPICELIB symbol tables are no longer used. Instead, two order */
/*        vectors are used to index the NAMES and CODES arrays. */

/* -    MOSPICE  Version 2.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    MOSPICE  Version 2.0.0, 15-JUL-1991 (WLT) */

/*       The body id's for the Uranian satellites discovered by Voyager */
/*       were modified to conform to those established by the IAU */
/*       nomenclature committee.  In addition the id's for Gaspra and */
/*       Ida were added. */

/* -    MOSPICE  Version 1.0.0,  7-MAR-1991 (WLT) */

/*       Items previously considered errors were downgraded */
/*       to simply be exceptions.  Any NAME is a legitimate input now. */
/*       If its not in the table, the FOUND flag is just set to .FALSE. */

/* -    GLLSPICE Version 1.0.0, 28-JUN-1990 (JEM) */

/* -& */

/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("ZZBODN2C", (ftnlen)8);
    }

/*     Assume we will not find the code we seek. */

    *found = FALSE_;

/*     On the first pass through this entry point, initialize the */
/*     built-in arrays, set the kernel pool watchers, and state */
/*     counters. */

    if (first) {

/*        Initialize counters. Set ZZBODTRN state counter, for */
/*        which this umbrella is the owner, to subsystem values. Set */
/*        POOL counter, for which this umbrella is the user, to user */
/*        values. */

	zzctrsin_(subctr);
	zzctruin_(pulctr);

/*        Populate the initial values of the DEFNAM, DEFNOR, and DEFCOD */
/*        arrays from the built-in code-name list. */

	zzbodget_(&c__853, defnam, defnor, defcod, &defsiz, (ftnlen)36, (
		ftnlen)36);
	if (failed_()) {
	    chkout_("ZZBODN2C", (ftnlen)8);
	    return 0;
	}

/*        Populate the initial built-in code-name hashes. */

	zzbodini_(defnam, defnor, defcod, &defsiz, &c__853, dnmlst, dnmpol, 
		dnmnms, dnmidx, didlst, didpol, didids, dididx, (ftnlen)36, (
		ftnlen)36, (ftnlen)36);
	if (failed_()) {
	    chkout_("ZZBODN2C", (ftnlen)8);
	    return 0;
	}

/*        Set up the watchers for the kernel pool name-code mapping */
/*        variables. */

	swpool_("ZZBODTRN", &nwatch, wnames, (ftnlen)8, (ftnlen)32);
	if (failed_()) {
	    chkout_("ZZBODN2C", (ftnlen)8);
	    return 0;
	}

/*        Set FIRST to .FALSE. to not repeat initialization again. */

	first = FALSE_;
    }

/*     Check for updates to the kernel pool variables. Note: the first */
/*     call to ZZCVPOOL after initialization always returns .TRUE. for */
/*     LUPDTE.  This ensures that any initial assignments are properly */
/*     processed. */

    zzcvpool_("ZZBODTRN", pulctr, &lupdte, (ftnlen)8);
    if (lupdte || nodata) {

/*        Conservatively increment the ZZBODTRN state counter */
/*        in expectation of successful update. */

	zzctrinc_(subctr);

/*        Update kernel pool mapping lists and hashes. */

	zzbodker_(kernam, kernor, kercod, &kersiz, &extker, knmlst, knmpol, 
		knmnms, knmidx, kidlst, kidpol, kidids, kididx, (ftnlen)36, (
		ftnlen)36, (ftnlen)36);
	if (failed_()) {
	    nodata = TRUE_;
	    chkout_("ZZBODN2C", (ftnlen)8);
	    return 0;
	}
	nodata = FALSE_;
    }

/*     Normalize the input argument NAME. We will look this normalized */
/*     name up in the built-in and kernel pool names hashes. */

    ljucrs_(&c__1, name__, tmpnam, name_len, (ftnlen)36);

/*     If necessary, first examine the contents of the kernel pool */
/*     name-code mapping list. */

    if (extker) {

/*        Check if this name is in the kernel pool names hash. */

	zzhscchk_(knmlst, knmpol, knmnms, tmpnam, &i__, (ftnlen)36, (ftnlen)
		36);
	if (i__ != 0) {
	    *code = kercod[(i__2 = knmidx[(i__1 = i__ - 1) < 14983 && 0 <= 
		    i__1 ? i__1 : s_rnge("knmidx", i__1, "zzbodtrn_", (ftnlen)
		    1196)] - 1) < 14983 && 0 <= i__2 ? i__2 : s_rnge("kercod",
		     i__2, "zzbodtrn_", (ftnlen)1196)];
	    *found = TRUE_;
	    chkout_("ZZBODN2C", (ftnlen)8);
	    return 0;
	}
    }

/*     If we reach here, we did not find this name in the kernel pool */
/*     names hash. Check the built-in names hash. */

    zzhscchk_(dnmlst, dnmpol, dnmnms, tmpnam, &i__, (ftnlen)36, (ftnlen)36);
    if (i__ != 0) {
	*code = defcod[(i__2 = dnmidx[(i__1 = i__ - 1) < 853 && 0 <= i__1 ? 
		i__1 : s_rnge("dnmidx", i__1, "zzbodtrn_", (ftnlen)1212)] - 1)
		 < 853 && 0 <= i__2 ? i__2 : s_rnge("defcod", i__2, "zzbodtr"
		"n_", (ftnlen)1212)];
	*found = TRUE_;
    }
    chkout_("ZZBODN2C", (ftnlen)8);
    return 0;
/* $Procedure ZZBODC2N ( Private --- Body code to name ) */

L_zzbodc2n:
/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines.  Users should not call this routine directly due */
/*     to the volatile nature of this routine. */

/*     Translate the integer code of a body into a common name for */
/*     that body. */

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
/*     CONVERSION */

/* $ Declarations */

/*     INTEGER               CODE */
/*     CHARACTER*(*)         NAME */
/*     LOGICAL               FOUND */

/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     CODE       I   Integer code to be translated. */
/*     NAME       O   Common name for the body identified by CODE. */
/*     FOUND      O   True if translated, otherwise false. */
/*     MAXL       P   Max name length. */

/* $ Detailed_Input */

/*     CODE       is an integer code for a body --- */
/*                a planet, satellite, barycenter, spacecraft, */
/*                asteroid, comet, or other ephemeris object. */

/* $ Detailed_Output */

/*     NAME       is the common name of the body identified by CODE. */
/*                If CODE has more than one translation, then the */
/*                most recently defined NAME corresponding to CODE */
/*                is returned.  The routine returns NAME in the exact */
/*                format (case and blanks) as used when defining */
/*                the name/code pair. */

/*     FOUND      returns as true if NAME has a translation. */
/*                Otherwise, FOUND returns as false. */

/* $ Parameters */

/*     MAXL       is the maximum length of a body name.  Defined in */
/*                the include file 'zzbodtrn.inc'. */
/* $ Exceptions */

/*     Errors may be signaled by routines in the call tree of this */
/*     routine. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     ZZBODC2N is one of three related entry points, */

/*        ZZBODN2C      Body name to code */

/*        ZZBODC2N      Body code to name */

/*        ZZBODDEF      Body name/code definition */

/*     ZZBODN2C and ZZBODC2N perform translations between body names */
/*     and their corresponding integer codes used in SPK and PCK files */
/*     and associated routines.  A default set of name/code */
/*     pairs are automatically defined during the first call to */
/*     any of the entry points.  Additional name/code pairs may */
/*     be defined via ZZBODDEF. */

/*     For the case in which multiple names map to a single code, a */
/*     ZZBODC2N call returns the name last assigned to that code - a */
/*     LIFO situation. */

/* $ Examples */

/*     See Examples section of ZZBODTRN umbrella header. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman   (JPL) */
/*     J.E. McLean    (JPL) */
/*     B.V. Semenov   (JPL) */
/*     M.J. Spencer   (JPL) */
/*     W.L. Taber     (JPL) */
/*     F.S. Turner    (JPL) */
/*     E.D. Wright    (JPL) */

/* $ Version */

/* -    SPICELIB Version 5.0.0, 16-SEP-2013 (BVS) */

/*        Changed to use name and ID-based hashes instead of the order */
/*        arrays. */

/*        Changed to keep track of the POOL's and ZZBODTRN internal */
/*        state using state counters. */

/*        Changed to call ZZCVPOOL to bypass watcher look-ups if the */
/*        POOL counter did not change. */

/* -    SPICELIB Version 4.1.0, 05-MAR-2009 (NJB) */

/*        Bug fix: this routine now keeps track of whether its */
/*        kernel pool look-up succeeded. If not, a kernel pool */
/*        lookup is attempted on the next call to any entry */
/*        point that calls ZZBODKER. */

/* -    SPICELIB Version 4.0.0, 23-AUG-2002 (FST) */

/*        Cleaned up module header and source code.  See the Revisions */
/*        section of ZZBODTRN for detailed update information. */

/* -    SPICELIB Version 3.2.0, 19-JUL-2002 (EDW) */

/*        Added logic to enforce the precedence masking. */

/* -    SPICELIB Version 3.1.0, 5-SEP-2001 (EDW) */

/*        Added logic to ensure the routine returns the NAME string */
/*        in the same format as when defined (case and space). */
/*        Added logic to handle error response in ZZBODINI. */

/*        To improve clarity, the BEGXX block initialization now */
/*        exists in the include file zzbodtrn.inc. */

/*        Removed the comments concerning the 851, 852, ... temporary */
/*        codes. */

/*        Set the WNAMES assignment to NAIF_BODY_CODE, NAIF_BODY_NAME */
/*        as a DATA statement. */

/*        Edited headers, removed typos and bad grammar, clarified */
/*        descriptions. */

/* -    SPICELIB Version 3.0.0, 29-MAR-2000 (WLT) */

/*        The ID codes for Cluster 1, 2, 3 and 4 were added.  The */
/*        ID coded for Pluto Express were removed.  The ID codes */
/*        for Pluto-Kuiper Express, Pluto-Kuiper Express Simulation */
/*        and Contour were added. */

/* -    SPICELIB Version 2.0.0, 21-JAN-1999 (EDW) */

/*        Added code to use the external name/ID kernel. */

/* -    SPICELIB Version 1.1.0, 29-FEB-1996 (WLT) */

/*        Added the id-code for Comet Hyakutake, Comet Hale-Bopp. */

/* -    SPICELIB Version 1.0.0, 25-SEP-1995 (BVS) */

/*        Renamed to ZZBODC2N (BVS) */

/* -    Beta Version 1.0.0, 29-APR-1994 (MJS) */

/*        SPICELIB symbol tables are no longer used. Instead, two order */
/*        vectors are used to index the NAMES and CODES arrays. */

/* -    MOSPICE  Version 2.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    MOSPICE  Version 2.0.0, 15-JUL-1991 (WLT) */

/*       The body id's for the Uranian satellites discovered by Voyager */
/*       were modified to conform to those established by the IAU */
/*       nomenclature committee.  In addition the id's for Gaspra and */
/*       Ida were added. */

/* -    MOSPICE  Version 1.0.0,  7-MAR-1991 (WLT) */

/*       Checks to see the input integer code can be represented */
/*       as a character string were removed along with the exceptions */
/*       associated with these checks.  It is now the responsibility */
/*       of a maintenance programmer to make sure MAXL is large */
/*       enough to allow any integer to be converted to a string */
/*       representation. */

/* -    GLLSPICE Version 1.0.0, 28-JUN-1990 (JEM) */


/* -& */

/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("ZZBODC2N", (ftnlen)8);
    }

/*     Assume we will not find the name we seek. */

    *found = FALSE_;

/*     On the first pass through this entry point, initialize the */
/*     built-in arrays, set the kernel pool watchers, and state */
/*     counters. */

    if (first) {

/*        Initialize counters. Set ZZBODTRN state counter, for */
/*        which this umbrella is the owner, to subsystem values. Set */
/*        POOL counter, for which this umbrella is the user, to user */
/*        values. */

	zzctrsin_(subctr);
	zzctruin_(pulctr);

/*        Populate the initial values of the DEFNAM, DEFNOR, and DEFCOD */
/*        arrays from the built-in code list. */

	zzbodget_(&c__853, defnam, defnor, defcod, &defsiz, (ftnlen)36, (
		ftnlen)36);
	if (failed_()) {
	    chkout_("ZZBODC2N", (ftnlen)8);
	    return 0;
	}

/*        Populate the initial built-in code-name hashes. */

	zzbodini_(defnam, defnor, defcod, &defsiz, &c__853, dnmlst, dnmpol, 
		dnmnms, dnmidx, didlst, didpol, didids, dididx, (ftnlen)36, (
		ftnlen)36, (ftnlen)36);
	if (failed_()) {
	    chkout_("ZZBODC2N", (ftnlen)8);
	    return 0;
	}

/*        Set up the watchers for the kernel pool name-code mapping */
/*        variables. */

	swpool_("ZZBODTRN", &nwatch, wnames, (ftnlen)8, (ftnlen)32);
	if (failed_()) {
	    chkout_("ZZBODC2N", (ftnlen)8);
	    return 0;
	}

/*        Set FIRST to .FALSE. to not repeat initialization again. */

	first = FALSE_;
    }

/*     Check for updates to the kernel pool variables. Note: the first */
/*     call to ZZCVPOOL after initialization always returns .TRUE. for */
/*     LUPDTE. This ensures that any initial assignments are properly */
/*     processed. */

    zzcvpool_("ZZBODTRN", pulctr, &lupdte, (ftnlen)8);
    if (lupdte || nodata) {

/*        Conservatively increment the ZZBODTRN state counter */
/*        in expectation of successful update. */

	zzctrinc_(subctr);

/*        Update kernel pool mapping lists and hashes. */

	zzbodker_(kernam, kernor, kercod, &kersiz, &extker, knmlst, knmpol, 
		knmnms, knmidx, kidlst, kidpol, kidids, kididx, (ftnlen)36, (
		ftnlen)36, (ftnlen)36);
	if (failed_()) {
	    nodata = TRUE_;
	    chkout_("ZZBODC2N", (ftnlen)8);
	    return 0;
	}
	nodata = FALSE_;
    }

/*     If necessary, first examine the contents of the kernel pool */
/*     name-code mapping list. */

    if (extker) {

/*        Check if this code is in the kernel pool codes hash. */

	zzhsichk_(kidlst, kidpol, kidids, code, &i__);
	if (i__ != 0) {
	    s_copy(name__, kernam + ((i__2 = kididx[(i__1 = i__ - 1) < 14983 
		    && 0 <= i__1 ? i__1 : s_rnge("kididx", i__1, "zzbodtrn_", 
		    (ftnlen)1574)] - 1) < 14983 && 0 <= i__2 ? i__2 : s_rnge(
		    "kernam", i__2, "zzbodtrn_", (ftnlen)1574)) * 36, 
		    name_len, (ftnlen)36);
	    *found = TRUE_;
	    chkout_("ZZBODC2N", (ftnlen)8);
	    return 0;
	}
    }

/*     If we reach here, we did not find this code in the kernel pool */
/*     codes hash. Check the built-in codes hash. */

    zzhsichk_(didlst, didpol, didids, code, &i__);

/*     If we find a match, verify that it is not masked by a kernel pool */
/*     entry before returning. */

    if (i__ != 0) {
	if (extker) {

/*           Only bother performing this check if there are actually */
/*           mappings present in the kernel pool lists. */

	    zzhscchk_(knmlst, knmpol, knmnms, defnor + ((i__2 = dididx[(i__1 =
		     i__ - 1) < 853 && 0 <= i__1 ? i__1 : s_rnge("dididx", 
		    i__1, "zzbodtrn_", (ftnlen)1600)] - 1) < 853 && 0 <= i__2 
		    ? i__2 : s_rnge("defnor", i__2, "zzbodtrn_", (ftnlen)1600)
		    ) * 36, &j, (ftnlen)36, (ftnlen)36);
	    if (j != 0) {

/*              This name is defined in the kernel pool mappings. Set */
/*              FOUND to .FALSE., as the contents of the kernel pool */
/*              have higher precedence than any entries in the built-in */
/*              mapping list. */

		*found = FALSE_;
	    } else {

/*              No match for this name in the kernel pool mapping list. */
/*              Return the name. */

		s_copy(name__, defnam + ((i__2 = dididx[(i__1 = i__ - 1) < 
			853 && 0 <= i__1 ? i__1 : s_rnge("dididx", i__1, 
			"zzbodtrn_", (ftnlen)1619)] - 1) < 853 && 0 <= i__2 ? 
			i__2 : s_rnge("defnam", i__2, "zzbodtrn_", (ftnlen)
			1619)) * 36, name_len, (ftnlen)36);
		*found = TRUE_;
	    }
	} else {

/*           No kernel pool mappings were defined, simply return */
/*           return the name. */

	    s_copy(name__, defnam + ((i__2 = dididx[(i__1 = i__ - 1) < 853 && 
		    0 <= i__1 ? i__1 : s_rnge("dididx", i__1, "zzbodtrn_", (
		    ftnlen)1630)] - 1) < 853 && 0 <= i__2 ? i__2 : s_rnge(
		    "defnam", i__2, "zzbodtrn_", (ftnlen)1630)) * 36, 
		    name_len, (ftnlen)36);
	    *found = TRUE_;
	}
    }
    chkout_("ZZBODC2N", (ftnlen)8);
    return 0;
/* $Procedure ZZBODDEF ( Private --- Body name/code definition ) */

L_zzboddef:
/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines.  Users should not call this routine directly due */
/*     to the volatile nature of this routine. */

/*     Define a body name/code pair for later translation by */
/*     ZZBODN2C or ZZBODC2N. */

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
/*     CONVERSION */

/* $ Declarations */

/*     CHARACTER*(*)         NAME */
/*     INTEGER               CODE */

/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     NAME       I   Common name of some body. */
/*     CODE       I   Integer code for that body. */
/*     MAXL       P   Max name length and max number of digits in code. */
/*     MAXP       P   Maximum number of name/code pair definitions. */

/* $ Detailed_Input */

/*     NAME       is an arbitrary name of a body which could be */
/*                a planet, satellite, barycenter, spacecraft, */
/*                asteroid, comet, or other ephemeris object. */

/*                The case and positions of blanks in a name */
/*                are significant. ZZBODC2N returns the exact */
/*                string (case and space) last mapped to a code. */
/*                When a name is made up of more than one word, */
/*                the words require separation by at least one blank, */
/*                i.e., all of the following strings belong to */
/*                the same equivalence class: */

/*                   'JUPITER BARYCENTER' */
/*                   'Jupiter Barycenter' */
/*                   'JUPITER BARYCENTER   ' */
/*                   'JUPITER    BARYCENTER' */
/*                   '   JUPITER BARYCENTER' */

/*                However, 'JUPITERBARYCENTER' is not equivalent to */
/*                the names above. */

/*                When ignoring trailing blanks, NAME must have fewer */
/*                than MAXL characters. */

/*     CODE       is the integer code for the named body. */

/*                CODE may already have a name as defined by a */
/*                previous call to ZZBODDEF or as part of the set of */
/*                default definitions.  That previous definition */
/*                remains and a translation of that name still */
/*                returns the same CODE.  However, future translations */
/*                of CODE will give the new NAME instead of the */
/*                previous one.  This feature is useful for assigning */
/*                a more familiar or abbreviated name to a body. */
/*                For example, in addition to the default name for */
/*                body 5, 'JUPITER BARYCENTER', you could define the */
/*                abbreviation 'JB' to mean 5. */

/* $ Detailed_Output */

/*     None. */

/* $ Parameters */

/*     The following relevant parameters are defined in zzbodtrn.inc: */

/*     MAXL        is the maximum length of a body name. */

/*     MAXP        is the maximum number of additional names that may */
/*                 be added via the ZZBODDEF interface. */

/* $ Exceptions */

/*     1) If the maximum number of definitions is exceeded, a the */
/*        error SPICE(TOOMANYPAIRS) is signaled. */

/*     2) If an attempt to assign a blank string an ID code is made, */
/*        the error SPICE(BLANKNAMEASSIGNED) is signaled. */

/*     3) Routines in the call tree of this routine may signal */
/*        errors. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     ZZBODDEF is one of three related entry points, */

/*        ZZBODN2C      Body name to code */

/*        ZZBODC2N      Body code to name */

/*        ZZBODDEF      Body name/code definition */

/*     ZZBODN2C and ZZBODC2N perform translations between body names */
/*     and their corresponding integer codes used in SPK and PCK files */
/*     and associated routines.  A default set of name/code */
/*     pairs are automatically defined during the first call to */
/*     any of the entry points.  Additional name/code pairs may */
/*     be defined via ZZBODDEF for two purposes: */

/*        1.  to associate another, perhaps more familiar or */
/*            abbreviated name with a previously defined body integer */
/*            code or */

/*        2.  to define a new body integer code and name, */

/*     Each body has a unique integer code, but may have several */
/*     names.  Thus you may associate more than one name with */
/*     a particular integer code.  However, associating more */
/*     than one integer code with a particular name creates ambiguity. */
/*     Therefore, once a name has been defined, it may not be redefined */
/*     with a different integer code. */

/*     For example, Europa is the name of the second satellite of */
/*     Jupiter, and has the NAIF integer code 502.  Thus (EUROPA, 502) */
/*     is one of the default definitions.  Europa is also the name */
/*     of an asteroid.  Suppose you were able to associate the asteroid */
/*     integer code with the name EUROPA.  Then when you call ZZBODN2C to */
/*     translate the name EUROPA, which code should be returned?  That */
/*     of the asteroid or 502? */

/*     ZZBODDEF prevents this ambiguity by signaling an error */
/*     if the specified name has already been defined with a */
/*     different code.  In the case of EUROPA, you may want to use the */
/*     name ASTEROID EUROPA.  The set of default definitions are listed */
/*     in DATA statements in the umbrella routine ZZBODTRN for easy */
/*     reference. */

/* $ Examples */

/*     See the Examples section of the ZZBODTRN umbrella header. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     J.E. McLean    (JPL) */
/*     B.V. Semenov   (JPL) */
/*     W.L. Taber     (JPL) */
/*     F.S. Turner    (JPL) */
/*     E.D. Wright    (JPL) */

/* $ Version */

/* -    SPICELIB Version 5.0.0, 16-SEP-2013 (BVS) */

/*        Changed to use name and ID-based hashes instead of the order */
/*        arrays. */

/*        Changed to keep track of the POOL's and ZZBODTRN internal */
/*        state using state counters. */

/*        Changed to call ZZCVPOOL to bypass watcher look-ups if the */
/*        POOL counter did not change. */

/* -    SPICELIB Version 4.0.1, 17-APR-2003 (EDW) */

/*        Correct typo in header docs. */

/*     SPICELIB Version 4.0.0, 23-AUG-2002 (FST) */

/*        Cleaned up module header and source code.  See the Revisions */
/*        section of ZZBODTRN for detailed update information. */

/*        Added the error SPICE(BLANKNAMEASSIGNED), when the caller */
/*        attempts to assign an ID code to a blank string. */

/* -    SPICELIB Version 1.3.0, 14-AUG-2002 (EDW) */

/*        Added logic to enforce the precedence masking; */
/*        logic removes duplicate assignments of ZZBODDEF. */
/*        Removed the NAMENOTUNIQUE error block. */

/* -    SPICELIB Version 1.2.0, 5-SEP-2001 (EDW) */

/*        Added logic to ensure the routine returns the NAME string */
/*        in the same format as when defined (case and space). */
/*        Added logic to handle error response from ZZBODINI. */

/*        To improve clarity, the BEGXX block initialization now */
/*        exists in the include file zzbodtrn.inc. */

/*        Removed the comments concerning the 851, 852, ... temporary */
/*        codes. */

/*        Set the WNAMES assignment to NAIF_BODY_CODE, NAIF_BODY_NAME */
/*        as a DATA statement. */

/*        Edited headers, removed typos and bad grammar, clarified */
/*        descriptions. */

/* -    SPICELIB Version 1.1.0, 29-FEB-1996 (WLT) */

/*        Added the id-code for Comet Hyakutake, Comet Hale-Bopp. */

/* -    SPICELIB Version 1.0.0, 25-SEP-1995 (BVS) */

/*        Renamed to ZZBODDEF (BVS). More careful checking for overflow */
/*        of the recognized names is now performed. */

/* -    Beta Version 1.0.0, 29-APR-1994 (MJS) */

/*        SPICELIB symbol tables are no longer used. Instead, two order */
/*        vectors are used to index the NAMES and CODES arrays. */

/* -    MOSPICE  Version 2.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    MOSPICE  Version 2.0.0, 15-JUL-1991 (WLT) */

/*       The body id's for the Uranian satellites discovered by Voyager */
/*       were modified to conform to those established by the IAU */
/*       nomenclature committee.  In addition the id's for Gaspra and */
/*       Ida were added. */

/* -    MOSPICE  Version 1.0.0,  7-MAR-1991 (WLT) */

/*       Checks to see an integer code can be represented */
/*       as a character string were removed along with the exceptions */
/*       associated with these checks.  It is now the responsibility */
/*       of a maintenance programmer to make sure MAXL is large */
/*       enough to allow any integer to be converted to a string */
/*       representation. */

/* -    GLLSPICE Version 1.0.0, 28-JUN-1990 (JEM) */

/* -& */

/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("ZZBODDEF", (ftnlen)8);
    }

/*     On the first pass through this entry point, initialize the */
/*     built-in arrays, set the kernel pool watchers, and state */
/*     counters. */

    if (first) {

/*        Initialize counters. Set ZZBODTRN state counter, for */
/*        which this umbrella is the owner, to subsystem values. Set */
/*        POOL counter, for which this umbrella is the user, to user */
/*        values. */

	zzctrsin_(subctr);
	zzctruin_(pulctr);

/*        Populate the initial values of the DEFNAM, DEFNOR, and DEFCOD */
/*        arrays from the built-in code list. */

	zzbodget_(&c__853, defnam, defnor, defcod, &defsiz, (ftnlen)36, (
		ftnlen)36);
	if (failed_()) {
	    chkout_("ZZBODDEF", (ftnlen)8);
	    return 0;
	}

/*        Populate the initial built-in code-name hashes. */

	zzbodini_(defnam, defnor, defcod, &defsiz, &c__853, dnmlst, dnmpol, 
		dnmnms, dnmidx, didlst, didpol, didids, dididx, (ftnlen)36, (
		ftnlen)36, (ftnlen)36);
	if (failed_()) {
	    chkout_("ZZBODDEF", (ftnlen)8);
	    return 0;
	}

/*        Set up the watchers for the kernel pool name-code mapping */
/*        variables. */

	swpool_("ZZBODTRN", &nwatch, wnames, (ftnlen)8, (ftnlen)32);
	if (failed_()) {
	    chkout_("ZZBODDEF", (ftnlen)8);
	    return 0;
	}

/*        Set FIRST to .FALSE. to not repeat initialization again. */

	first = FALSE_;
    }

/*     Begin by verifying that the user is not attempting to assign a */
/*     blank string a code. */

    if (s_cmp(name__, " ", name_len, (ftnlen)1) == 0) {
	setmsg_("An attempt to assign the code, #, to a blank string was mad"
		"e.  Check loaded text kernels for a blank string in the NAIF"
		"_BODY_NAME array.", (ftnlen)136);
	errint_("#", &i__, (ftnlen)1);
	sigerr_("SPICE(BLANKNAMEASSIGNED)", (ftnlen)24);
	chkout_("ZZBODDEF", (ftnlen)8);
	return 0;
    }

/*     Conservatively increment the ZZBODTRN state counter in */
/*     expectation of successful addition. */

    zzctrinc_(subctr);

/*     Get normalized form of the input NAME. */

    ljucrs_(&c__1, name__, tmpnam, name_len, (ftnlen)36);

/*     Determine if we are going to replace an entry currently present */
/*     in the DEF* lists. */

    zzhscchk_(dnmlst, dnmpol, dnmnms, tmpnam, &i__, (ftnlen)36, (ftnlen)36);
    if (i__ != 0) {
	index = dnmidx[(i__1 = i__ - 1) < 853 && 0 <= i__1 ? i__1 : s_rnge(
		"dnmidx", i__1, "zzbodtrn_", (ftnlen)2026)];

/*        We are going to replace an existing entry.  There are */
/*        two possible ways in which a replace operation can */
/*        happen: */

/*           1) The caller is attempting to replace the highest */
/*              precedent name-code mapping for a particular ID code. */
/*              When this happens, we need only change the entry in */
/*              DEFNAM at position INDEX. The user is simply changing */
/*              the name that maps to the same normalized name. */

/*           2) The caller is attempting to change the code */
/*              associated with a name, bump a lower precedence */
/*              name-code mapping to highest precedence, or some */
/*              combination of the two. */

/*        See if we should handle 1) first. */

	zzhsichk_(didlst, didpol, didids, code, &i__);
	if (i__ != 0) {
	    codidx = dididx[(i__1 = i__ - 1) < 853 && 0 <= i__1 ? i__1 : 
		    s_rnge("dididx", i__1, "zzbodtrn_", (ftnlen)2049)];
	} else {
	    codidx = 0;
	}

/*        If CODIDX matches INDEX, then we simply have to replace the */
/*        entry in DEFNAM and return. */

	if (codidx == index) {

/*           We altered the built-in body list. Set BODCHG to .TRUE. */

	    bodchg = TRUE_;
	    s_copy(defnam + ((i__1 = index - 1) < 853 && 0 <= i__1 ? i__1 : 
		    s_rnge("defnam", i__1, "zzbodtrn_", (ftnlen)2065)) * 36, 
		    name__, (ftnlen)36, name_len);
	    chkout_("ZZBODDEF", (ftnlen)8);
	    return 0;
	}

/*        At this point we have to replace all of the values for the */
/*        mapping defined at the INDEX position in DEFNAM, DEFNOR, and */
/*        DEFCOD. This will require recomputing the hashes. First */
/*        compress out the existing entry. */

	i__1 = defsiz;
	for (i__ = index + 1; i__ <= i__1; ++i__) {
	    s_copy(defnam + ((i__2 = i__ - 2) < 853 && 0 <= i__2 ? i__2 : 
		    s_rnge("defnam", i__2, "zzbodtrn_", (ftnlen)2080)) * 36, 
		    defnam + ((i__3 = i__ - 1) < 853 && 0 <= i__3 ? i__3 : 
		    s_rnge("defnam", i__3, "zzbodtrn_", (ftnlen)2080)) * 36, (
		    ftnlen)36, (ftnlen)36);
	    s_copy(defnor + ((i__2 = i__ - 2) < 853 && 0 <= i__2 ? i__2 : 
		    s_rnge("defnor", i__2, "zzbodtrn_", (ftnlen)2081)) * 36, 
		    defnor + ((i__3 = i__ - 1) < 853 && 0 <= i__3 ? i__3 : 
		    s_rnge("defnor", i__3, "zzbodtrn_", (ftnlen)2081)) * 36, (
		    ftnlen)36, (ftnlen)36);
	    defcod[(i__2 = i__ - 2) < 853 && 0 <= i__2 ? i__2 : s_rnge("defc"
		    "od", i__2, "zzbodtrn_", (ftnlen)2082)] = defcod[(i__3 = 
		    i__ - 1) < 853 && 0 <= i__3 ? i__3 : s_rnge("defcod", 
		    i__3, "zzbodtrn_", (ftnlen)2082)];
	}
    } else {

/*        We need to add this entry to the list.  See if there */
/*        is room; signal an error and return if there is not. */

	if (defsiz >= 853) {
	    setmsg_("There is no room available for adding '#'  to the list "
		    "of name/code pairs. The number of names that can be supp"
		    "orted is #.  This number has been reached. ", (ftnlen)154)
		    ;
	    errch_("#", name__, (ftnlen)1, name_len);
	    errint_("#", &defsiz, (ftnlen)1);
	    sigerr_("SPICE(TOOMANYPAIRS)", (ftnlen)19);
	    chkout_("ZZBODDEF", (ftnlen)8);
	    return 0;
	}

/*        If we reach here, then there is room in the list. Increase */
/*        it's size counter. */

	++defsiz;
    }

/*     We are changing the body list, inform ZZBODRST by setting BODCHG */
/*     to .TRUE. */

    bodchg = TRUE_;

/*     Now, we need to add the new entry on to the end of the */
/*     DEFNAM, DEFNOR, and DEFCOD lists. */

    s_copy(defnam + ((i__1 = defsiz - 1) < 853 && 0 <= i__1 ? i__1 : s_rnge(
	    "defnam", i__1, "zzbodtrn_", (ftnlen)2125)) * 36, name__, (ftnlen)
	    36, name_len);
    s_copy(defnor + ((i__1 = defsiz - 1) < 853 && 0 <= i__1 ? i__1 : s_rnge(
	    "defnor", i__1, "zzbodtrn_", (ftnlen)2126)) * 36, tmpnam, (ftnlen)
	    36, (ftnlen)36);
    defcod[(i__1 = defsiz - 1) < 853 && 0 <= i__1 ? i__1 : s_rnge("defcod", 
	    i__1, "zzbodtrn_", (ftnlen)2127)] = *code;

/*     Reset the built-in/BODDEF hashes. */

    zzbodini_(defnam, defnor, defcod, &defsiz, &c__853, dnmlst, dnmpol, 
	    dnmnms, dnmidx, didlst, didpol, didids, dididx, (ftnlen)36, (
	    ftnlen)36, (ftnlen)36);
    chkout_("ZZBODDEF", (ftnlen)8);
    return 0;
/* $Procedure ZZBODKIK ( Private --- Run the kernel read block ) */

L_zzbodkik:
/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines. Users should not call this routine directly due */
/*     to the volatile nature of this routine. */

/*     This routine executes the kernel pool read instructions */
/*     if necessary. */

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

/*     BODY MAPPING */

/* $ Declarations */

/*     None. */

/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     None. */

/* $ Detailed_Input */

/*     NONE. */

/* $ Detailed_Output */

/*     NONE. */

/* $ Parameters */

/*     NONE. */

/* $ Exceptions */

/*     NONE. */

/* $ Files */

/*     NONE. */

/* $ Particulars */

/*     This entry point provides a mechanism to allow a caller */
/*     to force the examination of the kernel pool variables that */
/*     define name-code mappings.  This is useful, if once a new */
/*     mapping is defined, diagnostics at the time of definition */
/*     are useful.  The way the system performs otherwise, the */
/*     diagnostics are not provided until a name-code conversion */
/*     is attempted. */

/* $ Examples */

/*     See ZZLDKER for sample usage. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman   (JPL) */
/*     B.V. Semenov   (JPL) */
/*     F.S. Turner    (JPL) */
/*     E.D. Wright    (JPL) */

/* $ Version */

/* -    SPICELIB Version 5.0.0, 16-SEP-2013 (BVS) */

/*        Changed to use name and ID-based hashes instead of the order */
/*        arrays. */

/*        Changed to keep track of the POOL's and ZZBODTRN internal */
/*        state using state counters. */

/*        Changed to call ZZCVPOOL to bypass watcher look-ups if the */
/*        POOL counter did not change. */

/* -    SPICELIB Version 4.1.0, 05-MAR-2009 (NJB) */

/*        Bug fix: this routine now keeps track of whether its */
/*        kernel pool look-up succeeded. If not, a kernel pool */
/*        lookup is attempted on the next call to any entry */
/*        point that calls ZZBODKER. */

/* -    SPICELIB Version 4.0.2, 19-SEP-2006 (EDW) */

/*        Added text to previously empty Declarations section. */

/* -    SPICELIB Version 4.0.0, 23-AUG-2002 (FST) */

/*        Added checks to watchers and the initialization */
/*        block. */

/* -    SPICELIB Version 1.0.0, 16-JUN-2002 (EDW) */

/* -& */

/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("ZZBODKIK", (ftnlen)8);
    }

/*     On the first pass through this entry point, initialize the */
/*     built-in arrays, set the kernel pool watchers, and state */
/*     counters. */

    if (first) {

/*        Initialize counters. Set ZZBODTRN state counter, for */
/*        which this umbrella is the owner, to subsystem values. Set */
/*        POOL counter, for which this umbrella is the user, to user */
/*        values. */

	zzctrsin_(subctr);
	zzctruin_(pulctr);

/*        Populate the initial values of the DEFNAM, DEFNOR, and DEFCOD */
/*        arrays from the built-in code list. */

	zzbodget_(&c__853, defnam, defnor, defcod, &defsiz, (ftnlen)36, (
		ftnlen)36);
	if (failed_()) {
	    chkout_("ZZBODKIK", (ftnlen)8);
	    return 0;
	}

/*        Populate the initial built-in/BODDEF hashes. */

	zzbodini_(defnam, defnor, defcod, &defsiz, &c__853, dnmlst, dnmpol, 
		dnmnms, dnmidx, didlst, didpol, didids, dididx, (ftnlen)36, (
		ftnlen)36, (ftnlen)36);
	if (failed_()) {
	    chkout_("ZZBODKIK", (ftnlen)8);
	    return 0;
	}

/*        Set up the watchers for the kernel pool name-code mapping */
/*        variables. */

	swpool_("ZZBODTRN", &nwatch, wnames, (ftnlen)8, (ftnlen)32);
	if (failed_()) {
	    chkout_("ZZBODKIK", (ftnlen)8);
	    return 0;
	}

/*        Set FIRST to .FALSE. to not repeat initialization again. */

	first = FALSE_;
    }

/*     Check for updates to the kernel pool variables. Note: the first */
/*     call to ZZCVPOOL after initialization always returns .TRUE. for */
/*     LUPDTE. This ensures that any initial assignments are properly */
/*     processed. */

    zzcvpool_("ZZBODTRN", pulctr, &lupdte, (ftnlen)8);
    if (lupdte || nodata) {

/*        Conservatively increment the ZZBODTRN state counter */
/*        in expectation of successful update. */

	zzctrinc_(subctr);

/*        Update kernel pool mapping lists and hashes. */

	zzbodker_(kernam, kernor, kercod, &kersiz, &extker, knmlst, knmpol, 
		knmnms, knmidx, kidlst, kidpol, kidids, kididx, (ftnlen)36, (
		ftnlen)36, (ftnlen)36);
	if (failed_()) {
	    nodata = TRUE_;
	    chkout_("ZZBODKIK", (ftnlen)8);
	    return 0;
	}
	nodata = FALSE_;
    }
    chkout_("ZZBODKIK", (ftnlen)8);
    return 0;
/* $Procedure ZZBODRST ( Private --- Body List Reset ) */

L_zzbodrst:
/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines.  Users should not call this routine directly due */
/*     to the volatile nature of this routine. */

/*     This routine resets the built-in body list, removing any */
/*     assignments or alterations made by the ZZBODDEF entry point. */

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

/*     None. */

/* $ Brief_I/O */

/*     None. */

/* $ Detailed_Input */

/*     None. */

/* $ Detailed_Output */

/*     None. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1) Routines in the call tree of this routine may signal errors. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     ZZBODRST resets the built-in body name-code mapping list.  This */
/*     list may only be modified by ZZBODDEF.  Further, any assignments */
/*     made through the kernel pool mechanism remain unaltered as a */
/*     result of invoking this routine. */

/* $ Examples */

/*     None. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     B.V. Semenov    (JPL) */
/*     F.S. Turner     (JPL) */

/* $ Version */

/* -    SPICELIB Version 5.0.0, 16-SEP-2013 (BVS) */

/*        Changed to use name and ID-based hashes instead of the order */
/*        arrays. */

/*        Changed to keep track of the POOL's and ZZBODTRN internal */
/*        state using state counters. */

/* -    SPICELIB Version 4.0.0, 26-AUG-2002 (FST) */


/* -& */

/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("ZZBODRST", (ftnlen)8);
    }

/*     On the first pass through this entry point, initialize the */
/*     built-in arrays, set the kernel pool watchers, and state */
/*     counters. */

    if (first) {

/*        Initialize counters. Set ZZBODTRN state counter, for */
/*        which this umbrella is the owner, to subsystem values. Set */
/*        POOL counter, for which this umbrella is the user, to user */
/*        values. */

	zzctrsin_(subctr);
	zzctruin_(pulctr);

/*        Populate the initial values of the DEFNAM, DEFNOR, and DEFCOD */
/*        arrays from the built-in code list. */

	zzbodget_(&c__853, defnam, defnor, defcod, &defsiz, (ftnlen)36, (
		ftnlen)36);
	if (failed_()) {
	    chkout_("ZZBODRST", (ftnlen)8);
	    return 0;
	}

/*        Populate the initial built-in code-name hashes. */

	zzbodini_(defnam, defnor, defcod, &defsiz, &c__853, dnmlst, dnmpol, 
		dnmnms, dnmidx, didlst, didpol, didids, dididx, (ftnlen)36, (
		ftnlen)36, (ftnlen)36);
	if (failed_()) {
	    chkout_("ZZBODRST", (ftnlen)8);
	    return 0;
	}

/*        Set up the watchers for the kernel pool name-code mapping */
/*        variables. */

	swpool_("ZZBODTRN", &nwatch, wnames, (ftnlen)8, (ftnlen)32);
	if (failed_()) {
	    chkout_("ZZBODRST", (ftnlen)8);
	    return 0;
	}

/*        Set FIRST to .FALSE. to not repeat initialization again. */

	first = FALSE_;
    }

/*     See if the body list needs to be reset. */

    if (bodchg) {
	bodchg = FALSE_;

/*        Conservatively increment the ZZBODTRN state counter */
/*        in expectation of successful update. */

	zzctrinc_(subctr);

/*        Fetch the initial body name-code mapping list. Note: we need */
/*        not check FAILED() here, because if an error had occurred due */
/*        to the improper specification of MAXE it would have been */
/*        signaled already to the user. */

	zzbodget_(&c__853, defnam, defnor, defcod, &defsiz, (ftnlen)36, (
		ftnlen)36);

/*        Reset the built-in/BODDEF hashes. */

	zzbodini_(defnam, defnor, defcod, &defsiz, &c__853, dnmlst, dnmpol, 
		dnmnms, dnmidx, didlst, didpol, didids, dididx, (ftnlen)36, (
		ftnlen)36, (ftnlen)36);
    }
    chkout_("ZZBODRST", (ftnlen)8);
    return 0;
/* $Procedure ZZBCTRCK ( Private -- check/update user's ZZBODTRN counter ) */

L_zzbctrck:
/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines.  Users should not call this routine directly due */
/*     to the volatile nature of this routine. */

/*     Check and update the ZZBODTRN state counter tracked by a caller */
/*     (user) routine. */

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
/*     PRIVATE */

/* $ Declarations */

/*     INTEGER               USRCTR    ( CTRSIZ ) */
/*     LOGICAL               UPDATE */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     USRCTR    I/O  ZZBODTRN state counter tracked by the caller */
/*     UPDATE     O   Flag indicating if input counter was updated */

/*     CTRSIZ     P   Dimension of the counter array */

/* $ Detailed_Input */

/*     USRCTR      is the value of the ZZBODTRN state counter tracked by */
/*                 (saved in) the caller (user) routine. */

/* $ Detailed_Output */

/*     USRCTR      is the current ZZBODTRN state counter. */

/*     UPDATE      is the logical flag indicating whether the input */
/*                 ZZBODTRN state counter was different from the current */
/*                 ZZBODTRN state counter and, therefore, had to be */
/*                 updated (UPDATE = .TRUE.) or if it was the same */
/*                 (UPDATE = .FALSE.). */

/* $ Parameters */

/*     CTRSIZ      is the dimension of the counter array used by */
/*                 various SPICE subsystems to uniquely identify */
/*                 changes in their states. This parameter is */
/*                 defined in the private include file 'zzctr.inc'. */

/* $ Exceptions */

/*     1) Routines in the call tree of this routine may signal errors. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine is not part of the SPICELIB API. This routine may be */
/*     removed in a later version of the SPICE Toolkit, or its interface */
/*     may change. */

/*     SPICE-based application code should not call this routine. */

/*     This routine allows other routines to be aware of ZZBODTRN state */
/*     change due to addition or deletion body name-code mappings via */
/*     ZZBODTRN calls and/or kernel POOL NAIF_BODY_NAME/NAIF_BODY_CODE */
/*     variables. Such awareness is needed to be able to locally save */
/*     some ZZBODTRN-based data (e.g. a particular body name/ID mapping) */
/*     and only update these locally saved values if the ZZBODTRN state */
/*     has changed. */

/*     To make use of the ZZBODTRN state counter to achieve this goal the */
/*     caller routines save the ZZBODTRN state counter returned by the */
/*     first call to this routine and then check that saved value */
/*     against the current ZZBODTRN state counter and update it by */
/*     subsequent calls this routine. */

/*     This routine checks if the watched POOL name-ID mapping keywords */
/*     changed and if so updates the internally buffered POOL-based */
/*     name-ID mappings. */

/* $ Examples */

/*     The routines that need to be aware of and act on the ZZBODTRN */
/*     state change initialize a local ZZBODTRN counter array using */
/*     ZZCTRUIN, save it, and check it against the current ZZBODTRN */
/*     state counter and update it, if needed, using this entry point, */
/*     as follows: */

/*        C */
/*        C     Include zzctr.inc to access CTRSIZ. */
/*        C */
/*              INCLUDE              'zzctr.inc' */
/*              ... */

/*        C */
/*        C     In local variable declarations declare and save */
/*        C     the local ZZBODTRN state counter. Also declare the */
/*        C     update flag. */
/*        C */
/*              INTEGER               USRCTR ( CTRSIZ ) */
/*              LOGICAL               UPDATE */
/*              ... */
/*              SAVE                  USRCTR */
/*              ... */

/*        C */
/*        C     In all places where initialization is done */
/*        C     initialize the local ZZBODTRN state counter using */
/*        C     ZZCTRUIN to ensure an update on the first check. */
/*        C */
/*              IF ( FIRST ) THEN */
/*                 ... */
/*                 CALL ZZCTRUIN( USRCTR ) */
/*                 FIRST = .FALSE. */
/*              END IF */
/*              ... */

/*        C */
/*        C     In all places where there is a need to check for */
/*        C     the ZZBODTRN state change call this entry to */
/*        C     check and update the local POOL state counter. */
/*        C */
/*              CALL ZZBCTRCK ( USRCTR, UPDATE ) */

/*              IF ( UPDATE ) THEN */

/*        C */
/*        C        It the ZZBODTRN state changed, do what needs to */
/*        C        be done to deal with saved values based */
/*        C        on ZZBODTRN data. */
/*        C */
/*                 ... */

/*              END IF */

/* $ Restrictions */

/*     1) This is a private routine. See $Particulars above. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     B.V. Semenov    (JPL) */

/* $ Version */

/* -    SPICELIB Version 5.0.0, 16-SEP-2013 (BVS) */

/* -& */

/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }

/*     Check for updates to the kernel pool variables. */

    zzcvpool_("ZZBODTRN", pulctr, &lupdte, (ftnlen)8);
    if (lupdte || nodata) {

/*        Check in because ZZBODKER can fail. */

	chkin_("ZZBCTRCK", (ftnlen)8);

/*        Conservatively increment the ZZBODTRN state counter in */
/*        expectation of successful update. */

	zzctrinc_(subctr);

/*        Update kernel pool mapping lists and hashes. */

	zzbodker_(kernam, kernor, kercod, &kersiz, &extker, knmlst, knmpol, 
		knmnms, knmidx, kidlst, kidpol, kidids, kididx, (ftnlen)36, (
		ftnlen)36, (ftnlen)36);
	if (failed_()) {
	    nodata = TRUE_;
	    chkout_("ZZBCTRCK", (ftnlen)8);
	    return 0;
	}
	nodata = FALSE_;
	chkout_("ZZBCTRCK", (ftnlen)8);
    }

/*     Check the input counter against the ZZBODTRN counter. */

    zzctrchk_(subctr, usrctr, update);
    return 0;
} /* zzbodtrn_ */

/* Subroutine */ int zzbodtrn_(char *name__, integer *code, logical *found, 
	integer *usrctr, logical *update, ftnlen name_len)
{
    return zzbodtrn_0_(0, name__, code, found, usrctr, update, name_len);
    }

/* Subroutine */ int zzbodn2c_(char *name__, integer *code, logical *found, 
	ftnlen name_len)
{
    return zzbodtrn_0_(1, name__, code, found, (integer *)0, (logical *)0, 
	    name_len);
    }

/* Subroutine */ int zzbodc2n_(integer *code, char *name__, logical *found, 
	ftnlen name_len)
{
    return zzbodtrn_0_(2, name__, code, found, (integer *)0, (logical *)0, 
	    name_len);
    }

/* Subroutine */ int zzboddef_(char *name__, integer *code, ftnlen name_len)
{
    return zzbodtrn_0_(3, name__, code, (logical *)0, (integer *)0, (logical *
	    )0, name_len);
    }

/* Subroutine */ int zzbodkik_(void)
{
    return zzbodtrn_0_(4, (char *)0, (integer *)0, (logical *)0, (integer *)0,
	     (logical *)0, (ftnint)0);
    }

/* Subroutine */ int zzbodrst_(void)
{
    return zzbodtrn_0_(5, (char *)0, (integer *)0, (logical *)0, (integer *)0,
	     (logical *)0, (ftnint)0);
    }

/* Subroutine */ int zzbctrck_(integer *usrctr, logical *update)
{
    return zzbodtrn_0_(6, (char *)0, (integer *)0, (logical *)0, usrctr, 
	    update, (ftnint)0);
    }

