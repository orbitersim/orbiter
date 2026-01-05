/* zzsrftrn.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;
static integer c__0 = 0;

/* $Procedure ZZSRFTRN ( Surface name/ID mapping umbrella ) */
/* Subroutine */ int zzsrftrn_0_(int n__, integer *bodyid, char *srfnam, 
	integer *surfid, integer *usrctr, logical *found, logical *update, 
	ftnlen srfnam_len)
{
    /* Initialized data */

    static logical extker = FALSE_;
    static integer polctr[2] = { 0,0 };
    static integer srfctr[2] = { 0,0 };
    static logical pass1 = TRUE_;

    /* System generated locals */
    integer i__1, i__2, i__3, i__4;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer), s_cmp(char *, char *, 
	    ftnlen, ftnlen);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    static integer node;
    extern /* Subroutine */ int zzctrchk_(integer *, integer *, logical *), 
	    zzctrinc_(integer *), zzsrfker_(char *, char *, integer *, 
	    integer *, logical *, integer *, integer *, integer *, integer *, 
	    integer *, integer *, integer *, ftnlen, ftnlen), zzctrsin_(
	    integer *), zzcvpool_(char *, integer *, logical *, ftnlen), 
	    zzctruin_(integer *), chkin_(char *, ftnlen);
    static integer nkvar;
    extern logical failed_(void);
    static integer kerbid[2000];
    static char kernam[36*2000];
    static integer kersid[2000], sididx[2003], sidhls[2003], itemat, lookat;
    static char nornam[36*2000];
    static integer sidpol[2009];
    static logical lupdte;
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen);
    static integer snmidx[2003];
    static char nsrfnm[36];
    static integer snmhls[2003], snmpol[2009];
    static char sqshnm[36];
    extern /* Subroutine */ int setmsg_(char *, ftnlen), ljucrs_(integer *, 
	    char *, char *, ftnlen, ftnlen);
    extern logical return_(void);
    extern /* Subroutine */ int cmprss_(char *, integer *, char *, char *, 
	    ftnlen, ftnlen, ftnlen);
    extern integer zzhash2_(char *, integer *, ftnlen), zzhashi_(integer *, 
	    integer *);

/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines.  Users should not call this routine directly due to the */
/*     volatile nature of this routine. */

/*     Umbrella routine for surface name/ID mapping entry points */
/*     and the data structures they use. */

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
/*     DSK */
/*     ID */
/*     NAME */
/*     STRING */
/*     SURFACE */

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

/*     Variable  I/O  Entry points */
/*     --------  ---  -------------------------------------------------- */
/*     BODYID     I   ZZSRFC2N, ZZSRFN2C */
/*     SRFNAM    I-O  ZZSRFC2N, ZZSRFN2C */
/*     SURFID    I-O  ZZSRFC2N, ZZSRFN2C */
/*     USRCTR    I-O  ZZSRFTRK */
/*     FOUND      O   ZZSRFC2N, ZZSRFN2C */
/*     UPDATE     O   ZZSRFTRK */

/* $ Detailed_Input */

/*     See the entry points for descriptions of their inputs. */

/* $ Detailed_Output */

/*     See the entry points for descriptions of their outputs. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1) If this routine is called directly, the error */
/*        SPICE(BOGUSENTRY) is signaled. */

/* $ Files */

/*     Surface-to-name mappings may be defined at run time by loading */
/*     text kernels containing kernel variable assignments of the form */

/*        NAIF_SURFACE_NAME += ( <surface name 1>, ... ) */
/*        NAIF_SURFACE_CODE += ( <surface code 1>, ... ) */
/*        NAIF_SURFACE_BODY += ( <body code 1>,    ... ) */

/*     Here the set of the three Ith list items on the right hand side */
/*     of the three assignments define the Ith surface name/ID mapping. */

/*     The same effect can be achieved using assignments formatted as */
/*     follows: */

/*        NAIF_SURFACE_NAME += <surface name 1> */
/*        NAIF_SURFACE_CODE += <surface code 1> */
/*        NAIF_SURFACE_BODY += <body code 1> */

/*        NAIF_SURFACE_NAME += <surface name 2> */
/*        NAIF_SURFACE_CODE += <surface code 2> */
/*        NAIF_SURFACE_BODY += <body code 2> */

/*           ... */

/*     Note the use of the */

/*        += */

/*     operator; this operator appends to rather than overwrites the */
/*     kernel variable named on the left hand side of the assignment. */

/* $ Particulars */

/*     This umbrella routine contains declarations of data structures */
/*     that support surface name/ID mapping. */

/*     This umbrella routine contains the following entry points: */

/*        ZZSRFC2N   {Surface code to name} */
/*        ZZSRFN2C   {Surface name to code} */
/*        ZZSRFTRK   {Track surface map updates} */


/* $ Examples */

/*     See the routines */

/*        SRFC2S */
/*        SRFCSS */
/*        SRFS2C */
/*        SRFSCC */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman    (JPL) */
/*     B.V. Semenov    (JPL) */
/*     E.D. Wright     (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.1, 05-NOV-2021 (NJB) */

/*        Corrected I/O descriptions in header of entry point ZZSRFC2N. */

/* -    SPICELIB Version 1.0.0, 01-APR-2016 (NJB) (EDW) (BVS) */

/* -& */
/* $ Index_Entries */

/*     surface name id mapping umbrella */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     Hash control area items. */


/*     Local variables */


/*     Data structures in this package */
/*     =============================== */

/*     The kernel variable table */
/*     ------------------------- */

/*     This table contains file scope arrays populated with the values */
/*     of the kernel variables that define the surface name/ID mapping. */
/*     These arrays contain */

/*        - surface names */
/*        - surface ID codes */
/*        - bodies associated with surface name/ID pairs */
/*        - an array of normalized names. These names are */
/*          upper case, left-justified, and compressed so that */
/*          the names contain no consecutive, embedded blanks */


/*     The surface ID table */
/*     -------------------- */

/*     This table enables pairs of surface IDs and body IDs to be mapped */
/*     to surface names. The table consists of */

/*        - a singly linked list pool */
/*        - a list head array */
/*        - a pointer array that maps pool nodes to */
/*          indices in the kernel variable table */

/*     The pointer array maps each node belonging to a collision list in */
/*     the pool to the index in the kernel table of the associated */
/*     values. The kernel table values used by this mapping are */

/*        - the  surface ID code */
/*        - the body ID code */
/*        - the original surface name */

/*     An integer hash function is used to map each surface ID to the */
/*     index in the list head array where the index of the head node for */
/*     that surface ID is located. */

/*     The layout of the structure is: */

/*                                                 Kernel variable table */
/*                                                 (only the portions */
/*                                                 used here are shown) */

/*                                                         body IDs */
/*                                                            | */
/*   +-- integer_hash( surface ID )                           |  original */
/*   |                                            surface IDs |  surface */
/*   |                                                   |    |   names */
/*   |                                                   |    |    | */
/*   |  list heads     list pool   pointer array         |    |    | */
/*   |  +---------+    +--------+  +-----------+        +--+ +--+ +--+ */
/*   |  |         |    |        |  |           |   +--->|  | |  | |  | */
/*   |  +---------+    +--------+  +-----------+   |    +--+ +--+ +--+ */
/*   +->|         |-+  | ^   *  |->|           |---+    |  | |  | |  | */
/*      +---------+ |  +-|---|--+  +-----------+        +--+ +--+ +--+ */
/*                  |    |   | */
/*          ...     |    |...|          ...                   ... */
/*                  |    |   | */
/*      +---------+ |  +-|---|--+  +-----------+        +--+ +--+ +--+ */
/*      |         | +->| *   |  |->|           |---+ +->|  | |  | |  | */
/*      +---------+    +-----|--+  +-----------+   | |  +--+ +--+ +--+ */
/*          ...           ...|          ...      +-|-+        ... */
/*      +---------+    +-----|--+  +-----------+ | |    +--+ +--+ +--+ */
/*      |         |    |     v  |->|           |-+ +--->|  | |  | |  | */
/*      +---------+    +--------+  +-----------+        +--+ +--+ +--+ */

/*      ----------------------------------------        -------------- */
/*                       NROOM                              MXNSRF */


/*      The diagram above is not to scale: the arrays on the left have */
/*      available length NROOM, while the arrays on the right have */
/*      length MXNSRF. */

/*      Note that the pool array is dimensioned (LBSNGL:NROOM). Elements */
/*      at indices -1 and 0 contain the size and location of the */
/*      first free element, respectively. */


/*     The surface name table */
/*     ---------------------- */

/*     This table enables pairs of surface names and body IDs to be */
/*     mapped to surface IDs. The structure is parallel to that of */
/*     the surface ID table; it contains */

/*        - a singly linked list pool */
/*        - a list head array */
/*        - a pointer array that maps pool nodes to */
/*          indices in the kernel variable table */

/*     The pointer array maps each node belonging to a collision list in */
/*     the pool to the index in the kernel table of the associated */
/*     values. The kernel table values used by this mapping are */

/*        - the normalized surface name */
/*        - the surface ID code */
/*        - the body ID code */

/*     An string hash function is used to map each surface name to the */
/*     index in the list head array where the index of the head node for */
/*     that surface ID is located. */

/*     The hash function is applied to the input string after it has */
/*     been normalized and then had all embedded blanks compressed out. */
/*     This allows the hash function terminate when it encounters the */
/*     first blank in the input string, while taking into account all */
/*     non-blank characters in the string. This makes it efficient while */
/*     enabling it to discriminate well between strings that may have */
/*     initial words in common. These compressed strings are not used */
/*     for any other purpose than hashing. For detection of the correct */
/*     matching elements in the kernel table, the normalized version */
/*     of the input string (which may contain blanks) is used. */

/*     The layout of the structure is: */


/*                                                 Kernel variable table */
/*                                                 (only the portions */
/*                                                 used here are shown) */

/*                                                         body IDs */
/*                                                            | */
/*   +-- string_hash(surface name)                            | */
/*   |                                         normalized     |  surface */
/*   |                                         surface names  |   IDs */
/*   |                                                   |    |    | */
/*   |                                                   |    |    | */
/*   |  list heads     list pool   pointer array         |    |    | */
/*   |  +---------+    +--------+  +-----------+        +--+ +--+ +--+ */
/*   |  |         |    |        |  |           |   +--->|  | |  | |  | */
/*   |  +---------+    +--------+  +-----------+   |    +--+ +--+ +--+ */
/*   +->|         |-+  | ^   *  |->|           |---+    |  | |  | |  | */
/*      +---------+ |  +-|---|--+  +-----------+        +--+ +--+ +--+ */
/*                  |    |   | */
/*          ...     |    |...|          ...                   ... */
/*                  |    |   | */
/*      +---------+ |  +-|---|--+  +-----------+        +--+ +--+ +--+ */
/*      |         | +->| *   |  |->|           |---+ +->|  | |  | |  | */
/*      +---------+    +-----|--+  +-----------+   | |  +--+ +--+ +--+ */
/*          ...           ...|          ...      +-|-+        ... */
/*      +---------+    +-----|--+  +-----------+ | |    +--+ +--+ +--+ */
/*      |         |    |     v  |->|           |-+ +--->|  | |  | |  | */
/*      +---------+    +--------+  +-----------+        +--+ +--+ +--+ */


/*      ----------------------------------------        -------------- */
/*                       NROOM                              MXNSRF */


/*      The diagram above is not to scale: the arrays on the left have */
/*      available length NROOM, while the arrays on the right have */
/*      length MXNSRF. */

/*      Note that the pool array is dimensioned (LBSNGL:NROOM). Elements */
/*      at indices -1 and 0 contain the size and location of the */
/*      first free element, respectively. */




/*     Declarations of data structures */
/*     =============================== */

/*        Kernel variable table */
/*        ===================== */

/*        Input names:                 KERNAM */
/*        Input surface IDs:           KERSID */
/*        Input body IDs:              KERBID */


/*        Normalized names:            NRMNAM */

/*     Each of these surface names is prefixed with an 11-character */
/*     string containing the associated body ID. */



/*        Surface ID table */
/*        ================ */

/*        Surface ID list heads:       SIDHLS */
/*        Surface ID pool:             SIDPOL */
/*        Surface ID name pointers:    SIDIDX */


/*        Surface Name table */
/*        ================== */

/*        Surface name list heads:     SNMHLS */
/*        Surface name pool:           SNMPOL */
/*        Surface name ID pointers:    SNMIDP */



/*     Other local declarations: */


/*     POLCTR tracks the state of the kernel pool. */
/*     SRFCTR tracks the state of the surface mapping */
/*     kernel variables. */



/*     Saved variables */


/*     Initial values */

    /* Parameter adjustments */
    if (usrctr) {
	}

    /* Function Body */
    switch(n__) {
	case 1: goto L_zzsrfn2c;
	case 2: goto L_zzsrfc2n;
	case 3: goto L_zzsrftrk;
	}

    if (return_()) {
	return 0;
    }
    chkin_("ZZSRFTRN", (ftnlen)8);
    setmsg_("ZZSRFTRN is an umbrella routine. It should never be called dire"
	    "ctly.", (ftnlen)68);
    sigerr_("SPICE(BOGUSENTRY)", (ftnlen)17);
    chkout_("ZZSRFTRN", (ftnlen)8);
    return 0;
/* $Procedure ZZSRFN2C ( Surface name to ID code mapping ) */

L_zzsrfn2c:
/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines.  Users should not call this routine directly due to the */
/*     volatile nature of this routine. */

/*     Map a surface name and body ID code to a surface ID code. */

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
/*     DSK */
/*     ID */
/*     NAME */
/*     STRING */
/*     SURFACE */

/* $ Declarations */

/*     CHARACTER*(*)         SRFNAM */
/*     INTEGER               BODYID */
/*     INTEGER               SURFID */
/*     LOGICAL               FOUND */

/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     SRFNAM     I   Surface name. */
/*     BODYID     I   Body ID code. */
/*     SURFID     O   Surface ID code. */
/*     FOUND      O   Found flag. */

/* $ Detailed_Input */

/*     SRFNAM     is the name of surface to be translated. */

/*     BODYID     is the ID code of a body with which the surface */
/*                designated by SRFNAM is associated. */

/* $ Detailed_Output */

/*     SURFID     is the surface ID code associated with the input */
/*                surface name and body ID code. */

/*                If multiple assignments for the input surface */
/*                name and body ID are present in the kernel pool, */
/*                the latest one is used to determine SURFID. */

/*     FOUND      is a logical flag that is .TRUE. if the inputs */
/*                were mapped to a surface ID code and .FALSE. */
/*                otherwise. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If an error occurs when the data structures of this package */
/*         are initialized, it will be signaled by a routine in the */
/*         call tree of this routine. */

/* $ Files */

/*     Surface-to-name mappings may be defined at run time by loading */
/*     text kernels containing kernel variable assignments of the form */

/*        NAIF_SURFACE_NAME += ( <surface name 1>, ... ) */
/*        NAIF_SURFACE_CODE += ( <surface code 1>, ... ) */
/*        NAIF_SURFACE_BODY += ( <body code 1>,    ... ) */

/*     Here the set of the three Ith list items on the right hand side */
/*     of the three assignments define the Ith surface name/ID mapping. */

/*     The same effect can be achieved using assignments formatted as */
/*     follows: */

/*        NAIF_SURFACE_NAME += <surface name 1> */
/*        NAIF_SURFACE_CODE += <surface code 1> */
/*        NAIF_SURFACE_BODY += <body code 1> */

/*        NAIF_SURFACE_NAME += <surface name 2> */
/*        NAIF_SURFACE_CODE += <surface code 2> */
/*        NAIF_SURFACE_BODY += <body code 2> */

/*           ... */

/*     Note the use of the */

/*        += */

/*     operator; this operator appends to rather than overwrites the */
/*     kernel variable named on the left hand side of the assignment. */

/* $ Particulars */

/*     This routine maps pairs of surface names and body ID codes to */
/*     surface ID codes. It relies on the mapping variables in the */
/*     kernel pool. */

/*     On the first pass through this routine, this routine */
/*     initializes the shared data structures of this package, */
/*     if the initialization has not already been done. */

/* $ Examples */

/*     See the routines */

/*        SRFS2C */
/*        SRFSCC */

/* $ Restrictions */

/*     This routine must not be called by user applications. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman    (JPL) */
/*     B.V. Semenov    (JPL) */
/*     E.D. Wright     (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 01-APR-2016 (NJB) (EDW) (BVS) */

/* -& */
/* $ Index_Entries */

/*     map surface name and body id to surface id */

/* -& */
    if (return_()) {
	return 0;
    }
    chkin_("ZZSRFN2C", (ftnlen)8);

/*     No result has been found. */

    *found = FALSE_;
    if (pass1) {

/*        Initialize the surface kernel variable update counter */
/*        and the local pool counter. Note that this routine */
/*        is a "subsystem" as seen by its callers and a "user" */
/*        with respect to the kernel pool. Hence the different */
/*        initializations. */

	zzctrsin_(srfctr);
	zzctruin_(polctr);

/*        Initialize local data structures. The first instance of this */
/*        call also sets a watch on the surface mapping kernel */
/*        variables. */

	zzsrfker_(kernam, nornam, kersid, kerbid, &extker, &nkvar, snmhls, 
		snmpol, snmidx, sidhls, sidpol, sididx, (ftnlen)36, (ftnlen)
		36);

/*        Sync POLCTR with the kernel pool counter. */

	zzcvpool_("ZZSRFTRN", polctr, &lupdte, (ftnlen)8);
	if (failed_()) {
	    chkout_("ZZSRFN2C", (ftnlen)8);
	    return 0;
	}
	pass1 = FALSE_;
    }

/*     Determine whether the data structures need to be updated */
/*     due to a change in the kernel pool contents. */

    zzcvpool_("ZZSRFTRN", polctr, &lupdte, (ftnlen)8);
    if (lupdte) {

/*        Conservatively increment the ZZSRFTRN state counter in */
/*        expectation of successful update. */

	zzctrinc_(srfctr);

/*        Initialize local data structures. */

	zzsrfker_(kernam, nornam, kersid, kerbid, &extker, &nkvar, snmhls, 
		snmpol, snmidx, sidhls, sidpol, sididx, (ftnlen)36, (ftnlen)
		36);
	if (failed_()) {
	    chkout_("ZZSRFN2C", (ftnlen)8);
	    return 0;
	}
    }

/*     No translation can be done if the surface mapping variables */
/*     are not in the pool. */

    if (! extker) {
	chkout_("ZZSRFN2C", (ftnlen)8);
	return 0;
    }

/*     Get a "normalized" copy of the input name: left-justified, */
/*     compressed, upper case. */

    ljucrs_(&c__1, srfnam, nsrfnm, srfnam_len, (ftnlen)36);

/*     Get a "squished" version of the above name: a version */
/*     containing no blanks. */

    cmprss_(" ", &c__0, nsrfnm, sqshnm, (ftnlen)1, (ftnlen)36, (ftnlen)36);

/*     Find the hash value of the squished input name. */

    lookat = zzhash2_(sqshnm, &snmpol[5], (ftnlen)36);
    node = snmhls[(i__1 = lookat - 1) < 2003 && 0 <= i__1 ? i__1 : s_rnge(
	    "snmhls", i__1, "zzsrftrn_", (ftnlen)742)];
    *found = FALSE_;
    if (node > 0) {

/*        Start at the head node and check each normalized name saved */
/*           for this hash value until we find a name and body ID that */
/*           match or run out of items in the collision list. */

	while(node > 0 && ! (*found)) {
	    *found = s_cmp(nsrfnm, nornam + ((i__2 = snmidx[(i__1 = node - 1) 
		    < 2003 && 0 <= i__1 ? i__1 : s_rnge("snmidx", i__1, "zzs"
		    "rftrn_", (ftnlen)754)] - 1) < 2000 && 0 <= i__2 ? i__2 : 
		    s_rnge("nornam", i__2, "zzsrftrn_", (ftnlen)754)) * 36, (
		    ftnlen)36, (ftnlen)36) == 0 && *bodyid == kerbid[(i__4 = 
		    snmidx[(i__3 = node - 1) < 2003 && 0 <= i__3 ? i__3 : 
		    s_rnge("snmidx", i__3, "zzsrftrn_", (ftnlen)754)] - 1) < 
		    2000 && 0 <= i__4 ? i__4 : s_rnge("kerbid", i__4, "zzsrf"
		    "trn_", (ftnlen)754)];
	    itemat = node;
	    node = snmpol[(i__1 = node + 5) < 2009 && 0 <= i__1 ? i__1 : 
		    s_rnge("snmpol", i__1, "zzsrftrn_", (ftnlen)758)];
	}

/*        ITEMAT is the value of the last node checked, or */
/*        0 if the list is empty. */

    }
    if (*found) {
	*surfid = kersid[(i__2 = snmidx[(i__1 = itemat - 1) < 2003 && 0 <= 
		i__1 ? i__1 : s_rnge("snmidx", i__1, "zzsrftrn_", (ftnlen)769)
		] - 1) < 2000 && 0 <= i__2 ? i__2 : s_rnge("kersid", i__2, 
		"zzsrftrn_", (ftnlen)769)];
    }
    chkout_("ZZSRFN2C", (ftnlen)8);
    return 0;
/* $Procedure ZZSRFC2N ( Surface ID code to name mapping ) */

L_zzsrfc2n:
/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines.  Users should not call this routine directly due to the */
/*     volatile nature of this routine. */

/*     Map a surface ID code and body ID code to a surface name. */

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
/*     DSK */
/*     ID */
/*     NAME */
/*     STRING */
/*     SURFACE */

/* $ Declarations */

/*     INTEGER               SURFID */
/*     INTEGER               BODYID */
/*     CHARACTER*(*)         SRFNAM */
/*     LOGICAL               FOUND */

/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     SURFID     I   Surface ID code. */
/*     BODYID     I   Body ID code. */
/*     SRFNAM     O   Surface name. */
/*     FOUND      O   Found flag. */

/* $ Detailed_Input */

/*     SURFID     is a surface ID code. */

/*     BODYID     is the ID code of a body with which the surface */
/*                designated by SURFID is associated. */

/* $ Detailed_Output */

/*     SRFNAM     is the name of the surface corresponding to the input */
/*                surface ID code and body ID code. */

/*                If multiple assignments for the input surface ID and */
/*                body ID are present in the kernel pool, the latest one */
/*                is used to determine SRFNAM. */

/*     FOUND      is a logical flag that is .TRUE. if the inputs */
/*                were mapped to a surface name and .FALSE. */
/*                otherwise. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If an error occurs when the data structures of this package */
/*         are initialized, it will be signaled by a routine in the */
/*         call tree of this routine. */

/* $ Files */

/*     Surface-to-name mappings may be defined at run time by loading */
/*     text kernels containing kernel variable assignments of the form */

/*        NAIF_SURFACE_NAME += ( <surface name 1>, ... ) */
/*        NAIF_SURFACE_CODE += ( <surface code 1>, ... ) */
/*        NAIF_SURFACE_BODY += ( <body code 1>,    ... ) */

/*     Here the set of the three Ith list items on the right hand side */
/*     of the three assignments define the Ith surface name/ID mapping. */

/*     The same effect can be achieved using assignments formatted as */
/*     follows: */

/*        NAIF_SURFACE_NAME += <surface name 1> */
/*        NAIF_SURFACE_CODE += <surface code 1> */
/*        NAIF_SURFACE_BODY += <body code 1> */

/*        NAIF_SURFACE_NAME += <surface name 2> */
/*        NAIF_SURFACE_CODE += <surface code 2> */
/*        NAIF_SURFACE_BODY += <body code 2> */

/*           ... */

/*     Note the use of the */

/*        += */

/*     operator; this operator appends to rather than overwrites the */
/*     kernel variable named on the left hand side of the assignment. */

/* $ Particulars */

/*     This routine maps pairs of surface ID codes and body ID codes to */
/*     surface names. It relies on the mapping variables in the kernel */
/*     pool. */

/*     On the first pass through this routine, this routine */
/*     initializes the shared data structures of this package, */
/*     if the initialization has not already been done. */

/* $ Examples */

/*     See the routines */

/*        SRFC2S */
/*        SRFCSS */

/* $ Restrictions */

/*     This routine must not be called by user applications. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman    (JPL) */
/*     B.V. Semenov    (JPL) */
/*     E.D. Wright     (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.1, 05-NOV-2021 (NJB) */

/*        Corrected errors in the $Brief_I/O and $Detailed_Output */
/*        sections of the header. */

/* -    SPICELIB Version 1.0.0, 01-APR-2016 (NJB) (EDW) (BVS) */

/* -& */
/* $ Index_Entries */

/*     map surface id and body id to surface name */

/* -& */
    if (return_()) {
	return 0;
    }
    chkin_("ZZSRFC2N", (ftnlen)8);

/*     No result has been found. */

    *found = FALSE_;
    if (pass1) {

/*        Initialize the surface kernel variable update counter */
/*        and the local pool counter. Note that this routine */
/*        is a "subsystem" as seen by its callers and a "user" */
/*        with respect to the kernel pool. Hence the different */
/*        initializations. */

	zzctrsin_(srfctr);
	zzctruin_(polctr);

/*        Initialize local data structures. The first instance of this */
/*        call also sets a watch on the surface mapping kernel */
/*        variables. */

	zzsrfker_(kernam, nornam, kersid, kerbid, &extker, &nkvar, snmhls, 
		snmpol, snmidx, sidhls, sidpol, sididx, (ftnlen)36, (ftnlen)
		36);

/*        Sync SRFCTR with the kernel pool counter. */

	zzcvpool_("ZZSRFTRN", polctr, &lupdte, (ftnlen)8);
	if (failed_()) {
	    chkout_("ZZSRFC2N", (ftnlen)8);
	    return 0;
	}
	pass1 = FALSE_;
    }

/*     Determine whether the data structures need to be updated */
/*     due to a change in the kernel pool contents. */

    zzcvpool_("ZZSRFTRN", polctr, &lupdte, (ftnlen)8);
    if (lupdte) {

/*        Conservatively increment the ZZSRFTRN state counter in */
/*        expectation of successful update. */

	zzctrinc_(srfctr);

/*        Initialize local data structures. */

	zzsrfker_(kernam, nornam, kersid, kerbid, &extker, &nkvar, snmhls, 
		snmpol, snmidx, sidhls, sidpol, sididx, (ftnlen)36, (ftnlen)
		36);
	if (failed_()) {
	    chkout_("ZZSRFC2N", (ftnlen)8);
	    return 0;
	}
    }

/*     No translation can be done if the surface mapping variables */
/*     are not in the pool. */

    if (! extker) {
	chkout_("ZZSRFC2N", (ftnlen)8);
	return 0;
    }

/*     Find the hash value of the squished input name. */

    lookat = zzhashi_(surfid, &sidpol[5]);
    node = sidhls[(i__1 = lookat - 1) < 2003 && 0 <= i__1 ? i__1 : s_rnge(
	    "sidhls", i__1, "zzsrftrn_", (ftnlen)1043)];
    *found = FALSE_;
    if (node > 0) {

/*        Start at the head node and check each normalized name saved */
/*           for this hash value until we find a name and body ID that */
/*           match or run out of items in the collision list. */

	while(node > 0 && ! (*found)) {
	    *found = *surfid == kersid[(i__2 = sididx[(i__1 = node - 1) < 
		    2003 && 0 <= i__1 ? i__1 : s_rnge("sididx", i__1, "zzsrf"
		    "trn_", (ftnlen)1055)] - 1) < 2000 && 0 <= i__2 ? i__2 : 
		    s_rnge("kersid", i__2, "zzsrftrn_", (ftnlen)1055)] && *
		    bodyid == kerbid[(i__4 = sididx[(i__3 = node - 1) < 2003 
		    && 0 <= i__3 ? i__3 : s_rnge("sididx", i__3, "zzsrftrn_", 
		    (ftnlen)1055)] - 1) < 2000 && 0 <= i__4 ? i__4 : s_rnge(
		    "kerbid", i__4, "zzsrftrn_", (ftnlen)1055)];
	    itemat = node;
	    node = sidpol[(i__1 = node + 5) < 2009 && 0 <= i__1 ? i__1 : 
		    s_rnge("sidpol", i__1, "zzsrftrn_", (ftnlen)1059)];
	}

/*        ITEMAT is the value of the last node checked, or */
/*        0 if the list is empty. */

    }
    if (*found) {
	s_copy(srfnam, kernam + ((i__2 = sididx[(i__1 = itemat - 1) < 2003 && 
		0 <= i__1 ? i__1 : s_rnge("sididx", i__1, "zzsrftrn_", (
		ftnlen)1070)] - 1) < 2000 && 0 <= i__2 ? i__2 : s_rnge("kern"
		"am", i__2, "zzsrftrn_", (ftnlen)1070)) * 36, srfnam_len, (
		ftnlen)36);
    }
    chkout_("ZZSRFC2N", (ftnlen)8);
    return 0;
/* $Procedure ZZSRFTRK ( Surface mapping tracker ) */

L_zzsrftrk:
/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines.  Users should not call this routine directly due to the */
/*     volatile nature of this routine. */

/*     Check a user counter against the surface mapping counter */
/*     maintained by this package. Indicate whether the caller */
/*     needs to update variables associated with the user counter. */

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
/*     DSK */
/*     ID */
/*     NAME */
/*     STRING */
/*     SURFACE */

/* $ Declarations */

/*     INTEGER               USRCTR ( * ) */
/*     LOGICAL               UPDATE */

/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     USRCTR    I-O  User counter. */
/*     UPDATE     O   Update flag. */

/* $ Detailed_Input */

/*     USRCTR     is a counter passed in by a calling routine. Normally */
/*                USRCTR would be used to enable the application calling */
/*                this routine to determine whether local associations */
/*                of surface names and IDs are up to date. */


/* $ Detailed_Output */

/*     USRCTR     is the user counter, updated if necessary to match the */
/*                counter maintained by this package. */


/*     UPDATE     is a logical flag indicating whether USRCTR was */
/*                updated. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If an error occurs when the data structures of this package */
/*         are initialized, it will be signaled by a routine in the */
/*         call tree of this routine. */

/* $ Files */

/*     Surface-to-name mappings may be defined at run time by loading */
/*     text kernels containing kernel variable assignments of the form */

/*        NAIF_SURFACE_NAME += ( <surface name 1>, ... ) */
/*        NAIF_SURFACE_CODE += ( <surface code 1>, ... ) */
/*        NAIF_SURFACE_BODY += ( <body code 1>,    ... ) */

/*     Here the set of the three Ith list items on the right hand side */
/*     of the three assignments define the Ith surface name/ID mapping. */

/*     The same effect can be achieved using assignments formatted as */
/*     follows: */

/*        NAIF_SURFACE_NAME += <surface name 1> */
/*        NAIF_SURFACE_CODE += <surface code 1> */
/*        NAIF_SURFACE_BODY += <body code 1> */

/*        NAIF_SURFACE_NAME += <surface name 2> */
/*        NAIF_SURFACE_CODE += <surface code 2> */
/*        NAIF_SURFACE_BODY += <body code 2> */

/*           ... */

/*     Note the use of the */

/*        += */

/*     operator; this operator appends to rather than overwrites the */
/*     kernel variable named on the left hand side of the assignment. */

/* $ Particulars */

/*     This routine allows SPICELIB routines to determine whether the */
/*     the surface mapping has been updated. */

/*     On every pass through this routine, this routine tests whether */
/*     the local mapping data structures need to be updated. If they do, */
/*     the data structures are re-initialized using the surface mapping */
/*     variables, if these are present in the kernel pool. */

/* $ Examples */

/*     See use of this routine in SPICELIB high-level geometry */
/*     routines such as */

/*        ILLUMF */
/*        SINCPT */
/*        SUBPNT */

/* $ Restrictions */

/*     This routine must not be called by user applications. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman    (JPL) */
/*     B.V. Semenov    (JPL) */
/*     E.D. Wright     (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 01-APR-2016 (NJB) (EDW) (BVS) */

/* -& */
/* $ Index_Entries */

/*     track surface mapping variable updates */

/* -& */

/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }
    if (pass1) {

/*        Check in because ZZSRFKER can fail. */

	chkin_("ZZSRFTRK", (ftnlen)8);

/*        Initialize the surface kernel variable update counter */
/*        and the local pool counter. Note that this routine */
/*        is a "subsystem" as seen by its callers and a "user" */
/*        with respect to the kernel pool. Hence the different */
/*        initializations. */

	zzctrsin_(srfctr);
	zzctruin_(polctr);

/*        Initialize local data structures. The first instance of this */
/*        call also sets a watch on the surface mapping kernel */
/*        variables. */

	zzsrfker_(kernam, nornam, kersid, kerbid, &extker, &nkvar, snmhls, 
		snmpol, snmidx, sidhls, sidpol, sididx, (ftnlen)36, (ftnlen)
		36);

/*        Sync SRFCTR with the kernel pool counter. */

	zzcvpool_("ZZSRFTRN", polctr, &lupdte, (ftnlen)8);

/*        Check out here since this routine doesn't check out */
/*        before its normal exit. */

	chkout_("ZZSRFTRK", (ftnlen)8);
	if (failed_()) {
	    return 0;
	}
	pass1 = FALSE_;
    }

/*     Check for updates to the kernel pool variables. */

    zzcvpool_("ZZSRFTRN", polctr, &lupdte, (ftnlen)8);
    if (lupdte) {

/*        Check in because ZZSRFKER can fail. */

	chkin_("ZZSRFTRK", (ftnlen)8);

/*        Conservatively increment the ZZSRFTRN state counter in */
/*        expectation of successful update. */

	zzctrinc_(srfctr);

/*        Update kernel pool mapping lists and hashes. */

	zzsrfker_(kernam, nornam, kersid, kerbid, &extker, &nkvar, snmhls, 
		snmpol, snmidx, sidhls, sidpol, sididx, (ftnlen)36, (ftnlen)
		36);
	chkout_("ZZSRFTRK", (ftnlen)8);
	if (failed_()) {
	    return 0;
	}
    }

/*     Check the input counter against the ZZSRFTRN counter; */
/*     sync the user counter. */

    zzctrchk_(srfctr, usrctr, update);
    return 0;
} /* zzsrftrn_ */

/* Subroutine */ int zzsrftrn_(integer *bodyid, char *srfnam, integer *surfid,
	 integer *usrctr, logical *found, logical *update, ftnlen srfnam_len)
{
    return zzsrftrn_0_(0, bodyid, srfnam, surfid, usrctr, found, update, 
	    srfnam_len);
    }

/* Subroutine */ int zzsrfn2c_(char *srfnam, integer *bodyid, integer *surfid,
	 logical *found, ftnlen srfnam_len)
{
    return zzsrftrn_0_(1, bodyid, srfnam, surfid, (integer *)0, found, (
	    logical *)0, srfnam_len);
    }

/* Subroutine */ int zzsrfc2n_(integer *surfid, integer *bodyid, char *srfnam,
	 logical *found, ftnlen srfnam_len)
{
    return zzsrftrn_0_(2, bodyid, srfnam, surfid, (integer *)0, found, (
	    logical *)0, srfnam_len);
    }

/* Subroutine */ int zzsrftrk_(integer *usrctr, logical *update)
{
    return zzsrftrn_0_(3, (integer *)0, (char *)0, (integer *)0, usrctr, (
	    logical *)0, update, (ftnint)0);
    }

