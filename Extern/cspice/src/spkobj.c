/* spkobj.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__2 = 2;
static integer c__6 = 6;

/* $Procedure SPKOBJ ( SPK objects ) */
/* Subroutine */ int spkobj_(char *spkfnm, integer *ids, ftnlen spkfnm_len)
{
    /* Builtin functions */
    integer s_cmp(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    char arch[80];
    extern /* Subroutine */ int dafgs_(doublereal *), chkin_(char *, ftnlen);
    doublereal descr[5];
    extern /* Subroutine */ int dafus_(doublereal *, integer *, integer *, 
	    doublereal *, integer *), errch_(char *, char *, ftnlen, ftnlen);
    logical found;
    doublereal dc[2];
    integer ic[6];
    extern /* Subroutine */ int daffna_(logical *);
    extern logical failed_(void);
    extern /* Subroutine */ int dafbfs_(integer *);
    integer handle;
    extern /* Subroutine */ int dafcls_(integer *), getfat_(char *, char *, 
	    char *, ftnlen, ftnlen, ftnlen), dafopr_(char *, integer *, 
	    ftnlen), sigerr_(char *, ftnlen), chkout_(char *, ftnlen), 
	    setmsg_(char *, ftnlen), insrti_(integer *, integer *);
    char kertyp[80];
    extern logical return_(void);

/* $ Abstract */

/*     Find the set of ID codes of all objects in a specified SPK file. */

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

/*     CELLS */
/*     DAF */
/*     NAIF_IDS */
/*     SETS */
/*     SPK */

/* $ Keywords */

/*     EPHEMERIS */
/*     UTILITY */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     SPKFNM     I   Name of SPK file. */
/*     IDS       I-O  Set of ID codes of objects in SPK file. */

/* $ Detailed_Input */

/*     SPKFNM   is the name of an SPK file. */

/*     IDS      is an initialized SPICE set data structure. IDS */
/*              optionally may contain a set of ID codes on input; on */
/*              output, the data already present in IDS will be combined */
/*              with ID code set found for the file SPKFNM. */

/*              If IDS contains no data on input, its size and */
/*              cardinality still must be initialized. */

/* $ Detailed_Output */

/*     IDS      is a SPICE set data structure which contains the union */
/*              of its contents upon input with the set of ID codes of */
/*              each object for which ephemeris data are present in the */
/*              indicated SPK file. The elements of SPICE sets are */
/*              unique; hence each ID code in IDS appears only once, even */
/*              if the SPK file contains multiple segments for that ID */
/*              code. */

/*              See the $Examples section below for a complete example */
/*              program showing how to retrieve the ID codes from IDS. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the input file has transfer format, the error */
/*         SPICE(INVALIDFORMAT) is signaled. */

/*     2)  If the input file is not a transfer file but has architecture */
/*         other than DAF, the error SPICE(INVALIDARCHTYPE) is signaled. */

/*     3)  If the input file is a binary DAF file of type other than SPK, */
/*         the error SPICE(INVALIDFILETYPE) is signaled. */

/*     4)  If the SPK file cannot be opened or read, an error is signaled */
/*         by a routine in the call tree of this routine. */

/*     5)  If the size of the output set argument IDS is insufficient to */
/*         contain the actual number of ID codes of objects covered by */
/*         the indicated SPK file, an error is signaled by a routine in */
/*         the call tree of this routine. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine provides an API via which applications can determine */
/*     the set of objects for which there are ephemeris data in a */
/*     specified SPK file. */

/* $ Examples */

/*     The numerical results shown for this example may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */

/*     1) This example demonstrates combined usage of SPKOBJ and the */
/*        related SPK utility SPKCOV. */

/*        Display the coverage for each object in a specified SPK file. */
/*        Find the set of objects in the file. Loop over the contents */
/*        of the ID code set: find the coverage for each item in the */
/*        set and display the coverage. */


/*        Example code begins here. */


/*              PROGRAM SPKOBJ_EX1 */
/*              IMPLICIT NONE */

/*        C */
/*        C     SPICELIB functions */
/*        C */
/*              INTEGER               CARDI */
/*              INTEGER               WNCARD */
/*        C */
/*        C     Local parameters */
/*        C */
/*        C */
/*        C     Declare the coverage window.  Make enough room */
/*        C     for MAXIV intervals. */
/*        C */
/*              INTEGER               FILSIZ */
/*              PARAMETER           ( FILSIZ = 255 ) */

/*              INTEGER               LBCELL */
/*              PARAMETER           ( LBCELL = -5 ) */

/*              INTEGER               MAXIV */
/*              PARAMETER           ( MAXIV  = 1000 ) */

/*              INTEGER               WINSIZ */
/*              PARAMETER           ( WINSIZ = 2 * MAXIV ) */

/*              INTEGER               TIMLEN */
/*              PARAMETER           ( TIMLEN = 50 ) */

/*              INTEGER               MAXOBJ */
/*              PARAMETER           ( MAXOBJ = 1000 ) */

/*        C */
/*        C     Local variables */
/*        C */
/*              CHARACTER*(FILSIZ)    LSK */
/*              CHARACTER*(FILSIZ)    SPKFNM */
/*              CHARACTER*(TIMLEN)    TIMSTR */

/*              DOUBLE PRECISION      B */
/*              DOUBLE PRECISION      COVER ( LBCELL : WINSIZ ) */
/*              DOUBLE PRECISION      E */

/*              INTEGER               I */
/*              INTEGER               IDS   ( LBCELL : MAXOBJ ) */
/*              INTEGER               J */
/*              INTEGER               NIV */


/*        C */
/*        C     Load a leapseconds kernel for output time conversion. */
/*        C     SPKCOV itself does not require a leapseconds kernel. */
/*        C */
/*              CALL PROMPT ( 'Name of leapseconds kernel > ', LSK ) */
/*              CALL FURNSH ( LSK ) */

/*        C */
/*        C     Get name of SPK file. */
/*        C */
/*              CALL PROMPT ( 'Name of SPK file           > ', SPKFNM ) */

/*        C */
/*        C     Initialize the set IDS. */
/*        C */
/*              CALL SSIZEI ( MAXOBJ, IDS ) */

/*        C */
/*        C     Initialize the window COVER. */
/*        C */
/*              CALL SSIZED ( WINSIZ, COVER ) */

/*        C */
/*        C     Find the set of objects in the SPK file. */
/*        C */
/*              CALL SPKOBJ ( SPKFNM, IDS ) */

/*        C */
/*        C     We want to display the coverage for each object.  Loop */
/*        C     over the contents of the ID code set, find the coverage */
/*        C     for each item in the set, and display the coverage. */
/*        C */
/*              DO I = 1, CARDI( IDS ) */
/*        C */
/*        C        Find the coverage window for the current */
/*        C        object. Empty the coverage window each time */
/*        C        so we don't include data for the previous object. */
/*        C */
/*                 CALL SCARDD ( 0,      COVER ) */
/*                 CALL SPKCOV ( SPKFNM, IDS(I), COVER ) */

/*        C */
/*        C        Get the number of intervals in the coverage */
/*        C        window. */
/*        C */
/*                 NIV = WNCARD ( COVER ) */

/*        C */
/*        C        Display a simple banner. */
/*        C */
/*                 WRITE (*,*) '========================================' */
/*                 WRITE (*,*) 'Coverage for object ', IDS(I) */

/*        C */
/*        C        Convert the coverage interval start and stop */
/*        C        times to TDB calendar strings. */
/*        C */
/*                 DO J = 1, NIV */
/*        C */
/*        C           Get the endpoints of the Jth interval. */
/*        C */
/*                    CALL WNFETD ( COVER, J, B, E ) */
/*        C */
/*        C           Convert the endpoints to TDB calendar */
/*        C           format time strings and display them. */
/*        C */
/*                    CALL TIMOUT ( B, */
/*             .                    'YYYY MON DD HR:MN:SC.### ' // */
/*             .                    '(TDB) ::TDB', */
/*             .                    TIMSTR                        ) */
/*                    WRITE (*,*) ' ' */
/*                    WRITE (*,*) 'Interval: ', J */
/*                    WRITE (*,*) 'Start:    ', TIMSTR */

/*                    CALL TIMOUT ( E, */
/*             .                    'YYYY MON DD HR:MN:SC.### ' // */
/*             .                    '(TDB) ::TDB', */
/*             .                    TIMSTR                        ) */
/*                    WRITE (*,*) 'Stop:     ', TIMSTR */
/*                    WRITE (*,*) ' ' */

/*                 END DO */

/*                 WRITE (*,*) '========================================' */

/*              END DO */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, using the LSK file named naif0012.tls and the SPK */
/*        file named mar097.bsp, the output was: */


/*        Name of leapseconds kernel > naif0012.tls */
/*        Name of SPK file           > mar097.bsp */
/*         ======================================== */
/*         Coverage for object            3 */

/*         Interval:            1 */
/*         Start:    1900 JAN 04 00:00:41.184 (TDB) */
/*         Stop:     2100 JAN 01 00:01:07.183 (TDB) */

/*         ======================================== */
/*         ======================================== */
/*         Coverage for object            4 */

/*         Interval:            1 */
/*         Start:    1900 JAN 04 00:00:41.184 (TDB) */
/*         Stop:     2100 JAN 01 00:01:07.183 (TDB) */

/*         ======================================== */
/*         ======================================== */
/*         Coverage for object           10 */

/*         Interval:            1 */
/*         Start:    1900 JAN 04 00:00:41.184 (TDB) */
/*         Stop:     2100 JAN 01 00:01:07.183 (TDB) */

/*         ======================================== */
/*         ======================================== */
/*         Coverage for object          399 */

/*         Interval:            1 */
/*         Start:    1900 JAN 04 00:00:41.184 (TDB) */
/*         Stop:     2100 JAN 01 00:01:07.183 (TDB) */

/*         ======================================== */
/*         ======================================== */
/*         Coverage for object          401 */

/*         Interval:            1 */
/*         Start:    1900 JAN 04 00:00:41.184 (TDB) */
/*         Stop:     2100 JAN 01 00:01:07.183 (TDB) */

/*         ======================================== */
/*         ======================================== */
/*         Coverage for object          402 */

/*         Interval:            1 */
/*         Start:    1900 JAN 04 00:00:41.184 (TDB) */
/*         Stop:     2100 JAN 01 00:01:07.183 (TDB) */

/*         ======================================== */
/*         ======================================== */
/*         Coverage for object          499 */

/*         Interval:            1 */
/*         Start:    1900 JAN 04 00:00:41.184 (TDB) */
/*         Stop:     2100 JAN 01 00:01:07.183 (TDB) */

/*         ======================================== */


/* $ Restrictions */

/*     1)  If an error occurs while this routine is updating the set */
/*         IDS, the set may be corrupted. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 08-OCT-2021 (JDR) (NJB) */

/*        Changed input argument name "SPK" to "SPKFNM" for consistency */
/*        with other routines. */

/*        Bug fix: added call to FAILED after call to GETFAT. */

/*        Edited the header to comply with NAIF standard. Added */
/*        solution to code example. */

/*        Corrected short error message in entries #2 and #3 in */
/*        $Exceptions section. Added NAIF_IDS to $Required_Reading */
/*        section. */

/* -    SPICELIB Version 1.0.2, 01-JUL-2014 (NJB) */

/*        Updated index entries. */

/* -    SPICELIB Version 1.0.1, 30-NOV-2007 (NJB) */

/*        Corrected bug in program in header $Examples section: */
/*        program now empties the coverage window prior to collecting */
/*        data for the current object. Deleted declaration of unused */
/*        parameter NAMLEN in example program. Updated example to */
/*        use WNCARD rather than CARDD. */

/* -    SPICELIB Version 1.0.0, 30-DEC-2004 (NJB) */

/* -& */
/* $ Index_Entries */

/*     find id codes of ephemeris objects in SPK file */
/*     find id codes of bodies in SPK file */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     Local variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }
    chkin_("SPKOBJ", (ftnlen)6);

/*     See whether GETFAT thinks we've got an SPK file. */

    getfat_(spkfnm, arch, kertyp, spkfnm_len, (ftnlen)80, (ftnlen)80);
    if (failed_()) {
	chkout_("SPKOBJ", (ftnlen)6);
	return 0;
    }
    if (s_cmp(arch, "XFR", (ftnlen)80, (ftnlen)3) == 0) {
	setmsg_("Input file # has architecture #. The file must be a binary "
		"SPK file to be readable by this routine.  If the input file "
		"is an SPK file in transfer format, run TOBIN on the file to "
		"convert it to binary format.", (ftnlen)207);
	errch_("#", spkfnm, (ftnlen)1, spkfnm_len);
	errch_("#", arch, (ftnlen)1, (ftnlen)80);
	sigerr_("SPICE(INVALIDFORMAT)", (ftnlen)20);
	chkout_("SPKOBJ", (ftnlen)6);
	return 0;
    } else if (s_cmp(arch, "DAF", (ftnlen)80, (ftnlen)3) != 0) {
	setmsg_("Input file # has architecture #. The file must be a binary "
		"SPK file to be readable by this routine.  Binary SPK files h"
		"ave DAF architecture.  If you expected the file to be a bina"
		"ry SPK file, the problem may be due to the file being an old"
		" non-native file lacking binary file format information. It'"
		"s also possible the file has been corrupted.", (ftnlen)343);
	errch_("#", spkfnm, (ftnlen)1, spkfnm_len);
	errch_("#", arch, (ftnlen)1, (ftnlen)80);
	sigerr_("SPICE(INVALIDARCHTYPE)", (ftnlen)22);
	chkout_("SPKOBJ", (ftnlen)6);
	return 0;
    } else if (s_cmp(kertyp, "SPK", (ftnlen)80, (ftnlen)3) != 0) {
	setmsg_("Input file # has file type #. The file must be a binary SPK"
		" file to be readable by this routine. If you expected the fi"
		"le to be a binary SPK file, the problem may be due to the fi"
		"le being an old non-native file lacking binary file format i"
		"nformation. It's also possible the file has been corrupted.", 
		(ftnlen)298);
	errch_("#", spkfnm, (ftnlen)1, spkfnm_len);
	errch_("#", kertyp, (ftnlen)1, (ftnlen)80);
	sigerr_("SPICE(INVALIDFILETYPE)", (ftnlen)22);
	chkout_("SPKOBJ", (ftnlen)6);
	return 0;
    }

/*     Open the file for reading. */

    dafopr_(spkfnm, &handle, spkfnm_len);
    if (failed_()) {
	chkout_("SPKOBJ", (ftnlen)6);
	return 0;
    }

/*     We will examine each segment descriptor in the file, and */
/*     we'll update our ID code set according to the data found */
/*     in these descriptors. */

/*     Start a forward search. */

    dafbfs_(&handle);

/*     Find the next DAF array. */

    daffna_(&found);
    while(found && ! failed_()) {

/*        Fetch and unpack the segment descriptor. */

	dafgs_(descr);
	dafus_(descr, &c__2, &c__6, dc, ic);

/*        Insert the current ID code into the output set. */
/*        The insertion algorithm will handle duplicates; no special */
/*        action is required here. */

	insrti_(ic, ids);
	daffna_(&found);
    }

/*     Release the file. */

    dafcls_(&handle);
    chkout_("SPKOBJ", (ftnlen)6);
    return 0;
} /* spkobj_ */

