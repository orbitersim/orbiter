/* dafec.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static logical c_false = FALSE_;
static integer c__1 = 1;
static integer c__5000 = 5000;

/* $Procedure DAFEC ( DAF extract comments ) */
/* Subroutine */ int dafec_(integer *handle, integer *bufsiz, integer *n, 
	char *buffer, logical *done, ftnlen buffer_len)
{
    /* Initialized data */

    static logical first = TRUE_;

    /* System generated locals */
    integer i__1, i__2, i__3;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer), i_len(char *, ftnlen), 
	    s_rdue(cilist *), do_uio(integer *, char *, ftnlen), e_rdue(void);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    integer free;
    extern integer cpos_(char *, char *, integer *, ftnlen, ftnlen);
    extern /* Subroutine */ int zzddhhlu_(integer *, char *, logical *, 
	    integer *, ftnlen);
    integer i__, j, k;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    integer ncomc, bward, fward, recno, index;
    logical found;
    integer ncomr;
    extern integer ncpos_(char *, char *, integer *, ftnlen, ftnlen);
    logical empty;
    char ch[1];
    integer nd;
    extern logical failed_(void);
    integer ni;
    extern /* Subroutine */ int dafsih_(integer *, char *, ftnlen);
    char ifname[60];
    static integer filhan[5000];
    static char crecrd[1000];
    extern /* Subroutine */ int dafrfr_(integer *, integer *, integer *, char 
	    *, integer *, integer *, integer *, ftnlen);
    static integer filchr[5000];
    integer daflun, nchars;
    static integer filcnt[5000];
    static char eocmrk[1];
    extern integer isrchi_(integer *, integer *, integer *);
    integer linlen;
    static integer nfiles;
    integer eocpos;
    static char eolmrk[1];
    static integer lsthan, lstrec[5000];
    extern /* Subroutine */ int chkout_(char *, ftnlen);
    integer numcom;
    extern /* Subroutine */ int sigerr_(char *, ftnlen);
    integer nelpos;
    extern /* Subroutine */ int errfnm_(char *, integer *, ftnlen);
    integer iostat;
    extern /* Subroutine */ int setmsg_(char *, ftnlen), errint_(char *, 
	    integer *, ftnlen);
    integer curpos;
    extern logical return_(void);
    static integer lstpos[5000];
    logical eol;

    /* Fortran I/O blocks */
    static cilist io___29 = { 1, 0, 1, 0, 0 };
    static cilist io___33 = { 1, 0, 1, 0, 0 };
    static cilist io___38 = { 1, 0, 1, 0, 0 };


/* $ Abstract */

/*     Extract comments from the comment area of a binary DAF. */

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

/*     DAF */

/* $ Keywords */

/*     FILES */
/*     UTILITY */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     HANDLE     I   Handle of binary DAF opened with read access. */
/*     BUFSIZ     I   Maximum size, in lines, of BUFFER. */
/*     N          O   Number of extracted comment lines. */
/*     BUFFER     O   Buffer where extracted comment lines are placed. */
/*     DONE       O   Indicates whether all comments have been extracted. */

/* $ Detailed_Input */

/*     HANDLE   is the file handle of a binary DAF which has been opened */
/*              with read access. */

/*     BUFSIZ   is the maximum number of comments that may be placed into */
/*              BUFFER. This would typically be the declared array size */
/*              for the Fortran character string array passed into this */
/*              routine. */

/* $ Detailed_Output */

/*     N        is the number of comment lines extracted from the comment */
/*              area of the binary DAF attached to HANDLE. This number */
/*              will be <= BUFSIZ on output. If N = BUFSIZ and DONE <> */
/*              .TRUE., then there are more comments left to to extract. */
/*              If N = 0, then DONE = .TRUE., i.e., there were no */
/*              comments in the comment area or we have extracted all */
/*              of the comments. If there are comments in the comment */
/*              area, or comments remaining after the extraction process */
/*              has begun, N > 0, always. */

/*     BUFFER   is an array of at most BUFSIZ comments which have been */
/*              extracted from the comment area of the binary DAF */
/*              attached to HANDLE. */

/*     DONE     is a logical flag indicating whether or not all of the */
/*              comment lines from the comment area of the DAF have */
/*              been read. This variable has the value .TRUE. after the */
/*              last comment line has been read. It will have the value */
/*              .FALSE. otherwise. */

/*              If there are no comments in the comment area, this */
/*              variable will have the value .TRUE., and N = 0. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the size of the output line buffer is is not positive, */
/*         the error SPICE(INVALIDARGUMENT) is signaled. */

/*     2)  If a comment line in a DAF is longer than the length */
/*         of a character string array element of BUFFER, the error */
/*         SPICE(COMMENTTOOLONG) is signaled. */

/*     3)  If the end of the comments cannot be found, i.e., the end of */
/*         comments marker is missing on the last comment record, the */
/*         error SPICE(BADCOMMENTAREA) is signaled. */

/*     4)  If the number of comment characters scanned exceeds the */
/*         number of comment characters computed, the error */
/*         SPICE(BADCOMMENTAREA) is signaled. */

/*     5)  If the binary DAF attached to HANDLE is not open for reading, */
/*         an error is signaled by a routine in the call tree of this */
/*         routine. */

/* $ Files */

/*     See argument HANDLE in $Detailed_Input. */

/* $ Particulars */

/*     A binary DAF contains an area which is reserved for storing */
/*     annotations or descriptive textual information describing the data */
/*     contained in a file. This area is referred to as the ``comment */
/*     area'' of the file. The comment area of a DAF is a line */
/*     oriented medium for storing textual information. The comment */
/*     area preserves any leading or embedded white space in the line(s) */
/*     of text which are stored, so that the appearance of the of */
/*     information will be unchanged when it is retrieved (extracted) at */
/*     some other time. Trailing blanks, however, are NOT preserved, */
/*     due to the way that character strings are represented in */
/*     standard Fortran 77. */

/*     This routine will read the comments from the comment area of */
/*     a binary DAF, placing them into a line buffer. If the line */
/*     buffer is not large enough to hold the entire comment area, */
/*     the portion read will be returned to the caller, and the DONE */
/*     flag will be set to .FALSE. This allows the comment area to be */
/*     read in ``chunks,'' a buffer at a time. After all of the comment */
/*     lines have been read, the DONE flag will be set to .TRUE. */

/*     This routine can be used to ``simultaneously'' extract comments */
/*     from the comment areas of multiple binary DAFs. See Example */
/*     2 in the $Examples section. */

/* $ Examples */

/*     The numerical results shown for these examples may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */

/*     1) The following example will extract a maximum of 30 lines */
/*        from the comment area of a binary DAF, displaying the */
/*        comments on the terminal screen. */

/*        Although it would be possible to read the 30 lines in one */
/*        go, for this example, use only a buffer size of 10, */
/*        demonstrating the use of the DONE logical flag. */


/*        Use the SPK kernel below as input DAF file for the program. */

/*           earthstns_itrf93_201023.bsp */


/*        Example code begins here. */


/*              PROGRAM DAFEC_EX1 */
/*              IMPLICIT NONE */

/*        C */
/*        C     SPICELIB functions */
/*        C */
/*              INTEGER               RTRIM */

/*        C */
/*        C     Local parameters */
/*        C */
/*              CHARACTER*(*)         KERNEL */
/*              PARAMETER           ( KERNEL = */
/*             .                         'earthstns_itrf93_201023.bsp' ) */

/*              INTEGER               BUFSIZ */
/*              PARAMETER           ( BUFSIZ = 10 ) */

/*              INTEGER               LINLEN */
/*              PARAMETER           ( LINLEN = 1000 ) */

/*        C */
/*        C     Local variables. */
/*        C */
/*              CHARACTER*(LINLEN)    BUFFER ( BUFSIZ ) */

/*              INTEGER               HANDLE */
/*              INTEGER               I */
/*              INTEGER               J */
/*              INTEGER               N */

/*              LOGICAL               DONE */

/*        C */
/*        C     Open a DAF for read. Return a HANDLE referring to the */
/*        C     file. */
/*        C */
/*              CALL DAFOPR ( KERNEL, HANDLE ) */

/*        C */
/*        C     Read a maximum of 30 lines of comments. */
/*        C */
/*              WRITE(*,'(A)') 'Comment area of input DAF file ' */
/*             .            // '(max. 30 lines): ' */
/*              WRITE(*,'(A)') '---------------------------------------' */
/*             .            // '-----------------------' */
/*              DO I = 0, 2 */

/*                 CALL DAFEC  ( HANDLE, BUFSIZ, N, BUFFER, DONE ) */

/*        C */
/*        C        Write the N lines to the terminal screen. */
/*        C */
/*                 DO J = 1, N */

/*                    WRITE (*,*) BUFFER(J)(:RTRIM(BUFFER(J))) */

/*                 END DO */

/*              END DO */

/*              WRITE(*,'(A)') '---------------------------------------' */
/*             .            // '-----------------------' */

/*        C */
/*        C     Have all the comments been read? */
/*        C */
/*              IF ( .NOT. DONE ) THEN */

/*                 WRITE(*,*) ' ' */
/*                 WRITE(*,*) 'Warning: Not all comments have been read!' */

/*              END IF */

/*        C */
/*        C     Safely close the DAF. */
/*        C */
/*              CALL DAFCLS ( HANDLE ) */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*        Comment area of input DAF file (max. 30 lines): */
/*        -------------------------------------------------------------- */

/*            SPK for DSN Station Locations */
/*            ========================================================*** */

/*            Original file name:                   earthstns_itrf93_2*** */
/*            Creation date:                        2020 October 28 12:30 */
/*            Created by:                           Nat Bachman  (NAIF*** */


/*            Introduction */
/*            ========================================================*** */

/*            This file provides geocentric states---locations and vel*** */
/*            set of DSN stations cited in the list below under "Posit*** */
/*            position vectors point from the earth's barycenter to th*** */
/*            velocities are estimates of the derivatives with respect*** */
/*            vectors; in this file, velocities are constant. Station *** */
/*            magnitudes on the order of a few cm/year. */

/*            The states in this file are given relative to the terres*** */
/*            frame ITRF93. */

/*            This SPK file has a companion file */

/*               earthstns_fx_201023.bsp */

/*            which differs from this one only in that it uses the ref*** */
/*            frame alias 'EARTH_FIXED'. See the comment area of that *** */
/*            and the Frames Required Reading for details. */

/*        -------------------------------------------------------------- */

/*         Warning: Not all comments have been read! */


/*        Warning: incomplete output. 12 lines extended past the right */
/*        margin of the header and have been truncated. These lines are */
/*        marked by "***" at the end of each line. */


/*     2) The following example demonstrates the use of this routine to */
/*        simultaneously read the comment areas of multiple DAFs. For */
/*        each file, the comments will be displayed on the screen as */
/*        they are extracted. */

/*        Use the SPK kernel below as the first DAF file for the */
/*        program. */

/*           earthstns_itrf93_201023.bsp */


/*        Use the CK kernel below as the second DAF file for the */
/*        program. */

/*           vo2_swu_ck2.bc */


/*        Example code begins here. */


/*              PROGRAM DAFEC_EX2 */
/*              IMPLICIT NONE */

/*        C */
/*        C     SPICELIB functions */
/*        C */
/*              INTEGER               RTRIM */

/*              LOGICAL               ALLTRU */

/*        C */
/*        C     Local parameters */
/*        C */
/*              INTEGER               BUFSIZ */
/*              PARAMETER           ( BUFSIZ = 10   ) */

/*              INTEGER               FNAMLN */
/*              PARAMETER           ( FNAMLN = 255  ) */

/*              INTEGER               LINLEN */
/*              PARAMETER           ( LINLEN = 1000 ) */

/*              INTEGER               NUMFIL */
/*              PARAMETER           ( NUMFIL = 2    ) */

/*        C */
/*        C     Local variables. */
/*        C */
/*              CHARACTER*(FNAMLN)    DAFNAM ( NUMFIL ) */
/*              CHARACTER*(LINLEN)    BUFFER ( BUFSIZ ) */

/*              INTEGER               HANDLE ( NUMFIL ) */
/*              INTEGER               I */
/*              INTEGER               J */
/*              INTEGER               N */

/*              LOGICAL               DONE   ( NUMFIL ) */

/*        C */
/*        C     Set the DAF file names. */
/*        C */
/*              DATA                  DAFNAM / */
/*             .                          'earthstns_itrf93_201023.bsp', */
/*             .                          'vo2_swu_ck2.bc'          / */

/*        C */
/*        C     Set the initial values for DONE and HANDLE. */
/*        C */
/*              DO I = 1, NUMFIL */

/*                 DONE(I)   = .FALSE. */
/*                 HANDLE(I) = 0 */

/*              END DO */

/*        C */
/*        C     Open the DAFs. */
/*        C */
/*              DO I = 1, NUMFIL */

/*                 CALL DAFOPR ( DAFNAM(I), HANDLE(I) ) */

/*              END DO */

/*        C */
/*        C     While there are still some comments left to read in at */
/*        C     least one of the files, read them and display them. */
/*        C */
/*              DO WHILE ( .NOT. ALLTRU( DONE, NUMFIL ) ) */

/*                 DO I = 1, NUMFIL */

/*                    IF ( .NOT. DONE(I) ) THEN */

/*                       WRITE (*,*) */
/*                       WRITE (*,*) 'File: ', */
/*             .                     DAFNAM(I)(:RTRIM(DAFNAM(I))) */
/*                       WRITE (*,*) '---------------------------------' */
/*             .                 //  '-----------------------------' */
/*                       N = 0 */

/*                       CALL DAFEC ( HANDLE(I), BUFSIZ, N, */
/*             .                      BUFFER,    DONE(I)   ) */

/*                       DO J = 1, N */

/*                          WRITE (*,*) BUFFER(J)(:RTRIM(BUFFER(J))) */

/*                       END DO */

/*                    END IF */

/*                 END DO */

/*              END DO */

/*        C */
/*        C     Safely close the DAF files. */
/*        C */
/*              DO I = 1, NUMFIL */

/*                 CALL DAFCLS ( HANDLE(I) ) */

/*              END DO */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*         File: earthstns_itrf93_201023.bsp */
/*         -------------------------------------------------------------- */

/*            SPK for DSN Station Locations */
/*            ========================================================*** */

/*            Original file name:                   earthstns_itrf93_2*** */
/*            Creation date:                        2020 October 28 12:30 */
/*            Created by:                           Nat Bachman  (NAIF*** */


/*            Introduction */

/*         File: vo2_swu_ck2.bc */
/*         -------------------------------------------------------------- */
/*         \beginlabel */
/*         PDS_VERSION_ID               = PDS3 */
/*         RECORD_TYPE                  = FIXED_LENGTH */
/*         RECORD_BYTES                 = 1024 */
/*         ^SPICE_KERNEL                = "vo2_swu_ck2.bc" */
/*         MISSION_NAME                 = VIKING */
/*         SPACECRAFT_NAME              = "VIKING ORBITER 2" */
/*         DATA_SET_ID                  = "VO1/VO2-M-SPICE-6-V1.0" */
/*         KERNEL_TYPE_ID               = CK */
/*         PRODUCT_ID                   = "vo2_swu_ck2.bc" */

/*         File: earthstns_itrf93_201023.bsp */
/*         -------------------------------------------------------------- */
/*            ========================================================*** */

/*            This file provides geocentric states---locations and vel*** */
/*            set of DSN stations cited in the list below under "Posit*** */
/*            position vectors point from the earth's barycenter to th*** */
/*            velocities are estimates of the derivatives with respect*** */
/*            vectors; in this file, velocities are constant. Station *** */
/*            magnitudes on the order of a few cm/year. */

/*            The states in this file are given relative to the terres*** */

/*         File: vo2_swu_ck2.bc */
/*         -------------------------------------------------------------- */
/*         PRODUCT_CREATION_TIME        = 2008-12-03T14:03:35 */
/*         PRODUCER_ID                  = "NAIF/JPL" */
/*         MISSION_PHASE_NAME           = { */
/*                                        PRIMARY_MISSION, */
/*                                        EXTENDED_MISSION, */
/*                                        CONTINUATION_MISSION */
/*                                        } */
/*         PRODUCT_VERSION_TYPE         = ACTUAL */
/*         PLATFORM_OR_MOUNTING_NAME    = "SPACECRAFT BUS" */
/*         START_TIME                   = 1977-01-09T18:33:12.707 */

/*         File: earthstns_itrf93_201023.bsp */
/*         -------------------------------------------------------------- */
/*            frame ITRF93. */

/*            This SPK file has a companion file */

/*               earthstns_fx_201023.bsp */

/*            which differs from this one only in that it uses the ref*** */
/*            frame alias 'EARTH_FIXED'. See the comment area of that *** */
/*            and the Frames Required Reading for details. */


/*         File: vo2_swu_ck2.bc */
/*         -------------------------------------------------------------- */
/*         STOP_TIME                    = 1977-11-27T05:55:16.772 */
/*         SPACECRAFT_CLOCK_START_COUNT = "1/0032380393.707" */
/*         SPACECRAFT_CLOCK_STOP_COUNT  = "1/0060155717.772" */
/*         TARGET_NAME                  = MARS */
/*         INSTRUMENT_NAME              = "SCAN PLATFORM" */
/*         NAIF_INSTRUMENT_ID           = -30000 */
/*         SOURCE_PRODUCT_ID            = "N/A" */
/*         NOTE                         = "See comments in the file fo*** */
/*         OBJECT                       = SPICE_KERNEL */
/*           INTERCHANGE_FORMAT         = BINARY */

/*         File: earthstns_itrf93_201023.bsp */
/*         -------------------------------------------------------------- */

/*            Revision description */
/*            -------------------- */

/*            This kernel contains data from a single, current source:*** */

/*            This kernel supersedes the kernels */

/*               earthstns_itrf93_050714.bsp */
/*               dss_53_prelim_itrf93_201018.bsp (data in this kernel *** */

/*         File: vo2_swu_ck2.bc */
/*         -------------------------------------------------------------- */
/*           KERNEL_TYPE                = POINTING */
/*           DESCRIPTION                = "VO2 instrument platform ori*** */
/*         reconstructed during cartographic image registration by S. *** */
/*         Type 2 CK file supersedes the less usable Type 1 CK vo2_swu*** */
/*         END_OBJECT                   = SPICE_KERNEL */
/*         \endlabel */

/*        [...] */


/*        Warning: incomplete output. Only 100 out of 1049 lines have */
/*        been provided. 18 lines extended past the right margin of the */
/*        header and have been truncated. These lines are marked by "***" */
/*        at the end of each line. */


/* $ Restrictions */

/*     1)  The comment area may consist only of printing ASCII */
/*         characters, decimal values 32 - 126. */

/*     2)  There is NO maximum length imposed on the significant portion */
/*         of a text line that may be placed into the comment area of a */
/*         DAF. The maximum length of a line stored in the comment area */
/*         should be kept reasonable, so that they may be easily */
/*         extracted. A good value for this would be 1000 characters, as */
/*         this can easily accommodate ``screen width'' lines as well as */
/*         long lines which may contain some other form of information. */

/*     3)  This routine is only used to read records on environments */
/*         whose characters are a single byte in size. Updates */
/*         to this routine and routines in its call tree may be */
/*         required to properly handle other cases. */

/*     4)  This routine is intended to be used on DAF files whose comment */
/*         area does not change while this routine is called to extract */
/*         comments, between the start and end of the extraction process. */
/*         If the comment area does change (gets updated, reduced, */
/*         extended, or deleted) between calls to this routine on the */
/*         same DAF file, the routine's outputs are undefined and */
/*         subsequent calls to it are likely to trigger an exception. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     K.R. Gehringer     (JPL) */
/*     B.V. Semenov       (JPL) */
/*     F.S. Turner        (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.2.0, 25-NOV-2021 (JDR) (BVS) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. Removed */
/*        unnecessary $Revisions section. */

/*        Added complete code examples from existing code fragments. */

/* -    SPICELIB Version 1.1.0, 12-APR-2012 (BVS) */

/*        Increased FTSIZE (from 1000 to 5000). */

/* -    SPICELIB Version 1.0.0, 08-NOV-2006 (NJB) (KRG) (FST) */

/*        Based on Support Version 2.0.0, 16-NOV-2001 (FST) */

/* -& */
/* $ Index_Entries */

/*     extract comments from a DAF */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */

/*     Length of a DAF internal filename. */


/*     Decimal value for the DAF comment area end-of-comment (EOC) */
/*     marker. */


/*     Decimal value for the DAF comment area end-of-line (EOL) marker. */


/*     The maximum number of DAFs that may be open simultaneously. */


/*     Length of a DAF character record, in characters. */


/*     Local variables */


/*     The file table declarations for keeping track of which files */
/*     are currently in the process of having comments extracted. */


/*     Saved variables */


/*     Save all of the file table information. */


/*     Initial values */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("DAFEC", (ftnlen)5);
    }

/*     If this is the first time that this routine has been called, */
/*     we need to initialize the character value of the end-of-line */
/*     marker, and the file table variables. */

    if (first) {
	first = FALSE_;
	nfiles = 0;
	lsthan = 0;
	*(unsigned char *)eocmrk = '\4';
	*(unsigned char *)eolmrk = '\0';
	for (i__ = 1; i__ <= 5000; ++i__) {
	    filchr[(i__1 = i__ - 1) < 5000 && 0 <= i__1 ? i__1 : s_rnge("fil"
		    "chr", i__1, "dafec_", (ftnlen)749)] = 0;
	    filcnt[(i__1 = i__ - 1) < 5000 && 0 <= i__1 ? i__1 : s_rnge("fil"
		    "cnt", i__1, "dafec_", (ftnlen)750)] = 0;
	    filhan[(i__1 = i__ - 1) < 5000 && 0 <= i__1 ? i__1 : s_rnge("fil"
		    "han", i__1, "dafec_", (ftnlen)751)] = 0;
	    lstpos[(i__1 = i__ - 1) < 5000 && 0 <= i__1 ? i__1 : s_rnge("lst"
		    "pos", i__1, "dafec_", (ftnlen)752)] = 0;
	    lstrec[(i__1 = i__ - 1) < 5000 && 0 <= i__1 ? i__1 : s_rnge("lst"
		    "rec", i__1, "dafec_", (ftnlen)753)] = 0;
	}
    }

/*     Verify that the DAF attached to HANDLE is opened for reading */
/*     by calling the routine to signal an invalid access mode on a */
/*     handle. */

    dafsih_(handle, "READ", (ftnlen)4);
    if (failed_()) {
	chkout_("DAFEC", (ftnlen)5);
	return 0;
    }

/*     Check for a nonpositive BUFFER size. */

    if (*bufsiz <= 0) {
	setmsg_("The output buffer size was not positive: #.", (ftnlen)43);
	errint_("#", bufsiz, (ftnlen)1);
	sigerr_("SPICE(INVALIDARGUMENT)", (ftnlen)22);
	chkout_("DAFEC", (ftnlen)5);
	return 0;
    }

/*     Convert the DAF handle to its corresponding Fortran logical */
/*     unit number for reading the comment records. */

    zzddhhlu_(handle, "DAF", &c_false, &daflun, (ftnlen)3);
    if (failed_()) {
	chkout_("DAFEC", (ftnlen)5);
	return 0;
    }

/*     Get the length of a single character string in the buffer. */

    linlen = i_len(buffer, buffer_len);

/*     If we have extracted comments from at least one file and we */
/*     didn't finish, check to see if HANDLE is in the file table. */

    if (nfiles > 0) {
	index = isrchi_(handle, &nfiles, filhan);
    } else {
	index = 0;
    }

/*     Check to see if we found HANDLE in the file handle table. If */
/*     we did, INDEX will be > 0. */

    if (index > 0) {

/*        Set the record number and the starting position accordingly, */
/*        i.e., where we left off when we last read from that file. */

	recno = lstrec[(i__1 = index - 1) < 5000 && 0 <= i__1 ? i__1 : s_rnge(
		"lstrec", i__1, "dafec_", (ftnlen)819)];
	curpos = lstpos[(i__1 = index - 1) < 5000 && 0 <= i__1 ? i__1 : 
		s_rnge("lstpos", i__1, "dafec_", (ftnlen)820)];
	nchars = filchr[(i__1 = index - 1) < 5000 && 0 <= i__1 ? i__1 : 
		s_rnge("filchr", i__1, "dafec_", (ftnlen)821)];
	ncomc = filcnt[(i__1 = index - 1) < 5000 && 0 <= i__1 ? i__1 : s_rnge(
		"filcnt", i__1, "dafec_", (ftnlen)822)];
    } else {

/*        We have not yet read any comments from this file, so start at */
/*        the start. To get to the first comment record, we need to skip */
/*        the file record. We also need to count the number of comment */
/*        characters. */

/*        Read the file record from the DAF attached to HANDLE. We will */
/*        get back some stuff that we do not use. */

	dafrfr_(handle, &nd, &ni, ifname, &fward, &bward, &free, (ftnlen)60);
	if (failed_()) {
	    chkout_("DAFEC", (ftnlen)5);
	    return 0;
	}

/*        Compute the number of comment records and the number of */
/*        comment characters. In order to perform these calculations, */
/*        we assume that we have a valid comment area in the DAF */
/*        attached to HANDLE. */

	ncomr = fward - 2;
	if (ncomr > 0) {

/*           The starting record number is the number of comment records */
/*           + 1 where the 1 skips the file record. */

	    empty = TRUE_;
	    found = FALSE_;
	    while(ncomr > 0 && ! found && empty) {
		recno = ncomr + 1;
		io___29.ciunit = daflun;
		io___29.cirec = recno;
		iostat = s_rdue(&io___29);
		if (iostat != 0) {
		    goto L100001;
		}
		iostat = do_uio(&c__1, crecrd, (ftnlen)1000);
		if (iostat != 0) {
		    goto L100001;
		}
		iostat = e_rdue();
L100001:
		if (iostat != 0) {
		    setmsg_("Error reading comment area of binary file named"
			    " '#'. IOSTAT = #.", (ftnlen)64);
		    errfnm_("#", &daflun, (ftnlen)1);
		    errint_("#", &iostat, (ftnlen)1);
		    sigerr_("SPICE(FILEREADFAILED)", (ftnlen)21);
		    chkout_("DAFEC", (ftnlen)5);
		    return 0;
		}

/*              Scan the comment record looking for the end of comments */
/*              marker. */

		eocpos = cpos_(crecrd, eocmrk, &c__1, (ftnlen)1000, (ftnlen)1)
			;
		if (eocpos > 0) {
		    found = TRUE_;
		} else {
		    nelpos = ncpos_(crecrd, eolmrk, &c__1, (ftnlen)1000, (
			    ftnlen)1);
		    if (nelpos != 0) {
			empty = FALSE_;
		    } else {
			--ncomr;
		    }
		}
	    }

/*           If we do not find the end of comments marker and the */
/*           comment area is not empty, then it is an error. */

	    if (! found && ! empty) {
		setmsg_("The comment area in the DAF file '#' may be damaged"
			". The end of the comments could not be found.", (
			ftnlen)96);
		errfnm_("#", &daflun, (ftnlen)1);
		sigerr_("SPICE(BADCOMMENTAREA)", (ftnlen)21);
		chkout_("DAFEC", (ftnlen)5);
		return 0;
	    } else if (found) {
		ncomc = (ncomr - 1) * 1000 + eocpos - 1;
	    } else if (empty) {
		ncomc = 0;
	    }
	} else {
	    ncomc = 0;
	}

/*        If the number of comment characters, NCOMC, is equal to zero, */
/*        then we have no comments to read, so set the number of comments */
/*        to zero, set DONE to .TRUE., check out,  and return. */

	if (ncomc == 0) {
	    *n = 0;
	    *done = TRUE_;
	    chkout_("DAFEC", (ftnlen)5);
	    return 0;
	}

/*        Otherwise, set the initial position  in the comment area. */

	recno = 2;
	curpos = 1;
	nchars = 0;
    }

/*     Begin reading the comment area into the buffer. */

    if (*handle != lsthan) {

/*        If the current DAF handle is not the same as the handle on */
/*        the last call, then we need to read in the appropriate record */
/*        from the DAF comment area. Otherwise the record was saved and */
/*        so we don't need to read it in. */

	io___33.ciunit = daflun;
	io___33.cirec = recno;
	iostat = s_rdue(&io___33);
	if (iostat != 0) {
	    goto L100002;
	}
	iostat = do_uio(&c__1, crecrd, (ftnlen)1000);
	if (iostat != 0) {
	    goto L100002;
	}
	iostat = e_rdue();
L100002:
	if (iostat != 0) {
	    setmsg_("Error reading comment area of binary file named FILE.  "
		    "IOSTAT = *.", (ftnlen)66);
	    errint_("*", &iostat, (ftnlen)1);
	    errfnm_("FILE", &daflun, (ftnlen)4);
	    sigerr_("SPICE(FILEREADFAILED)", (ftnlen)21);
	    chkout_("DAFEC", (ftnlen)5);
	    return 0;
	}
    }

/*     Initialize the BUFFER line counter, I, and the line position */
/*     counter, J. */

    i__ = 1;
    j = 1;

/*     Start filling up the BUFFER. */

    numcom = 0;
    *done = FALSE_;
    while(i__ <= *bufsiz && ! (*done)) {
	eol = FALSE_;
	while(! eol) {
	    ++nchars;
	    *(unsigned char *)ch = *(unsigned char *)&crecrd[curpos - 1];
	    if (*(unsigned char *)ch == 0) {
		eol = TRUE_;
		if (j <= linlen) {
		    s_copy(buffer + ((i__ - 1) * buffer_len + (j - 1)), " ", 
			    buffer_len - (j - 1), (ftnlen)1);
		}
	    } else {
		if (j <= linlen) {
		    *(unsigned char *)&buffer[(i__ - 1) * buffer_len + (j - 1)
			    ] = *(unsigned char *)ch;
		    ++j;
		} else {
		    setmsg_("The output buffer line length (#) was not long "
			    "enough to contain comment line #.", (ftnlen)80);
		    errint_("#", &linlen, (ftnlen)1);
		    errint_("#", &i__, (ftnlen)1);
		    sigerr_("SPICE(COMMENTTOOLONG)", (ftnlen)21);
		    chkout_("DAFEC", (ftnlen)5);
		    return 0;
		}
	    }

/*           If we have reached the end of the current comment record, */
/*           read in the next one and reset the current position. */
/*           Otherwise, just increment the current position. */

	    if (curpos == 1000) {
		++recno;
		io___38.ciunit = daflun;
		io___38.cirec = recno;
		iostat = s_rdue(&io___38);
		if (iostat != 0) {
		    goto L100003;
		}
		iostat = do_uio(&c__1, crecrd, (ftnlen)1000);
		if (iostat != 0) {
		    goto L100003;
		}
		iostat = e_rdue();
L100003:
		if (iostat != 0) {
		    setmsg_("Error reading comment area of binary file named"
			    " #.  IOSTAT = #.", (ftnlen)63);
		    errfnm_("#", &daflun, (ftnlen)1);
		    errint_("#", &iostat, (ftnlen)1);
		    sigerr_("SPICE(FILEREADFAILED)", (ftnlen)21);
		    chkout_("DAFEC", (ftnlen)5);
		    return 0;
		}
		curpos = 1;
	    } else {
		++curpos;
	    }

/*           Check to make sure that it is safe to continue, i.e., */
/*           that the number of comment characters we have processed */
/*           has not exceeded the number of comment characters in the */
/*           comment area of the DAF file. This should never happen. */

	    if (nchars > ncomc) {
		setmsg_("Count of comment characters (#) exceeds the number "
			"of comment characters (#) in the DAF file #.", (
			ftnlen)95);
		errint_("#", &nchars, (ftnlen)1);
		errint_("#", &ncomc, (ftnlen)1);
		errfnm_("#", &daflun, (ftnlen)1);
		sigerr_("SPICE(BADCOMMENTAREA)", (ftnlen)21);
		chkout_("DAFEC", (ftnlen)5);
		return 0;
	    }
	}

/*        We have just completed a comment line, so we save the comment */
/*        number, increment the buffer line counter, I, and reset the */
/*        buffer line position counter, J. */

	numcom = i__;
	++i__;
	j = 1;

/*        Check for the end of the comments. */

	if (nchars == ncomc) {

/*           If we have reached the end of the comments, signaled */
/*           by having processed all of the comment characters, NCOMC, */
/*           then we are done. So, set DONE to .TRUE. and remove the */
/*           entry for this file from the file table. */

	    *done = TRUE_;
	    lsthan = 0;

/*           0 <= INDEX <= NFILES, and we only want to remove things */
/*           from the file table if: */

/*              The file we are currently reading from is in the */
/*              file table, INDEX > 0, which implies NFILES > 0. */

/*           So, if INDEX > 0, we know that there are files in the file */
/*           table, and that we are currently reading from one of them. */

	    if (index > 0) {
		i__1 = nfiles - 1;
		for (k = index; k <= i__1; ++k) {
		    filchr[(i__2 = k - 1) < 5000 && 0 <= i__2 ? i__2 : s_rnge(
			    "filchr", i__2, "dafec_", (ftnlen)1114)] = filchr[
			    (i__3 = k) < 5000 && 0 <= i__3 ? i__3 : s_rnge(
			    "filchr", i__3, "dafec_", (ftnlen)1114)];
		    filcnt[(i__2 = k - 1) < 5000 && 0 <= i__2 ? i__2 : s_rnge(
			    "filcnt", i__2, "dafec_", (ftnlen)1115)] = filcnt[
			    (i__3 = k) < 5000 && 0 <= i__3 ? i__3 : s_rnge(
			    "filcnt", i__3, "dafec_", (ftnlen)1115)];
		    filhan[(i__2 = k - 1) < 5000 && 0 <= i__2 ? i__2 : s_rnge(
			    "filhan", i__2, "dafec_", (ftnlen)1116)] = filhan[
			    (i__3 = k) < 5000 && 0 <= i__3 ? i__3 : s_rnge(
			    "filhan", i__3, "dafec_", (ftnlen)1116)];
		    lstrec[(i__2 = k - 1) < 5000 && 0 <= i__2 ? i__2 : s_rnge(
			    "lstrec", i__2, "dafec_", (ftnlen)1117)] = lstrec[
			    (i__3 = k) < 5000 && 0 <= i__3 ? i__3 : s_rnge(
			    "lstrec", i__3, "dafec_", (ftnlen)1117)];
		    lstpos[(i__2 = k - 1) < 5000 && 0 <= i__2 ? i__2 : s_rnge(
			    "lstpos", i__2, "dafec_", (ftnlen)1118)] = lstpos[
			    (i__3 = k) < 5000 && 0 <= i__3 ? i__3 : s_rnge(
			    "lstpos", i__3, "dafec_", (ftnlen)1118)];
		}
		--nfiles;
	    }
	}
    }

/*     Set the number of comment lines in the buffer */

    *n = numcom;

/*     At this point, we have either filled the buffer or we have */
/*     finished reading in the comment area. Find out what has */
/*     happened and act accordingly. */

    if (! (*done)) {

/*        If we are not done, then we have filled the buffer, so save */
/*        everything that needs to be saved in the file table before */
/*        exiting. */

	if (index == 0) {

/*           This was the first time that the comment area of this file */
/*           has been read, so add it to the file table and save all of */
/*           its information if there is room in the file table. */

	    if (nfiles >= 5000) {
		setmsg_("The file table is full with # files, and another fi"
			"le could not be added.", (ftnlen)73);
		errint_("#", &c__5000, (ftnlen)1);
		sigerr_("SPICE(FILETABLEFULL)", (ftnlen)20);
		chkout_("DAFEC", (ftnlen)5);
		return 0;
	    }
	    ++nfiles;
	    filchr[(i__1 = nfiles - 1) < 5000 && 0 <= i__1 ? i__1 : s_rnge(
		    "filchr", i__1, "dafec_", (ftnlen)1162)] = nchars;
	    filcnt[(i__1 = nfiles - 1) < 5000 && 0 <= i__1 ? i__1 : s_rnge(
		    "filcnt", i__1, "dafec_", (ftnlen)1163)] = ncomc;
	    filhan[(i__1 = nfiles - 1) < 5000 && 0 <= i__1 ? i__1 : s_rnge(
		    "filhan", i__1, "dafec_", (ftnlen)1164)] = *handle;
	    lstrec[(i__1 = nfiles - 1) < 5000 && 0 <= i__1 ? i__1 : s_rnge(
		    "lstrec", i__1, "dafec_", (ftnlen)1165)] = recno;
	    lstpos[(i__1 = nfiles - 1) < 5000 && 0 <= i__1 ? i__1 : s_rnge(
		    "lstpos", i__1, "dafec_", (ftnlen)1166)] = curpos;
	    lsthan = *handle;
	} else {

/*           The comment area of this file is already in the file table, */
/*           so just update its information. */

	    filchr[(i__1 = index - 1) < 5000 && 0 <= i__1 ? i__1 : s_rnge(
		    "filchr", i__1, "dafec_", (ftnlen)1174)] = nchars;
	    lstrec[(i__1 = index - 1) < 5000 && 0 <= i__1 ? i__1 : s_rnge(
		    "lstrec", i__1, "dafec_", (ftnlen)1175)] = recno;
	    lstpos[(i__1 = index - 1) < 5000 && 0 <= i__1 ? i__1 : s_rnge(
		    "lstpos", i__1, "dafec_", (ftnlen)1176)] = curpos;
	    lsthan = *handle;
	}
    }
    chkout_("DAFEC", (ftnlen)5);
    return 0;
} /* dafec_ */

