/* dafps.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure DAFPS ( DAF, pack summary ) */
/* Subroutine */ int dafps_0_(int n__, integer *nd, integer *ni, doublereal *
	dc, integer *ic, doublereal *sum)
{
    /* System generated locals */
    integer i__1, i__2;
    static doublereal equiv_0[125];

    /* Local variables */
    integer m, n;
    extern /* Subroutine */ int moved_(doublereal *, integer *, doublereal *),
	     movei_(integer *, integer *, integer *);
#define dequiv (equiv_0)
#define iequiv ((integer *)equiv_0)

/* $ Abstract */

/*     Pack (assemble) an array summary from its double precision and */
/*     integer components. */

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

/*     CONVERSION */
/*     FILES */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     ND         I   Number of double precision components. */
/*     NI         I   Number of integer components. */
/*     DC         I   Double precision components. */
/*     IC         I   Integer components. */
/*     SUM        O   Array summary. */

/* $ Detailed_Input */

/*     ND       is the number of double precision components in */
/*              the summary to be packed. */

/*     NI       is the number of integer components in the summary. */

/*     DC       are the double precision components of the summary. */

/*     IC       are the integer components of the summary. */

/* $ Detailed_Output */

/*     SUM      is an array summary containing the components in DC */
/*              and IC. This identifies the contents and location of */
/*              a single array within a DAF. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/*     1)  If ND is zero or negative, no DP components are stored. */

/*     2)  If NI is zero or negative, no integer components are stored. */

/*     3)  If the total size of the summary is greater than 125 double */
/*         precision words, some components may not be stored. See */
/*         $Particulars for details. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     The components of array summaries are packed into double */
/*     precision arrays for reasons outlined in [1]. Two routines, */
/*     DAFPS (pack summary) and DAFUS (unpack summary) are provided */
/*     for packing and unpacking summaries. */

/*     The total size of the summary is */

/*             (NI - 1) */
/*        ND + -------- + 1 */
/*                 2 */

/*     double precision words (where ND, NI are nonnegative). */

/* $ Examples */

/*     The numerical results shown for this example may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */

/*     1) Replace the body ID code 301 (Moon) with a test body ID, */
/*        e.g. -999, in every descriptor of an SPK file. */


/*        Example code begins here. */


/*              PROGRAM DAFPS_EX1 */
/*              IMPLICIT NONE */

/*        C */
/*        C     SPICELIB functions. */
/*        C */
/*              INTEGER               CARDI */

/*        C */
/*        C     Local parameters. */
/*        C */
/*              INTEGER               LBCELL */
/*              PARAMETER           ( LBCELL = -5   ) */

/*              INTEGER               DSCSIZ */
/*              PARAMETER           ( DSCSIZ = 5    ) */

/*              INTEGER               FILSIZ */
/*              PARAMETER           ( FILSIZ = 256  ) */

/*              INTEGER               MAXOBJ */
/*              PARAMETER           ( MAXOBJ = 1000 ) */

/*              INTEGER               ND */
/*              PARAMETER           ( ND     = 2    ) */

/*              INTEGER               NI */
/*              PARAMETER           ( NI     = 6    ) */

/*              INTEGER               NEWCOD */
/*              PARAMETER           ( NEWCOD = -999 ) */

/*              INTEGER               OLDCOD */
/*              PARAMETER           ( OLDCOD =  301 ) */

/*        C */
/*        C     Local variables. */
/*        C */
/*              CHARACTER*(FILSIZ)    FNAME */

/*              DOUBLE PRECISION      DC     ( ND ) */
/*              DOUBLE PRECISION      SUM    ( DSCSIZ ) */

/*              INTEGER               HANDLE */
/*              INTEGER               I */
/*              INTEGER               IC     ( NI ) */
/*              INTEGER               IDS    ( LBCELL : MAXOBJ ) */

/*              LOGICAL               FOUND */

/*        C */
/*        C     Get the SPK file name. */
/*        C */
/*              CALL PROMPT ( 'Enter name of the SPK file > ', FNAME ) */

/*        C */
/*        C     Initialize the set IDS. */
/*        C */
/*              CALL SSIZEI ( MAXOBJ, IDS ) */

/*        C */
/*        C     Open for writing the SPK file. */
/*        C */
/*              CALL DAFOPW ( FNAME, HANDLE ) */

/*        C */
/*        C     Search the file in forward order. */
/*        C */
/*              CALL DAFBFS ( HANDLE ) */
/*              CALL DAFFNA ( FOUND ) */

/*              DO WHILE ( FOUND ) */

/*        C */
/*        C        Fetch and unpack the descriptor (aka summary) */
/*        C        of the current segment. */
/*        C */
/*                 CALL DAFGS ( SUM ) */
/*                 CALL DAFUS ( SUM, ND, NI, DC, IC ) */

/*        C */
/*        C        Replace ID codes if necessary. */
/*        C */
/*                 IF ( IC(1) .EQ. OLDCOD ) THEN */

/*                    IC(1) = NEWCOD */

/*                 END IF */

/*                 IF ( IC(2) .EQ. OLDCOD ) THEN */

/*                    IC(2) = NEWCOD */

/*                 END IF */

/*        C */
/*        C        Re-pack the descriptor; replace the descriptor */
/*        C        in the file. */
/*        C */
/*                 CALL DAFPS ( ND, NI, DC, IC, SUM ) */
/*                 CALL DAFRS ( SUM ) */

/*        C */
/*        C        Find the next segment. */
/*        C */
/*                 CALL DAFFNA ( FOUND ) */

/*              END DO */

/*        C */
/*        C     Close the file. */
/*        C */
/*              CALL DAFCLS ( HANDLE ) */

/*        C */
/*        C     Find the set of objects in the SPK file. */
/*        C */
/*              CALL SPKOBJ ( FNAME, IDS ) */

/*              WRITE(*,'(A)') 'Objects in the DAF file:' */
/*              WRITE(*,*) ' ' */
/*              WRITE(*,'(20I4)') ( IDS(I), I= 1, CARDI ( IDS ) ) */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, using the SPK file named de430.bsp, the output was: */


/*        Enter name of the SPK file > de430.bsp */
/*        Objects in the DAF file: */

/*        -999   1   2   3   4   5   6   7   8   9  10 199 299 399 */


/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     H.A. Neilan        (JPL) */
/*     W.L. Taber         (JPL) */
/*     I.M. Underwood     (JPL) */
/*     E.D. Wright        (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 06-JUL-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. Added complete */
/*        code example. */

/* -    SPICELIB Version 1.0.3, 10-OCT-2012 (EDW) */

/*        Removed the obsolete Reference citation to "NAIF */
/*        Document 167.0." */

/*        Corrected ordering of header section. */

/* -    SPICELIB Version 1.0.2, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.1, 22-MAR-1990 (HAN) */

/*        Literature references added to the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU) */

/* -& */
/* $ Index_Entries */

/*     pack DAF summary */

/* -& */

/*     Local variables */


/*     Equivalences */


/*     Here's the deal: the DP components always precede the integer */
/*     components, avoiding alignment problems. The DP components can */
/*     be stored directly. */

    switch(n__) {
	case 1: goto L_dafus;
	}

/* Computing MIN */
    i__1 = 125, i__2 = max(0,*nd);
    n = min(i__1,i__2);
    moved_(dc, &n, sum);

/*     The integer components must detour through an equivalence. */

/* Computing MIN */
    i__1 = 250 - (n << 1), i__2 = max(0,*ni);
    m = min(i__1,i__2);
    movei_(ic, &m, iequiv);
    i__1 = (m - 1) / 2 + 1;
    moved_(dequiv, &i__1, &sum[n]);
    return 0;
/* $Procedure DAFUS ( DAF, unpack summary ) */

L_dafus:
/* $ Abstract */

/*     Unpack an array summary into its double precision and integer */
/*     components. */

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

/*     CONVERSION */
/*     FILES */

/* $ Declarations */

/*     DOUBLE PRECISION      SUM      ( * ) */
/*     INTEGER               ND */
/*     INTEGER               NI */
/*     DOUBLE PRECISION      DC       ( * ) */
/*     INTEGER               IC       ( * ) */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     SUM        I   Array summary. */
/*     ND         I   Number of double precision components. */
/*     NI         I   Number of integer components. */
/*     DC         O   Double precision components. */
/*     IC         O   Integer components. */

/* $ Detailed_Input */

/*     SUM      is an array summary. This identifies the contents and */
/*              location of a single array within a DAF. */

/*     ND       is the number of double precision components in */
/*              the summary. */

/*     NI       is the number of integer components in the summary. */

/* $ Detailed_Output */

/*     DC       are the double precision components of the summary. */

/*     IC       are the integer components of the summary. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/*     1)  If ND is zero or negative, no double precision components */
/*         are returned. */

/*     2)  If NI is zero or negative, no integer components are returned. */

/*     3)  If the total size of the summary is greater than 125 double */
/*         precision words, some components may not be returned. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     The components of array summaries are packed into double */
/*     precision arrays for reasons outlined in [1]. Two routines, */
/*     DAFPS (pack summary) and DAFUS (unpack summary) are provided */
/*     for packing and unpacking summaries. */

/*     The total size of the summary is */

/*             (NI - 1) */
/*        ND + -------- + 1 */
/*                 2 */

/*     double precision words (where ND, NI are nonnegative). */

/* $ Examples */

/*     The numerical results shown for this example may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */

/*     1) Use a simple routine to output the double precision and */
/*        integer values stored in an SPK's segments descriptors. This */
/*        function opens a DAF for read, performs a forwards search for */
/*        the DAF arrays, prints segments description for each array */
/*        found, then closes the DAF. */

/*        Use the SPK kernel below as input DAF file for the program. */

/*           de421.bsp */


/*        Example code begins here. */


/*              PROGRAM DAFUS_EX1 */
/*              IMPLICIT NONE */

/*        C */
/*        C     Define the summary parameters appropriate */
/*        C     for an SPK file. */
/*        C */
/*              INTEGER               MAXSUM */
/*              PARAMETER           ( MAXSUM = 125 ) */

/*              INTEGER               ND */
/*              PARAMETER           ( ND = 2       ) */

/*              INTEGER               NI */
/*              PARAMETER           ( NI = 6       ) */

/*        C */
/*        C     Local variables. */
/*        C */
/*              CHARACTER*(32)        KERNEL */

/*              DOUBLE PRECISION      DC     ( ND     ) */
/*              DOUBLE PRECISION      SUM    ( MAXSUM ) */

/*              INTEGER               HANDLE */
/*              INTEGER               IC     ( NI ) */

/*              LOGICAL               FOUND */


/*        C */
/*        C     Open a DAF for read. Return a HANDLE referring to the */
/*        C     file. */
/*        C */
/*              KERNEL = 'de421.bsp' */
/*              CALL DAFOPR ( KERNEL, HANDLE ) */

/*        C */
/*        C     Begin a forward search on the file. */
/*        C */
/*              CALL DAFBFS ( HANDLE ) */

/*        C */
/*        C     Search until a DAF array is found. */
/*        C */
/*              CALL DAFFNA ( FOUND ) */

/*        C */
/*        C     Loop while the search finds subsequent DAF arrays. */
/*        C */
/*              DO WHILE ( FOUND ) */

/*                 CALL DAFGS ( SUM ) */
/*                 CALL DAFUS ( SUM, ND, NI, DC, IC ) */

/*                 WRITE(*,*)                'Doubles:', DC(1:ND) */
/*                 WRITE(*, FMT='(A,6I9)' ) 'Integers:', IC(1:NI) */

/*        C */
/*        C        Check for another segment. */
/*        C */
/*                 CALL DAFFNA ( FOUND ) */

/*              END DO */

/*        C */
/*        C     Safely close the DAF. */
/*        C */
/*              CALL DAFCLS ( HANDLE ) */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*         Doubles:  -3169195200.0000000        1696852800.0000000 */
/*        Integers:        1        0        1        2      641   310404 */
/*         Doubles:  -3169195200.0000000        1696852800.0000000 */
/*        Integers:        2        0        1        2   310405   423048 */
/*         Doubles:  -3169195200.0000000        1696852800.0000000 */
/*        Integers:        3        0        1        2   423049   567372 */
/*         Doubles:  -3169195200.0000000        1696852800.0000000 */
/*        Integers:        4        0        1        2   567373   628976 */
/*         Doubles:  -3169195200.0000000        1696852800.0000000 */
/*        Integers:        5        0        1        2   628977   674740 */
/*         Doubles:  -3169195200.0000000        1696852800.0000000 */
/*        Integers:        6        0        1        2   674741   715224 */
/*         Doubles:  -3169195200.0000000        1696852800.0000000 */
/*        Integers:        7        0        1        2   715225   750428 */
/*         Doubles:  -3169195200.0000000        1696852800.0000000 */
/*        Integers:        8        0        1        2   750429   785632 */
/*         Doubles:  -3169195200.0000000        1696852800.0000000 */
/*        Integers:        9        0        1        2   785633   820836 */
/*         Doubles:  -3169195200.0000000        1696852800.0000000 */
/*        Integers:       10        0        1        2   820837   944040 */
/*         Doubles:  -3169195200.0000000        1696852800.0000000 */
/*        Integers:      301        3        1        2   944041  1521324 */
/*         Doubles:  -3169195200.0000000        1696852800.0000000 */
/*        Integers:      399        3        1        2  1521325  2098608 */
/*         Doubles:  -3169195200.0000000        1696852800.0000000 */
/*        Integers:      199        1        1        2  2098609  2098620 */
/*         Doubles:  -3169195200.0000000        1696852800.0000000 */
/*        Integers:      299        2        1        2  2098621  2098632 */
/*         Doubles:  -3169195200.0000000        1696852800.0000000 */
/*        Integers:      499        4        1        2  2098633  2098644 */


/*        Note, the final entries in the integer array contains the */
/*        segment start/end indexes. The output indicates the search */
/*        proceeded from the start of the file (low value index) towards */
/*        the end (high value index). */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     H.A. Neilan        (JPL) */
/*     W.L. Taber         (JPL) */
/*     I.M. Underwood     (JPL) */
/*     E.D. Wright        (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 04-AUG-2020 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard.. */
/*        Added undeclared variables to code example. */

/* -    SPICELIB Version 1.0.3, 10-OCT-2012 (EDW) */

/*        Added a functional code example to the $Examples section. */

/*        Removed the obsolete Reference citation to "NAIF */
/*        Document 167.0." */

/*        Corrected ordering of header section. */

/* -    SPICELIB Version 1.0.2, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.1, 22-MAR-1990 (HAN) */

/*        Literature references added to the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU) */

/* -& */
/* $ Index_Entries */

/*     unpack DAF summary */

/* -& */
/* $ Revisions */

/* -    SPICELIB Version 1.1.0, 04-AUG-2020 (JDR) */

/*        Added IMPLICIT NONE statement. */

/* -& */

/*     Just undo whatever DAFPS did. */

/* Computing MIN */
    i__1 = 125, i__2 = max(0,*nd);
    n = min(i__1,i__2);
    moved_(sum, &n, dc);
/* Computing MIN */
    i__1 = 250 - (n << 1), i__2 = max(0,*ni);
    m = min(i__1,i__2);
    i__1 = (m - 1) / 2 + 1;
    moved_(&sum[n], &i__1, dequiv);
    movei_(iequiv, &m, ic);
    return 0;
} /* dafps_ */

#undef iequiv
#undef dequiv


/* Subroutine */ int dafps_(integer *nd, integer *ni, doublereal *dc, integer 
	*ic, doublereal *sum)
{
    return dafps_0_(0, nd, ni, dc, ic, sum);
    }

/* Subroutine */ int dafus_(doublereal *sum, integer *nd, integer *ni, 
	doublereal *dc, integer *ic)
{
    return dafps_0_(1, nd, ni, dc, ic, sum);
    }

