/* scanit.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure SCANIT ( Scan a character string ) */
/* Subroutine */ int scanit_0_(int n__, char *string, integer *start, integer 
	*room, integer *nmarks, char *marks, integer *mrklen, integer *pnters,
	 integer *ntokns, integer *ident, integer *beg, integer *end, ftnlen 
	string_len, ftnlen marks_len)
{
    /* System generated locals */
    integer i__1, i__2, i__3;

    /* Builtin functions */
    integer i_len(char *, ftnlen), s_cmp(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    integer last, jump, test, slot, stop, last1, this1, i__, j, l, n, fchar, 
	    lchar;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    logical equal;
    extern integer ncpos_(char *, char *, integer *, ftnlen, ftnlen);
    logical known;
    extern integer rtrim_(char *, ftnlen);
    extern /* Subroutine */ int ljust_(char *, char *, ftnlen, ftnlen);
    integer eblock, backup, finish, lbound, offset;
    extern /* Subroutine */ int rmdupc_(integer *, char *, ftnlen);
    integer ubound, intval;
    extern /* Subroutine */ int sigerr_(char *, ftnlen);
    char letter[1];
    extern /* Subroutine */ int chkout_(char *, ftnlen), setmsg_(char *, 
	    ftnlen);
    extern logical return_(void);

/* $ Abstract */

/*     This routine serves as an umbrella routine for routines */
/*     that are used to scan a string for recognized and unrecognized */
/*     substrings. */

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

/*     PARSE */
/*     SEARCH */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     STRING     I   a string to be scanned. */
/*     ROOM       I   space available for located substrings. */
/*     NMARKS    I-O  number of recognizable substrings. */
/*     MARKS     I-O  recognizable substrings. */
/*     MRKLEN    I-O  an auxiliary array describing MARKS. */
/*     PNTERS    I-O  an auxiliary array describing MARKS. */
/*     START     I-O  position from which to commence/resume scanning. */
/*     NTOKNS     O   number of scanned substrings. */
/*     BEG        O   beginnings of scanned substrings. */
/*     END        O   endings of scanned substrings. */
/*     IDENT      O   position of scanned substring within array MARKS. */

/* $ Detailed_Input */

/*     STRING   is any character string that is to be scanned */
/*              to locate recognized and unrecognized substrings. */

/*     ROOM     is the amount of space available for storing the */
/*              results of scanning the string. */

/*     NMARKS   is the number of marks that will be */
/*              recognized substrings of STRING. */

/*     MARKS    is an array of marks that will be recognized */
/*              by the scanning routine. The array must be */
/*              processed by a call to SCANPR before it can */
/*              be used by SCAN. Further details are given */
/*              in documentation for the individual entry points. */

/*     MRKLEN   is an auxiliary array populated by SCANPR */
/*              for use by SCAN. It should be declared with */
/*              length equal to the length of MARKS. */

/*     PNTERS   is an auxiliary array populated by SCANPR for */
/*              use by SCAN. It should be declared in the */
/*              calling program as */

/*                 INTEGER  PNTERS ( RCHARS ) */

/*              RCHARS is given by the expression */

/*                MAX - MIN + 5 */

/*              where */

/*              MAX is the maximum value of ICHAR(MARKS(I)(1:1)) */
/*                  over the range I = 1, NMARKS */

/*              MIN is the minimum value of ICHAR(MARKS(I)(1:1)) */
/*                  over the range I = 1, NMARKS */

/*               Further details are provided in the entry point */
/*               SCANPR. */

/*     START    is the position in the STRING from which scanning */
/*              should commence. */

/* $ Detailed_Output */

/*     NMARKS   is the number of marks in the array MARKS after it */
/*              has been prepared for SCANPR. */

/*     MARKS    is an array of recognizable substrings that has */
/*              been prepared for SCAN by SCANPR. Note that MARKS */
/*              will be sorted in increasing order. */

/*     MRKLEN   is an auxiliary array, populated by SCANPR for */
/*              use by SCAN. */

/*     PNTERS   is an auxiliary array, populated by a call to */
/*              SCANPR and is intended for use by SCAN. */

/*     START    is the position from which scanning should continue */
/*              in order to fully scan STRING (if sufficient memory was */
/*              not provided in BEG, END, and IDENT on the current */
/*              call to SCAN). */

/*     NTOKNS   is the number of substrings identified in the current */
/*              scan of STRING. */

/*     BEG      beginnings of scanned substrings. */
/*              This should be declared so that it is at least */
/*              as large as ROOM. */

/*     END      endings of scanned substrings. */
/*              This should be declared so that it is at least */
/*              as large as ROOM. */

/*     IDENT    positions of scanned substring within array MARKS. */
/*              If the substring STRING(BEG(I):END(I)) is not in the */
/*              list of MARKS then IDENT(I) will have the value 0. */
/*              This should be declared so that it is at least */
/*              as large as ROOM. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If this routine is called directly, the error */
/*         SPICE(BOGUSENTRY) is signaled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine serves as an umbrella routine for the two entry */
/*     points SCANPR and SCAN. It can be used to locate keywords */
/*     or delimited substrings within a string. */

/*     The process of breaking a string into those substrings that */
/*     have recognizable meaning, is called "scanning." The substrings */
/*     identified by the scanning process are called "tokens." */

/*     Scanning has many applications including: */

/*     -- the parsing of algebraic expressions */

/*     -- parsing calendar dates */

/*     -- processing text with embedded directions for displaying */
/*        the text. */

/*     -- interpretation of command languages */

/*     -- compilation of programming languages */

/*     This routine simplifies the process of scanning a string for */
/*     its tokens. */

/* $ Examples */

/*     Example 1. */
/*     ---------- */

/*     Suppose you need to identify all of the words within a string */
/*     and wish to ignore punctuation marks such as ',', ':', ';', ' ', */
/*     '---'. */

/*     The first step is to load the array of marks as shown here: */

/*        The minimum ASCII code for the first character of a marker is */
/*        32 ( for ' '). */

/*        INTEGER               FCHAR */
/*        PARAMETER           ( FCHAR = 32 ) */

/*        The maximum ASCII code for the first character of a marker is */
/*        59 (for ';' ) */

/*        INTEGER               LCHAR */
/*        PARAMETER           ( LCHAR = 59 ) */

/*        INTEGER               RCHAR */
/*        PARAMETER           ( RCHAR = LCHAR - FCHAR + 5 ) */

/*        LOGICAL               FIRST */
/*        CHARACTER*(3)         MARKS */
/*        INTEGER               NMARKS ( 5     ) */
/*        INTEGER               MRKLEN ( 5     ) */
/*        INTEGER               PNTERS ( RCHAR ) */

/*        INTEGER               ROOM */
/*        PARAMETER           ( ROOM = 50 ) */

/*        INTEGER               BEG    ( ROOM  ) */
/*        INTEGER               END    ( ROOM  ) */
/*        INTEGER               IDENT  ( ROOM  ) */

/*        SAVE                  FIRST */
/*        SAVE                  MARKS */
/*        SAVE                  MRKLEN */
/*        SAVE                  PNTERS */

/*        IF ( FIRST ) THEN */

/*           FIRST    = .FALSE. */

/*           MARKS(1) = ' ' */
/*           MARKS(2) = '---' */
/*           MARKS(3) = ':' */
/*           MARKS(4) = ',' */
/*           MARKS(5) = ';' */

/*           NMARKS   = 5 */

/*           CALL SCANPR ( NMARKS, MARKS, MRKLEN, PNTERS ) */

/*        END IF */

/*     Notice that the call to SCANPR is nested inside an */
/*     IF ( FIRST ) THEN ... END IF block. In this and many applications */
/*     the marks that will be used in the scan are fixed. Since the */
/*     marks are not changing, you need to process MARKS and set up */
/*     the auxiliary arrays MRKLEN and PNTERS only once (assuming that */
/*     you SAVE the appropriate variables as has been done above). */
/*     In this way if the code is executed many times, there is only */
/*     a small overhead required for preparing the data so that it */
/*     can be used efficiently in scanning. */

/*     To identify the substrings that represent words we scan the */
/*     string using the prepared MARKS, MRKLEN and PNTERS. */

/*        CALL SCAN ( STRING, MARKS,  MRKLEN, PNTERS, ROOM, */
/*       .            START,  NTOKNS, IDENT,  BEG,    END   ) */

/*     To isolate only the words of the string, we examine the */
/*     array IDENT and keep only those Begin and Ends for which */
/*     the corresponding identity is non-positive. */

/*        KEPT = 0 */

/*        DO I = 1, NTOKNS */

/*           IF ( IDENT(I) .LE. 0 ) THEN */

/*              KEPT      = KEPT + 1 */
/*              BEG(KEPT) = BEG(I) */
/*              END(KEPT) = END(I) */

/*           END IF */

/*        END DO */


/*     Example 2. */
/*     ---------- */

/*     To parse an algebraic expression such as */

/*        ( X + Y ) * ( 2*Z + SIN(W) ) ** 2 */

/*     You would select '**', '*', '+', '-', '(', ')' and ' ' */
/*     to be the markers. Note that all of these begin with one */
/*     of the characters in the string ' !"#$%&''()*+,-./' */
/*     so that we can declare PNTERS to have length 20. */

/*     Prepare the MARKS, MRKLEN, and PNTERS. */

/*        LOGICAL               FIRST */
/*        CHARACTER*(4)         MARKS */
/*        INTEGER               NMARKS ( 8  ) */
/*        INTEGER               MRKLEN ( 8  ) */
/*        INTEGER               PNTERS ( 20 ) */

/*        SAVE                  FIRST */
/*        SAVE                  MARKS */
/*        SAVE                  MRKLEN */
/*        SAVE                  PNTERS */

/*        IF ( FIRST ) THEN */

/*           MARKS(1) = '(' */
/*           MARKS(2) = ')' */
/*           MARKS(3) = '+' */
/*           MARKS(4) = '-' */
/*           MARKS(5) = '*' */
/*           MARKS(6) = '/' */
/*           MARKS(7) = '**' */
/*           MARKS(8) = ' ' */

/*           NMARKS   = 8 */

/*           CALL SCANPR ( NMARKS, MARKS, MRKLEN, PNTERS ) */

/*           Locate the blank character in MARKS once it has */
/*           been prepared. */

/*           BLANK = BSRCHC ( ' ', NMARKS, MARKS ) */

/*        END IF */


/*     Once all of the initializations are out of the way, */
/*     we can scan an input string. */

/*        CALL SCAN ( STRING, MARKS,  MRKLEN, PNTERS, ROOM, */
/*       .            START,  NTOKNS, IDENT,  BEG,    END   ) */


/*     Next eliminate any white space that was returned in the */
/*     list of tokens. */

/*     KEPT = 0 */

/*     DO I = 1, NTOKNS */

/*        IF ( IDENT(I) .NE. BLANK ) THEN */
/*           KEPT        = KEPT + 1 */
/*           BEG  (KEPT) = BEG   (I) */
/*           END  (KEPT) = END   (I) */
/*           IDENT(KEPT) = IDENT (I) */
/*        END IF */

/*     END DO */

/*     Now all of the substrings remaining point to grouping symbols, */
/*     operators, functions, or variables. Given that the individual */
/*     "words" of the expression are now in hand, the meaning of the */
/*     expression is much easier to determine. */

/*     The rest of the routine is left as a non-trivial exercise */
/*     for the reader. */

/* $ Restrictions */

/*     1)  The array of MARKS, MRKLEN, and PNTERS must be properly */
/*         formatted prior to calling SCAN. This is accomplished by */
/*         calling SCANPR. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 26-OCT-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 1.0.0, 26-JUL-1996 (WLT) */

/* -& */
/* $ Index_Entries */

/*     Scan a string for recognized and unrecognized tokens */
/*     Parse a string */

/* -& */

/*     SPICELIB functions */


/*     Local variables */

    /* Parameter adjustments */
    if (ident) {
	}
    if (beg) {
	}
    if (end) {
	}

    /* Function Body */
    switch(n__) {
	case 1: goto L_scanpr;
	case 2: goto L_scan;
	}

    if (! return_()) {
	chkin_("SCANIT", (ftnlen)6);
	setmsg_("Your program has referenced the umbrella subroutine SCANIT."
		"  This may indicate a programming error.", (ftnlen)99);
	sigerr_("SPICE(BOGUSENTRY)", (ftnlen)17);
	chkout_("SCANIT", (ftnlen)6);
    }
    return 0;
/* $Procedure SCANPR ( Scanning preparation ) */

L_scanpr:
/* $ Abstract */

/*     Prepare recognized markers and auxiliary arrays for the */
/*     routine SCAN. */

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

/*     PARSING */
/*     UTILITY */

/* $ Declarations */

/*     INTEGER               NMARKS */
/*     CHARACTER*(*)         MARKS   ( * ) */
/*     INTEGER               MRKLEN  ( * ) */
/*     INTEGER               PNTERS  ( * ) */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     NMARKS    I-O  Number of recognizable substrings. */
/*     MARKS     I-O  Recognizable substrings. */
/*     MRKLEN     O   auxiliary array describing MARKS. */
/*     PNTERS     O   auxiliary array describing MARKS. */

/* $ Detailed_Input */

/*     NMARKS   is the number of recognized marks that will be */
/*              recognized substrings of STRING. */

/*     MARKS    is an array of marks that will be recognized */
/*              by the scanning routine. Leading and trailing */
/*              blanks are not significant. (Except for the */
/*              blank character ' ', itself.  After all, some */
/*              part of it must be significant.)  Case of the */
/*              entries in MARKS is significant. The MARKS */
/*              'XX' and 'xx' are regarded as different MARKS. */

/* $ Detailed_Output */

/*     NMARKS   is the number of marks in the array MARKS after it */
/*              has been prepared for SCAN. */

/*     MARKS    is an array of recognizable substrings. */
/*              It has been prepared for use by SCAN */
/*              so as to be compatible with the other arrays. */
/*              It will be sorted in ascending order, left */
/*              justified and contain no duplicate entries. */

/*     MRKLEN   is an auxiliary array populated by SCANPR */
/*              for use by SCAN that describes MARKS. */

/*     PNTERS   is an auxiliary array populated by SCANPR for */
/*              use by SCAN. It should be declared in the */
/*              calling program as */

/*                 INTEGER   PNTERS ( RCHARS ) */

/*              RCHARS is given by the expression */

/*                MAX - MIN + 5 */

/*              where */

/*              MAX is the maximum value of ICHAR(MARKS(I)(1:1)) */
/*                  over the range I = 1, NMARKS */

/*              MIN is the minimum value of ICHAR(MARKS(I)(1:1)) */
/*                  over the range I = 1, NMARKS */

/*              Here are some typical values that may help you avoid */
/*              going through the computations above. (This assumes */
/*              that ICHAR returns the ASCII code for a character.) */

/*              Scanning Situation           RCHAR */
/*              ------------------          ------------------- */
/*              If NMARKS = 1 */
/*              or all MARKS                   5 */
/*              begin with the same */
/*              character. */

/*              All MARKS begin with */
/*              one of the characters          20 */
/*              in the string */
/*              ' !"#$%&''()*+,-./' */

/*              All MARKS begin with */
/*              one of the characters          11 */
/*              in the string */
/*              ':;<=>?@' */

/*              All MARKS begin with */
/*              one of the characters          37 */
/*              in the string */
/*              ' !"#$%&''()*+,-./:;<=>?@' */

/*              All MARKS begin with */
/*              an upper case English letter   30 */

/*              All MARKS begin with a */
/*              decimal digit                  14 */

/*              All Marks begin with a */
/*              lower case English letter      30 */

/*              All Marks begin with */
/*              a digit or upper case          47 */
/*              character. */

/*              All Marks begin with a */
/*              printing character or          100 */
/*              a blank. */

/*              Anything might be a mark       132 */

/*              Finally, so you won't have to look it up elsewhere */
/*              here are the ASCII codes for the printing */
/*              characters and blanks. */

/*              (Common Punctuations) Character     ASCII Code */
/*                                    -----------   ---------- */
/*                                    ' ' (space)     32 */
/*                                    '!'             33 */
/*                                    '"'             34 */
/*                                    '#'             35 */
/*                                    '$'             36 */
/*                                    '%'             37 */
/*                                    '&'             38 */
/*                                    ''''            39 */
/*                                    '('             40 */
/*                                    ')'             41 */
/*                                    '*'             42 */
/*                                    '+'             43 */
/*                                    ','             44 */
/*                                    '-'             45 */
/*                                    '.'             46 */
/*                                    '/'             47 */


/*              (Decimal Digits)      Character     ASCII Code */
/*                                    -----------   ---------- */
/*                                    '0'             48 */
/*                                    '1'             49 */
/*                                    '2'             50 */
/*                                    '3'             51 */
/*                                    '4'             52 */
/*                                    '5'             53 */
/*                                    '6'             54 */
/*                                    '7'             55 */
/*                                    '8'             56 */
/*                                    '9'             57 */

/*              (More punctuation)    Character     ASCII Code */
/*                                    -----------   ---------- */
/*                                    ':'             58 */
/*                                    ';'             59 */
/*                                    '<'             60 */
/*                                    '='             61 */
/*                                    '>'             62 */
/*                                    '?'             63 */
/*                                    '@'             64 */

/*              (Uppercase characters)  Character     ASCII Code */
/*                                    -----------   ---------- */
/*                                    'A'             65 */
/*                                    'B'             66 */
/*                                    'C'             67 */
/*                                    'D'             68 */
/*                                    'E'             69 */
/*                                    'F'             70 */
/*                                    'G'             71 */
/*                                    'H'             72 */
/*                                    'I'             73 */
/*                                    'J'             74 */
/*                                    'K'             75 */
/*                                    'L'             76 */
/*                                    'M'             77 */
/*                                    'N'             78 */
/*                                    'O'             79 */
/*                                    'P'             80 */
/*                                    'Q'             81 */
/*                                    'R'             82 */
/*                                    'S'             83 */
/*                                    'T'             84 */
/*                                    'U'             85 */
/*                                    'V'             86 */
/*                                    'W'             87 */
/*                                    'X'             88 */
/*                                    'Y'             89 */
/*                                    'Z'             90 */

/*              (More punctuation)    Character     ASCII Code */
/*                                    -----------   ---------- */
/*                                    '['             91 */
/*                                    '\'             92 */
/*                                    ']'             93 */
/*                                    '^'             94 */
/*                                    '_'             95 */
/*                                    '`'             96 */

/*              (Lowercase characters)  Character     ASCII Code */
/*                                    -----------   ---------- */
/*                                    'a'             97 */
/*                                    'b'             98 */
/*                                    'c'             99 */
/*                                    'd'            100 */
/*                                    'e'            101 */
/*                                    'f'            102 */
/*                                    'g'            103 */
/*                                    'h'            104 */
/*                                    'i'            105 */
/*                                    'j'            106 */
/*                                    'k'            107 */
/*                                    'l'            108 */
/*                                    'm'            109 */
/*                                    'n'            110 */
/*                                    'o'            111 */
/*                                    'p'            112 */
/*                                    'q'            113 */
/*                                    'r'            114 */
/*                                    's'            115 */
/*                                    't'            116 */
/*                                    'u'            117 */
/*                                    'v'            118 */
/*                                    'w'            119 */
/*                                    'x'            120 */
/*                                    'y'            121 */
/*                                    'z'            122 */

/*              (More punctuation)      Character     ASCII Code */
/*                                    -----------   ---------- */
/*                                    '{'            123 */
/*                                    '|'            124 */
/*                                    '}'            125 */
/*                                    '~'            126 */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/*     1)  A space is regarded as a special mark. If MARKS(I) = ' ', */
/*         then MARKS(I) will match any consecutive sequence of blanks. */

/*     2)  If NMARKS is less than or equal to zero, SCAN will always */
/*         find a single token, namely the entire string to be scanned. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine prepares the arrays MARKS, MRKLEN and PNTERS */
/*     so that they are suitable for input to the routine SCAN. */

/*     It is expected that users will need to scan many strings */
/*     and that from the programming point of view it is */
/*     easiest to simply supply a list of MARKS to a "formatting" */
/*     routine such as this so that the strings can then */
/*     be efficiently scanned by the routine SCAN. This formatting */
/*     is the function of this routine. */

/* $ Examples */

/*     Suppose you need to identify all of the words within a string */
/*     and wish to ignore punctuation marks such as ' ', ',', ':', ';' */
/*     '---'.  Then the first step is to load the array of marks as */
/*     shown here: */

/*        The minimum ASCII code for the first character of a marker is */
/*        32 (for ' '). */

/*        INTEGER               FCHAR */
/*        PARAMETER           ( FCHAR = 32 ) */

/*        The maximum ASCII code for the first character of a marker is */
/*        59 (for ';'). */

/*        INTEGER               LCHAR */
/*        PARAMETER           ( LCHAR = 59 ) */


/*        The proper size to declare PNTERS is given by the parameter */
/*        RCHAR defined in terms of LCHAR and FCHAR. */

/*        INTEGER               RCHAR */
/*        PARAMETER           ( RCHAR = LCHAR - FCHAR + 5 ) */

/*        LOGICAL               FIRST */
/*        CHARACTER*(4)         MARKS */
/*        INTEGER               NMARKS ( 5     ) */
/*        INTEGER               MRKLEN ( 5     ) */
/*        INTEGER               PNTERS ( RCHAR ) */

/*        SAVE                  FIRST */
/*        SAVE                  MARKS */
/*        SAVE                  MRKLEN */
/*        SAVE                  PNTERS */

/*        IF ( FIRST ) THEN */

/*           FIRST    = .FALSE. */

/*           MARKS(1) = ' ' */
/*           MARKS(2) = '---' */
/*           MARKS(3) = ':' */
/*           MARKS(4) = ',' */
/*           MARKS(5) = ';' */

/*           NMARKS   = 5 */

/*           CALL SCANPR ( NMARKS, MARKS, MRKLEN, PNTERS ) */

/*        END IF */

/*     Notice that the call to SCANPR is nested inside an */
/*     IF ( FIRST ) THEN ... END IF block. In this and many applications */
/*     the marks that will used in the scan are fixed. Since the marks */
/*     are not changing, you need to process MARKS and set up */
/*     the auxiliary arrays MRKLEN and PNTERS only once (assuming that */
/*     you SAVE the appropriate variables as has been done above). */
/*     In this way if the code is executed many times, there is only */
/*     a small overhead required for preparing the data so that it */
/*     can be used efficiently in scanning. */

/* $ Restrictions */

/*     1)  MRKLEN and PNTERS must be declared to be at least as large */
/*         as indicated above. If not, this routine will write */
/*         past the ends of these arrays. Much unpleasantness may */
/*         ensue in the attempt to debug such problems. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 26-OCT-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 1.0.0, 26-JUL-1996 (WLT) */

/* -& */
/* $ Index_Entries */

/*     Prepare for scanning strings */
/*     Prepare for parsing strings */

/* -& */

/*     We handle the case where NMARKS is non-positive separately. */

    if (*nmarks <= 0) {
	pnters[0] = 0;
	pnters[1] = 0;
	pnters[2] = 0;
	pnters[3] = 0;
	pnters[4] = 0;
	return 0;
    }

/*     First left justify MARKS and remove duplicates. */

    i__1 = *nmarks;
    for (i__ = 1; i__ <= i__1; ++i__) {
	ljust_(marks + (i__ - 1) * marks_len, marks + (i__ - 1) * marks_len, 
		marks_len, marks_len);
    }
    n = *nmarks;

/*     Sort and remove duplicates from the array MARKS. */

    rmdupc_(&n, marks, marks_len);

/*     All of the MARKS have the same declared length. */
/*     However, since all of your marks may not have */
/*     the same intended length (for example '*' and */
/*     '**') it is desirable to be able to specify */
/*     how much of MARKS(I) should actually be used */
/*     when examining STRING for a substring match. */
/*     This is done with the array MRKLEN. */
/*     MARKS(I)(1:MRKLEN(I)) will be used when */
/*     scanning STRING. */

/*     Here is the expected structure of PNTERS. */

/*             PNTERS(1) = MIN ( ICHAR(MARKS(I)(1:1)  ), I=1,NMARKS ) */
/*             PNTERS(2) = MAX ( ICHAR(MARKS(I)(1:1)  ), I=1,NMARKS ) */

/*     For ease of further discussion let */
/*     MYCHAR(I) represent the characters from PNTERS(1) */
/*     to PNTERS(2), and assume that legitimate values of */
/*     I are from 1 to M. */

/*             PNTERS(3)   = 0 */
/*             PNTERS(4)   = index of the last entry of MARKS */
/*                           that begins with the character */
/*                           MYCHAR(1). */

/*             PNTERS(5)   = index of the last entry of MARKS */
/*                           that begins with the character */
/*                           MYCHAR(2), if there is no such element */
/*                           of MARKS let PNTERS(5) = PNTERS(4) */
/*                . */
/*                . */
/*                . */

/*             PNTERS(3+K) = index of the last entry of MARKS */
/*                           that begins with the character */
/*                           MYCHAR(K), if there is no such element */
/*                           of MARKS, let PNTERS(3+K) = */
/*                           PNTERS(3+K-1) */
/*                . */
/*                . */
/*                . */

/*             PNTERS(3+M) = index of the last entry of MARKS */
/*                           that begins with the character */
/*                           MYCHAR(M). */

/*             PNTERS(4+M) = PNTERS(3+M) */



/*     Next determine the minimum and maximum ASCII values */
/*     of the first characters of the MARKS. */

    fchar = *(unsigned char *)&marks[0];
    lchar = *(unsigned char *)&marks[(n - 1) * marks_len];
    pnters[0] = fchar;
    pnters[1] = lchar;

/*     For the purposes of getting started, we will say the last */
/*     character that started a MARK was one before FCHAR.  We */
/*     will record the end of its block in slot 3 of PNTERS. */

    last1 = fchar - 1;
    slot = 3;
    i__1 = n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	mrklen[i__ - 1] = rtrim_(marks + (i__ - 1) * marks_len, marks_len);
	this1 = *(unsigned char *)&marks[(i__ - 1) * marks_len];
	if (this1 != last1) {

/*           We need to record the address of the end of the last */
/*           block of MARKS that began with the same character. */
/*           This is of course one before the current value of I. */

/*           While we are at it, we might as well determine how */
/*           many possible first letters were "jumped" over in */
/*           going from the last first character to the current */
/*           first character. */

	    eblock = i__ - 1;
	    jump = this1 - last1;

/*           The end of the block for all of the MARKS having */
/*           first character between the last one and this one */
/*           is the same. */

	    i__2 = slot + jump - 1;
	    for (j = slot; j <= i__2; ++j) {
		pnters[j - 1] = eblock;
	    }
	    slot += jump;
	    last1 = this1;
	}
    }
    pnters[slot - 1] = n;
    pnters[slot] = n;
    *nmarks = n;
    return 0;
/* $Procedure SCAN ( Scan a string for tokens ) */

L_scan:
/* $ Abstract */

/*     Scan a string and return the beginnings and ends of recognized */
/*     and unrecognized substrings. The full collection of these */
/*     substrings partitions the string. */

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

/*     PARSING */

/* $ Declarations */

/*     CHARACTER*(*)         STRING */
/*     CHARACTER*(*)         MARKS   ( * ) */
/*     INTEGER               MRKLEN  ( * ) */
/*     INTEGER               PNTERS  ( * ) */
/*     INTEGER               ROOM */
/*     INTEGER               START */
/*     INTEGER               NTOKNS */
/*     INTEGER               BEG     ( * ) */
/*     INTEGER               END     ( * ) */
/*     INTEGER               IDENT   ( * ) */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     STRING     I   string to be scanned. */
/*     MARKS      I   recognizable substrings. */
/*     MRKLEN     I   an auxiliary array describing MARKS. */
/*     PNTERS     I   an auxiliary array describing MARKS. */
/*     ROOM       I   space available for storing substring descriptions. */
/*     START     I-O  position from which to begin/resume scanning. */
/*     NTOKNS     O   number of scanned substrings. */
/*     BEG        O   beginnings of scanned substrings. */
/*     END        O   endings of scanned substrings. */
/*     IDENT      O   position of scanned substring within array MARKS. */

/* $ Detailed_Input */

/*     STRING   is any character string that is to be scanned */
/*              to locate recognized and unrecognized substrings. */

/*     MARKS    is an array of marks that will be recognized */
/*              by the scanning routine. This array must be prepared */
/*              by calling the routine SCANPR. */

/*              Note that the blank string is interpreted */
/*              in a special way by SCAN. If the blank character, */
/*              ' ', is one of the MARKS, it will match any unbroken */
/*              sequence of blanks in string.  Thus if ' ' is the only */
/*              marks supplied and STRING is */

/*                 'A   lot of      space ' */
/*                  ...................... */

/*              Then scan will locate the following substrings */

/*              'A'          STRING(1:1)    (unrecognized) */
/*              '   '        STRING(2:4)    (recognized --- all blanks) */
/*              'lot'        STRING(5:7)    (unrecognized) */
/*              ' '          STRING(8:8)    (recognized --- a blank) */
/*              'of'         STRING(9:10)   (unrecognized) */
/*              '      '     STRING(11:16)  (recognized --- all blanks) */
/*              'space'      STRING(17:21)  (unrecognized) */
/*              ' '          STRING(22:22)  (recognized --- a blank) */

/*     MRKLEN   is an auxiliary array populated by SCANPR */
/*              for use by SCAN. It should be declared with */
/*              length equal to the length of MARKS. It must */
/*              be prepared for use by the routine SCANPR. */

/*     PNTERS   is a specially structured array of integers that */
/*              describes the array MARKS. It is must be filled */
/*              in by the routine SCANPR. It should be declared */
/*              by the calling program as shown here: */

/*                 INTEGER  PNTERS ( RCHARS ) */

/*              RCHARS is given by the expression */

/*                MAX - MIN + 5 */

/*              where */

/*              MAX is the maximum value of ICHAR(MARKS(I)(1:1)) */
/*                  over the range I = 1, NMARKS */

/*              MIN is the minimum value of ICHAR(MARKS(I)(1:1)) */
/*                  over the range I = 1, NMARKS */

/*              See SCANPR for a more detailed description of the */
/*              declaration of PNTERS. */

/*     ROOM     is the amount of space available for storing the */
/*              results of scanning the string. */

/*     START    is the position from which scanning should commence. */
/*              Values of START less than 1 are treated as 1. */

/* $ Detailed_Output */

/*     START    is the position from which scanning should continue */
/*              in order to fully scan STRING (if sufficient memory was */
/*              not provided in BEG, END, and IDENT on the current */
/*              call to SCAN). */

/*     NTOKNS   is the number of substrings identified in the current */
/*              scan of STRING. */

/*     BEG      beginnings of scanned substrings. This should be */
/*              declared so that it is at least as large as ROOM. */

/*     END      endings of scanned substrings. This should be declared */
/*              so that it is at least as large as ROOM. */

/*     IDENT    positions of scanned substring within array MARKS. */
/*              If the substring STRING(BEG(I):END(I)) is in the array */
/*              MARKS, then MARKS(IDENT(I)) will equal */
/*              STRING(BEG(I):END(I)). */

/*              If the substring STRING(BEG(I):END(I)) is not in the */
/*              list of MARKS then IDENT(I) will have the value 0. */

/*              IDENT should be declared so that it can contain at least */
/*              ROOM integers. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/*     1)  A space is regarded as a special mark. If MARKS(I) = ' ', */
/*         then MARKS(I) will match any consecutive sequence of blanks. */

/*     2)  If START is less than 1 on input, it will be treated as */
/*         if it were 1. */

/*     3)  If START is greater than the length of the string, no */
/*         tokens will be found and the value of START will return */
/*         unchanged. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine allows you to scan a string and partition it into */
/*     recognized and unrecognized substrings. */

/*     For some applications the recognized substrings serve only as */
/*     delimiters between the portions of the string */
/*     that are of interest to your application. For other */
/*     applications the recognized substrings are equally important as */
/*     they may indicate operations that are to be performed on the */
/*     unrecognized portions of the string. However, the techniques */
/*     required to scan the string are the same in both instances. The */
/*     examples below illustrate some common situations. */

/* $ Examples */

/*     Example 1. */
/*     ---------- */

/*     Suppose you wished to write a routine that would return the words */
/*     of a string. The following routine shows how SCANPR and SCAN can */
/*     be used to accomplish this task. */

/*        SUBROUTINE GETWDS ( STRING, WDROOM, NWORDS, WORDS ) */

/*        CHARACTER*(*)      STRING */
/*        INTEGER            WDROOM */
/*        INTEGER            NWORDS */
/*        CHARACTER*(*)      WORDS  ( * ) */


/*        CHARACTER*(1)      MARKS  ( 1 ) */
/*        INTEGER            MRKLEN ( 1 ) */
/*        INTEGER            PNTERS ( 5 ) */

/*        INTEGER            ROOM */
/*        PARAMETER        ( ROOM = 50 ) */

/*        INTEGER            BEG   ( ROOM ) */
/*        INTEGER            END   ( ROOM ) */
/*        INTEGER            I */
/*        INTEGER            IDENT ( ROOM ) */
/*        INTEGER            NMARKS */
/*        INTEGER            NTOKNS */
/*        INTEGER            START */

/*        LOGICAL            FIRST */
/*        SAVE               FIRST */
/*        DATA               FIRST  / .TRUE. / */


/*        On the first time through the routine, set up the MARKS */
/*        MRKLEN, and PNTERS arrays. */

/*        IF( FIRST ) THEN */

/*           FIRST    = .FALSE. */
/*           MARKS(1) = ' ' */
/*           NMARKS   = 1 */

/*           CALL SCANPR ( NMARKS, MARKS, MRKLEN, PNTERS ) */

/*        END IF */

/*        Now simply scan the input string for words until we have */
/*        them all or until we run out of room. */

/*        START  = 1 */
/*        NWORDS = 0 */

/*        CALL SCAN ( STRING, */
/*                    MARKS,  MRKLEN, PNTERS, ROOM, START, */
/*                    NTOKNS, IDENT,  BEG,    END          ) */

/*        If we found something in our scan, copy the substrings into the */
/*        words array. */

/*        DO WHILE (       ( NWORDS .LT. WDROOM ) */
/*       .           .AND. ( NTOKNS .GT. 0      ) ) */


/*           Step through the scanned substrings, looking for those */
/*           that are not blank ... */

/*           I = 1 */

/*           DO WHILE (       ( NWORDS .LT. WDROOM ) */
/*          .           .AND. ( I      .LE. NTOKNS ) ) */

/*              Copy the non-blank substrings (those unidentified by */
/*              SCAN) into WORDS. */

/*              IF ( IDENT(I) .EQ. 0 ) THEN */
/*                 NWORDS        = NWORDS + 1 */
/*                 WORDS(NWORDS) = STRING(BEG(I):END(I)) */
/*              END IF */

/*              I      = I      + 1 */

/*           END DO */


/*           Scan the STRING again for any substrings that might */
/*           remain. Note that START is already pointing at the */
/*           point in the string from which to resume scanning. */

/*           CALL SCAN ( STRING, */
/*                       MARKS,  MRKLEN, PNTERS, ROOM, START, */
/*                       NTOKNS, IDENT,  BEG,    END          ) */
/*        END DO */

/*        That's all, we've got all the substrings there were (or */
/*        that we had room for). */

/*        RETURN */


/*     Example 2. */
/*     ---------- */

/*     To parse an algebraic expression such as */

/*        ( X + Y ) * ( 2*Z + SIN(W) ) ** 2 */

/*     You would select '**', '*', '+', '-', '(', ')' and ' ' */
/*     to be the markers. Note that all of these begin with one */
/*     of the characters in the string ' !"#$%&''()*+,-./' */
/*     so that we can declare PNTERS to have length 20. */

/*     Prepare the MARKS, MRKLEN, and PNTERS. */

/*        CHARACTER*(4)         MARKS */
/*        INTEGER               NMARKS ( 8  ) */
/*        INTEGER               MRKLEN ( 8  ) */
/*        INTEGER               PNTERS ( 20 ) */

/*        INTEGER               ROOM */
/*        PARAMETER           ( ROOM = 20 ) */

/*        INTEGER               NTOKNS */
/*        INTEGER               BEG    ( ROOM ) */
/*        INTEGER               END    ( ROOM ) */
/*        INTEGER               IDENT  ( ROOM ) */

/*        LOGICAL               FIRST */
/*        SAVE                  FIRST */
/*        SAVE                  MARKS */
/*        SAVE                  MRKLEN */
/*        SAVE                  PNTERS */

/*        DATA                  FIRST  / .TRUE. / */

/*        IF ( FIRST ) THEN */

/*           MARKS(1) = '(' */
/*           MARKS(2) = ')' */
/*           MARKS(3) = '+' */
/*           MARKS(4) = '-' */
/*           MARKS(5) = '*' */
/*           MARKS(6) = '/' */
/*           MARKS(7) = '**' */
/*           MARKS(8) = ' ' */

/*           NMARKS   = 8 */

/*           CALL SCANPR ( NMARKS, MARKS, MRKLEN, PNTERS ) */

/*           BLANK = BSRCHC ( ' ', NMARKS, MARKS ) */

/*        END IF */


/*        Once all of the initializations are out of the way, */
/*        we can scan an input string. */

/*        CALL SCAN ( STRING, MARKS,  MRKLEN, PNTERS, ROOM, */
/*       .            START,  NTOKNS, IDENT,  BEG,    END  ) */


/*        Next eliminate any white space that was returned in the */
/*        list of tokens. */

/*        KEPT = 0 */

/*        DO I = 1, NTOKNS */

/*           IF ( IDENT(I) .NE. BLANK ) THEN */

/*              KEPT        = KEPT + 1 */
/*              BEG  (KEPT) = BEG(I) */
/*              END  (KEPT) = END(I) */
/*              IDENT(KEPT) = IDENT(I) */

/*           END IF */

/*        END DO */

/*        Now all of the substrings remaining point to grouping symbols, */
/*        operators, functions, or variables. Given that the individual */
/*        "words" of the expression are now in hand, the meaning of the */
/*        expression is much easier to determine. */

/*        The rest of the routine is left as a non-trivial exercise */
/*        for the reader. */

/* $ Restrictions */

/*     1)  The arrays MARKS, MRKLEN, and PNTERS must be prepared by the */
/*         routine SCANPR prior to supplying them for use by SCAN. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 26-OCT-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 1.0.0, 26-JUL-1996 (WLT) */

/* -& */
/* $ Index_Entries */

/*     Scan a string for recognized and unrecognized tokens */
/*     Parse a string */

/* -& */

/*     All of the MARKS have the same declared length. */
/*     However, since all of your marks may not have */
/*     the same intended length (for example '*' and */
/*     '**') it is desirable to be able to specify */
/*     how much of MARKS(I) should actually be used */
/*     when examining STRING for a substring match. */
/*     This is done with the array MRKLEN. */
/*     MARKS(I)(1:MRKLEN(I)) will be used when */
/*     scanning STRING. */

/*     Here is the expected structure of PNTERS. */

/*             PNTERS(1) = MIN ( ICHAR(MARKS(I)(1:1)  ) */
/*             PNTERS(2) = MAX ( ICHAR(MARKS(I)(1:1)  ) */

/*     where I ranges from 1 to the number of MARKS stored */
/*     in MARKS.  For ease of further discussion let */
/*     MYCHAR(I) represent the characters from PNTERS(1) */
/*     to PNTERS(2), and assume that legitimate values of */
/*     I are from 1 to N. */

/*             PNTERS(3)   = 0 */
/*             PNTERS(4)   = index of the last entry of MARKS */
/*                           that begins with the character */
/*                           MYCHAR(1). */

/*             PNTERS(5)   = index of the last entry of MARKS */
/*                           that begins with the character */
/*                           MYCHAR(2), if there is no such element */
/*                           of MARKS let PNTERS(5) = PNTERS(4) */
/*                . */
/*                . */
/*                . */

/*             PNTERS(3+K) = index of the last entry of MARKS */
/*                           that begins with the character */
/*                           MYCHAR(K), if there is no such element */
/*                           of MARKS, let PNTERS(3+K) = */
/*                           PNTERS(3+K-1) */
/*                . */
/*                . */
/*                . */

/*             PNTERS(3+N) = index of the last entry of MARKS */
/*                           that begins with the character */
/*                           MYCHAR(N). */

/*             PNTERS(4+N) = PNTERS(3+N) */


/*     Get the information concerning the range of the */
/*     marks from the PNTERS array. */

    offset = pnters[0] - 4;
    lbound = pnters[0] - 1;
    ubound = pnters[1] + 1;
    last = i_len(string, string_len);
    *ntokns = 0;
    backup = *start - 1;
    known = TRUE_;
    *start = max(1,*start);
    while(*start <= last) {

/*        Get the numeric code for this letter, and look up */
/*        the range of markers that begin with this letter. */

	*(unsigned char *)letter = *(unsigned char *)&string[*start - 1];
/* Computing MAX */
/* Computing MIN */
	i__3 = *(unsigned char *)letter;
	i__1 = lbound, i__2 = min(i__3,ubound);
	intval = max(i__1,i__2);
	test = pnters[intval - offset - 1];
	finish = pnters[intval - offset - 2];
	equal = FALSE_;

/*        If TEST is greater than FINISH, then there is a range of */
/*        markers that start with this letter. */

	while(test > finish) {

/*           Look up the length of the next marker to test for */
/*           and compute where it would end in STRING if there */
/*           is a match. */

	    l = mrklen[test - 1];
	    stop = backup + l;

/*           Make sure that we are not going to violate any substring */
/*           references when we compare the current candidate mark with */
/*           the substring having the same length and starting at START. */

	    if (stop > last) {
		--test;
	    } else {

/*              OK. The substring reference STRING(START:STOP) is */
/*              legal.  See if it is equal to the current test mark. */

		equal = s_cmp(marks + (test - 1) * marks_len, string + (*
			start - 1), l, stop - (*start - 1)) == 0;

/*              If it isn't equal, just set up to test the next mark. */

		if (! equal) {
		    --test;
		} else {

/*                 If we were in the middle of an unrecognized string */
/*                 then, we need to check whether or not we have room */
/*                 to identify another token. If we don't we must return */
/*                 now. */

		    if (! known && *ntokns == *room) {
			return 0;
		    }

/*                 A space is a special kind of mark.  All white space */
/*                 is regarded as being the same.  If the current mark */
/*                 is a space, we need to collect all of the consecutive */
/*                 blanks beginning with the one at the START position. */

		    if (s_cmp(marks + (test - 1) * marks_len, " ", marks_len, 
			    (ftnlen)1) == 0) {
			stop = ncpos_(string, " ", start, string_len, (ftnlen)
				1) - 1;
			if (stop < 0) {
			    stop = last;
			}
		    }

/*                 Ok. We have a new known token. */

/*                 1)  Record its begin, end, and identity. */

/*                 2)  Set TEST to FINISH so that the loop will end. */

/*                 3)  Set START to the current STOP so that later when */
/*                     we add 1, START will point to the beginning */
/*                     of the remainder of the string that needs to be */
/*                     scanned. */

		    known = TRUE_;
		    ++(*ntokns);
		    beg[*ntokns - 1] = *start;
		    end[*ntokns - 1] = stop;
		    ident[*ntokns - 1] = test;
		    test = finish;
		    *start = stop;

/*                 If we have just used up all available room, */
/*                 position START so that we will be ready */
/*                 to continue scanning on a subsequent call */
/*                 and return. */

		    if (*ntokns == *room) {
			++(*start);
			return 0;
		    }
		}
	    }
	}

/*        If none of the markers matched a substring starting at */
/*        the current position, we are beginning or continuing */
/*        an unrecognized substring. */

	if (! equal) {

/*           If we are already in the middle of an unrecognized */
/*           substring, just extend our current unrecognized string. */

	    if (! known) {
		end[*ntokns - 1] = *start;

/*           Otherwise, start up a new unrecognized substring. */

	    } else {
		++(*ntokns);
		beg[*ntokns - 1] = *start;
		end[*ntokns - 1] = *start;
		ident[*ntokns - 1] = 0;
		known = FALSE_;
	    }
	}
	backup = *start;
	++(*start);
    }
    return 0;
} /* scanit_ */

/* Subroutine */ int scanit_(char *string, integer *start, integer *room, 
	integer *nmarks, char *marks, integer *mrklen, integer *pnters, 
	integer *ntokns, integer *ident, integer *beg, integer *end, ftnlen 
	string_len, ftnlen marks_len)
{
    return scanit_0_(0, string, start, room, nmarks, marks, mrklen, pnters, 
	    ntokns, ident, beg, end, string_len, marks_len);
    }

/* Subroutine */ int scanpr_(integer *nmarks, char *marks, integer *mrklen, 
	integer *pnters, ftnlen marks_len)
{
    return scanit_0_(1, (char *)0, (integer *)0, (integer *)0, nmarks, marks, 
	    mrklen, pnters, (integer *)0, (integer *)0, (integer *)0, (
	    integer *)0, (ftnint)0, marks_len);
    }

/* Subroutine */ int scan_(char *string, char *marks, integer *mrklen, 
	integer *pnters, integer *room, integer *start, integer *ntokns, 
	integer *ident, integer *beg, integer *end, ftnlen string_len, ftnlen 
	marks_len)
{
    return scanit_0_(2, string, start, room, (integer *)0, marks, mrklen, 
	    pnters, ntokns, ident, beg, end, string_len, marks_len);
    }

