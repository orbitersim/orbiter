/* zztpats.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure ZZTPATS (Private, Time --- Time Patterns) */
logical zztpats_(integer *room, integer *nknown, char *known, char *meanng, 
	ftnlen known_len, ftnlen meanng_len)
{
    /* Initialized data */

    static logical first = TRUE_;

    /* System generated locals */
    integer i__1, i__2;
    logical ret_val;

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_rnge(char *, integer, char *, integer);

    /* Local variables */
    integer i__;
    extern /* Subroutine */ int orderc_(char *, integer *, integer *, ftnlen),
	     reordc_(integer *, integer *, char *, ftnlen);
    integer ordvec[231];
    static char mymnng[32*231], myknwn[32*231];

/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines.  Users should not call this routine directly due */
/*     to the volatile nature of this routine. */

/*     Initialize the built-in time patterns. */

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

/*      None. */

/* $ Keywords */

/*      PRIVATE */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     ROOM       I   The declared space available for patterns */
/*     KNOWN      O   The patterns that are automatically recognized */
/*     MEANNG     O   The meaning associated with the patterns. */
/*     COUNT      P   The number of patterns built in to this routine. */
/*     The function returns .TRUE. if the initialization is successful. */

/* $ Detailed_Input */

/*     ROOM       an integer giving the room available for known patterns */
/*                and their meanings. */

/*                If ROOM does not equal the number of built-in patterns */
/*                the function returns only those patterns that will fit */
/*                and returns the value FALSE. */

/* $ Detailed_Output */

/*     NKNOWN     is the number of patterns/meanings returned in the */
/*                arrays KNOWN and MEANNG */

/*     KNOWN      is the array of automatically recognized calendar */
/*                date patterns.  KNOWN will be sorted according to */
/*                the FORTRAN collating sequence. */

/*     MEANNG     is the array of "meanings" associated with the built-in */
/*                patterns returned in the array KNOWN. MEANNG(I) is */
/*                the "meaning" associated with known pattern KNOWN(I). */

/*     The function returns TRUE if the arrays, KNOWN and MEANNG are */
/*     successfully initialized.  Otherwise it returns FALSE. */

/* $ Parameters */

/*     COUNT      is the number of patterns/meanings that are */
/*                returned by this routine. */

/* $ Exceptions */

/*     Error Free. */

/*     1) If ROOM is less than count, the function returns FALSE. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This is a utility routine that supports the SPICE routine */
/*     TPARTV that parses time strings.  This routine initializes */
/*     the set of built-in time patterns for use by TPARTV */

/*     The lengths of the strings assigned to an arbitrary MYKNWN */
/*     element must equal the string lengths of the corresponding */
/*     element MYMNNG. The relation, one-to-one and onto, */
/*     defining the meaning (or non meaning) and type of each */
/*     symbol in the time string abstract representation. */

/* $ Examples */

/*     See TPARTV */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     W.L. Taber      (JPL) */

/* $ Version */

/* -    SPICELIB Version 3.2.0, 01-OCT-2020 (EDW) (NJB) (BVS) */

/*        Implemented a FIRST block to prevent reinitialization */
/*        of patterns on each entry to routine. */

/*        Added patterns to accommodate ISO formats with 'Z' suffix. */

/* -    SPICELIB Version 3.1.0, 11-DEC-2013 (EDW) */

/*        Corrected typo which showed MEANNG(203) assigned */
/*        the value 'm*D*YH*M' rather than the correct assignment */
/*        to MYMNNG( 203 ). The assignment error often prevented */
/*        the Time subsystem from parsing time strings with */
/*        the format 'i-i-Yi:i'. */

/*        Corrected header section ordering to meet SPICE requirements. */

/* -    SPICELIB Version 3.0.0, 16-AUG-2002 (WLT) */

/*        The interface of the routine was changed from */

/*           ZZTPATS( ROOM, KNOWN, MEANNG ) */

/*        to */

/*           ZZTPATS( ROOM, NKNOWN, KNOWN, MEANNG ) */

/*        and made error free. */

/* -    SPICELIB Version 2.0.0, 16-APR-1997 (WLT) */

/*        The collection of recognized built in patterns was */
/*        increased from 185 to 203 patterns.  The new patterns */
/*        begin at KNOWN(186) below. */

/* -    SPICELIB Version 1.0.0, 02-APR-1996 (WLT) */

/* -& */
    if (first) {
	first = FALSE_;
	s_copy(myknwn, "Y-i-it", (ftnlen)32, (ftnlen)6);
	s_copy(mymnng, "Y*m*D*", (ftnlen)32, (ftnlen)6);
	s_copy(myknwn + 32, "Y-i-iti:i", (ftnlen)32, (ftnlen)9);
	s_copy(mymnng + 32, "Y*m*D*H*M", (ftnlen)32, (ftnlen)9);
	s_copy(myknwn + 64, "Y-i-iti:i:i", (ftnlen)32, (ftnlen)11);
	s_copy(mymnng + 64, "Y*m*D*H*M*S", (ftnlen)32, (ftnlen)11);
	s_copy(myknwn + 96, "Y-i-iti:i:n", (ftnlen)32, (ftnlen)11);
	s_copy(mymnng + 96, "Y*m*D*H*M*S", (ftnlen)32, (ftnlen)11);
	s_copy(myknwn + 128, "Y-i-iti:n", (ftnlen)32, (ftnlen)9);
	s_copy(mymnng + 128, "Y*m*D*H*M", (ftnlen)32, (ftnlen)9);
	s_copy(myknwn + 160, "Y-i/", (ftnlen)32, (ftnlen)4);
	s_copy(mymnng + 160, "Y*y*", (ftnlen)32, (ftnlen)4);
	s_copy(myknwn + 192, "Y-i/i:i", (ftnlen)32, (ftnlen)7);
	s_copy(mymnng + 192, "Y*y*H*M", (ftnlen)32, (ftnlen)7);
	s_copy(myknwn + 224, "Y-i/i:i:i", (ftnlen)32, (ftnlen)9);
	s_copy(mymnng + 224, "Y*y*H*M*S", (ftnlen)32, (ftnlen)9);
	s_copy(myknwn + 256, "Y-i/i:i:n", (ftnlen)32, (ftnlen)9);
	s_copy(mymnng + 256, "Y*y*H*M*S", (ftnlen)32, (ftnlen)9);
	s_copy(myknwn + 288, "Y-i/i:n", (ftnlen)32, (ftnlen)7);
	s_copy(mymnng + 288, "Y*y*H*M", (ftnlen)32, (ftnlen)7);
	s_copy(myknwn + 320, "Y-id", (ftnlen)32, (ftnlen)4);
	s_copy(mymnng + 320, "Y*y*", (ftnlen)32, (ftnlen)4);
	s_copy(myknwn + 352, "Y-idi:i", (ftnlen)32, (ftnlen)7);
	s_copy(mymnng + 352, "Y*y*H*M", (ftnlen)32, (ftnlen)7);
	s_copy(myknwn + 384, "Y-idi:i:i", (ftnlen)32, (ftnlen)9);
	s_copy(mymnng + 384, "Y*y*H*M*S", (ftnlen)32, (ftnlen)9);
	s_copy(myknwn + 416, "Y-idi:i:n", (ftnlen)32, (ftnlen)9);
	s_copy(mymnng + 416, "Y*y*H*M*S", (ftnlen)32, (ftnlen)9);
	s_copy(myknwn + 448, "Y-idi:n", (ftnlen)32, (ftnlen)7);
	s_copy(mymnng + 448, "Y*y*H*M", (ftnlen)32, (ftnlen)7);
	s_copy(myknwn + 480, "Y-it", (ftnlen)32, (ftnlen)4);
	s_copy(mymnng + 480, "Y*y*", (ftnlen)32, (ftnlen)4);
	s_copy(myknwn + 512, "Y-iti:i", (ftnlen)32, (ftnlen)7);
	s_copy(mymnng + 512, "Y*y*H*M", (ftnlen)32, (ftnlen)7);
	s_copy(myknwn + 544, "Y-iti:i:i", (ftnlen)32, (ftnlen)9);
	s_copy(mymnng + 544, "Y*y*H*M*S", (ftnlen)32, (ftnlen)9);
	s_copy(myknwn + 576, "Y-iti:i:n", (ftnlen)32, (ftnlen)9);
	s_copy(mymnng + 576, "Y*y*H*M*S", (ftnlen)32, (ftnlen)9);
	s_copy(myknwn + 608, "Y-iti:n", (ftnlen)32, (ftnlen)7);
	s_copy(mymnng + 608, "Y*y*H*M", (ftnlen)32, (ftnlen)7);
	s_copy(myknwn + 640, "Yid", (ftnlen)32, (ftnlen)3);
	s_copy(mymnng + 640, "Yy*", (ftnlen)32, (ftnlen)3);
	s_copy(myknwn + 672, "Yidi:i", (ftnlen)32, (ftnlen)6);
	s_copy(mymnng + 672, "Yy*H*M", (ftnlen)32, (ftnlen)6);
	s_copy(myknwn + 704, "Yidi:i:i", (ftnlen)32, (ftnlen)8);
	s_copy(mymnng + 704, "Yy*H*M*S", (ftnlen)32, (ftnlen)8);
	s_copy(myknwn + 736, "Yidi:i:n", (ftnlen)32, (ftnlen)8);
	s_copy(mymnng + 736, "Yy*H*M*S", (ftnlen)32, (ftnlen)8);
	s_copy(myknwn + 768, "Yidi:n", (ftnlen)32, (ftnlen)6);
	s_copy(mymnng + 768, "Yy*H*M", (ftnlen)32, (ftnlen)6);
	s_copy(myknwn + 800, "Yii", (ftnlen)32, (ftnlen)3);
	s_copy(mymnng + 800, "YmD", (ftnlen)32, (ftnlen)3);
	s_copy(myknwn + 832, "Yiii", (ftnlen)32, (ftnlen)4);
	s_copy(mymnng + 832, "YmDH", (ftnlen)32, (ftnlen)4);
	s_copy(myknwn + 864, "Yiii:i", (ftnlen)32, (ftnlen)6);
	s_copy(mymnng + 864, "YmDH*M", (ftnlen)32, (ftnlen)6);
	s_copy(myknwn + 896, "Yiii:i:i", (ftnlen)32, (ftnlen)8);
	s_copy(mymnng + 896, "YmDH*M*S", (ftnlen)32, (ftnlen)8);
	s_copy(myknwn + 928, "Yiii:i:n", (ftnlen)32, (ftnlen)8);
	s_copy(mymnng + 928, "YmDH*M*S", (ftnlen)32, (ftnlen)8);
	s_copy(myknwn + 960, "Yiii:n", (ftnlen)32, (ftnlen)6);
	s_copy(mymnng + 960, "YmDH*M", (ftnlen)32, (ftnlen)6);
	s_copy(myknwn + 992, "Yiiii", (ftnlen)32, (ftnlen)5);
	s_copy(mymnng + 992, "YmDHM", (ftnlen)32, (ftnlen)5);
	s_copy(myknwn + 1024, "Yiiiii", (ftnlen)32, (ftnlen)6);
	s_copy(mymnng + 1024, "YmDHMS", (ftnlen)32, (ftnlen)6);
	s_copy(myknwn + 1056, "Yiiiin", (ftnlen)32, (ftnlen)6);
	s_copy(mymnng + 1056, "YmDHMS", (ftnlen)32, (ftnlen)6);
	s_copy(myknwn + 1088, "Yiiin", (ftnlen)32, (ftnlen)5);
	s_copy(mymnng + 1088, "YmDHM", (ftnlen)32, (ftnlen)5);
	s_copy(myknwn + 1120, "Yiin", (ftnlen)32, (ftnlen)4);
	s_copy(mymnng + 1120, "YmDH", (ftnlen)32, (ftnlen)4);
	s_copy(myknwn + 1152, "Yim", (ftnlen)32, (ftnlen)3);
	s_copy(mymnng + 1152, "YDm", (ftnlen)32, (ftnlen)3);
	s_copy(myknwn + 1184, "Yimi", (ftnlen)32, (ftnlen)4);
	s_copy(mymnng + 1184, "YDmH", (ftnlen)32, (ftnlen)4);
	s_copy(myknwn + 1216, "Yimi:i", (ftnlen)32, (ftnlen)6);
	s_copy(mymnng + 1216, "YDmH*M", (ftnlen)32, (ftnlen)6);
	s_copy(myknwn + 1248, "Yimi:i:i", (ftnlen)32, (ftnlen)8);
	s_copy(mymnng + 1248, "YDmH*M*S", (ftnlen)32, (ftnlen)8);
	s_copy(myknwn + 1280, "Yimi:i:n", (ftnlen)32, (ftnlen)8);
	s_copy(mymnng + 1280, "YDmH*M*S", (ftnlen)32, (ftnlen)8);
	s_copy(myknwn + 1312, "Yimi:n", (ftnlen)32, (ftnlen)6);
	s_copy(mymnng + 1312, "YDmH*M", (ftnlen)32, (ftnlen)6);
	s_copy(myknwn + 1344, "Yimn", (ftnlen)32, (ftnlen)4);
	s_copy(mymnng + 1344, "YDmH", (ftnlen)32, (ftnlen)4);
	s_copy(myknwn + 1376, "Yin", (ftnlen)32, (ftnlen)3);
	s_copy(mymnng + 1376, "YmD", (ftnlen)32, (ftnlen)3);
	s_copy(myknwn + 1408, "Ymi", (ftnlen)32, (ftnlen)3);
	s_copy(mymnng + 1408, "YmD", (ftnlen)32, (ftnlen)3);
	s_copy(myknwn + 1440, "Ymii", (ftnlen)32, (ftnlen)4);
	s_copy(mymnng + 1440, "YmDH", (ftnlen)32, (ftnlen)4);
	s_copy(myknwn + 1472, "Ymii:i", (ftnlen)32, (ftnlen)6);
	s_copy(mymnng + 1472, "YmDH*M", (ftnlen)32, (ftnlen)6);
	s_copy(myknwn + 1504, "Ymii:i:i", (ftnlen)32, (ftnlen)8);
	s_copy(mymnng + 1504, "YmDH*M*S", (ftnlen)32, (ftnlen)8);
	s_copy(myknwn + 1536, "Ymii:i:n", (ftnlen)32, (ftnlen)8);
	s_copy(mymnng + 1536, "YmDH*M*S", (ftnlen)32, (ftnlen)8);
	s_copy(myknwn + 1568, "Ymii:n", (ftnlen)32, (ftnlen)6);
	s_copy(mymnng + 1568, "YmDH*M", (ftnlen)32, (ftnlen)6);
	s_copy(myknwn + 1600, "Ymin", (ftnlen)32, (ftnlen)4);
	s_copy(mymnng + 1600, "YmDH", (ftnlen)32, (ftnlen)4);
	s_copy(myknwn + 1632, "Ymn", (ftnlen)32, (ftnlen)3);
	s_copy(mymnng + 1632, "YmD", (ftnlen)32, (ftnlen)3);
	s_copy(myknwn + 1664, "Ynm", (ftnlen)32, (ftnlen)3);
	s_copy(mymnng + 1664, "YDm", (ftnlen)32, (ftnlen)3);
	s_copy(myknwn + 1696, "i-Y/", (ftnlen)32, (ftnlen)4);
	s_copy(mymnng + 1696, "y*Y*", (ftnlen)32, (ftnlen)4);
	s_copy(myknwn + 1728, "i-Y/i:i", (ftnlen)32, (ftnlen)7);
	s_copy(mymnng + 1728, "y*Y*H*M", (ftnlen)32, (ftnlen)7);
	s_copy(myknwn + 1760, "i-Y/i:i:i", (ftnlen)32, (ftnlen)9);
	s_copy(mymnng + 1760, "y*Y*H*M*S", (ftnlen)32, (ftnlen)9);
	s_copy(myknwn + 1792, "i-Y/i:i:n", (ftnlen)32, (ftnlen)9);
	s_copy(mymnng + 1792, "y*Y*H*M*S", (ftnlen)32, (ftnlen)9);
	s_copy(myknwn + 1824, "i-Y/i:n", (ftnlen)32, (ftnlen)7);
	s_copy(mymnng + 1824, "y*Y*H*M", (ftnlen)32, (ftnlen)7);
	s_copy(myknwn + 1856, "i-Yd", (ftnlen)32, (ftnlen)4);
	s_copy(mymnng + 1856, "y*Y*", (ftnlen)32, (ftnlen)4);
	s_copy(myknwn + 1888, "i-Ydi:i", (ftnlen)32, (ftnlen)7);
	s_copy(mymnng + 1888, "y*Y*H*M", (ftnlen)32, (ftnlen)7);
	s_copy(myknwn + 1920, "i-Ydi:i:i", (ftnlen)32, (ftnlen)9);
	s_copy(mymnng + 1920, "y*Y*H*M*S", (ftnlen)32, (ftnlen)9);
	s_copy(myknwn + 1952, "i-Ydi:i:n", (ftnlen)32, (ftnlen)9);
	s_copy(mymnng + 1952, "y*Y*H*M*S", (ftnlen)32, (ftnlen)9);
	s_copy(myknwn + 1984, "i-Ydi:n", (ftnlen)32, (ftnlen)7);
	s_copy(mymnng + 1984, "y*Y*H*M", (ftnlen)32, (ftnlen)7);
	s_copy(myknwn + 2016, "i-i-it", (ftnlen)32, (ftnlen)6);
	s_copy(mymnng + 2016, "Y*m*D*", (ftnlen)32, (ftnlen)6);
	s_copy(myknwn + 2048, "i-i-iti:i", (ftnlen)32, (ftnlen)9);
	s_copy(mymnng + 2048, "Y*m*D*H*M", (ftnlen)32, (ftnlen)9);
	s_copy(myknwn + 2080, "i-i-iti:i:i", (ftnlen)32, (ftnlen)11);
	s_copy(mymnng + 2080, "Y*m*D*H*M*S", (ftnlen)32, (ftnlen)11);
	s_copy(myknwn + 2112, "i-i-iti:i:n", (ftnlen)32, (ftnlen)11);
	s_copy(mymnng + 2112, "Y*m*D*H*M*S", (ftnlen)32, (ftnlen)11);
	s_copy(myknwn + 2144, "i-i-iti:n", (ftnlen)32, (ftnlen)9);
	s_copy(mymnng + 2144, "Y*m*D*H*M", (ftnlen)32, (ftnlen)9);
	s_copy(myknwn + 2176, "i-i/i:i", (ftnlen)32, (ftnlen)7);
	s_copy(mymnng + 2176, "Y*y*H*M", (ftnlen)32, (ftnlen)7);
	s_copy(myknwn + 2208, "i-i/i:i:i", (ftnlen)32, (ftnlen)9);
	s_copy(mymnng + 2208, "Y*y*H*M*S", (ftnlen)32, (ftnlen)9);
	s_copy(myknwn + 2240, "i-i/i:i:n", (ftnlen)32, (ftnlen)9);
	s_copy(mymnng + 2240, "Y*y*H*M*S", (ftnlen)32, (ftnlen)9);
	s_copy(myknwn + 2272, "i-i/i:n", (ftnlen)32, (ftnlen)7);
	s_copy(mymnng + 2272, "Y*y*H*M", (ftnlen)32, (ftnlen)7);
	s_copy(myknwn + 2304, "i-idi:i", (ftnlen)32, (ftnlen)7);
	s_copy(mymnng + 2304, "Y*y*H*M", (ftnlen)32, (ftnlen)7);
	s_copy(myknwn + 2336, "i-idi:i:i", (ftnlen)32, (ftnlen)9);
	s_copy(mymnng + 2336, "Y*y*H*M*S", (ftnlen)32, (ftnlen)9);
	s_copy(myknwn + 2368, "i-idi:i:n", (ftnlen)32, (ftnlen)9);
	s_copy(mymnng + 2368, "Y*y*H*M*S", (ftnlen)32, (ftnlen)9);
	s_copy(myknwn + 2400, "i-idi:n", (ftnlen)32, (ftnlen)7);
	s_copy(mymnng + 2400, "Y*y*H*M", (ftnlen)32, (ftnlen)7);
	s_copy(myknwn + 2432, "i-it", (ftnlen)32, (ftnlen)4);
	s_copy(mymnng + 2432, "Y*y*", (ftnlen)32, (ftnlen)4);
	s_copy(myknwn + 2464, "i-iti:i", (ftnlen)32, (ftnlen)7);
	s_copy(mymnng + 2464, "Y*y*H*M", (ftnlen)32, (ftnlen)7);
	s_copy(myknwn + 2496, "i-iti:i:i", (ftnlen)32, (ftnlen)9);
	s_copy(mymnng + 2496, "Y*y*H*M*S", (ftnlen)32, (ftnlen)9);
	s_copy(myknwn + 2528, "i-iti:i:n", (ftnlen)32, (ftnlen)9);
	s_copy(mymnng + 2528, "Y*y*H*M*S", (ftnlen)32, (ftnlen)9);
	s_copy(myknwn + 2560, "i-iti:n", (ftnlen)32, (ftnlen)7);
	s_copy(mymnng + 2560, "Y*y*H*M", (ftnlen)32, (ftnlen)7);
	s_copy(myknwn + 2592, "i:i:iimY", (ftnlen)32, (ftnlen)8);
	s_copy(mymnng + 2592, "H*M*SDmY", (ftnlen)32, (ftnlen)8);
	s_copy(myknwn + 2624, "i:i:imiY", (ftnlen)32, (ftnlen)8);
	s_copy(mymnng + 2624, "H*M*SmDY", (ftnlen)32, (ftnlen)8);
	s_copy(myknwn + 2656, "i:i:nimY", (ftnlen)32, (ftnlen)8);
	s_copy(mymnng + 2656, "H*M*SDmY", (ftnlen)32, (ftnlen)8);
	s_copy(myknwn + 2688, "i:i:nmiY", (ftnlen)32, (ftnlen)8);
	s_copy(mymnng + 2688, "H*M*SmDY", (ftnlen)32, (ftnlen)8);
	s_copy(myknwn + 2720, "i:iimY", (ftnlen)32, (ftnlen)6);
	s_copy(mymnng + 2720, "H*MDmY", (ftnlen)32, (ftnlen)6);
	s_copy(myknwn + 2752, "i:imiY", (ftnlen)32, (ftnlen)6);
	s_copy(mymnng + 2752, "H*MmDY", (ftnlen)32, (ftnlen)6);
	s_copy(myknwn + 2784, "i:nimY", (ftnlen)32, (ftnlen)6);
	s_copy(mymnng + 2784, "H*MDmY", (ftnlen)32, (ftnlen)6);
	s_copy(myknwn + 2816, "i:nmiY", (ftnlen)32, (ftnlen)6);
	s_copy(mymnng + 2816, "H*MmDY", (ftnlen)32, (ftnlen)6);
	s_copy(myknwn + 2848, "iYd", (ftnlen)32, (ftnlen)3);
	s_copy(mymnng + 2848, "yY*", (ftnlen)32, (ftnlen)3);
	s_copy(myknwn + 2880, "iYdi:i", (ftnlen)32, (ftnlen)6);
	s_copy(mymnng + 2880, "yY*H*M", (ftnlen)32, (ftnlen)6);
	s_copy(myknwn + 2912, "iYdi:i:i", (ftnlen)32, (ftnlen)8);
	s_copy(mymnng + 2912, "yY*H*M*S", (ftnlen)32, (ftnlen)8);
	s_copy(myknwn + 2944, "iYdi:i:n", (ftnlen)32, (ftnlen)8);
	s_copy(mymnng + 2944, "yY*H*M*S", (ftnlen)32, (ftnlen)8);
	s_copy(myknwn + 2976, "iYdi:n", (ftnlen)32, (ftnlen)6);
	s_copy(mymnng + 2976, "yY*H*M", (ftnlen)32, (ftnlen)6);
	s_copy(myknwn + 3008, "iiY", (ftnlen)32, (ftnlen)3);
	s_copy(mymnng + 3008, "mDY", (ftnlen)32, (ftnlen)3);
	s_copy(myknwn + 3040, "iiYi", (ftnlen)32, (ftnlen)4);
	s_copy(mymnng + 3040, "mDYH", (ftnlen)32, (ftnlen)4);
	s_copy(myknwn + 3072, "iiYi:i", (ftnlen)32, (ftnlen)6);
	s_copy(mymnng + 3072, "mDYH*M", (ftnlen)32, (ftnlen)6);
	s_copy(myknwn + 3104, "iiYi:i:i", (ftnlen)32, (ftnlen)8);
	s_copy(mymnng + 3104, "mDYH*M*S", (ftnlen)32, (ftnlen)8);
	s_copy(myknwn + 3136, "iiYi:i:n", (ftnlen)32, (ftnlen)8);
	s_copy(mymnng + 3136, "mDYH*M*S", (ftnlen)32, (ftnlen)8);
	s_copy(myknwn + 3168, "iiYi:n", (ftnlen)32, (ftnlen)6);
	s_copy(mymnng + 3168, "mDYH*M", (ftnlen)32, (ftnlen)6);
	s_copy(myknwn + 3200, "iiYn", (ftnlen)32, (ftnlen)4);
	s_copy(mymnng + 3200, "mDYH", (ftnlen)32, (ftnlen)4);
	s_copy(myknwn + 3232, "iid", (ftnlen)32, (ftnlen)3);
	s_copy(mymnng + 3232, "Yy*", (ftnlen)32, (ftnlen)3);
	s_copy(myknwn + 3264, "iidi:i", (ftnlen)32, (ftnlen)6);
	s_copy(mymnng + 3264, "Yy*H*M", (ftnlen)32, (ftnlen)6);
	s_copy(myknwn + 3296, "iidi:i:i", (ftnlen)32, (ftnlen)8);
	s_copy(mymnng + 3296, "Yy*H*M*S", (ftnlen)32, (ftnlen)8);
	s_copy(myknwn + 3328, "iidi:i:n", (ftnlen)32, (ftnlen)8);
	s_copy(mymnng + 3328, "Yy*H*M*S", (ftnlen)32, (ftnlen)8);
	s_copy(myknwn + 3360, "iidi:n", (ftnlen)32, (ftnlen)6);
	s_copy(mymnng + 3360, "Yy*H*M", (ftnlen)32, (ftnlen)6);
	s_copy(myknwn + 3392, "iim", (ftnlen)32, (ftnlen)3);
	s_copy(mymnng + 3392, "YDm", (ftnlen)32, (ftnlen)3);
	s_copy(myknwn + 3424, "iimi", (ftnlen)32, (ftnlen)4);
	s_copy(mymnng + 3424, "YDmH", (ftnlen)32, (ftnlen)4);
	s_copy(myknwn + 3456, "iimi:i", (ftnlen)32, (ftnlen)6);
	s_copy(mymnng + 3456, "YDmH*M", (ftnlen)32, (ftnlen)6);
	s_copy(myknwn + 3488, "iimi:i:i", (ftnlen)32, (ftnlen)8);
	s_copy(mymnng + 3488, "YDmH*M*S", (ftnlen)32, (ftnlen)8);
	s_copy(myknwn + 3520, "iimi:i:n", (ftnlen)32, (ftnlen)8);
	s_copy(mymnng + 3520, "YDmH*M*S", (ftnlen)32, (ftnlen)8);
	s_copy(myknwn + 3552, "iimi:n", (ftnlen)32, (ftnlen)6);
	s_copy(mymnng + 3552, "YDmH*M", (ftnlen)32, (ftnlen)6);
	s_copy(myknwn + 3584, "iimii", (ftnlen)32, (ftnlen)5);
	s_copy(mymnng + 3584, "YDmHM", (ftnlen)32, (ftnlen)5);
	s_copy(myknwn + 3616, "iimiii", (ftnlen)32, (ftnlen)6);
	s_copy(mymnng + 3616, "YDmHMS", (ftnlen)32, (ftnlen)6);
	s_copy(myknwn + 3648, "iimiin", (ftnlen)32, (ftnlen)6);
	s_copy(mymnng + 3648, "YDmHMS", (ftnlen)32, (ftnlen)6);
	s_copy(myknwn + 3680, "iimin", (ftnlen)32, (ftnlen)5);
	s_copy(mymnng + 3680, "YDmHM", (ftnlen)32, (ftnlen)5);
	s_copy(myknwn + 3712, "iimn", (ftnlen)32, (ftnlen)4);
	s_copy(mymnng + 3712, "YDmH", (ftnlen)32, (ftnlen)4);
	s_copy(myknwn + 3744, "imY", (ftnlen)32, (ftnlen)3);
	s_copy(mymnng + 3744, "DmY", (ftnlen)32, (ftnlen)3);
	s_copy(myknwn + 3776, "imYi", (ftnlen)32, (ftnlen)4);
	s_copy(mymnng + 3776, "DmYH", (ftnlen)32, (ftnlen)4);
	s_copy(myknwn + 3808, "imYi:i", (ftnlen)32, (ftnlen)6);
	s_copy(mymnng + 3808, "DmYH*M", (ftnlen)32, (ftnlen)6);
	s_copy(myknwn + 3840, "imYi:i:i", (ftnlen)32, (ftnlen)8);
	s_copy(mymnng + 3840, "DmYH*M*S", (ftnlen)32, (ftnlen)8);
	s_copy(myknwn + 3872, "imYi:i:n", (ftnlen)32, (ftnlen)8);
	s_copy(mymnng + 3872, "DmYH*M*S", (ftnlen)32, (ftnlen)8);
	s_copy(myknwn + 3904, "imYi:n", (ftnlen)32, (ftnlen)6);
	s_copy(mymnng + 3904, "DmYH*M", (ftnlen)32, (ftnlen)6);
	s_copy(myknwn + 3936, "imYn", (ftnlen)32, (ftnlen)4);
	s_copy(mymnng + 3936, "DmYH", (ftnlen)32, (ftnlen)4);
	s_copy(myknwn + 3968, "imi", (ftnlen)32, (ftnlen)3);
	s_copy(mymnng + 3968, "YmD", (ftnlen)32, (ftnlen)3);
	s_copy(myknwn + 4000, "imi:i:iY", (ftnlen)32, (ftnlen)8);
	s_copy(mymnng + 4000, "DmH*M*SY", (ftnlen)32, (ftnlen)8);
	s_copy(myknwn + 4032, "imi:i:nY", (ftnlen)32, (ftnlen)8);
	s_copy(mymnng + 4032, "DmH*M*SY", (ftnlen)32, (ftnlen)8);
	s_copy(myknwn + 4064, "imi:iY", (ftnlen)32, (ftnlen)6);
	s_copy(mymnng + 4064, "DmH*MY", (ftnlen)32, (ftnlen)6);
	s_copy(myknwn + 4096, "imi:nY", (ftnlen)32, (ftnlen)6);
	s_copy(mymnng + 4096, "DmH*MY", (ftnlen)32, (ftnlen)6);
	s_copy(myknwn + 4128, "imii", (ftnlen)32, (ftnlen)4);
	s_copy(mymnng + 4128, "YmDH", (ftnlen)32, (ftnlen)4);
	s_copy(myknwn + 4160, "imii:i", (ftnlen)32, (ftnlen)6);
	s_copy(mymnng + 4160, "YmDH*M", (ftnlen)32, (ftnlen)6);
	s_copy(myknwn + 4192, "imii:i:i", (ftnlen)32, (ftnlen)8);
	s_copy(mymnng + 4192, "YmDH*M*S", (ftnlen)32, (ftnlen)8);
	s_copy(myknwn + 4224, "imii:i:n", (ftnlen)32, (ftnlen)8);
	s_copy(mymnng + 4224, "YmDH*M*S", (ftnlen)32, (ftnlen)8);
	s_copy(myknwn + 4256, "imii:n", (ftnlen)32, (ftnlen)6);
	s_copy(mymnng + 4256, "YmDH*M", (ftnlen)32, (ftnlen)6);
	s_copy(myknwn + 4288, "imiii", (ftnlen)32, (ftnlen)5);
	s_copy(mymnng + 4288, "YmDHM", (ftnlen)32, (ftnlen)5);
	s_copy(myknwn + 4320, "imiiii", (ftnlen)32, (ftnlen)6);
	s_copy(mymnng + 4320, "YmDHMS", (ftnlen)32, (ftnlen)6);
	s_copy(myknwn + 4352, "imiiin", (ftnlen)32, (ftnlen)6);
	s_copy(mymnng + 4352, "YmDHMS", (ftnlen)32, (ftnlen)6);
	s_copy(myknwn + 4384, "imiin", (ftnlen)32, (ftnlen)5);
	s_copy(mymnng + 4384, "YmDHM", (ftnlen)32, (ftnlen)5);
	s_copy(myknwn + 4416, "imin", (ftnlen)32, (ftnlen)4);
	s_copy(mymnng + 4416, "YmDH", (ftnlen)32, (ftnlen)4);
	s_copy(myknwn + 4448, "imn", (ftnlen)32, (ftnlen)3);
	s_copy(mymnng + 4448, "YmD", (ftnlen)32, (ftnlen)3);
	s_copy(myknwn + 4480, "inY", (ftnlen)32, (ftnlen)3);
	s_copy(mymnng + 4480, "mDY", (ftnlen)32, (ftnlen)3);
	s_copy(myknwn + 4512, "inm", (ftnlen)32, (ftnlen)3);
	s_copy(mymnng + 4512, "YDm", (ftnlen)32, (ftnlen)3);
	s_copy(myknwn + 4544, "miY", (ftnlen)32, (ftnlen)3);
	s_copy(mymnng + 4544, "mDY", (ftnlen)32, (ftnlen)3);
	s_copy(myknwn + 4576, "miYi", (ftnlen)32, (ftnlen)4);
	s_copy(mymnng + 4576, "mDYH", (ftnlen)32, (ftnlen)4);
	s_copy(myknwn + 4608, "miYi:i", (ftnlen)32, (ftnlen)6);
	s_copy(mymnng + 4608, "mDYH*M", (ftnlen)32, (ftnlen)6);
	s_copy(myknwn + 4640, "miYi:i:i", (ftnlen)32, (ftnlen)8);
	s_copy(mymnng + 4640, "mDYH*M*S", (ftnlen)32, (ftnlen)8);
	s_copy(myknwn + 4672, "miYi:i:n", (ftnlen)32, (ftnlen)8);
	s_copy(mymnng + 4672, "mDYH*M*S", (ftnlen)32, (ftnlen)8);
	s_copy(myknwn + 4704, "miYi:n", (ftnlen)32, (ftnlen)6);
	s_copy(mymnng + 4704, "mDYH*M", (ftnlen)32, (ftnlen)6);
	s_copy(myknwn + 4736, "miYn", (ftnlen)32, (ftnlen)4);
	s_copy(mymnng + 4736, "mDYH", (ftnlen)32, (ftnlen)4);
	s_copy(myknwn + 4768, "mii", (ftnlen)32, (ftnlen)3);
	s_copy(mymnng + 4768, "mDY", (ftnlen)32, (ftnlen)3);
	s_copy(myknwn + 4800, "mii:i:iY", (ftnlen)32, (ftnlen)8);
	s_copy(mymnng + 4800, "mDH*M*SY", (ftnlen)32, (ftnlen)8);
	s_copy(myknwn + 4832, "mii:i:nY", (ftnlen)32, (ftnlen)8);
	s_copy(mymnng + 4832, "mDH*M*SY", (ftnlen)32, (ftnlen)8);
	s_copy(myknwn + 4864, "mii:iY", (ftnlen)32, (ftnlen)6);
	s_copy(mymnng + 4864, "mDH*MY", (ftnlen)32, (ftnlen)6);
	s_copy(myknwn + 4896, "mii:nY", (ftnlen)32, (ftnlen)6);
	s_copy(mymnng + 4896, "mDH*MY", (ftnlen)32, (ftnlen)6);
	s_copy(myknwn + 4928, "miii", (ftnlen)32, (ftnlen)4);
	s_copy(mymnng + 4928, "mDYH", (ftnlen)32, (ftnlen)4);
	s_copy(myknwn + 4960, "miii:i", (ftnlen)32, (ftnlen)6);
	s_copy(mymnng + 4960, "mDYH*M", (ftnlen)32, (ftnlen)6);
	s_copy(myknwn + 4992, "miii:i:i", (ftnlen)32, (ftnlen)8);
	s_copy(mymnng + 4992, "mDYH*M*S", (ftnlen)32, (ftnlen)8);
	s_copy(myknwn + 5024, "miii:i:n", (ftnlen)32, (ftnlen)8);
	s_copy(mymnng + 5024, "mDYH*M*S", (ftnlen)32, (ftnlen)8);
	s_copy(myknwn + 5056, "miii:n", (ftnlen)32, (ftnlen)6);
	s_copy(mymnng + 5056, "mDYH*M", (ftnlen)32, (ftnlen)6);
	s_copy(myknwn + 5088, "miiii", (ftnlen)32, (ftnlen)5);
	s_copy(mymnng + 5088, "mDYHM", (ftnlen)32, (ftnlen)5);
	s_copy(myknwn + 5120, "miiiii", (ftnlen)32, (ftnlen)6);
	s_copy(mymnng + 5120, "mDYHMS", (ftnlen)32, (ftnlen)6);
	s_copy(myknwn + 5152, "miiiin", (ftnlen)32, (ftnlen)6);
	s_copy(mymnng + 5152, "mDYHMS", (ftnlen)32, (ftnlen)6);
	s_copy(myknwn + 5184, "miiin", (ftnlen)32, (ftnlen)5);
	s_copy(mymnng + 5184, "mDYHM", (ftnlen)32, (ftnlen)5);
	s_copy(myknwn + 5216, "miin", (ftnlen)32, (ftnlen)4);
	s_copy(mymnng + 5216, "mDYH", (ftnlen)32, (ftnlen)4);
	s_copy(myknwn + 5248, "mnY", (ftnlen)32, (ftnlen)3);
	s_copy(mymnng + 5248, "mDY", (ftnlen)32, (ftnlen)3);
	s_copy(myknwn + 5280, "mni", (ftnlen)32, (ftnlen)3);
	s_copy(mymnng + 5280, "mDY", (ftnlen)32, (ftnlen)3);
	s_copy(myknwn + 5312, "nmY", (ftnlen)32, (ftnlen)3);
	s_copy(mymnng + 5312, "DmY", (ftnlen)32, (ftnlen)3);
	s_copy(myknwn + 5344, "i/i/i", (ftnlen)32, (ftnlen)5);
	s_copy(mymnng + 5344, "m*D*Y", (ftnlen)32, (ftnlen)5);
	s_copy(myknwn + 5376, "i/i/ii:i", (ftnlen)32, (ftnlen)8);
	s_copy(mymnng + 5376, "m*D*YH*M", (ftnlen)32, (ftnlen)8);
	s_copy(myknwn + 5408, "i/i/ii:n", (ftnlen)32, (ftnlen)8);
	s_copy(mymnng + 5408, "m*D*YH*M", (ftnlen)32, (ftnlen)8);
	s_copy(myknwn + 5440, "i/i/ii:i:n", (ftnlen)32, (ftnlen)10);
	s_copy(mymnng + 5440, "m*D*YH*M*S", (ftnlen)32, (ftnlen)10);
	s_copy(myknwn + 5472, "i/i/ii:i:i", (ftnlen)32, (ftnlen)10);
	s_copy(mymnng + 5472, "m*D*YH*M*S", (ftnlen)32, (ftnlen)10);
	s_copy(myknwn + 5504, "i/i/Y", (ftnlen)32, (ftnlen)5);
	s_copy(mymnng + 5504, "m*D*Y", (ftnlen)32, (ftnlen)5);
	s_copy(myknwn + 5536, "i/i/Yi:i", (ftnlen)32, (ftnlen)8);
	s_copy(mymnng + 5536, "m*D*YH*M", (ftnlen)32, (ftnlen)8);
	s_copy(myknwn + 5568, "i/i/ii:n", (ftnlen)32, (ftnlen)8);
	s_copy(mymnng + 5568, "m*D*YH*M", (ftnlen)32, (ftnlen)8);
	s_copy(myknwn + 5600, "i/i/Yi:i:n", (ftnlen)32, (ftnlen)10);
	s_copy(mymnng + 5600, "m*D*YH*M*S", (ftnlen)32, (ftnlen)10);
	s_copy(myknwn + 5632, "i/i/Yi:i:i", (ftnlen)32, (ftnlen)10);
	s_copy(mymnng + 5632, "m*D*YH*M*S", (ftnlen)32, (ftnlen)10);
	s_copy(myknwn + 5664, "Y-i-iti", (ftnlen)32, (ftnlen)7);
	s_copy(mymnng + 5664, "Y*m*D*H", (ftnlen)32, (ftnlen)7);
	s_copy(myknwn + 5696, "Y-iti", (ftnlen)32, (ftnlen)5);
	s_copy(mymnng + 5696, "Y*y*H", (ftnlen)32, (ftnlen)5);
	s_copy(myknwn + 5728, "Y-i-itn", (ftnlen)32, (ftnlen)7);
	s_copy(mymnng + 5728, "Y*m*D*H", (ftnlen)32, (ftnlen)7);
	s_copy(myknwn + 5760, "Y-itn", (ftnlen)32, (ftnlen)5);
	s_copy(mymnng + 5760, "Y*y*H", (ftnlen)32, (ftnlen)5);
	s_copy(myknwn + 5792, "i-i-iti", (ftnlen)32, (ftnlen)7);
	s_copy(mymnng + 5792, "Y*m*D*H", (ftnlen)32, (ftnlen)7);
	s_copy(myknwn + 5824, "i-i-itn", (ftnlen)32, (ftnlen)7);
	s_copy(mymnng + 5824, "Y*m*D*H", (ftnlen)32, (ftnlen)7);
	s_copy(myknwn + 5856, "i-iti", (ftnlen)32, (ftnlen)5);
	s_copy(mymnng + 5856, "Y*y*H", (ftnlen)32, (ftnlen)5);
	s_copy(myknwn + 5888, "i-itn", (ftnlen)32, (ftnlen)5);
	s_copy(mymnng + 5888, "Y*y*H", (ftnlen)32, (ftnlen)5);
	s_copy(myknwn + 5920, "i:ii/i/i", (ftnlen)32, (ftnlen)8);
	s_copy(mymnng + 5920, "H*Mm*D*Y", (ftnlen)32, (ftnlen)8);
	s_copy(myknwn + 5952, "i:ni/i/i", (ftnlen)32, (ftnlen)8);
	s_copy(mymnng + 5952, "H*Mm*D*Y", (ftnlen)32, (ftnlen)8);
	s_copy(myknwn + 5984, "i:i:ii/i/i", (ftnlen)32, (ftnlen)10);
	s_copy(mymnng + 5984, "H*M*Sm*D*Y", (ftnlen)32, (ftnlen)10);
	s_copy(myknwn + 6016, "i:i:ni/i/i", (ftnlen)32, (ftnlen)10);
	s_copy(mymnng + 6016, "H*M*Sm*D*Y", (ftnlen)32, (ftnlen)10);
	s_copy(myknwn + 6048, "i:ii/i/Y", (ftnlen)32, (ftnlen)8);
	s_copy(mymnng + 6048, "H*Mm*D*Y", (ftnlen)32, (ftnlen)8);
	s_copy(myknwn + 6080, "i:ni/i/Y", (ftnlen)32, (ftnlen)8);
	s_copy(mymnng + 6080, "H*Mm*D*Y", (ftnlen)32, (ftnlen)8);
	s_copy(myknwn + 6112, "i:i:ii/i/Y", (ftnlen)32, (ftnlen)10);
	s_copy(mymnng + 6112, "H*M*Sm*D*Y", (ftnlen)32, (ftnlen)10);
	s_copy(myknwn + 6144, "i:i:ni/i/Y", (ftnlen)32, (ftnlen)10);
	s_copy(mymnng + 6144, "H*M*Sm*D*Y", (ftnlen)32, (ftnlen)10);
	s_copy(myknwn + 6176, "i:ii-i-Y", (ftnlen)32, (ftnlen)8);
	s_copy(mymnng + 6176, "H*Mm*D*Y", (ftnlen)32, (ftnlen)8);
	s_copy(myknwn + 6208, "i:ni-i-Y", (ftnlen)32, (ftnlen)8);
	s_copy(mymnng + 6208, "H*Mm*D*Y", (ftnlen)32, (ftnlen)8);
	s_copy(myknwn + 6240, "i:i:ii-i-Y", (ftnlen)32, (ftnlen)10);
	s_copy(mymnng + 6240, "H*M*Sm*D*Y", (ftnlen)32, (ftnlen)10);
	s_copy(myknwn + 6272, "i:i:ni-i-Y", (ftnlen)32, (ftnlen)10);
	s_copy(mymnng + 6272, "H*M*Sm*D*Y", (ftnlen)32, (ftnlen)10);
	s_copy(myknwn + 6304, "i/i/Y/i:n", (ftnlen)32, (ftnlen)9);
	s_copy(mymnng + 6304, "m*D*Y*H*M", (ftnlen)32, (ftnlen)9);
	s_copy(myknwn + 6336, "i-i-Y", (ftnlen)32, (ftnlen)5);
	s_copy(mymnng + 6336, "m*D*Y", (ftnlen)32, (ftnlen)5);
	s_copy(myknwn + 6368, "i-i-Yi:n", (ftnlen)32, (ftnlen)8);
	s_copy(mymnng + 6368, "m*D*YH*M", (ftnlen)32, (ftnlen)8);
	s_copy(myknwn + 6400, "i-i-Yi:i:n", (ftnlen)32, (ftnlen)10);
	s_copy(mymnng + 6400, "m*D*YH*M*S", (ftnlen)32, (ftnlen)10);
	s_copy(myknwn + 6432, "i-i-Yi:i:i", (ftnlen)32, (ftnlen)10);
	s_copy(mymnng + 6432, "m*D*YH*M*S", (ftnlen)32, (ftnlen)10);
	s_copy(myknwn + 6464, "i-i-Yi:i", (ftnlen)32, (ftnlen)8);
	s_copy(mymnng + 6464, "m*D*YH*M", (ftnlen)32, (ftnlen)8);

/*        ISO patterns allowing the 'Z' suffix, only a suffix. */

	s_copy(myknwn + 6496, "Y-i-itx", (ftnlen)32, (ftnlen)7);
	s_copy(mymnng + 6496, "Y*m*D**", (ftnlen)32, (ftnlen)7);
	s_copy(myknwn + 6528, "Y-i-iti:ix", (ftnlen)32, (ftnlen)10);
	s_copy(mymnng + 6528, "Y*m*D*H*M*", (ftnlen)32, (ftnlen)10);
	s_copy(myknwn + 6560, "Y-i-iti:i:ix", (ftnlen)32, (ftnlen)12);
	s_copy(mymnng + 6560, "Y*m*D*H*M*S*", (ftnlen)32, (ftnlen)12);
	s_copy(myknwn + 6592, "Y-i-iti:i:nx", (ftnlen)32, (ftnlen)12);
	s_copy(mymnng + 6592, "Y*m*D*H*M*S*", (ftnlen)32, (ftnlen)12);
	s_copy(myknwn + 6624, "Y-i-iti:nx", (ftnlen)32, (ftnlen)10);
	s_copy(mymnng + 6624, "Y*m*D*H*M*", (ftnlen)32, (ftnlen)10);
	s_copy(myknwn + 6656, "Y-itx", (ftnlen)32, (ftnlen)5);
	s_copy(mymnng + 6656, "Y*y**", (ftnlen)32, (ftnlen)5);
	s_copy(myknwn + 6688, "Y-iti:ix", (ftnlen)32, (ftnlen)8);
	s_copy(mymnng + 6688, "Y*y*H*M*", (ftnlen)32, (ftnlen)8);
	s_copy(myknwn + 6720, "Y-iti:i:ix", (ftnlen)32, (ftnlen)10);
	s_copy(mymnng + 6720, "Y*y*H*M*S*", (ftnlen)32, (ftnlen)10);
	s_copy(myknwn + 6752, "Y-iti:i:nx", (ftnlen)32, (ftnlen)10);
	s_copy(mymnng + 6752, "Y*y*H*M*S*", (ftnlen)32, (ftnlen)10);
	s_copy(myknwn + 6784, "Y-iti:nx", (ftnlen)32, (ftnlen)8);
	s_copy(mymnng + 6784, "Y*y*H*M*", (ftnlen)32, (ftnlen)8);
	s_copy(myknwn + 6816, "i-i-itx", (ftnlen)32, (ftnlen)7);
	s_copy(mymnng + 6816, "Y*m*D**", (ftnlen)32, (ftnlen)7);
	s_copy(myknwn + 6848, "i-i-iti:ix", (ftnlen)32, (ftnlen)10);
	s_copy(mymnng + 6848, "Y*m*D*H*M*", (ftnlen)32, (ftnlen)10);
	s_copy(myknwn + 6880, "i-i-iti:i:ix", (ftnlen)32, (ftnlen)12);
	s_copy(mymnng + 6880, "Y*m*D*H*M*S*", (ftnlen)32, (ftnlen)12);
	s_copy(myknwn + 6912, "i-i-iti:i:nx", (ftnlen)32, (ftnlen)12);
	s_copy(mymnng + 6912, "Y*m*D*H*M*S*", (ftnlen)32, (ftnlen)12);
	s_copy(myknwn + 6944, "i-i-iti:nx", (ftnlen)32, (ftnlen)10);
	s_copy(mymnng + 6944, "Y*m*D*H*M*", (ftnlen)32, (ftnlen)10);
	s_copy(myknwn + 6976, "i-itx", (ftnlen)32, (ftnlen)5);
	s_copy(mymnng + 6976, "Y*y**", (ftnlen)32, (ftnlen)5);
	s_copy(myknwn + 7008, "i-iti:ix", (ftnlen)32, (ftnlen)8);
	s_copy(mymnng + 7008, "Y*y*H*M*", (ftnlen)32, (ftnlen)8);
	s_copy(myknwn + 7040, "i-iti:i:ix", (ftnlen)32, (ftnlen)10);
	s_copy(mymnng + 7040, "Y*y*H*M*S*", (ftnlen)32, (ftnlen)10);
	s_copy(myknwn + 7072, "i-iti:i:nx", (ftnlen)32, (ftnlen)10);
	s_copy(mymnng + 7072, "Y*y*H*M*S*", (ftnlen)32, (ftnlen)10);
	s_copy(myknwn + 7104, "i-iti:nx", (ftnlen)32, (ftnlen)8);
	s_copy(mymnng + 7104, "Y*y*H*M*", (ftnlen)32, (ftnlen)8);
	s_copy(myknwn + 7136, "Y-i-itix", (ftnlen)32, (ftnlen)8);
	s_copy(mymnng + 7136, "Y*m*D*H*", (ftnlen)32, (ftnlen)8);
	s_copy(myknwn + 7168, "Y-itix", (ftnlen)32, (ftnlen)6);
	s_copy(mymnng + 7168, "Y*y*H*", (ftnlen)32, (ftnlen)6);
	s_copy(myknwn + 7200, "Y-i-itnx", (ftnlen)32, (ftnlen)8);
	s_copy(mymnng + 7200, "Y*m*D*H*", (ftnlen)32, (ftnlen)8);
	s_copy(myknwn + 7232, "Y-itnx", (ftnlen)32, (ftnlen)6);
	s_copy(mymnng + 7232, "Y*y*H*", (ftnlen)32, (ftnlen)6);
	s_copy(myknwn + 7264, "i-i-itix", (ftnlen)32, (ftnlen)8);
	s_copy(mymnng + 7264, "Y*m*D*H*", (ftnlen)32, (ftnlen)8);
	s_copy(myknwn + 7296, "i-i-itnx", (ftnlen)32, (ftnlen)8);
	s_copy(mymnng + 7296, "Y*m*D*H*", (ftnlen)32, (ftnlen)8);
	s_copy(myknwn + 7328, "i-itix", (ftnlen)32, (ftnlen)6);
	s_copy(mymnng + 7328, "Y*y*H*", (ftnlen)32, (ftnlen)6);
	s_copy(myknwn + 7360, "i-itnx", (ftnlen)32, (ftnlen)6);
	s_copy(mymnng + 7360, "Y*y*H*", (ftnlen)32, (ftnlen)6);
    }

/*     Copy as many patterns and meanings as the input arrays allow. */

    *nknown = min(231,*room);
    i__1 = *nknown;
    for (i__ = 1; i__ <= i__1; ++i__) {
	s_copy(known + (i__ - 1) * known_len, myknwn + (((i__2 = i__ - 1) < 
		231 && 0 <= i__2 ? i__2 : s_rnge("myknwn", i__2, "zztpats_", (
		ftnlen)1126)) << 5), known_len, (ftnlen)32);
	s_copy(meanng + (i__ - 1) * meanng_len, mymnng + (((i__2 = i__ - 1) < 
		231 && 0 <= i__2 ? i__2 : s_rnge("mymnng", i__2, "zztpats_", (
		ftnlen)1127)) << 5), meanng_len, (ftnlen)32);
    }

/*     Make sure everything is in the proper order. */

    orderc_(known, nknown, ordvec, known_len);
    reordc_(ordvec, nknown, known, known_len);
    reordc_(ordvec, nknown, meanng, meanng_len);

/*     If there wasn't sufficient room to get all of the patterns */
/*     and meanings, return FALSE. */

    if (231 > *room) {
	ret_val = FALSE_;
	return ret_val;
    }
    ret_val = TRUE_;
    return ret_val;
} /* zztpats_ */

