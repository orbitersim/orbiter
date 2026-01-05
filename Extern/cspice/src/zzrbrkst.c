/* zzrbrkst.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure ZZRBRKST ( Private --- Reverse Bracketed String Extractor ) */
/* Subroutine */ int zzrbrkst_(char *string, char *lftend, char *rgtend, char 
	*substr, integer *length, logical *bkpres, ftnlen string_len, ftnlen 
	lftend_len, ftnlen rgtend_len, ftnlen substr_len)
{
    /* System generated locals */
    integer i__1;

    /* Builtin functions */
    integer i_len(char *, ftnlen);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    extern integer posr_(char *, char *, integer *, ftnlen, ftnlen);
    integer bsize, lsize, rsize, lindex, rindex;

/* $ Abstract */

/*    Extract from a string the last instance of a substring bracketed */
/*    by specified left and right strings . */

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

/*     STRINGS */
/*     UTILITY */
/*     SCANNING */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     STRING     I   A string from which to extract SUBSTR. */
/*     LFTEND     I   A string that brackets SUBSTR on the left. */
/*     RGTEND     I   A string that brackets SUBSTR on the right. */
/*     SUBSTR     O   The extracted substring. */
/*     LENGTH     O   The length of the extracted substring. */
/*     BKPRES     O   Logical indicating if either bracket is present. */

/* $ Detailed_Input */

/*     STRING     is a string to be searched for a substring bracketed */
/*                by the strings LFTEND and RGTEND (see below). */

/*     LFTEND,    are respectively the left and right bracketing strings. */
/*     RGTEND     Trailing and leading white space is significant. LFTEND */
/*                may equal RGTEND.  See the Exceptions section for a */
/*                discussion of the case in which either of these strings */
/*                is absent. */

/* $ Detailed_Output */

/*     SUBSTR     is the substring of interest.  It consists of the */
/*                substring between the last instances of LFTEND */
/*                and RGTEND in STRING. Note: The argument passed into */
/*                the routine should be large enough to hold the entire */
/*                substring, or else truncation will occur. SUBSTR is */
/*                padded with trailing blanks. */

/*     LENGTH     is the number of characters placed into SUBSTR. This */
/*                value permits any significant trailing whitespace to be */
/*                dealt with appropriately. In the event that no */
/*                substring is assigned to SUBSTR, LENGTH will be 0. */

/*     BKPRES     is a logical that indicates whether or not at least */
/*                one of LFTEND or RGTEND is present in STRING. */

/* $ Parameters */

/*     None. */

/* $ Files */

/*     None. */

/* $ Exceptions */

/*     1) If LFTEND or RGTEND are not present in STRING, then the routine */
/*        does not modify the contents of SUBSTR, LENGTH is returned as */
/*        0, and BKPRES is TRUE only if LFTEND or RGTEND is present. */

/*     2) If LFTEND and RGTEND are adjacent, then SUBSTR is not modified, */
/*        LENGTH is returned as 0, and BKPRES is TRUE. */

/* $ Particulars */

/*     The purpose of this routine is to extract the last instance of */
/*     a substring bracketed by two specified strings.  The searching */
/*     is case sensitive, and all white space is significant.  The */
/*     characters between LFTEND and RGTEND are placed into SUBSTR, */
/*     and LENGTH is set to the number of characters copied into SUBSTR. */
/*     The assignment is not substring assignment, so the resultant */
/*     SUBSTR will be blank padded.  The logical BKPRES is a flag that */
/*     indicates whether or not either of the two brackets was found. */
/*     This is diagnostic information of some limited use in the event */
/*     that SUBSTR was not assigned a value. */

/* $ Examples */

/*     The following table demonstrates the behavior of this routine: */
/*     ( If a row in the table has no entry for SUBSTR, then the */
/*       contents of SUBSTR are not modified by calling the routine */
/*       with these inputs. ) */

/*     STRING               LFTEND   RGTEND   SUBSTR             LENGTH */
/*     ===================  =======  =======  =================  ====== */
/*     'abc def ghi jkl'    'a'      'l'      'bc def ghi jk'      13 */
/*     'abc def ghi jkl'    'abc'    'ghi'    ' def '              5 */
/*     'abc def ghi jkl'    'abc'    '123'                         0 */
/*     'abc def ghi jkl'    '123'    'def'                         0 */
/*     'abc def ghi jkl'    'jkl'    'zzz'                         0 */
/*     'abc def abc jkl'    'abc'    'abc'    ' def '              5 */
/*     'ab cd ab ef ab '    'ab'     'ab'     ' ef '               4 */
/*     'ab cd ab ef ab '    'ef'     'cd'                          0 */
/*     'abc def-fed abc'    'def'    '-fed'                        0 */
/*     'aaaaaaaaaaaaaaa'    'aa'     'aaaa'                        0 */
/*     'aaaabbbaabababa'    'ba'     'a'      'b'                  1 */
/*     'aaaabbbaababada'    'ba'     'a'      'd'                  1 */
/*     'abcd efgh ijkl '    ' '      'l'      'ijk'                3 */
/*     'abcd efgh ijkl '    '    '   'l'                           0 */
/*     'ab  ef   ijklm '    '  '     'm'      'ijkl'               4 */
/*     'ab   ef  ijklm '    '   '    'm'      'ef  ijkl'           8 */

/* $ Restrictions */

/*     1) The size of SUBSTR must be large enough to contain any */
/*        possible substring bracketed by LFTEND or RGTEND, otherwise */
/*        truncation will occur at assignment. */

/* $ Author_and_Institution */

/*     F.S. Turner     (JPL) */

/* $ Literature_References */

/*     None. */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 22-MAR-1999 (FST) */


/* -& */
/* $ Index_Entries */

/*     reverse bracketed string extraction */

/* -& */

/*     SPICELIB Functions */


/*     Local Variables */


/*     Compute the sizes of the bracketing substrings and the text */
/*     block. */

    lsize = i_len(lftend, lftend_len);
    rsize = i_len(rgtend, rgtend_len);
    bsize = i_len(string, string_len);

/*     Search from the right for RGTEND. */

    rindex = posr_(string, rgtend, &bsize, string_len, rgtend_len);

/*     Now continue the search from RINDEX to the right, this time */
/*     looking for LFTEND. If RINDEX comes back as 0, then the right */
/*     bracketing substring is not present, so search the entire string */
/*     for LFTEND. Otherwise, search from where the right bracket */
/*     search left off. */

    if (rindex == 0) {
	lindex = posr_(string, lftend, &bsize, string_len, lftend_len);
    } else {
	i__1 = rindex - lsize;
	lindex = posr_(string, lftend, &i__1, string_len, lftend_len);
    }

/*     Interpret the results.  If RINDEX and LINDEX are both non-zero, */
/*     then return the substring they bracket, otherwise handle the */
/*     failed case. */

    if (rindex != 0 && lindex != 0) {

/*        Check to see whether or not the brackets are adjacent, and */
/*        thus have no characters between them. */

	if (lindex + lsize > rindex - 1) {
	    *bkpres = TRUE_;
	    *length = 0;

/*        If they aren't adjacent, then compute the length and prepare */
/*        SUBSTR. */

	} else {
	    *length = rindex - (lindex + lsize);
	    *bkpres = TRUE_;
	    i__1 = lindex + lsize - 1;
	    s_copy(substr, string + i__1, substr_len, rindex - 1 - i__1);
	}
    } else {

/*        Set BKPRES to TRUE only if LINDEX or RINDEX is non-zero, */
/*        indicating one was found by POSR. Set LENGTH to 0, since we */
/*        will not be changing SUBSTR. */

	*bkpres = lindex + rindex > 0;
	*length = 0;
    }
    return 0;
} /* zzrbrkst_ */

