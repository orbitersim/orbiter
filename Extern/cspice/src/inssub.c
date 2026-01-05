/* inssub.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure INSSUB ( Insert a substring ) */
/* Subroutine */ int inssub_(char *in, char *sub, integer *loc, char *out, 
	ftnlen in_len, ftnlen sub_len, ftnlen out_len)
{
    /* System generated locals */
    integer i__1, i__2;

    /* Builtin functions */
    integer i_len(char *, ftnlen), s_cmp(char *, char *, ftnlen, ftnlen);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    logical same;
    integer from, i__;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    integer inlen, nmove, to, subend, sublen;
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), setmsg_(char *, ftnlen), errint_(char *, integer *, 
	    ftnlen);
    integer outlen;
    char chr[1];

/* $ Abstract */

/*     Insert a substring into a character string at a specified */
/*     location. */

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

/*     ASSIGNMENT */
/*     CHARACTER */
/*     STRING */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     IN         I   Input string. */
/*     SUB        I   Substring to be inserted. */
/*     LOC        I   Position at which substring is to be inserted. */
/*     OUT        O   Output string. */

/* $ Detailed_Input */

/*     IN       is an input character string, into which a substring */
/*              is to be inserted. */

/*     SUB      is the substring to be inserted. Leading and trailing */
/*              blanks are significant. */

/*     LOC      is the index in the input string at which the substring */
/*              is to be inserted. The range of LOC is 1:LEN(IN). To */
/*              append to the string, set LOC equal to LEN(IN)+1. */

/* $ Detailed_Output */

/*     OUT      is the output string. This is equivalent to the */
/*              string that would be created by the concatenation */

/*                 OUT = IN(1:LOC-1) // SUB // IN(LOC: ) */

/*              If the output string is too long, it is truncated */
/*              on the right. */

/*              OUT may overwrite IN. OUT may NOT overwrite SUB. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If LOC is not in the interval [1, LEN(IN)+1], the error */
/*         SPICE(INVALIDINDEX) is signaled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     Shift the end of the input string, beginning with LOC, to the */
/*     right, leaving space for the substring. Then insert the substring */
/*     into the vacated space in the middle of the string. This has */
/*     the same effect as the concatenation */

/*        OUT = IN(1:LOC-1) // SUB // IN(LOC: ) */

/*     Because this operation is not standard for strings of length (*), */
/*     this routine does not use concatenation. */

/* $ Examples */

/*     The numerical results shown for these examples may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */

/*     1) The following table illustrates the use of INSSUB. */

/*           IN                SUB      LOC    OUT */
/*           ----------------- -------  ---    ------------------------ */
/*           'ABCDEFGHIJ'      ' YXZ '    3    'AB XYZ CDEFGHIJ' */
/*           'The rabbit'      'best '    5    'The best rabbit' */
/*           ' other woman'    'The'      1    'The other woman' */
/*           'An Apple a day'  ' keeps'  15    'An Apple a day keeps' */
/*           'Apple a day'     'An '      0     An error is signaled. */
/*           'Apple a day'     'An '     -3     An error is signaled. */
/*           'An Apple a day'  ' keeps'  16     An error is signaled. */


/*     2) INSSUB could be used to insert substrings at any position */
/*        within the declared length of the input string. Use it to */
/*        add an ascii arrow (--->) at the end of the string. */


/*        Example code begins here. */


/*              PROGRAM INSSUB_EX2 */
/*              IMPLICIT NONE */

/*        C */
/*        C     Local parameters. */
/*        C */
/*              INTEGER               STRSZ */
/*              PARAMETER           ( STRSZ = 20 ) */

/*        C */
/*        C     Local variables. */
/*        C */
/*              CHARACTER*(STRSZ)     STRIN */
/*              CHARACTER*(STRSZ)     STROUT */

/*        C */
/*        C     Assign the input string. */
/*        C */
/*              DATA                  STRIN / '0123456789         .' / */


/*        C */
/*        C     Insert a substring at index 17. This should leave 5 */
/*        C     blanks after the sequence of numbers. Note that the */
/*        C     resulting string is truncated. */
/*        C */
/*              CALL INSSUB ( STRIN, '--->', 17, STROUT ) */

/*              WRITE(*,*) 'Input string:  ', STRIN */
/*              WRITE(*,*) 'Output string: ', STROUT */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*         Input string:  0123456789         . */
/*         Output string: 0123456789      ---> */


/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     H.A. Neilan        (JPL) */
/*     W.L. Taber         (JPL) */
/*     I.M. Underwood     (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.2.0, 01-OCT-2021 (JDR) (NJB) */

/*        Added IMPLICIT NONE statement. */

/*        Updated long error message for LOC out of range check, */
/*        providing valid range of values. */

/*        Edited the header to comply with NAIF standard. Added complete */
/*        code example. */

/* -    SPICELIB Version 1.1.0, 24-OCT-1994 (NJB) */

/*        Bug fixes made. Now does discovery check-in. Header sections */
/*        re-arranged. Some clean-up of header format done. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU) (HAN) */

/* -& */
/* $ Index_Entries */

/*     insert a substring */

/* -& */
/* $ Revisions */

/* -    SPICELIB Version 1.1.0, 24-OCT-1994 (NJB) */

/*        Bug fix: case where insertion location follows end of */
/*        input string is now handled correctly. Formerly, an */
/*        out-of-range substring bound violation was incurred in this */
/*        case. */

/*        Bug fix: use of SHIFTC routine in old implementation */
/*        resulted in output string being truncated at length */
/*        LEN(IN), which is not consistent with the routine's */
/*        specification. */

/*        Now does discovery check-in. Header sections re-arranged. */
/*        Some clean-up of header format done. */

/* -    Beta Version 2.0.0, 04-JAN-1989 (HAN) */

/*        If the location at which the substring is to be inserted is */
/*        not in the interval [1, LEN(IN)+1], an error is signaled. */
/*        Locations not within that interval refer to non-existent */
/*        characters positions. (To append to the string, set the */
/*        location equal to LEN(IN)+1.) */

/* -& */

/*     Local Variables */


/*     Discovery check-in is used in this routine. */

/*     Note to the careful reader:  in order to scrupulously avoid */
/*     non-standard assignments of characters from a substring of IN to */
/*     an overlapping substring of OUT, in the case where IN and OUT */
/*     refer to the same memory, we'll test whether the output and */
/*     input strings are the same.  If they're the same, we can avoid */
/*     various assignments that could cause trouble if IN and OUT */
/*     actually refer to the same memory.  This test has little effect on */
/*     performance, and allows the author to sleep more soundly at night. */

/*     Capture the lengths of the input, output, and substitution */
/*     strings. */

    inlen = i_len(in, in_len);
    outlen = i_len(out, out_len);
    sublen = i_len(sub, sub_len);

/*     If insertion occurs before the beginning of the string */
/*     or after INLEN + 1, signal an error. */

    if (*loc < 1 || *loc > inlen + 1) {
	chkin_("INSSUB", (ftnlen)6);
	setmsg_("Index at which the substring is to be inserted is out of th"
		"e valid range [1,#]. Requested index was *.", (ftnlen)102);
	i__1 = inlen + 1;
	errint_("#", &i__1, (ftnlen)1);
	errint_("*", loc, (ftnlen)1);
	sigerr_("SPICE(INVALIDINDEX)", (ftnlen)19);
	chkout_("INSSUB", (ftnlen)6);
	return 0;
    }

/*     If the insertion occurs after the end of the output string, */
/*     just return the original string.  Don't do the assignment if */
/*     the output and input strings have equal values; the assignment */
/*     is not needed in this cause and could cause a run-time error if */
/*     OUT and IN refer to the same memory. */

    same = s_cmp(out, in, out_len, in_len) == 0;
    if (*loc > outlen) {
	if (! same) {
	    s_copy(out, in, out_len, in_len);
	}
	return 0;
    }

/*     At this point, we're guaranteed that */

/*        LOC  <  OUTLEN */
/*             - */

/*        LOC  <  INLEN + 1 */
/*             - */

/*        LOC  >  0 */


/*     The first part of the input string is copied without change */
/*     to the output string, if this first part is non-empty. */

    if (*loc > 1) {

/*        Again, do the assignment only if it's required. */

	if (! same) {
	    s_copy(out, in, *loc - 1, in_len);
	}
    }

/*     The part following the new substring is shifted into place, if */
/*     there's both something to move and a place to put it.  Move the */
/*     rightmost characters first. */

    subend = *loc - 1 + sublen;
    if (*loc <= inlen && subend < outlen) {
/* Computing MIN */
	i__1 = outlen - subend, i__2 = inlen - *loc + 1;
	nmove = min(i__1,i__2);
	for (i__ = nmove; i__ >= 1; --i__) {
	    from = *loc + i__ - 1;
	    to = subend + i__;
	    *(unsigned char *)chr = *(unsigned char *)&in[from - 1];
	    *(unsigned char *)&out[to - 1] = *(unsigned char *)chr;
	}
    }

/*     And the new word is dropped into the middle. */

    s_copy(out + (*loc - 1), sub, min(subend,outlen) - (*loc - 1), sub_len);

/*     Blank-pad the output string if necessary. */

    if (outlen > inlen + sublen) {
	i__1 = inlen + sublen;
	s_copy(out + i__1, " ", out_len - i__1, (ftnlen)1);
    }
    return 0;
} /* inssub_ */

