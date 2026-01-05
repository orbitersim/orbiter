/* zzekecmp.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure      ZZEKECMP ( EK, column entry element comparison ) */
integer zzekecmp_(integer *hans, integer *sgdscs, integer *cldscs, integer *
	rows, integer *elts)
{
    /* System generated locals */
    integer ret_val, i__1, i__2, i__3;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer);
    logical l_lt(char *, char *, ftnlen, ftnlen), l_gt(char *, char *, ftnlen,
	     ftnlen);

    /* Local variables */
    char cval[1024*2];
    doublereal dval[2];
    integer ival[2];
    logical null[2];
    integer i__;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    integer cvlen[2];
    logical found;
    integer cmplen[2], lhstyp, rhstyp;
    extern /* Subroutine */ int setmsg_(char *, ftnlen), errhan_(char *, 
	    integer *, ftnlen), errint_(char *, integer *, ftnlen), sigerr_(
	    char *, ftnlen), chkout_(char *, ftnlen), zzekrsc_(integer *, 
	    integer *, integer *, integer *, integer *, integer *, char *, 
	    logical *, logical *, ftnlen), zzekrsd_(integer *, integer *, 
	    integer *, integer *, integer *, doublereal *, logical *, logical 
	    *), zzekrsi_(integer *, integer *, integer *, integer *, integer *
	    , integer *, logical *, logical *);

/* $ Abstract */

/*     Compare two column entry elements, and return the relation of the */
/*     first to the second:  LT, EQ, or GT. */

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

/*     EK */

/* $ Keywords */

/*     COMPARE */
/*     EK */
/*     UTILITY */

/* $ Declarations */
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


/*     Include Section:  EK Column Descriptor Parameters */

/*        ekcoldsc.inc Version 6    23-AUG-1995 (NJB) */


/*     Note:  The column descriptor size parameter CDSCSZ  is */
/*     declared separately in the include section CDSIZE$INC.FOR. */

/*     Offset of column descriptors, relative to start of segment */
/*     integer address range.  This number, when added to the last */
/*     integer address preceding the segment, yields the DAS integer */
/*     base address of the first column descriptor.  Currently, this */
/*     offset is exactly the size of a segment descriptor.  The */
/*     parameter SDSCSZ, which defines the size of a segment descriptor, */
/*     is declared in the include file eksegdsc.inc. */


/*     Size of column descriptor */


/*     Indices of various pieces of column descriptors: */


/*     CLSIDX is the index of the column's class code.  (We use the */
/*     word `class' to distinguish this item from the column's data */
/*     type.) */


/*     TYPIDX is the index of the column's data type code (CHR, INT, DP, */
/*     or TIME).  The type is actually implied by the class, but it */
/*     will frequently be convenient to look up the type directly. */



/*     LENIDX is the index of the column's string length value, if the */
/*     column has character type.  A value of IFALSE in this element of */
/*     the descriptor indicates that the strings have variable length. */


/*     SIZIDX is the index of the column's element size value.  This */
/*     descriptor element is meaningful for columns with fixed-size */
/*     entries.  For variable-sized columns, this value is IFALSE. */


/*     NAMIDX is the index of the base address of the column's name. */


/*     IXTIDX is the data type of the column's index.  IXTIDX */
/*     contains a type value only if the column is indexed. For columns */
/*     that are not indexed, the location IXTIDX contains the boolean */
/*     value IFALSE. */


/*     IXPIDX is a pointer to the column's index.  IXTPDX contains a */
/*     meaningful value only if the column is indexed.  The */
/*     interpretation of the pointer depends on the data type of the */
/*     index. */


/*     NFLIDX is the index of a flag indicating whether nulls are */
/*     permitted in the column.  The value at location NFLIDX is */
/*     ITRUE if nulls are permitted and IFALSE otherwise. */


/*     ORDIDX is the index of the column's ordinal position in the */
/*     list of columns belonging to the column's parent segment. */


/*     METIDX is the index of the column's integer metadata pointer. */
/*     This pointer is a DAS integer address. */


/*     The last position in the column descriptor is reserved.  No */
/*     parameter is defined to point to this location. */


/*     End Include Section:  EK Column Descriptor Parameters */

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


/*     Include Section:  EK Operator Codes */

/*        ekopcd.inc  Version 1  30-DEC-1994 (NJB) */


/*     Within the EK system, operators used in EK queries are */
/*     represented by integer codes.  The codes and their meanings are */
/*     listed below. */

/*     Relational expressions in EK queries have the form */

/*        <column name> <operator> <value> */

/*     For columns containing numeric values, the operators */

/*        EQ,  GE,  GT,  LE,  LT,  NE */

/*     may be used; these operators have the same meanings as their */
/*     Fortran counterparts.  For columns containing character values, */
/*     the list of allowed operators includes those in the above list, */
/*     and in addition includes the operators */

/*        LIKE,  UNLIKE */

/*     which are used to compare strings to a template.  In the character */
/*     case, the meanings of the parameters */

/*        GE,  GT,  LE,  LT */

/*     match those of the Fortran lexical functions */

/*        LGE, LGT, LLE, LLT */


/*     The additional unary operators */

/*        ISNULL, NOTNUL */

/*     are used to test whether a value of any type is null. */



/*     End Include Section:  EK Operator Codes */

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


/*     Include Section:  EK Query Limit Parameters */

/*        ekqlimit.inc  Version 3    16-NOV-1995 (NJB) */

/*           Parameter MAXCON increased to 1000. */

/*        ekqlimit.inc  Version 2    01-AUG-1995 (NJB) */

/*           Updated to support SELECT clause. */


/*        ekqlimit.inc  Version 1    07-FEB-1995 (NJB) */


/*     These limits apply to character string queries input to the */
/*     EK scanner.  This limits are part of the EK system's user */
/*     interface:  the values should be advertised in the EK required */
/*     reading document. */


/*     Maximum length of an input query:  MAXQRY.  This value is */
/*     currently set to twenty-five 80-character lines. */


/*     Maximum number of columns that may be listed in the */
/*     `order-by clause' of a query:  MAXSEL.  MAXSEL = 50. */


/*     Maximum number of tables that may be listed in the `FROM */
/*     clause' of a query: MAXTAB. */


/*     Maximum number of relational expressions that may be listed */
/*     in the `constraint clause' of a query: MAXCON. */

/*     This limit applies to a query when it is represented in */
/*     `normalized form': that is, the constraints have been */
/*     expressed as a disjunction of conjunctions of relational */
/*     expressions. The number of relational expressions in a query */
/*     that has been expanded in this fashion may be greater than */
/*     the number of relations in the query as orginally written. */
/*     For example, the expression */

/*             ( ( A LT 1 ) OR ( B GT 2 ) ) */
/*        AND */
/*             ( ( C NE 3 ) OR ( D EQ 4 ) ) */

/*     which contains 4 relational expressions, expands to the */
/*     equivalent normalized constraint */

/*             (  ( A LT 1 ) AND ( C NE 3 )  ) */
/*        OR */
/*             (  ( A LT 1 ) AND ( D EQ 4 )  ) */
/*        OR */
/*             (  ( B GT 2 ) AND ( C NE 3 )  ) */
/*        OR */
/*             (  ( B GT 2 ) AND ( D EQ 4 )  ) */

/*     which contains eight relational expressions. */



/*     MXJOIN is the maximum number of tables that can be joined. */


/*     MXJCON is the maximum number of join constraints allowed. */


/*     Maximum number of order-by columns that may be used in the */
/*     `order-by clause' of a query: MAXORD. MAXORD = 10. */


/*     Maximum number of tokens in a query: 500. Tokens are reserved */
/*     words, column names, parentheses, and values. Literal strings */
/*     and time values count as single tokens. */


/*     Maximum number of numeric tokens in a query: */


/*     Maximum total length of character tokens in a query: */


/*     Maximum length of literal string values allowed in queries: */
/*     MAXSTR. */


/*     End Include Section:  EK Query Limit Parameters */

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


/*     Include Section:  EK Segment Descriptor Parameters */

/*        eksegdsc.inc  Version 8  06-NOV-1995 (NJB) */


/*     All `base addresses' referred to below are the addresses */
/*     *preceding* the item the base applies to.  This convention */
/*     enables simplied address calculations in many cases. */

/*     Size of segment descriptor.  Note:  the include file ekcoldsc.inc */
/*     must be updated if this parameter is changed.  The parameter */
/*     CDOFF in that file should be kept equal to SDSCSZ. */


/*     Index of the segment type code: */


/*     Index of the segment's number.  This number is the segment's */
/*     index in the list of segments contained in the EK to which */
/*     the segment belongs. */


/*     Index of the DAS integer base address of the segment's integer */
/*     meta-data: */


/*     Index of the DAS character base address of the table name: */


/*     Index of the segment's column count: */


/*     Index of the segment's record count: */


/*     Index of the root page number of the record tree: */


/*     Index of the root page number of the character data page tree: */


/*     Index of the root page number of the double precision data page */
/*     tree: */


/*     Index of the root page number of the integer data page tree: */


/*     Index of the `modified' flag: */


/*     Index of the `initialized' flag: */


/*     Index of the shadowing flag: */


/*     Index of the companion file handle: */


/*     Index of the companion segment number: */


/*     The next three items are, respectively, the page numbers of the */
/*     last character, d.p., and integer data pages allocated by the */
/*     segment: */


/*     The next three items are, respectively, the page-relative */
/*     indices of the last DAS word in use in the segment's */
/*     last character, d.p., and integer data pages: */


/*     Index of the DAS character base address of the column name list: */


/*     The last descriptor element is reserved for future use.  No */
/*     parameter is defined to point to this location. */


/*     End Include Section:  EK Segment Descriptor Parameters */

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


/*     Include Section:  EK Data Types */

/*        ektype.inc Version 1  27-DEC-1994 (NJB) */


/*     Within the EK system, data types of EK column contents are */
/*     represented by integer codes.  The codes and their meanings */
/*     are listed below. */

/*     Integer codes are also used within the DAS system to indicate */
/*     data types; the EK system makes no assumptions about compatibility */
/*     between the codes used here and those used in the DAS system. */


/*     Character type: */


/*     Double precision type: */


/*     Integer type: */


/*     `Time' type: */

/*     Within the EK system, time values are represented as ephemeris */
/*     seconds past J2000 (TDB), and double precision numbers are used */
/*     to store these values.  However, since time values require special */
/*     treatment both on input and output, and since the `TIME' column */
/*     has a special role in the EK specification and code, time values */
/*     are identified as a type distinct from double precision numbers. */


/*     End Include Section:  EK Data Types */

/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     HANS       I   EK handles. */
/*     SGDSCS     I   Segment descriptors. */
/*     CLDSCS     I   Column descriptors. */
/*     ROWS       I   Row numbers. */
/*     ELTS       I   Element indices. */

/*     The function returns a parameter indicating the order relation */
/*     satisfied by the input arguments.  Possible values are LT, EQ, */
/*     and GT. */

/* $ Detailed_Input */

/*     HANS           is an array containing file handles of two EKs */
/*                    containing column entry elements to be compared. */

/*     SGDSCS         is an array containing segment descriptors of */
/*                    the segments that contain the elements to be */
/*                    compared. */

/*     CLDSCS         is an array containing column descriptors for the */
/*                    columns containing the elements to be compared. */

/*     ROWS           is an array containing row numbers of the */
/*                    elements to be compared. */

/*     ELTS           is an array containing element indices of the */
/*                    elements to be compared.  These indices locate */
/*                    an element within the column entry it belongs to. */

/* $ Detailed_Output */

/*     The function returns a parameter indicating the order relation */
/*     satisfied by the input arguments.  Possible values are LT, EQ, */
/*     and GT.  If OP is the returned value, the scalar values */
/*     specified by the input arguments satisfy the relation */

/*        <row 1> OP <row 2> */

/* $ Parameters */

/*     See the include file ekopcd.inc. */

/* $ Exceptions */

/*     1)  If the either of input file handles is invalid, the error */
/*         will be diagnosed by routines called by this routine. */
/*         The function value is EQ in this case. */

/*     2)  If an I/O error occurs while attempting to look up */
/*         the specified column entry elements, the error will */
/*         be diagnosed by routines called by this routine.  The */
/*         function value is EQ in this case. */

/*     3)  If any of the input segment descriptors, column descriptors, */
/*         or row numbers are invalid, this routine may fail in */
/*         unpredictable, but possibly spectacular, ways.  Except */
/*         as described in this header section, no attempt is made to */
/*         handle these errors. */

/*     4)  If the data type code in the input column descriptor is not */
/*         recognized, the error SPICE(INVALIDDATATYPE) is signaled. */
/*         The function value is EQ in this case. */

/* $ Files */

/*     See the descriptions of the arguments HAN(1) and HAN(2) in */
/*     $Detailed_Input. */

/* $ Particulars */

/*     This routine is an EK utility intended to centralize a frequently */
/*     performed comparison operation. */

/* $ Examples */

/*     See ZZEKRCMP, ZZEKVCMP, ZZEKVMCH. */

/* $ Restrictions */

/*     1)  This routine must execute quickly.  Therefore, it checks in */
/*         only if it detects an error.  If an error is signaled by a */
/*         routine called by this routine, this routine will not appear */
/*         in the SPICELIB traceback display.  Also, in the interest */
/*         of speed, this routine does not test the value of the SPICELIB */
/*         function RETURN upon entry. */

/*     2)  This routine depends on the requested comparison to have */
/*         been semantically checked. Semantically invalid comparisons */
/*         are treated as bugs. */

/*     3)  Only the first MAXSTR characters of character strings are */
/*         used in comparisons. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman   (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.2.0, 07-FEB-2015 (NJB) */

/*        Now uses ERRHAN to insert DAS file name into */
/*        long error messages. */

/* -    SPICELIB Version 1.1.0, 26-MAY-2010 (NJB) */

/*        Bug fix: subscript out of range error caused by */
/*        column entry strings longer than MAXLEN has been */
/*        corrected. Also updated Restrictions header section. */

/* -    Beta Version 1.0.0, 10-OCT-1995 (NJB) */

/* -& */

/*     Local variables */


/*     Use discovery check-in for speed. */


/*     The function value defaults to `equal'. */

    ret_val = 1;
    lhstyp = cldscs[1];
    rhstyp = cldscs[12];
    if (lhstyp == 3) {

/*        The entities we're comparing are supposed to be */
/*        scalar.  The left hand side has integer type.  Either */
/*        integer or double precision types are acceptable on */
/*        the right hand side. */

	zzekrsi_(hans, sgdscs, cldscs, rows, elts, ival, null, &found);
	if (! found) {
	    chkin_("ZZEKECMP", (ftnlen)8);
	    setmsg_("EK = #; COLIDX = #; ROW = #; ELTIDX = #. Column entry e"
		    "lement was not found.", (ftnlen)76);
	    errhan_("#", hans, (ftnlen)1);
	    errint_("#", &cldscs[8], (ftnlen)1);
	    errint_("#", rows, (ftnlen)1);
	    errint_("#", elts, (ftnlen)1);
	    sigerr_("SPICE(INVALIDINDEX)", (ftnlen)19);
	    chkout_("ZZEKECMP", (ftnlen)8);
	    return ret_val;
	}
	if (rhstyp == 3) {
	    zzekrsi_(&hans[1], &sgdscs[24], &cldscs[11], &rows[1], elts, &
		    ival[1], &null[1], &found);
	    if (! found) {
		chkin_("ZZEKECMP", (ftnlen)8);
		setmsg_("EK = #; COLIDX = #; ROW = #; ELTIDX  = #.Column ent"
			"ry element was not found.", (ftnlen)76);
		errhan_("#", &hans[1], (ftnlen)1);
		errint_("#", &cldscs[19], (ftnlen)1);
		errint_("#", &rows[1], (ftnlen)1);
		errint_("#", &elts[1], (ftnlen)1);
		sigerr_("SPICE(INVALIDINDEX)", (ftnlen)19);
		chkout_("ZZEKECMP", (ftnlen)8);
		return ret_val;
	    }

/*           Null values precede all others. */

	    if (null[0] || null[1]) {
		if (! null[1]) {
		    ret_val = 5;
		} else if (! null[0]) {
		    ret_val = 3;
		}
	    } else {
		if (ival[0] < ival[1]) {
		    ret_val = 5;
		} else if (ival[0] > ival[1]) {
		    ret_val = 3;
		}
	    }
	} else if (rhstyp == 2) {
	    zzekrsd_(&hans[1], &sgdscs[24], &cldscs[11], &rows[1], elts, &
		    dval[1], &null[1], &found);
	    if (! found) {
		chkin_("ZZEKECMP", (ftnlen)8);
		setmsg_("EK = #; COLIDX = #; ROW = #; ELTIDX  = #.Column ent"
			"ry element was not found.", (ftnlen)76);
		errhan_("#", &hans[1], (ftnlen)1);
		errint_("#", &cldscs[19], (ftnlen)1);
		errint_("#", &rows[1], (ftnlen)1);
		errint_("#", &elts[1], (ftnlen)1);
		sigerr_("SPICE(INVALIDINDEX)", (ftnlen)19);
		chkout_("ZZEKECMP", (ftnlen)8);
		return ret_val;
	    }
	    if (null[0] || null[1]) {
		if (! null[1]) {
		    ret_val = 5;
		} else if (! null[0]) {
		    ret_val = 3;
		}
	    } else {
		if ((doublereal) ival[0] < dval[1]) {
		    ret_val = 5;
		} else if ((doublereal) ival[0] > dval[1]) {
		    ret_val = 3;
		}
	    }
	} else {

/*           This is a big-time semantic error.  We should */
/*           never arrive here. */

	    chkin_("ZZEKECMP", (ftnlen)8);
	    setmsg_("LHS data type is #; RHSTYP is #.", (ftnlen)32);
	    errint_("#", &lhstyp, (ftnlen)1);
	    errint_("#", &rhstyp, (ftnlen)1);
	    sigerr_("SPICE(BUG)", (ftnlen)10);
	    chkout_("ZZEKECMP", (ftnlen)8);
	    return ret_val;
	}
    } else if (lhstyp == 2) {

/*        This is a mirror image of the INT case. */

	zzekrsd_(hans, sgdscs, cldscs, rows, elts, dval, null, &found);
	if (! found) {
	    chkin_("ZZEKECMP", (ftnlen)8);
	    setmsg_("EK = #; COLIDX = #; ROW = #; ELTIDX = #. Column entry e"
		    "lement was not found.", (ftnlen)76);
	    errhan_("#", hans, (ftnlen)1);
	    errint_("#", &cldscs[8], (ftnlen)1);
	    errint_("#", rows, (ftnlen)1);
	    errint_("#", elts, (ftnlen)1);
	    sigerr_("SPICE(INVALIDINDEX)", (ftnlen)19);
	    chkout_("ZZEKECMP", (ftnlen)8);
	    return ret_val;
	}
	if (rhstyp == 3) {
	    zzekrsi_(&hans[1], &sgdscs[24], &cldscs[11], &rows[1], elts, &
		    ival[1], &null[1], &found);
	    if (! found) {
		chkin_("ZZEKECMP", (ftnlen)8);
		setmsg_("EK = #; COLIDX = #; ROW = #; ELTIDX  = #.Column ent"
			"ry element was not found.", (ftnlen)76);
		errhan_("#", &hans[1], (ftnlen)1);
		errint_("#", &cldscs[19], (ftnlen)1);
		errint_("#", &rows[1], (ftnlen)1);
		errint_("#", &elts[1], (ftnlen)1);
		sigerr_("SPICE(INVALIDINDEX)", (ftnlen)19);
		chkout_("ZZEKECMP", (ftnlen)8);
		return ret_val;
	    }

/*           Null values precede all others. */

	    if (null[0] || null[1]) {
		if (! null[1]) {
		    ret_val = 5;
		} else if (! null[0]) {
		    ret_val = 3;
		}
	    } else {
		if (dval[0] < (doublereal) ival[1]) {
		    ret_val = 5;
		} else if (dval[0] > (doublereal) ival[1]) {
		    ret_val = 3;
		}
	    }
	} else if (rhstyp == 2) {
	    zzekrsd_(&hans[1], &sgdscs[24], &cldscs[11], &rows[1], elts, &
		    dval[1], &null[1], &found);
	    if (! found) {
		chkin_("ZZEKECMP", (ftnlen)8);
		setmsg_("EK = #; COLIDX = #; ROW = #; ELTIDX  = #.Column ent"
			"ry element was not found.", (ftnlen)76);
		errhan_("#", &hans[1], (ftnlen)1);
		errint_("#", &cldscs[19], (ftnlen)1);
		errint_("#", &rows[1], (ftnlen)1);
		errint_("#", &elts[1], (ftnlen)1);
		sigerr_("SPICE(INVALIDINDEX)", (ftnlen)19);
		chkout_("ZZEKECMP", (ftnlen)8);
		return ret_val;
	    }
	    if (null[0] || null[1]) {
		if (! null[1]) {
		    ret_val = 5;
		} else if (! null[0]) {
		    ret_val = 3;
		}
	    } else {
		if (dval[0] < dval[1]) {
		    ret_val = 5;
		} else if (dval[0] > dval[1]) {
		    ret_val = 3;
		}
	    }
	} else {

/*           This is a big-time semantic error.  We should */
/*           never arrive here. */

	    chkin_("ZZEKECMP", (ftnlen)8);
	    setmsg_("LHS data type is #; RHSTYP is #.", (ftnlen)32);
	    errint_("#", &lhstyp, (ftnlen)1);
	    errint_("#", &rhstyp, (ftnlen)1);
	    sigerr_("SPICE(BUG)", (ftnlen)10);
	    chkout_("ZZEKECMP", (ftnlen)8);
	    return ret_val;
	}
    } else if (lhstyp == 4) {

/*        The entities we're comparing are supposed to be time values. */

	if (rhstyp != 4) {

/*           This is a big-time semantic error.  We should */
/*           never arrive here. */

	    chkin_("ZZEKECMP", (ftnlen)8);
	    setmsg_("LHS data type is #; RHSTYP is #.", (ftnlen)32);
	    errint_("#", &lhstyp, (ftnlen)1);
	    errint_("#", &rhstyp, (ftnlen)1);
	    sigerr_("SPICE(BUG)", (ftnlen)10);
	    chkout_("ZZEKECMP", (ftnlen)8);
	    return ret_val;
	}
	for (i__ = 1; i__ <= 2; ++i__) {
	    zzekrsd_(&hans[i__ - 1], &sgdscs[i__ * 24 - 24], &cldscs[i__ * 11 
		    - 11], &rows[i__ - 1], &elts[i__ - 1], &dval[(i__1 = i__ 
		    - 1) < 2 && 0 <= i__1 ? i__1 : s_rnge("dval", i__1, "zze"
		    "kecmp_", (ftnlen)486)], &null[(i__2 = i__ - 1) < 2 && 0 <=
		     i__2 ? i__2 : s_rnge("null", i__2, "zzekecmp_", (ftnlen)
		    486)], &found);
	    if (! found) {
		chkin_("ZZEKECMP", (ftnlen)8);
		setmsg_("EK = #; COLIDX = #; ROW = #; ELTIDX  = #.Column ent"
			"ry element was not found.", (ftnlen)76);
		errhan_("#", &hans[i__ - 1], (ftnlen)1);
		errint_("#", &cldscs[i__ * 11 - 3], (ftnlen)1);
		errint_("#", &rows[i__ - 1], (ftnlen)1);
		errint_("#", &elts[i__ - 1], (ftnlen)1);
		sigerr_("SPICE(INVALIDINDEX)", (ftnlen)19);
		chkout_("ZZEKECMP", (ftnlen)8);
		return ret_val;
	    }
	}
	if (null[0] || null[1]) {
	    if (! null[1]) {
		ret_val = 5;
	    } else if (! null[0]) {
		ret_val = 3;
	    }
	} else {
	    if (dval[0] < dval[1]) {
		ret_val = 5;
	    } else if (dval[0] > dval[1]) {
		ret_val = 3;
	    }
	}
    } else if (lhstyp == 1) {

/*        The entities we're comparing are supposed to be scalar. */

	if (rhstyp != 1) {

/*           You know what kind of semantic error this is. */

	    chkin_("ZZEKECMP", (ftnlen)8);
	    setmsg_("LHS data type is #; RHSTYP is #.", (ftnlen)32);
	    errint_("#", &lhstyp, (ftnlen)1);
	    errint_("#", &rhstyp, (ftnlen)1);
	    sigerr_("SPICE(BUG)", (ftnlen)10);
	    chkout_("ZZEKECMP", (ftnlen)8);
	    return ret_val;
	}
	for (i__ = 1; i__ <= 2; ++i__) {
	    zzekrsc_(&hans[i__ - 1], &sgdscs[i__ * 24 - 24], &cldscs[i__ * 11 
		    - 11], &rows[i__ - 1], &elts[i__ - 1], &cvlen[(i__1 = i__ 
		    - 1) < 2 && 0 <= i__1 ? i__1 : s_rnge("cvlen", i__1, 
		    "zzekecmp_", (ftnlen)548)], cval + (((i__2 = i__ - 1) < 2 
		    && 0 <= i__2 ? i__2 : s_rnge("cval", i__2, "zzekecmp_", (
		    ftnlen)548)) << 10), &null[(i__3 = i__ - 1) < 2 && 0 <= 
		    i__3 ? i__3 : s_rnge("null", i__3, "zzekecmp_", (ftnlen)
		    548)], &found, (ftnlen)1024);
	    if (! found) {
		chkin_("ZZEKECMP", (ftnlen)8);
		setmsg_("EK = #; COLIDX = #; ROW = #; ELTIDX  = #.Column ent"
			"ry element was not found.", (ftnlen)76);
		errhan_("#", &hans[i__ - 1], (ftnlen)1);
		errint_("#", &cldscs[i__ * 11 - 3], (ftnlen)1);
		errint_("#", &rows[i__ - 1], (ftnlen)1);
		errint_("#", &elts[i__ - 1], (ftnlen)1);
		sigerr_("SPICE(INVALIDINDEX)", (ftnlen)19);
		chkout_("ZZEKECMP", (ftnlen)8);
		return ret_val;
	    }

/*           Let CMPLEN(I) be the string length to use in comparisons. */

/* Computing MIN */
	    i__3 = cvlen[(i__2 = i__ - 1) < 2 && 0 <= i__2 ? i__2 : s_rnge(
		    "cvlen", i__2, "zzekecmp_", (ftnlen)577)];
	    cmplen[(i__1 = i__ - 1) < 2 && 0 <= i__1 ? i__1 : s_rnge("cmplen",
		     i__1, "zzekecmp_", (ftnlen)577)] = min(i__3,1024);
	}
	if (null[0] || null[1]) {
	    if (! null[1]) {
		ret_val = 5;
	    } else if (! null[0]) {
		ret_val = 3;
	    }
	} else {
	    if (l_lt(cval, cval + 1024, cmplen[0], cmplen[1])) {
		ret_val = 5;
	    } else if (l_gt(cval, cval + 1024, cmplen[0], cmplen[1])) {
		ret_val = 3;
	    } else {
		ret_val = 1;
	    }
	}
    } else {

/*        Something untoward has happened in our descriptor. */

	chkin_("ZZEKECMP", (ftnlen)8);
	setmsg_("The data type code # was not recognized.", (ftnlen)40);
	errint_("#", &lhstyp, (ftnlen)1);
	sigerr_("SPICE(INVALIDDATATYPE)", (ftnlen)22);
	chkout_("ZZEKECMP", (ftnlen)8);
	return ret_val;
    }
    return ret_val;
} /* zzekecmp_ */

