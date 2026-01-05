/* zzekscmp.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure      ZZEKSCMP ( EK, scalar value comparison ) */
logical zzekscmp_(integer *op, integer *handle, integer *segdsc, integer *
	coldsc, integer *row, integer *eltidx, integer *dtype, char *cval, 
	doublereal *dval, integer *ival, logical *null, ftnlen cval_len)
{
    /* System generated locals */
    integer i__1;
    logical ret_val;

    /* Builtin functions */
    integer i_len(char *, ftnlen);
    logical l_lt(char *, char *, ftnlen, ftnlen), l_gt(char *, char *, ftnlen,
	     ftnlen);

    /* Local variables */
    char eltc[1024];
    doublereal eltd;
    integer elti;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    integer cvlen;
    logical found, enull;
    extern logical failed_(void), matchi_(char *, char *, char *, char *, 
	    ftnlen, ftnlen, ftnlen, ftnlen);
    integer cmplen;
    doublereal numval;
    integer coltyp, strlen;
    extern /* Subroutine */ int setmsg_(char *, ftnlen), errint_(char *, 
	    integer *, ftnlen), sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), errhan_(char *, integer *, ftnlen);
    integer rel;
    extern /* Subroutine */ int zzekrsc_(integer *, integer *, integer *, 
	    integer *, integer *, integer *, char *, logical *, logical *, 
	    ftnlen), zzekrsd_(integer *, integer *, integer *, integer *, 
	    integer *, doublereal *, logical *, logical *), zzekrsi_(integer *
	    , integer *, integer *, integer *, integer *, integer *, logical *
	    , logical *);

/* $ Abstract */

/*     Compare a specified scalar EK column entry with a scalar value. */

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

/*     PRIVATE */
/*     EK */

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


/*     Include Section:  EK Template Matching Wild Characters */


/*        ekwild.inc  Version 1   16-JAN-1995 (NJB) */


/*     Within the EK system, templates used for pattern matching */
/*     are those accepted by the SPICELIB routine MATCHW.  MATCHW */
/*     accepts two special characters:  one representing wild */
/*     strings and one representing wild characters.  This include */
/*     file defines those special characters for use within the EK */
/*     system. */


/*     Wild string symbol:  this character matches any string. */


/*     Wild character symbol:  this character matches any character. */


/*     End Include Section:  EK Template Matching Wild Characters */

/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     OP         I   Relational operator code. */
/*     HANDLE     I   EK file handle. */
/*     SEGDSC     I   Segment descriptor. */
/*     COLDSC     I   Column descriptor. */
/*     ROW        I   ID of row containing column entry to compare. */
/*     ELTIDX     I   Index of element in array-valued column entry. */
/*     DTYPE      I   Data type of input value. */
/*     CVAL       I   Character string to compare with column entry. */
/*     DVAL       I   D.p. value to compare with column entry. */
/*     IVAL       I   Integer value to compare with column entry. */
/*     NULL       I   Flag indicating whether scalar is null. */

/*     The function returns .TRUE. if and only if the specified column */
/*     entry and input value of the corresponding data type satisfy the */
/*     relation specified by the input argument OP. */

/* $ Detailed_Input */

/*     OP             is an integer code representing a binary relational */
/*                    operator.  The possible values of OP are the */
/*                    parameters */

/*                       EQ */
/*                       GE */
/*                       GT */
/*                       LE */
/*                       LIKE */
/*                       LT */
/*                       NE */
/*                       ISNULL */
/*                       NOTNUL */


/*     HANDLE         is an EK file handle.  The file may be open for */
/*                    reading or writing. */

/*     SEGDSC         is the EK segment descriptor of the column entry */
/*                    to be compared. */

/*     COLDSC         is an EK column descriptor for the column */
/*                    containing the entry to be compared. */

/*     ROW            is the identifier of the row containing the column */
/*                    entry to be compared. Note that these identifiers */
/*                    are polymorphic: their meaning is a function of */
/*                    the class of column that contains the entry of */
/*                    interest. */

/*     ELTIDX         is the index of the column entry element to be */
/*                    compared, if the column is array-valued.  ELTIDX */
/*                    is ignored for scalar columns. */

/*     DTYPE          is the data type of the input scalar value. */


/*     CVAL, */
/*     DVAL, */
/*     IVAL           are, respectively, character, double precision, */
/*                    and integer scalar variables.  The column entry */
/*                    is compared against whichever of these has the */
/*                    same data type as the entry; the other two */
/*                    variables are ignored.  If the data type of the */
/*                    column entry is TIME, the entry is compared with */
/*                    the variable DVAL. */

/*     NULL */

/* $ Detailed_Output */

/*     The function returns .TRUE. if and only if the specified column */
/*     entry and input value of the corresponding data type satisfy the */
/*     relation specified by the input argument OP. */

/*     If the specified column entry is null, it is considered to */
/*     precede all non-null values, and the logical value of the */
/*     expression */

/*        <column element> OP <value> */

/*     is determined accordingly.  Null character values do not satisfy */
/*     the relation */

/*        <null column element> LIKE <character value> */

/*     for any character value. */

/* $ Parameters */

/*     Within the EK system, relational operators used in EK queries are */
/*     represented by integer codes.  The codes and their meanings are */
/*     listed below. */

/*     Relational expressions in EK queries have the form */

/*        <column name> <operator> <value> */

/*     For columns containing numeric values, the operators */

/*        EQ,  GE,  GT,  LE,  LT,  NE */

/*     may be used; these operators have the same meanings as their */
/*     Fortran counterparts.  For columns containing character values, */
/*     the list of allowed operators includes those in the above list, */
/*     and in addition includes the operator */

/*        LIKE */

/*     which is used to compare strings to a template.  In the character */
/*     case, the meanings of the parameters */

/*        GE,  GT,  LE,  LT */

/*     match those of the Fortran lexical functions */

/*        LGE, LGT, LLE, LLT */

/*     Null values are considered to precede all non-null values. */

/* $ Exceptions */

/*     1)  If the input file handle is invalid, the error will be */
/*         diagnosed by routines called by this routine. */
/*         The function value is .FALSE. in this case. */

/*     2)  If an I/O error occurs while attempting to find the address */
/*         range of the specified column entry element, the error will */
/*         be diagnosed by routines called by this routine.  The */
/*         function value is .FALSE. in this case. */

/*     3)  If any of SEGDSC, COLDSC, or ROW are invalid, this routine */
/*         may fail in unpredictable, but possibly spectacular, ways. */
/*         Except as described in this header section, no attempt is */
/*         made to handle these errors. */

/*     4)  If the data type code in the input column descriptor is not */
/*         recognized, the error SPICE(INVALIDDATATYPE) is signaled. */
/*         The function value is .FALSE. in this case. */

/*     5)  If the specified column entry cannot be found, the error */
/*         SPICE(INVALIDINDEX) is signaled.  The function value is */
/*         .FALSE. in this case. */

/*     6)  If the relational operator code OP is not recognized, the */
/*         error SPICE(UNNATURALRELATION) is signaled.  The function */
/*         value is .FALSE. in this case. */


/* $ Files */

/*     See the description of the argument HANDLE in $Detailed_Input. */

/* $ Particulars */

/*     This routine is an EK utility intended to centralize a frequently */
/*     performed comparison operation. */

/* $ Examples */

/*     See ZZEKRMCH. */

/* $ Restrictions */

/*     1)  This routine must execute quickly.  Therefore, it checks in */
/*         only if it detects an error.  If an error is signaled by a */
/*         routine called by this routine, this routine will not appear */
/*         in the SPICELIB traceback display.  Also, in the interest */
/*         of speed, this routine does not test the value of the SPICELIB */
/*         function RETURN upon entry. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman   (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.3.0, 09-FEB-2015 (NJB) */

/*        Now uses ERRHAN to insert DAS file name into */
/*        long error messages. */

/* -    SPICELIB Version 1.2.0, 31-MAY-2009 (NJB) */

/*        Bug fix: routine failed to account for the possibility */
/*        that scalar string column entries can have unlimited */
/*        length. Now at most the first MAXSTR characters of such */
/*        an entry are used in comparisons. */

/* -    SPICELIB Version 1.1.0, 21-DEC-2001 (NJB) */

/*        Bug fix:  routine now indicates "no match" when operator */
/*        is LIKE or UNLIKE and column entry is null. */

/* -    SPICELIB Version 1.0.0, 17-OCT-1995 (NJB) */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     Use discovery check-in for speed. */


/*     The function value defaults to .FALSE. */

    ret_val = FALSE_;

/*     Look up the specified column element. */

    coltyp = coldsc[1];
    if (coltyp == 1) {

/*        We'll use at most the first MAXSTR characters of the input */
/*        string. */

/* Computing MIN */
	i__1 = i_len(cval, cval_len);
	cvlen = min(i__1,1024);

/*        Fetch the column entry to be compared. Note that ROW */
/*        is a polymorphic identifier. See ZZEKRSC for details */
/*        on how ROW is used. */

	zzekrsc_(handle, segdsc, coldsc, row, eltidx, &strlen, eltc, &enull, &
		found, (ftnlen)1024);
	if (failed_()) {

/*           Don't check out here because we haven't checked in. */

	    return ret_val;
	}

/*        Let CMPLEN be the string length to use in comparisons. */

	if (found && ! enull) {
	    cmplen = min(strlen,1024);
	} else {
	    cmplen = 0;
	}
    } else if (coltyp == 2 || coltyp == 4) {
	zzekrsd_(handle, segdsc, coldsc, row, eltidx, &eltd, &enull, &found);
    } else if (coltyp == 3) {
	zzekrsi_(handle, segdsc, coldsc, row, eltidx, &elti, &enull, &found);
    } else {
	chkin_("ZZEKSCMP", (ftnlen)8);
	setmsg_("Data type code # not recognized.", (ftnlen)32);
	errint_("#", &coltyp, (ftnlen)1);
	sigerr_("SPICE(INVALIDDATATYPE)", (ftnlen)22);
	chkout_("ZZEKSCMP", (ftnlen)8);
	return ret_val;
    }
    if (! found) {
	chkin_("ZZEKSCMP", (ftnlen)8);
	setmsg_("EK = #; COLIDX = #; ROW = #; ELTIDX = #. Column entry eleme"
		"nt was not found.", (ftnlen)76);
	errhan_("#", handle, (ftnlen)1);
	errint_("#", &coldsc[8], (ftnlen)1);
	errint_("#", row, (ftnlen)1);
	errint_("#", eltidx, (ftnlen)1);
	sigerr_("SPICE(INVALIDINDEX)", (ftnlen)19);
	chkout_("ZZEKSCMP", (ftnlen)8);
	return ret_val;
    }

/*     Handle the ISNULL and NOTNUL operators, if perchance we see them. */

    if (*op == 9) {
	ret_val = enull;
	return ret_val;
    } else if (*op == 10) {
	ret_val = ! enull;
	return ret_val;
    }

/*     Find the order relation that applies to the input values. */

/*     Null values precede all others. */

    if (enull) {
	if (*null) {
	    rel = 1;
	} else {
	    rel = 5;
	}
    } else if (*null) {
	if (enull) {
	    rel = 1;
	} else {
	    rel = 3;
	}
    } else {


/*        Compare the value we looked up with the input scalar value. */

	if (coltyp == 1) {
	    if (*dtype != 1) {
		chkin_("ZZEKSCMP", (ftnlen)8);
		setmsg_("Column type is #; value type is #.", (ftnlen)34);
		errint_("#", &coltyp, (ftnlen)1);
		errint_("#", dtype, (ftnlen)1);
		sigerr_("SPICE(BUG)", (ftnlen)10);
		chkout_("ZZEKSCMP", (ftnlen)8);
		return ret_val;
	    }
	    if (l_lt(eltc, cval, cmplen, cvlen)) {
		rel = 5;
	    } else if (l_gt(eltc, cval, cmplen, cvlen)) {
		rel = 3;
	    } else {
		rel = 1;
	    }
	} else if (coltyp == 4) {
	    if (*dtype != 4 && *dtype != 2) {
		chkin_("ZZEKSCMP", (ftnlen)8);
		setmsg_("Column type is #; value type is #.", (ftnlen)34);
		errint_("#", &coltyp, (ftnlen)1);
		errint_("#", dtype, (ftnlen)1);
		sigerr_("SPICE(BUG)", (ftnlen)10);
		chkout_("ZZEKSCMP", (ftnlen)8);
		return ret_val;
	    }
	    if (eltd < *dval) {
		rel = 5;
	    } else if (eltd > *dval) {
		rel = 3;
	    } else {
		rel = 1;
	    }
	} else if (coltyp == 2) {
	    if (*dtype == 3) {
		numval = (doublereal) (*ival);
	    } else if (*dtype == 2 || *dtype == 4) {
		numval = *dval;
	    } else {
		chkin_("ZZEKSCMP", (ftnlen)8);
		setmsg_("Column type is #; value type is #.", (ftnlen)34);
		errint_("#", &coltyp, (ftnlen)1);
		errint_("#", dtype, (ftnlen)1);
		sigerr_("SPICE(BUG)", (ftnlen)10);
		chkout_("ZZEKSCMP", (ftnlen)8);
		return ret_val;
	    }
	    if (eltd < numval) {
		rel = 5;
	    } else if (eltd > numval) {
		rel = 3;
	    } else {
		rel = 1;
	    }
	} else if (coltyp == 3) {
	    if (*dtype == 3) {
		numval = (doublereal) (*ival);
	    } else if (*dtype == 2) {
		numval = *dval;
	    } else {
		chkin_("ZZEKSCMP", (ftnlen)8);
		setmsg_("Column type is #; value type is #.", (ftnlen)34);
		errint_("#", &coltyp, (ftnlen)1);
		errint_("#", dtype, (ftnlen)1);
		sigerr_("SPICE(BUG)", (ftnlen)10);
		chkout_("ZZEKSCMP", (ftnlen)8);
		return ret_val;
	    }
	    if ((doublereal) elti < numval) {
		rel = 5;
	    } else if ((doublereal) elti > numval) {
		rel = 3;
	    } else {
		rel = 1;
	    }
	} else {

/*           Something untoward has happened in our column descriptor */
/*           argument. */

	    chkin_("ZZEKSCMP", (ftnlen)8);
	    setmsg_("The data type code # was not recognized.", (ftnlen)40);
	    errint_("#", &coltyp, (ftnlen)1);
	    sigerr_("SPICE(INVALIDDATATYPE)", (ftnlen)22);
	    chkout_("ZZEKSCMP", (ftnlen)8);
	    return ret_val;
	}
    }

/*     Determine the truth of the input relational expression. */

    if (*op == 1) {
	ret_val = rel == 1;
    } else if (*op == 5) {
	ret_val = rel == 5;
    } else if (*op == 4) {
	ret_val = rel != 3;
    } else if (*op == 3) {
	ret_val = rel == 3;
    } else if (*op == 2) {
	ret_val = rel != 5;
    } else if (*op == 6) {
	ret_val = rel != 1;
    } else if (*op == 7 && *dtype == 1) {
	if (*null || enull) {
	    ret_val = FALSE_;
	} else {
	    ret_val = matchi_(eltc, cval, "*", "%", cmplen, cvlen, (ftnlen)1, 
		    (ftnlen)1);
	}
    } else if (*op == 8 && *dtype == 1) {
	if (*null || enull) {
	    ret_val = FALSE_;
	} else {
	    ret_val = ! matchi_(eltc, cval, "*", "%", cmplen, cvlen, (ftnlen)
		    1, (ftnlen)1);
	}
    } else {

/*        Sorry, we couldn't resist. */

	chkin_("ZZEKSCMP", (ftnlen)8);
	setmsg_("The relational operator # was not recognized or was not app"
		"licable for data type #.", (ftnlen)83);
	errint_("#", op, (ftnlen)1);
	errint_("#", dtype, (ftnlen)1);
	sigerr_("SPICE(UNNATURALRELATION)", (ftnlen)24);
	chkout_("ZZEKSCMP", (ftnlen)8);
	return ret_val;
    }
    return ret_val;
} /* zzekscmp_ */

