/* zzeksemc.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;

/* $Procedure  ZZEKSEMC ( Private: EK, semantically check encoded query ) */
/* Subroutine */ int zzeksemc_(char *query, integer *eqryi, char *eqryc, 
	logical *error, char *errmsg, integer *errptr, ftnlen query_len, 
	ftnlen eqryc_len, ftnlen errmsg_len)
{
    /* Initialized data */

    static char typstr[32*4] = "CHARACTER                       " "DOUBLE PR"
	    "ECISION                " "INTEGER                         " "TIM"
	    "E                            ";

    /* System generated locals */
    integer i__1, i__2;

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_rnge(char *, integer, char *, integer);

    /* Local variables */
    integer base, ntab, ncnj, ncns, nord;
    extern /* Subroutine */ int zzekqtab_(integer *, char *, integer *, char *
	    , char *, ftnlen, ftnlen, ftnlen), zzekreqi_(integer *, char *, 
	    integer *, ftnlen), zzekweqi_(char *, integer *, integer *, 
	    ftnlen);
    integer i__;
    extern /* Subroutine */ int ekcii_(char *, integer *, char *, integer *, 
	    ftnlen, ftnlen);
    char alias[64];
    extern /* Subroutine */ int chkin_(char *, ftnlen), repmc_(char *, char *,
	     char *, char *, ftnlen, ftnlen, ftnlen, ftnlen), repmi_(char *, 
	    char *, integer *, char *, ftnlen, ftnlen, ftnlen);
    extern logical failed_(void);
    char colnam[32], lhstab[64], ordtab[64], rhstab[64];
    integer attdsc[6], cnstyp, colidx, irsolv, lhssiz, lhstyp, opcode, rhssiz,
	     rhstyp, tabidx, trsolv;
    logical likeop, nulval;
    extern /* Subroutine */ int setmsg_(char *, ftnlen), sigerr_(char *, 
	    ftnlen), chkout_(char *, ftnlen);
    integer lxb[2], lxe[2];

/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines.  Users should not call this routine directly due */
/*     to the volatile nature of this routine. */

/*     Semantically check an encoded EK query. */

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

/*     EK */
/*     PRIVATE */

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


/*     Include Section:  EK Column Attribute Descriptor Parameters */

/*        ekattdsc.inc Version 1    23-AUG-1995 (NJB) */


/*     This include file declares parameters used in EK column */
/*     attribute descriptors.  Column attribute descriptors are */
/*     a simplified version of column descriptors:  attribute */
/*     descriptors describe attributes of a column but do not contain */
/*     addresses or pointers. */


/*     Size of column attribute descriptor */


/*     Indices of various pieces of attribute descriptors: */


/*     ATTSIZ is the index of the column's class code.  (We use the */
/*     word `class' to distinguish this item from the column's data */
/*     type.) */


/*     ATTTYP is the index of the column's data type code (CHR, INT, DP, */
/*     or TIME).  The type is actually implied by the class, but it */
/*     will frequently be convenient to look up the type directly. */



/*     ATTLEN is the index of the column's string length value, if the */
/*     column has character type.  A value of IFALSE in this element of */
/*     the descriptor indicates that the strings have variable length. */


/*     ATTSIZ is the index of the column's element size value.  This */
/*     descriptor element is meaningful for columns with fixed-size */
/*     entries.  For variable-sized columns, this value is IFALSE. */


/*     ATTIDX is the location of a flag that indicates whether the column */
/*     is indexed.  The flag takes the value ITRUE if the column is */
/*     indexed and otherwise takes the value IFALSE. */


/*     ATTNFL is the index of a flag indicating whether nulls are */
/*     permitted in the column.  The value at location NFLIDX is */
/*     ITRUE if nulls are permitted and IFALSE otherwise. */


/*     End Include Section:  EK Column Attribute Descriptor Parameters */

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


/*     Include Section:  EK Boolean Enumerated Type */


/*        ekbool.inc Version 1   21-DEC-1994 (NJB) */


/*     Within the EK system, boolean values sometimes must be */
/*     represented by integer or character codes.  The codes and their */
/*     meanings are listed below. */

/*     Integer code indicating `true': */


/*     Integer code indicating `false': */


/*     Character code indicating `true': */


/*     Character code indicating `false': */


/*     End Include Section:  EK Boolean Enumerated Type */

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


/*     Include Section:  EK Column Name Size */

/*        ekcnamsz.inc Version 1    17-JAN-1995 (NJB) */


/*     Size of column name, in characters. */


/*     End Include Section:  EK Column Name Size */

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


/*     Include Section:  EK Encoded Query Internal Parameters */

/*        ekquery.inc  Version 3    16-NOV-1995 (NJB) */

/*           Updated to reflect increased value of MAXCON in */
/*           ekqlimit.inc. */

/*        ekquery.inc  Version 2    03-AUG-1995 (NJB) */

/*           Updated to support representation of the SELECT clause. */


/*        ekquery.inc  Version 1    12-JAN-1995 (NJB) */


/*     An encoded EK query is an abstract data type implemented */
/*     as an integer cell, along with a double precision cell and */
/*     a character string.  The d.p. cell and string contain numeric */
/*     and string values from the query string represented by the */
/*     encoded query. */

/*     The parameters in this file are intended for use only by the */
/*     EK encoded query access routines.  Callers of EK routines should */
/*     not use these parameters. */

/*     The following parameters are indices of specified elements */
/*     in the integer portion of the encoded query. */

/*     Encoded query architecture type: */


/*     `Name resolution' consists of: */

/*        - Verifying existence of tables:  any table names listed */
/*          in the FROM clause of a query must be loaded. */

/*        - Validating table aliases used to qualify column names. */

/*        - Verifying existence of columns and obtaining data types */
/*          for columns. */

/*        - Setting data type codes for literal values in the encoded */
/*          query. */

/*        - Checking consistency of operators and operand data types. */

/*        - Making sure unqualified column names are unambiguous. */

/*        - For constraints, mapping the table names used to qualify */
/*          column names to the ordinal position in the FROM clause */
/*          of the corresponding table. */


/*     Initialization status---this flag indicates whether the encoded */
/*     query has been initialized.  Values are ITRUE or IFALSE.  See the */
/*     include file ekbool.inc for parameter values. */


/*     Parse status---this flag indicates whether the parsing operation */
/*     that produced an encoded query has been completed. Values are */
/*     ITRUE or IFALSE. */


/*     Name resolution status---this flag indicates whether names */
/*     have been resolved in an encoded query.  Values are ITRUE or */
/*     IFALSE. */


/*     Time resolution status---this flag indicates whether time values */
/*     have been resolved in an encoded query.  Time resolution */
/*     consists of converting strings representing time values to ET. */
/*     Values of the status are ITRUE or IFALSE. */


/*     Semantic check status---this flag indicates whether semantic */
/*     checking of constraints has been performed. */


/*     Number of tables specified in FROM clause: */


/*     Number of constraints in query: */


/*     A special value is used to indicate the `maximal' constraint--- */
/*     one that logically cannot be satisfied.  If the constraints */
/*     are equivalent to the maximal constraint, the location EQNCNS */
/*     is assigned the value EQMXML */


/*     Number of constraint conjunctions: */


/*     Number of order-by columns: */


/*     Number of SELECT columns: */


/*     Size of double precision buffer: */


/*     `Free' pointer into double precision buffer: */


/*     Size of character string buffer: */


/*     `Free' pointer into character string buffer: */


/*     The following four base pointers will be valid after a query */
/*     has been parsed: */

/*     Base pointer for SELECT column descriptors: */


/*     Base pointer for constraint descriptors: */


/*     Base pointer for conjunction sizes: */


/*     Base pointer for order-by column descriptors: */


/*     After the quantities named above, the integer array contains */
/*     series of descriptors for tables, constraints, and order-by */
/*     columns, as well as a list of `conjunction sizes'---that is, */
/*     the sizes of the groups of constraints that form conjunctions, */
/*     after the input query has been re-arranged as a disjunction of */
/*     conjunctions of constraints. */


/*     The offsets of specific elements within descriptors are */
/*     parameterized. The base addresses of the descriptors themselves */
/*     must be  calculated using the counts and sizes of the items */
/*     preceding them. */

/*     A diagram of the structure of the variable-size portion of the */
/*     integer array is shown below: */


/*        +-------------------------------------+ */
/*        | Fixed-size portion of encoded query | */
/*        +-------------------------------------+ */
/*        |         Encoded FROM clause         | */
/*        +-------------------------------------+ */
/*        |      Encoded constraint clause      | */
/*        +-------------------------------------+ */
/*        |          Conjunction sizes          | */
/*        +-------------------------------------+ */
/*        |       Encoded ORDER BY clause       | */
/*        +-------------------------------------+ */
/*        |        Encoded SELECT clause        | */
/*        +-------------------------------------+ */


/*     Value Descriptors */
/*     ---------------- */

/*     In order to discuss the various descriptors below, we'll make use */
/*     of sub-structures called `value descriptors'.  These descriptors */
/*     come in two flavors:  character and double precision.  For */
/*     strings, a descriptor is a set of begin and end pointers that */
/*     indicate the location of the string in the character portion of an */
/*     encoded query, along with the begin and end pointers for the */
/*     corresponding lexeme in the original query.  The pointers are set */
/*     to zero when they are not in use, for example if they refer to an */
/*     optional lexeme that did not appear in the input query. */

/*     All value descriptors start with a data type indicator; values */
/*     are from ektype.inc.  Integer and time values are referred to */
/*     by double precision descriptors. */

/*     Parameters for string value descriptor elements: */


/*     Numeric value descriptors are similar to those for string values, */
/*     the difference being that they have only one pointer to the value */
/*     they represent.  This pointer is the index of the value in the */
/*     encoded query's numeric buffer. */


/*     All value descriptors have the same size.  In order to allow */
/*     table descriptors to have the same size as value descriptors, */
/*     we include an extra element in the descriptor. */


/*     Column Descriptors */
/*     ----------------- */

/*     Each column descriptor consists of a character descriptor for the */
/*     name of the column, followed by an index, which gives the ordinal */
/*     position of the column in the logical table to which the column */
/*     belongs.  The index element is filled in during name resolution. */


/*     Table Descriptors */
/*     ----------------- */

/*     Each table descriptor consists of a character descriptor for the */
/*     name of the table, followed by an index, which gives the ordinal */
/*     position of the table in the FROM clause in the original query. */
/*     The index element is filled in during name resolution.  Aliases */
/*     and table names have identical descriptor structures. */


/*     Constraint descriptors */
/*     ------------------ */

/*     Each constraint is characterized by: */

/*        - A code indicating whether the constraint compares values */
/*          in two columns or the value in a column and a literal */
/*          value.  The values of this element are EQCOL and EQVAL. */



/*        - A descriptor for the table used to qualify the */
/*          column name on the left side of the constraint. */


/*        - A character value descriptor for the column name on the left */
/*          side of the query. */


/*        - An operator code indicating the relational operator used */
/*          in the constraint. */


/*        If the constraint compares values from two columns, the */
/*        next items are table and column name descriptors that apply to */
/*        the column named on the right side of the relational operator. */


/*        If the constraint has a literal value on the right side, the */
/*        operator code is followed by... */

/*        - a value descriptor. */


/*        - Size of a constraint descriptor: */


/*     Conjunction sizes */
/*     ----------------- */

/*     The size of each conjunction of constraints occupies a single */
/*     integer. */




/*     Order-by Column Descriptors */
/*     --------------------------- */

/*     Each order-by column descriptor contains descriptors for */
/*     the table containing the column and for the name of the column */
/*     itself; one additional element is used to indicate the direction */
/*     of the ordering (ascending vs descending). */


/*        - The last integer in the descriptor indicates whether the */
/*          order direction is ascending or descending. */


/*        - Size of an order-by column descriptor: */


/*     Codes indicating sense of ordering (ascending vs descending): */


/*     SELECT Column Descriptors */
/*     --------------------------- */

/*     Each SELECT column descriptor contains descriptors for */
/*     the table containing the column and for the name of the column */
/*     itself. */


/*        - Size of a SELECT column descriptor: */


/*     Miscellaneous parameters: */


/*     EQIMIN is the minimum size of the integer portion of */
/*     an encoded query.  EQIMIN depends on the parameters */

/*        MAXTAB */
/*        MAXCON */
/*        MAXORD */
/*        MAXSEL */

/*     all of which are declared in the include file ekqlimit.inc. */
/*     The functional definition of EQIMIN is: */

/*     INTEGER               EQIMIN */
/*     PARAMETER           ( EQIMIN =   EQVBAS */
/*    .                              +  MAXTAB * EQVDSZ * 2 */
/*    .                              +  MAXCON * EQCDSZ */
/*    .                              +  MAXCON */
/*    .                              +  MAXORD * EQODSZ */
/*    .                              +  MAXSEL * EQSDSZ     ) */


/*     End Include Section:  EK Encoded Query Internal Parameters */

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


/*     Include Section:  EK Table Name Size */

/*        ektnamsz.inc Version 1    17-JAN-1995 (NJB) */


/*     Size of table name, in characters. */


/*     End Include Section:  EK Table Name Size */

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
/*     QUERY      I   Original query. */
/*     EQRYI     I-O  Integer component of query. */
/*     EQRYC     I-O  Character component of query. */
/*     ERROR      O   Error flag. */
/*     ERRMSG     O   Semantic error message. */
/*     ERRPTR     O   Position in query where error was detected. */

/* $ Detailed_Input */

/*     QUERY          is the original query from which EQRYI and EQRYC */
/*                    were obtained.  QUERY is used only for */
/*                    construction of error messages. */

/*     EQRYI          is the integer portion of an encoded EK query. */
/*                    The query must have been parsed and must have */
/*                    its table and column names resolved.  Time values */
/*                    must also have been resolved. */

/*     EQRYC          is the character portion of an encoded EK query. */

/* $ Detailed_Output */

/*     EQRYI          is the integer portion of an encoded EK query. */
/*                    On output, semantic checking will have been */
/*                    performed. */

/*     EQRYC          is the character portion of an encoded EK query. */

/*     ERROR          is a logical flag indicating whether a semantic */
/*                    error was detected. */

/*     ERRMSG         is an error message describing a semantic error, */
/*                    if such an error was detected.  If ERROR is */
/*                    returned .FALSE., then ERRPTR is undefined. */

/*     ERRPTR         is the character position in the original query */
/*                    at which a semantic error was detected, if the */
/*                    input query contains a semantic error.  This */
/*                    index refers to the offending lexeme's position in */
/*                    the original query represented by the input encoded */
/*                    query.  If ERROR is returned .FALSE., ERRPTR is */
/*                    undefined. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If no EK files are loaded at the time this routine is called, */
/*         the error will be diagnosed by routines called by this */
/*         routine. */

/*     2)  If the input query is not initialized, the error will be */
/*         diagnosed by routines called by this routine.  The outputs */
/*         will not be modified. */

/*     3)  If the input query has not had its names resolved, the error */
/*         SPICE(UNRESOLVEDNAMES) will be signaled.  The outputs */
/*         will not be modified. */

/*     4)  If the input query contains time values that have not been */
/*         resolved, the error SPICE(UNRESOLVEDTIMES) will be signaled. */
/*         The outputs will not be modified. */

/*     5)  If any sort of semantic error is detected in the input query, */
/*         the output flag ERROR is set, an error message is returned, */
/*         and LXBEG and LXEND are set to indicate the location of the */
/*         first lexeme at which an error was detected. */

/*         The checks performed by this routine are listed below: */

/*           - Constraints comparing values from two columns must */
/*             refer to columns having identical data types, or else */
/*             both types must be numeric. */

/*           - Constraints comparing values from a column with literal */
/*             values must refer to columns having the data type of the */
/*             literal value. */

/*           - The LIKE and NOT LIKE operators may be used only with */
/*             string values. */

/*           - Columns named in constraints must be scalar-valued. */

/*           - Columns named as order-by columns must be scalar-valued. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     The semantic checking performed by this routine is dependent on */
/*     the kernels loaded at the time this routine is called. */

/*     This routine assumes that encoded EK query architecture version */
/*     1 is to be used with the query to be initialized; this routine */
/*     will not work with any other architecture version. */

/* $ Examples */

/*     See EKFIND. */

/* $ Restrictions */

/*     1) Loading or unloading EK files between name resolution of the */
/*        the input query and passing the query to this routine will */
/*        invalidate the checking done by this routine, and may cause */
/*        the routine to fail. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.1, 03-OCT-2021 (NJB) */

/*        Corrected typos in comments. */

/* -    Beta Version 1.0.0, 23-OCT-1995 (NJB) */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     Local variables */


/*     Saved variables */


/*     Initial values */


/*     Use discovery check-in. */

    *error = FALSE_;
    s_copy(errmsg, " ", errmsg_len, (ftnlen)1);
    *errptr = 0;
    zzekreqi_(eqryi, "NAMES_RESOLVED", &irsolv, (ftnlen)14);
    if (failed_()) {
	return 0;
    }
    if (irsolv == -1) {
	chkin_("ZZEKSEMC", (ftnlen)8);
	setmsg_("Encoded query has not had names resolved.", (ftnlen)41);
	sigerr_("SPICE(UNRESOLVEDNAMES)", (ftnlen)22);
	chkout_("ZZEKSEMC", (ftnlen)8);
	return 0;
    }
    zzekreqi_(eqryi, "TIMES_RESOLVED", &trsolv, (ftnlen)14);
    if (failed_()) {
	return 0;
    }
    if (trsolv == -1) {
	chkin_("ZZEKSEMC", (ftnlen)8);
	setmsg_("Encoded query has not had time values resolved.", (ftnlen)47)
		;
	sigerr_("SPICE(UNRESOLVEDTIMES)", (ftnlen)22);
	chkout_("ZZEKSEMC", (ftnlen)8);
	return 0;
    }

/*     Get the important counts from the query. */

    zzekreqi_(eqryi, "NUM_TABLES", &ntab, (ftnlen)10);
    zzekreqi_(eqryi, "NUM_CONSTRAINTS", &ncns, (ftnlen)15);
    zzekreqi_(eqryi, "NUM_CONJUNCTIONS", &ncnj, (ftnlen)16);
    zzekreqi_(eqryi, "NUM_ORDERBY_COLS", &nord, (ftnlen)16);

/*     Perform semantic checks applicable to constraints. */

    i__1 = ncns;
    for (i__ = 1; i__ <= i__1; ++i__) {

/*        Calculate the base address of the constraint. */

	base = ntab * 12 + 19 + (i__ - 1) * 26;

/*        Obtain the constraint type. */

	cnstyp = eqryi[base + 6];

/*        Get the index of the table containing the LHS column, and get */
/*        the index of this column within that table.  Look up the */
/*        table name. */

	tabidx = eqryi[base + 12];
	colidx = eqryi[base + 18];
	lxb[0] = eqryi[base + 14];
	lxe[0] = eqryi[base + 15];
	zzekqtab_(eqryi, eqryc, &tabidx, lhstab, alias, eqryc_len, (ftnlen)64,
		 (ftnlen)64);

/*        Look up the name and attributes of the column on the LHS of the */
/*        constraint. */

	ekcii_(lhstab, &colidx, colnam, attdsc, (ftnlen)64, (ftnlen)32);
	lhstyp = attdsc[1];
	lhssiz = attdsc[3];
	if (lhssiz != 1) {
	    *error = TRUE_;
	    s_copy(errmsg, "Non-scalar column <#> having size # found in que"
		    "ry constraint.", errmsg_len, (ftnlen)62);
	    i__2 = lxb[0] - 1;
	    repmc_(errmsg, "#", query + i__2, errmsg, errmsg_len, (ftnlen)1, 
		    lxe[1] - i__2, errmsg_len);
	    repmi_(errmsg, "#", &lhssiz, errmsg, errmsg_len, (ftnlen)1, 
		    errmsg_len);
	    *errptr = lxb[0];
	    return 0;
	}

/*        Get the operator for the current constraint. */

	opcode = eqryi[base + 19];

/*        Decide whether the constraint is an `IS NULL' or `IS NOT NULL' */
/*        test. */

	nulval = opcode == 9 || opcode == 10;

/*        Check for use of the LIKE or NOT LIKE operators.  These */
/*        operators may be used only if the LHS column has character */
/*        type. */

	likeop = opcode == 7 || opcode == 8;
	if (likeop && lhstyp != 1) {
	    *error = TRUE_;
	    s_copy(errmsg, "LIKE and NOT LIKE operators may be used only wit"
		    "h character columns.  Column <#> has type #.", errmsg_len,
		     (ftnlen)92);
	    i__2 = lxb[0] - 1;
	    repmc_(errmsg, "#", query + i__2, errmsg, errmsg_len, (ftnlen)1, 
		    lxe[0] - i__2, errmsg_len);
	    repmc_(errmsg, "#", typstr + (((i__2 = lhstyp - 1) < 4 && 0 <= 
		    i__2 ? i__2 : s_rnge("typstr", i__2, "zzeksemc_", (ftnlen)
		    383)) << 5), errmsg, errmsg_len, (ftnlen)1, (ftnlen)32, 
		    errmsg_len);
	    *errptr = lxb[0];
	    return 0;
	}

/*        If the constraint compares two columns, get the same */
/*        information for the RHS column. */

	if (cnstyp == 1) {
	    tabidx = eqryi[base + 25];
	    colidx = eqryi[base + 31];
	    lxb[1] = eqryi[base + 27];
	    lxe[1] = eqryi[base + 28];
	    zzekqtab_(eqryi, eqryc, &tabidx, rhstab, alias, eqryc_len, (
		    ftnlen)64, (ftnlen)64);

/*           Look up the name and attributes of the column on the RHS of */
/*           the constraint. */

	    ekcii_(rhstab, &colidx, colnam, attdsc, (ftnlen)64, (ftnlen)32);
	    rhstyp = attdsc[1];
	    rhssiz = attdsc[3];
	    if (rhssiz != 1) {
		*error = TRUE_;
		s_copy(errmsg, "Non-scalar column <#> having size # found in"
			" query constraint.", errmsg_len, (ftnlen)62);
		i__2 = lxb[0] - 1;
		repmc_(errmsg, "#", query + i__2, errmsg, errmsg_len, (ftnlen)
			1, lxe[1] - i__2, errmsg_len);
		repmi_(errmsg, "#", &rhssiz, errmsg, errmsg_len, (ftnlen)1, 
			errmsg_len);
		*errptr = lxb[1];
		return 0;
	    }

/*           Check for data type mismatch. */

	    if (rhstyp != lhstyp) {

/*              The only allowed mismatch is between integers and */
/*              d.p. numbers. */

		if (lhstyp == 4 || lhstyp == 1 || rhstyp == 4 || rhstyp == 1) 
			{
		    *error = TRUE_;
		    s_copy(errmsg, "Data type mismatch: column <#> has data "
			    "type #; column <#> has data type #.", errmsg_len, 
			    (ftnlen)75);
		    i__2 = lxb[0] - 1;
		    repmc_(errmsg, "#", query + i__2, errmsg, errmsg_len, (
			    ftnlen)1, lxe[0] - i__2, errmsg_len);
		    repmc_(errmsg, "#", typstr + (((i__2 = lhstyp - 1) < 4 && 
			    0 <= i__2 ? i__2 : s_rnge("typstr", i__2, "zzeks"
			    "emc_", (ftnlen)444)) << 5), errmsg, errmsg_len, (
			    ftnlen)1, (ftnlen)32, errmsg_len);
		    i__2 = lxb[1] - 1;
		    repmc_(errmsg, "#", query + i__2, errmsg, errmsg_len, (
			    ftnlen)1, lxe[1] - i__2, errmsg_len);
		    repmc_(errmsg, "#", typstr + (((i__2 = rhstyp - 1) < 4 && 
			    0 <= i__2 ? i__2 : s_rnge("typstr", i__2, "zzeks"
			    "emc_", (ftnlen)446)) << 5), errmsg, errmsg_len, (
			    ftnlen)1, (ftnlen)32, errmsg_len);
		    *errptr = lxb[1];
		    return 0;
		}
	    }
	} else {

/*           The constraint compares a column against a value.  If the */
/*           operator is `IS NULL' or `IS NOT NULL', there are no */
/*           further semantic checks to be made. */

	    if (nulval) {
		return 0;
	    }

/*           Get the data type of the value on the RHS. */

	    rhstyp = eqryi[base + 20];
	    lxb[1] = eqryi[base + 21];
	    lxe[1] = eqryi[base + 22];
	    if (rhstyp != lhstyp) {

/*              The only allowed mismatch is between integers and */
/*              d.p. numbers. */

		if (lhstyp == 4 || lhstyp == 1 || rhstyp == 4 || rhstyp == 1) 
			{
		    *error = TRUE_;
		    s_copy(errmsg, "Data type mismatch: column <#> has data "
			    "type #; value <#> has data type #.", errmsg_len, (
			    ftnlen)74);
		    i__2 = lxb[0] - 1;
		    repmc_(errmsg, "#", query + i__2, errmsg, errmsg_len, (
			    ftnlen)1, lxe[0] - i__2, errmsg_len);
		    repmc_(errmsg, "#", typstr + (((i__2 = lhstyp - 1) < 4 && 
			    0 <= i__2 ? i__2 : s_rnge("typstr", i__2, "zzeks"
			    "emc_", (ftnlen)488)) << 5), errmsg, errmsg_len, (
			    ftnlen)1, (ftnlen)32, errmsg_len);
		    i__2 = lxb[1] - 1;
		    repmc_(errmsg, "#", query + i__2, errmsg, errmsg_len, (
			    ftnlen)1, lxe[1] - i__2, errmsg_len);
		    repmc_(errmsg, "#", typstr + (((i__2 = rhstyp - 1) < 4 && 
			    0 <= i__2 ? i__2 : s_rnge("typstr", i__2, "zzeks"
			    "emc_", (ftnlen)490)) << 5), errmsg, errmsg_len, (
			    ftnlen)1, (ftnlen)32, errmsg_len);
		    *errptr = lxb[1];
		    return 0;
		}
	    }
	}

/*        We've finished the checks on the current constraint. */

    }

/*     Now check the order-by columns, if any are present.  These */
/*     columns must have scalar type. */

    i__1 = nord;
    for (i__ = 1; i__ <= i__1; ++i__) {

/*        Get the query column descriptor for the Ith order-by column. */

	base = ntab * 12 + 19 + ncnj + ncns * 26 + (i__ - 1) * 13;

/*        Look up the attributes of the column.  It's the size we're */
/*        after. */

	tabidx = eqryi[base + 11];
	colidx = eqryi[base + 17];
	lxb[0] = eqryi[base + 13];
	lxe[0] = eqryi[base + 14];
	zzekqtab_(eqryi, eqryc, &tabidx, ordtab, alias, eqryc_len, (ftnlen)64,
		 (ftnlen)64);
	ekcii_(ordtab, &colidx, colnam, attdsc, (ftnlen)64, (ftnlen)32);
	if (attdsc[3] != 1) {
	    *error = TRUE_;
	    s_copy(errmsg, "Non-scalar column <#> having size # found in ord"
		    "er-by column.", errmsg_len, (ftnlen)61);
	    i__2 = lxb[0] - 1;
	    repmc_(errmsg, "#", query + i__2, errmsg, errmsg_len, (ftnlen)1, 
		    lxe[1] - i__2, errmsg_len);
	    repmi_(errmsg, "#", &attdsc[3], errmsg, errmsg_len, (ftnlen)1, 
		    errmsg_len);
	    *errptr = lxb[0];
	    return 0;
	}
    }

/*     Indicate completion of semantic checking. */

    zzekweqi_("SEM_CHECKED", &c__1, eqryi, (ftnlen)11);
    return 0;
} /* zzeksemc_ */

