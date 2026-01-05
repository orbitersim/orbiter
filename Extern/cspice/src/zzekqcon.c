/* zzekqcon.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure  ZZEKQCON ( Private: EK, read constraints from query ) */
/* Subroutine */ int zzekqcon_(integer *eqryi, char *eqryc, doublereal *eqryd,
	 integer *n, integer *cnstyp, char *ltname, integer *ltidx, char *
	lcname, integer *lcidx, integer *opcode, char *rtname, integer *rtidx,
	 char *rcname, integer *rcidx, integer *dtype, integer *cbeg, integer 
	*cend, doublereal *dval, integer *ival, ftnlen eqryc_len, ftnlen 
	ltname_len, ftnlen lcname_len, ftnlen rtname_len, ftnlen rcname_len)
{
    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer i_dnnt(doublereal *);

    /* Local variables */
    integer base, ntab, ncns;
    extern /* Subroutine */ int zzekreqi_(integer *, char *, integer *, 
	    ftnlen), chkin_(char *, ftnlen);
    integer cb, ce;
    extern logical failed_(void);
    integer icheck, tb, te;
    extern /* Subroutine */ int setmsg_(char *, ftnlen), sigerr_(char *, 
	    ftnlen), chkout_(char *, ftnlen), errint_(char *, integer *, 
	    ftnlen);
    integer ptr;

/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines.  Users should not call this routine directly due */
/*     to the volatile nature of this routine. */

/*     Return elements of a specified constraint from an encoded EK */
/*     query. */

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
/*     EQRYI      I   Integer component of query. */
/*     EQRYC      I   Character component of query. */
/*     EQRYD      I   Numeric component of query. */
/*     N          I   Index of constraint to read. */
/*     CNSTYP     O   Type of constraint (column or value comparison). */
/*     LTNAME     O   LHS table name. */
/*     LTIDX      O   LHS table index in FROM clause. */
/*     LCNAME     O   LHS column name. */
/*     LCIDX      O   LHS column index in virtual parent table. */
/*     OPCODE     O   Operation code. */
/*     RTNAME     O   RHS table name. */
/*     RTIDX      O   RHS table index in FROM clause. */
/*     RCNAME     O   RHS column name. */
/*     RCIDX      O   RHS column index in virtual parent table. */
/*     DTYPE      O   Data type of RHS value. */
/*     CBEG       O   Character begin pointer for RHS value. */
/*     CEND       O   Character end pointer for RHS value. */
/*     DVAL       O   RHS double precision value. */
/*     IVAL       O   RHS integer value. */

/* $ Detailed_Input */

/*     EQRYI, */
/*     EQRYC, */
/*     EQRYD          are, respectively, the integer, character, and */
/*                    numeric components of an encoded EK query. */
/*                    The query must have names and values resolved and */
/*                    must have been semantically checked. */

/*     N              is the index, within the WHERE clause of the query, */
/*                    of the constraint to be fetched. */

/* $ Detailed_Output */

/*     CNSTYP         is the constraint type.  Possible values are */

/*                       EQCOL  ...  constraint compares two columns */
/*                       EQVAL  ...  constraint compares column and value */

/*     LTNAME         is the table name for the LHS of the constraint. */
/*                    If an alias was supplied in the query, that */
/*                    alias is returned.  If the column was unqualified, */
/*                    LTNAME is returned blank. */

/*     LTIDX          is the index of the LHS table in the FROM clause. */

/*     LCNAME         is the name of the LHS column. */

/*     LCIDX          is the index of the LHS column in the virtual */
/*                    table containing the column. */

/*     OPCODE         is the operator code used in the constraint. */

/*     RTNAME         is the table name for the RHS of the constraint. */
/*                    RTNAME is meaningful only if the constraint */
/*                    compares two columns, as indicated by CNSTYP. */
/*                    If an alias was supplied in the query, that */
/*                    alias is returned.  If the column was unqualified, */
/*                    RTNAME is returned blank. */

/*     RTIDX          is the index of the RHS table in the FROM clause. */
/*                    RTIDX is meaningful only if the constraint */
/*                    compares two columns, as indicated by CNSTYP. */

/*     RCNAME         is the name of the RHS column.  RCNAME is */
/*                    meaningful only if the constraint compares two */
/*                    columns, as indicated by CNSTYP. */

/*     RCIDX          is the index of the RHS column in the virtual */
/*                    table containing the column.  RCIDX is */
/*                    meaningful only if the constraint compares two */
/*                    columns, as indicated by CNSTYP. */

/*     DTYPE          is the data type of the value on the RHS of the */
/*                    constraint.  DTYPE is meaningful only if the */
/*                    constraint compares a column against a value, */
/*                    as indicated by CNSTYP. */

/*     CBEG, */
/*     CEND           are, respectively, begin and end character pointers */
/*                    into the EQRYC array; these pointers give the */
/*                    location of a character value on the RHS of a */
/*                    query constraint.  CBEG and CEND are meaningful */
/*                    only if the constraint compares a column against a */
/*                    value, as indicated by CNSTYP, and if the value's */
/*                    data type is CHR, as indicated by DTYPE. */

/*     IVAL           is an integer value on the RHS of the constraint. */
/*                    IVAL is meaningful only if the constraint compares */
/*                    a column against a value, as indicated by CNSTYP, */
/*                    and if the value's data type is INT, as indicated */
/*                    by DTYPE. */

/*     DVAL           is a double precision value on the RHS of the */
/*                    constraint.  DVAL is meaningful only if the */
/*                    constraint compares a column against a value, as */
/*                    indicated by CNSTYP, and if the value's data type */
/*                    is DP or TIME, as indicated by DTYPE. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the input query is not initialized, the error will be */
/*         diagnosed by routines called by this routine.  The outputs */
/*         will not be modified. */

/*     2)  If the input query has not been semantically checked, the */
/*         error SPICE(NOTSEMCHECKED) will be signaled.  The outputs */
/*         will not be modified. */

/*     3)  If the index N is less than 1 or greater than the number of */
/*         constraints in the query, the error SPICE(INVALIDINDEX) */
/*         will be signaled.  The outputs */
/*         will not be modified. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     The call */

/*        CALL ZZEKREQI (  EQRYI,  'NUM_CONSTRAINTS',  N  ) */

/*     may be used to get the constraint count from an encoded query. */

/* $ Examples */

/*     See EKSRCH. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 29-APR-2009 (NJB) */

/*        Bug fix: this routine now does not attempt to */
/*        read constraint RHS value parameters from the */
/*        encoded query when the RHS value is NULL, as */
/*        indicated by the opcode. */

/* -    Beta Version 1.0.0, 17-OCT-1995 (NJB) */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     Use discovery check-in. */

    zzekreqi_(eqryi, "SEM_CHECKED", &icheck, (ftnlen)11);
    if (failed_()) {
	return 0;
    }
    if (icheck == -1) {
	chkin_("ZZEKQCON", (ftnlen)8);
	setmsg_("Encoded query has not been semantically checked.", (ftnlen)
		48);
	sigerr_("SPICE(NOTSEMCHECKED)", (ftnlen)20);
	chkout_("ZZEKQCON", (ftnlen)8);
	return 0;
    }
    zzekreqi_(eqryi, "NUM_CONSTRAINTS", &ncns, (ftnlen)15);
    zzekreqi_(eqryi, "NUM_TABLES", &ntab, (ftnlen)10);
    if (*n < 1 || *n > ncns) {
	chkin_("ZZEKQCON", (ftnlen)8);
	setmsg_("Constraint index # is out of valid range 1:#.", (ftnlen)45);
	errint_("#", n, (ftnlen)1);
	errint_("#", &ncns, (ftnlen)1);
	sigerr_("SPICE(INVALIDINDEX)", (ftnlen)19);
	chkout_("ZZEKQCON", (ftnlen)8);
	return 0;
    }

/*     Compute the base address of the Nth constraint. */

    base = ntab * 12 + 19 + (*n - 1) * 26;

/*     Get the constraint type. */

    *cnstyp = eqryi[base + 6];

/*     Get the LHS items. */

    *ltidx = eqryi[base + 12];
    tb = eqryi[base + 10];
    te = eqryi[base + 11];
    if (tb != 0) {
	s_copy(ltname, eqryc + (tb - 1), ltname_len, te - (tb - 1));
    } else {
	s_copy(ltname, " ", ltname_len, (ftnlen)1);
    }
    *lcidx = eqryi[base + 18];
    cb = eqryi[base + 16];
    ce = eqryi[base + 17];
    s_copy(lcname, eqryc + (cb - 1), lcname_len, ce - (cb - 1));

/*     Next, the opcode. */

    *opcode = eqryi[base + 19];

/*     If the constraint compares two columns, get the RHS table and */
/*     column info. */

    if (*cnstyp == 1) {
	*rtidx = eqryi[base + 25];
	tb = eqryi[base + 23];
	te = eqryi[base + 24];
	if (tb != 0) {
	    s_copy(rtname, eqryc + (tb - 1), rtname_len, te - (tb - 1));
	} else {
	    s_copy(rtname, " ", rtname_len, (ftnlen)1);
	}
	*rcidx = eqryi[base + 31];
	cb = eqryi[base + 29];
	ce = eqryi[base + 30];
	s_copy(rcname, eqryc + (cb - 1), rcname_len, ce - (cb - 1));

/*        ...and clear out the scalar outputs. */

	*cbeg = 1;
	*cend = 1;
	*dval = 0.;
	*ival = 0;
    } else {

/*        The constraint compares a column and a value.  Set the */
/*        appropriate scalar output, and clear out the other outputs. */

	if (*opcode == 9 || *opcode == 10) {

/*           There's no output value; the opcode implies the value NULL. */
/*           Set the outputs to innocuous defaults. */

	    *cbeg = 1;
	    *cend = 1;
	    *dval = 0.;
	    *ival = 0;
	} else {

/*           This is the normal case; set the scalar output values */
/*           according to the RHS data type. */

	    *dtype = eqryi[base + 20];
	    if (*dtype == 1) {
		*cbeg = eqryi[base + 23];
		*cend = eqryi[base + 24];
		*dval = 0.;
		*ival = 0;
	    } else if (*dtype == 3) {
		ptr = eqryi[base + 23];
		*ival = i_dnnt(&eqryd[ptr - 1]);
		*dval = 0.;
		*cbeg = 1;
		*cend = 1;
	    } else {

/*              The data type is DP or TIME. */

		ptr = eqryi[base + 23];
		*dval = eqryd[ptr - 1];
		*ival = 0;
		*cbeg = 1;
		*cend = 1;
	    }
	}

/*        Set the RHS table and column outputs. */

	*rtidx = 0;
	s_copy(rtname, " ", rtname_len, (ftnlen)1);
	*rcidx = 0;
	s_copy(rtname, " ", rtname_len, (ftnlen)1);
    }
    return 0;
} /* zzekqcon_ */

