/* zzekqord.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure  ZZEKQORD ( Private: EK, read order-by columns from query ) */
/* Subroutine */ int zzekqord_(integer *eqryi, char *eqryc, integer *n, char *
	table, integer *tabidx, char *column, integer *colidx, integer *sense,
	 ftnlen eqryc_len, ftnlen table_len, ftnlen column_len)
{
    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    integer base, ntab, ncnj, ncns, nord;
    extern /* Subroutine */ int zzekreqi_(integer *, char *, integer *, 
	    ftnlen), chkin_(char *, ftnlen);
    integer cb, ce;
    extern logical failed_(void);
    integer tb, te, buflen, iparse, resolv;
    extern /* Subroutine */ int setmsg_(char *, ftnlen), sigerr_(char *, 
	    ftnlen), chkout_(char *, ftnlen), errint_(char *, integer *, 
	    ftnlen);

/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines.  Users should not call this routine directly due */
/*     to the volatile nature of this routine. */

/*     Read a specified order-by table and column name, along with the */
/*     corresponding order sense, from an encoded EK query. */

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

/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     EQRYI      I   Integer component of query. */
/*     EQRYC      I   Character component of query. */
/*     N          I   Index within FROM clause of table name to read. */
/*     TABLE      O   Name of table qualifying Nth ORDER BY column. */
/*     TABIDX     O   Index of TABLE in FROM clause, if known. */
/*     COLUMN     O   Nth column in ORDER BY clause of query. */
/*     TABIDX     O   Index of column in TABLE, if known. */
/*     SENSE      O   Code giving order sense for Nth column. */

/* $ Detailed_Input */

/*     EQRYI          is the integer portion of an encoded EK query. */
/*                    The query must have been parsed. */

/*     EQRYC          is the character portion of an encoded EK query. */

/*     N              is the index, within the ORDER BY clause of the */
/*                    query, of the table whose name is to be fetched. */

/* $ Detailed_Output */

/*     TABLE          is the table name or alias associated with the Nth */
/*                    column in the ORDER BY clause of an the input */
/*                    encoded query.  If the Nth column is unqualified, */
/*                    TABLE is returned blank. */

/*     TABIDX         is the ordinal position in the FROM clause of the */
/*                    input query of the table containing the Nth order- */
/*                    by column.  TABIDX is meaningful only if name */
/*                    resolution has not been performed on the input */
/*                    query; otherwise, TABIDX is returned as zero. */

/*     COLUMN         is the name of the Nth column in the ORDER BY */
/*                    clause of an the input encoded query. */

/*     COLIDX         is the ordinal position of the Nth column in the */
/*                    ORDER BY clause with respect to the virtual table */
/*                    designated by TABLE.  This index is available only */
/*                    if the query has already had names resolved; */
/*                    otherwise, COLIDX is returned as zero. */

/*     SENSE          is an integer code giving the ordering sense to */
/*                    use with the specified column.  The possible values */
/*                    of SENSE are EQASND, which indicates that the */
/*                    order sense is ascending, and EQDSND, which */
/*                    indicates that the order sense is descending. */
/*                    `Ascending order' means that the order relation */
/*                    defined by the indicated column orders rows */
/*                    according to the order of elements in the */
/*                    indicated order-by column; `descending order' means */
/*                    that the order relation orders columns in the */
/*                    reverse of ascending order. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the input query is not initialized, the error will be */
/*         diagnosed by routines called by this routine. The outputs */
/*         will not be modified. */

/*     2)  If the input query has not been parsed, the error */
/*         SPICE(UNPARSEDQUERY) will be signaled. The outputs */
/*         will not be modified. */

/*     3)  If the index N is less than 1 or greater than the number of */
/*         columns in the ORDER BY clause, the error SPICE(INVALIDINDEX) */
/*         will be signaled. The outputs will not be modified. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     The call */

/*        CALL ZZEKREQI (  EQRYI,  'NUM_ORDERBY_COLS',  N  ) */

/*     may be used to get the ORDER BY column count from an encoded */
/*     query. */

/* $ Examples */

/*     See EKSRCH. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.1, 01-OCT-2021 (NJB) */

/*        Corrected typos in comments. */

/* -    Beta Version 1.0.0, 17-OCT-1995 (NJB) */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     Use discovery check-in. */

    zzekreqi_(eqryi, "PARSED", &iparse, (ftnlen)6);
    if (failed_()) {
	return 0;
    }
    if (iparse == -1) {
	chkin_("ZZEKQORD", (ftnlen)8);
	setmsg_("Encoded query has not yet been parsed.", (ftnlen)38);
	sigerr_("SPICE(UNPARSEDQUERY)", (ftnlen)20);
	chkout_("ZZEKQORD", (ftnlen)8);
	return 0;
    }
    zzekreqi_(eqryi, "NUM_ORDERBY_COLS", &nord, (ftnlen)16);
    if (*n < 1 || *n > nord) {
	chkin_("ZZEKQORD", (ftnlen)8);
	setmsg_("Column index # is out of valid range 1:#.", (ftnlen)41);
	errint_("#", n, (ftnlen)1);
	errint_("#", &nord, (ftnlen)1);
	sigerr_("SPICE(INVALIDINDEX)", (ftnlen)19);
	chkout_("ZZEKQORD", (ftnlen)8);
	return 0;
    }
    zzekreqi_(eqryi, "NUM_TABLES", &ntab, (ftnlen)10);
    zzekreqi_(eqryi, "NUM_CONJUNCTIONS", &ncnj, (ftnlen)16);
    zzekreqi_(eqryi, "NUM_CONSTRAINTS", &ncns, (ftnlen)15);
    zzekreqi_(eqryi, "CHR_BUF_SIZE", &buflen, (ftnlen)12);

/*     Get the Nth table and column from the query.  The table */
/*     descriptor lies beyond the fixed-size portion of the query, the */
/*     conjunction size list, and the constraint descriptors, as */
/*     well as the (N-1) previous order-by column descriptors. */

    base = ntab * 12 + 19 + ncnj + ncns * 26 + (*n - 1) * 13;

/*     Pick up the column name first. */

    cb = eqryi[base + 15];
    ce = eqryi[base + 16];
    if (cb > 0 && ce > 0 && cb <= buflen && ce <= buflen && cb <= ce) {
	s_copy(column, eqryc + (cb - 1), column_len, ce - (cb - 1));
    } else {

/*        We should never see invalid pointers in a parsed, encoded */
/*        query, but let's not take chances. */

	chkin_("ZZEKQORD", (ftnlen)8);
	setmsg_("Invalid string bounds #:# for column #.", (ftnlen)39);
	errint_("#", &cb, (ftnlen)1);
	errint_("#", &ce, (ftnlen)1);
	errint_("#", n, (ftnlen)1);
	sigerr_("SPICE(BUG)", (ftnlen)10);
	chkout_("ZZEKQORD", (ftnlen)8);
	return 0;
    }

/*     Same deal for the qualifying table or alias, except that the begin */
/*     pointer is set to zero if there's no name. */

    tb = eqryi[base + 9];
    te = eqryi[base + 10];
    if (tb > 0) {
	if (te > 0 && tb <= buflen && te <= buflen && tb <= te) {
	    s_copy(table, eqryc + (tb - 1), table_len, te - (tb - 1));
	} else {

/*           If the first pointer is non-zero, both pointers should have */
/*           been valid. */

	    chkin_("ZZEKQORD", (ftnlen)8);
	    setmsg_("Invalid string bounds #:# for the table qualifying colu"
		    "mn #.", (ftnlen)60);
	    errint_("#", &tb, (ftnlen)1);
	    errint_("#", &te, (ftnlen)1);
	    errint_("#", n, (ftnlen)1);
	    sigerr_("SPICE(BUG)", (ftnlen)10);
	    chkout_("ZZEKQORD", (ftnlen)8);
	    return 0;
	}
    } else {

/*        No table was supplied. */

	s_copy(table, " ", table_len, (ftnlen)1);
    }

/*     Set the order sense. */

    *sense = eqryi[base + 18];

/*     If names have been resolved already, we can determine the index */
/*     of the table to which the specified order-by column belongs. */

    zzekreqi_(eqryi, "NAMES_RESOLVED", &resolv, (ftnlen)14);
    if (resolv == 1) {

/*        The qualifying table's index in the FROM clause is available. */
/*        So is the index of the column within the table. */

	*tabidx = eqryi[base + 11];
	*colidx = eqryi[base + 17];
    } else {
	*tabidx = 0;
	*colidx = 0;
    }
    return 0;
} /* zzekqord_ */

