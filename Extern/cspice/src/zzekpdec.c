/* zzekpdec.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__11 = 11;
static integer c__20 = 20;
static integer c__0 = 0;

/* $Procedure      ZZEKPDEC ( EK, parse column declaration ) */
/* Subroutine */ int zzekpdec_(char *decl, integer *pardsc, ftnlen decl_len)
{
    /* Initialized data */

    static char attkey[32*5] = "DATATYPE                        " "SIZE     "
	    "                       " "INDEXED                         " "NUL"
	    "LS_OK                        " "FIXED_COUNT                     ";
    static integer reqkey[1] = { 1 };

    /* System generated locals */
    integer i__1, i__2, i__3;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer), s_cmp(char *, char *, 
	    ftnlen, ftnlen);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    integer i__, j, n;
    extern /* Subroutine */ int chkin_(char *, ftnlen), ucase_(char *, char *,
	     ftnlen, ftnlen), errch_(char *, char *, ftnlen, ftnlen);
    logical found;
    extern logical eqstr_(char *, char *, ftnlen, ftnlen);
    extern /* Subroutine */ int cleari_(integer *, integer *);
    logical attfnd[11];
    integer attloc[11], tokloc;
    extern /* Subroutine */ int lparsm_(char *, char *, integer *, integer *, 
	    char *, ftnlen, ftnlen, ftnlen), sigerr_(char *, ftnlen);
    char tokens[32*20];
    extern /* Subroutine */ int setmsg_(char *, ftnlen), chkout_(char *, 
	    ftnlen), cmprss_(char *, integer *, char *, char *, ftnlen, 
	    ftnlen, ftnlen), nparsi_(char *, integer *, char *, integer *, 
	    ftnlen, ftnlen);
    extern logical return_(void);
    char msg[320];
    integer ptr;

/* $ Abstract */

/*     Parse a declaration of a new EK column. */

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
/*     DECL       I   String containing column declaration. */
/*     PARDSC     O   Partial column descriptor. */

/* $ Detailed_Input */

/*     DECL           is a character string containing a column */
/*                    declaration.  Column declarations are strings that */
/*                    contain `keyword=value' assignments that define */
/*                    the attributes of the columns to which they apply. */

/*                    The attributes of a column defined by a */
/*                    declaration are: */

/*                       DATA TYPE */
/*                       <size> */
/*                       <is the column indexed?> */
/*                       <are null values allowed?> */
/*                       <is the column fixed-count?> */

/*                    The form of a column declaration is */

/*                      .'DATATYPE     = <type descriptor>,'           // */
/*                      .'[SIZE        = <size descriptor>],'          // */
/*                      .'[INDEXED     = <boolean>],'                  // */
/*                      .'[NULLS_OK    = <boolean>]'                   // */
/*                      .'[FIXED_COUNT = <boolean>]' */

/*                    The order of the assignments does not matter. */

/*                    Here <type descriptor> can be any of */

/*                       CHARACTER*<integer length> */
/*                       CHARACTER*(*) */
/*                       DOUBLE PRECISION */
/*                       INTEGER */

/*                    and the optional <size descriptor> can be either of */

/*                       <integer size> */
/*                       VARIABLE */

/*                    Character columns may not have both variable */
/*                    string length and variable size. */

/*                    The column entry size defaults to 1 if the size */
/*                    descriptor is omitted. */

/*                    The optional clauses using the INDEXED, NULLS_OK, */
/*                    and FIXED_COUNT keywords take the values */

/*                       TRUE */
/*                       FALSE */

/*                    on the right-hand-sides of the equal signs. */

/*                    The INDEXED clause indicates that the column is */
/*                    indexed.  If the clause is omitted, the column is */
/*                    not indexed.  Only scalar-valued columns can be */
/*                    indexed. */

/*                    The NULLS_OK indicates that null values are */
/*                    permitted in the column; if the clause is omitted, */
/*                    null values are not permitted in the column. */

/*                    The FIXED_COUNT clause indicates that the column */
/*                    has a fixed number of entries; no records may be */
/*                    added to or deleted from the column.  If any */
/*                    column in a segment has a fixed record count, all */
/*                    columns in the segment must have the FIXED_COUNT */
/*                    attribute. */

/*                    FIXED_COUNT columns may be loaded only by the */
/*                    fast load routines. */

/*                    Unless the FIXED_COUNT keyword is used, the column */
/*                    does not have a fixed record count. */

/*                    Commas are required to separate the assignments */
/*                    within declarations.  White space is optional. */
/*                    Case is not significant. */

/* $ Detailed_Output */

/*     PARDSC         is an integer array that specifies the attributes */
/*                    of the column.  PARDSC is basically a */
/*                    partially-filled-in column descriptor:  it */
/*                    doesn't contain any pointer information.  In the */
/*                    locations where a column descriptor would contain */
/*                    an index pointer or a null flag array pointer, */
/*                    PARDSC contains boolean values indicating whether */
/*                    these items are supposed to be filled in later. */

/*                    The elements of PARDSC that are filled in upon */
/*                    return from this routine are: */

/*                       -- Class.  The column class is automatically */
/*                          determined from the declared attributes. */

/*                       -- Data type. */

/*                       -- String length, if applicable.  Variable- */
/*                          length strings are represented by a length */
/*                          specification of IFALSE. */

/*                       -- Column entry size.  Variable-size entries */
/*                          are represented by a size specification of */
/*                          IFALSE. */

/*                       -- The column's index type.  This element, */
/*                          which in a normal column descriptor contains */
/*                          an index type code, takes the boolean value */
/*                          ITRUE if the column is indexed and IFALSE */
/*                          otherwise. */

/*                       -- The column's null flag.  This element takes */
/*                          the boolean value ITRUE if the column can */
/*                          contain null values and is set to IFALSE */
/*                          otherwise. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the input declaration does not conform to the specification */
/*         given in $Detailed_Input, the error SPICE(BADCOLUMNDECL) is */
/*         signaled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine is a utility that supports EK writing subroutines. */

/* $ Examples */

/*     1)  Parse a declaration of an indexed column of 80-character */
/*         strings, in which null values are allowed: */

/*            CALL ZZEKPDEC ( 'DATATYPE = CHARACTER*80, '   // */
/*           .                'SIZE     = 1,'               // */
/*           .                'INDEXED  = TRUE',            // */
/*           .                'NULLS_OK = TRUE', */
/*           .                 PARDSC                       ) */


/*        When ZZEKPDEC returns, the values of its output column */
/*        descriptor will be */

/*        When ZZEKPDEC returns, the value of its output argument */
/*        PARDSC will be */

/*           +---------------+ */
/*           |       3       |  Class */
/*           +---------------+ */
/*           |     <CHR>     |  Data type */
/*           +---------------+ */
/*           |      80       |  String length */
/*           +---------------+ */
/*           |       1       |  Size */
/*           +---------------+ */
/*           |       0       |  Base addres of column name (not yet set) */
/*           +---------------+ */
/*           |     ITRUE     |  Index type (ITRUE means col is indexed) */
/*           +---------------+ */
/*           |       0       |  Index pointer */
/*           +---------------+ */
/*           |     ITRUE     |  Null flag  (ITRUE means nulls are */
/*           +---------------+  allowed) */
/*           |       0       |  Ordinal position of column in segment */
/*           +---------------+ */
/*           |       0       |  Metadata pointer */
/*           +---------------+ */
/*           |       0       |  (Reserved) */
/*           +---------------+ */



/*     2)  Parse a declaration of a variable-size column of 80-character */
/*         strings: */

/*            CALL ZZEKPDEC ( 'DATATYPE =  CHARACTER*80, '   // */
/*           .                'SIZE     =  VARIABLE', */
/*           .                 PARDSC                        ) */

/*        When ZZEKPDEC returns, the value of its output argument */
/*        PARDSC will be */

/*           +---------------+ */
/*           |       3       |  Class */
/*           +---------------+ */
/*           |     <CHR>     |  Data type */
/*           +---------------+ */
/*           |      80       |  String length */
/*           +---------------+ */
/*           |    IFALSE     |  Size (IFALSE indicates variable size) */
/*           +---------------+ */
/*           |       0       |  Base addres of column name (not yet set) */
/*           +---------------+ */
/*           |     IFALSE    |  Index type (IFALSE means unindexed col) */
/*           +---------------+ */
/*           |       0       |  Index pointer */
/*           +---------------+ */
/*           |     IFALSE    |  Null flag  (IFALSE means nulls are not */
/*           +---------------+  allowed) */
/*           |       0       |  Ordinal position of column in segment */
/*           +---------------+ */
/*           |       0       |  Metadata pointer */
/*           +---------------+ */
/*           |       0       |  (Reserved) */
/*           +---------------+ */


/* $ Restrictions */

/*     1) Currently does not diagnose extraneous keyword assignments. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman   (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 14-SEP-2005 (NJB) */

/*        Bug fix:  several error handling logic blocks were */
/*        missing SIGERR calls; these have been corrected. */

/*        Bug fix:  No diagnostic was issued for a declaration */
/*        of a variable-size, variable-string-length column. */
/*        This has been corrected. */

/* -    Beta Version 1.0.0, 16-NOV-1995 (NJB) */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     Parameters naming indices of keywords in the attribute list */
/*     ATTKEY: */


/*     Local variables */


/*     Saved variables */


/*     Initial values */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("ZZEKPDEC", (ftnlen)8);
    }

/*     Start with a clean slate. */

    cleari_(&c__11, pardsc);

/*     Our declaration language has been cleverly designed so that the */
/*     characters */

/*        ',' */
/*        '=' */

/*     act as delimiters that LPARSM can make use */
/*     of.  LPARSM will hand us back a token list that contains these */
/*     pairs of consecutive tokens: */

/*        +----------------------+ */
/*        | CLASS                | */
/*        +----------------------+ */
/*        | <integer>            | */
/*        +----------------------+ */

/*        +----------------------+ */
/*        | DATATYPE             | */
/*        +----------------------+ */
/*        | <type>               | */
/*        +----------------------+ */

/*        +----------------------+ */
/*        | SIZE                 | */
/*        +----------------------+ */
/*        | <size specification> |  ( 'VARIABLE' or <integer> ) */
/*        +----------------------+ */

/*        +----------------------+ */
/*        | INDEXED              |  (fixed-size columns only, optional) */
/*        +----------------------+ */
/*        | <TRUE/FALSE>         | */
/*        +----------------------+ */

/*        +----------------------+ */
/*        | NULLS_OK             |  (optional) */
/*        +----------------------+ */
/*        | <TRUE/FALSE>         | */
/*        +----------------------+ */


/*     The order of the token pairs is not necessarily as shown. */


    lparsm_(decl, ",=", &c__20, &n, tokens, decl_len, (ftnlen)2, (ftnlen)32);

/*     Make sure the tokens are in upper case.  They are already */
/*     left-justified. */

    i__1 = n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	ucase_(tokens + (((i__2 = i__ - 1) < 20 && 0 <= i__2 ? i__2 : s_rnge(
		"tokens", i__2, "zzekpdec_", (ftnlen)453)) << 5), tokens + (((
		i__3 = i__ - 1) < 20 && 0 <= i__3 ? i__3 : s_rnge("tokens", 
		i__3, "zzekpdec_", (ftnlen)453)) << 5), (ftnlen)32, (ftnlen)
		32);
    }

/*     See which clauses are present in the declaration, and keep track */
/*     of the token indices of the keywords that start the clauses. */

    for (i__ = 1; i__ <= 5; ++i__) {
	attfnd[(i__1 = i__ - 1) < 11 && 0 <= i__1 ? i__1 : s_rnge("attfnd", 
		i__1, "zzekpdec_", (ftnlen)461)] = FALSE_;
    }
    i__1 = n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	j = 1;
	found = FALSE_;
	while(j <= 5 && ! found) {
	    if (s_cmp(tokens + (((i__2 = i__ - 1) < 20 && 0 <= i__2 ? i__2 : 
		    s_rnge("tokens", i__2, "zzekpdec_", (ftnlen)471)) << 5), 
		    attkey + (((i__3 = j - 1) < 5 && 0 <= i__3 ? i__3 : 
		    s_rnge("attkey", i__3, "zzekpdec_", (ftnlen)471)) << 5), (
		    ftnlen)32, (ftnlen)32) == 0) {
		found = TRUE_;
		attfnd[(i__2 = j - 1) < 11 && 0 <= i__2 ? i__2 : s_rnge("att"
			"fnd", i__2, "zzekpdec_", (ftnlen)473)] = TRUE_;
		attloc[(i__2 = j - 1) < 11 && 0 <= i__2 ? i__2 : s_rnge("att"
			"loc", i__2, "zzekpdec_", (ftnlen)474)] = i__;
	    } else {
		++j;
	    }
	}
    }

/*     Make sure we got the required keyword tokens we were expecting. */

    for (i__ = 1; i__ <= 1; ++i__) {
	if (! attfnd[(i__2 = reqkey[(i__1 = i__ - 1) < 1 && 0 <= i__1 ? i__1 :
		 s_rnge("reqkey", i__1, "zzekpdec_", (ftnlen)488)] - 1) < 11 
		&& 0 <= i__2 ? i__2 : s_rnge("attfnd", i__2, "zzekpdec_", (
		ftnlen)488)]) {
	    setmsg_("Required keyword # was not found in column declaration "
		    "#.", (ftnlen)57);
	    errch_("#", attkey + (((i__2 = reqkey[(i__1 = i__ - 1) < 1 && 0 <=
		     i__1 ? i__1 : s_rnge("reqkey", i__1, "zzekpdec_", (
		    ftnlen)492)] - 1) < 5 && 0 <= i__2 ? i__2 : s_rnge("attk"
		    "ey", i__2, "zzekpdec_", (ftnlen)492)) << 5), (ftnlen)1, (
		    ftnlen)32);
	    errch_("#", decl, (ftnlen)1, decl_len);
	    sigerr_("SPICE(BADCOLUMDECL)", (ftnlen)19);
	    chkout_("ZZEKPDEC", (ftnlen)8);
	    return 0;
	}
    }

/*     If we got this far, we can start to fill in the data type */
/*     descriptor.  Starting at the location of the DATATYPE keyword, */
/*     we should see one of the following token sequences: */

/*        DATATYPE  =  DOUBLE PRECISION */
/*        DATATYPE  =  INTEGER */
/*        DATATYPE  =  TIME */
/*        DATATYPE  =  CHARACTER*<integer> */
/*        DATATYPE  =  CHARACTER*(<integer>) */
/*        DATATYPE  =  CHARACTER** */
/*        DATATYPE  =  CHARACTER*(*) */

/*     The character declarations may have white space surrounding */
/*     the length specifier. */

/*     Find the location where the data type token should be. */

    tokloc = attloc[0] + 1;
    if (n < tokloc) {
	setmsg_("Column data type specification did not follow \"DATATYPE\" "
		"keyword in declaration #.", (ftnlen)82);
	errch_("#", decl, (ftnlen)1, decl_len);
	sigerr_("SPICE(BADCOLUMNDECL)", (ftnlen)20);
	chkout_("ZZEKPDEC", (ftnlen)8);
	return 0;
    }
    if (s_cmp(tokens + (((i__1 = tokloc - 1) < 20 && 0 <= i__1 ? i__1 : 
	    s_rnge("tokens", i__1, "zzekpdec_", (ftnlen)531)) << 5), "INTEGER"
	    , (ftnlen)32, (ftnlen)7) == 0) {
	pardsc[1] = 3;
	pardsc[2] = 1;
    } else if (eqstr_(tokens + (((i__1 = tokloc - 1) < 20 && 0 <= i__1 ? i__1 
	    : s_rnge("tokens", i__1, "zzekpdec_", (ftnlen)536)) << 5), "DOUB"
	    "LE PRECISION", (ftnlen)32, (ftnlen)16)) {
	pardsc[1] = 2;
	pardsc[2] = 1;
    } else if (eqstr_(tokens + (((i__1 = tokloc - 1) < 20 && 0 <= i__1 ? i__1 
	    : s_rnge("tokens", i__1, "zzekpdec_", (ftnlen)541)) << 5), "TIME",
	     (ftnlen)32, (ftnlen)4)) {
	pardsc[1] = 4;
	pardsc[2] = 1;
    } else if (s_cmp(tokens + (((i__1 = tokloc - 1) < 20 && 0 <= i__1 ? i__1 :
	     s_rnge("tokens", i__1, "zzekpdec_", (ftnlen)546)) << 5), "CHARA"
	    "CTER", (ftnlen)9, (ftnlen)9) == 0) {
	pardsc[1] = 1;

/*        To simplify picking up the length specification, compress */
/*        out blanks and parentheses.  This should leave us with */
/*        a token of the form */

/*           CHARACTER*<integer> */

/*        or */

/*           CHARACTER** */


	cmprss_(" ", &c__0, tokens + (((i__1 = tokloc - 1) < 20 && 0 <= i__1 ?
		 i__1 : s_rnge("tokens", i__1, "zzekpdec_", (ftnlen)561)) << 
		5), tokens + (((i__2 = tokloc - 1) < 20 && 0 <= i__2 ? i__2 : 
		s_rnge("tokens", i__2, "zzekpdec_", (ftnlen)561)) << 5), (
		ftnlen)1, (ftnlen)32, (ftnlen)32);
	cmprss_("(", &c__0, tokens + (((i__1 = tokloc - 1) < 20 && 0 <= i__1 ?
		 i__1 : s_rnge("tokens", i__1, "zzekpdec_", (ftnlen)562)) << 
		5), tokens + (((i__2 = tokloc - 1) < 20 && 0 <= i__2 ? i__2 : 
		s_rnge("tokens", i__2, "zzekpdec_", (ftnlen)562)) << 5), (
		ftnlen)1, (ftnlen)32, (ftnlen)32);
	cmprss_(")", &c__0, tokens + (((i__1 = tokloc - 1) < 20 && 0 <= i__1 ?
		 i__1 : s_rnge("tokens", i__1, "zzekpdec_", (ftnlen)563)) << 
		5), tokens + (((i__2 = tokloc - 1) < 20 && 0 <= i__2 ? i__2 : 
		s_rnge("tokens", i__2, "zzekpdec_", (ftnlen)563)) << 5), (
		ftnlen)1, (ftnlen)32, (ftnlen)32);
	if (*(unsigned char *)&tokens[(((i__1 = tokloc - 1) < 20 && 0 <= i__1 
		? i__1 : s_rnge("tokens", i__1, "zzekpdec_", (ftnlen)566)) << 
		5) + 9] != '*') {
	    setmsg_("Required asterisk missing from character column declara"
		    "tion:  #  in declaration:  #", (ftnlen)83);
	    errch_("#", tokens + (((i__1 = tokloc - 1) < 20 && 0 <= i__1 ? 
		    i__1 : s_rnge("tokens", i__1, "zzekpdec_", (ftnlen)571)) 
		    << 5), (ftnlen)1, (ftnlen)32);
	    errch_("#", decl, (ftnlen)1, decl_len);
	    sigerr_("SPICE(BADCOLUMNDECL)", (ftnlen)20);
	    chkout_("ZZEKPDEC", (ftnlen)8);
	    return 0;
	}
	if (*(unsigned char *)&tokens[(((i__1 = tokloc - 1) < 20 && 0 <= i__1 
		? i__1 : s_rnge("tokens", i__1, "zzekpdec_", (ftnlen)580)) << 
		5) + 10] == '*') {

/*           The string length is variable. */

	    pardsc[2] = -1;
	} else {

/*           The portion of the token following the asterisk should be a */
/*           string length. */

	    s_copy(msg, " ", (ftnlen)320, (ftnlen)1);
	    nparsi_(tokens + ((((i__1 = tokloc - 1) < 20 && 0 <= i__1 ? i__1 :
		     s_rnge("tokens", i__1, "zzekpdec_", (ftnlen)592)) << 5) 
		    + 10), &pardsc[2], msg, &ptr, (ftnlen)22, (ftnlen)320);
	    if (s_cmp(msg, " ", (ftnlen)320, (ftnlen)1) != 0) {
		setmsg_("String length specification # didn't parse as an in"
			"teger in declaration   #", (ftnlen)75);
		errch_("#", tokens + ((((i__1 = tokloc - 1) < 20 && 0 <= i__1 
			? i__1 : s_rnge("tokens", i__1, "zzekpdec_", (ftnlen)
			598)) << 5) + 10), (ftnlen)1, (ftnlen)22);
		errch_("#", decl, (ftnlen)1, decl_len);
		sigerr_("SPICE(BADCOLUMNDECL)", (ftnlen)20);
		chkout_("ZZEKPDEC", (ftnlen)8);
		return 0;
	    }
	}
    } else {

/*        The type specification is invalid. */

	setmsg_("Data type specification # is unrecognized in declaration #.",
		 (ftnlen)59);
	errch_("#", tokens + (((i__1 = tokloc - 1) < 20 && 0 <= i__1 ? i__1 : 
		s_rnge("tokens", i__1, "zzekpdec_", (ftnlen)614)) << 5), (
		ftnlen)1, (ftnlen)32);
	errch_("#", decl, (ftnlen)1, decl_len);
	sigerr_("SPICE(BADCOLUMNDECL)", (ftnlen)20);
	chkout_("ZZEKPDEC", (ftnlen)8);
	return 0;
    }

/*     Next, parse the size specification, if we have one.  If it's */
/*     valid, it's either the string 'VARIABLE' or it's an integer. */

    if (attfnd[1]) {
	tokloc = attloc[1] + 1;
	if (n < tokloc) {
	    setmsg_("Column size specification did not follow \"SIZE\" keywo"
		    "rd in declaration #.", (ftnlen)73);
	    errch_("#", decl, (ftnlen)1, decl_len);
	    sigerr_("SPICE(BADCOLUMNDECL)", (ftnlen)20);
	    chkout_("ZZEKPDEC", (ftnlen)8);
	    return 0;
	}
	if (s_cmp(tokens + (((i__1 = tokloc - 1) < 20 && 0 <= i__1 ? i__1 : 
		s_rnge("tokens", i__1, "zzekpdec_", (ftnlen)643)) << 5), 
		"VARIABLE", (ftnlen)32, (ftnlen)8) == 0) {

/*           Variable size entries are not allowed for CHARACTER*(*) */
/*           columns. */

	    if (pardsc[1] == 1) {
		if (pardsc[2] == -1) {
		    setmsg_("Column size specification was VARIABLE for a CH"
			    "ARACTER*(*) column in  declaration #.", (ftnlen)
			    84);
		    errch_("#", decl, (ftnlen)1, decl_len);
		    sigerr_("SPICE(BADCOLUMNDECL)", (ftnlen)20);
		    chkout_("ZZEKPDEC", (ftnlen)8);
		    return 0;
		}
	    }
	    pardsc[3] = -1;
	} else {
	    nparsi_(tokens + (((i__1 = tokloc - 1) < 20 && 0 <= i__1 ? i__1 : 
		    s_rnge("tokens", i__1, "zzekpdec_", (ftnlen)669)) << 5), &
		    pardsc[3], msg, &ptr, (ftnlen)32, (ftnlen)320);
	    if (s_cmp(msg, " ", (ftnlen)320, (ftnlen)1) != 0) {
		setmsg_("Column element size  specification # didn't parse a"
			"s an integer in in declaration #", (ftnlen)83);
		errch_("#", tokens + (((i__1 = tokloc - 1) < 20 && 0 <= i__1 ?
			 i__1 : s_rnge("tokens", i__1, "zzekpdec_", (ftnlen)
			676)) << 5), (ftnlen)1, (ftnlen)32);
		errch_("#", decl, (ftnlen)1, decl_len);
		sigerr_("SPICE(BADCOLUMNDECL)", (ftnlen)20);
		chkout_("ZZEKPDEC", (ftnlen)8);
		return 0;
	    }
	}
    } else {

/*        If the size is not specified, it defaults to 1. */

	pardsc[3] = 1;
    }

/*     The data type and entry size determine the column's class. */

    if (pardsc[1] == 1) {

/*        The character classes are 3 for scalars, 6 for arrays. */

	if (pardsc[3] == 1) {
	    pardsc[0] = 3;
	} else {
	    pardsc[0] = 6;
	}
    } else if (pardsc[1] == 3) {

/*        The integer classes are 1 for scalars, 4 for arrays. */

	if (pardsc[3] == 1) {
	    pardsc[0] = 1;
	} else {
	    pardsc[0] = 4;
	}
    } else if (pardsc[1] == 2 || pardsc[1] == 4) {

/*        The d.p. classes are 2 for scalars, 6 for arrays.  TIME */
/*        values are represented using d.p. classes as well. */

	if (pardsc[3] == 1) {
	    pardsc[0] = 2;
	} else {
	    pardsc[0] = 5;
	}
    }

/*     Parse the `NULLS_OK' clause, if we have one. */

    if (attfnd[3]) {
	tokloc = attloc[3] + 1;
	if (n < tokloc) {
	    setmsg_("Boolean value did not follow \"NULLS_OK\" keyword in de"
		    "claration #.", (ftnlen)65);
	    errch_("#", decl, (ftnlen)1, decl_len);
	    sigerr_("SPICE(BADCOLUMNDECL)", (ftnlen)20);
	    chkout_("ZZEKPDEC", (ftnlen)8);
	    return 0;
	}
	if (s_cmp(tokens + (((i__1 = tokloc - 1) < 20 && 0 <= i__1 ? i__1 : 
		s_rnge("tokens", i__1, "zzekpdec_", (ftnlen)751)) << 5), 
		"TRUE", (ftnlen)32, (ftnlen)4) == 0) {
	    pardsc[7] = 1;
	} else if (s_cmp(tokens + (((i__1 = tokloc - 1) < 20 && 0 <= i__1 ? 
		i__1 : s_rnge("tokens", i__1, "zzekpdec_", (ftnlen)755)) << 5)
		, "FALSE", (ftnlen)32, (ftnlen)5) == 0) {
	    pardsc[7] = -1;
	} else {
	    setmsg_("Invalid token # follows NULLS_OK keyword in declaration"
		    " #. ", (ftnlen)59);
	    errch_("#", tokens + (((i__1 = tokloc - 1) < 20 && 0 <= i__1 ? 
		    i__1 : s_rnge("tokens", i__1, "zzekpdec_", (ftnlen)763)) 
		    << 5), (ftnlen)1, (ftnlen)32);
	    errch_("#", decl, (ftnlen)1, decl_len);
	    sigerr_("SPICE(BADCOLUMNDECL)", (ftnlen)20);
	    chkout_("ZZEKPDEC", (ftnlen)8);
	    return 0;
	}
    } else {

/*        As a default, nulls are not allowed. */

	pardsc[7] = -1;
    }


/*     Parse the `INDEXED' clause, if we have one. */

    if (attfnd[2]) {
	tokloc = attloc[2] + 1;
	if (n < tokloc) {
	    setmsg_("Boolean value did not follow \"INDEXED\" keyword in dec"
		    "laration #.", (ftnlen)64);
	    errch_("#", decl, (ftnlen)1, decl_len);
	    sigerr_("SPICE(BADCOLUMNDECL)", (ftnlen)20);
	    chkout_("ZZEKPDEC", (ftnlen)8);
	    return 0;
	}
	if (s_cmp(tokens + (((i__1 = tokloc - 1) < 20 && 0 <= i__1 ? i__1 : 
		s_rnge("tokens", i__1, "zzekpdec_", (ftnlen)802)) << 5), 
		"TRUE", (ftnlen)32, (ftnlen)4) == 0) {

/*           If we have a fixed-size column whose size is 1, then it's */
/*           possible to index that column.  Otherwise, we should not */
/*           have an `INDEXED' clause. */

	    if (pardsc[3] != 1) {
		setmsg_("Non-scalar columns cannot be indexed. Declaration w"
			"as #.", (ftnlen)56);
		errch_("#", decl, (ftnlen)1, decl_len);
		sigerr_("SPICE(BADCOLUMNDECL)", (ftnlen)20);
		chkout_("ZZEKPDEC", (ftnlen)8);
		return 0;
	    }
	    pardsc[5] = 1;
	} else if (s_cmp(tokens + (((i__1 = tokloc - 1) < 20 && 0 <= i__1 ? 
		i__1 : s_rnge("tokens", i__1, "zzekpdec_", (ftnlen)821)) << 5)
		, "FALSE", (ftnlen)32, (ftnlen)5) == 0) {
	    pardsc[5] = -1;
	} else {
	    setmsg_("Invalid token # follows INDEXED keyword in declaration "
		    "#. ", (ftnlen)58);
	    errch_("#", tokens + (((i__1 = tokloc - 1) < 20 && 0 <= i__1 ? 
		    i__1 : s_rnge("tokens", i__1, "zzekpdec_", (ftnlen)829)) 
		    << 5), (ftnlen)1, (ftnlen)32);
	    errch_("#", decl, (ftnlen)1, decl_len);
	    sigerr_("SPICE(BADCOLUMNDECL)", (ftnlen)20);
	    chkout_("ZZEKPDEC", (ftnlen)8);
	    return 0;
	}
    } else {

/*        As a default, the column is not indexed. */

	pardsc[5] = -1;
    }

/*     Parse the `FIXED_COUNT' clause, if we have one. */

    if (attfnd[4]) {
	tokloc = attloc[4] + 1;
	if (n < tokloc) {
	    setmsg_("Boolean value did not follow \"FIXED_COUNT\" keyword in"
		    " declaration #.", (ftnlen)68);
	    errch_("#", decl, (ftnlen)1, decl_len);
	    sigerr_("SPICE(BADCOLUMNDECL)", (ftnlen)20);
	    chkout_("ZZEKPDEC", (ftnlen)8);
	    return 0;
	}
	if (s_cmp(tokens + (((i__1 = tokloc - 1) < 20 && 0 <= i__1 ? i__1 : 
		s_rnge("tokens", i__1, "zzekpdec_", (ftnlen)867)) << 5), 
		"TRUE", (ftnlen)32, (ftnlen)4) == 0) {

/*           The column is a fixed-count column.  Only scalar columns */
/*           are permitted to have fixed count.  We adjust the column */
/*           class to indicate fixed-count columns. */

	    if (pardsc[0] == 1) {

/*              Map scalar integers. */

		pardsc[0] = 7;
	    } else if (pardsc[0] == 2) {

/*              Map scalar d.p. numbers. */

		pardsc[0] = 8;
	    } else if (pardsc[0] == 3) {

/*              Map scalar strings. */

		pardsc[0] = 9;
	    } else {
		setmsg_("FIXED_COUNT attribute used in non-scalar column dec"
			"laration #. ", (ftnlen)63);
		errch_("#", decl, (ftnlen)1, decl_len);
		sigerr_("SPICE(BADCOLUMNDECL)", (ftnlen)20);
		chkout_("ZZEKPDEC", (ftnlen)8);
		return 0;
	    }
	} else if (s_cmp(tokens + (((i__1 = tokloc - 1) < 20 && 0 <= i__1 ? 
		i__1 : s_rnge("tokens", i__1, "zzekpdec_", (ftnlen)903)) << 5)
		, "FALSE", (ftnlen)32, (ftnlen)5) != 0) {

/*           No action is required if the FIXED_COUNT keyword is set */
/*           to FALSE, but no value other than FALSE or TRUE may appear */
/*           on the RHS. */

	    setmsg_("Invalid token # follows NULLS_OK keyword in declaration"
		    " #. ", (ftnlen)59);
	    errch_("#", tokens + (((i__1 = tokloc - 1) < 20 && 0 <= i__1 ? 
		    i__1 : s_rnge("tokens", i__1, "zzekpdec_", (ftnlen)911)) 
		    << 5), (ftnlen)1, (ftnlen)32);
	    errch_("#", decl, (ftnlen)1, decl_len);
	    sigerr_("SPICE(BADCOLUMNDECL)", (ftnlen)20);
	    chkout_("ZZEKPDEC", (ftnlen)8);
	    return 0;
	}
    }
    chkout_("ZZEKPDEC", (ftnlen)8);
    return 0;
} /* zzekpdec_ */

