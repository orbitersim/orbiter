/* srfscc.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure SRFSCC (Surface string and body ID code to surface ID code) */
/* Subroutine */ int srfscc_(char *srfstr, integer *bodyid, integer *code, 
	logical *found, ftnlen srfstr_len)
{
    extern /* Subroutine */ int zzsrfn2c_(char *, integer *, integer *, 
	    logical *, ftnlen), chkin_(char *, ftnlen);
    extern logical failed_(void);
    extern /* Subroutine */ int nparsi_(char *, integer *, char *, integer *, 
	    ftnlen, ftnlen), chkout_(char *, ftnlen);
    char errmsg[80];
    extern logical return_(void);
    integer ptr;

/* $ Abstract */

/*     Translate a surface string, together with a body ID code, to the */
/*     corresponding surface ID code. The input surface string may */
/*     contain a name or an integer ID code. */

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

/*     DSK */
/*     NAIF_IDS */

/* $ Keywords */

/*     CONVERSION */
/*     DSK */
/*     ID */
/*     NAME */
/*     STRING */
/*     SURFACE */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     SRFSTR     I   Surface name or ID string. */
/*     BODYID     I   Body ID code. */
/*     CODE       O   Integer surface ID code. */
/*     FOUND      O   Flag indicating whether surface ID was found. */

/* $ Detailed_Input */

/*     SRFSTR   is a string designating a surface. SRFSTR may contain */
/*              a surface name or a string representation of the */
/*              surface's integer ID code. */

/*              If, for the body specified by BODYID, multiple surface */
/*              names are associated with one surface ID code, then any */
/*              of these names may be used as the value of SRFSTR. */

/*              Case and leading and trailing blanks in a surface name */
/*              are not significant. Sequences of consecutive embedded */
/*              blanks are considered equivalent to a single blank. */
/*              For example, all of the strings below are considered */
/*              to be equivalent: */

/*                 'MGS MOLA 128 PIXEL/DEG' */
/*                 'MGS MOLA 128 pixel/deg' */
/*                 'MGS MOLA 128 PIXEL/DEG   ' */
/*                 'MGS MOLA 128    PIXEL/DEG' */
/*                 '   MGS MOLA 128 PIXEL/DEG' */

/*              However, */

/*                 'MGSMOLA 128PIXEL/DEG' */

/*              is not equivalent to the names above. */


/*     BODYID   is the integer ID code of the body associated with the */
/*              surface designated by SRFSTR. */

/* $ Detailed_Output */

/*     CODE     is integer ID code of the surface designated by */
/*              SRFSTR, for the body designated by BODYID, if for this */
/*              body an association exists between the input surface */
/*              string and a surface ID code. CODE is defined if and */
/*              only if the output flag FOUND is .TRUE. */

/*     FOUND    is a logical flag that is .TRUE. if a surface code */
/*              corresponding to the input surface string and body ID */
/*              code was found. FOUND is .FALSE. otherwise. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the input surface string does not map to an ID code */
/*         and does not represent an integer, the output CODE is */
/*         undefined and the output FOUND is set to .FALSE. */

/*         This case is not treated as an error. */

/* $ Files */

/*     Surface name-to-ID mappings may be defined at run time by loading */
/*     text kernels containing kernel variable assignments of the form */

/*        NAIF_SURFACE_NAME += ( <surface name 1>, ... ) */
/*        NAIF_SURFACE_CODE += ( <surface code 1>, ... ) */
/*        NAIF_SURFACE_BODY += ( <body code 1>,    ... ) */

/*     Above, the Ith elements of the lists on the assignments' right */
/*     hand sides together define the Ith surface name/ID mapping. */

/*     The same effect can be achieved using assignments formatted as */
/*     follows: */

/*        NAIF_SURFACE_NAME += <surface name 1> */
/*        NAIF_SURFACE_CODE += <surface code 1> */
/*        NAIF_SURFACE_BODY += <body code 1> */

/*        NAIF_SURFACE_NAME += <surface name 2> */
/*        NAIF_SURFACE_CODE += <surface code 2> */
/*        NAIF_SURFACE_BODY += <body code 2> */

/*           ... */

/*     Note the use of the */

/*        += */

/*     operator; this operator appends to rather than overwrites the */
/*     kernel variable named on the left hand side of the assignment. */

/* $ Particulars */

/*     Surfaces are always associated with bodies (which usually are */
/*     ephemeris objects). For any given body, a mapping between surface */
/*     names and surface ID codes can be established. */

/*     Bodies serve to disambiguate surface names and ID codes: the set */
/*     of surface names and surface ID codes for a given body can be */
/*     thought of as belonging to a name space. A given surface ID code */
/*     or surface name may be used for surfaces of multiple bodies, */
/*     without conflict. */

/*     Associations between surface names and ID codes are always made */
/*     via kernel pool assignments; there are no built-in associations. */

/*     SRFSCC is one of four related subroutines: */

/*        SRFS2C      Surface string and body string to surface ID code */
/*        SRFSCC      Surface string and body ID code to surface ID code */
/*        SRFC2S      Surface ID code and body ID code to surface string */
/*        SRFCSS      Surface ID code and body string to surface string */

/*     SRFS2C, SRFC2S, SRFSCC, and SRFCSS perform translations between */
/*     surface strings and their corresponding integer ID codes. */

/*     Refer to naif_ids.req for details concerning adding new surface */
/*     name/code associations at run time by loading text kernels. */

/* $ Examples */

/*     The formatting of the results shown for this example may differ */
/*     across platforms. */

/*     1) Supposed a text kernel has been loaded that contains */
/*        the following assignments: */

/*           NAIF_SURFACE_NAME += ( 'MGS MOLA  64 pixel/deg', */
/*                                  'MGS MOLA 128 pixel/deg', */
/*                                  'PHOBOS GASKELL Q512'     ) */
/*           NAIF_SURFACE_CODE += (   1,   2,    1 ) */
/*           NAIF_SURFACE_BODY += ( 499, 499,  401 ) */

/*        Translate each surface string and body ID pair to the */
/*        associated surface ID code. Also perform a translation */
/*        for a surface name having no matching ID. */

/*        Use the meta-kernel shown below to define the required SPICE */
/*        kernel variables. */


/*           KPL/MK */

/*           File: srfscc_ex1.tm */

/*           This meta-kernel is intended to support operation of SPICE */
/*           example programs. The file contents shown here should not be */
/*           assumed to contain adequate or correct versions of data */
/*           required by SPICE-based user applications. */


/*           \begindata */

/*           NAIF_SURFACE_NAME += ( 'MGS MOLA  64 pixel/deg', */
/*                                  'MGS MOLA 128 pixel/deg', */
/*                                  'PHOBOS GASKELL Q512'     ) */
/*           NAIF_SURFACE_CODE += (   1,   2,    1 ) */
/*           NAIF_SURFACE_BODY += ( 499, 499,  401 ) */

/*           \begintext */


/*        Example code begins here. */


/*              PROGRAM SRFSCC_EX1 */
/*              IMPLICIT NONE */

/*              INCLUDE 'srftrn.inc' */

/*              INTEGER               FILSIZ */
/*              PARAMETER           ( FILSIZ = 255 ) */

/*              INTEGER               NCASE */
/*              PARAMETER           ( NCASE = 7 ) */

/*              INTEGER               BDNMLN */
/*              PARAMETER           ( BDNMLN = 36 ) */

/*              CHARACTER*(FILSIZ)    META */
/*              CHARACTER*(SFNMLN)    SRFSTR ( NCASE ) */

/*              INTEGER               BODYID ( NCASE ) */
/*              INTEGER               I */
/*              INTEGER               SURFID */

/*              LOGICAL               FOUND */


/*              DATA  ( SRFSTR(I), BODYID(I), I = 1, NCASE )  / */
/*             . */
/*             .        'MGS MOLA  64 pixel/deg',    499, */
/*             .        'PHOBOS GASKELL Q512',       401, */
/*             .        'MGS MOLA 128 pixel/deg',    499, */
/*             .        '1',                         499, */
/*             .        '1',                         401, */
/*             .        '2',                         499, */
/*             .        'ZZZ',                       499      / */


/*              META = 'srfscc_ex1.tm' */

/*              CALL FURNSH ( META ) */

/*              WRITE (*,*) ' ' */

/*              DO I = 1, NCASE */

/*                 CALL SRFSCC ( SRFSTR(I), BODYID(I), */
/*             .                 SURFID,    FOUND     ) */

/*                 WRITE (*,*) 'surface string   = ', SRFSTR(I) */
/*                 WRITE (*,*) 'body ID          = ', BODYID(I) */
/*                 WRITE (*,*) 'surface ID found = ', FOUND */

/*                 IF ( FOUND ) THEN */
/*                    WRITE (*,*) 'surface ID       = ', SURFID */
/*                 END IF */

/*                 WRITE (*,*) ' ' */

/*              END DO */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*         surface string   = MGS MOLA  64 pixel/deg */
/*         body ID          =          499 */
/*         surface ID found =  T */
/*         surface ID       =            1 */

/*         surface string   = PHOBOS GASKELL Q512 */
/*         body ID          =          401 */
/*         surface ID found =  T */
/*         surface ID       =            1 */

/*         surface string   = MGS MOLA 128 pixel/deg */
/*         body ID          =          499 */
/*         surface ID found =  T */
/*         surface ID       =            2 */

/*         surface string   = 1 */
/*         body ID          =          499 */
/*         surface ID found =  T */
/*         surface ID       =            1 */

/*         surface string   = 1 */
/*         body ID          =          401 */
/*         surface ID found =  T */
/*         surface ID       =            1 */

/*         surface string   = 2 */
/*         body ID          =          499 */
/*         surface ID found =  T */
/*         surface ID       =            2 */

/*         surface string   = ZZZ */
/*         body ID          =          499 */
/*         surface ID found =  F */


/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     B.V. Semenov       (JPL) */
/*     E.D. Wright        (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.1, 22-OCT-2021 (JDR) (NJB) */

/*        Edited the header to comply with NAIF standard. */

/*        Updated description of SRFSTR to indicate that any */
/*        surface name alias may be used. */

/* -    SPICELIB Version 1.0.0, 14-JAN-2016 (NJB) (EDW) (BVS) */

/* -& */
/* $ Index_Entries */

/*     surface string and body ID code to surface ID code */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     Local variables */

    if (return_()) {
	return 0;
    }
    chkin_("SRFSCC", (ftnlen)6);

/*     No result has been found yet. */

    *found = FALSE_;

/*     Try to translate the input surface name to a known surface ID */
/*     code. */

    zzsrfn2c_(srfstr, bodyid, code, found, srfstr_len);
    if (failed_()) {
	chkout_("SRFSCC", (ftnlen)6);
	return 0;
    }
    if (! (*found)) {

/*        The surface string could not be mapped to an ID code. */

/*        It's possible the name is a string representation of an */
/*        integer, for example, '999'.  If so, find the equivalent datum */
/*        of INTEGER type. */

	nparsi_(srfstr, code, errmsg, &ptr, srfstr_len, (ftnlen)80);

/*        If the string parsed as an integer, PTR is zero; otherwise */
/*        it's non-zero. */

	*found = ptr == 0;
    }

/*     CODE and FOUND are set. */

    chkout_("SRFSCC", (ftnlen)6);
    return 0;
} /* srfscc_ */

