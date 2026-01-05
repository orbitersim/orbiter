/* stdio.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure STDIO ( Standard IO ) */
/* Subroutine */ int stdio_(char *name__, integer *unit, ftnlen name_len)
{
    /* Builtin functions */
    integer s_cmp(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    extern /* Subroutine */ int chkin_(char *, ftnlen), ucase_(char *, char *,
	     ftnlen, ftnlen), errch_(char *, char *, ftnlen, ftnlen), ljust_(
	    char *, char *, ftnlen, ftnlen);
    char myname[8];
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), setmsg_(char *, ftnlen);
    extern logical return_(void);

/* $ Abstract */

/*     Return the logical unit associated with some standard input or */
/*     standard output. */

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

/*     INPUT-OUTPUT */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     NAME       I   is the name of a logical unit to return. */
/*     UNIT       O   is the logical unit associated with NAME. */

/* $ Detailed_Input */

/*     NAME     is the "name" of a FORTRAN unit to return. */
/*              Recognized names are 'STDIN' and 'STDOUT'. */
/*              The routine is case insensitive to NAME. */

/*              If NAME is not recognized the error */
/*              SPICE(BADSTDIONAME) is signaled and UNIT is */
/*              set to -100. */

/* $ Detailed_Output */

/*     UNIT     is the logical unit associated with NAME. If */
/*              NAME is not recognized, UNIT is set to -100. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If NAME is not recognized, the error SPICE(BADSTDIONAME) is */
/*         signaled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This is a low level utility for retrieving the logical units */
/*     associated with standard input and output. It exists to */
/*     isolate SPICE based code from compiler writer choices in the */
/*     implementation of standard input and output. */

/* $ Examples */

/*     Suppose you would like to send a message to standard output */
/*     and that this message is contained in the array of N character */
/*     strings MESSGE. The code below would handle the task. */

/*        CALL STDIO ( 'STDOUT', STDOUT ) */

/*        DO I = 1, N */
/*           CALL WRITLN ( MESSGE(I), STDOUT ) */
/*        END DO */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.1, 20-AUG-2021 (JDR) */

/*        Edited the header to comply with NAIF standard. Spelled out I/O */
/*        in $Keywords section. */

/* -    SPICELIB Version 1.0.0, 18-SEP-1996 (WLT) */

/* -& */
/* $ Index_Entries */

/*     logical units associated standard input and output */

/* -& */

/*     Spicelib Functions */


/*     Local Variables */

    ljust_(name__, myname, name_len, (ftnlen)8);
    ucase_(myname, myname, (ftnlen)8, (ftnlen)8);
    if (s_cmp(myname, "STDIN", (ftnlen)8, (ftnlen)5) == 0) {
	*unit = 5;
    } else if (s_cmp(myname, "STDOUT", (ftnlen)8, (ftnlen)6) == 0) {
	*unit = 6;
    } else if (return_()) {
	return 0;
    } else {
	chkin_("STDIO", (ftnlen)5);
	setmsg_("The only \"names\" recognized by STDIO are 'STDIN' and 'STD"
		"OUT' you requested a unit for '#'. ", (ftnlen)92);
	errch_("#", name__, (ftnlen)1, name_len);
	sigerr_("SPICE(BADSTDIONAME)", (ftnlen)19);
	chkout_("STDIO", (ftnlen)5);
    }
    return 0;
} /* stdio_ */

