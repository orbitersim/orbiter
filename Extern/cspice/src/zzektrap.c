/* zzektrap.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure      ZZEKTRAP ( EK tree, append item ) */
/* Subroutine */ int zzektrap_(integer *handle, integer *tree, integer *value,
	 integer *key)
{
    extern /* Subroutine */ int zzektrin_(integer *, integer *, integer *, 
	    integer *);
    extern integer zzektrsz_(integer *, integer *);

/* $ Abstract */

/*     Append an item to a tree.  The key indicating the location of */
/*     the new item is returned. */

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
/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     HANDLE     I   File handle. */
/*     TREE       I   Root of tree. */
/*     VALUE      I   Value to append. */
/*     KEY        O   Key pointing to new value. */

/* $ Detailed_Input */

/*     HANDLE         is a file handle of an EK open for write access. */

/*     TREE           is the root node number of the tree of interest. */

/*     VALUE          is an integer value to be appended to the */
/*                    specified tree. */

/* $ Detailed_Output */

/*     KEY            is an absolute key indicating the insertion */
/*                    location.  In EK trees, absolute keys are just */
/*                    ordinal positions relative to the leftmost element */
/*                    of the tree, with the leftmost element having */
/*                    position 1. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If HANDLE is invalid, the error will be diagnosed by routines */
/*         called by this routine.  The file will not be modified. */

/*     2)  If an I/O error occurs while reading or writing the indicated */
/*         file, the error will be diagnosed by routines called by this */
/*         routine. */

/* $ Files */

/*     See the EK Required Reading for a discussion of the EK file */
/*     format. */

/* $ Particulars */

/*     This routine appends a new value to an EK tree; this action is */
/*     equivalent to inserting the value at position (NKEYS+1), where */
/*     NKEYS is the number of keys in the tree prior to the insertion. */

/*     The tree is balanced and satisfies all invariants at the */
/*     completion of the appending. */

/* $ Examples */

/*     See EKBSEG. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     1)  Knuth, Donald E.  "The Art of Computer Programming, Volume */
/*         3/Sorting and Searching" 1973, pp 471-479. */

/*         EK trees are closely related to the B* trees described by */
/*         Knuth. */

/* $ Author_and_Institution */

/*     N.J. Bachman   (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.2, 22-SEP-2004 (EDW) */

/*        Edited 1.0.1 Version entry to not include */
/*        the token used to mark the $Procedure section. */

/* -    Beta Version 1.0.1, 14-OCT-1996 (NJB) */

/*        $Procedure line was corrected. */

/* -    Beta Version 1.0.0, 22-OCT-1995 (NJB) */

/* -& */

/*     Non-SPICELIB functions */

    *key = zzektrsz_(handle, tree) + 1;
    zzektrin_(handle, tree, key, value);
    return 0;
} /* zzektrap_ */

