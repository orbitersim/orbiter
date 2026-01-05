/* zzcln.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure      ZZCLN ( Private --- clean up ) */
/* Subroutine */ int zzcln_(integer *lookat, integer *nameat, integer *namlst,
	 integer *datlst, integer *nmpool, integer *chpool, integer *dppool)
{
    integer head, tail;
    extern /* Subroutine */ int chkin_(char *, ftnlen), lnkfsl_(integer *, 
	    integer *, integer *), chkout_(char *, ftnlen);

/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines.  Users should not call this routine directly due */
/*     to the volatile nature of this routine. */

/*     This routine cleans up changes to the kernel pool that were */
/*     made prior to the detection of a parsing error.  It is purely */
/*     a utility for use only by ZZRVAR. */

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

/*      None. */

/* $ Keywords */

/*       PRIVATE UTILITY */

/* $ Declarations */
/* $ Brief_I/O */

/*      VARIABLE  I/O  DESCRIPTION */
/*      --------  ---  -------------------------------------------------- */

/*      LOOKAT     I   The hash value of some name. */
/*      NAMEAT     I   The actual node where the name was stored */
/*      NAMLST    I/O  The array of heads of name lists. */
/*      DATLST    I/O  The array of heads of lists of values */
/*      NMPOOL    I/O  The linked list pool of variable names. */
/*      CHPOOL    I/O  The linked list pool of variable d.p. values. */
/*      DPPOOL    I/O  The linked list pool of variable string values. */


/* $ Detailed_Input */

/*     LOOKAT      is the hash value of some string.  NAMLST(LOOKAT) is */
/*                 the head of some collision resolution list of names. */

/*     NAMEAT      is the node in the list headed by NAMLST(LOOKAT) where */
/*                 some name has been stored in the kernel pool */
/*                 collection of NAMES. The node NAMEAT needs to be */
/*                 removed from its list in NMPOOL. */

/*     NAMLST      is an array of heads of collision */
/*                 resolution lists in NMPOOL.  If NAMLST(LOOKAT) is */
/*                 the same as NAMEAT, we need to adjust NAMLST(LOOKAT) */
/*                 so that it points to the next node in the list. */

/*     DATLST      is an array of heads of data value lists for the */
/*                 variables in the kernel pool.  We will need to free */
/*                 the data list pointed to by DATLST(NAMEAT) and */
/*                 zero out DATLST(NAMEAT). */

/*     NMPOOL      is a linked list pool for collision resolutions of */
/*                 a string hash function.  The node NAMEAT needs to */
/*                 be freed. */

/*     CHPOOL      is a linked list pool for string values associated */
/*                 with a kernel pool variable  If DATLST(NAMEAT) points */
/*                 into CHPOOL, then the list containing this node must */
/*                 be freed. */

/*     DPPOOL      is a linked list pool for d.p. values associated */
/*                 with a kernel pool variable. If DATLST(NAMEAT) points */
/*                 into DPPOOL, then the list containing this node must */
/*                 be freed. */


/* $ Detailed_Output */

/*      NAMLST    are the same structures as the input with the */
/*      DATLST    corrections made for the freeing of the NMPOOL */
/*      NMPOOL    node NAMEAT. */
/*      CHPOOL */
/*      DPPOOL */

/* $ Parameters */

/*      None. */

/* $ Files */

/*      None. */

/* $ Exceptions */

/*      None. */

/* $ Particulars */

/*     During the course of reading and parsing a kernel pool variable */
/*     it may happen that an error in the input text is encountered after */
/*     a kernel pool variable update has been initiated.  This routine */
/*     removes all traces of that variable from the kernel pool storage */
/*     structures. */

/* $ Examples */

/*     See ZZRVAR */

/* $ Restrictions */

/*     None. */

/* $ Author_and_Institution */

/*      W.L. Taber      (JPL) */

/* $ Literature_References */

/*      None. */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 20-SEP-1995 (WLT) */

/* -& */

/*     Local Parameters and Variables */


/*     First perform the clean up function. This variable */
/*     has been corrupted so there's no point in hanging */
/*     on to it. */

/*     First remove the data... */

    chkin_("ZZCLN", (ftnlen)5);
    head = datlst[*nameat - 1];
    if (head < 0) {
	head = -head;
	tail = -chpool[(head << 1) + 11];
	lnkfsl_(&head, &tail, chpool);
    } else if (head > 0) {
	tail = -dppool[(head << 1) + 11];
	lnkfsl_(&head, &tail, dppool);
    }

/*     Remove the sub-list head from the data list. */

    datlst[*nameat - 1] = 0;

/*     If this was a singleton list remove the pointer to */
/*     the head of the list. */

    head = namlst[*lookat - 1];
    tail = -nmpool[(head << 1) + 11];
    if (head == tail) {
	namlst[*lookat - 1] = 0;
    } else if (namlst[*lookat - 1] == *nameat) {
	namlst[*lookat - 1] = nmpool[(*nameat << 1) + 10];
    }

/*     Finally free up this node in the NMPOOL. */

    head = *nameat;
    tail = *nameat;
    lnkfsl_(&head, &tail, nmpool);
    chkout_("ZZCLN", (ftnlen)5);
    return 0;
} /* zzcln_ */

