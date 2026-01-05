/* zzscin01.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__100 = 100;

/* $Procedure ZZSCIN01 ( Private, SCLK database initialization, type 01 ) */
/* Subroutine */ int zzscin01_(integer *hdsclk, integer *scpool, integer *
	clklst, integer *dpfree, integer *ifree, integer *prvsc)
{
    extern /* Subroutine */ int zzhsiini_(integer *, integer *, integer *), 
	    cleari_(integer *, integer *);

/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines. Users should not call this routine directly due */
/*     to the volatile nature of this routine. */

/*     Initialize the SCLK type 1 database. */

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

/*     None. */

/* $ Declarations */

/*     Include file zzsclk01.inc */

/*     SPICE private file intended solely for the support of SPICE */
/*     routines. Users should not include this file directly due */
/*     to the volatile nature of this file. */

/*     Include file for SPICE-private declarations of SC01. */

/* -    Version 1.0.0, 11-NOV-2020 */


/*    Parameters copied from sclk.inc: */

/*        NOTE: these declarations are duplicated in the include file */
/*        sclk.inc. Those declarations must be kept in sync with */
/*        these ones. */


/*     Number of supported SCLK field delimiters: */


/*     Supported SCLK string field delimiters: */


/*     Time system codes */


/*     Maximum number of partitions: */


/*     Maximum number of coefficient records: */


/*     Maximum number of fields in an SCLK string: */


/*     Length of strings used to represent D.P. numbers: */


/*     End of duplicated declarations. */


/*     Indices of integer data items, relative to base index into */
/*     the integer data buffer: */


/*     Index of number of fields */


/*     Index of delimiter code */


/*     Index of time system code */


/*     Index of coefficient count. The count is 3 x the number of */
/*     coefficient records. */


/*     Index of the number of partitions */


/*     Index of the base index in the double precision buffer of the */
/*     coefficient set */


/*     Index of the base index in the double precision buffer of the */
/*     partition start times */


/*     Index of the base index in the double precision buffer of the */
/*     partition end times */


/*     Index of the base index in the double precision buffer of the */
/*     SCLK field moduli */


/*     Index of the base index in the double precision buffer of the */
/*     SCLK field offsets */


/*     Number of integer values per clock */


/*     Data structure parameters */


/*     Maximum number of clocks */


/*     DP buffer size */

/*     The buffer is large enough to hold data for one clock having the */
/*     maximum amount of data. */


/*     Integer buffer size */


/*     Lower bound of control area of singly linked list: */


/*     The add-only hash pool for frame IDs is singly linked. */


/*     End include file zzsc01.inc */

/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     HDSCLK     O   Array of heads of hash collision lists. */
/*     SCPOOL     O   Singly linked collision list pool. */
/*     CLKLST     O   Array of SCLK codes. */
/*     DPFREE     O   Index of first free entry in d.p. data buffer. */
/*     IFREE      O   Index of first free entry in integer data buffer. */
/*     PRVSC      O   Previous SCLK ID code. */

/* $ Detailed_Input */

/*     None. */

/* $ Detailed_Output */

/*     HDSCLK      is an array containing the head node indices of the */
/*                 SCLK ID collision lists stored in SCPOOL. On output, */
/*                 this array is zero-filled. */

/*     SCPOOL      is a singly linked list pool containing collision */
/*                 lists for sets of SCLK IDs that have the same hash */
/*                 values. On output, this data structure has the */
/*                 initial state set by ZZHSIINI. */

/*     CLKLST      is an array of SCLK IDs, which is parallel to the */
/*                 data portion of SCPOOL. Each allocated node of */
/*                 SCPOOL is the index in CLKLST of the associated */
/*                 SCLK ID. On output, this array is zero-filled. */

/*     DPFREE      is the index in the database's double precision */
/*                 buffer of the first free element. On output, this */
/*                 index is 1. */

/*     IFREE       is index in the database's integer buffer of the */
/*                 first free element. On output, this index is 1. */

/*     PRVSC       is the previous ID code passed to the SC01 subsystem. */
/*                 On output, this code is set to 0, which is never a */
/*                 valid SCLK ID code. */

/* $ Parameters */

/*     See the include file zzsc01.inc. */

/* $ Exceptions */

/*     Error free. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     None. */

/* $ Examples */

/*     None. See usage in SC01. */

/* $ Restrictions */

/*     This is a SPICE-private routine. It should not */
/*     be called directly by user application code. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman   (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 01-DEC-2021 (NJB) */

/* -& */
/* $ Index_Entries */

/*     initialize sclk type 1 database */

/* -& */
/* $ Revisions */

/*     None. */
/* ` */
/* -& */

/*     This routine must execute when SPICE error handling code is in */
/*     the process of responding to an error, so this routine doesn't */
/*     participate in SPICE error handling or tracing. The routines */
/*     called here are error-free. */

/*     Initialize SC01 data structures. */

/*     ZZHSIINI won't signal an error as long as the parameter MXNCLK */
/*     is positive, so we don't check FAILED here. */

    zzhsiini_(&c__100, hdsclk, scpool);
    cleari_(&c__100, clklst);
    *dpfree = 1;
    *ifree = 1;
    *prvsc = 0;
    return 0;
} /* zzscin01_ */

