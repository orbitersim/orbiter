/* eknseg.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;

/* $Procedure EKNSEG ( EK, number of segments in file ) */
integer eknseg_(integer *handle)
{
    /* System generated locals */
    integer ret_val, i__1, i__2;

    /* Local variables */
    integer base, tree;
    extern /* Subroutine */ int zzekpgch_(integer *, char *, ftnlen);
    extern integer zzektrbs_(integer *), zzektrsz_(integer *, integer *);
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    extern logical failed_(void);
    extern /* Subroutine */ int dasrdi_(integer *, integer *, integer *, 
	    integer *), chkout_(char *, ftnlen);
    extern logical return_(void);

/* $ Abstract */

/*     Return the number of segments in a specified EK. */

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
/*     UTILITY */

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


/*     Include Section:  EK File Metadata Parameters */

/*        ekfilpar.inc  Version 1  28-MAR-1995 (NJB) */

/*     These parameters apply to EK files using architecture 4. */
/*     These files use a paged DAS file as their underlying file */
/*     structure. */

/*     The metadata for an architecture 4 EK file is very simple:  it */
/*     consists of a single integer, which is a pointer to a tree */
/*     that in turn points to the segments in the EK.  However, in the */
/*     interest of upward compatibility, one integer page is reserved */
/*     for the file's metadata. */


/*     Size of file parameter block: */


/*     All offsets shown below are relative to the beginning of the */
/*     first integer page in the EK. */


/*     Index of the segment pointer tree---this location contains the */
/*     root page number of the tree: */


/*     End Include Section:  EK File Metadata Parameters */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     HANDLE     I   EK file handle. */

/*     The function returns the number of segments in the specified */
/*     E-kernel. */

/* $ Detailed_Input */

/*     HANDLE   is the handle of an EK file opened for read access. */

/* $ Detailed_Output */

/*     The function returns the number of segments in the EK identified */
/*     by HANDLE. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If HANDLE is invalid, an error is signaled by a routine in the */
/*         call tree of this routine. EKNSEG will return the value zero. */

/*     2)  If an I/O error occurs while trying to read the EK, the error */
/*         is signaled by a routine in the call tree of this routine. */
/*         EKNSEG will return the value zero. */

/* $ Files */

/*     See the description of HANDLE in $Detailed_Input. */

/* $ Particulars */

/*     This routine is used to support the function of summarizing an */
/*     EK file. Given the number of segments in the file, a program */
/*     can use EKSSUM in a loop to summarize each of them. */

/* $ Examples */

/*     The numerical results shown for this example may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */

/*     1) Find the number of segments on an EK. */

/*        Use the EK kernel below as test input file for loading the */
/*        experiment database. This kernel contains the Deep */
/*        Impact spacecraft sequence data based on the integrated */
/*        Predicted Events File covering the whole primary mission. */

/*           dif_seq_050112_050729.bes */


/*        Example code begins here. */


/*              PROGRAM EKNSEG_EX1 */
/*              IMPLICIT NONE */

/*        C */
/*        C     SPICELIB functions */
/*        C */
/*              INTEGER               EKNSEG */

/*        C */
/*        C     Local variables. */
/*        C */
/*              INTEGER               HANDLE */
/*              INTEGER               NSEG */

/*        C */
/*        C     Open the EK file, returning the file handle */
/*        C     associated with the open file to the variable named */
/*        C     HANDLE. */
/*        C */
/*              CALL EKOPR ( 'dif_seq_050112_050729.bes', HANDLE ) */


/*        C */
/*        C     Return the number of segments in the EK. */
/*        C */
/*              NSEG = EKNSEG( HANDLE ) */
/*              WRITE(*,*) 'Number of segments = ', NSEG */

/*        C */
/*        C     Close the file. */
/*        C */
/*              CALL EKCLS ( HANDLE ) */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*         Number of segments =            2 */


/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 25-MAY-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. */
/*        Added complete code example. */

/* -    SPICELIB Version 1.0.0, 26-SEP-1995 (NJB) */

/* -& */
/* $ Index_Entries */

/*     return number of segments in an E-kernel */

/* -& */

/*     SPICELIB functions */


/*     Non-SPICELIB functions */


/*     Local variables */


/*     Set a default value for EKNSEG. */

    ret_val = 0;

/*     Standard SPICE error handling. */

    if (return_()) {
	return ret_val;
    } else {
	chkin_("EKNSEG", (ftnlen)6);
    }

/*     Make sure this is a paged DAS EK. */

    zzekpgch_(handle, "READ", (ftnlen)4);
    if (failed_()) {
	chkout_("EKNSEG", (ftnlen)6);
	return ret_val;
    }

/*     Obtain the base address of the first integer page. */

    base = zzektrbs_(&c__1);

/*     Look up the head node of the segment tree. */

    i__1 = base + 1;
    i__2 = base + 1;
    dasrdi_(handle, &i__1, &i__2, &tree);

/*     Get the entry count for the segment tree. */

    ret_val = zzektrsz_(handle, &tree);
    chkout_("EKNSEG", (ftnlen)6);
    return ret_val;
} /* eknseg_ */

