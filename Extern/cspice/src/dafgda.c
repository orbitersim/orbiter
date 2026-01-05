/* dafgda.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure DAFGDA ( DAF, read data from address ) */
/* Subroutine */ int dafgda_(integer *handle, integer *baddr, integer *eaddr, 
	doublereal *data)
{
    /* System generated locals */
    integer i__1, i__2;

    /* Local variables */
    integer begr, begw, endr, endw, last, next;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    integer recno;
    logical found;
    integer first;
    extern /* Subroutine */ int dafgdr_(integer *, integer *, integer *, 
	    integer *, doublereal *, logical *), cleard_(integer *, 
	    doublereal *), dafarw_(integer *, integer *, integer *), sigerr_(
	    char *, ftnlen), chkout_(char *, ftnlen), setmsg_(char *, ftnlen),
	     errint_(char *, integer *, ftnlen);
    extern logical return_(void);

/* $ Abstract */

/*     Read the double precision data bounded by two addresses within */
/*     a DAF. */

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

/*     DAF */

/* $ Keywords */

/*     FILES */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     HANDLE     I   Handle of a DAF. */
/*     BADDR, */
/*     EADDR      I   Initial, final address within file. */
/*     DATA       O   Data contained between BADDR and EADDR. */

/* $ Detailed_Input */

/*     HANDLE   is the handle of a DAF. */

/*     BADDR, */
/*     EADDR    are the initial and final addresses of a contiguous */
/*              set of double precision numbers within a DAF. */
/*              Presumably, these make up all or part of a particular */
/*              array. */

/* $ Detailed_Output */

/*     DATA     are the double precision data contained between */
/*              the specified addresses within the specified file. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If BADDR is zero or negative, the error SPICE(DAFNEGADDR) */
/*         is signaled. */

/*     2)  If BADDR > EADDR, the error SPICE(DAFBEGGTEND) is signaled. */

/*     3)  If HANDLE is invalid, an error is signaled by a routine in the */
/*         call tree of this routine. */

/*     4)  If the range of addresses covered between BADDR and EADDR */
/*         includes records that do not contain strictly double */
/*         precision data, then the values returned in DATA are */
/*         undefined. See the $Restrictions section below for details. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     The principal reason that DAFs are so easy to use is that */
/*     the data in each DAF are considered to be one long contiguous */
/*     set of double precision numbers. You can grab data from anywhere */
/*     within a DAF without knowing (or caring) about the physical */
/*     records in which they are stored. */

/*     This routine replaces DAFRDA as the principle mechanism for */
/*     reading the contents of DAF arrays. */

/* $ Examples */

/*     The numerical results shown for this example may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */

/*     1) Open a type 8 SPK for read access, retrieve the data for */
/*        the first segment and identify the beginning and end addresses, */
/*        the number of data elements within, the size of the data */
/*        array, and print the first two records. */

/*        Use the SPK kernel below as input type 8 SPK file for the */
/*        example. */

/*           mer1_ls_040128_iau2000_v1.bsp */

/*        Each segment contains only two records which provide the start */
/*        and end position for the MER-1 rover landing site in the */
/*        IAU_MARS frame. Since the landing site does not change over */
/*        time, it is expected that both records are equal. */


/*        Example code begins here. */


/*              PROGRAM DAFGDA_EX1 */
/*              IMPLICIT NONE */

/*        C */
/*        C     Local constants. */
/*        C */
/*              CHARACTER*(*)         FMT */
/*              PARAMETER           ( FMT = '(6F10.3)' ) */

/*              INTEGER               MAXDAT */
/*              PARAMETER           ( MAXDAT = 1000 ) */

/*              INTEGER               MAXSUM */
/*              PARAMETER           ( MAXSUM = 125  ) */

/*              INTEGER               ND */
/*              PARAMETER           ( ND     = 2    ) */

/*              INTEGER               NI */
/*              PARAMETER           ( NI     = 6    ) */

/*        C */
/*        C     Local variables. */
/*        C */
/*              DOUBLE PRECISION      DAFSUM ( MAXSUM ) */
/*              DOUBLE PRECISION      DATA   ( MAXDAT ) */
/*              DOUBLE PRECISION      DC     ( ND ) */

/*              INTEGER               BADDR */
/*              INTEGER               EADDR */
/*              INTEGER               HANDLE */
/*              INTEGER               IC     ( NI ) */
/*              INTEGER               SIZE */

/*              LOGICAL               FOUND */

/*        C */
/*        C     Open the type 8 SPK for read access, then read the */
/*        C     data from the first segment. */
/*        C */
/*              CALL DAFOPR ( 'mer1_ls_040128_iau2000_v1.bsp', HANDLE ) */

/*        C */
/*        C     Begin a forward search; find the first segment; read the */
/*        C     segment summary. */
/*        C */
/*              CALL DAFBFS ( HANDLE ) */
/*              CALL DAFFNA ( FOUND  ) */
/*              CALL DAFGS  ( DAFSUM ) */
/*              CALL DAFUS  ( DAFSUM, ND, NI, DC, IC ) */

/*        C */
/*        C     Retrieve the data begin and end addresses. */
/*        C */
/*              BADDR = IC(5) */
/*              EADDR = IC(6) */

/*              WRITE(*,'(A,I4)') 'Beginning address       : ', BADDR */
/*              WRITE(*,'(A,I4)') 'Ending address          : ', EADDR */
/*              WRITE(*,'(A,I4)') 'Number of data elements : ', */
/*             .                                    EADDR - BADDR + 1 */

/*        C */
/*        C     Extract all data bounded by the begin and end addresses. */
/*        C */
/*              CALL DAFGDA ( HANDLE, BADDR, EADDR, DATA ) */

/*        C */
/*        C     Check the data. */
/*        C */
/*              WRITE(*,'(A)') 'The first and second states ' */
/*             .            // 'stored in the segment:' */
/*              WRITE(*,FMT) DATA(1),  DATA(2),  DATA(3), */
/*             .             DATA(4),  DATA(5),  DATA(6) */
/*              WRITE(*,FMT) DATA(7),  DATA(8),  DATA(9), */
/*             .             DATA(10), DATA(11), DATA(12) */

/*        C */
/*        C     Safely close the file */
/*        C */
/*              CALL DAFCLS ( HANDLE ) */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*        Beginning address       :  897 */
/*        Ending address          :  912 */
/*        Number of data elements :   16 */
/*        The first and second states stored in the segment: */
/*          3376.422  -326.649  -115.392     0.000     0.000     0.000 */
/*          3376.422  -326.649  -115.392     0.000     0.000     0.000 */


/* $ Restrictions */

/*     1)  There are several types of records in a DAF. This routine */
/*         is only to be used to read double precision data bounded */
/*         between two DAF addresses. The range of addresses input */
/*         may not cross data and summary record boundaries. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     F.S. Turner        (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 13-AUG-2021 (JDR) */

/*        Changed the input argument names BEGIN and END to BADDR to */
/*        EADDR for consistency with other routines. */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. Added */
/*        complete code example. Removed unnecessary $Revisions section. */

/* -    SPICELIB Version 1.0.0, 16-NOV-2001 (FST) */

/* -& */
/* $ Index_Entries */

/*     read data from DAF address */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }

/*     Bad addresses? */

    if (*baddr <= 0) {
	chkin_("DAFGDA", (ftnlen)6);
	setmsg_("Negative value for BADDR address: #", (ftnlen)35);
	errint_("#", baddr, (ftnlen)1);
	sigerr_("SPICE(DAFNEGADDR)", (ftnlen)17);
	chkout_("DAFGDA", (ftnlen)6);
	return 0;
    } else if (*baddr > *eaddr) {
	chkin_("DAFGDA", (ftnlen)6);
	setmsg_("Beginning address (#) greater than ending address (#).", (
		ftnlen)54);
	errint_("#", baddr, (ftnlen)1);
	errint_("#", eaddr, (ftnlen)1);
	sigerr_("SPICE(DAFBEGGTEND)", (ftnlen)18);
	chkout_("DAFGDA", (ftnlen)6);
	return 0;
    }

/*     Convert raw addresses to record/word representations. */

    dafarw_(baddr, &begr, &begw);
    dafarw_(eaddr, &endr, &endw);

/*     Get as many records as needed. Return the last part of the */
/*     first record, the first part of the last record, and all of */
/*     every record in between. Any record not found is assumed to */
/*     be filled with zeros. */

    next = 1;
    i__1 = endr;
    for (recno = begr; recno <= i__1; ++recno) {
	if (begr == endr) {
	    first = begw;
	    last = endw;
	} else if (recno == begr) {
	    first = begw;
	    last = 128;
	} else if (recno == endr) {
	    first = 1;
	    last = endw;
	} else {
	    first = 1;
	    last = 128;
	}
	dafgdr_(handle, &recno, &first, &last, &data[next - 1], &found);
	if (! found) {
	    i__2 = last - first + 1;
	    cleard_(&i__2, &data[next - 1]);
	}
	next += last - first + 1;
    }
    return 0;
} /* dafgda_ */

