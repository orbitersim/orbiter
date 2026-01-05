/* dascls.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__5000 = 5000;
static logical c_false = FALSE_;

/* $Procedure DASCLS ( DAS, close file ) */
/* Subroutine */ int dascls_(integer *handle)
{
    /* Initialized data */

    static logical pass1 = TRUE_;

    /* System generated locals */
    inlist ioin__1;

    /* Builtin functions */
    integer s_cmp(char *, char *, ftnlen, ftnlen), f_inqu(inlist *);

    /* Local variables */
    integer unit;
    extern /* Subroutine */ int zzddhhlu_(integer *, char *, logical *, 
	    integer *, ftnlen);
    extern logical elemi_(integer *, integer *);
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    static integer fhset[5006];
    extern logical failed_(void);
    extern /* Subroutine */ int dasham_(integer *, char *, ftnlen), dasllc_(
	    integer *), dashof_(integer *);
    char method[10];
    extern /* Subroutine */ int dassdr_(integer *), daswbr_(integer *), 
	    sigerr_(char *, ftnlen), chkout_(char *, ftnlen), setmsg_(char *, 
	    ftnlen);
    integer iostat;
    extern /* Subroutine */ int errint_(char *, integer *, ftnlen), ssizei_(
	    integer *, integer *);
    logical notscr;
    extern logical return_(void);

/* $ Abstract */

/*     Close a DAS file. */

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

/*     DAS */

/* $ Keywords */

/*     DAS */
/*     FILES */

/* $ Declarations */
/* $ Abstract */

/*     This file contains public, global parameter declarations */
/*     for the SPICELIB Direct Access Segregated (DAS) subsystem. */

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

/*     DAS */

/* $ Keywords */

/*     None. */

/* $ Restrictions */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 07-APR-2020 (JDR) */

/*        Added CHARDT, DPDT and INTDT parameters. */

/* -    SPICELIB Version 1.0.0, 10-FEB-2017 (NJB) */

/* -& */

/*     Parameter declarations follow. */


/*     DAS file table size: */

/*        The parameter name is FTSIZE. The value of the parameter is */
/*        defined in the include file */

/*           zzddhman.inc */

/*        That value is duplicated here, since zzddhman.inc contains */
/*        other declarations that conflict with some of those in DAS */
/*        routines. */


/*     Capacity of DAS data records: */

/*        -- NWD double precision numbers. */
/*        -- NWI integers. */
/*        -- NWC characters. */

/*     These parameters are named to enhance ease of maintenance of */
/*     the code; the values should not be changed. */

/*     DAS data type specifiers used in all DAS routines that require */
/*     a data type either as input or to extract data from an output */
/*     array. */

/*     CHARDT, */
/*     DPDT, */
/*     INTDT    are data type specifiers which indicate CHARACTER, */
/*              DOUBLE PRECISION, and INTEGER respectively. These */
/*              parameters are used in all DAS routines that require a */
/*              data type specifier. */


/*     End of include file das.inc */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     HANDLE     I   Handle of an open DAS file. */
/*     FTSIZE     P   Maximum number of simultaneously open DAS files. */

/* $ Detailed_Input */

/*     HANDLE   is the file handle of an open DAS file. */

/* $ Detailed_Output */

/*     None. */

/*     See $Particulars for a description of the effect of this routine. */

/* $ Parameters */

/*     All parameters described here are declared in the SPICELIB */
/*     include file das.inc. See that file for parameter values. */

/*     FTSIZE   is the maximum number of DAS files that can be */
/*              open at any one time. */

/* $ Exceptions */

/*     Error free. */

/*     1)  If HANDLE is not the handle of an open DAS file, no error */
/*         is signaled. */

/* $ Files */

/*     See the description of input argument HANDLE in $Detailed_Input. */

/* $ Particulars */

/*     This routine provides the primary recommended method of closing an */
/*     open DAS file. It is also possible to close a DAS file without */
/*     segregating it by calling DASWBR and DASLLC. Closing a DAS file by */
/*     any other means may cause the DAS mechanism for keeping track of */
/*     which files are open to fail. Closing a DAS file that has been */
/*     opened for writing by any other means may result in the production */
/*     of something other than a DAS file. */

/* $ Examples */

/*     The numerical results shown for these examples may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */

/*     1) Open a new DAS file called TEST.DAS, add 100 d.p. numbers */
/*        to it, and then close the file. */

/*        Example code begins here. */


/*              PROGRAM DASCLS_EX1 */
/*              IMPLICIT NONE */

/*              INTEGER               NMAX */
/*              PARAMETER           ( NMAX   = 100 ) */

/*              CHARACTER*(15)        FNAME */
/*              CHARACTER*(5)         FTYPE */
/*              CHARACTER*(15)        IFNAME */

/*              DOUBLE PRECISION      DDATA  ( NMAX ) */

/*              INTEGER               HANDLE */
/*              INTEGER               I */
/*              INTEGER               N */
/*              INTEGER               NCOMCH */

/*        C */
/*        C     We'll give the file the same internal file name */
/*        C     as the file's actual name.  We don't require any */
/*        C     comment records. */
/*        C */
/*              FNAME  = 'dascls_ex1.das' */
/*              FTYPE  = 'TEST' */
/*              IFNAME = FNAME */
/*              NCOMCH = 0 */

/*              WRITE(*,*) 'Opening the DAS file for writing...' */
/*              CALL DASONW ( FNAME, FTYPE,  FNAME, NCOMCH, HANDLE ) */

/*              DO I = 1, NMAX */
/*                 DDATA(I)  =  DBLE(I) */
/*              END DO */

/*              N = NMAX */

/*              WRITE(*,*) 'Adding the double precision numbers...' */
/*              CALL DASADD ( HANDLE, N, DDATA ) */

/*              WRITE(*,*) 'Closing the DAS file...' */
/*              CALL DASCLS ( HANDLE ) */

/*              WRITE(*,*) 'All ok.' */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*         Opening the DAS file for writing... */
/*         Adding the double precision numbers... */
/*         Closing the DAS file... */
/*         All ok. */


/*        Note that after run completion, a new DAS file exists in */
/*        the output directory. */


/*     2) Dump several parameters from the first DLA segment of a DSK */
/*        file. Note that DSK files are based on DAS. The segment is */
/*        assumed to be of type 2. */


/*        Example code begins here. */


/*              PROGRAM DASCLS_EX2 */
/*              IMPLICIT NONE */

/*              INCLUDE 'dla.inc' */
/*              INCLUDE 'dskdsc.inc' */
/*              INCLUDE 'dsk02.inc' */

/*        C */
/*        C     Local parameters */
/*        C */
/*              INTEGER               FILSIZ */
/*              PARAMETER           ( FILSIZ = 255 ) */

/*              INTEGER               LNSIZE */
/*              PARAMETER           ( LNSIZE = 80 ) */

/*        C */
/*        C     Local variables */
/*        C */
/*              CHARACTER*(FILSIZ)    DSK */
/*              CHARACTER*(LNSIZE)    OUTLIN */

/*              DOUBLE PRECISION      VOXORI ( 3 ) */
/*              DOUBLE PRECISION      VOXSIZ */
/*              DOUBLE PRECISION      VTXBDS ( 2, 3 ) */

/*              INTEGER               CGSCAL */
/*              INTEGER               DLADSC ( DLADSZ ) */
/*              INTEGER               HANDLE */
/*              INTEGER               NP */
/*              INTEGER               NV */
/*              INTEGER               NVXTOT */
/*              INTEGER               VGREXT ( 3 ) */
/*              INTEGER               VOXNPL */
/*              INTEGER               VOXNPT */
/*              INTEGER               VTXNPL */

/*              LOGICAL               FOUND */


/*        C */
/*        C     Prompt for the name of the DSK to read. */
/*        C */
/*              CALL PROMPT ( 'Enter DSK name > ', DSK ) */
/*        C */
/*        C     Open the DSK file for read access. */
/*        C     We use the DAS-level interface for */
/*        C     this function. */
/*        C */
/*              CALL DASOPR ( DSK, HANDLE ) */

/*        C */
/*        C     Begin a forward search through the */
/*        C     kernel, treating the file as a DLA. */
/*        C     In this example, it's a very short */
/*        C     search. */
/*        C */
/*              CALL DLABFS ( HANDLE, DLADSC, FOUND ) */

/*              IF ( .NOT. FOUND ) THEN */
/*        C */
/*        C        We arrive here only if the kernel */
/*        C        contains no segments.  This is */
/*        C        unexpected, but we're prepared for it. */
/*        C */
/*                 CALL SETMSG ( 'No segments found ' */
/*             .   //            'in DSK file #.'    ) */
/*                 CALL ERRCH  ( '#',  DSK           ) */
/*                 CALL SIGERR ( 'SPICE(NODATA)'     ) */

/*              END IF */

/*        C */
/*        C     If we made it this far, DLADSC is the */
/*        C     DLA descriptor of the first segment. */
/*        C */
/*        C     Read and display type 2 bookkeeping data. */
/*        C */
/*              CALL DSKB02 ( HANDLE, DLADSC, NV,     NP,     NVXTOT, */
/*             .              VTXBDS, VOXSIZ, VOXORI, VGREXT, CGSCAL, */
/*             .              VTXNPL, VOXNPT, VOXNPL                 ) */

/*        C */
/*        C     Show vertex and plate counts. */
/*        C */
/*              OUTLIN = 'Number of vertices:                 #' */
/*              CALL REPMI  ( OUTLIN, '#', NV, OUTLIN ) */
/*              CALL TOSTDO ( OUTLIN ) */

/*              OUTLIN = 'Number of plates:                   #' */
/*              CALL REPMI  ( OUTLIN, '#', NP, OUTLIN ) */
/*              CALL TOSTDO ( OUTLIN ) */

/*              OUTLIN = 'Voxel edge length (km):             #' */
/*              CALL REPMF  ( OUTLIN, '#', VOXSIZ, 6, 'E', OUTLIN ) */
/*              CALL TOSTDO ( OUTLIN ) */

/*              OUTLIN = 'Number of voxels:                   #' */
/*              CALL REPMI  ( OUTLIN, '#', NVXTOT, OUTLIN ) */
/*              CALL TOSTDO ( OUTLIN ) */

/*        C */
/*        C     Close the kernel.  This isn't necessary in a stand- */
/*        C     alone program, but it's good practice in subroutines */
/*        C     because it frees program and system resources. */
/*        C */
/*              CALL DASCLS ( HANDLE ) */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, using the DSK file named phobos512.bds, the output */
/*        was: */


/*        Enter DSK name > phobos512.bds */
/*        Number of vertices:                 1579014 */
/*        Number of plates:                   3145728 */
/*        Voxel edge length (km):             1.04248E-01 */
/*        Number of voxels:                   11914500 */


/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     K.R. Gehringer     (JPL) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 2.1.0, 12-OCT-2021 (JDR) (NJB) */

/*        Bug fix: added call to FAILED following call to DASHAM. */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. Created */
/*        complete code example #1 from existing code fragment */
/*        and added code example #2 using example from DSKB02. */

/*        Updated entries in the $Revisions section. */

/* -    SPICELIB Version 2.0.0, 10-FEB-2017 (NJB) */

/*        Updated to use DAF/DAS handle manager subsystem. */

/* -    SPICELIB Version 1.3.3, 05-OCT-2006 (NJB) */

/*        Corrected DASADD calling sequence error in code example. */
/*        Updated $Particulars header section to mention closing DAS */
/*        files without segregation via calls to DASWBR and DASLLC. */

/* -    SPICELIB Version 1.3.2, 24-MAR-2003 (NJB) */

/*        DASWBR call has been reinstated for scratch DAS case. */
/*        This call has the side effect of freeing buffer records */
/*        owned by the file DASWBR writes to. Failing to free these */
/*        records can cause write errors on HP/Fortran systems. */

/* -    SPICELIB Version 1.2.2, 27-FEB-2003 (NJB) */

/*        Tests whether file to be closed is a scratch DAS; if */
/*        so, buffer flushes and record segregation are omitted. */

/* -    SPICELIB Version 1.1.1, 26-OCT-1993 (KRG) */

/*        Removed references to specific DAS file open routines in the */
/*        $Detailed_Input section of the header. This was done in order */
/*        to minimize documentation changes if these open routines ever */
/*        change. */

/*        Modified the $Examples section to demonstrate the new ID word */
/*        format which includes a file type and to include a call to the */
/*        new routine DASONW, open new for write, which makes use of the */
/*        file type. Also,  a variable for the type of the file to be */
/*        created was added. */

/*        Changed the value of the parameter FTSIZE from 20 to 21. This */
/*        change makes the value of FTSIZE in DASCLS compatible with the */
/*        value in DASFM. See DASFM for a discussion of the reasons for */
/*        the increase in the value. */

/* -    SPICELIB Version 1.1.0, 08-JUL-1993 (NJB) */

/*        FHSET is now saved. */

/* -    SPICELIB Version 1.0.0, 30-JUN-1992 (NJB) (WLT) */

/* -& */
/* $ Index_Entries */

/*     close an open DAS file */

/* -& */
/* $ Revisions */

/* -    SPICELIB Version 1.3.2, 24-MAR-2003 (NJB) */

/*        DASWBR call has been reinstated for scratch DAS case. */
/*        This call has the side effect of freeing buffer records */
/*        owned by the file DASWBR writes to. Failing to free these */
/*        records can cause write errors on HP/Fortran systems. */

/* -    SPICELIB Version 1.2.2, 27-FEB-2003 (NJB) */

/*        Tests whether file to be closed is a scratch DAS; if */
/*        so, buffer flushes and record segregation are omitted. */

/* -    SPICELIB Version 1.1.1, 26-OCT-1993 (KRG) */

/*        Changed the value of the parameter FTSIZE from 20 to 21. This */
/*        change makes the value of FTSIZE in DASCLS compatible with the */
/*        value in DASFM. See DASFM for a discussion of the reasons for */
/*        the increase in the value. */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     Local variables */


/*     Saved variables */


/*     Initial values */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }
    chkin_("DASCLS", (ftnlen)6);
    if (pass1) {
	ssizei_(&c__5000, fhset);
	pass1 = FALSE_;
    }

/*     There are only four items on our worklist: */

/*        1)  Determine whether the file open for reading or writing, */
/*            and if it's open for writing, whether it's a scratch */
/*            file. */

/*        2)  If the DAS file is open for writing, flush any updated */
/*            records from the data buffers to the file. */

/*        3)  If the DAS file is open for writing, re-order the records */
/*            in the file so that the data is segregated by data type. */

/*        4)  Close the file. */


/*     See whether the input handle designates an open DAS file.  If not, */
/*     return now. */

    dashof_(fhset);
    if (! elemi_(handle, fhset)) {
	chkout_("DASCLS", (ftnlen)6);
	return 0;
    }

/*     If the file is open for writing, flush any buffered */
/*     records that belong to it. */

    dasham_(handle, method, (ftnlen)10);
    if (failed_()) {
	chkout_("DASCLS", (ftnlen)6);
	return 0;
    }
    if (s_cmp(method, "WRITE ", (ftnlen)10, (ftnlen)6) == 0) {

/*        Make sure that all buffered records belonging to the */
/*        indicated file are written out. */

	daswbr_(handle);

/*        We cannot directly test the status of the file, but if */
/*        the file is unnamed, it must be a scratch file. */

	zzddhhlu_(handle, "DAS", &c_false, &unit, (ftnlen)3);
	if (failed_()) {
	    chkout_("DASCLS", (ftnlen)6);
	    return 0;
	}
	ioin__1.inerr = 1;
	ioin__1.inunit = unit;
	ioin__1.infile = 0;
	ioin__1.inex = 0;
	ioin__1.inopen = 0;
	ioin__1.innum = 0;
	ioin__1.innamed = &notscr;
	ioin__1.inname = 0;
	ioin__1.inacc = 0;
	ioin__1.inseq = 0;
	ioin__1.indir = 0;
	ioin__1.infmt = 0;
	ioin__1.inform = 0;
	ioin__1.inunf = 0;
	ioin__1.inrecl = 0;
	ioin__1.innrec = 0;
	ioin__1.inblank = 0;
	iostat = f_inqu(&ioin__1);
	if (iostat != 0) {
	    setmsg_("Error occurred while performing an  INQUIRE on a DAS fi"
		    "le about to be closed.  IOSTAT = #. File handle was #.  "
		    "Logical unit was #.", (ftnlen)130);
	    errint_("#", &iostat, (ftnlen)1);
	    errint_("#", handle, (ftnlen)1);
	    errint_("#", &unit, (ftnlen)1);
	    sigerr_("SPICE(INQUIREFAILED)", (ftnlen)20);
	    chkout_("DASCLS", (ftnlen)6);
	    return 0;
	}
	if (notscr) {

/*           Segregate the data records in the file according to data */
/*           type. */

	    dassdr_(handle);
	}
    }

/*     Close the file. */

    dasllc_(handle);
    chkout_("DASCLS", (ftnlen)6);
    return 0;
} /* dascls_ */

