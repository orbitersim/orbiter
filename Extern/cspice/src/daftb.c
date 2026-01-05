/* daftb.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__9 = 9;
static integer c__1 = 1;
static integer c__2 = 2;
static integer c__0 = 0;
static integer c__3 = 3;

/* $Procedure DAFTB ( DAF, convert transfer file to binary file ) */
/* Subroutine */ int daftb_(integer *xfrlun, char *binfil, ftnlen binfil_len)
{
    /* System generated locals */
    integer i__1;
    cilist ci__1;

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_rsle(cilist *), do_lio(integer *, integer *, char *, ftnlen), 
	    e_rsle(void), s_cmp(char *, char *, ftnlen, ftnlen), s_rsfe(
	    cilist *), do_fio(integer *, char *, ftnlen), e_rsfe(void);

    /* Local variables */
    char name__[1000];
    integer barr;
    char line[255];
    integer bcnt, earr, ecnt;
    logical more;
    char word[255], rest[255];
    extern /* Subroutine */ int chkin_(char *, ftnlen), dafps_(integer *, 
	    integer *, doublereal *, integer *, doublereal *);
    char tarch[8];
    extern /* Subroutine */ int errch_(char *, char *, ftnlen, ftnlen);
    logical inarr;
    char ttype[8];
    extern /* Subroutine */ int idw2at_(char *, char *, char *, ftnlen, 
	    ftnlen, ftnlen), dafada_(doublereal *, integer *), dafbna_(
	    integer *, doublereal *, char *, ftnlen), dafena_(void);
    integer nd;
    extern logical failed_(void);
    integer ni;
    extern /* Subroutine */ int dafcls_(integer *);
    char ifname[60];
    integer binhdl;
    extern /* Subroutine */ int rdencd_(integer *, integer *, doublereal *), 
	    rdenci_(integer *, integer *, integer *), dafopn_(char *, integer 
	    *, integer *, char *, integer *, integer *, ftnlen, ftnlen);
    doublereal buffer[1024];
    integer dtacnt;
    extern /* Subroutine */ int dafonw_(char *, char *, integer *, integer *, 
	    char *, integer *, integer *, ftnlen, ftnlen, ftnlen);
    char idword[8];
    integer arrcnt, numdta;
    extern /* Subroutine */ int errfnm_(char *, integer *, ftnlen);
    integer snmlen;
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen);
    char errmsg[320];
    extern /* Subroutine */ int nparsi_(char *, integer *, char *, integer *, 
	    ftnlen, ftnlen);
    integer iostat, numarr, numlft;
    extern /* Subroutine */ int setmsg_(char *, ftnlen), errint_(char *, 
	    integer *, ftnlen), nextwd_(char *, char *, char *, ftnlen, 
	    ftnlen, ftnlen);
    integer lftovr;
    extern logical return_(void);
    integer errptr;
    doublereal dsumry[125];
    integer isumry[250];
    doublereal summry[125];

    /* Fortran I/O blocks */
    static cilist io___5 = { 1, 0, 1, 0, 0 };
    static cilist io___9 = { 1, 0, 1, 0, 0 };
    static cilist io___27 = { 1, 0, 1, 0, 0 };
    static cilist io___32 = { 1, 0, 1, 0, 0 };


/* $ Abstract */

/*     Convert the contents of an DAF transfer file into an equivalent */
/*     binary DAF file. */

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

/*     CONVERSION */
/*     FILES */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     XFRLUN     I   Logical unit of an open DAF transfer file. */
/*     BINFIL     I   Name of a binary DAF file to be created. */

/* $ Detailed_Input */

/*     XFRLUN   is the Fortran logical unit number of a previously opened */
/*              DAF transfer file has been. */

/*              The file pointer should be positioned ready to read */
/*              the file ID word. */

/*     BINFIL   is the name of the binary DAF file to be created. */

/* $ Detailed_Output */

/*     None. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the DAF transfer file cannot be read, the error */
/*         SPICE(FILEREADFAILED) is signaled. */

/*     2)  If the architecture of the file is not DAF, as specified by */
/*         the ID word, the error SPICE(NOTADAFFILE) is signaled. */

/*     3)  If an error occurs while attempting to decode data in the DAF */
/*         transfer file, the error SPICE(BADDAFTRANSFERFILE) is */
/*         signaled. */

/*     4)  If the DAF file cannot be written, an error is signaled by a */
/*         routine in the call tree of this routine. */

/*     5)  The binary DAF file opened by this routine, BINFIL, is only */
/*         GUARANTEED to be closed upon successful completion of the */
/*         transfer file to binary file conversion process. In the event */
/*         of an error, the caller of this routine is required to close */
/*         the binary DAF file BINFIL. */

/* $ Files */

/*     See arguments XFRLUN, BINFIL. */

/* $ Particulars */

/*     Any binary DAF file may be transferred between heterogeneous */
/*     Fortran environments by converting it to an equivalent file */
/*     containing only ASCII characters. Such a file can be transferred */
/*     almost universally, using any number of established protocols. */
/*     Once transferred, the ASCII file can be converted to a binary */
/*     file, using the representations native to the new host */
/*     environment. */

/*     This routine provides a mechanism for converting an DAF transfer */
/*     file created by DAFBT, or an equivalent procedure, into an */
/*     equivalent binary DAF file which may be used with the SPICE */
/*     system. It is one of a pair of routines for performing conversions */
/*     between the binary format of a DAF file and the DAF transfer file. */
/*     The inverse of this routine is the routine DAFBT. */

/*     This routine makes NO use of the DAF reserved record area. It */
/*     can only deal with the data portion of a DAF file in the DAF */
/*     transfer file. */

/*     Upon successful completion, the binary DAF file specified by */
/*     BINFIL will have been created. The binary DAF file that was */
/*     created will be closed when this routine exits. The DAF transfer */
/*     file will remain open, as it was on entry, and it will be */
/*     positioned to read the first line after the encoded DAF file data. */

/* $ Examples */

/*     Let */

/*        XFRLUN   be the Fortran logical unit attached to a DAF */
/*                 transfer file which is to be converted into its binary */
/*                 DAF equivalent. */

/*        BINFIL   be the name of the binary DAF file which will be */
/*                 created from the DAF transfer file. */

/*     The following subroutine call would read the DAF transfer file */
/*     attached to the Fortran logical unit XFRLUN, convert its data into */
/*     binary format, and write that data to the binary DAF file which */
/*     has been created: */

/*        CALL DAFTB( XFRLUN, BINFIL ) */

/* $ Restrictions */

/*     1)  This routine assumes that it is positioned ready to read the */
/*         file ID word from the DAF transfer file. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     K.R. Gehringer     (JPL) */
/*     E.D. Wright        (JPL) */

/* $ Version */

/* -    SPICELIB Version 3.1.0, 13-AUG-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 3.0.1, 22-AUG-2001 (EDW) */

/*        Corrected ENDIF to END IF. */

/* -    SPICELIB Version 3.0.0, 25-JAN-1995 (KRG) */

/*        Updated the header and in line comments to reflect the change */
/*        from calling files text files to calling them transfer files. */

/*        Changed the variable name TXTLUN to XFRLUN to make it */
/*        compatible with the change in terminology. */

/*        Changed the short error message from "BADDAFTEXTFILE" to */
/*        "BADDAFTRANSFERFILE". */

/* -    SPICELIB Version 2.0.0, 04-SEP-1993 (KRG) */

/*        This routine was modified to incorporate the file ID word */
/*        changes which will allow run time identification of the type of */
/*        data in a SPICE binary file. */

/*        Removed the error SPICE(IDWORDNOTKNOWN) as it was no longer */
/*        relevant. */

/*        Added the error SPICE(NOTADAFFILE) if this routine is called */
/*        with a file that does not contain an ID word identifying the */
/*        file as a DAF file. */

/* -    SPICELIB Version 1.0.1, 24-JUN-1993 (KRG) */

/*        Modified the description of the DAF encoded text file format */
/*        appearing before the program code. */

/* -    SPICELIB Version 1.0.0, 02-NOV-1992 (KRG) */

/* -& */
/* $ Index_Entries */

/*     convert DAF transfer file to binary */

/* -& */
/* $ Revisions */

/* -    SPICELIB Version 3.0.0, 25-JAN-1995 (KRG) */

/*        Updated the header and in line comments to reflect the change */
/*        from calling files text files to calling them transfer files. */

/*        Changed the variable name TXTLUN to XFRLUN to make it */
/*        compatible with the change in terminology. */

/*        Changed the short error message from "BADDAFTEXTFILE" to */
/*        "BADDAFTRANSFERFILE". */

/* -    SPICELIB Version 2.0.0, 04-SEP-1993 (KRG) */

/*        This routine was modified to incorporate the file ID word */
/*        changes which will allow runtime identification of the type of */
/*        data in a binary file SPICE binary file. */

/*        Removed the error SPICE(IDWORDNOTKNOWN) as it was no longer */
/*        relevant. */

/*        Added the error SPICE(NOTADAFFILE) if this routine is called */
/*        with a file that does not contain an ID word identifying the */
/*        file as a DAF file. */

/* -    SPICELIB Version 1.0.1, 24-JUN-1993 (KRG) */

/*        Modified the description of the DAF encoded text file format */
/*        appearing before the program code. Changed the line: */

/*           C        < DAF ND value > < DAF NI value > */

/*        to the lines: */

/*           C        < DAF ND value > */
/*           C        < DAF NI value > */

/*        This change was necessary because the output format for the */
/*        low level routines which encode and write the data were */
/*        modified to fix a problem. See the routines WRENCD and WRENCI */
/*        for details of the modification. */

/* -& */

/*     SPICELIB functions */


/*     Local Parameters */


/*     Local variables */


/*     Standard/ SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("DAFTB", (ftnlen)5);
    }

/*     A brief description of the DAF transfer file format and its */
/*     intended use follows. This description is intended to provide a */
/*     simple ``picture'' of the DAF transfer file format to aid in the */
/*     understanding of this routine. This description is NOT intended to */
/*     be a detailed specification of the file format. */

/*     A DAF transfer file contains all of the data from a binary */
/*     DAF file, except for the reserved record area, in an encoded */
/*     ASCII format. The file also contains some bookkeeping information */
/*     for maintaining the integrity of the data. The DAF transfer file */
/*     format allows the full precision of both integer and floating */
/*     point numeric data to be maintained in a portable fashion. The DAF */
/*     transfer file format is intended to provide a reliable and */
/*     accurate means for porting data among multiple computer systems */
/*     and for the archival storage of data. */

/*     A DAF transfer file is not intended to be used directly to */
/*     provide data to a program, the equivalent binary DAF file is */
/*     to be used for this purpose. In no way should any program, other */
/*     than a DAF binary <-> transfer conversion program, rely on the DAF */
/*     encoded transfer file format. */

/*     To correctly understand the DAF transfer file description */
/*     the reader should be familiar with the DAF file architecture. */
/*     Items enclosed in angle brackets, '<' and '>', are used to */
/*     represent the data which is to be placed at that position in */
/*     the file. The bookkeeping information is represented exactly */
/*     as it would appear in a DAF transfer file. */

/*     Let */

/*       BOF    denote the beginning of the file */
/*       EOF    denote the end of the file */

/*    and */

/*       n      denote the total number of arrays in a DAF file */
/*       NA(i)  denote the number of double precision numbers in array i */
/*       m(i)   denote the number of blocks of encoded data for array i */
/*       N(i,j) denote the number of encoded double precision numbers */
/*              in block j of array i */

/*     and */

/*              m(i) */
/*             ----- */
/*             \ */
/*              >   N(i,k) = NA(i),   i = 1, ..., n. */
/*             / */
/*             ----- */
/*              k=1 */

/*     A DAF encoded transfer file has the following format: */

/*        <BOF> */
/*        < Information line > */
/*        < DAF file ID word > */
/*        < DAF ND value > */
/*        < DAF NI value > */
/*        < DAF internal file name > */
/*        BEGIN_ARRAY 1 NA(1) */
/*        < Name for array 1 > */
/*        < ND double precision summary values > */
/*        < NI-2 integer summary values > */
/*        N(1,1) */
/*        < N(1,1) Encoded double precision numbers > */
/*        N(1,2) */
/*        < N(1,2) Encoded double precision numbers > */
/*                          . */
/*                          . */
/*                          . */
/*        N(1,m(1)) */
/*        < N(1,m(1)) Encoded double precision numbers > */
/*        END_ARRAY 1 NA(1) */
/*        BEGIN_ARRAY 2 NA(2) */
/*        < Name for array 2 > */
/*        < ND double precision summary values > */
/*        < NI-2 integer summary values > */
/*        N(2,1) */
/*        < N(2,1) Encoded double precision numbers > */
/*        N(2,2) */
/*        < N(2,2) Encoded double precision numbers > */
/*                          . */
/*                          . */
/*                          . */
/*        N(2,m(2)) */
/*        < N(2,m(2)) Encoded double precision numbers > */
/*        END_ARRAY 2 NA(2) */
/*                          . */
/*                          . */
/*                          . */
/*        BEGIN_ARRAY n NA(n) */
/*        < Name for array n > */
/*        < ND double precision summary values > */
/*        < NI-2 integer summary values > */
/*        N(n,1) */
/*        < N(n,1) Encoded double precision numbers > */
/*        N(n,2) */
/*        < N(n,2) Encoded double precision numbers > */
/*                          . */
/*                          . */
/*                          . */
/*        N(n,m(n)) */
/*        < N(n,m(n)) Encoded double precision numbers > */
/*        END_ARRAY n NA(n) */
/*        TOTAL_ARRAYS  n */
/*        <EOF> */


/*     Initialize a few things. */

    s_copy(tarch, " ", (ftnlen)8, (ftnlen)1);
    s_copy(ttype, " ", (ftnlen)8, (ftnlen)1);
    s_copy(idword, " ", (ftnlen)8, (ftnlen)1);

/*     We begin by reading the DAF file ID word from the DAF transfer */
/*     file. We should have been positioned ready to read this. If an */
/*     error occurs, set an appropriate error message and signal the */
/*     error. */

    io___5.ciunit = *xfrlun;
    iostat = s_rsle(&io___5);
    if (iostat != 0) {
	goto L100001;
    }
    iostat = do_lio(&c__9, &c__1, idword, (ftnlen)8);
    if (iostat != 0) {
	goto L100001;
    }
    iostat = e_rsle();
L100001:
    if (iostat != 0) {
	setmsg_("Error reading the file ID word from the DAF transfer file '"
		"#'. IOSTAT = #.", (ftnlen)74);
	errfnm_("#", xfrlun, (ftnlen)1);
	errint_("#", &iostat, (ftnlen)1);
	sigerr_("SPICE(FILEREADFAILED)", (ftnlen)21);
	chkout_("DAFTB", (ftnlen)5);
	return 0;
    }

/*     Separate the ID word into its components and verify that we are */
/*     looking at a DAF transfer file. If we're not, then this routine */
/*     should not be used. */

    idw2at_(idword, tarch, ttype, (ftnlen)8, (ftnlen)8, (ftnlen)8);
    if (s_cmp(tarch, "DAF", (ftnlen)8, (ftnlen)3) != 0) {
	setmsg_("File architecture is not 'DAF' for file '#'", (ftnlen)43);
	errfnm_("#", xfrlun, (ftnlen)1);
	sigerr_("SPICE(NOTADAFFILE)", (ftnlen)18);
	chkout_("DAFTB", (ftnlen)5);
	return 0;
    }

/*     The file architecture is OK, but before we can open the binary */
/*     DAF, we need to get the summary format and the internal file name */
/*     from the DAF transfer file. We begin doing this here. */

/*     Read in the ND and NI values for the DAF file. */

    rdenci_(xfrlun, &c__2, isumry);
    if (failed_()) {
	chkout_("DAFTB", (ftnlen)5);
	return 0;
    }
    nd = isumry[0];
    ni = isumry[1];

/*     Read the internal filename for the DAF file. */

    io___9.ciunit = *xfrlun;
    iostat = s_rsle(&io___9);
    if (iostat != 0) {
	goto L100002;
    }
    iostat = do_lio(&c__9, &c__1, ifname, (ftnlen)60);
    if (iostat != 0) {
	goto L100002;
    }
    iostat = e_rsle();
L100002:
    if (iostat != 0) {
	setmsg_("Error reading the internal filename from the DAF transfer f"
		"ile '#'. IOSTAT = #.", (ftnlen)79);
	errfnm_("#", xfrlun, (ftnlen)1);
	errint_("#", &iostat, (ftnlen)1);
	sigerr_("SPICE(FILEREADFAILED)", (ftnlen)21);
	chkout_("DAFTB", (ftnlen)5);
	return 0;
    }

/*     Open a new binary DAF file. Call the proper open routine, */
/*     depending on whether it's a new file or an old file. */

    if (s_cmp(ttype, "?", (ftnlen)8, (ftnlen)1) != 0) {
	dafonw_(binfil, ttype, &nd, &ni, ifname, &c__0, &binhdl, binfil_len, (
		ftnlen)8, (ftnlen)60);
    } else {
	dafopn_(binfil, &nd, &ni, ifname, &c__0, &binhdl, binfil_len, (ftnlen)
		60);
    }
    if (failed_()) {
	chkout_("DAFTB", (ftnlen)5);
	return 0;
    }

/*     Calculate the length of the segment names. */

    snmlen = nd + (ni + 1) / 2 << 3;

/*     Initialize a few things: the array counter and the data counter. */

    arrcnt = 0;
    dtacnt = 0;

/*     We currently have more to process. */

    more = TRUE_;

/*     We are currently not processing an array. */

    inarr = FALSE_;

/*     Begin converting the DAF transfer file into a binary DAF file */
/*     here. */

    while(more) {
	ci__1.cierr = 1;
	ci__1.ciend = 1;
	ci__1.ciunit = *xfrlun;
	ci__1.cifmt = "(A)";
	iostat = s_rsfe(&ci__1);
	if (iostat != 0) {
	    goto L100003;
	}
	iostat = do_fio(&c__1, line, (ftnlen)255);
	if (iostat != 0) {
	    goto L100003;
	}
	iostat = e_rsfe();
L100003:
	if (iostat != 0) {
	    setmsg_("Error reading from the DAF transfer file '#'. IOSTAT = "
		    "#.", (ftnlen)57);
	    errfnm_("#", xfrlun, (ftnlen)1);
	    errint_("#", &iostat, (ftnlen)1);
	    sigerr_("SPICE(FILEREADFAILED)", (ftnlen)21);
	    chkout_("DAFTB", (ftnlen)5);
	    return 0;
	}

/*        At this point, we should be beginning an array, ending an */
/*        array, or scanning for the total number of arrays. So look */
/*        for the appropriate keyword. */

	nextwd_(line, word, rest, (ftnlen)255, (ftnlen)255, (ftnlen)255);
	if (s_cmp(word, "BEGIN_ARRAY", (ftnlen)255, (ftnlen)11) == 0) {

/*           Get the array number. */

	    nextwd_(rest, word, rest, (ftnlen)255, (ftnlen)255, (ftnlen)255);
	    nparsi_(word, &barr, errmsg, &errptr, (ftnlen)255, (ftnlen)320);
	    if (s_cmp(errmsg, " ", (ftnlen)320, (ftnlen)1) != 0) {
		setmsg_("Begin array error, could not parse array number. Er"
			"ror: # File: #", (ftnlen)65);
		errch_("#", errmsg, (ftnlen)1, (ftnlen)320);
		errfnm_("#", xfrlun, (ftnlen)1);
		sigerr_("SPICE(BADDAFTRANSFERFILE)", (ftnlen)25);
		chkout_("DAFTB", (ftnlen)5);
		return 0;
	    }

/*           Parse the count of double precision numbers in the array. */

	    nextwd_(rest, word, rest, (ftnlen)255, (ftnlen)255, (ftnlen)255);
	    nparsi_(word, &bcnt, errmsg, &errptr, (ftnlen)255, (ftnlen)320);
	    if (s_cmp(errmsg, " ", (ftnlen)320, (ftnlen)1) != 0) {
		setmsg_("Begin array error, could not parse the data count f"
			"or array: #. Error: # File: #", (ftnlen)80);
		errint_("#", &barr, (ftnlen)1);
		errch_("#", errmsg, (ftnlen)1, (ftnlen)320);
		errfnm_("#", xfrlun, (ftnlen)1);
		sigerr_("SPICE(BADDAFTRANSFERFILE)", (ftnlen)25);
		chkout_("DAFTB", (ftnlen)5);
		return 0;
	    }

/*           If we got to here, we are inside an array, so set the in */
/*           array flag, INARR, to .TRUE. and increment the array */
/*           counter. */

	    inarr = TRUE_;
	    ++arrcnt;
	} else if (s_cmp(word, "END_ARRAY", (ftnlen)255, (ftnlen)9) == 0) {

/*           Get the array number. */

	    nextwd_(rest, word, rest, (ftnlen)255, (ftnlen)255, (ftnlen)255);
	    nparsi_(word, &earr, errmsg, &errptr, (ftnlen)255, (ftnlen)320);
	    if (s_cmp(errmsg, " ", (ftnlen)320, (ftnlen)1) != 0) {
		setmsg_("End array error, could not parse array number. Erro"
			"r: # File: #", (ftnlen)63);
		errch_("#", errmsg, (ftnlen)1, (ftnlen)320);
		errfnm_("#", xfrlun, (ftnlen)1);
		sigerr_("SPICE(BADDAFTRANSFERFILE)", (ftnlen)25);
		chkout_("DAFTB", (ftnlen)5);
		return 0;
	    }

/*           Parse the count of double precision numbers in the array. */

	    nextwd_(rest, word, rest, (ftnlen)255, (ftnlen)255, (ftnlen)255);
	    nparsi_(word, &ecnt, errmsg, &errptr, (ftnlen)255, (ftnlen)320);
	    if (s_cmp(errmsg, " ", (ftnlen)320, (ftnlen)1) != 0) {
		setmsg_("End array error, could not parse the data count for"
			" array: #. Error: # File: #", (ftnlen)78);
		errint_("#", &earr, (ftnlen)1);
		errch_("#", errmsg, (ftnlen)1, (ftnlen)320);
		errfnm_("#", xfrlun, (ftnlen)1);
		sigerr_("SPICE(BADDAFTRANSFERFILE)", (ftnlen)25);
		chkout_("DAFTB", (ftnlen)5);
		return 0;
	    }

/*           Check to see if the beginning and ending array numbers */
/*           match. If not, signal an appropriate error. */

	    if (earr != barr) {
		setmsg_("Data array number mismatch: Beginning number: #; En"
			"ding number: #. File: #", (ftnlen)74);
		errint_("#", &barr, (ftnlen)1);
		errint_("#", &earr, (ftnlen)1);
		errfnm_("#", xfrlun, (ftnlen)1);
		sigerr_("SPICE(BADDAFTRANSFERFILE)", (ftnlen)25);
		chkout_("DAFTB", (ftnlen)5);
		return 0;
	    }

/*           Check to see if the beginning and ending array data counts */
/*           match. If not, signal an appropriate error. */

	    if (ecnt != bcnt) {
		setmsg_("Data array count mismatch: Beginning count: #; Endi"
			"ng count: #. File: #", (ftnlen)71);
		errint_("#", &bcnt, (ftnlen)1);
		errint_("#", &ecnt, (ftnlen)1);
		errfnm_("#", xfrlun, (ftnlen)1);
		sigerr_("SPICE(BADDAFTRANSFERFILE)", (ftnlen)25);
		chkout_("DAFTB", (ftnlen)5);
		return 0;
	    }

/*           If we got to here, we have successfully ended the */
/*           processing of an array, so set the in array flag, INARR, */
/*           to  .FALSE.. */

	    inarr = FALSE_;
	} else if (s_cmp(word, "TOTAL_ARRAYS", (ftnlen)255, (ftnlen)12) == 0) 
		{

/*           We have the total arrays keyword to parse, so get */
/*           the total number of arrays processed. */

	    nextwd_(rest, word, rest, (ftnlen)255, (ftnlen)255, (ftnlen)255);
	    nparsi_(word, &numarr, errmsg, &errptr, (ftnlen)255, (ftnlen)320);
	    if (s_cmp(errmsg, " ", (ftnlen)320, (ftnlen)1) != 0) {
		setmsg_("Array count error, could not parse the total number"
			" of arrays: #. File: #", (ftnlen)73);
		errch_("#", errmsg, (ftnlen)1, (ftnlen)320);
		errfnm_("#", xfrlun, (ftnlen)1);
		sigerr_("SPICE(BADDAFTRANSFERFILE)", (ftnlen)25);
		chkout_("DAFTB", (ftnlen)5);
		return 0;
	    }
	    if (arrcnt != numarr) {
		setmsg_("The number of data arrays processed (#) was not equ"
			"al to the number of data arrays placed in the DAF tr"
			"ansfer file (#). File: #", (ftnlen)127);
		errint_("#", &arrcnt, (ftnlen)1);
		errint_("#", &numarr, (ftnlen)1);
		errfnm_("#", xfrlun, (ftnlen)1);
		sigerr_("SPICE(BADDAFTRANSFERFILE)", (ftnlen)25);
		chkout_("DAFTB", (ftnlen)5);
		return 0;
	    }

/*           If we got to here, we have successfully processed the */
/*           entire data portion of the DAF transfer file, so there is */
/*           no more data. */

	    more = FALSE_;
	} else {
	    setmsg_("Unknown keyword '#' encountered while processing the DA"
		    "F transfer file #.", (ftnlen)73);
	    errch_("#", word, (ftnlen)1, (ftnlen)255);
	    errfnm_("#", xfrlun, (ftnlen)1);
	    sigerr_("SPICE(BADDAFTRANSFERFILE)", (ftnlen)25);
	    chkout_("DAFTB", (ftnlen)5);
	    return 0;
	}

/*        If we have begun an array, then process it. Otherwise, we */
/*        have either ended an array or ended the file. */

	if (inarr) {
	    dtacnt = 0;
	    io___27.ciunit = *xfrlun;
	    iostat = s_rsle(&io___27);
	    if (iostat != 0) {
		goto L100004;
	    }
	    iostat = do_lio(&c__9, &c__1, name__, snmlen);
	    if (iostat != 0) {
		goto L100004;
	    }
	    iostat = e_rsle();
L100004:
	    if (iostat != 0) {
		setmsg_("Error reading the array name from the DAF transfer "
			"file #. IOSTAT = #.", (ftnlen)70);
		errfnm_("#", xfrlun, (ftnlen)1);
		errint_("#", &iostat, (ftnlen)1);
		sigerr_("SPICE(FILEREADFAILED)", (ftnlen)21);
		chkout_("DAFTB", (ftnlen)5);
		return 0;
	    }

/*           Read in the double precision part of the summary. */

	    rdencd_(xfrlun, &nd, dsumry);
	    if (failed_()) {
		chkout_("DAFTB", (ftnlen)5);
		return 0;
	    }

/*           Read in the integer part of the summary. The beginning and */
/*           ending addresses, ISUMRY(NI-1) and ISUMRY(NI), for the */
/*           array are not known currently. They will be filled in when */
/*           the array is actually written to the DAF file. */

	    i__1 = ni - 2;
	    rdenci_(xfrlun, &i__1, isumry);
	    if (failed_()) {
		chkout_("DAFTB", (ftnlen)5);
		return 0;
	    }

/*           Pack the summary information into the DAF array summary. */

	    dafps_(&nd, &ni, dsumry, isumry, summry);

/*           Begin a new array in the binary DAF file. */

	    dafbna_(&binhdl, summry, name__, snmlen);
	    if (failed_()) {
		chkout_("DAFTB", (ftnlen)5);
		return 0;
	    }

/*           Read and decode the data in the current DAF array. */

/*           First set the count of numbers yet to be decoded and placed */
/*           in the binary DAF file. */

	    numlft = bcnt;
	    while(numlft > 0) {

/*              First, read in the count of encoded numbers in the */
/*              current data block. */

		io___32.ciunit = *xfrlun;
		iostat = s_rsle(&io___32);
		if (iostat != 0) {
		    goto L100005;
		}
		iostat = do_lio(&c__3, &c__1, (char *)&numdta, (ftnlen)sizeof(
			integer));
		if (iostat != 0) {
		    goto L100005;
		}
		iostat = e_rsle();
L100005:
		if (iostat != 0) {
		    setmsg_("Error reading array data from the DAF transfer "
			    "file #. IOSTAT = #.", (ftnlen)66);
		    errfnm_("#", xfrlun, (ftnlen)1);
		    errint_("#", &iostat, (ftnlen)1);
		    sigerr_("SPICE(FILEREADFAILED)", (ftnlen)21);
		    chkout_("DAFTB", (ftnlen)5);
		    return 0;
		}

/*              Now read and decode the data in the current data block, */
/*              placing the data in the current array in the binary DAF */
/*              file. */

		lftovr = numdta;
		while(lftovr > 0) {
		    if (lftovr >= 1024) {
			numdta = 1024;
		    } else {
			numdta = lftovr;
		    }

/*                 Read and decode a buffer of encoded double precision */
/*                 data from the DAF transfer file. */

		    rdencd_(xfrlun, &numdta, buffer);
		    if (failed_()) {
			chkout_("DAFTB", (ftnlen)5);
			return 0;
		    }

/*                 Write the double precision data to the current array */
/*                 in the binary DAF file. */

		    dafada_(buffer, &numdta);
		    if (failed_()) {
			chkout_("DAFTB", (ftnlen)5);
			return 0;
		    }

/*                 Decrement the counters for the amount of data */
/*                 remaining to be moved from the current data block, */
/*                 LFTOVR, and the current array, NUMLFT. */

		    lftovr -= numdta;
		    numlft -= numdta;

/*                 Increment the counter for the amount of data that */
/*                 has been successfully moved into the current array */
/*                 in the binary DAF file. */

		    dtacnt += numdta;
		}

/*              At this point, we have either finished reading in the */
/*              entire array, or we have just completed reading the */
/*              current encoded block of data for the current array */
/*              from the DAF transfer file. */

	    }

/*           If we got to here, we have successfully written an array */
/*           to the binary file, so we need to end it. */

	    dafena_();
	    if (failed_()) {
		chkout_("DAFTB", (ftnlen)5);
		return 0;
	    }
	}
    }

/*     Close only the binary file. */

    dafcls_(&binhdl);
    chkout_("DAFTB", (ftnlen)5);
    return 0;
} /* daftb_ */

