/* dafbt.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static logical c_false = FALSE_;
static integer c__1 = 1;
static integer c__3 = 3;
static integer c__2 = 2;

/* $Procedure DAFBT ( DAF, convert binary file to transfer file ) */
/* Subroutine */ int dafbt_(char *binfil, integer *xfrlun, ftnlen binfil_len)
{
    /* System generated locals */
    address a__1[3];
    integer i__1[3], i__2, i__3;
    char ch__1[10], ch__2[62], ch__3[1002];
    cilist ci__1;

    /* Builtin functions */
    integer s_rdue(cilist *), do_uio(integer *, char *, ftnlen), e_rdue(void),
	     s_wsfe(cilist *), do_fio(integer *, char *, ftnlen), e_wsfe(void)
	    ;
    /* Subroutine */ int s_cat(char *, char **, integer *, integer *, ftnlen);
    integer s_rnge(char *, integer, char *, integer);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    char name__[1000];
    integer free;
    char line[80];
    extern /* Subroutine */ int zzddhhlu_(integer *, char *, logical *, 
	    integer *, ftnlen), dafgn_(char *, ftnlen), dafgs_(doublereal *), 
	    chkin_(char *, ftnlen);
    integer bward;
    extern /* Subroutine */ int dafus_(doublereal *, integer *, integer *, 
	    doublereal *, integer *);
    integer fward;
    logical found;
    extern /* Subroutine */ int repmi_(char *, char *, integer *, char *, 
	    ftnlen, ftnlen, ftnlen);
    extern integer rtrim_(char *, ftnlen);
    extern /* Subroutine */ int dafgda_(integer *, integer *, integer *, 
	    doublereal *), daffna_(logical *);
    integer nd;
    extern logical failed_(void);
    extern /* Subroutine */ int dafbfs_(integer *);
    integer dtabeg, ni;
    extern /* Subroutine */ int dafcls_(integer *);
    char ifname[60];
    integer binhdl;
    extern /* Subroutine */ int dafrfr_(integer *, integer *, integer *, char 
	    *, integer *, integer *, integer *, ftnlen);
    doublereal buffer[1024];
    integer dtacnt;
    extern /* Subroutine */ int dafopr_(char *, integer *, ftnlen), wrencd_(
	    integer *, integer *, doublereal *);
    integer binlun;
    char idword[8];
    integer numdta;
    extern /* Subroutine */ int errfnm_(char *, integer *, ftnlen), sigerr_(
	    char *, ftnlen);
    integer snmlen;
    extern /* Subroutine */ int chkout_(char *, ftnlen), wrenci_(integer *, 
	    integer *, integer *);
    integer iostat, numarr, numlft;
    extern /* Subroutine */ int setmsg_(char *, ftnlen), errint_(char *, 
	    integer *, ftnlen);
    extern logical return_(void);
    doublereal dsumry[125];
    integer isumry[250];
    doublereal summry[125];

    /* Fortran I/O blocks */
    static cilist io___4 = { 1, 0, 1, 0, 1 };


/* $ Abstract */

/*     Convert the contents of a binary DAF file to an equivalent DAF */
/*     transfer file. */

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
/*     BINFIL     I   The name of a binary DAF file to be converted. */
/*     XFRLUN     I   Logical unit of a previously opened file. */

/* $ Detailed_Input */

/*     BINFIL   is the name of a binary DAF file which is to be converted */
/*              to an equivalent DAF transfer file. */

/*     XFRLUN   is the Fortran logical unit number of a previously opened */
/*              file. The DAF transfer file will be written to the */
/*              file attached to this logical unit beginning at the */
/*              current position in the file. */

/* $ Detailed_Output */

/*     None. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the binary DAF file specified by the filename BINFIL cannot */
/*         be opened for read access, an error is signaled by a routine */
/*         in the call tree of this routine. */

/*     2)  If for some reason the DAF transfer file cannot be written */
/*         to, the error SPICE(FILEWRITEFAILED) is signaled. */

/*     3)  If, for any reason, the DAF file cannot be read, an error is */
/*         signaled by a routine in the call tree of this routine. */

/*     4)  If the ID word cannot be read from the binary file, the error */
/*         SPICE(FILEREADFAILED) is signaled. */

/*     5)  The binary DAF file opened by this routine, BINFIL, is only */
/*         GUARANTEED to be closed upon successful completion of the */
/*         conversion process. In the event of an error, the caller of */
/*         this routine is required to close the binary DAF file BINFIL. */

/* $ Files */

/*     See arguments BINFIL, XFRLUN. */

/* $ Particulars */

/*     Any binary DAF file may be transferred between heterogeneous */
/*     Fortran environments by converting it to an equivalent file */
/*     containing only ASCII characters. Such a file can be transferred */
/*     almost universally, using any number of established protocols. */
/*     Once transferred, the ASCII file can be converted to a binary */
/*     file, using the representations native to the new host */
/*     environment. */

/*     This routine provides a mechanism for converting a binary DAF */
/*     file into an equivalent encoded ASCII file called a DAF transfer */
/*     file. It is one of a pair of routines for performing conversions */
/*     between the binary format of a DAF file and the DAF transfer file. */
/*     The inverse of this routine is the routine DAFTB. */

/*     The contents of the reserved records in a binary DAF file are */
/*     ignored by this routine. They are not written to the DAF transfer */
/*     file. The reserved records must be dealt with separately from the */
/*     data in a DAF file. */

/*     Upon successful completion, the DAF transfer file attached to */
/*     Fortran logical unit XFRLUN will contain the same data as the */
/*     binary DAF file BINFIL. The binary DAF file BINFIL will be closed */
/*     when this routine exits. The DAF transfer file will remain open, */
/*     as it was on entry, and it will be positioned to write on the */
/*     first line following the encoded DAF data. */

/* $ Examples */

/*     Let */

/*        BINFIL   be the name of a binary DAF file which is to be */
/*                 converted to an equivalent DAF transfer file. */

/*        XFRLUN   be the Fortran logical unit to which the DAF transfer */
/*                 file is to be written. */

/*     The following subroutine call would read the binary DAF */
/*     file with the name BINFIL, convert its data into an encoded */
/*     format, and write that data to the DAF transfer file attached */
/*     to the Fortran logical unit XFRLUN, beginning at the current */
/*     position in the file. */

/*        CALL DAFBT( BINFIL, XFRLUN ) */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     K.R. Gehringer     (JPL) */
/*     F.S. Turner        (JPL) */

/* $ Version */

/* -    SPICELIB Version 4.1.0, 13-AUG-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 4.0.0, 16-NOV-2001 (FST) */

/*        Updated the routine to utilize the new handle manager */
/*        interfaces. */

/* -    SPICELIB Version 3.0.0, 25-JAN-1995 (KRG) */

/*        Updated the header and in line comments to reflect the change */
/*        from calling files text files to calling them transfer files. */

/*        Changed the variable name TXTLUN to XFRLUN to make it */
/*        compatible with the change in terminology. */

/* -    SPICELIB Version 2.0.0, 04-OCT-1993 (KRG) */

/*        No changes to this routine were necessary to incorporate the */
/*        new file ID word format. This routine already read and copied */
/*        the ID word to the text file being created. */

/*        Also, all list directed writes in this routine were replaced by */
/*        formatted writes with FMT = '(A)'. This routine only writes */
/*        character data. */

/*        Added a test of FAILED() after the call to DAFHLU for */
/*        completeness. */

/* -    SPICELIB Version 1.0.1, 24-JUN-1993 (KRG) */

/*        Modified the description of the DAF encoded text file format */
/*        appearing before the program code. */

/* -    SPICELIB Version 1.0.0, 29-OCT-1992 (KRG) */

/* -& */
/* $ Index_Entries */

/*     convert binary DAF into a DAF transfer file */

/* -& */
/* $ Revisions */

/* -    SPICELIB Version 4.0.0, 16-NOV-2001 (FST) */

/*        This routine still uses a naked READ to retrieve the */
/*        file IDWORD from the first 8 characters stored in the */
/*        file record. It may be that future environments */
/*        will have characters whose storage exceeds 1 byte, */
/*        in which case this routine will require modification. */
/*        One possibility is to call the private file record */
/*        reader ZZDAFGFR, which must address the translation */
/*        for all supported non-native binary file formats on this */
/*        platform. */

/*        The existing call to DAFHLU was replaced with ZZDDHHLU. */
/*        The call to DAFRDA was replaced with a call to the new, */
/*        translation-aware routine DAFGDA. */

/* -    SPICELIB Version 2.0.0, 04-OCT-1993 (KRG) */

/*        No changes to this routine were necessary to incorporate the */
/*        new file ID word format. This routine already read and copied */
/*        the ID word to the text file being created. */

/*        Also, all list directed writes in this routine were replaced by */
/*        formatted writes with FMT = '(A)'. This routine only writes */
/*        character data. */

/*        Added a test of FAILED() after the call to DAFHLU for */
/*        completeness. */

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


/*     Local parameters */


/*     Local variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("DAFBT", (ftnlen)5);
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

/*     This routine will check the SPICELIB function FAILED() after */
/*     each call, or consecutive sequence of calls, to data encoding */
/*     routines, and if an error was signaled it will simply check out */
/*     and return to the caller. */

/*     This routine will check the SPICELIB function FAILED() after */
/*     each DAF file access call, and if an error was signaled it will */
/*     simply check out and return to the caller. */

/*     We begin by opening the binary DAF file specified by BINFIL for */
/*     read access, obtaining a DAF file handle. */

    dafopr_(binfil, &binhdl, binfil_len);

/*     If the open failed, check out and return, as an appropriate error */
/*     message should have already been set. */

    if (failed_()) {
	chkout_("DAFBT", (ftnlen)5);
	return 0;
    }

/*     At this point, we know that we have a DAF file, because we were */
/*     able to successfully open it, so we will attempt to proceed with */
/*     the file conversion process. */

/*     Convert the DAF file handle to its equivalent Fortran logical */
/*     unit. We need to do this in order to accurately move the file */
/*     ID word to the DAF transfer file. */

    zzddhhlu_(&binhdl, "DAF", &c_false, &binlun, (ftnlen)3);

/*     If the translation failed, checkout and return, as an appropriate */
/*     error message should have already been set. */

    if (failed_()) {
	chkout_("DAFBT", (ftnlen)5);
	return 0;
    }

/*     Read the ID word from the binary file. It should be the first 8 */
/*     characters on the first record in the file. */

    io___4.ciunit = binlun;
    iostat = s_rdue(&io___4);
    if (iostat != 0) {
	goto L100001;
    }
    iostat = do_uio(&c__1, idword, (ftnlen)8);
    if (iostat != 0) {
	goto L100001;
    }
    iostat = e_rdue();
L100001:
    if (iostat != 0) {
	setmsg_("Error reading the file ID word from the binary DAF file '#'"
		". IOSTAT = #.", (ftnlen)72);
	errfnm_("#", &binlun, (ftnlen)1);
	errint_("#", &iostat, (ftnlen)1);
	sigerr_("SPICE(FILEREADFAILED)", (ftnlen)21);
	chkout_("DAFBT", (ftnlen)5);
	return 0;
    }

/*     Get the contents of the file record: the number of double */
/*     precision numbers in the summary (ND), the number of integers */
/*     in the summary (NI), the internal filename (IFNAME), and some */
/*     data pointer information (FWARD, BWARD, FREE). */

    dafrfr_(&binhdl, &nd, &ni, ifname, &fward, &bward, &free, (ftnlen)60);
    if (failed_()) {
	chkout_("DAFBT", (ftnlen)5);
	return 0;
    }

/*     Write the information line containing the file type information */
/*     for the DAF transfer file format to the current position in the */
/*     DAF transfer file. The file type information must be the first */
/*     ``word'' on the information line. The rest of the line may be used */
/*     for other purposes. Right now, it simply contains an expanded */
/*     description of the file type information ``word.'' */

    ci__1.cierr = 1;
    ci__1.ciunit = *xfrlun;
    ci__1.cifmt = "(A)";
    iostat = s_wsfe(&ci__1);
    if (iostat != 0) {
	goto L100002;
    }
    iostat = do_fio(&c__1, "DAFETF NAIF DAF ENCODED TRANSFER FILE", (ftnlen)
	    37);
    if (iostat != 0) {
	goto L100002;
    }
    iostat = e_wsfe();
L100002:
    if (iostat != 0) {
	setmsg_("Error writing to the DAF transfer file '#'.IOSTAT = #.", (
		ftnlen)54);
	errfnm_("#", xfrlun, (ftnlen)1);
	errint_("#", &iostat, (ftnlen)1);
	sigerr_("SPICE(FILEWRITEFAILED)", (ftnlen)22);
	chkout_("DAFBT", (ftnlen)5);
	return 0;
    }

/*     Write the ID word to the DAF transfer file. */

    ci__1.cierr = 1;
    ci__1.ciunit = *xfrlun;
    ci__1.cifmt = "(A)";
    iostat = s_wsfe(&ci__1);
    if (iostat != 0) {
	goto L100003;
    }
/* Writing concatenation */
    i__1[0] = 1, a__1[0] = "'";
    i__1[1] = 8, a__1[1] = idword;
    i__1[2] = 1, a__1[2] = "'";
    s_cat(ch__1, a__1, i__1, &c__3, (ftnlen)10);
    iostat = do_fio(&c__1, ch__1, (ftnlen)10);
    if (iostat != 0) {
	goto L100003;
    }
    iostat = e_wsfe();
L100003:
    if (iostat != 0) {
	setmsg_("Error writing to the DAF transfer file '#'. IOSTAT = #.", (
		ftnlen)55);
	errfnm_("#", xfrlun, (ftnlen)1);
	errint_("#", &iostat, (ftnlen)1);
	sigerr_("SPICE(FILEWRITEFAILED)", (ftnlen)22);
	chkout_("DAFBT", (ftnlen)5);
	return 0;
    }

/*     Write out the ND and NI values for the DAF file architecture. */

    isumry[0] = nd;
    isumry[1] = ni;
    wrenci_(xfrlun, &c__2, isumry);
    if (failed_()) {
	chkout_("DAFBT", (ftnlen)5);
	return 0;
    }

/*     Write out the internal file name. */

    ci__1.cierr = 1;
    ci__1.ciunit = *xfrlun;
    ci__1.cifmt = "(A)";
    iostat = s_wsfe(&ci__1);
    if (iostat != 0) {
	goto L100004;
    }
/* Writing concatenation */
    i__1[0] = 1, a__1[0] = "'";
    i__1[1] = 60, a__1[1] = ifname;
    i__1[2] = 1, a__1[2] = "'";
    s_cat(ch__2, a__1, i__1, &c__3, (ftnlen)62);
    iostat = do_fio(&c__1, ch__2, (ftnlen)62);
    if (iostat != 0) {
	goto L100004;
    }
    iostat = e_wsfe();
L100004:
    if (iostat != 0) {
	setmsg_("Error writing to the DAF transfer file '#'. IOSTAT = #.", (
		ftnlen)55);
	errfnm_("#", xfrlun, (ftnlen)1);
	errint_("#", &iostat, (ftnlen)1);
	sigerr_("SPICE(FILEWRITEFAILED)", (ftnlen)22);
	chkout_("DAFBT", (ftnlen)5);
	return 0;
    }

/*     Calculate the length of the segment names. */

    snmlen = nd + (ni + 1) / 2 << 3;

/*     Get ready to begin a forward search through the DAF file for the */
/*     data. */

    dafbfs_(&binhdl);
    if (failed_()) {
	chkout_("DAFBT", (ftnlen)5);
	return 0;
    }

/*     Initialize the number of arrays processed to zero. */

    numarr = 0;

/*     We'll assume that we will find some data, until proven otherwise. */

    found = TRUE_;

/*     Begin looking for and processing the arrays in the binary DAF */
/*     file. */

    while(found) {

/*        Look for a DAF array. */

	daffna_(&found);
	if (failed_()) {
	    chkout_("DAFBT", (ftnlen)5);
	    return 0;
	}

/*        If we found an array, then we need to process it. Start */
/*        by incrementing the number of arrays processed. If not, */
/*        we just skip to the bottom of the loop. */

	if (found) {
	    ++numarr;

/*           Get and unpack the summary information for the current */
/*           array. */

	    dafgs_(summry);
	    dafus_(summry, &nd, &ni, dsumry, isumry);

/*           Get the name of the current array. */

	    dafgn_(name__, (ftnlen)1000);
	    if (failed_()) {

/*              If an error occurred on any of the DAF system calls */
/*              above, return to the caller. An appropriate error */
/*              message will have already been set by the routine which */
/*              signaled the error. */

		chkout_("DAFBT", (ftnlen)5);
		return 0;
	    }

/*           Get the beginning address for the data in the current array. */

	    dtabeg = isumry[(i__2 = ni - 2) < 250 && 0 <= i__2 ? i__2 : 
		    s_rnge("isumry", i__2, "dafbt_", (ftnlen)664)];

/*           Set the number of double precision numbers in the current */
/*           array. */

	    dtacnt = isumry[(i__2 = ni - 1) < 250 && 0 <= i__2 ? i__2 : 
		    s_rnge("isumry", i__2, "dafbt_", (ftnlen)669)] - isumry[(
		    i__3 = ni - 2) < 250 && 0 <= i__3 ? i__3 : s_rnge("isumry"
		    , i__3, "dafbt_", (ftnlen)669)] + 1;
	    s_copy(line, "BEGIN_ARRAY # #", (ftnlen)80, (ftnlen)15);
	    repmi_(line, "#", &numarr, line, (ftnlen)80, (ftnlen)1, (ftnlen)
		    80);
	    repmi_(line, "#", &dtacnt, line, (ftnlen)80, (ftnlen)1, (ftnlen)
		    80);
	    ci__1.cierr = 1;
	    ci__1.ciunit = *xfrlun;
	    ci__1.cifmt = "(A)";
	    iostat = s_wsfe(&ci__1);
	    if (iostat != 0) {
		goto L100005;
	    }
	    iostat = do_fio(&c__1, line, rtrim_(line, (ftnlen)80));
	    if (iostat != 0) {
		goto L100005;
	    }
	    iostat = e_wsfe();
L100005:
	    if (iostat != 0) {
		setmsg_("Error writing to the DAF transfer file '#'. IOSTAT "
			"= #.", (ftnlen)55);
		errfnm_("#", xfrlun, (ftnlen)1);
		errint_("#", &iostat, (ftnlen)1);
		sigerr_("SPICE(FILEWRITEFAILED)", (ftnlen)22);
		chkout_("DAFBT", (ftnlen)5);
		return 0;
	    }

/*           Write the name of the current array. */

	    ci__1.cierr = 1;
	    ci__1.ciunit = *xfrlun;
	    ci__1.cifmt = "(A)";
	    iostat = s_wsfe(&ci__1);
	    if (iostat != 0) {
		goto L100006;
	    }
/* Writing concatenation */
	    i__1[0] = 1, a__1[0] = "'";
	    i__1[1] = snmlen, a__1[1] = name__;
	    i__1[2] = 1, a__1[2] = "'";
	    s_cat(ch__3, a__1, i__1, &c__3, (ftnlen)1002);
	    iostat = do_fio(&c__1, ch__3, snmlen + 2);
	    if (iostat != 0) {
		goto L100006;
	    }
	    iostat = e_wsfe();
L100006:
	    if (iostat != 0) {
		setmsg_("Error writing to the DAF transfer file '#'. IOSTAT "
			"= #.", (ftnlen)55);
		errfnm_("#", xfrlun, (ftnlen)1);
		errint_("#", &iostat, (ftnlen)1);
		sigerr_("SPICE(FILEWRITEFAILED)", (ftnlen)22);
		chkout_("DAFBT", (ftnlen)5);
		return 0;
	    }

/*           Write out the double precision part of the summary. */

	    wrencd_(xfrlun, &nd, dsumry);

/*           Write out the integer part of the summary, excluding the */
/*           beginning and ending addresses of the data in the array, */
/*           ISUMRY(NI-1) and ISUMRY(NI), since these values vary with */
/*           the number of reserved records allocated. */

	    i__2 = ni - 2;
	    wrenci_(xfrlun, &i__2, isumry);
	    if (failed_()) {

/*              If an error occurred on any of the data encoding calls */
/*              above, return to the caller. An appropriate error message */
/*              will have already been set by the routine which signaled */
/*              the error. */

		chkout_("DAFBT", (ftnlen)5);
		return 0;
	    }
	    numlft = dtacnt;
	    while(numlft > 0) {
		if (numlft >= 1024) {
		    numdta = 1024;
		} else {
		    numdta = numlft;
		}

/*              Read in NUMDTA numbers from the current array. The */
/*              desired data are specified by beginning and ending */
/*              indices into the array, inclusive: thus the subtraction */
/*              of 1 in the call. */

		i__2 = dtabeg + numdta - 1;
		dafgda_(&binhdl, &dtabeg, &i__2, buffer);
		if (failed_()) {

/*                 We want to check failed here because were in a loop. */
/*                 We should exit the loop, and the routine, as soon as */
/*                 an error is detected, so we don't continue doing */
/*                 things for a long time. */

		    chkout_("DAFBT", (ftnlen)5);
		    return 0;
		}

/*              Write out the count of double precision numbers which are */
/*              in the buffer. */

		s_copy(line, "#", (ftnlen)80, (ftnlen)1);
		repmi_(line, "#", &numdta, line, (ftnlen)80, (ftnlen)1, (
			ftnlen)80);
		ci__1.cierr = 1;
		ci__1.ciunit = *xfrlun;
		ci__1.cifmt = "(A)";
		iostat = s_wsfe(&ci__1);
		if (iostat != 0) {
		    goto L100007;
		}
		iostat = do_fio(&c__1, line, rtrim_(line, (ftnlen)80));
		if (iostat != 0) {
		    goto L100007;
		}
		iostat = e_wsfe();
L100007:
		if (iostat != 0) {
		    setmsg_("Error writing to the DAF transfer file '#'. IOS"
			    "TAT = #.", (ftnlen)55);
		    errfnm_("#", xfrlun, (ftnlen)1);
		    errint_("#", &iostat, (ftnlen)1);
		    sigerr_("SPICE(FILEWRITEFAILED)", (ftnlen)22);
		    chkout_("DAFBT", (ftnlen)5);
		    return 0;
		}

/*              Encode and write out a buffer of double precision */
/*              numbers. */

		wrencd_(xfrlun, &numdta, buffer);
		if (failed_()) {

/*                 We want to check failed here because were in a loop. */
/*                 We should exit the loop, and the routine, as soon as */
/*                 an error is detected, so we don't continue doing */
/*                 things for a long time. */

		    chkout_("DAFBT", (ftnlen)5);
		    return 0;
		}
		numlft -= numdta;
		dtabeg += numdta;
	    }
	    s_copy(line, "END_ARRAY # #", (ftnlen)80, (ftnlen)13);
	    repmi_(line, "#", &numarr, line, (ftnlen)80, (ftnlen)1, (ftnlen)
		    80);
	    repmi_(line, "#", &dtacnt, line, (ftnlen)80, (ftnlen)1, (ftnlen)
		    80);
	    ci__1.cierr = 1;
	    ci__1.ciunit = *xfrlun;
	    ci__1.cifmt = "(A)";
	    iostat = s_wsfe(&ci__1);
	    if (iostat != 0) {
		goto L100008;
	    }
	    iostat = do_fio(&c__1, line, rtrim_(line, (ftnlen)80));
	    if (iostat != 0) {
		goto L100008;
	    }
	    iostat = e_wsfe();
L100008:
	    if (iostat != 0) {
		setmsg_("Error writing to the DAF transfer file '#'. IOSTAT "
			"= #.", (ftnlen)55);
		errfnm_("#", xfrlun, (ftnlen)1);
		errint_("#", &iostat, (ftnlen)1);
		sigerr_("SPICE(FILEWRITEFAILED)", (ftnlen)22);
		chkout_("DAFBT", (ftnlen)5);
		return 0;
	    }
	}

/*        At this point, one complete DAF array has been written to the */
/*        DAF transfer file. */

    }

/*     Write out the number of arrays processed. */

    s_copy(line, "TOTAL_ARRAYS #", (ftnlen)80, (ftnlen)14);
    repmi_(line, "#", &numarr, line, (ftnlen)80, (ftnlen)1, (ftnlen)80);
    ci__1.cierr = 1;
    ci__1.ciunit = *xfrlun;
    ci__1.cifmt = "(A)";
    iostat = s_wsfe(&ci__1);
    if (iostat != 0) {
	goto L100009;
    }
    iostat = do_fio(&c__1, line, rtrim_(line, (ftnlen)80));
    if (iostat != 0) {
	goto L100009;
    }
    iostat = e_wsfe();
L100009:
    if (iostat != 0) {
	setmsg_("Error writing to the DAF transfer file '#'. IOSTAT = #.", (
		ftnlen)55);
	errfnm_("#", xfrlun, (ftnlen)1);
	errint_("#", &iostat, (ftnlen)1);
	sigerr_("SPICE(FILEWRITEFAILED)", (ftnlen)22);
	chkout_("DAFBT", (ftnlen)5);
	return 0;
    }

/*     Close only the binary file. */

    dafcls_(&binhdl);
    chkout_("DAFBT", (ftnlen)5);
    return 0;
} /* dafbt_ */

