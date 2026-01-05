/* dasbt.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static logical c_false = FALSE_;
static integer c__1 = 1;
static integer c__3 = 3;
static integer c__4 = 4;

/* $Procedure DASBT ( DAS, convert binary file to transfer file ) */
/* Subroutine */ int dasbt_(char *binfil, integer *xfrlun, ftnlen binfil_len)
{
    /* System generated locals */
    address a__1[3];
    integer i__1[3], i__2;
    char ch__1[10], ch__2[62];
    cilist ci__1;

    /* Builtin functions */
    integer s_wsfe(cilist *), do_fio(integer *, char *, ftnlen), e_wsfe(void);
    /* Subroutine */ int s_cat(char *, char **, integer *, integer *, ftnlen),
	     s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    char line[80];
    extern /* Subroutine */ int zzddhhlu_(integer *, char *, logical *, 
	    integer *, ftnlen), chkin_(char *, ftnlen);
    integer ncomc, recno;
    extern /* Subroutine */ int repmi_(char *, char *, integer *, char *, 
	    ftnlen, ftnlen, ftnlen);
    integer ncomr;
    extern integer rtrim_(char *, ftnlen);
    extern logical failed_(void);
    integer dtabeg, ncdata, handle, nddata;
    char ifname[60];
    integer nidata;
    extern /* Subroutine */ int daslla_(integer *, integer *, integer *, 
	    integer *);
    char crecrd[1024];
    extern /* Subroutine */ int dasioc_(char *, integer *, integer *, char *, 
	    ftnlen, ftnlen), dasrdc_(integer *, integer *, integer *, integer 
	    *, integer *, char *, ftnlen);
    char cbuffr[4*1024];
    doublereal dbuffr[1024];
    extern /* Subroutine */ int dascls_(integer *);
    integer ibuffr[1024];
    extern /* Subroutine */ int dasrdd_(integer *, integer *, integer *, 
	    doublereal *), dasrdi_(integer *, integer *, integer *, integer *)
	    ;
    integer daslun;
    extern /* Subroutine */ int dasrfr_(integer *, char *, char *, integer *, 
	    integer *, integer *, integer *, ftnlen, ftnlen);
    char idword[8];
    integer numblk, numdta;
    extern /* Subroutine */ int dasopr_(char *, integer *, ftnlen), chkout_(
	    char *, ftnlen), errfnm_(char *, integer *, ftnlen);
    integer nresvc;
    extern /* Subroutine */ int sigerr_(char *, ftnlen);
    integer iostat;
    extern /* Subroutine */ int setmsg_(char *, ftnlen);
    integer numlft;
    extern /* Subroutine */ int errint_(char *, integer *, ftnlen), wrenci_(
	    integer *, integer *, integer *), wrencc_(integer *, integer *, 
	    char *, ftnlen), wrencd_(integer *, integer *, doublereal *);
    extern logical return_(void);
    integer nresvr;

/* $ Abstract */

/*     Convert the contents of a binary DAS file to an equivalent DAS */
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

/*     DAS */

/* $ Keywords */

/*     CONVERSION */
/*     FILES */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     BINFIL     I   Name of the binary DAS file to be converted. */
/*     XFRLUN     I   Logical unit of a previously opened file. */

/* $ Detailed_Input */

/*     BINFIL   is the name of a binary DAS file which is to be converted */
/*              to an equivalent DAS transfer file. */

/*     XFRLUN   is the Fortran logical unit number of a previously opened */
/*              file. The DAS transfer file will be written to the */
/*              file attached to this logical unit beginning at the */
/*              current position in the file. */

/* $ Detailed_Output */

/*     None. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the binary DAS file specified by the filename BINFIL cannot */
/*         be opened for read access, an error is signaled by a routine */
/*         in the call tree of this routine. */

/*     2)  If for some reason the DAS transfer file cannot be written */
/*         to, the error SPICE(FILEWRITEFAILED) is signaled. */

/*     3)  If, for any reason, the DAS file cannot be read, an error is */
/*         signaled by a routine in the call tree of this routine. */

/*     4)  The binary DAS file opened by this routine, BINFIL, is only */
/*         GUARANTEED to be closed upon successful completion of the */
/*         binary to transfer conversion process. In the event of an */
/*         error, the caller of this routine is required to close the */
/*         binary DAS file BINFIL. */

/*     5)  If the values for the number of reserved records or the */
/*         number of reserved characters in a DAS file is nonzero, */
/*         the error SPICE(BADDASFILE) is signaled. THIS ERROR */
/*         IS SIGNALED ONLY BECAUSE THE RESERVED RECORD AREA HAS */
/*         NOT YET BEEN IMPLEMENTED. */

/* $ Files */

/*     See arguments BINFIL, XFRLUN. */

/* $ Particulars */

/*     Any binary DAS file may be transferred between heterogeneous */
/*     Fortran environments by converting it to an equivalent file */
/*     containing only ASCII characters called a DAS transfer file. */
/*     Such a file can be transferred almost universally using any number */
/*     of established protocols. Once transferred, the DAS transfer file */
/*     can be converted to a binary file using the representations native */
/*     to the new host environment. */

/*     This routine provides a mechanism for converting a binary DAS */
/*     file into an equivalent DAS transfer file. It is one of a pair of */
/*     routines for performing conversions between the binary format of a */
/*     DAS file and the DAS transfer file. The inverse of this routine is */
/*     the routine DASTB. */

/*     Upon successful completion, the DAS transfer file attached to */
/*     Fortran logical unit XFRLUN will contain the same data as the */
/*     binary DAS file BINFIL in an encoded ASCII format. The binary DAS */
/*     file BINFIL will be closed when this routine exits successfully. */
/*     The DAS transfer file will remain open, as it was on entry, and it */
/*     will be positioned to write on the first line following the */
/*     encoded data from the binary DAS file. */

/* $ Examples */

/*     Let */

/*        BINFIL   be the name of a binary DAS file which is to be */
/*                 converted to an equivalent DAS transfer file. This */
/*                 could be for purposes of porting the data to a */
/*                 different computer platform, or possibly for */
/*                 archival storage of the data. */

/*        XFRLUN   be the Fortran logical unit to which the DAS transfer */
/*                 file is to be written. */

/*     Then, the following subroutine call would read the binary DAS */
/*     file BINFIL, convert its contents into an encoded format, and */
/*     then write that data to the DAS transfer file attached to XFRLUN, */
/*     beginning at the current position in that file. */

/*        CALL DASBT ( BINFIL, XFRLUN ) */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     K.R. Gehringer     (JPL) */

/* $ Version */

/* -    SPICELIB Version 3.2.0, 02-JUN-2021 (JDR) */

/*        Added IMPLICIT NONE standard. */

/*        Edited the header to comply with NAIF standard. Removed */
/*        unnecessary $Revisions section. */

/* -    SPICELIB Version 3.1.0, 05-FEB-1995 (NJB) */

/*        Updated to support integration with the handle manager */
/*        subsystem. */

/* -    SPICELIB Version 3.0.0, 13-AUG-1994 (KRG) */

/*        Updated the header and in line comments to reflect the change */
/*        from calling files text files to calling them transfer files. */

/*        Changed the variable name TXTLUN to XFRLUN to make it */
/*        compatible with the change in terminology. */

/* -    SPICELIB Version 2.0.0, 13-AUG-1994 (KRG) */

/*        A potential problem with list directed writes was fixed. Some */
/*        compilers have list directed writes that write multiple comma */
/*        separated items to one line and other compilers write these to */
/*        multiple lines even when all of the output will fit on a single */
/*        line. This was fixed by replacing all of the affected list */
/*        directed write statements with code to put the desired data */
/*        into a character string and then write the character string. */

/* -    SPICELIB Version 1.0.0, 29-OCT-1992 (KRG) */

/* -& */
/* $ Index_Entries */

/*     convert binary DAS to DAS transfer file */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*      CHARACTER*(*)         BEGRES */
/*      PARAMETER           ( BEGRES = 'BEGIN_RESERVED_BLOCK'      ) */

/*      CHARACTER*(*)         ENDRES */
/*      PARAMETER           ( ENDRES = 'END_RESERVED_BLOCK'        ) */

/*      CHARACTER*(*)         TRRBLK */
/*      PARAMETER           ( TRRBLK = 'TOTAL_RESERVED_BLOCKS'     ) */


/*     Some parameters for writing the array markers */


/*     Length of a character buffer array element. */


/*     Length of a DAS file ID word. */


/*     Length of a DAS internal filename. */


/*     Length of a DAS comment record, in characters. */


/*     Size of the character, double precision, and integer data buffers. */


/*     Beginning and ending string positions for reading/writing */
/*     character data from/to a DAS file using the character data */
/*     buffer. */


/*     Local variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("DASBT", (ftnlen)5);
    }

/*     When converting a binary DAS file into its DAS transfer file */
/*     equivalent, all of the data contained in the binary file is */
/*     placed into the DAS transfer file by this routine. This includes */
/*     the reserved record area, the comment area, and the character, */
/*     double precision, and integer data arrays as well. */

/*     Currently, the reserved record area has not been implemented, as */
/*     there is no need for it at this time. If, or when, the reserved */
/*     record area is implemented, this routine will need to be modified */
/*     in order to support it. See the code for details. */

/*     The data from the binary file are written to the DAS transfer */
/*     file as sequences of small blocks of data. This is to provide */
/*     a means for performing some error detection when converting a */
/*     DAS transfer file into its binary equivalent. Each block of */
/*     data is enclosed within begin and end block markers which hold */
/*     the count of data items in a data block. When all of the data */
/*     blocks for a data area have been written, a total blocks line is */
/*     written to the DAS transfer file. */

/*     The data from the binary DAS file MUST appear in the following */
/*     order in the DAS transfer file. */

/*           1) Reserved records (when/if implemented) */
/*           2) Comment area */
/*           3) Character data array */
/*           4) Double precision data array */
/*           5) Integer data array */

/*     If the data count for any of these DAS data areas is zero, no */
/*     data or markers for it are placed into the DAS transfer file. */
/*     Conversion proceeds with the next DAS data area in the list. */

/*     For example, suppose that we have a binary DAS file where there */
/*     are 0 reserved characters in the reserved record area, 5000 */
/*     comment characters in the comment area, and that the character, */
/*     double precision, and integer array counts are 0, 2300, and */
/*     6900, respectively. Then, the DAS transfer file will contain */
/*     no reserved record data blocks, 2 comment data blocks, no */
/*     character data blocks, 3 double precision data blocks, and 7 */
/*     integer data blocks, in that order. */

/*     DAS transfer file description. */
/*     ---------------------------------- */

/*     A brief description of the DAS encoded file format and its */
/*     intended use follows. This description is intended to provide a */
/*     simple ``picture'' of the DAS transfer file format to aid in the */
/*     understanding of this routine. This description is NOT intended to */
/*     be a detailed specification of the file format. */

/*     A DAS transfer file contains all of the data from a binary */
/*     DAS file in an encoded ASCII format. It also contains some */
/*     bookkeeping information for maintaining the integrity of the */
/*     data. The DAS transfer file format allows the full precision of */
/*     character, integer, and floating point numeric data to be */
/*     maintained in a portable fashion. The DAS transfer file format is */
/*     intended to provide a reliable and accurate means for porting data */
/*     among multiple computer systems and for the archival storage of */
/*     data. */

/*     A DAS transfer file is not intended to be used directly to provide */
/*     data to a program. The equivalent binary DAS file is to be used */
/*     for this purpose. In no way should any program, other than a DAS */
/*     binary <-> transfer conversion program, rely on the DAS transfer */
/*     file format. */

/*     To correctly understand the DAS transfer file description  the */
/*     reader should be familiar with the DAS file architecture. Items */
/*     enclosed in angle brackets, '<' and '>', are used to represent the */
/*     data which are to be placed at that position in the file. The */
/*     bookkeeping information which appears is represented exactly as it */
/*     would appear in a DAS transfer file. */

/*     Let */

/*        <BOF>  denote the beginning of the file */
/*        <EOF>  denote the end of the file */

/*     and */

/*        nresvb  denote the number of encoded reserved record data */
/*                blocks generated */
/*        nresvc  denote the total number of reserved record characters */
/*                in the  reserved record area of a DAS file */
/*        ncomb   denote the number of encoded comment data blocks */
/*                generated */
/*        ncomc   denote the total number of comment characters in the */
/*                comment area of a DAS file */
/*        nchrb   denote the number of encoded character data blocks */
/*                generated */
/*        nchrs   denote the count of characters in the DAS character */
/*                data array */
/*        ndpb    denote the number of encoded double precision data */
/*                blocks generated */
/*        ndps    denote the count of double precision numbers in the DAS */
/*                double precision data array */
/*        nintb   denote the number of encoded integer data blocks */
/*                generated */
/*        nints   denote the count of integers in the DAS integer data */
/*                array */

/*     A DAS encoded transfer file has the following format: */

/*        <BOF> */
/*        < Information line > */
/*        < DAS file ID word > */
/*        < Internal filename > */
/*        < Encoded count of reserved records > */
/*        < Encoded count of reserved characters > */
/*        < Encoded count of comment records > */
/*        < Encoded count of comment characters > */
/*        < Blocks of encoded reserved record data, if nresvc > 0 > */
/*        TOTAL_RESERVED_BLOCKS nresvb nresvc */
/*        < Blocks of encoded comment data, if ncomc > 0 > */
/*        TOTAL_COMMENT_BLOCKS ncomb ncomc */
/*        < Encoded count of character data > */
/*        < Encoded count of double precision data > */
/*        < Encoded count of integer data > */
/*        < Blocks of encoded character data, if nchrs > 0 > */
/*        TOTAL_CHARACTER_BLOCKS nchrb nchrs */
/*        < Blocks of encoded double precision data, if ndps > 0 > */
/*        TOTAL_DP_BLOCKS ndpb ndps */
/*        < Blocks of encoded integer data, if nints > 0 > */
/*        TOTAL_INTEGER_BLOCKS nintb nints */
/*        <EOF> */

/*     This routine will check the SPICELIB function FAILED() after */
/*     each call, or consecutive sequence of calls, to data encoding */
/*     routines, and if an error was signaled it will simply check out */
/*     and return to the caller. */

/*     This routine will check the SPICELIB function FAILED() after */
/*     each DAS file access call, and if an error was signaled it will */
/*     simply check out and return to the caller. */

/*     We begin by opening the binary DAS file specified by BINFIL for */
/*     read access, obtaining a file handle. */

    dasopr_(binfil, &handle, binfil_len);
    if (failed_()) {

/*        If an error occurred while opening the file check out and */
/*        return to the caller. */

	chkout_("DASBT", (ftnlen)5);
	return 0;
    }

/*     Get the contents of the DAS file record. */

    dasrfr_(&handle, idword, ifname, &nresvr, &nresvc, &ncomr, &ncomc, (
	    ftnlen)8, (ftnlen)60);

/*     Convert the DAS file handle into its equivalent Fortran logical */
/*     unit. We need the logical unit so that we can read the reserved */
/*     records and the comment records. */

    zzddhhlu_(&handle, "DAS", &c_false, &daslun, (ftnlen)3);
    if (failed_()) {

/*        If an error occurred while converting the DAS file handle to */
/*        a logical unit, attempt to close the binary file, then check */
/*        out and return. */

	dascls_(&handle);
	chkout_("DASBT", (ftnlen)5);
	return 0;
    }

/*     Check to be sure that the number of reserved records and the */
/*     number of reserved characters are not being used. The DAS */
/*     reserved record area is not currently implemented, so nobody */
/*     should be using it. */

    if (nresvc != 0) {

/*        Set the error message, close the file, signal the error, and */
/*        exit. */

	setmsg_("The number of reserved characters was nonzero (#) in file: "
		"#, but the DAS reserved record area has NOT been implemented"
		" yet!", (ftnlen)124);
	errint_("#", &nresvc, (ftnlen)1);
	errfnm_("#", &daslun, (ftnlen)1);
	dascls_(&handle);
	sigerr_("SPICE(BADDASFILE)", (ftnlen)17);
	chkout_("DASBT", (ftnlen)5);
	return 0;
    }
    if (nresvr != 0) {

/*        Set the error message, close the file, signal the error, and */
/*        exit. */

	setmsg_("The number of reserved records was nonzero (#) in file: #, "
		"but the DAS reserved record area has NOT been implemented ye"
		"t!", (ftnlen)121);
	errint_("#", &nresvr, (ftnlen)1);
	errfnm_("#", &daslun, (ftnlen)1);
	dascls_(&handle);
	sigerr_("SPICE(BADDASFILE)", (ftnlen)17);
	chkout_("DASBT", (ftnlen)5);
	return 0;
    }

/*     Write the information line containing the file type information */
/*     and format version for the DAS transfer to the current position in */
/*     the file. The file format version information must be the first */
/*     ``word'' on the information line. The rest of the line may be used */
/*     for other purposes. Right now, it simply contains an expanded */
/*     description of the file format version information ``word.'' */

    ci__1.cierr = 1;
    ci__1.ciunit = *xfrlun;
    ci__1.cifmt = "(A)";
    iostat = s_wsfe(&ci__1);
    if (iostat != 0) {
	goto L100001;
    }
    iostat = do_fio(&c__1, "DASETF NAIF DAS ENCODED TRANSFER FILE", (ftnlen)
	    37);
    if (iostat != 0) {
	goto L100001;
    }
    iostat = e_wsfe();
L100001:
    if (iostat != 0) {

/*        An error occurred, so close the binary DAS file, set an */
/*        appropriate error message, and return to the caller. */

	dascls_(&handle);
	setmsg_("Error writing to the DAS transfer file: #. IOSTAT = #.", (
		ftnlen)54);
	errfnm_("#", xfrlun, (ftnlen)1);
	errint_("#", &iostat, (ftnlen)1);
	sigerr_("SPICE(FILEWRITEFAILED)", (ftnlen)22);
	chkout_("DASBT", (ftnlen)5);
	return 0;
    }

/*     Write the DAS ID word to the DAS transfer file. */

    ci__1.cierr = 1;
    ci__1.ciunit = *xfrlun;
    ci__1.cifmt = "(A)";
    iostat = s_wsfe(&ci__1);
    if (iostat != 0) {
	goto L100002;
    }
/* Writing concatenation */
    i__1[0] = 1, a__1[0] = "'";
    i__1[1] = 8, a__1[1] = idword;
    i__1[2] = 1, a__1[2] = "'";
    s_cat(ch__1, a__1, i__1, &c__3, (ftnlen)10);
    iostat = do_fio(&c__1, ch__1, (ftnlen)10);
    if (iostat != 0) {
	goto L100002;
    }
    iostat = e_wsfe();
L100002:
    if (iostat != 0) {

/*        An error occurred, so close the binary DAS file, set an */
/*        appropriate error message, and return to the caller. */

	dascls_(&handle);
	setmsg_("Error writing to the DAS transfer file: #. IOSTAT = #.", (
		ftnlen)54);
	errfnm_("#", xfrlun, (ftnlen)1);
	errint_("#", &iostat, (ftnlen)1);
	sigerr_("SPICE(FILEWRITEFAILED)", (ftnlen)22);
	chkout_("DASBT", (ftnlen)5);
	return 0;
    }

/*     Write the internal file name of the DAS file to the DAS transfer */
/*     file. */

    ci__1.cierr = 1;
    ci__1.ciunit = *xfrlun;
    ci__1.cifmt = "(A)";
    iostat = s_wsfe(&ci__1);
    if (iostat != 0) {
	goto L100003;
    }
/* Writing concatenation */
    i__1[0] = 1, a__1[0] = "'";
    i__1[1] = 60, a__1[1] = ifname;
    i__1[2] = 1, a__1[2] = "'";
    s_cat(ch__2, a__1, i__1, &c__3, (ftnlen)62);
    iostat = do_fio(&c__1, ch__2, (ftnlen)62);
    if (iostat != 0) {
	goto L100003;
    }
    iostat = e_wsfe();
L100003:
    if (iostat != 0) {

/*        An error occurred, so close the binary DAS file, set an */
/*        appropriate error message, and return to the caller. */

	dascls_(&handle);
	setmsg_("Error writing to the DAS transfer file: #. IOSTAT = #.", (
		ftnlen)54);
	errfnm_("#", xfrlun, (ftnlen)1);
	errint_("#", &iostat, (ftnlen)1);
	sigerr_("SPICE(FILEWRITEFAILED)", (ftnlen)22);
	chkout_("DASBT", (ftnlen)5);
	return 0;
    }

/*     Write the number of reserved records and reserved characters to */
/*     the DAS transfer file. */

    wrenci_(xfrlun, &c__1, &nresvr);
    wrenci_(xfrlun, &c__1, &nresvc);
    if (failed_()) {

/*        If an error occurred while writing the number of reserved */
/*        records or number of reserved characters, attempt to close */
/*        the binary file, then check out and return. */

	dascls_(&handle);
	chkout_("DASBT", (ftnlen)5);
	return 0;
    }

/*     Write the number of comment records and comment characters to */
/*     the DAS transfer file. */

    wrenci_(xfrlun, &c__1, &ncomr);
    wrenci_(xfrlun, &c__1, &ncomc);
    if (failed_()) {

/*        If an error occurred while writing the number of comment */
/*        records or number of comment characters, attempt to close */
/*        the binary file, then check out and return. */

	dascls_(&handle);
	chkout_("DASBT", (ftnlen)5);
	return 0;
    }

/*     ************************************************************** */
/*     When/if the reserved record area is implemented, the code to */
/*     convert it and place it into the DAS transfer file should go */
/*     here. It should be possible to simply copy the code for the */
/*     comment area, making all of the necessary variable name changes, */
/*     etc., since the reserved record area is going to contain ONLY */
/*     character data. */
/*     ************************************************************** */

/*     Write out the comment area of the DAS file, if there are any */
/*     comment characters stored in it. */

    if (ncomc > 0) {

/*        Write out the comment records, one at a time. */

	s_copy(crecrd, " ", (ftnlen)1024, (ftnlen)1);
	numlft = ncomc;
	numblk = 0;
	recno = nresvr + 1;
	while(numlft > 0) {
	    ++numblk;
	    ++recno;
	    if (numlft > 1024) {
		numdta = 1024;
	    } else {
		numdta = numlft;
	    }

/*           Write out the begin comment block marker and the number of */
/*           comment characters. */

	    s_copy(line, "BEGIN_COMMENT_BLOCK # #", (ftnlen)80, (ftnlen)23);
	    repmi_(line, "#", &numblk, line, (ftnlen)80, (ftnlen)1, (ftnlen)
		    80);
	    repmi_(line, "#", &numdta, line, (ftnlen)80, (ftnlen)1, (ftnlen)
		    80);
	    ci__1.cierr = 1;
	    ci__1.ciunit = *xfrlun;
	    ci__1.cifmt = "(A)";
	    iostat = s_wsfe(&ci__1);
	    if (iostat != 0) {
		goto L100004;
	    }
	    iostat = do_fio(&c__1, line, rtrim_(line, (ftnlen)80));
	    if (iostat != 0) {
		goto L100004;
	    }
	    iostat = e_wsfe();
L100004:
	    if (iostat != 0) {

/*              An error occurred, so close the binary DAS file, set an */
/*              appropriate error message, and return to the caller. */

		dascls_(&handle);
		setmsg_("Error writing to the DAS transfer file: #. IOSTAT ="
			" #.", (ftnlen)54);
		errfnm_("#", xfrlun, (ftnlen)1);
		errint_("#", &iostat, (ftnlen)1);
		sigerr_("SPICE(FILEWRITEFAILED)", (ftnlen)22);
		chkout_("DASBT", (ftnlen)5);
		return 0;
	    }

/*           Read a comment record and then encode and write it. */

	    dasioc_("READ", &daslun, &recno, crecrd, (ftnlen)4, (ftnlen)1024);
	    wrencc_(xfrlun, &numdta, crecrd, (ftnlen)1024);
	    if (failed_()) {

/*              We want to check failed here because were in a loop. */
/*              We should exit the loop, and the routine, as soon as */
/*              an error is detected, so we don't continue doing things */
/*              for a long time. Attempt to close the binary DAS file */
/*              that we opened and then return to the caller. */

		dascls_(&handle);
		chkout_("DASBT", (ftnlen)5);
		return 0;
	    }

/*           Write out the end comment block marker and the number of */
/*           comment characters. */

	    s_copy(line, "END_COMMENT_BLOCK # #", (ftnlen)80, (ftnlen)21);
	    repmi_(line, "#", &numblk, line, (ftnlen)80, (ftnlen)1, (ftnlen)
		    80);
	    repmi_(line, "#", &numdta, line, (ftnlen)80, (ftnlen)1, (ftnlen)
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

/*              An error occurred, so close the binary DAS file, set an */
/*              appropriate error message, and return to the caller. */

		dascls_(&handle);
		setmsg_("Error writing to the DAS transfer file: #. IOSTAT ="
			" #.", (ftnlen)54);
		errfnm_("#", xfrlun, (ftnlen)1);
		errint_("#", &iostat, (ftnlen)1);
		sigerr_("SPICE(FILEWRITEFAILED)", (ftnlen)22);
		chkout_("DASBT", (ftnlen)5);
		return 0;
	    }

/*           Update the number of comment characters remaining to be */
/*           written. */

	    numlft -= numdta;
	}

/*        Write out the number of comment blocks processed, and the */
/*        count of comment characters */

	s_copy(line, "TOTAL_COMMENT_BLOCKS # #", (ftnlen)80, (ftnlen)24);
	repmi_(line, "#", &numblk, line, (ftnlen)80, (ftnlen)1, (ftnlen)80);
	repmi_(line, "#", &ncomc, line, (ftnlen)80, (ftnlen)1, (ftnlen)80);
	ci__1.cierr = 1;
	ci__1.ciunit = *xfrlun;
	ci__1.cifmt = "(A)";
	iostat = s_wsfe(&ci__1);
	if (iostat != 0) {
	    goto L100006;
	}
	iostat = do_fio(&c__1, line, rtrim_(line, (ftnlen)80));
	if (iostat != 0) {
	    goto L100006;
	}
	iostat = e_wsfe();
L100006:
	if (iostat != 0) {

/*           An error occurred, so close the binary DAS file, set an */
/*           appropriate error message, and return to the caller. */

	    dascls_(&handle);
	    setmsg_("Error writing to the DAS transfer file: #. IOSTAT = #.", 
		    (ftnlen)54);
	    errfnm_("#", xfrlun, (ftnlen)1);
	    errint_("#", &iostat, (ftnlen)1);
	    sigerr_("SPICE(FILEWRITEFAILED)", (ftnlen)22);
	    chkout_("DASBT", (ftnlen)5);
	    return 0;
	}
    }

/*     Read in the data counts for each of the data types from the binary */
/*     DAS file. */

    daslla_(&handle, &ncdata, &nddata, &nidata);

/*     Write the data counts to the DAS transfer file. These will be */
/*     useful in determining which data types to expect in the DAS */
/*     transfer file when converting it back to binary. */

    wrenci_(xfrlun, &c__1, &ncdata);
    wrenci_(xfrlun, &c__1, &nddata);
    wrenci_(xfrlun, &c__1, &nidata);
    if (failed_()) {

/*        If an error occurred while writing any of the data counts to */
/*        the DAS transfer file, attempt to close the binary file, then */
/*        check out and return. */

	dascls_(&handle);
	chkout_("DASBT", (ftnlen)5);
	return 0;
    }

/*     Encode and write the CHARACTER data to the DAS transfer file, if */
/*     there is any character data. */

    if (ncdata > 0) {
	numblk = 0;
	dtabeg = 1;
	numlft = ncdata;
	while(numlft > 0) {
	    ++numblk;
	    if (numlft >= 4096) {
		numdta = 4096;
	    } else {
		numdta = numlft;
	    }

/*           Write out the begin data block identifier, the block */
/*           number, and the data count for the block. */

	    s_copy(line, "BEGIN_CHARACTER_BLOCK # #", (ftnlen)80, (ftnlen)25);
	    repmi_(line, "#", &numblk, line, (ftnlen)80, (ftnlen)1, (ftnlen)
		    80);
	    repmi_(line, "#", &numdta, line, (ftnlen)80, (ftnlen)1, (ftnlen)
		    80);
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

/*              An error occurred, so close the binary DAS file, set an */
/*              appropriate error message, and return to the caller. */

		dascls_(&handle);
		setmsg_("Error writing to the DAS transfer file: #. IOSTAT ="
			" #.", (ftnlen)54);
		errfnm_("#", xfrlun, (ftnlen)1);
		errint_("#", &iostat, (ftnlen)1);
		sigerr_("SPICE(FILEWRITEFAILED)", (ftnlen)22);
		chkout_("DASBT", (ftnlen)5);
		return 0;
	    }

/*           Read in NUMDTA characters. The desired data are specified by */
/*           beginning and ending indices into the array, inclusive: thus */
/*           the subtraction of 1 in the call. */

	    i__2 = dtabeg + numdta - 1;
	    dasrdc_(&handle, &dtabeg, &i__2, &c__1, &c__4, cbuffr, (ftnlen)4);

/*           Encode and write out a buffer of characters. */

	    wrencc_(xfrlun, &numdta, cbuffr, (ftnlen)4);
	    if (failed_()) {

/*              We want to check failed here because were in a loop. */
/*              We should exit the loop, and the routine, as soon as */
/*              an error is detected, so we don't continue doing things */
/*              for a long time. Attempt to close the binary DAS file */
/*              that we opened and then return to the caller. */

		dascls_(&handle);
		chkout_("DASBT", (ftnlen)5);
		return 0;
	    }

/*           Write out the end data block identifier, the block number, */
/*           and the data count for the block. */

	    s_copy(line, "END_CHARACTER_BLOCK # #", (ftnlen)80, (ftnlen)23);
	    repmi_(line, "#", &numblk, line, (ftnlen)80, (ftnlen)1, (ftnlen)
		    80);
	    repmi_(line, "#", &numdta, line, (ftnlen)80, (ftnlen)1, (ftnlen)
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

/*              An error occurred, so close the binary DAS file, set an */
/*              appropriate error message, and return to the caller. */

		dascls_(&handle);
		setmsg_("Error writing to the DAS transfer file: #. IOSTAT ="
			" #.", (ftnlen)54);
		errfnm_("#", xfrlun, (ftnlen)1);
		errint_("#", &iostat, (ftnlen)1);
		sigerr_("SPICE(FILEWRITEFAILED)", (ftnlen)22);
		chkout_("DASBT", (ftnlen)5);
		return 0;
	    }

/*           Increment the data pointer and decrement the amount of data */
/*           left to move. */

	    dtabeg += numdta;
	    numlft -= numdta;
	}

/*        Write out the number of character data blocks processed */
/*        processed, and the count of double precision data items. */

	s_copy(line, "TOTAL_CHARACTER_BLOCKS # #", (ftnlen)80, (ftnlen)26);
	repmi_(line, "#", &numblk, line, (ftnlen)80, (ftnlen)1, (ftnlen)80);
	repmi_(line, "#", &ncdata, line, (ftnlen)80, (ftnlen)1, (ftnlen)80);
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

/*           An error occurred, so close the binary DAS file, set an */
/*           appropriate error message, and return to the caller. */

	    dascls_(&handle);
	    setmsg_("Error writing to the DAS transfer file: #. IOSTAT = #.", 
		    (ftnlen)54);
	    errfnm_("#", xfrlun, (ftnlen)1);
	    errint_("#", &iostat, (ftnlen)1);
	    sigerr_("SPICE(FILEWRITEFAILED)", (ftnlen)22);
	    chkout_("DASBT", (ftnlen)5);
	    return 0;
	}
    }

/*     Encode and write the DOUBLE PRECISION data to the DAS transfer */
/*     file. */

    if (nddata > 0) {
	numblk = 0;
	dtabeg = 1;
	numlft = nddata;
	while(numlft > 0) {
	    ++numblk;
	    if (numlft >= 1024) {
		numdta = 1024;
	    } else {
		numdta = numlft;
	    }

/*           Write out the begin data block identifier, the block */
/*           number, and the data count for the block. */

	    s_copy(line, "BEGIN_DP_BLOCK # #", (ftnlen)80, (ftnlen)18);
	    repmi_(line, "#", &numblk, line, (ftnlen)80, (ftnlen)1, (ftnlen)
		    80);
	    repmi_(line, "#", &numdta, line, (ftnlen)80, (ftnlen)1, (ftnlen)
		    80);
	    ci__1.cierr = 1;
	    ci__1.ciunit = *xfrlun;
	    ci__1.cifmt = "(A)";
	    iostat = s_wsfe(&ci__1);
	    if (iostat != 0) {
		goto L100010;
	    }
	    iostat = do_fio(&c__1, line, rtrim_(line, (ftnlen)80));
	    if (iostat != 0) {
		goto L100010;
	    }
	    iostat = e_wsfe();
L100010:
	    if (iostat != 0) {

/*              An error occurred, so close the binary DAS file, set an */
/*              appropriate error message, and return to the caller. */

		dascls_(&handle);
		setmsg_("Error writing to the DAS transfer file: #. IOSTAT ="
			" #.", (ftnlen)54);
		errfnm_("#", xfrlun, (ftnlen)1);
		errint_("#", &iostat, (ftnlen)1);
		sigerr_("SPICE(FILEWRITEFAILED)", (ftnlen)22);
		chkout_("DASBT", (ftnlen)5);
		return 0;
	    }

/*           Read in NUMDTA double precision numbers.The desired data are */
/*           specified by beginning and ending indices into the array, */
/*           inclusive: thus the subtraction of 1 in the call. */

	    i__2 = dtabeg + numdta - 1;
	    dasrdd_(&handle, &dtabeg, &i__2, dbuffr);

/*           Encode and write out a buffer of double precision numbers. */

	    wrencd_(xfrlun, &numdta, dbuffr);
	    if (failed_()) {

/*              We want to check failed here because were in a loop. */
/*              We should exit the loop, and the routine, as soon as */
/*              an error is detected, so we don't continue doing things */
/*              for a long time. Attempt to close the binary DAS file */
/*              that we opened and then return to the caller. */

		dascls_(&handle);
		chkout_("DASBT", (ftnlen)5);
		return 0;
	    }

/*           Write out the end data block identifier, the block number, */
/*           and the data count for the block. */

	    s_copy(line, "END_DP_BLOCK # #", (ftnlen)80, (ftnlen)16);
	    repmi_(line, "#", &numblk, line, (ftnlen)80, (ftnlen)1, (ftnlen)
		    80);
	    repmi_(line, "#", &numdta, line, (ftnlen)80, (ftnlen)1, (ftnlen)
		    80);
	    ci__1.cierr = 1;
	    ci__1.ciunit = *xfrlun;
	    ci__1.cifmt = "(A)";
	    iostat = s_wsfe(&ci__1);
	    if (iostat != 0) {
		goto L100011;
	    }
	    iostat = do_fio(&c__1, line, rtrim_(line, (ftnlen)80));
	    if (iostat != 0) {
		goto L100011;
	    }
	    iostat = e_wsfe();
L100011:
	    if (iostat != 0) {

/*              An error occurred, so close the binary DAS file, set an */
/*              appropriate error message, and return to the caller. */

		dascls_(&handle);
		setmsg_("Error writing to the DAS transfer file: #. IOSTAT ="
			" #.", (ftnlen)54);
		errfnm_("#", xfrlun, (ftnlen)1);
		errint_("#", &iostat, (ftnlen)1);
		sigerr_("SPICE(FILEWRITEFAILED)", (ftnlen)22);
		chkout_("DASBT", (ftnlen)5);
		return 0;
	    }

/*           Increment the data pointer and decrement the amount of data */
/*           left to move. */

	    dtabeg += numdta;
	    numlft -= numdta;
	}

/*        Write out the number of double precision processed data blocks */
/*        processed, and the count of double precision data items. */

	s_copy(line, "TOTAL_DP_BLOCKS # #", (ftnlen)80, (ftnlen)19);
	repmi_(line, "#", &numblk, line, (ftnlen)80, (ftnlen)1, (ftnlen)80);
	repmi_(line, "#", &nddata, line, (ftnlen)80, (ftnlen)1, (ftnlen)80);
	ci__1.cierr = 1;
	ci__1.ciunit = *xfrlun;
	ci__1.cifmt = "(A)";
	iostat = s_wsfe(&ci__1);
	if (iostat != 0) {
	    goto L100012;
	}
	iostat = do_fio(&c__1, line, rtrim_(line, (ftnlen)80));
	if (iostat != 0) {
	    goto L100012;
	}
	iostat = e_wsfe();
L100012:
	if (iostat != 0) {

/*           An error occurred, so close the binary DAS file, set an */
/*           appropriate error message, and return to the caller. */

	    dascls_(&handle);
	    setmsg_("Error writing to the DAS transfer file: #. IOSTAT = #.", 
		    (ftnlen)54);
	    errfnm_("#", xfrlun, (ftnlen)1);
	    errint_("#", &iostat, (ftnlen)1);
	    sigerr_("SPICE(FILEWRITEFAILED)", (ftnlen)22);
	    chkout_("DASBT", (ftnlen)5);
	    return 0;
	}
    }

/*     Encode and write the INTEGER data to the DAS transfer file, if */
/*     there is any. */

    if (nidata > 0) {
	numblk = 0;
	dtabeg = 1;
	numlft = nidata;
	while(numlft > 0) {
	    ++numblk;
	    if (numlft >= 1024) {
		numdta = 1024;
	    } else {
		numdta = numlft;
	    }

/*           Write out the begin data block identifier, the block number, */
/*           and the data count for the block. */

	    s_copy(line, "BEGIN_INTEGER_BLOCK # #", (ftnlen)80, (ftnlen)23);
	    repmi_(line, "#", &numblk, line, (ftnlen)80, (ftnlen)1, (ftnlen)
		    80);
	    repmi_(line, "#", &numdta, line, (ftnlen)80, (ftnlen)1, (ftnlen)
		    80);
	    ci__1.cierr = 1;
	    ci__1.ciunit = *xfrlun;
	    ci__1.cifmt = "(A)";
	    iostat = s_wsfe(&ci__1);
	    if (iostat != 0) {
		goto L100013;
	    }
	    iostat = do_fio(&c__1, line, rtrim_(line, (ftnlen)80));
	    if (iostat != 0) {
		goto L100013;
	    }
	    iostat = e_wsfe();
L100013:
	    if (iostat != 0) {

/*              An error occurred, so close the binary DAS file, set an */
/*              appropriate error message, and return to the caller. */

		dascls_(&handle);
		setmsg_("Error writing to the DAS transfer file: #. IOSTAT ="
			" #.", (ftnlen)54);
		errfnm_("#", xfrlun, (ftnlen)1);
		errint_("#", &iostat, (ftnlen)1);
		sigerr_("SPICE(FILEWRITEFAILED)", (ftnlen)22);
		chkout_("DASBT", (ftnlen)5);
		return 0;
	    }

/*           Read in NUMDTA integers. The desired data are specified by */
/*           beginning and ending indices into the array,inclusive: thus */
/*           the subtraction of 1 in the call. */

	    i__2 = dtabeg + numdta - 1;
	    dasrdi_(&handle, &dtabeg, &i__2, ibuffr);

/*           Encode and write out a buffer of integers. */

	    wrenci_(xfrlun, &numdta, ibuffr);
	    if (failed_()) {

/*              We want to check failed here because were in a loop. */
/*              We should exit the loop, and the routine, as soon as */
/*              an error is detected, so we don't continue doing things */
/*              for a long time. Attempt to close the binary DAS file */
/*              that we opened and then return to the caller. */

		dascls_(&handle);
		chkout_("DASBT", (ftnlen)5);
		return 0;
	    }

/*           Write out the end data block identifier, the block number, */
/*           and the data count for the block. */

	    s_copy(line, "END_INTEGER_BLOCK # #", (ftnlen)80, (ftnlen)21);
	    repmi_(line, "#", &numblk, line, (ftnlen)80, (ftnlen)1, (ftnlen)
		    80);
	    repmi_(line, "#", &numdta, line, (ftnlen)80, (ftnlen)1, (ftnlen)
		    80);
	    ci__1.cierr = 1;
	    ci__1.ciunit = *xfrlun;
	    ci__1.cifmt = "(A)";
	    iostat = s_wsfe(&ci__1);
	    if (iostat != 0) {
		goto L100014;
	    }
	    iostat = do_fio(&c__1, line, rtrim_(line, (ftnlen)80));
	    if (iostat != 0) {
		goto L100014;
	    }
	    iostat = e_wsfe();
L100014:
	    if (iostat != 0) {

/*              An error occurred, so close the binary DAS file, set an */
/*              appropriate error message, and return to the caller. */

		dascls_(&handle);
		setmsg_("Error writing to the DAS transfer file: #. IOSTAT ="
			" #.", (ftnlen)54);
		errfnm_("#", xfrlun, (ftnlen)1);
		errint_("#", &iostat, (ftnlen)1);
		sigerr_("SPICE(FILEWRITEFAILED)", (ftnlen)22);
		chkout_("DASBT", (ftnlen)5);
		return 0;
	    }

/*           Increment the data pointers and decrement the amount of data */
/*           left. */

	    dtabeg += numdta;
	    numlft -= numdta;
	}

/*        Write out the number of processed integer data blocks */
/*        processed, and the count of double precision data items. */

	s_copy(line, "TOTAL_INTEGER_BLOCKS # #", (ftnlen)80, (ftnlen)24);
	repmi_(line, "#", &numblk, line, (ftnlen)80, (ftnlen)1, (ftnlen)80);
	repmi_(line, "#", &nidata, line, (ftnlen)80, (ftnlen)1, (ftnlen)80);
	ci__1.cierr = 1;
	ci__1.ciunit = *xfrlun;
	ci__1.cifmt = "(A)";
	iostat = s_wsfe(&ci__1);
	if (iostat != 0) {
	    goto L100015;
	}
	iostat = do_fio(&c__1, line, rtrim_(line, (ftnlen)80));
	if (iostat != 0) {
	    goto L100015;
	}
	iostat = e_wsfe();
L100015:
	if (iostat != 0) {

/*           An error occurred, so close the binary DAS file, set an */
/*           appropriate error message, and return to the caller. */

	    dascls_(&handle);
	    setmsg_("Error writing to the DAS transfer file: #. IOSTAT = #.", 
		    (ftnlen)54);
	    errfnm_("#", xfrlun, (ftnlen)1);
	    errint_("#", &iostat, (ftnlen)1);
	    sigerr_("SPICE(FILEWRITEFAILED)", (ftnlen)22);
	    chkout_("DASBT", (ftnlen)5);
	    return 0;
	}
    }

/*     Close only the binary DAS file. */

    dascls_(&handle);
    chkout_("DASBT", (ftnlen)5);
    return 0;
} /* dasbt_ */

