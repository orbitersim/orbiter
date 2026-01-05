/* zzftpchk.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;

/* $Procedure ZZFTPCHK ( Private --- Check for FTP Errors ) */
/* Subroutine */ int zzftpchk_(char *string, logical *ftperr, ftnlen 
	string_len)
{
    /* Initialized data */

    static logical first = TRUE_;

    extern /* Subroutine */ int zzrbrkst_(char *, char *, char *, char *, 
	    integer *, logical *, ftnlen, ftnlen, ftnlen, ftnlen), zzftpstr_(
	    char *, char *, char *, char *, ftnlen, ftnlen, ftnlen, ftnlen);
    char delim[1];
    extern integer rtrim_(char *, ftnlen);
    integer length;
    static char lftbkt[6];
    integer fsmidx, msfidx;
    static char rgtbkt[6];
    logical isther;
    char filstr[48];
    static char memstr[16];
    extern integer pos_(char *, char *, integer *, ftnlen, ftnlen);

/* $ Abstract */

/*    Check a character string that may contain the FTP validation */
/*    string for FTP based errors. */

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

/*     FILE */
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


/*     Include Section:  Private FTP Validation String Parameters */

/*        zzftprms.inc Version 1    01-MAR-1999 (FST) */

/*     This include file centralizes the definition of string sizes */
/*     and other parameters that are necessary to properly implement */
/*     the FTP error detection scheme for binary kernels. */

/*     Before making any alterations to the contents of this file, */
/*     refer to the header of ZZFTPSTR for a detailed discussion of */
/*     the FTP validation string. */

/*     Size of FTP Test String Component: */


/*     Size of Maximum Expanded FTP Validation String: */

/*      (This indicates the size of a buffer to hold the test */
/*       string sequence from a possibly corrupt file. Empirical */
/*       evidence strongly indicates that expansion due to FTP */
/*       corruption at worst doubles the number of characters. */
/*       So take 3*SIZSTR to be on the safe side.) */


/*     Size of FTP Validation String Brackets: */


/*     Size of FTP Validation String: */


/*     Size of DELIM. */


/*     Number of character clusters present in the validation string. */


/*     End Include Section:  Private FTP Validation String Parameters */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     STRING     I   String that may contain the FTP validation string. */
/*     FTPERR     O   Logical indicating if FTP corruption occurred. */

/* $ Detailed_Input */

/*     STRING     is a string read in directly from a binary file.  This */
/*                string should, but does not have to, contain the FTP */
/*                validation string.  Typically this block of characters */
/*                is read in from the file record of the binary kernel. */
/*                If 'FTPSTR' or 'ENDFTP' occur anywhere in STRING, then */
/*                validation will be attempted.  Multiple occurrences of */
/*                these two special strings in STRING is also an issue. */
/*                See Restrictions for details. */

/* $ Detailed_Output */

/*     FTPERR     is a logical that indicates whether or not an FTP */
/*                error has occurred.  If an error is detected FTPERR */
/*                is set to TRUE, otherwise FTPERR is FALSE.  In the */
/*                event that STRING does not contain either of the FTP */
/*                bracketing strings, then the test will not be */
/*                performed.  Thus, FTPERR is set to FALSE. */

/* $ Parameters */

/*     See include file zzftprms.inc. */

/* $ Exceptions */

/*     1) In the event that both the left and right end markers of the */
/*        FTP validation string are not present in STRING, the routine */
/*        assumes that this information is from a pre-FTP test file.  As */
/*        such, the file can not be validated, so FTPERR remains FALSE. */

/*     2) If the FTP string brackets 'FTPSTR' and 'ENDFTP' are present in */
/*        multiple places in the text block, then this routine assumes */
/*        the last occurrence of the substring these strings bracket is */
/*        the FTP test sequence that requires validation.  So if this */
/*        routine encounters a block of text: */

/*        ...FTPSTR:<TESTSQ>:ENDFTP...FTPSTR[THISISNOTATEST]ENDFTP... */

/*        where <TESTSQ> is the actual test sequence, then it will */
/*        incorrectly compare [THISISNOTATEST] to the test component */
/*        returned from ZZFTPSTR. */

/* $ Files */

/*     Although this routine validates information from a binary file, */
/*     it does not interact with the file directly, and relies upon */
/*     the caller to pass the appropriate string block. */

/* $ Particulars */

/*     The purpose of this routine is to examine for FTP errors a */
/*     string brought in from a binary kernel.  This text may or may */
/*     not contain the FTP validation string defined in ZZFTPSTR. */
/*     However, if it contains at least one of the two bracketing */
/*     substrings ('FTPSTR' and 'ENDFTP'), then the routine assumes */
/*     that the text is subject to FTP validation. As a result of this, */
/*     the caller should avoid passing in user controlled chunks of */
/*     character data from the file.  If the user has decided to place */
/*     one of the FTP string bracket components in this portion of the */
/*     file, then ZZFTPCHK may be confused and incorrectly indicate an */
/*     error condition. */

/* $ Examples */

/*     The following code fragment from DAFOPR reads in the DAF file */
/*     record and attempts to examine the contents for FTP errors. */
/*     (Note: this code fragment is from a 32 bit word length, 1 */
/*     byte character environment.) */

/*     C */
/*     C     Check for FTP transfer errors to prevent the user from */
/*     C     inadvertently using a damaged kernel. First read the file */
/*     C     record into a string of 1000 characters. */
/*     C */
/*           READ ( UNIT = LUN, REC = 1, IOSTAT = IOSTAT ) FTPTST */

/*           IF ( IOSTAT .NE. 0 ) THEN */

/*              CLOSE       ( LUN                                     ) */
/*              CALL SETMSG ( 'Error reading the file record from'   // */
/*          .                 ' the binary DAF file ''#''. IOSTAT'   // */
/*          .                 ' = #.'                                 ) */
/*              CALL ERRCH  ( '#', FNAME                              ) */
/*              CALL ERRINT ( '#', IOSTAT                             ) */
/*              CALL SIGERR ( 'SPICE(FILEREADFAILED)'                 ) */
/*              CALL CHKOUT ( 'DAFOPR'                                ) */
/*              RETURN */

/*           END IF */

/*     C */
/*     C     Since we are dealing with DAF files, only place the */
/*     C     last 500 characters of data from the file record into */
/*     C     ZZFTPCHK.  This ensures that the internal filename */
/*     C     and the ID word do not interfere with the FTP validation */
/*     C     process. */
/*     C */
/*           CALL ZZFTPCHK ( FTPTST(501:1000), FTPERR ) */

/*           IF ( FTPERR ) THEN */

/*              CLOSE       ( LUN                                      ) */
/*              CALL SETMSG ( 'FTP transfer error.  This binary DAF, '// */
/*          .                 '''#'', has most likely been corrupted '// */
/*          .                 'by an ASCII mode FTP transfer. '       // */
/*          .                 'Re-obtain the file using IMAGE or '    // */
/*          .                 'BINARY transfer mode.'                  ) */
/*              CALL ERRCH  ( '#', FNAME                               ) */
/*              CALL SIGERR ( 'SPICE(FTPXFERERROR)'                    ) */
/*              CALL CHKOUT ( 'DAFOPR'                                 ) */
/*              RETURN */

/*           END IF */

/* $ Restrictions */

/*     1) STRING may contain multiple occurrences of the FTP bracketing */
/*        substrings ('FTPSTR' and 'ENDFTP'), but only if the last */
/*        occurrence of both brackets the actual data for validation. */

/*     2) This routine assumes the presence of either 'FTPSTR' or */
/*        'ENDFTP' in STRING indicates that validation is to be */
/*        attempted. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     F.S. Turner     (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.1, 02-OCT-2021 (NJB) */

/*        Fixed typo in comment. Reordered header sections. */

/* -    SPICELIB Version 1.0.0, 22-MAR-1999 (FST) */


/* -& */
/* $ Index_Entries */

/*     check text block for FTP errors */

/* -& */
/* $ Revisions */

/* -    SPICELIB Version 1.0.0, 22-MAR-1999 (FST) */

/*        This routine does not require modification if the FTP */
/*        validation string is updated according to the guidelines */
/*        laid out in ZZFTPSTR.  The reason for this is the */
/*        verification algorithm extracts the chunk of text between */
/*        'FTPSTR' and 'ENDFTP'.  It then checks to see whether or */
/*        not this chunk is a subset of the test component stored */
/*        in ZZFTPSTR. Two cases: */

/*           (1) It is.  Then this indicates that at the worst, */
/*               the chunk is from a valid file with an earlier */
/*               version of the FTP validation string. */

/*           (2) It is not.  While this is a fair indication that */
/*               the file may be corrupt, it's not a complete */
/*               treatment, since we may be examining a file */
/*               created with a newer version of the FTP validation */
/*               string.  So now check to see whether that test */
/*               component from ZZFTPSTR is a subset of the text */
/*               chunk from STRING.  If it is, then the file is */
/*               as valid as far as this version of the toolkit is */
/*               concerned.  Otherwise, the file is damaged. */

/* -& */

/*     SPICELIB Functions */


/*     Local Variables */


/*     Saved Variables */


/*     Data Statements */


/*     On the first pass through, fetch a copy of the current FTP */
/*     validation string. */

    if (first) {
	zzftpstr_(memstr, lftbkt, rgtbkt, delim, (ftnlen)16, (ftnlen)6, (
		ftnlen)6, (ftnlen)1);

/*        Don't fetch the string on subsequent calls to this routine. */

	first = FALSE_;
    }

/*     Extract the FTP validation string from the block of text that */
/*     was passed into the routine via the argument STRING. Note, */
/*     if the bracketed substring in the text block STRING is larger */
/*     than the FILSTR string size, ZZRBRKST will truncate the data */
/*     that does not fit.  This loss of data is not an issue, since in */
/*     this case we may only validate the part of the substring near */
/*     the head, for which we have enough room in FILSTR. */

    zzrbrkst_(string, lftbkt, rgtbkt, filstr, &length, &isther, string_len, 
	    rtrim_(lftbkt, (ftnlen)6), rtrim_(rgtbkt, (ftnlen)6), (ftnlen)48);

/*     Now check ISTHER to see if either LFTBKT or RGTBKT was present */
/*     in the block of text from the file. If both are absent, then */
/*     we must assume that this text is from a pre-FTP validation file, */
/*     and as such do not return any indication of an error. */

    if (! isther) {
	*ftperr = FALSE_;

/*     If one of the brackets is present, then we may proceed with */
/*     validation.  First check to see if the length is 0.  If it is, */
/*     then at least one of the brackets was present, but ZZRBRKST was */
/*     unable to extract a properly bracketed substring.  This is an */
/*     error. */

    } else if (length <= 0) {
	*ftperr = TRUE_;

/*     Now we make it to this ELSE statement only if ISTHER is TRUE, and */
/*     LENGTH is a positive number.  Compare the contents of FILSTR */
/*     and MEMSTR. */

    } else {

/*        First determine if the data from the file is a subset of */
/*        what is stored in memory. */

	fsmidx = pos_(memstr, filstr, &c__1, (ftnlen)16, rtrim_(filstr, (
		ftnlen)48));

/*        In the event that FSMIDX is non-zero, we know that FILSTR */
/*        is a substring of MEMSTR, and thus we have validated all the */
/*        test clusters from the file. */

	if (fsmidx != 0) {
	    *ftperr = FALSE_;

/*        If FSMIDX is 0, then we do not yet know whether or not the */
/*        file is valid.  Now it may be the case that this file contains */
/*        a newer FTP validation string than this version of the */
/*        toolkit is aware.  Check to see whether what's in memory */
/*        is a substring of what's in FILSTR. */

	} else {
	    msfidx = pos_(filstr, memstr, &c__1, (ftnlen)48, rtrim_(memstr, (
		    ftnlen)16));

/*           If this comes back as zero, then we definitely have */
/*           an FTP error. Set FTPERR appropriately. */

	    *ftperr = msfidx == 0;
	}
    }
    return 0;
} /* zzftpchk_ */

