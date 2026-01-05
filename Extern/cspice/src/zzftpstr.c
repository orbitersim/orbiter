/* zzftpstr.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__3 = 3;
static integer c__2 = 2;
static integer c__0 = 0;

/* $Procedure ZZFTPSTR ( Private --- Fetch FTP Validation String ) */
/* Subroutine */ int zzftpstr_(char *tstcom, char *lend, char *rend, char *
	delim, ftnlen tstcom_len, ftnlen lend_len, ftnlen rend_len, ftnlen 
	delim_len)
{
    /* Initialized data */

    static logical first = TRUE_;
    static char locdlm[1] = ":";
    static char loclnd[6] = "FTPSTR";
    static char locrnd[6] = "ENDFTP";

    /* System generated locals */
    address a__1[3], a__2[2];
    integer i__1[3], i__2[2], i__3;

    /* Builtin functions */
    /* Subroutine */ int s_cat(char *, char **, integer *, integer *, ftnlen),
	     s_copy(char *, char *, ftnlen, ftnlen);
    integer s_rnge(char *, integer, char *, integer);

    /* Local variables */
    char asc000[1], asc010[1], asc013[1], asc016[1], asc206[1], asc129[1];
    integer i__;
    extern /* Subroutine */ int suffix_(char *, integer *, char *, ftnlen, 
	    ftnlen);
    static char locstr[16];
    char testsq[5*6];

/* $ Abstract */

/*    Retrieve the components of the FTP validation string. */

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
/*     TSTCOM     O   The FTP test component string. */
/*     LEND       O   String that brackets TSTCOM on the left in a file. */
/*     REND       O   String that brackets TSTCOM on the right in a file. */
/*     DELIM      O   Delimiter that separates the pieces of TSTCOM. */

/* $ Detailed_Input */

/*     None. */

/* $ Detailed_Output */

/*     TSTCOM     is a string composed of clusters of characters that */
/*                are susceptible to FTP ASCII mode transfer corruption, */
/*                separated by the DELIM character.  For example: */

/*                    :<CLUSTR(1)>:<CLUSTR(2):... */
/*                                            ...<CLUSTR(N)>: */

/*                where <CLUSTR(I)> is one cluster of characters that */
/*                is subject to improper FTP corruption. The string */
/*                that is to receive this value should be SIZSTR */
/*                characters in length. */

/*     LEND,      are the two sequences of printing characters that */
/*     REND       bracket TSTCOM in the binary file.  Their purpose is */
/*                to permit proper detection of TSTCOM in the event */
/*                of compression or expansion, due to improper FTP */
/*                transfer.  The variables which are to receive these */
/*                values should be SIZEND characters in length. */

/*     DELIM      is the printing character delimiter that separates the */
/*                test character clusters from one another, as well as */
/*                LEND and REND.  Since it is often the case that pairs */
/*                or triples of non-printing characters will trigger */
/*                FTP corruption, this delimiter blocks any unintended */
/*                interaction. */

/* $ Parameters */

/*     1) See include file zzftprms.inc */

/*     2) Since inserting non-printing characters into strings is a */
/*        somewhat arduous task requiring extensive use of the intrinsic */
/*        CHAR, integer parameters that map to the needed ASCII codes are */
/*        defined with variable names INT###, where ### is replaced with */
/*        the three digit ASCII integer code.  For each such integer */
/*        code, there is a corresponding character parameter whose name */
/*        is of the form ASC###.  For example: */

/*           INT010 = 10  -> ASC010 = <10> or <LF> */
/*           INT206 = 206 -> ASC206 = <206> */

/*        where <#> refers to CHAR(#) or CHAR(ICHAR('#')) in the case of */
/*        LF(line feed). */

/*        These naming conventions should be preserved when the FTP */
/*        validation string is updated. */

/* $ Files */

/*     While this routine is designed to aid in the detection of */
/*     improper FTP transfers, it simply returns the candidate */
/*     string for validation and does not interact with any */
/*     files directly. */

/* $ Exceptions */

/*     Error Free. */

/* $ Particulars */

/*     To minimize code alterations in the event of a string update, */
/*     the calling routine that declares the variables to receive */
/*     the strings stored here should include zzftprms.inc and utilize */
/*     the size parameters defined there as recommended in the Detailed */
/*     I/O sections above. */

/*     This private SPICELIB routine is designed to centralize the */
/*     definition of the FTP validation string present in binary */
/*     SPICE kernels.  If in the process of FTP'ing a binary */
/*     file from one platform to another, the user neglects to */
/*     invoke the IMAGE (BINARY) transfer mode, an ASCII mode */
/*     transfer may occur.  As this at the very least may substitute */
/*     one set of line terminators for another, corruption of the */
/*     binary file is likely.  By placing a string that encapsulates */
/*     a representative set of these character sequences that are */
/*     susceptible to corruption in the file record, it is possible */
/*     to trap and report any problems to the user when corrupted */
/*     kernels are loaded at run time. */

/*     To that end, analysis of evidence obtained by moving test binary */
/*     files from one platform to another indicates the following */
/*     clusters of ASCII codes are likely candidates for corruption: */

/*        Test Clusters: */

/*        <13>      - Text line terminator on Macintosh-based platforms. */
/*        <10>      - Text line terminator on UNIX-based platforms. */
/*        <13><10>  - Text line terminator on Microsoft platforms. */
/*        <13><0>   - Sequence of characters that maps into <13> on some */
/*                    UNIX-based systems. (HP, SGI, NEXT) */
/*        <129>     - Macintosh based systems permute ASCII values whose */
/*                    parity bit is set.  Codes in excess of ASCII */
/*                    127 are altered. */
/*        <16><206> - Some ancient FTP servers on PC's convert this */
/*                    sequence of ASCII characters to <16><16><206>. */

/*     The examples above show that substitution of one set of line */
/*     terminators for another can result in expansion or compression of */
/*     certain sequences of bytes.  If the clusters were juxtaposed, new */
/*     sequences of adjacent bytes, themselves subject to transformation, */
/*     might be formed.  So the FTP test string present in the binary */
/*     file should have some mechanism for preventing interaction between */
/*     the clusters.  The test string should also be constructed so that */
/*     it can be easily located in the event compression or expansion, */
/*     either internally or elsewhere in the file record, shifts it away */
/*     from its default location. */

/*     So by separating these clusters with a printable delimiter, then */
/*     bracketing the entire test string with start and stop identifiers, */
/*     we have a reasonable mechanism for locating and analyzing any */
/*     potential FTP corruption. Then the sequence of characters to be */
/*     inserted into the file will appear as: */

/*        FTPSTR:<13>:<10>:<13><10>:<13><0>:<129>:<16><206>:ENDFTP */

/*     where 'FTPSTR' and 'ENDFTP' are the bracketing substrings and */
/*     ':' is the delimiting character. */

/*     By no means do we claim that these are the complete set of */
/*     clusters that are corruptible through an improper FTP transfer. */
/*     An update procedure is provided in the Revisions section just */
/*     after the routine header.  Following this procedure will require */
/*     the least amount of effort to prevent older files from falsely */
/*     indicating corruption under new Toolkits, as well as newer files */
/*     failing on old Toolkits. */


/* $ Examples */

/*     This routine just fetches the components of the FTP validation */
/*     string. */

/* $ Restrictions */

/*     1) TSTCOM, LEND, REND, and DELIM must be large enough to hold */
/*        the entire values returned by this routine, otherwise */
/*        truncation will occur. */

/* $ Author_and_Institution */

/*     F.S. Turner     (JPL) */

/* $ Literature_References */

/*     None. */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 21-MAR-1999 (FST) */


/* -& */
/* $ Index_Entries */

/*     fetch the ftp validation string components */

/* -& */
/* $ Revisions */

/* -    SPICELIB Version 1.0.0, 21-MAR-1999 (FST) */

/*        FTP validation string update procedure: */

/*           (1) Leave 'FTPSTR', 'ENDFTP', and ':' alone, as */
/*               their alteration will require special */
/*               consideration for older files. */

/*           (2) Leave the existing test clusters in the */
/*               existing order, and place any new clusters */
/*               between the last ':' and the E in 'ENDFTP'. */
/*               Make certain these are ':' delimited as well. */

/*           (3) Modify the contents of zzftprms.inc to */
/*               indicate the new sizes of the various string */
/*               components. Routines that include this must */
/*               then be recompiled. */

/* -& */

/*     Local Parameters */

/*     Maximum size of an individual test cluster component */
/*     including the ':'. */


/*     Integer codes of characters appearing in test clusters. */



/*     Local Variables */


/*     Non-printing character values. */


/*     Saved Variables */


/*     Data Statements */


/*     Set up the components of the FTP validation string that */
/*     are not supposed to change for forward and backward */
/*     compatibility. */


/*     On the first invocation initialize the string values. */

    if (first) {

/*        Convert the integer parameters to their non-printing ASCII */
/*        equivalents. */

	*(unsigned char *)asc000 = '\0';
	*(unsigned char *)asc010 = '\n';
	*(unsigned char *)asc013 = '\r';
	*(unsigned char *)asc016 = '\20';
	*(unsigned char *)asc129 = 129;
	*(unsigned char *)asc206 = 206;

/*        Now build the individual components of the test clusters. */
/*        Make certain the first component begins and ends with a ':', */
/*        and that the remaining pieces end in ':'. If you intend to */
/*        add some clusters, then append them to the end of the */
/*        sequence so as not to break the existing detection code. */


/*        Cluster #1 : <CR> - <13> - Macintosh Line Terminator */

/* Writing concatenation */
	i__1[0] = 1, a__1[0] = locdlm;
	i__1[1] = 1, a__1[1] = asc013;
	i__1[2] = 1, a__1[2] = locdlm;
	s_cat(testsq, a__1, i__1, &c__3, (ftnlen)5);

/*        Cluster #2 : <LF> - <10> - Unix Line Terminator */

/* Writing concatenation */
	i__2[0] = 1, a__2[0] = asc010;
	i__2[1] = 1, a__2[1] = locdlm;
	s_cat(testsq + 5, a__2, i__2, &c__2, (ftnlen)5);

/*        Cluster #3 : <CR><LF> - <10><13> - Microsoft Line Terminator */

/* Writing concatenation */
	i__1[0] = 1, a__1[0] = asc013;
	i__1[1] = 1, a__1[1] = asc010;
	i__1[2] = 1, a__1[2] = locdlm;
	s_cat(testsq + 10, a__1, i__1, &c__3, (ftnlen)5);

/*        Cluster #4 : <13><0> */

/* Writing concatenation */
	i__1[0] = 1, a__1[0] = asc013;
	i__1[1] = 1, a__1[1] = asc000;
	i__1[2] = 1, a__1[2] = locdlm;
	s_cat(testsq + 15, a__1, i__1, &c__3, (ftnlen)5);

/*        Cluster #5 : <129> - Macintosh Permutation of Parity Codes */

/* Writing concatenation */
	i__2[0] = 1, a__2[0] = asc129;
	i__2[1] = 1, a__2[1] = locdlm;
	s_cat(testsq + 20, a__2, i__2, &c__2, (ftnlen)5);

/*        Cluster #6 : <16><206> */

/* Writing concatenation */
	i__1[0] = 1, a__1[0] = asc016;
	i__1[1] = 1, a__1[1] = asc206;
	i__1[2] = 1, a__1[2] = locdlm;
	s_cat(testsq + 25, a__1, i__1, &c__3, (ftnlen)5);

/*        Sample cluster addition code follows */

/*        Cluster #7 : <xxx> - Description */

/*        TESTSQ(7) = ASCxxx // ... // LOCDLM */


/*        Now build the local copy of TSTCOM, LOCSTR. First clear the */
/*        uninitialized contents. */

	s_copy(locstr, " ", (ftnlen)16, (ftnlen)1);
	for (i__ = 1; i__ <= 6; ++i__) {

/*           Append TESTSQ(I) to LOCSTR to properly construct the */
/*           test component of the FTP validation string. */

	    suffix_(testsq + ((i__3 = i__ - 1) < 6 && 0 <= i__3 ? i__3 : 
		    s_rnge("testsq", i__3, "zzftpstr_", (ftnlen)399)) * 5, &
		    c__0, locstr, (ftnlen)5, (ftnlen)16);
	}

/*        Prevent execution of this initialization code after first pass. */

	first = FALSE_;
    }

/*     Copy the local copies of the FTP string components to the */
/*     arguments passed in from the caller. */

    s_copy(tstcom, locstr, tstcom_len, (ftnlen)16);
    s_copy(lend, loclnd, lend_len, (ftnlen)6);
    s_copy(rend, locrnd, rend_len, (ftnlen)6);
    s_copy(delim, locdlm, delim_len, (ftnlen)1);
    return 0;
} /* zzftpstr_ */

