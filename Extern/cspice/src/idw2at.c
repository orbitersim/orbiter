/* idw2at.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;

/* $Procedure IDW2AT ( Get file architecture and type from ID word ) */
/* Subroutine */ int idw2at_(char *idword, char *arch, char *type__, ftnlen 
	idword_len, ftnlen arch_len, ftnlen type_len)
{
    /* System generated locals */
    integer i__1;

    /* Builtin functions */
    integer s_cmp(char *, char *, ftnlen, ftnlen);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    char part1[8], part2[8];
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    integer slash;
    extern /* Subroutine */ int chkout_(char *, ftnlen);
    extern logical return_(void);
    extern integer pos_(char *, char *, integer *, ftnlen, ftnlen);

/* $ Abstract */

/*     Extract the architecture and type of a SPICE binary kernel file */
/*     from a file ID word. */

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

/*     KERNEL */
/*     UTILITY */

/* $ Declarations */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     IDWORD     I   The IDWORD to be examined. */
/*     ARCH       O   The file architecture DAS or DAF. */
/*     TYPE       O   The type of the file. */

/* $ Detailed_Input */

/*     IDWORD   is the ID word from a SPICE binary kernel file or a */
/*              text version of a binary kernel file whose */
/*              architecture and type are to be extracted. */

/* $ Detailed_Output */

/*     ARCH     is the file architecture used to store the data in */
/*              a SPICE binary kernel file. If the architecture cannot */
/*              be extracted or is not recognized the value '?' is */
/*              returned. */

/*              The possible architectures are: */

/*                 ASC -- An ASCII text file. */
/*                 DAF -- A DAF based file. */
/*                 DAS -- A DAS based file. */
/*                 KPL -- Kernel Pool File (i.e., a text kernel) */
/*                 TXT -- An ASCII text file. */
/*                 TE1 -- Text E-Kernel type 1. */


/*     TYPE     is the type of the SPICE file. If the type can not be */
/*              extracted or if it is blank, the value '?' is */
/*              returned. */

/*              The type can only be extracted by this routine if */
/*              the ID word follows the convention */

/*                 <architecture>/<type> */

/*              where <architecture> is one of the file architectures */
/*              specified above, and */

/*                 <type> = 'xxxx' */

/*              where 'xxxx' represents a four character mnemonic or */
/*              code for the file type. */

/*              This subroutine does not do any checking of the file */
/*              types. If a valid architecture is found and the type */
/*              is non-blank, that is what will be returned. It is up */
/*              to a higher level authority to determine whether a type */
/*              is valid. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the variable ID word is blank, both the architecture and */
/*         type will be unknown, specified by '?'. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This subroutine is a support utility routine that attempts */
/*     to extract the architecture and type of a file from its ID word. */
/*     It may not be possible to determine the type of the file from the */
/*     ID word alone. Older files which contain the ID words 'NAIF/NIP', */
/*     or 'NAIF/DAF' do not have sufficient information in the ID word to */
/*     determine the type of the file. A type for the ID word 'NAIF/DAS' */
/*     is always 'PRE ', since files with this ID word were pre-release */
/*     DAS files. */

/*     A file architecture can always be extracted from a valid SPICE */
/*     ID word. */

/*     This subroutine and the subroutine GETFAT (get file architecture */
/*     and type) are intimately related. Whenever one of them is modified */
/*     the other should be checked to see if the modifications affect it. */
/*     Whenever a new architecture is added, both of the subroutines are */
/*     affected. */

/* $ Examples */

/*     Suppose you wish to write a single routine for converting files */
/*     between text and binary formats. You can use this routine to */
/*     determine the architecture and type of the file and then pass the */
/*     file to the appropriate low level file conversion routine to */
/*     handle the actual conversion. */

/*        CALL IDW2AT ( IDWORD, ARCH, TYPE ) */

/*        IF ( ARCH .EQ. 'DAF' ) THEN */

/*           convert a DAF file */

/*        ELSE IF ( ARCH .EQ. 'DAS' ) THEN */

/*           convert a DAS file */

/*        ELSE */

/*           WRITE(*,*) 'File architecture not supported.' */

/*        END IF */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     K.R. Gehringer     (JPL) */

/* $ Version */

/* -    SPICELIB Version 2.1.0, 20-AUG-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 2.0.0, 26-OCT-1995 (KRG) */

/*        Changed the Version line from "Beta" to "SPICELIB" for the */
/*        current revisions. The subroutine was already in SPICELIB, */
/*        but the Version line said "Beta." */

/*        Added several new architectures: */

/*           KPL -- Kernel Pool File (i.e., a text kernel) */
/*           TXT -- An ASCII text file. */
/*           ASC -- An ASCII text file. */
/*           TE1 -- Text E-Kernel type 1. */

/*        Changed the response foe the ID word 'NAIF/DAS' to be */
/*        consistent with GETFAT. It now sets the architecture to 'DAS' */
/*        and the type to 'PRE', for pre-release version. */

/* -    SPICELIB Version 1.0.0, 30-SEP-1993 (KRG) */

/* -& */
/* $ Index_Entries */

/*     extract architecture and type from an id word */

/* -& */
/* $ Revisions */

/* -     SPICELIB Version 2.0.0, 26-OCT-1995 (KRG) */

/*         Changed the Version line from "Beta" to "SPICELIB" for the */
/*         current revisions. The subroutine was already in SPICELIB, */
/*         but the Version line said "Beta." */

/*         Added several new architectures: */

/*            KPL -- Kernel Pool File (i.e., a text kernel) */
/*            TXT -- An ASCII text file. */
/*            ASC -- An ASCII text file. */
/*            TE1 -- Text E-Kernel type 1. */

/*         Changed the response foe the ID word 'NAIF/DAS' to be */
/*         consistent with GETFAT. It now sets the architecture to 'DAS' */
/*         and the type to 'PRE', for pre-release version. */

/* -& */

/*     Spicelib Routines */


/*     Set the length of a SPICE file ID word. */


/*     Local Variables */


/*     Standard obligatory error handling stuff. */

    if (return_()) {
	return 0;
    } else {
	chkin_("IDW2AT", (ftnlen)6);
    }

/*     Check to see if we got a blank string for the ID word. If we did, */
/*     set the architecture and type to unknown. */

    if (s_cmp(idword, " ", idword_len, (ftnlen)1) == 0) {
	s_copy(arch, "?", arch_len, (ftnlen)1);
	s_copy(type__, "?", type_len, (ftnlen)1);
	chkout_("IDW2AT", (ftnlen)6);
	return 0;
    }

/*     Initialize the temporary storage variables that we use. */

    s_copy(part1, " ", (ftnlen)8, (ftnlen)1);
    s_copy(part2, " ", (ftnlen)8, (ftnlen)1);

/*     See if we can get the architecture and type from the ID word. */

/*     Look for a '/' in the string. If we can't find it, we don't */
/*     recognize the architecture or the type, so set the architecture */
/*     and type to unknown. */

    slash = pos_(idword, "/", &c__1, idword_len, (ftnlen)1);
    if (slash == 0) {
	s_copy(arch, "?", arch_len, (ftnlen)1);
	s_copy(type__, "?", type_len, (ftnlen)1);
	chkout_("IDW2AT", (ftnlen)6);
	return 0;
    }

/*     The part before the slash is the architecture or the word 'NAIF' */
/*     in older files and the part after the slash is the type of file or */
/*     the architecture in older files. */

    s_copy(part1, idword, (ftnlen)8, slash - 1);
    i__1 = slash;
    s_copy(part2, idword + i__1, (ftnlen)8, idword_len - i__1);

/*     Let's now do some testing to try and figure out what's going on. */

/*     First we look for the information in the ID word format: */

/*        <architecture>/<type>, */

/*    then we look for the things that begin with the word 'NAIF' */

    if (s_cmp(part1, "DAF", (ftnlen)8, (ftnlen)3) == 0) {

/*        We have a DAF file, so set the architecture and type. */

	s_copy(arch, "DAF", arch_len, (ftnlen)3);
	if (s_cmp(part2, " ", (ftnlen)8, (ftnlen)1) != 0) {
	    s_copy(type__, part2, type_len, (ftnlen)8);
	} else {
	    s_copy(type__, "?", type_len, (ftnlen)1);
	}
    } else if (s_cmp(part1, "DAS", (ftnlen)8, (ftnlen)3) == 0) {

/*        We have a DAS file, so set the architecture and type. */

	s_copy(arch, "DAS", arch_len, (ftnlen)3);
	if (s_cmp(part2, " ", (ftnlen)8, (ftnlen)1) != 0) {
	    s_copy(type__, part2, type_len, (ftnlen)8);
	} else {
	    s_copy(type__, "?", type_len, (ftnlen)1);
	}
    } else if (s_cmp(part1, "TXT", (ftnlen)8, (ftnlen)3) == 0) {

/*        We have an ASCII text file, so set the architecture and type. */

	s_copy(arch, "TXT", arch_len, (ftnlen)3);
	if (s_cmp(part2, " ", (ftnlen)8, (ftnlen)1) != 0) {
	    s_copy(type__, part2, type_len, (ftnlen)8);
	} else {
	    s_copy(type__, "?", type_len, (ftnlen)1);
	}
    } else if (s_cmp(part1, "ASC", (ftnlen)8, (ftnlen)3) == 0) {

/*        We have an ASCII text file, so set the architecture and type. */

	s_copy(arch, "TXT", arch_len, (ftnlen)3);
	if (s_cmp(part2, " ", (ftnlen)8, (ftnlen)1) != 0) {
	    s_copy(type__, part2, type_len, (ftnlen)8);
	} else {
	    s_copy(type__, "?", type_len, (ftnlen)1);
	}
    } else if (s_cmp(part1, "KPL", (ftnlen)8, (ftnlen)3) == 0) {

/*        We have a kernel pool file, so set the architecture and type. */

	s_copy(arch, "KPL", arch_len, (ftnlen)3);
	if (s_cmp(part2, " ", (ftnlen)8, (ftnlen)1) != 0) {
	    s_copy(type__, part2, type_len, (ftnlen)8);
	} else {
	    s_copy(type__, "?", type_len, (ftnlen)1);
	}
    } else if (s_cmp(part1, "NAIF", (ftnlen)8, (ftnlen)4) == 0) {

/*        We have a DAF (or NIP, these are equivalent) or DAS file, */
/*        identified by the value of PART2, but we have no idea what the */
/*        type is, unless the file is a DAS file, in which case it is a */
/*        pre-release EK file, since these are the only DAS files which */
/*        used the 'NAIF/DAS' ID word. */

/*        First, we determine the architecture from PART2, then if it is */
/*        DAF or NIP, we give up on the type. As mentioned above, if */
/*        PART2 contains DAS, we know a priori the type of the file. */

	if (s_cmp(part2, "DAF", (ftnlen)8, (ftnlen)3) == 0 || s_cmp(part2, 
		"NIP", (ftnlen)8, (ftnlen)3) == 0) {
	    s_copy(arch, "DAF", arch_len, (ftnlen)3);
	    s_copy(type__, "?", type_len, (ftnlen)1);
	} else if (s_cmp(part2, "DAS", (ftnlen)8, (ftnlen)3) == 0) {
	    s_copy(arch, "DAS", arch_len, (ftnlen)3);
	    s_copy(type__, "PRE", type_len, (ftnlen)3);
	} else {
	    s_copy(arch, "?", arch_len, (ftnlen)1);
	    s_copy(type__, "?", type_len, (ftnlen)1);
	}
    } else {
	s_copy(arch, "?", arch_len, (ftnlen)1);
	s_copy(type__, "?", type_len, (ftnlen)1);
    }
    chkout_("IDW2AT", (ftnlen)6);
    return 0;
} /* idw2at_ */

