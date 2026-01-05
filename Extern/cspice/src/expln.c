/* expln.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure EXPLN ( Get Explanation for Short Error Message ) */
/* Subroutine */ int expln_(char *msg, char *expl, ftnlen msg_len, ftnlen 
	expl_len)
{
    /* Builtin functions */
    integer s_cmp(char *, char *, ftnlen, ftnlen);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

/* $ Abstract */

/*     Return the explanation of a short error message. */

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

/*     ERROR */

/* $ Keywords */

/*     ERROR */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     MSG        I   A short error message. */
/*     EXPL       O   The explanation of the short error message. */

/* $ Detailed_Input */

/*     MSG      is a "short" error message. */
/*              MSG indicates the type of error that has occurred. */

/*              The exact format that MSG must follow is */
/*              described in the required reading file, error.req. */

/* $ Detailed_Output */

/*     EXPL     is a character string containing an one-line */
/*              explanation of the short error message, MSG. */

/*              If there is no explanatory text corresponding */
/*              to the input string, MSG, EXPL is blank. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  This routine does not detect any errors. */

/*         However, this routine is part of the interface to the SPICELIB */
/*         error handling mechanism. For this reason, this routine does */
/*         not participate in the trace scheme, even though it has */
/*         external references. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     None. */

/* $ Examples */

/*     C */
/*     C     We want to find the explanation corresponding to */
/*     C     the short message, SPICE(ZERORADIUS) : */
/*     C */

/*            CALL EXPLN ( SPICE(ZERORADIUS), EXPL ) */


/*     Now, EXPL  = */

/*     'Invalid Radius--Equatorial or Polar Radius is Zero' */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     B.V. Semenov       (JPL) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 12-AUG-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 1.0.2, 18-APR-2014 (BVS) */

/*        Minor header edits. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (NJB) */

/* -& */
/* $ Index_Entries */

/*     get explanation for short error message */

/* -& */
/* $ Revisions */

/* -    Beta Version 1.1.0, 27-OCT-1988 (NJB) */

/*        Removed code used to create upper case, left-justified */
/*        copy of the short error message. The resulting message */
/*        was not used. */

/* -& */

/*     Executable Code: */


/*     Note: the short error messages should be ordered */
/*     alphabetically. */

    if (s_cmp(msg, "SPICE(BADENDPOINTS)", msg_len, (ftnlen)19) == 0) {
	s_copy(expl, "Invalid Endpoints--Left Endpoint Exceeds Right Endpoint"
		, expl_len, (ftnlen)55);
    } else if (s_cmp(msg, "SPICE(BADGEFVERSION)", msg_len, (ftnlen)20) == 0) {
	s_copy(expl, "Version Identification of GEF File is Invalid", 
		expl_len, (ftnlen)45);
    } else if (s_cmp(msg, "SPICE(BLANKMODULENAME)", msg_len, (ftnlen)22) == 0)
	     {
	s_copy(expl, "A blank string was used as a module name", expl_len, (
		ftnlen)40);
    } else if (s_cmp(msg, "SPICE(BOGUSENTRY)", msg_len, (ftnlen)17) == 0) {
	s_copy(expl, "This Entry Point Contains No Executable Code", expl_len,
		 (ftnlen)44);
    } else if (s_cmp(msg, "SPICE(CELLTOOSMALL)", msg_len, (ftnlen)19) == 0) {
	s_copy(expl, "Cardinality of Output Cell is Too Small", expl_len, (
		ftnlen)39);
    } else if (s_cmp(msg, "SPICE(CLUSTERWRITEERROR)", msg_len, (ftnlen)24) == 
	    0) {
	s_copy(expl, "Error Writing to Ephemeris File", expl_len, (ftnlen)31);
    } else if (s_cmp(msg, "SPICE(DATATYPENOTRECOG)", msg_len, (ftnlen)23) == 
	    0) {
	s_copy(expl, "Unrecognized Data Type Specification was Encountered", 
		expl_len, (ftnlen)52);
    } else if (s_cmp(msg, "SPICE(DATEEXPECTED)", msg_len, (ftnlen)19) == 0) {
	s_copy(expl, "The Value in the Kernel File was Expected to be a date."
		, expl_len, (ftnlen)55);
    } else if (s_cmp(msg, "SPICE(DEVICENAMETOOLONG)", msg_len, (ftnlen)24) == 
	    0) {
	s_copy(expl, "Name of Device Exceeds 128-Character Limit", expl_len, (
		ftnlen)42);
    } else if (s_cmp(msg, "SPICE(EMBEDDEDBLANK)", msg_len, (ftnlen)20) == 0) {
	s_copy(expl, "Invalid embedded blank was found in character string", 
		expl_len, (ftnlen)52);
    } else if (s_cmp(msg, "SPICE(FILEALREADYOPEN)", msg_len, (ftnlen)22) == 0)
	     {
	s_copy(expl, "File Open Failed Because the File was Already Open", 
		expl_len, (ftnlen)50);
    } else if (s_cmp(msg, "SPICE(FILEOPENFAILED)", msg_len, (ftnlen)21) == 0) 
	    {
	s_copy(expl, "An Attempt to Open a File Failed", expl_len, (ftnlen)32)
		;
    } else if (s_cmp(msg, "SPICE(FILEREADFAILED)", msg_len, (ftnlen)21) == 0) 
	    {
	s_copy(expl, "An Attempt to Read a File Failed", expl_len, (ftnlen)32)
		;
    } else if (s_cmp(msg, "SPICE(FILEWRITEFAILED)", msg_len, (ftnlen)22) == 0)
	     {
	s_copy(expl, "An Attempt to Write a File Failed", expl_len, (ftnlen)
		33);
    } else if (s_cmp(msg, "SPICE(INCOMPATIBLEUNITS)", msg_len, (ftnlen)24) == 
	    0) {
	s_copy(expl, "The Input and Output Units are Incompatible", expl_len, 
		(ftnlen)43);
    } else if (s_cmp(msg, "SPICE(INVALIDACTION)", msg_len, (ftnlen)20) == 0) {
	s_copy(expl, "An Invalid Action Value Was Supplied", expl_len, (
		ftnlen)36);
    } else if (s_cmp(msg, "SPICE(INVALIDARGUMENT)", msg_len, (ftnlen)22) == 0)
	     {
	s_copy(expl, "An Invalid Function Argument was Supplied", expl_len, (
		ftnlen)41);
    } else if (s_cmp(msg, "SPICE(INVALIDCHECKOUT)", msg_len, (ftnlen)22) == 0)
	     {
	s_copy(expl, "Checkout Was Attempted When No Routines Were Checked In"
		, expl_len, (ftnlen)55);
    } else if (s_cmp(msg, "SPICE(INVALIDCLUSTERNUM)", msg_len, (ftnlen)24) == 
	    0) {
	s_copy(expl, "Invalid Cluster Number -- Cluster Numbers Must Exceed "
		"1 ", expl_len, (ftnlen)56);
    } else if (s_cmp(msg, "SPICE(INVALIDEPOCH)", msg_len, (ftnlen)19) == 0) {
	s_copy(expl, "An Invalid Epoch Type Specification Was Supplied", 
		expl_len, (ftnlen)48);
    } else if (s_cmp(msg, "SPICE(INVALIDINDEX)", msg_len, (ftnlen)19) == 0) {
	s_copy(expl, "There Is No Element Corresponding to the Supplied Index"
		, expl_len, (ftnlen)55);
    } else if (s_cmp(msg, "SPICE(INVALIDTIMESTRING)", msg_len, (ftnlen)24) == 
	    0) {
	s_copy(expl, "Time String Could Not Be Parsed", expl_len, (ftnlen)31);
    } else if (s_cmp(msg, "SPICE(INVALIDLISTITEM)", msg_len, (ftnlen)22) == 0)
	     {
	s_copy(expl, "An Invalid Item Was Found in a List", expl_len, (ftnlen)
		35);
    } else if (s_cmp(msg, "SPICE(INVALIDMSGTYPE)", msg_len, (ftnlen)21) == 0) 
	    {
	s_copy(expl, "An Invalid Error Message Type Was Specified", expl_len, 
		(ftnlen)43);
    } else if (s_cmp(msg, "SPICE(INVALIDOPERATION)", msg_len, (ftnlen)23) == 
	    0) {
	s_copy(expl, "An Invalid Operation Value Was Supplied", expl_len, (
		ftnlen)39);
    } else if (s_cmp(msg, "SPICE(INVALIDOPTION)", msg_len, (ftnlen)20) == 0) {
	s_copy(expl, "An Invalid Option Value Was Supplied", expl_len, (
		ftnlen)36);
    } else if (s_cmp(msg, "SPICE(INVALIDTIMEFORMAT)", msg_len, (ftnlen)24) == 
	    0) {
	s_copy(expl, "Specification of Time String Format Was Not Recognized",
		 expl_len, (ftnlen)54);
    } else if (s_cmp(msg, "SPICE(KERNELVARNOTFOUND)", msg_len, (ftnlen)24) == 
	    0) {
	s_copy(expl, "The Variable Was not Found in the Kernel Pool.", 
		expl_len, (ftnlen)46);
    } else if (s_cmp(msg, "SPICE(NAMETABLEFULL)", msg_len, (ftnlen)20) == 0) {
	s_copy(expl, "No Further Symbols Can be Inserted; the Name Table is "
		"Full", expl_len, (ftnlen)58);
    } else if (s_cmp(msg, "SPICE(NOFREELOGICALUNIT)", msg_len, (ftnlen)24) == 
	    0) {
	s_copy(expl, "No More Logical Units are Available for Allocation", 
		expl_len, (ftnlen)50);
    } else if (s_cmp(msg, "SPICE(NOINTERVAL)", msg_len, (ftnlen)17) == 0) {
	s_copy(expl, "Window Does Not Contain Interval Corresponding to the "
		"Supplied Index", expl_len, (ftnlen)68);
    } else if (s_cmp(msg, "SPICE(NOSEGMENT)", msg_len, (ftnlen)16) == 0) {
	s_copy(expl, "No Applicable Segment Found in Ephemeris File", 
		expl_len, (ftnlen)45);
    } else if (s_cmp(msg, "SPICE(NOSUCHSYMBOL)", msg_len, (ftnlen)19) == 0) {
	s_copy(expl, "The Symbol Does Not Exist in the Symbol Table", 
		expl_len, (ftnlen)45);
    } else if (s_cmp(msg, "SPICE(NOTDISTINCT)", msg_len, (ftnlen)18) == 0) {
	s_copy(expl, "The Elements Must Be Distinct", expl_len, (ftnlen)29);
    } else if (s_cmp(msg, "SPICE(NUMBEREXPECTED)", msg_len, (ftnlen)21) == 0) 
	    {
	s_copy(expl, "The Value in the Kernel File was Expected to be a Numb"
		"er.", expl_len, (ftnlen)57);
    } else if (s_cmp(msg, "SPICE(POINTERTABLEFULL)", msg_len, (ftnlen)23) == 
	    0) {
	s_copy(expl, "No Further Symbols Can be Inserted; the Pointer Table "
		"is Full", expl_len, (ftnlen)61);
    } else if (s_cmp(msg, "SPICE(REFNOTREC)", msg_len, (ftnlen)16) == 0) {
	s_copy(expl, "A Reference Frame Specification was Not Recognized", 
		expl_len, (ftnlen)50);
    } else if (s_cmp(msg, "SPICE(SETEXCESS)", msg_len, (ftnlen)16) == 0) {
	s_copy(expl, "Cardinality of Set Is Too Small to Contain Result of t"
		"he Requested Operation", expl_len, (ftnlen)76);
    } else if (s_cmp(msg, "SPICE(TOOMANYFILESOPEN)", msg_len, (ftnlen)23) == 
	    0) {
	s_copy(expl, "The SPICELIB Limit for Number of Open Files Has Alread"
		"y Been Reached", expl_len, (ftnlen)68);
    } else if (s_cmp(msg, "SPICE(TRACEBACKOVERFLOW)", msg_len, (ftnlen)24) == 
	    0) {
	s_copy(expl, "No More Entries Can Be Added to the Traceback Represen"
		"tation", expl_len, (ftnlen)60);
    } else if (s_cmp(msg, "SPICE(UNITSNOTREC)", msg_len, (ftnlen)18) == 0) {
	s_copy(expl, "The Input or Output Units Were Not Recognized", 
		expl_len, (ftnlen)45);
    } else if (s_cmp(msg, "SPICE(UNMATCHENDPTS)", msg_len, (ftnlen)20) == 0) {
	s_copy(expl, "Window Does Not Have an Even Number of Endpoints", 
		expl_len, (ftnlen)48);
    } else if (s_cmp(msg, "SPICE(VALUETABLEFULL)", msg_len, (ftnlen)21) == 0) 
	    {
	s_copy(expl, "No Further Symbols Can be Inserted; the Value Table is"
		" Full", expl_len, (ftnlen)59);
    } else if (s_cmp(msg, "SPICE(WINDOWEXCESS)", msg_len, (ftnlen)19) == 0) {
	s_copy(expl, "Cardinality of Window Is Too Small to Contain Result o"
		"f the Requested Operation", expl_len, (ftnlen)79);
    } else if (s_cmp(msg, "SPICE(WINDOWTOOSMALL)", msg_len, (ftnlen)21) == 0) 
	    {
	s_copy(expl, "Cardinality of Output Window is Too Small", expl_len, (
		ftnlen)41);
    } else if (s_cmp(msg, "SPICE(WRITEERROR)", msg_len, (ftnlen)17) == 0) {
	s_copy(expl, "An Attempt to write to a specified unit failed.", 
		expl_len, (ftnlen)47);
    } else if (s_cmp(msg, "SPICE(ZERORADIUS)", msg_len, (ftnlen)17) == 0) {
	s_copy(expl, "Invalid Radius--Equatorial or Polar Radius is Zero", 
		expl_len, (ftnlen)50);
    } else if (s_cmp(msg, "SPICE(ZEROVECTOR)", msg_len, (ftnlen)17) == 0) {
	s_copy(expl, "Input Vector is the Zero Vector", expl_len, (ftnlen)31);
    } else if (s_cmp(msg, "SPICE(ZEROAXISLENGTH)", msg_len, (ftnlen)21) == 0) 
	    {
	s_copy(expl, "Input Axis Length is Zero", expl_len, (ftnlen)25);
    } else {
	s_copy(expl, " ", expl_len, (ftnlen)1);
    }
    return 0;
} /* expln_ */

