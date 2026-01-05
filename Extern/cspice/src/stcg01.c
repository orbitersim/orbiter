/* stcg01.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;
static integer c__2 = 2;
static integer c__3 = 3;
static integer c__4 = 4;
static integer c__5 = 5;
static integer c__6 = 6;
static integer c__7 = 7;

/* $Procedure STCG01 ( STAR catalog type 1, get star data ) */
/* Subroutine */ int stcg01_(integer *index, doublereal *ra, doublereal *dec, 
	doublereal *rasig, doublereal *decsig, integer *catnum, char *sptype, 
	doublereal *vmag, ftnlen sptype_len)
{
    extern /* Subroutine */ int ekgc_(integer *, integer *, integer *, char *,
	     logical *, logical *, ftnlen), ekgd_(integer *, integer *, 
	    integer *, doublereal *, logical *, logical *), ekgi_(integer *, 
	    integer *, integer *, integer *, logical *, logical *);
    logical null;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    logical found;
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), setmsg_(char *, ftnlen), errint_(char *, integer *, 
	    ftnlen);
    extern logical return_(void);
    extern doublereal rpd_(void);

/* $ Abstract */

/*     Get data for a single star from a SPICE type 1 star catalog. */

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

/*     None. */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     INDEX      I   Star index. */
/*     RA         O   Right ascension in radians. */
/*     DEC        O   Declination in radians. */
/*     RASIG      O   Right ascension uncertainty in radians. */
/*     DECSIG     O   Declination uncertainty in radians. */
/*     CATNUM     O   Catalog number. */
/*     SPTYPE     O   Spectral type. */
/*     VMAG       O   Visual magnitude. */

/* $ Detailed_Input */

/*     INDEX    is the index of the star in the list of stars */
/*              that satisfy the selection criteria specified in */
/*              the last call to STCF01. */

/* $ Detailed_Output */

/*     RA       is right ascension of the star at the catalog epoch */
/*              in radians relative to the J2000 inertial frame. */

/*     DEC      is declination of the star at the catalog epoch in */
/*              radians relative to the J2000 inertial frame. */

/*     RASIG    is the uncertainty in right ascension of the star at */
/*              the catalog epoch in radians. */

/*     DECSIG   is the uncertainty in declination of the star at */
/*              the catalog epoch in radians. */

/*     CATNUM   is the star number in the catalog. */

/*     SPTYPE   is the star's spectral type. See catalog description */
/*              for more information regarding encoding of spectral */
/*              type values. */

/*     VMAG     is the visual magnitude of the star. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If fetching of any of output values fails, the error */
/*         SPICE(BADSTARINDEX) is signaled. */

/*     2)  If no star catalog has been loaded, an error is signaled by a */
/*         routine in the call tree of this routine. */

/*     3)  If a star catalog type 1 was not queried by calling STCF01 */
/*         before calling this routine, an error is signaled by a routine */
/*         in the call tree of this routine. */

/* $ Files */

/*     This routine reads the data from SPICE type 1 star catalog file */
/*     loaded into the program by a call to STCL01. */

/*     SPICE type 1 star catalog files MUST contain a single data table. */
/*     It can occupy a single segment or it can spread across multiple */
/*     segments. This table MUST include the following columns: */

/*        column name                data type          units */
/*        ---------------------------------------------------- */
/*        RA                   DOUBLE PRECISION        DEGREES */
/*        DEC                  DOUBLE PRECISION        DEGREES */
/*        RA_SIGMA             DOUBLE PRECISION        DEGREES */
/*        DEC_SIGMA            DOUBLE PRECISION        DEGREES */
/*        CATALOG_NUMBER       INTEGER */
/*        SPECTRAL_TYPE        CHARACTER*(4) */
/*        VISUAL_MAGNITUDE     DOUBLE PRECISION */

/*     Nulls are not allowed in any of the columns. */
/*     Other columns can also be present in the table but their data */
/*     will NOT be accessible through STCF01 and STCG01 -- */
/*     the interface used to access data in the catalog. Note */
/*     that the names and attributes of these additional columns */
/*     must be identical for all segments containing this table. */

/* $ Particulars */

/*     This routine is intended to be a part of the user interface to */
/*     the SPICE type 1 star catalog. It allows the caller to retrieve */
/*     data for a single star found by STCF01 using the star's */
/*     index within the search result array. This subroutine MUST */
/*     NOT be called before a search by STCF01 was done. */

/*     Other routines in the SPICE type 1 star catalog access */
/*     family are: */

/*        STCL01  load the catalog file and make its data */
/*                available for search and retrieval. */

/*        STCF01  search through the catalog for all stars within */
/*                a specified RA-DEC rectangle. */

/* $ Examples */

/*     In the following code fragment, STCG01 is used to retrieve */
/*     position and characteristics for every star within an RA - DEC */
/*     rectangle from a particular SPICE type 1 star catalog. */

/*     C */
/*     C     Load catalog file. */
/*     C */
/*           CALL STCL01 ( CATFN, TABNAM, HANDLE ) */
/*     C */
/*     C     Search through the loaded catalog. */
/*     C */
/*           CALL STCF01 ( TABNAM, RAMIN,  RAMAX, */
/*          .              DECMIN, DECMAX, NSTARS ) */
/*     C */
/*     C     Retrieve data for every star found. */
/*     C */
/*           DO I = 1, NSTARS */

/*              CALL STCG01 ( I, RA, DEC, RASIG, DECSIG, */
/*          .                 CATNUM, SPTYPE, VMAG ) */

/*           END DO */

/* $ Restrictions */

/*     1)  The catalog file STCG01 reads data from MUST be loaded */
/*         by STCL01 and a search through the catalog MUST be done by */
/*         STCF01 before STCG01 is called. */

/*     2)  No other EK queries can be made between the call to STCF01 */
/*         and the call to STCG01. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     B.V. Semenov       (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 16-JUN-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Updated header to comply with NAIF standard. Corrected argument */
/*        names "RASIG" and "DECSIG" in $Brief_I/O. */

/*        Updated entry #3 in $Exceptions section. */

/* -    SPICELIB Version 1.0.0, 15-MAY-1996 (BVS) */

/* -& */
/* $ Index_Entries */

/*     get data for single star from a type 1 star catalog */

/* -& */


/*     SPICELIB functions */


/*     Local variables. */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("STCG01", (ftnlen)6);
    }

/*     Fetch data from the catalog in the following order */
/*     as defined QUERY string template in STCF01 routine */

/*           RA, DEC, RASIG, DECSIG, CATNUM, SPTYPE, VMAG */

/*     Check FOUNDs and report error if any of the parameters */
/*     is not found. */

/*     Since NULLs are not allowed in any of the star catalog */
/*     columns, no check for NULLs is performed. */

    ekgd_(&c__1, index, &c__1, ra, &null, &found);
    if (! found) {
	setmsg_("RA value for star # not found. ", (ftnlen)31);
	errint_("#", index, (ftnlen)1);
	sigerr_("SPICE(BADSTARINDEX)", (ftnlen)19);
	chkout_("STCG01", (ftnlen)6);
	return 0;
    }
    ekgd_(&c__2, index, &c__1, dec, &null, &found);
    if (! found) {
	setmsg_("DEC value for star # not found. ", (ftnlen)32);
	errint_("#", index, (ftnlen)1);
	sigerr_("SPICE(BADSTARINDEX)", (ftnlen)19);
	chkout_("STCG01", (ftnlen)6);
	return 0;
    }
    ekgd_(&c__3, index, &c__1, rasig, &null, &found);
    if (! found) {
	setmsg_("RASIG value for star # not found. ", (ftnlen)34);
	errint_("#", index, (ftnlen)1);
	sigerr_("SPICE(BADSTARINDEX)", (ftnlen)19);
	chkout_("STCG01", (ftnlen)6);
	return 0;
    }
    ekgd_(&c__4, index, &c__1, decsig, &null, &found);
    if (! found) {
	setmsg_("DECSIG value for star # not found.", (ftnlen)34);
	errint_("#", index, (ftnlen)1);
	sigerr_("SPICE(BADSTARINDEX)", (ftnlen)19);
	chkout_("STCG01", (ftnlen)6);
	return 0;
    }
    ekgi_(&c__5, index, &c__1, catnum, &null, &found);
    if (! found) {
	setmsg_("CATNUM value for star # not found.", (ftnlen)34);
	errint_("#", index, (ftnlen)1);
	sigerr_("SPICE(BADSTARINDEX)", (ftnlen)19);
	chkout_("STCG01", (ftnlen)6);
	return 0;
    }
    ekgc_(&c__6, index, &c__1, sptype, &null, &found, sptype_len);
    if (! found) {
	setmsg_("SPTYPE value for star # not found.", (ftnlen)34);
	errint_("#", index, (ftnlen)1);
	sigerr_("SPICE(BADSTARINDEX)", (ftnlen)19);
	chkout_("STCG01", (ftnlen)6);
	return 0;
    }
    ekgd_(&c__7, index, &c__1, vmag, &null, &found);
    if (! found) {
	setmsg_("VMAG value for star # not found. ", (ftnlen)33);
	errint_("#", index, (ftnlen)1);
	sigerr_("SPICE(BADSTARINDEX)", (ftnlen)19);
	chkout_("STCG01", (ftnlen)6);
	return 0;
    }

/*     Convert angles to radians before return. */

    *ra *= rpd_();
    *dec *= rpd_();
    *rasig *= rpd_();
    *decsig *= rpd_();
    chkout_("STCG01", (ftnlen)6);
    return 0;
} /* stcg01_ */

