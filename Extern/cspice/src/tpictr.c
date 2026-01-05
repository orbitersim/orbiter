/* tpictr.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure TPICTR ( Create a Time Format Picture ) */
/* Subroutine */ int tpictr_(char *sample, char *pictur, logical *ok, char *
	errmsg, ftnlen sample_len, ftnlen pictur_len, ftnlen errmsg_len)
{
    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_cmp(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    doublereal tvec[10];
    logical mods;
    char type__[5];
    integer ntvec;
    logical succes, yabbrv;
    char modify[8*5];
    extern /* Subroutine */ int tpartv_(char *, doublereal *, integer *, char 
	    *, char *, logical *, logical *, logical *, char *, char *, 
	    ftnlen, ftnlen, ftnlen, ftnlen, ftnlen);

/* $ Abstract */

/*     Create a time format picture suitable for use by the routine */
/*     TIMOUT from a given sample time string. */

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

/*     TIME */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     SAMPLE     I   is a sample date time string */
/*     PICTUR     O   is a format picture that describes SAMPLE */
/*     OK         O   indicates success or failure to parse SAMPLE */
/*     ERRMSG     O   a diagnostic returned if SAMPLE cannot be parsed */

/* $ Detailed_Input */

/*     SAMPLE   is a representative time string that to use */
/*              as a model to format time strings. */

/* $ Detailed_Output */

/*     PICTUR   is a format picture suitable for use with the SPICE */
/*              routine TIMOUT. This picture when used to format */
/*              the appropriate  epoch via TIMOUT will yield the same */
/*              time components in the same order as the components */
/*              in SAMPLE. */

/*              Picture should be declared to be at least 80 characters */
/*              in length. If Picture is not sufficiently large */
/*              to contain the format picture, the picture will */
/*              be truncated on the right. */

/*     OK       is a logical flag. If all of the components of SAMPLE */
/*              are recognizable, OK will be returned with the value */
/*              .TRUE. If some part of PICTUR cannot be parsed, */
/*              OK will be returned with the value .FALSE. */

/*     ERRMSG   is a diagnostic message  that indicates what part of */
/*              SAMPLE was not recognizable. If SAMPLE can be */
/*              successfully parsed, OK will be .TRUE. and ERRMSG will */
/*              be returned as a blank string. If ERRMSG does not */
/*              have sufficient room (up to 400 characters) to */
/*              contain the full message, the message will be truncated */
/*              on the right. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/*     1)  All problems with the inputs are reported via OK and ERRMSG. */

/*     2)  If a format picture can not be created from the sample */
/*         time string, PICTUR is returned as a blank string. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     Although the routine TIMOUT provides SPICE users with a great */
/*     deal of flexibility in formatting time strings, users must */
/*     master the means by which a time picture is constructed */
/*     suitable for use by TIMOUT. */

/*     This routine allows SPICE users to supply a sample time string */
/*     from which a corresponding time format picture can be created, */
/*     freeing users from the task of mastering the intricacies of */
/*     the routine TIMOUT. */

/*     Note that TIMOUT can produce many time strings whose patterns */
/*     can not be discerned by this routine. When such outputs are */
/*     called for, the user must consult TIMOUT and construct the */
/*     appropriate format picture "by hand." However, these exceptional */
/*     formats are not widely used and are not generally recognizable */
/*     to an uninitiated reader. */

/* $ Examples */

/*     The numerical results shown for this example may differ across */
/*     platforms. The results depend on the SPICE kernels used as input, */
/*     the compiler and supporting libraries, and the machine specific */
/*     arithmetic implementation. */

/*     1) Given a sample with the format of the UNIX date string */
/*        local to California, create a SPICE time picture for use */
/*        in TIMOUT. */

/*        Using that SPICE time picture, convert a series of ephemeris */
/*        times to that picture format. */

/*        Use the LSK kernel below to load the leap seconds and time */
/*        constants required for the conversions. */

/*           naif0012.tls */


/*        Example code begins here. */


/*              PROGRAM TPICTR_EX1 */
/*              IMPLICIT NONE */

/*        C */
/*        C     Local parameters. */
/*        C */
/*              INTEGER               ERRLEN */
/*              PARAMETER           ( ERRLEN  = 400 ) */

/*              INTEGER               TIMLEN */
/*              PARAMETER           ( TIMLEN  = 64  ) */

/*        C */
/*        C     Local variables */
/*        C */
/*              CHARACTER*(ERRLEN)    ERR */
/*              CHARACTER*(TIMLEN)    PICTUR */
/*              CHARACTER*(TIMLEN)    SAMPLE */
/*              CHARACTER*(TIMLEN)    TIMSTR */
/*              CHARACTER*(TIMLEN)    UTCSTR */

/*              DOUBLE PRECISION      ET */

/*              LOGICAL               OK */

/*        C */
/*        C     Load LSK file. */
/*        C */
/*              CALL FURNSH ( 'naif0012.tls' ) */

/*        C */
/*        C     Create the required time picture. */
/*        C */
/*              SAMPLE = 'Thu Oct 01 11:11:11 PDT 1111' */

/*              CALL TPICTR ( SAMPLE, PICTUR, OK, ERR ) */

/*              IF ( .NOT. OK ) THEN */

/*                 WRITE(*,*) 'Invalid time picture.' */
/*                 WRITE(*,*) ERR */

/*              ELSE */

/*        C */
/*        C        Convert the input UTC time to ephemeris time. */
/*        C */
/*                 UTCSTR = '24 Mar 2018  16:23:00 UTC' */
/*                 CALL STR2ET ( UTCSTR, ET ) */

/*        C */
/*        C         Now convert ET to the desired output format. */
/*        C */
/*                  CALL TIMOUT ( ET, PICTUR, TIMSTR ) */
/*                  WRITE (*,*) 'Sample format: ', SAMPLE */
/*                  WRITE (*,*) 'Time picture : ', PICTUR */
/*                  WRITE (*,*) */
/*                  WRITE (*,*) 'Input UTC    : ', UTCSTR */
/*                  WRITE (*,*) 'Output       : ', TIMSTR */

/*              END IF */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*         Sample format: Thu Oct 01 11:11:11 PDT 1111 */
/*         Time picture : Wkd Mon DD HR:MN:SC PDT YYYY ::UTC-7 */

/*         Input UTC    : 24 Mar 2018  16:23:00 UTC */
/*         Output       : Sat Mar 24 09:23:00 PDT 2018 */


/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 25-AUG-2021 (JDR) */

/*        Changed output argument name ERROR to ERRMSG for consistency */
/*        with other routines. */

/*        Edited the header to comply with NAIF standard. */
/*        Converted the existing code fragments into complete example */
/*        and added reference to required LSK. */

/* -    SPICELIB Version 1.0.1, 16-MAR-1999 (WLT) */

/*        Corrected a minor spelling error in the header comments. */

/* -    SPICELIB Version 1.0.0, 10-AUG-1996 (WLT) */

/* -& */
/* $ Index_Entries */

/*     Use a sample time string to produce a time format picture */

/* -& */

/*     Local variables */


/*     This routine is really just a front for one aspect of */
/*     the routine TPARTV. */

    s_copy(errmsg, " ", errmsg_len, (ftnlen)1);
    tpartv_(sample, tvec, &ntvec, type__, modify, &mods, &yabbrv, &succes, 
	    pictur, errmsg, sample_len, (ftnlen)5, (ftnlen)8, pictur_len, 
	    errmsg_len);
    if (s_cmp(pictur, " ", pictur_len, (ftnlen)1) == 0) {
	*ok = FALSE_;
    } else {
	*ok = TRUE_;
	s_copy(errmsg, " ", errmsg_len, (ftnlen)1);
    }
    return 0;
} /* tpictr_ */

