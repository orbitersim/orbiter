/* timdef.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__8 = 8;
static integer c__0 = 0;
static integer c__1 = 1;

/* $Procedure TIMDEF ( Time Software Defaults ) */
/* Subroutine */ int timdef_(char *action, char *item, char *value, ftnlen 
	action_len, ftnlen item_len, ftnlen value_len)
{
    /* Initialized data */

    static char defsys[16] = "UTC             ";
    static char defzon[16] = "                ";
    static char defcal[16] = "GREGORIAN       ";
    static char zones[16*8] = "EST             " "EDT             " "CST    "
	    "         " "CDT             " "MST             " "MDT           "
	    "  " "PST             " "PDT             ";
    static char trnslt[16*8] = "UTC-5           " "UTC-4           " "UTC-6 "
	    "          " "UTC-5           " "UTC-7           " "UTC-6        "
	    "   " "UTC-8           " "UTC-7           ";

    /* System generated locals */
    integer i__1;

    /* Builtin functions */
    integer s_cmp(char *, char *, ftnlen, ftnlen);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_rnge(char *, integer, char *, integer);

    /* Local variables */
    static doublereal hoff, moff;
    static integer last, zone;
    extern /* Subroutine */ int chkin_(char *, ftnlen), ucase_(char *, char *,
	     ftnlen, ftnlen), errch_(char *, char *, ftnlen, ftnlen);
    static char myval[16];
    extern /* Subroutine */ int ljust_(char *, char *, ftnlen, ftnlen);
    extern integer isrchc_(char *, integer *, char *, ftnlen, ftnlen);
    static logical succes;
    static char myactn[16];
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), prefix_(char *, integer *, char *, ftnlen, ftnlen), 
	    setmsg_(char *, ftnlen);
    static char myitem[16];
    extern logical return_(void);
    extern /* Subroutine */ int zzutcpm_(char *, integer *, doublereal *, 
	    doublereal *, integer *, logical *, ftnlen);

/* $ Abstract */

/*     Set and retrieve the defaults associated with calendar */
/*     input strings. */

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
/*     ACTION     I   Kind of action to take 'SET' or 'GET'. */
/*     ITEM       I   Default item of interest. */
/*     VALUE     I-O  Value associated with the default item. */

/* $ Detailed_Input */

/*     ACTION   is a word that specifies whether TIMDEF sets the */
/*              value associated with ITEM or retrieves the value */
/*              associated with ITEM. The allowed values for */
/*              ACTION are 'SET' and 'GET'. The routine is not */
/*              sensitive to the case of the letters in ACTION. */

/*     ITEM     is the default items whose value should be set or */
/*              retrieved. The items that may be requested are: */

/*                 ITEM        Allowed Values */
/*                 ---------   -------------- */
/*                 CALENDAR    GREGORIAN */
/*                             JULIAN */
/*                             MIXED */

/*                 SYSTEM      TDB */
/*                             TDT */
/*                             TT */
/*                             UTC */

/*                 ZONE        EST, EDT, CST, CDT, MST, MDT, PST, PDT */
/*                             UTC+HR */
/*                             UTC-HR       ( 0 <= HR < 13 ) */
/*                             UTC+HR:MN    ( 0 <= MN < 60 ) */
/*                             UTC-HR:MN */

/*              The case of ITEM is not significant. */

/*     VALUE    if the action is 'SET' then VALUE is an input and */
/*              is the value to be associated with ITEM. Note that */
/*              VALUE is checked to ensure it is within the range */
/*              of allowed values for ITEM. If it is not within */
/*              the expected range and appropriate error message */
/*              is signaled. The case of VALUE is not significant. */

/* $ Detailed_Output */

/*     VALUE    if the action is 'GET' then VALUE will be the */
/*              value associated with the requested ITEM. Note that */
/*              when time zones are set, they are translated to the */
/*              UTC offset form ( UTC(+/-)HR[:MN] ). When VALUE is */
/*              an output it will be in upper case. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the ACTION specified is not 'SET' or 'GET', the error */
/*         SPICE(BADACTION) is signaled. */

/*     2)  If the ITEM specified is not one the recognized items, */
/*         the error SPICE(BADTIMEITEM) is signaled. */

/*     3)  If the value associated with a 'SET' item input */
/*         is not one of the recognized items, the error */
/*         SPICE(BADDEFAULTVALUE) is signaled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine exists to allow SPICE toolkit users to alter */
/*     the default interpretation of time strings made by the */
/*     routine STR2ET. */

/*     Normally, unlabelled time strings are assumed to belong to */
/*     the Gregorian Calendar and are UTC times. However, you */
/*     may alter the default behavior by calling TIMDEF. */

/*     Calendar */
/*     -------- */

/*     You may set the calendar to be one of the following */

/*     Gregorian   --- This is the calendar used daily the */
/*                     Western Hemisphere. Leap years occur in this */
/*                     calendar every 4 years except on centuries */
/*                     such as 1900 that are not divisible by 400. */

/*     Julian      --- This is the calendar that was in use prior */
/*                     to October 15, 1582. Leap years occur every */
/*                     4 years on the Julian Calendar (including all */
/*                     centuries.) October 5, 1582 on the Julian */
/*                     calendar corresponds to October 15, 1582 of the */
/*                     Gregorian Calendar. */

/*     Mixed       --- This calendar uses the Julian calendar */
/*                     for days prior to October 15, 1582 and */
/*                     the Gregorian calendar for days on or after */
/*                     October 15, 1582. */

/*     To set the default calendar, select on of the above for VALUE */
/*     and make the following call. */

/*        CALL TIMDEF ( 'SET', 'CALENDAR', VALUE ) */


/*     System */
/*     ------- */

/*     You may set the system used for keeping time to be UTC (default) */
/*     TDB (barycentric Dynamical Time), TDT (Terrestrial Dynamical */
/*     Time), or TT (Terrestrial Time). TDT and TT represent the same */
/*     time system. Both TDB and TT (TDT) have no leapseconds. As such */
/*     the time elapsed between any two epochs on these calendars does */
/*     not depend upon when leapseconds occur. */

/*     To set the default time system, select TDT, TT, TDB or UTC for */
/*     VALUE and make the following call. */

/*        CALL TIMDEF ( 'SET', 'SYSTEM', VALUE ) */

/*     Note that such a call has the side effect of setting the value */
/*     associated with ZONE to a blank. */

/*     Zone */
/*     ----- */

/*     You may alter the UTC system by specifying a time zone (UTC */
/*     offset). For example you may specify that epochs are referred */
/*     to Pacific Standard Time (PST --- UTC-7). The standard */
/*     abbreviations for U.S. time zones are recognized: */

/*        EST   UTC-5 */
/*        EDT   UTC-4 */
/*        CST   UTC-6 */
/*        CDT   UTC-5 */
/*        MST   UTC-7 */
/*        MDT   UTC-6 */
/*        PST   UTC-8 */
/*        PDT   UTC-7 */

/*     In addition you may specify any commercial time zone by using */
/*     "offset" notation. This notation starts with the letters 'UTC' */
/*     followed by a + for time zones east of Greenwich and - for time */
/*     zones west of Greenwich. This is followed by the number of hours */
/*     to add or subtract from UTC. This is optionally followed by a */
/*     colon ':' and the number of minutes to add or subtract (based on */
/*     the sign that follows 'UTC') to get the local time zone. Thus to */
/*     specify the time zone of Calcutta you would specify the time zone */
/*     to be UTC+5:30. To specify the time zone of Newfoundland use the */
/*     time zone UTC-3:30. */

/*     To set a default time zone, select one of the "built-in" U.S. */
/*     zones or construct an offset as discussed above. Then make the */
/*     call */

/*        CALL TIMDEF ( 'SET', 'ZONE', VALUE ) */

/*     If you 'GET' a 'ZONE' it will either be blank, or have the */
/*     form 'UTC+/-HR[:MN]' */

/*     Note that such a call has the side effect of setting the value */
/*     associated with SYSTEM to a blank. */

/* $ Examples */

/*     Suppose you wish to modify the behavior of STR2ET so that */
/*     it interprets unlabeled time strings as being times in */
/*     Pacific Daylight Time and that you want the calendar to use */
/*     to be the "Mixed" calendar. The following two calls will */
/*     make the desired changes to the behavior of STR2ET */

/*         CALL TIMDEF ( 'SET', 'CALENDAR', 'MIXED' ) */
/*         CALL TIMDEF ( 'SET', 'ZONE',     'PDT'   ) */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     W.L. Taber         (JPL) */
/*     E.D. Wright        (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.3.0, 14-OCT-2021 (EDW) (JDR) */

/*        UCASE and LJUST called on VALUE only in 'SET' block. */

/*        Add time system name 'TT' (Terrestrial Time) as alternate */
/*        assignment of 'TDT' (Terrestrial Dynamical Time). */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 1.2.0, 26-MAY-1998 (WLT) */

/*        The previous version did not check out and return */
/*        when an error was detected in the if block that */
/*        begins with */

/*           ELSE IF ( MYITEM .EQ. 'ZONE' ) THEN */

/*        The routine did eventually check out and return so */
/*        that the trace stack was maintained correctly, but */
/*        the default time zone would be modified which was not */
/*        the desired behavior. */

/* -    SPICELIB Version 1.1.0, 27-JUN-1997 (WLT) */

/*        The previous version failed to check out when */
/*        the default value was set. */

/* -    SPICELIB Version 1.0.0, 13-NOV-1996 (WLT) */

/* -& */
/* $ Index_Entries */

/*     Change time software defaults. */
/*     Time Zones */
/*     Gregorian and Julian Calendars */

/* -& */


/*     SPICELIB Functions */


/*     Local Variables. */

    if (return_()) {
	return 0;
    }
    chkin_("TIMDEF", (ftnlen)6);

/*     Normalize the input. */

    ljust_(action, myactn, action_len, (ftnlen)16);
    ucase_(myactn, myactn, (ftnlen)16, (ftnlen)16);
    ljust_(item, myitem, item_len, (ftnlen)16);
    ucase_(myitem, myitem, (ftnlen)16, (ftnlen)16);

/*     Admittedly, the decision making below is not very elegant. */
/*     However, this works and is simpler than anything that comes */
/*     to mind at the moment and allows us to give quite specific */
/*     diagnostic messages easily. */

    if (s_cmp(myactn, "SET", (ftnlen)16, (ftnlen)3) == 0) {
	ljust_(value, myval, value_len, (ftnlen)16);
	ucase_(myval, myval, (ftnlen)16, (ftnlen)16);
	if (s_cmp(myitem, "SYSTEM", (ftnlen)16, (ftnlen)6) == 0) {

/*           Recognize only the know time systems. Note, 'UTC' not */
/*           actually a time system, more a time representation. */

	    if (s_cmp(myval, "TDB", (ftnlen)16, (ftnlen)3) == 0 || s_cmp(
		    myval, "TDT", (ftnlen)16, (ftnlen)3) == 0 || s_cmp(myval, 
		    "TT", (ftnlen)16, (ftnlen)2) == 0 || s_cmp(myval, "UTC", (
		    ftnlen)16, (ftnlen)3) == 0) {
		s_copy(defzon, " ", (ftnlen)16, (ftnlen)1);
		s_copy(defsys, myval, (ftnlen)16, (ftnlen)16);
	    } else {
		setmsg_("The default value assigned to the time system must "
			"be one of 'UTC', 'TDT', 'TT', or 'TDB'. The value su"
			"pplied was '#'. ", (ftnlen)119);
		errch_("#", value, (ftnlen)1, value_len);
		sigerr_("SPICE(BADDEFAULTVALUE)", (ftnlen)22);
		chkout_("TIMDEF", (ftnlen)6);
		return 0;
	    }
	} else if (s_cmp(myitem, "ZONE", (ftnlen)16, (ftnlen)4) == 0) {
	    zone = isrchc_(myval, &c__8, zones, (ftnlen)16, (ftnlen)16);

/*           If MYVAL was one of the recognized time zones, we */
/*           translate it to the UTC offset form. */

	    if (zone > 0) {
		s_copy(myval, trnslt + (((i__1 = zone - 1) < 8 && 0 <= i__1 ? 
			i__1 : s_rnge("trnslt", i__1, "timdef_", (ftnlen)404))
			 << 4), (ftnlen)16, (ftnlen)16);
	    }
	    prefix_("::", &c__0, myval, (ftnlen)2, (ftnlen)16);
	    zzutcpm_(myval, &c__1, &hoff, &moff, &last, &succes, (ftnlen)16);
	    if (! succes) {
		setmsg_("The input value for a time zone \"#\" was not recog"
			"nized as known time zone and could not be parsed acc"
			"ording to the pattern UTC(+/-)HR[:MN]. Known time zo"
			"nes are: 'EST', 'EDT', 'CST', 'CDT', 'MST', 'MDT', '"
			"PST', and 'PDT'. ", (ftnlen)222);
		errch_("#", value, (ftnlen)1, value_len);
		sigerr_("SPICE(BADDEFAULTVALUE)", (ftnlen)22);
		chkout_("TIMDEF", (ftnlen)6);
		return 0;
	    }
	    s_copy(defzon, myval + 2, (ftnlen)16, (ftnlen)14);
	    s_copy(defsys, " ", (ftnlen)16, (ftnlen)1);
	} else if (s_cmp(myitem, "CALENDAR", (ftnlen)16, (ftnlen)8) == 0) {
	    if (s_cmp(myval, "JULIAN", (ftnlen)16, (ftnlen)6) == 0 || s_cmp(
		    myval, "GREGORIAN", (ftnlen)16, (ftnlen)9) == 0 || s_cmp(
		    myval, "MIXED", (ftnlen)16, (ftnlen)5) == 0) {
		s_copy(defcal, myval, (ftnlen)16, (ftnlen)16);
	    } else {
		setmsg_("The input value for '#' is not a recognized calenda"
			"r type.  The recognized calendars are 'GREGORIAN', '"
			"JULIAN', and 'MIXED'. ", (ftnlen)125);
		errch_("#", value, (ftnlen)1, value_len);
		sigerr_("SPICE(BADDEFAULTVALUE)", (ftnlen)22);
		chkout_("TIMDEF", (ftnlen)6);
		return 0;
	    }
	} else {
	    setmsg_("The specified item '#' is not a recognized time default"
		    " item.  The items that you may \"SET\" via the routine T"
		    "IMDEF are 'CALENDAR', 'SYSTEM', or 'ZONE' ", (ftnlen)151);
	    errch_("#", item, (ftnlen)1, item_len);
	    sigerr_("SPICE(BADTIMEITEM)", (ftnlen)18);
	    chkout_("TIMDEF", (ftnlen)6);
	    return 0;
	}
	chkout_("TIMDEF", (ftnlen)6);
	return 0;
    } else if (s_cmp(myactn, "GET", (ftnlen)16, (ftnlen)3) == 0) {
	if (s_cmp(myitem, "CALENDAR", (ftnlen)16, (ftnlen)8) == 0) {
	    s_copy(value, defcal, value_len, (ftnlen)16);
	} else if (s_cmp(myitem, "SYSTEM", (ftnlen)16, (ftnlen)6) == 0) {
	    s_copy(value, defsys, value_len, (ftnlen)16);
	} else if (s_cmp(myitem, "ZONE", (ftnlen)16, (ftnlen)4) == 0) {
	    s_copy(value, defzon, value_len, (ftnlen)16);
	} else {
	    setmsg_("The specified item '#' is not a recognized time default"
		    " item.  The items that you may \"SET\" via the routine T"
		    "IMDEF are 'CALENDAR', 'SYSTEM', or 'ZONE' ", (ftnlen)151);
	    errch_("#", item, (ftnlen)1, item_len);
	    sigerr_("SPICE(BADTIMEITEM)", (ftnlen)18);
	    chkout_("TIMDEF", (ftnlen)6);
	    return 0;
	}
    } else {
	setmsg_("The action specified to TIMDEF was '#'.  This is not a reco"
		"gnized action. The recognized actions are 'SET' and 'GET'. ", 
		(ftnlen)118);
	errch_("#", action, (ftnlen)1, action_len);
	sigerr_("SPICE(BADACTION)", (ftnlen)16);
	chkout_("TIMDEF", (ftnlen)6);
	return 0;
    }
    chkout_("TIMDEF", (ftnlen)6);
    return 0;
} /* timdef_ */

