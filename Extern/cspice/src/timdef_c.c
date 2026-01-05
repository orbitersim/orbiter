/*

-Procedure timdef_c ( Time Software Defaults )

-Abstract

   Set and retrieve the defaults associated with calendar
   input strings.

-Disclaimer

   THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
   CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
   GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
   ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
   PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
   TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
   WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
   PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
   SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
   SOFTWARE AND RELATED MATERIALS, HOWEVER USED.

   IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
   BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
   LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
   INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
   REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
   REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.

   RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
   THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
   CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
   ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.

-Required_Reading

   None.

-Keywords

   TIME

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZst.h"
   #include "SpiceZmc.h"

   void timdef_c ( ConstSpiceChar * action,
                   ConstSpiceChar * item,
                   SpiceInt         vallen,
                   SpiceChar      * value )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   action     I   is the kind of action to take "SET" or "GET".
   item       I   is the default item of interest.
   vallen     I   Length of list for output.
   value     I-O  is the value associated with the default item.

-Detailed_Input

   action      is a word that specifies whether timdef_c sets the
               value associated with `item' or retrieves the value
               associated with `item'. The allowed values for
               `action' are "SET" and "GET". The routine is not
               sensitive to the case of the letters in `action'.

   item        is the default items whose value should be set or
               retrieved. The items that may be requested are:

                  item        Allowed Values
                  ---------   --------------
                  CALENDAR    GREGORIAN
                              JULIAN
                              MIXED

                  SYSTEM      TDB
                              TDT
                              TT
                              UTC

                  ZONE        EST, EDT, CST, CDT, MST, MDT, PST, PDT
                              UTC+hr
                              UTC-HR       ( 0 <= hr < 13 )
                              UTC+hr:MN    ( 0 <= mn < 60 )
                              UTC-HR:MN

               The case of `item' is not significant.

   value       if the action is "SET" then `value' is an input and
               is the value to be associated with `item'. Note that
               `value' is checked to ensure it is within the range
               of allowed values for `item'. If it is not within
               the expected range and appropriate error message
               is signaled. The case of `value' is not significant.

-Detailed_Output

   value       if the action is "GET" then `value' will be the
               value associated with the requested `item'. Note that
               when time zones are set, they are translated to the
               UTC offset form ( UTC(+/-)HR[:MN] ). When `value' is
               an output it will be in upper case.

-Parameters

   None.

-Exceptions

   1)  If the `action' specified is not "SET" or "GET", the error
       SPICE(BADACTION) is signaled.

   2)  If the `item' specified is not one the recognized items, the error
       SPICE(BADTIMEITEM) is signaled by a routine in the call tree of this
       routine.

   3)  If the value associated with a "SET" item input is not one of the
       recognized items, the error SPICE(BADDEFAULTVALUE) is signaled by a
       routine in the call tree of this routine.

   4)  If any of the `action' or `item' input string pointers are null, the
       error SPICE(NULLPOINTER) is signaled.

   5)  If any of the `action' or `item' input strings have zero length, the
       error SPICE(EMPTYSTRING) is signaled, since the input string cannot be
       converted to a Fortran-style string in this case.

   6)  If the `value' string pointer is null, the error SPICE(NULLPOINTER)
       is signaled.

   7)  If `action' is "SET" and the `value' input string has zero length, the
       error SPICE(EMPTYSTRING) is signaled, since the input string cannot be
       converted to a Fortran-style string in this case.

   8)  If `action' is "GET" and the `value' output string has length less than
       two characters, the error SPICE(STRINGTOOSHORT) is signaled, since the
       output string is too short to contain one character of output data plus
       a null terminator, and therefore it cannot be passed to the underlying
       Fortran routine.

-Files

   None.

-Particulars

   This routine exists to allow SPICE toolkit users to alter
   the default interpretation of time strings made by the
   routine str2et_c.

   Normally, unlabeled time strings are assumed to belong to
   the Gregorian Calendar and are UTC times. However, you
   may alter the default behavior by calling timdef_c.

   Calendar
   --------

   You may set the calendar to be one of the following

   Gregorian   --- This is the calendar used daily the
                   Western Hemisphere. Leap years occur in this
                   calendar every 4 years except on centuries
                   such as 1900 that are not divisible by 400.

   Julian      --- This is the calendar that was in use prior
                   to October 15, 1582. Leap years occur every
                   4 years on the Julian Calendar (including all
                   centuries.) October 5, 1582 on the Julian
                   calendar corresponds to October 15, 1582 of the
                   Gregorian Calendar.

   Mixed       --- This calendar uses the Julian calendar
                   for days prior to October 15, 1582 and
                   the Gregorian calendar for days on or after
                   October 15, 1582.

   To set the default calendar, select on of the above for `value'
   and make the following call.

      timdef_c ( "SET", "CALENDAR", lenval, value );


   System
   ------

   You may set the system used for keeping time to be UTC (default)
   TDB (barycentric Dynamical Time), TDT (Terrestrial Dynamical
   Time), or TT (Terrestrial Time). TDT and TT represent the same
   time system. Both TDB and TT (TDT) have no leapseconds. As such
   the time elapsed between any two epochs on these calendars does
   not depend upon when leapseconds occur.

   To set the default time system, select TDT, TT, TDB or UTC for
   `value' and make the following call.

      timdef_c ( "SET", "SYSTEM", lenval, value );

   Note that such a call has the side effect of setting the value
   associated with ZONE to a blank.

   Zone
   ----

   You may alter the UTC system by specifying a time zone (UTC
   offset). For example you may specify that epochs are referred
   to Pacific Standard Time (PST --- UTC-7). The standard
   abbreviations for U.S. time zones are recognized:

      EST   UTC-5
      EDT   UTC-4
      CST   UTC-6
      CDT   UTC-5
      MST   UTC-7
      MDT   UTC-6
      PST   UTC-8
      PDT   UTC-7

   In addition you may specify any commercial time zone by using
   "offset" notation. This notation starts with the letters "UTC"
   followed by a + for time zones east of Greenwich and - for time
   zones west of Greenwich. This is followed by the number of hours
   to add or subtract from UTC. This is optionally followed by a
   colon ":" and the number of minutes to add or subtract (based on
   the sign that follows "UTC") to get the local time zone. Thus to
   specify the time zone of Calcutta you would specify the time zone
   to be UTC+5:30. To specify the time zone of Newfoundland use the
   time zone UTC-3:30.

   To set a default time zone, select one of the "built-in" U.S.
   zones or construct an offset as discussed above. Then make the
   call

      timdef_c ( "SET", "ZONE", lenval, value );

   If you "GET" a "ZONE" it will either be blank, or have the
   form "UTC+/-HR[:MN]"

   Note that such a call has the side effect of setting the value
   associated with SYSTEM to a blank.

-Examples

   The numerical results shown for these examples may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Retrieve the current settings for calendar, system and zone
      parameters of the time subsystem calendar string processing.

      Example code begins here.


      /.
         Program timdef_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Local constants.
         ./
         #define        MAXLEN        40

         ConstSpiceChar        * item   [ 3 ] = { "CALENDAR",
                                                  "SYSTEM",
                                                  "ZONE"    };

         /.
         Local variables.
         ./
         SpiceChar               value  [ MAXLEN ];
         SpiceInt                i;

         for ( i = 0;  i < 3;  i++ )
         {

            /.
            Get the value for the current `item'
            ./
            timdef_c( "GET", item[i], MAXLEN, value );
            printf ( "%s -> %s\n", item[i], value    );

         }

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      CALENDAR -> GREGORIAN
      SYSTEM -> UTC
      ZONE ->


   2) Suppose you wish to modify the behavior of str2et_c so that
      it interprets unlabeled time strings as being times in
      Pacific Daylight Time and that you want the calendar to use
      to be the "Mixed" calendar.


      Example code begins here.


      /.
         Program timdef_ex2
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Local constants.
         ./
         #define        MAXLEN        40

         ConstSpiceChar        * item   [ 3 ] = { "CALENDAR",
                                                  "SYSTEM",
                                                  "ZONE"    };

         /.
         Local variables.
         ./
         SpiceChar               value  [ MAXLEN ];
         SpiceInt                i;

         /.
         Retrieve the initial values.
         ./
         printf ( "Initial values:\n" );
         for ( i = 0;  i < 3;  i++ )
         {

            /.
            Get the value for the current `item'
            ./
            timdef_c( "GET", item[i], MAXLEN, value );
            printf ( "   %s -> %s\n", item[i], value    );

         }

         /.
         Apply the required changes.
         ./
         timdef_c ( "SET", "CALENDAR", MAXLEN, "MIXED" );
         timdef_c ( "SET", "ZONE"    , MAXLEN, "PDT"   );

         /.
         Retrieve the final values.
         ./
         printf ( "Final values:\n" );
         for ( i = 0;  i < 3;  i++ )
         {

            /.
            Get the value for the current `item'
            ./
            timdef_c( "GET", item[i], MAXLEN, value );
            printf ( "   %s -> %s\n", item[i], value    );

         }

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Initial values:
         CALENDAR -> GREGORIAN
         SYSTEM -> UTC
         ZONE ->
      Final values:
         CALENDAR -> MIXED
         SYSTEM ->
         ZONE -> UTC-7


-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   E.D. Wright         (JPL)

-Version

   -CSPICE Version 1.1.0, 05-JUL-2021 (EDW) (JDR)

       Changed the input argument name "lenout" to "vallen" for consistency
       with other routines.

       Fixed bug: Added missing "else" block to signal the corresponding error
       when the input action is not "GET" or "SET".

       Added time system name "TT" (Terrestrial Time) as alternate
       assignment of "TDT" (Terrestrial Dynamical Time).

       Edited the header to comply with NAIF standard. Added complete code
       examples. Added entries #4 to #8 in -Exceptions section.

   -CSPICE Version 1.0.1, 13-APR-2000 (NJB)

       Made some minor updates and corrections in the header comments.

   -CSPICE Version 1.0.0, 04-FEB-1998 (EDW)

-Index_Entries

   Change time software defaults.
   Time Zones
   Gregorian and Julian Calendars

-&
*/

{ /* Begin timdef_c */


   /*
   Participate in error tracing.
   */
   chkin_c ( "timdef_c" );


   /*
   Check the input strings to make sure the pointers are non-null
   and the string length is non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "timdef_c", action );
   CHKFSTR ( CHK_STANDARD, "timdef_c", item   );


   /*
   Select a task based on the value of the action string.
   */
   if ( eqstr_c ( action, "SET") )
      {

      /*
      Operation is SET. "value" will be an input string.  Check
      value as well.
      */
      CHKFSTR ( CHK_STANDARD, "timdef_c", value );

      /*
      Call the f2c'd Fortran routine.
      */
      timdef_( ( char * ) action,
               ( char * ) item,
               ( char * ) value,
               ( ftnlen ) strlen(action),
               ( ftnlen ) strlen(item),
               ( ftnlen ) strlen(value)  );


      }

   else if ( eqstr_c (action, "GET" ) )
      {

      /*
      Operation is GET.  "action" will be an output string.  Make sure
      the output string has at least enough room for one output
      character and a null terminator.  Also check for a null pointer.
      */
      CHKOSTR ( CHK_STANDARD, "timdef_c", value, vallen );


      /*
      Call the f2c'd Fortran routine.
      */

      timdef_( ( char * ) action,
               ( char * ) item,
               ( char * ) value,
               ( ftnlen ) strlen(action),
               ( ftnlen ) strlen(item),
               ( ftnlen ) vallen - 1  );


      /*
      Convert our Fortran string to C.
      */
      F2C_ConvertStr( vallen, value );

      }

   else
      {

      setmsg_c ( "The action specified to timdef_c was "
                 "\"#\". This is not a recognized action. "
                 "The recognized actions are \"SET\" and "
                 "\"GET\"."                                );
      errch_c  ( "#",  action                              );
      sigerr_c ( "SPICE(BADACTION)"                        );
      chkout_c ( "timdef_c"                                );
      return;

      }


   chkout_c ( "timdef_c" );

} /* End timdef_c */
