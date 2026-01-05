/*

-Procedure erract_c ( Get/Set Default Error Action )

-Abstract

   Retrieve or set the default error action.

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

   ERROR

-Keywords

   ERROR

*/

   #include "SpiceUsr.h"
   #include "SpiceZmc.h"
   #include "SpiceZfc.h"
   #include "SpiceZst.h"

   void erract_c ( ConstSpiceChar * op,
                   SpiceInt         actlen,
                   SpiceChar      * action )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   op         I   Operation -- "GET" or "SET".
   actlen     I   Length of list for output.
   action    I-O  Error response action.

-Detailed_Input

   op          indicates the operation -- "GET" or "SET".  "GET" means,
               "Set action to the current value of the error response
               action."  "SET" means, "update the error response action to
               the value indicated by action."

               `op' may be in mixed case; for example,

                  erract_c ( "gEt", actlen, action );

               will work.

   actlen      is the string size of output `action' when op equals "GET".
               The size described by `actlen' should be large enough to
               hold all characters of any possible output string
               plus 1 (to accommodate the C null terminator).

   action      is an input argument when `op' is "SET." It takes the
               values,  "ABORT",  "IGNORE", "REPORT", "RETURN", and
               "DEFAULT".

               Briefly, the meanings of the error response
               choices are as follows:

               1.  "ABORT"  --  When an error is detected by a
                                CSPICE routine, or when
                                ANY routine signals detection
                   of an error via a call to sigerr_c, the
                   toolkit will output any error messages that
                   it has been enabled to output (see errprt_c
                   and errdev_c also ), and then execute an
                   exit statement.

               2.  "REPORT" --  In this mode, the toolkit does
                                NOT abort when errors are detected.
                                When sigerr_c is called to report
                   an error, all error messages that the toolkit
                   is enabled to output will be sent to the
                   designated error output device. Similarly,
                   a call to setmsg_c will result in the long
                   error message being output, if the toolkit
                   is enabled to output it.

               3.  "RETURN" --  In this mode, the toolkit also
                                does NOT abort when errors are
                                detected. Instead, error messages
                   are output if the toolkit is enabled to do
                   so, and subsequently, ALL TOOLKIT ROUTINES
                   RETURN IMMEDIATELY UPON ENTRY until the
                   error status is reset via a call to RESET.
                   (No, RESET itself doesn't return on entry).
                   Resetting the error status will cause the
                   toolkit routines to resume their normal
                   execution threads.


               4.  "IGNORE" --  The toolkit will not take any
                                action in response to errors;
                                calls to sigerr_c will have no
                                effect.

               5.  "DEFAULT" -- This mode is the same as "ABORT",
                                except that an additional error
                                message is output. The additional
                                message informs the user that the
                                error response action can be
                                modified, and refers to documentation
                                of the error handling feature.

               `action' may be in mixed case; for example,

                   erract_c ( "SET", actlen, "igNORe" );

               will work.

-Detailed_Output

   action      is an output argument returning the current error
               response action when `op' equals "GET". Possible values
               are:  "ABORT", "REPORT", "RETURN", and "IGNORE".
               See "Detailed Input" for descriptions of these values.

-Parameters

   None.

-Exceptions

   1)  If an invalid value of the operation `op' is supplied, the error
       SPICE(INVALIDOPERATION) is signaled.

   2)  If `op' is "SET" and the input argument `action' does not indicate
       a valid error handling action, the error SPICE(INVALIDACTION)
       is signaled by a routine in the call tree of this routine.

   3)  If the `op' input string pointer is null, the error
       SPICE(NULLPOINTER) is signaled.

   4)  If the `op' input string has zero length, the error
       SPICE(EMPTYSTRING) is signaled, since the input string cannot
       be converted to a Fortran-style string in this case.

   5)  If the `action' string pointer is null, the error SPICE(NULLPOINTER) is
       signaled.

   6)  If `op' is "SET" and the `action' string has zero length, the error
       SPICE(EMPTYSTRING) is signaled, since the input string cannot
       be converted to a Fortran-style string in this case.

   7)  If `op is "GET" and the `action' string has length less than
       two characters, the error SPICE(STRINGTOOSHORT) is signaled,
       since the output string is too short to contain one character
       of output data plus a null terminator, and therefore it cannot
       be passed to the underlying Fortran routine.

-Files

   None.

-Particulars

   As indicated in the "detailed input" section above, the choices for
   the Toolkit's error handling action are designated by the strings
   "ABORT", "REPORT", "RETURN", "IGNORE", and "DEFAULT".  These
   choices control the way the toolkit behaves when an error is
   detected. The toolkit thinks an error has been detected when
   sigerr_c is called.

   1.  "ABORT"   In this mode, the toolkit sends error messages
        to the error output device and then stops.
        This is the default mode. It is probably
        the one to choose for running non-interactive programs.
        You may also wish to use this for programs which
        have many bugs, or in other cases where continued
        operation following detection of an error isn't useful.

   2.  "REPORT"  In this mode, the toolkit sends error messages
        to the error output device and keeps going. This mode
        may be useful if you are debugging a large program,
        since you can get more information from a single test run.
        You will probably want to use errdev_c to indicate a file
        where your error messages should be sent.

   3.  "RETURN"  In this mode, the toolkit also sends error messages
        to the error output device and "keeps going".  But
        instead of following their normal execution threads,
        the toolkit routines will simply return immediately upon
        entry, once an error has been detected.
        The availability of this feature makes it safe to call
        multiple toolkit routines without checking the error
        status after each one returns; if one routine detects
        an error, subsequent calls to toolkit routines will have
        no effect; therefore, no crash will occur. The error
        messages set by the routine which detected the error
        will remain available for retrieval by getmsg_.

   4.   "IGNORE"  This mode can be dangerous!  It is best
        used when running a program whose behavior you
        understand well, in cases where you wish to suppress
        annoying messages.  BUT, if an unexpected error
        occurs, you won't hear about it from anyone, except
        possibly your run-time system.

   5.  "DEFAULT"  As the name suggests, this is the default
        error handling mode. The error handling mechanism
        starts out in this mode when a program using the
        toolkit is run, and the mode remains "DEFAULT" until
        it is changed via a call to this routine.
        This mode is the same as "ABORT",
        except that an additional error message is output.
        The additional message informs the user that the
        error response action can be modified, and refers
        to documentation of the error handling feature.


   NOTE:

        By default, error messages are printed to the screen
        when errors are detected. You may want to send them
        to a different output device, or choose a subset to
        output. Use the routines errdev_c and errprt_c to choose
        the output device and select the messages to output,
        respectively.

        You can also suppress the automatic output of messages
        and retrieve them directly in your own program. getmsg_
        can be used for this. To make sure that the messages
        retrieved correspond to the FIRST error that occurred,
        use "RETURN" mode. In "REPORT" mode, new messages
        overwrite old ones in the CSPICE message storage
        area, so getmsg_ will get the messages from the LATEST
        error that occurred.

-Examples

   1. Setting up "ABORT" mode:

          /.
          We wish to have our program abort if an error
          is detected. But instead of having the error
          messages printed on the screen, we want them
          to be written to the file, ERROR_LOG.TXT

          We want to see all of the messages, so we
          call errprt_c, using the "ALL" option.

          Finally, we call erract_c to set the action to "ABORT":
          ./

          errdev_c ( "SET", actlen, "ERROR_LOG.DAT" );

          errprt_c ( "SET", actlen, "ALL" );

          erract_c ( "SET", actlen, "ABORT" );



   2. Setting up "REPORT" mode:

          errdev_c ( "SET", actlen, "ERROR_LOG.DAT" );

          errprt_c ( "SET", actlen, "ALL"  );

          erract_c ( "SET", actlen, "REPORT" );


   3. Setting up "RETURN" mode: This is the same
       as example #2, except that the erract_c call becomes:

          erract_c ( "SET", actlen, "RETURN" );



   4. Setting up "IGNORE" mode:

          /.
          In this case, we aren't going to have
          ANY error messages (unless the call
          to erract_c itself fails), so we don't
          really need to call errprt_c and errdev_c.
          (If the call to erract_c DOES fail, which
          it can do only if we misspell "IGNORE,"
          the resulting error messages will go to
          the screen).
          ./

          erract_c ( "SET", actlen, "IGNORE" );

-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   E.D. Wright         (JPL)

-Version

   -CSPICE Version 1.4.0, 10-AUG-2021 (JDR)

       Changed the input argument name "lenout" to "actlen" for consistency
       with other routines.

       Edited the header to comply with NAIF standard.

   -CSPICE Version 1.3.1, 25-SEP-2003 (EDW)

       Corrected confusing description of 'actlen' argument.

   -CSPICE Version 1.3.0, 24-JUN-2003 (NJB)

       Bug fix: case of invalid operation keyword is now
       diagnosed, as per the -Exceptions section of the header.

   -CSPICE Version 1.2.0, 09-FEB-1998 (NJB)

       Re-implemented routine without dynamically allocated, temporary
       strings. Made various header fixes.

   -CSPICE Version 1.0.1, 30-OCT-1997 (EDW)

       Corrected errors in examples in which the call sequence
       was incorrect.

   -CSPICE Version 1.0.0, 25-OCT-1997 (EDW)

-Index_Entries

   get/set default error action

-&
*/

{ /* Begin erract_c */


   /*
   Participate in error tracing.
   */
   if ( return_c() )
   {
      return;
   }

   chkin_c ( "erract_c" );


   /*
   Check the input string op to make sure the pointer is non-null
   and the string length is non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "erract_c", op );


   if( eqstr_c ( op, "SET") )
   {

      /*
      Operation is SET. "action" will be an input string.  Check
      action as well.
      */
      CHKFSTR ( CHK_STANDARD, "erract_c", action );

      /*
      Call the f2c'd Fortran routine.
      */
      erract_ ( ( char * ) op,
                ( char * ) action,
                ( ftnlen ) strlen(op),
                ( ftnlen ) strlen(action) );
   }

   else if ( eqstr_c (op, "GET" ) )
   {

      /*
      Operation is GET.  "action" will be an output string.  Make sure
      the output string has at least enough room for one output
      character and a null terminator.  Also check for a null pointer.
      */
      CHKOSTR ( CHK_STANDARD, "erract_c", action, actlen );


      /*
      Call the f2c'd Fortran routine.
      */
      erract_ ( ( char * ) op,
                ( char * ) action,
                ( ftnlen ) strlen(op),
                ( ftnlen ) actlen-1    );


      F2C_ConvertStr( actlen, action );
   }

   else
   {
      setmsg_c ( "Input argument op had value: # "
                 "Valid choices are GET or SET."   );
      errch_c  ( "#",  op                          );
      sigerr_c ( "SPICE(INVALIDOPERATION)"         );
      chkout_c ( "erract_c"                        );
      return;
   }


   chkout_c ( "erract_c" );

} /* End erract_c */
