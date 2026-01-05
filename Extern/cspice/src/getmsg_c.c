/*

-Procedure getmsg_c ( Get Error Message )

-Abstract

   Retrieve the current short error message, the explanation of the
   short error message, or the long error message.

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
   #include "SpiceZfc.h"
   #include "SpiceZst.h"
   #include "SpiceZmc.h"


   void getmsg_c ( ConstSpiceChar  * option,
                   SpiceInt          msglen,
                   SpiceChar       * msg     )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   option     I   Indicates type of error message.
   msglen     I   Available space in the output string `msg'.
   msg        O   The error message to be retrieved.

-Detailed_Input

   option      is a string that indicates the type of error message to
               be retrieved.

               Possible values of `option' are:

                  "SHORT"     indicates that the short message is to be
                              retrieved.

                  "EXPLAIN"   indicates that the explanation of the
                              short message is to be retrieved.

                  "LONG"      indicates that the long message is to be
                              retrieved.

               The input strings indicating the choice of option may be
               in mixed case. Leading and trailing blanks in `option' are
               not significant.

   msglen      is the maximum allowed length of the output message string,
               including the terminating null character. For example,
               if the caller wishes to be able to accept an 1840-character
               message, `msglen' must be set to (at least) 1841. The current
               maximum long error message length is in fact 1840 characters.

-Detailed_Output

   msg         is the error message to be retrieved. Its value depends
               on `option', and on whether an error condition exists.

               When there is no error condition, `msg' is empty.

               If an error condition does exist, and `option' is

                 "SHORT"        `msg' is the current short error message.
                                This is a very condensed, 25-character
                                description of the error.

                 "EXPLAIN"      `msg' is the explanation of the current
                                short error message. This is a one-line
                                expansion of the text of the short
                                message.

                                Most CSPICE short error messages
                                have corresponding explanation text.
                                For other short error messages, if
                                there is no explanation text, `msg'
                                will be blank.

                 "LONG"         `msg' is the current long error message.
                                The long error message is a detailed
                                explanation of the error, possibly
                                containing data specific to the
                                particular occurrence of the error.
                                Not all errors have long error messages.
                                If there is none, `msg' will be blank.
                                Long error messages are no longer than
                                320 characters.

               If `option' is invalid, `msg' will remain unchanged from its
               value on input.

-Parameters

   None.

-Exceptions

   1)  If the input `option' is invalid, the error
       SPICE(INVALIDMSGTYPE) is signaled by a routine in the call
       tree of this routine. In that case no messages are returned;
       `msg' retains the value it had on input.

   2)  If the `option' input string pointer is null, the error
       SPICE(NULLPOINTER) is signaled.

   3)  If the `option' input string has zero length, the error
       SPICE(EMPTYSTRING) is signaled.

   4)  If the `msg' output string pointer is null, the error
       SPICE(NULLPOINTER) is signaled.

   5)  If the `msg' output string has length less than two
       characters, the error SPICE(STRINGTOOSHORT) is signaled, since
       the output string is too short to contain one character of
       output data plus a null terminator.

-Files

   None.

-Particulars

   Please read the "required reading" first!

   A good time to call this routine would be when an error
   condition exists, as indicated by the CSPICE function,
   failed_c.

   See the example below for a serving suggestion.

   getmsg_c isn't too useful if an error condition doesn't
   exist, since it will return a blank string in that case.

-Examples

   Here's an example of a real-life call to getmsg_c to get the
   explanation of the current short error message.

   In this example, a CSPICE routine, ckopn_c, is called.
   Following the return from ckopn_c, the logical function,
   failed_c, is tested to see whether an error occurred.
   If it did, the message is retrieved and output via
   a user-defined output routine:

      #include "SpiceUsr.h"
      #include <stdio.h>

      #define  MSGLEN         1841

      SpiceChar               msg [ MSGLEN ];

            .
            .
            .
      /.
      We call ckopn_c; then test for errors...
      ./

      ckopn_c ( filename, ifname, ncomch, &handle );

      if ( failed_c() )
      {
         /.
         Get explanation text for the current short message
         and print it:
         ./

         getmsg_c ( "EXPLAIN", MSGLEN, msg );

         [Output message]
                  .
                  .
                  .
      }

-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)

-Version

   -CSPICE Version 1.1.0, 02-AUG-2021 (JDR)

       Changed the input argument "lenout" to "msglen" for consistency
       with other routines.

       Edited the header to comply with NAIF standard.

   -CSPICE Version 1.0.0, 05-APR-1999 (NJB)

-Index_Entries

   get error message

-&
*/

{ /* Begin getmsg_c */



   /*
   Participate in error tracing.
   */
   chkin_c ( "getmsg_c" );


   /*
   Check the input string op to make sure the pointer is non-null
   and the string length is non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "getmsg_c", option );

   /*
   Make sure the output string has at least enough room for one output
   character and a null terminator.  Also check for a null pointer.
   */
   CHKOSTR ( CHK_STANDARD, "getmsg_c", msg, msglen );


   /*
   Call the f2c'd Fortran routine.
   */
   getmsg_ ( ( char * ) option,
             ( char * ) msg,
             ( ftnlen ) strlen(option),
             ( ftnlen ) msglen-1       );

   /*
   Convert the output string from Fortran-style to C-style.
   */
   F2C_ConvertStr( msglen, msg );


   chkout_c ( "getmsg_c" );

} /* End getmsg_c */
