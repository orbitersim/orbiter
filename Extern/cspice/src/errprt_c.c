/*

-Procedure errprt_c ( Get/Set Error Output Items )

-Abstract

   Retrieve or set the list of error message items
   to be output when an error is detected.

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

   void errprt_c ( ConstSpiceChar * op,
                   SpiceInt         lislen,
                   SpiceChar      * list  )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   op         I   The operation:  "GET" or "SET".
   lislen     I   Length of list for output.
   list      I-O  Specification of error messages to be output.

-Detailed_Input

   op          indicates the operation to be performed. Possible
               values are "GET" and "SET".

               "SET" means, "the following list specifies the default
               selection of error messages to be output." These are
               the messages that will be output to the default error
               output device (selected by errdev_c) when an error is
               detected.

               "GET" means, "return the current list of error output
               items." This is the exact list that was set by the
               last call to this routine with the "SET" option.

               The option can be specified in mixed case. For example,
               the following call will work:

               errprt_c ( "SeT", lislen, "ALL" )

   lislen      is the allowed length of `list' when `list' is returning a
               the error message list. The size described by `lislen'
               should be large enough to hold any possible output plus 1.

   list        is a list of error message items. The items
               are delimited by commas. The items that can be
               in the list are the words:

               1.  SHORT        ...indicates the short error message
               2.  EXPLAIN      ...the explanation of the short message
               3.  LONG         ...the long error message
               4.  TRACEBACK    ...the traceback
               5.  ALL          ...indicates "output all messages"
               6.  NONE         ...indicates "don't output any messages"
               7.  DEFAULT      ...same as ALL, but includes default
                                    message

               A "list" is a character string containing some or
               all of the above words, delimited by commas. Examples
               are:

               1.  "SHORT, EXPLAIN"
               2.  "SHORT, LONG"
               3.  "ALL"
               4.  "NONE"
               5.  "ALL, NONE, ALL, SHORT, NONE"

               Each word in the list can be thought of as
               "flipping a switch" to enable or disable the output
               of the message(s) indicated by the word. The
               words are acted on in the order they occur in the
               list, starting with the leftmost word. As examples,
               consider the sample lists above.

               The effect of the first list above, "SHORT, EXPLAIN",
               is to enable the output of the short error message
               and the explanatory text corresponding to it.

               The effect of the second list is to enable the output
               of the short and long messages.

               The effect of the third list is to enable the output of
               all of the error messages (short, long, explanation
               of the short message, and traceback).

               The effect of the fourth list is to disable output of
               all of the messages.

               The effect of the fifth list is to disable output of
               all of the messages. The reason for this is that
               the words in the list are responded to in order,
               from left to right, and "NONE" is the last word.

               If any words other than SHORT, LONG, EXPLAIN, ALL,
               DEFAULT, TRACEBACK or NONE appear in list, those words
               that are recognized are responded to. The words
               that are not recognized are diagnosed as
               erroneous, and error messages are generated
               for each such unrecognized word.

               The length of list is caller-defined, but only
               the first 100 characters of list will be saved
               for later retrieval.

               Only the first 10 items in the list are used;
               the rest are ignored.

-Detailed_Output

   list        is a list of error message items. The value of
               `list' is that set by the last call to this routine
               using the "SET" option. See "Detailed Input"
               for a description of the possible values and
               meanings of `list'.

               The initial value returned is "DEFAULT".

               Only the first 100 characters of `list' are saved
               when the list is set; any additional characters
               are truncated. Therefore, the first 100
               characters, at most, of the saved value of `list'
               will be returned.

-Parameters

   None.

-Exceptions

   1)  If an invalid value of the argument `op' is supplied, the error
       SPICE(INVALIDOPERATION) is signaled.

   2)  If `op' is "SET" and an invalid word is detected within the list
       of error message items `list', the error SPICE(INVALIDLISTITEM)
       is signaled by a routine in the call tree of this routine.

   3)  If the `op' input string pointer is null, the error
       SPICE(NULLPOINTER) is signaled.

   4)  If the `op' input string has zero length, the error
       SPICE(EMPTYSTRING) is signaled, since the input string cannot
       be converted to a Fortran-style string in this case.

   5)  If the `list' string pointer is null, the error
       SPICE(NULLPOINTER) is signaled.

   6)  If `op' is "SET" and the `list' input string has zero length, the
       error SPICE(EMPTYSTRING) is signaled, since the input string cannot
       be converted to a Fortran-style string in this case.

   7)  If `op' is "GET" and the `list' output string has length less than two
       characters, the error SPICE(STRINGTOOSHORT) is signaled, since the
       output string is too short to contain one character of output data plus
       a null terminator, and therefore it cannot be passed to the underlying
       Fortran routine.

-Files

   None.

-Particulars

   Please read the "required reading"!

   This routine is intended to be used in conjunction with
   errdev_c, which selects the default output device to which
   the error messages selected by this routine will be
   output.

   Additionally, the error response action must be
   something other than "IGNORE" if the error messages
   are to be output. Possible choices of the error
   response action are "RETURN", "REPORT", "ABORT", "DEFAULT", and
   "IGNORE".  Use erract_c to set the error response action.


   Only the first 100 characters of list are saved.

   The default set of error messages that are output is the
   set specified by "DEFAULT"; i.e., all of them, including
   the "default" message.

-Examples

   1. In this example, we select as the output device
       the file, SPUD.DAT, and then select the error
       messages to be output. We choose the short
       error message and the traceback. Since a
       different set of messages may have been selected
       previously, we clear the old setting by putting
       the word, "NONE", at the beginning of the list.

          /.
          Set the error output device to SPUD.DAT:
          ./
          errdev_c (  "SET", lislen, "SPUD.DAT" );

          /.
          Choose error messages:
          ./
          errprt_c (  "SET", lislen, "NONE, SHORT, TRACEBACK" );


   2. In this example we are retrieving the error message list.

          /.
          Declare the output string and its size.
          ./

         #define     LENOUT  50

         SpiceChar   list[ LENOUT ];

         errdev_c ( "GET", LENOUT, list );

-Restrictions

   1)  The device to which the selected error messages will be
       written must be selected via errdev_c; otherwise, messages will
       be written to the initial default device.

   2)  Only the first 100 characters of `list' are saved.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   E.D. Wright         (JPL)

-Version

   -CSPICE Version 1.4.0, 10-AUG-2021 (JDR)

       Changed the input argument "lenout" to "lislen" for consistency
       with other routines.

       Edited the header to comply with NAIF standard.

   -CSPICE Version 1.3.0, 24-JUN-2003 (NJB)

       Bug fix: case of invalid operation keyword is now
       diagnosed, as per the -Exceptions section of the header.

   -CSPICE Version 2.0.0, 09-FEB-1998 (NJB) (EDW)

       Input argument op was changed to type ConstSpiceChar *.

       Re-implemented routine without dynamically allocated, temporary
       strings.

       Corrected errors in examples in which the call sequence
       was incorrect.

   -CSPICE Version 1.0.0, 25-OCT-1997 (EDW)

-Index_Entries

   get/set error output items

-&
*/

{ /* Begin errprt_c */


   /*
   Participate in error tracing.
   */
   if ( return_c() )
   {
      return;
   }

   chkin_c ( "errprt_c" );


   /*
   Check the input string op to make sure the pointer is non-null
   and the string length is non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "errprt_c", op );


   if ( eqstr_c ( op, "SET") )
   {

      /*
      Operation is SET. The argument "list" will be an input string.
      Check "list" as well.
      */
      CHKFSTR ( CHK_STANDARD, "errprt_c", list );


      errprt_( ( char * ) op,
               ( char * ) list,
               ( ftnlen ) strlen(op),
               ( ftnlen ) strlen(list) );
   }

   else if ( eqstr_c (op, "GET" ) )
   {

      /*
      Operation is GET.  "list" will be an output string.

      Make sure the output string has at least enough room for one
      output character and a null terminator.  Also check for a null
      pointer.
      */
      CHKOSTR ( CHK_STANDARD, "errprt_c", list, lislen );

      /*
      After the routine call, create a C string from the
      Fortran output string.
      */
      errprt_( ( char * ) op,
               ( char * ) list,
               ( ftnlen ) strlen(op),
               ( ftnlen ) lislen-1     );


      F2C_ConvertStr( lislen, list );
   }

   else
   {
      setmsg_c ( "Input argument op had value: # "
                 "Valid choices are GET or SET."   );
      errch_c  ( "#",  op                          );
      sigerr_c ( "SPICE(INVALIDOPERATION)"         );
      chkout_c ( "errprt_c"                        );
      return;
   }


   chkout_c ( "errprt_c" );


} /* End errprt_c */
