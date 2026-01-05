/*

-Procedure qcktrc_c ( Get Quick Traceback )

-Abstract

   Return a string containing a traceback.

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

   void qcktrc_c ( SpiceInt     tracelen,
                   SpiceChar  * trace    )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   tracelen   I   Maximum length of output traceback string.
   trace      O   A traceback string.
   SPICE_ERROR_MAXMOD
              P   Maximum traceback module count.
   SPICE_ERROR_MODLEN
              P   Maximum module name length.
   SPICE_ERROR_TRCLEN
              P   Maximum length of output traceback string.

-Detailed_Input

   None.

-Detailed_Output

   trace       is a list of module names, delimited by the string,
               " --> ".  An example would be

                  "SPUD --> SPAM --> FOOBAR".

               The maximum length of the returned string is given
               by the parameter SPICE_ERROR_TRCLEN. The value of this
               parameter includes room for the terminating null.

               In general, the meaning of the trace is as follows:

               The first name in the list is the name of the first
               module to check in (that hasn't yet checked out). The
               last name is the name of the module at the end of the
               call chain; this is the last module that checked in.

               The meaning of the traceback depends on the state
               of the error handling mechanism. There are two
               cases:

                  1)  In RETURN mode, when an error is signaled, the
                      traceback at that point is saved. trcdep_c,
                      trcnam_c, and qcktrc_c return values
                      pertaining to the saved traceback.

                  2)  In all other modes, the traceback represents
                      the CURRENT call chain. trcdep_c, trcnam_c,
                      and qcktrc_c return values pertaining to the
                      current trace representation.

               Any module names exceeding SPICE_ERROR_MODLEN
               characters in length are truncated on the right.

-Parameters

   The following parameters are declared in the header file SpiceErr.h:


   SPICE_ERROR_MAXMOD    is the maximum number of module names that can
                         be accommodated in the SPICE trace stack; this
                         is the maximum number of names that can appear
                         in the output traceback.

   SPICE_ERROR_MODLEN    is the maximum module name length that can be
                         accommodated by this routine.

   SPICE_ERROR_TRCLEN    is the maximum length of the string returned
                         by this routine. The value of this parameter
                         includes room for the terminating null.

-Exceptions

   1)  If the `trace' output string pointer is null, the error
       SPICE(NULLPOINTER) is signaled.

   2)  If the `trace' output string has length less than two
       characters, the error SPICE(STRINGTOOSHORT) is signaled, since
       the output string is too short to contain one character of
       output data plus a null terminator.

-Files

   None.

-Particulars

   This routine is part of the CSPICE error handling mechanism.

-Examples

   1) Deliberately generate a SPICE error to demonstrate use of
      this routine together with trcnam_c. We'll attempt to look up
      a state vector via spkezr_c without first having loaded any
      SPK files.


      Example code begins here.


      #include <stdio.h>
      #include "SpiceUsr.h"

      int main()
      {
         /.
         Local constants
         ./
         #define ACTION          "RETURN"

         /.
         Local variables
         ./
         SpiceChar             * abcorr;
         SpiceChar               trace  [ SPICE_ERROR_TRCLEN ];
         SpiceChar             * obsrvr;
         SpiceChar             * frame;
         SpiceChar             * target;

         SpiceDouble             et;
         SpiceDouble             lt;
         SpiceDouble             state [6];

         /.
         Set error handling action to RETURN so that this program
         won't terminate when a SPICE error is signaled. Note that
         the input string length argument is unused for a "SET"
         operation.
         ./
         erract_c ( "SET", 0, ACTION );

         /.
         Generate a SPICE error: call spkezr_c without first having
         loaded an SPK file.
         ./
         et     = 0.0;
         target = "Moon";
         obsrvr = "Earth";
         frame  = "J2000";
         abcorr = "NONE";

         spkezr_c ( target, et, frame, abcorr, obsrvr, state, &lt );

         if ( failed_c() )
         {
            /.
            An error has been signaled. Fetch and display
            the traceback.
            ./
            qcktrc_c ( SPICE_ERROR_TRCLEN, trace );

            printf ( "Traceback: \n%s\n", trace );
            /.
            Reset the error status so that CSPICE can resume normal
            operation.
            ./
            reset_c();
         }

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit platform, the
      output (which has been reformatted to fit in the available
      space in this header) was:


         ====================================================================
         ============

         Toolkit version: N0065

         SPICE(NOLOADEDFILES) --

         At least one SPK file needs to be loaded by SPKLEF before beginning
         a search.

         A traceback follows.  The name of the highest level module is first.
         spkezr_c --> SPKEZR --> SPKEZ --> SPKGEO --> SPKSFS

         ====================================================================
         ============
         Traceback:
         spkezr_c --> SPKEZR --> SPKEZ --> SPKGEO --> SPKSFS


-Restrictions

   1)  It is assumed no module names exceed SPICE_ERROR_MODLEN
       characters in length.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   K.R. Gehringer      (JPL)

-Version

   -CSPICE Version 1.0.1, 05-AUG-2021 (JDR)

       Edited the header to comply with NAIF standard.

   -CSPICE Version 1.0.0, 05-NOV-2013 (NJB) (KRG)

-Index_Entries

   get quick traceback

-&
*/

{ /* Begin qcktrc_c */


   /*
   This routine does not check in unless an input error occurs.
   */


   /*
   Make sure the output string has at least enough room for one output
   character and a null terminator. Also check for a null pointer.

   We don't use the usual CHKOSTR macro here because we must reset
   the error status before signaling an error.
   */
   if ( trace == NULL )
   {
      reset_c  ();

      chkin_c  ( "qcktrc_c"                                   );
      setmsg_c ( "The output string pointer 'trace' is null." );
      sigerr_c ( "SPICE(NULLPOINTER)"                         );
      chkout_c ( "qcktrc_c"                                   );
      return;
   }

   if ( tracelen < 2 )
   {
      reset_c  ();

      chkin_c  ( "qcktrc_c"                                     );
      setmsg_c ( "The output string 'trace' has length #; the "
                 "minimum allowed length is 2 characters."      );
      errint_c ( "#",  tracelen                                 );
      sigerr_c ( "SPICE(STRINGTOOSHORT)"                        );
      chkout_c ( "qcktrc_c"                                     );
      return;
   }


   /*
   Fetch the traceback.
   */
   qcktrc_ ( ( char       * ) trace,
             ( ftnlen       ) tracelen-1 );

   /*
   Convert the output name string to a null-terminated,
   C style string.
   */
   F2C_ConvertStr ( tracelen, trace );


} /* End qcktrc_c */
