/*

-Procedure trcnam_c ( Get module name from traceback )

-Abstract

   Return the name of the module having the specified position in
   the trace representation. The first module to check in is at
   index 0.

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

   void trcnam_c ( SpiceInt       index,
                   SpiceInt       namlen,
                   SpiceChar    * name     )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   index      I   The position of the requested module name.
   namlen     I   Available space in output name string.
   name       O   The name at position `index' in the traceback.
   SPICE_ERROR_MODLEN
              P   Maximum length of stored module names.

-Detailed_Input

   index       is the position in the traceback of the requested
               module name. Indices are zero-based: the index
               of the module at the start of the traceback is zero.
               The last module to check in is located at index
               depth - 1, where `depth' is the trace stack depth
               returned by trcdep_c.


   namlen      is the available space in the output string `name',
               including room for the null terminator.

-Detailed_Output

   name        is the name of the module at the position in
               the traceback indicated by `index'. `name' should
               be declared with length at least SPICE_ERROR_MODLEN.

               The meaning of the traceback depends on the state
               of the error handling mechanism. There are two
               cases:

                  1)  In RETURN mode, when an error is
                      signaled, the traceback at that point is
                      saved. trcdep_c, trcnam_c, and qcktrc_c
                      return values pertaining to the saved
                      traceback.

                  2)  In all other modes, the traceback represents
                      the CURRENT call chain. trcdep_c, trcnam_c,
                      and qcktrc_c return values pertaining to
                      the current trace representation.

-Parameters

   SPICE_ERROR_MODLEN

               is the maximum length of any module name that can be
               returned by this routine, including room for the null
               terminator. SPICE_ERROR_MODLEN is declared in SpiceErr.h.

-Exceptions

   If a SPICE error occurs, in order to ensure that this error can be
   signaled, this routine resets the SPICE error status. Error messages
   from any pre-existing error will be lost.

   1)  If `index' is out of range, the error SPICE(INDEXOUTOFRANGE) is
       signaled.

       Note that the underlying SPICELIB routine cannot signal an error
       if `index' is out of range.

   2)  If the output string pointer is null, the error SPICE(NULLPOINTER)
       is signaled.

   3)  If the output string has length less than 2 characters, the error
       SPICE(STRINGTOOSHORT) is signaled.

-Files

   None.

-Particulars

   SPICE-based applications can use this routine, together with
   trcdep_c, to create a customized traceback report. This is normally
   done after an application detects a SPICE error condition using
   failed_c.

   The CSPICE routine qcktrc_c is an alternative to the combination of
   trcdep_c and trcnam_c; qcktrc_c can be used to fetch a complete,
   fixed-format traceback string in a single call.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Deliberately generate a SPICE error to demonstrate use of
      this routine together with trcdep_c. We'll attempt to look up
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
         SpiceChar               longms  [ SPICE_ERROR_LMSGLN ];
         SpiceChar               modnam  [ SPICE_ERROR_MODLEN ];
         SpiceChar               shrtms  [ SPICE_ERROR_SMSGLN ];
         SpiceChar             * obsrvr;
         SpiceChar             * frame;
         SpiceChar             * target;

         SpiceDouble             et;
         SpiceDouble             lt;
         SpiceDouble             state [6];

         SpiceInt                depth;
         SpiceInt                i;

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
            An error has been signaled. First fetch the long
            and short error message. Next fetch the traceback depth,
            then fetch and display the module names.
            ./
            getmsg_c ( "SHORT", SPICE_ERROR_SMSGLN, shrtms );
            getmsg_c ( "LONG",  SPICE_ERROR_LMSGLN, longms );

            printf ( "\n%s\n", shrtms );
            printf ( "%s\n\n", longms );

            trcdep_c ( &depth );

            for ( i = 0;  i < depth;  i++ )
            {
               trcnam_c ( i, SPICE_ERROR_MODLEN, modnam );

               printf ( "Trace level: %d. Module name = %s\n",
                        (int)i,
                        modnam                                 );
            }

            /.
            Reset the error status so that CSPICE can resume normal
            operation.
            ./
            reset_c();
         }

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      =====================================================================

      Toolkit version: N0066

      SPICE(NOLOADEDFILES) --

      At least one SPK file needs to be loaded by SPKLEF before beginning a
      search.

      A traceback follows.  The name of the highest level module is first.
      spkezr_c --> SPKEZR --> SPKEZ --> SPKGEO --> SPKSFS

      =====================================================================

      SPICE(NOLOADEDFILES)
      At least one SPK file needs to be loaded by SPKLEF before beginning a ***

      Trace level: 0. Module name = spkezr_c
      Trace level: 1. Module name = SPKEZR
      Trace level: 2. Module name = SPKEZ
      Trace level: 3. Module name = SPKGEO
      Trace level: 4. Module name = SPKSFS


      Warning: incomplete output. 1 line extended past the right
      margin of the header and has been truncated. This line is
      marked by "***" at the end of the line.


      Note that the error SPICE(NOLOADEDFILES) is signaled as expected,
      since no SPK files have been loaded before calling spkezr_c.

-Restrictions

   1)  If a SPICE error occurs during the execution of this routine,
       this routine resets the SPICE error handling status. See the
       -Exceptions section above.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   K.R. Gehringer      (JPL)
   E.D. Wright         (JPL)

-Version

   -CSPICE Version 1.1.0, 10-AUG-2021 (JDR)

       Changed the input argument name "namelen" to "namlen" for consistency
       with other routines.

       Edited the header to comply with NAIF standard.

   -CSPICE Version 1.0.1, 12-JUL-2016 (EDW)

       Edit to example program to use "%d" with explicit casts
       to int for printing SpiceInts with printf.

   -CSPICE Version 1.0.0, 05-NOV-2013 (NJB) (KRG)

-Index_Entries

   get module name from traceback

-&
*/

{ /* Begin trcnam_c */

   /*
   Local variables
   */
   SpiceInt                depth;
   SpiceInt                i;


   /*
   This routine doesn't check in unless an input error is
   detected.
   */

   /*
   Make sure the output string has at least enough room for one output
   character and a null terminator. Also check for a null pointer.

   We don't use the usual CHKOSTR macro here because we must reset
   the error status before signaling an error.
   */
   if ( name == NULL )
   {
      /*
      We must reset the error handling status in order to
      be able to signal an error.
      */
      reset_c  ();

      chkin_c  ( "trcnam_c"                                  );
      setmsg_c ( "The output string pointer 'name' is null." );
      sigerr_c ( "SPICE(NULLPOINTER)"                        );
      chkout_c ( "trcnam_c"                                  );
      return;
   }

   if ( namlen < 2 )
   {
      /*
      We must reset the error handling status in order to
      be able to signal an error.
      */
      reset_c  ();

      chkin_c  ( "trcnam_c"                                    );
      setmsg_c ( "The output string 'name' has length #; the "
                 "minimum allowed length is 2 characters."     );
      errint_c ( "#",  namlen                                 );
      sigerr_c ( "SPICE(STRINGTOOSHORT)"                       );
      chkout_c ( "trcnam_c"                                    );
      return;
   }


   /*
   Check the module index.
   */
   trcdep_c ( &depth );

   if (  ( index < 0 ) || ( index >= depth )  )
   {
      /*
      We must reset the error handling status in order to
      be able to signal an error.
      */
      reset_c  ();

      chkin_c  ( "trcnam_c"                               );
      setmsg_c ( "Module index must be in the range #:# "
                 "but is #."                              );
      errint_c ( "#",  0                                  );
      errint_c ( "#",  depth - 1                          );
      errint_c ( "#",  index                              );
      sigerr_c ( "SPICE(INDEXOUTOFRANGE)"                 );
      chkout_c ( "trcnam_c"                               );
      return;
   }


   /*
   Convert the input C style index to Fortran style.
   */
   i = index + 1;

   /*
   Fetch the module name.
   */
   trcnam_ ( ( integer    * ) &i,
             ( char       * ) name,
             ( ftnlen       ) namlen-1 );

   /*
   Convert the output name string to a null-terminated,
   C style string.
   */
   F2C_ConvertStr ( namlen, name );


} /* End trcnam_c */
