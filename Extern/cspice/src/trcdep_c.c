/*

-Procedure trcdep_c ( Traceback depth )

-Abstract

   Return the number of modules in the traceback representation.

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

   void trcdep_c ( SpiceInt  * depth )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  ---------------------------------------------------
   depth      O   The number of modules in the traceback.

-Detailed_Input

   None.

-Detailed_Output

   depth       indicates the number of module names in the traceback
               representation.

               The module names represent modules in a call chain,
               with the first name being the top-level module, and
               the name with index `depth' being the lowest level
               module.

               The meaning of the traceback depends on the state of
               the error handling mechanism. There are two cases:

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

   None.

-Exceptions

   Error free.

-Files

   None.

-Particulars

   SPICE-based applications can use this routine, together with
   trcnam_c, to create a customized traceback report. This is normally
   done after an application detects a SPICE error condition using
   failed_c.

   The CSPICE routine qcktrc_c is an alternative to the combination of
   trcdep_c and trcnam_c; qcktrc_c can be used to fetch a complete,
   fixed-format traceback string in a single call.

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

         SPICE(NOLOADEDFILES)
         At least one SPK file needs to be loaded by SPKLEF before beginning
         a search.

         Trace level: 0. Module name = spkezr_c
         Trace level: 1. Module name = SPKEZR
         Trace level: 2. Module name = SPKEZ
         Trace level: 3. Module name = SPKGEO
         Trace level: 4. Module name = SPKSFS


-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   K.R. Gehringer      (JPL)
   E.D. Wright         (JPL)

-Version

   -CSPICE Version 1.0.2, 02-JUN-2021 (JDR)

       Edited the header to comply with NAIF standard.

   -CSPICE Version 1.0.1, 12-JUL-2016 (EDW)

       Edit to example program to use "%d" with explicit casts
       to int for printing SpiceInts with printf.

   -CSPICE Version 1.0.0, 05-NOV-2013 (NJB) (KRG)

-Index_Entries

   return traceback depth

-&
*/

{ /* Begin trcdep_c */


   /*
   This routine does not check in.
   */

   trcdep_ ( (integer *) depth );


} /* End trcdep_c */
