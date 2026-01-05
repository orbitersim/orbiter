/*

-Procedure gfinth_c ( GF, interrupt signal handler )

-Abstract

   Respond to the interrupt signal SIGINT: save an indication
   that the signal has been received. This routine restores
   itself as the handler for SIGINT.

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

   GF

-Keywords

   GEOMETRY
   SEARCH
   UTILITY

*/

   #include <signal.h>
   #include "SpiceUsr.h"


   void gfinth_c ( int sigcode )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   sigcode    I   Interrupt signal ID code.

-Detailed_Input

   sigcode     is a signal code. `sigcode' is expected to be the ANSI C
               parameter SIGINT, which represents the interrupt signal.

-Detailed_Output

   None.

-Parameters

   None.

-Exceptions

   1)  If `sigcode' is not SIGINT, the error SPICE(INVALIDSIGNAL)
       is signaled (in the SPICE error handling sense).

   2)  If the call to the ANSI C function `signal' made
       by this routine fails, the error SPICE(SIGNALFAILED)
       is signaled (via SPICE error handling).

-Files

   None.

-Particulars

   This interrupt handler should be used by routines that
   participate in GF interrupt handling. Such routines should
   call the ANSI C library routine `signal' with the ANSI C
   macro SIGINT and this routine as the input arguments.

   When this routine executes, it re-establishes itself as the
   handler for the interrupt signal SIGINT. Code that uses
   CSPICE interrupt handling must restore the previous
   handler before returning.

   Once this routine is established as the handler for the
   interrupt signal SIGINT, the GF "bail out" test routine
   gfbail_c will return SPICETRUE until the interrupt status
   is cleared via a call to gfclrh_c.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as input,
   the compiler and supporting libraries, and the machine specific
   arithmetic implementation.

   1) Make this routine the GF signal handler, then restore
      the previous handler. This example serves only to
      demonstrate the use of signal; the example code
      performs no useful function.


      Example code begins here.


      /.
         Program gfinth_ex1
      ./
      #include <stdio.h>
      #include <signal.h>
      #include "SpiceUsr.h"

      int main()
      {
         /.
         Prototypes
         ./
         static void      ( * previousHandler )(int);
         static void      ( * handlerPtr      )(int);

         /.
         Make gfinth_c the handler for the SIGINT signal.
         ./
         previousHandler = signal ( SIGINT, gfinth_c );

         if ( previousHandler == SIG_ERR )
         {
            setmsg_c ( "Attempt to establish gfinth_c as the "
                       "handler for the SIGINT signal failed." );
            sigerr_c ( "SPICE(SIGNALFAILED)"                   );
         }
         printf( "gfinth_c has been established as handler "
                 "for SIGNIT signal.\n"                     );

         /.
         Restore the previous handler.
         ./
         handlerPtr = signal ( SIGINT, previousHandler );

         if ( handlerPtr == SIG_ERR )
         {
            setmsg_c ( "Attempt to re-establish the previous "
                       "handler for the SIGINT signal failed." );
            sigerr_c ( "SPICE(SIGNALFAILED)"                   );
         }
         printf( "The previous handler has been re-established "
                 "for SIGNIT signal.\n"                        );

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      gfinth_c has been established as handler for SIGNIT signal.
      The previous handler has been re-established for SIGNIT signal.


-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)

-Version

   -CSPICE Version 1.0.1, 05-AUG-2021 (JDR)

       Edited the header to comply with NAIF standard. Extended
       code example to generate outputs and provided example's solution.

   -CSPICE Version 1.0.0, 25-FEB-2009 (NJB)

-Index_Entries

   GF handle interrupt signal

-&
*/

{ /* Begin gfinth_c */

   /*
   Prototypes
   */
   void                      zzgfsavh_c ( SpiceBoolean status );


   /*
   Local variables
   */
   void                  ( * handler )( int );


   /*
   This routine uses discovery check-in.
   */

   if ( sigcode == SIGINT )
   {
      /*
      Re-establish this routine as the signal handler
      for SIGINT.
      */
      handler = signal ( SIGINT, gfinth_c );

      if ( handler == SIG_ERR )
      {
         setmsg_c ( "Attempt to establish gfinth_c as the "
                    "handler for the SIGINT signal failed." );
         sigerr_c ( "SPICE(SIGNALFAILED)"                   );
      }


      /*
      An interrupt signal has been received. Update the
      signal status.
      */
      zzgfsavh_c ( SPICETRUE );
   }
   else
   {
      /*
      This handler should not receive any other signal.
      */
      chkin_c  ( "gfinth_c"                                  );
      setmsg_c ( "A signal other than SIGINT was received. "
                 "The signal code was #."                    );
      errint_c ( "#",  (SpiceInt)sigcode                     );
      sigerr_c ( "SPICE(INVALIDSIGNAL)"                      );
      chkout_c ( "gfinth_c"                                  );
   }


} /* End gfinth_c */
