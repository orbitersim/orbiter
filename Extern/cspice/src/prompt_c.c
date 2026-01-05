/*

-Procedure prompt_c ( Prompt a user for a string )

-Abstract

   Prompt a user for keyboard input.

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

   UTILITY

*/

   #include <stdlib.h>
   #include <stdio.h>

   #include "SpiceUsr.h"
   #include "SpiceZmc.h"

   SpiceChar * prompt_c ( ConstSpiceChar * dspmsg,
                          SpiceInt         buflen,
                          SpiceChar      * buffer )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   dspmsg     I   The prompt string to display when asking for input.
   buflen     I   Minimum number of characters for response plus one.
   buffer     O   The string containing the response typed by a user.

   The routine returns a pointer to the output buffer.

-Detailed_Input

   dspmsg      is a character string displayed from the current cursor
               position which describes the requested input. The prompt
               string should be relatively short, i.e., 50 or fewer
               characters, so a response may be typed on the line where
               the prompt appears.

               All characters (including trailing blanks) in `dspmsg'
               are considered significant and will be displayed.

   buflen      is the integer number of characters plus one for the
               response string.

-Detailed_Output

   buffer      is the user supplied string which holds the response. The
               string's memory is allocated in the calling routine.

   The routine returns a pointer to buffer as well as passing the
   pointer back via an argument.

-Parameters

   None.

-Exceptions

   1)  If the `buffer' output string pointer is null, the error
       SPICE(NULLPOINTER) is signaled.

   2)  If the `buffer' output string has length less than two
       characters, the error SPICE(STRINGTOOSHORT) is signaled.

-Files

   None.

-Particulars

   This is a utility that allows you to "easily" request information
   from a program user. The calling program declares an array or
   allocate memory to contain the user's response to the prompt.

-Examples

   The numerical results shown for these examples may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Suppose you have an interactive program that computes state
      vectors by calling spkezr_c. The program prompts the user for
      the inputs to spkezr_c. After each prompt is written, the program
      leaves the cursor at the end of the string as shown here:

         Enter UTC epoch  > _

      (The underscore indicates the cursor position).

      The following code example illustrates the acquisition of input
      values using prompt_c as a void function.

      Use the meta-kernel shown below to load the required SPICE
      kernels.


         KPL/MK

         File: prompt_ex1.tm

         This meta-kernel is intended to support operation of SPICE
         example programs. The kernels shown here should not be
         assumed to contain adequate or correct versions of data
         required by SPICE-based user applications.

         In order for an application to use this meta-kernel, the
         kernels referenced here must be present in the user's
         current working directory.

         The names and contents of the kernels referenced
         by this meta-kernel are as follows:

            File name                        Contents
            ---------                        --------
            de430.bsp                        Planetary ephemeris
            mar097.bsp                       Mars satellite ephemeris
            naif0011.tls                     Leapseconds


         \begindata

            KERNELS_TO_LOAD = ( 'de430.bsp',
                                'mar097.bsp',
                                'naif0011.tls' )

         \begintext

         End of meta-kernel


      Example code begins here.


      /.
         Program prompt_ex1
      ./
      #include <stdlib.h>
      #include <stdio.h>
      #include "SpiceUsr.h"


      int main()
      {
         /.
         Local parameters
         ./
         #define   STRLEN    37

         /.
         Local variables.
         ./
         SpiceChar      utc    [STRLEN];
         SpiceChar      obs    [STRLEN];
         SpiceChar      targ   [STRLEN];

         SpiceDouble    et;
         SpiceDouble    lt;
         SpiceDouble    state [6];

         /.
         Load kernel.
         ./
         furnsh_c ( "prompt_ex1.tm" );

         /.
         Prompt for the required inputs
         ./
         prompt_c ( "Enter UTC epoch             > ", STRLEN, utc  );
         prompt_c ( "Enter observer name         > ", STRLEN, obs  );
         prompt_c ( "Enter target name           > ", STRLEN, targ );

         /.
         Convert the UTC request time to ET (seconds past
         J2000, TDB).
         ./
         str2et_c ( utc, &et );

         /.
         Look up the state vector at the requested `et'
         ./
         spkezr_c ( targ, et, "J2000",  "NONE", obs, state, &lt );

         printf( "\nEpoch               : %22.10f\n", et     );
         printf( "   x-position   (km): %22.10f\n", state[0] );
         printf( "   y-position   (km): %22.10f\n", state[1] );
         printf( "   z-position   (km): %22.10f\n", state[2] );
         printf( "   x-velocity (km/s): %22.10f\n", state[3] );
         printf( "   y-velocity (km/s): %22.10f\n", state[4] );
         printf( "   z-velocity (km/s): %22.10f\n", state[5] );

         return ( 0 );

      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, using the time string "2017-07-14T19:46:00" as epoch,
      "MARS" as target and "EARTH" as observer, the output was:


      Enter UTC epoch             > 2017-07-14T19:46:00
      Enter observer name         > MARS
      Enter target name           > EARTH

      Epoch               :   553333628.1837273836
         x-position   (km):   173881563.8231496215
         y-position   (km):  -322898311.5398598909
         z-position   (km):  -147992421.0068917871
         x-velocity (km/s):          47.4619819770
         y-velocity (km/s):          19.0770886182
         z-velocity (km/s):           7.9424268278


   2) The following code example illustrates the acquisition of input
      values using prompt_c as a non-void function.

      Use the meta-kernel from the first example.


      Example code begins here.


      /.
         Program prompt_ex2
      ./
      #include <stdlib.h>
      #include <stdio.h>
      #include "SpiceUsr.h"


      int main()
      {
         /.
         Local parameters
         ./
         #define   STRLEN    37

         /.
         Local variables.
         ./
         SpiceChar    * utc;
         SpiceChar    * obs;
         SpiceChar    * targ;

         SpiceDouble    et;
         SpiceDouble    lt;
         SpiceDouble    state [6];

         /.
         Load kernels.
         ./
         furnsh_c ( "prompt_ex1.tm" );

         /.
         Allocate memory to each of the input variables.
         ./
         utc   = ( SpiceChar * ) malloc (STRLEN);
         obs   = ( SpiceChar * ) malloc (STRLEN);
         targ  = ( SpiceChar * ) malloc (STRLEN);

         /.
         Prompt for the required inputs
         ./
         utc  = prompt_c ( "Enter UTC epoch        > ", STRLEN, utc  );
         obs  = prompt_c ( "Enter observer name    > ", STRLEN, obs  );
         targ = prompt_c ( "Enter target name      > ", STRLEN, targ );

         /.
         Convert the UTC request time to ET (seconds past
         J2000, TDB).
         ./
         str2et_c ( utc, &et );

         /.
         Look up the state vector at the requested `et'
         ./
         spkezr_c ( targ, et, "J2000",  "NONE", obs,  state,  &lt );

         printf( "\nEpoch               : %22.10f\n", et     );
         printf( "   x-position   (km): %22.10f\n", state[0] );
         printf( "   y-position   (km): %22.10f\n", state[1] );
         printf( "   z-position   (km): %22.10f\n", state[2] );
         printf( "   x-velocity (km/s): %22.10f\n", state[3] );
         printf( "   y-velocity (km/s): %22.10f\n", state[4] );
         printf( "   z-velocity (km/s): %22.10f\n", state[5] );

         return ( 0 );

      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, using the time string "2017-07-14T19:46:00" as epoch,
      "MARS" as target and "EARTH" as observer, the output was:


      Enter UTC epoch        > 2017-07-14T19:46:00
      Enter observer name    > MARS
      Enter target name      > EARTH

      Epoch               :   553333628.1837273836
         x-position   (km):   173881563.8231496215
         y-position   (km):  -322898311.5398598909
         z-position   (km):  -147992421.0068917871
         x-velocity (km/s):          47.4619819770
         y-velocity (km/s):          19.0770886182
         z-velocity (km/s):           7.9424268278


-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   E.D. Wright         (JPL)

-Version

   -CSPICE Version 1.1.0, 13-AUG-2021 (JDR)

       Changed the input argument names "prmptStr" and "lenout" to
       "dspmsg" and "buflen" for consistency with other routines.

       Updated -Exceptions section to include missing exceptions.

       Edited the header to comply with NAIF standard. Converted the
       existing code fragments into complete examples and added required
       meta-kernel and inputs to produce the presented solution.

   -CSPICE Version 1.0.0, 25-JUN-1999 (EDW) (NJB)

-Index_Entries

   Prompt for keyboard input
   Prompt for input with a user supplied message

-&
*/

{ /* Begin prompt_c */

   /*
   Local variables
   */
   SpiceChar               c;
   SpiceInt                i;


   /*
   Participate in error tracing.
   */
   chkin_c ( "prompt_c" );


   /*
   Make sure the output string has at least enough room for one output
   character and a null terminator.  Also check for a null pointer.
   */
   CHKOSTR_VAL ( CHK_STANDARD, "prompt_c", buffer, buflen, NULLCPTR );


   /*
   Initialize i to zero.
   */
   i = 0;


   /*
   Display the prompt string.
   */
   printf ( "%s", dspmsg );


   /*
   Get input from stdin, check for an end of line terminator.
   The loop continues until the terminator is found.
   */

   c = getchar();

   while ( ( c !=  (char)'\n') )
      {

      /*
      We have room for buflen characters, the last of which will
      be a null terminator.  Slurp only (buflen - 1) characters
      from the input into buffer.  Ignore anything afterwards.
      */
      if ( i < (buflen - 1 ) )
         {

         /*
         Read in no more than buflen - 1 characters.
         */
         buffer[i] = c;
         i++;

         }

         /*
         Get the next character from the input line.
         */
         c  = getchar();

      }


   /*
   Null terminate the current buffer.  The counter i points to the
   first free location in the buffer.
   */
   buffer[i] = NULLCHAR;


   /*
   Done.  Checkout.
   */
   chkout_c ( "prompt_c");


   /*
   Return the buffer so the user may elect to use the function call
   capability.
   */

   return buffer;


} /* End prompt_c */
