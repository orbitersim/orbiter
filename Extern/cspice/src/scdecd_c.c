/*

-Procedure scdecd_c ( Decode spacecraft clock )

-Abstract

   Convert a double precision encoding of spacecraft clock time into
   a character representation.

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

   SCLK

-Keywords

   CONVERSION
   TIME

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZst.h"
   #include "SpiceZmc.h"

   void scdecd_c ( SpiceInt       sc,
                   SpiceDouble    sclkdp,
                   SpiceInt       scllen,
                   SpiceChar    * sclkch  )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   SPICE_SCLK_MXPART
              P   Maximum number of spacecraft clock partitions.
   sc         I   NAIF spacecraft identification code.
   sclkdp     I   Encoded representation of a spacecraft clock count.
   scllen     I   Maximum allowed length of output SCLK string.
   sclkch     O   Character representation of a clock count.

-Detailed_Input

   sc          is the NAIF integer code of the spacecraft whose
               clock's time is being decoded.

   sclkdp      is the double precision encoding of a clock time in
               units of ticks since the spacecraft clock start time.
               This value does reflect partition information.

               An analogy may be drawn between a spacecraft clock
               and a standard wall clock. The number of ticks
               corresponding to the wall clock string

                  hh:mm:ss

               would be the number of seconds represented by that
               time.

               For example:

                  Clock string      Number of ticks
                  ------------      ---------------
                    00:00:10              10
                    00:01:00              60
                    00:10:00             600
                    01:00:00            3600

               If `sclkdp' contains a fractional part the result
               is the same as if `sclkdp' had been rounded to the
               nearest whole number.

   scllen      is the maximum number of characters that can be
               accommodated in the output string. This count
               includes room for the terminating null character.
               For example, if the maximum allowed length of the
               output string, including the terminating null, is 25
               characters, then `scllen' should be set to 25.

-Detailed_Output

   sclkch      is the character representation of the clock count.
               The exact form that `sclkch' takes depends on the
               spacecraft.

               Nevertheless, `sclkch' will have the following general
               format:

                  "pp/sclk_string"

               "pp" is an integer greater than or equal to one and
               represents a "partition number".

               Each mission is divided into some number of partitions.
               A new partition starts when the spacecraft clock
               resets, either to zero, or to some other
               value. Thus, the first partition for any mission
               starts with launch, and ends with the first clock
               reset. The second partition starts immediately when
               the first stopped, and so on.

               In order to be completely unambiguous about a
               particular time, you need to specify a partition number
               along with the standard clock string.

               Information about when partitions occur for different
               missions is contained in a spacecraft clock kernel
               file which needs to be loaded into the kernel pool
               before calling scdecd_c.

               The routine scpart_c may be used to read the partition
               start and stop times, in encoded units of ticks, from
               the kernel file.

               Since the end time of one partition is coincident with
               the begin time of the next, two different time strings
               with different partition numbers can encode into the
               same value.

               For example, if partition 1 ends at time t1, and
               partition 2 starts at time t2, then

                  "1/t1" and "2/t2"

               will be encoded into the same value, say X. scdecd_c
               always decodes such values into the latter of the
               two partitions. In this example,

                 scdecd_c ( x, sc, scllen, clkstr )

               will result in

                  clkstr = "2/t2".

               "sclk_string" is a spacecraft specific clock string,
               typically consisting of a number of components
               separated by delimiters.

               Using Galileo as an example, the full format is

                  wwwwwwww:xx:y:z

               where z is a mod-8 counter (values 0-7) which
               increments approximately once every 8 1/3 ms., y is a
               mod-10 counter (values 0-9) which increments once
               every time z turns over, i.e., approximately once every
               66 2/3 ms., xx is a mod-91 (values 0-90) counter
               which increments once every time y turns over, i.e.,
               once every 2/3 seconds. wwwwwwww is the Real-Time Image
               Count (RIM), which increments once every time xx turns
               over, i.e., once every 60 2/3 seconds. The roll-over
               expression for the RIM is 16777215, which corresponds
               to approximately 32 years.

               wwwwwwww, xx, y, and z are referred to interchangeably
               as the fields or components of the spacecraft clock.
               SCLK components may be separated by any of these five
               characters: " "  ":"  ","  "-"  "."
               The delimiter used is determined by a kernel pool
               variable and can be adjusted by the user.

               Some spacecraft clock components have offset, or
               starting, values different from zero. For example,
               with an offset value of 1, a mod 20 counter would
               cycle from 1 to 20 instead of from 0 to 19.

               See the SCLK required reading for a detailed
               description of the Voyager and Mars Observer clock
               formats.

-Parameters

   SPICE_SCLK_MXPART

               is the maximum number of spacecraft clock partitions
               expected in the kernel file for any one spacecraft.
               See the header file SpiceSCLK.h for this parameter's
               value.

-Exceptions

   1)  If kernel variables required by this routine are unavailable,
       an error is signaled by a routine in the call tree of this
       routine. `sclkch' will be returned as a blank string in this
       case.

   2)  If the number of partitions in the kernel file for spacecraft `sc'
       exceeds the parameter SPICE_SCLK_MXPART, the error
       SPICE(TOOMANYPARTS) is signaled by a routine in the call tree of this
       routine. `sclkch' will be returned as a blank string in this case.

   3)  If the encoded value does not fall in the boundaries of the
       mission, the error SPICE(VALUEOUTOFRANGE) is signaled by a
       routine in the call tree of this routine. `sclkch' will be
       returned as a blank string in this case.

   4)  If the declared length of `sclkch' is not large enough to
       contain the output clock string, the error
       SPICE(SCLKTRUNCATED) is signaled by either this routine or a
       routine in the call tree of this routine. On output `sclkch'
       will contain a portion of the truncated clock string.

   5)  If the `sclkch' output string pointer is null, the error
       SPICE(NULLPOINTER) is signaled.

   6)  If the `sclkch' output string has length less than two
       characters, the error SPICE(STRINGTOOSHORT) is signaled, since
       the output string is too short to contain one character of
       output data plus a null terminator.

-Files

   A kernel file containing spacecraft clock partition information
   for the desired spacecraft must be loaded, using the routine
   furnsh_c, before calling this routine.

-Particulars

   In general, it is difficult to compare spacecraft clock counts
   numerically since there are too many clock components for a
   single comparison. The routine scencd_c provides a method of
   assigning a single double precision number to a spacecraft's
   clock count, given one of its character representations.

   This routine performs the inverse operation to scencd_c, converting
   an encoded double precision number to character format.

   To convert the number of ticks since the start of the mission to
   a clock format character string, scdecd_c:

      1) Determines the spacecraft clock partition that TICKS falls
         in.

      2) Subtracts off the number of ticks occurring in previous
         partitions, to get the number of ticks since the beginning
         of the current partition.

      3) Converts the resulting ticks to clock format and forms the
         string

            "partition_number/clock_string"

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as input,
   the compiler and supporting libraries, and the machine specific
   arithmetic implementation.

   1) Double precision encodings of spacecraft clock counts are used
      to tag pointing data in the C-kernel.

      In the following example, pointing for a sequence of images
      from the CASSINI Imaging Science Subsystem (ISS) is requested
      from the C-kernel using an array of character spacecraft clock
      counts as input. The clock counts attached to the output are
      then decoded to character and compared with the input strings.

      Use the CK kernel below to load the CASSINI image navigated
      spacecraft pointing and orientation data.

         04153_04182ca_ISS.bc


      Use the SCLK kernel below to load the CASSINI spacecraft clock
      time correlation data required for the conversion between
      spacecraft clock string representation and double precision
      encoding of spacecraft clock counts.

         cas00071.tsc


      Example code begins here.


      /.
         Program scdecd_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main()
      {
         /.
         Local parameters.

         The instrument we want pointing for is the CASSINI
         spacecraft. The reference frame we want is
         J2000. The spacecraft is CASSINI.
         ./
         #define  SC         -82
         #define  INST       -82000
         #define  REF        "J2000"
         #define  CK         "04153_04182ca_ISS.bc"
         #define  SCLK       "cas00071.tsc"
         #define  NPICS      4
         #define  CLKTOL     "1.0"
         #define  MAXLEN     30

         /.
         Local variables.
         ./
         SpiceBoolean       found;

         SpiceChar          sclkout[25];

         SpiceDouble        tol;
         SpiceDouble        timein;
         SpiceDouble        timeout;
         SpiceDouble        cmat   [3][3];

         SpiceInt           i;

         /.
         Set the input SCLK strings.
         ./
         SpiceChar          sclkin [4][25] = { {"1/1465644279.0"},
                                               {"1/1465644281.0"},
                                               {"1/1465644351.0"},
                                               {"1/1465644361.0"} };

         /.
         Load the appropriate files. We need

            1. CK file containing pointing data.
            2. Spacecraft clock kernel file.
         ./
         furnsh_c ( CK   );
         furnsh_c ( SCLK );

         /.
         Convert the tolerance string to ticks.
         ./
         sctiks_c ( SC, CLKTOL, &tol );

         for ( i = 0;  i < NPICS;  i++ )
         {
            scencd_c ( SC,   sclkin[i], &timein );

            ckgp_c   ( INST,  timein,    tol,    REF,
                       cmat,  &timeout,  &found       );

            printf ( "\nInput s/c clock count : %s\n", sclkin[i] );
            if ( found )
            {
               scdecd_c ( SC, timeout, MAXLEN, sclkout );

               printf ( "Output s/c clock count: %s\n"
                        "Output C-Matrix:\n"
                        "   %19.15f   %19.15f   %19.15f\n"
                        "   %19.15f   %19.15f   %19.15f\n"
                        "   %19.15f   %19.15f   %19.15f\n",
                        sclkout,
                        cmat[0][0],  cmat[0][1],  cmat[0][2],
                        cmat[1][0],  cmat[1][1],  cmat[1][2],
                        cmat[2][0],  cmat[2][1],  cmat[2][2] );
            }
            else
            {
               printf ( "No pointing found.\n" );
            }
         }

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Input s/c clock count : 1/1465644279.0
      No pointing found.

      Input s/c clock count : 1/1465644281.0
      Output s/c clock count: 1/1465644281.171
      Output C-Matrix:
          -0.335351455948710     0.864374440205611     0.374694846658341
          -0.937887426812980    -0.343851965210223    -0.046184419961653
           0.088918927227039    -0.366909598048763     0.925997176691424

      Input s/c clock count : 1/1465644351.0
      Output s/c clock count: 1/1465644351.071
      Output C-Matrix:
          -0.335380929397586     0.864363638262230     0.374693385378623
          -0.937874292008090    -0.343889838107825    -0.046169163264003
           0.088946301703530    -0.366899550417080     0.925998528787713

      Input s/c clock count : 1/1465644361.0
      No pointing found.


-Restrictions

   1)  Assumes that an SCLK kernel file appropriate for the clock
       designated by `sc' is loaded in the kernel pool at the time
       this routine is called.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   E.D. Wright         (JPL)

-Version

   -CSPICE Version 1.3.0, 05-AUG-2021 (JDR)

       Changed the input argument "lenout" to "scllen" for consistency
       with other routines.

       Edited the header to comply with NAIF standard. Reformatted
       example's output and updated kernel set to work with PDS
       archived CASSINI data.

   -CSPICE Version 1.2.0, 11-FEB-2008 (NJB)

       Definition of constant macro MXPART was deleted.
       Documentation was updated to reflect current
       MXPART value of 9999.

   -CSPICE Version 1.1.2, 14-AUG-2006 (EDW)

       Replace mention of ldpool_c with furnsh_c.

   -CSPICE Version 1.1.1, 26-MAR-2003 (NJB)

       Fixed description of exception (6):  replaced "scllen-1"
       with "scllen." Corrected spelling of "signaled."

   -CSPICE Version 1.1.0, 09-FEB-1998 (NJB)

       Re-implemented routine without dynamically allocated, temporary
       strings. Added output string length and pointer checks.

   -CSPICE Version 1.0.0, 25-OCT-1997 (NJB)

-Index_Entries

   decode spacecraft_clock

-&
*/

{ /* Begin scdecd_c */


   /*
   Participate in error handling
   */
   chkin_c ( "scdecd_c");

   /*
   Make sure the output string has at least enough room for one output
   character and a null terminator.  Also check for a null pointer.
   */
   CHKOSTR ( CHK_STANDARD, "scdecd_c", sclkch, scllen );


   /*
   Decode the encoded SCLK value.
   */
   scdecd_ (  ( integer    * ) &sc,
              ( doublereal * ) &sclkdp,
              ( char       * ) sclkch,
              ( ftnlen       ) scllen-1  );

   /*
   Convert the Fortran string to a C string by placing a null
   after the last non-blank character.  This operation is valid
   whether or not the SPICELIB routine signaled an error.
   */
   F2C_ConvertStr ( scllen, sclkch );


   chkout_c ( "scdecd_c");


} /* End scdecd_c */
