/*

-Procedure scencd_c ( Encode spacecraft clock )

-Abstract

   Encode a character representation of spacecraft clock time into a
   double precision number.

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
   #include "SpiceZmc.h"


   void scencd_c ( SpiceInt           sc,
                   ConstSpiceChar   * sclkch,
                   SpiceDouble      * sclkdp )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   sc         I   NAIF spacecraft identification code.
   sclkch     I   Character representation of a spacecraft clock.
   sclkdp     O   Encoded representation of the clock count.
   SPICE_SCLK_MXPART
              P   Maximum number of spacecraft clock partitions.

-Detailed_Input

   sc          is the standard NAIF ID of the spacecraft whose clock's
               time is being encoded.

   sclkch      is the character representation of some spacecraft's
               clock count.

               `sclkch' will have the following general format:

                  "pp/sclk_string", or just
                     "sclk_string"

               "pp" is an integer greater than or equal to one
               and is called the partition number.

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
               file, which needs to be loaded into the kernel pool,
               using the routine furnsh_c.

               The routine scpart_c is used to read the partition
               start and stop times, in encoded units of SCLK (called
               "ticks" -- see `sclkdp' below) from the kernel file.

               If the partition number is included, it must be
               separated from the rest of the string by a "/".
               Any number of spaces may separate the partition number,
               the "/", and the rest of the clock string.

               If the partition number is omitted, a default partition
               will be assumed. The default partition is the lowest-
               numbered partition that contains the given clock time.
               If the clock time does not fall in any of the
               partition boundaries then an error is signaled.

               "sclk_string" is a spacecraft specific clock string.
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
               SCLK components may be separated by any of these
               five characters: " "  ":"  ","  "-"  "."
               Any number of spaces can separate the components and
               the delimiters. The presence of the RIM component
               is required. Successive components may be omitted, and
               in such cases are assumed to represent zero values.

               Values for the individual components may exceed the
               maximum expected values. For instance, "0:0:0:9" is
               an acceptable Galileo clock string, and will convert
               to the same number of ticks as "0:0:1:1".

               Consecutive delimiters containing no intervening digits
               are treated as if they delimit zero components.

               Trailing zeros should always be included to match the
               length of the counter. For example, a Galileo clock
               count of "25684.90" should not be represented as
               "25684.9".

               Some spacecraft clock components have offset, or
               starting, values different from zero. For example,
               with an offset value of 1, a mod 20 counter would
               cycle from 1 to 20 instead of from 0 to 19.

               See the SCLK required reading for a detailed
               description of the Voyager and Mars Observer clock
               formats.

-Detailed_Output

   sclkdp      is the double precision encoding of `sclkch'.

               The encoding is such that order and proximity will be
               preserved. That is, if t1, t2, and t3 are spacecraft
               clock times, and t1*, t2*, and t3* are their encodings,
               then if

                  t1 < t2 < t3, and

               t2 is closer to t1 than to t3, you will have the result
               that

                  t1* < t2* < t3*, and

               t2* is closer to t1* than to t3*.

               The units of encoded SCLK are "ticks since the start of
               the mission", where a "tick" is defined to be the
               shortest time increment expressible by a particular
               spacecraft's clock.

               Each clock string without partition number represents
               a certain number of ticks, but you need to include
               partition information to determine the relative
               position of that time in relation to the start of the
               mission.

               Since the end time of one partition is coincident
               with the begin time of the next, there are two
               different representations for this instant, and they
               will both yield the same encoding.

               For example, if partition 1 has an end time of t1, and
               partition 2 has a begin time of t2, then if we executed
               the code fragment

                  scencd_c ( "1/t1", sc, &x );
                  scencd_c ( "2/t2", sc, &y );

               we would obtain x = y.

-Parameters

   SPICE_SCLK_MXPART

               is the maximum number of spacecraft clock partitions
               expected in the kernel file for any one spacecraft.
               See the header file SpiceSCLK.h for this parameter's
               value.

-Exceptions

   1)  If the number of partitions in the kernel file for spacecraft
       `sc' exceeds the parameter SPICE_SCLK_MXPART, the error
       SPICE(TOOMANYPARTS) is signaled by a routine in the call tree of
       this routine.

   2)  If any of the extracted clock components cannot be parsed as
       integers, or the string has too many components, or the value
       of one of the components is less than the offset value,
       an error is signaled by a routine in the call tree of this
       routine.

   If a partition number is included in the SCLK string, the
   following exceptions may occur:

   3)  If the partition number cannot be parsed as an integer, the
       error SPICE(BADPARTNUMBER) is signaled by a routine in the
       call tree of this routine.

   4)  If the partition number is not in the range of the number of
       partitions found in the kernel pool, the error
       SPICE(BADPARTNUMBER) is signaled by a routine in the call tree
       of this routine.

   5)  If the clock count does not fall in the boundaries of the
       specified partition, the error SPICE(NOTINPART) is
       signaled by a routine in the call tree of this routine.

   If a partition number is not included in the SCLK string, the
   following exception may occur.

   6)  If the clock count does not fall in the boundaries of any
       partition found in the kernel pool, the error
       SPICE(NOPARTITION) is signaled by a routine in the call tree
       of this routine.

   7)  If the partition delimiter (slash) is first found in the last
       position of SCLKCH, the error SPICE(INVALIDSCLKSTRING) is
       signaled by a routine in the call tree of this routine.

   8)  If the `sclkch' input string pointer is null, the error
       SPICE(NULLPOINTER) is signaled.

   9)  If the `sclkch' input string has zero length, the error
       SPICE(EMPTYSTRING) is signaled.

-Files

   A kernel file containing spacecraft clock partition information
   for the desired spacecraft must be loaded, using the routine
   furnsh_c, before calling this routine.

-Particulars

   In general, it is difficult to compare spacecraft clock counts
   numerically since there are too many clock components for a
   single comparison. This routine provides a method of assigning a
   single double precision number to a spacecraft's clock count,
   given one of its character representations.

   The routine scdecd_c performs the inverse operation of scencd_c,
   converting an encoded double precision number to character format.

   To convert the string to ticks since the start of the mission,
   scencd_c

      1) Converts the non-partition portion of the string to
         ticks, using the routine sctiks_c.

      2) Determines the partition number for the clock time,
         either by getting it directly from the input string, or
         determining the default partition if none was specified.

      3) Includes partition start and stop times, which are also
         measured in ticks, to compute the number of ticks
         from the beginning of the mission to the clock time.

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
         Program scencd_ex1
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
         #define  MAXLEN     25

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

   None.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   E.D. Wright         (JPL)

-Version

   -CSPICE Version 1.2.1, 21-NOV-2021 (JDR)

       Edited the header to comply with NAIF standard. Reformatted
       example's output and updated kernel set to work with PDS
       archived CASSINI data.

   -CSPICE Version 1.2.0, 11-FEB-2008 (NJB)

       Definition of constant macro MXPART was deleted.
       Documentation was updated to reflect current
       MXPART value of 9999.

   -CSPICE Version 1.1.1, 14-AUG-2006 (EDW)

       Replace mention of ldpool_c with furnsh_c.

   -CSPICE Version 1.1.0, 08-FEB-1998 (NJB)

       References to C2F_CreateStr_Sig were removed; code was
       cleaned up accordingly. String checks are now done using
       the macro CHKFSTR.

   -CSPICE Version 1.0.0, 25-OCT-1997 (NJB)

       Based on SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)

-Index_Entries

   encode spacecraft_clock

-&
*/

{ /* Begin scencd_c */


   /*
   Participate in error handling
   */
   chkin_c ( "scencd_c");


   /*
   Check the input string to make sure the pointer
   is non-null and the string length is non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "scencd_c", sclkch );


   /*
   Carry out the encoding.
   */
   scencd_ ( ( integer    * ) &sc,
             ( char       * ) sclkch,
             ( doublereal * ) sclkdp,
             ( ftnlen       ) strlen(sclkch) );


   chkout_c ( "scencd_c");

} /* End scencd_c */
