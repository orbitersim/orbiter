/*

-Procedure sce2s_c ( ET to SCLK string )

-Abstract

   Convert an epoch specified as ephemeris seconds past J2000 (ET) to a
   character string representation of a spacecraft clock value (SCLK).

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
   TIME

-Keywords

   CONVERSION
   TIME

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZst.h"
   #include "SpiceZmc.h"

   void sce2s_c ( SpiceInt        sc,
                  SpiceDouble     et,
                  SpiceInt        scllen,
                  SpiceChar     * sclkch  )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   sc         I   NAIF spacecraft clock ID code.
   et         I   Ephemeris time, specified as seconds past J2000.
   scllen     I   Maximum length of output string `sclkch'.
   sclkch     O   An SCLK string.

-Detailed_Input

   sc          is a NAIF ID code for a spacecraft clock whose
               reading at the epoch specified by `et' is desired.

   et          is an epoch, specified as ephemeris seconds past
               J2000 TDB.

   scllen      is the maximum number of characters that can be
               accommodated in the output string. This count
               includes room for the terminating null character. For
               example, if the maximum allowed length of the output
               string, including the terminating null, is 25
               characters, then `scllen' should be set to 25.

               In order to choose an appropriate value of `scllen',
               you can examine an SCLK kernel for the clock specified
               by `sc'. The format of string representations of
               the clock's values is specified by kernel variables
               associated with the clock. See -Examples below for
               further information.

-Detailed_Output

   sclkch      is a character string representation of the
               spacecraft clock value that corresponds to `et', for
               the spacecraft clock specified by the input
               argument `sc'. `sclkch' is an absolute spacecraft
               clock value, so a partition number is included in
               the string. The format of `sclkch' is specified in
               the SCLK kernel for the clock `sc'. A general
               discussion of spacecraft clock string formats is
               available in the SCLK Required Reading.

               In order to choose an appropriate length for
               `sclkch', you can examine an SCLK kernel for the
               clock specified by `sc'. The format of string
               representations of the clock's values is specified
               by kernel variables associated with the clock. See
               -Examples below for further information.

-Parameters

   None.

-Exceptions

   1)  If an SCLK kernel has not been loaded, does not contain all of
       the required data, or contains invalid data, an error is
       signaled by a routine in the call tree of this routine. The
       output argument `sclkch' will not be modified. This routine
       assumes that an SCLK kernel appropriate to the spacecraft
       clock identified by the input argument `sc' has been loaded.

   2)  If a leapseconds kernel is required for conversion between
       SCLK and `et' but is not loaded, an error is signaled by a
       routine in the call tree of this routine. The output argument
       `sclkch' will not be modified. When using an SCLK kernel that
       maps SCLK to a time system other than `et' (also called
       barycentric dynamical time---`TDB'), it is necessary to have a
       leapseconds kernel loaded at the time this routine is called.

       The time system to which an SCLK kernel maps SCLK epochs is
       indicated by the variable SCLK_TIME_SYSTEM_nn in the kernel,
       where nn is the negative of the NAIF integer code for the
       spacecraft. The time system used in a kernel is TDB if and
       only if the variable is assigned the value 1.

   3)  If the input `et' value is not representable in the spacecraft
       clock string format for the spacecraft clock identified by `sc',
       an error is signaled by a routine in the call tree of this
       routine. The output argument `sclkch' will not be modified.

   4)  If the output argument `sclkch' is too short to contain the
       output spacecraft clock string produced by this routine, an
       error is signaled by a routine in the call tree of this
       routine. The output argument `sclkch' may contain a portion of
       the truncated string.

   5)  If the `sclkch' output string pointer is null, the error
       SPICE(NULLPOINTER) is signaled.

   6)  If the `sclkch' output string has length less than two
       characters, the error SPICE(STRINGTOOSHORT) is signaled, since
       the output string is too short to contain one character of
       output data plus a null terminator.

-Files

   An SCLK kernel, appropriate to the spacecraft clock identified
   by `sc', must be loaded at the time this routine is called.

   If the SCLK kernel used with this routine does not map SCLK
   directly to barycentric dynamical time, a leapseconds kernel
   must be loaded at the time this routine is called.

-Particulars

   This routine is provided as a convenience; it is simply shorthand
   for the code fragment

      sce2t_c  ( sc, et,    &sclkdp         );
      scdecd_c ( sc, sclkdp, scllen, sclkch );

   See the SCLK Required Reading for a list of the entire set of
   SCLK conversion routines.

-Examples

   The numerical results shown for these examples may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Determine the length of Galileo spacecraft clock strings.

      Examine a Galileo SCLK kernel. There you'll find the
      kernel variable assignments

         SCLK01_MODULI_77          = ( 16777215 91 10 8 )
         SCLK01_OFFSETS_77         = (        0  0  0 0 )

      Each field of the clock string contains values ranging
      from the offset value to M-1, where M is the corresponding
      modulus. So the Galileo clock fields have maximum values

         16777214 90 9 7

      representing the partition number by the symbol "pp" and
      the field delimiter character by the symbol "D", we see
      that the GLL SCLK format is

         pp/xxxxxxxxDxxDxDx

      This string has length 18 characters. Accounting for the
      terminating null character, the value of `scllen' should
      be set to at least 19.

      Note: the delimiter character is determined by the integer
      code assignment

         SCLK01_OUTPUT_DELIM_77    = (                2 )

      The SCLK Required Reading indicates that 2 is the SCLK kernel
      code for the colon character.


   2) Find the CASSINI SCLK value corresponding to the ET value

         140223701.608.

      Depending on the SCLK kernel used, it may be necessary to
      load a leapseconds kernel file, as it is the case of CASSINI
      SCLK.

      Use the meta-kernel shown below to load the required SPICE
      kernels.


         KPL/MK

         File name: sce2s_ex2.tm

         This meta-kernel is intended to support operation of SPICE
         example programs. The kernels shown here should not be
         assumed to contain adequate or correct versions of data
         required by SPICE-based user applications.

         In order for an application to use this meta-kernel, the
         kernels referenced here must be present in the user's
         current working directory.

         The names and contents of the kernels referenced
         by this meta-kernel are as follows:

            File name                     Contents
            ------------                  ------------
            naif0012.tls                  Leapseconds
            cas00071.tsc                  CASSINI SCLK


         \begindata

            KERNELS_TO_LOAD = ( 'naif0012.tls',
                                'cas00071.tsc' )

         \begintext

         End of meta-kernel


      Example code begins here.


      /.
         Program sce2s_ex2
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main()
      {
         #define SCLKLEN         30

         /.
         The spacecraft ID code for the CASSINI orbiter
         is -82.  This is the code for the CASSINI spacecraft
         clock as well.
         ./
         #define CASSINI         -82

         SpiceChar               sclkch[SCLKLEN];
         SpiceDouble             et;


         /.
         Start out by loading the kernels.
         ./
         furnsh_c ( "sce2s_ex2.tm" );

         et = 140223701.608;

         sce2s_c ( CASSINI, et, SCLKLEN, sclkch );

         printf ( "ET            = %25.17e\n"
                  "CASSINI SCLK  =  %s\n",
                  et,
                  sclkch                    );

         return ( 0 );

      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      ET            =   1.40223701608000010e+08
      CASSINI SCLK  =  1/1465644305.040


   3) Convert the UTC time

         June 11 2004 11:00:37

      to the character string representation of the CASSINI
      spacecraft clock value.

      Use the meta-kernel from the previous example to load the
      required SPICE kernels.


      Example code begins here.


      /.
         Program sce2s_ex3
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main()
      {
         /.
         The spacecraft ID code for the CASSINI spacecraft
         is -82.  This is the code for the CASSINI spacecraft
         clock as well.
         ./
         #define CASSINI        -82
         #define SCLKLEN         30

         SpiceChar               sclkch[SCLKLEN];
         SpiceDouble             et;

         /.
         Load SCLK and leapseconds kernels.
         ./
         furnsh_c ( "sce2s_ex2.tm" );

         /.
         Find the CASSINI SCLK string corresponding to the
         specified UTC time.
         ./
         str2et_c ( "June 11 2004 11:00:37", &et              );
         sce2s_c  ( CASSINI  ,  et,          SCLKLEN,  sclkch );

         printf ( "ET           = %25.17e\n"
                  "CASSINI SCLK =  %s\n",
                  et,
                  sclkch                    );

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      ET           =   1.40223701184634447e+08
      CASSINI SCLK =  1/1465644304.188


-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   C.H. Acton          (JPL)
   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   B.V. Semenov        (JPL)

-Version

   -CSPICE Version 1.2.0, 10-AUG-2021 (JDR)

       Changed the input argument "lenout" to "scllen" for consistency
       with other routines.

       Edited the header to comply with NAIF standard. Added entries #5 and #6
       to -Exceptions section. Added meta-kernel to example #2 with updated
       kernel set to use PDS archived CASSINI data. Added reference to
       meta-kernel of example #2 in code example #3.

   -CSPICE Version 1.1.2, 21-JAN-2014 (BVS)

       Fixed an incomplete sentence in the item 2) in the header
       section -Files.

   -CSPICE Version 1.1.1, 29-JUL-2003 (NJB) (CHA)

       Various header changes were made to improve clarity and
       more fully explain the routine's functionality.

   -CSPICE Version 1.1.0, 09-FEB-1998 (NJB)

       Re-implemented routine without dynamically allocated, temporary
       strings. Updated the -Exceptions header section.

   -CSPICE Version 1.0.0, 25-OCT-1997 (NJB)

       Based on SPICELIB Version 1.2.0, 10-APR-1992 (NJB) (WLT)

-Index_Entries

   ephemeris time to spacecraft_clock string

-&
*/

{ /* Begin sce2s_c */


   /*
   Participate in error tracing.
   */
   chkin_c ( "sce2s_c");


   /*
   Make sure the output sclkch has at least enough room for one output
   character and a null terminator.  Also check for a null pointer.
   */
   CHKOSTR ( CHK_STANDARD, "sce2s_c", sclkch, scllen );


   /*
   Do the conversion.
   */
   sce2s_ ( ( integer    * ) &sc,
            ( doublereal * ) &et,
            ( char       * ) sclkch,
            ( ftnlen       ) scllen-1 );

   /*
   Convert sclkch to a null-terminated C string.
   */
   F2C_ConvertStr ( scllen, sclkch );


   chkout_c ( "sce2s_c");

} /* End sce2s_c */
