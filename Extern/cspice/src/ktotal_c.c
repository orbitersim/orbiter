/*

-Procedure ktotal_c ( Kernel Totals )

-Abstract

   Return the number of kernels of a specified type that are
   currently loaded via the furnsh_c interface.

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

   KERNEL

-Keywords

   KERNEL

*/

  #include "SpiceUsr.h"
  #include "SpiceZfc.h"
  #include "SpiceZmc.h"


   void ktotal_c ( ConstSpiceChar   * kind,
                   SpiceInt         * count )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   kind       I   A list of kinds of kernels to count.
   count      O   The number of kernels of type `kind'.

-Detailed_Input

   kind        is a list of types of kernels to count when computing
               loaded kernels. `kind' should consist of a list of words of
               kernels to examine. Recognized types are

                  SPK  --- All SPK files are counted in the total.
                  CK   --- All CK files are counted in the total.
                  PCK  --- All binary PCK files are counted in the
                           total.
                  DSK  --- All DSK files are counted in the total.
                  EK   --- All EK files are counted in the total.
                  TEXT --- All text kernels that are not meta-text
                           kernels are included in the total.
                  META --- All meta-text kernels are counted in the
                           total.
                  ALL  --- Every type of kernel is counted in the
                           total.

               `kind' is case insensitive. If a word appears in `kind'
               that is not one of those listed above, it is ignored.

               When `kind' consists of multiple words, the words must
               be separated by blanks. Examples of valid lists are the
               strings

                  "SPK CK TEXT"
                  "SPK CK text"
                  "PCK DSK"
                  "CK"
                  "ALL"

               See the -Examples section for illustrations of the
               use of `kind'.

-Detailed_Output

   count       is the number of kernels loaded through furnsh_c that
               belong to the list specified by `kind'.

-Parameters

   None.

-Exceptions

   1)  If a word on the list specified by `kind' is not recognized,
       it is ignored.

   2)  If `kind' is blank, or none of the words in `kind' is on the
       list specified above, `count' will be returned as zero.

   3)  If the `kind' input string pointer is null, the error
       SPICE(NULLPOINTER) is signaled.

   4)  If the `kind' input string has zero length, the error
       SPICE(EMPTYSTRING) is signaled.

-Files

   None.

-Particulars

   ktotal_c allows you to easily determine the number of kernels
   loaded via the interface furnsh_c that are of a type of interest.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Load a meta-kernel with a PCK, an LSK and an SPK, and
      separately, a text kernel and a binary PCK. Show the
      total number of kernels and meta-kernels loaded. Determine the
      number of text kernels loaded, and the number of binary
      kernels.

      Unload all kernels and clear the kernel pool using
      kclear_c, and check that no kernels are loaded.


      Use the meta-kernel shown below to load the required SPICE
      kernels.


         KPL/MK

         File name: ktotal_ex1.tm

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
            ---------                     --------
            de421.bsp                     Planetary ephemeris
            pck00008.tpc                  Planet orientation and
                                          radii
            naif0009.tls                  Leapseconds


         \begindata

            KERNELS_TO_LOAD = ( 'de421.bsp',
                                'pck00008.tpc',
                                'naif0009.tls'  )

         \begintext

         End of meta-kernel


      Use the PCK kernel below as the binary PCK required for the
      example.

         earth_latest_high_prec.bpc


      Use the FK kernel below as the text kernel required for the
      example.

         RSSD0002.TF


      Example code begins here.


      /.
         Program ktotal_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      SpiceInt        count;

      int main()
      {
         /.
         Load several kernel files.
         ./
         furnsh_c( "ktotal_ex1.tm" );
         furnsh_c( "RSSD0002.TF" );
         furnsh_c( "earth_latest_high_prec.bpc" );

         /.
         Count the number of loaded kernel files.
         ./
         ktotal_c ( "ALL", &count );

         printf ( "The total number of kernels after "
                  "final furnsh_c: %d\n", count );

         /.
         Count the number of meta-kernels.
         ./
         ktotal_c ( "META", &count );

         printf ( "The total number of meta-kernels  "
                  "              : %d\n", count );

         /.
         Count the number of text kernels.
         ./
         ktotal_c ( "TEXT", &count );

         printf ( "The total number of text kernels  "
                  "              : %d\n", count );

         /.
         Count the number of binary kernels. These kernels
         are of type CK, DSK, EK, PCK or SPK.
         ./
         ktotal_c ( "CK DSK EK PCK SPK", &count );

         printf ( "The total number of binary kernels"
                  "              : %d\n", count );

         /.
         Clear the KEEPER system, retrieve the number of loaded
         after the clear.
         ./
         kclear_c ( );

         ktotal_c ( "ALL", &count );

         printf ( "The total number of kernels after "
                  "kclear_c      : %d\n", count );

      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      The total number of kernels after final furnsh_c: 6
      The total number of meta-kernels                : 1
      The total number of text kernels                : 3
      The total number of binary kernels              : 2
      The total number of kernels after kclear_c      : 0


-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   W.L. Taber          (JPL)

-Version

   -CSPICE Version 1.0.2, 01-NOV-2021 (JDR) (NJB)

       Edited the header to comply with NAIF standard. Added
       complete example.

       Updated -Detailed_Input description of input argument "kind" to
       illustrate use of multi-word lists. Added KERNEL to the list of
       required readings.

   -CSPICE Version 1.0.1, 20-JAN-2016 (NJB) (WLT)

       Header was updated to reflect support for use
       of DSKs. Header sections were reordered.

   -CSPICE Version 1.0.0, 01-SEP-1999 (NJB) (WLT)

-Index_Entries

   Number of loaded kernels of a given type

-&
*/

{ /* Begin ktotal_c */


   /*
   Use discovery check-in.

   Check the input file kind to make sure the pointer is non-null
   and the string length is non-zero.
   */
   CHKFSTR ( CHK_DISCOVER, "ktotal_c", kind );


   ktotal_ (  ( char    * ) kind,
              ( integer * ) count,
              ( ftnlen    ) strlen(kind)  );


} /* End ktotal_c */
