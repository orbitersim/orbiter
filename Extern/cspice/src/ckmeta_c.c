/*

-Procedure ckmeta_c ( CK ID to associated SCLK )

-Abstract

   Return (depending upon the user's request) the ID code of either
   the spacecraft or spacecraft clock associated with a C-Kernel ID
   code.

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

   CK
   FRAMES

-Keywords

   UTILITY

*/
   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZmc.h"

   void ckmeta_c ( SpiceInt            ckid,
                   ConstSpiceChar    * meta,
                   SpiceInt          * idcode )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   ckid       I   The ID code for some C kernel object.
   meta       I   The kind of meta data requested "SPK" or "SCLK"
   idcode     O   The requested SCLK or spacecraft ID code.

-Detailed_Input

   ckid        is the ID code for some object whose attitude
               and possibly angular velocity are stored in
               some C-kernel.

   meta        is a character string that indicates which piece
               of meta data to fetch. Acceptable values are
               "SCLK" and "SPK". The routine is case insensitive.
               Leading and trailing blanks are insignificant.
               However, blanks between characters are regarded
               as being significant.

-Detailed_Output

   idcode      if `meta' is "SCLK" then the value returned in `idcode'
               is the ID code of the spacecraft clock used for
               converting ET to TICKS and TICKS to ET for the
               C-kernel used to represent the attitude of the
               object with ID code `ckid'.

               If `meta' is "SPK" then the value returned in `idcode' is the
               ID code of the spacecraft on which the platform indicated
               by `ckid' is mounted.

-Parameters

   None.

-Exceptions

   1)  If the variable `meta' is not recognized to be one of the
       inputs "SPK" or "SCLK", the error SPICE(UNKNOWNCKMETA)
       is signaled by a routine in the call tree of this routine.

   2)  If `ckid' is greater than -1000, the associated SCLK and SPK
       IDs must be in the kernel pool. If they are not present
       a value of zero is returned for the requested item. Zero
       is never the valid ID of a spacecraft clock.

   3)  If the `meta' input string pointer is null, the error
       SPICE(NULLPOINTER) is signaled.

   4)  If the `meta' input string has zero length, the error
       SPICE(EMPTYSTRING) is signaled.

-Files

   None.

-Particulars

   This is a utility routine for mapping C-kernels to associated
   spacecraft clocks.

   An association of an SCLK ID and spacecraft ID with a CK frame
   class ID may be made by placing in a text kernel the kernel
   variable assignments

      CK_<ck_frame_class_ID_code>_SCLK = <ID code of SCLK>
      CK_<ck_frame_class_ID_code>_SPK  = <SPK ID code>

   See the Frames Required Reading section on CK frames.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Suppose you would like to look up the attitude of an object
      in a C-kernel but have ET and seconds as your input time and
      tolerance.

      Use the meta-kernel shown below to load the required SPICE
      kernels.


         KPL/MK

         File name: ckmeta_ex1.tm

         This meta-kernel is intended to support operation of SPICE
         example programs. The kernels shown here should not be
         assumed to contain adequate or correct versions of data
         required by SPICE-based user applications.

         In order for an application to use this meta-kernel, the
         kernels referenced here must be present in the user's
         current working directory.

         The names and contents of the kernels referenced
         by this meta-kernel are as follows:

            File name              Contents
            --------------------   -----------------------
            cas00071.tsc           CASSINI SCLK
            naif0012.tls           Leapseconds
            04153_04182ca_ISS.bc   CASSINI image navigated
                                   spacecraft CK


         \begindata

            KERNELS_TO_LOAD = ( 'naif0012.tls',
                                'cas00071.tsc'
                                '04153_04182ca_ISS.bc' )

         \begintext

         End of meta-kernel


      Example code begins here.


      /.
         Program ckmeta_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Local parameters.

         -- The code for CASSINI spacecraft reference frame is
            -82000.

         -- The reference frame we want is J2000.
         ./
         #define REF          "J2000"
         #define CKID         -82000

         /.
         Local variables.
         ./
         SpiceDouble          av     [3];
         SpiceDouble          clkout;
         SpiceDouble          cmat   [3][3];
         SpiceDouble          etout;
         SpiceDouble          ticks;
         SpiceDouble          tick2;
         SpiceDouble          tol;

         SpiceInt             idcode;

         SpiceBoolean         found;

         /.
         Initial values.
         ./
         SpiceDouble          et = 141162208.034340;
         SpiceDouble          sectol = 0.5;

         /.
         First load the CK, LSK and SCLK files.
         ./
         furnsh_c ( "ckmeta_ex1.tm" );

         /.
         Get the SCLK identifier of the spacecraft clock required
         to convert from `et' to `ticks'.
         ./
         ckmeta_c ( CKID, "SCLK", &idcode );

         /.
         Convert `et' and et+sectol to spacecraft clock ticks.
         ./
         sce2c_c ( idcode, et, &ticks );
         sce2c_c ( idcode, et+sectol, &tick2 );

         /.
         Compute the tolerance in spacecraft clock ticks.
         ./
         tol = tick2 - ticks;

         /.
         Look the attitude up.
         ./
         ckgpav_c ( CKID, ticks, tol, REF, cmat, av, &clkout, &found );

         printf( "Input ET:             %19.6f\n", et );

         if ( found )
         {
            sct2e_c ( idcode, clkout, &etout );
            printf( "Attitude found at ET: %19.6f\n", etout );
         }
         else
         {
            printf( "No attitude found at ET.\n" );
         }

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Input ET:                141162208.034340
      Attitude found at ET:    141162208.034586


-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   J. Diaz del Rio     (ODC Space)

-Version

   -CSPICE Version 1.0.0, 08-JUL-2021 (JDR)

-Index_Entries

   Map C-kernel ID to SCLK and SPK ID

-&
*/

{ /* Begin ckmeta_c */

   /*
   Participate in error tracing.
   */
   chkin_c ( "ckmeta_c" );

   /*
   Check the input string `meta' to make sure the pointer is non-null
   and the string length is non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "ckmeta_c", meta   );

   /*
   Call the f2c'd Fortran routine.
   */
   ckmeta_ (  ( integer    * ) &ckid,
              ( char       * )  meta,
              ( integer    * )  idcode,
              ( ftnlen       )  strlen(meta)  );

   chkout_c ( "ckmeta_c" );

} /* End ckmeta_c */
