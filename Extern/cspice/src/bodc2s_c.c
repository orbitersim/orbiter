/*

-Procedure bodc2s_c ( Body ID code to string translation )

-Abstract

   Translate a body ID code to either the corresponding name or if no
   name to ID code mapping exists, the string representation of the
   body ID value.

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

   NAIF_IDS

-Keywords

   BODY
   CONVERSION
   ID
   NAME
   UTILITY

*/
   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZst.h"
   #include "SpiceZmc.h"
   #undef   bodc2s_c

   void bodc2s_c ( SpiceInt        code,
                   SpiceInt        namlen,
                   SpiceChar     * name )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   code       I   Integer ID code to translate to a string.
   namlen     I   Maximum length of output `name'.
   name       O   String corresponding to `code'.

-Detailed_Input

   code        the integer code for a body: planet, satellite,
               barycenter, spacecraft, asteroid, comet, or
               other ephemeris object.

   namlen      is the maximum allowed length of the output `name',
               including the terminating null character. For example,
               if the caller wishes to be able to accept a 32-character
               `name', `namlen' must be set to (at least) 33. The current
               maximum `name' length is 32 characters, so a value of 33
               for `namlen' will suffice.

-Detailed_Output

   name        the string name of the body identified by `code'
               if a mapping between `code' and a body name exists
               within SPICE.

               If `code' has more than one translation, then the
               most recently defined `name' corresponding to `code'
               is returned.  `name' will have the exact format (case
               and blanks) as when the name/code pair was defined.

               If the input value of `code' does not map to a body
               name, `name' returns the string representation
               of `code'.

-Parameters

   MAXL        is the maximum allowable length of a body name. The
               current value of this parameter is 36.

-Exceptions

   1)  If there is any problem with the body name-ID mapping kernel
       variables present in the kernel pool, an error is signaled by
       a routine in the call tree of this routine.

   2)  If the `name' output string pointer is null, the error
       SPICE(NULLPOINTER) is signaled.

   3)  If the `name' output string has length less than two
       characters, the error SPICE(STRINGTOOSHORT) is signaled, since
       the output string is too short to contain one character of
       output data plus a null terminator.

-Files

   Body-name mappings may be defined at run time by loading text
   kernels containing kernel variable assignments of the form

      NAIF_BODY_NAME += ( <name 1>, ... )
      NAIF_BODY_CODE += ( <code 1>, ... )

   See naif_ids.req for details.

-Particulars

   bodc2s_c is one of five related functions,

      bods2c_c      Body string to code
      bodc2s_c      Body code to string
      bodn2c_c      Body name to code
      bodc2n_c      Body code to name
      boddef_c      Body name/code definition

   bods2c_c, bodc2s_c, bodn2c_c, and bodc2n_c perform translations between
   body names and their corresponding integer ID codes which are
   used in SPICE files and routines.

   bods2c_c is a slightly more general version of bodn2c_c: support
   for strings containing ID codes in string format enables a caller
   to identify a body using a string, even when no name is
   associated with that body.

   bodc2s_c is a general version of bodc2n_c; the routine returns either
   the name assigned in the body ID to name mapping or a string
   representation of the `code' value if no mapping exists.

   boddef_c assigns a body name to ID mapping. The mapping has
   priority in name-to-ID and ID-to-name translations.

   Refer to naif_ids.req for the list of name/code associations built
   into SPICE, and for details concerning adding new name/code
   associations at run time by loading text kernels.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Apply the bodc2s_c call to several IDs representing codes
      included in the default SPICE ID-name lists and codes not
      included in the list.

      Example code begins here.


      /.
         Program bodc2s_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"


      int main()
      {
         /.
         Local constants.
         ./
         #define  NAMLEN         33

         /.
         Local variables.
         ./
         SpiceChar               name [NAMLEN];
         SpiceInt                i;

         /.
         Assign an array of body ID codes. Not all the listed codes
         map to a body name.
         ./
         SpiceInt                code[] = { 399, 0, 3, -77,
                                            11, -1, 6000001 };

         /.
         Loop over the `code' array, call bodc2s_c for each
         element of `code'.
         ./
         printf( "Code      Name\n" );
         printf( "-------   -----------------------\n" );

         for (i=0; i<7; i++ )
         {
            bodc2s_c ( code[i], NAMLEN, name );
            printf( "%7d   %s\n", code[i], name );
         }

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Code      Name
      -------   -----------------------
          399   EARTH
            0   SOLAR SYSTEM BARYCENTER
            3   EARTH BARYCENTER
          -77   GALILEO ORBITER
           11   11
           -1   GEOTAIL
      6000001   6000001


      Note that the codes 11 and 6000001 did not map to a name so the
      call returns as `name' the string expression of the codes.

-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   J. Diaz del Rio     (ODC Space)
   E.D. Wright         (JPL)

-Version

   -CSPICE Version 1.1.0, 07-JUL-2021 (JDR)

       Changed the input argument name "lenout" to "namlen" for
       consistency with other routines.

       Edited the header comments and code example to comply with NAIF
       standard.

       Added description of MAXL parameter. Added -Exceptions and
       -Restrictions.

   -CSPICE Version 1.0.1, 12-JUL-2016 (EDW)

       Edit to example program to use "%d" with explicit casts
       to int for printing SpiceInts with printf.

   -CSPICE Version 1.0.0, 24-APR-2010 (EDW)

-Index_Entries

   body ID code to string

-&
*/

{ /* Begin bodc2s_c */


   /*
   Local variables
   */

   /*
   Participate in error tracing.
   */
   chkin_c ( "bodc2s_c");


   /*
   Make sure the output name has at least enough room for one output
   character and a null terminator.  Also check for a null pointer.
   */
   CHKOSTR ( CHK_STANDARD, "bodc2s_c", name, namlen );


   /*
   Call the f2c'd routine.
   */
   (void) bodc2s_( ( integer * )  &code,
                   ( char    * )  name,
                   ( ftnlen    )  namlen-1 );

   /*
   Convert the Fortran string to a C string by placing a null
   after the last non-blank character.  This operation is valid
   whether or not the CSPICE routine signaled an error.
   */
   F2C_ConvertStr ( namlen, name );

   chkout_c ( "bodc2s_c");

} /* End bodc2s_c */
