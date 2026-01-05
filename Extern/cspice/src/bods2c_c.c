/*

-Procedure bods2c_c ( Body string to ID code translation )

-Abstract

   Translate a string containing a body name or ID code to an integer
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
   #include "SpiceZmc.h"

   void bods2c_c ( ConstSpiceChar  * name,
                   SpiceInt        * code,
                   SpiceBoolean    * found )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   name       I   String to be translated to an ID code.
   code       O   Integer ID code corresponding to `name'.
   found      O   Flag indicating whether translation succeeded.

-Detailed_Input

   name        is a string containing the name or ID code of a body or
               object, such as a planet, satellite, comet, asteroid,
               barycenter, DSN station, spacecraft, or instrument.

               If `name' contains the name of a body or object, that
               name must be "known" to the SPICE system, whether
               through hard-coded registration or run-time registration
               in the SPICE kernel pool.

               Case and leading and trailing blanks in `name'
               are not significant. However when a name is made
               up of more than one word, they must be separated by
               at least one blank. That is, all of the following
               strings are equivalent names:

                       "JUPITER BARYCENTER"
                       "Jupiter Barycenter"
                       "JUPITER BARYCENTER   "
                       "JUPITER    BARYCENTER"
                       "   JUPITER BARYCENTER"

               However, "JUPITERBARYCENTER" is not equivalent to
               the names above.

               If NAME is a string representation of an integer,
               for example

                  "399"

               the string will be translated to the equivalent SpiceInt
               datum. The input integer need not be one recognized by
               the SPICE system: the integer need not be a built-in
               NAIF ID code, nor need it be associated with a name via
               run-time registration.

-Detailed_Output

   code        is, if `name' contains the name of a body or object,
               the corresponding NAIF or user-defined integer ID
               code, as determined by the SPICE name-code mapping
               subsystem. If `name' represents an integer, the same
               integer is returned in `code'.

               `code' is assigned a value only if `found' is returned
               as SPICETRUE; otherwise it is returned unchanged.

   found       is SPICETRUE if `name' has a translation or represents an
               integer within the bounds of representable integers
               as defined by the CSPICE routines intmax_c and intmin_c.
               Otherwise, `found' is SPICEFALSE.

-Parameters

   MAXL        is the maximum allowable length of a body name. The
               current value of this parameter is 36.

-Exceptions

   1)  If there is any problem with the body name-ID mapping kernel
       variables present in the kernel pool, an error is signaled by
       a routine in the call tree of this routine.

   2)  Body name strings are upper-cased, their leading and trailing
       blanks removed, and embedded blanks are compressed out, after
       which they get truncated to the maximum body name length MAXL.
       Therefore, two body names that differ only after that maximum
       length are considered equal.

   3)  If the `name' input string pointer is null, the error
       SPICE(NULLPOINTER) is signaled.

   4)  If the `name' input string has zero length, the error
       SPICE(EMPTYSTRING) is signaled.

-Files

   Body-name mappings may be defined at run time by loading text
   kernels containing kernel variable assignments of the form

      NAIF_BODY_NAME += ( <name 1>, ... )
      NAIF_BODY_CODE += ( <code 1>, ... )

   See naif_ids.req for details.

-Particulars

   bods2c_c is one of five related subroutines,

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
   representation of the CODE value if no mapping exists.

   boddef_c assigns a body name to ID mapping. The mapping has priority
   in name-to-ID and ID-to-name translations.

   Refer to NAIF_IDs for the list of name/code associations built into
   SPICE, and for details concerning adding new name/code
   associations at run time by loading text kernels.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Apply the bods2c_c call to several body names to retrieve
      their associated NAIF IDs included in the default SPICE ID-name
      lists, a name not included in that list, a string representing a
      positive integer and another representing a negative integer.

      Example code begins here.


      /.
         Program bods2c_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main()
      {

         /.
         Local variables.
         ./
         SpiceBoolean            found;
         SpiceInt                code;
         SpiceInt                i;

         /.
         Assign an array of body names. Not all the listed names
         map to a NAIF ID.
         ./
         SpiceChar             * names[] = { "Hyperion", "Earth",
                                             "  Earth ", "EMB",
                                             "Solar System Barycenter",
                                             "Voyager 2",
                                             "U.S.S. Enterprise",
                                             "1000000", "-999" };

         /.
         Loop over the `names' array, call bods2c_c for each
         element of `names'.
         ./
         printf( "Name                      Code   \n" );
         printf( "-----------------------   -------\n" );

         for ( i = 0; i < 9; i++ )
         {
            bods2c_c ( names[i], &code, &found );

            if ( found )
            {
               printf( "%23s   %7d\n", names[i], (int)code);
            }
            else
            {
               printf( "%23s   **UNK**\n", names[i] );
            }

         }

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Name                      Code
      -----------------------   -------
                     Hyperion       607
                        Earth       399
                       Earth        399
                          EMB         3
      Solar System Barycenter         0
                    Voyager 2       -32
            U.S.S. Enterprise   **UNK**
                      1000000   1000000
                         -999      -999


-Restrictions

   1)  See exception <2>.

-Literature_References

   None.

-Author_and_Institution

   C.H. Acton          (JPL)
   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   K.R. Gehringer      (JPL)
   B.V. Semenov        (JPL)
   E.D. Wright         (JPL)

-Version

   -CSPICE Version 1.0.3, 10-AUG-2021 (JDR)

       Edited the header to comply with NAIF standard. Added complete
       code example.

       Updated output argument `found' description in -Detailed_Output. Added
       description of MAXL parameter. Added -Exceptions and -Restrictions.

   -CSPICE Version 1.0.2, 16-MAY-2009 (EDW)

       Edit to -Particulars section to document the bodc2s_c routine.

   -CSPICE Version 1.0.1, 27-FEB-2008 (BVS)

       Corrected the contents of the -Required_Reading section of
       the header.

   -CSPICE Version 1.0.0, 23-JUL-2004 (CHA) (NJB) (KRG)

-Index_Entries

   body name to code

-&
*/

{ /* Begin bods2c_c */

   /*
   Local variables
   */
   logical                 fnd;


   /*
   Participate in error handling
   */
   chkin_c ( "bods2c_c");


   /*
   Check the input string name to make sure the pointer is non-null
   and the string length is non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "bods2c_c", name );


   /*
   Translate the name to the corresponding code.
   */
   bods2c_( ( char    * ) name,
            ( integer * ) code,
            ( logical * ) &fnd,
            ( ftnlen    ) strlen(name)    );


   /*
   Assign the SpiceBoolean found flag.
   */

   *found = fnd;



   chkout_c ( "bods2c_c");

} /* End bods2c_c */
