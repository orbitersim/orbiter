/*

-Procedure bodc2n_c ( Body ID code to name translation )

-Abstract

   Translate the SPICE integer code of a body into a common name
   for that body.

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

*/
   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZst.h"
   #include "SpiceZmc.h"
   #undef   bodc2n_c

   void bodc2n_c ( SpiceInt        code,
                   SpiceInt        namlen,
                   SpiceChar     * name,
                   SpiceBoolean  * found   )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   code       I   Integer ID code to be translated into a name.
   namlen     I   Available length in the output string `name'.
   name       O   A common name for the body identified by `code'.
   found      O   SPICETRUE if translated, otherwise false.

-Detailed_Input

   code        is an integer code for a body ---
               a planet, satellite, barycenter, spacecraft,
               asteroid, comet, or other ephemeris object.

   namlen      is the maximum allowed length of the output name,
               including the terminating null character. For example,
               if the caller wishes to be able to accept a 32-character
               name, `namlen' must be set to (at least) 33. The current
               maximum name length is 32 characters, so a value of 33
               for `namlen' will suffice.

-Detailed_Output

   name        is a common name of the body identified by `code'.
               If `code' has more than one translation, then the
               most recently defined `name' corresponding to `code'
               is returned.  `name' will have the exact format (case
               and blanks) as when the name/code pair was defined.

               No more than `namlen' characters, including the
               terminating null, will be written to `name'. A terminating
               null will always be written.

   found       is SPICETRUE if `code' has a translation. Otherwise, `found'
               is SPICEFALSE.

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

   bodc2n_c is one of five related subroutines,

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

   Refer to naif_id.req for the list of name/code associations built into
   SPICE, and for details concerning adding new name/code
   associations at run time by loading text kernels.

-Examples

   The numerical results shown for these examples may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Suppose you ran the utility program SPACIT to summarize
      an SPK ephemeris file and the following data was output
      to the terminal screen.

         ----------------------------------------------------------
         Segment identifier: JPL archive 21354
         Body        : -77                         Center     : 399
         From        : 1990 DEC 08 18:00:00.000
         To          : 1990 DEC 10 21:10:00.000
         Reference   : DE-200                      SPK Type    :1
         ----------------------------------------------------------

      Write a program to translate the body codes shown in the SPACIT
      output.

      Example code begins here.


      /.
         Program bodc2n_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main( )
      {
         /.
         Local constants.
         ./
         #define MAXLEN          32

         SpiceInt                code[] = { -77, 399 };

         /.
         Local variables.
         ./
         SpiceBoolean            found;
         SpiceChar               name  [ MAXLEN ];
         SpiceInt                i;

         for ( i = 0; i < 2; i++ )
         {
            bodc2n_c ( code[i], MAXLEN, name, &found );

            if ( found )
            {
               printf ( "body ID %d =  %s\n", (int)code[i], name );
            }
            else
            {
               printf ( "No name corresponding to code: %d\n",
                        (int)code[i]                         );
            }
         }

         return ( 0 );

      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      body ID -77 =  GALILEO ORBITER
      body ID 399 =  EARTH


      Note, you could also read the body and center codes directly from
      the SPK files, using the appropriate DAF routines, and then
      translate them, as above.

   2) Apply the bodc2n_c call to several IDs representing codes
      included in the default SPICE ID-name lists and codes not
      included in the list.

      Example code begins here.


      /.
         Program bodc2n_ex2
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main()
      {
         /.
         Local constants.
         ./
         #define  LENNAM         37

         /.
         Local variables.
         ./
         SpiceBoolean            found;
         SpiceChar               name [LENNAM];
         SpiceInt                i;

         /.
         Assign an array of body ID codes. Not all the listed codes
         map to a body name.
         ./
         SpiceInt                code[] = { 399, 0, 3, -77,
                                            11, -1, 6000001 };

         /.
         Loop over the `code' array, call bodc2n_c for each
         element of `code'.
         ./
         printf( "Code      Name\n" );
         printf( "-------   -----------------------\n" );

         for (i=0; i<7; i++ )
         {
            bodc2n_c ( code[i], LENNAM, name, &found );

            if ( found )
            {
               printf( "%7d   %s\n", (int)code[i], name );
            }
            else
            {
               printf( "%7d   *** NOT FOUND ***\n", (int)code[i] );
            }

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
           11   *** NOT FOUND ***
           -1   GEOTAIL
      6000001   *** NOT FOUND ***


-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   B.V. Semenov        (JPL)
   E.D. Wright         (JPL)

-Version

   -CSPICE Version 2.3.0, 10-AUG-2021 (JDR)

       Changed the argument name "lenout" to "namlen" and edited the
       header comments to comply with NAIF standard.

       Edited the header to comply with NAIF standard. Added complete
       code examples.

       Added description of MAXL parameter. Added -Exceptions, -Restrictions
       and -Files.

   -CSPICE Version 2.2.2, 24-APR-2010 (EDW)

       Edit to -Particulars section to document the bodc2s_c routine.
       Minor edit to code comments eliminating typo.

   -CSPICE Version 2.2.1, 27-FEB-2008 (BVS)

       Corrected the contents of the -Required_Reading section of
       the header.

   -CSPICE Version 2.2.0, 02-SEP-1999 (NJB)

       Local type logical variable now used for found flag used in
       interface of bodc2n_.

   -CSPICE Version 2.1.1, 25-MAR-1998 (EDW)

       Minor corrections to header.

   -CSPICE Version 2.1.0, 09-FEB-1998 (NJB)

       Re-implemented routine without dynamically allocated, temporary
       strings. Updated the -Exceptions header section.

   -CSPICE Version 2.0.1, 16-JAN-1998 (EDW)

       Corrected and clarified header entries.

   -CSPICE Version 1.0.0, 25-OCT-1997 (NJB)

       Based on SPICELIB Version 1.0.0, 23-JAN-1996 (KRG)

-Index_Entries

   body id code to name

-&
*/

{ /* Begin bodc2n_c */


   /*
   Local variables
   */
   logical                 fnd;


   /*
   Participate in error tracing.
   */
   chkin_c ( "bodc2n_c");


   /*
   Make sure the output name has at least enough room for one output
   character and a null terminator.  Also check for a null pointer.
   */
   CHKOSTR ( CHK_STANDARD, "bodc2n_c", name, namlen );


   /*
   Call the f2c'd routine.
   */
   bodc2n_( ( integer * )  &code,
            ( char    * )  name,
            ( logical * )  &fnd,
            ( ftnlen    )  namlen-1 );


   /*
   Assign the SpiceBoolean found flag.
   */

   *found = fnd;


   /*
   Convert the Fortran string to a C string by placing a null
   after the last non-blank character.  This operation is valid
   whether or not the CSPICE routine signaled an error.
   */
   F2C_ConvertStr ( namlen, name );


   chkout_c ( "bodc2n_c");

} /* End bodc2n_c */
