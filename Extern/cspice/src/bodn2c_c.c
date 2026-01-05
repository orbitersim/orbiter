/*

-Procedure bodn2c_c ( Body name to ID code translation )

-Abstract

   Translate the name of a body or object to the corresponding SPICE
   integer ID code.

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

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZmc.h"


   void bodn2c_c ( ConstSpiceChar  * name,
                   SpiceInt        * code,
                   SpiceBoolean    * found )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   name       I   Body name to be translated into a SPICE ID code.
   code       O   SPICE integer ID code for the named body.
   found      O   SPICETRUE if translated, otherwise SPICEFALSE.

-Detailed_Input

   name        is the name of a body or object, such as a planet,
               satellite, comet, asteroid, barycenter, DSN station,
               spacecraft, or instrument, that is "known" to the SPICE
               system, whether through hard-coded registration or
               run-time registration in the SPICE kernel pool.

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

-Detailed_Output

   code        is the SPICE or user-defined integer ID code for the
               named body.

   found       is SPICETRUE if `name' has a translation. Otherwise,
               `found' is SPICEFALSE.

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

   bodn2c_c is one of five related subroutines,

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

   Programmers writing user interface code should consider using the
   CSPICE routine bods2c_c. bods2c_c provides more flexibility in
   handling input strings, since it accepts both body names and
   strings representing integer ID codes, for example "399".

   Refer to NAIF_IDs for the list of name/code associations built into
   SPICE, and for details concerning adding new name/code
   associations at run time by loading text kernels.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Apply the bodn2c_c call to several body names to retrieve
      their associated NAIF IDs included in the default SPICE ID-name
      lists and a name not included in that list.

      Example code begins here.


      /.
         Program bodn2c_ex1
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
                                             "U.S.S. Enterprise" };

         /.
         Loop over the `names' array, call bodn2c_c for each
         element of `names'.
         ./
         printf( "Name                      Code   \n" );
         printf( "-----------------------   -------\n" );

         for ( i = 0; i < 7; i++ )
         {
            bodn2c_c ( names[i], &code, &found );

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


-Restrictions

   1)  See exception <2>.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   B.V. Semenov        (JPL)
   E.D. Wright         (JPL)

-Version

   -CSPICE Version 2.1.7, 10-AUG-2021 (JDR)

       Edited the header to comply with NAIF standard. Added complete
       code example.

       Added description of MAXL parameter. Added -Exceptions and
       -Restrictions.

   -CSPICE Version 2.1.6, 16-MAY-2009 (EDW)

       Edit to -Particulars section to document the bodc2s_c routine.

   -CSPICE Version 2.1.5, 27-FEB-2008 (BVS)

       Corrected the contents of the -Required_Reading section of
       the header.

   -CSPICE Version 2.1.4, 31-JAN-2008 (NJB)

       References to the routine bods2c_c were added to the header.

   -CSPICE Version 2.1.3, 27-OCT-2005 (NJB)

       Header update: replaced references to bodvar_c with
       references to bodvcd_c.

   -CSPICE Version 2.1.2, 23-JUL-2004 (NJB)

       Header correction: -Exceptions section was updated to document
       input string error handling.

   -CSPICE Version 2.1.1, 28-JUL-2003 (NJB)

       Various header changes were made to improve clarity. Some
       minor header corrections were made.

   -CSPICE Version 2.1.0, 02-SEP-1999 (NJB)

       Local type logical variable now used for found flag used in
       interface of bodn2c_.

   -CSPICE Version 2.0.2, 25-MAR-1998 (EDW)

       Minor corrections to header.

   -CSPICE Version 2.0.1, 08-FEB-1998 (EDW)

       Corrected and clarified header entries.

   -CSPICE Version 2.0.0, 06-JAN-1998 (NJB)

       The type of the input argument name was changed to
       ConstSpiceChar *.

       References to C2F_CreateStr_Sig were removed; code was
       cleaned up accordingly. String checks are now done using
       the macro CHKFSTR.

   -CSPICE Version 1.0.0, 25-OCT-1997 (NJB)

       Based on SPICELIB Version 1.0.0, 23-JAN-1996 (KRG)

-Index_Entries

   body name to code

-&
*/

{ /* Begin bodn2c_c */

   /*
   Local variables
   */
   logical                 fnd;


   /*
   Participate in error handling
   */
   chkin_c ( "bodn2c_c");


   /*
   Check the input string name to make sure the pointer is non-null
   and the string length is non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "bodn2c_c", name );


   /*
   Translate the name to the corresponding code.
   */
   bodn2c_( ( char    * ) name,
            ( integer * ) code,
            ( logical * ) &fnd,
            ( ftnlen    ) strlen(name)    );


   /*
   Assign the SpiceBoolean found flag.
   */

   *found = fnd;



   chkout_c ( "bodn2c_c");

} /* End bodn2c_c */
