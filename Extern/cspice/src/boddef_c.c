/*

-Procedure boddef_c ( Body name/ID code definition )

-Abstract

   Define a body name/ID code pair for later translation via
   bodn2c_c or bodc2n_c.

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
   #include "SpiceZmc.h"

   void boddef_c ( ConstSpiceChar   * name,
                   SpiceInt           code )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   name       I   Common name of some body.
   code       I   Integer code for that body.

-Detailed_Input

   name        is an arbitrary name of a body which could be
               a planet, satellite, barycenter, spacecraft,
               asteroid, comet, or other ephemeris object.

               The case and positions of blanks in a name are
               significant. bodc2n_c returns the same string
               (case and space) most recently mapped to a code.
               When 'name' consists of more than one word, the
               words require separation by at least one blank.

               The kernel sub-system stores 'name' as described in
               the boddef_c call, but creates an equivalence class
               based on 'name for comparisons in bodn2c_c. This class
               ignores leading/trailing whitespace, compresses
               interior whitespace to a single space, and ignores
               character case.

               The following strings belong to the same equivalence
               class:

                       "JUPITER BARYCENTER"
                       "Jupiter Barycenter"
                       "JUPITER BARYCENTER   "
                       "JUPITER    BARYCENTER"
                       "   JUPITER BARYCENTER"

               However, "JUPITERBARYCENTER" is distinct from
               the names above.

               When ignoring trailing blanks, NAME must be short
               enough to fit into the space defined by parameter
               MAXL.The value may be found in the C file
               zzbodtrn.c. Due to the way in which f2c converts
               FORTRAN code to C, you must examine the dimensions
               assigned to the variables:

                   defnam
                   defnor
                   kernam
                   kernor

                to obtain the MAXL value. These variables have a
                declaration of the form:

                   static char variable_name[MAXL*array_length]

                  (note MAXL is this first value).

               The maximum allowed length of a name is in any case
               at least 32 characters.

   code        is the integer ID code for assignment to body 'name'.

-Detailed_Output

   None.

-Parameters

   None.

-Exceptions

   1)  If improper inputs are supplied, or if there is insufficient
       room to store the requested addition, an error is signaled by
       a routine in the call tree of this routine.

   2)  If the length of `name' exceeds the maximum allowed length for a
       body name, the name stored in the kernel pool will be
       truncated on the right.

   3)  If a name-code definition inserted into this routine seems to
       have no effect, it is possible that the contents of the
       definition are masked by the higher precedence kernel pool
       assignments. See the -Particulars section of this document
       for more information.

   4)  If the `name' input string pointer is null, the error
       SPICE(NULLPOINTER) is signaled.

   5)  If the `name' input string has zero length, the error
       SPICE(EMPTYSTRING) is signaled.

-Files

   None.

-Particulars

   boddef_c is one of five related subroutines,

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

   Modifying the SPICE name-ID mapping set
   =======================================

   Each body has a unique integer 'code', but may have several
   names. Thus you may associate more than one name with
   a particular integer code.

   'code' may already have a name as defined by a previous
   call to boddef_c or as part of the set of default
   definitions. That previous definition will remain,
   and a translation of that name will still give the
   same 'code'.  However, future translations of 'code' will
   give the new 'name' instead of the previous one. This
   feature is useful for assigning a more familiar or
   abbreviated name to a body. For example, in addition
   to the default name for body 5, "JUPITER BARYCENTER",
   you could define the abbreviation "JB" to mean 5.

   Note: In the case where boddef_c performs a name-to-ID mapping
   assignment for an unused body name and unused ID value,
   any subsequent assignment to NAME destroys the previous
   mapping.

      boddef_c ( "spud", 22)

   then

      boddef_c ( "spud", 23)

   results in the state "spud" maps to 23, 23 maps to "spud",
   and 22 maps to nothing ('found' in bodc2n_c returns SPICEFALSE).

-Examples

   You may associate a new name with a particular code that
   has already been defined:

          boddef_c ( "JB", 5 );

   You may also define the name and integer code for a new body:

          boddef_c ( "Asteroid Frank", 20103456 );

   After these calls to boddef_c, bodn2c_c would return the following
   translations:

      Name                         Code    Found?
      ------------------------   ------    ------
      "JB"                            5    Yes
      "Jupiter Barycenter"            5    Yes
      "ASTEROID FRANK"         20103456    Yes
      "ASTEROIDFRANK"                 -    No
      "Frank"                         -    No

   and BODC2N will return these translations:

      Code        Name                     Found?
      -------     -------------------      ------
             5    "JB"                     Yes
      20103456    "Asteroid Frank"         Yes

-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   K.R. Gehringer      (JPL)
   B.V. Semenov        (JPL)
   E.D. Wright         (JPL)

-Version

   -CSPICE Version 2.2.3, 08-JUL-2021 (JDR)

       Edited the header to comply with NAIF standard. Added entry #3 to
       -Exceptions section.

   -CSPICE Version 2.2.2, 16-MAY-2009 (EDW)

       Edit to -Particulars section to document the bodc2s_c routine.

   -CSPICE Version 2.2.1, 27-FEB-2008 (BVS)

       Corrected the contents of the -Required_Reading section of
       the header.

   -CSPICE Version 2.2.0, 23-JAN-2004 (EDW)

       Rewrote header for clarity with regards to the
       current capabilities of the kernel subsystem.

   -CSPICE Version 2.1.0, 17-NOV-2003 (EDW)

       Updated header to describe the maximum allowed length
       for 'name' and its effect on this module.

       Updated header with information on new functionality.
       The code-to-name retrieval routines now return the exact
       string as defined in the last code/name mapping (case
       and space).

   -CSPICE Version 2.0.1, 08-FEB-1998 (EDW)

       Corrected and clarified header entries.

   -CSPICE Version 2.0.0, 06-JAN-1998 (NJB)

       The type of the input argument name was changed to
       ConstSpiceChar *.

       References to C2F_CreateStr_Sig were removed; code was
       cleaned up accordingly. String checks are now done using
       the macro CHKFSTR.

   -CSPICE Version 1.0.0, 25-OCT-1997 (NJB) (KRG)

       Based on SPICELIB Version 1.0.0, 23-JAN-1996 (KRG)

-Index_Entries

   body name/id code definition

-&
*/

{ /* Begin boddef_c */

   /*
   Participate in error handling
   */
   chkin_c ( "boddef_c");


   /*
   Check the input string name to make sure the pointer is non-null
   and the string length is non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "boddef_c", name );


   /*
   Effect the new name/code mapping.
   */
   boddef_ ( ( char    * )  name,
             ( integer * ) &code,
             ( ftnlen    )  strlen(name) );


   chkout_c ( "boddef_c");

} /* End boddef_c */
