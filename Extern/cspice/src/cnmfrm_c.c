/*

-Procedure cnmfrm_c ( Center name to associated frame )

-Abstract

   Retrieve frame ID code and name to associate with an object.

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

   None.

-Keywords

   FRAMES

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZmc.h"
   #include "SpiceZst.h"


   void cnmfrm_c ( ConstSpiceChar   * cname,
                   SpiceInt           frnlen,
                   SpiceInt         * frcode,
                   SpiceChar        * frname,
                   SpiceBoolean     * found   )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   cname      I   Name of the object to find a frame for.
   frnlen     I   Maximum length available for frame name.
   frcode     O   The ID code of the frame associated with cname.
   frname     O   The name of the frame with ID frcode.
   found      O   SPICETRUE if the requested information is available.

-Detailed_Input

   cname       is the name for object for which there is a
               preferred reference frame.

   frnlen      is the amount of space available, counting the
               space required for the terminating null character,
               in the output string `frname'. Normally `frnlen' is
               the declared length of `frname'.

-Detailed_Output

   frcode      is the frame ID code to associate with a the object
               specified by `cname'.

   frname      is the name of the frame that should be associated
               with the object specified by `cname'. `frname' should be
               declared as SpiceChar[33] to ensure that it can
               contain the full name of the frame. If `frname' does
               not have enough room to hold the full name of the
               frame, the name will be truncated on the right.

   found       is SPICETRUE if the appropriate frame ID code and frame
               name can be determined. Otherwise `found' is returned
               with the value SPICEFALSE.

-Parameters

   None.

-Exceptions

   1)  If `frname' does not have enough room to contain the frame name,
       the name will be truncated on the right. (Declaring `frname' to
       be SpiceChar[33] will ensure that the name will not be truncated).

   2)  If the `cname' input string pointer is null, the error
       SPICE(NULLPOINTER) is signaled.

   3)  If the `cname' input string has zero length, the error
       SPICE(EMPTYSTRING) is signaled.

   4)  If the `frname' output string pointer is null, the error
       SPICE(NULLPOINTER) is signaled.

   5)  If the `frname' output string has length less than two
       characters, the error SPICE(STRINGTOOSHORT) is signaled, since
       the output string is too short to contain one character of
       output data plus a null terminator.

-Files

   None.

-Particulars

   This routine allows the caller to determine the frame that should
   be associated with a particular object. For example, if you
   need the frame to associate with the Io, you can call cnmfrm_c
   to determine the frame name and id-code for the bodyfixed frame
   of Io.

   The preferred frame to use with an object is specified via one
   of the kernel pool variables:

       OBJECT_<cname>_FRAME

   where <cname> is the non-blank portion of the string CNAME.

   For those PCK objects that have "built-in" frame names this
   routine returns the corresponding "IAU" frame and frame ID code.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Retrieve the ID code and name of the preferred frame
      associated with a set of body names.

      Example code begins here.


      /.
         Program cnmfrm_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main( )
      {
         /.
         Local constants.
         ./
         #define FRNMLN          33

         /.
         Local variables.
         ./
         SpiceBoolean            found;

         SpiceChar               frname [ FRNMLN ];
         SpiceInt                frcode;

         SpiceInt                i;

         /.
         Assign an array of body names. Not all the listed names
         have a frame associated to them.
         ./
         SpiceChar             * body[] = { "IO",   "EARTH",
                                            "MOON", "HALO_DELTA" };

         /.
         Loop over the `body' array, call cnmfrm_c for each
         element of `body'.
         ./
         printf( "   Body     Frame ID  Frame name\n" );
         printf( "----------  --------  ----------\n" );

         for (i = 0; i < 4; i++ )
         {
            cnmfrm_c ( body[i], FRNMLN, &frcode, frname, &found );

            if ( found )
            {
               printf( "%10s %9d  %s\n", body[i], (int)frcode, frname );
            }
            else
            {
               printf( "%10s  No frame associated with body\n", body[i] );
            }

         }

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


         Body     Frame ID  Frame name
      ----------  --------  ----------
              IO     10023  IAU_IO
           EARTH     10013  IAU_EARTH
            MOON     10020  IAU_MOON
      HALO_DELTA  No frame associated with body


-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   W.L. Taber          (JPL)

-Version

   -CSPICE Version 1.1.0, 02-AUG-2021 (JDR)

       Changed the input argument name "lenout" to "frnlen" for
       consistency with other routines.

       Edited the header to comply with NAIF standard. Added
       complete code example.

       Updated -Detailed_Output and -Exceptions to indicate the
       required minimum length for the output frame character
       string to not be truncated.

   -CSPICE Version 1.0.0, 25-JUN-1999 (NJB) (WLT)

-Index_Entries

   Fetch reference frame attributes

-&
*/

{ /* Begin cnmfrm_c */


   /*
   Participate in error tracing.
   */
   chkin_c ( "cnmfrm_c" );

   /*
   Check the input object's name string to make sure the pointer
   is non-null and the string length is non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "cnmfrm_c", cname );

   /*
   Make sure the output string has at least enough room for one output
   character and a null terminator.  Also check for a null pointer.
   */
   CHKOSTR ( CHK_STANDARD, "cnmfrm_c", frname, frnlen );


   /*
   Invoke the f2c'd routine.
   */
   cnmfrm_ (  ( char      * ) cname,
              ( integer   * ) frcode,
              ( char      * ) frname,
              ( logical   * ) found,
              ( ftnlen      ) strlen(cname),
              ( ftnlen      ) frnlen-1       );


   /*
   Convert the output string to C-style.
   */
   F2C_ConvertStr ( frnlen, frname );


   chkout_c ( "cnmfrm_c" );

} /* End cnmfrm_c */
