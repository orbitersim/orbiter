/*

-Procedure cidfrm_c ( center SPK ID frame )

-Abstract

   Retrieve frame ID code and name to associate with a frame center.

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
   #include "SpiceZst.h"
   #include "SpiceZmc.h"


   void cidfrm_c ( SpiceInt        cent,
                   SpiceInt        namlen,
                   SpiceInt      * frcode,
                   SpiceChar     * frname,
                   SpiceBoolean  * found  )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   cent       I   An object to associate a frame with.
   namlen     I   Available space in output string `frname'.
   frcode     O   The ID code of the frame associated with `cent'.
   frname     O   The name of the frame with ID `frcode'.
   found      O   SPICETRUE if the requested information is available.

-Detailed_Input

   cent        is the ID code for object for which there is a
               preferred reference frame.

   namlen      is the available space in the output string frname,
               including room for the terminating null character.

-Detailed_Output

   frcode      is the frame ID code to associate with the object
               specified by `cent'.

   frname      is the name of the frame that should be associated
               with the object specified by `cent'. `frname' should be
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

   2)  If the `frname' output string pointer is null, the error
       SPICE(NULLPOINTER) is signaled.

   3)  If the `frname' output string has length less than two
       characters, the error SPICE(STRINGTOOSHORT) is signaled, since
       the output string is too short to contain one character of
       output data plus a null terminator.

-Files

   None.

-Particulars

   This routine allows the user to determine the frame that should
   be associated with a particular object. For example, if you
   need the frame to associate with the Io, you can call cidfrm_c
   to determine the frame name and ID code for the bodyfixed frame
   of Io.

   The preferred frame to use with an object is specified via one
   of the kernel pool variables:

       OBJECT_<cent>_FRAME

   where <cent> is the decimal representation of the integer `cent'.

   For those PCK objects that have "built-in" frame names this
   routine returns the corresponding "IAU" frame and frame ID code.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Determine the "preferred" frame to use with Jupiter's moon Io,
      the Earth and the Moon. Use their corresponding NAIF ID codes.

      Note that for any objects that has a "built-in" frame name, the
      preferred frame is its corresponding "IAU" frame, which may not
      be suitable for high-precision computations.

      Example code begins here.


      /.
         Program cidfrm_ex1
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
         Assign an array of body IDs. Not all the listed NAIF IDs
         have a frame associated to them.
         ./
         SpiceInt                bodyid[] = { 501, 399, 301, -999 };


         /.
         Loop over the `body' array, call cidfrm_c for each
         element of `body'.
         ./
         printf( "Body ID  Frame ID  Frame name\n" );
         printf( "-------  --------  ----------\n" );

         for (i = 0; i < 4; i++ )
         {
            cidfrm_c ( bodyid[i], FRNMLN, &frcode, frname, &found );

            if ( found )
            {
               printf( "%7d %9d  %s\n", (int)bodyid[i],
                                        (int)frcode, frname );
            }
            else
            {
               printf( "%7d  No frame associated with body ID\n",
                       (int)bodyid[i]                          );
            }

         }


         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Body ID  Frame ID  Frame name
      -------  --------  ----------
          501     10023  IAU_IO
          399     10013  IAU_EARTH
          301     10020  IAU_MOON
         -999  No frame associated with body ID


-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   W.L. Taber          (JPL)

-Version

   -CSPICE Version 1.1.0, 13-AUG-2021 (JDR)

       Changed the argument name "lenout" to "namlen" for consistency with
       other routines.

       Edited the header to comply with NAIF standard.
       Added complete code example.

       Updated -Detailed_Output to indicate the required minimum length for
       the output frame character string to not be truncated.
       Added -Exceptions section.

   -CSPICE Version 1.0.0, 22-JUL-1999 (NJB) (WLT)

-Index_Entries

   Fetch reference frame attributes

-&
*/

{ /* Begin cidfrm_c */

   /*
   Local variables
   */
   logical                 fnd;


   /*
   Participate in error tracing.
   */
   chkin_c ( "cidfrm_c" );


   /*
   Check the output string to make sure the pointer is non-null and that
   there is room for at least one character plus a null terminator.
   */
   CHKOSTR ( CHK_STANDARD, "cidfrm_c", frname, namlen );


   /*
   Call the f2c'd routine.
   */
   cidfrm_ (  ( integer   * ) &cent,
              ( integer   * ) frcode,
              ( char      * ) frname,
              ( logical   * ) &fnd,
              ( ftnlen      ) namlen-1 );

   /*
   Convert the output string from Fortran to C style.
   */
   F2C_ConvertStr ( namlen, frname );


   /*
   Set the SpiceBoolean found flag.
   */
   *found = fnd;


   chkout_c ( "cidfrm_c" );

} /* End cidfrm_c */
