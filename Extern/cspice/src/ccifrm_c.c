/*

-Procedure ccifrm_c ( Class and class ID to associated frame )

-Abstract

   Return the frame name, frame ID, and center associated with
   a given frame class and class ID.

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

   FRAMES

-Keywords

   FRAMES

*/

   #include "SpiceUsr.h"
   #include "SpiceZmc.h"
   #include "SpiceZfc.h"
   #include "SpiceZst.h"

   void ccifrm_c ( SpiceInt          frclss,
                   SpiceInt          clssid,
                   SpiceInt          frnlen,
                   SpiceInt        * frcode,
                   SpiceChar       * frname,
                   SpiceInt        * cent,
                   SpiceBoolean    * found   )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   frclss     I   Class of frame.
   clssid     I   Class ID of frame.
   frnlen     I   Maximum length of output string.
   frcode     O   ID code of the frame.
   frname     O   Name of the frame.
   cent       O   ID code of the center of the frame.
   found      O   SPICETRUE if the requested information is available.

-Detailed_Input

   frclss      is the class or type of the frame. This identifies which
               subsystem will be used to perform frame transformations.

   clssid      is the ID code used for the frame within its class. This
               may be different from the frame ID code.

   frnlen      is the allowed length of the output frame name. This length
               must large enough to hold the output string plus the
               null terminator. If the output string is expected to have
               n characters, `frnlen' should be n + 1.

-Detailed_Output

   frcode      is the frame ID code for the reference frame
               identified by `frclss' and `clssid'.

   frname      is the name of the frame identified by
               `frclss' and `clssid'.

               `frname' should be declared

                   SpiceChar frname [33]

               to ensure that it can contain the full name of the
               frame. If `frname' does not have enough room to hold
               the full name of the frame, the name will be truncated
               on the right.

   cent        is the body ID code for the center of the reference
               frame identified by `frclss' and `clssid'.

   found       is SPICETRUE if `frcode', `frname', and `cent' are available.
               Otherwise, `found' is returned with the value SPICEFALSE.

-Parameters

   None.

-Exceptions

   1)  This routine assumes that the first frame found with matching
       class and class ID is the correct one. SPICE's frame system
       does not diagnose the situation where there are multiple,
       distinct frames with matching classes and class ID codes, but
       this situation could occur if such conflicting frame
       specifications are loaded via one or more frame kernels. The
       user is responsible for avoiding such frame specification
       conflicts.

   2)  If `frname' does not have room to contain the frame name, the
       name will be truncated on the right. (Declaring `frname' to be
       SpiceChar[33] will ensure that the name will not be truncated).

   3)  If a frame class assignment is found that associates a string
       (as opposed to numeric) value with a frame class keyword, the
       error SPICE(INVALIDFRAMEDEF) is signaled by a routine in the
       call tree of this routine.

   4)  If a frame class assignment is found that matches the input
       class, but a corresponding class ID assignment is not
       found in the kernel pool, the error SPICE(INVALIDFRAMEDEF)
       is signaled by a routine in the call tree of this routine.

   5)  If a frame specification is found in the kernel pool with
       matching frame class and class ID, but either the frame name
       or frame ID code are not found, the error
       SPICE(INVALIDFRAMEDEF) is signaled by a routine in the call
       tree of this routine.

   6)  If a frame specification is found in the kernel pool with
       matching frame class and class ID, but the frame center
       is not found, an error is signaled by a routine
       in the call tree of this routine.

   7)  If the `frname' output string pointer is null, the error
       SPICE(NULLPOINTER) is signaled.

   8)  If the `frname' output string has length less than two
       characters, the error SPICE(STRINGTOOSHORT) is signaled, since
       the output string is too short to contain one character of
       output data plus a null terminator.

-Files

   The frame specifications sought by this routine may be provided
   by loaded frames kernels. Such kernels will always be required if
   the frame class is CK, TK, or dynamic, and will be required if
   the frame class is PCK but the frame of interest is not built-in.

-Particulars

   This routine allows the user to determine the frame associated
   with a given frame class and class ID code. The kernel pool is
   searched first for a matching frame; if no match is found, then
   the set of built-in frames is searched.

   Since the neither the frame class nor the class ID are primary
   keys, searching for matching frames is a linear (and therefore
   typically slow) process.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) The following code example demonstrates how to find the frame
      information about a frame by its ID using frinfo_c and
      by its class and class ID using ccifrm_c.


      Example code begins here.


      /.
         Program ccifrm_ex1
      ./
      #include <stdio.h>
      #include <stdlib.h>
      #include <string.h>
      #include "SpiceUsr.h"

      int main()
      {

         /.
         Local constants.
         ./
         #define FRNLEN         33

         /.
         Local variables.
         ./
         SpiceChar              frname[FRNLEN];
         SpiceInt               clss;
         SpiceInt               clss_ID;

         SpiceInt               frcode1;
         SpiceInt               frcode2;

         SpiceInt               center1;
         SpiceInt               center2;

         SpiceBoolean           found;

         /.
         Assign the frame name.
         ./
         strcpy ( frname, "ITRF93" );

         namfrm_c ( frname, &frcode1 );

         frinfo_c ( frcode1, &center1, &clss, &clss_ID, &found );

         if ( !found )
         {
            printf( "No info found for frame %d\n", (int)frcode1 );
            exit(1);
         }

         printf(  "Frame %s info:\n"
                  "   Frame Code: %d \n"
                  "   Center ID : %d \n"
                  "   Class     : %d \n"
                  "   Class ID  : %d \n",
                  frname,
                  (int)frcode1,
                  (int)center1,
                  (int)clss,
                  (int)clss_ID );

         ccifrm_c ( clss,      clss_ID, FRNLEN,
                    &frcode2,  frname,  &center2, &found );

         if ( !found )
         {
            printf( "No info found for type %d frame %d.\n", (int)clss,
                                                             (int)clss_ID );
            exit(1);
         }

         printf(  "Type %2d frame %5d info:\n"
                  "   Frame name: %s \n"
                  "   Frame Code: %d \n"
                  "   Center ID : %d \n",
                  (int)clss,
                  (int)clss_ID,
                  frname,
                  (int)frcode2,
                  (int)center2 );

         exit(0);
         }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Frame ITRF93 info:
         Frame Code: 13000
         Center ID : 399
         Class     : 2
         Class ID  : 3000
      Type  2 frame  3000 info:
         Frame name: ITRF93
         Frame Code: 13000
         Center ID : 399


-Restrictions

   1)  See item (1) in the -Exceptions section above.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   B.V. Semenov        (JPL)
   E.D. Wright         (JPL)

-Version

   -CSPICE Version 1.1.0, 13-AUG-2021 (JDR)

       Changed the argument names "lenout" and "center" to "frnlen" and
       "cent" for consistency with other routines.

       Edited the header to comply with NAIF standard.

   -CSPICE Version 1.0.1, 04-FEB-2017 (EDW) (BVS)

       Edit to example program to use "%d" with explicit casts
       to int for printing SpiceInts with printf.

       Shortened one of permuted index entries.

   -CSPICE Version 1.0.0, 14-JUL-2014 (NJB)

       Added to the -Brief_I/O header section a description
       of input argument "lenout".

    Last update was  10-JAN-2011 (NJB)(EDW)

-Index_Entries

   Find info associated with a frame class and class id
   Map frame class and class id to frame info
   Map frame class and class id to frame name, id, center

-&
*/

{ /* Begin ccifrm_c */



   /*
   Local variables
   */
   logical                 fnd;

   /*
   Participate in error tracing.
   */
   chkin_c ( "ccifrm_c" );

   /*
   Make sure the output string has at least enough room for one output
   character and a null terminator.  Also check for a null pointer.
   */
   CHKOSTR ( CHK_STANDARD, "ccifrm_c", frname, frnlen );

   /*
   Map the inputs to frame attributes, if possible.
   */
   ccifrm_( ( integer * ) &frclss,
            ( integer * ) &clssid,
            ( integer * ) frcode,
            ( char    * ) frname,
            ( integer * ) cent,
            ( logical * ) &fnd,
            ( ftnlen    ) frnlen-1 );

   /*
   The string returned, output, is a Fortranish type string.
   Convert the string to C style.
   */
   F2C_ConvertStr ( frnlen, frname );

   /*
   Return the FOUND flag as a SpiceBoolean value.
   */

   *found = (SpiceBoolean)fnd;


   chkout_c ( "ccifrm_c" );

} /* End ccifrm_c */
