/*

-Procedure srfs2c_c ( Surface and body strings to surface ID code )

-Abstract

   Translate a surface string, together with a body string, to the
   corresponding surface ID code. The input strings may contain
   names or integer ID codes.

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

   DSK
   NAIF_IDS

-Keywords

   CONVERSION
   DSK
   ID
   NAME
   STRING
   SURFACE

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZst.h"
   #include "SpiceZmc.h"

   void srfs2c_c ( ConstSpiceChar  * srfstr,
                   ConstSpiceChar  * bodstr,
                   SpiceInt        * code,
                   SpiceBoolean    * found )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   srfstr     I   Surface name or ID string.
   bodstr     I   Body name or ID string.
   code       O   Integer surface ID code.
   found      O   Flag indicating whether surface ID was found.

-Detailed_Input

   srfstr      is a string designating a surface. `srfstr' may contain
               a surface name or a string representation of the
               surface's integer ID code.

               If, for the body specified by `bodstr', multiple surface
               names are associated with one surface ID code, then any
               of these names may be used as the value of `srfstr'.

               Case and leading and trailing blanks in a surface name
               are not significant. Sequences of consecutive embedded
               blanks are considered equivalent to a single blank.
               For example, all of the strings below are considered
               to be equivalent:

                  "MGS MOLA 128 PIXEL/DEG"
                  "MGS MOLA 128 pixel/deg"
                  "MGS MOLA 128 PIXEL/DEG   "
                  "MGS MOLA 128    PIXEL/DEG"
                  "   MGS MOLA 128 PIXEL/DEG"

               However,

                  "MGSMOLA 128PIXEL/DEG"

               is not equivalent to the names above.


   bodstr      is a string designating the body associated with the
               input surface string. `bodstr' may contain a body name
               or a string representation of the body's integer ID
               code. For example, `bodstr' may contain

                  "1000012"

               instead of

                  "67P/CHURYUMOV-GERASIMENKO (1969 R1)"

               Case and leading and trailing blanks in a name are not
               significant. The treatment of blanks in `bodstr' is the
               same as for `srfstr'.

-Detailed_Output

   code        is integer ID code of the surface designated by
               `srfstr', for the body designated by `bodstr', if for this
               body an association exists between the input surface
               string and a surface ID code. `code' is defined if and
               only if the output flag `found' is SPICETRUE.

   found       is a logical flag that is SPICETRUE if a surface code
               corresponding to the input strings was found and
               SPICEFALSE otherwise.

-Parameters

   None.

-Exceptions

   1)  If the input surface string does not map to an ID code
       and does not represent an integer, the output `code' is
       undefined and the output `found' is set to SPICEFALSE.

       This case is not treated as an error.

   2)  If the input body string does not map to an ID code and does
       not represent an integer, the output `code' is undefined and
       the output `found' is set to SPICEFALSE.

       This case is not treated as an error.

   3)  If any of the `srfstr' or `bodstr' input string pointers is
       null, the error SPICE(NULLPOINTER) is signaled.

   4)  If any of the `srfstr' or `bodstr' input strings has zero
       length, the error SPICE(EMPTYSTRING) is signaled.

-Files

   Surface name-to-ID mappings may be defined at run time by loading
   text kernels containing kernel variable assignments of the form

      NAIF_SURFACE_NAME += ( <surface name 1>, ... )
      NAIF_SURFACE_CODE += ( <surface code 1>, ... )
      NAIF_SURFACE_BODY += ( <body code 1>,    ... )

   Above, the Ith elements of the lists on the assignments' right
   hand sides together define the Ith surface name/ID mapping.

   The same effect can be achieved using assignments formatted as
   follows:

      NAIF_SURFACE_NAME += <surface name 1>
      NAIF_SURFACE_CODE += <surface code 1>
      NAIF_SURFACE_BODY += <body code 1>

      NAIF_SURFACE_NAME += <surface name 2>
      NAIF_SURFACE_CODE += <surface code 2>
      NAIF_SURFACE_BODY += <body code 2>

         ...

   Note the use of the

      +=

   operator; this operator appends to rather than overwrites the
   kernel variable named on the left hand side of the assignment.

-Particulars

   Surfaces are always associated with bodies (which usually are
   ephemeris objects). For any given body, a mapping between surface
   names and surface ID codes can be established.

   Bodies serve to disambiguate surface names and ID codes: the set
   of surface names and surface ID codes for a given body can be
   thought of as belonging to a name space. A given surface ID code
   or surface name may be used for surfaces of multiple bodies,
   without conflict.

   Associations between surface names and ID codes are always made
   via kernel pool assignments; there are no built-in associations.

   srfs2c_c is one of four related functions:

      srfs2c_c      Surface string and body string to surface ID code
      srfscc_c      Surface string and body ID code to surface ID code
      srfc2s_c      Surface ID code and body ID code to surface string
      srfcss_c      Surface ID code and body string to surface string

   srfs2c_c, srfc2s_c, srfscc_c, and srfcss_c perform translations between
   surface strings and their corresponding integer ID codes.

   Refer to naif_ids.req for details concerning adding new surface
   name/code associations at run time by loading text kernels.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as input,
   the compiler and supporting libraries, and the machine specific
   arithmetic implementation.

   1) Suppose a text kernel has been loaded that contains
      the following assignments:

         NAIF_SURFACE_NAME += ( 'MGS MOLA  64 pixel/deg',
                                'MGS MOLA 128 pixel/deg',
                                'PHOBOS GASKELL Q512'     )
         NAIF_SURFACE_CODE += (   1,   2,    1 )
         NAIF_SURFACE_BODY += ( 499, 499,  401 )

      Translate each surface and body string pair to the
      associated surface ID code. Also perform a translation
      for a surface name having no matching ID and for
      a body string having no matching body ID code.

      Use the meta-kernel shown below to define the required SPICE
      kernel variables.


         KPL/MK

         File: srfs2c_ex1.tm

         This meta-kernel is intended to support operation of SPICE
         example programs. The file contents shown here should not be
         assumed to contain adequate or correct versions of data
         required by SPICE-based user applications.


         \begindata

         NAIF_SURFACE_NAME += ( 'MGS MOLA  64 pixel/deg',
                                'MGS MOLA 128 pixel/deg',
                                'PHOBOS GASKELL Q512'     )
         NAIF_SURFACE_CODE += (   1,   2,    1 )
         NAIF_SURFACE_BODY += ( 499, 499,  401 )

         \begintext

         End of meta-kernel


      Example code begins here.


      /.
         Program srfs2c_ex1
      ./

      #include <stdio.h>
      #include "SpiceUsr.h"

      int main()
      {
         /.
         Local constants
         ./
         #define BDNMLN          37
         #define SFNMLN          SPICE_SRF_SFNMLN
         #define NCASE           8

         /.
         Local variables
         ./
         SpiceBoolean            found;

         static SpiceChar        bodstr [NCASE][BDNMLN] =

                                   { "MARS", "PHOBOS", "MARS",
                                     "499",  "PHOBOS", "499",
                                     "MARS", "ZZZ" };

         static SpiceChar        srfstr [NCASE][SFNMLN] =

                                   { "MGS MOLA  64 pixel/deg",
                                     "PHOBOS GASKELL Q512",
                                     "MGS MOLA 128 pixel/deg",
                                     "MGS MOLA  64 pixel/deg",
                                     "1",
                                     "2",
                                     "ZZZ",
                                     "1"                      };

         static SpiceChar        tf     [2][6] =

                                   { "false", "true" };

         SpiceChar             * meta;

         SpiceInt                i;
         SpiceInt                surfid;

         meta = "srfs2c_ex1.tm";

         furnsh_c ( meta );

         printf ( "\n" );

         for ( i = 0;  i < NCASE;  i++ )
         {
            srfs2c_c ( srfstr[i], bodstr[i], &surfid, &found );

            printf ( "surface string   = %s\n"
                     "body string      = %s\n"
                     "surface ID found = %s\n",
                     srfstr[i],
                     bodstr[i],
                     tf[found]                 );

            if ( found )
            {
               printf ( "surface ID       = %d\n", surfid );
            }
            printf ( "\n" );
         }
         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      surface string   = MGS MOLA  64 pixel/deg
      body string      = MARS
      surface ID found = true
      surface ID       = 1

      surface string   = PHOBOS GASKELL Q512
      body string      = PHOBOS
      surface ID found = true
      surface ID       = 1

      surface string   = MGS MOLA 128 pixel/deg
      body string      = MARS
      surface ID found = true
      surface ID       = 2

      surface string   = MGS MOLA  64 pixel/deg
      body string      = 499
      surface ID found = true
      surface ID       = 1

      surface string   = 1
      body string      = PHOBOS
      surface ID found = true
      surface ID       = 1

      surface string   = 2
      body string      = 499
      surface ID found = true
      surface ID       = 2

      surface string   = ZZZ
      body string      = MARS
      surface ID found = false

      surface string   = 1
      body string      = ZZZ
      surface ID found = false


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

   -CSPICE Version 1.0.1, 01-NOV-2021 (JDR) (NJB)

       Edited the header to comply with NAIF standard.

       Updated description of "srfstr" to indicate that any
       surface name alias may be used.

   -CSPICE Version 1.0.0, 22-JAN-2016 (NJB) (BVS) (EDW)

-Index_Entries

   surface string and body string to surface ID code

-&
*/

{ /* Begin srfs2c_c */


   /*
   Local variables
   */
   logical                 fnd;


   /*
   Participate in error tracing.
   */
   chkin_c ( "srfs2c_c" );

   /*
   Check the input strings to make sure the pointers are non-null and
   the string lengths are non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "srfs2c_c", srfstr );
   CHKFSTR ( CHK_STANDARD, "srfs2c_c", bodstr );

   /*
   Hand off the work to the f2c'd routine.
   */
   srfs2c_ ( (char       *) srfstr,
             (char       *) bodstr,
             (integer    *) code,
             (logical    *) &fnd,
             (ftnlen      ) strlen(srfstr),
             (ftnlen      ) strlen(bodstr)  );

   /*
   Assign output SpiceBoolean found flag.
   */
   *found = fnd;

   chkout_c ( "srfs2c_c" );

} /* End srfs2c_c */
