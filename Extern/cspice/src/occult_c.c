/*

-Procedure occult_c ( find occultation type at time )

-Abstract

   Determine the occultation condition (not occulted, partially
   occulted, etc.) of one target relative to another target as seen
   by an observer at a given time.

   The surfaces of the target bodies may be represented by triaxial
   ellipsoids, points, or by topographic data provided by DSK files.

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

   KERNEL
   SPK
   TIME

-Keywords

   ELLIPSOID
   GEOMETRY
   OCCULTATION

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZmc.h"
   #include <stdio.h>

   void occult_c ( ConstSpiceChar   * targ1,
                   ConstSpiceChar   * shape1,
                   ConstSpiceChar   * frame1,
                   ConstSpiceChar   * targ2,
                   ConstSpiceChar   * shape2,
                   ConstSpiceChar   * frame2,
                   ConstSpiceChar   * abcorr,
                   ConstSpiceChar   * obsrvr,
                   SpiceDouble        et,
                   SpiceInt         * ocltid )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  -------------------------------------------
   targ1      I   Name or ID of first target.
   shape1     I   Type of shape model used for first target.
   frame1     I   Body-fixed, body-centered frame for first body.
   targ2      I   Name or ID of second target.
   shape2     I   Type of shape model used for second target.
   frame2     I   Body-fixed, body-centered frame for second body.
   abcorr     I   Aberration correction flag.
   obsrvr     I   Name or ID of the observer.
   et         I   Time of the observation (seconds past J2000).
   ocltid     O   Occultation identification code.

-Detailed_Input

   targ1       is the name of the first target body. Both object
               names and NAIF IDs are accepted. For example, both
               "Moon" and "301" are accepted.

   shape1      is a string indicating the geometric model used to
               represent the shape of the first target body. The
               supported options are:

                  "ELLIPSOID"

                     Use a triaxial ellipsoid model with radius values
                     provided via the kernel pool. A kernel variable
                     having a name of the form

                        "BODYnnn_RADII"

                     where nnn represents the NAIF integer code
                     associated with the body, must be present in the
                     kernel pool. This variable must be associated with
                     three numeric values giving the lengths of the
                     ellipsoid's X, Y, and Z semi-axes.

                  "POINT"

                     Treat the body as a single point. When a point
                     target is specified, the occultation conditions
                     can only be total, annular, or none.

                  "DSK/UNPRIORITIZED[/SURFACES = <surface list>]"

                      Use topographic data provided by DSK files to
                      model the body's shape. These data must be
                      provided by loaded DSK files.

                      The surface list specification is optional. The
                      syntax of the list is

                         <surface 1> [, <surface 2>...]

                      If present, it indicates that data only for the
                      listed surfaces are to be used; however, data
                      need not be available for all surfaces in the
                      list. If absent, loaded DSK data for any surface
                      associated with the target body are used.

                      The surface list may contain surface names or
                      surface ID codes. Names containing blanks must
                      be delimited by double quotes, for example

                         SURFACES = "Mars MEGDR 128 PIXEL/DEG"

                      If multiple surfaces are specified, their names
                      or IDs must be separated by commas.

                      See the -Particulars section below for details
                      concerning use of DSK data.

               The combinations of the shapes of the target bodies
               `targ1' and `targ2' must be one of:

                  One ELLIPSOID, one POINT
                  Two ELLIPSOIDs
                  One DSK, one POINT

               Case and leading or trailing blanks are not
               significant in the string `shape1'.


   frame1      is the name of the body-fixed, body-centered reference
               frame associated with the first target body. Examples
               of such names are "IAU_SATURN" (for Saturn) and
               "ITRF93" (for the Earth).

               If the first target body is modeled as a point, `frame1'
               should be left blank (Ex: " ").

               Case and leading or trailing blanks bracketing a
               non-blank frame name are not significant in the string.


   targ2       is the name of the second target body. See the description
               of `targ1' above for more details.


   shape2      is the shape specification for the body designated
               by `targ2'. See the description of `shape1' above for
               details.


   frame2      is the name of the body-fixed, body-centered reference
               frame associated with the second target body. See the
               description of `frame1' above for more details.


   abcorr      indicates the aberration corrections to be applied to
               the state of each target body to account for one-way
               light time. Stellar aberration corrections are
               ignored if specified, since these corrections don't
               improve the accuracy of the occultation determination.

               See the header of the SPICE routine spkezr_c for a
               detailed description of the aberration correction
               options. For convenience, the options supported by
               this routine are listed below:

                  "NONE"     Apply no correction.

                  "LT"       "Reception" case: correct for
                             one-way light time using a Newtonian
                             formulation.

                  "CN"       "Reception" case: converged
                             Newtonian light time correction.

                  "XLT"      "Transmission" case: correct for
                             one-way light time using a Newtonian
                             formulation.

                  "XCN"      "Transmission" case: converged
                             Newtonian light time correction.

               Case and blanks are not significant in the string
               `abcorr'.


   obsrvr      is the name of the body from which the occultation
               is observed. See the description of `targ1' above for
               more details.


   et          is the observation time in seconds past the J2000
               epoch.

-Detailed_Output

   ocltid      is an integer occultation code indicating the geometric
               relationship of the three bodies.

                  The meaning of the sign of `ocltid' is given below.

                      Code sign          Meaning
                      ---------          ------------------------------
                         > 0             The second ellipsoid is
                                         partially or fully occulted
                                         by the first.

                         < 0             The first ellipsoid is
                                         partially of fully
                                         occulted by the second.

                         = 0             No occultation.

               Possible `ocltid' values and meanings are given below.
               The variable names indicate the type of occultation and
               which target is in the back. For example,
               SPICE_OCCULT_TOTAL1 represents a total occultation in
               which the first target is in the back (or occulted by)
               the second target.

                  Name                Code    Meaning
                  ------              -----   ------------------------------
                  SPICE_OCCULT_TOTAL1  -3     Total occultation of first
                                              target by second.

                  SPICE_OCCULT_ANNLR1  -2     Annular occultation of first
                                              target by second. The second
                                              target does not block the limb
                                              of the first.

                  SPICE_OCCULT_PARTL1  -1     Partial occultation of first
                                              target by second target.

                  SPICE_OCCULT_NOOCC    0     No occultation or transit:
                                              both objects are completely
                                              visible to the observer.

                  SPICE_OCCULT_PARTL2   1     Partial occultation of second
                                              target by first target.

                  SPICE_OCCULT_ANNLR2   2     Annular occultation of
                                              second target by first.

                  SPICE_OCCULT_TOTAL2   3     Total occultation of second
                                              target by first.

-Parameters

   None.

-Exceptions

   1)  If the target or observer body names input by the user are
       not recognized, an error is signaled by a routine in
       the call tree of this routine.

   2)  If the input shapes are not accepted, an error is signaled by
       a routine in the call tree of this routine.

   3)  If both input shapes are points, an error is signaled by a
       routine in the call tree of this routine.

   4)  If the radii of a target body modeled as an ellipsoid cannot
       be determined by searching the kernel pool for a kernel
       variable having a name of the form

          "BODYnnn_RADII"

       where nnn represents the NAIF integer code associated with
       the body, an error is signaled by a routine in the
       call tree of this routine.

   5)  If any of the target or observer bodies (`targ1', `targ2', or
       `obsrvr') are the same, an error is signaled by a routine in the
       call tree of this routine.

   6)  If the loaded kernels provide insufficient data to compute any
       required state vector, an error is signaled by a routine in
       the call tree of this routine.

   7)  If an error occurs while reading an SPK or other kernel,
       the error is signaled by a routine in the call tree
       of this routine.

   8)  If the aberration correction specification `abcorr' is invalid,
       an error is signaled by a routine in the call tree of this
       routine.

   9)  If either `shape1' or `shape2' specifies that the target surface
       is represented by DSK data, and no DSK files are loaded for
       the specified target, an error is signaled by a routine in
       the call tree of this routine.

   10) If either `shape1' or `shape2' specifies that the target surface
       is represented by DSK data, but the shape specification is
       invalid, an error is signaled by a routine in the call tree
       of this routine.

   11) If any of the `targ1', `shape1', `frame1', `targ2', `shape2',
       `frame2', `abcorr' or `obsrvr' input string pointers is null,
       the error SPICE(NULLPOINTER) is signaled.

   12) If any of the `targ1', `shape1', `frame1', `targ2', `shape2',
       `frame2', `abcorr' or `obsrvr' input strings has zero length,
       the error SPICE(EMPTYSTRING) is signaled.

-Files

   Appropriate SPICE kernels must be loaded by the calling program
   before this routine is called.

   The following data are required:

   -  SPK data: the calling application must load ephemeris data
      for the targets, source and observer that cover the time
      instant specified by the argument `et'. If aberration
      corrections are used, the states of the target bodies and of
      the observer relative to the solar system barycenter must be
      calculable from the available ephemeris data. Typically
      ephemeris data are made available by loading one or more SPK
      files via furnsh_c.

   -  PCK data: bodies modeled as triaxial ellipsoids must have
      semi-axis lengths provided by variables in the kernel pool.
      Typically these data are made available by loading a text
      PCK file via furnsh_c.

   -  FK data: if either of the reference frames designated by
      `frame1' or `frame2' are not built in to the SPICE system,
      one or more FKs specifying these frames must be loaded.

   The following data may be required:

   -  DSK data: if either `shape1' or `shape2' indicates that DSK
      data are to be used, DSK files containing topographic data
      for the target body must be loaded. If a surface list is
      specified, data for at least one of the listed surfaces must
      be loaded.

   -  Surface name-ID associations: if surface names are specified
      in `shape1' or `shape2', the association of these names with
      their corresponding surface ID codes must be established by
      assignments of the kernel variables

         NAIF_SURFACE_NAME
         NAIF_SURFACE_CODE
         NAIF_SURFACE_BODY

      Normally these associations are made by loading a text
      kernel containing the necessary assignments. An example
      of such a set of assignments is

         NAIF_SURFACE_NAME += 'Mars MEGDR 128 PIXEL/DEG'
         NAIF_SURFACE_CODE += 1
         NAIF_SURFACE_BODY += 499

   -  CK data: either of the body-fixed frames to which `frame2' or
      `frame1' refer might be a CK frame. If so, at least one CK
      file will be needed to permit transformation of vectors
      between that frame and the J2000 frame.

   -  SCLK data: if a CK file is needed, an associated SCLK
      kernel is required to enable conversion between encoded SCLK
      (used to time-tag CK data) and barycentric dynamical time
      (TDB).

   Kernel data are normally loaded once per program run, NOT every
   time this routine is called.

-Particulars

   For many purposes, modeling extended bodies as triaxial
   ellipsoids is adequate for determining whether one body is
   occulted by another as seen from a specified observer.

   Using DSK data
   ==============

      DSK loading and unloading
      -------------------------

      DSK files providing data used by this routine are loaded by
      calling furnsh_c and can be unloaded by calling unload_c or
      kclear_c. See the documentation of furnsh_c for limits on numbers
      of loaded DSK files.

      For run-time efficiency, it's desirable to avoid frequent
      loading and unloading of DSK files. When there is a reason to
      use multiple versions of data for a given target body---for
      example, if topographic data at varying resolutions are to be
      used---the surface list can be used to select DSK data to be
      used for a given computation. It is not necessary to unload
      the data that are not to be used. This recommendation presumes
      that DSKs containing different versions of surface data for a
      given body have different surface ID codes.


      DSK data priority
      -----------------

      A DSK coverage overlap occurs when two segments in loaded DSK
      files cover part or all of the same domain---for example, a
      given longitude-latitude rectangle---and when the time
      intervals of the segments overlap as well.

      When DSK data selection is prioritized, in case of a coverage
      overlap, if the two competing segments are in different DSK
      files, the segment in the DSK file loaded last takes
      precedence. If the two segments are in the same file, the
      segment located closer to the end of the file takes
      precedence.

      When DSK data selection is unprioritized, data from competing
      segments are combined. For example, if two competing segments
      both represent a surface as a set of triangular plates, the
      union of those sets of plates is considered to represent the
      surface.

      Currently only unprioritized data selection is supported.
      Because prioritized data selection may be the default behavior
      in a later version of the routine, the UNPRIORITIZED keyword is
      required in the `shape1' and `shape2' arguments.


      Syntax of the shape input arguments for the DSK case
      ----------------------------------------------------

      The keywords and surface list in the target shape arguments
      `shape1' and `shape2' are called "clauses." The clauses may
      appear in any order, for example

         "DSK/<surface list>/UNPRIORITIZED"
         "DSK/UNPRIORITIZED/<surface list>"
         "UNPRIORITIZED/<surface list>/DSK"

      The simplest form of the `method' argument specifying use of
      DSK data is one that lacks a surface list, for example:

         "DSK/UNPRIORITIZED"

      For applications in which all loaded DSK data for the target
      body are for a single surface, and there are no competing
      segments, the above string suffices. This is expected to be
      the usual case.

      When, for the specified target body, there are loaded DSK
      files providing data for multiple surfaces for that body, the
      surfaces to be used by this routine for a given call must be
      specified in a surface list, unless data from all of the
      surfaces are to be used together.

      The surface list consists of the string

         "SURFACES = "

      followed by a comma-separated list of one or more surface
      identifiers. The identifiers may be names or integer codes in
      string format. For example, suppose we have the surface
      names and corresponding ID codes shown below:

         Surface Name                              ID code
         ------------                              -------
         "Mars MEGDR 128 PIXEL/DEG"                1
         "Mars MEGDR 64 PIXEL/DEG"                 2
         "Mars_MRO_HIRISE"                         3

      If data for all of the above surfaces are loaded, then
      data for surface 1 can be specified by either

         "SURFACES = 1"

      or

         "SURFACES = \"Mars MEGDR 128 PIXEL/DEG\""

      Escaped double quotes are used to delimit the surface name because
      it contains blank characters.

      To use data for surfaces 2 and 3 together, any
      of the following surface lists could be used:

         "SURFACES = 2, 3"

         "SURFACES = \"Mars MEGDR  64 PIXEL/DEG\", 3"

         "SURFACES = 2, Mars_MRO_HIRISE"

         "SURFACES = \"Mars MEGDR 64 PIXEL/DEG\", Mars_MRO_HIRISE"

      An example of a shape argument that could be constructed
      using one of the surface lists above is

         "DSK/UNPRIORITIZED/SURFACES = \"Mars MEGDR 64 PIXEL/DEG\", 3"

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Find whether MRO is occulted by Mars as seen by
      the DSS-13 ground station at a few specific
      times.

      Use the meta-kernel shown below to load the required SPICE
      kernels.


         KPL/MK

         File: occult_ex1.tm

         This meta-kernel is intended to support operation of SPICE
         example programs. The kernels shown here should not be
         assumed to contain adequate or correct versions of data
         required by SPICE-based user applications.

         In order for an application to use this meta-kernel, the
         kernels referenced here must be present in the user's
         current working directory.

         The names and contents of the kernels referenced
         by this meta-kernel are as follows:

            File name                        Contents
            ---------                        --------
            de410.bsp                        Planetary ephemeris
            mar063.bsp                       Mars satellite ephemeris
            pck00010.tpc                     Planet orientation and
                                             radii
            naif0011.tls                     Leapseconds
            earth_latest_high_prec.bpc       Earth latest binary PCK
            earthstns_itrf93_050714.bsp      DSN station SPK
            mro_psp35.bsp                    MRO ephemeris
            megr90n000cb_plate.bds           Plate model based on
                                             MEGDR DEM, resolution
                                             4 pixels/degree.

         \begindata

            KERNELS_TO_LOAD = ( 'de410.bsp',
                                'mar063.bsp',
                                'mro_psp34.bsp',
                                'earthstns_itrf93_050714.bsp',
                                'earth_latest_high_prec.bpc',
                                'pck00010.tpc',
                                'naif0011.tls',
                                'megr90n000cb_plate.bds'
                              )
         \begintext

         End of meta-kernel.


      Example code begins here.


      /.
         Program occult_ex1
      ./

      #include <stdio.h>
      #include "SpiceUsr.h"

      int main()
      {
         /.
         Constants

         `abcorr' is the desired light time and stellar
         aberration correction setting.

         METAKR is the name of the meta-kernel.
         ./
         #define ABCORR "CN"
         #define METAKR "occult_ex1.tm"
         #define TIMLEN  41

         /.
         Local variables
         ./
         SpiceChar             * abcorr      = "CN";
         SpiceChar             * time_format = "YYYY-MON-DD HR:MN ::UTC-8";
         SpiceChar             * obsrvr      = "DSS-13";
         SpiceChar             * out_format  = "%s %s %s %s wrt %s\n";
         SpiceChar             * shape1      = "point";
         SpiceChar             * shape2;
         SpiceChar             * shapes[2]   = { "ELLIPSOID",
                                                 "DSK/UNPRIORITIZED" };
         SpiceChar             * targ1       = "MRO ";
         SpiceChar             * targ2       = "Mars";
         SpiceChar               time[TIMLEN];

         static SpiceChar      * ouputStr[4] =
                                 {
                                    "totally occulted by  ",
                                    "transited by         ",
                                    "partially occulted by",
                                    "not occulted by      "
                                 };

         SpiceDouble             et;
         SpiceDouble             et1;
         SpiceDouble             et2;

         SpiceInt                dt      = 1000;
         SpiceInt                i;
         SpiceInt                ocltid;

         /.
         Load kernel files via the meta-kernel.
         ./
         furnsh_c ( METAKR );

         /.
         Calculate the type of occultation that
         corresponds to time ET.
         ./
         str2et_c ( "2015-FEB-28 1:15:00 UTC", &et1 );
         str2et_c ( "2015-FEB-28 2:50:00 UTC", &et2  );

         for ( i = 0;  i < 2;  i++ )
         {
            /.
            Set the planet shape model for this pass.
            ./
            shape2 = shapes[i];

            printf ( "\nMars shape: %s\n\n", shape2 );

            et     = et1;

            while ( et < et2 ) {

               /.
               Calculate the type of occultation that
               corresponds to time ET.
               ./
               occult_c ( targ1, shape1,  " ",
                          targ2, shape2,  "IAU_MARS",
                          abcorr,  obsrvr, et, &ocltid );

               /.
               Output the results.
               ./
               timout_c ( et, time_format, TIMLEN, time );

               switch ( ocltid ) {

                  case SPICE_OCCULT_TOTAL1:

                     printf( out_format, time, targ1, ouputStr[0],
                                         targ2, obsrvr );
                     break;

                  case SPICE_OCCULT_ANNLR1:

                     printf( out_format, time, targ1, ouputStr[1],
                                         targ2, obsrvr );
                     break;

                  case SPICE_OCCULT_PARTL1:

                     printf( out_format, time, targ1, ouputStr[2],
                                         targ2, obsrvr );
                     break;

                  case SPICE_OCCULT_NOOCC:

                     printf( out_format, time, targ1, ouputStr[3],
                                         targ2, obsrvr );
                     break;

                  case SPICE_OCCULT_PARTL2:

                     printf( out_format, time, targ2, ouputStr[2],
                                         targ1, obsrvr );
                     break;

                  case SPICE_OCCULT_ANNLR2:

                     printf( out_format, time, targ2, ouputStr[1],
                                         targ1, obsrvr );
                     break;

                  case SPICE_OCCULT_TOTAL2:

                     printf( out_format, time, targ2, ouputStr[0],
                                         targ1, obsrvr );
                     break;

                  default:

                     printf( "Bad occultation code: %d", ocltid );
                     break;

               }
               /.
               Increment the time.
               ./
               et += dt;
            }
         }
         printf( "\n" );
         return 0;
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Mars shape: ELLIPSOID

      2015-FEB-27 17:15 Mars transited by          MRO  wrt DSS-13
      2015-FEB-27 17:31 MRO  not occulted by       Mars wrt DSS-13
      2015-FEB-27 17:48 MRO  totally occulted by   Mars wrt DSS-13
      2015-FEB-27 18:04 MRO  totally occulted by   Mars wrt DSS-13
      2015-FEB-27 18:21 MRO  not occulted by       Mars wrt DSS-13
      2015-FEB-27 18:38 Mars transited by          MRO  wrt DSS-13

      Mars shape: DSK/UNPRIORITIZED

      2015-FEB-27 17:15 Mars transited by          MRO  wrt DSS-13
      2015-FEB-27 17:31 MRO  not occulted by       Mars wrt DSS-13
      2015-FEB-27 17:48 MRO  totally occulted by   Mars wrt DSS-13
      2015-FEB-27 18:04 MRO  totally occulted by   Mars wrt DSS-13
      2015-FEB-27 18:21 MRO  not occulted by       Mars wrt DSS-13
      2015-FEB-27 18:38 Mars transited by          MRO  wrt DSS-13


-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   S.C. Krening        (JPL)

-Version

   -CSPICE Version 2.0.1, 01-NOV-2021 (JDR) (NJB)

       Edited the header to comply with NAIF standard. Extended
       -Abstract description.

       Updated example's meta-kernel and inserted code example that
       shows use of DSKs.

   -CSPICE Version 2.0.0, 29-FEB-2016 (NJB)

       Upgraded to support surfaces represented by DSKs. Renamed some
       arguments. Updated example program to show use of DSKs. Updated
       example meta-kernel. Corrected various comment typos.

   -CSPICE Version 1.0.0, 01-FEB-2012 (SCK) (NJB)

-Index_Entries

   occultation type at a specified time

-&
*/

{ /* Begin occult_c */

   /*
   Participate in error tracing.
   */
   if ( return_c() )
   {
      return;
   }
   chkin_c ( "occult_c" );

   /*
   Check the input strings to make sure the pointers are non-null
   and the string lengths are non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "occult_c", targ1  );
   CHKFSTR ( CHK_STANDARD, "occult_c", shape1 );
   CHKFSTR ( CHK_STANDARD, "occult_c", frame1 );
   CHKFSTR ( CHK_STANDARD, "occult_c", targ2  );
   CHKFSTR ( CHK_STANDARD, "occult_c", shape2 );
   CHKFSTR ( CHK_STANDARD, "occult_c", frame2 );
   CHKFSTR ( CHK_STANDARD, "occult_c", abcorr );
   CHKFSTR ( CHK_STANDARD, "occult_c", obsrvr );

   /*
   Call the f2c'd routine.
   */
   occult_ ( ( char       * ) targ1,
             ( char       * ) shape1,
             ( char       * ) frame1,
             ( char       * ) targ2,
             ( char       * ) shape2,
             ( char       * ) frame2,
             ( char       * ) abcorr,
             ( char       * ) obsrvr,
             ( doublereal * ) &et,
             ( integer    * ) ocltid,
             ( ftnlen       ) strlen(targ1),
             ( ftnlen       ) strlen(shape1),
             ( ftnlen       ) strlen(frame1),
             ( ftnlen       ) strlen(targ2),
             ( ftnlen       ) strlen(shape2),
             ( ftnlen       ) strlen(frame2),
             ( ftnlen       ) strlen(abcorr),
             ( ftnlen       ) strlen(obsrvr) );

   chkout_c ( "occult_c" );

}
