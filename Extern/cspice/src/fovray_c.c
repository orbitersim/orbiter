/*

-Procedure fovray_c ( Is ray in FOV at time? )

-Abstract

   Determine if a specified ray is within the field-of-view (FOV) of
   a specified instrument at a given time.

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

   CK
   FRAMES
   KERNEL
   NAIF_IDS
   PCK
   SPK
   TIME

-Keywords

   EVENT
   FOV
   GEOMETRY
   INSTRUMENT

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZmc.h"
   #undef fovray_c

   void fovray_c ( ConstSpiceChar   * inst,
                   ConstSpiceDouble   raydir [3],
                   ConstSpiceChar   * rframe,
                   ConstSpiceChar   * abcorr,
                   ConstSpiceChar   * obsrvr,
                   SpiceDouble      * et,
                   SpiceBoolean     * visibl  )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  -------------------------------------------------
   inst       I   Name or ID code string of the instrument.
   raydir     I   Ray's direction vector.
   rframe     I   Reference frame of ray's direction vector.
   abcorr     I   Aberration correction flag.
   obsrvr     I   Name or ID code string of the observer.
   et         I   Time of the observation (seconds past J2000).
   visibl     O   Visibility flag (SPICETRUE/SPICEFALSE).

-Detailed_Input

   inst        indicates the name of an instrument, such as a
               spacecraft-mounted framing camera. The field of view
               (FOV) of the instrument will be used to determine if
               the direction from the observer to a target,
               represented as a ray, is visible with respect to the
               instrument.

               The position of the instrument `inst' is considered to
               coincide with that of the ephemeris object `obsrvr' (see
               description below).

               The size of the instrument's FOV is constrained by the
               following: There must be a vector A such that all of
               the instrument's FOV boundary vectors have an angular
               separation from A of less than (pi/2)-SPICE_GF_MARGIN radians
               (see description below). For FOVs that are circular or
               elliptical, the vector A is the boresight. For FOVs
               that are rectangular or polygonal, the vector A is
               calculated.

               See the header of the CSPICE routine getfov_c for a
               description of the required parameters associated with
               an instrument.

               Both object names and NAIF IDs are accepted. For
               example, both "CASSINI_ISS_NAC" and "-82360" are
               accepted. Case and leading or trailing blanks are not
               significant in the string.

   raydir      is the direction vector defining a ray of interest.
               The ray emanates from the location of the ephemeris
               object designated by the input argument `obsrvr' and
               is expressed relative to the reference frame
               designated by `rframe' (see description below).

   rframe      is the name of the reference frame associated with
               the input ray's direction vector `raydir'. Note: `rframe'
               does not need to be the instrument's reference frame.

               Since light time corrections are not supported for
               rays, the orientation of the frame is always evaluated
               at the epoch associated with the observer, as opposed
               to the epoch associated with the light-time corrected
               position of the frame center.

               Case, leading and trailing blanks are not significant
               in the string.

   abcorr      indicates the aberration corrections to be applied
               when computing the ray's direction.

               The supported aberration correction options are:

                  "NONE"          No correction.
                  "S"             Stellar aberration correction,
                                  reception case.
                  "XS"            Stellar aberration correction,
                                  transmission case.

               For detailed information, see the geometry finder
               required reading, gf.req.

               Case, leading and trailing blanks are not significant
               in the string.

   obsrvr      is the name of the body from which the target
               represented by `raydir' is observed. The instrument
               designated by `inst' is treated as if it were co-located
               with the observer.

               Both object names and NAIF IDs are accepted. For
               example, both "CASSINI" and "-82" are accepted. Case
               and leading or trailing blanks are not significant in
               the string.

   et          is the observation time in seconds past the J2000
               epoch.

-Detailed_Output

   visibl      is SPICETRUE if the ray is "visible", or in the
               field-of-view, of `inst' at the time `et'. Otherwise,
               `visibl' is SPICEFALSE.

-Parameters

   SPICE_GF_MAXVRT   is the maximum number of vertices that may be used
                     to define the boundary of the specified instrument's
                     field of view.

   SPICE_GF_MARGIN   is a small positive number used to constrain the
                     orientation of the boundary vectors of polygonal
                     FOVs. Such FOVs must satisfy the following constraints:

                        1)  The boundary vectors must be contained within a
                            right circular cone of angular radius less than
                            than (pi/2) - SPICE_GF_MARGIN radians; in other
                            words, there must be a vector A such that all
                            boundary vectors have angular separation from A
                            of less than (pi/2)-SPICE_GF_MARGIN radians.

                        2)  There must be a pair of boundary vectors U, V
                            such that all other boundary vectors lie in the
                            same half space bounded by the plane containing
                            U and V. Furthermore, all other boundary vectors
                            must have orthogonal projections onto a specific
                            plane normal to this plane (the normal plane
                            contains the angle bisector defined by U and V)
                            such that the projections have angular
                            separation of at least 2*SPICE_GF_MARGIN radians
                            from the plane spanned by U and V.

                     SPICE_GF_MARGIN is currently set to 1.e-12.

   See header file SpiceGF.h for declarations and descriptions of
   parameters used throughout the GF system.

-Exceptions

   1)  If the observer's name cannot be mapped to a NAIF ID code, the
       error SPICE(IDCODENOTFOUND) is signaled by a routine in the
       call tree of this routine.

   2)  If the aberration correction flag calls for light time
       correction, the error SPICE(INVALIDOPTION) is signaled by a
       routine in the call tree of this routine.

   3)  If the ray's direction vector is zero, the error
       SPICE(ZEROVECTOR) is signaled by a routine in the call tree of
       this routine.

   4)  If the instrument name `inst' does not have corresponding NAIF
       ID code, an error is signaled by a routine in the call
       tree of this routine.

   5)  If the FOV parameters of the instrument are not present in
       the kernel pool, an error is signaled by a routine
       in the call tree of this routine.

   6)  If the FOV boundary has more than SPICE_GF_MAXVRT vertices, an error
       is signaled by a routine in the call tree of this
       routine.

   7)  If the instrument FOV shape is a polygon or rectangle, and
       this routine cannot find a ray R emanating from the FOV vertex
       such that maximum angular separation of R and any FOV boundary
       vector is within the limit (pi/2)-SPICE_GF_MARGIN radians, an error
       is signaled by a routine in the call tree of this routine. If the
       FOV is any other shape, the same error check will be applied
       with the instrument boresight vector serving the role of R.

   8)  If the loaded kernels provide insufficient data to compute a
       requested state vector, an error is signaled by a
       routine in the call tree of this routine.

   9)  If an error occurs while reading an SPK or other kernel file,
       the error is signaled by a routine in the call tree
       of this routine.

   10) If any of the `inst', `abcorr', `obsrvr' or `rframe' input
       string pointers is null, the error SPICE(NULLPOINTER) is
       signaled.

   11) If any of the `inst', `abcorr' or `obsrvr' input strings has
       zero length, the error SPICE(EMPTYSTRING) is signaled.

-Files

   Appropriate SPICE kernels must be loaded by the calling program
   before this routine is called.

   The following data are required:

   -  SPK data: ephemeris data for the observer at the time
      `et'. If aberration corrections are used, the state of the
      observer relative to the solar system barycenter
      must be calculable from the available ephemeris data.

   -  Data defining the reference frame in which the instrument's
      FOV is defined must be available in the kernel pool.
      Additionally the name `inst' must be associated with an ID
      code.

   -  IK data: the kernel pool must contain data such that
      the CSPICE routine getfov_c may be called to obtain
      parameters for `inst'.

   The following data may be required:

   -  CK data: if the frame in which the instrument's FOV is
      defined is fixed to a spacecraft, at least one CK file will
      be needed to permit transformation of vectors between that
      frame and the J2000 frame.

   -  SCLK data: if a CK file is needed, an associated SCLK
      kernel is required to enable conversion between encoded SCLK
      (used to time-tag CK data) and barycentric dynamical time
      (TDB).

   -  Since the input ray direction may be expressed in any
      frame, additional FKs, CKs, SCLK kernels, PCKs, and SPKs
      may be required to map the direction to the J2000 frame.

   Kernel data are normally loaded via furnsh_c once per program run,
   NOT every time this routine is called.

-Particulars

   To treat the target as an ephemeris object rather than a ray, use
   the higher-level CSPICE routine fovtrg_c. fovtrg_c may be used to
   determine if ephemeris objects such as Saturn are visible in an
   instrument's FOV at a given time.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) The Cassini Ultraviolet Imaging Spectrograph (UVIS)
      has been used to measure variations in starlight as
      rings and moons occult Cassini's view of the stars.
      One of these events happened at 2008-054T21:31:55.158 UTC.
      Let's verify that Epsilon CMa (Adhara) was in the
      Cassini UVIS field-of-view at the observation time.

      Use the meta-kernel shown below to load the required SPICE
      kernels.


         KPL/MK

         File name: fovray_ex1.tm

         This meta-kernel is intended to support operation of SPICE
         example programs. The kernels shown here should not be
         assumed to contain adequate or correct versions of data
         required by SPICE-based user applications.

         In order for an application to use this meta-kernel, the
         kernels referenced here must be present in the user's
         current working directory.

         The names and contents of the kernels referenced
         by this meta-kernel are as follows:

           File name                      Contents
           ---------                      --------
           naif0010.tls                   Leapseconds
           cpck26Jan2007.tpc              Satellite orientation and
                                          radii
           cas00145.tsc                   Cassini SCLK
           cas_v40.tf                     Cassini frames
           cas_uvis_v06.ti                Cassini UVIS instrument
           080428R_SCPSE_08045_08067.bsp  Merged spacecraft,
                                          planetary, and satellite
                                          ephemeris
           08052_08057ra.bc               Orientation for Cassini

         \begindata

           KERNELS_TO_LOAD = ( 'cpck26Jan2007.tpc'
                               'naif0010.tls'
                               'cas00145.tsc'
                               'cas_v40.tf'
                               'cas_uvis_v06.ti'
                               '080428R_SCPSE_08045_08067.bsp'
                               '08052_08057ra.bc')

         \begintext

         End of meta-kernel


      Example code begins here.


      /.
         Program fovray_ex1
      ./

      #include <stdio.h>
      #include "SpiceUsr.h"
      #include "SpiceZmc.h"

      int main()
      {

         /.
         Local parameters
         ./
         #define META   "fovray_ex1.tm"
         #define BODLEN 32
         #define TIMLEN 32
         #define FRMLEN 32
         #define TIMFMT "YYYY-MON-DD HR:MN:SC.###::TDB (TDB)"

         /.
         This is the UTC time of the observation.
         ./
         #define TIME   "2008-054T21:31:55.158"


         /.
         Local variables
         ./
         SpiceChar               timstr[TIMLEN];

         SpiceDouble             dec;
         SpiceDouble             et;
         SpiceDouble             ra;
         SpiceDouble             raydir [3];

         SpiceBoolean            visibl;

         /.
         The variables `ra' and `dec' are the right ascension
         and declination of Epsilon CMa in degrees.
         ./
         ra  = 104.656;
         dec = -28.972;

         /.
         Load kernels.
         ./
         furnsh_c ( META );

         /.
         Convert the observation time to `et'.
         ./
         str2et_c ( TIME, &et );

         /.
         Create a unit direction vector pointing from Cassini
         to the specified star. For details on corrections such
         as parallax, please see the example in gfrfov_c.
         ./
         radrec_c ( 1.0, ra*rpd_c(), dec*rpd_c(), raydir );

         /.
         Is the star in the field-of-view of Cassini's UVIS?
         ./
         fovray_c ( "CASSINI_UVIS_FUV_OCC", raydir, "J2000",
                    "S", "CASSINI", &et, &visibl );

         /.
         Put the time in a specified format for output and
         report the result.
         ./
         timout_c ( et, TIMFMT, TIMLEN, timstr );

         if ( visibl )
         {
            printf ( "Epsilon CMa was visible from the Cassini\n" );
            printf ( "UVIS instrument at %s\n", timstr );
         }

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Epsilon CMa was visible from the Cassini
      UVIS instrument at 2008-FEB-23 21:33:00.343 (TDB)


-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   S.C. Krening        (JPL)

-Version

   -CSPICE Version 1.0.1, 02-AUG-2021 (JDR)

       Edited header to comply with NAIF standard. Changed the
       parameter name "MARGIN" to "SPICE_GF_MARGIN".

       Changed the argument names 'observer' and 'visible' to 'obsrvr'
       and 'visibl' for consistency with other routines.

       Changed parameter and variable names in code example to comply
       with NAIF standard.

   -CSPICE Version 1.0.0, 15-FEB-2012 (SCK) (NJB)

-Index_Entries

   Ray in instrument FOV at specified time
   Ray in instrument field_of_view at specified time

-&
*/

{ /* Begin fovray_c */

   /*
   Local variables
   */
   SpiceChar               * rFrameStr;

   /*
   Static variables
   */
   static const SpiceChar  * blankStr = " ";

   /*
   Participate in error tracing.
   */
   if ( return_c() )
   {
      return;
   }
   chkin_c ( "fovray_c" );

   /*
   Check the input strings to make sure the pointers are non-null
   and the string lengths are non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "fovray_c", inst   );
   CHKFSTR ( CHK_STANDARD, "fovray_c", abcorr );
   CHKFSTR ( CHK_STANDARD, "fovray_c", obsrvr );

   /*
   The input frame name is a special case because we allow the caller
   to pass in an empty string. If this string is empty,
   we pass a null-terminated string containing one blank character to
   the underlying f2c'd routine.

   First make sure the frame name pointer is non-null.
   */
   CHKPTR ( CHK_STANDARD, "fovray_c", rframe );

   /*
   Use the input frame string if it's non-empty; otherwise
   use a blank string for the frame name.
   */

   if ( rframe[0] )
   {
      rFrameStr = (SpiceChar *) rframe;
   }
   else
   {
      rFrameStr = (SpiceChar *) blankStr;
   }

   /*
   Call the f2c'd Fortran routine. Use explicit type casts for every
   type defined by f2c.
   */
   fovray_ ( (char         *) inst,
             (doublereal   *) raydir,
             (char         *) rFrameStr,
             (char         *) abcorr,
             (char         *) obsrvr,
             (doublereal   *) et,
             (logical      *) visibl,
             (ftnlen        ) strlen(inst),
             (ftnlen        ) strlen(rframe),
             (ftnlen        ) strlen(abcorr),
             (ftnlen        ) strlen(obsrvr)  );

   chkout_c ( "fovray_c" );


} /* End fovray_c */
