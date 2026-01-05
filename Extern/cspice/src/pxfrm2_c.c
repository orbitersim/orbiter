/*

-Procedure pxfrm2_c ( Position Transform Matrix, Different Epochs )

-Abstract

   Return the 3x3 matrix that transforms position vectors from one
   specified frame at a specified epoch to another specified
   frame at another specified epoch.

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
   TRANSFORM

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZmc.h"
   #include "SpiceZst.h"

   void pxfrm2_c ( ConstSpiceChar   * from,
                   ConstSpiceChar   * to,
                   SpiceDouble        etfrom,
                   SpiceDouble        etto,
                   SpiceDouble        rotate[3][3]     )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   from       I   Name of the frame to transform from.
   to         I   Name of the frame to transform to.
   etfrom     I   Evaluation time of `from' frame.
   etto       I   Evaluation time of `to' frame.
   rotate     O   A position transformation matrix from
                  frame `from' to frame `to'.

-Detailed_Input

   from        is the name of a reference frame recognized by
               CSPICE that corresponds to the input `etfrom'.


   to          is the name of a reference frame recognized by
               CSPICE that corresponds to the desired output
               at `etto'.


   etfrom      is the epoch in ephemeris seconds past the epoch
               of J2000 (TDB) corresponding to the `from' reference
               frame.


   etto        is the epoch in ephemeris seconds past the epoch
               of J2000 (TDB) that corresponds to the `to' reference
               frame.

-Detailed_Output

   rotate      is the transformation matrix that relates the reference
               frame `from' at epoch `etfrom' to the frame `to' at epoch
               `etto'.

               If (x, y, z) is a position relative to the reference
               frame `from' at time `etfrom' then the vector ( x', y',
               z') is the same position relative to the frame `to' at
               epoch `etto'. Here the vector ( x', y', z' ) is defined
               by the equation:

                  -   -       -        -     -  -
                 | x'  |     |          |   | x  |
                 | y'  |  =  |  rotate  |   | y  |
                 | z'  |     |          |   | z  |
                  -   -       -        -     -  -

-Parameters

   None.

-Exceptions

   1)  If sufficient information has not been supplied via loaded
       SPICE kernels to compute the transformation between the
       two frames, an error is signaled by a routine
       in the call tree of this routine.

   2)  If either frame `from' or `to' is not recognized, the error
       SPICE(UNKNOWNFRAME) is signaled by a routine in the call tree
       of this routine.

   3)  If any of the `from' or `to' input string pointers is null,
       the error SPICE(NULLPOINTER) is signaled.

   4)  If any of the `from' or `to' input strings has zero length,
       the error SPICE(EMPTYSTRING) is signaled.

-Files

   Appropriate kernels must be loaded by the calling program before
   this routine is called. Kernels that may be required include
   SPK files, PCK files, frame kernels, C-kernels, and SCLK kernels.

   Such kernel data are normally loaded once per program
   run, NOT every time this routine is called.

-Particulars

   pxfrm2_c is most commonly used to transform a position between
   time-dependent reference frames.

   For more examples of where to use pxfrm2_c, please see:

         sincpt_c
         surfpt_c
         subslr_c
         ilumin_c

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Suppose that MGS has taken a picture of Mars at time `etrec' with
      the MOC narrow angle camera. We want to know the latitude and
      longitude associated with two pixels projected to Mars'
      surface: the boresight and one along the boundary of the
      field of view (FOV). Due to light time, the photons taken in
      the picture left Mars at time `etemit', when Mars was at a
      different state than at time `etrec'.

      In order to solve this problem, we could use the sincpt_c
      routine for both pixels, but this would be slow. Instead, we
      will assume that the light time for each pixel is the same. We
      will call sincpt_c once to get the light time and surface point
      associated with the boresight. Then, we will rotate one of the
      FOV boundary vectors from the camera frame at `etrec' to the
      body-fixed Mars frame at `etemit', and call the faster routine
      surfpt_c to retrieve the surface point for one of the FOV
      boundary vectors.

      This example problem could be extended to find the latitude
      and longitude associated with every pixel in an instrument's
      field of view, but this example is simplified to only solve
      for two pixels: the boresight and one along the boundary of
      the field of view.

      Assumptions:

         1)  The light times from the surface points in the camera's
             field of view to the camera are equal.

         2)  The camera offset from the center of gravity of the
             spacecraft is zero. If the data are more accurate
             and precise, this assumption can be easily discarded.

         3)  An ellipsoid shape model for the target body is
             sufficient.

         4)  The boundary field of view vector returned from getfov_c
             is associated with a boundary field of view pixel. If
             this example were extended to include a geometric camera
             model, this assumption would not be needed since the
             direction vectors associated with each pixel would be
             calculated from the geometric camera model.

      Use the meta-kernel shown below to load the required SPICE
      kernels.


         KPL/MK

         File name: pxfrm2_ex1.tm

         This is the meta-kernel file for the example problem for
         the subroutine PXFRM2. These kernel files can be found in
         the NAIF archives.

         In order for an application to use this meta-kernel, the
         kernels referenced here must be present in the user's
         current working directory.

         The names and contents of the kernels referenced
         by this meta-kernel are as follows:

            File name                     Contents
            ---------                     --------
            de421.bsp                     Planetary ephemeris
            pck00009.tpc                  Planet orientation and
                                          radii
            naif0009.tls                  Leapseconds
            mgs_ext12_ipng_mgs95j.bsp     MGS ephemeris
            mgs_moc_v20.ti                MGS MOC instrument
                                          parameters
            mgs_sclkscet_00061.tsc        MGS SCLK coefficients
            mgs_sc_ext12.bc               MGS s/c bus attitude

         \begindata

         KERNELS_TO_LOAD = ( 'de421.bsp',
                             'pck00009.tpc',
                             'naif0009.tls',
                             'mgs_ext12_ipng_mgs95j.bsp',
                             'mgs_moc_v20.ti',
                             'mgs_sclkscet_00061.tsc',
                             'mgs_sc_ext12.bc' )

         \begintext

         End of meta-kernel.


      Example code begins here.


      /.
         Program pxfrm2_ex1
      ./

      #include <stdio.h>
      #include "SpiceUsr.h"

      int main()
      {
         /.
         Constants

         ABCORR is the desired light time and stellar
         aberration correction setting.

         METAKR is the name of the meta-kernel.
         ./

         #define ABCORR "CN+S"
         #define METAKR "pxfrm2_ex1.tm"
         #define FRMNLN 32
         #define NCORNR 4
         #define SHPLEN 80

         /.
         Local variables
         ./
         SpiceBoolean            found;

         /.
         MGS_MOC_NA is the name of the camera that took
         the picture being analyzed.
         ./
         SpiceChar              *camera  = "MGS_MOC_NA";

         /.
         The variable `obsref' is the observer reference frame
         on MGS.
         ./
         SpiceChar               obsref [FRMNLN] ;
         SpiceChar               shape  [SHPLEN] ;

         SpiceDouble             bounds [NCORNR][3];
         SpiceDouble             bndvec [3];
         SpiceDouble             bsight [3];
         SpiceDouble             dist;

         /.
         The variable `etemit' is the time at which the photons were
         emitted from Mars, and `etrec' is the time at
         which the picture was taken by MGS.
         ./
         SpiceDouble             etemit;
         SpiceDouble             etrec;

         /.
         The variables `lat' and `lon' and the latitude and longitude
         associated with one of the boundary FOV vectors.
         ./
         SpiceDouble             lat;
         SpiceDouble             lon;

         /.
         The variable `pmgsmr' is the opposite of the apparent
         position of Mars with respect to MGS.
         ./
         SpiceDouble             pmgsmr [3];

         /.
         The variable `radii' is a vector of the semi-axes of Mars.
         ./
         SpiceDouble             radii [3];
         SpiceDouble             radius;

         /.
         The variable `rotate' is a position transformation matrix
         from the camera frame at `etrec' to the IAU_MARS frame
         at `etemit'.
         ./
         SpiceDouble             rotate [3][3];
         SpiceDouble             spoint [3];
         SpiceDouble             srfvec [3];
         SpiceDouble             tmp [3];

         SpiceInt                camid;
         SpiceInt                dim;
         SpiceInt                n;


         /.  ------------------ Program Setup ------------------

         Load kernels.
         ./
         furnsh_c ( METAKR );

         /.
         Convert the time the picture was taken from a
         UTC time string to seconds past J2000, TDB.
         ./
         str2et_c ( "2003 OCT 13 06:00:00 UTC", &etrec );

         /.
         Assume the one-way light times from different
         surface points on Mars to MGS within the camera's
         FOV are equal. This means the photons that make
         up different pixels were all emitted from Mars at
         `etemit' and received by the MGS MOC camera at `etrec'. It
         would be slow to process images using `sincpt_c' for every
         pixel. Instead, we will use `sincpt_c' on the
         boresight pixel and use `surfpt_c' for one of the FOV
         boundary pixels. If this example program were extended
         to include all of the camera's pixels, `surfpt_c' would
         be used for the remaining pixels.

         Get the MGS MOC Narrow angle camera (MGS_MOC_NA)
         ID code. Then look up the field of view (FOV)
         parameters by calling `getfov_c'.
         ./

         bodn2c_c ( camera, &camid, &found );
         if ( !found  )
         {
            setmsg_c ("Could not find ID code for instrument #." );
            errch_c  ("#", camera );
            sigerr_c ("SPICE(NOTRANSLATION)");
         }

         /.
         `getfov_c' will return the name of the camera-fixed frame
         in the string `obsref', the camera boresight vector in
         the array `bsight', and the FOV corner vectors in the
         array `bounds'.
         ./

         getfov_c ( camid,  NCORNR, SHPLEN, FRMNLN, shape,
                    obsref, bsight, &n,     bounds        );

         printf( "Observation Reference Frame:  %s\n", obsref );

         /. ----------- Boresight Surface Intercept -----------

         Retrieve the time, surface intercept point, and vector
         from MGS to the boresight surface intercept point
         in IAU_MARS coordinates.
         ./
         sincpt_c ( "Ellipsoid", "Mars", etrec, "IAU_MARS",
                    ABCORR, "MGS",   obsref,
                    bsight,  spoint, &etemit, srfvec, &found );
         if ( !found  )
         {
            setmsg_c("Intercept not found for the boresight vector.");
            sigerr_c("SPICE(NOINTERCEPT)");
         }

         /.
         Convert the intersection point of the boresight
         vector and Mars from rectangular into latitudinal
         coordinates. Convert radians to degrees.
         ./
         reclat_c ( spoint, &radius, &lon, &lat );

         lon *= dpr_c();
         lat *= dpr_c();

         printf( "Boresight surface intercept coordinates:\n"
                 "    Radius    (km) :  %f\n"
                 "    Latitude  (deg):  %f\n"
                 "    Longitude (deg):  %f\n",
                 radius, lat, lon );

         /.---- A Boundary FOV Surface Intercept (surfpt_c) -----

         Now we will transform one of the FOV corner vectors into the
         IAU_MARS frame so the surface intercept point can be
         calculated using surfpt_c, which is faster than subpnt_c.

         If this example program were extended to include all
         of the pixels in the camera's FOV, a few steps, such as
         finding the rotation matrix from the camera frame to the
         IAU_MARS frame, looking up the radii values for Mars,
         and finding the position of MGS with respect to Mars could
         be done once and used for every pixel.

         Find the rotation matrix from the ray's reference
         frame at the time the photons were received (etrec)
         to IAU_MARS at the time the photons were emitted
         (etemit).
         ./
         pxfrm2_c ( obsref, "IAU_MARS", etrec, etemit, rotate );

         /.
         Look up the radii values for Mars.
         ./
         bodvrd_c ( "MARS", "RADII", 3, &dim, radii );

         /.
         Find the position of the center of Mars with respect
         to MGS.  The position of the observer with respect
         to Mars is required for the call to surfpt_c.  Note:
         the apparent position of MGS with respect to Mars is
         not the same as the negative of Mars with respect to MGS.
         ./
         vsub_c ( spoint, srfvec, pmgsmr );

         /.
         The selected boundary FOV pixel must be rotated into the
         IAU_MARS reference frame.
         ./
         mxv_c ( rotate, bounds[0], bndvec );

         /.
         Calculate the surface point of the boundary FOV
         vector.
         ./
         surfpt_c ( pmgsmr, bndvec, radii[0], radii[1], radii[2],
                    spoint, &found );

         if ( !found  )
         {
            setmsg_c ("Could not calculate surface point.");
            sigerr_c ("SPICE(NOTFOUND)");
         }
         vequ_c ( spoint, tmp );

         /.
         Convert the intersection point of the boundary
         FOV vector and Mars from rectangular into
         latitudinal coordinates. Convert radians
         to degrees.
         ./
         reclat_c ( spoint, &radius, &lon, &lat );

         lon *= dpr_c();
         lat *= dpr_c();

         printf( "Boundary vector surface intercept coordinates "
                 "using surfpt_c:\n"
                 "    Radius    (km) :  %f\n"
                 "    Latitude  (deg):  %f\n"
                 "    Longitude (deg):  %f\n"
                 "    Emit time using boresight LT (s):  %10.8f\n",
                 radius, lat, lon, etemit);

         /. ---- A Boundary FOV Surface Intercept Verification ----

         For verification only, we will calculate the surface
         intercept coordinates for the selected boundary vector using
         sincpt_c and compare to the faster surfpt_c method.
         ./
         sincpt_c ( "Ellipsoid",    "Mars",   etrec, "IAU_MARS",
                    ABCORR, "MGS",  obsref,  bounds[0],
                    spoint, &etemit, srfvec, &found );

         if ( !found  )
         {
            setmsg_c("Intercept not found for the boresight vector.");
            sigerr_c("SPICE(NOINTERCEPT)");
         }

         /.
         Convert the intersection point of the selected boundary
         vector and Mars from rectangular into latitudinal
         coordinates. Convert radians to degrees.
         ./
         reclat_c ( spoint, &radius, &lon, &lat );

         lon *= dpr_c();
         lat *= dpr_c();

         printf( "Boundary vector surface intercept coordinates "
                 "using surfpt_c:\n"
                 "    Radius    (km) :  %f\n"
                 "    Latitude  (deg):  %f\n"
                 "    Longitude (deg):  %f\n"
                 "    Emit time using boundary LT (s):  %10.8f\n",
                 radius, lat, lon, etemit);

         /.
         We expect this to be a very small distance.
         ./
         dist = vdist_c ( tmp, spoint );

         printf( "Distance between surface points of the selected\n"
                 "boundary vector using surfpt_c and sincpt_c:\n"
                 "    Distance (mm):    %f\n", dist*1e+6            );

         return ( 0 );

      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Observation Reference Frame:  MGS_MOC_NA
      Boresight surface intercept coordinates:
          Radius    (km) :  3384.940410
          Latitude  (deg):  -48.479580
          Longitude (deg):  -123.436454
      Boundary vector surface intercept coordinates using surfpt_c:
          Radius    (km) :  3384.941136
          Latitude  (deg):  -48.477482
          Longitude (deg):  -123.474080
          Emit time using boresight LT (s):  119296864.18105948
      Boundary vector surface intercept coordinates using surfpt_c:
          Radius    (km) :  3384.941136
          Latitude  (deg):  -48.477482
          Longitude (deg):  -123.474079
          Emit time using boundary LT (s):  119296864.18105946
      Distance between surface points of the selected
      boundary vector using surfpt_c and sincpt_c:
          Distance (mm):    32.139880


-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   J. Diaz del Rio     (ODC Space)
   S.C. Krening        (JPL)
   W.L. Taber          (JPL)

-Version

   -CSPICE Version 1.0.1, 13-AUG-2021 (JDR)

       Updated -Exceptions section with description of missing
       exceptions and its format to comply with NAIF standard.

       Edited the header to comply with NAIF standard.

       Fixed reference to CSPICE API in code example output. Changed
       boundary vector used in code example for consistency with other
       examples.

   -CSPICE Version 1.0.0, 01-FEB-2012 (SCK) (WLT)

-Index_Entries

   Position transformation matrix for different epochs

-&
*/

{ /* Begin pxfrm2_c */

   /*
   Static local variables
   */

   /*
   Local variables
   */

   /*
   Participate in error tracing.
   */
   if ( return_c() )
   {
      return;
   }
   chkin_c ( "pxfrm2_c" );

   /*
   Check the input strings to make sure the pointers are non-null
   and the string lengths are non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "pxfrm2_c", from );
   CHKFSTR ( CHK_STANDARD, "pxfrm2_c", to   );

   /*
   Call the f2c'd routine.
   */
   pxfrm2_ ( ( char       * ) from,
             ( char       * ) to,
             ( doublereal * ) &etfrom,
             ( doublereal * ) &etto,
             ( doublereal * ) rotate,
             ( ftnlen       ) strlen(from),
             ( ftnlen       ) strlen(to)    );

   /*
   Transpose the output to obtain row-major order.
   */
   xpose_c ( rotate, rotate );


   chkout_c ( "pxfrm2_c" );

} /* End pxfrm2_c */
