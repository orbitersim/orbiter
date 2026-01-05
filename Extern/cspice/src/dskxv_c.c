/*

-Procedure dskxv_c ( DSK, ray-surface intercept, vectorized )

-Abstract

   Compute ray-surface intercepts for a set of rays, using data
   provided by multiple loaded DSK segments.

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
   DSK
   FRAMES
   PCK
   SPK
   TIME

-Keywords

   GEOMETRY
   INTERCEPT
   SURFACE
   TOPOGRAPHY

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZst.h"
   #include "SpiceZmc.h"
   #include "zzalloc.h"
   #undef dskxv_c


   void dskxv_c ( SpiceBoolean         pri,
                  ConstSpiceChar     * target,
                  SpiceInt             nsurf,
                  ConstSpiceInt        srflst[],
                  SpiceDouble          et,
                  ConstSpiceChar     * fixref,
                  SpiceInt             nrays,
                  ConstSpiceDouble     vtxarr[][3],
                  ConstSpiceDouble     dirarr[][3],
                  SpiceDouble          xptarr[][3],
                  SpiceBoolean         fndarr[]     )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   pri        I   Data prioritization flag.
   target     I   Target body name.
   nsurf      I   Number of surface IDs in list.
   srflst     I   Surface ID list.
   et         I   Epoch, expressed as seconds past J2000 TDB.
   fixref     I   Name of target body-fixed reference frame.
   nrays      I   Number of rays.
   vtxarr     I   Array of vertices of rays.
   dirarr     I   Array of direction vectors of rays.
   xptarr     O   Intercept point array.
   fndarr     O   Found flag array.

-Detailed_Input

   pri         is a logical flag indicating whether to perform
               a prioritized or unprioritized DSK segment search.
               In an unprioritized search, no segment masks another:
               data from all specified segments are used to
               define the surface of interest.

               The search is unprioritized if and only if `pri'
               is set to SPICEFALSE. In the N0066 SPICE Toolkit, this
               is the only allowed value.

   target      is the name of the target body on which a surface
               intercept is sought.

   nsurf,
   srflst      are, respectively, a count of surface ID codes in a
               list and the containing list. Only DSK segments for
               the body designated by `target' and having surface
               IDs in this list will considered in the intercept
               computation. If the list is empty, all DSK segments
               for `target' will be considered.

   et          is the epoch of the intersection computation,
               expressed as seconds past J2000 TDB. This epoch is
               used only for DSK segment selection. Segments used
               the intercept computation must include `et' in their
               time coverage intervals.

   fixref      is the name of a body-fixed, body-centered reference
               frame associated with the target. The input ray vectors
               are specified in this frame, as is the output intercept
               point.

               The frame designated by `fixref' must have a fixed
               orientation relative to the frame of any DSK segment
               used in the computation.

   nrays,
   vtxarr,
   dirarr      are, respectively, a count of rays, an array containing
               the vertices of rays, and an array containing the
               direction vectors of the rays.

               The ray's vertices are considered to represent offsets
               from the center of the target body.

               The rays' vertices and direction vectors are
               represented in the reference frame designated by
               `fixref'.

-Detailed_Output

   xptarr      is an array containing the intercepts of the input
               rays on the surface specified by the inputs

                  pri
                  target
                  nsurf
                  srflst
                  et

               The ith element of `xptarr' is the intercept
               corresponding to the ith ray, if such an intercept
               exists. If a ray intersects the surface at multiple
               points, the intercept closest to the ray's vertex is
               selected.

               The ith element of `xptarr' is defined if and only if the
               ith element of `fndarr' is SPICETRUE.

               Units are km.

   fndarr      is an array of logical flags indicating whether the
               input rays intersect the surface. The ith element of
               `fndarr' is set to SPICETRUE if and only if an intercept
               was found for the ith ray.

-Parameters

   See the header file

      SpiceDtl.h

   for the values of tolerance parameters used by default by the
   ray-surface intercept algorithm.

   These parameters are discussed in the -Particulars section
   below.

   See the header file

      SpiceDLA.h

   for declarations of DLA descriptor sizes and documentation of the
   contents of DLA descriptors.

   See the header file

      SpiceDSK.h

   for declarations of DSK descriptor sizes and documentation of the
   contents of DSK descriptors.

-Exceptions

   1)  If the input prioritization flag `pri' is set to SPICETRUE, the
       error SPICE(BADPRIORITYSPEC) is signaled by a routine in the
       call tree of this routine.

   2)  If `nrays' is less than 1, the error SPICE(INVALIDCOUNT)
       is signaled.

   3)  If `nsurf' is less than 0, the error SPICE(INVALIDCOUNT)
       is signaled by a routine in the call tree of this routine.

   4)  If the input body name `target' cannot be mapped to an ID code,
       the error SPICE(IDCODENOTFOUND) is signaled by a routine in
       the call tree of this routine.

   5)  If the input frame name `fixref' cannot be mapped to an ID code,
       the error SPICE(IDCODENOTFOUND) is signaled by a routine in
       the call tree of this routine.

   6)  If the frame center associated with `fixref' cannot be
       retrieved, the error SPICE(NOFRAMEINFO) is signaled by a
       routine in the call tree of this routine.

   7)  If the frame center associated with `fixref' is not the target
       body, the error SPICE(INVALIDFRAME) is signaled by a routine
       in the call tree of this routine.

   8)  If an error occurs during the intercept computation, the error
       is signaled by a routine in the call tree of this routine.

   9)  If any of the `target' or `fixref' input string pointers is
       null, the error SPICE(NULLPOINTER) is signaled.

   10) If any of the `target' or `fixref' input strings has zero
       length, the error SPICE(EMPTYSTRING) is signaled.

   11) If memory cannot be allocated to create the temporary variable
       required for the execution of the underlying Fortran routine,
       the error SPICE(MALLOCFAILED) is signaled.

-Files

   Appropriate kernels must be loaded by the calling program before
   this routine is called.

   The following data are required:

   -  SPK data: ephemeris data for the positions of the centers
      of DSK reference frames relative to the target body are
      required if those frames are not centered at the target
      body center.

      Typically ephemeris data are made available by loading one
      or more SPK files via furnsh_c.

   -  DSK data: DSK files containing topographic data for the
      target body must be loaded. If a surface list is specified,
      data for at least one of the listed surfaces must be loaded.

   -  Frame data: if a frame definition is required to convert
      DSK segment data to the body-fixed frame designated by
      `fixref', the target, that definition must be available in the
      kernel pool. Typically the definitions of frames not already
      built-in to SPICE are supplied by loading a frame kernel.

   -  CK data: if the frame to which `fixref' refers is a CK frame,
      and if any DSK segments used in the computation have a
      different frame, at least one CK file will be needed to
      permit transformation of vectors between that frame and both
      the J2000 and the target body-fixed frames.

   -  SCLK data: if a CK file is needed, an associated SCLK
      kernel is required to enable conversion between encoded SCLK
      (used to time-tag CK data) and barycentric dynamical time
      (TDB).

   In all cases, kernel data are normally loaded once per program
   run, NOT every time this routine is called.

-Particulars

   This routine is suitable for efficient ray-surface intercept
   computations in which the relative observer-target geometry is
   constant but the rays vary.

   For cases in which it is necessary to know the source of the
   data defining the surface on which an intercept was found,
   use the CSPICE routine dskxsi_c.

   For cases in which a ray's vertex is not explicitly known but is
   defined by relative observer-target geometry, the CSPICE
   ray-surface intercept routine sincpt_c should be used.

   This routine works with multiple DSK files. It places no
   restrictions on the data types or coordinate systems of the DSK
   segments used in the computation. DSK segments using different
   reference frames may be used in a single computation. The only
   restriction is that any pair of reference frames used directly or
   indirectly are related by a constant rotation.


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
      both represent a surface as sets of triangular plates, the
      union of those sets of plates is considered to represent the
      surface.

      Currently only unprioritized data selection is supported.
      Because prioritized data selection may be the default behavior
      in a later version of the routine, the presence of the `pri'
      argument is required.


      Round-off errors and mitigating algorithms
      ------------------------------------------

      When topographic data are used to represent the surface of a
      target body, round-off errors can produce some results that
      may seem surprising.

      Note that, since the surface in question might have mountains,
      valleys, and cliffs, the points of intersection found for
      nearly identical sets of inputs may be quite far apart from
      each other: for example, a ray that hits a mountain side in a
      nearly tangent fashion may, on a different host computer, be
      found to miss the mountain and hit a valley floor much farther
      from the observer, or even miss the target altogether.

      Round-off errors can affect segment selection: for example, a
      ray that is expected to intersect the target body's surface
      near the boundary between two segments might hit either
      segment, or neither of them; the result may be
      platform-dependent.

      A similar situation exists when a surface is modeled by a set
      of triangular plates, and the ray is expected to intersect the
      surface near a plate boundary.

      To avoid having the routine fail to find an intersection when
      one clearly should exist, this routine uses two "greedy"
      algorithms:

         1) If the ray passes sufficiently close to any of the
            boundary surfaces of a segment (for example, surfaces of
            maximum and minimum longitude or latitude), that segment
            is tested for an intersection of the ray with the
            surface represented by the segment's data.

            This choice prevents all of the segments from being
            missed when at least one should be hit, but it could, on
            rare occasions, cause an intersection to be found in a
            segment other than the one that would be found if higher
            precision arithmetic were used.

         2) For type 2 segments, which represent surfaces as
            sets of triangular plates, each plate is expanded very
            slightly before a ray-plate intersection test is
            performed. The default plate expansion factor is

               1 + SPICE_DSK_XFRACT

            where SPICE_DSK_XFRACT is declared in

               SpiceDtl.h

            For example, given a value for SPICE_DSK_XFRACT of 1.e-10, the
            sides of the plate are lengthened by 1/10 of a micron
            per km. The expansion keeps the centroid of the plate
            fixed.

            Plate expansion prevents all plates from being missed
            in cases where clearly at least one should be hit.

            As with the greedy segment selection algorithm, plate
            expansion can occasionally cause an intercept to be
            found on a different plate than would be found if higher
            precision arithmetic were used. It also can occasionally
            cause an intersection to be found when the ray misses
            the target by a very small distance.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Compute surface intercepts of rays emanating from a set of
      vertices distributed on a longitude-latitude grid. All
      vertices are outside the target body, and all rays point
      toward the target's center.

      Check intercepts against expected values. Indicate the
      number of errors, the number of computations, and the
      number of intercepts found.


      Use the meta-kernel shown below to load example SPICE
      kernels.


         KPL/MK

         File: dskxv_ex1.tm

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
            phobos512.bds                    DSK based on
                                             Gaskell ICQ Q=512
                                             plate model
         \begindata

            KERNELS_TO_LOAD = ( 'phobos512.bds' )

         \begintext

         End of meta-kernel


      Example code begins here.


      /.
         Program dskxv_ex1

         Multi-segment, vectorized spear program.

         This program expects all loaded DSKs
         to represent the same body and surface.
      ./
      #include <stdio.h>
      #include <stdlib.h>
      #include "SpiceUsr.h"

      int main( )
      {
         /.
         Local constants
         ./
         #define  DTOL           1.0e-14
         #define  FILSIZ         256
         #define  FRNMLN         33
         #define  BDNMLN         37
         #define  TYPLEN         5
         #define  INTLEN         12
         #define  MAXN           100000
         #define  MAXSRF         100

         /.
         Local variables
         ./
         static SpiceBoolean     fndarr [MAXN];
         SpiceBoolean            found;

         SpiceChar               dsk1   [FILSIZ];
         SpiceChar               filtyp [TYPLEN];
         SpiceChar               fixref [FRNMLN];
         SpiceChar               meta   [FILSIZ];
         SpiceChar               source [FILSIZ];
         SpiceChar               target [BDNMLN];

         SpiceDLADescr           dladsc;

         SpiceDSKDescr           dskdsc;

         SpiceDouble             d;
         static SpiceDouble      dirarr[MAXN][3];
         SpiceDouble             et;
         SpiceDouble             lat;
         SpiceDouble             latcrd[3];
         SpiceDouble             latstp;
         SpiceDouble             lon;
         SpiceDouble             lonstp;
         SpiceDouble             polmrg;
         SpiceDouble             r;
         SpiceDouble             radius;
         SpiceDouble             vlat;
         SpiceDouble             vlon;
         SpiceDouble             vrad;
         static SpiceDouble      vtxarr[MAXN][3];
         static SpiceDouble      xptarr[MAXN][3];
         SpiceDouble             xyzhit[3];

         SpiceInt                bodyid;
         SpiceInt                framid;
         SpiceInt                handle;
         SpiceInt                i;
         SpiceInt                nderr;
         SpiceInt                nhits;
         SpiceInt                nlstep;
         SpiceInt                nrays;
         SpiceInt                nsurf;
         static SpiceInt         srflst [MAXSRF];
         SpiceInt                surfid;


         chkin_c ( "vspear"  );

         /.
         Prompt for the name of the meta-kernel.
         ./
         prompt_c ( "Enter meta-kernel name >  ", FILSIZ, meta );

         /.
         Load the meta-kernel.
         ./
         furnsh_c ( meta );

         /.
         Get a handle for one of the loaded DSKs,
         then find the first segment and extract
         the body and surface IDs.
         ./
         kdata_c ( 0,    "DSK",  FILSIZ, TYPLEN,  FILSIZ,
                   dsk1, filtyp, source, &handle, &found );

         if ( !found )
         {
            sigerr_c ( "SPICE(NOINFO)" );
         }

         dlabfs_c ( handle, &dladsc, &found );

         if ( !found )
         {
            sigerr_c ( "SPICE(NOSEGMENT)" );
         }

         dskgd_c ( handle, &dladsc, &dskdsc );

         bodyid = dskdsc.center;
         surfid = dskdsc.surfce;
         framid = dskdsc.frmcde;

         bodc2n_c ( bodyid, BDNMLN, target, &found );

         if ( !found )
         {
            setmsg_c ( "Cannot map body ID # to a name." );
            errint_c ( "#", bodyid                       );
            sigerr_c ( "SPICE(BODYNAMENOTFOUND)"         );
         }

         frmnam_c ( framid, FRNMLN, fixref );

         if ( eqstr_c( fixref, " " ) )
         {
            setmsg_c ( "Cannot map frame ID # to a name." );
            errint_c ( "#", framid                        );
            sigerr_c ( "SPICE(BODYNAMENOTFOUND)"          );
         }

         /.
         Set the magnitude of the ray vertices. Use a large
         number to ensure the vertices are outside of
         any realistic target.
         ./
         r = 1.0e10;

         /.
         Spear the target with rays pointing toward
         the origin.  Use a grid of ray vertices
         located on a sphere enclosing the target.

         The variable `polmrg' ("pole margin") can
         be set to a small positive value to reduce
         the number of intercepts done at the poles.
         This may speed up the computation for
         the multi-segment case, since rays parallel
         to the Z axis will cause all segments converging
         at the pole of interest to be tested for an
         intersection.
         ./
         polmrg =    0.5;
         latstp =    1.0;
         lonstp =    2.0;

         nhits  =    0;
         nderr  =    0;

         lon    = -180.0;
         lat    =   90.0;
         nlstep =    0;
         nrays  =    0;

         /.
         Set the epoch for interval selection.
         ./
         et     = 0.0;

         /.
         Generate rays.
         ./
         while ( lon < 180.0 )
         {
            while ( nlstep <= 180 )
            {
               if ( lon == 180.0 )
               {
                  lat = 90.0 - nlstep*latstp;
               }
               else
               {
                  if ( nlstep == 0 )
                  {
                     lat =  90.0 - polmrg;
                  }
                  else if ( nlstep == 180 )
                  {
                     lat = -90.0 + polmrg;
                  }
                  else
                  {
                     lat =  90.0 - nlstep*latstp;
                  }
               }

               latrec_c ( r,             lon*rpd_c(),
                          lat*rpd_c(),   vtxarr[nrays] );

               vminus_c ( vtxarr[nrays], dirarr[nrays] );

               ++ nrays;
               ++ nlstep;
            }

            lon   += lonstp;
            lat    = 90.0;
            nlstep = 0;
         }

         /.
         Assign surface ID list.

         Note that, if we knew that all files had the desired
         surface ID, we could set `nsurf' to 0 and omit the
         initialization of the surface ID list.
         ./
         nsurf     = 1;
         srflst[0] = surfid;

         printf ( "\nComputing intercepts...\n" );

         dskxv_c ( SPICEFALSE,  target,  nsurf,  srflst,
                   et,          fixref,  nrays,  vtxarr,
                   dirarr,      xptarr,  fndarr         );

         printf ( "Done.\n\n" );

         /.
         Check results.
         ./
         for ( i = 0;  i < nrays;  i++ )
         {
            if ( fndarr[i] )
            {
               /.
               Record that a new intercept was found.
               ./
               ++ nhits;

               /.
               Compute the latitude and longitude of
               the intercept. Make sure these agree
               well with those of the vertex.
               ./
               reclat_c ( xptarr[i], latcrd, latcrd+1, latcrd+2 );
               radius = latcrd[0];

               /.
               Recover the vertex longitude and latitude.
               ./
               reclat_c ( vtxarr[i], &vrad, &vlon, &vlat  );
               latrec_c ( radius,     vlon,  vlat, xyzhit );

               d = vdist_c( xptarr[i], xyzhit );

               if ( d/r > DTOL )
               {
                  printf ( "===========================\n" );
                  printf ( "Lon = %f;  Lat = %f\n",
                           lon, lat                        );
                  printf ( "Bad intercept\n"               );
                  printf ( "Distance error = %e\n", d      );
                  printf ( "xpt    = (%e %e %e)\n",
                           xptarr[i][0], xptarr[i][1], xptarr[i][2] );
                  printf ( "xyzhit = (%e %e %e)\n",
                           xyzhit[0], xyzhit[1], xyzhit[2] );

                  ++ nderr;
               }
            }
            else
            {
               /.
               Missing the target entirely is a fatal error.

               This is true only for this program, not in
               general. For example, if the target shape is
               a torus, many rays would miss the target.
               ./
               printf ( "===========================\n" );
               printf ( "Lon = %f;  Lat = %f\n",
                        lon, lat                        );
               printf ( "No intercept\n"                );
               exit( 1 );
            }
         }
         printf( "nrays = %d\n", (int)nrays );
         printf( "nhits = %d\n", (int)nhits );
         printf( "nderr = %d\n", (int)nderr );

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, using as input the meta-kernel dskxv_ex1.tm, the
      output was:


      Enter meta-kernel name >  dskxv_ex1.tm

      Computing intercepts...
      Done.

      nrays = 32580
      nhits = 32580
      nderr = 0


-Restrictions

   1)  The frame designated by `fixref' must have a fixed
       orientation relative to the frame of any DSK segment
       used in the computation. This routine has no
       practical way of ensuring that this condition is met;
       so this responsibility is delegated to the calling
       application.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   E.D. Wright         (JPL)

-Version

   -CSPICE Version 1.1.0, 10-AUG-2021 (JDR) (EDW)

       Added use of ALLOC_CHECK_INTRA to check net null effect on
       alloc count.

       Edited the header to comply with NAIF standard.

       Updated code example to prompt for input meta-kernel name and
       set input time to zero.

       Updated -Exceptions section, adding missing exceptions and correcting
       the short message in Exception #1.

   -CSPICE Version 1.0.0, 26-FEB-2016 (NJB)

-Index_Entries

   vectorized ray-surface intercept
   vectorized ray-DSK intercept

-&
*/

{ /* Begin dskxv_c */


   /*
   Local variables
   */
   SpiceInt                arrSize;
   SpiceInt                i;

   int                     nowalloc;

   logical               * foundFlags;
   logical                 priFlag;

   /*
   Participate in error tracing.
   */
   chkin_c ( "dskxv_c" );

   /*
   Check the input string arguments:

      target
      fixref

   Make sure each pointer is non-null and each string contains
   at least one data character: that is, one character
   preceding the null terminator.
   */
   CHKFSTR ( CHK_STANDARD, "dskxv_c", target );
   CHKFSTR ( CHK_STANDARD, "dskxv_c", fixref );

   /*
   Check `nrays' here, since it must be valid in order to
   allocate memory.
   */
   if ( nrays < 1 )
   {
      setmsg_c ( "The ray count must be at least 1 "
                 "but was #."                       );
      errint_c ( "#",  nrays                        );
      sigerr_c ( "SPICE(INVALIDCOUNT)"              );
      chkout_c ( "dskxv_c"                          );
      return;
   }

   nowalloc = alloc_count();

   arrSize = sizeof(logical) * nrays;

   /*
   Allocate an array of type logical to receive flags
   returned by f2c'd routine.
   */
   foundFlags = (logical *) alloc_SpiceMemory( (size_t)arrSize );

   if ( !foundFlags )
   {
      setmsg_c ( "Attempt to allocate # bytes of memory for "
                 "the foundFlags array failed."               );
      errint_c ( "#", arrSize                                 );
      sigerr_c ( "SPICE(MALLOCFAILED)"                        );
      chkout_c ( "dskxv_c"                                    );
      return;
   }

   /*
   The input prioritization flag must be converted to type
   logical for the following call.
   */
   priFlag = (logical)pri;

   dskxv_ ( (logical     *) &priFlag,
            (char        *) target,
            (integer     *) &nsurf,
            (integer     *) srflst,
            (doublereal  *) &et,
            (char        *) fixref,
            (integer     *) &nrays,
            (doublereal  *) vtxarr,
            (doublereal  *) dirarr,
            (doublereal  *) xptarr,
            (logical     *) foundFlags,
            (ftnlen       ) strlen(target),
            (ftnlen       ) strlen(fixref)  );

   if ( failed_c() )
   {
      /*
      Always deallocate the flags array, even if the
      above call failed.
      */
      free_SpiceMemory( (void *)foundFlags );

      chkout_c ( "dskxv_c" );
      return;
   }

   /*
   Transfer the logical flag values to the output
   SpiceBoolean array.
   */
   for ( i = 0;  i < nrays;  i++  )
   {
      fndarr[i] = (SpiceBoolean)(foundFlags[i]);
   }

   /*
   Deallocate the flags array.
   */
   free_SpiceMemory( (void *)foundFlags );

   ALLOC_CHECK_INTRA(nowalloc);

   chkout_c ( "dskxv_c" );

} /* End dskxv_c */
