/*

-Procedure tipbod_c ( Transformation, inertial position to bodyfixed )

-Abstract

   Return a 3x3 matrix that transforms positions in inertial
   coordinates to positions in body-equator-and-prime-meridian
   coordinates.

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
   PCK
   NAIF_IDS
   ROTATION
   TIME

-Keywords

   ROTATION
   TRANSFORMATION

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZst.h"
   #include "SpiceZmc.h"

   void tipbod_c ( ConstSpiceChar  * ref,
                   SpiceInt          body,
                   SpiceDouble       et,
                   SpiceDouble       tipm[3][3] )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   ref        I   ID of inertial reference frame to transform from.
   body       I   ID code of body.
   et         I   Epoch of transformation.
   tipm       O   Position transformation matrix, inertial to prime
                  meridian.

-Detailed_Input

   ref         is the NAIF name for an inertial reference frame.
               Acceptable names include:

                  Name       Description
                  --------   --------------------------------
                  "J2000"    Earth mean equator, dynamical
                             equinox of J2000

                  "B1950"    Earth mean equator, dynamical
                             equinox of B1950

                  "FK4"      Fundamental Catalog (4)

                  "DE-118"   JPL Developmental Ephemeris (118)

                  "DE-96"    JPL Developmental Ephemeris ( 96)

                  "DE-102"   JPL Developmental Ephemeris (102)

                  "DE-108"   JPL Developmental Ephemeris (108)

                  "DE-111"   JPL Developmental Ephemeris (111)

                  "DE-114"   JPL Developmental Ephemeris (114)

                  "DE-122"   JPL Developmental Ephemeris (122)

                  "DE-125"   JPL Developmental Ephemeris (125)

                  "DE-130"   JPL Developmental Ephemeris (130)

                  "GALACTIC" Galactic System II

                  "DE-200"   JPL Developmental Ephemeris (200)

                  "DE-202"   JPL Developmental Ephemeris (202)

               See the Frames Required Reading frames.req for a full
               list of inertial reference frame names built into
               SPICE.

               The output `tipm' will give the transformation
               from this frame to the bodyfixed frame specified by
               `body' at the epoch specified by `et'.

   body        is the integer ID code of the body for which the
               position transformation matrix is requested. Bodies
               are numbered according to the standard NAIF numbering
               scheme. The numbering scheme is explained in the NAIF
               IDs Required Reading naif_ids.req.

   et          is the epoch at which the position transformation
               matrix is requested. (This is typically the
               epoch of observation minus the one-way light time
               from the observer to the body at the epoch of
               observation.)

-Detailed_Output

   tipm        is a 3x3 coordinate transformation matrix. It is
               used to transform positions from inertial
               coordinates to body fixed (also called equator and
               prime meridian --- PM) coordinates.

               Given a position P in the inertial reference frame
               specified by `ref', the corresponding bodyfixed
               position is given by the matrix vector product:

                  tipm * s

               The X axis of the PM system is directed to the
               intersection of the equator and prime meridian.
               The Z axis points along  the spin axis and points
               towards the same side of the invariable plane of
               the solar system as does earth's north pole.

-Parameters

   None.

-Exceptions

   1)  If the kernel pool does not contain all of the data required
       for computing the transformation matrix, `tipm', the error
       SPICE(INSUFFICIENTANGLES) is signaled by a routine in the call
       tree of this routine.

   2)  If the reference frame, `ref', is not recognized, an error is
       signaled by a routine in the call tree of this routine.

   3)  If the specified body code, `body', is not recognized, an error
       is signaled by a routine in the call tree of this routine.

   4)  If the `ref' input string pointer is null, the error
       SPICE(NULLPOINTER) is signaled.

   5)  If the `ref' input string has zero length, the error
       SPICE(EMPTYSTRING) is signaled.

-Files

   None.

-Particulars

   tipbod_c takes PCK information as input, either in the form of a
   binary or text PCK file. If the data required to compute tipm are
   available in a binary PCK, these data will take precedence over data
   from a text PCK. If there are multiple binary PCKs containing data
   from which tipm can be computed, the last loaded PCK takes
   precedence. If binary PCK data are available for the requested body
   and time, the Euler angles giving the body's orientation are
   evaluated, and the transformation matrix tipm is calculated from
   them. Using the Euler angles PHI, DELTA and W we compute

      TIPM = [W] [DELTA] [PHI]
                3       1     3

   If no appropriate binary PCK files have been loaded, text PCK data
   are used. Here information is found as RA, DEC and W (with the
   possible addition of nutation and libration terms for satellites).
   Again, the Euler angles are found, and the transformation matrix is
   calculated from them. The transformation from inertial to
   bodyfixed coordinates is represented as:

      TIPM = [W] [HALFPI-DEC] [RA+HALFPI]
                3            1           3

   These Euler angles RA, DEC and W are related to PHI, DELTA and W
   by the equations

      RA  = PHI  - pi/2
      DEC = pi/2 - DELTA
      W   = W

   In the text file, RA, DEC, and W are defined as follows:

                                   2      ____
                              RA2*t       \
      RA  = RA0  + RA1*t/T  + ------   +  /     a  sin theta
                                 2        ----   i          i
                                T           i

                                    2     ____
                              DEC2*t      \
      DEC = DEC0 + DEC1*t/T + -------  +  /    d  cos theta
                                  2       ----  i          i
                                 T          i


                                  2      ____
                              W2*t       \
      W   = W0   + W1*t/d   + -----   +  /     w  sin theta
                                 2       ----   i          i
                                d          i

   where:

      d = seconds/day

      T = seconds/Julian century

      a , d , and w  arrays apply to satellites only.
       i   i       i

      theta  = THETA0(i) + THETA1(i)*t/T are specific to each
           i

      planet.


   These angles ---typically nodal rates--- vary in number and
   definition from one planetary system to the next.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Calculate the matrix to rotate a position vector from the
      J2000 frame to the Saturn fixed frame at a specified
      time, and use it to compute the position of Titan in
      Saturn's body-fixed frame.

      Use the meta-kernel shown below to load the required SPICE
      kernels.


         KPL/MK

         File name: tipbod_ex1.tm

         This meta-kernel is intended to support operation of SPICE
         example programs. The kernels shown here should not be
         assumed to contain adequate or correct versions of data
         required by SPICE-based user applications.

         In order for an application to use this meta-kernel, the
         kernels referenced here must be present in the user's
         current working directory.

         The names and contents of the kernels referenced
         by this meta-kernel are as follows:

            File name                     Contents
            ---------                     --------
            sat375.bsp                    Saturn satellite ephemeris
            pck00010.tpc                  Planet orientation and
                                          radii
            naif0012.tls                  Leapseconds


         \begindata

            KERNELS_TO_LOAD = ( 'sat375.bsp',
                                'pck00010.tpc',
                                'naif0012.tls'  )

         \begintext

         End of meta-kernel


      Example code begins here.


      /.
         Program tipbod_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Local variables
         ./
         SpiceDouble          et;
         SpiceDouble          lt;
         SpiceDouble          pos    [3];
         SpiceDouble          satvec [3];
         SpiceDouble          tipm   [3][3];

         SpiceInt             satid;

         /.
         Load the kernels.
         ./
         furnsh_c ( "tipbod_ex1.tm" );

         /.
         The body ID for Saturn.
         ./
         satid = 699;

         /.
         Retrieve the transformation matrix at some time.
         ./
         str2et_c ( "Jan 1 2005", &et );
         tipbod_c ( "J2000", satid, et, tipm );

         /.
         Retrieve the position of Titan as seen from Saturn
         in the J2000 frame at `et'.
         ./
         spkpos_c ( "TITAN", et, "J2000", "NONE", "SATURN", pos, &lt );

         printf( "Titan as seen from Saturn:\n" );
         printf( "   in J2000 frame     : %12.3f %12.3f %12.3f\n",
                                            pos[0], pos[1], pos[2] );

         /.
         Rotate the position 3-vector `pos' into the
         Saturn body-fixed reference frame.
         ./
         mxv_c ( tipm, pos, satvec );

         printf( "   in IAU_SATURN frame: %12.3f %12.3f %12.3f\n",
                                   satvec[0], satvec[1], satvec[2] );

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Titan as seen from Saturn:
         in J2000 frame     :  1071928.661  -505781.970   -60383.976
         in IAU_SATURN frame:   401063.338 -1116965.364    -5408.806


      Note that the complete example could be replaced by a single
      spkpos_c call:

         spkpos_c ( "TITAN", et, "IAU_SATURN", "NONE", "SATURN", pos, &lt );

-Restrictions

   1)  The kernel pool must be loaded with the appropriate
       coefficients (from a text or binary PCK file) prior to
       calling this routine.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   E.D. Wright         (JPL)

-Version

   -CSPICE Version 1.0.3, 05-AUG-2021 (JDR)

       Edited the header to comply with NAIF standard. Added complete
       code example.

       Added frames.req to -Required_Reading. Updated -Exceptions
       section.

   -CSPICE Version 1.0.2, 14-AUG-2006 (EDW)

       Replace mention of ldpool_c with furnsh_c.

   -CSPICE Version 1.0.1, 13-APR-2000 (NJB)

       Made some minor updates and corrections in the code example.

   -CSPICE Version 1.0.0, 08-FEB-1998 (NJB)

       Based on SPICELIB Version 1.0.3, 10-MAR-1994 (KSZ).

-Index_Entries

   transformation from inertial position to bodyfixed

-&
*/

{ /* Begin tipbod_c */


   /*
   Participate in error tracing.
   */
   chkin_c ( "tipbod_c" );

   /*
   Check the input string ref to make sure the pointer is non-null
   and the string length is non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "tipbod_c", ref );

   /*
   Call the f2c'd Fortran routine.
   */
   tipbod_ ( ( char        * )  ref,
             ( integer     * )  &body,
             ( doublereal  * )  &et,
             ( doublereal  * )  tipm,
             ( ftnlen        )  strlen(ref) );

   /*
   Transpose the output matrix to put it in row-major order.
   */
   xpose_c  ( tipm, tipm );

   chkout_c ( "tipbod_c" );


} /* End tipbod_c */
