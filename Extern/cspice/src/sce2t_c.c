/*

-Procedure sce2t_c ( ET to SCLK ticks )

-Abstract

   Convert ephemeris seconds past J2000 (ET) to integral
   encoded spacecraft clock (`ticks'). For conversion to
   fractional ticks, (required for C-kernel production), see
   the routine sce2c_c.

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
   SCLK
   SPK
   TIME

-Keywords

   CONVERSION
   TIME

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"

   void sce2t_c ( SpiceInt       sc,
                  SpiceDouble    et,
                  SpiceDouble  * sclkdp )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   sc         I   NAIF spacecraft ID code.
   et         I   Ephemeris time, seconds past J2000.
   sclkdp     O   SCLK, encoded as ticks since spacecraft clock
                  start.

-Detailed_Input

   sc          is a NAIF integer code for a spacecraft whose encoded
               SCLK value at the epoch specified by `et' is desired.

   et          is an epoch, specified as ephemeris seconds past
               J2000.

-Detailed_Output

   sclkdp      is an encoded integral spacecraft clock value.
               `sclkdp' is an encoded representation of the total
               count of spacecraft clock ticks measured from the
               time the spacecraft clock started to the epoch `et':
               partition information IS reflected in the encoded
               value. `sclkdp' is rounded to the nearest integral
               double precision number.

-Parameters

   None.

-Exceptions

   1)  If an SCLK kernel has not been loaded, does not contain all of
       the required data, or contains invalid data, an error is
       signaled by a routine in the call tree of this routine. The
       output argument `sclkdp' will not be modified. This routine
       assumes that that an SCLK kernel appropriate to the spacecraft
       clock identified by the input argument `sc' has been loaded.

   2)  If a leapseconds kernel is required for conversion between
       SCLK and `et' but is not loaded, an error is signaled by a
       routine in the call tree of this routine. The output argument
       `sclkdp' will not be modified. When using SCLK kernels that map
       SCLK to a time system other than `et' (also called barycentric
       dynamical time---`TDB'), it is necessary to have a leapseconds
       kernel loaded at the time this routine is called.

       The time system that an SCLK kernel maps SCLK to is indicated
       by the variable SCLK_TIME_SYSTEM_nn in the kernel, where nn
       is the negative of the NAIF integer code for the spacecraft.
       The time system used in a kernel is TDB if and only if the
       variable is assigned the value 1.


   3)  If the clock type for the spacecraft clock identified by `sc' is
       not supported by this routine, the error SPICE(NOTSUPPORTED)
       is signaled by a routine in the call tree of this routine. The
       output argument `sclkdp' will not be modified.

   4)  If the input `et' value is not representable as an encoded
       spacecraft clock value for the spacecraft clock identified by
       `sc', an error is signaled by a routine in the call tree of this
       routine. The output argument `sclkdp' will not be modified.

-Files

   None.

-Particulars

   This function outputs discrete, encoded SCLK values. Since
   continuous SCLK values are generally more useful, the newer
   function sce2c_c (ET to continuous ticks) should normally be used
   in place of this one. However, the functionality of this
   routine is needed for converting ET to SCLK strings, and the
   routine sce2s_c calls this routine for that purpose.

   This function outputs encoded SCLK values. The advantage of
   encoded SCLK, as opposed to character string representations of
   SCLK, is that encoded SCLK values are easy to perform arithmetic
   operations on. Also, working with encoded SCLK reduces the
   overhead of repeated conversion of character strings to integers
   or double precision numbers.

   To convert ET to a string representation of an SCLK value, use
   the CSPICE function sce2s_c.

   See the SCLK Required Reading for a list of the entire set of
   SCLK conversion routines.

-Examples

   1)  Convert ET directly to an encoded SCLK value. Use both of
       these time values to look up both C-kernel (pointing) and
       SPK (position and velocity) data for an epoch specified by an
       ephemeris time.

          During program initialization, load the leapseconds and
          SCLK kernels. We will pretend that these files are named
          "leapseconds.ker" and "gllsclk.ker".  To use this code
          fragment, you must substitute the actual names of these
          kernel files for the names used here.

             /.
             Load leapseconds and SCLK kernels:
             ./
             furnsh_c ( "leapseconds.ker" )
             furnsh_c ( "gllsclk.ker"     )

          The mission is Galileo, which has spacecraft ID -77.
          Let ET be the epoch, specified in ephemeris seconds
          past J2000, at which both position and pointing data
          is desired.

          Find the encoded SCLK value corresponding to ET.

             sce2t_c  ( -77,  et,  &sclkdp );

          Now you're ready to call both ckgp_c, which expects the input
          epoch to be specified by an encoded SCLK string, and
          spkez_c, which expects the epoch to be specified as an
          ephemeris time.

             /.
             Find scan platform pointing cmat and s/c--target
             vector (first 3 components of state) at epoch.
             We assume that CK and SPK kernels have been loaded
             already, via cklpf_c and spklef_c respectively.
             ./
             ckgp_c  ( scanpl,  sclkdp,   tol,     refsys,
                       cmat,    &clkout,  &found          );

             spkez_c ( target,  et,       refsys,  corr,
                       -77,     state,    &lt           );


   2)  Convert UTC to an encoded Voyager 2 SCLK value.

          Again, your initialization code must load the leapseconds
          and SCLK kernels.

             /.
             Load leapseconds and SCLK kernels:
             ./
             furnsh_c ( "leapseconds.ker" );
             furnsh_c ( "vgr2sclk.ker"    );

             /.
             Find the encoded Voyager 2 SCLK value sclkdp
             corresponding to the given UTC time.
             ./
             utc2et  (  utc,  &et          );
             sce2t_c ( -32,   et,  &sclkdp );

-Restrictions

   1)  An SCLK kernel appropriate to the spacecraft clock identified
       by `sc' must be loaded at the time this routine is called.

   2)  If the SCLK kernel used with this routine does not map SCLK
       directly to barycentric dynamical time, a leapseconds kernel
       must be loaded at the time this routine is called.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)

-Version

   -CSPICE Version 1.0.3, 05-AUG-2021 (JDR)

       Edited the header to comply with NAIF standard. Moved CK and SPK
       required readings from -Literature_References to -Required_Reading
       section.

   -CSPICE Version 1.0.2, 27-JAN-2004 (NJB)

       Header was updated to remove comments indicating this routine
       was deprecated. Minor changes were made to clarify both the
       functionality of this routine and the difference between
       this routine and sce2c_c. Examples were updated to use furnsh_c.

   -CSPICE Version 1.0.1, 09-MAR-1999 (NJB)

       Updated to reflect the introduction of continuous ticks and
       the function sce2c_c.

       Occurrences of "routine" were changed to "function" in the header.

   -CSPICE Version 1.0.0, 08-FEB-1998 (NJB)

       Based on SPICELIB Version 1.0.2, 10-APR-1992 (NJB) (WLT)

-Index_Entries

   ephemeris time to spacecraft_clock ticks

-&
*/

{ /* Begin sce2t_c */

   /*
   Local variables
   */
   SpiceDouble             loc_sclkdp;


   /*
   Participate in error handling.
   */
   chkin_c ( "sce2t_c");

   /*
   Do the conversion.
   */
   sce2t_ ( ( integer    * ) &sc,
            ( doublereal * ) &et,
            ( doublereal * ) &loc_sclkdp );

   /*
   Assign the output argument.
   */
   *sclkdp = loc_sclkdp;


   chkout_c ( "sce2t_c");


} /* End sce2t_c */
