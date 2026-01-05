/*

-Procedure sct2e_c ( SCLK ticks to ET )

-Abstract

   Convert encoded spacecraft clock (`ticks') to ephemeris
   seconds past J2000 (ET).

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

   void sct2e_c ( SpiceInt       sc,
                  SpiceDouble    sclkdp,
                  SpiceDouble  * et     )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   sc         I   NAIF spacecraft ID code.
   sclkdp     I   SCLK, encoded as ticks since spacecraft clock
                  start.
   et         O   Ephemeris time, seconds past J2000.

-Detailed_Input

   sc          is a NAIF integer code for a spacecraft, one of
               whose encoded clock values is represented by
               `sclkdp'.

   sclkdp      is an encoded spacecraft clock value. `sclkdp'
               represents time measured from spacecraft clock
               start: partition information IS reflected in the
               encoded value.

-Detailed_Output

   et          is the epoch, specified as ephemeris seconds past
               J2000, that corresponds to `sclkdp'.

-Parameters

   None.

-Exceptions

   1)  If an SCLK kernel has not been loaded, does not contain all of
       the required data, or contains invalid data, an error is
       signaled by a routine in the call tree of this routine. The
       output argument `et' will not be modified. This routine assumes
       that that an SCLK kernel appropriate to the spacecraft clock
       identified by the input argument `sc' has been loaded.

   2)  If a leapseconds kernel is required for conversion between
       SCLK and ET but is not loaded, an error is signaled by a
       routine in the call tree of this routine. The output argument
       `et' will not be modified. When using SCLK kernels that map SCLK
       to a time system other than ET (also called barycentric
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
       output argument `et' will not be modified.

   4)  If the input argument `sclkdp' is invalid, an error is signaled
       by a routine in the call tree of this routine. The output
       argument `et' will not be modified.

-Files

   None.

-Particulars

   This routine operates on encoded SCLK values. These values
   are obtained by calling the CSPICE routine scencd_c or other
   SCLK conversion routines. The advantage of encoded SCLK, as
   opposed to character string representations of SCLK is that
   encoded SCLK values are easy to perform arithmetic operations on.
   Additionally, working with encoded SCLK reduces the overhead of
   repeated conversion of character strings to integers or double
   precision numbers.

   To convert a string representation of an SCLK value to ET, use
   the CSPICE routine scs2e_c.

   See the SCLK Required Reading for a list of the entire set of
   SCLK conversion routines.

-Examples

   1)  Encode a Galileo SCLK string, and convert the encoded value
       to ET; use these time values to look up both GLL orbiter
       scan platform's pointing and the GLL--Earth state vector
       for an epoch specified by an SCLK string.

          During program initialization, load the leapseconds and
          SCLK kernels. We will pretend that these files are named
          "leapseconds.ker" and "gllsclk.ker".  To use this code
          fragment, you must substitute the actual names of these
          kernel files for the names used here.

             /.
             load leapseconds and SCLK kernels:
             ./
             furnsh_c ( "leapseconds.ker" );
             furnsh_c ( "gllsclk.ker"     );

             /.
             The mission is Galileo, which has spacecraft ID -77.
             Let's assume that the SCLK string is

                1 / 1900000:00:00

             The number 1, followed by a slash, indicates that the
             epoch is in the first partition.

             The next step is to encode this SCLK string, and also
             find the corresponding ET value:
             ./

             scencd_c ( -77, "1/ 1900000:00:00", &sclkdp );
             sct2e_c  ( -77,  sclkdp,            &et     );


          We'll assume that you've already loaded SPK and CK files
          containing ephemeris data for the GLL orbiter and the
          Earth, as well as scan platform pointing. Now you're
          ready to call both ckgp_c, which expects the input epoch to
          be specified by an encoded SCLK string, and spkez_c, which
          expects the epoch to be specified as an ephemeris time.

             /.
             Find scan platform pointing cmat and s/c--target
             vector (first 3 components of state) at epoch.
             We assume that CK and SPK kernels have been loaded
             already, via cklpf_c and spklef_c respectively.

             Use tolerance of 80 ticks for the CK look-up.
             ./
             scanpl = -77001;
             earth  =    399;
             tol    =   80.0;

             ckgp_c  ( scanpl,  sclkdp,   tol,    refsys,
                       cmat,    &clkout,  &found         );

             if ( !found )
             {
                [ Indicate to user that pointing was not
                  available ]
             }

             spkez_c ( earth,  et,     refsys,  corr,
                       -77,    state,  &lt           );



   2)  Convert an encoded Voyager 2 SCLK value to UTC, using calendar
       format, with 3 digits of precision.

          Again, your initialization code must load the leapseconds
          and SCLK kernels:

             /.
             load leapseconds and SCLK kernels:
             ./
             furnsh_c ( "leapseconds.ker" );
             furnsh_c ( "vgr2sclk.ker"    );


          To find the UTC value corresponding to the encoded
          Voyager 2 SCLK value sclkdp, you can use the code fragment

             sct2e_c  ( -32,  sclkdp,     &et          );
             et2utc_c (  et,  "c",    3,  UTCLEN,  utc );

          where UTCLEN is a constant indicating the available
          room in the string utc. A value of 25 characters suffices.

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
   E.D. Wright         (JPL)

-Version

   -CSPICE Version 1.0.2, 05-AUG-2021 (JDR)

       Edited the header to comply with NAIF standard. Moved CK and SPK
       required readings from -Literature_References to -Required_Reading
       section.

   -CSPICE Version 1.0.1, 14-AUG-2006 (EDW)

       Replace mention of ldpool_c with furnsh_c.

   -CSPICE Version 1.0.0, 08-FEB-1998 (NJB)

       Based on SPICELIB Version 1.0.2, 10-APR-1992 (NJB) (WLT)

-Index_Entries

   spacecraft_clock ticks to ephemeris time

-&
*/

{ /* Begin sct2e_c */


   /*
   Local variables
   */
   SpiceDouble             loc_et;


   /*
   Participate in error handling
   */
   chkin_c ( "sct2e_c");


   /*
   Carry out the conversion.
   */
   sct2e_ ( ( integer    * ) &sc,
            ( doublereal * ) &sclkdp,
            ( doublereal * ) &loc_et  );

   /*
   Assign the output argument.
   */
   *et =  loc_et;


   chkout_c ( "sct2e_c");


} /* End sct2e_c */
