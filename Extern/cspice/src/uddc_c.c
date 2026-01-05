/*

-Procedure uddc_c ( Derivative of function less than zero, df(x)/dx < 0 )

-Abstract

   Return SPICETRUE if the derivative of the callback function `udfunc'
   at a given abscissa value is negative.

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

   GF

-Keywords

   DERIVATIVE
   GEOMETRY

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #undef   uddc_c

   void uddc_c ( void            ( * udfunc ) ( SpiceDouble    x,
                                                SpiceDouble  * value ),
                 SpiceDouble         x,
                 SpiceDouble         dx,
                 SpiceBoolean      * isdecr )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   udfunc     I   The routine that computes the scalar value
                  of interest.
   x          I   Independent variable of 'udfunc'.
   dx         I   Interval from 'x' for derivative calculation.
   isdecr     O   Boolean indicating if the derivative is negative.

-Detailed_Input

   udfunc      is the routine that returns the value of the scalar quantity
               function of interest at `x'. The calling sequence for `udfunc'
               is:

                  udfunc ( x, &value );

               where:

                  x       is the double precision value of the
                          independent variable of the function
                          at which to determine the scalar value.

                  value   is the double precision value returned by
                          `udfunc' at `x'.

               Functionally:

                  value = udfunc ( x )

   x           is a scalar double precision value at which to determine
               the derivative of `udfunc'.

               For many SPICE uses, `x' will represent ephemeris time,
               expressed as seconds past J2000 TDB.

   dx          is a scalar double precision value representing half the
               interval in units of `x' separating the evaluation
               values of `udfunc'; the evaluations occur at (x + dx)
               and (x - dx).

               `dx' may be negative but must be non-zero.

-Detailed_Output

   isdecr      is a scalar boolean indicating if the first derivative
               of `udfunc' with respect to time at `et' is less than
               zero.

               Functionally:

                 d udfunc(x) |
                 ----------- |  <  0
                      dx     |
                              x

-Parameters

   None.

-Exceptions

   1)  If `dx' has a value of zero, an error is signaled by a routine
       in the call tree of this routine.

-Files

   If the evaluation of 'udfunc' requires SPICE kernel data, the
   appropriate kernels must be loaded before calling this routine.

   -  SPK data: the calling application must load ephemeris data
      for the targets, observer, and any intermediate objects in
      a chain connecting the targets and observer for the time
      used in the evaluation. If aberration corrections are used,
      the states of target and observer relative to the solar system
      barycenter must be calculable from the available ephemeris
      data.

   -  If non-inertial reference frames are used, then PCK
      files, frame kernels, C-kernels, and SCLK kernels may be
      needed.

   Such kernel data are normally loaded once per program run, NOT
   every time this routine is called.

-Particulars

   None.

-Examples

   See gfuds_c.

-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   E.D. Wright         (JPL)

-Version

   -CSPICE Version 1.0.1, 01-NOV-2021 (JDR)

       Edited the header to comply with NAIF standard.

   -CSPICE Version 1.0.0, 31-MAR-2010 (EDW) (NJB)

-Index_Entries

   first derivative less-than zero

-&
*/

   {

   SpiceDouble               deriv;

   /*
   Participate in error tracing.
   */
   if ( return_c() )
     {
      return;
      }
   chkin_c ( "uddc_c" );

   *isdecr = SPICEFALSE;

   uddf_c ( udfunc, x, dx, &deriv );

   if ( failed_c() )
     {
     chkout_c ( "uddc_c" );
     return;
     }

   *isdecr = deriv <  0.;

   chkout_c ( "uddc_c" );
   return;
   }
