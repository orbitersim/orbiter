/*

-Procedure uddf_c ( First derivative of a function, df(x)/dx )

-Abstract

   Calculate the first derivative of a caller-specified scalar
   function using a three-point estimation.

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

   None.

-Keywords

   DERIVATIVE
   MATH

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #undef   uddf_c

   void uddf_c (  void             ( * udfunc ) ( SpiceDouble    et,
                                                  SpiceDouble  * value ),
                  SpiceDouble          x,
                  SpiceDouble          dx,
                  SpiceDouble        * deriv )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   udfunc     I   The routine that computes the scalar value
                  of interest.
   x          I   Independent variable of `udfunc'.
   dx         I   Interval from `x' for derivative calculation.
   deriv      O   Approximate derivative of `udfunc' at `x'.

-Detailed_Input

   udfunc      is an externally specified routine that returns the
               value of the scalar quantity function of interest
               at x.

               The prototype for `udfunc' is

                   void   ( * udfunc ) ( SpiceDouble    et,
                                         SpiceDouble  * value )

               where:

                   et      an input double precision value of the independent
                           variable the function at which to determine the
                           scalar value.

                   value   the scalar double precision value of `udfunc'
                           at `x'.

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

   deriv       is the scalar double precision approximate value of the
               first derivative of `udfunc' with respect to `x'.

               Functionally:

                           d udfunc ( y )  |
                  deriv = ---------------- |
                                 dy        | y=x

-Parameters

   None.

-Exceptions

   1)  If `dx' has a value of zero, an error is signaled by a routine
       in the call tree of this routine.

-Files

   If the evaluation of `udfunc' requires SPICE kernel data, the
   appropriate kernels must be loaded before calling this routine.

   -  SPK data: the calling application must load ephemeris data
      for the targets, observer, and any intermediate objects in
      a chain connecting the targets and observer for the time
      used in the evaluation. If aberration corrections are
      used, the states of target and observer relative to the
      solar system barycenter must be calculable from the
      available ephemeris data.

   -  If non-inertial reference frames are used, then PCK
      files, frame kernels, C-kernels, and SCLK kernels may be
      needed.

   Such kernel data are normally loaded once per program run, NOT
   every time this routine is called.

-Particulars

   This routine provides a simple interface to numerically calculate
   the first derivative of a scalar quantity function, `udfunc'.
   `udfunc' is expected to be "well behaved" across at the evaluation
   interval [ x - dx, x + dx ]. This means a linear approximation to
   the function over the interval is sufficiently accurate to
   calculate the approximate derivative at `x'.

   The routine qderiv_c performs the differentiation using a three
   point estimation. See the header of the SPICE routine qderiv_c for
   details of the discrete derivative computation performed by this
   routine.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Calculate the time derivative of the light time corresponding
      to the apparent position of Mercury relative to the Moon at
      time "JAN 1 2009."

      Use the meta-kernel shown below to load the required SPICE
      kernels.


         KPL/MK

         File name: uddf_ex1.tm

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
            de421.bsp                     Planetary ephemeris
            naif0009.tls                  Leapseconds


         \begindata

            KERNELS_TO_LOAD = ( 'de421.bsp',
                                'naif0009.tls'  )

         \begintext

         End of meta-kernel


      Example code begins here.


      /.
         Program uddf_ex1
      ./

      #include <stdio.h>
      #include "SpiceUsr.h"

      void udfunc ( SpiceDouble et, SpiceDouble * value );

      int main()
         {

         SpiceDouble       et;
         SpiceDouble       dt;
         SpiceDouble       deriv;

         /.
         Load leapsecond and SPK kernels. The name of the
         meta kernel file shown here is fictitious; you
         must supply the name of a file available
         on your own computer system.
         ./

         furnsh_c ( "uddf_ex1.tm" );

         /.
         Use a shift of one second off the epoch of interest.
         ./
         dt = 1.;

         /.
         Convert the epoch date string to ephemeris seconds.
         ./
         str2et_c ( "JAN 1 2009", &et );

         /.
         Calculate the derivative of `udfunc' at `et'.
         ./
         uddf_c( udfunc, et, dt, &deriv );

         /.
         Output the calculated derivative.
         ./

         printf( "%18.12f\n", deriv );

         return ( 0 );
         }


      /.
      A scalar quantity function that returns the light-time
      between the Moon and Mercury at 'et'.
      ./

      void udfunc ( SpiceDouble et, SpiceDouble * value )
      {

         SpiceDouble          lt;
         SpiceDouble          pos[3];

         /.
         Evaluate the apparent position of Mercury with respect
         to the Moon at 'et'.
         ./
         spkpos_c ( "MERCURY", et, "J2000", "LT+S", "MOON", pos, &lt );

         /.
         Return the light-time value as the scalar quantity.
         ./
         *value = lt;

         return;
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


         -0.000135670941


-Restrictions

   1)  The function `udfunc' must exist everywhere within [x - dx, x + dx].

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   E.D. Wright         (JPL)

-Version

   -CSPICE Version 1.0.1, 05-AUG-2021 (JDR)

       Edited the header to comply with NAIF standard. Renamed example's
       meta-kernel.

       Added the -Exceptions, -Particulars and -Files sections.

   -CSPICE Version 1.0.0, 31-MAR-2010 (EDW) (NJB)

-Index_Entries

   first derivative of a user-defined scalar function

-&
*/

   {  /* Begin uddf_c */

   /*
   Local variables
   */

   SpiceInt                  n;
   SpiceDouble               dfdx  [1];
   SpiceDouble               udval [2];

   /*
   Participate in error tracing.
   */
   if ( return_c() )
      {
      return;
      }
   chkin_c ( "uddf_c" );

   /*
   Apply a three-point estimation of the derivative for 'udfunc' at
   'x' by evaluating udfunc at [x-dx, x+dx].

   The qderiv_ call returns a single value in the 'dfdx' array.
   */
   n = 1;

   udfunc ( x - dx, &(udval[0]) );
   udfunc ( x + dx, &(udval[1]) );

   (void) qderiv_( (integer    *) &n,
                   (doublereal *) &(udval[0]),
                   (doublereal *) &(udval[1]),
                   (doublereal *) &dx,
                   (doublereal *) dfdx );

   *deriv = dfdx[0];

   chkout_c (  "uddf_c" );
   }
