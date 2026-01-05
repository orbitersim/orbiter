/*

-Procedure dvhat_c ( Derivative and unit vector "V-hat" of a state)

-Abstract

   Find the unit vector corresponding to a state vector and the
   derivative of the unit vector.

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
   VECTOR

*/
   #include "SpiceUsr.h"
   #undef   dvhat_c

   void dvhat_c ( ConstSpiceDouble s1  [6],
                  SpiceDouble      sout[6] )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   s1         I   State to be normalized.
   sout       O   Unit vector s1 / |s1|, and its time derivative.

-Detailed_Input

   s1          is any double precision state. If the position
               component of the state is the zero vector, this
               routine will detect it and will not attempt to divide
               by zero.

-Detailed_Output

   sout        is a state containing the unit vector pointing in
               the direction of position component of `s1' and the
               derivative of the unit vector with respect to time.

               `sout' may overwrite `s1'.

-Parameters

   None.

-Exceptions

   Error free.

   1)  If `s1' represents the zero vector, then the position
       component of `sout' will also be the zero vector. The
       velocity component will be the velocity component
       of `s1'.

-Files

   None.

-Particulars

   Let `s1' be a state vector with position and velocity components P
   and V respectively. From these components one can compute the
   unit vector parallel to P, call it `u' and the derivative of `u'
   with respect to time, `du'. This pair (u,du) is the state returned
   by this routine in `sout'.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Suppose that `state' gives the apparent state of a body with
      respect to an observer. This routine can be used to compute the
      instantaneous angular rate of the object across the sky as seen
      from the observers vantage.

      Use the meta-kernel shown below to load the required SPICE
      kernels.


         KPL/MK

         File name: dvhat_ex1.tm

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
            pck00008.tpc                  Planet orientation and
                                          radii
            naif0009.tls                  Leapseconds


         \begindata

            KERNELS_TO_LOAD = ( 'de421.bsp',
                                'pck00008.tpc',
                                'naif0009.tls'  )

         \begintext

         End of meta-kernel


      Example code begins here.


      /.
         Program dvhat_ex1
      ./
      #include <stdio.h>
      #include <math.h>
      #include "SpiceUsr.h"

      int main()
      {

         SpiceDouble       et;
         SpiceDouble       lt;
         SpiceDouble       omega;
         SpiceDouble       state  [6];
         SpiceDouble       ustate [6];

         SpiceChar       * epoch  = "Jan 1 2009";
         SpiceChar       * target = "MOON";
         SpiceChar       * frame  = "J2000";
         SpiceChar       * abcorr = "LT+S";
         SpiceChar       * obsrvr = "EARTH BARYCENTER";

         /.
         Load SPK, PCK, and LSK kernels, use a meta kernel for
         convenience.
         ./
         furnsh_c ( "dvhat_ex1.tm" );

         /.
         Define an arbitrary epoch, convert the epoch to ephemeris time.
         ./
         str2et_c ( epoch, &et );

         /.
         Calculate the state of the moon with respect to the earth-moon
         barycenter in J2000, corrected for light time and stellar
         aberration at `et'.
         ./

         spkezr_c ( target, et, frame, abcorr, obsrvr, state, &lt );

         /.
         Calculate the unit vector of `state' and the derivative of the
         unit vector.
         ./
         dvhat_c ( state, ustate );

         /.
         Calculate the instantaneous angular velocity from the magnitude
         of the derivative of the unit vector.

            v = r x omega

             ||omega|| = ||v||  for  r . v = 0
                         -----
                         ||r||

             ||omega|| = ||v||  for  ||r|| = 1
         ./
         omega = vnorm_c ( ustate+3 );

         printf( "Instantaneous angular velocity (rad/sec): %18.12e\n",
                                                                 omega );

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Instantaneous angular velocity (rad/sec): 2.481066592694e-06


-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   J. Diaz del Rio     (ODC Space)
   E.D. Wright         (JPL)

-Version

   -CSPICE Version 1.0.2, 02-JUL-2021 (JDR)

       Edits to header to comply with NAIF standard. Added
       meta-kernel to the example.

   -CSPICE Version 1.0.1, 06-MAY-2010 (EDW)

       Reordered header sections to proper NAIF convention.
       Minor edit to code comments eliminating typo.

   -CSPICE Version 1.0.0, 07-JUL-1999 (EDW)

-Index_Entries

   State of a unit vector parallel to a state vector

-&
*/

{ /* Begin dvhat_c */

   /*
   Local variables
   */
   SpiceDouble       length;
   SpiceDouble       posin [3];
   SpiceDouble       posout[3];
   SpiceDouble       velin [3];
   SpiceDouble       velout[3];


   /*
   We'll do this the obvious way for now.  Unpack the input vector
   into two working vectors.
   */
   posin[0] = s1[0];
   posin[1] = s1[1];
   posin[2] = s1[2];
   velin[0] = s1[3];
   velin[1] = s1[4];
   velin[2] = s1[5];


   /*
   Get the position portion of the output state and the length of
   the input position.
   */
   unorm_c ( posin, posout, &length );

   if ( length == 0. )
      {

      /*
      If the length of the input position is zero, just copy
      the input velocity to the output velocity.
      */
      vequ_c ( velin, velout );

      }
   else
      {

      /*
      Otherwise the derivative of the unit vector is just the
      component of the input velocity perpendicular to the input
      position, scaled by the reciprocal of the length of the
      input position.
      */
      vperp_c ( velin    , posout, velout );
      vscl_c  ( 1./length, velout, velout );

      }


   /*
   Pack everything and return.  Hazar!
   */
   sout[0] = posout[0];
   sout[1] = posout[1];
   sout[2] = posout[2];
   sout[3] = velout[0];
   sout[4] = velout[1];
   sout[5] = velout[2];

} /* End dvhat_c */
