/*

-Procedure dvsep_c ( Time derivative of separation angle )

-Abstract

   Calculate the time derivative of the separation angle between
   two input states, S1 and S2.

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

   GEOMETRY

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #undef dvsep_c

   SpiceDouble dvsep_c  ( ConstSpiceDouble    s1     [6],
                          ConstSpiceDouble    s2     [6] )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   s1         I   State vector of the first body
   s2         I   State vector of the second  body

-Detailed_Input

   s1,
   s2          are, respectively, the state vector of the first and
               second target bodies as seen from the observer

               An implicit assumption exists that both states lie in
               the same reference frame with the same observer for the
               same epoch. If this is not the case, the numerical
               result has no meaning.

-Detailed_Output

   The function returns the double precision value of the time derivative
   of the angular separation between S1 and S2.

-Parameters

   None.

-Exceptions

   1)  If numeric overflow and underflow cases are detected, an error
       is signaled by a routine in the call tree of this routine.

   2)  If called in "RETURN" mode, the function returns 0.

   3)  Linear dependent position components of `s1' and `s1' constitutes
       a non-error exception. The function returns 0 for this case.

-Files

   None.

-Particulars

   In this discussion, the notation

      < V1, V2 >

   indicates the dot product of vectors V1 and V2. The notation

      V1 x V2

   indicates the cross product of vectors V1 and V2.

   To start out, note that we need consider only unit vectors,
   since the angular separation of any two non-zero vectors
   equals the angular separation of the corresponding unit vectors.
   Call these vectors U1 and U2; let their velocities be V1 and V2.

   For unit vectors having angular separation

      THETA

   the identity

      || U1 x U1 || = ||U1|| * ||U2|| * sin(THETA)                (1)

   reduces to

      || U1 x U2 || = sin(THETA)                                  (2)

   and the identity

      | < U1, U2 > | = || U1 || * || U2 || * cos(THETA)           (3)

   reduces to

      | < U1, U2 > | = cos(THETA)                                 (4)

   Since THETA is an angular separation, THETA is in the range

      0 : Pi

   Then letting s be +1 if cos(THETA) > 0 and -1 if cos(THETA) < 0,
     we have for any value of THETA other than 0 or Pi


                                2          1/2
      cos(THETA) = s * ( 1 - sin (THETA)  )                       (5)

   or

                                2          1/2
      < U1, U2 > = s * ( 1 - sin (THETA)  )                       (6)


   At this point, for any value of THETA other than 0 or Pi,
   we can differentiate both sides with respect to time (T)
   to obtain

                                                    2        -1/2
      < U1, V2 > + < V1, U2 > =    s * (1/2)(1 - sin (THETA))

                                 * (-2) sin(THETA)*cos(THETA)

                                 * d(THETA)/dT                   (7a)


   Using equation (5), and noting that s = 1/s, we can cancel
   the cosine terms on the right hand side

                                                    -1
      < U1, V2 > + < V1, U2 > =    (1/2)(cos(THETA))

                                 * (-2) sin(THETA)*cos(THETA)

                                 * d(THETA)/dT                   (7b)

   With (7b) reducing to

      < U1, V2 > + < V1, U2 > = - sin(THETA) * d(THETA)/dT        (8)

   Using equation (2) and switching sides, we obtain

      || U1 x U2 || * d(THETA)/dT  =  - < U1, V2 > - < V1, U2 >   (9)

   or, provided U1 and U2 are linearly independent,

      d(THETA)/dT = ( - < U1, V2 > - < V1, U2 > ) / ||U1 x U2||  (10)

   Note for times when U1 and U2 have angular separation 0 or Pi
   radians, the derivative of angular separation with respect to
   time doesn't exist. (Consider the graph of angular separation
   with respect to time; typically the graph is roughly v-shaped at
   the singular points.)

-Examples

   None.

-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   E.D. Wright         (JPL)

-Version

   -CSPICE Version 1.0.1, 13-AUG-2021 (JDR)

       Edited the header to comply with NAIF standard.

   -CSPICE Version 1.0.0, 09-MAR-2009 (EDW) (NJB)

-Index_Entries

   time derivative of angular separation

-&
*/

{ /* Begin dvsep_c */

   /*
   Local variables.
   */
   SpiceDouble       retval;

   /*
   Participate in error tracing.
   */
   chkin_c ( "dvsep_c" );

   /*
   Call the f2c'd Fortran routine.
   */
   retval = (SpiceDouble) dvsep_( ( doublereal * ) s1,
                                  ( doublereal * ) s2 );

   chkout_c ( "dvsep_c" );

   return(retval);

} /* End dvsep_c */
