/*

-Procedure vzero_c ( Is a vector the zero vector? )

-Abstract

   Indicate whether a 3-vector is the zero vector.

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

   MATH
   VECTOR

*/

   #include "SpiceUsr.h"
   #undef    vzero_c


   SpiceBoolean vzero_c ( ConstSpiceDouble v[3] )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   v          I   Vector to be tested.

   The function returns the value SPICETRUE if and only if `v' is the
   zero vector.

-Detailed_Input

   v           is a vector in 3-space.

-Detailed_Output

   The function returns the value SPICETRUE if and only if `v' is the
   zero vector.

-Parameters

   None.

-Exceptions

   Error free.

-Files

   None.

-Particulars

   This function has the same truth value as the logical expression

      vnorm_c ( v )  ==  0.

   Replacing the above expression by

      vzero_c ( v );

   has several advantages: the latter expresses the test more
   clearly, looks better, and doesn't go through the work of scaling,
   squaring, taking a square root, and re-scaling (all of which
   vnorm_c must do) just to find out that a vector is non-zero.

   A related function is vzerog_c, which accepts vectors of arbitrary
   dimension.

-Examples

   1)  When testing whether a vector is the zero vector, one
       normally constructs tests like

          if (  vnorm_c ( v ) ==  0.  )
             {
                      .
                      .
                      .


       These can be replaced with the code

          if (  vzero_c ( v )  )
             {
                      .
                      .
                      .


    2) Check that a normal vector is non-zero before creating
       a plane with pnv2pl_c:

       if (  vzero_c ( NORMAL )  )
          {
          [ handle error ]
          }

       else
          {
          pnv2pl_c ( POINT, NORMAL, PLANE )
                        .
                        .
                        .
          }

-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   I.M. Underwood      (JPL)
   E.D. Wright         (JPL)

-Version

   -CSPICE Version 1.1.1, 05-JUL-2021 (JDR)

       Edited the header to comply with NAIF standard.

   -CSPICE Version 1.1.0, 22-OCT-1998 (NJB)

       Made input vector const. Removed #include of SpiceZfc.h.

   -CSPICE Version 1.0.0, 08-FEB-1998 (EDW) (IMU)

-Index_Entries

   test whether a 3-dimensional vector is the zero vector

-&
*/

{ /* Begin vzero_c */

   return  ( SpiceBoolean ) ( v[0] == 0. && v[1] == 0. && v[2] == 0.) ;

} /* End vzero_c */
