/*

-Procedure isrot_c ( Indicate whether a matrix is a rotation matrix )

-Abstract

   Indicate whether a 3x3 matrix is a rotation matrix.

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

   ROTATION

-Keywords

   ERROR
   MATRIX
   ROTATION

*/

   #include "SpiceUsr.h"
   #undef    isrot_c


   SpiceBoolean isrot_c ( ConstSpiceDouble    m   [3][3],
                          SpiceDouble         ntol,
                          SpiceDouble         dtol       )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   m          I   A matrix to be tested.
   ntol       I   Tolerance for the norms of the columns of m.
   dtol       I   Tolerance for the determinant of a matrix whose
                  columns are the unitized columns of m.

   The function returns the value SPICETRUE if and only if m is
   a rotation matrix.

-Detailed_Input

   m           is a 3x3 matrix to be tested.

   ntol        is the tolerance for the norms of the columns
               of m.

   dtol        is the tolerance for the determinant of a matrix
               whose columns are the unitized columns of m.

-Detailed_Output

   The function returns the value SPICETRUE if and only if m is found
   to be a rotation matrix. The criteria that m must meet are:


      1) The norm of each column of m must satisfy the relation

            1. - ntol  <   || column ||   <  1. + ntol.
                       -                  -

      2) The determinant of the matrix whose columns are the
         unitized columns of m must satisfy

            1. - dtol  <   determinant   <  1. + dtol.
                       -                 -

-Parameters

   None.

-Exceptions

   1)  If either of `ntol' or `dtol' is negative, the error
       SPICE(VALUEOUTOFRANGE) is signaled. isrot_c returns the value
       SPICEFALSE in this case.

-Files

   None.

-Particulars

   This routine is an error checking "filter"; its purpose is to
   detect gross errors, such as uninitialized matrices. Matrices
   that do not pass the tests used by this routine hardly qualify as
   rotation matrices. The test criteria can be adjusted by varying
   the parameters ntol and dtol.

   A property of rotation matrices is that their columns form a
   right-handed, orthonormal basis in 3-dimensional space. The
   converse is true: all 3x3 matrices with this property are
   rotation matrices.

   An ordered set of three vectors V1, V2, V3 forms a right-handed,
   orthonormal basis if and only if

      1)   || V1 ||  =  || V2 ||  =  || V3 ||  =  1

      2)   V3 = V1 x V2. Since V1, V2, and V3 are unit vectors,
           we also have

           < V3, V1 x V2 > = 1.

           This quantity is the determinant of the matrix whose
           columns are V1, V2 and V3.

   When finite precision numbers are used, rotation matrices will
   usually fail to satisfy these criteria exactly. We must use
   criteria that indicate approximate conformance to the criteria
   listed above. We choose

      1)   |   || Vi ||  -  1   |   <   ntol,  i = 1, 2, 3.
                                    -

      2)   Let

                     Vi
              Ui = ------ ,   i = 1, 2, 3.
                   ||Vi||

           Then we require

              | < U3, U1 x U2 > - 1 |  <  dtol;
                                       -

           equivalently, letting U be the matrix whose columns
           are U1, U2, and U3, we insist on

              | det(U) - 1 |  <  dtol.
                              _

-Examples

   1)  We have obtained an instrument pointing matrix C from a
       C-kernel, and we wish to test whether it is in fact a
       rotation matrix. We can use isrot_c to check this:

          #include "SpiceUsr.h"
               .
               .
               .
          /.
          Obtain pointing matrix:
          ./
          ckgp_c ( inst, timein, tol, ref, c, &timout, &found );


          /.
          Verify that c is a rotation:
          ./

          if ( !isrot_c( c ) )
          {

             [ perform exception handling ]

          }
          else
          {

             [ code for the normal case goes here ]

          }

-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   H.A. Neilan         (JPL)

-Version

   -CSPICE Version 1.0.1, 03-AUG-2021 (JDR)

       Edited the header to comply with NAIF standard.

   -CSPICE Version 1.0.0, 16-AUG-1999 (NJB) (HAN)

-Index_Entries

   indicate whether a matrix is a rotation matrix

-&
*/

{ /* Begin isrot_c */


   /*
   Local variables
   */
   SpiceBoolean            detok;
   SpiceBoolean            normok;

   SpiceDouble             d;
   SpiceDouble             mtrans[3][3];
   SpiceDouble             n0;
   SpiceDouble             n1;
   SpiceDouble             n2;
   SpiceDouble             unit  [3][3];



   /*
   Tolerances must be non-negative.
   */
   if ( ntol < 0.0 )
   {
      chkin_c  ( "isrot_c"                               );
      setmsg_c ( "ntol should be non-negative; it is #." );
      errdp_c  ( "#", ntol                               );
      sigerr_c ( "SPICE(VALUEOUTOFRANGE)"                );
      chkout_c ( "isrot_c"                               );
      return   ( SPICEFALSE                              );
   }
   else if ( dtol < 0.0 )
   {
      chkin_c  ( "isrot_c"                               );
      setmsg_c ( "dtol should be non-negative; it is #." );
      errdp_c  ( "#", dtol                               );
      sigerr_c ( "SPICE(VALUEOUTOFRANGE)"                );
      chkout_c ( "isrot_c"                               );
      return   ( SPICEFALSE                              );
   }


   /*
   The columns of m must resemble unit vectors.  If the norms are
   outside of the allowed range, m is not a rotation matrix.

   Also, the columns of m are required to be pretty nearly
   orthogonal.  The discrepancy is gauged by taking the determinant
   of the matrix unit, computed below, whose columns are the
   unitized columns of m.
   */

   xpose_c ( m, mtrans );

   unorm_c ( mtrans[0], unit[0], &n0 );
   unorm_c ( mtrans[1], unit[1], &n1 );
   unorm_c ( mtrans[2], unit[2], &n2 );


   normok =     (   n0 == brcktd_c ( n0,  1.0 - ntol,  1.0 + ntol )  )
             && (   n1 == brcktd_c ( n1,  1.0 - ntol,  1.0 + ntol )  )
             && (   n2 == brcktd_c ( n2,  1.0 - ntol,  1.0 + ntol )  );

   d      =     det_c ( unit );
   detok  =  (  d == brcktd_c ( d,  1.0 - dtol,  1.0 + dtol )  );


   return ( normok && detok );


} /* End isrot_c */
