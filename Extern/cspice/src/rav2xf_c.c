/*

-Procedure rav2xf_c ( Rotation and angular velocity to transform )

-Abstract

   Determine a state transformation matrix from a rotation matrix
   and the angular velocity of the rotation.

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

   FRAMES

*/

   #include "SpiceUsr.h"
   #undef rav2xf_c


   void rav2xf_c ( ConstSpiceDouble    rot   [3][3],
                   ConstSpiceDouble    av    [3],
                   SpiceDouble         xform [6][6]  )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   rot        I   Rotation matrix.
   av         I   Angular velocity vector.
   xform      O   State transformation associated with `rot' and `av'.

-Detailed_Input

   rot         is a rotation matrix that gives the transformation from
               some frame FRAME1 to another frame FRAME2.

   av          is the angular velocity of the transformation.
               In other words, if `p' is the position of a fixed
               point in FRAME2, then from the point of view of
               FRAME1, `p' rotates (in a right handed sense) about
               an axis parallel to `av'. Moreover the rate of rotation
               in radians per unit time is given by the length of
               `av'.

               More formally, the velocity `v' of `p' in FRAME1 is
               given by
                                  T
                  v  =  av x ( rot  * p )

-Detailed_Output

   xform       is a state transformation matrix associated
               with `rot' and `av'. If `s1' is the state of an object
               with respect to FRAME1, then the state `s2' of the
               object with respect to FRAME2 is given by

                  s2  =  xform * s1

               where "*" denotes Matrix-Vector multiplication.

-Parameters

   None.

-Exceptions

   Error free.

   1)  No checks are performed on `rot' to ensure that it is indeed
       a rotation matrix.

-Files

   None.

-Particulars

   This routine is essentially a macro routine for converting
   a rotation and angular velocity of the rotation to the
   equivalent state transformation matrix.

   This routine is an inverse of xf2rav_c.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) The following example program uses ckgpav_c to get C-matrix
      and associated angular velocity vector for an image whose
      SCLK count (un-encoded character string version) is known.

      From that matrix and angular velocity vector, the associated
      state transformation matrix is obtained.

      Note that we need to load a SCLK kernel to convert from clock
      string to "ticks." Although not required for older spacecraft
      clocks, most modern spacecraft ones require a leapseconds
      kernel to be loaded in addition to a SCLK kernel.


      Use the meta-kernel shown below to load the required SPICE
      kernels.


         KPL/MK

         File name: rav2xf_ex1.tm

         This meta-kernel is intended to support operation of SPICE
         example programs. The kernels shown here should not be
         assumed to contain adequate or correct versions of data
         required by SPICE-based user applications.

         In order for an application to use this meta-kernel, the
         kernels referenced here must be present in the user's
         current working directory.

         The names and contents of the kernels referenced
         by this meta-kernel are as follows:

            File name              Contents
            --------------------   -----------------------
            cas00071.tsc           CASSINI SCLK
            04161_04164ra.bc       CASSINI spacecraft
                                   reconstructed CK

         \begindata

           KERNELS_TO_LOAD = ( 'cas00071.tsc'
                               '04161_04164ra.bc' )

         \begintext

         End of meta-kernel


      Example code begins here.


      /.
         Program rav2xf_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Constants for this program.

         -- The code for the CASSINI spacecraft clock is -82.

         -- The code for CASSINI spacecraft reference frame is
            -82000.

         --  Spacecraft clock tolerance is 1.0 seconds. This may
            not be an acceptable tolerance for some applications.
            It must be converted to "ticks" (units of encoded
            SCLK) for input to ckgpav_c.

         -- The reference frame we want is J2000.
         ./
         #define META         "rav2xf_ex1.tm"
         #define REFFRM       "J2000"
         #define SCLKCH       "1/1465476046.160"
         #define SCLTOL       "1.0"
         #define SCID         -82
         #define INSTID       -82000

         /.
         Local variables.
         ./
         SpiceDouble          av     [3];
         SpiceDouble          clkout;
         SpiceDouble          cmat   [3][3];
         SpiceDouble          fxmat  [6][6];
         SpiceDouble          sclkdp;
         SpiceDouble          toltik;

         SpiceInt             i;

         SpiceBoolean         found;

         /.
         Load kernels.
         ./
         furnsh_c ( META );

         /.
         Convert tolerance from CASSINI formatted character
         string SCLK to ticks which are units of encoded SCLK.
         ./
         sctiks_c ( SCID, SCLTOL, &toltik );

         /.
         ckgpav_c requires encoded spacecraft clock.
         ./
         scencd_c ( SCID, SCLKCH, &sclkdp );

         ckgpav_c ( INSTID, sclkdp, toltik,  REFFRM,
                    cmat,   av,     &clkout, &found );

         /.
         Recall that `cmat' and `av' are the rotation and angular
         velocity of the transformation from J2000 to the
         spacecraft frame.
         ./
         if ( found )
         {

            /.
            Display `cmat' and `av'.
            ./
            printf( "Rotation matrix:\n" );
            for ( i = 0; i < 3; i++ )
            {

               printf( "%10.6f %9.6f %9.6f\n",
                       cmat[i][0], cmat[i][1], cmat[i][2] );

            }

            printf( "Angular velocity:\n" );
            printf( "%20.16f %19.16f %19.16f\n", av[0], av[1], av[2] );

            /.
            Get state transformation from J2000 to the spacecraft
            frame.
            ./
            rav2xf_c ( cmat, av, fxmat );

            /.
            Display the results.
            ./
            printf( "\n" );
            printf( "State transformation matrix:\n" );
            for ( i = 0; i < 6; i++ )
            {

               printf( "%10.6f %9.6f %9.6f %9.6f %9.6f %9.6f\n",
                       fxmat[i][0], fxmat[i][1], fxmat[i][2],
                       fxmat[i][3], fxmat[i][4], fxmat[i][5] );

            }
         }
         else
         {
            printf( "No rotation matrix/angular velocity found for %s\n",
                                                                   SCLKCH );
         }

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Rotation matrix:
       -0.604984  0.796222 -0.005028
       -0.784160 -0.596891 -0.169748
       -0.138158 -0.098752  0.985475
      Angular velocity:
        0.0000032866819065 -0.0000099372638338  0.0000197597699770

      State transformation matrix:
       -0.604984  0.796222 -0.005028  0.000000  0.000000  0.000000
       -0.784160 -0.596891 -0.169748  0.000000  0.000000  0.000000
       -0.138158 -0.098752  0.985475  0.000000  0.000000  0.000000
       -0.000016 -0.000012 -0.000003 -0.604984  0.796222 -0.005028
        0.000013 -0.000015 -0.000010 -0.784160 -0.596891 -0.169748
       -0.000008 -0.000006 -0.000002 -0.138158 -0.098752  0.985475


-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   W.L. Taber          (JPL)
   E.D. Wright         (JPL)

-Version

   -CSPICE Version 1.0.2, 16-JUL-2020 (JDR)

       Edited the header to comply with NAIF standard. Added complete
       code example based on existing example.

       Added ROTATION to the required readings.

   -CSPICE Version 1.0.1, 12-APR-2007 (EDW)

       Edit to abstract.

   -CSPICE Version 1.0.0, 18-JUN-1999 (WLT) (NJB)

-Index_Entries

   State transformation to rotation and angular velocity

-&
*/

   { /* Begin rav2xf_c */


   /*
   Local variables
   */

   SpiceDouble             drdt   [3][3];
   SpiceDouble             omegat [3][3];

   SpiceInt                i;
   SpiceInt                j;



   /*
   Error free:  no tracing required.


   A state transformation matrix xform has the following form


       [      |     ]
       |  r   |  0  |
       |      |     |
       | -----+-----|
       |  dr  |     |
       |  --  |  r  |
       [  dt  |     ]


   where r is a rotation and dr/dt is the time derivative of that
   rotation.  From this we can immediately fill in most of the
   state transformation matrix.
   */



   for ( i = 0;  i < 3;  i++ )
      {
      for ( j = 0;  j < 3;  j++ )
         {
         xform[i  ][j  ]  =  rot [i][j];
         xform[i+3][j+3]  =  rot [i][j];
         xform[i  ][j+3]  =  0.;
         }
      }



   /*
   Now for the rest.

   Recall that rot is a transformation that converts positions
   in some frame frame1 to positions in a second frame frame2.

   The angular velocity matrix omega (the cross product matrix
   corresponding to av) has the following property.

   If p is the position of an object that is stationary with
   respect to frame2 then the velocity v of that object in frame1
   is given by:
                        t
       v  =  omega * rot  *  p

   But v is also given by

                  t
             d rot
       v =   -----  * p
               dt

   So that
                                t
                  t        d rot
       omega * rot    =   -------
                             dt

   Hence

        d rot                 t
        -----   =  rot * omega
          dt


   From this discussion we can see that we need omega transpose.
   Here it is.
   */

   omegat[0][0] =  0.0;
   omegat[1][0] = -av[2];
   omegat[2][0] =  av[1];

   omegat[0][1] =  av[2];
   omegat[1][1] =  0.0;
   omegat[2][1] = -av[0];

   omegat[0][2] = -av[1];
   omegat[1][2] =  av[0];
   omegat[2][2] =  0.0;


   mxm_c ( rot, omegat, drdt );


   for ( i = 0;  i < 3;  i++ )
      {
      for ( j = 0;  j < 3;  j++ )
         {
         xform[i+3][j]  =  drdt [i][j];
         }
      }


   } /* End rav2xf_c */
