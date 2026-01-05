/*

-Procedure spkpvn_c ( S/P Kernel, position and velocity in native frame )

-Abstract

   Return, for a specified SPK segment and time, the state (position
   and velocity) of the segment's target body relative to its center
   of motion.

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

   SPK

-Keywords

   EPHEMERIS

*/
   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZst.h"
   #undef spkpvn_c

   void spkpvn_c ( SpiceInt             handle,
                   ConstSpiceDouble     descr [5],
                   SpiceDouble          et,
                   SpiceInt           * ref,
                   SpiceDouble          state [6],
                   SpiceInt           * center    )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   handle     I   File handle.
   descr      I   Segment descriptor.
   et         I   Evaluation epoch.
   ref        O   Segment reference frame ID code.
   state      O   Output state vector.
   center     O   Center of state.

-Detailed_Input

   handle,
   descr       are, respectively, the file handle assigned to a SPK
               file and the descriptor for a segment within the file.
               Together they determine the ephemeris data from which
               the state of the body is to be computed.

   et          is the epoch (ephemeris time) at which the state
               is to be computed. `et' is expressed as seconds
               past J2000 TDB.

-Detailed_Output

   ref         is the ID code of the reference frame relative to which
               the state returned by the routine is expressed.

   state       contains the position and velocity, at epoch `et',
               for the body covered by the specified segment.
               `state' has six elements: the first three contain the
               body's position; the last three contain the body's
               velocity. These vectors are expressed into the
               specified  reference frame. Units are always km and
               km/sec.

   center      is the integer ID code of the center of motion for
               the state.

-Parameters

   None.

-Exceptions

   1)  If the segment type is not supported by the current
       version of spkpvn_c, the error SPICE(SPKTYPENOTSUPP)
       is signaled by a routine in the call tree of this routine.

-Files

   See argument `handle'.

-Particulars

   For most user applications, the high-level SPK APIs

      spkezr_c
      spkez_c
      spkpos_c
      spkezp_c

   should be used instead of this routine.

   The f2c'd routine spkpvn_ called by spkpvn_c is the most basic of
   the SPK readers, the reader upon which spkezr_c and spkgeo_c, etc.
   are built. spkpvn_c normally should not be called directly except in
   cases where some optimization is required. (That is, where the
   calling program has prior knowledge of the center-barycenter shifts
   to be performed, or a non-standard method of determining the files
   and segments to be used when computing states.)

   This is the only reader that makes distinctions between the
   various segment types in the SPK format. The complete list
   of types currently supported is shown below.

      Type   Description
      ----   -------------------------
         1   Modified Difference Array
         2   Chebyshev (P)
         3   Chebyshev (P,V)
         5   Two body propagation between discrete states
         8   Lagrange interpolation, equally spaced discrete states
         9   Lagrange interpolation, unequally spaced discrete states
        12   Hermite interpolation, equally spaced discrete states
        13   Hermite interpolation, unequally spaced discrete states
        14   Chebyshev Unequally spaced
        15   Precessing Ellipse
        17   Equinoctial Elements
        18   ESOC/DDID Hermite/Lagrange Interpolation
        19   ESOC/DDID Piecewise Interpolation
        21   Extended Modified Difference Array

      The maximum record lengths for each data type currently
      supported are as follows:

                Data type       Maximum record length
                ---------       ---------------------
                    1                    71
                    2                    87
                    3                   171
                    5                    15
                    8                   171
                    9                   197
                   12                    87
                   13                    99
                   14                 Variable
                   15                    16
                   17                    12
                   18                   198
                   19                   198
                   21                    96

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) In the following code example, an SPK file is searched for
      a segment containing ephemeris data for the Jupiter system
      barycenter at a particular epoch. Using this segment,
      states of the Jupiter system barycenter relative to the
      solar system barycenter are evaluated at a sequence of times.

      This method of state computation minimizes the number of
      segment searches required to obtain requested data, but
      it bypasses the SPK subsystem's state chaining mechanism.

      Use the meta-kernel shown below to load the required SPICE
      kernels.


         KPL/MK

         File name: spkpvn_ex1.tm

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
            naif0010.tls                  Leapseconds

         \begindata

            KERNELS_TO_LOAD = ( 'de421.bsp',
                                'naif0010.tls'  )

         \begintext

         End of meta-kernel


      Example code begins here.


      /.
         Program spkpvn_ex1
      ./
      #include <stdio.h>
      #include <stdlib.h>
      #include "SpiceUsr.h"

      int main()
      {
         /.
         Local constants
         ./
         #define FRNMLN   33
         #define META     "spkpvn_ex1.tm"
         #define DSCSIZ   5
         #define ND       2
         #define NI       6
         #define SIDLEN   41
         #define TIMFMT   "YYYY MON DD HR:MN:SC.######::TDB TDB"
         #define TIMLEN   41

         /.
         Local variables
         ./
         SpiceBoolean            found;

         SpiceChar               frname [ FRNMLN ];
         SpiceChar               segid  [ SIDLEN ];
         SpiceChar             * timstr;
         SpiceChar               outstr [ TIMLEN ];

         SpiceDouble             dc     [ ND ] ;
         SpiceDouble             descr  [ DSCSIZ ];
         SpiceDouble             et;
         SpiceDouble             et0;
         SpiceDouble             state  [ 6 ];

         SpiceInt                body;
         SpiceInt                center;
         SpiceInt                handle;
         SpiceInt                i;
         SpiceInt                ic     [ NI ];
         SpiceInt                refID;

         /.
         Load meta-kernel.
         ./
         furnsh_c ( META );

         /.
         Convert starting time to seconds past J2000 TDB.
         ./
         timstr = "2012 APR 27 00:00:00.000 TDB";

         str2et_c ( timstr, &et0 );

         /.
         Find a loaded segment for the Jupiter barycenter
         that covers `et0'.
         ./
         body = 5;
         spksfs_c ( body,    et0,   SIDLEN,
                    &handle, descr, segid,  &found );

         if ( !found )
         {
            setmsg_c ( "No SPK segment available for body # at "
                       "time #."                                );
            errint_c ( "#",  body                               );
            errch_c  ( "#",  timstr                             );
            sigerr_c ( "SPICE(NOSEGMENT)"                       );
         }

         /.
         Unpack the descriptor of the current segment.
         ./
         dafus_c ( descr, 2, 6, dc, ic );

         frmnam_c ( ic[2], FRNMLN, frname );

         printf ( "\n"
                  " Body       = %d\n"
                  " Center     = %d\n"
                  " Frame      = %s\n"
                  " Data type  = %d\n"
                  " Start ET   = %24.17e\n"
                  " Stop ET    = %24.17e\n"
                  " Segment ID = %s\n",
                  (int)ic[0],
                  (int)ic[1],
                  frname,
                  (int)ic[3],
                  dc[0],
                  dc[1],
                  segid                 );

         /.
         Evaluate states at 10-second steps, starting at `et0'
         and continuing for 20 seconds.
         ./

         for ( i = 0;  i < 3;  i++ )
         {
            et = et0 + ( 10.0 * i );

            /.
            Convert `et' to a string for display.
            ./
            timout_c ( et, TIMFMT, TIMLEN, outstr );

            /.
            Attempt to compute a state only if the segment's
            coverage interval contains `et'.
            ./
            if ( et <= dc[1] )
            {
               /.
               This segment has data at `et'. Evaluate the
               state of the target relative to its center
               of motion.
               ./
               spkpvn_c ( handle, descr, et, &refID, state, &center );

               /.
               Display the time and state.
               ./
               printf ( "\n"
                        " %s\n"
                        "   Position X (km):   %24.17e\n"
                        "   Position Y (km):   %24.17e\n"
                        "   Position Z (km):   %24.17e\n"
                        "   Velocity X (km/s): %24.17e\n"
                        "   Velocity Y (km/s): %24.17e\n"
                        "   Velocity Z (km/s): %24.17e\n",
                        outstr,
                        state[0], state[1], state[2],
                        state[3], state[4], state[5]     );
            }
            else
            {
               printf ( "\nNo data for body %d found at %s.\n",
                        (int)body,
                        outstr                                 );
               exit(0);
            }
         }
         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


       Body       = 5
       Center     = 0
       Frame      = J2000
       Data type  = 2
       Start ET   = -3.16919520000000000e+09
       Stop ET    =  1.69685280000000000e+09
       Segment ID = DE-0421LE-0421

       2012 APR 27 00:00:00.000000 TDB
         Position X (km):    4.64528993982164860e+08
         Position Y (km):    5.41513126156852007e+08
         Position Z (km):    2.20785135624629408e+08
         Velocity X (km/s): -1.03868564830765493e+01
         Velocity Y (km/s):  7.95324700713742416e+00
         Velocity Z (km/s):  3.66185835431306517e+00

       2012 APR 27 00:00:10.000000 TDB
         Position X (km):    4.64528890113592625e+08
         Position Y (km):    5.41513205689313412e+08
         Position Z (km):    2.20785172243209451e+08
         Velocity X (km/s): -1.03868579616041927e+01
         Velocity Y (km/s):  7.95324528430304944e+00
         Velocity Z (km/s):  3.66185765185608103e+00

       2012 APR 27 00:00:20.000000 TDB
         Position X (km):    4.64528786245005608e+08
         Position Y (km):    5.41513285221757650e+08
         Position Z (km):    2.20785208861782461e+08
         Velocity X (km/s): -1.03868594401314791e+01
         Velocity Y (km/s):  7.95324356146845002e+00
         Velocity Z (km/s):  3.66185694939899253e+00


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

   -CSPICE Version 1.0.2, 27-AUG-2021 (JDR)

       Edited the header to comply with NAIF standard.

   -CSPICE Version 1.0.1, 12-JUL-2016 (EDW)

       Edit to example program to use "%d" with explicit casts
       to int for printing SpiceInts with printf.

   -CSPICE Version 1.0.0, 05-OCT-2012 (NJB) (WLT)

-Index_Entries

   position and velocity from ephemeris
   SPK file position and velocity

-&
*/

{ /* Begin spkpvn_c */


   /*
   Participate in error tracing.
   */
   chkin_c ( "spkpvn_c" );

   spkpvn_ ( ( integer     * ) &handle,
             ( doublereal  * ) descr,
             ( doublereal  * ) &et,
             ( integer     * ) ref,
             ( doublereal  * ) state,
             ( integer     * ) center  );

   chkout_c ( "spkpvn_c" );

} /* End spkpvn_c */
