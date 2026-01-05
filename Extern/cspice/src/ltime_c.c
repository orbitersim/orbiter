/*

-Procedure ltime_c ( Light Time )

-Abstract

   Compute the transmission (or reception) time of a signal at a
   specified target, given the reception (or transmission) time at a
   specified observer. Also return the elapsed time between
   transmission and reception.

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

   SPK

*/
   #include <string.h>
   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZmc.h"


   void ltime_c ( SpiceDouble        etobs,
                  SpiceInt           obs,
                  ConstSpiceChar   * dir,
                  SpiceInt           targ,
                  SpiceDouble      * ettarg,
                  SpiceDouble      * elapsd  )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   etobs      I   Epoch of a signal at some observer.
   obs        I   NAIF ID of some observer.
   dir        I   Direction the signal travels ( "->" or "<-" ).
   targ       I   NAIF ID of the target object.
   ettarg     O   Epoch of the signal at the target.
   elapsd     O   Time between transmit and receipt of the signal.

-Detailed_Input

   etobs       is an epoch expressed as ephemeris seconds past J2000
               TDB. This is the time at which an electromagnetic
               signal is "at" the observer.

   obs         is the NAIF ID of some observer.

   dir         is the direction the signal travels. The
               acceptable values are "->" and "<-". When
               you read the calling sequence from left to
               right, the "arrow" given by `dir' indicates
               which way the electromagnetic signal is traveling.

               If the argument list reads as below,

                  ..., `obs', "->", `targ', ...

               the signal is traveling from the observer to the
               target.

               If the argument reads as

                  ..., `obs', "<-", `targ'

               the signal is traveling from the target to
               the observer.

   targ        is the NAIF ID of the target.

-Detailed_Output

   ettarg      is the epoch expressed as ephemeris seconds past J2000
               TDB at which the electromagnetic signal is "at" the
               target body.

               Note `ettarg' is computed using only Newtonian
               assumptions about the propagation of light.

   elapsd      is the number of ephemeris seconds (TDB) between
               transmission and receipt of the signal.

               `elapsd' is computed as:

                  elapsd = fabs( etobs - ettarg )

-Parameters

   None.

-Exceptions

   1)  If `dir' is not one of "->" or "<-", the error
       SPICE(BADDIRECTION) is signaled by a routine in the call tree
       of this routine. In this case `ettarg' and `elapsd' will not be
       altered from their input values.

   2)  If insufficient ephemeris information is available to
       compute the outputs `ettarg' and `elapsd', or if observer
       or target is not recognized, an error is signaled
       by a routine in the call tree of this routine.

       In this case, the value of `ettarg' will be set to `etobs'
       and `elapsd' will be set to zero.

   3)  If the `dir' input string pointer is null, the error
       SPICE(NULLPOINTER) is signaled.

   4)  If the `dir' input string has zero length, the error
       SPICE(EMPTYSTRING) is signaled.

-Files

   None.

-Particulars

   Suppose a radio signal travels between two solar system objects.
   Given an ephemeris for the two objects, which way the signal is
   traveling, and the time when the signal is "at" at one of the
   objects (the observer obs), this routine determines when the signal
   is "at" the other object (the target targ).  It also returns the
   elapsed time between transmission and receipt of the signal.

-Examples

   1) Suppose a signal is transmitted at time et from the Goldstone
      tracking site (ID code 399001) to a spacecraft whose ID code
      is -77.


         signal traveling to spacecraft
         *  -._.-._.-._.-._.-._.-._.-._.-._.->  *

         Goldstone (obs=399001)            Spacecraft (targ = -77)
         at epoch etobs(given)             at epoch ettarg(unknown)

      Assuming that all of the required SPICE kernels have been
      loaded, the code fragment below shows how to compute the
      time (arrive) at which the signal arrives at the spacecraft
      and how long (howlng) it took the signal to reach the spacecraft.
      (Note that we display the arrival time as the number of seconds
      past J2000.)

         #include <stdio.h>
         #include "SpiceUsr.h"
               .
               .
               .
         #define  OBS            399001
         #define  TARG           -77
         #define  LENOUT         81
         #define  OBSUTC         "1999 May 25"
         #define  LSK            "leapseconds.ker"

         SpiceChar               timestr [ LENOUT ];

         SpiceDouble             arrive;
         SpiceDouble             howlng;
         SpiceDouble             etobs;
         SpiceDouble             sent;


         [ load kernels ]

         str2et_c ( OBSUTC, &etobs );

         ltime_c ( etobs,   OBS,    "->",   TARG,  &arrive, &howlng );
         etcal_c ( arrive,  LENOUT, timestr );

         printf ( "The signal arrived at time: %s\n",       timestr );
         printf ( "It took %15.6f seconds to get there.\n", howlng  );


   2) Suppose a signal is received at the Goldstone tracking sight
      at epoch ET from the spacecraft of the previous example.

              signal sent from spacecraft
         *  <-._.-._.-._.-._.-._.-._.-._.-._.- *

         Goldstone (OBS=399001)               Spacecraft (TARG = -77)
         at epoch ETOBS(given)                at epoch ETTARG(unknown)

      Again assuming that all the required kernels have been loaded
      the code fragment below computes the epoch at which the
      signal was transmitted from the spacecraft.

         ltime_c ( etobs,   OBS,    "<-",   TARG,  &sent, &howlng );
         etcal_c ( sent,    LENOUT, timestr );

         printf ( "The signal was transmitted at: %s\n",    timestr );
         printf ( "It took %15.6f seconds to get there.\n", howlng );


   3) Suppose there is a transponder on board the spacecraft of
      the previous examples that transmits a signal back to the
      sender exactly 1 microsecond after a signal arrives at
      the spacecraft. If we send a signal from Goldstone
      to the spacecraft and wait to receive it at Canberra.
      What will be the epoch at which the return signal arrives
      in Canberra? ( The ID code for Canberra is 399002 ).

      Again, assuming we've loaded all the necessary kernels,
      the fragment below will give us the answer.

         #define   GSTONE        399001
         #define   SC            -77
         #define   CANBER        399002

         str2et_c ( OBSUTC, &etgold );

         ltime_c ( etgold, GSTONE, "->", SC, &scget, &lt1 );

         /.
         Account for the microsecond delay between receipt and
         transmission.
         ./
         scsend = scget + 0.000001;

         ltime_c ( scsend, SC, "->", CANBER, &etcanb, &lt2 );

         rndtrp = etcanb - etgold;

         printf ( "The  signal arrives in Canberra at ET: %15.6f\n"
                  "Round trip time for the signal was:    %15.6f\n",
                  etcanb,
                  rndtrp                                            );

-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   W.L. Taber          (JPL)

-Version

   -CSPICE Version 1.0.2, 01-NOV-2021 (JDR)

       Edited the header to comply with NAIF standard.

   -CSPICE Version 1.0.1, 09-NOV-2006 (NJB)

       Corrected a reference to the function j2000_c; this had been
       erroneously changed from the name J2000 to j2000_c
       during translation from Fortran.

       Re-ordered header sections to conform to standard.

   -CSPICE Version 1.0.0, 29-MAY-1999 (WLT) (NJB)

-Index_Entries

   Compute uplink and downlink light time

-&
*/

{ /* Begin ltime_c */


   /*
   Participate in error tracing.
   */
   chkin_c ( "ltime_c" );


   /*
   Check the input direction string.  The pointer must be non-null
   and the string length must be at least 1.
   */
   CHKFSTR ( CHK_STANDARD, "ltime_c", dir );


   /*
   Call the f2c'd routine.
   */
   ltime_ (  ( doublereal   * ) &etobs,
             ( integer      * ) &obs,
             ( char         * ) dir,
             ( integer      * ) &targ,
             ( doublereal   * ) ettarg,
             ( doublereal   * ) elapsd,
             ( ftnlen         ) strlen(dir) );


   chkout_c ( "ltime_c" );

} /* End ltime_c */
