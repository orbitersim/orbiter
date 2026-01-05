/*

-Procedure dskgd_c ( DSK, return DSK segment descriptor  )

-Abstract

   Return the DSK descriptor from a DSK segment identified
   by a DAS handle and DLA descriptor.

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

   DAS
   DSK
   NAIF_IDS

-Keywords

   DAS
   DSK
   FILES
   TOPOGRAPHY

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZmc.h"
   #include "SpiceZim.h"
   #undef dskgd_c


   void dskgd_c ( SpiceInt               handle,
                  ConstSpiceDLADescr   * dladsc,
                  SpiceDSKDescr        * dskdsc )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   handle     I   Handle of a DSK file.
   dladsc     I   DLA segment descriptor.
   dskdsc     O   DSK segment descriptor.

-Detailed_Input

   handle      is the handle of a DSK file that is open for
               read access.

   dladsc      is the DLA segment descriptor corresponding to
               a DSK segment.

-Detailed_Output

   dskdsc      is the DSK segment descriptor of the segment
               designated by the input handle and DLA descriptor.

-Parameters

   See the header file

      SpiceDLA.h

   for declarations of DLA descriptor sizes and documentation of the
   contents of DLA descriptors.

   See the header file

      SpiceDSK.h

   for declarations of DSK descriptor sizes and documentation of the
   contents of DSK descriptors.

-Exceptions

   1)  If the size of the double precision component of the segment
       is smaller than that of a DSK descriptor, the error
       SPICE(INVALIDFORMAT) is signaled by a routine in the call tree
       of this routine.

   2)  If the input handle is invalid, an error is signaled by a
       routine in the call tree of this routine.

   3)  If the input DLA descriptor is invalid, the effect of this
       routine is undefined. The error *may* be diagnosed by
       routines in the call tree of this routine, but there are no
       guarantees.

   4)  If any DAS read error is detected, the error is signaled by a
       routine in the call tree of this routine.

-Files

   See input argument `handle'.

-Particulars

   This is a convenience routine intended for use by low-level
   routines that read DSK segments. This routine may also be called
   by user applications that must access DSK files at the segment
   level.

-Examples

   The numerical results shown for these examples may differ across
   platforms. The results depend on the SPICE kernels used as input,
   the compiler and supporting libraries, and the machine specific
   arithmetic implementation.

   1) Dump the DSK descriptors of a DSK file.


      Example code begins here.


      /.
         Program dskgd_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main()
      {
         /.
         Local constants
         ./
         #define FILSIZ              256

         /.
         Local variables
         ./
         SpiceChar               dsk    [FILSIZ];
         SpiceBoolean            found;
         SpiceDLADescr           dladsc;
         SpiceDLADescr           nxtdsc;
         SpiceDSKDescr           dskdsc;
         SpiceInt                handle;
         SpiceInt                i;

         /.
         Prompt for the name of a DSK file; open the file.
         ./
         prompt_c ( "Enter DSK name > ", FILSIZ, dsk );
         dasopr_c ( dsk, &handle );

         /.
         Search for the first segment in the file; obtain
         the segment's DLA descriptor.
         ./
         dlabfs_c ( handle, &nxtdsc, &found );

         while ( found )
         {
            dladsc = nxtdsc;

            dskgd_c ( handle, &dladsc, &dskdsc );

            printf ( "\nDSK descriptor contents:\n" );

            printf ( "   %15d\n",  (int) dskdsc.surfce );
            printf ( "   %15d\n",  (int) dskdsc.center );
            printf ( "   %15d\n",  (int) dskdsc.dclass );
            printf ( "   %15d\n",  (int) dskdsc.dtype  );
            printf ( "   %15d\n",  (int) dskdsc.frmcde );
            printf ( "   %15d\n",  (int) dskdsc.corsys );

            for ( i = 0; i < SPICE_DSK_NSYPAR;  i++ )
            {
               printf ( "   %15.6f\n",        dskdsc.corpar[i] );
            }

            printf ( "   %15.6f\n",    dskdsc.co1min );
            printf ( "   %15.6f\n",    dskdsc.co1max );
            printf ( "   %15.6f\n",    dskdsc.co2min );
            printf ( "   %15.6f\n",    dskdsc.co2max );
            printf ( "   %15.6f\n",    dskdsc.co3min );
            printf ( "   %15.6f\n",    dskdsc.co3max );
            printf ( "% 15.6f\n",   dskdsc.start  );
            printf ( "% 15.6f\n",   dskdsc.stop   );

            /.
            Fetch next DLA descriptor.
            ./
            dlafns_c ( handle, &dladsc, &nxtdsc, &found );
         }
         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, using the DSK file named phobos512.bds, the output
      was:


      Enter DSK name > phobos512.bds

      DSK descriptor contents:
                     401
                     401
                       1
                       2
                   10021
                       1
                0.000000
                0.000000
                0.000000
                0.000000
                0.000000
                0.000000
                0.000000
                0.000000
                0.000000
                0.000000
               -3.141593
                3.141593
               -1.570796
                1.570796
                8.049632
               13.940940
      -1577879958.816059
       1577880066.183913


   2) Again, dump the DSK descriptors of a DSK file, this time
      interpreting the descriptor information and displaying
      it in a user-friendly form. This display is a simplified
      version of that created by the utility DSKBRIEF.

      This program requests the name of an optional meta-kernel.
      The meta-kernel can be used to define surface name-ID
      associations. If no meta-kernel is needed, the user can
      enter a carriage return at the prompt for this file.


      Example code begins here.


      /.
         Program dskgd_ex2
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main()
      {
         /.
         Local constants
         ./
         #define BDNMLN          37
         #define FILSIZ          256
         #define FRNMLN          33
         #define TIMLEN          41
         #define NSYS             4
         #define NAMLEN          21
         #define NCLASS           2
         #define SFNMLN          SPICE_SRF_SFNMLN

         /.
         Local variables
         ./
         SpiceBoolean            found;
         SpiceBoolean            isname;

         SpiceChar               bodnam [ SFNMLN ];
         SpiceChar               btime  [ TIMLEN ];

         SpiceChar             * clsnms [2] =
                                { "Single-valued surface",
                                  "General surface"       };

         SpiceChar               dsk    [ FILSIZ ];
         SpiceChar               etime  [ TIMLEN ];
         SpiceChar               frame  [ FRNMLN ];
         SpiceChar               meta   [ FILSIZ ];
         SpiceChar               srfnam [ SFNMLN ];
         SpiceChar             * sysnam;

         SpiceChar             * sysnms [ NSYS ] =
                                  { "Latitudinal", "Cylindrical",
                                    "Rectangular", "Planetodetic" };

         SpiceDLADescr           dladsc;
         SpiceDLADescr           nxtdsc;

         SpiceDouble             f;
         SpiceDouble             re;
         SpiceDouble             rp;

         SpiceDSKDescr           dskdsc;

         SpiceInt                bodyid;
         SpiceInt                corsys;
         SpiceInt                dclass;
         SpiceInt                dtype;
         SpiceInt                framid;
         SpiceInt                handle;
         SpiceInt                segno;
         SpiceInt                surfid;


         prompt_c ( "Enter DSK name         > ", FILSIZ, dsk  );
         prompt_c ( "Enter meta-kernel name > ", FILSIZ, meta );

         if (    ( !eqstr_c( meta, " ") )
              && ( !eqstr_c( meta, "" ) )  )
         {
            furnsh_c ( meta );
         }

         /.
         Open the DLA file and begin a forward search
         for segments.
         ./
         dasopr_c ( dsk, &handle );

         segno = 0;

         dlabfs_c ( handle, &nxtdsc, &found );

         while ( found )
         {
            ++segno;

            /.
            Make the DLA descriptor we just fetched
            the current one.
            ./
            dladsc = nxtdsc;

            dskgd_c ( handle, &dladsc, &dskdsc );

            bodyid = dskdsc.center;
            surfid = dskdsc.surfce;
            framid = dskdsc.frmcde;
            dtype  = dskdsc.dtype;
            dclass = dskdsc.dclass;

            bodc2s_c ( bodyid, BDNMLN, bodnam );
            srfc2s_c ( surfid, bodyid, SFNMLN, srfnam, &isname );
            frmnam_c ( framid, FRNMLN, frame  );

            if ( eqstr_c( frame, " " ) )
            {
               sprintf ( frame, "%d", (int)framid );
            }

            etcal_c ( dskdsc.start, TIMLEN, btime );
            etcal_c ( dskdsc.stop,  TIMLEN, etime );

            corsys = dskdsc.corsys;
            sysnam = sysnms[corsys-1];

            printf ( "%s\n"
                     " DSK descriptor for segment %d\n"
                     "  Body:              %s\n"
                     "  Surface:           %s\n"
                     "  Frame:             %s\n"
                     "  Start time (TDB):  %s\n"
                     "  Stop time  (TDB):  %s\n"
                     "  Data type:         %d\n"
                     "  Data class:        %s\n"
                     "  Coordinate System: %s\n",
                     "====================================",
                     (int) segno,
                     bodnam,
                     srfnam,
                     frame,
                     btime,
                     etime,
                     (int) dtype,
                     clsnms[ dclass-1 ],
                     sysnam                                );

            if ( corsys == SPICE_DSK_PDTSYS )
            {
               re = dskdsc.corpar[0];
               f  = dskdsc.corpar[1];
               rp = re * (1.0 - f);

               printf ( "    Equatorial radius (km): %21.14f\n"
                        "    Polar radius      (km): %21.14f\n",
                        re, rp                                  );
            }

            printf ( "  Segment boundaries:\n" );

            if ( corsys == SPICE_DSK_LATSYS )
            {
               printf ( "    Longitude (deg):   %21.14f  %21.14f\n"
                        "    Latitude  (deg):   %21.14f  %21.14f\n"
                        "    Radius     (km):   %21.14f  %21.14f\n",
                        dskdsc.co1min * dpr_c(),
                        dskdsc.co1max * dpr_c(),
                        dskdsc.co2min * dpr_c(),
                        dskdsc.co2max * dpr_c(),
                        dskdsc.co3min,
                        dskdsc.co3max                      );
            }
            else if ( corsys == SPICE_DSK_CYLSYS )
            {
               setmsg_c ( "Coordinate system was Cylindrical." );
               sigerr_c ( "SPICE(NOTSUPPORTED)" );
            }
            else if ( corsys == SPICE_DSK_RECSYS )
            {
               printf ( "    X-coordinate (km): %21.14f  %21.14f\n"
                        "    Y-coordinate (km): %21.14f  %21.14f\n"
                        "    Z-coordinate (km): %21.14f  %21.14f\n",
                        dskdsc.co1min,
                        dskdsc.co1max,
                        dskdsc.co2min,
                        dskdsc.co2max,
                        dskdsc.co3min,
                        dskdsc.co3max                      );
            }
            else if ( corsys == SPICE_DSK_PDTSYS )
            {
               printf ( "    Longitude (deg):   %21.14f  %21.14f\n"
                        "    Latitude  (deg):   %21.14f  %21.14f\n"
                        "    Altitude   (km):   %21.14f  %21.14f\n",
                        dskdsc.co1min * dpr_c(),
                        dskdsc.co1max * dpr_c(),
                        dskdsc.co2min * dpr_c(),
                        dskdsc.co2max * dpr_c(),
                        dskdsc.co3min,
                        dskdsc.co3max                      );
            }
            /.
            Find the next segment, if it exists.
            ./
            dlafns_c ( handle, &dladsc, &nxtdsc, &found );
         }
         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, using the DSK file named phobos512.bds and an empty
      string instead of the meta-kernel name, the output was:


      Enter DSK name         > phobos512.bds
      Enter meta-kernel name >
      ====================================
       DSK descriptor for segment 1
        Body:              PHOBOS
        Surface:           401
        Frame:             IAU_PHOBOS
        Start time (TDB):  1950 JAN 01 00:00:41.183
        Stop time  (TDB):  2050 JAN 01 00:01:06.183
        Data type:         2
        Data class:        Single-valued surface
        Coordinate System: Latitudinal
        Segment boundaries:
          Longitude (deg):     -180.00000000000000     180.00000000000000
          Latitude  (deg):      -90.00000000000000      90.00000000000000
          Radius     (km):        8.04963224872155      13.94093983212395


   3) Again, dump the DSK descriptors of a DSK file, using the
      program from example 2, but this time reading the DSK file

         phobos_3_3_3seg.bds

      which can be created by running an example program from
      dskw02_c. Use the meta-kernel shown below to demonstrate surface
      name-ID mapping.


         KPL/MK

         File: dskgd_ex3.tm

         This meta-kernel is intended to support operation of SPICE
         example programs. The file contents shown here should not be
         assumed to contain adequate or correct versions of data
         required by SPICE-based user applications.


         \begindata

         NAIF_SURFACE_NAME += ( 'Phobos example surface 1',
                                'Phobos example surface 2',
                                'Phobos example surface 3' )
         NAIF_SURFACE_CODE += (   1,   2,   3 )
         NAIF_SURFACE_BODY += ( 401, 401, 401 )

         \begintext


         End of meta-kernel


      When Example #2 was executed on a Mac/Intel/cc/64-bit
      platform, using the DSK file named phobos_3_3_3seg.bds and the
      meta-kernel dskgd_ex3.tm, the output was:


      Enter DSK name         > phobos_3_3_3seg.bds
      Enter meta-kernel name > dskgd_ex3.tm
      ====================================
       DSK descriptor for segment 1
        Body:              PHOBOS
        Surface:           Phobos example surface 1
        Frame:             IAU_PHOBOS
        Start time (TDB):  1950 JAN 01 00:00:00.000
        Stop time  (TDB):  2050 JAN 01 00:00:00.000
        Data type:         2
        Data class:        General surface
        Coordinate System: Latitudinal
        Segment boundaries:
          Longitude (deg):     -180.00000000000000     180.00000000000000
          Latitude  (deg):      -90.00000000000000      90.00000000000000
          Radius     (km):        8.22529807597397      14.01176814562576
      ====================================
       DSK descriptor for segment 2
        Body:              PHOBOS
        Surface:           Phobos example surface 2
        Frame:             IAU_PHOBOS
        Start time (TDB):  1950 JAN 01 00:00:00.000
        Stop time  (TDB):  2050 JAN 01 00:00:00.000
        Data type:         2
        Data class:        General surface
        Coordinate System: Rectangular
        Segment boundaries:
          X-coordinate (km):     -1.30000000000000       1.31000000000000
          Y-coordinate (km):     -1.21000000000000       1.20000000000000
          Z-coordinate (km):     -9.45293235778800       9.63817977905300
      ====================================
       DSK descriptor for segment 3
        Body:              PHOBOS
        Surface:           Phobos example surface 3
        Frame:             IAU_PHOBOS
        Start time (TDB):  1950 JAN 01 00:00:00.000
        Stop time  (TDB):  2050 JAN 01 00:00:00.000
        Data type:         2
        Data class:        General surface
        Coordinate System: Planetodetic
          Equatorial radius (km):     13.00000000000000
          Polar radius      (km):      9.10000000000000
        Segment boundaries:
          Longitude (deg):     -180.00000000000000     180.00000000000000
          Latitude  (deg):      -90.00000000000000      90.00000000000000
          Altitude   (km):       -3.72866868360370       1.37201579108146


-Restrictions

   1)  See Exception #3.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)

-Version

   -CSPICE Version 1.1.1, 10-AUG-2021 (JDR)

       Edited the header to comply with NAIF standard. Extended the
       -Exceptions section. Fixed bug in example #2.

   -CSPICE Version 1.1.0, 05-APR-2017 (NJB)

       Corrected a header comment typo.

       22-JAN-2016 (NJB)

          CSPICE header file references were updated.
          Example program 1 was re-written;
          example program 2 was added.

   -CSPICE Version 1.0.0, 13-NOV-2012 (NJB)

-Index_Entries

   return DSK segment_descriptor

-&
*/

{ /* Begin dskgd_c */

   /*
   Local variables
   */
   SpiceDouble             fDSKDescr [ SPICE_DSK_DSCSIZ ];

   SpiceInt                fDLADescr [ SPICE_DLA_DSCSIZ ];


   /*
   Participate in error tracing.
   */
   if ( return_c() )
   {
      return;
   }
   chkin_c ( "dskgd_c" );


   /*
   Populate the Fortran DLA descriptor array fDLADescr with the
   contents of the input descriptor.
   */
   fDLADescr[SPICE_DLA_BWDIDX] = dladsc->bwdptr;
   fDLADescr[SPICE_DLA_FWDIDX] = dladsc->fwdptr;
   fDLADescr[SPICE_DLA_IBSIDX] = dladsc->ibase;
   fDLADescr[SPICE_DLA_ISZIDX] = dladsc->isize;
   fDLADescr[SPICE_DLA_DBSIDX] = dladsc->dbase;
   fDLADescr[SPICE_DLA_DSZIDX] = dladsc->dsize;
   fDLADescr[SPICE_DLA_CBSIDX] = dladsc->cbase;
   fDLADescr[SPICE_DLA_CSZIDX] = dladsc->csize;

   /*
   Call the f2c'd "get DSK descriptor" routine.
   */
   dskgd_ ( ( integer    * ) &handle,
            ( integer    * ) fDLADescr,
            ( doublereal * ) fDSKDescr  );

   /*
   Populate the contents of the output DSK descriptor with
   the contents of the DSK descriptor array returned by
   dskgd_.
   */
   dskdsc->surfce = fDSKDescr[SPICE_DSK_SRFIDX];
   dskdsc->center = fDSKDescr[SPICE_DSK_CTRIDX];
   dskdsc->dclass = fDSKDescr[SPICE_DSK_CLSIDX];
   dskdsc->dtype  = fDSKDescr[SPICE_DSK_TYPIDX];
   dskdsc->corsys = fDSKDescr[SPICE_DSK_SYSIDX];
   dskdsc->frmcde = fDSKDescr[SPICE_DSK_FRMIDX];

   MOVED ( fDSKDescr + SPICE_DSK_PARIDX,
           SPICE_DSK_NSYPAR,
           dskdsc->corpar                );

   dskdsc->co1min = fDSKDescr[SPICE_DSK_MN1IDX];
   dskdsc->co1max = fDSKDescr[SPICE_DSK_MX1IDX];
   dskdsc->co2min = fDSKDescr[SPICE_DSK_MN2IDX];
   dskdsc->co2max = fDSKDescr[SPICE_DSK_MX2IDX];
   dskdsc->co3min = fDSKDescr[SPICE_DSK_MN3IDX];
   dskdsc->co3max = fDSKDescr[SPICE_DSK_MX3IDX];
   dskdsc->start  = fDSKDescr[SPICE_DSK_BTMIDX];
   dskdsc->stop   = fDSKDescr[SPICE_DSK_ETMIDX];

   chkout_c ( "dskgd_c" );

} /* End dskgd_c */
