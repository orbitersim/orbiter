/*

-Procedure dskn02_c ( DSK, type 2, compute normal vector for plate )

-Abstract

   Compute the unit normal vector for a specified plate from a type
   2 DSK segment.

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

-Keywords

   DAS
   DSK
   FILES

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #undef dskn02_c


   void dskn02_c ( SpiceInt               handle,
                   ConstSpiceDLADescr   * dladsc,
                   SpiceInt               plid,
                   SpiceDouble            normal[3] )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   handle     I   DSK file handle.
   dladsc     I   DLA descriptor.
   plid       I   Plate ID.
   normal     O   Plate's unit normal vector.

-Detailed_Input

   handle      is the handle of a DSK file containing a type 2
               segment from which data are to be fetched.

   dladsc      is the DLA descriptor associated with the segment
               from which data are to be fetched.

   plid        is the plate ID. Plate IDs range from 1 to NP
               (the number of plates).

-Detailed_Output

   normal      is the normal vector associated with the plate
               designated by `plid'. The direction of `normal' is
               determined by the order of the plate's vertices;
               the vertices are presumed to be ordered in the
               right-handed (counterclockwise) sense about the
               normal direction. If the plate's vertices are
               `v1', `v2', `v3', then `normal' points in the direction

                  (v2 - v1) x ( v3 - v2 )

               where "x" represents the cross product operator.

               The vector `normal' is expressed in the body-fixed
               reference frame of the segment designated by `dladsc'.
               The center of this frame is the origin of the cartesian
               coordinate system in which the vertices are expressed.
               Note that the frame center need not coincide with the
               central body of the segment. Units are km.

               The vector has magnitude 1.

               If an error occurs on the call, `normal' is undefined.

-Parameters

   See the header file

      SpiceDLA.h

   for declarations of DLA descriptor sizes and documentation of the
   contents of DLA descriptors.

   See the header file

      SpiceDSK.h

   for declarations of DSK descriptor sizes and documentation of the
   contents of DSK descriptors.

   See the header file

      SpiceDSK.h

   for declarations of DSK data type 2 (plate model) parameters.

-Exceptions

   1)  If the input handle is invalid, an error is signaled by a
       routine in the call tree of this routine.

   2)  If a file read error occurs, the error is signaled by a
       routine in the call tree of this routine.

   3)  If the input DLA descriptor is invalid, the effect of this
       routine is undefined. The error *may* be diagnosed by
       routines in the call tree of this routine, but there are no
       guarantees.

   4)  If `plid' is less than 1 or greater than the number of plates in
       the segment, the error SPICE(INDEXOUTOFRANGE) is signaled by a
       routine in the call tree of this routine.

   5)  This routine does not check for linear independence of the
       plate's edges. The plate model is assumed to be geometrically
       valid.

-Files

   See input argument `handle'.

-Particulars

   None.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Look up all the vertices associated with each plate
      of the model contained in a specified type 2 segment. For each
      of the first 5 plates, display the plate's vertices and normal
      vector.

      For this example, we'll show the context of this look-up:
      opening the DSK file for read access, traversing a trivial,
      one-segment list to obtain the segment of interest.


      Example code begins here.


      /.
         Program dskn02_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main()
      {
         /.
         Constants
         ./
         #define PBUFSIZ         10000
         #define FILSIZ          256

         /.
         Local variables
         ./
         SpiceBoolean            found;

         SpiceChar               dsk  [ FILSIZ ];

         SpiceDLADescr           dladsc;

         SpiceDouble             normal [3];
         SpiceDouble             verts  [3][3];

         SpiceInt                handle;
         SpiceInt                i;
         SpiceInt                j;
         SpiceInt                n;
         SpiceInt                np;
         SpiceInt                nread;
         SpiceInt                nv;
         SpiceInt                nvtx;
         SpiceInt                plates[PBUFSIZ][3];
         SpiceInt                plix;
         SpiceInt                remain;
         SpiceInt                start;

         /.
         Prompt for name of DSK and open file for reading.
         ./
         prompt_c ( "Enter DSK name > ", FILSIZ, dsk );

         dasopr_c ( dsk,    &handle );

         dlabfs_c ( handle, &dladsc, &found );

         if ( !found )
         {
            setmsg_c ( "No segment found in file #." );
            errch_c  ( "#",  dsk                     );
            sigerr_c ( "SPICE(NOSEGMENT)"            );
         }

         /.
         Get segment vertex and plate counts.
         ./
         dskz02_c ( handle, &dladsc, &nv, &np );

         printf ( "\n"
                  "Number of vertices:  %d\n"
                  "Number of plates:    %d\n",
                  (int)nv,
                  (int)np                           );

         /.
         Display the vertices of each of the first 5 plates.
         ./
         remain = mini_c ( 2, 5, np );
         start  = 1;

         while ( remain > 0 )
         {
            /.
            `nread' is the number of plates we"ll read on this
            loop pass.
            ./
            nread  = mini_c ( 2, PBUFSIZ, remain );

            dskp02_c ( handle, &dladsc, start, nread, &n, plates );

            for ( i = 0; i < nread; i++ )
            {
               plix = start + i;

               /.
               Read the vertices of the current plate.
               ./
               for ( j = 0;  j < 3;  j++ )
               {
                  dskv02_c ( handle, &dladsc, plates[i][j],
                             1,      &nvtx,
                             ( SpiceDouble(*)[3] )(verts[j])  );
               }

               /.
               Display the vertices of the current plate:
               ./
               printf ( "\n"
                        " Plate number: %d\n"
                        "    Vertex 1: ( %16.8e %16.8e %16.8e )\n"
                        "    Vertex 2: ( %16.8e %16.8e %16.8e )\n"
                        "    Vertex 3: ( %16.8e %16.8e %16.8e )\n",
                        (int)plix,
                        verts[0][0],  verts[0][1],  verts[0][2],
                        verts[1][0],  verts[1][1],  verts[1][2],
                        verts[2][0],  verts[2][1],  verts[2][2]   );

               /.
               Display the normal vector of the current plate:
               ./
               dskn02_c ( handle, &dladsc, plix, normal );

               printf( "    Normal:   ( %16.8e %16.8e %16.8e )\n",
                       normal[0], normal[1], normal[2]           );
            }

            start  = start  + nread;
            remain = remain - nread;
         }

         /.
         Close the kernel.  This isn't necessary in a stand-
         alone program, but it's good practice in subroutines
         because it frees program and system resources.
         ./
         dascls_c ( handle );

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, using the DSK file named phobos512.bds, the output
      was:


      Enter DSK name > phobos512.bds

      Number of vertices:  1579014
      Number of plates:    3145728

       Plate number: 1
          Vertex 1: (  -6.77444000e+00   6.26815000e+00   6.01149000e+00 )
          Vertex 2: (  -6.76238000e+00   6.25728000e+00   6.02556000e+00 )
          Vertex 3: (  -6.75710000e+00   6.27754000e+00   6.02096000e+00 )
          Normal:   (  -5.81973770e-01   3.21285613e-01   7.47048918e-01 )

       Plate number: 2
          Vertex 1: (  -6.77444000e+00   6.26815000e+00   6.01149000e+00 )
          Vertex 2: (  -6.77973000e+00   6.24790000e+00   6.01610000e+00 )
          Vertex 3: (  -6.76238000e+00   6.25728000e+00   6.02556000e+00 )
          Normal:   (  -5.81456950e-01   3.21988310e-01   7.47148809e-01 )

       Plate number: 3
          Vertex 1: (  -6.77973000e+00   6.24790000e+00   6.01610000e+00 )
          Vertex 2: (  -6.76768000e+00   6.23701000e+00   6.03019000e+00 )
          Vertex 3: (  -6.76238000e+00   6.25728000e+00   6.02556000e+00 )
          Normal:   (  -5.81597068e-01   3.22641957e-01   7.46757671e-01 )

       Plate number: 4
          Vertex 1: (  -6.77973000e+00   6.24790000e+00   6.01610000e+00 )
          Vertex 2: (  -6.78499000e+00   6.22762000e+00   6.02070000e+00 )
          Vertex 3: (  -6.76768000e+00   6.23701000e+00   6.03019000e+00 )
          Normal:   (  -5.83129010e-01   3.20560704e-01   7.46459237e-01 )

       Plate number: 5
          Vertex 1: (  -6.78499000e+00   6.22762000e+00   6.02070000e+00 )
          Vertex 2: (  -6.77299000e+00   6.21674000e+00   6.03482000e+00 )
          Vertex 3: (  -6.76768000e+00   6.23701000e+00   6.03019000e+00 )
          Normal:   (  -5.83664048e-01   3.23060196e-01   7.44962005e-01 )


-Restrictions

   See -Exceptions section.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)

-Version

   -CSPICE Version 1.0.2, 10-AUG-2021 (JDR)

       Edited the header to comply with NAIF standard. Updated
       code example to reduce the number of plates whose vertices are
       shown on output and to remove unnecessary include files.

   -CSPICE Version 1.0.1, 27-JAN-2016 (NJB)

       Edit to example program to use "%d" with explicit casts
       to int for printing SpiceInts with printf.

       Updated header -Detailed_Output and -Index_Entries
       sections.

   -CSPICE Version 1.0.0, 03-JUN-2010 (NJB)

-Index_Entries

   compute normal vector for a type 2 DSK plate
   compute normal vector from DSK type 2 plate id

-&
*/

{ /* Begin dskn02_c */


   /*
   Local variables
   */
   integer                 fDLADescr [ SPICE_DLA_DSCSIZ ];

   /*
   Participate in error tracing.
   */
   if ( return_c() )
   {
      return;
   }
   chkin_c ( "dskn02_c" );

   /*
   Populate the Fortran DLA descriptor array fDLADescr with the contents
   of the input descriptor.
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
   Call the f2c'd routine.
   */
   dskn02_ ( ( integer     * ) &handle,
             ( integer     * ) fDLADescr,
             ( integer     * ) &plid,
             ( doublereal  * ) normal );


   chkout_c ( "dskn02_c" );

} /* End dskn02_c */
