/*

-Procedure dskopn_c ( DSK, open new file )

-Abstract

   Open a new DSK file for subsequent write operations.

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

   #include <string.h>
   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZst.h"
   #include "SpiceZmc.h"
   #include "SpiceDSK.h"

   void dskopn_c ( ConstSpiceChar  * fname,
                   ConstSpiceChar  * ifname,
                   SpiceInt          ncomch,
                   SpiceInt       *  handle )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   fname      I   Name of a DSK file to be opened.
   ifname     I   Internal file name.
   ncomch     I   Number of comment characters to allocate.
   handle     O   Handle assigned to the opened DSK file.

-Detailed_Input

   fname       is the name of a new DSK file to be created. The
               file will be left opened for write access.

   ifname      is the internal file name for the new file. The name
               may contain as many as 60 characters. All characters
               of `ifname' should be printing characters (ASCII codes
               32-126 decimal). This name should uniquely identify
               the file.

   ncomch      is the number of comment characters to allocate.
               Allocating comment characters at file creation time
               may reduce the likelihood of having to expand the
               comment area later.

-Detailed_Output

   handle      is the file handle associated with the file. This
               handle is used to identify the file in subsequent
               calls to other DSK routines.

-Parameters

   None.

-Exceptions

   1)  If the input filename is blank, an error is signaled by a
       routine in the call tree of this routine. No file will be
       created.

   2)  If the specified file cannot be opened without exceeding the
       maximum allowed number of open DAS files, an error is signaled
       by a routine in the call tree of this routine. No file will be
       created.

   3)  If the file cannot be opened properly, an error is signaled by
       a routine in the call tree of this routine. No file will be
       created.

   4)  If the initial records in the file cannot be written, an error
       is signaled by a routine in the call tree of this routine. No
       file will be created.

   5)  If no logical units are available, an error is signaled by a
       routine in the call tree of this routine. No file will be
       created.

   6)  If the internal file name contains nonprinting characters
       (ASCII codes decimal 0-31 and 127-255), an error is signaled
       by a routine in the call tree of this routine. No file will be
       created.

   7)  If the number of comment characters allocated `ncomch' is
       negative, an error is signaled by a routine in the call
       tree of this routine. No file will be created.

   8)  If any of the `fname' or `ifname' input string pointers is
       null, the error SPICE(NULLPOINTER) is signaled.

   9)  If any of the `fname' or `ifname' input strings has zero
       length, the error SPICE(EMPTYSTRING) is signaled.

-Files

   See argument FNAME.

-Particulars

   DSK files are built using the DLA low-level format and
   the DAS architecture; DLA files are a specialized type of DAS
   file in which data are organized as a doubly linked list of
   segments. Each segment's data belong to contiguous components of
   character, double precision, and integer type.

   This routine creates a new DSK file and sets the type of the
   file to the mnemonic code passed to it.

   DSK files created by this routine have initialized file records.
   The ID word in a DSK file record has the form

      DAS/DSK

   where the characters following the slash are supplied by the
   caller of this routine.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Create a three-segment DSK file using plate model data for
      Phobos. Use latitudinal, rectangular, and planetodetic
      coordinates in the respective segments. This is not a
      realistic example, but it serves to demonstrate use of
      the supported coordinate systems.

      Use the DSK kernel below to provide, for simplicity, the
      input plate and vertex data. The selected input file has one
      segment.

         phobos_3_3.bds


      Example code begins here.


      /.
         Program dskopn_ex1

         Example program for dskw02_c, dskmi2_c, and dskrb2_c

         Create a three-segment DSK file using plate model data for
         Phobos. Use latitudinal, rectangular, and planetodetic
         coordinates in the respective segments.

         For simplicity, use an existing DSK file to provide the
         input plate and vertex data. The selected input file has one
         segment.

            Version 1.0.0 22-JAN-2016 (NJB)

      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main()
      {

         /.
         Local constants
         ./
         #define FILSIZ          256
         #define LNSIZE           81
         #define NCOR              4
         #define NSEG              3
         #define NAMLEN           21

         /.
         Local variables
         ./

         /.
         Below, we declare large arrays static to avoid stack
         overflow problems.
         ./

         SpiceBoolean            found;

         SpiceChar               cornam [ NCOR ][ NAMLEN ] =
                                 { "radius",
                                   "Z-coordinate",
                                   "Z-coordinate",
                                   "altitude" };

         SpiceChar             * dsk;
         SpiceChar             * frame;
         SpiceChar             * indsk;

         SpiceDLADescr           dladsc;

         SpiceDouble             corpar [ SPICE_DSK_NSYPAR ];
         SpiceDouble             f;
         SpiceDouble             finscl;
         SpiceDouble             first;
         SpiceDouble             last;
         SpiceDouble             mncor1;
         SpiceDouble             mncor2;
         SpiceDouble             mncor3;
         SpiceDouble             mxcor1;
         SpiceDouble             mxcor2;
         SpiceDouble             mxcor3;
         SpiceDouble             re;
         SpiceDouble             rp;

         /.
         Note: the values of SPICE_DSK02_MAXVRT and
         SPICE_DSK02_MAXPLT declared in SpiceDSK.h,
         integer spatial index dimension SPICE_DSK02_SPAISZ,
         and the workspace dimension SPICE_DSK02_MAXCEL
         are very large. Smaller buffers can be used for most
         applications.
         ./
         static SpiceDouble      spaixd [ SPICE_DSK02_SPADSZ ];
         static SpiceDouble      vrtces [ SPICE_DSK02_MAXVRT ][3];

         SpiceInt                center;
         SpiceInt                corscl;
         SpiceInt                corsys;
         SpiceInt                dclass;
         SpiceInt                handle;
         SpiceInt                i;
         SpiceInt                inhan;
         SpiceInt                np;
         SpiceInt                nv;
         static SpiceInt         plates [ SPICE_DSK02_MAXPLT ][3];
         SpiceInt                segno;
         static SpiceInt         spaixi [ SPICE_DSK02_SPAISZ ];
         SpiceInt                spaisz;
         SpiceInt                surfid;
         SpiceInt                voxpsz;
         SpiceInt                voxlsz;
         static SpiceInt         work   [ SPICE_DSK02_MAXCEL ][2];
         SpiceInt                worksz;


         /.
         Assign names of input and output DSK files.
         ./
         indsk = "phobos_3_3.bds";
         dsk   = "phobos_3_3_3seg.bds";

         /.
         Open input DSK for read access; find first segment.
         ./
         dasopr_c ( indsk, &inhan );
         dlabfs_c ( inhan, &dladsc, &found );

         /.
         Fetch vertices and plates from input DSK file.

         Note that vertex and plate indices are 1-based.
         ./
         printf ( "Reading input data...\n" );

         dskv02_c ( inhan, &dladsc, 1, SPICE_DSK02_MAXVRT,
                    &nv,   vrtces                           );
         dskp02_c ( inhan, &dladsc, 1, SPICE_DSK02_MAXPLT,
                    &np,   plates                           );

         printf ( "Done.\n" );


         /.
         Set input array sizes required by dskmi2_c.
         ./
         voxpsz = SPICE_DSK02_MAXVXP;
         voxlsz = SPICE_DSK02_MXNVLS;
         worksz = SPICE_DSK02_MAXCEL;
         spaisz = SPICE_DSK02_SPAISZ;

         /.
         Set fine and coarse voxel scales. (These usually
         need to determined by experimentation.)
         ./
         finscl = 5.0;
         corscl = 4;

         /.
         Open a new DSK file.
         ./
         dskopn_c ( dsk, dsk, 0, &handle );

         /.
         Create three segments and add them to the file.
         ./
         for ( segno = 1;  segno <= NSEG;  segno++ )
         {
            /.
            Create spatial index. We won't generate a
            vertex-plate mapping, so we set the flag
            for creating this map to "false."
            ./
            printf ( "Creating segment %d\n", (int)segno );
            printf ( "Creating spatial index...\n" );

            dskmi2_c ( nv,     vrtces,     np,     plates,
                       finscl, corscl,     worksz, voxpsz,
                       voxlsz, SPICEFALSE, spaisz, work,
                       spaixd, spaixi                    );

            printf ( "Done.\n" );

            /.
            Set up inputs describing segment attributes:

            - Central body: Phobos
            - Surface ID code: user's choice.
              We use the segment number here.
            - Data class: general (arbitrary) shape
            - Body-fixed reference frame
            - Time coverage bounds (TBD)
            ./
            center = 401;
            surfid = segno;
            dclass = SPICE_DSK_GENCLS;
            frame  = "IAU_PHOBOS";

            first = -50 * jyear_c();
            last  =  50 * jyear_c();

            /.
            Set the coordinate system and coordinate system
            bounds based on the segment index.

            Zero out the coordinate parameters to start.
            ./
            for ( i = 0;  i < SPICE_DSK_NSYPAR;  i++ )
            {
               corpar[i] = 0.0;
            }

            if ( segno == 1 )
            {
               /.
               Use planetocentric latitudinal coordinates. Set
               the longitude and latitude bounds.
               ./
               corsys = SPICE_DSK_LATSYS;

               mncor1 = -pi_c();
               mxcor1 =  pi_c();
               mncor2 = -pi_c()/2;
               mxcor2 =  pi_c()/2;
            }
            else if ( segno == 2 )
            {

               /.
               Use rectangular coordinates. Set the
               X and Y bounds.

               The bounds shown here were derived from
               the plate data. They lie slightly outside
               of the range spanned by the plates.
               ./
               corsys = SPICE_DSK_RECSYS;

               mncor1 = -1.3;
               mxcor1 =  1.31;
               mncor2 = -1.21;
               mxcor2 =  1.2;
            }
            else
            {
               /.
               Set the coordinate system to planetodetic.
               ./
               corsys    = SPICE_DSK_PDTSYS;

               mncor1    = -pi_c();
               mxcor1    =  pi_c();
               mncor2    = -pi_c()/2;
               mxcor2    =  pi_c()/2;

               /.
               We'll use equatorial and polar radii from
               pck00010.tpc. These normally would be fetched
               at run time, but for simplicity, we'll use
               hard-coded values.
               ./
               re        = 13.0;
               rp        =  9.1;
               f         = ( re - rp ) / re;

               corpar[0] = re;
               corpar[1] = f;
            }
            /.
            Compute plate model radius bounds.
            ./
            printf ( "Computing %s bounds of plate set...\n",
                     cornam[corsys-1]                        );

            dskrb2_c ( nv,     vrtces, np,      plates,
                       corsys, corpar, &mncor3, &mxcor3 );

            printf ( "Done.\n" );

            /.
            Write the segment to the file.
            ./
            printf ( "Writing segment...\n" );

            dskw02_c ( handle,
                       center, surfid, dclass, frame,  corsys,
                       corpar, mncor1, mxcor1, mncor2, mxcor2,
                       mncor3, mxcor3, first,  last,   nv,
                       vrtces, np,     plates, spaixd, spaixi );

            printf ( "Done.\n" );
         }

         /.
         Segregate the data records in the DSK file and
         close the file.
         ./
         printf ( "Segregating and closing DSK file...\n" );

         dskcls_c ( handle, SPICETRUE );

         printf ( "Done.\n" );

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Reading input data...
      Done.
      Creating segment 1
      Creating spatial index...
      Done.
      Computing radius bounds of plate set...
      Done.
      Writing segment...
      Done.
      Creating segment 2
      Creating spatial index...
      Done.
      Computing Z-coordinate bounds of plate set...
      Done.
      Writing segment...
      Done.
      Creating segment 3
      Creating spatial index...
      Done.
      Computing altitude bounds of plate set...
      Done.
      Writing segment...
      Done.
      Segregating and closing DSK file...
      Done.


      Note that after run completion, a new DSK exists in the output
      directory.

-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)

-Version

   -CSPICE Version 1.0.1, 09-JUL-2021 (JDR)

       Edited the header to comply with NAIF standard. Added
       complete code example based on example in dskmi2_c.

   -CSPICE Version 1.0.0, 23-JAN-2016 (NJB)

       Corrected spelling errors in comments.

       DSKLIB_C Version 1.0.0, 12-FEB-2010 (NJB)

-Index_Entries

   open a new DSK file
   open a new DSK file with write access

-&
*/

{ /* Begin dskopn_c */




   /*
   Participate in error tracing.
   */

   chkin_c ( "dskopn_c" );

   /*
   Check the input string fname to make sure the pointer is non-null
   and the string length is non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "dskopn_c", fname );

   /*
   Check the input string ifname to make sure the pointer is
   non-null and the string length is non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "dskopn_c", ifname );


   dskopn_  (  ( char        * ) fname,
               ( char        * ) ifname,
               ( integer     * ) &ncomch,
               ( integer     * ) handle,
               ( ftnlen        ) strlen(fname),
               ( ftnlen        ) strlen(ifname)  );



   chkout_c ( "dskopn_c" );

} /* End dskopn_c */
