/*

-Procedure cklpf_c ( CK, load pointing file )

-Abstract

   Load a CK pointing file for use by the CK readers. Return that
   file's handle, to be used by other CK routines to refer to the
   file.

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

   CK
   DAF

-Keywords

   POINTING

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZmc.h"


   void cklpf_c ( ConstSpiceChar * fname,
                  SpiceInt       * handle )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   fname      I   Name of the CK file to be loaded.
   handle     O   Loaded file's handle.

-Detailed_Input

   fname       is the name of a C-kernel file to be loaded.

-Detailed_Output

   handle      is an integer handle assigned to the file upon loading.
               Almost every other CK routine will subsequently use
               this number to refer to the file.

-Parameters

   FTSIZE     is the maximum number of pointing files that can
              be loaded by cklpf_c at any given time for use by the
              readers.

-Exceptions

   1)  If an attempt is made to load more DAF files than is
       specified by the parameter FTSIZE in DAF system, an error
       is signaled by a routine in the call tree of this routine.

   2)  If an attempt is made to load more files than is specified by
       the parameter FTSIZE in the CK subsystem, and if the DAF
       system has room to load another file, the error
       SPICE(CKTOOMANYFILES) is signaled by a routine in the call
       tree of this routine. The current setting of FTSIZE does not
       allow this situation to arise: the DAF system will trap the
       error before this routine has the chance.

   3)  If the file specified by `fname' can not be opened, an error
       is signaled by a routine in the call tree of this routine.

   4)  If the file specified by `fname' has already been loaded,
       it will become the "last-loaded" file. The readers
       search the last-loaded file first.

   5)  If the `fname' input string pointer is null, the error
       SPICE(NULLPOINTER) is signaled.

   6)  If the `fname' input string has zero length, the error
       SPICE(EMPTYSTRING) is signaled.

-Files

   The C-kernel file specified by `fname' is loaded. The file is
   assigned an integer handle by cklpf_c. Other CK routines will refer
   to this file by its handle.

-Particulars

   Before a file can be read by the C-kernel readers, it must be
   loaded by cklpf_c, which among other things load the file into
   the DAF subsystem.

   Up to FTSIZE files may be loaded for use simultaneously, and a
   file only has to be loaded once to become a potential search
   target for any number of subsequent reads.

   Once a C-kernel has been loaded, it is assigned a file
   handle, which is used to keep track of the file internally, and
   which is used by the calling program to refer to the file in all
   subsequent calls to CK routines.

   If there is room for a new file, cklpf_c opens the file for
   reading. This routine must be called prior to a call to ckgp_c or
   ckgpav_c.

   CK readers search files loaded with cklpf_c in the reverse order
   in which they were loaded. That is, last-loaded files are
   searched first.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) The following example will extract the entire comment area of a
      CK, displaying the comments on the terminal screen.


      Example code begins here.


      /.
         Program cklpf_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main()
      {
         #define FILSIZ          256
         #define LINLEN          1001
         #define BUFFSZ          25

         SpiceBoolean            done = SPICEFALSE;

         SpiceChar               ckname [FILSIZ];
         SpiceChar               buffer [BUFFSZ][LINLEN];

         SpiceInt                handle;
         SpiceInt                i;
         SpiceInt                n;


         prompt_c ( "Enter name of CK > ", FILSIZ, ckname );

         /.
         Open the CK for read access. This operation could have
         been done with dafopr_c.
         ./
         cklpf_c ( ckname, &handle );

         while ( !done )
         {
            dafec_c ( handle, BUFFSZ, LINLEN, &n, buffer, &done );

            for ( i = 0;  i < n;  i++ )
            {
               printf ( "%s\n", buffer[i] );
            }
         }

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, using the Cassini CK file named 04161_04164ra.bc as
      input CK file, the output was:


      Enter name of CK > 04161_04164ra.bc
      \beginlabel
      PDS_VERSION_ID               = PDS3
      RECORD_TYPE                  = FIXED_LENGTH
      RECORD_BYTES                 = 1024
      ^SPICE_KERNEL                = "04161_04164ra.bc"
      MISSION_NAME                 = "CASSINI-HUYGENS"
      SPACECRAFT_NAME              = "CASSINI ORBITER"
      DATA_SET_ID                  = "CO-S/J/E/V-SPICE-6-V1.0"
      KERNEL_TYPE_ID               = CK
      PRODUCT_ID                   = "04161_04164ra.bc"
      PRODUCT_CREATION_TIME        = 2005-06-29T21:28:09
      PRODUCER_ID                  = "CASSINI_AACS/JPL"
      MISSION_PHASE_NAME           = "SCIENCE CRUISE"
      PRODUCT_VERSION_TYPE         = ACTUAL
      PLATFORM_OR_MOUNTING_NAME    = "N/A"
      START_TIME                   = 2004-06-09T12:00:03.631
      STOP_TIME                    = 2004-06-12T11:58:57.943
      SPACECRAFT_CLOCK_START_COUNT = "1/1465475046.160"
      SPACECRAFT_CLOCK_STOP_COUNT  = "1/1465734182.160"
      TARGET_NAME                  = "N/A"
      INSTRUMENT_NAME              = "CASSINI ORBITER"
      NAIF_INSTRUMENT_ID           = -82000
      SOURCE_PRODUCT_ID            = "N/A"
      NOTE                         = "See comments in the file for details"
      OBJECT                       = SPICE_KERNEL
        INTERCHANGE_FORMAT         = BINARY
        KERNEL_TYPE                = POINTING
        DESCRIPTION                = "Reconstructed Cassini Spacecraft
      Orientation CK file. "
      END_OBJECT                   = SPICE_KERNEL
      \endlabel


      Cassini Spacecraft Orientation Reconstructed CK File
      ======================================================================***

           Comments added by Lee Elson, NAIF/JPL, Wed Jun 29 15:05:33 PDT 2005


      Orientation Data in the File
      --------------------------------------------------------

           This file contains reconstructed orientation and angular velocity
           data for the Cassini Orbiter spacecraft frame, 'CASSINI_SC_COORD',
           relative to the 'J2000' inertial frame. The NAIF ID code for the
           'CASSINI_SC_COORD' frame is -82000.


      Status
      --------------------------------------------------------

           The data in this file were created by the Cassini Project for
           archiving with the Planetary Data System (PDS).


      Pedigree
      --------------------------------------------------------

           Reconstructed CK files were delivered to the Cassini Project by the
           Attitude and Articulation Subsystem (AACS). These reconstructed
           files contain spacecraft orientation and angular rates returned
           in Cassini spacecraft telemetry.  The data were packaged by AACS
           using the SPICE utility MSOPCK provided by NAIF.  Later files
           were created by accessing the MSOPCK software through the AACS
           C-Kernel Generation Tool (ACKT) user interface.

           Files generated prior to January 2004 have been merged to remove
           redundant data and in some cases provide longer interpolation
           intervals since the spacecraft was primarily earth pointed during
           that time.

      Approximate Time Coverage
      --------------------------------------------------------

           This file covers the following interval of the mission:

                Coverage Begin UTC       Coverage End UTC
                ------------------------ ------------------------
                2004-JUN-09 12:00:03.631 2004-JUN-12 11:58:57.943

           This file is a type 3 CK file which allows linear interpolation
           between orientation data points extracted from telemetry. Such
           interpolation is not applicable to the whole file but only inside
           intervals where enough orientation telemetry data are available
           and orientation data points were close enough to each other in time
           for such interpolation to make sense.

           The nominal quaternion time spacing in the telemetry stream is
           4 seconds but a spacing as large as 16 seconds may exist between
           adjacent data points.

           A table containing the complete list of valid interpolation inter***
           in the file is provided in Appendix 2.

      Related Information
      --------------------------------------------------------

           AACS has carried out comparisons between some predicted CK
           files and the corresponding reconstructed CK files. Numerical

      [...]


      Warning: incomplete output. Only 100 out of 272 lines have been
      provided. 2 lines extended past the right margin of the header
      and have been truncated. These lines are marked by "***" at the
      end of each line.


-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   J.M. Lynch          (JPL)
   B.V. Semenov        (JPL)
   W.L. Taber          (JPL)
   R.E. Thurman        (JPL)
   I.M. Underwood      (JPL)
   E.D. Wright         (JPL)

-Version

   -CSPICE Version 2.1.0, 01-NOV-2021 (JDR)

       Changed input argument name "filename" to "fname" for
       consistency with other routines.

       Edited the header to comply with NAIF standard. Added
       complete code example based on that present in dafec_c.

   -CSPICE Version 2.0.1, 31-JAN-2008 (BVS)

       Removed '-Revisions' from the header.

   -CSPICE Version 2.0.0, 08-FEB-1998 (NJB)

       Input argument filename changed to type ConstSpiceChar *;
       name was changed to "filename" from "fname."

       References to C2F_CreateStr_Sig were removed; code was
       cleaned up accordingly. String checks are now done using
       the macro CHKFSTR.

   -CSPICE Version 1.0.0, 25-OCT-1997 (EDW) (RET) (IMU) (JML) (WLT)

-Index_Entries

   load CK pointing file

-&
*/

{ /* Begin spklef_c */


   /*
   Participate in error tracing.
   */
   chkin_c ( "cklpf_c" );


   /*
   Check the input string `fname' to make sure the pointer is non-null
   and the string length is non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "cklpf_c", fname );


   /*
   Call the f2c'd Fortran routine.
   */
   cklpf_ ( ( char     * )  fname,
            ( integer  * )  handle,
            ( ftnlen     )  strlen(fname) );


   chkout_c ( "cklpf_c" );

} /* end cklpf_c */
