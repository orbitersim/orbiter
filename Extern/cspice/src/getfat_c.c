/*

-Procedure getfat_c ( Get file architecture and type )

-Abstract

   Determine the file architecture and file type of most SPICE kernel
   files.

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

   KERNEL
   UTILITY

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZst.h"
   #include "SpiceZmc.h"

   void getfat_c ( ConstSpiceChar   * file,
                   SpiceInt           arclen,
                   SpiceInt           kertln,
                   SpiceChar        * arch,
                   SpiceChar        * kertyp   )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   file       I   The name of a file to be examined.
   arclen     I   Maximum length of output architecture string.
   kertln     I   Maximum length of output `kertyp' string.
   arch       O   The architecture of the kernel file.
   kertyp     O   The type of the kernel file.

-Detailed_Input

   file        is the name of a SPICE kernel file whose architecture
               and type are desired. This file must be closed when
               this routine is called.

   arclen      is the maximum length of output architecture string
               `arch', including the terminating null. For example, if
               `arch' is to hold strings having 10 characters of actual
               data, `arclen' should be set to 11.

   kertln      is the maximum length of output file type string `kertyp',
               including the terminating null. For example, if `kertyp'
               is to hold strings having 10 characters of actual data,
               `kertln' should be set to 11.

-Detailed_Output

   arch        is the file architecture of the SPICE kernel file
               specified by `file'. If the architecture cannot be
               determined or is not recognized the value "?" is
               returned.

               Architectures currently recognized are:

                  DAF -- The file is based on the DAF architecture.
                  DAS -- The file is based on the DAS architecture.
                  XFR -- The file is in a SPICE transfer file format.
                  DEC -- The file is an old SPICE decimal text file.
                  ASC -- An ASCII text file.
                  KPL -- Kernel Pool File (i.e., a text kernel)
                  TXT -- An ASCII text file.
                  TE1 -- Text E-Kernel type 1.
                   ?  -- The architecture could not be determined.

               This variable must be at least 3 characters long.

   kertyp      is the type of the SPICE kernel file. If the type
               can not be determined the value "?" is returned.

               Kernel file types may be any sequence of at most four
               printing characters. NAIF has reserved for its use
               types which contain all upper case letters.

               A file type of "PRE" means that the file is a
               pre-release file.

               This variable may be at most 4 characters long.

-Parameters

   None.

-Exceptions

   1)  If the filename specified is blank, the error
       SPICE(BLANKFILENAME) is signaled by a routine in the call tree
       of this routine.

   2)  If any inquire on the filename specified by `file', required to
       obtain information about the physical file, fails for some
       reason, the error SPICE(INQUIREERROR) is signaled by a routine
       in the call tree of this routine.

   3)  If the file specified by `file' does not exist, the error
       SPICE(FILENOTFOUND) is signaled by a routine in the call tree
       of this routine.

   4)  If the file specified by `file' is already open but not through
       SPICE interfaces, the error SPICE(EXTERNALOPEN) is signaled by
       a routine in the call tree of this routine.

   5)  If an attempt to open the file specified by `file' fails when
       this routine requires that it succeed, the error
       SPICE(FILEOPENFAILED) is signaled by a routine in the call
       tree of this routine.

   6)  If an attempt to read the file specified by `file' fails when
       this routine requires that it succeed, the error
       SPICE(FILEREADFAILED) is signaled by a routine in the call
       tree of this routine.

   7)  If an issue is detected during the opening the input file or
       the process to determine its architecture and type, an error
       is signaled by a routine in the call tree of this routine.

   8)  If the ID word in a DAF based kernel is "NAIF/DAF", then the
       algorithm getfat_c uses to distinguish between CK and SPK
       kernels may result in an indeterminate `kertyp' if the SPK or
       CK files have invalid first segments.

   9)  If the `file' input string pointer is null, the error
       SPICE(NULLPOINTER) is signaled.

   10) If the `file' input string has zero length, the error
       SPICE(EMPTYSTRING) is signaled.

   11) If any of the `arch' or `kertyp' output string pointers is
       null, the error SPICE(NULLPOINTER) is signaled.

   12) If any of the `arch' or `kertyp' output strings has length
       less than two characters, the error SPICE(STRINGTOOSHORT) is
       signaled, since the output string is too short to contain one
       character of output data plus a null terminator.

-Files

   The SPICE kernel file specified by `file' is opened and then
   closed by this routine to determine its file architecture and
   type. Filenames of open files should not be passed to this
   routine.

-Particulars

   This subroutine is a support utility routine that determines the
   architecture and type of a SPICE kernel file.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Determine the file architecture and file type of all the
      kernels loaded through a meta-kernel and of a kernel in
      transfer format.

      Use the SPK kernel below to provide an example of a kernel in
      transfer format.

         earthstns_itrf93_050714.xsp


      Use the meta-kernel shown below to load the other types of
      SPICE kernels.


         KPL/MK

         File: getfat_ex1.tm

         This meta-kernel is intended to support operation of SPICE
         example programs. The kernels shown here should not be
         assumed to contain adequate or correct versions of data
         required by SPICE-based user applications.

         In order for an application to use this meta-kernel, the
         kernels referenced here must be present in the user's
         current working directory.

         The names and contents of the kernels referenced
         by this meta-kernel are as follows:

            File name                        Contents
            ---------                        --------
            de430.bsp                        Planetary ephemeris
            mar097.bsp                       Mars satellite ephemeris
            pck00010.tpc                     Planet orientation and
                                             radii
            naif0011.tls                     Leapseconds
            mgs_moc_v20.ti                   MGS MOC instrument
                                             parameters
            mgs_sclkscet_00061.tsc           MGS SCLK coefficients
            mgs_sc_ext12.bc                  MGS s/c bus attitude
            mgs_ext12_ipng_mgs95j.bsp        MGS ephemeris
            megr90n000cb_plate.bds           Plate model based on
                                             MEGDR DEM, resolution
                                             4 pixels/degree.

         \begindata

            KERNELS_TO_LOAD = ( 'de430.bsp',
                                'mar097.bsp',
                                'pck00010.tpc',
                                'naif0011.tls',
                                'mgs_moc_v20.ti',
                                'mgs_sclkscet_00061.tsc',
                                'mgs_sc_ext12.bc',
                                'mgs_ext12_ipng_mgs95j.bsp',
                                'megr90n000cb_plate.bds'      )
         \begintext

         End of meta-kernel


      Example code begins here.


      /.
         Program getfat_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main( )
      {
         /.
         Local parameters.
         ./
         #define  ARCHLN         4
         #define  FILSIZ         256
         #define  KTYPLN         5

         /.
         Local variables.
         ./
         SpiceBoolean            found;

         SpiceChar               arch       [ ARCHLN ];
         SpiceChar               fname      [ FILSIZ ];
         SpiceChar             * fname1;
         SpiceChar               ktype      [ KTYPLN ];
         SpiceChar               source     [ FILSIZ ];

         SpiceInt                count;
         SpiceInt                handle;
         SpiceInt                i;

         /.
         Check the file architecture and type of an SPK
         in transfer format.
         ./
         fname1 = "earthstns_itrf93_050714.xsp";
         getfat_c ( fname1, ARCHLN, KTYPLN, arch, ktype );
         printf ( "File name     : %s\n", fname1  );
         printf ( "  Architecture: %s\n", arch    );
         printf ( "  Kernel type : %s\n\n", ktype );

         /.
         Load the kernels.
         ./
         furnsh_c ( "getfat_ex1.tm" );


         /.
         Get the file architecture and kernel type for each of
         the kernels in the kernel pool.
         ./
         ktotal_c ( "ALL", &count );

         for ( i = 0; i < count; i++ )
         {
            kdata_c  ( i,     "ALL", FILSIZ, KTYPLN, FILSIZ,
                       fname, ktype, source, &handle, &found );

            getfat_c ( fname, ARCHLN, KTYPLN, arch, ktype );

            printf ( "File name     : %s\n", fname   );
            printf ( "  Source      : %s\n", source  );
            printf ( "  Architecture: %s\n", arch    );
            printf ( "  Kernel type : %s\n\n", ktype );
         }

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      File name     : earthstns_itrf93_050714.xsp
        Architecture: XFR
        Kernel type : DAF

      File name     : getfat_ex1.tm
        Source      :
        Architecture: KPL
        Kernel type : MK

      File name     : de430.bsp
        Source      : getfat_ex1.tm
        Architecture: DAF
        Kernel type : SPK

      File name     : mar097.bsp
        Source      : getfat_ex1.tm
        Architecture: DAF
        Kernel type : SPK

      File name     : pck00010.tpc
        Source      : getfat_ex1.tm
        Architecture: KPL
        Kernel type : PCK

      File name     : naif0011.tls
        Source      : getfat_ex1.tm
        Architecture: KPL
        Kernel type : LSK

      File name     : mgs_moc_v20.ti
        Source      : getfat_ex1.tm
        Architecture: KPL
        Kernel type : IK

      File name     : mgs_sclkscet_00061.tsc
        Source      : getfat_ex1.tm
        Architecture: KPL
        Kernel type : SCLK

      File name     : mgs_sc_ext12.bc
        Source      : getfat_ex1.tm
        Architecture: DAF
        Kernel type : CK

      File name     : mgs_ext12_ipng_mgs95j.bsp
        Source      : getfat_ex1.tm
        Architecture: DAF
        Kernel type : SPK

      File name     : megr90n000cb_plate.bds
        Source      : getfat_ex1.tm
        Architecture: DAS
        Kernel type : DSK


-Restrictions

   1)  In order to properly determine the type of DAF based binary
       kernels, the routine requires that their first segments and
       the meta data necessary to address them are valid.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)

-Version

   -CSPICE Version 1.1.0, 10-AUG-2021 (JDR)

       Changed argument names "typlen" and "type" to "kertln" and
       "kertyp" for consistency with other routines.

       Edited the header to comply with NAIF standard.

       Added complete code example to -Examples section. Updated
       -Exceptions and -Restrictions sections.

   -CSPICE Version 1.0.0, 18-AUG-1998 (NJB)

-Index_Entries

   determine the architecture and type of a kernel file

-&
*/

{ /* Begin getfat_c */


   /*
   Participate in error tracing.
   */
   chkin_c ( "getfat_c" );

   /*
   Check the input file name to make sure the pointer is non-null
   and the string length is non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "getfat_c", file );

   /*
   Make sure the output architecture string has at least enough room
   for one output character and a null terminator.  Also check for a
   null pointer.
   */
   CHKOSTR ( CHK_STANDARD, "getfat_c", arch, arclen );

   /*
   Make sure the output file type string has at least enough room
   for one output character and a null terminator.  Also check for a
   null pointer.
   */
   CHKOSTR ( CHK_STANDARD, "getfat_c", kertyp, kertln );

   getfat_ (  ( char   * ) file,
              ( char   * ) arch,
              ( char   * ) kertyp,
              ( ftnlen   ) strlen(file),
              ( ftnlen   ) arclen-1,
              ( ftnlen   ) kertln-1      );


   /*
   Convert each Fortran output string to a C string by placing a
   null after the last non-blank character.  This operation is valid
   whether or not the CSPICE routine signaled an error.
   */

   F2C_ConvertStr ( arclen, arch   );
   F2C_ConvertStr ( kertln, kertyp );


   chkout_c ( "getfat_c" );

} /* End getfat_c */
