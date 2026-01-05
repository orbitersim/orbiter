/*

-Procedure bodvrd_c ( Return d.p. values from the kernel pool )

-Abstract

   Fetch from the kernel pool the double precision values
   of an item associated with a body.

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

   KERNEL
   NAIF_IDS

-Keywords

   CONSTANTS

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZmc.h"


   void bodvrd_c ( ConstSpiceChar   * bodynm,
                   ConstSpiceChar   * item,
                   SpiceInt           maxn,
                   SpiceInt         * dim,
                   SpiceDouble      * values )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   bodynm     I   Body name.
   item       I   Item for which values are desired. ("RADII",
                  "NUT_PREC_ANGLES", etc. )
   maxn       I   Maximum number of values that may be returned.
   dim        O   Number of values returned.
   values     O   Values.

-Detailed_Input

   bodynm      is the name of the body for which `item' is requested.
               `bodynm' is case-insensitive, and leading and trailing
               blanks in `bodynm' are not significant. Optionally, you
               may supply the integer ID code for the object as an
               integer string. For example both "MOON" and "301" are
               legitimate strings that indicate the moon is the body
               of interest.

   item        is the item to be returned. Together, the NAIF ID
               code of the body and the item name combine to form a
               kernel variable name, e.g.,

                  "BODY599_RADII"
                  "BODY401_POLE_RA"

               The values associated with the kernel variable having
               the name constructed as shown are sought. Below
               we'll take the shortcut of calling this kernel variable
               the "requested kernel variable."

               Note that `item' *is* case-sensitive. This attribute
               is inherited from the case-sensitivity of kernel
               variable names.

   maxn        is the maximum number of values that may be returned.
               The output array `values' must be declared with size at
               least `maxn'. It's an error to supply an output array
               that is too small to hold all of the values associated
               with the requested kernel variable.

-Detailed_Output

   dim         is the number of values returned; this is always the
               number of values associated with the requested kernel
               variable unless an error has been signaled.

   values      is the array of values associated with the requested
               kernel variable. If `values' is too small to hold all
               of the values associated with the kernel variable, the
               returned values of `dim' and `values' are undefined.

-Parameters

   None.

-Exceptions

   1)  If the input body name cannot be translated to an ID code,
       and if the name is not a string representation of an integer
       (for example, "399"), the error SPICE(NOTRANSLATION) is
       signaled by a routine in the call tree of this routine.

   2)  If the requested kernel variable is not found in the kernel
       pool, the error SPICE(KERNELVARNOTFOUND) is signaled by a
       routine in the call tree of this routine.

   3)  If the requested kernel variable is found but the associated
       values aren't numeric, the error SPICE(TYPEMISMATCH) is
       signaled by a routine in the call tree of this routine.

   4)  If the dimension of `values' indicated by `maxn' is too small to
       contain the requested values, the error SPICE(ARRAYTOOSMALL)
       is signaled by a routine in the call tree of this routine. The
       output array `values' must be declared with sufficient size to
       contain all of the values associated with the requested kernel
       variable.

   5)  If the input dimension `maxn' indicates there is more room in
       `values' than there really is---for example, if `maxn' is 10 but
       values is declared with dimension 5---and the dimension of the
       requested kernel variable is larger than the actual dimension
       of `values', then this routine may overwrite memory. The results
       are unpredictable.

   6)  If any of the `bodynm' or `item' input string pointers is
       null, the error SPICE(NULLPOINTER) is signaled.

   7)  If any of the `bodynm' or `item' input strings has zero
       length, the error SPICE(EMPTYSTRING) is signaled.

-Files

   None.

-Particulars

   This routine simplifies looking up PCK kernel variables by
   constructing names of requested kernel variables and by
   performing error checking.

   This routine is intended for use in cases where the maximum number
   of values that may be returned is known at compile time. The
   caller fetches all of the values associated with the specified
   kernel variable via a single call to this routine. If the number
   of values to be fetched cannot be known until run time, the
   lower-level routine gdpool_c should be used instead. gdpool_c supports
   fetching arbitrary amounts of data in multiple "chunks."

   This routine is intended for use in cases where the requested
   kernel variable is expected to be present in the kernel pool. If
   the variable is not found or has the wrong data type, this
   routine signals an error. In cases where it is appropriate to
   indicate absence of an expected kernel variable by returning a
   boolean "found flag" with the value SPICEFALSE, again the routine
   gdpool_c should be used.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Retrieve the radii of the Earth from the kernel pool, using both
      "RADII" and "radii" as the item name to return. Since the `item'
      variable possesses case sensitivity, the later case should fail.
      Trap the error and print it to the output.

      Use the PCK kernel below to load the required triaxial
      ellipsoidal shape model for the Earth.

         pck00008.tpc


      Example code begins here.


      /.
         Program bodvrd_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Local parameters.
         ./
         #define NVALS        3

         /.
         Local variables.
         ./
         SpiceDouble          values [NVALS];

         SpiceInt             dim;

         /.
         Load a PCK.
         ./
         furnsh_c ( "pck00008.tpc" );

         /.
         When the kernel variable

            BODY399_RADII

         is present in the kernel pool---normally because a PCK
         defining this variable has been loaded (as is the case
         here)---the call
         ./
         bodvrd_c ( "EARTH", "RADII", 3, &dim, values );

         /.
         returns the dimension and values associated with the
         variable "BODY399_RADII"
         ./
         printf( "EARTH RADII:  %9.3f %9.3f %9.3f\n",
                      values[0], values[1], values[2] );

         /.
         The call
         ./
         bodvrd_c ( "earth", "RADII", 3, &dim, values );

         /.
         will produce the same results since the case of the
         input argument `bodynm' is not significant.
         ./
         printf( "earth RADII:  %9.3f %9.3f %9.3f\n",
                      values[0], values[1], values[2] );

         /.
         The call
         ./
         bodvrd_c ( "399", "RADII", 3, &dim, values );

         /.
         will also produce the same results since strings
         containing integer codes are accepted
         by this routine.
         ./
         printf( "'399 RADII: ' %9.3f %9.3f %9.3f\n",
                      values[0], values[1], values[2] );

         /.
         The `item' variable possesses case sensitivity. This
         call should cause an error.
         ./
         bodvrd_c ( "EARTH", "radii", 3, &dim, values );
         printf( "EARTH radii:  %9.3f %9.3f %9.3f\n",
                      values[0], values[1], values[2] );

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      EARTH RADII:   6378.140  6378.140  6356.750
      earth RADII:   6378.140  6378.140  6356.750
      '399 RADII: '  6378.140  6378.140  6356.750

      =====================================================================

      Toolkit version: N0066

      SPICE(KERNELVARNOTFOUND) -- The Variable Was not Found in the Kernel
      Pool.

      The variable BODY399_radii could not be found in the kernel pool.

      A traceback follows.  The name of the highest level module is first.
      bodvrd_c --> BODVRD

      Oh, by the way:  The SPICELIB error handling actions are USER-
      TAILORABLE.  You can choose whether the Toolkit aborts or continues
      when errors occur, which error messages to output, and where to send
      the output.  Please read the ERROR "Required Reading" file, or see
      the routines ERRACT, ERRDEV, and ERRPRT.

      =====================================================================


      Note that, usually, the last call will cause a
      SPICE(KERNELVARNOTFOUND) error to be signaled, because this
      call will attempt to look up the values associated with a
      kernel variable of the name

         'BODY399_radii'

      Since kernel variable names are case sensitive, this
      name is not considered to match the name

         'BODY399_RADII'

      which normally would be present after a text PCK
      containing data for all planets and satellites has
      been loaded.

-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)

-Version

   -CSPICE Version 1.0.2, 25-AUG-2021 (JDR)

       Edited the header to comply with NAIF standard. Added
       complete code example based on existing fragments.

   -CSPICE Version 1.0.1, 12-APR-2006 (NJB)

       Header fix: output argument `dim' is now preceded by
       an ampersand in example calls to bodvrd_c.c.

   -CSPICE Version 1.0.0, 22-FEB-2004 (NJB)

-Index_Entries

   fetch constants for a body from the kernel pool
   physical constants for a body

-&
*/

{ /* Begin bodvrd_c */


   /*
   Participate in error tracing.
   */
   if ( return_c() )
   {
      return;
   }
   chkin_c ( "bodvrd_c" );


   /*
   Check the input strings.
   */
   CHKFSTR ( CHK_STANDARD, "bodvrd_c", bodynm );
   CHKFSTR ( CHK_STANDARD, "bodvrd_c", item   );


   /*
   Call the f2c'd SPICELIB function.
   */
   bodvrd_ ( (char       *) bodynm,
             (char       *) item,
             (integer    *) &maxn,
             (integer    *) dim,
             (doublereal *) values,
             (ftnlen      ) strlen(bodynm),
             (ftnlen      ) strlen(item)    );

   chkout_c ( "bodvrd_c" );

} /* End bodvrd_c */
