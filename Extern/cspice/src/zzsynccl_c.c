/*

-Procedure zzsynccl_c ( Sync a CSPICE cell )

-Abstract

   CSPICE Private routine intended solely for the support of CSPICE
   routines.  Users should not call this routine directly due
   to the volatile nature of this routine.
 
   Sync a CSPICE cell. 

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
 
   CELLS 
 
-Keywords
 
   CELLS 
 
*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceCel.h"
   #include "SpiceZmc.h"

   void zzsynccl_c ( SpiceTransDir     xdir,
                     SpiceCell       * cell )   

/*

-Brief_I/O
 
   VARIABLE  I/O  DESCRIPTION 
   --------  ---  -------------------------------------------------- 
   xdir       I   Translation direction. 
   cell      I/O  Cell to be synced. 
 
-Detailed_Input
 
   xdir        indicates the translation direction.  Values and
               meanings are:

 
                  C2F            Support C to Fortran translation.

                                 Set the size and cardinality
                                 represented by the control area of the
                                 cell's data array.  The size and
                                 cardinality will be set to the values
                                 indicated by the corresponding members
                                 of the SpiceCell structure.
 
                                 This operation is meaningful only for
                                 numeric SpiceCell types.  For
                                 character SpiceCells, this option
                                 results in a no-op.
 
                  F2C            Support Fortran to C translation.

                                 Set the size and cardinality members
                                 of the SpiceCell structure to the
                                 values represented by the control area
                                 of the cell's data array. 


   cell        The cell to be synced.  The cell's size and cardinality
               values in the SpiceCell structure and in the data array
               are to be synced---set to identical values.  

-Detailed_Output
 
   cell        The cell to be synced.  The cell's size and cardinality
               values in the SpiceCell structure and in the data array
               are synced---set to identical values---with the direction
               of synchronization controlled by the argument xdir. 
 
-Parameters
 
   None. 
 
-Exceptions
 
   1) If the input cell does not have a recognized data type,
      the error SPICE(NOTSUPPORTED) is signaled.

   2) It's a no-op, but not an error, to have this routine perform an
      C2F sync on a character cell.  The reason this operational
      capability is omitted is that the control area of a character
      cell's data array is not used:  when a character cell is to
      be operated on by an f2c'd routine, the cell's contents are mapped
      to a dynamically allocated array, and the control area of that
      array is set up on the fly via calls to ssizec_ and scardc_.

-Files
 
   None. 
 
-Particulars

   This utility performs a commonly required cell operation, simplifying
   the coding of CSPICE wrappers for functions that have SpiceCell 
   inputs or outputs.

-Examples
 
   See wninsd_c and the CELLINIT macro defined in SpiceZmc.h.

-Restrictions
 
   1) This is a CSPICE private routine.  The interface may be changed
      without notice, so this routine should not be called except by
      other CSPICE routines.
 
-Literature_References
 
   None. 
 
-Author_and_Institution
 
   N.J. Bachman    (JPL) 
 
-Version
 
   -CSPICE Version 1.0.0, 29-JUL-2002 (NJB) 

-Index_Entries
 
   sync a CSPICE cell
 
-&
*/


{
   /*
   Local variables 
   */
   SpiceCellDataType       dtype;

   SpiceInt                ccard;
   SpiceInt                csize;
   SpiceInt                cstrlen;

   void                  * fcell;

   /*
   Discovery check-in here. 
   */
 

   /*
   Define some abbreviations first. 
   */
   csize = cell->size;
   ccard = cell->card;
   dtype = cell->dtype;
   fcell = cell->base;

   if ( xdir == C2F )
   {
      /*
      Sync the Fortran array with the size and cardinality values 
      stored in the associated C structure. 

      Setting a Fortran cell's size automatically sets the cardinality
      to zero, so scard* must be called to set the cardinality.
      */
      if ( dtype == SPICE_DP )
      {
         ssized_ ( ( integer    * ) &csize,
                   ( doublereal * ) fcell   );

         scardd_ ( ( integer    * ) &ccard,
                   ( doublereal * ) fcell   );
      }

      else if ( dtype == SPICE_INT )
      {
         ssizei_ ( ( integer * ) &csize,
                   ( integer * ) fcell   );

         scardi_ ( ( integer * ) &ccard,
                   ( integer * ) fcell   );
      }

      else if ( dtype != SPICE_CHR )
      {
         chkin_c  ( "zzsynccl_c"                    );
         setmsg_c ( "Invalid data type code # seen" ); 
         errint_c ( "#",  (SpiceInt) dtype          );   
         sigerr_c ( "SPICE(NOTSUPPORTED)"           );
         chkout_c ( "zzsynccl_c"                    );
         return;
      }
   }


   else
   {
      /*
      Sync the C structure size and cardinality values with those
      in the Fortran array.
      */
      if ( dtype == SPICE_CHR )
      {
         cstrlen = cell->length;

         cell->size = sizec_ (  ( char      * ) fcell,
                                ( ftnlen      ) cstrlen-1 );
         cell->card = cardc_ (  ( char      * ) fcell,
                                ( ftnlen      ) cstrlen-1 );
      }
 
      else if ( dtype == SPICE_DP )
      {
         cell->size = sized_ (  ( doublereal * ) fcell  );
         cell->card = cardd_ (  ( doublereal * ) fcell  );
      }

      else if ( dtype == SPICE_INT )
      {
         cell->size = sizei_ (  ( integer * ) fcell  );
         cell->card = cardi_ (  ( integer * ) fcell  );
      }

      else
      {
         chkin_c  ( "zzsynccl_c"                    );
         setmsg_c ( "Invalid data type code # seen" ); 
         errint_c ( "#",  (SpiceInt) dtype          );   
         sigerr_c ( "SPICE(NOTSUPPORTED)"           );
         chkout_c ( "zzsynccl_c"                    );
         return;
      }
   }
}
