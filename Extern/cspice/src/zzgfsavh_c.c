/*

-Procedure zzgfsavh_c ( GF, save interrupt handler status )

-Abstract
 
   CSPICE Private routine intended solely for the support of CSPICE
   routines.  Users should not call this routine directly due
   to the volatile nature of this routine.

   Store the interrupt handler status polled by gfbail_c.
 
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
 
   GF
 
-Keywords
 
   GEOMETRY
   SEARCH
   UTILITY 
 
*/

   #include <signal.h>
   #include "SpiceUsr.h"
 

   static SpiceBoolean signalStatus = SPICEFALSE;


   void zzgfsavh_c ( SpiceBoolean status ) 

/*

-Brief_I/O
 
   VARIABLE  I/O  DESCRIPTION 
   --------  ---  -------------------------------------------------- 
   status     I   Interrupt status.
 
-Detailed_Input
 
   status     is a logical flag indicating whether the most 
              recent instance of the interrupt signal
              SIGINT has processed by the GF subsystem. See
              the Particulars section below for details.

              The value of `status' is stored in static memory by this routine.

   
-Detailed_Output
 
   None.
 
-Parameters
 
   None. 
 
-Exceptions
 
   None. 
 
-Files
 
   None. 
 
-Particulars
  
   The static status flag `signalStatus' is initialized by this routine
   to SPICEFALSE. If an interrupt signal is raised and the default GF
   interrupt polling routine gfbail_c is used, then the interrupt
   signal handler gfinth_c will set the interrupt status to SPICETRUE.
   The interrupt status must be cleared via a call to gfclrh_c before
   interrupt processing can resume.

   This file shares access to the static variable `signalStatus' with
   the routine zzgfgeth_c.
 
-Examples
 
   See usage in gfinth_c.

-Restrictions
 
   None.
 
-Literature_References
 
   None. 
 
-Author_and_Institution
 
   N.J. Bachman   (JPL) 
 
-Version
 
   -CSPICE Version 1.0.0, 13-APR-2009 (NJB)

-Index_Entries
 
   GF save interrupt status
 
-&
*/

{ /* Begin zzgfsavh_c */


   /*
   Simply save the input status value. 
   */

   signalStatus = status;


} /* End zzgfsavh_c */





/*

-Procedure zzgfgeth_c ( GF, get interrupt handler status )

-Abstract
 
   CSPICE Private routine intended solely for the support of CSPICE
   routines.  Users should not call this routine directly due
   to the volatile nature of this routine.

   Return the saved interrupt handler status.
 
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
 
   GF
 
-Keywords
 
   GEOMETRY
   SEARCH
   UTILITY 
 
*/

   SpiceBoolean zzgfgeth_c ( void ) 

/*

-Brief_I/O
 
   VARIABLE  I/O  DESCRIPTION 
   --------  ---  -------------------------------------------------- 

   The function returns the saved interrupt status.
 
-Detailed_Input
 
   None.
   
-Detailed_Output
 
   This function returns the interrupt signal status stored 
   by the last call to zzgfsavh_c. If no such call has occurred,
   then the value SPICEFALSE is returned.
 
-Parameters
 
   None. 
 
-Exceptions
 
   None. 
 
-Files
 
   None. 
 
-Particulars
  
   The static status flag `signalStatus' is initialized by this routine
   to SPICEFALSE. If an interrupt signal is raised and the default GF
   interrupt polling routine gfbail_c is used, then the interrupt
   signal handler gfinth_c will set the interrupt status to SPICETRUE.
   The interrupt status must be cleared via a call to gfclrh_c before
   interrupt processing can resume.

   This file shares access to the static variable `signalStatus' with
   the routine zzgfsavh_c.
 
-Examples
 
   See usage in gfbail_c.

-Restrictions
 
   None.
 
-Literature_References
 
   None. 
 
-Author_and_Institution
 
   N.J. Bachman   (JPL) 
 
-Version
 
   -CSPICE Version 1.0.0, 13-APR-2009 (NJB)

-Index_Entries
 
   GF get interrupt status
 
-&
*/

{ /* Begin zzgfgeth_c */


   /*
   Simply return the saved status value. 
   */

   return ( signalStatus );


} /* End zzgfgeth_c */


