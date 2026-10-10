
/*

-Header_File SpiceDAS.h ( CSPICE DAS-specific definitions )

-Abstract

   Perform CSPICE DAS-specific definitions, including macros.
         
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
   
-Particulars

   This header defines macros that may be referenced in application
   code that calls CSPICE DAS functions.

   Macros
   ======
   
      Name                  Description
      ----                  -----------
      SPICE_DAS_FTSIZE      is the maximum number of DAS files that can be
                            open at any one time.
      SPICE_DAS_CHARDT,
      SPICE_DAS_DPDT,
      SPICE_DAS_INTDT       are data type specifiers which indicate SpiceChar,
                            SpiceDouble, and SpiceInt respectively. These
                            parameters are used in all DAS routines that
                            require a data type specifier.

-Literature_References

   None.

-Author_and_Institution

   J. Diaz del Rio     (ODC Space)
   
-Restrictions

   It is recommended that the default values defined in this file be
   changed only by expert SPICE users.     
      
-Version

   -CSPICE Version 1.0.0, 07-APR-2020 (JDR)  

*/


#ifndef HAVE_SPICE_DAS_H

   #define HAVE_SPICE_DAS_H


   /*
   Constants
   */


   /*
   Fortran maximum number of DAS files that can be open at any one time:
   */
   #define  SPICE_DAS_FTSIZE                5000


   /*
   DAS data type specifiers used in all DAS routines that require
   a data type either as input or to extract data from an output
   array.

   SPICE_DAS_CHARDT,
   SPICE_DAS_DPDT,
   SPICE_DAS_INTDT    are data type specifiers which indicate SpiceChar,
                      SpiceDouble, and SpiceInt respectively. These
                      parameters are used in all DAS routines that require a
                      data type specifier.
   */
   #define  SPICE_DAS_CHARDT                0
   #define  SPICE_DAS_DPDT                  1
   #define  SPICE_DAS_INTDT                 2


#endif

/*
End of header file SpiceDAS.h
*/

