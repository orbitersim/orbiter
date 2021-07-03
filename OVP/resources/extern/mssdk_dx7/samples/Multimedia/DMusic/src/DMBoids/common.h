#ifndef COMMON_H
#define COMMON_H
/*
**-----------------------------------------------------------------------------
**  File:       Common.h
**  Purpose:    Common definitions and include files
**
**  Microsoft Copyright (c) 1995-1999, All rights reserved.
**-----------------------------------------------------------------------------
*/


/*
**-----------------------------------------------------------------------------
**  Include files
**-----------------------------------------------------------------------------
*/
#define STRICT
//#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <math.h>
#include <assert.h>
#include <tchar.h>

#include <ddraw.h>

// Note:  Must Define D3D_OVERLOADS to get C++ version of D3DMATRIX
#define D3D_OVERLOADS
#include <d3d.h>
#include <d3dtypes.h>

#include "resource.h"


/*
**-----------------------------------------------------------------------------
**  Defines
**-----------------------------------------------------------------------------
*/
#ifndef FAR
    #define FAR
#endif

#ifndef NEAR
    #define NEAR
#endif

#ifndef CONST
    #define CONST const
#endif

#ifndef MAX_PATH
    #define MAX_PATH 260
#endif

#ifndef MAX_STRING
    #define MAX_STRING 260
#endif

#ifndef NULL
    #ifdef __cplusplus
        #define NULL 0
    #else
        #define NULL ((void *)0)
    #endif
#endif

#ifndef FALSE
    #define FALSE 0
#endif

#ifndef TRUE
    #define TRUE 1
#endif


/*
**-----------------------------------------------------------------------------
**  Typedefs
**-----------------------------------------------------------------------------
*/


    
/*
**-----------------------------------------------------------------------------
**  Macros
**-----------------------------------------------------------------------------
*/
#ifndef max
    #define max(a,b)            (((a) > (b)) ? (a) : (b))
#endif

#ifndef min
    #define min(a,b)            (((a) < (b)) ? (a) : (b))
#endif

#ifndef max3
    #define max3(a,b,c)         (((a) > (b)) ? (((a) > (c)) ? (a) : (c)) : (((b) > (c)) ? (b) : (c)))
#endif

#ifndef min3
    #define min3(a,b,c)         (((a) < (b)) ? (((a) < (c)) ? (a) : (c)) : (((b) < (c)) ? (b) : (c)))
#endif

#ifndef clamp
    #define clamp(L,V,U)        (((L) > (V)) ? (L) : (((U) < (V)) ? (U) : (V)))
#endif


/*
**-----------------------------------------------------------------------------
**  End of File
**-----------------------------------------------------------------------------
*/
#endif // COMMON_H


