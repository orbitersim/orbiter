//****************************************************************************
//* Program: Mosquito
//*
//* Description:
//*     Sample application to demonstrate the creation, display, and 
//*     positioning of overlays.
//*
//* Contents:
//*     WinMain() - Main entry point for this application.
//*     MainDlgProc() - Message proc for this app.
//*     InitDirectDraw() - Creates core DirectDraw objects.
//*     FreeDirectDraw() - Frees up core DirectDraw objects.
//*     CreatePrimarySurface() - Creates Primary surface.
//*     CreateOverlay() - Creates a multi-buffer flippable overlay surface.
//*     DestroyOverlay() - Hides overlay surface and releases it.
//*     CopyBitmapToYUVSurface() - Copies a GDI bitmap to a YUV surface.
//*                                Used by LoadImageOntoSurface().
//*     LoadImageOntoSurface() - Loads a resource based bitmap onto a surface.
//*     EnumAttachedCallback() - Callback for EnumAttachedSurfaces().
//*     DisplayOverlay() - Displays our overlay on the primary.
//*     RestoreAllSurfaces() - Restores the primary and overlay surfaces.
//*     MoveOverlayTimerCallback() - TimerProc for moving the overlay surface.
//*
//* THIS CODE AND INFORMATION IS PROVIDED "AS IS" WITHOUT WARRANTY OF 
//* ANY KIND, EITHER EXPRESSED OR IMPLIED, INCLUDING BUT NOT LIMITED TO 
//* THE IMPLIED WARRANTIES OF MERCHANTABILITY AND/OR FITNESS FOR A 
//* PARTICULAR PURPOSE.
//*
//* Copyright (C) 1996-1999 Microsoft Corporation.  All Rights Reserved.
//****************************************************************************

// C RunTime Header Files
#include <stdlib.h>
#include <malloc.h>
#include <memory.h>

// Windows Header Files:
#include <windows.h>
#include <mmsystem.h> //added to compile under NT
#define INITGUID
#include <objbase.h>
#include <initguid.h>
#include <ddraw.h>                                                                

// Local Headers
#include "resource.h"

// Define some macros
#define INIT_DIRECTDRAW_STRUCT(x) (ZeroMemory(&x, sizeof(x)), x.dwSize=sizeof(x))
#define RAND_INT(x) (rand()*x/RAND_MAX)
#define RANDOM_VELOCITY() ((RAND_INT(5)+3)*2)

// Global Variables:

LPDIRECTDRAW7        g_lpdd = NULL;
LPDIRECTDRAWSURFACE7 g_lpddsPrimary = NULL;
LPDIRECTDRAWSURFACE7 g_lpddsOverlay = NULL;

int g_nOverlayXPos, g_nOverlayYPos;
int g_nOverlayXVel, g_nOverlayYVel;
int g_nOverlayWidth, g_nOverlayHeight;
int g_nOverlayFlipCounter; 
DWORD g_dwOverlayXPositionAlignment;  // used to keep track of any alignment
// restrictions on the X postion of
// our overlay surface.  Comes from the
// the DDCAPS dwBoundaryAlignDest field.
// Initialized in DisplayOverlay().

// These are the pixel formats this app supports.  Most display adapters
// with overlay support will recognize one or more of these formats.
// The first on our list is the 16-bit RGB formats.  These have the widest
// support.
DDPIXELFORMAT g_ddpfOverlayFormats[] = 
{   {sizeof(DDPIXELFORMAT), DDPF_RGB, 0, 16,  0x7C00, 0x03e0, 0x001F, 0},      // 16-bit RGB 5:5:5
{sizeof(DDPIXELFORMAT), DDPF_RGB, 0, 16,  0xF800, 0x07e0, 0x001F, 0},   // 16-bit RGB 5:6:5
{sizeof(DDPIXELFORMAT), DDPF_FOURCC,MAKEFOURCC('U','Y','V','Y'),0,0,0,0,0}, // UYVY
{sizeof(DDPIXELFORMAT), DDPF_FOURCC,MAKEFOURCC('Y','U','Y','2'),0,0,0,0,0}};  // YUY2

#define NUM_OVERLAY_FORMATS (sizeof(g_ddpfOverlayFormats) / sizeof(g_ddpfOverlayFormats[0]))

// Define some error messages.  If we had many more we'd stick 'em in our RC file.
#define NO_OVERLAY_HARDWARE     "Sorry, In order to run this sample application you must "\
"have a display adapter and driver which support overlays."

#define UNABLE_TO_CREATE_OVERLAY    "Sorry, your display adapter appears to "\
    "support overlays, but we were unable to "\
    "create an overlay in any of the formats "\
    "this app supports (16-bit RGB and YUV).  "\
    "You may want to try shutting down other "\
    "DirectX apps to free video memory, or try "\
    "rerunning this app in a different display "\
"mode."

#define UNABLE_TO_DISPLAY_OVERLAY   "Sorry, we created an overlay on your "\
    "system, but were unable to display it.  "\
    "Please try rerunning this app in "\
"a different display mode."

//****************************************************************************
//* Function: FreeDirectDraw
//*
//* Description:
//*             Releases core DirectDraw objects
//****************************************************************************
void FreeDirectDraw(void)
{
    if (g_lpddsPrimary)
    {
        g_lpddsPrimary->Release();
        g_lpddsPrimary=NULL;
    }
    
    if (g_lpdd)
    {
        g_lpdd->Release();
        g_lpdd=NULL;
    }
}


//****************************************************************************
//* Function: CreatePrimarySurface()
//*
//* Description:
//*     Creates a Primary Surface.  Implemented as a separate function from
//* InitDirectDraw() because we occasionally need to just recreate the primary
//* surface such as when a mode change occurs.
//****************************************************************************
HRESULT CreatePrimarySurface()
{
    DDSURFACEDESC2   ddsd;
    HRESULT         ddrval;
    
    if (!g_lpdd) return E_FAIL;
    
    INIT_DIRECTDRAW_STRUCT(ddsd);
    ddsd.dwFlags = DDSD_CAPS;
    ddsd.ddsCaps.dwCaps = DDSCAPS_PRIMARYSURFACE;
    ddrval = g_lpdd->CreateSurface(&ddsd, &g_lpddsPrimary, NULL );
    
    return ddrval;
}


//****************************************************************************
//* Function: InitDirectDraw
//*
//* Description:
//*     Performs DirectDraw initialization.  Creates a primary surface which
//*     is needed to display our overlay on.  The actual overlay surface is
//*     created later in our CreateOverlay() call.
//****************************************************************************
BOOL InitDirectDraw()
{
    HRESULT         ddrval;
    
    ddrval = DirectDrawCreateEx(NULL, (VOID**)&g_lpdd, IID_IDirectDraw7, NULL);
    if( FAILED(ddrval))
        goto ErrorOut;
    
    // For NORMAL cooperative level we no longer need to provide an HWND.
    ddrval = g_lpdd->SetCooperativeLevel(NULL, DDSCL_NORMAL);
    if( FAILED(ddrval))
        goto ErrorOut;
    
    ddrval= CreatePrimarySurface();
    if( FAILED(ddrval))
        goto ErrorOut;
    
    return TRUE;
    
ErrorOut:
    
    FreeDirectDraw();
    
    return FALSE;
}


//****************************************************************************
//* Function: DestroyOverlay()
//* Description:
//*     Hides the overlay then releases it's surface.
//****************************************************************************
void DestroyOverlay()
{
    if (g_lpddsOverlay)
    {
        // Use UpdateOverlay() with the DDOVER_HIDE flag to remove an overlay 
        // from the display.
        g_lpddsOverlay->UpdateOverlay(NULL, g_lpddsPrimary, NULL, DDOVER_HIDE, NULL);
        g_lpddsOverlay->Release();
        g_lpddsOverlay=NULL;
    }
}


//****************************************************************************
//* Function: CopyBitmapToYUVSurface
//* Description: 
//*     Copies an RGB GDI bitmap to a YUV surface. Both bitmap and surface
//*     must be a multiple of 2 pixels in width for the supported YUV formats.  
//*     The following formats are supported:
//*             YUY2
//*             UYVY
//*     
//*     The "YUY2" YUV pixel format looks like this:
//*         As a series of BYTES:    [Y0][U][Y1][V] (reverse it for a DWORD)
//*
//*     The "UYVY" YUV pixel format looks like this:
//*         As a series of BYTES:    [U][Y0][V][Y1] (reverse it for a DWORD)
//*
//*     As you can see, both formats pack two pixels into a single DWORD. The 
//*     pixels share U and V components and have separate Y components.
//*     
//* Returns: TRUE if successful, otherwise FALSE.
//****************************************************************************
BOOL CopyBitmapToYUVSurface(LPDIRECTDRAWSURFACE7 lpDDSurf, HBITMAP hbm)
{
    HDC                 hdcImage;
    HRESULT             ddrval;
    DDSURFACEDESC2      ddsd;
    DWORD               x, y, dwWidth, dwHeight;
    LONG                lPitch;
    LPBYTE              pSurf;
    DWORD               dwBytesInRow;
    COLORREF            color;
    BYTE                R,G,B, Y0,Y1,U,V;
    BOOL                bRet = FALSE;
    
    if (hbm == NULL || lpDDSurf == NULL)
        return FALSE;
    
    //
    //  select bitmap into a memoryDC so we can use it.
    //
    hdcImage = CreateCompatibleDC(NULL);
    SelectObject(hdcImage, hbm);
    
    INIT_DIRECTDRAW_STRUCT(ddsd);
    // Lock down the surface so we can modify it's contents.
    ddrval=lpDDSurf->Lock( NULL, &ddsd, DDLOCK_SURFACEMEMORYPTR|DDLOCK_WAIT, NULL);
    if (FAILED(ddrval))
        goto CleanUp;
    
    dwWidth=ddsd.dwWidth;
    dwHeight=ddsd.dwHeight;
    lPitch=ddsd.lPitch;
    pSurf=(LPBYTE)ddsd.lpSurface;
    dwBytesInRow=ddsd.dwWidth*2;
    
    // Go through the image 2 pixels at a time and convert to YUV
    for(y=0; y<dwHeight; y++)
    {
        for(x=0; x<dwWidth; x+=2)
        {
            // The equations for color conversion used here, probably aren't 
            // exact, but they seem to do an OK job.
            color=GetPixel(hdcImage, x,y);
            R=GetRValue(color);
            G=GetGValue(color);
            B=GetBValue(color);
            Y0= (BYTE)(0.29*R + 0.59*G + 0.14*B);
            U= (BYTE)(128.0 - 0.14*R - 0.29*G + 0.43*B);
            
            color=GetPixel(hdcImage, x+1,y);
            R=GetRValue(color);
            G=GetGValue(color);
            B=GetBValue(color);
            Y1= (BYTE)(0.29*R + 0.57*G + 0.14*B);
            V= (BYTE)(128.0 + 0.36*R - 0.29*G - 0.07*B);
            
            switch (ddsd.ddpfPixelFormat.dwFourCC)
            {
            case MAKEFOURCC('Y','U','Y','2'): 
                *(pSurf++) = Y0;
                *(pSurf++) = U;
                *(pSurf++) = Y1;
                *(pSurf++) = V;
                break;
            case MAKEFOURCC('U','Y','V','Y'): 
                *(pSurf++) = U;
                *(pSurf++) = Y0;
                *(pSurf++) = V;
                *(pSurf++) = Y1;
                break;
            }                       
        }
        pSurf+=(lPitch-dwBytesInRow);
    }
    
    lpDDSurf->Unlock(NULL);     
    
CleanUp:
    if(hdcImage)
        DeleteDC(hdcImage);
    
    return TRUE;
}

//****************************************************************************
//* Function: LoadImageOnToSurface
//* Description:
//*     Loads a resource based bitmap image onto a DirectDraw surface.  Can
//*     covert the bitmap to all RGB formats, plus a couple YUV formats.
//****************************************************************************
BOOL LoadImageOntoSurface(LPDIRECTDRAWSURFACE7 lpdds, LPSTR lpstrResID)
{
    HBITMAP hbm;
    HDC     hdcImage= NULL;
    HDC     hdcSurf = NULL;
    BOOL bRetVal = FALSE;
    HRESULT ddrval;
    DDSURFACEDESC2 ddsd;
    
    if (!lpdds)
        return FALSE;
    
    //
    // get surface size and format.
    //
    INIT_DIRECTDRAW_STRUCT(ddsd);
    ddsd.dwFlags = DDSD_HEIGHT | DDSD_WIDTH;
    ddrval = lpdds->GetSurfaceDesc(&ddsd);
    if (FAILED(ddrval))
        goto Exit;
    
    // Load the bitmap resource.  We'll use LoadImage() since it'll scale the 
    // image to fit our surface, and maintain the color information in the
    // bitmap.
    hbm = (HBITMAP)LoadImage(GetModuleHandle(NULL), MAKEINTRESOURCE(lpstrResID), IMAGE_BITMAP, ddsd.dwWidth, ddsd.dwHeight, LR_CREATEDIBSECTION);
    if (hbm == NULL)
        goto Exit;
    
    
    // If our surface is a FOURCC YUV format, we need to do a little work to convert
    // our RGB resource bitmap into the appropriate YUV format.
    if (ddsd.ddpfPixelFormat.dwFlags == DDPF_FOURCC)
    {
        if (!CopyBitmapToYUVSurface(lpdds, hbm))
            goto Exit;        
    }
    else  //Looks like we're just using a standard RGB surface format, let GDI do the work.
    {
        // Create a DC and associate the bitmap with it.
        hdcImage = CreateCompatibleDC(NULL);
        SelectObject(hdcImage, hbm);
        
        ddrval = lpdds->GetDC(&hdcSurf);
        if (FAILED(ddrval))
            goto Exit;
        
        if (BitBlt(hdcSurf, 0, 0, ddsd.dwWidth, ddsd.dwHeight, hdcImage, 0, 0, SRCCOPY) == FALSE)
            goto Exit;
    }
    
    bRetVal = TRUE;
    
Exit:
    if (hdcSurf)
        lpdds->ReleaseDC(hdcSurf);
    if (hdcImage)
        DeleteDC(hdcImage);
    if(hbm)
        DeleteObject(hbm);
    
    return bRetVal;
    
}


//****************************************************************************
//* Function: EnumAttachedCallback
//* 
//* Description:
//*     Callback function for EnumAttachedSurfaces().  Used to recursively
//*     load each frame of the flying insect animation onto the appropriate
//*     overlay surface buffer.
//****************************************************************************
HRESULT WINAPI EnumAttachedCallback(LPDIRECTDRAWSURFACE7 lpdds, LPDDSURFACEDESC2 lpddsd, LPVOID lpContext)
{
    int     nResourceID = (int)lpContext;
    HRESULT hr = DDENUMRET_OK;
    
    // Check to see if we've gone full circle through all surfaces and are now looking at the first one again.
    if (lpdds == g_lpddsOverlay)
        goto Exit;
    
    if (!LoadImageOntoSurface(lpdds, MAKEINTRESOURCE(nResourceID)))
    {
        hr = DDENUMRET_CANCEL;
        goto Exit;
    }
    
    nResourceID++;
    
    lpdds->EnumAttachedSurfaces((LPVOID)nResourceID, EnumAttachedCallback);    
    
Exit:
    lpdds->Release();
    return DDENUMRET_OK;
}

//****************************************************************************
//* Function: DisplayOverlay
//*
//* Description:
//*     Displays the overlay on the primary surface
//****************************************************************************
BOOL DisplayOverlay()
{
    HRESULT         ddrval;
    RECT            rs, rd;
    DDOVERLAYFX     ovfx;
    DDCAPS          capsDrv;
    unsigned int    uStretchFactor1000;
    unsigned int    uDestSizeAlign, uSrcSizeAlign;
    DWORD           dwUpdateFlags;
    
    if(!g_lpdd || !g_lpddsPrimary || !g_lpddsOverlay)
        return FALSE;
    
    // Get driver capabilities
    INIT_DIRECTDRAW_STRUCT(capsDrv);
    ddrval = g_lpdd->GetCaps(&capsDrv, NULL);
    if (FAILED(ddrval))
        return FALSE;
    
    // We need to check the minimum stretch.  Many display adpators require that
    // the overlay be stretched by a minimum amount.  The stretch factor will 
    // usually vary with the display mode (including changes in refresh rate).
    // The stretch factor is returned x1000.
    uStretchFactor1000 = capsDrv.dwMinOverlayStretch>1000 ? capsDrv.dwMinOverlayStretch : 1000;
    
    // Grab any alignment restrictions.  The DDCAPS struct contains a series of
    // alignment fields that are not clearly defined. They are intended for
    // overlay use.  It's important to observe alignment restrictions.
    // Many adapters with overlay capabilities require the overlay image be
    // located on 4 or even 8 byte boundaries, and have similar restrictions
    // on the overlay width (for both source and destination areas).
    uDestSizeAlign = capsDrv.dwAlignSizeDest;
    uSrcSizeAlign =  capsDrv.dwAlignSizeSrc;
    
    // Set the flags we'll send to UpdateOverlay
    dwUpdateFlags = DDOVER_SHOW | DDOVER_DDFX;
    
    // Does the overlay hardware support source color keying?
    // If so, we can hide the black background around the image.
    // This probably won't work with YUV formats
    if (capsDrv.dwCKeyCaps & DDCKEYCAPS_SRCOVERLAY)
        dwUpdateFlags |= DDOVER_KEYSRCOVERRIDE;
    
    // Create an overlay FX structure so we can specify a source color key.
    // This information is ignored if the DDOVER_SRCKEYOVERRIDE flag isn't set.
    INIT_DIRECTDRAW_STRUCT(ovfx);
    ovfx.dckSrcColorkey.dwColorSpaceLowValue=0; // Specify black as the color key
    ovfx.dckSrcColorkey.dwColorSpaceHighValue=0;
    
    // Set up our Source Rect. This is the area of the overlay surface we
    // want to display.  If you want to display your entire surface and
    // happen to know for certain that your surface width meets any alignment
    // restrictions you can go ahead and pass NULL for the source rect in your
    // calls to UpdateOverlay().  Our surface width of 320 probably will meet
    // every alignment restriction, but just in case we'll create a source rect
    // and check for it.
    rs.left=0; rs.top=0; // position 0,0 is already position (boundary aligned)
    rs.right = 320;
    rs.bottom = 240;
    //Apply any size alignment restrictions if necessary.
    if (capsDrv.dwCaps & DDCAPS_ALIGNSIZESRC && uSrcSizeAlign)
        rs.right -= rs.right % uSrcSizeAlign;
    
    // Set up our destination rect, indicating where we want the overlay to 
    // appear on the primary surface.  This is where we have to take into 
    // account any stretch factor which may be needed to ensure the overlay is
    // displayed.  Really only the destination width must be stretched for the
    // overlay hardware to work, but we stretch the height as well just to
    // maintain a proper aspect ratio.
    
    
    // Note: We use the source rect dimensions, not the surface dimensions in
    // case they differ.
    // UpdateOverlay will fail unless the minimum stretch value is observed.
    
    rd.left=0; rd.top=0; 
    rd.right  = (rs.right*uStretchFactor1000+999)/1000; // adding 999 takes care of integer truncation problems.
    rd.bottom = rs.bottom*uStretchFactor1000/1000;
    
    // It's also important to observe any alignment restrictions on size and
    // position with respect to the destination rect. Tweak the destination 
    // width a bit more to get the size alignment correct (Be sure to round up
    // to keep any minimum stretch restrictions met). we'll assume the 
    // position 0,0 is already "position aligned".
    if (capsDrv.dwCaps & DDCAPS_ALIGNSIZEDEST && uDestSizeAlign)
        rd.right = (int)((rd.right+uDestSizeAlign-1)/uDestSizeAlign)*uDestSizeAlign;
    
    // Make the call to UpdateOverlay() which actually displays the overlay on
    // the screen.
    ddrval = g_lpddsOverlay->UpdateOverlay(&rs, g_lpddsPrimary, &rd, dwUpdateFlags, &ovfx);
    if(FAILED(ddrval))
    {
        // Ok, the call to UpdateOVerlay() failed.  A likely cause is the
        // driver lied about the minimum stretch needed. 
        // Ideally we should try upping the destination size a bit, or
        // perhaps shrinking the source size so the destination stretch
        // is effectively higher.   For this sample, however, we'll just
        // bail!
        return FALSE;
    }
    
    // Set the initial position and velocity for our overlay.  We'll actually
    // move the image around using a timer proc (see MoveOverlayTimerCallback
    // below).
    g_nOverlayXPos = 0;
    g_nOverlayYPos = 0;
    g_nOverlayXVel = RANDOM_VELOCITY();
    g_nOverlayYVel = RANDOM_VELOCITY();
    g_nOverlayWidth = rd.right - rd.left;
    g_nOverlayHeight = rd.bottom - rd.top;
    g_nOverlayFlipCounter=0;
    
    // Set the "destination position alignment" global so we won't have to
    // keep calling GetCaps() everytime we move the overlay surface.
    if (capsDrv.dwCaps & DDCAPS_ALIGNBOUNDARYDEST)
        g_dwOverlayXPositionAlignment = capsDrv.dwAlignBoundaryDest;
    else 
        g_dwOverlayXPositionAlignment = 0;
    
    return TRUE;
}


//****************************************************************************
//* Function: AreOverlaysSupported
//*
//* Description:
//*     Determines whether or not the display hardware supports overlays.  If
//*     so, the function returns TRUE, otherwise FALSE.
//****************************************************************************
BOOL AreOverlaysSupported()
{
    DDCAPS  capsDrv;
    HRESULT ddrval;
    
    // Get driver capabilities to determine Overlay support.
    INIT_DIRECTDRAW_STRUCT(capsDrv);
    ddrval = g_lpdd->GetCaps(&capsDrv, NULL);
    if (FAILED(ddrval))
        return FALSE;
    
    // Does the driver support overlays in the current mode? 
    // (Currently the DirectDraw emulation layer does not support overlays.
    // Overlay related APIs will fail without hardware support).  
    if (!(capsDrv.dwCaps & DDCAPS_OVERLAY))
        return FALSE;
    
    return TRUE;
}


//****************************************************************************
//* Function: CreateOverlay
//*
//* Description:
//*     This is where we create the overlay surface, and put the flying insect 
//*     artwork on it.
//****************************************************************************
BOOL CreateOverlay()
{
    DDSURFACEDESC2   ddsdOverlay;
    HRESULT         ddrval;
    int             i;
    
    if (!g_lpdd || !g_lpddsPrimary)
        return FALSE;
    
    // It's currently not possible to query for pixel formats supported by the
    // overlay hardware (though GetFourCCCodes() usually provides a partial 
    // list).  Instead you need to call CreateSurface() to try a variety of  
    // formats till one works.  
    INIT_DIRECTDRAW_STRUCT(ddsdOverlay);
    ddsdOverlay.ddsCaps.dwCaps=DDSCAPS_OVERLAY | DDSCAPS_FLIP | DDSCAPS_COMPLEX | DDSCAPS_VIDEOMEMORY;
    ddsdOverlay.dwFlags= DDSD_CAPS|DDSD_HEIGHT|DDSD_WIDTH|DDSD_BACKBUFFERCOUNT| DDSD_PIXELFORMAT;
    ddsdOverlay.dwWidth=320;
    ddsdOverlay.dwHeight=240;
    ddsdOverlay.dwBackBufferCount=2;
    
    // Try to create an overlay surface using one of the pixel formats in our
    // global list.
    i=0;
    do 
    {
        ddsdOverlay.ddpfPixelFormat=g_ddpfOverlayFormats[i];
        // Try to create the overlay surface
        ddrval = g_lpdd->CreateSurface(&ddsdOverlay, &g_lpddsOverlay, NULL);
    } while( FAILED(ddrval) && (++i < NUM_OVERLAY_FORMATS) );
    
    // If we failed to create an overlay surface, let's try again with a single
    // (non-flippable) buffer.
    if(FAILED(ddrval))
    {
        // Just in case we're short on video memory or the hardware doesn't like flippable
        // overlay surfaces, let's make another pass using a single buffer.
        ddsdOverlay.dwBackBufferCount=0;
        ddsdOverlay.ddsCaps.dwCaps=DDSCAPS_OVERLAY | DDSCAPS_VIDEOMEMORY;
        ddsdOverlay.dwFlags= DDSD_CAPS|DDSD_HEIGHT|DDSD_WIDTH|DDSD_PIXELFORMAT;
        // Try to create the overlay surface
        ddrval = g_lpdd->CreateSurface(&ddsdOverlay, &g_lpddsOverlay, NULL);
        i=0;
        do 
        {
            ddsdOverlay.ddpfPixelFormat=g_ddpfOverlayFormats[i];
            ddrval = g_lpdd->CreateSurface(&ddsdOverlay, &g_lpddsOverlay, NULL);
        } while( FAILED(ddrval) && (++i < NUM_OVERLAY_FORMATS) );
        
        // We just couldn't create an overlay surface.  Let's exit.
        if (FAILED(ddrval))
            return FALSE;
    }
    
    // Put the first bug image onto the first buffer of our complex surface.
    if (!LoadImageOntoSurface(g_lpddsOverlay, MAKEINTRESOURCE(IDB_BUGIMAGE1)))
        return FALSE;
    
    // This will recursively get all back buffers and load them with the appropriate image.
    ddrval = g_lpddsOverlay->EnumAttachedSurfaces((LPVOID)IDB_BUGIMAGE2, EnumAttachedCallback);
    if(FAILED(ddrval))
        return FALSE;
    
    return TRUE;
}


//****************************************************************************
//* Function: RestoreAllSurfaces
//****************************************************************************
BOOL RestoreAllSurfaces()
{
    HRESULT ddrval;
    
    // It's possible that our surfaces were destroyed in a prior call to 
    // RestoreAllSurfaces().  If this happened we need to check for it, 
    // and perhaps try to recreate the surfaces.
    if (!g_lpddsPrimary)
    {
        ddrval = CreatePrimarySurface();
        if (FAILED(ddrval))
        {
            // We probably couldn't recreate the primary because someone has
            // exclusive mode.
            g_lpddsPrimary = NULL;
            return FALSE;
        }
    }
    
    if(!g_lpddsOverlay)
        if (!CreateOverlay())
            return FALSE;
        
        // Try Restoring the primary surface.
        ddrval = g_lpddsPrimary->Restore();
        if(FAILED(ddrval))
        {
            // If we weren't able to restore the primary surface, It's probably 
            // because some one else has exclusive mode, or the display mode
            // has been changed such that our primary surface needs to be recreated
            
            // Check to see if the mode changed on us.  Is so, we'll need to recreate
            // all surfaces.  (Note: we could have watched for the WM_DISPLAYCHANGE 
            // message as well)
            if (ddrval == DDERR_WRONGMODE)
            {
                g_lpddsPrimary->Release();
                DestroyOverlay();
                g_lpddsPrimary = NULL;
                g_lpddsOverlay = NULL;
                
                return FALSE;
            }
            
            else 
                return FALSE;
        }
        
        // Try Restoring the overlay surface.
        ddrval = g_lpddsOverlay->Restore();
        if (FAILED(ddrval))
            return FALSE;
        
        // Reload artwork onto overlay
        // Put the first bug image onto the first buffer of our complex surface.
        if ( !LoadImageOntoSurface(g_lpddsOverlay, MAKEINTRESOURCE(IDB_BUGIMAGE1)) )
            return FALSE;
        
        // This will recursively get all back buffers and load them with the appropriate image.
        ddrval = g_lpddsOverlay->EnumAttachedSurfaces((LPVOID)IDB_BUGIMAGE2, EnumAttachedCallback);
        if(FAILED(ddrval))
            return FALSE;
        
        // Redisplay overlay
        if(!DisplayOverlay())
            return FALSE;
        
        return TRUE;
}


//****************************************************************************
//* Function: MoveOverlayTimerCallback
//*
//* Description:
//*     TimeProc callback for moving the overlay surface around
//****************************************************************************
void CALLBACK MoveOverlayTimerCallback(HWND hwndUnused, UINT uUnused, UINT uUnused2, DWORD dwUnused)
{
    HRESULT ddrval;
    DWORD   dwXAligned;
    
    // Make sure the overlay really exists before we mess with it.
    if (!g_lpddsOverlay)
        if (!RestoreAllSurfaces())
            return;
        
        //Add the current velocity vectors to the position.
        g_nOverlayXPos += g_nOverlayXVel;
        g_nOverlayYPos += g_nOverlayYVel;
        
        // Check to see if this new position puts the overlay off the edge of the screen.
        // SetOverlayPosition() won't like that.
        
        // Have we gone off the left edge?
        if (g_nOverlayXPos < 0)
        {
            g_nOverlayXPos = 0;
            g_nOverlayXVel = RANDOM_VELOCITY();
        }
        
        // Have we gone off the right edge?
        if ( (g_nOverlayXPos+g_nOverlayWidth) >  GetSystemMetrics(SM_CXSCREEN))
        {
            g_nOverlayXPos = GetSystemMetrics(SM_CXSCREEN)-g_nOverlayWidth;
            g_nOverlayXVel = -RANDOM_VELOCITY();
        }
        
        // Have we gone off the top edge?
        if (g_nOverlayYPos < 0)
        {
            g_nOverlayYPos = 0;
            g_nOverlayYVel = RANDOM_VELOCITY();
        }
        
        // Have we gone off the bottom edge?
        if ( (g_nOverlayYPos+g_nOverlayHeight) >  GetSystemMetrics(SM_CYSCREEN))
        {
            g_nOverlayYPos = GetSystemMetrics(SM_CYSCREEN)-g_nOverlayHeight;
            g_nOverlayYVel = -RANDOM_VELOCITY();
        }
        
        // We need to check for any alignment restrictions on the X position
        if (g_dwOverlayXPositionAlignment)
            dwXAligned = g_nOverlayXPos - g_nOverlayXPos % g_dwOverlayXPositionAlignment;
        else
            dwXAligned = g_nOverlayXPos;
        
        // Set the overlay to it's new position.
        ddrval = g_lpddsOverlay->SetOverlayPosition(dwXAligned, g_nOverlayYPos);
        if (ddrval == DDERR_SURFACELOST)
        {
            if (!RestoreAllSurfaces()) 
                return;
        }
        
        // Every fourth time this timer proc is called, lets flip the overlay surface.
        if (g_nOverlayFlipCounter++ > 3) 
        {
            g_nOverlayFlipCounter = 0; // reset counter
            // Flip to the next image in the sequence.  This is gonna fail if we
            // our overlay surface only contains one buffer (see CreateOverlay. 
            // This is a possibility), but the failure should be benign.
            g_lpddsOverlay->Flip(NULL, DDFLIP_WAIT);
        }
}

//****************************************************************************
//* Function: DisplayError
//* Description:
//*    Displays an error message box.
//****************************************************************************
int DisplayError(HINSTANCE hInst, LPSTR lpstrErr)
{
    MSGBOXPARAMS mbp;
    
    mbp.cbSize = sizeof(mbp);
    mbp.hwndOwner=NULL;
    mbp.hInstance=hInst;
    mbp.lpszText = lpstrErr;
    mbp.lpszCaption = "Mosquito Error!";
    mbp.dwStyle = MB_OK | MB_USERICON;
    mbp.lpszIcon = MAKEINTRESOURCE(IDI_BUGICON);
    mbp.dwContextHelpId = NULL;
    mbp.lpfnMsgBoxCallback = NULL;
    mbp.dwLanguageId = NULL;
    
    return MessageBoxIndirect(&mbp);
}

//****************************************************************************
//* Function: MainDialogProc
//****************************************************************************
LRESULT CALLBACK MainDialogProc(HWND hDlg, UINT message, WPARAM wParam, LPARAM lParam)
{
    int uTimer=0;
    
    switch (message) 
    {
    case WM_INITDIALOG:
        // Create a timer to periodically move the overlay image around.
        uTimer = SetTimer(NULL, 0, 50, (TIMERPROC)MoveOverlayTimerCallback);
        break;
        
    case WM_COMMAND:
        // Check to see if the "Quit" button was clicked.
        if ( IDQUIT == LOWORD(wParam))
        {
            KillTimer(NULL, uTimer);
            EndDialog(hDlg, TRUE);
            return (TRUE);
        }
        break;
        
    case WM_CLOSE:
        KillTimer(NULL, uTimer);
        EndDialog(hDlg, TRUE);
        return (TRUE);
    }
    
    return FALSE;
}


//****************************************************************************
//* Function: WinMain(HANDLE, HANDLE, LPSTR, int)
//*
//* Description: 
//*     Entry point for the application.  Since we use a simple dialog for 
//*     user interaction we don't need to pump messages.
//*
//****************************************************************************
int APIENTRY WinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance, LPSTR lpCmdLine, int nCmdShow)
{
    
    // Call DirectDraw Initialization function
    if (!InitDirectDraw())
        return FALSE;
    
    
    if (!AreOverlaysSupported())
    {
        // Display an error dialog if the hardware doesn't support overlays.
        DisplayError(hInstance, NO_OVERLAY_HARDWARE);
        return FALSE;
    }
    
    if (!CreateOverlay())
    {
        // display an error dialog if we couldn't create the overlay.
        DisplayError(hInstance, UNABLE_TO_CREATE_OVERLAY);
        return FALSE;
    }
    
    if (!DisplayOverlay())
    {
        // Display an error dialog if we couldn't display the overlay.
        DisplayError(hInstance, UNABLE_TO_DISPLAY_OVERLAY);
        return FALSE;
    }
    
    // Display our "Quit" dialog box.  As part of it's initialization it'll get the overlay going.
    DialogBox(hInstance, MAKEINTRESOURCE(IDD_QUITDIALOG), NULL, (DLGPROC)MainDialogProc);
    
    DestroyOverlay();
    FreeDirectDraw();
    
    lpCmdLine; // This will prevent 'unused formal parameter' warnings
    
    return TRUE;
}
