/*
**-----------------------------------------------------------------------------
** Name:    Debug.cpp
** Purpose: Example debug code for D3D sample
** Notes:
**
** Copyright (C) 1995-1999 Microsoft Corporation. All Rights Reserved.
**-----------------------------------------------------------------------------
*/

/*
**-----------------------------------------------------------------------------
** Includes
**-----------------------------------------------------------------------------
*/

#include "Debug.h"
#include "WinMain.h"
#include "WinProc.h"


/*
**-----------------------------------------------------------------------------
** Global variables
**-----------------------------------------------------------------------------
*/

#ifdef DEBUG
    DWORD   g_dwDebugLevel = DEBUG_ALWAYS;
    BOOL    g_fDebug = TRUE;
#else
    BOOL    g_fDebug = FALSE;
#endif



/*
**-----------------------------------------------------------------------------
** Local prototypes
**-----------------------------------------------------------------------------
*/

BOOL GetDDErrorString (HRESULT hResult, LPTSTR lpszError, DWORD cchError);



/*
**-----------------------------------------------------------------------------
** Functions
**-----------------------------------------------------------------------------
*/


/*
**-----------------------------------------------------------------------------
**  Name:       dprintf
**  Purpose:	Printf to debug output
**-----------------------------------------------------------------------------
*/

#ifdef DEBUG
void __cdecl dprintf (DWORD dwDebugLevel, LPCTSTR szFormat, ...)
{
    TCHAR   szBuffer[MAX_STRING];
    va_list va;

	// Check if current debug level
    if (dwDebugLevel <= g_dwDebugLevel)
	    return;
    
    lstrcpy (szBuffer, START_STR);
    va_start(va, szFormat);
    wvsprintf (szBuffer+lstrlen (szBuffer), szFormat, va);
    va_end(va);
    lstrcat (szBuffer, END_STR);
    OutputDebugString (szBuffer);
}
#endif // DEBUG



/*
**-----------------------------------------------------------------------------
** Name:    ReportDDError
** Purpose: 
**-----------------------------------------------------------------------------
*/

void _cdecl ReportDDError (HRESULT hResult, LPCTSTR szFormat, ...)
{
    TCHAR   szMsg[MAX_STRING];
    TCHAR   szErr[MAX_STRING];
    TCHAR   szUnknown[] = TEXT("Unknown");
    DWORD   cchErr = sizeof(szErr)/sizeof(TCHAR);
    va_list va;

    // Check for Success Error code
    if (hResult == DD_OK)
        return;

    // Get DD/D3D error string
    szErr[0] = 0;
    GetDDErrorString (hResult, szErr, cchErr);    
    wsprintf (szMsg, TEXT("DD/D3D Error = %s\r\n"), szErr);

    // Append the rest of the error message
    va_start( va, szFormat );
    wvsprintf (szMsg + lstrlen(szMsg), szFormat, va);
    va_end( va );

    // Dump to debugger
    DPF (DEBUG_ERROR, TEXT("%s"), szMsg);

	//
	// Define the following if you want an in your face Messagebox on errors
	//
#ifdef DEBUG_PROMPTME
    HWND    hWindow;

    // Pause App
	if (g_hMainWindow)
		OnPause (g_hMainWindow, TRUE);

    // Display Error Message to user
    if (g_hMainWindow)
        hWindow = g_hMainWindow;
    else
        hWindow = NULL;

    MessageBox (hWindow, szMsg, g_szMainTitle, MB_OK | MB_APPLMODAL);

    // Unpause app
	if (g_hMainWindow)
		OnPause (g_hMainWindow, FALSE);
#endif

} // ReportDDError



/*
**-----------------------------------------------------------------------------
** Name:    GetDDErrorString
** Purpose: outputs a debug string to debugger
**-----------------------------------------------------------------------------
*/

BOOL GetDDErrorString (HRESULT hResult, LPTSTR lpszErrorBuff, DWORD cchError)
{
    DWORD  cLen;
    LPTSTR lpszError;
    TCHAR  szMsg[MAX_STRING];

    // Check parameters
    if (!lpszErrorBuff || !cchError)
    {
        // Error, invalid parameters
        return FALSE;
    }

    switch (hResult)
    {
    case DD_OK:
        // The request completed successfully.
        lpszError = TEXT("DD_OK");
        break;

    case DDERR_ALREADYINITIALIZED:
        // The object has already been initialized.
        lpszError = TEXT("DDERR_ALREADYINITIALIZED");
        break;

    case DDERR_BLTFASTCANTCLIP:
        // A DirectDrawClipper object is attached to a source surface 
        // that has passed into a call to the IDirectDrawSurface2::BltFast method. 
        lpszError = TEXT("DDERR_BLTFASTCANTCLIP");
        break;

    case DDERR_CANNOTATTACHSURFACE:
        // A surface cannot be attached to another requested surface. 
        lpszError = TEXT("DDERR_CANNOTATTACHSURFACE");
        break;

    case DDERR_CANNOTDETACHSURFACE:
        // A surface cannot be detached from another requested surface. 
        lpszError = TEXT("DDERR_CANNOTDETACHSURFACE");
        break;

    case DDERR_CANTCREATEDC:
        // Windows cannot create any more device contexts (DCs). 
        lpszError = TEXT("DDERR_CANTCREATEDC");
        break;

    case DDERR_CANTDUPLICATE:
        // Primary and 3D surfaces, or surfaces that are 
        // implicitly created, cannot be duplicated. 
        lpszError = TEXT("DDERR_CANTDUPLICATE");
        break;

    case DDERR_CANTLOCKSURFACE:
        // Access to this surface is refused because an 
        // attempt was made to lock the primary surface without DCI support. 
        lpszError = TEXT("DDERR_CANTLOCKSURFACE"); 
        break;

    case DDERR_CANTPAGELOCK:
        // An attempt to page lock a surface failed. 
        // Page lock will not work on a display-memory 
        // surface or an emulated primary surface.
        lpszError = TEXT("DDERR_CANTPAGELOCK"); 
        break;

    case DDERR_CANTPAGEUNLOCK:
        // An attempt to page unlock a surface failed. 
        // Page unlock will not work on a display-memory 
        // surface or an emulated primary surface. 
        lpszError = TEXT("DDERR_CANTPAGEUNLOCK");
        break;

    case DDERR_CLIPPERISUSINGHWND:
        // An attempt was made to set a clip list for a DirectDrawClipper 
        // object that is already monitoring a window handle. 
        lpszError = TEXT("DDERR_CLIPPERISUSINGHWND");
        break;

    case DDERR_COLORKEYNOTSET:
        // No source color key is specified for this operation
        lpszError = TEXT("DDERR_COLORKEYNOTSET");
        break;

    case DDERR_CURRENTLYNOTAVAIL:
        // No support is currently available. 
        lpszError = TEXT("DDERR_CURRENTLYNOTAVAIL");
        break;

    case DDERR_DCALREADYCREATED:
        // A device context (DC) has already been returned for this surface. 
        // Only one DC can be retrieved for each surface. 
        lpszError = TEXT("DDERR_DCALREADYCREATED");
        break;

    case DDERR_DIRECTDRAWALREADYCREATED:
        // A DirectDraw object representing this driver 
        // has already been created for this process. 
        lpszError = TEXT("DDERR_DIRECTDRAWALREADYCREATED");
        break;

    case DDERR_EXCEPTION:
        // An exception was encountered while 
        // performing the requested operation. 
        lpszError = TEXT("DDERR_EXCEPTION");
        break;

    case DDERR_EXCLUSIVEMODEALREADYSET:
        // An attempt was made to set the cooperative 
        // level when it was already set to exclusive. 
        lpszError = TEXT("DDERR_EXCLUSIVEMODEALREADYSET");
        break;

    case DDERR_GENERIC:
        // There is an undefined error condition. 
        lpszError = TEXT("DDERR_GENERIC");
        break;

    case DDERR_HEIGHTALIGN:
        // The height of the provided rectangle 
        // is not a multiple of the required alignment. 
        lpszError = TEXT("DDERR_HEIGHTALIGN");
        break;

    case DDERR_HWNDALREADYSET:
        // The DirectDraw cooperative level window 
        // handle has already been set. It cannot 
        // be reset while the process has surfaces or palettes created. 
        lpszError = TEXT("DDERR_HWNDALREADYSET");
        break;

    case DDERR_HWNDSUBCLASSED:
        // DirectDraw is prevented from restoring state because the
        // DirectDraw cooperative level window handle has been subclassed. 
        lpszError = TEXT("DDERR_HWNDSUBCLASSED");
        break;

    case DDERR_IMPLICITLYCREATED:
        // The surface cannot be restored because 
        // it is an implicitly created surface. 
        lpszError = TEXT("DDERR_IMPLICITLYCREATED");
        break;
 
    case DDERR_INCOMPATIBLEPRIMARY:
        // The primary surface creation request 
        // does not match with the existing primary surface. 
        lpszError = TEXT("DDERR_INCOMPATIBLEPRIMARY");
        break;

    case DDERR_INVALIDCAPS:
        // One or more of the capability bits 
        // passed to the callback function are incorrect. 
        lpszError = TEXT("DDERR_INVALIDCAPS");
        break;

    case DDERR_INVALIDCLIPLIST:
        // DirectDraw does not support the provided clip list.  
        lpszError = TEXT("DDERR_INVALIDCLIPLIST");
        break;

    case DDERR_INVALIDDIRECTDRAWGUID:
        // The globally unique identifier (GUID) passed to the
        // DirectDrawCreate function is not a valid DirectDraw driver identifier. 
        lpszError = TEXT("DDERR_INVALIDDIRECTDRAWGUID");
        break;

    case DDERR_INVALIDMODE:
        // DirectDraw does not support the requested mode. 
        lpszError = TEXT("DDERR_INVALIDMODE");
        break;

    case DDERR_INVALIDOBJECT:
        // DirectDraw received a pointer that was an invalid DirectDraw object. 
        lpszError = TEXT("DDERR_INVALIDOBJECT");
        break;

    case DDERR_INVALIDPARAMS:
        // One or more of the parameters passed to the method are incorrect. 
        lpszError = TEXT("DDERR_INVALIDPARAMS");
        break;

    case DDERR_INVALIDPIXELFORMAT:
        // The pixel format was invalid as specified. 
        lpszError = TEXT("DDERR_INVALIDPIXELFORMAT");
        break;

    case DDERR_INVALIDPOSITION:
        // The position of the overlay on the destination is no longer legal. 
        lpszError = TEXT("DDERR_INVALIDPOSITION");
        break;

    case DDERR_INVALIDRECT:
        // The provided rectangle was invalid. 
        lpszError = TEXT("DDERR_INVALIDRECT");
        break;

    case DDERR_INVALIDSURFACETYPE:
        // The requested operation could not be performed
        // because the surface was of the wrong type. 
        lpszError = TEXT("DDERR_INVALIDSURFACETYPE");
        break;

    case DDERR_LOCKEDSURFACES:
        // One or more surfaces are locked, 
        // causing the failure of the requested operation. 
        lpszError = TEXT("DDERR_LOCKEDSURFACES");
        break;

    case DDERR_MOREDATA:
        // There is more data available than the specified 
        // buffer size could hold.
        lpszError = TEXT("DDERR_MOREDATA");
        break;

    case DDERR_NO3D:
        // No 3D hardware or emulation is present. 
        lpszError = TEXT("DDERR_NO3D");
        break;

    case DDERR_NOALPHAHW:
        // No alpha acceleration hardware is present or available, 
        // causing the failure of the requested operation. 
        lpszError = TEXT("DDERR_NOALPHAHW");
        break;

    case DDERR_NOBLTHW:
        // No blitter hardware is present. 
        lpszError = TEXT("DDERR_NOBLTHW");
        break;

    case DDERR_NOCLIPLIST:
        // No clip list is available. 
        lpszError = TEXT("DDERR_NOCLIPLIST");
        break;

    case DDERR_NOCLIPPERATTACHED:
        // No DirectDrawClipper object is attached to the surface object. 
        lpszError = TEXT("DDERR_NOCLIPPERATTACHED");
        break;

    case DDERR_NOCOLORCONVHW:
        // The operation cannot be carried out because 
        // no color-conversion hardware is present or available. 
        lpszError = TEXT("DDERR_NOCOLORCONVHW");
        break;

    case DDERR_NOCOLORKEY:
        // The surface does not currently have a color key. 
        lpszError = TEXT("DDERR_NOCOLORKEY");
        break;

    case DDERR_NOCOLORKEYHW:
        // The operation cannot be carried out because there 
        // is no hardware support for the destination color key. 
        lpszError = TEXT("DDERR_NOCOLORKEYHW");
        break;

    case DDERR_NOCOOPERATIVELEVELSET:
        // A create function is called without the 
        // IDirectDraw2::SetCooperativeLevel method being called. 
        lpszError = TEXT("DDERR_NOCOOPERATIVELEVELSET");
        break;

    case DDERR_NODC:
        // No DC has ever been created for this surface. 
        lpszError = TEXT("DDERR_NODC");
        break;

    case DDERR_NODDROPSHW:
        // No DirectDraw raster operation (ROP) hardware is available. 
        lpszError = TEXT("DDERR_NODDROPSHW");
        break;

    case DDERR_NODIRECTDRAWHW:
        // Hardware-only DirectDraw object creation is not possible; 
        // the driver does not support any hardware. 
        lpszError = TEXT("DDERR_NODIRECTDRAWHW");
        break;

    case DDERR_NODIRECTDRAWSUPPORT:
        // DirectDraw support is not possible with the current display driver. 
        lpszError = TEXT("DDERR_NODIRECTDRAWSUPPORT");
        break;

    case DDERR_NOEMULATION: 
        // Software emulation is not available. 
        lpszError = TEXT("DDERR_NOEMULATION");
        break;

    case DDERR_NOEXCLUSIVEMODE:
        // The operation requires the application to have 
        // exclusive mode, but the application does not have exclusive mode. 
        lpszError = TEXT("DDERR_NOEXCLUSIVEMODE");
        break;

    case DDERR_NOFLIPHW: 
        // Flipping visible surfaces is not supported. 
        lpszError = TEXT("DDERR_NOFLIPHW");
        break;

    case DDERR_NOGDI: 
        // No GDI is present. 
        lpszError = TEXT("DDERR_NOGDI");
        break;

    case DDERR_NOHWND: 
        // Clipper notification requires a window handle, 
        // or no window handle has been previously set 
        // as the cooperative level window handle. 
        lpszError = TEXT("DDERR_NOHWND");
        break;

    case DDERR_NOMIPMAPHW: 
        // The operation cannot be carried out because no 
        // mipmap texture mapping hardware is present or available. 
        lpszError = TEXT("DDERR_NOMIPMAPHW");
        break;

    case DDERR_NOMIRRORHW: 
        // The operation cannot be carried out because 
        // no mirroring hardware is present or available. 
        lpszError = TEXT("DDERR_NOMIRRORHW");
        break;

    case DDERR_NONONLOCALVIDMEM: 
        // An attempt was made to allocate non-local video memory
        // from a device that does not support non-local video memory.
        lpszError = TEXT("DDERR_NONONLOCALVIDMEM");
        break;
 
    case DDERR_NOOVERLAYDEST: 
        // The IDirectDrawSurface2::GetOverlayPosition method 
        // is called on an overlay that the IDirectDrawSurface2::UpdateOverlay 
        // method has not been called on to establish a destination. 
        lpszError = TEXT("DDERR_NOOVERLAYDEST");
        break;

    case DDERR_NOOVERLAYHW: 
        // The operation cannot be carried out because 
        // no overlay hardware is present or available. 
        lpszError = TEXT("DDERR_NOOVERLAYHW");
        break;

    case DDERR_NOPALETTEATTACHED: 
        // No palette object is attached to this surface. 
        lpszError = TEXT("DDERR_NOPALETTEATTACHED");
        break;

    case DDERR_NOPALETTEHW: 
        // There is no hardware support for 16- or 256-color palettes. 
        lpszError = TEXT("DDERR_NOPALETTEHW");
        break;

    case DDERR_NORASTEROPHW: 
        // The operation cannot be carried out because 
        // no appropriate raster operation hardware is present or available. 
        lpszError = TEXT("DDERR_NORASTEROPHW");
        break;

    case DDERR_NOROTATIONHW: 
        // The operation cannot be carried out because 
        // no rotation hardware is present or available. 
        lpszError = TEXT("DDERR_NOROTATIONHW");
        break;

    case DDERR_NOSTRETCHHW: 
        // The operation cannot be carried out because 
        // there is no hardware support for stretching. 
        lpszError = TEXT("DDERR_NOSTRETCHHW");
        break;

    case DDERR_NOT4BITCOLOR: 
        // The DirectDrawSurface object is not using a 
        // 4-bit color palette and the requested operation 
        // requires a 4-bit color palette. 
        lpszError = TEXT("DDERR_NOT4BITCOLOR");
        break;

    case DDERR_NOT4BITCOLORINDEX: 
        // The DirectDrawSurface object is not using a 4-bit 
        // color index palette and the requested operation 
        // requires a 4-bit color index palette. 
        lpszError = TEXT("DDERR_NOT4BITCOLORINDEX");
        break;

    case DDERR_NOT8BITCOLOR: 
        // The DirectDrawSurface object is not using an 8-bit 
        // color palette and the requested operation requires 
        // an 8-bit color palette. 
        lpszError = TEXT("DDERR_NOT8BITCOLOR");
        break;

    case DDERR_NOTAOVERLAYSURFACE: 
        // An overlay component is called for a non-overlay surface. 
        lpszError = TEXT("DDERR_NOTAOVERLAYSURFACE");
        break;

    case DDERR_NOTEXTUREHW: 
        // The operation cannot be carried out because no 
        // texture-mapping hardware is present or available. 
        lpszError = TEXT("DDERR_NOTEXTUREHW");
        break;

    case DDERR_NOTFLIPPABLE: 
        // An attempt has been made to flip a surface that cannot be flipped. 
        lpszError = TEXT("DDERR_NOTFLIPPABLE");
        break;

    case DDERR_NOTFOUND: 
        // The requested item was not found. 
        lpszError = TEXT("DDERR_NOTFOUND");
        break;

    case DDERR_NOTINITIALIZED: 
        // An attempt was made to call an interface method of a DirectDraw object 
        // created by CoCreateInstance before the object was initialized. 
        lpszError = TEXT("DDERR_NOTINITIALIZED");
        break;

    case DDERR_NOTLOCKED: 
        // An attempt is made to unlock a surface that was not locked. 
        lpszError = TEXT("DDERR_NOTLOCKED");
        break;

    case DDERR_NOTPAGELOCKED: 
        // An attempt is made to page unlock a surface 
        // with no outstanding page locks. 
        lpszError = TEXT("DDERR_NOTPAGELOCKED");
        break;

    case DDERR_NOTPALETTIZED: 
        // The surface being used is not a palette-based surface. 
        lpszError = TEXT("DDERR_NOTPALETTIZED");
        break;

    case DDERR_NOVSYNCHW: 
        // The operation cannot be carried out because 
        // there is no hardware support for vertical blank synchronized operations. 
        lpszError = TEXT("DDERR_NOVSYNCHW");
        break;

    case DDERR_NOZBUFFERHW: 
        // The operation to create a z-buffer in display memory 
        // or to perform a blit using a z-buffer cannot be carried 
        // out because there is no hardware support for z-buffers. 
        lpszError = TEXT("DDERR_NOZBUFFERHW");
        break;

    case DDERR_NOZOVERLAYHW: 
        // The overlay surfaces cannot be z-layered based 
        // on the z-order because the hardware does not 
        // support z-ordering of overlays. 
        lpszError = TEXT("DDERR_NOZOVERLAYHW");
        break;

    case DDERR_OUTOFCAPS: 
        // The hardware needed for the requested operation has already been allocated. 
        lpszError = TEXT("DDERR_OUTOFCAPS");
        break;

    case DDERR_OUTOFMEMORY: 
        // DirectDraw does not have enough memory to perform the operation. 
        lpszError = TEXT("DDERR_OUTOFMEMORY");
        break;

    case DDERR_OUTOFVIDEOMEMORY: 
        // DirectDraw does not have enough display memory to perform the operation. 
        lpszError = TEXT("DDERR_OUTOFVIDEOMEMORY");
        break;

    case DDERR_OVERLAYCANTCLIP: 
        // The hardware does not support clipped overlays. 
        lpszError = TEXT("DDERR_OVERLAYCANTCLIP");
        break;

    case DDERR_OVERLAYCOLORKEYONLYONEACTIVE: 
        // An attempt was made to have more than one color key active on an overlay. 
        lpszError = TEXT("DDERR_OVERLAYCOLORKEYONLYONEACTIVE");
        break;

    case DDERR_OVERLAYNOTVISIBLE: 
        // The IDirectDrawSurface2::GetOverlayPosition method is called on a hidden overlay. 
        lpszError = TEXT("DDERR_OVERLAYNOTVISIBLE");
        break;

    case DDERR_PALETTEBUSY: 
        // Access to this palette is refused 
        // because the palette is locked by another thread. 
        lpszError = TEXT("DDERR_PALETTEBUSY");
        break;

    case DDERR_PRIMARYSURFACEALREADYEXISTS: 
        // This process has already created a primary surface. 
        lpszError = TEXT("DDERR_PRIMARYSURFACEALREADYEXISTS");
        break;

    case DDERR_REGIONTOOSMALL: 
        // The region passed to the 
        // IDirectDrawClipper::GetClipList method is too small. 
        lpszError = TEXT("DDERR_REGIONTOOSMALL");
        break;

    case DDERR_SURFACEALREADYATTACHED: 
        // An attempt was made to attach a surface to 
        // another surface to which it is already attached. 
        lpszError = TEXT("DDERR_SURFACEALREADYATTACHED");
        break;

    case DDERR_SURFACEALREADYDEPENDENT: 
        // An attempt was made to make a surface a dependency 
        // of another surface to which it is already dependent. 
        lpszError = TEXT("DDERR_SURFACEALREADYDEPENDENT");
        break;

    case DDERR_SURFACEBUSY: 
        // Access to the surface is refused because the 
        // surface is locked by another thread. 
        lpszError = TEXT("DDERR_SURFACEBUSY");
        break;

    case DDERR_SURFACEISOBSCURED: 
        // Access to the surface is refused 
        // because the surface is obscured. 
        lpszError = TEXT("DDERR_SURFACEISOBSCURED");
        break;

    case DDERR_SURFACELOST: 
        // Access to the surface is refused because the 
        // surface memory is gone. The DirectDrawSurface 
        // object representing this surface should have 
        // the IDirectDrawSurface2::Restore method called on it. 
        lpszError = TEXT("DDERR_SURFACELOST");
        break;

    case DDERR_SURFACENOTATTACHED: 
        // The requested surface is not attached. 
        lpszError = TEXT("DDERR_SURFACENOTATTACHED");
        break;

    case DDERR_TOOBIGHEIGHT: 
        // The height requested by DirectDraw is too large. 
        lpszError = TEXT("DDERR_TOOBIGHEIGHT");
        break;

    case DDERR_TOOBIGSIZE: 
        // The size requested by DirectDraw is too large. 
        // However, the individual height and width are OK.
        lpszError = TEXT("DDERR_TOOBIGSIZE");
        break;

    case DDERR_TOOBIGWIDTH: 
        // The width requested by DirectDraw is too large. 
        lpszError = TEXT("DDERR_TOOBIGWIDTH");
        break;

    case DDERR_UNSUPPORTED: 
        // The operation is not supported. 
        lpszError = TEXT("DDERR_UNSUPPORTED");
        break;

    case DDERR_UNSUPPORTEDFORMAT: 
        // The FourCC format requested is not supported by DirectDraw. 
        lpszError = TEXT("DDERR_UNSUPPORTEDFORMAT");
        break;

    case DDERR_UNSUPPORTEDMASK: 
        // The bitmask in the pixel format requested is not supported by DirectDraw. 
        lpszError = TEXT("DDERR_UNSUPPORTEDMASK");
        break;

    case DDERR_UNSUPPORTEDMODE: 
        // The display is currently in an unsupported mode. 
        lpszError = TEXT("DDERR_UNSUPPORTEDMODE");
        break;

    case DDERR_VERTICALBLANKINPROGRESS: 
        // A vertical blank is in progress. 
        lpszError = TEXT("DDERR_VERTICALBLANKINPROGRESS");
        break;

    case DDERR_WASSTILLDRAWING: 
        // The previous blit operation that is transferring 
        // information to or from this surface is incomplete. 
        lpszError = TEXT("DDERR_WASSTILLDRAWING");
        break;

    case DDERR_WRONGMODE:
        // This surface cannot be restored because it was created in a different mode. 
        lpszError = TEXT("DDERR_WRONGMODE");
        break;

    case DDERR_XALIGN:
        // The provided rectangle was not horizontally aligned on a required boundary. 
        lpszError = TEXT("DDERR_XALIGN");
        break;

    case DDERR_VIDEONOTACTIVE:
        // The video port is not active
        lpszError = TEXT("DDERR_VIDEONOTACTIVE");
        break;


        //
        // D3D Immediate Mode Errors
        //

    case D3DERR_BADMAJORVERSION:
        // ???
        lpszError = TEXT("D3DERR_BADMAJORVERSION");
        break;

    case D3DERR_BADMINORVERSION:
        // ???
        lpszError = TEXT("D3DERR_BADMINORVERSION");
        break;

    case D3DERR_INVALID_DEVICE:
        // ???
        lpszError = TEXT("D3DERR_INVALID_DEVICE");
        break;

    case D3DERR_INITFAILED:
        // ???
        lpszError = TEXT("D3DERR_INITFAILED");
        break;

    case D3DERR_EXECUTE_CREATE_FAILED:
        // ???
        lpszError = TEXT("D3DERR_EXECUTE_CREATE_FAILED");
        break;

    case D3DERR_EXECUTE_DESTROY_FAILED:
        // ???
        lpszError = TEXT("D3DERR_EXECUTE_DESTROY_FAILED");
        break;

    case D3DERR_EXECUTE_LOCK_FAILED:
        // ???
        lpszError = TEXT("D3DERR_EXECUTE_LOCK_FAILED");
        break;

    case D3DERR_EXECUTE_UNLOCK_FAILED:
        // ???
        lpszError = TEXT("D3DERR_EXECUTE_UNLOCK_FAILED");
        break;

    case D3DERR_EXECUTE_LOCKED:
        // ???
        lpszError = TEXT("D3DERR_EXECUTE_LOCKED");
        break;

    case D3DERR_EXECUTE_NOT_LOCKED:
        // ???
        lpszError = TEXT("D3DERR_EXECUTE_NOT_LOCKED");
        break;

    case D3DERR_EXECUTE_FAILED:
        // ???
        lpszError = TEXT("D3DERR_EXECUTE_FAILED");
        break;

    case D3DERR_EXECUTE_CLIPPED_FAILED:
        // ???
        lpszError = TEXT("D3DERR_CLIPPED_FAILED");
        break;

    case D3DERR_TEXTURE_NO_SUPPORT:
        // ???
        lpszError = TEXT("D3DERR_TEXTURE_NO_SUPPORT");
        break;

    case D3DERR_TEXTURE_CREATE_FAILED:
        // ???
        lpszError = TEXT("D3DERR_TEXTURE_CREATE_FAILED");
        break;

    case D3DERR_TEXTURE_DESTROY_FAILED:
        // ???
        lpszError = TEXT("D3DERR_TEXTURE_DESTROY_FAILED");
        break;

    case D3DERR_TEXTURE_LOCK_FAILED:
        // ???
        lpszError = TEXT("D3DERR_TEXTURE_LOCK_FAILED");
        break;

    case D3DERR_TEXTURE_UNLOCK_FAILED:
        // ???
        lpszError = TEXT("D3DERR_TEXTURE_UNLOCK_FAILED");
        break;

    case D3DERR_TEXTURE_LOAD_FAILED:
        // ???
        lpszError = TEXT("D3DERR_LOAD_FAILED");
        break;

    case D3DERR_TEXTURE_SWAP_FAILED:
        // ???
        lpszError = TEXT("D3DERR_SWAP_FAILED");
        break;

    case D3DERR_TEXTURE_LOCKED:
        // ???
        lpszError = TEXT("D3DERR_TEXTURE_LOCKED");
        break;

    case D3DERR_TEXTURE_NOT_LOCKED:
        // ???
        lpszError = TEXT("D3DERR_TEXTURE_NOT_LOCKED");
        break;

    case D3DERR_TEXTURE_GETSURF_FAILED:
        // ???
        lpszError = TEXT("D3DERR_TEXTURE_GETSURF_FAILED");
        break;

    case D3DERR_MATRIX_CREATE_FAILED:
        // ???
        lpszError = TEXT("D3DERR_MATRIX_CREATE_FAILED");
        break;

    case D3DERR_MATRIX_DESTROY_FAILED:
        // ???
        lpszError = TEXT("D3DERR_MATRIX_DESTROY_FAILED");
        break;

    case D3DERR_MATRIX_SETDATA_FAILED:
        // ???
        lpszError = TEXT("D3DERR_MATRIX_SETDATA_FAILED");
        break;

    case D3DERR_MATRIX_GETDATA_FAILED:
        // ???
        lpszError = TEXT("D3DERR_MAXTRIX_GETDATA_FAILED");
        break;

    case D3DERR_SETVIEWPORTDATA_FAILED:
        // ???
        lpszError = TEXT("D3DERR_SETVIEWPORTDATA_FAILED");
        break;

    case D3DERR_INVALIDCURRENTVIEWPORT:
        // ???
        lpszError = TEXT("D3DERR_INVALIDCURRENTVIEWPORT");
        break;
	
    case D3DERR_INVALIDPRIMITIVETYPE:
        // ???
        lpszError = TEXT("D3DERR_INVALIDPRIMITIVETYPE");
        break;

    case D3DERR_INVALIDVERTEXTYPE:
        // ???
        lpszError = TEXT("D3DERR_INVALIDVERTEXTYPE");
        break;

    case D3DERR_TEXTURE_BADSIZE:
        // ???
        lpszError = TEXT("D3DERR_TEXTURE_BADSIZE");
        break;

    case D3DERR_MATERIAL_CREATE_FAILED:
        // ???
        lpszError = TEXT("D3DERR_MATERIAL_CREATE_FAILED");
        break;

    case D3DERR_MATERIAL_DESTROY_FAILED:
        // ???
        lpszError = TEXT("D3DERR_MATERIAL_DESTROY_FAILED");
        break;

    case D3DERR_MATERIAL_SETDATA_FAILED:
        // ???
        lpszError = TEXT("D3DERR_MATERIAL_SETDATA_FAILED");
        break;

    case D3DERR_MATERIAL_GETDATA_FAILED:
        // ???
        lpszError = TEXT("D3DERR_MATERIAL_GETDATA_FAILED");
        break;

    case D3DERR_INVALIDPALETTE:
        // ???
        lpszError = TEXT("D3DERR_INVALIDPALETTE");
        break;

    case D3DERR_ZBUFF_NEEDS_SYSTEMMEMORY:
        // ???
        lpszError = TEXT("D3DERR_ZBUFF_NEEDS_SYSTEMMEMORY");
        break;

    case D3DERR_ZBUFF_NEEDS_VIDEOMEMORY:
        // ???
        lpszError = TEXT("D3DERR_ZBUFF_NEEDS_VIDEOMEMORY");
        break;

    case D3DERR_SURFACENOTINVIDMEM:
        // ???
        lpszError = TEXT("D3DERR_SURFACENOTINVIDMEM");
        break;

    case D3DERR_LIGHT_SET_FAILED:
        // ???
        lpszError = TEXT("D3DERR_LIGHT_SET_FAILED");
        break;

    case D3DERR_LIGHTHASVIEWPORT:
        // ???
        lpszError = TEXT("D3DERR_LIGHTHASVIEWPORT");
        break;

    case D3DERR_LIGHTNOTINTHISVIEWPORT:
        // ???
        lpszError = TEXT("D3DERR_LIGHTNOTINTHISVIEWPORT");
        break;

    case D3DERR_SCENE_IN_SCENE:
        // ???
        lpszError = TEXT("D3DERR_SCENE_IN_SCENE");
        break;

    case D3DERR_SCENE_NOT_IN_SCENE:
        // ???
        lpszError = TEXT("D3DERR_SCENE_NOT_IN_SCENE");
        break;

    case D3DERR_SCENE_BEGIN_FAILED:
        // ???
        lpszError = TEXT("D3DERR_SCENE_BEGIN_FAILED");
        break;

    case D3DERR_SCENE_END_FAILED:
        // ???
        lpszError = TEXT("D3DERR_SCENE_END_FAILED");
        break;

    case D3DERR_INBEGIN:
        // ???
        lpszError = TEXT("D3DERR_INBEGIN");
        break;

    case D3DERR_NOTINBEGIN:
        // ???
        lpszError = TEXT("D3DERR_NOTINBEGIN");
        break;

    case D3DERR_NOVIEWPORTS:
        // ???
        lpszError = TEXT("D3DERR_NOVIEWPORTS");
        break;

    case D3DERR_VIEWPORTDATANOTSET:
        // ???
        lpszError = TEXT("D3DERR_VIEWPORTDATANOTSET");
        break;

    case D3DERR_VIEWPORTHASNODEVICE:
        // ???
        lpszError = TEXT("D3DERR_VIEWPORTHASNODEVICE");
        break;

    
        //
        // D3D Retained Mode Errors
        //
#if 0    
    case D3DRMERR_BADOBJECT:
        // ???
        lpszError = TEXT("D3DRMERR_BADOBJECT");
        break;

    case D3DRMERR_BADTYPE:
        // ???
        lpszError = TEXT("D3DRMERR_BADTYPE");
        break;

    case D3DRMERR_BADALLOC:
        // ???
        lpszError = TEXT("D3DRMERR_BADALLOC");
        break;

    case D3DRMERR_FACEUSED:
        // ???
        lpszError = TEXT("D3DRMERR_FACEUSED");
        break;

    case D3DRMERR_NOTFOUND:
        // ???
        lpszError = TEXT("D3DRMERR_NOTFOUND");
        break;

    case D3DRMERR_NOTDONEYET:
        // ???
        lpszError = TEXT("D3DRMERR_NOTDONEYET");
        break;

    case D3DRMERR_FILENOTFOUND:
        // ???
        lpszError = TEXT("D3DRMERR_FILENOTFOUND");
        break;

    case D3DRMERR_BADFILE:
        // ???
        lpszError = TEXT("D3DRMERR_BADFILE");
        break;

    case D3DRMERR_BADDEVICE:
        // ???
        lpszError = TEXT("D3DRMERR_BADDEVICE");
        break;

    case D3DRMERR_BADVALUE:
        // ???
        lpszError = TEXT("D3DRMERR_BADVALUE");
        break;

    case D3DRMERR_BADMAJORVERSION:
        // ???
        lpszError = TEXT("D3DRMERR_BADMAJORVERSION");
        break;

    case D3DRMERR_BADMINORVERSION:
        // ???
        lpszError = TEXT("D3DRMERR_BADMINORVERSION");
        break;

    case D3DRMERR_UNABLETOEXECUTE:
        // ???
        lpszError = TEXT("D3DRMERR_BADMINORVERSION");
        break;
#endif // D3DRM_ERRORS


		//
		// Application defined errors
		//
	case APPERR_GENERIC:
        // Generic Error
        lpszError = TEXT("APPERR_GENERIC");
        break;

	case APPERR_INVALIDPARAMS:
        // Invalid Parameters passed into function
        lpszError = TEXT("APPERR_INVALIDPARAMS");
        break;

	case APPERR_NOTINITIALIZED:
		// Programmer error, called function without proper initialization
		lpszError = TEXT("APPERR_NOT_INITIALIZED");
		break;

    default:
        // Unknown DD/D3D/App Error
        wsprintf (szMsg, "Unknown Error #%ld", (DWORD)(hResult & 0x0000FFFFL));
        lpszError = szMsg;
        break;
    }

    // Copy DD/D3D Error string to buff
    cLen = strlen (lpszError);
    if (cLen >= cchError)
    {
        cLen = cchError - 1;
    }

    if (cLen)
    {
        strncpy (lpszErrorBuff, lpszError, cLen);
        lpszErrorBuff[cLen] = 0;
    }

    return TRUE;
} // End GetDDErrorString




/*
**-----------------------------------------------------------------------------
** End of File
**-----------------------------------------------------------------------------
*/


