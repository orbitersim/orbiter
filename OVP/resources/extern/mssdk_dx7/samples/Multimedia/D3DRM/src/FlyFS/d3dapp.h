/*
 *  Copyright (C) 1998 Microsoft Corporation. All Rights Reserved.
 *
 *  File: d3dapp.h
 *
 *  Header to be included in source using D3DApp.  Contains D3DApp function
 *  prototypes and defines.
 *
 *  D3DApp is a collection of helper functions for Direct3D applications.
 *  D3DApp consists of the following files:
 *      d3dapp.h    Main D3DApp header to be included by application
 *      d3dappi.h   Internal header
 *      d3dapp.c    D3DApp functions seen by application.
 *      ddcalls.c   All calls to DirectDraw objects except textures
 *      d3dcalls.c  All calls to Direct3D objects except textures
 *      texture.c   Texture loading and managing texture list
 *      misc.c      Miscellaneous calls
 */

#ifndef __D3DAPP_H__
#define __D3DAPP_H__

#ifdef __cplusplus
extern "C" {
#endif

/*
 * DEFINES
 */
#define D3DAPP_WINDOWMINIMUM 50     /* smallest buffer size allowed */
#define D3DAPP_DEFAULTWINDOWDIM 320 /* replaces window size if invalid */
#define D3DAPP_MINBUFFERSIZE 15360  /* minimum "maximum buffer size" for a
                                       D3D driver to be accepted */
#define D3DAPP_MINVERTEXCOUNT 320   /* minimum "maximum vertex count" for a
                                       D3D driver to be accepted */
#define D3DAPP_MAXD3DDRIVERS 5      /* maximum Direct3D drivers ever expected
                                       to find */
#define D3DAPP_MAXDDDRIVERS 3       /* maximum DirectDraw drivers ever expected
                                       to find */
#define D3DAPP_MAXTEXTUREFORMATS 50 /* maximum texture formats */
#define D3DAPP_MAXMODES 50          /* maximum display modes ever expected to
                                       be reported by DirectDraw */
#define D3DAPP_MAXTEXTURES 15       /* maximum number of textures that wil be
                                       loaded and managed */
#define D3DAPP_MAXCLEARRECTS 30     /* maximum num. rectangles (ie extents)
                                       for clearing */
#define D3DAPP_BOGUS -100           /* unused parameters accept this */
#define D3DAPP_YOUDECIDE -25        /* Use this for certain parameters to
                                       have D3DApp decide an appropriate
                                       value for you */
#define D3DAPP_USEWINDOW -24        /* Used in place of fullscreen mode */

/*
 * DATA STRUCTURES
 */

/*
 * D3DAppDDDriver structure
 * Describes a DD driver
 */
typedef struct tagD3DAppDDDriver {
    char    Name[30];   /* short name of the driver */
    DDCAPS  HWCaps;     /* Hardware capabilities */
    GUID    Guid;       /* it's GUID */
    BOOL    bIsPrimary; /* is this the primary device */
} D3DAppDDDriver;

/*
 * D3DAppD3DDriver structure
 * Describes a D3D driver
 */
typedef struct tagD3DAppD3DDriver {
    char Name[30];      /* short name of the driver */
    char About[50];     /* short string about the driver */
    D3DDEVICEDESC Desc; /* D3DDEVICEDESC for complete information */
    GUID Guid;          /* it's GUID */
    BOOL bIsHardware;   /* does this driver represent a hardware device? */
    BOOL bDoesTextures; /* does this driver do texture mapping? */
    BOOL bDoesZBuffer;  /* can this driver use a z-buffer? */
    BOOL bCanDoWindow;  /* can it render to Window's display depth? */
} D3DAppD3DDriver;

/*
 * D3DAppTextureFormat stucture
 * Describes a texture format
 */
typedef struct tagD3DAppTextureFormat {
    DDSURFACEDESC ddsd; /* DDSURFACEDESC for complete information */
    BOOL bPalettized;   /* is this format palettized */
    int RedBPP;         /* number of red, */
    int BlueBPP;        /* blue, */
    int GreenBPP;       /* and green bits per pixel */
    int IndexBPP;       /* number of bits in palette index */
} D3DAppTextureFormat;

/*
 * D3DAppMode structure
 * Describes a display mode
 */
typedef struct tagD3DAppMode {
    int     w;                /* width */
    int     h;                /* height */
    int     bpp;              /* bits per pixel */
    BOOL    bThisDriverCanDo; /* can current D3D driver render in this mode?*/
} D3DAppMode;

/*
 * D3DAppInfo structure
 * Contains all the information D3DApp makes available to the application. A
 * pointer to the internal, read only copy is returned by the initializing
 * function.
 */
typedef struct tagD3DAppInfo {
    HWND                    hwnd;          /*handle of window being managed*/
    /*
     * Direct3D objects and information
     */
    LPDIRECT3D2             lpD3D;         /* D3D object */
    LPDIRECT3DDEVICE2       lpD3DDevice;   /* D3D device */
    LPDIRECT3DVIEWPORT      lpD3DViewport; /* D3D viewport, created by
                                              application */
    int                     NumDrivers;    /* number of D3D drivers avail. */
    int                     CurrDriver;    /* number of curr. D3D driver */
    D3DAppD3DDriver         Driver[D3DAPP_MAXD3DDRIVERS]; /* avail. drivers*/
    D3DAppD3DDriver         ThisDriver;    /* description of this driver,
                                           identical to Driver[CurrDriver] */

    int                     NumTextureFormats; /* num texture formats avail*/
    int                     CurrTextureFormat; /* current texture format
                  will only change if driver changes or when app changes it*/
    D3DAppTextureFormat     TextureFormat[D3DAPP_MAXTEXTUREFORMATS];
                                      /* description of all avail. formats */
    D3DAppTextureFormat     ThisTextureFormat; /* description of this format,
                             identical to TextureFormat[CurrTextureFormat] */

    int                     NumTextures;    /* number of textures in D3DApp's
                                               texture list */
    char                    ImageFile[D3DAPP_MAXTEXTURES][50]; /* files */
    D3DTEXTUREHANDLE        TextureHandle[D3DAPP_MAXTEXTURES]; /* handles */
    LPDIRECTDRAWSURFACE     lpTextureSurf[D3DAPP_MAXTEXTURES]; /* surfaces */
    LPDIRECT3DTEXTURE2      lpTexture[D3DAPP_MAXTEXTURES]; /* texture objs */
    int                     NumUsableTextures; /* the number of currently usable
                                                  textures (e.g. for a hardware
                                                  device there may not be enough
                                                  video memory*/
    /*
     * DirectDraw objects and information
     */
    LPDIRECTDRAW            lpDD;          /* DirectDraw object */
    BOOL                    bIsPrimary;    /* Is this the primary DD device?
               If FALSE, we're using a hardware DD device that cannot
               display a window and so only fullscreen modes are available */
    LPDIRECTDRAWSURFACE     lpFrontBuffer; /* front buffer surface */
    LPDIRECTDRAWSURFACE     lpBackBuffer;  /* back buffer surface */
    LPDIRECTDRAWSURFACE     lpZBuffer;     /* z-buffer surface */
    BOOL                    bBackBufferInVideo; /* back buf in video mem? */
    BOOL                    bZBufferInVideo;    /* is Z-buf in video mem? */

    int                     NumModes; /* number of available display modes */
    int                     CurrMode; /* number of current display mode (only
                                         when fullscreen) */
    D3DAppMode              Mode[D3DAPP_MAXMODES]; /* desc avail modes */
    D3DAppMode              ThisMode; /* description of this mode, identical
                                         to Mode[CurrMode] */
    BOOL                    bFullscreen; /* in fullscreen exclusive mode? */
    D3DAppMode              WindowsDisplay; /* current Windows disply mode */

    SIZE                    szClient;         /* dimensions of client win */
    POINT                   pClientOnPrimary; /* position of client win */

    BOOL                    bPaused;           /* the app is paused */
    BOOL                    bAppActive;        /* the app is active */
    BOOL                    bTexturesDisabled; /* textures are disabled */
    BOOL                    bOnlySystemMemory; /* all surfaces forced into
                                                  system memory */
    BOOL                    bOnlyEmulation;    /* no hardware DD or D3D
                                                  devices allowed */
    BOOL                    bMinimized;        /* app window is minimized */
    BOOL                    bRenderingIsOK;    /* All objects etc. necessary
                                                  for rendering are in ok */
} D3DAppInfo;

/*
 * D3DAppRenderState structure
 * The "render state" is the status of this collection of D3D options and
 * variables.  This structure is used to get and set the render state.  The
 * render state will only change during program initialization and when
 * the application sets it.
 */
typedef struct tagD3DAppRenderState {
    BOOL             bZBufferOn;    /* Z buffer is on */
    BOOL             bPerspCorrect; /* perspective correction is on */
    D3DSHADEMODE     ShadeMode;     /* flat, gouraud, phong? */
    D3DTEXTUREFILTER TextureFilter; /* linear or bi-linear texture filter */
    D3DTEXTUREBLEND  TextureBlend;  /* Use shade mode or copy mode? */
    D3DFILLMODE      FillMode;      /* solid, lines or points? */
    BOOL             bDithering;    /* dithering is on */
    BOOL             bSpecular;     /* specular highlights are on */
    BOOL             bAntialiasing; /* anti-aliasing is on */

    BOOL             bFogEnabled;   /* fog is on */
    D3DCOLOR         FogColor;      /* fog color */
    D3DFOGMODE       FogMode;       /* linear, exp. etc. */
    D3DVALUE         FogStart;      /* begining depth */
    D3DVALUE         FogEnd;        /* ending depth */
} D3DAppRenderState;

/*
 * FUNCTION PROTOTYPES
 */

/*
 * D3DAppEnumerateDDDevices
 *
 * All the 3d capable DirectDraw devices including the primary display are
 * found.  The driver information structures are filled in with appropriate
 * information.  lpDriver must point to an array with at least D3DAPP_MAXDDDRIVERS
 * elements.
 */
BOOL D3DAppEnumerateDDDevices(int* NumDevices, D3DAppDDDriver* lpDriver);

/*
 * D3DAppCreateFromHWND
 *
 * Call this before all other D3DApp functions (except AddTexture and DD enumeration).
 * Initializes all DD and D3D objects necessary for rendering, enumerates the
 * available display modes and drivers and loads textures specified by prior
 * AddTexture() calls.  Caller passes the handle of the window to be manged
 * and callback functions to execute for device creation and destruction.
 *
 * DeviceCreateCallback is executed AFTER the creation of D3D device and all
 * objects D3DApp created using the device.  This allows an application to
 * reconstruct the scene and create any additional objects.  The callback
 * must create and return (in the variable provided) the DIRECT3DVIEWPORT
 * from the given width and height.  The returned pointer is stored in the
 * D3DAppInfo structure and used for clearing and setting the render state.
 * A NULL pointer is fine if D3DApp is not used for either of these
 * functions. The create callback will always be called before any calls to
 * the destroy callback.  The boolean returned indicates success or failure.
 *
 * DeviceDestroyCallback is executed BEFORE the D3D device and objects
 * created by D3DApp using the device are released.  This allows an
 * application to save data regarding the scene or release any objects
 * created from the device before it is destroyed.  The DIRECT3DVIEWPORT
 * should be released in this callback.  The boolean returned indicates the
 * success or failure.
 *
 * A pointer to the internal D3DAppInfo data structure is returned.  This
 * should be READ ONLY!
 *
 * The Direct3D driver, display mode and texture format
 * will all be chosen by D3DApp.  Hardware DD and D3D devices are prefered.
 * Mono lighting D3D drivers are prefered.  Paletted texture formats are
 * prefered.  If possible, the current window size will be used, otherwise
 * a fullscreen mode will be selected.
 *
 * Call AddTexture() to add textures to be loaded upon initialization.
 *
 * Valid flags:
 *    D3DAPP_ONLYSYSTEMMEMORY  Force all surfaces into system memory.  Also
 *                             disables hardware DD and D3D drivers.
 *    D3DAPP_ONLYD3DEMULATION  Disable D3D hardware
 *    D3DAPP_ONLYDDEMULATION   Disable DD hardware
 */
#define D3DAPP_ONLYSYSTEMMEMORY 0x00000001
#define D3DAPP_ONLYD3DEMULATION 0x00000002
#define D3DAPP_ONLYDDEMULATION  0x00000004
BOOL D3DAppCreateFromHWND(DWORD flags, HWND hwnd, LPGUID lpDDGuid,
                          BOOL(*DeviceCreateCallback)(int, int,
                                                      LPDIRECT3DVIEWPORT*,
                                                      LPVOID),
                          LPVOID lpCreateContext,
                          BOOL(*DeviceDestroyCallback)(LPVOID),
                          LPVOID lpDestroyContext,
                          D3DAppInfo** D3DApp);

/*
 * D3DAppWindowProc
 * To be truly effective, D3DApp should be allowed to trap incoming window
 * messages such as WM_SIZE.  Call D3DAppWindowProc at the begining of the
 * application's main window WindowProc with the message information.  If
 * bStopProcessing is set to TRUE, stop processing the message and return
 * lresult.
 */
BOOL D3DAppWindowProc(BOOL* bStopProcessing, LRESULT* lresult, HWND hwnd,
                      UINT message, WPARAM wParam, LPARAM lParam);

/*
 * D3DAppFullscreen
 * Places the app in a fullscreen mode using the current driver.
 */
BOOL D3DAppFullscreen(int mode);

/*
 * D3DAppWindow
 * Places the application in windowed mode at the given client size.  If w
 * and h are D3DAPP_YOUDECIDE, D3DApp will decide on a suitable client size.
 * If called while in fullscreen, restores the display mode and returns the
 * hooked window to the size it was before a call to D3DAppFullscreen or to
 * the size specified.
 */
BOOL D3DAppWindow(int w, int h);

/*
 * D3DAppChangeDriver
 * Changes the driver.  If the current display mode is incompatible with the
 * driver, a new one will be selected and employed.  A new texture format is
 * selected and textures are reloaded, hence their handles may change.  By
 * default, paletted formats are prefered.
 */
BOOL D3DAppChangeDriver(int driver, DWORD flags);

/*
 * D3DAppAddTexture
 * D3DApp has an internal list of textures which it maintains.  The image
 * files will be reloaded when necessary and the texture handles, objects and
 * surfaces will always be up to date and available in the D3DAppInfo
 * structure.  D3DAppAddTextures adds the given PPM image file to this
 * list.
 *
 * This is the only function which can be called before CreateD3DAppFromHWND.
 * Use it to create a list of textures to load during this creation function.
 *
 * The handles and texture objects will change when the device or texture
 * format changes.  To react to changes in the texture objects and surfaces,
 * use the callback for D3D device creation/release and make necessary
 * changes whenever calling D3DAppChangeTextureFormat.
 *
 * Image files are searched for in the current directory, D3DPATH env var,
 * and the "Software\Microsoft\Direct3D\4.0\D3D Path" registry entry.
 *
 */
BOOL D3DAppAddTexture(const char* imagefile);

/*
 * D3DAppChangeTextureFormat
 * Changes all textures to the given format.  Texture handles and objects
 * will change.
 */
BOOL D3DAppChangeTextureFormat(int format);

/*
 * D3DAppDisableTextures
 * Disables the textures by turning handles to NULL.  If the driver changes,
 * textures are not loaded until this state is toggled by another call with a
 * TRUE flag.
 */
BOOL D3DAppDisableTextures(BOOL flag);

/*
 * D3DAppSwapTextures
 * Swaps first texture with the second, second with third, etc. while keeping
 * the handles array the same.
 */
BOOL D3DAppSwapTextures(void);

/*
 * D3DAppSetRenderState
 * Uses a D3D execute buffer to set the render state.  If lpState is NULL,
 * the current settings are reset.
 */
BOOL D3DAppSetRenderState(D3DAppRenderState* lpState);

/*
 * D3DAppGetRenderState
 * Returns the current render state.
 */
BOOL D3DAppGetRenderState(D3DAppRenderState* lpState);

/*
 * D3DAppShowBackBuffer
 * Blts or flips the back buffer to the primary surface.  In the windowed
 * case, only the dirty portion of the front buffer is blt'ed over.  The
 * dirty region of front and back buffers is maintained by calls to
 * D3DAppRenderExtents(). D3DAPP_SHOWALL will force the entire front buffer
 * to be updated.
 */
#define D3DAPP_SHOWALL 0x00000001
BOOL D3DAppShowBackBuffer(DWORD flags);

/*
 * D3DAppRenderExtents
 * Tells D3DApp the extents of all regions updated on the back buffer as a
 * list of D3DRECTs (no more than D3DAPP_MAXCLEARRECTS).  Call this before
 * clearing the back buffer.  If the D3DAPP_CLEARALL flag is set, the extents
 * are ignored and the entire back buffer is assumed to have changed.
 */
#define D3DAPP_CLEARALL 0x00000001
BOOL D3DAppRenderExtents(DWORD dwCount, LPD3DRECT extent, DWORD flags);

/*
 * D3DAppClearBackBuffer
 * Clears the back buffer and Z-buffer (if enabled).  D3DAPP_CLEARALL can be
 * used to clear the entire back buffer.
 */
#define D3DAPP_CLEARALL 0x00000001
BOOL D3DAppClearBackBuffer(DWORD flags);

/*
 * D3DAppCheckForLostSurfaces
 * Checks all surfaces D3DApp has allocated and restores them if necessary.
 * An error is returned on any type of failure, but it may be best to ignore
 * it since restoring surface can fail for non-fatal reasons and the app may
 * just want to spin.
 */
BOOL D3DAppCheckForLostSurfaces(void);

/*
 * D3DAppPause
 * Use D3DAppPause(TRUE) to pause the app and D3DAppPause(FALSE) to unpause.
 * When fullscreen, the menu bar is redrawn.  bPaused is updated to reflect
 * the current status.
 */
BOOL D3DAppPause(BOOL flag);

/*
 * D3DAppErrorToString
 * Converts a DirectDraw, Direct3D or Direct3D RM error code to a string.
 */
char* D3DAppErrorToString(HRESULT error);

/*
 * D3DAppCreateSurface
 * Creates a surface described by ddsd.  Will force the surface into
 * systemmemory if D3DApp was initialized with D3DAPP_ONLYSYSTEMMEMORY.
 */
BOOL D3DAppCreateSurface(DDSURFACEDESC *ddsd, LPDIRECTDRAWSURFACE *lplpSurf);

/*
 * D3DAppLastError
 * D3DAppLastErrorString
 * Returns the last D3DApp error as a string and HRESULT.
 */
HRESULT D3DAppLastError(void);
char* D3DAppLastErrorString(void);

/*
 * D3DAppDestroy
 * Destroys all objects including Direct Draw.  Call before program
 * termination.
 */
BOOL D3DAppDestroy(void);

#ifdef __cplusplus
};
#endif

#endif // __D3DAPP_H__
