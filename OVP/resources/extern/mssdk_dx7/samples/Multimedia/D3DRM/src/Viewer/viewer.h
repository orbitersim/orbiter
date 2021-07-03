/*==========================================================================
 *
 *  Copyright (C) 1995-1999 Microsoft Corporation. All Rights Reserved.
 *
 *  File: viewer.h
 *
 ***************************************************************************/

#define MENU_FILE_ABOUT			  1
#define MENU_FILE_LOAD_XFILE		  2
#define MENU_FILE_EXIT			  5
#define MENU_FILE_OPEN_PMESH    	  7

#define MENU_EDIT_CUT			100
#define MENU_EDIT_COPY			101
#define MENU_EDIT_PASTE			102
#define MENU_EDIT_DELETE		103
#define MENU_EDIT_COLOR		104
#define MENU_EDIT_BOXES			105

#define COLOR_ALPHAACCEL                711
#define COLOR_ALPHA                     714

#define MENU_QUALITY_LIGHTING		200
#define MENU_QUALITY_POINTS		201
#define MENU_QUALITY_WIREFRAME		202
#define MENU_QUALITY_SOLID		203
#define MENU_QUALITY_FLAT		204
#define MENU_QUALITY_GOURAUD		205
#define MENU_QUALITY_PHONG		206

#define MENU_MODEL_MONO			207
#define MENU_MODEL_RGB			208

#define MENU_DITHER			209

#define MENU_TEXTURE_FILTERING		210

#define MENU_LIGHT_DIRECTIONAL		301
#define MENU_LIGHT_PARALLEL_POINT 	302
#define MENU_LIGHT_POINT		303
#define MENU_LIGHT_SPOT			304

#undef RELEASE
#ifdef __cplusplus
#define RELEASE(x) if (x != NULL) {x->Release(); x = NULL;}
#else
#define RELEASE(x) if (x != NULL) {x->lpVtbl->Release(x); x = NULL;}
#endif

#ifdef __cplusplus
extern "C" {
#endif

typedef struct _AppInfo
{
    LPDIRECT3DRMFRAME3 scene, camera;
    LPDIRECT3DRMDEVICE3 dev;
    LPDIRECT3DRMVIEWPORT2 view;
    D3DRMCOLORMODEL model;
    LPDIRECT3DRMPROGRESSIVEMESH lpPM;
    BOOL bMinimized;
    HWND win;
} AppInfo;

extern AppInfo* active_window;

extern LPDIRECT3DRM3 lpD3DRM;
extern float gfVal;

extern void SelectPM(LPDIRECT3DRMPROGRESSIVEMESH);
extern void GetDXVersion(LPDWORD pdwDXVersion, LPDWORD pdwDXPlatform);
char* OpenNewFile(HWND, const char *);
int ChooseNewColor(HWND, D3DCOLOR*);
#ifdef __cplusplus
}
#endif
