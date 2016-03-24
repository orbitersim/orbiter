#pragma once

#define RENDERPROC_DELETE		0x0000
#define RENDERPROC_HUD_1ST		0x0001
#define RENDERPROC_HUD_2ND		0x0002

#define SKETCHPAD_NONE			0x0000
#define SKETCHPAD_GDI			0x0001
#define SKETCHPAD_DIRECTX		0x0002

typedef void * CAMERAHANDLE;

// --------------------------------------------------------------------------------------
typedef void(__cdecl *__ogciRenderProc)(SURFHANDLE hSurf);

// --------------------------------------------------------------------------------------
typedef DWORD(__cdecl *__ogciClientID)();
typedef SURFHANDLE(__cdecl *__ogciCreateSurfaceEx)(int  width, int  height, DWORD  attrib);
typedef DWORD(__cdecl *__ogciGetSurfaceAttribs)(SURFHANDLE hSurf, bool bCreation);
typedef void (__cdecl *__ogciConvertSurface)(SURFHANDLE hSurf, DWORD attrib);
typedef bool (__cdecl *__ogciGenerateMipMaps)(SURFHANDLE hSurface, DWORD filter);
typedef bool (__cdecl *__ogciRegisterSkinName)(const VISHANDLE hVisual, const char *name);
typedef bool (__cdecl *__ogciRegisterRenderProc)(__ogciRenderProc proc, DWORD id);

// Custom Camera Interface
typedef int   (__cdecl *__ogciDeleteCustomCamera)(CAMERAHANDLE hCam);
typedef void  (__cdecl *__ogciCustomCameraOnOff)(CAMERAHANDLE hCam, bool bOn);
typedef CAMERAHANDLE(__cdecl *__ogciSetupCustomCamera)(CAMERAHANDLE hCam, OBJHANDLE hVessel, VECTOR3 &pos, VECTOR3 &dir, VECTOR3 &up, double fov, SURFHANDLE hSurf, DWORD flags);

// Custom Sketchpad Goodies
typedef int   (__cdecl *__ogciSketchpadVersion)(oapi::Sketchpad *pSkp);
typedef void  (__cdecl *__ogciSketchBlt)(oapi::Sketchpad *pSkp, SURFHANDLE hSrc, int tx, int ty);
typedef void  (__cdecl *__ogciSketchBltEx)(oapi::Sketchpad *pSkp, SURFHANDLE hSrc, LPRECT s, LPRECT t, float alpha, VECTOR3 *color);
typedef void  (__cdecl *__ogciSketchRotateBlt)(oapi::Sketchpad *pSkp, SURFHANDLE hSrc, LPRECT s, int tcx, int tcy, int w, int h, float angle, float alpha, VECTOR3 *color);

