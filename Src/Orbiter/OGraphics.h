// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// =======================================================================
// OrbiterGraphics class
// Inline graphics client
// =======================================================================

#ifndef __OGRAPHICS_H
#define __OGRAPHICS_H

#define OAPI_IMPLEMENTATION
#include "D3d7enum.h"
#include "D3d7frame.h"
#include "GraphicsAPI.h"
#include "GDIClient/GDIClient.h"

// =======================================================================

struct DeviceData {
	DWORD dwDevice;
	DWORD dwMode;
	BOOL  bFullscreen;
	BOOL  bStereo;
};

class Orbiter;
class OrbiterGraphics;
class Scene;
class Config;

// =======================================================================

class VideoTab {
public:
	VideoTab (OrbiterGraphics *og);
	void Init();
	INT_PTR WndProc (HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam);

protected:
	void DeviceChanged (D3D7Enum_DeviceInfo *dev);
	void ModeChanged (D3D7Enum_DeviceInfo *dev, DWORD idx);
	void BPPChanged (D3D7Enum_DeviceInfo *dev, DWORD idx);
	void DispmodeChanged (D3D7Enum_DeviceInfo *dev, BOOL bWindow);
	void PageFlipChanged ();
	void WidthChanged ();
	void HeightChanged ();
	void FixedAspectChanged ();
	void ForceDeviceEnum ();

private:
	OrbiterGraphics *gclient;
	Config *cfg;
	HWND hVid;
	int aspect_idx;
};

// =======================================================================

class OrbiterGraphics: public GDIClient {
	friend class Orbiter;
	friend class Scene;
	friend class VideoTab;

public:
	OrbiterGraphics (Orbiter *po);
	~OrbiterGraphics ();
	void clbkRefreshVideoData ();
	bool clbkInitialise ();
	HWND clbkCreateRenderWindow ();
	void clbkPostCreation ();
	bool clbkSplashLoadMsg (const char *msg, int line);

	void clbkCloseSession (bool fastclose);
	void clbkDestroyRenderWindow (bool fastclose);
	void clbkRenderScene ();
	bool clbkDisplayFrame ();
	bool clbkFullscreenMode () const;
	void clbkGetViewportSize (DWORD *width, DWORD *height) const;
	bool clbkGetRenderParam (DWORD prm, DWORD *value) const;
	LRESULT RenderWndProc (HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam);
	INT_PTR LaunchpadVideoWndProc (HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam);

	int clbkVisEvent (OBJHANDLE hObj, VISHANDLE vis, DWORD msg, UINT context);
	MESHHANDLE clbkGetMesh (VISHANDLE vis, UINT idx);
	int clbkGetMeshGroup (DEVMESHHANDLE hMesh, DWORD grpidx, GROUPREQUESTSPEC *grs);
	int clbkEditMeshGroup (DEVMESHHANDLE hMesh, DWORD grpidx, GROUPEDITSPEC *ges);

	// general event notifications
	void clbkUpdate (bool running);
	void clbkTimeJump (double simt, double simdt, double mjd);

	// dialog methods
	void clbkPreOpenPopup ();

	// vessel notifications
	void clbkNewVessel (OBJHANDLE hVessel);
	void clbkDeleteVessel (OBJHANDLE hVessel);
	void clbkVesselJump (OBJHANDLE hVessel);

	// declare in GraphicsClient (but aspect, nearplane and farplane should be managed by the client itself)
	void clbkSetCamera (double aspect, double tan_ap, double nearplane, double farplane);

	void clbkRender2DPanel (SURFHANDLE *hSurf, MESHHANDLE hMesh, MATRIX3 *T, bool additive = false);
	void clbkRender2DPanel (SURFHANDLE *hSurf, MESHHANDLE hMesh, MATRIX3 *T, float alpha, bool additive = false);

	inline Scene *GetScene() { return scene; }
	inline DWORD GetViewW() const { return viewW; }
	inline DWORD GetViewH() const { return viewH; }

	// Framework accessor functions
	inline CD3DFramework7*      GetFramework() const  { return m_pFramework; }
	inline LPDIRECTDRAW7        GetDirectDraw() const { return m_pDD; }
	inline LPDIRECT3D7          GetDirect3D7() const  { return m_pD3D; }
    inline LPDIRECT3DDEVICE7    GetDevice()		 	  { return m_pd3dDevice; }
	inline LPDIRECTDRAWSURFACE7 GetRenderTarget()     {
		return m_pddsRenderTarget;
	}

	inline bool isTLDevice() const { return m_pFramework->IsTLDevice() != 0; }
	inline DWORD GetStencilDepth() const { return m_pFramework->GetStencilBitDepth(); }

    void SetAppLeftViewMatrix( D3DMATRIX mat )  { m_matLeftView  = mat; }
    void SetAppRightViewMatrix( D3DMATRIX mat ) { m_matRightView = mat; }
    void SetAppViewMatrix( D3DMATRIX mat )      { m_matView      = mat; }

	// particle stream methods
	oapi::ParticleStream *clbkCreateParticleStream (PARTICLESTREAMSPEC *pss);
	oapi::ParticleStream *clbkCreateExhaustStream (PARTICLESTREAMSPEC *pss,
		OBJHANDLE hVessel, const double *lvl, const VECTOR3 *ref, const VECTOR3 *dir);
	oapi::ParticleStream *clbkCreateExhaustStream (PARTICLESTREAMSPEC *pss,
		OBJHANDLE hVessel, const double *lvl, const VECTOR3 &ref, const VECTOR3 &dir);
	oapi::ParticleStream *clbkCreateReentryStream (PARTICLESTREAMSPEC *pss,
		OBJHANDLE hVessel);
	bool clbkParticleStreamExists (const oapi::ParticleStream *ps);

	// texture functions
	SURFHANDLE clbkLoadTexture (const char *fname, DWORD flags = 0);
	void clbkReleaseTexture (SURFHANDLE hTex);
	bool clbkSetMeshTexture (DEVMESHHANDLE hMesh, DWORD texidx, SURFHANDLE tex);
    int clbkSetMeshMaterial (DEVMESHHANDLE hMesh, DWORD matidx, const MATERIAL *mat);
	int clbkMeshMaterial (DEVMESHHANDLE hMesh, DWORD matidx, MATERIAL *mat);
	bool clbkSetMeshProperty (DEVMESHHANDLE hMesh, DWORD property, DWORD value);

	// surface functions
	SURFHANDLE clbkCreateSurface (DWORD w, DWORD h, SURFHANDLE hTemplate = NULL);
	SURFHANDLE clbkCreateSurfaceEx (DWORD w, DWORD h, DWORD attrib);
	SURFHANDLE clbkCreateTexture (DWORD w, DWORD h);
	void clbkIncrSurfaceRef (SURFHANDLE surf);
	bool clbkReleaseSurface (SURFHANDLE surf);
	bool clbkGetSurfaceSize (SURFHANDLE surf, DWORD *w, DWORD *h);
	bool clbkSetSurfaceColourKey (SURFHANDLE surf, DWORD ckey);
	DWORD clbkGetDeviceColour (BYTE r, BYTE g, BYTE b);

	// blitting functions
	bool clbkBlt (SURFHANDLE tgt, DWORD tgtx, DWORD tgty, SURFHANDLE src, DWORD flag = 0) const;
	bool clbkBlt (SURFHANDLE tgt, DWORD tgtx, DWORD tgty, SURFHANDLE src, DWORD srcx, DWORD srcy, DWORD w, DWORD h, DWORD flag = 0) const;
	bool clbkScaleBlt (SURFHANDLE tgt, DWORD tgtx, DWORD tgty, DWORD tgtw, DWORD tgth,
		SURFHANDLE src, DWORD srcx, DWORD srcy, DWORD srcw, DWORD srch, DWORD flag = 0) const;
	bool clbkBltCK (SURFHANDLE tgt, DWORD tgtx, DWORD tgty, SURFHANDLE src, DWORD srcx, DWORD srcy, DWORD w, DWORD h, DWORD ck) const;
	bool clbkScaleBltCK (SURFHANDLE tgt, DWORD tgtx, DWORD tgty, DWORD tgtw, DWORD tgth,
		SURFHANDLE src, DWORD srcx, DWORD srcy, DWORD srcw, DWORD srch, DWORD ck) const;
	bool clbkFillSurface (SURFHANDLE surf, DWORD col) const;
	bool clbkFillSurface (SURFHANDLE surf, DWORD tgtx, DWORD tgty, DWORD w, DWORD h, DWORD col) const;

	// GDI methods
	HDC clbkGetSurfaceDC (SURFHANDLE surf);
	void clbkReleaseSurfaceDC (SURFHANDLE surf, HDC hDC);

protected:
	inline void SetDeviceInfo (D3D7Enum_DeviceInfo *di) { m_pDeviceInfo = di; }
	inline D3D7Enum_DeviceInfo *GetDeviceInfo () { return m_pDeviceInfo; }
	bool SelectDevice (D3D7Enum_DeviceInfo **dev, DeviceData *dd);
	HRESULT ReEnumerate (TCHAR* fname, DeviceData *dd);
	HRESULT Init3DEnvironment ();
	HRESULT Change3DEnvironment ();
	void Exit3DEnvironment ();
	HRESULT RestoreSurfaces ();
	bool RenderWithPopupWindows ();
	static HRESULT ConfirmDevice (DDCAPS*, D3DDEVICEDESC7*);
	void LogRenderParams () const;
	void WriteLog (const char *msg) const;

	// splash screen load indicator helpers
	void InitOutputLoadStatus ();
	void ExitOutputLoadStatus ();
	bool OutputLoadStatus (const char *msg, int line);

private:
	Orbiter *orbiter;
	Scene *scene;
	VideoTab *vtab;
    D3D7Enum_DeviceInfo* m_pDeviceInfo;
	DWORD viewW, viewH;     // dimensions of render viewport
	DWORD viewBPP;          // bit depth of render viewport
	bool bFullscreen;       // fullscreen mode?
	bool bUseZBuffer;       // supports z-buffer? (should always be true!)
    bool bUseStereo;        // stereo view enabled device? (not supported)
	bool bNoVSync;

	// Framework objects
	CD3DFramework7      *m_pFramework;
	LPDIRECTDRAW7        m_pDD;
	LPDIRECT3D7          m_pD3D;
	LPDIRECT3DDEVICE7    m_pd3dDevice;
    LPDIRECTDRAWSURFACE7 m_pddsRenderTarget;
	LPDIRECTDRAWSURFACE7 m_pddsRenderTargetLeft; // For stereo modes (not supported)
	DDSURFACEDESC2       m_ddsdRenderTarget;
	LPDIRECTDRAWCLIPPER  clipper;

    // View control functions (for stereo-enabled applications)
    D3DMATRIX m_matLeftView;
    D3DMATRIX m_matRightView;
    D3DMATRIX m_matView;

	// Load status output parameters
	struct {
		int x, y, w, h;
		HBITMAP hBkg;
		HDC bkgDC;
	} lstatus;
};

#endif // !__OGRAPHICS_H