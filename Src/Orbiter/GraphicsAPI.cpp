// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#ifndef __linux__
#define STRICT 1
#endif // !__linux__
#define OAPI_IMPLEMENTATION

#include "Orbiter.h"
#include "Launchpad.h"
#include "LpadTab.h"
#include "TabVideo.h"
#include "Psys.h"
#include "Pane.h"
#include "VCockpit.h"
#include "GraphicsAPI.h"
#include "DlgMgr.h"
#include "Log.h"
#include "Util.h"
#include "resource.h"
#ifndef __linux__
#include <wincodec.h>
#else // __linux__
#include "OrbiterResource.h"
// wincodec.h left out: image files go through QImage
#include <QWindow>
#include <QScreen>
#include <QImage>
#include <QImageReader>
#include <QBuffer>
#include <QPainter>
#include <QCoreApplication>
#include <QElapsedTimer>
#include <set>
#endif // __linux__
#include <filesystem>
namespace fs = std::filesystem;

using std::min;

extern Orbiter *g_pOrbiter;
extern PlanetarySystem *g_psys;
extern Pane *g_pane;
#ifdef __linux__
extern const char *g_strAppTitle; // file scope: declared inside the member function it named oapi::g_strAppTitle
#endif // __linux__

using namespace oapi;

const char *strWndClass = "Orbiter Render Window";

#ifndef __linux__
OAPIFUNC LRESULT CALLBACK WndProc (HWND, UINT, WPARAM, LPARAM);
// Render window callback (calls RenderWndProc)
#else // __linux__
// WndProc counterpart: an event filter on the render window that passes its events to RenderWndProc
class RenderWndHook: public QObject {
public:
	RenderWndHook (QWindow *hWnd, GraphicsClient *_gc): QObject (hWnd), gc (_gc)
	{ setObjectName (strWndClass); hWnd->installEventFilter (this); live.insert (this); }
	~RenderWndHook () { live.erase (this); }
	bool eventFilter (QObject *obj, QEvent *event) override
	{ return gc && gc->RenderWndProc (static_cast<QWindow*>(obj), event); }
	GraphicsClient *gc;
	static std::set<RenderWndHook*> live; // hooks whose window still exists
};
std::set<RenderWndHook*> RenderWndHook::live;
#endif // __linux__

#ifndef __linux__
OAPIFUNC INT_PTR CALLBACK LaunchpadVideoWndProc (HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam);
// 'Video' tab window callback
#else // __linux__
// LaunchpadVideoWndProc export left out: the video tab calls GraphicsClient::LaunchpadVideoWndProc directly
#endif // __linux__

// ======================================================================
// class GraphicsClient

#ifndef __linux__
GraphicsClient::GraphicsClient (HINSTANCE hInstance): Module (hInstance)
#else // __linux__
GraphicsClient::GraphicsClient (void *hInstance): Module (hInstance)
#endif // __linux__
{
	hOrbiterInst = g_pOrbiter->GetInstance();
	VideoData.fullscreen = false;
	VideoData.forceenum = true;
	VideoData.trystencil = false;
	VideoData.novsync = true;
	VideoData.pageflip = true;
	VideoData.deviceidx = -1;
	VideoData.modeidx = 0;
	VideoData.outputidx = 0;
	VideoData.style = 1;
	VideoData.winw = 1024;
	VideoData.winh = 768;
	surfBltTgt = RENDERTGT_NONE;
	splashFont = 0;
#ifdef __linux__
	hVid = NULL;
	hRenderWnd = NULL;
#endif // __linux__

#ifndef __linux__
    // Create WIC factory for formatted image output
    HRESULT hr = CoCreateInstance (
        CLSID_WICImagingFactory,
        NULL,
        CLSCTX_INPROC_SERVER,
        IID_PPV_ARGS(&m_pIWICFactory)
    );
	if (hr != S_OK)
		m_pIWICFactory = NULL;
		
#else // __linux__
	// WIC factory left out: image files go through QImage
#endif // __linux__
}

// ======================================================================

GraphicsClient::~GraphicsClient ()
{
#ifndef __linux__
	if (hVid) SetWindowLongPtr (hVid, GWLP_USERDATA, 0);
#else // __linux__
	// hVid userdata reset left out: the video tab disconnects the client's controls itself
	for (RenderWndHook *hook : RenderWndHook::live) // not hRenderWnd: the session end deletes that window, the pointer is kept
		if (hook->gc == this) hook->gc = NULL;
#endif // __linux__
	if (splashFont) clbkReleaseFont (splashFont);
}

// ======================================================================

bool GraphicsClient::clbkInitialise ()
{
#ifndef __linux__
    // Register a window class for the render window
    WNDCLASS wndClass = {0, ::WndProc, 0, 0, hModule,
		LoadIcon (g_pOrbiter->GetInstance(), MAKEINTRESOURCE(IDI_MAIN_ICON)),
		LoadCursor (NULL, IDC_ARROW),
		(HBRUSH)GetStockObject (WHITE_BRUSH),
		NULL, strWndClass};
    RegisterClass (&wndClass);
#else // __linux__
	// RegisterClass left out: the render window is a QWindow, its events reach RenderWndProc through RenderWndHook
#endif // __linux__

	if (clbkUseLaunchpadVideoTab() && g_pOrbiter->Launchpad()) {
		hVid = g_pOrbiter->Launchpad()->GetTab(PG_VID)->TabWnd();
#ifndef __linux__
		SetWindowLongPtr (hVid, GWLP_USERDATA, (LONG_PTR)this);
#endif // !__linux__
	} else hVid = NULL;

	// set default parameters from config data
	Config *cfg = g_pOrbiter->Cfg();
	VideoData.fullscreen = cfg->CfgDevPrm.bFullscreen;
	VideoData.forceenum  = cfg->CfgDevPrm.bForceEnum;
	VideoData.trystencil = cfg->CfgDevPrm.bTryStencil;
	VideoData.novsync    = cfg->CfgDevPrm.bNoVsync;
	VideoData.pageflip   = cfg->CfgDevPrm.bPageflip;
	VideoData.deviceidx  = cfg->CfgDevPrm.Device_idx;
	VideoData.outputidx  = cfg->CfgDevPrm.Device_out;
	VideoData.style		 = cfg->CfgDevPrm.Device_style;
	VideoData.modeidx    = (int)cfg->CfgDevPrm.Device_mode;
	VideoData.winw       = (int)cfg->CfgDevPrm.WinW;
	VideoData.winh       = (int)cfg->CfgDevPrm.WinH;

#ifndef __linux__
	char fname[256];
	GetModuleFileName(hModule, fname, 256);
#else // __linux__
	const char *fname = ModuleFileName (hModule); // GetModuleFileName
#endif // __linux__
	((orbiter::DefVideoTab*)g_pOrbiter->Launchpad()->GetTab(PG_VID))->OnGraphicsClientLoaded(this, fname);

	return true;
}

// ======================================================================

void GraphicsClient::RegisterVisObject (OBJHANDLE hObj, VISHANDLE vis)
{
	((Body*)hObj)->RegisterVisual (vis);
}

// ======================================================================

void GraphicsClient::UnregisterVisObject (OBJHANDLE hObj)
{
	((Body*)hObj)->UnregisterVisual();
}

// ======================================================================

int GraphicsClient::clbkVisEvent (OBJHANDLE hObj, VISHANDLE vis, DWORD msg, DWORD_PTR context)
{
	return 0;
}

// ======================================================================

ParticleStream *GraphicsClient::clbkCreateParticleStream (PARTICLESTREAMSPEC *pss)
{
	return NULL;
}

// ======================================================================

ParticleStream *GraphicsClient::clbkCreateExhaustStream (PARTICLESTREAMSPEC *pss,
	OBJHANDLE hVessel, const double *lvl, const VECTOR3 *ref, const VECTOR3 *dir)
{
	return NULL;
}

// ======================================================================

ParticleStream *GraphicsClient::clbkCreateExhaustStream (PARTICLESTREAMSPEC *pss,
	OBJHANDLE hVessel, const double *lvl, const VECTOR3 &ref, const VECTOR3 &dir)
{
	return NULL;
}

// ======================================================================

ParticleStream *GraphicsClient::clbkCreateReentryStream (PARTICLESTREAMSPEC *pss,
	OBJHANDLE hVessel)
{
	return NULL;
}

// ======================================================================

ScreenAnnotation *GraphicsClient::clbkCreateAnnotation ()
{
	TRACENEW; return new ScreenAnnotation (this);
}

// ======================================================================

bool GraphicsClient::TexturePath (const char *fname, char *path) const
{
	// first try htex directory
	strcpy (path, g_pOrbiter->Cfg()->CfgDirPrm.HightexDir);
	strcat (path, fname);
#ifndef __linux__
	if (fs::exists(path)) return true;
#else // __linux__
	std::string r = oapiResolvePath (path); // the resolved path is returned: callers open it directly
	if (fs::exists(r)) { snprintf (path, MAX_PATH, "%s", r.c_str()); return true; }
#endif // __linux__

	// try tex directory
	strcpy (path, g_pOrbiter->Cfg()->CfgDirPrm.TextureDir);
	strcat (path, fname);

#ifndef __linux__
	if (fs::exists(path)) return true;
#else // __linux__
	r = oapiResolvePath (path);
	if (fs::exists(r)) { snprintf (path, MAX_PATH, "%s", r.c_str()); return true; }
#endif // __linux__

	return false;
}

// ======================================================================

bool GraphicsClient::PlanetTexturePath(const char* planetname, char* path) const
{
	g_pOrbiter->Cfg()->PTexPath(path, planetname);
	return true;
}

// ======================================================================

#ifndef __linux__
DWORD GraphicsClient::GetPopupList (const HWND **hPopupWnd) const
#else // __linux__
DWORD GraphicsClient::GetPopupList (QWidget *const **hPopupWnd) const
#endif // __linux__
{
	DialogManager *dlgmgr = g_pOrbiter->DlgMgr();
	if (dlgmgr) return dlgmgr->GetDlgList (hPopupWnd);
	else return 0;
}

// ======================================================================

SURFHANDLE GraphicsClient::GetVCHUDSurface (const VCHUDSPEC **hudspec) const
{
	VirtualCockpit *vc;
	if (g_pane && g_pane->GetHUD() && (vc = g_pane->GetVC())) {
		*hudspec = vc->GetHUDParams ();
		return vc->GetHUDSurf();
	} else
		return NULL;
}

// ======================================================================

SURFHANDLE GraphicsClient::GetMFDSurface (int mfd) const
{
	return (g_pane ? g_pane->GetMFDSurface (mfd) : NULL);
}

// ======================================================================

SURFHANDLE GraphicsClient::GetVCMFDSurface (int mfd, const VCMFDSPEC **mfdspec) const
{
	*mfdspec = g_pane->GetVCMFDParams (mfd);
	if (g_pane && g_pane->GetVC() && g_pane->MFD (mfd)) {
		return g_pane->MFD(mfd)->Texture();
	} else
		return NULL;
}

// ======================================================================

DWORD GraphicsClient::GetBaseTileList (OBJHANDLE hBase, const SurftileSpec **tile) const
{
	return ((Base*)hBase)->GetTileList (tile);
}

// ======================================================================

void GraphicsClient::GetBaseStructures (OBJHANDLE hBase, MESHHANDLE **mesh_bs, DWORD *nmesh_bs, MESHHANDLE **mesh_as, DWORD *nmesh_as) const
{
	((Base*)hBase)->ExportBaseStructures ((Mesh***)mesh_bs, nmesh_bs, (Mesh***)mesh_as, nmesh_as);
}

// ======================================================================

void GraphicsClient::GetBaseShadowGeometry (OBJHANDLE hBase, MESHHANDLE **mesh_sh, double **elev, DWORD *nmesh_sh) const
{
	((Base*)hBase)->ExportShadowGeometry ((Mesh***)mesh_sh, elev, nmesh_sh);
}

// ======================================================================

const void *GraphicsClient::GetConfigParam (DWORD paramtype) const
{
	return g_pOrbiter->Cfg()->GetParam (paramtype);
}

// ======================================================================

#ifndef __linux__
HWND GraphicsClient::clbkCreateRenderWindow ()
#else // __linux__
QWindow *GraphicsClient::clbkCreateRenderWindow ()
#endif // __linux__
{
#ifndef __linux__
	HWND hWnd;
#else // __linux__
	QWindow *hWnd = new QWindow;
	hWnd->setObjectName (strWndClass);
	hWnd->setSurfaceType (QSurface::VulkanSurface); // must precede create(); the client attaches its QVulkanInstance
#endif // __linux__

	if (VideoData.fullscreen) {
#ifndef __linux__
		hWnd = CreateWindow (strWndClass, "", // dummy window
			WS_POPUP | WS_EX_TOPMOST| WS_VISIBLE,
			CW_USEDEFAULT, CW_USEDEFAULT, 10, 10, 0, 0, hModule, (LPVOID)this);
#else // __linux__
		hWnd->showFullScreen (); // the Direct3D client resized a 10x10 dummy popup; Qt shows the window fullscreen directly
#endif // __linux__
	} else {
#ifndef __linux__
		hWnd = CreateWindow (strWndClass, "",
			WS_OVERLAPPED | WS_CAPTION | WS_SYSMENU | WS_VISIBLE,
			CW_USEDEFAULT, CW_USEDEFAULT, VideoData.winw, VideoData.winh, 0, 0, hModule, (LPVOID)this);
#else // __linux__
		qreal dpr = (hWnd->screen() ? hWnd->screen()->devicePixelRatio() : 1.0); // winw/winh are device pixels
		QSize size ((int)(VideoData.winw/dpr), (int)(VideoData.winh/dpr));
		hWnd->setFlags (Qt::Window | Qt::CustomizeWindowHint | Qt::WindowTitleHint | Qt::WindowSystemMenuHint | Qt::WindowCloseButtonHint); // WS_OVERLAPPED | WS_CAPTION | WS_SYSMENU
		hWnd->resize (size);
		hWnd->setMinimumSize (size); // no WS_THICKFRAME: a fixed size
		hWnd->setMaximumSize (size);
		hWnd->show ();
		// not upstream: CreateWindow returns with the final size; a Wayland compositor decides it in its first configure (KWin fits it to the work area)
		QElapsedTimer t;
		t.start ();
		while (!hWnd->isExposed() && t.elapsed() < 1000)
			QCoreApplication::processEvents (QEventLoop::ExcludeUserInputEvents, 20);
		hWnd->setMinimumSize (hWnd->size());
		hWnd->setMaximumSize (hWnd->size());
#endif // __linux__
	}
	return hWnd;
}

// ======================================================================

void GraphicsClient::clbkDestroyRenderWindow (bool fastclose)
{
	if (splashFont) {
		clbkReleaseFont (splashFont);
		splashFont = 0;
	}
}

// ======================================================================

void GraphicsClient::Render2DOverlay ()
{
	g_pane->Render();
}

// ======================================================================

bool GraphicsClient::ElevationGrid (ELEVHANDLE hElev, int ilat, int ilng, int lvl,
	int pilat, int pilng, int plvl, INT16 *pelev, float *elev, double *emean) const
{
	if (!hElev) return false;
	ElevationManager *emgr = (ElevationManager*)hElev;
	emgr->ElevationGrid (ilat, ilng, lvl, pilat, pilng, plvl, pelev, elev, emean);
	return true;
}

// ======================================================================

bool GraphicsClient::ElevationGrid(ELEVHANDLE hElev, int ilat, int ilng, int lvl,
	int pilat, int pilng, int plvl, INT16* pelev, INT16* elev, double* emean) const
{
	if (!hElev) return false;
	ElevationManager* emgr = (ElevationManager*)hElev;
	emgr->ElevationGrid(ilat, ilng, lvl, pilat, pilng, plvl, pelev, elev, emean);
	return true;
}

// ======================================================================

void GraphicsClient::ShowDefaultSplash ()
{
	const DWORD texcol = 0xA06060;
	DWORD rw, rh;
	clbkGetViewportSize (&rw, &rh);

	//const DWORD bmw = rw, bmh = (rw*10)/16; // source image is 1920x1200, i.e. 16/20 aspect ratio
	const DWORD bmw = min(rw, (rh*16)/10);
	const DWORD bmh = (bmw*10)/16;
#ifndef __linux__
	HMODULE hMod = GetModuleHandle(NULL);
	HRSRC hRsrc = FindResource(hMod,MAKEINTRESOURCE(IDR_IMAGE1), "IMAGE");
	HGLOBAL hGlob = LoadResource(hMod, hRsrc);
	BYTE *pBuf = (BYTE*)LockResource(hGlob);
	DWORD nBuf = SizeofResource(hMod,hRsrc);
	HBITMAP hbm = ReadImageFromMemory (pBuf, nBuf, bmw, bmh);
#else // __linux__
	const RESDATA *res = oapiFindResData (g_pOrbiter->GetInstance(), "IMAGE", IDR_IMAGE1); // FindResource/LoadResource on the exe
	BYTE *pBuf = (BYTE*)(res ? res->data : NULL);
	DWORD nBuf = (res ? res->size : 0);
	QImage *hbm = ReadImageFromMemory (pBuf, nBuf, bmw, bmh);
#endif // __linux__

	// copy splash screen to viewport
	SURFHANDLE surf = GraphicsClient::clbkCreateSurface (hbm);
	DWORD tgtx = 0, tgty = 0;
	if (bmw < rw) tgtx = (rw-bmw)/2;
	if (bmh < rh) tgty = (rh-bmh)/2;
	if (surf) clbkBlt (NULL, tgtx, tgty, surf, 0, 0, bmw, bmh);
	clbkReleaseSurface (surf);

	oapi::Sketchpad *skp = clbkGetSketchpad (NULL);
	skp->SetBackgroundMode (oapi::Sketchpad::BK_TRANSPARENT);
	DWORD fontsize = 16; //max(rw/120,10);
	DWORD x0 = 10;//rw-fontsize*25;
	DWORD y0 = rh-fontsize; //tgty + (DWORD)(rw*0.078);
	if (splashFont) clbkReleaseFont (splashFont);
	splashFont = clbkCreateFont(fontsize,true,"Arial", FONT_NORMAL);
	skp->SetFont (splashFont);
	skp->SetTextColor (texcol);
	skp->SetTextAlign (oapi::Sketchpad::LEFT, oapi::Sketchpad::TOP);

#ifndef __linux__
	//DeleteObject(hbm);
#else // __linux__
	delete hbm; // DeleteObject; upstream leaked the bitmap
#endif // __linux__
	clbkReleaseSketchpad (skp);

	clbkDisplayFrame();
}

// ======================================================================

#ifndef __linux__
#define DIB_WIDTHBYTES(bits) ((((bits) + 31)>>5)<<2)

#endif // !__linux__
// Image decoding engine: extract an image from a decoder and rescale it to the desired size
// Return as bitmap
#ifndef __linux__
HBITMAP ReadImageFromDecoder (IWICImagingFactory *m_pIWICFactory, IWICBitmapDecoder *piDecoder, UINT w, UINT h)
#else // __linux__
static QImage *ReadImageFromDecoder (QImageReader &reader, UINT w, UINT h)
#endif // __linux__
{
#ifndef __linux__
	IWICBitmapFrameDecode *piFrame = NULL;
	IWICFormatConverter *piConvertedFrame = NULL;
	IWICBitmapScaler *piScaler = NULL;

	UINT nWidth, nHeight, nFrame, nCount = 0;
	piDecoder->GetFrameCount(&nCount);
	nFrame = nCount-1;

	piDecoder->GetFrame(nFrame, &piFrame);
	piFrame->GetSize(&nWidth, &nHeight);

	m_pIWICFactory->CreateFormatConverter(&piConvertedFrame);
	piConvertedFrame->Initialize(
		piFrame,                        // Source frame to convert
        GUID_WICPixelFormat32bppBGR,     // The desired pixel format
        WICBitmapDitherTypeNone,         // The desired dither pattern
        NULL,                            // The desired palette
        0.f,                             // The desired alpha threshold
        WICBitmapPaletteTypeCustom       // Palette translation type
    );

	if (!w) w = nWidth;
	if (!h) h = nHeight;
	UINT nStride = DIB_WIDTHBYTES(w * 32);
	UINT nImage = nStride * h;
	m_pIWICFactory->CreateBitmapScaler(&piScaler);
	piScaler->Initialize(piConvertedFrame, w, h, WICBitmapInterpolationModeFant);

	HDC hdcScreen = GetDC(NULL);
	BITMAPINFO bminfo;
	ZeroMemory(&bminfo, sizeof(bminfo));
	bminfo.bmiHeader.biSize   = sizeof(BITMAPINFOHEADER);
	bminfo.bmiHeader.biWidth  = w;
	bminfo.bmiHeader.biHeight = -(LONG)h;
	bminfo.bmiHeader.biPlanes = 1;
	bminfo.bmiHeader.biBitCount = 32;
	bminfo.bmiHeader.biCompression = BI_RGB;
	void *pvImageBits = NULL;
	HBITMAP hDIBBitmap = CreateDIBSection (hdcScreen, &bminfo, DIB_RGB_COLORS, &pvImageBits, NULL, 0);
	ReleaseDC (NULL, hdcScreen);

	piScaler->CopyPixels(NULL, nStride, nImage, reinterpret_cast<BYTE*>(pvImageBits));

	piFrame->Release();
	piConvertedFrame->Release();
	piScaler->Release();
#else // __linux__
	int nCount = reader.imageCount ();
	if (nCount > 1) reader.jumpToImage (nCount-1); // last frame, as the WIC decoder was asked for
	QImage frame = reader.read ();
	if (frame.isNull ()) return NULL;
	frame = frame.convertToFormat (QImage::Format_RGB32); // GUID_WICPixelFormat32bppBGR: B,G,R,x bytes
#endif // __linux__

#ifndef __linux__
	return hDIBBitmap;
#else // __linux__
	if (!w) w = frame.width();
	if (!h) h = frame.height();
	if ((int)w != frame.width() || (int)h != frame.height())
		frame = frame.scaled (w, h, Qt::IgnoreAspectRatio, Qt::SmoothTransformation); // WICBitmapInterpolationModeFant
	return new QImage (frame); // CreateDIBSection + CopyPixels
#endif // __linux__
}

// ======================================================================

#ifndef __linux__
HBITMAP GraphicsClient::ReadImageFromMemory (BYTE *pBuf, DWORD nBuf, UINT w, UINT h)
#else // __linux__
QImage *GraphicsClient::ReadImageFromMemory (BYTE *pBuf, DWORD nBuf, UINT w, UINT h)
#endif // __linux__
{
#ifndef __linux__
	IWICBitmapDecoder *piDecoder = NULL;
	
	IWICStream *piStream;
	m_pIWICFactory->CreateStream(&piStream);
	piStream->InitializeFromMemory(pBuf,nBuf);
	m_pIWICFactory->CreateDecoderFromStream (piStream, NULL, WICDecodeMetadataCacheOnDemand, &piDecoder);

	piStream->Release();
	if (piDecoder) {
		HBITMAP hDIBBitmap = ReadImageFromDecoder (m_pIWICFactory, piDecoder, w, h);
		piDecoder->Release();
		return hDIBBitmap;
	} else {
#else // __linux__
	QByteArray data = QByteArray::fromRawData ((const char*)pBuf, nBuf);
	QBuffer buffer (&data);
	QImageReader reader (&buffer);
	QImage *hDIBBitmap = (pBuf && nBuf ? ReadImageFromDecoder (reader, w, h) : NULL);
	if (!hDIBBitmap)
#endif // __linux__
		LOGOUT_WARN("Couldn't create decoder for memory image data");
#ifndef __linux__
		return 0;
	}
#else // __linux__
	return hDIBBitmap;
#endif // __linux__
}

// ======================================================================

#ifndef __linux__
HBITMAP GraphicsClient::ReadImageFromFile (const char *fname, UINT w, UINT h)
#else // __linux__
QImage *GraphicsClient::ReadImageFromFile (const char *fname, UINT w, UINT h)
#endif // __linux__
{
#ifndef __linux__
	wchar_t wcbuf[256];
	mbstowcs (wcbuf, fname, 256);

	IWICBitmapDecoder *piDecoder = NULL;
	m_pIWICFactory->CreateDecoderFromFilename (wcbuf, NULL, GENERIC_READ, WICDecodeMetadataCacheOnDemand, &piDecoder);

	if (piDecoder) {
		HBITMAP hDIBBitmap = ReadImageFromDecoder (m_pIWICFactory, piDecoder, w, h);
		piDecoder->Release();
		return hDIBBitmap;
	} else {
#else // __linux__
	QImageReader reader (QString::fromStdString (oapiResolvePath (fname)));
	QImage *hDIBBitmap = ReadImageFromDecoder (reader, w, h);
	if (!hDIBBitmap)
#endif // __linux__
		LOGOUT_WARN("Couldn't create decoder for image file: %s (does it exist?)", fname);
#ifndef __linux__
		return 0;
	}
#else // __linux__
	return hDIBBitmap;
#endif // __linux__
}

// ======================================================================

bool GraphicsClient::WriteImageDataToFile (const ImageData &data,
	const char *fname, ImageFileFormat fmt, float quality)
{
	const char *extension[4] = {".bmp", ".png", ".jpg", ".tif"};

#ifndef __linux__
	const GUID FormatGUID[4] = {
		GUID_ContainerFormatBmp,
		GUID_ContainerFormatPng,
		GUID_ContainerFormatJpeg,
		GUID_ContainerFormatTiff,
#else // __linux__
	const char *Format[4] = { // GUID_ContainerFormat* counterparts
		"BMP",
		"PNG",
		"JPG",
		"TIFF",
#endif // __linux__
	};

#ifndef __linux__
	HRESULT hr = S_OK;
	// Note: hr should be checked after every function returning it.
	// The rest of the code should only be executed if SUCCEEDED(hr)

	if (!m_pIWICFactory)
		return false;

#endif // !__linux__
	if (data.bpp != 24)
		return false;  // can only deal with 24bit images for now

	if (data.stride != ((data.width * data.bpp + 31) & ~31) >> 3)
		return false;

	if (data.bufsize < data.stride * data.height)
		return false;
#ifndef __linux__
	UINT bufsize = data.stride * data.height;
#endif // !__linux__

#ifndef __linux__
	wchar_t wcbuf[256];
#endif // !__linux__
	char cbuf[256];
	strcpy (cbuf, fname);
	strcat (cbuf, extension[fmt]);
#ifndef __linux__
	mbstowcs (wcbuf, cbuf, 256);
#else // __linux__
	QString path = QString::fromStdString (oapiResolvePath (cbuf));
#endif // __linux__

#ifndef __linux__
	IWICStream *piStream = NULL;
	IWICBitmapEncoder *piEncoder = NULL;
	IWICBitmapFrameEncode *piBitmapFrame = NULL;
	IPropertyBag2 *pPropertybag = NULL;

	hr = m_pIWICFactory->CreateStream (&piStream);
	hr = piStream->InitializeFromFilename(wcbuf, GENERIC_WRITE);
	if ((hr & 0xFF) == ERROR_PATH_NOT_FOUND && MakePath(fname)) {
		hr = piStream->InitializeFromFilename(wcbuf, GENERIC_WRITE);
	}

	hr = m_pIWICFactory->CreateEncoder (FormatGUID[fmt], NULL, &piEncoder);
	hr = piEncoder->Initialize (piStream, WICBitmapEncoderNoCache);
	hr = piEncoder->CreateNewFrame (&piBitmapFrame, &pPropertybag);

	// customize output
	PROPBAG2 option = { 0 };
	option.pstrName = (wchar_t*)L"ImageQuality";
	VARIANT varValue;
	VariantInit (&varValue);
	varValue.vt = VT_R4;
	varValue.fltVal = quality;
	hr = pPropertybag->Write(1,&option,&varValue);
	hr = piBitmapFrame->Initialize (pPropertybag);

	hr = piBitmapFrame->SetSize (data.width, data.height);

	WICPixelFormatGUID formatGUID = GUID_WICPixelFormat24bppBGR;
	hr = piBitmapFrame->SetPixelFormat(&formatGUID);

	hr = IsEqualGUID(formatGUID, GUID_WICPixelFormat24bppBGR) ? S_OK : E_FAIL;

	hr = piBitmapFrame->WritePixels(data.height, data.stride, bufsize, data.data);

	hr = piBitmapFrame->Commit ();
	hr = piEncoder->Commit ();

	piBitmapFrame->Release();
	piEncoder->Release();
	piStream->Release();
	return true;
#else // __linux__
	// GUID_WICPixelFormat24bppBGR rows, top-down
	QImage img ((const uchar*)data.data, data.width, data.height, data.stride, QImage::Format_BGR888);
	int q = (int)(quality*100.0f + 0.5f); // ImageQuality 0..1 -> 0..100
	bool ok = img.save (path, Format[fmt], q);
	if (!ok && MakePath (fname)) // the WIC stream reported ERROR_PATH_NOT_FOUND
		ok = img.save (path, Format[fmt], q);
	return ok;
#endif // __linux__
}

// ======================================================================

void GraphicsClient::clbkRender2DPanel (SURFHANDLE *hSurf, MESHHANDLE hMesh, MATRIX3 *T, bool additive)
{
	// can we do any default device-independent processing here?
}

// ======================================================================

void GraphicsClient::clbkRender2DPanel (SURFHANDLE *hSurf, MESHHANDLE hMesh, MATRIX3 *T, float alpha, bool additive)
{
	// if not implemented by the client, just use default (opaque) rendering
	clbkRender2DPanel (hSurf, hMesh, T, additive);
}

// ======================================================================

#ifndef __linux__
SURFHANDLE GraphicsClient::clbkCreateSurface (HBITMAP hBmp)
#else // __linux__
SURFHANDLE GraphicsClient::clbkCreateSurface (QImage *hBmp)
#endif // __linux__
{
#ifndef __linux__
	BITMAP bm;
	GetObject (hBmp, sizeof(bm), &bm);
	SURFHANDLE surf = clbkCreateSurface (bm.bmWidth, bm.bmHeight);
#else // __linux__
	if (!hBmp) return NULL; // GetObject failed on a NULL bitmap
	SURFHANDLE surf = clbkCreateSurface (hBmp->width(), hBmp->height());
#endif // __linux__
	if (surf) {
		if (!clbkCopyBitmap (surf, hBmp, 0, 0, 0, 0)) {
			clbkReleaseSurface (surf);
			surf = NULL;
		}
	}
	return surf;
}

// ======================================================================

int GraphicsClient::clbkBeginBltGroup (SURFHANDLE tgt)
{
	if (tgt == RENDERTGT_NONE)
		return -3;
	if (surfBltTgt != RENDERTGT_NONE && surfBltTgt != tgt)
		return -2;
	surfBltTgt = tgt;
	return 0;
}

// ======================================================================

int GraphicsClient::clbkEndBltGroup ()
{
	if (surfBltTgt == RENDERTGT_NONE)
		return -2;
	surfBltTgt = RENDERTGT_NONE;
	return 0;
}

// ======================================================================

#ifndef __linux__
bool GraphicsClient::clbkCopyBitmap (SURFHANDLE pdds, HBITMAP hbm,
    int x, int y, int dx, int dy)
#else // __linux__
bool GraphicsClient::clbkCopyBitmap (SURFHANDLE pdds, QImage *hbm,
    int x, int y, int dx, int dy)
#endif // __linux__
{
#ifndef __linux__
    HDC                     hdcImage;
    HDC                     hdc;
    BITMAP                  bm;
#else // __linux__
    QPainter                *hdc;
#endif // __linux__
    //DDSURFACEDESC2          ddsd;
    //HRESULT                 hr;
	DWORD                   surfW, surfH;

    if (hbm == NULL || pdds == NULL)
        return false;
#ifndef __linux__
    //
    // Select bitmap into a memoryDC so we can use it.
    //
    hdcImage = CreateCompatibleDC(NULL);
    if (!hdcImage)
        OutputDebugString("createcompatible dc failed\n");
    SelectObject(hdcImage, hbm);
#else // __linux__
    // memory DC left out: QPainter draws from the QImage directly
#endif // __linux__
    //
    // Get size of the bitmap
    //
#ifndef __linux__
    GetObject(hbm, sizeof(bm), &bm);
    dx = dx == 0 ? bm.bmWidth : dx;     // Use the passed size, unless zero
    dy = dy == 0 ? bm.bmHeight : dy;
#else // __linux__
    dx = dx == 0 ? hbm->width() : dx;     // Use the passed size, unless zero
    dy = dy == 0 ? hbm->height() : dy;
#endif // __linux__
    //
    // Get size of surface.
    //
	clbkGetSurfaceSize (pdds, &surfW, &surfH);
    //ddsd.dwSize = sizeof(ddsd);
    //ddsd.dwFlags = DDSD_HEIGHT | DDSD_WIDTH;
    //pdds->GetSurfaceDesc(&ddsd);

#ifndef __linux__
	if (hdc = clbkGetSurfaceDC (pdds)) {
        StretchBlt(hdc, 0, 0, surfW, surfH, hdcImage, x, y,
                   dx, dy, SRCCOPY);
#else // __linux__
	if ((hdc = clbkGetSurfaceDC (pdds))) {
		hdc->save ();
		hdc->setCompositionMode (QPainter::CompositionMode_Source); // SRCCOPY
		hdc->drawImage (QRect (0, 0, surfW, surfH), *hbm, QRect (x, y, dx, dy)); // StretchBlt
		hdc->restore ();
#endif // __linux__
		clbkReleaseSurfaceDC (pdds, hdc);
    }
#ifndef __linux__
	DeleteDC(hdcImage);
#endif // !__linux__
    return true;
}

// ======================================================================

#ifndef __linux__
HWND GraphicsClient::InitRenderWnd (HWND hWnd)
#else // __linux__
QWindow *GraphicsClient::InitRenderWnd (QWindow *hWnd)
#endif // __linux__
{
	if (!hWnd) { // create a dummy window
#ifndef __linux__
		hWnd = CreateWindow (strWndClass, "",
			WS_POPUP | WS_VISIBLE,
			CW_USEDEFAULT, CW_USEDEFAULT, 10, 10, 0, 0, hModule, 0);
#else // __linux__
		hWnd = new QWindow;
		hWnd->setObjectName (strWndClass);
		hWnd->setFlags (Qt::Window | Qt::FramelessWindowHint); // WS_POPUP
		hWnd->resize (10, 10);
		hWnd->show ();
#endif // __linux__
	}
#ifndef __linux__
	SetWindowLongPtr (hWnd, GWLP_USERDATA, (LONG_PTR)this);
#else // __linux__
	new RenderWndHook (hWnd, this);
#endif // __linux__
	// store class instance with window for access in the message handler

	char title[256], cbuf[128];
#ifndef __linux__
	extern const TCHAR *g_strAppTitle;
#endif // !__linux__
	strcpy (title, g_strAppTitle);
#ifndef __linux__
	GetWindowText (hWnd, cbuf, 128);
#else // __linux__
	strncpy (cbuf, hWnd->title().toUtf8().constData(), 127); cbuf[127] = '\0'; // GetWindowText
#endif // __linux__
	if (cbuf[0]) {
		strcat (title, " ");
		strcat (title, cbuf);
	}
#ifndef __linux__
	SetWindowText (hWnd, title);
#else // __linux__
	hWnd->setTitle (QString::fromUtf8 (title)); // SetWindowText
#endif // __linux__
	hRenderWnd = hWnd;
	return hRenderWnd;
}

// ======================================================================


#ifndef __linux__
LRESULT GraphicsClient::RenderWndProc (HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
#else // __linux__
bool GraphicsClient::RenderWndProc (QWindow *hWnd, QEvent *event)
#endif // __linux__
{
#ifndef __linux__
	switch (uMsg) {
#else // __linux__
	switch (event->type()) {
#endif // __linux__
	// graphics-specific stuff to go here
	default:
#ifndef __linux__
		return g_pOrbiter->MsgProc (hWnd, uMsg, wParam, lParam);
#else // __linux__
		return g_pOrbiter->MsgProc (hWnd, event);
#endif // __linux__
	}
#ifndef __linux__
    return DefWindowProc (hWnd, uMsg, wParam, lParam);
#else // __linux__
    return false; // DefWindowProc: Qt's default handling
#endif // __linux__
}

// ======================================================================

#ifndef __linux__
INT_PTR GraphicsClient::LaunchpadVideoWndProc (HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
#else // __linux__
void GraphicsClient::LaunchpadVideoWndProc (QWidget *hWnd)
#endif // __linux__
{
#ifndef __linux__
	return FALSE;
#endif // !__linux__
}

// ==================================================================
// Functions for the celestial sphere

const std::vector<GraphicsClient::LABELLIST>& GraphicsClient::GetCelestialMarkers() const
{
	return g_psys->LabelList();
}

// ==================================================================

DWORD GraphicsClient::GetSurfaceMarkers (OBJHANDLE hObj, const LABELLIST **sm_list) const
{
	int nlist;
	*sm_list = ((Planet*)hObj)->LabelList (&nlist);
	return (DWORD)nlist;
}

// ==================================================================

DWORD GraphicsClient::GetSurfaceMarkerLegend (OBJHANDLE hObj, const LABELTYPE **lspec) const
{
	Planet *planet = (Planet*)hObj;
	*lspec = planet->LabelLegend();
	return planet->NumLabelLegend();
}

// ======================================================================
// ======================================================================
// class ParticleStream

ParticleStream::ParticleStream (GraphicsClient *_gc, PARTICLESTREAMSPEC *pss)
{
	gc = _gc;
	level = NULL;
	hRef = NULL;
	lpos = _V(0,0,0);  pos = &lpos;
	ldir = _V(0,0,0);  dir = &ldir;
}

ParticleStream::~ParticleStream ()
{
	if (hRef) // stream is being deleted while still attached
		((Vessel*)hRef)->DelParticleStream (this);
		// notify vessel
}

void ParticleStream::Attach (OBJHANDLE hObj, const VECTOR3 *ppos, const VECTOR3 *pdir, const double *srclvl)
{
	hRef = hObj;
	SetVariablePos (ppos);
	SetVariableDir (pdir);
	SetLevelPtr (srclvl);
}

void ParticleStream::Attach (OBJHANDLE hObj, const VECTOR3 &_pos, const VECTOR3 &_dir, const double *srclvl)
{
	hRef = hObj;
	SetFixedPos (_pos);
	SetFixedDir (_dir);
	SetLevelPtr (srclvl);
}

void ParticleStream::Detach ()
{
	level = NULL;
	hRef = NULL;
	pos = &lpos;
	dir = &ldir;
}

void ParticleStream::SetFixedPos (const VECTOR3 &_pos)
{
	lpos = _pos;  pos = &lpos;
}

void ParticleStream::SetFixedDir (const VECTOR3 &_dir)
{
	ldir = _dir;  dir = &ldir;
}

void ParticleStream::SetVariablePos (const VECTOR3 *ppos)
{
	pos = ppos;
}

void ParticleStream::SetVariableDir (const VECTOR3 *pdir)
{
	dir = pdir;
}

void ParticleStream::SetLevelPtr (const double *srclvl)
{
	level = srclvl;
}


// ======================================================================
// ======================================================================
// class ScreenAnnotation

ScreenAnnotation::ScreenAnnotation (GraphicsClient *_gc)
{
	gc = _gc;
	_gc->clbkGetViewportSize (&viewW, &viewH);
	txt = 0;
	buflen = 0;
	txtscale = 0;
	font = NULL;
	//hFont = NULL;
	Reset();
}

ScreenAnnotation::~ScreenAnnotation ()
{
	if (txt) delete[]txt;
	if (font) gc->clbkReleaseFont (font);
	//DeleteObject (hFont);
}

void ScreenAnnotation::Reset ()
{
	ClearText();
	SetPosition (0.1, 0.1, 0.5, 0.6);
	SetColour (_V(1.0,0.7,0.2));
	SetSize (1.0);
}

void ScreenAnnotation::SetPosition (double x1, double y1, double x2, double y2)
{
	nx1 = (int)(x1*viewW); nx2 = (int)(x2*viewW); nw = nx2-nx1;
	ny1 = (int)(y1*viewH); ny2 = (int)(y2*viewH); nh = ny2-ny1;
}

void ScreenAnnotation::SetText (char *str)
{
	int len = strlen (str)+1;
	if (len > buflen) {
		if (txt) delete []txt;
		txt = new char[len]; TRACENEW
		buflen = len;
	}
	strcpy (txt, str);
	txtlen = len-1;
	for (int i = 0; i < txtlen-1; i++)
		if (txt[i] == ' ' && txt[i+1] == ' ') {
			txt[i] = '\r';
			txt[i+1] = '\n';
		}
}

void ScreenAnnotation::ClearText ()
{
	txtlen = 0;
}

void ScreenAnnotation::SetSize (double scale)
{
	if (scale != txtscale) {
		txtscale = scale;
		hf = (int)(viewH*txtscale/35.0);
		if (font) gc->clbkReleaseFont (font);
		font = gc->clbkCreateFont (-hf, true, "Sans");
	}
}

void ScreenAnnotation::SetColour (const VECTOR3 &col)
{
	int r = min (255, (int)(col.x*256.0));
	int g = min (255, (int)(col.y*256.0));
	int b = min (255, (int)(col.z*256.0));
	txtcol  = r | (g << 8) | (b << 16);
	txtcol2 = (r/2) | ((g/2) << 8) | ((b/2) << 16);
}

void ScreenAnnotation::Render ()
{
	if (!txtlen) return;

	Sketchpad *skp = gc->clbkGetSketchpad (0);
	if (skp) {
		skp->SetFont (font);
		skp->SetTextColor (txtcol2);
		skp->SetBackgroundMode (oapi::Sketchpad::BK_TRANSPARENT);
		skp->TextBox (nx1+1, ny1+1, nx2+1, ny2+1, txt, txtlen);
		skp->SetTextColor (txtcol);
		skp->TextBox (nx1, ny1, nx2, ny2, txt, txtlen);
		gc->clbkReleaseSketchpad (skp);
	}
}

// ======================================================================
// Nonmember functions

#ifndef __linux__
//-----------------------------------------------------------------------
// Name: WndProc()
// Desc: Static msg handler which passes messages from the render window
//       to the application class.
//-----------------------------------------------------------------------
DLLEXPORT LRESULT CALLBACK WndProc (HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	GraphicsClient *gc = (GraphicsClient*)GetWindowLongPtr (hWnd, GWLP_USERDATA);
	if (gc) return gc->RenderWndProc (hWnd, uMsg, wParam, lParam);
	else return DefWindowProc (hWnd, uMsg, wParam, lParam);
}

// ======================================================================

DLLEXPORT INT_PTR CALLBACK LaunchpadVideoWndProc (HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	GraphicsClient *gc = (GraphicsClient*)GetWindowLongPtr (hWnd, GWLP_USERDATA);
	if (gc) return gc->LaunchpadVideoWndProc (hWnd, uMsg, wParam, lParam);
	else return FALSE;
}
#else // __linux__
// WndProc and LaunchpadVideoWndProc exports left out: RenderWndHook and the video tab call the client directly
#endif // __linux__

// ======================================================================
// API interface: register/unregister the graphics client

DLLEXPORT bool oapiRegisterGraphicsClient (GraphicsClient *gc)
{
	return g_pOrbiter->AttachGraphicsClient (gc);
}

DLLEXPORT bool oapiUnregisterGraphicsClient (GraphicsClient *gc)
{
	return g_pOrbiter->RemoveGraphicsClient (gc);
}
