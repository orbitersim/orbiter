
#include <list>
#include <vector>
#include <windows.h>
#include <windowsx.h>
#include "OrbiterAPI.h"
#include "VesselAPI.h"
#include "ModuleAPI.h"
#include "DrawAPI.h"
#include "gcCoreAPI.h"
#include "gcGUI.h"
#include "gcPropertyTree.h"
#include "QTree.h"

#pragma once

#define IDC_CALLBACK	3000
#define IDC_LIGHTNESS	2000
#define IDC_BRIGHTNESS	2001
#define IDC_GAMMA		2002
#define IDC_ALPHA		2003
#define IDC_RED			2004
#define IDC_GREEN		2005
#define IDC_BLUE		2006
#define IDC_LAYER		2007
#define IDC_ALPHAEDG	2008


namespace Select
{
	// -----------------------
	const int SRender = 0;
	const int SHighestOwn = 1;
	// -----------------------
	const int WTexture = 0;
	const int WNightlight = 1;
	const int WElevation = 2;
	// -----------------------
};

#define MODE_EXPORT		0
#define MODE_IMPORT		1



struct selentry {
	QTree *pNode;
	int x, y; // Index (position) in the selection
};


struct Position {
	double lng, lat;
	double elev;
	FMATRIX4 mWorld;
};


class Layer 
{	
public:

	enum class LayerType { TEXTURE = 0, NIGHT = 1, WATER = 2, ELEVATION = 3 };
	enum class LrFmt { UNDEF, DDS, RAW_U16, RAW_S16, RAW_F32 };

				Layer(gcPropertyTree *pProp, gcCore2 *pCore, SURFHANDLE hSr, LayerType type, string name);
				~Layer();

	void		ComputeLevel(Position points[4]);
	void		UpdateProps();
	LrFmt		GetFormat();
	bool		GetTransparency();
	int			GetWaterMode();
	FVECTOR4	GetColor();
	FVECTOR4	GetAdjustments();
	double		Max(double a, double b, double c, double d);
		double		Min(double a, double b, double c, double d);

	SURFHANDLE hSource;
	string name;
	WORD width, height;
	double lvl;
	bool bAlpha;
	LrFmt fmt;
	LayerType type;

	HPROP hSecGfx, hSecClr;
	HPROP hIFil, hIWid, hIHei, hIFmt, hILog, hISec, hIRes, hIRef, hITrs, hIWtr;
	HPROP hGLig, hGBri, hGGam;
	HPROP hBRed, hBGrn, hBBlu;

	gcPropertyTree* pProp;
	gcCore2* pCore;
};


class ToolKit : public gcGUIApp, public oapi::Module
{
	struct sSelection
	{
		vector<selentry> area;	// List of selected tiles 
		DRECT bounds;			// Selection bounds (min, max) * (lng, lat)
		int	selw, selh, slvl;	// Number of tiles selected (w,h) and a level 
		QTree *pLeftTop;
		QTree *pRightBottom;
	};

public:

				ToolKit(HINSTANCE hDLL);
				~ToolKit();

	bool		SaveFile(OPENFILENAMEA &SaveImage);
	void		Export();
	void		ExportElev();
	void		OpenImage(Layer::LayerType lr);
	bool		UpdateOverlay(int olay);
	bool		UpdateOverlays();
	bool		Initialize();
	bool		IsLayerValid(Layer::LayerType lr);
	void		SetupPlanet(OBJHANDLE hPln);
	BOOL		DlgProc(HWND hDlg, UINT uMsg, WPARAM wParam, LPARAM lParam);

				// From gcGUIApp
	void		clbkShutdown();
	bool		clbkMessage(DWORD uMsg, HNODE hNode, int data);

				// From oapi::Module			
	void		clbkSimulationStart(RenderMode rm);
	void		clbkSimulationEnd();
	void		clbkPreStep(double simt, double simdt, double mjd);
	bool		clbkProcessMouse(UINT event, DWORD state, DWORD x, DWORD y);
	bool		clbkProcessKeyboardBuffered(DWORD key, char kstate[256], bool simRunning);

				// From D3D9Client
	void		clbkMouseClick(int iUser, void *pData);
	void		clbkRender();

private:
	void		RenderSelection(list<QTree*> sel, int mode, DWORD color);
	void		RenderSelection(sSelection *sel, int mode, DWORD color);
	void		RenderTileBounds(QTree *tn, DWORD color);
	void		RenderTileBounds(gcCore::PickGround &pg, DWORD color);
	void		DrawBox(FVECTOR3 *box, DWORD color);

	// ------------------------------------------------------ 
	DRECT		GetBounds(list<QTree*> sel);
	bool		CreateOverlays();
	bool		UpdateBackGround(SURFHANDLE hSrf, DWORD flags);
	Layer*		GetLayer(Layer::LayerType type) { return pLr[(int)type]; }
	SURFHANDLE	GetBaseElevation(int elev_fmt);
	VECTOR3		GetSurfacePosUnit(double lng, double lat);
	FMATRIX4	CreateWorldMatrix(OBJHANDLE hPlanet, double lng, double lat, double elev, float scale);
	void		UpdateTileInfo(int flags, QTree *pF, gcCore::PickGround *pG);
	int			SelectedLevel();
	int			SelectionFlags();
	void		AutoSelectCorners();
	void		BakeImport();
	void		StopImport();
	void		BakeParents(SURFHANDLE hOvrl, SURFHANDLE hTemp, int flags, list<QTree *> parents, int levels);
	void		MakeProgress();


	sSelection			selection;
	sSelection			oldsel;
	OPENFILENAMEA		SaveImage;
	OPENFILENAMEA		SaveDDS;
	OPENFILENAMEA		SaveElevation;
	char				SaveFileName[MAX_PATH];
	char				SaveFileTitle[MAX_PATH];

	gcCore::SystemSpecs	sp;
	Position			points[4];
	FMATRIX4			mIdent;
	HPLANETMGR			hMgr;
	DEVMESHHANDLE		dmSphere;
	OBJHANDLE			hPlanet;
	HOVERLAY			hOverlay;
	SURFHANDLE			hGradient;
	SURFHANDLE			hOverlaySrf, hOverlayMsk, hOverlayElv;
	Layer				*pLr[4];
	gcPropertyTree		*pProp;
	gcCore2				*pCore;
	QTree				*pRoot;
	QTree				*pFirst, *pSecond;
	
	int					xpos, ypos;
	int					selw, selh;		// Selection size in tiles
	int					sel_corner;
	int					down_corner;
	int					progress;


	//	Dialogs and User Interface ------------------------------
	//
	HPROP				hSecCur, hSecExp;

	HPROP				hCLng, hCLat, hCEle, hCFil, hCLvl, hBLvs, hCEdg, hSLng, hSLat, hMLng, hMLat, hSXPx, hSYPx;
	HPROP				hIFil, hIWid, hIHei, hILog;

	

	HWND				hProgDlg;

	HWND				hMainDlg;
	HWND				hCtrlDlg;
	HWND				hDataDlg;
	HWND				hImpoDlg;
	HWND				hAppMainWnd;

	HNODE				hRootNode;
	HNODE				hMainNode;
	HNODE				hCtrlNode;
	HNODE				hDataNode;
	HNODE				hImpoNode;
	HNODE				hExpoNode;

	DWORD				dwCmd;
	bool				bGo;
	bool				bImport;	// Import in progress
};
