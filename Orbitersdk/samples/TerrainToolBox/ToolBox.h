
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

#define IDC_CALLBACK	4000
#define IDC_LIGHTNESS	2000
#define IDC_BRIGHTNESS	2001
#define IDC_GAMMA		2002
#define IDC_ALPHA		2003
#define IDC_RED			2004
#define IDC_GREEN		2005
#define IDC_BLUE		2006

#define IDC_PINX		2008
#define IDC_PINY		2009
#define IDC_PINW		2010
#define IDC_PINN		2011
#define IDC_PINF		2012

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
	const int WTreeOnly = 0x1000;
};

#define MODE_EXPORT		0
#define MODE_IMPORT		1


struct selentry {
	QTree *pNode;
	int x, y;
};

struct Position {
	double lng, lat;
	double elev;
	FMATRIX4 mWorld;
};


class ToolKit : public gcGUIApp, public oapi::Module
{
	struct sSelection
	{
		vector<selentry> area;
		DRECT bounds;
		int	selw, selh, slvl;
		QTree *pLeftTop;
		QTree *pRightBottom;
	};

	struct Pin {
		double lng, lat;
		int x, y;
	};

public:

				ToolKit(HINSTANCE hDLL);
				~ToolKit();

	bool		SaveFile(OPENFILENAMEA &SaveImage);
	void		Export();
	void		ExportElev();
	void		ImportImage();
	void		UpdateOverlay();
	void		ComputeLevel();
	bool		Initialize();
	void		SetMode(int mode);
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
	void		RenderSelection(sSelection *sel, int mode, DWORD color);
	void		RenderTileBounds(QTree *tn, DWORD color);
	void		RenderTileBounds(gcCore::PickGround &pg, DWORD color);
	void		DrawBox(FVECTOR3 *box, DWORD color);

				// ------------------------------------------------------ 

	SURFHANDLE GetBaseElevation(int elev_fmt);
	VECTOR3		GetSurfacePosUnit(double lng, double lat);
	FMATRIX4	CreateWorldMatrix(OBJHANDLE hPlanet, double lng, double lat, double elev, float scale);
	void		UpdateTileInfo(QTree *pF, gcCore::PickGround *pG);
	int			SelectedLevel();
	void		AutoSelectCorners();
	void		BakeImport();
	void		StopImport();
	bool		CreatePin(Pin &pn, DRECT bounds);
	void		BakeParents(SURFHANDLE hTemp, list<QTree *> parents);
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
	SURFHANDLE			hOverlaySrf, hSource, hOverlayBkg;
	gcPropertyTree			*pProp;
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
	HPROP				hSecGfx, hSecCur, hSecExp, hSecImp, hSecClr, hSecPin;

	HPROP				hCLng, hCLat, hCEle, hCFil, hSLng, hSLat, hMLng, hMLat, hSLvl, hSXPx, hSYPx;
	HPROP				hIFil, hIWid, hIHei, hILog, hILT, hILB, hIRB, hIRT;
	HPROP				hGLig, hGBri, hGGam, hGEdg;
	HPROP				hBRed, hBGrn, hBBlu;
	HPROP				hPinX, hPinY, hPinW, hPinN, hPinF;

	HWND				hProgDlg;

	HWND				hCtrlDlg;
	HWND				hDataDlg;
	HWND				hImpoDlg;
	HWND				hAppMainWnd;

	HNODE				hRootNode;
	HNODE				hCtrlNode;
	HNODE				hDataNode;
	HNODE				hImpoNode;
	HNODE				hExpoNode;

	DWORD				dwCmd;
	bool				bGo;
};
