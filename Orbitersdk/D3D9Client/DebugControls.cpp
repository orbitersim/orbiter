// ==============================================================
// DebugControls implementation
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// Released under GNU General Public License
// Copyright (C) 2012 Jarmo Nikkanen
// ==============================================================


#include "D3D9Client.h"
#include "resource.h"
#include "D3D9Config.h"
#include "D3D9Surface.h"
#include "DebugControls.h"
#include "Commctrl.h"
#include "vObject.h"
#include "vVessel.h"
#include "Mesh.h"
#include "MaterialMgr.h"
#include <stdio.h>

using namespace oapi;

extern HINSTANCE g_hInst;
extern D3D9Client *g_client;

// Little binary helper
#define SETFLAG(bitmap, bit, value) (value ? bitmap |= bit : bitmap &= ~bit)

namespace DebugControls {

DWORD dwCmd, nMesh, nGroup, sMesh, sGroup, debugFlags, dspMode, camMode, SelColor;
double camSpeed;
char visual[64];

HWND hDlg = NULL;
vObject *vObj = NULL;

void UpdateMaterialDisplay(bool bSetup=false);

// ===========================================================================
// Same functionality than 'official' GetConfigParam, but for non-provided
// debug-control config parameters
//
const void *GetConfigParam (DWORD paramtype)
{
	switch (paramtype) {
		case CFGPRM_GETSELECTEDMESH  : return (void*)&sMesh;
		case CFGPRM_GETSELECTEDGROUP : return (void*)&sGroup;
		case CFGPRM_GETDEBUGFLAGS    : return (void*)&debugFlags;
		case CFGPRM_GETDISPLAYMODE   : return (void*)&dspMode;
		case CFGPRM_GETCAMERAMODE    : return (void*)&camMode;
		case CFGPRM_GETCAMERASPEED   : return (void*)&camSpeed;
		default                      : return NULL;
	}
}

void Create()
{
	vObj = NULL;
	hDlg = NULL;
	nMesh = 0;
	nGroup = 0;
	sMesh = 0;
	sGroup = 0;
	debugFlags = 0;
	camSpeed = 0.5;
	camMode = 0;
	dspMode = 0;
	SelColor = 0;
	dwCmd = oapiRegisterCustomCmd("D3D9 Debug Controls", "This dialog allows to control various debug controls", OpenDlgClbk, NULL);
}

bool IsActive()
{
	return (hDlg!=NULL);
}

void Release()
{
	vObj=NULL;
	hDlg=NULL;
	if (dwCmd) oapiUnregisterCustomCmd(dwCmd);
	dwCmd = NULL;
}

void UpdateFlags()
{
	SETFLAG(debugFlags, DBG_FLAGS_SELGRPONLY,	(SendDlgItemMessageA(hDlg, IDC_DBG_GRPO, BM_GETCHECK, 0, 0)==BST_CHECKED));
	SETFLAG(debugFlags, DBG_FLAGS_SELMSHONLY,	(SendDlgItemMessageA(hDlg, IDC_DBG_MSHO, BM_GETCHECK, 0, 0)==BST_CHECKED));
	SETFLAG(debugFlags, DBG_FLAGS_BOXES,		(SendDlgItemMessageA(hDlg, IDC_DBG_BOXES, BM_GETCHECK, 0, 0)==BST_CHECKED));
	SETFLAG(debugFlags, DBG_FLAGS_SPHERES,		(SendDlgItemMessageA(hDlg, IDC_DBG_SPHERES, BM_GETCHECK, 0, 0)==BST_CHECKED));
	SETFLAG(debugFlags, DBG_FLAGS_HLMESH,		(SendDlgItemMessageA(hDlg, IDC_DBG_HSM, BM_GETCHECK, 0, 0)==BST_CHECKED));
	SETFLAG(debugFlags, DBG_FLAGS_HLGROUP,		(SendDlgItemMessageA(hDlg, IDC_DBG_HSG, BM_GETCHECK, 0, 0)==BST_CHECKED));
	SETFLAG(debugFlags, DBG_FLAGS_TILES,		(SendDlgItemMessageA(hDlg, IDC_DBG_TILES, BM_GETCHECK, 0, 0)==BST_CHECKED));
	SETFLAG(debugFlags, DBG_FLAGS_SELVISONLY,	(SendDlgItemMessageA(hDlg, IDC_DBG_VISO, BM_GETCHECK, 0, 0)==BST_CHECKED));
	SETFLAG(debugFlags, DBG_FLAGS_AMBIENT,	    (SendDlgItemMessageA(hDlg, IDC_DBG_AMBIENT, BM_GETCHECK, 0, 0)==BST_CHECKED));
	SETFLAG(debugFlags, DBG_FLAGS_WIREFRAME,	(SendDlgItemMessageA(hDlg, IDC_DBG_WIRE, BM_GETCHECK, 0, 0)==BST_CHECKED));
	SETFLAG(debugFlags, DBG_FLAGS_DUALSIDED,	(SendDlgItemMessageA(hDlg, IDC_DBG_DUAL, BM_GETCHECK, 0, 0)==BST_CHECKED));
	SETFLAG(debugFlags, DBG_FLAGS_DSPENVMAP,	(SendDlgItemMessageA(hDlg, IDC_DBG_ENVMAP, BM_GETCHECK, 0, 0)==BST_CHECKED));
	SETFLAG(debugFlags, DBG_FLAGS_PICK,			(SendDlgItemMessageA(hDlg, IDC_DBG_PICK, BM_GETCHECK, 0, 0)==BST_CHECKED));
}

void SetGroupHighlight(bool bStat)
{
	SETFLAG(debugFlags, DBG_FLAGS_HLGROUP, bStat);
}

void OpenDlgClbk(void *context)
{
	DWORD idx = 0;
	HWND l_hDlg = oapiOpenDialog(g_hInst, IDD_D3D9MESHDEBUG, WndProc);

	if (l_hDlg) hDlg = l_hDlg; // otherwise open already
	else return;

	SendDlgItemMessageA(hDlg, IDC_DBG_DISPLAY, CB_RESETCONTENT, 0, 0);
	SendDlgItemMessageA(hDlg, IDC_DBG_DISPLAY, CB_ADDSTRING, 0, (LPARAM)"Everything");
	SendDlgItemMessageA(hDlg, IDC_DBG_DISPLAY, CB_ADDSTRING, 0, (LPARAM)"Selected Visual");
	SendDlgItemMessageA(hDlg, IDC_DBG_DISPLAY, CB_ADDSTRING, 0, (LPARAM)"Selected Mesh");
	SendDlgItemMessageA(hDlg, IDC_DBG_DISPLAY, CB_ADDSTRING, 0, (LPARAM)"Selected Group");
	SendDlgItemMessageA(hDlg, IDC_DBG_DISPLAY, CB_SETCURSEL, 0, 0);

	SendDlgItemMessageA(hDlg, IDC_DBG_CAMERA, CB_RESETCONTENT, 0, 0);
	SendDlgItemMessageA(hDlg, IDC_DBG_CAMERA, CB_ADDSTRING, 0, (LPARAM)"Center on visual");
	SendDlgItemMessageA(hDlg, IDC_DBG_CAMERA, CB_ADDSTRING, 0, (LPARAM)"Wheel Fly/Pan Cam");
	SendDlgItemMessageA(hDlg, IDC_DBG_CAMERA, CB_SETCURSEL, 0, 0);

	SendDlgItemMessageA(hDlg, IDC_DBG_MATPRP, CB_RESETCONTENT, 0, 0);
	SendDlgItemMessageA(hDlg, IDC_DBG_MATPRP, CB_ADDSTRING, 0, (LPARAM)"Diffuse");
	SendDlgItemMessageA(hDlg, IDC_DBG_MATPRP, CB_ADDSTRING, 0, (LPARAM)"Ambient");
	SendDlgItemMessageA(hDlg, IDC_DBG_MATPRP, CB_ADDSTRING, 0, (LPARAM)"Specular");
	SendDlgItemMessageA(hDlg, IDC_DBG_MATPRP, CB_ADDSTRING, 0, (LPARAM)"Emission");
	SendDlgItemMessageA(hDlg, IDC_DBG_MATPRP, CB_ADDSTRING, 0, (LPARAM)"Reflect");
	SendDlgItemMessageA(hDlg, IDC_DBG_MATPRP, CB_ADDSTRING, 0, (LPARAM)"Dissolve");
	SendDlgItemMessageA(hDlg, IDC_DBG_MATPRP, CB_SETCURSEL, 0, 0);

	SendDlgItemMessageA(hDlg, IDC_DBG_MATEFF, CB_RESETCONTENT, 0, 0);
	SendDlgItemMessageA(hDlg, IDC_DBG_MATEFF, CB_ADDSTRING, 0, (LPARAM)"None");
	while (true) {
		SURFHANDLE hSrf = g_client->GetDissolveMap(idx++);
		if (hSrf) SendDlgItemMessageA(hDlg, IDC_DBG_MATEFF, CB_ADDSTRING, 0, (LPARAM)SURFACE(hSrf)->GetName());
		else break;
	}
	SendDlgItemMessageA(hDlg, IDC_DBG_MATEFF, CB_SETCURSEL, 0, 0);


	// Speed slider
	SendDlgItemMessage(hDlg, IDC_DBG_SPEED, TBM_SETRANGEMAX, 1, 200);
	SendDlgItemMessage(hDlg, IDC_DBG_SPEED, TBM_SETRANGEMIN, 1, 1);
	SendDlgItemMessage(hDlg, IDC_DBG_SPEED, TBM_SETTICFREQ,  1, 0);
	SendDlgItemMessage(hDlg, IDC_DBG_SPEED, TBM_SETPOS,  1, 75);
	SetWindowTextA(GetDlgItem(hDlg, IDC_DBG_SPEEDDSP), "29");

	// Meterial slider
	SendDlgItemMessage(hDlg, IDC_DBG_MATADJ, TBM_SETRANGEMAX, 1, 255);
	SendDlgItemMessage(hDlg, IDC_DBG_MATADJ, TBM_SETRANGEMIN, 1, 0);
	SendDlgItemMessage(hDlg, IDC_DBG_MATADJ, TBM_SETTICFREQ,  1, 0);
	SendDlgItemMessage(hDlg, IDC_DBG_MATADJ, TBM_SETPOS,  1, 0);
	

	camMode = 0;
	dspMode = 0;

	OBJHANDLE hTgt = oapiCameraTarget();

	SetVisual(g_client->GetScene()->GetVisObject(hTgt));	// This will call SetupMeshGroups()

	UpdateFlags();
}

void UpdateDissolveMap(SURFHANDLE hSrf)
{
	OBJHANDLE hObj = vObj->GetObjectA();

	if (!oapiIsVessel(hObj)) return;

	D3D9Mesh *hMesh = (D3D9Mesh *)vObj->GetMesh(sMesh);

	if (!hMesh) return;

	DWORD matidx = hMesh->GetMeshGroupMaterialIdx(sGroup);
	D3DMATERIAL9 *pMat = hMesh->GetMaterial(matidx);
	D3D9MatExt * pMatE = hMesh->GetMaterialExtension(matidx);

	if (!pMat || !pMatE) return;

	pMatE->pDissolve = hSrf;
	pMatE->ModFlags |= D3D9MATEX_DISSOLVE;

	vVessel *vVes = (vVessel *)vObj;

	vVes->GetMaterialManager()->RegisterMaterialChange(hMesh, matidx, pMat, pMatE); 
}


void UpdateMeshMaterial(float value, DWORD MatPrp, DWORD clr)
{
	OBJHANDLE hObj = vObj->GetObjectA();

	if (!oapiIsVessel(hObj)) return;

	D3D9Mesh *hMesh = (D3D9Mesh *)vObj->GetMesh(sMesh);

	if (!hMesh) return;

	DWORD matidx = hMesh->GetMeshGroupMaterialIdx(sGroup);
	D3DMATERIAL9 *pMat = hMesh->GetMaterial(matidx);
	D3D9MatExt * pMatE = hMesh->GetMaterialExtension(matidx);

	if (!pMat || !pMatE) return;

	switch(MatPrp) {

		case 0:	// Diffuse
		{
			pMatE->ModFlags |= D3D9MATEX_DIFFUSE;
			switch(clr) {
				case 0: pMat->Diffuse.r = value; break;
				case 1: pMat->Diffuse.g = value; break;
				case 2: pMat->Diffuse.b = value; break;
				case 3: pMat->Diffuse.a = value; break;
			}
			break;
		}

		case 1:	// Ambient
		{
			pMatE->ModFlags |= D3D9MATEX_AMBIENT;
			switch(clr) {
				case 0: pMat->Ambient.r = value; break;
				case 1: pMat->Ambient.g = value; break;
				case 2: pMat->Ambient.b = value; break;
				case 3: pMat->Ambient.a = 0.0; break;
			}
			break;
		}

		case 2:	// Specular
		{
			pMatE->ModFlags |= D3D9MATEX_SPECULAR;
			switch(clr) {
				case 0: pMat->Specular.r = value; break;
				case 1: pMat->Specular.g = value; break;
				case 2: pMat->Specular.b = value; break;
				case 3: pMat->Power = value; break;
			}
			break;
		}

		case 3:	// Emission
		{
			pMatE->ModFlags |= D3D9MATEX_EMISSIVE;
			switch(clr) {
				case 0: pMat->Emissive.r = value; break;
				case 1: pMat->Emissive.g = value; break;
				case 2: pMat->Emissive.b = value; break;
				case 3: pMat->Emissive.a = 0.0; break;
			}
			break;
		}

		case 4:	// Reflectivity
		{
			pMatE->ModFlags |= D3D9MATEX_REFLECT;
			switch(clr) {
				case 0: pMatE->Reflect = value;	break;
				case 1: pMatE->Glass = value; break;
				
			}
			break;
		}

		case 5:	// Dissolve
		{
			pMatE->ModFlags |= D3D9MATEX_DISSOLVE;
			switch(clr) {
				case 0: pMatE->DissolveScl = value; break;
				case 1: pMatE->DissolveSct = value; break;
			}
			break;
		}
	}


	vVessel *vVes = (vVessel *)vObj;
	vVes->GetMaterialManager()->RegisterMaterialChange(hMesh, matidx, pMat, pMatE); 
}


float GetMaterialValue(DWORD MatPrp, DWORD clr)
{
	OBJHANDLE hObj = vObj->GetObjectA();

	if (!oapiIsVessel(hObj)) return 0.0f;

	D3D9Mesh *hMesh = (D3D9Mesh *)vObj->GetMesh(sMesh);

	if (!hMesh) return 0.0f;

	DWORD matidx = hMesh->GetMeshGroupMaterialIdx(sGroup);
	D3DMATERIAL9 *pMat = hMesh->GetMaterial(matidx);
	D3D9MatExt * pMatE = hMesh->GetMaterialExtension(matidx);

	if (!pMat || !pMatE) return 0.0f;

	switch(MatPrp) {

		case 0:	// Diffuse
		{
			switch(clr) {
				case 0: return pMat->Diffuse.r;
				case 1: return pMat->Diffuse.g;
				case 2: return pMat->Diffuse.b;
				case 3: return pMat->Diffuse.a;
			}
			break;
		}

		case 1:	// Ambient
		{
			switch(clr) {
				case 0: return pMat->Ambient.r;
				case 1: return pMat->Ambient.g;
				case 2: return pMat->Ambient.b;
				case 3: return pMat->Ambient.a;
			}
			break;
		}

		case 2:	// Specular
		{
			switch(clr) {
				case 0: return pMat->Specular.r;
				case 1: return pMat->Specular.g;
				case 2: return pMat->Specular.b;
				case 3: return pMat->Power;
			}
			break;
		}

		case 3:	// Emission
		{
			switch(clr) {
				case 0: return pMat->Emissive.r;
				case 1: return pMat->Emissive.g;
				case 2: return pMat->Emissive.b;
				case 3: return pMat->Emissive.a;
			}
			break;
		}

		case 4:	// Reflectivity
		{
			switch(clr) {
				case 0: return pMatE->Reflect;
				case 1: return pMatE->Glass;
			}
			break;
		}

		case 5:	// Dissolve
		{
			switch(clr) {
				case 0: return pMatE->DissolveScl;
				case 1: return pMatE->DissolveSct;
			}
			break;
		}
	}

	return 0.0f;
}



void SetColorSlider()
{
	DWORD MatPrp = SendDlgItemMessageA(hDlg, IDC_DBG_MATPRP, CB_GETCURSEL, 0, 0);

	float val = GetMaterialValue(MatPrp, SelColor);
	
	if (MatPrp==2 && SelColor==3) val/=80.0; // Specular Power
	if (MatPrp==5 && SelColor==0) val/=12.0;  // Dissolve scale
	if (MatPrp==5 && SelColor==1) val/=0.2f;  // Dissolve scatter
	if (MatPrp==4 && SelColor==1) val/=2.0;  // Glass reflect
	
	SendDlgItemMessage(hDlg, IDC_DBG_MATADJ, TBM_SETPOS,  1, WORD(val*255.0f));
}


void DisplayMat(bool bRed, bool bGreen, bool bBlue, bool bAlpha)
{
	char lbl[32];

	DWORD MatPrp = SendDlgItemMessageA(hDlg, IDC_DBG_MATPRP, CB_GETCURSEL, 0, 0);

	float r = GetMaterialValue(MatPrp, 0);
	float g = GetMaterialValue(MatPrp, 1);
	float b = GetMaterialValue(MatPrp, 2);
	float a = GetMaterialValue(MatPrp, 3);

	if (bRed) sprintf_s(lbl,32,"%2.2f", r);
	else	  sprintf_s(lbl,32,"");
	SetWindowText(GetDlgItem(hDlg, IDC_DBG_RED),   lbl);
	
	if (bGreen) sprintf_s(lbl,32,"%2.2f", g);
	else		sprintf_s(lbl,32,"");
	SetWindowText(GetDlgItem(hDlg, IDC_DBG_GREEN), lbl);

	if (bBlue) sprintf_s(lbl,32,"%2.2f", b);
	else	   sprintf_s(lbl,32,"");
	SetWindowText(GetDlgItem(hDlg, IDC_DBG_BLUE),  lbl);

	if (bAlpha) sprintf_s(lbl,32,"%2.2f", a);
	else	    sprintf_s(lbl,32,"");
	SetWindowText(GetDlgItem(hDlg, IDC_DBG_ALPHA), lbl);

	if (bRed)   EnableWindow(GetDlgItem(hDlg, IDC_DBG_RED), true);
	else	    EnableWindow(GetDlgItem(hDlg, IDC_DBG_RED), false);	
	if (bGreen) EnableWindow(GetDlgItem(hDlg, IDC_DBG_GREEN), true);
	else		EnableWindow(GetDlgItem(hDlg, IDC_DBG_GREEN), false);	
	if (bBlue)  EnableWindow(GetDlgItem(hDlg, IDC_DBG_BLUE), true);
	else	    EnableWindow(GetDlgItem(hDlg, IDC_DBG_BLUE), false);	
	if (bAlpha) EnableWindow(GetDlgItem(hDlg, IDC_DBG_ALPHA), true);
	else		EnableWindow(GetDlgItem(hDlg, IDC_DBG_ALPHA), false);
}

void UpdateMaterialDisplay(bool bSetup)
{
	char lbl[256];
	char lbl2[64];

	OBJHANDLE hObj = vObj->GetObjectA();
	if (!oapiIsVessel(hObj)) return;
	
	vVessel *vVes = (vVessel *)vObj;
	D3D9Mesh *hMesh = (D3D9Mesh *)vObj->GetMesh(sMesh);
	if (!hMesh) return;

	DWORD matidx = hMesh->GetMeshGroupMaterialIdx(sGroup);

	// Set material info
	const char *skin = NULL;
	if (skin)	sprintf_s(lbl, 256, "Material %u: [Skin %s]", matidx, skin);
	else		sprintf_s(lbl, 256, "Material %u:", matidx);

	GetWindowText(GetDlgItem(hDlg, IDC_DBG_MATGRP), lbl2, 64);
	if (strcmp(lbl, lbl2)) SetWindowText(GetDlgItem(hDlg, IDC_DBG_MATGRP), lbl); // Avoid causing flashing

	DWORD MatPrp = SendDlgItemMessageA(hDlg, IDC_DBG_MATPRP, CB_GETCURSEL, 0, 0);

	switch(MatPrp) {
		case 0:	// Diffuse
			DisplayMat(true, true, true, true);
			if (bSetup) SelColor = 3;
		break;
		case 1:	// Ambient
			DisplayMat(true, true, true, false);
			if (bSetup) SelColor = 3;
		break;
		case 2:	// Specular
			DisplayMat(true, true, true, true);
			if (bSetup) SelColor = 3;
		break;
		case 3:	// Emission
			DisplayMat(true, true, true, false);
			if (bSetup) SelColor = 0;
		break;
		case 4:	// Reflectivity
			DisplayMat(true, true, false, false);
			if (bSetup) SelColor = 0;
		break;
		case 5:	// Dissolve
			DisplayMat(true, true, false, false);
			if (bSetup) SelColor = 0;
		break;
	}

	DWORD texidx = hMesh->GetMeshGroupTextureIdx(sGroup);

	if (texidx==0) SetWindowText(GetDlgItem(hDlg, IDC_DBG_TEXTURE), "Texture: None");
	else {
		//char lbl[256];
		SURFHANDLE hSrf = hMesh->GetTexture(texidx);
		if (hSrf) {
			sprintf_s(lbl, 256, "Texture: %s", SURFACE(hSrf)->GetName());
			SetWindowText(GetDlgItem(hDlg, IDC_DBG_TEXTURE), lbl);
		}
	}

	sprintf_s(lbl, 256, "Mesh: %s", hMesh->GetName());
	SetWindowText(GetDlgItem(hDlg, IDC_DBG_MESHNAME), lbl);

	// Setup dissolve texture
	D3D9MatExt * pMatE = hMesh->GetMaterialExtension(matidx);
	if (pMatE->pDissolve==NULL) SendDlgItemMessageA(hDlg, IDC_DBG_MATEFF, CB_SETCURSEL, 0, 0);
	else {
		int id = g_client->GetIndexOfDissolveMap(pMatE->pDissolve);	
		if (id>=0) SendDlgItemMessageA(hDlg, IDC_DBG_MATEFF, CB_SETCURSEL, id+1, 0);
	}
}


void UpdateColorSlider(WORD pos)
{
	float val = float(pos)/255.0f;
	
	DWORD MatPrp = SendDlgItemMessageA(hDlg, IDC_DBG_MATPRP, CB_GETCURSEL, 0, 0);

	if (MatPrp==2 && SelColor==3) val*=80.0; // Specular Power
	if (MatPrp==5 && SelColor==0) val*=12.0;  // Dissolve scale
	if (MatPrp==5 && SelColor==1) val*=0.2f;  // Dissolve scatter
	if (MatPrp==4 && SelColor==1) val*=2.0;  // Glass reflect

	UpdateMeshMaterial(val, MatPrp, SelColor);
}




DWORD GetSelectedMesh()
{
	return sMesh;
}


void SelectGroup(DWORD idx)
{
	if (idx<nGroup) {
		sGroup = idx;
		SetupMeshGroups();
	}
}

void SelectMesh(D3D9Mesh *pMesh)
{
	for (DWORD i=0;i<nMesh;i++) {
		if (vObj->GetMesh(i)==pMesh) {
			sMesh = i;
			break;
		}
	}
	SetupMeshGroups();
}


void SetupMeshGroups()
{
	char lbl[256];

	if (!vObj) return; 

	SetWindowText(GetDlgItem(hDlg, IDC_DBG_VISUAL), visual);

	if (nMesh!=0) {
		if (sMesh>0xFFFF) sMesh = nMesh-1;
		if (sMesh>=nMesh) sMesh = 0;
	}
	else {
		sMesh=0, sGroup=0, nGroup=0;
		SetWindowText(GetDlgItem(hDlg, IDC_DBG_MESH), "N/A");
		SetWindowText(GetDlgItem(hDlg, IDC_DBG_GROUP), "N/A");
		return;
	}

	sprintf_s(lbl,256,"%u/%u",sMesh,nMesh-1);
	SetWindowText(GetDlgItem(hDlg, IDC_DBG_MESH), lbl);

	D3D9Mesh *mesh = (class D3D9Mesh *)vObj->GetMesh(sMesh);

	if (mesh) nGroup = mesh->GroupCount();
	else	  nGroup = 0;

	if (nGroup!=0) {
		if (sGroup>0xFFFF)  sGroup = nGroup-1;
		if (sGroup>=nGroup) sGroup = 0;
	}
	else {
		sGroup=0;
		SetWindowText(GetDlgItem(hDlg, IDC_DBG_GROUP), "N/A");
		return;
	}

	sprintf_s(lbl,256,"%u/%u",sGroup,nGroup-1);
	SetWindowText(GetDlgItem(hDlg, IDC_DBG_GROUP), lbl);

	UpdateMaterialDisplay();
	SetColorSlider();
}

double GetVisualSize()
{
	if (hDlg && vObj) {
		OBJHANDLE hObj = vObj->GetObjectA();
		if (hObj) return oapiGetSize(hObj);
	}
	return 1.0;
}

vObject * GetVisual()
{
	return vObj;
}

void SetVisual(vObject *vo)
{
	if (!hDlg) {
		vObj = NULL;	// Always set the visual to NULL if the dialog isn't open
		return;
	}
	vObj = vo;
	UpdateVisual();
}

void UpdateVisual()
{
	if (!vObj || !hDlg) return; 
	nMesh = vObj->GetMeshCount();
	sprintf_s(visual, 64, "Visual: %s", vObj->GetName());
	SetupMeshGroups();
}

void RemoveVisual(vObject *vo)
{
	if (vObj==vo) vObj=NULL;
}

void SetColorValue(const char *lbl)
{
	DWORD MatPrp = SendDlgItemMessageA(hDlg, IDC_DBG_MATPRP, CB_GETCURSEL, 0, 0);
	UpdateMeshMaterial(float(atof(lbl)), MatPrp, SelColor);
	SetColorSlider();
}

// ==============================================================
// Dialog message handler

BOOL CALLBACK WndProc(HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	char lbl[32];

	switch (uMsg) {

	case WM_INITDIALOG:
	{
		return TRUE;	// All Init actions are done in OpenDlgClbk();
	}

	case WM_HSCROLL:
	{
		if (LOWORD(wParam)==TB_THUMBTRACK || LOWORD(wParam)==TB_ENDTRACK) {
			WORD pos = HIWORD(wParam);
			if (HWND(lParam)==GetDlgItem(hWnd, IDC_DBG_SPEED)) {
				if (pos==0) pos = WORD(SendDlgItemMessage(hDlg, IDC_DBG_SPEED, TBM_GETPOS,  0, 0));
				double fpos = pow(2.0,double(pos)*13.0/200.0); 
				sprintf_s(lbl,32,"%1.0f",fpos);
				SetWindowTextA(GetDlgItem(hWnd, IDC_DBG_SPEEDDSP), lbl);
				camSpeed = fpos/50.0;
			}
			if (HWND(lParam)==GetDlgItem(hWnd, IDC_DBG_MATADJ)) {
				if (pos==0) pos = WORD(SendDlgItemMessage(hDlg, IDC_DBG_MATADJ, TBM_GETPOS,  0, 0));
				UpdateColorSlider(pos);
				UpdateMaterialDisplay();
			}
		}
		return false;
	}

	case WM_COMMAND:

		switch (LOWORD(wParam)) {

			case IDCANCEL:
				oapiCloseDialog(hWnd);
				hDlg = NULL;
				vObj = NULL;
				break;
		
			case IDC_DBG_MATSAVE:
			{
				OBJHANDLE hObj = vObj->GetObjectA();
				if (oapiIsVessel(hObj)) {
					vVessel *vVes = (vVessel *)vObj;
					vVes->GetMaterialManager()->SaveConfiguration();
				}
				break;
			}
				

			case IDC_DBG_RED:
				if (HIWORD(wParam)==EN_SETFOCUS) {
					SelColor = 0;
					UpdateMaterialDisplay();
					SetColorSlider();
				}
				if (HIWORD(wParam)==EN_KILLFOCUS) {
					GetWindowTextA(HWND(lParam), lbl, 32);
					SetColorValue(lbl);
				}
				break;

			case IDC_DBG_GREEN:
				if (HIWORD(wParam)==EN_SETFOCUS) {
					SelColor = 1;
					UpdateMaterialDisplay();
					SetColorSlider();
				}
				if (HIWORD(wParam)==EN_KILLFOCUS) {
					GetWindowTextA(HWND(lParam), lbl, 32);
					SetColorValue(lbl);
				}
				break;

			case IDC_DBG_BLUE:
				if (HIWORD(wParam)==EN_SETFOCUS) {
					SelColor = 2;
					UpdateMaterialDisplay();
					SetColorSlider();
				}
				if (HIWORD(wParam)==EN_KILLFOCUS) {
					GetWindowTextA(HWND(lParam), lbl, 32);
					SetColorValue(lbl);
				}
				break;

			case IDC_DBG_ALPHA:
				if (HIWORD(wParam)==EN_SETFOCUS) {
					SelColor = 3;
					UpdateMaterialDisplay();
					SetColorSlider();
				}
				if (HIWORD(wParam)==EN_KILLFOCUS) {
					GetWindowTextA(HWND(lParam), lbl, 32);
					SetColorValue(lbl);
				}
				break;

			case IDC_DBG_MATPRP:
				if (HIWORD(wParam)==CBN_SELCHANGE) {
					UpdateMaterialDisplay(true);
					SetColorSlider();
				}
				break;

			case IDC_DBG_MATEFF:
				if (HIWORD(wParam)==CBN_SELCHANGE) {
					DWORD idx = SendDlgItemMessage(hWnd, IDC_DBG_MATEFF, CB_GETCURSEL, 0, 0);
					if (idx==0) UpdateDissolveMap(NULL);
					else        UpdateDissolveMap(g_client->GetDissolveMap(idx-1));
					UpdateMaterialDisplay(true);
					SetColorSlider();
				}
				break;

			case IDC_DBG_DISPLAY:
				if (HIWORD(wParam)==CBN_SELCHANGE) dspMode = SendDlgItemMessage(hWnd, IDC_DBG_DISPLAY, CB_GETCURSEL, 0, 0);
				break;

			case IDC_DBG_CAMERA:
				if (HIWORD(wParam)==CBN_SELCHANGE) camMode = SendDlgItemMessage(hWnd, IDC_DBG_CAMERA, CB_GETCURSEL, 0, 0);
				break;

			case IDC_DBG_MSHUP: 
				if (HIWORD(wParam)==BN_CLICKED) sMesh--;
				SetupMeshGroups();
				break;

			case IDC_DBG_MSHDN: 
				if (HIWORD(wParam)==BN_CLICKED) sMesh++;
				SetupMeshGroups();
				break;

			case IDC_DBG_GRPUP: 
				if (HIWORD(wParam)==BN_CLICKED) sGroup--;
				SetupMeshGroups();
				break;

			case IDC_DBG_GRPDN: 
				if (HIWORD(wParam)==BN_CLICKED) sGroup++;	
				SetupMeshGroups();
				break;

			
			case IDC_DBG_MESH: 
				if (HIWORD(wParam)==EN_KILLFOCUS) {
					char cbuf[32];
					GetWindowText(GetDlgItem(hWnd, IDC_DBG_MESH),  cbuf, 32); 
					for (int i=0; i<32;i++) if (cbuf[i]==0 || cbuf[i]=='/') { cbuf[i]=0; break; }
					sMesh = atoi(cbuf);
					SetupMeshGroups();
				}
				break;

			case IDC_DBG_GROUP: 
				if (HIWORD(wParam)==EN_KILLFOCUS) {
					char cbuf[32];
					GetWindowText(GetDlgItem(hWnd, IDC_DBG_GROUP),  cbuf, 32);
					for (int i=0; i<32;i++) if (cbuf[i]==0 || cbuf[i]=='/') { cbuf[i]=0; break; }
					sGroup = atoi(cbuf);
					SetupMeshGroups();
				}
				break;

			case IDC_DBG_GRPO:
			case IDC_DBG_VISO:
			case IDC_DBG_MSHO:
			case IDC_DBG_BOXES:
			case IDC_DBG_SPHERES:
			case IDC_DBG_HSM:
			case IDC_DBG_HSG:
			case IDC_DBG_AMBIENT:
			case IDC_DBG_TILES:
			case IDC_DBG_WIRE:
			case IDC_DBG_DUAL:
			case IDC_DBG_ENVMAP:
			case IDC_DBG_PICK:
				UpdateFlags();
				break;
		
			default: 
				LogErr("LOWORD(%hu), HIWORD(0x%hX)",LOWORD(wParam),HIWORD(wParam));
				break;
		}
		break;
	}

	return oapiDefDialogProc(hWnd, uMsg, wParam, lParam);
}

} //namespace


		