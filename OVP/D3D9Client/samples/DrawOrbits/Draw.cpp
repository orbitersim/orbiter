// ==================================================================
// Copyright (c) 2021 Jarmo Nikkanen
// Licensed under the MIT License
// ==================================================================


#define STRICT 1
#define ORBITER_MODULE

#include "OrbiterAPI.h"
#include "VesselAPI.h"
#include "ModuleAPI.h"
#include "DrawAPI.h"
#include <windowsx.h>
#include "gcCoreAPI.h"
#include "Orbit.h"
#include "Reference.h"

#define NTEMP 5

static double eEll[NTEMP] { 0.0f, 0.5f, 0.75f, 0.88f, 0.95f };
static double eHyp[NTEMP] { 1.01f, 1.1f, 1.3f, 1.6f, 2.0f };


struct Body {
	OBJHANDLE hRef;		// Reference handle
	OBJHANDLE hObj;		// Object handle
	COrbit *  pOrb;		// Orbit information
	float	  fInts;	// Orbit line intensity
};

#define ODR_APS		0x1	// Apsides
#define ODR_NOD		0x2	// Nodes
#define ODR_LON		0x4	// Line of Nodes
#define ODR_LAB		0x8 // Labels

#define PLN_MAIN	0
#define PLN_MOON	1

class Orbits : public oapi::Module
{
	struct Clipper {
		VECTOR3 uPos;
		VECTOR3 Pos;
		double  vcov;
		double  dRad;
		double  hdst;
	};

public:

				Orbits(HINSTANCE hDLL);
				~Orbits();

	void		clbkRender(oapi::Sketchpad *pSkp);
	void		clbkSimulationStart(RenderMode rm);
	void		clbkSimulationEnd();
	void		clbkPreStep(double simt, double simdt, double mjd);
	
private:
	void		Label(Sketchpad *pSkp2, IVECTOR2 *pt, VECTOR3 &plnDir, const char *label);
	void		DrawOrbit(Sketchpad *pSkp2, COrbit *pOrb, OBJHANDLE hRef, oapi::FVECTOR4 &color, DWORD flags = 0);
	void		CreateOrbitTemplates();
	bool		IsVisible(VECTOR3 pos, oapi::IVECTOR2 *pt, const SIZE& s);
	FVECTOR3	WorldDirection(VECTOR3 d);
	bool		WorldToScreenSpace(const VECTOR3& wpos, oapi::IVECTOR2* pt, const FMATRIX4* pVP, const SIZE& s, float clip = 1.0f);
	void		SetClipper(Sketchpad *pSkp2, OBJHANDLE hObj, DWORD Idx=0);

	HPOLY		pElliptic[NTEMP];
	HPOLY		pHyperbolic[NTEMP];

	DWORD		upidx;
	
	Clipper		Clip[2];
	VECTOR3     CamPos;
	
	SURFHANDLE  hTex;
	oapi::Font  *hFnt;
	const oapi::FMATRIX4* pVP;
	ReferenceClass *Ref;
	Body *pBody;
	gcCore* pCore;
};


// =================================================================================================
// Render HUD Wrapper
// =================================================================================================
//
void __cdecl RenderOrbitClbk(oapi::Sketchpad *pSkp, void *pParam)
{
	((Orbits*)pParam)->clbkRender(pSkp);
}


// =================================================================================================
// Initialize module
// =================================================================================================
//
DLLCLBK void InitModule(HINSTANCE hModule)
{
	oapiRegisterModule(new Orbits(hModule));
}


// =================================================================================================
//
DLLCLBK void ExitModule(HINSTANCE  hModule)
{

}



// =================================================================================================
// Orbiter Module
// =================================================================================================
//
Orbits::Orbits(HINSTANCE hInst) : Module(hInst), pCore(NULL), Ref(NULL), pBody(NULL)
{

	FILE *fp = fopen("Config/DrawOrbits.cfg", "rt");

	if (fp) {
		char buf[256];
		while (fgets(buf, 256, fp)) {
			//if (strcmp(buf, "KeyActivation") == 0) sscanf(buf, "KeyActivation %X", &KeyActivation);
			//if (strcmp(buf, "NullZone") == 0) sscanf(buf, "NullZone %d", &NullZone);
		}
		fclose(fp);
	}
}


// =================================================================================================
//
Orbits::~Orbits()
{

}


// =================================================================================================
//
void Orbits::clbkSimulationStart(RenderMode rm)
{
	upidx = 0;

	oapiWriteLog("oapi::Module::clbkSimulationStart");
	
	size_t bcnt{ oapiGetGbodyCount() };
	Ref = new ReferenceClass();
	pBody = new Body[bcnt+1];
	memset(pBody, 0, (bcnt+1) * sizeof(Body));

	pCore = gcGetCoreInterface();

	if (pCore)
	{
		hTex = oapiLoadTexture("samples/DrawOrbits/Orbits.dds");
		hFnt = oapiCreateFontEx(15, "Arial");

		pCore->RegisterRenderProc(RenderOrbitClbk, RENDERPROC_PLANETARIUM, this);

		CreateOrbitTemplates();

		for (size_t i = 0; i < bcnt; i++) {
			pBody[i].hObj = oapiGetGbodyByIndex(i);
			if (!pBody[i].hObj) continue;
			pBody[i].hRef = Ref->GetReference(pBody[i].hObj);
			if (pBody[i].hRef) {
				pBody[i].pOrb = new COrbit(pBody[i].hObj, pBody[i].hRef);
			}
			else {
				if (i != 0) oapiWriteLogV("Orbits: No Reference for object %u, 0x%X", i, pBody[i].hObj);
			}
		}
	}
	else oapiWriteLog("Error: No pCore");
}


// =================================================================================================
//
void Orbits::clbkSimulationEnd()
{
	oapiWriteLog("oapi::Module::clbkSimulationEnd");

	if (pCore) 
	{
		//oapiReleaseTexture(hTex);
		//oapiReleaseFont(hFnt);
		//gcRegisterRenderProc(RenderOrbitClbk, RENDERPROC_DELETE, NULL);	// Unregister callback 

		int i = 0;
		while (pBody[i].hObj) {
			if (pBody[i].pOrb) delete pBody[i].pOrb;
			i++;
		}

		if (Ref) delete Ref;

		for (int i = 0; i < NTEMP; i++) {
			//gcDeletePoly(pElliptic[i]);
			//gcDeletePoly(pHyperbolic[i]);
		}
	}
}

// =================================================================================================
//
float saturate(double s)
{
	if (s > 1.0f) return 1.0f;
	if (s < 0.0f) return 0.0f;
	return float(s);
}

// =================================================================================================
//
void Orbits::clbkPreStep(double simt, double simdt, double mjd)
{
	double cosdir = cos(30.0*PI / 180.0);

	if (pBody[upidx].hRef) {

		pBody[upidx].pOrb->Create(pBody[upidx].hObj, pBody[upidx].hRef);

		VECTOR3 cpos, bpos;
		oapiCameraGlobalPos(&cpos);
		oapiGetGlobalPos(pBody[upidx].hRef, &bpos);

		VECTOR3 rdir = unit(bpos - cpos);		// Reference body direction
		double cdst = oapiCameraTargetDist();
		double rdst = length(bpos - cpos);

		COrbit *pO = pBody[upidx].pOrb;

		// Compute/Update orbit line intensity ---------------------------------------

		// Fade away orbits viewed from a shallow angle
		float f = float(pow(abs(dotp(rdir, pO->_W)), 0.2));

		if (f < 0.6f) f = 0.6f;

		// Fade away orbits those are too small to be viewed
		float a = saturate((pO->ApD() * 20.0) / rdst);

		// Fade away orbits those are too large for parctical viewing
		float b = saturate(cdst / (pO->PeD() * 0.1));

		f *= (b*b);
		f *= (a*a);
		
		pBody[upidx].fInts = f;
	}

	upidx++;
	if (upidx >= oapiGetGbodyCount()) upidx = 0;

}


// =================================================================================================
//
void Orbits::clbkRender(oapi::Sketchpad *pSkp2)
{

	oapiCameraGlobalPos(&CamPos);

	if (oapiCameraInternal() == false)
	{	

		pVP = pSkp2->GetViewProjectionMatrix();

		pSkp2->SetFont(hFnt);
		pSkp2->QuickPen(0x80808080, 1.0f);

		int i = 1;
		while (pBody[i].hObj) {

			if (pBody[i].fInts>0.01f) {

				if (pBody[i].pOrb) {

					float fI = pBody[i].fInts;
					FVECTOR4 color = FVECTOR4(0.3f*fI, 0.3f*fI, 0.3f*fI, fI*fI);

					SetClipper(pSkp2, pBody[i].hObj, PLN_MOON);
					SetClipper(pSkp2, pBody[i].hRef, PLN_MAIN);

					DrawOrbit(pSkp2, pBody[i].pOrb, pBody[i].hRef, color, ODR_APS|ODR_NOD);
				}
			}

			i++;
		}

		FVECTOR4 color = FVECTOR4(0, 1, 0, 1);

		VESSEL *hVes = oapiGetFocusInterface();
		OBJHANDLE hRef = hVes->GetGravityRef();

		COrbit orb;
		orb.Create(hVes);
		orb.ReferencePole(_I_ECL, _K_ECL);

		//SetClipper(pSkp2, oapiCameraTarget(), PLN_MOON);
		SetClipper(pSkp2, hRef, PLN_MAIN);

		DrawOrbit(pSkp2, &orb, hRef, color, ODR_APS|ODR_LON|ODR_NOD|ODR_LAB);
	}
}


// =================================================================================================
// 
void Orbits::SetClipper(Sketchpad *pSkp2, OBJHANDLE hObj, DWORD idx)
{
	VECTOR3 bpos;

	if (hObj) {

		oapiGetGlobalPos(hObj, &bpos);

		double dRad = oapiGetSize(hObj);

		Clip[idx].Pos = (bpos - CamPos);				// Object position
		Clip[idx].dRad = dRad;

		double len2 = dotp(Clip[idx].Pos, Clip[idx].Pos);
		double hdst = sqrt(len2 - dRad*dRad);
		double ilen = 1.0 / sqrt(len2);
		
		Clip[idx].uPos = Clip[idx].Pos * ilen;		// Object position [unit] 
		Clip[idx].vcov = hdst * ilen;				// View coverage
		
		if (idx == PLN_MOON) Clip[idx].hdst = 0.0;
		else				 Clip[idx].hdst = hdst;

		pSkp2->Clipper(idx, &Clip[idx].uPos, Clip[idx].vcov, Clip[idx].hdst);
	}
	else {
		Clip[idx].vcov = 2.0; // This will disable clipping
		pSkp2->Clipper(idx);
	}
}


// =================================================================================================
// Convert a world space direction to screen space direction
// Positive directions in screen space are (right, down, away from camera)
//
FVECTOR3 Orbits::WorldDirection(VECTOR3 d)
{
	FVECTOR4 sc = mul(FVECTOR4(d, 1.0f), *pVP);
	float f = abs(1.0f / sc.w);
	sc.x *= f;
	sc.y *= -f;
	sc.z *= f;
	return unit(sc.xyz);
}


bool Orbits::WorldToScreenSpace(const VECTOR3& wpos, oapi::IVECTOR2* pt, const FMATRIX4* pVP, const SIZE& s, float clip)
{

	FVECTOR4 homog = mul(FVECTOR4(wpos, 1.0f), *pVP);

	if (homog.w < 0.0f) return false;

	homog.xyz /= homog.w;
	
	bool bVis = true;
	if (homog.x < -clip || homog.x > clip || homog.y < -clip || homog.y > clip) bVis = false;

	if (_hypot(homog.x, homog.y) < 1e-6) {
		pt->x = s.cx / 2;
		pt->y = s.cy / 2;
	}
	else {
		pt->x = (long)((float(s.cx) * 0.5f * (1.0f + homog.x)) + 0.5f);
		pt->y = (long)((float(s.cy) * 0.5f * (1.0f - homog.y)) + 0.5f);
	}

	return bVis;
}


// =================================================================================================
// Check if orbital location is visible. pos = location relative to reference body which must be Clip[0]
//
bool Orbits::IsVisible(VECTOR3 pos, oapi::IVECTOR2 *pt, const SIZE &s)
{
	pos += Clip[0].Pos;	// Conver to camera centric location. Clip[0].Pos = camera centric planet position
	double len = length(pos);
	VECTOR3 uPos = pos / len;

	for (int i = 0; i < 2; i++) if ((Clip[i].vcov < dotp(Clip[i].uPos, uPos)) && (len > Clip[i].hdst)) return false;

	return WorldToScreenSpace(pos, pt, pVP, s);
}


// =================================================================================================
//
void Orbits::DrawOrbit(Sketchpad *pSkp2, COrbit *pOrb, OBJHANDLE hRef, oapi::FVECTOR4 &color, DWORD of)
{	
	double smi = 0.0;
	double ecc = pOrb->Ecc();
	double diff = 1e6;
	int idx = 0;

	// Choose a closest matching unit template, SMa = 1.0
	//
	if (ecc < 1.0) {
		for (int i = 0; i < NTEMP; i++) if (abs(eEll[i] - ecc) < diff) idx = i, diff = abs(eEll[i] - ecc);
		smi = sqrt(1.0 - eEll[idx] * eEll[idx]);
	} else {
		for (int i = 0; i < NTEMP; i++) if (abs(eHyp[i] - ecc) < diff) idx = i, diff = abs(eHyp[i] - ecc);
		smi = sqrt(eHyp[idx] * eHyp[idx] - 1.0);
	}

	// Set pen colors
	//
	DWORD black = FVECTOR4(0.0f, 0.0f, 0.0f, color.a).dword_abgr();
	DWORD draw = color.dword_abgr();


	// Build Matrix to render from a pre-computed orbit templates
	//
	VECTOR3 _P = pOrb->_P;
	VECTOR3 _Q = pOrb->_Q;
	VECTOR3 _W = crossp_LH(_P, _Q);
	VECTOR3 _F = _P * (pOrb->SMa() * pOrb->Ecc()); // Offset the template to actual planet position

	FMATRIX4 mat;
	mat._y = FVECTOR4(_Q * (pOrb->SMi() / smi), 0.0f);
	mat._x = FVECTOR4(_P * (pOrb->SMa()), 0.0f);
	mat._z = FVECTOR4(_W, 0.0f);
	mat._p = FVECTOR4(Clip[0].Pos - _F, 1.0f);


	SIZE screen;
	pSkp2->GetRenderSurfaceSize(&screen);
	pSkp2->SetWorldTransform(&mat);

	pSkp2->SetViewMode(Sketchpad::USER);
	pSkp2->SetClipDistance(1.0f, 1e13f);
	pSkp2->QuickPen(draw, 2.0f);
	

	if (ecc<1.0) pSkp2->DrawPoly(pElliptic[idx]);
	else		 pSkp2->DrawPoly(pHyperbolic[idx]);


	// Update matrix for generic drawing in 3D ----------------------
	//
	mat._y = FVECTOR4(_Q, 0.0f);
	mat._x = FVECTOR4(_P, 0.0f);
	mat._z = FVECTOR4(_W, 0.0f);
	mat._p = FVECTOR4(Clip[0].Pos, 1.0f);

	pSkp2->SetWorldTransform(&mat);

	double dLan = pOrb->TrAOfAscendingNode(pOrb->GetPole());	// TrA of AN
	double dLdn = limit(dLan + PI);								// TrA of DN
	double dLPe = 0.0;											// TrA of PE
	double dLAp = PI;											// TrA of AP
	double dRad = oapiGetSize(hRef);
	float  fRad = float(dRad);

	FVECTOR2 Points[2];
	IVECTOR2 pt;
	long s = 4;

	if (of&ODR_LON) {

		pSkp2->QuickPen(draw, 2.0f, 2);

		if (pOrb->IsTrAValid(dLan)) {
			Points[1] = pOrb->PQPosByTrA(dLan);
			Points[0] = unit(Points[1]) * fRad;
			pSkp2->Lines(Points, 1);
		}

		if (pOrb->IsTrAValid(dLdn)) {
			Points[1] = pOrb->PQPosByTrA(dLdn);
			Points[0] = unit(Points[1]) * fRad;
			pSkp2->Lines(Points, 1);
		}
	}

	// Switch to orthographic projection to draw markers and other things
	//
	pSkp2->SetViewMode(Sketchpad::SkpView::ORTHO);
	pSkp2->SetClipDistance(-1.0f, 1.0f);
	pSkp2->SetWorldTransform();
	pSkp2->QuickPen(draw);

	char buf[256];

	if (of&ODR_NOD) {
		if (pOrb->IsTrAValid(dLan)) {
			VECTOR3 pos = pOrb->PosByTrA(dLan);
			if (IsVisible(pos, &pt, screen)) {
				pSkp2->QuickBrush(draw);
				pSkp2->Rectangle(pt.x - s, pt.y - s, pt.x + s, pt.y + s);
				if (of&ODR_LAB) {
					strcpy_s(buf, 256, "LAN ");
					strcat_s(buf, 256, AngleToText(pOrb->LAN()*DEG, 2));
					strcat_s(buf, 256, ";RIn ");
					strcat_s(buf, 256, AngleToText(pOrb->Inc()*DEG, 2));
					Label(pSkp2, &pt, pos, buf);
				}
			}
		}
		if (pOrb->IsTrAValid(dLdn)) {
			VECTOR3 pos = pOrb->PosByTrA(dLdn);
			if (IsVisible(pos, &pt, screen)) {
				pSkp2->QuickBrush(black);
				pSkp2->Rectangle(pt.x - s, pt.y - s, pt.x + s, pt.y + s);
				if (of&ODR_LAB) {
					strcpy_s(buf, 256, "LAN ");
					strcat_s(buf, 256, AngleToText(limit(pOrb->LAN()-PI)*DEG, 2));
					strcat_s(buf, 256, ";RIn ");
					strcat_s(buf, 256, AngleToText(pOrb->Inc()*DEG, 2));
					Label(pSkp2, &pt, pos, buf);
				}
			}
		}
	}

	if (of&ODR_APS) {
		if (pOrb->IsTrAValid(dLPe)) 
		{
			VECTOR3 pos = pOrb->PosByTrA(dLPe);
			if (IsVisible(pos, &pt, screen)) 
			{
				pSkp2->QuickBrush(draw);
				pSkp2->Ellipse(pt.x - s, pt.y - s, pt.x + s, pt.y + s);
				if (of&ODR_LAB) {
					strcpy_s(buf, 256, "PeT ");
					strcat_s(buf, 256, ValueToText(pOrb->PeT(), 2));
					strcat_s(buf, 256, ";PeA ");
					strcat_s(buf, 256, ValueToText(pOrb->PeD()-dRad, 2));
					Label(pSkp2, &pt, pos, buf);
				}
			}
		}
		if (pOrb->IsTrAValid(dLAp)) {
			VECTOR3 pos = pOrb->PosByTrA(dLAp);
			if (IsVisible(pos, &pt, screen)) {
				pSkp2->QuickBrush(black);
				pSkp2->Ellipse(pt.x - s, pt.y - s, pt.x + s, pt.y + s);
				if (of&ODR_LAB) {
					strcpy_s(buf, 256, "ApT ");
					strcat_s(buf, 256, ValueToText(pOrb->ApT(), 2));
					strcat_s(buf, 256, ";ApA ");
					strcat_s(buf, 256, ValueToText(pOrb->ApD() - dRad, 2));
					Label(pSkp2, &pt, pos, buf);
				}
			}
		}
	}
}

// =================================================================================================
//
inline void Swap(long *a, long *b)
{
	long c = *a; *a = *b; *b = c;
}

// =================================================================================================
//
void Orbits::Label(Sketchpad *pSkp2, IVECTOR2 *pt, VECTOR3 &plnDir, const char *label)
{
	int w = 95;
	int h = 60;
	int xo = 5;
	int yo = 5;
	int tx = 18;
	int ty = 20;
	
	SIZE size;
	pSkp2->GetRenderSurfaceSize(&size);

	RECT src = { 0, 0, w, h };

	FVECTOR3 sd = WorldDirection(plnDir);

	// Horizontal Mirroring
	if (sd.x<0) {
		Swap(&src.left, &src.right);
		xo = -w;
		tx = -w + 10;
	}

	// Vertical Mirroring
	if (sd.y<0) {
		Swap(&src.top, &src.bottom);
		yo = -h - 5;
		ty = -h + 5;
	}

	RECT tgt = { pt->x + xo, pt->y + yo, pt->x + w + xo, pt->y + h + yo };

	pSkp2->StretchRect(hTex, &src, &tgt);
	
	int x = pt->x + tx;
	int y = pt->y + ty;

	char buffer[256];
	strcpy_s(buffer, 256, label);
	char *tok = strtok(buffer, ";");

	while (tok) {
		pSkp2->Text(x, y, tok, -1);
		tok = strtok(NULL, ";");
		y += (pSkp2->GetCharSize() & 0xFFFF) + 1;
	}
}


// =================================================================================================
//
void Orbits::CreateOrbitTemplates()
{
	FVECTOR2 points[512];

	for (int i = 0; i < NTEMP; i++) 
	{
		double ecc = eEll[i];
		double smi = sqrt(1.0 - ecc*ecc);
		double nra = 0.0;
		double stp = PI2 / 512.0;

		for (int i = 0; i < 511; i++) 
		{
			double eca = nra2eca(nra, ecc);
			points[i].x = float(cos(eca));
			points[i].y = float(sin(eca)*smi);
			nra += stp;
		}

		pElliptic[i] = pCore->CreatePoly(NULL, points, 511, PF_CONNECT);
	}


	for (int i = 0; i < NTEMP; i++) 
	{
		double ecc = eHyp[i];
		double smi = sqrt(ecc*ecc-1.0);
		double nra = eca2nra(6.0, ecc);
		double stp = abs(nra*2.0) / 511.0;
		nra = PI2-nra;
		for (int i = 0; i < 512; i++) 
		{
			nra = limit(nra);
			double eca = nra2eca(nra, ecc);
			points[i].x = float(cosh(eca));
			points[i].y = float(sinh(eca)*smi);
			nra += stp;
		}

		pHyperbolic[i] = pCore->CreatePoly(NULL, points, 512, 0);
	}
}
