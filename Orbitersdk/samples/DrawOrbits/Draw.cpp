// =================================================================================================================================
//
// Copyright (C) 2016 Jarmo Nikkanen
//
// Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation 
// files (the "Software"), to use, copy, modify, merge, publish, distribute, interact with the Software and sublicense
// copies of the Software, subject to the following conditions:
//
// a) You do not sell, rent or auction the Software.
// b) You do not collect distribution fees.
// c) You do not remove or alter any copyright notices contained within the Software.
// d) This copyright notice must be included in all copies or substantial portions of the Software.
//
// If the Software is distributed in an object code form then in addition to conditions above:
// e) It must inform that the source code is available and how to obtain it.
// f) It must display "NO WARRANTY" and "DISCLAIMER OF LIABILITY" statements on behalf of all contributors like the one below.
//
// The accompanying materials such as artwork, if any, are provided under the terms of this license unless otherwise noted. 
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
// OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
// LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR
// IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
// =================================================================================================================================

#define STRICT 1
#define ORBITER_MODULE

#include "OrbiterAPI.h"
#include "VesselAPI.h"
#include "ModuleAPI.h"
#include "DrawAPI.h"
#include <windowsx.h>
#include "gcAPI.h"
#include "Sketchpad2.h"
#include "Orbit.h"
#include "Reference.h"

#define NTEMP 5

static float eEll[NTEMP] = { 0.0f, 0.5f, 0.75f, 0.88f, 0.95f };
static float eHyp[NTEMP] = { 1.01f, 1.1f, 1.3f, 1.6f, 2.0f };


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
	void		Label(Sketchpad2 *pSkp2, IVECTOR2 *pt, VECTOR3 &plnDir, const char *label);
	void		DrawOrbit(Sketchpad2 *pSkp2, COrbit *pOrb, OBJHANDLE hRef, oapi::FVECTOR4 &color, DWORD flags = 0);
	void		CreateOrbitTemplates();
	bool		IsVisible(VECTOR3 pos, oapi::IVECTOR2 *pt);
	VECTOR3		WorldDirection(VECTOR3 d, MATRIX4 &mVP);
	void		SetClipper(Sketchpad2 *pSkp2, OBJHANDLE hObj, DWORD Idx=0);

	HPOLY		pElliptic[NTEMP];
	HPOLY		pHyperbolic[NTEMP];

	DWORD		upidx;
	
	Clipper		Clip[2];
	VECTOR3     CamPos;
	
	SURFHANDLE  hTex;
	oapi::Font  *hFnt;
	oapi::FMATRIX4 mVP;
	MATRIX4		dmVP;
	ReferenceClass *Ref;
	Body *pBody;
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
Orbits::Orbits(HINSTANCE hInst) : Module(hInst)
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
	Ref = NULL;
	upidx = 0;

	oapiWriteLog("oapi::Module::clbkSimulationStart");
	
	DWORD bcnt = oapiGetGbodyCount();
	Ref = new ReferenceClass();
	pBody = new Body[bcnt+1];
	memset(pBody, 0, (bcnt+1) * sizeof(Body));


	if (gcInitialize()) {

		hTex = oapiLoadTexture("samples/DrawOrbits/Orbits.dds");
		hFnt = oapiCreateFont(15, false, "Arial");

		gcRegisterRenderProc(RenderOrbitClbk, RENDERPROC_PLANETARIUM, this);

		CreateOrbitTemplates();

		for (DWORD i = 0; i < bcnt; i++) {
			pBody[i].hObj = oapiGetGbodyByIndex(i);
			if (!pBody[i].hObj) continue;
			pBody[i].hRef = Ref->GetReference(pBody[i].hObj);
			if (pBody[i].hRef) {
				pBody[i].pOrb = new COrbit(pBody[i].hObj, pBody[i].hRef);
			}
			else {
				if (i!=0) oapiWriteLogV("Orbits: No Reference for object %u, 0x%X", i, pBody[i].hObj);
			}
		}
	}
}


// =================================================================================================
//
void Orbits::clbkSimulationEnd()
{
	oapiWriteLog("oapi::Module::clbkSimulationEnd");

	if (gcEnabled()) {

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
void Orbits::clbkRender(oapi::Sketchpad *pSkp)
{

	oapiCameraGlobalPos(&CamPos);

	if ((gcSketchpadVersion(pSkp) == 2) && (oapiCameraInternal() == false)) {
		
		oapi::Sketchpad2 *pSkp2 = (oapi::Sketchpad2 *)pSkp;

		memcpy(&mVP, pSkp2->GetViewProjectionMatrix(), sizeof(FMATRIX4));

		dmVP = gcMatrix4(&mVP);

		pSkp2->SetFont(hFnt);
		pSkp2->QuickPen(0x80808080, 1.0f);

		int i = 1;
		while (pBody[i].hObj) {

			if (pBody[i].fInts>0.01f) {

				if (pBody[i].pOrb) {

					float fI = pBody[i].fInts;
					FVECTOR4 color = _FVECTOR4(0.3f*fI, 0.3f*fI, 0.3f*fI, fI*fI);

					SetClipper(pSkp2, pBody[i].hObj, PLN_MOON);
					SetClipper(pSkp2, pBody[i].hRef, PLN_MAIN);

					DrawOrbit(pSkp2, pBody[i].pOrb, pBody[i].hRef, color, ODR_APS|ODR_NOD);
				}
			}

			i++;
		}

		FVECTOR4 color = _FVECTOR4(0, 1, 0, 1);

		VESSEL *hVes = oapiGetFocusInterface();
		OBJHANDLE hRef = hVes->GetGravityRef();

		COrbit orb;
		orb.Create(hVes);
		orb.ReferencePole(_I_ECL, _K_ECL);

		SetClipper(pSkp2, oapiCameraTarget(), PLN_MOON);
		SetClipper(pSkp2, hRef, PLN_MAIN);

		DrawOrbit(pSkp2, &orb, hRef, color, ODR_APS|ODR_LON|ODR_NOD|ODR_LAB);
	}
}


// =================================================================================================
// 
void Orbits::SetClipper(Sketchpad2 *pSkp2, OBJHANDLE hObj, DWORD idx)
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
VECTOR3 Orbits::WorldDirection(VECTOR3 d, MATRIX4 &mVP)
{
	VECTOR4 in = _V(d.x, d.y, d.z, 0.0);
	VECTOR4 sc = mul(in, mVP);
	double f = abs(1.0 / sc.w);
	sc.x *= f;
	sc.y *= -f;
	sc.z *= f;
	return unit(_V(sc.x, sc.y, sc.z));
}


// =================================================================================================
// Check if orbital location is visible. pos = location relative to reference body which must be Clip[0]
//
bool Orbits::IsVisible(VECTOR3 pos, oapi::IVECTOR2 *pt)
{
	pos += Clip[0].Pos;				// Conver to camera centric location. Clip[0].Pos = camera centric planet position
	double len = length(pos);
	VECTOR3 uPos = pos / len;

	for (int i = 0; i < 2; i++) if ((Clip[i].vcov < dotp(Clip[i].uPos, uPos)) && (len > Clip[i].hdst)) return false;

	return gcWorldToScreenSpace(pos, pt, &mVP);
}


// =================================================================================================
//
void Orbits::DrawOrbit(Sketchpad2 *pSkp2, COrbit *pOrb, OBJHANDLE hRef, oapi::FVECTOR4 &color, DWORD of)
{
	FMATRIX4 mat;
	
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
	DWORD black = gcColor(&_FVECTOR4(0, 0, 0, color.a));
	DWORD draw  = gcColor(&color);


	// Build Matrix to render from a pre-computed orbit templates
	//
	VECTOR3 _P = pOrb->_P;
	VECTOR3 _Q = pOrb->_Q;
	VECTOR3 _W = crossp_LH(_P, _Q);

	mat._y = _FVECTOR4(_Q * (pOrb->SMi() / smi) );
	mat._x = _FVECTOR4(_P * (pOrb->SMa()) );
	mat._z = _FVECTOR4(_W);

	// Offset the template to actual planet position
	//
	VECTOR3 offs = _P * (pOrb->SMa()*pOrb->Ecc());

	gcSetTranslation(&mat, Clip[0].Pos - offs);

	pSkp2->SetWorldTransform(&mat);
	pSkp2->SetViewMode(Sketchpad2::USER);
	pSkp2->QuickPen(draw, 2.0f);
	

	if (ecc<1.0) pSkp2->DrawPoly(pElliptic[idx]);
	else		 pSkp2->DrawPoly(pHyperbolic[idx]);


	// Update matrix for generic drawing in 3D ----------------------
	//
	mat._y = _FVECTOR4(_Q);
	mat._x = _FVECTOR4(_P);
	mat._z = _FVECTOR4(_W);

	// Move origin to the center of the reference planet
	//
	gcSetTranslation(&mat, Clip[0].Pos);

	pSkp2->SetWorldTransform(&mat);

	double dLan = pOrb->TrAOfAscendingNode(pOrb->GetPole());		// TrA of AN
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
	pSkp2->SetViewMode(Sketchpad2::ORTHO);
	pSkp2->SetWorldTransform();
	pSkp2->QuickPen(draw);

	char buf[256];

	if (of&ODR_NOD) {
		if (pOrb->IsTrAValid(dLan)) {
			VECTOR3 pos = pOrb->PosByTrA(dLan);
			if (IsVisible(pos, &pt)) {
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
			if (IsVisible(pos, &pt)) {
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
		if (pOrb->IsTrAValid(dLPe)) {
			VECTOR3 pos = pOrb->PosByTrA(dLPe);
			if (IsVisible(pos, &pt)) {
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
			if (IsVisible(pos, &pt)) {
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
void Orbits::Label(Sketchpad2 *pSkp2, IVECTOR2 *pt, VECTOR3 &plnDir, const char *label)
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

	VECTOR3 sd = WorldDirection(plnDir, dmVP);

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

	for (int i = 0; i < NTEMP; i++) {

		double ecc = eEll[i];
		double smi = sqrt(1.0 - ecc*ecc);
		double nra = 0.0;
		double stp = PI2 / 512.0;

		for (int i = 0; i < 511; i++) {
			double eca = nra2eca(nra, ecc);
			points[i].x = float(cos(eca));
			points[i].y = float(sin(eca)*smi);
			nra += stp;
		}

		pElliptic[i] = gcCreatePoly(NULL, points, 511, PF_CONNECT);
	}


	for (int i = 0; i < NTEMP; i++) {

		double ecc = eHyp[i];
		double smi = sqrt(ecc*ecc-1.0);
		double nra = eca2nra(6.0, ecc);
		double stp = abs(nra*2.0) / 511.0;
		nra = PI2-nra;
		for (int i = 0; i < 512; i++) {
			nra = limit(nra);
			double eca = nra2eca(nra, ecc);
			points[i].x = float(cosh(eca));
			points[i].y = float(sinh(eca)*smi);
			nra += stp;
		}

		pHyperbolic[i] = gcCreatePoly(NULL, points, 512, 0);
	}
}