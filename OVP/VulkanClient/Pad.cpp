// ===================================================
// Copyright (C) 2012-2021 Jarmo Nikkanen
// licensed under LGPL v2
// ===================================================


#include "Pad.h"
#include "Client.h"
#include "Surface.h"
#include "Util.h"
#include "TextMgr.h"
#include "Config.h"
#include "Log.h"
#include "Mesh.h"

using namespace oapi;


// ===============================================================================================
// Font cache
// ===============================================================================================

struct FontCache {
	int         height;
	int         orient;
	bool        prop;
	char        face[64];
	FontStyle	style;
	vkTextPtr pFont;
};

struct QFontCache {
	int         height;
	int         width;
	int			weight;
	char        face[64];
	FontStyle	style;
	float		spacing;
	vkTextPtr pFont;
};

std::vector<QFontCache *> qcache;
std::vector<FontCache *> fcache;


oapi::Font * deffont = 0;
oapi::Pen * defpen = 0;


// ===============================================================================================
//
void vkPad::SinCos(int n, int k)
{
	pSinCos[k] = new FVECTOR2[n];
	float s = float(PI2) / float(n);
	float q = -s / 2.0f;
	for (int i = 0; i<n; i++) { 
		pSinCos[k][i].x = sin(q) + 1.0f;	
		pSinCos[k][i].y = cos(q) + 1.0f; 
		q += s; 
	}
}



// ===============================================================================================
//
void vkPad::vkTechInit(vkClient *_gc, LPDIRECT3DDEVICE9 pDevice)
{
	pDev = pDevice;
	gc = _gc;
	log = NULL;

	InitializeCriticalSectionAndSpinCount(&LogCrit, 256);


#ifdef SKPDBG
	if (fopen_s(&log, "Sketchpad.log", "w+")) { log = NULL; } // Failed
#endif // SKPDBG


	Idx = new WORD[3 * nQueueMax + 3];
	Vtx = new SkpVtx[3 * nQueueMax + 3];

	SinCos(8, 0);
	SinCos(16, 1);
	SinCos(32, 2);
	SinCos(64, 3);

	// Initialize Techniques -------------------------------------------------------------------------
	//
	char name[256];
	sprintf_s(name, 256, "Modules/vkShaders/Sketchpad.fx");

	// Create the Effect from a .fx file.
	ID3DXBuffer* errors = 0;

	HR(D3DXCreateEffectFromFileA(pDev, name, 0, 0, 0, 0, &FX, &errors));

	if (errors) {
		LogErr("Effect Error: %s",(char*)errors->GetBufferPointer());
		MessageBoxA(0, (char*)errors->GetBufferPointer(), "Sketchpad.fx Error", 0);
		FatalAppExitA(0,"Critical error has occured. See Orbiter.log for details");
	}

	if (FX==0) {
		LogErr("Failed to create an Effect (%s)",name);
		MissingRuntimeError();
		return;
	}

	if (Config->ShaderDebug) {
		LPD3DXBUFFER pBuffer = NULL;
		if (D3DXDisassembleEffect(FX, true, &pBuffer) == S_OK) {
			FILE *fp = NULL;
			if (!fopen_s(&fp, "Sketchpad_asm.html", "w")) {
				fwrite(pBuffer->GetBufferPointer(), 1, pBuffer->GetBufferSize(), fp);
				fclose(fp);
			}
			pBuffer->Release();
		}
	}


	pNoise	  = gc->GetNoiseTex();

	eDrawMesh = FX->GetTechniqueByName("SketchMesh");
	eSketch   = FX->GetTechniqueByName("SketchTech");
	eVP       = FX->GetParameterByName(0, "gVP");
	eTex0     = FX->GetParameterByName(0, "gTex0");
	eFnt0	  = FX->GetParameterByName(0, "gFnt0");
	eDashEn   = FX->GetParameterByName(0, "gDashEn");
	eW		  = FX->GetParameterByName(0, "gW");
	eKey	  = FX->GetParameterByName(0, "gKey");
	ePen      = FX->GetParameterByName(0, "gPen");
	eWVP	  = FX->GetParameterByName(0, "gWVP");
	eFov	  = FX->GetParameterByName(0, "gFov");
	eRandom   = FX->GetParameterByName(0, "gRandom");
	eTarget	  = FX->GetParameterByName(0, "gTarget");
	eTexEn	  = FX->GetParameterByName(0, "gTexEn");
	eFntEn    = FX->GetParameterByName(0, "gFntEn");
	eKeyEn    = FX->GetParameterByName(0, "gKeyEn");
	eWide	  = FX->GetParameterByName(0, "gWide");
	eWidth	  = FX->GetParameterByName(0, "gWidth");
	eSize	  = FX->GetParameterByName(0, "gSize");
	eMtrl	  = FX->GetParameterByName(0, "gMtrl");
	eShade    = FX->GetParameterByName(0, "gShade");
	ePos	  = FX->GetParameterByName(0, "gPos");
	ePos2	  = FX->GetParameterByName(0, "gPos2");
	eCov	  = FX->GetParameterByName(0, "gCov");
	eCovEn	  = FX->GetParameterByName(0, "gClipEn");
	eClearEn  = FX->GetParameterByName(0, "gClearEn");
	eEffectsEn= FX->GetParameterByName(0, "gEffectsEn");
	eNoiseTex = FX->GetParameterByName(0, "gNoiseTex");
	eGamma	  = FX->GetParameterByName(0, "gGamma");
	eNoiseColor = FX->GetParameterByName(0, "gNoiseColor");
	eColorMatrix = FX->GetParameterByName(0, "gColorMatrix");
	
}


// ===============================================================================================
//
void vkPad::GlobalExit()
{
	LogAlw("Clearing Font Cache... %d Fonts are stored in the cache",fcache.size() + qcache.size());
	for (auto it = fcache.begin(); it != fcache.end(); ++it) {
		delete *it;
	}
	for (auto it = qcache.begin(); it != qcache.end(); ++it) {
		delete *it;
	}
	fcache.clear();
	qcache.clear();

	SAFE_RELEASE(FX);
	SAFE_DELETEA(Idx);
	SAFE_DELETEA(Vtx);
	for (int i=0;i<4;i++) SAFE_DELETEA(pSinCos[i]);

	if (log) fclose(log);
	log = NULL;

	DeleteCriticalSection(&LogCrit);
}

// ===============================================================================================
//
void vkPad::Log(const char *format, ...) const
{
	if (log == NULL) return;
	EnterCriticalSection(&LogCrit);
	char ErrBuf[1024];
	DWORD th = GetCurrentThreadId();
	va_list args;
	va_start(args, format);
	_vsnprintf_s(ErrBuf, 1024, 1024, format, args);
	va_end(args);
	fprintf_s(log, "<0x%X> [%s] %s\n", th, _PTR(this), ErrBuf);
	fflush(log);
	LeaveCriticalSection(&LogCrit);
}

// ===============================================================================================
//
void vkPad::Reset()
{
	bBeginDraw = false;
	bMustEndScene = false;
	vI = 0;
	iI = 0;
	oapiMatrixIdentity(&mO);
	oapiMatrixIdentity(&mVOrig);
	oapiMatrixIdentity(&mPOrig);
	vTarget = F4_One;
	pTgt = NULL;
	pDep = NULL;
	zfar = 1.0f;
	tgt = { 0,0,0,0 };
}



// ===============================================================================================
// Restore Default Settings: Fonts, Pens, Colors, etc...
//
void vkPad::LoadDefaults()
{
	assert(vI == 0);

	cfont = deffont;
	cpen = NULL;
	cbrush = NULL;
	hTexture = NULL;
	hFontTex = NULL;
	cx = 0;
	cy = 0;
	linescale = 1.0f;
	pattern = 1.0f;
	Enable = 0;
	RenderConfig = 0;

	tah = LEFT;
	tav = TOP;
	vmode = ORTHO;
	tCurrent = NONE;
	Change = SKPCHG_ALL;
	bkmode = TRANSPARENT;
	dwBlendState = Sketchpad::BlendState::ALPHABLEND;

	bColorComp = true;
	bLine = false;
	bEnableScissor = false;
	bDepthEnable = false;
	bColorKey = false;
	QPen.bEnabled = false;
	QBrush.bEnabled = false;

	memset(ClipData, 0, sizeof(ClipData));
	ScissorRect = { 0,0,0,0 };

	cColorKey  = FVECTOR4(DWORD(0));
	brushcolor = SkpColor(0xFF00FF00);
	bkcolor    = SkpColor(0xFF000000);
	textcolor  = SkpColor(0xFF00FF00);
	pencolor   = SkpColor(0xFF00FF00);

	oapiMatrixIdentity(&mVP);
	oapiMatrixIdentity(&mW);
	oapiMatrixIdentity(&mP);
	oapiMatrixIdentity(&mV);
	oapiMatrixIdentity((FMATRIX4*)&ColorMatrix);

	Gamma = F4_One;
	Noise = F4_Zero;
}


// ===============================================================================================
// class vkPad
// ===============================================================================================
// Constructor will create vkPad interface but doesn't prepare it for drawing.
// BeginDrawing() must be called
//
vkPad::vkPad(SURFHANDLE s, const char *_name) : Sketchpad(s),
	_isSaveBuffer(false),
	_saveBuffer(NULL),
	_saveBufferSize(0)
{
#ifdef SKPDBG 
	Log("#### Sketchpad Interface Created");
#endif
	pRState = new RenderState(pDev);
	if (_name) strcpy_s(name, 32, _name);
	else strcpy_s(name, 32, "NoName");
	Reset();
	LoadDefaults();
}


// ===============================================================================================
// class vkPad
// ===============================================================================================
// Constructor will create vkPad interface but doesn't prepare it for drawing.
// BeginDrawing() must be called
//
vkPad::vkPad(const char *_name) : Sketchpad(NULL),
	_isSaveBuffer(false),
	_saveBuffer(NULL),
	_saveBufferSize(0)
{
#ifdef SKPDBG 
	Log("#### Sketchpad Interface Created (NoTgt)");
#endif
	if (_name) strcpy_s(name, 32, _name);
	else strcpy_s(name, 32, "NoName");
	pRState = new RenderState(pDev);
	Reset();
	LoadDefaults();
}



// ===============================================================================================
//
vkPad::~vkPad ()
{
#ifdef SKPDBG 
	Log("#### Sketchpad Interface Deleted");
#endif
	if (pRState) delete pRState;
	assert(bBeginDraw == false);
	SAFE_DELETEA(_saveBuffer);
}


// ===============================================================================================
// Private
//
void vkPad::SetViewProj(const FMATRIX4* pV, const FMATRIX4* pP)
{
	mV = mVOrig = *pV;
	mP = mPOrig = *pP;
}


// ===============================================================================================
// Bind existing Sketchpad interface to TOP render targets and prepare for rendering
//
void vkPad::BeginDrawing() 
{
	// Acquire render targets from Stack
	BeginDrawing(gc->GetTopRenderTarget(), gc->GetTopDepthStencil());
}



// ===============================================================================================
// Bind existing Sketchpad interface to render targets and prepare for rendering
//
void vkPad::BeginDrawing(LPDIRECT3DSURFACE9 pRenderTgt, LPDIRECT3DSURFACE9 pDepthStensil)
{
#ifdef SKPDBG 
	Log("==== BeginDrawing %s, %s ====\n", _PTR(pRenderTgt), _PTR(pDepthStensil));
#endif

	assert(pRenderTgt != NULL);

	if (vI != 0) LogErr("Sketchpad %s has received drawing commands outside Begin() End() pair", _PTR(this));

	if (bBeginDraw == true) {
		LogErr("vkPad::BeginDrawing() called multiple times");
		HALT();
	}

	// Capture current device state
	pRState->Capture();

	Reset();
	
	bBeginDraw = true;

	// If not already in scene, then start a new one
	if (gc->IsInScene() == false) {
		gc->BeginScene();
		bMustEndScene = true;
	}
	else bMustEndScene = false;

	pRenderTgt->GetDesc(&tgt_desc);
	zfar = float(max(tgt_desc.Width, tgt_desc.Height));
	D3DMAT_OrthoOffCenterLH(&mO, 0.0f, (float)tgt_desc.Width, (float)tgt_desc.Height, 0.0f, 0.0f, zfar);
	vTarget = FVECTOR4(2.0f / (float)tgt_desc.Width, 2.0f / (float)tgt_desc.Height, (float)tgt_desc.Width, (float)tgt_desc.Height);

	

	pTgt = pRenderTgt;
	pDep = pDepthStensil;
	tgt = { 0, 0, (long)tgt_desc.Width, (long)tgt_desc.Height };
	Change = SKPCHG_ALL;
}



// ===============================================================================================
//
void vkPad::EndDrawing()
{
#ifdef SKPDBG 
	Log("==== EndDrawing ====\n");
#endif

	if (bBeginDraw == false) {
		LogErr("vkPad::EndDrawing() called without BeginDrawing()");
		HALT();
	}

	Flush();

	bBeginDraw = false;

	if (pRState) pRState->Restore();

	if (bMustEndScene) {
		gc->EndScene();
		bMustEndScene = false;
	}
}



// ===============================================================================================
//
bool vkPad::Flush(HPOLY hPoly)
{
	if (bBeginDraw == false) {
		LogErr("vkPad::Flush() called without BeginDrawing()");
		HALT();
	}
	
	UINT numPasses;
	static DWORD bkALPHA, bkZEN, bkZW, bkCULL;

	if ((iI == 0) && (hPoly == NULL)) {
#ifdef SKPDBG 
		Log("Flush (Nothing)", hPoly, iI);
#endif
		return false;
	}

	DWORD dwBlend = dwBlendState & 0xF;
	DWORD dwFilter = dwBlendState & 0xF0;
	
#ifdef SKPDBG 
	char buf[128]; strcpy_s(buf, 128, "");
	char buf2[128]; strcpy_s(buf2, 128, "");

	if (dwBlend == SKPBS_ALPHABLEND) strcpy_s(buf, 128, "SKPBS_ALPHABLEND");
	if (dwBlend == SKPBS_COPY) strcpy_s(buf, 128, "SKPBS_COPY");
	if (dwBlend == SKPBS_COPY_ALPHA) strcpy_s(buf, 128, "SKPBS_COPY_ALPHA");
	if (dwBlend == SKPBS_COPY_COLOR) strcpy_s(buf, 128, "SKPBS_COPY_COLOR");
	
	if (bDepthEnable && pDep) strcpy_s(buf2, 128, "DEPTH_ENABLED");
	else strcpy_s(buf2, 128, "DEPTH_DISABLED");

	Log("Flush [%s] [%s] hPloy=%s, iI=%hu", buf, buf2, _PTR(hPoly), iI);
#endif

	HR(pDev->GetRenderState(D3DRS_ALPHABLENDENABLE, &bkALPHA));

	//HR(pDev->GetRenderState(D3DRS_ZENABLE, &bkZEN));
	//HR(pDev->GetRenderState(D3DRS_ZWRITEENABLE, &bkZW));
	//HR(pDev->GetRenderState(D3DRS_CULLMODE, &bkCULL));

	HR(pDev->SetRenderState(D3DRS_CULLMODE, D3DCULL_NONE));
	HR(pDev->SetVertexDeclaration(pSketchpadDecl));
		
	HR(FX->SetFloat(eRandom, float(oapiRand())));
	HR(FX->SetVector(eTarget, _DX(vTarget)));
	HR(FX->SetTechnique(eSketch));
	HR(FX->Begin(&numPasses, D3DXFX_DONOTSAVESTATE));

	if (vmode == ORTHO) {
		HR(FX->BeginPass(0));
	}
	else {
		HR(FX->BeginPass(1));
	}

	if (dwBlend == Sketchpad::BlendState::ALPHABLEND) {
		pDev->SetRenderState(D3DRS_COLORWRITEENABLE, 0x7);
		HR(pDev->SetRenderState(D3DRS_ALPHABLENDENABLE, TRUE));
	}
	else if (dwBlend == Sketchpad::BlendState::COPY) {
		pDev->SetRenderState(D3DRS_COLORWRITEENABLE, 0xF);
		HR(pDev->SetRenderState(D3DRS_ALPHABLENDENABLE, FALSE));
	}
	else if (dwBlend == Sketchpad::BlendState::COPY_ALPHA) {
		pDev->SetRenderState(D3DRS_COLORWRITEENABLE, 0x8);
		HR(pDev->SetRenderState(D3DRS_ALPHABLENDENABLE, FALSE));
	}
	else if (dwBlend == Sketchpad::BlendState::COPY_COLOR) {
		pDev->SetRenderState(D3DRS_COLORWRITEENABLE, 0x7);
		HR(pDev->SetRenderState(D3DRS_ALPHABLENDENABLE, FALSE));
	}

	if (dwFilter) {
		if (dwFilter == Sketchpad::BlendState::FILTER_POINT) {
			pDev->SetSamplerState(0, D3DSAMP_MAGFILTER, D3DTEXF_POINT);
			pDev->SetSamplerState(0, D3DSAMP_MINFILTER, D3DTEXF_POINT);
		}
		if (dwFilter == Sketchpad::BlendState::FILTER_ANISOTROPIC) {
			pDev->SetSamplerState(0, D3DSAMP_MAGFILTER, D3DTEXF_ANISOTROPIC);
			pDev->SetSamplerState(0, D3DSAMP_MINFILTER, D3DTEXF_ANISOTROPIC);
			HR(pDev->SetSamplerState(0, D3DSAMP_MAXANISOTROPY, 8));
		}
	}

	if (bDepthEnable && pDep) {
		pDev->SetRenderState(D3DRS_ZENABLE, 1);
		pDev->SetRenderState(D3DRS_ZWRITEENABLE, 1);
	}
	else {
		pDev->SetRenderState(D3DRS_ZENABLE, 0);
		pDev->SetRenderState(D3DRS_ZWRITEENABLE, 0);
	}

	if (hPoly) {
		assert(iI == 0);
		vkPolyBase *pBase = static_cast<vkPolyBase *>(hPoly);
		pBase->Draw(this, pDev);
	}
	else {
		if (tCurrent == TRIANGLE) HR(pDev->DrawIndexedPrimitiveUP(D3DPT_TRIANGLELIST, 0, vI, iI / 3, Idx, D3DFMT_INDEX16, Vtx, sizeof(SkpVtx)));
		if (tCurrent == LINE) HR(pDev->DrawIndexedPrimitiveUP(D3DPT_LINELIST, 0, vI, iI / 2, Idx, D3DFMT_INDEX16, Vtx, sizeof(SkpVtx)));
	}

	HR(FX->EndPass());
	HR(FX->End());
	
	HR(pDev->SetRenderState(D3DRS_COLORWRITEENABLE, 0xF));
	HR(pDev->SetRenderState(D3DRS_ALPHABLENDENABLE, bkALPHA));

	//HR(pDev->SetRenderState(D3DRS_ZENABLE, bkZEN));
	//HR(pDev->SetRenderState(D3DRS_ZWRITEENABLE, bkZW));
	//HR(pDev->SetRenderState(D3DRS_CULLMODE, bkCULL));
		
	HR(pDev->SetRenderState(D3DRS_SCISSORTESTENABLE, 0));

	if (dwFilter) {
		pDev->SetSamplerState(0, D3DSAMP_MAGFILTER, D3DTEXF_LINEAR);
		pDev->SetSamplerState(0, D3DSAMP_MINFILTER, D3DTEXF_LINEAR);
	}
		
	iI = vI = 0;

	return true;
	
}


// ===============================================================================================
//
bool vkPad::Topology(Topo tRequest)
{
	if (tRequest == LINE) {
		if (bLine) { // Can we do LINE Topology ?
			if (tRequest != tCurrent) Change |= SKPCHG_TOPOLOGY;
			SetupDevice(tRequest);
			return true;
		}
		return false;
	}

	if (tRequest != tCurrent) Change |= SKPCHG_TOPOLOGY;

	SetupDevice(tRequest);

	return true;
}


// ===============================================================================================
//
void vkPad::SetupDevice(Topo tNew)
{

	// Check that this is the top one. Only do the check for ones created with oapiGetSketchpad()
	//if (GetSurface()) assert(gc->GetTopInterface() == this);

	// If the queue is filling up, Flush it
	//
	if (iI > (nQueueMax >> 1)) Flush();


	// Has something changed ?
	//
	if (!Change) return;


	// Flush pending drawing instructions before changing a device state
	//
	Flush();

	tCurrent = tNew;

#ifdef SKPDBG 
	char buf[512];
	strcpy_s(buf, 512, "");
	if (Change&SKPCHG_TOPOLOGY)	 strcat_s(buf, 512, "SKPCHG_TOPOLOGY ");
	if (Change&SKPCHG_TRANSFORM) strcat_s(buf, 512, "SKPCHG_TRANSFORM ");
	if (Change&SKPCHG_CLIPCONE)	 strcat_s(buf, 512, "SKPCHG_CLIPCONE ");
	if (Change&SKPCHG_PEN) strcat_s(buf, 512, "SKPCHG_PEN ");
	if (Change&SKPCHG_EFFECTS)	 strcat_s(buf, 512, "SKPCHG_EFFECTS ");
	if (Change&SKPCHG_CLIPRECT)	 strcat_s(buf, 512, "SKPCHG_CLIPRECT ");
	if (Change&SKPCHG_TEXTURE)	strcat_s(buf, 512, "SKPCHG_TEXTURE ");
	if (Change&SKPCHG_FONT)	strcat_s(buf, 512, "SKPCHG_FONT ");
	if (Change&SKPCHG_DEPTH)	strcat_s(buf, 512, "SKPCHG_DEPTH ");
	if (Change&SKPCHG_PATTERN)	strcat_s(buf, 512, "SKPCHG_PATTERN ");
	Log("StateChange = [%s]", buf);
#endif
	

	// Apply a new setup -----------------------------------------------------------------
	//
	if (Change == SKPCHG_TOPOLOGY) {
		HR(FX->SetBool(eWide, (tCurrent == TRIANGLE)));
		Change = 0;
		return;
	}


	// Apply a new setup -----------------------------------------------
	//
	if (Change & (SKPCHG_TRANSFORM | SKPCHG_CLIPCONE)) {

		if (vmode == ORTHO) {
			FMATRIX4 mWVP;
			oapiMatrixMultiply(&mWVP, &mW, &mO);
			HR(FX->SetMatrix(eWVP, _DX(mWVP)));
			HR(FX->SetMatrix(eVP, _DX(mO)));
			HR(FX->SetMatrix(eW, _DX(mW)));
			HR(FX->SetBool(eCovEn, false));
		}
		else {
			float d = float(tgt_desc.Height) * mP.m22;
			float f = atan(1.0f / d) * 1.7f;
			oapiMatrixMultiply(&mVP, &mV, &mP);
			HR(FX->SetMatrix(eVP, _DX(mVP)));
			HR(FX->SetMatrix(eW, _DX(mW)));
			HR(FX->SetFloat(eFov, f));
			HR(FX->SetBool(eCovEn, ClipData[0].bEnable || ClipData[1].bEnable));
		}
	}


	// Apply a new setup -----------------------------------------------------------------
	//
	if (Change & SKPCHG_PEN) {
		float offset = 0.0f;
		int w = int(ceil(GetPenWidth()));
		if ((w & 1) == 0) offset = 0.5f;
		HR(FX->SetValue(ePen, &pencolor.fclr, sizeof(FVECTOR4)));
		HR(FX->SetBool(eDashEn, IsDashed()));
		HR(FX->SetValue(eWidth, &(FVECTOR3(GetPenWidth(), pattern*0.13f, offset)), sizeof(FVECTOR3)));
	}


	// Apply a new setup -----------------------------------------------------------------
	//
	if (Change & SKPCHG_EFFECTS) {
		HR(FX->SetValue(eNoiseColor, &Noise, sizeof(FVECTOR4)));
		HR(FX->SetValue(eGamma, &Gamma, sizeof(FVECTOR4)));
		HR(FX->SetTexture(eNoiseTex, pNoise));
		HR(FX->SetValue(eColorMatrix, &ColorMatrix, sizeof(FMATRIX4)));
		HR(FX->SetBool(eEffectsEn, Enable != 0));
	}


	// Apply a new setup -----------------------------------------------------------------
	//
	if (Change & SKPCHG_CLIPRECT) {
		if (bEnableScissor) {
			pDev->SetScissorRect(&ScissorRect);
			pDev->SetRenderState(D3DRS_SCISSORTESTENABLE, 1);
		}
		else {
			pDev->SetRenderState(D3DRS_SCISSORTESTENABLE, 0);
		}
	}


	// Apply a new setup -----------------------------------------------------------------
	//
	if (Change & SKPCHG_CLIPCONE) {
		if (ClipData[0].bEnable || ClipData[1].bEnable) {
			HR(FX->SetValue(ePos, &ClipData[0].uDir, sizeof(FVECTOR3)));
			HR(FX->SetValue(ePos2, &ClipData[1].uDir, sizeof(FVECTOR3)));
			HR(FX->SetValue(eCov, &(FVECTOR4(ClipData[0].ca, ClipData[0].dst, ClipData[1].ca, ClipData[1].dst)), sizeof(FVECTOR4)));
		}
	}


	// Apply a new setup -----------------------------------------------------------------
	//
	if (Change & (SKPCHG_TEXTURE | SKPCHG_FONT)) {

		float tw = 1.0f, th = 1.0f;

		D3DSURFACE_DESC desc;

		if (hTexture) {

			hTexture->GetLevelDesc(0, &desc);

			tw = 1.0f / float(desc.Width);
			th = 1.0f / float(desc.Height);

			if (Change & SKPCHG_TEXTURE) {
				HR(FX->SetTexture(eTex0, hTexture));
				HR(FX->SetBool(eKeyEn, bColorKey));
				HR(FX->SetValue(eKey, &cColorKey, sizeof(FVECTOR4)));
			}
		}

		if (hFontTex) {
			if (Change & SKPCHG_FONT) {
				HR(FX->SetTexture(eFnt0, hFontTex));
			}
		}

		HR(FX->SetVector(eSize, _DX(FVECTOR4(tw, th, 1.0f, 1.0f))));
		HR(FX->SetBool(eTexEn, (hTexture != NULL)));
		HR(FX->SetBool(eFntEn, (hFontTex != NULL)));
	}


	// Apply a new setup -----------------------------------------------------------------
	//
	if (Change & SKPCHG_TOPOLOGY) {
		HR(FX->SetBool(eWide, (tCurrent == TRIANGLE)));
	}

	// All Clear
	Change = 0;

	return;
}


// ===============================================================================================
//
DWORD vkPad::ColorComp(DWORD c) const
{
	if (bColorComp) if ((c & 0xFF000000) == 0) return c | 0xFF000000;
	return c;
}

// ===============================================================================================
//
SkpColor vkPad::ColorComp(const SkpColor &c) const
{
	if (bColorComp) {
		if ((c.dclr & 0xFF000000) == 0) {
			DWORD q = c.dclr | 0xFF000000;
			return SkpColor(q);
		}
	}
	return c;
}






// ===============================================================================================
//
HDC vkPad::GetDC()
{
	DWORD *cf = SURFACE(GetSurface())->GetClientFlags();

	if ((*cf & OAPISURF_SKP_GDI_WARN) == 0) {
		*cf |= OAPISURF_SKP_GDI_WARN;
		LogErr("Call to obsolete Sketchpad::GetDC() detected. Returned NULL");
		if (Config->DebugBreak) DebugBreak();
	}

	return NULL;
}


// ===============================================================================================
//
Font *vkPad::SetFont(Font *font)
{
	if (cfont == font) return font;

#ifdef SKPDBG 
	LOGFONTA lf;
	GetObjHandle(font->GetGDIFont(), sizeof(LOGFONT), &lf);
	Log("SetFont(%s) Face=[%s] Height=%d Weight=%d", _PTR(font), lf.lfFaceName, lf.lfHeight, lf.lfWeight);
#endif
	// No "Change" falgs required here, covered in SetFontTextureNative()

	Font *pfont = cfont;
	if (font) cfont = font;
	else      cfont = deffont;
	return pfont;
}


// ===============================================================================================
//
Brush *vkPad::SetBrush (Brush *brush)
{
	if (cbrush == brush && QBrush.bEnabled == false) return brush;

#ifdef SKPDBG 
	Log("SetBrush(%s)", _PTR(brush));
#endif

	// No "Change" falgs required here, color stored in vertex data

	QBrush.bEnabled = false;

	Brush *pbrush = cbrush;
	cbrush = brush;
	if (cbrush) brushcolor = ColorComp((static_cast<vkPadBrush *>(cbrush))->clr);
	else	    brushcolor = SkpColor(0);

	IsLineTopologyAllowed();

	return const_cast<Brush*>(pbrush);
}


// ===============================================================================================
//
Pen *vkPad::SetPen (Pen *pen)
{
	if (cpen == pen && QPen.bEnabled == false) return pen;

#ifdef SKPDBG 
	Log("SetPen(%s)", _PTR(pen));
#endif

	// Change required due to pen width and style change
	Change |= SKPCHG_PEN;

	QPen.bEnabled = false;

	Pen *ppen = cpen;
	if (pen) cpen = pen;
	else     cpen = NULL;
	if (cpen) pencolor = ColorComp(static_cast<vkPadPen *>(cpen)->clr);

	IsLineTopologyAllowed();

	return ppen;
}


// ===============================================================================================
//
void vkPad::SetTextAlign (TAlign_horizontal _tah, TAlign_vertical _tav)
{
	// No Change flags
	tah = _tah;
	tav = _tav;
}


// ===============================================================================================
//
DWORD vkPad::SetTextColor(DWORD col)
{
	// Color stored in vertex data, no Change required
	DWORD prev = textcolor.dclr;
	textcolor = SkpColor(ColorComp(col));
	return prev;
}


// ===============================================================================================
//
DWORD vkPad::SetBackgroundColor(DWORD col)
{
	// Color stored in vertex data, no Change required
	DWORD prev = bkcolor.dclr;
	bkcolor = SkpColor(ColorComp(col));
	return prev;
}


// ===============================================================================================
//
void vkPad::SetBackgroundMode(BkgMode mode)
{
	// No Change required

	switch (mode) {
		case BK_TRANSPARENT: bkmode = TRANSPARENT; break;
		case BK_OPAQUE:      bkmode = OPAQUE; break;
	}
}


// ===============================================================================================
//
DWORD vkPad::GetCharSize ()
{
	TEXTMETRIC tm;
	if (cfont==NULL) return 0;
	static_cast<const vkPadFont *>(cfont)->pFont->GetvkTextMetrics(&tm);
	return MAKELONG(tm.tmHeight-tm.tmInternalLeading, tm.tmAveCharWidth);
}


// ===============================================================================================
//
DWORD vkPad::GetLineHeight () // ... *with* "internal leading"
{
	TEXTMETRIC tm;
	if (cfont == NULL) return 0;
	static_cast<const vkPadFont *>(cfont)->pFont->GetvkTextMetrics(&tm);
	return tm.tmHeight;
}


// ===============================================================================================
//
DWORD vkPad::GetTextWidth (const char *str, int len)
{
	if (str) if (str[0] == '_') if (strcmp(str, "_SkpVerInfo") == 0) return 2;
	if (cfont==NULL) return 0;
	return DWORD(static_cast<vkPadFont *>(cfont)->pFont->Length2(str, len));
}


// ===============================================================================================
//
void vkPad::SetOrigin (int x, int y)
{
#ifdef SKPDBG 
	Log("SetOrigin(%d, %d)", x, y);
#endif
	Change |= SKPCHG_TRANSFORM;

	mW.m41 = float(x);
	mW.m42 = float(y);
}


// ===============================================================================================
//
void vkPad::GetOrigin(int *x, int *y) const
{
	if (x) *x = int(mW.m41);
	if (y) *y = int(mW.m42);
}


// ===============================================================================================
//
bool vkPad::HasPen() const
{
	if (QPen.bEnabled) return true;
	if (cpen==NULL) return false;
	if (static_cast<vkPadPen*>(cpen)->style==PS_NULL) return false;
	return true;
}


// ===============================================================================================
//
void vkPad::IsLineTopologyAllowed()
{
	bLine = false;
	if ((HasPen() == true) && (GetPenWidth() < 1.1f)) bLine = true;
}


// ===============================================================================================
//
bool vkPad::IsDashed() const
{
	if (QPen.bEnabled) return QPen.style == 2;
	if (cpen==NULL) return false;
	if (static_cast<vkPadPen*>(cpen)->style==PS_DOT) return true;
	return false;
}


// ===============================================================================================
//
bool vkPad::IsAlphaTarget() const
{
	if (tgt_desc.Format == D3DFMT_A8R8G8B8) return true;
	if (tgt_desc.Format == D3DFMT_A16B16G16R16F) return true;
	if (tgt_desc.Format == D3DFMT_A32B32G32R32F) return true;
	return false;
}


// ===============================================================================================
//
bool vkPad::HasBrush() const
{
	if (QBrush.bEnabled) return true;
	return (cbrush != NULL);
}


// ===============================================================================================
//
float vkPad::GetPenWidth() const
{
	if (QPen.bEnabled) return linescale * QPen.width;
	if (cpen==NULL) return 1.0f;
	return float(static_cast<vkPadPen*>(cpen)->width*linescale);
}


// ===============================================================================================
//
void vkPad::WrapOneLine (char* str, int len, int maxWidth)
{
	vkTextPtr pText = static_cast<vkPadFont *>(cfont)->pFont;
	if (pText->Length2(str) > maxWidth) {
		char *pStr = str, // sub-string start
		     *it = pStr,  // 'iterator' char
		     *pEnd = str + len, // <= point to terminating zero
		     *pLastSpace = NULL;
		float currentWidth = 0;
		while (it < pEnd)
		{
			while (it < pEnd && currentWidth < maxWidth) {
				if (*it == ' ') { pLastSpace = it; }
				currentWidth = pText->Length2( pStr, int(it - pStr + 1) );
				++it;
			}
			// only split if we have space for it AND we have to (avoids cutting the last word)
			if (pLastSpace != NULL && currentWidth >= maxWidth) {
				*pLastSpace = '\n';
				pStr = pLastSpace + 1; // skip the space (now a newline)
				currentWidth = 0;
				pLastSpace = NULL;
			}
		}
	}
}

// ===============================================================================================
//
bool vkPad::TextBox (int x1, int y1, int x2, int y2, const char *str, int len)
{
#ifdef SKPDBG 
	Log("TextBox()");
#endif

	// No "Setup" required, done on PrintSkp

	if (cfont==NULL) return false;

	bool result = true;
	int lineSpace = static_cast<vkPadFont *>(cfont)->pFont->GetLineSpace();

	ToSaveBuffer(str, len);

	char *pch, *pEnd =_saveBuffer+len; // <= point to terminating zero
	for (pch = strtok(_saveBuffer, "\n"); pch != NULL; pch = strtok(NULL, "\n"))
	{
		int _len = lstrlen(pch);
		if (_len>1) { WrapOneLine(pch, _len, x2-x1); }
		if (pch+_len < pEnd) { *(pch+_len) = '\n'; } // strtok splits by inserting '\0's => revert'em
	}

	// "forEach(line...)" split multi-lines
	for (pch = strtok(_saveBuffer, "\n"); pch != NULL; pch = strtok(NULL, "\n")) {
		result = Text(x1, y1, pch, -1); // len is irrelevant for pointer into 'save' buffer
		y1 += lineSpace;
	}

	ReleaseSaveBuffer();
	return result;
}

// ===============================================================================================
//
bool vkPad::Text (int x, int y, const char *str, int len)
{
#ifdef SKPDBG 
	Log("Text(%s)", str);
#endif
	// No "Setup" required, done on PrintSkp

	if (cfont==NULL) return false;

	vkTextPtr pText = static_cast<vkPadFont *>(cfont)->pFont;

	switch(tah) {
		default:
		case LEFT:   pText->SetTextHAlign(0); break;
		case CENTER: pText->SetTextHAlign(1); break;
		case RIGHT:  pText->SetTextHAlign(2); break;
	}

	switch(tav) {
		default:
		case TOP:      pText->SetTextVAlign(0); break;
		case BASELINE: pText->SetTextVAlign(1); break;
		case BOTTOM:   pText->SetTextVAlign(2); break;
	}

	pText->SetRotation(static_cast<vkPadFont *>(cfont)->rotation);
	pText->SetScaling(1.0f);
	pText->PrintSkp(this, float(x - 1), float(y - 1), str, len, (bkmode == OPAQUE));

	return true;
}


// ===============================================================================================
//
void SwapRB(DWORD *c)
{
	DWORD r = ((*c) & 0x00FF0000) >> 16;
	DWORD b = ((*c) & 0x000000FF) << 16;
	*c = ((*c) & 0xFF00FF00) | b | r;
}


// ===============================================================================================
//
void vkPad::Pixel (int x, int y, DWORD col)
{
	FillRect(x, y, x + 1, y + 2, ColorComp(SkpColor(col)));
}


// ===============================================================================================
//
void vkPad::MoveTo (int x, int y)
{
	cx = x;
	cy = y;
}


// ===============================================================================================
//
void vkPad::LineTo (int tx, int ty)
{
	if (!HasPen()) return;
#ifdef SKPDBG 
	Log("LineTo()");
#endif
	Line(cx, cy, tx, ty);
	cx=tx; cy=ty;
}


// ===============================================================================================
//
void vkPad::Line (int x0, int y0, int x1, int y1)
{
	if (!HasPen()) return;
#ifdef SKPDBG 
	Log("Line()");
#endif
	IVECTOR2 pt[2];

	pt[0].x = x0; pt[0].y = y0;
	pt[1].x = x1; pt[1].y = y1;

	AppendLineVertexList<IVECTOR2>(pt);

	cx = x1; cy = y1;
}


// ===============================================================================================
//
void vkPad::FillRect(int l, int t, int r, int b, const SkpColor &c)
{
	if (r == l) return;
	if (b == t) return;
	if (r < l) swap(r, l);
	if (b < t) swap(t, b);

#ifdef SKPDBG 
	Log("FillRect()");
#endif
	if (Topology(TRIANGLE)) {
		AddRectIdx(vI);
		SkpVtxIC(Vtx[vI++], l, t, c);
		SkpVtxIC(Vtx[vI++], r, t, c);
		SkpVtxIC(Vtx[vI++], r, b, c);
		SkpVtxIC(Vtx[vI++], l, b, c);
	}
}


// ===============================================================================================
//
void vkPad::Rectangle (int l, int t, int r, int b)
{
	if (r == l) return;
	if (b == t) return;
	if (r < l) swap(r, l);
	if (b < t) swap(t, b);

#ifdef SKPDBG 
	Log("Rectangle()");
#endif
	r--;
	b--;

	// Fill interion ----------------------------------------------
	//
	if (HasBrush()) FillRect(l, t, r, b, brushcolor);

	// Draw outline ------------------------------------------
	//
	if (HasPen()) {

		IVECTOR2 pts[4];
		pts[0].x = pts[3].x = l;
		pts[0].y = pts[1].y = t;
		pts[1].x = pts[2].x = r;
		pts[2].y = pts[3].y = b;

		AppendLineVertexList<IVECTOR2>(pts, 4, true);
	}
}


// ===============================================================================================
//
void vkPad::Ellipse (int x0, int y0, int x1, int y1)
{
	if (x1 == x0) return;
	if (y1 == y0) return;
	if (x1 < x0) swap(x0, x1);
	if (y1 < y0) swap(y0, y1);

#ifdef SKPDBG 
	Log("Ellipse()");
#endif

	float w = float(x1 - x0); float h = float(y1 - y0);	float fx0 = float(x0); float fy0 = float(y0);
	DWORD z = max((x1-x0), (y1-y0));

	w *= 0.5f;
	h *= 0.5f;
	//fl += w;
	//ft += h;

	IVECTOR2 pts[65] = {};

	WORD n = 8;
	WORD q = 0;

	if (z > 16) q = 1, n = 16;
	if (z > 32) q = 2, n = 32;
	if (z > 64) q = 3, n = 64;

	for (WORD i = 0; i<n; i++) {
		pts[i].x = long(fx0 + pSinCos[q][i].x * w);
		pts[i].y = long(fy0 + pSinCos[q][i].y * h);
	}

	
	// Fill interion -------------------------------------------
	//
	if (HasBrush()) {
		if (Topology(TRIANGLE)) {

			WORD aV = vI;

			SkpVtxIC(Vtx[vI++], (x1 + x0) / 2, (y1 + y0) / 2, brushcolor);

			for (WORD i = 0; i < n; i++) SkpVtxIC(Vtx[vI++], pts[i].x, pts[i].y, brushcolor);
			for (WORD i = 0; i < n; i++) {
				Idx[iI++] = aV;
				Idx[iI++] = aV + i + 1;
				Idx[iI++] = aV + i + 2;
			}
			Idx[iI - 1] = aV + 1;
		}
	}

	// Draw outline ------------------------------------------
	//
	if (HasPen()) AppendLineVertexList<IVECTOR2>(pts, n, true);
}


// ===============================================================================================
//
void vkPad::Polygon (const IVECTOR2 *pt, int npt)
{
#ifdef SKPDBG 
	Log("Polygon(%d)", npt);
#endif

	if (npt<3) return;
	if (HasBrush() && npt > 64) return;


	// Create filled polygon interior -----------------------------------------
	//
	if (HasBrush()) {
		if (Topology(TRIANGLE)) {

			int sIdx = vI;

			// File a vertex buffer.
			for (int i = 0; i < npt; i++) SkpVtxIC(Vtx[vI++], pt[i].x, pt[i].y, brushcolor);

			WORD qIdx[256];
			int nIdx = CreatePolyIndexList<IVECTOR2>(pt, npt, qIdx);

			// Add indices to index buffer
			for (int i = 0; i < nIdx; i++) Idx[iI++] = qIdx[i] + sIdx;
		}
	}

	// Draw outline ------------------------------------------
	//
	if (HasPen()) AppendLineVertexList<IVECTOR2>(pt, npt, true);
}


// ===============================================================================================
//
void vkPad::Polyline (const IVECTOR2 *pt, int npt)
{
#ifdef SKPDBG 
	Log("Polyline(%d)", npt);
#endif
	if (npt < 2) return;
	if (HasPen()) AppendLineVertexList<IVECTOR2>(pt, npt, false);
}


// ===============================================================================================
//
void vkPad::DrawPoly (HPOLY hPoly, DWORD flags)
{
#ifdef SKPDBG 
	Log("DrawPoly(%s, 0x%X)", _PTR(hPoly), flags);
#endif

	if (hPoly) {

		vkPolyBase *pBase = static_cast<vkPolyBase *>(hPoly);
		int PolyType = pBase->type;

		if ((PolyType == 0) && !HasPen()) return;

		// Flush pending graphics before a use of different interface 
		Flush();

		if (Topology(TRIANGLE)) {

			// Flush the poly object
			Flush(hPoly);
		}
	}
}


// ===============================================================================================
//
void vkPad::Lines(const FVECTOR2 *pt, int nlines)
{
#ifdef SKPDBG 
	Log("Lines(%d)", nlines);
#endif
	if (!HasPen()) return;
	for (int i = 0; i < nlines; i++) {
		AppendLineVertexList<FVECTOR2>(pt);
		pt += 2;
	}
}




// -----------------------------------------------------------------------------------------------
// Save buffer helpers (null-terminated string for Text & TextBox)
// -----------------------------------------------------------------------------------------------

// ===============================================================================================
// Copy string to internal 'save' buffer, so it can be changed (adding terminating zeroes, etc.)
void vkPad::ToSaveBuffer (const char *str, int len)
{
	if (_saveBufferSize < len)
	{ // re-allloc bigger space
		if (_saveBuffer) { delete[] _saveBuffer; }
		_saveBuffer = new char[len + 1];
		_saveBufferSize = len;
	}
	strncpy_s(_saveBuffer, len + 1, str, len);
	_isSaveBuffer = true;
}

// ===============================================================================================
//
void vkPad::ReleaseSaveBuffer () {
	_isSaveBuffer = false;
}


// -----------------------------------------------------------------------------------------------
// Subroutines Section
// -----------------------------------------------------------------------------------------------

// ===============================================================================================
//
short mod(short a, short b)
{
	if (a<0) return b-1;
	if (a>=b) return 0;
	return a;
}


// ===============================================================================================
//
template <typename Type>
int CheckTriangle(short x, const Type *pt, const WORD *Idx, float hd, short npt, bool bSharp)
{
	WORD A = Idx[x];
	WORD B = Idx[mod(x-1,npt)];
	WORD C = Idx[mod(x+1,npt)];

	float bx = float(pt[B].x - pt[A].x);
	float by = float(pt[B].y - pt[A].y);
	float ax = float(pt[C].x - pt[A].x);
	float ay = float(pt[C].y - pt[A].y);

	if ((bx*ay-by*ax)*hd > 0.0f) return 0;	// Check handiness

	float aa = ax*ax + ay*ay;			// dot(a,a)
	float ab = ax*bx + ay*by;			// dot(a,b)
	float bb = bx*bx + by*by;			// dot(b,b)

	float qw = fabs(ab) / sqrt(aa*bb);	// abs(cos(a,b))
	if (bSharp && qw>0.9f) return 0;	// Bad Ear

	float id = 1.0f / (aa * bb - ab * ab);

	for (int i=0;i<npt;i++) {

		WORD P = Idx[i];

		if ((P==B) || (P==A) || (P==C)) continue;

		float cx = float(pt[P].x - pt[A].x);
		float cy = float(pt[P].y - pt[A].y);
		float ac = ax*cx + ay*cy;
		float bc = bx*cx + by*cy;
		float u  = (bb*ac - ab*bc) * id;
		float v  = (aa*bc - ab*ac) * id;

		// Check if the point is inside the triangle
		// NOTE: Having u+v slightly above 1.0 is a bad condition, should find a better ear.
		if  ((u>-0.0001) && (v>-0.0001) && ((u+v)<1.0001f)) return 0;
	}

	return 1; // It's an ear
}


// ===============================================================================================
//
template <typename Type>
int CreatePolyIndexList(const Type *pt, short npt, WORD *Out)
{
	if (npt > 255) return 0;
	if (npt==3) { Out[0]=0; Out[1]=1; Out[2]=2;	return 3; }

	short idx = 0;		// Number of indices written in the output
	short x = npt-1;	// First ear to test is the last one in the list
	bool bSharp = false;// Avoid sharp ears

	// Build initial index list
	WORD In[256];
	for (int i=0;i<npt;i++) In[i]=i;
	float sum = 0;
	int k = npt-1;
	int nr = 0;
	for (int i=0;i<k;i++) sum += (float(pt[i].x)*float(pt[(i+1)%k].y) - float(pt[(i+1)%k].x)*float(pt[i].y));

	if (sum>0) sum=1.0; else sum=-1.0;

	while (npt>3) {

		switch (CheckTriangle<Type>(x, pt, In, sum, npt, bSharp)) {

			case 0:
			{
				x--;
				if (x<0) { // Restart
					if (!bSharp && nr>10) return idx;	
					bSharp = false;
					x = npt - 1;
					nr++;
				}
				break;
			}

			case 1:
			{
				Out[idx] = In[mod(x-1,npt)]; idx++;
				Out[idx] = In[mod(x,npt)]; idx++;
				Out[idx] = In[mod(x+1,npt)]; idx++;
				npt--;
				for (int i=x;i<npt;i++) In[i]=In[i+1];
				x = mod(x-1,npt);
				break;
			}
		}
	}

	Out[idx] = In[0]; idx++;
	Out[idx] = In[1]; idx++;
	Out[idx] = In[2]; idx++;

	return idx;
}


// ===============================================================================================
//
inline FVECTOR2 _DXV2(const IVECTOR2 &pt)
{
	return FVECTOR2(float(pt.x), float(pt.y));
}

inline FVECTOR2 _DXV2(const FVECTOR2 &pt)
{
	return FVECTOR2(pt.x, pt.y);
}


// ===============================================================================================
//
template <typename Type>
void vkPad::AppendLineVertexList(const Type *pt, int _npt, bool bLoop)
{
	if (_npt < 2) return;


	// ----------------------------------------------------------------------
	// Draw a thin hairline
	// ----------------------------------------------------------------------

	if (Topology(LINE)) {

		WORD npt = WORD(_npt);
		WORD wL = vI;
		WORD li = WORD(npt - 1);
		WORD aV;
		float length = 0.0f;

		// Create line segments -------------------------------------------------
		//
		for (WORD i = 0; i<npt; i++) {

			Vtx[vI].x = float(pt[i].x);
			Vtx[vI].y = float(pt[i].y);
			Vtx[vI].l = length;
			Vtx[vI].fnc = SKPSW_CENTER | SKPSW_FRAGMENT;
			Vtx[vI].clr = pencolor.dclr;

			if (IsDashed() && i!=li) {
				float x = float(pt[i].x - pt[i+1].x);
				float y = float(pt[i].y - pt[i+1].y);
				length += sqrt(x*x + y*y);
			}
			vI++;
		}
		aV = wL;
		for (WORD i = 0; i < (npt-1); i++) {
			Idx[iI++] = aV;
			aV++;
			Idx[iI++] = aV;
		}

		// Last segment ---------------------------------------------------------
		//
		if (bLoop) {
			Idx[iI++] = vI - 1;
			Idx[iI++] = wL;
		}

		return;
	}



	// ----------------------------------------------------------------------
	// Wide line mode
	// ----------------------------------------------------------------------

	if (Topology(TRIANGLE)) {

		WORD npt = WORD(_npt);
		WORD wL = vI;
		WORD li = WORD(npt - 1);
		WORD aV, bV, cV, dV;
		float length = 0.0f;

		FVECTOR2 pp; // Prev point
		FVECTOR2 np;	// Next point

		// Line Init ------------------------------------------------------------
		//
		if (bLoop) pp = _DXV2(pt[npt - 1]);
		else	   pp = _DXV2(pt[0]) * 2.0 - _DXV2(pt[1]);

		// Create line segments -------------------------------------------------
		//
		for (WORD i = 0; i < npt; i++) {

			if (i != li)	np = _DXV2(pt[i + 1]);
			else {
				if (bLoop)	np = _DXV2(pt[0]);
				else		np = _DXV2(pt[i]) * 2.0 - _DXV2(pt[i - 1]);
			}

			WORD vII = vI + 1;

			// --------------------------------------
			Vtx[vI].x = Vtx[vII].x = float(pt[i].x);
			Vtx[vI].y = Vtx[vII].y = float(pt[i].y);
			Vtx[vI].nx = Vtx[vII].nx = np.x;
			Vtx[vI].ny = Vtx[vII].ny = np.y;
			Vtx[vI].px = Vtx[vII].px = pp.x;
			Vtx[vI].py = Vtx[vII].py = pp.y;
			Vtx[vI].l = Vtx[vII].l = length;
			Vtx[vI].clr = Vtx[vII].clr = pencolor.dclr;
			// --------------------------------------

			Vtx[vI].fnc = SKPSW_WIDEPEN_L | SKPSW_FRAGMENT;
			aV = vI; vI++;
			Vtx[vI].fnc = SKPSW_WIDEPEN_R | SKPSW_FRAGMENT;
			bV = vI; vI++;
			// --------------------------------------

			if (i > 0) {
				Idx[iI++] = cV;	Idx[iI++] = aV;
				Idx[iI++] = dV;	Idx[iI++] = dV;
				Idx[iI++] = aV;	Idx[iI++] = bV;
			}

			cV = aV;
			dV = bV;

			pp = _DXV2(pt[i]);

			if (IsDashed()) length += ::length(np - pp);
		}

		// Last segment ---------------------------------------------------------
		//
		if (bLoop) {
			Idx[iI++] = wL;		Idx[iI++] = aV;
			Idx[iI++] = wL + 1;	Idx[iI++] = wL + 1;
			Idx[iI++] = aV;		Idx[iI++] = bV;
		}
	}
}


// ===============================================================================================
//
template <typename Type>
void vkPad::AppendLineVertexList(const Type *pt)
{

	// ----------------------------------------------------------------------
	// Draw a thin hairline
	// ----------------------------------------------------------------------

	if (Topology(LINE)) {

		Vtx[vI].x = float(pt[0].x);
		Vtx[vI].y = float(pt[0].y);
		Vtx[vI].fnc = SKPSW_CENTER | SKPSW_FRAGMENT;
		Vtx[vI].l = 0.0f;
		Vtx[vI].clr = pencolor.dclr;
		Idx[iI++] = vI;
		vI++;

		Vtx[vI].x = float(pt[1].x);
		Vtx[vI].y = float(pt[1].y);
		Vtx[vI].px = float(pt[0].x);
		Vtx[vI].py = float(pt[0].y);
		Vtx[vI].fnc = SKPSW_CENTER | SKPSW_LENGTH | SKPSW_FRAGMENT;
		Vtx[vI].clr = pencolor.dclr;
		Idx[iI++] = vI;
		vI++;

		return;
	}


	// ----------------------------------------------------------------------
	// Wide line mode
	// ----------------------------------------------------------------------

	if (Topology(TRIANGLE)) {

		FVECTOR2  pp = _DXV2(pt[0]) * 2.0 - _DXV2(pt[1]);
		FVECTOR2  np;

		WORD vF = vI;

		for (int i = 0; i < 2; i++) {

			if (i == 0) np = _DXV2(pt[1]);
			else np = _DXV2(pt[1]) * 2.0 - _DXV2(pt[0]);

			WORD vII = vI + 1;

			// --------------------------------------
			Vtx[vI].x = Vtx[vII].x = float(pt[i].x);
			Vtx[vI].y = Vtx[vII].y = float(pt[i].y);
			Vtx[vI].nx = Vtx[vII].nx = np.x;
			Vtx[vI].ny = Vtx[vII].ny = np.y;
			Vtx[vI].px = Vtx[vII].px = pp.x;
			Vtx[vI].py = Vtx[vII].py = pp.y;
			Vtx[vI].l = Vtx[vII].l = 0.0f;
			Vtx[vI].clr = Vtx[vII].clr = pencolor.dclr;
			// --------------------------------------
			Vtx[vI].fnc = SKPSW_WIDEPEN_L | SKPSW_FRAGMENT;
			if (i) Vtx[vI].fnc |= SKPSW_LENGTH;
			vI++;
			Vtx[vI].fnc = SKPSW_WIDEPEN_R | SKPSW_FRAGMENT;
			if (i) Vtx[vI].fnc |= SKPSW_LENGTH;
			vI++;
			// --------------------------------------

			pp = _DXV2(pt[i]);
		}

		Idx[iI++] = vF + 0;
		Idx[iI++] = vF + 1;
		Idx[iI++] = vF + 2;
		Idx[iI++] = vF + 1;
		Idx[iI++] = vF + 3;
		Idx[iI++] = vF + 2;
	}
}


// ===============================================================================================
//
D3DXHANDLE   vkPad::eSketch = 0;
D3DXHANDLE   vkPad::eDrawMesh = 0;
D3DXHANDLE   vkPad::eVP = 0;
D3DXHANDLE   vkPad::eW = 0;
D3DXHANDLE   vkPad::eKey = 0;
D3DXHANDLE   vkPad::ePen = 0;
D3DXHANDLE   vkPad::eWVP = 0;
D3DXHANDLE   vkPad::eFov = 0;
D3DXHANDLE   vkPad::eRandom = 0;
D3DXHANDLE   vkPad::eTarget = 0;
D3DXHANDLE   vkPad::eTexEn = 0;
D3DXHANDLE   vkPad::eFntEn = 0;
D3DXHANDLE   vkPad::eKeyEn = 0;
D3DXHANDLE   vkPad::eWidth = 0;
D3DXHANDLE   vkPad::eTex0 = 0;
D3DXHANDLE   vkPad::eFnt0 = 0;
D3DXHANDLE   vkPad::eDashEn = 0;
D3DXHANDLE   vkPad::eSize = 0;
D3DXHANDLE   vkPad::eWide = 0;
D3DXHANDLE   vkPad::eMtrl = 0;
D3DXHANDLE   vkPad::eShade = 0;
D3DXHANDLE   vkPad::ePos = 0;
D3DXHANDLE   vkPad::ePos2 = 0;
D3DXHANDLE   vkPad::eCov = 0;
D3DXHANDLE   vkPad::eCovEn = 0;
D3DXHANDLE   vkPad::eClearEn = 0;
D3DXHANDLE   vkPad::eEffectsEn = 0;

D3DXHANDLE	 vkPad::eNoiseTex = 0;
D3DXHANDLE   vkPad::eNoiseColor = 0;
D3DXHANDLE   vkPad::eColorMatrix = 0;
D3DXHANDLE   vkPad::eGamma = 0;

ID3DXEffect* vkPad::FX = 0;
vkClient * vkPad::gc = 0;
WORD * vkPad::Idx = 0;
SkpVtx * vkPad::Vtx = 0;
FVECTOR2* vkPad::pSinCos[];
LPDIRECT3DDEVICE9 vkPadFont::pDev = 0;
LPDIRECT3DDEVICE9 vkPad::pDev = 0;
LPDIRECT3DTEXTURE9 vkPad::pNoise = 0;

FILE* vkPad::log = 0;
CRITICAL_SECTION vkPad::LogCrit;


// ======================================================================
// class GDIFont
// ======================================================================
using namespace oapi;

vkPadFont::vkPadFont(int height, bool prop, const char *face, FontStyle style, int orientation, DWORD flags) : Font(height, prop, face, style, orientation)
{
	const char *def_fixedface = "Courier New";
	const char *def_sansface = "Arial";
	const char *def_serifface = "Times New Roman";

	if (face[0]!='*') {
		if (!_stricmp (face, "fixed")) face = def_fixedface;
		else if (!_stricmp (face, "sans")) face = def_sansface;
		else if (!_stricmp (face, "serif")) face = def_serifface;
		else if (_stricmp (face, def_fixedface) && _stricmp (face, def_sansface) && _stricmp (face, def_serifface)) face = (prop ? def_sansface : def_fixedface);
	}
	else face++;

	hFont = NULL;

	if (orientation!=0) rotation = float(orientation) * 0.1f;
	else                rotation = 0.0f;

	// Browse cache ---------------------------------------------------
	//

	for (size_t i = 0; i < fcache.size(); ++i) {
		if (fcache[i]->height!=height) continue;
		if (fcache[i]->style!=style) continue;
		if (fcache[i]->prop!=prop) continue;
		if (_stricmp(fcache[i]->face,face)!=0) continue;
		pFont = fcache[i]->pFont;
		break;
	}

	int weight = (style & FONT_BOLD) ? FW_BOLD : FW_NORMAL;
	DWORD italic = (style & FONT_ITALIC) ? TRUE : FALSE;
	DWORD underline = (style & FONT_UNDERLINE) ? TRUE : FALSE;
	DWORD strikeout = (style & FONT_STRIKEOUT) ? TRUE : FALSE;

	Quality = NONANTIALIASED_QUALITY;

	if ((flags & 0xF) == 0) {
		if (Config->SketchpadFont == 1) Quality = PROOF_QUALITY;
		if (Config->SketchpadFont == 2) Quality = CLEARTYPE_QUALITY;
	}
	else {
		if (flags&SKP_FONT_ANTIALIAS) Quality = PROOF_QUALITY;
		if (flags&SKP_FONT_CLEARTYPE) Quality = CLEARTYPE_QUALITY;
	}

	// Create DirectX accelerated font for a use with vkPad ------------------
	//
	if (pFont==NULL) {

		HFONT hNew = CreateFont(height, 0, 0, 0, weight, italic, underline, strikeout, 0, 0, 2, Quality, 49, face);

		pFont = std::make_shared<vkText>(pDev);
		pFont->Init(hNew);

		DeleteObject(hNew);

		pFont->SetRotation(rotation);

		// Fill the cache --------------------------------
		FontCache *p = new FontCache();
		p->pFont  = pFont;
		p->height = height;
		p->style  = style;
		p->prop   = prop;
		strcpy_s(p->face, 64, face);
		fcache.push_back(p);
	}

	// Create Rotated windows GDI Font for a use with GDIPad ---------------------------
	//
	hFont = CreateFontA(height, 0, orientation, orientation, weight, italic, underline, strikeout, 0, 0, 2, Quality, 49, face);

	if (hFont==NULL) {
		face  = (prop ? def_sansface : def_fixedface);
		hFont = CreateFont(height, 0, orientation, orientation, weight, italic, underline, strikeout, 0, 0, 2, Quality, 49, face);
	}
}



vkPadFont::vkPadFont(int height, char *face, int width, int weight, FontStyle style, float spacing)
: Font(height, false, face, style, 0)
{

	hFont = NULL;
	rotation = 0.0f;

	// Browse cache ---------------------------------------------------
	//
	for (size_t i = 0; i < qcache.size(); ++i) {
		if (qcache[i]->height != height) continue;
		if (qcache[i]->style != style) continue;
		if (qcache[i]->width != width) continue;
		if (qcache[i]->weight != weight) continue;
		if (qcache[i]->spacing != spacing) continue;
		if (_stricmp(qcache[i]->face, face) != 0) continue;
		pFont = qcache[i]->pFont;
		break;
	}

	DWORD italic = (style & FONT_ITALIC) ? TRUE : FALSE;
	DWORD underline = (style & FONT_UNDERLINE) ? TRUE : FALSE;
	DWORD strikeout = (style & FONT_STRIKEOUT) ? TRUE : FALSE;

	Quality = NONANTIALIASED_QUALITY;
	if (Config->SketchpadFont == 1) Quality = ANTIALIASED_QUALITY;
	if (Config->SketchpadFont == 2) Quality = PROOF_QUALITY;
	
	if (style & FONT_CRISP) Quality = NONANTIALIASED_QUALITY;
	if (style & FONT_ANTIALIAS) Quality = ANTIALIASED_QUALITY;
	
	
	// Create DirectX accelerated font for a use with vkPad ------------------
	//
	if (pFont == NULL) {

		hFont = CreateFont(height, width, 0, 0, weight, italic, underline, strikeout, 0, 0, 2, Quality, 49, face);

		pFont = std::make_shared<vkText>(pDev);
		pFont->Init(hFont);
		pFont->SetRotation(0.0f);
		pFont->SetTextSpace(spacing);

		// Fill the cache --------------------------------
		QFontCache *p = new QFontCache();
		p->pFont = pFont;
		p->height = height;
		p->width = width;
		p->weight = weight;
		p->style = style;
		p->spacing = spacing;
		strcpy_s(p->face, 64, face);
		qcache.push_back(p);
	}
	else {
		// Create windows GDI Font for a use with GDIPad ---------------------------
		//
		hFont = CreateFont(height, width, 0, 0, weight, italic, underline, strikeout, 0, 0, 2, Quality, 49, face);
	}
}

// -----------------------------------------------------------------------------------------------
//
vkPadFont::~vkPadFont ()
{
	if (pFont) pFont->SetRotation(0.0f), pFont.reset();
	if (hFont) DeleteObject(hFont);
}


// -----------------------------------------------------------------------------------------------
//
HFONT vkPadFont::GetGDIFont () const
{
	return hFont;
}


// -----------------------------------------------------------------------------------------------
//
int vkPadFont::GetTextLength(const char *pText, int len) const
{
	return int(pFont->Length2(pText, len));
}


// -----------------------------------------------------------------------------------------------
//
int vkPadFont::GetIndexByPosition(const char *pText, int pos, int len) const
{
	return int(pFont->GetIndex(pText, float(pos), len));
}

// -----------------------------------------------------------------------------------------------
//
void vkPadFont::vkTechInit(LPDIRECT3DDEVICE9 pDevice)
{
	pDev = pDevice;
}



// ======================================================================
// class GDIPen
// ======================================================================

vkPadPen::vkPadPen (int s, int w, DWORD col): oapi::Pen (style, width, col)
{
	switch (s) {
		case 0:  style = PS_NULL;  break;
		case 2:  style = PS_DOT;   break;
		default: style = PS_SOLID; break;
	}
	width = w;
	if (width<1) width = 1;
	hPen = CreatePen(style, width, COLORREF(col&0xFFFFFF));
	clr = SkpColor(col);
}

// -----------------------------------------------------------------------------------------------
//
vkPadPen::~vkPadPen ()
{
	DeleteObject(hPen);
}

// -----------------------------------------------------------------------------------------------
//
void vkPadPen::vkTechInit(LPDIRECT3DDEVICE9 pDevice)
{
	//pDev = pDevice;
}



// ======================================================================
// class GDIBrush
// ======================================================================

vkPadBrush::vkPadBrush (DWORD col): oapi::Brush (col)
{
	hBrush = CreateSolidBrush(COLORREF(col&0xFFFFFF));
	clr = SkpColor(col);
}

// -----------------------------------------------------------------------------------------------
//
vkPadBrush::~vkPadBrush ()
{
	DeleteObject(hBrush);
}

// -----------------------------------------------------------------------------------------------
//
void vkPadBrush::vkTechInit(LPDIRECT3DDEVICE9 pDevice)
{
	//pDev = pDevice;
}
