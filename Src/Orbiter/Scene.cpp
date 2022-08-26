// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#include "OGraphics.h"
#include <memory.h>
#include <stdio.h>
#include "Orbiter.h"
#include "Util.h"
#include "Scene.h"
#include "Particle.h"
#include "Config.h"
#include "Texture.h"
#include "Star.h"
#include "Planet.h"
#include "Psys.h"
#include "Pane.h"
#include "Camera.h"
#include "VVessel.h"
#include "VPlanet.h"
#include "Vstar.h"
#include "VBase.h"
#include "CSphereMgr.h"
#include "Log.h"
#include "D3dmath.h"
#include "resource.h"

#define NSEG 64

#define SURFLABEL_LIMIT 0.3 // apparent planet radius limit for displaying surface base markers
#define VORLABEL_LIMIT 0.6  // apparent planet radius limit for displaying VOR transmitter labels

const int chunksize = 16;
extern Orbiter *g_pOrbiter;
extern Vessel *g_focusobj;
extern TextureManager2 *g_texmanager2;
extern Camera *g_camera;
extern PlanetarySystem *g_psys;
extern Pane *g_pane;
extern DWORD g_vtxcount;
extern DWORD g_tilecount;
extern char DBG_MSG[256];

static Vector cc(0.4,0.3,0.2); // constellation line colour
static COLORREF labelcol[6] = {0x00FFFF, 0xFFFF00, 0x4040FF, 0xFF00FF, 0x40FF40, 0xFF8080};

D3DMATERIAL7 Scene::default_mtrl = {
	{1,1,1,1},
	{1,1,1,1},
	{0,0,0,0},
	{0,0,0,0},
	0.0
};

D3DMATERIAL7 Scene::shadow_mtrl = { // material for shadows (=black)
	{0,0,0,0.7f},
	{0,0,0,1},
	{0,0,0,1},
	{0,0,0,1},
	0.0
};

static D3DLIGHT7 starlight_specular = {
	D3DLIGHT_DIRECTIONAL,
	{1,1,1,0},
	{1,1,1,0},
	{0,0,0,0},
	{0,0,0},
	{0,0,0},
	0,0,0,0,0,0,0
};

static D3DLIGHT7 starlight_nospecular = {
	D3DLIGHT_DIRECTIONAL,
	{1,1,1,0},
	{0,0,0,0},
	{0,0,0,0},
	{0,0,0},
	{0,0,0},
	0,0,0,0,0,0,0
};

static D3DMATRIX ident = {
	1,0,0,0,
	0,1,0,0,
	0,0,1,0,
	0,0,0,1
};

struct VB_XYZ { float x, y, z; };

Scene::Scene (OrbiterGraphics *og)
{
	int i;
	DWORD val;

	gc = og;

	viewW = og->GetViewW();
	viewH = og->GetViewH();

	nobj = nsun = cobj = nbuf = 0;
	nstarlight  = 0;
	nstream     = 0;
	nsbuf = nsvtx = 0;
	gcanvas = 0;
	csphere = 0;
	csphere2 = 0;
	RegisterDevices (og->GetDevice());
	InitGDI();
	sunvis = false;
	if (!og->clbkGetRenderParam (RP_MAXLIGHTS, &maxlight) || (LONG)maxlight < 0) maxlight = 8;
	if (g_pOrbiter->Cfg()->CfgVisualPrm.MaxLight)
		maxlight = min (maxlight, g_pOrbiter->Cfg()->CfgVisualPrm.MaxLight);
	locallight = g_pOrbiter->Cfg()->CfgVisualPrm.bLocalLight;
	if (locallight)
		lightlist = new LIGHTLIST[maxlight];

	bglvl = 0;

	zclearflag = D3DCLEAR_ZBUFFER;
	bool bstencil = (g_pOrbiter->Cfg()->CfgDevPrm.bTryStencil &&
		og->clbkGetRenderParam (RP_STENCILDEPTH, &val) && val >= 1);
	if (bstencil) zclearflag |= D3DCLEAR_STENCIL;
	// use stencil buffers (for shadow rendering etc.)

	//Vector cc (g_pOrbiter->Cfg()->ConstellationCol);
	cnstlimit = (int)((cc.x + cc.y + cc.z)/3.0*256.0);
	star_lght = (g_pOrbiter->Cfg()->CfgVisualPrm.bSpecular ? &starlight_specular : &starlight_nospecular);
	Mesh::GlobalEnableSpecular (g_pOrbiter->Cfg()->CfgVisualPrm.bSpecular);

	//csphere = new CSphereManager;

	LoadStars ();
	LoadConstellations ();
	AllocGrids ();

	// some general-use textures
	char cbuf[256];
	FILE *file;
	strcpy (cbuf, g_pOrbiter->Cfg()->CfgDirPrm.TextureDir);
	strcat (cbuf, "\\exhaust.dds");

	static char *gtex_name[4] = {"Exhaust", "Horizon", "Reentry", "Contrail1"};
	for (i = 0; i < 4; i++) {
		gtex[i] = 0;
		if (file = g_pOrbiter->OpenTextureFile (gtex_name[i], ".dds")) {
			if (FAILED (g_texmanager2->ReadTexture (file, gtex+i)))
				LOGOUT_ERR (g_pOrbiter->TexPath (gtex_name[i]));
			fclose (file);
		}
	}
	Init3DFonts ();
	//nexhausttex = 0;

	VObject::scene = this;
}

Scene::~Scene ()
{
	DWORD i;

	if (nbuf) {
		DelAllVisuals();
		delete []vobj;
	}
	if (nstream) {
		for (i = 0; i < nstream; i++)
			delete pstream[i];
		delete []pstream;
	}
	if (nsun) delete []vsun;
	if (nstarlight) delete []starlight;
	if (ncnst) delete []cnstvtx;
	if (csphere) delete csphere;
	if (csphere2) delete csphere2;

	grdlng->Release();
	grdlat->Release();

	for (i = 0; i < ncnstlabel; i++)
		delete []cnstlabel[i].full;

	for (i = 0; i < 4; i++)
		if (gtex[i]) gtex[i]->Release();

	for (i = 0; i < 1; i++)
		if (gfont[i]) gfont[i]->Release();

	if (gcanvas) gcanvas->Release();

	if (locallight) delete []lightlist;
//	if (nexhausttex) {
//		for (DWORD j = 0; j < nexhausttex; j++)
//			exhausttex[j].tex->Release();
//		delete []exhausttex;
//	}

	if (vb_target) vb_target->Release();
	if (vb_cnstlabel) vb_cnstlabel->Release();
	if (nsbuf) {
		for (i = 0; i < nsbuf; i++)	svtx[i]->Release();
		delete []svtx;
	}
	FreeGDI();

	VObject::scene = NULL;
}

void Scene::RegisterDevices (LPDIRECT3DDEVICE7 dv)
{
	dev = dv;
	dev->SetMaterial (&default_mtrl);

	// create multipurpose canvas texture
	const DWORD texsize = CANVAS_SIZE; // render target texture size
	DDSURFACEDESC2 ddsd;
	memset (&ddsd, 0, sizeof (ddsd));
	ddsd.dwSize = sizeof(ddsd);
	ddsd.dwFlags = DDSD_WIDTH | DDSD_HEIGHT | DDSD_PIXELFORMAT | DDSD_CAPS | DDSD_CKSRCBLT;
	ddsd.dwWidth = ddsd.dwHeight = texsize;
	ddsd.ddsCaps.dwCaps = DDSCAPS_TEXTURE; // anything else?
	ddsd.ddpfPixelFormat.dwSize = sizeof (DDPIXELFORMAT);
	ddsd.ddpfPixelFormat.dwFlags = 0;
	ddsd.ddckCKSrcBlt.dwColorSpaceLowValue = ddsd.ddckCKSrcBlt.dwColorSpaceLowValue = 0;
	gc->GetFramework()->GetBackBuffer()->GetPixelFormat (&ddsd.ddpfPixelFormat);
	gc->GetDirectDraw()->CreateSurface (&ddsd, &gcanvas, NULL);
	DDCOLORKEY ck = {0,0};
	gcanvas->SetColorKey (DDCKEY_SRCBLT, &ck);
}

void Scene::UnregisterDevices ()
{
}

void Scene::Init()
{
	if (csphere) delete csphere;
	if (csphere2) delete csphere2;
	CreateCSphere(g_pOrbiter->Cfg()->CfgVisualPrm.CSphereBgPath);
}

void Scene::InitGDI ()
{
	gdires.hFont1_scale = max (viewH/60, 14);
	gdires.hFont1 = CreateFont (gdires.hFont1_scale, 0, 0, 0, 400, TRUE, 0, 0, 0, 3, 2, 1, 49, "Arial");
	for (int i = 0; i < 6; i++)
		gdires.hPen[i] = CreatePen (PS_SOLID, 0, labelcol[i]);

	const int fsize[4] = {12, 16, 20, 26};
	for (int i = 0; i < 4; i++)
		label_font[i] = gc->clbkCreateFont(fsize[i], true, "Arial", FONT_BOLD);
	label_pen = gc->clbkCreatePen(1, 0, RGB(255,255,255));
}

void Scene::FreeGDI ()
{
	DeleteObject (gdires.hFont1);
	for (int i = 0; i < 6; i++)
		DeleteObject (gdires.hPen[i]);

	for (int i = 0; i < 4; i++)
		gc->clbkReleaseFont(label_font[i]);
	gc->clbkReleasePen(label_pen);
}

void Scene::Init3DFonts ()
{
	int i, x, y, w;
	gfont[0] = (LPDIRECTDRAWSURFACE7)gc->clbkCreateSurfaceEx (256, 256, OAPISURFACE_RENDERTARGET | OAPISURFACE_GDI);
	DDBLTFX ddbltfx;
	memset (&ddbltfx, 0, sizeof (DDBLTFX));
	ddbltfx.dwSize = sizeof(DDBLTFX);
	ddbltfx.dwFillColor = 0;
	gfont[0]->Blt (NULL, NULL, NULL, DDBLT_COLORFILL, &ddbltfx);
	HFONT pfnt, fnt = CreateFont (28, 0, 0, 0, FW_BOLD, FALSE, FALSE,
		FALSE, DEFAULT_CHARSET, OUT_DEFAULT_PRECIS, CLIP_DEFAULT_PRECIS,
		PROOF_QUALITY, FF_SWISS, "Arial");
	HDC hDC;
	gfont[0]->GetDC (&hDC);
	pfnt = (HFONT)SelectObject (hDC, fnt);
	SetTextColor (hDC, 0xffffff);
	SetBkMode (hDC, TRANSPARENT);
	GetCharWidth (hDC, 32, 126, gfont_cw[0]);
	y = 0;
	for (i = x = 0; i < 95; i++) {
		char c = 32+i;
		if (x + gfont_cw[0][i] > 256) {
			x = 0; y += 28;
		}
		TextOut (hDC, x, y-3, &c, 1);
		gfont_ofs[0][i] = x;
		gfont_yofs[i] = y;
		x += gfont_cw[0][i];
	}
	SelectObject (hDC, pfnt);
	gfont[0]->ReleaseDC (hDC);
	DeleteObject (fnt);
	DDCOLORKEY ck = {0,0};
	gfont[0]->SetColorKey (DDCKEY_SRCBLT, &ck);

	// common labels
	static char *label = "-x-y-z";
	RECT sr = {0,0,0,0};
	w = i = 0;
	for (char *c = label; *c; c++, i++) {
		int idx = *c - 32;
		sr.left = gfont_ofs[0][idx];
		sr.right = sr.left + gfont_cw[0][idx];
		sr.bottom = 28 + (sr.top = gfont_yofs[idx]);
		gcanvas->BltFast (w, 228, gfont[0], &sr, DDBLTFAST_WAIT);
		comlabel_ofs[i] = w;
		w += gfont_cw[0][idx];
	}
	comlabel_w[0] = comlabel_ofs[2] - comlabel_ofs[0]; // '-x'
	comlabel_w[1] = comlabel_ofs[2] - comlabel_ofs[1]; // 'x'
	comlabel_w[2] = comlabel_ofs[4] - comlabel_ofs[2]; // '-y'
	comlabel_w[3] = comlabel_ofs[4] - comlabel_ofs[3]; // 'y'
	comlabel_w[4] = w - comlabel_ofs[4];               // '-z'
	comlabel_w[5] = w - comlabel_ofs[5];               // 'z'
}

void Scene::UpdateVisual (Body *body, Camera **camlist, int ncam)
{
	DWORD i;
	bool vis = false;

	for (i = 0; !vis && i < ncam; i++) {
		Camera *cam = camlist[i];
		double dist = body->GPos().dist (cam->GPos());
		double apps = body->Size()/(dist*cam->TanAperture());
		vis = (apps > body->VisLimit());
#ifdef UNDEF
		double visrad = cam->Apprad_dist (1.0, body->Size()); // camera radius of visibility
		switch (body->Type()) {
		case OBJTP_PLANET:
		case OBJTP_STAR:
			visrad *= 100.0; // extend visibility radius for celestial objects
			break;
		case OBJTP_VESSEL:
			visrad *= 10.0;  // extend visibility radius for vessels - make factor vessel-selectable
			break;
		}
		double ldist = body->GPos().dist (cam->GPos()) - visrad;
		if (!strcmp (body->Name(), "GL-01"))
			sprintf (DBG_MSG, "apps=%f, ldist=%f", apps, ldist);
		//vis = (ldist < 0.0);
		// ldist is the object's distance from the range of visibility of the
		// camera. Values < 0: body is within visible range, otherwise outside
#endif
	}
	if (vis) { // create visual if it doesn't exist yet
		if (!body->GetVishandle())
			AddVisual (body);
		if (body->Type() == OBJTP_PLANET) {
			Planet *planet = (Planet*)body;
			for (i = 0; i < planet->nBase(); i++)
				UpdateVisual (planet->GetBase(i), camlist, ncam);
		}
	} else { // delete visual and children if exist
		if (body->GetVishandle())
			DelVisual (body);
	}
}

void Scene::UpdateVisuals (PlanetarySystem *psys, Camera **camlist, int ncam, bool force)
{
	int i, i0, i1;
	bool vis;

	if (force)
		i0 = 0, i1 = psys->nObj();
	else {
		if (cobj >= psys->nObj()) cobj = 0;
		i0 = cobj++, i1 = i0+1;
	}
	// check visibility from any camera
	for (i = i0; i < i1; i++) {
		Body *body = psys->GetObj(i);
		if (vis = (body->Type() == OBJTP_STAR)) { // we assume stars are always visible
			if (!body->GetVishandle()) AddVisStar (body);
			continue;
		}
		UpdateVisual (body, camlist, ncam);
	}
}

VObject *Scene::AddVisual (const Body *body)
{
	VObject *v = NULL;

	switch (body->Type()) {
		case OBJTP_VESSEL:
			v = new VVessel ((const Vessel*)body);
			break;
		case OBJTP_STAR:
			v = new VStar ((const Star*)body);
			break;
		case OBJTP_PLANET:
			v = new VPlanet ((const Planet*)body);
			break;
		case OBJTP_SURFBASE:
			v = new VBase ((const Base*)body);
			break;
	}

	if (!v) return 0;
	gc->RegisterVisObject ((OBJHANDLE)body, (VISHANDLE)v);
	if (nbuf == nobj) { // allocate space
		VObject **tmp = new VObject*[nbuf += chunksize]; TRACENEW
		memcpy (tmp, vobj, nobj*sizeof(VObject*));
		if (nobj) delete []vobj;
		vobj = tmp;
	}
	return (vobj[nobj++] = v);
}

void Scene::AddBody (Body *body, Camera **camlist, int ncam)
{
	UpdateVisual (body, camlist, ncam);
}

VObject *Scene::AddVisStar (Body *body)
{
	VStar *v = (VStar*)AddVisual (body);
	VStar **tmp = new VStar*[nsun+1]; TRACENEW
	if (nsun) {
		memcpy (tmp, vsun, nsun*sizeof(VStar*));
		delete []vsun;
	}
	vsun = tmp;
	return (vsun[nsun++] = v);
}

VObject *Scene::GetVisual (const Body *body)
{
	return (VObject*)body->GetVishandle();

	//for (int i = 0; i < nobj; i++)
	//	if (vobj[i]->GetBody() == body) return vobj[i];
	//return NULL;
}

void Scene::DelVisual (Body *body)
{
	DWORD n;
	int i;

	// first delete all child visuals
	if (body->Type() == OBJTP_PLANET) {
		Planet *planet = (Planet*)body;
		for (n = 0; n < planet->nBase(); n++) {
			Body *child = planet->GetBase(n);
			if (child->GetVishandle()) DelVisual (child);
		}
	}
	for (i = nobj-1; i >= 0; i--)
		if (vobj[i]->GetBody() == body) {
			gc->UnregisterVisObject (body);
			delete vobj[i];
			if (i < nobj-1) memmove (vobj+i, vobj+i+1, (nobj-i-1)*sizeof(VObject*));// vobj[i] = vobj[nobj-1];
			nobj--;
			break;
		}
}

void Scene::DelAllVisuals ()
{
	for (int i = 0; i < nobj; i++) {
		gc->UnregisterVisObject ((OBJHANDLE)vobj[i]->GetBody());
		delete vobj[i];
	}
	nobj = 0;
}

void Scene::AddStarlight (Star *star)
{
	STARLIGHT *tmp = new STARLIGHT[nstarlight+1]; TRACENEW
	memcpy (tmp, starlight, nstarlight*sizeof(STARLIGHT));
	if (nstarlight) delete []starlight;
	starlight = tmp;
	starlight[nstarlight].col = star->GetLightColor();
	starlight[nstarlight].gpos = &star->GPos();
	dev->LightEnable (nstarlight++, TRUE);
}

void Scene::AddLocalLight (const LightEmitter *le, const VObject *vo, DWORD idx)
{
	D3DLIGHT7 lght;
	switch (le->GetType()) {
	case LightEmitter::LT_POINT: {
		lght.dltType = D3DLIGHT_POINT;
		lght.dvRange = (float)((PointLight*)le)->GetRange();
		const double *att = ((PointLight*)le)->GetAttenuation();
		lght.dvAttenuation0 = (float)att[0];
		lght.dvAttenuation1 = (float)att[1];
		lght.dvAttenuation2 = (float)att[2];
		} break;
	case LightEmitter::LT_SPOT: {
		lght.dltType = D3DLIGHT_SPOT;
		lght.dvRange = (float)((SpotLight*)le)->GetRange();
		const double *att = ((SpotLight*)le)->GetAttenuation();
		lght.dvAttenuation0 = (float)att[0];
		lght.dvAttenuation1 = (float)att[1];
		lght.dvAttenuation2 = (float)att[2];
		lght.dvFalloff = 1.0f;
		lght.dvTheta = (float)((SpotLight*)le)->GetUmbra();
		lght.dvPhi = (float)((SpotLight*)le)->GetPenumbra();
		} break;
	}
	double intens = le->GetIntensity();
	const COLOUR4 &col_d = le->GetDiffuseColour();
	lght.dcvDiffuse.dvR = (float)(col_d.r*intens);
	lght.dcvDiffuse.dvG = (float)(col_d.g*intens);
	lght.dcvDiffuse.dvB = (float)(col_d.b*intens);
	lght.dcvDiffuse.dvA = (float)(col_d.a*intens);
	const COLOUR4 &col_s = le->GetSpecularColour();
	lght.dcvSpecular.dvR = (float)(col_s.r*intens);
	lght.dcvSpecular.dvG = (float)(col_s.g*intens);
	lght.dcvSpecular.dvB = (float)(col_s.b*intens);
	lght.dcvSpecular.dvA = (float)(col_s.a*intens);
	const COLOUR4 &col_a = le->GetAmbientColour();
	lght.dcvAmbient.dvR = (float)(col_a.r*intens);
	lght.dcvAmbient.dvG = (float)(col_a.g*intens);
	lght.dcvAmbient.dvB = (float)(col_a.b*intens);
	lght.dcvAmbient.dvA = (float)(col_a.a*intens);
	if (lght.dltType != D3DLIGHT_DIRECTIONAL) {
		const VECTOR3 pos = le->GetPosition();
		D3DVECTOR p = { (float)pos.x, (float)pos.y, (float)pos.z }; 
		D3DMath_VectorMatrixMultiply (lght.dvPosition, p, vo->MWorld());
	}
	if (lght.dltType != D3DLIGHT_POINT) {
		const VECTOR3 dir = le->GetDirection();
		Vector d = mul (vo->GetBody()->GRot(), MakeVector(dir));
		lght.dvDirection.dvX = (float)d.x;
		lght.dvDirection.dvY = (float)d.y;
		lght.dvDirection.dvZ = (float)d.z;
	}
	dev->SetLight (idx, &lght);
	dev->LightEnable (idx, TRUE);
}

void Scene::AddParticleStream (D3D7ParticleStream *_pstream)
{
	D3D7ParticleStream **tmp = new D3D7ParticleStream*[nstream+1]; TRACENEW
	if (nstream) {
		memcpy (tmp, pstream, nstream*sizeof(D3D7ParticleStream*));
		delete []pstream;
	}
	pstream = tmp;
	pstream[nstream++] = _pstream;
}

void Scene::DelParticleStream (DWORD idx)
{
	D3D7ParticleStream **tmp;
	if (nstream > 1) {
		DWORD i, j;
		tmp = new D3D7ParticleStream*[nstream-1]; TRACENEW
		for (i = j = 0; i < nstream; i++)
			if (i != idx) tmp[j++] = pstream[i];
	} else tmp = 0;
	delete pstream[idx];
	delete []pstream;
	pstream = tmp;
	nstream--;
}

void Scene::CreateCSphere(const char *path)
{
	csphere = 0;
	csphere2 = 0;
	if (!path[0]) return;

	char cbuf[256];
	g_pOrbiter->Cfg()->TexPath(cbuf, path);
	DWORD fa = GetFileAttributes(cbuf);
	if (fa & FILE_ATTRIBUTE_DIRECTORY) {
		csphere2 = new CsphereManager(path, 8, 8);

		Matrix R(2000, 0, 0, 0, 2000, 0, 0, 0, 2000), ecl2gal;
		double theta = 60.25*RAD; // 60.18*RAD;
		double phi = 90.09*RAD; // 90.02*RAD;
		double lambda = 173.64*RAD; // 173.6*RAD;
		double sint = sin(theta), cost = cos(theta);
		double sinp = sin(phi), cosp = cos(phi);
		double sinl = sin(lambda), cosl = cos(lambda);
		ecl2gal.Set(cosp, 0, sinp, 0, 1, 0, -sinp, 0, cosp);
		ecl2gal.premul(Matrix(1, 0, 0, 0, cost, sint, 0, -sint, cost));
		ecl2gal.premul(Matrix(cosl, 0, sinl, 0, 1, 0, -sinl, 0, cosl));
		R.premul(ecl2gal);
		m_WMcsphere = _M(R.m11, R.m12, R.m13, 0,
			             R.m21, R.m22, R.m23, 0,
			             R.m31, R.m32, R.m33, 0,
			             0    , 0    , 0    , 1);
	}
	else {
		csphere = new CSphereManager;
	}
}

bool Scene::ParticleStreamExists (const oapi::ParticleStream *_pstream) const
{
	for (DWORD i = 0; i < nstream; i++)
		if (pstream[i] == _pstream) return true;
	return false;
}

void Scene::Update (PlanetarySystem *psys, Camera **camlist, DWORD ncam, bool moving, bool force)
{
	DWORD i;
	UpdateVisuals (psys, camlist, ncam, force);
	for (i = 0; i < nobj; i++)
		vobj[i]->Update (moving, force);
	if (moving) {
		for (i = 0; i < nstream;) {
			if (pstream[i]->Expired()) DelParticleStream (i);
			else pstream[i++]->Update ();
		}
	}
	mincamparticledist = 1e100; // invalidate
}

void Scene::Timejump (PlanetarySystem *psys, Camera **camlist, DWORD ncam, bool moving)
{
	DWORD i;
	UpdateVisuals (psys, camlist, ncam, true);
	for (i = 0; i < nobj; i++)
		vobj[i]->Timejump (moving);
	for (i = 0; i < nstream;)
		pstream[i++]->Timejump();
}

static int lvlid[256];

#pragma optimize("g",off)

DWORD Scene::LoadStars ()
{
	// parameters for mapping an apparent magnitude value to a pixel colour intensity
	double mag_hi = g_pOrbiter->Cfg()->CfgVisualPrm.StarPrm.mag_hi;  // visual magnitude for max display brightness
	double mag_lo = g_pOrbiter->Cfg()->CfgVisualPrm.StarPrm.mag_lo;  // highest magnitude to be displayed
	double brt_min = g_pOrbiter->Cfg()->CfgVisualPrm.StarPrm.brt_min;// lowest display brightness
	bool logmap = g_pOrbiter->Cfg()->CfgVisualPrm.StarPrm.map_log;   // linear/log mapping flag

	if (mag_lo <= mag_hi) {
		LOGOUT_ERR("Inconsistent magnitude limits for background star brightness. Disabling background stars.");
		return 0;
	}

	DWORD i, j, idx = 0, nv;
	DWORD buflen = D3DMAXNUMVERTICES;
	DWORD bufsize = 16;
	double a, b;
	nsvtx = 0;
	nsbuf = 0;

	extern double g_farplane;
	DWORD lvl, plvl = 256;
	const float rad = (float)(0.5*g_farplane);
	// Make sure stars are within the fustrum limit
	// Since they are rendered without z-buffer, the actual distance doesn't matter
	float c;
	double xz;

	D3DVERTEXBUFFERDESC vbdesc;
	vbdesc.dwSize = sizeof (D3DVERTEXBUFFERDESC);
	vbdesc.dwCaps = (gc->GetFramework()->IsTLDevice() ? 0 : D3DVBCAPS_SYSTEMMEMORY);
	vbdesc.dwFVF  = D3DFVF_XYZ | D3DFVF_DIFFUSE;

	svtx = new LPDIRECT3DVERTEXBUFFER7[bufsize]; TRACENEW

	if (logmap) {
		// scaling factors for logarithmic brightness mapping
		a = -log(brt_min)/(mag_lo-mag_hi);
	} else {
		// scaling factors for linear brightness mapping
		a = (1.0-brt_min)/(mag_hi-mag_lo);
		b = brt_min - mag_lo*a;
	}

#pragma pack(1)
	struct StarRec {
		float lng, lat, mag;
	} *data = new StarRec[buflen]; TRACENEW
#pragma pack()

	// read binary data from file
	FILE *f = fopen ("Star.bin", "rb");
	if (!f) return 0; // error reading data base
	while (nv = fread (data, sizeof(StarRec), buflen, f)) {
		// limit number of stars to predefined magnitude - SHOULD BE BINARY SEARCH
		for (i = 0; i < nv; i++)
			if (data[i].mag > mag_lo) { nv = i; break; }
		if (nv) {
			if (nsbuf >= bufsize) { // grow vertex buffer list
				LPDIRECT3DVERTEXBUFFER7 *tmp = new LPDIRECT3DVERTEXBUFFER7[bufsize + 16];
				memcpy (tmp, svtx, bufsize*sizeof(LPDIRECT3DVERTEXBUFFER7*));
				delete []svtx;
				svtx = tmp;
				bufsize += 16;
			}
			vbdesc.dwNumVertices = nv;
			g_pOrbiter->GetInlineGraphicsClient()->GetDirect3D7()->CreateVertexBuffer (&vbdesc, svtx+nsbuf, 0);
			VERTEX_XYZC *vbuf;
			svtx[nsbuf]->Lock (DDLOCK_WAIT | DDLOCK_WRITEONLY | DDLOCK_DISCARDCONTENTS, (LPVOID*)&vbuf, NULL);
			for (j = 0; j < nv; j++) {
				StarRec &rec = data[j];
				VERTEX_XYZC &v = vbuf[j];
				xz = rad * cos (rec.lat);
				v.x = (float)(xz * cos (rec.lng));
				v.z = (float)(xz * sin (rec.lng));
				v.y = (float)(rad * sin (rec.lat));

				if (logmap)
					c = (float)min (1.0, max (brt_min, exp(-(rec.mag-mag_hi)*a)));
				else
					c = (float)min (1.0, max (brt_min, a*rec.mag+b));

				v.col = D3DRGBA (c,c,c,1);
				lvl = (DWORD)(c*256.0*0.5);
				if (lvl > 255) lvl = 255;
				for (DWORD k = lvl; k < plvl; k++) lvlid[k] = idx;
				plvl = lvl;
				idx++;
			}
			svtx[nsbuf]->Unlock();
			svtx[nsbuf]->Optimize (dev, 0);
			nsvtx += nv;
			nsbuf++;
		}
		if (nv < buflen) break;
	}
	fclose (f);
	
	if (bufsize > nsbuf) { // shrink buffer list to size
		LPDIRECT3DVERTEXBUFFER7 *tmp = new LPDIRECT3DVERTEXBUFFER7[nsbuf];
		memcpy (tmp, svtx, nsbuf*sizeof(LPDIRECT3DVERTEXBUFFER7*));
		delete []svtx;
		svtx = tmp;
	}

	for (i = 0; i < plvl; i++) lvlid[i] = idx;
	LOGOUT("Loading %d records from star database", nsvtx);

	delete []data;
	return nsvtx;
}

int Scene::LoadConstellations ()
{
	extern double g_farplane;
	const int maxn = 1000;
	const float rad = (float)(0.5*g_farplane);
	int i, n;
	double xz;
	struct {
		float lng1, lat1;
		float lng2, lat2;
	} rec;
	// load lines
	FILE *f = fopen ("Constell.bin", "rb");
	if (!f) return 0;
	cnstvtx = new VERTEX_XYZ[maxn*2]; TRACENEW
	for (n = 0; n < maxn; n++) {
		if (!fread (&rec, sizeof(rec), 1, f)) break;
		xz = rad * cos (rec.lat1);
		cnstvtx[n*2].x = (float)(xz * cos(rec.lng1));
		cnstvtx[n*2].z = (float)(xz * sin(rec.lng1));
		cnstvtx[n*2].y = (float)(rad * sin(rec.lat1));
		xz = rad * cos (rec.lat2);
		cnstvtx[n*2+1].x = (float)(xz * cos(rec.lng2));
		cnstvtx[n*2+1].z = (float)(xz * sin(rec.lng2));
		cnstvtx[n*2+1].y = (float)(rad * sin(rec.lat2));
	}
	if (n < maxn) {
		VERTEX_XYZ *tmp = new VERTEX_XYZ[n*2]; TRACENEW
		memcpy (tmp, cnstvtx, n*2*sizeof(VERTEX_XYZ));
		delete []cnstvtx;
		cnstvtx = tmp;
	}
	fclose (f);
	// load labels
	f = fopen ("Constell2.bin", "rb");
	ncnstlabel = 0;
	if (f) {
		struct {
			double lng, lat;
		} buf;
		D3DVERTEXBUFFERDESC vbdesc;
		vbdesc.dwSize = sizeof (D3DVERTEXBUFFERDESC);
		vbdesc.dwCaps = D3DVBCAPS_SYSTEMMEMORY; // 0;
		vbdesc.dwFVF  = D3DFVF_XYZ;
		vbdesc.dwNumVertices = MAXCONST+1;
		// NOTE: without buffersize "+1", after calling ProcessVertices a CTD occurs on exit
		// at rare occasions (device bug?)
		VB_XYZ *vbpos;
		g_pOrbiter->GetInlineGraphicsClient()->GetDirect3D7()->CreateVertexBuffer (&vbdesc, &vb_cnstlabel, 0);
		g_pOrbiter->GetInlineGraphicsClient()->GetDirect3D7()->CreateVertexBuffer (&vbdesc, &vb_target, 0);
		vb_cnstlabel->Lock (DDLOCK_WAIT | DDLOCK_WRITEONLY | DDLOCK_DISCARDCONTENTS, (LPVOID*)&vbpos, NULL);
		for (i = 0; i < MAXCONST; i++) {
			if (!fread (&buf, sizeof(buf), 1, f)) break;
			xz = cos(buf.lat);
			vbpos[i].x = (float)(xz * cos(buf.lng));
			vbpos[i].z = (float)(xz * sin(buf.lng));
			vbpos[i].y = (float)(sin(buf.lat));
			if (!fread (cnstlabel[i].abbr, 3, 1, f)) break;
			if (!fread (&cnstlabel[i].len, sizeof (int), 1, f)) break;
			cnstlabel[i].full = new char[cnstlabel[i].len]; TRACENEW
			if (!fread (cnstlabel[i].full, cnstlabel[i].len, 1, f)) {
				delete []cnstlabel[i].full;
				break;
			}
		}
		vb_cnstlabel->Unlock();
		fclose(f);
		ncnstlabel = i;
	}
	return ncnst = n;
}

//#pragma optimize("g",on)

void Scene::AllocGrids ()
{
	extern double g_farplane;
	const double rad = 0.5*g_farplane;
	int i, j, idx;
	double lng, lat, xz, y;

	D3DVERTEXBUFFERDESC vbdesc;
	vbdesc.dwSize = sizeof (D3DVERTEXBUFFERDESC);
	vbdesc.dwCaps = (g_pOrbiter->GetInlineGraphicsClient()->GetFramework()->IsTLDevice() ? 0 : D3DVBCAPS_SYSTEMMEMORY);
	vbdesc.dwFVF  = D3DFVF_XYZ;
	vbdesc.dwNumVertices = (NSEG+1) * 11;
	g_pOrbiter->GetInlineGraphicsClient()->GetDirect3D7()->CreateVertexBuffer (&vbdesc, &grdlng, 0);
	VERTEX_XYZ *vbuf;
	grdlng->Lock (DDLOCK_WAIT | DDLOCK_WRITEONLY | DDLOCK_DISCARDCONTENTS, (LPVOID*)&vbuf, NULL);
	for (j = idx = 0; j <= 10; j++) {
		lat = (j-5)*15*RAD;
		xz = rad * cos(lat);
		y  = rad * sin(lat);
		for (i = 0; i <= NSEG; i++) {
			lng = Pi2 * (double)i/(double)NSEG;
			vbuf[idx].x = (float)(xz * cos(lng));
			vbuf[idx].z = (float)(xz * sin(lng));
			vbuf[idx].y = (float)y;
			idx++;
		}
	}
	grdlng->Unlock();
	grdlng->Optimize (dev, 0);

	vbdesc.dwNumVertices = (NSEG+1) * 12;
	g_pOrbiter->GetInlineGraphicsClient()->GetDirect3D7()->CreateVertexBuffer (&vbdesc, &grdlat, 0);
	grdlat->Lock (DDLOCK_WAIT | DDLOCK_WRITEONLY | DDLOCK_DISCARDCONTENTS, (LPVOID*)&vbuf, NULL);
	for (j = idx = 0; j < 12; j++) {
		lng = j*15*RAD;
		for (i = 0; i <= NSEG; i++) {
			lat = Pi2 * (double)i/(double)NSEG;
			xz = rad * cos(lat);
			y  = rad * sin(lat);
			vbuf[idx].x = (float)(xz * cos(lng));
			vbuf[idx].z = (float)(xz * sin(lng));
			vbuf[idx].y = (float)y;
			idx++;
		}
	}
	grdlat->Unlock ();
	grdlat->Optimize (dev, 0);
}

void Scene::Render3DLabel (const Vector &gp, char *label, double scale, DWORD colour)
{
	static VERTEX_TL1TEX Vtx[4] = {
		{0,0,0,0,(D3DCOLOR)D3DRGBA(1,1,1,1),0.001f,0.001f},
		{0,0,0,0,(D3DCOLOR)D3DRGBA(1,1,1,1),1.000f,0.001f},
		{0,0,0,0,(D3DCOLOR)D3DRGBA(1,1,1,1),0.001f,1.000f},
		{0,0,0,0,(D3DCOLOR)D3DRGBA(1,1,1,1),1.000f,1.000f}
	};
	static WORD Idx[6] = {0,1,2,3,2,1};

	Vector lp = gp-g_camera->GPos();
	double dist = lp.length();
	int ix, iy, w;
	RECT sr = {0,0,0,28};
	if (g_pane->GlobalToScreen (lp/dist, ix, iy)) {
		float v, x0;

		switch (label[0]) { // check for predefined labels
		case 1:
		case 2:
		case 3:
		case 4:
		case 5:
		case 6:
			x0 = (float)comlabel_ofs[label[0]-1]/(float)CANVAS_SIZE;
			w  = comlabel_w[label[0]-1];
			v  = 0.89f;
			break;
		default: // blit the string characters into the target texture
			w  = 0;
			x0 = 0;
			v  = 0;
			for (char *c = label; *c; c++) {
				int idx = *c - 32;
				sr.left  = gfont_ofs[0][idx];
				sr.right = sr.left + gfont_cw[0][idx];
				sr.bottom = 28 + (sr.top = gfont_yofs[idx]);
				gcanvas->BltFast (w, 0, gfont[0], &sr, DDBLTFAST_WAIT);
				w += gfont_cw[0][idx];
			}
			break;
		}
		Vtx[0].tu = Vtx[2].tu = x0 + 0.001f;
		Vtx[1].tu = Vtx[3].tu = x0 + (float)w/(float)CANVAS_SIZE;
		Vtx[0].tv = Vtx[1].tv = v+0.001f;
		Vtx[2].tv = Vtx[3].tv = v + 27.5f/(float)CANVAS_SIZE;

		int i;
		float x = (float)ix, y = (float)iy;
		float rhw = (float)(g_camera->TanAperture());
		float size = (float)(scale / (dist*rhw));
		float xsize = (float)w * size;
		float ysize = 28.0f * size;
		for (i = 0; i < 4; i++) Vtx[i].col = colour;
		Vtx[0].x = x-xsize; Vtx[0].y = y-ysize; Vtx[0].rhw = rhw;
		Vtx[1].x = x+xsize; Vtx[1].y = y-ysize; Vtx[1].rhw = rhw;
		Vtx[2].x = x-xsize; Vtx[2].y = y+ysize; Vtx[2].rhw = rhw;
		Vtx[3].x = x+xsize; Vtx[3].y = y+ysize; Vtx[3].rhw = rhw;
		dev->SetTexture (0, gcanvas);
		dev->SetRenderState (D3DRENDERSTATE_COLORKEYENABLE, TRUE);
		//dev->SetRenderState (D3DRENDERSTATE_DESTBLEND, D3DBLEND_INVSRCCOLOR /*D3DBLEND_ONE*/);
		//dev->SetRenderState (D3DRENDERSTATE_SRCBLEND, D3DBLEND_INVDESTCOLOR);
		dev->DrawIndexedPrimitive (D3DPT_TRIANGLELIST, FVF_TL1TEX,
			Vtx, 4, Idx, 6, D3DDP_WAIT);
		//dev->SetRenderState (D3DRENDERSTATE_DESTBLEND, D3DBLEND_INVSRCALPHA);
		//dev->SetRenderState (D3DRENDERSTATE_SRCBLEND, D3DBLEND_SRCALPHA);
		dev->SetRenderState (D3DRENDERSTATE_COLORKEYENABLE, FALSE);
		dev->SetTexture (0,0);
	}
}

void Scene::RenderGrid (bool render_eq)
{
	int i;
	for (i = 0; i <= 10; i++) if (render_eq || i != 5)
		dev->DrawPrimitiveVB  (D3DPT_LINESTRIP, grdlng, i*(NSEG+1), NSEG+1, 0);
	for (i = 0; i < 12; i++)
		dev->DrawPrimitiveVB (D3DPT_LINESTRIP, grdlat, i*(NSEG+1), NSEG+1, 0);
}

void Scene::RenderEqLine ()
{
	dev->DrawPrimitiveVB (D3DPT_LINESTRIP, grdlng, 5*(NSEG+1), NSEG+1, 0);
}

HDC Scene::GetLabelDC (int mode)
{
	HDC hDC;
	g_pOrbiter->GetInlineGraphicsClient()->GetRenderTarget()->GetDC (&hDC);
	SelectObject (hDC, GetStockObject (NULL_BRUSH));
			SelectObject (hDC, GetStockObject (NULL_PEN));
	SelectObject (hDC, gdires.hPen[mode]);
	SelectObject (hDC, gdires.hFont1);
	SetTextAlign (hDC, TA_CENTER | TA_BOTTOM);
	SetTextColor (hDC, labelcol[mode]);
	SetBkMode (hDC, TRANSPARENT);
	return hDC;
}

void Scene::ReleaseLabelDC (HDC hDC)
{
	SelectObject (hDC, GetStockObject (SYSTEM_FONT));
	SelectObject (hDC, GetStockObject (BLACK_PEN));
	g_pOrbiter->GetInlineGraphicsClient()->GetRenderTarget()->ReleaseDC (hDC);
}

double Scene::MinParticleCameraDist() const
{
	if (mincamparticledist == 1e100) {
		for (int i = 0; i < nstream; i++) {
			double dist = pstream[i]->MinCameraDist();;
			if (dist < mincamparticledist) mincamparticledist = dist;
		}
	}
	return mincamparticledist;
}

void Scene::RenderObjectMarker (const Vector &gpos, const char *label1, const char *label2, HDC hDC, int mode, int scale)
{
	RenderDirectionMarker ((gpos-g_camera->GPos()).unit(), label1, label2, hDC, mode, scale);
}

void Scene::RenderDirectionMarker (const Vector &rdir, const char *label1, const char *label2, HDC hDC, int mode, int scale)
{
	int x, y;
	bool local_hdc = (hDC == 0);
	if (!scale) scale = viewH/80;

	D3DVECTOR homog;
	D3DMath_VectorMatrixMultiply (homog, D3DMath_Vector (-rdir.x, -rdir.y, -rdir.z), *g_camera->D3D_ProjViewMatrix());
	if (homog.x >= -1.0f && homog.x <= 1.0f &&
			homog.y >= -1.0f && homog.y <= 1.0f &&
			homog.z >=  0.0) {
		if (_hypot (homog.x, homog.y) < 1e-6) {
			x = viewW/2, y = viewH/2;
		} else {
			x = (int)(viewW*0.5*(1.0f+homog.x));
			y = (int)(viewH*0.5*(1.0f-homog.y));
		}
		if (local_hdc) hDC = GetLabelDC (mode);
		switch (mode) {
		case 0: // box
			Rectangle (hDC, x-scale, y-scale, x+scale+1, y+scale+1);
			break;
		case 1: // circle
			Ellipse (hDC, x-scale, y-scale, x+scale+1, y+scale+1);
			break;
		case 2: // diamond
			MoveToEx (hDC, x, y-scale, NULL);
			LineTo (hDC, x+scale, y); LineTo (hDC, x, y+scale);
			LineTo (hDC, x-scale, y); LineTo (hDC, x, y-scale);
			break;
		case 3: { // nabla
			int scl1 = (int)(scale*1.1547);
			MoveToEx (hDC, x, y-scale, NULL);
			LineTo (hDC, x+scl1, y+scale); LineTo (hDC, x-scl1, y+scale); LineTo (hDC, x, y-scale);
			} break;
		case 4: { // delta
			int scl1 = (int)(scale*1.1547);
			MoveToEx (hDC, x, y+scale, NULL);
			LineTo (hDC, x+scl1, y-scale); LineTo (hDC, x-scl1, y-scale); LineTo (hDC, x, y+scale);
			} break;
		case 5: { // crosshair
			int scl1 = scale/4;
			MoveToEx (hDC, x, y-scale, NULL); LineTo (hDC, x, y-scl1);
			MoveToEx (hDC, x, y+scale, NULL); LineTo (hDC, x, y+scl1);
			MoveToEx (hDC, x-scale, y, NULL); LineTo (hDC, x-scl1, y);
			MoveToEx (hDC, x+scale, y, NULL); LineTo (hDC, x+scl1, y);
			} break;
		case 6: { // rotated crosshair
			int scl1 = scale/4;
			MoveToEx (hDC, x-scale, y-scale, NULL); LineTo (hDC, x-scl1, y-scl1);
			MoveToEx (hDC, x-scale, y+scale, NULL); LineTo (hDC, x-scl1, y+scl1);
			MoveToEx (hDC, x+scale, y-scale, NULL); LineTo (hDC, x+scl1, y-scl1);
			MoveToEx (hDC, x+scale, y+scale, NULL); LineTo (hDC, x+scl1, y+scl1);
			} break;
		}
		if (label1) TextOut (hDC, x, y-scale, label1, strlen (label1));
		if (label2) TextOut (hDC, x, y+scale+gdires.hFont1_scale, label2, strlen (label2));
		if (local_hdc) ReleaseLabelDC (hDC);
	}
}

void Scene::Render (D3DRECT* vp_rect)
{
	int i, j, k;
	DWORD n;
	Vector col;
	HRESULT res;
	g_vtxcount = g_tilecount = 0;

	// select background colour in atmosphere
	if (g_camera->ProxyPlanet() && g_camera->ProxyPlanet()->HasAtmosphere()) {
		const Planet *pp = g_camera->ProxyPlanet();
		const ATMCONST *atmp = pp->AtmParams();
		Vector pc (g_camera->GPos() - pp->GPos());
		double cdist = pc.length();
		if (cdist < atmp->radlimit) {
			ATMPARAM prm;
			pp->GetAtmParam (cdist-pp->Size(), 0, 0, &prm);
			Vector ps (-pp->GPos());
			ps.unify();
			double coss = (pc & ps) / cdist;
			//double intens = min (1.0,(0.9*coss+0.5)) * sqrt (dns/atmp->rho0);
			double intens = min (1.0,(1.0839*coss+0.4581)) * sqrt (prm.rho/atmp->rho0);
			// => intensity=0 at sun zenith distance 115°
			//    intensity=1 at sun zenith distance 60°
			if (intens > 0.0) {
				col += Vector (atmp->color0.x*intens, atmp->color0.y*intens, atmp->color0.z*intens);
			}
		}
	}
	for (i = 0; i < 3; i++) if (col.data[i] > 1.0) col.data[i] = 1.0;
	bgcol.Set (col);
	D3DCOLOR bg_rgba = D3DRGBA (bgcol.x, bgcol.y, bgcol.z, 1);

    // Clear the viewport
	dev->Clear (0, NULL, D3DCLEAR_TARGET|zclearflag, bg_rgba, 1.0f, 0L);

	float npl = (float)g_camera->Nearplane(), fpl = (float)g_camera->Farplane();
	// remember default fustrum limits

	int ns = nsvtx;
	bglvl = 0;
	if (nsvtx) {
		if (bg_rgba) { // suppress stars darker than the background
			bglvl = (bg_rgba & 0xff) + ((bg_rgba >> 8) & 0xff) + ((bg_rgba >> 16) & 0xff);
			bglvl = min (bglvl/2, 255);
			ns = lvlid[bglvl];
		}
	}

    // Begin the scene
	res = dev->BeginScene ();
	if (FAILED (res)) {
		return; // skip this render instance
	}

	// set lighting
	for (i = 0; i < nstarlight; i++) {
		star_lght->dcvDiffuse = starlight[i].col;
		Vector dir = g_camera->GPos() - *starlight[i].gpos;
		dir.unify();
		star_lght->dvDirection.x = (D3DVALUE)dir.x;
		star_lght->dvDirection.y = (D3DVALUE)dir.y;
		star_lght->dvDirection.z = (D3DVALUE)dir.z;
		dev->SetLight (i, star_lght);
	}
	int nlight = nstarlight;

	if (locallight) {
		DWORD j, k;
		for (i = 0; i < nobj; i++) {
			const Body *body = vobj[i]->GetBody();
			if (body->Type() == OBJTP_VESSEL) {
				Vessel *vessel = (Vessel*)body;
				DWORD nemitter = vessel->LightEmitterCount();
				for (j = 0; j < nemitter; j++) {
					const LightEmitter *em = vessel->GetLightEmitter(j);
					if (!em->IsActive() || !em->GetIntensity()) continue;
					if (g_camera->IsInternal()) {
						if (em->GetVisibility() == LightEmitter::VIS_EXTERNAL)
							continue;
					}
					else {
						if (em->GetVisibility() == LightEmitter::VIS_COCKPIT)
							continue;
					}
					const VECTOR3 *pos = em->GetPositionRef();
					D3DVECTOR q, p = {(float)pos->x, (float)pos->y, (float)pos->z};
					D3DMath_VectorMatrixMultiply (q, p, vobj[i]->MWorld());
					double dst2 = q.x*q.x + q.y*q.y + q.z*q.z;
					for (k = nlight-1; k >= nstarlight; k--) {
						if (lightlist[k].camdist2 < dst2) {
							break;
						} else if (k < maxlight-1) {
							lightlist[k+1] = lightlist[k]; // shift entries to make space
						} else
							nlight--;
					}
					if (k == maxlight-1) continue;
					lightlist[k+1].plight = em;
					lightlist[k+1].vobj = vobj[i];
					lightlist[k+1].camdist2 = dst2;
					nlight++;
				}
			}
		}
		for (i = nstarlight; i < nlight; i++)
			if (lightlist[i].plight->GetVisibility() & LightEmitter::VIS_EXTERNAL)
				AddLocalLight (lightlist[i].plight, lightlist[i].vobj, i);
	}

	dev->SetRenderState (D3DRENDERSTATE_ZENABLE, FALSE);
	dev->SetRenderState (D3DRENDERSTATE_ZWRITEENABLE, FALSE);
	dev->SetRenderState (D3DRENDERSTATE_LIGHTING, FALSE);

	// render background stars, celestial markers and grids
	DWORD flagPItem = g_pOrbiter->Cfg()->CfgVisHelpPrm.flagPlanetarium;
	if (ns || flagPItem & PLN_ENABLE) {
		g_camera->SetFrustumLimits (fpl*0.1, fpl);
		// set limits to some arbitrary values (everything in there is
		// rendered without z-tests)
		dev->SetTransform (D3DTRANSFORMSTATE_WORLD, &ident);
		dev->SetTexture (0,0);
		//dev->SetRenderState (D3DRENDERSTATE_ZENABLE, FALSE);
		//dev->SetRenderState (D3DRENDERSTATE_ZWRITEENABLE, FALSE);
		//dev->SetRenderState (D3DRENDERSTATE_LIGHTING, FALSE);
		if ((flagPItem & PLN_ENABLE) /*&& bglvl < cnstlimit*/) {

			dev->SetTextureStageState (0, D3DTSS_COLORARG1, D3DTA_TFACTOR);
			dev->SetTextureStageState (0, D3DTSS_COLOROP, D3DTOP_SELECTARG1);

			DWORD dstblend;
			dev->GetRenderState (D3DRENDERSTATE_DESTBLEND, &dstblend);
			dev->SetRenderState (D3DRENDERSTATE_DESTBLEND, D3DBLEND_ONE);
			dev->SetRenderState (D3DRENDERSTATE_ALPHABLENDENABLE, TRUE);

			if (flagPItem & PLN_EGRID) {                     // render ecliptic grid
				dev->SetRenderState (D3DRENDERSTATE_TEXTUREFACTOR, D3DRGBA(0,0,0.4,1));
				RenderGrid ((flagPItem & PLN_ECL) == 0);
			}
			if (flagPItem & PLN_ECL) {
				dev->SetRenderState (D3DRENDERSTATE_TEXTUREFACTOR, D3DRGBA(0,0,0.6,1));
				RenderEqLine ();
			}
			if (flagPItem & PLN_CGRID) {                     // render celestial grid
				Planet *p = g_psys->GetPlanet ("Earth");
				D3DMATRIX *prot;
				if (p) {
					static D3DMATRIX rot = {0,0,0,0,  0,0,0,0,  0,0,0,0,  0,0,0,1};
					double eps = p->Obliquity();
					double lan = p->EqLng();
					float coso = (float)cos(eps), sino = (float)sin(eps);
					float cosl = (float)cos(lan), sinl = (float)sin(lan);
					rot._11 = cosl; rot._13 = sinl;
					rot._21 = -sino*sinl; rot._22 = coso; rot._23 = sino*cosl;
					rot._31 = -coso*sinl; rot._32 = -sino; rot._33 = coso*cosl;
					prot = &rot;
				} else { // Default values: J2000
					static double eps = 0.4092797095927;
					static double coso = cos(eps), sino = sin(eps);
					static D3DMATRIX rot = {1.0f,0.0f,0.0f,0.0f,  0.0f,(float)coso,(float)sino,0.0f,  0.0f,-(float)sino,(float)coso,0.0f,  0.0f,0.0f,0.0f,1.0f};
					prot = &rot;
				}
				dev->SetTransform (D3DTRANSFORMSTATE_WORLD, prot);
				dev->SetRenderState (D3DRENDERSTATE_TEXTUREFACTOR, D3DRGBA(0.35,0,0.35,1));
				RenderGrid (false);
				dev->SetRenderState (D3DRENDERSTATE_TEXTUREFACTOR, D3DRGBA(0.6,0,0.6,1));
				RenderEqLine ();
				dev->SetTransform (D3DTRANSFORMSTATE_WORLD, &ident);
			}
			if (flagPItem & PLN_EQU) {                       // render target equator grid
				const Body *ref = g_camera->Target();
				if (ref && ref->Type() == OBJTP_VESSEL) ref = ((Vessel*)ref)->ElRef();
				if (ref && ref->Type() == OBJTP_PLANET) {
					D3DMATRIX rot;
					VMAT_identity (rot);
					SetInvD3DRotation (rot, ref->GRot());
					dev->SetTransform (D3DTRANSFORMSTATE_WORLD, &rot);
					dev->SetRenderState (D3DRENDERSTATE_TEXTUREFACTOR, D3DRGBA(0,0.6,0,1));
					RenderEqLine ();
					dev->SetTransform (D3DTRANSFORMSTATE_WORLD, &ident);
				}
			}
			if ((flagPItem & PLN_CONST) && ncnst) {          // render constellation lines
				dev->SetRenderState (D3DRENDERSTATE_TEXTUREFACTOR, D3DRGBA(cc.x,cc.y,cc.z,1));
				dev->DrawPrimitive (D3DPT_LINELIST, D3DFVF_XYZ, cnstvtx, ncnst*2, 0);
			}
			if ((flagPItem & PLN_CNSTLABEL) && ncnstlabel) { // render constellation labels
				res = vb_target->ProcessVertices (D3DVOP_TRANSFORM, 0, ncnstlabel, vb_cnstlabel, 0, dev, 0);
				if (res == D3D_OK) {
					HDC hDC;
					g_pOrbiter->GetInlineGraphicsClient()->GetRenderTarget()->GetDC (&hDC);
					SelectObject (hDC, gdires.hFont1);
					SetTextAlign (hDC, TA_CENTER | TA_BOTTOM);
					SetTextColor (hDC, 0xA0A0A0);
					SetBkMode (hDC, TRANSPARENT);
					VB_XYZ *vbpos;
					vb_target->Lock (DDLOCK_WAIT | DDLOCK_READONLY | DDLOCK_SURFACEMEMORYPTR, (LPVOID*)&vbpos, NULL);
					bool bfull = (flagPItem & PLN_CNSTLONG) == PLN_CNSTLONG;
					for (n = 0; n < ncnstlabel; n++) {
						if (vbpos[n].z < 0 && vbpos[n].x >= 0 && vbpos[n].y >= 0 && vbpos[n].x < viewW && vbpos[n].y < viewH) {
							TextOut (hDC, (DWORD)vbpos[n].x, (DWORD)vbpos[n].y+gdires.hFont1_scale/2, bfull ? cnstlabel[n].full : cnstlabel[n].abbr, bfull ? cnstlabel[n].len : 3);
						}
					}
					vb_target->Unlock();
					SelectObject (hDC, GetStockObject (SYSTEM_FONT));
					g_pOrbiter->GetInlineGraphicsClient()->GetRenderTarget()->ReleaseDC (hDC);
				}
			}
			dev->SetRenderState (D3DRENDERSTATE_DESTBLEND, dstblend);
			dev->SetRenderState (D3DRENDERSTATE_ALPHABLENDENABLE, FALSE);

			if (flagPItem & PLN_CCMARK) { // celestial markers
				int nlist;
				HDC hDC = 0;
				oapi::GraphicsClient::LABELLIST *list = g_psys->LabelList (&nlist);
				for (i = 0; i < nlist; i++) {
					if (list[i].active) {
						int col = list[i].colour;
						int shape = list[i].shape;
						int size = (int)(viewH/80.0*list[i].size+0.5);
						if (!hDC) hDC = GetLabelDC (1);
						SelectObject (hDC, gdires.hPen[col]);
						SetTextColor (hDC, labelcol[col]);
						const oapi::GraphicsClient::LABELSPEC *uls = list[i].list;
						for (j = 0; j < list[i].length; j++) {
							Vector sp (uls[j].pos.x,uls[j].pos.y,uls[j].pos.z);
							//if (dotp (sp, g_camera->GPos() - sp) >= 0.0) // surface point visible?
							RenderDirectionMarker (sp, uls[j].label[0], uls[j].label[1], hDC, shape, size);
						}
					}
				}
				if (hDC) ReleaseLabelDC (hDC);
			}
			dev->SetTextureStageState (0, D3DTSS_COLORARG1, D3DTA_TEXTURE);
			dev->SetTextureStageState (0, D3DTSS_COLOROP, D3DTOP_MODULATE);
		}
		// stars
		for (i = j = 0; i < ns; i += D3DMAXNUMVERTICES, j++)
			dev->DrawPrimitiveVB (D3DPT_POINTLIST, svtx[j], 0, min (ns-i, D3DMAXNUMVERTICES), 0);

		//dev->SetRenderState (D3DRENDERSTATE_ZENABLE, TRUE);
		//dev->SetRenderState (D3DRENDERSTATE_ZWRITEENABLE, TRUE);
		//dev->SetRenderState (D3DRENDERSTATE_LIGHTING, TRUE);
		g_camera->SetFrustumLimits (npl, fpl); // reset fustrum limits
	}

	if (csphere2) {
		VPlanet::RenderPrm rprm;
		memset(&rprm, 0, sizeof(VPlanet::RenderPrm));
		csphere2->Render(dev, m_WMcsphere, 0, false, rprm);
	}
	else if (csphere) {
		csphere->Render(dev, 8, bglvl);
	}

	dev->SetRenderState (D3DRENDERSTATE_ZENABLE, TRUE);
	dev->SetRenderState (D3DRENDERSTATE_ZWRITEENABLE, TRUE);
	dev->SetRenderState (D3DRENDERSTATE_LIGHTING, TRUE);

	const int MAXPLANET = 256;
	int np;
	static int pidx[MAXPLANET];
	double dist;
	VStar *star_ob = NULL;
	float dmin, dmax;
	bool npl_adjust = (npl < 5.0);
	bool npl_adjusted = false;

	// render planets and stars "distant objects" without zbuffer
	for (i = np = 0; i < nobj; i++) {
		const Body *body = vobj[i]->GetBody();
		if (body->Type() == OBJTP_PLANET || body->Type() == OBJTP_STAR) {
			dist = vobj[i]->CDist();
			for (j = 0; j < np; j++)
				if (dist > vobj[pidx[j]]->CDist()) break;
			for (k = np-1; k >= j; k--) pidx[k+1] = pidx[k];
			pidx[j] = i;
			np++;
		}
	}
	for (i = 0; i < np; i++) {
		VObject *vo = vobj[pidx[i]];
		if (npl_adjust && vo->RenderDistRange (dmin, dmax) && dmin > 5.0f) {
			g_camera->SetFrustumLimits (5.0f, fpl); // reset fustrum limits
			npl_adjusted = true;
		}
		vo->Render (dev);
		if (flagPItem & PLN_ENABLE) {
			if (flagPItem & PLN_CMARK)
				RenderObjectMarker (vo->GetBody()->GPos(), vo->GetBody()->Name(), 0);
			if ((flagPItem & PLN_SURFMARK) && vo->GetBody()->Type() == OBJTP_PLANET) {
				Planet *pl = (Planet*)vo->GetBody();
				double lng, lat, apprad = vo->AppRad()/(0.5*viewH);
				Vector sp;
				if ((flagPItem & PLN_BMARK) && apprad > SURFLABEL_LIMIT) { // mark surface bases
					HDC hDC = GetLabelDC (0);
					for (n = 0; n < pl->nBase(); n++) {
						Base *base = pl->GetBase(n);
						base->EquPos (lng, lat);
						pl->EquatorialToGlobal (lng, lat, pl->Size(), sp);
						if (dotp (sp - pl->GPos(), g_camera->GPos() - sp) >= 0.0) // surface point visible?
							RenderObjectMarker (sp, base->Name(), 0, hDC, 0);
					}
					ReleaseLabelDC (hDC);
				}
				if ((flagPItem & PLN_RMARK) && apprad > VORLABEL_LIMIT && pl->nNav()) { // mark VOR transmitters
					NavManager &navm = pl->NavMgr();
					Vector cloc (tmul (pl->GRot(), g_camera->GPos() - pl->GPos())); // camera in planet coords
					char cbuf[64];
					bool found;
					HDC hDC = GetLabelDC (0);
					for (n = 0; n < navm.nNav(); n++) {
						const Nav *nav = navm.GetNav (n);
						switch (nav->Type()) {
						case TRANSMITTER_VOR:
							((Nav_VOR*)nav)->LPos (sp);
							found = true;
							break;
						default:
							found = false;
							break;
						}
						if (found) {
							if (sp.dist2 (cloc) < 2.5e11 && dotp (sp, cloc-sp) >= 0.0) { // surface point visible?
								sprintf (cbuf, "%0.2f", nav->GetFreq());
								RenderObjectMarker (mul (pl->GRot(), sp) + pl->GPos(), cbuf, 0, hDC, 0);
							}
						}
					}
					ReleaseLabelDC (hDC);
				}
				if (pl->LabelFormat() < 2 && (flagPItem & PLN_LMARK)) { // user-defined planetary surface labels
					int nlist;
					HDC hDC = 0;
					Vector cp, mp;
					oapi::GraphicsClient::LABELLIST *list = pl->LabelList (&nlist);
					for (k = 0; k < nlist; k++) {
						if (list[k].active) {
							if (apprad*list[k].distfac > VORLABEL_LIMIT) {
								int col = list[k].colour;
								int shape = list[k].shape;
								int size = (int)(viewH/80.0*list[k].size+0.5);
								if (!hDC) {
									hDC = GetLabelDC (1);
									cp = tmul (pl->GRot(), g_camera->GPos() - pl->GPos()); // camera in local planet coords
								}
								SelectObject (hDC, gdires.hPen[col]);
								SetTextColor (hDC, labelcol[col]);
								const oapi::GraphicsClient::LABELSPEC *uls = list[k].list;
								for (j = 0; j < list[k].length; j++) {
									mp = MakeVector (uls[j].pos);
									if (dotp (mp, cp-mp) >= 0.0) { // surface point visible?
										sp = mul (pl->GRot(), mp) + pl->GPos();
										RenderObjectMarker (sp, uls[j].label[0], uls[j].label[1], hDC, shape, size);
									}
								}
							}
						}
					}
					if (hDC) ReleaseLabelDC (hDC);
				}
			}
		}
		if (npl_adjusted)
			g_camera->SetFrustumLimits (npl, fpl); // reset fustrum limits

	}

	// render new-style surface markers
	if ((flagPItem & PLN_ENABLE) && (flagPItem & PLN_LMARK)) {
		oapi::Sketchpad *skp = 0;
		int fontidx = -1;
		for (i = 0; i < np; i++) {
			VObject *vo = vobj[pidx[i]];
			if (vo->GetBody()->Type() != OBJTP_PLANET) continue;
			Planet *pl = (Planet*)vo->GetBody();
			if (pl->LabelFormat() == 2) {
				if (!skp) {
					skp = gc->clbkGetSketchpad(0);
					skp->SetPen(label_pen);
				}
				((VPlanet*)vo)->RenderLabels(skp, label_font, &fontidx);
			}
		}
		if (skp)
			gc->clbkReleaseSketchpad (skp);
	}

	// render other objects

	VVessel *focusvis = 0;
	for (i = 0; i < nobj; i++) {
		const Body *body = vobj[i]->GetBody();
		if (body == g_focusobj) {
			focusvis = (VVessel*)vobj[i];
			if (g_camera->IsInternal() && !focusvis->ExtRenderPass()) continue;
			// skip render of internal meshes here
		}
		if (body->Type() != OBJTP_PLANET && body->Type() != OBJTP_SURFBASE && body->Type() != OBJTP_STAR) {
			vobj[i]->Render (dev);
			if ((flagPItem & (PLN_ENABLE|PLN_VMARK)) == (PLN_ENABLE|PLN_VMARK))
				RenderObjectMarker (body->GPos(), body->Name(), 0);
		}
	}

	// render engine exhaust
	dev->SetRenderState (D3DRENDERSTATE_ALPHABLENDENABLE, TRUE);
	for (i = 0; i < nobj; i++) {
		vobj[i]->RenderBeacons (dev);
		if (vobj[i]->GetCaps () & VOCAPS_HASENGINES)
			vobj[i]->RenderExhaust (dev, gtex[0]);
	}

	// render exhaust particle system
	LPDIRECTDRAWSURFACE7 ptex = 0;
	for (n = 0; n < nstream; n++)
		pstream[n]->Render (dev, ptex);
	if (ptex) dev->SetTexture (0, 0);

	// render object vectors - should this be done without z-buffer?
	bool bRenderVectors = true;
	if (bRenderVectors) {
		g_camera->SetFrustumLimits (1.0, 1e30);
		for (i = 0; i < nobj; i++)
			vobj[i]->RenderVectors (dev);
		g_camera->SetFrustumLimits (npl, fpl); // reset fustrum limits
	}

	// render focus object in cockpit view
	if (g_camera->IsInternal() && focusvis && focusvis->bRenderInternal()) {

		// switch cockpit lights on, external-only lights off
		if (locallight) {
			for (i = nstarlight; i < nlight; i++) {
				switch (lightlist[i].plight->GetVisibility()) {
				case LightEmitter::VIS_EXTERNAL:
					dev->LightEnable (i, FALSE);
					break;
				case LightEmitter::VIS_COCKPIT:
					AddLocalLight (lightlist[i].plight, lightlist[i].vobj, i);
					break;
				}
			}
		}
		dev->Clear (0, NULL, zclearflag, 0, 1.0f, 0L);
		g_camera->SetFrustumLimits (0.1, 2.0*g_focusobj->Size());
		focusvis->Render (dev, true);
		g_camera->SetFrustumLimits (npl, fpl);
	}

	if (locallight)
		for (i = nstarlight; i < nlight; i++)
			dev->LightEnable (i, FALSE);

	// render 2D overlay elements (2D panel, HUD, info text)
	gc->Render2DOverlay();

	dev->SetRenderState (D3DRENDERSTATE_ALPHABLENDENABLE, FALSE);
	SetDefaultMaterial();

	// End the scene.
	dev->EndScene();
}

void Scene::RenderVesselShadows ()
{
	static D3DMATERIAL7 shmat_black = {{0,0,0,1},{0,0,0,0},{0,0,0,0},{0,0,0,0},0};
	dev->SetTexture (0,0);

	if (g_pOrbiter->UseStencil()) { // use alpha-blended shadows with stencil buffer
		dev->SetRenderState (D3DRENDERSTATE_ALPHABLENDENABLE, TRUE);
		dev->SetRenderState (D3DRENDERSTATE_STENCILENABLE, TRUE);
		dev->SetRenderState (D3DRENDERSTATE_STENCILREF, 1);
		dev->SetRenderState (D3DRENDERSTATE_STENCILMASK, 1);
		dev->SetRenderState (D3DRENDERSTATE_STENCILFUNC, D3DCMP_NOTEQUAL);
		dev->SetRenderState (D3DRENDERSTATE_STENCILPASS, D3DSTENCILOP_REPLACE);
		dev->SetRenderState (D3DRENDERSTATE_STENCILFAIL, D3DSTENCILOP_KEEP);
		dev->SetRenderState (D3DRENDERSTATE_STENCILZFAIL, D3DSTENCILOP_REPLACE);

	} else {            // use black shadows
		dev->SetMaterial (&shmat_black);
	}
	const Planet *planet = g_camera->ProxyPlanet();
	for (int i = 0; i < nobj; i++)
		if (vobj[i]->GetBody()->Type() == OBJTP_VESSEL)
			((VVessel*)vobj[i])->RenderGroundShadow (dev, planet);

	if (g_pOrbiter->UseStencil())
		dev->SetRenderState (D3DRENDERSTATE_STENCILENABLE, FALSE);
	else
		dev->SetRenderState (D3DRENDERSTATE_ALPHABLENDENABLE, TRUE);

	LPDIRECTDRAWSURFACE7 tex = 0;
	for (DWORD j = 0; j < nstream; j++) {
		pstream[j]->RenderGroundShadow (dev, tex);
	}

	SetDefaultMaterial();
	dev->SetRenderState (D3DRENDERSTATE_ALPHABLENDENABLE, FALSE);
}
