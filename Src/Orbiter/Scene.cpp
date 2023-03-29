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
	m_celSphere = new OGCelestialSphere(gc, this);
	gcanvas = 0;
	RegisterDevices (og->GetDevice());
	InitGDIResources();
	sunvis = false;
	if (!og->clbkGetRenderParam (RP_MAXLIGHTS, &maxlight) || (LONG)maxlight < 0) maxlight = 8;
	if (g_pOrbiter->Cfg()->CfgVisualPrm.MaxLight)
		maxlight = min (maxlight, g_pOrbiter->Cfg()->CfgVisualPrm.MaxLight);
	locallight = g_pOrbiter->Cfg()->CfgVisualPrm.bLocalLight;
	if (locallight)
		lightlist = new LIGHTLIST[maxlight];

	atmidx = 0;

	zclearflag = D3DCLEAR_ZBUFFER;
	bool bstencil = (g_pOrbiter->Cfg()->CfgDevPrm.bTryStencil &&
		og->clbkGetRenderParam (RP_STENCILDEPTH, &val) && val >= 1);
	if (bstencil) zclearflag |= D3DCLEAR_STENCIL;
	// use stencil buffers (for shadow rendering etc.)

	star_lght = (g_pOrbiter->Cfg()->CfgVisualPrm.bSpecular ? &starlight_specular : &starlight_nospecular);
	Mesh::GlobalEnableSpecular (g_pOrbiter->Cfg()->CfgVisualPrm.bSpecular);

	//csphere = new CSphereManager;

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
	delete m_celSphere;
	if (nsun) delete []vsun;
	if (nstarlight) delete []starlight;

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

	ExitGDIResources();

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

void Scene::InitGDIResources ()
{
	const int fsize[4] = { 12, 16, 20, 26 };
	for (int i = 0; i < 4; i++)
		labelFont[i] = gc->clbkCreateFont(fsize[i], true, "Arial", FontStyle::FONT_BOLD);
}

void Scene::ExitGDIResources ()
{
	for (int i = 0; i < 4; i++)
		gc->clbkReleaseFont(labelFont[i]);
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

void Scene::OnOptionChanged(int cat, int item)
{
	if (cat == OPTCAT_CELSPHERE)
		m_celSphere->OnOptionChanged(cat, item);
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

	if (force) {
		i0 = 0;
		i1 = psys->nObj();
	}
	else {
		if (cobj >= psys->nObj())
			cobj = 0;
		i0 = cobj++;
		i1 = i0 + 1;
	}

	// check visibility from any camera
	for (i = i0; i < i1; i++) {
		Body *body = psys->GetObj(i);
		if (body->Type() == OBJTP_STAR) { // we assume stars are always visible
			if (!body->GetVishandle())
				AddVisStar (body);
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
	starlight[nstarlight].col = VObject::ColorToD3D(star->GetLightColor());
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

void Scene::Render3DLabel (const Vector &gp, const char *label, double scale, DWORD colour)
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
			for (const char *c = label; *c; c++) {
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

VECTOR3 Scene::SkyColour()
{
	Vector col;
	const Planet* pp = g_camera->ProxyPlanet();
	if (pp && pp->HasAtmosphere()) {
		const ATMCONST* atmp = pp->AtmParams();
		Vector pc(g_camera->GPos() - pp->GPos());
		double cdist = pc.length();
		if (cdist < atmp->radlimit) {
			ATMPARAM prm;
			pp->GetAtmParam(cdist - pp->Size(), 0, 0, &prm);
			Vector ps(-pp->GPos());
			ps.unify();
			double coss = (pc & ps) / cdist;
			double intens = min(1.0, (1.0839 * coss + 0.4581)) * sqrt(prm.rho / atmp->rho0);
			// => intensity=0 at sun zenith distance 115°
			//    intensity=1 at sun zenith distance 60°
			if (intens > 0.0)
				col += Vector(atmp->color0.x * intens, atmp->color0.y * intens, atmp->color0.z * intens);
		}
		for (int i = 0; i < 3; i++)
			if (col.data[i] > 1.0) col.data[i] = 1.0;
	}
	return MakeVECTOR3(col);
}

void Scene::RenderObjectMarker (oapi::Sketchpad* pSkp, const Vector &gpos, const std::string& label1, const std::string& label2, int mode, int scale)
{
	m_celSphere->RenderMarker(pSkp, MakeVECTOR3((gpos - g_camera->GPos()).unit()), label1, label2, mode, scale);
}

void Scene::Render (D3DRECT* vp_rect)
{
	int i, j, k;
	DWORD n;
	Vector col;
	HRESULT res;
	g_vtxcount = g_tilecount = 0;

	bgcol = SkyColour();
	double bglvl = (bgcol.x + bgcol.y + bgcol.z) / 3.0;
	atmidx = min(255, (int)(bglvl * 1.5 * 255.0));

    // Clear the viewport
	D3DCOLOR bg_rgba = D3DRGBA(bgcol.x, bgcol.y, bgcol.z, 1);
	dev->Clear (0, NULL, D3DCLEAR_TARGET|zclearflag, bg_rgba, 1.0f, 0L);

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

	// turn off z-buffer for rendering celestial sphere
	dev->SetRenderState(D3DRENDERSTATE_ZENABLE, FALSE);
	dev->SetRenderState(D3DRENDERSTATE_ZVISIBLE, FALSE);
	dev->SetRenderState(D3DRENDERSTATE_ZWRITEENABLE, FALSE);

	// stretch the z limits to make sure everything is rendered (z-fighting
	// is not an issue here because everything is rendered without z-tests)
	double npl = g_camera->Nearplane();
	double fpl = g_camera->Farplane();
	g_camera->SetFrustumLimits (0.1, 10);

	// render the celestial sphere background
	m_celSphere->Render(dev, bgcol);

	// reset clipping planes and turn z-buffer back on
	g_camera->SetFrustumLimits(npl, fpl);
	dev->SetRenderState(D3DRENDERSTATE_ZENABLE, TRUE);
	dev->SetRenderState(D3DRENDERSTATE_ZVISIBLE, TRUE);
	dev->SetRenderState(D3DRENDERSTATE_ZWRITEENABLE, TRUE);

	const int MAXPLANET = 256;
	int np;
	static int pidx[MAXPLANET];
	double dist;
	VStar *star_ob = NULL;
	float dmin, dmax;
	bool npl_adjust = (npl < 5.0);
	bool npl_adjusted = false;

	DWORD flagMItem = g_pOrbiter->Cfg()->CfgVisHelpPrm.flagMarkers;

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
		if (flagMItem & MKR_ENABLE) {
			oapi::Sketchpad* pSkp = nullptr;
			oapi::Font* font = m_celSphere->MarkerFont();
			if (flagMItem & MKR_CMARK) {
				m_celSphere->EnsureMarkerDrawingContext(&pSkp, font, m_celSphere->MarkerColor(0), m_celSphere->MarkerPen(0));
				font = nullptr;
				RenderObjectMarker(pSkp, vo->GetBody()->GPos(), std::string(vo->GetBody()->Name()), std::string());
			}
			if ((flagMItem & MKR_SURFMARK) && (vo->GetBody()->Type() == OBJTP_PLANET)) {
				m_celSphere->EnsureMarkerDrawingContext(&pSkp, font, m_celSphere->MarkerColor(0), m_celSphere->MarkerPen(0));
				font = nullptr;
				Planet* pl = (Planet*)vo->GetBody();
				double lng, lat, apprad = vo->AppRad() / (0.5 * viewH);
				Vector sp;
				if ((flagMItem & MKR_BMARK) && apprad > SURFLABEL_LIMIT) { // mark surface bases
					for (n = 0; n < pl->nBase(); n++) {
						Base* base = pl->GetBase(n);
						base->EquPos(lng, lat);
						pl->EquatorialToGlobal(lng, lat, pl->Size(), sp);
						if (dotp(sp - pl->GPos(), g_camera->GPos() - sp) >= 0.0) // surface point visible?
							RenderObjectMarker(pSkp, sp, std::string(base->Name()), std::string(), 0);
					}
				}
				if ((flagMItem & MKR_RMARK) && apprad > VORLABEL_LIMIT && pl->nNav()) { // mark VOR transmitters
					NavManager& navm = pl->NavMgr();
					Vector cloc(tmul(pl->GRot(), g_camera->GPos() - pl->GPos())); // camera in planet coords
					char cbuf[64];
					bool found;
					for (n = 0; n < navm.nNav(); n++) {
						const Nav* nav = navm.GetNav(n);
						switch (nav->Type()) {
						case TRANSMITTER_VOR:
							((Nav_VOR*)nav)->LPos(sp);
							found = true;
							break;
						default:
							found = false;
							break;
						}
						if (found) {
							if (sp.dist2(cloc) < 2.5e11 && dotp(sp, cloc - sp) >= 0.0) { // surface point visible?
								sprintf(cbuf, "%0.2f", nav->GetFreq());
								RenderObjectMarker(pSkp, mul(pl->GRot(), sp) + pl->GPos(), std::string(cbuf), std::string(), 0);
							}
						}
					}
				}
				if (pl->LabelFormat() < 2 && (flagMItem & MKR_LMARK)) { // user-defined planetary surface labels
					int nlist;
					Vector cp, mp;
					bool bNeedSetup = true;
					oapi::GraphicsClient::LABELLIST* list = pl->LabelList(&nlist);
					for (k = 0; k < nlist; k++) {
						if (list[k].active) {
							if (apprad * list[k].distfac > VORLABEL_LIMIT) {
								int col = list[k].colour;
								int shape = list[k].shape;
								int size = (int)(viewH / 80.0 * list[k].size + 0.5);
								m_celSphere->EnsureMarkerDrawingContext(&pSkp, font, m_celSphere->MarkerColor(col), m_celSphere->MarkerPen(col));
								font = nullptr;
								if (bNeedSetup) {
									cp = tmul(pl->GRot(), g_camera->GPos() - pl->GPos()); // camera in local planet coords
									bNeedSetup = false;
								}
								const std::vector< oapi::GraphicsClient::LABELSPEC>& uls = list[k].marker;
								for (j = 0; j < uls.size(); j++) {
									mp = MakeVector(uls[j].pos);
									if (dotp(mp, cp - mp) >= 0.0) { // surface point visible?
										sp = mul(pl->GRot(), mp) + pl->GPos();
										RenderObjectMarker(pSkp, sp, uls[j].label[0], uls[j].label[1], shape, size);
									}
								}
							}
						}
					}
				}
			}
			if (pSkp)
				gc->clbkReleaseSketchpad(pSkp);
		}
		if (npl_adjusted)
			g_camera->SetFrustumLimits (npl, fpl); // reset fustrum limits

	}

	// render new-style surface markers
	if ((flagMItem & MKR_ENABLE) && (flagMItem & MKR_LMARK)) {
		oapi::Sketchpad *pSkp = 0;
		int fontidx = -1;
		for (i = 0; i < np; i++) {
			VObject *vo = vobj[pidx[i]];
			if (vo->GetBody()->Type() != OBJTP_PLANET) continue;
			Planet *pl = (Planet*)vo->GetBody();
			if (pl->LabelFormat() == 2) {
				m_celSphere->EnsureMarkerDrawingContext(&pSkp, 0, 0, m_celSphere->MarkerPen(6));
				((VPlanet*)vo)->RenderLabels(pSkp, labelFont, &fontidx);
			}
		}
		if (pSkp)
			gc->clbkReleaseSketchpad (pSkp);
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
		}
	}

	if ((flagMItem & (MKR_ENABLE | MKR_VMARK)) == (MKR_ENABLE | MKR_VMARK)) {
		oapi::Sketchpad* pSkp = nullptr;
		oapi::Font* font = m_celSphere->MarkerFont();
		oapi::Pen* pen = m_celSphere->MarkerPen(0);
		COLORREF col = m_celSphere->MarkerColor(0);
		for (i = 0; i < nobj; i++) {
			const Body* body = vobj[i]->GetBody();
			if (body->Type() != OBJTP_PLANET && body->Type() != OBJTP_SURFBASE && body->Type() != OBJTP_STAR) {
				m_celSphere->EnsureMarkerDrawingContext(&pSkp, font, col, pen);
				font = nullptr;
				pen = nullptr;
				col = 0;
				RenderObjectMarker (pSkp, body->GPos(), std::string(body->Name()), std::string());
			}
		}
		if (pSkp)
			gc->clbkReleaseSketchpad(pSkp);
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

	// render object vectors
	if (*(DWORD*)gc->GetConfigParam(CFGPRM_FORCEVECTORFLAG) & BFV_ENABLE || *(DWORD*)gc->GetConfigParam(CFGPRM_FRAMEAXISFLAG) & FAV_ENABLE) {
		g_camera->SetFrustumLimits(1.0, 1e30);
		RenderVectors();
		g_camera->SetFrustumLimits(npl, fpl); // reset fustrum limits
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

void Scene::RenderVectors()
{
	dev->SetRenderState(D3DRENDERSTATE_ZENABLE, FALSE);
	dev->SetTextureStageState(0, D3DTSS_ALPHAARG1, D3DTA_TFACTOR);
	dev->SetTextureStageState(0, D3DTSS_ALPHAOP, D3DTOP_SELECTARG1);

	dev->SetTextureStageState(0, D3DTSS_COLORARG1, D3DTA_TFACTOR);
	dev->SetTextureStageState(0, D3DTSS_COLORARG2, D3DTA_DIFFUSE);
	dev->SetTextureStageState(0, D3DTSS_COLOROP, D3DTOP_MODULATE);

	dev->SetRenderState(D3DRENDERSTATE_SPECULARENABLE, TRUE);
	dev->SetTextureStageState(0, D3DTSS_MAGFILTER, D3DTFG_POINT);
	dev->SetTextureStageState(0, D3DTSS_MINFILTER, D3DTFN_POINT);
	dev->SetTexture(0, 0);
	D3DMATERIAL7 pmtrl, mtrl = { {1,1,1,1},{1,1,1,1},{1,1,1,1},{0.2,0.2,0.2,1},40 };
	dev->GetMaterial(&pmtrl);
	dev->SetMaterial(&mtrl);

	for (int i = 0; i < nobj; i++)
		vobj[i]->RenderVectors(dev);

	dev->SetTextureStageState(0, D3DTSS_ALPHAARG1, D3DTA_TEXTURE);
	dev->SetTextureStageState(0, D3DTSS_COLORARG1, D3DTA_TEXTURE);
	dev->SetTextureStageState(0, D3DTSS_COLORARG2, D3DTA_CURRENT);
	dev->SetRenderState(D3DRENDERSTATE_SPECULARENABLE, FALSE);
	dev->SetTextureStageState(0, D3DTSS_MAGFILTER, D3DTFG_LINEAR);
	dev->SetTextureStageState(0, D3DTSS_MINFILTER, D3DTFN_LINEAR);
	dev->SetMaterial(&pmtrl);

	for (int i = 0; i < nobj; i++)
		vobj[i]->RenderVectorLabels(dev);

	dev->SetRenderState(D3DRENDERSTATE_ZENABLE, TRUE);
}