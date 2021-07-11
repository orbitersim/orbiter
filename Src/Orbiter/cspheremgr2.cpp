#include "cspheremgr2.h"
#include "Orbiter.h"
#include "Camera.h"
#include "Texture.h"

extern Orbiter *g_pOrbiter;
extern Camera *g_camera;
extern TextureManager2 *g_texmanager2;
extern DWORD g_vtxcount;
extern DWORD g_tilecount;

CsphereTile::CsphereTile(TileManager2Base *_mgr, int _lvl, int _ilat, int _ilng)
	: Tile(_mgr, _lvl, _ilat, _ilng)
{
	cmgr = static_cast<TileManager2<CsphereTile>* >(_mgr);
	node = 0;
}

CsphereTile::~CsphereTile()
{
}

void CsphereTile::Load()
{
	bool bLoadMip = true; // for now
	bool ok;

	DWORD flag = (bLoadMip ? 0 : 4);
	char path[256];

	ok = false;
	owntex = true;
	if (mgr->Cprm().tileLoadFlags & 0x0001) { // try loading from individual tile file
		sprintf(path, "csphere\\%s\\Surf\\%02d\\%06d\\%06d", cmgr->getName(), lvl + 4, ilat, ilng);
		ok = (g_texmanager2->OpenTexture(path, ".dds", 0, &tex, flag) != 0);
	}
	if (!ok && cmgr->treeMgr[0]) { // try loading from compressed archive
		BYTE *buf;
		DWORD ndata = cmgr->treeMgr[0]->ReadData(lvl + 4, ilat, ilng, &buf);
		if (ndata) {
			ok = (g_texmanager2->ReadCompatibleSurfaceFromMemory(buf, ndata, &tex, flag) == S_OK);
			cmgr->treeMgr[0]->ReleaseData(buf);
		}
	}
	if (!ok) { // no texture found - interpolate subregion from ancestor
		if (GetParentSubTexRange(&texrange)) {
			tex = getCsphereParent()->Tex();
			owntex = false;
		}
		else {
			tex = 0;
		}
	}

	bool shift_origin = (lvl >= 4);
	int res = mgr->GridRes();
	if (!lvl) {
		mesh = CreateMesh_hemisphere(res, 0, mean_elev);
	}
	else {
		mesh = CreateMesh_quadpatch(res, res, 0, 1.0, mean_elev, &texrange, shift_origin, &vtxshift);
	}
}

void CsphereTile::Render()
{
	if (!mesh) return;
	g_tilecount++;
	g_vtxcount += mesh->nv;

	TileManager2Base::Dev()->SetTexture(0, tex);
	LPDIRECT3DVERTEXBUFFER7 vb = mesh->vb;        // processed vertex buffer
	mgr->DrawIndexedPrimitiveVB(D3DPT_TRIANGLELIST, vb, 0,
		mesh->nv, mesh->idx, mesh->ni, 0);
}

// ============================================================================

template<>
void TileManager2Base::ProcessNode(QuadTreeNode<CsphereTile> *node)
{
	const int tgtres = 8; // for now

	Tile *tile = node->Entry();
	tile->SetState(Tile::ForRender);
	int lvl = tile->lvl;
	int ilng = tile->ilng;
	int ilat = tile->ilat;
	int nlng = 2 << lvl;
	int nlat = 1 << lvl;
	bool bstepdown = true;

	// Check if patch bounding box intersects viewport
	MATRIX4 transform = mul(WorldMatrix(ilng, nlng, ilat, nlat), prm.dviewproj);
	if (!tile->InView(transform)) {
		if (lvl == 0)
			bstepdown = false;
		else {
			node->DelChildren();
			tile->SetState(Tile::Invisible);
			return;
		}
	}

	if (bstepdown) {
		bstepdown = (lvl < tgtres);
	}

	if (bstepdown) {
		bool subcomplete = true;
		int i, idx;
		for (idx = 0; idx < 4; idx++) {
			QuadTreeNode<CsphereTile> *child = node->Child(idx);
			if (!child)
				child = LoadChildNode(node, idx);
			else if (child->Entry()->state == Tile::Invalid) {
				if (bTileLoadThread)
					loader->LoadTileAsync(child->Entry());
				else {
					child->Entry()->Load();
					child->Entry()->state = Tile::Inactive;
				}
			}
			Tile::TileState state = child->Entry()->state;
			if (!(state & TILE_VALID))
				subcomplete = false;
		}
		if (subcomplete) {
			tile->state = Tile::Active;
			for (i = 0; i < 4; i++)
				ProcessNode(node->Child(i));
			return;
		}
	}

	if (!bstepdown)
		node->DelChildren();
}

// ============================================================================

template<>
TileManager2<CsphereTile>::TileManager2(const char *name, int _maxres, int _gridres)
	: TileManager2Base(0, _maxres, _gridres)
{
	m_name = new char[strlen(name) + 1];
	strcpy(m_name, name);

	// Initialise the compressed packed tile archives
	ntreeMgr = 0;
	LoadZTrees();

	// Load the low-res full-sphere tiles
	for (int i = 0; i < 3; i++) {
		globtile[i] = new CsphereTile(this, i - 3, 0, 0);
		globtile[i]->Load();
	}

	// Set the root tiles for quadtree level 0
	for (int i = 0; i < 2; i++) {
		tiletree[i].SetEntry(new CsphereTile(this, 0, 0, i));
		tiletree[i].Entry()->Load();
	}
}

template<>
MATRIX4 CsphereManager::WorldMatrix(int ilng, int nlng, int ilat, int nlat)
{

	double lat, lng = Pi2 * (double)ilng / (double)nlng + Pi; // add pi so texture wraps at +-180°
	double slng = sin(lng), clng = cos(lng);
	MATRIX4 lrot = { clng,0,slng,0,  0,1.0,0,0,  -slng,0,clng,0,  0,0,0,1.0 };

	//MATRIX4 wm = { 2000.0,0,0,0, 0,2000.0,0,0, 0,0,2000.0,0, 0,0,0,1 }; // now need to add camera offset
	return mul(lrot, prm.dwmat);
}

// -----------------------------------------------------------------------

template<>
void CsphereManager::Render(LPDIRECT3DDEVICE7 dev, MATRIX4 &dwmat, VPlanet *vbody, bool use_zbuf, const VPlanet::RenderPrm &rprm)
{
	int i;
	VPlanet::RenderPrm vprm;
	memset(&vprm, 0, sizeof(VPlanet::RenderPrm));

	prm.dwmat = dwmat;

	// build a transformation matrix for frustum testing
	MATRIX4 Mproj = g_camera->ProjectionMatrix();
	Mproj.m33 = 1.0; Mproj.m43 = -1.0;  // adjust near plane to 1, far plane to infinity
	MATRIX4 Mview = g_camera->ViewMatrix();
	prm.dviewproj = mul(Mview, Mproj);

	loader->WaitForMutex();

	for (i = 0; i < 2; i++)
		ProcessNode(tiletree + i);

	for (i = 0; i < 2; i++)
		RenderNode(tiletree + i);

	loader->ReleaseMutex();
}

// -----------------------------------------------------------------------

template<>
void CsphereManager::LoadZTrees()
{
	treeMgr = new ZTreeMgr*[ntreeMgr = 1];
	if (cprm.tileLoadFlags & 0x0002) {
		char cbuf[256];
		//char path[256] = "csphere\\";
		//strcat(path, m_name);
		g_pOrbiter->Cfg()->PTexPath(cbuf, m_name);
		treeMgr[0] = ZTreeMgr::CreateFromFile(cbuf, ZTreeMgr::LAYER_SURF);
	}
	else {
		for (int i = 0; i < ntreeMgr; i++)
			treeMgr[i] = 0;
	}
}