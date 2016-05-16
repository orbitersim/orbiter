// ==============================================================
// Mesh.h
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// Dual licensed under GPL v3 and LGPL v3
// Copyright (C) 2006-2016 Martin Schweiger
//				 2012-2016 Jarmo Nikkanen
// ==============================================================

// ==============================================================
// class D3D9Mesh (interface)
//
// This class represents a mesh in terms of DX7 interface elements
// (vertex buffers, index lists, materials, textures) which allow
// it to be rendered to the D3D9 device.
// ==============================================================

#ifndef __MESH_H
#define __MESH_H

#include "D3D9Client.h"
#include "D3D9Effect.h"
#include "AABBUtil.h"
#include <d3d9.h> 
#include <d3dx9.h>
#include <vector>

const DWORD SPEC_DEFAULT = (DWORD)(-1); // "default" material/texture flag
const DWORD SPEC_INHERIT = (DWORD)(-2); // "inherit" material/texture flag

#define RENDER_VESSEL		0
#define RENDER_BASE			1
#define RENDER_ASTEROID		2
#define RENDER_BASETILES	3
#define RENDER_VC			4
#define RENDER_BASEBS		5
#define RENDER_CUSTOM		6

#define ENVMAP_MIRROR		0


struct _LightList {
	int		idx;
	float	illuminace;
};

 
/**
 * \brief Mesh object with D3D9-specific vertex buffer
 *
 * Meshes consist of one or more vertex groups, and a set of materials and
 * textures.
 */

class D3D9Mesh : private D3D9Effect 
{

public:

	bool bCanRenderFast;		// Mesh doesn't contain any advanced features in any group
	bool bIsReflective;			// Mesh has a reflective material in one or more groups

	D9BBox BBox;

	D3DXMATRIX InstMatrix[8];
	
	struct GROUPREC {			// mesh group definition
		DWORD VertOff;			// Main mesh Vertex Offset
		DWORD FaceOff;			// Main mesh Index Offset
		//------------------------------------------------
		DWORD GeoVOff;			// Geometry Vertex Offset
		DWORD GeoFOff;			// Geometry Face Offset
		DWORD GeoBIdx;			// Geometry Base Index
		//------------------------------------------------
		DWORD nFace;			// Face count
		DWORD nVert;			// Vertex count
		//------------------------------------------------
		DWORD MtrlIdx;			// material index
		DWORD TexIdx;			// texture index 0=None
		DWORD UsrFlag;			// user-defined flag
		WORD  IntFlag;			// internal flags
		WORD  GeometryRec;		// Geometry record ID
		WORD  zBias;
		WORD  MFDScreenId;		// MFD screen ID + 1
		bool  bTransform;
		bool  bUpdate;			// Bounding box update required
		bool  bGrouped;
		bool  bDualSided;
		bool  bDeleted;			// This entry is deleted by DelGroup()
		bool  bRendered;		
		bool  bAdvanced;		// This group reguires more advanced shader than default one
		bool  bReflective;		// Requires enabling env map 
		D3DXMATRIX  Transform;	// Group specific transformation matrix
		D9BBox BBox;
		DWORD TexIdxEx[MAXTEX];
		float TexMixEx[MAXTEX];
	};

	struct GEOMREC {
		DWORD  nVert;
		DWORD  nFace;
		DWORD  nGrp;
		DWORD  VertOff;
		DWORD  FaceOff;
		DWORD  Flags;
		WORD   GrpIdx[8];
		bool   bBroken;
		bool   bNoShadow;
	};

	explicit		D3D9Mesh(const char *name);
					D3D9Mesh(const D3D9Mesh &mesh);
					D3D9Mesh(class AdMesh &mesh, bool bHasUV=true);
					
					/**
					 * \brief Create a mesh consisting of a single mesh group
					 * \param client graphics client
					 * \param grp vertex group definition
					 * \param deepcopy if true, group contents are copied; otherwise, group
					 *   definition pointer is used directly
					 */
					D3D9Mesh(DWORD nGrp, const MESHGROUPEX **hGroup, const SURFHANDLE *hSurf);
					D3D9Mesh(const MESHGROUPEX *pGroup, const MATERIAL *pMat, D3D9ClientSurface *pTex);
					D3D9Mesh(MESHHANDLE hMesh, bool asTemplate=false);
					~D3D9Mesh();

	void			LoadMeshFromHandle(MESHHANDLE hMesh, bool asTemplate);
	void			UnLockVertexBuffer();
	void			UnLockIndexBuffer();
	NMVERTEX *		LockVertexBuffer(DWORD grp, DWORD flags);
	WORD *			LockIndexBuffer(DWORD grp, DWORD flags);
	
	void			SetName(const char *name);
	const char *	GetName() const { return name; }

	
	

	/**
	 * \brief Check if a mesh is casting shadows
	 * \return Returns true if the mesh is casting shadows.
	 */
	bool			HasShadow();

	/**
	 * \brief Returns a pointer to a mesh group.
	 * \param idx group index (>= 0)
	 * \return Pointer to group structure.
	 */
	const GROUPREC * GetGroup(DWORD idx) const;
	void            SetMFDScreenId(DWORD idx, WORD id);
	void			SetDualSided(DWORD idx, bool bState) { Grp[idx].bDualSided = bState; }

	/**
	 * \brief Returns number of material specifications.
	 * \return Number of materials.
	 */
	SURFHANDLE		GetTexture(DWORD idx) const { return Tex[idx]; } 
	bool			HasTexture(SURFHANDLE hSurf);
	bool			IsReflective() const { return bIsReflective; }

	/**
	 * \brief returns a pointer to a material definition.
	 * \param idx material index (>= 0)
	 * \return Pointer to material object.
	 */
	const D3D9MatExt *	GetMaterial(DWORD idx) const;
	bool			GetMaterial(D3D9MatExt *pMat, DWORD idx) const;
	void			SetMaterial(const D3D9MatExt *pMat, DWORD idx, bool bUpdateStatus = true);
	void			SetMaterial(const D3DMATERIAL9 *pMat, DWORD idx, bool bUpdateStatus = true);
	bool			GetTexTune(D3D9Tune *pT, DWORD idx) const;
	void			SetTexTune(const D3D9Tune *pT, DWORD idx);

	DWORD			GetGroupCount() const { return nGrp; }
	DWORD			GetMaterialCount() const { return nMtrl; }
	DWORD			GetTextureCount() const { return nTex; }
	DWORD			GetVertexCount(int grp=-1) const;
	DWORD			GetIndexCount(int grp=-1) const;

	DWORD			GetMeshGroupMaterialIdx(DWORD grp) const;
	DWORD			GetMeshGroupTextureIdx(DWORD grp) const;
	DWORD			GetGroupTransformCount() const;
	D3DXVECTOR3		GetBoundingSpherePos();
	float			GetBoundingSphereRadius();
	D9BBox *		GetAABB();
	D3DXVECTOR3		GetGroupSize(DWORD idx);
	LPD3DXMATRIX	GetTransform() { if (bGlobalTF) return &mTransform; else return NULL; }

	void			SetPosition(VECTOR3 &pos);
	void			SetRotation(D3DXMATRIX &rot);

	/**
	 * \brief Replace a mesh texture.
	 * \param texidx texture index (>= 0)
	 * \param tex texture handle
	 * \return \e true on success, \e false otherwise.
	 */
	bool			SetTexture(DWORD texidx, LPD3D9CLIENTSURFACE tex);
	void			SetTexMixture (DWORD ntex, float mix);

	void			RenderGroup(const GROUPREC *grp);
	void			RenderGroup(int idx);
	void			RenderBaseTile(const LPD3DXMATRIX pW);
	void			RenderBoundingBox(const LPD3DXMATRIX pW);
	void			Render(const LPD3DXMATRIX pW, int iTech=RENDER_VESSEL, LPDIRECT3DCUBETEXTURE9 *pEnv=NULL, int nEnv=0);
	void			RenderFast(const LPD3DXMATRIX pW, int iTech);
	void			RenderShadows(float alpha, const LPD3DXMATRIX pW);
	void			RenderShadowsEx(float alpha, const LPD3DXMATRIX pP, const LPD3DXMATRIX pW, const D3DXVECTOR4 *light, const D3DXVECTOR4 *param);
	void			RenderRings(const LPD3DXMATRIX pW, LPDIRECT3DTEXTURE9 pTex);
	void			RenderRings2(const LPD3DXMATRIX pW, LPDIRECT3DTEXTURE9 pTex, float irad, float orad);
	void			RenderAxisVector(LPD3DXMATRIX pW, const LPD3DXCOLOR pColor, float len);
	
	void			CheckMeshStatus();
	void			ConvertToDynamic();
	void			ResetTransformations();
	void			TransformGroup(DWORD n, const D3DXMATRIX *m);
	void			Transform(const D3DXMATRIX *m);
	int				GetGroup (DWORD grp, GROUPREQUESTSPEC *grs);
	void			DynamicGroup(DWORD idx);
	int				EditGroup (DWORD grp, GROUPEDITSPEC *ges);
	void			UpdateGroupEx(DWORD idx, const MESHGROUPEX *mg);

	void			SetSunLight(const D3D9Sun *pLight);
	
	D3D9Pick		Pick(const LPD3DXMATRIX pW, const D3DXVECTOR3 *vDir);
	
	void			UpdateBoundingBox();
	void			BoundingBox(const NMVERTEX *vtx, DWORD n, D9BBox *box);

	void			SetAmbientColor(D3DCOLOR c);
	void			SetupFog(const LPD3DXMATRIX pW);

	void			DumpTextures();
	void			DumpGroups();

	/**
	 * \brief Enable/disable material alpha value for transparency calculation.
	 * \param enable flag for enabling/disabling material alpha calculation.
	 * \note By default, material alpha values are ignored for mesh groups
	 *   with textures, and the texture alpha values are used instead.
	 *   By enabling material alpha calculation, the final alpha value is
	 *   calculated as the product of material and texture alpha value.
	 */
	inline void		EnableMatAlpha (bool enable) { bModulateMatAlpha = enable; }
	
	DWORD			AddTexture(D3D9ClientSurface *pTex);
	DWORD			AddMaterial(D3D9MatExt *pMat);
	void			SetMeshGroupTextureIdx(DWORD grp, DWORD tex_idx);
	void			SetMeshGroupMaterialIdx(DWORD grp, DWORD mtrl_idx);
	bool			Bake();

private:

	void UpdateTangentSpace(NMVERTEX *pVrt, WORD *pIdx, DWORD nVtx, DWORD nFace, bool bTextured);
	void ProcessInherit();
	bool CopyVertices(GROUPREC *grp, const MESHGROUPEX *mg);
	void SetGroupRec(DWORD i, const MESHGROUPEX *mg);
	void CreateGeometryBuffers();
	void UpdateGeometryBuffer();
	void Null();

	LPDIRECT3DVERTEXBUFFER9 pVB; ///< (Local) Vertex buffer pointer
	LPDIRECT3DVERTEXBUFFER9 pGB;
	LPDIRECT3DINDEXBUFFER9  pIB;
	LPDIRECT3DINDEXBUFFER9  pGI;
	
	DWORD	MaxVert;
	DWORD	MaxFace;
	DWORD   Constr;

	GROUPREC *Grp;              // list of mesh groups
	GEOMREC *Geom;				// Geometry record
	DWORD nGrp;                 // number of mesh groups
	DWORD nGeom;				// number of geometry groups
	DWORD nMtrl;                // number of mesh materials
	DWORD nTex;                 // number of mesh textures
	D3D9MatExt *Mtrl;           // list of mesh materials
	LPD3D9CLIENTSURFACE *Tex;	// list of mesh textures
	D3D9Tune *pTune;
	D3DXMATRIX mTransform;
	D3DXMATRIX mTransformInv;
	D3DXMATRIX *pGrpTF;
	const D3D9Sun *sunLight;
	D3DCOLOR cAmbient;
	
	_LightList LightList[MAX_SCENE_LIGHTS];
	LightStruct Locals[MAX_MESH_LIGHTS];
	

	bool bDynamic;				// Mesh is using a dynamic vertex buffer for faster read-modify-write 
	bool bTemplate;             // mesh used as template only (not for rendering)
	bool bBSRecompute;			// Bounding sphere must be recomputed
	bool bBSRecomputeAll;
	bool bModulateMatAlpha;     // mix material and texture alpha channels
	bool bGlobalTF;				// Mesh has a valid mTransform matrix

	char name[128];
};

#endif // !__MESH_H