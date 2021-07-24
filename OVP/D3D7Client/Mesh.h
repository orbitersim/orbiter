// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ==============================================================
//   ORBITER VISUALISATION PROJECT (OVP)
//   D3D7 Client module
// ==============================================================

// ==============================================================
// Mesh.h
// class D3D7Mesh (interface)
//
// This class represents a mesh in terms of DX7 interface elements
// (vertex buffers, index lists, materials, textures) which allow
// it to be rendered to the D3D7 device.
// ==============================================================

#ifndef __MESH_H
#define __MESH_H

#include "D3D7Client.h"
#include <d3d.h>

const DWORD SPEC_DEFAULT = (DWORD)(-1); // "default" material/texture flag
const DWORD SPEC_INHERIT = (DWORD)(-2); // "inherit" material/texture flag

/**
 * \brief Mesh object with D3D7-specific vertex buffer
 *
 * Meshes consist of one or more vertex groups, and a set of materials and
 * textures.
 */
class D3D7Mesh {
public:
	struct GROUPREC {  // mesh group definition
		DWORD nVtx;           // number of vertices
		DWORD nIdx;           // number of indices
		LPDIRECT3DVERTEXBUFFER7 VtxBuf; // vertex buffer
		WORD *Idx;            // vertex index list
		DWORD MtrlIdx;        // material index
		DWORD TexIdx;         // texture indices
		DWORD UsrFlag;        // user-defined flag
		WORD zBias;           // z-bias value
		WORD IntFlag;         // internal flags
		DWORD TexIdxEx[MAXTEX];
		float TexMixEx[MAXTEX];
	};

	/**
	 * \brief Create an empty mesh
	 * \param client graphics client
	 */
	D3D7Mesh (const oapi::D3D7Client *client);

	/**
	 * \brief Create a mesh consisting of a single mesh group
	 * \param client graphics client
	 * \param grp vertex group definition
	 * \param deepcopy if true, group contents are copied; otherwise, group
	 *   definition pointer is used directly
	 */
	D3D7Mesh (const oapi::D3D7Client *client, GROUPREC *grp, bool deepcopy=true);

	D3D7Mesh (const oapi::D3D7Client *client, MESHHANDLE hMesh, bool asTemplate = false);
	D3D7Mesh (const D3D7Mesh &mesh); // copy constructor
	~D3D7Mesh ();

	/**
	 * \brief Add a new vertex group to the mesh
	 * \param grp group definition
	 * \param deepcopy data copy flag (see notes)
	 * \return group index of the added group (>= 0)
	 * \note If deepcopy=true (default), the contents of the group definition
	 *   are copied into the mesh instance. deepcopy=false indicates that
	 *   the group definition was dynamically allocated, and that the pointer can
	 *   be used directly by the mesh. The calling function must not deallocate
	 *   the group after the call.
	 */
	DWORD AddGroup (GROUPREC *grp, bool deepcopy = true);

	/**
	 * \brief Add a new vertex group to the mesh
	 * \param mg group definition in generic parameters
	 * \return group index of the added group (>= 0)
	 * \note This method accepts a MESHGROUPEX structure to define the
	 *   group parameters. It differs from the GROUPREC specification by
	 *   providing the vertex list as an array instead of a vertex buffer.
	 *   This method creates the vertex buffer on the fly.
	 * \note deepcopy is implied. The contents of \a mg can be discarded
	 *   after the call.
	 */
	DWORD AddGroup (const MESHGROUPEX *mg);

	/**
	 * \brief Returns number of vertex groups
	 * \return Number of groups
	 */
	inline DWORD GroupCount() const { return nGrp; }

	/**
	 * \brief Returns a pointer to a mesh group.
	 * \param idx group index (>= 0)
	 * \return Pointer to group structure.
	 */
	inline GROUPREC *GetGroup (DWORD idx) { return Grp[idx]; }

	/**
	 * \brief Returns number of material specifications.
	 * \return Number of materials.
	 */
	inline DWORD MaterialCount() const { return nMtrl; }

	/**
	 * \brief returns a pointer to a material definition.
	 * \param idx material index (>= 0)
	 * \return Pointer to material object.
	 */
	inline D3DMATERIAL7 *GetMaterial (DWORD idx) { return Mtrl+idx; }

	/**
	 * \brief Replace a mesh texture.
	 * \param texidx texture index (>= 0)
	 * \param tex texture handle
	 * \return \e true on success, \e false otherwise.
	 */
	bool SetTexture (DWORD texidx, SURFHANDLE tex);

	void SetTexMixture (DWORD ntex, float mix);

	void RenderGroup (LPDIRECT3DDEVICE7 dev, GROUPREC *grp);
	void Render (LPDIRECT3DDEVICE7 dev);

	void TransformGroup (DWORD n, const D3DMATRIX *m);
	int GetGroup (DWORD grp, GROUPREQUESTSPEC *grs);
	int EditGroup (DWORD grp, GROUPEDITSPEC *ges);

	/**
	 * \brief Enable/disable material alpha value for transparency calculation.
	 * \param enable flag for enabling/disabling material alpha calculation.
	 * \note By default, material alpha values are ignored for mesh groups
	 *   with textures, and the texture alpha values are used instead.
	 *   By enabling material alpha calculation, the final alpha value is
	 *   calculated as the product of material and texture alpha value.
	 */
	inline void EnableMatAlpha (bool enable) { bModulateMatAlpha = enable; }

	/**
	 * \brief Globally enable/disable specular reflections for mesh rendering.
	 * \param enable flag for enabling/disabling specular reflections
	 */
	static void GlobalEnableSpecular (bool enable);

protected:
	bool CopyGroup (GROUPREC *tgt, const GROUPREC *src);
	bool CopyGroup (GROUPREC *grp, const MESHGROUPEX *mg);
	void DeleteGroup (GROUPREC *grp);
	void ClearGroups ();
	bool CopyMaterial (D3DMATERIAL7 *mat7, MATERIAL *mat);

private:
	const oapi::D3D7Client *gc; // the graphics client instance
	GROUPREC **Grp;             // list of mesh groups
	DWORD nGrp;                 // number of mesh groups
	LPDIRECTDRAWSURFACE7 *Tex;  // list of mesh textures
	DWORD nTex;                 // number of mesh textures
	D3DMATERIAL7 *Mtrl;         // list of mesh materials
	DWORD nMtrl;                // number of mesh materials
	bool bTemplate;             // mesh used as template only (not for rendering)
	bool bVideoMem;             // create vertex buffers in video memory
	                            // (can be overwritten by individual mesh groups)
	bool bModulateMatAlpha;     // mix material and texture alpha channels

	// global mesh flags
	static bool bEnableSpecular;   // enable specular reflection
};

#endif // !__MESH_H