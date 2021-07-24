// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// =======================================================================
// Classes to support meshes under D3D:
// Triangle, Mesh
// =======================================================================

#ifndef __MESH_H
#define __MESH_H

#include <d3d.h>
#include <d3dtypes.h>
#include <iostream>

typedef char Str256[256];

const DWORD SPEC_DEFAULT = (DWORD)(-1); // "default" material/texture flag
const DWORD SPEC_INHERIT = (DWORD)(-2); // "inherit" material/texture flag

// =======================================================================
// Class Triangle
// triangular surface patch

class Triangle {
public:
	Triangle ();
	Triangle (const Triangle &tri);
	Triangle (int n0, int n1, int n2);

	void SetNodes (int n0, int n1, int n2);

	int nd[3];  // node index list
	int nm[3];  // normal index list

	bool hasNodes;   // true if nd[] is valid
	bool hasNormals; // true if nm[] is valid
};

// =======================================================================
// mesh group descriptor

typedef struct {	
	D3DVERTEX *Vtx;
	WORD      *Idx;
	DWORD     nVtx;
	DWORD     nIdx;
	DWORD     MtrlIdx;
	DWORD     TexIdx;
	WORD      zBias;
	WORD      Flags;
	char      *Label;
	char      *Comment;
} GroupSpec;

// =======================================================================
// Class Mesh

class Mesh {
public:
	Mesh ();
	// Create an empty mesh

	Mesh (D3DVERTEX *vtx, DWORD nvtx, WORD *idx, DWORD nidx,
		DWORD matidx = SPEC_DEFAULT, DWORD texidx = SPEC_DEFAULT);
	// Create a single-group mesh

	Mesh (const Mesh &mesh);
	// copy constructor

	~Mesh ();

	void Set (const Mesh &mesh);

	void Setup ();
	// call after all groups are assembled or whenever groups change,
	// to set up group parameters

	DWORD nGroup() const { return nGrp; }
	// Number of groups

	bool GetGroup (DWORD grp, D3DVERTEX *&vtx, DWORD &nvtx, WORD *&idx, DWORD &nidx);
	// Return pointer to vertex and index list for group grp
	// Return value is false if grp index is out of range

	int AddGroup (D3DVERTEX *vtx, DWORD nvtx, WORD *idx, DWORD nidx,
		DWORD mtrl_idx = -1, DWORD tex_idx = -1, WORD zbias = 0);
	// Add new group to the mesh and return its group index
	// The lists are handled by the mesh and should not be released by
	// the calling program

	bool DeleteGroup (DWORD grp);
	// Delete group 'grp'. Note that the indices of the other
	// groups may change as a result.
	// Return value is false if grp index is out of range

	void AddMesh (Mesh &mesh);
	// Merge "mesh" into "this", by adding all groups of "mesh"
	// Currently this does not use the materials and textures of "mesh"

	int AddMaterial (D3DMATERIAL7 &mtrl);
	// Add new material to the mesh and return its list index

	void ScaleGroup (DWORD grp, D3DVALUE sx, D3DVALUE sy, D3DVALUE sz);
	void Scale (D3DVALUE sx, D3DVALUE sy, D3DVALUE sz);
	// scale an individual group or the whole mesh

	void TranslateGroup (DWORD grp, D3DVALUE dx, D3DVALUE dy, D3DVALUE dz);
	void Translate (D3DVALUE dx, D3DVALUE dy, D3DVALUE dz);
	// translate an individual group or the whole mesh

	enum RotAxis { ROTATE_X, ROTATE_Y, ROTATE_Z };
	void RotateGroup (DWORD grp, RotAxis axis, D3DVALUE angle);
	void Rotate (RotAxis axis, D3DVALUE angle);
	// rotate the mesh 'angle' rad around a coordiate axis

	enum MirrorDir { MIRROR_X, MIRROR_Y, MIRROR_Z };
	void MirrorGroup (DWORD grp, MirrorDir dir);
	void Mirror (MirrorDir dir);

	void TexScaleGroup (DWORD grp, D3DVALUE su, D3DVALUE sv);
	void TexScale (D3DVALUE su, D3DVALUE sv);
	// scale the texture coordinates of an individual group or the whole mesh

	void CalcNormals (DWORD grp, bool missingonly);
	// automatic calculation of vertex normals for group grp
	// if missingonly=true then only normals with zero length are calculated

	void CalcTexCoords (DWORD grp);
	// under construction

	void ZeroThreshold (float eps, int which = 7);
	// snap all vertex coordinates, normals, and/or texture coordinates with magnitude
	// less than eps to zero
	// which&1: vertex coords; which&2: normals; which&4: texture coords

	void CheckGroup (DWORD grp, DWORD &nd_removed);
	// check group integrity and fix if necessary. Tests performed:
	// 1: remove all vertices not referenced by triangle list

	void Clear ();

	//DWORD Render (LPDIRECT3DDEVICE7 dev);
	// render the mesh using device dev
	// return value is the number of rendered groups

	friend std::istream &operator>> (std::istream &is, Mesh &mesh);
	// read mesh from file

	friend std::ostream &operator<< (std::ostream &os, const Mesh &mesh);
	// write mesh to file

	void Separate ();

private:
	DWORD nGrp;         // number of groups
	GroupSpec *Grp;     // list of group specs	

	DWORD nMtrl;        // number of materials
	D3DMATERIAL7 *Mtrl; // list of materials used by the mesh
	char **Matname;     // list of material names

	DWORD nTex;         // number of textures
	char **Texname;     // list of texture names

	bool GrpSetup;      // true if the following arrays are allocated
	D3DVECTOR *GrpCnt;  // list of barycentres for each group (local coords)
	D3DVALUE *GrpRad;   // list of max. radii for each group
	DWORD *GrpVis;      // visibility flags for each group
};

#endif // !__MESH_H