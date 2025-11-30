// ==============================================================
// MaterialMgr.h
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// Dual licensed under GPL v3 and LGPL v3
// Copyright (C) 2012 - 2016 Jarmo Nikkanen
// ==============================================================

#ifndef __MATERIALMGR_H
#define __MATERIALMGR_H

#include <d3d9.h>
#include "MathAPI.h"

#include "Mesh.h"
#include "Client.h"
#include "Util.h"
#include "vObject.h"

/**
 * \brief Management of custom configurations for vessel materials
 */
class MatMgr {

public:
	// Disable copy construct & copy assign
					MatMgr    (MatMgr const&) = delete;
	MatMgr &		operator= (MatMgr const&) = delete;

					MatMgr(class vObject *vObj, class vkClient *_gc);
					~MatMgr();

	void			RegisterMaterialChange(vkMesh *pMesh, DWORD midx, const vkMatExt *pM);
	void			RegisterShaderChange(vkMesh *pMesh, WORD id);
	void			ApplyConfiguration(vkMesh *pMesh);
	bool			SaveConfiguration();
	bool			LoadConfiguration(bool bAppend=false);
	bool			LoadCameraConfig();
	bool			HasMesh(const char *name);

private:

	vObject			*vObj;
	vkClient		*gc;

	struct SHADER {
		SHADER(string x, WORD i) { name = x; id = i; }
		string name;
		WORD id;
	};

	struct MESHREC {
		WORD shader;
		map<int, vkMatExt> material;
	};

	std::map<string, MESHREC> MeshConfig;
	std::list<SHADER> Shaders;
};

#endif
