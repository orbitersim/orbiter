
// =================================================================================================================================
// The MIT Lisence:
//
// Copyright (C) 2012 Jarmo Nikkanen
//
// Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation 
// files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, 
// modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software 
// is furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
// OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
// LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR
// IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
// =================================================================================================================================


#ifndef __MATERIALMGR_H
#define __MATERIALMGR_H

#include <d3d9.h> 
#include <d3dx9.h>

#include "Mesh.h"
#include "D3D9Client.h"
#include "D3D9Util.h"
#include "vObject.h"

#define ENVCAM_OMIT_ATTC		0x0001
#define ENVCAM_OMIT_DOCKS		0x0002
#define ENVCAM_FOCUS			0x0004


/**
 * \brief Storage structure to keep environmental camera information.
 */
struct ENVCAMREC {
	D3DXVECTOR3		lPos;			///< Camera local position
	float			near_clip;		///< Near clip-plane distance
	DWORD			flags;			///< Camera flags
	WORD			nAttc;			///< Number of attachments points in a list
	WORD			nDock;			///< Number of docking ports in a list
	BYTE *			pOmitAttc;		///< Omit attachments
	BYTE *			pOmitDock;		///< Omit cessels in docking ports
};

/**
 * \brief Management of custom configurations for vessel materials
 */
class MatMgr {

public:
					MatMgr(class vObject *vObj, class D3D9Client *_gc);
					~MatMgr();

	DWORD			NewRecord(const char *name, DWORD midx);
	void			ClearRecord(DWORD iRec);
	void			RegisterMaterialChange(D3D9Mesh *pMesh, DWORD midx, D3D9MatExt *pM);
	void			ApplyConfiguration(D3D9Mesh *pMesh);
	bool			SaveConfiguration();
	bool			LoadConfiguration();
	bool			LoadCameraConfig();
	void			ResetCamera(DWORD idx);

	ENVCAMREC *		GetCamera(DWORD idx);
	DWORD			CameraCount();
	
private:

	vObject			*vObj;
	D3D9Client		*gc;
	DWORD			nRec;	///< Number of records
	DWORD			mRec;	///< Allocated records

	/**
	 * \brief Storage structure to keep material information.
	 */
	struct MATREC {
		char*			mesh_name;		///< Name of the mesh
		DWORD			mat_idx;		///< Material index
		D3D9MatExt		Mat;			///< Material itself
		bool			bSaved;			///< Flag indicating whether the material has been saved
	} *pRecord;

	ENVCAMREC *pCamera;
};

#endif