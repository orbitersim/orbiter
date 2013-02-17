
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


/**
 * \brief Management of custom configurations for vessel materials
 */
class MatMgr {

public:
					MatMgr(class vObject *vObj, class D3D9Client *_gc);
					~MatMgr();

	DWORD			NewRecord(const char *name, DWORD midx);
	void			ClearRecord(DWORD iRec);
	void			RegisterMaterialChange(D3D9Mesh *pMesh, DWORD midx, D3DMATERIAL9 *pM, D3D9MatExt *pME);
	void			ApplyConfiguration(D3D9Mesh *pMesh);
	bool			SaveConfiguration();
	bool			LoadConfiguration();
	//const char *	GetSkinName();

private:

	bool			ParseIfStatement(const char *cbuf);

	vObject			*vObj;
	D3D9Client		*gc;

	char			skinname[64];	///< Name of what? skin

	DWORD			nRec;	///< Number of records
	DWORD			mRec;	///< Allocated records

	/**
	 * \brief Storage structure to keep material information.
	 */
	struct MATREC {
		char			mesh_name[64];	///< Name of the mesh
		DWORD			mat_idx;		///< Material index
		D3DMATERIAL9	Mat;			///< Material itself
		D3D9MatExt		MatEx;			///< Extended material
		bool			bSaved;			///< Flag indicating whether the material has been saved
	} *pRecord;

};

#endif