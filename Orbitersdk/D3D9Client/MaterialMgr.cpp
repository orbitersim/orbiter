// =================================================================================================================================
// The MIT Lisence:
//
// Copyright (C) 2013 Jarmo Nikkanen
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


#include "MaterialMgr.h"
#include "D3D9Surface.h"
#include "OapiExtension.h"
#include "vVessel.h"



// ===========================================================================================
//
MatMgr::MatMgr(class vObject *v, class D3D9Client *_gc)
{
	gc = _gc;
	vObj = v;
	nRec = 0;
	mRec = 32;
	pRecord = (MatMgr::MATREC *)malloc(mRec*sizeof(MatMgr::MATREC));
	pCamera = new ENVCAMREC[1];

	ResetCamera(0);
}


// ===========================================================================================
//
MatMgr::~MatMgr()
{
	if (pRecord) {
		for (DWORD i=0;i<nRec;i++) { if (pRecord[i].mesh_name) {
			delete [] pRecord[i].mesh_name;
		}}
		free(pRecord);
	}
	if (pCamera) {
		if (pCamera[0].pOmitAttc) delete[] pCamera[0].pOmitAttc;
		if (pCamera[0].pOmitDock) delete[] pCamera[0].pOmitDock;
		delete[] pCamera;
	}
}


// ===========================================================================================
//
ENVCAMREC * MatMgr::GetCamera(DWORD idx)
{
	return &pCamera[0];
}


// ===========================================================================================
//
DWORD MatMgr::CameraCount()
{
	return 1;
}


// ===========================================================================================
//
void MatMgr::ResetCamera(DWORD idx)
{
	pCamera[idx].near_clip = 0.25f;
	pCamera[idx].lPos = D3DXVECTOR3(0,0,0);
	pCamera[idx].nAttc = 0;
	pCamera[idx].nDock = 0;
	pCamera[idx].flags = ENVCAM_OMIT_ATTC;
	pCamera[idx].pOmitAttc = NULL;
	pCamera[idx].pOmitDock = NULL;
}
	

// ===========================================================================================
//
void MatMgr::RegisterMaterialChange(D3D9Mesh *pMesh, DWORD midx, D3D9MatExt *pM)
{
	if (nRec==mRec) {
		mRec *= 2;
		pRecord = (MatMgr::MATREC *)realloc(pRecord, mRec*sizeof(MatMgr::MATREC));
	}

	DWORD iRec = nRec;
	bool bExists = false;

	// Seek an existing mesh and group
	for (DWORD i=0;i<nRec;i++) {
		if (strcmp(pRecord[i].mesh_name, pMesh->GetName())==0 && midx==pRecord[i].mat_idx) {
			iRec = i;
			bExists = true;
			break;
		}
	}

	if (!bExists) {
		// Create a new record
		const char* name = pMesh->GetName();
		pRecord[iRec].mesh_name = new char[strlen(name)+1]();
		strcpy_s(pRecord[iRec].mesh_name, strlen(name)+1, name);
		pRecord[iRec].mat_idx = midx;
		nRec++;
	}

	// Fill the data
	if (pM) pRecord[iRec].Mat = *pM;
}


// ===========================================================================================
//
void MatMgr::ApplyConfiguration(D3D9Mesh *pMesh)
{

	if (pMesh==NULL || nRec==0) return;

	const char *name = pMesh->GetName();

	LogAlw("Applying custom configuration to a mesh (%s)",name);

	for (DWORD i=0;i<nRec;i++) {

		if (strcmp(pRecord[i].mesh_name, name)==0) {

			DWORD idx = pRecord[i].mat_idx;
			
			if (idx>=pMesh->MaterialCount()) continue;

			D3D9MatExt *pM = pMesh->GetMaterial(idx);

			if (pM==NULL) continue;

			DWORD flags = pRecord[i].Mat.ModFlags;

			if (flags&D3D9MATEX_AMBIENT) pM->Ambient = pRecord[i].Mat.Ambient;
			if (flags&D3D9MATEX_DIFFUSE) pM->Diffuse = pRecord[i].Mat.Diffuse;
			if (flags&D3D9MATEX_EMISSIVE) pM->Emissive = pRecord[i].Mat.Emissive;
			if (flags&D3D9MATEX_REFLECT) pM->Reflect = pRecord[i].Mat.Reflect;
			if (flags&D3D9MATEX_SPECULAR) pM->Specular = pRecord[i].Mat.Specular;
			if (flags&D3D9MATEX_FRESNEL) {
				pM->Fresnel = pRecord[i].Mat.Fresnel;
				pM->FOffset = pRecord[i].Mat.FOffset;
			}
			if (flags&D3D9MATEX_DISSOLVE) {
				pM->DislScale = pRecord[i].Mat.DislScale;
				pM->DislMag = pRecord[i].Mat.DislMag;
				pM->pDissolve = pRecord[i].Mat.pDissolve;
			}
			pM->ModFlags = flags;

			LogBlu("Material %u setup applied to mesh (%s) Flags=0x%X", idx, name, flags);
		}
	}
}


// ===========================================================================================
//
DWORD MatMgr::NewRecord(const char *name, DWORD midx)
{
	DWORD rv = nRec;

	if (nRec==mRec) {
		mRec *= 2;
		pRecord = (MatMgr::MATREC *)realloc(pRecord, mRec*sizeof(MatMgr::MATREC));
	}

	ClearRecord(nRec);

	pRecord[nRec].mesh_name = new char[strlen(name)+1]();
	strcpy_s(pRecord[nRec].mesh_name, strlen(name)+1, name);
	pRecord[nRec].mat_idx = midx;
	nRec++;
	return rv;
}

// ===========================================================================================
//
void MatMgr::ClearRecord(DWORD iRec)
{
	if (iRec>=mRec) return;
	memset(&pRecord[iRec],0,sizeof(MatMgr::MATREC));
}

// ===========================================================================================
//
void parse_vessel_classname(char *lbl)
{
	int i = -1;
	while (lbl[++i]!=0) if (lbl[i]=='/' || lbl[i]=='\\') lbl[i]='_';
}

// ===========================================================================================
//
bool MatMgr::LoadConfiguration()
{
	_TRACE;
	bool bParseMat = false;

	char cbuf[256];
	char path[256];
	char classname[256];
	char meshname[64];

	OBJHANDLE hObj = vObj->GetObjectA();

	if (oapiGetObjectType(hObj)!=OBJTP_VESSEL) return false; 

	const char *cfgdir = OapiExtension::GetConfigDir();
	const char *skinname = ((vVessel *)vObj)->GetSkinName();

	VESSEL *vessel = oapiGetVesselInterface(hObj);
	strcpy_s(classname, 256, vessel->GetClassNameA());
	parse_vessel_classname(classname);

	AutoFile file;

	if (skinname) {
		sprintf_s(path, 256, "%sGC\\%s_%s.cfg", cfgdir, classname, skinname);
		fopen_s(&file.pFile, path, "r");
	}

	if (file.IsInvalid()) {
		sprintf_s(path, 256, "%sGC\\%s.cfg", cfgdir, classname);
		fopen_s(&file.pFile, path, "r");	
	}

	if (file.IsInvalid()) return true;

	LogAlw("Reading a custom configuration file for a vessel %s (%s)", vessel->GetName(), vessel->GetClassNameA());
	
	DWORD iRec = 0;
	DWORD mesh = 0;
	DWORD material = 0;

	while (fgets2(cbuf, 256, file.pFile, 0x0A)>=0) 
	{	
		float a, b, c, d;
		DWORD id;
		
		// --------------------------------------------------------------------------------------------
		if (!strncmp(cbuf, "CAMERA_LPOS", 11)) {
			if (sscanf_s(cbuf, "CAMERA_LPOS %u %g %g %g", &id, &a, &b, &c)!=4) LogErr("Invalid Line in (%s): %s", path, cbuf);
			continue;
		}

		// --------------------------------------------------------------------------------------------
		if (!strncmp(cbuf, "MESH", 4)) {
			if (sscanf_s(cbuf, "MESH %s", &meshname, 64)!=1) LogErr("Invalid Line in (%s): %s", path, cbuf);
			continue;
		}

		// --------------------------------------------------------------------------------------------
		if (!strncmp(cbuf, "MATERIAL", 8)) {
			if (sscanf_s(cbuf, "MATERIAL %u", &material)!=1) LogErr("Invalid Line in (%s): %s", path, cbuf);
			iRec = NewRecord(meshname, material);
			continue;
		}

		// --------------------------------------------------------------------------------------------
		if (!strncmp(cbuf, "SPECULAR", 8)) {
			if (sscanf_s(cbuf, "SPECULAR %f %f %f %f", &a, &b, &c, &d)!=4) LogErr("Invalid Line in (%s): %s", path, cbuf);
			pRecord[iRec].Mat.Specular.r = a;
			pRecord[iRec].Mat.Specular.g = b;
			pRecord[iRec].Mat.Specular.b = c;
			pRecord[iRec].Mat.Specular.a = d;
			pRecord[iRec].Mat.ModFlags |= D3D9MATEX_SPECULAR;
			continue;
		}

		// --------------------------------------------------------------------------------------------
		if (!strncmp(cbuf, "DIFFUSE", 7)) {
			if (sscanf_s(cbuf, "DIFFUSE %f %f %f %f", &a, &b, &c, &d)!=4) LogErr("Invalid Line in (%s): %s", path, cbuf);
			pRecord[iRec].Mat.Diffuse.r = a;
			pRecord[iRec].Mat.Diffuse.g = b;
			pRecord[iRec].Mat.Diffuse.b = c;
			pRecord[iRec].Mat.Diffuse.a = d;
			pRecord[iRec].Mat.ModFlags |= D3D9MATEX_DIFFUSE;
			continue;
		}

		// --------------------------------------------------------------------------------------------
		if (!strncmp(cbuf, "EMISSIVE", 8)) {
			if (sscanf_s(cbuf, "EMISSIVE %f %f %f", &a, &b, &c)!=3) LogErr("Invalid Line in (%s): %s", path, cbuf);
			pRecord[iRec].Mat.Emissive.r = a;
			pRecord[iRec].Mat.Emissive.g = b;
			pRecord[iRec].Mat.Emissive.b = c;
			pRecord[iRec].Mat.Emissive.a = 1.0;
			pRecord[iRec].Mat.ModFlags |= D3D9MATEX_EMISSIVE;
			continue;
		}

		// --------------------------------------------------------------------------------------------
		if (!strncmp(cbuf, "AMBIENT", 7)) {
			if (sscanf_s(cbuf, "AMBIENT %f %f %f", &a, &b, &c)!=3) LogErr("Invalid Line in (%s): %s", path, cbuf);
			pRecord[iRec].Mat.Ambient.r = a;
			pRecord[iRec].Mat.Ambient.g = b;
			pRecord[iRec].Mat.Ambient.b = c;
			pRecord[iRec].Mat.Ambient.a = 1.0;
			pRecord[iRec].Mat.ModFlags |= D3D9MATEX_AMBIENT;
			continue;
		}

		// --------------------------------------------------------------------------------------------
		if (!strncmp(cbuf, "REFLECT", 7)) {
			if (sscanf_s(cbuf, "REFLECT %f %f %f %f", &a, &b, &c, &d)!=4) LogErr("Invalid Line in (%s): %s", path, cbuf);
			pRecord[iRec].Mat.Reflect.r = a;
			pRecord[iRec].Mat.Reflect.g = b;
			pRecord[iRec].Mat.Reflect.b = c;
			pRecord[iRec].Mat.Reflect.a = d;
			pRecord[iRec].Mat.ModFlags |= D3D9MATEX_REFLECT;
			continue;
		}

		// --------------------------------------------------------------------------------------------
		if (!strncmp(cbuf, "DISSOLVE", 8)) {

			char tex[128];
			if (sscanf_s(cbuf, "DISSOLVE %s %f %f", tex, sizeof(tex), &a, &b)!=3) LogErr("Invalid Line in (%s): %s", path, cbuf);
			
			pRecord[iRec].Mat.DislScale = a;
			pRecord[iRec].Mat.DislMag = b;
			pRecord[iRec].Mat.pDissolve = gc->clbkLoadTexture(tex, 0x8);
			pRecord[iRec].Mat.ModFlags |= D3D9MATEX_DISSOLVE;
			
			if (pRecord[iRec].Mat.pDissolve==NULL) {
				LogErr("Failed to load a texture (%s)",tex);
				return false;
			}
			else {
				gc->RegisterDissolveMap(pRecord[iRec].Mat.pDissolve);
			}
			continue;
		}

		// --------------------------------------------------------------------------------------------
		if (!strncmp(cbuf, "FRESNEL", 7)) {
			if (sscanf_s(cbuf, "FRESNEL %f %f", &a, &b)!=2) LogErr("Invalid Line in (%s): %s", path, cbuf);
			pRecord[iRec].Mat.Fresnel = a;
			pRecord[iRec].Mat.FOffset = b;
			pRecord[iRec].Mat.ModFlags |= D3D9MATEX_FRESNEL;
			continue;
		}
	}

	return true;
}


// ===========================================================================================
//
bool MatMgr::SaveConfiguration()
{
	_TRACE;
	bool bIfStatement = false;

	char path[256];
	char classname[256];
	char *current = NULL; // ..._mesh_name

	OBJHANDLE hObj = vObj->GetObjectA();

	if (oapiGetObjectType(hObj)!=OBJTP_VESSEL) return false; 

	VESSEL *vessel = oapiGetVesselInterface(hObj);
	const char *skinname = ((vVessel *)vObj)->GetSkinName();
	const char *cfgdir = OapiExtension::GetConfigDir();

	strcpy_s(classname, 256, vessel->GetClassNameA());
	parse_vessel_classname(classname);

	AutoFile file;
	
	if (skinname) sprintf_s(path, 256, "%sGC\\%s_%s.cfg", cfgdir, classname, skinname);
	else 		  sprintf_s(path, 256, "%sGC\\%s.cfg", cfgdir, classname);
	
	fopen_s(&file.pFile, path, "w");

	if (file.IsInvalid()) {
		LogErr("Failed to write a file");
		return false;
	}

	for (DWORD k=0;k<nRec;k++) pRecord[k].bSaved = false;

	for (DWORD k=0;k<nRec;k++) {
		
		if (!pRecord[k].bSaved) {
			current = pRecord[k].mesh_name;
			fprintf(file.pFile,"; =============================================\n");
			fprintf(file.pFile,"MESH %s\n", current);
		}

		for (DWORD i=0;i<nRec;i++) {
		
			if (strcmp(pRecord[i].mesh_name, current)!=0) continue;
			else if (pRecord[i].bSaved) break;
		
			fprintf(file.pFile,"; ---------------------------------------------\n");
			fprintf(file.pFile,"MATERIAL %u\n", pRecord[i].mat_idx);
			
			pRecord[i].bSaved = true;

			DWORD flags = pRecord[i].Mat.ModFlags;
			D3D9MatExt *pM = &pRecord[i].Mat; 
			
			if (flags&D3D9MATEX_AMBIENT)  fprintf(file.pFile,"AMBIENT %f %f %f\n", pM->Ambient.r, pM->Ambient.g, pM->Ambient.b);
			if (flags&D3D9MATEX_DIFFUSE)  fprintf(file.pFile,"DIFFUSE %f %f %f %f\n", pM->Diffuse.r, pM->Diffuse.g, pM->Diffuse.b, pM->Diffuse.a);
			if (flags&D3D9MATEX_SPECULAR) fprintf(file.pFile,"SPECULAR %f %f %f %f\n", pM->Specular.r, pM->Specular.g, pM->Specular.b, pM->Specular.a);
			if (flags&D3D9MATEX_EMISSIVE) fprintf(file.pFile,"EMISSIVE %f %f %f\n", pM->Emissive.r, pM->Emissive.g, pM->Emissive.b);
			
			if (flags&D3D9MATEX_REFLECT) {
				if (pM->Reflect.a>1e-3f) fprintf(file.pFile,"REFLECT %f %f %f %f\n", pM->Reflect.r, pM->Reflect.g, pM->Reflect.b, pM->Reflect.a);
			}
			
			if (flags&D3D9MATEX_FRESNEL) {
				if (pM->Fresnel>1e-3f) fprintf(file.pFile,"FRESNEL %f %f\n", pM->Fresnel, pM->FOffset);
			}

			if (pM->pDissolve && flags&D3D9MATEX_DISSOLVE) {
				const char *name = SURFACE(pM->pDissolve)->GetName();
				if (name && pM->DislScale>1e-3f && pM->DislMag>1e-3f) fprintf(file.pFile,"DISSOLVE %s %f %f\n", name, pM->DislScale, pM->DislMag);
			}
		}
	}
	return true;
}


// ===========================================================================================
//
bool MatMgr::LoadCameraConfig()
{
	_TRACE;

	char cbuf[256];
	char path[256];
	char classname[256];

	OBJHANDLE hObj = vObj->GetObjectA();

	if (oapiGetObjectType(hObj)!=OBJTP_VESSEL) return false; 

	const char *cfgdir = OapiExtension::GetConfigDir();
	
	VESSEL *vessel = oapiGetVesselInterface(hObj);
	strcpy_s(classname, 256, vessel->GetClassNameA());
	parse_vessel_classname(classname);

	AutoFile file;

	sprintf_s(path, 256, "%sGC\\%s_ecam.cfg", cfgdir, classname);
	fopen_s(&file.pFile, path, "r");	
	
	if (file.IsInvalid()) return true;

	LogAlw("Reading a camera configuration file for a vessel %s (%s)", vessel->GetName(), vessel->GetClassNameA());
	
	DWORD iattc = 0;
	DWORD idock = 0;
	DWORD camera = 0;

	BYTE attclist[256];
	BYTE docklist[256];

	while(fgets2(cbuf, 256, file.pFile, 0x08)>=0) 
	{	
		float a, b, c;
		DWORD id;

		// --------------------------------------------------------------------------------------------
		if (!strncmp(cbuf, "END_CAMERA", 10)) {

			if (iattc) pCamera[camera].pOmitAttc = new BYTE[iattc];
			if (idock) pCamera[camera].pOmitDock = new BYTE[idock];
			
			if (iattc) memcpy(pCamera[camera].pOmitAttc, attclist, iattc); 
			if (idock) memcpy(pCamera[camera].pOmitDock, docklist, idock); 
			
			pCamera[camera].nAttc = WORD(iattc);
			pCamera[camera].nDock = WORD(idock);
			
			continue;
		}
		
		// --------------------------------------------------------------------------------------------
		if (!strncmp(cbuf, "BEGIN_CAMERA", 12)) {
			if (sscanf_s(cbuf, "BEGIN_CAMERA %u", &camera)!=1) LogErr("Invalid Line in (%s): %s", path, cbuf);
			camera = 0; // For now just one camera
			pCamera[camera].flags = 0; // Clear default flags
			continue;
		}

		// --------------------------------------------------------------------------------------------
		if (!strncmp(cbuf, "LPOS", 4)) {
			if (sscanf_s(cbuf, "LPOS %g %g %g", &a, &b, &c)!=3) LogErr("Invalid Line in (%s): %s", path, cbuf);
			pCamera[camera].lPos = D3DXVECTOR3(a,b,c);
			continue;
		}

		// --------------------------------------------------------------------------------------------
		if (!strncmp(cbuf, "OMITATTC", 8)) {
			if (sscanf_s(cbuf, "OMITATTC %u", &id)!=1) LogErr("Invalid Line in (%s): %s", path, cbuf);
			attclist[iattc++] = BYTE(id);
			continue;
		}

		// --------------------------------------------------------------------------------------------
		if (!strncmp(cbuf, "OMITDOCK", 8)) {
			if (sscanf_s(cbuf, "OMITDOCK %u", &id)!=1) LogErr("Invalid Line in (%s): %s", path, cbuf);
			docklist[idock++] = BYTE(id);
			continue;
		}

		// --------------------------------------------------------------------------------------------
		if (!strncmp(cbuf, "CLIPDIST", 8)) {
			if (sscanf_s(cbuf, "CLIPDIST %g", &a)!=1) LogErr("Invalid Line in (%s): %s", path, cbuf);
			pCamera[camera].near_clip = a;
			continue;
		}

		// --------------------------------------------------------------------------------------------
		if (!strncmp(cbuf, "OMIT_ALL_ATTC", 13)) {
			pCamera[camera].flags |= ENVCAM_OMIT_ATTC;
			continue;
		}

		// --------------------------------------------------------------------------------------------
		if (!strncmp(cbuf, "DO_NOT_OMIT_FOCUS", 17)) {
			pCamera[camera].flags |= ENVCAM_FOCUS;
			continue;
		}

		// --------------------------------------------------------------------------------------------
		if (!strncmp(cbuf, "OMIT_ALL_DOCKS", 14)) {
			pCamera[camera].flags |= ENVCAM_OMIT_DOCKS;
			continue;
		}

		if (cbuf[0]!=';') LogErr("Invalid Line in (%s): %s", path, cbuf);
	}

	return true;
}



