
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
	skinname[0] = NULL;
	pRecord = (MatMgr::MATREC *)malloc(mRec*sizeof(MatMgr::MATREC));
}


// ===========================================================================================
//
MatMgr::~MatMgr()
{
	if (pRecord) free(pRecord);
}


// ===========================================================================================
//
void MatMgr::RegisterMaterialChange(D3D9Mesh *pMesh, DWORD midx, D3DMATERIAL9 *pM, D3D9MatExt *pME)
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
		strcpy_s(pRecord[iRec].mesh_name, 64, pMesh->GetName());
		pRecord[iRec].mat_idx = midx;
		nRec++;
	}

	// Fill the data
	if (pM) pRecord[iRec].Mat = *pM;
	if (pME) pRecord[iRec].MatEx = *pME;
}


// ===========================================================================================
//
void MatMgr::ApplyConfiguration(D3D9Mesh *pMesh)
{
	if (pMesh==NULL || nRec==0) return;

	const char *name = pMesh->GetName();

	for (DWORD i=0;i<nRec;i++) {

		if (strcmp(pRecord[i].mesh_name, name)==0) {

			DWORD idx = pRecord[i].mat_idx;
			
			if (idx>=pMesh->MaterialCount()) continue;

			D3D9MatExt *pME = pMesh->GetMaterialExtension(idx);
			D3DMATERIAL9 *pM = pMesh->GetMaterial(idx);

			if (pM==NULL || pME==NULL) continue;

			DWORD flags = pRecord[i].MatEx.ModFlags;

			if (flags&D3D9MATEX_AMBIENT) pM->Ambient = pRecord[i].Mat.Ambient;
			if (flags&D3D9MATEX_DIFFUSE) pM->Diffuse = pRecord[i].Mat.Diffuse;
			if (flags&D3D9MATEX_EMISSIVE) pM->Emissive = pRecord[i].Mat.Emissive;
			if (flags&D3D9MATEX_REFLECT) {
				pME->Reflect = pRecord[i].MatEx.Reflect;
				pME->Glass = pRecord[i].MatEx.Glass;
			}
			if (flags&D3D9MATEX_SPECULAR) {
				pM->Specular = pRecord[i].Mat.Specular;
				pM->Power = pRecord[i].Mat.Power;
			}
			if (flags&D3D9MATEX_DISSOLVE) {
				pME->DissolveScl = pRecord[i].MatEx.DissolveScl;
				pME->DissolveSct = pRecord[i].MatEx.DissolveSct;
				pME->pDissolve = pRecord[i].MatEx.pDissolve;
			}
			pME->ModFlags = flags;
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

	strcpy_s(pRecord[nRec].mesh_name, 64, name);
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
		file.pFile = fopen(path, "r");
	}

	if (file.IsInvalid()) {
		sprintf_s(path, 256, "%sGC\\%s.cfg", cfgdir, classname);
		file.pFile = fopen(path, "r");	
	}

	if (file.IsInvalid()) return true;

	LogAlw("Reading a custom configuration file for a vessel %s (%s)", vessel->GetName(), vessel->GetClassNameA());
	
	DWORD iRec = 0;
	DWORD mesh = 0;
	DWORD material = 0;

	while(fgets2(cbuf, 256, file.pFile, 0x08)>=0) 
	{	
		float a, b, c, d;
		
		// --------------------------------------------------------------------------------------------
		if (!strncmp(cbuf, "MESH", 4)) {
			if (sscanf_s(cbuf, "MESH %s", &meshname, 64)!=1) LogErr("Invalid Line in (%s): %s", path, cbuf);
			continue;
		}

		// --------------------------------------------------------------------------------------------
		if (!strncmp(cbuf, "MATERIAL", 8)) {
			if (sscanf(cbuf, "MATERIAL %u", &material)!=1) LogErr("Invalid Line in (%s): %s", path, cbuf);
			iRec = NewRecord(meshname, material);
			continue;
		}

		// --------------------------------------------------------------------------------------------
		if (!strncmp(cbuf, "SPECULAR", 8)) {
			if (sscanf(cbuf, "SPECULAR %f %f %f %f", &a, &b, &c, &d)!=4) LogErr("Invalid Line in (%s): %s", path, cbuf);
			pRecord[iRec].Mat.Specular.r = a;
			pRecord[iRec].Mat.Specular.g = b;
			pRecord[iRec].Mat.Specular.b = c;
			pRecord[iRec].Mat.Specular.a = 1.0;
			pRecord[iRec].Mat.Power = d;
			pRecord[iRec].MatEx.ModFlags |= D3D9MATEX_SPECULAR;
			continue;
		}

		// --------------------------------------------------------------------------------------------
		if (!strncmp(cbuf, "DIFFUSE", 7)) {
			if (sscanf(cbuf, "DIFFUSE %f %f %f %f", &a, &b, &c, &d)!=4) LogErr("Invalid Line in (%s): %s", path, cbuf);
			pRecord[iRec].Mat.Diffuse.r = a;
			pRecord[iRec].Mat.Diffuse.g = b;
			pRecord[iRec].Mat.Diffuse.b = c;
			pRecord[iRec].Mat.Diffuse.a = d;
			pRecord[iRec].MatEx.ModFlags |= D3D9MATEX_DIFFUSE;
			continue;
		}

		// --------------------------------------------------------------------------------------------
		if (!strncmp(cbuf, "EMISSIVE", 8)) {
			if (sscanf(cbuf, "EMISSIVE %f %f %f", &a, &b, &c)!=3) LogErr("Invalid Line in (%s): %s", path, cbuf);
			pRecord[iRec].Mat.Emissive.r = a;
			pRecord[iRec].Mat.Emissive.g = b;
			pRecord[iRec].Mat.Emissive.b = c;
			pRecord[iRec].Mat.Emissive.a = 1.0;
			pRecord[iRec].MatEx.ModFlags |= D3D9MATEX_EMISSIVE;
			continue;
		}

		// --------------------------------------------------------------------------------------------
		if (!strncmp(cbuf, "AMBIENT", 7)) {
			if (sscanf(cbuf, "AMBIENT %f %f %f", &a, &b, &c)!=3) LogErr("Invalid Line in (%s): %s", path, cbuf);
			pRecord[iRec].Mat.Ambient.r = a;
			pRecord[iRec].Mat.Ambient.g = b;
			pRecord[iRec].Mat.Ambient.b = c;
			pRecord[iRec].Mat.Ambient.a = 1.0;
			pRecord[iRec].MatEx.ModFlags |= D3D9MATEX_AMBIENT;
			continue;
		}

		// --------------------------------------------------------------------------------------------
		if (!strncmp(cbuf, "REFLECT", 7)) {
			if (sscanf(cbuf, "REFLECT %f %f", &a, &b)!=2) LogErr("Invalid Line in (%s): %s", path, cbuf);
			pRecord[iRec].MatEx.Reflect = a;
			pRecord[iRec].MatEx.Glass = b;
			pRecord[iRec].MatEx.ModFlags |= D3D9MATEX_REFLECT;
			continue;
		}

		// --------------------------------------------------------------------------------------------
		if (!strncmp(cbuf, "DISSOLVE", 8)) {

			char tex[128];
			if (sscanf_s(cbuf, "DISSOLVE %s %f %f", tex, sizeof(tex), &a, &b)!=3) LogErr("Invalid Line in (%s): %s", path, cbuf);
			
			pRecord[iRec].MatEx.DissolveScl = a;
			pRecord[iRec].MatEx.DissolveSct = b;
			pRecord[iRec].MatEx.pDissolve = gc->clbkLoadTexture(tex, 0x8);
			pRecord[iRec].MatEx.ModFlags |= D3D9MATEX_DISSOLVE;
			
			if (pRecord[iRec].MatEx.pDissolve==NULL) {
				LogErr("Failed to load a texture (%s)",tex);
				return false;
			}
			else {
				gc->RegisterDissolveMap(pRecord[iRec].MatEx.pDissolve);
			}
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

	char cbuf[256];
	char path[256];
	char classname[256];
	char current[64];

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
	
	file.pFile = fopen(path, "w");

	if (file.IsInvalid()) {
		LogErr("Failed to write a file");
		return false;
	}

	for (DWORD k=0;k<nRec;k++) pRecord[k].bSaved = false;

	for (DWORD k=0;k<nRec;k++) {
		
		if (!pRecord[k].bSaved) {
			strcpy_s(current, 64, pRecord[k].mesh_name);
			fprintf(file.pFile,"; =============================================\n");
			fprintf(file.pFile,"MESH %s\n", current);
		}

		for (DWORD i=0;i<nRec;i++) {
		
			if (strcmp(pRecord[i].mesh_name, current)!=0) continue;
			else if (pRecord[i].bSaved) break;
		
			fprintf(file.pFile,"; ---------------------------------------------\n");
			fprintf(file.pFile,"MATERIAL %u\n", pRecord[i].mat_idx);
			
			pRecord[i].bSaved = true;

			DWORD flags = pRecord[i].MatEx.ModFlags;
			D3DMATERIAL9 *pM = &pRecord[i].Mat; 
			D3D9MatExt *pME = &pRecord[i].MatEx;

			if (flags&D3D9MATEX_AMBIENT) fprintf(file.pFile,"AMBIENT %f %f %f\n", pM->Ambient.r, pM->Ambient.g, pM->Ambient.b);
			if (flags&D3D9MATEX_DIFFUSE) fprintf(file.pFile,"DIFFUSE %f %f %f %f\n", pM->Diffuse.r, pM->Diffuse.g, pM->Diffuse.b, pM->Diffuse.a);
			if (flags&D3D9MATEX_SPECULAR) fprintf(file.pFile,"SPECULAR %f %f %f %f\n", pM->Specular.r, pM->Specular.g, pM->Specular.b, pM->Power);
			if (flags&D3D9MATEX_EMISSIVE) fprintf(file.pFile,"EMISSIVE %f %f %f\n", pM->Emissive.r, pM->Emissive.g, pM->Emissive.b);
			if (flags&D3D9MATEX_REFLECT) fprintf(file.pFile,"REFLECT %f %f\n", pME->Reflect, pME->Glass);

			if (pME->pDissolve && flags&D3D9MATEX_DISSOLVE) {
				const char *name = SURFACE(pME->pDissolve)->GetName();
				if (name) fprintf(file.pFile,"DISSOLVE %s %f %f\n", name, pME->DissolveScl, pME->DissolveSct);
			}
		}
	}
	return true;
}



