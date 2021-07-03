// dxtexDoc.cpp : implementation of the CDxtexDoc class
//

#include "stdafx.h"
#include "dxtex.h"

#include "dxtexDoc.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CDxtexDoc

IMPLEMENT_DYNCREATE(CDxtexDoc, CDocument)

BEGIN_MESSAGE_MAP(CDxtexDoc, CDocument)
	//{{AFX_MSG_MAP(CDxtexDoc)
	ON_COMMAND(ID_FILE_OPENALPHA, OnFileOpenAlpha)
	ON_COMMAND(ID_FORMAT_GENERATEMIPMAPS, OnGenerateMipMaps)
	ON_COMMAND(ID_FORMAT_DXT1, OnFormatDxt1)
	ON_COMMAND(ID_FORMAT_DXT2, OnFormatDxt2)
	ON_COMMAND(ID_FORMAT_DXT3, OnFormatDxt3)
	ON_COMMAND(ID_FORMAT_DXT4, OnFormatDxt4)
	ON_COMMAND(ID_FORMAT_DXT5, OnFormatDxt5)
	ON_COMMAND(ID_FORMAT_CHANGECUBEMAPFACES, OnFormatChangeCubeMapFaces)
	ON_UPDATE_COMMAND_UI(ID_FILE_OPENALPHA, OnUpdateFileOpenAlpha)
	ON_UPDATE_COMMAND_UI(ID_FORMAT_GENERATEMIPMAPS, OnUpdateFormatGenerateMipmaps)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CDxtexDoc diagnostics

#ifdef _DEBUG
void CDxtexDoc::AssertValid() const
{
	CDocument::AssertValid();
}

void CDxtexDoc::Dump(CDumpContext& dc) const
{
	CDocument::Dump(dc);
}
#endif //_DEBUG

/////////////////////////////////////////////////////////////////////////////
// CDxtexDoc construction/destruction

CDxtexDoc::CDxtexDoc()
{
	m_pddsOrig = NULL;
	m_pddsNew = NULL;
	m_dwWidth = 0;
	m_dwHeight = 0;
	m_numMips = 0;
	m_dwCubeMapFlags = 0;
	m_bTitleModsChanged = FALSE;
}

CDxtexDoc::~CDxtexDoc()
{
	ReleasePpo(&m_pddsOrig);
	ReleasePpo(&m_pddsNew);
}

BOOL CDxtexDoc::OnNewDocument()
{
	if (!CDocument::OnNewDocument())
		return FALSE;

	return TRUE;
}


/////////////////////////////////////////////////////////////////////////////
// CDxtexDoc serialization

void CDxtexDoc::Serialize(CArchive& ar)
{
	HRESULT hr;
	
	if (ar.IsStoring())
	{
		LPDIRECTDRAWSURFACE7 pdds;
		pdds = (m_pddsNew == NULL ? m_pddsOrig : m_pddsNew);
		SaveDDS(pdds, ar);
	}
	else
	{
		CFile* pFile = ar.GetFile();
		CString str = pFile->GetFilePath();
		TCHAR* pszExt = strrchr(str, '.');
		if (lstrcmpi(pszExt, ".bmp") == 0)
		{
			if (FAILED(hr = LoadBmp(str)))
				AfxThrowArchiveException(CArchiveException::generic);
		}
		else if (lstrcmpi(pszExt, ".dds") == 0)
		{
			ReleasePpo(&m_pddsOrig);
			ReleasePpo(&m_pddsNew);
			if (FAILED(hr = LoadDDS(&m_pddsOrig, ar)))
				AfxThrowArchiveException(CArchiveException::badIndex); // invalid file format
		}
		else
		{
			AfxThrowArchiveException(CArchiveException::badIndex); // invalid file format
		}
	}
}


HRESULT CDxtexDoc::SaveDDS(LPDIRECTDRAWSURFACE7 pdds, CArchive& ar)
{
	HRESULT hr;
	DDSURFACEDESC2 ddsd;
	DWORD dwMagic;
	DWORD dwTopCubeFace;

	dwMagic = MAKEFOURCC('D','D','S',' ');
	ar.Write(&dwMagic, sizeof(dwMagic));
	ZeroMemory(&ddsd, sizeof(ddsd));
	ddsd.dwSize = sizeof(ddsd);
	if (FAILED(hr = pdds->GetSurfaceDesc(&ddsd)))
		return hr;

	// If this is a cube map, remember which cubemap face is the "top" one
	dwTopCubeFace = ddsd.ddsCaps.dwCaps2 & DDSCAPS2_CUBEMAP_ALLFACES;
	
	// Mask out all caps indicating image uses, and only
	// keep those intrinsic to the image
	// e.g., remove DDSCAPS_SYSTEMMEMORY, but keep DDSCAPS_MIPMAP
	ddsd.ddsCaps.dwCaps &= (DDSCAPS_ALPHA | DDSCAPS_COMPLEX | DDSCAPS_PALETTE | DDSCAPS_TEXTURE | DDSCAPS_MIPMAP | DDSCAPS_OPTIMIZED);
	ddsd.ddsCaps.dwCaps2 &= (DDSCAPS2_CUBEMAP);
	ddsd.ddsCaps.dwCaps3 &= ( 0 );
	ddsd.ddsCaps.dwCaps4 &= ( 0 );

	// The ddsd of the top surface currently only contains the cube map flag for one
	// of the cube map faces.  But we want to store all faces in the file's DDSD:
	ddsd.ddsCaps.dwCaps2 |= m_dwCubeMapFlags;

	ar.Write(&ddsd, sizeof(ddsd));

	// Write out top level, along with any mips
	if (FAILED(hr = SaveAllMipSurfaces(pdds, ar)))
		return hr;

	// Save additional cubemap faces, if any
	if (m_dwCubeMapFlags != dwTopCubeFace)
	{
		LPDIRECTDRAWSURFACE7 pddsFaceTop = NULL;

		if ((m_dwCubeMapFlags & DDSCAPS2_CUBEMAP_POSITIVEX) && !(dwTopCubeFace & DDSCAPS2_CUBEMAP_POSITIVEX))
		{
			if (FAILED(hr = GetTopCubeFace(pdds, DDSCAPS2_CUBEMAP_POSITIVEX, &pddsFaceTop)))
				return hr;
			if (FAILED(hr = SaveAllMipSurfaces(pddsFaceTop, ar)))
				return hr;
			ReleasePpo(&pddsFaceTop);
		}
		if ((m_dwCubeMapFlags & DDSCAPS2_CUBEMAP_NEGATIVEX) && !(dwTopCubeFace & DDSCAPS2_CUBEMAP_NEGATIVEX))
		{
			if (FAILED(hr = GetTopCubeFace(pdds, DDSCAPS2_CUBEMAP_NEGATIVEX, &pddsFaceTop)))
				return hr;
			if (FAILED(hr = SaveAllMipSurfaces(pddsFaceTop, ar)))
				return hr;
			ReleasePpo(&pddsFaceTop);
		}
		if ((m_dwCubeMapFlags & DDSCAPS2_CUBEMAP_POSITIVEY) && !(dwTopCubeFace & DDSCAPS2_CUBEMAP_POSITIVEY))
		{
			if (FAILED(hr = GetTopCubeFace(pdds, DDSCAPS2_CUBEMAP_POSITIVEY, &pddsFaceTop)))
				return hr;
			if (FAILED(hr = SaveAllMipSurfaces(pddsFaceTop, ar)))
				return hr;
			ReleasePpo(&pddsFaceTop);
		}
		if ((m_dwCubeMapFlags & DDSCAPS2_CUBEMAP_NEGATIVEY) && !(dwTopCubeFace & DDSCAPS2_CUBEMAP_NEGATIVEY))
		{
			if (FAILED(hr = GetTopCubeFace(pdds, DDSCAPS2_CUBEMAP_NEGATIVEY, &pddsFaceTop)))
				return hr;
			if (FAILED(hr = SaveAllMipSurfaces(pddsFaceTop, ar)))
				return hr;
			ReleasePpo(&pddsFaceTop);
		}
		if ((m_dwCubeMapFlags & DDSCAPS2_CUBEMAP_POSITIVEZ) && !(dwTopCubeFace & DDSCAPS2_CUBEMAP_POSITIVEZ))
		{
			if (FAILED(hr = GetTopCubeFace(pdds, DDSCAPS2_CUBEMAP_POSITIVEZ, &pddsFaceTop)))
				return hr;
			if (FAILED(hr = SaveAllMipSurfaces(pddsFaceTop, ar)))
				return hr;
			ReleasePpo(&pddsFaceTop);
		}
		if ((m_dwCubeMapFlags & DDSCAPS2_CUBEMAP_NEGATIVEZ) && !(dwTopCubeFace & DDSCAPS2_CUBEMAP_NEGATIVEZ))
		{
			if (FAILED(hr = GetTopCubeFace(pdds, DDSCAPS2_CUBEMAP_NEGATIVEZ, &pddsFaceTop)))
				return hr;
			if (FAILED(hr = SaveAllMipSurfaces(pddsFaceTop, ar)))
				return hr;
			ReleasePpo(&pddsFaceTop);
		}
	}

	return S_OK;
}


HRESULT CDxtexDoc::SaveAllMipSurfaces(LPDIRECTDRAWSURFACE7 pddsTop, CArchive& ar)
{
	HRESULT hr;
	DDSCAPS2 ddsCaps;
	LPDIRECTDRAWSURFACE7 pdds;
	LPDIRECTDRAWSURFACE7 pdds2;
	DDSURFACEDESC2 ddsd;

	pdds = pddsTop;
	pdds->AddRef();

	ZeroMemory(&ddsCaps, sizeof(ddsCaps));
	ddsCaps.dwCaps = DDSCAPS_TEXTURE;
	ddsCaps.dwCaps2 = DDSCAPS2_MIPMAPSUBLEVEL;

	while (TRUE)
	{
		ZeroMemory(&ddsd, sizeof(ddsd));
		ddsd.dwSize = sizeof(ddsd);
		hr = pdds->Lock(NULL, &ddsd, DDLOCK_WAIT, NULL);
		if (ddsd.dwFlags & DDSD_LINEARSIZE)
		{
			ar.Write(ddsd.lpSurface, ddsd.dwLinearSize);
		}
		else 
		{
			DWORD yp;
			BYTE* pbSrc = (BYTE*)ddsd.lpSurface;
			LONG dataBytesPerRow = ddsd.dwWidth * ddsd.ddpfPixelFormat.dwRGBBitCount / 8;
			for (yp = 0; yp < ddsd.dwHeight; yp++)
			{
				ar.Write(pbSrc, dataBytesPerRow);
				pbSrc += ddsd.lPitch;
			}
		}
		pdds->Unlock(NULL);

		if (FAILED(hr = pdds->GetAttachedSurface(&ddsCaps, &pdds2)))
		{
			ReleasePpo(&pdds);
			break;
		}
		ReleasePpo(&pdds);
		pdds = pdds2;
	}
	return S_OK;
}


HRESULT CDxtexDoc::LoadDDS(LPDIRECTDRAWSURFACE7* ppdds, CArchive& ar)
{
	HRESULT hr;
	DWORD dwMagic;
	LPDIRECTDRAWSURFACE7 pdds;
	DDSURFACEDESC2 ddsd;
	DWORD dwTopCubeFace;

	ar.Read(&dwMagic, sizeof(dwMagic));
	if (dwMagic != MAKEFOURCC('D','D','S',' '))
		return E_FAIL;
	ar.Read(&ddsd, sizeof(ddsd));
	if (ddsd.dwSize != sizeof(ddsd))
		return E_FAIL;
	ddsd.ddsCaps.dwCaps |= DDSCAPS_SYSTEMMEMORY;
	ddsd.dwFlags = DDSD_CAPS | DDSD_WIDTH | DDSD_HEIGHT | DDSD_PIXELFORMAT;
	m_dwWidth = ddsd.dwWidth;
	m_dwHeight = ddsd.dwHeight;
	m_dwCubeMapFlags = (ddsd.ddsCaps.dwCaps2 & DDSCAPS2_CUBEMAP_ALLFACES);
	if (FAILED(hr = PDxtexApp()->Pdd()->CreateSurface(&ddsd, &pdds, NULL)))
		return hr;

	if (FAILED(hr = LoadAllMipSurfaces(pdds, ar)))
		return hr;
	
	// If this is a cube map, remember which cubemap face is the "top" one
	ZeroMemory(&ddsd, sizeof(ddsd));
	ddsd.dwSize = sizeof(ddsd);
	if (FAILED(hr = pdds->GetSurfaceDesc(&ddsd)))
		return hr;
	dwTopCubeFace = (ddsd.ddsCaps.dwCaps2 & DDSCAPS2_CUBEMAP_ALLFACES);

	// Load additional cubemap faces, if any
	if (m_dwCubeMapFlags != dwTopCubeFace)
	{
		LPDIRECTDRAWSURFACE7 pddsFaceTop = NULL;

		if ((m_dwCubeMapFlags & DDSCAPS2_CUBEMAP_POSITIVEX) && !(dwTopCubeFace & DDSCAPS2_CUBEMAP_POSITIVEX))
		{
			if (FAILED(hr = GetTopCubeFace(pdds, DDSCAPS2_CUBEMAP_POSITIVEX, &pddsFaceTop)))
				return hr;
			if (FAILED(hr = LoadAllMipSurfaces(pddsFaceTop, ar)))
				return hr;
			ReleasePpo(&pddsFaceTop);
		}
		if ((m_dwCubeMapFlags & DDSCAPS2_CUBEMAP_NEGATIVEX) && !(dwTopCubeFace & DDSCAPS2_CUBEMAP_NEGATIVEX))
		{
			if (FAILED(hr = GetTopCubeFace(pdds, DDSCAPS2_CUBEMAP_NEGATIVEX, &pddsFaceTop)))
				return hr;
			if (FAILED(hr = LoadAllMipSurfaces(pddsFaceTop, ar)))
				return hr;
			ReleasePpo(&pddsFaceTop);
		}
		if ((m_dwCubeMapFlags & DDSCAPS2_CUBEMAP_POSITIVEY) && !(dwTopCubeFace & DDSCAPS2_CUBEMAP_POSITIVEY))
		{
			if (FAILED(hr = GetTopCubeFace(pdds, DDSCAPS2_CUBEMAP_POSITIVEY, &pddsFaceTop)))
				return hr;
			if (FAILED(hr = LoadAllMipSurfaces(pddsFaceTop, ar)))
				return hr;
			ReleasePpo(&pddsFaceTop);
		}
		if ((m_dwCubeMapFlags & DDSCAPS2_CUBEMAP_NEGATIVEY) && !(dwTopCubeFace & DDSCAPS2_CUBEMAP_NEGATIVEY))
		{
			if (FAILED(hr = GetTopCubeFace(pdds, DDSCAPS2_CUBEMAP_NEGATIVEY, &pddsFaceTop)))
				return hr;
			if (FAILED(hr = LoadAllMipSurfaces(pddsFaceTop, ar)))
				return hr;
			ReleasePpo(&pddsFaceTop);
		}
		if ((m_dwCubeMapFlags & DDSCAPS2_CUBEMAP_POSITIVEZ) && !(dwTopCubeFace & DDSCAPS2_CUBEMAP_POSITIVEZ))
		{
			if (FAILED(hr = GetTopCubeFace(pdds, DDSCAPS2_CUBEMAP_POSITIVEZ, &pddsFaceTop)))
				return hr;
			if (FAILED(hr = LoadAllMipSurfaces(pddsFaceTop, ar)))
				return hr;
			ReleasePpo(&pddsFaceTop);
		}
		if ((m_dwCubeMapFlags & DDSCAPS2_CUBEMAP_NEGATIVEZ) && !(dwTopCubeFace & DDSCAPS2_CUBEMAP_NEGATIVEZ))
		{
			if (FAILED(hr = GetTopCubeFace(pdds, DDSCAPS2_CUBEMAP_NEGATIVEZ, &pddsFaceTop)))
				return hr;
			if (FAILED(hr = LoadAllMipSurfaces(pddsFaceTop, ar)))
				return hr;
			ReleasePpo(&pddsFaceTop);
		}
	}

	*ppdds = pdds;
	return S_OK;
}


HRESULT CDxtexDoc::LoadAllMipSurfaces(LPDIRECTDRAWSURFACE7 pddsTop, CArchive& ar)
{
	HRESULT hr;
	LPDIRECTDRAWSURFACE7 pdds;
	LPDIRECTDRAWSURFACE7 pdds2;
	DDSCAPS2 ddsCaps;
	DDSURFACEDESC2 ddsd;

	pdds = pddsTop;
	pdds->AddRef();

	ZeroMemory(&ddsCaps, sizeof(ddsCaps));
	ddsCaps.dwCaps = DDSCAPS_TEXTURE;
	ddsCaps.dwCaps2 = DDSCAPS2_MIPMAPSUBLEVEL;

	pdds->AddRef();
	m_numMips = 0;
	while (TRUE)
	{
		ZeroMemory(&ddsd, sizeof(ddsd));
		ddsd.dwSize = sizeof(ddsd);
		pdds->Lock(NULL, &ddsd, DDLOCK_WAIT, NULL);
		if (ddsd.dwFlags & DDSD_LINEARSIZE)
		{
			ar.Read(ddsd.lpSurface, ddsd.dwLinearSize);
		}
		else
		{
			DWORD yp;
			BYTE* pbDest = (BYTE*)ddsd.lpSurface;
			LONG dataBytesPerRow = ddsd.dwWidth * ddsd.ddpfPixelFormat.dwRGBBitCount / 8;
			for (yp = 0; yp < ddsd.dwHeight; yp++)
			{
				ar.Read(pbDest, dataBytesPerRow);
				pbDest += ddsd.lPitch;
			}
		}
		m_numMips++;
		pdds->Unlock(NULL);
		if (FAILED(hr = pdds->GetAttachedSurface(&ddsCaps, &pdds2)))
		{
			ReleasePpo(&pdds);
			break;
		}
		ReleasePpo(&pdds);
		pdds = pdds2;
	}
	return S_OK;
}


/////////////////////////////////////////////////////////////////////////////
// CDxtexDoc commands

HRESULT CDxtexDoc::LoadBmp(CString& strPath)
{
	HRESULT hr;
	LPDIRECTDRAWSURFACE7 pdds;
	DDSURFACEDESC2 ddsd;

	if (FAILED(hr = CreateSurfaceFromBmp(strPath, &pdds)))
		return hr;

	// Ensure that source image dimensions are power of 2
	ZeroMemory(&ddsd, sizeof(ddsd));
	ddsd.dwSize = sizeof(ddsd);
	if (FAILED(hr = pdds->GetSurfaceDesc(&ddsd)))
	{
		ReleasePpo(&pdds);
		return hr;
	}
	LONG lwTempW;
	LONG lwTempH;
	lwTempW = ddsd.dwWidth;
	lwTempH = ddsd.dwHeight;
	while ((lwTempW & 1) == 0)
		lwTempW = lwTempW >> 1;
	while ((lwTempH & 1) == 0)
		lwTempH = lwTempH >> 1;
	if (lwTempW != 1 || lwTempH != 1)
	{
		AfxMessageBox(ID_ERROR_NOTPOW2);
		ReleasePpo(&pdds);
		return E_FAIL;
	}

	ReleasePpo(&m_pddsOrig);
	m_pddsOrig = pdds;
	m_dwWidth = ddsd.dwWidth;
	m_dwHeight = ddsd.dwHeight;
	m_numMips = 1;

	// Look for "foo_a.bmp" for alpha channel
	int i = strPath.ReverseFind('.');
	strPath = strPath.Left(i) + "_a.bmp";
	CFileStatus status;
	if (CFile::GetStatus(strPath, status))
	{
		if (FAILED(hr = LoadAlphaIntoSurface(strPath, m_pddsOrig)))
			return hr;
	}

	m_strTitle.Empty();
	m_strPathName.Empty();
	SetModifiedFlag(TRUE);

	return S_OK;
}


HRESULT CDxtexDoc::LoadAlphaBmp(CString& strPath)
{
	HRESULT hr;

	if (FAILED(hr = LoadAlphaIntoSurface(strPath, m_pddsOrig)))
		return hr;

	UpdateAllViews(NULL, 1); // tell CView to pick up new surface pointers
	return S_OK;
}


HRESULT CDxtexDoc::Compress(DWORD dwFourCC, BOOL bSwitchView)
{
	HRESULT hr;
	DDSURFACEDESC2 ddsdOrig;
	DDSURFACEDESC2 ddsdComp;
	LPDIRECTDRAWSURFACE7 pddsSrc = NULL;
	LPDIRECTDRAWSURFACE7 pddsSrc2 = NULL;
	LPDIRECTDRAWSURFACE7 pddsDest = NULL;
	LPDIRECTDRAWSURFACE7 pddsDest2 = NULL;
	
	ReleasePpo(&m_pddsNew);
	
	ZeroMemory(&ddsdOrig, sizeof(ddsdOrig));
	ddsdOrig.dwSize = sizeof(ddsdOrig);
	if (FAILED(hr = m_pddsOrig->GetSurfaceDesc(&ddsdOrig)))
		return hr;

	// If source is DXT2 or DXT4, we can't convert to anything with nonpremult
	// alpha because of premult -> nonpremult Blt issues.  We can convert to 
	// DXT1, but RGB will be affected by alpha
	if ((ddsdOrig.ddpfPixelFormat.dwFlags & DDPF_FOURCC) &&
		(ddsdOrig.ddpfPixelFormat.dwFourCC == FOURCC_DXT2 ||
		 ddsdOrig.ddpfPixelFormat.dwFourCC == FOURCC_DXT4))
	{
		if (dwFourCC == FOURCC_DXT1)
		{
			AfxMessageBox(ID_ERROR_PREMULTTODXT1);
		}
		else if (dwFourCC != FOURCC_DXT2 &&
			dwFourCC != FOURCC_DXT4)
		{
			AfxMessageBox(ID_ERROR_PREMULTALPHA);
			return S_OK;
		}
	}

	// Make m_pddsNew exactly like m_pddsOrig except in specified DXTn format
	ddsdComp = ddsdOrig;
	ddsdComp.dwFlags = DDSD_CAPS | DDSD_WIDTH | DDSD_HEIGHT | DDSD_PIXELFORMAT;
	if (m_dwCubeMapFlags != 0)
		ddsdComp.ddsCaps.dwCaps2 = DDSCAPS2_CUBEMAP | m_dwCubeMapFlags;
	ZeroMemory(&ddsdComp.ddpfPixelFormat, sizeof(ddsdComp.ddpfPixelFormat));
	ddsdComp.ddpfPixelFormat.dwSize = sizeof(DDPIXELFORMAT);
	ddsdComp.ddpfPixelFormat.dwFlags = DDPF_FOURCC;
	ddsdComp.ddpfPixelFormat.dwFourCC = dwFourCC;

	if (FAILED(hr = PDxtexApp()->Pdd()->CreateSurface(&ddsdComp, &m_pddsNew, NULL)))
		return hr;

	if (m_dwCubeMapFlags == 0)
	{
		if (FAILED(BltAllLevels(m_pddsOrig, m_pddsNew)))
			return hr;
	}
	else
	{
		LPDIRECTDRAWSURFACE7 pddsSrcFaceTop = NULL;
		LPDIRECTDRAWSURFACE7 pddsDestFaceTop = NULL;
		if (m_dwCubeMapFlags & DDSCAPS2_CUBEMAP_NEGATIVEX)
		{
			if (FAILED(hr = GetTopCubeFace(m_pddsOrig, DDSCAPS2_CUBEMAP_NEGATIVEX, &pddsSrcFaceTop)))
				return hr;
			if (FAILED(hr = GetTopCubeFace(m_pddsNew, DDSCAPS2_CUBEMAP_NEGATIVEX, &pddsDestFaceTop)))
				return hr;
			if (FAILED(hr = BltAllLevels(pddsSrcFaceTop, pddsDestFaceTop)))
				return hr;
			ReleasePpo(&pddsSrcFaceTop);
			ReleasePpo(&pddsDestFaceTop);
		}
		if (m_dwCubeMapFlags & DDSCAPS2_CUBEMAP_POSITIVEX)
		{
			if (FAILED(hr = GetTopCubeFace(m_pddsOrig, DDSCAPS2_CUBEMAP_POSITIVEX, &pddsSrcFaceTop)))
				return hr;
			if (FAILED(hr = GetTopCubeFace(m_pddsNew, DDSCAPS2_CUBEMAP_POSITIVEX, &pddsDestFaceTop)))
				return hr;
			if (FAILED(hr = BltAllLevels(pddsSrcFaceTop, pddsDestFaceTop)))
				return hr;
			ReleasePpo(&pddsSrcFaceTop);
			ReleasePpo(&pddsDestFaceTop);
		}
		if (m_dwCubeMapFlags & DDSCAPS2_CUBEMAP_NEGATIVEY)
		{
			if (FAILED(hr = GetTopCubeFace(m_pddsOrig, DDSCAPS2_CUBEMAP_NEGATIVEY, &pddsSrcFaceTop)))
				return hr;
			if (FAILED(hr = GetTopCubeFace(m_pddsNew, DDSCAPS2_CUBEMAP_NEGATIVEY, &pddsDestFaceTop)))
				return hr;
			if (FAILED(hr = BltAllLevels(pddsSrcFaceTop, pddsDestFaceTop)))
				return hr;
			ReleasePpo(&pddsSrcFaceTop);
			ReleasePpo(&pddsDestFaceTop);
		}
		if (m_dwCubeMapFlags & DDSCAPS2_CUBEMAP_POSITIVEY)
		{
			if (FAILED(hr = GetTopCubeFace(m_pddsOrig, DDSCAPS2_CUBEMAP_POSITIVEY, &pddsSrcFaceTop)))
				return hr;
			if (FAILED(hr = GetTopCubeFace(m_pddsNew, DDSCAPS2_CUBEMAP_POSITIVEY, &pddsDestFaceTop)))
				return hr;
			if (FAILED(hr = BltAllLevels(pddsSrcFaceTop, pddsDestFaceTop)))
				return hr;
			ReleasePpo(&pddsSrcFaceTop);
			ReleasePpo(&pddsDestFaceTop);
		}
		if (m_dwCubeMapFlags & DDSCAPS2_CUBEMAP_NEGATIVEZ)
		{
			if (FAILED(hr = GetTopCubeFace(m_pddsOrig, DDSCAPS2_CUBEMAP_NEGATIVEZ, &pddsSrcFaceTop)))
				return hr;
			if (FAILED(hr = GetTopCubeFace(m_pddsNew, DDSCAPS2_CUBEMAP_NEGATIVEZ, &pddsDestFaceTop)))
				return hr;
			if (FAILED(hr = BltAllLevels(pddsSrcFaceTop, pddsDestFaceTop)))
				return hr;
			ReleasePpo(&pddsSrcFaceTop);
			ReleasePpo(&pddsDestFaceTop);
		}
		if (m_dwCubeMapFlags & DDSCAPS2_CUBEMAP_POSITIVEZ)
		{
			if (FAILED(hr = GetTopCubeFace(m_pddsOrig, DDSCAPS2_CUBEMAP_POSITIVEZ, &pddsSrcFaceTop)))
				return hr;
			if (FAILED(hr = GetTopCubeFace(m_pddsNew, DDSCAPS2_CUBEMAP_POSITIVEZ, &pddsDestFaceTop)))
				return hr;
			if (FAILED(hr = BltAllLevels(pddsSrcFaceTop, pddsDestFaceTop)))
				return hr;
			ReleasePpo(&pddsSrcFaceTop);
			ReleasePpo(&pddsDestFaceTop);
		}
	}

	SetModifiedFlag();
	m_bTitleModsChanged = TRUE; // force title bar update
	if (bSwitchView)
		AfxGetMainWnd()->PostMessage(WM_COMMAND, ID_VIEW_COMPRESSED, 0);

	return S_OK;
}


void CDxtexDoc::OnGenerateMipMaps() 
{
	GenerateMipMaps();
}


void CDxtexDoc::GenerateMipMaps() 
{
	LONG lwTempH;
	LONG lwTempW;
	LPDIRECTDRAWSURFACE7 pddsNew = NULL;
	DDSURFACEDESC2 ddsd;
	HRESULT hr;

	ddsd.dwSize = sizeof(ddsd);
	if (FAILED(hr = m_pddsOrig->GetSurfaceDesc(&ddsd)))
		goto LFail;

	// Ensure that source image is power of 2
	lwTempW = ddsd.dwWidth;
	lwTempH = ddsd.dwHeight;
	while ((lwTempW & 1) == 0)
		lwTempW = lwTempW >> 1;
	while ((lwTempH & 1) == 0)
		lwTempH = lwTempH >> 1;
	if (lwTempW != 1 || lwTempH != 1)
	{
		AfxMessageBox(ID_ERROR_NOTPOW2);
		return;
	}

	// Create destination mipmap surface - same format as source
	ddsd.dwFlags = DDSD_CAPS | DDSD_WIDTH | DDSD_HEIGHT | DDSD_PIXELFORMAT;
	ddsd.ddsCaps.dwCaps = DDSCAPS_SYSTEMMEMORY | DDSCAPS_TEXTURE | DDSCAPS_MIPMAP | DDSCAPS_COMPLEX;
	if (m_dwCubeMapFlags != 0)
		ddsd.ddsCaps.dwCaps2 = DDSCAPS2_CUBEMAP | m_dwCubeMapFlags;

	if (FAILED(hr = PDxtexApp()->Pdd()->CreateSurface(&ddsd, &pddsNew, NULL)))
		goto LFail;

	if (m_dwCubeMapFlags == 0)
	{
		// Copy top mip level - no filtering
		if (FAILED(hr = pddsNew->Blt(NULL, m_pddsOrig, NULL, DDBLT_WAIT, NULL)))
			goto LFail;
		if (FAILED(hr = GenerateMipMapsFromTop(pddsNew)))
			goto LFail;
	}
	else
	{
		LPDIRECTDRAWSURFACE7 pddsSrcFaceTop = NULL;
		LPDIRECTDRAWSURFACE7 pddsDestFaceTop = NULL;
		if (m_dwCubeMapFlags & DDSCAPS2_CUBEMAP_NEGATIVEX)
		{
			if (FAILED(hr = GetTopCubeFace(m_pddsOrig, DDSCAPS2_CUBEMAP_NEGATIVEX, &pddsSrcFaceTop)))
				return;
			if (FAILED(hr = GetTopCubeFace(pddsNew, DDSCAPS2_CUBEMAP_NEGATIVEX, &pddsDestFaceTop)))
				return;
			if (FAILED(hr = pddsDestFaceTop->Blt(NULL, pddsSrcFaceTop, NULL, DDBLT_WAIT, NULL)))
				return;
			if (FAILED(hr = GenerateMipMapsFromTop(pddsDestFaceTop)))
				return;
			ReleasePpo(&pddsSrcFaceTop);
			ReleasePpo(&pddsDestFaceTop);
		}
		if (m_dwCubeMapFlags & DDSCAPS2_CUBEMAP_POSITIVEX)
		{
			if (FAILED(hr = GetTopCubeFace(m_pddsOrig, DDSCAPS2_CUBEMAP_POSITIVEX, &pddsSrcFaceTop)))
				return;
			if (FAILED(hr = GetTopCubeFace(pddsNew, DDSCAPS2_CUBEMAP_POSITIVEX, &pddsDestFaceTop)))
				return;
			if (FAILED(hr = pddsDestFaceTop->Blt(NULL, pddsSrcFaceTop, NULL, DDBLT_WAIT, NULL)))
				return;
			if (FAILED(hr = GenerateMipMapsFromTop(pddsDestFaceTop)))
				return;
			ReleasePpo(&pddsSrcFaceTop);
			ReleasePpo(&pddsDestFaceTop);
		}
		if (m_dwCubeMapFlags & DDSCAPS2_CUBEMAP_NEGATIVEY)
		{
			if (FAILED(hr = GetTopCubeFace(m_pddsOrig, DDSCAPS2_CUBEMAP_NEGATIVEY, &pddsSrcFaceTop)))
				return;
			if (FAILED(hr = GetTopCubeFace(pddsNew, DDSCAPS2_CUBEMAP_NEGATIVEY, &pddsDestFaceTop)))
				return;
			if (FAILED(hr = pddsDestFaceTop->Blt(NULL, pddsSrcFaceTop, NULL, DDBLT_WAIT, NULL)))
				return;
			if (FAILED(hr = GenerateMipMapsFromTop(pddsDestFaceTop)))
				return;
			ReleasePpo(&pddsSrcFaceTop);
			ReleasePpo(&pddsDestFaceTop);
		}
		if (m_dwCubeMapFlags & DDSCAPS2_CUBEMAP_POSITIVEY)
		{
			if (FAILED(hr = GetTopCubeFace(m_pddsOrig, DDSCAPS2_CUBEMAP_POSITIVEY, &pddsSrcFaceTop)))
				return;
			if (FAILED(hr = GetTopCubeFace(pddsNew, DDSCAPS2_CUBEMAP_POSITIVEY, &pddsDestFaceTop)))
				return;
			if (FAILED(hr = pddsDestFaceTop->Blt(NULL, pddsSrcFaceTop, NULL, DDBLT_WAIT, NULL)))
				return;
			if (FAILED(hr = GenerateMipMapsFromTop(pddsDestFaceTop)))
				return;
			ReleasePpo(&pddsSrcFaceTop);
			ReleasePpo(&pddsDestFaceTop);
		}
		if (m_dwCubeMapFlags & DDSCAPS2_CUBEMAP_NEGATIVEZ)
		{
			if (FAILED(hr = GetTopCubeFace(m_pddsOrig, DDSCAPS2_CUBEMAP_NEGATIVEZ, &pddsSrcFaceTop)))
				return;
			if (FAILED(hr = GetTopCubeFace(pddsNew, DDSCAPS2_CUBEMAP_NEGATIVEZ, &pddsDestFaceTop)))
				return;
			if (FAILED(hr = pddsDestFaceTop->Blt(NULL, pddsSrcFaceTop, NULL, DDBLT_WAIT, NULL)))
				return;
			if (FAILED(hr = GenerateMipMapsFromTop(pddsDestFaceTop)))
				return;
			ReleasePpo(&pddsSrcFaceTop);
			ReleasePpo(&pddsDestFaceTop);
		}
		if (m_dwCubeMapFlags & DDSCAPS2_CUBEMAP_POSITIVEZ)
		{
			if (FAILED(hr = GetTopCubeFace(m_pddsOrig, DDSCAPS2_CUBEMAP_POSITIVEZ, &pddsSrcFaceTop)))
				return;
			if (FAILED(hr = GetTopCubeFace(pddsNew, DDSCAPS2_CUBEMAP_POSITIVEZ, &pddsDestFaceTop)))
				return;
			if (FAILED(hr = pddsDestFaceTop->Blt(NULL, pddsSrcFaceTop, NULL, DDBLT_WAIT, NULL)))
				return;
			if (FAILED(hr = GenerateMipMapsFromTop(pddsDestFaceTop)))
				return;
			ReleasePpo(&pddsSrcFaceTop);
			ReleasePpo(&pddsDestFaceTop);
		}
	}

	ReleasePpo(&m_pddsOrig);
	m_pddsOrig = pddsNew;

	if (m_pddsNew != NULL)
	{
		m_pddsNew->GetSurfaceDesc(&ddsd);
		Compress(ddsd.ddpfPixelFormat.dwFourCC, FALSE);
	}

	m_bTitleModsChanged = TRUE; // Generate title bar update
	UpdateAllViews(NULL, 1); // tell CView to pick up new surface pointers
	SetModifiedFlag();
	return;

LFail:
	ReleasePpo(&pddsNew);
}


// Generate lower mip levels by filtering down top level
HRESULT CDxtexDoc::GenerateMipMapsFromTop(LPDIRECTDRAWSURFACE7 pddsSrcTop)
{
	HRESULT hr;
	DDSURFACEDESC2 ddsd;
	LPDIRECTDRAWSURFACE7 pddsTempSrc = NULL;
	LPDIRECTDRAWSURFACE7 pddsTempDest = NULL;
	LPDIRECTDRAWSURFACE7 pddsCurMip = NULL;
	LPDIRECTDRAWSURFACE7 pddsPrevMip = NULL;
	LPDIRECTDRAWSURFACE7 pddsT = NULL;
	DDSCAPS2 ddsCaps;
	RECT rcDest;

	ddsd.dwSize = sizeof(ddsd);
	if (FAILED(hr = pddsSrcTop->GetSurfaceDesc(&ddsd)))
		goto LFail;
	SetRect(&rcDest, 0, 0, ddsd.dwWidth, ddsd.dwHeight);

	// Generate temporary ARGB-8888 surfaces if source is not ARGB-8888
	if (ddsd.ddpfPixelFormat.dwRGBBitCount != 32)
	{
		ddsd.dwFlags = DDSD_CAPS | DDSD_WIDTH | DDSD_HEIGHT | DDSD_PIXELFORMAT;
		ddsd.ddsCaps.dwCaps = DDSCAPS_OFFSCREENPLAIN;
		ddsd.ddsCaps.dwCaps2 = 0;
		ddsd.ddpfPixelFormat.dwFlags = DDPF_RGB | DDPF_ALPHAPIXELS;
		ddsd.ddpfPixelFormat.dwFourCC = 0;
		ddsd.ddpfPixelFormat.dwRGBBitCount = 32;
		ddsd.ddpfPixelFormat.dwRBitMask = 0x00ff0000;
		ddsd.ddpfPixelFormat.dwGBitMask = 0x0000ff00;
		ddsd.ddpfPixelFormat.dwBBitMask = 0x000000ff;
		ddsd.ddpfPixelFormat.dwRGBAlphaBitMask = 0xff000000;
		if (FAILED(hr = PDxtexApp()->Pdd()->CreateSurface(&ddsd, &pddsTempSrc, NULL)))
			goto LFail;
		if (FAILED(hr = PDxtexApp()->Pdd()->CreateSurface(&ddsd, &pddsTempDest, NULL)))
			goto LFail;
		if (FAILED(hr = pddsTempSrc->Blt(NULL, pddsSrcTop, NULL, DDBLT_WAIT, NULL)))
			goto LFail;
	}

	pddsCurMip = NULL;
	pddsPrevMip = pddsSrcTop;
	pddsPrevMip->AddRef();
	ZeroMemory(&ddsCaps, sizeof(ddsCaps));
	ddsCaps.dwCaps = DDSCAPS_TEXTURE;
	ddsCaps.dwCaps2 = DDSCAPS2_MIPMAPSUBLEVEL;

	m_numMips = 1;
	while (TRUE)
	{
		if (FAILED(pddsPrevMip->GetAttachedSurface(&ddsCaps, &pddsCurMip)))
			break;
		m_numMips++;
		rcDest.right /= 2;
		rcDest.bottom /= 2;
		if (pddsTempSrc == NULL)
		{
			GenerateMip(pddsPrevMip, pddsCurMip, &rcDest);
		}
		else
		{
			GenerateMip(pddsTempSrc, pddsTempDest, &rcDest);
			if (FAILED(hr = pddsCurMip->Blt(&rcDest, pddsTempDest, &rcDest, DDBLT_WAIT, NULL)))
				goto LEnd;
			// swap pointers so pddsTempDest's reduced image is used as source next time
			pddsT = pddsTempSrc;
			pddsTempSrc = pddsTempDest;
			pddsTempDest = pddsT;
		}
		ReleasePpo(&pddsPrevMip);
		pddsPrevMip = pddsCurMip;
	}
	ReleasePpo(&pddsCurMip);
LEnd:
	return S_OK;
LFail:
	ReleasePpo(&pddsTempSrc);
	ReleasePpo(&pddsTempDest);
	ReleasePpo(&pddsCurMip);
	ReleasePpo(&pddsPrevMip);
	return hr;
}


// Turn off optimizer to work around a VC compiler bug
#pragma optimize("", off)
#pragma optimize("q", off)

HRESULT CDxtexDoc::GenerateMip(LPDIRECTDRAWSURFACE7 pddsSrc, LPDIRECTDRAWSURFACE7 pddsDest, RECT * prcDest)
{
	HRESULT hr;
	LONG xpSrc;
	LONG ypSrc;
	DDSURFACEDESC2 ddsdSrc;
	BYTE* pbRowSrc;
	DWORD* pdwPixelSrc;
	DDSURFACEDESC2 ddsdDest;
	BYTE* pbRowDest;
	DWORD* pdwPixelDest;
	
	DWORD dwPixel1;
	DWORD dwPixel2;
	DWORD dwPixel3;
	DWORD dwPixel4;
	
	BYTE bRed;
	BYTE bGreen;
	BYTE bBlue;
	BYTE bAlpha;

	ZeroMemory(&ddsdSrc, sizeof(ddsdSrc));
	ddsdSrc.dwSize = sizeof(ddsdSrc);
	ZeroMemory(&ddsdDest, sizeof(ddsdDest));
	ddsdDest.dwSize = sizeof(ddsdDest);
	if (FAILED(hr = pddsSrc->Lock(NULL, &ddsdSrc, DDLOCK_WAIT, NULL)))
		return hr;
	if (FAILED(hr = pddsDest->Lock(NULL, &ddsdDest, DDLOCK_WAIT, NULL)))
		return hr;
	pbRowSrc = (BYTE*)ddsdSrc.lpSurface;
	pdwPixelSrc = (DWORD*)pbRowSrc;
	pbRowDest = (BYTE*)ddsdDest.lpSurface;
	pdwPixelDest = (DWORD*)pbRowDest;

	for (ypSrc = 0; ypSrc < prcDest->bottom * 2; ypSrc += 2)
	{
		for (xpSrc = 0; xpSrc < prcDest->right * 2; xpSrc += 2)
		{
			dwPixel1 = *(pdwPixelSrc);
			dwPixel2 = *(pdwPixelSrc + 1);
			dwPixel3 = *(pdwPixelSrc + ddsdSrc.lPitch / 4);
			dwPixel4 = *(pdwPixelSrc + ddsdSrc.lPitch / 4 + 1);
			bRed = (BYTE)((RGBA_GETRED(dwPixel1) + RGBA_GETRED(dwPixel2) +
				           RGBA_GETRED(dwPixel3) + RGBA_GETRED(dwPixel4)) / 4);
			bGreen = (BYTE)((RGBA_GETGREEN(dwPixel1) + RGBA_GETGREEN(dwPixel2) +
						     RGBA_GETGREEN(dwPixel3) + RGBA_GETGREEN(dwPixel4)) / 4);
			bBlue = (BYTE)((RGBA_GETBLUE(dwPixel1) + RGBA_GETBLUE(dwPixel2) +
						    RGBA_GETBLUE(dwPixel3) + RGBA_GETBLUE(dwPixel4)) / 4);
			bAlpha = (BYTE)((RGBA_GETALPHA(dwPixel1) + RGBA_GETALPHA(dwPixel2) +
						     RGBA_GETALPHA(dwPixel3) + RGBA_GETALPHA(dwPixel4)) / 4);
			*pdwPixelDest = RGBA_MAKE(bRed, bGreen, bBlue, bAlpha);
			pdwPixelSrc += 2;
			pdwPixelDest++;
		}
		pbRowSrc += 2 * ddsdSrc.lPitch;
		pdwPixelSrc = (DWORD*)pbRowSrc;
		pbRowDest += ddsdDest.lPitch;
		pdwPixelDest = (DWORD*)pbRowDest;
	}
	pddsSrc->Unlock(NULL);
	pddsDest->Unlock(NULL);
	return S_OK;
}

#pragma optimize("", on)
#pragma optimize("q", on)


void CDxtexDoc::SetPathName(LPCTSTR lpszPathName, BOOL bAddToMRU) 
{
	CDocument::SetPathName(lpszPathName, bAddToMRU);

	TCHAR* pszLeaf = strrchr(lpszPathName, '\\') + 1;
	TCHAR* pszExtension = strrchr(lpszPathName, '.');
	if (lstrcmpi(pszExtension, ".dds") != 0)
	{
		lstrcpy(pszExtension, "");
		SetModifiedFlag(TRUE);
		SetTitle(pszLeaf);
		m_strPathName.Empty();
	}
}

DWORD CDxtexDoc::NumMips(VOID)
{
	return m_numMips;
}

void CDxtexDoc::OnFormatDxt1() 
{
	Compress(FOURCC_DXT1, TRUE);	
}

void CDxtexDoc::OnFormatDxt2() 
{
	Compress(FOURCC_DXT2, TRUE);	
}

void CDxtexDoc::OnFormatDxt3() 
{
	Compress(FOURCC_DXT3, TRUE);	
}

void CDxtexDoc::OnFormatDxt4() 
{
	Compress(FOURCC_DXT4, TRUE);	
}


void CDxtexDoc::OnFormatDxt5() 
{
	Compress(FOURCC_DXT5, TRUE);	
}


void CDxtexDoc::OnFileOpenAlpha() 
{
	HRESULT hr;
	CString fileName;

	// Premultiplied-alpha files don't support this feature:
	DDSURFACEDESC2 ddsd;
	ZeroMemory(&ddsd, sizeof(ddsd));
	ddsd.dwSize = sizeof(ddsd);
	if (SUCCEEDED(hr = m_pddsOrig->GetSurfaceDesc(&ddsd)))
	{
		if ((ddsd.ddpfPixelFormat.dwFlags & DDPF_FOURCC) &&
			(ddsd.ddpfPixelFormat.dwFourCC == FOURCC_DXT2 ||
			 ddsd.ddpfPixelFormat.dwFourCC == FOURCC_DXT4))
		{
			AfxMessageBox(ID_ERROR_PREMULTALPHA);
			return;
		}
	}

	if (!PromptForBmp(&fileName))
		return;

	if (FAILED(hr = LoadAlphaIntoSurface(fileName, m_pddsOrig)))
		return;
	if (m_numMips > 1)
		OnGenerateMipMaps();
	else if (m_pddsNew != NULL)
	{
		DDSURFACEDESC2 ddsd;
		ddsd.dwSize = sizeof(ddsd);
		m_pddsNew->GetSurfaceDesc(&ddsd);
		Compress(ddsd.ddpfPixelFormat.dwFourCC, FALSE);
	}
	UpdateAllViews(NULL, 1);
}


// Creates a new 32-bit ARGB surface and loads strPath's BMP into it. Alpha is set to 0xff.
HRESULT CDxtexDoc::CreateSurfaceFromBmp(CString& strPath, LPDIRECTDRAWSURFACE7* ppDDS)
{
	HRESULT hr;
	HBITMAP hbm;
	BITMAP bm;
	HDC hdc;
	HDC hdcImage;
	HGDIOBJ hgdiobjOld = NULL;
	LPDIRECTDRAWSURFACE7 pddsLoad = NULL; // *ppDDS will be set to this
	DDSURFACEDESC2 ddsd;

	hbm = (HBITMAP)LoadImage(NULL, strPath, IMAGE_BITMAP, 0, 0, 
		LR_LOADFROMFILE | LR_CREATEDIBSECTION);
	if (hbm == NULL)
	{
		hr = E_FAIL;
		LONG lwErr = GetLastError();
		if (lwErr == ERROR_NOT_ENOUGH_MEMORY || lwErr == ERROR_OUTOFMEMORY)
			hr = E_OUTOFMEMORY;
		return hr;
	}
	GetObject(hbm, sizeof(bm), &bm);      // get size of bitmap

	ZeroMemory(&ddsd, sizeof(ddsd));
	ddsd.dwSize = sizeof(ddsd);
	ddsd.dwFlags = DDSD_CAPS | DDSD_WIDTH | DDSD_HEIGHT | DDSD_PIXELFORMAT;
	ddsd.ddsCaps.dwCaps = DDSCAPS_SYSTEMMEMORY | DDSCAPS_TEXTURE;
	ddsd.dwWidth = bm.bmWidth;
	ddsd.dwHeight = bm.bmHeight;
	ddsd.ddpfPixelFormat.dwSize = sizeof(DDPIXELFORMAT);
	ddsd.ddpfPixelFormat.dwFlags = DDPF_RGB | DDPF_ALPHAPIXELS;
	ddsd.ddpfPixelFormat.dwRGBBitCount = 32;
	ddsd.ddpfPixelFormat.dwRBitMask = 0x00ff0000;
	ddsd.ddpfPixelFormat.dwGBitMask = 0x0000ff00;
	ddsd.ddpfPixelFormat.dwBBitMask = 0x000000ff;
	ddsd.ddpfPixelFormat.dwRGBAlphaBitMask = 0xff000000;
	if (FAILED(hr = PDxtexApp()->Pdd()->CreateSurface(&ddsd, &pddsLoad, NULL)))
		return hr;

	//
	// WARNING: Win16Lock coming...don't step through this code!!
	//
	if (FAILED(hr = pddsLoad->GetDC(&hdc)))
		return hr;

	hdcImage = CreateCompatibleDC(NULL);
	if (hdcImage == NULL)
	{
		pddsLoad->ReleaseDC(hdc);
		return E_FAIL;
	}
	hgdiobjOld = SelectObject(hdcImage, hbm);
	StretchBlt(hdc, 0, 0, bm.bmWidth, bm.bmHeight, hdcImage, 0, 0, 
		bm.bmWidth, bm.bmHeight, SRCCOPY);
	SelectObject(hdcImage, hgdiobjOld); // restore previously selected object
	DeleteDC(hdcImage);
	DeleteObject(hbm);
	
	pddsLoad->ReleaseDC(hdc);

	// Fill alpha channel with 0xff
	DWORD xp;
	DWORD yp;
	BYTE* pbRow;
	DWORD* pdwPixel;
	if (FAILED(hr = pddsLoad->Lock(NULL, &ddsd, DDLOCK_WAIT, NULL)))
		return hr;
	pbRow = (BYTE*)ddsd.lpSurface;
	pdwPixel = (DWORD*)pbRow;
	for (yp = 0; yp < ddsd.dwHeight; yp++)
	{
		for (xp = 0; xp < ddsd.dwWidth; xp++)
		{
			*pdwPixel |= 0xff000000;
			pdwPixel++;
		}
		pbRow += ddsd.lPitch;
		pdwPixel = (DWORD*)pbRow;
	}
	pddsLoad->Unlock(NULL);

	*ppDDS = pddsLoad;
	return S_OK;
}


HRESULT CDxtexDoc::LoadAlphaIntoSurface(CString& strPath, LPDIRECTDRAWSURFACE7 pdds)
{
	HRESULT hr;
	HBITMAP hbm;
	BITMAP bm;
	DDSURFACEDESC2 ddsd;
	HDC hdc;
	HDC hdcImage;
	HGDIOBJ hgdiobjOld = NULL;
	LPDIRECTDRAWSURFACE7 pddsAlpha = NULL;
	LPDIRECTDRAWSURFACE7 pddsTemp = NULL;
	DWORD dwWidth;
	DWORD dwHeight;

	ZeroMemory(&ddsd, sizeof(ddsd));
	ddsd.dwSize = sizeof(ddsd);
	if (FAILED(hr = pdds->GetSurfaceDesc(&ddsd)))
		return hr;
	dwWidth = ddsd.dwWidth;
	dwHeight = ddsd.dwHeight;

	hbm = (HBITMAP)LoadImage(NULL, strPath, IMAGE_BITMAP, 0, 0, 
		LR_LOADFROMFILE | LR_CREATEDIBSECTION);
	if (hbm == NULL)
		return E_FAIL;
	GetObject(hbm, sizeof(bm), &bm);      // get size of bitmap

	if (bm.bmWidth != (LONG)dwWidth || bm.bmHeight != (LONG)dwHeight)
	{
		if (IDNO == AfxMessageBox(ID_ERROR_WRONGDIMENSIONS, MB_YESNO))
			return S_FALSE;
	}

	ZeroMemory(&ddsd, sizeof(ddsd));
	ddsd.dwSize = sizeof(ddsd);
	ddsd.dwFlags = DDSD_CAPS | DDSD_WIDTH | DDSD_HEIGHT | DDSD_PIXELFORMAT;
	ddsd.ddsCaps.dwCaps = DDSCAPS_SYSTEMMEMORY | DDSCAPS_TEXTURE;
	ddsd.dwWidth = dwWidth;
	ddsd.dwHeight = dwHeight;
	ddsd.ddpfPixelFormat.dwSize = sizeof(DDPIXELFORMAT);
	ddsd.ddpfPixelFormat.dwFlags = DDPF_RGB | DDPF_ALPHAPIXELS;
	ddsd.ddpfPixelFormat.dwRGBBitCount = 32;
	ddsd.ddpfPixelFormat.dwRBitMask = 0x00ff0000;
	ddsd.ddpfPixelFormat.dwGBitMask = 0x0000ff00;
	ddsd.ddpfPixelFormat.dwBBitMask = 0x000000ff;
	ddsd.ddpfPixelFormat.dwRGBAlphaBitMask = 0xff000000;

	if (FAILED(hr = PDxtexApp()->Pdd()->CreateSurface(&ddsd, &pddsAlpha, NULL)))
		return hr;
	if (FAILED(hr = pddsAlpha->GetDC(&hdc)))
		return hr;

	hdcImage = CreateCompatibleDC(NULL);
	if (hdcImage == NULL)
	{
		m_pddsOrig->ReleaseDC(hdc);
		return E_FAIL;
	}
	hgdiobjOld = SelectObject(hdcImage, hbm);
	StretchBlt(hdc, 0, 0, ddsd.dwWidth, ddsd.dwHeight, hdcImage, 0, 0, 
		bm.bmWidth, bm.bmHeight, SRCCOPY);
	SelectObject(hdcImage, hgdiobjOld); // restore previously selected object
	DeleteDC(hdcImage);
	DeleteObject(hbm);
	pddsAlpha->ReleaseDC(hdc);

	// If pdds is not RGBA, we need to create a temporary surface
	// in RGBA so we can insert the alpha values
	if (FAILED(hr = pdds->GetSurfaceDesc(&ddsd)))
		return hr;
	if ((ddsd.ddpfPixelFormat.dwFlags & DDPF_FOURCC))
	{
		ZeroMemory(&ddsd, sizeof(ddsd));
		ddsd.dwSize = sizeof(ddsd);
		ddsd.dwFlags = DDSD_CAPS | DDSD_WIDTH | DDSD_HEIGHT | DDSD_PIXELFORMAT;
		ddsd.ddsCaps.dwCaps = DDSCAPS_SYSTEMMEMORY | DDSCAPS_TEXTURE;
		ddsd.dwWidth = bm.bmWidth;
		ddsd.dwHeight = bm.bmHeight;
		ddsd.ddpfPixelFormat.dwSize = sizeof(DDPIXELFORMAT);
		ddsd.ddpfPixelFormat.dwFlags = DDPF_RGB | DDPF_ALPHAPIXELS;
		ddsd.ddpfPixelFormat.dwRGBBitCount = 32;
		ddsd.ddpfPixelFormat.dwRBitMask = 0x00ff0000;
		ddsd.ddpfPixelFormat.dwGBitMask = 0x0000ff00;
		ddsd.ddpfPixelFormat.dwBBitMask = 0x000000ff;
		ddsd.ddpfPixelFormat.dwRGBAlphaBitMask = 0xff000000;

		if (FAILED(hr = PDxtexApp()->Pdd()->CreateSurface(&ddsd, &pddsTemp, NULL)))
			return hr;
		if (FAILED(hr = pddsTemp->Blt(NULL, m_pddsOrig, NULL, DDBLT_WAIT, NULL)))
			return hr;
	}

	// Fill pdds's (or pddsTemp)'s alpha channel from pddsAlpha
	DWORD xp;
	DWORD yp;
	BYTE* pbRowSrc;
	DWORD* pdwPixelSrc;
	BYTE* pbRowDest;
	DWORD* pdwPixelDest;
	DDSURFACEDESC2 ddsdSrc;
	ZeroMemory(&ddsdSrc, sizeof(ddsdSrc));
	ddsdSrc.dwSize = sizeof(ddsdSrc);
	DDSURFACEDESC2 ddsdDest;
	ZeroMemory(&ddsdDest, sizeof(ddsdDest));
	ddsdDest.dwSize = sizeof(ddsdDest);
	if (FAILED(hr = pddsAlpha->Lock(NULL, &ddsdSrc, DDLOCK_WAIT, NULL)))
		return hr;
	if (pddsTemp == NULL)
	{
		if (FAILED(hr = pdds->Lock(NULL, &ddsdDest, DDLOCK_WAIT, NULL)))
			return hr;
	}
	else
	{
		if (FAILED(hr = pddsTemp->Lock(NULL, &ddsdDest, DDLOCK_WAIT, NULL)))
			return hr;
	}
	pbRowSrc = (BYTE*)ddsdSrc.lpSurface;
	pdwPixelSrc = (DWORD*)pbRowSrc;
	pbRowDest = (BYTE*)ddsdDest.lpSurface;
	pdwPixelDest = (DWORD*)pbRowDest;
	for (yp = 0; yp < ddsdSrc.dwHeight; yp++)
	{
		for (xp = 0; xp < ddsdSrc.dwWidth; xp++)
		{
			*pdwPixelDest &= 0x00ffffff;
			*pdwPixelDest |= (*pdwPixelSrc & 0x000000ff) << 24;
			pdwPixelSrc++;
			pdwPixelDest++;
		}
		pbRowSrc += ddsdSrc.lPitch;
		pdwPixelSrc = (DWORD*)pbRowSrc;
		pbRowDest += ddsdDest.lPitch;
		pdwPixelDest = (DWORD*)pbRowDest;
	}
	if (pddsTemp == NULL)
		pdds->Unlock(NULL);
	else
		pddsTemp->Unlock(NULL);
	pddsAlpha->Unlock(NULL);
	ReleasePpo(&pddsAlpha);
	if (pddsTemp != NULL)
	{
		if (FAILED(hr = pdds->Blt(NULL, pddsTemp, NULL, DDBLT_WAIT, NULL)))
			return hr;
		ReleasePpo(&pddsTemp);
	}
	return S_OK;
}


HRESULT CDxtexDoc::GetNthMipMap(LPDIRECTDRAWSURFACE7 pddsTop, LONG lwMip,
							   LPDIRECTDRAWSURFACE7* ppdds)
{
	HRESULT hr;
	LPDIRECTDRAWSURFACE7 pdds = pddsTop;
	LPDIRECTDRAWSURFACE7 pdds2 = NULL;
	DDSCAPS2 ddsCaps;
	ZeroMemory(&ddsCaps, sizeof(ddsCaps));
	ddsCaps.dwCaps = DDSCAPS_TEXTURE | DDSCAPS_MIPMAP;

	pdds->AddRef();
	while (lwMip > 0)
	{
		if (FAILED(hr = pdds->GetAttachedSurface(&ddsCaps, &pdds2)))
		{
			ReleasePpo(&pdds);
			return hr;
		}
		ReleasePpo(&pdds);
		pdds = pdds2;
		lwMip--;
	}
	*ppdds = pdds;
	return S_OK;
}


BOOL CDxtexDoc::PromptForBmp(CString* pstrPath)
{
	CFileDialog dlgFile(TRUE);

	CString title;
	VERIFY(title.LoadString(AFX_IDS_OPENFILE));

	CString strFilter;
	CString strDefault;

	strFilter += "Bitmap Files (*.bmp)";
	strFilter += (TCHAR)'\0';   // next string please
	strFilter += _T("*.bmp");
	strFilter += (TCHAR)'\0';   // last string
	dlgFile.m_ofn.nMaxCustFilter++;

	// append the "*.*" all files filter
	CString allFilter;
	VERIFY(allFilter.LoadString(AFX_IDS_ALLFILTER));
	strFilter += allFilter;
	strFilter += (TCHAR)'\0';   // next string please
	strFilter += _T("*.*");
	strFilter += (TCHAR)'\0';   // last string
	dlgFile.m_ofn.nMaxCustFilter++;

	dlgFile.m_ofn.lpstrFilter = strFilter;
	dlgFile.m_ofn.lpstrTitle = title;
	dlgFile.m_ofn.lpstrFile = pstrPath->GetBuffer(_MAX_PATH);

	int nResult = dlgFile.DoModal();
	pstrPath->ReleaseBuffer();
	if (nResult != IDOK)
		return FALSE;
	return TRUE;
}


void CDxtexDoc::OpenSubsurface(DWORD dwCubeMapFlags, LONG lwMip)
{
	HRESULT hr;
	CString fileName;
	LPDIRECTDRAWSURFACE7 pddsOrigTopFace = NULL;
	LPDIRECTDRAWSURFACE7 pddsNewTopFace = NULL;
	LPDIRECTDRAWSURFACE7 pddsOrigSubSurface = NULL;
	LPDIRECTDRAWSURFACE7 pddsNewSubSurface = NULL;
	LPDIRECTDRAWSURFACE7 pddsLoad = NULL;

	if (FAILED(hr = GetTopCubeFace(m_pddsOrig, dwCubeMapFlags, &pddsOrigTopFace)))
		return;
	if (FAILED(hr = GetNthMipMap(pddsOrigTopFace, lwMip, &pddsOrigSubSurface)))
		return;
	ReleasePpo(&pddsOrigTopFace);

	if (m_pddsNew != NULL)
	{
		if (FAILED(hr = GetTopCubeFace(m_pddsNew, dwCubeMapFlags, &pddsNewTopFace)))
			return;
		if (FAILED(hr = GetNthMipMap(pddsNewTopFace, lwMip, &pddsNewSubSurface)))
			return;
		ReleasePpo(&pddsNewTopFace);
	}

	if (!PromptForBmp(&fileName))
		return;

	if (FAILED(hr = CreateSurfaceFromBmp(fileName, &pddsLoad)))
	{
		// TODO: report error
		return;
	}

	// Make sure size is right:
	DDSURFACEDESC2 ddsd;
	ZeroMemory(&ddsd, sizeof(ddsd));
	ddsd.dwSize = sizeof(ddsd);
	if (FAILED(hr = pddsOrigSubSurface->GetSurfaceDesc(&ddsd)))
		return;
	DDSURFACEDESC2 ddsdLoad;
	ZeroMemory(&ddsdLoad, sizeof(ddsdLoad));
	ddsdLoad.dwSize = sizeof(ddsdLoad);
	if (FAILED(hr = pddsLoad->GetSurfaceDesc(&ddsdLoad)))
		return;
	if (ddsd.dwWidth != ddsdLoad.dwWidth || ddsd.dwHeight != ddsdLoad.dwHeight)
	{
		// TODO: ask if stretch-to-fit is ok
	}

	// Look for "foo_a.bmp" for alpha channel
	int i = fileName.ReverseFind('.');
	fileName = fileName.Left(i) + "_a.bmp";
	CFileStatus status;
	if (CFile::GetStatus(fileName, status))
	{
		if (FAILED(hr = LoadAlphaIntoSurface(fileName, pddsLoad)))
			return;
	}

	if (FAILED(hr = pddsOrigSubSurface->Blt(NULL, pddsLoad, NULL, DDBLT_WAIT, NULL)))
		return;

	if (pddsNewSubSurface != NULL)
	{
		if (FAILED(hr = pddsNewSubSurface->Blt(NULL, pddsLoad, NULL, DDBLT_WAIT, NULL)))
			return;
	}

	ReleasePpo(&pddsLoad);
	ReleasePpo(&pddsOrigSubSurface);
	ReleasePpo(&pddsNewSubSurface);

	SetModifiedFlag(TRUE);
	UpdateAllViews(NULL, 1);
}


void CDxtexDoc::OpenAlphaSubsurface(DWORD dwCubeMapFlags, LONG lwMip)
{
	HRESULT hr;
	CString fileName;
	LPDIRECTDRAWSURFACE7 pddsOrigTopFace = NULL;
	LPDIRECTDRAWSURFACE7 pddsNewTopFace = NULL;
	LPDIRECTDRAWSURFACE7 pddsOrigSubSurface = NULL;
	LPDIRECTDRAWSURFACE7 pddsNewSubSurface = NULL;
	LPDIRECTDRAWSURFACE7 pddsLoad = NULL;

	if (FAILED(hr = GetTopCubeFace(m_pddsOrig, dwCubeMapFlags, &pddsOrigTopFace)))
		return;
	if (FAILED(hr = GetNthMipMap(pddsOrigTopFace, lwMip, &pddsOrigSubSurface)))
		return;
	ReleasePpo(&pddsOrigTopFace);
	
	if (m_pddsNew != NULL)
	{
		if (FAILED(hr = GetTopCubeFace(m_pddsNew, dwCubeMapFlags, &pddsNewTopFace)))
			return;
		if (FAILED(hr = GetNthMipMap(pddsNewTopFace, lwMip, &pddsNewSubSurface)))
			return;
		ReleasePpo(&pddsNewTopFace);
	}

	// Premultiplied-alpha files don't support this feature:
	DDSURFACEDESC2 ddsd;
	ZeroMemory(&ddsd, sizeof(ddsd));
	ddsd.dwSize = sizeof(ddsd);
	if (SUCCEEDED(hr = pddsOrigSubSurface->GetSurfaceDesc(&ddsd)))
	{
		if ((ddsd.ddpfPixelFormat.dwFlags & DDPF_FOURCC) &&
			(ddsd.ddpfPixelFormat.dwFourCC == FOURCC_DXT2 ||
			 ddsd.ddpfPixelFormat.dwFourCC == FOURCC_DXT4))
		{
			AfxMessageBox(ID_ERROR_PREMULTALPHA);
			return;
		}
	}
	if (pddsNewSubSurface != NULL)
	{
		ZeroMemory(&ddsd, sizeof(ddsd));
		ddsd.dwSize = sizeof(ddsd);
		if (SUCCEEDED(hr = pddsNewSubSurface->GetSurfaceDesc(&ddsd)))
		{
			if ((ddsd.ddpfPixelFormat.dwFlags & DDPF_FOURCC) &&
				(ddsd.ddpfPixelFormat.dwFourCC == FOURCC_DXT2 ||
				 ddsd.ddpfPixelFormat.dwFourCC == FOURCC_DXT4))
			{
				AfxMessageBox(ID_ERROR_PREMULTALPHA);
				return;
			}
		}
	}
	if (!PromptForBmp(&fileName))
		return;

	if (FAILED(hr = LoadAlphaIntoSurface(fileName, pddsOrigSubSurface)))
		return;

	if (pddsNewSubSurface != NULL)
	{
		if (FAILED(hr = LoadAlphaIntoSurface(fileName, pddsNewSubSurface)))
			return;
	}

	ReleasePpo(&pddsOrigSubSurface);
	ReleasePpo(&pddsNewSubSurface);

	SetModifiedFlag(TRUE);
	UpdateAllViews(NULL, 1);
}


void CDxtexDoc::OnFormatChangeCubeMapFaces() 
{
	DWORD dwCubeMapFlagsNew = 0;
	HRESULT hr;

	CCubeMapDlg cubeMapDlg;
	if (m_dwCubeMapFlags & DDSCAPS2_CUBEMAP_NEGATIVEX)
		cubeMapDlg.m_bNegX = TRUE;
	if (m_dwCubeMapFlags & DDSCAPS2_CUBEMAP_POSITIVEX)
		cubeMapDlg.m_bPosX = TRUE;
	if (m_dwCubeMapFlags & DDSCAPS2_CUBEMAP_NEGATIVEY)
		cubeMapDlg.m_bNegY = TRUE;
	if (m_dwCubeMapFlags & DDSCAPS2_CUBEMAP_POSITIVEY)
		cubeMapDlg.m_bPosY = TRUE;
	if (m_dwCubeMapFlags & DDSCAPS2_CUBEMAP_NEGATIVEZ)
		cubeMapDlg.m_bNegZ = TRUE;
	if (m_dwCubeMapFlags & DDSCAPS2_CUBEMAP_POSITIVEZ)
		cubeMapDlg.m_bPosZ = TRUE;
	cubeMapDlg.DoModal();
	if (cubeMapDlg.m_bNegX)
		dwCubeMapFlagsNew |= DDSCAPS2_CUBEMAP_NEGATIVEX;
	if (cubeMapDlg.m_bPosX)
		dwCubeMapFlagsNew |= DDSCAPS2_CUBEMAP_POSITIVEX;
	if (cubeMapDlg.m_bNegY)
		dwCubeMapFlagsNew |= DDSCAPS2_CUBEMAP_NEGATIVEY;
	if (cubeMapDlg.m_bPosY)
		dwCubeMapFlagsNew |= DDSCAPS2_CUBEMAP_POSITIVEY;
	if (cubeMapDlg.m_bNegZ)
		dwCubeMapFlagsNew |= DDSCAPS2_CUBEMAP_NEGATIVEZ;
	if (cubeMapDlg.m_bPosZ)
		dwCubeMapFlagsNew |= DDSCAPS2_CUBEMAP_POSITIVEZ;

	if (dwCubeMapFlagsNew != m_dwCubeMapFlags)
	{
		if (FAILED(hr = ChangeCubeMapFlags(&m_pddsOrig, dwCubeMapFlagsNew)))
		{
		}
		if (m_pddsNew != NULL)
		{
			if (FAILED(hr = ChangeCubeMapFlags(&m_pddsNew, dwCubeMapFlagsNew)))
			{
			}
		}
	}
	SetModifiedFlag();
	UpdateAllViews(NULL, 1); // tell CView to pick up new surface pointers
}


HRESULT CDxtexDoc::ChangeCubeMapFlags(LPDIRECTDRAWSURFACE7* ppddsSrc, DWORD dwCubeMapFlagsNew)
{
	HRESULT hr;
	LPDIRECTDRAWSURFACE7 pddsNew = NULL;
	DDSURFACEDESC2 ddsd;
	BOOL bHadFacesBefore = FALSE;

	ZeroMemory(&ddsd, sizeof(ddsd));
	ddsd.dwSize = sizeof(ddsd);
	if (FAILED(hr = (*ppddsSrc)->GetSurfaceDesc(&ddsd)))
		return hr;
	if (ddsd.ddsCaps.dwCaps2 & DDSCAPS2_CUBEMAP)
		bHadFacesBefore = TRUE;
	if (ddsd.ddsCaps.dwCaps2 & DDSCAPS2_TEXTUREMANAGE)
		ddsd.ddsCaps.dwCaps &= ~(DDSCAPS_SYSTEMMEMORY | DDSCAPS_VIDEOMEMORY | DDSCAPS_LOCALVIDMEM | DDSCAPS_NONLOCALVIDMEM);
	if (!(ddsd.ddsCaps.dwCaps & DDSCAPS_MIPMAP))
		ddsd.ddsCaps.dwCaps &= ~DDSCAPS_COMPLEX;
	ddsd.ddsCaps.dwCaps2 &= ~(DDSCAPS2_CUBEMAP | DDSCAPS2_CUBEMAP_ALLFACES);
	if (dwCubeMapFlagsNew != 0)
	{
		ddsd.ddsCaps.dwCaps |= DDSCAPS_COMPLEX;
		ddsd.ddsCaps.dwCaps2 |= (DDSCAPS2_CUBEMAP | dwCubeMapFlagsNew);
	}

	ddsd.dwFlags = DDSD_CAPS | DDSD_HEIGHT | DDSD_WIDTH | DDSD_PIXELFORMAT;
	if (FAILED(hr = PDxtexApp()->Pdd()->CreateSurface(&ddsd, &pddsNew, NULL)))
		return hr;

	// Now need to copy all surface data from pddsSrc to pddsNew
	// If previous surface was not a cube map, just copy its data to "first" cube face
	if (!bHadFacesBefore)
	{
		if (FAILED(hr = BltAllLevels(*ppddsSrc, pddsNew)))
			return hr;
	}
	else
	{
		LPDIRECTDRAWSURFACE7 pddsNewFaceTop = NULL;
		LPDIRECTDRAWSURFACE7 pddsSrcFaceTop = NULL;

		if (dwCubeMapFlagsNew & DDSCAPS2_CUBEMAP_NEGATIVEX)
		{
			if (FAILED(hr = GetTopCubeFace(pddsNew, DDSCAPS2_CUBEMAP_NEGATIVEX, &pddsNewFaceTop)))
				return hr;
			if (FAILED(hr = GetTopCubeFace(*ppddsSrc, DDSCAPS2_CUBEMAP_NEGATIVEX, &pddsSrcFaceTop)))
			{
				// Source doesn't have that cube face.  Leave it blank
			}
			else
			{
				if (FAILED(hr = BltAllLevels(pddsSrcFaceTop, pddsNewFaceTop)))
					return hr;
			}
		}
		if (dwCubeMapFlagsNew & DDSCAPS2_CUBEMAP_POSITIVEX)
		{
			if (FAILED(hr = GetTopCubeFace(pddsNew, DDSCAPS2_CUBEMAP_POSITIVEX, &pddsNewFaceTop)))
				return hr;
			if (FAILED(hr = GetTopCubeFace(*ppddsSrc, DDSCAPS2_CUBEMAP_POSITIVEX, &pddsSrcFaceTop)))
			{
				// Source doesn't have that cube face.  Leave it blank
			}
			else
			{
				if (FAILED(hr = BltAllLevels(pddsSrcFaceTop, pddsNewFaceTop)))
					return hr;
			}
		}
		if (dwCubeMapFlagsNew & DDSCAPS2_CUBEMAP_NEGATIVEY)
		{
			if (FAILED(hr = GetTopCubeFace(pddsNew, DDSCAPS2_CUBEMAP_NEGATIVEY, &pddsNewFaceTop)))
				return hr;
			if (FAILED(hr = GetTopCubeFace(*ppddsSrc, DDSCAPS2_CUBEMAP_NEGATIVEY, &pddsSrcFaceTop)))
			{
				// Source doesn't have that cube face.  Leave it blank
			}
			else
			{
				if (FAILED(hr = BltAllLevels(pddsSrcFaceTop, pddsNewFaceTop)))
					return hr;
			}
		}
		if (dwCubeMapFlagsNew & DDSCAPS2_CUBEMAP_POSITIVEY)
		{
			if (FAILED(hr = GetTopCubeFace(pddsNew, DDSCAPS2_CUBEMAP_POSITIVEY, &pddsNewFaceTop)))
				return hr;
			if (FAILED(hr = GetTopCubeFace(*ppddsSrc, DDSCAPS2_CUBEMAP_POSITIVEY, &pddsSrcFaceTop)))
			{
				// Source doesn't have that cube face.  Leave it blank
			}
			else
			{
				if (FAILED(hr = BltAllLevels(pddsSrcFaceTop, pddsNewFaceTop)))
					return hr;
			}
		}
		if (dwCubeMapFlagsNew & DDSCAPS2_CUBEMAP_NEGATIVEZ)
		{
			if (FAILED(hr = GetTopCubeFace(pddsNew, DDSCAPS2_CUBEMAP_NEGATIVEZ, &pddsNewFaceTop)))
				return hr;
			if (FAILED(hr = GetTopCubeFace(*ppddsSrc, DDSCAPS2_CUBEMAP_NEGATIVEZ, &pddsSrcFaceTop)))
			{
				// Source doesn't have that cube face.  Leave it blank
			}
			else
			{
				if (FAILED(hr = BltAllLevels(pddsSrcFaceTop, pddsNewFaceTop)))
					return hr;
			}
		}
		if (dwCubeMapFlagsNew & DDSCAPS2_CUBEMAP_POSITIVEZ)
		{
			if (FAILED(hr = GetTopCubeFace(pddsNew, DDSCAPS2_CUBEMAP_POSITIVEZ, &pddsNewFaceTop)))
				return hr;
			if (FAILED(hr = GetTopCubeFace(*ppddsSrc, DDSCAPS2_CUBEMAP_POSITIVEZ, &pddsSrcFaceTop)))
			{
				// Source doesn't have that cube face.  Leave it blank
			}
			else
			{
				if (FAILED(hr = BltAllLevels(pddsSrcFaceTop, pddsNewFaceTop)))
					return hr;
			}
		}
	}

	ReleasePpo(ppddsSrc);
	*ppddsSrc = pddsNew;
	m_dwCubeMapFlags = dwCubeMapFlagsNew;
	return S_OK;
}


HRESULT CDxtexDoc::GetTopCubeFace(LPDIRECTDRAWSURFACE7 pdds, DWORD dwCubeMapFlags, LPDIRECTDRAWSURFACE7* ppddsFaceTop)
{
	HRESULT hr;
	DDSURFACEDESC2 ddsd;
	ZeroMemory(&ddsd, sizeof(ddsd));
	ddsd.dwSize = sizeof(ddsd);
	if (FAILED(hr = pdds->GetSurfaceDesc(&ddsd)))
		return hr;
	if ((ddsd.ddsCaps.dwCaps2 & DDSCAPS2_CUBEMAP_ALLFACES) == dwCubeMapFlags)
	{
		*ppddsFaceTop = pdds;
		pdds->AddRef();
		return S_OK;
	}
	DDSCAPS2 ddsCaps;
	ZeroMemory(&ddsCaps, sizeof(ddsCaps));
	ddsCaps.dwCaps2 = dwCubeMapFlags;
	if (FAILED(hr = pdds->GetAttachedSurface(&ddsCaps, ppddsFaceTop)))
		return hr;
	return S_OK;
}


HRESULT CDxtexDoc::BltAllLevels(LPDIRECTDRAWSURFACE7 pddsSrcTop, LPDIRECTDRAWSURFACE7 pddsDestTop)
{
	HRESULT hr;
	LPDIRECTDRAWSURFACE7 pddsSrc;
	LPDIRECTDRAWSURFACE7 pddsDest;
	LPDIRECTDRAWSURFACE7 pddsSrc2;
	LPDIRECTDRAWSURFACE7 pddsDest2;
	DDSCAPS2 ddsCaps;
	
	ZeroMemory(&ddsCaps, sizeof(ddsCaps));
	ddsCaps.dwCaps = DDSCAPS_TEXTURE;
	ddsCaps.dwCaps2 = DDSCAPS2_MIPMAPSUBLEVEL;

	pddsSrc = pddsSrcTop;
	pddsSrc->AddRef();
	pddsDest = pddsDestTop;
	pddsDest->AddRef();
	while (TRUE)
	{
		if (FAILED(hr = pddsDest->Blt(NULL, pddsSrc, NULL, DDBLT_WAIT, NULL)))
		{
			ReleasePpo(&pddsDest);
			ReleasePpo(&pddsSrc);
			return hr;
		}
		
		if (FAILED(hr = pddsSrc->GetAttachedSurface(&ddsCaps, &pddsSrc2)) ||
			FAILED(hr = pddsDest->GetAttachedSurface(&ddsCaps, &pddsDest2)))
		{
			ReleasePpo(&pddsDest);
			ReleasePpo(&pddsSrc);
			break;
		}
		ReleasePpo(&pddsDest);
		pddsDest = pddsDest2;
		ReleasePpo(&pddsSrc);
		pddsSrc = pddsSrc2;
	}

	return S_OK;
}


void CDxtexDoc::OpenCubeFace(DWORD dwCubeMapFlags)
{
	HRESULT hr;
	CString fileName;
	LPDIRECTDRAWSURFACE7 pddsOrigFaceTop = NULL;
	LPDIRECTDRAWSURFACE7 pddsNewFaceTop = NULL;
	LPDIRECTDRAWSURFACE7 pddsLoad = NULL;

	if (FAILED(hr = GetTopCubeFace(m_pddsOrig, dwCubeMapFlags, &pddsOrigFaceTop)))
		return;

	if (m_pddsNew != NULL)
	{
		if (FAILED(hr = GetTopCubeFace(m_pddsNew, dwCubeMapFlags, &pddsNewFaceTop)))
			return;
	}

	if (!PromptForBmp(&fileName))
		return;

	if (FAILED(hr = CreateSurfaceFromBmp(fileName, &pddsLoad)))
	{
		// TODO: report error
		return;
	}

	// Make sure size is right:
	DDSURFACEDESC2 ddsd;
	ZeroMemory(&ddsd, sizeof(ddsd));
	ddsd.dwSize = sizeof(ddsd);
	if (FAILED(hr = pddsOrigFaceTop->GetSurfaceDesc(&ddsd)))
		return;
	DDSURFACEDESC2 ddsdLoad;
	ZeroMemory(&ddsdLoad, sizeof(ddsdLoad));
	ddsdLoad.dwSize = sizeof(ddsdLoad);
	if (FAILED(hr = pddsLoad->GetSurfaceDesc(&ddsdLoad)))
		return;
	if (ddsd.dwWidth != ddsdLoad.dwWidth || ddsd.dwHeight != ddsdLoad.dwHeight)
	{
		// TODO: ask if stretch-to-fit is ok
	}

	// Look for "foo_a.bmp" for alpha channel
	int i = fileName.ReverseFind('.');
	fileName = fileName.Left(i) + "_a.bmp";
	CFileStatus status;
	if (CFile::GetStatus(fileName, status))
	{
		if (FAILED(hr = LoadAlphaIntoSurface(fileName, pddsLoad)))
			return;
	}

	if (FAILED(hr = pddsOrigFaceTop->Blt(NULL, pddsLoad, NULL, DDBLT_WAIT, NULL)))
		return;
	if (m_numMips > 0)
	{
		if (FAILED(hr = GenerateMipMapsFromTop(pddsOrigFaceTop)))
			return;
	}


	if (pddsNewFaceTop != NULL)
	{
		if (FAILED(hr = pddsNewFaceTop->Blt(NULL, pddsLoad, NULL, DDBLT_WAIT, NULL)))
			return;
		if (m_numMips > 0)
		{
			if (FAILED(hr = GenerateMipMapsFromTop(pddsNewFaceTop)))
				return;
		}
	}

	ReleasePpo(&pddsLoad);
	ReleasePpo(&pddsOrigFaceTop);
	ReleasePpo(&pddsNewFaceTop);

	SetModifiedFlag(TRUE);
	UpdateAllViews(NULL, 1);
}


void CDxtexDoc::OpenAlphaCubeFace(DWORD dwCubeMapFlags)
{
	HRESULT hr;
	CString fileName;
	LPDIRECTDRAWSURFACE7 pddsOrigFaceTop = NULL;
	LPDIRECTDRAWSURFACE7 pddsNewFaceTop = NULL;

	// Premultiplied-alpha files don't support this feature:
	DDSURFACEDESC2 ddsd;
	ZeroMemory(&ddsd, sizeof(ddsd));
	ddsd.dwSize = sizeof(ddsd);
	if (SUCCEEDED(hr = m_pddsOrig->GetSurfaceDesc(&ddsd)))
	{
		if ((ddsd.ddpfPixelFormat.dwFlags & DDPF_FOURCC) &&
			(ddsd.ddpfPixelFormat.dwFourCC == FOURCC_DXT2 ||
			 ddsd.ddpfPixelFormat.dwFourCC == FOURCC_DXT4))
		{
			AfxMessageBox(ID_ERROR_PREMULTALPHA);
			return;
		}
	}
	if (m_pddsNew != NULL)
	{
		ZeroMemory(&ddsd, sizeof(ddsd));
		ddsd.dwSize = sizeof(ddsd);
		if (SUCCEEDED(hr = m_pddsNew->GetSurfaceDesc(&ddsd)))
		{
			if ((ddsd.ddpfPixelFormat.dwFlags & DDPF_FOURCC) &&
				(ddsd.ddpfPixelFormat.dwFourCC == FOURCC_DXT2 ||
				 ddsd.ddpfPixelFormat.dwFourCC == FOURCC_DXT4))
			{
				AfxMessageBox(ID_ERROR_PREMULTALPHA);
				return;
			}
		}
	}

	if (FAILED(hr = GetTopCubeFace(m_pddsOrig, dwCubeMapFlags, &pddsOrigFaceTop)))
		return;

	if (m_pddsNew != NULL)
	{
		if (FAILED(hr = GetTopCubeFace(m_pddsNew, dwCubeMapFlags, &pddsNewFaceTop)))
			return;
	}

	if (!PromptForBmp(&fileName))
		return;

	if (FAILED(hr = LoadAlphaIntoSurface(fileName, pddsOrigFaceTop)))
		return;
	if (pddsNewFaceTop != NULL)
	{
		if (FAILED(hr = LoadAlphaIntoSurface(fileName, pddsNewFaceTop)))
			return;
	}

	if (m_numMips > 0)
	{
		if (FAILED(hr = GenerateMipMapsFromTop(pddsOrigFaceTop)))
			return;
		if (pddsNewFaceTop != NULL)
		{
			if (FAILED(hr = GenerateMipMapsFromTop(pddsNewFaceTop)))
				return;
		}
	}

	ReleasePpo(&pddsOrigFaceTop);
	ReleasePpo(&pddsNewFaceTop);

	SetModifiedFlag(TRUE);
	UpdateAllViews(NULL, 1);
}


void CDxtexDoc::OnUpdateFileOpenAlpha(CCmdUI* pCmdUI) 
{
	pCmdUI->Enable(m_dwCubeMapFlags == 0); 
}


void CDxtexDoc::OnUpdateFormatGenerateMipmaps(CCmdUI* pCmdUI) 
{
	pCmdUI->Enable(m_numMips <= 1); 
}


/////////////////////////////////////////////////////////////////////////////
// CCubeMapDlg dialog


CCubeMapDlg::CCubeMapDlg(CWnd* pParent /*=NULL*/)
	: CDialog(CCubeMapDlg::IDD, pParent)
{
	//{{AFX_DATA_INIT(CCubeMapDlg)
	m_bNegX = FALSE;
	m_bNegY = FALSE;
	m_bNegZ = FALSE;
	m_bPosX = FALSE;
	m_bPosY = FALSE;
	m_bPosZ = FALSE;
	//}}AFX_DATA_INIT
}


void CCubeMapDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CCubeMapDlg)
	DDX_Check(pDX, IDC_NEGX, m_bNegX);
	DDX_Check(pDX, IDC_NEGY, m_bNegY);
	DDX_Check(pDX, IDC_NEGZ, m_bNegZ);
	DDX_Check(pDX, IDC_POSX, m_bPosX);
	DDX_Check(pDX, IDC_POSY, m_bPosY);
	DDX_Check(pDX, IDC_POSZ, m_bPosZ);
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(CCubeMapDlg, CDialog)
	//{{AFX_MSG_MAP(CCubeMapDlg)
		// NOTE: the ClassWizard will add message map macros here
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()


