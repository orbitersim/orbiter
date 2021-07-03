// dxtexView.cpp : implementation of the CDxtexView class
//

#include "stdafx.h"
#include "dxtex.h"
#include "dxtexDoc.h"
#include "dxtexView.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

IMPLEMENT_DYNCREATE(CDxtexView, CScrollView)

BEGIN_MESSAGE_MAP(CDxtexView, CScrollView)
	//{{AFX_MSG_MAP(CDxtexView)
	ON_WM_LBUTTONUP()
	ON_COMMAND(ID_FILE_OPENFACE, OnFileOpenFace)
	ON_COMMAND(ID_FILE_OPENALPHAFACE, OnFileOpenAlphaFace)
	ON_COMMAND(ID_VIEW_ORIGINAL, OnViewOriginal)
	ON_COMMAND(ID_VIEW_COMPRESSED, OnViewCompressed)
	ON_COMMAND(ID_VIEW_SMALLERMIPLEVEL, OnViewSmallerMipLevel)
	ON_COMMAND(ID_VIEW_LARGERMIPLEVEL, OnViewLargerMipLevel)
	ON_COMMAND(ID_VIEW_ALPHACHANNEL, OnViewAlphaChannel)
	ON_COMMAND(ID_VIEW_ZOOMIN, OnViewZoomIn)
	ON_COMMAND(ID_VIEW_ZOOMOUT, OnViewZoomOut)
	ON_COMMAND(ID_VIEW_CHANGEBACKGROUNDCOLOR, OnViewChangeBackgroundColor)
	ON_COMMAND(ID_FILE_OPENSUBSURFACE, OnFileOpenSubsurface)
	ON_COMMAND(ID_FILE_OPENALPHASUBSURFACE, OnFileOpenAlphaSubsurface)
	ON_COMMAND(ID_VIEW_NEGX, OnViewNegX)
	ON_COMMAND(ID_VIEW_POSX, OnViewPosX)
	ON_COMMAND(ID_VIEW_NEGY, OnViewNegY)
	ON_COMMAND(ID_VIEW_POSY, OnViewPosY)
	ON_COMMAND(ID_VIEW_NEGZ, OnViewNegZ)
	ON_COMMAND(ID_VIEW_POSZ, OnViewPosZ)
	ON_UPDATE_COMMAND_UI(ID_VIEW_ORIGINAL, OnUpdateViewOriginal)
	ON_UPDATE_COMMAND_UI(ID_VIEW_COMPRESSED, OnUpdateViewCompressed)
	ON_UPDATE_COMMAND_UI(ID_VIEW_ALPHACHANNEL, OnUpdateViewAlphaChannel)
	ON_UPDATE_COMMAND_UI(ID_VIEW_LARGERMIPLEVEL, OnUpdateViewLargerMipLevel)
	ON_UPDATE_COMMAND_UI(ID_VIEW_SMALLERMIPLEVEL, OnUpdateViewSmallerMipLevel)
	ON_UPDATE_COMMAND_UI(ID_VIEW_ZOOMIN, OnUpdateViewZoomIn)
	ON_UPDATE_COMMAND_UI(ID_VIEW_ZOOMOUT, OnUpdateViewZoomOut)
	ON_UPDATE_COMMAND_UI(ID_FILE_OPENSUBSURFACE, OnUpdateFileOpenSubsurface)
	ON_UPDATE_COMMAND_UI(ID_FILE_OPENALPHASUBSURFACE, OnUpdateFileOpenAlphaSubsurface)
	ON_UPDATE_COMMAND_UI(ID_VIEW_NEGX, OnUpdateViewNegX)
	ON_UPDATE_COMMAND_UI(ID_VIEW_POSX, OnUpdateViewPosX)
	ON_UPDATE_COMMAND_UI(ID_VIEW_NEGY, OnUpdateViewNegY)
	ON_UPDATE_COMMAND_UI(ID_VIEW_POSY, OnUpdateViewPosY)
	ON_UPDATE_COMMAND_UI(ID_VIEW_NEGZ, OnUpdateViewNegZ)
	ON_UPDATE_COMMAND_UI(ID_VIEW_POSZ, OnUpdateViewPosZ)
	ON_UPDATE_COMMAND_UI(ID_FILE_OPENFACE, OnUpdateFileOpenFace)
	ON_UPDATE_COMMAND_UI(ID_FILE_OPENALPHAFACE, OnUpdateFileOpenAlphaFace)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()


CDxtexView::CDxtexView()
{
	m_pddsCur = NULL;
	m_pddsBack = NULL;
	m_pd3ddev = NULL;
	m_lwMipCur = 0;
	m_dwCubeMapCur = 0;
	m_fZoom = 1.0f;
	m_bViewOrig = TRUE;
	m_bViewAlpha = FALSE;
}


CDxtexView::~CDxtexView()
{
	ReleasePpo(&m_pddsCur);
	ReleasePpo(&m_pd3ddev);
	ReleasePpo(&m_pddsBack);
}


// Note: repaints don't require re-rendering, just recopy from back buffer to view's DC
void CDxtexView::OnDraw(CDC* pDC)
{
	CDxtexDoc* pDoc = GetDocument();
	ASSERT_VALID(pDoc);

	HDC hdc;
	if (SUCCEEDED(m_pddsBack->GetDC(&hdc)))
	{
		pDC->StretchBlt(0, 0, m_rcDest.Width(), m_rcDest.Height(), 
			CDC::FromHandle(hdc), 0, 0, m_rcSrc.Width(), m_rcSrc.Height(), SRCCOPY);
		m_pddsBack->ReleaseDC(hdc);
	}
}


#ifdef _DEBUG
void CDxtexView::AssertValid() const
{
	CScrollView::AssertValid();
}


void CDxtexView::Dump(CDumpContext& dc) const
{
	CScrollView::Dump(dc);
}


CDxtexDoc* CDxtexView::GetDocument() // non-debug version is inline
{
	ASSERT(m_pDocument->IsKindOf(RUNTIME_CLASS(CDxtexDoc)));
	return (CDxtexDoc*)m_pDocument;
}
#endif //_DEBUG


void CDxtexView::OnLButtonUp(UINT nFlags, CPoint point) 
{
	// Button click means toggle compressed / uncompressed view
	if (m_bViewOrig)
		OnViewCompressed();
	else
		OnViewOriginal();

	CScrollView::OnLButtonUp(nFlags, point);
}


void CDxtexView::OnInitialUpdate() 
{
	SetClassLong(GetSafeHwnd(), GCL_HBRBACKGROUND, (LONG)CreateSolidBrush(RGB(100, 100, 120)));

	BuildViewSurface(m_bViewOrig, 0, m_lwMipCur, m_bViewAlpha);
	UpdateDevice();
	RenderScene();

	SetScrollSizes(MM_TEXT, CSize(m_rcDest.Width(), m_rcDest.Height()));
	ResizeParentToFit();

	CScrollView::OnInitialUpdate();

	m_bTitleModsChanged = TRUE; // force title bar update
}


VOID CDxtexView::GetImageInfo(CString& strInfo)
{
	LPDIRECTDRAWSURFACE7 pdds;
	DDSURFACEDESC2 ddsd;
	DDPIXELFORMAT* pddpf;
	TCHAR szFormat[20];
	TCHAR sz[100];
	DWORD dwBytes = 0;
	DWORD dwWidth;
	DWORD dwHeight;
	DWORD dwTopCubeFace;
	DWORD dwCubeMapFlags = GetDocument()->DwCubeMapFlags();

	if (m_bViewOrig)
		pdds = GetDocument()->PddsOrig();
	else
		pdds = GetDocument()->PddsNew();
	if (pdds == NULL)
		return;

	ZeroMemory(&ddsd, sizeof(ddsd));
	ddsd.dwSize = sizeof(ddsd);
	if (FAILED(pdds->GetSurfaceDesc(&ddsd)))
		return;
	dwWidth = ddsd.dwWidth;
	dwHeight = ddsd.dwHeight;
	dwTopCubeFace = (ddsd.ddsCaps.dwCaps2 & DDSCAPS2_CUBEMAP_ALLFACES);
	pddpf = &ddsd.ddpfPixelFormat;
	if (pddpf->dwFlags & DDPF_FOURCC)
	{
		wsprintf(szFormat, "%c%c%c%c", 
			LOBYTE(LOWORD(pddpf->dwFourCC)),
			HIBYTE(LOWORD(pddpf->dwFourCC)),
			LOBYTE(HIWORD(pddpf->dwFourCC)),
			HIBYTE(HIWORD(pddpf->dwFourCC)));
	}
	else if (pddpf->dwFlags & DDPF_RGB)
	{
		wsprintf(szFormat, "ARGB-%d", ddsd.ddpfPixelFormat.dwRGBBitCount);
	}
	else
	{
		lstrcpy(szFormat, "");
	}

	// Count bytes in main surface chain
	dwBytes += NumBytesInSurfaces(pdds);

	// Count bytes in other cubemap faces, if any
	if (GetDocument()->DwCubeMapFlags() != dwTopCubeFace)
	{
		LPDIRECTDRAWSURFACE7 pddsFaceTop = NULL;

		if ((dwCubeMapFlags & DDSCAPS2_CUBEMAP_POSITIVEX) && !(dwTopCubeFace & DDSCAPS2_CUBEMAP_POSITIVEX))
		{
			if (SUCCEEDED(GetDocument()->GetTopCubeFace(pdds, DDSCAPS2_CUBEMAP_POSITIVEX, &pddsFaceTop)))
			{
				dwBytes += NumBytesInSurfaces(pddsFaceTop);
				ReleasePpo(&pddsFaceTop);
			}
		}
		if ((dwCubeMapFlags & DDSCAPS2_CUBEMAP_NEGATIVEX) && !(dwTopCubeFace & DDSCAPS2_CUBEMAP_NEGATIVEX))
		{
			if (SUCCEEDED(GetDocument()->GetTopCubeFace(pdds, DDSCAPS2_CUBEMAP_NEGATIVEX, &pddsFaceTop)))
			{
				dwBytes += NumBytesInSurfaces(pddsFaceTop);
				ReleasePpo(&pddsFaceTop);
			}
		}
		if ((dwCubeMapFlags & DDSCAPS2_CUBEMAP_POSITIVEY) && !(dwTopCubeFace & DDSCAPS2_CUBEMAP_POSITIVEY))
		{
			if (SUCCEEDED(GetDocument()->GetTopCubeFace(pdds, DDSCAPS2_CUBEMAP_POSITIVEY, &pddsFaceTop)))
			{
				dwBytes += NumBytesInSurfaces(pddsFaceTop);
				ReleasePpo(&pddsFaceTop);
			}
		}
		if ((dwCubeMapFlags & DDSCAPS2_CUBEMAP_NEGATIVEY) && !(dwTopCubeFace & DDSCAPS2_CUBEMAP_NEGATIVEY))
		{
			if (SUCCEEDED(GetDocument()->GetTopCubeFace(pdds, DDSCAPS2_CUBEMAP_NEGATIVEY, &pddsFaceTop)))
			{
				dwBytes += NumBytesInSurfaces(pddsFaceTop);
				ReleasePpo(&pddsFaceTop);
			}
		}
		if ((dwCubeMapFlags & DDSCAPS2_CUBEMAP_POSITIVEZ) && !(dwTopCubeFace & DDSCAPS2_CUBEMAP_POSITIVEZ))
		{
			if (SUCCEEDED(GetDocument()->GetTopCubeFace(pdds, DDSCAPS2_CUBEMAP_POSITIVEZ, &pddsFaceTop)))
			{
				dwBytes += NumBytesInSurfaces(pddsFaceTop);
				ReleasePpo(&pddsFaceTop);
			}
		}
		if ((dwCubeMapFlags & DDSCAPS2_CUBEMAP_NEGATIVEZ) && !(dwTopCubeFace & DDSCAPS2_CUBEMAP_NEGATIVEZ))
		{
			if (SUCCEEDED(GetDocument()->GetTopCubeFace(pdds, DDSCAPS2_CUBEMAP_NEGATIVEZ, &pddsFaceTop)))
			{
				dwBytes += NumBytesInSurfaces(pddsFaceTop);
				ReleasePpo(&pddsFaceTop);
			}
		}
	}

	wsprintf(sz, "%d x %d, %s, %d bytes", dwWidth, dwHeight, szFormat, dwBytes);
	strInfo = sz;
}


DWORD CDxtexView::NumBytesInSurfaces(LPDIRECTDRAWSURFACE7 pddsTopMip)
{
	DWORD dwBytes = 0;
	LPDIRECTDRAWSURFACE7 pdds = NULL;
	LPDIRECTDRAWSURFACE7 pddsNext = NULL;
	DDSCAPS2 ddsCaps;
	DDSURFACEDESC2 ddsd;

	ZeroMemory(&ddsd, sizeof(ddsd));
	ddsd.dwSize = sizeof(ddsd);
	if (FAILED(pddsTopMip->GetSurfaceDesc(&ddsd)))
		return 0;

	ZeroMemory(&ddsCaps, sizeof(ddsCaps));
	ddsCaps.dwCaps = DDSCAPS_TEXTURE;
	ddsCaps.dwCaps2 = DDSCAPS2_MIPMAPSUBLEVEL;

	pdds = pddsTopMip;
	pdds->AddRef();
	while (TRUE)
	{
		if (ddsd.dwFlags & DDSD_LINEARSIZE)
			dwBytes += ddsd.dwLinearSize;
		else
			dwBytes += ddsd.dwWidth * ddsd.dwHeight * (ddsd.ddpfPixelFormat.dwRGBBitCount / 8);
		if (FAILED(pdds->GetAttachedSurface(&ddsCaps, &pddsNext)))
		{
			ReleasePpo(&pdds);
			break;
		}
		ReleasePpo(&pdds);
		pdds = pddsNext;
		pddsNext = NULL;
		pdds->GetSurfaceDesc(&ddsd);
	}
	return dwBytes;
}


HRESULT CDxtexView::UpdateDevice(VOID)
{
	HRESULT hr;
	DWORD dwWidth;
	DWORD dwHeight;
	DDSURFACEDESC2 ddsd;

	ReleasePpo(&m_pd3ddev);
	ReleasePpo(&m_pddsBack);

	dwWidth = m_rcSrc.Width();
	dwHeight = m_rcSrc.Height();
	if (dwWidth == 0 || dwHeight == 0)
		return S_OK;

	ZeroMemory(&ddsd, sizeof(ddsd));
	ddsd.dwSize = sizeof(ddsd);
	ddsd.dwFlags = DDSD_WIDTH | DDSD_HEIGHT | DDSD_CAPS | DDSD_PIXELFORMAT;
	ddsd.dwWidth = dwWidth;
	ddsd.dwHeight = dwHeight;
	ddsd.ddsCaps.dwCaps = DDSCAPS_SYSTEMMEMORY | DDSCAPS_OFFSCREENPLAIN | DDSCAPS_3DDEVICE;
	ddsd.ddpfPixelFormat.dwSize = sizeof(DDPIXELFORMAT);
	ddsd.ddpfPixelFormat.dwFlags = DDPF_RGB;
	ddsd.ddpfPixelFormat.dwRGBBitCount = 32;
	ddsd.ddpfPixelFormat.dwRBitMask = 0x00ff0000;
	ddsd.ddpfPixelFormat.dwGBitMask = 0x0000ff00;
	ddsd.ddpfPixelFormat.dwBBitMask = 0x000000ff;

	if (FAILED(hr = PDxtexApp()->Pdd()->CreateSurface(&ddsd, &m_pddsBack, NULL)))
		return hr;

	if (FAILED(hr = PDxtexApp()->Pd3d()->CreateDevice(IID_IDirect3DRefDevice, m_pddsBack, &m_pd3ddev)))
		return hr;

	D3DVIEWPORT7 viewdata;
	viewdata.dwX = 0;
	viewdata.dwY = 0;
	viewdata.dwWidth = dwWidth;
	viewdata.dwHeight = dwHeight;
	viewdata.dvMinZ = 0.0f;
	viewdata.dvMaxZ = 1.0f;

	if (FAILED(hr = m_pd3ddev->SetViewport(&viewdata)))
		return hr;

	COLORREF crBkgd;
	crBkgd = PDxtexApp()->GetProfileInt("Settings", "Background Color", RGB(0, 255, 255));
	m_dwClearColor = RGBA_MAKE(GetRValue(crBkgd), GetGValue(crBkgd), GetBValue(crBkgd), 255);

	return S_OK;
}


HRESULT CDxtexView::RenderScene(VOID)
{
	if (m_pd3ddev == NULL)
		return S_OK;
	
	CWaitCursor waitCursor;
	HRESULT hr;
	D3DCOLOR col = (D3DCOLOR)D3DRGBA(0.0f, 0.0f, 0.0f, 1.0f);
	D3DCOLOR col2 = (D3DCOLOR)D3DRGBA(1.0f, 1.0f, 1.0f, 1.0f);

	FLOAT fWidth = (FLOAT)m_rcSrc.Width();
	FLOAT fHeight = (FLOAT)m_rcSrc.Height();

	D3DTLVERTEX vertexArray[4] = 
	{
		D3DTLVERTEX(D3DVECTOR(  0.0f,    0.0f, 0.9999f), 2.0f, col2, col, 0.0f, 0.0f),
		D3DTLVERTEX(D3DVECTOR(fWidth,    0.0f, 0.9999f), 2.0f, col2, col, 1.0f, 0.0f),
		D3DTLVERTEX(D3DVECTOR(  0.0f, fHeight, 0.9999f), 2.0f, col2, col, 0.0f, 1.0f),
		D3DTLVERTEX(D3DVECTOR(fWidth, fHeight, 0.9999f), 2.0f, col2, col, 1.0f, 1.0f),
	};

	RECT rcBack;
	SetRect(&rcBack, 0, 0, m_rcSrc.Width(), m_rcSrc.Height());
	if (FAILED(hr = m_pd3ddev->Clear(1, (D3DRECT*)&rcBack, D3DCLEAR_TARGET, m_dwClearColor, 1.0f, 0)))
		return hr;

	// If the texture uses premultiplied alpha, the source blend should be D3DBLEND_ONE
	// since RGB is already at the level we want.  With nonpremultiplied alpha, the
	// source blend should be D3DBLEND_SRCALPHA.
	DDSURFACEDESC2 ddsd;
	ZeroMemory(&ddsd, sizeof(ddsd));
	ddsd.dwSize = sizeof(ddsd);
	if (FAILED(m_pddsCur->GetSurfaceDesc(&ddsd)))
		return hr;
	if (!m_bViewAlpha && (ddsd.ddpfPixelFormat.dwFlags & DDPF_FOURCC) != 0 &&
		(ddsd.ddpfPixelFormat.dwFourCC == FOURCC_DXT2 ||
		 ddsd.ddpfPixelFormat.dwFourCC == FOURCC_DXT4))
	{
		if (FAILED(hr = m_pd3ddev->SetRenderState(D3DRENDERSTATE_SRCBLEND, D3DBLEND_ONE)))
			return hr;
	}
	else
	{
		if (FAILED(hr = m_pd3ddev->SetRenderState(D3DRENDERSTATE_SRCBLEND, D3DBLEND_SRCALPHA)))
			return hr;
	}

	if (FAILED(hr = m_pd3ddev->SetRenderState(D3DRENDERSTATE_ALPHABLENDENABLE, TRUE)))
		return hr;
	if (FAILED(hr = m_pd3ddev->SetRenderState(D3DRENDERSTATE_DESTBLEND, D3DBLEND_INVSRCALPHA)))
		return hr;

	if (FAILED(hr = m_pd3ddev->SetTexture(0, m_pddsCur)))
		return hr;

	if (FAILED(hr = m_pd3ddev->BeginScene()))
		return hr;

	if (FAILED(hr = m_pd3ddev->DrawPrimitive(D3DPT_TRIANGLESTRIP,
		D3DFVF_TLVERTEX, vertexArray, 4, 0)))
	{
		m_pd3ddev->EndScene();
		return hr;
	}
	if (FAILED(hr = m_pd3ddev->EndScene()))
		return hr;

	m_pd3ddev->SetTexture(0, NULL);

	return S_OK;
}


void CDxtexView::OnViewOriginal() 
{
	if (GetDocument()->PddsOrig() == NULL)
		return;
	BuildViewSurface(TRUE, m_dwCubeMapCur, m_lwMipCur, m_bViewAlpha);
	RenderScene();
	m_bTitleModsChanged = TRUE; // force title bar update
	InvalidateRect(&m_rcDest, FALSE); // force redraw of this view
}


void CDxtexView::OnViewCompressed() 
{
	if (GetDocument()->PddsNew() == NULL)
		return;
	BuildViewSurface(FALSE, m_dwCubeMapCur, m_lwMipCur, m_bViewAlpha);
	RenderScene();
	m_bTitleModsChanged = TRUE; // force title bar update
	InvalidateRect(&m_rcDest, FALSE); // force redraw of this view
}


void CDxtexView::OnUpdateViewOriginal(CCmdUI* pCmdUI) 
{
	if (GetDocument()->PddsOrig() == NULL)
	{
		pCmdUI->Enable(FALSE);
		pCmdUI->SetCheck(0);
	}
	else
	{
		pCmdUI->Enable(TRUE);
		pCmdUI->SetCheck(m_bViewOrig);
	}
}


void CDxtexView::OnUpdateViewCompressed(CCmdUI* pCmdUI) 
{
	if (GetDocument()->PddsNew() == NULL)
	{
		pCmdUI->Enable(FALSE);
		pCmdUI->SetCheck(0);
	}
	else
	{
		pCmdUI->Enable(TRUE);
		pCmdUI->SetCheck(!m_bViewOrig);
	}
}


void CDxtexView::OnUpdate(CView* pSender, LPARAM lHint, CObject* pHint) 
{
	m_bTitleModsChanged = TRUE; // force title bar update
	if (lHint == 1)
	{
		BuildViewSurface(m_bViewOrig, m_dwCubeMapCur, m_lwMipCur, m_bViewAlpha);
		RenderScene();
	}
	else if (lHint == 2)
	{
		UpdateDevice();
		RenderScene();
	}
	else if (lHint == 3)
	{
		RenderScene();
	}

	CScrollView::OnUpdate(pSender, lHint, pHint);
}


DWORD CDxtexView::DwCubeMapCur(LPDIRECTDRAWSURFACE7 pdds)
{
	HRESULT hr;
	DDSURFACEDESC2 ddsd;
	ZeroMemory(&ddsd, sizeof(ddsd));
	ddsd.dwSize = sizeof(ddsd);

	if (FAILED(hr = pdds->GetSurfaceDesc(&ddsd)))
		return 0;
	return (ddsd.ddsCaps.dwCaps2 & DDSCAPS2_CUBEMAP_ALLFACES);
}


void CDxtexView::OnViewSmallerMipLevel() 
{
	m_lwMipCur++;
	BuildViewSurface(m_bViewOrig, m_dwCubeMapCur, m_lwMipCur, m_bViewAlpha);
	UpdateDevice();
	RenderScene();
	m_bTitleModsChanged = TRUE; // force title bar update
	SetScrollSizes(MM_TEXT, CSize(m_rcDest.Width(), m_rcDest.Height()));
	Invalidate(); // force redraw of this view
}


void CDxtexView::OnViewLargerMipLevel() 
{
	m_lwMipCur--;
	BuildViewSurface(m_bViewOrig, m_dwCubeMapCur, m_lwMipCur, m_bViewAlpha);
	UpdateDevice();
	RenderScene();
	m_bTitleModsChanged = TRUE; // force title bar update
	SetScrollSizes(MM_TEXT, CSize(m_rcDest.Width(), m_rcDest.Height()));
	Invalidate(); // force redraw of this view
}


void CDxtexView::OnViewAlphaChannel(VOID) 
{
	BuildViewSurface(m_bViewOrig, m_dwCubeMapCur, m_lwMipCur, !m_bViewAlpha);
	RenderScene();
	Invalidate(); // force redraw of this view
	m_bTitleModsChanged = TRUE; // force title bar update
}


void CDxtexView::OnUpdateViewAlphaChannel(CCmdUI* pCmdUI) 
{
	pCmdUI->SetCheck(m_bViewAlpha);
}


HRESULT CDxtexView::GenerateAlphaImage(LPDIRECTDRAWSURFACE7 pddsSrc, LPDIRECTDRAWSURFACE7* ppddsDest)
{
	HRESULT hr;
	DDSURFACEDESC2 ddsd;
	DDSURFACEDESC2 ddsd2;
	DWORD xp;
	DWORD yp;
	BYTE* pbRow;
	BYTE* pbRow2;
	DWORD* pdwPixel;
	DWORD* pdwPixel2;
	DWORD dwAlpha;
	LPDIRECTDRAWSURFACE7 pddsTemp = NULL;
		
	ddsd.dwSize = sizeof(ddsd);
	pddsSrc->GetSurfaceDesc(&ddsd);
	ddsd.dwFlags = DDSD_CAPS | DDSD_WIDTH | DDSD_HEIGHT | DDSD_PIXELFORMAT;
	ddsd.ddsCaps.dwCaps = DDSCAPS_TEXTURE | DDSCAPS_SYSTEMMEMORY;
	ddsd.ddpfPixelFormat.dwSize = sizeof(DDPIXELFORMAT);
	ddsd.ddpfPixelFormat.dwFlags = DDPF_RGB | DDPF_ALPHAPIXELS;
	ddsd.ddpfPixelFormat.dwRGBBitCount = 32;
	ddsd.ddpfPixelFormat.dwRBitMask = 0x00ff0000;
	ddsd.ddpfPixelFormat.dwGBitMask = 0x0000ff00;
	ddsd.ddpfPixelFormat.dwBBitMask = 0x000000ff;
	ddsd.ddpfPixelFormat.dwRGBAlphaBitMask = 0xff000000;
	if (FAILED(hr = PDxtexApp()->Pdd()->CreateSurface(&ddsd, ppddsDest, NULL)))
		return hr;

	// If the source image has premultiplied alpha, we have to use a 
	// temporary, intermediate RGBA image with premultiplied alpha.
	// The reason: premult images can only be blitted to other premult images,
	// but premult RGB images can not be rendered directly by the refrast.
	if (ddsd.ddpfPixelFormat.dwFourCC == FOURCC_DXT2 ||
		ddsd.ddpfPixelFormat.dwFourCC == FOURCC_DXT4)
	{
		ddsd.ddsCaps.dwCaps = DDSCAPS_SYSTEMMEMORY | DDSCAPS_OFFSCREENPLAIN;
		ddsd.ddsCaps.dwCaps2 = 0;
		ddsd.ddpfPixelFormat.dwFourCC = 0;
		ddsd.ddpfPixelFormat.dwFlags = DDPF_RGB | DDPF_ALPHAPIXELS | DDPF_ALPHAPREMULT;
		if (FAILED(hr = PDxtexApp()->Pdd()->CreateSurface(&ddsd, &pddsTemp, NULL)))
			return hr;
	}

	if (pddsTemp == NULL)
	{
		if (FAILED(hr = (*ppddsDest)->Blt(NULL, pddsSrc, NULL, DDBLT_WAIT, NULL)))
		{
			ReleasePpo(ppddsDest);
			return hr;
		}
	}
	else
	{
		if (FAILED(hr = pddsTemp->Blt(NULL, pddsSrc, NULL, DDBLT_WAIT, NULL)))
		{
			ReleasePpo(ppddsDest);
			ReleasePpo(&pddsTemp);
			return hr;
		}
	}

	if (FAILED(hr = (*ppddsDest)->Lock(NULL, &ddsd, DDLOCK_WAIT, NULL)))
	{
		ReleasePpo(ppddsDest);
		ReleasePpo(&pddsTemp);
		return hr;
	}
	pbRow = (BYTE*)ddsd.lpSurface;
	pdwPixel = (DWORD*)pbRow;
	if (pddsTemp != NULL)
	{
		ZeroMemory(&ddsd2, sizeof(ddsd2));
		ddsd2.dwSize = sizeof(ddsd2);
		if (FAILED(hr = pddsTemp->Lock(NULL, &ddsd2, DDLOCK_WAIT, NULL)))
		{
			(*ppddsDest)->Unlock(NULL);
			ReleasePpo(ppddsDest);
			ReleasePpo(&pddsTemp);
			return hr;
		}
		pbRow2 = (BYTE*)ddsd2.lpSurface;
		pdwPixel2 = (DWORD*)pbRow2;
	}
	for (yp = 0; yp < ddsd.dwHeight; yp++)
	{
		for (xp = 0; xp < ddsd.dwWidth; xp++)
		{
			if (pddsTemp == NULL)
				dwAlpha = *pdwPixel & 0xff000000;
			else
				dwAlpha = *pdwPixel2 & 0xff000000;
			*pdwPixel = (0xff000000) | (dwAlpha >> 8) | (dwAlpha >> 16) | (dwAlpha >> 24);
			pdwPixel++;
			pdwPixel2++;
		}
		pbRow += ddsd.lPitch;
		pdwPixel = (DWORD*)pbRow;
		if (pddsTemp != NULL)
		{
			pbRow2 += ddsd2.lPitch;
			pdwPixel2 = (DWORD*)pbRow2;
		}
	}
	(*ppddsDest)->Unlock(NULL);
	if (pddsTemp != NULL)
	{
		pddsTemp->Unlock(NULL);
		ReleasePpo(&pddsTemp);
	}
	return S_OK;
}


void CDxtexView::OnUpdateViewLargerMipLevel(CCmdUI* pCmdUI) 
{
	if (m_lwMipCur > 0)
		pCmdUI->Enable(TRUE);
	else
		pCmdUI->Enable(FALSE);
}


void CDxtexView::OnUpdateViewSmallerMipLevel(CCmdUI* pCmdUI) 
{
	if (m_lwMipCur < (LONG)GetDocument()->NumMips() - 1)
		pCmdUI->Enable(TRUE);
	else
		pCmdUI->Enable(FALSE);
}


void CDxtexView::OnViewZoomIn() 
{
	if (m_fZoom < 8.0f)
		m_fZoom *= 2.0f;
	m_rcDest.right = (LONG)(m_rcSrc.right * m_fZoom);
	m_rcDest.bottom = (LONG)(m_rcSrc.bottom * m_fZoom);
	SetScrollSizes(MM_TEXT, CSize(m_rcDest.Width(), m_rcDest.Height()));
	m_bTitleModsChanged = TRUE; // force title bar update
	Invalidate(); // force redraw of this view
}


void CDxtexView::OnViewZoomOut() 
{
	if (m_fZoom > 0.125f)
		m_fZoom /= 2.0f;
	m_rcDest.right = (LONG)(m_rcSrc.right * m_fZoom);
	m_rcDest.bottom = (LONG)(m_rcSrc.bottom * m_fZoom);
	SetScrollSizes(MM_TEXT, CSize(m_rcDest.Width(), m_rcDest.Height()));
	m_bTitleModsChanged = TRUE; // force title bar update
	Invalidate(); // force redraw of this view
}


void CDxtexView::OnUpdateViewZoomIn(CCmdUI* pCmdUI) 
{
	pCmdUI->Enable(m_fZoom < 8.0f);
}


void CDxtexView::OnUpdateViewZoomOut(CCmdUI* pCmdUI) 
{
	pCmdUI->Enable(m_fZoom > 0.125f);
}


CString CDxtexView::GetStrTitleMods(VOID)
{
	CString strTitleMods;
	strTitleMods = "(";

	// Append alpha, if in alpha mode
	if (m_bViewAlpha)
		strTitleMods += "Alpha, ";

	// Show RGB or FourCC
	LPDIRECTDRAWSURFACE7 pdds;
	CString strFormat;
	DDSURFACEDESC2 ddsd;
	DDPIXELFORMAT* pddpf;

	if (m_bViewOrig)
		pdds = GetDocument()->PddsOrig();
	else
		pdds = GetDocument()->PddsNew();

	ZeroMemory(&ddsd, sizeof(ddsd));
	ddsd.dwSize = sizeof(ddsd);
	ddsd.dwFlags = DDSD_WIDTH | DDSD_HEIGHT | DDSD_PIXELFORMAT;
	if (SUCCEEDED(pdds->GetSurfaceDesc(&ddsd)))
	{
		pddpf = &ddsd.ddpfPixelFormat;
		if (pddpf->dwFlags & DDPF_FOURCC)
		{
			strFormat.Format("%c%c%c%c", 
				LOBYTE(LOWORD(pddpf->dwFourCC)),
				HIBYTE(LOWORD(pddpf->dwFourCC)),
				LOBYTE(HIWORD(pddpf->dwFourCC)),
				HIBYTE(HIWORD(pddpf->dwFourCC)));
		}
		else if (pddpf->dwFlags & DDPF_RGB)
		{
			strFormat.Format("ARGB-%d", ddsd.ddpfPixelFormat.dwRGBBitCount);
		}
		strTitleMods += strFormat + ", ";
	}

	// Append cube map info, if a cube map
	switch (m_dwCubeMapCur)
	{
	case DDSCAPS2_CUBEMAP_NEGATIVEX:
		strTitleMods += "Negative X, ";
		break;
	case DDSCAPS2_CUBEMAP_POSITIVEX:
		strTitleMods += "Positive X, ";
		break;
	case DDSCAPS2_CUBEMAP_NEGATIVEY:
		strTitleMods += "Negative Y, ";
		break;
	case DDSCAPS2_CUBEMAP_POSITIVEY:
		strTitleMods += "Positive Y, ";
		break;
	case DDSCAPS2_CUBEMAP_NEGATIVEZ:
		strTitleMods += "Negative Z, ";
		break;
	case DDSCAPS2_CUBEMAP_POSITIVEZ:
		strTitleMods += "Positive Z, ";
		break;
	}

	// Append mip info, if multiple mip levels
	DWORD dwNumMips = GetDocument()->NumMips();
	if (dwNumMips > 1)
	{
		CString strMipInfo;
		strMipInfo.Format("Mip %d of %d, ", m_lwMipCur + 1, dwNumMips);
		strTitleMods += strMipInfo;
	}

	// Append view magnification
	CString strView;
	strView.Format("%d", (LONG)(100 * m_fZoom));
	strTitleMods += strView + "%";

	strTitleMods += ")";
	return strTitleMods;
}


void CDxtexView::OnViewChangeBackgroundColor() 
{
	CHOOSECOLOR cc;
	COLORREF crArray[16];

	ZeroMemory(&cc, sizeof(cc));
	cc.lStructSize = sizeof(cc);
	cc.hwndOwner = GetSafeHwnd();
	cc.rgbResult = PDxtexApp()->GetProfileInt("Settings", "Background Color", RGB(0, 255, 255));
	cc.lpCustColors = crArray;
	cc.Flags = CC_RGBINIT | CC_ANYCOLOR | CC_FULLOPEN;

	if (ChooseColor(&cc))
	{
		PDxtexApp()->WriteProfileInt("Settings", "Background Color", cc.rgbResult);

		// Update all views of all documents of our one doc template
		POSITION posTemp = PDxtexApp()->GetFirstDocTemplatePosition();
		CDocTemplate* pDocTemplate = PDxtexApp()->GetNextDocTemplate(posTemp);
		CDocument* pdoc;
		POSITION pos = pDocTemplate->GetFirstDocPosition();
		while (pos != NULL)
		{
			pdoc = pDocTemplate->GetNextDoc(pos);
			pdoc->UpdateAllViews(NULL, 2);
		}
	}
}


void CDxtexView::OnFileOpenSubsurface() 
{
	GetDocument()->OpenSubsurface(m_dwCubeMapCur, m_lwMipCur);
}


void CDxtexView::OnUpdateFileOpenSubsurface(CCmdUI* pCmdUI) 
{
	if (GetDocument()->NumMips() <= 1)
	{
		pCmdUI->Enable(FALSE);
	}
	else
	{
		pCmdUI->Enable(TRUE);
	}
}


void CDxtexView::OnFileOpenAlphaSubsurface() 
{
	GetDocument()->OpenAlphaSubsurface(m_dwCubeMapCur, m_lwMipCur);
}


void CDxtexView::OnUpdateFileOpenAlphaSubsurface(CCmdUI* pCmdUI) 
{
	if (GetDocument()->NumMips() <= 1)
	{
		pCmdUI->Enable(FALSE);
	}
	else
	{
		pCmdUI->Enable(TRUE);
	}
}


HRESULT CDxtexView::BuildViewSurface(BOOL bOrig, DWORD dwCubeMapFace, LONG lwMip, BOOL bViewAlpha)
{
	HRESULT hr;
	LPDIRECTDRAWSURFACE7 pddsTop = NULL; // top of whole surface
	LPDIRECTDRAWSURFACE7 pddsTopFace = NULL; // top of face we want
	LPDIRECTDRAWSURFACE7 pddsLevel = NULL; // mip level we want
	LPDIRECTDRAWSURFACE7 pddsCur = NULL; // new texture we will use
	DDSURFACEDESC2 ddsd;

	// Get top of original or new surface
	if (bOrig)
		pddsTop = GetDocument()->PddsOrig();
	else
		pddsTop = GetDocument()->PddsNew();
	pddsTop->AddRef();

	// Adjust dwCubeMapFace if necessary
	// If specified face doesn't exist, set to 0 and let code below pick one
	if ((dwCubeMapFace & GetDocument()->DwCubeMapFlags()) == 0)
		dwCubeMapFace = 0;
	ZeroMemory(&ddsd, sizeof(ddsd));
	ddsd.dwSize = sizeof(ddsd);
	if (FAILED(hr = pddsTop->GetSurfaceDesc(&ddsd)))
		goto LFail;
	// If surface has no faces, set current face to "none"
	if (dwCubeMapFace != 0 && (ddsd.ddsCaps.dwCaps2 & DDSCAPS2_CUBEMAP) == 0)
		dwCubeMapFace = 0;
	// If surface has faces and no face specified, set current face to first one
	if (dwCubeMapFace == 0 && (ddsd.ddsCaps.dwCaps2 & DDSCAPS2_CUBEMAP) != 0)
		dwCubeMapFace = ddsd.ddsCaps.dwCaps2 & DDSCAPS2_CUBEMAP_ALLFACES;

	// Get top of the correct face
	if (dwCubeMapFace == 0)
	{
		pddsTopFace = pddsTop;
		pddsTopFace->AddRef();
	}
	else
	{
		ZeroMemory(&ddsd, sizeof(ddsd));
		ddsd.dwSize = sizeof(ddsd);
		if (FAILED(hr = pddsTop->GetSurfaceDesc(&ddsd)))
			goto LFail;
		if ((ddsd.ddsCaps.dwCaps2 & DDSCAPS2_CUBEMAP_ALLFACES) == dwCubeMapFace)
		{
			pddsTopFace = pddsTop;
			pddsTopFace->AddRef();
		}
		else
		{
			DDSCAPS2 ddsCaps;
			ZeroMemory(&ddsCaps, sizeof(ddsCaps));
			ddsCaps.dwCaps2 = dwCubeMapFace;

			if (FAILED(hr = pddsTop->GetAttachedSurface(&ddsCaps, &pddsTopFace)))
				return hr;
		}
	}
	ReleasePpo(&pddsTop);

	// Get correct mip level
	{
		LPDIRECTDRAWSURFACE7 pdds;
		LPDIRECTDRAWSURFACE7 pdds2;
		DDSCAPS2 ddsCaps;
		INT i;
		HRESULT hr;

		ZeroMemory(&ddsCaps, sizeof(ddsCaps));
		ddsCaps.dwCaps = DDSCAPS_TEXTURE;
		ddsCaps.dwCaps2 = DDSCAPS2_MIPMAPSUBLEVEL;
		pdds = pddsTopFace;
		pdds->AddRef();
		for (i = 0; i < lwMip; i++)
		{
			if (FAILED(hr = pdds->GetAttachedSurface(&ddsCaps, &pdds2)))
				return hr;
			ReleasePpo(&pdds);
			pdds = pdds2;
		}
		pddsLevel = pdds;
	}
	ReleasePpo(&pddsTopFace);

	// make surface that will be used as texture for current view
	if (bViewAlpha)
	{
		if (FAILED(hr = GenerateAlphaImage(pddsLevel, &pddsCur)))
			goto LFail;
	}
	else
	{
		ZeroMemory(&ddsd, sizeof(ddsd));
		ddsd.dwSize = sizeof(ddsd);
		if (FAILED(hr = pddsLevel->GetSurfaceDesc(&ddsd)))
			goto LFail;
		ddsd.ddsCaps.dwCaps &= ~(DDSCAPS_MIPMAP | DDSCAPS_COMPLEX);
		ddsd.ddsCaps.dwCaps2 &= ~(DDSCAPS2_CUBEMAP | DDSCAPS2_CUBEMAP_ALLFACES);
		ddsd.dwFlags = DDSD_WIDTH | DDSD_HEIGHT | DDSD_CAPS | DDSD_PIXELFORMAT;
		if (FAILED(hr = PDxtexApp()->Pdd()->CreateSurface(&ddsd, &pddsCur, NULL)))
			goto LFail;
		if (FAILED(hr = pddsCur->Blt(NULL, pddsLevel, NULL, DDBLT_WAIT, NULL)))
			goto LFail;
	}
	ReleasePpo(&pddsLevel);

	ZeroMemory(&ddsd, sizeof(ddsd));
	ddsd.dwSize = sizeof(ddsd);
	if (FAILED(hr = pddsCur->GetSurfaceDesc(&ddsd)))
		goto LFail;
	m_rcSrc.SetRect(0, 0, ddsd.dwWidth, ddsd.dwHeight);
	m_rcDest.SetRect(0, 0, (INT)(ddsd.dwWidth * m_fZoom), (INT)(ddsd.dwHeight * m_fZoom));

	ReleasePpo(&m_pddsCur);
	m_pddsCur = pddsCur;
	m_bViewOrig = bOrig;
	m_bViewAlpha = bViewAlpha;
	m_dwCubeMapCur = dwCubeMapFace;
	m_lwMipCur = lwMip;
	return S_OK;

LFail:
	ReleasePpo(&pddsTop);
	ReleasePpo(&pddsTopFace);
	ReleasePpo(&pddsLevel);
	ReleasePpo(&pddsCur);
	return hr;
}


void CDxtexView::OnViewNegX() 
{
	BuildViewSurface(m_bViewOrig, DDSCAPS2_CUBEMAP_NEGATIVEX, m_lwMipCur, m_bViewAlpha);
	m_bTitleModsChanged = TRUE; // force title bar update
	RenderScene();
	Invalidate(); // force redraw of this view
}


void CDxtexView::OnUpdateViewNegX(CCmdUI* pCmdUI) 
{
	BOOL bEnable = ((GetDocument()->DwCubeMapFlags() & DDSCAPS2_CUBEMAP_NEGATIVEX) != 0);
	pCmdUI->Enable(bEnable); 
	pCmdUI->SetCheck(m_dwCubeMapCur == DDSCAPS2_CUBEMAP_NEGATIVEX);
}


void CDxtexView::OnViewPosX() 
{
	BuildViewSurface(m_bViewOrig, DDSCAPS2_CUBEMAP_POSITIVEX, m_lwMipCur, m_bViewAlpha);
	m_bTitleModsChanged = TRUE; // force title bar update
	RenderScene();
	Invalidate(); // force redraw of this view
}


void CDxtexView::OnUpdateViewPosX(CCmdUI* pCmdUI) 
{
	BOOL bEnable = ((GetDocument()->DwCubeMapFlags() & DDSCAPS2_CUBEMAP_POSITIVEX) != 0);
	pCmdUI->Enable(bEnable); 
	pCmdUI->SetCheck(m_dwCubeMapCur == DDSCAPS2_CUBEMAP_POSITIVEX);
}


void CDxtexView::OnViewNegY() 
{
	BuildViewSurface(m_bViewOrig, DDSCAPS2_CUBEMAP_NEGATIVEY, m_lwMipCur, m_bViewAlpha);
	m_bTitleModsChanged = TRUE; // force title bar update
	RenderScene();
	Invalidate(); // force redraw of this view
}


void CDxtexView::OnUpdateViewNegY(CCmdUI* pCmdUI) 
{
	BOOL bEnable = ((GetDocument()->DwCubeMapFlags() & DDSCAPS2_CUBEMAP_NEGATIVEY) != 0);
	pCmdUI->Enable(bEnable); 
	pCmdUI->SetCheck(m_dwCubeMapCur == DDSCAPS2_CUBEMAP_NEGATIVEY);
}


void CDxtexView::OnViewPosY() 
{
	BuildViewSurface(m_bViewOrig, DDSCAPS2_CUBEMAP_POSITIVEY, m_lwMipCur, m_bViewAlpha);
	m_bTitleModsChanged = TRUE; // force title bar update
	RenderScene();
	Invalidate(); // force redraw of this view
}


void CDxtexView::OnUpdateViewPosY(CCmdUI* pCmdUI) 
{
	BOOL bEnable = ((GetDocument()->DwCubeMapFlags() & DDSCAPS2_CUBEMAP_POSITIVEY) != 0);
	pCmdUI->Enable(bEnable); 
	pCmdUI->SetCheck(m_dwCubeMapCur == DDSCAPS2_CUBEMAP_POSITIVEY);
}


void CDxtexView::OnViewNegZ() 
{
	BuildViewSurface(m_bViewOrig, DDSCAPS2_CUBEMAP_NEGATIVEZ, m_lwMipCur, m_bViewAlpha);
	m_bTitleModsChanged = TRUE; // force title bar update
	RenderScene();
	Invalidate(); // force redraw of this view
}


void CDxtexView::OnUpdateViewNegZ(CCmdUI* pCmdUI) 
{
	BOOL bEnable = ((GetDocument()->DwCubeMapFlags() & DDSCAPS2_CUBEMAP_NEGATIVEZ) != 0);
	pCmdUI->Enable(bEnable); 
	pCmdUI->SetCheck(m_dwCubeMapCur == DDSCAPS2_CUBEMAP_NEGATIVEZ);
}


void CDxtexView::OnViewPosZ() 
{
	BuildViewSurface(m_bViewOrig, DDSCAPS2_CUBEMAP_POSITIVEZ, m_lwMipCur, m_bViewAlpha);
	m_bTitleModsChanged = TRUE; // force title bar update
	RenderScene();
	Invalidate(); // force redraw of this view
}


void CDxtexView::OnUpdateViewPosZ(CCmdUI* pCmdUI) 
{
	BOOL bEnable = ((GetDocument()->DwCubeMapFlags() & DDSCAPS2_CUBEMAP_POSITIVEZ) != 0);
	pCmdUI->Enable(bEnable); 
	pCmdUI->SetCheck(m_dwCubeMapCur == DDSCAPS2_CUBEMAP_POSITIVEZ);
}

void CDxtexView::OnFileOpenFace() 
{
	GetDocument()->OpenCubeFace(m_dwCubeMapCur);
}

void CDxtexView::OnUpdateFileOpenFace(CCmdUI* pCmdUI) 
{
	BOOL bEnable = (m_dwCubeMapCur != 0);
	pCmdUI->Enable(bEnable); 
}

void CDxtexView::OnFileOpenAlphaFace() 
{
	GetDocument()->OpenAlphaCubeFace(m_dwCubeMapCur);
}

void CDxtexView::OnUpdateFileOpenAlphaFace(CCmdUI* pCmdUI) 
{
	BOOL bEnable = (m_dwCubeMapCur != 0);
	pCmdUI->Enable(bEnable); 
}
