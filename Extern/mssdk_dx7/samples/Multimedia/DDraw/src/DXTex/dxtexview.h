// dxtexView.h : interface of the CDxtexView class
//
/////////////////////////////////////////////////////////////////////////////

#if !defined(AFX_DXTXVIEW_H__712C53D1_D63B_11D1_A8B5_00C04FC2DC22__INCLUDED_)
#define AFX_DXTXVIEW_H__712C53D1_D63B_11D1_A8B5_00C04FC2DC22__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000


class CDxtexView : public CScrollView
{
protected: // create from serialization only
	CDxtexView();
	DECLARE_DYNCREATE(CDxtexView)
	CDxtexDoc* GetDocument();

// Operations
public:

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CDxtexView)
	public:
	virtual void OnDraw(CDC* pDC);  // overridden to draw this view
	virtual void OnInitialUpdate();
	protected:
	virtual void OnUpdate(CView* pSender, LPARAM lHint, CObject* pHint);
	//}}AFX_VIRTUAL

// Implementation
public:
	virtual ~CDxtexView();
#ifdef _DEBUG
	virtual void AssertValid() const;
	virtual void Dump(CDumpContext& dc) const;
#endif
	BOOL TitleModsChanged(VOID) { return m_bTitleModsChanged; }
	VOID ClearTitleModsChanged(VOID) { m_bTitleModsChanged = FALSE; }
	CString GetStrTitleMods(VOID);
	VOID GetImageInfo(CString& strInfo);

// Generated message map functions
protected:
	//{{AFX_MSG(CDxtexView)
	afx_msg void OnLButtonUp(UINT nFlags, CPoint point);
	afx_msg void OnFileOpenFace();
	afx_msg void OnFileOpenAlphaFace();
	afx_msg void OnViewOriginal();
	afx_msg void OnViewCompressed();
	afx_msg void OnViewSmallerMipLevel();
	afx_msg void OnViewLargerMipLevel();
	afx_msg void OnViewAlphaChannel();
	afx_msg void OnViewZoomIn();
	afx_msg void OnViewZoomOut();
	afx_msg void OnViewChangeBackgroundColor();
	afx_msg void OnFileOpenSubsurface();
	afx_msg void OnFileOpenAlphaSubsurface();
	afx_msg void OnViewNegX();
	afx_msg void OnViewPosX();
	afx_msg void OnViewNegY();
	afx_msg void OnViewPosY();
	afx_msg void OnViewNegZ();
	afx_msg void OnViewPosZ();
	afx_msg void OnUpdateViewOriginal(CCmdUI* pCmdUI);
	afx_msg void OnUpdateViewCompressed(CCmdUI* pCmdUI);
	afx_msg void OnUpdateViewAlphaChannel(CCmdUI* pCmdUI);
	afx_msg void OnUpdateViewLargerMipLevel(CCmdUI* pCmdUI);
	afx_msg void OnUpdateViewSmallerMipLevel(CCmdUI* pCmdUI);
	afx_msg void OnUpdateViewZoomIn(CCmdUI* pCmdUI);
	afx_msg void OnUpdateViewZoomOut(CCmdUI* pCmdUI);
	afx_msg void OnUpdateFileOpenSubsurface(CCmdUI* pCmdUI);
	afx_msg void OnUpdateFileOpenAlphaSubsurface(CCmdUI* pCmdUI);
	afx_msg void OnUpdateViewNegX(CCmdUI* pCmdUI);
	afx_msg void OnUpdateViewPosX(CCmdUI* pCmdUI);
	afx_msg void OnUpdateViewNegY(CCmdUI* pCmdUI);
	afx_msg void OnUpdateViewPosY(CCmdUI* pCmdUI);
	afx_msg void OnUpdateViewNegZ(CCmdUI* pCmdUI);
	afx_msg void OnUpdateViewPosZ(CCmdUI* pCmdUI);
	afx_msg void OnUpdateFileOpenFace(CCmdUI* pCmdUI);
	afx_msg void OnUpdateFileOpenAlphaFace(CCmdUI* pCmdUI);
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()

private:
	HRESULT GenerateAlphaImage(LPDIRECTDRAWSURFACE7 pddsSrc, LPDIRECTDRAWSURFACE7* ppddsDest);
	HRESULT UpdateDevice(VOID);
	HRESULT RenderScene(VOID);
	CDxtexApp* PDxtexApp(VOID) { return (CDxtexApp*)AfxGetApp(); }
	DWORD DwCubeMapCur(LPDIRECTDRAWSURFACE7 pdds);
	HRESULT BuildViewSurface(BOOL bOrig, DWORD dwCubeMapFace, LONG lwMip, BOOL bViewAlpha);
	DWORD NumBytesInSurfaces(LPDIRECTDRAWSURFACE7 pddsTopMip);

	LPDIRECTDRAWSURFACE7 m_pddsCur; // Currently-viewed texture surface
	LPDIRECTDRAWSURFACE7 m_pddsBack; // The render target surface
	LPDIRECT3DDEVICE7 m_pd3ddev; // The renderer device
	CRect m_rcSrc; // Size of m_pddsCur and m_pddsBack
	CRect m_rcDest; // m_rcDest scaled by m_fZoom
	FLOAT m_fZoom; // Zoom factor
	BOOL m_bViewOrig; // View "original" vs. "new" surface
	BOOL m_bViewAlpha; // View alpha channel alone vs. normal image
	BOOL m_bTitleModsChanged; // Whether title bar text modifiers need to be updated
	LONG m_lwMipCur; // Currently-viewed mip.  0 = top, 1 = next one down, etc.
	DWORD m_dwCubeMapCur; // 0 = no cube map, or DDSCAPS2_CUBEMAP_NEGATIVEX, etc.
	DWORD m_dwClearColor; // Background color that will show through where alpha is non-opaque
};

#ifndef _DEBUG  // debug version in dxtexView.cpp
inline CDxtexDoc* CDxtexView::GetDocument()
   { return (CDxtexDoc*)m_pDocument; }
#endif

/////////////////////////////////////////////////////////////////////////////

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_DXTXVIEW_H__712C53D1_D63B_11D1_A8B5_00C04FC2DC22__INCLUDED_)
