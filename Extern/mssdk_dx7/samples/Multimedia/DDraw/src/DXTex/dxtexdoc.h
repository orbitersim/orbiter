// dxtexDoc.h : interface of the CDxtexDoc class
//
/////////////////////////////////////////////////////////////////////////////

#if !defined(AFX_DXTXDOC_H__712C53CF_D63B_11D1_A8B5_00C04FC2DC22__INCLUDED_)
#define AFX_DXtxDOC_H__712C53CF_D63B_11D1_A8B5_00C04FC2DC22__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000


class CDxtexDoc : public CDocument
{
protected: // create from serialization only
	CDxtexDoc();
	DECLARE_DYNCREATE(CDxtexDoc)

public:
// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CDxtexDoc)
	public:
	virtual BOOL OnNewDocument();
	virtual void Serialize(CArchive& ar);
	virtual void SetPathName(LPCTSTR lpszPathName, BOOL bAddToMRU = TRUE);
	//}}AFX_VIRTUAL

// Implementation
public:
	HRESULT LoadAlphaBmp(CString& strPath);
	VOID GenerateMipMaps(VOID);
	HRESULT GenerateMipMapsFromTop(LPDIRECTDRAWSURFACE7 pddsSrcTop);
	HRESULT Compress(DWORD dwFourCC, BOOL bSwitchView);
	DWORD NumMips(VOID);
	LPDIRECTDRAWSURFACE7 PddsOrig(VOID) { return m_pddsOrig; }
	LPDIRECTDRAWSURFACE7 PddsNew(VOID) { return m_pddsNew; }
	DWORD DwWidth(VOID) { return m_dwWidth; }
	DWORD DwHeight(VOID) { return m_dwHeight; }
	HRESULT GetTopCubeFace(LPDIRECTDRAWSURFACE7 pdds, DWORD dwCubeMapFlags, LPDIRECTDRAWSURFACE7* ppddsFaceTop);
	BOOL TitleModsChanged(VOID) { return m_bTitleModsChanged; }
	VOID ClearTitleModsChanged(VOID) { m_bTitleModsChanged = FALSE; }
	virtual ~CDxtexDoc();
	void OpenSubsurface(DWORD dwCubeMapFlags, LONG lwMip);
	void OpenAlphaSubsurface(DWORD dwCubeMapFlags, LONG lwMip);
	void OpenCubeFace(DWORD dwCubeMapFlags);
	void OpenAlphaCubeFace(DWORD dwCubeMapFlags);
	DWORD DwCubeMapFlags(VOID) { return m_dwCubeMapFlags; }
#ifdef _DEBUG
	virtual void AssertValid() const;
	virtual void Dump(CDumpContext& dc) const;
#endif

// Generated message map functions
protected:
	//{{AFX_MSG(CDxtexDoc)
	afx_msg void OnFileOpenAlpha();
	afx_msg void OnGenerateMipMaps();
	afx_msg void OnFormatDxt1();
	afx_msg void OnFormatDxt2();
	afx_msg void OnFormatDxt3();
	afx_msg void OnFormatDxt4();
	afx_msg void OnFormatDxt5();
	afx_msg void OnFormatChangeCubeMapFaces();
	afx_msg void OnUpdateFileOpenAlpha(CCmdUI* pCmdUI);
	afx_msg void OnUpdateFormatGenerateMipmaps(CCmdUI* pCmdUI);
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()

private:
	LPDIRECTDRAWSURFACE7 m_pddsOrig;
	LPDIRECTDRAWSURFACE7 m_pddsNew;
	DWORD m_dwWidth;
	DWORD m_dwHeight;
	DWORD m_numMips;
	DWORD m_dwCubeMapFlags;
	BOOL m_bTitleModsChanged;

	HRESULT LoadBmp(CString& strPath);
	CDxtexApp* PDxtexApp(VOID) { return (CDxtexApp*)AfxGetApp(); }
	HRESULT GetNthMipMap(LPDIRECTDRAWSURFACE7 pddsTop, LONG lwMip,
		LPDIRECTDRAWSURFACE7* ppDDS);
	HRESULT CreateSurfaceFromBmp(CString& strPath, LPDIRECTDRAWSURFACE7* ppdds);
	HRESULT LoadAlphaIntoSurface(CString& strPath, LPDIRECTDRAWSURFACE7 pdds);
	HRESULT ChangeCubeMapFlags(LPDIRECTDRAWSURFACE7* ppddsSrc, DWORD dwCubeMapFlagsNew);
	HRESULT BltAllLevels(LPDIRECTDRAWSURFACE7 pddsSrcTop, LPDIRECTDRAWSURFACE7 pddsDestTop);
	BOOL PromptForBmp(CString* pstrPath);
	HRESULT SaveDDS(LPDIRECTDRAWSURFACE7 pdds, CArchive& ar);
	HRESULT SaveAllMipSurfaces(LPDIRECTDRAWSURFACE7 pddsTop, CArchive& ar);
	HRESULT LoadDDS(LPDIRECTDRAWSURFACE7* ppdds, CArchive& ar);
	HRESULT LoadAllMipSurfaces(LPDIRECTDRAWSURFACE7 pddsTop, CArchive& ar);
	HRESULT GenerateMip(LPDIRECTDRAWSURFACE7 pddsSrc, LPDIRECTDRAWSURFACE7 pddsDest, RECT* prcDest);
};

/////////////////////////////////////////////////////////////////////////////

/////////////////////////////////////////////////////////////////////////////
// CCubeMapDlg dialog

class CCubeMapDlg : public CDialog
{
// Construction
public:
	CCubeMapDlg(CWnd* pParent = NULL);   // standard constructor

// Dialog Data
	//{{AFX_DATA(CCubeMapDlg)
	enum { IDD = IDD_CUBEMAP };
	BOOL	m_bNegX;
	BOOL	m_bNegY;
	BOOL	m_bNegZ;
	BOOL	m_bPosX;
	BOOL	m_bPosY;
	BOOL	m_bPosZ;
	//}}AFX_DATA

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CCubeMapDlg)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation
protected:

	// Generated message map functions
	//{{AFX_MSG(CCubeMapDlg)
		// NOTE: the ClassWizard will add member functions here
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};
//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_DXTXDOC_H__712C53CF_D63B_11D1_A8B5_00C04FC2DC22__INCLUDED_)
