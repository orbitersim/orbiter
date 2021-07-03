// ChildFrm.cpp : implementation of the CChildFrame class
//

#include "stdafx.h"
#include "dxtex.h"
#include "dxtexdoc.h"
#include "dxtexview.h"

#include "ChildFrm.h"

#ifndef WM_IDLEUPDATECMDUI
	#define WM_IDLEUPDATECMDUI  0x0363  // wParam == bDisableIfNoHandler
#endif

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CChildFrame

IMPLEMENT_DYNCREATE(CChildFrame, CMDIChildWnd)

BEGIN_MESSAGE_MAP(CChildFrame, CMDIChildWnd)
	//{{AFX_MSG_MAP(CChildFrame)
	ON_MESSAGE(WM_IDLEUPDATECMDUI, OnIdleUpdateCmdUI)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CChildFrame construction/destruction

CChildFrame::CChildFrame()
{
}

CChildFrame::~CChildFrame()
{
}

BOOL CChildFrame::PreCreateWindow(CREATESTRUCT& cs)
{
	if( !CMDIChildWnd::PreCreateWindow(cs) )
		return FALSE;

	return TRUE;
}


/////////////////////////////////////////////////////////////////////////////
// CChildFrame diagnostics

#ifdef _DEBUG
void CChildFrame::AssertValid() const
{
	CMDIChildWnd::AssertValid();
}

void CChildFrame::Dump(CDumpContext& dc) const
{
	CMDIChildWnd::Dump(dc);
}

#endif //_DEBUG

/////////////////////////////////////////////////////////////////////////////
// CChildFrame message handlers

BOOL CChildFrame::Create(LPCTSTR lpszClassName, LPCTSTR lpszWindowName, DWORD dwStyle, const RECT& rect, CMDIFrameWnd* pParentWnd, CCreateContext* pContext) 
{
	if (!CMDIChildWnd::Create(lpszClassName, lpszWindowName, dwStyle, rect, pParentWnd, pContext))
		return FALSE;

	return TRUE;
}



// Handle WM_IDLEUPDATECMDUI to update modified indicator if necessary.
LRESULT CChildFrame::OnIdleUpdateCmdUI(WPARAM wParam, LPARAM)
{
	// Only update the title if the doc or view state has changed.
	// Otherwise, the title bar will flicker.
	CDxtexDoc* pDoc = (CDxtexDoc*)GetActiveDocument();
	CDxtexView* pView = (CDxtexView*)GetActiveView();
	if (pView->TitleModsChanged() || pDoc->TitleModsChanged())
	{
		// This will force MFC to call CChildFrame::OnUpdateTitleFrame:
		m_nIdleFlags |= idleTitle;
		pView->ClearTitleModsChanged();
		pDoc->ClearTitleModsChanged();
	}

	// Do the default thing
	CMDIChildWnd::OnIdleUpdateCmdUI();
	return 0L;
} 


void CChildFrame::OnUpdateFrameTitle(BOOL bAddToTitle)
{
	CMDIChildWnd::OnUpdateFrameTitle(bAddToTitle);
	CDxtexView* pView = (CDxtexView*)GetActiveView();
	{
		CString title;
		GetWindowText(title);
		title += " " + pView->GetStrTitleMods();
		SetWindowText(title);
	}
}
