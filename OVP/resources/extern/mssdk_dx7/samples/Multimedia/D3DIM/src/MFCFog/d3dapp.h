//-----------------------------------------------------------------------------
// File: D3DApp.h
//
// Desc: Header file for a D3DIM app that uses MFC
//
//
// Copyright (c) 1997-1999 Microsoft Corporation. All rights reserved.
//-----------------------------------------------------------------------------
#ifndef D3D_MFC_APP_H
#define D3D_MFC_APP_H

#if _MSC_VER >= 1000
#pragma once
#endif
#ifndef __AFXWIN_H__
#error include 'stdafx.h' before including this file
#endif

#include "resource.h"




//-----------------------------------------------------------------------------
// Name: class CD3DApp
// Desc: Main MFCapplication class derived from CWinApp.
//-----------------------------------------------------------------------------
class CD3DApp : public CWinApp
{
public:
    CD3DApp();

    //{{AFX_VIRTUAL(CD3DApp)
    virtual BOOL InitInstance();
    virtual BOOL OnIdle( LONG );
    //}}AFX_VIRTUAL

    //{{AFX_MSG(CD3DApp)
    //}}AFX_MSG
    DECLARE_MESSAGE_MAP()
};




//-----------------------------------------------------------------------------
// Name: class CAppDoc
// Desc: Empty CDocument class for MFC programming
//-----------------------------------------------------------------------------
class CAppDoc : public CDocument
{
protected:
    DECLARE_DYNCREATE(CAppDoc)
};




//-----------------------------------------------------------------------------
// Name: class CAppForm
// Desc: CFormView-based class which allows the UI to be created with a form
//       (dialog) resource. This class manages all the controls on the form.
//-----------------------------------------------------------------------------
class CAppForm : public CFormView
{
    D3DDEVICEINFO*      m_pDeviceInfo;
    D3DDEVICEINFO*      m_pFullScreenDeviceInfo;
	CD3DFramework7*     m_pFramework;
	
	BOOL                m_bUsingHELForWindowedMode;
    HWND                m_hwndRenderWindow;
    HWND                m_hwndRenderFullScreen;
    
    BOOL                m_bReady;
    BOOL                m_bWindowed;

    LPDIRECT3DDEVICE7   m_pd3dDevice;

    BOOL                m_bHiResTerrain;
    DWORD               m_dwFogColor;
    DWORD               m_dwFogMode;
    BOOL                m_bCanDoTableFog;
    BOOL                m_bCanDoVertexFog;
    BOOL                m_bCanDoWFog;
    BOOL                m_bRangeBasedFog;
    BOOL                m_bUsingTableFog;
    FLOAT               m_fFogStart;
    FLOAT               m_fFogEnd;
    FLOAT               m_fFogDensity;

    HRESULT AppInitialize( HWND );
    HRESULT Initialize3DEnvironment();
    HRESULT Change3DEnvironment();
    HRESULT Restore3DEnvironment();
    HRESULT OutputText( DWORD, DWORD, CHAR* );

    VOID    UpdateDeviceInfo();
    VOID    UpdateUIForDeviceCapabilites();
    VOID    SetFogParameters();

public:
    VOID    Cleanup3DEnvironment();
    HRESULT Render3DEnvironment();
    VOID    GoFullScreen();
    VOID    GoWindowed();
    VOID    Move( int, int );

protected:
    DECLARE_DYNCREATE(CAppForm)

             CAppForm();
    virtual  ~CAppForm();
public:
    //{{AFX_DATA(CAppForm)
    enum { IDD = IDD_FORMVIEW };
    //}}AFX_DATA

    //{{AFX_VIRTUAL(CAppForm)
    virtual void OnInitialUpdate();
    //}}AFX_VIRTUAL

    //{{AFX_MSG(CAppForm)
    afx_msg void OnToggleFullScreen();
    afx_msg void OnChangeDevice();
    afx_msg void OnHScroll( UINT, UINT, CScrollBar* );
    afx_msg void OnFogColor();
    afx_msg void OnRangeBasedFog();
    afx_msg void OnVertexFog();
    afx_msg void OnTableFog();
    afx_msg void OnFogMode();
    afx_msg void OnTerrainResolution();
	//}}AFX_MSG
    DECLARE_MESSAGE_MAP()
};




//-----------------------------------------------------------------------------
// Name: class CAppFrameWnd
// Desc: CFrameWnd-based class needed to get the Move message
//-----------------------------------------------------------------------------
class CAppFrameWnd : public CFrameWnd
{
protected:
    DECLARE_DYNCREATE(CAppFrameWnd)
public:
    virtual BOOL PreCreateWindow( CREATESTRUCT& cs );
    afx_msg VOID OnMove( int, int );
    DECLARE_MESSAGE_MAP()
};




#endif


