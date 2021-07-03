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
// Error reporting support
//-----------------------------------------------------------------------------
enum    APPMSGTYPE { MSG_NONE, MSGERR_APPMUSTEXIT,
                     MSGWARN_SWITCHTOSOFTWARE, MSGWARN_CANTDOFULLSCREEN };
VOID    DisplayFrameworkError( HRESULT hr, APPMSGTYPE errType );




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

    CComboBox *m_pTex0ColorArg1, *m_pTex0ColorOp, *m_pTex0ColorArg2;
    CComboBox *m_pTex0AlphaArg1, *m_pTex0AlphaOp, *m_pTex0AlphaArg2;
    CComboBox *m_pTex1ColorArg1, *m_pTex1ColorOp, *m_pTex1ColorArg2;
    CComboBox *m_pTex1AlphaArg1, *m_pTex1AlphaOp, *m_pTex1AlphaArg2;
    CComboBox *m_pTex2ColorArg1, *m_pTex2ColorOp, *m_pTex2ColorArg2;
    CComboBox *m_pTex2AlphaArg1, *m_pTex2AlphaOp, *m_pTex2AlphaArg2;

    VOID    InitializeUIControls();
    VOID    UpdateDeviceInfo();
    VOID    UpdateUIForDeviceCapabilites();
    VOID    UpdateStageColor( WORD stage, LONG op, LONG arg1, LONG arg2 );
    VOID    UpdateStageAlpha( WORD stage, LONG op, LONG arg1, LONG arg2 );

    HRESULT AppInitialize( HWND );
    HRESULT Initialize3DEnvironment();
    HRESULT Change3DEnvironment();
    HRESULT Restore3DEnvironment();
    HRESULT OutputText( DWORD, DWORD, CHAR* );

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
    afx_msg VOID OnToggleFullScreen();
    afx_msg VOID OnViewCode();
    afx_msg VOID OnChangeDevice();
    afx_msg VOID OnChangePresetEffects();
    afx_msg VOID OnChangeTex();
    afx_msg VOID OnSelectTexture0Name();
    afx_msg VOID OnSelectTexture1Name();
    afx_msg VOID OnSelectTexture2Name();
    afx_msg VOID OnChangeBlendFactor();
    afx_msg VOID OnChangeDiffuseColor();
    afx_msg VOID OnChangeStageArgs();
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


