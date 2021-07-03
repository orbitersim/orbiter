//-----------------------------------------------------------------------------
// File: D3DApp.h
//
// Desc: Application class for the Direct3D samples framework library.
//
// Copyright (c) 1998-1999 Microsoft Corporation. All rights reserved.
//-----------------------------------------------------------------------------
#ifndef  D3DAPP_H
#define  D3DAPP_H
#define  D3D_OVERLOADS
#include <d3d.h>
#include "D3DFrame.h"
#include "D3DEnum.h"
#include "D3DUtil.h"
#include "D3DRes.h"




//-----------------------------------------------------------------------------
// Name: class CD3DApplication
// Desc:
//-----------------------------------------------------------------------------
class CD3DApplication
{
    // Internal variables and member functions
    CD3DFramework7* m_pFramework;
    BOOL            m_bActive;
    BOOL            m_bReady;

    BOOL            m_bFrameMoving;
    BOOL            m_bSingleStep;
    DWORD           m_dwBaseTime;
    DWORD           m_dwStopTime;

    HRESULT Initialize3DEnvironment();
    HRESULT Change3DEnvironment();

    HRESULT Render3DEnvironment();
    VOID    Cleanup3DEnvironment();
    VOID    DisplayFrameworkError( HRESULT, DWORD );

protected:
    HWND                 m_hWnd;
    D3DEnum_DeviceInfo*  m_pDeviceInfo;
    LPDIRECTDRAW7        m_pDD;
    LPDIRECT3D7          m_pD3D;
    LPDIRECT3DDEVICE7    m_pd3dDevice;
    LPDIRECTDRAWSURFACE7 m_pddsRenderTarget;
    LPDIRECTDRAWSURFACE7 m_pddsRenderTargetLeft; // For stereo modes
    DDSURFACEDESC2       m_ddsdRenderTarget;

    // Overridable variables for the app
    TCHAR*               m_strWindowTitle;
    BOOL                 m_bAppUseZBuffer;
    BOOL                 m_bAppUseStereo;
    BOOL                 m_bShowStats;
    HRESULT              (*m_fnConfirmDevice)(DDCAPS*, D3DDEVICEDESC7*);

    // Overridable functions for the 3D scene created by the app
    virtual HRESULT OneTimeSceneInit()     { return S_OK; }
    virtual HRESULT InitDeviceObjects()    { return S_OK; }
    virtual HRESULT DeleteDeviceObjects()  { return S_OK; }
    virtual HRESULT Render()               { return S_OK; }
    virtual HRESULT FrameMove( FLOAT )     { return S_OK; }
    virtual HRESULT RestoreSurfaces()      { return S_OK; }
    virtual HRESULT FinalCleanup()         { return S_OK; }

    // Overridable power management (APM) functions
    virtual LRESULT OnQuerySuspend( DWORD dwFlags );
    virtual LRESULT OnResumeSuspend( DWORD dwData );

    // View control functions (for stereo-enabled applications)
    D3DMATRIX m_matLeftView;
    D3DMATRIX m_matRightView;
    D3DMATRIX m_matView;
    VOID    SetAppLeftViewMatrix( D3DMATRIX mat )  { m_matLeftView  = mat; }
    VOID    SetAppRightViewMatrix( D3DMATRIX mat ) { m_matRightView = mat; }
    VOID    SetAppViewMatrix( D3DMATRIX mat )      { m_matView      = mat; }
    VOID    SetViewParams( D3DVECTOR* vEyePt, D3DVECTOR* vLookatPt,
                           D3DVECTOR* vUpVec, FLOAT fEyeDistance );

    // Miscellaneous functions
    VOID    ShowStats();
    VOID    OutputText( DWORD x, DWORD y, TCHAR* str );

public:
    // Functions to create, run, pause, and clean up the application
    virtual HRESULT Create( HINSTANCE, TCHAR* );
    virtual INT     Run();
    virtual LRESULT MsgProc( HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam );
    virtual VOID    Pause( BOOL bPause );

    // Class constructor
    CD3DApplication();
};




#endif // D3DAPP_H


