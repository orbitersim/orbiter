VERSION 5.00
Begin VB.Form MainFrm 
   BorderStyle     =   0  'None
   Caption         =   "Teapot Sample"
   ClientHeight    =   6165
   ClientLeft      =   0
   ClientTop       =   0
   ClientWidth     =   6795
   Icon            =   "MainFrm.frx":0000
   LinkTopic       =   "Form1"
   ScaleHeight     =   6165
   ScaleWidth      =   6795
   ShowInTaskbar   =   1  'True
   StartUpPosition =   3  'Windows Default
End
Attribute VB_Name = "MainFrm"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
' Sample showing how to use the shadow object as well as going full screen in RM
Option Explicit

Dim DX As New DirectX7
Dim DD As DirectDraw4
Dim ddClipper As DirectDrawClipper
Dim RM As Direct3DRM3

Dim SurfPrimary As DirectDrawSurface4
Dim SurfBack As DirectDrawSurface4
Dim DDSDPrimary As DDSURFACEDESC2
Dim DDCapsBack As DDSCAPS2
Dim rmDevice As Direct3DRMDevice3
Dim rmViewport As Direct3DRMViewport2
Dim rootFrame As Direct3DRMFrame3
Dim lightFrame As Direct3DRMFrame3
Dim cameraFrame As Direct3DRMFrame3
Dim objectFrame As Direct3DRMFrame3
Dim light As Direct3DRMLight
Dim shadow_light As Direct3DRMLight
Dim meshBuilder As Direct3DRMMeshBuilder3
Dim object As Direct3DRMMeshBuilder3
Dim shadow As Direct3DRMShadow2
Dim bRunning As Boolean
Dim CurModeActiveStatus As Boolean
Dim bRestore As Boolean




Sub InitDX()

    ' create the ddraw object and set the cooperative level
    Set DD = DX.DirectDraw4Create("")
    MainFrm.Show
    DD.SetCooperativeLevel MainFrm.hWnd, DDSCL_FULLSCREEN Or DDSCL_EXCLUSIVE
    
    ' this will be full-screen, so set the display mode
    DD.SetDisplayMode 640, 480, 16, 0, DDSDM_DEFAULT
    
    ' create the primary surface
    DDSDPrimary.lFlags = DDSD_CAPS Or DDSD_BACKBUFFERCOUNT
    DDSDPrimary.ddsCaps.lCaps = DDSCAPS_PRIMARYSURFACE Or DDSCAPS_3DDEVICE Or DDSCAPS_COMPLEX Or DDSCAPS_FLIP
    DDSDPrimary.lBackBufferCount = 1
    Set SurfPrimary = DD.CreateSurface(DDSDPrimary)
           
    ' get the back buffer
    DDCapsBack.lCaps = DDSCAPS_BACKBUFFER
    Set SurfBack = SurfPrimary.GetAttachedSurface(DDCapsBack)
    
    ' Create the Retained Mode object
    Set RM = DX.Direct3DRMCreate()
    
    ' Now, create the device from the full screen DD surface
    Set rmDevice = RM.CreateDeviceFromSurface("IID_IDirect3DRGBDevice", DD, SurfBack, D3DRMDEVICE_DEFAULT)
    rmDevice.SetBufferCount 2
    rmDevice.SetQuality D3DRMLIGHT_ON Or D3DRMFILL_SOLID Or D3DRMRENDER_GOURAUD
    rmDevice.SetTextureQuality D3DRMTEXTURE_NEAREST
    rmDevice.SetRenderMode D3DRMRENDERMODE_BLENDEDTRANSPARENCY
End Sub



Sub InitScene(sMesh As String)
    
    ' Create the scene frames
    Set rootFrame = RM.CreateFrame(Nothing)
    Set cameraFrame = RM.CreateFrame(rootFrame)
    Set lightFrame = RM.CreateFrame(rootFrame)
    Set objectFrame = RM.CreateFrame(rootFrame)
    
    ' Set the background color
    rootFrame.SetSceneBackgroundRGB 0, 255, 255
    
    ' create & position lights and the viewport
    cameraFrame.SetPosition Nothing, 0, 0, -10
    Set rmViewport = RM.CreateViewport(rmDevice, cameraFrame, 0, 0, 640, 480)
    lightFrame.SetPosition Nothing, 2, 5, -10
    
    Set shadow_light = RM.CreateLightRGB(D3DRMLIGHT_POINT, 0.9, 0.8, 0.7)
    lightFrame.AddLight shadow_light
    
    Set light = RM.CreateLightRGB(D3DRMLIGHT_AMBIENT, 0.1, 0.1, 0.1)
    rootFrame.AddLight light
    
    ' create the mesh and load the teapot x file
    Set meshBuilder = RM.CreateMeshBuilder()
    meshBuilder.LoadFromFile sMesh, 0, 0, Nothing, Nothing
    
    ' make the shadow and enable alpha
    Set shadow = RM.CreateShadow(meshBuilder, shadow_light, 0, -3, 0, 0, 1, 0)
    shadow.SetOptions D3DRMSHADOW_TRUEALPHA

    ' add the visuals
    objectFrame.AddVisual meshBuilder
    objectFrame.AddVisual shadow
    
    'Have the object rotating
    objectFrame.SetRotation Nothing, 3, 3, 1, 0.45


End Sub

Sub RenderLoop()
    Dim t1 As Long
    Dim t2 As Long
    Dim delta As Single
    
    On Local Error Resume Next
    
    bRunning = True
    t1 = DX.TickCount()
    Do While bRunning = True
        DoEvents
        
        ' this will keep us from trying to blt in case we lose the surfaces (alt-tab)
        bRestore = False
        Do Until ExModeActive
            DoEvents
            bRestore = True
        Loop
    
        ' if we lost and got back the surfaces, then restore them
        DoEvents
        If bRestore Then
            bRestore = False
            DD.RestoreAllSurfaces
        End If
        
        
        rootFrame.Move 0.5
        rmViewport.Clear D3DRMCLEAR_TARGET Or D3DRMCLEAR_ZBUFFER ' clear the viewport
        rmDevice.Update   'blt the image to the screen
        rmViewport.Render rootFrame 'render to the device
        Call SurfBack.DrawText(10, 10, "D3DRM Full Screen with Alpha Shadow", False)
        Call SurfBack.DrawText(10, 30, "Click screen or hit ESC to exit", False)
        SurfPrimary.Flip Nothing, DDFLIP_WAIT
    Loop
End Sub

Function ExModeActive() As Boolean
    Dim TestCoopRes As Long
    
    TestCoopRes = DD.TestCooperativeLevel
    
    If (TestCoopRes = DD_OK) Then
        ExModeActive = True
    Else
        ExModeActive = False
    End If
End Function

Sub FindMediaDir(sFile As String)
    On Local Error Resume Next
    If Mid$(App.Path, 2, 1) = ":" Then
        ChDrive Mid$(App.Path, 1, 1)
    End If
    ChDir App.Path
    If Dir$(sFile) = "" Then
        ChDir "..\media"
    End If
    If Dir$(sFile) = "" Then
        ChDir "..\..\media"
    End If
End Sub

Private Sub Form_Load()
    Show
    DoEvents
    InitDX
    FindMediaDir "teapot.x"
    InitScene "teapot.x"
    RenderLoop
    End
End Sub


Sub EndIT()
    bRunning = False
    Call DD.RestoreDisplayMode
    Call DD.SetCooperativeLevel(Me.hWnd, DDSCL_NORMAL)
    End
End Sub

Private Sub Form_KeyPress(KeyAscii As Integer)
    If KeyAscii = 27 Then ' if ESC is pressed
        EndIT
    End If
End Sub

Private Sub Form_Click()
    EndIT
End Sub
