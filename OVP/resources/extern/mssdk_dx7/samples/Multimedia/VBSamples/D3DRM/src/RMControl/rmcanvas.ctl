VERSION 5.00
Begin VB.UserControl RMCanvas
   ClientHeight    =   4065
   ClientLeft      =   0
   ClientTop       =   0
   ClientWidth     =   5265
   ScaleHeight     =   271
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   351
   Begin VB.PictureBox Picture1 
      Height          =   2655
      Left            =   0
      ScaleHeight     =   173
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   261
      TabIndex        =   0
      Top             =   0
      Width           =   3975
   End
End
Attribute VB_Name = "RMCanvas"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Option Explicit

'Functions
'-  StartWindowed                   start in windowed mode  -
'
'-  InitFullScreen                  allows you to start full screen given a w h and bpp
'                                   allows you to select the ddraw and d3d guids
'
'-  InitWindowed                    allows you to start in windowed mode
'                                   allows you to select the ddraw and d3d guids
'
'-  Tick                            increment simulation/animations
'-  Render                          render the scene
'-  Update                          Combines tick and render
'-  GetBltRect                      get the update rectangle for our window
'-  CreateBoxMesh                   create a 3d cube
'-  CreateSheetMesh                 create a 1 or 2 sided rectange polygon
'-  GetBoundingBox                  get the bounding box for a frame
'-  PickTopMesh                     get a mesh from x,y screen coordinatess
'-  PickTopFrame                    get a frame from x,y screen coordinatess
'-  RotateFromXY                    xy screen coordinate rotation - for UI
'-  CreateUpdateableTexture         creates textures that allow you to modify them
'                                   unlikes LoadTexture. note: sFile can be ""
'
' Properties
'-  hWnd                    r       return control hwnd
'-  FPS                     r       returns Frame Persecond (from calls to update)
'-  SceneSpeed              rw
'-  IsFullScreen            r       returns true if fullscreen
'-  DisplayModes            r       returns list of available display modes
'                                   use before ChangeFullScreenDisplayMode
'-  VideoCards              r       return list of available cards
'-  Devices                 r       return list of available rederers
'
'-  Device                  r       return current RMdevice
'-  Viewport                r       return current RMviewport (hooked to CameraFrame)
'
'-  SceneFrame              r       root frame of our scene graph
'-  DirLightFrame           r       returns Frame (node) that contains DirLight
'                                   use it to position the light
'-  DirLight                r       returns default directional light
'                                   use it to change light color
'-  CameraFrame             r       returns Frame that represents camera
'-  AmbientLight            r       returns AmbientLight object
'
'-  DX                      r       returns DirectX object
'-  DDRaw                   r       returns DDraw Object
'-  BackBuffer              r       returns DDrawSurface used to write to
'-  D3DRM                   r       returns D3DRM object
'
'-  DirectDrawGuid          r       current directdraw object in use
'-  Direct3DGuid            r       current d3d rasterizer in use
'
'-  RotateFrame             rw      set enable use of auto UI rotation
'-  RotateMode              rw      set to change how things are rotated in the UI
'-  RotateRadius            rw      set to change how fast UI rotation is
'
'-  Use3DHardware           rw      if false forces Software rasterization
'-  UseBackbuffer           rw      if false allows for faster redering but
'                                   at the expense of flexiblity-
'                                   call before InitWindowed
'
Private Declare Function GetWindowRect Lib "user32.dll" (ByVal hwnd As Long, ByRef r As RECT) As Long



'-============================================================
' Full screen Events
'-============================================================
Dim WithEvents frmFSWindow As FSWindow
Attribute frmFSWindow.VB_VarHelpID = -1
Public Event KeyDown(keyCode As Integer, Shift As Integer)
Public Event KeyPress(KeyAscii As Integer)
Public Event KeyUp(keyCode As Integer, Shift As Integer)
Public Event MouseDown(Button As Integer, Shift As Integer, X As Single, Y As Single)
Public Event MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
Public Event MouseUp(Button As Integer, Shift As Integer, X As Single, Y As Single)
Public Event Click()
Public Event DblClick()
Public Event RestoreSurfaces()
Public Event Paint()
Public Event DirecXNotInstalled()
Public Event SceneMove(delta As Single)
Public Event ViewportClear()
Public Event PostRender()


'Enum
Enum RotateMethodEnum
    ROTATE_TRACKBALL = 0
    ROTATE_GLOBE = 1
End Enum

'UDTs
Private Type DeviceCharacteristics
    bDither As Long
    Name As String
    Quality As Long
    RenderMode As Long
    Shades As Long
    TexQ As Long
    WFoptions As Long
End Type

Private Type ViewportCharacteristics
    Back As Single
    Field As Single
    Front As Single
    Name As String
    Projection As Long
    left As Single
    right As Single
    bottom As Single
    top As Single
    scaling As Long
End Type
    

'- direct x object
Dim m_dx As New DirectX7

'- direct draw objects
Dim m_dd As DirectDraw4
Dim m_ddClip As DirectDrawClipper       'Direct Draw clipper object
Dim m_frontBuffer As DirectDrawSurface4
Dim m_backBuffer As DirectDrawSurface4

'- direct 3drm objects
Dim m_rm As Direct3DRM3
Dim m_rmDevice As Direct3DRMDevice3
Dim m_rmViewport As Direct3DRMViewport2
Dim m_rmFrameScene As Direct3DRMFrame3
Dim m_rmFrameCamera As Direct3DRMFrame3
Dim m_rmFrameDirLight As Direct3DRMFrame3
Dim m_rmFrameAmbientLight As Direct3DRMFrame3
Dim m_rmDirLight As Direct3DRMLight
Dim m_rmAmbientLight As Direct3DRMLight


'- state
Dim m_strDDGuid As String               'DirectDraw device guid
Dim m_strD3DGuid As String              'Direct3DRM device guid
Dim m_scenespeed As Single              'how fast animation run

Dim m_hwnd As Long                      'hwnd (either FSWindow or our ocx)
Dim m_binit As Boolean                  'are we initailized?
Dim m_bResizing As Boolean              'Are we in the midle of a resize operation
Dim m_lastRMMove As Long                'time stamp of last update
Dim m_lastFPS As Long                   'time stamp of last FPS update
Dim m_fps As Single                     'frame per second
Dim m_bCreateFromClipper As Boolean  'Use a clipper to start the RM
Dim m_DevInfo As DeviceCharacteristics
Dim m_ViewInfo As ViewportCharacteristics
Dim m_bfullscreen As Boolean
Dim m_bUseSoftwareOnly As Boolean
Dim m_errorReason As String
Dim m_emptyrect As RECT
Dim m_lastX As Long
Dim m_lastY As Long
Dim m_createid As Long
Dim m_bMouseDown

Public RotateMethod As RotateMethodEnum
Public RotateFrame As Direct3DRMFrame3
Public RotateRadius As Single
Public UpdateDC
Public Background   As DirectDrawSurface4

'-============================================================
' StartWindowed
'-============================================================
Public Function StartWindowed() As Boolean
    Dim b As Boolean
                
    b = InitWindowed("", "IID_IDirect3DHALDevice")
    If b = True Then
        StartWindowed = True
        Exit Function
    End If
    
    b = InitWindowed("", "IID_IDirect3DRGBDevice")
    StartWindowed = b
        
End Function




'-============================================================
' InitWindowed
'-============================================================
Public Function InitWindowed(ddrawguid As String, d3dguid As String) As Boolean
    Dim b As Boolean
    Dim ddsd As DDSURFACEDESC2
    
    On Local Error GoTo errOut
        
    m_errorReason = ""
    
    m_binit = False
    
    Picture1.Visible = False
    
    'make sure we have com out of fullscreen mode
    If Not (m_dd Is Nothing) Then m_dd.RestoreDisplayMode
    If Not (m_dd Is Nothing) Then m_dd.SetCooperativeLevel 0, DDSCL_NORMAL

    Cleanup
    
    CloseFSWindow
    
    'get rid of our current rm device..
    Set m_rmDevice = Nothing
    Set m_rmViewport = Nothing
    
    m_hwnd = UserControl.hwnd
    
    m_strDDGuid = ddrawguid
    m_strD3DGuid = d3dguid
    
    If d3dguid = "" Then m_strD3DGuid = "IID_IDirect3DRGBDevice"
    If m_bUseSoftwareOnly = True Then m_strD3DGuid = "IID_IDirect3DRGBDevice"


    'DirectDrawCreate
    m_errorReason = "RMCanvas: Could not create requested DirectDraw object from ddrawguid"
    Set m_dd = m_dx.DirectDraw4Create(m_strDDGuid)
    
    'Set The CooperativeLevel
    m_errorReason = "RMCanvas: Could set the cooperative level to normal"
    m_dd.SetCooperativeLevel m_hwnd, DDSCL_NORMAL
    
    
    
    'CreatePrimary
    m_errorReason = "RMCanvas: unable get screen surface from DirectDraw"
    ddsd.lFlags = DDSD_CAPS
    ddsd.ddsCaps.lCaps = DDSCAPS_PRIMARYSURFACE
    Set m_frontBuffer = m_dd.CreateSurface(ddsd)
    
    
    'Setup a clipper
    m_errorReason = "RMCanvas: failed to setup the clipper"
    Set m_ddClip = m_dd.CreateClipper(0)
    m_ddClip.SetHWnd m_hwnd
    m_frontBuffer.SetClipper m_ddClip
            
    
    b = ResizeWindowedDevice(m_strD3DGuid)
    If b = False Then GoTo errOut
    
    SetDeviceDefaults
    
    m_binit = True
    m_bfullscreen = False
    InitWindowed = True
    Exit Function
    
errOut:
    Cleanup
    
End Function

'-============================================================
' ResizeWindowedDevice
'-============================================================
Private Function ResizeWindowedDevice(d3dg As String) As Boolean
    If m_dd Is Nothing Then Exit Function
    
    If m_bfullscreen Then Exit Function
    
    On Local Error GoTo errOut
    
    
    Dim memflags As Long
    Dim r As RECT
    Dim ddsd As DDSURFACEDESC2
        
    
    'Get window extent
    Call GetWindowRect(m_hwnd, r)
    ddsd.lWidth = r.right - r.left
    ddsd.lHeight = r.bottom - r.top
    
    Set m_rmViewport = Nothing
    Set m_rmDevice = Nothing
    Set m_backBuffer = Nothing
    
    
    'Take care of createFromWindowed shortcut
    If m_bCreateFromClipper Then
      
        
        m_errorReason = "RMCanvas: unable to create RM Device or Viewport for current window size"
        Set m_rmDevice = m_rm.CreateDeviceFromClipper(m_ddClip, d3dg, ddsd.lWidth, ddsd.lHeight)
        Set m_rmViewport = m_rm.CreateViewport(m_rmDevice, m_rmFrameCamera, 0, 0, ddsd.lWidth, ddsd.lHeight)
        
        ResizeWindowedDevice = True
        Exit Function
    
    End If
    
    
    If UCase(d3dg) = "IID_IDIRECT3DHALDEVICE" Then
        memflags = DDSCAPS_VIDEOMEMORY
    Else
        memflags = DDSCAPS_SYSTEMMEMORY
    End If

    
    'CreateBacksurface
    ddsd.lFlags = DDSD_CAPS Or DDSD_HEIGHT Or DDSD_WIDTH
    ddsd.ddsCaps.lCaps = DDSCAPS_OFFSCREENPLAIN Or DDSCAPS_3DDEVICE Or memflags

           
    m_errorReason = "RMCanvas: unable to create backbuffer for current window size - try setting Use3DHardware=FALSE"
    Set m_backBuffer = m_dd.CreateSurface(ddsd)
    
    m_errorReason = "RMCanvas: unable to create RM Device or Viewport for current window size"
    Set m_rmViewport = Nothing
    Set m_rmDevice = Nothing
    Set m_rmDevice = m_rm.CreateDeviceFromSurface(d3dg, m_dd, m_backBuffer, 0)
    Set m_rmViewport = m_rm.CreateViewport(m_rmDevice, m_rmFrameCamera, 0, 0, ddsd.lWidth, ddsd.lHeight)
    
        
    ResizeWindowedDevice = True
    RaiseEvent RestoreSurfaces
    Exit Function
    
errOut:

    Err.Clear
    Set m_rmDevice = Nothing
    Set m_rmViewport = Nothing
    Set m_backBuffer = Nothing
    
    ResizeWindowedDevice = False
    m_binit = False


End Function


'-============================================================
' InitFullScreen
'-============================================================
Public Function InitFullScreen(ddrawguid As String, d3dguid As String, w As Long, h As Long, bpp As Long) As Boolean
    
    On Local Error GoTo errOut
    
    m_binit = False
    m_errorReason = ""
    
    CloseFSWindow
    Set frmFSWindow = New FSWindow
    frmFSWindow.Show
    m_hwnd = frmFSWindow.hwnd

    'get rid of our current rm device..
    Cleanup
    
    'make sure fs window is up
    
    DoEvents
    
    
    m_strDDGuid = ddrawguid
    If d3dguid = "" Then m_strD3DGuid = "IID_IDirect3DRGBDevice"
    
    'DirectDrawCreate
    m_errorReason = "RMCanvas: failed  on  DirectDraw Object Create for given ddrawguid"
    Set m_dd = dx.DirectDraw4Create(m_strDDGuid)
    
    'Set cooperative level
    m_errorReason = "RMCanvas: failed  on SetCooperativeLevel for fullscreen operation"
    m_dd.SetCooperativeLevel m_hwnd, DDSCL_ALLOWMODEX Or DDSCL_FULLSCREEN Or DDSCL_NOWINDOWCHANGES Or DDSCL_EXCLUSIVE

    'set the display mode
    If w <> 0 And h <> 0 And bpp <> 0 Then
        m_errorReason = "RMCanvas: Unable to set full screen display mode at requested w h and bpp"
        m_dd.SetDisplayMode w, h, bpp, 0, DDSDM_DEFAULT
    End If


    'create Flipping Surfaces - one front and 1 back buffer
    Dim ddsd As DDSURFACEDESC2
    ddsd.lFlags = DDSD_CAPS Or DDSD_BACKBUFFERCOUNT
    ddsd.ddsCaps.lCaps = DDSCAPS_PRIMARYSURFACE Or DDSCAPS_FLIP Or DDSCAPS_COMPLEX Or DDSCAPS_VIDEOMEMORY Or DDSCAPS_3DDEVICE
    ddsd.lBackBufferCount = 1
    m_errorReason = "RMCanvas: unable to create filipable surface for fullscreen operation"
    Set m_frontBuffer = m_dd.CreateSurface(ddsd)
    
    'Setup a clipper
    m_errorReason = "RMCanvas: failed to setup the clipper"
    Set m_ddClip = m_dd.CreateClipper(0)
    m_ddClip.SetHWnd m_hwnd
    m_frontBuffer.SetClipper m_ddClip
            
    'Get backbuffer
    Dim caps As DDSCAPS2
    caps.lCaps = DDSCAPS_BACKBUFFER
    m_errorReason = "RMCanvas: unable to get the fullscreen backbuffer"
    Set m_backBuffer = m_frontBuffer.GetAttachedSurface(caps)
        
    'get backbuffer description
    Dim ddsd2 As DDSURFACEDESC2
    m_errorReason = "RMCanvas: unable to get the fullscreen backbuffer description"
    m_backBuffer.GetSurfaceDesc ddsd2
    
    
    'see if they turned hw off
    If m_bUseSoftwareOnly Then
        m_strD3DGuid = "IID_IDirect3DRGBDevice"
    Else
        m_strD3DGuid = d3dguid
    End If
    
    'create the rm device from surface
    m_errorReason = "RMCanvas: unable to create the Retained Mode device - try a smaller resolution or try setting  Use3DHardware=false"
    Set m_rmDevice = m_rm.CreateDeviceFromSurface(m_strD3DGuid, m_dd, m_backBuffer, D3DRMDEVICE_DEFAULT)
    Set m_rmViewport = m_rm.CreateViewport(m_rmDevice, m_rmFrameCamera, 0, 0, ddsd2.lWidth, ddsd2.lHeight)
    
    SetDeviceDefaults
    
    m_binit = True
    m_bfullscreen = True
    InitFullScreen = True
    Exit Function

errOut:
    m_binit = False
    Cleanup
    Exit Function
End Function


'Run Time R/O access properties

'-============================================================
' Dx
'-============================================================
Public Function dx() As DirectX7
    Set dx = m_dx
End Function

'-============================================================
' DDraw
'-============================================================
Public Function DDraw() As DirectDraw4
    Set DDraw = m_dd
End Function

'-============================================================
' BackBuffer
'-============================================================
Public Function BackBuffer() As DirectDrawSurface4
    Set BackBuffer = m_backBuffer
End Function

'-============================================================
' D3drm
'-============================================================
Public Function D3DRM() As Direct3DRM3
    Set D3DRM = m_rm
End Function

'-============================================================
' Device
'-============================================================
Public Function Device() As Direct3DRMDevice3
    Set Device = m_rmDevice
End Function

'-============================================================
' Viewport
'-============================================================
Public Function Viewport() As Direct3DRMViewport2
    Set Viewport = m_rmViewport
End Function
 
'-============================================================
' DirLightFrame
'-============================================================
Public Function DirLightFrame() As Direct3DRMFrame3
    Set DirLightFrame = m_rmFrameDirLight
End Function
 
'-============================================================
' SceneFrame
'-============================================================
Public Function SceneFrame() As Direct3DRMFrame3
    Set SceneFrame = m_rmFrameScene
End Function
 
'-============================================================
' CameraFrame
'-============================================================
Public Function CameraFrame() As Direct3DRMFrame3
    Set CameraFrame = m_rmFrameCamera
End Function
 
'-============================================================
' DirLight
'-============================================================
Public Function DirLight() As Direct3DRMLight
    Set DirLight = m_rmDirLight
End Function

'-============================================================
' AmbientLight
'-============================================================
Public Function AmbientLight() As Direct3DRMLight
    Set AmbientLight = m_rmAmbientLight
End Function
 
 
'-============================================================
' Use3DHardware
'-============================================================
Property Let Use3DHardware(b As Boolean)
    m_bUseSoftwareOnly = Not b
End Property

Property Get Use3DHardware() As Boolean
    Use3DHardware = Not m_bUseSoftwareOnly
End Property


'-============================================================
' UseBackbuffer
'-============================================================
Property Let UseBackbuffer(b As Boolean)
    m_bCreateFromClipper = Not b
End Property

Property Get UseBackbuffer() As Boolean
    UseBackbuffer = Not m_bCreateFromClipper
End Property



'-============================================================
' LastError
'-============================================================
Property Get LastError() As String
     LastError = m_errorReason
End Property


'-============================================================
' DirectDrawGuid
'-============================================================
Property Get DirectDrawGuid() As String
     DirectDrawGuid = m_strDDGuid
End Property

'-============================================================
' Direct3DGuid
'-============================================================
Property Get Direct3DGuid() As String
     Direct3DGuid = m_strD3DGuid
End Property




 
'- Runtime only List Functions
'

'-============================================================
' Devices
'-============================================================
Public Function Devices(Optional ddrawguid = "") As Direct3DEnumDevices
    On Local Error GoTo exitOut:
    Dim dd As DirectDraw7
    Dim d3d As Direct3D7
    
    Set dd = dx.DirectDrawCreate(CStr(ddrawguid))
        
    Set d3d = dd.GetDirect3D()
    
    Set Devices = d3d.GetDevicesEnum()
    
    Set dd = Nothing
    Set d3d = Nothing
    Exit Function
exitOut:

End Function

'-============================================================
' VideoCards
'-============================================================
Public Function VideoCards() As DirectDrawEnum
    Set VideoCards = m_dx.GetDDEnum()
End Function

'-============================================================
' DisplayModes
'-============================================================
Public Function DisplayModes(Optional ddrawguid = "") As DirectDrawEnumModes
    On Local Error GoTo exitOut
    Dim dd As DirectDraw4
    Set dd = dx.DirectDraw4Create(CStr(ddrawguid))
    Dim ddsd As DDSURFACEDESC2
    Set DisplayModes = dd.GetDisplayModesEnum(0, ddsd)
    Set dd = Nothing
exitOut:
End Function


'-============================================================
' GetBltRect
'-============================================================
Public Function GetBltRect(top As Long, left As Long, bottom As Long, right As Long)
        Dim rc As RECT
        GetWindowRect m_hwnd, rc
        left = 0
        right = rc.right - rc.left
        top = 0
        bottom = rc.bottom - rc.top
        
End Function

'-============================================================
' IsFullScreen
'-============================================================
Property Get IsFullScreen() As Boolean
    IsFullScreen = m_bfullscreen
End Property





'-============================================================
' SceneSpeed
'-============================================================
Public Property Get SceneSpeed() As Single
    SceneSpeed = m_scenespeed
End Property

Public Property Let SceneSpeed(s As Single)
    m_scenespeed = s
End Property



'-============================================================
' FPS
'-============================================================
Public Property Get FPS() As Single
    FPS = m_fps
End Property


'-============================================================
' Update
'-============================================================
Public Sub Update()
    Tick
    Render
End Sub



'-============================================================
' Render
'-============================================================
Public Sub Render()
    
    On Local Error GoTo errOut
    If m_binit = False Then Exit Sub
    
    Dim t As Long
    Dim delta As Single
    Dim r As RECT
    Static fcount As Long
    
    t = dx.TickCount()
    

    
    
    
    m_rmViewport.Clear D3DRMCLEAR_ZBUFFER Or D3DRMCLEAR_TARGET

    If Not Background Is Nothing Then
        m_backBuffer.Blt m_emptyrect, Background, m_emptyrect, DDBLT_WAIT
    End If

    RaiseEvent ViewportClear
    
    m_rmViewport.Render m_rmFrameScene

    RaiseEvent PostRender

    m_rmDevice.Update

   
    
    If m_bfullscreen Then
            m_frontBuffer.Flip Nothing, DDFLIP_WAIT
    Else
        If m_bCreateFromClipper = False Then
            Call GetWindowRect(m_hwnd, r)
            m_frontBuffer.Blt r, m_backBuffer, m_emptyrect, DDBLT_WAIT
        End If
    End If
    
    
    
    fcount = fcount + 1
    If fcount = 30 Then
        t = dx.TickCount()
        m_fps = 30000 / (t - m_lastFPS)
        fcount = 0
        m_lastFPS = t
    End If
     
    
errOut:
    
End Sub

'-============================================================
' hwnd
'-============================================================
Public Function hwnd() As Long
    hwnd = m_hwnd
End Function


'-============================================================
' Tick
'-============================================================
Public Sub Tick()
    
    On Local Error GoTo errOut
    If m_binit = False Then Exit Sub
    
    Dim t As Long
    Dim delta As Single
    
    t = dx.TickCount()
    
    If m_lastRMMove <> 0 Then
        delta = (t - m_lastRMMove) * m_scenespeed / 1000
        m_rmFrameScene.Move delta
        RaiseEvent SceneMove(delta)
    End If
    m_lastRMMove = t
    
errOut:
    
End Sub

'-============================================================
' UserControl_InitProperties
'-============================================================
Private Sub UserControl_InitProperties()
    UserControl_Resize
End Sub


'-============================================================
' UserControl_Show
'-============================================================
Private Sub UserControl_Show()
    If m_binit = False Then Exit Sub
    m_rmDevice.HandleActivate 0
    m_rmDevice.HandlePaint UserControl.hDC
End Sub

    
'-============================================================
' UserControl_Resize
'-============================================================
Private Sub UserControl_Resize()
    on local error resume next    
    
    Dim b As Boolean
    
    If m_binit = False Then
        Picture1.width = UserControl.ScaleWidth
        Picture1.height = UserControl.ScaleHeight
        Exit Sub
    End If
    
    
    
    'full screen apps shouldnt resize
    If m_bfullscreen Then Exit Sub
    
    'tell others functions not to try and render during a resize
    m_bResizing = True

    
    SaveDeviceViewportCharacteristics
    
   If Not m_bUseSoftwareOnly Then
        b = InitWindowed(m_strDDGuid, "IID_IDirect3DHALDevice")
    End If
    If Not b Then
       b = InitWindowed(m_strDDGuid, "IID_IDirect3DRGBDevice")
    End If
    
    RestoreDeviceViewportCharacteristics
    
    'let others functions render
    m_bResizing = False
    
    'update the display
    UserControl_Paint
    
    
End Sub

'-============================================================
' UserControl_Initialize
'-============================================================
Private Sub UserControl_Initialize()
    Dim b As Boolean
    
    m_bCreateFromClipper = TRUE
    m_scenespeed = 30
    RotateRadius = 100
    
    b = InitSceneGraph()
    If Not b Then
        RaiseEvent DirecXNotInstalled
        Exit Sub
    End If
    
    
    UserControl_Resize
End Sub

'-============================================================
' UserControl_Terminate
'-============================================================
Private Sub UserControl_Terminate()
    CloseFSWindow
    
    Cleanup
    CleanupRMObjects
    m_binit = False
    
End Sub


'-============================================================
' CloseFSWindow
'-============================================================
Private Sub CloseFSWindow()
    On Local Error Resume Next
    Unload frmFSWindow
    Set frmFSWindow = Nothing
End Sub


'-============================================================
' Marshall full screen events
'-============================================================
Private Sub frmFSWindow_KeyDown(keyCode As Integer, Shift As Integer)
    RaiseEvent KeyDown(keyCode, Shift)
End Sub
Private Sub frmFSWindow_KeyPress(KeyAscii As Integer)
    RaiseEvent KeyPress(KeyAscii)
End Sub
Private Sub frmFSWindow_KeyUp(keyCode As Integer, Shift As Integer)
    RaiseEvent KeyUp(keyCode, Shift)
End Sub
Private Sub frmFSWindow_MouseDown(Button As Integer, Shift As Integer, X As Single, Y As Single)
    m_bMouseDown = True
    RotateFromXY CInt(X), CInt(Y), True
    RaiseEvent MouseDown(Button, Shift, X, Y)
End Sub
Private Sub frmFSWindow_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
    If m_bMouseDown Then RotateFromXY CInt(X), CInt(Y), False
    RaiseEvent MouseMove(Button, Shift, X, Y)
End Sub
Private Sub frmFSWindow_MouseUp(Button As Integer, Shift As Integer, X As Single, Y As Single)
    m_bMouseDown = False
    RaiseEvent MouseUp(Button, Shift, X, Y)
End Sub
Private Sub frmFSWindow_Click()
    RaiseEvent Click
End Sub
Private Sub frmFSWindow_DblClick()
    RaiseEvent DblClick
End Sub
Private Sub frmFSWindow_Paint()
    UserControl_Paint
End Sub


'-============================================================
' Marshall windowed events
'-============================================================
Private Sub UserControl_KeyDown(keyCode As Integer, Shift As Integer)
    RaiseEvent KeyDown(keyCode, Shift)
End Sub

Private Sub UserControl_KeyPress(KeyAscii As Integer)
    RaiseEvent KeyPress(KeyAscii)
End Sub

Private Sub UserControl_KeyUp(keyCode As Integer, Shift As Integer)
    RaiseEvent KeyUp(keyCode, Shift)
End Sub

Private Sub UserControl_MouseDown(Button As Integer, Shift As Integer, X As Single, Y As Single)
    m_bMouseDown = True
    RotateFromXY CInt(X), CInt(Y), True
    RaiseEvent MouseDown(Button, Shift, X, Y)
End Sub

Private Sub UserControl_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
    If m_bMouseDown Then RotateFromXY CInt(X), CInt(Y), False
    RaiseEvent MouseMove(Button, Shift, X, Y)
End Sub

Private Sub UserControl_MouseUp(Button As Integer, Shift As Integer, X As Single, Y As Single)
    m_bMouseDown = False
    RaiseEvent MouseUp(Button, Shift, X, Y)
End Sub

Private Sub UserControl_Click()
    RaiseEvent Click
End Sub

Private Sub UserControl_DblClick()
    RaiseEvent DblClick
End Sub

Private Sub UserControl_Paint()
    
    If m_binit = False Then Exit Sub
    RaiseEvent Paint
    m_rmDevice.HandleActivate 0
    m_rmDevice.HandlePaint m_hwnd
    Render
    
End Sub



'-============================================================
' Cleanup objects that can hold onto vmem
'-============================================================

Private Sub Cleanup()
    Err.Clear
    On Local Error Resume Next
    m_dd.RestoreDisplayMode
    m_dd.SetCooperativeLevel m_hwnd, DDSCL_NORMAL
    Set m_backBuffer = Nothing
    Set m_frontBuffer = Nothing
    Set m_dd = Nothing
    Set m_ddClip = Nothing
    Set m_rmViewport = Nothing
    Set m_rmDevice = Nothing
    m_bfullscreen = False
    m_binit = False
End Sub

'-============================================================
' Cleanup rest of RM objects
'-============================================================
Private Sub CleanupRMObjects()
    Set m_rmFrameCamera = Nothing
    Set m_rmFrameScene = Nothing
    Set m_rmFrameDirLight = Nothing
    Set m_rmFrameAmbientLight = Nothing
    Set m_rmDirLight = Nothing
    Set m_rmAmbientLight = Nothing
End Sub

    
    


'-====================================================
' RestoreDeviceViewportCharacteristics
'
' when the viewport is destroyed for whatever reason (resize)
' this function allows us to retain the characteristics
' of the viewport we just destroyed
'-====================================================
Private Sub RestoreDeviceViewportCharacteristics()
    With m_DevInfo
        m_rmDevice.SetDither .bDither
        m_rmDevice.SetName .Name
        m_rmDevice.SetQuality .Quality
        m_rmDevice.SetRenderMode .RenderMode
        m_rmDevice.SetShades .Shades
        m_rmDevice.SetTextureQuality .TexQ
    End With
    With m_ViewInfo
        m_rmViewport.SetBack .Back
        m_rmViewport.SetField .Field
        m_rmViewport.SetFront .Front
        m_rmViewport.SetName .Name
        m_rmViewport.SetProjection .Projection
        m_rmViewport.SetPlane .left, .right, .bottom, .top
        m_rmViewport.SetUniformScaling .scaling
    End With
End Sub

'-====================================================
' SaveDeviceViewportCharacteristics
'
' we need to retain certain characteristics about the
' viewport and device so that they look the same
' when recreated after a resize
'-====================================================
Private Sub SaveDeviceViewportCharacteristics()
    
    With m_DevInfo
        .bDither = m_rmDevice.GetDither
        .Name = m_rmDevice.GetName
        .Quality = m_rmDevice.GetQuality
        .RenderMode = m_rmDevice.GetRenderMode
        .Shades = m_rmDevice.GetShades
        .TexQ = m_rmDevice.GetTextureQuality
        .WFoptions = m_rmDevice.GetWireframeOptions
    End With
    With m_ViewInfo
        .Back = m_rmViewport.GetBack
        .Field = m_rmViewport.GetField
        .Front = m_rmViewport.GetFront
        .Name = m_rmViewport.GetName
        .Projection = m_rmViewport.GetProjection
        .scaling = m_rmViewport.GetUniformScaling
        m_rmViewport.GetPlane .left, .right, .bottom, .top
        
    End With
End Sub



'-====================================================
' SetDeviceDefaults
'-====================================================

Private Sub SetDeviceDefaults()
    m_rmDevice.SetQuality D3DRMRENDER_GOURAUD
End Sub


'-====================================================
' InitSceneGraph
'
' create default lighting and cameras
'-====================================================
Private Function InitSceneGraph() As Boolean
    On Local Error GoTo errOut
    'create a skeletal scene graph
    Set m_rm = m_dx.Direct3DRMCreate()
    Set m_rmFrameScene = m_rm.CreateFrame(Nothing)
    Set m_rmFrameCamera = m_rm.CreateFrame(m_rmFrameScene)
    m_rmFrameCamera.SetPosition Nothing, 0, 0, -10
    
    
    'create a bright directional light
    Set m_rmFrameDirLight = m_rm.CreateFrame(m_rmFrameScene)
    Set m_rmDirLight = m_rm.CreateLightRGB(D3DRMLIGHT_DIRECTIONAL, 1, 1, 1)
    
    'create a dull ambient light
    Set m_rmAmbientLight = m_rm.CreateLightRGB(D3DRMLIGHT_AMBIENT, 0.2, 0.2, 0.2)
    
    'add the lights to the scene graph
    m_rmFrameDirLight.AddLight m_rmDirLight
    m_rmFrameScene.AddLight m_rmAmbientLight
    m_rmFrameDirLight.SetPosition Nothing, 5, 5, -5
    m_rmFrameDirLight.LookAt m_rmFrameScene, Nothing, 0
       
    InitSceneGraph = True
    Exit Function
errOut:
    InitSceneGraph = False
End Function



'-============================================================
' RotateFromXY
'-============================================================

Public Sub RotateFromXY(X As Integer, Y As Integer, bStartPos As Boolean)
    
    If RotateFrame Is Nothing Then Exit Sub
    If RotateRadius = 0 Then Exit Sub
    
    If bStartPos Then
        m_lastX = X
        m_lastY = Y
        Exit Sub
    End If

    If RotateMethod = ROTATE_GLOBE Then
        RotateGlobe X, Y
    Else
        RotateTrackBall X, Y
    End If
    
    Update
End Sub

'-============================================================
' RotateTrackBall
'-============================================================
 
Private Sub RotateTrackBall(X As Integer, Y As Integer)

    On Local Error GoTo errOut:
    
    
    Dim delta_x As Single, delta_y As Single
    Dim delta_r As Single, radius As Single, denom As Single, angle As Single

    ' rotation axis in camcoords, worldcoords, sframecoords
    Dim axisC As D3DVECTOR
    Dim wc As D3DVECTOR
    Dim axisS As D3DVECTOR
    Dim base As D3DVECTOR
    Dim origin As D3DVECTOR

    delta_x = X - m_lastX
    delta_y = Y - m_lastY
    m_lastX = X
    m_lastY = Y


    delta_r = Sqr(delta_x * delta_x + delta_y * delta_y)
    radius = RotateRadius
    denom = Sqr(radius * radius + delta_r * delta_r)

    If (delta_r = 0 Or denom = 0) Then Exit Sub
    angle = (delta_r / denom)

    axisC.X = (-delta_y / delta_r)
    axisC.Y = (-delta_x / delta_r)
    axisC.z = 0

    m_rmFrameCamera.Transform wc, axisC
    RotateFrame.InverseTransform axisS, wc

    m_rmFrameCamera.Transform wc, origin
    RotateFrame.InverseTransform base, wc

    axisS.X = axisS.X - base.X
    axisS.Y = axisS.Y - base.Y
    axisS.z = axisS.z - base.z

    RotateFrame.AddRotation D3DRMCOMBINE_BEFORE, axisS.X, axisS.Y, axisS.z, angle
    
errOut:

End Sub


'-============================================================
' RotateGlobe
'-============================================================
Private Sub RotateGlobe(newx As Integer, newy As Integer)
   On Local Error GoTo errOut:
   
   Dim X As Integer
   Dim Y As Integer
   Dim dx As Integer
   Dim dy As Integer
   
    dx = m_lastX - newx
    dy = m_lastY - newy
    
    X = m_lastX
    Y = m_lastY
    
    m_lastX = 0
    RotateFrame.AddRotation D3DRMCOMBINE_BEFORE, 0, 1, 0, (3.14 / (10 * RotateRadius)) * dx
    RotateTrackBall 0, newy
    
    m_lastX = newx
                    
errOut:

End Sub


'-============================================================
' GetBoundingBox
'-============================================================
Public Sub GetBoundingBox(frame As Direct3DRMFrame3, ByRef xmin As Single, ByRef ymin As Single, ByRef zmin As Single, ByRef xmax As Single, ByRef ymax As Single, ByRef zmax As Single)
    Dim box1 As D3DRMBOX
    Dim mb As Direct3DRMMeshBuilder3
    
    Set mb = m_rm.CreateMeshBuilder()
    mb.AddFrame frame
    
    mb.GetBox box1
    
    xmin = box1.Min.X
    ymin = box1.Min.Y
    zmin = box1.Min.z
    
    xmax = box1.Max.X
    ymax = box1.Max.Y
    zmax = box1.Max.z
        
End Sub


'-============================================================
' CreateSheetMesh
'-============================================================

Public Function CreateSheetMesh(nSides As Integer, height As Single, width As Single) As Direct3DRMMeshBuilder3
    Dim m As Direct3DRMMeshBuilder3
    Dim f As Direct3DRMFace2
    Set m = m_rm.CreateMeshBuilder()
        
    Dim dx  As Single
    Dim dy  As Single
    dy = height / 2
    dx = width / 2
    
    'Front Face
    Set f = m_rm.CreateFace()
    f.AddVertex dx, dy, 0
    f.AddVertex dx, -dy, 0
    f.AddVertex -dx, -dy, 0
    f.AddVertex -dx, dy, 0
    m.AddFace f
    
    m.SetTextureCoordinates 3, 0, 0
    m.SetTextureCoordinates 2, 0, 1
    m.SetTextureCoordinates 1, 1, 1
    m.SetTextureCoordinates 0, 1, 0
    
    
    If nSides > 1 Then
        'Back Face
        Set f = m_rm.CreateFace()
        f.AddVertex -dx, dy, 0
        f.AddVertex -dx, -dy, 0
        f.AddVertex dx, -dy, 0
        f.AddVertex dx, dy, 0
        m.AddFace f
    
        m.SetTextureCoordinates 7, 0, 0
        m.SetTextureCoordinates 6, 0, 1
        m.SetTextureCoordinates 5, 1, 1
        m.SetTextureCoordinates 4, 1, 0
    
    End If
    
        
    m.SetName "Sheet" + CStr(m_createid)
    m_createid = m_createid + 1
    
    Set CreateSheetMesh = m

End Function

'-============================================================
' CreateBoxMesh
'-============================================================
Public Function CreateBoxMesh(width As Single, height As Single, depth As Single) As Direct3DRMMeshBuilder3
    
    
    Dim m As Direct3DRMMeshBuilder3
    Dim f As Direct3DRMFace2
    Set m = m_rm.CreateMeshBuilder()
    
    Dim dx As Single
    Dim dy As Single
    Dim dz As Single
    
    dx = width / 2
    dy = height / 2
    dz = depth / 2
    
    'Front Face
    Set f = m_rm.CreateFace()
    f.AddVertex dx, dy, -dz
    f.AddVertex dx, -dy, -dz
    f.AddVertex -dx, -dy, -dz
    f.AddVertex -dx, dy, -dz
    m.AddFace f
    
    'Back Face
    Set f = m_rm.CreateFace()
    f.AddVertex -dx, dy, dz
    f.AddVertex -dx, -dy, dz
    f.AddVertex dx, -dy, dz
    f.AddVertex dx, dy, dz
    m.AddFace f
    
    'Right face
    Set f = m_rm.CreateFace()
    f.AddVertex dx, dy, dz
    f.AddVertex dx, -dy, dz
    f.AddVertex dx, -dy, -dz
    f.AddVertex dx, dy, -dz
    m.AddFace f

    'Left face
    Set f = m_rm.CreateFace()
    f.AddVertex -dx, -dy, dz
    f.AddVertex -dx, dy, dz
    f.AddVertex -dx, dy, -dz
    f.AddVertex -dx, -dy, -dz
    m.AddFace f
    
    'Top face
    Set f = m_rm.CreateFace()
    f.AddVertex dx, dy, -dz
    f.AddVertex -dx, dy, -dz
    f.AddVertex -dx, dy, dz
    f.AddVertex dx, dy, dz
    m.AddFace f
    
    
    'Bottom face
    Set f = m_rm.CreateFace()
    f.AddVertex dx, -dy, dz
    f.AddVertex -dx, -dy, dz
    f.AddVertex -dx, -dy, -dz
    f.AddVertex dx, -dy, -dz
    m.AddFace f
    
    m.SetTextureCoordinates 3, 0, 0
    m.SetTextureCoordinates 2, 0, 1
    m.SetTextureCoordinates 1, 1, 1
    m.SetTextureCoordinates 0, 1, 0
    
    m.SetTextureCoordinates 7, 0, 0
    m.SetTextureCoordinates 6, 0, 1
    m.SetTextureCoordinates 5, 1, 1
    m.SetTextureCoordinates 4, 1, 0
    
    m.SetTextureCoordinates 11, 0, 0
    m.SetTextureCoordinates 10, 0, 1
    m.SetTextureCoordinates 9, 1, 1
    m.SetTextureCoordinates 8, 1, 0
    
    
    m.SetTextureCoordinates 12, 0, 1
    m.SetTextureCoordinates 13, 1, 1
    m.SetTextureCoordinates 14, 1, 0
    m.SetTextureCoordinates 15, 0, 0
    
    m.SetTextureCoordinates 19, 0, 0
    m.SetTextureCoordinates 18, 0, 1
    m.SetTextureCoordinates 17, 1, 1
    m.SetTextureCoordinates 16, 1, 0
                                
    m.SetName "Box" + CStr(m_createid)
    m_createid = m_createid + 1
    
    Set CreateBoxMesh = m
    
End Function


'-============================================================
' PickTopMesh
'-============================================================

Public Function PickTopMesh(X As Long, Y As Long) As Direct3DRMMeshBuilder3
    On Local Error GoTo errOut
    Dim pickarray As Direct3DRMPickArray
    Dim mb As Direct3DRMMeshBuilder3
    Dim desc As D3DRMPICKDESC
    Set pickarray = m_rmViewport.Pick(X, Y)
    If pickarray.GetSize() = 0 Then Exit Function
    Set mb = pickarray.GetPickVisual(0, desc)
    Set PickTopMesh = mb
errOut:
End Function

'-============================================================
' PickTopFrame
'-============================================================
Public Function PickTopFrame(X As Long, Y As Long) As Direct3DRMFrame3
    On Local Error GoTo errOut
    Dim pickarray As Direct3DRMPickArray
    Dim f As Direct3DRMFrame3
    Dim fa As Direct3DRMFrameArray
    Dim desc As D3DRMPICKDESC
    Set pickarray = m_rmViewport.Pick(X, Y)
    If pickarray.GetSize() = 0 Then Exit Function

    Set fa = pickarray.GetPickFrame(0, desc)
    Set f = fa.GetElement(fa.GetSize() - 1)
    Set PickTopFrame = f
errOut:

End Function


'-============================================================
' CreateUpdateableTexture
'-============================================================
Public Function CreateUpdateableTexture(w As Long, h As Long, sfile As String) As Direct3DRMTexture3
    On Local Error GoTo errOut
    Dim sLoadFile As String
    Dim ddsd As DDSURFACEDESC2
    Dim SurfaceObject As DirectDrawSurface4
    Dim out As Direct3DRMTexture3
    Dim Init As Boolean
    
    ddsd.lFlags = DDSD_CAPS
    ddsd.ddsCaps.lCaps = DDSCAPS_OFFSCREENPLAIN Or DDSCAPS_SYSTEMMEMORY
    If (w <> 0) Then
        ddsd.lFlags = DDSD_CAPS Or DDSD_WIDTH Or DDSD_HEIGHT
        ddsd.lWidth = w
        ddsd.lHeight = h
    End If
    
    If sfile = "" Then
        Set SurfaceObject = m_dd.CreateSurface(ddsd)
    Else
        Set SurfaceObject = m_dd.CreateSurfaceFromFile(sfile, ddsd)
    End If
    
    Set out = m_rm.CreateTextureFromSurface(SurfaceObject)
    Set CreateUpdateableTexture = out
    
    
errOut:
    Set SurfaceObject = Nothing
End Function


'-============================================================
' LoadBackground
'-============================================================
Function LoadBackground(sfile As String) As Boolean
    On Local Error GoTo errOut
    Dim ddsd As DDSURFACEDESC2
    Set Background = m_dd.CreateSurfaceFromFile(sfile, ddsd)
    LoadBackground = True
errOut:
End Function

