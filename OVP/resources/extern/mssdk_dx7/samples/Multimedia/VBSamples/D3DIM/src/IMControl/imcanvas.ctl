VERSION 5.00
Begin VB.UserControl IMCanvas 
   ClientHeight    =   3600
   ClientLeft      =   0
   ClientTop       =   0
   ClientWidth     =   4800
   ScaleHeight     =   240
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   320
   Begin VB.PictureBox Picture1 
      Height          =   3135
      Left            =   0
      ScaleHeight     =   3075
      ScaleWidth      =   3795
      TabIndex        =   0
      Top             =   0
      Width           =   3855
   End
End
Attribute VB_Name = "IMCanvas"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Option Explicit
Dim m_dx As New DirectX7
Dim m_dd As DirectDraw7
Dim m_d3d As Direct3D7
Dim m_dev As Direct3DDevice7
Dim m_ddClipper As DirectDrawClipper
Dim m_frontSurface As DirectDrawSurface7
Dim m_backSurface As DirectDrawSurface7
Dim m_ZBuffer As DirectDrawSurface7
Dim m_DevEnum As Direct3DEnumDevices

Dim m_DevDesc As D3DDEVICEDESC7
Dim m_ViewPortDesc As D3DVIEWPORT7
Dim m_backSurfaceDesc As DDSURFACEDESC2
Dim m_DDSDescPrim As DDSURFACEDESC2

Dim m_srcRect As RECT
Dim m_destRect As RECT
Dim m_backRect As RECT
Dim m_memFlags As Long
Dim m_binit As Boolean


Dim m_strDDGuid As String
Dim m_str3dDevGuid As String
Dim m_bIsfullscreen As Boolean
Dim m_fsH As Long
Dim m_fsW As Long
Dim m_fsbpp As Long
Dim m_bShowFps As Boolean
Dim m_bClearZ as Boolean

Public BackBufferClearValue As Long
Public EnableF5ResChange As Boolean

Public Enum TEXTUREFLAGS
 D3DTEXTR_DEFAULT = 0
 D3DTEXTR_TRANSPARENTBLACK = 1
 D3DTEXTR_TRANSPARENTWHITE = 2
End Enum

Dim WithEvents frmFSWindow As frmFullScreen
Attribute frmFSWindow.VB_VarHelpID = -1

Public Event KeyDown(KeyCode As Integer, Shift As Integer)
Public Event KeyPress(KeyAscii As Integer)
Public Event KeyUp(KeyCode As Integer, Shift As Integer)
Public Event MouseDown(Button As Integer, Shift As Integer, X As Single, Y As Single)
Public Event MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
Public Event MouseUp(Button As Integer, Shift As Integer, X As Single, Y As Single)
Public Event Click()
Public Event DblClick()
Public Event Paint()
Public Event NewDDraw()



'''''''''''''''''''''''''''''''''''''''
'- PUBLIC FUNCTIONS                   '
'''''''''''''''''''''''''''''''''''''''

Public Function StartWindowed() As Boolean
    
    m_str3dDevGuid = "IID_IDIRECT3DHALDEVICE"
    If InitWindowed("", m_str3dDevGuid) = False Then
        m_str3dDevGuid = "IID_IDIRECT3DRGBDEVICE"
        If InitWindowed("", m_str3dDevGuid) = False Then
            StartWindowed = False
            Exit Function
        End If
    End If
    StartWindowed = True
End Function


Public Function InitWindowed(sDDGuid As String, sD3DGuid As String) As Boolean
    Dim attempt As String
    On Local Error GoTo errOut
    
    m_binit = False
    
    
    Picture1.Visible = False
    If Not m_dd Is Nothing Then m_dd.RestoreDisplayMode
    If Not m_dd Is Nothing Then m_dd.SetCooperativeLevel 0, DDSCL_NORMAL
    
    If Not frmFSWindow Is Nothing Then
        Unload frmFSWindow
        Set frmFSWindow = Nothing
        DoEvents
    End If
    
    
    DoEvents
    
    Cleanup
    
    If sD3DGuid = "" Then sD3DGuid = "IID_IDirect3DRGBDevice"
    
    m_str3dDevGuid = sD3DGuid
    
    If UCase(sD3DGuid) = "IID_IDIRECT3DRGBDEVICE" Then
        m_memFlags = DDSCAPS_SYSTEMMEMORY
    ElseIf UCase(sD3DGuid) = "IID_IDIRECT3DHALDEVICE" Then
        m_memFlags = DDSCAPS_VIDEOMEMORY
    End If
    
    
    
    'DDRAWCREATE
    attempt = "create the directdraw object"
    Set m_dd = m_dx.DirectDrawCreate(sDDGuid)
    
    'SET COOPERATIVE LEVEL
    attempt = "set the cooperative level"
    m_dd.SetCooperativeLevel 0, DDSCL_NORMAL
    
    'GET FRONTBUFFER
    attempt = "get the screen surface"
    
    m_DDSDescPrim.ddsCaps.lCaps = DDSCAPS_PRIMARYSURFACE
    Set m_frontSurface = m_dd.CreateSurface(m_DDSDescPrim)
    
    'APPLY A CLIPPER TO OUR FRONT BUFFER
    attempt = "create and set Clipper"
    Set m_ddClipper = m_dd.CreateClipper(0&)
    m_ddClipper.SetHWnd UserControl.hWnd
    m_frontSurface.SetClipper m_ddClipper
        
    'CREATE BACKBUFFER
    attempt = "Create BackBuffer"
    CreateOffscreenBackBuffer
        
    'CREATE THE D3D OBJECT
    attempt = "GetDirect3D"
    Set m_d3d = m_dd.GetDirect3D
    
    'ATTATCH ZBUFER
    attempt = "Attatch Z Buffer"
    AttatchZBuffer


    'CREATE THE D3D DEVICE
    attempt = "Create the D3D Device"
    Set m_dev = m_d3d.CreateDevice(m_str3dDevGuid, m_backSurface)
    
    'SET THE VIEWPORT
    attempt = "Setup the viewport"
    With m_ViewPortDesc
        .lHeight = m_backSurfaceDesc.lHeight
        .lWidth = m_backSurfaceDesc.lWidth
        .minz = 0#
        .maxz = 1#
    End With
    m_dev.SetViewport m_ViewPortDesc
    
        
    'SETUP OUR SRC AND DEST RECTS FOR BLTS
    m_srcRect.Bottom = m_backSurfaceDesc.lHeight
    m_srcRect.Right = m_backSurfaceDesc.lWidth
    m_dx.GetWindowRect UserControl.hWnd, m_destRect
    
    InitWindowed = True
    m_binit = True
    
    
    Exit Function
    
errOut:
    Debug.Print "ERROR: " + attempt
    InitWindowed = False
    
End Function





Public Function InitFullScreen(sDDGuid As String, sD3DGuid As String, w As Long, h As Long, bpp As Long) As Boolean
    
    On Local Error GoTo errOut
    
    
    Dim attempt As String
    Dim e As Long
    
    m_binit = False
        
    Picture1.Visible = False
    
    
    If frmFSWindow Is Nothing Then Set frmFSWindow = New frmFullScreen
    
    frmFSWindow.Show
    DoEvents
    
    Cleanup
    
    If sD3DGuid = "" Then sD3DGuid = "IID_IDirect3DRGBDevice"
    
    m_str3dDevGuid = sD3DGuid
    
    m_memFlags = DDSCAPS_VIDEOMEMORY
    
    'DDRAWCREATE
    attempt = "create the directdraw object"
    Set m_dd = m_dx.DirectDrawCreate(sDDGuid)
    
        
    '- Setting the CooperativeLevel
    '  Modex allows us to change display modes
    '  Exclusive allows us to perform flip operations
    '  and indicates we dont want windows to get in the way
    attempt = "SetCooperativeLevel"
    'm_dd.SetCooperativeLevel frmfswindow.hWnd, DDSCL_NOWINDOWCHANGES Or DDSCL_FULLSCREEN Or DDSCL_ALLOWMODEX Or DDSCL_EXCLUSIVE
    m_dd.SetCooperativeLevel frmFSWindow.hWnd, DDSCL_FULLSCREEN Or DDSCL_ALLOWMODEX Or DDSCL_EXCLUSIVE
            
    '- SetDisplayMode
    attempt = "SetDisplayMode"
    If w <> 0 And h <> 0 And bpp <> 0 Then
        m_dd.SetDisplayMode w, h, bpp, 0, DDSDM_DEFAULT
    End If
        
    '- Get the SCREEN SURFACE and create a back buffer too
    '  the DDSCAPS_FLIP us to call flip and swap the
    '  front and back buffers for fast rendering
    attempt = "CreateScreenSurface"
    m_DDSDescPrim.lFlags = DDSD_CAPS Or DDSD_BACKBUFFERCOUNT
    m_DDSDescPrim.ddsCaps.lCaps = DDSCAPS_PRIMARYSURFACE Or DDSCAPS_FLIP Or DDSCAPS_COMPLEX Or DDSCAPS_VIDEOMEMORY Or DDSCAPS_3DDEVICE
    m_DDSDescPrim.lBackBufferCount = 1
    Set m_frontSurface = m_dd.CreateSurface(m_DDSDescPrim)
    
    
    
    '- Get the BACK SURFACE we render to
    '  since the back buffer is already attached
    '  all we need to do is get a reference to it
    Dim caps As DDSCAPS2
    caps.lCaps = DDSCAPS_BACKBUFFER
    attempt = "GetBackBuffer"
    Set m_backSurface = screenSurface.GetAttachedSurface(caps)
    
    '- Get some default info about the back surface
    m_backSurface.GetSurfaceDesc m_backSurfaceDesc
        
    
    'APPLY A CLIPPER TO OUR FRONT BUFFER
    'attempt = "create and set Clipper"
    'Set m_ddClipper = m_dd.CreateClipper(0&)
    'm_ddClipper.SetHWnd frmFullScreen.hWnd
    'm_frontSurface.SetClipper m_ddClipper
        
    
    'CREATE THE D3D OBJECT
    attempt = "GetDirect3D"
    Set m_d3d = m_dd.GetDirect3D
    
    AttatchZBuffer
        
    'CREATE THE D3D DEVICE
    attempt = "Create the D3D Device"
    Set m_dev = m_d3d.CreateDevice(m_str3dDevGuid, m_backSurface)
    
    'SET THE VIEWPORT
    attempt = "Setup the viewport"
    With m_ViewPortDesc
        .lHeight = m_backSurfaceDesc.lHeight
        .lWidth = m_backSurfaceDesc.lWidth
        .minz = 0#
        .maxz = 1#
    End With
    m_dev.SetViewport m_ViewPortDesc
    
        
    'SETUP OUR SRC AND DEST RECTS FOR BLTS
    m_srcRect.Bottom = m_backSurfaceDesc.lHeight
    m_srcRect.Right = m_backSurfaceDesc.lWidth
    m_dx.GetWindowRect frmFSWindow.hWnd, m_destRect
    
    
    
    
    
    '- Indicate our surfaces are setup for full screen operation
    m_bIsfullscreen = True
    m_binit = True
    InitFullScreen = True
    Exit Function

errOut:

    e = Err.Number
    Call m_dd.SetCooperativeLevel(0, DDSCL_NORMAL)
    m_dd.RestoreDisplayMode
    Debug.Print "Error " + attempt
    
    InitFullScreen = False
    
End Function


Public Sub SetDefaultTransformsLightsAndMaterials()

    Dim matWorld As D3DMATRIX, matView As D3DMATRIX, matProj As D3DMATRIX
    Dim Mat As D3DMATERIAL7
    Dim light As D3DLIGHT7
    Dim c As D3DCOLORVALUE
    
    m_dx.IdentityMatrix matWorld
    m_dx.IdentityMatrix matView
    m_dx.IdentityMatrix matProj
    Call m_dx.ViewMatrix(matView, MakeVector(0, 0, -5), MakeVector(0, 0, 0), MakeVector(0, 1, 0), 0)
    Call m_dx.ProjectionMatrix(matProj, 10, 1000, 3.141 / 4)
    
    m_dev.SetTransform D3DTRANSFORMSTATE_WORLD, matWorld
    m_dev.SetTransform D3DTRANSFORMSTATE_VIEW, matView
    m_dev.SetTransform D3DTRANSFORMSTATE_PROJECTION, matProj
     
    With c
        .a = 1
        .r = 1
        .g = 0.5
        .b = 0.5
    
    End With
    With light
        .dltType = D3DLIGHT_POINT
        .Ambient = c
        .diffuse = c
        .specular = c
        .position.Y = 100
    End With
        
    m_dev.SetLight 0, light
    m_dev.LightEnable 0, True
    
    With c
        .a = 0#
        .r = 4
        .g = 4
        .b = 4
    End With
    Mat.Ambient = c
    With c
        .a = 1
        .r = 0.5
        .g = 0.5
        .b = 0.5
    End With
    Mat.diffuse = c
    With c
        .a = 0#
        .r = 0.5
        .g = 0.5
        .b = 0.5
    End With
    Mat.specular = c
    Mat.emissive = c
    
    ' Set the material as the current material
    m_dev.SetMaterial Mat
    
    
End Sub
    
    
    
Public Sub Update()
    On Local Error Resume Next
    If m_binit = False Then Exit Sub
    Dim srcR As RECT
    Dim dstR As RECT
    
    If m_bShowFps Then UpdateStats
    
    If m_bIsfullscreen Then
        m_frontSurface.Flip Nothing, DDFLIP_WAIT
    Else
        srcR.Right = m_backSurfaceDesc.lWidth
        srcR.Bottom = m_backSurfaceDesc.lHeight
        m_dx.GetWindowRect UserControl.hWnd, dstR
        m_frontSurface.Blt dstR, m_backSurface, srcR, DDBLT_WAIT
    End If
End Sub

    
Public Sub ClearBackSurface()
    on local error resume next
    
    If m_binit = False Then Exit Sub
    
    Dim rSrc As RECT
    Dim i As Integer
    Dim recs(1) As D3DRECT
    
    recs(0).X2 = m_backSurfaceDesc.lWidth
    recs(0).Y2 = m_backSurfaceDesc.lHeight

    If  m_bClearZ then
	    m_dev.Clear 1, recs(), D3DCLEAR_TARGET or D3DCLEAR_ZBUFFER, BackBufferClearValue, 1, 0
    Else 
	    m_dev.Clear 1, recs(), D3DCLEAR_TARGET,BackBufferClearValue, 1, 0
    End If

End Sub

Public Sub SetViewPosition(fromX As Single, fromY As Single, fromZ As Single, atX As Single, atY As Single, atZ As Single, upX As Single, upY As Single, upZ As Single)
                
    Dim matView As D3DMATRIX
    Call m_dx.ViewMatrix(matView, MakeVector(fromX, fromY, fromZ), MakeVector(atX, atY, atZ), MakeVector(upX, upY, upZ), 0)
    If m_dev Is Nothing Then Exit Sub
    m_dev.SetTransform D3DTRANSFORMSTATE_VIEW, matView
    
End Sub

Public Sub SetViewFrustrum(nearZ As Single, farZ As Single, radian As Single)
            
    Dim matProj As D3DMATRIX
    
    Call m_dx.ProjectionMatrix(matProj, nearZ, farZ, radian)
    If m_dev Is Nothing Then Exit Sub
    m_dev.SetTransform D3DTRANSFORMSTATE_PROJECTION, matProj
    
End Sub

Public Function CreateTextureSurface(sFile As String, extraFlags As TEXTUREFLAGS, w As Long, h As Long)

    Dim bOK As Boolean
    Dim enumTex As Direct3DEnumPixelFormats
    Dim sLoadFile As String
    Dim i As Long
    Dim ddsd As DDSURFACEDESC2
    Dim SurfaceObject As DirectDrawSurface7
    Dim Init As Boolean
    
    ddsd.lFlags = DDSD_CAPS Or DDSD_TEXTURESTAGE Or DDSD_PIXELFORMAT
    If ((h <> 0) And (w <> 0)) Then
        ddsd.lFlags = ddsd.lFlags Or DDSD_HEIGHT Or DDSD_WIDTH
        ddsd.lHeight = h
        ddsd.lWidth = w
    End If
    
     
    Set enumTex = m_dev.GetTextureFormatsEnum()
    
   
    For i = 1 To enumTex.GetCount()
        bOK = True
        Call enumTex.GetItem(i, ddsd.ddpfPixelFormat)
            
        With ddsd.ddpfPixelFormat
            
            If .lRGBBitCount <> 16 Then bOK = False
            If .lFourCC <> 0 Then bOK = False
                        
            
            If ((D3DTEXTR_TRANSPARENTBLACK And extraFlags) _
                  Or (D3DTEXTR_TRANSPARENTWHITE And extraFlags)) Then
                  If (.lRGBAlphaBitMask = 0) Then bOK = False
                  'DDPF_ALPHAPIXELS
            Else
                  If (.lRGBAlphaBitMask <> 0) Then bOK = False
            End If
                  
            
        End With
        If bOK = True Then Exit For
    Next
    
    If bOK = False Then
        Debug.Print "Unable to find 16bit surface support on your hardware - exiting"
        Init = False
    End If
    
    If (((D3DTEXTR_TRANSPARENTBLACK And extraFlags) _
          Or (D3DTEXTR_TRANSPARENTWHITE And extraFlags))) Then
        ddsd.ddpfPixelFormat.lFlags = DDPF_ALPHAPIXELS Or DDPF_RGB
        ddsd.lFlags = ddsd.lFlags Or DDSD_PIXELFORMAT
        
    Else
        ddsd.ddpfPixelFormat.lFlags = DDPF_RGB
  
    End If
    
    If m_dev.GetDeviceGuid()="IID_IDirect3DHALDevice" then
	    ddsd.ddsCaps.lCaps = DDSCAPS_TEXTURE
	    ddsd.ddsCaps.lCaps2 = DDSCAPS2_TEXTUREMANAGE
	    ddsd.lTextureStage = 0
    Else
	    ddsd.ddsCaps.lCaps = DDSCAPS_TEXTURE or DDSCAPS_SYSTEMMEMORY
	    ddsd.ddsCaps.lCaps2 = 0
	    ddsd.lTextureStage = 0
    End If    

    If sFile = "" Then
        Set SurfaceObject = m_dd.CreateSurface(ddsd)
    Else
        Set SurfaceObject = m_dd.CreateSurfaceFromFile(sFile, ddsd)
    End If
    
    
    Set CreateTextureSurface = SurfaceObject
    
    If Not (((D3DTEXTR_TRANSPARENTBLACK = extraFlags) _
        Or (D3DTEXTR_TRANSPARENTWHITE = extraFlags)) _
        ) Then Exit Function

        
        
    Dim ddsd3 As DDSURFACEDESC2
    Dim mem() As Integer
    Dim lAlphaMask As Long
    Dim lRGBMask As Long
    Dim lColorKey As Long
    Dim c As Long
    Dim X As Long
    Dim Y As Long
    Dim r1 As RECT
    
    ReDim mem(ddsd.lHeight * ddsd.lPitch)
                        
    With ddsd.ddpfPixelFormat
        lAlphaMask = .lRGBAlphaBitMask
        lRGBMask = .lRBitMask Or .lGBitMask Or .lBBitMask
    End With
    If (extraFlags And D3DTEXTR_TRANSPARENTWHITE) Then
        lColorKey = lRGBMask     'color key on white
    End If
    If (extraFlags And D3DTEXTR_TRANSPARENTBLACK) Then
        lColorKey = 0                  'color key on black
    End If
    
    'pixel format should be 16 bit because thats what we selected
    Dim rl As RECT
    
    r1.Bottom = ddsd.lHeight
    r1.Right = ddsd.lWidth
    SurfaceObject.Lock r1, ddsd3, DDLOCK_WAIT, 0
    
    ' Add an opaque alpha value to each non-colorkeyed pixel
    For Y = 0 To ddsd3.lHeight - 1
        For X = 0 To ddsd3.lWidth - 1
            c = SurfaceObject.GetLockedPixel(X, Y)
            If c And lRGBMask <> lColorKey Then
                SurfaceObject.SetLockedPixel X, Y, c Or lAlphaMask
            End If
        Next
    Next

    SurfaceObject.Unlock r1
        
    
End Function







'''''''''''''''''''''''''''''''''''''''
'- PUBLIC Properties
'''''''''''''''''''''''''''''''''''''''
Property Get dx() As DirectX7
    Set dx = m_dx
End Property

Property Get Direct3d() As Direct3D7
    Set Direct3d = m_d3d
End Property


Property Get Direct3DDevice() As Direct3DDevice7
    Set Direct3DDevice = m_dev
End Property

Property Get DirectDraw() As DirectDraw7
    Set DirectDraw = m_dd
End Property

Property Get screenSurface() As DirectDrawSurface7
    Set screenSurface = m_frontSurface
End Property

Property Get backSurface() As DirectDrawSurface7
    Set backSurface = m_backSurface
End Property




'''''''''''''''''''''''''''''''''''''''
'- Private functions
'''''''''''''''''''''''''''''''''''''''
Private Sub Cleanup()

    Set m_dx = Nothing
    Set m_d3d = Nothing
    Set m_dev = Nothing
    Set m_DevEnum = Nothing
    Set m_ZBuffer = Nothing
    Set m_backSurface = Nothing
    Set m_frontSurface = Nothing
    Set m_ddClipper = Nothing
    Set m_dd = Nothing
    
    Dim emptydesc As DDSURFACEDESC2
    Dim devDesc As D3DDEVICEDESC7
    Dim viewDesc As D3DVIEWPORT7
    Dim rc As RECT
    
    m_DDSDescPrim = emptydesc
    m_backSurfaceDesc = emptydesc
    m_DevDesc = devDesc
    m_ViewPortDesc = viewDesc
    m_srcRect = rc
    m_destRect = rc
    m_backRect = rc
    m_bIsfullscreen = False
    m_binit = False
    m_memFlags = 0
    
    
End Sub

Private Function MakeVector(a As Single, b As Single, c As Single) As D3DVECTOR
    Dim vecOut As D3DVECTOR
    
    With vecOut
        .X = a
        .Y = b
        .z = c
    End With
    
    MakeVector = vecOut
End Function

Function CreateOffscreenBackBuffer() As Boolean
    
    'CREATE A BACK BUFFER THE SAME SIZE AS OUR WINDOW
    m_dx.GetWindowRect UserControl.hWnd, m_backRect
    m_backSurfaceDesc.lFlags = DDSD_CAPS Or DDSD_HEIGHT Or DDSD_WIDTH
    m_backSurfaceDesc.ddsCaps.lCaps = DDSCAPS_OFFSCREENPLAIN Or DDSCAPS_3DDEVICE Or m_memFlags
    m_backSurfaceDesc.lWidth = m_backRect.Right - m_backRect.Left
    m_backSurfaceDesc.lHeight = m_backRect.Bottom - m_backRect.Top
    Set m_backSurface = m_dd.CreateSurface(m_backSurfaceDesc)
    m_backSurface.GetSurfaceDesc m_backSurfaceDesc

End Function


'-=========================================
'- AttatchZBuffer
'
'  (A ZBuffer holds state for every pixel rendered
'  indicating what depth the rendered pixel is supposed to
'  represent so that when pixels are rendered on top of it
'  the rendered can decide if the new pixel is behind the
'  all ready rendered one)
'
'-=========================================
Function AttatchZBuffer() As Boolean
    On Local Error GoTo errOut
            
    
    Dim hr As Integer
    Dim ddsZBuff  As DirectDrawSurface7
    Dim ddsd As DDSURFACEDESC2
    Dim ddsd2 As DDSURFACEDESC2
    
    Dim l As Long
    Dim i As Long

    m_bClearZ =FALSE

    '- look for a 16 bit z buffer formats
    '  each card suports only certain formats
    '  but as a rule they all have at least
    '  a 16 bit z buffer - or none at all
    
    Dim fenum As Direct3DEnumPixelFormats
    Set fenum = m_d3d.GetEnumZBufferFormats(m_str3dDevGuid)
    l = fenum.GetCount()
    
    '-  some cards dont support zbuffering so
    '   we return success here because some cards can do
    '   3d with out none
    If l = 0 Then
        Exit Function
        AttatchZBuffer = True
    End If
    
    '- loop through zbuffer formats and get the first 16 bit one
    '  we find
    For i = 1 To l
        Call fenum.GetItem(i, ddsd2.ddpfPixelFormat)
        If ddsd2.ddpfPixelFormat.lZBufferBitDepth = 16 Then Exit For
    Next
    
    '- Get Z-buffer surface info
    '  from back buffer
    '  (w, h, bpp, video vs. system memory)
    Call m_backSurface.GetSurfaceDesc(ddsd)
                           
    '- to describe a zbuffer surface we need the pixel format that
    '  we already copied into ddsd2 above and the DDSCAPS_ZBUFFER
    '  flag. m_memtype must be the same for the back buffer and the
    '  the zbuffer (SYSTEM or VIDEO)
    
    ddsd2.lFlags = DDSD_CAPS Or _
                DDSD_WIDTH Or _
                DDSD_HEIGHT Or _
                DDSD_PIXELFORMAT
    ddsd2.ddsCaps.lCaps = ddsd2.ddsCaps.lCaps Or DDSCAPS_ZBUFFER Or m_memFlags
    ddsd2.lWidth = ddsd.lWidth
    ddsd2.lHeight = ddsd.lHeight
    ddsd2.ddpfPixelFormat.lFlags = DDPF_ZBUFFER Or ddsd2.ddpfPixelFormat.lFlags
    
    Set m_ZBuffer = m_dd.CreateSurface(ddsd2)
    
    
    '- Attach Z-buffer to rendering surface
    Call m_backSurface.AddAttachedSurface(m_ZBuffer)
    
    m_bClearZ =TRUE
    
    AttatchZBuffer = True
    Exit Function
errOut:
    
    AttatchZBuffer = False
End Function

Private Sub UpdateStats()
    Static FPS   As Single
    Static LastTime  As Single
    Static nFrames   As Single
    
    
    ' Keep track of the time lapse and frame count
    Dim fTime As Single
    fTime = m_dx.TickCount() * 0.001 ' // Get current time in seconds
    nFrames = nFrames + 1
    
    
    '// Update the frame rate once per second
    If (fTime - LastTime > 1#) Then
    
        FPS = nFrames / (fTime - LastTime)
        LastTime = fTime
        nFrames = 0
    End If

    If m_backSurface Is Nothing Then Exit Sub
    Dim desc As DDSURFACEDESC2
    m_backSurface.GetSurfaceDesc desc
    m_backSurface.SetForeColor vbYellow
    m_backSurface.DrawText 10, 10, "FPS:" + Str(FPS) + Str(desc.lWidth) + Str(desc.lHeight) + Str(desc.ddpfPixelFormat.lRGBBitCount) + " " + m_str3dDevGuid, False
    
End Sub



Private Function F5ResChange(KeyCode As Integer)
    Dim b As Boolean
    
    if not m_binit then exit function

    If m_bIsfullscreen Then
        'Go to windowed mode from Fullscreen on Esc
        If KeyCode = vbKeyEscape Then
            m_bIsfullscreen = False
            b = InitWindowed(m_strDDGuid, m_str3dDevGuid)
            If b = False Then
                m_str3dDevGuid = "IID_IDirect3DRGBDevice"
                b = InitWindowed(m_strDDGuid, m_str3dDevGuid)
            End If
            
            RaiseEvent NewDDraw
        End If
    Else
        'In windowed mode F5 brings up selection dialog
        If KeyCode = vbKeyF5 Then
            b = frmSelectRes.ChangeConfig(m_strDDGuid, m_str3dDevGuid, m_fsW, m_fsH, m_fsbpp, m_bIsfullscreen, m_bShowFps)
            If b Then
                b = False

                'if they ask to go Full Screen - try
                If m_bIsfullscreen Then
                    b = InitFullScreen(m_strDDGuid, m_str3dDevGuid, m_fsW, m_fsH, m_fsbpp)
                End If
            
                'if failed to go full screen or haven tried - try windowed
                If b = False Then
                    b = InitWindowed(m_strDDGuid, m_str3dDevGuid)
                End If
                
                'if still failed try windowed with RGB rasterizer
                If b = False Then
                    m_str3dDevGuid = "IID_IDirect3DRGBDevice"
                    b = InitWindowed(m_strDDGuid, m_str3dDevGuid)
                End If
            
                RaiseEvent NewDDraw
            End If
       End If
    End If

End Function



'Setup defaults
Private Sub UserControl_Initialize()
    EnableF5ResChange = True
End Sub

Private Sub UserControl_Resize()
    If m_binit Then
      '  Set m_backSurface = Nothing
      '  Set m_ZBuffer = Nothing
        
      '  CreateOffScreenBackBuffer
      '  AttatchZBuffer
      '
      '  m_dev.SetRenderTarget m_backSurface
    
    Else
        Picture1.Width = UserControl.ScaleWidth
        Picture1.Height = UserControl.ScaleHeight
    End If
    
End Sub

Private Sub UserControl_Show()
    If UserControl.Ambient.UserMode = True Then
        Picture1.Visible = False
    Else
        Picture1.Visible = True
    End If
    
End Sub



'Events to marhall back to the user from windowed operation

Private Sub UserControl_KeyDown(KeyCode As Integer, Shift As Integer)
    If EnableF5ResChange Then
        F5ResChange KeyCode
    End If
    RaiseEvent KeyDown(KeyCode, Shift)
End Sub


Private Sub UserControl_KeyPress(KeyAscii As Integer)
    RaiseEvent KeyPress(KeyAscii)
End Sub

Private Sub UserControl_KeyUp(KeyCode As Integer, Shift As Integer)
    RaiseEvent KeyUp(KeyCode, Shift)
End Sub

Private Sub UserControl_MouseDown(Button As Integer, Shift As Integer, X As Single, Y As Single)
    RaiseEvent MouseDown(Button, Shift, X, Y)
End Sub

Private Sub UserControl_MouseUp(Button As Integer, Shift As Integer, X As Single, Y As Single)
    RaiseEvent MouseUp(Button, Shift, X, Y)
End Sub

Private Sub UserControl_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
    RaiseEvent MouseMove(Button, Shift, X, Y)
End Sub

Private Sub UserControl_Paint()
    RaiseEvent Paint
End Sub

Private Sub UserControl_Click()
    RaiseEvent Click
End Sub

Private Sub UserControl_DblClick()
    RaiseEvent DblClick
End Sub


'Marshal events from full screen window
'NOTE: we must use a form full fullscreen operation because
' SetCooperativeLevel expects an hwnd that has no parent
' That is why we need to marshal events for both the UserControl
' and the FSWindow

Private Sub frmFSWindow_Click()
    RaiseEvent Click
End Sub

Private Sub frmFSWindow_DblClick()
    RaiseEvent DblClick
End Sub

Private Sub frmFSWindow_KeyDown(KeyCode As Integer, Shift As Integer)
    If EnableF5ResChange Then
        F5ResChange KeyCode
    End If

    RaiseEvent KeyDown(KeyCode, Shift)
End Sub

Private Sub frmFSWindow_KeyPress(KeyAscii As Integer)
    RaiseEvent KeyPress(KeyAscii)
End Sub

Private Sub frmFSWindow_KeyUp(KeyCode As Integer, Shift As Integer)
    RaiseEvent KeyUp(KeyCode, Shift)
End Sub

Private Sub frmFSWindow_MouseDown(ButtonN As Integer, Shift As Integer, X As Single, Y As Single)
    RaiseEvent MouseDown(ButtonN, Shift, X, Y)
End Sub

Private Sub frmFSWindow_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
    RaiseEvent MouseDown(Button, Shift, X, Y)
End Sub

Private Sub frmFSWindow_MouseUp(Button As Integer, Shift As Integer, X As Single, Y As Single)
    RaiseEvent MouseUp(Button, Shift, X, Y)
End Sub

Private Sub frmFSWindow_Paint()
    RaiseEvent Paint
End Sub

