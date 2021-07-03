VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
END
Attribute VB_Name = "IMClass"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
'
' DIRECT3D IM SETUP USING DIRECTDRAW4 and DIRECT3D3
'
'

Option Explicit

Private Declare Function GetWindowRect Lib "user32.dll" (ByVal hwnd As Long, ByRef r As RECT) As Long

                                            
'- Public Variables
'
Public Dx As DirectX7                       '- DirectX globals object
Public dd As DirectDraw4                    '- DirectDraw      object
Public d3d As Direct3D3                     '- Direct3D        object
Public d3dViewport As Direct3DViewport3     '- Direct3DViewport
Public d3dDevice As Direct3DDevice3         '- Direct3DDevice
Public screenSurface As DirectDrawSurface4  '- DirectDrawSurface representing the screen
Public backSurface As DirectDrawSurface4    '- DirectDrawSurface representing our window
Public ZBuffer As DirectDrawSurface4        '- Surface: ZBuffer used for 3d



'- Private Variables
'
                                            '- State set by init funtions
Dim m_bUseVMem As Boolean                   '- Try Video memory. influences m_memtype
Dim m_bSoftwareOnly As Boolean              '- Do not attempt to use HW if this is set
Dim m_strTry3dDevGuid As String             '- If not "" then try to set up 3d hardware
Dim m_bUse3d As Boolean                     '- Setup surfaces for 2d only                                            '  with this guid as opposed to finding one
Dim m_bCreateViewport As Boolean            '- Create a default viewport
Dim m_bUseZBuffer As Boolean                '- try to use a zbuffer
Dim m_bTLVertices As Boolean                '- set to true if vieport is to be setup
                                            '  for only Transoformed an lit Vertices
Dim m_bIsfullscreen As Boolean              '- Are we running full screen?


                                            '- DirectDraw state
Dim m_memtype As Long                       '- Indicates if we want our surfaces in
                                            '  System Memory or in VideoMemory
                                            '  For 3d im to run full screen
                                            '  and accelerated all surfaces
                                            '  must be in video memory
Dim m_hwnd As Long                          '- Hwnd we use for clipping
                                            '  for full screen this is the FSWindow
                                            '  for windowed this is the controls hwnd
Dim m_backRect As RECT                      '- Rectange describing back buffer
Dim m_bpp As Integer                        '- Bits per pixel we are in
Dim m_height As Integer                     '- Height of back buffer
Dim m_width As Integer                      '- Width of back buffer
Dim m_ddsdBack As DDSURFACEDESC2            '- Desc of the Back Buffer
Dim m_ddsdScreen As DDSURFACEDESC2          '- Desc of the Screen Buffer
Dim m_bddinit As Boolean                    '- Has DirectDraw been initialized?
Dim m_strDDDevGuid As String                '- GUID for DirectDraw device in use

                                            '- Direct3D state
Dim m_bHW As Boolean                        '- 3d device in use is HardWare device
Dim m_str3dDevGuid As String                '- GUID of enumerated 3d Device (usually hw)
Dim m_str3dFallbackGuid As String           '- GUID of Software fallback device
Dim m_strActiveDevGuid As String            '- GUID of device in use

Dim m_str3dDevDesc As String                '- Readable description of best match 3d Device
Dim m_str3dFallbackDesc As String           '- Readable description of sofrware fallback dev.
Dim m_strActiveDevDesc As String            '- Readable description of device in use

Dim m_hwEnabled As Boolean                  '- 3d HW device found and in use
Dim m_bNoZBuffer As Boolean
Dim m_d3dFlags As Long                      '- flags needed createSurface

'-=========================================
'  Init
'
'  note:  pass in "" for default
'         sets the directDrawguid to be used
'-=========================================
Public Function Init(sguid As String) As Boolean
    On Local Error GoTo errOut
    
    
    Set Dx = New DirectX7
    Set dd = Dx.DirectDrawCreate(sguid)
    m_strDDDevGuid = sguid
        
    Init = True
    Exit Function
    
errOut:
    Init = False
    DebugLog " DirectDraw failed to initialize your card may not have DirectDraw drivers installed"
End Function


'-=========================================
'  Start
'       hwnd        - SetCooperativeLevel Hwnd
'       bFullscreen - Start fullscreen mode
'       bUse3D      - (TRUE) unless you just want 2d support
'       bTryVMem    - (FALSE only if TRUE fails)
'
'  NOTE: make sure all surfaces are destroyed
'  before calling start as start will fail otherwise
'
'-=========================================

Public Function Start(hwnd As Long, bFullScreen As Boolean, bUse3D As Boolean, bTryVMem As Boolean) As Boolean
    On Local Error GoTo errOut
    Dim b As Boolean
    
    'Free our existing back and screen buffer
    Set backSurface = Nothing
    Set screenSurface = Nothing
    
    
    '-Save our inputs as State
    m_hwnd = hwnd
    m_bIsfullscreen = bFullScreen
    m_bUse3d = bUse3D
    m_bUseVMem = bTryVMem
        
    '- Our Surfaces are going to have to be created in video memory
    '  or in System memory. We let the user decide but keep in mind
    '  to have FS or HW work things need to be in VIDEO memory
    '  we later pass these flags to createSurface
    '
    If m_bUseVMem Then
        m_memtype = DDSCAPS_VIDEOMEMORY
    Else
        m_memtype = DDSCAPS_SYSTEMMEMORY
    End If
    
    '- More flags we pass to createSurface
    '  3DDEVICE needs to be set to create as surface that
    '  the 3d immediate mode can render to
    '
    If m_bUse3d Then
        m_d3dFlags = DDSCAPS_3DDEVICE
    Else
        m_d3dFlags = 0
    End If
            
            
    '- Running Full screen and running windowed follow
    '  very different code paths to setup directDraw
    '  These functions simply create DirectDrawSurfaces
    '  to render to.
    If m_bIsfullscreen Then
        b = InitFullScreen()
    Else
        b = InitWindowed()
    End If
    
    '- If we failed our DirectDrawSurface creation
    '  then clean up and exit
    If b = False Then
        DebugLog "DirectDraw was unable to create the necessary surfaces to continue"
        Start = False
        Cleanup
        Exit Function
    End If
    
    'NOTE: if we where to support palettized modes we should
    '      attach palettes to our primary and back buffer here!!!
    
    
    DebugLog "DirectDraw SURFACES OK"
    
    '- More 3d Setup
    
    If m_bUse3d = True Then
        
        '- We get our D3D object for our DirectDraw object
        '  (remember that the direct draw object is analogous
        '   to your VideoCard- 1 object per card)
        Set d3d = dd.GetDirect3D
        
        '- If the user hasnt set a prefered device GUID
        '  then we go find one
        '  and initialize the m_strGuid state members
        If m_str3dDevGuid = "" Then
            EnumDevices
        End If
        
                
        '- Now that our Front and Back Buffer are created we need
        '  a ZBuffer
        b = AttatchZBuffer()
        If b = False Then
             GoTo errOut
        End If
    
        
        '- Create the Direct3DDevice
        '  use the device guids we enumerated to pick the device
        b = CreateIMDevice()
         If b = False Then
         
            '- If we failed try and fall back using the
            '  Software device guids
            b = CreateIMSoftwareDevice()
            If b = False Then GoTo errOut
        End If
        
        '- Create a default viewport
        If m_bCreateViewport Then
            b = CreateViewport(m_width, m_height)
            If b = False Then GoTo errOut
        End If
            
    End If
    Start = True
    Exit Function
    
    
    
errOut:
    Start = False
    Cleanup
End Function




'-=========================================
'  CreateIMDevice
'-=========================================

Private Function CreateIMDevice() As Boolean
    On Local Error GoTo errOut
    
    '- If the user gave us a guid to try try it first
    If m_strTry3dDevGuid <> "" Then
        Set d3dDevice = d3d.CreateDevice(m_strTry3dDevGuid, backSurface)
        m_strActiveDevGuid = m_strTry3dDevGuid
        m_strActiveDevDesc = ""
    
    '- other wise use m_str3dDevGuid which EnumDevices filled in.
    '  note call CreateIMSoftwareDevice if they explictly want Software
    Else
        If m_bSoftwareOnly Then GoTo errOut
        Set d3dDevice = d3d.CreateDevice(m_str3dDevGuid, backSurface)
        m_strActiveDevGuid = m_str3dDevGuid
        m_strActiveDevDesc = m_str3dDevDesc
    End If
    
    CreateIMDevice = True
    Exit Function

errOut:
    CreateIMDevice = False
    m_strActiveDevGuid = ""
    m_strActiveDevDesc = ""

End Function



'-=========================================
'  CreateIMSoftwareDevice
'
'  note: should call EnumDevices before
'        attempting this call
'        EnumDevices fills in m_str3dFallbackGuid
'-=========================================

Private Function CreateIMSoftwareDevice() As Boolean
    If m_str3dFallbackGuid = "" Then Exit Function
    On Local Error GoTo errOut
    Set d3dDevice = d3d.CreateDevice(m_str3dFallbackGuid, backSurface)
    m_strActiveDevGuid = m_str3dFallbackGuid
    m_strActiveDevDesc = m_str3dFallbackDesc

    CreateIMSoftwareDevice = True
    Exit Function
errOut:
    m_strActiveDevGuid = ""
    m_strActiveDevDesc = ""

    CreateIMSoftwareDevice = False
End Function



'-=========================================
'  SetDisplayMode
'
'  note: only used in FullScreen modes
'        call prior to Start to select the resolution
'        or after to change the resoultion after the fact
'
'-=========================================
Public Function SetDisplayMode(width As Integer, height As Integer, bpp As Integer) As Boolean
    On Local Error GoTo errOut
    
    If m_bIsfullscreen And m_bddinit Then
        dd.SetDisplayMode width, height, bpp, 0, 0
    End If
    
    m_bpp = bpp
    m_height = height
    m_width = width
    
    SetDisplayMode = True
    Exit Function
    
errOut:
    SetDisplayMode = False
End Function

'-=========================================
'  InitFullScreen
'
'  sets up DirectDrawSurfaces for Fullscreen
'  viewing
'
'-=========================================

Private Function InitFullScreen() As Boolean
    On Local Error GoTo errOut
    Dim e As Long
    
    '- Setting the CooperativeLevel
    '  Modex allows us to change display modes
    '  Exclusive allows us to perform flip operations
    '  and indicates we dont want windows to get in the way
    DebugLog "set Cooperitive level - about " + Str(m_hwnd)
    dd.SetCooperativeLevel m_hwnd, DDSCL_NOWINDOWCHANGES Or DDSCL_FULLSCREEN Or DDSCL_ALLOWMODEX Or DDSCL_EXCLUSIVE
    'dd.SetCooperativeLevel m_hwnd, DDSCL_FULLSCREEN Or DDSCL_EXCLUSIVE
        
    '- If we got this fare we succeeded in Initilalizing DirectDraw
    m_bddinit = True
    DebugLog "set Cooperitive level ok"
            
            
    If m_width <> 0 And m_height <> 0 And m_bpp <> 0 Then
        SetDisplayMode m_width, m_height, m_bpp
    End If
        
    '- Get the SCREEN SURFACE and create a back buffer too
    '  the DDSCAPS_FLIP us to call flip and swap the
    '  front and back buffers for fast rendering
    DebugLog "mem type" + Str(m_memtype)
    m_ddsdScreen.lFlags = DDSD_CAPS Or DDSD_BACKBUFFERCOUNT
    m_ddsdScreen.ddsCaps.lCaps = DDSCAPS_PRIMARYSURFACE Or DDSCAPS_FLIP Or DDSCAPS_COMPLEX Or m_d3dFlags Or m_memtype
    m_ddsdScreen.lBackBufferCount = 1
    Set screenSurface = dd.CreateSurface(m_ddsdScreen)
    
    DebugLog "created Primary OK"
    
    '- Get the BACK SURFACE we render to
    '  since the back buffer is already attached
    '  all we need to do is get a reference to it
    Dim caps As DDSCAPS2
    caps.lCaps = DDSCAPS_BACKBUFFER
    Set backSurface = screenSurface.GetAttachedSurface(caps)
    
    DebugLog "Got Backbuffer OK"
    
    '- Get some default info about the back surface
    backSurface.GetSurfaceDesc m_ddsdBack
    m_width = m_ddsdBack.lWidth
    m_height = m_ddsdBack.lHeight
    m_bpp = m_ddsdBack.ddpfPixelFormat.lRGBBitCount
    
    '- Indicate our surfaces are setup for full screen operation
    m_bIsfullscreen = True
    
    InitFullScreen = True
    Exit Function

errOut:
    e = Error.Number
    Call dd.SetCooperativeLevel(m_hwnd, DDSCL_NORMAL)
    dd.RestoreDisplayMode
    Debug.Print "InitFullScreen failed" + Str(e)
    

End Function

'-=========================================
'  InitWindowed
'
'  sets up DirectDrawSurfaces for a Window
'
'-=========================================
Private Function InitWindowed() As Boolean
    On Local Error GoTo errOut
    Dim b As Boolean
    
    '- Set DirectDraw Cooperative Level to normal
    Call dd.SetCooperativeLevel(m_hwnd, DDSCL_NORMAL)
    m_bddinit = True
    
        
    '- get the SCREEN SURFACE
    m_ddsdScreen.lFlags = DDSD_CAPS
    m_ddsdScreen.ddsCaps.lCaps = DDSCAPS_PRIMARYSURFACE
    Set screenSurface = dd.CreateSurface(m_ddsdScreen)
            
    
    '- Create a clipper object that tracks our window
    '  when the clipper is attached to the primary(screen)
    '  any blts to the primary will be "clipped" by
    '  a rectangle defined by our window.
    '  also if the window is minimized nothing is blt
    '  and if the window is obscured by another only
    '  the visible portion of our window will be blt to
    '
    Dim clip As DirectDrawClipper
    Set clip = dd.CreateClipper(0)
    clip.SetHWnd m_hwnd
    screenSurface.SetClipper clip
    
    
    '- create a Backbuffer
    CreateOffscreenBackBuffer
        
    
    InitWindowed = True
    Exit Function
                    
errOut:
    InitWindowed = False
End Function



'-=========================================
'  CreateOffscreenBackBuffer
'
'  helper for InitWindowed
'
'-=========================================
Private Function CreateOffscreenBackBuffer()
    
    '- release existing backbuffer
    Set backSurface = Nothing
    
    '- Get dimensions of our destination window
    GetWindowRect m_hwnd, m_backRect
    
    '- create the back surface to fit the window
    m_ddsdBack.lFlags = DDSD_CAPS Or DDSD_HEIGHT Or DDSD_WIDTH
    m_ddsdBack.ddsCaps.lCaps = m_memtype Or DDSCAPS_OFFSCREENPLAIN Or m_d3dFlags
    m_ddsdBack.lWidth = m_backRect.right - m_backRect.left
    m_ddsdBack.lHeight = m_backRect.bottom - m_backRect.top
        
    Set backSurface = dd.CreateSurface(m_ddsdBack)
    
    
    'Safe some state - TODO
    m_width = m_ddsdBack.lWidth
    m_height = m_ddsdBack.lHeight
    m_bpp = m_ddsdBack.ddpfPixelFormat.lRGBBitCount
    
    
End Function


'-=========================================
'  UpdateScreen
'
'  CONSIDER: removing DDBLT_WAIT flag
'            and optimizing speed
'-=========================================
Public Function UpdateScreen() As Boolean
    On Local Error GoTo errOut
    Dim destRect As RECT
    
    If m_bIsfullscreen Then
    
        ' Flip back buffer to front
        screenSurface.Flip Nothing, DDFLIP_WAIT
        
    Else
        Dim srcRect As RECT
                
        'get Rectangle describing backbuffer
        srcRect.bottom = m_ddsdBack.lHeight
        srcRect.right = m_ddsdBack.lWidth
        
        'Get rectangle of or window and blt our back buffer
        'to the screen (through the clipper)
        GetWindowRect m_hwnd, destRect
        screenSurface.Blt destRect, backSurface, srcRect, DDBLT_WAIT
                
    End If
    
errOut:

End Function

'-=========================================
'  Resize
'
'  call when our hWnd changes size
'  to recongifure the back buffer.
'  be aware that content may fallback to Software
'  if there is not enought video memory.
'
'  TODO: currently once a fallback to software
'  rasterization occurs
'  we no longer attempt to use HW.
'-=========================================
Public Function Resize() As Boolean
    
    Dim b As Boolean
    
    '- want to see how many resize calls we get
    DebugLog "RESIZE" + Str(Timer)
    
    '- Full screen apps must use SetDisplayMode instaed
    If m_bIsfullscreen = True Then
        DebugLog "Must use SetDisplayMode to change size when in full screen"
        Exit Function
    End If
    
    '- Create a new back buffer
    If CreateNewRenderTarget() = False Then
        
        'TODO:
        '- If that fails try system memory
        '  m_memtype = DDSCAPS_SYSTEMMEMORY
        '  If CreateNewRenderTarget() = False Then
        '    DebugLog "cant resize the viewport"
        '    Resize = False
        '  End If
        Exit Function
    End If
    Resize = True
End Function

'-=========================================
'- CreateNewRenderTarget
'-=========================================
Function CreateNewRenderTarget() As Boolean
    
    On Local Error GoTo errOut
    Dim b As Boolean
        
    
    '- create new back buffer from our hwnd
    '  sets m_width and m_height from hwnd
    CreateOffscreenBackBuffer
    
    '- Skip if we dont need 3d
    If m_bUse3d = True Then
    
        '- Attach a new zbuffer
        b = AttatchZBuffer()
        If b = False Then GoTo errOut
        
        '- Set the new back buffer as our render target
        d3dDevice.SetRenderTarget backSurface
        
        '- create viewport
        If m_bCreateViewport Then
            b = CreateViewport(m_width, m_height)
            If b = False Then GoTo errOut
        End If
        
    End If
    CreateNewRenderTarget = True
    Exit Function
errOut:
    CreateNewRenderTarget = False
    Set backSurface = Nothing
    Set ZBuffer = Nothing
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
            
    If m_bUseZBuffer = False Then
        AttatchZBuffer = True
        Exit Function
    End If
    
    
    Dim hr As Integer
    Dim ddsZBuff  As DirectDrawSurface4
    Dim ddsd As DDSURFACEDESC2
    Dim ddsd2 As DDSURFACEDESC2
    
    Dim l As Long
    Dim i As Long

    '- look for a 16 bit z buffer formats
    '  each card suports only certain formats
    '  but as a rule they all have at least
    '  a 16 bit z buffer - or none at all
    
    Dim fenum As Direct3DEnumPixelFormats
    Set fenum = d3d.GetEnumZBufferFormats(m_str3dDevGuid)
    l = fenum.GetCount()
    
    '-  some cards dont support zbuffering so
    '   we return success here because some cards can do
    '   3d with out none
    If l = 0 Then
        DebugLog "card does not support Z buffering"
        Exit Function
        m_bNoZBuffer = True
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
    Call backSurface.GetSurfaceDesc(ddsd)
                           
    '- to describe a zbuffer surface we need the pixel format that
    '  we already copied into ddsd2 above and the DDSCAPS_ZBUFFER
    '  flag. m_memtype must be the same for the back buffer and the
    '  the zbuffer (SYSTEM or VIDEO)
    
    ddsd2.lFlags = DDSD_CAPS Or _
                DDSD_WIDTH Or _
                DDSD_HEIGHT Or _
                DDSD_PIXELFORMAT
    ddsd2.ddsCaps.lCaps = ddsd2.ddsCaps.lCaps Or DDSCAPS_ZBUFFER Or m_memtype
    ddsd2.lWidth = ddsd.lWidth
    ddsd2.lHeight = ddsd.lHeight
    ddsd2.ddpfPixelFormat.lFlags = DDPF_ZBUFFER Or ddsd2.ddpfPixelFormat.lFlags
    DebugLog "OK on get surface " + Str(ddsd.lWidth) + Str(ddsd.lHeight)
    Set ZBuffer = dd.CreateSurface(ddsd2)
    DebugLog "OK on create"
    
    '- Attach Z-buffer to rendering surface
    Call backSurface.AddAttachedSurface(ZBuffer)
    
    DebugLog "OK on attach Z"
    AttatchZBuffer = True
    Exit Function
errOut:
    DebugLog "couldnt attach Z buffer"
    AttatchZBuffer = False
End Function





'-=========================================
'- Clear
'-=========================================
Public Sub Clear()
    On Local Error Resume Next
    
    Dim hr As Long
    Dim r As RECT
    Dim r2 As RECT
    Dim arect(1) As D3DRECT
        
    
    
    '- TODO
    '  can get much better performance
    '  if I only blt update rectagles passed back
    '  from d3d
    '  It only crear the Zbuffer and
    '  back buffer wear there are changes.
    
    '- setup rectangle to clear entire back buffer
    '  and z buffer
    arect(0).X1 = 0
    arect(0).X2 = m_width
    arect(0).Y1 = 0
    arect(0).Y2 = m_height


    '- Clear the ZBuffer (if there is one)
    '  and clear the BackBuffer
    Call d3dViewport.Clear(1, arect, D3DCLEAR_TARGET Or D3DCLEAR_ZBUFFER)
    
End Sub





'-=========================================
' CreateViewport
'
' NOTE: There is a different setup if using
'       TLverts vs other verts
' NOTE: In windowed mode we use the size of the
'       window for the viewport
'       In Full Screen.. the viewport is sized
'       to current primary frontbuffer.
'-=========================================
'
Public Function CreateViewport(width As Integer, height As Integer) As Boolean
   
    Dim hr As Integer
    
    On Local Error GoTo errOut

    '- Create the Viewport
    Set d3dViewport = d3d.CreateViewport()
        
    '- Attach it to device
    Call d3dDevice.AddViewport(d3dViewport)
    
    '- setup viewport data
    Dim viewData As D3DVIEWPORT2
        
    viewData.lX = 0
    viewData.lY = 0
    viewData.lWidth = width
    viewData.lHeight = height
    viewData.minz = 0#
    viewData.maxz = 1#
    
    '-If we plan to let the driver to tranforms
    ' then our clipping info needs to be
    ' set up like so
    If (Not m_bTLVertices) Then
        viewData.clipX = -1#
        viewData.clipWidth = 2#
        viewData.clipHeight = (height * 2# / width)
        viewData.clipY = viewData.clipHeight / 2#
    
    '- if we plan to present coord in screen
    '  space to the driver then our viewdata
    '  needs to be setup as such
    Else
        viewData.clipX = 0
        viewData.clipY = 0
        viewData.clipWidth = width
        viewData.clipHeight = height
               
    End If

    '- set the view data
    Call d3dViewport.SetViewport2(viewData)
        
    '- make sure the Device knows which
    '  viewport to use..
    Call d3dDevice.SetCurrentViewport(d3dViewport)
                
    CreateViewport = True
    Exit Function
    
errOut:
    CreateViewport = False
End Function


'-=========================================
' EnumDevices
' called to find the best 3ddevice
'-=========================================
Public Sub EnumDevices()
    Dim devEnum As Direct3DEnumDevices
    Dim devDescHW As D3DDEVICEDESC
    Dim devDescSW As D3DDEVICEDESC
    Dim info As DXDRIVERINFO
    Dim i As Long
    
    
    '- Get all the differnt 3d Devices (renderers) that
    '  the current video card supports
    '
    Set devEnum = d3d.GetDevicesEnum()
        
    '- Assume HW is not supported
    m_hwEnabled = False
    
    '- Loop through the list
    '  CONSIDER - what if HW comes
    For i = 1 To devEnum.GetCount()
        m_hwEnabled = False
        
        '- Save the device guid and descirption
        m_str3dDevGuid = devEnum.GetGuid(i)
        m_str3dDevDesc = devEnum.GetName(i)
        
        '- Get the HW and Software Capabilities
        devEnum.GetHWDesc i, devDescHW
        devEnum.GetHELDesc i, devDescSW
        
        '- If we found a software device
        '  the Softwarecaps color model will be non zero
        If devDescSW.lColorModel = D3DCOLOR_RGB Then
            m_str3dFallbackGuid = devEnum.GetGuid(i)
            m_str3dFallbackDesc = devEnum.GetName(i)
        End If
        
        '- If we are using System memory we have to use the software
        '  device we found so exit out of the loop
        If m_memtype = DDSCAPS_SYSTEMMEMORY And devDescHW.lColorModel = 0 Then
            Exit For
            
        '- other wise exit out of the loop if our HW caps color model is non zero
        '  meaning we found a HW 3d device
        ElseIf m_memtype <> DDSCAPS_SYSTEMMEMORY And devDescHW.lColorModel <> 0 Then
            m_hwEnabled = True
            Exit For
        End If
        
        
    Next
    
    DebugLog "D3DIM " + m_str3dDevDesc
End Sub


'-=========================================
' Cleanup
'
' release all surfaces and devices
'-=========================================

Public Function Cleanup()
    Set backSurface = Nothing
    Set screenSurface = Nothing
    
    Set dd = Nothing
    Set ZBuffer = Nothing
    Set d3dViewport = Nothing
    Set d3dDevice = Nothing
    Set d3d = Nothing
    
End Function



'-=========================================
' SetInitDisplayMode
'
' call before start to setup full screen
'-=========================================
Public Sub SetInitDisplayMode(w As Integer, h As Integer, bpp As Integer)
    m_width = w
    m_height = h
    m_bpp = bpp
    
    dd.SetDisplayMode w, h, bpp, 0, 0

End Sub

'-=========================================
' EnableViewport
'
' call before start to indicate if you want a default
' viewport setup for you - (default is true)
'-=========================================
Public Sub EnableViewport(b As Boolean)
    m_bCreateViewport = b
End Sub


'-=========================================
' EnableViewport
'
' call before start to indicate if you want a default
' Zbuffer setup for you - (default is true)
'-=========================================
Public Sub EnableZBuffer(b As Boolean)
    m_bUseZBuffer = b
End Sub


'-=========================================
' SetDeviceGuid
'
' call before start to indicate if a preference
' for a specific 3d device (rasterizer)
'
'-=========================================
Public Sub SetDeviceGuid(strGuid As String)
    m_strTry3dDevGuid = strGuid
End Sub

'-=========================================
' SetSoftwareOnly
'
' call before start to indicate that HW
' should not be used to render
'
Public Sub SetSoftwareOnly(b As Boolean)
    m_bSoftwareOnly = b
End Sub

'-=========================================
' Class_Initialize
'
'-=========================================
Private Sub Class_Initialize()
    
    m_memtype = DDSCAPS_VIDEOMEMORY
    m_bCreateViewport = True
    m_width = 0
    m_height = 0
    m_bpp = 0
    m_bUseZBuffer = True
End Sub


'-=========================================
' IsFullScreen
'-=========================================
Public Function IsFullScreen() As Boolean
    IsFullScreen = m_bIsfullscreen
End Function

'-=========================================
' m_strActiveDevGuid
'-=========================================
Public Function GetDeviceGuid() As String
    GetDeviceGuid = m_strActiveDevGuid
End Function

'-=========================================
' GetDeviceDesc
'-=========================================
Public Function GetDeviceDesc() As String
    GetDeviceDesc = m_strActiveDevDesc
End Function


'-=========================================
' UseScreenCooredinateViewport
'
' dont bother using
'-=========================================
Property Let UseScreenCooredinateViewport(b As Boolean)
    m_bTLVertices = b
End Property

'-=========================================
' UseScreenCooredinateViewport
'
' dont bother using
'-=========================================
Property Get UseScreenCooredinateViewport() As Boolean
    'default to false
    UseScreenCooredinateViewport = m_bTLVertices
End Property






