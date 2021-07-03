VERSION 5.00
Begin VB.Form Form1 
   BorderStyle     =   1  'Fixed Single
   Caption         =   "TextureVB"
   ClientHeight    =   3195
   ClientLeft      =   90
   ClientTop       =   660
   ClientWidth     =   3720
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   213
   ScaleMode       =   0  'User
   ScaleWidth      =   254
   StartUpPosition =   3  'Windows Default
End
Attribute VB_Name = "Form1"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Const pi As Single = 3.141592

' Define the cube.
Const NUM_CUBE_VERTICES As Integer = 4 * 6
Dim g_vCube(NUM_CUBE_VERTICES) As D3DVERTEX

' Textures for the cube.
Dim g_ddsTexture1 As DirectDrawSurface7, _
    g_ddsTexture2 As DirectDrawSurface7, _
    g_ddsTexture3 As DirectDrawSurface7

Dim g_dx As New DirectX7
Dim g_dd As DirectDraw7
Dim g_ddsd As DDSURFACEDESC2
Dim g_ddsPrimary As DirectDrawSurface7, _
    g_ddsBackBuffer As DirectDrawSurface7
Dim g_d3dDevice As Direct3DDevice7
Dim g_rcDest As RECT, _
    g_rcSrc As RECT
Dim g_d3drcViewport(0) As D3DRECT
Dim g_bRunning As Boolean

Private Sub Form_Load()
    Dim CNT As Single
    Dim j As Long

    ' Initialize the DirectDraw and Direct3D objects that this
    ' sample application will use to render and display the triangle.
    InitDDraw
    InitD3D
    InitDeviceObjects
    
    Me.Show
    
    g_bRunning = True
    Do While g_bRunning = True
        CNT = CNT + 1
        RenderScene
        FrameMove (CNT / 360)
        
        g_dx.GetWindowRect Me.hWnd, g_rcDest
    
        j = g_ddsPrimary.Blt(g_rcDest, g_ddsBackBuffer, g_rcSrc, DDBLT_WAIT)
        If j <> DD_OK Then
            MsgBox "Couldn't copy the source rectangle to the destination surface." & Chr$(13) & Hex(j)
            End
        End If
        DoEvents
    Loop
End Sub


''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
'   Animate the scene.
'
'   Called once per frame, the call is used for animating the scene. The device is
'   used for changing various render states, and the stepVal parameter is used for
'   the timing of the dynamics of the scene.
'
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
Private Sub FrameMove(stepVal As Single)
    
    ' Set the view matrix so that the camera is backed out along the z-axis, and looks
    ' down on the cube (rotating along the x-axis by -0.5 radians).
    Dim matView  As D3DMATRIX
    
    g_dx.IdentityMatrix matView
    
    matView.rc11 = 1
    matView.rc22 = Cos(-0.5)
    matView.rc23 = Sin(-0.5)
    matView.rc32 = -Sin(-0.5)
    matView.rc33 = Cos(-0.5)
    matView.rc43 = 5
    matView.rc44 = 1
            
    g_d3dDevice.SetTransform D3DTRANSFORMSTATE_VIEW, matView
    
    ' Set the world matrix to rotate along the y-axis
    Dim matWorld As D3DMATRIX
    
    g_dx.IdentityMatrix matWorld
    g_dx.RotateYMatrix matWorld, stepVal
    g_d3dDevice.SetTransform D3DTRANSFORMSTATE_WORLD, matWorld
      
End Sub

''''''''''''''''''''''''''''''''''''''''''''''''''''''
'
' Render the scene
'
''''''''''''''''''''''''''''''''''''''''''''''''''''''
Private Sub RenderScene()

    Dim i As Integer
          
    ' Clear the viewport to a blue color, and clear the z-buffer.
    g_d3dDevice.Clear 1, g_d3drcViewport(), D3DCLEAR_TARGET, &HFF, 1, 0
    
    ' Begin the scene.
    g_d3dDevice.BeginScene
    
    ' Draw the front and back faces of the cube using texture 1
    g_d3dDevice.SetTexture 0, g_ddsTexture1
    Call g_d3dDevice.DrawPrimitive(D3DPT_TRIANGLESTRIP, D3DFVF_VERTEX, g_vCube(0), _
         4, D3DDP_DEFAULT)
    Call g_d3dDevice.DrawPrimitive(D3DPT_TRIANGLESTRIP, D3DFVF_VERTEX, g_vCube(4), _
         4, D3DDP_DEFAULT)
         
    ' Draw the top and bottom faces of the cube using texture 2
    g_d3dDevice.SetTexture 0, g_ddsTexture2
    Call g_d3dDevice.DrawPrimitive(D3DPT_TRIANGLESTRIP, D3DFVF_VERTEX, g_vCube(8), _
         4, D3DDP_DEFAULT)
    Call g_d3dDevice.DrawPrimitive(D3DPT_TRIANGLESTRIP, D3DFVF_VERTEX, g_vCube(12), _
         4, D3DDP_DEFAULT)
   
    ' Draw the left and right faces of the cube using texture 3
    g_d3dDevice.SetTexture 0, g_ddsTexture3
    Call g_d3dDevice.DrawPrimitive(D3DPT_TRIANGLESTRIP, D3DFVF_VERTEX, g_vCube(16), _
         4, D3DDP_DEFAULT)
    Call g_d3dDevice.DrawPrimitive(D3DPT_TRIANGLESTRIP, D3DFVF_VERTEX, g_vCube(20), _
         4, D3DDP_DEFAULT)
   
    ' End the scene.
    g_d3dDevice.EndScene
End Sub

Private Sub Form_Unload(Cancel As Integer)
    g_bRunning = False
End Sub


''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
'
' Initalize DirectDraw.
'
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
Private Sub InitDDraw()
    ' Create the DirectDraw object and set the application
    ' cooperative level.
    Set g_dd = g_dx.DirectDrawCreate("")
    
    g_dd.SetCooperativeLevel Me.hWnd, DDSCL_NORMAL
    
    ' Prepare and create the primary surface.
    g_ddsd.lFlags = DDSD_CAPS
    g_ddsd.ddsCaps.lCaps = DDSCAPS_PRIMARYSURFACE
    
    Set g_ddsPrimary = g_dd.CreateSurface(g_ddsd)
    
    ' Now create the render-target surface. We are reusing g_ddsd here.
    g_ddsd.lFlags = DDSD_HEIGHT Or DDSD_WIDTH Or DDSD_CAPS
    g_ddsd.ddsCaps.lCaps = DDSCAPS_OFFSCREENPLAIN Or DDSCAPS_3DDEVICE
    
    ' Use the size of the form to determine the size of the render target
    ' and viewport rectangle.
    g_dx.GetWindowRect Me.hWnd, g_rcDest
    
    ' Set the dimensions of the surface description
    g_ddsd.lWidth = g_rcDest.Right - g_rcDest.Left
    g_ddsd.lHeight = g_rcDest.Bottom - g_rcDest.Top
    
    ' Create the render-target surface
    Set g_ddsBackBuffer = g_dd.CreateSurface(g_ddsd)
        
    ' Cache the dimensions of the render target. We'll use
    ' it for blitting operations.
    With g_rcSrc
        .Left = 0: .Top = 0
        .Bottom = g_ddsd.lHeight
        .Right = g_ddsd.lWidth
    End With
    
    ' Create a DirectDrawClipper and attach it to the primary surface.
    Dim pcClipper As DirectDrawClipper
    
    Set pcClipper = g_dd.CreateClipper(0)
    pcClipper.SetHWnd Me.hWnd
    
    g_ddsPrimary.SetClipper pcClipper
    
End Sub

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
'
' Initalize Direct3D, including the rendering device, lighting,
' the viewport, and the material.
'
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
Sub InitD3D()
    Dim d3d As Direct3D7
    Dim ddsd As DDSURFACEDESC2
        
    ' Retrieve a reference to the Direct3D7 class from the
    ' DirectDraw7 object.
    Set d3d = g_dd.GetDirect3D
    
    ' Query DirectDraw for the current display mode. For simplicity, this
    ' does not support palleted display modes (8-bit and lower).
    g_dd.GetDisplayMode ddsd
    
    If ddsd.ddpfPixelFormat.lRGBBitCount <= 8 Then
        MsgBox "This application does not support screen display " & _
               "modes lower than 16-bit."
        End
    End If
    
    '
    ' Create the device. The GUID is hardcoded for now, but should come from
    ' device enumeration, which is the topic of a tutorial. The device
    ' is created off of our back buffer, which becomes the render target for
    ' the newly created device.
    '
    On Error Resume Next
    Set g_d3dDevice = d3d.CreateDevice("IID_IDirect3DHALDevice", g_ddsBackBuffer)
    
    If g_d3dDevice Is Nothing Then
        Set g_d3dDevice = d3d.CreateDevice("IID_IDirect3DRGBDevice", g_ddsBackBuffer)
    End If
    
    ' Define the viewport rectangle.
    Dim VPDesc As D3DVIEWPORT7
        
    VPDesc.lWidth = g_rcDest.Right - g_rcDest.Left
    VPDesc.lHeight = g_rcDest.Bottom - g_rcDest.Top
    VPDesc.minz = 0#
    VPDesc.maxz = 1#
    
    g_d3dDevice.SetViewport VPDesc
    
    ' Cache the viewport rectangle for use in clearing operations later.
    With g_d3drcViewport(0)
        .X1 = 0: .Y1 = 0
        .X2 = VPDesc.lWidth
        .Y2 = VPDesc.lHeight
    End With
        
 End Sub

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
'
' Initalize the geometry.
'
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
Private Sub InitDeviceObjects()

    ' Generate the vertices for the cube.
    CreateCube g_vCube
    
    ' Set our directory for the bitmap images to be used as textures.
    FindMediaDir "tree1.bmp"
     
    ' Create textures from file-based bitmaps.
    Set g_ddsTexture1 = CreateTextureSurface("tree1.bmp")
    Set g_ddsTexture2 = CreateTextureSurface("tex1.bmp")
    Set g_ddsTexture3 = CreateTextureSurface("earth.bmp")
    
   ' For simplicity, use ambient lighting and a white material.
    Dim mtrl As D3DMATERIAL7
   
    mtrl.diffuse.r = 1#: mtrl.diffuse.g = 1#: mtrl.diffuse.b = 1#
    mtrl.Ambient.r = 1#: mtrl.Ambient.g = 1#: mtrl.Ambient.b = 1#
    
    ' Commit the material to the device.
    g_d3dDevice.SetMaterial mtrl
    
    ' Enable ambient lighting
    g_d3dDevice.SetRenderState D3DRENDERSTATE_AMBIENT, g_dx.CreateColorRGBA(1#, 1#, 1#, 1#)
    
    ' Set the projection matrix. Note that the view and world matrices are set in the
    ' FrameMove function, so that they can be animated each frame.
    Dim matProj As D3DMATRIX
    
    g_dx.IdentityMatrix matProj
    
    Call g_dx.ProjectionMatrix(matProj, 1, 1000, pi / 3#)
    g_d3dDevice.SetTransform D3DTRANSFORMSTATE_PROJECTION, matProj
    
    
    
End Sub

''''''''''''''''''''''''''''''''''''''''''''''''''''''
'
' Create the cube by passing this subroutine the array of the cube's vertices.
'
''''''''''''''''''''''''''''''''''''''''''''''''''''''
Private Sub CreateCube(vertices() As D3DVERTEX)
           
    ' Create vertices describing the front face of the cube.
    g_dx.CreateD3DVertex -1, 1, -1, 0, 0, -1, 0, 0, vertices(0)
    g_dx.CreateD3DVertex 1, 1, -1, 0, 0, -1, 1, 0, vertices(1)
    g_dx.CreateD3DVertex -1, -1, -1, 0, 0, -1, 0, 1, vertices(2)
    g_dx.CreateD3DVertex 1, -1, -1, 0, 0, -1, 1, 1, vertices(3)
        
    ' Create vertices describing the back face of the cube.
    g_dx.CreateD3DVertex -1, 1, 1, 0, 0, 1, 1, 0, vertices(4)
    g_dx.CreateD3DVertex -1, -1, 1, 0, 0, 1, 1, 1, vertices(5)
    g_dx.CreateD3DVertex 1, 1, 1, 0, 0, 1, 0, 0, vertices(6)
    g_dx.CreateD3DVertex 1, -1, 1, 0, 0, 1, 0, 1, vertices(7)
        
    ' Create vertices describing the top face of the cube.
    g_dx.CreateD3DVertex -1, 1, 1, 0, 1, 0, 0, 0, vertices(8)
    g_dx.CreateD3DVertex 1, 1, 1, 0, 1, 0, 1, 0, vertices(9)
    g_dx.CreateD3DVertex -1, 1, -1, 0, 1, 0, 0, 1, vertices(10)
    g_dx.CreateD3DVertex 1, 1, -1, 0, 1, 0, 1, 1, vertices(11)
        
    ' Create vertices describing the bottom face of the cube.
    g_dx.CreateD3DVertex -1, -1, 1, 0, -1, 0, 0, 0, vertices(12)
    g_dx.CreateD3DVertex -1, -1, -1, 0, -1, 0, 0, 1, vertices(13)
    g_dx.CreateD3DVertex 1, -1, 1, 0, -1, 0, 1, 0, vertices(14)
    g_dx.CreateD3DVertex 1, -1, -1, 0, -1, 0, 1, 1, vertices(15)
        
    ' Create vertices describing the right face of the cube.
    g_dx.CreateD3DVertex 1, 1, -1, 1, 0, 0, 0, 0, vertices(16)
    g_dx.CreateD3DVertex 1, 1, 1, 1, 0, 0, 1, 0, vertices(17)
    g_dx.CreateD3DVertex 1, -1, -1, 1, 0, 0, 0, 1, vertices(18)
    g_dx.CreateD3DVertex 1, -1, 1, 1, 0, 0, 1, 1, vertices(19)
        
    ' Create vertices describing the left face of the cube.
    g_dx.CreateD3DVertex -1, 1, -1, -1, 0, 0, 1, 0, vertices(20)
    g_dx.CreateD3DVertex -1, -1, -1, -1, 0, 0, 1, 1, vertices(21)
    g_dx.CreateD3DVertex -1, 1, 1, -1, 0, 0, 0, 0, vertices(22)
    g_dx.CreateD3DVertex -1, -1, 1, -1, 0, 0, 0, 1, vertices(23)
    
End Sub

''''''''''''''''''''''''''''''''''''''''''''''''''''''
'
' Creates a texture surface from a file-based bitmap. The name of the bitmap is passed in
' as the sFile parameter.
'
''''''''''''''''''''''''''''''''''''''''''''''''''''''
Public Function CreateTextureSurface(sFile As String) As DirectDrawSurface7
    Dim ddsTexture As DirectDrawSurface7
    Dim i As Long
    Dim bIsFound As Boolean
    
    ' Prepare the texture surface.
    Dim ddsd As DDSURFACEDESC2
    
    ddsd.lFlags = DDSD_CAPS Or DDSD_HEIGHT Or DDSD_WIDTH Or DDSD_PIXELFORMAT Or DDSD_TEXTURESTAGE
    
    ' Enumerate the texture formats, and find a device-supported texture pixel format. This
    ' simple tutorial is simply looking for a 16-bit texture. Real applications may be interested in
    ' other formats, for alpha textures, bumpmaps, etc..
    Dim TextureEnum As Direct3DEnumPixelFormats
    
    Set TextureEnum = g_d3dDevice.GetTextureFormatsEnum()
    
    For i = 1 To TextureEnum.GetCount()
        bIsFound = True
        Call TextureEnum.GetItem(i, ddsd.ddpfPixelFormat)
        
        With ddsd.ddpfPixelFormat
            ' Skip unusual modes.
            If .lFlags And (DDPF_LUMINANCE Or DDPF_BUMPLUMINANCE Or DDPF_BUMPDUDV) Then bIsFound = False
            
            ' Skip any FourCC formats.
            If .lFourCC <> 0 Then bIsFound = False
            
            'Skip alpha modes.
            If .lFlags And DDPF_ALPHAPIXELS Then bIsFound = False
            
            'We only want 16-bit formats, so skip all others.
            If .lRGBBitCount <> 16 Then bIsFound = False
        End With
        
        If bIsFound Then Exit For
        
    Next i
    
    ' If we did not find surface support, we should exit the application.
    If Not bIsFound Then
        MsgBox "Unable to locate 16-bit surface support on your hardware."
        End
    End If
        
        
    ' Turn on texture managment for the device.
    ddsd.ddsCaps.lCaps = DDSCAPS_TEXTURE
    ddsd.ddsCaps.lCaps2 = DDSCAPS2_TEXTUREMANAGE
    ddsd.lTextureStage = 0
    
    ' Create a new surface for the texture.
    Set ddsTexture = g_dd.CreateSurfaceFromFile(sFile, ddsd)
    
    ' Return the newly created texture.
    Set CreateTextureSurface = ddsTexture
    
End Function

''''''''''''''''''''''''''''''''''''''''''''''''''''''
'
' Locates the directory where the bitmaps are placed.
'
''''''''''''''''''''''''''''''''''''''''''''''''''''''
Private Sub FindMediaDir(sLoadFile As String)
    On Local Error Resume Next

    If Dir$(sLoadFile) = "" Then
        If Mid$(App.Path, 2, 1) = ":" Then
            ChDrive Mid$(App.Path, 1, 1)
        End If
        ChDir App.Path
        ChDir "..\media"
    End If
    If Dir$(sLoadFile) = "" Then
        ChDir App.Path
        ChDir "..\..\media"
    End If
    Err.Number = 0
End Sub
