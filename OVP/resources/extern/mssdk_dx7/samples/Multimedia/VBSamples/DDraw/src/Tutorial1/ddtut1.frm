VERSION 5.00
Begin VB.Form DDTut1 
   Caption         =   "DirectDraw Tutorial 1"
   ClientHeight    =   5010
   ClientLeft      =   570
   ClientTop       =   690
   ClientWidth     =   6675
   Icon            =   "DDTut1.frx":0000
   LinkTopic       =   "Form1"
   ScaleHeight     =   334
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   445
   Begin VB.PictureBox Picture1 
      ClipControls    =   0   'False
      Height          =   4452
      Left            =   0
      ScaleHeight     =   4395
      ScaleWidth      =   5595
      TabIndex        =   0
      Top             =   0
      Width           =   5652
   End
End
Attribute VB_Name = "DDTut1"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit



'Module level variables
Dim objDX As New DirectX7
Dim objDD As DirectDraw7
Dim objDDSurf As DirectDrawSurface7
Dim objDDPrimSurf As DirectDrawSurface7
Dim ddsd1 As DDSURFACEDESC2
Dim ddsd2 As DDSURFACEDESC2
Dim ddClipper As DirectDrawClipper

Dim bInit As Boolean
Dim pal As DirectDrawPalette

Private Sub Form_Load()
    init
End Sub

Sub init()
    
    'Initialization procedure
      
    'The empty string parameter means to use the active display driver
    Set objDD = objDX.DirectDrawCreate("")
    'Notice that the show event calls Form_Resize
        
    'Indicate this app will be a normal windowed app
    'with the same display depth as the current display
    Call objDD.SetCooperativeLevel(Me.hWnd, DDSCL_NORMAL)
    
    
        
    
    'Indicate that the ddsCaps member is valid in this type
    ddsd1.lFlags = DDSD_CAPS
    'This surface is the primary surface (what is visible to the user)
    ddsd1.ddsCaps.lCaps = DDSCAPS_PRIMARYSURFACE
    'Your creating the primary surface now with the surface description you just set
    Set objDDPrimSurf = objDD.CreateSurface(ddsd1)
    
    'Call the FindMediaDir procedure
    FindMediaDir "lake.bmp"
    
    'Now let's set the second surface description
    ddsd2.lFlags = DDSD_CAPS
    'This is going to be a plain off-screen surface
    ddsd2.ddsCaps.lCaps = DDSCAPS_OFFSCREENPLAIN
    'Now we create the off-screen surface
    Set objDDSurf = objDD.CreateSurfaceFromFile("lake.bmp", ddsd2)
    
    Set ddClipper = objDD.CreateClipper(0)
    ddClipper.SetHWnd Picture1.hWnd
    objDDPrimSurf.SetClipper ddClipper
    'Yes it has been initialized and is ready to blit
    bInit = True
    
    'Ok now were ready to blit this thing, call the blt procedure
    blt

End Sub
Private Sub Form_Resize()
    
    'This procedure is called by the me.show event or when
    'The form is resized during runtime.
    'Since DX uses pixels and VB uses twips this procedure
    'Syncs up the two scales
    'Remember to change the ScaleMode property on the
    'Form to Pixels. Notice the Width and Height of the form
    'Stay in twips even after you change the ScaleMode, but
    'The ScaleWidth and the ScaleHeight are now in pixels.
    Picture1.Width = Me.ScaleWidth
    Picture1.Height = Me.ScaleHeight
    blt
End Sub
Sub blt()
        
    'Has it been initialized? If not let's get out of this procedure
    If bInit = False Then Exit Sub
    
    'Some local variables
    Dim ddrval As Long
    Dim r1 As RECT
    Dim r2 As RECT
    
    'Gets the bounding rect for the entire window handle, stores in r1
    Call objDX.GetWindowRect(Picture1.hWnd, r1)
    
    r2.Bottom = ddsd2.lHeight
    r2.Right = ddsd2.lWidth
    
    ddrval = objDDPrimSurf.blt(r1, objDDSurf, r2, DDBLT_WAIT)
    
End Sub


Sub FindMediaDir(sFile As String)
    On Local Error Resume Next
    If Dir$(sFile) <> "" Then Exit Sub
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


Private Sub Picture1_Paint()
    'This procedure is called during runtime when the form
    'is moved or resized.
    objDD.RestoreAllSurfaces
    init
    blt
End Sub
