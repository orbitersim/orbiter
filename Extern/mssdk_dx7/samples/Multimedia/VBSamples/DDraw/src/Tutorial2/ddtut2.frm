VERSION 5.00
Begin VB.Form DDTransparentBlt 
   BorderStyle     =   3  'Fixed Dialog
   Caption         =   "DD Transparency"
   ClientHeight    =   5070
   ClientLeft      =   630
   ClientTop       =   630
   ClientWidth     =   6495
   BeginProperty Font 
      Name            =   "Courier New"
      Size            =   72
      Charset         =   0
      Weight          =   400
      Underline       =   0   'False
      Italic          =   -1  'True
      Strikethrough   =   0   'False
   EndProperty
   Icon            =   "DDtut2.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   338
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   433
   ShowInTaskbar   =   1  'True
   Begin VB.PictureBox Picture1 
      FillStyle       =   7  'Diagonal Cross
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   18
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   5055
      Left            =   0
      ScaleHeight     =   4995
      ScaleWidth      =   6435
      TabIndex        =   0
      Top             =   0
      Width           =   6495
   End
End
Attribute VB_Name = "DDTransparentBlt"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
 Option Explicit




'NOTE THIS SAMPLES SHOWS HOW TO BLIT TO AREAS OF THE SCREEN


Dim objDX               As New DirectX7
Dim objDD               As DirectDraw7
Dim objDDLakeSurf       As DirectDrawSurface7
Dim objDDSpriteSurf     As DirectDrawSurface7
Dim objDDScreen         As DirectDrawSurface7
Dim objDDBackBuffer     As DirectDrawSurface7
Dim objDDClip           As DirectDrawClipper

Dim ddsdLake        As DDSURFACEDESC2
Dim ddsdSprite      As DDSURFACEDESC2
Dim ddsdScreen      As DDSURFACEDESC2
Dim ddsdBackBuffer  As DDSURFACEDESC2
Dim rBackBuffer     As RECT
Dim rLake           As RECT
Dim rSprite         As RECT
Dim lastX As Long
Dim lastY As Long
Dim fps As Single
Dim running As Boolean

Sub Init()
        
    Dim file As String
    
    'The empty string parameter means use the active display
    Set objDD = objDX.DirectDrawCreate("")
    Me.Show
    
    'Indicate the application will be a normal windowed application
    'with the same display depth as the current display
    Call objDD.SetCooperativeLevel(Me.hWnd, DDSCL_NORMAL)
   
    
    '----- getting a surface that represents the screen
        
    'Indicate that the ddsCaps member is valid
    ddsdScreen.lFlags = DDSD_CAPS
    
    'Ask for the primary surface (one that is visible on the screen)
    ddsdScreen.ddsCaps.lCaps = DDSCAPS_PRIMARYSURFACE
    
    'Get the primary surface object
    Set objDDScreen = objDD.CreateSurface(ddsdScreen)
    
    'Create a clipper object
    'Clippers are used to set the writable region of the screen
    Set objDDClip = objDD.CreateClipper(0)
    
    'Assoiciate the picture hwnd with the clipper
    objDDClip.SetHWnd Picture1.hWnd
    
    'Have the blts to the screen clipped to the Picture box
    objDDScreen.SetClipper objDDClip
    
    
    
    '----- creating an invisible  surface to draw to
    '      use it as a compositing surface in system memory
    
    'Indicate that we want to specify the ddscaps height and width
    'The format of the surface (bits per pixel) will be the same
    'as the primary
    ddsdBackBuffer.lFlags = DDSD_CAPS Or DDSD_HEIGHT Or DDSD_WIDTH
    
    'Indicate that we want a surface that is not visible and that
    'we want it in system memory wich is plentiful as opposed to
    'video memory
    ddsdBackBuffer.ddsCaps.lCaps = DDSCAPS_OFFSCREENPLAIN Or DDSCAPS_SYSTEMMEMORY
    
    'Specify the height and width of the surface to be the same
    'as the picture box (note unit are in pixels)
    ddsdBackBuffer.lWidth = Picture1.Width
    ddsdBackBuffer.lHeight = Picture1.Height
    
    'Create the requested surface
    Set objDDBackBuffer = objDD.CreateSurface(ddsdBackBuffer)
                                                                                
                                                                                
    'Change the current directory to be the media directory
    FindMediaDir "lake.bmp"
                                                                        
    InitSurfaces
    
    rBackBuffer.Bottom = ddsdBackBuffer.lHeight
    rBackBuffer.Right = ddsdBackBuffer.lWidth
    
    
    
    'get the area of the bitmap we want ot blt
    rLake.Bottom = ddsdLake.lHeight
    rLake.Right = ddsdLake.lWidth

    rSprite.Bottom = ddsdSprite.lHeight
    rSprite.Right = ddsdSprite.lWidth
    
    RepaintEntireBackground
                                                    
    
    running = True
    Do While running
        DoFrame
        DoEvents
    Loop
    
End Sub


'copy the backround bitmap to the background surface
Sub RepaintEntireBackground()
    
    Call objDDBackBuffer.BltFast(0, 0, objDDLakeSurf, rLake, DDBLTFAST_WAIT)
    
End Sub
Sub InitSurfaces()
    '----- loading a background image of the lake
            
    'Indicate that we want to create an offscreen surface
    'An offscreen surface is one that is available in memory
    '(video or system memory) but is not visible to the user
    ddsdLake.lFlags = DDSD_CAPS Or DDSD_WIDTH Or DDSD_HEIGHT
    ddsdLake.ddsCaps.lCaps = DDSCAPS_OFFSCREENPLAIN
    ddsdLake.lWidth = Picture1.Width
    ddsdLake.lHeight = Picture1.Height
    
    'create the surface and load lake.bmp onto the surface
    Set objDDLakeSurf = objDD.CreateSurfaceFromFile("lake.bmp", ddsdLake)
                                                                        
    'copy the background to the compositing surface
    RepaintEntireBackground
                                                                        
                                                                        
    '----- loading a sprit image (face)
    
    'load the bitmap into the second surface
        
    'specify that the ddsCaps field is valid
    ddsdSprite.lFlags = DDSD_CAPS Or DDSD_WIDTH Or DDSD_HEIGHT
    ddsdSprite.lWidth = 64
    ddsdSprite.lHeight = 64
    'indicate we want an offscreen surface
    ddsdSprite.ddsCaps.lCaps = DDSCAPS_OFFSCREENPLAIN
    
    'create the surface
    'since we are not specifying the height and width
    'the bitmap will be the same size as the bitmap
    Set objDDSpriteSurf = objDD.CreateSurfaceFromFile("disk1.bmp", ddsdSprite)
    
    
    '----- setting the transparent color of the sprite
    
    
    Dim key As DDCOLORKEY
    
    'You can set a range of colors to be the
    'here we set it to white
    'CreateColor take 3 singles representing ranging from 0 to 1
    'for red green and blue components of the color
    key.low = 0
    key.high = 0
    
    
    'Assign the transparent color to the sprite object
    'DDCKEY_SRCBLT specifies that when a blt is done the
    'transparent color is associated with the surface being
    'blitted and not the one being blitted to
    objDDSpriteSurf.SetColorKey DDCKEY_SRCBLT, key
    

End Sub
Sub DoFrame()
    
    
    Dim ddrval As Long
    Dim rPrim As RECT
    Dim x As Single
    Dim y As Single
    
    Static a As Single
    Static t1 As Single
    Static t2 As Single
    Static i As Integer
    Static tLast As Single
    Static tNow As Single
    
                
    'calculate the angle of where we place the sprite
    t2 = Timer
    If t1 <> 0 Then
        
        a = a + (t2 - t1) * 100
        If a > 360 Then a = a - 360
    End If
    t1 = t2
        
    
    Dim bRestore As Boolean
    ' this will keep us from trying to blt in case we lose the surfaces (another fullscreen app takes over)
    bRestore = False
    Do Until ExModeActive
        DoEvents
        bRestore = True
    Loop
    
    ' if we lost and got back the surfaces, then restore them
    DoEvents
    If bRestore Then
        bRestore = False
        objDD.RestoreAllSurfaces
        InitSurfaces ' must init the surfaces again if they we're lost
    End If
    
    'calculate FPS
    i = i + 1
    If i = 30 Then
        tNow = Timer
        If tNow <> tLast Then
            fps = 30 / (Timer - tLast)
            tLast = Timer
            i = 0
            Me.Caption = "DD Transparency    Frames per Second =" + Format$(fps, "#.0")
        End If
    End If

    
    'calculate the x y coordinate of where we place the sprite
    x = Cos((a / 360) * 2 * 3.141) * Picture1.Width / 8
    y = Sin((a / 360) * 2 * 3.141) * Picture1.Height / 8
    x = x + Picture1.Width / 2
    y = y + Picture1.Height / 2
    
    'clean up background from last frame
    'by only reparing the background where it needs to
    'be you wont need to reblit the whole thing
    Dim rClean As RECT
    If lastX <> 0 Then
        rClean.Left = lastX
        rClean.Top = lastY
        rClean.Right = lastX + ddsdSprite.lWidth
        rClean.Bottom = lastY + ddsdSprite.lHeight
        Call objDDBackBuffer.BltFast(lastX, lastY, objDDLakeSurf, rClean, DDBLTFAST_WAIT)
    End If
    
    lastX = x
    lastY = y
    'blt to the backbuffer from our  sprite
    'use the color key on the source - (our sprite)
    'wait for the blt to finish before moving one
    Dim rtemp As RECT
    rtemp.Left = x
    rtemp.Top = y
    rtemp.Right = x + ddsdSprite.lWidth
    rtemp.Bottom = y + ddsdSprite.lHeight
    
    objDDBackBuffer.Blt rtemp, objDDSpriteSurf, rSprite, DDBLT_KEYSRC Or DDBLT_WAIT
    
    '
    'Call objDDBackBuffer.BltFast(x, y, objDDSpriteSurf, rSprite, DDBLTFAST_SRCCOLORKEY)
    'Call objDDBackBuffer.BltFast(x, y, objDDSpriteSurf, rSprite, DDBLTFAST_SRCCOLORKEY Or DDBLTFAST_WAIT)
   ' Call objDDBackBuffer.BltFast(x, y, objDDSpriteSurf, rSprite, DDBLTFAST_WAIT)
        
    
    
        
    'Get the position of our picture box in screen coordinates
    Call objDX.GetWindowRect(Picture1.hWnd, rPrim)
    
    
    'blt our back buffer to the screen
    Call objDDScreen.Blt(rPrim, objDDBackBuffer, rBackBuffer, DDBLT_WAIT)
        



End Sub


Private Sub Form_Load()
    Init
End Sub


Private Sub Form_Resize()
    'This tutorial does not handle resize
    'To resize we would need to recreate the backbuffer
    'The lake bitmap would have to be larger as well
    'for the dirty rectangle clean up to be correct.
    'see sprite engine sample for more ideas.
    
End Sub

Private Sub Form_Unload(Cancel As Integer)
    running = False
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
    DoFrame
End Sub


Function ExModeActive() As Boolean
    Dim TestCoopRes As Long
    
    TestCoopRes = objDD.TestCooperativeLevel
    
    If (TestCoopRes = DD_OK) Then
        ExModeActive = True
    Else
        ExModeActive = False
    End If
    
End Function

