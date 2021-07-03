VERSION 5.00
Begin VB.Form Form1 
   BorderStyle     =   0  'None
   Caption         =   "DD Animation"
   ClientHeight    =   5625
   ClientLeft      =   2355
   ClientTop       =   1620
   ClientWidth     =   7065
   Icon            =   "DDtut4.frx":0000
   LinkTopic       =   "Form1"
   ScaleHeight     =   375
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   471
   ShowInTaskbar   =   1  'True
End
Attribute VB_Name = "Form1"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit



Dim binit As Boolean



'NOTE THIS SAMPLES SHOWS HOW TO BLIT TO AREAS OF THE SCREEN


Dim dx As New DirectX7
Dim dd As DirectDraw7
Dim lakesurf As DirectDrawSurface7
Dim spritesurf As DirectDrawSurface7
Dim primary As DirectDrawSurface7
Dim backbuffer As DirectDrawSurface7
Dim ddsd1 As DDSURFACEDESC2
Dim ddsd2 As DDSURFACEDESC2
Dim ddsd3 As DDSURFACEDESC2
Dim ddsd4 As DDSURFACEDESC2
Dim spriteWidth As Integer
Dim spriteHeight As Integer
Dim cols As Integer
Dim rows As Integer
Dim row As Integer
Dim col As Integer
Dim currentFrame As Integer
Dim brunning As Boolean
Dim CurModeActiveStatus As Boolean
Dim bRestore As Boolean


Sub Init()
    On Local Error GoTo errOut
    
    Dim file As String
    
    Set dd = dx.DirectDrawCreate("")
    Me.Show
    
    'indicate that we dont need to change display depth
    Call dd.SetCooperativeLevel(Me.hWnd, DDSCL_FULLSCREEN Or DDSCL_ALLOWMODEX Or DDSCL_EXCLUSIVE)
    Call dd.SetDisplayMode(640, 480, 16, 0, DDSDM_DEFAULT)
    
        
    'get the screen surface and create a back buffer too
    ddsd1.lFlags = DDSD_CAPS Or DDSD_BACKBUFFERCOUNT
    ddsd1.ddsCaps.lCaps = DDSCAPS_PRIMARYSURFACE Or DDSCAPS_FLIP Or DDSCAPS_COMPLEX
    ddsd1.lBackBufferCount = 1
    Set primary = dd.CreateSurface(ddsd1)
    
    'Get the backbuffer
    Dim caps As DDSCAPS2
    caps.lCaps = DDSCAPS_BACKBUFFER
    Set backbuffer = primary.GetAttachedSurface(caps)
    backbuffer.GetSurfaceDesc ddsd4
         
    'Create DrawableSurface class form backbuffer
    
    backbuffer.SetFontTransparency True
    backbuffer.SetForeColor vbGreen
         
    ' init the surfaces
    InitSurfaces
                                                  
    binit = True
    brunning = True
    Do While brunning
        blt
        DoEvents
    Loop

errOut:
    EndIt
End Sub

Sub InitSurfaces()


    Set lakesurf = Nothing
    Set spritesurf = Nothing
    

    FindMediaDir "lake.bmp"
    
    
    'load the bitmap into a surface - lake
    ddsd2.lFlags = DDSD_CAPS Or DDSD_HEIGHT Or DDSD_WIDTH
    ddsd2.ddsCaps.lCaps = DDSCAPS_OFFSCREENPLAIN
    ddsd2.lWidth = ddsd4.lWidth
    ddsd2.lHeight = ddsd4.lHeight
    Set lakesurf = dd.CreateSurfaceFromFile("lake.bmp", ddsd2)
                        
                                                                        
    'load the bitmap into a surface - donuts
    'this bitmap has many frames of animation
    'each is 64 by 64 in layed out in cols x rows
    ddsd3.lFlags = DDSD_CAPS
    ddsd3.ddsCaps.lCaps = DDSCAPS_OFFSCREENPLAIN
    Set spritesurf = dd.CreateSurfaceFromFile("donut.bmp ", ddsd3)
    spriteWidth = 64
    spriteHeight = 64
    cols = ddsd3.lWidth / spriteWidth
    rows = ddsd3.lHeight / spriteHeight
    
    
    'use black for transparent color key which is on
    'the source bitmap -> use src keying
    Dim key As DDCOLORKEY
    key.low = 0
    key.high = 0
    spritesurf.SetColorKey DDCKEY_SRCBLT, key
End Sub


Sub blt()
    On Local Error GoTo errOut
    If binit = False Then Exit Sub
    
    Dim ddrval As Long
    Static i As Integer
    
    Dim rBack As RECT
    Dim rLake As RECT
    Dim rSprite As RECT
    Dim rSprite2 As RECT
    Dim rPrim As RECT
    
    Static a As Single
    Static x As Single
    Static y As Single
    Static t As Single
    Static t2 As Single
    Static tLast As Single
    Static fps As Single
    
    
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
        dd.RestoreAllSurfaces
        InitSurfaces ' must init the surfaces again if they we're lost
    End If
    
    'get the area of the screen where our window is
    
    rBack.Bottom = ddsd4.lHeight
    rBack.Right = ddsd4.lWidth
    
    'get the area of the bitmap we want ot blt
    rLake.Bottom = ddsd2.lHeight
    rLake.Right = ddsd2.lWidth



    
    
    'blt to the backbuffer from our  surface to
    'the screen surface such that our bitmap
    'appears over the window
    ddrval = backbuffer.BltFast(0, 0, lakesurf, rLake, DDBLTFAST_WAIT)

    
    'Calculate the frame rate
    If i = 30 Then
        If tLast <> 0 Then fps = 30 / (Timer - tLast)
        tLast = Timer
        i = 0
    End If
    i = i + 1
    Call backbuffer.DrawText(10, 10, "640x480x16 Frames per Second " + Format$(fps, "#.0"), False)
    Call backbuffer.DrawText(10, 30, "Click Screen to Exit", False)
    
             
    
    
    'calculate the angle from the center
    'at witch to place the sprite
    'calcultate wich frame# we are on in the sprite bitmap
    t2 = Timer
    If t <> 0 Then
        a = a + (t2 - t) * 40
        If a > 360 Then a = a - 360
        currentFrame = currentFrame + (t2 - t) * 40
        If currentFrame > rows * cols - 1 Then currentFrame = 0
    End If
    t = t2
    
    'calculat the x and y position of the sprite
    x = Cos((a / 360) * 2 * 3.141) * 100
    y = Sin((a / 360) * 2 * 3.141) * 100
    rSprite2.Top = y + Me.ScaleHeight / 2
    rSprite2.Left = x + Me.ScaleWidth / 2
    rSprite2.Right = rSprite2.Left + spriteWidth
    rSprite2.Bottom = rSprite2.Top + spriteHeight
    
    'from the current frame select the bitmap we want
    col = currentFrame Mod cols
    row = Int(currentFrame / cols)
    rSprite.Left = col * spriteWidth
    rSprite.Top = row * spriteHeight
    rSprite.Right = rSprite.Left + spriteWidth
    rSprite.Bottom = rSprite.Top + spriteHeight
      
   
    'blt to the backbuffer our animated sprite
    ddrval = backbuffer.BltFast(rSprite2.Left, rSprite2.Top, spritesurf, rSprite, DDBLTFAST_SRCCOLORKEY Or DDBLTFAST_WAIT)
    
    'flip the back buffer to the screen
    primary.Flip Nothing, DDFLIP_WAIT

errOut:

End Sub

Sub EndIt()
    Call dd.RestoreDisplayMode
    Call dd.SetCooperativeLevel(Me.hWnd, DDSCL_NORMAL)
    End
End Sub

Private Sub Form_Click()
    EndIt
End Sub

Private Sub Form_Load()
    Init
End Sub

Private Sub Form_Paint()
    blt
End Sub

Function ExModeActive() As Boolean
    Dim TestCoopRes As Long
    
    TestCoopRes = dd.TestCooperativeLevel
    
    If (TestCoopRes = DD_OK) Then
        ExModeActive = True
    Else
        ExModeActive = False
    End If
    
End Function


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

