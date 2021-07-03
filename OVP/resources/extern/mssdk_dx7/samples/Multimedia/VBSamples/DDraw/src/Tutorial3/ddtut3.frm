VERSION 5.00
Begin VB.Form DDFullScreen 
   BorderStyle     =   0  'None
   Caption         =   "Form1"
   ClientHeight    =   5625
   ClientLeft      =   885
   ClientTop       =   585
   ClientWidth     =   7065
   Icon            =   "DDtut3.frx":0000
   LinkTopic       =   "Form1"
   ScaleHeight     =   375
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   471
   ShowInTaskbar   =   1  'True
End
Attribute VB_Name = "DDFullScreen"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit







'NOTE THIS SAMPLES SHOWS HOW TO USE FULL SCREEN FEATURES


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
Dim brunning As Boolean
Dim binit As Boolean
Dim CurModeActiveStatus As Boolean
Dim bRestore As Boolean

Sub Init()
    On Local Error GoTo errOut
            
    Dim file As String
    
    Set dd = dx.DirectDrawCreate("")
    Me.Show
    
    'indicate that we dont need to change display depth
    Call dd.SetCooperativeLevel(Me.hWnd, DDSCL_FULLSCREEN Or DDSCL_ALLOWMODEX Or DDSCL_EXCLUSIVE)
    
    dd.SetDisplayMode 640, 480, 16, 0, DDSDM_DEFAULT
    

            
    'get the screen surface and create a back buffer too
    ddsd1.lFlags = DDSD_CAPS Or DDSD_BACKBUFFERCOUNT
    ddsd1.ddsCaps.lCaps = DDSCAPS_PRIMARYSURFACE Or DDSCAPS_FLIP Or DDSCAPS_COMPLEX
    ddsd1.lBackBufferCount = 1
        
    Set primary = dd.CreateSurface(ddsd1)
    
    
    
    Dim caps As DDSCAPS2
    caps.lCaps = DDSCAPS_BACKBUFFER
    Set backbuffer = primary.GetAttachedSurface(caps)
    
    backbuffer.GetSurfaceDesc ddsd4
    
    

    'We create a DrawableSurface class from our backbuffer
    'that makes it easy to draw text
    backbuffer.SetForeColor vbGreen
    backbuffer.SetFontTransparency True
    
    ' init the surfaces
    InitSurfaces
                                                    
    binit = True
    brunning = True
    Do While brunning
        blt
        DoEvents
    Loop
    
    
errOut:
    
    EndIT
    
End Sub

Sub InitSurfaces()

    Set lakesurf = Nothing
    Set spritesurf = Nothing

    FindMediaDir "lake.bmp"
    
    'load the bitmap into the second surface same size
    'as our back buffer
    ddsd2.lFlags = DDSD_CAPS Or DDSD_HEIGHT Or DDSD_WIDTH
    ddsd2.ddsCaps.lCaps = DDSCAPS_OFFSCREENPLAIN
    ddsd2.lWidth = ddsd4.lWidth
    ddsd2.lHeight = ddsd4.lHeight
    Set lakesurf = dd.CreateSurfaceFromFile("lake.bmp", ddsd2)
                        
                                                                        
    'load the bitmap into the second surface
    ddsd3.lFlags = DDSD_CAPS
    ddsd3.ddsCaps.lCaps = DDSCAPS_OFFSCREENPLAIN
    Set spritesurf = dd.CreateSurfaceFromFile("disk1.bmp", ddsd3)
    
    'use black for transparent color key
    Dim key As DDCOLORKEY
    key.low = 0
    key.high = 0
    spritesurf.SetColorKey DDCKEY_SRCBLT, key
End Sub


Sub blt()
    On Local Error GoTo errOut
    If binit = False Then Exit Sub
    
    Dim rSprite As RECT
    Dim rSprite2 As RECT
    Dim rPrim As RECT
    
    Static i As Integer
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

    'get the rectangle for our source sprite
    rSprite.Bottom = ddsd3.lHeight
    rSprite.Right = ddsd3.lWidth
    
    

    'calculate an angle to place the sprite
    t2 = Timer
    If t <> 0 Then
        a = a + (t - t2) * 80
        If a > 360 Then a = a - 360
    End If
    t = t2
    
    'caculate the center x y position
    x = Cos((a / 360) * 2 * 3.141) * 100
    y = Sin((a / 360) * 2 * 3.141) * 100
    
    'where on the screen do you want the sprite
    rSprite2.Top = y + Me.ScaleHeight / 2
    rSprite2.Left = x + Me.ScaleWidth / 2
                    
                        
    'paint the background onto our back buffer
    Dim rLake As RECT, rback As RECT
    rLake.Bottom = ddsd2.lHeight
    rLake.Right = ddsd2.lWidth
    rback.Bottom = ddsd4.lHeight
    rback.Right = ddsd4.lWidth
    Call backbuffer.BltFast(0, 0, lakesurf, rLake, DDBLTFAST_WAIT)
    
    'Calculate the frame rate
    If i = 30 Then
        If tLast <> 0 Then fps = 30 / (Timer - tLast)
        tLast = Timer
        i = 0
    End If
    i = i + 1
    Call backbuffer.DrawText(10, 10, "640x480x16 Frames per Second " + Format$(fps, "#.0"), False)
    Call backbuffer.DrawText(10, 30, "Click Screen to Exit", False)
    
    'blt to the backbuffer from our  surface
    Call backbuffer.BltFast(rSprite2.Left, rSprite2.Top, spritesurf, rSprite, DDBLTFAST_SRCCOLORKEY Or DDBLTFAST_WAIT)
    

    
    'flip the backbuffer to the screen
    primary.Flip Nothing, DDFLIP_WAIT
    

errOut:

End Sub

Sub EndIT()
    Call dd.RestoreDisplayMode
    Call dd.SetCooperativeLevel(Me.hWnd, DDSCL_NORMAL)
    End
End Sub

Private Sub Form_Click()
    EndIT
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

