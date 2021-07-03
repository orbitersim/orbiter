Attribute VB_Name = "FireMod"
Option Explicit
Public DX As New DirectX7
Public DDraw As DirectDraw7
Public DDSFront As DirectDrawSurface7
Public DDSFrontDesc As DDSURFACEDESC2
Public DDSBack As DirectDrawSurface7
Public DDSBackDesc As DDSURFACEDESC2
Public Clipper As DirectDrawClipper
Dim Pict() As Byte
Dim AlphaRect As RECT
Dim X As Long, Y As Long
Dim Temp As Long
Dim Index As Long
Dim Index2 As Long
Dim Pos As Long
Dim PosPlus1 As Long
Dim PosPlus2 As Long
Dim PosPlus3 As Long
Public Pal(255) As PALETTEENTRY
Public Palette As DirectDrawPalette
Public BlitRect As RECT
Public FullSize As Boolean
Public ExitLoop As Boolean
Dim Accum As Long
Dim Msg(9) As String
Dim Counter As Long
Dim MsgIndex As Long
Dim bDrawText As Boolean
Dim lastTime As Long
Dim XPos As Long, YPos As Long
Dim wait As Long
Dim Angle As Single
Dim Flag As Boolean
Dim Count As Long
Dim CurModeActiveStatus As Boolean
Dim bRestore As Boolean
Dim Mode As Boolean
Private Sub Main()
    
    InitializeDX

    'On Error Resume Next
    DDSBack.SetForeColor RGB(255, 255, 255)
    DDSBack.SetFont MainForm.Font
    
    Msg(0) = "A Simple Fiery Demo"
    Msg(1) = "Demonstrating"
    Msg(2) = "Direct Access"
    Msg(3) = "To Video Memory"
    Msg(4) = "With VB Arrays"
    Msg(5) = "{Esc} to exit"

    'setup 8bit palette
    
    For Index = 0 To 84
        Pal(Index + 1).red = Index * 3 + 3
        Pal(Index + 1).green = 0
        Pal(Index + 1).blue = 0
    
        Pal(Index + 86).red = 255
        Pal(Index + 86).green = Index * 3 + 3
        Pal(Index + 86).blue = 0
    
        Pal(Index + 171).red = 255
        Pal(Index + 171).green = 255
        Pal(Index + 171).blue = Index * 3 + 3
    Next
    
    Set Palette = DDraw.CreatePalette(DDPCAPS_8BIT _
        Or DDPCAPS_ALLOW256, Pal())
    DDSFront.SetPalette Palette
    
    
    AlphaRect.Right = DDSBackDesc.lWidth - 1
    AlphaRect.Bottom = DDSBackDesc.lHeight - 1
    
    'Lock must have corresponding unlock...
    DDSBack.Lock AlphaRect, DDSBackDesc, DDLOCK_WAIT, 0
    
    'Get locked array gives you access to
    'a byte array that represents video memory.
    'be aware that the color information is orgnanized
    'differntly for various color depths.
    '
    DDSBack.GetLockedArray Pict()
    
    
    For X = 0 To 639
        For Y = 0 To 479
            Pict(X, Y) = 0
        Next
    Next
    
    'Corresponding unlock
    DDSBack.Unlock AlphaRect
    
    While Not ExitLoop
        
        Mode = ExModeActive
        
        'DoEvents
        bRestore = False
        Do Until ExModeActive
            DoEvents
            bRestore = True
        Loop
        DoEvents
        
        If bRestore Then
            bRestore = False
            DDraw.RestoreAllSurfaces
        End If
        
        'Lock
        DDSBack.Lock AlphaRect, DDSBackDesc, DDLOCK_WAIT, 0
        
        'GetVBArray
        DDSBack.GetLockedArray Pict()
    
        For Y = 0 To 479
            Pict(0, Y) = 0
            Pict(639, Y) = 0
        Next
        For X = 0 To 639
            Pict(X, 477) = Rnd * 220 + 35
            Pict(X, 478) = Rnd * 220 + 35
            Pict(X, 479) = Rnd * 220 + 35
        Next
        
        Accum = 0
        For X = 1 To 638
            For Y = 0 To 477
                Accum = (Accum + Pict(X, Y + 1) _
                    + Pict(X, Y + 2) _
                    + Pict(X + 1, Y + 1) _
                    + Pict(X - 1, Y + 1)) \ 5
                    If Accum < 0 Then
                        Accum = 0
                    ElseIf Accum > 255 Then
                        Accum = 255
                    End If
                Pict(X, Y) = Accum
            Next
        Next
        
        For X = 0 To 639
            Pict(X, 0) = 0
            Pict(X, 1) = 0
        Next
        X = Rnd * 639
        For Y = 50 To 439
        Next
        'Unlock
        DDSBack.Unlock AlphaRect
        
        If DX.TickCount() - lastTime > wait Then
            If Counter = 0 Then
                bDrawText = True
                Counter = 1
                XPos = Rnd * 200
                YPos = 300 + Rnd * 140
                wait = 400
            ElseIf Counter = 1 Then
                MsgIndex = MsgIndex + 1
                If MsgIndex > 5 Then MsgIndex = 0
                bDrawText = False
                Counter = 0
                wait = 2000
            End If
            lastTime = DX.TickCount
        End If
        
        'Draw Text to the backbuffer
        If bDrawText Then
            On Error Resume Next
            DDSBack.DrawText XPos, YPos, Msg(MsgIndex), False
            On Error GoTo 0
        End If
        
        MainForm.Form_Paint
        
    Wend
    
    TerminateDX
    End
End Sub
Function ExModeActive() As Boolean
    Dim TestCoopRes As Long
    TestCoopRes = DDraw.TestCooperativeLevel
    Select Case TestCoopRes
        Case DDERR_NOEXCLUSIVEMODE
            ExModeActive = False
        Case DD_OK
            ExModeActive = True
    End Select
End Function
Public Sub InitializeDX()
     
    MainForm.Left = 0
    MainForm.Top = 0
    MainForm.Height = 640 * Screen.TwipsPerPixelY
    MainForm.Width = 480 * Screen.TwipsPerPixelX
    MainForm.Show
    
    Set DDraw = DX.DirectDrawCreate("")
    
    DDraw.SetCooperativeLevel MainForm.hWnd, DDSCL_EXCLUSIVE Or DDSCL_FULLSCREEN ' DDSCL_NORMAL
    
   ' You can set the cooperative level to normal if you are already in an 8bit mode
   ' and want to debug the code
   ' DDraw.SetCooperativeLevel MainForm.hWnd, DDSCL_NORMAL
   
    DDraw.SetDisplayMode 640, 480, 8, 0, DDSDM_DEFAULT
    
    With DDSFrontDesc
        .lFlags = DDSD_CAPS
        .ddsCaps.lCaps = DDSCAPS_PRIMARYSURFACE 'Or DDSCAPS_SYSTEMMEMORY
    End With
    With DDSBackDesc
        .ddsCaps.lCaps = DDSCAPS_SYSTEMMEMORY
        .lFlags = DDSD_CAPS Or DDSD_WIDTH Or DDSD_HEIGHT
        .lWidth = 640
        .lHeight = 480
    End With
    Set DDSFront = DDraw.CreateSurface(DDSFrontDesc)
    Set DDSBack = DDraw.CreateSurface(DDSBackDesc)
    Set Clipper = DDraw.CreateClipper(0)
    Clipper.SetHWnd MainForm.hWnd
    DDSFront.SetClipper Clipper
    DDSBack.SetClipper Clipper
    DoEvents
    Exit Sub
ERRoUT:
    If Not (DDraw Is Nothing) Then
        DDraw.RestoreDisplayMode
        DDraw.SetCooperativeLevel MainForm.hWnd, DDSCL_NORMAL
        DoEvents
    End If
    MsgBox "Unable to initialize DirectDraw " + Chr(13) + "Your display card may not support 640x480x8 resolution " + Chr(13) + "required for this sample"
    End
End Sub
Public Sub TerminateDX()
    DDraw.RestoreDisplayMode
    DDraw.SetCooperativeLevel MainForm.hWnd, DDSCL_NORMAL
    DoEvents
    Set Clipper = Nothing
    Set DDSBack = Nothing
    Set DDSFront = Nothing
    Set DDraw = Nothing
    Set DX = Nothing
End Sub
