Attribute VB_Name = "modDDBlend"
Declare Function ShowCursor& Lib "user32" (ByVal bShow As Long)

Public Const NumParticles = 10000
Public Const MaxParticles = 100000

Type ParticleType
    X As Long
    Y As Long
    Angle As Single
    Speed As Long
    Decay As Single
    HalfLife As Single
    AngleAdjustment As Single
End Type
Type DisplayModeType
    Width As Long
    Height As Long
    BPP As Long
End Type
Public DX As New DirectX7
Public DDraw As DirectDraw7
Public DDSFront As DirectDrawSurface7
Public DDSFrontDesc As DDSURFACEDESC2
Public DDSBack As DirectDrawSurface7
Public DDSBackDesc As DDSURFACEDESC2
Public DDSDisplayDesc As DDSURFACEDESC2
Public DisplayModesEnum As DirectDrawEnumModes

Public ScreenWidth As Long, ScreenHeight As Long
Public ExitLoop As Boolean
Public MX As Long, MY As Long
Public ParticleCount As Long
Public BlurFactor As Long
Public PaletteNum As Long
Public CurrentDisplayMode As Long
Public DisplayMode() As DisplayModeType
Public Start As Boolean

Dim Pict() As Byte
Dim AlphaRect As RECT
Dim X As Long, Y As Long
Dim Index As Long
Dim Pal(255) As PALETTEENTRY
Dim Pal2(255) As PALETTEENTRY
Dim Palette As DirectDrawPalette
Dim Accum As Long
Dim Particle(MaxParticles) As ParticleType
Dim PE1 As PALETTEENTRY
Dim ModeIndex As Long
Dim Mode As Boolean
Dim bRestore As Boolean

Private Sub Main()
        
    Dim frmInstructions As Instructions
    
    Set frmInstructions = New Instructions
    frmInstructions.Show
    'Wait for the instructions to be read...
    Do Until Start
        DoEvents
    Loop
    Set frmInstructions = Nothing
    
    'Set our default number of particles and blur factor
    ParticleCount = NumParticles
    BlurFactor = 1
    
    'Win32 API call:
    'Turn the mouse cursor off.  We'll turn it back on before leaving.
    ShowCursor False
    
    'Do all of our basic DX initialization.  We'll call this again later
    'when the user decides to change video modes.
    InitializeDX
    DDSBack.SetForeColor RGB(255, 255, 255)
            
    'Setup all of our particles.  We're going to setup more
    'particles than we will initially use... 100,000 in all.
    For Index = 0 To MaxParticles
        With Particle(Index)
            .Speed = 1 + CInt(Rnd * 3)
            .Angle = Rnd * 6.28 ' 2 pi for a full range of directions
            .X = Rnd * ((ScreenWidth) - 2) + 1 ' initial starting point
            .Y = Rnd * ((ScreenHeight) - 2) + 1
            .Decay = 1  ' the amount of hitpoints left in a particle
            .HalfLife = Rnd / 20 ' will allow a particle to be recycled
            .AngleAdjustment = Rnd / 20 ' will produce a spiraling particle
        End With
    Next
    
    ' setup the 8-bit 256c color palette
    Pal(0).blue = 255
    For Index = 1 To 32
        Pal(Index).red = Index * 8 - 1
        Pal(Index).green = Index * 8 - 1
        Pal(Index).blue = 256 - Index * 8
    
        Pal(Index + 32).red = 255
        Pal(Index + 32).green = 256 - Index * 8
        Pal(Index + 32).blue = 0
        
        Pal(Index + 64).red = 256 - Index * 8
        Pal(Index + 64).green = 0
        Pal(Index + 64).blue = Index * 8 - 1
    
        Pal(Index + 96).red = Index * 8 - 1
        Pal(Index + 96).green = Index * 8 - 1
        Pal(Index + 96).blue = 255
        
        Pal(Index + 128).red = 256 - Index * 8
        Pal(Index + 128).green = 256 - Index * 8
        Pal(Index + 128).blue = 255
            
        Pal(Index + 160).red = Index * 8 - 1
        Pal(Index + 160).green = Index * 8 - 1
        Pal(Index + 160).blue = 256 - Index * 8
    
        Pal(Index + 192).red = 255
        Pal(Index + 192).green = 255
        Pal(Index + 192).blue = Index * 8 - 1
        
        Pal(Index + 223).red = 256 - Index * 8
        Pal(Index + 223).green = 256 - Index * 8
        Pal(Index + 223).blue = 255
    Next
    Set Palette = DDraw.CreatePalette(DDPCAPS_8BIT _
        Or DDPCAPS_ALLOW256, Pal())
    ' setup an alternative pallete here based on the original
    ' every other color will be black to produce a trippy little effect
    For Index = 0 To 255 Step 2
        Pal2(Index).red = Pal(Index).red
        Pal2(Index).green = Pal(Index).green
        Pal2(Index).blue = Pal(Index).blue
    Next
    ' attaching the original pallete to the front surface...
    ' in other words, activating it.
    DDSFront.SetPalette Palette
    
    ' Setup a rectangle for use in our locks.
    AlphaRect.Right = DDSBackDesc.lWidth - 1
    AlphaRect.Bottom = DDSBackDesc.lHeight - 1
    ' Clear the surface pixel by pixel
    ' GetLockedArray offers direct access to surfaces.  For best
    ' performance use it with surfaces stored in system memory, not
    ' video memory.
    DDSBack.Lock AlphaRect, DDSBackDesc, DDLOCK_WAIT, 0
    DDSBack.GetLockedArray Pict()
    For X = 0 To ScreenWidth - 1
        For Y = 0 To ScreenHeight - 1
            Pict(X, Y) = 0
        Next
    Next
    DDSBack.Unlock AlphaRect
    ' start our main loop
    While Not ExitLoop
        ' deal with the possibility that the user may switch out of
        ' exclusive mode and back in again.  We will restore all
        ' surfaces if this occurs.
        Mode = ExModeActive
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
        ' cycle whichever color pallete is currently active
        Select Case PaletteNum
            Case 0
                PE1 = Pal(0)
                For Index = 0 To 254
                    Pal(Index) = Pal(Index + 1)
                Next
                Pal(255) = PE1
                Set Palette = DDraw.CreatePalette(DDPCAPS_8BIT _
                Or DDPCAPS_ALLOW256, Pal())
            Case 1
                PE1 = Pal2(0)
                For Index = 0 To 254
                    Pal2(Index) = Pal2(Index + 1)
                Next
                Pal2(255) = PE1
                Set Palette = DDraw.CreatePalette(DDPCAPS_8BIT _
                Or DDPCAPS_ALLOW256, Pal2())
        End Select
        ' implement the new pallete
        DDSFront.SetPalette Palette
        ' lock the surface to prepare the main effect
        DDSBack.Lock AlphaRect, DDSBackDesc, DDLOCK_WAIT, 0
        DDSBack.GetLockedArray Pict()
        ' loop through the active particles (this number may change)
        For Index = 0 To ParticleCount
            With Particle(Index)
                .Decay = .Decay - .HalfLife ' recycle
                If .Decay <= 0 Then
                    .Decay = 1
                    .X = MX
                    .Y = MY
                End If
                .Angle = .Angle + .AngleAdjustment ' spiral
                If .Angle >= 6.28 Then Angle = 0
                .X = .X + Cos(.Angle) * .Speed ' update position
                .Y = .Y + Sin(.Angle) * .Speed
                If (.X > ScreenWidth - 2) Or (.X < 2) Then ' recycle
                    .X = MX
                    .Y = MY
                    .Angle = Rnd * 6.28
                ElseIf (.Y > ScreenHeight - 2) Or (.Y < 2) Then
                    .X = MX
                    .Y = MY
                    .Angle = Rnd * 6.28
                End If
                    Pict(.X, .Y) = .Speed * 16 + 186 ' plot pixel
            End With
        Next
        ' create a blurring effect
        For Index = 1 To BlurFactor
            For X = 2 To ScreenWidth - 2
               For Y = 2 To (ScreenHeight - 2)
                    Accum = 0
                    ' the "Accum = Accum + " allows VB to cast the bytes
                    ' being added as longs... or something like that ;-)
                    ' otherwise we'd end up with an overflow
                    Accum = Accum + Pict(X, Y) _
                        + Pict(X, Y + 1) _
                        + Pict(X, Y - 1) _
                        + Pict(X + 1, Y) _
                        + Pict(X - 1, Y) _
                        + Pict(X + 1, Y + 1) _
                        + Pict(X - 1, Y - 1) _
                        + Pict(X + 1, Y - 1) _
                        + Pict(X - 1, Y + 1)
                    
                    Accum = Accum \ 9 ' average the pixels
                    Pict(X, Y) = Accum
                Next
            Next
        Next
        ' remove artifacts from the sides of the screen
        For Index = 0 To ScreenWidth - 1
            Pict(Index, 0) = 127
            Pict(Index, ScreenHeight - 1) = 127
            Pict(Index, 1) = 127
            Pict(Index, ScreenHeight - 2) = 127
        Next
        For Index = 0 To ScreenHeight - 1
            Pict(0, Index) = 127
            Pict(ScreenWidth - 1, Index) = 127
            Pict(1, Index) = 127
            Pict(ScreenWidth - 2, Index) = 127
        Next
        DDSBack.Unlock AlphaRect
        'update the display
        MainForm.Form_Paint
    Wend
    TerminateDX ' remove most of DX
    ShowCursor True
    End
End Sub
' tests to see if we are in exclusive mode
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
    MainForm.Height = ScreenWidth * Screen.TwipsPerPixelY
    MainForm.Width = ScreenHeight * Screen.TwipsPerPixelX
    MainForm.Show
    DoEvents
    ' create our directdraw object
    Set DDraw = DX.DirectDrawCreate("")
    ' set an initial cooperativelevel of normal
    DDraw.SetCooperativeLevel MainForm.hWnd, DDSCL_NORMAL
    ' query the display for a list of supported 8-bit modes
    ' we'll put these modes into an array for later use.
    If DisplayModesEnum Is Nothing Then
        Set DisplayModesEnum = DDraw.GetDisplayModesEnum(0, DDSDisplayDesc)
        ReDim DisplayMode(DisplayModesEnum.GetCount - 1)
        ModeIndex = -1
        For Index = 1 To DisplayModesEnum.GetCount
            DisplayModesEnum.GetItem Index, DDSDisplayDesc
            If DDSDisplayDesc.ddpfPixelFormat.lRGBBitCount = 8 Then
                ModeIndex = ModeIndex + 1
                With DisplayMode(ModeIndex)
                    .Width = DDSDisplayDesc.lWidth
                    .Height = DDSDisplayDesc.lHeight
                    .BPP = DDSDisplayDesc.ddpfPixelFormat.lRGBBitCount
                End With
            End If
        Next
        ReDim Preserve DisplayMode(ModeIndex)
        ' if we can't find a display mode then we'll have to exit.
        If ModeIndex = -1 Then GoTo DXErr
    End If
    ' switch the screen mode to whatever should be the current mode
    ScreenWidth = DisplayMode(CurrentDisplayMode).Width
    ScreenHeight = DisplayMode(CurrentDisplayMode).Height
    DDraw.SetCooperativeLevel MainForm.hWnd, DDSCL_EXCLUSIVE Or DDSCL_FULLSCREEN
    DDraw.SetDisplayMode ScreenWidth, ScreenHeight, 8, 0, DDSDM_DEFAULT
    ' setup the surfaces
    With DDSFrontDesc
        .lFlags = DDSD_CAPS
        .ddsCaps.lCaps = DDSCAPS_PRIMARYSURFACE
    End With
    With DDSBackDesc
        .lFlags = DDSD_CAPS Or DDSD_WIDTH Or DDSD_HEIGHT
        .ddsCaps.lCaps = DDSCAPS_OFFSCREENPLAIN Or DDSCAPS_SYSTEMMEMORY
        .lWidth = ScreenWidth
        .lHeight = ScreenHeight
    End With
    Set DDSFront = DDraw.CreateSurface(DDSFrontDesc)
    Set DDSBack = DDraw.CreateSurface(DDSBackDesc)
    DoEvents
    Exit Sub
DXErr:
    TerminateDX
    MsgBox "Cannot find an 8-bit display mode."
    ShowCursor True
    End
End Sub
' terminate DX objects so we can recreate them again later.
Public Sub TerminateDX()
    DDraw.SetCooperativeLevel MainForm.hWnd, DDSCL_NORMAL
    DoEvents
    Set Clipper = Nothing
    Set DDSBack = Nothing
    Set DDSFront = Nothing
    Set DDraw = Nothing
    Set DX = Nothing
End Sub
