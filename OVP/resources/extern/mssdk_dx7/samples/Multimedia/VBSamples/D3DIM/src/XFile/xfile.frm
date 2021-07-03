VERSION 5.00
Object = "{F9043C88-F6F2-101A-A3C9-08002B2F49FB}#1.2#0"; "comdlg32.ocx"
Object = "{D0B8DDCE-E796-11D2-A21E-00C04F68AD33}#1.1#0"; "IMControl.ocx"
Begin VB.Form XFileLoader 
   Caption         =   "Direct X File Loader"
   ClientHeight    =   3210
   ClientLeft      =   165
   ClientTop       =   735
   ClientWidth     =   4680
   Icon            =   "XFile.frx":0000
   LinkTopic       =   "Form1"
   ScaleHeight     =   214
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   312
   StartUpPosition =   3  'Windows Default
   Begin DirectXIMControl.IMCanvas IMCanvas1 
      Height          =   1935
      Left            =   0
      TabIndex        =   0
      Top             =   0
      Width           =   2295
      _ExtentX        =   4048
      _ExtentY        =   3413
   End
   Begin MSComDlg.CommonDialog CDiag 
      Left            =   4080
      Top             =   2640
      _ExtentX        =   847
      _ExtentY        =   847
      _Version        =   393216
   End
   Begin VB.Menu File 
      Caption         =   "&File"
      Begin VB.Menu OpenNewXFile 
         Caption         =   "&Open .X File"
      End
      Begin VB.Menu Exit 
         Caption         =   "E&xit"
      End
   End
   Begin VB.Menu FillOptions 
      Caption         =   "&Fill Options"
      Begin VB.Menu OptionsSolid 
         Caption         =   "&Solid"
         Checked         =   -1  'True
      End
      Begin VB.Menu OptionWireframe 
         Caption         =   "&Wireframe"
      End
      Begin VB.Menu OptionPoint 
         Caption         =   "&Point"
      End
   End
   Begin VB.Menu ShadeOptions 
      Caption         =   "&Shade Options"
      Begin VB.Menu OptionGauraud 
         Caption         =   "&Gauraud"
         Checked         =   -1  'True
      End
      Begin VB.Menu OptionFlat 
         Caption         =   "&Flat"
      End
   End
   Begin VB.Menu HelpInfo 
      Caption         =   "&Help!"
   End
End
Attribute VB_Name = "XFileLoader"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

'D3DDeviceObject
Private d3ddev As Direct3DDevice7

'D3D Matrix Info
Private matWorld1 As D3DMATRIX
Private matView1 As D3DMATRIX
Private matProj1 As D3DMATRIX

'position information
Private MustExit As Boolean
Private MouseIsDown As Boolean
Private MouseX As Long, MouseY As Long
Private OldMouseX As Long, OldMouseY As Long
Private MouseButton As Integer
Private UseRGB As Boolean

'other
Private z As Single

'Xfile loader class
Private XClass As New XFileClass



'---------------------------------------
' Form_Load Entry Point
'---------------------------------------
Private Sub Form_Load()
    
    'make sure we are not re-enterd
    Static b As Boolean
    If b Then Exit Sub
    b = True
           
    InitDX
    ResetDevice
    
    'begin screen updates
    DoLoop
    
    IM7Terminate
    
    
End Sub

'---------------------------------------
' DoLoop
'---------------------------------------
Sub DoLoop()
    
    Dim fRestore As Boolean
    ' main loop
    Do Until MustExit ' must exit can be set to true in the XFileLoader form.
        With IMCanvas1
                        
            fRestore = False
            While .DirectDraw.TestCooperativeLevel <> DD_OK
                fRestore = True
                DoEvents
            Wend
            If fRestore Then
                .DirectDraw.RestoreAllSurfaces
            End If
            'setup our matrix state
            With .Direct3DDevice
                    Call .SetTransform(D3DTRANSFORMSTATE_WORLD, matWorld1)
                    Call .SetTransform(D3DTRANSFORMSTATE_PROJECTION, matProj1)
                    Call .SetTransform(D3DTRANSFORMSTATE_VIEW, matView1)
            End With
        
            'clear the background
            .ClearBackSurface
                
            'begin scene
            .Direct3DDevice.BeginScene
        
            'render to backbuffer
            XClass.Render .Direct3DDevice
        
            'begin scene
            .Direct3DDevice.EndScene
                    
            'Display the newly rendered scene on the screen
            .Update
        
        End With
        
        ' allow for events to get processed now
        DoEvents
        
    Loop
    
    
End Sub



'---------------------------------------
' Key Events
'---------------------------------------
Private Sub IMCanvas1_KeyDown(KeyCode As Integer, Shift As Integer)
    
    Select Case KeyCode
        Case vbKeyEscape
            MustExit = True
        Case vbKeyLeft
            XClass.Rotate 0, -0.1
        Case vbKeyRight
            XClass.Rotate 0, 0.1
        Case vbKeyUp
            XClass.Rotate -0.1, 0
        Case vbKeyDown
            XClass.Rotate 0.1, 0
        Case vbKeyAdd
            z = z - 0.2
            XClass.SetPosition 0, 0, CDbl(z)
        Case vbKeySubtract
            z = z + 0.2
            XClass.SetPosition 0, 0, CDbl(z)
    End Select
    
End Sub


'---------------------------------------
' Key Events
'---------------------------------------
Private Sub Form_Resize()
    IMCanvas1.Width = Me.ScaleWidth
    IMCanvas1.Height = Me.ScaleHeight
    
    
    ' We must clean up all references to DX objects to
    ' free video memory
    CleanUp
    
    ' StartWindowed will see if we can still be hardware
    ' excellerated at new size otherwise it will fall back
    ' to software rendering
    If UseRGB Then
        IMCanvas1.InitWindowed "", "IID_IDirect3DRGBDevice"
    Else
        IMCanvas1.StartWindowed
    End If
        
    ' since we got rid of the d3d object we need to resetup our lights
    ResetDevice
    
    XClass.ReloadTextures IMCanvas1.DirectDraw, IMCanvas1.Direct3DDevice
    
End Sub



'---------------------------------------
' Mouse Events
'---------------------------------------
Private Sub IMCanvas1_MouseMove(Button As Integer, Shift As Integer, x As Single, y As Single)
    If MouseIsDown And MouseButton = 1 Then
        XClass.Rotate 0, (x - MouseX) / 128
        XClass.Rotate (y - MouseY) / 128, 0
        
        OldMouseX = MouseX
        OldMouseY = MouseY
        MouseX = x
        MouseY = y

    ElseIf MouseIsDown And MouseButton = 2 Then
        If y < MouseY - 10 Then
            z = z + 0.1
            XClass.SetPosition 0, 0, CDbl(z)
        ElseIf y > MouseY + 10 Then
            z = z - 0.1
            XClass.SetPosition 0, 0, CDbl(z)
        End If

    End If

End Sub

Private Sub IMCanvas1_MouseDown(Button As Integer, Shift As Integer, x As Single, y As Single)
    MouseIsDown = True
    MouseButton = Button
    If MouseIsDown And Button = 1 Then
        XClass.Rotate 0, (x - MouseX) / 128
        XClass.Rotate (y - MouseY) / 128, 0
    End If
    MouseX = x
    MouseY = y

End Sub

Private Sub IMCanvas1_MouseUp(Button As Integer, Shift As Integer, x As Single, y As Single)
    MouseIsDown = False
    If Button = 1 Then
        XClass.YawSpin = (x - OldMouseX) / 1280
        XClass.PitchSpin = (y - OldMouseY) / 1280
        MouseX = x
        MouseY = y
    End If
    
End Sub

'---------------------------------------
' Click Events
'---------------------------------------

Private Sub Exit_Click()
    MustExit = True
End Sub


Private Sub HelpInfo_Click()
    MsgBox "Arrows/Mouse : Spin the Object" & vbCrLf _
        & "(+/-) : Move the Object"
End Sub


'---------------------------------------
' Menu Events
'---------------------------------------
Private Sub OpenNewXFile_Click()
    With CDiag
        .Filter = ".X File|*.X"
        .FileName = "*.x"
        .ShowOpen
        If Trim$(.FileName <> vbNullString) Then
            If Trim$(.FileName) = "*.x" Then Exit Sub 'They immediately hit cancel
            XClass.Load IMCanvas1.DirectDraw, IMCanvas1.Direct3DDevice, .FileName
        Else
            Exit Sub
        End If
    End With
    
    Dim X1 As Single, Y1 As Single, z1 As Single
    Dim X2 As Single, Y2 As Single, z2 As Single
    Dim dx As Single, dy As Single, dz As Single
    Dim m As Single
    z = 2
    XClass.SetPosition 0, 0, CDbl(z)
    XClass.GetMinExtent X1, Y1, z1
    XClass.GetMaxExtent X2, Y2, z2
    
    dx = X2 - X1
    dy = Y2 - Y1
    dz = z2 - z1
    If dx > m Then m = dx
    If dy > m Then m = dy
    If dz > m Then m = dz
    If m = 0 Then Exit Sub
    m = 1.5 / m

    XClass.AdjustScale m, m, m
    
    
    
End Sub





'---------------------------------------
' InitDX
'---------------------------------------

Private Sub InitDX()
    
    Dim lProp As D3DLIGHT7
    Dim Index As Long
    Dim b As Boolean
    XFileLoader.Show
    DoEvents
        
    With IMCanvas1
        
        .EnableF5ResChange = False
        
        'Make sure we support 16bpp
        If .dx.SystemBpp <= 8 Then
            MsgBox "This sample was designed to run in High Color (16 bit) displays"
            End
        End If
        
        'Let the imcanvas pick a device
        b = .StartWindowed()
        If b = False Then End
        
        'Make sure we support texturing if we are a haldevice
        'if not fall back to RGB
        If IMCanvas1.Direct3DDevice.GetDeviceGuid() = "IID_IDirect3DHALDevice" Then
            Dim caps As D3DDEVICEDESC7
            IMCanvas1.Direct3DDevice.GetCaps caps
            If (caps.lDevCaps And D3DDEVCAPS_TEXTUREVIDEOMEMORY) = 0 Then
                b = .InitWindowed("", "IID_IDirect3DRGBDevice")
                If b = False Then End
            End If
            UseRGB = True
        End If
        
        'Setup World View and Projection Matrix
        With IMCanvas1.dx
            .IdentityMatrix matWorld1
            .IdentityMatrix matView1
            .IdentityMatrix matProj1
            .ViewMatrix matView1, RVector(0, 0, 0), RVector(0, 0, 100), RVector(0, 1, 0), 0
            .ProjectionMatrix matProj1, 0.1, 300, 1.57
        End With
        
    End With
End Sub

Private Sub ResetDevice()
        Set d3ddev = IMCanvas1.Direct3DDevice
        d3ddev.SetTransform D3DTRANSFORMSTATE_WORLD, matWorld1
        d3ddev.SetTransform D3DTRANSFORMSTATE_VIEW, matView1
        d3ddev.SetTransform D3DTRANSFORMSTATE_PROJECTION, matProj1

         Dim c As D3DCOLORVALUE
        With c
            .a = 1
            .r = 1
            .g = 1
            .b = 1
        End With
        
        Dim Material1 As D3DMATERIAL7
        Material1.diffuse = c
        Material1.power = 1
        Material1.Ambient = c
        d3ddev.SetMaterial Material1
    
    Dim m_light As D3DLIGHT7
    m_light.dltType = D3DLIGHT_POINT
    
    With c
        .a = 1
        .r = 0.5
        .g = 0.5
        .b = 0.5
    
    End With
    With m_light
        .dltType = D3DLIGHT_DIRECTIONAL
        .Ambient = c
        .diffuse = c
        .specular = c
    End With
    
    
    
    ' position light behind viewer
    m_light.position.x = 0#
    m_light.position.y = 1000#
    m_light.position.z = -100#
    m_light.direction.x = -1
    m_light.direction.y = -1
    m_light.direction.z = 1
    
    
    d3ddev.SetLight 0, m_light
    d3ddev.LightEnable 0, True
    
    IMCanvas1.BackBufferClearValue = &H4040FF
    
        
    
End Sub


'---------------------------------------
' Exiting and Cleanup
'---------------------------------------
Private Sub Form_Unload(Cancel As Integer)
    ' MustExist in a true state will cause the main loop to stop.
    MustExit = True
End Sub


Private Sub CleanUp()
    Set d3ddev = Nothing
End Sub


Sub IM7Terminate()
    On Local Error Resume Next
    With IMCanvas1.DirectDraw
        .RestoreDisplayMode
        .SetCooperativeLevel Me.hWnd, DDSCL_NORMAL
    End With
    
    CleanUp
    
    End
End Sub


Private Function TLVertex(sx As Single, sy As Single, sz As Single, w As Single, c As Long, s As Single, u As Single, v As Single) As D3DTLVERTEX
    Dim vert As D3DTLVERTEX
    vert.sx = sx
    vert.sy = sy
    vert.sz = sz
    vert.rhw = w
    vert.color = c
    vert.specular = s
    vert.tu = u
    vert.tv = v
    
    TLVertex = vert
End Function



Private Sub OptionFlat_Click()
    OptionGauraud.Checked = False
    OptionFlat.Checked = True
    XClass.ShadeMode = Flat
End Sub

Private Sub OptionGauraud_Click()
    OptionGauraud.Checked = True
    OptionFlat.Checked = False
    XClass.ShadeMode = Gouraud
End Sub

Private Sub OptionPoint_Click()
    OptionsSolid.Checked = False
    OptionWireframe.Checked = False
    OptionPoint.Checked = True
    XClass.FillMode = Points
End Sub

Private Sub OptionsSolid_Click()
    OptionsSolid.Checked = True
    OptionWireframe.Checked = False
    OptionPoint.Checked = False
    XClass.FillMode = Solid
End Sub

Private Sub OptionWireframe_Click()
    OptionsSolid.Checked = False
    OptionWireframe.Checked = True
    OptionPoint.Checked = False
    XClass.FillMode = Wireframe
End Sub
