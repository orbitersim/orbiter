VERSION 5.00
Begin VB.Form MainForm 
   Appearance      =   0  'Flat
   BackColor       =   &H80000005&
   BorderStyle     =   0  'None
   Caption         =   "DDBlend Sample"
   ClientHeight    =   3195
   ClientLeft      =   0
   ClientTop       =   0
   ClientWidth     =   4680
   BeginProperty Font 
      Name            =   "Times New Roman"
      Size            =   15.75
      Charset         =   0
      Weight          =   400
      Underline       =   0   'False
      Italic          =   0   'False
      Strikethrough   =   0   'False
   EndProperty
   Icon            =   "satMainForm.frx":0000
   LinkTopic       =   "Form1"
   ScaleHeight     =   213
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   312
   StartUpPosition =   3  'Windows Default
End
Attribute VB_Name = "MainForm"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Dim BlitRect As RECT

Private Sub Form_KeyDown(KeyCode As Integer, Shift As Integer)
    Select Case KeyCode
        Case vbKeyEscape
            ExitLoop = True
        Case vbKeyPageUp
            BlurFactor = BlurFactor + 1
        Case vbKeyPageDown
            BlurFactor = BlurFactor - 1
            If BlurFactor < 1 Then BlurFactor = 1
        Case vbKeyHome
            ParticleCount = ParticleCount + 1000
            If ParticleCount > MaxParticles Then ParticleCount = MaxParticles
        Case vbKeyEnd
            ParticleCount = ParticleCount - 1000
            If ParticleCount < 0 Then ParticleCount = 0
        Case vbKeySpace
            If PaletteNum = 0 Then PaletteNum = 1 Else PaletteNum = 0
        Case vbKeyInsert
            CurrentDisplayMode = CurrentDisplayMode + 1
            If CurrentDisplayMode > UBound(DisplayMode) Then
                CurrentDisplayMode = 0
            End If
            TerminateDX
            InitializeDX
        Case vbKeyDelete
            CurrentDisplayMode = CurrentDisplayMode - 1
            If CurrentDisplayMode < 0 Then
                CurrentDisplayMode = UBound(DisplayMode)
            End If
            TerminateDX
            InitializeDX
    End Select
End Sub

Public Sub Form_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
    MX = X
    MY = Y
    If MX < 1 Then MX = 1 Else If _
        MX > ScreenWidth - 2 Then MX = ScreenWidth - 2
    If MY < 1 Then MY = 1 Else If _
        MY > ScreenHeight - 2 Then MY = ScreenHeight - 2
End Sub

Public Sub Form_Paint()
    On Error Resume Next
    BlitRect.Right = DDSBackDesc.lWidth
    BlitRect.Bottom = DDSBackDesc.lHeight
    DDSFront.Blt BlitRect, DDSBack, BlitRect, DDBLT_WAIT
    DoEvents
End Sub
