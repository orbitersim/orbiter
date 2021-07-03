VERSION 5.00
Object = "{D0B8DDCE-E796-11D2-A21E-00C04F68AD33}#1.1#0"; "IMCONTROL.OCX"
Begin VB.Form MipMapForm 
   Caption         =   "Mipmap Sample"
   ClientHeight    =   3345
   ClientLeft      =   60
   ClientTop       =   345
   ClientWidth     =   4785
   Icon            =   "MipMapForm.frx":0000
   LinkTopic       =   "Form1"
   ScaleHeight     =   223
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   319
   StartUpPosition =   3  'Windows Default
   Begin DirectXIMControl.IMCanvas IMCanvas1 
      Height          =   3255
      Left            =   0
      TabIndex        =   0
      Top             =   0
      Width           =   4695
      _ExtentX        =   8281
      _ExtentY        =   5741
   End
End
Attribute VB_Name = "MipMapForm"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Private Sub Form_Resize()
    
    IMCanvas1.Width = MipMapForm.ScaleWidth
    IMCanvas1.Height = MipMapForm.ScaleHeight
    If binit = False Then Exit Sub
    CleanUp
    If UseRGB Then
        IMCanvas1.InitWindowed "", "IID_IDirect3DRGBDevice"
    Else
        IMCanvas1.StartWindowed
    End If
    InitDeviceObjects
    InitMipMap

End Sub

Private Sub Form_Unload(Cancel As Integer)
    ' MustExist in a true state will cause the main loop to stop.
    MustExit = True
End Sub

Private Sub IMCanvas1_NewDDraw()
        InitDeviceObjects
        InitMipMap
End Sub
