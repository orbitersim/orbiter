VERSION 5.00
Begin VB.Form frmSelectDevice 
   BorderStyle     =   1  'Fixed Single
   Caption         =   "Select Device"
   ClientHeight    =   2595
   ClientLeft      =   7065
   ClientTop       =   3210
   ClientWidth     =   4125
   Icon            =   "frmSelectDevice.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   2595
   ScaleWidth      =   4125
   StartUpPosition =   2  'CenterScreen
   Begin VB.CommandButton cmdCancel 
      Caption         =   "Cancel"
      Height          =   375
      Left            =   2160
      TabIndex        =   6
      Top             =   2040
      Width           =   1575
   End
   Begin VB.CommandButton cmdOK 
      Caption         =   "OK"
      Height          =   375
      Left            =   360
      TabIndex        =   5
      Top             =   2040
      Width           =   1695
   End
   Begin VB.ComboBox cboSound 
      Height          =   315
      Left            =   120
      Style           =   2  'Dropdown List
      TabIndex        =   2
      Top             =   1560
      Width           =   3975
   End
   Begin VB.ComboBox cboCapture 
      Height          =   315
      Left            =   120
      Style           =   2  'Dropdown List
      TabIndex        =   1
      Top             =   720
      Width           =   3975
   End
   Begin VB.Label lblSoundDeivce 
      Caption         =   "DirectSound Devices:"
      Height          =   255
      Left            =   120
      TabIndex        =   4
      Top             =   1200
      Width           =   3975
   End
   Begin VB.Label lblCaptureDevices 
      Caption         =   "DirectSoundCapture Devices:"
      Height          =   255
      Left            =   120
      TabIndex        =   3
      Top             =   480
      Width           =   3855
   End
   Begin VB.Label lblSelect 
      Caption         =   "Please select your preferred devices"
      Height          =   255
      Left            =   120
      TabIndex        =   0
      Top             =   120
      Width           =   2895
   End
End
Attribute VB_Name = "frmSelectDevice"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Private DX As New DirectX7
Private dse As DirectSoundEnum
Private dsce As DirectSoundEnum


Private Sub cmdCancel_Click()
    End
End Sub

Private Sub INIT_SOUND()
    Set dse = DX.GetDSEnum
    Set dsce = DX.GetDSCaptureEnum
End Sub
    

Private Sub AddInfoToCombos()
    Dim i As Integer
    
    For i = 1 To dse.GetCount
        cboSound.AddItem dse.GetDescription(i)
    Next
    
    
    For i = 1 To dsce.GetCount
        cboCapture.AddItem dsce.GetDescription(i)
    Next
    
    cboCapture.ListIndex = 0
    cboSound.ListIndex = 0
    

End Sub

Private Sub cmdOK_Click()
    
    Full.CaptureGUID = dsce.GetGuid(cboCapture.ListIndex + 1)
    Full.SoundGUID = dse.GetGuid(cboSound.ListIndex + 1)
    Full.SoundDesc = cboSound.Text
    Full.CaptureDesc = cboCapture.Text
    
    Unload Me
    
    frmFormats.Show
    
End Sub

Private Sub form_load()
On Error GoTo err_out

    INIT_SOUND
    AddInfoToCombos

    Exit Sub

err_out:
    MsgBox "Error Starting DirectSound!", vbApplicationModal
    End
End Sub


Private Sub subCheckFullDuplex()

End Sub
    

