VERSION 5.00
Begin VB.Form frmMain 
   BorderStyle     =   1  'Fixed Single
   Caption         =   "Full Duplex"
   ClientHeight    =   3450
   ClientLeft      =   150
   ClientTop       =   720
   ClientWidth     =   4680
   Icon            =   "frmMain.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   3450
   ScaleWidth      =   4680
   StartUpPosition =   2  'CenterScreen
   Begin VB.CommandButton cmdStop 
      Caption         =   "Stop"
      Height          =   375
      Left            =   3360
      TabIndex        =   12
      Top             =   2760
      Width           =   1095
   End
   Begin VB.CommandButton cmdEnabled 
      Cancel          =   -1  'True
      Caption         =   "Run"
      Height          =   375
      Left            =   3360
      TabIndex        =   11
      Top             =   2280
      Width           =   1095
   End
   Begin VB.ComboBox cboEffect 
      Height          =   315
      Left            =   120
      TabIndex        =   10
      Text            =   "Combo1"
      Top             =   2280
      Width           =   3135
   End
   Begin VB.Frame fraOutput 
      Caption         =   "Output"
      Height          =   975
      Left            =   120
      TabIndex        =   1
      Top             =   1200
      Width           =   4335
      Begin VB.Label lblOutputFormatDesc 
         Caption         =   "Label1"
         Height          =   255
         Left            =   840
         TabIndex        =   9
         Top             =   600
         Width           =   3375
      End
      Begin VB.Label lblOutputDeviceDesc 
         Caption         =   "Label1"
         Height          =   255
         Left            =   840
         TabIndex        =   8
         Top             =   240
         Width           =   3375
      End
      Begin VB.Label lblOutputFormat 
         AutoSize        =   -1  'True
         Caption         =   "Format:"
         Height          =   195
         Left            =   120
         TabIndex        =   7
         Top             =   600
         Width           =   525
      End
      Begin VB.Label lblOutputDevice 
         AutoSize        =   -1  'True
         Caption         =   "Device:"
         Height          =   195
         Left            =   120
         TabIndex        =   6
         Top             =   240
         Width           =   555
      End
   End
   Begin VB.Frame fraInput 
      Caption         =   "Input"
      Height          =   975
      Left            =   120
      TabIndex        =   0
      Top             =   120
      Width           =   4335
      Begin VB.Label lblInputFormatDesc 
         Caption         =   "Label1"
         Height          =   255
         Left            =   840
         TabIndex        =   5
         Top             =   600
         Width           =   3255
      End
      Begin VB.Label lblInputDeviceDesc 
         Caption         =   "Label1"
         Height          =   255
         Left            =   840
         TabIndex        =   4
         Top             =   240
         Width           =   3135
      End
      Begin VB.Label lblInputFormat 
         AutoSize        =   -1  'True
         Caption         =   "Format:"
         Height          =   195
         Left            =   120
         TabIndex        =   3
         Top             =   600
         Width           =   525
      End
      Begin VB.Label lblInputDevice 
         AutoSize        =   -1  'True
         Caption         =   "Device:"
         Height          =   195
         Left            =   120
         TabIndex        =   2
         Top             =   240
         Width           =   555
      End
   End
   Begin VB.Menu mnuFile 
      Caption         =   "File"
      Begin VB.Menu mnuExit 
         Caption         =   "E&xit"
      End
   End
   Begin VB.Menu mnuSettings 
      Caption         =   "Settings"
      Begin VB.Menu mnuFormats 
         Caption         =   "Formats"
      End
   End
   Begin VB.Menu mnuHelp 
      Caption         =   "Help"
      Begin VB.Menu mnuAbout 
         Caption         =   "About"
      End
   End
End
Attribute VB_Name = "frmMain"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False

Private Sub FillLabels()
    lblInputDeviceDesc.Caption = Full.CaptureDesc
    lblInputFormatDesc.Caption = Full.CaptureFormatDesc
    lblOutputDeviceDesc.Caption = Full.SoundDesc
    lblOutputFormatDesc.Caption = Full.SoundFormatDesc
    
    cboEffect.AddItem "[Pass Through]"
    
    ''''''''''''''''''''''''''''''''
    'No effect yet
    ''''''''''''''''''''''''''''''''
    'cboEffect.AddItem "Gargle"
    
    cboEffect.ListIndex = 0

End Sub


Private Sub RunIt()
    
        If gDSB Is Nothing And gDSCB Is Nothing Then Call CreateDevices
        Call record(gDSCB)
        Call PlayBack(gDSB)
        
        gRUN = True
        
        Do While gRUN = True
            Call CopyBuffers(gDSB, gDSCB, cboEffect.ListIndex)
            DoEvents
        Loop

End Sub



Private Sub cboEffect_click()
    gRUN = False
    StopAll gDSB, gDSCB
    cmdEnabled.Enabled = True
End Sub

Private Sub cmdEnabled_Click()
    cmdEnabled.Enabled = False
    Call RunIt
End Sub

Private Sub cmdStop_Click()
    cmdEnabled.Enabled = True
    Call StopAll(gDSB, gDSCB)
End Sub

Private Sub form_load()
    FillLabels
End Sub

Private Sub Form_Unload(Cancel As Integer)
    gRUN = False
    Set gDS = Nothing
    Set gDSC = Nothing
    Set gDSB = Nothing
    Set gDSCB = Nothing
    Set gDX = Nothing
End Sub

Private Sub mnuAbout_Click()
    frmAbout.Show 1
End Sub

Private Sub mnuExit_Click()
    End
End Sub


Private Sub CreateDevices()
    on local error goto errOut:
	        
    '''''''''''''''''''''''''''''
    'Init the devices
    '''''''''''''''''''''''''''''
    basUtils.InitDirectSound hWnd, Full.SoundGUID
    basUtils.InitDirectSoundCapture
    Exit sub

errOut:
    MsgBox "Unable to create DirectSound or DirectSoundCapture Device"
    end
End Sub

Private Sub mnuFormats_Click()
    Unload Me
    frmSelectDevice.Show
End Sub
