VERSION 5.00
Begin VB.Form frmBegin 
   BorderStyle     =   3  'Fixed Dialog
   Caption         =   "#"
   ClientHeight    =   3540
   ClientLeft      =   1740
   ClientTop       =   1410
   ClientWidth     =   7545
   BeginProperty Font 
      Name            =   "MS Sans Serif"
      Size            =   8.25
      Charset         =   0
      Weight          =   700
      Underline       =   0   'False
      Italic          =   0   'False
      Strikethrough   =   0   'False
   EndProperty
   Icon            =   "begin.frx":0000
   LinkMode        =   1  'Source
   LinkTopic       =   "Form1"
   LockControls    =   -1  'True
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   3540
   ScaleWidth      =   7545
   Begin VB.CommandButton cmdInstall 
      Default         =   -1  'True
      Height          =   1080
      Left            =   330
      MaskColor       =   &H0000FF00&
      Picture         =   "begin.frx":0442
      Style           =   1  'Graphical
      TabIndex        =   0
      Top             =   510
      UseMaskColor    =   -1  'True
      Width           =   1170
   End
   Begin VB.Frame fraDir 
      Caption         =   "#"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   660
      Left            =   135
      TabIndex        =   5
      Top             =   2010
      Width           =   7296
      Begin VB.CommandButton cmdChDir 
         Caption         =   "#"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   390
         Left            =   4890
         MaskColor       =   &H00000000&
         TabIndex        =   1
         Top             =   195
         Width           =   2310
      End
      Begin VB.Label lblDestDir 
         Caption         =   "#"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   240
         Left            =   135
         TabIndex        =   6
         Top             =   300
         Width           =   4440
      End
   End
   Begin VB.CommandButton cmdExit 
      Cancel          =   -1  'True
      Caption         =   "#"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   420
      Left            =   2610
      MaskColor       =   &H00000000&
      TabIndex        =   2
      Top             =   3030
      Width           =   2205
   End
   Begin VB.Line linTopOfExitButtonIfNoDestDir 
      Visible         =   0   'False
      X1              =   2670
      X2              =   4725
      Y1              =   2280
      Y2              =   2280
   End
   Begin VB.Label lblInstallMsg 
      AutoSize        =   -1  'True
      Caption         =   "*"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   195
      Left            =   1725
      TabIndex        =   4
      Top             =   915
      Width           =   5565
      WordWrap        =   -1  'True
   End
   Begin VB.Label lblBegin 
      AutoSize        =   -1  'True
      Caption         =   "#"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   192
      Left            =   288
      TabIndex        =   3
      Top             =   132
      Width           =   6456
      WordWrap        =   -1  'True
   End
End
Attribute VB_Name = "frmBegin"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Option Compare Text

' Constants/Types for a simulated "picture button"
Const x3DLineWidth = 1 '"Width" of highlight/shadow "lines"
Const y3DLineWidth = x3DLineWidth
Const pbXDeltaMouseDown = x3DLineWidth
Const pbYDeltaMouseDown = y3DLineWidth

Private fMouseDown As Boolean

Private Sub cmdChDir_Click()
    ShowPathDialog gstrDIR_DEST

    If gfRetVal = gintRET_CONT Then
        lblDestDir.Caption = gstrDestDir
        cmdInstall.SetFocus
    End If
End Sub

Private Sub cmdExit_Click()
    ExitSetup Me, gintRET_EXIT
End Sub


Private Sub cmdInstall_Click()
    If IsValidDestDir(gstrDestDir) = True Then
        Unload Me
        DoEvents
    End If
End Sub

Private Sub Form_Load()
    SetFormFont Me
    fraDir.Caption = ResolveResString(resFRMDIRECTORY)
    cmdChDir.Caption = ResolveResString(resBTNCHGDIR)
    cmdExit.Caption = ResolveResString(resBTNEXIT)
    lblBegin.Caption = ResolveResString(resLBLBEGIN)
    cmdInstall.ToolTipText = ResolveResString(resBTNTOOLTIPBEGIN)
    
    Caption = gstrTitle
    lblInstallMsg.Caption = ResolveResString(IIf(gfForceUseDefDest, resSPECNODEST, resSPECDEST), "|1", gstrAppName)
    lblDestDir.Caption = gstrDestDir

    If gfForceUseDefDest Then
        'We are forced to use the default destination directory, so the user
        '  will not be able to change it.
        fraDir.Visible = False
        
        'Close in the blank space on the form by moving the Exit button to where this frame
        'currently is, and adjusting the size of the form respectively
        Dim yAdjust As Integer
        yAdjust = cmdExit.Top - linTopOfExitButtonIfNoDestDir.y1
        cmdExit.Top = cmdExit.Top - yAdjust
        Height = Height - yAdjust
        
        EtchedLine Me, fraDir.Left, cmdExit.Top - cmdExit.Height \ 2, fraDir.Width
    Else
        EtchedLine Me, fraDir.Left, cmdExit.Top - cmdExit.Height \ 2, fraDir.Width
    End If
    
    CenterForm Me
End Sub

Private Sub Form_QueryUnload(Cancel As Integer, UnloadMode As Integer)
    If UnloadMode <> 1 Then
        ExitSetup Me, gintRET_EXIT
        Cancel = 1
    End If
End Sub


