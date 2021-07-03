VERSION 5.00
Begin VB.Form frmWelcome 
   AutoRedraw      =   -1  'True
   BorderStyle     =   3  'Fixed Dialog
   Caption         =   "#"
   ClientHeight    =   3255
   ClientLeft      =   540
   ClientTop       =   6000
   ClientWidth     =   6435
   ClipControls    =   0   'False
   BeginProperty Font 
      Name            =   "MS Sans Serif"
      Size            =   8.25
      Charset         =   0
      Weight          =   700
      Underline       =   0   'False
      Italic          =   0   'False
      Strikethrough   =   0   'False
   EndProperty
   Icon            =   "welcome.frx":0000
   LinkTopic       =   "Form1"
   LockControls    =   -1  'True
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   3255
   ScaleWidth      =   6435
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
      Left            =   3705
      MaskColor       =   &H00000000&
      TabIndex        =   1
      Top             =   2655
      Width           =   1440
   End
   Begin VB.CommandButton cmdOK 
      Caption         =   "#"
      Default         =   -1  'True
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
      Left            =   1470
      MaskColor       =   &H00000000&
      TabIndex        =   0
      Top             =   2655
      Width           =   1440
   End
   Begin VB.Image imgWelcome 
      Height          =   480
      Left            =   630
      Picture         =   "welcome.frx":0442
      Top             =   330
      Width           =   480
   End
   Begin VB.Label lblWelcome 
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
      Left            =   1305
      TabIndex        =   2
      Top             =   330
      Width           =   4800
      WordWrap        =   -1  'True
   End
   Begin VB.Label lblRunning 
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
      Height          =   195
      Left            =   435
      TabIndex        =   3
      Top             =   915
      Width           =   5535
      WordWrap        =   -1  'True
   End
   Begin VB.Shape shpWelcome 
      BorderColor     =   &H00000000&
      BorderWidth     =   2
      Height          =   2250
      Left            =   210
      Top             =   135
      Width           =   6015
   End
End
Attribute VB_Name = "frmWelcome"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_TemplateDerived = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Option Compare Text
'frmWelcome
Private Sub cmdExit_Click()
    ExitSetup Me, gintRET_EXIT
End Sub

Private Sub cmdOK_Click()
    Unload Me
End Sub

Private Sub Form_Load()
    Dim intWidth As Integer

    SetFormFont Me
    cmdExit.Caption = ResolveResString(resBTNEXIT)
    cmdOK.Caption = ResolveResString(resBTNOK)
    lblRunning.Caption = ResolveResString(resLBLRUNNING)
    
    Caption = gstrTitle
    intWidth = TextWidth(Caption) + cmdOK.Width * 2
    If intWidth > Width Then
        Width = intWidth
    End If

    lblWelcome.Caption = ResolveResString(resWELCOME, "|1", gstrAppName)

    shpWelcome.Move (ScaleWidth - shpWelcome.Width) \ 2
    cmdOK.Left = (ScaleWidth - cmdOK.Width * 1.5 - cmdExit.Width) \ 2
    cmdExit.Left = cmdOK.Left + cmdOK.Width * 1.5

    EtchedLine Me, shpWelcome.Left - 50, cmdOK.Top - cmdOK.Height \ 2, shpWelcome.Width + 100

    CenterForm Me
End Sub

Private Sub Form_QueryUnload(Cancel As Integer, UnloadMode As Integer)
    If UnloadMode <> 1 Then
        ExitSetup Me, gintRET_EXIT
        Cancel = 1
    End If
End Sub

