VERSION 5.00
Begin VB.Form frmName 
   Caption         =   "Enter Your Name"
   ClientHeight    =   1740
   ClientLeft      =   60
   ClientTop       =   345
   ClientWidth     =   4680
   BeginProperty Font 
      Name            =   "Arial"
      Size            =   12
      Charset         =   0
      Weight          =   400
      Underline       =   0   'False
      Italic          =   0   'False
      Strikethrough   =   0   'False
   EndProperty
   LinkTopic       =   "Form1"
   ScaleHeight     =   1740
   ScaleWidth      =   4680
   StartUpPosition =   2  'CenterScreen
   Begin VB.CommandButton cmdOK 
      Caption         =   "&OK"
      Default         =   -1  'True
      Height          =   495
      Left            =   2633
      TabIndex        =   2
      Top             =   960
      Width           =   1215
   End
   Begin VB.CommandButton cmdQuit 
      Cancel          =   -1  'True
      Caption         =   "&Quit"
      Height          =   495
      Left            =   833
      TabIndex        =   1
      Top             =   960
      Width           =   1215
   End
   Begin VB.TextBox txtName 
      Height          =   390
      Left            =   293
      MaxLength       =   25
      TabIndex        =   0
      Top             =   240
      Width           =   4095
   End
End
Attribute VB_Name = "frmName"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Private Sub cmdOK_Click()
  If txtName.Text = "" Then
    Beep
    txtName.SetFocus
    Exit Sub
  End If
  
  PlayerName = txtName.Text
  Unload Me
  frmSession.Show
  
End Sub

Private Sub cmdQuit_Click()
  Unload Me
End Sub

Private Sub Form_Load()

  ' Get user name from system for default name
  Me.Show
  
  Dim flStrName As String * 255
  Dim vlStrName As String
  Dim result As Long
  Dim lSize As Long
  
  lSize = txtName.MaxLength
  result = GetUserName(flStrName, lSize)
  ' lSize returns the number of characters actually copied. We need to trim
  ' the string to that size, minus the terminating null.
  If result <> 0 Then
    vlStrName = Left(flStrName, lSize - 1)
    txtName.Text = vlStrName
  End If
  txtName.SelLength = lSize
  
End Sub

