VERSION 5.00
Begin VB.Form frmMultiplayer 
   BorderStyle     =   1  'Fixed Single
   Caption         =   "Multiple Player"
   ClientHeight    =   4620
   ClientLeft      =   45
   ClientTop       =   330
   ClientWidth     =   8430
   BeginProperty Font 
      Name            =   "Verdana"
      Size            =   9.75
      Charset         =   0
      Weight          =   700
      Underline       =   0   'False
      Italic          =   0   'False
      Strikethrough   =   0   'False
   EndProperty
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   4620
   ScaleWidth      =   8430
   StartUpPosition =   2  'CenterScreen
   Visible         =   0   'False
   Begin VB.CommandButton cmdCancel 
      Cancel          =   -1  'True
      Caption         =   "Cancel"
      Height          =   735
      Left            =   6840
      TabIndex        =   4
      Top             =   2400
      Width           =   1455
   End
   Begin VB.CommandButton cmdOK 
      Caption         =   "&OK"
      Default         =   -1  'True
      Height          =   735
      Left            =   6840
      TabIndex        =   5
      Top             =   3480
      Width           =   1455
   End
   Begin VB.TextBox txtYourName 
      Height          =   495
      Left            =   360
      MaxLength       =   25
      TabIndex        =   3
      Text            =   "Player"
      Top             =   3600
      Width           =   5055
   End
   Begin VB.ListBox lstConnections 
      BeginProperty Font 
         Name            =   "Verdana"
         Size            =   9.75
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   1980
      ItemData        =   "MultiForm.frx":0000
      Left            =   240
      List            =   "MultiForm.frx":0002
      TabIndex        =   1
      Top             =   600
      Width           =   6375
   End
   Begin VB.Label Label2 
      Caption         =   "&Your Name:"
      Height          =   375
      Left            =   360
      TabIndex        =   2
      Top             =   3120
      Width           =   2295
   End
   Begin VB.Label Label1 
      Caption         =   "&Choose Connection Type:"
      Height          =   375
      Left            =   360
      TabIndex        =   0
      Top             =   120
      Width           =   3615
   End
End
Attribute VB_Name = "frmMultiplayer"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Private Sub cmdCancel_Click()
  ' Force initialization of DirectPlay if user comes back in
  Unload Me
  frmMainMenu.Show
End Sub

Private Sub cmdOK_Click()
  Dim CIndex As Long
  Dim ConnectionMade As Boolean
  Dim objDPAddress As DirectPlayAddress
  ' Initialize the connection. Any service provider dialogs are not called till the
  ' connection is used, e.g. to enumerate sessions.
  
  CIndex = lstConnections.ListIndex + 1
  On Error GoTo INITIALIZEFAILED
  Set objDPAddress = gObjEnumConnections.GetAddress(CIndex)
  Call gObjDPlay.InitializeConnection(objDPAddress)
  On Error GoTo 0
  
  ' Enumerate the sessions to be shown in the SessionForm listbox. If this fails with
  ' DPERR_USERCANCEL, the user cancelled out of a service provider dialog. This is
  ' not a fatal error, because for the modem connection it simply indicates the
  ' player wishes to host a session and not to make a dial-up connection. The "answer"
  ' dialog will come up when the user attempts to create a session.
  
  ConnectionMade = frmSessions.UpdateSessionList
  If ConnectionMade Then
    Hide
    frmSessions.Show
  Else
    InitDPlay
  End If
  Exit Sub
  ' Error handlers
INITIALIZEFAILED:
  If Err.Number <> DPERR_ALREADYINITIALIZED Then
    MsgBox ("Failed to initialize connection.")
    Exit Sub
  End If
  
End Sub

Private Sub Form_Load()
' Get user name and initialize text box. To use the Win32 function we need to
' supply a fixed-length string. (We could also use a string initialized with nulls.)

  Const UserNameLen = 99
  Dim flStrName As String * UserNameLen
  Dim vlStrName As String
  Dim result As Long
  Dim lSize As Long
  
  lSize = UserNameLen
  result = GetUserName(flStrName, lSize)
  ' lSize returns the number of characters actually copied. We need to trim
  ' the string to that size, minus the terminating null.
  If result <> 0 Then
    vlStrName = Left(flStrName, lSize - 1)
    txtYourName.Text = vlStrName
  End If
  
  ' Enumerate connections
  If Not InitConnectionList Then
    Call MsgBox("Failed to Enumerate Connections.")
    CloseDownDPlay
    End
  End If
  
End Sub

Private Sub Form_Unload(Cancel As Integer)
  frmMainMenu.Show
End Sub

Private Sub lstConnections_DblClick()
    cmdOK_Click
End Sub

' Highlight player name when selected

Private Sub txtYourName_GotFocus()
  txtYourName.SelStart = 0
  txtYourName.SelLength = txtYourName.MaxLength
End Sub

' Enumerate connections

Public Function InitConnectionList() As Boolean

  Dim NumConnections As Long
  Dim strName As String
  Dim X As Long
  
  Call InitDPlay     ' Program aborts on failure
 
  lstConnections.Clear
  
  On Error GoTo FAILED
  NumConnections = gObjEnumConnections.GetCount
  For X = 1 To NumConnections
    strName = gObjEnumConnections.GetName(X)
    Call lstConnections.AddItem(strName)
  Next X
  
  ' Initialize selection
  lstConnections.ListIndex = 0
  InitConnectionList = True
  Exit Function
  
  ' Error handlers
FAILED:
  InitConnectionList = False
  Exit Function
  
End Function
