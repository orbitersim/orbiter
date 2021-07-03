VERSION 5.00
Begin VB.Form frmGroup 
   BorderStyle     =   3  'Fixed Dialog
   Caption         =   "* #"
   ClientHeight    =   5250
   ClientLeft      =   1095
   ClientTop       =   1515
   ClientWidth     =   5460
   ClipControls    =   0   'False
   HasDC           =   0   'False
   Icon            =   "group.frx":0000
   LockControls    =   -1  'True
   MaxButton       =   0   'False
   MinButton       =   0   'False
   NegotiateMenus  =   0   'False
   ScaleHeight     =   350
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   364
   ShowInTaskbar   =   0   'False
   Begin VB.DirListBox dir95Groups 
      Height          =   930
      Left            =   765
      TabIndex        =   8
      Top             =   60
      Visible         =   0   'False
      Width           =   3810
   End
   Begin VB.Frame Frame1 
      Height          =   30
      Left            =   105
      TabIndex        =   7
      Top             =   4650
      Width           =   5220
   End
   Begin VB.CommandButton cmdCancel 
      Cancel          =   -1  'True
      Caption         =   "#"
      Height          =   345
      Left            =   2880
      MaskColor       =   &H00000000&
      TabIndex        =   3
      Top             =   4800
      Width           =   1140
   End
   Begin VB.CommandButton cmdContinue 
      Caption         =   "#"
      Default         =   -1  'True
      Height          =   345
      Left            =   1395
      MaskColor       =   &H00000000&
      TabIndex        =   2
      Top             =   4800
      Width           =   1140
   End
   Begin VB.ListBox lstGroups 
      Height          =   2010
      ItemData        =   "group.frx":0442
      Left            =   1080
      List            =   "group.frx":0449
      Sorted          =   -1  'True
      TabIndex        =   1
      Top             =   2220
      Width           =   3240
   End
   Begin VB.TextBox txtGroup 
      Height          =   300
      Left            =   1080
      MaxLength       =   128
      TabIndex        =   0
      Text            =   "*"
      Top             =   1410
      Width           =   3270
   End
   Begin VB.Label lblDDE 
      Height          =   225
      Left            =   225
      TabIndex        =   9
      Top             =   1350
      Visible         =   0   'False
      Width           =   705
   End
   Begin VB.Label lblGroups 
      AutoSize        =   -1  'True
      Caption         =   "#"
      Height          =   195
      Left            =   1080
      TabIndex        =   6
      Top             =   1950
      Width           =   105
   End
   Begin VB.Label lblGroup 
      AutoSize        =   -1  'True
      Caption         =   "#"
      Height          =   195
      Left            =   1080
      TabIndex        =   5
      Top             =   1170
      Width           =   105
   End
   Begin VB.Label lblMain 
      AutoSize        =   -1  'True
      Caption         =   "#"
      Height          =   195
      Left            =   180
      TabIndex        =   4
      Top             =   165
      Width           =   5100
      WordWrap        =   -1  'True
   End
End
Attribute VB_Name = "frmGroup"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Private mstrGroup As String
Private mstrDefGroup As String
Private mstrProgramsPath As String
Private mfrm As Form
Private mfPrivate As Boolean
Private mfStartMenu As Boolean

Private Sub cmdCancel_Click()
    ExitSetup frmGroup, gintRET_EXIT
End Sub

Private Sub cmdContinue_Click()
    mstrGroup = txtGroup.Text
    If Not fCreateProgGroup() Then
        '
        ' Couldn't create the group. Let
        ' the user try again.
        '
        txtGroup.SetFocus
    Else
        '
        ' The group got created ok, so unload Choose Program Group dialog
        ' and continue on with setup.
        '
        Unload Me
    End If
End Sub
Private Function fCreateProgGroup() As Boolean
'
' Create a program group.
'
    Dim strMsg As String

    If Not fValidFilename(mstrGroup) Then
        strMsg = ResolveResString(resGROUPINVALIDGROUPNAME, gstrPIPE1, CStr(gintMAX_PATH_LEN), gstrPIPE2, ResolveResString(resCOMMON_INVALIDFILECHARS))
        MsgFunc strMsg, vbOKOnly Or vbQuestion, gstrTitle
        Exit Function
    End If
    '
    'Go ahead and create the main program group
    '
    If Not fCreateShellGroup(mstrGroup, True, , mfPrivate, mfStartMenu) Then
        Exit Function
    End If
    
    fCreateProgGroup = True
End Function
Private Sub Form_Load()
    '
    ' Initialize localized control properties.
    '
    SetFormFont Me
    Caption = ResolveResString(resGROUPFRM, gstrPIPE1, gstrAppName)
    lblMain.Caption = ResolveResString(resGROUPLBLMAIN)
    lblGroup.Caption = ResolveResString(resGROUPLBLGROUP)
    lblGroups.Caption = ResolveResString(resGROUPLBLGROUPS)
    cmdContinue.Caption = ResolveResString(resGROUPBTNCONTINUE)
    cmdCancel.Caption = ResolveResString(resLOG_vbCancel)
    '
    ' Initialize the Program Group text box with the
    ' title of the application.
    '
    txtGroup.Text = gstrTitle
    '
    ' Load the ListBox with the program manager groups.
    '
    LoadW95Groups
    '
    ' Initialize the Program Group textbox with the
    ' default group selected in the list box.
    '
    lstGroups_Click
End Sub

Private Sub lstGroups_Click()
    txtGroup.Text = lstGroups.Text
End Sub

Private Sub txtGroup_Change()
    cmdContinue.Enabled = Len(Trim$(txtGroup.Text)) > 0
End Sub
Private Sub LoadW95Groups()
'
' This routine uses the system registry to
' retrieve a list of all the subfolders in the
' \windows\start menu\programs folder.
'
    Dim strFolder As String
    Dim iFolder As Integer

    mstrProgramsPath = strGetProgramsFilesPath()
    strFolder = Dir$(mstrProgramsPath, vbDirectory)   ' Retrieve the first entry.
    lstGroups.Clear
    Do While Len(strFolder) > 0
        '
        ' Ignore the current directory and the encompassing directory.
        '
        If strFolder <> "." Then
            If strFolder <> ".." Then
                '
                ' Verify that we actually got a directory and not a file.
                '
                If DirExists(mstrProgramsPath & strFolder) Then
                    '
                    ' We got a directory, add it to the list.
                    '
                    lstGroups.AddItem strFolder
                End If
            End If
        End If
        '
        ' Get the next subfolder in the Programs folder
        '
        strFolder = Dir$
    Loop
    '
    ' The lstGroups listbox now contains a listing of all the Programs
    ' subfolders (the groups).
    '
    ' Look for the default folder in the list and select it.  If it's
    ' not there, add it.
    '
    iFolder = SendMessageString(lstGroups.hWnd, LB_FINDSTRINGEXACT, -1, mstrDefGroup)
    If iFolder = LB_ERR Then
        '
        ' The group doesn't yet exist, add it to the list.
        '
        lstGroups.AddItem mstrDefGroup
        lstGroups.ListIndex = lstGroups.NewIndex
    Else
        lstGroups.ListIndex = iFolder
    End If
End Sub
Public Property Get GroupName(frm As Form, strDefGroup As String, Optional fPriv As Boolean = True, Optional ByVal fStart As Boolean = False) As String
    mstrDefGroup = strDefGroup
    Set mfrm = frm

    mfPrivate = fPriv
    mfStartMenu = fStart
    If gfNoUserInput Then
        mstrGroup = mstrDefGroup
        If Not fCreateProgGroup() Then
            ExitSetup frmSetup1, gintRET_FATAL
        End If
    Else
        Show vbModal
    End If
    GroupName = mstrGroup
End Property

Private Sub txtGroup_GotFocus()
    txtGroup.SelStart = 0
    txtGroup.SelLength = Len(txtGroup.Text)
End Sub
