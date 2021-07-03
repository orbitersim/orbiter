VERSION 5.00
Begin VB.Form frmGroup 
   BorderStyle     =   3  'Fixed Dialog
   Caption         =   "* #"
   ClientHeight    =   5250
   ClientLeft      =   1095
   ClientTop       =   1515
   ClientWidth     =   5460
   Icon            =   "group.frx":0000
   LinkTopic       =   "Form1"
   LockControls    =   -1  'True
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   350
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   364
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
      Height          =   2040
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
Dim mstrGroup As String
Dim mstrDefGroup As String
Dim mstrProgramsPath As String
Dim mfrm As Form

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
' Create a program group for either NT or Win95.
'
    Dim strMsg As String
    If TreatAsWin95() Then
        If Not fValidFilename(mstrGroup) Then
            strMsg = ResolveResString(resGROUPINVALIDGROUPNAME, "|1", CStr(gintMAX_PATH_LEN), "|2", ResolveResString(resCOMMON_INVALIDFILECHARS))
            MsgFunc strMsg, vbOKOnly Or vbQuestion, gstrTitle
            GoTo CGError
        End If
    Else
        If Not fValidNTGroupName(mstrGroup) Then
            strMsg = ResolveResString(resGROUPINVALIDGROUPNAME, "|1", CStr(gintMAX_GROUPNAME_LEN%), "|2", ResolveResString(resGROUPINVALIDCHARS))
            MsgFunc strMsg, vbOKOnly Or vbQuestion, gstrTitle
            GoTo CGError
        End If
            
    End If
    '
    'Go ahead and create the main program group
    '
    If Not fCreateOSProgramGroup(mfrm, mstrGroup, True) Then
        GoTo CGError
    End If
    
    fCreateProgGroup = True
    Exit Function
CGError:
    fCreateProgGroup = False
End Function
Private Sub Form_Load()
    '
    ' Initialize localized control properties.
    '
    SetFormFont Me
    Me.Caption = ResolveResString(resGROUPFRM, "|1", gstrAppName)
    lblMain.Caption = ResolveResString(resGROUPLBLMAIN)
    lblGroup.Caption = ResolveResString(resGROUPLBLGROUP)
    lblGroups.Caption = ResolveResString(resGROUPLBLGROUPS)
    cmdContinue.Caption = ResolveResString(resGROUPBTNCONTINUE)
    cmdCancel.Caption = ResolveResString(resLOG_IDCANCEL)
    '
    ' Initialize the Program Group text box with the
    ' title of the application.
    '
    txtGroup.Text = gstrTitle
    '
    ' Load the ListBox with the program manager groups.
    '
    If TreatAsWin95() Then
        LoadW95Groups
    Else
        LoadProgManGroups
    End If
    '
    ' Initialize the Program Group textbox with the
    ' default group selected in the list box.
    '
    txtGroup.Text = lstGroups.List(lstGroups.ListIndex)
End Sub

Private Sub lstGroups_Click()
    txtGroup.Text = lstGroups.List(lstGroups.ListIndex)
End Sub

Private Sub txtGroup_Change()
    cmdContinue.Enabled = Len(Trim(txtGroup.Text)) > 0
End Sub
Sub LoadProgManGroups()
'
' This routine uses DDE to talk to Program Manager
' to retrieve a list of all the groups it manages.
' It should only be called if the shell is NT 3.51.
' If it is Win95 or NT4, call LoadW95Groups()
' instead.
'
' Special strings used in this routine.  Do not
' localize these strings.
'
    Const strPROGMANLINKTOPIC = "ProgMan|Progman"
    Const strPROGMANLINKITEM = "Progman"
    Const strNDWGROUP = "Quick Access"

    Dim strGroups As String
    Dim strGroup As String
    Dim intOffset As Integer
    Dim intAnchor As Integer
    Dim iGroup As Long
    
    lblDDE.LinkTopic = strPROGMANLINKTOPIC
    lblDDE.LinkItem = strPROGMANLINKITEM
    lblDDE.LinkMode = 2
    lblDDE.LinkRequest
    On Error Resume Next
    lblDDE.LinkMode = 0
    '
    ' The DDE call just made put the names of all the groups
    ' into the caption property of the lblDDE control.
    ' We want to transfer them to the list box.  They are
    ' separated by CRLF's.
    '
    strGroups = lblDDE.Caption
    intAnchor = 1
    intOffset = InStr(intAnchor, strGroups, CRLF)
    lstGroups.Clear
    Do While intOffset > 0
        strGroup = Mid(strGroups, intAnchor, intOffset - intAnchor)
        '
        ' Norton Desktop for Windows uses the "Quick Access" group
        ' to replace program manager.  Trying to add icons to this
        ' group will fail later when we perform our DDE.linkrequest.
        ' Therefore, skip this group.
        '
        If strGroup <> strNDWGROUP Then
            lstGroups.AddItem strGroup
        End If
        
        intAnchor = intOffset + 2
        intOffset = InStr(intAnchor, strGroups, CRLF)
    Loop
    '
    ' The lstGroups listbox now contains a listing of all the program
    ' manager groups.
    '
    ' Look for the default group in the list and select it.  If it's
    ' not there, add it.
    '
    iGroup = SendMessageString(lstGroups.hwnd, LB_FINDSTRINGEXACT, -1, mstrDefGroup)
    If iGroup = LB_ERR Then
        '
        ' The group doesn't yet exist, add it to the list.
        '
        lstGroups.AddItem mstrDefGroup
        lstGroups.ListIndex = lstGroups.NewIndex
    Else
        lstGroups.ListIndex = iGroup
    End If
End Sub
Sub LoadW95Groups()
'
' This routine uses the system registry to
' retrieve a list of all the subfolders in the
' \windows\start menu\programs folder.
' It should only be called if the shell is Win95
' NT4.  If it is NT 3.51, call LoadProgManGroups()
' instead.
'
    Dim strFolder As String
    Dim iFolder As Integer
    
    mstrProgramsPath = strGetProgramsFilesPath()
    strFolder = Dir(mstrProgramsPath, vbDirectory)   ' Retrieve the first entry.
    lstGroups.Clear
    Do While strFolder <> ""
        '
        ' Ignore the current directory and the encompassing directory.
        '
        If strFolder <> "." And strFolder <> ".." Then
            '
            ' Verify that we actually got a directory and not a file.
            '
            If (GetAttr(mstrProgramsPath & strFolder) And vbDirectory) = vbDirectory Then
                '
                ' We got a directory, add it to the list.
                '
                lstGroups.AddItem strFolder
            End If
        End If
        '
        ' Get the next subfolder in the Programs folder
        '
        strFolder = Dir
    Loop
    '
    ' The lstGroups listbox now contains a listing of all the Programs
    ' subfolders (the groups).
    '
    ' Look for the default folder in the list and select it.  If it's
    ' not there, add it.
    '
    iFolder = SendMessageString(lstGroups.hwnd, LB_FINDSTRINGEXACT, -1, mstrDefGroup)
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
Public Property Get GroupName(frm As Form, strDefGroup As String) As String
    mstrDefGroup = strDefGroup
    Set mfrm = frm
    
    If gfNoUserInput = True Then
        mstrGroup = mstrDefGroup
        If Not fCreateProgGroup() Then
            ExitSetup frmSetup1, gintRET_FATAL
        End If
    Else
        Me.Show vbModal
    End If
    GroupName = mstrGroup
End Property

Private Sub txtGroup_GotFocus()
    txtGroup.SelStart = 0
    txtGroup.SelLength = 32767
End Sub
