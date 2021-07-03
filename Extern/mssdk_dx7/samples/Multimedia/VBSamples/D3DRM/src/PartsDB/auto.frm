VERSION 5.00
Object = "{831FDD16-0C5C-11D2-A9FC-0000F8754DA1}#2.0#0"; "MSCOMCTL.OCX"
Begin VB.Form Form1 
   BorderStyle     =   3  'Fixed Dialog
   Caption         =   "Auto Collision Parts Database"
   ClientHeight    =   8520
   ClientLeft      =   480
   ClientTop       =   615
   ClientWidth     =   10875
   Icon            =   "auto.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   568
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   725
   ShowInTaskbar   =   1  'True
   Begin MSComctlLib.TreeView TreeView1 
      Height          =   3495
      Left            =   120
      TabIndex        =   27
      Top             =   480
      Width           =   3975
      _ExtentX        =   7011
      _ExtentY        =   6165
      _Version        =   393217
      HideSelection   =   0   'False
      Style           =   7
      Appearance      =   1
   End
   Begin VB.TextBox Text8 
      Enabled         =   0   'False
      Height          =   375
      Left            =   9000
      TabIndex        =   25
      Top             =   7380
      Width           =   1695
   End
   Begin VB.PictureBox Picture2 
      Height          =   735
      Left            =   120
      ScaleHeight     =   675
      ScaleWidth      =   10515
      TabIndex        =   19
      Top             =   4080
      Width           =   10575
      Begin VB.Label Label9 
         Caption         =   $"auto.frx":0442
         Height          =   495
         Left            =   120
         TabIndex        =   20
         Top             =   120
         Width           =   9495
      End
   End
   Begin VB.CommandButton Command1 
      Caption         =   "Add To Invoice"
      Height          =   495
      Left            =   120
      TabIndex        =   16
      Top             =   7920
      Width           =   3975
   End
   Begin MSComctlLib.ListView ListView1 
      Height          =   2355
      Left            =   4320
      TabIndex        =   15
      Top             =   4920
      Width           =   6375
      _ExtentX        =   11245
      _ExtentY        =   4154
      View            =   3
      LabelWrap       =   -1  'True
      HideSelection   =   0   'False
      _Version        =   393217
      ForeColor       =   -2147483640
      BackColor       =   -2147483643
      BorderStyle     =   1
      Appearance      =   1
      NumItems        =   5
      BeginProperty ColumnHeader(1) {BDD1F052-858B-11D1-B16A-00C0F0283628} 
         Key             =   "price"
         Text            =   "DESCRIPTION"
         Object.Width           =   5821
      EndProperty
      BeginProperty ColumnHeader(2) {BDD1F052-858B-11D1-B16A-00C0F0283628} 
         SubItemIndex    =   1
         Key             =   "part"
         Text            =   "PRICE"
         Object.Width           =   2117
      EndProperty
      BeginProperty ColumnHeader(3) {BDD1F052-858B-11D1-B16A-00C0F0283628} 
         SubItemIndex    =   2
         Key             =   "id"
         Text            =   "ID"
         Object.Width           =   2117
      EndProperty
      BeginProperty ColumnHeader(4) {BDD1F052-858B-11D1-B16A-00C0F0283628} 
         SubItemIndex    =   3
         Key             =   "modid"
         Text            =   "MODID"
         Object.Width           =   0
      EndProperty
      BeginProperty ColumnHeader(5) {BDD1F052-858B-11D1-B16A-00C0F0283628} 
         SubItemIndex    =   4
         Object.Width           =   38100
      EndProperty
   End
   Begin VB.CommandButton Command3 
      Caption         =   "Process Order"
      Height          =   495
      Left            =   7680
      TabIndex        =   3
      Top             =   7920
      Width           =   3015
   End
   Begin VB.CommandButton Command2 
      Caption         =   "Remove From Invoice"
      Height          =   495
      Left            =   4320
      TabIndex        =   2
      Top             =   7920
      Width           =   3135
   End
   Begin VB.PictureBox largepict 
      Height          =   3495
      Left            =   4320
      ScaleHeight     =   229
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   421
      TabIndex        =   1
      Top             =   480
      Width           =   6375
   End
   Begin VB.PictureBox Picture1 
      Height          =   2835
      Left            =   120
      ScaleHeight     =   2775
      ScaleWidth      =   3915
      TabIndex        =   0
      Top             =   4920
      Width           =   3975
      Begin VB.TextBox Text7 
         Enabled         =   0   'False
         Height          =   285
         Left            =   1440
         TabIndex        =   26
         Top             =   2400
         Width           =   1695
      End
      Begin VB.TextBox Text6 
         Enabled         =   0   'False
         Height          =   285
         Left            =   1440
         TabIndex        =   24
         Top             =   2040
         Width           =   1695
      End
      Begin VB.TextBox Text5 
         Enabled         =   0   'False
         Height          =   285
         Left            =   1440
         TabIndex        =   13
         Top             =   1680
         Width           =   1695
      End
      Begin VB.TextBox Text4 
         Enabled         =   0   'False
         Height          =   285
         Left            =   1440
         TabIndex        =   11
         Top             =   1320
         Width           =   1695
      End
      Begin VB.TextBox Text3 
         Enabled         =   0   'False
         Height          =   285
         Left            =   1440
         TabIndex        =   9
         Top             =   960
         Width           =   1695
      End
      Begin VB.TextBox Text2 
         Enabled         =   0   'False
         Height          =   285
         Left            =   1440
         TabIndex        =   7
         Top             =   600
         Width           =   1695
      End
      Begin VB.TextBox Text1 
         Enabled         =   0   'False
         Height          =   285
         Left            =   1440
         TabIndex        =   4
         Top             =   240
         Width           =   1695
      End
      Begin VB.Label Label12 
         Caption         =   "MAKE"
         Height          =   255
         Left            =   120
         TabIndex        =   23
         Top             =   2400
         Width           =   735
      End
      Begin VB.Label Label11 
         Caption         =   "Label11"
         Height          =   15
         Left            =   240
         TabIndex        =   22
         Top             =   2640
         Width           =   735
      End
      Begin VB.Label Label10 
         Caption         =   "STOCK"
         Height          =   255
         Left            =   120
         TabIndex        =   21
         Top             =   2040
         Width           =   1095
      End
      Begin VB.Label Label5 
         Caption         =   "ASSEMBLY"
         Height          =   375
         Left            =   120
         TabIndex        =   12
         Top             =   1680
         Width           =   1335
      End
      Begin VB.Label Label4 
         Caption         =   "COMPAT PARTS"
         Height          =   255
         Left            =   120
         TabIndex        =   10
         Top             =   1320
         Width           =   1335
      End
      Begin VB.Label Label3 
         Caption         =   "PRICE"
         Height          =   255
         Left            =   120
         TabIndex        =   8
         Top             =   960
         Width           =   1095
      End
      Begin VB.Label Label2 
         Caption         =   "DESCRIPTION"
         Height          =   375
         Left            =   120
         TabIndex        =   6
         Top             =   600
         Width           =   1215
      End
      Begin VB.Label Label1 
         Caption         =   "PARTID"
         Height          =   255
         Left            =   120
         TabIndex        =   5
         Top             =   240
         Width           =   735
      End
   End
   Begin VB.Label Label8 
      Caption         =   "Select Assembly"
      Height          =   255
      Left            =   120
      TabIndex        =   18
      Top             =   120
      Width           =   2895
   End
   Begin VB.Label Label7 
      Caption         =   "Click On a Part From Assembly - Use the mouse to Rotate the Assembly"
      Height          =   255
      Left            =   4320
      TabIndex        =   17
      Top             =   120
      Width           =   5895
   End
   Begin VB.Label Label6 
      Caption         =   "TOTAL"
      Height          =   255
      Left            =   4440
      TabIndex        =   14
      Top             =   7380
      Width           =   1455
   End
   Begin VB.Menu MENU_FILE 
      Caption         =   "&File"
      Begin VB.Menu MENU_EXIT 
         Caption         =   "E&xit"
      End
   End
   Begin VB.Menu MENU_HELP 
      Caption         =   "&Help"
      Begin VB.Menu MENU_ABOUT 
         Caption         =   "&About..."
      End
   End
End
Attribute VB_Name = "Form1"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

'The model used by this sample, engine1.x, is provided courtesy of Viewpoint
'Digital, Inc. (www.viewpoint.com).  It is provided for use with this sample
'only and cannot be distributed with any application without prior written
'consent.  V6 Engine Model copyright 1999 Viewpoint Digital, Inc..

Dim m_assemblies(100) As Direct3DRMFrame3
Dim m_assemblyName(100) As String
Dim m_nAssembly As Long

Dim m_rm As Direct3DRM3
Dim m_scene As Direct3DRMFrame3
Dim m_root As Direct3DRMFrame3
Dim m_bMouseDown As Boolean
Dim m_lastX As Integer
Dim m_lastY As Integer

Dim m_largewindow As RMWindow
Dim m_dd As DirectDraw7
Dim m_dx As New DirectX7
Dim m_bInLoad As Boolean
Dim m_binit As Boolean
Dim m_data As New Data
Dim fLoading As Boolean


Private Sub Command1_Click()
    

    Dim itm As ListItem
    If Text1.Text = "" Then Exit Sub
    
    Set itm = ListView1.ListItems.Add(, , Text2.Text)
    itm.SubItems(1) = Text3.Text
    itm.SubItems(2) = Text1.Text
    Set ListView1.SelectedItem = itm
    itm.EnsureVisible
    Text8.Text = Format(Val(Text8.Text) + Val(Text3.Text), "#0.00")
    
End Sub


Private Sub Command2_Click()
    If ListView1.SelectedItem Is Nothing Then Exit Sub
    
    Text8 = Format(Val(Text8.Text) - Val(ListView1.SelectedItem.SubItems(1)), "#0.00")
    ListView1.ListItems.Remove ListView1.SelectedItem.index
End Sub

Private Sub Form_Load()
    Me.Show
    
    FindMediaDir "partstable.txt"
    
    m_data.InitData "partstable.txt"
    

    DoEvents
    InitializeDXObjects
  
    FillTreeViewControl
    
End Sub

'- InitializeDXObjects
Sub InitializeDXObjects()
            
    'Create Direct Draw and RM objects
    Set m_dd = m_dx.DirectDrawCreate("")
    Set m_rm = m_dx.Direct3DRMCreate()
    
    'Create RMWindow object and initialize
    'with our rm and ddraw objects
    'give it a picture control to draw to
    'and indicate it should find the best
    '3d rederer it can find
    Set m_largewindow = New RMWindow
    m_largewindow.init m_dx, m_dd, m_rm, largepict, ""
    
    
    'Save off the scene which all object hang from
    'Set its background color
    'Render the emty scene
    Set m_scene = m_largewindow.m_rootFrame
    m_scene.SetSceneBackground &HFF0010FF
    m_largewindow.Render
    
    
    'Setup a Light pointed in from the left
    Dim light1 As Direct3DRMLight
    Dim lightFrame1 As Direct3DRMFrame3
    Set light1 = m_rm.CreateLightRGB(D3DRMLIGHT_DIRECTIONAL, 0.5, 0.5, 0.5)
    Set lightFrame1 = m_rm.CreateFrame(m_scene)
    lightFrame1.AddLight light1
    lightFrame1.SetPosition Nothing, -10, 4, -5
    lightFrame1.LookAt m_largewindow.m_rootFrame, Nothing, 0
    
    
        
End Sub


'- SelectPart
'  fill in the text boxes given a certain identifier
'  from a model. We query the database for the identifier
'  and from there we get the rest of the info
Sub SelectPart(strName As String)
   
      
  
   
   If m_data.MoveToModelPartRecord(strName) = False Then Exit Sub
   
   Text1.Text = m_data.PartID
   Text2.Text = m_data.Description
   Text3.Text = Format$(m_data.Price, "#0.00")
   Text4.Text = m_data.CompatibleParts
   Text5.Text = "Engine"
   Text6.Text = m_data.Stock
   Text7.Text = m_data.PartMake
   
errOut:
End Sub

'- Rotate Track ball
'  given a point on the screen the mouse was moved to
'  simulate a track ball
Private Sub RotateTrackBall(x As Integer, y As Integer)

    
    Dim delta_x As Single, delta_y As Single
    Dim delta_r As Single, radius As Single, denom As Single, angle As Single
    
    ' rotation axis in camcoords, worldcoords, sframecoords
    Dim axisC As D3DVECTOR
    Dim wc As D3DVECTOR
    Dim axisS As D3DVECTOR
    Dim base As D3DVECTOR
    Dim origin As D3DVECTOR
    
    delta_x = x - m_lastX
    delta_y = y - m_lastY
    m_lastX = x
    m_lastY = y

            
     delta_r = Sqr(delta_x * delta_x + delta_y * delta_y)
     radius = 50
     denom = Sqr(radius * radius + delta_r * delta_r)
    
    If (delta_r = 0 Or denom = 0) Then Exit Sub
    angle = (delta_r / denom)

    axisC.x = (-delta_y / delta_r)
    axisC.y = (-delta_x / delta_r)
    axisC.z = 0

    m_largewindow.m_cameraFrame.Transform wc, axisC
    m_root.InverseTransform axisS, wc
        
    m_largewindow.m_cameraFrame.Transform wc, origin
    m_root.InverseTransform base, wc
    
    axisS.x = axisS.x - base.x
    axisS.y = axisS.y - base.y
    axisS.z = axisS.z - base.z
    
    m_root.AddRotation D3DRMCOMBINE_BEFORE, axisS.x, axisS.y, axisS.z, angle
    
End Sub

'- LoadAssembly
'  See if we have the assembly loaded
'  if not figure out which model to use from a db
'  and load it
'  by default it will attach it to the scene

Function LoadAssembly(sname As String) As Long
    
    
    Dim I As Long
    Dim strCap As String
    Dim strModel As String
    
    Static b As Boolean
    
    
    If b = True Then Exit Function
    b = True
    
    
    'make sure we dont habe it already
    For I = 1 To m_nAssembly
        If sname = m_assemblyName(I) Then
            LoadAssembly = I
            b = False
            Exit Function
        End If
    Next
    
    
    m_nAssembly = m_nAssembly + 1
    m_assemblyName(m_nAssembly) = sname
    
    
    'look up the model we need to load
    'for this example we only show 1 model
    'but one could query for the files from a database
    strModel = "engine1.x"
    
    Set m_assemblies(m_nAssembly) = m_rm.CreateFrame(m_scene)
    strCap = Me.Caption
    Me.Caption = "Loading- please wait"
    DoEvents
    Err.Number = 0

    Form2.Top = Me.Top + Me.Height / 4
    Form2.Left = Me.Left + Me.Width / 8
    Form2.Show
    DoEvents
    
    m_assemblies(m_nAssembly).LoadFromFile strModel, 0, 0, Nothing, Nothing
        
    If Err.Number <> 0 Then
        m_scene.DeleteChild m_assemblies(m_nAssembly)
        Set m_assemblies(m_nAssembly) = Nothing
        m_assemblyName(m_nAssembly) = ""
        m_nAssembly = m_nAssembly - 1
        Unload Form2
        Me.Caption = strCap
    
        GoTo errOut
    End If
    Me.Caption = strCap
    
    m_assemblies(m_nAssembly).SetPosition Nothing, 0, 0, 10

        
    Recolor m_assemblies(m_nAssembly)
        
    LoadAssembly = m_nAssembly
    
    
    Unload Form2
    DoEvents
    If fLoading Then End
    m_largewindow.Render
    
    DoEvents
    
    Set m_root = m_assemblies(m_nAssembly)
    
    m_largewindow.Render
    
    m_binit = True
    
errOut:
    
    
    b = False
    
    
    TreeView1.Enabled = True
    largepict.SetFocus
    DoEvents
    
End Function


Private Sub Form_QueryUnload(Cancel As Integer, UnloadMode As Integer)
    fLoading = True
End Sub

'- MOUSE DOWN
'
Private Sub largepict_MouseDown(Button As Integer, Shift As Integer, x As Single, y As Single)
    
    If m_binit = False Then Exit Sub
    
    Dim mb As Direct3DRMMeshBuilder3
    Dim r As Integer, c As Integer
    Dim f As Direct3DRMFrame3
    Dim p As Direct3DRMFrame3
    Dim strName As String
    
    
    '- save our current position
    m_bMouseDown = True
    m_lastX = x
    m_lastY = y
    
    
    If Button = 1 Then
    
        
        
    
        
        'Get the frame under the the mouse
        Set f = m_largewindow.Pick(CLng(x), CLng(y))
        
        If f Is Nothing Then Exit Sub
        
        Recolor m_root
        
        f.SetColor &HFFFF0000
        
        'Get its id and call SelectPart
        'to fill in our text boxes
        strName = f.GetName()
        strName = Right$(strName, Len(strName) - 1)
        'The words V6 and Chevy are part of the manifold cover.
        If strName = "words" Or strName = "v6" Then strName = "manifoldt"
        
        SelectPart strName
        SelectTreeview strName
        DoEvents
        
    End If
    
    m_largewindow.Render
    
End Sub

'- MOUSE MOVE
'
Private Sub largepict_MouseMove(Button As Integer, Shift As Integer, x As Single, y As Single)
    
    '- dont do anything unless the mouse is down
    If m_bMouseDown = False Then
        Exit Sub
    End If
    
    '- Rotate the object
    RotateTrackBall CInt(x), CInt(y)
    
    '- Rerender
    m_largewindow.Render
        
End Sub

'- MOUSE UP
'  reset the mouse state
'
Private Sub largepict_MouseUp(Button As Integer, Shift As Integer, x As Single, y As Single)
    m_bMouseDown = False
End Sub

Private Sub largepict_Paint()
    If m_largewindow Is Nothing Then Exit Sub
    m_largewindow.Render
End Sub

Private Sub MENU_ABOUT_Click()
    MsgBox "The model used by this sample, engine1.x, is provided courtesy of Viewpoint"+ chr(10)+chr(13)+ _
	"Digital, Inc. (www.viewpoint.com).  It is provided for use with this sample"+ chr(10)+chr(13)+ _
	"only and cannot be distributed with any application without prior written"+ chr(10)+chr(13)+ _
	"consent.  V6 Engine Model copyright 1999 Viewpoint Digital, Inc.."
End Sub

Private Sub MENU_EXIT_Click()
    End
End Sub

Private Sub TreeView1_Expand(ByVal Node As MSComctlLib.Node)
    Dim I As Long
    
    Static b As Boolean
        
    If b Then Exit Sub
    b = True
        
    'See if they are asking for a new assembly alltogether
    If Mid$(Node.Tag, 1, 8) = "ASSMBLY:" Then
        m_bInLoad = True
        I = LoadAssembly(Node.Tag)
        If I = 0 Then
            MsgBox "Assembly not available at this time- try a different Engine"
            b = False
            Exit Sub
        End If
        
    End If
    
    b = False
    
End Sub

'- TREEVIEW interaction
Private Sub TreeView1_NodeClick(ByVal Node As MSComctlLib.Node)
    
    
    
    Dim b As Boolean
    If b Then Exit Sub
    b = True

    Dim o As Direct3DRMFrame3
    Dim I  As Long
    
    If Node.Tag = "" Then
        b = False
        Exit Sub
    End If
    
    'Fill in the text boxes
    SelectPart Node.Tag
    DoEvents
    
    If Not m_root Is Nothing Then
        'Turn everything grey
        Recolor m_root
        
        'Turn the selected object red
        Set o = m_rm.GetNamedObject("_" & Node.Tag)
        If Not (o Is Nothing) Then
            o.SetColor &HFFFF0000
        End If
    End If
    
    'Render
    m_largewindow.Render
    DoEvents
    
    b = False
End Sub


'- Recolor
'   turn the object grey
Sub Recolor(ByVal f As Direct3DRMFrame3)
    On Local Error Resume Next
    Dim l As Integer
    Dim I As Integer
    Dim f2 As Direct3DRMFrame3
    Dim m As Direct3DRMMeshBuilder3
    l = f.GetVisualCount
    
    l = f.GetChildren().GetSize
    f.SetColor &HFF606060
    
    f.SetMaterialMode D3DRMMATERIAL_FROMFRAME
            
    If l <> 0 Then
        For I = 0 To l - 1
            Set f2 = f.GetChildren().GetElement(I)
            Recolor f2
        Next
    End If
    
End Sub


'- FillTreeViewControl
Sub FillTreeViewControl()
    TreeView1.Nodes.Clear
    
    Dim sPartID As String
    Dim sDesc As String
    
    
    'A non-demo application would build the tree view
    'from the database and dynamically load in new
    'information into the treeview
    
    Dim n As Node
    Call TreeView1.Nodes.Add(, , "ASSEMBLIES", "Assemblies - [click here to start]")
    
    Set n = TreeView1.Nodes.Add("ASSEMBLIES", tvwChild, "ENG V6 1996", "V6 4 Liter 1996 - [click here]")
    n.Tag = "ASSMBLY:ENG V6 1996"
    n.Selected = True
    
    TreeView1.Nodes.Add("ASSEMBLIES", tvwChild, "ENG V8 1998", "V8 6 Liter 1998 - [not available]").Tag = ""
    TreeView1.Nodes.Add("ASSEMBLIES", tvwChild, "OTHERENG", "Other Assemblies not available").Tag = ""
    
    m_data.MoveTop
    Do While m_data.IsEOF() = False
        sPartID = m_data.ModelPart
        sDesc = m_data.Description
        TreeView1.Nodes.Add("ENG V6 1996", tvwChild, sPartID, sDesc).Tag = sPartID
        m_data.MoveNext
    Loop
    
End Sub

Sub SelectTreeview(sname As String)
    On Local Error Resume Next
    TreeView1.Nodes(sname).Selected = True
    DoEvents
End Sub


Sub FindMediaDir(sFile As String)
    On Local Error Resume Next
    If Mid$(App.Path, 2, 1) = ":" Then
        ChDrive Mid$(App.Path, 1, 1)
    End If
    ChDir App.Path
    If Dir$(sFile) = "" Then
        ChDir "..\media"
    End If
    If Dir$(sFile) = "" Then
        ChDir "..\..\media"
    End If
End Sub

