VERSION 5.00
Object = "{08216199-47EA-11D3-9479-00AA006C473C}#2.1#0"; "RMControl.ocx"
Begin VB.UserControl DxBarChart 
   ClientHeight    =   4020
   ClientLeft      =   0
   ClientTop       =   0
   ClientWidth     =   4800
   ScaleHeight     =   268
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   320
   Begin RMControl7.RMCanvas RMCanvas1 
      Height          =   2775
      Left            =   0
      TabIndex        =   1
      Top             =   0
      Width           =   4695
      _ExtentX        =   8281
      _ExtentY        =   4895
   End
   Begin VB.TextBox Text1 
      BorderStyle     =   0  'None
      Height          =   255
      Left            =   1080
      TabIndex        =   0
      Text            =   "Text1"
      Top             =   3000
      Visible         =   0   'False
      Width           =   2175
   End
   Begin VB.Timer Timer1 
      Enabled         =   0   'False
      Interval        =   2000
      Left            =   3960
      Top             =   3000
   End
   Begin VB.Menu MENU_ROW 
      Caption         =   "ROW"
      Begin VB.Menu MENU_HIDE_ROW 
         Caption         =   "Hide Row"
      End
      Begin VB.Menu MENU_UNHIDE_ROW 
         Caption         =   "Unhide Row"
      End
   End
   Begin VB.Menu MENU_BAR 
      Caption         =   "BAR"
      Begin VB.Menu MENU_HIDE_BAR 
         Caption         =   "Hide"
      End
      Begin VB.Menu MENU_UNHIDE_BAR 
         Caption         =   "Unhide"
      End
   End
   Begin VB.Menu MENU_COL 
      Caption         =   "COL"
      Begin VB.Menu MENU_HIDE_COL 
         Caption         =   "Hide Column"
      End
      Begin VB.Menu MENU_UNHIDE_COL 
         Caption         =   "Unhide Column"
      End
   End
   Begin VB.Menu MENU_OPTIONS 
      Caption         =   "OPTIONS"
      Begin VB.Menu MENU_RESET 
         Caption         =   "Reset Orientation"
      End
      Begin VB.Menu MENU_RANGE 
         Caption         =   "Set Range"
      End
   End
End
Attribute VB_Name = "DxBarChart"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Option Explicit

Dim m_cols As Integer
Dim m_rows As Integer
Dim m_root As Direct3DRMFrame3
Dim m_pivot As Direct3DRMFrame3

Dim m_row As Long
Dim m_col As Long

Dim m_rowColFrame() As Direct3DRMFrame3
Dim m_rowColScaleFrame() As Direct3DRMFrame3
Dim m_rowColMesh() As Direct3DRMMeshBuilder3
Dim m_rowColVal() As Single
Dim m_rowLabel() As String
Dim m_colLabel() As String
Dim m_colCol() As Long
Dim m_rowColCol() As Long

Dim m_rowTextures() As String
Dim m_colTextures() As String


Dim m_rowColMax As Single
Dim m_rowColMin As Single
Dim m_bMouseDown As Boolean
Dim m_bInit As Boolean
Dim m_range As Range

Dim m_bColTexture As Boolean
Dim m_bRowTexture As Boolean
Dim m_path As String



Dim rowoffset As Single
Dim coloffset As Single
Dim d3drm As Direct3DRM3
Dim scene As Direct3DRMFrame3

Dim m_SelectedVisual As Direct3DRMMeshBuilder3


Public Sub SetRowColData(row As Integer, col As Integer, val As Single)
    On Local Error Resume Next
    m_rowColVal(row, col) = val
    
    If m_rowColMax < val Then
        m_rowColMax = val
       Exit Sub
    End If
    Dim r As Single
    
    r = (val / m_rowColMax) * 6
    m_rowColScaleFrame(row, col).AddScale D3DRMCOMBINE_REPLACE, 1, r, 1

End Sub

Public Sub SetRowLabel(row As Integer, label As String)
    On Local Error Resume Next
    m_rowLabel(row) = label
End Sub

Public Sub SetColLabel(col As Integer, label As String)
    On Local Error Resume Next
    m_colLabel(col) = label
End Sub

Private Sub Recalibrate()
    On Local Error Resume Next
    Dim i As Integer
    Dim j As Integer
    
    For i = 1 To m_rows
        For j = 1 To m_cols
            Call SetRowColData(i, j, m_rowColVal(i, j))
        Next
    Next
            
End Sub

Public Sub InitFromCells(r As Range)
        On Local Error GoTo errOut:
        
        Dim i As Integer
        Dim j As Integer
        
        
        m_bInit = False
        
        Set m_range = r
        
        m_cols = r.Columns.Count - 1
        m_rows = r.rows.Count - 1
                

        
        ReDim m_rowTextures(m_rows)
        ReDim m_colTextures(m_cols)
                
        
        If val(r.Cells(2, m_cols + 1)) = 0 Then
            For i = 2 To m_rows
                m_rowTextures(i - 1) = r.Cells(i, m_cols + 1)
            Next
            
            m_bRowTexture = True
            m_rows = m_rows - 1
        End If
        

        
        If val(r.Cells(m_rows + 2, 2)) = 0 Then
            For i = 2 To m_cols
                m_colTextures(i - 1) = r.Cells(m_rows + 2, i)
            Next
            
            m_bColTexture = True
            m_cols = m_cols - 1
        End If
        

        
        UseColumnGraph m_rows, m_cols
        
        For i = 1 To m_rows
            SetRowLabel i, r.Cells(i + 1, 1)
        Next
        
        For i = 1 To m_cols
            SetColLabel i, r.Cells(1, i + 1)
        Next
        
        For i = 1 To m_cols
            For j = 1 To m_rows
                Call SetRowColData(j, i, r.Cells(j + 1, i + 1))
            Next
        Next
        
        
        RMCanvas1.SceneFrame.SetSceneBackgroundRGB 1, 1, 1
        
        Set m_range = r
        m_bInit = True
        
        
        
        Render
        
        Exit Sub
        
errOut:
        MsgBox "Invalid Range"
        DeleteOld
        RMCanvas1.SceneFrame.SetSceneBackgroundRGB 1, 1, 1
End Sub



Public Sub UseColumnGraph(rows As Integer, cols As Integer)
    
    Dim i As Long
    Dim j As Long
    
    m_bInit = False
    
    On Local Error Resume Next
    
    m_cols = cols
    m_rows = rows
            
    
    Dim m As Direct3DRMMeshBuilder3
    
    i = RMCanvas1.StartWindowed
    If i = 0 Then
        MsgBox "Couldnt initialize 3d library"
        Exit Sub
    End If
    
    Set d3drm = RMCanvas1.d3drm
    Set scene = RMCanvas1.SceneFrame
    RMCanvas1.SceneFrame.SetSceneBackgroundRGB 1, 1, 1
    
    RMCanvas1.Device.SetTextureQuality D3DRMTEXTURE_LINEAR
        
    DeleteOld
        
    Set m_pivot = d3drm.CreateFrame(scene)
    Set m_root = d3drm.CreateFrame(m_pivot)
    m_root.SetPosition m_pivot, 0, -0.1, 0
    
    ReDim m_rowColFrame(rows + 1, cols + 1)
    ReDim m_rowColScaleFrame(rows + 1, cols + 1)
    ReDim m_rowColMesh(rows + 1, cols + 1)
    ReDim m_rowColVal(rows + 1, cols + 1)
    ReDim m_rowColCol(rows + 1, cols + 1)
    ReDim m_rowLabel(rows + 1)
    ReDim m_colLabel(cols + 1)
    ReDim m_colCol(cols + 1)
    
    rowoffset = (rows + 1) / 2
    coloffset = (cols + 1) / 2
    
    
    
    
    
    'initialize scene graph
    For i = 1 To rows
        For j = 1 To cols
            
            CreateLabelBox i, j
            
            m_rowColMesh(i, j).SetName "RC:" + CStr(i) + Str(j)
            m_rowColMesh(i, j).ScaleMesh 1.2, 1, 1.2
            Call m_rowColMesh(i, j).SetColorRGB(2 * (j / cols), (i / rows), 1 - (j / cols))
            
            m_rowColCol(i, j) = RMCanvas1.dx.CreateColorRGB(2 * (j / cols), (i / rows), 1 - (j / cols))

        Next
    Next
                    
    
    
    Dim t As Direct3DRMTexture3
    Dim path As String
    
    If m_bRowTexture = True Then

        For i = 1 To m_rows
            CreateLabelBox i, 0
            SizeLabelBox i, 0
           
            
            path = m_path + m_colTextures(i)
            
            Set t = d3drm.LoadTexture(path)
                        
            m_rowColMesh(i, 0).SetTexture t
            m_rowColScaleFrame(i, 0).AddRotation D3DRMCOMBINE_BEFORE, 0, 1, 0, -3.14 / 2
            m_rowColMesh(i, 0).SetName "ROWA" + Str(i)
            
            CreateLabelBox i, m_cols + 1
            SizeLabelBox i, m_cols + 1
            m_rowColMesh(i, m_cols + 1).SetTexture t
            m_rowColMesh(i, m_cols + 1).SetName "ROWB" + Str(i)
            
        Next
    End If
    
    
    
    If m_bColTexture = True Then
        
        For i = 1 To m_cols
            CreateLabelBox 0, i
            SizeLabelBox 0, i
            path = m_path + m_rowTextures(i)
            Set t = d3drm.LoadTexture(path)
            
            m_rowColMesh(0, i).SetTexture t
            m_rowColMesh(0, i).SetName "COLA" + Str(i)
            
            CreateLabelBox m_rows + 1, i
            SizeLabelBox m_rows + 1, i
            m_rowColMesh(m_rows + 1, i).SetTexture t
            m_rowColMesh(m_rows + 1, i).SetName "COLB" + Str(i)
            
        Next
    
    End If
    
                
    'Base box
    Dim b As Direct3DRMMeshBuilder3
    Set b = RMCanvas1.CreateBoxMesh(2, 2, 2)
    b.Translate 0, 1, 0
    b.ScaleMesh 0.25, 0.25, 0.25

    
    b.Translate 0, -0.5, 0
    b.ScaleMesh cols * 2, 0.5, rows * 2
    b.SetColorRGB 1, 1, 1
    m_root.AddVisual b
                    

    scene.SetSceneBackground -&HFFFFFFFF
    RMCanvas1.DirLight.SetColor &HFFFFFFFF
    RMCanvas1.DirLightFrame.SetPosition Nothing, 10, 5, -10
    RMCanvas1.DirLightFrame.LookAt m_root, Nothing, 0
    
    RMCanvas1.Viewport.SetProjection D3DRMPROJECT_ORTHOGRAPHIC
    m_root.AddScale D3DRMCOMBINE_AFTER, 0.1, 0.1, 0.1
    
    m_bInit = True
    
    
End Sub

Sub CreateLabelBox(i As Long, j As Long)
        Dim b As Direct3DRMMeshBuilder3
        On Local Error Resume Next
        Set m_rowColFrame(i, j) = d3drm.CreateFrame(m_root)
        Set m_rowColScaleFrame(i, j) = d3drm.CreateFrame(m_rowColFrame(i, j))
        m_rowColFrame(i, j).SetPosition m_root, j - coloffset, 0, i - rowoffset
        Set b = RMCanvas1.CreateBoxMesh(2, 2, 2)
        b.Translate 0, 1, 0
        b.ScaleMesh 0.25, 0.25, 0.25
        Set m_rowColMesh(i, j) = b
        
        m_rowColScaleFrame(i, j).AddVisual m_rowColMesh(i, j)
        
End Sub

Sub SizeLabelBox(i As Long, j As Long)
    On Local Error Resume Next
        m_rowColMesh(i, j).ScaleMesh 1.8, 1.8, 1.8
        m_rowColMesh(i, j).Translate 0, -1, 0
        m_rowColMesh(i, j).SetColorRGB 1, 1, 1
        m_rowColMesh(i, j).SetQuality D3DRMLIGHT_OFF Or D3DRMSHADE_FLAT Or D3DRMFILL_SOLID

End Sub

Function box() As Direct3DRMMeshBuilder3
End Function


Sub UpdateLabels()
    On Local Error Resume Next
    Dim i As Integer
    Dim rowoffset As Single, coloffset As Single
    
    rowoffset = (m_rows + 1) / 2
    coloffset = (m_cols + 1) / 2

    For i = 1 To m_cols
        m_rowColScaleFrame(0, i).LookAt RMCanvas1.CameraFrame, Nothing, 0
        m_rowColFrame(0, i).SetPosition m_root, 0 - coloffset, 0, i - rowoffset
    Next
    
    For i = 1 To m_rows
        m_rowColFrame(i, 0).SetPosition m_root, i - coloffset, 0, 0 - rowoffset
        m_rowColScaleFrame(i, 0).LookAt RMCanvas1.CameraFrame, Nothing, 0
    Next
    
End Sub


Sub Render()
    On Local Error Resume Next
    RMCanvas1.Device.SetTextureQuality D3DRMTEXTURE_LINEAR
    RMCanvas1.Update
End Sub



Private Sub MENU_HIDE_BAR_Click()
    If m_bInit = False Then Exit Sub
    m_SelectedVisual.SetQuality D3DRMRENDER_WIREFRAME
End Sub

Private Sub MENU_HIDE_COL_Click()
    If m_bInit = False Then Exit Sub
    Dim i As Integer
    For i = 1 To m_rows
        
        With m_rowColMesh(i, m_col)
            .SetQuality D3DRMRENDER_WIREFRAME
        End With
    Next
End Sub

Private Sub MENU_HIDE_ROW_Click()
    If m_bInit = False Then Exit Sub
    Dim i As Integer
    For i = 1 To m_cols
        With m_rowColMesh(m_row, i)
            .SetQuality D3DRMRENDER_WIREFRAME
        End With
    Next
End Sub

Private Sub MENU_RANGE_Click()
    On Local Error GoTo errOut
    Dim sRange As String
    Dim r As Range
    Dim l1 As Integer
    Dim l2 As Integer
    
    sRange = InputBox("Range:", "Enter Range", "A3:F8")
    
    Dim ws As Worksheet
    Dim wb As Workbook
    
    Set wb = UserControl.Parent
    Set ws = wb.ActiveSheet
    Set r = ws.Range(sRange)
    
    m_path = UserControl.Parent.FullName
    
    l1 = Len(UserControl.Parent.Name)
    l2 = Len(m_path)
    
    m_path = Mid$(m_path, 1, l2 - l1)
    

    RMCanvas1.Visible = True
    InitFromCells r
    
    Exit Sub
    
errOut:
    MsgBox "Unable to set Range"
End Sub

Private Sub MENU_UNHIDE_BAR_Click()
    If m_bInit = False Then Exit Sub
    m_SelectedVisual.SetQuality D3DRMRENDER_GOURAUD
End Sub

Private Sub MENU_UNHIDE_COL_Click()
    If m_bInit = False Then Exit Sub
    Dim i As Integer
    For i = 1 To m_rows
        With m_rowColMesh(i, m_col)
            .SetQuality D3DRMRENDER_GOURAUD
            .SetColor m_rowColCol(i, m_col)
        End With
    Next
End Sub

Private Sub MENU_UNHIDE_ROW_Click()
    If m_bInit = False Then Exit Sub
    Dim i As Integer
    For i = 1 To m_rows
        m_rowColMesh(m_row, i).SetQuality D3DRMRENDER_GOURAUD
        m_rowColMesh(m_row, i).SetColor m_rowColCol(m_row, i)
    Next

End Sub

Private Sub RMCanvas1_KeyDown(keyCode As Integer, Shift As Integer)
    If m_bInit = False Then Exit Sub
        
    Set RMCanvas1.RotateFrame = m_root
    RMCanvas1.RotateFromXY 0, 0, True
    If keyCode = 39 Then
        m_root.AddRotation D3DRMCOMBINE_BEFORE, 0, 1, 0, -3.14 / 32
    ElseIf keyCode = 37 Then
        m_root.AddRotation D3DRMCOMBINE_BEFORE, 0, 1, 0, 3.14 / 32
    ElseIf keyCode = 40 Then
        RMCanvas1.RotateFromXY 0, 5, False
    ElseIf keyCode = 38 Then
        RMCanvas1.RotateFromXY 0, -5, False
    End If
        
    Set RMCanvas1.RotateFrame = Nothing
                
    Render
    
End Sub

Private Sub RMCanvas1_MouseDown(Button As Integer, Shift As Integer, x As Single, y As Single)
    
    If m_bInit = False Then
        
        If Button = 2 Then
            ShowOptions
        End If
        
        Exit Sub
    End If
    
    Set RMCanvas1.RotateFrame = m_root
    
    
    If Button = 1 Then
        m_bMouseDown = True
        
    ElseIf Button = 2 Then
        
    
        Dim mb As Direct3DRMMeshBuilder3
        Dim strName As String
                
        Set mb = RMCanvas1.PickTopMesh(CLng(x), CLng(y))
        If mb Is Nothing Then
                ShowOptions
                Text1.Text = ""
                Exit Sub
        End If
    
        
        strName = mb.GetName()
        If strName = "" Then
                ShowOptions
                Text1.Text = ""
                Exit Sub
        End If
    
        
        If Len(strName) < 3 Then GoTo errOut
        
        If Mid$(strName, 1, 3) = "ROW" Then
            m_row = Mid$(strName, 5)
            PopupMenu MENU_ROW, vbPopupMenuLeftAlign
        End If
        If Mid$(strName, 1, 3) = "COL" Then
            m_col = Mid$(strName, 5)
            PopupMenu MENU_COL, vbPopupMenuLeftAlign
        End If
                
                
    End If
    
    
    
errOut:
End Sub

Private Sub RMCanvas1_MouseMove(Button As Integer, Shift As Integer, x As Single, y As Single)
    
    If m_bInit = False Then Exit Sub
        
    If m_bMouseDown = False Then
        CheckToolTips x, y
        Exit Sub
    End If
    
     
End Sub

Private Sub RMCanvas1_MouseUp(Button As Integer, Shift As Integer, x As Single, y As Single)
    m_bMouseDown = False
    Set RMCanvas1.RotateFrame = Nothing
End Sub




Private Sub UserControl_Initialize()
     RMCanvas1.StartWindowed
     RMCanvas1.SceneFrame.SetSceneBackgroundRGB 1, 1, 1
     RMCanvas1.Visible = False
End Sub


Private Sub UserControl_MouseDown(Button As Integer, Shift As Integer, x As Single, y As Single)
    ShowOptions
End Sub

Private Sub UserControl_Resize()
    RMCanvas1.Width = UserControl.ScaleWidth
    RMCanvas1.Height = UserControl.ScaleHeight
End Sub

Private Sub CheckToolTips(x As Single, y As Single)
        
    Static b As Boolean
        
    If m_bMouseDown Then Exit Sub
    If b = True Then Exit Sub
    
    b = True
        
    
    
    Dim mb As Direct3DRMMeshBuilder3
    Dim strName As String
    Dim q As Long, r As Long, c As Long
    
    If x = 0 And y = 0 Then GoTo errOut
    
    Set mb = RMCanvas1.PickTopMesh(CLng(x), CLng(y))
    If mb Is Nothing Then GoTo errOut
    
    strName = mb.GetName()
    If strName = "" Then GoTo errOut

    If Len(strName) < 3 Then GoTo errOut
    If Mid$(strName, 1, 3) <> "RC:" Then GoTo errOut

    q = InStr(strName, " ")
    r = val(Mid$(strName, 4, q - 3))
    c = val(Mid$(strName, q + 1))
    
    Text1.Top = 0
    Text1.Left = 0
    Text1.Text = m_rowLabel(r) + " " + m_colLabel(c) + "=" + Str(m_rowColVal(r, c))
    Text1.ZOrder
    Text1.Visible = True
    
    
    b = False
               
    Exit Sub
               
errOut:
    b = False
    Text1.Text = ""

End Sub
    
Private Sub ShowOptions()
    Dim b As Boolean
    PopupMenu MENU_OPTIONS, vbPopupMenuLeftAlign
End Sub
           
Private Sub MENU_RESET_Click()
    If m_bInit = False Then Exit Sub
    m_root.SetOrientation m_pivot, 0, 0, 1, 0, 1, 0
    m_root.AddScale D3DRMCOMBINE_AFTER, 0.1, 0.1, 0.1
    m_root.SetPosition m_pivot, 0, -0.1, 0
    Render
End Sub

Private Sub UserControl_Terminate()
    m_bInit = False
End Sub

Private Sub DeleteOld()
    On Local Error Resume Next
    RMCanvas1.SceneFrame.DeleteChild m_pivot
End Sub
