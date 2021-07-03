VERSION 5.00
Object = "{08216199-47EA-11D3-9479-00AA006C473C}#2.1#0"; "RMControl.ocx"
Begin VB.UserControl XYZGraph 
   ClientHeight    =   4215
   ClientLeft      =   0
   ClientTop       =   0
   ClientWidth     =   5535
   ScaleHeight     =   281
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   369
   Begin VB.TextBox Text1 
      Appearance      =   0  'Flat
      Height          =   375
      Left            =   0
      TabIndex        =   0
      Top             =   0
      Visible         =   0   'False
      Width           =   4335
   End
   Begin RMControl7.RMCanvas RMCanvas1 
      Height          =   3495
      Left            =   0
      TabIndex        =   1
      Top             =   0
      Width           =   5055
      _ExtentX        =   8916
      _ExtentY        =   6165
   End
   Begin VB.Menu MENU_POP 
      Caption         =   "Pop Up"
      Begin VB.Menu MENU_PERSPECTIVE 
         Caption         =   "Perspective"
      End
      Begin VB.Menu MENU_ORTHO 
         Caption         =   "Orthographic"
      End
      Begin VB.Menu MENU_RANGE 
         Caption         =   "Set Range"
      End
   End
End
Attribute VB_Name = "XYZGraph"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
' XYZ Scatter Graph
'

Option Explicit

Private Type pointdata
    X As Single
    Y As Single
    z As Single
End Type

Dim m_id As Long

Dim m_root As Direct3DRMFrame3
Dim m_pivot As Direct3DRMFrame3
Dim m_pointFrame() As Direct3DRMFrame3
Dim m_pointMesh() As Direct3DRMMeshBuilder3

Dim m_maxZ As Single
Dim m_minZ As Single
Dim m_spreadZ As Single
Dim m_labelZ As String

Dim m_maxX As Single
Dim m_minX As Single
Dim m_spreadX As Single
Dim m_labelX As String

Dim m_minY As Single
Dim m_maxY As Single
Dim m_spreadY As Single
Dim m_labelY As String

Dim m_bMouseDown As Boolean
Dim m_binit As Boolean
Dim m_range As Range
Dim m_path As String

Dim d3drm As Direct3DRM3
Dim scene As Direct3DRMFrame3
Dim m_nPoints As Long
Dim m_nMaxPoints As Long
Dim m_nRows As Long
Dim m_Points() As pointdata
Dim m_bInitFromCells As Boolean


Public Function ClearPoints()
    m_nPoints = 0
    m_nMaxPoints = 100
    ReDim m_Points(m_nMaxPoints)
End Function

Public Function AddPoint(X As Single, Y As Single, z As Single)
    m_nPoints = m_nPoints + 1
    m_Points(m_nPoints).X = X
    m_Points(m_nPoints).Y = Y
    m_Points(m_nPoints).z = z
    
    If m_nPoints >= m_nMaxPoints Then
        m_nMaxPoints = m_nMaxPoints + 100
        ReDim Preserve m_Points(m_nMaxPoints)
    End If
End Function


Public Function GraphPoints() As Boolean
    Set m_range = Nothing
    m_binit = False

    dim dx7 as new Directx7
    if dx7.Systembpp <= 8 then
        MsgBox "This control designed to run on high color displays"
	exit function
    end if
    
    DestroyOld
    
    Start3D
        
    AllocateMemory
    CreatePoints
    UpdatePointDataFromPoints
    CreateBackDrop
    
    m_binit = True
    m_bInitFromCells = False
    
    Render
End Function

Public Sub GraphCells(r As Range)
        Dim i As Integer
        
        m_binit = False

    	dim dx7 as new Directx7
	if dx7.Systembpp <= 8 then
        	MsgBox "This control designed to run on high color displays"
		exit sub
	end if


        ClearPoints
        Set m_range = r
        m_nRows = r.Rows.Count
        
        DestroyOld
        
        Start3D

        m_labelX = m_range.Cells(1, 1)
        m_labelY = m_range.Cells(1, 2)
        m_labelZ = m_range.Cells(1, 3)
        

        For i = 2 To m_nRows
            AddPoint CSng(m_range.Cells(i, 1)), CSng(m_range.Cells(i, 2)), CSng(m_range.Cells(i, 3))
        Next
                
          
        AllocateMemory
        CreatePoints
        UpdatePointDataFromPoints
        CreateBackDrop
    
        m_binit = True
        m_bInitFromCells = True
        Render
End Sub


Private Sub CreatePoints()
    Dim i As Integer
    For i = 1 To m_nPoints
        Set m_pointFrame(i) = d3drm.CreateFrame(m_root)
        Set m_pointMesh(i) = RMCanvas1.CreateBoxMesh(1, 1, 1)
                    
        m_pointMesh(i).ScaleMesh 0.05, 0.05, 0.05
        m_pointMesh(i).SetColorRGB 0, 1, 0
        m_pointFrame(i).AddVisual m_pointMesh(i)
        
    Next
End Sub



Private Sub UpdatePointDataFromCells()
        Dim i As Integer
        
        Dim X As Single
        Dim Y As Single
        Dim z As Single
        Dim minN As Single
        Dim maxN As Single
        minN = -Exp(10)
        maxN = Exp(10)
        m_maxX = minN
        m_maxY = minN
        m_maxZ = minN
        m_minX = maxN
        m_minY = maxN
        m_minZ = maxN
        
        For i = 1 To m_nPoints
        
            If m_minX > m_range.Cells(i, 1) Then m_minX = m_range.Cells(i, 1)
            If m_minY > m_range.Cells(i, 2) Then m_minY = m_range.Cells(i, 2)
            If m_minZ > m_range.Cells(i, 3) Then m_minZ = m_range.Cells(i, 3)
            
            If m_maxX < m_range.Cells(i, 1) Then m_maxX = m_range.Cells(i, 1)
            If m_maxY < m_range.Cells(i, 2) Then m_maxY = m_range.Cells(i, 2)
            If m_maxZ < m_range.Cells(i, 3) Then m_maxZ = m_range.Cells(i, 3)
                        
        Next
        
        m_spreadX = m_maxX - m_minX
        m_spreadY = m_maxY - m_minY
        m_spreadZ = m_maxZ - m_minZ
        
        For i = 1 To m_nPoints
            X = (m_maxX - m_range.Cells(i, 1)) / m_spreadX
            Y = (m_maxY - m_range.Cells(i, 2)) / m_spreadY
            z = (m_maxZ - m_range.Cells(i, 3)) / m_spreadZ
            m_pointFrame(i).SetPosition m_root, X - 0.5, Y - 0.5, z - 0.5
            m_pointMesh(i).SetName "point " + Str(i)
            
            Dim mb2 As Direct3DRMMeshBuilder3
            Set mb2 = RMCanvas1.CreateBoxMesh(0.01, Y, 0.01)
            mb2.Translate 0, -Y / 2, 0
            mb2.SetColor &H20001616
            m_pointFrame(i).AddVisual mb2
        Next
        
End Sub


Private Sub UpdatePointDataFromPoints()
        Dim i As Integer
        
        Dim X As Single
        Dim Y As Single
        Dim z As Single
        Dim minN As Single
        Dim maxN As Single
        minN = -Exp(10)
        maxN = Exp(10)
        m_maxX = minN
        m_maxY = minN
        m_maxZ = minN
        m_minX = maxN
        m_minY = maxN
        m_minZ = maxN
        
        For i = 1 To m_nPoints
        
            If m_minX > m_Points(i).X Then m_minX = m_Points(i).X
            If m_minY > m_Points(i).Y Then m_minY = m_Points(i).Y
            If m_minZ > m_Points(i).z Then m_minZ = m_Points(i).z
            
            If m_maxX < m_Points(i).X Then m_maxX = m_Points(i).X
            If m_maxY < m_Points(i).Y Then m_maxY = m_Points(i).Y
            If m_maxZ < m_Points(i).z Then m_maxZ = m_Points(i).z
                        
        Next
        
        m_spreadX = m_maxX - m_minX
        m_spreadY = m_maxY - m_minY
        m_spreadZ = m_maxZ - m_minZ

        
        For i = 1 To m_nPoints
            
            X = (-m_minX + m_Points(i).X) / m_spreadX
            Y = (-m_minY + m_Points(i).Y) / m_spreadY
            z = (-m_minZ + m_Points(i).z) / m_spreadZ

            m_pointFrame(i).SetPosition m_root, X - 0.5, Y - 0.5, z - 0.5
            m_pointMesh(i).SetName "point " + Str(i)
            
            Dim mb2 As Direct3DRMMeshBuilder3
            Set mb2 = RMCanvas1.CreateBoxMesh(0.01, Y, 0.01)
            mb2.Translate 0, -Y / 2, 0
            mb2.SetColor &H20001616
            m_pointFrame(i).AddVisual mb2

        Next
        

        
End Sub


Private Sub AllocateMemory()
    ReDim Preserve m_pointFrame(m_nPoints)
    ReDim Preserve m_pointMesh(m_nPoints)
End Sub


Private Sub DestroyOld()
    On Local Error Resume Next
    If Not m_root Is Nothing Then
        Set RMCanvas1.RotateFrame = Nothing
        RMCanvas1.SceneFrame.DeleteChild m_pivot
    End If

End Sub
    
    
Private Sub Start3D()


    RMCanvas1.Visible = True
    
    Dim m As Direct3DRMMeshBuilder3
    
    RMCanvas1.StartWindowed
    Set d3drm = RMCanvas1.d3drm
    Set scene = RMCanvas1.SceneFrame
    scene.SetSceneBackgroundRGB 1, 1, 1
    
    
    RMCanvas1.Device.SetTextureQuality D3DRMTEXTURE_LINEAR
    RMCanvas1.AmbientLight.SetColorRGB 0.2, 0.2, 0.2
        
        
    Set m_pivot = d3drm.CreateFrame(scene)
    Set m_root = d3drm.CreateFrame(m_pivot)
    
        
    m_root.AddScale D3DRMCOMBINE_REPLACE, 5, 5, 5
                
    
        
    RMCanvas1.DirLightFrame.SetPosition Nothing, 0, -1, -10
    
    RMCanvas1.DirLightFrame.LookAt m_root, Nothing, 0
    
    
End Sub

Private Sub CreateBackDrop()
    
    Dim m As Direct3DRMMeshBuilder3
    Dim f As Direct3DRMFace2
    Dim txy As Direct3DRMTexture3
    Dim txz As Direct3DRMTexture3
    Dim tzy As Direct3DRMTexture3
    Dim incx As Single
    Dim incy As Single
    Dim incz As Single
    Dim i As Long
    
    
    'Decide how the units are divided
    
    If m_spreadY > 1 Then
        incy = CInt(m_spreadY / 5)
        If incy = 0 Then incy = 0.5
    Else
        incy = m_maxY / 5
    End If
    
    If m_spreadZ > 1 Then
        incz = CInt(m_spreadZ / 5)
        If incz = 0 Then incz = 0.5
    Else
        incz = m_maxZ / 5
    End If
    
    If m_spreadX > 1 Then
        incx = CInt(m_spreadX / 5)
        If incx = 0 Then incx = 0.5
    Else
        incx = m_maxX / 6
    End If
    

    Set m = RMCanvas1.d3drm.CreateMeshBuilder()
        
    Set txy = CreatePanelTexture(m_minY, m_maxY, incy, m_minX, m_maxX, incx, m_labelY, m_labelX)
    Set txz = CreatePanelTexture(m_minZ, m_maxZ, incz, m_minX, m_maxX, incx, m_labelZ, m_labelX)
    Set tzy = CreatePanelTexture(m_minY, m_maxY, incy, m_minZ, m_maxZ, incz, m_labelY, m_labelZ)
    
    
    'Back Face
    Set f = RMCanvas1.d3drm.CreateFace()
    
    f.AddVertex 1, 1, 1:      f.AddVertex 1, -1, 1
    f.AddVertex -1, -1, 1:    f.AddVertex -1, 1, 1
    f.SetTexture txy
    m.AddFace f
    
    'Left face
    Set f = RMCanvas1.d3drm.CreateFace()
    f.AddVertex -1, 1, 1:    f.AddVertex -1, -1, 1
    f.AddVertex -1, -1, -1:  f.AddVertex -1, 1, -1
    f.SetTexture tzy
    m.AddFace f
        
    'Bottom face
    Set f = RMCanvas1.d3drm.CreateFace()
    f.AddVertex 1, -1, 1:    f.AddVertex 1, -1, -1
    f.AddVertex -1, -1, -1:  f.AddVertex -1, -1, 1
    f.SetTexture txz
    m.AddFace f
     
    For i = 0 To 2
        m.SetTextureCoordinates 3 + i * 4, 0, 0
        m.SetTextureCoordinates 2 + i * 4, 0, 1
        m.SetTextureCoordinates 1 + i * 4, 1, 1
        m.SetTextureCoordinates 0 + i * 4, 1, 0
   Next
    
    m.SetQuality D3DRMRENDER_UNLITFLAT
    m.GenerateNormals 0, 0
    m.ScaleMesh 0.5, 0.5, 0.5
    m_root.AddVisual m
    
End Sub


Private Sub Render()
    RMCanvas1.Device.SetTextureQuality D3DRMTEXTURE_LINEAR
    RMCanvas1.Update
End Sub


Private Sub MENU_ORTHO_Click()
    If m_binit = False Then Exit Sub
    m_root.AddScale D3DRMCOMBINE_REPLACE, 0.4, 0.4, 0.4
    RMCanvas1.Viewport.SetProjection D3DRMPROJECT_ORTHOGRAPHIC

End Sub

Private Sub MENU_PERSPECTIVE_Click()
   If m_binit = False Then Exit Sub
   m_root.AddScale D3DRMCOMBINE_REPLACE, 5, 5, 5
   RMCanvas1.Viewport.SetProjection D3DRMPROJECT_PERSPECTIVE

End Sub

Private Sub MENU_RANGE_Click()
    On Local Error GoTo errOut1
    
    
    
    Dim sRange As String
    Dim r As Range
    Dim l1 As Integer
    Dim l2 As Integer
    Dim wb As Workbook
    Dim ws As Worksheet
    sRange = InputBox("Range:", "Enter Range", "A1:C12")

    If sRange = "" Then Exit Sub
    

    Set wb = UserControl.Parent
    Set ws = wb.ActiveSheet
    Set r = ws.Range(sRange)
            
    On Local Error GoTo errOut2
        
    m_path = UserControl.Parent.FullName
    l1 = Len(UserControl.Parent.Name)
    l2 = Len(m_path)
    m_path = Mid$(m_path, 1, l2 - l1)
            
    GraphCells r
    
    
    Exit Sub
errOut1:
    MsgBox "Cant use Range, not in Excel"
    Exit Sub
    
errOut2:
    MsgBox "Unable to set Range"
    UserControl_Initialize
End Sub

Private Sub RMCanvas1_KeyDown(keyCode As Integer, Shift As Integer)
    Set RMCanvas1.RotateFrame = m_root
    If keyCode = 39 Then
        m_root.AddRotation D3DRMCOMBINE_BEFORE, 0, 1, 0, -3.14 / 32
    ElseIf keyCode = 37 Then
        m_root.AddRotation D3DRMCOMBINE_BEFORE, 0, 1, 0, 3.14 / 32
    ElseIf keyCode = 40 Then
        RMCanvas1.RotateFromXY 0, 0, True
        RMCanvas1.RotateFromXY 0, 5, False
    ElseIf keyCode = 38 Then
        RMCanvas1.RotateFromXY 0, 0, True
        RMCanvas1.RotateFromXY 0, -5, False
     End If
    Set RMCanvas1.RotateFrame = Nothing
    
    Render
    
End Sub

Private Sub RMCanvas1_MouseDown(Button As Integer, Shift As Integer, X As Single, Y As Single)
    Set RMCanvas1.RotateFrame = m_root
    m_bMouseDown = True
    If Button = 2 Then
        PopupMenu MENU_POP
    End If
End Sub

Private Sub RMCanvas1_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
    Dim mb As Direct3DRMMeshBuilder3
    Dim p As Long
    Dim strName As String
    Set mb = RMCanvas1.PickTopMesh(CLng(X), CLng(Y))
    If mb Is Nothing Then Exit Sub
    strName = mb.GetName()
    If strName = "" Then
        Text1.Visible = False
        Exit Sub
    End If
    
    If InStr(strName, "points") <> 0 Then Exit Sub
    p = Val(Mid$(strName, 7))
    Text1.Visible = True
    With m_Points(p)
        Text1.Text = m_labelX + "=" + Str(.X) + ": " + m_labelY + "=" + Str(.Y) + ": " + m_labelZ + "=" + Str(.z)
    End With
    
End Sub

Private Sub RMCanvas1_MouseUp(Button As Integer, Shift As Integer, X As Single, Y As Single)
    Set RMCanvas1.RotateFrame = Nothing
    m_bMouseDown = False
    Text1.Visible = False
    DoEvents
End Sub

Private Sub RMCanvas71_KeyDown(keyCode As Integer, Shift As Integer)

End Sub

Private Sub RMCanvas71_KeyPress(KeyAscii As Integer)

End Sub

Private Sub UserControl_MouseDown(Button As Integer, Shift As Integer, X As Single, Y As Single)
    If Button = 2 Then PopupMenu MENU_POP
End Sub


Private Sub UserControl_Initialize()
    RMCanvas1.Visible = False
    RMCanvas1.UseBackbuffer = False
    ClearPoints
    
    m_labelX = "X"
    m_labelY = "Y"
    m_labelZ = "Z"
    
End Sub


Private Sub UserControl_Resize()
    RMCanvas1.Width = UserControl.ScaleWidth
    RMCanvas1.Height = UserControl.ScaleHeight
End Sub

Private Function CreatePanelTexture(rowmin As Single, rowmax As Single, incr As Single, colmin As Single, colmax As Single, incc As Single, rowtext As String, coltext As String) As Direct3DRMTexture3
    'On Local Error GoTo errOut
    
    Dim dd As DirectDraw4
    Set dd = RMCanvas1.DDraw
    If dd Is Nothing Then Exit Function
    Dim surf As DirectDrawSurface4
    Dim ddsd As DDSURFACEDESC2
    ddsd.lFlags = DDSD_CAPS Or DDSD_WIDTH Or DDSD_HEIGHT
    ddsd.ddsCaps.lCaps = DDSCAPS_TEXTURE Or DDSCAPS_SYSTEMMEMORY
    ddsd.lWidth = 256
    ddsd.lHeight = 256
    Set surf = dd.CreateSurface(ddsd)
    
    surf.SetForeColor vbBlue
    surf.SetFillColor vbWhite
    surf.DrawBox 0, 0, 256, 256
    
    Dim at As Single
    Dim Y As Single
    Dim X As Single
    
    at = rowmin
    
    Do While at < rowmax
        If rowmax <> rowmin Then
            Y = 256 - 256 * (at - rowmin) / (rowmax - rowmin)
        Else
            Y = 10
        End If
        surf.SetForeColor &H505050
        surf.DrawLine 0, Y, 256, Y
        surf.SetForeColor vbRed
        surf.DrawText 2, Y - 5, Str(at), False
        at = at + incr
    Loop
    at = colmin
    Do While at < colmax
        If colmax <> colmin Then
            X = 256 * (at - colmin) / (colmax - colmin)
        Else
            X = 10
        End If
        surf.SetForeColor &H505000
        surf.DrawLine X, 0, X, 256
        surf.SetForeColor vbBlue
        surf.DrawText X - 2, 15, Str(at), False
        at = at + incc
    Loop

    surf.SetForeColor &HFF40&
    surf.DrawText 20, 128, rowtext, False
    surf.DrawText 128, 30, coltext, False

    Set CreatePanelTexture = RMCanvas1.d3drm.CreateTextureFromSurface(surf)
errOut:
End Function








 

