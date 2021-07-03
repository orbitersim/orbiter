VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "XFileClass"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False


Option Explicit

Private Type XGroup
    IndexList() As Integer
    VertexList() As D3DVERTEX
    mtVertexList() As D3DVERTEX
    mat As D3DMATERIAL7
    Texture As DirectDrawSurface7
    TextureFileName As String
End Type

Private dx As DirectX7
Private RM As Direct3DRM3

Private TempMatrix As D3DMATRIX
Private TranMatrix As D3DMATRIX
Private RotMatrix As D3DMATRIX
Private ScaleMatrix As D3DMATRIX
Private GroupCount As Long
Private Groups() As XGroup
Private Box As D3DRMBOX

Private Active As Boolean

Public YawSpin As Single
Public PitchSpin As Single
Public FillMode As FillModeEnum
Public ShadeMode As ShadeModeEnum

Public Enum FillModeEnum
    Points = 1
    Solid = 3
    Wireframe = 2
End Enum

Public Enum ShadeModeEnum
    Gouraud = 2
    Flat = 1
End Enum




' Renders the group object to the backsurface
Private Sub RenderGroup(groupid As Long, d3ddev As Direct3DDevice7)
    On Local Error Resume Next
    Dim mat As D3DMATERIAL7
    Dim c As Long
    With Groups(groupid)
    
        If (Not .Texture Is Nothing) Then
            d3ddev.SetTexture 0, .Texture
        Else
            d3ddev.SetTexture 0, Nothing
        End If
        
        d3ddev.SetMaterial .mat
        
        d3ddev.DrawIndexedPrimitive D3DPT_TRIANGLELIST, D3DFVF_VERTEX, _
            .VertexList(0), UBound(.VertexList) + 1, .IndexList, _
                UBound(.IndexList), D3DDP_DEFAULT
    End With
                
End Sub


Private Sub AddVertex(groupid As Long, Vertex As D3DVERTEX)
    With Groups(groupid)
        LSet .VertexList(UBound(.VertexList)) = Vertex
        ReDim Preserve .VertexList(UBound(.VertexList) + 1)
        ReDim .mtVertexList(UBound(.VertexList))
    End With
End Sub

Private Function VertexIndex(groupid As Long, Index As Long) As Long
    With Groups(groupid)
        VertexIndex = .IndexList(Index)
    End With
End Function

Private Sub AddVertexIndex(groupid As Long, VertexIndex As Long)
    With Groups(groupid)
        .IndexList(UBound(.IndexList)) = VertexIndex
        ReDim Preserve .IndexList(UBound(.IndexList) + 1)
    End With
End Sub

Private Function IndexCount(groupid As Long) As Long
    With Groups(groupid)
        IndexCount = UBound(.IndexList) + 1
    End With
End Function


Public Sub Rotate(rotPitch As Single, rotYaw As Single)
    dx.RotateXMatrix TempMatrix, rotPitch
    dx.MatrixMultiply RotMatrix, TempMatrix, RotMatrix
    dx.RotateYMatrix TempMatrix, rotYaw
    dx.MatrixMultiply RotMatrix, TempMatrix, RotMatrix
End Sub

Public Sub SetPosition(tranX As Single, tranY As Single, tranZ As Single)
    TranslateMatrix TranMatrix, MakeVector(tranX, tranY, tranZ)
End Sub
Public Sub AdjustScale(sx As Single, sy As Single, sz As Single)
    With ScaleMatrix
        .rc11 = sx
        .rc22 = sy
        .rc33 = sz
    End With
End Sub

Private Sub Class_Initialize()
    ' setup the object's world matrix
    Set dx = New DirectX7
    ReDim Groups(0)
    dx.IdentityMatrix RotMatrix
    dx.IdentityMatrix ScaleMatrix
    dx.IdentityMatrix TempMatrix
    dx.IdentityMatrix TranMatrix
    FillMode = Solid
    ShadeMode = Gouraud
End Sub

' Loads an .X file into the scene.
Public Sub Load(ddraw As DirectDraw7, dev As Direct3DDevice7, FileName As String)
    

    Dim GIndex As Long
    Dim IIndex As Long
    Dim Index As Long
    Dim ArraySize As Long
    Dim IArray() As Long
    Dim VertCount As Long
    Dim FaceCount As Long
    Dim FaceVertCount As Long
    Dim TempVertex As D3DVERTEX
    Dim TempRMVertex As D3DRMVERTEX
    Dim TextureDesc As DDSURFACEDESC2
    Dim frame As Direct3DRMFrame3
    Dim strPath As String
    Dim MeshB As Direct3DRMMeshBuilder3
    Dim Mesh As Direct3DRMMesh

    strPath = StripFilenameFromPath(FileName)
    
    On Error GoTo XFileErr
    
    ' create the required retained mode objects... we're going to use
    ' retained mode to do our dirty work instead of trying to parse
    ' the .X file ourselves.
    
    Set RM = dx.Direct3DRMCreate
    
    Active = False
    
    ' try to load the .X file into a mesh
    On Local Error Resume Next
    Set MeshB = RM.CreateMeshBuilder
    MeshB.LoadFromFile FileName, "", D3DRMLOAD_FIRST, Nothing, Nothing
    
    ' if that didnt work try loading it into a frame
    If Err.Number <> 0 Then
        Err.Clear
        On Error GoTo XFileErr
        Set MeshB = RM.CreateMeshBuilder
        Set frame = RM.CreateFrame(Nothing)
        frame.LoadFromFile FileName, "", D3DRMLOAD_FIRST, Nothing, Nothing
        MeshB.AddFrame frame
    End If
    
    MeshB.GetBox Box
    
    ' generate appropriate normals if they aren't already correct.
    MeshB.GenerateNormals 3.14 / 4, D3DRMGENERATENORMALS_USECREASEANGLE
    
    ' use the meshbuilder information to create a mesh object.
    ' it will be easier to parse from the mesh object than from the
    ' meshbuilder.
    Set Mesh = MeshB.CreateMesh
    
    ' resize our group object array
    GroupCount = Mesh.GetGroupCount
    ReDim Groups(GroupCount - 1)
    
    ' loop through the number of groups in the mesh
    ' each group can contain a seperate texture
    For GIndex = 0 To Mesh.GetGroupCount - 1
        
        ' get the required information for our parsing
        With Groups(GIndex)
            ReDim .IndexList(0)
            ReDim .VertexList(0)
            ReDim .mtVertexList(0)
        End With

        
        Mesh.GetSizes GIndex, VertCount, FaceCount, FaceVertCount, ArraySize
        ReDim IArray(ArraySize)
        
        Mesh.GetGroupData GIndex, IArray()
                       
        
        'setup our texture for this particular group object
        If dev.GetDeviceGuid() = "IID_IDirect3DHALDevice" Then
            TextureDesc.lFlags = DDSD_CAPS
            TextureDesc.ddsCaps.lCaps = DDSCAPS_TEXTURE
            TextureDesc.ddsCaps.lCaps2 = DDSCAPS2_TEXTUREMANAGE
        Else
            TextureDesc.lFlags = DDSD_CAPS
            TextureDesc.ddsCaps.lCaps = DDSCAPS_TEXTURE Or DDSCAPS_SYSTEMMEMORY
            TextureDesc.ddsCaps.lCaps2 = 0
        End If
        
        
        If Not Mesh.GetGroupTexture(GIndex) Is Nothing Then
            On Local Error Resume Next
            With Groups(GIndex)
                .TextureFileName = strPath + Mesh.GetGroupTexture(GIndex).GetName
                Set .Texture = ddraw.CreateSurfaceFromFile(.TextureFileName, TextureDesc)
            End With
            On Error GoTo XFileErr
        End If
    
    
        
        Dim c As Long
        Dim rmmat As Direct3DRMMaterial2
        c = Mesh.GetGroupColor(GIndex)
        Set rmmat = Mesh.GetGroupMaterial(GIndex)
        
        With Groups(GIndex).mat.diffuse
            .a = dx.ColorGetAlpha(c)
            .r = dx.ColorGetRed(c)
            .g = dx.ColorGetGreen(c)
            .b = dx.ColorGetBlue(c)
        End With
        With Groups(GIndex).mat.Ambient
            rmmat.GetAmbient .r, .g, .b
            .a = 1
        End With
        With Groups(GIndex).mat.specular
            rmmat.GetSpecular .r, .g, .b
            .a = 1
        End With
        With Groups(GIndex).mat.emissive
            rmmat.GetEmissive .r, .g, .b
            .a = 1
        End With
        Groups(GIndex).mat.power = rmmat.GetPower()

        
        ' loop through the faces in the group
        ' we'll need to deal with 4 verterice faces differently than
        ' 3 sided.  we're adding indices to the group object here.
        Dim fanIndex As Long
        Dim j As Long
        Dim bGetFaceCount As Boolean
        
        IIndex = 0
        bGetFaceCount = False
        If FaceVertCount = 0 Then bGetFaceCount = True
        
        Do While IIndex < ArraySize
             If bGetFaceCount Then
                FaceVertCount = IArray(IIndex)
                IIndex = IIndex + 1
             End If
             

            fanIndex = IIndex
                
                
            For j = 0 To FaceVertCount - 3
                'add the fan point
                AddVertexIndex GIndex, IArray(fanIndex)
                
                'add 2 other points
                AddVertexIndex GIndex, IArray(IIndex + j + 1)
                AddVertexIndex GIndex, IArray(IIndex + j + 2)
            Next
            IIndex = IIndex + FaceVertCount
        Loop
             
        
        ' loop through the vertices and add them to the group object.
        For Index = 0 To VertCount - 1
                Mesh.GetVertex GIndex, Index, TempRMVertex
                With TempRMVertex
                    TempVertex.x = .position.x
                    TempVertex.y = .position.y
                    TempVertex.z = .position.z
                    TempVertex.nx = .Normal.x
                    TempVertex.ny = .Normal.y
                    TempVertex.nz = .Normal.z
                    TempVertex.tu = .tu
                    TempVertex.tv = .tv
                End With
                AddVertex GIndex, TempVertex
        Next
    Next
    
    Active = True
Exit Sub

XFileErr:
 
    MsgBox "error loading xfile"
    Active = False
End Sub

Public Sub Render(d3ddev As Direct3DDevice7)
    
    On Local Error Resume Next

    Dim Index As Long

    If Not Active Then Exit Sub
            
    dx.RotateXMatrix TempMatrix, PitchSpin
    dx.MatrixMultiply RotMatrix, TempMatrix, RotMatrix
    dx.RotateYMatrix TempMatrix, YawSpin
    dx.MatrixMultiply RotMatrix, TempMatrix, RotMatrix
    
    dx.MatrixMultiply TempMatrix, RotMatrix, TranMatrix
    dx.MatrixMultiply TempMatrix, ScaleMatrix, TempMatrix
    
    
    d3ddev.SetTransform D3DTRANSFORMSTATE_WORLD, TempMatrix
    
    d3ddev.SetRenderState D3DRENDERSTATE_FILLMODE, CLng(FillMode)
    d3ddev.SetRenderState D3DRENDERSTATE_SHADEMODE, CLng(ShadeMode)
    
    
    ' render each group
    For Index = 0 To UBound(Groups)
        RenderGroup Index, d3ddev
    Next
    

End Sub

Private Function StripFilenameFromPath(sPath As String) As String
    Dim Q As Integer
    Dim r As Integer
    
    'do we have a path string
    r = InStr(sPath, "\")
    If r = 0 Then Exit Function

    Do While r <> 0
        Q = r
        r = InStr(r + 1, sPath, "\")
    Loop
    
    StripFilenameFromPath = Mid$(sPath, 1, Q)
End Function

Public Function GetMinExtent(X1 As Single, Y1 As Single, z1 As Single)
    X1 = Box.Min.x
    Y1 = Box.Min.y
    z1 = Box.Min.z
End Function

Public Function GetMaxExtent(X2 As Single, Y2 As Single, z2 As Single)
    X2 = Box.Max.x
    Y2 = Box.Max.y
    z2 = Box.Max.z
End Function

Public Function ReloadTextures(ddraw As DirectDraw7, dev As Direct3DDevice7)
    On Local Error Resume Next
    Dim i As Long
    Dim TextureDesc As DDSURFACEDESC2
            
    
    'setup our texture for this particular group object
    If dev.GetDeviceGuid() = "IID_IDirect3DHALDevice" Then
        TextureDesc.lFlags = DDSD_CAPS
        TextureDesc.ddsCaps.lCaps = DDSCAPS_TEXTURE
        TextureDesc.ddsCaps.lCaps2 = DDSCAPS2_TEXTUREMANAGE
    Else
        TextureDesc.lFlags = DDSD_CAPS
        TextureDesc.ddsCaps.lCaps = DDSCAPS_TEXTURE Or DDSCAPS_SYSTEMMEMORY
        TextureDesc.ddsCaps.lCaps2 = 0
    End If
                
    For i = 0 To GroupCount - 1
        With Groups(i)
            Set .Texture = ddraw.CreateSurfaceFromFile(.TextureFileName, TextureDesc)
        End With
    Next
End Function



Private Function MakeVector(x As Single, y As Single, z As Single) As D3DVECTOR
    Dim v As D3DVECTOR
    v.x = x
    v.y = y
    v.z = z
    MakeVector = v
End Function



