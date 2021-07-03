Attribute VB_Name = "MATH"
Option Explicit
'=================================
'Helper Library for Direct3DIM Applications
'=================================
'
' Note some functions have 2 implementations
' Ones that start with Ret tend to be slower
' than the ones that fill structures by reference
' both versions are provided because the former
' is more readable
'
' Also note that many of these function are also
' found on the DirectX root object of the DXVB
' typelibrary.
'
'=================================
'Constants for IMCanvas texturing
'=================================

Global Const D3DTEXTR_TRANSPARENTBLACK = 1
Global Const D3DTEXTR_TRANSPARENTWHITE = 2

'=================================
' FVF VERTEX  helpers
' used for mutli textured polygons
'=================================
Global Const D3DFVF_TEXTUREFORMAT2 = 0
Global Const D3DFVF_TEXTUREFORMAT1 = 3
Global Const D3DFVF_TEXTUREFORMAT3 = 1
Global Const D3DFVF_TEXTUREFORMAT4 = 2

'=================================
' MatrixMult
'=================================
' concatentates matrices together.
' not A x B is not equal to B x A
'
Sub MatrixMult(result As D3DMATRIX, a As D3DMATRIX, b As D3DMATRIX)
  Dim ret As D3DMATRIX
  Dim tmp As Double
  Dim i As Integer
  Dim j As Integer
  Dim k As Integer
  
  Call ZeroMatrix(ret)
  ret.rc11 = b.rc11 * a.rc11 + b.rc21 * a.rc12 + b.rc31 * a.rc13 + b.rc41 * a.rc14
  ret.rc12 = b.rc12 * a.rc11 + b.rc22 * a.rc12 + b.rc32 * a.rc13 + b.rc42 * a.rc14
  ret.rc13 = b.rc13 * a.rc11 + b.rc23 * a.rc12 + b.rc33 * a.rc13 + b.rc43 * a.rc14
  ret.rc14 = b.rc14 * a.rc11 + b.rc24 * a.rc12 + b.rc34 * a.rc13 + b.rc44 * a.rc14
  ret.rc21 = b.rc11 * a.rc21 + b.rc21 * a.rc22 + b.rc31 * a.rc23 + b.rc41 * a.rc24
  ret.rc22 = b.rc12 * a.rc21 + b.rc22 * a.rc22 + b.rc32 * a.rc23 + b.rc42 * a.rc24
  ret.rc23 = b.rc13 * a.rc21 + b.rc23 * a.rc22 + b.rc33 * a.rc23 + b.rc43 * a.rc24
  ret.rc24 = b.rc14 * a.rc21 + b.rc24 * a.rc22 + b.rc34 * a.rc23 + b.rc44 * a.rc24
  ret.rc31 = b.rc11 * a.rc31 + b.rc21 * a.rc32 + b.rc31 * a.rc33 + b.rc41 * a.rc34
  ret.rc32 = b.rc12 * a.rc31 + b.rc22 * a.rc32 + b.rc32 * a.rc33 + b.rc42 * a.rc34
  ret.rc33 = b.rc13 * a.rc31 + b.rc23 * a.rc32 + b.rc33 * a.rc33 + b.rc43 * a.rc34
  ret.rc34 = b.rc14 * a.rc31 + b.rc24 * a.rc32 + b.rc34 * a.rc33 + b.rc44 * a.rc34
  ret.rc41 = b.rc11 * a.rc41 + b.rc21 * a.rc42 + b.rc31 * a.rc43 + b.rc41 * a.rc44
  ret.rc42 = b.rc12 * a.rc41 + b.rc22 * a.rc42 + b.rc32 * a.rc43 + b.rc42 * a.rc44
  ret.rc43 = b.rc13 * a.rc41 + b.rc23 * a.rc42 + b.rc33 * a.rc43 + b.rc43 * a.rc44
  ret.rc44 = b.rc14 * a.rc41 + b.rc24 * a.rc42 + b.rc34 * a.rc43 + b.rc44 * a.rc44
  result = ret
End Sub

Function RetMatrixMult(a As D3DMATRIX, b As D3DMATRIX) As D3DMATRIX
  Dim ret As D3DMATRIX
  ret.rc11 = b.rc11 * a.rc11 + b.rc21 * a.rc12 + b.rc31 * a.rc13 + b.rc41 * a.rc14
  ret.rc12 = b.rc12 * a.rc11 + b.rc22 * a.rc12 + b.rc32 * a.rc13 + b.rc42 * a.rc14
  ret.rc13 = b.rc13 * a.rc11 + b.rc23 * a.rc12 + b.rc33 * a.rc13 + b.rc43 * a.rc14
  ret.rc14 = b.rc14 * a.rc11 + b.rc24 * a.rc12 + b.rc34 * a.rc13 + b.rc44 * a.rc14
  ret.rc21 = b.rc11 * a.rc21 + b.rc21 * a.rc22 + b.rc31 * a.rc23 + b.rc41 * a.rc24
  ret.rc22 = b.rc12 * a.rc21 + b.rc22 * a.rc22 + b.rc32 * a.rc23 + b.rc42 * a.rc24
  ret.rc23 = b.rc13 * a.rc21 + b.rc23 * a.rc22 + b.rc33 * a.rc23 + b.rc43 * a.rc24
  ret.rc24 = b.rc14 * a.rc21 + b.rc24 * a.rc22 + b.rc34 * a.rc23 + b.rc44 * a.rc24
  ret.rc31 = b.rc11 * a.rc31 + b.rc21 * a.rc32 + b.rc31 * a.rc33 + b.rc41 * a.rc34
  ret.rc32 = b.rc12 * a.rc31 + b.rc22 * a.rc32 + b.rc32 * a.rc33 + b.rc42 * a.rc34
  ret.rc33 = b.rc13 * a.rc31 + b.rc23 * a.rc32 + b.rc33 * a.rc33 + b.rc43 * a.rc34
  ret.rc34 = b.rc14 * a.rc31 + b.rc24 * a.rc32 + b.rc34 * a.rc33 + b.rc44 * a.rc34
  ret.rc41 = b.rc11 * a.rc41 + b.rc21 * a.rc42 + b.rc31 * a.rc43 + b.rc41 * a.rc44
  ret.rc42 = b.rc12 * a.rc41 + b.rc22 * a.rc42 + b.rc32 * a.rc43 + b.rc42 * a.rc44
  ret.rc43 = b.rc13 * a.rc41 + b.rc23 * a.rc42 + b.rc33 * a.rc43 + b.rc43 * a.rc44
  ret.rc44 = b.rc14 * a.rc41 + b.rc24 * a.rc42 + b.rc34 * a.rc43 + b.rc44 * a.rc44
  RetMatrixMult = ret
End Function

'=================================
' TranslateMatrix
'=================================
' used to position an object

Sub TranslateMatrix(m As D3DMATRIX, v As D3DVECTOR)
  Call IdentityMatrix(m)
  m.rc41 = v.x
  m.rc42 = v.y
  m.rc43 = v.z
End Sub

Function RetTranslateMatrix(v As D3DVECTOR) As D3DMATRIX
  Dim m As D3DMATRIX
  Call IdentityMatrix(m)
  m.rc41 = v.x
  m.rc42 = v.y
  m.rc43 = v.z
  RetTranslateMatrix = m
End Function


'=================================
' RotateXMatrix
'=================================
' rotate an object about x axis rad radians

Sub RotateXMatrix(ret As D3DMATRIX, rads As Single)
  Dim cosine As Single
  Dim sine As Single
  cosine = Cos(rads)
  sine = Sin(rads)
  Call IdentityMatrix(ret)
  ret.rc22 = cosine
  ret.rc33 = cosine
  ret.rc23 = -sine
  ret.rc32 = sine
End Sub

Function RetRotateXMatrix(rads As Single) As D3DMATRIX
  Dim cosine As Single
  Dim sine As Single
  Dim ret As D3DMATRIX
  cosine = Cos(rads)
  sine = Sin(rads)
  Call IdentityMatrix(ret)
  ret.rc22 = cosine
  ret.rc33 = cosine
  ret.rc23 = -sine
  ret.rc32 = sine
  RetRotateXMatrix = ret
End Function

'=================================
' RotateYMatrix
'=================================
' rotate an object about y axis rad radians

Sub RotateYMatrix(ret As D3DMATRIX, rads As Single)
  Dim cosine As Single
  Dim sine As Single
  cosine = Cos(rads)
  sine = Sin(rads)
  Call IdentityMatrix(ret)
  ret.rc11 = cosine
  ret.rc33 = cosine
  ret.rc13 = sine
  ret.rc31 = -sine
End Sub

Function RetRotateYMatrix(rads As Single) As D3DMATRIX
  Dim cosine As Single
  Dim sine As Single
  Dim ret As D3DMATRIX
  cosine = Cos(rads)
  sine = Sin(rads)
  Call IdentityMatrix(ret)
  ret.rc11 = cosine
  ret.rc33 = cosine
  ret.rc13 = sine
  ret.rc31 = -sine
  RetRotateYMatrix = ret
End Function

'=================================
' RotateZMatrix
'=================================
' rotate an object about z axis rad radians

Sub RotateZMatrix(ret As D3DMATRIX, rads As Single)
  Dim cosine As Single
  Dim sine As Single
  cosine = Cos(rads)
  sine = Sin(rads)
  Call IdentityMatrix(ret)
  ret.rc11 = cosine
  ret.rc22 = cosine
  ret.rc12 = -sine
  ret.rc21 = sine
End Sub

Function RetRotateZMatrix(rads As Single) As D3DMATRIX
  Dim ret As D3DMATRIX
  Dim cosine As Single
  Dim sine As Single
  cosine = Cos(rads)
  sine = Sin(rads)
  Call IdentityMatrix(ret)
  ret.rc11 = cosine
  ret.rc22 = cosine
  ret.rc12 = -sine
  ret.rc21 = sine
  RetRotateZMatrix = ret
End Function

'=================================
' ViewMatrix
'=================================
' setup the placement of the camera
' from the location of the camera
' at is where its looking toward
' up (usually 0 1 0) is the orientation
' roll is the sideways tilt of the camera


Sub ViewMatrix(view As D3DMATRIX, from As D3DVECTOR, at As D3DVECTOR, world_up As D3DVECTOR, roll As Single)
  
  Dim up As D3DVECTOR
  Dim right As D3DVECTOR
  Dim view_Dir As D3DVECTOR
  
  Call IdentityMatrix(view)
  Call VectorSubtract(view_Dir, at, from)
  Call VectorNormalize(view_Dir)
  
  'think lefthanded coords
  Call VectorCrossProduct(right, world_up, view_Dir)
  Call VectorCrossProduct(up, view_Dir, right)
  
  Call VectorNormalize(right)
  Call VectorNormalize(up)
  
  view.rc11 = right.x
  view.rc21 = right.y
  view.rc31 = right.z
  view.rc12 = up.x   'AK? should this be negative?
  view.rc22 = up.y
  view.rc32 = up.z
  view.rc13 = view_Dir.x
  view.rc23 = view_Dir.y
  view.rc33 = view_Dir.z
  
  view.rc41 = -VectorDotProduct(right, from)
  view.rc42 = -VectorDotProduct(up, from)
  view.rc43 = -VectorDotProduct(view_Dir, from)

  ' Set roll
  If (roll <> 0#) Then
      Dim rotZMat As D3DMATRIX
      Call RotateZMatrix(rotZMat, -roll)
      Call MatrixMult(view, rotZMat, view)
  End If
  
  
End Sub

Function RetViewMatrix(from As D3DVECTOR, at As D3DVECTOR, world_up As D3DVECTOR, roll As Single) As D3DMATRIX
    Dim ret As D3DMATRIX
    ViewMatrix ret, from, at, world_up, roll
  
End Function

'=================================
' ProjectionMatrix
'=================================
' near_plane (must be greter than zero)
' and far_plane define a view frustrum
' the near_plane define how close the camera
' can see in front of you and the far_plane
' detrermines how far away the camera can see.
' fov is in radians and determines the
' cone angle of the frusrum..
' (narrow  to wide angle)

Sub ProjectionMatrix(ret As D3DMATRIX, _
              near_plane As Single, _
                far_plane As Single, _
                fov As Single)

              
  Dim c As Single
  Dim s As Single
  Dim Q As Single
  
  
  c = Cos(fov * 0.5)
  s = Sin(fov * 0.5)
  Q = s / (1# - near_plane / far_plane)

  Call ZeroMatrix(ret)
  ret.rc11 = c
  ret.rc22 = c
  ret.rc33 = Q
  ret.rc43 = -Q * near_plane
  ret.rc34 = s
  
End Sub

Function RetProjectionMatrix( _
        near_plane As Single, _
        far_plane As Single, _
        fov As Single) As D3DMATRIX
    Dim ret As D3DMATRIX
    ProjectionMatrix ret, near_plane, far_plane, fov
    RetProjectionMatrix = ret
End Function

'=================================
' CopyMatrix
'=================================
Sub CopyMatrix(dest As D3DMATRIX, src As D3DMATRIX)
  
  dest.rc11 = src.rc11
  dest.rc12 = src.rc12
  dest.rc13 = src.rc13
  dest.rc14 = src.rc14
  dest.rc21 = src.rc21
  dest.rc22 = src.rc22
  dest.rc23 = src.rc23
  dest.rc24 = src.rc24
  dest.rc31 = src.rc31
  dest.rc32 = src.rc32
  dest.rc33 = src.rc33
  dest.rc34 = src.rc34
  dest.rc41 = src.rc41
  dest.rc42 = src.rc42
  dest.rc43 = src.rc43
  dest.rc44 = src.rc44
  
End Sub

Function RetCopyMatrix(src As D3DMATRIX) As D3DMATRIX
    Dim ret As D3DMATRIX
    CopyMatrix ret, src
    RetCopyMatrix = ret
End Function

'=================================
' IdentityMatrix
'=================================
Sub IdentityMatrix(dest As D3DMATRIX)
  
  dest.rc11 = 1
  dest.rc12 = 0
  dest.rc13 = 0
  dest.rc14 = 0
  dest.rc21 = 0
  dest.rc22 = 1
  dest.rc23 = 0
  dest.rc24 = 0
  dest.rc31 = 0
  dest.rc32 = 0
  dest.rc33 = 1
  dest.rc34 = 0
  dest.rc41 = 0
  dest.rc42 = 0
  dest.rc43 = 0
  dest.rc44 = 1
  
End Sub

Function RetIdentityMatrix() As D3DMATRIX
    Dim ret As D3DMATRIX
    IdentityMatrix ret
End Function

'=================================
' ZeroMatrix
'=================================

Sub ZeroMatrix(dest As D3DMATRIX)
  
  dest.rc11 = 0
  dest.rc12 = 0
  dest.rc13 = 0
  dest.rc14 = 0
  dest.rc21 = 0
  dest.rc22 = 0
  dest.rc23 = 0
  dest.rc24 = 0
  dest.rc31 = 0
  dest.rc32 = 0
  dest.rc33 = 0
  dest.rc34 = 0
  dest.rc41 = 0
  dest.rc42 = 0
  dest.rc43 = 0
  dest.rc44 = 0
  
End Sub

Function RetZeroMatrix() As D3DMATRIX
    Dim ret As D3DMATRIX
    ZeroMatrix ret
    RetZeroMatrix = ret
End Function


'=================================
' VectorNegate
'=================================
Sub VectorNegate(v As D3DVECTOR)
  v.x = -v.x
  v.y = -v.y
  v.z = -v.z
End Sub

Function VNegate(v As D3DVECTOR) As D3DVECTOR
    Dim ret As D3DVECTOR
    ret.x = -v.x
    ret.y = -v.y
    ret.z = -v.z
    VNegate = ret
End Function

'=================================
' VectorSubtract
'=================================
Sub VectorSubtract(dest As D3DVECTOR, a As D3DVECTOR, b As D3DVECTOR)
  dest.x = a.x - b.x
  dest.y = a.y - b.y
  dest.z = a.z - b.z
End Sub

Function VSub(a As D3DVECTOR, b As D3DVECTOR) As D3DVECTOR
  Dim dest As D3DVECTOR
  dest.x = a.x - b.x
  dest.y = a.y - b.y
  dest.z = a.z - b.z
  VSub = dest
End Function

'=================================
' VectorAdd
'=================================
Sub VectorAdd(dest As D3DVECTOR, a As D3DVECTOR, b As D3DVECTOR)
  dest.x = a.x + b.x
  dest.y = a.y + b.y
  dest.z = a.z + b.z
End Sub

Function VAdd(a As D3DVECTOR, b As D3DVECTOR) As D3DVECTOR
  Dim dest As D3DVECTOR
  dest.x = a.x + b.x
  dest.y = a.y + b.y
  dest.z = a.z + b.z
  VAdd = dest
End Function

'=================================
' VectorCrossProduct
'=================================
' can be used to compute normals.
'
Sub VectorCrossProduct(dest As D3DVECTOR, a As D3DVECTOR, b As D3DVECTOR)
   dest.x = a.y * b.z - a.z * b.y
   dest.y = a.z * b.x - a.x * b.z
   dest.z = a.x * b.y - a.y * b.x
End Sub
  
Function VCross(a As D3DVECTOR, b As D3DVECTOR) As D3DVECTOR
   Dim dest As D3DVECTOR
   dest.x = a.y * b.z - a.z * b.y
   dest.y = a.z * b.x - a.x * b.z
   dest.z = a.x * b.y - a.y * b.x
   VCross = dest
End Function
  
'=================================
' VectorNormalize
'=================================
' creates a vector of length 1 in the same direction
'
Sub VectorNormalize(dest As D3DVECTOR)
  On Local Error Resume Next
  Dim l As Double
  l = dest.x * dest.x + dest.y * dest.y + dest.z * dest.z
  l = Sqr(l)
  If l = 0 Then
    dest.x = 0
    dest.y = 0
    dest.z = 0
    Exit Sub
  End If
  dest.x = dest.x / l
  dest.y = dest.y / l
  dest.z = dest.z / l
End Sub
  
Function VNormalize(dest As D3DVECTOR) As D3DVECTOR

  Dim ret As D3DVECTOR
  
  Dim l As Double
  l = dest.x * dest.x + dest.y * dest.y + dest.z * dest.z
  l = Sqr(l)
  If l = 0 Then
    ret.x = 0
    ret.y = 0
    ret.z = 0
  Else
    ret.x = dest.x / l
    ret.y = dest.y / l
    ret.z = dest.z / l
  End If
  VNormalize = ret
End Function
  
'=================================
' VectorDotProduct
'=================================
Function VectorDotProduct(a As D3DVECTOR, b As D3DVECTOR) As Single
  VectorDotProduct = a.x * b.x + a.y * b.y + a.z * b.z
End Function

Function VDot(a As D3DVECTOR, b As D3DVECTOR) As Single
  VDot = a.x * b.x + a.y * b.y + a.z * b.z
End Function

'=================================
' VectorAddAndScale
'=================================
Sub VectorAddAndScale(dest As D3DVECTOR, s1 As Single, v1 As D3DVECTOR, s2 As Single, v2 As D3DVECTOR)
  dest.x = s1 * v1.x + s2 * v2.x
  dest.y = s1 * v1.y + s2 * v2.y
  dest.z = s1 * v1.z + s2 * v2.z
End Sub

'=================================
' VectorCopy
'=================================
Sub VectorCopy(dest As D3DVECTOR, src As D3DVECTOR)
  dest.x = src.x
  dest.y = src.y
  dest.z = src.z
End Sub

Function VCopy(src As D3DVECTOR) As D3DVECTOR
  Dim dest As D3DVECTOR
  dest.x = src.x
  dest.y = src.y
  dest.z = src.z
  VCopy = dest
End Function

'=================================
' VectorScale
'=================================
' scale a vector by a scalar
Sub VectorScale(dest As D3DVECTOR, src As D3DVECTOR, s As Single)
  dest.x = src.x * s
  dest.y = src.y * s
  dest.z = src.z * s
End Sub

Function VScale(src As D3DVECTOR, s As Single) As D3DVECTOR
  Dim dest As D3DVECTOR
  dest.x = src.x * s
  dest.y = src.y * s
  dest.z = src.z * s
  VScale = dest
End Function

'=================================
' MakeVector
'=================================
Sub MakeVector(v As D3DVECTOR, x As Single, y As Single, z As Single)
    v.x = x
    v.y = z
    v.z = y
End Sub

Function RVector(x As Single, y As Single, z As Single) As D3DVECTOR
    Dim v As D3DVECTOR
    v.x = x
    v.y = y
    v.z = z
    RVector = v
End Function

'=================================
' MakeVertex
'=================================
Sub MakeVertex(ret As D3DVERTEX, Vect As D3DVECTOR, vNorm As D3DVECTOR, tu As Single, tv As Single)
    
    With ret
        .nx = vNorm.x
        .ny = vNorm.y
        .nz = vNorm.z
        .tu = tu
        .tv = tv
        .x = Vect.x
        .y = Vect.y
        .z = Vect.z
    End With

End Sub

Function RVertex(Vect As D3DVECTOR, vNorm As D3DVECTOR, tu As Single, tv As Single) As D3DVERTEX
    Dim ret As D3DVERTEX
    With ret
        .nx = vNorm.x
        .ny = vNorm.y
        .nz = vNorm.z
        .tu = tu
        .tv = tv
        .x = Vect.x
        .y = Vect.y
        .z = Vect.z
    End With
    RVertex = ret
End Function


'=================================
' MakeLVertex
'=================================
Sub MakeLVertex(ret As D3DLVERTEX, x As Single, y As Single, z As Single, color As Long, specular As Single, tu As Single, tv As Single)
    
    With ret
        .specular = specular
        .tu = tu
        .tv = tv
        .x = x
        .y = y
        .z = z
        .color = color
    End With
    
End Sub

Function RLVertex(x As Single, y As Single, z As Single, color As Long, specular As Single, tu As Single, tv As Single) As D3DLVERTEX
    Dim ret As D3DLVERTEX
    With ret
        .specular = specular
        .tu = tu
        .tv = tv
        .x = x
        .y = y
        .z = z
        .color = color
    End With
    RLVertex = ret
End Function

'=================================
' MakeTLVertex
'=================================
Function MakeTLVertex(vert As D3DTLVERTEX, sx As Single, sy As Single, sz As Single, w As Single, c As Long, s As Single, u As Single, v As Single) As D3DTLVERTEX
    
    vert.sx = sx
    vert.sy = sy
    vert.sz = sz
    vert.rhw = w
    vert.color = c
    vert.specular = s
    vert.tu = u
    vert.tv = v
    
End Function

Function RTLVertex(sx As Single, sy As Single, sz As Single, w As Single, c As Long, s As Single, u As Single, v As Single) As D3DTLVERTEX
    Dim vert As D3DTLVERTEX
    vert.sx = sx
    vert.sy = sy
    vert.sz = sz
    vert.rhw = w
    vert.color = c
    vert.specular = s
    vert.tu = u
    vert.tv = v
    RTLVertex = vert
End Function



'=================================
' MakeRect
'=================================
Function MakeRect(ret As RECT, X1 As Single, Y1 As Single, X2 As Single, Y2 As Single)
    With ret
        .Left = X1
        .Top = Y1
        .right = X2
        .Bottom = Y2
    End With
End Function

Function RRect(X1 As Single, Y1 As Single, X2 As Single, Y2 As Single) As RECT
    Dim RetRect As RECT
    With RetRect
        .Left = X1
        .Top = Y1
        .right = X2
        .Bottom = Y2
    End With
    RRect = RetRect
End Function

'=================================
' ResetFloat
' easy way of reseting the floating
' point cpu flags so vb doesnt complian
' of Overflow error.
' Issues are always driver specific
'=================================
Sub ResetFloat()
  On Local Error GoTo out
  Dim s As Single
  Dim v As Single
  s = 1#
  s = s / v

out:
  s = 0

End Sub

'=================================
' PrintVector
' aids in debuging
'=================================

Sub PrintVector(v As D3DVECTOR)
  Debug.Print v.x, v.y, v.z
End Sub

'=================================
' FVF VERTEX  helpers
' used for mutli textured polygons
'=================================

'Helper function for
Private Function RaisePower(ByVal lPower As Long) As Long
  Dim lCount As Long, lRaised As Long
  lRaised = 1
  For lCount = 1 To lPower
      lRaised = lRaised * 2
  Next
  RaisePower = lRaised
End Function
Private Function ShiftLeft(ByVal lInitNum As Long, ByVal lBitsLeft As Long) As Long

  'Shift Left is computed as floor( this * (2**BitsLeft))
  Dim lPower As Long
  lPower = RaisePower(lBitsLeft)
  ShiftLeft = CLng(lInitNum * lPower)
End Function

Private Function D3DFVF_TEXCOORDSIZE1(ByVal CoordIndex As Long) As Long
  D3DFVF_TEXCOORDSIZE1 = ShiftLeft(D3DFVF_TEXTUREFORMAT1, (CoordIndex * 2 + 16))
End Function

Private Function D3DFVF_TEXCOORDSIZE2(ByVal CoordIndex As Long) As Long
  D3DFVF_TEXCOORDSIZE2 = D3DFVF_TEXTUREFORMAT2
End Function

Private Function D3DFVF_TEXCOORDSIZE3(ByVal CoordIndex As Long) As Long
  D3DFVF_TEXCOORDSIZE3 = ShiftLeft(D3DFVF_TEXTUREFORMAT3, (CoordIndex * 2 + 16))
End Function

Private Function D3DFVF_TEXCOORDSIZE4(ByVal CoordIndex As Long) As Long
  D3DFVF_TEXCOORDSIZE4 = ShiftLeft(D3DFVF_TEXTUREFORMAT4, (CoordIndex * 2 + 16))
End Function



'=================================
'  FlagsToBitDepth
'=================================
'  Purpose:    Gets Bit Depth from DDPF Flags
Function FlagsToBitDepth(dwFlags As Long) As Long

  If (dwFlags & DDBD_1) Then
      FlagsToBitDepth = 1
  ElseIf (dwFlags And DDBD_2) Then
      FlagsToBitDepth = 2
  ElseIf (dwFlags And DDBD_4) Then
      FlagsToBitDepth = 4
  ElseIf (dwFlags And DDBD_8) Then
      FlagsToBitDepth = 8
  ElseIf (dwFlags And DDBD_16) Then
      FlagsToBitDepth = 16
  ElseIf (dwFlags And DDBD_24) Then
      FlagsToBitDepth = 24
  ElseIf (dwFlags And DDBD_32) Then
      FlagsToBitDepth = 32
  Else
      FlagsToBitDepth = 0
  End If
End Function




'=================================
'  BitDepthToFlags
'=================================
'  Converts BPP to corresponding DDPF flag
'
Function BitDepthToFlags(dwBPP As Long) As Long

  Select Case dwBPP
  Case 1:
      BitDepthToFlags = DDBD_1
  Case 2:
      BitDepthToFlags = DDBD_2
  Case 4:
      BitDepthToFlags = DDBD_4
  Case 8:
      BitDepthToFlags = DDBD_8
  Case 16:
      BitDepthToFlags = DDBD_16
  Case 24:
      BitDepthToFlags = DDBD_24
  Case 32:
      BitDepthToFlags = DDBD_32
  Case Else
      BitDepthToFlags = 0
  End Select
  
End Function



