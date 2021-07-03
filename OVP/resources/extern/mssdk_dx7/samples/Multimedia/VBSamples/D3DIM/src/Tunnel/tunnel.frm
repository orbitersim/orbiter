VERSION 5.00
Object = "{D0B8DDCE-E796-11D2-A21E-00C04F68AD33}#1.1#0"; "IMControl.ocx"
Begin VB.Form TunnelForm 
   Caption         =   "VB D3DIM Tunnel"
   ClientHeight    =   3195
   ClientLeft      =   60
   ClientTop       =   345
   ClientWidth     =   4680
   Icon            =   "tunnel.frx":0000
   LinkTopic       =   "Form1"
   ScaleHeight     =   213
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   312
   StartUpPosition =   3  'Windows Default
   Begin DirectXIMControl.IMCanvas IMCanvas1 
      Height          =   3255
      Left            =   0
      TabIndex        =   0
      Top             =   0
      Width           =   4695
      _ExtentX        =   8281
      _ExtentY        =   5741
   End
End
Attribute VB_Name = "TunnelForm"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Const NUM_SEGMENTS = 20                                 ' # of segments in memory at one time.  Each
                                                        ' segment is made up of triangles spanning
                                                        ' between two rings.
Const NUM_SIDES = 24                                    ' # of sides on each ring.
Const NUM_TEX_RINGS = 5                                 ' # of rings to stretch the texture over.
Const NUM_VERTICES = (NUM_SIDES * (NUM_SEGMENTS + 1))   ' # of vertices in memory
Const NUM_TRIANGLES = (NUM_SIDES * NUM_SEGMENTS * 2)    ' # of triangles in memory
Const NUM_SPLINE_POINTS = 50                            ' Number of spline points to initially
                                                        ' calculate.  The section in memory represents
                                                        ' only a fraction of this.
Const TUNNEL_RADIUS = 1#                                ' Radius of the tunnel.

                                                        ' Movement and track scalars given in terms of position along the spline
                                                        ' curve.
Const SEGMENT_LENGTH = 0.05                             ' Length of each segment along curve.
Const SPEED = 0.008                                      ' Amount to increment camera position along
                                                        ' curve for each frame.
Const DEPTH = 0.8                                       ' How close the camera can get to the end of
                                                        ' track before new segments are added.
Const PATH_LENGTH = (NUM_SPLINE_POINTS - 1)             'Total length of the tunnel.

Const m_bSpecularEnabled = False
Const m_bPerspectiveCorrect = False
Const m_dwShading = D3DSHADE_GOURAUD
Const m_dwAntialias = D3DANTIALIAS_NONE
Const m_bFogEnabled = False
Const m_bDitherEnabled = False

Dim m_dx As DirectX7

Dim m_Vertices() As D3DVERTEX
Dim m_Indices() As Integer
Dim m_Points() As D3DVECTOR
Dim m_vEndN As D3DVECTOR
Dim m_vEndP As D3DVECTOR
Dim m_vEndD As D3DVECTOR

Dim m_fEndPos As Single
Dim m_fCameraPos As Single


Dim m_light As D3DLIGHT7
Dim m_mtrl As D3DMATERIAL7
Dim m_matView As D3DMATRIX
Dim m_matWorld As D3DMATRIX
Dim m_matProj As D3DMATRIX

Dim m_vCameraP As D3DVECTOR
Dim m_vCameraD As D3DVECTOR
Dim m_vCameraN As D3DVECTOR
Dim m_Tex As DirectDrawSurface7

Dim m_iCurrentRing
Dim m_iCurrentSegment

Dim m_running As Boolean
Dim m_binit As Boolean


Const m_pi = 3.14159265358972

Private Sub Form_Load()

    If m_binit Then End
    m_binit = True
    
        
    
    Me.Show
    
    Set m_dx = IMCanvas1.dx
    
    IMCanvas1.StartWindowed
    If IMCanvas1.dx.SystemBpp < 16 Then
        MsgBox "This sample is designed to run in 16bpp or better color"
        End
    End If
    IMCanvas1.backSurface.SetForeColor vbGreen
    
    InitPoints
    InitDevice
    
    m_running = True
    Dim fRestore As Boolean
    
    Do While m_running
	on local error resume next
        DoEvents
        fRestore = False
        While IMCanvas1.DirectDraw.TestCooperativeLevel <> DD_OK
            fRestore = True
            DoEvents
        Wend
        If fRestore Then
            IMCanvas1.DirectDraw.RestoreAllSurfaces
            InitDevice
        End If
        MoveScene
        Render
    Loop
    
    
    End
End Sub




Private Sub Form_Resize()
    IMCanvas1.Width = Me.ScaleWidth
    IMCanvas1.Height = Me.ScaleHeight
    Cleanup
    IMCanvas1.StartWindowed
    InitDevice
    If IMCanvas1.backSurface Is Nothing Then Exit Sub
    IMCanvas1.backSurface.SetForeColor vbGreen
    
End Sub


        


Sub MoveScene()

    Dim dev As Direct3DDevice7
    Dim fFloatKey As Single
    
    Set dev = IMCanvas1.Direct3DDevice
        
    ' Move the camera through the tunnel.  Create new segments of the tunnel
    ' when the camera gets close to the end of the section in memory.
    m_fCameraPos = m_fCameraPos + SPEED
    
    If (m_fCameraPos > PATH_LENGTH) Then
        m_fCameraPos = m_fCameraPos - PATH_LENGTH
    End If
        
    Call MoveToPosition(m_fCameraPos, m_vCameraP, m_vCameraD, m_vCameraN)

    ' If the camera is close to the end, add a new segment.
    If ((m_fEndPos - m_fCameraPos) < DEPTH) Then
    
        m_fEndPos = m_fEndPos + SEGMENT_LENGTH
        If (m_fEndPos > PATH_LENGTH) Then m_fEndPos = m_fEndPos - PATH_LENGTH
        Call UpdateTubeInMemory
    End If

    ' Move the camera and the light
    Call PositionCamera(m_matView, m_vCameraP, m_vCameraD, m_vCameraN)
    
    Call m_dx.VectorCopy(m_light.position, m_vCameraP)
    
End Sub




'-----------------------------------------------------------------------------
' Name: App_Render()
' Desc: Called once per frame, the call is the entry point for 3d
'       rendering. This function sets up render states, clears the
'       viewport, and renders the scene.
'-----------------------------------------------------------------------------
Sub Render()
    
    Dim dev As Direct3DDevice7
    Dim fFloatKey As Single
    
    Set dev = IMCanvas1.Direct3DDevice
    
    
    ' No need to clear the viewport since we redraw over every pixel
    
    IMCanvas1.ClearBackSurface
    
    
    dev.BeginScene
    
    ' Update the camera and the light position
    Call dev.SetTransform(D3DTRANSFORMSTATE_VIEW, m_matView)
    dev.SetLight 0, m_light
    dev.LightEnable 0, True

    Call dev.SetTextureStageState(0, D3DTSS_MINFILTER, D3DTFN_LINEAR)
    Call dev.SetTextureStageState(0, D3DTSS_MAGFILTER, D3DTFG_LINEAR)
        
    Call dev.SetRenderState(D3DRENDERSTATE_SPECULARENABLE, m_bSpecularEnabled)
    Call dev.SetRenderState(D3DRENDERSTATE_SHADEMODE, m_dwShading)
    Call dev.SetRenderState(D3DRENDERSTATE_FOGENABLE, m_bFogEnabled)
    Call dev.SetRenderState(D3DRENDERSTATE_DITHERENABLE, m_bDitherEnabled)
    Call dev.SetMaterial(m_mtrl)
    
    
    Call dev.DrawIndexedPrimitive(D3DPT_TRIANGLELIST, D3DFVF_VERTEX, _
            m_Vertices(0), NUM_VERTICES, m_Indices, 3 * NUM_TRIANGLES, 0)
    Call dev.EndScene
                
    
    
    IMCanvas1.Update
End Sub


Sub InitPoints()

    Dim fPosition As Single
    Dim i As Integer
    
    ReDim m_Vertices(NUM_VERTICES)
    ReDim m_Indices(3 * NUM_TRIANGLES)
    ReDim m_Points(NUM_SPLINE_POINTS)
    
    
    fPosition = 0
    

    ' Generate spline points
    ' m_points represents an array of points at the center of the tunnel
    For i = 0 To NUM_SPLINE_POINTS - 1
        m_Points(i).x = (Cos(i * 4#) * 20#)
        m_Points(i).y = (Sin(i * 4#) * 20#)
        m_Points(i).z = i * 20#
    Next
    

   
    'Create the initial tube section in memory.
    m_vEndN.x = 0
    m_vEndN.y = 1
    m_vEndN.z = 0
    
    
    
    For i = 0 To NUM_SEGMENTS
        Call MoveToPosition(fPosition, m_vEndP, m_vEndD, m_vEndN)
        fPosition = fPosition + SEGMENT_LENGTH
        Call MakeRing(m_vEndP, m_vEndD, m_vEndN, _
             (i Mod NUM_TEX_RINGS) / NUM_TEX_RINGS, _
             (NUM_SEGMENTS - i) * NUM_SIDES)
    Next
    
    For i = 0 To NUM_SEGMENTS - 1
        Call MakeSegment(i + 1, i, 3 * i * NUM_SIDES * 2)
    Next
    
    '' Move the camera to the begining and set some globals
    m_vCameraN.x = 0
    m_vCameraN.y = 1
    m_vCameraN.z = 0
    
    Call MoveToPosition(0#, m_vCameraP, m_vCameraD, m_vCameraN)
    m_iCurrentRing = 0
    m_iCurrentSegment = 0
    m_fCameraPos = 0
    m_fEndPos = fPosition
    
    FindMediaDir "tex2.bmp"
    
    

    
End Sub

    
                
                
                 
                




'-----------------------------------------------------------------------------
' Name: BuildInvertedRotationMatrix()
' Desc: Set the rotation part of a matrix such that the vector pD is the new
'       z-axis and pU is the new y-axis.
'-----------------------------------------------------------------------------
Private Sub BuildInvertedRotationMatrix(pM As D3DMATRIX, vd As D3DVECTOR, vU As D3DVECTOR)



    Dim d As D3DVECTOR
    Dim u As D3DVECTOR
    Dim r As D3DVECTOR
    
    Dim temp As D3DVECTOR
    
    Call m_dx.VectorCopy(d, vd)
    Call m_dx.VectorCopy(u, vU)
    Call m_dx.VectorNormalize(d)
    
    '' Project u into the plane defined by d and normalise.
    'u = Normalize( u - d * DotProduct( u, d ) );
    Call m_dx.VectorScale(temp, d, m_dx.VectorDotProduct(u, d))
    Call m_dx.VectorSubtract(u, u, temp)
    Call m_dx.VectorNormalize(u)

    ' Calculate the vector pointing along the matrix x axis (in a right
    ' handed coordinate system) using cross product.
    Call m_dx.VectorCrossProduct(r, u, d)

    ' Build the matrix INVERTED (all elements are reversed)
    pM.rc11 = r.x: pM.rc21 = r.y: pM.rc31 = r.z
    pM.rc12 = u.x: pM.rc22 = u.y: pM.rc32 = u.z
    pM.rc13 = d.x: pM.rc23 = d.y: pM.rc33 = d.z
    pM.rc14 = 0: pM.rc24 = 0: pM.rc34 = 0: pM.rc44 = 1
    
End Sub
    



'-----------------------------------------------------------------------------
' Name: PositionCamera()
' Desc: Creates a matrix which is equivalent to having the camera at a
'       specified position. This matrix can be used to convert vertices to
'       camera coordinates. Input parameters are the position of the camera,
'       the direction of the view, and the up vector.
'-----------------------------------------------------------------------------
Private Sub PositionCamera(pM As D3DMATRIX, vp As D3DVECTOR, vd As D3DVECTOR, vN As D3DVECTOR)

    ' Set the rotation part of the matrix and invert it. Vertices must be
    ' inverse rotated to achieve the same result of a corresponding
    ' camera rotation.
    Call BuildInvertedRotationMatrix(pM, vd, vN)

    ' Multiply the rotation matrix by a translation transform.  The
    ' translation matrix must be applied first (left of rotation).
    pM.rc41 = -(pM.rc11 * vp.x + pM.rc21 * vp.y + pM.rc31 * vp.z)
    pM.rc42 = -(pM.rc12 * vp.x + pM.rc22 * vp.y + pM.rc32 * vp.z)
    pM.rc43 = -(pM.rc13 * vp.x + pM.rc23 * vp.y + pM.rc33 * vp.z)
    
End Sub

  

'-----------------------------------------------------------------------------
' Name: spline()
' Desc: Calculates a point along a B-Spline curve defined by four points. The
'       parameter, t, is the parametric value along the spline function.
'-----------------------------------------------------------------------------
Private Sub spline(ret As D3DVECTOR, t As Single, vSplinePoint() As D3DVECTOR)
    Dim t2 As Single
    Dim t3 As Single
    Dim m(4) As Single
    Dim temp As D3DVECTOR
    Dim i As Integer
    
    ret.x = 0
    ret.y = 0
    ret.z = 0
    
    
    t2 = t * t
    t3 = t * t * t
    
    
    m(0) = (0.5 * ((-1# * t3) + (2# * t2) + (-1# * t)))
    m(1) = (0.5 * ((3# * t3) + (-5# * t2) + (0# * t) + 2#))
    m(2) = (0.5 * ((-3# * t3) + (4# * t2) + (1# * t)))
    m(3) = (0.5 * ((1# * t3) + (-1# * t2) + (0# * t)))

    For i = 0 To 3
        m_dx.VectorScale temp, vSplinePoint(i), m(i)
        m_dx.VectorAdd ret, ret, temp
    Next
    
End Sub





'-----------------------------------------------------------------------------
' Name: MoveToPosition()
' Desc: Updates the given position, direction and normal vectors to a given
'       position on the spline curve. The given up vector is used to
'       determine the new up vector.
'-----------------------------------------------------------------------------
Private Sub MoveToPosition(fPosition As Single, vp As D3DVECTOR, vd As D3DVECTOR, vN As D3DVECTOR)
    
    Dim vSplinePoint(4) As D3DVECTOR
    Dim point As Integer
    Dim fPos As Single
    Dim vTemp1 As D3DVECTOR
    Dim vTemp2 As D3DVECTOR
    Dim j As Integer
    
    fPos = fPosition
    
    
    ' Loop from back for get number id of point at the camera (where pos==1.0f)
    Do While fPos > 1#
        point = point + 1
        If (point = NUM_SPLINE_POINTS) Then point = 0
        fPos = fPos - 1
    Loop
        
    ' Find the four points along the curve which are around the position.
    For j = 0 To 3
        vSplinePoint(j) = m_Points(point)
        point = point + 1
        If point = NUM_SPLINE_POINTS Then point = 0
    Next
        

    ' Calculate the direction from the given position, and the position
    ' just before it
    ' P=spline(fPos,vSplinePoint())
    ' D=Normalize(P - spline(fPos-.01,vSplinePoint()))
    'fpos as number between 0 and 4 here
    'to indicate where in the 4 points we are
    Call spline(vp, fPos, vSplinePoint())
    Call spline(vTemp1, fPos - 0.01, vSplinePoint())
    'Call m_dx.VEectorNegate(vTemp1)
    vTemp1.x = -vTemp1.x
    vTemp1.y = -vTemp1.y
    vTemp1.z = -vTemp1.z
    
    Call m_dx.VectorAdd(vd, vp, vTemp1)
    Call m_dx.VectorNormalize(vd)
    
    
    ' Find the new normal.  This method will work provided the change in
    ' the normal is not very large.
    ' N=Normalize(D x (N x D))
    Call m_dx.VectorCrossProduct(vTemp1, vN, vd)
    Call m_dx.VectorCrossProduct(vTemp2, vd, vTemp1)
    Call m_dx.VectorCopy(vN, vTemp2)
    Call m_dx.VectorNormalize(vN)
    
    
    
End Sub




'-----------------------------------------------------------------------------
' Name: MakeRing()
' Desc: Generates a ring of vertices in a plane defined by vN and the cross
'       product of vN and vP.  On exit, pvJoint contains the vertices. Normals
'       are generated pointing in.
'-----------------------------------------------------------------------------
Private Sub MakeRing(vp As D3DVECTOR, vd As D3DVECTOR, vN As D3DVECTOR, fTV As Single, JointStartIndex As Integer)


    Dim vNxD As D3DVECTOR
    Dim iSpoke As Integer
    Dim fTheta As Double
    Dim vPt As D3DVECTOR
    Dim i As Integer
    Dim vTemp1 As D3DVECTOR
    Dim vTemp2 As D3DVECTOR
    
    'NxD= N x D
    Call m_dx.VectorCrossProduct(vNxD, vN, vd)
    
    For iSpoke = 0 To NUM_SIDES - 1
        fTheta = (2 * m_pi) * iSpoke / NUM_SIDES
    
    
        ' x, y, z define a unit vector in standard coordiante space
        'vPt = ((vN x vD) * cos( fTheta )+(vN  * sin( fTheta ))
        Call m_dx.VectorScale(vTemp1, vNxD, Cos(fTheta))
        Call m_dx.VectorScale(vTemp2, vN, Sin(fTheta))
        Call m_dx.VectorAdd(vPt, vTemp1, vTemp2)
                        
        i = JointStartIndex + iSpoke
        
        ' Position, normals and texture coordiantes.
        With m_Vertices(i)
            .x = vp.x + vPt.x * TUNNEL_RADIUS
            .y = vp.y + vPt.y * TUNNEL_RADIUS
            .z = vp.z + vPt.z * TUNNEL_RADIUS
            .nx = -vPt.x
            .ny = -vPt.y
            .nz = -vPt.z
            .tu = 1# - fTheta / (2# * m_pi)
            .tv = fTV
        End With
    Next
End Sub



'-----------------------------------------------------------------------------
' Name: MakeSegment()
' Desc: Defines the triangles (or indices for triangles) which form a segment
'       between the two rings of the tunnel.
'-----------------------------------------------------------------------------
Sub MakeSegment(iRing1 As Integer, iRing2 As Integer, iStartIndex As Integer)
    Dim iSide As Integer
    Dim iTri As Integer
    Dim i As Integer

    iTri = 0
    For iSide = 0 To NUM_SIDES - 1
                    
        ' Each side consists of two triangles.
        i = iStartIndex + 3 * iTri
        m_Indices(i) = iRing1 * NUM_SIDES + iSide
        m_Indices(i + 1) = iRing2 * NUM_SIDES + iSide
        m_Indices(i + 2) = iRing2 * NUM_SIDES + ((iSide + 1) Mod NUM_SIDES)
        iTri = iTri + 1
        
        i = iStartIndex + 3 * iTri
        
        
        m_Indices(i + 1) = iRing2 * NUM_SIDES + ((iSide + 1) Mod NUM_SIDES)
        m_Indices(i + 2) = iRing1 * NUM_SIDES + ((iSide + 1) Mod NUM_SIDES)
        m_Indices(i + 0) = iRing1 * NUM_SIDES + iSide
        iTri = iTri + 1
    Next
End Sub


Sub InitDevice()
    On Local Error GoTo errOut
    Dim dev As Direct3DDevice7
    Dim d3d As Direct3D7
    
    Set dev = IMCanvas1.Direct3DDevice
    Set d3d = IMCanvas1.Direct3d
    Set m_Tex = IMCanvas1.CreateTextureSurface("tex2.bmp", 0, 0, 0)
    
    m_dx.IdentityMatrix m_matWorld
    m_dx.IdentityMatrix m_matView
    m_dx.ProjectionMatrix m_matProj, 1, 1000, m_pi / 4
    
    

    Call dev.SetTransform(D3DTRANSFORMSTATE_WORLD, m_matWorld)
    Call dev.SetTransform(D3DTRANSFORMSTATE_VIEW, m_matView)
    Call dev.SetTransform(D3DTRANSFORMSTATE_PROJECTION, m_matProj)

    dev.SetTexture 0, m_Tex
    
    
    Call dev.SetTextureStageState(0, D3DTSS_COLORARG1, D3DTA_TEXTURE)
    Call dev.SetTextureStageState(0, D3DTSS_COLORARG2, D3DTA_DIFFUSE)
    Call dev.SetTextureStageState(0, D3DTSS_COLOROP, D3DTOP_MODULATE)

    
   
   m_light.dltType = D3DLIGHT_POINT
   m_light.attenuation2 = 0.01
   m_light.falloff = 1000
   m_light.diffuse.r = 1
   m_light.diffuse.g = 1
   m_light.diffuse.b = 1
   
   m_dx.VectorCopy m_light.position, m_vCameraP
   
   
   
    
   
    m_mtrl.Ambient.r = 0.7
    m_mtrl.Ambient.g = 0.7
    m_mtrl.Ambient.b = 0.7
    m_mtrl.diffuse.r = 1
    m_mtrl.diffuse.g = 1
    m_mtrl.diffuse.b = 1
    
    Call dev.SetRenderState(D3DRENDERSTATE_LIGHTING, True)
    Call dev.SetRenderState(D3DRENDERSTATE_AMBIENT, &HFFFFFFFF)
    Call dev.SetRenderState(D3DRENDERSTATE_WRAP0, True)
    Call dev.SetRenderState(D3DRENDERSTATE_WRAP1, True)
    

  
errOut:

End Sub


'-----------------------------------------------------------------------------
' Name: UpdateTubeInMemory()
' Desc: Creates a new segment of the tunnel at the current end position.
'-----------------------------------------------------------------------------
Sub UpdateTubeInMemory()


    Static iTexRing As Integer        ' Counter defining the position of
                                      ' this ring on the texture.
    Dim iEndRing As Integer           ' Ring at the end of the tunnel in mem
    Dim iRingOffset As Integer        ' Offsets into the vertex and triangle
    Dim iSegmentOffset As Integer     ' lists for the new data.
    Dim v As D3DVERTEX

    ' Replace the back ring with a new ring at the front of the tube
    ' in memory.
    MoveMemory2
    
    'MoveMemory m_Vertices(0), m_Vertices(NUM_SIDES), Len(v) * (NUM_VERTICES - NUM_SIDES)
    Call MakeRing(m_vEndP, m_vEndD, m_vEndN, _
              iTexRing / NUM_TEX_RINGS, 0)
              
              

    'Replace the back segment with a new segment at the front of the
    'tube in memory. Update the current end position of the tube in
    'memory.
    iEndRing = (m_iCurrentRing + NUM_SEGMENTS) Mod (NUM_SEGMENTS + 1)
    Call MoveToPosition(m_fEndPos, m_vEndP, m_vEndD, m_vEndN)

    ' Update the  buffer with the new vertices and triangles.
    'iRingOffset = Len(D3DVERTEX) * g_Tunnel.wCurrentRing * NUM_SIDES
    'iSegmentOffset = Len(D3DTRIANGLE) * g_Tunnel.wCurrentSegment * NUM_SIDES * 2

    ' Update the position of the back of the tube in memory and texture
    ' counter.
    m_iCurrentRing = (m_iCurrentRing + 1) Mod (NUM_SEGMENTS + 1)
    m_iCurrentSegment = (m_iCurrentSegment + 1) Mod NUM_SEGMENTS
    iTexRing = (iTexRing + 1) Mod NUM_TEX_RINGS

End Sub



Sub MoveMemory2()
    
    'CONSIDER CALLIND MOVEMEMORY VIA DLL DECLARE
    
    Dim i As Integer
    For i = NUM_VERTICES - 1 To NUM_SIDES Step -1
        With m_Vertices(i)
            .x = m_Vertices(i - NUM_SIDES).x
            .y = m_Vertices(i - NUM_SIDES).y
            .z = m_Vertices(i - NUM_SIDES).z
            .nx = m_Vertices(i - NUM_SIDES).nx
            .ny = m_Vertices(i - NUM_SIDES).ny
            .nz = m_Vertices(i - NUM_SIDES).nz
            .tu = m_Vertices(i - NUM_SIDES).tu
            .tv = m_Vertices(i - NUM_SIDES).tv
         End With
    Next

End Sub



Sub FindMediaDir(sLoadFile As String)
    On Local Error Resume Next

    If Dir$(sLoadFile) = "" Then
        If Mid$(App.Path, 2, 1) = ":" Then
            ChDrive Mid$(App.Path, 1, 1)
        End If
        ChDir App.Path
        ChDir "..\media"
    End If
    If Dir$(sLoadFile) = "" Then
        ChDir App.Path
        ChDir "..\..\media"
    End If
    Err.Number = 0
End Sub

Private Sub Form_Unload(Cancel As Integer)
    m_running = False
End Sub


Private Sub Cleanup()
    'all references to d3d objects must be gone for
    'the parent device to go away and be created at
    'a new height and width
    Set m_Tex = Nothing
End Sub

Private Sub IMCanvas1_NewDDraw()
        InitDevice
        If IMCanvas1.backSurface Is Nothing Then Exit Sub
        IMCanvas1.backSurface.SetForeColor vbGreen
End Sub


