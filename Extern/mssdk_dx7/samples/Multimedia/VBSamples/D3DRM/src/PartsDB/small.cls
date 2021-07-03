VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
END
Attribute VB_Name = "RMWindow"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Option Explicit

Dim m_dx As DirectX7
Dim m_dd As DirectDraw7
Dim m_ddClipper As DirectDrawClipper
Dim m_rm As Direct3DRM3

Public m_rmDevice As Direct3DRMDevice3
Public m_rmViewport As Direct3DRMViewport2

Public m_rootFrame As Direct3DRMFrame3
Public m_lightFrame As Direct3DRMFrame3
Public m_cameraFrame As Direct3DRMFrame3
Public m_objectFrame As Direct3DRMFrame3
Public m_light As Direct3DRMLight
Public m_meshBuilder As Direct3DRMMeshBuilder3
Public m_object As Direct3DRMMeshBuilder3

Dim m_width As Long
Dim m_height As Long
Dim m_running As Boolean
Dim m_finished As Boolean
Dim m_picture As PictureBox
Dim m_fallbackGuid As String

Sub init(dx As DirectX7, dd As DirectDraw7, rm As Direct3DRM3, pict As PictureBox, ByVal sGuid As String)
    on local error resume next

    Set m_dd = dd
    Set m_rm = rm
    Set m_dx = dx
    Set m_picture = pict

    
    'Create new clipper object and associate it with a window'
    Set m_ddClipper = m_dd.CreateClipper(0)
    m_ddClipper.SetHWnd m_picture.hWnd
            
    
    'save the widht and height of the picture in pixels
    m_width = m_picture.ScaleWidth
    m_height = m_picture.ScaleHeight


    'choose hardware 3d if "" is passed in    
    if sGuid="" then sguid ="IID_IDirect3DHALDevice"

    'Try hardware device..
    Set m_rmDevice = m_rm.CreateDeviceFromClipper(m_ddClipper, sGuid, m_width, m_height)
    
    'If that fails have RM select a device for us
    if err.number <> 0 then
	err.clear
	Set m_rmDevice = m_rm.CreateDeviceFromClipper(m_ddClipper,"", m_width, m_height)
    end if    

    'Setup a scene graph with a camera light and object
    Set m_rootFrame = m_rm.CreateFrame(Nothing)
    Set m_cameraFrame = m_rm.CreateFrame(m_rootFrame)
    Set m_lightFrame = m_rm.CreateFrame(m_rootFrame)
    Set m_objectFrame = m_rm.CreateFrame(m_rootFrame)
    
    'position the camera and create the Viewport
    'provide the device thre viewport uses to render, the frame whose orientation and position
    'is used to determine the camera, and a rectangle describing the extents of the viewport
    m_cameraFrame.SetPosition Nothing, 0, 0, 3
    Set m_rmViewport = m_rm.CreateViewport(m_rmDevice, m_cameraFrame, 0, 0, m_width, m_height)
    
    
    'create a white light and hang it off the light frame
    Set m_light = m_rm.CreateLight(D3DRMLIGHT_DIRECTIONAL, &HFFFFFFFF)
    m_lightFrame.AddLight m_light
    m_lightFrame.SetPosition Nothing, -5, -10, -20
    m_lightFrame.LookAt m_objectFrame, Nothing, 0

End Sub

Sub Render()
    
    m_rmDevice.HandleActivate 0
    m_rmDevice.HandlePaint m_picture.hDC
    m_objectFrame.SetPosition Nothing, 0, 0, 10
    m_rmViewport.Clear 7  'clear the rendering surface rectangle described by the viewport
    m_rmViewport.Render m_rootFrame 'render to the device
    FixFloat
    m_rmDevice.Update   'blt the image to the screen
End Sub

Sub FixFloat()
    On Local Error Resume Next
    Dim l As Single
    l = 6
    
    l = l / 0
End Sub

Function Pick(x As Long, y As Long) As Direct3DRMFrame3


    Dim pickarray As Direct3DRMPickArray
    Dim mb As Direct3DRMMeshBuilder3
    Dim pickdesc As D3DRMPICKDESC, q As Integer
    Dim r As Integer, c As Integer
    Dim f As Direct3DRMFrame3
    Dim p As Direct3DRMFrame3
    Dim fa As Direct3DRMFrameArray
    Dim strName As String
    
    'Get the frame under the the mouse
    Set pickarray = m_rmViewport.Pick(CLng(x), CLng(y))
    If pickarray.GetSize() = 0 Then Exit Function
    Set fa = pickarray.GetPickFrame(0, pickdesc)
    Set f = fa.GetElement(fa.GetSize() - 1)
    
    
    
    Set Pick = f
End Function


