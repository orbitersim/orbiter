VERSION 5.00
Begin VB.Form Dialog 
   BorderStyle     =   1  'Fixed Single
   Caption         =   "Pick a Direct3D Device"
   ClientHeight    =   1650
   ClientLeft      =   885
   ClientTop       =   1485
   ClientWidth     =   3255
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   110
   ScaleMode       =   0  'User
   ScaleWidth      =   222.25
   Begin VB.ComboBox cmbDevice 
      Height          =   315
      ItemData        =   "Dialog.frx":0000
      Left            =   840
      List            =   "Dialog.frx":0002
      Style           =   2  'Dropdown List
      TabIndex        =   2
      Top             =   120
      Width           =   2295
   End
   Begin VB.ComboBox cmbMode 
      Height          =   315
      Left            =   840
      Style           =   2  'Dropdown List
      TabIndex        =   1
      Top             =   720
      Width           =   2295
   End
   Begin VB.CommandButton cmdOK 
      Caption         =   "OK"
      Height          =   330
      Left            =   2040
      TabIndex        =   0
      Top             =   1200
      Width           =   1095
   End
   Begin VB.Label Label2 
      Caption         =   "&Mode"
      Height          =   255
      Left            =   120
      TabIndex        =   4
      Top             =   720
      Width           =   855
   End
   Begin VB.Label Label1 
      Caption         =   "&Device"
      Height          =   255
      Left            =   120
      TabIndex        =   3
      Top             =   120
      Width           =   855
   End
End
Attribute VB_Name = "Dialog"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False

Option Explicit

Dim g_dx As New DirectX7

Dim g_ddEnum As DirectDrawEnum
Dim g_d3dEnumDevices As Direct3DEnumDevices
Dim g_ddEnumModes As DirectDrawEnumModes

Dim g_d3d As Direct3D7
Dim g_dd As DirectDraw7

' Variables to hold enumerated values.
Dim g_sDriverGUID As String
Dim g_sDeviceGUID As String
Dim g_ddsdMode As DDSURFACEDESC2
Dim g_bUsingFullScreen As Boolean
Dim g_bUsing3DHardware As Boolean
Dim g_enumInfo As DDSURFACEDESC2

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
'   Called by the main form to retrieve selected driver, device, and mode
'   information.
'
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
Public Sub GetSelectedDriver(sDriverGUID As String, sDeviceGUID As String, Width As Long, Height As Long, BitCount As Long, bUsingFullScreen As Boolean, bUsing3DHardware As Boolean)
    ' Set the enumerated values.
    sDeviceGUID = g_sDriverGUID
    sDeviceGUID = g_sDeviceGUID
    Width = g_ddsdMode.lWidth
    Height = g_ddsdMode.lHeight
    BitCount = g_ddsdMode.ddpfPixelFormat.lRGBBitCount
    bUsingFullScreen = g_bUsingFullScreen
    bUsing3DHardware = g_bUsing3DHardware
End Sub


Private Sub cmbDevice_Click()
    SelectDisplayModes
End Sub

Private Sub Form_Load()
    Form1.Show
    
    ' Get driver information.
    EnumDriver
    
    ' Enumerate the display modes.
    EnumModes
    
    ' Enumerate the devices.
    EnumDevices
    
    ' Select an enumerated device and build a list of supported display modes
    ' for that device.
    SelectDisplayModes
    
    Me.Show
    
 End Sub

Private Sub EnumDriver()
    Dim i As Long
    
    ' Create the driver, in this case the active display driver.
    Set g_dd = g_dx.DirectDrawCreate("")
        
    ' Get driver information. For this simple tutorial we are just going to use
    ' the primary display driver.
    Set g_ddEnum = g_dx.GetDDEnum()
    
    For i = 1 To g_ddEnum.GetCount()
        g_sDriverGUID = g_ddEnum.GetDescription(i)
    Next i

End Sub



''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
'   Enumerate devices.
'
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
Private Sub EnumDevices()
    Dim i As Long
      
    ' Retrieve the Direct3D object for the current display device.
    Set g_d3d = g_dd.GetDirect3D()
            
    ' Get device information and place device user-friendly names in a combo box.
    cmbDevice.Clear
    Set g_d3dEnumDevices = g_d3d.GetDevicesEnum()
    For i = 1 To g_d3dEnumDevices.GetCount()
        cmbDevice.AddItem g_d3dEnumDevices.GetName(i)
    Next
          
    cmbDevice.ListIndex = 0
      
    End Sub

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
'   Enumerate display modes.
'
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
Private Sub EnumModes()
   Set g_ddEnumModes = g_dd.GetDisplayModesEnum(DDEDM_DEFAULT, g_enumInfo)

End Sub

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
'   Retrieve the display modes for a specific device and display those values in a
'   combo box.
'
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
Private Sub SelectDisplayModes()
    Dim desc As DDSURFACEDESC2
    Dim deviceDesc As D3DDEVICEDESC7
    Dim j As Long
    Dim bCompatible As Boolean
    Dim modeDepth As Long
    Dim bitDepth As Long
    Dim currMode As Long
               
    ' Get the current display mode information
    g_dd.GetDisplayMode desc
    currMode = desc.ddpfPixelFormat.lRGBBitCount
    
    ' Add the first item to the combo box, which is the current display mode.
    cmbMode.Clear
    cmbMode.AddItem Str(currMode) + "-bit Display Window"
    
    ' Determine which device has been selected in the combo box. This is the device we want to
    ' build a list of supported modes for, which we will then place in the combo box.
    Call g_d3dEnumDevices.GetDesc(cmbDevice.ListIndex + 1, deviceDesc)
    
    '
    ' Build the list of supported modes for the device using the following algorithm:
    '   Step 1: Return a video mode description for the specified element in the
    '           DirectDrawEnumModes object.
    '   Step 2: Retrieve the bits per pixel support in the returned video mode description.
    '   Step 3: Retrieve the given device's rendering depth.
    '   Step 4: Check to see if the video mode of the specified element in the DirectDrawEnumModes
    '           object is compatible with the given device's rendering depth. If it is, then accept
    '           the display mode; otherwise, reject it. Skip all 8-bit modes.
    '
    
    For j = 1 To g_ddEnumModes.GetCount()
        ' We have not tested this display mode yet, so assume it is not compatible.
        bCompatible = False
        ' Step 1:
        g_ddEnumModes.GetItem j, g_enumInfo
                
        ' Step 2:
        modeDepth = g_enumInfo.ddpfPixelFormat.lRGBBitCount
        
        ' Step 3:
        bitDepth = deviceDesc.lDeviceRenderBitDepth
        
        ' Step 4:
        If (32 = modeDepth) And (bitDepth And DDBD_32) Then bCompatible = True
        If (24 = modeDepth) And (bitDepth And DDBD_24) Then bCompatible = True
        If (16 = modeDepth) And (bitDepth And DDBD_16) Then bCompatible = True
        
        ' Populate the combo box with only the display modes that we want.
        If bCompatible Then
            cmbMode.AddItem Str(g_enumInfo.lWidth) + " x" + Str(g_enumInfo.lHeight) + " x" + Str(g_enumInfo.ddpfPixelFormat.lRGBBitCount)
        End If
    Next j  ' move on to the next mode
       
    cmbMode.ListIndex = 0
    
End Sub

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
'   Save the driver, device, and mode information that was selected in the
'   combo box.
'
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
Private Sub cmdOK_Click()
Dim i As Long
    Dim sDeviceDesc As String
    Dim sModeRes As String
    
    ' Find the device in the combo list.
    sDeviceDesc = cmbDevice.Text
    
    For i = 1 To g_d3dEnumDevices.GetCount()
        If sDeviceDesc = g_d3dEnumDevices.GetName(i) Then
            g_sDeviceGUID = g_d3dEnumDevices.GetGuid(i)
        End If
    Next
    
    ' Determine if we are using a hardware device.
    If g_sDeviceGUID = "IID_IDirect3DHALDevice" Then
        g_bUsing3DHardware = True
    Else
        g_bUsing3DHardware = False
    End If
        
    ' Determine if we are going to render in windowed mode or in full-screen exclusive mode.
    sModeRes = cmbMode.Text
    If sModeRes = cmbMode.List(0) Then
        g_bUsingFullScreen = False
    Else
        g_bUsingFullScreen = True
    End If
    
    ' Find the DirectDraw surface description.
    Dim ddsd As DDSURFACEDESC2
    
    For i = 1 To g_ddEnumModes.GetCount()
        g_ddEnumModes.GetItem i, ddsd
         If sModeRes = Str(ddsd.lWidth) + " x" + Str(ddsd.lHeight) + " x" + Str(ddsd.ddpfPixelFormat.lRGBBitCount) Then
            g_ddsdMode = ddsd
         End If
    Next i
    
    Unload Me
    
    ' Run the Application.
    Form1.RunApp

End Sub




