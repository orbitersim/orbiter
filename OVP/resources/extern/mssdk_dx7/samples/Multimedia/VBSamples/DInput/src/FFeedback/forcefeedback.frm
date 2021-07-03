VERSION 5.00
Object = "{831FDD16-0C5C-11D2-A9FC-0000F8754DA1}#2.0#0"; "Mscomctl.ocx"
Begin VB.Form frmForceFeedback 
   BorderStyle     =   3  'Fixed Dialog
   Caption         =   "VB Force Feedback"
   ClientHeight    =   7020
   ClientLeft      =   45
   ClientTop       =   330
   ClientWidth     =   9315
   Icon            =   "ForceFeedback.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   7020
   ScaleWidth      =   9315
   StartUpPosition =   2  'CenterScreen
   Begin VB.Frame frameDirection 
      Caption         =   "Direction"
      Height          =   2895
      Left            =   6720
      TabIndex        =   70
      Top             =   3960
      Width           =   2415
      Begin VB.OptionButton optDirection 
         Caption         =   "Option1"
         Height          =   210
         Index           =   0
         Left            =   1080
         TabIndex        =   78
         Top             =   480
         Value           =   -1  'True
         Width           =   200
      End
      Begin VB.OptionButton optDirection 
         Caption         =   "Option2"
         Height          =   210
         Index           =   4
         Left            =   1080
         TabIndex        =   77
         Top             =   2280
         Width           =   200
      End
      Begin VB.OptionButton optDirection 
         Caption         =   "Option3"
         Height          =   210
         Index           =   6
         Left            =   240
         TabIndex        =   76
         Top             =   1380
         Width           =   200
      End
      Begin VB.OptionButton optDirection 
         Caption         =   "Option4"
         Height          =   210
         Index           =   2
         Left            =   1920
         TabIndex        =   75
         Top             =   1320
         Width           =   200
      End
      Begin VB.OptionButton optDirection 
         Caption         =   "Option5"
         Height          =   210
         Index           =   7
         Left            =   480
         TabIndex        =   74
         Top             =   840
         Width           =   200
      End
      Begin VB.OptionButton optDirection 
         Caption         =   "Option6"
         Height          =   210
         Index           =   1
         Left            =   1680
         TabIndex        =   73
         Top             =   840
         Width           =   200
      End
      Begin VB.OptionButton optDirection 
         Caption         =   "Option7"
         Height          =   210
         Index           =   5
         Left            =   480
         TabIndex        =   72
         Top             =   1920
         Width           =   200
      End
      Begin VB.OptionButton optDirection 
         Caption         =   "Option8"
         Height          =   210
         Index           =   3
         Left            =   1680
         TabIndex        =   71
         Top             =   1920
         Width           =   200
      End
   End
   Begin VB.Frame frameEnvelope 
      Caption         =   "Envelope"
      Height          =   2895
      Left            =   3720
      TabIndex        =   56
      Top             =   3960
      Width           =   2775
      Begin MSComctlLib.Slider sldEnvelope 
         Height          =   255
         Index           =   0
         Left            =   240
         TabIndex        =   58
         Top             =   855
         Width           =   2295
         _ExtentX        =   4048
         _ExtentY        =   450
         _Version        =   393216
         LargeChange     =   100
         SmallChange     =   10
         Max             =   10000
         SelStart        =   10000
         TickFrequency   =   1000
         Value           =   10000
      End
      Begin VB.CheckBox chkEnvelope 
         Caption         =   "Use envelope"
         Height          =   255
         Left            =   720
         TabIndex        =   57
         Top             =   280
         Width           =   1335
      End
      Begin MSComctlLib.Slider sldEnvelope 
         Height          =   255
         Index           =   1
         Left            =   240
         TabIndex        =   59
         Top             =   1380
         Width           =   2295
         _ExtentX        =   4048
         _ExtentY        =   450
         _Version        =   393216
         LargeChange     =   10000
         SmallChange     =   1000
         Max             =   5000000
         TickFrequency   =   500000
      End
      Begin MSComctlLib.Slider sldEnvelope 
         Height          =   255
         Index           =   2
         Left            =   240
         TabIndex        =   60
         Top             =   1965
         Width           =   2295
         _ExtentX        =   4048
         _ExtentY        =   450
         _Version        =   393216
         LargeChange     =   100
         SmallChange     =   10
         Max             =   10000
         SelStart        =   10000
         TickFrequency   =   1000
         Value           =   10000
      End
      Begin MSComctlLib.Slider sldEnvelope 
         Height          =   255
         Index           =   3
         Left            =   240
         TabIndex        =   61
         Top             =   2520
         Width           =   2295
         _ExtentX        =   4048
         _ExtentY        =   450
         _Version        =   393216
         LargeChange     =   1000
         SmallChange     =   100
         Max             =   5000000
         TickFrequency   =   500000
      End
      Begin VB.Label lblEnvelope 
         AutoSize        =   -1  'True
         Caption         =   "0"
         Height          =   195
         Index           =   3
         Left            =   1440
         TabIndex        =   69
         Top             =   2325
         Width           =   90
      End
      Begin VB.Label lblEnvelope 
         AutoSize        =   -1  'True
         Caption         =   "10000"
         Height          =   195
         Index           =   2
         Left            =   1440
         TabIndex        =   68
         Top             =   1740
         Width           =   450
      End
      Begin VB.Label lblEnvelope 
         AutoSize        =   -1  'True
         Caption         =   "0"
         Height          =   195
         Index           =   1
         Left            =   1440
         TabIndex        =   67
         Top             =   1170
         Width           =   90
      End
      Begin VB.Label lblEnvelope 
         AutoSize        =   -1  'True
         Caption         =   "10000"
         Height          =   195
         Index           =   0
         Left            =   1440
         TabIndex        =   66
         Top             =   600
         Width           =   450
      End
      Begin VB.Label Label20 
         AutoSize        =   -1  'True
         Caption         =   "Fade Time:"
         Height          =   195
         Left            =   510
         TabIndex        =   65
         Top             =   2325
         Width           =   795
      End
      Begin VB.Label Label19 
         AutoSize        =   -1  'True
         Caption         =   "Fade Level:"
         Height          =   195
         Left            =   465
         TabIndex        =   64
         Top             =   1740
         Width           =   840
      End
      Begin VB.Label Label18 
         AutoSize        =   -1  'True
         Caption         =   "Attack Time:"
         Height          =   195
         Left            =   405
         TabIndex        =   63
         Top             =   1170
         Width           =   900
      End
      Begin VB.Label Label17 
         AutoSize        =   -1  'True
         Caption         =   "Attack Level:"
         Height          =   195
         Left            =   360
         TabIndex        =   62
         Top             =   600
         Width           =   945
      End
   End
   Begin VB.Frame frameTypeContainer 
      Caption         =   "Type-Specific Parameters"
      Height          =   3495
      Left            =   3720
      TabIndex        =   12
      Top             =   240
      Width           =   5415
      Begin VB.Frame frameCondition 
         Caption         =   "Conditional Force"
         Height          =   3015
         Left            =   120
         TabIndex        =   37
         Top             =   240
         Visible         =   0   'False
         Width           =   5175
         Begin VB.OptionButton optConditionAxis 
            Caption         =   "Y Axis"
            Height          =   255
            Index           =   1
            Left            =   2280
            TabIndex        =   80
            Top             =   2700
            Width           =   735
         End
         Begin VB.OptionButton optConditionAxis 
            Caption         =   "X Axis"
            Height          =   255
            Index           =   0
            Left            =   2280
            TabIndex        =   79
            Top             =   2440
            Value           =   -1  'True
            Width           =   735
         End
         Begin MSComctlLib.Slider sldCondition 
            Height          =   255
            Index           =   0
            Left            =   240
            TabIndex        =   38
            Top             =   600
            Width           =   2055
            _ExtentX        =   3625
            _ExtentY        =   450
            _Version        =   393216
            LargeChange     =   100
            SmallChange     =   10
            Max             =   10000
            TickFrequency   =   1000
         End
         Begin MSComctlLib.Slider sldCondition 
            Height          =   255
            Index           =   1
            Left            =   240
            TabIndex        =   39
            Top             =   1320
            Width           =   2055
            _ExtentX        =   3625
            _ExtentY        =   450
            _Version        =   393216
            LargeChange     =   100
            SmallChange     =   10
            Min             =   -10000
            Max             =   10000
            TickFrequency   =   1000
         End
         Begin MSComctlLib.Slider sldCondition 
            Height          =   255
            Index           =   2
            Left            =   240
            TabIndex        =   40
            Top             =   2160
            Width           =   2055
            _ExtentX        =   3625
            _ExtentY        =   450
            _Version        =   393216
            LargeChange     =   100
            SmallChange     =   10
            Max             =   10000
            SelStart        =   10000
            TickFrequency   =   1000
            Value           =   10000
         End
         Begin MSComctlLib.Slider sldCondition 
            Height          =   255
            Index           =   3
            Left            =   2880
            TabIndex        =   41
            Top             =   600
            Width           =   2055
            _ExtentX        =   3625
            _ExtentY        =   450
            _Version        =   393216
            LargeChange     =   100
            SmallChange     =   10
            Min             =   -10000
            Max             =   10000
            TickFrequency   =   1000
         End
         Begin MSComctlLib.Slider sldCondition 
            Height          =   255
            Index           =   4
            Left            =   2880
            TabIndex        =   42
            Top             =   1320
            Width           =   2055
            _ExtentX        =   3625
            _ExtentY        =   450
            _Version        =   393216
            LargeChange     =   100
            SmallChange     =   10
            Min             =   -10000
            Max             =   10000
            TickFrequency   =   1000
         End
         Begin MSComctlLib.Slider sldCondition 
            Height          =   255
            Index           =   5
            Left            =   2880
            TabIndex        =   43
            Top             =   2160
            Width           =   2055
            _ExtentX        =   3625
            _ExtentY        =   450
            _Version        =   393216
            LargeChange     =   100
            SmallChange     =   10
            Max             =   10000
            SelStart        =   10000
            TickFrequency   =   1000
            Value           =   10000
         End
         Begin VB.Label Label11 
            AutoSize        =   -1  'True
            Caption         =   "Dead band:"
            Height          =   195
            Left            =   240
            TabIndex        =   55
            Top             =   360
            Width           =   840
         End
         Begin VB.Label Label12 
            AutoSize        =   -1  'True
            Caption         =   "Negative Coefficient:"
            Height          =   195
            Left            =   240
            TabIndex        =   54
            Top             =   1080
            Width           =   1455
         End
         Begin VB.Label Label13 
            AutoSize        =   -1  'True
            Caption         =   "Negative Saturation:"
            Height          =   195
            Left            =   240
            TabIndex        =   53
            Top             =   1920
            Width           =   1455
         End
         Begin VB.Label Label14 
            AutoSize        =   -1  'True
            Caption         =   "Offset:"
            Height          =   195
            Left            =   3120
            TabIndex        =   52
            Top             =   360
            Width           =   465
         End
         Begin VB.Label Label15 
            AutoSize        =   -1  'True
            Caption         =   "Positive Coefficient:"
            Height          =   195
            Left            =   3000
            TabIndex        =   51
            Top             =   1080
            Width           =   1365
         End
         Begin VB.Label Label16 
            AutoSize        =   -1  'True
            Caption         =   "Positive Saturation:"
            Height          =   195
            Left            =   3000
            TabIndex        =   50
            Top             =   1920
            Width           =   1365
         End
         Begin VB.Label lblCondition 
            AutoSize        =   -1  'True
            Caption         =   "0"
            Height          =   195
            Index           =   0
            Left            =   1200
            TabIndex        =   49
            Top             =   360
            Width           =   90
         End
         Begin VB.Label lblCondition 
            AutoSize        =   -1  'True
            Caption         =   "0"
            Height          =   195
            Index           =   1
            Left            =   1800
            TabIndex        =   48
            Top             =   1080
            Width           =   90
         End
         Begin VB.Label lblCondition 
            AutoSize        =   -1  'True
            Caption         =   "10000"
            Height          =   195
            Index           =   2
            Left            =   1800
            TabIndex        =   47
            Top             =   1920
            Width           =   450
         End
         Begin VB.Label lblCondition 
            AutoSize        =   -1  'True
            Caption         =   "0"
            Height          =   195
            Index           =   3
            Left            =   3600
            TabIndex        =   46
            Top             =   360
            Width           =   90
         End
         Begin VB.Label lblCondition 
            AutoSize        =   -1  'True
            Caption         =   "0"
            Height          =   195
            Index           =   4
            Left            =   4440
            TabIndex        =   45
            Top             =   1080
            Width           =   90
         End
         Begin VB.Label lblCondition 
            AutoSize        =   -1  'True
            Caption         =   "10000"
            Height          =   195
            Index           =   5
            Left            =   4410
            TabIndex        =   44
            Top             =   1920
            Width           =   450
         End
      End
      Begin VB.Frame framePeriodic 
         Caption         =   "Periodic Force"
         Height          =   3015
         Left            =   120
         TabIndex        =   24
         Top             =   240
         Visible         =   0   'False
         Width           =   5175
         Begin MSComctlLib.Slider sldPeriodic 
            Height          =   255
            Index           =   0
            Left            =   600
            TabIndex        =   25
            Top             =   480
            Width           =   4095
            _ExtentX        =   7223
            _ExtentY        =   450
            _Version        =   393216
            LargeChange     =   100
            SmallChange     =   10
            Max             =   10000
            TickFrequency   =   1000
         End
         Begin MSComctlLib.Slider sldPeriodic 
            Height          =   255
            Index           =   1
            Left            =   600
            TabIndex        =   26
            Top             =   1080
            Width           =   4095
            _ExtentX        =   7223
            _ExtentY        =   450
            _Version        =   393216
            LargeChange     =   1000
            SmallChange     =   100
            Min             =   -10000
            Max             =   10000
            TickFrequency   =   1000
         End
         Begin MSComctlLib.Slider sldPeriodic 
            Height          =   255
            Index           =   2
            Left            =   600
            TabIndex        =   27
            Top             =   1800
            Width           =   4095
            _ExtentX        =   7223
            _ExtentY        =   450
            _Version        =   393216
            LargeChange     =   1000
            SmallChange     =   100
            Max             =   35999
            TickFrequency   =   1000
         End
         Begin MSComctlLib.Slider sldPeriodic 
            Height          =   255
            Index           =   3
            Left            =   600
            TabIndex        =   28
            Top             =   2520
            Width           =   4095
            _ExtentX        =   7223
            _ExtentY        =   450
            _Version        =   393216
            LargeChange     =   1000
            SmallChange     =   100
            Max             =   500000
            TickFrequency   =   10000
         End
         Begin VB.Label lblPeriodic 
            AutoSize        =   -1  'True
            Caption         =   "0"
            Height          =   195
            Index           =   3
            Left            =   2400
            TabIndex        =   36
            Top             =   2280
            Width           =   90
         End
         Begin VB.Label Label10 
            AutoSize        =   -1  'True
            Caption         =   "Period:"
            Height          =   195
            Left            =   1800
            TabIndex        =   35
            Top             =   2280
            Width           =   495
         End
         Begin VB.Label lblPeriodic 
            AutoSize        =   -1  'True
            Caption         =   "0"
            Height          =   195
            Index           =   2
            Left            =   2400
            TabIndex        =   34
            Top             =   1560
            Width           =   90
         End
         Begin VB.Label Label9 
            AutoSize        =   -1  'True
            Caption         =   "Phase:"
            Height          =   195
            Left            =   1800
            TabIndex        =   33
            Top             =   1560
            Width           =   495
         End
         Begin VB.Label lblPeriodic 
            AutoSize        =   -1  'True
            Caption         =   "0"
            Height          =   195
            Index           =   1
            Left            =   2400
            TabIndex        =   32
            Top             =   840
            Width           =   90
         End
         Begin VB.Label Label8 
            AutoSize        =   -1  'True
            Caption         =   "Offset:"
            Height          =   195
            Left            =   1800
            TabIndex        =   31
            Top             =   840
            Width           =   465
         End
         Begin VB.Label lblPeriodic 
            AutoSize        =   -1  'True
            Caption         =   "0"
            Height          =   195
            Index           =   0
            Left            =   2400
            TabIndex        =   30
            Top             =   240
            Width           =   90
         End
         Begin VB.Label Label7 
            AutoSize        =   -1  'True
            Caption         =   "Magnitude:"
            Height          =   195
            Left            =   1560
            TabIndex        =   29
            Top             =   240
            Width           =   795
         End
      End
      Begin VB.Frame frameRampForce 
         Caption         =   "Ramp Force"
         Height          =   3015
         Left            =   120
         TabIndex        =   17
         Top             =   240
         Width           =   5175
         Begin MSComctlLib.Slider sldRampRange 
            Height          =   495
            Index           =   0
            Left            =   480
            TabIndex        =   18
            Top             =   840
            Width           =   4215
            _ExtentX        =   7435
            _ExtentY        =   873
            _Version        =   393216
            LargeChange     =   1000
            SmallChange     =   100
            Min             =   -10000
            Max             =   10000
            TickFrequency   =   1000
         End
         Begin MSComctlLib.Slider sldRampRange 
            Height          =   495
            Index           =   1
            Left            =   480
            TabIndex        =   19
            Top             =   1920
            Width           =   4215
            _ExtentX        =   7435
            _ExtentY        =   873
            _Version        =   393216
            LargeChange     =   1000
            SmallChange     =   100
            Min             =   -10000
            Max             =   10000
            TickFrequency   =   1000
         End
         Begin VB.Label lblRange 
            AutoSize        =   -1  'True
            Caption         =   "0"
            Height          =   195
            Index           =   1
            Left            =   2400
            TabIndex        =   23
            Top             =   1680
            Width           =   90
         End
         Begin VB.Label Label2 
            AutoSize        =   -1  'True
            Caption         =   "Range End:"
            Height          =   195
            Left            =   1440
            TabIndex        =   22
            Top             =   1680
            Width           =   855
         End
         Begin VB.Label lblRange 
            AutoSize        =   -1  'True
            Caption         =   "0"
            Height          =   195
            Index           =   0
            Left            =   2400
            TabIndex        =   21
            Top             =   600
            Width           =   90
         End
         Begin VB.Label Label1 
            AutoSize        =   -1  'True
            Caption         =   "Range Start:"
            Height          =   195
            Left            =   1440
            TabIndex        =   20
            Top             =   600
            Width           =   900
         End
      End
      Begin VB.Frame frameConstantForce 
         Caption         =   "Constant Force"
         Height          =   3015
         Left            =   120
         TabIndex        =   13
         Top             =   240
         Visible         =   0   'False
         Width           =   5175
         Begin MSComctlLib.Slider sldConstantForce 
            Height          =   495
            Left            =   840
            TabIndex        =   14
            Top             =   1320
            Width           =   3615
            _ExtentX        =   6376
            _ExtentY        =   873
            _Version        =   393216
            LargeChange     =   100
            SmallChange     =   10
            Max             =   10000
            SelStart        =   10000
            TickFrequency   =   1000
            Value           =   10000
         End
         Begin VB.Label lblConstantForce 
            AutoSize        =   -1  'True
            Caption         =   "10000"
            Height          =   195
            Left            =   3360
            TabIndex        =   16
            Top             =   1080
            Width           =   450
         End
         Begin VB.Label Label5 
            AutoSize        =   -1  'True
            Caption         =   "Constant Force Magnitude:"
            Height          =   195
            Left            =   1320
            TabIndex        =   15
            Top             =   1080
            Width           =   1920
         End
      End
   End
   Begin VB.Frame frmGeneral 
      Caption         =   "General Parameters"
      Height          =   2895
      Left            =   120
      TabIndex        =   1
      Top             =   3960
      Width           =   3375
      Begin MSComctlLib.Slider sldDuration 
         Height          =   495
         Left            =   240
         TabIndex        =   2
         Top             =   600
         Width           =   2775
         _ExtentX        =   4895
         _ExtentY        =   873
         _Version        =   393216
         LargeChange     =   100
         SmallChange     =   10
         Min             =   1
         Max             =   50001
         SelStart        =   50001
         TickFrequency   =   5000
         Value           =   50001
      End
      Begin MSComctlLib.Slider sldGain 
         Height          =   495
         Left            =   240
         TabIndex        =   5
         Top             =   1320
         Width           =   2775
         _ExtentX        =   4895
         _ExtentY        =   873
         _Version        =   393216
         LargeChange     =   100
         SmallChange     =   10
         Max             =   10000
         SelStart        =   10000
         TickFrequency   =   1000
         Value           =   10000
      End
      Begin MSComctlLib.Slider sldSamplePeriod 
         Height          =   495
         Left            =   240
         TabIndex        =   9
         Top             =   2160
         Width           =   2775
         _ExtentX        =   4895
         _ExtentY        =   873
         _Version        =   393216
         LargeChange     =   1000
         SmallChange     =   100
         Max             =   100000
         TickFrequency   =   10000
      End
      Begin VB.Label lblSamplePeriod 
         AutoSize        =   -1  'True
         Caption         =   "Default"
         Height          =   195
         Left            =   1320
         TabIndex        =   11
         Top             =   1920
         Width           =   510
      End
      Begin VB.Label Label6 
         AutoSize        =   -1  'True
         Caption         =   "Sample rate:"
         Height          =   195
         Left            =   360
         TabIndex        =   10
         Top             =   1920
         Width           =   885
      End
      Begin VB.Label lblGain 
         AutoSize        =   -1  'True
         Caption         =   "10000"
         Height          =   195
         Left            =   1200
         TabIndex        =   7
         Top             =   1080
         Width           =   450
      End
      Begin VB.Label Label4 
         AutoSize        =   -1  'True
         Caption         =   "Effect gain:"
         Height          =   195
         Left            =   360
         TabIndex        =   6
         Top             =   1080
         Width           =   810
      End
      Begin VB.Label lblDuration 
         AutoSize        =   -1  'True
         Caption         =   "Infinite"
         Height          =   195
         Left            =   1560
         TabIndex        =   4
         Top             =   360
         Width           =   465
      End
      Begin VB.Label Label3 
         AutoSize        =   -1  'True
         Caption         =   "Effect Duration:"
         Height          =   195
         Left            =   360
         TabIndex        =   3
         Top             =   360
         Width           =   1110
      End
   End
   Begin VB.ListBox lstEffects 
      Height          =   3375
      Left            =   120
      TabIndex        =   0
      Top             =   360
      Width           =   3375
   End
   Begin VB.Label lblAvailable 
      AutoSize        =   -1  'True
      Caption         =   "Available effects:"
      Height          =   195
      Left            =   120
      TabIndex        =   8
      Top             =   120
      Width           =   1215
   End
End
Attribute VB_Name = "frmForceFeedback"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Dim dx As New DirectX7                                  'DirectX 7 object
Dim di As DirectInput                                   'DirectInput object
Dim diJoystick As DirectInputDevice                     'DirectInput device object
Dim enumDevice As DirectInputEnumDevices                'DirectInput enumeration for devices object
Dim diEnumObjects As DirectInputEnumDeviceObjects       'DirectInput enumeration for objects on a device object
Dim diDevObjInstance As DirectInputDeviceObjectInstance 'DirectInput object on a device object
Dim diEffEnum As DirectInputEnumEffects                 'DirectInput enumeration for force feedback effects object
Dim diFFEffect() As DirectInputEffect                   'Force feedback effects object
Dim diEffectType As Long                                'Will be used to store the type of effect an effect object is
Dim diFFStaticParams As Long                            'Will be used to store the static parameters of an effect object
Dim EffectParams() As Long                              'Used to store the type of effect it is
Dim Button() As Long                                    'Will be used to store the offset of all the buttons on diJoystick
Dim Caps As DIDEVCAPS                                   'Will be used to store the capabilities of the diJoystick
Dim lngLastEffectIndex As Long                          'Will be used to store the last effect

Private Function CreateFFEffect(Index As Integer) As DIEFFECT

    'This sub creates a generic effect DIEFFECT structure to be used in creating the effect
    
    With CreateFFEffect
        .lDuration = -1                                 'Infinite duration
        .lGain = 10000                                  'Full gain
        .lSamplePeriod = 0                              'Default sample period
        .lTriggerButton = Button(1)                     'Use button 1 on the joystick as the trigger
        .lTriggerRepeatInterval = -1                    'Turn off trigger repeat interval
                
        .constantForce.lMagnitude = 10000               'Make the magnitude of a constant force effect at full
        .rampForce.lRangeStart = 0                      'Make the magnitude at the start of a ramp force 0
        .rampForce.lRangeEnd = 0                        'Make the magnitude at the end of a ramp force 0
        .conditionFlags = DICONDITION_USE_BOTH_AXES     'Use both axis when using a conditional force
        With .conditionX                                'For the X axis
            .lDeadBand = 0                              'Make an effect with no deadband
            .lNegativeSaturation = 10000                'Turn the negative saturation all the way up
            .lOffset = 0                                'Zero the offset
            .lPositiveSaturation = 10000                'Turn the positive saturation all the way up
        End With
        With .conditionY                                'For the Y axis
            .lDeadBand = 0                              'Make an effect with no deadband
            .lNegativeSaturation = 10000                'Turn the negative saturation all the way up
            .lOffset = 0                                'Zero the offset
            .lPositiveSaturation = 10000                'Turn the positive saturation all the way up
        End With
        With .periodicForce                             'For a periodic force
            .lMagnitude = 10000                         'Turn the magnitude of the force all the way up
            .lOffset = 0                                'Zero the offset
            .lPeriod = 1                                'Set the length of a cycle to 1
            .lPhase = 0                                 'Zero the starting phase. Phase is something that has very
                                                        'limited support, so changing this parameter will almost always
                                                        'fail. Be prepared to catch the error this will return.
        End With
    End With
    
End Function
Private Sub chkEnvelope_Click()

    Call ChangeParameter("envelope")                    'Call the sub to change the parameters for the envelope of the effect.
            
End Sub

Private Sub Form_Load()

    Dim intCount As Integer                             'Count variable
        
    Set di = dx.DirectInputCreate                       'Create the direct input device
    Set enumDevice = di.GetDIEnumDevices(DIDEVTYPE_JOYSTICK, DIEDFL_ATTACHEDONLY Or DIEDFL_FORCEFEEDBACK)
                                                        'Enumerate all joysticks that are attached to the system
    If enumDevice.GetCount > 0 Then                     'If the count is above 0, there is a force feedback joystick attached
        Set diJoystick = di.CreateDevice(enumDevice.GetItem(1).GetGuidInstance)
                                                        'Get the GUID for the joystick
        diJoystick.SetCommonDataFormat DIFORMAT_JOYSTICK
                                                        'Set the format of the device to that of a joystick
        diJoystick.SetCooperativeLevel Me.hWnd, DISCL_BACKGROUND Or DISCL_EXCLUSIVE
                                                        'Set the cooperative level of the device as an exclusive
                                                        'background device, and attach it to the form's hwnd
        diJoystick.GetCapabilities Caps                 'Get the capabilites of the device
        Set diEnumObjects = diJoystick.GetDeviceObjectsEnum(DIDFT_FFEFFECTTRIGGER Or DIDFT_BUTTON)
                                                    'Grab all the objects on the device, as long as it can be used
                                                    'as a force feedback trigger, and it is a button
                    
        ReDim Button(diEnumObjects.GetCount)        'Redimension the array to the number of buttons found
        For intCount = 1 To diEnumObjects.GetCount  'Loop through all the objects
            Set diDevObjInstance = diEnumObjects.GetItem(intCount)
                                                    'Get this item, and create it as a device object instance
            Button(intCount) = diDevObjInstance.GetOfs
                                                    'Store the offset of this object in the button array
        Next intCount
        
        'turn off autocenter
        Dim prop As DIPROPLONG
        prop.lData = 0
        prop.lHow = DIPH_DEVICE
        prop.lObj = 0
        prop.lSize = Len(prop)
        Call diJoystick.SetProperty("DIPROP_AUTOCENTER", prop)
         
        
        diJoystick.Acquire                          'Make sure to aquire the device
        Set diEffEnum = diJoystick.GetEffectsEnum(DIEFT_ALL)
                                                    'Enumerate all the available effects
        For intCount = 1 To diEffEnum.GetCount      'Loop through all the effects
            diEffectType = diEffEnum.GetType(intCount) And &HFF
                                                    'Filter out the major type of effect it is
            diFFStaticParams = diEffEnum.GetStaticParams(intCount)
                                                    'Get the static parameters of this effect
            If (diEffectType = DIEFT_HARDWARE) And _
            (diFFStaticParams And DIEP_TYPESPECIFICPARAMS) <> 0 Then
                                                    'If this is a hardware effect that has type-specific parameters,
                GoTo Ignore                         'ignore it and skip to the next effect
            ElseIf diEffectType = DIEFT_CUSTOMFORCE Then
                                                    'If this effect is a custom effect,
                GoTo Ignore                         'ignore it and skip to the next effect
            End If
            lstEffects.AddItem diEffEnum.GetName(intCount)
                                                    'Add this effect to the listbox, displaying the name of the
                                                    'effect
            ReDim Preserve EffectParams(lstEffects.ListCount - 1)
                                                    'Redimension the array that stores the type of effect this
                                                    'effect is
            EffectParams(lstEffects.ListCount - 1) = diEffectType
                                                    'store the type of effect in the EffectParams array
            ReDim Preserve diFFEffect(lstEffects.ListCount - 1)
                                                    'Redimension the effect object array
            
            On Local Error GoTo ErrorHandler        'Catch any errors when creating the effect object
            Set diFFEffect(UBound(diFFEffect)) = diJoystick.CreateEffect(diEffEnum.GetEffectGuid(intCount), _
            CreateFFEffect(intCount))               'Create the effect, using the return value from the
                                                    'CreateFFEffect function, which returns a generic effect
                                                    'structure
            diFFEffect(UBound(diFFEffect)).Unload   'Since creating an effect automtically downloads it, unload
                                                    'it so we don't run out of room on the device.
                                                    
                                                    
            
Ignore:
        Next
    Else                                                'If no joystick is found attached to the system
        MsgBox "No force feedback joystick attached"    'Display this message
        Unload Me                                       'Unload the form
        End                                             'End the program
    End If
    lngLastEffectIndex = UBound(diFFEffect)             'Store the ubound of the effect array as the last effect accessed
    lstEffects.ListIndex = 0                            'make the first index of the listbox selected
    
    Exit Sub                                            'Exit the sub
    
ErrorHandler:                                           'Handle any errors that occur when trying to create an effect object
    
    If Err.Number = 5 Then                              'If this is an effect that isn't able to be loaded
        lstEffects.RemoveItem lstEffects.ListCount - 1  'Remove the item from the list
        ReDim Preserve diFFEffect(lstEffects.ListCount - 1)
                                                        'Redimension the array since this effect will not be included
        Resume Next                                     'Continue program execution on the next line
    ElseIf Err.Number = DIERR_NOTEXCLUSIVEACQUIRED Then 'If the program loses exclusive use of the joystick,
        diJoystick.Unacquire                            'Unacquire the joystick
        diJoystick.SetCooperativeLevel Me.hWnd, DISCL_BACKGROUND Or DISCL_EXCLUSIVE
                                                        'Set the cooperative level again
        diJoystick.Acquire                              'Acquire the joystick again
        Resume                                          'Resume execution on that line again, since the program
                                                        'now has control of the joystick
    End If
    
End Sub

Private Sub lstEffects_Click()
    
    'This sub unloads the last effect, downloads the new one, stores the index in the last effect variable, and
    'calls the sub to update all the frames and controls on the form
    
    Dim EffectInfo As DIEFFECT                          'Dim a DIEFFECT structure
    
    On Local Error GoTo ErrorHandler                    'Catch any errors while unloading/downloading effects
    
    diFFEffect(lngLastEffectIndex).Unload               'Unload the last effect
    diFFEffect(lstEffects.ListIndex).Download           'Download the new one
    lngLastEffectIndex = lstEffects.ListIndex           'Store this list index in the lasteffectindex variable
    Call UpdateFrames                                   'Call the sub that updates all the frames and the controls
                                                        'they contain on the form
    Exit Sub
    
ErrorHandler:                                           'Handle any errors
    
    If Err.Number = DIERR_NOTEXCLUSIVEACQUIRED Then     'If the program loses exclusive use of the joystick,
        diJoystick.Unacquire                            'Unacquire the joystick
        diJoystick.SetCooperativeLevel Me.hWnd, DISCL_BACKGROUND Or DISCL_EXCLUSIVE
                                                        'Set the cooperative level again
        diJoystick.Acquire                              'Acquire the joystick again
        Resume                                          'Resume execution on that line again, since the program
                                                        'now has control of the joystick
    End If
    
End Sub

Private Sub ChangeParameter(Param As String)

    'This sub changes the specified parameter
    
    Dim EffectInfo As DIEFFECT                          'Dim a DIEFFECT structure

    diFFEffect(lstEffects.ListIndex).GetParameters EffectInfo
                                                        'Get the parameters of this effect
    On Local Error GoTo ErrorHandler                    'If there are any errors that occur, trap them
        
    Select Case Param                                   'Select the string value passed to the sub
        Case "duration"                                 'If duration is being changed
            If sldDuration = 50001 Then                 'If the slider's value is at 50,001
                EffectInfo.lDuration = -1               'Then make the effect have an infinite duration
            Else                                        'Otherwise
                EffectInfo.lDuration = sldDuration * 100
                                                        'Multiply the slider's value by 100 to convert to microseconds
            End If
            diFFEffect(lstEffects.ListIndex).SetParameters EffectInfo, DIEP_DURATION
                                                        'Set the new parameter, specifiying the duration is what needs
                                                        'to be changed
        Case "gain"                                     'If gain is the parameter to be changed,
            EffectInfo.lGain = sldGain                  'Set the effect info element lGain to the value of the gain slider
            diFFEffect(lstEffects.ListIndex).SetParameters EffectInfo, DIEP_GAIN
                                                        'Set the new gain
        Case "samplerate"                               'The sample rate is the parameter to be changed
            EffectInfo.lSamplePeriod = sldSamplePeriod  'Set the effectinfo lSamplePeriod to the value of the sample period slider
            diFFEffect(lstEffects.ListIndex).SetParameters EffectInfo, DIEP_SAMPLEPERIOD
                                                        'Set the parameter
        Case "constantforcemagnitude"                   'If the constant force magnitude is to be changed
            EffectInfo.constantForce.lMagnitude = sldConstantForce
                                                        'Set the constant force element to the value of the constant
                                                        'force slider
            diFFEffect(lstEffects.ListIndex).SetParameters EffectInfo, DIEP_TYPESPECIFICPARAMS
                                                        'Change the parameter
        Case "rampforce"                                'If ramp force is the parameter to change
            EffectInfo.rampForce.lRangeStart = sldRampRange(0).Value
                                                        'Set the start range to the range slider(0) value
            EffectInfo.rampForce.lRangeEnd = sldRampRange(1).Value
                                                        'Set the end range to the range slider(1) value
            diFFEffect(lstEffects.ListIndex).SetParameters EffectInfo, DIEP_TYPESPECIFICPARAMS
                                                        'Set the parameters
        Case "periodic"                                 'If a periodic effect parameter needs to be changed
            With EffectInfo.periodicForce
                .lMagnitude = sldPeriodic(0)            'set the magnitude
                .lOffset = sldPeriodic(1)               'the offset
                .lPhase = sldPeriodic(2)                'the phase (this is almost always going to fail, there aren't
                                                        'a lot of drivers that will support this)
                .lPeriod = sldPeriodic(3)               'the period of the effect
            End With
            diFFEffect(lstEffects.ListIndex).SetParameters EffectInfo, DIEP_TYPESPECIFICPARAMS
                                                        'set the parameters
        Case "condition"                                'If it is a conditional effect parameter
            If optConditionAxis(0) Then                 'If the X axis is selected
                With EffectInfo.conditionX              'Update the X axis condition information
                    .lDeadBand = sldCondition(0)        'set the deadband
                    .lNegativeCoefficient = sldCondition(1)
                                                        'set the negative coefficient
                    .lNegativeSaturation = sldCondition(2)
                                                        'set the negative saturation
                    .lOffset = sldCondition(3)          'set the offset
                    .lPositiveCoefficient = sldCondition(4)
                                                        'set the positive coefficient
                    .lPositiveSaturation = sldCondition(5)
                                                        'set the positive saturation
                End With
            Else                                        'Otherwise, the Y axis is being set
                With EffectInfo.conditionY
                    .lDeadBand = sldCondition(0)        'set the deadband
                    .lNegativeCoefficient = sldCondition(1)
                                                        'set the negative coefficient
                    .lNegativeSaturation = sldCondition(2)
                                                        'set the negative saturation
                    .lOffset = sldCondition(3)          'set the offset
                    .lPositiveCoefficient = sldCondition(4)
                                                        'set the positive coefficient
                    .lPositiveSaturation = sldCondition(5)
                                                        'set the positive saturation
                End With
            End If
            diFFEffect(lstEffects.ListIndex).SetParameters EffectInfo, DIEP_TYPESPECIFICPARAMS
                                                        'set the parameters
        Case "envelope"                                 'the envelope needs to be changed
            If chkEnvelope.Value = 1 Then               'if the envelope checkbox is checked
                With EffectInfo
                    .bUseEnvelope = True                'let DirectInput know this effect has an envelope
                    With .envelope
                        .lAttackLevel = sldEnvelope(0)  'set the attack level
                        .lAttackTime = sldEnvelope(1)   'set the attack time
                        .lFadeLevel = sldEnvelope(2)    'set the fade level
                        .lFadeTime = sldEnvelope(3)     'set the fade time
                    End With
                End With
                diFFEffect(lstEffects.ListIndex).SetParameters EffectInfo, DIEP_ENVELOPE
                                                        'Set the new parameters
            Else
                EffectInfo.bUseEnvelope = False         'Otherwise, let DirectInput know that this effect doesn't use an envelope
                diFFEffect(lstEffects.ListIndex).SetParameters EffectInfo, DIEP_ENVELOPE
                                                        'Set the parameters to reflect this change
            End If
        Case "direction"                                'Direction is the parameter that needs to be changed
            With EffectInfo
                If optDirection(0) Then                 'If the north button is selected
                    .x = 0                              'set the effect's direction to come from the north
                ElseIf optDirection(1) Then             'If the north-east button is selected
                    .x = 4500                           'set the effect's direction to come from the north-east
                ElseIf optDirection(2) Then             'if the east button is selected
                    .x = 9000                           'set the effect's direction to come from the east
                ElseIf optDirection(3) Then             'if the south-east button is selected
                    .x = 13500                          'set the effect's direction to come from the south east
                ElseIf optDirection(4) Then             'if the south button is selected
                    .x = 18000                          'set the effect's direction to come from the south
                ElseIf optDirection(5) Then             'if the south-west button is selected
                    .x = 22500                          'set the effect's direction to come from the south-west
                ElseIf optDirection(6) Then             'if the west button is selected
                    .x = 27000                          'set the effect's direction to come from the west
                ElseIf optDirection(7) Then             'if the north-west button is selected
                    .x = 31500                          'set the effects's direction to come from the north-west
                End If
            End With
            diFFEffect(lstEffects.ListIndex).SetParameters EffectInfo, DIEP_DIRECTION
                                                        'set the parameters
    End Select
        
    Exit Sub
    
ErrorHandler:                                           'if an error is encountered while setting a parameter
    
    If Err.Number = 445 Then                            'If the object doesn't support this action then
        MsgBox "The parameter cannot be set to this value for this effect type"
                                                        'display this message
    ElseIf Err.Number = DIERR_NOTEXCLUSIVEACQUIRED Then 'If the program loses exclusive use of the joystick,
        diJoystick.Unacquire                            'Unacquire the joystick
        diJoystick.SetCooperativeLevel Me.hWnd, DISCL_BACKGROUND Or DISCL_EXCLUSIVE
                                                        'Set the cooperative level again
        diJoystick.Acquire                              'Acquire the joystick again
        Resume                                          'Resume execution on that line again, since the program
                                                        'now has control of the joystick
    End If

End Sub

Private Sub UpdateFrames()
    
    'This sub updates all the frames and controls the frame contains on the form, with the values in the
    'currently selected effect
    
    Dim EffType As DIEFFECT                             'dim a DIEFFECT structure
        
    frameConstantForce.Visible = False                  '
    frameRampForce.Visible = False                      'Hide all type-specific frames
    framePeriodic.Visible = False                       '
    frameCondition.Visible = False                      '
    diFFEffect(lstEffects.ListIndex).GetParameters EffType
                                                        'get the parameters of the effect
    optDirection(EffType.x \ 4500).Value = True           'make sure the correct direction is displayed
    If EffType.bUseEnvelope = True Then                 'if this effect is using an envelope
        chkEnvelope.Value = 1                           'make sure the envelope checkbox is checked
        sldEnvelope(0).Value = EffType.envelope.lAttackLevel
                                                        'display the attack level
        sldEnvelope(1).Value = EffType.envelope.lAttackTime
                                                        'display the attack time
        sldEnvelope(2).Value = EffType.envelope.lFadeLevel
                                                        'display the fade level
        sldEnvelope(3).Value = EffType.envelope.lFadeTime
                                                        'display the fade time
    Else                                                'otherwise
        chkEnvelope.Value = 0                           '
        sldEnvelope(0).Value = 0                        'Zero everything out
        sldEnvelope(1).Value = 0                        '
        sldEnvelope(2).Value = 0                        '
        sldEnvelope(3).Value = 0                        '
    End If
    If EffType.lDuration = -1 Then                      'If the effect duration is infinite
        sldDuration = 50001                             'make sure the slider reflects this
    Else                                                'otherwise
        sldDuration = EffType.lDuration \ 100           'set the slider to the milliseconds
    End If
    sldGain = EffType.lGain                             'display the gain
    sldSamplePeriod = EffType.lSamplePeriod             'display the sample period
    sldConstantForce = EffType.constantForce.lMagnitude 'display the constant force magnitude
    sldRampRange(0) = EffType.rampForce.lRangeStart     'display the range start of the ramp force
    sldRampRange(1) = EffType.rampForce.lRangeEnd       'display the range end of the ramp force
    sldPeriodic(0) = EffType.periodicForce.lMagnitude   'display the magnitude of the periodic force
    sldPeriodic(1) = EffType.periodicForce.lOffset      'display the offset of the periodic force
    sldPeriodic(2) = EffType.periodicForce.lPhase       'display the phase of the periodic force
    sldPeriodic(3) = EffType.periodicForce.lPeriod      'display the period of the periodic force
    If optConditionAxis(0) Then                         'if the X axis option button is selected
        sldCondition(0) = EffType.conditionX.lDeadBand  'display the deadband value
        sldCondition(1) = EffType.conditionX.lNegativeCoefficient
                                                        'display the neg. coefficient
        sldCondition(2) = EffType.conditionX.lNegativeSaturation
                                                        'display the neg. saturation
        sldCondition(3) = EffType.conditionX.lOffset    'display the offset
        sldCondition(4) = EffType.conditionX.lPositiveCoefficient
                                                        'display the pos. coefficient
        sldCondition(5) = EffType.conditionX.lPositiveSaturation
                                                        'display the pos. saturation
    Else                                                'otherwise, display all the info for the Y axis
        sldCondition(0) = EffType.conditionY.lDeadBand  'display the deadband value
        sldCondition(1) = EffType.conditionY.lNegativeCoefficient
                                                        'display the neg. coefficient
        sldCondition(2) = EffType.conditionY.lNegativeSaturation
                                                        'display the neg. saturation
        sldCondition(3) = EffType.conditionY.lOffset    'display the offset
        sldCondition(4) = EffType.conditionY.lPositiveCoefficient
                                                        'display the pos. coefficient
        sldCondition(5) = EffType.conditionY.lPositiveSaturation
                                                        'display the pos. saturation
    End If
    Select Case EffectParams(lstEffects.ListIndex)
                                                        'Get the effect type of this effect from the type stored in
                                                        'the array.
        Case DIEFT_CONSTANTFORCE                        'If this is a constant force effect
            frameConstantForce.Visible = True           'display the constant force frame
        Case DIEFT_RAMPFORCE                            'If this is a ramp force effect
            frameRampForce.Visible = True               'Show the ramp force frame
        Case DIEFT_PERIODIC                             'If this is a square force
            framePeriodic.Visible = True                'show the periodic effect frame
        Case DIEFT_CONDITION                              'if this is a spring effect
            frameCondition.Visible = True               'show the condition frame
        Case Else                                       'this effect is a hardware effect with no type-specific parameters
    End Select
    
End Sub

Private Sub optConditionAxis_Click(Index As Integer)
    
    Call UpdateFrames                                   'Update the frames
    
End Sub

Private Sub optDirection_Click(Index As Integer)
    
    Call ChangeParameter("direction")                   'Change the direction parameter
    
End Sub

Private Sub sldCondition_Change(Index As Integer)

    lblCondition(Index) = sldCondition(Index)           'display the value in the label
    Call ChangeParameter("condition")                   'change the condition parameter

End Sub

Private Sub sldConstantForce_Change()
    
    lblConstantForce = sldConstantForce                 'display the value
    Call ChangeParameter("constantforcemagnitude")      'change the parameter
    
End Sub

Private Sub sldDuration_Change()
    
    lblDuration = sldDuration \ 10 & " Milliseconds"    'show the value in milliseconds
    If sldDuration = 50001 Then lblDuration = "Infinite"
                                                        'if the value is 50,001 make sure that infinite is displayed
    Call ChangeParameter("duration")                    'change the duration parameter
    
End Sub

Private Sub sldEnvelope_Change(Index As Integer)

    lblEnvelope(Index) = sldEnvelope(Index) \ 1000      'show the value in milliseconds
    If chkEnvelope.Value = 1 Then                       'if the envelope check box is checked
        Call ChangeParameter("envelope")                'change the envelope parameter
    End If

End Sub

Private Sub sldGain_Change()

    lblGain = sldGain                                   'show the value
    Call ChangeParameter("gain")                        'change the parameter
    
End Sub

Private Sub sldPeriodic_Change(Index As Integer)

    lblPeriodic(Index) = sldPeriodic(Index)             'show the value
    Call ChangeParameter("periodic")                    'change the parameter
                
End Sub

Private Sub sldrampRange_Change(Index As Integer)
    
    lblRange(Index) = sldRampRange(Index).Value         'show the value
    Call ChangeParameter("rampforce")                   'change the parameter
    
End Sub

Private Sub sldSamplePeriod_Change()
    
    If sldSamplePeriod = 0 Then                         'if the sample period is 0,
        lblSamplePeriod = "Default"                     'display that it is the default
    Else                                                'otherwise
        lblSamplePeriod = sldSamplePeriod               'display the sample period
    End If
    Call ChangeParameter("samplerate")                  'change the parameter
    
End Sub

