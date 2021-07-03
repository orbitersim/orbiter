Attribute VB_Name = "DirectInputBas"
Option Explicit

Public Sub InitializeDI()

    'This sub initializes the direct input object, and enables the devices used in the game
    
    Dim caps As DIDEVCAPS                                                   'dim a capabilites flag
    Dim Button() As Long                                                    'dim a variable array
    Dim diDevObjInst As DirectInputDeviceObjectInstance                     'dim a device object instance object
    Dim diEnumObjects As DirectInputEnumDeviceObjects                       'dim an enumeration object for all objects on a deivce
    Dim enumDevice As DirectInputEnumDevices                                'dim an enumeration for all devices
    Dim nI As Integer                                                       'count variable
    Dim i As Integer                                                        'another count variable
    
    Set DI = dx.DirectInputCreate()                                         'create the direct input object
    
    'Keyboard
     Set diKeyBoard = DI.CreateDevice("GUID_SysKeyboard")                   'create a direct input device using a standard keyboard GUID
     diKeyBoard.SetCommonDataFormat DIFORMAT_KEYBOARD                       'set the format of the device to a keyboard
     diKeyBoard.SetCooperativeLevel frmSpaceShooter.hWnd, DISCL_BACKGROUND Or DISCL_NONEXCLUSIVE
     diKeyBoard.Acquire                                                     'acquire the keyboard device
    
    'Joystick

    Set enumDevice = DI.GetDIEnumDevices(DIDEVTYPE_JOYSTICK, DIEDFL_ATTACHEDONLY)
                                                                            'enumerate all attached joysticks
    If enumDevice.GetCount > 0 Then                                         'if the enumeration count is greater than 0 then
        IsJ = True                                                          'there is a joystick attached
        blnJoystickEnabled = True                                           'the joystick is enabled
        Set diJoystick = DI.CreateDevice(enumDevice.GetItem(1).GetGuidInstance)
                                                                            'create a device out of the enumerated GUID
        diJoystick.SetCommonDataFormat DIFORMAT_JOYSTICK                    'set the format of the device to that of a joystick
        diJoystick.SetCooperativeLevel frmSpaceShooter.hWnd, DISCL_BACKGROUND Or DISCL_EXCLUSIVE
                                                                            'set the joysticks cooperative level
        diJoystick.GetCapabilities caps                                     'get the capabilites of the joystick and put it in the caps structure
        If caps.lFlags And DIDC_FORCEFEEDBACK Then                          'if the joystick supports force feedback then
            Set diEnumObjects = diJoystick.GetDeviceObjectsEnum(DIDFT_FFEFFECTTRIGGER Or DIDFT_BUTTON)
                                                                            'enumerate the number of objects on the device that trigger force feedback effects
            ReDim Button(diEnumObjects.GetCount)                            'redimension the number of buttons on the joystick that trigger force feeback
            For nI = 1 To diEnumObjects.GetCount                            'loop through all the objects
              Set diDevObjInst = diEnumObjects.GetItem(nI)                  'set the device object to the enumerated object on the device
              Button(nI) = diDevObjInst.GetOfs                              'the offset for this button on the device
            Next nI
            diJoystick.Acquire                                              'acquire the joystick
            Set diE = diJoystick.GetEffectsEnum(DIEFT_ALL)                  'enum all effects
            For i = 1 To diE.GetCount                                       'loop through all the effects
                If diE.GetName(i) = "RandomNoise" Then                      'if there is a random noise effect available
                    diEF(3).lDuration = 1000000                             'set the duration to 1 second
                    diEF(3).x = 0                                           'make sure the effect is centered in the X position
                    diEF(3).y = 0                                           'make sure the effect is centered in the Y position
                    diEF(3).lGain = 10000                                   'Set the gain to a very strong effect
                    diEF(3).lSamplePeriod = 1000                            'set the rate at which the effect is played back
                    diEF(3).lTriggerRepeatInterval = -1                     'turn off repeating
                    diEF(3).lTriggerButton = -1                             'this effect doesn't have a trigger button
                    Eguid(3) = diE.GetEffectGuid(i)                         'store this GUID for use later
                End If
                If diE.GetName(i) = "OutOfAmmo" Then                        'if there is an out of ammo effect available
                    diEF(1).lDuration = 1000000                             'set the duration to 1 second
                    diEF(1).y = 0                                           'make sure the effect is centered in the Y position
                    diEF(1).x = 0                                           'make sure the effect is centered in the X position
                    diEF(1).lGain = 10000                                   'Set the gain to a very strong effect
                    diEF(1).lSamplePeriod = 1000                            'set the rate at which the effect is played back
                    diEF(1).lTriggerRepeatInterval = -1                     'turn off repeating
                    diEF(1).lTriggerButton = -1                             'this effect doesn't have a trigger button
                    Eguid(2) = diE.GetEffectGuid(i)                         'store this GUID for use later
                End If
                If diE.GetName(i) = "MachineGun" Then                       'if there a machine gun effect available
                    diEF(2).lDuration = -1                                  'the effect has an infinite duration
                    diEF(2).lTriggerButton = Button(1)                      'assign this effect to button 1 on the joystick
                    diEF(2).lGain = 10000                                   'Set the gain to a very strong effect
                    diEF(2).lSamplePeriod = 1000                            'set the rate at which the effect is played back
                    diEF(2).lTriggerRepeatInterval = -1                     'turn off repeating
                    diEF(2).x = 0                                           'make sure the effect is centered in the X position
                    diEF(2).y = 0                                           'make sure the effect is centered in the Y position
                    Eguid(0) = diE.GetEffectGuid(i)                         'store this GUID for use later
                End If
                If diE.GetName(i) = "Constant Force" Then                   'if there is a constant force effect available
                    diEF(0).constantForce.lMagnitude = 100000               'set the magnitude of the effect to all the way up
                    diEF(0).lDuration = 2000000                             'set the duration to 2 seconds
                    diEF(0).x = 0                                           'make sure the effect is centered in the X position
                    diEF(0).y = 0                                           'make sure the effect is centered in the Y position
                    diEF(0).lGain = 10000                                   'Set the gain to a very strong effect
                    diEF(0).lSamplePeriod = 1000                            'set the rate at which the effect is played back
                    diEF(0).lTriggerRepeatInterval = -1                     'turn off repeating
                    diEF(0).lTriggerButton = -1                             'this effect doesn't have a trigger button
                    Eguid(1) = diE.GetEffectGuid(i)                         'store this GUID for use later
                End If
            Next i
        End If
    End If
    
    If Eguid(0) <> "" Then                                                  'if we have a guid for the first effect
        If Eguid(1) <> "" Then                                              'and a guid for the second effect
            If Eguid(2) <> "" Then                                          'and a guid for the third effect
                IsFF = True                                                 'then enable force feedback support
            End If
        End If
    End If

End Sub

Public Sub GetInput()

    'This subroutine gets input from the player using Direct Input. The boolean flag is so that the missile fire routine doesn't
    'loop when a missile is fired
    
    Dim intCount As Integer                                     'standard count variable
    Dim KeyboardState(0 To 255) As Byte                         'Byte array to hold the state of the keyboard
    Dim JoystickState As DIJOYSTATE                             'joystick state type
    Dim lngTime As Long                                         'variable to hold the time
    Dim lngTargetTime As Long                                   'holds a targeted time
    Dim TempTime As Long
    
    diKeyBoard.Acquire                                          'make sure we have acquired the keyboard
    diKeyBoard.GetDeviceState 256, KeyboardState(0)             'get the state of all the keyboard keys
            
    If Not diJoystick Is Nothing And blnJoystickEnabled Then    'if the joystick object has been set, and the joystick is enabled
        diJoystick.Acquire                                      'acquire the joystick
        diJoystick.Poll                                         'poll the joystick
        diJoystick.GetDeviceState Len(JoystickState), JoystickState
                                                                'get the current state of the joystick
    End If
    
    If boolStarted And Not boolGettingInput Then                'if the game has started and we aren't getting input for high scores from the regular form key press events
        
        'Keyboard
        If (KeyboardState(DIK_UP) And &H80) <> 0 Then           'if the up arrow is down
            Ship.YVelocity = Ship.YVelocity - DISPLACEMENT      'the constant displacement is subtracted from the ships Y velocity
        End If
        If (KeyboardState(DIK_DOWN) And &H80) <> 0 Then         'if the down arrow is pressed down
            Ship.YVelocity = Ship.YVelocity + DISPLACEMENT      'the constant displacement is added to the ships Y velocity
        End If
        If (KeyboardState(DIK_LEFT) And &H80) <> 0 Then         'if the left arrow is pressed down
            Ship.XVelocity = Ship.XVelocity - DISPLACEMENT      'the constant displacement is subtracted from the ships X velocity
        End If
        If (KeyboardState(DIK_RIGHT) And &H80) <> 0 Then        'if the right arrow is down
            Ship.XVelocity = Ship.XVelocity + DISPLACEMENT      'the constant displacement is added to the ships X velocity
        End If
        If (KeyboardState(DIK_SPACE) And &H80) <> 0 Then        'if the space bar is down
            FireWeapon                                          'call the sub to fire the weapon
        End If
        If (KeyboardState(DIK_RCONTROL) And &H80) <> 0 And Ship.FiringMissile = False And Ship.NumBombs > 0 Then
            Ship.FiringMissile = True                           'if the control key is pressed
            FireMissile                                         'fire the missile
        End If
        If (KeyboardState(DIK_LCONTROL) And &H80) <> 0 And Ship.FiringMissile = False And Ship.NumBombs > 0 Then
            Ship.FiringMissile = True                           'if the control key is pressed
            FireMissile                                         'fire the missile
        End If
        
        'Joystick
        If Not diJoystick Is Nothing And blnJoystickEnabled Then
                                                                'if the joystick object exists, and the joystick is enabled
            
            Ship.XVelocity = Ship.XVelocity + (JoystickState.x - JOYSTICKCENTERED) * 0.00002136
                                                                'increment the players x velocity by the offset from the joysticks center times the offset factor
            Ship.YVelocity = Ship.YVelocity + (JoystickState.y - JOYSTICKCENTERED) * 0.00002136
                                                                'increment the players x velocity by the offset from the joysticks center times the offset factor
            If JoystickState.buttons(0) And &H80 Then           'if button 0 is pressed
                Call FireWeapon                                 'fire the weapon
            End If
            If JoystickState.buttons(1) And &H80 And Ship.FiringMissile = False And Ship.NumBombs > 0 Then
                                                                'if button 1 is pressed, and the ship isn't firing a missile and the player has missile to fire then
                Ship.FiringMissile = True                       'the ship is now firing a missile
                Call FireMissile                                'fire the missile
            End If
        End If
        
        If (KeyboardState(DIK_BACK) And &H80) <> 0 Then         'if the backspace key is pressed
            If Ship.Invulnerable Then                           'if the ship is invulnerable
                dsInvulnerability.Stop                          'stop playing the invulnerability sound
                TempTime = Ship.InvulnerableTime - dx.TickCount 'capture the current time so the player doesn't lose the amount of time he has left to be invulnerable
            End If
            If Ship.AlarmActive Then dsAlarm.Stop               'if the low shield alarm is playing, stop that
            ' pause music
            If Not (seg Is Nothing) And Not (segstate Is Nothing) Then
                If perf.IsPlaying(seg, segstate) Then                   'Is there a Segment playing?
                    mtTime = perf.GetMusicTime()
                    GetStartTime = segstate.GetStartTime()
                    Call perf.Stop(seg, Nothing, 0, 0)
                End If
            End If

            DrawText 200, 200, "Paused...press Enter to start again", RGB(255, 50, 100)
                                                                'display the pause text
            ddsFront.Flip Nothing, 0                            'flip the surfaces to show the back buffer
            diKeyBoard.GetDeviceState 256, KeyboardState(0)     'Check the keyboard for keypresses
            Do Until ((KeyboardState(DIK_RETURN) And &H80) <> 0) Or _
                ((KeyboardState(DIK_NUMPADENTER) And &H80) <> 0)
                                                                'If the enter key is pressed, exit the loop
                diKeyBoard.GetDeviceState 256, KeyboardState(0) 'Check the keyboard for keypresses
                DoEvents
            Loop
            Do Until lngTargetTime > (lngTime + 200)            'Loop for two hundred milliseconds
                lngTargetTime = dx.TickCount                    'get the elapsed time
                DoEvents                                        'Don't hog the CPU
            Loop
            ' resume music
            If Not (seg Is Nothing) And Not (segstate Is Nothing) Then
                Offset = mtTime - GetStartTime + Offset + 1
                Call seg.SetStartPoint(Offset)
                Set segstate = perf.PlaySegment(seg, 0, 0)
            End If
            If Ship.Invulnerable Then                           'if the ship was invulnerable
                dsInvulnerability.Play DSBPLAY_LOOPING          'start the invulenrability wave again
                Ship.InvulnerableTime = TempTime + dx.TickCount 'the amount of time the player had left is restored
            End If
            If Ship.AlarmActive Then                            'if the low shield alarm was playing
                dsAlarm.Play DSBPLAY_LOOPING                    'start it again
            End If
        End If
    Else                                                        'The game has not started yet
        If ((KeyboardState(DIK_RETURN) And &H80) <> 0) Or _
            ((KeyboardState(DIK_NUMPADENTER) And &H80) <> 0) Then
                                                                'if the enter key is pressed then
            boolStarted = True                                  'the game has started
            If Not ef(2) Is Nothing And IsFF = True Then ef(2).Download
                                                                'download the force feedback effect for firing lasers
            FadeScreen                                          'fade the current screen
            StartIntro                                          'show the intro text
            byteLives = 3                                       'Set lives
            intShields = 100                                    'Set shields
            byteLevel = 1                                       'level 1 to start with
            SectionCount = 999                                  'start at the first section and count down
            LoadLevel CStr(byteLevel)                           'load level 1
            PlayMidi "level1.mid"                               'start the level 1 midi
            For intCount = 0 To UBound(StarDesc)                'reset all the stars
                StarDesc(intCount).Exists = False               'they no longer exist
            Next
        ElseIf (KeyboardState(DIK_ESCAPE) And &H80) <> 0 Then   'if the escape key is pressed,
            DoCredits                                           'Show the credits
            EndGame                                             'Call sub to reset all variables
            End                                                 'Exit the application
        End If
    End If
    
End Sub

