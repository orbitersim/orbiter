Attribute VB_Name = "SpaceShooter"
Option Explicit

'*****************************************************************************************************************
'SPACE SHOOTER 2000!
'Main programming by Adam "Gollum" Lonnberg
'Force feedback support by Dominic "DirectX" Riccetti
'*****************************************************************************************************************

'Declare all external Win32 API calls, types, and constants

Public Declare Function CreateCompatibleDC Lib "gdi32" (ByVal hdc As Long) As Long
                                        'used to create a DC for reading in and creating a direct draw surface from other formats besides .bmp
Public Declare Function SelectObject Lib "gdi32" (ByVal hdc As Long, ByVal hObject As Long) As Long
                                        'used to select the dc into an object
Public Declare Function StretchBlt Lib "gdi32" (ByVal hdc As Long, ByVal x As Long, ByVal y As Long, ByVal nWidth As Long, ByVal nHeight As Long, ByVal hSrcDC As Long, ByVal xSrc As Long, ByVal ySrc As Long, ByVal nSrcWidth As Long, ByVal nSrcHeight As Long, ByVal dwRop As Long) As Long
                                        'used to blit the bitmap from a dc to a direct draw surface
Public Declare Function DeleteDC Lib "gdi32" (ByVal hdc As Long) As Long
                                        'used to destroy a dc
Public Declare Function ShowCursor Lib "user32" (ByVal bShow As Long) As Long
                                        'Disables/Enables the cursor
Public Declare Function IntersectRect Lib "user32" (ByRef r As RECT, ByRef r2 As RECT, ByRef r3 As RECT) As Long
                                        'Used for collision detection
Public Declare Function GetAsyncKeyState Lib "user32" (ByVal vKey As Long) As Integer
                                        'Used to get input from the keyboard via Win32
Public Type TEXTMETRIC                  'Structure needed to get detailed font information
        tmHeight As Long
        tmAscent As Long
        tmDescent As Long
        tmInternalLeading As Long
        tmExternalLeading As Long
        tmAveCharWidth As Long
        tmMaxCharWidth As Long
        tmWeight As Long
        tmOverhang As Long
        tmDigitizedAspectX As Long
        tmDigitizedAspectY As Long
        tmFirstChar As Byte
        tmLastChar As Byte
        tmDefaultChar As Byte
        tmBreakChar As Byte
        tmItalic As Byte
        tmUnderlined As Byte
        tmStruckOut As Byte
        tmPitchAndFamily As Byte
        tmCharSet As Byte
End Type
Public Declare Function GetTextMetrics Lib "gdi32" Alias "GetTextMetricsA" (ByVal hdc As Long, lpMetrics As TEXTMETRIC) As Long
                                        'Used to center the text
Public Const VK_ESCAPE = &H1B           'used for Win32 key detection
Public Const VK_RETURN = &HD            'used for Win32 key detection
Public Const SRCCOPY = &HCC0020         'used for copying a bitmap via GDI
'----------------------------------------------------------------------------------------

                                        
'Define all UDT's and constants
Public Const SCREENWIDTH = 640          'Width for the display mode
Public Const SCREENHEIGHT = 480         'Height for the display mode
Public Const TRANSPARENTINDEX = 8       'Index of the transparency color
Public Const TITLEINDEX = 11            'Index of the title color to fade the logo in and out
Public Const JOYSTICKCENTERED = 32768   'Center value of the joystick

Const SHIELD = &H0                      'Constant for the shield powerup
Const WEAPON = &H20                     'Constant for the weapon powerup
Const BOMB = &H40                       'Constant for the bomb powerup
Const INVULNERABILITY = &H60            'Constant for the invulenrability powerup

Public Const SHIPWIDTH = 35             'Width of the players ship
Public Const SHIPHEIGHT = 60            'Height of the players ship
Public Const LASERSPEED = 9.5           'Speed of the laser fire
Public Const LASER1WIDTH = 4            'Width of the stage 1 laser fire
Public Const LASER1HEIGHT = 8           'Height of the stage 1 laser fire
Public Const LASER2WIDTH = 8            'Width of the stage 2 laser fire
Public Const LASER2HEIGHT = 8           'Height of the stage 2 laser fire
Public Const LASER3HEIGHT = 5           'Height of the stage 3 laser fire
Public Const LASER3WIDTH = 17           'Width of the stage 3 laser fire
Public Const NUMOBSTACLES = 150         'The maximum number of second-layer objects that can appear
Public Const POWERUPHEIGHT = 17         'Height of the powerups
Public Const POWERUPWIDTH = 16          'Width of the powerups
Public Const NUMENEMIES = 100           'How many enemies can appear on the screen at one time
Public Const XMAXVELOCITY = 3           'Maximum X velocity of the ship
Public Const YMAXVELOCITY = 3           'Maximum Y velocity of the ship
Public Const DISPLACEMENT = 0.7         'Rate at which the velocity changes
Public Const FRICTION = 0.18            'The amount of friction applied in the universe
Public Const MAXMISSILEVELOCITY = 3.1   'The maximum rate a missile can go
Public Const MISSILEDIMENSIONS = 4      'The width and height of the missile
Public Const TARGETEDFIRE = 1           'The object aims at the player
Public Const NONTARGETEDFIRE = 0        'The object just shoots straight
Public Const CHASEOFF = 0               'The object doesn't follow the players' X coordinates
Public Const CHASESLOW = 1              'The object does follow the players' X coordinates, but slowly
Public Const CHASEFAST = 2              'The object does follow the players' X coordinates, but fast
Public Const EXTRALIFETARGET = 250000   'If the player exceeds this value he gets an extra life

Type typeWeaponDesc                     'UDT to define the weapon object
    x As Single                         'X position of the weapon
    y As Single                         'Y position of the weapon
    XVelocity As Single                 'The X velocity of the weapon
    YVelocity As Single                 'The Y velocity of the weapon
    Damage As Byte                      'How many points of damage this weapon does
    StillColliding As Boolean           'Flag that is set when the weapon has entered a target, and is still within it
    Width As Integer                    'Width of the weapon beam in pixels
    Height As Integer                   'Height of the weapon beam in pixels
    Exists As Boolean                   'Set to true if the weapon exists on screen
    TargetIndex As Byte                 'For guided weapons, sets the enemy target index
    TargetSet As Boolean                'Flag that is set once a target has been selected for the guided weapon
End Type

Type typeBackGroundDesc                 'UDT to define any small background objects (stars, enemies, other objects)
    FileName As String                  'Name of the file
    x As Single                         'X position of the B.G. object
    y As Single                         'Y position of the B.G. object
    XVelocity As Single                 'X velocity of the enemy ship
    Speed As Single                     'The speed the object scrolls
    ChaseValue As Byte                  'Flag that is set to CHASEOFF, CHASESLOW, or CHASEFAST. If the flag isn't CHASEOFF, then it sets whether or not the enemy "follows" the players movement, and if the chase rate is fast or slow
    Exists As Boolean                   'Determines if the object exists
    HasDeadIndex As Boolean             'Toggles whether or not this object has a bitmap that will be displayed when it is destroyed
    DeadIndex As Integer                'Index of picture to display when this object has been destroyed
    ExplosionIndex As Byte              'The index of which explosion gets played back when this enemy is destroyed
    TimesHit As Byte                    'Number of times this enemy has been hit
    TimesDies As Byte                   'Max number of hits when enemy dies
    CollisionDamage As Byte             'If the player collides with this enemy, the amount of damage it does
    Score As Integer                    'The score added to the player when this is destroyed
    Index As Byte                       'Index of the container for this bitmap -or- How many frames the bitmap has existed
    Frame As Byte                       'The current frame number
    NumFrames As Byte                   'The number of frames the bitmap contains/How many frames the bitmap should exist
    FrameDelay As Byte                  'Used to delay the incrementing of frames to slow down frame animation, if needed
    FrameDelayCount As Byte             'Used to store the current frame delay number count
    Width As Integer                    'the width of one frame
    Height As Integer                   'the height of one frame
    DoesFire As Boolean                 'Does this object fire a weapon?
    FireType As Byte                    'The style of fire the object uses (targeted or non-targeted)
    HasFired As Boolean                 'Has this enemy fired its' weapon
    Invulnerable As Boolean             'Can this object be hit with weapon fire?
    XFire As Single                     'X position of the weapon fire
    YFire As Single                     'Y position of the weapon fire
    FireFrame As Byte                   'Frame of the weapon fire
    FireFrameCount As Byte              'Used to indicate when it is time to change the animation frame of the enemy fire
    TargetX As Single                   'X vector of the weapon fire direction
    TargetY As Single                   'Y vector of the weapon fire direction
    Solid As Boolean                    'Toggles whether this item needs to be blitted transparent or not
End Type

Type typeShipDesc                       'UDT to define the players' ship bitmap
    PowerUpState As Byte                'Determines how many levels of power-ups the player has
    Invulnerable As Boolean             'Determines whether or not the player is invulnerable
    InvulnerableTime As Long            'Used to keep track of the amount of time the player has left when invulnerable
    x As Single                         'X of the ship
    y As Single                         'Y of the ship
    XOffset As Integer                  'X Offset of the animation frame
    YOffset As Integer                  'Y Offset of the animation frame
    XVelocity As Single                 'X velocity of the ship
    YVelocity As Single                 'Y velocity of the ship
    NumBombs As Integer                 'the number of super bombs the player has
    AlarmActive As Boolean              'Determines if the alarm sound is being played so it can be turned off temporarily when the game is paused
    FiringMissile As Boolean            'Toggles whether the ship is currently firing a missile
End Type

Type typeBackgroundObject               'UDT to define background pictures
    x As Single                         'X position of the object
    y As Single                         'Y position of the object
    Width As Integer                    'Width of the B.G. object
    Height As Integer                   'Height of the B.G. object
    PathName As String                  'Path to the bitmap of the background object
End Type

Type typeSectionInfo                    'UDT to define the section information for each level.
    Slot(125) As Byte                   'Each section contains 125 slots. The value of each slot refers
End Type                                'to the index of the object contained in that slot. -1 means no object is in this slot.

Public SectionInfo(999) As typeSectionInfo
                                        'There are 1000 sections to a level
Public ObstacleInfo(999) As typeSectionInfo
                                        'There are 1000 obstacle sections to a level
                                
'dx
Public dx As New DirectX7
'--------------------------------------------------------------------------

'DirectDraw Section
Public dd As DirectDraw7                            'DirectDraw object
Public ddsdFront As DDSURFACEDESC2                  'Front surface description
Public ddsFront As DirectDrawSurface7               'Front buffer
Public ddsBack As DirectDrawSurface7                'Back buffer
Public ddfxBack As DDBLTFX                          'Effects for Blt for back buffer
Public ddsShip As DirectDrawSurface7                'Ship bitmap
Public ddsLaser As DirectDrawSurface7               'Laser 1 laser surface
Public ddsLaser2R As DirectDrawSurface7             'Right diagonal laser
Public ddsLaser2L As DirectDrawSurface7             'Left diagonal laser
Public ddsLaser3 As DirectDrawSurface7              'Laser 3 laser surface
Public ddsGuidedMissile As DirectDrawSurface7       'Guided Missile
Public ddsStar As DirectDrawSurface7                'Star
Public ddsEnemyFire As DirectDrawSurface7           'Enemy laser fire
Public ddsPowerUp As DirectDrawSurface7             'Power Up dd surface
Public ddsShieldIndicator As DirectDrawSurface7     'Shield indicator bitmap
Public ddCaps As DDSCAPS2                           'Direct draw surface capabilities
Public ddsTitle As DirectDrawSurface7               'Title Screen surface
Public ddsHit As DirectDrawSurface7                 'Direct draw surface for small explosions
Public ddsBackgroundObject(7) As DirectDrawSurface7 'Background objects
Public ddsEnemyContainer(13) As DirectDrawSurface7  'Enemy surface container
Public ddsExplosion(1) As DirectDrawSurface7        'Explosion surfaces
Public ddsObstacle(40) As DirectDrawSurface7        'Obstacle direct draw surfaces
Public ddsDisplayBomb As DirectDrawSurface7         'Bomb direct draw surface
Public ddsInvulnerable As DirectDrawSurface7        'Invulnerable bitmap surface
'-------------------------------------------------------------------------

'DirectSound Section
Public DS As DirectSound                            'Direct Sound object
Public dsPrimaryBuffer As DirectSoundBuffer         'Primary direct sound buffer
Public dsLaser2 As DirectSoundBuffer                'stage 2 laser fire buffer
Public dsLaser2Duplicate(3) As DirectSoundBuffer    'duplicate stage 2 laser fire
Public dsLaser As DirectSoundBuffer                 'stage 1 laser fire
Public dsExplosion As DirectSoundBuffer             'explosion sound effect
Public dsExplosionDuplicate(2) As DirectSoundBuffer 'duplicate explosion effects
Public dsPowerUp As DirectSoundBuffer               'power up sound effect buffer
Public dsMissile As DirectSoundBuffer               'missile sound effect buffer
Public dsEnergize As DirectSoundBuffer              'sound effect for when the player materializes
Public dsAlarm As DirectSoundBuffer                 'low shield alarm
Public dsEnemyFire As DirectSoundBuffer             'enemy fire direct sound buffer
Public dsEnemyFireDuplicate(3) As DirectSoundBuffer 'duplicate enemy sound buffers
Public dsNoHit As DirectSoundBuffer                 'player hits an object that isn't destroyed
Public dsNoHitDuplicate(3) As DirectSoundBuffer     'duplicate sounds for an object that isn't destroyed
Public dsPulseCannon As DirectSoundBuffer           'sound for the pulse cannon
Public dsPlayerDies As DirectSoundBuffer            'sound for when the player dies
Public dsInvulnerability As DirectSoundBuffer       'sound for when the player is invulnerable
Public dsInvPowerDown As DirectSoundBuffer          'sound for when the invulnerability wears off
Public dsExtraLife As DirectSoundBuffer             'sound for when the player gets an extra life
Public DSEnemyFireIndex As Integer                  'Keeps track of how many sounds are playing at once
'----------------------------------------------------------------------------

'Direct Input
Public DI As DirectInput                            'DirectInput object
Public diE As DirectInputEnumEffects                'Enumeration object for force feedback
Public ef(3) As DirectInputEffect                   'Direct input effect object
Public diEF(3) As DIEFFECT                          'Direct input effect type to describe the effect
Public diKeyBoard As DirectInputDevice              'Keyboard device object
Public diJoystick As DirectInputDevice              'Joystick device object
Public Eguid(3) As String                           'Effect GUID
Public IsJ As Boolean                               'Flag that is set if a joystick is present
Public IsFF As Boolean                              'Flag that is set if force feedback is present
'----------------------------------------------------------------------------

'Direct Music section
Public perf As DirectMusicPerformance                  'DirectMusic Performance object
Public seg As DirectMusicSegment                        'DirectMusic Segment
Public segstate As DirectMusicSegmentState              'DirectMusic Segment State
Public loader As DirectMusicLoader                      'DirectMusic Loader
'----------------------------------------------------------------------------

'Palettes variables
Public DDPalette As DirectDrawPalette               'Create a direct draw palette
Dim boolColorReset As Boolean                       'Tells the game to reset color index 10, which is used to cycle the title's color
Dim intColorDirection As Integer                    'Which direction that the color effect for the title is incremented
'-------------------------------------------------------------------------

'Variables to handle graphics
Public boolBackgroundExists As Boolean              'Boolean to determine if a background object exists
Public sngBackgroundX As Single                     'X coordinate of the background image
Public sngBackgroundY As Single                     'Y coordinate of the background image
Public intObjectIndex As Integer                    'The index number of the object
Public intShipFrameCount As Integer                 'The frame number of the players ship
Public Ship As typeShipDesc                         'Set up the players ship
Public LaserDesc(14) As typeWeaponDesc              'Set up an array for 15 laser blasts
Public Laser2RDesc(6) As typeWeaponDesc             'Set up an array for 7 right diagonal laser blasts
Public Laser2LDesc(6) As typeWeaponDesc             'Set up an array for 7 left diagonal laser blasts
Public Laser3Desc(2) As typeWeaponDesc              'Set up an array for 3 laser 3 blasts
Public GuidedMissile(2) As typeWeaponDesc           'Set up an array for 3 guided missiles
Public StarDesc(49) As typeBackGroundDesc           'Set up an array for 50 stars
Public EnemyDesc(NUMENEMIES) As typeBackGroundDesc  'Set up an array for all enemies
Public EnemyContainerDesc(13) As typeBackGroundDesc 'Set up an array for the enemy containers descriptions
Public ObstacleContainerInfo(40) As typeBackGroundDesc
                                                    'Background objects container
Public ObstacleDesc(NUMOBSTACLES) As typeBackGroundDesc
                                                    'Background objects
Public BackgroundObject(7) As typeBackgroundObject  'Set up an array for 8 large background pictures
Public PowerUp(3) As typeBackGroundDesc             'Set up an array for the power ups
Public HitDesc(19) As typeBackGroundDesc            'Set an array for small explosions when an object is hit
Public ExplosionDesc(80) As typeBackGroundDesc      'Array for explosions
Public EmptyRect As RECT                            'Structure for empty rectangles
'-------------------------------------------------------------------------

'Player Info
Public byteLives As Byte                            'Number of lives the player has left
Public intShields As Integer                        'The amount of shields the player has left
Public intEnemiesKilled As Integer                  'The number of enemies the player has destroyed. For every 30 enemies destroyed, a powerup will appear
Public lngScore As Long                             'Players score
Public lngNextExtraLifeScore As Long                'The next score the player gets an extra life at
Public lngNumEnemiesKilled As Long                  'The number of enemies killed
Public lngTotalNumEnemies As Long                   'The total number of enemies on the level
Public byteLevel As Integer                         'Players level
Public strName As String                            'Players name when they get a high score
'-------------------------------------------------------------------------

'The rest are miscellaneous variables

Public SectionCount As Integer                      'Keeps track of what section the player is on
Public FrameCount As Long                           'keeps track of the number of accumulated frames. When it reaches 20, a new section is added
Public boolStarted As Boolean                       'Determines whether a game is running or not
Private lngHighScore(9) As Long                     'Keeps track of high scores
Private strHighScoreName(9) As String               'Keeps track of high score names
Private byteNewHighScore As Byte                    'index of a new high score to paint the name color differently
Public strBuffer As String                          'Buffer to pass keypresses
Public boolEnterPressed As Boolean                  'Flag to determine if the enter key was pressed
Public boolGettingInput As Boolean                  'Flag to see if we are getting input from the player
Public boolFrameRate As Boolean                     'Flag to toggle frame rate display on and off
Private strLevelText As String                      'Stores the associated startup text for the level
Public blnInvulnerable As Boolean                   'Toggles invulnerability
Public blnJoystickEnabled As Boolean                'Toggles joystick on or off
Public blnMidiEnabled As Boolean                    'Toggles Midi music on or off
Public boolMaxFrameRate As Boolean                  'Removes all frame rate limits
Public mtTime As Long                              'DirectMusic times
Public Offset As Long
Public GetStartTime As Long

Private Sub CheckScore()
    
    'This routine checks the current score, and determines if it has gone past the extra life threshold.
    'If it has, then display that the player has gained an extra life, and give the player an extra life
    
    Static blnExtraLifeDisplay As Boolean                                   'Flag that is set if an extra life message needs to be displayed
    Static lngTargetTime As Long                                            'Variable used to hold the targeted time
    
    If lngScore > lngNextExtraLifeScore Then                                'If the current score is larger than the score needed to get an extra life
        lngNextExtraLifeScore = lngNextExtraLifeScore + EXTRALIFETARGET     'Increase the extra life target score
        dsExtraLife.SetCurrentPosition 0                                    'Set the extra life wave position to the beginning
        dsExtraLife.Play DSBPLAY_DEFAULT                                    'Play the extra life wave file
        blnExtraLifeDisplay = True                                          'Toggle the extra life display flag to on
        lngTargetTime = dx.TickCount + 3000                                 'Set the end time for displaying the extra life message
        byteLives = byteLives + 1                                           'increase the players life by 1
    End If
    
    If lngTargetTime > dx.TickCount And blnExtraLifeDisplay Then            'As long as the target time is larger than the current time, and the extra life display flag is set
        DrawText FindStringCenter("Extra Life!!!"), 250, "Extra Life!!!", RGB(255, 50, 50)
                                                                            'Display the extra life message
    Else
        blnExtraLifeDisplay = False                                         'Otherwise, if we have gone past the display duration, turn the display flag off
    End If
        
End Sub

Public Function FindStringCenter(strInput As String) As Long
    
    'This sub finds the approximate center of a string
    
    Dim hdc As Long                             'Variable to store the device context handle
    Dim ReturnResults As TEXTMETRIC             'textmetric type to hold the info we collect
    
    hdc = ddsBack.GetDC                         'get the device context for the back buffer
    GetTextMetrics hdc, ReturnResults           'call the GetTextMetrics api to gain information about the currently selected font
    ddsBack.ReleaseDC hdc                       'release the handle
    
    FindStringCenter = (SCREENWIDTH \ 2) - ((ReturnResults.tmAveCharWidth * Len(strInput)) \ 2)
                                                'assign the new X position of the string to the function after we find the center of the string
                                                'in relationship to the screen width and the average width of all characters in the font, and
                                                'add the offset of the length of the string
End Function

Public Sub Main()

    'This is the main sub for the game. From here everything branches out to all the subs that handle collisions,
    'enemies, player, weapon fire, sounds, level updating, etc.
    
    Dim PalInfo(0) As PALETTEENTRY                                      'Variable that holds a type PALETTEENTRY structure...used to cycle colors for title
    Dim lngTargetTick As Long                                           'Variable to store the targeted tick count time
    Dim lngFrameRateTick As Long                                        'Variable to store the frame rate tick
    Dim lngStartTime As Long                                            'stores the start time of a frame rate count
    Dim lngCurrentTime As Long                                          'stores the current tick for frame rate count
    Dim intFinalFrame As Integer                                        'holds the total number of frames in a second
    Dim intFrameCount As Integer                                        'keeps track of how many frames have elapsed
    Dim EmptyRect As RECT                                               'Empty rectangle structure
    
    On Error GoTo ErrorHandler
    
    ChDir App.Path                                                      'Change the working directory to this one
    Load frmSpaceShooter                                                'Make sure we can get a window handle by loading the form
    InitializeStartup                                                   'Do the startup routines
    intColorDirection = 5                                               'Set the intitial color direction to 3
    GetHighScores                                                       'Call the sub to load the high scores
    lngNextExtraLifeScore = EXTRALIFETARGET                             'Initialize the extra life score to 100,000
    Do                                                                  'The main loop of the game.
        lngTargetTick = dx.TickCount                                    'Store the current time of the loop
        GetInput                                                        'call sub that checks for player input
        ddsBack.BltColorFill EmptyRect, 0                               'fill the back buffer with black
        UpdateBackground                                                'Update the background bitmaps
        UpdateStars                                                     'Update the stars
        If boolStarted And boolGettingInput = False Then                'If the game has started, and we are not getting high score input from the player
            If boolColorReset = False Then                              'If the palette index 10 needs to be reset, reset it
                With PalInfo(0)                                         '(This is necessary since we use index 11 to cycle the space shooter title)
                    .blue = 204
                    .flags = 0
                    .green = 0
                    .red = 51
                End With
                DDPalette.SetEntries 0, TITLEINDEX, PalInfo()           'Set index the index back to our custom palette color
                boolColorReset = True                                   'Set the flag indicating we reset the palette
            End If
            FrameCount = FrameCount + 1                                 'Keep track of the frame increment
            If FrameCount >= 20 Then                                    'When 20 frames elapsed
                SectionCount = SectionCount - 1                         'Reduce the section the player is on
                UpdateLevels                                            'Update the section
                FrameCount = 0                                          'Reset the frame count
            End If
            UpdateObstacles                                             'Update the back layer of objects
            UpdateEnemys                                                'Move and draw the enemys
            UpdatePowerUps                                              'Move and draw the power ups
            UpdateHits                                                  'Update the small explosions
            UpdateWeapons                                               'Move and draw the weapons
            UpdateExplosions                                            'Update any large explosions
            UpdateShip                                                  'Move and draw the ship
            If Ship.Invulnerable Then UpdateInvulnerability             'if the player is invulnerable, then update the invulnerability effect
            CheckForCollisions                                          'Branch to collision checking subs
            UpdateShields                                               'Branch to sub that paints shields
            UpdateBombs
            DrawText 30, 10, "Score: " & Format$(lngScore, "###,###,###"), RGB(149, 248, 153)
                                                                        'Display the score
            DrawText 175, 10, "Lives: " & byteLives, RGB(149, 248, 153) 'Display lives left.
            DrawText 560, 10, "Level: " & byteLevel, RGB(149, 248, 153) 'Display the current level
            CheckScore
        ElseIf boolStarted = False And boolGettingInput = False Then    'If we haven't started, and we aren't getting high score input from the player
            ShowTitle                                                   'Show the title screen with high scores and directions
        ElseIf boolGettingInput Then                                    'If we are getting input from the player, then
            CheckHighScore                                              'call the high score subroutine
        End If
        If boolFrameRate Then                                           'If the frame rate display is toggled
            intFrameCount = intFrameCount + 1                           'increment the frame count
            lngCurrentTime = dx.TickCount                               'get the current tick count
            If lngCurrentTime > lngStartTime + 1000 Then                'if one second has elapsed
                intFinalFrame = intFrameCount                           'the total number of frames is stored in intFinalFrame
                intFrameCount = 0                                       'reset the frame count
                lngStartTime = dx.TickCount                             'get a new start time
            End If
            DrawText 30, 30, "FPS: " & intFinalFrame, RGB(255, 255, 255)
                                                                        'display the frame rate
        End If
        
        If Not boolMaxFrameRate Then
            Do Until dx.TickCount - lngTargetTick > 18                  'Make sure the game doesn't get out of control
            Loop                                                        'speed-wise by looping until we reach the targeted frame rate
        Else
            DrawText 30, 45, "MaxFrameRate enabled", RGB(255, 255, 255) 'Let the player know there is no frame rate limitation
        End If
        ddsFront.Flip Nothing, 0                                        'Flip the front buffer with the back
        
        DoEvents                                                        'Let the system process stuff
        If boolStarted And GetAsyncKeyState(VK_ESCAPE) And &H8000 Then  'If the game has started, and the player presses escape
            If IsFF = True Then ef(2).Unload                            'unload the laser force feedback effect
            Call ResetGame                                              'call the sub to reset the game variables
        End If
                                                                        'If the escape key is preseed, reset the game and go back to the title screen
    Loop While True                                                     'keep looping endlessly
 
    Exit Sub

ErrorHandler:                                                           'Error handling
    
    If Err.Number = DDERR_NOEXCLUSIVEMODE Then                          'If the game can't set exclusive mode, or loses it
        EndGame                                                         'set all variables to nothing
        End                                                             'end the program
    ElseIf Err.Number = DDERR_SURFACELOST Then                          'if any surfaces are lost
        EndGame                                                         'set all variables to nothing
        End                                                             'end the program
    End If
    
End Sub
Public Sub ResetGame()
    
    'This sub resets all the variables in the game that need to be reset when the game is started over.
    
    Dim intCount As Integer                         'variable for looping
    
    For intCount = 0 To UBound(EnemyDesc)           'loop through all the enemies and
        EnemyDesc(intCount).Exists = False          'the enemies no longer exist
        EnemyDesc(intCount).HasFired = False        'the enemies' wepaons no longer exist
    Next
    For intCount = 0 To UBound(GuidedMissile)       'loop through all the players guided missiles
        GuidedMissile(intCount).Exists = False      'they no longer exist
    Next
    For intCount = 0 To UBound(ObstacleDesc)        'make all the obstacles non-existent
        ObstacleDesc(intCount).Exists = False
        ObstacleDesc(intCount).HasFired = False
    Next
    For intCount = 0 To UBound(ExplosionDesc)       'Make sure that no explosions get left over
        ExplosionDesc(intCount).Exists = False
    Next
    For intCount = 0 To UBound(PowerUp)
        PowerUp(intCount).Exists = False            'if there are any power ups currently on screen, get rid of them
    Next
    intShields = 100                                'shields are at full again
        
    FadeScreen                                      'Fade the screen to black
    intShields = 100                                'shields are at 100%
    With Ship
        .x = 300                                    'center the ships' X
        .y = 300                                    'and Y
        .PowerUpState = 0                           'the player is back to no powerups
        .PowerUpState = 0                           'no powerups
        .NumBombs = 0                               'the player has no bombs
        .Invulnerable = False                       'the player is no longer invulnerable if he was
        .AlarmActive = False                        'the low shield alarm no longer needs to be flagged
        .FiringMissile = False                      'the ship is not firing a missile
    End With
    dsInvulnerability.Stop                          'make sure the invulnerability sound effect is not playing
    dsAlarm.Stop                                    'make sure the alarm sound effect is not playing
    boolStarted = False                             'the game hasn't been started
    SectionCount = 999                              'start at the beginning of the level
    byteLevel = 1                                   'player is at level 1 again
    byteLives = 3                                   'the player has 3 lives left
    boolBackgroundExists = False                    'there is no background picture
    CheckHighScore                                  'call the sub to see if the player got a high score

End Sub

Private Sub UpdateBombs()
        
    'This sub updates the animated bombs that appear at the top of the screen when the player gets one
    
    Dim TempY As Long                               'Temporary Y coordinate
    Dim TempX As Long                               'Temporary X coordinate
    Dim SrcRect As RECT                             'Source rectangle structure
    Dim XOffset As Long                             'Offset for the X coordinate
    Dim YOffset As Long                             'Offset for the Y coordinate
    Dim intCount As Integer                         'Count variable
    Static BombFrame As Integer                     'Keeps track of which animation frame the bombs are one
    Static BombCount As Integer                     'The number of game frames that elapse before advancing the animation frame
    
    If Ship.NumBombs > 0 Then                       'if the player does have a bomb
        BombCount = BombCount + 1                   'increment the bomb frame count
        If BombCount = 2 Then                       'if we go through 2 game frames
            BombCount = 0                           'reset the bomb frame count
            BombFrame = BombFrame + 1               'increment the bomb frame
            If BombFrame > 9 Then BombFrame = 0     'there are 10 frames of animation for the bomb, if the count reaches
                                                    'the end of the number of frames, reset it to the first frame
        End If
        TempY = BombFrame \ 4                       'get the temporary Y coordinate of the frame
        TempX = BombFrame - (TempY * 4)             'get the temporary X coordinate of the frame
        XOffset = CLng(TempX * 20)                  'offset the rectangle by the X coordinate * the width of the frame
        YOffset = CLng(TempY * 20)                  'offset the rectangle by the Y coordinate * the height of the frame
        With SrcRect                                'define the source rectangle of the bomb
            .Top = YOffset                          'define the top coordinate of the rectangle
            .Bottom = .Top + 20                     'the bottom is the top + the height of the bomb
            .Left = XOffset                         'define the left coordinate of the bomb
            .Right = .Left + 20                     'the right is the left + the width of the bomb
        End With
        For intCount = 1 To Ship.NumBombs           'loop through the number of bombs the player has
            ddsBack.BltFast 250 + (intCount * 22), 5, ddsDisplayBomb, SrcRect, DDBLTFAST_WAIT Or DDBLTFAST_SRCCOLORKEY
                                                    'draw as many bombs as the player has, adding a two pixel space
                                                    'between them
        Next
    End If
    
End Sub

Private Sub UpdateInvulnerability()

    'This sub updates the invulnerability animation, and starts and stops the sound that goes with it
    
    Dim TempX As Long                                               'Temporary X variable
    Dim TempY As Long                                               'Temporary Y variable
    Dim XOffset As Long                                             'Offset of the rectangle
    Dim YOffset As Long                                             'Offset of the rectangle
    Dim SrcRect As RECT                                             'Source rectangle
    Static intInvFrameCount As Integer                              'Keep track of what animation frame the animation is on
    Static blnInvWarning As Boolean                                 'Flag that is set if it is time to warn the player that the invulnerability is running out
    Static intWarningCount As Integer                               'Keep track of how many times the player has been warned
    
    If dx.TickCount > Ship.InvulnerableTime Then                    'If the amount of invulenrability exceeds the time alloted to the player
        Ship.Invulnerable = False                                   'The ship is no longer invulnerable
        intInvFrameCount = 0                                        'The animation is reset to the starting frame
        dsInvulnerability.Stop                                      'Stop playing the invulnerable sound effect
        dsInvPowerDown.Play DSBPLAY_DEFAULT                         'Play the power down sound effect
        blnInvWarning = False                                       'No longer warning the player
        intWarningCount = 0                                         'Reset the warning count
    Else                                                            'Otherwise, the ship is invulnerable
        If (Ship.InvulnerableTime - dx.TickCount) < 3000 Then       'If there are only three seconds left
            blnInvWarning = True                                    'Toggle the warning flag to on
        End If
            
        If blnInvWarning Then                                       'If the player is being warned
            intWarningCount = intWarningCount + 1                   'Increment the warning count
            If intWarningCount > 30 Then intWarningCount = 0        'If the warning count goes through 30 frames, reset it
            If intWarningCount < 15 Then                            'If the warning count is less than 30 frames
                dsInvulnerability.Play DSBPLAY_LOOPING              'Play the invulnerability sound effect
                If intInvFrameCount > 49 Then intInvFrameCount = 0  'If the animation goes past the maximum number of frames, reset it
                TempY = intInvFrameCount \ 4                        'Calculate the left of the animation
                TempX = intInvFrameCount - (TempY * 4)              'Calculate the top of the animation
                XOffset = CLng(TempX * SHIPWIDTH)                   'Calculate the right of the animation
                YOffset = CLng(TempY * SHIPHEIGHT)                  'Calculate the bottom of the animation
                intInvFrameCount = intInvFrameCount + 1             'Increment the frame count
                With SrcRect                                        'Input the rectangle values
                    .Top = YOffset
                    .Bottom = .Top + SHIPHEIGHT
                    .Left = XOffset
                    .Right = .Left + SHIPWIDTH
                End With
                ddsBack.BltFast Ship.x, Ship.y, ddsInvulnerable, SrcRect, DDBLTFAST_WAIT Or DDBLTFAST_SRCCOLORKEY
                                                                    'Blit the animation frame
            Else
                dsInvulnerability.Stop                              'If we are above 15 frames of animation, stop playing the invulnerability sound effect
            End If
        Else                                                        'Otherwise, the player is not in warning mode
            If intInvFrameCount > 49 Then intInvFrameCount = 0      'If the animation goes past the maximum number of frames, reset it
            TempY = intInvFrameCount \ 4                            'Calculate the left of the animation
            TempX = intInvFrameCount - (TempY * 4)                  'Calculate the top of the animation
            XOffset = CLng(TempX * SHIPWIDTH)                       'Calculate the right of the animation
            YOffset = CLng(TempY * SHIPHEIGHT)                      'Calculate the bottom of the animation
            intInvFrameCount = intInvFrameCount + 1                 'Increment the frame count
            With SrcRect                                            'Input the rectangle values
                .Top = YOffset
                .Bottom = .Top + SHIPHEIGHT
                .Left = XOffset
                .Right = .Left + SHIPWIDTH
            End With
            ddsBack.BltFast Ship.x, Ship.y, ddsInvulnerable, SrcRect, DDBLTFAST_WAIT Or DDBLTFAST_SRCCOLORKEY
                                                                    'Blit the animation frame
        End If
        dsInvulnerability.SetPan (Ship.x - (SCREENWIDTH \ 2)) * 3   'If we are above 15 frames of animation, stop playing the invulnerability sound effect
    End If
    
End Sub

Private Sub UpdateLevels()

    'This sub checks to see if the end of the game has been reached, increments the levels if the end of a level is
    'reached, and also initializes new enemies and obstacles as they appear in the level
    
    Dim intCount As Integer                                         'Count variable
    Dim intCount2 As Integer                                        'Another count variable
    Dim EnemySectionNotEmpty As Boolean                             'Flag to set if there are no enemies in the section
    Dim ObstacleSectionNotEmpty As Boolean                          'Flag to set if there are no obstacles in the section
    Static NumberEmptySections As Integer                           'Stores the number of empty sections counted
    Dim lngStartTime As Long                                        'The beginning time
    Dim TempInfo As typeBackGroundDesc                              'Temporary description variable
    Dim blnTempInfo As Boolean                                      'Temporary flag
    Dim strString As String                                         'String variable
    Dim SrcRect As RECT                                             'Source rectangle
    Dim lngDelayTime As Long                                        'Stores the amount of delay
    Dim byteIndex As Byte                                           'Index count variable
    Dim DSExplosionIndex As Integer                                 'Holds the direct sound explosion buffer count
        
    If SectionCount < 0 Then                                        'If the end of the level is reached
        byteLevel = byteLevel + 1                                   'Increment the level the player is on
        If byteLevel = 9 Then                                       'If all levels have been beat
            PlayMidi ""                                             'Stop playing any midi
            dsAlarm.Stop                                            'Turn off any alarm
            dsInvulnerability.Stop                                  'Stop any invulnerability sound effect
            ddsBack.BltColorFill EmptyRect, &H0                     'fill the back buffer with black
            lngStartTime = dx.TickCount                             'grab the current time
            
            Do While lngStartTime + 8000 > dx.TickCount             'loop this routine for 8 seconds
                Randomize                                           'reseed the random number generator
                DSExplosionIndex = DSExplosionIndex + 1             'increment the duplicate sound buffer count
                If DSExplosionIndex > UBound(dsExplosionDuplicate) Then DSExplosionIndex = 0
                                                                    'if all buffers are active, reset the count
                If Int((75 * Rnd) + 1) < 25 Then                    'if we get a number that is between 1-25 then
                    intCount = Int((SCREENWIDTH * Rnd) + 1)         'get a random X coordinate
                    intCount2 = Int((SCREENHEIGHT * Rnd) + 1)       'get a random Y coordinate
                    With SrcRect                                    'Enter the rectangle values
                        .Top = intCount2
                        .Bottom = .Top + 10
                        .Left = intCount
                        .Right = .Left + 10
                    End With
                    If Int((20 * Rnd) + 1) > 10 Then                'if we get a random number that is greater than ten
                        byteIndex = 1                               'set the explosion index to the second explosion
                    Else                                            'otherwise
                        byteIndex = 0                               'set it to the first
                    End If
                    CreateExplosion SrcRect, byteIndex, True        'create the explosion, and we don't give the player any credit for killing an enemy since there are none
                    lngDelayTime = dx.TickCount                     'grab the current time
                    Do While lngDelayTime + 35 > dx.TickCount       'loop for 35 milliseconds
                        DoEvents                                    'don't hog the processor while looping
                    Loop
                    dsExplosionDuplicate(DSExplosionIndex).SetCurrentPosition 0
                                                                    'set the play position of the duplicate buffer to the beginning
                    dsExplosionDuplicate(DSExplosionIndex).Play DSBPLAY_DEFAULT
                                                                    'play the explosion sound
                    DSExplosionIndex = DSExplosionIndex + 1
                                                                    'increment the explosion index to the next duplicate
                End If
                ddsFront.Flip Nothing, 0                            'Flip the front buffer with the back
                ddsBack.BltColorFill EmptyRect, &H0                 'fill the back buffer with black
                UpdateExplosions                                    'update the explosions
            Loop
            
            FadeScreen                                              'fade the screen to black
            
            For intCount = 0 To UBound(ExplosionDesc)               'loop through all explosions
                ExplosionDesc(intCount).Exists = False              'they all no longer exist
            Next
            ddsBack.BltColorFill EmptyRect, &H0                     'fill the back buffer with black
            
            'The next lines all display the winning text
            strString = "You win!!!"
            DrawText FindStringCenter(strString), 150, strString, RGB(135, 119, 8)
            strString = "After emerging victorious through 8 different alien galaxies, the enemy has been"
            DrawText FindStringCenter(strString), 165, strString, RGB(135, 119, 8)
            strString = "driven to the point of near-extinction. Congratulations on a victory well deserved!"
            DrawText FindStringCenter(strString), 180, strString, RGB(135, 119, 8)
            strString = "You return to Earth, triumphant."
            DrawText FindStringCenter(strString), 195, strString, RGB(135, 119, 8)
            strString = "As the peoples of the Earth revel in celebration,"
            DrawText FindStringCenter(strString), 210, strString, RGB(135, 119, 8)
            strString = "and the world rejoices from relief of the threat of annihalation, you can't help"
            DrawText FindStringCenter(strString), 225, strString, RGB(135, 119, 8)
            strString = "but ponder...were all of the aliens really destroyed?"
            DrawText FindStringCenter(strString), 240, strString, RGB(135, 119, 8)
            strString = "The End"
            DrawText FindStringCenter(strString), 270, strString, RGB(135, 119, 8)

            FadeScreen True                                 'fade the screen in
            lngStartTime = dx.TickCount                     'set the start time
            Do While lngStartTime + 20000 > dx.TickCount    'display the winning message for 20 seconds
                DoEvents                                    'don't hog the processor
            Loop
            FadeScreen                                      'fade the screen to black again
            intShields = 100                                'shields are at 100%
            With Ship
                .x = 300                                    'reset the players X
                .y = 300                                    'and Y coordinates
                .PowerUpState = 0                           'no powerups
                .NumBombs = 0                               'no bombs
                .Invulnerable = False                       'no longer invulnerable
                .AlarmActive = False                        'make sure the low shield alarm is off
            End With
            boolStarted = False                             'the game hasn't been started
            byteLives = 3                                   'the player has 3 lives left
            byteLevel = 1                                   'reset to level 1
            SectionCount = 999                              'start at the first section
            NumberEmptySections = 0                         'all the sections are filled again
            boolBackgroundExists = False                    'a background bitmap no longer exists
            CheckHighScore                                  'call the sub to see if the player got a high score
            Exit Sub                                        'exit the sub
        Else                                                'Otherwise, load a new level
            dsAlarm.Stop                                    'Stop playing the low shield alarm
            dsInvulnerability.Stop                          'Stop playing the invulnerability alarm
            LoadLevel CStr(byteLevel)                       'Load the new level
            SectionCount = 999                              'The section count starts at the beginning
            PlayMidi "level" & byteLevel & ".mid"           'Play the new midi
        End If
    End If
    
    For intCount = 0 To 125                                 'Loop through all the slots of this section
        If SectionInfo(SectionCount).Slot(intCount) < 255 Then
                                                            'If there is something in the this slot
            Do Until intCount2 > UBound(EnemyDesc)          'Loop through all the enemies
                If EnemyDesc(intCount2).Exists = False Then 'If this index is open
                    If EnemyDesc(intCount2).HasFired Then   'If the old enemy has a weapon that had fired still on the screen
                        blnTempInfo = True                  'flag that we need to pass some information to the new enemy
                        TempInfo = EnemyDesc(intCount2)     'store the information on this enemy temporarily
                    Else                                    'otherwise
                        blnTempInfo = False                 'we don't need to give any info to this enemy
                    End If
                    EnemyDesc(intCount2) = EnemyContainerDesc(SectionInfo(SectionCount).Slot(intCount))
                                                            'create the enemy using the enemy template
                    With EnemyDesc(intCount2)               'fill in all the enemy parameters
                        .Index = SectionInfo(SectionCount).Slot(intCount)
                                                            'the enemies index is equal to the value of the slot
                        .Exists = True                      'the enemy exists
                        .y = 0 - EnemyDesc(intCount2).Height
                                                            'set the enemy off the screen using its' height as the offset
                        .x = intCount * 5                   'offset the X by the slot we are on
                        .TimesHit = 0                       'the enemy has never been hit
                        If blnTempInfo Then                 'if the old enemy has fired, pass the info to this enemy
                            .HasFired = True                'this enemy has fired
                            .TargetX = TempInfo.TargetX     'give the enemy the target info of the last one
                            .TargetY = TempInfo.TargetY     'give the enemy the target info of the last one
                            .XFire = TempInfo.XFire         'give the enemy the target info of the last one
                            .YFire = TempInfo.YFire         'give the enemy the target info of the last one
                        End If
                        If Not .Invulnerable Then lngTotalNumEnemies = lngTotalNumEnemies + 1
                                                            'if this enemy is not invulnerable, increment the total number of enemies the level has
                    End With
                    Exit Do                                 'exit the loop
                End If
                intCount2 = intCount2 + 1                   'increment the search index
            Loop
            intCount2 = 0                                   'reset the search index
            EnemySectionNotEmpty = True                     'this section is not an empty one
        End If
        intCount2 = 0                                       'start the count variable at zero
        If ObstacleInfo(SectionCount).Slot(intCount) < 255 Then
                                                            'if the obstacle section has something in it
            Do Until intCount2 > UBound(ObstacleDesc)       'loop through all obsctacles
                If ObstacleDesc(intCount2).Exists = False Then
                                                            'if there is an open slot begin filling in the info for this obstacle
                    If ObstacleDesc(intCount2).HasFired Then
                                                            'if the obstacle has fired
                        blnTempInfo = True                  'flag that we have info to pass to the new obstacle
                        TempInfo = ObstacleDesc(intCount2)  'store the information about this obstacle
                    Else                                    'otherwise
                        blnTempInfo = False                 'we don't have info to pass on
                    End If
                    ObstacleDesc(intCount2) = ObstacleContainerInfo(ObstacleInfo(SectionCount).Slot(intCount))
                                                            'fill in the info on the new obstacle using the obstacle's template
                    With ObstacleDesc(intCount2)            'fill in the dynamic values
                        .Index = ObstacleInfo(SectionCount).Slot(intCount)
                                                            'the index of this obsacle is stored in the slot value
                        .Exists = True                      'the obstacle exists
                        .y = -80                            'set the obstacle off the top of the screen by 80 pixels
                        .x = intCount * 5                   'set the offset of the X position of the obstacle
                        If blnTempInfo Then                 'if there is info to pass to the new obstacle
                            .HasFired = True                'then the obstacle has fired
                            .TargetX = TempInfo.TargetX     'fill in the fire information
                            .TargetY = TempInfo.TargetY     'fill in the fire information
                            .XFire = TempInfo.XFire         'fill in the fire information
                            .YFire = TempInfo.YFire         'fill in the fire information
                        End If
                        If Not .Invulnerable Then lngTotalNumEnemies = lngTotalNumEnemies + 1
                                                            'if this obstacle is not invulnerable, increment the total number of enemies on this level
                    End With
                    Exit Do                                 'exit the loop
                End If
                intCount2 = intCount2 + 1                   'increment the count index
            Loop
            intCount2 = 0                                   'reset the count variable
            ObstacleSectionNotEmpty = True                  'the obstacle section isn't empty
        End If
    Next
    
    If ObstacleSectionNotEmpty = False And EnemySectionNotEmpty = False Then
                                                            'if the both sections are empty then
        NumberEmptySections = NumberEmptySections + 1       'increment the number of empty sections
        If NumberEmptySections = 40 Then                    'if 40 empty sections are reached
            SectionCount = 0                                'set the section count to 0
            NumberEmptySections = 0                         'set the number of empty sections to 0
        End If
    Else
        NumberEmptySections = 0                             'otherwise, reset the number of empty sections to 0
    End If


End Sub
Public Sub LoadLevel(strLevel As String)
    
    'This sub loads a level and dynamically initializes direct draw objects needed for the level. I also
    'shows the statistics of the previous level if there are any.
    
    Dim intLowerCount As Integer                                    'lower range count variable
    Dim intUpperCount As Integer                                    'upper range count variable
    Dim FileFree As Integer                                         'holds an available file handle
    Dim intCount As Integer                                         'standard count variable
    Dim intCount2 As Integer                                        'another count variable
    Dim TempString As String                                        'temporary string variable
    Dim LoadingString As String * 30                                'string loaded from the binary level file
    Dim KeyboardState(0 To 255) As Byte                             'Byte array to hold the state of the keyboard
    Dim SrcRect As RECT                                             'source rectangle structure
    Dim ColorKey As DDCOLORKEY                                      'a color key variable
    Dim strStats As String                                          'string to hold statistics
    Dim strNumEnemiesKilled As String                               'string to hold the number of enemies killed
    Dim strTotalNumEnemies As String                                'string to hold the total number of enemies on the level
    Dim strPercent As String                                        'string to hold the percentage of the enemies killed
    Dim strBonus As String                                          'string to display the bonus amount
    Dim strLoaded As String                                         'Stores the status of loading the bitmaps into surfaces
    Dim EmptyRect As RECT                                           'Empty rectangle structure
    
    With ColorKey                                                   'set the color key used for blitting the background image bitmaps
        .high = 0                                                   'uses black as the colorkey
        .low = 0
    End With

    ddsBack.BltColorFill EmptyRect, 0                               'fill the backbuffer with black
    PlayMidi "inbtween.mid"                                         'play the midi that goes inbetween the title screen and the levels
    
    If Not ddsBackgroundObject(byteLevel - 1) Is Nothing Then Set ddsBackgroundObject(byteLevel - 1) = Nothing                 'set the current background object to nothing
    
    For intCount = 0 To UBound(ddsBackgroundObject)                 'loop through all the background objects
        Set ddsBackgroundObject(intCount) = CreateDDSFromBitmap(dd, BackgroundObject(intCount).PathName)
                                                                    'Load one of the background bitmaps
        ddsBackgroundObject(intCount).SetColorKey DDCKEY_SRCBLT, ColorKey
                                                                    'set the color key of the background
    Next

    For intCount = 0 To UBound(ddsEnemyContainer)                   'Loop through all the enemy surfaces
        If Not ddsEnemyContainer(intCount) Is Nothing Then Set ddsEnemyContainer(intCount) = Nothing
                                                                    'release them if they exist
    Next
    
    For intCount = 0 To 30                                          'Loop through all the obstacle surfaces, except the last ten (which are static, not dynamic)
        If Not ddsObstacle(intCount) Is Nothing Then Set ddsObstacle(intCount) = Nothing
                                                                    'release those also
    Next
        
    FileFree = FreeFile                                             'get a handle to the next available free file
    Open App.Path & "\level" & strLevel & ".bin" For Binary Access Read As FileFree
                                                                    'open the level file for reading
    intCount = 0                                                    'set the count to 0
    Get FileFree, , LoadingString                                   'load the loading string into the LoadingString variable
    Do Until intCount > 999                                         'loop through all elements of the sectioncount array
        Get FileFree, , SectionInfo(intCount)                       'get the SectionInfo information from this record, and put it in the array
        intCount = intCount + 1                                     'increment the count
    Loop
    intCount = 0
    Do Until intCount > 999                                         'loop through all elements of the ObstacleInfo array
        Get FileFree, , ObstacleInfo(intCount)                      'get the ObstacleInfo information from this record, and put it in the array
        intCount = intCount + 1                                     'increment the count
    Loop

    Close FileFree                                                  'close the file

    With ColorKey                                                   'set the color key used for blitting the background image bitmaps
        .high = TRANSPARENTINDEX                                    'uses violet as the colorkey
        .low = TRANSPARENTINDEX
    End With

    For intCount = 0 To 999                                         'loop through the entire SectionInfo array for the level
        For intCount2 = 0 To 125                                    'there are 126 slots in each section, loop through all of those
            If SectionInfo(intCount).Slot(intCount2) < 255 Then     'if the slot value is less than 255, an object exists there
                If ddsEnemyContainer(SectionInfo(intCount).Slot(intCount2)) Is Nothing Then
                                                                    'if this object hasn't been loaded then
                    Set ddsEnemyContainer(SectionInfo(intCount).Slot(intCount2)) = CreateDDSFromBitmap(dd, App.Path & EnemyContainerDesc(SectionInfo(intCount).Slot(intCount2)).FileName)
                                                                    'create this object
                    ddsEnemyContainer(SectionInfo(intCount).Slot(intCount2)).SetColorKey DDCKEY_SRCBLT, ColorKey
                                                                    'set the objects color key
                End If
            End If
        Next
    Next
                                                                    'We do the exact same thing for the obstacle array
    For intCount = 0 To 999
        For intCount2 = 0 To 125
            If ObstacleInfo(intCount).Slot(intCount2) < 255 Then
                If ddsObstacle(ObstacleInfo(intCount).Slot(intCount2)) Is Nothing Then
                    Set ddsObstacle(ObstacleInfo(intCount).Slot(intCount2)) = CreateDDSFromBitmap(dd, App.Path & ObstacleContainerInfo(ObstacleInfo(intCount).Slot(intCount2)).FileName)
                    ddsObstacle(ObstacleInfo(intCount).Slot(intCount2)).SetColorKey DDCKEY_SRCBLT, ColorKey
                End If
            End If
        Next
    Next
        
    With SrcRect                                                    'set the source rectangle for the star surface
        .Top = 0
        .Bottom = 1
        .Left = 4
        .Right = 5
    End With
    
    For intCount = 1 To 500                                         'loop this 500 times
        intLowerCount = Int((SCREENHEIGHT - 1) * Rnd) + 1           'find a random Y coordinate
        intUpperCount = Int((SCREENWIDTH - 1) * Rnd) + 1            'find a random X coordinate
        ddsBack.BltFast intUpperCount, intLowerCount, ddsStar, SrcRect, DDBLTFAST_WAIT
                                                                    'blit a star in to the random coordinates
    Next
    
    intCount = 1                                                    'set the count variable to 1
    Do While intCount < Len(LoadingString)                          'loop until we reach the end of the string
        If Mid$(LoadingString, intCount, 1) = Chr$(0) Then Mid$(LoadingString, intCount, 1) = " "
                                                                    'set any null characters in the string to spaces
        intCount = intCount + 1                                     'increment the count
    Loop
    
    Call ShowMapLocation                                            'call the sub that shows the location of the player in the enemies galaxy
    strLevelText = LoadingString                                    'pass the loading string to the strLevelText variable
    strLevelText = Trim(strLevelText)                               'Trim any spaces from the loading string
            
    If byteLevel > 1 Then                                           'If the player is has passed level 1 then show statistics for the completed level
        strStats = "Last level statistics"                          'Display a message
        strNumEnemiesKilled = "Number of enemies destroyed: " & lngNumEnemiesKilled
                                                                    'set the string with the number of enemies killed
        strTotalNumEnemies = "Total number of enemies in level: " & lngTotalNumEnemies
                                                                    'set the string with the total number of enemies on the level
        If lngNumEnemiesKilled > 2 Then                             'if the player killed more than 1 enemy then
            strPercent = "Percentage of enemies destroyed: " & Format$(lngNumEnemiesKilled / lngTotalNumEnemies, "0%")
                                                                    'set the string with  the percentage of enemies killed
            strBonus = "Bonus: 10,000 X " & Format$(lngNumEnemiesKilled / lngTotalNumEnemies, "0%") & " = " & Format(CInt(10000 * (lngNumEnemiesKilled / lngTotalNumEnemies)), "###,###")
                                                                    'set the string with any bonus awarded
            lngScore = lngScore + CLng(10000 * (lngNumEnemiesKilled / lngTotalNumEnemies))
                                                                    'add the bonus to the players score
        End If
    End If
    
    FadeScreen True                                                 'fade the screen in
    
    diKeyBoard.Acquire                                              'make sure we have acquired the keyboard
    
    intCount = 0                                                    'set the count variable to 0
    Do While ((KeyboardState(DIK_RETURN) And &H80) = 0) Or _
        ((KeyboardState(DIK_NUMPADENTER) And &H80) = 0)             'if the enter key is pressed then
        intCount = intCount + 1                                     'begin incrementing the count
        If intCount > 10 And intCount <= 20 Then                    'if the count is currently greater than 10 and less than 20
            ShowMapLocation True                                    'show the map location, with the current position outlined
        ElseIf intCount <= 10 Then                                  'if it is less than 10
            ShowMapLocation                                         'show the map location with no outline
        End If
        If intCount > 20 Then intCount = 0                          'if the count is larger than 20, set it to 0
        If byteLevel > 1 Then                                       'if the player has passed level 1 then
            DrawText FindStringCenter(strStats), 80, strStats, RGB(51, 153, 1)
                                                                    'display the statistics
            DrawText FindStringCenter(strNumEnemiesKilled), 100, strNumEnemiesKilled, RGB(51, 153, 1)
                                                                    'display the number of enemies killed
            DrawText FindStringCenter(strTotalNumEnemies), 120, strTotalNumEnemies, RGB(51, 153, 1)
                                                                    'display the total number of enemies on the level
            If lngNumEnemiesKilled > 0 Then                         'if any enemies have been killed then
                DrawText FindStringCenter(strPercent), 140, strPercent, RGB(51, 153, 1)
                                                                    'display the percentage of enemies killed
                DrawText FindStringCenter(strBonus), 160, strBonus, RGB(51, 153, 1)
                                                                    'display the bonus awarded
            End If
        End If
        TempString = "Next level:  Level " & CStr(byteLevel)        'set the temp string with the next level number
        DrawText FindStringCenter(TempString), 200, TempString, RGB(136, 143, 172)
                                                                    'display the temp string
        DrawText FindStringCenter(strLevelText), 220, strLevelText, RGB(136, 143, 172)
                                                                    'display the level text
        TempString = "(Press enter to continue)"                    'set the temp string with this message
        DrawText FindStringCenter(TempString), 450, TempString, RGB(136, 143, 172)
                                                                    'display the temp string
        For intCount2 = 0 To UBound(KeyboardState)                  'loop through the array of keyboard keys
            KeyboardState(intCount2) = 0                            'set them all to 0
        Next
        diKeyBoard.GetDeviceState 256, KeyboardState(0)             'get the state of all the keyboard keys
        If ((KeyboardState(DIK_RETURN) And &H80) <> 0) Or _
            ((KeyboardState(DIK_NUMPADENTER) And &H80) <> 0) Then Exit Do   'if the enter key is pressed
        DoEvents                                                    'don't hog the processor
        ddsFront.Flip Nothing, 0                                    'flip the direct draw front buffer to display the info
    Loop
                                                                    'create force feedback effects out of the enumerated effects
                                                                    

    
    If ef(2) Is Nothing And IsFF = True Then Set ef(2) = diJoystick.CreateEffect(Eguid(0), diEF(2))
    If ef(0) Is Nothing And IsFF = True Then Set ef(0) = diJoystick.CreateEffect(Eguid(1), diEF(0))
    If ef(1) Is Nothing And IsFF = True Then Set ef(1) = diJoystick.CreateEffect(Eguid(2), diEF(1))
    If ef(3) Is Nothing And IsFF = True Then Set ef(3) = diJoystick.CreateEffect(Eguid(3), diEF(3))
        
    lngNumEnemiesKilled = 0                                         'reset the number of enemies killed
    lngTotalNumEnemies = 0                                          'reset the total number of enemies on the level
    FadeScreen                                                      'fade the screen to black
    
    intObjectIndex = byteLevel - 1                                  'set the background index to the current level number
    If intObjectIndex > UBound(BackgroundObject) Then               'if we go beyond the boundaries of how many objects we have allocated
        boolBackgroundExists = False                                'then the background object doesn't exist
        intObjectIndex = 0                                          'set the index to 0
    Else
        boolBackgroundExists = True                                 'reset background
        sngBackgroundX = (SCREENWIDTH \ 2) - (BackgroundObject(intObjectIndex).Width \ 2)
                                                                    'set the coorindates of the background object to be centered
        sngBackgroundY = -100 - BackgroundObject(intObjectIndex).Height
                                                                    'set the starting Y position of the object off the screen
    End If
    For intCount = 0 To UBound(PowerUp)
        PowerUp(intCount).Exists = False                            'reset powerups
    Next
    For intCount = 0 To UBound(EnemyDesc)                           'reset enemy lasers
        EnemyDesc(intCount).HasFired = False
    Next
    For intCount = 0 To UBound(GuidedMissile)                       'reset guided missiles
        GuidedMissile(intCount).Exists = False
    Next
    For intCount = 0 To UBound(LaserDesc)                           'reset lasers
        LaserDesc(intCount).Exists = False
    Next
    For intCount = 0 To UBound(Laser2LDesc)                         'reset level2 lasers
        Laser2LDesc(intCount).Exists = False
        Laser2RDesc(intCount).Exists = False
    Next
    For intCount = 0 To UBound(Laser3Desc)                          'reset level3 lasers
        Laser3Desc(intCount).Exists = False
    Next
    For intCount = 0 To UBound(ExplosionDesc)                       'reset explosions
        ExplosionDesc(intCount).Exists = False
    Next
    
    For intCount = 0 To UBound(ddsBackgroundObject)                 'loop through all the backgrounds and
        Set ddsBackgroundObject(intCount) = Nothing                 'set all the backgrounds displayed in the level display screen to nothing to free up some memory
    Next

    With ColorKey                                                   'set the color key used for blitting the background image bitmaps
        .high = 0                                                   'use black as the colorkey
        .low = 0
    End With

    Set ddsBackgroundObject(byteLevel - 1) = CreateDDSFromBitmap(dd, BackgroundObject(byteLevel - 1).PathName)
                                                                    'Now we load only the necessary background object
    ddsBackgroundObject(byteLevel - 1).SetColorKey DDCKEY_SRCBLT, ColorKey
                                                                    'set its' color key
    
    With Ship                                                       'Reset the ships' position and velocity
        .x = 300                                                    'Set X coordinates for ship
        .y = 300                                                    'Set Y coordinates for ship
        .XVelocity = 0                                              'the ship has no velocity in the X direction
        .YVelocity = 0                                              'the ship has no velocity in the Y direction
    End With


    dsEnergize.SetCurrentPosition 0                                 'Set the position of the energize wav to the beginning
    dsEnergize.Play DSBPLAY_DEFAULT                                 'and then play it
    
End Sub

Private Sub UpdateHits(Optional NewHit As Boolean = False, Optional x As Long, Optional y As Long)
    
    'This sub creates, destroys, and updates small explosions for when the player hits an object or is hit
    'It also plays a small "no hit" sound effect
    
    Dim intCount As Integer                                     'Count variable
    Dim SrcRect As RECT                                         'Rectangle structure
    Static DSNoHitIndex As Integer                              'Keep track of the duplicate sound buffer
    
    If NewHit Then                                              'If this is a new hit
        For intCount = 0 To UBound(HitDesc)                     'Loop through the hit array
            If HitDesc(intCount).Exists = False Then            'If we find a spot that is free
                With HitDesc(intCount)                          'Add in the coordinates of the new hit
                    .Exists = True                              'This hit now exists
                    .x = x - 2                                  'Center the x if the hit
                    .y = y                                      'The Y of the hit
                End With
            End If
        Next
        dsNoHitDuplicate(DSNoHitIndex).SetCurrentPosition 0     'Set the sound effect to the start
        dsNoHitDuplicate(DSNoHitIndex).Play DSBPLAY_DEFAULT     'Play the sound effect
        DSNoHitIndex = DSNoHitIndex + 1                         'Increment the duplicate count
        If DSNoHitIndex > UBound(dsNoHitDuplicate) Then DSNoHitIndex = 0
                                                                'If we run out of buffers, set them to 0
    Else                                                        'Otherwise, if this is updating an existing hit
        For intCount = 0 To UBound(HitDesc)                     'Loop through the hit array
            If HitDesc(intCount).Exists Then                    'If this hit exists
                If HitDesc(intCount).Index > HitDesc(intCount).NumFrames Then
                                                                'If the current frame is larger than the number of frames the hit animation has
                    HitDesc(intCount).Exists = False            'The hit no longer exists
                    HitDesc(intCount).Index = 0                 'Set the frame of the hit to 0
                Else                                            'Otherwise, the hit animation frame needs to be displayed
                    If HitDesc(intCount).x > 0 And HitDesc(intCount).x < (SCREENWIDTH - HitDesc(intCount).Width) And HitDesc(intCount).y > 0 And HitDesc(intCount).y < (SCREENHEIGHT - HitDesc(intCount).Height) Then
                                                                'If the hit is on screen
                        ddsBack.BltFast HitDesc(intCount).x, HitDesc(intCount).y, ddsHit, EmptyRect, DDBLTFAST_WAIT Or DDBLTFAST_SRCCOLORKEY
                                                                'blit the hit to the screen
                    End If
                    HitDesc(intCount).Index = HitDesc(intCount).Index + 1
                                                                'increment the animation
                End If
            End If
        Next
    End If

End Sub


Private Sub UpdatePowerUps(Optional CreatePowerup As Boolean)

    'This sub checks to see if there is a power-up on the screen, updates it
    'if there is, or see if it is time to create a new power-up.
    'If there is a power-up on screen, it paints it, and advances the animation
    'frames as needed for the existing power-up
    
    Dim intRandomNumber As Integer                                                          'variable to hold a random number
    Dim SrcRect As RECT                                                                     'rect structure
    Dim byteFrameOffset As Byte                                                             'offset for animation frames
    Dim intCount As Integer                                                                 'standard count integer
    Static byteAdvanceFrameOffset As Byte                                                   'counter to advance the animation frames
    Static byteFrameCount As Byte                                                           'holds which animation frame we are on
    
    If CreatePowerup Then                                                                   'If there it is time to create a power-up
        intCount = 0                                                                        'reset the count variable
        Do Until PowerUp(intCount).Exists = False                                           'find an empty power up index
            intCount = intCount + 1                                                         'increment the count
        Loop
        If intCount < UBound(PowerUp) Then                                                  'if there was an empty spot found
            intRandomNumber = Int((900 - 1) * Rnd) + 1                                      'Create a random number to see which power up
            If intRandomNumber <= 400 Then                                                  'see what value the random number is
                PowerUp(intCount).Index = SHIELD                                            'make it a shield powerup
            ElseIf intRandomNumber > 400 And intRandomNumber < 600 Then
                PowerUp(intCount).Index = WEAPON                                            'make it a weapon powerup
            ElseIf intRandomNumber >= 600 And intRandomNumber < 800 Then
                PowerUp(intCount).Index = BOMB                                              'make it a bomb powerup
            ElseIf intRandomNumber >= 800 And intRandomNumber < 900 Then
                PowerUp(intCount).Index = INVULNERABILITY                                   'Make it an invulnerability powerup
            End If
            PowerUp(intCount).x = Int(((SCREENWIDTH - POWERUPWIDTH) - 1) * Rnd + 1)         'Create the power-up, and set a random X position
            PowerUp(intCount).y = 0                                                         'Make the power-up start at the top of the screen
            PowerUp(intCount).Exists = True                                                 'The power up now exists
        End If
    End If
    
    For intCount = 0 To UBound(PowerUp)                                                     'loop through all power ups
        If PowerUp(intCount).Exists Then                                                    'if a power up exists
            If byteAdvanceFrameOffset > 3 Then                                              'if it is time to increment the animation frame
                If byteFrameCount = 0 Then                                                  'if it is frame 0
                    byteFrameCount = 1                                                      'switch to frame 1
                Else                                                                        'otherwise
                    byteFrameCount = 0                                                      'switch to frame 0
                End If
                byteAdvanceFrameOffset = 0                                                  'reset the frame advance count to 0
            Else
                byteAdvanceFrameOffset = byteAdvanceFrameOffset + 1                         'otherwise, increment the advance frame counter by 1
            End If
            
            byteFrameOffset = (POWERUPWIDTH * byteFrameCount) + PowerUp(intCount).Index     'determine the offset for the surfces rectangle
            
            With SrcRect                                                                    'Set the rectangle structure of the power-up
                .Top = 0                                                                    'start at the very top of the surface
                .Bottom = .Top + POWERUPHEIGHT                                              'use the powerup constant to determine the top
                .Left = 0 + byteFrameOffset                                                 'determine the frame offset
                .Right = .Left + POWERUPWIDTH                                               'use the constant to set the width of the surface to blit
            End With
            
            If PowerUp(intCount).y + POWERUPHEIGHT > SCREENHEIGHT Then                      'If the power-up goes off screen,
                PowerUp(intCount).Exists = False                                            'destroy it
            Else
                ddsBack.BltFast PowerUp(intCount).x, PowerUp(intCount).y, ddsPowerUp, SrcRect, DDBLTFAST_WAIT Or DDBLTFAST_SRCCOLORKEY
                                                                                            'otherwise, blit it to the back buffer,
            End If
            
            PowerUp(intCount).y = PowerUp(intCount).y + 1.25                                'and increment its' Y position
        End If
    Next
    
    Exit Sub
    
End Sub

Private Sub CheckForCollisions()
    
    'This sub checks all objects on the screen to see if they are colliding,
    'increments points, and plays sounds effects.
    'This also is the largest sub in the program, since it has to increment through
    'everything on the screen
    
    Dim SrcRect As RECT                                                             'rect structure
    Dim SrcRect2 As RECT                                                            'another rect structure
    Dim intCount As Integer                                                         'counter for loops
    Dim intCount2 As Integer                                                        'second loop counter
    Dim ShipRect As RECT                                                            'holds the position of the player
    Dim ddTempBltFx As DDBLTFX                                                      'used to hold info about the special effects for flashing the screen when something is hit
    Dim TempDesc As typeBackGroundDesc
    Dim blnTempDesc As Boolean
    Static DSExplosionIndex As Integer                                              'used to keep track of which direct sound explosion duplicate we are on
    
    If DSExplosionIndex > UBound(dsExplosionDuplicate) Then DSExplosionIndex = 0    'increment the duplicate explosion sound effect index to the next position
    With ddTempBltFx                                                                'used to fill the screen with red when the player is hit.
        .lFill = 143                                                                'Index 143 in the palette is bright red
    End With
    
    With ShipRect                                                                   'define the rectangle for the player
        .Top = Ship.y                                                               'get the Y coordinate of the player
        .Bottom = .Top + (SHIPHEIGHT - 15)                                          'make sure not to include the flames from the bottom of the ship
        .Left = Ship.x + 10                                                         'make sure to not include the orbiting elements
        .Right = .Left + (SHIPWIDTH - 10)                                           'same thing, but on the right
    End With
    For intCount = 0 To UBound(PowerUp)
        With SrcRect                                                                'define the coordinates for the powerups
            .Top = PowerUp(intCount).y
            .Bottom = .Top + POWERUPHEIGHT
            .Left = PowerUp(intCount).x
            .Right = .Left + POWERUPWIDTH
        End With
    
        If PowerUp(intCount).Exists And DetectCollision(ShipRect, SrcRect) Then     'if the power up exists, and the player has collided with it
            If PowerUp(intCount).Index = SHIELD Then                                'if it is a shield powerup
                intShields = intShields + 20                                        'increase the shields by 20
                lngScore = lngScore + 100                                           'player gets a 100 points for this
                If intShields > 100 Then intShields = 100                           'if the shields are already maxed out, make sure it doesn't go beyond max
                PowerUp(intCount).Exists = False                                    'the power up no longer exists
                dsPowerUp.SetCurrentPosition 0                                      'set the playback buffer position to 0
                dsPowerUp.Play DSBPLAY_DEFAULT                                      'play the wav
                Exit Sub
            ElseIf PowerUp(intCount).Index = WEAPON Then                            'if the powerup is a weapon powerup
                If Ship.PowerUpState < 3 Then Ship.PowerUpState = Ship.PowerUpState + 1
                                                                                    'if the powerups reach 3, make sure it doesn't go any higher than that
                lngScore = lngScore + 200                                           'player gets 200 points for this
                PowerUp(intCount).Exists = False                                    'the power up no longer exists
                dsPowerUp.SetCurrentPosition 0                                      'set the playback buffer position to 0
                dsPowerUp.Play DSBPLAY_DEFAULT                                      'play the wav
                Exit Sub
            ElseIf PowerUp(intCount).Index = BOMB Then                              'the power up is a bomb powerup
                If Ship.NumBombs < 5 Then Ship.NumBombs = Ship.NumBombs + 1         'if we haven't reached the maxiumum number of bomb, increase the number of bombs the player has
                lngScore = lngScore + 200                                           'give the player a score increase, even if the bombs are at max
                PowerUp(intCount).Exists = False                                    'the power up no longer exists
                dsPowerUp.SetCurrentPosition 0                                      'set the playback buffer position to 0
                dsPowerUp.Play DSBPLAY_DEFAULT                                      'play the wav
                Exit Sub                                                            'exit the sub
            ElseIf PowerUp(intCount).Index = INVULNERABILITY Then                   'the power up is an invulnerability power up
                Ship.Invulnerable = True                                            'set the ships' invulnerable flag
                Ship.InvulnerableTime = dx.TickCount + 15000                        'set the duration of the invulnerability
                lngScore = lngScore + 500
                PowerUp(intCount).Exists = False
                dsPowerUp.SetCurrentPosition 0                                      'set the playback buffer position to 0
                dsPowerUp.Play DSBPLAY_DEFAULT                                      'play the wav
                dsInvulnerability.SetCurrentPosition 0                              'set the playback buffer position to 0
                dsInvulnerability.Play DSBPLAY_LOOPING                              'play the wav
            End If
        End If
    Next
    
    For intCount = 0 To UBound(EnemyDesc)                                           'loop through the entire enemy array
        If EnemyDesc(intCount).Exists = True Then                                   'if the enemy exists
            With SrcRect                                                            'define the rectangle coordinates of the enemy
                .Top = EnemyDesc(intCount).y
                .Bottom = .Top + EnemyDesc(intCount).Height
                .Left = EnemyDesc(intCount).x
                .Right = .Left + EnemyDesc(intCount).Width
            End With
            If DetectCollision(SrcRect, ShipRect) Then                              'if the enemy ship collides with the player
                dsExplosionDuplicate(DSExplosionIndex).SetCurrentPosition 0         'set the position of the buffer to the beginning
                dsExplosionDuplicate(DSExplosionIndex).SetPan (SrcRect.Left - (SCREENWIDTH \ 2)) * 3
                dsExplosionDuplicate(DSExplosionIndex).Play DSBPLAY_DEFAULT         'play the explosion sound
                If IsFF = True Then ef(1).start 1, 0                                'If force feedback is enabled, start the effect
                DSExplosionIndex = DSExplosionIndex + 1                             'increment the explosion index to the next duplicate
                If Not EnemyDesc(intCount).Invulnerable Then EnemyDesc(intCount).Exists = False
                                                                                    'if the enemy isn't invulnerable the enemy is destroyed
                CreateExplosion SrcRect, EnemyDesc(intCount).ExplosionIndex         'Call the create explosion sub with the rect coordinates, and the index of the explosion type
                If Not Ship.Invulnerable Then                                       'If the ship is not invulnerable then
                    intShields = intShields - EnemyDesc(intCount).CollisionDamage   'take points off the shields for colliding with the enemy
                    If Ship.PowerUpState > 0 Then                                   'reduce a powerup level if the player has one
                        Ship.PowerUpState = Ship.PowerUpState - 1
                    End If
                End If
                lngScore = lngScore + EnemyDesc(intCount).Score                     'add the score value of this enemy to the players score
                Exit Sub
            End If
        End If
        If EnemyDesc(intCount).HasFired Then                                        'Determine if the enemy laser fire hit player
            With SrcRect                                                            'define coordinates of enemy weapon fire
                .Top = EnemyDesc(intCount).YFire
                .Bottom = .Top + 5
                .Left = EnemyDesc(intCount).XFire
                .Right = .Left + 5
            End With
            If DetectCollision(SrcRect, ShipRect) Then                              'if the enemy weapon fire hits the player then
                EnemyDesc(intCount).HasFired = False                                'the enemy weapon fire is destroyed
                If Not Ship.Invulnerable Then
                    intShields = intShields - 5                                     'subtract 5 from the playres shields
                    If Ship.PowerUpState > 0 Then                                   'if the player has a power up,
                        Ship.PowerUpState = Ship.PowerUpState - 1                   'knock it down a level
                    End If
                End If
                UpdateHits True, CLng(EnemyDesc(intCount).XFire), CLng(EnemyDesc(intCount).YFire)
                                                                                    'Call the sub that displays a small explosion bitmap where the player was hit
                If IsFF = True Then ef(1).start 1, 0                                'If force feeback is enabled, start the effect
                Exit Sub
            End If
        End If
    Next
    
    For intCount = 0 To UBound(ObstacleDesc)
        If ObstacleDesc(intCount).HasFired Then                                     'Determine if the obstacle laser fire hit player
            With SrcRect                                                            'define coordinates of obstacle weapon fire
                .Top = ObstacleDesc(intCount).YFire
                .Bottom = .Top + 5
                .Left = ObstacleDesc(intCount).XFire
                .Right = .Left + 5
            End With
            If DetectCollision(SrcRect, ShipRect) Then                              'if the obstacle weapon fire hits the player then
                ObstacleDesc(intCount).HasFired = False                             'the obstacle weapon fire is destroyed
                If Not Ship.Invulnerable Then                                       'If the player isn't invulnerable then
                    intShields = intShields - 5                                     'subtract 5 from the playres shields
                    If Ship.PowerUpState > 0 Then                                   'if the player has a power up,
                        Ship.PowerUpState = Ship.PowerUpState - 1                   'knock it down a level
                    End If
                End If
                UpdateHits True, CLng(ObstacleDesc(intCount).XFire), CLng(ObstacleDesc(intCount).YFire)                                                                                    'Small explosion sub
                Exit Sub
            End If
        End If
    Next
    
    For intCount2 = 0 To UBound(LaserDesc)                                          'Collision detection for stage 1 laser
        If LaserDesc(intCount2).Exists Then                                         'If this index of the laser is on screen
            With SrcRect2                                                           'Define the coordinates of the rectangle
                .Top = LaserDesc(intCount2).y
                .Bottom = .Top + LaserDesc(intCount2).Height
                .Left = LaserDesc(intCount2).x
                .Right = .Left + LaserDesc(intCount2).Width
            End With
            For intCount = 0 To UBound(EnemyDesc)                                   'Loop through all the enemies
                If EnemyDesc(intCount).Exists Then                                  'If this enemy is on the screen then
                    With SrcRect                                                    'Define this enemies coordinates
                        .Top = EnemyDesc(intCount).y
                        .Bottom = .Top + EnemyDesc(intCount).Height
                        .Left = EnemyDesc(intCount).x
                        .Right = .Left + EnemyDesc(intCount).Width
                    End With
                    If DetectCollision(SrcRect, SrcRect2) Then                      'If this enemy is struck by the weapon
                        EnemyDesc(intCount).TimesHit = EnemyDesc(intCount).TimesHit + LaserDesc(intCount2).Damage
                                                                                    'Subtract the amount of damage the weapon does from the enemy
                        If EnemyDesc(intCount).TimesHit > EnemyDesc(intCount).TimesDies And Not EnemyDesc(intCount).Invulnerable Then
                                                                                    'If the number of times the enemy has been hit is greater than
                                                                                    'the amount of times the enemy can be hit, then
                            lngScore = lngScore + EnemyDesc(intCount).Score         'add the score value of this enemy to the players score
                            dsExplosionDuplicate(DSExplosionIndex).SetCurrentPosition 0
                            dsExplosionDuplicate(DSExplosionIndex).SetPan (SrcRect.Left - (SCREENWIDTH \ 2)) * 3
                            dsExplosionDuplicate(DSExplosionIndex).Play DSBPLAY_DEFAULT
                                                                                    'play the explosion sound
                            DSExplosionIndex = DSExplosionIndex + 1                 'increment the explosion index to the next duplicate
                            EnemyDesc(intCount).Exists = False                      'This enemy no longer exists
                            LaserDesc(intCount2).Exists = False                     'The players weapon fire no longer exists
                            CreateExplosion SrcRect, EnemyDesc(intCount).ExplosionIndex
                            Exit Sub
                        Else                                                        'If the enemy is still alive, then
                            UpdateHits True, SrcRect2.Left, SrcRect2.Top
                            LaserDesc(intCount2).Exists = False                     'The players weapon fire no longer exists
                            Exit Sub
                        End If
                    End If
                End If
            Next
            For intCount = 0 To UBound(ObstacleDesc)                                'Loop through all the obstacles
                If ObstacleDesc(intCount).Exists And Not ObstacleDesc(intCount).Invulnerable Then
                                                                                    'If this obstacle is on the screen then
                    With SrcRect                                                    'Define this enemies coordinates
                        .Top = ObstacleDesc(intCount).y
                        .Bottom = .Top + ObstacleDesc(intCount).Height
                        .Left = ObstacleDesc(intCount).x
                        .Right = .Left + ObstacleDesc(intCount).Width
                    End With
                    If DetectCollision(SrcRect, SrcRect2) Then                      'If this obstacle is struck by the weapon
                        ObstacleDesc(intCount).TimesHit = ObstacleDesc(intCount).TimesHit + LaserDesc(intCount2).Damage
                                                                                    'Subtract the amount of damage the weapon does from the obstacle
                        If ObstacleDesc(intCount).TimesHit > ObstacleDesc(intCount).TimesDies Then
                                                                                    'If the number of times the obstacle has been hit is greater than
                                                                                    'the amount of times the obstacle can be hit, then
                            lngScore = lngScore + ObstacleDesc(intCount).Score      'add the score value of this obstacle to the players score
                            dsExplosionDuplicate(DSExplosionIndex).SetPan (SrcRect.Left - (SCREENWIDTH \ 2)) * 3
                            dsExplosionDuplicate(DSExplosionIndex).Play DSBPLAY_DEFAULT 'play the explosion sound
                            DSExplosionIndex = DSExplosionIndex + 1                 'increment the explosion index to the next duplicate
                            If ObstacleDesc(intCount).HasFired Then
                                TempDesc = ObstacleDesc(intCount)
                                blnTempDesc = True
                            Else
                                blnTempDesc = False
                            End If
                            If ObstacleDesc(intCount).HasDeadIndex Then
                                ObstacleDesc(intCount) = ObstacleContainerInfo(ObstacleDesc(intCount).DeadIndex)
                                With ObstacleDesc(intCount)
                                    .Exists = True
                                    .x = SrcRect.Left
                                    .y = SrcRect.Top
                                    .Index = ObstacleDesc(intCount).DeadIndex
                                    If blnTempDesc Then
                                        .XFire = TempDesc.XFire
                                        .YFire = TempDesc.YFire
                                        .TargetX = TempDesc.TargetX
                                        .TargetY = TempDesc.TargetY
                                    End If
                                End With
                            Else
                                ObstacleDesc(intCount).Exists = False
                            End If
                            LaserDesc(intCount2).Exists = False                     'The players weapon fire no longer exists
                            CreateExplosion SrcRect, ObstacleDesc(intCount).ExplosionIndex
                            Exit Sub
                        Else                                                        'If the obstacle is still alive, then
                            UpdateHits True, SrcRect2.Left, SrcRect2.Top
                            LaserDesc(intCount2).Exists = False                     'The players weapon fire no longer exists
                            Exit Sub
                        End If
                    End If
                End If
            Next
        End If
    Next
    
    'The rest of the collision detection is pretty much the same. Loop through whatever it is
    'that needs to be checked, set up the source rectangle, set up the 2nd source, check if they
    'collide, and handle it appropriately. With the above comments, you should be able to figure out
    'what the rest is doing.
    
    For intCount2 = 0 To UBound(Laser2RDesc)
        If Laser2RDesc(intCount2).Exists Then
            With SrcRect2
                .Top = Laser2RDesc(intCount2).y
                .Bottom = .Top + LASER2HEIGHT
                .Left = Laser2RDesc(intCount2).x
                .Right = .Left + LASER2WIDTH
            End With
            For intCount = 0 To UBound(EnemyDesc)
                If EnemyDesc(intCount).Exists Then
                    With SrcRect
                        .Top = EnemyDesc(intCount).y
                        .Bottom = .Top + EnemyDesc(intCount).Height
                        .Left = EnemyDesc(intCount).x
                        .Right = .Left + EnemyDesc(intCount).Width
                    End With
                    If DetectCollision(SrcRect, SrcRect2) Then
                        EnemyDesc(intCount).TimesHit = EnemyDesc(intCount).TimesHit + Laser2RDesc(intCount2).Damage
                        If EnemyDesc(intCount).TimesHit > EnemyDesc(intCount).TimesDies And Not EnemyDesc(intCount).Invulnerable Then
                            lngScore = lngScore + EnemyDesc(intCount).Score
                            dsExplosionDuplicate(DSExplosionIndex).SetCurrentPosition 0
                            dsExplosionDuplicate(DSExplosionIndex).SetPan (SrcRect.Left - (SCREENWIDTH \ 2)) * 3
                            dsExplosionDuplicate(DSExplosionIndex).Play DSBPLAY_DEFAULT
                            DSExplosionIndex = DSExplosionIndex + 1
                            EnemyDesc(intCount).Exists = False
                            CreateExplosion SrcRect, EnemyDesc(intCount).ExplosionIndex
                            Laser2RDesc(intCount2).Exists = False
                            Exit Sub
                        Else
                            Laser2RDesc(intCount2).Exists = False
                            UpdateHits True, SrcRect2.Left, SrcRect.Top
                            Exit Sub
                        End If
                    End If
                End If
            Next
            For intCount = 0 To UBound(ObstacleDesc)
                If ObstacleDesc(intCount).Exists And Not ObstacleDesc(intCount).Invulnerable Then
                    With SrcRect
                        .Top = ObstacleDesc(intCount).y
                        .Bottom = .Top + ObstacleDesc(intCount).Height
                        .Left = ObstacleDesc(intCount).x
                        .Right = .Left + ObstacleDesc(intCount).Width
                    End With
                    If DetectCollision(SrcRect, SrcRect2) Then
                        ObstacleDesc(intCount).TimesHit = ObstacleDesc(intCount).TimesHit + Laser2RDesc(intCount2).Damage
                        If ObstacleDesc(intCount).TimesHit > ObstacleDesc(intCount).TimesDies Then
                            lngScore = lngScore + ObstacleDesc(intCount).Score
                            dsExplosionDuplicate(DSExplosionIndex).SetCurrentPosition 0
                            dsExplosionDuplicate(DSExplosionIndex).SetPan (SrcRect.Left - (SCREENWIDTH \ 2)) * 3
                            dsExplosionDuplicate(DSExplosionIndex).Play DSBPLAY_DEFAULT
                            DSExplosionIndex = DSExplosionIndex + 1
                            If ObstacleDesc(intCount).HasFired Then
                                TempDesc = ObstacleDesc(intCount)
                                blnTempDesc = True
                            Else
                                blnTempDesc = False
                            End If
                            If ObstacleDesc(intCount).HasDeadIndex Then
                                ObstacleDesc(intCount) = ObstacleContainerInfo(ObstacleDesc(intCount).DeadIndex)
                                With ObstacleDesc(intCount)
                                    .Exists = True
                                    .x = SrcRect.Left
                                    .y = SrcRect.Top
                                    .Index = ObstacleDesc(intCount).DeadIndex
                                    If blnTempDesc Then
                                        .XFire = TempDesc.XFire
                                        .YFire = TempDesc.YFire
                                        .TargetX = TempDesc.TargetX
                                        .TargetY = TempDesc.TargetY
                                    End If
                                End With
                            Else
                                ObstacleDesc(intCount).Exists = False
                            End If
                            CreateExplosion SrcRect, ObstacleDesc(intCount).ExplosionIndex
                            Laser2RDesc(intCount2).Exists = False
                            Exit Sub
                        Else
                            Laser2RDesc(intCount2).Exists = False
                            UpdateHits True, SrcRect2.Left, SrcRect.Top
                            Exit Sub
                        End If
                    End If
                End If
            Next
        End If
    Next
    For intCount2 = 0 To UBound(Laser2LDesc)
        If Laser2LDesc(intCount2).Exists Then
            With SrcRect2
                .Top = Laser2LDesc(intCount2).y
                .Bottom = .Top + LASER2HEIGHT
                .Left = Laser2LDesc(intCount2).x
                .Right = .Left + LASER2WIDTH
            End With
            For intCount = 0 To UBound(EnemyDesc)
                If EnemyDesc(intCount).Exists Then
                    With SrcRect
                        .Top = EnemyDesc(intCount).y
                        .Bottom = .Top + EnemyDesc(intCount).Height
                        .Left = EnemyDesc(intCount).x
                        .Right = .Left + EnemyDesc(intCount).Width
                    End With
                    If DetectCollision(SrcRect, SrcRect2) Then
                        EnemyDesc(intCount).TimesHit = EnemyDesc(intCount).TimesHit + Laser2LDesc(intCount2).Damage
                        If EnemyDesc(intCount).TimesHit > EnemyDesc(intCount).TimesDies And Not EnemyDesc(intCount).Invulnerable Then
                            lngScore = lngScore + EnemyDesc(intCount).Score
                            dsExplosionDuplicate(DSExplosionIndex).SetCurrentPosition 0
                            dsExplosionDuplicate(DSExplosionIndex).SetPan (SrcRect.Left - (SCREENWIDTH \ 2)) * 3
                            dsExplosionDuplicate(DSExplosionIndex).Play DSBPLAY_DEFAULT
                            DSExplosionIndex = DSExplosionIndex + 1
                            EnemyDesc(intCount).Exists = False
                            CreateExplosion SrcRect, EnemyDesc(intCount).ExplosionIndex
                            Laser2LDesc(intCount2).Exists = False
                            Exit Sub
                        Else
                            Laser2LDesc(intCount2).Exists = False
                            UpdateHits True, SrcRect2.Left, SrcRect.Top
                            Exit Sub
                        End If
                    End If
                End If
            Next
            For intCount = 0 To UBound(ObstacleDesc)
                If ObstacleDesc(intCount).Exists And Not ObstacleDesc(intCount).Invulnerable Then
                    With SrcRect
                        .Top = ObstacleDesc(intCount).y
                        .Bottom = .Top + ObstacleDesc(intCount).Height
                        .Left = ObstacleDesc(intCount).x
                        .Right = .Left + ObstacleDesc(intCount).Width
                    End With
                    If DetectCollision(SrcRect, SrcRect2) Then
                        ObstacleDesc(intCount).TimesHit = ObstacleDesc(intCount).TimesHit + Laser2LDesc(intCount2).Damage
                        If ObstacleDesc(intCount).TimesHit > ObstacleDesc(intCount).TimesDies Then
                            lngScore = lngScore + ObstacleDesc(intCount).Score
                            dsExplosionDuplicate(DSExplosionIndex).SetCurrentPosition 0
                            dsExplosionDuplicate(DSExplosionIndex).SetPan (SrcRect.Left - (SCREENWIDTH \ 2)) * 3
                            dsExplosionDuplicate(DSExplosionIndex).Play DSBPLAY_DEFAULT
                            DSExplosionIndex = DSExplosionIndex + 1
                            If ObstacleDesc(intCount).HasFired Then
                                TempDesc = ObstacleDesc(intCount)
                                blnTempDesc = True
                            Else
                                blnTempDesc = False
                            End If
                            If ObstacleDesc(intCount).HasDeadIndex Then
                                ObstacleDesc(intCount) = ObstacleContainerInfo(ObstacleDesc(intCount).DeadIndex)
                                With ObstacleDesc(intCount)
                                    .Exists = True
                                    .x = SrcRect.Left
                                    .y = SrcRect.Top
                                    .Index = ObstacleDesc(intCount).DeadIndex
                                    If blnTempDesc Then
                                        .XFire = TempDesc.XFire
                                        .YFire = TempDesc.YFire
                                        .TargetX = TempDesc.TargetX
                                        .TargetY = TempDesc.TargetY
                                    End If
                                End With
                            Else
                                ObstacleDesc(intCount).Exists = False
                            End If
                            CreateExplosion SrcRect, ObstacleDesc(intCount).ExplosionIndex
                            Laser2LDesc(intCount2).Exists = False
                            Exit Sub
                        Else
                            Laser2LDesc(intCount2).Exists = False
                            UpdateHits True, SrcRect2.Left, SrcRect.Top
                            Exit Sub
                        End If
                    End If
                End If
            Next
        End If
    Next
    For intCount2 = 0 To UBound(Laser3Desc)
        If Laser3Desc(intCount2).Exists Then
            With SrcRect2
                .Top = Laser3Desc(intCount2).y
                .Bottom = .Top + Laser3Desc(intCount2).Height
                .Left = Laser3Desc(intCount2).x
                .Right = .Left + Laser3Desc(intCount2).Width
            End With
            For intCount = 0 To UBound(EnemyDesc)
                If EnemyDesc(intCount).Exists Then
                    With SrcRect
                        .Top = EnemyDesc(intCount).y
                        .Bottom = .Top + EnemyDesc(intCount).Height
                        .Left = EnemyDesc(intCount).x
                        .Right = .Left + EnemyDesc(intCount).Width
                    End With
                    If DetectCollision(SrcRect, SrcRect2) And Laser3Desc(intCount2).StillColliding = False Then
                        Laser3Desc(intCount2).StillColliding = True
                        EnemyDesc(intCount).TimesHit = EnemyDesc(intCount).TimesHit + Laser3Desc(intCount2).Damage
                        If EnemyDesc(intCount).TimesHit > EnemyDesc(intCount).TimesDies And Not EnemyDesc(intCount).Invulnerable Then
                            lngScore = lngScore + EnemyDesc(intCount).Score
                            dsExplosionDuplicate(DSExplosionIndex).SetCurrentPosition 0
                            dsExplosionDuplicate(DSExplosionIndex).SetPan (SrcRect.Left - (SCREENWIDTH \ 2)) * 3
                            dsExplosionDuplicate(DSExplosionIndex).Play DSBPLAY_DEFAULT
                            DSExplosionIndex = DSExplosionIndex + 1
                            CreateExplosion SrcRect, EnemyDesc(intCount).ExplosionIndex
                            EnemyDesc(intCount).Exists = False
                            Exit Sub
                        Else
                            UpdateHits True, SrcRect2.Left, SrcRect2.Top
                            Exit Sub
                        End If
                    ElseIf DetectCollision(SrcRect, SrcRect2) = False And Laser3Desc(intCount2).StillColliding = True Then
                        Laser3Desc(intCount2).StillColliding = False
                    End If
                End If
            Next
            For intCount = 0 To UBound(ObstacleDesc)
                If ObstacleDesc(intCount).Exists And Not ObstacleDesc(intCount).Invulnerable Then
                    With SrcRect
                        .Top = ObstacleDesc(intCount).y
                        .Bottom = .Top + ObstacleDesc(intCount).Height
                        .Left = ObstacleDesc(intCount).x
                        .Right = .Left + ObstacleDesc(intCount).Width
                    End With
                    If DetectCollision(SrcRect, SrcRect2) And Laser3Desc(intCount2).StillColliding = False Then
                        Laser3Desc(intCount2).StillColliding = True
                        ObstacleDesc(intCount).TimesHit = ObstacleDesc(intCount).TimesHit + Laser3Desc(intCount2).Damage
                        If ObstacleDesc(intCount).TimesHit > ObstacleDesc(intCount).TimesDies Then
                            lngScore = lngScore + ObstacleDesc(intCount).Score
                            dsExplosionDuplicate(DSExplosionIndex).SetCurrentPosition 0
                            dsExplosionDuplicate(DSExplosionIndex).SetPan (SrcRect.Left - (SCREENWIDTH \ 2)) * 3
                            dsExplosionDuplicate(DSExplosionIndex).Play DSBPLAY_DEFAULT
                            DSExplosionIndex = DSExplosionIndex + 1
                            CreateExplosion SrcRect, ObstacleDesc(intCount).ExplosionIndex
                            If ObstacleDesc(intCount).HasFired Then
                                TempDesc = ObstacleDesc(intCount)
                                blnTempDesc = True
                            Else
                                blnTempDesc = False
                            End If
                            If ObstacleDesc(intCount).HasDeadIndex Then
                                ObstacleDesc(intCount) = ObstacleContainerInfo(ObstacleDesc(intCount).DeadIndex)
                                With ObstacleDesc(intCount)
                                    .Exists = True
                                    .x = SrcRect.Left
                                    .Index = ObstacleDesc(intCount).DeadIndex
                                    .y = SrcRect.Top
                                    If blnTempDesc Then
                                        .XFire = TempDesc.XFire
                                        .YFire = TempDesc.YFire
                                        .TargetX = TempDesc.TargetX
                                        .TargetY = TempDesc.TargetY
                                    End If
                                End With
                            Else
                                ObstacleDesc(intCount).Exists = False
                            End If
                            
                            Exit Sub
                        Else
                            UpdateHits True, SrcRect2.Left, SrcRect2.Top
                            Exit Sub
                        End If
                    ElseIf DetectCollision(SrcRect, SrcRect2) = False And Laser3Desc(intCount2).StillColliding = True Then
                        Laser3Desc(intCount2).StillColliding = False
                    End If
                End If
            Next
        End If
    Next
    For intCount2 = 0 To UBound(GuidedMissile)
        If GuidedMissile(intCount2).Exists = True Then
            With SrcRect2
                .Top = GuidedMissile(intCount2).y
                .Bottom = .Top + MISSILEDIMENSIONS
                .Left = GuidedMissile(intCount2).x
                .Right = .Left + MISSILEDIMENSIONS
            End With
            For intCount = 0 To UBound(EnemyDesc)
                If EnemyDesc(intCount).Exists Then
                    With SrcRect
                        .Top = EnemyDesc(intCount).y
                        .Bottom = .Top + EnemyDesc(intCount).Height
                        .Left = EnemyDesc(intCount).x
                        .Right = .Left + EnemyDesc(intCount).Width
                    End With
                    If DetectCollision(SrcRect, SrcRect2) Then
                        EnemyDesc(intCount).TimesHit = EnemyDesc(intCount).TimesHit + 10
                        dsExplosionDuplicate(DSExplosionIndex).SetCurrentPosition 0
                        dsExplosionDuplicate(DSExplosionIndex).SetPan (SrcRect.Left - (SCREENWIDTH \ 2)) * 3
                        dsExplosionDuplicate(DSExplosionIndex).Play DSBPLAY_DEFAULT
                        DSExplosionIndex = DSExplosionIndex + 1
                        If EnemyDesc(intCount).TimesHit > EnemyDesc(intCount).TimesDies And Not EnemyDesc(intCount).Invulnerable Then
                            EnemyDesc(intCount).Exists = False
                            lngScore = lngScore + EnemyDesc(intCount).Score
                            CreateExplosion SrcRect, EnemyDesc(intCount).ExplosionIndex
                        Else
                            CreateExplosion SrcRect, EnemyDesc(intCount).ExplosionIndex, True
                        End If
                        GuidedMissile(intCount2).Exists = False
                        GuidedMissile(intCount2).TargetSet = False
                        Exit Sub
                    End If
                End If
            Next
            For intCount = 0 To UBound(ObstacleDesc)
                If ObstacleDesc(intCount).Exists And Not ObstacleDesc(intCount).Invulnerable Then
                    With SrcRect
                        .Top = ObstacleDesc(intCount).y
                        .Bottom = .Top + ObstacleDesc(intCount).Height
                        .Left = ObstacleDesc(intCount).x
                        .Right = .Left + ObstacleDesc(intCount).Width
                    End With
                    If DetectCollision(SrcRect, SrcRect2) Then
                        ObstacleDesc(intCount).TimesHit = ObstacleDesc(intCount).TimesHit + 10
                        dsExplosionDuplicate(DSExplosionIndex).SetCurrentPosition 0
                        dsExplosionDuplicate(DSExplosionIndex).SetPan (SrcRect.Left - (SCREENWIDTH \ 2)) * 3
                        dsExplosionDuplicate(DSExplosionIndex).Play DSBPLAY_DEFAULT
                        DSExplosionIndex = DSExplosionIndex + 1
                        If ObstacleDesc(intCount).TimesHit > ObstacleDesc(intCount).TimesDies Then
                            lngScore = lngScore + ObstacleDesc(intCount).Score
                            If ObstacleDesc(intCount).HasFired Then
                                TempDesc = ObstacleDesc(intCount)
                                blnTempDesc = True
                            Else
                                blnTempDesc = False
                            End If
                            If ObstacleDesc(intCount).HasDeadIndex Then
                                ObstacleDesc(intCount) = ObstacleContainerInfo(ObstacleDesc(intCount).DeadIndex)
                                With ObstacleDesc(intCount)
                                    .Exists = True
                                    .x = SrcRect.Left
                                    .y = SrcRect.Top
                                    .Index = ObstacleDesc(intCount).DeadIndex
                                    If blnTempDesc Then
                                        .XFire = TempDesc.XFire
                                        .YFire = TempDesc.YFire
                                        .TargetX = TempDesc.TargetX
                                        .TargetY = TempDesc.TargetY
                                    End If
                                End With
                            Else
                                ObstacleDesc(intCount).Exists = False
                            End If
                            CreateExplosion SrcRect, ObstacleDesc(intCount).ExplosionIndex
                        Else
                            CreateExplosion SrcRect, ObstacleDesc(intCount).ExplosionIndex, True
                        End If
                        GuidedMissile(intCount2).Exists = False
                        GuidedMissile(intCount2).TargetSet = False
                        Exit Sub
                    End If
                End If
            Next
        End If
     Next
                                                
End Sub

Private Function DetectCollision(ObjectRectA As RECT, ObjectRectB As RECT) As Boolean
    
    'This sub takes two rectangles and determines if they overlap each other
    
    Dim ReturnRect As RECT                                              'rectangle structure
    
    If IntersectRect(ReturnRect, ObjectRectA, ObjectRectB) Then       'If the two rectangles overlap in any way
        DetectCollision = True                                          'return True
    End If
    
End Function
Private Sub CreateExplosion(Coordinates As RECT, ExplosionIndex As Byte, Optional NoCredit As Boolean = False)
    
    'This sub creates the explosions that appear when a player destroys an object. The index controls which
    'explosion bitmap to play. Player explosion is a flag so the player doesn't get credit for blowing himself up.
    'It also adds to the number of enemies the player has killed to be displayed upon level completion.
    
    Dim lngCount As Long                                'Standard count variable
    
    If NoCredit = False Then                            'If the NoCredit flag is not set
        intEnemiesKilled = intEnemiesKilled + 1         'The number of enemies the player has killed that count toward a powerup being triggered is incremented
        lngNumEnemiesKilled = lngNumEnemiesKilled + 1   'The total number of enemies the player has killed is incremented
        If intEnemiesKilled = 25 Then                   'If the number of enemies the player has killed exceeds 25, then
            intEnemiesKilled = 0                        'Reset the enemies killed power up trigger count to 0
            UpdatePowerUps True                         'Trigger a powerup
        End If
    End If

    For lngCount = 0 To UBound(ExplosionDesc)           'loop through the whole explosion array
        If ExplosionDesc(lngCount).Exists = False Then  'if we find an empty array element
            With ExplosionDesc(lngCount)
                .ExplosionIndex = ExplosionIndex        'Set the explosion type to the enemys'
                .Exists = True                          'this array element now exists
                .Frame = 0                              'set its' frame to the first one
                .x = (((Coordinates.Right - Coordinates.Left) \ 2) + Coordinates.Left) - (ExplosionDesc(lngCount).Width \ 2)
                                                        'assign it the center of the object, at the edge
                .y = (((Coordinates.Bottom - Coordinates.Top) \ 2) + Coordinates.Top) - (ExplosionDesc(lngCount).Height \ 2)
                                                        'assign it the center of the object, along the edge
            End With
            Exit Sub
        End If
    Next
        
End Sub
Private Sub UpdateExplosions()
    
    'Thsi subroutine updates the animation for the large explosions
    
    Dim lngCount As Long                                'count variable
    Dim SrcRect As RECT                                 'source rectangle
    Dim lngRightOffset As Long                          'offset for the animation rectangle
    Dim lngLeftOffset As Long                           'offset for the animation rectangle
    Dim lngTopOffset As Long                            'offset for the animation rectangle
    Dim lngBottomOffset As Long                         'offset for the animation rectangle
    Dim FinalX As Long                                  'Final X coordinate for the rectangle
    Dim FinalY As Long                                  'FInal Y coordinate for the rectangle
    Dim TempY As Long                                   'Temporary Y position
    Dim TempX As Long                                   'Temporary X position
    Dim XOffset As Long                                 'X offset of the animation frame
    Dim YOffset As Long                                 'Y offset of the animation frame
    
    For lngCount = 0 To UBound(ExplosionDesc)           'Loop through all explosions
        lngLeftOffset = 0                               'Set all the variables to 0
        lngTopOffset = 0                                'Set all the variables to 0
        lngBottomOffset = 0                             'Set all the variables to 0
        TempX = 0                                       'Set all the variables to 0
        TempY = 0                                       'Set all the variables to 0
        XOffset = 0                                     'Set all the variables to 0
        YOffset = 0                                     'Set all the variables to 0
        With SrcRect                                    'Set all the variables to 0
            .Bottom = 0
            .Top = 0
            .Left = 0
            .Right = 0
        End With
        If ExplosionDesc(lngCount).Exists = True Then   'If this explosion exists then
            FinalX = ExplosionDesc(lngCount).x          'Start by getting the X coorindate of the explosion
            FinalY = ExplosionDesc(lngCount).y          'Get the Y coordinate of the explosion
            If ExplosionDesc(lngCount).x + ExplosionDesc(lngCount).Width > SCREENWIDTH Then
                                                        'If the explosion hangs off the right edge of the screen
                lngRightOffset = (SCREENWIDTH - (ExplosionDesc(lngCount).x + ExplosionDesc(lngCount).Width)) + ExplosionDesc(lngCount).Width
                                                        'Adjust the rectangle to compensate
            Else                                        'Otherwise
                lngRightOffset = ExplosionDesc(lngCount).Width
                                                        'The rectangle width is equal to the width of a single explosion frame
            End If
            If ExplosionDesc(lngCount).x < 0 Then       'If the explosion is off the left hand side of the screen
                lngLeftOffset = Abs(ExplosionDesc(lngCount).x)
                                                        'Adjust the rectangle to compensate
                lngRightOffset = ExplosionDesc(lngCount).Width + ExplosionDesc(lngCount).x
                                                        'Adjust the width of the rectangle
                FinalX = 0                              'The X coordinate is 0
            End If
            If ExplosionDesc(lngCount).y + ExplosionDesc(lngCount).Height > SCREENHEIGHT Then
                                                        'If the bottom of the explosion hangs off the bottom of the screen
                lngBottomOffset = (SCREENHEIGHT - (ExplosionDesc(lngCount).y + ExplosionDesc(lngCount).Height)) + ExplosionDesc(lngCount).Height
                                                        'Adjust the rectangle to compensate
            Else                                        'Otherwise
                lngBottomOffset = ExplosionDesc(lngCount).Height
                                                        'The explosion rectangle is equal to the height of the explosion
            End If
            If ExplosionDesc(lngCount).y < 0 Then       'If the explosion hangs off the top of the screen
                lngTopOffset = Abs(ExplosionDesc(lngCount).y)
                                                        'Adjust the top of the rectangle to compensate
                lngBottomOffset = ExplosionDesc(lngCount).Height + ExplosionDesc(lngCount).y
                                                        'Adjust the height of the rectangle as well
                FinalY = 0                              'The Y coordinate is set to 0
            End If
            
            If ExplosionDesc(lngCount).Frame > ExplosionDesc(lngCount).NumFrames Then
                                                        'If the animation frame goes beyond the number of frames the that the explosion has
                ExplosionDesc(lngCount).Frame = 0       'Reset the frame to the first one
                ExplosionDesc(lngCount).Exists = False  'The explosion no longer exists
                Exit Sub
            End If
            TempY = ExplosionDesc(lngCount).Frame \ 4   'Calculate the left of the rectangle
            TempX = ExplosionDesc(lngCount).Frame - (TempY * 4)
                                                        'Calculate the top of the rectang;e
            XOffset = CLng(TempX * ExplosionDesc(lngCount).Width)
                                                        'Calculate the right of the rectangle
            YOffset = CLng(TempY * ExplosionDesc(lngCount).Height)
                                                        'Calculate the bottom of the rectangle
            With SrcRect                                'Place the above calculated values in the rectangle struct
                .Top = 0 + YOffset + lngTopOffset
                .Bottom = .Top + lngBottomOffset
                .Left = 0 + XOffset + lngLeftOffset
                .Right = .Left + lngRightOffset
                If .Top >= .Bottom Then Exit Sub
                If .Left >= .Right Then Exit Sub
            End With

            ddsBack.BltFast FinalX, FinalY, ddsExplosion(ExplosionDesc(lngCount).ExplosionIndex), SrcRect, DDBLTFAST_WAIT Or DDBLTFAST_SRCCOLORKEY
                                                        'Blit the explosion frame to the screen
            ExplosionDesc(lngCount).Frame = ExplosionDesc(lngCount).Frame + 1
                                                        'Increment the frame the explosion is on
        End If
    Next
            
End Sub
Private Sub UpdateShields()
    
    'This sub updates the shield display and also checks whether or not there are any shields left, as well as
    'updating the players lives. If there are no lives left, it will reset the game.
    
    Dim SrcRect As RECT                                     'The source rectangle for the shield indicator
    Dim hDDWindow As Long                                   'A variable to store the handle to the direct draw surface
    Dim lngTime As Long                                     'variable to store the current tick count
    Dim lngTargetTime As Long                               'variable to store a target tick count
    Dim lngTargetTick As Long                               'variable to stabilize frame rate
    Dim intCount As Integer                                 'standard loop variable
    
    If intShields > 0 Then                                  'if there is more than 0% shields left
        With SrcRect                                        'define the shield indicator coordinates
            .Top = 0
            .Bottom = 20
            .Left = 0
            .Right = intShields                             'intShields is the right hand side of the rectangle, which will grow smaller as the player takes more damage
        End With
        
        ddsBack.SetForeColor RGB(255, 255, 255)             'set the fore color to white
        ddsBack.DrawBox 449, 6, 551, 28                     'draw a box for the shield indicator
        ddsBack.BltFast 450, 7, ddsShieldIndicator, SrcRect, DDBLTFAST_WAIT Or DDBLTFAST_NOCOLORKEY
                                                            'blt the indicator rectangle to the screen
        DrawText 390, 10, "Shields:", RGB(255, 200, 200)    'display some text
        If intShields < 25 Then                             'if the shields are less than 25% then
            dsAlarm.Play DSBPLAY_LOOPING                    'play the alarm sound effect, and loop it
            Ship.AlarmActive = True                         'set the alarm flag to on
        Else                                                'otherwise
            dsAlarm.Stop                                    'make sure the alarm sound effect is off
            Ship.AlarmActive = False                        'the flag is set to off
        End If
    Else                                                    'The player has died
        dsPlayerDies.SetCurrentPosition 0                   'set the dies wave to the beginning
        dsPlayerDies.Play DSBPLAY_DEFAULT                   'play the explosion sound
        If IsFF = True Then ef(3).start 1, 0                'if force feedback is enabled then start the death effect
        If IsFF = True Then ef(2).Unload                    'disable the trigger force feedback effect
        dsAlarm.Stop                                        'stop playing the alarm sound effect
        With SrcRect                                        'setup a rectangle structure for the explosion
            .Top = Ship.y
            .Bottom = .Top + SHIPHEIGHT
            .Left = Ship.x
            .Right = .Left + SHIPWIDTH
        End With
        CreateExplosion SrcRect, 0, True                    'create an explosion where the player was
        lngTime = dx.TickCount                              'get the current tick count
        For intCount = 0 To UBound(EnemyDesc)               'loop through all the enemies and
            EnemyDesc(intCount).Exists = False              'the enemies no longer exist
            EnemyDesc(intCount).HasFired = False            'the enemies' weapons no longer exist
        Next
        For intCount = 0 To UBound(GuidedMissile)           'loop through all the players guided missiles
            GuidedMissile(intCount).Exists = False          'they no longer exist
        Next
        For intCount = 0 To UBound(ObstacleDesc)            'make all the obstacles non-existent
            ObstacleDesc(intCount).Exists = False           'the obstacle doesn't exist
            ObstacleDesc(intCount).HasFired = False         'the obstacle hasn't fired
        Next
        For intCount = 0 To UBound(PowerUp)
            PowerUp(intCount).Exists = False                'if there is a power up currently on screen, get rid of it
        Next
        byteLives = byteLives - 1                           'the player loses a life
        intShields = 100                                    'shields are at full again
        With Ship
            Ship.x = 300                                    'center the ships' X
            Ship.y = 300                                    'and Y
            Ship.PowerUpState = 0                           'the player is back to no powerups
            Ship.AlarmActive = False                        'the alarm flag is set to off
            Ship.FiringMissile = False                      'the firing missle flag is set to off
        End With
        SectionCount = SectionCount + 30                    'Set the player back a bit
        If SectionCount > 999 Then SectionCount = 999       'Make sure we don't go over the limit
        If byteLives > 0 Then                               'If the player still has lives left then
            Do Until dx.TickCount > lngTime + 2000          'Loop this for two seconds
                lngTargetTick = dx.TickCount                'get the current time
                ddsBack.BltColorFill EmptyRect, &H0         'fill the back buffer with black
                UpdateBackground                            'you seen this before
                UpdateStars                                 'this too
                UpdateExplosions                            'same here
                UpdateWeapons                               'as well as this
                DrawText 275, 200, "Lives left: " & byteLives, RGB(255, 255, 255)
                                                            'display a message letting the player know how many ships are left
                ddsFront.Flip Nothing, 0                    'flip the front buffer with the back
                Do Until dx.TickCount - lngTargetTick > 18  'Make sure the game doesn't get out of control
                Loop                                        'speed-wise by looping until we reach the targeted frame rate
            Loop                                            'keep looping until two seconds pass
            dsEnergize.SetCurrentPosition 0                 'set the energize sound effect to the beginning
            dsEnergize.Play DSBPLAY_DEFAULT                 'play the energize sound effect
            If IsFF = True Then ef(2).Download              'start the trigger force feedback again
        Else                                                'If the player has no lives left
            Do Until dx.TickCount > lngTime + 3000          'Loop for three seconds
                lngTargetTick = dx.TickCount                'get the current time
                ddsBack.BltColorFill EmptyRect, &H0         'fill the back buffer with black
                UpdateStars                                 'these lines are the same as above
                UpdateBackground
                UpdateExplosions
                UpdateWeapons
                DrawText 275, 200, "Game Over", RGB(255, 255, 255)
                                                            'display that the game is now over
                ddsFront.Flip Nothing, 0                    'flip the front and back surfaces
                DoEvents                                    'don't hog the processor
                Do Until dx.TickCount - lngTargetTick > 18  'Make sure the game doesn't get out of control
                Loop
            Loop                                            'continues looping for three seconds
            FadeScreen                                      'Fade the screen to black
            intShields = 100                                'shields are at 100%
            Ship.x = 300                                    'reset the players X
            Ship.y = 300                                    'and Y coordinates
            Ship.PowerUpState = 0                           'no powerups
            Ship.NumBombs = 0                               'the player has no bombs
            SectionCount = 999                              'start at the beginning
            byteLevel = 1                                   'level 1 starts over
            byteLives = 3                                   'the player has 3 lives left
            boolBackgroundExists = False                    'there is no background picture
            CheckHighScore                                  'call the sub to see if the player got a high score
            boolStarted = False                             'the game hasn't been started
        End If
    End If
    
End Sub
Public Sub FireMissile()
    
    'This routine fires a missle if the player has one in his possesion
    
    Dim intCount As Integer                                     'standard count variable
    Dim TempPal(0) As PALETTEENTRY                              'palette to increment through colors
    Dim OldPal(0) As PALETTEENTRY                               'palette to store the old palette
    Dim lngTargetTick As Long                                   'long value to hold the tick count
    Dim ExplosionRect As RECT                                   'rect structure that defines the position of an enemy ship
    Dim EmptyRect As RECT                                       'rect structure to pass when an empty rectangle structure is needed
    
    If Ship.NumBombs = 0 Then Exit Sub                          'if there aren't any missiles, exit the sub
    Ship.NumBombs = Ship.NumBombs - 1                           'otherwise, decrement the number of bombs the player has
    DDPalette.GetEntries 0, 1, OldPal()                         'store the current palette index in the oldpal structure
    For intCount = 0 To 255 Step 20                             'cycle through the palette
        lngTargetTick = dx.TickCount                            'get the current time and store it
        With TempPal(0)                                         'we are manipulating palette index 0
            .blue = 0                                           'set all elements to 0 except red
            .flags = 0
            .green = 0
            .red = intCount                                     'red we are going to be incrementing with the for...next loop count variable
        End With
        FrameCount = FrameCount + 1                             'Keep track of the frame increment
        If FrameCount >= 20 Then                                'When 20 frames elapsed
            SectionCount = SectionCount - 1                     'Reduce the section the player is on
            UpdateLevels                                        'Update the section
            FrameCount = 0                                      'Reset the frame count
        End If
        
        'Since this sub will be looping until we finish manipulating the palette, we will need to call all of the normal
        'main functions from here to maintain gameplay while we are busy with this sub
        
        Call GetInput                                           'Get input from the player
        Call CheckForCollisions                                 'Check to see if there are any collisions
        ddsBack.BltColorFill EmptyRect, 0                       'Fill the back buffer with black
        Call UpdateBackground(True)                             'Update the background bitmap, using a transparent blit
        Call UpdateStars                                        'Update the stars
        Call UpdateObstacles                                    'Update all obstacles
        Call UpdateEnemys                                       'Update the enemies
        Call UpdatePowerUps                                     'Update the powerups
        Call UpdateWeapons                                      'Update the weapon fire
        Call UpdateExplosions                                   'Update the explosions
        Call UpdateShip                                         'Update the players ship
        If Ship.Invulnerable Then UpdateInvulnerability         'If the player is invulnerable, update the invulenerability animation
        UpdateShields                                           'Update the shield indicator
        UpdateBombs                                             'Update the missile animation
        DrawText 30, 10, "Score: " & Format$(lngScore, "###,###,###"), RGB(149, 248, 153)
                                                                'Display the score
        DrawText 175, 10, "Lives: " & byteLives, RGB(149, 248, 153)
                                                                'Display lives left
        DrawText 560, 10, "Level: " & byteLevel, RGB(149, 248, 153)
                                                                'Display the current level
        DDPalette.SetEntries 0, 1, TempPal()                    'Set the palette to our new palette entry values

        If Not boolMaxFrameRate Then
            Do Until dx.TickCount - lngTargetTick > 18          'Make sure the game doesn't get out of control
            Loop                                                'speed-wise by looping until we reach the targeted frame rate
        Else
            DrawText 30, 45, "MaxFrameRate enabled", RGB(255, 255, 255)
                                                                'Let the player know there is no frame rate limitation
        End If
        ddsFront.Flip Nothing, 0                                'Flip the front buffer with the back buffer
    Next
    dsMissile.SetCurrentPosition 0                              'set the missile wav buffer position to 0
    dsMissile.Play DSBPLAY_DEFAULT                              'play the missile wav
    If IsFF = True Then ef(0).start 1, 0                        'if force feedback exists, start the missile effect
    For intCount = 0 To UBound(EnemyDesc)                       'loop through all the enemies
        If EnemyDesc(intCount).Exists And Not EnemyDesc(intCount).Invulnerable Then
                                                                'if the enemy exists on screen, and is not invulnerable
            With ExplosionRect                                  'set the explosion rectangle coordinates
                .Top = EnemyDesc(intCount).y
                .Bottom = .Top + EnemyDesc(intCount).Height
                .Left = EnemyDesc(intCount).x
                .Right = .Left + EnemyDesc(intCount).Width
            End With
            CreateExplosion ExplosionRect, EnemyDesc(intCount).ExplosionIndex
                                                                'call the sub that creates large explosions and plays the explosion sound
            With EnemyDesc(intCount)
                .HasFired = False                               'erase any existing enemy fire
                .TimesHit = .TimesHit + 30                      'the missile does 30x the normal laser 1 damage, add this value to the number of times the enemy has been hit
            End With
            If EnemyDesc(intCount).TimesHit >= EnemyDesc(intCount).TimesDies Then
                                                                'check to see if the enemy has been hit more times than it takes for it to die, if it has
                With EnemyDesc(intCount)                        'reset the enemy
                    .Exists = False                             'the enemy no longer exists
                    .TargetX = 0                                'it has no x target
                    .TargetY = 0                                'it has no y target
                    .TimesHit = 0                               'it has never been hit
                    .XVelocity = 0                              'there is no velocity
                End With
            End If
        End If
    Next
    
    'The rest of the sub takes the red index, and increments it back to black, while mainting normal gameplay procedures
    
    For intCount = 255 To 0 Step -5
        lngTargetTick = dx.TickCount
        With TempPal(0)
            .blue = 0
            .flags = 0
            .green = 0
            .red = intCount
        End With
        FrameCount = FrameCount + 1
        If FrameCount >= 20 Then
            SectionCount = SectionCount - 1
            UpdateLevels
            FrameCount = 0
        End If
        GetInput
        CheckForCollisions
        ddsBack.BltColorFill EmptyRect, &H0
        UpdateBackground True
        UpdateStars
        UpdateObstacles
        UpdateEnemys
        UpdatePowerUps
        UpdateWeapons
        UpdateExplosions
        UpdateShip
        If Ship.Invulnerable Then UpdateInvulnerability
        UpdateShields
        UpdateBombs
        DrawText 30, 10, "Score: " & Format$(lngScore, "###,###,###"), RGB(149, 248, 153)
        DrawText 175, 10, "Lives: " & byteLives, RGB(149, 248, 153)
        DrawText 560, 10, "Level: " & byteLevel, RGB(149, 248, 153)
        DDPalette.SetEntries 0, 1, TempPal()
        If Not boolMaxFrameRate Then
            Do Until dx.TickCount - lngTargetTick > 18
            Loop
        Else
            DrawText 30, 45, "MaxFrameRate enabled", RGB(255, 255, 255)
        End If
        ddsFront.Flip Nothing, 0
    Next
    dd.WaitForVerticalBlank DDWAITVB_BLOCKBEGIN, 0
    DDPalette.SetEntries 0, 1, OldPal()
    Ship.FiringMissile = False                              'The ship is no longer firing a missle
    
End Sub
Private Sub UpdateBackground(Optional Transparent As Boolean = False)
        
    'This sub updates the large slow scrolling bitmap in the background of the level. The optional parameter is
    'flagged when a missile is launched, and we need to do a transparent bilt instead of a solid blit. Transparent
    'blits take more time, so this optimizes the efficiency.
    
    Dim sngFinalX As Single                                         'the final X position of the bitmap
    Dim sngFinalY As Single                                         'the final Y position of the bitmap
    Dim OffsetTop As Integer                                        'The offset of the top of the bitmap
    Dim OffsetBottom As Integer                                     'The offset of the bottom of the bitmap
    Dim SrcRect As RECT                                             'Source rectangle of the bitmap
    
    If boolBackgroundExists = False Then                            'If there is no background bitmap,
        Exit Sub                                                    'exit the sub
    Else                                                            'otherwise
        sngBackgroundY = sngBackgroundY + 0.1                       'increment the Y position of the bitmap
        If sngBackgroundY + BackgroundObject(intObjectIndex).Height < 0 Then Exit Sub
                                                                    'if the y position is less than 0 exit the sub
        If sngBackgroundY >= SCREENHEIGHT - 1 Then                  'if the bitmap is less that the height of the screen then
            boolBackgroundExists = False                            'the bitmap no longer exists, since it has left the screen
        Else                                                        'otherwise
            If sngBackgroundY + BackgroundObject(intObjectIndex).Height >= SCREENHEIGHT Then
                                                                    'if bitmap is partially on the bottom of the screen
                OffsetBottom = SCREENHEIGHT - (sngBackgroundY + BackgroundObject(intObjectIndex).Height)
                                                                    'set the offset amount to adjust for this
            Else                                                    'otherwise
                OffsetBottom = 0                                    'there is no offset
            End If
            If sngBackgroundY <= 0 Then                             'if the bitmap Y coordinate is partially off the top of the screen
                OffsetTop = Not sngBackgroundY                      'set the offset to reflect this
                If OffsetTop < 0 Then OffsetTop = 0                 'if the offset is less than 0, set the offset to 0
                sngFinalY = 0                                       'the final Y coordinate is 0
            Else                                                    'otherwise
                sngFinalY = sngBackgroundY                          'the finaly Y coordinate is the same as the background Y coordinate
                OffsetTop = 0                                       'and there is no offset
            End If
                                    
            With SrcRect                                            'define the source rectangle for the background bitmap
                .Top = OffsetTop                                    'the top is the topoffset variable
                .Bottom = BackgroundObject(intObjectIndex).Height + OffsetBottom
                                                                    'for the bottom of the rect, and the height of the object plus the offset
                .Left = 0                                           'left is always 0
                .Right = .Left + BackgroundObject(intObjectIndex).Width
                                                                    'right is set to the width of the bitmap
            End With
            If Not Transparent Then                                 'if we are not doing a transparent blit then
                ddsBack.BltFast sngBackgroundX, sngFinalY, ddsBackgroundObject(intObjectIndex), SrcRect, DDBLTFAST_WAIT Or DDBLTFAST_NOCOLORKEY
                                                                    'blit the background object to the backbuffer without using a colorkey
            Else                                                    'otherwise, we are doing a transparent blit, so
                ddsBack.BltFast sngBackgroundX, sngFinalY, ddsBackgroundObject(intObjectIndex), SrcRect, DDBLTFAST_WAIT Or DDBLTFAST_SRCCOLORKEY
                                                                    'blit the background object to the backbuffer, using a source color key
            End If
        End If
    End If
       
 End Sub

Private Sub UpdateShip()
         
    'This sub updates the player's ship, and animates it
    
    Dim SrcRect As RECT                                     'source rectangle
    Dim TempX  As Integer                                   'X poistion of the animation
    Dim TempY As Integer                                    'Y position of the animation
    Static byteFrameDirection As Byte                       'keep track of the direction the animation is moving
        
    If intShipFrameCount > 29 And byteFrameDirection = 0 Then
                                                            'if the end of the animation is reached
         byteFrameDirection = 1                             'reverse the direction
    ElseIf intShipFrameCount < 1 And byteFrameDirection = 1 Then
                                                            'if the end is reached the other direction
         byteFrameDirection = 0                             'reverse the direction
    End If
    
    If byteFrameDirection = 1 Then                          'if the animation is headed backwards
        intShipFrameCount = intShipFrameCount - 1           'reduce the frame the animation is on
    Else                                                    'otherwise
        intShipFrameCount = intShipFrameCount + 1           'increment the animation frame
    End If
    
    TempY = intShipFrameCount \ 4                           'find the left of the animation
    TempX = intShipFrameCount - (TempY * 4)                 'find the top of the animation
    Ship.XOffset = CLng(TempX * SHIPWIDTH)                  'set the X offset of the animation frame
    Ship.YOffset = CLng(TempY * SHIPHEIGHT)                 'set the Y offset of the animation frame
    
    With SrcRect                                            'fill in the values of the animation frame
        .Top = Ship.YOffset
        .Bottom = .Top + SHIPHEIGHT
        .Left = Ship.XOffset
        .Right = .Left + SHIPWIDTH
    End With
    
    If Abs(Ship.XVelocity) > XMAXVELOCITY Then              'if the ship reaches the maximum velocity in this direction then
        If Ship.XVelocity < 0 Then                          'if the ship is headed to the left of the screen
            Ship.XVelocity = XMAXVELOCITY - XMAXVELOCITY - XMAXVELOCITY
                                                            'set the velocity to equal the maximum velocity in this direction
        Else                                                'otherwise
            Ship.XVelocity = XMAXVELOCITY                   'set the velocity to equal the maximum velocity in this direction
        End If
    End If
    If Abs(Ship.YVelocity) > YMAXVELOCITY Then              'if the ship reaches the maximum velocity in this direction then
        If Ship.YVelocity < 0 Then                          'if the ship is headed to the top of the screen
            Ship.YVelocity = YMAXVELOCITY - YMAXVELOCITY - YMAXVELOCITY
                                                            'set the velocity to equal the maximum velocity in this direction
        Else                                                'otherwise
            Ship.YVelocity = YMAXVELOCITY                   'set the velocity to equal the maximum velocity in this direction
        End If
    End If
    
    If Ship.XVelocity > 0 Then                              'if the ship's velocity is positive
        Ship.XVelocity = Ship.XVelocity - FRICTION          'subtract some of the velocity using friction
        If Ship.XVelocity < 0 Then Ship.XVelocity = 0       'if the ship goes under zero velocity, the ship has no velocity anymore
    ElseIf Ship.XVelocity < 0 Then                          'otherwise, if the ship has negative velocity
        Ship.XVelocity = Ship.XVelocity + FRICTION          'add some friction to the negative value
        If Ship.XVelocity > 0 Then Ship.XVelocity = 0       'if the ship goes above 0, the ship no longer has velocity
    End If
    If Ship.YVelocity > 0 Then                              'if the ships Y velocity is positive
        Ship.YVelocity = Ship.YVelocity - FRICTION          'subtract some of the velocity using friction
        If Ship.YVelocity < 0 Then Ship.YVelocity = 0       'if the ship goes under zero velocity, the ship has no velocity anymore
    ElseIf Ship.YVelocity < 0 Then                          'otherwise, if the ship has negative velocity
        Ship.YVelocity = Ship.YVelocity + FRICTION          'add some friction to the negative value
        If Ship.YVelocity > 0 Then Ship.YVelocity = 0       'if the ship goes above 0, the ship no longer has velocity
    End If
    
    Ship.x = Ship.x + Ship.XVelocity                        'increment the ship's X position by the amount of velocity
    Ship.y = Ship.y + Ship.YVelocity                        'increment the ship's Y position by the amount of velocity
    
    If Ship.x < 0 Then Ship.x = 0                           'if the ship hits the left of the screen, set the X to 0
    If Ship.y < 0 Then Ship.y = 0                           'if the ship hits the bottom of the screen, set the Y to 0
    If Ship.x >= SCREENWIDTH - SHIPWIDTH Then Ship.x = SCREENWIDTH - SHIPWIDTH
                                                            'if the ship hits the right of the screen, set it to the right edge
    If Ship.y >= SCREENHEIGHT - SHIPHEIGHT Then Ship.y = SCREENHEIGHT - SHIPHEIGHT
                                                            'if the ship hits the bottom of the screen, set it to the bottom edge

    ddsBack.BltFast Ship.x, Ship.y, ddsShip, SrcRect, DDBLTFAST_WAIT Or DDBLTFAST_SRCCOLORKEY
                                                            'blit the ship to the screen
End Sub
 Private Sub UpdateEnemys()
    
    'This sub updates all the enemies that are being displayed on the screen
    
    Dim intCount As Integer                             'count variable
    Dim SrcRect As RECT                                 'source rectangle for the blit
    Dim intReturnResult As Integer                      'return holder
    Dim sngChaseSpeed As Single                         'chase speed of the enemy
    Dim TempX As Integer                                'temporary X coordinate
    Dim TempY As Integer                                'temporary Y coordinate
    Dim XOffset As Integer                              'X offset of the animation frame
    Dim YOffset As Integer                              'Y offset of the animation frame
    Dim FinalY As Long                                  'Finaly Y position of the enemy
    Dim FinalX As Long                                  'Final X position of the enemy
    Dim lngTopOffset As Long                            'Offset of the top of the rectangle
    Dim lngBottomOffset As Long                         'Offset of the bottom of the rectangle
    Dim lngRightOffset As Long                          'Offset of the right of the rectangle
    Dim lngLeftOffset As Long                           'Offset of the left of the rectangle
    
    Randomize                                           'reset the random seed generator
    
    For intCount = 0 To UBound(EnemyDesc)               'loop through all the enemies
        
        'these next lines reset all the variables to zero
        lngTopOffset = 0
        lngBottomOffset = 0
        lngRightOffset = 0
        lngLeftOffset = 0
        XOffset = 0
        YOffset = 0
        If EnemyDesc(intCount).Exists Then              'if the enemy exists
            EnemyDesc(intCount).y = EnemyDesc(intCount).y + EnemyDesc(intCount).Speed
                                                        'increment the enemies Y position by its' speed
            FinalY = EnemyDesc(intCount).y              'start off with the final Y the same as the enemies Y
            If EnemyDesc(intCount).y + EnemyDesc(intCount).Height > SCREENHEIGHT Then
                                                        'if the enemy is partially off the bottom of the screen
                lngBottomOffset = (SCREENHEIGHT - (EnemyDesc(intCount).y + EnemyDesc(intCount).Height)) + EnemyDesc(intCount).Height
                                                        'adjust the offset of the rectangle to compensate
            Else                                        'otherwise
                lngBottomOffset = EnemyDesc(intCount).Height
                                                        'blit the whole enemy
            End If
            If EnemyDesc(intCount).y < 0 Then           'if the enemy is partially off the top of the screen
                lngTopOffset = Abs(EnemyDesc(intCount).y)
                                                        'adjust the top offset of the rectangle to compensate
                lngBottomOffset = EnemyDesc(intCount).Height + EnemyDesc(intCount).y
                                                        'also adjust the bottom offset of the rectangle to compensate
                FinalY = 0                              'the Y position of the rectangle will be zero
            End If
            If EnemyDesc(intCount).y < SCREENHEIGHT Then
                                                        'if the enemy is on the screen then
                If Ship.y > EnemyDesc(intCount).y Then  'if the the enemyies Y coorindate is larger than the players ship
                    If EnemyDesc(intCount).ChaseValue > 0 Then
                                                        'if the enemy has a chase value
                        If EnemyDesc(intCount).ChaseValue = CHASEFAST Then sngChaseSpeed = 0.2
                                                        'if the enemy is supposed to rapidly follow the players X coordinate, set it to a large increment
                        If EnemyDesc(intCount).ChaseValue = CHASESLOW Then sngChaseSpeed = 0.05
                                                        'if the enemy is supposed to slowly follow the players X coordinate, set it to a smaller increment
                        
                        If (Ship.x + (SHIPWIDTH \ 2)) < (EnemyDesc(intCount).x + (EnemyDesc(intCount).Width \ 2)) Then
                                                        'if the player is to the left of the enemy
                            EnemyDesc(intCount).XVelocity = EnemyDesc(intCount).XVelocity - sngChaseSpeed
                                                        'make the enemy move to the left
                            If Abs(EnemyDesc(intCount).XVelocity) > XMAXVELOCITY Then EnemyDesc(intCount).XVelocity = XMAXVELOCITY - XMAXVELOCITY - XMAXVELOCITY
                                                        'if the enemies velocity is greater than the maximum velocity, reverse the direction of the enemy
                        ElseIf (Ship.x + (SHIPWIDTH \ 2)) > (EnemyDesc(intCount).x + (EnemyDesc(intCount).Width \ 2)) Then
                                                        'if the player is to the right of the enemy
                            EnemyDesc(intCount).XVelocity = EnemyDesc(intCount).XVelocity + sngChaseSpeed
                                                        'make the enemy move to the right
                            If Abs(EnemyDesc(intCount).XVelocity) > XMAXVELOCITY Then EnemyDesc(intCount).XVelocity = XMAXVELOCITY
                                                        'if the enemies velocity is greater than the maximum velocity, reverse the direction of the enemy
                        End If
                    End If
                End If
                
                EnemyDesc(intCount).x = EnemyDesc(intCount).x + EnemyDesc(intCount).XVelocity
                                                        'increment the X position of the enemy by its' velocity
                FinalX = EnemyDesc(intCount).x          'the final X will be set to the enemies X coordinate
                If EnemyDesc(intCount).x + EnemyDesc(intCount).Width > SCREENWIDTH Then
                                                        'if the enemy is partially off the screen to the right
                    lngRightOffset = (SCREENWIDTH - (EnemyDesc(intCount).x + EnemyDesc(intCount).Width)) + EnemyDesc(intCount).Width
                                                        'set the right offset of the animation rectangle to compensate
                Else                                    'otherwise
                    lngRightOffset = EnemyDesc(intCount).Width
                                                        'the right offset is equal to the width of the animation frame
                End If
                If EnemyDesc(intCount).x < 0 Then       'if the enemy is partially off the screen to the left
                    lngLeftOffset = Abs(EnemyDesc(intCount).x)
                                                        'set the left offset to compensate
                    lngRightOffset = EnemyDesc(intCount).Width + EnemyDesc(intCount).x
                                                        'set the right offset of the rectangle so the correct width will be displayed
                    FinalX = 0                          'the X will be set to zero, since we can't pass a negative number
                End If
                
                If EnemyDesc(intCount).FrameDelay > 0 Then
                                                        'if the frame delay count of this enemy is greater than zero,
                                                        'it means this enemy should have a delay in the number of frames
                                                        'that are displayed
                    EnemyDesc(intCount).FrameDelayCount = EnemyDesc(intCount).FrameDelayCount + 1
                                                        'increment it by one
                    If EnemyDesc(intCount).FrameDelayCount > EnemyDesc(intCount).FrameDelay Then
                                                        'if the delay count is larger than the frame delay
                        EnemyDesc(intCount).FrameDelayCount = 0
                                                        'reset the count
                        EnemyDesc(intCount).Frame = EnemyDesc(intCount).Frame + 1
                                                        'increment the animation frame by one
                    End If
                Else                                    'otherwise,
                    EnemyDesc(intCount).Frame = EnemyDesc(intCount).Frame + 1
                                                        'increment the frame displayed
                End If
                
                If EnemyDesc(intCount).Frame > EnemyDesc(intCount).NumFrames Then EnemyDesc(intCount).Frame = 0
                                                        'if the frame number goes over the number of frames this enemy
                                                        'has, reset the animation frame to the beginning
                TempY = EnemyDesc(intCount).Frame \ 4   'set the starting Y position for this animation frame
                TempX = EnemyDesc(intCount).Frame - (TempY * 4)
                                                        'set the starting X position for this animation frame
                XOffset = CLng(TempX * EnemyDesc(intCount).Width)
                                                        'set the X offset of the animation frame
                YOffset = CLng(TempY * EnemyDesc(intCount).Height)
                                                        'set the Y offset of the animation frame
                With SrcRect
                    .Top = 0 + YOffset + lngTopOffset   'set the top of the rect
                    .Bottom = .Top + lngBottomOffset    'the bottom is the top plus the bottom offset
                    .Left = 0 + XOffset + lngLeftOffset 'set the left offset
                    .Right = .Left + lngRightOffset     'the right is the left plus the right offset
                End With
                
                If (EnemyDesc(intCount).Width + EnemyDesc(intCount).x) > 1 And EnemyDesc(intCount).x < SCREENWIDTH And SrcRect.Right > SrcRect.Left And SrcRect.Bottom > SrcRect.Top Then
                                                        'make sure that the enemy is within the bounds for blitting
                    ddsBack.BltFast FinalX, FinalY, ddsEnemyContainer(EnemyDesc(intCount).Index), SrcRect, DDBLTFAST_WAIT Or DDBLTFAST_SRCCOLORKEY
                                                        'blit the enemy with a transparent key
                End If
            Else
                EnemyDesc(intCount).Exists = False      'otherwise, this enemy no longer exists
            End If
        End If
        
        If EnemyDesc(intCount).HasFired = False And EnemyDesc(intCount).Exists = True And EnemyDesc(intCount).DoesFire And EnemyDesc(intCount).y > 0 And (EnemyDesc(intCount).y + EnemyDesc(intCount).Height) < SCREENHEIGHT And EnemyDesc(intCount).x > 0 And (EnemyDesc(intCount).x + EnemyDesc(intCount).Width) < SCREENWIDTH Then
                                                        'This incredibly long line has a very important job. It makes sure that the enemy hasn't fired, that it exists, and that it is visible on the screen
            intReturnResult = Int((1500 - 1) * Rnd + 1) 'make a random number to to determine whether the enemy will fire
            If intReturnResult < 20 Then                'if the random number is less than 20, make the enemy fire
                If DSEnemyFireIndex > UBound(dsEnemyFireDuplicate) Then DSEnemyFireIndex = 0
                                                        'if we have reached the upper boundary of the duplicate sound buffer, reset the buffer to zero
                dsEnemyFireDuplicate(DSEnemyFireIndex).SetCurrentPosition 0
                                                        'set the position of the buffer to the beginning
                dsEnemyFireDuplicate(DSEnemyFireIndex).Play DSBPLAY_DEFAULT
                                                        'play the duplicate sound buffer
                DSEnemyFireIndex = DSEnemyFireIndex + 1 'increment the count for the number of active sound buffers
                If EnemyDesc(intCount).x < Ship.x Then  'if the players X coordinate is less than the enemies
                    EnemyDesc(intCount).TargetX = 3     'set the X fire direction to +3 pixels every frame
                Else                                    'otherwise
                    EnemyDesc(intCount).TargetX = -3    'set it to -3 pixels every frame
                End If

                If EnemyDesc(intCount).y < Ship.y Then  'if the enemy ship's Y coordinate is less than the ships then
                    EnemyDesc(intCount).TargetY = 3     'set the enemy fire to move +3 pixels every frame
                Else                                    'otherwise
                    EnemyDesc(intCount).TargetY = -3    'set the enemy fire to move -3 pixels every frame
                End If
                EnemyDesc(intCount).XFire = (EnemyDesc(intCount).Width / 2) + EnemyDesc(intCount).x
                                                        'center the enemies X fire
                EnemyDesc(intCount).YFire = (EnemyDesc(intCount).Height / 2) + EnemyDesc(intCount).y
                                                        'center the eneies Y fire
                EnemyDesc(intCount).HasFired = True     'the enemy has fired
            End If

        ElseIf EnemyDesc(intCount).HasFired = True Then 'otherwise, if the enemy has fired
            If EnemyDesc(intCount).FireType = TARGETEDFIRE Then
                                                        'if the type of fire that the enemy uses aims in the general direction of the player then
                EnemyDesc(intCount).XFire = EnemyDesc(intCount).XFire + EnemyDesc(intCount).TargetX
                                                        'increment the enemy X fire in the direction specified
                EnemyDesc(intCount).YFire = EnemyDesc(intCount).YFire + EnemyDesc(intCount).TargetY
                                                        'increment the enemy Y fire in the direction specified
            Else                                        'otherwise
                EnemyDesc(intCount).YFire = EnemyDesc(intCount).YFire + 5
                                                        'increment the Y fire only, by 5 pixels
            End If
            If EnemyDesc(intCount).FireFrameCount > 3 Then
                                                        'if we have reached the end of the number of frames to wait until it is time to change the fire animation frame then
                EnemyDesc(intCount).FireFrameCount = 0  'reset the counter
                If EnemyDesc(intCount).FireFrame = 5 Then
                                                        'if the enemy animation frame is on the second frame
                    EnemyDesc(intCount).FireFrame = 0   'reset the animation frame to the first one
                Else                                    'otherwise
                    EnemyDesc(intCount).FireFrame = 5   'change it to the second one
                End If
            Else                                        'otherwise
                EnemyDesc(intCount).FireFrameCount = EnemyDesc(intCount).FireFrameCount + 1
                                                        'increment the wait time
            End If

            With SrcRect
                .Top = 0                                'set the top of the enemy fire animation
                .Bottom = 5                             'the animation is 5 pixels high
                .Left = EnemyDesc(intCount).FireFrame   'set the left to which frame we are on
                .Right = .Left + 5                      'the width of the fire is 5 pixels
            End With

            If EnemyDesc(intCount).XFire > SCREENWIDTH - 5 Or EnemyDesc(intCount).XFire < 0 Or EnemyDesc(intCount).YFire > SCREENHEIGHT - 5 Or EnemyDesc(intCount).YFire < 0 Then
                                                        'if the enemy fire is off the visible screen
                EnemyDesc(intCount).HasFired = False    'the enemy hasn't fired
            Else                                        'otherwise
                ddsBack.BltFast EnemyDesc(intCount).XFire, EnemyDesc(intCount).YFire, ddsEnemyFire, SrcRect, DDBLTFAST_WAIT
                                                        'blit the enemy fire
            End If
        End If
    Next
    
    'The rest of the sub does the exact same thing that the code above does when firing an enemy weapon,
    'except it does it for any of the obstacles that have the ability to fire
    
    For intCount = 0 To UBound(ObstacleDesc)
        If ObstacleDesc(intCount).HasFired = False And ObstacleDesc(intCount).Exists = True And ObstacleDesc(intCount).DoesFire Then
            intReturnResult = Int((3000 - 1) * Rnd + 1)
            If intReturnResult < 20 Then
                If DSEnemyFireIndex > UBound(dsEnemyFireDuplicate) Then DSEnemyFireIndex = 0
                dsEnemyFireDuplicate(DSEnemyFireIndex).SetCurrentPosition 0
                dsEnemyFireDuplicate(DSEnemyFireIndex).Play DSBPLAY_DEFAULT
                DSEnemyFireIndex = DSEnemyFireIndex + 1
                If ObstacleDesc(intCount).x < Ship.x Then
                    ObstacleDesc(intCount).TargetX = 3
                Else
                    ObstacleDesc(intCount).TargetX = -3
                End If
                If ObstacleDesc(intCount).y < Ship.y Then
                    ObstacleDesc(intCount).TargetY = 3
                Else
                    ObstacleDesc(intCount).TargetY = -3
                End If
                ObstacleDesc(intCount).XFire = (ObstacleDesc(intCount).Width / 2) + ObstacleDesc(intCount).x
                ObstacleDesc(intCount).YFire = (ObstacleDesc(intCount).Height / 2) + ObstacleDesc(intCount).y
                ObstacleDesc(intCount).HasFired = True
            End If
        ElseIf ObstacleDesc(intCount).HasFired = True Then
            ObstacleDesc(intCount).XFire = ObstacleDesc(intCount).XFire + ObstacleDesc(intCount).TargetX
            ObstacleDesc(intCount).YFire = ObstacleDesc(intCount).YFire + ObstacleDesc(intCount).TargetY
            If ObstacleDesc(intCount).FireFrameCount > 3 Then
                ObstacleDesc(intCount).FireFrameCount = 0
                If ObstacleDesc(intCount).FireFrame = 5 Then
                    ObstacleDesc(intCount).FireFrame = 0
                Else
                    ObstacleDesc(intCount).FireFrame = 5
                End If
            Else
                ObstacleDesc(intCount).FireFrameCount = ObstacleDesc(intCount).FireFrameCount + 1
            End If
            With SrcRect
                .Top = 0
                .Bottom = 5
                .Left = ObstacleDesc(intCount).FireFrame
                .Right = .Left + 5
            End With
            If ObstacleDesc(intCount).XFire > SCREENWIDTH - 5 Or ObstacleDesc(intCount).XFire < 0 Or ObstacleDesc(intCount).YFire > SCREENHEIGHT - 5 Or ObstacleDesc(intCount).YFire < 0 Then
                ObstacleDesc(intCount).HasFired = False
            Else
                ddsBack.BltFast ObstacleDesc(intCount).XFire, ObstacleDesc(intCount).YFire, ddsEnemyFire, SrcRect, DDBLTFAST_WAIT Or DDBLTFAST_SRCCOLORKEY
            End If
        End If
    Next
    
End Sub
Private Sub UpdateObstacles()
    
    'This sub updates all the obstacles on the screen, and animates them if there are any animations for the obstacle
    
    Dim intCount As Integer                                     'count variable
    Dim TempX As Integer                                        'variable for left of the rectangle
    Dim TempY As Integer                                        'variable for top of the rectangle
    Dim XOffset As Long                                         'offset for the right of the rectangle
    Dim YOffset As Long                                         'offset for the bottom of the rectangle
    Dim OffsetTop As Integer                                    'the top offset of the animation frame
    Dim OffsetBottom As Integer                                 'the bottom offset of the animation frame
    Dim sngFinalY As Single                                     'the final Y position of the obstacle
    Dim SrcRect As RECT                                         'source rectangle
    
    For intCount = 0 To UBound(ObstacleDesc)                    'loop through all obstacles
        If ObstacleDesc(intCount).Exists Then                   'if this obstacle exists
            ObstacleDesc(intCount).y = ObstacleDesc(intCount).y + ObstacleDesc(intCount).Speed
                                                                'increment the obstacle by its' speed
            If ObstacleDesc(intCount).y >= SCREENHEIGHT Then    'if the obstacle goes completely off the screen
                ObstacleDesc(intCount).Exists = False           'the obstacle no longer exists
            Else                                                'otherwise
                XOffset = 0                                     'reset the X offset
                YOffset = 0                                     'reset the Y offset
                If ObstacleDesc(intCount).NumFrames > 0 Then    'if this obstacle has an animation
                    ObstacleDesc(intCount).Frame = ObstacleDesc(intCount).Frame + 1
                                                                'increment the frame the animation is on
                    If ObstacleDesc(intCount).Frame > ObstacleDesc(intCount).NumFrames Then ObstacleDesc(intCount).Frame = 0
                                                                'if the animation goes beyond the number of frames it has, reset it to the start
                    TempY = ObstacleDesc(intCount).Frame \ 4    'calculate the left of the animation
                    TempX = ObstacleDesc(intCount).Frame - (TempY * 4)
                                                                'calculate the top of the animation
                    XOffset = CLng(TempX * ObstacleDesc(intCount).Width)
                                                                'calculate the right of the animation
                    YOffset = CLng(TempY * ObstacleDesc(intCount).Height)
                                                                'calculate the bottom of the animation
                End If
                If ObstacleDesc(intCount).y < 0 Then            'if the obstacle is partially off the top of the screen
                    OffsetTop = (Not ObstacleDesc(intCount).y) + 1
                                                                'adjust the rectangle
                    sngFinalY = 0                               'the Y of the obstacle is 0
                Else                                            'otherwise
                    sngFinalY = ObstacleDesc(intCount).y        'set the y position of the obstacle
                    OffsetTop = 0                               'there is no top offset for the obstacle
                End If
                If ObstacleDesc(intCount).y + ObstacleDesc(intCount).Height >= SCREENHEIGHT Then
                                                                'if the obstacle is partially off the bottom of the screen
                    OffsetBottom = ObstacleDesc(intCount).Height + (SCREENHEIGHT - (ObstacleDesc(intCount).y + ObstacleDesc(intCount).Height))
                                                                'adjust the rectangle
                Else                                            'otherwise
                    OffsetBottom = ObstacleDesc(intCount).Height
                                                                'the bottom is the entire height of the obstacle
                End If
                With SrcRect                                    'enter in the values for the rectangle
                    .Top = OffsetTop + YOffset
                    .Bottom = OffsetBottom + YOffset
                    .Left = XOffset
                    .Right = .Left + ObstacleDesc(intCount).Width
                End With
                
                If SrcRect.Top < SrcRect.Bottom Then            'if the rectangle is valid then
                    If ObstacleDesc(intCount).Solid Then        'if the obstacle is a solid obstacle
                        ddsBack.BltFast ObstacleDesc(intCount).x, sngFinalY, ddsObstacle(ObstacleDesc(intCount).Index), SrcRect, DDBLTFAST_WAIT
                                                                'blit the obstacle with no color key
                    Else
                        ddsBack.BltFast ObstacleDesc(intCount).x, sngFinalY, ddsObstacle(ObstacleDesc(intCount).Index), SrcRect, DDBLTFAST_WAIT Or DDBLTFAST_SRCCOLORKEY
                                                                'otherwise blit it with a color key
                    End If
                End If
            End If
        End If
    Next

End Sub
Private Sub UpdateStars()
 
    'This sub creates as well as updates stars
    
    Dim intCount As Integer                             'count variable
    Dim SrcRect As RECT                                 'source rectangle
    
    Randomize                                           'reseed the random number generator

    For intCount = 0 To UBound(StarDesc)                'loop through all the stars
         If StarDesc(intCount).Exists = False Then      'if this star doesn't exist then
            If (Int((3500 - 1) * Rnd) + 1) <= 25 Then   'if a number between 3500 and 1 is less than 25 then
                With StarDesc(intCount)                 'begin creating a new star
                    .Exists = True                      'the star exists
                    .x = Int((SCREENWIDTH - 1) * Rnd) + 1
                                                        'set a random X coordinate
                    .y = 0                              'start at the top of the screen
                    .Index = Int((5 - 1) * Rnd) + 1     'set a random number for a color
                    .Speed = ((2 - 0.4) * Rnd) + 0.4    'set a random number for the speed of the star
                End With
            End If
        Else
            StarDesc(intCount).y = StarDesc(intCount).y + StarDesc(intCount).Speed
                                                        'increment the stars position by its' speed
            If StarDesc(intCount).y >= SCREENHEIGHT - 1 Then
                                                        'if the star goes off the screen
                StarDesc(intCount).y = 0                'set the stars Y position to 0
                StarDesc(intCount).Exists = False       'the star no longer exists
            Else                                        'otherwise
                With SrcRect                            'set the stars rectangle coordinates
                    .Top = 0
                    .Bottom = .Top + 1
                    .Left = StarDesc(intCount).Index
                    .Right = .Left + 1
                End With
                ddsBack.BltFast StarDesc(intCount).x, StarDesc(intCount).y, ddsStar, SrcRect, DDBLTFAST_WAIT Or DDBLTFAST_NOCOLORKEY
                                                        'blit the star to the screen
            End If
        End If
    Next

    Exit Sub
     
End Sub
Public Sub FireWeapon()

    'This sub fires the players weapons, and plays the associated wavefile
    
    Dim intCount As Integer                                         'Standard count variable for loops
    Static byteLaserCounter As Byte                                 'variable to hold the number of times this sub has been called to determine if it is time to let another laser be created
    Static byteGuidedMissileCounter As Byte                         'variable to hold the number of times this sub has been called to determine if it is time to let another guided missile be created
    Static byteLaser2Counter As Byte                                'variable to hold the number of times this sub has been called to determine if it is time to let another level2 laser (left side) be created
    Static byteLaser3Counter As Byte                                'variable to hold the number of times this sub has been called to determine if it is time to let another level2 laser (right side) be created
    Static DSLaser2Index As Integer                                 'variable to hold the number of times this sub has been called to determine if it is time to let another laser wave effect sound to be created
    Dim lngSoundPosition As Long                                    'the variable that holds the position of the sound
    
                                                                    'Stage 1 laser
    intCount = 0                                                    'reset the count loop variable
    byteLaserCounter = byteLaserCounter + 1                         'increment the number of lasers by 1
    If byteLaserCounter = 5 Then                                    'if we have looped through the sub 5 times
        Do Until intCount > 7                                       'loop through all the lasers
            If LaserDesc(intCount).Exists = False Then              'and see if there is an empty slot, and if there is
                With LaserDesc(intCount)                            'create a new laser description
                    .Exists = True                                  'the laser exists
                    .x = Ship.x + ((SHIPWIDTH \ 2) - (LASER1WIDTH \ 2))
                                                                    'center the laser fire
                    .y = Ship.y                                     'the laser starts at the same Y as the ship
                    .Damage = 1                                     'the amount of damage this laser does
                End With
                dsLaser.SetCurrentPosition 0                        'set the position of the buffer to 0
                dsLaser.SetPan (Ship.x - (SCREENWIDTH \ 2)) * 3     'pan the sound according to the ships location
                dsLaser.Play DSBPLAY_DEFAULT                        'play the laser sound
                Exit Do                                             'exit the do loop
            End If
            intCount = intCount + 1                                 'incrementing the count
        Loop                                                        'loop until we find an empty slot
        byteLaserCounter = 0                                        'reset the counter to 0
    End If
    
    If Ship.PowerUpState > 0 Then                                   'Guided missiles
        intCount = 0                                                'reset the count variable
        byteGuidedMissileCounter = byteGuidedMissileCounter + 1     'increment the counter
        If byteGuidedMissileCounter = 20 Then                       'if we called the sub 20 times, then
            Do Until intCount > UBound(GuidedMissile)               'loop through all the guided missile types
                If GuidedMissile(intCount).Exists = False Then      'if we find an empty slot
                    With GuidedMissile(intCount)                    'create a new guided missile
                        .Exists = True                              'the guided missile exists
                        .x = Ship.x + (SHIPWIDTH / 2)               'center the x coordinate
                        .y = Ship.y + (SHIPHEIGHT / 2)              'center the y coordinate
                        .XVelocity = 0                              'set the velocity to 0
                        .YVelocity = -4.5                           'set the y velocity to 4.5 pixels every frame
                        .Damage = 3                                 'the guided missile does 3 points of damage
                    End With
                    Exit Do                                         'exit the do loop
                End If
                intCount = intCount + 1                             'increment the count
            Loop
            byteGuidedMissileCounter = 0                            'reset the guided missile counter
        End If
    End If
    
    'The rest of the weapons are handled in just about the same manner as these were. You should be able to find
    'the similarities and figure out what is going on from there.
    
    If Ship.PowerUpState > 1 Then                                   'Stage 2 lasers, this weapon shoots lasers diagonally from the ship
        intCount = 0
        byteLaser2Counter = byteLaser2Counter + 1
        If byteLaser2Counter > 15 Then
            byteLaser2Counter = 0
            Do Until intCount > UBound(Laser2RDesc)
                If Laser2RDesc(intCount).Exists = False Then
                    With Laser2RDesc(intCount)
                        .Exists = True
                        .x = (Ship.x + SHIPWIDTH) - 15
                        .y = Ship.y + 14
                        .XVelocity = 0 + (LASERSPEED - 4)
                        .YVelocity = 0 - LASERSPEED
                        .Damage = 1
                    End With
                    dsLaser2Duplicate(DSLaser2Index).SetCurrentPosition 0
                    dsLaser2Duplicate(DSLaser2Index).SetPan (Ship.x - (SCREENWIDTH \ 2)) * 3
                    dsLaser2Duplicate(DSLaser2Index).Play DSBPLAY_DEFAULT
                    DSLaser2Index = DSLaser2Index + 1
                    If DSLaser2Index > UBound(dsLaser2Duplicate) Then DSLaser2Index = 0
                    Exit Do
                End If
                intCount = intCount + 1
            Loop
            Do Until intCount > UBound(Laser2LDesc)
                If Laser2LDesc(intCount).Exists = False Then
                    With Laser2LDesc(intCount)
                        .Exists = True
                        .x = Ship.x + 5
                        .y = Ship.y + 14
                        .XVelocity = 0 - (LASERSPEED - 4)
                        .YVelocity = 0 - LASERSPEED
                        .Damage = 1
                    End With
                    dsLaser2Duplicate(DSLaser2Index).SetCurrentPosition 0
                    dsLaser2Duplicate(DSLaser2Index).Play DSBPLAY_DEFAULT
                    dsLaser2Duplicate(DSLaser2Index).SetPan (Ship.x - (SCREENWIDTH \ 2)) * 3
                    DSLaser2Index = DSLaser2Index + 1
                    If DSLaser2Index > UBound(dsLaser2Duplicate) Then DSLaser2Index = 0
                    Exit Do
                End If
                intCount = intCount + 1
            Loop
        End If
    End If
    
    If Ship.PowerUpState > 2 Then                                       'Plasma pulse cannon, this is the only weapon that is not stopped by objects
        intCount = 0
        byteLaser3Counter = byteLaser3Counter + 1
        If byteLaser3Counter = 35 Then
            Do Until intCount > UBound(Laser3Desc)
                If Laser3Desc(intCount).Exists = False Then
                    With Laser3Desc(intCount)
                        .Exists = True
                        .x = Ship.x + ((SHIPWIDTH \ 2) - (Laser3Desc(intCount).Width \ 2))
                        .y = Ship.y
                        .YVelocity = (LASERSPEED + 1.5)
                        .Damage = 2
                    End With
                    dsPulseCannon.SetCurrentPosition 0
                    dsPulseCannon.Play DSBPLAY_DEFAULT
                    Exit Do
                End If
                intCount = intCount + 1
            Loop
            byteLaser3Counter = 0
        End If
    End If
    
End Sub

Private Sub UpdateWeapons()

    'This sub updates all of the players weapon fire
    
    Dim intCount As Integer                                 'count variable
    Dim intCounter As Integer                               'another count variable
    Dim SrcRect As RECT                                     'source rectuangle
    Dim TempX As Long                                       'X position of the rectangle
    Dim TempY As Long                                       'Y position of the rectangle
    Dim XOffset As Long                                     'X offset of the rectangle
    Dim YOffset As Long                                     'Y offset of the rectangle
     
    Do Until intCount > UBound(LaserDesc)                   'Loop through all the level 1 lasers
        If LaserDesc(intCount).Exists Then                  'if the laser exists
            LaserDesc(intCount).y = LaserDesc(intCount).y - LASERSPEED
                                                            'increment the Y position by the speed of the laser
            If LaserDesc(intCount).y < 0 Then               'if the laser goes off the screen
                LaserDesc(intCount).Exists = False          'the laser no longer exists
                LaserDesc(intCount).y = 0                   'reset the Y position
                LaserDesc(intCount).x = 0                   'reset the X position
            Else                                            'otherwise
                ddsBack.BltFast LaserDesc(intCount).x, LaserDesc(intCount).y, ddsLaser, EmptyRect, DDBLTFAST_WAIT Or DDBLTFAST_SRCCOLORKEY
                                                            'blit the laser to the screen
            End If
        End If
        intCount = intCount + 1                             'increment the count
    Loop
      
    With SrcRect                                            'set the coordinates of the level 2 laser
        .Top = 0
        .Bottom = .Top + 8
        .Left = 0
        .Right = .Left + 8
    End With
    
    intCount = 0                                            'reset the count variable
    Do Until intCount > UBound(Laser2RDesc)                 'loop through all the level 2 lasers on the right side
        If Laser2RDesc(intCount).Exists Then                'if the laser exists
            Laser2RDesc(intCount).y = Laser2RDesc(intCount).y + Laser2RDesc(intCount).YVelocity
                                                            'increment the Y by the Y velocity
            Laser2RDesc(intCount).x = Laser2RDesc(intCount).x + Laser2RDesc(intCount).XVelocity
                                                            'increment the X by the X velocity
            With SrcRect                                    'fill in the source rectangle values
                .Left = LASER2WIDTH
                .Right = .Left + LASER2WIDTH
                .Top = 0
                .Bottom = LASER2HEIGHT
            End With
            If Laser2RDesc(intCount).x < 0 Or Laser2RDesc(intCount).x > (SCREENWIDTH - LASER2WIDTH) Or Laser2RDesc(intCount).y < 0 Or Laser2RDesc(intCount).y > (SCREENHEIGHT - LASER2HEIGHT) Then
                                                            'if the laser goes off the screen then
                Laser2RDesc(intCount).Exists = False        'the laser no longer exists
            Else                                            'otherwise
                ddsBack.BltFast Laser2RDesc(intCount).x, Laser2RDesc(intCount).y, ddsLaser2R, SrcRect, DDBLTFAST_WAIT Or DDBLTFAST_SRCCOLORKEY
                                                            'blit the laser to the screen
            End If
        End If
        intCount = intCount + 1                             'increment the count
    Loop
                        
                                                            'The next part does the same thing as the above code.
                                                            'but for the left side of the laser
    intCount = 0
    Do Until intCount > UBound(Laser2LDesc)
        If Laser2LDesc(intCount).Exists Then
            Laser2LDesc(intCount).y = Laser2LDesc(intCount).y + Laser2LDesc(intCount).YVelocity
            Laser2LDesc(intCount).x = Laser2LDesc(intCount).x + Laser2LDesc(intCount).XVelocity
            With SrcRect
                .Left = 0
                .Right = .Left + LASER2WIDTH
                .Top = 0
                .Bottom = LASER2HEIGHT
            End With
            If Laser2LDesc(intCount).x < 0 Or Laser2LDesc(intCount).x > (SCREENWIDTH - LASER2WIDTH) Or Laser2LDesc(intCount).y < 0 Or Laser2LDesc(intCount).y > (SCREENHEIGHT - LASER2HEIGHT) Then
                Laser2LDesc(intCount).Exists = False
            Else
                ddsBack.BltFast Laser2LDesc(intCount).x, Laser2LDesc(intCount).y, ddsLaser2L, SrcRect, DDBLTFAST_WAIT Or DDBLTFAST_SRCCOLORKEY
            End If
        End If
        intCount = intCount + 1
    Loop
    
    intCount = 0
    Do Until intCount > UBound(Laser3Desc)
        If Laser3Desc(intCount).Exists Then
            Laser3Desc(intCount).y = Laser3Desc(intCount).y - Laser3Desc(intCount).YVelocity
            If Laser3Desc(intCount).y < 0 Then
                Laser3Desc(intCount).Exists = False
                Laser3Desc(intCount).y = 0
                Laser3Desc(intCount).x = 0
            Else
                ddsBack.BltFast Laser3Desc(intCount).x, Laser3Desc(intCount).y, ddsLaser3, EmptyRect, DDBLTFAST_WAIT Or DDBLTFAST_SRCCOLORKEY
            End If
        End If
        intCount = intCount + 1
    Loop
    
    intCount = 0                                            'reset the count variable
    Do Until intCount > UBound(GuidedMissile)               'loop through all the guided missle indexes
        If GuidedMissile(intCount).Exists Then              'if the missil exists
            If GuidedMissile(intCount).TargetSet = False Then
                                                            'and the target for it has not been set
                For intCounter = 0 To UBound(EnemyDesc)     'loop through all the enemies
                    If EnemyDesc(intCounter).Exists Then    'if the first enemy encountered exists
                        GuidedMissile(intCount).TargetIndex = intCounter
                                                            'set the index of the target to the index of the enemy
                        GuidedMissile(intCount).TargetSet = True
                                                            'the target has now been set
                        Exit For                            'exit the loop
                    End If
                Next
            Else                                            'otherwise, the target has already been set for this missle
                If EnemyDesc(GuidedMissile(intCount).TargetIndex).Exists Then
                                                            'if the target enemy still exists
                    If (EnemyDesc(GuidedMissile(intCount).TargetIndex).x + (EnemyDesc(GuidedMissile(intCount).TargetIndex).Width / 2)) > GuidedMissile(intCount).x Then
                                                            'check if the missile has gone past the enemy
                        GuidedMissile(intCount).XVelocity = GuidedMissile(intCount).XVelocity + 0.05
                                                            'compensate if it has
                        If GuidedMissile(intCount).XVelocity > MAXMISSILEVELOCITY Then GuidedMissile(intCount).XVelocity = MAXMISSILEVELOCITY
                                                            'make sure that the missiles velocity doesn't go past the maximum
                    ElseIf (EnemyDesc(GuidedMissile(intCount).TargetIndex).x + (EnemyDesc(GuidedMissile(intCount).TargetIndex).Width / 2)) < GuidedMissile(intCount).x Then
                                                            'check if the missile has gone past the enemy
                        GuidedMissile(intCount).XVelocity = GuidedMissile(intCount).XVelocity - 0.05
                                                            'compensate if it has
                        If Abs(GuidedMissile(intCount).XVelocity) > MAXMISSILEVELOCITY Then GuidedMissile(intCount).XVelocity = MAXMISSILEVELOCITY - MAXMISSILEVELOCITY - MAXMISSILEVELOCITY
                                                            'make sure that the missiles velocity doesn't go past the maximum
                    End If
                    If (EnemyDesc(GuidedMissile(intCount).TargetIndex).y + (EnemyDesc(GuidedMissile(intCount).TargetIndex).Height / 2)) > GuidedMissile(intCount).y Then
                                                            'check if the missile has gone past the enemy
                        GuidedMissile(intCount).YVelocity = GuidedMissile(intCount).YVelocity + 0.05
                                                            'compensate if it has
                        If GuidedMissile(intCount).YVelocity > MAXMISSILEVELOCITY Then GuidedMissile(intCount).YVelocity = MAXMISSILEVELOCITY
                                                            'make sure that the missiles velocity doesn't go past the maximum
                    ElseIf (EnemyDesc(GuidedMissile(intCount).TargetIndex).y + (EnemyDesc(GuidedMissile(intCount).TargetIndex).Height / 2)) < GuidedMissile(intCount).y Then
                                                            'check if the missile has gone past the enemy
                        GuidedMissile(intCount).YVelocity = GuidedMissile(intCount).YVelocity - 0.05
                                                            'compensate if it has
                        If Abs(GuidedMissile(intCount).YVelocity) > MAXMISSILEVELOCITY Then GuidedMissile(intCount).YVelocity = MAXMISSILEVELOCITY - MAXMISSILEVELOCITY - MAXMISSILEVELOCITY
                                                            'make sure that the missiles velocity doesn't go past the maximum
                    End If
                Else
                    GuidedMissile(intCount).TargetSet = False
                                                            'if the enemy does not exist, the target has no longer been set
                End If
            End If
            
            GuidedMissile(intCount).x = GuidedMissile(intCount).x + GuidedMissile(intCount).XVelocity
                                                            'increment the missile X by the velocity of the missile
            GuidedMissile(intCount).y = GuidedMissile(intCount).y + GuidedMissile(intCount).YVelocity
                                                            'increment the missile X by the velocity of the missile
            If GuidedMissile(intCount).x < 0 Or (GuidedMissile(intCount).x + MISSILEDIMENSIONS) > SCREENWIDTH Or GuidedMissile(intCount).y < 0 Or (GuidedMissile(intCount).y + MISSILEDIMENSIONS) > SCREENHEIGHT Then
                                                            'if the missile goes off the screen
                GuidedMissile(intCount).Exists = False      'the guided missile no longer exists
                GuidedMissile(intCount).TargetSet = False   'the guided missile has no target
            Else                                            'otherwise
                ddsBack.BltFast GuidedMissile(intCount).x, GuidedMissile(intCount).y, ddsGuidedMissile, EmptyRect, DDBLTFAST_WAIT Or DDBLTFAST_SRCCOLORKEY
                                                            'blit the missile to the screen
            End If
        End If
        intCount = intCount + 1                             'increment the count
    Loop
        
    Exit Sub
      
End Sub
Public Function CreateDDSFromBitmap(dd As DirectDraw7, ByVal strFile As String, Optional VideoMem As Boolean) As DirectDrawSurface7
    
    'This function creates a direct draw surface from any valid file format that loadpicture uses, and returns
    'the newly created surface
    
    If Dir(strFile) = "" Then                                               'If the file doesn't exist
        DisplayError Err.Number, "Unable to create " & strFile & " as a Direct Draw surface." & vbCrLf & "Check to ensure this file exists and is in the same directory as Space Shooter.exe, and is a valid bitmap."
                                                                            'Send the displayerror sub the error message and location the error occurred
    End If
    
    Dim ddsd As DDSURFACEDESC2                                              'Surface description
    Dim dds As DirectDrawSurface7                                           'Created surface
    Dim hdcPicture As Long                                                  'Device context for picture
    Dim hdcSurface As Long                                                  'Device context for surface
    Dim Picture As StdPicture                                               'stdole2 StdPicture object
    
    Set Picture = LoadPicture(strFile)                                      'Load the bitmap
    
    With ddsd                                                               'Fill the surface description
        .lFlags = DDSD_CAPS Or DDSD_HEIGHT Or DDSD_WIDTH                    'Tell Direct Draw that the caps element is valid, the height element is valid, and the width element is valid
        If VideoMem Then                                                    'If the videomem flag is set, then
            .ddsCaps.lCaps = DDSCAPS_OFFSCREENPLAIN Or DDSCAPS_VIDEOMEMORY  'Create the surface in video memory
        Else
            .ddsCaps.lCaps = DDSCAPS_OFFSCREENPLAIN Or DDSCAPS_SYSTEMMEMORY 'Otherwise, creat the surface in system memory
        End If
        .lWidth = Screen.ActiveForm.ScaleX(Picture.Width, vbHimetric, vbPixels)
                                                                            'The width of the surface is set by scaling from the stdpicture objects vbhimetric scale mode to pixels
        .lHeight = Screen.ActiveForm.ScaleY(Picture.Height, vbHimetric, vbPixels)
                                                                            'The height of the surface is set by scaling from the stdpicture objects vbhimetric scale mode to pixels
    End With
    Set dds = dd.CreateSurface(ddsd)                                        'Create the surface
    hdcPicture = CreateCompatibleDC(ByVal 0&)                               'Create a memory device context
    SelectObject hdcPicture, Picture.Handle                                 'Select the bitmap into this memory device
    dds.restore                                                             'Restore the surface
    hdcSurface = dds.GetDC                                                  'Get the surface's DC
    StretchBlt hdcSurface, 0, 0, ddsd.lWidth, ddsd.lHeight, hdcPicture, 0, 0, Screen.ActiveForm.ScaleX(Picture.Width, vbHimetric, vbPixels), Screen.ActiveForm.ScaleY(Picture.Height, vbHimetric, vbPixels), SRCCOPY
                                                                            'Copy from the memory device to the DirectDrawSurface
    dds.ReleaseDC hdcSurface                                                'Release the surface's DC
    DeleteDC hdcPicture                                                     'Release the memory device context
    Set Picture = Nothing                                                   'Release the picture object
    Set CreateDDSFromBitmap = dds                                           'Sets the function to the newly created direct draw surface
    
End Function

Public Sub DrawText(lngXPos As Long, lngYPos As Long, strText As String, lngColor As Long)
    
    'This sub draws text to the back buffer
              
    ddsBack.SetFontTransparency True                    'Set the transparency flag to true
    ddsBack.SetForeColor lngColor                       'Set the color of the text to the color passed to the sub
    ddsBack.SetFont frmSpaceShooter.Font                'Set the font used to the font on the form
    ddsBack.DrawText lngXPos, lngYPos, strText, False   'Draw the text on to the screen, in the coordinates specified

 End Sub
  
Private Sub InitializeStartup()
        
    'This sub initializes all neccessary objects, classes, variables, and user-defined types
    
    Static boolStarted As Boolean                                           'Local flag that is set if the app has started
    Dim intCount As Integer                                                 'counter variable
    
    boolStarted = True                                                      'let the app know the program has Started
    ShowCursor (False)                                                      'don't show the cursor while DX is active
    blnMidiEnabled = True                                                   'turn on the midi by default
    DoEvents                                                                'don't hog the processor while we are loading
    byteNewHighScore = 255                                                  'set the new high score to no new high score
    Call InitializeDS
    Call InitDMusic
    Call InitializeDI                                                       'call the sub that initializes direct input
    Call InitializeDD                                                       'call the sub that initialized direct draw
    
                                                                            'Initialize all the descriptions of all entities in the game
    
    With EnemyContainerDesc(0)                                              'This UDT describes the enemy
        .FileName = "\graphics\enemy1.gif"                                  'Name of the file
        .Height = 50                                                        'The bitmap is 30 pixels high
        .Width = 40                                                         '80 pixels wide
        .NumFrames = 60                                                     '61 frames of animation (Zero based)
        .TimesDies = 1                                                      'It takes one hit to destroy
        .ExplosionIndex = 0                                                 'The enemy uses the first explosion type
        .Score = 150                                                        'Player gets 150 points for destroying it
        .Speed = 3                                                          'It moves 3 pixels every frame
        .ChaseValue = CHASEFAST                                             'This enemy chases the players' X coordinates, and does it fast
        .DoesFire = True                                                    'This enemy fires a weapon
        .FireType = NONTARGETEDFIRE                                         'It doesn't aim towards the player
        .CollisionDamage = 5                                                'It does 5 points of damage if the player collides with it
    End With
    
    'The rest of the entities are setup much the same way. Any differences from previous entities are noted.
    
    With EnemyContainerDesc(1)
        .FileName = "\graphics\enemy2.gif"
        .Height = 64
        .Width = 64
        .NumFrames = 60
        .TimesDies = 3
        .ExplosionIndex = 1                                                 'This enemy displays the second explosion type
        .Score = 250
        .Speed = 1.5
        .ChaseValue = CHASESLOW                                             'This enemy chases the players' X coordinates, but chases slowly
        .DoesFire = True
        .FireType = NONTARGETEDFIRE
        .CollisionDamage = 10
    End With
    
    With EnemyContainerDesc(2)
        .FileName = "\graphics\enemy3.gif"
        .Height = 64
        .Width = 64
        .NumFrames = 30
        .TimesDies = 4
        .ExplosionIndex = 0
        .Score = 350
        .Speed = 2
        .ChaseValue = CHASEOFF                                              'This enemy does not chase the players' X coordinates. This value doesn't need to be setm though, since its' value is 0. It can be set merely as a reminder of the enemies behavior.
        .FireType = TARGETEDFIRE                                            'This enemy does fire towards the player
        .DoesFire = True
        .CollisionDamage = 5
    End With

    With EnemyContainerDesc(3)
        .FileName = "\graphics\enemy4.gif"
        .Height = 90
        .Width = 38
        .NumFrames = 60
        .TimesDies = 1
        .ExplosionIndex = 1
        .Score = 300
        .Speed = 4
        .DoesFire = True
        .FireType = TARGETEDFIRE
        .CollisionDamage = 10
    End With

    With EnemyContainerDesc(4)
        .FileName = "\graphics\blocker.gif"
        .NumFrames = 120
        .Height = 31
        .Width = 31
        .Speed = 1
        .Score = 400
        .TimesDies = 40
        .ExplosionIndex = 0
        .CollisionDamage = 20
    End With

    With EnemyContainerDesc(5)
        .FileName = "\graphics\asteroid1.gif"
        .NumFrames = 28
        .Height = 75
        .Width = 75
        .Speed = 1.75
        .Score = 600
        .TimesDies = 30
        .ExplosionIndex = 1
        .FrameDelay = 1                                                                             'Used to slow the animation down
        .CollisionDamage = 35
    End With

    With EnemyContainerDesc(6)
        .FileName = "\graphics\asteroid2.gif"
        .NumFrames = 28
        .Height = 70
        .Width = 75
        .Speed = 2.5
        .FrameDelay = 1
        .Score = 600
        .TimesDies = 30
        .ExplosionIndex = 0
        .CollisionDamage = 35
    End With

    With EnemyContainerDesc(7)
        .FileName = "\graphics\asteroid3.gif"
        .NumFrames = 28
        .Height = 65
        .Width = 75
        .Speed = 2
        .FrameDelay = 1
        .Score = 600
        .TimesDies = 30
        .ExplosionIndex = 1
        .CollisionDamage = 35
    End With

    With EnemyContainerDesc(8)
        .FileName = "\graphics\asteroid4.gif"
        .NumFrames = 28
        .Height = 65
        .Width = 75
        .Speed = 3
        .Score = 600
        .TimesDies = 30
        .ExplosionIndex = 0
        .FrameDelay = 1
        .CollisionDamage = 35
    End With

    With EnemyContainerDesc(9)
        .FileName = "\graphics\enemy5.gif"
        .NumFrames = 49
        .Height = 70
        .Width = 50
        .Speed = 2.5
        .Score = 600
        .TimesDies = 4
        .ExplosionIndex = 0
        .FrameDelay = 1
        .CollisionDamage = 10
        .DoesFire = True
        .FireType = TARGETEDFIRE
    End With

    With EnemyContainerDesc(10)
        .FileName = "\graphics\enemy6.gif"
        .NumFrames = 99
        .Height = 60
        .Width = 60
        .Speed = 4.5
        .Score = 800
        .TimesDies = 1
        .ExplosionIndex = 1
        .CollisionDamage = 5
        .DoesFire = True
        .FireType = TARGETEDFIRE
    End With

    With EnemyContainerDesc(11)
        .FileName = "\graphics\enemy7.gif"
        .NumFrames = 99
        .Height = 100
        .Width = 40
        .Speed = 3.5
        .Score = 1000
        .TimesDies = 10
        .ExplosionIndex = 0
        .CollisionDamage = 20
    End With

    With EnemyContainerDesc(12)
        .FileName = "\graphics\enemy8.gif"
        .NumFrames = 99
        .Height = 50
        .Width = 50
        .Speed = 4
        .Score = 500
        .TimesDies = 1
        .ExplosionIndex = 1
        .CollisionDamage = 20
    End With

    With EnemyContainerDesc(13)
        .FileName = "\graphics\enemy9.gif"
        .NumFrames = 99
        .Height = 45
        .Width = 65
        .Speed = 2
        .ChaseValue = CHASESLOW
        .Score = 1000
        .TimesDies = 6
        .ExplosionIndex = 0
        .CollisionDamage = 10
        .DoesFire = True
        .FireType = TARGETEDFIRE
    End With
    
    With ObstacleContainerInfo(0)
        .FileName = "\graphics\plate1.gif"
        .Height = 80
        .Width = 80
        .Invulnerable = True
        .Speed = 1
    End With

    With ObstacleContainerInfo(1)
        .FileName = "\graphics\movingplate.gif"
        .Height = 40
        .Width = 40
        .HasDeadIndex = True
        .DeadIndex = 40
        .DoesFire = True
        .FireType = NONTARGETEDFIRE
        .TimesDies = 5
        .ExplosionIndex = 0
        .Score = 600
        .Speed = 1
        .Solid = True
        .NumFrames = 39
    End With

    With ObstacleContainerInfo(2)
        .FileName = "\graphics\plate3.gif"
        .Height = 40
        .Width = 40
        .CollisionDamage = 100
        .Speed = 1
        .Solid = True
        .HasDeadIndex = True
        .DeadIndex = 40
        .TimesDies = 3
        .ExplosionIndex = 1
        .Score = 400
    End With

    With ObstacleContainerInfo(3)
        .FileName = "\graphics\plate4.gif"
        .Height = 40
        .Width = 40
        .Invulnerable = True
        .Speed = 1
        .Solid = True
    End With

    With ObstacleContainerInfo(4)
        .FileName = "\graphics\plate5.gif"
        .Height = 40
        .Width = 40
        .HasDeadIndex = True
        .DeadIndex = 40
        .TimesDies = 3
        .ExplosionIndex = 0
        .Speed = 1
        .Solid = True
        .Score = 400
    End With

    With ObstacleContainerInfo(5)
        .FileName = "\graphics\plate6.gif"
        .Height = 40
        .Width = 40
        .Invulnerable = True
        .Speed = 1
    End With

    With ObstacleContainerInfo(6)
        .FileName = "\graphics\plate7.gif"
        .Height = 40
        .Width = 40
        .Invulnerable = True
        .Speed = 1
    End With

    With ObstacleContainerInfo(7)
        .FileName = "\graphics\plate8.gif"
        .Height = 40
        .Width = 40
        .Invulnerable = True
        .Speed = 1
    End With

    With ObstacleContainerInfo(8)
        .FileName = "\graphics\plate9.gif"
        .Height = 40
        .Width = 40
        .Invulnerable = True
        .Speed = 1
    End With

    With ObstacleContainerInfo(9)
        .FileName = "\graphics\plate10.gif"
        .Height = 40
        .Width = 40
        .Invulnerable = True
        .Speed = 1
    End With

    With ObstacleContainerInfo(10)
        .FileName = "\graphics\plate11.gif"
        .Height = 40
        .Width = 40
        .Invulnerable = True
        .Speed = 1
    End With

    With ObstacleContainerInfo(11)
        .FileName = "\graphics\plate12.gif"
        .Height = 40
        .Width = 40
        .Invulnerable = True
        .Speed = 1
    End With

    With ObstacleContainerInfo(12)
        .FileName = "\graphics\plate13.gif"
        .Height = 40
        .Width = 40
        .Invulnerable = True
        .Speed = 1
    End With
    
    With ObstacleContainerInfo(13)
        .FileName = "\graphics\plate2.gif"
        .Height = 40
        .Width = 40
        .HasDeadIndex = True
        .DeadIndex = 40
        .Speed = 1
        .Solid = True
        .TimesDies = 3
        .ExplosionIndex = 1
        .Score = 450
    End With
    
    With ObstacleContainerInfo(14)
        .FileName = "\graphics\plate14.gif"
        .Height = 40
        .Width = 40
        .HasDeadIndex = True
        .DeadIndex = 40
        .Speed = 1
        .Solid = True
        .TimesDies = 3
        .ExplosionIndex = 0
        .Score = 350
    End With

    With ObstacleContainerInfo(15)
        .FileName = "\graphics\plate15.gif"
        .Height = 40
        .Width = 40
        .HasDeadIndex = True
        .DeadIndex = 40
        .Speed = 1
        .Solid = True
        .TimesDies = 3
        .ExplosionIndex = 1
        .Score = 450
    End With

    With ObstacleContainerInfo(40)
        .FileName = "\graphics\deadplate.gif"
        .Height = 40
        .Width = 40
        .Invulnerable = True
        .Speed = 1
        .NumFrames = 23
        .DeadIndex = 40
        .Solid = True
    End With
    
    'Setup the data for all the background bitmaps
    
    BackgroundObject(0).Width = 600
    BackgroundObject(0).Height = 400
    BackgroundObject(0).PathName = App.Path & "\graphics\nebulae1.gif"
    BackgroundObject(1).Width = 600
    BackgroundObject(1).Height = 400
    BackgroundObject(1).PathName = App.Path & "\graphics\asteroid field.gif"
    BackgroundObject(2).Width = 600
    BackgroundObject(2).Height = 400
    BackgroundObject(2).PathName = App.Path & "\graphics\red giant.gif "
    BackgroundObject(3).Width = 600
    BackgroundObject(3).Height = 400
    BackgroundObject(3).PathName = App.Path & "\graphics\Nebulae2.gif"
    BackgroundObject(4).Width = 600
    BackgroundObject(4).Height = 460
    BackgroundObject(4).PathName = App.Path & "\graphics\cometary.gif"
    BackgroundObject(5).Width = 600
    BackgroundObject(5).Height = 400
    BackgroundObject(5).PathName = App.Path & "\graphics\Nebulae5.gif"
    BackgroundObject(6).Width = 600
    BackgroundObject(6).Height = 400
    BackgroundObject(6).PathName = App.Path & "\graphics\Nebulae3.gif"
    BackgroundObject(7).Width = 600
    BackgroundObject(7).Height = 400
    BackgroundObject(7).PathName = App.Path & "\graphics\Nebulae4.gif"

End Sub
       
Public Sub DoCredits()
    
    'This routine displays the ending credits, fading in and out
    
    Dim lngTime As Long                                                         'standard count
    Dim PalInfo As PALETTEENTRY                                                 'set a variable to hold a palette entry
    Dim ddsEndCredits As DirectDrawSurface7                                     'holds the end credit direct draw surface
    Dim byteCount As Byte                                                       'standard count for bytes
    Dim h As RECT

    Set ddsEndCredits = CreateDDSFromBitmap(dd, App.Path & "\graphics\endcredits.gif")
                                                                                'create the end credits direct draw surface
    ddsBack.BltColorFill h, &H0                                                 'fill the back buffer with black
    ddsBack.BltFast 0, 100, ddsEndCredits, h, DDBLTFAST_WAIT                    'blt the end credits to the back buffer
    FadeScreen True                                                             'Fade the screen in
    lngTime = dx.TickCount                                                      'get the current time
    Do Until dx.TickCount > lngTime + 1500                                      'loop until we have waited 1.5 seconds
        DoEvents                                                                'get the current time
    Loop
    FadeScreen                                                                  'Fade the screen out
    Set ddsEndCredits = Nothing                                                 'release our direct draw surface
    lngTime = dx.TickCount                                                      'get the current time
    Do Until dx.TickCount > lngTime + 500                                       'loop for .5 second
        DoEvents                                                                'get the current time
    Loop
    
End Sub
Public Sub PlayMidi(MidiString As String)
    
    Dim length As Long, length2 As Long
    
    On Local Error GoTo ErrOut
    If Not (seg Is Nothing) And Not (segstate Is Nothing) Then
        If perf.IsPlaying(seg, segstate) Then
            Call perf.Stop(seg, segstate, 0, 0)
        End If
    End If
    If MidiString = vbNullString Then Exit Sub
    Set seg = loader.LoadSegment(MidiString)
    ' Set the search directory based on the app.path
    loader.SetSearchDirectory App.Path
    Offset = 0
    Set segstate = perf.PlaySegment(seg, 0, 0)
    Exit Sub
ErrOut:
    Debug.Print "Error "; Err.Number; " - "; Err.Description
    
End Sub

Public Sub InitDMusic()

    On Local Error GoTo ErrOut
    Set loader = dx.DirectMusicLoaderCreate()                   'Create a new DMusic Loader
    Set perf = dx.DirectMusicPerformanceCreate()                'Create a new DMusic Performance object
    Call perf.Init(Nothing, 0)                                  'Initialize the Performance object
    perf.SetPort -1, 16                                         'Set the default port to 4 sets(64) of voices
    Call perf.SetMasterAutoDownload(True)
    Call perf.SetMasterVolume(1200)
    Exit Sub
ErrOut:
    Debug.Print "Error "; Err.Number; " - "; Err.Description
End Sub
Private Sub ShowTitle()

    'This sub displays the title screen, and rotates one of the palette indexes from blue to black
    
    Dim PalInfo(0) As PALETTEENTRY                                      'array of variables to hold our palette info
    Dim SrcRect As RECT                                                 'source rectangle structure
    Dim lngYCount As Long                                               'Y variable for counting
    Dim lngCount As Long                                                'x variable for counting
    Dim EmptyRect As RECT                                               'empty rectangle structure
    
    boolColorReset = True                                               'make sure that the game knows to reset the palette index that is going to be changed
    ddsBack.BltFast 200, 50, ddsTitle, EmptyRect, DDBLTFAST_SRCCOLORKEY Or DDBLTFAST_WAIT
                                                                        'blit the entire title screen bitmap to the backbuffer using the source color key as a mask
    DDPalette.GetEntries TITLEINDEX, 1, PalInfo()                       'get the index of the blue color in the title screen bitmap, and store it in palinfo
    With PalInfo(0)                                                     'manipulate the blue value of the selected palette index
        .blue = .blue + intColorDirection                               'increment the blue color in the direction specified by intColorDirection
        If .blue > 245 Then intColorDirection = -5                      'if we are greater than 245, make the color direction negative
        If .blue < 5 Then intColorDirection = 5                         'if we are less than 5, make the color direction positive
        .flags = 0
        .green = 0
        .red = 51
    End With
    DDPalette.SetEntries TITLEINDEX, 1, PalInfo()                       'set the palette with the modified palette entry
    DrawText 290, 250, "High Scores", RGB(255, 200, 175)                'Display the high scores message
    lngYCount = 265                                                     'Initialize the starting y coordinate for the high scores
    For lngCount = 0 To 9                                               'loop through the 10 high scores
        
        If lngCount = byteNewHighScore Then
            DrawText 265, lngYCount, lngHighScore(lngCount) & "   " & strHighScoreName(lngCount), RGB(254, 255, 102)
        Else
            DrawText 265, lngYCount, lngHighScore(lngCount) & "   " & strHighScoreName(lngCount), RGB(50, 100, 200)
        End If
                                                                        'display the high score information
        lngYCount = lngYCount + 15                                      'increment the Y coordinate for the next high score
    Next
    
    If blnMidiEnabled Then                                              'if midi is enabled
        DrawText FindStringCenter("Press M to toggle Midi music. Midi: Enabled"), 435, "Press M to toggle Midi music. Midi: Enabled", RGB(58, 84, 26)
                                                                        'display this message
    Else                                                                'otherwise
        DrawText FindStringCenter("Press M to toggle Midi music. Midi: Disabled"), 435, "Press M to toggle Midi music. Midi: Disabled", RGB(58, 84, 26)
                                                                        'display this message
    End If
    
    If Not diJoystick Is Nothing Then                                   'if a joystick object exists
        If blnJoystickEnabled Then                                      'if the joystick is enabled display this message
            DrawText FindStringCenter("Press J to toggle joystick. Joystick: Enabled"), 450, "Press J to toggle joystick. Joystick: Enabled", RGB(56, 78, 56)
        Else                                                            'otherwise
            DrawText FindStringCenter("Press J to toggle joystick. Joystick: Disabled"), 450, "Press J to toggle joystick. Joystick: Disabled", RGB(56, 78, 56)
                                                                        'display this message
        End If
    End If
    
End Sub

Private Sub CheckHighScore()
    
    'This sub checks the current high scores, and updates it with a new high score
    'if the players score is larger than one of the current high scores, then saves
    'it to disk
    
    Static lngCount As Long                                                                     'standard count variable
    Dim intCount As Integer                                                                     'another counting variable
    Dim intCount2 As Integer                                                                    'a second counter variable
    Dim tempHighScore As Long                                                                   'used to temporarily manipulate the score
    Dim tempHighScoreName As String * 15                                                        'used to manipulate the name string
    Dim lngFileFree As Integer                                                                  'hold the next free file handle
    Dim strFileName As String                                                                   'the name of the file
    Dim lngTemp As Long                                                                         'a temporary variable for storage
    Dim strTemp As String                                                                       'temporary string variable for storage
    Dim lngTargetTick As Long                                                                   'used to set the time
    
    diKeyBoard.Unacquire
    If boolGettingInput = False Then                                                            'if the player isn't entering a name then
        boolEnterPressed = False                                                                'the enter key hasn't been pressed
        lngCount = 0                                                                            'reset the count
        Do While lngScore < lngHighScore(lngCount)                                              'loop until we reach the end of the high scores
            lngCount = lngCount + 1                                                             'increment the counter
            If lngCount > 9 Then                                                                'if we reach the end of the high scores
                lngScore = 0                                                                    'reset the players score
                PlayMidi "title.mid"                                                            'play the title midi
                byteNewHighScore = 255                                                          'set the new high score to no new high score
                Exit Sub                                                                        'get out of here
            End If
        Loop
        lngHighScore(9) = lngScore                                                              'if the player does have a high score, assign it to the last place
        boolGettingInput = True                                                                 'we are now getting keyboard input
        strName = ""                                                                            'clear the string
        PlayMidi "inbtween.mid"                                                                 'play the inbetween levels & title screen midi
    End If

    If boolGettingInput And boolEnterPressed = False Then                                       'as long as we are getting input, and the player hasn't pressed enter
        If Len(strName) < 14 And strBuffer <> "" Then                                           'if we haven't reached the limit of characters for the name, and the buffer isn't empty then
            If Asc(strBuffer) > 65 Or strBuffer = Chr$(32) Then strName = strName & strBuffer   'if the buffer contains a letter or a space, add it to the buffer
        End If
        strTemp = Format(lngHighScore(9), "###,###,###") & "  New high score!!!"
        DrawText FindStringCenter(strTemp), 200, strTemp, RGB(200, 200, 50)                     'Display the new high score message
        DrawText 240, 220, "Enter your name: " & strName & "_", RGB(200, 200, 50)               'Give the player a cursor, and display the buffer
    ElseIf boolGettingInput And boolEnterPressed Then                                           'If we are getting input, and the player presses then enter key then
        strHighScoreName(9) = strName                                                           'assign the new high score name the string contained in the buffer
        For intCount = 0 To 9                                                                   'loop through the high scores and re-arrange them
            For intCount2 = 0 To 8                                                              'so that the highest scores are on top, and the lowest
                If lngHighScore(intCount2 + 1) > lngHighScore(intCount2) Then                   'are on the bottom
                    lngTemp = lngHighScore(intCount2)
                    strTemp = strHighScoreName(intCount2)
                    lngHighScore(intCount2) = lngHighScore(intCount2 + 1)
                    strHighScoreName(intCount2) = strHighScoreName(intCount2 + 1)
                    lngHighScore(intCount2 + 1) = lngTemp
                    strHighScoreName(intCount2 + 1) = strTemp
                End If
            Next
        Next
                
        For intCount = 0 To 9                                                                   'loop through all the high scores
            If lngHighScore(intCount) = lngScore Then byteNewHighScore = intCount                             'find the new high score from the list and store it's index
        Next
        lngScore = 0                                                                            'reset the score
        lngFileFree = FreeFile                                                                  'get the next available file handle
        strFileName = App.Path & "\HighScores.bin"                                              'create the path and file name for the high scores
        Open strFileName For Binary Access Write As #lngFileFree                                'open the file for random output
        For intCount = 0 To 9                                                                   'loop through all the high scores
            tempHighScore = lngHighScore(intCount)                                              'and write them to disk
            tempHighScoreName = strHighScoreName(intCount)
            Put #lngFileFree, , tempHighScore
            Put #lngFileFree, , tempHighScoreName
        Next
        boolGettingInput = False                                                                'we are no longer getting input
        PlayMidi "title.mid"                                                                    'Start the title midi again
    End If
    
    strBuffer = ""                                                                              'clear the buffer
    boolEnterPressed = False                                                                    'clear the enter toggle
    
End Sub

Public Sub FadeScreen(Optional FadeIn As Boolean)

    'This sub fades the screen in or out (out by default)
    
    Dim tempPalette(255) As PALETTEENTRY                                'structure that holds 255 temporary palette entries
    Dim OldPalette(255) As PALETTEENTRY                                 'structure to hold the old palette entries
    Dim byteCount As Byte                                               'variable to loop through bytes
    Dim byteCount2 As Byte                                              'another byte loop variable
    Dim intCount As Integer                                             'standard count variable
    
    DDPalette.GetEntries 0, 255, tempPalette()                          'get the current palette entries
    DDPalette.GetEntries 0, 255, OldPalette()                           'get the current palette entries
    
    If Not FadeIn Then                                                  'if we aren't fading in then darken then fade out
        byteCount2 = 255                                                'start at the upper end of the pallette
        Do While byteCount2 - 5 > 0                                     'while we aren't reaching the end of the palette limit
            byteCount = 255                                             'the upper end of the count is 255
            Do While byteCount > 0                                      'loop through each palette entry
                With tempPalette(byteCount)                             'set the palette entry to the new value
                    If .blue - 8 > 0 Then .blue = .blue - 8
                    .flags = 0
                    If .green - 8 > 0 Then .green = .green - 8
                    If .red - 8 > 0 Then .red = .red - 8
                End With
                byteCount = byteCount - 1                               'decrement the palette count
            Loop
            dd.WaitForVerticalBlank DDWAITVB_BLOCKBEGIN, 0              'wait until the vertical blank begins
            DDPalette.SetEntries 0, 255, tempPalette()                  'set the palette entry with the new color
            byteCount2 = byteCount2 - 5                                 'decrement the color
        Loop
         
        ddsBack.BltColorFill EmptyRect, 0                               'fill the back buffer with black
        ddsFront.Flip Nothing, 0                                        'flip the surfaces
        DDPalette.SetEntries 0, 255, OldPalette()                       'set the palette back to the old palette
    Else                                                                'otherwise we are fading in
        dd.WaitForVerticalBlank DDWAITVB_BLOCKBEGIN, 0                  'Wait for the vertical blank
        
        For intCount = 0 To 255                                         'Loop through the temporary palette, setting all values to 0
            With tempPalette(intCount)
                .blue = 0
                .red = 0
                .green = 0
                .flags = 0
            End With
        Next
        
        DDPalette.SetEntries 0, 255, tempPalette()                      'set the palette entry
        ddsFront.Flip Nothing, 0                                        'flip the surface
        byteCount2 = 0                                                  'start at 0 for the color
        Do While byteCount2 < 255                                       'loop until we reach the end of the palette
            byteCount = 0                                               'set the first count to 0
            Do Until byteCount = 255                                    'loop through each palette entry
                With tempPalette(byteCount)                             'set each entry to the new value
                    If .blue + 8 < OldPalette(byteCount).blue Then
                        .blue = .blue + 8
                    ElseIf .blue > OldPalette(byteCount).blue Then
                        .blue = OldPalette(byteCount).blue
                    End If
                    .flags = 0
                    If .green + 8 < OldPalette(byteCount).green Then
                        .green = .green + 8
                    ElseIf .green > OldPalette(byteCount).green Then
                        .green = OldPalette(byteCount).green
                    End If
                    If .red + 8 < OldPalette(byteCount).red Then
                        .red = .red + 8
                    ElseIf .red > OldPalette(byteCount).red Then
                        .red = OldPalette(byteCount).red
                    End If
                End With
                byteCount = byteCount + 1                               'increment the count
            Loop
            dd.WaitForVerticalBlank DDWAITVB_BLOCKBEGIN, 0              'Wait for the vertical blank
            DDPalette.SetEntries 0, 255, tempPalette()                  'set the palette entry
            byteCount2 = byteCount2 + 5                                 'increment the color value
        Loop
        dd.WaitForVerticalBlank DDWAITVB_BLOCKBEGIN, 0                  'wait for a vertical blank
        DDPalette.SetEntries 0, 255, OldPalette()                       'set the palette entry
    End If
        
End Sub

Private Sub GetHighScores()
        
    'This subroutine reads in the binary HighScores.bin file and
    'reads in the player names and scores
    
    Dim intCount As Integer                                             'Just your standard integer variable used for counting in loops
    Dim tempHighScore As Long                                           'Variable used to temporarily hold the variable read in from the file
    Dim tempHighScoreName As String * 15                                'String variable used to temporarily hold the variable read in from the file
    Dim lngFileFree As Integer                                          'Variable used to hold the next available free file

    lngFileFree = FreeFile                                              'Assign the next free file to the variable
    Open App.Path & "\HighScores.bin" For Binary Access Read As #lngFileFree
                                                                        'Open the high score file as a binary file and get ready to read info from it
    For intCount = 0 To 9                                               'Loop through all the records in the file
        Get #lngFileFree, , tempHighScore                               'get the record, and assign it to the temphighscore long variable
        Get #lngFileFree, , tempHighScoreName                           'get the record, and assign it to the temphighscorename string variable
        lngHighScore(intCount) = tempHighScore                          'store the temp value in the high score variable
        strHighScoreName(intCount) = Trim(tempHighScoreName)            'trim the highscorename variable of all spaces and assign it to the name array
    Next                                                                'loop

End Sub

Public Sub EndGame()
    
    'This sub releases all Direct X objects
    
    Dim intCount As Integer                                 'standard count variable
    
    On Error Resume Next
    
    ShowCursor (True)                                       'turn the cursor back on
    
                                                            'Release direct input objects
    diKeyBoard.Unacquire                                    'unaquire the keyboard
    If Not diJoystick Is Nothing Then                       'if the joystick device exists,
        diJoystick.Unacquire                                'unacquire it
        Set diJoystick = Nothing                            'set the joystick instance to nothing
    End If
    
    Set diKeyBoard = Nothing                                'set the keyboard instance to nothing
    Set DI = Nothing                                        'set the direct input device to nothing
                                                            'Release direct sound objects
    Set dsExplosion = Nothing                               'set the explosion ds buffer to nothing
    For intCount = 0 To UBound(dsExplosionDuplicate)        'loop through all the duplicate explosion buffers
        Set dsExplosionDuplicate(intCount) = Nothing        'set them to nothing
    Next
    Set dsEnergize = Nothing                                'set the ds enemrgize buffer to nothing
    Set dsAlarm = Nothing                                   'set the alarm ds buffer to nothing
    Set dsEnemyFire = Nothing                               'set the enemy fire ds buffer to nothing
    For intCount = 0 To UBound(dsEnemyFireDuplicate)        'loop through all the enemy duplicate ds buffers
        Set dsEnemyFireDuplicate(intCount) = Nothing        'set them to nothing
    Next
    Set dsNoHit = Nothing                                   'set the no hit ds buffer to nothing
    For intCount = 0 To UBound(dsNoHitDuplicate)            'loop through all the no hit ds duplicate buffers
        Set dsNoHitDuplicate(intCount) = Nothing            'set them to nothing
    Next
    Set dsLaser2 = Nothing                                  'set the level2 ds buffer to nothing
    For intCount = 0 To UBound(dsLaser2Duplicate)           'loop through all the level2 ds duplicate buffers
        Set dsLaser2Duplicate(intCount) = Nothing           'set them to nothing
    Next
    Set dsLaser = Nothing                                   'set the ds laser buffer to nothing
    Set dsPulseCannon = Nothing
    Set dsPlayerDies = Nothing
    Set dsPowerUp = Nothing                                 'set the power up ds buffer to nothing
    Set dsMissile = Nothing                                 'set the ds missile buffer to nothing
    Set dsInvulnerability = Nothing                         'set the ds invulnerable to nothing
    Set dsInvPowerDown = Nothing                            'set the power down sound to nothing
    Set dsExtraLife = Nothing                               'set the extra life sound to nothing
    Set dsPrimaryBuffer = Nothing                           'set the primary buffer to nothing
    Set DS = Nothing                                        'set the ds direct sound object to nothing
        
                                                            'Direct Draw
        
    Set ddsHit = Nothing                                    'set the hit direct draw surface to nothing
    Set ddsLaser = Nothing                                  'set the laser dds to nothing
    Set ddsLaser2R = Nothing                                'laser2 right side dds to nothing
    Set ddsLaser2L = Nothing                                'laser2 left side dds to nothing
    Set ddsLaser3 = Nothing                                 'laser3 surface to nothing
    Set ddsStar = Nothing                                   'set the stars surface to nothing
    Set ddsEnemyFire = Nothing                              'enemy fire to nothing
    Set ddsGuidedMissile = Nothing                          'guided missiles to nothing
    Set ddsTitle = Nothing                                  'title to nothing
    Set ddsPowerUp = Nothing                                'power up to nothing
    Set ddsShieldIndicator = Nothing                        'shield indicator to nothing (getting tired of this yet?)
    Set ddsShip = Nothing                                   'ship to nothing
    Set ddsExplosion(0) = Nothing                           'explosion to nothing
    Set ddsExplosion(1) = Nothing                           'explosion to nothing
    Set ddsDisplayBomb = Nothing                            'set the bomb surface to nothing
    Set ddsInvulnerable = Nothing                           'invulnerable surface to nothing
    Set DDPalette = Nothing                                 'sets the direct draw palette object to nothing
    
    'The following lines loop through the arrays
    'and set their surfaces to nothing
    
    For intCount = 0 To UBound(ddsBackgroundObject)
        Set ddsBackgroundObject(intCount) = Nothing
    Next
    For intCount = 0 To UBound(ddsEnemyContainer)
        Set ddsEnemyContainer(intCount) = Nothing
    Next
    For intCount = 0 To UBound(ddsObstacle)
        Set ddsObstacle(intCount) = Nothing
    Next
        
    Set ddsBack = Nothing                                   'set the back buffer to nothing
    Set ddsFront = Nothing                                  'set the front buffer to nothing
    dd.RestoreDisplayMode                                   'restore the display
    dd.SetCooperativeLevel 0, DDSCL_NORMAL                  'exit exclusive mode
    
    Set dd = Nothing                                        'set the direct draw object to nothing
    '-----------------------------------
    If Not (seg Is Nothing) And Not (segstate Is Nothing) Then
        If perf.IsPlaying(seg, segstate) Then                   'Is there a Segment playing?
            Call perf.Stop(seg, segstate, 0, 0)                 'Stop playing any midi's currently playing
        End If
    End If
        
End Sub
