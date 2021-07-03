//-----------------------------------------------------------------------------
// 
// Sample Name: ModeTest Sample
// 
// Copyright (c) 1999 Microsoft Corporation. All rights reserved.
// 
//-----------------------------------------------------------------------------


Description
===========
    This example demonstrates basic usage of the IDirectDraw7::StartModeTest
    and IDirectDraw7::EvaluateMode methods. Together, these methods allow an
    application to explore what display modes and refresh rates the monitor
    connected to this display device is able to display, though a manual
    user-controlled process. The application will present the UI that asks
    the user if the current mode under test is being displayed correctly by
    the monitor.

    Applications should use these methods when they are interested in using
    higher refresh rates.

    The basic idea is that DirectDraw will setup a list of modes to be tested
    (based on the list the app passed in), and then sequentially test them
    under application control. The app starts the test process, and then
    calls IDirectDraw7::EvaluateMode continuously. DirectDraw will take care
    of settings the modes. All the app has to do is SetCooperativeLevel
    beforehand, and then handle surface loss and drawing the UI that asks the
    user if they can see the current mode under test. DirectDraw returns
    enough information from IDirectDraw7::EvalulateMode to allow the app to
    know when to do these things, and when to stop testing. The app can pass
    a flag to IDirectDraw7::EvaluateMode if the user happened to say they
    could see the mode corretly, which will cause DirectDraw to mark the mode
    as good and move on. DirectDraw may also decide that time as run out and
    give up on a certain mode.

    DirectDraw uses information at its disposal from any automated means to
    make the testing process as short as possible, and applications only need
    to test modes they are interested in.

Path
====
  Source: Mssdk\Samples\Multimedia\DDraw\Src\ModeTest

  Executable: Mssdk\Samples\Multimedia\DDraw\Bin

User's Guide
============
  The user interface is a simple dialog box. First select the DirectDraw display
  device.  Then select which modes you would like to test, and click 'Test'.   
  Each display mode will be tested, and the user can select if the mode appears
  correctly or not. 
  
  If the user makes a mistake, and accidently says YES when a mode cannot be seen, 
  or NO when a mode really can be seen (thus ending up with a lower refresh rate than 
  possible) this allows the user to reset the test results and try again.  
  
  Click the Close button to exit the application.

  

