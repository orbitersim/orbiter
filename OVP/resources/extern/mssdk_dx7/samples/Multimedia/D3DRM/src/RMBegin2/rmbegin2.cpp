//-----------------------------------------------------------------------------
// File: RMBegin1.cpp
//
// Desc: This minimal file creates a default D3DRM device, loads meshes from
//       DirectX files, scales and colors them, sets up a hierarchy of frames,
//       adds lights, and rotates the objects. The result is a sun, planet
//       and moon scenario, with the moon revolving around the planet and
//       the planet revolving around the sun. This sample also contains
//       standard code which allows the sample to run in the Windows
//       environment. 
//
// Copyright (C) 1998-1999 Microsoft Corporation. All Rights Reserved.
//-----------------------------------------------------------------------------

#define INITGUID       // Required for Direct3D applications 
#include <windows.h>   // Standard Windows header file 
#include <direct.h>    // DirectDraw definitions 
#include <d3drmwin.h>  // D3DRM definitions 


// Macro to release an object. 
#define RELEASE(x) if (x != NULL) {x->Release(); x = NULL;} 
// Macro to display a message box containing the given string. 
#define DISPLAYMSG(x) MessageBox(NULL, x, "D3DRM Sample", MB_OK);


// Global Variables 
LPDIRECT3DRM3 lpD3DRM = NULL;            // Direct3DRM object 
LPDIRECTDRAWCLIPPER lpDDClipper = NULL; // DirectDrawClipper object 


// Global Structure
struct _myglobs
{
    // Direct3DRM device. 
    LPDIRECT3DRMDEVICE3 dev;           

    // Direct3DRM window device. 
    LPDIRECT3DRMWINDEVICE windevice;           

    // Direct3DRM viewport through which to view the scene. 
    LPDIRECT3DRMVIEWPORT2 view;  

    // Master frame in which other frames are placed. 
    LPDIRECT3DRMFRAME3 scene;    

    // Frame describing the users point of view. 
    LPDIRECT3DRMFRAME3 camera;   

    // Application is minimized. 
    BOOL bMinimized;            

    // All D3DRM objects have been initialized. 
    BOOL bInitialized;          

} myglobs;


// Prototypes 
BOOL             BuildScene( LPDIRECT3DRM3, LPDIRECT3DRMDEVICE3, 
                             LPDIRECT3DRMFRAME3, LPDIRECT3DRMFRAME3 );
static BOOL      RenderScene();
BOOL             CreateObjects( HWND win );
static HWND      InitApp( HINSTANCE, int );
int APIENTRY     WinMain( HINSTANCE, HINSTANCE, LPSTR, int );
LRESULT CALLBACK WindowProc( HWND, UINT, WPARAM, LPARAM );




//-----------------------------------------------------------------------------
// Name: BuildScene()
// Desc: Create the scene to be rendered. 
//-----------------------------------------------------------------------------
BOOL BuildScene( LPDIRECT3DRM3 lpD3DRM,  LPDIRECT3DRMDEVICE3 dev, 
                 LPDIRECT3DRMFRAME3 scene, LPDIRECT3DRMFRAME3 camera )
{
    LPDIRECT3DRMMESHBUILDER3 sun_builder = NULL;
    LPDIRECT3DRMMESHBUILDER3 planet_builder = NULL;
    LPDIRECT3DRMMESHBUILDER3 moon_builder = NULL;
    LPDIRECT3DRMMESH sun_mesh = NULL;
    LPDIRECT3DRMMESH planet_mesh = NULL;
    LPDIRECT3DRMMESH moon_mesh = NULL;
    LPDIRECT3DRMFRAME3 sun = NULL;
    LPDIRECT3DRMFRAME3 planet = NULL;
    LPDIRECT3DRMFRAME3 moon = NULL;
    LPDIRECT3DRMLIGHT light1 = NULL;
    LPDIRECT3DRMLIGHT light2 = NULL;
    LPDIRECT3DRMLIGHT light3 = NULL;
    LPDIRECT3DRMFRAME3 lights = NULL;
    HRESULT rval;
    

    // Create a meshbuilder for each DirectX file to be loaded (one 
    // for sun, planet, and moon) and load the meshes.
    if (FAILED(lpD3DRM->CreateMeshBuilder(&sun_builder)))
        goto generic_error;
    if (FAILED(lpD3DRM->CreateMeshBuilder(&planet_builder)))
        goto generic_error;
    if (FAILED(lpD3DRM->CreateMeshBuilder(&moon_builder)))
        goto generic_error;

    rval = sun_builder->Load("sphere2.x", NULL,
                    D3DRMLOAD_FROMFILE, NULL, NULL);
    if (FAILED(rval)) {
        DISPLAYMSG("Failed to load sphere2.x file.");
        goto ret_with_error;
    }

    rval = planet_builder->Load("sphere1.x", NULL,
                    D3DRMLOAD_FROMFILE, NULL, NULL);
    if (FAILED(rval)) {
        DISPLAYMSG("Failed to load sphere1.x file.");
    goto ret_with_error;
    }

    rval = moon_builder->Load("sphere0.x", NULL,
                    D3DRMLOAD_FROMFILE, NULL, NULL);
    if (FAILED(rval)) {
        DISPLAYMSG("Failed to load sphere0.x file.");
        goto ret_with_error;
    }

    // Adjust the scale (size) of the sun, planet, and moon meshes.
    // Scale equally in the x, y, and z directions to maintain the
    // spherical shape of each mesh. Increase the size of the sun
    // and decrease the size of the planet and moon meshes to
    // achieve the desired relative sizes.
    if (FAILED(sun_builder->Scale(D3DVAL(1.2), D3DVAL(1.2), 
                                  D3DVAL(1.2))))
        goto generic_error;
    if (FAILED(planet_builder->Scale(D3DVAL(0.5), D3DVAL(0.5),
                                     D3DVAL(0.5))))
        goto generic_error;
    if (FAILED(moon_builder->Scale(D3DVAL(0.2), D3DVAL(0.2),
                                   D3DVAL(0.2))))
        goto generic_error;

    // Adjust the color of the sun, planet, and moon meshes.
    // Set the sun to yellow, the planet to greenish-blue, and the
    // moon to deep red.
    if (FAILED(sun_builder->SetColorRGB(D3DVAL(1.0), D3DVAL(1.0),
                                        D3DVAL(0.0))))
        goto generic_error;
    if (FAILED(planet_builder->SetColorRGB(D3DVAL(0.2), D3DVAL(1.0), 
                                           D3DVAL(0.8))))
        goto generic_error;
    if (FAILED(moon_builder->SetColorRGB(D3DVAL(0.7), D3DVAL(0.2), 
                                         D3DVAL(0.3))))
        goto generic_error;

    // Create hierarchies of frames:
    // - Sun frame within the master scene frame
    // - Planet frame within sun frame
    // - Moon frame within planet frame
    if (FAILED(lpD3DRM->CreateFrame(scene, &sun)))
        goto generic_error;
    if (FAILED(lpD3DRM->CreateFrame(sun, &planet)))
        goto generic_error;
    if (FAILED(lpD3DRM->CreateFrame(planet, &moon)))
        goto generic_error;

    // Create meshes from the meshbuilders to avoid the same future 
    // conversion by Direct3D Retained mode. The meshbuilders could be
    // released now, but for clarity they are released with the other
    // objects at the end of this function.
    if (FAILED(sun_builder->CreateMesh(&sun_mesh))) 
        goto generic_error;
    if (FAILED(planet_builder->CreateMesh(&planet_mesh))) 
        goto generic_error;
    if (FAILED(moon_builder->CreateMesh(&moon_mesh))) 
        goto generic_error;

    // Add the meshes into the frames.
    if (FAILED(sun->AddVisual((LPDIRECT3DRMVISUAL) sun_mesh)))
        goto generic_error;
    if (FAILED(planet->AddVisual((LPDIRECT3DRMVISUAL) planet_mesh)))
        goto generic_error;
    if (FAILED(moon->AddVisual((LPDIRECT3DRMVISUAL) moon_mesh)))
        goto generic_error;

    // Set up the frame positions.
    if (FAILED(camera->SetPosition(scene, D3DVAL(0), D3DVAL(0), 
                                   -D3DVAL(25))))
        goto generic_error;
    if (FAILED(sun->SetPosition(scene, D3DVAL(0), D3DVAL(0), 
                                D3DVAL(0))))
        goto generic_error;
    if (FAILED(planet->SetPosition(sun, D3DVAL(7), D3DVAL(0), 
                                   D3DVAL(0))))
        goto generic_error;
    if (FAILED(moon->SetPosition(planet, D3DVAL(0), D3DVAL(3), 
                                 D3DVAL(0))))
        goto generic_error;

    // Set up the frame rotations.
    if (FAILED(sun->SetRotation(scene, D3DVAL(0), D3DVAL(0), 
                                D3DVAL(1), 
                                D3DVAL(0.01))))
        goto generic_error;
    if (FAILED(planet->SetRotation(sun, D3DVAL(1), D3DVAL(0), 
                                   D3DVAL(0), 
                                   D3DVAL(0.02))))
        goto generic_error;
    if (FAILED(moon->SetRotation(planet, D3DVAL(0.1), D3DVAL(0.2),
                               D3DVAL(0.7), 
                               D3DVAL(0.03))))
       goto generic_error;

    // Initialize the lights in the scene. 
    // - Create a light frame. Set its position within the scene 
    //   (straight ahead, directly in front of the sun, to illuminate
    //   the sun), and attach a point light to the light frame.
    // - Create a soft ambient light and add it to the scene.
    // - Create a bright parallel point light and add it to the sun
    //   frame to properly illuminate the planet and moon as they
    //   revolve around the sun.
     
    // Create a point light in front of sun. 
    if (FAILED(lpD3DRM->CreateFrame(scene, &lights)))
        goto generic_error;
    if (FAILED(lights->SetPosition(scene, D3DVAL(0), D3DVAL(0),
                                -D3DVAL(7)))) 
        goto generic_error;
    if (FAILED(lpD3DRM->CreateLightRGB(D3DRMLIGHT_POINT, D3DVAL(0.9),
                                  D3DVAL(0.8), D3DVAL(0.7), &light1)))
        goto generic_error;
    if (FAILED(lights->AddLight(light1)))
        goto generic_error;

    // Create an ambient light and add it to the scene. 
    if (FAILED(lpD3DRM->CreateLightRGB(D3DRMLIGHT_AMBIENT, D3DVAL(0.1),
                                  D3DVAL(0.1), D3DVAL(0.1), &light2)))
        goto generic_error;
    if (FAILED(scene->AddLight(light2)))
        goto generic_error;

    // Create a parallel point light and add it to the sun frame. 
    if (FAILED(lpD3DRM->CreateLightRGB(D3DRMLIGHT_PARALLELPOINT, 
                                  D3DVAL(0.9), D3DVAL(0.9), D3DVAL(0.9),
                                  &light3)))
        goto generic_error;
    if (FAILED(sun->AddLight(light3)))
        goto generic_error;

    // Clean up.
    RELEASE(sun_builder);
    RELEASE(planet_builder);
    RELEASE(moon_builder);
    RELEASE(sun_mesh);
    RELEASE(planet_mesh);
    RELEASE(moon_mesh);
    RELEASE(sun);
    RELEASE(planet);
    RELEASE(moon);
    RELEASE(lights);
    RELEASE(light1);
    RELEASE(light2);
    RELEASE(light3);
    return TRUE;

generic_error:
    DISPLAYMSG("A failure occurred while building the scene.");
ret_with_error:
    RELEASE(sun_builder);
    RELEASE(planet_builder);
    RELEASE(moon_builder);
    RELEASE(sun_mesh);
    RELEASE(planet_mesh);
    RELEASE(moon_mesh);
    RELEASE(sun);
    RELEASE(planet);
    RELEASE(moon);
    RELEASE(lights);
    RELEASE(light1);
    RELEASE(light2);
    RELEASE(light3);
    return FALSE;
}




//-----------------------------------------------------------------------------
// Name: RenderScene()
// Desc: Clear the viewport, render the next frame, and update the window.
//-----------------------------------------------------------------------------
static BOOL RenderScene()
{
    HRESULT rval;

    // Move the scene.
    rval = myglobs.scene->Move(D3DVAL(1.0));
    if (FAILED(rval)) {
        DISPLAYMSG("Moving scene failed.");
        return FALSE;
    }

    // Clear the viewport.
    rval = myglobs.view->Clear(D3DRMCLEAR_ALL);
    if (FAILED(rval)) {
        DISPLAYMSG("Clearing viewport failed.");
        return FALSE;
    }

    // Render the scene to the viewport.
    rval = myglobs.view->Render(myglobs.scene);
    if (FAILED(rval)) {
        DISPLAYMSG("Rendering scene failed.");
        return FALSE;
    }

    // Update the window.
    rval = myglobs.dev->Update();
    if (FAILED(rval)) {
        DISPLAYMSG("Updating device failed.");
        return FALSE;
    }
    return TRUE;
}




//-----------------------------------------------------------------------------
// Name: AddMediaPath()
// Desc: Looks in the system registry to determine the media path for the
//       sample. Then, it adds that path to the string passed in, checks if the
//       file exists, and returns a path to the file.
//-----------------------------------------------------------------------------
VOID AddMediaPath( LPDIRECT3DRM3 pD3DRM )
{
	HKEY   key;
	LONG   result;
	TCHAR  strPath[512];
	DWORD  type, size = 512;

	// Open the registry
	result = RegOpenKeyEx( HKEY_LOCAL_MACHINE, "Software\\Microsoft\\DirectX",
						   0, KEY_READ, &key );
	if( ERROR_SUCCESS != result )
		return;

	// Search for the desired registry value, and close the registry
        result = RegQueryValueEx( key, "DXSDK Samples Path", NULL, &type, 
		                      (BYTE*)strPath, &size );
	RegCloseKey( key );

	if( ERROR_SUCCESS != result )
		return;

	strcat( strPath, "\\D3DRM\\Media" );

	pD3DRM->AddSearchPath( strPath );

	return;
}




//-----------------------------------------------------------------------------
// Name: CreateObjects()
// Desc: Initialize globals, create the device and objects. 
//-----------------------------------------------------------------------------
BOOL CreateObjects( HWND win )
{
    HRESULT rval; // Return value 
    RECT rc;      // Bounding rectangle for main window 
    int width;    // Device's width 
    int height;   // Device's height 

    // Initialize the entire global variable structure to zero. 
    memset(&myglobs, 0, sizeof(myglobs));

    // Create a DirectDrawClipper object and associate the window with it.
    rval = DirectDrawCreateClipper(0, &lpDDClipper, NULL);
    if (FAILED(rval)) {
        DISPLAYMSG("Failed to create DirectDrawClipper object.");
        return FALSE;
    }
    rval = lpDDClipper->SetHWnd(0, win);
    if (FAILED(rval)) {
        DISPLAYMSG("Failed to set the window handle for the DirectDrawClipper.");
        return FALSE;
    }

    // Create the Direct3DRM object.
    LPDIRECT3DRM pD3DRMTemp;
    rval = Direct3DRMCreate(&pD3DRMTemp);
    if (FAILED(rval)) 
    {
        DISPLAYMSG("Failed to create Direct3DRM.");
        return FALSE;
    }
    if( FAILED( pD3DRMTemp->QueryInterface(IID_IDirect3DRM3, (void **)&lpD3DRM) ) )
    {
        RELEASE(pD3DRMTemp);
        DISPLAYMSG("Failed query for Direct3DRM3.\n" );
        return FALSE;
    }
    RELEASE(pD3DRMTemp);

    
    // Create a default Direct3DRM device.
    GetClientRect(win, &rc);

    rval = lpD3DRM->CreateDeviceFromClipper(lpDDClipper, 
                           NULL, // Default device 
                           rc.right, rc.bottom, &myglobs.dev);

    if (FAILED(rval)) {
        DISPLAYMSG("Failed to create the D3DRM device.");
        return FALSE;
    }


    // Create a Direct3D Retained Mode window device to handle the
    // WM_ACTIVATE and WM_PAINT messages.
    rval = myglobs.dev->QueryInterface(IID_IDirect3DRMWinDevice,
                                       (void **) &myglobs.windevice);
    if (FAILED(rval)) {
        DISPLAYMSG("Failed to create the window device.");
        return FALSE;
    }
    
    
    // Create the master scene frame and the camera frame.
    rval = lpD3DRM->CreateFrame(NULL, &myglobs.scene);
    if (FAILED(rval)) {
        DISPLAYMSG("Failed to create the master scene frame.");
        return FALSE;
    }

    rval = lpD3DRM->CreateFrame(myglobs.scene, &myglobs.camera);
    if (FAILED(rval)) {
        DISPLAYMSG("Failed to create the camera's frame.");
        return FALSE;
    }

    // Create the Direct3DRM viewport using the device, camera frame,
    // and the device's width and height.
    width = myglobs.dev->GetWidth();
    height = myglobs.dev->GetHeight();

    rval = lpD3DRM->CreateViewport(myglobs.dev, myglobs.camera, 0, 0, 
                                   width, height, &myglobs.view);
    if (FAILED(rval)) {
        myglobs.bInitialized = FALSE;
        RELEASE(myglobs.dev);
        return FALSE;
    }

	// Add the media path so our textures and .x files can be found
	AddMediaPath( lpD3DRM );

    // Create the scene to be rendered.
    if (!BuildScene(lpD3DRM, myglobs.dev, myglobs.scene, 
                    myglobs.camera))
        return FALSE;

    // Globals are initialized.
    myglobs.bInitialized = TRUE;

    return TRUE;
}




//-----------------------------------------------------------------------------
// Name: InitApp()
// Desc: Create the main window and initialize objects. 
//-----------------------------------------------------------------------------
static HWND InitApp( HINSTANCE this_inst, int cmdshow )
{
    HWND win;     // Main window handle 
    WNDCLASS wc;

    // Set up and register the window class.
    wc.style = CS_HREDRAW | CS_VREDRAW;
    wc.lpfnWndProc = WindowProc;
    wc.cbClsExtra = 0;
    wc.cbWndExtra = sizeof(DWORD);
    wc.hInstance = this_inst;
    wc.hIcon = LoadIcon(this_inst, "AppIcon"); 
    wc.hCursor = LoadCursor(NULL, IDC_ARROW);
    wc.hbrBackground = (HBRUSH)GetStockObject(BLACK_BRUSH);
    wc.lpszMenuName = NULL;
    wc.lpszClassName = "D3DRM Example";
    if (!RegisterClass(&wc))
        return FALSE;

    // Create the window.
    win = CreateWindow(   
            "D3DRM Example",                   // class 
            "RMBegin2: Direct3DRM Sample Two", // caption 
            WS_OVERLAPPEDWINDOW,               // style 
            CW_USEDEFAULT,                     // init. x pos 
            CW_USEDEFAULT,                     // init. y pos 
            350,                               // init. x size 
            300,                               // init. y size 
            NULL,                              // parent window 
            NULL,                              // menu handle 
            this_inst,                         // program handle 
            NULL                               // create parms 
        );
    if (!win)
        return FALSE;

    // Initialize global variables, enumerate devices, and create the 
    // D3DRM objects.
    if (!CreateObjects(win))
        return FALSE;

    // Display the window.
    ShowWindow(win, cmdshow);
    UpdateWindow(win);

    return win;
}




//-----------------------------------------------------------------------------
// Name: WinMain()
// Desc: Initialize the application, process messages, and render the scene.
//-----------------------------------------------------------------------------
int APIENTRY WinMain( HINSTANCE this_inst, HINSTANCE, LPSTR, int cmdshow )
{
    HWND    hwnd;
    MSG     msg;

    // Create the window and initialize objects. 
    if (!(hwnd = InitApp(this_inst, cmdshow)))
        return 1;

    while (TRUE) {  
        // Monitor the message queue and process messages. PeekMessage 
        // returns control to the application immediately so the 
        // application can both process messages and continue rendering. 
        // If the message is WM_QUIT, break out of this function
        // because the application is terminating.
        if (PeekMessage(&msg, NULL, 0, 0, PM_REMOVE)) {

            if (msg.message == WM_QUIT) 
                break;
            TranslateMessage(&msg);
            DispatchMessage(&msg);
        }
    
        // If the application is not minimized and the Direct3D Retained 
        // Mode objects are initialized, render.
        if (!myglobs.bMinimized && myglobs.bInitialized) {

            // Render one frame of the scene. If rendering fails, post
            // the WM_DESTROY message, enabling the application to 
            // free resources before terminating the application.
            if (!RenderScene()) {
                DISPLAYMSG("Rendering failed. Aborting execution.");
                PostMessage(NULL, WM_DESTROY, 0, 0);
                break;
            }

        // Yield to other applications if this application is
        // not currently rendering.
        } else 
              WaitMessage();

    }
    return msg.wParam;
}




//-----------------------------------------------------------------------------
// Name: WindowProc()
// Desc: Handle messages for the main window.
//-----------------------------------------------------------------------------
LRESULT CALLBACK WindowProc( HWND win, UINT msg, WPARAM wparam, LPARAM lparam )
{
HRESULT rval;

switch (msg) {

    case WM_SIZE:
        // Handle resizing of the window. If the application was 
        // minimized, set a flag so rendering will stop while minimized
        // and break out of this function. Otherwise, make sure the
        // minimized flag is clear and check for window resizing other
        // than minimized.
        {
        if (SIZE_MINIMIZED == wparam) 
        {
            myglobs.bMinimized = TRUE;
            break;
        }
        else
            myglobs.bMinimized = FALSE;

        // Handle resizing of the window. Obtain the window's current
        // width and height.
        int width = LOWORD(lparam);
        int height = HIWORD(lparam);

        // If the global variables such as the viewport and device 
        // have been initialized, then obtain the viewport's and
        // device's width and height. Create a new device and window
        // device or viewport as needed. A new device and window device
        // are only needed if the window is larger than the old device.
        if (myglobs.bInitialized) 
        {
            int view_width = myglobs.view->GetWidth();
            int view_height = myglobs.view->GetHeight();
            int dev_width = myglobs.dev->GetWidth();
            int dev_height = myglobs.dev->GetHeight();

            // Check whether the window size increased in either
            // width or height.
            if (width > dev_width || height > dev_height)
            {
                // Application window is now larger than the current  
                // device. Destroy the old viewport and device and create 
                // new ones. Be sure to free the viewport before freeing
                // the device or a general protection fault might occur.
                myglobs.bInitialized = FALSE;
                RELEASE(myglobs.view);
                RELEASE(myglobs.dev);

                // Create a new default Direct3DRM device.
                rval = lpD3DRM->CreateDeviceFromClipper(lpDDClipper, 
                                        NULL, // Default device 
                                        width, height, 
                                        &myglobs.dev);

                if (FAILED(rval)) 
                {
                    // If the new device could not be created, notify 
                    // the user and shut down the application.
                    DISPLAYMSG("Failed to create the D3DRM device.");
                    PostMessage(NULL, WM_DESTROY, 0, 0);
                    return FALSE;
                } 

                // Destroy the old Direct3D Retained Mode window device 
                // and create a new one to match the newly created 
                // device. The window device handles the WM_ACTIVATE and 
                // WM_PAINT messages.
                RELEASE(myglobs.windevice);
                rval = myglobs.dev->QueryInterface(
                                       IID_IDirect3DRMWinDevice,
                                       (void **) &myglobs.windevice);
                if (FAILED(rval)) {
                    DISPLAYMSG("Failed to create the window device.");
                    return FALSE;
                }

            }

            // If the window sized changed, free the old viewport and
            // create a new viewport of the proper size.
            myglobs.bInitialized = FALSE;
            RELEASE(myglobs.view);
            rval = lpD3DRM->CreateViewport(myglobs.dev, 
                                           myglobs.camera,
                                           0, 0, width, height,
                                           &myglobs.view);
            if (FAILED(rval)) 
            {
                // If the new viewport could not be created, notify 
                // the user and shut down the application.
                DISPLAYMSG("Failed to create a new viewport.");
                PostMessage(NULL, WM_DESTROY, 0, 0);
                break;
            }

            myglobs.bInitialized = TRUE;
        }
        }
        break;

    case WM_ACTIVATE:
        // Use the window device's HandleActivate method to handle 
        // window activation.
        if (myglobs.bInitialized)
            if (FAILED(myglobs.windevice->HandleActivate(wparam)))
                DISPLAYMSG("Failed to handle WM_ACTIVATE.");
        break;

    case WM_PAINT:
        // Use the window device's HandlePaint method to handle 
        // repainting unless global variables have not yet been 
        // initialized. 
        if (!myglobs.bInitialized)
            return DefWindowProc(win, msg, wparam, lparam);

        // Use the window device to handle repainting.
        RECT rc;
        PAINTSTRUCT ps;

        if (GetUpdateRect(win, &rc, FALSE)) {
            BeginPaint(win, &ps);
            if (FAILED(myglobs.windevice->HandlePaint(ps.hdc)))
                DISPLAYMSG("Failed to handle WM_PAINT.");
        } 
        EndPaint(win, &ps);
        break;

    case WM_DESTROY:
        // Clear the bInitialized flag, free objects, and post the WM_QUIT
        // message to terminate the application. Be sure to free the 
        // viewport before freeing the device or a general protection
        // fault might occur.
        myglobs.bInitialized = FALSE;
        RELEASE(myglobs.view);
        RELEASE(myglobs.windevice);
        RELEASE(myglobs.dev);
        RELEASE(myglobs.camera);
        RELEASE(myglobs.scene);
        RELEASE(lpD3DRM);
        RELEASE(lpDDClipper);
        PostQuitMessage(0);
        break;

    default:
        return DefWindowProc(win, msg, wparam, lparam);
    }
    return 0L;
}




