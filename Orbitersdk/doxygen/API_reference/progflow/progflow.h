/**
  \page progflow Orbiter program flow and module callback order

  This section defines the program flow of the Orbiter frame loop and the order in which module callback
  functions are called by Orbiter.

  \section Contents
  \subpage progflow1 \n
  \subpage progflow2 \n
  \subpage progflow3

  \page progflow1 The frame update loop and vessel module callback functions

  \section fameupd Frame update diagram
  The program flow diagram below illustrates the events in the Orbiter frame update loop and the calling order
  of VESSEL2 callback functions.
  The clbkPreStep and clbkPostStep methods are called for every vessel at each frame update. Other callback
  functions are only called when the associated event has occurred. Some callback functions such as
  clbkPanelRedrawEvent may be called multiple times for a vessel in a single frame.
  \image html progflow1.png "Orbiter frame update loop and position of VESSEL2 callback functions." width=10cm
  \sa VESSEL2::clbkSetClassCaps,
    VESSEL2::clbkLoadStateEx,
	VESSEL2::clbkPostCreation,
    VESSEL2::clbkPreStep,
    VESSEL2::clbkPostStep,
	VESSEL2::clbkVisualCreated,
	VESSEL2::clbkVisualDestroyed,
	VESSEL3::clbkDrawHUD,
	VESSEL3::clbkRenderHUD,
	VESSEL2::clbkRCSMode,
	VESSEL2::clbkADCtrlMode,
	VESSEL2::clbkHUDMode,
	VESSEL2::clbkMFDMode,
	VESSEL2::clbkLoadGenericCockpit,
	VESSEL3::clbkLoadPanel2D,
	VESSEL3::clbkPanelMouseEvent,
	VESSEL3::clbkPanelRedrawEvent,
	VESSEL2::clbkLoadVC,
	VESSEL2::clbkVCMouseEvent,
	VESSEL2::clbkVCRedrawEvent,
    VESSEL2::clbkConsumeDirectKey,
	VESSEL2::clbkConsumeBufferedKey,
	VESSEL2::clbkSaveState
  \page progflow2

  \page progflow3
  */