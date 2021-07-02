// ======================================================================
//                     ORBITER SOFTWARE DEVELOPMENT KIT
//                  Copyright (C) 2001-2005 Martin Schweiger
//                           All rights reserved
// MFDAPI.h
// Class interfaces for MFD instruments and MFD modes
// ======================================================================

/**
 * \file MFDAPI.h
 * \brief Class interfaces for MFD instruments and MFD modes.
 */

#ifndef __MFDAPI_H
#define __MFDAPI_H

#ifndef SKIP_MFD_API
#include "OrbiterAPI.h"

class Instrument;

// ======================================================================
// class MFD
// ======================================================================

/**
 * \brief This class acts as an interface for user defined %MFD (multi functional display) modes
 *
 * This class acts as an interface for user defined %MFD (multi functional display) modes. It
 * provides control over keyboard and mouse functions to manipulate the %MFD mode, and
 * allows the module to draw the %MFD display. The %MFD class is a pure virtual class. Each userdefined
 * %MFD mode requires the definition of a specialised class derived from %MFD. An
 * example for a generic %MFD mode implemented as a plugin module can be found in
 * orbitersdk\\samples\\CustomMFD.
 */
// ======================================================================

class OAPIFUNC MFD {
	friend class MFD2;

public:
	/** 
	 * \brief Constructor. Creates a new MFD.
	 * \param w width of the MFD display (pixel)
	 * \param h height of the MFD display (pixel)
	 * \param vessel pointer to VESSEL interface associated with the MFD.
	 * \note MFD is a pure virtual function, so it can't be instantiated directly. It is used as
     *	a base class for specialised MFD modes.
	 * \note New MFD modes are registered by a call to oapiRegisterMFDMode().
     *	Whenever the new mode is selected by the user, Orbiter sends a
 	 *	OAPI_MSG_MFD_OPENED signal to the message handler, to which the
 	 *	module should respond by creating the MFD mode and returning a pointer to
	 *	it. Orbiter will automatically destroy the MFD mode when it is turned off.
	 */
	MFD (DWORD w, DWORD h, VESSEL *vessel);

	/**
	 * \brief MFD destructor.
	 */
	virtual ~MFD();

	/**
	 * \brief Callback function: Orbiter calls this method when the MFD needs to update its display.
	 * \param hDC Windows device context for drawing on the MFD display surface.
	 * \note The frequency at which this function is called corresponds to the "MFD
	 *	refresh rate" setting in Orbiter's parameter settings, unless a redraw is forced
	 *	by InvalidateDisplay().
	 * \deprecated This method is deprecated. %MFD implementations should derive from MFD2
	 *   and use the device-independent \ref MFD2::Update(oapi::Sketchpad*) method instead.
	 */
	virtual void Update (HDC hDC) = 0;

	/**	
	 * \brief Force a display update in the next frame.
	 *
	 * Force a display update in the next frame. This function causes Orbiter to call the
	 * MFD's Update method in the next frame.
	 */
	void InvalidateDisplay ();

	/**
	 * \brief Force the MFD buttons to be redrawn. 
	 *  
	 * Force the MFD buttons to be redrawn. This is useful to alert Orbiter
	 * that the MFD mode has dynamically modified its button labels.
	 * \note Orbiter will call the MFD::ButtonLabel method to retrieve the new button
	 *	 labels. Therefore this must have been updated to return the new labels
  	 *	 before calling InvalidateButtons().
	 * \note If the MFD is part of a 2-D panel view or 3-D virtual cockpit view, Orbiter
	 *	 calls the VESSEL2::clbkMFDMode() method to allow the vessel to update its
	 *	 button labels. If the MFD is one of the two glass cockpit MFD displays, the
	 *	 buttons are updated internally.
	 * \note If the MFD is displayed in an external window, Orbiter calls the
	 *	 ExternMFD::clbkRefreshButtons() method to refresh the buttons. 
	 */
	void InvalidateButtons ();

	/**
	 * \brief Displays a title string in the upper left corner of the MFD display.
	 * \param hDC device context
	 * \param title title string (null-terminated)
	 * \note This method should be called from within Update()
	 * \note The title string can contain up to approx. 35 characters when displayed in the
	 *	 default Courier MFD font.
	 * \note This method switches the text colour of the GDI context to white.
	 * \deprecated This method is deprecated. %MFD implementations should derive from MFD2
	 *   and use the device-independent \ref MFD2::Title method instead.
	 */
	void Title (HDC hDC, const char *title) const;

	/**
	 * \brief Selects a predefined pen into the device context.
	 * \param hDC Windows device context
	 * \param i pen index
	 * \return Handle of pen being replaced.
	 * \note Currently supported are pen indices 0-5, where\n\n
	 *		0 = solid, HUD display colour\n
	 *		1 = solid, light green\n
	 *		2 = solid, medium green\n
	 *		3 = solid, medium yellow\n
	 *		4 = solid, dark yellow\n
	 *		5 = solid, medium grey\n\n
	 * \note In principle, an MFD mode may create its own pen resources using the
	 *	 standard Windows CreatePen function, but using predefined pens is
	 *   preferred to provide a consistent MFD look.
	 * \deprecated This method is deprecated. %MFD implementations should derive from MFD2
	 *   and use the device-independent \ref MFD2::GetDefaultPen method instead.
	 */
	HPEN SelectDefaultPen (HDC hDC, DWORD i) const;

	/**
	 * \brief Selects a predefined MFD font into the device context
	 * \param hDC Windows device context
	 * \param i font index
	 * \return Handle of font being replaced.
	 * \note Currently supported are font indices 0-3, where \n\n
	 *		0 = standard MFD font (<tt>Courier, fixed pitch</tt>)\n
	 *		1 = small font (<tt>Arial, variable pitch</tt>)\n
	 *		2 = small font, rotated 90 degrees (<tt>Arial, variable pitch</tt>)\n
	 *      3 = medium font, (<tt>Arial, variable pitch</tt>)\n\n
	 * \note In principle, an MFD mode may create its own fonts using the standard
	 *   Windows CreateFont function, but using the predefined fonts is preferred to
	 *   provide a consistent MFD look.
	 * \note Default fonts are scaled automatically according to the MFD display size.
	 * \deprecated This method is deprecated. %MFD implementations should derive from MFD2
	 *   and use the device-independent \ref MFD2::GetDefaultFont method instead.
	 */
	HFONT SelectDefaultFont (HDC hDC, DWORD i) const;

	/** 
	 * \brief MFD keyboard handler for buffered keys.
	 * \param key key code (<i>see OAPI_KEY_xxx constants in orbitersdk.h</i>)
	 * \return The function should return true if it recognises and processes the key, false otherwise.
	 */ 
	virtual bool ConsumeKeyBuffered (DWORD key) { return false; }

	/**
	 * \brief MFD keyboard handler for immediate (unbuffered) keys.
	 * \param kstate keyboard state.
	 * \return The function should return true only if it wants to inhibit Orbiter's default
	 * immediate key handler for this time step completely.
	 * \note The state of single keys can be queried by the KEYDOWN macro.
     * \note The immediate key handler is useful where an action should take place while a key is pressed.
	 */
	virtual bool ConsumeKeyImmediate (char *kstate) { return false; }

	/**
	 * \brief MFD button handler. 
	 *
	 * MFD button handler. This function is called when the user performs a mouse click on 
	 * a panel button associated with the MFD.
 	 * \param bt button identifier.
	 * \param event mouse event (<i>see PANEL_MOUSE_xxx constants in orbitersdk.h</i>)
	 * \return The function should return true if it processes the button event, false otherwise.
	 * \note This function is invoked as a response to a call to oapiProcessMFDButton() in a vessel module.
	 * \note Typically, ConsumeButton() will call ConsumeKeyBuffered() or ConsumeKeyImmediate() to emulate a keyboard event.
	 */
	virtual bool ConsumeButton (int bt, int event) { return false; }

	/** 
	 * \brief Return the label for the specified MFD button.
	 * \param bt button identifier
     * \return The function should return a 0-terminated character string of up to 3 characters,
	 *	or NULL if the button is unlabelled.
	 * \bug This function should really return a const char*
	 */
	virtual char *ButtonLabel (int bt) { return 0; }

	/**
	 * \brief Defines a list of short descriptions for the various MFD mode button/key functions.
	 * \param menu on return this should point to an array of button menu items. (see notes)
	 * \return number of items in the list
	 * \note The definition of the MFDBUTTONMENU struct is:\n\n
	 *	<tt>typedef struct {\n
	 *	       const char *line1, *line2; \n
	 *	       char selchar; \n
	 *	    } MFDBUTTONMENU;</tt>\n\n
	 * containing up to 2 lines of short description, and the keyboard key to trigger the function.\n\n
	 * \note Each line should contain no more than 16 characters, to fit into the MFD display.
	 * \note If the menu item only uses one line, then line2 should be set to NULL.
	 * \note menu==0 is valid and indicates that the caller only requires the number ofitems, not the actual list.
	 * \n\n A typical implementation would be
	 * \code
	 *	int MyMFD::ButtonMenu (const MFDBUTTONMENU **menu) const
	 *	{
	 *		static const MFDBUTTONMENU mnu[2] = {
	 *		{"Select target", 0, 'T'},
	 *		{"Select orbit", "reference", 'R'}
	 *		};
	 *		if (menu) *menu = mnu;
	 *		return 2;
	 *	}
	 * \endcode
	 */
	virtual int  ButtonMenu (const MFDBUTTONMENU **menu) const { return 0; }

	/** 
	 * \brief Called when the MFD should write its status to a scenario file.
	 * \param scn scenario file handle (write only)
	 * \note Use the oapiWriteScenario_xxx functions to write MFD status parameters to the scenario.
	 * \note The default behaviour is to do nothing. MFD modes which need to save status parameters 
	 *   should overwrite this function.
	 */
	virtual void WriteStatus (FILEHANDLE scn) const {}

	/** 
	 * \brief Called when the MFD should read its status from a scenario file.
	 * \param scn scenario file handle (read only)
	 * \note Use a loop with oapiReadScenario_nextline() to read MFD status parameters from the scenario.
	 * \note The default behaviour is to do nothing. MFD modes which need to read
	 *	 status parameters should overwrite this function.
	 */
	virtual void ReadStatus (FILEHANDLE scn) {}

	/**
	 * Called before destruction of the MFD mode, to allow the mode to save its status to static memory.
	 * \note This function is called before an MFD mode is destroyed (either because the
	 *   MFD switches to a different mode, or because the MFD itself is destroyed). It
	 *   allows the MFD to back up its status parameters, so it can restore its last
	 *   status when it is created next time.
	 * \note Since the MFD mode instance is about to be destroyed, status parameters
	 *   should be backed up either in static data members, or outside the class instance.
	 * \note In principle this function could be implemented by opening a file and calling
	 *   WriteStatus() with the file handle. However for performance reasons file I/O
	 *   should be avoided in this function.
	 * \note The default behaviour is to do nothing. MFD modes which need to save status 
	 *   parameters should overwrite this function.
	 */
	virtual void StoreStatus () const {}

	/**
	 * Called after creation of the MFD mode, to allow the mode to restore its status from the last save.
	 * \note This is the counterpart to the StoreStatus() function. It should be implemented
	 *   if and only if StoreStatus() is implemented.
	 */
	virtual void RecallStatus () {}

protected:
	DWORD W, H;   //!< width and height of MFD display area (pixel)
	DWORD cw, ch; //!< character width, height of standard MFD font 0 (pixel)
	VESSEL *pV;   //!< pointer to vessel interface

private:
	friend class Instrument_User;  // Orbiter private class
	Instrument_User *instr;
};



// ======================================================================
// class MFD2
// ======================================================================
/**
 * \brief Extended MFD class.
 *
 * MFD2 replaces GDI-specific functions with versions that use the generic
 * Sketchpad class. MFD addons should derive from MFD2 instead of MFD, to
 * ensure compatibility with non-GDI graphics clients.
 */
class OAPIFUNC MFD2: public MFD {
public:
	/** 
	 * \brief Constructor. Creates a new MFD.
	 * \param w width of the MFD display (pixel)
	 * \param h height of the MFD display (pixel)
	 * \param vessel pointer to VESSEL interface associated with the MFD.
	 * \note MFD is a pure virtual function, so it can't be instantiated directly. It is used as
     *	a base class for specialised MFD modes.
	 * \note New MFD modes are registered by a call to oapiRegisterMFDMode().
     *	Whenever the new mode is selected by the user, Orbiter sends a
 	 *	OAPI_MSG_MFD_OPENED signal to the message handler, to which the
 	 *	module should respond by creating the MFD mode and returning a pointer to
	 *	it. Orbiter will automatically destroy the MFD mode when it is turned off.
	 */
	MFD2 (DWORD w, DWORD h, VESSEL *vessel): MFD (w, h, vessel) {}

	/**
	 * \brief MFD destructor.
	 */
	~MFD2() {}

	/**
	 * \brief Returns the MFD display width.
	 * \return MFD display width [pixel]
	 * \sa GetHeight
	 */
	inline DWORD GetWidth() const { return W; }

	/**
	 * \brief Returns the MFD display height.
	 * \return MFD display height [pixel]
	 * \sa GetWidth
	 */
	inline DWORD GetHeight() const { return H; }

	/**
	 * \brief Dummy implementation of GDI-specific base class method.
	 * \note Derived classes should overload the \ref Update(oapi::Sketchpad*) method instead.
	 */
	void Update (HDC hDC) {}

	/**
	 * \brief Callback function: Orbiter calls this method when the MFD needs to update its display.
	 * \param skp Drawing context for drawing on the MFD display surface.
	 * \note The frequency at which this function is called corresponds to the "MFD
	 *	refresh rate" setting in Orbiter's parameter settings, unless a redraw is forced
	 *	by InvalidateDisplay().
	 * \note This function must be overwritten by derived classes.
	 */
	virtual bool Update (oapi::Sketchpad *skp);

	/**
	 * \brief Displays a title string in the upper left corner of the MFD display.
	 * \param skp Drawing context
	 * \param title title string (null-terminated)
	 * \note This method should be called from within Update()
	 * \note The title string can contain up to approx. 35 characters when displayed in the
	 *	 default Courier MFD font.
	 * \note This method switches the text colour of the drawing context to white.
	 */
	void Title (oapi::Sketchpad *skp, const char *title) const;

	/**
	 * \brief Returns a predefined MFD pen resource.
	 * \param colidx pen colour index (see notes)
	 * \param intens pen brightness (0=bright, 1=dark)
	 * \param style pen style (1=solid, 2=dashed)
	 * \return pen resource
	 * \note Valid colour indices are 0 to 4, where
	 *  <table>
	 *  <tr><td>Index</td><td>Function</td><td>default colour</td></tr>
	 *  <tr><td>0</td><td>Main MFD colour</td><td>green</td></tr>
	 *  <tr><td>1</td><td>Auxiliary colour 1</td><td>yellow</td></tr>
	 *  <tr><td>2</td><td>Auxiliary colour 2</td><td>white</td></tr>
	 *  <tr><td>3</td><td>Auxiliary colour 3</td><td>red</td></tr>
	 *  <tr><td>4</td><td>Auxiliary colour 4</td><td>blue</td></tr>
	 *  </table>
	 * \note To select the pen for drawing in the MFD display, call the MFD
	 *   drawing context's oapi::Sketchpad::SetPen method.
	 * \note The default colours can be overridden by editing Config/MFD/default.cfg.
	 * \note In principle, an MFD mode may create its own pen resources using the
	 *	 oapi::GraphicsClient::clbkCreatePen function, but using predefined pens is
	 *   preferred to provide a consistent MFD look, and to avoid excessive allocation
	 *   of drawing resources.
	 * \sa oapi::Sketchpad::SetPen
	 */
	oapi::Pen *GetDefaultPen (DWORD colidx, DWORD intens=0, DWORD style=1) const;

	/**
	 * \brief Returns a predefined MFD font resource.
	 * \param fontidx font index
	 * \return font resource
	 * \note Currently supported are font indices 0-2, where \n\n
	 *		0 = standard MFD font (<tt>Courier, fixed pitch</tt>)\n
	 *		1 = small font (<tt>Arial, variable pitch</tt>)\n
	 *		2 = small font, rotated 90 degrees (<tt>Arial, variable pitch</tt>)\n\n
	 * \note To select the font for drawing in the MFD display, call the MFD
	 *   drawing context's oapi::Sketchpad::SetFont method.
	 * \note In principle, an MFD mode may create its own fonts using the standard
	 *   Windows CreateFont function, but using the predefined fonts is preferred to
	 *   provide a consistent MFD look.
	 * \note Default fonts are scaled automatically according to the MFD display size.
	 */
	oapi::Font *GetDefaultFont (DWORD fontidx) const;

	/**
	 * \brief Returns the colour value for a specified colour index and intensity.
	 * \param colidx colour index (see notes)
	 * \param intens colour brightness (0=bright, 1=dark)
	 * \return Colour value in 0xBBGGRR format.
	 * \note Valid colour indices are 0 to 4, where
	 *  <table>
	 *  <tr><td>Index</td><td>Function</td><td>default colour</td></tr>
	 *  <tr><td>0</td><td>Main MFD colour</td><td>green</td></tr>
	 *  <tr><td>1</td><td>Auxiliary colour 1</td><td>yellow</td></tr>
	 *  <tr><td>2</td><td>Auxiliary colour 2</td><td>white</td></tr>
	 *  <tr><td>3</td><td>Auxiliary colour 3</td><td>red</td></tr>
	 *  <tr><td>4</td><td>Auxiliary colour 4</td><td>blue</td></tr>
	 *  </table>
	 * \note The returned colour values can be used to set standard text,
	 *   pen or brush colours for particular display elements.
	 * \sa oapi::Sketchpad::SetTextColor
	 */
	DWORD GetDefaultColour (DWORD colidx, DWORD intens=0) const;
};


// ======================================================================
// class GraphMFD
// ======================================================================

/** 
 * \brief This class is derived from MFD and provides a template for MFD modes containing 2D graphs
 *
 * This class is derived from MFD and provides a template for MFD modes containing 2D
 * graphs. An example is the ascent profile recorder in the samples\\CustomMFD folder.
 */
class OAPIFUNC GraphMFD: public MFD {
public:
	/**
	 * \brief Constructor. Creates a new GraphMFD.
	 * \param w width of the MFD display (pixel)
	 * \param h height of the MFD display (pixel)
	 * \param vessel pointer to VESSEL interface associated with the MFD
	 */
	GraphMFD (DWORD w, DWORD h, VESSEL *vessel);

	virtual ~GraphMFD ();

	/** 
	 * \brief Adds a new graph to the MFD.
	 * \return graph identifier
	 * \note This function allocates data for a new graph. To display plots in the new
	 *	 graph, one or more calls to AddPlot are required.
	 */
	int AddGraph (void);

	/**
	 * \brief Adds a plot to an existing graph.
	 * \param g graph identifier
	 * \param absc pointer to array containing the abscissa (x-axis) values.
	 * \param data pointer to array containing the data (y-axis) values.
	 * \param ndata number of data points
	 * \param col plot colour index
	 * \param ofs pointer to data offset (optional)
	 * \note Data arrays are not copied, so they should not be deleted after the call to AddPlot().
	 * \note col is used as an index to select a pen for the plot using the
	 *	 SelectDefaultPen function. Valid range is the same as for SelectDefaultPen().
	 * \note If defined, *ofs is the index of the first plot value in the data array. The plot is
	 *	 drawn using the points *ofs to ndata-1, followed by points 0 to *ofs-1. This
	 *   allows to define continuously updated plots without having to copy blocks of
	 *   data within the arrays.
	 */
	void AddPlot (int g, float *absc, float *data, int ndata, int col, int *ofs = 0);

	/**
	 * \brief Sets a fixed range for the x or y axis of a graph.
	 * \param g graph identifier
	 * \param axis axis identifier (0=x, 1=y)
	 * \param rmin minimum value
	 * \param rmax maximum value
	 * \note The range applies to all plots in the graph.
	 */
	void SetRange (int g, int axis, float rmin, float rmax);

	/**
	 * \brief Allows the graph to set its range automatically according to the range of the plots.
	 * \param g graph identifier
	 * \param axis axis identifier (0=x, 1=y)
	 * \param p plot identifier (-1=all)
     * \note If p>=0, then p specifies the plot used for determining the graph range. If p = -1,
	 *   then all of the graph's plots are used to determine the range.
	 */
	void SetAutoRange (int g, int axis, int p = -1);

	/**
	 * \brief Calculates tick intervals for a given graph and axis.
	 * \param g graph identifier
	 * \param axis axis identifier (0=x, 1=y)
	 * \note This function is called from within SetRange and normally doesn't need to be
	 *   called explicitly by derived classes.
	 */
	void SetAutoTicks (int g, int axis);

	/** 
	 * \brief Sets the title for a given graph and axis.
	 * \param g graph identifier
	 * \param axis axis identifier (0=x, 1=y)
	 * \param title axis title
	 * \note The MFD may append an extension of the form "x \<scale\>" to the title,
	 *   where \<scale\> is a scaling factor applied to the tick labels of the axis. It is
	 *   therefore a good idea to finish the title with the units applicable to the data of
	 *   this axis, so that for example a title "Altitude: km" may become "Altitude: km x 1000".
	 */
	void SetAxisTitle (int g, int axis, char *title);

	/**
	 * \brief Displays a graph.
	 * \param hDC Windows device context
	 * \param g graph identifier
	 * \param h0 upper boundary of plot area (pixel)
	 * \param h1 lower boundary of plot area (pixel)
	 * \param title graph title
	 * \note This function should be called from Update to paint the graph(s) into the
	 *   provided device context.
	 */
	void Plot (HDC hDC, int g, int h0, int h1, const char *title = 0);

	/**
	 * \brief Determines the range of an array of data.
	 * \param d data array
	 * \param ndata number of data
	 * \param dmin minimum value on return
	 * \param dmax maximum value on return
	 */
	void FindRange (float *d, int ndata, float &dmin, float &dmax) const;

protected:
	int ngraph;  // number of graphs in MFD
	struct GRAPH {
		int nplot; // number of plots in graph
		struct PLOT {
			float *absc;
			float *data;
			int ndata;
			int col;
			int *ofs;
		} *plot;
		float absc_min, absc_max;
		float data_min, data_max;
		float absc_dtick, absc_tickmin, absc_tickscale;
		float data_dtick, data_tickmin, data_tickscale;
		int absc_minortick, data_minortick;
		char absc_title[64], data_title[64];
	} *graph;
};

// ======================================================================
// class ExternMFD
// ======================================================================

/**
 * \brief ExternMFD provides support for defining an MFD display in a plugin module.
 *
 * ExternMFD provides support for defining an MFD display in a plugin module, e.g. for
 * displaying the MFD in a dialog box. Unlike the MFD class described above, which defines a
 * logical MFD mode, this class represents an actual MFD instrument, i.e. the physical display
 * and associated push buttons.\n
 * A plugin module should derive its own MFD class from ExternMFD and overload the virtual
 * notification callback methods.\n
 * The class interface is defined in Orbitersdk\\include\\MFDAPI.h.\n
 * For an example using the ExternMFD class, see project Orbitersdk\\samples\\ExtMFD.
 */

class OAPIFUNC ExternMFD {
public:
	/**
	 * \brief Constructor. Creates a new instance of ExternMFD.
	 * \param spec structure containing MFD layout geometry data
	 * \note To use a new MFD instance, it must be registered with Orbiter via a call to 
	 *   oapiRegisterExternMFD(), e.g. with oapiRegisterExternMFD(new ExternMFD (spec));
	 * \note To unregister an MFD instance, use oapiUnregisterExternMFD(). Note that
	 *   oapiUnregisterExternMFD() automatically calls the ~ExternMFD() destructor, so
	 *   the plugin should not try to delete the MFD instance manually.
	 */
	ExternMFD (const MFDSPEC &spec);

	/**
	 * \brief Destructor. Deallocates the ExternMFD instance.
	 * \note The destructor should not be called directly by the module. Instead, a call to
	 *	 oapiUnregisterExternMFD() will invoke the ~ExternMFD() destructor (or the
	 *	 overloaded destructor of a derived class), as well as remove the MFD
	 *	 instance from Orbiter's internal list of MFDs.
	 */
	virtual ~ExternMFD ();

	/**
	 * \brief Returns an identifier for the MFD instance.
	 * \return A unique identifier for the MFD instance.
	 * \note Unlike the internal MFD instances (e.g. MFDs embedded in panels) whose
	 *   identifiers are in the range 0 ... MAXMFD-1, the ExternMFD class simply
	 *   uses its own instance pointer (UINT)this to create an identifier.
	 */
	UINT Id() const;

	/**
	 * \brief Returns a flag indicating active/passive MFD state.
	 * \return \c true indicates that the MFD is active (switched on), \c false indicates inactive (switched off).
	 */
	bool Active () const;
	
	/**
	 * \brief Returns the handle of the vessel associated with the MFD.
	 * \return Vessel handle associated with the MFD.
	 * \note Normally, the ExternMFD class always connects to the "focus vessel", i.e.
	 *   the vessel receiving user input. If the user switches to a different vessel (e.g.
	 *   via F3), then ExternMFD re-attaches itself to the new vessel.
	 * \note This behaviour can be changed by overloading the clbkFocusChanged()
	 *   method. For example, the MFD could be forced to stick to a given vessel, regardless of the focus object.
	 */
	OBJHANDLE GetVessel () const;

	/** 
	 * \brief Attaches the MFD to a different vessel.
	 * \param hV vessel handle\n\n
	 * <b>Default behaviour:</b>
	 *    Sets the vessel reference to hV. If an MFD mode is active, the mode is closed
	 *    and reopened with the new vessel reference.
	 */
	virtual void SetVessel (OBJHANDLE hV);

	/** 
	 * \brief Returns a handle to the surface containing the current MFD display.
	 * \return Handle to the MFD display surface.
	 * \note The handle can be used to modify or copy the current contents of the MFD
	 *   display. For example, you can obtain a GDI drawing device context for the
	 *   surface with oapiGetDC().
	 */
	SURFHANDLE GetDisplaySurface () const;

	/**
	 * \brief Returns the label currently associated with one of the MFD buttons.
	 * \param bt button number (0 <= bt < nbuttons)
	 * \return Pointer to the label associated with the button (up to 3 characters, zeroterminated),
	 *  or NULL if no function is associated with the button by the current MFD mode.
	 * \note The number of buttons provided by the MFD depends on the data passed to
	 *   the constructor in the MFDSPEC structure.
	 * \note The module can use this method to update its button labels within the
	 *   clbkRefreshButtons() callback function.
	 */
	const char *GetButtonLabel (int bt) const;
	
   /*! \b TODO */
	bool ProcessButton (int bt, int event);

   /*! \b TODO */
	bool SendKey (DWORD key);

   /*! \b TODO */
	bool Resize (const MFDSPEC &spec);

   /*! \b TODO */
	bool SetMode (int mode);

   /*! \b TODO */
	bool OpenModeHelp () const;

	
	// callback functions
	
    /*! \b TODO */
	virtual void clbkUpdate ();

    /*! \b TODO */
	virtual void clbkRefreshDisplay (SURFHANDLE hSurf);

    /*! \b TODO */
	virtual void clbkRefreshButtons ();

    /*! \b TODO */
	virtual void clbkFocusChanged (OBJHANDLE hFocus);

protected:
	OBJHANDLE hVessel; //!< vessel associated with the MFD
	int DW, DH;        //!< display width, height (pixel)
	int pmode;         //!< previous mode identifier
	int nbt1, nbt2;    //!< number of left, right buttons
	int bty0, btdy;    //!< geometry parameters
	int btpressed;     //!< currently pressed button (-1 if none)

public:
	Instrument *instr; // reserved
};

#endif // !SKIP_MFD_API
#endif // !__MFDAPI_H

