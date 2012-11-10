// =================================================================================================================================
// The MIT Lisence:
//
// Copyright (C) 2012 Jarmo Nikkanen
//
// Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation 
// files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, 
// modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software 
// is furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
// OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
// LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR
// IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
// =================================================================================================================================

#ifndef __DEBUGCONTROLS_H
#define __DEBUGCONTROLS_H

/// \defgroup dbgprm Debug control configuration parameter identifiers
/// Used by DebugControls::GetConfigParam()
/// @{

/// Boolean flag for "Highlight selected mesh" element.
/// \par Parameter type:
///   DWORD
#define CFGPRM_GETSELECTEDMESH			0x2001

/// Boolean flag for "Highlight selected group" element.
/// \par Parameter type:
///   DWORD
#define CFGPRM_GETSELECTEDGROUP			0x2002

/// Bit flag for "debug-flags" elements.
/// For a description of the available bit flags, see \ref dbgflgs
/// \par Parameter type:
///   DWORD
#define CFGPRM_GETDEBUGFLAGS			0x2003

/// Display mode setting
/// \par Parameter type:
///   DWORD
#define CFGPRM_GETDISPLAYMODE			0x2004

/// Camera mode setting
/// \par Parameter type:
///   DWORD
#define CFGPRM_GETCAMERAMODE			0x2005

/// Value of camara speed setting
/// \par Parameter type:
///   double
#define CFGPRM_GETCAMERASPEED			0x2006

/// @}


/// \defgroup dbgflgs Debug control parameter identifiers
/// @{
#define DBG_FLAGS_SELMSHONLY			0x0001	///< Selected mesh only
#define DBG_FLAGS_SELGRPONLY			0x0002	///< Selected group only
#define DBG_FLAGS_BOXES					0x0004	///< Boxes
#define DBG_FLAGS_SPHERES				0x0008	///< Spheres
#define DBG_FLAGS_HLMESH				0x0010	///< Highlight selected mesh
#define DBG_FLAGS_HLGROUP				0x0020	///< Highlight selected group
#define DBG_FLAGS_TILES					0x0040	///< Planet tile debugger
#define DBG_FLAGS_SELVISONLY			0x0080	///< Selected visual only
#define DBG_FLAGS_AMBIENT				0x0100	///< Add Ambient Light for Visual
#define DBG_FLAGS_WIREFRAME				0x0200	///< Enable WireFrame
#define DBG_FLAGS_DUALSIDED				0x0400	///< Dual Sided

/// @}


class vObject;

// ==============================================================

namespace DebugControls {

	extern  DWORD sMesh;
	extern  DWORD sGroup;
	extern  DWORD debugFlags;
	extern  DWORD dspMode;
	extern  DWORD camMode;
	extern  double camSpeed;
	
	/**
	 * \brief Same functionality than 'official' GetConfigParam, but for
	 * non-provided debug-control config parameters
	 *
	 * This function can be used to access various configuration parameters
	 * defined in the DebugControls core (e.g. debugFlags or camera settings).
	 * \param paramtype Parameter identifier (see \ref dbgprm)
	 * \return Pointer to parameter
	 * \note The pointer must be cast into the appropriate variable type.
	 *   The variable types can be found in the parameter type list (\ref
	 *   cfgprm).
	 * \par Example:
	 * \code
	 * double speed = *(double*)GetConfigParam(CFGPRM_GETCAMERASPEED);
	 * \endcode
	 */
	const void *GetConfigParam (DWORD paramtype);

	void		Create();
	void		Release();
	void		OpenDlgClbk(void *context);

	void		SetVisual(vObject *vo);
	void		RemoveVisual(vObject *vo);
	vObject *	GetVisual();

	void		SetupMeshGroups();
	void		UpdateVisual();
	void		UpdateFlags();
	
	bool		IsActive();

	BOOL CALLBACK WndProc(HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam);
};

#endif // !__DEBUGCONTROLS_H
