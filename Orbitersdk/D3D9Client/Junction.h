// ==============================================================
// Junction.h
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// Dual licensed under GPL v3 and LGPL v3
// Copyright (C) 2012 Peter Schneider (Kuddel)
//
// With a lot of help from Microsoft and some very important ideas
// regarding 'union padding' from Mahmoud Al-Qudsi of NeoSmart
// (Under MIT License)
// ==============================================================

#ifndef __JUNCTION_H
#define __JUNCTION_H

#include <Windows.h>

/**
 * \brief Junction point handling API.
 * Functions to create junction points (a.k.a. symbolic links) as required by
 * some addons like the 'spacecraft3'
 */
namespace junction
{
	/**
	 * \brief Creates a junction point.
	 * This functions creates a junction point (a.k.a symbolic link).
	 * \param origin The path the junction points to.
	 * \param junction The path to the junction.
	 * \return \e true on success, \e false otherwise.
	 * \note All relative paths are relative to the Orbiter root directory!
	 */
	bool CreateJunctionPoint(LPCSTR origin, LPCSTR junction);


	/**
	 * \brief Checks whether a (target-)directory exists.
	 * This function checks whether the directory \p path exists and is a
	 * directory.
	 * \param path The path to check
	 * \param attributes Optional file attributes
	 * \return Whether the directory exists.
	 * \note All relative paths are relative to the Orbiter root directory!
	 */
	bool TargetDirectoryExists(LPCSTR path, DWORD attributes = 0);


	/**
	 * \brief Checks whether a directory is a junction point.
	 * This function checks whether the directory \p path is a junction point.
	 * \param path The path to check
	 * \param attributes Optional file attributes
	 * \return Whether the directory is a junction point.
	 * \note All relative paths are relative to the Orbiter root directory!
	 */
	bool IsDirectoryJunction(LPCSTR path, DWORD attributes = 0);
}

#endif // !__JUNCTION_H
