// Copyright (c) Martin Schweiger
// Licensed under the MIT License

/**
 * \file CelSphereAPI.h
 * \brief Defines the \ref CelestialSphere interface base class for rendering the deep
 *   space background (stars, and the "planetarium" mode representations of constellations,
 *   coordinate grids and object markers).
 */

#ifndef __CELSPHERE_H
#define __CELSPHERE_H

#include "GraphicsAPI.h"

namespace oapi {

	/**
	 * \class CelestialSphere
	 * \brief Base class for rendering the celestial sphere (the deep space background of stars,
	 *   and additionally a "planetarium" mode for displaying constellations, coordinate grids
	 *   and object markers).
	 *
	 * The CelestialSphere class implements the graphics-client independent tasks of loading the
	 * database files with star positions and brightnesses, constellation names and lines, marker
	 * positions and labels, etc. Graphics clients should implement derived classes to provide
	 * the client-dependent functionality.
	 */
	class OAPIFUNC CelestialSphere {
	public:
		CelestialSphere(GraphicsClient* gc);

		/**
		 * \brief Star database record structure, as used by \ref LoadStarData.
		 * \note The spectral class index is a linear encoding of the star's
		 *    spectral class, from O0 (0) to M9 (69) for classes OBANGKM. Since
		 *    the spectral information is only used for modifying the render
		 *    colour by Orbiter, any additional classes present in the original
		 *    database have been mapped to those seven classes. For example,
		 *    all red giant classes (R, N, C, S) have been mapped to M.
		 */
		struct StarDataRec {
			double lng;   ///< ecliptic longitude (J2000) [rad]
			double lat;   ///< ecliptic latitude (J2000) [rad]
			double mag;   ///< apparent magnitude
			WORD specidx; ///< spectral class index (0-69)
		};

		/**
		 * \brief Star render data record structure, as returned by \ref StarData2RenderData
		 * \note The brightness level is computed using the user-defined brightness mapping
		 *    parameters contained in \ref StarRenderPrm.
		 */
		struct StarRenderRec {
			VECTOR3 pos;       ///< render vertex coordinates (on unit sphere)
			VECTOR3 col;       ///< render colour (x=r, y=g, z=b, range 0-1)
			double brightness; ///< render brightness (mean of colour channels)
		};

		/**
		 * \brief Record for constellation line data.
		 * \note Constellation lines are assumed to consist of a collection of straight
		 *   line segments. Each record represents one line segment.
		 * \note Each record contains the two end point coordinates (ecliptic longitude and
		 *   latitude) of the line in the J2000 ecliptic reference frame.
		 */
		struct LineDataRec {
			double lng1;  ///< longitude of endpoint 1
			double lat1;  ///< latitude of endpoint 1
			double lng2;  ///< longitude of endpoint 2
			double lat2;  ///< latitude of endpoint 2
		};

	protected:
		/**
		 * \brief Load star data from the database file and pre-process them
		 *    according to user-defined display parameters.
		 * \return Vector of \ref StarRenderRec records providing information for
		 *    displaying stars on the celestial sphere.
		 * \note The returned records provide graphics-client independent data.
		 *    Client-specific implementations of CelestialSphere should map these
		 *    data into device-dependent data structures for rendering.
		 */
		const std::vector<StarRenderRec> LoadStars() const;

		/**
		 * \brief Load constellation line data from the database file and pre-process
		 *    them for use by graphics clients.
		 * \return Vector of vertex points for constellation line segments on the
		 *    unit sphere.
		 * \note Each pair of consecutive points in the vector defines the endpoints
		 *    of a line segment in a constellation. The length of the vector is an
		 *    even number. The points are located on the unit sphere (radius 1). 
		 */
		const std::vector<VECTOR3> LoadConstellationLines() const;

		GraphicsClient* m_gc;

	private:
		/**
		 * \brief Load star data from Orbiter's data base file.
		 *
		 * Load database records up to apparent magnitude \a maxAppMag (database
		 * is assumed to be sorted in decreasing brightness). The Hipparcos
		 * catalog contains ~118000 entries up to magnitude ~9, although some
		 * entries with higher magnitudes are present.
		 * \param maxAppMag apparent magnitude limit. The returned records are
		 *    truncated at this magnitude.
		 * \return Vector of database records.
		 */
		const std::vector<StarDataRec> LoadStarData(double maxAppMag) const;

		/**
		 * \brief Map the star data to a list that can be used for rendering.
		 *
		 * This function maps the star data read by \ref LoadStarData into a form
		 * that makes it easier to build a vertex list for rendering onto the celestial
		 * sphere. The ecliptic coordinates (longitude, latitude) are converted to
		 * Cartesian coordinates (x,y,z) assuming radius 1 for the celestial sphere.
		 * The apparent magnitude is mapped into a brightness value (0-1), using the user
		 * settings for star brightness. In addition, the spectral class information
		 * is mapped into red, green and blue components (0-1).
		 * \param starRec star data list as provided by \ref LoadStarData
		 * \param prm user choice for star render parameters
		 * \return List of transformed star data
		 */
		const std::vector<StarRenderRec> StarData2RenderData(const std::vector<StarDataRec>& starRec, const StarRenderPrm& prm) const;

		/**
		 * \brief Load constellation line data from Orbiter's data base file.
		 * \return Vector of line segment database records.
		 */
		const std::vector<LineDataRec> LoadConstellationLineData() const;

		const std::vector<VECTOR3> ConstellationLineData2RenderData(const std::vector<LineDataRec>& clineRec) const;
	};

}
#endif // !__CELSPHERE_H