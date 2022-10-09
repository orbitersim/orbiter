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

//#pragma pack(push,1)
		/**
		 * \brief Star database record structure, as used by \ref LoadStarData
		 * \note The spectral class index is a linear encoding of the star's
		 *    spectral class, from O0 (0) to M9 (69) for classes OBANGKM. Since
		 *    the spectral information is only used for modifying the render
		 *    colour by Orbiter, any additional classes present in the original
		 *    database have been mapped to those seven classes. For example,
		 *    all red giant classes (R, N, C, S) have been mapped to M.
		 */
		struct StarRec {
			double lng; ///< ecliptic longitude (J2000) [rad]
			double lat; ///< ecliptic latitude (J2000) [rad]
			double mag; ///< apparent magnitude
			WORD specidx; ///< spectral class index (0-69)
		};
//#pragma pack(pop)

		/**
		 * \brief Star render data record structure, as returned by \ref StarData2RenderData
		 * \note The brightness level is computed using the user-defined brightness mapping
		 *    parameters contained in \ref StarRenderPrm.
		 */
		struct StarRenderRec {
			float x;           ///< vertex x coordinate
			float y;           ///< vertex y coordinate
			float z;           ///< vertex z coordinate
			float brightness;  ///< apparent brightness level (0-1)
			float r;           ///< red intensity (0-1)
			float g;           ///< green intensity (0-1)
			float b;           ///< blue intensity (0-1)
		};

	protected:
		const std::vector<StarRenderRec> LoadStars();

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
		const std::vector<StarRec> LoadStarData(double maxAppMag) const;

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
		const std::vector<StarRenderRec> StarData2RenderData(const std::vector<StarRec>& starRec, const StarRenderPrm& prm) const;
	};

}
#endif // !__CELSPHERE_H