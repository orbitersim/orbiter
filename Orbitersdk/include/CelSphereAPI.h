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
#include <array>

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

		virtual ~CelestialSphere();

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

		/**
		 * \brief Render a marker for a given direction on the celestial sphere.
		 * \param pSkp Sketchpad context
		 * \param rdir normalised direction from camera in global (ecliptic) frame
		 * \param label1 label above marker
		 * \param label2 label below marker
		 * \param mode marker shape
		 * \param scale marker size
		 * \note The required sketchpad settings (font, pen, font and line colours)
		 ^     must have been applied to the Sketchpad context before the call.
		 */
		virtual void RenderMarker(oapi::Sketchpad* pSkp, const VECTOR3& rdir, const std::string& label1, const std::string& label2, int mode, int scale);

		virtual void RenderConstellationLabels(oapi::Sketchpad** ppSkp, bool fullName);

		virtual void RenderCelestialMarkers(oapi::Sketchpad** ppSkp);

		virtual oapi::Font* MarkerFont() const;
		virtual oapi::Pen* MarkerPen(DWORD idx) const;
		virtual COLORREF MarkerColor(DWORD idx) const;
		virtual oapi::FVECTOR4 MarkerColorFloat(DWORD idx) const;

		virtual void EnsureMarkerDrawingContext(oapi::Sketchpad** ppSkp, oapi::Font* font = 0, COLORREF textcol = 0, oapi::Pen* pen = 0);

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

		const std::vector<VECTOR3> LoadConstellationBoundaries() const;

		/**
		 * \brief Load constellation label database from file into \ref m_cLabel array.
		 */
		virtual void LoadConstellationLabels();

		const std::vector<oapi::GraphicsClient::LABELSPEC>& ConstellationLabels() const
		{ return m_cLabel; }

		/**
		 * \brief Convert a direction into viewport coordinates
		 * \param dir direction in the ecliptic frame provided as a point on the
		 *    celestial sphere.
		 * \param x x-position in the viewport window [pixel]
		 * \param y y-position in the viewport window [pixel]
		 * \return true if point is visible in the viewport, false otherwise.
		 */
		virtual bool EclDir2WindowPos(const VECTOR3& dir, int& x, int& y) const = 0;

		/**
		 * \brief Returns the rotation matrix for mapping from J2000 ecliptic frame
		 *    to celestial frame at current epoch.
		 */
		MATRIX3 Ecliptic_CelestialAtEpoch() const;

		/**
		 * \brief Returns the rotation matrix for mapping from J2000 ecliptic frame
		 *    to J2000 celestial frame.
		 */
		const MATRIX3& Ecliptic_CelestialJ2000() const;

		/**
		 * \brief Returns the rotation matrix for mapping from galactic frame
		 *    to J2000 celestial frame.
		 */
		const MATRIX3& CelestialJ2000_Galactic() const;

		const MATRIX3& Ecliptic_Galactic() const;

		bool LocalHorizon_Ecliptic(MATRIX3& R) const;

		void SetSkyColour(const VECTOR3& skyCol);
		const VECTOR3& GetSkyColour() const { return m_skyCol; }
		double GetSkyBrightness() const { return m_skyBrt; }

		double ElevationScaleRotation(const MATRIX3& R) const;

		FVECTOR4 ColorAdjusted(const FVECTOR4& baseCol) const;
		DWORD MarkerColorAdjusted(const FVECTOR4& baseCol) const;
		DWORD TextColorAdjusted(const FVECTOR4& baseCol) const;

		/**
		 * \brief Returns the mesh for rendering tick labels for grids.
		 */
		const MESHHANDLE GridLabelMesh();

		GraphicsClient* m_gc;

		bool m_textBlendAdditive;

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
		 * \brief Map star data to a list that can be used for rendering.
		 *
		 * This function maps the star data read by \ref LoadStarData into a form
		 * that makes it easier to build a vertex list for rendering onto the celestial
		 * sphere. The ecliptic coordinates (longitude, latitude) are converted to
		 * Cartesian coordinates (x,y,z) on the unit sphere (radius 1).
		 * The apparent magnitude is mapped into a brightness value (0-1), using the user
		 * settings for star brightness. In addition, the spectral class information
		 * is mapped into red, green and blue components (0-1).
		 * \param starDataRec star data list as provided by \ref LoadStarData
		 * \param prm user choice for star render parameters
		 * \return List of transformed star data
		 */
		const std::vector<StarRenderRec> StarData2RenderData(const std::vector<StarDataRec>& starDataRec, const StarRenderPrm& prm) const;

		/**
		 * \brief Load an array of line segment definitions on the celestial sphere from file.
		 * \param fname file name (relative to Orbiter root dir)
		 * \return Vector of line endpoint pairs (ecliptic longitude,latitude)
		 * \note No connectivity is assumed. Each line definition is independent.
		 */
		const std::vector<LineDataRec> LoadEclipticLineArray(const std::string& fname) const;

		/**
		 * \brief Load constellation line data from Orbiter's data base file.
		 * \return Vector of line segment database records.
		 */
		const std::vector<LineDataRec> LoadConstellationLineData() const;

		/**
		 * \brief Load constellation boundary data from Orbiter's data base file.
		 * \return Vector of line segment database records.
		 */
		const std::vector<LineDataRec> LoadConstellationBoundaryData() const;

		/**
		 * \brief Map ecliptic line data to a list that can be used for rendering.
		 * 
		 * This function maps the star data read by \ref LoadConstellationLineData into
		 * a form that makes it easier to build a vertex list for rendering onto the
		 * celestial sphere. The ecliptic coordinates (longitude, latitude) of the line
		 * segment end points are converted to Cartesian coordinates (x,y,z) on the unit
		 * sphere (radius 1) and stored in a VECTOR3 array.
		 * \param lineDataRec constellation line data list as provided by
		 *    \ref LoadConstellationLineData.
		 * \return List of constellation line end points
		 */
		const std::vector<VECTOR3> EclipticLineData2RenderData(const std::vector<LineDataRec>& lineDataRec) const;

		std::vector<oapi::GraphicsClient::LABELSPEC> m_cLabel; ///< list of constellation labels

		DWORD m_viewW, m_viewH;          ///< viewport dimensions
		oapi::Font* m_cLabelFont;        ///< font for constellation labels
		oapi::Font* m_markerFont;        ///< font for celestial sphere markers
		oapi::Pen* m_markerPen[7];       ///< pens for celestial sphere markers
		VECTOR3 m_skyCol;                ///< background sky colour at current render pass (0-1 per channel)
		double m_skyBrt;                 ///< background brightness level at current render pass (0-1)
		std::string m_dataDir;           ///< data directory
		MESHHANDLE m_meshGridLabel;      ///< mesh for grid tick labels

	protected:
		const std::vector<GraphicsClient::ConstLabelRec> LoadConstellationLabelData() const;

		const std::vector<GraphicsClient::LABELSPEC> ConstellationLabelData2RenderData(const std::vector<GraphicsClient::ConstLabelRec>& clabelRec) const;

		std::array<int, 256> ComputeStarBrightnessCutoff(const std::vector<oapi::CelestialSphere::StarRenderRec>& starRenderRec) const;
	};

}
#endif // !__CELSPHERE_H