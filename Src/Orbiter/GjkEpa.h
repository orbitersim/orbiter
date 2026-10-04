// Copyright (c) EvESpirit
// Licensed under the MIT License

// =======================================================================
// GjkEpa.h
// Convex-shape intersection (GJK) and penetration depth (EPA) for point
// clouds. Used by the mesh-to-mesh collision system (see Vessel.cpp and
// "Mesh-to-mesh collision detection and response" in the Technical
// Reference).
//
// This module is self-contained: it depends only on Vecmat.h (Vector) and
// the VECTOR3 type from the SDK, and holds no global state, so it can be
// unit-tested without a running simulation (Tests/Collision.GjkEpa.cpp).
//
// All shapes are given as point clouds; the shape is the convex hull of the
// points. Points of shape A and shape B must be expressed in the same
// coordinate frame.
// =======================================================================

#ifndef GJKEPA_H
#define GJKEPA_H

#include <cstddef>
#include <vector>
#include "Vecmat.h"
#include "OrbiterAPI.h"

namespace GjkEpa {

// Maximum number of iterations of the GJK loop and of the EPA loop. When a
// loop reaches this limit without converging, the query reports "no result"
// (Intersect: false; Penetration: false).
const int MaxIterations = 64;

// EPA convergence threshold [length units of the point clouds, i.e. m].
// The returned depth is accurate to within this value; for shapes smaller
// than a few times this value the result is correspondingly coarse.
const double EpaTolerance = 0.01;

// A vertex of the Minkowski difference A - B, together with the points on A
// and B that generated it.
struct Support {
	Vector pos; // Minkowski difference point (pA - pB)
	Vector pA;  // point on shape A
	Vector pB;  // point on shape B
};

// Simplex produced by Intersect and consumed by Penetration.
typedef std::vector<Support> Simplex;

// Test whether the convex hulls of two point clouds intersect.
//   ptsA, nA   points of shape A
//   ptsB, nB   points of shape B
//   simplex    [out] on a true result, a tetrahedron (4 vertices) enclosing
//              the origin of the Minkowski difference, as required by
//              Penetration. Any previous content is discarded.
// Returns true if the hulls intersect, including configurations where the
// shapes are symmetric or aligned so that the origin of the Minkowski
// difference lies on a simplex edge or face. Returns false if they are
// separated (or merely touching), if either cloud is empty, or if the
// iteration limit was reached.
bool Intersect (const VECTOR3 *ptsA, size_t nA, const VECTOR3 *ptsB, size_t nB,
	Simplex &simplex);

// Compute penetration depth and contact for two intersecting hulls.
//   simplex    the simplex returned by a successful Intersect on the same
//              point clouds
//   normal     [out] unit normal of the minimum-translation direction,
//              pointing from A towards B (translating A by -normal*depth
//              separates the shapes)
//   depth      [out] penetration depth, accurate to EpaTolerance
//   contactPt  [out] contact point on shape A
// Returns true on success; normal is then always a finite unit vector.
// Returns false if the iteration limit was reached or the polytope degenerated
// (no face with a defined normal is left).
bool Penetration (const VECTOR3 *ptsA, size_t nA, const VECTOR3 *ptsB, size_t nB,
	Simplex &simplex, Vector &normal, double &depth, Vector &contactPt);

} // namespace GjkEpa

#endif // !GJKEPA_H
