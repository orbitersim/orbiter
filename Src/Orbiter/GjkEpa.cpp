// Copyright (c) EvESpirit
// Licensed under the MIT License

// =======================================================================
// GjkEpa.cpp
// GJK intersection test and EPA penetration depth for point clouds.
// See GjkEpa.h for the interface.
//
// The algorithms come from Vessel.cpp. Differences from that version:
//  * The iteration limit and EPA tolerance are named constants (same values).
//  * Intersect clears the simplex it is given.
//  * Degenerate configurations are handled. When the origin of the Minkowski
//    difference lies on an edge or face of the working simplex/polytope -
//    which is typical for symmetric and axis-aligned shapes - the original
//    code could report "no intersection" for shapes that overlap (GJK), or
//    return depth 0 and a zero or reversed normal (EPA). See EdgeSearchDirection
//    and Face below. Wherever the original code was correct the results are
//    bit-identical; they differ only where it was wrong.
// =======================================================================

#include "GjkEpa.h"
#include <cmath>

namespace GjkEpa {

namespace {

Vector GetFarthestPointInDirection(const VECTOR3* points, size_t count, const Vector& dir, int& bestIdx) {
	double maxDot = -1e30;
	bestIdx = 0;
	Vector bestP(0,0,0);
	for (size_t i = 0; i < count; ++i) {
		Vector p(points[i].x, points[i].y, points[i].z);
		double d = dotp(p, dir);
		if (d > maxDot) {
			maxDot = d;
			bestP = p;
			bestIdx = (int)i;
		}
	}
	return bestP;
}

Support GetMinkowskiSupport(const VECTOR3* ptsA, size_t countA, const VECTOR3* ptsB, size_t countB, const Vector& dir, int& idxA, int& idxB) {
	Support s;
	s.pA = GetFarthestPointInDirection(ptsA, countA, dir, idxA);
	s.pB = GetFarthestPointInDirection(ptsB, countB, -dir, idxB);
	s.pos = s.pA - s.pB;
	return s;
}

bool SameDirection(const Vector& direction, const Vector& ao) {
	return dotp(direction, ao) > 0;
}

// Search direction perpendicular to the edge ab, on the side of the origin
// (ao). This is (ab x ao) x ab, which vanishes when the origin lies on the
// line through the edge - e.g. for symmetric or axis-aligned shapes. Any
// perpendicular to ab is a valid search direction in that case; returning the
// zero vector instead would make the next support query useless and the
// caller report "no intersection".
Vector EdgeSearchDirection(const Vector& ab, const Vector& ao) {
	Vector d = crossp(crossp(ab, ao), ab);
	const double ab2 = ab.length2();
	// |d|^2 = |ab|^4 |ao|^2 sin^2(angle); treat angles below ~1e-6 rad as collinear
	if (d.length2() > 1e-12 * ab2 * ab2 * ao.length2()) return d;
	// pick the coordinate axis least aligned with ab and cross with it
	Vector axis(1, 0, 0);
	const double ax = fabs(ab.x), ay = fabs(ab.y), az = fabs(ab.z);
	if (ay <= ax && ay <= az) axis.Set(0, 1, 0);
	else if (az <= ax && az <= ay) axis.Set(0, 0, 1);
	return crossp(ab, axis);
}

bool Line(Simplex& simplex, Vector& direction) {
	Support a = simplex[1];
	Support b = simplex[0];
	Vector ab = b.pos - a.pos;
	Vector ao = -a.pos;
	if (SameDirection(ab, ao)) {
		direction = EdgeSearchDirection(ab, ao);
	} else {
		simplex.clear();
		simplex.push_back(a);
		direction = ao;
	}
	return false;
}

bool Triangle(Simplex& simplex, Vector& direction) {
	Support a = simplex[2];
	Support b = simplex[1];
	Support c = simplex[0];
	Vector ab = b.pos - a.pos;
	Vector ac = c.pos - a.pos;
	Vector ao = -a.pos;
	Vector abc = crossp(ab, ac);

	if (SameDirection(crossp(abc, ac), ao)) {
		if (SameDirection(ac, ao)) {
			simplex.clear();
			simplex.push_back(c);
			simplex.push_back(a);
			direction = EdgeSearchDirection(ac, ao);
		} else {
			simplex.clear();
			simplex.push_back(b);
			simplex.push_back(a);
			return Line(simplex, direction);
		}
	} else {
		if (SameDirection(crossp(ab, abc), ao)) {
			simplex.clear();
			simplex.push_back(b);
			simplex.push_back(a);
			return Line(simplex, direction);
		} else {
			if (SameDirection(abc, ao)) {
				direction = abc;
			} else {
				simplex.clear();
				simplex.push_back(b);
				simplex.push_back(c);
				simplex.push_back(a);
				direction = -abc;
			}
		}
	}
	return false;
}

bool Tetrahedron(Simplex& simplex, Vector& direction) {
	Support a = simplex[3];
	Support b = simplex[2];
	Support c = simplex[1];
	Support d = simplex[0];

	Vector ab = b.pos - a.pos;
	Vector ac = c.pos - a.pos;
	Vector ad = d.pos - a.pos;
	Vector ao = -a.pos;

	Vector abc = crossp(ab, ac);
	Vector acd = crossp(ac, ad);
	Vector adb = crossp(ad, ab);

	if (SameDirection(abc, ao)) {
		simplex.clear();
		simplex.push_back(c);
		simplex.push_back(b);
		simplex.push_back(a);
		return Triangle(simplex, direction);
	}

	if (SameDirection(acd, ao)) {
		simplex.clear();
		simplex.push_back(d);
		simplex.push_back(c);
		simplex.push_back(a);
		return Triangle(simplex, direction);
	}

	if (SameDirection(adb, ao)) {
		simplex.clear();
		simplex.push_back(b);
		simplex.push_back(d);
		simplex.push_back(a);
		return Triangle(simplex, direction);
	}

	return true;
}

bool NextSimplex(Simplex& simplex, Vector& direction) {
	switch (simplex.size()) {
		case 2: return Line(simplex, direction);
		case 3: return Triangle(simplex, direction);
		case 4: return Tetrahedron(simplex, direction);
	}
	return false;
}

struct Face {
	Support a, b, c;
	Vector normal;
	double distance;
	bool valid; // false for a zero-area face, which has no defined normal
	// inside: a point strictly inside the polytope (centroid of the initial
	// tetrahedron). Orienting by it, rather than by the origin, stays correct
	// when the origin lies on a face of the polytope.
	Face(const Support& _a, const Support& _b, const Support& _c, const Vector& inside) : a(_a), b(_b), c(_c) {
		normal = crossp(b.pos - a.pos, c.pos - a.pos);
		double len = normal.length();
		valid = (len > 1e-8);
		if (valid) normal /= len;
		if (dotp(normal, a.pos - inside) < 0) {
			normal = -normal;
			Support tmp = b; b = c; c = tmp;
		}
		distance = dotp(normal, a.pos);
		if (distance < 0) distance = 0; // origin marginally outside this face (rounding)
	}
};

struct Edge {
	Support a, b;
	Edge(const Support& _a, const Support& _b) : a(_a), b(_b) {}
	bool operator==(const Edge& o) const {
		return (a.pos.dist2(o.a.pos) < 1e-6 && b.pos.dist2(o.b.pos) < 1e-6) ||
		       (a.pos.dist2(o.b.pos) < 1e-6 && b.pos.dist2(o.a.pos) < 1e-6);
	}
};

} // anonymous namespace

// -----------------------------------------------------------------------

bool Intersect(const VECTOR3* ptsA, size_t countA, const VECTOR3* ptsB, size_t countB, Simplex& simplex) {
	simplex.clear();
	if (countA == 0 || countB == 0) return false;
	Vector direction(1, 0, 0);
	int idA, idB;
	Support support = GetMinkowskiSupport(ptsA, countA, ptsB, countB, direction, idA, idB);
	simplex.push_back(support);
	direction = -support.pos;

	for (int i = 0; i < MaxIterations; i++) {
		support = GetMinkowskiSupport(ptsA, countA, ptsB, countB, direction, idA, idB);
		if (dotp(support.pos, direction) <= 0) {
			return false; // No intersection
		}
		simplex.push_back(support);
		if (NextSimplex(simplex, direction)) {
			return true; // Intersection
		}
	}
	return false;
}

// -----------------------------------------------------------------------

bool Penetration(const VECTOR3* ptsA, size_t countA, const VECTOR3* ptsB, size_t countB, Simplex& simplex, Vector& normal, double& depth, Vector& contactPtGlobal) {
	const Vector inside = (simplex[0].pos + simplex[1].pos + simplex[2].pos + simplex[3].pos) * 0.25;
	std::vector<Face> faces;
	faces.push_back(Face(simplex[0], simplex[1], simplex[2], inside));
	faces.push_back(Face(simplex[0], simplex[2], simplex[3], inside));
	faces.push_back(Face(simplex[0], simplex[3], simplex[1], inside));
	faces.push_back(Face(simplex[1], simplex[3], simplex[2], inside));

	int idA, idB;
	for (int iter = 0; iter < MaxIterations; iter++) {
		double minDist = 1e30;
		int closestFaceIdx = -1;
		for (size_t i = 0; i < faces.size(); i++) {
			if (faces[i].valid && faces[i].distance < minDist) {
				minDist = faces[i].distance;
				closestFaceIdx = (int)i;
			}
		}
		if (closestFaceIdx == -1) return false;

		Face closestFace = faces[closestFaceIdx];
		Support support = GetMinkowskiSupport(ptsA, countA, ptsB, countB, closestFace.normal, idA, idB);
		double dist = dotp(closestFace.normal, support.pos);

		if (dist - closestFace.distance < EpaTolerance) {
			normal = closestFace.normal;
			depth = closestFace.distance;
			// Compute contact point on shape A via barycentric coordinates
			Vector n = closestFace.normal;
			Vector p0 = closestFace.a.pos, p1 = closestFace.b.pos, p2 = closestFace.c.pos;
			Vector p = n * depth;
			Vector e0 = p1 - p0, e1 = p2 - p0, e2 = p - p0;
			double d00 = dotp(e0, e0), d01 = dotp(e0, e1), d11 = dotp(e1, e1), d20 = dotp(e2, e0), d21 = dotp(e2, e1);
			double denom = d00 * d11 - d01 * d01;
			if (fabs(denom) < 1e-12) {
				contactPtGlobal = closestFace.a.pA;
				return true;
			}
			double v = (d11 * d20 - d01 * d21) / denom;
			double w = (d00 * d21 - d01 * d20) / denom;
			double u = 1.0 - v - w;
			contactPtGlobal = closestFace.a.pA * u + closestFace.b.pA * v + closestFace.c.pA * w;
			return true;
		}

		std::vector<Edge> uniqueEdges;
		for (size_t i = 0; i < faces.size(); i++) {
			// a zero-area face has no normal and is always replaced by the new cap
			if (!faces[i].valid || dotp(faces[i].normal, support.pos - faces[i].a.pos) > 0) {
				Edge edges[3] = { Edge(faces[i].a, faces[i].b), Edge(faces[i].b, faces[i].c), Edge(faces[i].c, faces[i].a) };
				for (int e = 0; e < 3; e++) {
					bool unique = true;
					for (size_t j = 0; j < uniqueEdges.size(); j++) {
						if (uniqueEdges[j] == edges[e]) {
							uniqueEdges.erase(uniqueEdges.begin() + j);
							unique = false;
							break;
						}
					}
					if (unique) uniqueEdges.push_back(edges[e]);
				}
				faces.erase(faces.begin() + i);
				i--;
			}
		}

		for (size_t i = 0; i < uniqueEdges.size(); i++) {
			faces.push_back(Face(uniqueEdges[i].a, uniqueEdges[i].b, support, inside));
		}
	}
	return false;
}

} // namespace GjkEpa
