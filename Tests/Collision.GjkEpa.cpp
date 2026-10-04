// Unit tests for the GJK/EPA convex collision module (Src/Orbiter/GjkEpa.*)
//
// Ground truth for box-vs-box queries comes from an independent separating
// axis test (SAT) implemented below, so the tests do not merely restate the
// algorithm under test.

#include <algorithm>
#include <array>
#include <cmath>
#include <random>
#include <vector>

#include "GjkEpa.h"

// these collide with std::min/max and Windows SDK macros
#undef min
#undef max
#undef small
#undef far
#undef near

#define CATCH_CONFIG_MAIN  // This tells Catch to provide a main() - only do this in one cpp file
#include "catch2/catch_all.hpp"

namespace {

typedef std::array<double, 3> V3;

V3 Add (const V3 &a, const V3 &b) { return { a[0]+b[0], a[1]+b[1], a[2]+b[2] }; }
V3 Sub (const V3 &a, const V3 &b) { return { a[0]-b[0], a[1]-b[1], a[2]-b[2] }; }
V3 Mul (const V3 &a, double s)    { return { a[0]*s, a[1]*s, a[2]*s }; }
double Dot (const V3 &a, const V3 &b) { return a[0]*b[0] + a[1]*b[1] + a[2]*b[2]; }
V3 Cross (const V3 &a, const V3 &b)
{
	return { a[1]*b[2]-a[2]*b[1], a[2]*b[0]-a[0]*b[2], a[0]*b[1]-a[1]*b[0] };
}
double Len (const V3 &a) { return std::sqrt (Dot (a, a)); }

VECTOR3 ToVec3 (const V3 &a)
{
	VECTOR3 v;
	v.x = a[0]; v.y = a[1]; v.z = a[2];
	return v;
}

V3 ToV3 (const Vector &v) { return { v.x, v.y, v.z }; }

// Oriented box: centre, three orthonormal axes, half extents
struct Obb {
	V3 c;
	V3 axis[3];
	V3 h;
};

Obb AxisAlignedBox (const V3 &lo, const V3 &hi)
{
	Obb b;
	b.c = Mul (Add (lo, hi), 0.5);
	b.h = Mul (Sub (hi, lo), 0.5);
	b.axis[0] = { 1, 0, 0 };
	b.axis[1] = { 0, 1, 0 };
	b.axis[2] = { 0, 0, 1 };
	return b;
}

Obb Rotated (Obb b, const std::array<double, 4> &q) // q = (w, x, y, z), unit
{
	const double w = q[0], x = q[1], y = q[2], z = q[3];
	const V3 col0 = { 1-2*(y*y+z*z), 2*(x*y+w*z),   2*(x*z-w*y)   };
	const V3 col1 = { 2*(x*y-w*z),   1-2*(x*x+z*z), 2*(y*z+w*x)   };
	const V3 col2 = { 2*(x*z+w*y),   2*(y*z-w*x),   1-2*(x*x+y*y) };
	b.axis[0] = col0; b.axis[1] = col1; b.axis[2] = col2;
	return b;
}

std::vector<VECTOR3> Corners (const Obb &b)
{
	std::vector<VECTOR3> pts;
	for (int i = 0; i < 8; i++) {
		V3 p = b.c;
		for (int k = 0; k < 3; k++)
			p = Add (p, Mul (b.axis[k], (i & (1<<k) ? b.h[k] : -b.h[k])));
		pts.push_back (ToVec3 (p));
	}
	return pts;
}

Obb Translated (Obb b, const V3 &d)
{
	b.c = Add (b.c, d);
	return b;
}

// Separating axis test for two boxes.
// Returns true if the boxes overlap. In that case depth is the minimum
// translation distance, normal the unit direction from A towards B, and
// secondDepth the smallest overlap along any axis not parallel to normal
// (used to detect ambiguous cases where several axes nearly tie).
// If the boxes are separated, depth is the negative of the largest gap
// found on any candidate axis.
bool Sat (const Obb &A, const Obb &B, double &depth, V3 &normal, double &secondDepth)
{
	std::vector<V3> axes;
	for (int i = 0; i < 3; i++) axes.push_back (A.axis[i]);
	for (int i = 0; i < 3; i++) axes.push_back (B.axis[i]);
	for (int i = 0; i < 3; i++)
		for (int j = 0; j < 3; j++) {
			V3 c = Cross (A.axis[i], B.axis[j]);
			double l = Len (c);
			if (l > 1e-9) axes.push_back (Mul (c, 1.0/l));
		}

	const V3 d = Sub (B.c, A.c);
	std::vector<std::pair<double, V3> > overlaps;
	for (const V3 &L : axes) {
		double rA = 0, rB = 0;
		for (int k = 0; k < 3; k++) {
			rA += A.h[k] * std::fabs (Dot (A.axis[k], L));
			rB += B.h[k] * std::fabs (Dot (B.axis[k], L));
		}
		double ov = rA + rB - std::fabs (Dot (d, L));
		V3 n = (Dot (d, L) >= 0 ? L : Mul (L, -1.0));
		overlaps.push_back (std::make_pair (ov, n));
	}
	std::sort (overlaps.begin(), overlaps.end(),
		[](const std::pair<double, V3> &a, const std::pair<double, V3> &b) { return a.first < b.first; });

	depth = overlaps[0].first;
	normal = overlaps[0].second;
	secondDepth = 1e30;
	for (size_t i = 1; i < overlaps.size(); i++)
		if (std::fabs (Dot (overlaps[i].second, normal)) < 0.99) {
			secondDepth = overlaps[i].first;
			break;
		}
	return depth > 0;
}

bool Intersect (const std::vector<VECTOR3> &a, const std::vector<VECTOR3> &b, GjkEpa::Simplex &s)
{
	return GjkEpa::Intersect (a.data(), a.size(), b.data(), b.size(), s);
}

struct Pen {
	bool ok;
	double depth;
	V3 normal;
	V3 contact;
};

// Full query: GJK then EPA. hit is false if GJK reports no intersection.
bool Query (const std::vector<VECTOR3> &a, const std::vector<VECTOR3> &b, Pen &out)
{
	GjkEpa::Simplex s;
	if (!Intersect (a, b, s)) return false;
	Vector n, c;
	double d = 0;
	out.ok = GjkEpa::Penetration (a.data(), a.size(), b.data(), b.size(), s, n, d, c);
	out.depth = d;
	out.normal = ToV3 (n);
	out.contact = ToV3 (c);
	return true;
}

bool Finite (const V3 &v) { return std::isfinite (v[0]) && std::isfinite (v[1]) && std::isfinite (v[2]); }

} // namespace

// =========================================================================

TEST_CASE ("GJK: separated and overlapping axis-aligned boxes", "[collision][gjk]")
{
	const auto A = Corners (AxisAlignedBox ({0,0,0}, {1,1,1}));
	GjkEpa::Simplex s;

	SECTION ("clearly separated") {
		REQUIRE_FALSE (Intersect (A, Corners (AxisAlignedBox ({3,0,0}, {4,1,1})), s));
		REQUIRE_FALSE (Intersect (A, Corners (AxisAlignedBox ({0,-5,0}, {1,-4,1})), s));
		REQUIRE_FALSE (Intersect (A, Corners (AxisAlignedBox ({0,0,2}, {1,1,3})), s));
	}
	SECTION ("clearly overlapping") {
		REQUIRE (Intersect (A, Corners (AxisAlignedBox ({0.5,0.4,0.3}, {1.5,1.6,1.2})), s));
		REQUIRE (s.size() == 4); // tetrahedron, as Penetration requires
	}
	SECTION ("one box inside the other") {
		const auto inner = Corners (AxisAlignedBox ({0.35,0.4,0.55}, {0.6,0.7,0.8})); // not concentric with A
		REQUIRE (Intersect (A, inner, s));
		REQUIRE (Intersect (inner, A, s));
	}
}

TEST_CASE ("GJK: degenerate input", "[collision][gjk]")
{
	const auto A = Corners (AxisAlignedBox ({0,0,0}, {1,1,1}));
	const std::vector<VECTOR3> none;
	GjkEpa::Simplex s;

	REQUIRE_FALSE (Intersect (none, A, s));
	REQUIRE_FALSE (Intersect (A, none, s));
	REQUIRE_FALSE (Intersect (none, none, s));

	SECTION ("single point inside / outside a box") {
		std::vector<VECTOR3> in  = { ToVec3 ({0.3, 0.6, 0.7}) }; // off-centre; see the degenerate tests for the centre
		std::vector<VECTOR3> out = { ToVec3 ({2.0, 0.5, 0.5}) };
		REQUIRE (Intersect (in, A, s));
		REQUIRE_FALSE (Intersect (out, A, s));
	}
}

TEST_CASE ("GJK: the simplex argument is reset on entry", "[collision][gjk]")
{
	const auto A = Corners (AxisAlignedBox ({0,0,0}, {1,1,1}));
	const auto B = Corners (AxisAlignedBox ({0.5,0.4,0.3}, {1.5,1.6,1.2}));
	const auto C = Corners (AxisAlignedBox ({5,5,5}, {6,6,6}));

	GjkEpa::Simplex fresh, reused;
	REQUIRE (Intersect (A, B, fresh));

	// leave junk from an unrelated query in the simplex, then query again
	REQUIRE (Intersect (A, B, reused));
	REQUIRE_FALSE (Intersect (A, C, reused));
	REQUIRE (Intersect (A, B, reused));

	REQUIRE (fresh.size() == reused.size());
	for (size_t i = 0; i < fresh.size(); i++)
		REQUIRE (Len (Sub (ToV3 (fresh[i].pos), ToV3 (reused[i].pos))) == 0.0);
}

TEST_CASE ("EPA: offset boxes match the analytic minimum translation", "[collision][epa]")
{
	struct Case { V3 loB, hiB; double depth; V3 normal; };
	// A is the unit cube; B overlaps it from various sides with unequal
	// extents, so the configuration is generic. The minimum translation is
	// along the axis with the least overlap, pointing A -> B.
	const Case cases[] = {
		{ { 0.8,  0.1,  0.2 }, { 1.8,  1.3,  0.9 }, 0.2,  {  1, 0, 0 } },
		{ {-0.7,  0.2, -0.1 }, { 0.3,  1.2,  1.4 }, 0.3,  { -1, 0, 0 } },
		{ { 0.1,  0.9,  0.3 }, { 1.3,  1.9,  0.8 }, 0.1,  {  0, 1, 0 } },
		{ {-0.2, -0.6,  0.1 }, { 0.7,  0.4,  1.2 }, 0.4,  {  0,-1, 0 } },
		{ { 0.2,  0.1,  0.75}, { 0.9,  1.3,  1.75}, 0.25, {  0, 0, 1 } },
		{ { 0.1, -0.1, -0.55}, { 1.2,  0.8,  0.45}, 0.45, {  0, 0,-1 } },
	};
	const auto A = Corners (AxisAlignedBox ({0,0,0}, {1,1,1}));
	for (const Case &c : cases) {
		Pen p;
		REQUIRE (Query (A, Corners (AxisAlignedBox (c.loB, c.hiB)), p));
		REQUIRE (p.ok);
		INFO ("expected depth " << c.depth << " got " << p.depth);
		REQUIRE (p.depth <= c.depth + 1e-9);                      // EPA never over-estimates
		REQUIRE (c.depth - p.depth <= GjkEpa::EpaTolerance + 1e-9); // and is within tolerance
		REQUIRE (Len (p.normal) == Catch::Approx (1.0).margin (1e-9));
		REQUIRE (Dot (p.normal, c.normal) > 0.999);
	}
}

TEST_CASE ("EPA: normal points from A towards B and flips when the arguments swap", "[collision][epa]")
{
	const auto A = Corners (AxisAlignedBox ({0,0,0}, {1,1,1}));
	const auto B = Corners (AxisAlignedBox ({0.8,0.15,0.3}, {1.7,0.9,0.75}));
	Pen ab, ba;
	REQUIRE (Query (A, B, ab));
	REQUIRE (Query (B, A, ba));
	REQUIRE (ab.ok);
	REQUIRE (ba.ok);
	REQUIRE (ab.normal[0] > 0.99);
	REQUIRE (ba.normal[0] < -0.99);
	REQUIRE (std::fabs (ab.depth - ba.depth) <= GjkEpa::EpaTolerance);
}

TEST_CASE ("EPA: contact point lies on shape A", "[collision][epa]")
{
	const Obb a = AxisAlignedBox ({0,0,0}, {1,1,1});
	const auto A = Corners (a);
	const auto B = Corners (AxisAlignedBox ({0.8,0.2,0.3}, {1.8,0.75,0.8}));
	Pen p;
	REQUIRE (Query (A, B, p));
	REQUIRE (p.ok);
	const double eps = 1e-9;
	for (int k = 0; k < 3; k++) {
		REQUIRE (p.contact[k] >= -eps);
		REQUIRE (p.contact[k] <= 1.0 + eps);
	}
}

TEST_CASE ("EPA: containment depth is the distance to the nearest face", "[collision][epa]")
{
	// small box inside a large box, off-centre towards +x
	const Obb smallBox = AxisAlignedBox ({3.5,-0.4,-0.3}, {4.5,0.5,0.6});
	const Obb big   = AxisAlignedBox ({-5,-5,-5}, {5,5,5});
	Pen p;
	REQUIRE (Query (Corners (smallBox), Corners (big), p));
	REQUIRE (p.ok);
	// the separating translation is the smallest one found by the SAT
	double depth, second; V3 n;
	REQUIRE (Sat (smallBox, big, depth, n, second));
	REQUIRE (p.depth <= depth + 1e-9);
	REQUIRE (depth - p.depth <= GjkEpa::EpaTolerance + 1e-9);
}

TEST_CASE ("EPA: random oriented boxes agree with the separating axis test", "[collision][epa][random]")
{
	std::mt19937 rng (12345);
	std::uniform_real_distribution<double> uh (0.5, 3.0), uc (-3.0, 3.0);
	std::normal_distribution<double> gauss (0.0, 1.0);

	auto randomBox = [&]() {
		Obb b = AxisAlignedBox ({-1,-1,-1}, {1,1,1});
		b.h = { uh(rng), uh(rng), uh(rng) };
		std::array<double, 4> q = { gauss(rng), gauss(rng), gauss(rng), gauss(rng) };
		double l = std::sqrt (q[0]*q[0]+q[1]*q[1]+q[2]*q[2]+q[3]*q[3]);
		for (double &x : q) x /= l;
		b = Rotated (b, q);
		b.c = { uc(rng), uc(rng), uc(rng) };
		return b;
	};

	int nOverlap = 0, nSeparated = 0, nSkipped = 0, nNormalChecked = 0;
	for (int iter = 0; iter < 400; iter++) {
		const Obb a = randomBox(), b = randomBox();
		double depth, second; V3 n;
		const bool sat = Sat (a, b, depth, n, second);

		// skip near-touching pairs: GJK/EPA tolerances make the verdict undefined there
		if (std::fabs (depth) < 0.02) { nSkipped++; continue; }

		const auto A = Corners (a), B = Corners (b);
		GjkEpa::Simplex s;
		const bool gjk = Intersect (A, B, s);
		INFO ("iteration " << iter << " SAT depth " << depth);
		REQUIRE (gjk == sat);

		if (!sat) { nSeparated++; continue; }
		nOverlap++;

		Vector pn, pc; double pd = 0;
		REQUIRE (GjkEpa::Penetration (A.data(), A.size(), B.data(), B.size(), s, pn, pd, pc));
		REQUIRE (std::isfinite (pd));
		REQUIRE (Finite (ToV3 (pn)));
		REQUIRE (Len (ToV3 (pn)) == Catch::Approx (1.0).margin (1e-6));
		REQUIRE (pd <= depth + 1e-9);
		REQUIRE (depth - pd <= GjkEpa::EpaTolerance + 1e-9);
		if (second - depth > 0.1) { // unambiguous minimum axis
			nNormalChecked++;
			REQUIRE (Dot (ToV3 (pn), n) > 0.9);
		}
	}
	// make sure the random set exercised both outcomes and the normal check
	REQUIRE (nOverlap > 50);
	REQUIRE (nSeparated > 50);
	REQUIRE (nNormalChecked > 20);
}

TEST_CASE ("Triangle against a convex hull (base collision usage)", "[collision][epa]")
{
	const auto box = Corners (AxisAlignedBox ({0,0,0}, {1,1,1}));

	SECTION ("triangle poking through the +x face") {
		std::vector<VECTOR3> tri = { ToVec3 ({0.9, 0.2, 0.2}), ToVec3 ({1.3, 0.2, 0.8}), ToVec3 ({1.3, 0.8, 0.2}) };
		Pen p;
		REQUIRE (Query (box, tri, p));
		REQUIRE (p.ok);
		REQUIRE (p.depth > 0.0);
		REQUIRE (p.depth <= 0.1 + 1e-9);
		REQUIRE (p.normal[0] > 0.9);
	}
	SECTION ("triangle clear of the hull") {
		std::vector<VECTOR3> tri = { ToVec3 ({1.5, 0.2, 0.2}), ToVec3 ({1.9, 0.2, 0.8}), ToVec3 ({1.9, 0.8, 0.2}) };
		GjkEpa::Simplex s;
		REQUIRE_FALSE (Intersect (box, tri, s));
	}
}

TEST_CASE ("Results are translation-invariant at planetary distances", "[collision][epa]")
{
	// hull points are stored in planet-local coordinates, i.e. millions of metres from the origin
	const Obb a = AxisAlignedBox ({0,0,0}, {4,2,6});
	const Obb b = AxisAlignedBox ({3.5,0.5,1}, {7.2,2.7,4.8});
	Pen near0;
	REQUIRE (Query (Corners (a), Corners (b), near0));
	REQUIRE (near0.ok);

	const V3 farOffset = { 1.7374e6, -3.2e5, 6.378e6 }; // ~ lunar / terrestrial radius scale
	Pen farP;
	REQUIRE (Query (Corners (Translated (a, farOffset)), Corners (Translated (b, farOffset)), farP));
	REQUIRE (farP.ok);
	REQUIRE (std::fabs (farP.depth - near0.depth) <= GjkEpa::EpaTolerance);
	REQUIRE (Dot (farP.normal, near0.normal) > 0.99);
}

TEST_CASE ("Touching hulls never yield non-finite results", "[collision][epa][robustness]")
{
	// Exactly touching shapes are on the boundary of GJK's decision. Either
	// verdict is acceptable; a "hit" must not return NaN/inf or a zero normal.
	const auto A = Corners (AxisAlignedBox ({0,0,0}, {1,1,1}));
	const V3 offsets[] = { {1,0,0}, {0,1,0}, {0,0,1}, {1,1,0}, {1,1,1}, {-1,0,0} };
	for (const V3 &o : offsets) {
		const auto B = Corners (AxisAlignedBox (o, Add (o, {1,1,1})));
		Pen p;
		if (Query (A, B, p)) {
			INFO ("touching offset " << o[0] << "," << o[1] << "," << o[2]);
			if (p.ok) {
				REQUIRE (std::isfinite (p.depth));
				REQUIRE (Finite (p.normal));
				REQUIRE (Finite (p.contact));
				REQUIRE (Len (p.normal) == Catch::Approx (1.0).margin (1e-6));
			}
		}
	}
}

// =========================================================================
// Degenerate configurations: the origin of the Minkowski difference lies on a
// feature (edge, face) of the intermediate simplex or polytope. This happens
// for symmetric and axis-aligned shapes, which are common (station modules,
// aligned vessels, landing pads).
// === BEGIN DEGENERATE ===

TEST_CASE ("GJK: coincident and centred shapes intersect", "[collision][gjk][degenerate]")
{
	const auto A = Corners (AxisAlignedBox ({0,0,0}, {1,1,1}));
	GjkEpa::Simplex s;
	REQUIRE (Intersect (A, A, s));

	std::vector<VECTOR3> centre = { ToVec3 ({0.5, 0.5, 0.5}) };
	REQUIRE (Intersect (centre, A, s));
	REQUIRE (Intersect (A, centre, s));
}

TEST_CASE ("EPA: aligned boxes of equal cross-section", "[collision][epa][degenerate]")
{
	struct Case { V3 loB, hiB; double depth; V3 normal; };
	const Case cases[] = {
		{ { 0.8, 0.0, 0.0 }, { 1.8, 1.0, 1.0 }, 0.2,  {  1, 0, 0 } },
		{ {-0.7, 0.0, 0.0 }, { 0.3, 1.0, 1.0 }, 0.3,  { -1, 0, 0 } },
		{ { 0.0, 0.9, 0.0 }, { 1.0, 1.9, 1.0 }, 0.1,  {  0, 1, 0 } },
		{ { 0.0,-0.6, 0.0 }, { 1.0, 0.4, 1.0 }, 0.4,  {  0,-1, 0 } },
		{ { 0.0, 0.0, 0.75}, { 1.0, 1.0, 1.75}, 0.25, {  0, 0, 1 } },
		{ { 0.0, 0.0,-0.55}, { 1.0, 1.0, 0.45}, 0.45, {  0, 0,-1 } },
		{ { 0.8, 0.1, 0.1 }, { 1.8, 1.1, 1.1 }, 0.2,  {  1, 0, 0 } },
	};
	const auto A = Corners (AxisAlignedBox ({0,0,0}, {1,1,1}));
	for (const Case &c : cases) {
		Pen p;
		INFO ("B = [" << c.loB[0] << "," << c.loB[1] << "," << c.loB[2] << "]");
		REQUIRE (Query (A, Corners (AxisAlignedBox (c.loB, c.hiB)), p));
		REQUIRE (p.ok);
		REQUIRE (p.depth <= c.depth + 1e-9);
		REQUIRE (c.depth - p.depth <= GjkEpa::EpaTolerance + 1e-9);
		REQUIRE (Len (p.normal) == Catch::Approx (1.0).margin (1e-9));
		REQUIRE (Dot (p.normal, c.normal) > 0.999);
	}
}

// Exhaustive check over every pair of boxes with corners on a coarse integer
// grid. Such pairs share faces, edges and symmetry planes, i.e. they hit the
// degenerate paths of both GJK and EPA constantly. Ground truth is exact.
TEST_CASE ("Grid of aligned box pairs agrees with the analytic answer", "[collision][epa][degenerate]")
{
	std::vector<Obb> boxes;
	for (int x0 = 0; x0 < 4; x0++) for (int x1 = x0+1; x1 < 4; x1++)
	for (int y0 = 0; y0 < 4; y0++) for (int y1 = y0+1; y1 < 4; y1++)
	for (int z0 = 0; z0 < 4; z0++) for (int z1 = z0+1; z1 < 4; z1++)
		boxes.push_back (AxisAlignedBox ({double(x0), double(y0), double(z0)}, {double(x1), double(y1), double(z1)}));

	int nOverlap = 0, nSeparated = 0, nTouching = 0;
	for (const Obb &a : boxes) {
		const auto A = Corners (a);
		for (const Obb &b : boxes) {
			// analytic AABB overlap on each axis (all values are exact in binary)
			bool separated = false, touching = false;
			double minOv = 1e30;
			double cand[6]; V3 candN[6];
			for (int ax = 0; ax < 3; ax++) {
				double lo = std::max (a.c[ax]-a.h[ax], b.c[ax]-b.h[ax]);
				double hi = std::min (a.c[ax]+a.h[ax], b.c[ax]+b.h[ax]);
				if (hi < lo) separated = true;
				if (hi == lo) touching = true;
				V3 up = {0,0,0}; up[ax] = 1;
				cand[2*ax]   = (a.c[ax]+a.h[ax]) - (b.c[ax]-b.h[ax]); candN[2*ax]   = up;           // A moves -axis
				cand[2*ax+1] = (b.c[ax]+b.h[ax]) - (a.c[ax]-a.h[ax]); candN[2*ax+1] = Mul (up, -1.0); // A moves +axis
			}
			if (separated)  { nSeparated++; }
			else if (touching) { nTouching++; continue; } // boundary case: either verdict is fine
			else nOverlap++;
			for (double v : cand) minOv = std::min (minOv, v);

			const auto B = Corners (b);
			GjkEpa::Simplex s;
			const bool hit = Intersect (A, B, s);
			REQUIRE (hit == !separated);
			if (separated) continue;

			Vector pn, pc; double pd = 0;
			REQUIRE (GjkEpa::Penetration (A.data(), A.size(), B.data(), B.size(), s, pn, pd, pc));
			const V3 n = ToV3 (pn);
			REQUIRE (Finite (n));
			REQUIRE (Len (n) == Catch::Approx (1.0).margin (1e-6));
			REQUIRE (pd <= minOv + 1e-9);
			REQUIRE (minOv - pd <= GjkEpa::EpaTolerance + 1e-9);
			// the normal must be one of the (near-)minimal translation directions
			double best = -1;
			for (int i = 0; i < 6; i++)
				if (cand[i] <= minOv + 0.05) best = std::max (best, Dot (n, candN[i]));
			REQUIRE (best >= 0.99);
		}
	}
	// the grid must actually have exercised all three outcomes (216 boxes, 46656 pairs)
	REQUIRE (nOverlap == 17576);
	REQUIRE (nSeparated == 7352);
	REQUIRE (nTouching == 21728);
}

// === END DEGENERATE ===
