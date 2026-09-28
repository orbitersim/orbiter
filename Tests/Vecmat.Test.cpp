// not upstream: unit tests for Src/Orbiter/Vecmat
#include <catch2/catch_test_macros.hpp>
#include <catch2/matchers/catch_matchers_floating_point.hpp>
#include "Vecmat.h"

using Catch::Matchers::WithinAbs;

static void RequireVec (const Vector &a, const Vector &b, double eps = 1e-12)
{
	REQUIRE_THAT(a.x, WithinAbs(b.x, eps));
	REQUIRE_THAT(a.y, WithinAbs(b.y, eps));
	REQUIRE_THAT(a.z, WithinAbs(b.z, eps));
}

TEST_CASE("Vector arithmetic", "[vecmat]")
{
	Vector a(1, 2, 3), b(4, 5, 6);
	RequireVec(a + b, Vector(5, 7, 9));
	REQUIRE((a & b) == 32.0);
	RequireVec(crossp(a, b), Vector(-3, 6, -3));
	REQUIRE_THAT(Vector(3, 4, 0).length(), WithinAbs(5.0, 1e-15));
	RequireVec(Vector(0, 0, 2).unit(), Vector(0, 0, 1));
	REQUIRE_THAT(xangle(Vector(1, 0, 0), Vector(0, 1, 0)), WithinAbs(Pi05, 1e-12));
}

TEST_CASE("Matrix inverse, transpose, mul/tmul", "[vecmat]")
{
	Matrix R;
	R.Set(Vector(0.3, -0.7, 1.1));
	Matrix I = R * inv(R);
	for (int i = 0; i < 3; i++)
		for (int j = 0; j < 3; j++)
			REQUIRE_THAT(I(i, j), WithinAbs(i == j ? 1.0 : 0.0, 1e-12));
	Vector p(1, -2, 0.5);
	RequireVec(tmul(R, mul(R, p)), p);
	RequireVec(mul(transp(R), p), tmul(R, p));
}

TEST_CASE("Quaternion round trip and rotation", "[vecmat]")
{
	Matrix R;
	R.Set(Vector(0.2, 0.4, -0.9));
	Quaternion q(R);
	Matrix R2;
	R2.Set(q);
	for (int i = 0; i < 9; i++)
		REQUIRE_THAT(R2.data[i], WithinAbs(R.data[i], 1e-12));
	Vector p(0.3, 2.0, -1.0);
	RequireVec(mul(q, p), mul(R, p));
	RequireVec(tmul(q, p), tmul(R, p));
	REQUIRE_THAT(q.norm(), WithinAbs(1.0, 1e-12));
}

TEST_CASE("QR solve 3x3 and 4x4", "[vecmat]")
{
	Matrix A(4, 1, 2, 1, 5, 3, 2, 3, 6);
	Vector x(1, -2, 3), b = mul(A, x), c, d;
	int sing;
	qrdcmp(A, c, d, &sing);
	REQUIRE(sing == 0);
	qrsolv(A, c, d, b);
	RequireVec(b, x, 1e-10);

	Matrix4 M(4, 1, 0, 2,  1, 5, 1, 0,  0, 1, 6, 1,  2, 0, 1, 7);
	Vector4 x4(1, 2, -1, 0.5), b4, c4, d4, r4;
	for (int i = 0; i < 4; i++)
		for (int j = 0; j < 4; j++) b4(i) += M(i, j) * x4(j);
	Matrix4 Q(M);
	QRFactorize(Q, c4, d4);
	QRSolve(Q, c4, d4, b4, r4);
	for (int i = 0; i < 4; i++)
		REQUIRE_THAT(r4(i), WithinAbs(x4(i), 1e-10));
}

TEST_CASE("Plane helpers", "[vecmat]")
{
	double a, b, c, d;
	PlaneCoeffs(Vector(0, 0, 1), Vector(1, 0, 1), Vector(0, 1, 1), a, b, c, d);
	REQUIRE_THAT(fabs(PointPlaneDist(Vector(5, 5, 4), a, b, c, d)), WithinAbs(3.0, 1e-12));
	Vector r;
	REQUIRE(LinePlaneIntersect(a, b, c, d, Vector(2, 3, 0), Vector(0, 0, 1), r));
	RequireVec(r, Vector(2, 3, 1));
}
