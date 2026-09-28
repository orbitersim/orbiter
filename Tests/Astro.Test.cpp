// not upstream: unit tests for Src/Orbiter/Astro and TimeData
#include <catch2/catch_test_macros.hpp>
#include <catch2/matchers/catch_matchers_floating_point.hpp>
#include <cstring>
#include "Astro.h"
#include "TimeData.h"

using Catch::Matchers::WithinAbs;

TEST_CASE("MJD date conversions", "[astro]")
{
	struct tm *d = mjddate(MJD2000);  // Orbiter keeps tm_mon 1-based here
	REQUIRE(d->tm_year == 100);
	REQUIRE(d->tm_mon == 1);
	REQUIRE(d->tm_mday == 1);
	REQUIRE(d->tm_hour == 12);
	REQUIRE_THAT(date2mjd(d), WithinAbs(MJD2000, 1e-9));
	REQUIRE(std::strcmp(DateStr(MJD2000), "Sat Jan 01 12:00:00 2000") == 0);
	REQUIRE_THAT(MJD((time_t)0), WithinAbs(40587.0, 1e-12));
	REQUIRE_THAT(JC2MJD(MJD2JC(60000.25)), WithinAbs(60000.25, 1e-9));
}

TEST_CASE("Coordinate transformations", "[astro]")
{
	REQUIRE_THAT(Obliquity(0.0), WithinAbs(0.4090928042, 1e-12));
	double ob = Obliquity(0.2), l, b, ra, dc;
	Equ2Ecl(cos(ob), sin(ob), 1.2, -0.3, l, b);
	Ecl2Equ(cos(ob), sin(ob), l, b, ra, dc);
	REQUIRE_THAT(ra, WithinAbs(1.2, 1e-12));
	REQUIRE_THAT(dc, WithinAbs(-0.3, 1e-12));
	REQUIRE_THAT(Orthodome(0.0, 0.0, PI05, 0.0), WithinAbs(PI05, 1e-12));
	double dist, dir;
	Orthodome(0.0, 0.0, 0.0, 0.5, dist, dir);
	REQUIRE_THAT(dist, WithinAbs(0.5, 1e-12));
	REQUIRE_THAT(dir, WithinAbs(0.0, 1e-12));
}

TEST_CASE("Number formatting", "[astro]")
{
	REQUIRE(std::strcmp(FloatStr(12346.0), " 12.35k") == 0);
	REQUIRE(std::strcmp(DistStr(10.0*AU), " 10.00AU") == 0);
	REQUIRE(std::strcmp(SciStr(1.5e-7, 3), "1.5\xc2\xb7" "10^-7") == 0);
}

TEST_CASE("TimeData stepping and warp", "[timedata]")
{
	TimeData td;
	td.Reset(MJD2000);
	td.BeginStep(0.5, true);
	td.EndStep(true);
	REQUIRE_THAT(td.SimT0, WithinAbs(0.5, 1e-15));
	REQUIRE_THAT(td.MJD0, WithinAbs(MJD2000 + 0.5/86400.0, 1e-12));
	td.SetWarp(10.0);
	REQUIRE(td.WarpChanged());
	td.BeginStep(0.5, true);
	td.EndStep(true);
	REQUIRE_THAT(td.SimT0, WithinAbs(5.5, 1e-12));
	td.BeginStep(0.5, false);  // paused: sim time stands still
	td.EndStep(false);
	REQUIRE_THAT(td.SimT0, WithinAbs(5.5, 1e-12));
	REQUIRE_THAT(td.JumpTo(MJD2000 + 1.0), WithinAbs(86400.0 - 5.5, 1e-6));
	REQUIRE(td.FrameCount() == 3);
}
