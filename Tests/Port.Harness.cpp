// not upstream: smoke test for the unit-test harness
#include <catch2/catch_test_macros.hpp>
#include <cstdint>

TEST_CASE("Harness runs", "[harness]")
{
	REQUIRE(sizeof(void*) == 8);
	REQUIRE(static_cast<std::uint32_t>(0xFFFFFFFFu) + 1u == 0u);
}
