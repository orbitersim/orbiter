////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2023 Dimitry Ishenko
// Contact: dimitry (dot) ishenko (at) (gee) mail (dot) com
//
// Distributed under the MIT license.

////////////////////////////////////////////////////////////////////////////////
#ifndef COLOUR_HPP
#define COLOUR_HPP

#include "math.hpp"
#include "vector.hpp"

#include <algorithm>
#include <cstdint> // std::uint32_t

////////////////////////////////////////////////////////////////////////////////
/**
 * @brief rgb color
 * @note r, g and b values are in range [0..1]
 */
struct COLOUR3 { float r, g, b; };

/**
 * @brief rgba color
 * @note r, g, b and a values are in range [0..1]
 */
struct COLOUR4 { float r, g, b, a; };

////////////////////////////////////////////////////////////////////////////////
/**
 * @brief construct from 32-bit uint in abgr format
 */
constexpr auto from_abgr32(std::uint32_t c)
{
	auto r = static_cast<float>(0xff & (c >>  0)) / 255;
	auto g = static_cast<float>(0xff & (c >>  8)) / 255;
	auto b = static_cast<float>(0xff & (c >> 16)) / 255;
	auto a = static_cast<float>(0xff & (c >> 24)) / 255;
	return COLOUR4{r, g, b, a};
}

/**
 * @brief get largest of the r, g and b values
 */
constexpr auto max_rgb(const COLOUR3& c) { return (std::max)({c.r, c.g, c.b}); }
constexpr auto max_rgb(const COLOUR4& c) { return (std::max)({c.r, c.g, c.b}); }

template<typename V, if_vector3(V)> constexpr auto max_rgb(const V& v) { return (std::max)({v.x, v.y, v.z}); }
template<typename V, if_vector4(V)> constexpr auto max_rgb(const V& v) { return (std::max)({v.x, v.y, v.z}); }

/**
 * @brief convert to 32-bit uint in abgr format
 */
constexpr auto to_abgr32(const COLOUR4& c)
{
	auto r = static_cast<std::uint32_t>(saturate(c.r) * 255 + 0.5f);
	auto g = static_cast<std::uint32_t>(saturate(c.g) * 255 + 0.5f);
	auto b = static_cast<std::uint32_t>(saturate(c.b) * 255 + 0.5f);
	auto a = static_cast<std::uint32_t>(saturate(c.a) * 255 + 0.5f);
	return a << 24 | b << 16 | g << 8 | r;
}

/**
 * @brief convert to 32-bit uint in abgr format
 */
constexpr auto to_argb32(const COLOUR4& c)
{
	auto r = static_cast<std::uint32_t>(saturate(c.r) * 255 + 0.5f);
	auto g = static_cast<std::uint32_t>(saturate(c.g) * 255 + 0.5f);
	auto b = static_cast<std::uint32_t>(saturate(c.b) * 255 + 0.5f);
	auto a = static_cast<std::uint32_t>(saturate(c.a) * 255 + 0.5f);
	return a << 24 | r << 16 | g << 8 | b;
}

/**
 * @brief convert vector to color
*/
template<typename V, if_vector3(V)> constexpr auto to_COLOUR3(const V& v) { return COLOUR3{v.x, v.y, v.z}; }
template<typename V, if_vector4(V)> constexpr auto to_COLOUR4(const V& v) { return COLOUR4{v.x, v.y, v.z, v.w}; }

////////////////////////////////////////////////////////////////////////////////
#endif
