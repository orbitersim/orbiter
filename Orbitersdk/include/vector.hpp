////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2023 Dimitry Ishenko
// Contact: dimitry (dot) ishenko (at) (gee) mail (dot) com
//
// Distributed under the MIT license.

////////////////////////////////////////////////////////////////////////////////
#ifndef VECTOR_HPP
#define VECTOR_HPP

#include <cmath>
#include <type_traits>

////////////////////////////////////////////////////////////////////////////////
/**
 * @brief type traits for 2-, 3- and 4-dimensional vectors
 *
 * Classes belonging to is_vector2 must have x and y member variables.
 * Classes belonging to is_vector3 must have x, y and z member variables.
 * Classes belonging to is_vector4 must have x, y, z and w member variables.
 */
template<typename> struct is_vector2 : std::false_type { };
template<typename> struct is_vector3 : std::false_type { };
template<typename> struct is_vector4 : std::false_type { };

////////////////////////////////////////////////////////////////////////////////
/**
 * @brief helper macros for vector type traits
 */
#define if_vector2(V) std::enable_if_t<is_vector2<V>::value>* = nullptr
#define if_vector3(V) std::enable_if_t<is_vector3<V>::value>* = nullptr
#define if_vector4(V) std::enable_if_t<is_vector4<V>::value>* = nullptr
#define if_vector( V) std::enable_if_t<is_vector2<V>::value || is_vector3<V>::value || is_vector4<V>::value>* = nullptr

/**
 * @brief vector operators
 */
template<typename V, if_vector2(V)> constexpr auto& operator+=(V& l, const V& r) { l.x += r.x; l.y += r.y; return l; }
template<typename V, if_vector3(V)> constexpr auto& operator+=(V& l, const V& r) { l.x += r.x; l.y += r.y; l.z += r.z; return l; }
template<typename V, if_vector4(V)> constexpr auto& operator+=(V& l, const V& r) { l.x += r.x; l.y += r.y; l.z += r.z; l.w += r.w; return l; }

template<typename V, if_vector2(V)> constexpr auto& operator+=(V& v, auto q) { v.x += q; v.y += q; return v; }
template<typename V, if_vector3(V)> constexpr auto& operator+=(V& v, auto q) { v.x += q; v.y += q; v.z += q; return v; }
template<typename V, if_vector4(V)> constexpr auto& operator+=(V& v, auto q) { v.x += q; v.y += q; v.z += q; v.w += q; return v; }

template<typename V, if_vector2(V)> constexpr auto& operator-=(V& l, const V& r) { l.x -= r.x; l.y -= r.y; return l; }
template<typename V, if_vector3(V)> constexpr auto& operator-=(V& l, const V& r) { l.x -= r.x; l.y -= r.y; l.z -= r.z; return l; }
template<typename V, if_vector4(V)> constexpr auto& operator-=(V& l, const V& r) { l.x -= r.x; l.y -= r.y; l.z -= r.z; l.w -= r.w; return l; }

template<typename V, if_vector2(V)> constexpr auto& operator-=(V& v, auto q) { v.x -= q; v.y -= q; return v; }
template<typename V, if_vector3(V)> constexpr auto& operator-=(V& v, auto q) { v.x -= q; v.y -= q; v.z -= q; return v; }
template<typename V, if_vector4(V)> constexpr auto& operator-=(V& v, auto q) { v.x -= q; v.y -= q; v.z -= q; v.w -= q; return v; }

template<typename V, if_vector2(V)> constexpr auto& operator*=(V& l, const V& r) { l.x *= r.x; l.y *= r.y; return l; }
template<typename V, if_vector3(V)> constexpr auto& operator*=(V& l, const V& r) { l.x *= r.x; l.y *= r.y; l.z *= r.z; return l; }
template<typename V, if_vector4(V)> constexpr auto& operator*=(V& l, const V& r) { l.x *= r.x; l.y *= r.y; l.z *= r.z; l.w *= r.w; return l; }

template<typename V, if_vector2(V)> constexpr auto& operator*=(V& v, auto q) { v.x *= q; v.y *= q; return v; }
template<typename V, if_vector3(V)> constexpr auto& operator*=(V& v, auto q) { v.x *= q; v.y *= q; v.z *= q; return v; }
template<typename V, if_vector4(V)> constexpr auto& operator*=(V& v, auto q) { v.x *= q; v.y *= q; v.z *= q; v.w *= q; return v; }

template<typename V, if_vector2(V)> constexpr auto& operator/=(V& l, const V& r) { l.x /= r.x; l.y /= r.y; return l; }
template<typename V, if_vector3(V)> constexpr auto& operator/=(V& l, const V& r) { l.x /= r.x; l.y /= r.y; l.z /= r.z; return l; }
template<typename V, if_vector4(V)> constexpr auto& operator/=(V& l, const V& r) { l.x /= r.x; l.y /= r.y; l.z /= r.z; l.w /= r.w; return l; }

template<typename V, if_vector2(V)> constexpr auto& operator/=(V& v, auto q) { v.x /= q; v.y /= q; return v; }
template<typename V, if_vector3(V)> constexpr auto& operator/=(V& v, auto q) { v.x /= q; v.y /= q; v.z /= q; return v; }
template<typename V, if_vector4(V)> constexpr auto& operator/=(V& v, auto q) { v.x /= q; v.y /= q; v.z /= q; v.w /= q; return v; }

template<typename V, if_vector2(V)> constexpr auto  operator+ (const V& v) { return V{+v.x, +v.y}; } // unary +
template<typename V, if_vector3(V)> constexpr auto  operator+ (const V& v) { return V{+v.x, +v.y, +v.z}; } // unary +
template<typename V, if_vector4(V)> constexpr auto  operator+ (const V& v) { return V{+v.x, +v.y, +v.z, +v.w}; } // unary +

template<typename V, if_vector2(V)> constexpr auto  operator- (const V& v) { return V{-v.x, -v.y}; } // unary -
template<typename V, if_vector3(V)> constexpr auto  operator- (const V& v) { return V{-v.x, -v.y, -v.z}; } // unary -
template<typename V, if_vector4(V)> constexpr auto  operator- (const V& v) { return V{-v.x, -v.y, -v.z, -v.w}; } // unary -

template<typename V, if_vector2(V)> constexpr auto  operator+ (const V& l, const V& r) { return V{l.x + r.x, l.y + r.y}; }
template<typename V, if_vector3(V)> constexpr auto  operator+ (const V& l, const V& r) { return V{l.x + r.x, l.y + r.y, l.z + r.z}; }
template<typename V, if_vector4(V)> constexpr auto  operator+ (const V& l, const V& r) { return V{l.x + r.x, l.y + r.y, l.z + r.z, l.w + r.w}; }

template<typename V, if_vector2(V)> constexpr auto  operator+ (const V& v, auto q) { return V{v.x + q, v.y + q}; }
template<typename V, if_vector3(V)> constexpr auto  operator+ (const V& v, auto q) { return V{v.x + q, v.y + q, v.z + q}; }
template<typename V, if_vector4(V)> constexpr auto  operator+ (const V& v, auto q) { return V{v.x + q, v.y + q, v.z + q, v.w + q}; }

template<typename V, if_vector2(V)> constexpr auto  operator+ (auto q, const V& v) { return V{q + v.x, q + v.y}; }
template<typename V, if_vector3(V)> constexpr auto  operator+ (auto q, const V& v) { return V{q + v.x, q + v.y, q + v.z}; }
template<typename V, if_vector4(V)> constexpr auto  operator+ (auto q, const V& v) { return V{q + v.x, q + v.y, q + v.z, q + v.w}; }

template<typename V, if_vector2(V)> constexpr auto  operator- (const V& l, const V& r) { return V{l.x - r.x, l.y - r.y}; }
template<typename V, if_vector3(V)> constexpr auto  operator- (const V& l, const V& r) { return V{l.x - r.x, l.y - r.y, l.z - r.z}; }
template<typename V, if_vector4(V)> constexpr auto  operator- (const V& l, const V& r) { return V{l.x - r.x, l.y - r.y, l.z - r.z, l.w - r.w}; }

template<typename V, if_vector2(V)> constexpr auto  operator- (const V& v, auto q) { return V{v.x - q, v.y - q}; }
template<typename V, if_vector3(V)> constexpr auto  operator- (const V& v, auto q) { return V{v.x - q, v.y - q, v.z - q}; }
template<typename V, if_vector4(V)> constexpr auto  operator- (const V& v, auto q) { return V{v.x - q, v.y - q, v.z - q, v.w - q}; }

template<typename V, if_vector2(V)> constexpr auto  operator- (auto q, const V& v) { return V{q - v.x, q - v.y}; }
template<typename V, if_vector3(V)> constexpr auto  operator- (auto q, const V& v) { return V{q - v.x, q - v.y, q - v.z}; }
template<typename V, if_vector4(V)> constexpr auto  operator- (auto q, const V& v) { return V{q - v.x, q - v.y, q - v.z, q - v.w}; }

template<typename V, if_vector2(V)> constexpr auto  operator* (const V& l, const V& r) { return V{l.x * r.x, l.y * r.y}; }
template<typename V, if_vector3(V)> constexpr auto  operator* (const V& l, const V& r) { return V{l.x * r.x, l.y * r.y, l.z * r.z}; }
template<typename V, if_vector4(V)> constexpr auto  operator* (const V& l, const V& r) { return V{l.x * r.x, l.y * r.y, l.z * r.z, l.w * r.w}; }

template<typename V, if_vector2(V)> constexpr auto  operator* (const V& v, auto q) { return V{v.x * q, v.y * q}; }
template<typename V, if_vector3(V)> constexpr auto  operator* (const V& v, auto q) { return V{v.x * q, v.y * q, v.z * q}; }
template<typename V, if_vector4(V)> constexpr auto  operator* (const V& v, auto q) { return V{v.x * q, v.y * q, v.z * q, v.w * q}; }

template<typename V, if_vector2(V)> constexpr auto  operator* (auto q, const V& v) { return V{q * v.x, q * v.y}; }
template<typename V, if_vector3(V)> constexpr auto  operator* (auto q, const V& v) { return V{q * v.x, q * v.y, q * v.z}; }
template<typename V, if_vector4(V)> constexpr auto  operator* (auto q, const V& v) { return V{q * v.x, q * v.y, q * v.z, q * v.w}; }

template<typename V, if_vector2(V)> constexpr auto  operator/ (const V& l, const V& r) { return V{l.x / r.x, l.y / r.y}; }
template<typename V, if_vector3(V)> constexpr auto  operator/ (const V& l, const V& r) { return V{l.x / r.x, l.y / r.y, l.z / r.z}; }
template<typename V, if_vector4(V)> constexpr auto  operator/ (const V& l, const V& r) { return V{l.x / r.x, l.y / r.y, l.z / r.z, l.w / r.w}; }

template<typename V, if_vector2(V)> constexpr auto  operator/ (const V& v, auto q) { return V{v.x / q, v.y / q}; }
template<typename V, if_vector3(V)> constexpr auto  operator/ (const V& v, auto q) { return V{v.x / q, v.y / q, v.z / q}; }
template<typename V, if_vector4(V)> constexpr auto  operator/ (const V& v, auto q) { return V{v.x / q, v.y / q, v.z / q, v.w / q}; }

template<typename V, if_vector2(V)> constexpr auto  operator/ (auto q, const V& v) { return V{q / v.x, q / v.y}; }
template<typename V, if_vector3(V)> constexpr auto  operator/ (auto q, const V& v) { return V{q / v.x, q / v.y, q / v.z}; }
template<typename V, if_vector4(V)> constexpr auto  operator/ (auto q, const V& v) { return V{q / v.x, q / v.y, q / v.z, q / v.w}; }

template<typename V, if_vector2(V)> constexpr auto  operator==(const V& l, const V& r) { return l.x == r.x && l.y == r.y; }
template<typename V, if_vector3(V)> constexpr auto  operator==(const V& l, const V& r) { return l.x == r.x && l.y == r.y && l.z == r.z; }
template<typename V, if_vector4(V)> constexpr auto  operator==(const V& l, const V& r) { return l.x == r.x && l.y == r.y && l.z == r.z && l.w == r.w; }

template<typename V, if_vector2(V)> constexpr auto  operator!=(const V& l, const V& r) { return l.r != r.x || l.y != r.y; }
template<typename V, if_vector3(V)> constexpr auto  operator!=(const V& l, const V& r) { return l.r != r.x || l.y != r.y || l.z != r.z; }
template<typename V, if_vector4(V)> constexpr auto  operator!=(const V& l, const V& r) { return l.r != r.x || l.y != r.y || l.z != r.z || l.w != r.w; }

////////////////////////////////////////////////////////////////////////////////
/**
 * @brief absolute value
 */
template<typename V, if_vector2(V)> constexpr auto abs(const V& v) { return V{std::abs(v.x), std::abs(v.y)}; }
template<typename V, if_vector3(V)> constexpr auto abs(const V& v) { return V{std::abs(v.x), std::abs(v.y), std::abs(v.z)}; }
template<typename V, if_vector4(V)> constexpr auto abs(const V& v) { return V{std::abs(v.x), std::abs(v.y), std::abs(v.z), std::abs(v.w)}; }

/**
 * @brief angle between two vectors
 */
template<typename V, if_vector(V)> constexpr auto angle(const V& l, const V& r) { return std::acos( dot(unit(l), unit(r)) ); }

/**
 * @brief cross product
 */
template<typename V, if_vector3(V)>
constexpr auto cross(const V& l, const V& r)
{
	return V{l.y * r.z - r.y * l.z, l.z * r.x - r.z * l.x, l.x * r.y - r.x * l.y};
}

/**
 * @brief distance between two points, squared and not
 */
template<typename V, if_vector(V)> constexpr auto dist_2(const V& l, const V& r) { return len_2(l - r); }
template<typename V, if_vector(V)> constexpr auto dist(const V& l, const V& r) { return std::sqrt(dist_2(l, r)); }

/**
 * @brief dot (scalar) product
 */
template<typename V, if_vector2(V)> constexpr auto dot(const V& l, const V& r) { return l.x * r.x + l.y * r.y; }
template<typename V, if_vector3(V)> constexpr auto dot(const V& l, const V& r) { return l.x * r.x + l.y * r.y + l.z * r.z; }
template<typename V, if_vector4(V)> constexpr auto dot(const V& l, const V& r) { return l.x * r.x + l.y * r.y + l.z * r.z + l.w * r.w; }

/**
 * @brief vector norm/length (squared and not)
 */
template<typename V, if_vector(V)> constexpr auto norm_2(const V& v) { return dot(v, v); }
template<typename V, if_vector(V)> constexpr auto len_2 (const V& v) { return norm_2(v); }

template<typename V, if_vector(V)> constexpr auto norm(const V& v) { return std::sqrt(norm_2(v)); }
template<typename V, if_vector(V)> constexpr auto len (const V& v) { return norm(v); }

/**
 * @brief normalized unit vector
 */
template<typename V, if_vector(V)> constexpr auto unit(const V& v) { return v / len(v); }

////////////////////////////////////////////////////////////////////////////////
#endif
