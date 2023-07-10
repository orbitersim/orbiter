////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2023 Dimitry Ishenko
// Contact: dimitry (dot) ishenko (at) (gee) mail (dot) com
//
// Distributed under the MIT license.

////////////////////////////////////////////////////////////////////////////////
#ifndef VECTOR_HPP
#define VECTOR_HPP

#include <cmath>
#include <cstdint> // std::size_t
#include <iosfwd>  // std::ostream
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
 * @brief 3-dimensional vector of type double
 */
union VECTOR3
{
	struct { double x, y, z; };

	constexpr VECTOR3() : x{0}, y{0}, z{0} { }
	constexpr VECTOR3(double x, double y, double z) : x{x}, y{y}, z{z} { }

	constexpr VECTOR3(const VECTOR3&) = default;
	constexpr VECTOR3& operator=(const VECTOR3&) = default;

	constexpr auto const& operator[](std::size_t i) const { const double* d[] = {&x, &y, &z}; return *d[i]; }
	constexpr auto& operator[](std::size_t i) { double* d[] = {&x, &y, &z}; return *d[i]; }
};

/**
 * @brief 4-dimensional vector of type double
 */
union VECTOR4
{
	struct { double x, y, z, w; };

	constexpr VECTOR4() : x{0}, y{0}, z{0}, w{0} { }
	constexpr VECTOR4(double x, double y, double z, double w) : x{x}, y{y}, z{z}, w{w} { }

	constexpr VECTOR4(const VECTOR4&) = default;
	constexpr VECTOR4& operator=(const VECTOR4&) = default;

	constexpr auto const& operator[](std::size_t i) const { const double* d[] = {&x, &y, &z, &w}; return *d[i]; }
	constexpr auto& operator[](std::size_t i) { double* d[] = {&x, &y, &z, &w}; return *d[i]; }
};

template<> struct is_vector3<VECTOR3> : std::true_type { };
template<> struct is_vector4<VECTOR4> : std::true_type { };

////////////////////////////////////////////////////////////////////////////////
// TODO: move below types to global namespace
namespace oapi {

/**
 * @brief 2-dimensional vector of type float
 */
union FVECTOR2
{
	struct { float x, y; };

	constexpr FVECTOR2() : x{0}, y{0} { }
	constexpr FVECTOR2(float x, float y) : x{x}, y{y} { }

	constexpr FVECTOR2(const FVECTOR2&) = default;
	constexpr FVECTOR2& operator=(const FVECTOR2&) = default;

	constexpr auto const& operator[](std::size_t i) const { const float* d[] = {&x, &y}; return *d[i]; }
	constexpr auto& operator[](std::size_t i) { float* d[] = {&x, &y}; return *d[i]; }
};

/**
 * @brief 3-dimensional vector of type float
 */
union FVECTOR3
{
	struct { float x, y, z; };

	constexpr FVECTOR3() : x{0}, y{0}, z{0} { }
	constexpr FVECTOR3(float x, float y, float z) : x{x}, y{y}, z{z} { }

	constexpr FVECTOR3(const FVECTOR3&) = default;
	constexpr FVECTOR3& operator=(const FVECTOR3&) = default;

	constexpr auto const& operator[](std::size_t i) const { const float* d[] = {&x, &y, &z}; return *d[i]; }
	constexpr auto& operator[](std::size_t i) { float* d[] = {&x, &y, &z}; return *d[i]; }
};

}

using oapi::FVECTOR2;
using oapi::FVECTOR3;

template<> struct is_vector2<FVECTOR2> : std::true_type { };
template<> struct is_vector3<FVECTOR3> : std::true_type { };

////////////////////////////////////////////////////////////////////////////////
/**
 * @brief helper macros for vector type traits
 */
#define if_vector2(V) std::enable_if_t<is_vector2<V>::value>* = nullptr
#define if_vector3(V) std::enable_if_t<is_vector3<V>::value>* = nullptr
#define if_vector4(V) std::enable_if_t<is_vector4<V>::value>* = nullptr
#define if_vector( V) std::enable_if_t<is_vector2<V>::value || is_vector3<V>::value || is_vector4<V>::value>* = nullptr

template<typename V> using dtype = decltype (V::x);

/**
 * @brief vector operators
 */
template<typename V, if_vector2(V)> constexpr auto& operator+=(V& l, const V& r) { l.x += r.x; l.y += r.y; return l; }
template<typename V, if_vector3(V)> constexpr auto& operator+=(V& l, const V& r) { l.x += r.x; l.y += r.y; l.z += r.z; return l; }
template<typename V, if_vector4(V)> constexpr auto& operator+=(V& l, const V& r) { l.x += r.x; l.y += r.y; l.z += r.z; l.w += r.w; return l; }

template<typename V, if_vector2(V)> constexpr auto& operator+=(V& v, dtype<V> q) { v.x += q; v.y += q; return v; }
template<typename V, if_vector3(V)> constexpr auto& operator+=(V& v, dtype<V> q) { v.x += q; v.y += q; v.z += q; return v; }
template<typename V, if_vector4(V)> constexpr auto& operator+=(V& v, dtype<V> q) { v.x += q; v.y += q; v.z += q; v.w += q; return v; }

template<typename V, if_vector2(V)> constexpr auto& operator-=(V& l, const V& r) { l.x -= r.x; l.y -= r.y; return l; }
template<typename V, if_vector3(V)> constexpr auto& operator-=(V& l, const V& r) { l.x -= r.x; l.y -= r.y; l.z -= r.z; return l; }
template<typename V, if_vector4(V)> constexpr auto& operator-=(V& l, const V& r) { l.x -= r.x; l.y -= r.y; l.z -= r.z; l.w -= r.w; return l; }

template<typename V, if_vector2(V)> constexpr auto& operator-=(V& v, dtype<V> q) { v.x -= q; v.y -= q; return v; }
template<typename V, if_vector3(V)> constexpr auto& operator-=(V& v, dtype<V> q) { v.x -= q; v.y -= q; v.z -= q; return v; }
template<typename V, if_vector4(V)> constexpr auto& operator-=(V& v, dtype<V> q) { v.x -= q; v.y -= q; v.z -= q; v.w -= q; return v; }

template<typename V, if_vector2(V)> constexpr auto& operator*=(V& l, const V& r) { l.x *= r.x; l.y *= r.y; return l; }
template<typename V, if_vector3(V)> constexpr auto& operator*=(V& l, const V& r) { l.x *= r.x; l.y *= r.y; l.z *= r.z; return l; }
template<typename V, if_vector4(V)> constexpr auto& operator*=(V& l, const V& r) { l.x *= r.x; l.y *= r.y; l.z *= r.z; l.w *= r.w; return l; }

template<typename V, if_vector2(V)> constexpr auto& operator*=(V& v, dtype<V> q) { v.x *= q; v.y *= q; return v; }
template<typename V, if_vector3(V)> constexpr auto& operator*=(V& v, dtype<V> q) { v.x *= q; v.y *= q; v.z *= q; return v; }
template<typename V, if_vector4(V)> constexpr auto& operator*=(V& v, dtype<V> q) { v.x *= q; v.y *= q; v.z *= q; v.w *= q; return v; }

template<typename V, if_vector2(V)> constexpr auto& operator/=(V& l, const V& r) { l.x /= r.x; l.y /= r.y; return l; }
template<typename V, if_vector3(V)> constexpr auto& operator/=(V& l, const V& r) { l.x /= r.x; l.y /= r.y; l.z /= r.z; return l; }
template<typename V, if_vector4(V)> constexpr auto& operator/=(V& l, const V& r) { l.x /= r.x; l.y /= r.y; l.z /= r.z; l.w /= r.w; return l; }

template<typename V, if_vector2(V)> constexpr auto& operator/=(V& v, dtype<V> q) { v.x /= q; v.y /= q; return v; }
template<typename V, if_vector3(V)> constexpr auto& operator/=(V& v, dtype<V> q) { v.x /= q; v.y /= q; v.z /= q; return v; }
template<typename V, if_vector4(V)> constexpr auto& operator/=(V& v, dtype<V> q) { v.x /= q; v.y /= q; v.z /= q; v.w /= q; return v; }

template<typename V, if_vector2(V)> constexpr auto  operator+ (const V& v) { return V{+v.x, +v.y}; } // unary +
template<typename V, if_vector3(V)> constexpr auto  operator+ (const V& v) { return V{+v.x, +v.y, +v.z}; } // unary +
template<typename V, if_vector4(V)> constexpr auto  operator+ (const V& v) { return V{+v.x, +v.y, +v.z, +v.w}; } // unary +

template<typename V, if_vector2(V)> constexpr auto  operator- (const V& v) { return V{-v.x, -v.y}; } // unary -
template<typename V, if_vector3(V)> constexpr auto  operator- (const V& v) { return V{-v.x, -v.y, -v.z}; } // unary -
template<typename V, if_vector4(V)> constexpr auto  operator- (const V& v) { return V{-v.x, -v.y, -v.z, -v.w}; } // unary -

template<typename V, if_vector2(V)> constexpr auto  operator+ (const V& l, const V& r) { return V{l.x + r.x, l.y + r.y}; }
template<typename V, if_vector3(V)> constexpr auto  operator+ (const V& l, const V& r) { return V{l.x + r.x, l.y + r.y, l.z + r.z}; }
template<typename V, if_vector4(V)> constexpr auto  operator+ (const V& l, const V& r) { return V{l.x + r.x, l.y + r.y, l.z + r.z, l.w + r.w}; }

template<typename V, if_vector2(V)> constexpr auto  operator+ (const V& v, dtype<V> q) { return V{v.x + q, v.y + q}; }
template<typename V, if_vector3(V)> constexpr auto  operator+ (const V& v, dtype<V> q) { return V{v.x + q, v.y + q, v.z + q}; }
template<typename V, if_vector4(V)> constexpr auto  operator+ (const V& v, dtype<V> q) { return V{v.x + q, v.y + q, v.z + q, v.w + q}; }

template<typename V, if_vector2(V)> constexpr auto  operator+ (dtype<V> q, const V& v) { return V{q + v.x, q + v.y}; }
template<typename V, if_vector3(V)> constexpr auto  operator+ (dtype<V> q, const V& v) { return V{q + v.x, q + v.y, q + v.z}; }
template<typename V, if_vector4(V)> constexpr auto  operator+ (dtype<V> q, const V& v) { return V{q + v.x, q + v.y, q + v.z, q + v.w}; }

template<typename V, if_vector2(V)> constexpr auto  operator- (const V& l, const V& r) { return V{l.x - r.x, l.y - r.y}; }
template<typename V, if_vector3(V)> constexpr auto  operator- (const V& l, const V& r) { return V{l.x - r.x, l.y - r.y, l.z - r.z}; }
template<typename V, if_vector4(V)> constexpr auto  operator- (const V& l, const V& r) { return V{l.x - r.x, l.y - r.y, l.z - r.z, l.w - r.w}; }

template<typename V, if_vector2(V)> constexpr auto  operator- (const V& v, dtype<V> q) { return V{v.x - q, v.y - q}; }
template<typename V, if_vector3(V)> constexpr auto  operator- (const V& v, dtype<V> q) { return V{v.x - q, v.y - q, v.z - q}; }
template<typename V, if_vector4(V)> constexpr auto  operator- (const V& v, dtype<V> q) { return V{v.x - q, v.y - q, v.z - q, v.w - q}; }

template<typename V, if_vector2(V)> constexpr auto  operator- (dtype<V> q, const V& v) { return V{q - v.x, q - v.y}; }
template<typename V, if_vector3(V)> constexpr auto  operator- (dtype<V> q, const V& v) { return V{q - v.x, q - v.y, q - v.z}; }
template<typename V, if_vector4(V)> constexpr auto  operator- (dtype<V> q, const V& v) { return V{q - v.x, q - v.y, q - v.z, q - v.w}; }

template<typename V, if_vector2(V)> constexpr auto  operator* (const V& l, const V& r) { return V{l.x * r.x, l.y * r.y}; }
template<typename V, if_vector3(V)> constexpr auto  operator* (const V& l, const V& r) { return V{l.x * r.x, l.y * r.y, l.z * r.z}; }
template<typename V, if_vector4(V)> constexpr auto  operator* (const V& l, const V& r) { return V{l.x * r.x, l.y * r.y, l.z * r.z, l.w * r.w}; }

template<typename V, if_vector2(V)> constexpr auto  operator* (const V& v, dtype<V> q) { return V{v.x * q, v.y * q}; }
template<typename V, if_vector3(V)> constexpr auto  operator* (const V& v, dtype<V> q) { return V{v.x * q, v.y * q, v.z * q}; }
template<typename V, if_vector4(V)> constexpr auto  operator* (const V& v, dtype<V> q) { return V{v.x * q, v.y * q, v.z * q, v.w * q}; }

template<typename V, if_vector2(V)> constexpr auto  operator* (dtype<V> q, const V& v) { return V{q * v.x, q * v.y}; }
template<typename V, if_vector3(V)> constexpr auto  operator* (dtype<V> q, const V& v) { return V{q * v.x, q * v.y, q * v.z}; }
template<typename V, if_vector4(V)> constexpr auto  operator* (dtype<V> q, const V& v) { return V{q * v.x, q * v.y, q * v.z, q * v.w}; }

template<typename V, if_vector2(V)> constexpr auto  operator/ (const V& l, const V& r) { return V{l.x / r.x, l.y / r.y}; }
template<typename V, if_vector3(V)> constexpr auto  operator/ (const V& l, const V& r) { return V{l.x / r.x, l.y / r.y, l.z / r.z}; }
template<typename V, if_vector4(V)> constexpr auto  operator/ (const V& l, const V& r) { return V{l.x / r.x, l.y / r.y, l.z / r.z, l.w / r.w}; }

template<typename V, if_vector2(V)> constexpr auto  operator/ (const V& v, dtype<V> q) { return V{v.x / q, v.y / q}; }
template<typename V, if_vector3(V)> constexpr auto  operator/ (const V& v, dtype<V> q) { return V{v.x / q, v.y / q, v.z / q}; }
template<typename V, if_vector4(V)> constexpr auto  operator/ (const V& v, dtype<V> q) { return V{v.x / q, v.y / q, v.z / q, v.w / q}; }

template<typename V, if_vector2(V)> constexpr auto  operator/ (dtype<V> q, const V& v) { return V{q / v.x, q / v.y}; }
template<typename V, if_vector3(V)> constexpr auto  operator/ (dtype<V> q, const V& v) { return V{q / v.x, q / v.y, q / v.z}; }
template<typename V, if_vector4(V)> constexpr auto  operator/ (dtype<V> q, const V& v) { return V{q / v.x, q / v.y, q / v.z, q / v.w}; }

template<typename V, if_vector2(V)> constexpr auto  operator==(const V& l, const V& r) { return l.x == r.x && l.y == r.y; }
template<typename V, if_vector3(V)> constexpr auto  operator==(const V& l, const V& r) { return l.x == r.x && l.y == r.y && l.z == r.z; }
template<typename V, if_vector4(V)> constexpr auto  operator==(const V& l, const V& r) { return l.x == r.x && l.y == r.y && l.z == r.z && l.w == r.w; }

template<typename V, if_vector2(V)> constexpr auto  operator!=(const V& l, const V& r) { return l.r != r.x || l.y != r.y; }
template<typename V, if_vector3(V)> constexpr auto  operator!=(const V& l, const V& r) { return l.r != r.x || l.y != r.y || l.z != r.z; }
template<typename V, if_vector4(V)> constexpr auto  operator!=(const V& l, const V& r) { return l.r != r.x || l.y != r.y || l.z != r.z || l.w != r.w; }

/**
 * @brief stream insertion operators
 */
template<typename V, if_vector2(V)> inline auto& operator<<(std::ostream& os, const V& v) { os << v.x << ',' << v.y; return os; }
template<typename V, if_vector3(V)> inline auto& operator<<(std::ostream& os, const V& v) { os << v.x << ',' << v.y << ',' << v.z; return os; }
template<typename V, if_vector4(V)> inline auto& operator<<(std::ostream& os, const V& v) { os << v.x << ',' << v.y << ',' << v.z << ',' << v.w; return os; }

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
 * @brief distance between two points (squared and not)
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
 * @brief e raised to the power
 */
template<typename V, if_vector2(V)> constexpr auto exp(const V& v) { return V{std::exp(v.x), std::exp(v.y)}; }
template<typename V, if_vector3(V)> constexpr auto exp(const V& v) { return V{std::exp(v.x), std::exp(v.y), std::exp(v.z)}; }
template<typename V, if_vector4(V)> constexpr auto exp(const V& v) { return V{std::exp(v.x), std::exp(v.y), std::exp(v.z), std::exp(v.w)}; }

/**
 * @brief linear interpolation
 */
template<typename V, if_vector(V)> constexpr auto lerp(const V& a, const V& b, dtype<V> t) { return a + t * (b - a); }

/**
 * @brief type conversions
 */
template<typename U, typename V, if_vector2(U), if_vector2(V)>
constexpr auto morph_to(const V& v) { return U{static_cast<dtype<U>>(v.x), static_cast<dtype<U>>(v.y)}; }

template<typename U, typename V, if_vector3(U), if_vector3(V)>
constexpr auto morph_to(const V& v) { return U{static_cast<dtype<U>>(v.x), static_cast<dtype<U>>(v.y), static_cast<dtype<U>>(v.z)}; }

template<typename U, typename V, if_vector4(U), if_vector4(V)>
constexpr auto morph_to(const V& v) { return U{static_cast<dtype<U>>(v.x), static_cast<dtype<U>>(v.y), static_cast<dtype<U>>(v.z), static_cast<dtype<U>>(v.w)}; }

/**
 * @brief reduction
 */
template<typename U, typename V, if_vector2(U), if_vector3(V)>
constexpr auto morph_to(const V& v) { return U{static_cast<dtype<U>>(v.x), static_cast<dtype<U>>(v.y)}; }

template<typename U, typename V, if_vector3(U), if_vector4(V)>
constexpr auto morph_to(const V& v) { return U{static_cast<dtype<U>>(v.x), static_cast<dtype<U>>(v.y), static_cast<dtype<U>>(v.z)}; }

/**
 * @brief expansion
 */
template<typename U, typename V, if_vector3(U), if_vector2(V)>
constexpr auto morph_to(const V& v, dtype<U> z = 0) { return U{static_cast<dtype<U>>(v.x), static_cast<dtype<U>>(v.y), z}; }

template<typename U, typename V, if_vector4(U), if_vector3(V)>
constexpr auto morph_to(const V& v, dtype<U> w = 0) { return U{static_cast<dtype<U>>(v.x), static_cast<dtype<U>>(v.y), static_cast<dtype<U>>(v.z), w}; }

/**
 * @brief vector norm/length (squared and not)
 */
template<typename V, if_vector(V)> constexpr auto norm_2(const V& v) { return dot(v, v); }
template<typename V, if_vector(V)> constexpr auto len_2 (const V& v) { return norm_2(v); }

template<typename V, if_vector(V)> constexpr auto norm(const V& v) { return std::sqrt(norm_2(v)); }
template<typename V, if_vector(V)> constexpr auto len (const V& v) { return norm(v); }

/**
 * @brief raise to the power
 */
template<typename V, if_vector2(V)> constexpr auto pow(const V& l, const V& r) { return V{std::pow(l.x, r.x), std::pow(l.y, r.y)}; }
template<typename V, if_vector3(V)> constexpr auto pow(const V& l, const V& r) { return V{std::pow(l.x, r.x), std::pow(l.y, r.y), std::pow(l.z, r.z)}; }
template<typename V, if_vector4(V)> constexpr auto pow(const V& l, const V& r) { return V{std::pow(l.x, r.x), std::pow(l.y, r.y), std::pow(l.z, r.z), std::pow(l.w, r.w)}; }

template<typename V, if_vector2(V)> constexpr auto pow(const V& v, dtype<V> e) { return V{std::pow(v.x, e), std::pow(v.y, e)}; }
template<typename V, if_vector3(V)> constexpr auto pow(const V& v, dtype<V> e) { return V{std::pow(v.x, e), std::pow(v.y, e), std::pow(v.z, e)}; }
template<typename V, if_vector4(V)> constexpr auto pow(const V& v, dtype<V> e) { return V{std::pow(v.x, e), std::pow(v.y, e), std::pow(v.z, e), std::pow(v.w, e)}; }

template<typename V, if_vector2(V)> constexpr auto pow(dtype<V> b, const V& v) { return V{std::pow(b, v.x), std::pow(b, v.y)}; }
template<typename V, if_vector3(V)> constexpr auto pow(dtype<V> b, const V& v) { return V{std::pow(b, v.x), std::pow(b, v.y), std::pow(b, v.z)}; }
template<typename V, if_vector4(V)> constexpr auto pow(dtype<V> b, const V& v) { return V{std::pow(b, v.x), std::pow(b, v.y), std::pow(b, v.z), std::pow(b, v.w)}; }

/**
 * @brief square root
 */
template<typename V, if_vector2(V)> constexpr auto sqrt(const V& v) { return V{std::sqrt(v.x), std::sqrt(v.y)}; }
template<typename V, if_vector3(V)> constexpr auto sqrt(const V& v) { return V{std::sqrt(v.x), std::sqrt(v.y), std::sqrt(v.z)}; }
template<typename V, if_vector4(V)> constexpr auto sqrt(const V& v) { return V{std::sqrt(v.x), std::sqrt(v.y), std::sqrt(v.z), std::sqrt(v.w)}; }

/**
 * @brief normalized unit vector
 */
template<typename V, if_vector(V)> constexpr auto unit(const V& v) { return v / len(v); }

////////////////////////////////////////////////////////////////////////////////
#endif
