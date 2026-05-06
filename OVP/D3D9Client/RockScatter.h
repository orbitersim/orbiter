// ==============================================================
// RockScatter.h
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// Dual licensed under GPL v3 and LGPL v3
// ==============================================================

// ==============================================================
// Procedural surface rock scatter system.
// Generates deterministic fields of small, medium and large rocks
// on planetary surfaces that support the feature.  Every tile
// always produces the exact same rock field given the same seed.
// Potentially also works for vegetation and shrubbery.
// ==============================================================

#ifndef __ROCKSCATTER_H
#define __ROCKSCATTER_H

#include "D3D9Client.h"
#include "D3D9Util.h"
#include "Mesh.h"
#include <mutex>
#include <unordered_map>
#include <vector>

// Maximum number of AREA_TO_CLEAR entries per base
#define MAX_CLEAR_AREAS_PER_BASE 32

class vPlanet;

// Per-planet configuration (parsed from the planet's .cfg file)

struct RockScatterCfg {
  bool bEnabled;           // Master enable
  float fDrawDist;         // Max draw distance in metres (default 500)
  float fDensity;          // Rocks per square metre      (default 0.01)
  UINT uSeed;              // Base seed (0 = derive from planet name hash)
  float fSizeSmall[2];     // Min / max scale for small rocks   {0.1, 0.5}
  float fSizeMedium[2];    // Min / max scale for medium rocks  {0.5, 2.0}
  float fSizeLarge[2];     // Min / max scale for large rocks   {2.0, 8.0}
  float fRatioSmall;       // Proportion of small  rocks (0.70)
  float fRatioMedium;      // Proportion of medium rocks (0.25)
  float fRatioLarge;       // Proportion of large  rocks (0.05)
  std::string sMeshPrefix; // Optional custom mesh prefix for procedural loading
};

// Scatter renderer

class RockScatter {
public:
  RockScatter(vPlanet *planet, LPDIRECT3DDEVICE9 pDev);
  ~RockScatter();

  /// Called once per frame from vPlanet::Render().
  void Render(LPDIRECT3DDEVICE9 pDev);

  /// Called from vPlanet::RenderBaseShadows().
  void RenderShadows(LPDIRECT3DDEVICE9 pDev, float alpha);

  double GetElevationModifier(double lng, double lat) const;

  /// 3D bounding-sphere collision check for mesh-accurate collisions
  struct CollisionResult {
    bool hit;
    VECTOR3 normal; // mesh surface normal in planet-local frame, points away
                    // from rock
    double depth;   // penetration depth in metres
    VECTOR3 contactPtLocal; // deepest contact point in planet-local frame
  };
  CollisionResult CheckCollision(const VECTOR3 *hullPtsLocal, int nPts,
                                 const VECTOR3 &vesselPosLocal,
                                 double vesselRadius,
                                 float maxCollisionDist) const;

private:
  // One rock instance inside a tile
  struct RockInstance {
    D3DXVECTOR3 localPos; // Position on unit sphere (geocentric direction)
    float elevation;      // Surface elevation at the point (metres)
    float scale;          // Size multiplier
    float rotY;           // Y-axis rotation in radians
    uint8_t sizeClass;    // 0 = small, 1 = medium, 2 = large
    uint8_t
        meshIndex; // Index of the specific mesh inside m_meshPool[sizeClass]
  };

  // Tile key for the instance cache
  struct TileKey {
    int lvl, ilat, ilng;
    bool operator==(const TileKey &o) const {
      return lvl == o.lvl && ilat == o.ilat && ilng == o.ilng;
    }
  };

  // Hash functor for TileKey (used by unordered_map)
  struct TileKeyHash {
    size_t operator()(const TileKey &k) const {
      size_t h = (size_t)k.lvl * 73856093u ^ (size_t)k.ilat * 19349663u ^
                 (size_t)k.ilng * 83492791u;
      return h;
    }
  };

  // Rectangular exclusion zone around a surface base
  struct ClearZone {
    double baseLng; // Base longitude (radians)
    double baseLat; // Base latitude (radians)
    float halfExtX; // Half-extent in east-west direction (metres)
    float halfExtY; // Half-extent in north-south direction (metres)
  };

  // Generate (or look up cached) rocks for one tile
  const std::vector<RockInstance> &GetRocksForTile(int lvl, int ilat,
                                                   int ilng) const;

  // Load AREA_TO_CLEAR definitions from all base config files on this planet
  void LoadBaseClearZones();

  // Check whether a given position (radians) falls inside any clear zone
  bool IsInClearZone(double lng, double lat) const;

  // Build procedural icosphere-based rock meshes
  void CreateRockMeshes();
  void CreateIcosphereMesh(int subdivisions, UINT seed, float baseScale,
                           std::vector<NTVERTEX> &outVerts,
                           std::vector<WORD> &outIdxs);

  // Deterministic hash / helpers
  static uint32_t HashTile(uint32_t seed, int lvl, int ilat, int ilng);
  static uint32_t XorShift32(uint32_t &state);
  static float RandFloat(uint32_t &state); // [0, 1)
  static float RandRange(uint32_t &state, float lo, float hi);

  // Owner planet
  vPlanet *m_planet;
  LPDIRECT3DDEVICE9 m_pDev;

  // Per-mesh collision geometry (triangles stored as position-only)
  struct CollisionGeom {
    struct Tri {
      D3DXVECTOR3 v0, v1, v2;
    };
    std::vector<Tri> tris;
    float maxRadius; // bounding sphere radius (model space)
  };

  // Meshes used for the 3 size classes (0=Small, 1=Medium, 2=Large)
  std::vector<D3D9Mesh *> m_meshPool[3];
  std::vector<float> m_meshBottomExtent[3];
  std::vector<CollisionGeom> m_collGeom[3]; // parallel to m_meshPool

  // Cached per-tile rock lists
  mutable std::mutex m_cacheMutex;
  mutable std::unordered_map<TileKey, std::vector<RockInstance>, TileKeyHash>
      m_cache;

  // Derived seed (computed once from planet name + config seed)
  uint32_t m_seed;

  float m_lastDensityMult;

  // Base clear zones loaded from all surface base configs on this planet
  std::vector<ClearZone> m_clearZones;

  /// Raycast downward through a rock's collision mesh.
  /// Returns the highest mesh Y coordinate at the given local XZ position,
  /// or -1e30 if the point is outside all triangles.
  static float RaycastMeshY(const CollisionGeom &geom, float localX,
                            float localZ);
};

#endif // !__ROCKSCATTER_H
