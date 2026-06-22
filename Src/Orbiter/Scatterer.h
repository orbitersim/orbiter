// Copyright (c) EvESpirit
// Licensed under the MIT License
// ==============================================================
// Scatterer.h
// Part of the ORBITER core engine
// ==============================================================
//
// Core surface scatter system - procedurally generates deterministic
// rock fields on planetary surfaces and provides collision geometry.
// Rendering is handled separately by the graphics client.
// ==============================================================

#ifndef __ROCKSCATTER_H
#define __ROCKSCATTER_H

#include "OrbiterAPI.h"
#include <mutex>
#include <unordered_map>
#include <vector>
#include <string>
#include "ZTreeMgr.h"

#define MAX_CLEAR_AREAS_PER_BASE 32

class Planet;

class Scatterer {
public:
  Scatterer(Planet *planet);
  ~Scatterer();

  /// Get the config for this planet's surface scatter
  const ScattererCfg& GetConfig() const { return m_cfg; }

  /// Get rocks for a specific tile (cached)
  const std::vector<ScatterInstance>& GetScatterForTile(int lvl, int ilat, int ilng) const;

  /// Get the additional elevation caused by rocks at a given lng/lat
  double GetElevationModifier(double lng, double lat) const;

  /// Collision result from mesh-to-mesh check
  struct CollisionResult {
    bool hit;
    VECTOR3 normal;
    double depth;
    VECTOR3 contactPtLocal;
  };

  /// Check hull points against all nearby rocks for collision
  CollisionResult CheckCollision(const VECTOR3 *hullPtsLocal, int nPts,
                                 const VECTOR3 &vesselPosLocal,
                                 double vesselRadius,
                                 float maxCollisionDist) const;

  /// Get collision geometry for a given size class and mesh index
  struct CollisionGeom {
    struct Tri {
      VECTOR3 v0, v1, v2;
    };
    std::vector<Tri> tris;
    float maxRadius;
  };

  const std::vector<CollisionGeom>& GetCollisionGeom(int sizeClass) const {
    return m_collGeom[sizeClass];
  }
  const std::vector<float>& GetBottomExtents(int sizeClass) const {
    return m_meshBottomExtent[sizeClass];
  }

public:
  struct ScatterTileData {
    uint8_t *data;
    uint8_t *originalData;
    int width, height;
    int bpp;
    uint32_t rMask, gMask, bMask, aMask;
    bool isDXT1;
    bool isDXT5;
    int lvl_loaded, ilat_loaded, ilng_loaded;

    ScatterTileData() : data(nullptr), originalData(nullptr), width(0), height(0), bpp(0), rMask(0), gMask(0), bMask(0), aMask(0), isDXT1(false), isDXT5(false), lvl_loaded(0), ilat_loaded(0), ilng_loaded(0) {}
  };

private:
  struct TileKey {
    int lvl, ilat, ilng;
    bool operator==(const TileKey &o) const {
      return lvl == o.lvl && ilat == o.ilat && ilng == o.ilng;
    }
  };



  struct TileKeyHash {
    size_t operator()(const TileKey &k) const {
      return (size_t)k.lvl * 73856093u ^ (size_t)k.ilat * 19349663u ^
             (size_t)k.ilng * 83492791u;
    }
  };

  struct ClearZone {
    double baseLng;
    double baseLat;
    float halfExtX;
    float halfExtY;
  };

  void LoadBaseClearZones();
  bool IsInClearZone(double lng, double lat) const;
  void BuildCollisionGeometry();
  void LoadScatterMapTile(int lvl, int ilat, int ilng, ScatterTileData &tileData) const;

  static uint32_t HashTile(uint32_t seed, int lvl, int ilat, int ilng);
  static uint32_t XorShift32(uint32_t &state);
  static float RandFloat(uint32_t &state);
  static float RandRange(uint32_t &state, float lo, float hi);

  static VECTOR3 Norm3(const VECTOR3 &v);

  Planet *m_planet;
  ScattererCfg m_cfg;

  std::vector<float> m_meshBottomExtent[3];
  std::vector<CollisionGeom> m_collGeom[3];
  int m_meshCount[3]; // number of mesh variants per size class

  mutable std::mutex m_cacheMutex;
  mutable std::unordered_map<TileKey, std::vector<ScatterInstance>, TileKeyHash> m_cache;
  mutable std::unordered_map<TileKey, ScatterTileData, TileKeyHash> m_mapCache;

  ZTreeMgr *m_scatterTreeMgr;
  bool m_bScatterDirExists;

  uint32_t m_seed;
  std::vector<ClearZone> m_clearZones;
  mutable float m_lastDensityMult;

  static float RaycastMeshY(const CollisionGeom &geom, float localX, float localZ);
};

#endif // !__ROCKSCATTER_H
