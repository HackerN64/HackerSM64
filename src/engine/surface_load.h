#ifndef SURFACE_LOAD_H
#define SURFACE_LOAD_H

#include <PR/ultratypes.h>

#include "surface_collision.h"
#include "types.h"

#define SURFACE_VERTICAL_BUFFER 5

#define NORMAL_FLOOR_THRESHOLD 0.01f
#define NORMAL_CEIL_THRESHOLD -NORMAL_FLOOR_THRESHOLD

/**
 * The size of the dynamic surface pool, in bytes.
 */
#define DYNAMIC_SURFACE_POOL_SIZE 0x8000

struct SurfaceNode {
    struct SurfaceNode *next;
    struct Surface *surface;
};

enum SpatialPartitions {
    SPATIAL_PARTITION_FLOORS,
    SPATIAL_PARTITION_CEILS,
    SPATIAL_PARTITION_WALLS,
    SPATIAL_PARTITION_WATER,
    NUM_SPATIAL_PARTITIONS
};

typedef struct SurfaceNode *SpatialPartitionCell[NUM_SPATIAL_PARTITIONS];

extern SpatialPartitionCell gStaticSurfacePartition[NUM_CELLS][NUM_CELLS];
extern SpatialPartitionCell gDynamicSurfacePartition[NUM_CELLS][NUM_CELLS];
extern void *gCurrStaticSurfacePool;
extern void *gDynamicSurfacePool;
extern void *gCurrStaticSurfacePoolEnd;
extern void *gDynamicSurfacePoolEnd;
extern u32 gTotalStaticSurfaceData;

void alloc_surface_pools(void);
#ifdef NO_SEGMENTED_MEMORY
u32 get_area_terrain_size(TerrainData *data);
#endif
void load_area_terrain(TerrainData *data, RoomData *surfaceRooms);
void clear_dynamic_surfaces(void);
void load_object_collision_model(void);
void load_object_static_model(void);

#endif // SURFACE_LOAD_H
