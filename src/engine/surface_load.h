#ifndef SURFACE_LOAD_H
#define SURFACE_LOAD_H

#include <PR/ultratypes.h>

#include "surface_collision.h"
#include "types.h"


#ifdef COLLISION_IMPROVEMENTS
    // Improves the handling of convex wall corners by rounding wall collision at triangle edges to close the seams.
    #define ROUNDED_WALL_CORNERS
    //! TODO: Pproperly handles simultaneous collision with multiple walls (eg. concave wall corners or narrow tunnels).
    #define SIMULTANEOUS_WALL_COLLISIONS
    // Uses the surface closest to the source height when searching for a floor or ceiling, instead of just using the first applicable surface in the list.
    #define SLOPE_FIX
    // Fixes an issue where entering any area above a ceiling without an intermediate floor would count as hitting a ceiling.
    // NOTE: This causes Mario to potentially clip through the vertical part of the rocking JRB ship at cettain angles.
    #define EXPOSED_CEILINGS_FIX
    // Disables bonking on sloped ceilings by separating the handling of ceiling interactions and wall interactions.
    #define DISABLE_CEILING_BONKS
#endif


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

typedef struct SurfaceNode SpatialPartitionCell[NUM_SPATIAL_PARTITIONS];

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
void load_area_terrain(s32 index, TerrainData *data, RoomData *surfaceRooms, MacroObject *macroObjects);
void clear_dynamic_surfaces(void);
void load_object_collision_model(void);
void load_object_static_model(void);

#endif // SURFACE_LOAD_H
