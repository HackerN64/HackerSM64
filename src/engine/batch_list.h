#pragma once

#include "sm64.h"
#include "engine/graph_node.h"

struct BatchArray* batch_list_objects_alloc_xlu_decal(struct AllocOnlyPool *pool);
struct BatchArray* batch_list_objects_alloc_cld(struct AllocOnlyPool *pool);

static inline struct BatchArray* batch_list_objects_alloc(struct AllocOnlyPool *pool, enum RenderLayers layer) {
    switch (layer) {
        case LAYER_TRANSPARENT_DECAL:
            return batch_list_objects_alloc_xlu_decal(pool);
        case LAYER_CLD:
            return batch_list_objects_alloc_cld(pool);
        default:
            return 0;
    }
}
