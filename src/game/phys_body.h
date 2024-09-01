#include "include/types.h"

struct PhysBody {
    struct Object *obj;
    struct CollisionMesh *colMesh;
    f32 centerGravity;
    f32 friction;
    void *vtxData;

};

struct CollisionMesh {

};
