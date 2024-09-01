#pragma once

#include <PR/ultratypes.h>
#include "macros.h"
#include "engine/math_util.h"

#define DAMPING 0.999f

#define SLEEP_DETECTION_BIAS 0.8f
#define SLEEP_DETECTION_THRESHOLD 3.5f

#define PENETRATION_BIAS 0.8f
#define PENETRATION_MIN_DEPTH 0.f
#define PENETRATION_MAX_DEPTH 50.f

#define GRAVITY_FORCE -4.f
#define FRICTION 2.9f

#define NUM_RIGID_BODY_STEPS 2
#define NUM_IMPULSE_ITERATIONS 2

#define MAX_RIGID_BODIES 40

typedef Vec4f Quat;

#define increment_debug_counter(a, b)

/// Add a vector scaled by a constant to another vector.
#define vec3f_add_scaled(dest, src, scale) { \
    dest[0] += src[0] * scale; \
    dest[1] += src[1] * scale; \
    dest[2] += src[2] * scale; \
}

/// Set a vector to another vector scaled by a constant.
#define vec3f_scale(dest, src, scale) { \
    dest[0] = src[0] * scale; \
    dest[1] = src[1] * scale; \
    dest[2] = src[2] * scale; \
}


// Controls an instance of a rigid body
struct RigidBody {
    u8 allocated:1; // Mark if the struct has been allocated
    u8 isStatic:1; // Can the rigid body move
    u8 asleep:1; // Body goes to sleep if inactive for a while, and collision is not calculated
    u8 hasGravity:1; // Apply gravity to the body
    u8 hardcodedFloor:1; // Determines if it uses the hardcoded floor
    u8 colOnly:1;

    f32 mass;
    f32 invMass; // 1/m (for performance)
    f32 motion; // Average motion over the past few frames, used to determine if the body should sleep
    Vec3f size;
    Vec3f centerOfMass; // Position
    Vec3f minCorner; // Lowest coords of AABB
    Vec3f maxCorner; // Highest coords of AABB
    Quat angleQuat; // Orientation

    Vec3f linearVel; // Linear velocity
    Vec3f angularVel; // Angular velocity
    Vec3f linearDisplacement;
    Vec3f angularDisplacement;
    Vec3f netForce; // Total force applied
    Vec3f netTorque; // Total torque applied

    Mat4 transform; // The transformation matrix of the body
    Mat4 invInertia; // Inverse inertia tensor

    struct MeshInfo *mesh; // Pointer to the mesh info of the body
    struct Object *obj; // Pointer to the object this rigid body is linked to
    s16 minRoll;
    s16 maxRoll;
    s16 minYaw;
    s16 maxYaw;
    s16 minPitch;
    s16 maxPitch;
    struct RigidBody *parentBody;
    Vec3f attachPoint[5];
};

struct PhysicsConstraint {
    struct RigidBody *body;

    
};

typedef u8 MeshEdge[2];
typedef u8 MeshTri[3];
typedef u8 MeshQuad[4];

// A specific contact point between two rigid bodies
struct CollisionPoint {
    Vec3f point;
    Vec3f normal;
    f32 penetration;
};

// An array of all contact points between two specific bodies
struct Collision {
    struct RigidBody *body1;
    struct RigidBody *body2;
    struct CollisionPoint points[50];
    u32 numPoints;
};

struct MeshInfo {
    Vec3f *vertices; // will be scaled by rigid body's size
    MeshEdge *edges;
    MeshTri *tris;
    MeshQuad *quads;
    u32 numVertices;
    u32 numEdges;
    u32 numTris;
    u32 numQuads;
};

struct TriangleInfo {
    Vec3f vertices[3];
    Vec3f normal;
};

struct QuadInfo {
    Vec3f vertices[4];
    Vec3f normal;
};

extern struct Collision gCollisions[100];
extern u32 gNumCollisions;
extern struct RigidBody gRigidBodies[MAX_RIGID_BODIES];

struct RigidBody *allocate_rigid_body_from_object(struct Object *obj, struct MeshInfo *mesh, f32 mass, Vec3f size, u8 hc_floor);
struct RigidBody *allocate_rigid_body(struct MeshInfo *mesh, f32 mass, Vec3f size, Vec3f pos);
void deallocate_rigid_body(struct RigidBody *body);
void rigid_body_set_yaw(struct RigidBody *body, s16 yaw);
void rigid_body_do_collision(u32 bodyIndex);

extern void quat_to_euler(Quat q, Vec3f dest);
extern void euler_to_quat(Vec3f from, Quat q);

void do_rigid_body_step(void);
