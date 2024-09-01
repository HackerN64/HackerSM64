#include <PR/ultratypes.h>

#include "config.h"
#include "sm64.h"
#include "rigid_body.h"
#include "object_list_processor.h"
#include "engine/surface_collision.h"
#include "engine/surface_load.h"

struct RigidBody gRigidBodies[MAX_RIGID_BODIES];

f32 dt = 1.f/NUM_RIGID_BODY_STEPS;

// Sets matrix 'dest' to the matrix product T(b) * a. Assumes that b has no
// translation component.
void mtxf_mul_transpose(Mat4 dest, Mat4 a, Mat4 b) {
    Vec3f entry;
    register f32 *temp  = (f32 *)a;
    register f32 *temp2 = (f32 *)dest;
    register f32 *temp3;
    register s32 i;
    for (i = 0; i < 16; i++) {
        vec3_copy(entry, temp);
        for (temp3 = (f32 *)b; (i & 3) != 3; i++) {
            *temp2 = ((entry[0] * temp3[0])
                    + (entry[1] * temp3[1])
                    + (entry[2] * temp3[2]));
            temp2++;
            temp3 += 4;
        }
        *temp2 = 0;
        temp += 4;
        temp2++;
    }
    ((u32 *) dest)[15] = FLOAT_ONE;
}


/// Convert a quaternion to a rotation matrix.
void mtxf_from_quat(Quat q, Mat4 dest) {
    dest[0][0] = -2.f * (q[1] * q[1] + q[2] * q[2]) + 1.f;
    dest[0][1] =  2.f * (q[0] * q[1] - q[3] * q[2]);
    dest[0][2] =  2.f * (q[0] * q[2] + q[3] * q[1]);

    dest[1][0] =  2.f * (q[0] * q[1] + q[3] * q[2]);
    dest[1][1] = -2.f * (q[0] * q[0] + q[2] * q[2]) + 1.f;
    dest[1][2] =  2.f * (q[1] * q[2] - q[3] * q[0]);

    dest[2][0] =  2.f * (q[0] * q[2] - q[3] * q[1]);
    dest[2][1] =  2.f * (q[1] * q[2] + q[3] * q[0]);
    dest[2][2] = -2.f * (q[0] * q[0] + q[1] * q[1]) + 1.f;

    dest[3][0] = dest[3][1] = dest[3][2] = dest[0][3] = dest[1][3] = dest[2][3] = 0.f;
    dest[3][3] = 1.f;
}

/*
void quat_to_euler(Quat q, Vec3f dest) {
    // roll (x-axis rotation)
    double sinr_cosp = 2 * (q[3] * q[0] + q[1] * q[2]);
    double cosr_cosp = 1 - 2 * (q[0] * q[0] + q[1] * q[1]);
    dest[0] = atan2f(sinr_cosp, cosr_cosp);

    double m22 = sqrtf(1 - 2 * (q[0] * q[0] + q[2] * q[2]));
    double m21 = sqrtf(2 * (q[1] * q[2] - q[3] * q[0]));
    dest[1] = atan2f(m21, m22);

    // yaw (z-axis rotation)
    double siny_cosp = 2 * (q[3] * q[2] + q[0] * q[1]);
    double cosy_cosp = 1 - 2 * (q[1] * q[1] + q[2] * q[2]);
    dest[2] = 2 * atan2f(siny_cosp, cosy_cosp) - M_PI / 2;
}
*/

void quat_to_euler(Quat q, Vec3f dest) {
    dest[0] = 2.0f * (q[1] * q[3] - q[0] * q[2]);
    dest[2] = 2.0f * (q[2] * q[3] + q[0] * q[1]);
    dest[1] = q[0] * q[0] - q[1] * q[1] - q[2] * q[2] + q[3] * q[3];
}

void euler_to_quat(Vec3f from, Quat q) {

    double cos_x_2 = cosf(from[0] / 2.0);
    double sin_x_2 = sinf(from[0] / 2.0);
    double cos_y_2 = cosf(from[1] / 2.0);
    double sin_y_2 = sinf(from[1] / 2.0);
    double cos_z_2 = cosf(from[2] / 2.0);
    double sin_z_2 = sinf(from[2] / 2.0);

    q[3] = cos_z_2 * cos_y_2 * cos_x_2 + sin_z_2 * sin_y_2 * sin_x_2;
    q[0] = cos_z_2 * cos_y_2 * sin_x_2 - sin_z_2 * sin_y_2 * cos_x_2;
    q[1] = cos_z_2 * sin_y_2 * cos_x_2 + sin_z_2 * cos_y_2 * sin_x_2;
    q[2] = sin_z_2 * cos_y_2 * cos_x_2 - cos_z_2 * sin_y_2 * sin_x_2;

}

/// Copy a quaternion.
void quat_copy(Quat dest, Quat src) {
    dest[0] = src[0];
    dest[1] = src[1];
    dest[2] = src[2];
    dest[3] = src[3];
}

/// Multiply two quaternions.
void quat_mul(Quat dest, Quat a, Quat b) {
    dest[0] = a[3] * b[0] + a[0] * b[3] + a[1] * b[2] - a[2] * b[1];
    dest[1] = a[3] * b[1] + a[1] * b[3] + a[2] * b[0] - a[0] * b[2];
    dest[2] = a[3] * b[2] + a[2] * b[3] + a[0] * b[1] - a[1] * b[0];
    dest[3] = a[3] * b[3] - a[0] * b[0] - a[1] * b[1] - a[2] * b[2];
}

/// Normalize a quaternion.
void quat_normalize(Quat quat) {
    f32 invMag = 1.f / sqrtf(quat[0] * quat[0] + quat[1] * quat[1] + quat[2] * quat[2] + quat[3] * quat[3]);
    quat[0] *= invMag;
    quat[1] *= invMag;
    quat[2] *= invMag;
    quat[3] *= invMag;
}

struct Collision gCollisions[100];
u32 gNumCollisions = 0;

struct RigidBody gRigidBodyFloor;
u8 sReverseContactPoint = FALSE;

/// Initializes a collision struct. 
struct Collision *init_collision(struct RigidBody *body1, struct RigidBody *body2) {
    struct Collision *collision = &gCollisions[gNumCollisions];
    gNumCollisions++;

    collision->body1 = body1;
    collision->body2 = body2;
    collision->numPoints = 0;
    return collision;
}

/// Adds a contact point to the given collision struct.
void add_collision(struct Collision *collision, Vec3f point, Vec3f normal, f32 penetration) {
    increment_debug_counter(&pNumCols, 1);
    f32 normalMultiplier = (sReverseContactPoint ? -1.f : 1.f);
    // Check if there is a nearby point already in the collision.
    for (u32 i = 0; i < collision->numPoints; i++) {
        Vec3f dist;
        vec3f_diff(dist, point, collision->points[i].point);
        if (vec3f_dot(dist, dist) < 0.1f) {
            vec3f_scale(dist, normal, normalMultiplier);
            if (vec3f_dot(dist, collision->points[i].normal) > 0.9f) {
                if (penetration < collision->points[i].penetration) {
                    collision->points[i].penetration = penetration;
                }
                return;
            }
        }
    }
    increment_debug_counter(&pNumColsTrunc, 1);
    struct CollisionPoint *colPoint = &collision->points[collision->numPoints];
    vec3f_copy(colPoint->point, point);
    vec3f_scale(colPoint->normal, normal, normalMultiplier);
    colPoint->penetration = penetration;
    collision->numPoints++;
}

/// Check how close a point is to a plane along the plane's normal.
f32 point_in_plane(Vec3f point, Vec3f planePoint, Vec3f normal) {
    Vec3f planeToPointRelative;
    vec3f_diff(planeToPointRelative, planePoint, point);
    return vec3f_dot(planeToPointRelative, normal);
}

/// Check if a point is inside a triangle, assuming the point is coplanar with the triangle.
s32 point_is_in_tri(Vec3f point, struct TriangleInfo *tri) {
    Vec3f edge1, edge2, edge3;
    Vec3f pointToVertex1, pointToVertex2, pointToVertex3;
    Vec3f cross1, cross2, cross3;
    f32 dot1, dot2, dot3;

    vec3f_diff(edge1, tri->vertices[1], tri->vertices[0]);
    vec3f_diff(edge2, tri->vertices[2], tri->vertices[1]);
    vec3f_diff(edge3, tri->vertices[0], tri->vertices[2]);

    vec3f_diff(pointToVertex1, point, tri->vertices[0]);
    vec3f_diff(pointToVertex2, point, tri->vertices[1]);
    vec3f_diff(pointToVertex3, point, tri->vertices[2]);

    vec3f_cross(cross1, edge1, pointToVertex1);
    vec3f_cross(cross2, edge2, pointToVertex2);
    vec3f_cross(cross3, edge3, pointToVertex3);

    dot1 = vec3f_dot(cross1, tri->normal);
    dot2 = vec3f_dot(cross2, tri->normal);
    dot3 = vec3f_dot(cross3, tri->normal);

    return (dot1 >= 0.f && dot2 >= 0.f && dot3 >= 0.f) || (dot1 <= 0.f && dot2 <= 0.f && dot3 <= 0.f);
}


/// Check if a point is inside a quad.
s32 point_is_in_quad(Vec3f point, struct QuadInfo *quad) {
    Vec3f edge1, edge2, edge3, edge4;
    Vec3f pointToVertex1, pointToVertex2, pointToVertex3, pointToVertex4;
    Vec3f cross1, cross2, cross3, cross4;
    f32 dot1, dot2, dot3, dot4;

    vec3f_diff(edge1, quad->vertices[1], quad->vertices[0]);
    vec3f_diff(edge2, quad->vertices[2], quad->vertices[1]);
    vec3f_diff(edge3, quad->vertices[3], quad->vertices[2]);
    vec3f_diff(edge4, quad->vertices[0], quad->vertices[3]);

    vec3f_diff(pointToVertex1, point, quad->vertices[0]);
    vec3f_diff(pointToVertex2, point, quad->vertices[1]);
    vec3f_diff(pointToVertex3, point, quad->vertices[2]);
    vec3f_diff(pointToVertex4, point, quad->vertices[3]);

    vec3f_cross(cross1, edge1, pointToVertex1);
    vec3f_cross(cross2, edge2, pointToVertex2);
    vec3f_cross(cross3, edge3, pointToVertex3);
    vec3f_cross(cross4, edge4, pointToVertex4);

    dot1 = vec3f_dot(cross1, quad->normal);
    dot2 = vec3f_dot(cross2, quad->normal);
    dot3 = vec3f_dot(cross3, quad->normal);
    dot4 = vec3f_dot(cross4, quad->normal);

    return (dot1 >= 0.f && dot2 >= 0.f && dot3 >= 0.f && dot4 >= 0.f) || (dot1 <= 0.f && dot2 <= 0.f && dot3 <= 0.f && dot4 <= 0.f);
}

u32 rigid_bodies_near(struct RigidBody *body1, struct RigidBody *body2) {
    if (body1->maxCorner[0] < body2->minCorner[0]) return FALSE;
    if (body1->minCorner[0] > body2->maxCorner[0]) return FALSE;
    if (body1->maxCorner[1] < body2->minCorner[1]) return FALSE;
    if (body1->minCorner[1] > body2->maxCorner[1]) return FALSE;
    if (body1->maxCorner[2] < body2->minCorner[2]) return FALSE;
    if (body1->minCorner[2] > body2->maxCorner[2]) return FALSE;
    return TRUE;
}

/// Find the point of intersection between a line and a plane. Returns FALSE if the line doesn't intersect.
s32 edge_intersects_plane(Vec3f intersectionPoint, Vec3f edgePoint1, Vec3f edgePoint2, Vec3f planePoint, Vec3f planeNormal) {
    Vec3f lineDir, relPlane;
    // Find the point of intersection.
    vec3f_diff(lineDir, edgePoint2, edgePoint1);
    f32 dot = vec3f_dot(planeNormal, lineDir);
    if (absf(dot) < 0.1f) return FALSE;
    vec3f_diff(relPlane, planePoint, edgePoint1);
    dot = vec3f_dot(planeNormal, relPlane) / dot;
    if (dot < 0.f || dot > 1.f) return FALSE;
    vec3f_scale(intersectionPoint, lineDir, dot);
    vec3f_add(intersectionPoint, edgePoint1);
    return TRUE;
}


/// Check if a mesh's vertices are intersecting a triangle's face.
void vertices_vs_tri_face(Vec3f vertices[], u32 numVertices, struct TriangleInfo *tri, struct Collision *col) {
    //increment_debug_counter(&pNumVertexChecks, numVertices);
    
    

    for (u32 i = 0; i < numVertices; i++) {
        f32 distance = point_in_plane(vertices[i], tri->vertices[0], tri->normal);
        if (distance <= PENETRATION_MIN_DEPTH || distance >= PENETRATION_MAX_DEPTH) continue;
        if (point_is_in_tri(vertices[i], tri)) {
            add_collision(col, vertices[i], tri->normal, distance);
        }
    }
}

void vertex_vs_tri_face(Vec3f vertex, struct TriangleInfo *tri, struct Collision *col) {
    //increment_debug_counter(&pNumVertexChecks, numVertices);
    
    f32 distance = point_in_plane(vertex, tri->vertices[0], tri->normal);
    if (distance <= PENETRATION_MIN_DEPTH || distance >= PENETRATION_MAX_DEPTH) {
        return;
    }
    if (point_is_in_tri(vertex, tri)) {
        add_collision(col, vertex, tri->normal, distance);
    }
    return;
}

#define vec3f_mag(a) sqrtf(vec3f_dot(a, a))

void ball_vs_corner(Vec3f center, f32 size, Vec3f corner, struct Collision *col) {
    // Check if closest point is within ball
    Vec3f dist;
    vec3f_diff(dist, center, corner);
    f32 distMag = vec3f_mag(dist);
    if (distMag > size) return;
    vec3f_normalize(dist);

    // Find collision point
    Vec3f colPoint;
    vec3f_scale(colPoint, dist, -size);
    vec3f_add(colPoint, center);

    add_collision(col, colPoint, dist, size - distMag);
}

void ball_vs_edge(Vec3f center, f32 size, Vec3f edge1, Vec3f edge2, struct Collision *col) {
    // Determine closest point on edge to center of ball
    Vec3f ab, ap;

    vec3f_diff(ab, edge2, edge1);
    vec3f_diff(ap, center, edge1);
    f32 t = vec3f_dot(ab, ap) / vec3f_dot(ab, ab);
    if (t < 0.f || t > 1.f) return;
    Vec3f closestPoint;
    vec3f_scale(closestPoint, ab, t);
    vec3f_add(closestPoint, edge1);

    ball_vs_corner(center, size, closestPoint, col);
}


/// Check if a mesh's vertices are intersecting a quad's face.
void vertices_vs_quad_face(Vec3f vertices[], u32 numVertices, struct QuadInfo *quad, struct Collision *col) {
    increment_debug_counter(&pNumVertexChecks, numVertices);
    for (u32 i = 0; i < numVertices; i++) {
        f32 distance = point_in_plane(vertices[i], quad->vertices[0], quad->normal);
        if (distance <= PENETRATION_MIN_DEPTH || distance >= PENETRATION_MAX_DEPTH) continue;
        if (point_is_in_quad(vertices[i], quad)) {
            add_collision(col, vertices[i], quad->normal, distance);
        }
    }
}

/// Check if a mesh's edges are intersecting an edge belonging to a face.
void edges_vs_edge(Vec3f vertices[], MeshEdge edges[], u32 numEdges, Vec3f edgePoint1, Vec3f edgePoint2, Vec3f edgeNormal, struct Collision *col) {
    Vec3f temp, edge, closestPointOnEdge, planeNormal, intersectionPoint;
    
    vec3f_diff(edge, edgePoint2, edgePoint1);
    vec3f_cross(planeNormal, edgeNormal, edge);
    vec3f_normalize(planeNormal);
    increment_debug_counter(&pNumEdgeChecks, numEdges);
    for (u32 i = 0; i < numEdges; i++) {
        if (edge_intersects_plane(intersectionPoint, vertices[edges[i][0]], vertices[edges[i][1]], edgePoint1, planeNormal)) {
            // Find distance from intersection point to edge
            vec3f_diff(temp, edgePoint1, intersectionPoint);
            f32 distance = vec3f_dot(temp, edgeNormal);
            if (distance <= PENETRATION_MIN_DEPTH || distance >= PENETRATION_MAX_DEPTH) continue;

            // Find closest point to intersection point on edge
            vec3f_scale(closestPointOnEdge, edgeNormal, -distance);
            vec3f_add(closestPointOnEdge, intersectionPoint);

            // Check that closest point is on line segment
            vec3f_diff(temp, closestPointOnEdge, edgePoint1);
            f32 dot = vec3f_dot(temp, edge);
            if (dot < 0.f || dot > vec3f_dot(edge, edge)) continue;

            add_collision(col, intersectionPoint, edgeNormal, distance);
        }
    }
}

void tris_vs_vertex(struct TriangleInfo tris[], u32 numTris, Vec3f point, Vec3f vertexNormal, struct Collision *col) {
    increment_debug_counter(&pNumFaceChecks, numTris);
    for (u32 i = 0; i < numTris; i++) {
        Vec3f edge1;
        vec3f_diff(edge1, point, tris[i].vertices[0]);
        f32 distance = vec3f_dot(tris[i].normal, vertexNormal);
        if (distance == 0.f) continue;
        distance = vec3f_dot(tris[i].normal, edge1) / distance;
        if (distance <= PENETRATION_MIN_DEPTH || distance >= PENETRATION_MAX_DEPTH) continue;

        if (point_is_in_tri(point, &tris[i])) {
            Vec3f colPoint;
            vec3f_scale(colPoint, vertexNormal, -distance);
            vec3f_add(colPoint, point);
            add_collision(col, colPoint, vertexNormal, distance);
        }
    }
}

void quads_vs_vertex(struct QuadInfo quads[], u32 numQuads, Vec3f point, Vec3f vertexNormal, struct Collision *col) {
    increment_debug_counter(&pNumFaceChecks, numQuads);
    for (u32 i = 0; i < numQuads; i++) {
        Vec3f edge1;
        vec3f_diff(edge1, point, quads[i].vertices[0]);
        f32 distance = vec3f_dot(quads[i].normal, vertexNormal);
        if (distance == 0.f) continue;
        distance = vec3f_dot(quads[i].normal, edge1) / distance;
        if (distance <= PENETRATION_MIN_DEPTH || distance >= PENETRATION_MAX_DEPTH) continue;

        if (point_is_in_quad(point, &quads[i])) {
            Vec3f colPoint;
            vec3f_scale(colPoint, vertexNormal, -distance);
            vec3f_add(colPoint, point);
            add_collision(col, colPoint, vertexNormal, distance);
        }
    }
}


// Buffer to store the locations of each vertex of the current rigid body in world space.
static Vec3f sCurrentVertices[50];
static Vec3f sCurrentVertices2[50];

static struct TriangleInfo sCurrentTris[50];
static struct TriangleInfo sCurrentTris2[50];

static struct QuadInfo sCurrentQuads[50];
static struct QuadInfo sCurrentQuads2[50];

/// Transform all the vertices of the current rigid body.
void calculate_mesh(struct RigidBody *body, Vec3f vertices[], struct TriangleInfo tris[], struct QuadInfo quads[]) {

    //the ball has a 600 unit box of collision detection
    if (body->mesh->numVertices == 0) {
        body->minCorner[0] = body->transform[3][0] - 300.0f;
        body->minCorner[1] = body->transform[3][1] - 300.0f;
        body->minCorner[2] = body->transform[3][2] - 300.0f;
        body->maxCorner[0] = body->transform[3][0] + 300.0f;
        body->maxCorner[1] = body->transform[3][1] + 300.0f;
        body->maxCorner[2] = body->transform[3][2] + 300.0f;
        return;
    }

    // Calculate vertices
    vec3f_set(body->minCorner,  1000000.f,  1000000.f,  1000000.f);
    vec3f_set(body->maxCorner, -1000000.f, -1000000.f, -1000000.f);
    for (u32 i = 0; i < body->mesh->numVertices; i++) {
        Vec3f vertex;
        vec3f_copy(vertex, body->mesh->vertices[i]);
        vec3f_mul(vertex, body->size);
        linear_mtxf_mul_vec3f_and_translate(body->transform, vertices[i], vertex);       

        for (u32 j = 0; j < 3; j++) {
            if (vertices[i][j] < body->minCorner[j]) body->minCorner[j] = vertices[i][j];
            if (vertices[i][j] > body->maxCorner[j]) body->maxCorner[j] = vertices[i][j];
        }

        
    }

    if (body->parentBody) {
        body->attachPoint[3][0] = vertices[7][0];
            body->attachPoint[3][1] = vertices[7][1];
            body->attachPoint[3][2] = vertices[7][2];
    }
    else {
        for (int i = 0; i < 5; i++) {
            body->attachPoint[i][0] = vertices[i + 4][0];
            body->attachPoint[i][1] = vertices[i + 4][1];
            body->attachPoint[i][2] = vertices[i + 4][2];
        }
    }

    
    Vec3f edge1, edge2;
    // Calculate tris
    for (u32 i = 0; i < body->mesh->numTris; i++) {
        vec3f_copy(tris[i].vertices[0], vertices[body->mesh->tris[i][0]]);
        vec3f_copy(tris[i].vertices[1], vertices[body->mesh->tris[i][1]]);
        vec3f_copy(tris[i].vertices[2], vertices[body->mesh->tris[i][2]]);

        vec3f_diff(edge1, tris[i].vertices[1], tris[i].vertices[0]);
        vec3f_diff(edge2, tris[i].vertices[2], tris[i].vertices[0]);
        vec3f_cross(tris[i].normal, edge1, edge2);
        vec3f_normalize(tris[i].normal);
    }
    // Calculate quads
    for (u32 i = 0; i < body->mesh->numQuads; i++) {
        vec3f_copy(quads[i].vertices[0], vertices[body->mesh->quads[i][0]]);
        vec3f_copy(quads[i].vertices[1], vertices[body->mesh->quads[i][1]]);
        vec3f_copy(quads[i].vertices[2], vertices[body->mesh->quads[i][2]]);
        vec3f_copy(quads[i].vertices[3], vertices[body->mesh->quads[i][3]]);

        vec3f_diff(edge1, quads[i].vertices[1], quads[i].vertices[0]);
        vec3f_diff(edge2, quads[i].vertices[2], quads[i].vertices[0]);
        vec3f_cross(quads[i].normal, edge1, edge2);
        vec3f_normalize(quads[i].normal);
    }

}

/// Determine if a rigid body is near a triangle.
s32 is_body_near_tri(struct RigidBody *body, struct TriangleInfo *tri) {
    s32 maxTriX = MAX(tri->vertices[0][0], MAX(tri->vertices[1][0], tri->vertices[2][0]));
    s32 minTriX = MIN(tri->vertices[0][0], MIN(tri->vertices[1][0], tri->vertices[2][0]));
    s32 maxTriY = MAX(tri->vertices[0][1], MAX(tri->vertices[1][1], tri->vertices[2][1]));
    s32 minTriY = MIN(tri->vertices[0][1], MIN(tri->vertices[1][1], tri->vertices[2][1]));
    s32 maxTriZ = MAX(tri->vertices[0][2], MAX(tri->vertices[1][2], tri->vertices[2][2]));
    s32 minTriZ = MIN(tri->vertices[0][2], MIN(tri->vertices[1][2], tri->vertices[2][2]));

    if (body->minCorner[0] > maxTriX) return FALSE;
    if (body->maxCorner[0] < minTriX) return FALSE;
    if (body->minCorner[1] > maxTriY) return FALSE;
    if (body->maxCorner[1] < minTriY) return FALSE;
    if (body->minCorner[2] > maxTriZ) return FALSE;
    if (body->maxCorner[2] < minTriZ) return FALSE;
    return TRUE;
}

/// Check for collisions between a rigid body and a static floor triangle.
void body_vs_surface_collision(struct RigidBody *body, struct Surface *tri, struct Collision *col) {
    struct TriangleInfo triInfo;
    vec3s_to_vec3f(triInfo.vertices[0], tri->vertex1);
    vec3s_to_vec3f(triInfo.vertices[1], tri->vertex2);
    vec3s_to_vec3f(triInfo.vertices[2], tri->vertex3);
    vec3f_copy(triInfo.normal, &tri->normal.x);
    if (!is_body_near_tri(body, &triInfo)) return;
    increment_debug_counter(&pNumTrisChecked, 1);
    struct MeshInfo *mesh = body->mesh;

    u32 prevCollisions = col->numPoints;

    // Collision detection
    if (body->mesh->numVertices == 0) {
        Vec3f pos;
        pos[0] = body->transform[3][0] - (tri->normal.x * 90.0f);
        pos[1] = body->transform[3][1] - (tri->normal.y * 90.0f);
        pos[2] = body->transform[3][2] - (tri->normal.z * 90.0f);
        vertex_vs_tri_face(&pos, &triInfo, col);
    } else {
        vertices_vs_tri_face(sCurrentVertices, mesh->numVertices, &triInfo, col);
    }


    //ball_vs_edge(body->centerOfMass, 90.f, triInfo.vertices[0], triInfo.vertices[1], col);
    //ball_vs_edge(body->centerOfMass, 90.f, triInfo.vertices[1], triInfo.vertices[2], col);
    //ball_vs_edge(body->centerOfMass, 90.f, triInfo.vertices[2], triInfo.vertices[0], col);

    //ball_vs_corner(body->centerOfMass, 90.f, triInfo.vertices[0], col);
    //ball_vs_corner(body->centerOfMass, 90.f, triInfo.vertices[1], col);
    //ball_vs_corner(body->centerOfMass, 90.f, triInfo.vertices[2], col);

    if (col->numPoints - prevCollisions < 4) {
        for (u32 i = 0; i < 3; i++) {
            edges_vs_edge(sCurrentVertices, mesh->edges, mesh->numEdges, triInfo.vertices[i], triInfo.vertices[i == 2 ? 0 : i + 1], triInfo.normal, col);
        }
    }
    
    if (mesh->numTris > 0) {
        for (u32 i = 0; i < 3; i++) {
            tris_vs_vertex(sCurrentTris, mesh->numTris, triInfo.vertices[i], triInfo.normal, col);
        }
    }

    if (mesh->numQuads > 0) {
        for (u32 i = 0; i < 3; i++) {
            quads_vs_vertex(sCurrentQuads, mesh->numQuads, triInfo.vertices[i], triInfo.normal, col);
        }
    }

}

f32 find_floor(f32 x, f32 y, f32 z, struct Surface **floor);

/// Checks for collisions for the current rigid body.
void rigid_body_check_surf_collisions(struct RigidBody *body) {
    if (body->isStatic || body->asleep) {
        return;
    }

    struct Collision *col = init_collision(body, &gRigidBodyFloor);
    s32 minCellX = GET_CELL_COORD(body->minCorner[0]);
    s32 minCellZ = GET_CELL_COORD(body->minCorner[2]);
    s32 maxCellX = GET_CELL_COORD(body->maxCorner[0]);
    s32 maxCellZ = GET_CELL_COORD(body->maxCorner[2]);
    // Iterate over all triangles
    for (s32 cellZ = minCellZ; cellZ <= maxCellZ; cellZ++) {
        for (s32 cellX = minCellX; cellX <= maxCellX; cellX++) {
            for (u32 i = 0; i < 3; i++) {
                struct SurfaceNode *node = gStaticSurfacePartition[cellZ][cellX][i].next;
                while (node != NULL) {
                    body_vs_surface_collision(body, node->surface, col);
                    node = node->next;
                }

                node = gDynamicSurfacePartition[cellZ][cellX][i].next;
                while (node != NULL) {
                    if (node->surface->object->rigidBody == NULL) {
                        body_vs_surface_collision(body, node->surface, col);
                    }
                    node = node->next;
                }

            }
        }
    }
}

void rigid_body_check_body_collisions(struct RigidBody *body1, struct RigidBody *body2) {
    if ((body1->isStatic || body1->asleep) && (body2->isStatic || body2->asleep)) {
        return;
    }

    calculate_mesh(body2, sCurrentVertices2, sCurrentTris2, sCurrentQuads2);
    if (!rigid_bodies_near(body1, body2)) {
        return;
    }
    struct Collision *col = init_collision(body1, body2);

    // Body 1 verts vs body 2 tris
    if (body2->mesh->numTris > 0) {
        for (u32 i = 0; i < body2->mesh->numTris; i++) {
            // Check for collisions
            vertices_vs_tri_face(sCurrentVertices, body1->mesh->numVertices, &sCurrentTris2[i], col);
        }
    }
    // Body 1 verts and edges vs body 2 quads
    if (body2->mesh->numQuads > 0) {
        for (u32 i = 0; i < body2->mesh->numQuads; i++) {
            // Check for vertex collisions
            vertices_vs_quad_face(sCurrentVertices, body1->mesh->numVertices, &sCurrentQuads2[i], col);
        
            // Check for edge collisions
            for (u32 j = 0; j < 4; j++) {
                edges_vs_edge(sCurrentVertices, body1->mesh->edges, body1->mesh->numEdges,
                    sCurrentVertices2[body2->mesh->quads[i][j]], sCurrentVertices2[body2->mesh->quads[i][j == 3 ? 0 : j + 1]],
                    sCurrentQuads2[i].normal, col);
            }
        }
    }

    sReverseContactPoint = TRUE;
    // Body 1 tris vs body 2 verts
    if (body1->mesh->numTris > 0) {
        for (u32 i = 0; i < body1->mesh->numTris; i++) {
            // Check for collisions
            vertices_vs_tri_face(sCurrentVertices2, body2->mesh->numVertices, &sCurrentTris[i], col);
        }
    }

    // Body 1 quads vs body 2 verts
    if (body1->mesh->numQuads > 0) {
        for (u32 i = 0; i < body1->mesh->numQuads; i++) {
            // Check for collisions
            vertices_vs_quad_face(sCurrentVertices2, body2->mesh->numVertices, &sCurrentQuads[i], col);
        }
    }
    sReverseContactPoint = FALSE;
}

void rigid_body_do_body_collision(u32 bodyIndex) {
    struct RigidBody *body = &gRigidBodies[bodyIndex];
    if (!body->allocated) return;

    calculate_mesh(body, sCurrentVertices, sCurrentTris, sCurrentQuads);
    for (u32 j = bodyIndex + 1; j < MAX_RIGID_BODIES; j++) {
        if (gRigidBodies[j].allocated && !gRigidBodies[j].colOnly) {
            rigid_body_check_body_collisions(body, &gRigidBodies[j]);
        }
    }
}

Vec3f floornormal = {0.f,1.f,0.f};

void rigid_body_do_collision(u32 bodyIndex) {
    struct RigidBody *body = &gRigidBodies[bodyIndex];
    if (!body->allocated) return;

    calculate_mesh(body, sCurrentVertices, sCurrentTris, sCurrentQuads);

    if (body->hardcodedFloor) {
        //Hardcoded Floor Collision
        struct Collision *col = init_collision(body, &gRigidBodyFloor);
        // Iterate over vertices
        for (u32 i = 0; i < body->mesh->numVertices; i++) {
            f32 y = sCurrentVertices[i][1];
            if (y < 1820.f) {
                add_collision(col, sCurrentVertices[i], floornormal, 1820.f - y);
            }
        }
    } else {
        // Regular Collision Routine
        rigid_body_check_surf_collisions(body);
    }
}

/// Resolve a collision impulse between two rigid bodies.
void rigid_body_collision_impulse(struct RigidBody *body1, Vec3f hitPoint, Vec3f normal, f32 penetration) {
    f32 body1InvMass = body1->invMass;
    Vec3f body1Linear, body1Angular;

    if (body1->isStatic) {
        return;
    }

    increment_debug_counter(&pNumImpulsesApplied, 1);

    vec3f_set(body1Linear, 0.f, 0.f, 0.f);
    vec3f_set(body1Angular, 0.f, 0.f, 0.f);

    // Determine the relative velocity (dv) of the two bodies at the point of impact.
    Vec3f r0, v0, dv;
    vec3f_diff(r0, hitPoint, body1->centerOfMass);
    vec3f_cross(v0, r0, body1->angularVel);
    vec3f_add(v0, body1->linearVel);
    vec3f_copy(dv, v0);

    // Normal Impulse Code
    Vec3f temp1_1, temp1_2;
    vec3f_cross(temp1_1, normal, r0);
    linear_mtxf_mul_vec3f(body1->invInertia, temp1_2, temp1_1);
    vec3f_cross(temp1_1, r0, temp1_2);

    f32 kNormal = body1InvMass + vec3f_dot(normal, temp1_1);

    f32 vn = vec3f_dot(dv, normal);
    f32 bias = PENETRATION_BIAS * MAX(0.f, penetration - PENETRATION_MIN_DEPTH) * NUM_RIGID_BODY_STEPS;

    f32 dPn = MAX(((-vn + bias) / kNormal), 0.f);

    Vec3f P;
    vec3f_scale(P, normal, dPn);

    // Apply impulse
    vec3f_add_scaled(body1Linear, P, body1InvMass);
    vec3f_cross(temp1_1, P, r0);
    linear_mtxf_mul_vec3f(body1->invInertia, temp1_2, temp1_1);
    vec3f_add(body1Angular, temp1_2);

    // Tangent Impulse Code (friction)
    {
        Vec3f tangent;
        tangent[0] = dv[0] - normal[0] * vn;
        tangent[1] = dv[1] - normal[1] * vn;
        tangent[2] = dv[2] - normal[2] * vn;
        vec3f_normalize(tangent);
        f32 vt = vec3f_dot(dv, tangent);

        vec3f_cross(temp1_1, tangent, r0);
        linear_mtxf_mul_vec3f(body1->invInertia, temp1_2, temp1_1);
        vec3f_cross(temp1_1, r0, temp1_2);

        f32 kTangent = body1InvMass + vec3f_dot(tangent, temp1_1);

        f32 maxPt = FRICTION * dPn;
        f32 dPt = CLAMP(((-vt + bias) / kTangent), -maxPt, maxPt);

        vec3f_scale(P, tangent, dPt);

        // Apply impulse
        vec3f_add_scaled(body1Linear, P, body1InvMass);
        vec3f_cross(temp1_1, P, r0);
        linear_mtxf_mul_vec3f(body1->invInertia, temp1_2, temp1_1);
        vec3f_add(body1Angular, temp1_2);
    }


    vec3f_add(body1->linearVel, body1Linear);
    vec3f_add(body1->linearDisplacement, body1Linear);
    vec3f_add(body1->angularVel, body1Angular);
    vec3f_add(body1->angularDisplacement, body1Angular);


    //Child->Parent body impulses
    if (body1->parentBody) {
        Vec3f diminishedLinear;
        vec3f_copy(diminishedLinear, body1Linear);
        diminishedLinear[0] /= 5;
        diminishedLinear[1] /= 5;
        diminishedLinear[2] /= 5;
        vec3f_add(body1->parentBody->linearVel, diminishedLinear);

        Vec3f diminishedAngular;
        vec3f_copy(diminishedAngular, body1Angular);
        diminishedAngular[0] /= 8;
        diminishedAngular[1] /= 8;
        diminishedAngular[2] /= 8;
        vec3f_add(body1->parentBody->angularVel, diminishedAngular);
    }
}

/// Updates the rigid body's transformation matrix and its inertia tensor.
void rigid_body_update_matrix(struct RigidBody *body) {
    mtxf_from_quat(body->angleQuat, body->transform);

    // Calculate the inverse of the inertia tensor.
    // will need to be modified a ton for rigid bodies that aren't uniform size in all dimensions
	f32 x2 = body->size[0] * body->size[0];
    f32 y2 = body->size[1] * body->size[1];
    f32 z2 = body->size[2] * body->size[2];
    f32 ix = 2.f / ((y2 + z2) * body->mass);
    f32 iy = 2.f / ((x2 + z2) * body->mass);
    f32 iz = 2.f / ((x2 + y2) * body->mass);
    mtxf_identity(body->invInertia);
    body->invInertia[0][0] = ix;
    body->invInertia[1][1] = iy;
    body->invInertia[2][2] = iz;

    Mat4 tmp;
    mtxf_mul(tmp, body->transform, body->invInertia);
    mtxf_mul_transpose(body->invInertia, tmp, body->transform);

    vec3f_copy(body->transform[3], body->centerOfMass);
}

struct RigidBody *allocate_rigid_body_from_object(struct Object *obj, struct MeshInfo *mesh, f32 mass, Vec3f size, u8 hc_floor) {
    Vec3f pos;
    //vec3f_set(obj->rigidBodyOffset, xOffset, yOffset, zOffset);
    vec3f_copy(pos, &obj->oPosVec);
    //vec3f_sub(pos, obj->rigidBodyOffset);
    struct RigidBody *body = allocate_rigid_body(mesh, mass, size, pos);
    rigid_body_set_yaw(body, obj->oFaceAngleYaw);
    body->obj = obj;
    obj->rigidBody = body;
    body->hardcodedFloor = hc_floor;
    return body;
}

/// Allocate a rigid body and return a pointer to it.
struct RigidBody *allocate_rigid_body(struct MeshInfo *mesh, f32 mass, Vec3f size, Vec3f pos) {
    // Search list for deallocated rigid body
    for (u32 i = 0; i < MAX_RIGID_BODIES; i++) {
        struct RigidBody *body = &gRigidBodies[i];
        if (!body->allocated) {
            body->allocated = TRUE;
            body->hasGravity = TRUE;
            body->asleep = FALSE;
            body->mesh = mesh;
            body->mass = mass;
            if (mass != 0.f) {
                body->invMass = 1.0f / mass;
                body->isStatic = FALSE;
            } else {
                body->invMass = 0.0f;
                body->isStatic = TRUE;
            }
            vec3f_copy(body->size, size);
            vec3f_copy(body->centerOfMass, pos);
            body->angleQuat[0] = 0.0f;
            body->angleQuat[1] = 0.0f;
            body->angleQuat[2] = 0.0f;
            body->angleQuat[3] = 1.0f;
            body->motion = 10.0f;
            vec3f_set(body->linearVel, 0.0f, 0.0f, 0.0f);
            vec3f_set(body->angularVel, 0.0f, 0.0f, 0.0f);
            vec3f_set(body->netForce, 0.0f, 0.0f, 0.0f);
            vec3f_set(body->netTorque, 0.0f, 0.0f, 0.0f);

            body->obj = NULL;
            rigid_body_update_matrix(body);
            return body;
        }
    }
    return NULL;
}

void rigid_body_set_yaw(struct RigidBody *body, s16 yaw) {

    struct RigidBody *con = body;

    if (con) {
        if (con->maxYaw && con->minYaw) {
            if (yaw < con->minYaw) {
                yaw = con->minYaw;
            }
            if (yaw > con->maxYaw) {
                yaw = con->maxYaw;
            }
        }
    }

    s32 ang = (-yaw) >> 1;
    body->angleQuat[0] = 0.f;
    body->angleQuat[1] = sins(ang);
    body->angleQuat[2] = 0.f;
    body->angleQuat[3] = coss(ang);
    rigid_body_update_matrix(body);
}

/// Deallocates a rigid body.
void deallocate_rigid_body(struct RigidBody *body) {
    body->allocated = FALSE;
}

void rigid_body_set_pitch(struct RigidBody *body, s16 pitch) {

    struct RigidBody *con = body;

    if (con) {
        if (con->maxPitch && con->minPitch) {
            if (pitch < con->minPitch) {
                pitch = con->minPitch;
            }
            if (pitch > con->maxPitch) {
                pitch = con->maxPitch;
            }
        }
    }

    s32 ang = (-pitch) >> 1;
    body->angleQuat[0] = sins(ang);
    body->angleQuat[1] = 0.f;
    body->angleQuat[2] = 0.f;
    body->angleQuat[3] = coss(ang);
    rigid_body_update_matrix(body);
}

/// Applies a force to a rigid body at a given point.
void rigid_body_add_force(struct RigidBody *body, Vec3f contactPoint, Vec3f force, u32 wake) {
    // Calculate force
    vec3f_add(body->netForce, force);
    
    // Calculate torque
    // τ = r x F
    Vec3f torque, contactOffset;
    vec3f_copy(contactOffset, contactPoint);
    vec3f_sub(contactOffset, body->centerOfMass);
    vec3f_cross(torque, force, contactOffset);
    vec3f_add(body->netTorque, torque);

    if (wake) {
        body->asleep = FALSE;
        body->motion = 10.f;
    }
}

void rigid_body_apply_displacement(struct RigidBody *body, Vec3f linear, Vec3f angular, u8 type) {
    //keep ridge bodies inbounds
    struct Surface *floor;
    find_floor(body->transform[3][0]+linear[0],body->transform[3][1]+linear[1],body->transform[3][2]+linear[2],&floor);
    if (!floor) {
        play_sound(SOUND_ACTION_BONK, body->obj->header.gfx.cameraToObject);
        linear[0] = -linear[0];
        linear[1] = -linear[1];
        linear[2] = -linear[2];
        vec3f_normalize(linear);
    }

    if (body->parentBody && type == 0) {
        Vec3f multiplier = {0.86f, 0.86f, 0.96f};
        Vec3f angMultiplier = {0.86f, 0.86f, 0.96f};
        vec3f_mul(linear, multiplier);
        vec3f_mul(angular, angMultiplier);
    }
    if (body->parentBody && type == 1) {
        Vec3f multiplier = {0.82f, 0.82f, 0.82f};
        Vec3f angMultiplier = {0.92f, 0.92f, 0.92f};
        vec3f_mul(linear, multiplier);
        vec3f_mul(angular, angMultiplier);
    }


    // Apply linear velocity
    // Δx = v * Δt
    vec3f_add_scaled(body->centerOfMass, linear, dt);
    
    // Apply angular velocity
    // Δθ = ω * Δt
    Quat angleChange;
    f32 norm = sqrtf(angular[0] * angular[0] + angular[1] * angular[1] + angular[2] * angular[2]);
    norm *= dt;

    if (norm > 0.0001f) {
        // Create quaternion from angular velocity
        f32 sinVal = sinf(norm * 0.5f) / norm;
        f32 cosVal = cosf(norm * 0.5f);

        angleChange[0] = angular[0] * dt * sinVal;
        angleChange[1] = angular[1] * dt * sinVal;
        angleChange[2] = angular[2] * dt * sinVal;
        angleChange[3] = cosVal;

        // Apply and normalize quaternion
        Quat curRot;
        quat_copy(curRot, body->angleQuat);
        quat_mul(body->angleQuat, curRot, angleChange);
        quat_normalize(body->angleQuat);
    }

    rigid_body_update_matrix(body);
}

/// Updates the position of a rigid body based on its velocity.
void rigid_body_update_position_from_velocity(struct RigidBody *body) {
    if (body->isStatic || body->asleep) {
        return;
    }

    rigid_body_apply_displacement(body, body->linearVel, body->angularVel, 0);

    // Reset forces and torques
    vec3f_set(body->netForce, 0.0f, 0.0f, 0.0f);
    vec3f_set(body->netTorque, 0.0f, 0.0f, 0.0f);

    vec3f_set(body->linearDisplacement, 0.f, 0.f, 0.f);
    vec3f_set(body->angularDisplacement, 0.f, 0.f, 0.f);
}

void rigid_body_update_position_from_collisions(struct RigidBody *body) {
    if (body->isStatic || body->asleep) {
        return;
    }
    
    rigid_body_apply_displacement(body, body->linearDisplacement, body->angularDisplacement, 1);

    vec3f_set(body->linearDisplacement, 0.f, 0.f, 0.f);
    vec3f_set(body->angularDisplacement, 0.f, 0.f, 0.f);
    f32 motion = vec3f_dot(body->linearVel, body->linearVel) + vec3f_dot(body->angularVel, body->angularVel);
    body->motion = SLEEP_DETECTION_BIAS * body->motion + (1.f - SLEEP_DETECTION_BIAS) * motion;

    if (body->motion < SLEEP_DETECTION_THRESHOLD) {
        body->asleep = TRUE;
        vec3f_set(body->linearVel, 0.0f, 0.0f, 0.0f);
        vec3f_set(body->angularVel, 0.0f, 0.0f, 0.0f);
    }
}

void rigid_body_update_obj(struct RigidBody *body) {
    if (body->obj != NULL) {
        mtxf_copy(body->obj->transform, body->transform);
        vec3f_copy(&body->obj->oPosVec, body->obj->transform[3]);
        body->obj->header.gfx.throwMatrix = &body->obj->transform;
    }
}

#include "game_init.h"
#include "game/level_update.h"

void rigid_body_apply_gravity(struct RigidBody *body) {
    Vec3f gravityForce;
    vec3f_set(gravityForce, 0.f, GRAVITY_FORCE * body->mass, 0.f);
    rigid_body_add_force(body, body->centerOfMass, gravityForce, FALSE);
}

/// Updates the velocity of a rigid body.
void rigid_body_update_velocity(struct RigidBody *body) {
    if (body->isStatic || body->asleep) {
        return;
    }

    // Apply Gravity
    // Fg = m * g
    if (body->hasGravity) {
        rigid_body_apply_gravity(body);
    }

    

    // Calculate linear velocity
    // Δv = (F / m) * Δt
    vec3f_add_scaled(body->linearVel, body->netForce, body->invMass * dt);

    // Calculate angular velocity
    // Δω = (τ / I) * Δt
    Vec3f angularAccel;
    linear_mtxf_mul_vec3f(body->invInertia, angularAccel, body->netTorque);
    vec3f_add_scaled(body->angularVel, angularAccel, dt);

    // Damping
    vec3f_scale(body->linearVel, body->linearVel, DAMPING);
    vec3f_scale(body->angularVel, body->angularVel, DAMPING);

    if (body->parentBody) {
        //vec3f_set(body->linearVel, 0.0f, 0.0f, 0.0f);
        //vec3f_set(body->angularVel, 0.0f, 0.0f, 0.0f);
    }

    rigid_body_update_matrix(body);
}

/// Apply impulses to rigid bodies to resolve stored collisions.
void apply_impulses(void) {
    for (u32 i = 0; i < gNumCollisions; i++) {
        struct Collision *col = &gCollisions[i];

        for (u32 j = 0; j < col->numPoints; j++) {
            struct CollisionPoint *colPoint = &col->points[j];
            Vec3f normal;
            vec3f_copy(normal, colPoint->normal);
            vec3f_normalize(normal);
            rigid_body_collision_impulse(col->body1, colPoint->point, normal, colPoint->penetration);
        }
    }
}

/// Perform one step for the rigid body physics system.
void do_rigid_body_step(void) {
    gNumCollisions = 0;

    // Update velocity and gravity
    for (u32 i = 0; i < MAX_RIGID_BODIES; i++) {
        if (gRigidBodies[i].allocated) {
            rigid_body_update_velocity(&gRigidBodies[i]);
            rigid_body_update_position_from_velocity(&gRigidBodies[i]);
        }
    }

    // Check collisions
    for (u32 i = 0; i < MAX_RIGID_BODIES; i++) {
        rigid_body_do_collision(i);
        rigid_body_do_body_collision(i);
    }

    // Apply impulses
    for (u32 iter = 0; iter < NUM_IMPULSE_ITERATIONS; iter++) {
        apply_impulses();
    }

    // Update position
    for (u32 i = 0; i < MAX_RIGID_BODIES; i++) {
        if (gRigidBodies[i].allocated) {
            rigid_body_update_position_from_collisions(&gRigidBodies[i]);
            rigid_body_update_obj(&gRigidBodies[i]);
        }
    }

    for (u32 i = 0; i < MAX_RIGID_BODIES; i++) {
        if (gRigidBodies[i].allocated) {
            struct RigidBody *body = &gRigidBodies[i];
            
            if (body->obj && body->obj->oBehParams2ndByte > 1 && body->obj->oBehParams2ndByte < 50) {
                body->obj->rigidBody->centerOfMass[0] = body->obj->parentObj->rigidBody->attachPoint[body->obj->oBehParams2ndByte - 4][0];
                body->obj->rigidBody->centerOfMass[1] = body->obj->parentObj->rigidBody->attachPoint[body->obj->oBehParams2ndByte - 4][1];
                body->obj->rigidBody->centerOfMass[2] = body->obj->parentObj->rigidBody->attachPoint[body->obj->oBehParams2ndByte - 4][2];
                }

            
        }
    }
}

static const Vtx vertex_collision_point[] = {
    {{{  -40,   0, 0}, 0, {     0,      0}, {0xff, 0xff, 0xff, 0xff}}},
    {{{   40,   0, 0}, 0, {     0,      0}, {0xff, 0xff, 0xff, 0xff}}},
    {{{    0, 150, 0}, 0, {     0,      0}, {0xff, 0xff, 0xff, 0xff}}},

    {{{       0,      0,    -40}, 0, {     0,      0}, {0xff, 0xff, 0xff, 0xff}}},
    {{{       0,      0,     40}, 0, {     0,      0}, {0xff, 0xff, 0xff, 0xff}}},
    {{{       0,     150,     0}, 0, {     0,      0}, {0xff, 0xff, 0xff, 0xff}}},
};

static const Gfx dl_draw_collision_point_start[] = {
    gsSPClearGeometryMode(G_LIGHTING | G_CULL_BACK),
    gsDPSetEnvColor(255,0,0,255),
    gsDPSetCombineMode(G_CC_FADE, G_CC_FADE),
    gsDPSetRenderMode(G_RM_XLU_SURF, G_RM_XLU_SURF2),
    gsSPEndDisplayList(),
};

static const Gfx dl_draw_collision_point[] = {
    gsSPVertex(vertex_collision_point, 6, 0),
    gsSP1Triangle( 0,  1,  2, 0x0),
    gsSP1Triangle( 3,  4,  5, 0x0),
    gsSPEndDisplayList(),
};

static const Gfx dl_draw_collision_point_end[] = {
    gsSPSetGeometryMode(G_LIGHTING | G_CULL_BACK),
    gsDPSetRenderMode(G_RM_AA_ZB_OPA_SURF, G_RM_AA_ZB_OPA_SURF2),
    gsDPSetCombineMode(G_CC_SHADE, G_CC_SHADE),
    gsSPEndDisplayList(),
};

static void render_collision_point(struct CollisionPoint *point) {
    Mtx *mtx = alloc_display_list(sizeof(Mtx));
    Mat4 mtxf;
    Vec3f pos;
    vec3f_copy(pos, point->point);
    vec3_mul_val(pos, WORLD_SCALE);
    mtxf_align_terrain_normal(mtxf, point->normal, pos, 0);
    mtxf_to_mtx(mtx, mtxf);
    gSPMatrix(gDisplayListHead++, mtx, (G_MTX_MODELVIEW | G_MTX_PUSH));
    gSPDisplayList(gDisplayListHead++, dl_draw_collision_point);
    gSPPopMatrix(gDisplayListHead++, G_MTX_MODELVIEW);
}

void render_collision_points(void) {
    gSPDisplayList(gDisplayListHead++, dl_draw_collision_point_start);
    for (u32 i = 0; i < gNumCollisions; i++) {
        struct Collision *col = &gCollisions[i];

        for (u32 j = 0; j < col->numPoints; j++) {
            render_collision_point(&col->points[j]);
        }
    }
    gSPDisplayList(gDisplayListHead++, dl_draw_collision_point_end);
}
