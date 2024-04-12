#ifndef S2D_OPTIMIZE_H
#define S2D_OPTIMIZE_H

struct s2d_optimized_linkedlist {
    struct s2d_optimized_linkedlist *next;
    struct s2d_optimized_linkedlist *prev;
    u8 envR;
    u8 envG;
    u8 envB;
    u8 envA;
    int glyph;
    uObjSubMtx *mtx;
    uObjSubMtx *dropshadow;
};
typedef struct s2d_optimized_linkedlist S2DListNode;

struct s2dlist {
	S2DListNode *head;
	int len;
};
typedef struct s2dlist S2DList;

#endif