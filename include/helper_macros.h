#pragma once

#define LIST_NEXT_ITEM(curItem, type) ((type *)((s32)curItem + sizeof(type)))
