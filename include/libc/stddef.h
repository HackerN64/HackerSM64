#pragma once

#include "PR/ultratypes.h"

#ifndef offsetof
#define offsetof(st, m) ((size_t)&(((st *)0)->m))
#endif
