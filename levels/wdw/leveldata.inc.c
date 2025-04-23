
#include "levels/wdw/texture.inc.c"
#include "levels/wdw/areas/1/1/model.inc.c"
#include "levels/wdw/areas/1/2/model.inc.c"
#include "levels/wdw/areas/2/1/model.inc.c"
#include "levels/wdw/areas/2/2/model.inc.c"
#include "levels/wdw/areas/1/3/model.inc.c"
#include "levels/wdw/double_arrows/model.inc.c" // This is weird, but the only instance of a shared area object in a level. So we're treating it as a seperate model. It does not have collision anyway so it may have been combined in both areas by some sort of optimizer at compile time.
#include "levels/wdw/areas/1/collision.inc.c"
#include "levels/wdw/areas/2/collision.inc.c"
