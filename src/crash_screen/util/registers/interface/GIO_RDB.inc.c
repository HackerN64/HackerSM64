// -- GIO & RDB --



// -- GIO --

static const char* sRegDesc_GIO[] = {
    "unknown",
};
ALIGNED32 static const RegisterInfo sRegInfo_GIO[] = {
    [REGID_GIO_GIO_INTR ] = DEF_IREG(GIO_GIO_INTR_REG,  "INTERRUPT",      0),
    [REGID_GIO_GIO_SYNC ] = DEF_IREG(GIO_GIO_SYNC_REG,  "SYNC",           0),
    [REGID_GIO_CART_INTR] = DEF_IREG(GIO_CART_INTR_REG, "CART_INTERRUPT", 0),
};
static const RegisterSource sRegisters_GIO = DEF_REG_LIST_INTERFACE(
    "GIO",
    "Development board",
    sRegDesc_GIO,
    sRegInfo_GIO
);


// -- RDB --

static const char* sRegDesc_RDB[] = {
    "unknown",
};
ALIGNED32 static const RegisterInfo sRegInfo_RDB[] = {
    [REGID_RDB_BASE             ] = DEF_IREG(RDB_BASE_REG,          "BASE",            0),
    [REGID_RDB_WRITE_INTR       ] = DEF_IREG(RDB_WRITE_INTR_REG,    "WRITE_INTERRUPT", 0),
    [REGID_RDB_READ_INTR        ] = DEF_IREG(RDB_READ_INTR_REG,     "READ_INTERRUPT",  0),
};
static const RegisterSource sRegisters_RDB = DEF_REG_LIST_INTERFACE(
    "RDB",
    "Debug port",
    sRegDesc_RDB,
    sRegInfo_RDB
);


// -- GIO_RDB --

static const char* sRegDesc_GIO_RDB[] = {
    "unknown",
};
ALIGNED32 static const RegisterInfo sRegInfo_GIO_RDB[] = {
    [REGID_GIO_RDB_BASE      ] = DEF_IREG(GIO_RDB_BASE_REG,       "BASE",            0),
    [REGID_GIO_RDB_WRITE_INTR] = DEF_IREG(GIO_RDB_WRITE_INTR_REG, "WRITE_INTERRUPT", 0),
    [REGID_GIO_RDB_READ_INTR ] = DEF_IREG(GIO_RDB_READ_INTR_REG,  "READ_INTERRUPT",  0),
};
static const RegisterSource sRegisters_GIO_RDB = DEF_REG_LIST_INTERFACE(
    "GIO_RDB",
    "Development Board & Debug Port",
    sRegDesc_GIO_RDB,
    sRegInfo_GIO_RDB
);
