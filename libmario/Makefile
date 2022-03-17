DECOMP_INPUT_FILES := ../src/game/mario_actions_airborne.c ../src/game/mario_actions_automatic.c ../src/game/mario_actions_cutscene.c \
    ../src/game/mario_actions_moving.c ../src/game/mario_actions_object.c ../src/game/mario_actions_stationary.c ../src/game/mario_actions_submerged.c \
	../src/game/mario_misc.c ../src/game/mario_step.c ../src/game/mario.c ../src/engine/math_util.c ../src/game/object_helpers.c

all: libmario.dll libmario.so

libmario.dll: $(wildcard mariolib/*.c) $(DECOMP_INPUT_FILES)
	x86_64-w64-mingw32-gcc $^ -o $@ -shared -DNON_MATCHING -DAVOID_UB -D_LANGUAGE_C -ggdb -I.. -I../include -I../include/n64 -I../src/engine -I../src/game -I../src -Ism64 -lm -Wl,--subsystem,windows

libmario.so: $(wildcard mariolib/*.c) $(DECOMP_INPUT_FILES)
	gcc $^ -o $@ -shared -DNON_MATCHING -DAVOID_UB -D_LANGUAGE_C -ggdb -I.. -I../include -I../include/n64 -I../src/engine -I../src/game -I../src -Ism64 -lm -fPIC
