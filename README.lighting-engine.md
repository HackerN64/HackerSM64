**Lighting Engine by Wiseguy**
- Lighting Engine is NOT finished. It's a WIP. Expect issues and expect it to be somewhat hard to use. Point lights in particular are extremely finnicky. 
- In order for lighting engine to work, your materials must have Shade Color in the combiner, **but you have to DISABLE Shade Color in sources**. If you leave Shade Color ticked for even a single material, the lighting will break **for the entire mesh**. Until Fast64 gets some sort of Lighting Engine support, it's recommended you create some custom presets to make this less annoying. 
  - You can intentionally make a material fullbright by leaving Shade Color out of the combiner for that material. Useful for, say, a lamp.
- Use the function `set_directional_light` to set the directional light for the current area. It takes a `Vec3f` for the direction and three `s32` for the RGB values.
- Use the function `set_ambient_light` to set the ambient light for the current area. It takes three `s32` for the RGB values.
- You can use the function `emit_light` to emit a point light via code. It takes a `Vec3f` for the position, three `s32` for the RGB values and three `u32` for each one of the falloff types. The falloff is basically how strong the light is: the HIGHER the falloff is, the DIMMER the light will be. The three types of falloffs can be stacked, and work like this:
  - 2 for constant falloff = the point light is half as bright always.
  - 2 for linear falloff = the point light is half as bright when you double the distance
  - 2 for quadratic falloff = the point is half as bright when you double the square of the distance
- Important note regarding point lights: They are affected by WORLD_SCALE, which is set based on your extended bounds mode. Esentially, at 2x bounds lights will be twice as bright as in 1x bounds, and with 4x bounds they will be twice as bright as that. This can be dealt with simply by using high falloffs, but it can be annoying if you decide to change the extended bounds mode mid-development.
- You can also make an object emit light by using the `OBJ_FLAG_EMIT_LIGHT` object flag, as well as `SET_LIGHT_COLOR` and `SET_LIGHT_FALLOFF` in the script.


Actors ready for use with lighting engine:
- mario (it is a completely CUSTOM mario)
- breakable box
- bully
- goomba
- snowman
- white particle
- white particle small
