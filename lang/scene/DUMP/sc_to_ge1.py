"""!
BlenderFDS, export geometry to ge1 cad file format.
"""

import bpy, bmesh
from .... import utils

# GE1 file format:

# [APPEARANCE]  < immutable title
# 3             < number of appearances (from Blender materials)
# INERT                     < appearance name
# 0 200 200 50 0.0 0.0 0.5  < index, red, green, blue, twidth, theight, alpha, tx0, ty0, tz0 (t is texture)
#                           < texture file, blank if None
# Burner
# 1 255 0 0 0.0 0.0 0.5
#
# Dummy Hole                   < automatically inserted to render HOLEs
# 2 150 150 150 0.0 0.0 0.5
#
# [FACES]       < immutable title
# 2             < number of *quad* faces (from OBST and SURF objects triamgulated bm.faces)
# 6.0 3.9 0.5  6.0 1.9 0.5  6.0 1.9 1.9  6.0 3.9 1.9  0 < x0, y0, z0, x1, y1, z1, ..., ref to appearance index
# 6.0 3.9 0.5  6.0 1.9 0.5  6.0 1.9 1.9  6.0 3.9 1.9  0
# EOF


def _get_appearance(name, i, rgb):
    return f"{name}\n{i} {rgb[0]} {rgb[1]} {rgb[2]} 0. 0. {rgb[3]:.3f} 0. 0. 0.\n\n"


def scene_to_ge1(context, scene):
    """!
    Export scene geometry in FDS GE1 notation.
    @param context: the <a href="https://docs.blender.org/api/current/bpy.context.html">blender context</a>.
    @param scene: the Blender scene.
    @return FDS GE1 notation.
    """
    # Cursor
    w = context.window_manager.windows[0]
    w.cursor_modal_set("WAIT")
    # Get GE1 appearances from materials
    appearances, ma_to_appearance = list(), dict()
    for i, ma in enumerate(bpy.data.materials):
        appearances.append(
            _get_appearance(
                name=ma.name,
                i=i,
                rgb=(
                    int(ma.diffuse_color[0] * 255),
                    int(ma.diffuse_color[1] * 255),
                    int(ma.diffuse_color[2] * 255),
                    ma.diffuse_color[3],
                ),
            )
        )
        ma_to_appearance[ma.name] = i
    # Append dummy material for HOLEs
    i += 1
    appearances.append(
        _get_appearance(name="Dummy Hole", i=i, rgb=(150, 150, 150, 0.5))
    )
    ma_to_appearance[ma.name] = i
    # Select GE1 objects
    allowed_nls = ("ON_OBST", "ON_GEOM", "ON_VENT", "ON_HOLE")
    obs = (
        ob
        for ob in context.scene.objects
        if ob.type == "MESH"
        and not ob.hide_render  # show only exported obs
        and not ob.bf_is_tmp  # do not show tmp obs
        and ob.bf_namelist_cls in allowed_nls  # show only allowed namelists
        and (ob.active_material and ob.active_material.name != "OPEN")  # no OPEN
    )
    # Get GE1 faces from selected objects  # TODO use dummy color for HOLE
    gefaces = list()
    for ob in obs:
        # Get the bmesh from the Object and triangulate
        bm = utils.geometry.get_object_bmesh(context=context, ob=ob, world=True)
        bmesh.ops.triangulate(bm, faces=bm.faces)
        # Get ob material_slots
        material_slots = ob.material_slots
        # Get default_material_name
        if ob.bf_namelist_cls == "ON_HOLE":
            default_material_name = "Dummy Hole"
        elif ob.bf_namelist_cls == "ON_GEOM":
            default_material_name = None
        elif ob.active_material:
            default_material_name = ob.active_material.name
        else:
            default_material_name = "INERT"
        scale_length = context.scene.unit_settings.scale_length
        for f in bm.faces:
            # Grab ordered vertices coordinates
            coos = [co for v in f.verts for co in v.co]
            coos.extend((coos[-3], coos[-2], coos[-1]))  # tri to quad
            items = [f"{coo * scale_length:.6f}" for coo in coos]
            # Get appearance_index
            if default_material_name:
                material_name = default_material_name
            else:
                material_name = material_slots[f.material_index].material.name
            appearance_index = str(ma_to_appearance.get(material_name, 0)) + "\n"
            items.append(appearance_index)
            # Append GE1 face
            gefaces.append(" ".join(items))

    # Prepare GE1 file and return
    ge1_file_a = f"[APPEARANCE]\n{len(appearances)}\n{''.join(appearances)}"
    ge1_file_f = f"[FACES]\n{len(gefaces)}\n{''.join(gefaces)}"
    # Close
    w.cursor_modal_restore()
    return "".join((ge1_file_a, ge1_file_f))
