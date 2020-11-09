"""!
BlenderFDS, handlers.
"""

import bpy, logging
from bpy.app.handlers import persistent, load_post, save_pre, depsgraph_update_post
from bpy.types import Object

from .. import geometry
from .. import config

log = logging.getLogger(__name__)

# Handlers


@persistent
def _load_post(self):
    """!
    Run automatic setup after loading a Blender file.
    """
    # Beware: self is None
    # Check file format version
    bf_file_version = tuple(bpy.data.scenes[0].bf_file_version)
    log.debug(f"Load post, file version: <{bf_file_version}>")
    if bf_file_version == (4, 0, 0):
        for ma in bpy.data.materials:
            ma.bf_namelist_cls = "MN_SURF"
        for ob in bpy.data.objects:
            try:
                bf_xb = ob["bf_xb"]
            except KeyError:
                pass
            else:
                if bf_xb == 0 or ob.bf_xb == "":  # was "NONE" or bad
                    ob.bf_xb = "BBOX"
                    ob.bf_xb_export = False
                else:
                    ob.bf_xb_export = True
            try:
                bf_xyz = ob["bf_xyz"]
            except KeyError:
                pass
            else:
                if bf_xyz == 0 or ob.bf_xyz == "":  # was "NONE" or bad
                    ob.bf_xyz = "VERTICES"
                    ob.bf_xyz_export = False
                else:
                    ob.bf_xyz_export = True
            try:
                bf_pb = ob["bf_pb"]
            except KeyError:
                pass
            else:
                if bf_pb == 0 or ob.bf_pb == "":  # was "NONE" or bad
                    ob.bf_pb = "PLANES"
                    ob.bf_pb_export = False
                else:
                    ob.bf_pb_export = True
        bpy.ops.wm.bf_dialog(
            "INVOKE_DEFAULT",
            msg="Check your data!",
            description="This file was created with an old BlenderFDS version.",
            type="ERROR",
        )
    elif bf_file_version > config.supported_file_version:
        bpy.ops.wm.bf_dialog(
            "INVOKE_DEFAULT",
            msg="Install latest BlenderFDS!",
            description="This file was created with a new BlenderFDS version.",
            type="ERROR",
        )
    else:
        # Fix some properties, due to a development error!
        for ma in bpy.data.materials:
            try:
                ma["bf_backing"] = {0: 100, 1: 200, 2: 300}[ma["bf_backing"]]
            except KeyError:
                if ma.bf_backing == "":
                    ma.bf_backing = "EXPOSED"
        for ob in bpy.data.objects:
            try:
                ob["bf_xb"] = {0: 100, 1: 200, 2: 300, 3: 400, 4: 500,}[ob["bf_xb"]]
            except KeyError:
                if ob.bf_xb == "":
                    ob.bf_xb = "BBOX"
            try:
                ob["bf_xyz"] = {0: 100, 1: 200,}[ob["bf_xyz"]]
            except KeyError:
                if ob.bf_xyz == "":
                    ob.bf_xyz = "CENTER"
            try:
                ob["bf_pb"] = {0: 100,}[ob["bf_pb"]]
            except KeyError:
                if ob.bf_pb == "":
                    ob.bf_pb = "PLANES"
            try:
                ob["bf_id_suffix"] = {
                    0: 100,
                    1: 200,
                    2: 300,
                    3: 400,
                    4: 500,
                    5: 600,
                    6: 700,
                    7: 800,
                }[ob["bf_id_suffix"]]
            except KeyError:
                if ob.bf_id_suffix == "":
                    ob.bf_id_suffix = "IDI"

    # Remove all caches and tmp objects, clean up to remove stale caches
    geometry.utils.rm_geometric_caches()
    geometry.utils.rm_tmp_objects()

    # Init FDS default materials
    for k, v in config.default_mas.items():
        ma = bpy.data.materials.get(k)  # k is ID
        if not ma:
            ma = bpy.data.materials.new(k)
        ma.diffuse_color = v[0]  # RGB
        ma.bf_surf_export = True
        ma.use_fake_user = True

    # Set default object and material appearance
    # TODO scene appearance?
    context = bpy.context
    for ob in bpy.data.objects:
        bf_namelist = ob.bf_namelist
        if bf_namelist:
            ob.bf_namelist.set_appearance(context=context)
    for ma in bpy.data.materials:
        bf_namelist = ma.bf_namelist
        if bf_namelist:
            ma.bf_namelist.set_appearance(context=context)


@persistent
def _save_pre(self):
    """!
    Run automatic setup before saving a Blender file.
    """
    # Beware: self is None
    # Remove all caches and tmp objects, clean up to prevent stale caches
    geometry.utils.rm_geometric_caches()
    geometry.utils.rm_tmp_objects()
    # Set file format version
    for sc in bpy.data.scenes:
        sc.bf_file_version = config.supported_file_version


@persistent
def _depsgraph_update_post(scene):
    """!
    Detect object change and erase cached geometry.
    """
    for update in bpy.context.view_layer.depsgraph.updates:
        ob = update.id.original
        if (
            isinstance(ob, Object)
            and ob.type in {"MESH", "CURVE", "SURFACE", "FONT", "META"}
            and (update.is_updated_geometry or update.is_updated_transform)
        ):
            # log.debug(f"Remove <{ob.name}> xbs cache")
            ob["ob_to_xbs_cache"] = None


# Register


def register():
    """!
    Load the Python classes and functions to Blender.
    """
    log.debug(f"Registering handlers")
    load_post.append(_load_post)
    save_pre.append(_save_pre)
    depsgraph_update_post.append(_depsgraph_update_post)


def unregister():
    """!
    Unload the Python classes and functions from Blender.
    """
    log.debug(f"Unregistering handlers")
    load_post.remove(_load_post)
    save_pre.remove(_save_pre)
    depsgraph_update_post.remove(_depsgraph_update_post)
