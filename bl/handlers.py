# SPDX-License-Identifier: GPL-3.0-or-later

"""!
BlenderFDS, handlers.
"""

import bpy, logging
from bpy.app.handlers import persistent, load_post, save_pre, depsgraph_update_post
from bpy.types import Object
from .. import utils
from .. import config

from ..lang.bf_object import OP_other
from ..lang.bf_material import MP_other

log = logging.getLogger(__name__)


@persistent
def _load_post(self):
    """!
    Run automatic setup after loading a Blender file.
    """
    # Beware: self is None
    # Check file format version
    bf_file_version = tuple(bpy.data.scenes[0].bf_file_version)

    if bf_file_version < config.SUPPORTED_FILE_VERSION:
        context = bpy.context

        for ob in bpy.data.objects:

            # Fix old Object export toggle
            if ob.hide_viewport:
                ob.hide_render = True

            # Fix old removed namelists (eg. 1017 HVAC)
            if not ob.bf_namelist_cls:
                ob.bf_namelist_cls = "ON_other"

            # Fix old DEVC namelist removed params
            if ob.bf_namelist_cls == "ON_DEVC":
                op_other = OP_other(ob)
                if ob.get("bf_devc_setpoint_export") and ob.get("bf_devc_setpoint"):
                    op_other.set_value(
                        context, f"SETPOINT={ob['bf_devc_setpoint']:.3f}"
                    )
                    ob["bf_devc_setpoint_export"] = False
                if ob.get("bf_devc_initial_state"):
                    op_other.set_value(context, f"INITIAL_STATE=T")
                    ob["bf_devc_initial_state"] = False
                if ob.get("bf_devc_latch"):
                    op_other.set_value(context, f"LATCH=T")
                    ob["bf_devc_latch"] = False

        # Fix old SURF namelist removed params
        for ma in bpy.data.materials:
            ma.bf_namelist_cls = "MN_SURF"
            mp_other = MP_other(ma)
            if ma.get("bf_thickness_export") and ma.get("bf_thickness"):
                mp_other.set_value(context, f"THICKNESS={ma['bf_thickness']:.3f}")
                ma["bf_thickness_export"] = False
            if ma.get("bf_hrrpua"):
                mp_other.set_value(context, f"HRRPUA={ma['bf_hrrpua']:.1f}")
                ma["bf_hrrpua"] = 0.0
            if ma.get("bf_tau_q"):
                mp_other.set_value(context, f"TAU_Q={ma['bf_tau_q']:.1f}")
                ma["bf_tau_q"] = 0.0
            if ma.get("bf_matl_id_export") and ma.get("bf_matl_id"):
                mp_other.set_value(context, f"MATL_ID='{ma['bf_matl_id']}'")
                ma["bf_matl_id_export"] = False
            if ma.get("bf_ignition_temperature_export") and ma.get(
                "bf_ignition_temperature"
            ):
                mp_other.set_value(
                    context, f"IGNITION_TEMPERATURE={ma['bf_ignition_temperature']:.1f}"
                )
                ma["bf_ignition_temperature_export"] = False
            if ma.get("bf_backing_export") and ma.get("bf_backing") != "EXPOSED":
                mp_other.set_value(context, f"BACKING='{ma['bf_backing']}'")
                ma["bf_backing_export"] = False

        # Inform
        bpy.ops.wm.bf_dialog(
            "INVOKE_DEFAULT",
            msg="Check your data!",
            description="This file was created with an old BlenderFDS version.",
            type="ERROR",
        )

    elif bf_file_version > config.SUPPORTED_FILE_VERSION:
        bpy.ops.wm.bf_dialog(
            "INVOKE_DEFAULT",
            msg="Install latest BlenderFDS!",
            description="This file was created with a new BlenderFDS version.",
            type="ERROR",
        )

    # Remove all caches and tmp objects, clean up to remove stale caches
    utils.geometry.rm_geometric_caches()
    utils.geometry.rm_tmp_objects()

    # Init FDS default materials
    for k, v in config.DEFAULT_MAS.items():
        ma = bpy.data.materials.get(k)  # k is ID
        if not ma:
            ma = bpy.data.materials.new(k)
        ma.diffuse_color = v[0]  # RGB
        ma.bf_surf_export = True
        ma.use_fake_user = True

    # Set default object and material appearance
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
    utils.geometry.rm_geometric_caches()
    utils.geometry.rm_tmp_objects()
    # Set file format version
    for sc in bpy.data.scenes:
        sc.bf_file_version = config.SUPPORTED_FILE_VERSION


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
            ob["ob_to_xbs_cache"] = None


# Register


def register():
    log.info("Register handlers...")
    load_post.append(_load_post)
    save_pre.append(_save_pre)
    depsgraph_update_post.append(_depsgraph_update_post)


def unregister():
    log.info("Unregister handlers...")
    depsgraph_update_post.remove(_depsgraph_update_post)
    save_pre.remove(_save_pre)
    load_post.remove(_load_post)
