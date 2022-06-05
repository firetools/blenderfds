# SPDX-License-Identifier: GPL-3.0-or-later

import logging
from bpy.types import Object
from bpy.props import FloatProperty, BoolProperty, StringProperty
from ..types import BFParam, BFNamelistOb
from .bf_object import OP_namelist_cls, OP_ID, OP_FYI, OP_ID_suffix, OP_other
from .OP_XB import OP_XB, OP_XB_voxel_size, OP_XB_center_voxels
from .OP_XYZ import OP_XYZ
from .OP_SURF_ID import OP_SURF_ID

log = logging.getLogger(__name__)


class OP_DEVC_QUANTITY(BFParam):
    label = "QUANTITY"
    description = "Output quantity"
    fds_label = "QUANTITY"
    bpy_type = Object
    bpy_prop = StringProperty
    bpy_idname = "bf_quantity"

    def draw_operators(self, context, layout):
        layout.operator("object.bf_choose_devc_quantity", icon="VIEWZOOM", text="")


class OP_DEVC_PROP_ID(BFParam):
    label = "PROP_ID"
    description = "Reference to a PROP namelist"
    fds_label = "PROP_ID"
    bpy_type = Object
    bpy_prop = StringProperty
    bpy_idname = "bf_devc_prop_id"

    def draw_operators(self, context, layout):
        layout.operator("object.bf_choose_devc_prop_id", icon="VIEWZOOM", text="")


class ON_DEVC(BFNamelistOb):
    label = "DEVC"
    description = "Device"
    collection = "Output"
    enum_id = 1011
    fds_label = "DEVC"
    bf_params = (
        OP_namelist_cls,
        OP_ID,
        OP_FYI,
        OP_DEVC_QUANTITY,
        OP_DEVC_PROP_ID,
        OP_SURF_ID,
        OP_XB,
        OP_XB_voxel_size,
        OP_XB_center_voxels,
        OP_XYZ,
        OP_ID_suffix,
        OP_other,
    )
    bf_other = {"appearance": "WIRE"}
